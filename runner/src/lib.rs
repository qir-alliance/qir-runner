// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#![deny(clippy::all, clippy::pedantic)]
#![allow(unused)]

extern crate getopts;

pub use qir_backend::{
    arrays::*, bigints::*, callables::*, exp::*, math::*, output_recording::*, range_support::*,
    result_bool::*, strings::*, tuples::*, *,
};

use inkwell::{
    attributes::AttributeLoc,
    context::Context,
    execution_engine::ExecutionEngine,
    memory_buffer::MemoryBuffer,
    module::Module,
    passes::{PassManager, PassManagerBuilder},
    targets::{InitializationConfig, Target, TargetMachine},
    types::VoidType,
    values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue, GenericValue, IntValue},
    OptimizationLevel,
};
use std::{ffi::OsStr, path::Path};

use getopts::Options;

/// # Errors
///
/// Will return `Err` if
/// - `filename` does not exist or the user does not have permission to read it.
/// - `filename` does not contain a valid bitcode module
/// - `filename` does not have either a .ll or .bc as an extension
/// - `args` does not match an entry point and its parameters
/// - Entry point has a non-void return
pub fn run_file(path: impl AsRef<Path>, args: &[String]) -> Result<(), String> {
    let context = Context::create();
    let module = load_file(path, &context)?;
    run_module(&module, args)
}

/// # Errors
///
/// Will return `Err` if
/// - `bytes` does not contain a valid bitcode module
/// - `args` does not match an entry point and its parameters
/// - Entry point has a non-void return
pub fn run_bitcode(bytes: &[u8], args: &[String]) -> Result<(), String> {
    let context = Context::create();
    let buffer = MemoryBuffer::create_from_memory_range(bytes, "");
    let module = Module::parse_bitcode_from_buffer(&buffer, &context).map_err(|e| e.to_string())?;
    run_module(&module, args)
}

fn run_module(module: &Module, args: &[String]) -> Result<(), String> {
    module
        .verify()
        .map_err(|e| format!("Failed to verify module: {}", e.to_string()))?;

    run_basic_passes_on(module);

    Target::initialize_native(&InitializationConfig::default())?;
    let default_triple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&default_triple).map_err(|e| e.to_string())?;
    if !target.has_asm_backend() {
        return Err("Target doesn't have an ASM backend.".to_owned());
    }
    if !target.has_target_machine() {
        return Err("Target doesn't have a target machine.".to_owned());
    }

    let (entry_point, args) = choose_entry_point(module_functions(module), args)?;
    inkwell::support::load_library_permanently("");

    unsafe { run_entry_point(module, entry_point, args) }
}

fn load_file(path: impl AsRef<Path>, context: &Context) -> Result<Module, String> {
    let path = path.as_ref();
    let extension = path.extension().and_then(OsStr::to_str);

    match extension {
        Some("ll") => MemoryBuffer::create_from_file(path)
            .and_then(|buffer| context.create_module_from_ir(buffer))
            .map_err(|e| e.to_string()),
        Some("bc") => Module::parse_bitcode_from_path(path, context).map_err(|e| e.to_string()),
        _ => Err(format!("Unsupported file extension '{:?}'.", extension)),
    }
}

unsafe fn run_entry_point(
    module: &Module,
    entry_point: FunctionValue,
    args: &[String],
) -> Result<(), String> {
    if entry_point.get_type().get_return_type().is_none() {
        let wrapper = add_entry_point_wrapper(module, entry_point, args)?;

        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::None)
            .map_err(|e| e.to_string())?;

        bind_functions(module, &execution_engine);

        execution_engine.run_function(wrapper, &[]);
        Ok(())
    } else {
        Err("Entry point has a non-void return type.".to_owned())
    }
}

fn options_from_entry_point(entry_point: FunctionValue) -> Options {
    let mut opts = Options::new();
    opts.long_only(true);

    for param in entry_point.get_param_iter() {
        match param {
            BasicValueEnum::IntValue(v) => opts.reqopt(
                "",
                v.get_name()
                    .to_str()
                    .expect("Entry point parameter name missing."),
                format!("{}-bit Integer", v.get_type().get_bit_width()).as_str(),
                "",
            ),
            BasicValueEnum::FloatValue(v) => opts.reqopt(
                "",
                v.get_name()
                    .to_str()
                    .expect("Entry point parameter name missing."),
                "Double",
                "",
            ),
            BasicValueEnum::PointerValue(v) => opts.reqopt(
                "",
                v.get_name()
                    .to_str()
                    .expect("Entry point parameter name missing."),
                if v.get_type().get_element_type().is_int_type() {
                    "String"
                } else {
                    "Unsupported pointer type"
                },
                "",
            ),
            BasicValueEnum::ArrayValue(v) => opts.reqopt(
                "",
                v.get_name()
                    .to_str()
                    .expect("Entry point parameter name missing."),
                "Unsupported array type",
                "This parameter cannot be used.",
            ),
            BasicValueEnum::StructValue(v) => opts.reqopt(
                "",
                v.get_name()
                    .to_str()
                    .expect("Entry point parameter name missing."),
                "Unsupported struct type",
                "This parameter cannot be used.",
            ),
            BasicValueEnum::VectorValue(v) => opts.reqopt(
                "",
                v.get_name()
                    .to_str()
                    .expect("Entry point parameter name missing."),
                "Unsupported vector type",
                "This parameter cannot be used.",
            ),
        };
    }
    opts
}

#[allow(clippy::cast_sign_loss)]
fn add_entry_point_wrapper<'ctx>(
    module: &Module<'ctx>,
    entry_point: FunctionValue,
    args: &[String],
) -> Result<FunctionValue<'ctx>, String> {
    let opts = options_from_entry_point(entry_point);
    let parsed_args = opts.parse(args);

    if let Ok(parsed_args) = parsed_args {
        let mut generated_args = vec![];
        for param in entry_point.get_param_iter() {
            generated_args.push(match param {
                BasicValueEnum::IntValue(v) => {
                    let name = v
                        .get_name()
                        .to_str()
                        .expect("Entry point params must have names");
                    BasicMetadataValueEnum::IntValue(
                        v.get_type().const_int(
                            parsed_args
                                .opt_get::<i64>(name)
                                .map_err(|e| {
                                    format!("Unable to parse integer argument '{}': {}", name, e)
                                })?
                                .expect("All parameters must be present.")
                                as u64,
                            true,
                        ),
                    )
                }
                BasicValueEnum::FloatValue(v) => {
                    let name = v
                        .get_name()
                        .to_str()
                        .expect("Entry point params must have names");
                    BasicMetadataValueEnum::FloatValue(
                        v.get_type().const_float(
                            parsed_args
                                .opt_get(name)
                                .map_err(|e| {
                                    format!("Unable to parse double argument '{}': {}", name, e)
                                })?
                                .expect("All parameters must be present."),
                        ),
                    )
                }
                BasicValueEnum::PointerValue(v) => todo!(),
                _ => unimplemented!("Unsupported argument type: {}", param),
            });
        }

        let context = module.get_context();
        let wrapper = module.add_function(
            "__generated_entry_point_wrapper__",
            context.void_type().fn_type(&[], false),
            None,
        );
        let basic_block = context.append_basic_block(wrapper, "entry");
        let builder = context.create_builder();
        builder.position_at_end(basic_block);
        let _ = builder.build_call(entry_point, &generated_args, "call");
        let _ = builder.build_return(None);

        Ok(wrapper)
    } else {
        println!(
            "PARAMETERS:{}",
            opts.usage_with_format(|params| params.fold(
                "".to_string(),
                |accum, param_desc| format!("{}\n{}", accum, param_desc)
            ))
        );
        Err(format!(
            "Failed to parse arguments: {}",
            parsed_args.expect_err("Parsed args known to be Err type.")
        ))
    }
}

fn choose_entry_point<'ctx>(
    functions: impl Iterator<Item = FunctionValue<'ctx>>,
    args: &[String],
) -> Result<(FunctionValue<'ctx>, &[String]), String> {
    let entry_points: Vec<FunctionValue> = functions.filter(|f| is_entry_point(*f)).collect();

    if entry_points.is_empty() {
        Err("No entry point functions found.".to_owned())
    } else if entry_points.len() == 1 {
        Ok((entry_points[0], args))
    } else if let Some(entry_point) = entry_points
        .iter()
        .find(|f| f.get_name().to_str().ok() == args.first().map(|a| &**a))
    {
        Ok((*entry_point, args.split_at(1).1))
    } else {
        let (first_entry_point, other_entry_points) = entry_points
            .split_first()
            .expect("Should have at least one entry point.");

        println!("AVAILABLE ENTRY POINTS:\n");
        for entry_point in entry_points {
            println!(
                "{}",
                entry_point
                    .get_name()
                    .to_str()
                    .expect("Entry point parameters must have names.")
            );
            if entry_point.count_params() > 0 {
                print!(
                    "{}",
                    options_from_entry_point(entry_point).usage_with_format(|params| params.fold(
                        "".to_string(),
                        |accum, param_desc| format!("{}{}\n", accum, param_desc)
                    ))
                );
            }
            println!("");
        }

        Err("Multiple entry points found, entry point parameter required.".to_owned())
    }
}

fn module_functions<'ctx>(module: &Module<'ctx>) -> impl Iterator<Item = FunctionValue<'ctx>> {
    struct FunctionValueIter<'ctx>(Option<FunctionValue<'ctx>>);

    impl<'ctx> Iterator for FunctionValueIter<'ctx> {
        type Item = FunctionValue<'ctx>;

        fn next(&mut self) -> Option<Self::Item> {
            let function = self.0;
            self.0 = function.and_then(inkwell::values::FunctionValue::get_next_function);
            function
        }
    }

    FunctionValueIter(module.get_first_function())
}

fn is_entry_point(function: FunctionValue) -> bool {
    function
        .get_string_attribute(AttributeLoc::Function, "EntryPoint")
        .is_some()
}

fn run_basic_passes_on(module: &Module) -> bool {
    let pass_manager_builder = PassManagerBuilder::create();
    pass_manager_builder.set_optimization_level(OptimizationLevel::None);
    let fpm = PassManager::create(());
    fpm.add_global_dce_pass();
    fpm.add_strip_dead_prototypes_pass();
    pass_manager_builder.populate_module_pass_manager(&fpm);
    fpm.run_on(module)
}

// Forward declare IR defined functions to force linking with static library.
// Note that the types are not needed as linking is only based on function name.
extern "C" {
    fn __quantum__rt__array_slice_1d();
    fn __quantum__rt__range_to_string();
}

#[allow(clippy::too_many_lines)]
fn bind_functions(module: &Module, execution_engine: &ExecutionEngine) {
    macro_rules! bind {
        ($func:ident) => {
            if let Some(func) =
                &module_functions(&module).find(|f| f.get_name().to_str() == Ok(stringify!($func)))
            {
                execution_engine.add_global_mapping(func, $func as usize);
            }
        };
    }

    bind!(__quantum__qis__arccos__body);
    bind!(__quantum__qis__arcsin__body);
    bind!(__quantum__qis__arctan__body);
    bind!(__quantum__qis__arctan2__body);
    bind!(__quantum__qis__assertmeasurementprobability__body);
    bind!(__quantum__qis__assertmeasurementprobability__ctl);
    bind!(__quantum__qis__ccx__body);
    bind!(__quantum__qis__cnot__body);
    bind!(__quantum__qis__cos__body);
    bind!(__quantum__qis__cosh__body);
    bind!(__quantum__qis__cx__body);
    bind!(__quantum__qis__cz__body);
    bind!(__quantum__qis__drawrandomdouble__body);
    bind!(__quantum__qis__drawrandomint__body);
    bind!(__quantum__qis__dumpmachine__body);
    bind!(__quantum__qis__exp__body);
    bind!(__quantum__qis__exp__adj);
    bind!(__quantum__qis__exp__body);
    bind!(__quantum__qis__exp__ctl);
    bind!(__quantum__qis__exp__ctladj);
    bind!(__quantum__qis__exp__ctl);
    bind!(__quantum__qis__h__body);
    bind!(__quantum__qis__h__ctl);
    bind!(__quantum__qis__ieeeremainder__body);
    bind!(__quantum__qis__infinity__body);
    bind!(__quantum__qis__isinf__body);
    bind!(__quantum__qis__isnan__body);
    bind!(__quantum__qis__isnegativeinfinity__body);
    bind!(__quantum__qis__log__body);
    bind!(__quantum__qis__m__body);
    bind!(__quantum__qis__measure__body);
    bind!(__quantum__qis__mz__body);
    bind!(__quantum__qis__nan__body);
    bind!(__quantum__qis__r__adj);
    bind!(__quantum__qis__r__body);
    bind!(__quantum__qis__r__ctl);
    bind!(__quantum__qis__r__ctladj);
    bind!(__quantum__qis__read_result__body);
    bind!(__quantum__qis__reset__body);
    bind!(__quantum__qis__rx__body);
    bind!(__quantum__qis__rx__ctl);
    bind!(__quantum__qis__ry__body);
    bind!(__quantum__qis__ry__ctl);
    bind!(__quantum__qis__rz__body);
    bind!(__quantum__qis__rz__ctl);
    bind!(__quantum__qis__s__adj);
    bind!(__quantum__qis__s__body);
    bind!(__quantum__qis__s__ctl);
    bind!(__quantum__qis__s__ctladj);
    bind!(__quantum__qis__sin__body);
    bind!(__quantum__qis__sinh__body);
    bind!(__quantum__qis__sqrt__body);
    bind!(__quantum__qis__swap__body);
    bind!(__quantum__qis__t__adj);
    bind!(__quantum__qis__t__body);
    bind!(__quantum__qis__t__ctl);
    bind!(__quantum__qis__t__ctladj);
    bind!(__quantum__qis__tan__body);
    bind!(__quantum__qis__tanh__body);
    bind!(__quantum__qis__x__body);
    bind!(__quantum__qis__x__ctl);
    bind!(__quantum__qis__y__body);
    bind!(__quantum__qis__y__ctl);
    bind!(__quantum__qis__z__body);
    bind!(__quantum__qis__z__ctl);
    bind!(__quantum__rt__array_concatenate);
    bind!(__quantum__rt__array_copy);
    bind!(__quantum__rt__array_create_1d);
    bind!(__quantum__rt__array_end_record_output);
    bind!(__quantum__rt__array_get_element_ptr_1d);
    bind!(__quantum__rt__array_get_size_1d);
    bind!(__quantum__rt__array_slice_1d);
    bind!(__quantum__rt__array_start_record_output);
    bind!(__quantum__rt__array_update_alias_count);
    bind!(__quantum__rt__array_update_reference_count);
    bind!(__quantum__rt__bigint_add);
    bind!(__quantum__rt__bigint_bitand);
    bind!(__quantum__rt__bigint_bitnot);
    bind!(__quantum__rt__bigint_bitor);
    bind!(__quantum__rt__bigint_bitxor);
    bind!(__quantum__rt__bigint_create_array);
    bind!(__quantum__rt__bigint_create_i64);
    bind!(__quantum__rt__bigint_divide);
    bind!(__quantum__rt__bigint_equal);
    bind!(__quantum__rt__bigint_get_data);
    bind!(__quantum__rt__bigint_get_length);
    bind!(__quantum__rt__bigint_greater);
    bind!(__quantum__rt__bigint_greater_eq);
    bind!(__quantum__rt__bigint_modulus);
    bind!(__quantum__rt__bigint_multiply);
    bind!(__quantum__rt__bigint_negate);
    bind!(__quantum__rt__bigint_power);
    bind!(__quantum__rt__bigint_shiftleft);
    bind!(__quantum__rt__bigint_shiftright);
    bind!(__quantum__rt__bigint_subtract);
    bind!(__quantum__rt__bigint_to_string);
    bind!(__quantum__rt__bigint_update_reference_count);
    bind!(__quantum__rt__bool_record_output);
    bind!(__quantum__rt__bool_to_string);
    bind!(__quantum__rt__callable_copy);
    bind!(__quantum__rt__callable_create);
    bind!(__quantum__rt__callable_invoke);
    bind!(__quantum__rt__callable_make_adjoint);
    bind!(__quantum__rt__callable_make_controlled);
    bind!(__quantum__rt__callable_update_alias_count);
    bind!(__quantum__rt__callable_update_reference_count);
    bind!(__quantum__rt__capture_update_alias_count);
    bind!(__quantum__rt__capture_update_reference_count);
    bind!(__quantum__rt__double_record_output);
    bind!(__quantum__rt__double_to_string);
    bind!(__quantum__rt__fail);
    bind!(__quantum__rt__int_record_output);
    bind!(__quantum__rt__int_to_string);
    bind!(__quantum__rt__memory_allocate);
    bind!(__quantum__rt__message);
    bind!(__quantum__rt__pauli_to_string);
    bind!(__quantum__rt__qubit_allocate);
    bind!(__quantum__rt__qubit_allocate_array);
    bind!(__quantum__rt__qubit_release);
    bind!(__quantum__rt__qubit_release_array);
    bind!(__quantum__rt__qubit_to_string);
    bind!(__quantum__rt__range_to_string);
    bind!(__quantum__rt__result_equal);
    bind!(__quantum__rt__result_get_one);
    bind!(__quantum__rt__result_get_zero);
    bind!(__quantum__rt__result_record_output);
    bind!(__quantum__rt__result_to_string);
    bind!(__quantum__rt__result_update_reference_count);
    bind!(__quantum__rt__string_concatenate);
    bind!(__quantum__rt__string_create);
    bind!(__quantum__rt__string_equal);
    bind!(__quantum__rt__string_get_data);
    bind!(__quantum__rt__string_get_length);
    bind!(__quantum__rt__string_update_reference_count);
    bind!(__quantum__rt__tuple_copy);
    bind!(__quantum__rt__tuple_create);
    bind!(__quantum__rt__tuple_end_record_output);
    bind!(__quantum__rt__tuple_start_record_output);
    bind!(__quantum__rt__tuple_update_alias_count);
    bind!(__quantum__rt__tuple_update_reference_count);
}
