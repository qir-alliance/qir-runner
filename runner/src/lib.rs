// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#![deny(clippy::all, clippy::pedantic)]
#![allow(unused)]

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
    values::FunctionValue,
    OptimizationLevel,
};
use std::{collections::HashMap, ffi::OsStr, path::Path};

/// # Errors
///
/// Will return `Err` if
/// - `filename` does not exist or the user does not have permission to read it.
/// - `filename` does not contain a valid bitcode module
/// - `filename` does not have either a .ll or .bc as an extension
/// - `entry_point` is not found in the QIR
/// - Entry point has parameters or a non-void return type.
pub fn run_file(path: impl AsRef<Path>, entry_point: Option<&str>) -> Result<(), String> {
    let context = Context::create();
    let module = load_file(path, &context)?;
    run_module(&module, entry_point)
}

/// # Errors
///
/// Will return `Err` if
/// - `bytes` does not contain a valid bitcode module
/// - `entry_point` is not found in the QIR
/// - Entry point has parameters or a non-void return type.
pub fn run_bitcode(bytes: &[u8], entry_point: Option<&str>) -> Result<(), String> {
    let context = Context::create();
    let buffer = MemoryBuffer::create_from_memory_range(bytes, "");
    let module = Module::parse_bitcode_from_buffer(&buffer, &context).map_err(|e| e.to_string())?;
    run_module(&module, entry_point)
}

fn run_module(module: &Module, entry_point: Option<&str>) -> Result<(), String> {
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

    inkwell::support::load_library_permanently("");

    let execution_engine = module
        .create_jit_execution_engine(OptimizationLevel::None)
        .map_err(|e| e.to_string())?;

    bind_functions(module, &execution_engine)?;

    let entry_point = choose_entry_point(module_functions(module), entry_point)?;
    // TODO: need a cleaner way to get the attr strings for metadata
    let attrs: Vec<(String, String)> = entry_point
        .attributes(AttributeLoc::Function)
        .iter()
        .map(|attr| {
            (
                attr.get_string_kind_id()
                    .to_str()
                    .expect("Invalid UTF8 data")
                    .to_string(),
                attr.get_string_value()
                    .to_str()
                    .expect("Invalid UTF8 data")
                    .to_string(),
            )
        })
        .collect();

    // This loop is a placeholder where shots will be defined
    for _ in 1..=1 {
        println!("START");
        for attr in &attrs {
            print!("METADATA\t{}", attr.0);
            if !attr.1.is_empty() {
                print!("\t{}", attr.1);
            }
            println!();
        }

        unsafe { run_entry_point(&execution_engine, entry_point)? }
        println!("END\t0");
    }
    Ok(())
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
    execution_engine: &ExecutionEngine,
    entry_point: FunctionValue,
) -> Result<(), String> {
    if entry_point.count_params() == 0 && entry_point.get_type().get_return_type().is_none() {
        execution_engine.run_function(entry_point, &[]);
        Ok(())
    } else {
        Err("Entry point has parameters or a non-void return type.".to_owned())
    }
}

fn choose_entry_point<'ctx>(
    functions: impl Iterator<Item = FunctionValue<'ctx>>,
    name: Option<&str>,
) -> Result<FunctionValue<'ctx>, String> {
    let mut entry_points = functions
        .filter(|f| is_entry_point(*f) && name.iter().all(|n| f.get_name().to_str() == Ok(n)));

    let entry_point = entry_points
        .next()
        .ok_or_else(|| "No matching entry point found.".to_owned())?;

    if entry_points.next().is_some() {
        Err("Multiple matching entry points found.".to_owned())
    } else {
        Ok(entry_point)
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
        .get_string_attribute(AttributeLoc::Function, "entry_point")
        .is_some()
        || function
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
fn bind_functions(module: &Module, execution_engine: &ExecutionEngine) -> Result<(), String> {
    const DEPRECATED: &str = r#"Use of deprecated output recording call.
    Please update your QIR tooling.
    Found:"#;

    let mut declarations: HashMap<String, FunctionValue> = HashMap::default();
    for func in module_functions(module).filter(|f| {
        f.count_basic_blocks() == 0
            && !f
                .get_name()
                .to_str()
                .expect("Unable to coerce function name into str.")
                .starts_with("llvm.")
    }) {
        declarations.insert(
            func.get_name()
                .to_str()
                .expect("Unable to coerce function name into str.")
                .to_owned(),
            func,
        );
    }

    macro_rules! bind {
        ($func:ident) => {
            if let Some(func) = declarations.get(stringify!($func)) {
                execution_engine.add_global_mapping(func, $func as usize);
                declarations.remove(stringify!($func));
            }
        };
    }

    macro_rules! deprecate_error {
        ($func:ident) => {
            if let Some(func) = declarations.get(stringify!($func)) {
                return Err(format!("{} {}", DEPRECATED, stringify!($func)));
            }
        };
    }

    macro_rules! bind_output_record {
        ($func:ident) => {
            if let Some(func) = declarations.get(stringify!($func)) {
                if func.get_params().len() == 1 {
                    execution_engine.add_global_mapping(
                        func,
                        qir_backend::output_recording::unlabeled::$func as usize,
                    );
                    declarations.remove(stringify!($func));
                } else {
                    execution_engine.add_global_mapping(func, $func as usize);
                    declarations.remove(stringify!($func));
                }
            }
        };
    }

    // Legacy output methods
    deprecate_error!(__quantum__rt__array_end_record_output);
    deprecate_error!(__quantum__rt__array_start_record_output);
    deprecate_error!(__quantum__rt__tuple_end_record_output);
    deprecate_error!(__quantum__rt__tuple_start_record_output);

    bind!(__quantum__rt__initialize);
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

    // New calls
    bind_output_record!(__quantum__rt__array_record_output);
    bind_output_record!(__quantum__rt__tuple_record_output);

    // calls with unlabeled signature variants
    bind_output_record!(__quantum__rt__bool_record_output);
    bind_output_record!(__quantum__rt__double_record_output);
    bind_output_record!(__quantum__rt__int_record_output);

    // results need special handling as they aren't in the std lib
    if let Some(func) = declarations.get("__quantum__rt__result_record_output") {
        if func.get_params().len() == 1 {
            execution_engine.add_global_mapping(
                func,
                qir_backend::unlabeled::__quantum__rt__result_record_output as usize,
            );
            declarations.remove("__quantum__rt__result_record_output");
        } else {
            execution_engine.add_global_mapping(func, __quantum__rt__result_record_output as usize);
            declarations.remove("__quantum__rt__result_record_output");
        }
    }

    bind!(__quantum__rt__array_get_element_ptr_1d);
    bind!(__quantum__rt__array_get_size_1d);
    bind!(__quantum__rt__array_slice_1d);
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
    bind!(__quantum__rt__tuple_update_alias_count);
    bind!(__quantum__rt__tuple_update_reference_count);

    if declarations.is_empty() {
        Ok(())
    } else {
        let keys = declarations.keys().collect::<Vec<_>>();
        let (first, rest) = keys
            .split_first()
            .expect("Declarations list should be non-empty.");
        Err(format!(
            "Failed to link some declared functions: {}",
            rest.iter().fold((*first).to_string(), |mut accum, f| {
                accum.push_str(", ");
                accum.push_str(f);
                accum
            })
        ))
    }
}
