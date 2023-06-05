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
use std::{collections::HashMap, ffi::OsStr, path::Path, ptr::null_mut};

/// # Errors
///
/// Will return `Err` if
/// - `filename` does not exist or the user does not have permission to read it.
/// - `filename` does not contain a valid bitcode module
/// - `filename` does not have either a .ll or .bc as an extension
/// - `entry_point` is not found in the QIR
/// - Entry point has parameters or a non-void return type.
pub fn run_file(
    path: impl AsRef<Path>,
    entry_point: Option<&str>,
    shots: u32,
) -> Result<(), String> {
    let context = Context::create();
    let module = load_file(path, &context)?;
    run_module(&module, entry_point, shots)
}

/// # Errors
///
/// Will return `Err` if
/// - `bytes` does not contain a valid bitcode module
/// - `entry_point` is not found in the QIR
/// - Entry point has parameters or a non-void return type.
pub fn run_bitcode(bytes: &[u8], entry_point: Option<&str>, shots: u32) -> Result<(), String> {
    let context = Context::create();
    let buffer = MemoryBuffer::create_from_memory_range(bytes, "");
    let module = Module::parse_bitcode_from_buffer(&buffer, &context).map_err(|e| e.to_string())?;
    run_module(&module, entry_point, shots)
}

fn run_module(module: &Module, entry_point: Option<&str>, shots: u32) -> Result<(), String> {
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

    for _ in 1..=shots {
        println!("START");
        for attr in &attrs {
            print!("METADATA\t{}", attr.0);
            if !attr.1.is_empty() {
                print!("\t{}", attr.1);
            }
            println!();
        }

        __quantum__rt__initialize(null_mut());
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
        _ => Err(format!("Unsupported file extension '{extension:?}'.")),
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
    let mut uses_legacy = vec![];
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
        ($func:ident, $param_count:expr) => {
            if let Some(func) = declarations.get(stringify!($func)) {
                if func.get_params().len() != $param_count {
                    return Err(format!(
                        "Function '{}' has mismatched parameters: expected {}, found {}",
                        stringify!($func),
                        $param_count,
                        func.get_params().len()
                    ));
                }
                execution_engine.add_global_mapping(func, $func as usize);
                declarations.remove(stringify!($func));
            }
        };
    }

    macro_rules! legacy_output {
        ($func:ident) => {
            if let Some(func) = declarations.get(stringify!($func)) {
                execution_engine.add_global_mapping(
                    func,
                    qir_backend::output_recording::legacy::$func as usize,
                );
                declarations.remove(stringify!($func));
                Some(true)
            } else {
                None
            }
        };
    }

    macro_rules! bind_output_record {
        ($func:ident) => {
            if let Some(func) = declarations.get(stringify!($func)) {
                if func.get_params().len() == 1 {
                    execution_engine.add_global_mapping(
                        func,
                        qir_backend::output_recording::legacy::$func as usize,
                    );
                    declarations.remove(stringify!($func));
                    Some(true)
                } else {
                    execution_engine.add_global_mapping(func, $func as usize);
                    declarations.remove(stringify!($func));
                    Some(false)
                }
            } else {
                None
            }
        };
    }

    // Legacy output methods
    uses_legacy.push(legacy_output!(__quantum__rt__array_end_record_output));
    uses_legacy.push(legacy_output!(__quantum__rt__array_start_record_output));
    uses_legacy.push(legacy_output!(__quantum__rt__tuple_end_record_output));
    uses_legacy.push(legacy_output!(__quantum__rt__tuple_start_record_output));

    bind!(__quantum__rt__initialize, 1);
    bind!(__quantum__qis__arccos__body, 1);
    bind!(__quantum__qis__arcsin__body, 1);
    bind!(__quantum__qis__arctan__body, 1);
    bind!(__quantum__qis__arctan2__body, 2);
    bind!(__quantum__qis__assertmeasurementprobability__body, 6);
    bind!(__quantum__qis__assertmeasurementprobability__ctl, 6);
    bind!(__quantum__qis__ccx__body, 3);
    bind!(__quantum__qis__cnot__body, 2);
    bind!(__quantum__qis__cos__body, 1);
    bind!(__quantum__qis__cosh__body, 1);
    bind!(__quantum__qis__cx__body, 2);
    bind!(__quantum__qis__cz__body, 2);
    bind!(__quantum__qis__drawrandomdouble__body, 2);
    bind!(__quantum__qis__drawrandomint__body, 2);
    bind!(__quantum__qis__dumpmachine__body, 1);
    bind!(__quantum__qis__exp__body, 3);
    bind!(__quantum__qis__exp__adj, 3);
    bind!(__quantum__qis__exp__ctl, 2);
    bind!(__quantum__qis__exp__ctladj, 2);
    bind!(__quantum__qis__h__body, 1);
    bind!(__quantum__qis__h__ctl, 2);
    bind!(__quantum__qis__ieeeremainder__body, 2);
    bind!(__quantum__qis__infinity__body, 0);
    bind!(__quantum__qis__isinf__body, 1);
    bind!(__quantum__qis__isnan__body, 1);
    bind!(__quantum__qis__isnegativeinfinity__body, 1);
    bind!(__quantum__qis__log__body, 1);
    bind!(__quantum__qis__m__body, 1);
    bind!(__quantum__qis__measure__body, 2);
    bind!(__quantum__qis__mz__body, 2);
    bind!(__quantum__qis__nan__body, 0);
    bind!(__quantum__qis__r__adj, 3);
    bind!(__quantum__qis__r__body, 3);
    bind!(__quantum__qis__r__ctl, 2);
    bind!(__quantum__qis__r__ctladj, 2);
    bind!(__quantum__qis__read_result__body, 1);
    bind!(__quantum__qis__reset__body, 1);
    bind!(__quantum__qis__rx__body, 2);
    bind!(__quantum__qis__rx__ctl, 2);
    bind!(__quantum__qis__ry__body, 2);
    bind!(__quantum__qis__ry__ctl, 2);
    bind!(__quantum__qis__rz__body, 2);
    bind!(__quantum__qis__rz__ctl, 2);
    bind!(__quantum__qis__s__adj, 1);
    bind!(__quantum__qis__s__body, 1);
    bind!(__quantum__qis__s__ctl, 2);
    bind!(__quantum__qis__s__ctladj, 2);
    bind!(__quantum__qis__sin__body, 1);
    bind!(__quantum__qis__sinh__body, 1);
    bind!(__quantum__qis__sqrt__body, 1);
    bind!(__quantum__qis__swap__body, 2);
    bind!(__quantum__qis__t__adj, 1);
    bind!(__quantum__qis__t__body, 1);
    bind!(__quantum__qis__t__ctl, 2);
    bind!(__quantum__qis__t__ctladj, 2);
    bind!(__quantum__qis__tan__body, 1);
    bind!(__quantum__qis__tanh__body, 1);
    bind!(__quantum__qis__x__body, 1);
    bind!(__quantum__qis__x__ctl, 2);
    bind!(__quantum__qis__y__body, 1);
    bind!(__quantum__qis__y__ctl, 2);
    bind!(__quantum__qis__z__body, 1);
    bind!(__quantum__qis__z__ctl, 2);
    bind!(__quantum__rt__array_concatenate, 2);
    bind!(__quantum__rt__array_copy, 2);
    bind!(__quantum__rt__array_create_1d, 2);

    // New calls
    bind!(__quantum__rt__array_record_output, 2);
    bind!(__quantum__rt__tuple_record_output, 2);

    // calls with unlabeled signature variants
    uses_legacy.push(bind_output_record!(__quantum__rt__bool_record_output));
    uses_legacy.push(bind_output_record!(__quantum__rt__double_record_output));
    uses_legacy.push(bind_output_record!(__quantum__rt__int_record_output));

    // results need special handling as they aren't in the std lib
    uses_legacy.push(
        if let Some(func) = declarations.get("__quantum__rt__result_record_output") {
            if func.get_params().len() == 1 {
                execution_engine.add_global_mapping(
                    func,
                    qir_backend::legacy_output::__quantum__rt__result_record_output as usize,
                );
                declarations.remove("__quantum__rt__result_record_output");
                Some(true)
            } else {
                execution_engine
                    .add_global_mapping(func, __quantum__rt__result_record_output as usize);
                declarations.remove("__quantum__rt__result_record_output");
                Some(false)
            }
        } else {
            None
        },
    );

    bind!(__quantum__rt__array_get_element_ptr_1d, 2);
    bind!(__quantum__rt__array_get_size_1d, 1);
    bind!(__quantum__rt__array_slice_1d, 3);
    bind!(__quantum__rt__array_update_alias_count, 2);
    bind!(__quantum__rt__array_update_reference_count, 2);
    bind!(__quantum__rt__bigint_add, 2);
    bind!(__quantum__rt__bigint_bitand, 2);
    bind!(__quantum__rt__bigint_bitnot, 1);
    bind!(__quantum__rt__bigint_bitor, 2);
    bind!(__quantum__rt__bigint_bitxor, 2);
    bind!(__quantum__rt__bigint_create_array, 2);
    bind!(__quantum__rt__bigint_create_i64, 1);
    bind!(__quantum__rt__bigint_divide, 2);
    bind!(__quantum__rt__bigint_equal, 2);
    bind!(__quantum__rt__bigint_get_data, 1);
    bind!(__quantum__rt__bigint_get_length, 1);
    bind!(__quantum__rt__bigint_greater, 2);
    bind!(__quantum__rt__bigint_greater_eq, 2);
    bind!(__quantum__rt__bigint_modulus, 2);
    bind!(__quantum__rt__bigint_multiply, 2);
    bind!(__quantum__rt__bigint_negate, 1);
    bind!(__quantum__rt__bigint_power, 2);
    bind!(__quantum__rt__bigint_shiftleft, 2);
    bind!(__quantum__rt__bigint_shiftright, 2);
    bind!(__quantum__rt__bigint_subtract, 2);
    bind!(__quantum__rt__bigint_to_string, 1);
    bind!(__quantum__rt__bigint_update_reference_count, 2);
    bind!(__quantum__rt__bool_to_string, 1);
    bind!(__quantum__rt__callable_copy, 2);
    bind!(__quantum__rt__callable_create, 3);
    bind!(__quantum__rt__callable_invoke, 3);
    bind!(__quantum__rt__callable_make_adjoint, 1);
    bind!(__quantum__rt__callable_make_controlled, 1);
    bind!(__quantum__rt__callable_update_alias_count, 2);
    bind!(__quantum__rt__callable_update_reference_count, 2);
    bind!(__quantum__rt__capture_update_alias_count, 2);
    bind!(__quantum__rt__capture_update_reference_count, 2);
    bind!(__quantum__rt__double_to_string, 1);
    bind!(__quantum__rt__fail, 1);
    bind!(__quantum__rt__int_to_string, 1);
    bind!(__quantum__rt__memory_allocate, 1);
    bind!(__quantum__rt__message, 1);
    bind!(__quantum__rt__pauli_to_string, 1);
    bind!(__quantum__rt__qubit_allocate, 0);
    bind!(__quantum__rt__qubit_allocate_array, 1);
    bind!(__quantum__rt__qubit_release, 1);
    bind!(__quantum__rt__qubit_release_array, 1);
    bind!(__quantum__rt__qubit_to_string, 1);
    bind!(__quantum__rt__range_to_string, 1);
    bind!(__quantum__rt__result_equal, 2);
    bind!(__quantum__rt__result_get_one, 0);
    bind!(__quantum__rt__result_get_zero, 0);
    bind!(__quantum__rt__result_to_string, 1);
    bind!(__quantum__rt__result_update_reference_count, 2);
    bind!(__quantum__rt__string_concatenate, 2);
    bind!(__quantum__rt__string_create, 1);
    bind!(__quantum__rt__string_equal, 2);
    bind!(__quantum__rt__string_get_data, 1);
    bind!(__quantum__rt__string_get_length, 1);
    bind!(__quantum__rt__string_update_reference_count, 2);
    bind!(__quantum__rt__tuple_copy, 2);
    bind!(__quantum__rt__tuple_create, 1);
    bind!(__quantum__rt__tuple_update_alias_count, 2);
    bind!(__quantum__rt__tuple_update_reference_count, 2);

    if !(uses_legacy.iter().filter_map(|&b| b).all(|b| b)
        || uses_legacy.iter().filter_map(|&b| b).all(|b| !b))
    {
        Err("Use of legacy and current output recording functions in the same program is not supported".to_string())
    } else if declarations.is_empty() {
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
