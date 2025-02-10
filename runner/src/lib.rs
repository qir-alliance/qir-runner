// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#![deny(clippy::all, clippy::pedantic)]
#![allow(unused)]

use std::{
    borrow::Cow,
    error::Error,
    ffi::{c_char, c_void, CStr, CString},
    mem::MaybeUninit,
    ptr::null,
    sync::Once,
};

mod cli;
pub use cli::main;

pub use qir_backend::{
    arrays::*, bigints::*, callables::*, exp::*, math::*, output_recording::*, range_support::*,
    result_bool::*, strings::*, tuples::*, *,
};

use inkwell::{
    attributes::AttributeLoc,
    context::Context,
    execution_engine::ExecutionEngine,
    llvm_sys::{
        analysis::{LLVMVerifierFailureAction, LLVMVerifyModule},
        core::LLVMDisposeMessage,
        prelude::LLVMModuleRef,
        target::{
            LLVMInitializeWebAssemblyAsmParser, LLVMInitializeWebAssemblyAsmPrinter,
            LLVMInitializeWebAssemblyDisassembler, LLVMInitializeWebAssemblyTarget,
            LLVMInitializeWebAssemblyTargetInfo, LLVMInitializeWebAssemblyTargetMC,
        },
        target_machine::{
            LLVMCodeGenFileType, LLVMCodeGenOptLevel, LLVMCodeModel, LLVMCreateTargetMachine,
            LLVMGetTargetFromTriple, LLVMRelocMode, LLVMTargetMachineEmitToFile,
        },
    },
    memory_buffer::MemoryBuffer,
    module::Module,
    passes::{PassBuilderOptions, PassManager},
    targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple},
    values::FunctionValue,
    OptimizationLevel,
};
use std::{
    collections::HashMap,
    ffi::OsStr,
    io::{Read, Write},
    path::Path,
    ptr::null_mut,
};

use wasmtime::{ArrayRef, Caller, Engine, Linker, Rooted, Store};

static LLVM_INIT: Once = Once::new();

pub(crate) fn ensure_init() {
    LLVM_INIT.call_once(|| unsafe {
        LLVMInitializeWebAssemblyTargetInfo();
        LLVMInitializeWebAssemblyTarget();
        LLVMInitializeWebAssemblyTargetMC();
        LLVMInitializeWebAssemblyAsmPrinter();
        LLVMInitializeWebAssemblyAsmParser();
        LLVMInitializeWebAssemblyDisassembler();
    });
}

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
    rng_seed: Option<u64>,
    output_writer: &mut impl Write,
) -> Result<(), String> {
    if let Some(seed) = rng_seed {
        qir_backend::set_rng_seed(seed);
    }
    let context = Context::create();
    let module = load_file(path, &context)?;
    run_module(&module, entry_point, shots, output_writer)
}

/// # Errors
///
/// Will return `Err` if
/// - `bytes` does not contain a valid bitcode module
/// - `entry_point` is not found in the QIR
/// - Entry point has parameters or a non-void return type.
pub fn run_bitcode(
    bytes: &[u8],
    entry_point: Option<&str>,
    shots: u32,
    output_writer: &mut impl Write,
) -> Result<(), String> {
    let context = Context::create();
    let buffer = MemoryBuffer::create_from_memory_range(bytes, "");
    let module = Module::parse_bitcode_from_buffer(&buffer, &context).map_err(|e| e.to_string())?;
    run_module(&module, entry_point, shots, output_writer)
}

fn to_c_str(mut s: &str) -> Cow<'_, CStr> {
    if s.is_empty() {
        s = "\0";
    }
    if !s.chars().rev().any(|ch| ch == '\0') {
        return Cow::from(CString::new(s).expect("CString::new failed"));
    }

    unsafe { Cow::from(CStr::from_ptr(s.as_ptr() as *const _)) }
}

pub unsafe fn verify(module: LLVMModuleRef) -> Option<String> {
    let action = LLVMVerifierFailureAction::LLVMReturnStatusAction;
    let mut error = std::ptr::null_mut();
    if LLVMVerifyModule(module, action, &mut error) == 0 {
        None
    } else {
        let error_cstr = CStr::from_ptr(error);
        let string = error_cstr.to_str().unwrap().to_string();
        LLVMDisposeMessage(error);
        Some(string)
    }
}

unsafe fn write_wasm_to_file(module: LLVMModuleRef, file_path: &str) -> Result<(), String> {
    if let Some(error) = verify(module) {
        return Err(error);
    }

    ensure_init();

    let level = LLVMCodeGenOptLevel::LLVMCodeGenLevelAggressive;
    let reloc_mode = LLVMRelocMode::LLVMRelocDynamicNoPic;
    let code_model = LLVMCodeModel::LLVMCodeModelLarge;

    let triple = "wasm32-unknown-unknown";
    let mut target = std::ptr::null_mut();
    let mut err_string = MaybeUninit::uninit();
    let success =
        LLVMGetTargetFromTriple(triple.as_ptr().cast(), &mut target, err_string.as_mut_ptr());

    if success != 0 {
        let error_ptr = err_string.assume_init();
        let message = CStr::from_ptr(error_ptr)
            .to_str()
            .expect("Failed to get error string");
        let message_string = message.to_string();
        LLVMDisposeMessage(error_ptr);
        return Err(message_string);
    }

    let target_machine = unsafe {
        LLVMCreateTargetMachine(
            target,
            triple.as_ptr().cast(),
            "generic\0".as_ptr().cast(),
            null(),
            level.into(),
            reloc_mode.into(),
            code_model.into(),
        )
    };

    if target_machine.is_null() {
        return Err("Failed to create target machine".to_string());
    }
    let mut err_string = MaybeUninit::uninit();
    
    let file = to_c_str(file_path);
    let success: i32 = LLVMTargetMachineEmitToFile(
        target_machine,
        module,
        file.as_ptr().cast_mut().cast(),
        LLVMCodeGenFileType::LLVMObjectFile,
        err_string.as_mut_ptr(),
    );

    if success != 0 {
        let error_ptr = err_string.assume_init();
        let message = CStr::from_ptr(error_ptr)
            .to_str()
            .expect("Failed to get error string");
        let message_string = message.to_string();
        LLVMDisposeMessage(error_ptr);
        return Err(message_string);
    }
    // TODO: Need to link the wasm file to get it fully compiled.
    Ok(())
}

fn run_module(
    module: &Module,
    entry_point: Option<&str>,
    shots: u32,
    output_writer: &mut impl Write,
) -> Result<(), String> {
    module
        .verify()
        .map_err(|e| format!("Failed to verify module: {}", e.to_string()))?;

    let entry_point = choose_entry_point(module_functions(module), entry_point)?;
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
    let entry_point_name = entry_point.get_name().to_string_lossy().to_string();

    use tempfile::NamedTempFile;
    let mut temp_file = NamedTempFile::new().unwrap();
    let temp_path = temp_file.path().to_string_lossy().into_owned();

    let mut buffer = Vec::new();
    unsafe {
        write_wasm_to_file(module.as_mut_ptr(), &temp_path)?;
        temp_file.read_to_end(&mut buffer).unwrap();
    }

    let engine = Engine::default();
    struct MarkerData {};
    let mut store = Store::new(&engine, MarkerData {});
    let mut linker = Linker::new(&engine);
  

    bind_functions( &linker)?;

    let module = wasmtime::Module::new(&engine, &buffer).map_err(|e| format!("{e}"))?;
    let instance = linker
        .instantiate(&mut store, &module)
        .map_err(|e| format!("{e}"))?;
    
    let main = instance
        .get_typed_func::<(), ()>(&mut store, &entry_point_name)
        .unwrap();

    for _ in 1..=shots {
        output_writer
            .write_all("START\n".as_bytes())
            .expect("Failed to write output");
        for attr in &attrs {
            output_writer
                .write_all(format!("METADATA\t{}", attr.0).as_bytes())
                .expect("Failed to write output");
            if !attr.1.is_empty() {
                output_writer
                    .write_all(format!("\t{}", attr.1).as_bytes())
                    .expect("Failed to write output");
            }
            output_writer
                .write_all(qir_stdlib::output_recording::LINE_ENDING)
                .expect("Failed to write output");
        }

        __quantum__rt__initialize(null_mut());
        main.call(&mut store, ()).map_err(|e| format!("{e}"))?;

        // Write the saved output records to the output_writer
        OUTPUT.with(|output| {
            let mut output = output.borrow_mut();
            output_writer
                .write_all(output.drain().as_slice())
                .expect("Failed to write output");
        });

        // Write the end of the shot
        output_writer
            .write_all("END\t0".as_bytes())
            .expect("Failed to write output");
        output_writer
            .write_all(qir_stdlib::output_recording::LINE_ENDING)
            .expect("Failed to write output");
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
    if entry_point.count_params() == 0 {
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

fn run_basic_passes_on(
    module: &Module,
    target_triple: &TargetTriple,
    target: &Target,
) -> Result<(), String> {
    // Description of this syntax:
    // https://github.com/llvm/llvm-project/blob/2ba08386156ef25913b1bee170d8fe95aaceb234/llvm/include/llvm/Passes/PassBuilder.h#L308-L347
    const BASIC_PASS_PIPELINE: &str = "globaldce,strip-dead-prototypes";

    // Boilerplate taken from here:
    // https://github.com/TheDan64/inkwell/blob/5c9f7fcbb0a667f7391b94beb65f1a670ad13221/examples/kaleidoscope/main.rs#L86-L95
    let target_machine = target
        .create_target_machine(
            target_triple,
            "generic",
            "",
            OptimizationLevel::None,
            RelocMode::Default,
            CodeModel::Default,
        )
        .ok_or("Unable to create TargetMachine from Target")?;
    module
        .run_passes(
            BASIC_PASS_PIPELINE,
            &target_machine,
            PassBuilderOptions::create(),
        )
        .map_err(|e| e.to_string())
}

#[allow(clippy::too_many_lines)]
fn bind_functions<Simulator>(
    linker: &Linker<MarkerData>,
) -> Result<(), Box<dyn Error>> {
    linker
    .func_wrap(
        "env",
        "__linear_memory",
        || {
            
        },
    )
    .unwrap();
    linker
        .func_wrap(
            "env",
            "__quantum__rt__initialize",
            |_: i32| {
                __quantum__rt__initialize(null_mut());
            },
        )
        .unwrap();

    linker
        .func_wrap("env", "__quantum__qis__h__body", |qubit: i32| {
            __quantum__qis__h__body(qubit as *mut c_void);
        })
        .unwrap();

    linker
        .func_wrap("env", "__quantum__qis__x__body", |qubit: i32| {
            __quantum__qis__x__body(qubit as *mut c_void);
        })
        .unwrap();
    linker
        .func_wrap("env", "__quantum__qis__y__body", |qubit: i32| {
            __quantum__qis__y__body(qubit as *mut c_void);
        })
        .unwrap();
    linker
        .func_wrap("env", "__quantum__qis__z__body", |qubit: i32| {
            __quantum__qis__z__body(qubit as *mut c_void);
        })
        .unwrap();
    linker
        .func_wrap("env", "__quantum__qis__t__body", |qubit: i32| {
            __quantum__qis__t__body(qubit as *mut c_void);
        })
        .unwrap();
    linker
        .func_wrap("env", "__quantum__qis__s__body", |qubit: i32| {
            __quantum__qis__s__body(qubit as *mut c_void);
        })
        .unwrap();
    linker
        .func_wrap(
            "env",
            "__quantum__qis__cx__body",
            |control: i32, target: i32| {
                __quantum__qis__cx__body(control as *mut c_void, target as *mut c_void);
            },
        )
        .unwrap();
    linker
        .func_wrap(
            "env",
            "__quantum__qis__cy__body",
            |control: i32, target: i32| {
                __quantum__qis__cy__body(control as *mut c_void, target as *mut c_void);
            },
        )
        .unwrap();
    linker
        .func_wrap(
            "env",
            "__quantum__qis__cz__body",
            |control: i32, target: i32| {
                __quantum__qis__cz__body(control as *mut c_void, target as *mut c_void);
            },
        )
        .unwrap();
    linker
    .func_wrap(
        "env",
        "__quantum__qis__ccx__body",
        |control1: i32, control2:i32, target: i32| {
            __quantum__qis__ccx__body(control1 as *mut c_void, control2 as *mut c_void, target as *mut c_void);
        },
    )
    .unwrap();
    linker
        .func_wrap(
            "env",
            "__quantum__qis__mresetz__body",
            |qubit: i32, result: i32| {
                __quantum__qis__mresetz__body(qubit as *mut c_void, result as *mut c_void);
            },
        )
        .unwrap();
    linker
        .func_wrap(
            "env",
            "__quantum__qis__read_result__body",
            |result: i32| -> i32 {
                __quantum__qis__read_result__body(result as *mut c_void) as i32
            },
        )
        .unwrap();

    linker
        .func_wrap(
            "env",
            "__quantum__rt__array_record_output",
            |value: i64, tag: i32| unsafe {
                __quantum__rt__array_record_output(value, null_mut());
            },
        )
        .unwrap();
    linker
        .func_wrap(
            "env",
            "__quantum__rt__tuple_record_output",
            |value: i64, tag: i32| unsafe {
                __quantum__rt__tuple_record_output(value, null_mut());
            },
        )
        .unwrap();
    linker
        .func_wrap(
            "env",
            "__quantum__rt__result_record_output",
            |value: i64, tag: i32| unsafe {
                __quantum__rt__result_record_output(value as *mut c_void, null_mut());
            },
        )
        .unwrap();
    linker
        .func_wrap(
            "env",
            "__quantum__rt__bool_record_output",
            |value: i32, tag: i32| unsafe {
                __quantum__rt__bool_record_output(value != 0, null_mut());
            },
        )
        .unwrap();
    linker
        .func_wrap(
            "env",
            "__quantum__rt__int_record_output",
            |value: i64, tag: i32| unsafe {
                __quantum__rt__int_record_output(value, null_mut());
            },
        )
        .unwrap();
    linker
        .func_wrap(
            "env",
            "__quantum__rt__double_record_output",
            |value: f64, tag: i32| unsafe {
                __quantum__rt__double_record_output(value, null_mut());
            },
        )
        .unwrap();

        linker
        .func_wrap(
            "env",
            "__quantum__rt__qubit_allocate",
            ||->i32{ unsafe {
                __quantum__rt__qubit_allocate() as i32
            }},
        )
        .unwrap();
    Ok(())
}
