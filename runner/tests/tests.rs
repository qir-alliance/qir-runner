// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use qir_runner::{run_bitcode, run_file};

// This test verifies the behavior of QIR execution with a series of quantum gate checks based on the Choi–Jamiołkowski Isomorphism.
// It will verify the behavior of body, adjoint, controlled, and controlled adjoint specializations of several gates against decompositions thereof,
// and additionally makes use of several features from the QIR standard library, namely callables, tuples, arrays, and strings.
// It is based on code from https://github.com/microsoft/qsharp-runtime/tree/main/src/Simulation/TargetDefinitions/Tests.
#[test]
fn test_choi_jamiolkowski_isomorphism() -> Result<(), String> {
    let bitcode = include_bytes!("resources/cji.bc");
    run_bitcode(bitcode, None)
}

// This test confirms selection of a particular named entry point from a file with two entry points
// works as expected. The function invoked simply returns a hard coded integer, which should get displayed
// to stdout, making use of stdlib functions for int to string and message.
#[test]
fn test_full_qir_simple() -> Result<(), String> {
    let bitcode = include_bytes!("resources/full-qir.bc");
    run_bitcode(bitcode, Some("QIR_App_Test__Simple"))
}

// This test confirms selection of the other named entry point from a file with two entry points
// works as expected. The function invoked uses single qubit allocation, qubit array allocation, callables,
// multi-controlled gates, adjoint gates, and joint measurement with mixed Pauli bases, and acts as
// a broad smoke test for the backend. It returns a measurement result, which should get displayed
// to stdout, making use of backend result to string and stdlib message functionality.
#[test]
fn test_full_qir_other() -> Result<(), String> {
    let bitcode = include_bytes!("resources/full-qir.bc");
    run_bitcode(bitcode, Some("QIR_App_Test__Other"))
}

// This tests a simple Bernstein-Vazirani algorithm looking for the encoded pattern |110⟩ in a qubit array.
// It should achieve the desired state with 100% probability, so the program asserts that the qubits are
// in that state, which will trigger a panic with a message like "Qubit in invalid state. Expecting: One"
// if that state is not achieved.
#[test]
fn test_bernstein_vazirani() -> Result<(), String> {
    let bitcode = include_bytes!("resources/bv.bc");
    run_bitcode(bitcode, None)
}

// This tests support for range operations `__quantum__rt__array_slice_1d` and `__quantum__rt__range_to_string`
// since those two operations are defined in LLVM IR instead of Rust. As such, they are compiled into the stdlib
// and linked by the runner in a manner different than all the other functions. The test verifies that a slice of
// an array has the expected values and prints a calculated range to stdout. Range support is not exhaustively
// tested (that is left to unit tests in the stdlib), this just verifies the function linking works as expected
// for these two special cases.
#[test]
fn test_ranges() -> Result<(), String> {
    let bitcode = include_bytes!("resources/ranges.bc");
    run_bitcode(bitcode, None)
}

// This test runs a sample Shor's algorithm for integer factorization. It makes use of quantum execution
// from backend and BigInt support from stdlib. It prints to stdout the value being factored, progress
// and number of retries followed by a tuple of the identified prime factors.
#[test]
fn test_shor() -> Result<(), String> {
    let bitcode = include_bytes!("resources/shor.bc");
    run_bitcode(bitcode, None)
}

#[test]
fn run_file_errors_on_invalid_ext() {
    let result = run_file("/some/bad/path", None);
    assert!(result.is_err());
    assert_eq!("Unsupported file extension 'None'.", result.unwrap_err());
}

#[test]
fn run_file_errors_on_missing_file() {
    let result = run_file("/some/bad/path.ll", None);
    assert!(result.is_err());
    assert_eq!(
        "no such file or directory",
        result.unwrap_err().to_lowercase()
    );
}

#[test]
fn run_file_errors_on_missing_binding() {
    let bitcode = include_bytes!("resources/missing-intrinsic.bc");
    let result = run_bitcode(bitcode, None);
    assert!(result.is_err());
    assert_eq!(
        "failed to link some declared functions: __quantum__qis__mycustomintrinsic__body",
        result.unwrap_err().to_lowercase()
    );
}

#[test]
fn legacy_output_recording_calls_are_mapped() {
    let bitcode = include_bytes!("resources/legacy_output.bc");
    if let Err(msg) = run_bitcode(bitcode, None) {
        panic!("legacy output recording mapping failed: {msg}")
    }
}

#[test]
fn deprecated_output_array_start_returns_error() {
    let bitcode = include_bytes!("resources/deprecated_output_array_start.bc");
    if let Err(msg) = run_bitcode(bitcode, None) {
        println!("{msg}");
        assert!(msg.contains("Found: __quantum__rt__array_start_record_output"))
    }
}

#[test]
fn deprecated_output_array_end_returns_error() {
    let bitcode = include_bytes!("resources/deprecated_output_array_end.bc");
    if let Err(msg) = run_bitcode(bitcode, None) {
        println!("{msg}");
        assert!(msg.contains("Found: __quantum__rt__array_end_record_output"))
    }
}

#[test]
fn deprecated_output_tuple_start_returns_error() {
    let bitcode = include_bytes!("resources/deprecated_output_tuple_start.bc");
    if let Err(msg) = run_bitcode(bitcode, None) {
        println!("{msg}");
        assert!(msg.contains("Found: __quantum__rt__tuple_start_record_output"))
    }
}

#[test]
fn deprecated_output_tuple_end_returns_error() {
    let bitcode = include_bytes!("resources/deprecated_output_tuple_end.bc");
    if let Err(msg) = run_bitcode(bitcode, None) {
        println!("{msg}");
        assert!(msg.contains("Found: __quantum__rt__tuple_end_record_output"))
    }
}
