// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// This test verifies the behavior of QIR execution with a series of quantum gate checks based on the Choi–Jamiołkowski Isomorphism.
// It will verify the behavior of body, adjoint, controlled, and controlled adjoint specializations of several gates against decompositions thereof,
// and additionally makes use of several features from the QIR standard library, namely callables, tuples, arrays, and strings.
// It is based on code from https://github.com/microsoft/qsharp-runtime/tree/main/src/Simulation/TargetDefinitions/Tests.
#[test]
fn test_choi_jamiolkowski_isomorphism() -> Result<(), String> {
    let bitcode = include_bytes!("resources/cji.bc");
    qir_runner::run_bitcode(bitcode, None)
}
