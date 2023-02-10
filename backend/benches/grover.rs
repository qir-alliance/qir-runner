// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::ffi::c_void;

use criterion::{criterion_group, criterion_main, Criterion};
use qir_backend::*;

/// Benchmarks a simple Grover's search on 2 qubits for the pattern |01⟩. This has the benefit of
/// being a deterministic instance of the algorithm, such that the correct pattern is always found with 100% probability.
pub fn grover(c: &mut Criterion) {
    c.bench_function("Simple Grover's Search Program", |b| {
        b.iter(|| {
            // Prepare a uniform superposition.
            __quantum__qis__h__body(std::ptr::null_mut());
            __quantum__qis__h__body(1 as *mut c_void);

            // Reflect about the marked state, using an auxiliary qubit.
            __quantum__qis__x__body(2 as *mut c_void);
            __quantum__qis__h__body(2 as *mut c_void);
            __quantum__qis__x__body(std::ptr::null_mut());
            __quantum__qis__h__body(2 as *mut c_void);
            __quantum__qis__t__adj(std::ptr::null_mut());
            __quantum__qis__t__adj(1 as *mut c_void);
            __quantum__qis__cnot__body(2 as *mut c_void, std::ptr::null_mut());
            __quantum__qis__t__body(std::ptr::null_mut());
            __quantum__qis__cnot__body(1 as *mut c_void, 2 as *mut c_void);
            __quantum__qis__cnot__body(1 as *mut c_void, std::ptr::null_mut());
            __quantum__qis__t__body(2 as *mut c_void);
            __quantum__qis__t__adj(std::ptr::null_mut());
            __quantum__qis__cnot__body(1 as *mut c_void, 2 as *mut c_void);
            __quantum__qis__cnot__body(2 as *mut c_void, std::ptr::null_mut());
            __quantum__qis__t__adj(2 as *mut c_void);
            __quantum__qis__t__body(std::ptr::null_mut());
            __quantum__qis__cnot__body(1 as *mut c_void, std::ptr::null_mut());
            __quantum__qis__h__body(2 as *mut c_void);
            __quantum__qis__x__body(std::ptr::null_mut());
            __quantum__qis__h__body(2 as *mut c_void);
            __quantum__qis__x__body(2 as *mut c_void);

            // Reflect about the uniform superposition state.
            __quantum__qis__h__body(1 as *mut c_void);
            __quantum__qis__h__body(std::ptr::null_mut());
            __quantum__qis__x__body(std::ptr::null_mut());
            __quantum__qis__x__body(1 as *mut c_void);
            __quantum__qis__cz__body(std::ptr::null_mut(), 1 as *mut c_void);
            __quantum__qis__x__body(1 as *mut c_void);
            __quantum__qis__x__body(std::ptr::null_mut());
            __quantum__qis__h__body(std::ptr::null_mut());
            __quantum__qis__h__body(1 as *mut c_void);

            // At this point, the qubits should be in the pattern state with 100% probability.
            // Take advantage of that by unpreparing the pattern (including the accumulated phase)
            // and assert both qubits are back in the |0⟩ state.
            __quantum__qis__z__body(1 as *mut c_void);
            __quantum__qis__x__body(1 as *mut c_void);
            assert!(qubit_is_zero(std::ptr::null_mut()));
            assert!(qubit_is_zero(1 as *mut c_void));
        })
    });
}

criterion_group!(benches, grover);
criterion_main!(benches);
