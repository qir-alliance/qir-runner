// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use criterion::{Criterion, criterion_group, criterion_main};
use qir_backend::*;

/// Benchmarks large number of qubit allocations and releases.
pub fn allocate_release(c: &mut Criterion) {
    c.bench_function("Allocate-Release 2k qubits", |b| {
        b.iter(|| {
            // Prepare a uniform superposition.
            let qs = __quantum__rt__qubit_allocate_array(2_000);
            unsafe {
                __quantum__rt__qubit_release_array(qs);
            }
        })
    });
}

criterion_group!(benches, allocate_release);
criterion_main!(benches);
