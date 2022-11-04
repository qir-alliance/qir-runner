// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use criterion::{criterion_group, criterion_main, Criterion};
use qir_backend::*;

macro_rules! bench_single_qubit_gate {
    ($c:ident, $qir_gate:expr, $desc:expr) => {
        $c.bench_function($desc, |b| {
            b.iter(|| {
                let mut qs = vec![];
                for _ in 0..5 {
                    let qubit = __quantum__rt__qubit_allocate();
                    $qir_gate(qubit);
                    qs.push(qubit);
                }
                for qubit in qs {
                    __quantum__rt__qubit_release(qubit)
                }
            })
        });
    };
}

pub fn x_gate(c: &mut Criterion) {
    bench_single_qubit_gate!(c, __quantum__qis__x__body, "X Gate");
}

pub fn y_gate(c: &mut Criterion) {
    bench_single_qubit_gate!(c, __quantum__qis__y__body, "Y Gate");
}

pub fn z_gate(c: &mut Criterion) {
    bench_single_qubit_gate!(c, __quantum__qis__z__body, "Z Gate");
}

pub fn h_gate(c: &mut Criterion) {
    bench_single_qubit_gate!(c, __quantum__qis__h__body, "H Gate");
}

pub fn s_gate(c: &mut Criterion) {
    bench_single_qubit_gate!(c, __quantum__qis__s__body, "S Gate");
}

pub fn sadj_gate(c: &mut Criterion) {
    bench_single_qubit_gate!(c, __quantum__qis__s__adj, "S Adj Gate");
}

pub fn t_gate(c: &mut Criterion) {
    bench_single_qubit_gate!(c, __quantum__qis__t__body, "T Gate");
}

pub fn tadj_gate(c: &mut Criterion) {
    bench_single_qubit_gate!(c, __quantum__qis__t__adj, "T Adj Gate");
}

pub fn rx_gate(c: &mut Criterion) {
    let rx_gate = |qubit| __quantum__qis__rx__body(std::f64::consts::PI / 7.0, qubit);
    bench_single_qubit_gate!(c, rx_gate, "Rx Gate");
}

pub fn ry_gate(c: &mut Criterion) {
    let ry_gate = |qubit| __quantum__qis__ry__body(std::f64::consts::PI / 7.0, qubit);
    bench_single_qubit_gate!(c, ry_gate, "Ry Gate");
}

pub fn rz_gate(c: &mut Criterion) {
    let rz_gate = |qubit| __quantum__qis__rz__body(std::f64::consts::PI / 7.0, qubit);
    bench_single_qubit_gate!(c, rz_gate, "Rz Gate");
}

criterion_group!(
    benches, x_gate, y_gate, z_gate, h_gate, s_gate, sadj_gate, t_gate, tadj_gate, rx_gate,
    ry_gate, rz_gate
);
criterion_main!(benches);
