// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// This file contains the native support for the multi-qubit Exp rotation gate.
// See https://learn.microsoft.com/en-us/qsharp/api/qsharp/microsoft.quantum.intrinsic.exp for details on the gate.
// This is intentionally kept separate from the main simulator implementation as it is likely to be removed
// in favor of having high level languages decompose into CNOT and single qubit rotations (see
// https://github.com/microsoft/qsharp-runtime/issues/999 and https://github.com/microsoft/QuantumLibraries/issues/579).

use crate::{SIM_STATE, ensure_sufficient_qubits};
use qir_stdlib::arrays::{
    __quantum__rt__array_get_element_ptr_1d, __quantum__rt__array_get_size_1d, QirArray,
};
use quantum_sparse_sim::exp::Pauli as SparsePauli;
use std::os::raw::{c_double, c_void};

fn map_pauli(pauli: u8) -> SparsePauli {
    match pauli & 3 {
        0 => SparsePauli::I,
        1 => SparsePauli::X,
        2 => SparsePauli::Z,
        3 => SparsePauli::Y,
        _ => unreachable!(),
    }
}

/// QIR API for applying an exponential of a multi-qubit rotation about the given Pauli axes with the given angle and qubits.
/// # Safety
///
/// This function should only be called with arrays and tuples created by the QIR runtime library.
#[allow(clippy::cast_ptr_alignment)]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn __quantum__qis__exp__body(
    paulis: *const QirArray,
    theta: c_double,
    qubits: *const QirArray,
) {
    SIM_STATE.with(|sim_state| {
        let state = &mut *sim_state.borrow_mut();

        let paulis_size = unsafe { __quantum__rt__array_get_size_1d(paulis) };
        let paulis: Vec<SparsePauli> = (0..paulis_size)
            .map(|index| {
                map_pauli(unsafe {
                    *__quantum__rt__array_get_element_ptr_1d(paulis, index).cast::<u8>()
                })
            })
            .collect();

        let qubits_size = unsafe { __quantum__rt__array_get_size_1d(qubits) };
        let targets: Vec<usize> = (0..qubits_size)
            .map(|index| {
                let qubit_id = unsafe {
                    *__quantum__rt__array_get_element_ptr_1d(qubits, index).cast::<*mut c_void>()
                        as usize
                };
                ensure_sufficient_qubits(&mut state.sim, qubit_id, &mut state.max_qubit_id);
                qubit_id
            })
            .collect();

        state.sim.exp(&paulis, theta, &targets);
    });
}

/// QIR API for applying an adjoint exponential of a multi-qubit rotation about the given Pauli axes with the given angle and qubits.
/// # Safety
///
/// This function should only be called with arrays and tuples created by the QIR runtime library.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn __quantum__qis__exp__adj(
    paulis: *const QirArray,
    theta: c_double,
    qubits: *const QirArray,
) {
    unsafe {
        __quantum__qis__exp__body(paulis, -theta, qubits);
    }
}

#[derive(Copy, Clone)]
#[repr(C)]
struct ExpArgs {
    paulis: *const QirArray,
    theta: c_double,
    qubits: *const QirArray,
}

#[allow(clippy::cast_ptr_alignment)]
unsafe fn apply_controlled_exp(
    paulis: *const QirArray,
    theta: c_double,
    ctls: *const QirArray,
    qubits: *const QirArray,
) {
    SIM_STATE.with(|sim_state| {
        let state = &mut *sim_state.borrow_mut();

        let ctls_size = unsafe { __quantum__rt__array_get_size_1d(ctls) };
        let ctls: Vec<usize> = (0..ctls_size)
            .map(|index| {
                let qubit_id = unsafe {
                    *__quantum__rt__array_get_element_ptr_1d(ctls, index).cast::<*mut c_void>()
                } as usize;
                ensure_sufficient_qubits(&mut state.sim, qubit_id, &mut state.max_qubit_id);
                qubit_id
            })
            .collect();

        let paulis_size = unsafe { __quantum__rt__array_get_size_1d(paulis) };
        let paulis: Vec<SparsePauli> = (0..paulis_size)
            .map(|index| {
                map_pauli(unsafe {
                    *__quantum__rt__array_get_element_ptr_1d(paulis, index).cast::<u8>()
                })
            })
            .collect();

        let qubits_size = unsafe { __quantum__rt__array_get_size_1d(qubits) };
        let targets: Vec<usize> = (0..qubits_size)
            .map(|index| {
                let qubit_id = unsafe {
                    *__quantum__rt__array_get_element_ptr_1d(qubits, index).cast::<*mut c_void>()
                } as usize;
                ensure_sufficient_qubits(&mut state.sim, qubit_id, &mut state.max_qubit_id);
                qubit_id
            })
            .collect();

        state.sim.mcexp(&ctls, &paulis, theta, &targets);
    });
}

/// QIR API for applying a controlled multi-qubit Pauli exponential from a runtime tuple.
/// # Safety
///
/// This function should only be called with arrays and tuples created by the QIR runtime library.
#[allow(clippy::cast_ptr_alignment)]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn __quantum__qis__exp__ctl(
    ctls: *const QirArray,
    arg_tuple: *mut *const Vec<u8>,
) {
    let args = unsafe { *arg_tuple.cast::<ExpArgs>() };
    unsafe { apply_controlled_exp(args.paulis, args.theta, ctls, args.qubits) };
}

/// QIR API for applying the adjoint controlled multi-qubit Pauli exponential from a runtime tuple.
/// # Safety
///
/// This function should only be called with arrays and tuples created by the QIR runtime library.
#[allow(clippy::cast_ptr_alignment)]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn __quantum__qis__exp__ctladj(
    ctls: *const QirArray,
    arg_tuple: *mut *const Vec<u8>,
) {
    let args = unsafe { *arg_tuple.cast::<ExpArgs>() };
    unsafe { apply_controlled_exp(args.paulis, -args.theta, ctls, args.qubits) };
}

/// QIR API for applying a controlled multi-qubit Pauli exponential with flat arguments.
/// # Safety
///
/// This function should only be called with arrays created by the QIR runtime library.
#[allow(non_snake_case)]
pub unsafe extern "C" fn __quantum__qis__exp__ctl_flat(
    paulis: *const QirArray,
    theta: c_double,
    ctls: *const QirArray,
    qubits: *const QirArray,
) {
    unsafe { apply_controlled_exp(paulis, theta, ctls, qubits) };
}

/// QIR API for applying the adjoint controlled multi-qubit Pauli exponential with flat arguments.
/// # Safety
///
/// This function should only be called with arrays created by the QIR runtime library.
#[allow(non_snake_case)]
pub unsafe extern "C" fn __quantum__qis__exp__ctladj_flat(
    paulis: *const QirArray,
    theta: c_double,
    ctls: *const QirArray,
    qubits: *const QirArray,
) {
    unsafe { apply_controlled_exp(paulis, -theta, ctls, qubits) };
}
