// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// This file contains the native support for the multi-qubit Exp rotation gate.
// See https://learn.microsoft.com/en-us/qsharp/api/qsharp/microsoft.quantum.intrinsic.exp for details on the gate.
// This is intentionally kept separate from the main simulator implementation as it is likely to be removed
// in favor of having high level languages decompose into CNOT and single qubit rotations (see
// https://github.com/microsoft/qsharp-runtime/issues/999 and https://github.com/microsoft/QuantumLibraries/issues/579).

use crate::{ensure_sufficient_qubits, SIM_STATE};
use qir_stdlib::{
    arrays::{QirArray, __quantum__rt__array_get_element_ptr_1d, __quantum__rt__array_get_size_1d},
    tuples::{__quantum__rt__tuple_create, __quantum__rt__tuple_update_reference_count},
    Pauli,
};
use quantum_sparse_sim::exp::Pauli as SparsePauli;
use std::{
    mem::size_of,
    os::raw::{c_double, c_void},
};

fn map_pauli(pauli: Pauli) -> SparsePauli {
    match pauli {
        Pauli::I => SparsePauli::I,
        Pauli::X => SparsePauli::X,
        Pauli::Z => SparsePauli::Z,
        Pauli::Y => SparsePauli::Y,
    }
}

/// QIR API for applying an exponential of a multi-qubit rotation about the given Pauli axes with the given angle and qubits.
/// # Safety
///
/// This function should only be called with arrays and tuples created by the QIR runtime library.
#[allow(clippy::cast_ptr_alignment)]
#[no_mangle]
pub unsafe extern "C" fn __quantum__qis__exp__body(
    paulis: *const QirArray,
    theta: c_double,
    qubits: *const QirArray,
) {
    SIM_STATE.with(|sim_state| {
        let state = &mut *sim_state.borrow_mut();

        let paulis_size = __quantum__rt__array_get_size_1d(paulis);
        let paulis: Vec<SparsePauli> = (0..paulis_size)
            .map(|index| {
                map_pauli(*__quantum__rt__array_get_element_ptr_1d(paulis, index).cast::<Pauli>())
            })
            .collect();

        let qubits_size = __quantum__rt__array_get_size_1d(qubits);
        let targets: Vec<usize> = (0..qubits_size)
            .map(|index| {
                let qubit_id = *__quantum__rt__array_get_element_ptr_1d(qubits, index)
                    .cast::<*mut c_void>() as usize;
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
#[no_mangle]
pub unsafe extern "C" fn __quantum__qis__exp__adj(
    paulis: *const QirArray,
    theta: c_double,
    qubits: *const QirArray,
) {
    __quantum__qis__exp__body(paulis, -theta, qubits);
}

#[derive(Copy, Clone)]
#[repr(C)]
struct ExpArgs {
    paulis: *const QirArray,
    theta: c_double,
    qubits: *const QirArray,
}

/// QIR API for applying an exponential of a multi-qubit rotation about the given Pauli axes with the given angle and qubits.
/// # Safety
///
/// This function should only be called with arrays and tuples created by the QIR runtime library.
#[allow(clippy::cast_ptr_alignment)]
#[no_mangle]
pub unsafe extern "C" fn __quantum__qis__exp__ctl(
    ctls: *const QirArray,
    arg_tuple: *mut *const Vec<u8>,
) {
    SIM_STATE.with(|sim_state| {
        let state = &mut *sim_state.borrow_mut();
        let args = *arg_tuple.cast::<ExpArgs>();

        let ctls_size = __quantum__rt__array_get_size_1d(ctls);
        let ctls: Vec<usize> = (0..ctls_size)
            .map(|index| {
                let qubit_id = *__quantum__rt__array_get_element_ptr_1d(ctls, index)
                    .cast::<*mut c_void>() as usize;
                ensure_sufficient_qubits(&mut state.sim, qubit_id, &mut state.max_qubit_id);
                qubit_id
            })
            .collect();

        let paulis_size = __quantum__rt__array_get_size_1d(args.paulis);
        let paulis: Vec<SparsePauli> = (0..paulis_size)
            .map(|index| {
                map_pauli(
                    *__quantum__rt__array_get_element_ptr_1d(args.paulis, index).cast::<Pauli>(),
                )
            })
            .collect();

        let qubits_size = __quantum__rt__array_get_size_1d(args.qubits);
        let targets: Vec<usize> = (0..qubits_size)
            .map(|index| {
                let qubit_id = *__quantum__rt__array_get_element_ptr_1d(args.qubits, index)
                    .cast::<*mut c_void>() as usize;
                ensure_sufficient_qubits(&mut state.sim, qubit_id, &mut state.max_qubit_id);
                qubit_id
            })
            .collect();

        state.sim.mcexp(&ctls, &paulis, args.theta, &targets);
    });
}

/// QIR API for applying an exponential of a multi-qubit rotation about the given Pauli axes with the given angle and qubits.
/// # Safety
///
/// This function should only be called with arrays and tuples created by the QIR runtime library.
#[allow(clippy::cast_ptr_alignment)]
#[no_mangle]
pub unsafe extern "C" fn __quantum__qis__exp__ctladj(
    ctls: *const QirArray,
    arg_tuple: *mut *const Vec<u8>,
) {
    let args = *arg_tuple.cast::<ExpArgs>();
    let new_args = ExpArgs {
        paulis: args.paulis,
        theta: -args.theta,
        qubits: args.qubits,
    };
    let new_arg_tuple = __quantum__rt__tuple_create(size_of::<ExpArgs>() as u64);
    *new_arg_tuple.cast::<ExpArgs>() = new_args;
    __quantum__qis__exp__ctl(ctls, new_arg_tuple);
    __quantum__rt__tuple_update_reference_count(new_arg_tuple, -1);
}
