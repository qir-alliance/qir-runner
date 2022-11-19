// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#![deny(clippy::all, clippy::pedantic)]

//! Module defining QIR compliant APIs for quantum simulation.

pub mod result_bool;

pub mod exp;

mod nearly_zero;
mod simulator;

use bitvec::prelude::*;
use num_complex::Complex64;
use simulator::QuantumSim;
use std::cell::RefCell;
use std::convert::TryInto;
use std::ffi::{c_void, CString};
use std::mem::size_of;
use std::os::raw::c_char;
use std::os::raw::c_double;

use result_bool::{
    __quantum__rt__result_equal, __quantum__rt__result_get_one, __quantum__rt__result_get_zero,
};

pub use qir_stdlib::{
    arrays::*, bigints::*, callables::*, math::*, output_recording::*, range_support::*,
    strings::*, tuples::*, *,
};

// Additional test infrastructure is available in matrix_testing that allows comparing the transformations
// implemented here with direct matrix application to the state vector.
#[cfg(test)]
mod matrix_testing;

struct SimulatorState {
    pub sim: QuantumSim,
    pub res: BitVec,
    pub max_qubit_id: usize,
}

thread_local! {
    static SIM_STATE: RefCell<SimulatorState> = RefCell::new(SimulatorState {
        sim: QuantumSim::default(),
        res: bitvec![],
        max_qubit_id: 0
    });
}

/// Initializes the execution environment.
#[no_mangle]
pub unsafe extern "C" fn __quantum__rt__initialize(_: *mut c_char) {
    SIM_STATE.with(|sim_state| {
        let state = &mut *sim_state.borrow_mut();
        state.sim = QuantumSim::default();
        state.res = bitvec![];
        state.max_qubit_id = 0;
    });
}

fn ensure_sufficient_qubits(sim: &mut QuantumSim, qubit_id: usize, max: &mut usize) {
    while qubit_id + 1 > *max {
        let _ = sim.allocate();
        *max += 1;
    }
}

#[allow(clippy::cast_ptr_alignment)]
unsafe fn map_paulis(
    state: &mut SimulatorState,
    paulis: *const QirArray,
    qubits: *const QirArray,
) -> Vec<(Pauli, usize)> {
    let paulis_size = __quantum__rt__array_get_size_1d(paulis);
    let qubits_size = __quantum__rt__array_get_size_1d(qubits);
    if paulis_size != qubits_size {
        __quantum__rt__fail(__quantum__rt__string_create(
            CString::new("Pauli array and Qubit array must be the same size.")
                .unwrap()
                .as_bytes_with_nul()
                .as_ptr() as *mut i8,
        ));
    }

    let combined_list: Vec<(Pauli, usize)> = (0..paulis_size)
        .filter_map(|index| {
            let p =
                *__quantum__rt__array_get_element_ptr_1d(paulis, index).cast::<Pauli>() as Pauli;
            let q = *__quantum__rt__array_get_element_ptr_1d(qubits, index as u64)
                .cast::<*mut c_void>() as usize;
            if let Pauli::I = p {
                None
            } else {
                ensure_sufficient_qubits(&mut state.sim, q, &mut state.max_qubit_id);
                Some((p, q))
            }
        })
        .collect();

    for (pauli, qubit) in &combined_list {
        match pauli {
            Pauli::X => state.sim.h(*qubit),
            Pauli::Y => {
                state.sim.h(*qubit);
                state.sim.s(*qubit);
                state.sim.h(*qubit);
            }
            _ => (),
        }
    }

    combined_list
}

fn unmap_paulis(state: &mut SimulatorState, combined_list: Vec<(Pauli, usize)>) {
    for (pauli, qubit) in combined_list {
        match pauli {
            Pauli::X => state.sim.h(qubit),
            Pauli::Y => {
                state.sim.h(qubit);
                state.sim.s(qubit);
                state.sim.h(qubit);
            }
            _ => (),
        }
    }
}

macro_rules! single_qubit_gate {
    ($(#[$meta:meta])*
    $qir_name:ident, $gate:expr) => {
        $(#[$meta])*
        #[no_mangle]
        pub extern "C" fn $qir_name(qubit: *mut c_void) {
            SIM_STATE.with(|sim_state| {
                let state = &mut *sim_state.borrow_mut();
                ensure_sufficient_qubits(&mut state.sim, qubit as usize, &mut state.max_qubit_id);

                $gate(&mut state.sim, qubit as usize);
            });
        }
    };
}

single_qubit_gate!(
    /// QIR API for performing the H gate on the given qubit.
    __quantum__qis__h__body,
    QuantumSim::h
);
single_qubit_gate!(
    /// QIR API for performing the S gate on the given qubit.
    __quantum__qis__s__body,
    QuantumSim::s
);
single_qubit_gate!(
    /// QIR API for performing the Adjoint S gate on the given qubit.
    __quantum__qis__s__adj,
    QuantumSim::sadj
);
single_qubit_gate!(
    /// QIR API for performing the T gate on the given qubit.
    __quantum__qis__t__body,
    QuantumSim::t
);
single_qubit_gate!(
    /// QIR API for performing the Adjoint T gate on the given qubit.
    __quantum__qis__t__adj,
    QuantumSim::tadj
);
single_qubit_gate!(
    /// QIR API for performing the X gate on the given qubit.
    __quantum__qis__x__body,
    QuantumSim::x
);
single_qubit_gate!(
    /// QIR API for performing the Y gate on the given qubit.
    __quantum__qis__y__body,
    QuantumSim::y
);
single_qubit_gate!(
    /// QIR API for performing the Z gate on the given qubit.
    __quantum__qis__z__body,
    QuantumSim::z
);

macro_rules! controlled_qubit_gate {
    ($(#[$meta:meta])*
    $qir_name:ident, $gate:expr, 1) => {
        $(#[$meta])*
        #[no_mangle]
        pub extern "C" fn $qir_name(control: *mut c_void, target: *mut c_void) {
            SIM_STATE.with(|sim_state| {
                let state = &mut *sim_state.borrow_mut();
                ensure_sufficient_qubits(&mut state.sim, target as usize, &mut state.max_qubit_id);
                ensure_sufficient_qubits(&mut state.sim, control as usize, &mut state.max_qubit_id);

                $gate(&mut state.sim, &[control as usize], target as usize);
            });
        }
    };

    ($(#[$meta:meta])*
    $qir_name:ident, $gate:expr, 2) => {
        $(#[$meta])*
        #[no_mangle]
        pub extern "C" fn $qir_name(
            control_1: *mut c_void,
            control_2: *mut c_void,
            target: *mut c_void,
        ) {
            SIM_STATE.with(|sim_state| {
                let state = &mut *sim_state.borrow_mut();
                ensure_sufficient_qubits(&mut state.sim, target as usize, &mut state.max_qubit_id);
                ensure_sufficient_qubits(&mut state.sim, control_1 as usize, &mut state.max_qubit_id);
                ensure_sufficient_qubits(&mut state.sim, control_2 as usize, &mut state.max_qubit_id);

                $gate(&mut state.sim, &[control_1 as usize, control_2 as usize], target as usize);
            });
        }
    };
}

controlled_qubit_gate!(
    /// QIR API for performing the CNOT gate with the given qubits.
    __quantum__qis__cnot__body,
    QuantumSim::mcx,
    1
);
controlled_qubit_gate!(
    /// QIR API for performing the CNOT gate with the given qubits.
    __quantum__qis__cx__body,
    QuantumSim::mcx,
    1
);
controlled_qubit_gate!(
    /// QIR API for performing the CCNOT gate with the given qubits.
    __quantum__qis__ccx__body,
    QuantumSim::mcx,
    2
);
controlled_qubit_gate!(
    /// QIR API for performing the CZ gate with the given qubits.
    __quantum__qis__cz__body,
    QuantumSim::mcz,
    1
);

macro_rules! single_qubit_rotation {
    ($(#[$meta:meta])*
    $qir_name:ident, $gate:expr) => {
        $(#[$meta])*
        #[no_mangle]
        pub extern "C" fn $qir_name(theta: c_double, qubit: *mut c_void) {
            SIM_STATE.with(|sim_state| {
                let state = &mut *sim_state.borrow_mut();
                ensure_sufficient_qubits(&mut state.sim, qubit as usize, &mut state.max_qubit_id);

                $gate(&mut state.sim, theta, qubit as usize);
            });
        }
    };
}

single_qubit_rotation!(
    /// QIR API for applying a Pauli-X rotation with the given angle and qubit.
    __quantum__qis__rx__body,
    QuantumSim::rx
);
single_qubit_rotation!(
    /// QIR API for applying a Pauli-Y rotation with the given angle and qubit.
    __quantum__qis__ry__body,
    QuantumSim::ry
);
single_qubit_rotation!(
    /// QIR API for applying a Pauli-Z rotation with the given angle and qubit.
    __quantum__qis__rz__body,
    QuantumSim::rz
);

macro_rules! multicontrolled_qubit_gate {
    ($(#[$meta:meta])*
    $qir_name:ident, $gate:expr) => {
        $(#[$meta])*
        /// # Safety
        ///
        /// This function should only be called with arrays and tuples created by the QIR runtime library.
        #[no_mangle]
        pub unsafe extern "C" fn $qir_name(ctls: *const QirArray, qubit: *mut c_void) {
            SIM_STATE.with(|sim_state| {
                let state = &mut *sim_state.borrow_mut();
                ensure_sufficient_qubits(&mut state.sim, qubit as usize, &mut state.max_qubit_id);
                let ctls_size = __quantum__rt__array_get_size_1d(ctls);
                let ctls_list: Vec<usize> = (0..ctls_size)
                    .map(|index| {
                        let q = *__quantum__rt__array_get_element_ptr_1d(ctls, index)
                            .cast::<*mut c_void>() as usize;
                        ensure_sufficient_qubits(&mut state.sim, q, &mut state.max_qubit_id);
                        q
                    })
                    .collect();

                $gate(&mut state.sim, &ctls_list, qubit as usize);
            });
        }
    };
}

multicontrolled_qubit_gate!(
    /// QIR API for performing the multicontrolled H gate with the given qubits.
    __quantum__qis__h__ctl,
    QuantumSim::mch
);
multicontrolled_qubit_gate!(
    /// QIR API for performing the multicontrolled S gate with the given qubits.
    __quantum__qis__s__ctl,
    QuantumSim::mcs
);
multicontrolled_qubit_gate!(
    /// QIR API for performing the multicontrolled Adjoint S gate with the given qubits.
    __quantum__qis__s__ctladj,
    QuantumSim::mcsadj
);
multicontrolled_qubit_gate!(
    /// QIR API for performing the multicontrolled T gate with the given qubits.
    __quantum__qis__t__ctl,
    QuantumSim::mct
);
multicontrolled_qubit_gate!(
    /// QIR API for performing the multicontrolled Adjoint T gate with the given qubits.
    __quantum__qis__t__ctladj,
    QuantumSim::mctadj
);
multicontrolled_qubit_gate!(
    /// QIR API for performing the multicontrolled X gate with the given qubits.
    __quantum__qis__x__ctl,
    QuantumSim::mcx
);
multicontrolled_qubit_gate!(
    /// QIR API for performing the multicontrolled Y gate with the given qubits.
    __quantum__qis__y__ctl,
    QuantumSim::mcy
);
multicontrolled_qubit_gate!(
    /// QIR API for performing the multicontrolled Z gate with the given qubits.
    __quantum__qis__z__ctl,
    QuantumSim::mcz
);

#[derive(Copy, Clone)]
#[repr(C)]
struct RotationArgs {
    theta: c_double,
    qubit: *mut c_void,
}

macro_rules! multicontrolled_qubit_rotation {
    ($(#[$meta:meta])*
    $qir_name:ident, $gate:expr) => {
        $(#[$meta])*
        /// # Safety
        ///
        /// This function should only be called with arrays and tuples created by the QIR runtime library.
        #[no_mangle]
        pub unsafe extern "C" fn $qir_name(
            ctls: *const QirArray,
            arg_tuple: *mut *const Vec<u8>,
        ) {
            SIM_STATE.with(|sim_state| {
                let state = &mut *sim_state.borrow_mut();

                let args = *arg_tuple.cast::<RotationArgs>();

                ensure_sufficient_qubits(&mut state.sim, args.qubit as usize, &mut state.max_qubit_id);
                let ctls_size = __quantum__rt__array_get_size_1d(ctls);
                let ctls_list: Vec<usize> = (0..ctls_size)
                    .map(|index| {
                        let q = *__quantum__rt__array_get_element_ptr_1d(ctls, index)
                            .cast::<*mut c_void>() as usize;
                        ensure_sufficient_qubits(&mut state.sim, q, &mut state.max_qubit_id);
                        q
                    })
                    .collect();

                $gate(
                    &mut state.sim,
                    &ctls_list,
                    args.theta,
                    args.qubit as usize,
                );
            });
        }
    };
}

multicontrolled_qubit_rotation!(
    /// QIR API for applying a multicontrolled Pauli-X rotation with the given angle and qubit.
    __quantum__qis__rx__ctl,
    QuantumSim::mcrx
);
multicontrolled_qubit_rotation!(
    /// QIR API for applying a multicontrolled Pauli-Y rotation with the given angle and qubit.
    __quantum__qis__ry__ctl,
    QuantumSim::mcry
);
multicontrolled_qubit_rotation!(
    /// QIR API for applying a multicontrolled Pauli-Z rotation with the given angle and qubit.
    __quantum__qis__rz__ctl,
    QuantumSim::mcrz
);

/// QIR API for applying a rotation about the given Pauli axis with the given angle and qubit.
#[no_mangle]
pub extern "C" fn __quantum__qis__r__body(pauli: Pauli, theta: c_double, qubit: *mut c_void) {
    match pauli {
        Pauli::I => (),
        Pauli::X => __quantum__qis__rx__body(theta, qubit),
        Pauli::Y => __quantum__qis__ry__body(theta, qubit),
        Pauli::Z => __quantum__qis__rz__body(theta, qubit),
    }
}

/// QIR API for applying an adjoint rotation about the given Pauli axis with the given angle and qubit.
#[no_mangle]
pub extern "C" fn __quantum__qis__r__adj(pauli: Pauli, theta: c_double, qubit: *mut c_void) {
    __quantum__qis__r__body(pauli, -theta, qubit);
}

#[derive(Copy, Clone)]
#[repr(C)]
struct PauliRotationArgs {
    pauli: Pauli,
    theta: c_double,
    qubit: *mut c_void,
}

/// QIR API for applying a controlled rotation about the given Pauli axis with the given angle and qubit.
/// # Safety
///
/// This function should only be called with arrays and tuples created by the QIR runtime library.
#[allow(clippy::cast_ptr_alignment)]
#[no_mangle]
pub unsafe extern "C" fn __quantum__qis__r__ctl(
    ctls: *const QirArray,
    arg_tuple: *mut *const Vec<u8>,
) {
    let args = *arg_tuple.cast::<PauliRotationArgs>();
    let rot_args = RotationArgs {
        theta: args.theta,
        qubit: args.qubit,
    };
    let rot_arg_tuple = __quantum__rt__tuple_create(size_of::<RotationArgs>() as u64);
    *rot_arg_tuple.cast::<RotationArgs>() = rot_args;

    match args.pauli {
        Pauli::X => __quantum__qis__rx__ctl(ctls, rot_arg_tuple),
        Pauli::Y => __quantum__qis__ry__ctl(ctls, rot_arg_tuple),
        Pauli::Z => __quantum__qis__rz__ctl(ctls, rot_arg_tuple),
        Pauli::I => {
            if __quantum__rt__array_get_size_1d(ctls) > 0 {
                SIM_STATE.with(|sim_state| {
                    let state = &mut *sim_state.borrow_mut();

                    ensure_sufficient_qubits(
                        &mut state.sim,
                        args.qubit as usize,
                        &mut state.max_qubit_id,
                    );
                    let ctls_size = __quantum__rt__array_get_size_1d(ctls);
                    let ctls_list: Vec<usize> = (0..ctls_size)
                        .map(|index| {
                            let q = *__quantum__rt__array_get_element_ptr_1d(ctls, index)
                                .cast::<*mut c_void>() as usize;
                            ensure_sufficient_qubits(&mut state.sim, q, &mut state.max_qubit_id);
                            q
                        })
                        .collect();

                    if let Some((head, rest)) = ctls_list.split_first() {
                        state.sim.mcphase(
                            rest,
                            Complex64::exp(Complex64::new(0.0, -args.theta / 2.0)),
                            *head,
                        );
                    }
                });
            }
        }
    };

    __quantum__rt__tuple_update_reference_count(rot_arg_tuple, -1);
}

/// QIR API for applying an adjoint controlled rotation about the given Pauli axis with the given angle and qubit.
/// # Safety
///
/// This function should only be called with arrays and tuples created by the QIR runtime library.
#[no_mangle]
pub unsafe extern "C" fn __quantum__qis__r__ctladj(
    ctls: *const QirArray,
    arg_tuple: *mut *const Vec<u8>,
) {
    let args = *arg_tuple.cast::<PauliRotationArgs>();
    let new_args = PauliRotationArgs {
        pauli: args.pauli,
        theta: -args.theta,
        qubit: args.qubit,
    };
    let new_arg_tuple = __quantum__rt__tuple_create(size_of::<PauliRotationArgs>() as u64);
    *new_arg_tuple.cast::<PauliRotationArgs>() = new_args;
    __quantum__qis__r__ctl(ctls, new_arg_tuple);
    __quantum__rt__tuple_update_reference_count(new_arg_tuple, -1);
}

/// QIR API for applying a SWAP gate to the given qubits.
#[no_mangle]
pub extern "C" fn __quantum__qis__swap__body(qubit1: *mut c_void, qubit2: *mut c_void) {
    SIM_STATE.with(|sim_state| {
        let state = &mut *sim_state.borrow_mut();
        ensure_sufficient_qubits(&mut state.sim, qubit1 as usize, &mut state.max_qubit_id);
        ensure_sufficient_qubits(&mut state.sim, qubit2 as usize, &mut state.max_qubit_id);

        state.sim.swap_qubit_ids(qubit1 as usize, qubit2 as usize);
    });
}

/// QIR API for resetting the given qubit in the computational basis.
#[no_mangle]
pub extern "C" fn __quantum__qis__reset__body(qubit: *mut c_void) {
    SIM_STATE.with(|sim_state| {
        let state = &mut *sim_state.borrow_mut();
        ensure_sufficient_qubits(&mut state.sim, qubit as usize, &mut state.max_qubit_id);

        if state.sim.measure(qubit as usize) {
            state.sim.x(qubit as usize);
        }
    });
}

/// QIR API for measuring the given qubit in the computation basis and storing the measured value with the given result identifier.
#[no_mangle]
pub extern "C" fn __quantum__qis__mz__body(qubit: *mut c_void, result: *mut c_void) {
    SIM_STATE.with(|sim_state| {
        let state = &mut *sim_state.borrow_mut();
        let res_id = result as usize;
        ensure_sufficient_qubits(&mut state.sim, qubit as usize, &mut state.max_qubit_id);

        if state.res.len() < res_id + 1 {
            state.res.resize(res_id + 1, false);
        }

        *state
            .res
            .get_mut(res_id)
            .expect("Result with given id missing after expansion.") =
            state.sim.measure(qubit as usize);
    });
}

/// QIR API that reads the Boolean value corresponding to the given result identifier, where true
/// indicates a |1⟩ state and false indicates a |0⟩ state.
#[no_mangle]
pub extern "C" fn __quantum__qis__read_result__body(result: *mut c_void) -> bool {
    SIM_STATE.with(|sim_state| {
        let res = &mut sim_state.borrow_mut().res;
        let res_id = result as usize;
        if res.len() < res_id + 1 {
            res.resize(res_id + 1, false);
        }

        let b = *res
            .get(res_id)
            .expect("Result with given id missing after expansion.");
        b
    })
}

/// QIR API that measures a given qubit in the computational basis, returning a runtime managed result value.
/// # Panics
///
#[no_mangle]
pub extern "C" fn __quantum__qis__m__body(qubit: *mut c_void) -> *mut c_void {
    SIM_STATE.with(|sim_state| {
        let state = &mut *sim_state.borrow_mut();
        ensure_sufficient_qubits(&mut state.sim, qubit as usize, &mut state.max_qubit_id);

        if state.sim.measure(qubit as usize) {
            __quantum__rt__result_get_one()
        } else {
            __quantum__rt__result_get_zero()
        }
    })
}

/// QIR API that performs joint measurement of the given qubits in the corresponding Pauli bases, returning the parity as a runtime managed result value.
/// # Safety
///
/// This function should only be called with arrays created by the QIR runtime library.
/// # Panics
///
/// This function will panic if the provided paulis and qubits arrays are not of the same size.
#[allow(clippy::cast_ptr_alignment)]
#[no_mangle]
pub unsafe extern "C" fn __quantum__qis__measure__body(
    paulis: *const QirArray,
    qubits: *const QirArray,
) -> *mut c_void {
    SIM_STATE.with(|sim_state| {
        let mut state = sim_state.borrow_mut();

        let combined_list = map_paulis(&mut state, paulis, qubits);

        let res = state.sim.joint_measure(
            &combined_list
                .iter()
                .map(|(_, q)| *q)
                .collect::<Vec<usize>>(),
        );

        unmap_paulis(&mut state, combined_list);

        if res {
            __quantum__rt__result_get_one()
        } else {
            __quantum__rt__result_get_zero()
        }
    })
}

/// QIR API for checking internal simulator state and verifying the probability of the given parity measurement result
/// for the given qubits in the given Pauli bases is equal to the expected probability, within the given tolerance.
/// # Safety
///
/// This function should only be called with arrays created by the QIR runtime library.
#[no_mangle]
pub unsafe extern "C" fn __quantum__qis__assertmeasurementprobability__body(
    paulis: *const QirArray,
    qubits: *const QirArray,
    result: *mut c_void,
    prob: c_double,
    msg: *const CString,
    tol: c_double,
) {
    SIM_STATE.with(|sim_state| {
        let mut state = sim_state.borrow_mut();

        let combined_list = map_paulis(&mut state, paulis, qubits);

        let mut actual_prob = state.sim.joint_probability(
            &combined_list
                .iter()
                .map(|(_, q)| *q)
                .collect::<Vec<usize>>(),
        );

        if __quantum__rt__result_equal(result, __quantum__rt__result_get_zero()) {
            actual_prob = 1.0 - actual_prob;
        }

        if (actual_prob - (prob as f64)).abs() > tol as f64 {
            __quantum__rt__fail(msg);
        }

        unmap_paulis(&mut state, combined_list);
    });
}

#[derive(Copy, Clone)]
#[repr(C)]
struct AssertMeasurementProbabilityArgs {
    paulis: *const QirArray,
    qubits: *const QirArray,
    result: *mut c_void,
    prob: c_double,
    msg: *const CString,
    tol: c_double,
}

/// QIR API for checking internal simulator state and verifying the probability of the given parity measurement result
/// for the given qubits in the given Pauli bases is equal to the expected probability, within the given tolerance.
/// Note that control qubits are ignored.
/// # Safety
///
/// This function should only be called with arrays created by the QIR runtime library.
#[no_mangle]
pub unsafe extern "C" fn __quantum__qis__assertmeasurementprobability__ctl(
    _ctls: *const QirArray,
    arg_tuple: *mut *const Vec<u8>,
) {
    let args = *arg_tuple.cast::<AssertMeasurementProbabilityArgs>();
    __quantum__qis__assertmeasurementprobability__body(
        args.paulis,
        args.qubits,
        args.result,
        args.prob,
        args.msg,
        args.tol,
    );
}

/// QIR API for recording the given result into the program output.
#[no_mangle]
pub extern "C" fn __quantum__rt__result_record_output(result: *mut c_void, tag: *mut c_char) {
    SIM_STATE.with(|sim_state| {
        let res = &mut sim_state.borrow_mut().res;
        let res_id = result as usize;
        let b = if res.is_empty() {
            // No static measurements have been used, so default to dynamic handling.
            __quantum__rt__result_equal(result, __quantum__rt__result_get_one())
        } else {
            if res.len() < res_id + 1 {
                res.resize(res_id + 1, false);
            }
            *res.get(res_id)
                .expect("Result with given id missing after expansion.")
        };

        print!("OUTPUT\tRESULT\t{}", if b { "1" } else { "0" });
        if tag.is_null() {
            println!();
        } else {
            unsafe {
                let tag_string = strings::to_string(tag);
                println!("\t{}", tag_string);
            }
        }
    });
}

/// QIR API that allocates the next available qubit in the simulation.
#[no_mangle]
pub extern "C" fn __quantum__rt__qubit_allocate() -> *mut c_void {
    SIM_STATE.with(|sim_state| {
        let mut state = sim_state.borrow_mut();
        let qubit_id = state.sim.allocate();

        // Increase the max qubit id global so that `ensure_sufficient_qubits` wont trigger more allocations.
        // NOTE: static allocation and dynamic allocation shouldn't be used together, so this is safe to do.
        state.max_qubit_id = state.max_qubit_id.max(qubit_id + 1);

        qubit_id as *mut c_void
    })
}

/// QIR API for allocating the given number of qubits in the simulation, returning them as a runtime managed array.
/// # Panics
///
/// This function will panic if the underlying platform has a pointer size that cannot be described in
/// a `u32`.
#[allow(clippy::cast_ptr_alignment)]
#[no_mangle]
pub extern "C" fn __quantum__rt__qubit_allocate_array(size: u64) -> *const QirArray {
    let arr = __quantum__rt__array_create_1d(size_of::<usize>().try_into().unwrap(), size);
    for index in 0..size {
        unsafe {
            let elem = __quantum__rt__array_get_element_ptr_1d(arr, index).cast::<*mut c_void>();
            *elem = __quantum__rt__qubit_allocate();
        }
    }
    arr
}

/// QIR API for releasing the given runtime managed qubit array.
/// # Safety
///
/// This function should only be called with arrays created by `__quantum__rt__qubit_allocate_array`.
#[allow(clippy::cast_ptr_alignment)]
#[no_mangle]
pub unsafe extern "C" fn __quantum__rt__qubit_release_array(arr: *const QirArray) {
    for index in 0..__quantum__rt__array_get_size_1d(arr) {
        let elem = __quantum__rt__array_get_element_ptr_1d(arr, index).cast::<*mut c_void>();
        __quantum__rt__qubit_release(*elem);
    }
    __quantum__rt__array_update_alias_count(arr, -1);
}

/// QIR API for releasing the given qubit from the simulation.
#[no_mangle]
pub extern "C" fn __quantum__rt__qubit_release(qubit: *mut c_void) {
    SIM_STATE.with(|sim_state| {
        let mut state = sim_state.borrow_mut();
        state.sim.release(qubit as usize);
    });
}

/// QIR API for getting the string interpretation of a qubit identifier.
/// # Panics
///
/// This function will panic if it is unable to allocate the memory for the string.
#[no_mangle]
pub extern "C" fn __quantum__rt__qubit_to_string(qubit: *mut c_void) -> *const CString {
    unsafe {
        __quantum__rt__string_create(
            CString::new(format!("{}", qubit as usize))
                .unwrap()
                .as_bytes_with_nul()
                .as_ptr() as *mut i8,
        )
    }
}

/// API for viewing the current global result and quantum state for the simulator.
#[no_mangle]
pub extern "C" fn dump_state() {
    SIM_STATE.with(|sim_state| {
        let mut state = sim_state.borrow_mut();

        if !state.res.is_empty() {
            println!("Global Results: {}", state.res);
        }
        state.sim.dump();
    });
}

/// QIR API for dumping full internal simulator state.
#[no_mangle]
pub extern "C" fn __quantum__qis__dumpmachine__body(location: *mut c_void) {
    if !location.is_null() {
        unimplemented!("Dump to location is not implemented.")
    }
    dump_state();
}

#[cfg(test)]
mod tests {
    use std::ffi::c_void;

    use super::{
        __quantum__qis__cnot__body, __quantum__qis__h__body, __quantum__qis__m__body,
        __quantum__qis__mz__body, __quantum__qis__read_result__body, __quantum__qis__x__body,
        __quantum__rt__qubit_allocate, __quantum__rt__qubit_allocate_array,
        __quantum__rt__qubit_release, __quantum__rt__qubit_release_array,
        __quantum__rt__result_equal, __quantum__rt__result_get_one, dump_state,
    };
    use qir_stdlib::arrays::__quantum__rt__array_get_element_ptr_1d;

    #[test]
    fn basic_test_static() {
        let q0 = 5 as *mut c_void;
        let r0 = std::ptr::null_mut();
        let r1 = 1 as *mut c_void;
        __quantum__qis__mz__body(q0, r0);
        assert!(!__quantum__qis__read_result__body(r0));
        __quantum__qis__x__body(q0);
        __quantum__qis__mz__body(q0, r1);
        assert!(__quantum__qis__read_result__body(r1));
        __quantum__qis__x__body(q0);
        __quantum__qis__mz__body(q0, r0);
        assert!(!__quantum__qis__read_result__body(r0));
        assert!(!__quantum__qis__read_result__body(3 as *mut c_void));
        dump_state();
    }

    #[allow(clippy::cast_ptr_alignment)]
    #[test]
    fn basic_test_dynamic() {
        let q1 = __quantum__rt__qubit_allocate();
        let q2 = __quantum__rt__qubit_allocate();
        __quantum__qis__h__body(q1);
        __quantum__qis__cnot__body(q1, q2);
        let r1 = __quantum__qis__m__body(q1);
        let r2 = __quantum__qis__m__body(q2);
        assert!(__quantum__rt__result_equal(r1, r2));
        dump_state();
        __quantum__rt__qubit_release(q2);
        __quantum__rt__qubit_release(q1);
        let qs = __quantum__rt__qubit_allocate_array(4);
        unsafe {
            let q_elem = __quantum__rt__array_get_element_ptr_1d(qs, 3).cast::<*mut c_void>();
            __quantum__qis__x__body(*q_elem);
            dump_state();
            let r = __quantum__qis__m__body(*q_elem);
            assert!(__quantum__rt__result_equal(
                r,
                __quantum__rt__result_get_one()
            ));
            __quantum__rt__qubit_release_array(qs);
        }
    }
}
