// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// This file contains the native support for the multi-qubit Exp rotation gate.
// See https://learn.microsoft.com/en-us/qsharp/api/qsharp/microsoft.quantum.intrinsic.exp for details on the gate.
// This is intentionally kept separate from the main simulator implementation as it is likely to be removed
// in favor of having high level languages decompose into CNOT and single qubit rotations (see
// https://github.com/microsoft/qsharp-runtime/issues/999 and https://github.com/microsoft/QuantumLibraries/issues/579).

use num_bigint::BigUint;
use num_complex::Complex64;
use num_traits::{One, Zero};
use qir_stdlib::{
    arrays::{QirArray, __quantum__rt__array_get_element_ptr_1d, __quantum__rt__array_get_size_1d},
    tuples::{__quantum__rt__tuple_create, __quantum__rt__tuple_update_reference_count},
    Pauli,
};
use std::{
    mem::size_of,
    ops::ControlFlow,
    os::raw::{c_double, c_void},
};

use crate::{
    ensure_sufficient_qubits,
    nearly_zero::NearlyZero,
    simulator::{FlushLevel, QuantumSim, SparseState},
    SIM_STATE,
};

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
        let paulis: Vec<Pauli> = (0..paulis_size)
            .into_iter()
            .map(|index| *__quantum__rt__array_get_element_ptr_1d(paulis, index).cast::<Pauli>())
            .collect();

        let qubits_size = __quantum__rt__array_get_size_1d(qubits);
        let targets: Vec<usize> = (0..qubits_size)
            .into_iter()
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
        let paulis: Vec<Pauli> = (0..paulis_size)
            .into_iter()
            .map(|index| {
                *__quantum__rt__array_get_element_ptr_1d(args.paulis, index).cast::<Pauli>()
            })
            .collect();

        let qubits_size = __quantum__rt__array_get_size_1d(args.qubits);
        let targets: Vec<usize> = (0..qubits_size)
            .into_iter()
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

impl QuantumSim {
    pub(crate) fn exp(&mut self, paulis: &[Pauli], theta: f64, targets: &[usize]) {
        self.mcexp(&[], paulis, theta, targets);
    }

    #[allow(clippy::too_many_lines)]
    pub(crate) fn mcexp(
        &mut self,
        ctls: &[usize],
        paulis: &[Pauli],
        theta: f64,
        targets: &[usize],
    ) {
        self.flush_queue(ctls, FlushLevel::HRxRy);
        self.flush_queue(targets, FlushLevel::HRxRy);

        let ctls: Vec<u64> = ctls
            .iter()
            .map(|c| {
                *self
                    .id_map
                    .get(c)
                    .unwrap_or_else(|| panic!("Unable to find qubit with id {}", c))
                    as u64
            })
            .collect();

        let targets: Vec<u64> = targets
            .iter()
            .map(|c| {
                *self
                    .id_map
                    .get(c)
                    .unwrap_or_else(|| panic!("Unable to find qubit with id {}", c))
                    as u64
            })
            .collect();

        let mut sorted_qubits = ctls.clone();
        sorted_qubits.append(&mut targets.clone());
        sorted_qubits.sort_unstable();
        if let ControlFlow::Break(Some(duplicate)) =
            sorted_qubits.iter().try_fold(None, |last, current| {
                last.map_or_else(
                    || ControlFlow::Continue(Some(current)),
                    |last| {
                        if last == current {
                            ControlFlow::Break(Some(current))
                        } else {
                            ControlFlow::Continue(Some(current))
                        }
                    },
                )
            })
        {
            panic!("Duplicate qubit id '{}' found in application.", duplicate);
        }

        let id_coeff = Complex64::new(theta.cos(), 0.0);
        let pauli_coeff = Complex64::new(0.0, theta.sin());

        let mut xy_mask = BigUint::zero();
        let mut yz_mask = BigUint::zero();
        let mut y_count = 0_u64;
        for i in 0..paulis.len() {
            match paulis[i] {
                Pauli::I => (),
                Pauli::X => xy_mask.set_bit(targets[i], true),
                Pauli::Y => {
                    yz_mask.set_bit(targets[i], true);
                    xy_mask.set_bit(targets[i], true);
                    y_count += 1;
                }
                Pauli::Z => yz_mask.set_bit(targets[i], true),
            }
        }

        self.state = if xy_mask.is_zero() {
            // The operation is purely Pauli-Z, so we can rotate in the computational basis.
            let pauli_coeff = pauli_coeff + id_coeff;
            let id_coeff = 2.0 * id_coeff - pauli_coeff;
            if pauli_coeff.is_nearly_zero() {
                // pauli_coeff is zero, so use only the states multiplied by id_coeff.
                self.state.drain().into_iter().fold(
                    SparseState::default(),
                    |mut accum, (index, value)| {
                        if ctls.iter().all(|c| index.bit(*c as u64))
                            && (&index & &yz_mask).count_ones() & 1 != 0
                        {
                            accum.insert(index, value * id_coeff);
                        }
                        accum
                    },
                )
            } else if id_coeff.is_nearly_zero() {
                // id_coeff is zero, so use only the states multiplied by pauli_coeff.
                self.state.drain().into_iter().fold(
                    SparseState::default(),
                    |mut accum, (index, value)| {
                        if ctls.iter().all(|c| index.bit(*c as u64))
                            && (&index & &yz_mask).count_ones() & 1 != 0
                        {
                            accum.insert(index, value * pauli_coeff);
                        }
                        accum
                    },
                )
            } else {
                // Both coefficients are non-zero, so modify each of the state records.
                self.state.drain().into_iter().fold(
                    SparseState::default(),
                    |mut accum, (index, val)| {
                        if ctls.iter().all(|c| index.bit(*c as u64)) {
                            accum.insert(
                                index.clone(),
                                val * if (index & &yz_mask).count_ones() & 1 == 0 {
                                    pauli_coeff
                                } else {
                                    id_coeff
                                },
                            );
                        } else {
                            accum.insert(index, val);
                        }
                        accum
                    },
                )
            }
        } else {
            // The operation includes some non-Pauli-Z rotations.
            let pauli_coeff = pauli_coeff
                * match y_count % 4 {
                    1 => Complex64::i(),
                    2 => -Complex64::one(),
                    3 => -Complex64::i(),
                    _ => Complex64::one(),
                };
            let pauli_coeff_alt = if y_count % 2 == 0 {
                pauli_coeff
            } else {
                -pauli_coeff
            };

            let mut new_state = SparseState::default();
            for (index, value) in &self.state {
                if ctls.iter().all(|c| index.bit(*c as u64)) {
                    let alt_index = index ^ &xy_mask;
                    if !self.state.contains_key(&alt_index) {
                        new_state.insert(index.clone(), value * id_coeff);
                        new_state.insert(
                            alt_index,
                            value
                                * if (index & &yz_mask).count_ones() & 1 == 0 {
                                    pauli_coeff
                                } else {
                                    -pauli_coeff
                                },
                        );
                    } else if index < &alt_index {
                        let parity = (index & &yz_mask).count_ones() & 1 != 0;
                        let alt_value = self.state[&alt_index] as Complex64;

                        let new_value = value * id_coeff
                            + alt_value
                                * if parity {
                                    -pauli_coeff_alt
                                } else {
                                    pauli_coeff_alt
                                };
                        if !new_value.is_nearly_zero() {
                            new_state.insert(index.clone(), new_value);
                        }

                        let new_value = alt_value * id_coeff
                            + value * if parity { -pauli_coeff } else { pauli_coeff };
                        if !new_value.is_nearly_zero() {
                            new_state.insert(alt_index, new_value);
                        }
                    }
                } else {
                    new_state.insert(index.clone(), *value);
                }
            }

            new_state
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::f64::consts::PI;

    #[test]
    fn test_exp_from_cnot() {
        let sim = &mut QuantumSim::default();
        let (control, target, paired) = (sim.allocate(), sim.allocate(), sim.allocate());

        // Entangle the check qubit `paired` with both `control` and `target`
        sim.h(paired);
        sim.mcx(&[paired], control);
        sim.mcx(&[paired], target);

        // Perform the decomposition of CNOT in terms of Rx, Rz, and Exp.
        // This decomposition is sensitive to angle convention in rotations, including
        // multipliers and sign.
        let theta = PI / -4.0;
        sim.rx(2.0 * theta, target);
        sim.rz(2.0 * theta, control);
        sim.exp(&[Pauli::Z, Pauli::X], theta, &[control, target]);

        // Perform the adjoint of CNOT, which is just CNOT again.
        sim.mcx(&[control], target);

        // Undo the entanglement.
        sim.mcx(&[paired], target);
        sim.mcx(&[paired], control);
        sim.h(paired);

        // If the rotations were performed correctly, the check qubit `paired` should
        // always be back in the ground state, and the whole state vector should be
        // back to a single zero state.
        assert!(!sim.joint_measure(&[paired]));
        assert_eq!(sim.state.len(), 1);
    }
}
