// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use core::f64::consts::FRAC_1_SQRT_2;
use ndarray::{array, s, Array2};
use num_complex::Complex64;
use num_traits::One;
use num_traits::ToPrimitive;
use num_traits::Zero;

use crate::{nearly_zero::NearlyZero, FlushLevel, QuantumSim, SparseState};

impl QuantumSim {
    /// Applies the given unitary to the given targets, extending the unitary to accomodate controls if any.
    /// # Panics
    ///
    /// This function will panic if given ids in either targets or optional controls that do not correspond to allocated
    /// qubits, or if there is a duplicate id in targets or controls.
    /// This funciton will panic if the given unitary matrix does not match the number of targets provided.
    /// This function will panic if the given unitary is not square.
    /// This function will panic if the total number of targets and controls too large for a `u32`.
    pub(crate) fn apply(
        &mut self,
        unitary: &Array2<Complex64>,
        targets: &[usize],
        controls: Option<&[usize]>,
    ) {
        let mut targets = targets.to_vec();
        let mut unitary = unitary.clone();

        assert!(
            unitary.ncols() == unitary.nrows(),
            "Application given non-square matrix."
        );

        assert!(
            targets.len() == unitary.ncols() / 2,
            "Application given incorrect number of targets; expected {}, given {}.",
            unitary.ncols() / 2,
            targets.len()
        );

        if let Some(ctrls) = controls {
            // Add controls in order as targets.
            ctrls
                .iter()
                .enumerate()
                .for_each(|(index, &element)| targets.insert(index, element));

            // Extend the provided unitary by inserting it into an identity matrix.
            unitary = controlled(&unitary, ctrls.len().try_into().unwrap());
        }
        Self::check_for_duplicates(&targets);

        self.flush_queue(&targets, FlushLevel::HRxRy);

        targets
            .iter()
            .rev()
            .enumerate()
            .for_each(|(target_loc, target)| {
                let loc = *self
                    .id_map
                    .get(target)
                    .unwrap_or_else(|| panic!("Unable to find qubit with id {target}"));
                let swap_id = *self
                    .id_map
                    .iter()
                    .find(|(_, &value)| value == target_loc)
                    .unwrap()
                    .0;
                self.swap_qubit_state(loc, target_loc);
                *(self.id_map.get_mut(&swap_id).unwrap()) = loc;
                *(self.id_map.get_mut(target).unwrap()) = target_loc;
            });

        let op_size = unitary.nrows();
        self.state = self
            .state
            .drain()
            .fold(SparseState::default(), |mut accum, (index, val)| {
                let i = &index / op_size;
                let l = (&index % op_size)
                    .to_usize()
                    .expect("Cannot operate on more than 64 qubits at a time.");
                for j in (0..op_size).filter(|j| !unitary.row(*j)[l].is_nearly_zero()) {
                    let loc = (&i * op_size) + j;
                    if let Some(entry) = accum.get_mut(&loc) {
                        *entry += unitary.row(j)[l] * val;
                    } else {
                        accum.insert((&i * op_size) + j, unitary.row(j)[l] * val);
                    }
                    if accum
                        .get(&loc)
                        .map_or_else(|| false, |entry| (*entry).is_nearly_zero())
                    {
                        accum.remove(&loc);
                    }
                }
                accum
            });
        assert!(
            !self.state.is_empty(),
            "State vector should never be empty."
        );
    }
}

/// Returns a unitary matrix representing the `X` operation.
#[must_use]
pub fn x() -> Array2<Complex64> {
    array![
        [Complex64::zero(), Complex64::one()],
        [Complex64::one(), Complex64::zero()]
    ]
}

/// Returns a unitary matrix representing the `Y` operation.
#[must_use]
pub fn y() -> Array2<Complex64> {
    array![
        [Complex64::zero(), -Complex64::i()],
        [Complex64::i(), Complex64::zero()]
    ]
}

/// Returns a unitary matrix representing the `Z` operation.
#[must_use]
pub fn z() -> Array2<Complex64> {
    array![
        [Complex64::one(), Complex64::zero()],
        [Complex64::zero(), -Complex64::one()]
    ]
}

/// Returns a unitary matrix representing the single-qubit Hadamard transformation.
#[must_use]
pub fn h() -> Array2<Complex64> {
    array![
        [Complex64::one(), Complex64::one()],
        [Complex64::one(), -Complex64::one()]
    ] * FRAC_1_SQRT_2
}

/// Returns a unitary matrix representing the `T` operation.
#[must_use]
pub fn t() -> Array2<Complex64> {
    array![
        [Complex64::one(), Complex64::zero()],
        [
            Complex64::zero(),
            Complex64::new(FRAC_1_SQRT_2, FRAC_1_SQRT_2)
        ]
    ]
}

/// Returns a unitary matrix representing the `S` operation.
#[must_use]
pub fn s() -> Array2<Complex64> {
    array![
        [Complex64::one(), Complex64::zero()],
        [Complex64::zero(), Complex64::i()]
    ]
}

/// Returns a unitary matrix representing the `Rx` operation with the given angle.
#[must_use]
pub fn rx(theta: f64) -> Array2<Complex64> {
    let cos_theta = f64::cos(theta / 2.0);
    let sin_theta = f64::sin(theta / 2.0);
    array![
        [
            Complex64::new(cos_theta, 0.0),
            Complex64::new(0.0, -sin_theta)
        ],
        [
            Complex64::new(0.0, -sin_theta),
            Complex64::new(cos_theta, 0.0)
        ]
    ]
}

/// Returns a unitary matrix representing the `Ry` operation with the given angle.
#[must_use]
pub fn ry(theta: f64) -> Array2<Complex64> {
    let cos_theta = f64::cos(theta / 2.0);
    let sin_theta = f64::sin(theta / 2.0);
    array![
        [
            Complex64::new(cos_theta, 0.0),
            Complex64::new(-sin_theta, 0.0)
        ],
        [
            Complex64::new(sin_theta, 0.0),
            Complex64::new(cos_theta, 0.0)
        ]
    ]
}

/// Returns a unitary matrix representing the `Rz` operation with the given angle.
#[must_use]
pub fn rz(theta: f64) -> Array2<Complex64> {
    let exp_theta = Complex64::exp(Complex64::new(0.0, theta / 2.0));
    let neg_exp_theta = Complex64::exp(Complex64::new(0.0, -theta / 2.0));
    array![
        [neg_exp_theta, Complex64::zero()],
        [Complex64::zero(), exp_theta]
    ]
}

/// Returns a unitary matrix representing the `G` or `GlobalPhase` operation with the given angle.
#[must_use]
pub fn g(theta: f64) -> Array2<Complex64> {
    let neg_exp_theta = Complex64::exp(Complex64::new(0.0, -theta / 2.0));
    array![
        [Complex64::one(), Complex64::zero()],
        [Complex64::zero(), neg_exp_theta]
    ]
}

/// Returns a unitary matrix representing the `SWAP` operation.
#[must_use]
pub fn swap() -> Array2<Complex64> {
    array![
        [
            Complex64::one(),
            Complex64::zero(),
            Complex64::zero(),
            Complex64::zero()
        ],
        [
            Complex64::zero(),
            Complex64::zero(),
            Complex64::one(),
            Complex64::zero()
        ],
        [
            Complex64::zero(),
            Complex64::one(),
            Complex64::zero(),
            Complex64::zero()
        ],
        [
            Complex64::zero(),
            Complex64::zero(),
            Complex64::zero(),
            Complex64::one()
        ]
    ]
}

/// Transforms the given matrix into it's adjoint using the transpose of the complex conjugate.
#[must_use]
pub fn adjoint(u: &Array2<Complex64>) -> Array2<Complex64> {
    u.t().map(Complex64::conj)
}

/// Extends the given unitary matrix into a matrix corresponding to the same unitary with a given number of controls
/// by inserting it into an identity matrix.
#[must_use]
pub fn controlled(u: &Array2<Complex64>, num_ctrls: u32) -> Array2<Complex64> {
    let mut controlled_u = Array2::eye(u.nrows() * 2_usize.pow(num_ctrls));
    controlled_u
        .slice_mut(s![
            (controlled_u.nrows() - u.nrows())..,
            (controlled_u.ncols() - u.ncols())..
        ])
        .assign(u);
    controlled_u
}

mod tests {
    use super::*;
    use core::f64::consts::PI;

    fn is_self_adjoint(arr: &Array2<Complex64>) -> bool {
        arr == adjoint(arr)
    }

    fn are_equal_to_precision(actual: Array2<Complex64>, expected: Array2<Complex64>) -> bool {
        // If we use assert_eq here, we'll get bitten by finite precision.
        // We also can't use LAPACK, since that greatly complicates bindings,
        // so we do an ad hoc implementation here.
        (actual - expected).map(|x| x.norm()).sum() <= 1e-10
    }

    #[test]
    fn h_is_self_adjoint() {
        assert!(is_self_adjoint(&h()));
    }

    #[test]
    fn x_is_self_adjoint() {
        assert!(is_self_adjoint(&x()));
    }

    #[test]
    fn y_is_self_adjoint() {
        assert!(is_self_adjoint(&y()));
    }

    #[test]
    fn z_is_self_adjoint() {
        assert!(is_self_adjoint(&z()));
    }

    #[test]
    fn swap_is_self_adjoint() {
        assert!(is_self_adjoint(&swap()));
    }

    #[test]
    fn s_squares_to_z() {
        assert_eq!(s().dot(&s()), z());
    }

    #[test]
    fn t_squares_to_s() {
        assert!(are_equal_to_precision(t().dot(&t()), s()));
    }

    #[test]
    fn rx_pi_is_x() {
        assert!(are_equal_to_precision(Complex64::i() * rx(PI), x()));
    }

    #[test]
    fn ry_pi_is_y() {
        assert!(are_equal_to_precision(Complex64::i() * ry(PI), y()));
    }

    #[test]
    fn rz_pi_is_z() {
        assert!(are_equal_to_precision(Complex64::i() * rz(PI), z()));
    }

    #[test]
    fn gate_multiplication() {
        assert!(are_equal_to_precision(x().dot(&y()), Complex64::i() * z()));
    }

    #[test]
    fn controlled_extension() {
        fn cnot() -> Array2<Complex64> {
            array![
                [
                    Complex64::one(),
                    Complex64::zero(),
                    Complex64::zero(),
                    Complex64::zero()
                ],
                [
                    Complex64::zero(),
                    Complex64::one(),
                    Complex64::zero(),
                    Complex64::zero()
                ],
                [
                    Complex64::zero(),
                    Complex64::zero(),
                    Complex64::zero(),
                    Complex64::one()
                ],
                [
                    Complex64::zero(),
                    Complex64::zero(),
                    Complex64::one(),
                    Complex64::zero()
                ]
            ]
        }
        assert!(are_equal_to_precision(controlled(&x(), 1), cnot()));
        assert!(are_equal_to_precision(
            controlled(&x(), 2),
            controlled(&cnot(), 1)
        ));
        assert_eq!(controlled(&x(), 3).nrows(), 2_usize.pow(4));
    }

    /// Utility for testing operation equivalence.
    fn assert_operation_equal_referenced<F1, F2>(mut op: F1, mut reference: F2, count: usize)
    where
        F1: FnMut(&mut QuantumSim, &[usize]),
        F2: FnMut(&mut QuantumSim, &[usize]),
    {
        let mut sim = QuantumSim::default();

        // Allocate the controls we use to verify behavior.
        // Allocate the requested number of targets, entangling the control with them.
        let mut ctls = vec![];
        let mut qs = vec![];
        for _ in 0..count {
            let ctl = sim.allocate();
            let q = sim.allocate();
            sim.h(ctl);
            sim.mcx(&[ctl], q);
            qs.push(q);
            ctls.push(ctl);
        }

        op(&mut sim, &qs);
        reference(&mut sim, &qs);

        // Undo the entanglement.
        for (q, ctl) in qs.iter().zip(&ctls) {
            sim.mcx(&[*ctl], *q);
            sim.h(*ctl);
        }

        println!("{}", sim.dump());

        // We know the operations are equal if the qubits are left in the zero state.
        for (q, ctl) in qs.iter().zip(&ctls) {
            assert!(sim.joint_probability(&[*q]).is_nearly_zero());
            assert!(sim.joint_probability(&[*ctl]).is_nearly_zero());
        }

        // Sparse state vector should have one entry for |0⟩.
        assert_eq!(sim.state.len(), 1);
        // If the operations are equal including the phase, the entry should be 1.
        assert!((sim.state.values().next().unwrap() - Complex64::one()).is_nearly_zero());
    }

    #[test]
    fn test_h() {
        assert_operation_equal_referenced(
            |sim, qs| {
                sim.h(qs[0]);
            },
            |sim, qs| {
                sim.apply(&h(), &[qs[0]], None);
            },
            1,
        );
    }

    #[test]
    fn test_x() {
        assert_operation_equal_referenced(
            |sim, qs| {
                sim.x(qs[0]);
            },
            |sim, qs| {
                sim.apply(&x(), &[qs[0]], None);
            },
            1,
        );
    }

    #[test]
    fn test_y() {
        assert_operation_equal_referenced(
            |sim, qs| {
                sim.y(qs[0]);
            },
            |sim, qs| {
                sim.apply(&y(), &[qs[0]], None);
            },
            1,
        );
    }

    #[test]
    fn test_z() {
        assert_operation_equal_referenced(
            |sim, qs| {
                sim.z(qs[0]);
            },
            |sim, qs| {
                sim.apply(&z(), &[qs[0]], None);
            },
            1,
        );
    }

    #[test]
    fn test_s() {
        assert_operation_equal_referenced(
            |sim, qs| {
                sim.s(qs[0]);
            },
            |sim, qs| {
                sim.apply(&adjoint(&s()), &[qs[0]], None);
            },
            1,
        );
    }

    #[test]
    fn test_sadj() {
        assert_operation_equal_referenced(
            |sim, qs| {
                sim.sadj(qs[0]);
            },
            |sim, qs| {
                sim.apply(&s(), &[qs[0]], None);
            },
            1,
        );
    }

    #[test]
    fn test_cx() {
        assert_operation_equal_referenced(
            |sim, qs| {
                sim.mcx(&[qs[0]], qs[1]);
            },
            |sim, qs| {
                sim.apply(&x(), &[qs[1]], Some(&[qs[0]]));
            },
            2,
        );
    }

    #[test]
    fn test_cz() {
        assert_operation_equal_referenced(
            |sim, qs| {
                sim.mcz(&[qs[0]], qs[1]);
            },
            |sim, qs| {
                sim.apply(&z(), &[qs[1]], Some(&[qs[0]]));
            },
            2,
        );
    }

    #[test]
    fn test_swap() {
        assert_operation_equal_referenced(
            |sim, qs| {
                sim.swap_qubit_ids(qs[0], qs[1]);
            },
            |sim, qs| {
                sim.apply(&swap(), &[qs[0], qs[1]], None);
            },
            2,
        );
    }

    #[test]
    fn test_rz() {
        assert_operation_equal_referenced(
            |sim, qs| {
                sim.rz(PI / 7.0, qs[0]);
            },
            |sim, qs| {
                sim.apply(&adjoint(&rz(PI / 7.0)), &[qs[0]], None);
            },
            1,
        );
    }

    #[test]
    fn test_rz_pi() {
        assert_operation_equal_referenced(
            |sim, qs| {
                sim.rz(PI, qs[0]);
            },
            |sim, qs| {
                sim.apply(&adjoint(&rz(PI)), &[qs[0]], None);
            },
            1,
        );
    }

    #[test]
    fn test_rx() {
        assert_operation_equal_referenced(
            |sim, qs| {
                sim.rx(PI / 7.0, qs[0]);
            },
            |sim, qs| {
                sim.apply(&adjoint(&rx(PI / 7.0)), &[qs[0]], None);
            },
            1,
        );
    }

    #[test]
    fn test_rx_pi() {
        assert_operation_equal_referenced(
            |sim, qs| {
                sim.rx(PI, qs[0]);
            },
            |sim, qs| {
                sim.apply(&adjoint(&rx(PI)), &[qs[0]], None);
            },
            1,
        );
    }

    #[test]
    fn test_rx_2pi() {
        assert_operation_equal_referenced(
            |sim, qs| {
                sim.rx(2.0 * PI, qs[0]);
            },
            |sim, qs| {
                sim.apply(&adjoint(&rx(2.0 * PI)), &[qs[0]], None);
            },
            1,
        );
    }

    #[test]
    fn test_rx_zero() {
        assert_operation_equal_referenced(
            |sim, qs| {
                sim.rx(0.0, qs[0]);
            },
            |sim, qs| {
                sim.apply(&adjoint(&rx(0.0)), &[qs[0]], None);
            },
            1,
        );
    }

    #[test]
    fn test_rx_3pi() {
        assert_operation_equal_referenced(
            |sim, qs| {
                sim.rx(3.0 * PI, qs[0]);
            },
            |sim, qs| {
                sim.apply(&adjoint(&rx(3.0 * PI)), &[qs[0]], None);
            },
            1,
        );
    }

    #[test]
    fn test_rx_4pi() {
        assert_operation_equal_referenced(
            |sim, qs| {
                sim.rx(4.0 * PI, qs[0]);
            },
            |sim, qs| {
                sim.apply(&adjoint(&rx(4.0 * PI)), &[qs[0]], None);
            },
            1,
        );
    }

    #[test]
    fn test_ry() {
        assert_operation_equal_referenced(
            |sim, qs| {
                sim.ry(PI / 7.0, qs[0]);
            },
            |sim, qs| {
                sim.apply(&adjoint(&ry(PI / 7.0)), &[qs[0]], None);
            },
            1,
        );
    }

    #[test]
    fn test_ry_pi() {
        assert_operation_equal_referenced(
            |sim, qs| {
                sim.ry(PI, qs[0]);
            },
            |sim, qs| {
                sim.apply(&adjoint(&ry(PI)), &[qs[0]], None);
            },
            1,
        );
    }

    #[test]
    fn test_ry_2pi() {
        assert_operation_equal_referenced(
            |sim, qs| {
                sim.ry(2.0 * PI, qs[0]);
            },
            |sim, qs| {
                sim.apply(&adjoint(&ry(2.0 * PI)), &[qs[0]], None);
            },
            1,
        );
    }

    #[test]
    fn test_ry_zero() {
        assert_operation_equal_referenced(
            |sim, qs| {
                sim.ry(0.0, qs[0]);
            },
            |sim, qs| {
                sim.apply(&adjoint(&ry(0.0)), &[qs[0]], None);
            },
            1,
        );
    }

    #[test]
    fn test_ry_3pi() {
        assert_operation_equal_referenced(
            |sim, qs| {
                sim.ry(3.0 * PI, qs[0]);
            },
            |sim, qs| {
                sim.apply(&adjoint(&ry(3.0 * PI)), &[qs[0]], None);
            },
            1,
        );
    }

    #[test]
    fn test_ry_4pi() {
        assert_operation_equal_referenced(
            |sim, qs| {
                sim.ry(4.0 * PI, qs[0]);
            },
            |sim, qs| {
                sim.apply(&adjoint(&ry(4.0 * PI)), &[qs[0]], None);
            },
            1,
        );
    }

    #[test]
    fn test_mcri() {
        assert_operation_equal_referenced(
            |sim, qs| {
                sim.mcphase(
                    &qs[2..3],
                    Complex64::exp(Complex64::new(0.0, -(PI / 7.0) / 2.0)),
                    qs[1],
                );
            },
            |sim, qs| {
                sim.apply(&adjoint(&g(PI / 7.0)), &[qs[1]], Some(&qs[2..3]));
            },
            3,
        );
    }
}
