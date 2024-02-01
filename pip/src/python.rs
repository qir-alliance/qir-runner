// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::ffi::OsString;

use pyo3::prelude::*;
use pyo3::wrap_pyfunction;

#[pyfunction]
#[pyo3(text_signature = "(path, entry_point, shots, rng_seed)")]
#[allow(clippy::needless_pass_by_value)]
pub(crate) fn run_file(
    _py: Python,
    path: String,
    entry_point: Option<String>,
    shots: Option<u32>,
    rng_seed: Option<u64>,
) -> PyResult<()> {
    runner::run_file(path, entry_point.as_deref(), shots.unwrap_or(1), rng_seed)
        .map_err(|msg| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(msg))?;
    Ok(())
}

#[pyfunction]
#[pyo3(text_signature = "(args)")]
pub(crate) fn main(args: Option<Vec<String>>) -> PyResult<()> {
    match args {
        Some(args) => {
            let args: Vec<OsString> = args.into_iter().map(|s| s.into()).collect::<Vec<_>>();
            runner::main(Some(args))
                .map_err(|msg| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(msg))?;
        }
        None => {
            // skip the first arg, which is the name of the python executable
            let args = std::env::args_os().skip(1);
            runner::main(Some(args))
                .map_err(|msg| PyErr::new::<pyo3::exceptions::PyRuntimeError, _>(msg))?;
        }
    }

    Ok(())
}

#[pymodule]
fn _native(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(run_file, m)?)?;
    m.add_function(wrap_pyfunction!(main, m)?)?;

    Ok(())
}
