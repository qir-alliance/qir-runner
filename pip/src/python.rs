// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::ffi::OsString;

use pyo3::prelude::*;
use pyo3::types::PyTuple;
use pyo3::wrap_pyfunction;

struct OptionalCallbackReceiver<'a> {
    callback: Option<PyObject>,
    py: Python<'a>,
}

#[pyclass(unsendable)]
pub(crate) struct Output(String);

#[pymethods]
/// An output returned from the Q# interpreter.
/// Outputs can be a state dumps or messages. These are normally printed to the console.
impl Output {
    fn __repr__(&self) -> String {
        self.0.clone()
    }

    fn __str__(&self) -> String {
        self.__repr__()
    }

    fn _repr_html_(&self) -> String {
        format!("<p>{}</p>", self.0)
    }
}

impl std::io::Write for OptionalCallbackReceiver<'_> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        if let Some(callback) = &self.callback {
            let msg = std::str::from_utf8(buf).expect("should be able to convert to utf8");
            let out = msg.to_owned();
            callback
                .call1(
                    self.py,
                    PyTuple::new(
                        self.py,
                        &[Py::new(self.py, Output(out)).expect("should be able to create output")],
                    ),
                )
                .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))?;
            Ok(msg.len())
        } else {
            std::io::stdout().write(buf)
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        std::io::stdout().flush()
    }
}

#[pyfunction]
#[pyo3(text_signature = "(path, entry_point, shots, rng_seed)")]
#[allow(clippy::needless_pass_by_value)]
pub(crate) fn run_file(
    py: Python,
    path: String,
    entry_point: Option<String>,
    shots: Option<u32>,
    rng_seed: Option<u64>,
    callback: Option<PyObject>,
) -> PyResult<()> {
    let mut receiver = OptionalCallbackReceiver { callback, py };
    runner::run_file(
        path,
        entry_point.as_deref(),
        shots.unwrap_or(1),
        rng_seed,
        &mut receiver,
    )
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
    m.add_class::<Output>()?;

    Ok(())
}
