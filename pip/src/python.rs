// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::ffi::OsString;

use pyo3::exceptions::PyRuntimeError;
use pyo3::prelude::*;
use pyo3::types::PyTuple;
use runner::OUTPUT;

struct OptionalCallbackReceiver<'a> {
    callback: Option<Py<PyAny>>,
    py: Python<'a>,
}

#[pyclass(unsendable)]
pub(crate) struct Output(String);

#[pymethods]
/// An output returned from the QIR execution.
/// These are normally printed to the console.
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
                    )?,
                )
                .map_err(std::io::Error::other)?;
            Ok(msg.len())
        } else {
            std::io::stdout().write(buf)
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        std::io::stdout().flush()
    }
}

/// Runs the supplied QIR (bitcode or textual IR) file.
/// :param path: Path to the QIR file to run. If not provided or '-', standard input will be read.
/// :param entry_point: Name of the entry point function to execute.
///     Default is `None`.
/// :param shots: The number of times to repeat the execution of
///     the chosen entry point in the program. Default is 1.
/// :param rngseed: The value to use when seeding the random number generator
///     used for quantum simulation.
/// :param output_fn: A callback function to receive the output of the QIR
///     runtime functions. Default is `None`. When no callback is provided,
///     the output is printed to the console.
#[pyfunction]
#[pyo3(signature = (path=None, entry_point=None, shots=None, rng_seed=None, output_fn=None))]
#[allow(clippy::needless_pass_by_value)]
pub(crate) fn run(
    py: Python,
    path: Option<String>,
    entry_point: Option<String>,
    shots: Option<u32>,
    rng_seed: Option<u64>,
    output_fn: Option<Py<PyAny>>,
) -> PyResult<()> {
    OUTPUT.with(|output| {
        let mut output = output.borrow_mut();
        output.use_std_out(output_fn.is_none());
    });
    let mut receiver = OptionalCallbackReceiver {
        callback: output_fn,
        py,
    };
    let file = path.and_then(|p| if p == "-" { None } else { Some(p) });
    if let Some(path) = file {
        runner::run_file(
            path,
            entry_point.as_deref(),
            shots.unwrap_or(1),
            rng_seed,
            &mut receiver,
        )
    } else {
        let bytes = read_stdin_bytes(py)?;
        runner::run_bytes(
            &bytes,
            entry_point.as_deref(),
            shots.unwrap_or(1),
            rng_seed,
            &mut receiver,
        )
    }
    .map_err(PyErr::new::<PyRuntimeError, _>)?;
    Ok(())
}

/// Reads the entirety of Python's `sys.stdin` as a byte array.
fn read_stdin_bytes(py: Python) -> PyResult<Vec<u8>> {
    // Read from Python's sys.stdin so that the standard input stream
    // respected by Python (including any redirection or replacement of
    // sys.stdin) is used.
    let stdin = py.import("sys")?.getattr("stdin")?;
    let buffer = stdin.call_method0("read")?;
    // `read()` may return either `bytes` or `str`; normalize to bytes.
    if let Ok(bytes) = buffer.extract::<Vec<u8>>() {
        Ok(bytes)
    } else if let Ok(text) = buffer.extract::<String>() {
        Ok(text.into_bytes())
    } else {
        Err(PyErr::new::<PyRuntimeError, _>(
            "Failed to read bytes from sys.stdin",
        ))
    }
}

#[pyfunction]
#[pyo3(signature = (bytes, entry_point=None, shots=None, rng_seed=None, output_fn=None))]
#[allow(clippy::needless_pass_by_value)]
pub(crate) fn run_bytes(
    py: Python,
    bytes: Vec<u8>,
    entry_point: Option<String>,
    shots: Option<u32>,
    rng_seed: Option<u64>,
    output_fn: Option<Py<PyAny>>,
) -> PyResult<()> {
    OUTPUT.with(|output| {
        let mut output = output.borrow_mut();
        output.use_std_out(output_fn.is_none());
    });
    let mut receiver = OptionalCallbackReceiver {
        callback: output_fn,
        py,
    };
    runner::run_bytes(
        &bytes,
        entry_point.as_deref(),
        shots.unwrap_or(1),
        rng_seed,
        &mut receiver,
    )
    .map_err(PyErr::new::<PyRuntimeError, _>)?;
    Ok(())
}

#[pyfunction]
#[pyo3(signature = (args = None))]
pub(crate) fn main(args: Option<Vec<String>>) -> PyResult<()> {
    match args {
        Some(args) => {
            // when invoked from python, but not from the command line
            let args: Vec<OsString> = args.into_iter().map(|s| s.into()).collect::<Vec<_>>();
            runner::main(Some(args)).map_err(PyErr::new::<PyRuntimeError, _>)?;
        }
        None => {
            // We are being invoked from the command line
            // skip the first arg, which is the name of the python executable
            let args = std::env::args_os().skip(1);
            runner::main(Some(args)).map_err(PyErr::new::<PyRuntimeError, _>)?;
        }
    }

    Ok(())
}

#[pymodule]
fn _native<'py>(_py: Python<'py>, m: &Bound<'py, PyModule>) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(run, m)?)?;
    m.add_function(wrap_pyfunction!(run_bytes, m)?)?;
    m.add_function(wrap_pyfunction!(main, m)?)?;
    m.add_class::<Output>()?;

    Ok(())
}
