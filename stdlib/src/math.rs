// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// TODO: transition math functions to `__quantum__rt` once compiler support is ready (https://github.com/microsoft/qsharp-compiler/issues/1557).

use rand::Rng;
use std::ffi::c_double;

use crate::strings::convert;

#[cfg(not(feature = "fail-support"))]
#[allow(improper_ctypes)]
extern "C" {
    fn __quantum__rt__fail(str: *const std::ffi::CString);
}

#[cfg(feature = "fail-support")]
use crate::__quantum__rt__fail;

#[unsafe(no_mangle)]
pub extern "C" fn __quantum__qis__nan__body() -> c_double {
    c_double::NAN
}

#[unsafe(no_mangle)]
pub extern "C" fn __quantum__qis__isnan__body(val: c_double) -> bool {
    val.is_nan()
}

#[unsafe(no_mangle)]
pub extern "C" fn __quantum__qis__infinity__body() -> c_double {
    c_double::INFINITY
}

#[unsafe(no_mangle)]
pub extern "C" fn __quantum__qis__isinf__body(val: c_double) -> bool {
    val.is_infinite() && val.is_sign_positive()
}

#[unsafe(no_mangle)]
pub extern "C" fn __quantum__qis__isnegativeinfinity__body(val: c_double) -> bool {
    val.is_infinite() && val.is_sign_negative()
}

#[unsafe(no_mangle)]
pub extern "C" fn __quantum__qis__sin__body(val: c_double) -> c_double {
    val.sin()
}

#[unsafe(no_mangle)]
pub extern "C" fn __quantum__qis__cos__body(val: c_double) -> c_double {
    val.cos()
}
#[unsafe(no_mangle)]
pub extern "C" fn __quantum__qis__tan__body(val: c_double) -> c_double {
    val.tan()
}

#[unsafe(no_mangle)]
pub extern "C" fn __quantum__qis__arctan2__body(y: c_double, x: c_double) -> c_double {
    y.atan2(x)
}

#[unsafe(no_mangle)]
pub extern "C" fn __quantum__qis__sinh__body(val: c_double) -> c_double {
    val.sinh()
}

#[unsafe(no_mangle)]
pub extern "C" fn __quantum__qis__cosh__body(val: c_double) -> c_double {
    val.cosh()
}

#[unsafe(no_mangle)]
pub extern "C" fn __quantum__qis__tanh__body(val: c_double) -> c_double {
    val.tanh()
}

#[unsafe(no_mangle)]
pub extern "C" fn __quantum__qis__arcsin__body(val: c_double) -> c_double {
    val.asin()
}

#[unsafe(no_mangle)]
pub extern "C" fn __quantum__qis__arccos__body(val: c_double) -> c_double {
    val.acos()
}

#[unsafe(no_mangle)]
pub extern "C" fn __quantum__qis__arctan__body(val: c_double) -> c_double {
    val.atan()
}

#[unsafe(no_mangle)]
pub extern "C" fn __quantum__qis__sqrt__body(val: c_double) -> c_double {
    val.sqrt()
}

#[unsafe(no_mangle)]
pub extern "C" fn __quantum__qis__log__body(val: c_double) -> c_double {
    val.ln()
}

#[unsafe(no_mangle)]
pub extern "C" fn __quantum__qis__ieeeremainder__body(x: c_double, y: c_double) -> c_double {
    x - y * (x / y).round()
}

#[unsafe(no_mangle)]
pub extern "C" fn __quantum__qis__drawrandomint__body(min: i64, max: i64) -> i64 {
    if min > max {
        unsafe {
            __quantum__rt__fail(convert(&"Invalid Argument: minimum > maximum".to_string()));
        }
    }
    rand::thread_rng().gen_range(min..=max)
}

#[unsafe(no_mangle)]
pub extern "C" fn __quantum__qis__drawrandomdouble__body(min: c_double, max: c_double) -> f64 {
    if min > max {
        unsafe {
            __quantum__rt__fail(convert(&"Invalid Argument: minimum > maximum".to_string()));
        }
    }
    rand::thread_rng().gen_range(min..=max)
}
