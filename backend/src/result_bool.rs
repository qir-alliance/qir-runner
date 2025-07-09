// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.
#![deny(clippy::all, clippy::pedantic)]

use qir_stdlib::strings::__quantum__rt__string_create;
use std::ffi::{CString, c_char, c_void};

#[unsafe(no_mangle)]
pub extern "C" fn __quantum__rt__result_get_zero() -> *mut c_void {
    std::ptr::null_mut()
}

#[unsafe(no_mangle)]
#[allow(clippy::manual_dangling_ptr)]
pub extern "C" fn __quantum__rt__result_get_one() -> *mut c_void {
    1 as *mut c_void
}

#[unsafe(no_mangle)]
pub extern "C" fn __quantum__rt__result_equal(r1: *mut c_void, r2: *mut c_void) -> bool {
    r1 == r2
}

#[unsafe(no_mangle)]
pub extern "C" fn __quantum__rt__result_update_reference_count(_res: *mut c_void, _update: i32) {
    // no-op
}

/// # Panics
/// This function panics if the memory cannot be allocated for the result string.
#[unsafe(no_mangle)]
pub extern "C" fn __quantum__rt__result_to_string(res: *mut c_void) -> *const CString {
    unsafe {
        __quantum__rt__string_create(
            CString::new(
                if __quantum__rt__result_equal(res, __quantum__rt__result_get_one()) {
                    "One"
                } else {
                    "Zero"
                },
            )
            .expect("Failed to allocate memory for result string.")
            .as_bytes_with_nul()
            .as_ptr() as *mut c_char,
        )
    }
}
