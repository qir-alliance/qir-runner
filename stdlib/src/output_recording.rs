// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::{
    ffi::CString,
    fmt::Display,
    os::raw::{c_char, c_double},
};

use crate::strings::{self, double_to_string};

fn output(ty: &str, val: &dyn Display, tag: *mut c_char) {
    print!("OUTPUT\t{}\t{}", ty, val);
    if tag.is_null() {
        println!();
    } else {
        unsafe {
            let tag_string = strings::to_string(tag);
            println!("\t{}", tag_string);
        }
    }
}

/// Inserts a marker in the generated output that indicates the
/// start of an array and how many array elements it has. The second
/// parameter defines a string label for the array. Depending on
/// the output schema, the label is included in the output or omitted.
#[no_mangle]
pub extern "C" fn __quantum__rt__array_record_output(val: i64, tag: *mut c_char) {
    output("ARRAY", &val, tag);
}

/// Inserts a marker in the generated output that indicates the
/// start of a tuple and how many tuple elements it has. The second
/// parameter defines a string label for the tuple. Depending on
/// the output schema, the label is included in the output or omitted.
#[no_mangle]
pub extern "C" fn __quantum__rt__tuple_record_output(val: i64, tag: *mut c_char) {
    output("TUPLE", &val, tag);
}

#[no_mangle]
pub extern "C" fn __quantum__rt__int_record_output(val: i64, tag: *mut c_char) {
    output("INT", &val, tag);
}

#[no_mangle]
pub extern "C" fn __quantum__rt__double_record_output(val: c_double, tag: *mut c_char) {
    output("DOUBLE", &double_to_string(val), tag);
}

#[no_mangle]
pub extern "C" fn __quantum__rt__bool_record_output(val: bool, tag: *mut c_char) {
    output("BOOL", &val, tag);
}

#[no_mangle]
pub unsafe extern "C" fn __quantum__rt__message_record_output(str: *const CString) {
    println!(
        "INFO\t{}",
        (*str)
            .to_str()
            .expect("Unable to convert input string")
            .escape_default()
    );
}
