// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::{
    ffi::CString,
    fmt::Display,
    io::{self, Write},
    os::raw::{c_char, c_double},
};

use crate::strings::double_to_string;

#[cfg(windows)]
const LINE_ENDING: &'static [u8] = b"\r\n";
#[cfg(not(windows))]
const LINE_ENDING: &'static [u8] = b"\n";

fn output(
    ty: &str,
    val: &dyn Display,
    tag: *mut c_char,
    output: &mut impl Write,
) -> std::io::Result<()> {
    output.write_fmt(format_args!("OUTPUT\t{ty}\t{val}"))?;
    if !tag.is_null() {
        output.write(b"\t")?;
        unsafe {
            extern "C" {
                /// Provided by libc or compiler_builtins.
                fn strlen(s: *const c_char) -> usize;
            }
            let len = strlen(tag);
            let ptr = tag as *const u8;
            let bytes = std::slice::from_raw_parts(ptr, len as usize + 1);
            output.write(bytes)?;
        }
    }
    output.write(LINE_ENDING)?;
    Ok(())
}

/// Inserts a marker in the generated output that indicates the
/// start of an array and how many array elements it has. The second
/// parameter defines a string label for the array. Depending on
/// the output schema, the label is included in the output or omitted.
#[no_mangle]
pub extern "C" fn __quantum__rt__array_record_output(val: i64, tag: *mut c_char) {
    output("ARRAY", &val, tag, &mut io::stdout()).expect("Failed to write array output");
}

/// Inserts a marker in the generated output that indicates the
/// start of a tuple and how many tuple elements it has. The second
/// parameter defines a string label for the tuple. Depending on
/// the output schema, the label is included in the output or omitted.
#[no_mangle]
pub extern "C" fn __quantum__rt__tuple_record_output(val: i64, tag: *mut c_char) {
    output("TUPLE", &val, tag, &mut io::stdout()).expect("Failed to write tuple output");
}

#[no_mangle]
pub extern "C" fn __quantum__rt__int_record_output(val: i64, tag: *mut c_char) {
    output("INT", &val, tag, &mut io::stdout()).expect("Failed to write int output");
}

#[no_mangle]
pub extern "C" fn __quantum__rt__double_record_output(val: c_double, tag: *mut c_char) {
    output("DOUBLE", &double_to_string(val), tag, &mut io::stdout())
        .expect("Failed to write double output");
}

#[no_mangle]
pub extern "C" fn __quantum__rt__bool_record_output(val: bool, tag: *mut c_char) {
    output("BOOL", &val, tag, &mut io::stdout()).expect("Failed to write bool output");
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

#[cfg(test)]
mod tests {
    use std::ptr::null_mut;

    use super::*;

    #[test]
    fn test_output_int_untagged() {
        let val: i64 = 42;
        assert_untagged_output_match("INT", &val, "OUTPUT\tINT\t42");
    }
    #[test]
    fn test_output_double_untagged() {
        let val: f64 = 42.4533;
        let double_str = double_to_string(val);
        assert_untagged_output_match("DOUBLE", &double_str, "OUTPUT\tDOUBLE\t42.4533");
    }
    #[test]
    fn test_output_double_whole_untagged() {
        let val: c_double = 42.000000000000001;
        let double_str = double_to_string(val);
        assert_untagged_output_match("DOUBLE", &double_str, "OUTPUT\tDOUBLE\t42.0");
    }
    #[test]
    fn test_output_bool_true_untagged() {
        let val: bool = true;
        assert_untagged_output_match("BOOL", &val, "OUTPUT\tBOOL\ttrue");
    }
    #[test]
    fn test_output_bool_false_untagged() {
        let val: bool = false;
        assert_untagged_output_match("BOOL", &val, "OUTPUT\tBOOL\tfalse");
    }
    #[test]
    fn test_output_tuple_untagged() {
        let val: i64 = 42;
        assert_untagged_output_match("TUPLE", &val, "OUTPUT\tTUPLE\t42");
    }
    #[test]
    fn test_output_array_untagged() {
        let val: i64 = 42;
        assert_untagged_output_match("ARRAY", &val, "OUTPUT\tARRAY\t42");
    }
    fn assert_untagged_output_match(ty: &str, val: &dyn Display, expected_str: &str) {
        let mut out: Vec<u8> = Vec::new();
        output(ty, &val, null_mut(), &mut out).unwrap();
        let actual = get_byte_vec_as_string(out);
        let expected = get_string_with_line_ending(expected_str);
        assert_eq!(actual, expected);
    }
    fn get_string_with_line_ending(value: &str) -> String {
        let ending = get_byte_vec_as_string(LINE_ENDING.to_vec());
        value.to_owned() + ending.as_str()
    }
    fn get_byte_vec_as_string(out: Vec<u8>) -> String {
        let val = std::str::from_utf8(&out).unwrap();
        let result = val.to_string();
        result
    }
}
