// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::{
    ffi::{c_char, c_double, CString},
    fmt::Display,
    io::Write,
};

use crate::strings::double_to_string;

#[cfg(windows)]
pub const LINE_ENDING: &[u8] = b"\r\n";
#[cfg(not(windows))]
pub const LINE_ENDING: &[u8] = b"\n";

/// Holds output messages from calls to the QIR
/// output recording functions and message calls.
pub struct OutputRecorder {
    buffer: Vec<u8>,
    use_std_out: bool,
}

impl Write for OutputRecorder {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        if self.use_std_out {
            std::io::stdout().write(buf)
        } else {
            self.buffer.write(buf)
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        if self.use_std_out {
            std::io::stdout().flush()
        } else {
            self.buffer.flush()
        }
    }
}

impl Default for OutputRecorder {
    fn default() -> Self {
        OutputRecorder {
            buffer: Vec::new(),
            use_std_out: true,
        }
    }
}

impl OutputRecorder {
    /// Sets whether the output should be written to stdout
    /// or stored in the buffer.
    pub fn use_std_out(&mut self, use_std_out: bool) {
        self.use_std_out = use_std_out;
    }

    /// Writes the newline char(s) to the output.
    pub fn write_newline(&mut self) {
        self.write_all(LINE_ENDING).expect("Failed to write output");
    }

    /// Drains the buffer and returns the contents.
    pub fn drain(&mut self) -> std::vec::Drain<u8> {
        self.buffer.drain(..)
    }
}

thread_local! {
    pub static OUTPUT: std::cell::RefCell<Box<OutputRecorder>> = std::cell::RefCell::new(Box::new(OutputRecorder::default()));
}

fn output(ty: &str, val: &dyn Display, tag: *mut c_char) -> std::io::Result<()> {
    OUTPUT.with(|output| {
        let mut output = output.borrow_mut();
        output
            .write_fmt(format_args!("OUTPUT\t{ty}\t{val}"))
            .expect("Failed to write output");
        if !tag.is_null() {
            output.write_all(b"\t").expect("Failed to write output");
            unsafe {
                output
                    .write_all(CString::from_raw(tag).as_bytes())
                    .expect("Failed to write output");
            }
        }
        output.write_newline();
    });
    Ok(())
}

/// Inserts a marker in the generated output that indicates the
/// start of an array and how many array elements it has. The second
/// parameter defines a string label for the array. Depending on
/// the output schema, the label is included in the output or omitted.
#[no_mangle]
pub extern "C" fn __quantum__rt__array_record_output(val: i64, tag: *mut c_char) {
    output("ARRAY", &val, tag).expect("Failed to write array output");
}

/// Inserts a marker in the generated output that indicates the
/// start of a tuple and how many tuple elements it has. The second
/// parameter defines a string label for the tuple. Depending on
/// the output schema, the label is included in the output or omitted.
#[no_mangle]
pub extern "C" fn __quantum__rt__tuple_record_output(val: i64, tag: *mut c_char) {
    output("TUPLE", &val, tag).expect("Failed to write tuple output");
}

#[no_mangle]
pub extern "C" fn __quantum__rt__int_record_output(val: i64, tag: *mut c_char) {
    output("INT", &val, tag).expect("Failed to write int output");
}

#[no_mangle]
pub extern "C" fn __quantum__rt__double_record_output(val: c_double, tag: *mut c_char) {
    output("DOUBLE", &double_to_string(val), tag).expect("Failed to write double output");
}

#[no_mangle]
pub extern "C" fn __quantum__rt__bool_record_output(val: bool, tag: *mut c_char) {
    output("BOOL", &val, tag).expect("Failed to write bool output");
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

pub mod legacy {
    use std::{ffi::c_double, ptr::null_mut};

    use crate::strings::double_to_string;

    #[allow(non_snake_case)]
    pub extern "C" fn __quantum__rt__array_start_record_output() {
        println!("RESULT\tARRAY_START");
    }

    #[allow(non_snake_case)]
    pub extern "C" fn __quantum__rt__array_end_record_output() {
        println!("RESULT\tARRAY_END");
    }

    #[allow(non_snake_case)]
    pub extern "C" fn __quantum__rt__tuple_start_record_output() {
        println!("RESULT\tTUPLE_START");
    }

    #[allow(non_snake_case)]
    pub extern "C" fn __quantum__rt__tuple_end_record_output() {
        println!("RESULT\tTUPLE_END");
    }

    #[allow(non_snake_case)]
    pub extern "C" fn __quantum__rt__int_record_output(val: i64) {
        println!("RESULT\t{val}");
    }

    #[allow(non_snake_case)]
    pub extern "C" fn __quantum__rt__double_record_output(val: c_double) {
        println!("RESULT\t{}", double_to_string(val));
    }

    #[allow(non_snake_case)]
    pub extern "C" fn __quantum__rt__bool_record_output(val: bool) {
        println!("RESULT\t{val}");
    }

    #[allow(non_snake_case)]
    pub extern "C" fn __quantum__rt__array_record_output(val: i64) {
        super::__quantum__rt__array_record_output(val, null_mut());
    }

    #[allow(non_snake_case)]
    pub extern "C" fn __quantum__rt__tuple_record_output(val: i64) {
        super::__quantum__rt__tuple_record_output(val, null_mut());
    }
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
        let val: c_double = 42.000_000_000_000_001;
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
        OUTPUT.with(|output| output.borrow_mut().use_std_out(false));
        output(ty, &val, null_mut()).unwrap();
        let actual =
            OUTPUT.with(|output| get_byte_vec_as_string(output.borrow_mut().drain().as_slice()));
        OUTPUT.with(|output| output.borrow_mut().use_std_out(true));
        let expected = get_string_with_line_ending(expected_str);
        assert_eq!(actual, expected);
    }
    fn get_string_with_line_ending(value: &str) -> String {
        let ending = get_byte_vec_as_string(LINE_ENDING);
        value.to_owned() + ending.as_str()
    }
    fn get_byte_vec_as_string(out: &[u8]) -> String {
        let val = std::str::from_utf8(out).unwrap();
        val.to_string()
    }
}
