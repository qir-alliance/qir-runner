// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use runner::{OUTPUT, run_bytes};

const X_CONTROL: &str = "call void @__quantum__qis__x__body(%Qubit* null)";
const H_CONTROL: &str = "call void @__quantum__qis__h__body(%Qubit* null)";
const H_TARGET: &str = "call void @__quantum__qis__h__body(%Qubit* inttoptr (i64 1 to %Qubit*))";

fn ir(declarations: &str, body: &str, result_qubit: u64) -> String {
    format!(
        r#"
%Array = type opaque
%Qubit = type opaque
%Result = type opaque

define void @main() #0 {{
entry:
  %controls = call %Array* @__quantum__rt__array_create_1d(i32 8, i64 1)
  %slot = call i8* @__quantum__rt__array_get_element_ptr_1d(%Array* %controls, i64 0)
  %qubit_slot = bitcast i8* %slot to %Qubit**
  store %Qubit* null, %Qubit** %qubit_slot
  {body}
  call void @__quantum__qis__mz__body(%Qubit* inttoptr (i64 {result_qubit} to %Qubit*), %Result* null)
  call void @__quantum__rt__result_record_output(%Result* null, i8* null)
  ret void
}}

declare %Array* @__quantum__rt__array_create_1d(i32, i64)
declare i8* @__quantum__rt__array_get_element_ptr_1d(%Array*, i64)
declare void @__quantum__qis__x__body(%Qubit*)
declare void @__quantum__qis__h__body(%Qubit*)
declare void @__quantum__qis__mz__body(%Qubit*, %Result*)
declare void @__quantum__rt__result_record_output(%Result*, i8*)
{declarations}
attributes #0 = {{ "entry_point" }}
"#
    )
}

fn assert_result(name: &str, ir: &str, expected: u8) {
    OUTPUT.with(|output| output.borrow_mut().use_std_out(false));
    let mut output = Vec::new();
    run_bytes(ir.as_bytes(), None, 1, None, &mut output)
        .unwrap_or_else(|error| panic!("{name}: {error}\n{ir}"));
    let output = String::from_utf8(output).unwrap();
    assert!(
        output.contains(&format!("OUTPUT\tRESULT\t{expected}")),
        "{name}: {output}"
    );
}

#[test]
fn flat_controlled_rotations() {
    for (gate, before, after) in [("rx", "", ""), ("ry", "", ""), ("rz", H_TARGET, H_TARGET)] {
        let symbol = format!("__quantum__qis__{gate}__ctl");
        let declaration = format!("declare void @{symbol}(double, %Array*, %Qubit*)");
        let call = format!(
            "call void @{symbol}(double 3.141592653589793, %Array* %controls, %Qubit* inttoptr (i64 1 to %Qubit*))"
        );
        let body = format!("{X_CONTROL}\n  {before}\n  {call}\n  {after}");
        assert_result(gate, &ir(&declaration, &body, 1), 1);
    }

    let declaration = "declare void @__quantum__qis__rx__ctl(double, %Array*, %Qubit*)";
    let call = "call void @__quantum__qis__rx__ctl(double 3.141592653589793, %Array* %controls, %Qubit* inttoptr (i64 1 to %Qubit*))";
    assert_result("inactive control", &ir(declaration, call, 1), 0);
}

const EXP_SETUP: &str = r#"
  call void @__quantum__qis__x__body(%Qubit* inttoptr (i64 2 to %Qubit*))
  %paulis = call %Array* @__quantum__rt__array_create_1d(i32 1, i64 2)
  %p0 = call i8* @__quantum__rt__array_get_element_ptr_1d(%Array* %paulis, i64 0)
  store i8 -15, i8* %p0
  %p1 = call i8* @__quantum__rt__array_get_element_ptr_1d(%Array* %paulis, i64 1)
  store i8 -14, i8* %p1
  %targets = call %Array* @__quantum__rt__array_create_1d(i32 8, i64 2)
  %t0 = call i8* @__quantum__rt__array_get_element_ptr_1d(%Array* %targets, i64 0)
  %q0 = bitcast i8* %t0 to %Qubit**
  store %Qubit* inttoptr (i64 1 to %Qubit*), %Qubit** %q0
  %t1 = call i8* @__quantum__rt__array_get_element_ptr_1d(%Array* %targets, i64 1)
  %q1 = bitcast i8* %t1 to %Qubit**
  store %Qubit* inttoptr (i64 2 to %Qubit*), %Qubit** %q1
"#;

#[test]
fn flat_controlled_r_and_exp() {
    let r_declarations = "declare void @__quantum__qis__r__ctl(i2, double, %Array*, %Qubit*)\ndeclare void @__quantum__qis__r__ctladj(i2, double, %Array*, %Qubit*)";
    let r_call = "call void @__quantum__qis__r__ctl(i2 1, double 3.141592653589793, %Array* %controls, %Qubit* inttoptr (i64 1 to %Qubit*))";
    assert_result(
        "r",
        &ir(r_declarations, &format!("{X_CONTROL}\n  {r_call}"), 1),
        1,
    );

    let r_y = r_call.replace("i2 1", "i2 %pauli");
    let r_y_body = format!(
        "{X_CONTROL}\n  %raw = alloca i8, align 1\n  store i8 -13, i8* %raw, align 1\n  %pauli_ptr = bitcast i8* %raw to i2*\n  %pauli = load i2, i2* %pauli_ptr, align 1\n  {r_y}"
    );
    assert_result("r Y", &ir(r_declarations, &r_y_body, 1), 1);

    let r_inverse = "call void @__quantum__qis__r__ctladj(i2 1, double 1.5707963267948966, %Array* %controls, %Qubit* inttoptr (i64 1 to %Qubit*))";
    let r_forward = r_call.replace("3.141592653589793", "1.5707963267948966");
    assert_result(
        "r adjoint",
        &ir(
            r_declarations,
            &format!("{X_CONTROL}\n  {r_forward}\n  {r_inverse}"),
            1,
        ),
        0,
    );

    let controlled_i = "call void @__quantum__qis__r__ctl(i2 0, double 6.283185307179586, %Array* %controls, %Qubit* inttoptr (i64 1 to %Qubit*))";
    assert_result(
        "controlled I phase",
        &ir(
            r_declarations,
            &format!("{H_CONTROL}\n  {controlled_i}\n  {H_CONTROL}"),
            0,
        ),
        1,
    );

    let exp_declarations = "declare void @__quantum__qis__exp__ctl(%Array*, double, %Array*, %Array*)\ndeclare void @__quantum__qis__exp__ctladj(%Array*, double, %Array*, %Array*)";
    let exp_call = "call void @__quantum__qis__exp__ctl(%Array* %paulis, double 1.5707963267948966, %Array* %controls, %Array* %targets)";
    assert_result(
        "exp",
        &ir(
            exp_declarations,
            &format!("{X_CONTROL}\n  {EXP_SETUP}\n  {exp_call}"),
            1,
        ),
        1,
    );

    let exp_forward = exp_call.replace("1.5707963267948966", "0.7853981633974483");
    let exp_inverse = "call void @__quantum__qis__exp__ctladj(%Array* %paulis, double 0.7853981633974483, %Array* %controls, %Array* %targets)";
    assert_result(
        "exp adjoint",
        &ir(
            exp_declarations,
            &format!("{X_CONTROL}\n  {EXP_SETUP}\n  {exp_forward}\n  {exp_inverse}"),
            1,
        ),
        0,
    );
}

#[test]
fn legacy_controlled_tuples() {
    let rx_declaration = "declare void @__quantum__qis__rx__ctl(%Array*, i8*)";
    let rx_body = format!(
        "{X_CONTROL}\n  %args = alloca {{ double, %Qubit* }}\n  store {{ double, %Qubit* }} {{ double 3.141592653589793, %Qubit* inttoptr (i64 1 to %Qubit*) }}, {{ double, %Qubit* }}* %args\n  %tuple = bitcast {{ double, %Qubit* }}* %args to i8*\n  call void @__quantum__qis__rx__ctl(%Array* %controls, i8* %tuple)"
    );
    assert_result("legacy rx", &ir(rx_declaration, &rx_body, 1), 1);

    let r_declaration = "declare void @__quantum__qis__r__ctl(%Array*, i8*)";
    let r_body = format!(
        "{X_CONTROL}\n  %args = alloca {{ i8, double, %Qubit* }}\n  store {{ i8, double, %Qubit* }} {{ i8 1, double 3.141592653589793, %Qubit* inttoptr (i64 1 to %Qubit*) }}, {{ i8, double, %Qubit* }}* %args\n  %tuple = bitcast {{ i8, double, %Qubit* }}* %args to i8*\n  call void @__quantum__qis__r__ctl(%Array* %controls, i8* %tuple)"
    );
    assert_result("legacy r", &ir(r_declaration, &r_body, 1), 1);
    let r_adj_declarations =
        format!("{r_declaration}\ndeclare void @__quantum__qis__r__ctladj(%Array*, i8*)");
    let r_adj_body = format!(
        "{}\n  call void @__quantum__qis__r__ctladj(%Array* %controls, i8* %tuple)",
        r_body.replace("3.141592653589793", "1.5707963267948966")
    );
    assert_result(
        "legacy r adjoint",
        &ir(&r_adj_declarations, &r_adj_body, 1),
        0,
    );

    let exp_declaration = "declare void @__quantum__qis__exp__ctl(%Array*, i8*)";
    let exp_body = format!(
        "{X_CONTROL}\n  {EXP_SETUP}\n  %args = alloca {{ %Array*, double, %Array* }}\n  %pauli_field = getelementptr {{ %Array*, double, %Array* }}, {{ %Array*, double, %Array* }}* %args, i32 0, i32 0\n  store %Array* %paulis, %Array** %pauli_field\n  %angle_field = getelementptr {{ %Array*, double, %Array* }}, {{ %Array*, double, %Array* }}* %args, i32 0, i32 1\n  store double 1.5707963267948966, double* %angle_field\n  %target_field = getelementptr {{ %Array*, double, %Array* }}, {{ %Array*, double, %Array* }}* %args, i32 0, i32 2\n  store %Array* %targets, %Array** %target_field\n  %tuple = bitcast {{ %Array*, double, %Array* }}* %args to i8*\n  call void @__quantum__qis__exp__ctl(%Array* %controls, i8* %tuple)"
    );
    assert_result("legacy exp", &ir(exp_declaration, &exp_body, 1), 1);
    let exp_adj_declarations =
        format!("{exp_declaration}\ndeclare void @__quantum__qis__exp__ctladj(%Array*, i8*)");
    let exp_adj_body = format!(
        "{}\n  call void @__quantum__qis__exp__ctladj(%Array* %controls, i8* %tuple)",
        exp_body.replace("1.5707963267948966", "0.7853981633974483")
    );
    assert_result(
        "legacy exp adjoint",
        &ir(&exp_adj_declarations, &exp_adj_body, 1),
        0,
    );
}

#[test]
fn unsupported_controlled_arities() {
    for (name, declaration, call, found) in [
        (
            "__quantum__qis__rx__ctl",
            "declare void @__quantum__qis__rx__ctl(double, %Array*, %Qubit*, i8)",
            "call void @__quantum__qis__rx__ctl(double 0.0, %Array* %controls, %Qubit* null, i8 0)",
            "found 4",
        ),
        (
            "__quantum__qis__r__ctladj",
            "declare void @__quantum__qis__r__ctladj(i2, double, %Array*)",
            "call void @__quantum__qis__r__ctladj(i2 1, double 0.0, %Array* %controls)",
            "found 3",
        ),
        (
            "__quantum__qis__exp__ctl",
            "declare void @__quantum__qis__exp__ctl(%Array*, double, %Array*)",
            "call void @__quantum__qis__exp__ctl(%Array* %controls, double 0.0, %Array* %controls)",
            "found 3",
        ),
    ] {
        let error = run_bytes(
            ir(declaration, call, 1).as_bytes(),
            None,
            1,
            None,
            &mut std::io::sink(),
        )
        .unwrap_err();
        assert!(error.contains(name) && error.contains(found), "{error}");
    }
}
