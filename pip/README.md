# QIR Runner

This project implements a basic QIR runtime and execution tool. Once installed, `qir-runner` will be available via the command line in your python environment as well as the `qirrunner` module which can be imported into a Python program.

## Usage

### Command line

```shell
Usage: qir-runner [OPTIONS] --file <PATH>

Options:
  -f, --file <PATH>        (Required) Path to the QIR file to run
  -e, --entrypoint <NAME>  Name of the entry point function to execute
  -s, --shots <NUM>        The number of times to repeat the execution of the chosen entry point in the program [default: 1]
  -r, --rngseed <NUM>      The value to use when seeding the random number generator used for quantum simulation
  -h, --help               Print help
  -V, --version            Print version
```

### Python module

From a Python program, `qirrunner` provides a `run` function and two output helpers `Output` and `OutputHandler`. If the `output_fn` parameter of `run` is not specified, output will be written to `stdout`. Supplying the parameter allows the output of the execution to be captured.

```python
from qirrunner import run, OutputHandler

path = "./runner/tests/resources/bv.bc"

handler = OutputHandler()
run(path, shots=2, output_fn=handler.handle)

print(handler.get_output())
```

## Installation

```shell
pip install qirrunner
```

### Installing from sdist

Platforms for which `qirrunner` doesn't have pre-built wheels (such as `aarch64` macos), installation is available via sdist. Before installing `qirrunner` via `pip`:

- Install a usable LLVM distribution which has `llvm-config` available.
- Set the `LLVM_SYS_201_PREFIX` environment variable to the LLVM installation directory
  - example: `export LLVM_SYS_201_PREFIX=/Users/sample/llvm`
- Install: `python -m pip install qirrunner`
  - This will build `qirrunner` from source. You will need a working Rust installation in order for this to compile.

## Implemented APIs

```llvm
double @__quantum__qis__arccos__body(double)
double @__quantum__qis__arcsin__body(double)
double @__quantum__qis__arctan__body(double)
double @__quantum__qis__arctan2__body(double)
void @__quantum__qis__assertmeasurementprobability__body(ptr, ptr, ptr, double, ptr, double)
void @__quantum__qis__assertmeasurementprobability__ctl(ptr, ptr)
void @__quantum__qis__barrier__body()
void @__quantum__qis__ccx__body(ptr, ptr)
void @__quantum__qis__cnot__body(ptr, ptr)
double @__quantum__qis__cos__body(double)
double @__quantum__qis__cosh__body(double)
void @__quantum__qis__cx__body(ptr, ptr)
void @__quantum__qis__cy__body(ptr, ptr)
void @__quantum__qis__cz__body(ptr, ptr)
double @__quantum__qis__drawrandomdouble__body()
i64 @__quantum__qis__drawrandomint__body()
void @__quantum__qis__dumpmachine__body()
void @__quantum__qis__exp__adj(ptr, double, ptr)
void @__quantum__qis__exp__body(ptr, double, ptr)
void @__quantum__qis__exp__ctl(ptr, ptr)
void @__quantum__qis__exp__ctladj(ptr, ptr)
void @__quantum__qis__exp__ctl(ptr, ptr)
void @__quantum__qis__h__body(ptr)
void @__quantum__qis__h__ctl(ptr, ptr)
double @__quantum__qis__ieeeremainder__body(double, double)
double @__quantum__qis__infinity__body()
i1 @__quantum__qis__isinf__body()
i1 @__quantum__qis__isnan__body()
i1 @__quantum__qis__isnegativeinfinity__body()
double @__quantum__qis__log__body(double)
ptr @__quantum__qis__m__body(ptr)
ptr @__quantum__qis__measure__body(ptr, ptr)
ptr @__quantum__qis__mresetz__body(ptr)
void @__quantum__qis__mz__body(ptr, ptr)
double @__quantum__qis__nan__body()
void @__quantum__qis__r__adj(i2, double, ptr)
void @__quantum__qis__r__body(i2, double, ptr)
void @__quantum__qis__r__ctl(ptr, ptr)
void @__quantum__qis__r__ctladj(ptr, ptr)
bool @__quantum__qis__read_result__body(ptr)
void @__quantum__qis__reset__body(ptr)
void @__quantum__qis__rx__body(double, ptr)
void @__quantum__qis__rx__ctl(ptr, ptr)
void @__quantum__qis__rxx__body(double, ptr, ptr)
void @__quantum__qis__ry__body(double, ptr)
void @__quantum__qis__ry__ctl(ptr, ptr)
void @__quantum__qis__ryy__body(double, ptr, ptr)
void @__quantum__qis__rz__body(double, ptr)
void @__quantum__qis__rz__ctl(ptr, ptr)
void @__quantum__qis__rzz__body(double, ptr, ptr)
void @__quantum__qis__s__adj(ptr)
void @__quantum__qis__s__body(ptr)
void @__quantum__qis__s__ctl(ptr, ptr)
void @__quantum__qis__s__ctladj(ptr, ptr)
void @__quantum__qis__sx__body(ptr)
double @__quantum__qis__sin__body(double)
double @__quantum__qis__sinh__body(double)
double @__quantum__qis__sqrt__body(double)
void @__quantum__qis__swap__body(ptr, ptr)
void @__quantum__qis__t__adj(ptr)
void @__quantum__qis__t__body(ptr)
void @__quantum__qis__t__ctl(ptr, ptr)
void @__quantum__qis__t__ctladj(ptr, ptr)
double @__quantum__qis__tan__body(double)
double @__quantum__qis__tanh__body(double)
void @__quantum__qis__x__body(ptr)
void @__quantum__qis__x__ctl(ptr, ptr)
void @__quantum__qis__y__body(ptr)
void @__quantum__qis__y__ctl(ptr, ptr)
void @__quantum__qis__z__body(ptr)
void @__quantum__qis__z__ctl(ptr, ptr)
ptr @__quantum__rt__array_concatenate(ptr, ptr)
ptr @__quantum__rt__array_copy(ptr, bool)
ptr @__quantum__rt__array_create_1d(i32, i64)
ptr @__quantum__rt__array_get_element_ptr_1d(ptr, i64)
i64 @__quantum__rt__array_get_size_1d(ptr)
void @__quantum__rt__array_record_output(i64, ptr)
void @__quantum__rt__array_update_alias_count(ptr, i32)
void @__quantum__rt__array_update_reference_count(ptr, i32)
ptr @__quantum__rt__bigint_add(ptr, ptr)
ptr @__quantum__rt__bigint_bitand(ptr, ptr)
ptr @__quantum__rt__bigint_bitnot(ptr)
ptr @__quantum__rt__bigint_bitor(ptr, ptr)
ptr @__quantum__rt__bigint_bitxor(ptr, ptr)
ptr @__quantum__rt__bigint_create_array(i32, ptr)
ptr @__quantum__rt__bigint_create_i64(i64)
ptr @__quantum__rt__bigint_divide(ptr, ptr)
bool @__quantum__rt__bigint_equal(ptr, ptr)
ptr @__quantum__rt__bigint_get_data(ptr)
i32 @__quantum__rt__bigint_get_length(ptr)
bool @__quantum__rt__bigint_greater(ptr, ptr)
bool @__quantum__rt__bigint_greater_eq(ptr, ptr)
ptr @__quantum__rt__bigint_modulus(ptr, ptr)
ptr @__quantum__rt__bigint_multiply(ptr, ptr)
ptr @__quantum__rt__bigint_negate(ptr)
ptr @__quantum__rt__bigint_power(ptr, i32)
ptr @__quantum__rt__bigint_shiftleft(ptr, i64)
ptr @__quantum__rt__bigint_shiftright(ptr, i64)
void @__quantum__rt__bigint_subtract(ptr, ptr)
ptr @__quantum__rt__bigint_to_string(ptr)
void @__quantum__rt__bigint_update_reference_count(ptr, i32)
void @__quantum__rt__bool_record_output(i1, ptr)
ptr @__quantum__rt__bool_to_string(i1)
ptr @__quantum__rt__callable_copy(ptr, bool)
ptr @__quantum__rt__callable_create(ptr, ptr, ptr)
void @__quantum__rt__callable_invoke(ptr, ptr, ptr)
void @__quantum__rt__callable_make_adjoint(ptr)
void @__quantum__rt__callable_make_controlled(ptr)
void @__quantum__rt__callable_update_alias_count(ptr, i32)
void @__quantum__rt__callable_update_reference_count(ptr, i32)
void @__quantum__rt__capture_update_alias_count(ptr, i32)
void @__quantum__rt__capture_update_reference_count(ptr, i32)
void @__quantum__rt__double_record_output(double, ptr)
ptr @__quantum__rt__double_to_string(double)
void @__quantum__rt__fail(ptr)
void @__quantum__rt__int_record_output(i64, ptr)
ptr @__quantum__rt__int_to_string(i64)
ptr @__quantum__rt__memory_allocate(i64)
void @__quantum__rt__message(ptr)
void @__quantum__rt__message_record_output(ptr)
ptr @__quantum__rt__pauli_to_string(i2)
ptr @__quantum__rt__qubit_allocate()
ptr @__quantum__rt__qubit_allocate_array(i64)
void @__quantum__rt__qubit_release(ptr)
void @__quantum__rt__qubit_release_array(ptr)
ptr @__quantum__rt__qubit_to_string(ptr)
bool @__quantum__rt__read_result(ptr)
bool @__quantum__rt__result_equal(ptr, ptr)
ptr @__quantum__rt__result_get_one()
ptr @__quantum__rt__result_get_zero()
void @__quantum__rt__result_record_output(ptr, ptr)
ptr @__quantum__rt__result_to_string(ptr)
void @__quantum__rt__result_update_reference_count(ptr, i32)
ptr @__quantum__rt__string_concatenate(ptr, ptr)
ptr @__quantum__rt__string_create(ptr)
bool @__quantum__rt__string_equal(ptr, ptr)
ptr @__quantum__rt__string_get_data(ptr)
i32 @__quantum__rt__string_get_length(ptr)
void @__quantum__rt__string_update_reference_count(ptr, i32)
ptr @__quantum__rt__tuple_copy(ptr, i1)
ptr @__quantum__rt__tuple_create(i64)
void @__quantum__rt__tuple_record_output(i64, ptr)
void @__quantum__rt__tuple_update_alias_count(ptr, i32)
void @__quantum__rt__tuple_update_reference_count(ptr, i32)
```
