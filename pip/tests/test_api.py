# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

from qirrunner import run, run_bytes, OutputHandler
from qirrunner import _native


def test_run_writes_to_callback() -> None:
    path = "./runner/tests/resources/bv.bc"

    handler = OutputHandler()
    run(path, shots=2, output_fn=handler.handle)
    expected = """START
METADATA\tEntryPoint
INFO\t[One, One, Zero]
END\t0
START
METADATA\tEntryPoint
INFO\t[One, One, Zero]
END\t0
"""
    assert expected == handler.get_output().replace("\r\n", "\n")


def test_run_bytes_writes_to_callback() -> None:
    path = "./runner/tests/resources/bv.bc"
    with open(path, "rb") as f:
        bytes = f.read()

    handler = OutputHandler()
    run_bytes(bytes, shots=2, output_fn=handler.handle)
    expected = """START
METADATA\tEntryPoint
INFO\t[One, One, Zero]
END\t0
START
METADATA\tEntryPoint
INFO\t[One, One, Zero]
END\t0
"""
    assert expected == handler.get_output().replace("\r\n", "\n")


def test_run_bytes_writes_to_callback_from_ll() -> None:
    path = "./runner/tests/resources/teleportation.ll"
    with open(path, "r") as f:
        contents = f.read()

    handler = OutputHandler()
    run_bytes(contents.encode(), shots=2, output_fn=handler.handle)
    expected = """START
METADATA\tentry_point
METADATA\toutput_labeling_schema
METADATA\tqir_profiles\tadaptive_profile
METADATA\trequired_num_qubits\t3
METADATA\trequired_num_results\t12
OUTPUT\tARRAY\t4\t0_a
OUTPUT\tRESULT\t0\t1_a0r
OUTPUT\tRESULT\t1\t2_a1r
OUTPUT\tRESULT\t0\t3_a2r
OUTPUT\tRESULT\t1\t4_a3r
END\t0
START
METADATA\tentry_point
METADATA\toutput_labeling_schema
METADATA\tqir_profiles\tadaptive_profile
METADATA\trequired_num_qubits\t3
METADATA\trequired_num_results\t12
OUTPUT\tARRAY\t4\t0_a
OUTPUT\tRESULT\t0\t1_a0r
OUTPUT\tRESULT\t1\t2_a1r
OUTPUT\tRESULT\t0\t3_a2r
OUTPUT\tRESULT\t1\t4_a3r
END\t0
"""
    assert expected == handler.get_output().replace("\r\n", "\n")


def test_main_works_with_args() -> None:
    _native.main(["qir-runner", "--help"])


def test_main_works_without_args() -> None:
    try:
        _native.main()
    except RuntimeError as ex:
        assert "For more information, try '--help'" in str(ex)
