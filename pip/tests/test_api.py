# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

import io

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


def test_main_without_args_empty_input() -> None:
    try:
        _native.main()
        assert False, "Expected RuntimeError for empty input"
    except RuntimeError as ex:
        assert "Input is empty" in str(ex)


def test_run_with_dash_reads_from_stdin(monkeypatch) -> None:
    path = "./runner/tests/resources/teleportation.ll"
    with open(path, "r") as f:
        contents = f.read()

    expected_handler = OutputHandler()
    run(path, shots=1, output_fn=expected_handler.handle)
    expected = expected_handler.get_output().replace("\r\n", "\n")

    monkeypatch.setattr("sys.stdin", io.StringIO(contents))
    handler = OutputHandler()
    run("-", shots=1, output_fn=handler.handle)
    assert expected == handler.get_output().replace("\r\n", "\n")

def test_run_without_args_reads_from_stdin(monkeypatch) -> None:
    path = "./runner/tests/resources/teleportation.ll"
    with open(path, "r") as f:
        contents = f.read()

    expected_handler = OutputHandler()
    run(path, shots=1, output_fn=expected_handler.handle)
    expected = expected_handler.get_output().replace("\r\n", "\n")

    monkeypatch.setattr("sys.stdin", io.StringIO(contents))
    handler = OutputHandler()
    run(shots=1, output_fn=handler.handle)
    assert expected == handler.get_output().replace("\r\n", "\n")
