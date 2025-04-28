# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

from qirrunner import run, OutputHandler
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


def test_main_works_with_args() -> None:
    path = "./runner/tests/resources/bv.bc"

    _native.main(["qir-runner", "--help"])


def test_main_works_without_args() -> None:
    try:
        _native.main()
    except RuntimeError as ex:
        assert "For more information, try '--help'" in str(ex)
