# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

from qirrunner import run, OutputHandler


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
