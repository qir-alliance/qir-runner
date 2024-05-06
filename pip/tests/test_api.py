# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

import qir_runner
from qir_runner import run, Output, OutputHandler


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
    assert expected == handler.get_output()
