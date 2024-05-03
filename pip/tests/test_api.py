import qir_runner
from qir_runner import Output


def test_run_file_writes_to_callback() -> None:
    path = "./runner/tests/resources/bv.bc"

    out = []

    def callback(output: Output) -> None:
        out.append(str(output))

    qir_runner.run_file(path, None, 2, None, callback)
    expected = """START
METADATA\tEntryPoint
INFO\t[One, One, Zero]
END\t0
START
METADATA\tEntryPoint
INFO\t[One, One, Zero]
END\t0
"""
    assert expected == "".join(out)
