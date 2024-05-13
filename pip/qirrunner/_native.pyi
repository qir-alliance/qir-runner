# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

from typing import Callable, List, Optional

class Output:
    """
    An output recording from the QIR runtime functions.
    These are normally printed to the console.
    """

    def __repr__(self) -> str: ...
    def __str__(self) -> str: ...
    def _repr_html_(self) -> str: ...

def run(
    path: str,
    entry_point: Optional[str] = None,
    shots: Optional[int] = None,
    rng_seed: Optional[int] = None,
    output_fn: Optional[Callable[[Output], None]] = None,
):
    """
    Runs the supplied QIR (bitcode or textual IR) file.
    :param path: (Required) Path to the QIR file to run
    :param entry_point: Name of the entry point function to execute.
        Default is `None`.
    :param shots: The number of times to repeat the execution of
        the chosen entry point in the program. Default is 1.
    :param rngseed: The value to use when seeding the random number generator
        used for quantum simulation.
    :param output_fn: A callback function to receive the output of the QIR
        runtime functions. Default is `None`. When no callback is provided,
        the output is printed to the console.
    """

def main(args: Optional[List[str]]):
    """
    Main function for the QIR runner. This is only used when the runner is
    invoked from the command line and should not be used in other contexts.
    """
