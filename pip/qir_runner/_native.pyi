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

def run_file(
    path: str,
    entry_point: Optional[str],
    shots: Optional[int],
    rng_seed: Optional[int],
    output_fn: Optional[Callable[[Output], None]],
): ...
def main(args: Optional[List[str]]): ...
