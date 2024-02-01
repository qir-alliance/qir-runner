# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

from typing import List, Optional

def run_file(
    path: str, entry_point: Optional[str], shots: Optional[int], rng_seed: Optional[int]
): ...
def main(args: Optional[List[str]]): ...
