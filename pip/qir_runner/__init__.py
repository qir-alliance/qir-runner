# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

from qirrunner._native import run, Output


class OutputHandler:
    """
    Callback handler for the output of the QIR runtime functions.
    Output is stored in a list and can be retrieved as a single string.
    """

    def __init__(self):
        self._output = []

    def handle(self, output: Output) -> None:
        """
        Append the output to the internal list.
        """
        self._output.append(str(output))

    def get_output(self) -> str:
        """
        Return the output as a single string.
        """
        return "".join(self._output)


__all__ = ["run", "Output", "OutputHandler"]
