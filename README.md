# QIR Runner

QIR-Runner is a set of tools for executing [Quantum
Intermediate Representation (QIR)](https://github.com/qir-alliance/qir-spec) programs to assist with QIR development and validation. It consists of the following components:

- [**backend**](backend):
 This package implements a sparse quantum state simulator
- [**runner**](runner):
  This package provides an API or CLI tool to execute QIR.
- [**stdlib**](stdlib)
  This package implements the QIR standard runtime library

## Command Line Usage

```
Usage: qir-runner [OPTIONS] --file <PATH>

Options:
  -f, --file <PATH>        (Required) Path to the QIR file to run
  -e, --entrypoint <NAME>  Name of the entry point function to execute
  -s, --shots <NUM>        The number of times to repeat the execution of the chosen entry point in the program [default: 1]
  -r, --rngseed <NUM>      The value to use when seeding the random number generator used for quantum simulation
  -h, --help               Print help
```

## Documentation

API documentation is available at [https://qir-alliance.github.io/qir-runner](https://qir-alliance.github.io/qir-runner).

## Feedback

If you have feedback about the content in this repository, please let us know by
filing a [new issue](https://github.com/qir-alliance/qir-runner/issues/new)!

## Contributing

There are many ways in which you can contribute to `qir-runner`, whether by
contributing a feature or by engaging in discussions; we value contributions in
all shapes and sizes! We refer to [this document](CONTRIBUTING.md) for
guidelines and ideas for how you can get involved.

Contributing a pull request to this repo requires to agree to a [Contributor
License Agreement
(CLA)](https://en.wikipedia.org/wiki/Contributor_License_Agreement) declaring
that you have the right to, and actually do, grant us the rights to use your
contribution. A CLA-bot will automatically determine whether you need to provide
a CLA and decorate the PR appropriately. Simply follow the
instructions provided by the bot. You will only need to do this once.

## Contributing

This project welcomes contributions and suggestions.  Most contributions require you to agree to a
Contributor License Agreement (CLA) declaring that you have the right to, and actually do, grant us
the rights to use your contribution. For details, visit https://cla.opensource.microsoft.com.

When you submit a pull request, a CLA bot will automatically determine whether you need to provide
a CLA and decorate the PR appropriately (e.g., status check, comment). Simply follow the instructions
provided by the bot. You will only need to do this once across all repos using our CLA.

This project has adopted the [Microsoft Open Source Code of Conduct](https://opensource.microsoft.com/codeofconduct/).
For more information see the [Code of Conduct FAQ](https://opensource.microsoft.com/codeofconduct/faq/) or
contact [opencode@microsoft.com](mailto:opencode@microsoft.com) with any additional questions or comments.

## Trademarks

This project may contain trademarks or logos for projects, products, or services. Authorized use of Microsoft 
trademarks or logos is subject to and must follow 
[Microsoft's Trademark & Brand Guidelines](https://www.microsoft.com/en-us/legal/intellectualproperty/trademarks/usage/general).
Use of Microsoft trademarks or logos in modified versions of this project must not cause confusion or imply Microsoft sponsorship.
Any use of third-party trademarks or logos are subject to those third-party's policies.
