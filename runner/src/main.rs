// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#![deny(clippy::all, clippy::pedantic)]

use std::path::PathBuf;

use clap::{arg, value_parser, Command};
// use std::env;

fn main() -> Result<(), String> {
    let cmd = Command::new("qir-runner").args(&[
        arg!(-f --file <PATH> "(Required) Path to the QIR file to run")
            .value_parser(value_parser!(PathBuf))
            .required(true),
        arg!(-e --entrypoint <NAME> "Name of the entry point function to execute"),
        arg!(-s --shots <NUM> "The number of times to repeat the execution of the chosen entry point in the program")
            .value_parser(value_parser!(u32))
            .default_value("1")]);

    let matches = cmd.try_get_matches().map_err(|e| e.to_string());
    match matches {
        Err(e) => {
            print!("{}", e);
            Ok(())
        }
        Ok(matches) => qir_runner::run_file(
            matches.get_one::<PathBuf>("file").unwrap(),
            matches
                .get_one::<String>("entrypoint")
                .map(std::string::String::as_str),
            *matches.get_one::<u32>("shots").unwrap(),
        ),
    }
}
