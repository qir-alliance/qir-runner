// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#![deny(clippy::all, clippy::pedantic)]

use clap::error::ErrorKind;
use clap::{Command, arg, crate_version, value_parser};
use std::io::IsTerminal;
use std::{ffi::OsString, path::PathBuf};

/// # Errors
/// Returns an error if the arguments are invalid.
/// # Panics
/// Panics if the arguments cannot be read.
pub fn main<I, T>(args: Option<I>) -> Result<(), String>
where
    I: IntoIterator<Item = T>,
    T: Into<OsString> + Clone,
{
    let cmd = Command::new("qir-runner").args(&[
        arg!(-f --file <PATH> "Path to the QIR file to run. If not provided or '-', standard input will be read.")
            .value_parser(value_parser!(PathBuf)),
        arg!(-e --entrypoint <NAME> "Name of the entry point function to execute"),
        arg!(-s --shots <NUM> "The number of times to repeat the execution of the chosen entry point in the program")
            .value_parser(value_parser!(u32))
            .default_value("1"),
        arg!(-r --rngseed <NUM> "The value to use when seeding the random number generator used for quantum simulation")
            .value_parser(value_parser!(u64))
        ]).version(crate_version!());
    let mut help_cmd = cmd.clone();
    let matches = match args {
        Some(args) => cmd.try_get_matches_from(args),
        None => cmd.try_get_matches(),
    };
    match matches {
        Err(e) => {
            let msg = e.to_string();
            match e.kind() {
                ErrorKind::DisplayHelp | ErrorKind::DisplayVersion => {
                    eprint!("{msg}");
                    Ok(())
                }
                _ => Err(msg),
            }
        }
        Ok(matches) => {
            let file = matches.get_one::<PathBuf>("file").and_then(|path| {
                if path.as_os_str() == "-" {
                    None
                } else {
                    Some(path)
                }
            });
            let entry_point = matches
                .get_one::<String>("entrypoint")
                .map(std::string::String::as_str);
            let shots = *matches
                .get_one::<u32>("shots")
                .expect("Shots is required or should have a default value");
            let rng_seed = matches
                .try_get_one::<u64>("rngseed")
                .map_or(None, Option::<&u64>::copied);
            let output = &mut std::io::stdout();

            if let Some(path) = file {
                crate::run_file(path, entry_point, shots, rng_seed, output)
            } else {
                if std::io::stdin().is_terminal() {
                    return help_cmd.print_help().map_err(|e| e.to_string());
                }
                crate::run_input(
                    &mut std::io::stdin().lock(),
                    entry_point,
                    shots,
                    rng_seed,
                    output,
                )
            }
        }
    }
}
