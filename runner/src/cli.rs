// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#![deny(clippy::all, clippy::pedantic)]

use clap::error::ErrorKind;
use clap::{arg, crate_version, value_parser, Command};
use std::env::{self, ArgsOs};
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
        arg!(-f --file <PATH> "(Required) Path to the QIR file to run")
            .value_parser(value_parser!(PathBuf))
            .required(true),
        arg!(-e --entrypoint <NAME> "Name of the entry point function to execute"),
        arg!(-s --shots <NUM> "The number of times to repeat the execution of the chosen entry point in the program")
            .value_parser(value_parser!(u32))
            .default_value("1"),
        arg!(-r --rngseed <NUM> "The value to use when seeding the random number generator used for quantum simulation")
            .value_parser(value_parser!(u64))
        ]).version(crate_version!());
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
        Ok(matches) => crate::run_file(
            matches
                .get_one::<PathBuf>("file")
                .expect("File path is required"),
            matches
                .get_one::<String>("entrypoint")
                .map(std::string::String::as_str),
            *matches
                .get_one::<u32>("shots")
                .expect("Shots is required or should have a default value"),
            matches
                .try_get_one::<u64>("rngseed")
                .map_or(None, Option::<&u64>::copied),
            &mut std::io::stdout(),
        ),
    }
}
