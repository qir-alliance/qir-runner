// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#![deny(clippy::all, clippy::pedantic)]

use std::env;

mod lib;

fn main() -> Result<(), String> {
    let args: Vec<String> = env::args().collect();
    if args.len() == 1 {
        println!("OVERVIEW: Virtual execution environment for QIR programs");
        println!();
        println!("USAGE: qir-runner <ll/bc filename> [entry point function]");
        Ok(())
    } else {
        let path = &args[1];
        let name = if args.len() > 2 {
            Some(args[2].as_str())
        } else {
            None
        };
        lib::run_file(path, name)
    }
}
