// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

#![deny(clippy::all, clippy::pedantic)]

use std::env;

fn main() -> Result<(), String> {
    let args: Vec<String> = env::args().collect();
    if args.len() == 1 {
        println!("OVERVIEW: Virtual execution environment for QIR programs");
        println!();
        println!("USAGE: qir-runner <ll/bc filename> [entry point function]");
        Ok(())
    } else {
        let (path, args) = args.split_at(2);
        qir_runner::run_file(&path[1], args)
    }
}
