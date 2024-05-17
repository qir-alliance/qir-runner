// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::path::PathBuf;
use std::{env, fs};

fn main() -> Result<(), String> {
    let out_dir = env::var_os("OUT_DIR")
        .map(PathBuf::from)
        .ok_or_else(|| "Environment variable OUT_DIR not defined.".to_string())?;

    // Copy the include files for non-Rust consumers and make them available for downstream compilation.
    let include_dir = out_dir.join("include");
    fs::create_dir_all(&include_dir)
        .map_err(|err| format!("Error creating 'include' directory: {}", err))?;
    fs::copy("include/qir_stdlib.def", include_dir.join("qir_stdlib.def"))
        .map_err(|err| format!("Error copying 'qir_stdlib.def' file: {}", err))?;
    fs::copy("include/qir_stdlib.h", include_dir.join("qir_stdlib.h"))
        .map_err(|err| format!("Error copying 'qir_stdlib.h' file: {}", err))?;
    println!("cargo:include={}", include_dir.display());

    Ok(())
}
