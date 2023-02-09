// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::path::PathBuf;
use std::{env, fs};

fn main() -> Result<(), String> {
    let out_dir = env::var_os("OUT_DIR")
        .map(PathBuf::from)
        .ok_or_else(|| "Environment variable OUT_DIR not defined.".to_string())?;

    #[cfg(feature = "ir-range-support")]
    {
        // Compile the LLVM IR bridge file. Requires the llvm-tools-preview component.
        // This is only needed for range support, and this entire build.rs can be dropped when that functionality is
        // no longer needed.

        let llvm_tools = llvm_tools::LlvmTools::new().map_err(|err| {
        format!(
            "Failed to locate llvm tools: {:?}. Is the llvm-tools-preview component installed? Try using `rustup component add llvm-tools-preview`.",
            err
        )
    })?;

        let llc_path = llvm_tools
            .tool(llvm_tools::exe("llc").to_string().as_str())
            .ok_or_else(|| "Failed to find llc.".to_string())?;
        let llvm_ar_path = llvm_tools
            .tool(llvm_tools::exe("llvm-ar").to_string().as_str())
            .ok_or_else(|| "Failed to find llvm-ar.".to_string())?;
        let lib_name = if cfg!(target_os = "windows") {
            "bridge-rt.lib"
        } else {
            "libbridge-rt.a"
        };

        std::process::Command::new(llc_path)
            .args(&[
                "--filetype=obj",
                "./src/bridge-rt.ll",
                "-o",
                &format!("{}/bridge-rt.o", out_dir.display()),
            ])
            .status()
            .map_err(|err| format!("llc failed: {}.", err))?;
        std::process::Command::new(llvm_ar_path)
            .args(&[
                "-r",
                &format!("{}/{}", out_dir.display(), lib_name),
                &format!("{}/bridge-rt.o", out_dir.display()),
            ])
            .status()
            .map_err(|err| format!("llvm-ar failed: {}.", err))?;

        println!("cargo:rustc-link-lib=static=bridge-rt");
        println!("cargo:rustc-link-search=native={}", out_dir.display());
    }

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
