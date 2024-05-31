#!/usr/bin/env pwsh

# Copyright (c) Microsoft Corporation.
# Licensed under the MIT License.

#Requires -PSEdition Core

function update-noticefiles {
    # use cargo-about to generate a notice files
    # notice files are only for wheel distributions
    # as no bundled sources are in the sdist.

    # llvm special license is already in the template
    # as it is a hidden transitive dependency.
    # https://github.com/EmbarkStudios/cargo-about
    $config = Join-Path $PSScriptRoot notice.toml
    foreach ($project in @("pip", "stdlib")) {
        Push-Location $project
        try {
            $template = Join-Path $PSScriptRoot $project notice.hbs
            $notice = Join-Path $PSScriptRoot $project NOTICE.txt
            cargo about generate --config $config --all-features --output-file $notice $template
            $contents = Get-Content -Raw $notice
            [System.Web.HttpUtility]::HtmlDecode($contents) | Out-File $notice
        }
        finally {
            Pop-Location
        }
    }
}

update-noticefiles
