# PS1 FILE: ert-test.ps1
#
# Purpose   : Run an Ert test  (Windows PowerShell equivalent of bin/ert-test)
# Created   : Wednesday, February 25 2026.
# Author    : Pierre Rouleau <prouleau001@gmail.com>
# ----------------------------------------------------------------------------
# Module Description
# ------------------
#
# Run an Ert test file specified on the command line argument.
# Activate the maximum level of back-trace capability.
# Windows PowerShell equivalent of the POSIX bin/ert-test shell script.
#
# Usage: ert-test.ps1  TEST-FILE  [EMACS-BIN]
#        ert-test.ps1  -h | --help

param(
    [Parameter(Position = 0)]
    [string]$TestFile,

    [Parameter(Position = 1)]
    [string]$EmacsBin
)

$pgmName = Split-Path -Leaf $PSCommandPath

function Print-Usage {
    Write-Host @"

$pgmName -- Run the Emacs Ert test.

 Usage: $pgmName -h|--help

  * Print this help information.

 Usage: $pgmName TEST-FILE [EMACS-BIN]

  * Print the name of TEST-FILE and run the Emacs Ert test.
  * If the EMACS-BIN argument is specified it identifies the name of
    the emacs executable.
  * When the test passes, create a file with the same name and a
    '.test-passed' suffix and exit with an exit code of 0.
    These tag files are used by the Makefile to identify tests that
    do not need to be re-executed.
  * Exit with 1 on error.

"@
}

# ---------------------------------------------------------------------------
# Check validity of arguments

if (-not $TestFile -or $TestFile -eq '-h' -or $TestFile -eq '--help') {
    Print-Usage
    if ($TestFile -eq '-h' -or $TestFile -eq '--help') {
        exit 0
    }
    exit 1
}

# Determine the Emacs binary: explicit arg > $EMACS env var > 'emacs'
if ($EmacsBin) {
    $emacsBinary = $EmacsBin
} elseif ($env:EMACS) {
    $emacsBinary = $env:EMACS
} else {
    $emacsBinary = 'emacs'
}

# ---------------------------------------------------------------------------
# Proceed.

Write-Host "----------------- Testing ${TestFile}:"

if (-not (Test-Path $TestFile)) {
    Write-Host "*** ERROR: file does not exist: $TestFile"
    exit 1
}

# Build argument list
$emacsArgs = @(
    '-batch',
    '-L', '.',
    '-l', 'ert',
    '-l', $TestFile,
    '--eval', '(setq ert-batch-print-length nil ert-batch-print-level nil)',
    '-f', 'ert-run-tests-batch-and-exit'
)

# Echo the command (equivalent of set -x)
Write-Host "+ $emacsBinary $($emacsArgs -join ' ')"

# Set verbose env var for the child process
$env:EMACS_TEST_VERBOSE = '1'
& $emacsBinary `@emacsArgs`
$exitCode = $LASTEXITCODE
Remove-Item Env:\EMACS_TEST_VERBOSE -ErrorAction SilentlyContinue

if ($exitCode -eq 0) {
    # Create the .test-passed tag file (equivalent of 'touch')
    New-Item -ItemType File -Path "${TestFile}.test-passed" -Force | Out-Null
    Write-Host "---------------------------------------------------------------"
    exit 0
} else {
    exit 1
}

# ----------------------------------------------------------------------------
