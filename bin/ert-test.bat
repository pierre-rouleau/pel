`@echo` off
:: BAT FILE: ert-test.bat
::
:: Purpose   : Run an Ert test  (Windows CMD wrapper for ert-test.ps1)
:: Created   : Wednesday, February 25 2026.
:: Author    : Pierre Rouleau <prouleau001@gmail.com>
:: ----------------------------------------------------------------------------
:: Delegates to ert-test.ps1 in the same directory.

setlocal enabledelayedexpansion

set "SCRIPT_DIR=%~dp0"

powershell.exe -NoProfile -ExecutionPolicy Bypass ^
    -File "%SCRIPT_DIR%ert-test.ps1" %*

exit /b %ERRORLEVEL%

rem --------------------------------------------------------------------------
