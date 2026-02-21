@echo off
setlocal enabledelayedexpansion

REM Arl CLI launcher for Windows
REM Equivalent of inst/bin/posix/arl -- starts R interactively with a
REM temporary profile that loads the arl package and runs the CLI.

if "%R_HOME%"=="" (
  for /f "tokens=*" %%i in ('R RHOME 2^>nul') do set "R_HOME=%%i"
)
if "%R_HOME%"=="" (
  echo Error: R_HOME is not set and R is not on PATH.
  exit /b 1
)

REM Create a temporary R profile
set "TEMP_PROFILE=%TEMP%\arl_cli_%RANDOM%%RANDOM%.R"

(
echo # Arl CLI launcher - auto-generated temporary profile
echo local({
echo   on.exit(q(save = "no", status = 0^), add = TRUE^)
echo   if (!nzchar(Sys.getenv("ARL_SKIP_USER_PROFILE"^)^)^) {
echo     user_profile ^<- path.expand("~/.Rprofile"^)
echo     if (file.exists(user_profile^)^) {
echo       tryCatch(source(user_profile^), error = function(e^) {
echo         warning("Error loading ~/.Rprofile: ", conditionMessage(e^)^)
echo       }^)
echo     }
echo   }
echo   suppressPackageStartupMessages(library(arl^)^)
echo   tryCatch(
echo     cli(args = commandArgs(trailingOnly = TRUE^)^),
echo     arl_cli_error = function(e^) {
echo       q(save = "no", status = 1^)
echo     }
echo   ^)
echo }^)
) > "%TEMP_PROFILE%"

REM When skipping user profile, preserve library paths
if defined ARL_SKIP_USER_PROFILE (
  for /f "tokens=*" %%j in ('Rscript -e "cat(paste(.libPaths(), collapse=';'))" 2^>nul') do set "R_LIBS=%%j"
)

set "R_PROFILE_USER=%TEMP_PROFILE%"
"%R_HOME%\bin\R.exe" --quiet --no-save --no-restore --args %*

set "EXIT_CODE=%ERRORLEVEL%"
del /f "%TEMP_PROFILE%" 2>nul
exit /b %EXIT_CODE%
