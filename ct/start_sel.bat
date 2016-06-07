@ECHO OFF

SET /P suite="Mandatory: Enter filer name of test suite [ct/xxx_SUITE]: "
SET /P group="Optional : Enter filer name of test case group(s) [list]: "
SET /P case="Optional : Enter filer name of test case(s)       [list]: "

REM PATHS
SET logs=ct\logs
SET paths=-pa
SET paths=%paths% %cd%\ebin

REM Commom Test start options
ECHO ------------------------------------------
ECHO Starting Common Test (Opts)
ECHO ------------------------------------------
ECHO test suite         : %suite%
ECHO test case group(s) : %group%
ECHO test case(s)       : %case%
ECHO EBIN Path          : %paths%
ECHO log files          : %logs%
ECHO ------------------------------------------

REM Starting Common Test
if "%suite%"=="" (
    ECHO.
    ECHO ========================================
    ECHO == Error: No test suite specified !!! ==
    ECHO ========================================
    GOTO END
)

IF "%group%"=="" IF "%case%"=="" (
    ct_run -suite ct\%suite%_SUITE -logdir %logs% -pa %paths%
    GOTO END
)

IF "%group%"=="" IF NOT "%case%"=="" (
    ct_run -suite ct\%suite%_SUITE -case %case% -logdir %logs% -pa %paths%
    GOTO END
)

IF "%case%"=="" (
    ct_run -suite ct\%suite%_SUITE -group [%group%] -logdir %logs% -pa %paths%
    GOTO END
)

ct_run -suite ct\%suite%_SUITE -group [%group%] -case %case% -logdir %logs% -pa %paths%

:END
