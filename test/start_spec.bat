@ECHO OFF

SET /p spec="Enter filer name of spec file [default value: all]: "

IF "%1" == "" (
   SET spec=all
)

IF NOT "%2" == "" (
   SET spec=all
)

REM PATHS
SET logs=ct\logs
SET paths=-pa
SET paths=%paths% %cd%\ebin

REM Commom Test start options
ECHO ------------------------------------------
ECHO Starting Common Test (Opts)
ECHO ------------------------------------------
ECHO spec file(s) : %spec%
ECHO EBIN Path    : %paths%
ECHO log files    : %logs%
ECHO ------------------------------------------

REM Starting Common Test
ct_run -spec ct/%spec%.spec -logdir %logs% -pa %paths%
