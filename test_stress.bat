@ECHO OFF

Setlocal EnableDelayedExpansion

ECHO ============================================================================
ECHO !TIME! Start run

SET no_runs=%1
IF "%1" == "" (
   SET no_runs=1
)

RD ct\logs /Q /S
MD ct\logs

FOR /L %%G IN (1,1,%no_runs%) DO (
   ECHO ----------------------------------------------------------------------------
   ECHO !TIME! %%G Step: test_generator.bat
   CALL test_generator.bat
   ECHO !TIME! %%G Step: rebar ct
   CALL rebar.cmd ct
)

ECHO ----------------------------------------------------------------------------
ECHO !TIME! End   run
ECHO ============================================================================
