@ECHO OFF

SET no_runs=%1
IF "%1" == "" (
   SET no_runs=1
)

Setlocal EnableDelayedExpansion

RD ct\logs /Q /S
MD ct\logs

FOR /L %%G IN (1,1,%no_runs%) DO (
   ECHO ============================================================================
   ECHO !TIME! %%G Start run
   ECHO !TIME! %%G Step: test_generator.bat
   test_generator.bat
   ECHO ----------------------------------------------------------------------------
   ECHO !TIME! %%G Step: rebar ct
   rebar ct
   ECHO !TIME! %%G End   run
)
ECHO ============================================================================

PAUSE
