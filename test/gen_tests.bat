@echo off
rem ----------------------------------------------------------------------------
rem
rem gen_tests.bat: opencypher - generating test data.
rem
rem Copyright (c) 2017 Walter Weinmann.  All Rights Reserved.
rem
rem This file is provided to you under the Apache License,
rem Version 2.0 (the "License"); you may not use this file
rem except in compliance with the License.  You may obtain
rem a copy of the License at
rem
rem   http://www.apache.org/licenses/LICENSE-2.0
rem
rem Unless required by applicable law or agreed to in writing,
rem software distributed under the License is distributed on an
rem "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
rem KIND, either express or implied.  See the License for the
rem specific language governing permissions and limitations
rem under the License.
rem
rem ----------------------------------------------------------------------------

> gen_tests.log (

    SETLOCAL enableDelayedExpansion
    ECHO !time! Start Test Data Generation

    IF EXIST _build\test\lib\ocparse\test\generated (
        RD /Q /S _build\test\lib\ocparse\test\generated
    )
    IF EXIST _build\test\lib\ocparse\test\performance_*.* (
        DEL /Q _build\test\lib\ocparse\test\performance_*.*
    )
    IF EXIST _build\test\lib\ocparse\test\reliability_*.* (
        DEL /Q _build\test\lib\ocparse\test\reliability_*.*
    )
    IF EXIST test\generated (
        RD /Q /S test\generated
    )

    CALL rebar3 as test compile

    REM Setting ocparse options ...............................................
    IF "%GENERATE_COMPACTED%" == "" (
        REM true: compacted / false: detailed.
        SET GENERATE_COMPACTED=true
        SET GENERATE_CT=true
        SET GENERATE_EUNIT=false
        SET GENERATE_PERFORMANCE=true
        SET GENERATE_RELIABILITY=true
        SET HEAP_SIZE=+hms 67108864
        SET LOGGING=false
        SET MAX_BASIC_RULE=100
    )

    REM Starting test data generator ...........................................
    erl -noshell -pa _build\test\lib\ocparse\test %HEAP_SIZE% -s ocparse_generator generate -s init stop

    IF EXIST code_templates (
        DIR code_templates
        DEL /Q code_templates
    )

    ECHO !time! End  Test Data Generation

)
