@ECHO off
rem ----------------------------------------------------------------------------
rem
rem gen_tests_and_run.bat: opencypher - generate and run test data.
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

> gen_tests_and_run.log (

    SETLOCAL enableDelayedExpansion
    ECHO %time% Start Test Data Generation and Run

    REM Setting ocparse options ...............................................
    REM true: compacted / false: detailed.
    SET GENERATE_COMPACTED=true
    SET GENERATE_CT=true
    SET GENERATE_EUNIT=false
    SET GENERATE_PERFORMANCE=true
    SET GENERATE_RELIABILITY=true
    SET HEAP_SIZE=+hms 100663296
    SET LOGGING=false
    SET MAX_BASIC_RULE=100
    CALL test\gen_tests

    ECHO %time% Start EUnit Tests
    SET SOURCEFILES_OLD=SOURCEFILES
    SET SOURCEFILES=
    CALL rebar3 eunit

    SET SOURCEFILES=SOURCEFILES_OLD
    ECHO %time% Start Common Tests
    CALL rebar3 ct

    ECHO %time% Start Coverage Analysis
    CALL rebar3 cover

    ECHO %time% Start Dialyzer
    CALL rebar3 dialyzer

    ECHO %time% End   Test Data Generation and Run

)
