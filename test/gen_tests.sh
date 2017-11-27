#!/bin/bash

exec > >(tee -i gen_tests.log)
sleep .1

# ----------------------------------------------------------------------------
#
# gen_tests.sh: opencypher - generating test data.
#
# Copyright (c) 2017 Walter Weinmann.  All Rights Reserved.
#
# This file is provided to you under the Apache License,
# Version 2.0 (the "License"); you may not use this file
# except in compliance with the License.  You may obtain
# a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
#
# ----------------------------------------------------------------------------

timestamp() {
  date +"%T"
}

echo "$(timestamp) Start Test Data Generation"

if ls _build/test/lib/ocparse/test/performance_*.* 1> /dev/null 2>&1; then
    rm _build/test/lib/ocparse/test/performance_*.*
fi
if ls _build/test/lib/ocparse/test/reliability_*.* 1> /dev/null 2>&1; then
    rm _build/test/lib/ocparse/test/reliability_*.*
fi
if ls test/performance_*.* 1> /dev/null 2>&1; then
    rm test/performance_*.*
fi
if ls test/reliability_*.* 1> /dev/null 2>&1; then
    rm test/reliability_*.*
fi

rebar3 as test compile

# Setting ocparse options ...............................................
if [ "$GENERATE_COMPACTED" == "" ]; then
    # true: compacted / false: detailed.
    export GENERATE_COMPACTED="true"
    export GENERATE_CT="true"
    export GENERATE_EUNIT="false"
    export GENERATE_PERFORMANCE="true"
    export GENERATE_RELIABILITY="true"
    export HEAP_SIZE="+hms 100663296"
    export LOGGING="false"
    export MAX_BASIC_RULE=100
fi

# Starting test data generator ...........................................
erl -noshell -pa _build/test/lib/ocparse/test $HEAP_SIZE -s ocparse_generator generate -s init stop

if [ -f "code_templates" ]; then
    ls -l code_templates
    rm code_templates
fi

echo "$(timestamp) End  Test Data Generation"

exit 0
