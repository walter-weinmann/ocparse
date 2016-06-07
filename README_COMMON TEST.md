ocparse and Common Test
=======================

### 1. Common Test Environment

All files related to **Common Test** can be found in the directory `ct`. The main components in directory ct are:

- the subdirectory `logs`, which contains all the result files relating to previously executed tests,
- test specification files - suffixed with `.spec` -, eg. `all.spec`,
- the script files `start_spec.bat` and `start_spec.sh` to run a selected test specification file,
- the script files `start_sel.bat` and `start_sel.sh` to run a selected test suite, test case groups or test cases,
- for each test suite, e.g. for module `ocparse_sql_select.erl`:
    - a directory containing test suite related data: `ocparse_sql_select_SUITE_DATA`,
    - the test suite module file, e.g. `ocparse_sql_select_SUITE.erl`,
    - the compiled test suite module file, e.g. `ocparse_sql_select_SUITE.beam`.

Furthermore, the following files have been adjusted:

- `.gitignore`: defining the test suite beam files and the test result files to be ignored,
- `.travis.yml`: adding a `rebar ct` command (`after success`),
- `rebar.config`: defining the **Common Test** directory and the associated log file directory. 

### 2. Running Common Test

**Common Test** can be executed from the command line with either the `ct_run` command or the `rebar ct` command. The execution of **Common Test** can also be included into Travis CI.

##### 2.1 Command `ct_run`

The execution of **Common Test** from the command line is supported by the following scripts, both of which have to be called from the directory `ocparse` :

###### `start_sel.bat` and `start_sel.sh`

The scripts allow you to select one of the following options for the execution of the **Common Test** 
 
- a whole test suite, or
- one or more test case groups of a test suite, or 
- one or more test cases of a test suite, or
- one or more test cases of a test suite and a contained test case group.

###### `start_spec.bat` and `start_spec.sh`

The scripts allow you to select a test specification file for the execution of the **Common Test**. If no test specification file is selected, **Common Test** runs with the default test specification file `all.spec`.
 
##### 2.2 Command `rebar ct`

The command `rebar help ct` shows the configuration options to run **Common Test** with rebar:
``` Erlang
=== rebar_ct:ct ===
Run common_test suites.

Valid rebar.config options:
  {ct_dir,"ct"}
  {ct_log_dir,"ct/logs"}
  {ct_extra_params,"-boot start_sasl -s myapp"}
  {ct_use_short_names,true}
  {ct_search_specs_from_test_dir,false}
Valid command line options:
  suites=Suite1,Suite2,...,SuiteN
      - run Suite1_SUITE, Suite2_SUITE, ..., SuiteN_SUITE
      in the test folder.
  groups=Group1,Group2,...,GroupN
      - run test groups Group1, Group2, ..., GroupN of specified suites.
  cases=Case1,Case2,...,CaseM
      - run test cases Case1, Case2, ..., CaseN of specified suites.
  case="mycase" - run individual test case Suite1_SUITE:mycase.
      This option is deprecated and remains for backward compability.
      It is recommended to use 'cases' instead.
```
Since the parameters `ct_dir` and `ct_log_dir` are already part of the rebar configuration, a simple `rebat ct` command runs all the test suites, which are contained in the directory `ct`. 

##### 2.3 Travis CI

**Common Test** is currently configured as a `after success` script in Travis CI. This has the disadvantage, that the build of **ocparse** is marked as successfull even if **Common Test** fails.  

### 3. Checking Common Test Results

The results of the **Common Test** execution can be checked in the following file:
 
 file:///.../ocparse/ct/logs/index.html.
