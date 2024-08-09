# Unit Testing

FMS includes a suite of MPI unit tests using the testing infrastructure included in the GNU autotools build system
in order to check the functionality of the library's modules.

It consists of programs in the test_fms/ directory, with shell scripts to handle directory set up and input files.
test_lib.sh.in and tap-driver.sh provide additional helper functions used in the scripts and manage output.

### Running the Tests

1. Configure with autotools
```
mkdir build # create a build directory in FMS
autoreconf -if ../configure.ac
../configure <configure options>
```

2. Build and run suite
```
make check
```
This will compile any code not already compiled and then proceed to run the test scripts.

### Debugging Output

Setting the environment variable TEST_VERBOSE will direct output to stdout as the test runs, while setting VERBOSE will only output on failure.
Logs are created for each test as well, with the name \<test script name\>.log in it's corresponding test_fms/ directory.

To run an individual test:
```
make check -C test_fms/<test directory> TESTS=<test script name>
```

SKIP_TESTS can be set to in order to skip specific tests in a script. It uses the script name and test number, and takes ranges as well:
```
SKIP_TESTS="test_name.4 test_name.[1-3]"
```

### Overriding MPI ranks

Each test is run with a default number of MPI ranks(ie. -n nranks) but these values can be overriden for any given test.

It should be noted that many unit tests are written with a specific PE count in mind, so tests may need additional
modification to run successfully with overriden task counts.

This is done through environment variables that are checked at run time. The variables should follow this format:
```
<TEST_NAME>_<TEST_NUMBER>_RANKS=t
```
Where TEST_NAME is the name of the script (without .sh) and TEST_NUMBER indicates the n-th test run within that script.
t can be any positive integer value, oversubscribing for larger counts than available will be done automatically.

### Configure Options

Some options that effect the test suite can be set by passing options to the ./configure script that creates the makefiles
for the build system. All configure options can be viewed via `./configure --help`:

-    `--enable-code-coverage` allows for compilation with flags for coverage information.
     If enabled a coverage report can be generated with `make check-code-coverage`
-    `--enable-test-input=/path/to/input` turns on test scripts that require input netcdf files (interpolator, xgrid, data_override).
     This option is mainly used internally and in automated testing since we do not host the input data publicly.
-    `--with-yaml` compile with yaml input and enable any associated tests
