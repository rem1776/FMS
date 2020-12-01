#***********************************************************************
#*                   GNU Lesser General Public License
#*
#* This file is part of the GFDL Flexible Modeling System (FMS).
#*
#* FMS is free software: you can redistribute it and/or modify it under
#* the terms of the GNU Lesser General Public License as published by
#* the Free Software Foundation, either version 3 of the License, or (at
#* your option) any later version.
#*
#* FMS is distributed in the hope that it will be useful, but WITHOUT
#* ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
#* FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
#* for more details.
#*
#* You should have received a copy of the GNU Lesser General Public
#* License along with FMS.  If not, see <http://www.gnu.org/licenses/>.
#***********************************************************************

# This is an automake file for the test_fms/time_interp directory of the FMS
# package.

# uramirez, Ed Hartnett, Ryan Mulhall

# Find the fms_mod.mod file.
AM_CPPFLAGS = -I${top_builddir}/.mod

# Link to the FMS library.
LDADD = ${top_builddir}/libFMS/libFMS.la

# Set env var for tests with input files if present
if ENABLE_INPUT_TESTS
TESTS_ENVIRONMENT = skip_input_tests=""
else
TESTS_ENVIRONMENT = skip_input_tests="skip"
endif 

# Build these test programs.
check_PROGRAMS = test_time_interp test_time_interp_external

# These are the sources for the tests.
test_time_interp_SOURCES = test_time_interp.F90
test_time_interp_external_SOURCES = test_time_interp_external.F90

# Run the test programs.
TESTS = test_time_interp2.sh test_time_interp_external.sh

# These files will also be included in the distribution.
if ENABLE_INPUT_TESTS
EXTRA_DIST = INPUT/aerosol.climatology.nc input_base.nml test_time_interp2.sh test_time_interp_external.sh
else
EXTRA_DIST = input_base.nml test_time_interp2.sh test_time_interp_external.sh
endif

# Clean up
CLEANFILES = input.nml *.out* INPUT/*.nc
