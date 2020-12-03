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

# This is an automake file for the test_fms/mosaic directory of the
# FMS package.

# uramirez, Ed Hartnett

# Find the needed mod and include files.
AM_CPPFLAGS = -I${top_srcdir}/include -I${top_builddir}/.mod

# Link to the FMS library.
LDADD = ${top_builddir}/libFMS/libFMS.la

# Set env var for tests with input files if present
if ENABLE_INPUT_TESTS
TESTS_ENVIRONMENT = skip_input_tests=""
else
TESTS_ENVIRONMENT = skip_input_tests="skip"
endif 

# Build this test program.
check_PROGRAMS = test_mosaic

# This is the source code for the test.
test_mosaic_SOURCES = test_mosaic.F90

# Run the test program.
TESTS = test_mosaic2.sh

# These files will also be included in the distribution.
if ENABLE_INPUT_TESTS
EXTRA_DIST = test_mosaic2.sh input.nml INPUT/*
else
EXTRA_DIST = test_mosaic2.sh input.nml
endif
