#!/bin/sh

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

# This is part of the GFDL FMS package. This is a shell script to
# execute tests in the test_fms/field_manager directory.

# Ed Hartnett 11/29/19

# Set common test settings.
. ../test_common.sh

# Copy files for test.
cp $top_srcdir/test_fms/interpolator/input_base.nml input.nml
cp $top_srcdir/test_fms/interpolator/diag_table_base diag_table

if test "$skip_input_tests" != "skip" ; then
  cp -ruT $top_srcdir/test_fms/interpolator/INPUT
fi
# Test is skipped in bats file.
run_test test_interpolator 2 $skip_input_tests 


