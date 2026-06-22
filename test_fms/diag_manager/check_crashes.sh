#!/bin/sh

#***********************************************************************
#*                             Apache License 2.0
#*
#* This file is part of the GFDL Flexible Modeling System (FMS).
#*
#* Licensed under the Apache License, Version 2.0 (the "License");
#* you may not use this file except in compliance with the License.
#* You may obtain a copy of the License at
#*
#*     http://www.apache.org/licenses/LICENSE-2.0
#*
#* FMS is distributed in the hope that it will be useful, but WITHOUT
#* WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied;
#* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
#* PARTICULAR PURPOSE. See the License for the specific language
#* governing permissions and limitations under the License.
#***********************************************************************

# This is part of the GFDL FMS package. This is a shell script to
# execute tests in the test_fms/data_override directory.

# Set common test settings.

printf "&check_crashes_nml \n checking_crashes = .true. \n/" | cat > input.nml
sed '/tile/d' diag_table.yaml_base > diag_table.yaml
test_expect_error "Missing tile when using the 'index' grid type" '
  mpirun -n 1 ../test_diag_yaml
'

sed '/new_file_freq: 6 hours/new_file_freq: 6/g' diag_table.yaml_base > diag_table.yaml
test_expect_error "Missing new_file_freq_units when using new_file_freq_units" '
  mpirun -n 1 ../test_diag_yaml
'

sed 's/new_file_freq: 6 hours/new_file_freq: 6 mullions/g' diag_table.yaml_base > diag_table.yaml
test_expect_error "new_file_freq_units is not valid" '
  mpirun -n 1 ../test_diag_yaml
'

sed '/file_duration: 12 hours/file_duration: 12/g' diag_table.yaml_base > diag_table.yaml
test_expect_error "Missing file_duration_units when using file_duration" '
  mpirun -n 1 ../test_diag_yaml
'

sed 's/file_duration: 12 hours/file_duration: 12 mullions/g' diag_table.yaml_base > diag_table.yaml
test_expect_error "file_duration_units is not valid" '
  mpirun -n 1 ../test_diag_yaml
'

sed 's/freq: 6 hours/freq: 6 mullions/g' diag_table.yaml_base > diag_table.yaml
test_expect_error "freq units is not valid" '
  mpirun -n 1 ../test_diag_yaml
'

sed 's/freq: 6 hours/freq: -6 hours/g' diag_table.yaml_base > diag_table.yaml
test_expect_error "freq is less than -1" '
  mpirun -n 1 ../test_diag_yaml
'

sed 's/kind: r4/kind: mullions/g' diag_table.yaml_base > diag_table.yaml
test_expect_error "kind is not valid" '
  mpirun -n 1 ../test_diag_yaml
'

sed 's/reduction: average/reduction: mullions/g' diag_table.yaml_base > diag_table.yaml
test_expect_error "reduction is not valid" '
  mpirun -n 1 ../test_diag_yaml
'

sed 's/reduction: average/reduction: diurnal0/g' diag_table.yaml_base > diag_table.yaml
test_expect_error "diurnal samples is less than 0" '
  mpirun -n 1 ../test_diag_yaml
'

sed 's/reduction: average/reduction: diurnal99r/g' diag_table.yaml_base > diag_table.yaml
test_expect_error "diurnal samples is not an integer" '
  mpirun -n 1 ../test_diag_yaml
'

sed 's/reduction: average/reduction: pow0/g' diag_table.yaml_base > diag_table.yaml
test_expect_error "power value is less than 0" '
  mpirun -n 1 ../test_diag_yaml
'

sed 's/reduction: average/reduction: pow99r/g' diag_table.yaml_base > diag_table.yaml
test_expect_error "power value is not an integer" '
  mpirun -n 1 ../test_diag_yaml
'

sed 's/grid_type: latlon/grid_type: ice_cream/g' diag_table.yaml_base > diag_table.yaml
test_expect_error "the sub_region grid_type is not valid" '
  mpirun -n 1 ../test_diag_yaml
'
