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

# Set common test settings.
. ../test-lib.sh

if [ -z "${parser_skip}" ]; then
# create and enter directory for in/output files
output_dir

cat <<_EOF > diag_table.yaml
title: test_diag_simple
base_date: 2 1 1 0 0 0
diag_files:
- file_name: test_diag_simple
  time_units: hours
  unlimdim: time
  freq: 1 hours
  varlist:
  - module: ocn_mod
    var_name: var3
    reduction: average
    kind: r8
_EOF

printf "&diag_manager_nml\n  use_modern_diag=.true.\n/\n" | cat > input.nml
printf "&test_register_axis_nml\n  use_domain_for_vertical_axis = .false.\n/\n" | cat >> input.nml

# remove any existing files that would result in false passes during checks
test_expect_success "modern diag simple 3d field test" '
  mpirun -n 4 ../test_diag_simple
'
sed -i 's/use_domain_for_vertical_axis = .false./use_domain_for_vertical_axis = .true./' input.nml
test_expect_failure "check for errors when more than 2 domain axes registered" '
  mpirun -n 4 ../test_diag_simple
'
fi

test_done
