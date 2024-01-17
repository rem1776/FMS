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

# Set common test settings.
. ../test-lib.sh

if [ -z "${skipflag}" ]; then
# create and enter directory for in/output files
output_dir


  cat <<_EOF > diag_table.yaml
title: test_diag_manager
base_date: 2 1 1 0 0 0

diag_files:
- file_name: static_file
  freq: -1
  time_units: hours
  unlimdim: time
  varlist:
  - module: atm_mod
    var_name: var7
    reduction: none
    kind: r4
  global_meta:
  - is_important: False
    has_important: True
- file_name: file1
  freq: 6 hours
  time_units: hours
  unlimdim: time
  varlist:
  - module: ocn_mod
    var_name: var1
    reduction: average
    kind: r4
  - module: ocn_mod
    var_name: var2
    output_name: potato
    reduction: average
    kind: r4
- file_name: file2
  freq: 6 hours
  time_units: hours
  unlimdim: time
  varlist:
  - module: atm_mod
    var_name: var3
    reduction: average
    kind: r4
  - module: atm_mod
    var_name: var4
    output_name: i_on_a_sphere
    reduction: average
    kind: r8
  - module: atm_mod
    var_name: var6
    reduction: average
    kind: r8
  - module: atm_mod
    var_name: var4
    output_name: var4_bounded
    reduction: average
    kind: r8
    zbounds: 2.0 3.0
- file_name: file3
  freq: 6 hours
  time_units: hours
  unlimdim: time
  varlist:
  - module: lnd_mod
    var_name: var5
    reduction: average
    kind: r4
  - module: atm_mod
    var_name: var7
    reduction: average
    kind: r4
- file_name: file4
  freq: 6 hours
  time_units: hours
  unlimdim: time
  varlist:
  - module: lnd_mod
    var_name: var1
    reduction: average
    kind: r4
- file_name: file5
  freq: 6 hours
  time_units: hours
  unlimdim: time
  varlist:
  - module: atm_mod
    var_name: var4
    reduction: average
    kind: r4
  sub_region:
  - grid_type: index
    tile: 1
    corner1: 10, 15
    corner2: 20, 15
    corner3: 10, 25
    corner4: 20, 25
- file_name: file6%4yr%2mo%2dy%2hr
  freq: 6 hours
  time_units: hours
  unlimdim: time
  new_file_freq: 6 hours
  start_time: 2 1 1 0 0 0
  file_duration: 12 hours
  varlist:
  - module: ocn_mod
    var_name: var1
    reduction: average
    kind: r4
- file_name: file7
  freq: 6 hours
  time_units: hours
  unlimdim: time
  varlist:
  - module: ocn_mod
    var_name: var1
    reduction: none
    kind: r4
    attributes:
    - GFDL_name: var_var
- file_name: file8%4yr%2mo%2dy%2hr%2min
  freq: 1 hours,1 hours,1 hours
  time_units: hours
  unlimdim: time
  new_file_freq: 6 hours, 3 hours, 1 hours
  start_time: 2 1 1 0 0 0
  file_duration: 12 hours, 3 hours, 9 hours
  varlist:
  - module: ocn_mod
    var_name: var1
    reduction: average
    kind: r4
- file_name: file9%4yr%2mo%2dy%2hr%2min
  filename_time: begin
  freq: 1 hours,1 hours,1 hours
  time_units: hours
  unlimdim: time
  new_file_freq: 6 hours, 3 hours, 1 hours
  start_time: 2 1 1 0 0 0
  file_duration: 12 hours, 3 hours, 9 hours
  varlist:
  - module: ocn_mod
    var_name: var1
    reduction: average
    kind: r4
- file_name: file10_diurnal
  freq: 1 days
  time_units: hours
  unlimdim: time
  varlist:
  - module: ocn_mod
    var_name: var1
    reduction: diurnal12
    kind: r4
_EOF

cat <<_EOF > diag_out_ref.yaml
---
title: test_diag_manager
base_date: 2 1 1 0 0 0
diag_files:
- file_name: static_file
  freq: -1 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999
  freq_units: days null null null null null null null null null null null
  time_units: hours
  unlimdim: time
  new_file_freq: -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999
  new_file_freq_units: null null null null null null null null null null null null
  start_time:
  file_duration: -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999
  file_duration_units: null null null null null null null null null null null null
  varlist:
  - module: atm_mod
    var_name: var7
    reduction: none
    kind: r4
    output_name:
    long_name:
    units:
    zbounds: -999.0000, -999.0000
    n_diurnal: 0
    pow_value: 0
    attributes:
  sub_region:
  - grid_type: null
    tile: -999
    corner1:
    corner2:
    corner3:
    corner4:
  global_meta:
  - is_important: False
    has_important: True
- file_name: file1
  freq: 6 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999
  freq_units: hours null null null null null null null null null null null
  time_units: hours
  unlimdim: time
  new_file_freq: -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999
  new_file_freq_units: null null null null null null null null null null null null
  start_time:
  file_duration: -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999
  file_duration_units: null null null null null null null null null null null null
  varlist:
  - module: ocn_mod
    var_name: var1
    reduction: average
    kind: r4
    output_name:
    long_name:
    units:
    zbounds: -999.0000, -999.0000
    n_diurnal: 0
    pow_value: 0
    attributes:
  - module: ocn_mod
    var_name: var2
    reduction: average
    kind: r4
    output_name: potato
    long_name:
    units:
    zbounds: -999.0000, -999.0000
    n_diurnal: 0
    pow_value: 0
    attributes:
  sub_region:
  - grid_type: null
    tile: -999
    corner1:
    corner2:
    corner3:
    corner4:
  global_meta:
  - {}
- file_name: file2
  freq: 6 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999
  freq_units: hours null null null null null null null null null null null
  time_units: hours
  unlimdim: time
  new_file_freq: -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999
  new_file_freq_units: null null null null null null null null null null null null
  start_time:
  file_duration: -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999
  file_duration_units: null null null null null null null null null null null null
  varlist:
  - module: atm_mod
    var_name: var3
    reduction: average
    kind: r4
    output_name:
    long_name:
    units:
    zbounds: -999.0000, -999.0000
    n_diurnal: 0
    pow_value: 0
    attributes:
  - module: atm_mod
    var_name: var4
    reduction: average
    kind: r8
    output_name: i_on_a_sphere
    long_name:
    units:
    zbounds: -999.0000, -999.0000
    n_diurnal: 0
    pow_value: 0
    attributes:
  - module: atm_mod
    var_name: var6
    reduction: average
    kind: r8
    output_name:
    long_name:
    units:
    zbounds: -999.0000, -999.0000
    n_diurnal: 0
    pow_value: 0
    attributes:
  - module: atm_mod
    var_name: var4
    reduction: average
    kind: r8
    output_name: var4_bounded
    long_name:
    units:
    zbounds: 2.000000, 3.000000
    n_diurnal: 0
    pow_value: 0
    attributes:
  sub_region:
  - grid_type: null
    tile: -999
    corner1:
    corner2:
    corner3:
    corner4:
  global_meta:
  - {}
- file_name: file3
  freq: 6 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999
  freq_units: hours null null null null null null null null null null null
  time_units: hours
  unlimdim: time
  new_file_freq: -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999
  new_file_freq_units: null null null null null null null null null null null null
  start_time:
  file_duration: -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999
  file_duration_units: null null null null null null null null null null null null
  varlist:
  - module: lnd_mod
    var_name: var5
    reduction: average
    kind: r4
    output_name:
    long_name:
    units:
    zbounds: -999.0000, -999.0000
    n_diurnal: 0
    pow_value: 0
    attributes:
  - module: atm_mod
    var_name: var7
    reduction: average
    kind: r4
    output_name:
    long_name:
    units:
    zbounds: -999.0000, -999.0000
    n_diurnal: 0
    pow_value: 0
    attributes:
  sub_region:
  - grid_type: null
    tile: -999
    corner1:
    corner2:
    corner3:
    corner4:
  global_meta:
  - {}
- file_name: file4
  freq: 6 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999
  freq_units: hours null null null null null null null null null null null
  time_units: hours
  unlimdim: time
  new_file_freq: -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999
  new_file_freq_units: null null null null null null null null null null null null
  start_time:
  file_duration: -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999
  file_duration_units: null null null null null null null null null null null null
  varlist:
  - module: lnd_mod
    var_name: var1
    reduction: average
    kind: r4
    output_name:
    long_name:
    units:
    zbounds: -999.0000, -999.0000
    n_diurnal: 0
    pow_value: 0
    attributes:
  sub_region:
  - grid_type: null
    tile: -999
    corner1:
    corner2:
    corner3:
    corner4:
  global_meta:
  - {}
- file_name: file5
  freq: 6 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999
  freq_units: hours null null null null null null null null null null null
  time_units: hours
  unlimdim: time
  new_file_freq: -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999
  new_file_freq_units: null null null null null null null null null null null null
  start_time:
  file_duration: -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999
  file_duration_units: null null null null null null null null null null null null
  varlist:
  - module: atm_mod
    var_name: var4
    reduction: average
    kind: r4
    output_name:
    long_name:
    units:
    zbounds: -999.0000, -999.0000
    n_diurnal: 0
    pow_value: 0
    attributes:
  sub_region:
  - grid_type: index
    tile: 1
    corner1: 10, 15
    corner2: 20, 15
    corner3: 10, 25
    corner4: 20, 25
  global_meta:
  - {}
- file_name: file6%4yr%2mo%2dy%2hr
  freq: 6 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999
  freq_units: hours null null null null null null null null null null null
  time_units: hours
  unlimdim: time
  new_file_freq: 6 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999
  new_file_freq_units: hours null null null null null null null null null null null
  start_time: 2 1 1 0 0 0
  file_duration: 12 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999
  file_duration_units: hours null null null null null null null null null null null
  varlist:
  - module: ocn_mod
    var_name: var1
    reduction: average
    kind: r4
    output_name:
    long_name:
    units:
    zbounds: -999.0000, -999.0000
    n_diurnal: 0
    pow_value: 0
    attributes:
  sub_region:
  - grid_type: null
    tile: -999
    corner1:
    corner2:
    corner3:
    corner4:
  global_meta:
  - {}
- file_name: file7
  freq: 6 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999
  freq_units: hours null null null null null null null null null null null
  time_units: hours
  unlimdim: time
  new_file_freq: -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999
  new_file_freq_units: null null null null null null null null null null null null
  start_time:
  file_duration: -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999
  file_duration_units: null null null null null null null null null null null null
  varlist:
  - module: ocn_mod
    var_name: var1
    reduction: none
    kind: r4
    output_name:
    long_name:
    units:
    zbounds: -999.0000, -999.0000
    n_diurnal: 0
    pow_value: 0
    attributes: GFDL_name:var_var
  sub_region:
  - grid_type: null
    tile: -999
    corner1:
    corner2:
    corner3:
    corner4:
  global_meta:
  - {}
- file_name: file8%4yr%2mo%2dy%2hr%2min
  freq: 1 1 1 -999 -999 -999 -999 -999 -999 -999 -999 -999
  freq_units: hours hours hours null null null null null null null null null
  time_units: hours
  unlimdim: time
  new_file_freq: 6 3 1 -999 -999 -999 -999 -999 -999 -999 -999 -999
  new_file_freq_units: hours hours hours null null null null null null null null null
  start_time: 2 1 1 0 0 0
  file_duration: 12 3 9 -999 -999 -999 -999 -999 -999 -999 -999 -999
  file_duration_units: hours hours hours null null null null null null null null null
  varlist:
  - module: ocn_mod
    var_name: var1
    reduction: average
    kind: r4
    output_name:
    long_name:
    units:
    zbounds: -999.0000, -999.0000
    n_diurnal: 0
    pow_value: 0
    attributes:
  sub_region:
  - grid_type: null
    tile: -999
    corner1:
    corner2:
    corner3:
    corner4:
  global_meta:
  - {}
- file_name: file9%4yr%2mo%2dy%2hr%2min
  freq: 1 1 1 -999 -999 -999 -999 -999 -999 -999 -999 -999
  freq_units: hours hours hours null null null null null null null null null
  time_units: hours
  unlimdim: time
  new_file_freq: 6 3 1 -999 -999 -999 -999 -999 -999 -999 -999 -999
  new_file_freq_units: hours hours hours null null null null null null null null null
  start_time: 2 1 1 0 0 0
  file_duration: 12 3 9 -999 -999 -999 -999 -999 -999 -999 -999 -999
  file_duration_units: hours hours hours null null null null null null null null null
  varlist:
  - module: ocn_mod
    var_name: var1
    reduction: average
    kind: r4
    output_name:
    long_name:
    units:
    zbounds: -999.0000, -999.0000
    n_diurnal: 0
    pow_value: 0
    attributes:
  sub_region:
  - grid_type: null
    tile: -999
    corner1:
    corner2:
    corner3:
    corner4:
  global_meta:
  - {}
- file_name: file10_diurnal
  freq: 1 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999
  freq_units: days null null null null null null null null null null null
  time_units: hours
  unlimdim: time
  new_file_freq: -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999
  new_file_freq_units: null null null null null null null null null null null null
  start_time:
  file_duration: -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 -999
  file_duration_units: null null null null null null null null null null null null
  varlist:
  - module: ocn_mod
    var_name: var1
    reduction:
    kind: r4
    output_name:
    long_name:
    units:
    zbounds: -999.0000, -999.0000
    n_diurnal: 12
    pow_value: 0
    attributes:
  sub_region:
  - grid_type: null
    tile: -999
    corner1:
    corner2:
    corner3:
    corner4:
  global_meta:
  - {}
...
_EOF

# tests with no mask, no openmp
printf "&diag_manager_nml \n use_modern_diag=.true. \n /n/" | cat > input.nml
test_expect_success "Checking answers for the "avg" reduction method with halo output with real mask (test $my_test_count)" '
  mpirun -n 1 ../test_out_yaml
'
fi
test_done
