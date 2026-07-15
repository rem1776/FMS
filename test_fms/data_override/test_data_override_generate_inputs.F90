!***********************************************************************
!*                             Apache License 2.0
!*
!* This file is part of the GFDL Flexible Modeling System (FMS).
!*
!* Licensed under the Apache License, Version 2.0 (the "License");
!* you may not use this file except in compliance with the License.
!* You may obtain a copy of the License at
!*
!*     http://www.apache.org/licenses/LICENSE-2.0
!*
!* FMS is distributed in the hope that it will be useful, but WITHOUT
!* WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied;
!* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
!* PARTICULAR PURPOSE. See the License for the specific language
!* governing permissions and limitations under the License.
!***********************************************************************

!! Program to generate minimal input files for test_data_override tests
program test_data_override_generate_inputs

  use fms2_io_mod, only: fms2_io_init
  use mpp_mod, only: mpp_init, mpp_exit
  use create_test_input_mod, only: create_data_override_test_input

  implicit none

  integer, parameter :: atmos_nx = 288, atmos_ny = 180
  integer, parameter :: land_nx = 180, land_ny = 180
  integer, parameter :: data_nx = 360, data_ny = 180
  integer, parameter :: ice_nx = 180, ice_ny = 91

  ! Initialize MPI and FMS2 I/O
  call mpp_init()
  call fms2_io_init()

  call create_data_override_test_input(atmos_nx, atmos_ny, land_nx, land_ny, data_nx, data_ny, ice_nx, ice_ny)

  call mpp_exit()
end program test_data_override_generate_inputs
