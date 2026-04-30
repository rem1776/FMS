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
!> Simple test that registers a single 3d variable by default, and checks for
!! errors when trying to register more than 2 axes with a 2D domain.
program test_register_axis
  use fms_mod
  use diag_manager_mod
  use mpp_mod
  use mpp_domains_mod
  use time_manager_mod
  use platform_mod

  implicit none

  real(r8_kind), allocatable :: cdata(:,:,:), missing_value
  integer :: nx, ny, nz, layout(2), io_layout(2), nhalox, nhaloy, ntimes
  integer :: id_x, id_y, id_z, id_var3, i, io_status
  integer :: isc, iec, jsc, jec, isd, ied, jsd, jed
  type(time_type) :: Time, Time_step
  type(domain2d) :: Domain
  logical :: used
  logical :: use_domain_for_vertical_axis = .false.
  
  namelist / test_register_axis_nml / use_domain_for_vertical_axis
   
  call fms_init
  call set_calendar_type(JULIAN)
  call diag_manager_init

  read (input_nml_file, test_register_axis_nml, iostat=io_status)
  if (io_status > 0) call mpp_error(FATAL,'=>test_register_axis: Error reading input.nml')

  Time = set_date(2,1,1,0,0,0)
  Time_step = set_time (3600,0) !< 1 hour
  nx = 96
  ny = 96
  nz = 5
  layout = (/1, mpp_npes()/)
  io_layout = (/1, 1/)
  nhalox = 2
  nhaloy = 2
  ntimes = 3

  !< Create a lat/lon domain
  call mpp_define_domains( (/1,nx,1,ny/), layout, Domain, name='2D domain', symmetry=.true., &
    xhalo=nhalox, yhalo=nhaloy)
  call mpp_define_io_domain(Domain, io_layout)
  call mpp_get_compute_domain(Domain, isc, iec, jsc, jec)
  call mpp_get_data_domain(Domain, isd, ied, jsd, jed)
  allocate(cdata(isc:iec, jsc:jec, nz))

  !< Register x/y axes
  id_x  = diag_axis_init('x',  real((/ (i, i = 1,nx) /), kind=r8_kind),  'point_E', 'x', long_name='point_E', &
    Domain2=Domain)
  id_y  = diag_axis_init('y',  real((/ (i, i = 1,ny) /), kind=r8_kind),  'point_N', 'y', long_name='point_N', &
    Domain2=Domain)

  if ( use_domain_for_vertical_axis ) then
    !> this should result in an error from too many axis for a 2d domain 
    id_z  = diag_axis_init('z',  real((/ (i, i = 1,nz) /), kind=r8_kind),  'point_Z', 'z', long_name='point_Z', &
      Domain2=Domain)
  else
    id_z  = diag_axis_init('z',  real((/ (i, i = 1,nz) /), kind=r8_kind),  'point_Z', 'z', long_name='point_Z')
  endif
   

  missing_value = -666._r8_kind

  !< Register the fields
  id_var3 = register_diag_field  ('ocn_mod', 'var3', (/id_x, id_y, id_z/), Time, 'Var3d', &
    'mullions', missing_value = missing_value)

  call diag_manager_set_time_end(set_date(2,1,1,3,0,0))

  do i = 1, ntimes
    Time = Time + Time_step
    cdata = i

    used = send_data(id_var3, cdata, Time)
    call diag_send_complete(Time_step)
  enddo

  call diag_manager_end(Time)

  call fms_end

end program
