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

  use fms2_io_mod, only: fms2_io_init, open_file, close_file, FmsNetcdfFile_t
  use fms2_io_mod, only: register_axis, register_field, write_data, register_variable_attribute
  use mpp_mod, only: mpp_init, mpp_exit, mpp_npes, mpp_get_current_pelist
  use fms2_io_mod, only: unlimited

  implicit none

  integer, allocatable :: pes(:)
  integer, parameter :: grid_nx = 288, grid_ny = 180

  ! Initialize MPI and FMS2 I/O
  call mpp_init()
  call fms2_io_init()

  ! Allocate PE list
  allocate(pes(mpp_npes()))
  call mpp_get_current_pelist(pes)

  ! Create required input files
  call create_grid_spec()
  call create_atmos_mosaic()
  call create_atmos_hgrid()
  call create_land_mosaic()
  call create_land_hgrids()
  call create_sst_ice_clim()

  call mpp_exit()

contains

  subroutine create_grid_spec()
    type(FmsNetcdfFile_t) :: fileobj
    character(len=6), dimension(1) :: str_arr 

    if (open_file(fileobj, 'INPUT/grid_spec.nc', 'overwrite', pelist=pes)) then
      call register_axis(fileobj, 'str', 255)
      call register_axis(fileobj, 'nfile_aXo', 1)
      call register_axis(fileobj, 'nfile_aXl', 1)
      call register_axis(fileobj, 'nfile_lXo', 1)

      call register_field(fileobj, 'atm_mosaic_dir', 'char', (/'str'/))
      call register_field(fileobj, 'atm_mosaic_file', 'char', (/'str'/))
      call register_field(fileobj, 'atm_mosaic', 'char', (/'str'/))
      call register_field(fileobj, 'lnd_mosaic_dir', 'char', (/'str'/))
      call register_field(fileobj, 'lnd_mosaic_file', 'char', (/'str'/))
      call register_field(fileobj, 'lnd_mosaic', 'char', (/'str'/))
      call register_field(fileobj, 'ocn_mosaic_dir', 'char', (/'str'/))
      call register_field(fileobj, 'ocn_mosaic_file', 'char', (/'str'/))
      call register_field(fileobj, 'ocn_mosaic', 'char', (/'str'/))
      call register_field(fileobj, 'ocn_topog_dir', 'char', (/'str'/))
      call register_field(fileobj, 'ocn_topog_file', 'char', (/'str'/))
      call register_field(fileobj, 'aXo_file', 'char', (/'str', 'nfile_aXo'/))
      call register_field(fileobj, 'aXl_file', 'char', (/'str', 'nfile_aXl'/))
      call register_field(fileobj, 'lXo_file', 'char', (/'str', 'nfile_lXo'/))

      call write_data(fileobj, 'atm_mosaic_dir', './')
      call write_data(fileobj, 'atm_mosaic_file', 'atmos_mosaic.nc')
      call write_data(fileobj, 'atm_mosaic', 'atmos_mosaic')
      call write_data(fileobj, 'lnd_mosaic_dir', './')
      call write_data(fileobj, 'lnd_mosaic_file', 'unit_test_land_mosaic.nc')
      call write_data(fileobj, 'lnd_mosaic', 'unit_test_land_mosaic')
      call write_data(fileobj, 'ocn_mosaic_dir', './')
      call write_data(fileobj, 'ocn_mosaic_file', 'atmos_mosaic.nc')
      call write_data(fileobj, 'ocn_mosaic', 'ocean_mosaic')
      call write_data(fileobj, 'ocn_topog_dir', './')
      call write_data(fileobj, 'ocn_topog_file', 'topog.nc')
      str_arr(1) = 'aXo.nc'
      call write_data(fileobj, 'aXo_file', str_arr)
      str_arr(1) = 'aXl.nc'
      call write_data(fileobj, 'aXl_file', str_arr)
      str_arr(1) = 'lXo.nc'
      call write_data(fileobj, 'lXo_file', str_arr)

      call close_file(fileobj)
    end if
  end subroutine create_grid_spec

  subroutine create_atmos_mosaic()
    type(FmsNetcdfFile_t) :: fileobj
    character(len=38), dimension(1) :: gridfiles, gridtiles

    if (open_file(fileobj, 'INPUT/atmos_mosaic.nc', 'overwrite', pelist=pes)) then
      call register_axis(fileobj, 'ntiles', 1)
      call register_axis(fileobj, 'string', 50)

      call register_field(fileobj, 'mosaic', 'char', (/'string'/))
      call register_field(fileobj, 'gridlocation', 'char', (/'string'/))
      call register_field(fileobj, 'gridfiles', 'char', dimensions=(/'string', 'ntiles'/))
      call register_field(fileobj, 'gridtiles', 'char', dimensions=(/'string', 'ntiles'/))

      call write_data(fileobj, 'mosaic', 'atmos_mosaic')
      call write_data(fileobj, 'gridlocation', './')
      gridfiles(1) = 'atmos_hgrid.nc'
      gridtiles(1) = 'atmos'
      call write_data(fileobj, 'gridfiles', gridfiles) 
      call write_data(fileobj, 'gridtiles', gridtiles) 

      call close_file(fileobj)
    end if
  end subroutine create_atmos_mosaic

  subroutine create_atmos_hgrid()
    type(FmsNetcdfFile_t) :: fileobj
    real, allocatable :: x(:,:), y(:,:)
    integer :: i, j

    allocate(x(grid_nx+1, grid_ny+1))
    allocate(y(grid_nx+1, grid_ny+1))

    ! Create simple lat-lon grid
    do i = 1, grid_nx+1
      do j = 1, grid_ny+1
        x(i,j) = real(i-1) * 360.0 / grid_nx
        y(i,j) = real(j-1) * 180.0 / grid_ny - 90.0
      end do
    end do

    if (open_file(fileobj, 'INPUT/atmos_hgrid.nc', 'overwrite', pelist=pes)) then
      call register_axis(fileobj, 'nx', grid_nx)
      call register_axis(fileobj, 'ny', grid_ny)
      call register_axis(fileobj, 'nxp', grid_nx+1)
      call register_axis(fileobj, 'nyp', grid_ny+1)

      call register_field(fileobj, 'x', 'double', (/'nxp', 'nyp'/))
      call register_field(fileobj, 'y', 'double', (/'nxp', 'nyp'/))
      call register_field(fileobj, 'area', 'double', (/'nx', 'ny'/))

      call write_data(fileobj, 'x', x)
      call write_data(fileobj, 'y', y)

      call close_file(fileobj)
    end if

    deallocate(x, y)
  end subroutine create_atmos_hgrid

  subroutine create_land_mosaic()
    type(FmsNetcdfFile_t) :: fileobj
    character(len=255), dimension(6) :: gridfiles, gridtiles

    gridfiles = (/'unit_test_land_hgrid.tile1.nc', &
                  'unit_test_land_hgrid.tile2.nc', &
                  'unit_test_land_hgrid.tile3.nc', &
                  'unit_test_land_hgrid.tile4.nc', &
                  'unit_test_land_hgrid.tile5.nc', &
                  'unit_test_land_hgrid.tile6.nc'/)
    gridtiles = (/'tile1', 'tile2', 'tile3', 'tile4', 'tile5', 'tile6'/)

    if (open_file(fileobj, 'INPUT/unit_test_land_mosaic.nc', 'overwrite', pelist=pes)) then
      call register_axis(fileobj, 'ntiles', 6)
      call register_axis(fileobj, 'string', 255)

      call register_field(fileobj, 'mosaic', 'char', (/'string'/))
      call register_field(fileobj, 'gridfiles', 'char', (/'string', 'ntiles'/))
      call register_field(fileobj, 'gridtiles', 'char', (/'string', 'ntiles'/))

      call write_data(fileobj, 'mosaic', 'land_mosaic')
      call write_data(fileobj, 'gridfiles', gridfiles)
      call write_data(fileobj, 'gridtiles', gridtiles)

      call close_file(fileobj)
    end if
  end subroutine create_land_mosaic

  subroutine create_land_hgrids()
    type(FmsNetcdfFile_t) :: fileobj
    real, allocatable :: x(:,:), y(:,:)
    character(len=35) :: filename
    integer :: tile, i, j
    integer :: land_nx, land_ny

    ! Create single parent tile file
    land_nx = 180
    land_ny = 180 
    allocate(x(land_nx+1, land_ny+1))
    allocate(y(land_nx+1, land_ny+1))

    do i = 1, land_nx+1
      do j = 1, land_ny+1
        x(i,j) = real(i-1) * 360.0 / land_nx
        y(i,j) = real(j-1) * 180.0 / land_ny - 90.0
      end do
    end do

    if (open_file(fileobj, 'INPUT/unit_test_land_hgrid.nc', 'overwrite', pelist=pes)) then
      call register_axis(fileobj, 'nx', land_nx)
      call register_axis(fileobj, 'ny', land_ny)
      call register_axis(fileobj, 'nxp', land_nx+1)
      call register_axis(fileobj, 'nyp', land_ny+1)

      call register_field(fileobj, 'x', 'double', (/'nxp', 'nyp'/))
      call register_field(fileobj, 'y', 'double', (/'nxp', 'nyp'/))

      call write_data(fileobj, 'x', x)
      call write_data(fileobj, 'y', y)

      call close_file(fileobj)
    end if

    ! Create tile-specific files
    do tile = 1, 6
      write(filename, '(A,I1,A)') 'INPUT/unit_test_land_hgrid.tile', tile, '.nc'

      if (open_file(fileobj, trim(filename), 'overwrite', pelist=pes)) then
        call register_axis(fileobj, 'nx', land_nx)
        call register_axis(fileobj, 'ny', land_ny)
        call register_axis(fileobj, 'nxp', land_nx+1)
        call register_axis(fileobj, 'nyp', land_ny+1)

        call register_field(fileobj, 'x', 'double', (/'nxp', 'nyp'/))
        call register_field(fileobj, 'y', 'double', (/'nxp', 'nyp'/))
        call register_field(fileobj, 'area', 'double', (/'nx', 'ny'/))

        call write_data(fileobj, 'x', x)
        call write_data(fileobj, 'y', y)

        call close_file(fileobj)
      end if
    end do

    deallocate(x, y)
  end subroutine create_land_hgrids

  subroutine create_sst_ice_clim()
    type(FmsNetcdfFile_t) :: fileobj
    real, allocatable :: sst(:,:,:), sic(:,:,:), sit(:,:,:)
    double precision, allocatable :: lon(:), lat(:), time(:), lon_ice(:), lat_ice(:)
    integer :: i, j, t

    allocate(lon(360))
    allocate(lat(180))
    allocate(lon_ice(180))
    allocate(lat_ice(91))
    allocate(time(12))
    allocate(sst(360, 180, 12))
    allocate(sic(360, 180, 12))
    allocate(sit(180, 91, 12))

    ! Create coordinates
    do i = 1, 360
      lon(i) = real(i-1) - 180.0
    end do
    do i = 1, 180
      lat(i) = real(i-1) * 2.0 - 90.0
    end do
    do i = 1, 180
      lon_ice(i) = real(i-1) * 2.0 - 180.0
    end do
    do i = 1, 91
      lat_ice(i) = real(i-1) * 2.0 - 90.0
    end do
    do t = 1, 12
      time(t) = real(t-1) * 30.0
    end do

    ! Create simple data (constants)
    sst = 20.0
    sic = 0.0
    sit = 0.0

    if (open_file(fileobj, 'INPUT/sst_ice_clim.nc', 'overwrite', pelist=pes)) then
      call register_axis(fileobj, 'LONGITUDE', 360)
      call register_axis(fileobj, 'LATITUDE', 180)
      call register_axis(fileobj, 'LON_ICE', 180)
      call register_axis(fileobj, 'LAT', 91)
      call register_axis(fileobj, 'TIME', unlimited)

      call register_field(fileobj, 'LONGITUDE', 'double', (/'LONGITUDE'/))
      call register_field(fileobj, 'LATITUDE', 'double', (/'LATITUDE'/))
      call register_field(fileobj, 'LON_ICE', 'double', (/'LON_ICE'/))
      call register_field(fileobj, 'LAT', 'double', (/'LAT'/))
      call register_field(fileobj, 'TIME', 'double', (/'TIME'/))
      call register_field(fileobj, 'SST', 'float', (/'LONGITUDE', 'LATITUDE ', 'TIME      '/))
      call register_field(fileobj, 'SIC', 'float', (/'LONGITUDE', 'LATITUDE ', 'TIME      '/))
      call register_field(fileobj, 'SIT', 'float', (/'LON_ICE   ', 'LAT       ', 'TIME      '/))

      call register_variable_attribute(fileobj, 'LONGITUDE', 'units', 'degrees_east', str_len=12)
      call register_variable_attribute(fileobj, 'LATITUDE', 'units', 'degrees_north', str_len=13)
      call register_variable_attribute(fileobj, 'LON_ICE', 'units', 'degrees_east', str_len=12)
      call register_variable_attribute(fileobj, 'LAT', 'units', 'degrees_north', str_len=13)
      call register_variable_attribute(fileobj, 'TIME', 'units', 'days since 0002-01-01 00:00:00', str_len=30)
      call register_variable_attribute(fileobj, 'TIME', 'calendar', 'noleap', str_len=6)
      call register_variable_attribute(fileobj, 'SST', 'long_name', 'Sea surface temperature', str_len=23)
      call register_variable_attribute(fileobj, 'SST', 'units', 'K', str_len=1)
      call register_variable_attribute(fileobj, 'SIC', 'long_name', 'Sea-ice concentration', str_len=21)
      call register_variable_attribute(fileobj, 'SIC', 'units', '%', str_len=1)
      call register_variable_attribute(fileobj, 'SIT', 'long_name', 'sea ice thickness', str_len=17)
      call register_variable_attribute(fileobj, 'SIT', 'units', 'm', str_len=1)

      call write_data(fileobj, 'LONGITUDE', lon)
      call write_data(fileobj, 'LATITUDE', lat)
      call write_data(fileobj, 'LON_ICE', lon_ice)
      call write_data(fileobj, 'LAT', lat_ice)
      call write_data(fileobj, 'TIME', time)
      call write_data(fileobj, 'SST', sst)
      call write_data(fileobj, 'SIC', sic)
      call write_data(fileobj, 'SIT', sit)

      call close_file(fileobj)
    end if

    deallocate(lon, lat, lon_ice, lat_ice, time, sst, sic, sit)
  end subroutine create_sst_ice_clim


end program test_data_override_generate_inputs
