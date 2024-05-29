!*                   GNU Lesser General Public License
!*
!* This file is part of the GFDL Flexible Modeling System (FMS).
!*
!* FMS is free software: you can redistribute it and/or modify it under
!* the terms of the GNU Lesser General Public License as published by
!* the Free Software Foundation, either version 3 of the License, or (at
!* your option) any later version.
!*
!* FMS is distributed in the hope that it will be useful, but WITHOUT
!* ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
!* FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
!* for more details.
!*
!* You should have received a copy of the GNU Lesser General Public
!* License along with FMS.  If not, see <http://www.gnu.org/licenses/>.
!***********************************************************************
!> @defgroup mosaic2_mod mosaic2_mod
!> @ingroup mosaic2
!> @brief This module is to provide access to c functions used within mosaic,
!! mosaic2, and horiz_interp.
!!
!> @author Ryan Mulhall 
!!

!> @addtogroup mosaic2_mod
!> @{
module grid_utils_mod

  !> for mosaic2
  public :: get_grid_area, get_grid_great_circle_area
  !> for horiz_interp
  public :: create_xgrid_2DX1D_order1, create_xgrid_1DX2D_order1, get_maxxgrid, create_xgrid_great_circle, &
            create_xgrid_2Dx2D_order1
!!#ifdef use_deprecated_io
  !> for mosaic
  public :: grad_c2l, calc_c2l_grid_info
!!#endif

  interface 
    !>
    subroutine get_grid_area(nlon, nlat, lon, lat, area) bind(C, name="get_grid_area")
      use iso_c_binding, only: c_int, c_double, c_ptr
      integer(c_int), value :: nlon
      integer(c_int), value :: nlat
      real(c_double) :: lon(:,:)
      real(c_double) :: lat(:,:)
      real(c_double) :: area(:,:)
    end subroutine

    !>
    subroutine get_grid_great_circle_area(nlon, nlat, lon, lat, area) bind(C, name="get_grid_great_circle_area")
      use iso_c_binding, only: c_int, c_double
      integer(c_int) :: nlon
      integer(c_int) :: nlat
      real(c_double) :: lon(:,:)
      real(c_double) :: lat(:,:)
      real(c_double) :: area(:,:)
    end subroutine

    !>
    subroutine grad_c2l(nlon, nlat, pin, dx, dy, area, edge_w, edge_e, edge_s, edge_n, &
                        en_n, en_e, vlon, vlat, grad_x, grad_y, on_west_edge, on_east_edge, &
                        on_south_edge, on_north_edge) bind(C, name="grad_c2l")
      use iso_c_binding, only: c_int, c_double
      integer(c_int) :: nlon(:)
      integer(c_int) :: nlat(:)
      real(c_double) :: pin(:)
      real(c_double) :: dx (:)
      real(c_double) :: dy(:)
      real(c_double) :: area(:)
      real(c_double) :: edge_w (:)
      real(c_double) :: edge_e(:)
      real(c_double) :: edge_s(:)
      real(c_double) :: edge_n(:)
      real(c_double) :: en_n (:)
      real(c_double) :: en_e(:)
      real(c_double) :: vlon(:)
      real(c_double) :: vlat(:)
      real(c_double) :: grad_x (:)
      real(c_double) :: grad_y (:)
      integer(c_int) :: on_west_edge (:)
      integer(c_int) :: on_east_edge (:)
      integer(c_int) :: on_south_edge (:)
      integer(c_int) :: on_north_edge (:)
    end subroutine

    !>
    subroutine calc_c2l_grid_info(nx_pt, ny_pt, xt, yt, xc, yc, dx, dy, area, edge_w, edge_e, edge_s, edge_n, &
                                  en_n, en_e, vlon, vlat, on_west_edge, on_east_edge, on_south_edge, on_north_edge) &
                                  bind(C, name="calc_c2l_grid_info")
      use iso_c_binding, only: c_int, c_double
      integer(c_int) :: nx_pt(:)
      integer(c_int) :: ny_pt(:)
      real(c_double), allocatable :: pin(:)
      real(c_double) :: dx (:)
      real(c_double) :: dy(:)
      real(c_double) :: area(:)
      real(c_double) :: edge_w (:)
      real(c_double) :: edge_e(:)
      real(c_double) :: edge_s(:)
      real(c_double) :: edge_n(:)
      real(c_double) :: en_n (:)
      real(c_double) :: en_e(:)
      real(c_double) :: vlon(:)
      real(c_double) :: vlat(:)
      real(c_double), allocatable :: grad_x (:)
      real(c_double), allocatable :: grad_y (:)
      integer(c_int) :: on_west_edge (:)
      integer(c_int) :: on_east_edge (:)
      integer(c_int) :: on_south_edge (:)
      integer(c_int) :: on_north_edge (:)
    end subroutine

    !>
    integer function create_xgrid_2Dx1D_order1(nlon_in, nlat_in, nlon_out, nlat_out, lon_in, lat_in, &
                                         lon_out, lat_out, mask_in, i_in, j_in, i_out, j_out, &
                                         xgrid_area) bind(C, name="create_xgrid_2dx1d_order1") 
      use iso_c_binding, only: c_int, c_double
      integer(c_int) :: nlon_in
      integer(c_int) :: nlat_in
      integer(c_int) :: nlon_out
      integer(c_int) :: nlat_out
      real(c_double), allocatable :: lon_in(:,:)
      real(c_double), allocatable :: lat_in(:,:)
      real(c_double), allocatable :: lon_out(:)
      real(c_double), allocatable :: lat_out(:)
      real(c_double) :: mask_in(:,:)
      integer(c_int), allocatable :: i_in(:)
      integer(c_int), allocatable :: j_in(:)
      integer(c_int), allocatable :: i_out(:)
      integer(c_int), allocatable :: j_out(:)
      real(c_double), allocatable :: xgrid_area(:)
    end function create_xgrid_2Dx1D_order1 


    integer function create_xgrid_1Dx2D_order1(nlon_in, nlat_in, nlon_out, nlat_out, lon_in, lat_in, &
                                         lon_out, lat_out, mask_in, i_in, j_in, i_out, j_out, &
                                         xgrid_area) bind(C, name="create_xgrid_1dx2d_order1") 
      use iso_c_binding, only: c_int, c_double
      integer(c_int) :: nlon_in
      integer(c_int) :: nlat_in
      integer(c_int) :: nlon_out
      integer(c_int) :: nlat_out
      real(c_double), allocatable :: lon_in(:)
      real(c_double), allocatable :: lat_in(:)
      real(c_double), allocatable :: lon_out(:,:)
      real(c_double), allocatable :: lat_out(:,:)
      real(c_double) :: mask_in(:,:)
      integer(c_int), allocatable :: i_in(:)
      integer(c_int), allocatable :: j_in(:)
      integer(c_int), allocatable :: i_out(:)
      integer(c_int), allocatable :: j_out(:)
      real(c_double), allocatable :: xgrid_area(:)
    end function create_xgrid_1Dx2D_order1 

    integer function create_xgrid_2Dx2D_order1(nlon_in, nlat_in, nlon_out, nlat_out, lon_in, lat_in, &
                                         lon_out, lat_out, mask_in, i_in, j_in, i_out, j_out, &
                                         xgrid_area) bind(C, name="create_xgrid_2dx2d_order1") 
      use iso_c_binding, only: c_int, c_double
      integer(c_int) :: nlon_in
      integer(c_int) :: nlat_in
      integer(c_int) :: nlon_out
      integer(c_int) :: nlat_out
      real(c_double), allocatable :: lon_in(:,:)
      real(c_double), allocatable :: lat_in(:,:)
      real(c_double), allocatable :: lon_out(:,:)
      real(c_double), allocatable :: lat_out(:,:)
      real(c_double) :: mask_in(:,:)
      integer(c_int), allocatable :: i_in(:)
      integer(c_int), allocatable :: j_in(:)
      integer(c_int), allocatable :: i_out(:)
      integer(c_int), allocatable :: j_out(:)
      real(c_double), allocatable :: xgrid_area(:)
    end function create_xgrid_2Dx2D_order1 
    

    !> Returns the MAXXGRID size, which is the maximum number of grid points to calculate.
    !! Default is 1e6, but can be set via the MAXXGRID cpp macro
    integer function get_maxxgrid() bind(C, name="get_maxxgrid") 
    end function get_maxxgrid 
 
    integer function create_xgrid_great_circle(nlon_in, nlat_in, nlon_out, nlat_out, lon_in, lat_in, &
                                         lon_out, lat_out, mask_in, i_in, j_in, i_out, j_out, &
                                         xgrid_area, xgrid_clon, xgrid_clat) bind(C, name="create_xgrid_great_circle")
      use iso_c_binding, only: c_int, c_double
      integer(c_int) :: nlon_in
      integer(c_int) :: nlat_in
      integer(c_int) :: nlon_out
      integer(c_int) :: nlat_out
      real(c_double), allocatable :: lon_in(:,:)
      real(c_double), allocatable :: lat_in(:,:)
      real(c_double), allocatable :: lon_out(:,:)
      real(c_double), allocatable :: lat_out(:,:)
      real(c_double) :: mask_in(:,:)
      integer(c_int), allocatable :: i_in(:)
      integer(c_int), allocatable :: j_in(:)
      integer(c_int), allocatable :: i_out(:)
      integer(c_int), allocatable :: j_out(:)
      real(c_double), allocatable :: xgrid_area(:)
      real(c_double), allocatable :: xgrid_clon(:)
      real(c_double), allocatable :: xgrid_clat(:)
    end function create_xgrid_great_circle
    

  end interface

end module
