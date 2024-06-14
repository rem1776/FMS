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
!> @brief This module is to provide fortran-native access to c functions used within mosaic,
!! mosaic2, and horiz_interp.
!!
!> @author Ryan Mulhall 

!> @addtogroup mosaic2_mod
!> @{
module grid_utils_mod

  !> for mosaic2
  public :: get_grid_great_circle_area, get_grid_area
  !> for horiz_interp
  public :: create_xgrid_2DX1D_order1, create_xgrid_1DX2D_order1, get_maxxgrid, create_xgrid_great_circle, &
            create_xgrid_2Dx2D_order1
!!#ifdef use_deprecated_io
  !> for mosaic
  public :: grad_c2l, calc_c2l_grid_info
!!#endif

#ifndef MAXXGRID
#define MAXXGRID 1e6 
#endif

    !! TODO
    !! change nlon/nlat to x/y or something more descriptive, they are used as the 1st and 2nd dims of both lon and lat arrays

  interface 
    !> Calculates the area of each cell in the given lon/lat arrays
    subroutine get_grid_area(nlon, nlat, lon, lat, area) bind(C, name="get_grid_area")
      use iso_c_binding, only: c_int, c_double
      integer(c_int), value :: nlon
      integer(c_int), value :: nlat
      real(c_double), intent(in) :: lon(nlon, nlat)
      real(c_double), intent(in) :: lat(nlon, nlat)
      real(c_double) :: area(nlon, nlat)
    end subroutine

    !>
    subroutine get_grid_great_circle_area(nlon, nlat, lon, lat, area) bind(C, name="get_grid_great_circle_area")
      use iso_c_binding, only: c_int, c_double
      integer(c_int), value :: nlon
      integer(c_int), value :: nlat
      real(c_double) :: lon(nlon,nlat)
      real(c_double) :: lat(nlon,nlat)
      real(c_double) :: area(nlon,nlat)
    end subroutine

    !> Routine to compute gradient terms for SCRIP
    !! pin has halo size = 1.
    subroutine grad_c2l(nlon, nlat, pin, dx, dy, area, edge_w, edge_e, edge_s, edge_n, &
                        en_n, en_e, vlon, vlat, grad_x, grad_y, on_west_edge, on_east_edge, &
                        on_south_edge, on_north_edge) bind(C, name="grad_c2l")
      use iso_c_binding, only: c_int, c_double
      integer(c_int), value :: nlon
      integer(c_int), value :: nlat
      real(c_double) :: pin(nlon+2, nlat+2)
      real(c_double) :: dx (nlon, nlat+1)
      real(c_double) :: dy(nlon+1, nlat)
      real(c_double) :: area(nlon, nlat)
      real(c_double) :: edge_w (nlat+1)
      real(c_double) :: edge_e(nlat+1)
      real(c_double) :: edge_s(nlon+1)
      real(c_double) :: edge_n(nlon+1)
      real(c_double) :: en_n (nlat, nlon+1, 3)
      real(c_double) :: en_e(nlon+1, nlat, 3)
      real(c_double) :: vlon(nlon, nlat, 3)
      real(c_double) :: vlat(nlon, nlat, 3)
      real(c_double) :: grad_x (nlon, nlat)
      real(c_double) :: grad_y (nlon, nlat)
      integer(c_int), value :: on_west_edge
      integer(c_int), value :: on_east_edge
      integer(c_int), value :: on_south_edge
      integer(c_int), value :: on_north_edge
    end subroutine

    !> This routine is used to calculate grid information for second order conservative interpolation
    !! from cubic grid to other grid
    subroutine calc_c2l_grid_info(nx_pt, ny_pt, xt, yt, xc, yc, dx, dy, area, edge_w, edge_e, edge_s, edge_n, &
                                  en_n, en_e, vlon, vlat, on_west_edge, on_east_edge, on_south_edge, on_north_edge) &
                                  bind(C, name="calc_c2l_grid_info")
      use iso_c_binding, only: c_int, c_double
      integer(c_int), value :: nx_pt
      integer(c_int), value :: ny_pt
      real(c_double) :: xt(nx_pt+2, ny_pt+2)
      real(c_double) :: yt(nx_pt+2, ny_pt+2)
      real(c_double) :: xc(nx_pt+1, ny_pt+1)
      real(c_double) :: yc(nx_pt+1, ny_pt+1)
      real(c_double) :: dx(nx_pt, ny_pt+1)
      real(c_double) :: dy(nx_pt+1, ny_pt)
      real(c_double) :: area(nx_pt, ny_pt)
      real(c_double) :: edge_w(ny_pt-1)
      real(c_double) :: edge_e(ny_pt-1)
      real(c_double) :: edge_s(nx_pt-1)
      real(c_double) :: edge_n(nx_pt-1)
      real(c_double) :: en_n(nx_pt, ny_pt+1, 3)
      real(c_double) :: en_e(nx_pt+1, ny_pt, 3)
      real(c_double) :: vlon(nx_pt, ny_pt)
      real(c_double) :: vlat(nx_pt, ny_pt)
      integer(c_int), value :: on_west_edge
      integer(c_int), value :: on_east_edge
      integer(c_int), value :: on_south_edge
      integer(c_int), value :: on_north_edge
    end subroutine

    !>
    integer function create_xgrid_2Dx1D_order1(nlon_in, nlat_in, nlon_out, nlat_out, lon_in, lat_in, &
                                         lon_out, lat_out, mask_in, i_in, j_in, i_out, j_out, &
                                         xgrid_area) bind(C, name="create_xgrid_2dx1d_order1") 
      use iso_c_binding, only: c_int, c_double
      integer(c_int), value :: nlon_in
      integer(c_int), value :: nlat_in
      integer(c_int), value :: nlon_out
      integer(c_int), value :: nlat_out
      real(c_double) :: lon_in(nlon_in,nlat_in)
      real(c_double) :: lat_in(nlon_in,nlat_in)
      real(c_double) :: lon_out(nlon_out)
      real(c_double) :: lat_out(nlat_out)
      real(c_double) :: mask_in(nlon_in,nlat_in)
      integer(c_int) :: i_in(MAXXGRID)
      integer(c_int) :: j_in(MAXXGRID)
      integer(c_int) :: i_out(MAXXGRID)
      integer(c_int) :: j_out(MAXXGRID)
      real(c_double) :: xgrid_area(MAXXGRID)
    end function create_xgrid_2Dx1D_order1 


    integer function create_xgrid_1Dx2D_order1(nlon_in, nlat_in, nlon_out, nlat_out, lon_in, lat_in, &
                                         lon_out, lat_out, mask_in, i_in, j_in, i_out, j_out, &
                                         xgrid_area) bind(C, name="create_xgrid_1dx2d_order1") 
      use iso_c_binding, only: c_int, c_double
      integer(c_int), value :: nlon_in
      integer(c_int), value :: nlat_in
      integer(c_int), value :: nlon_out
      integer(c_int), value :: nlat_out
      real(c_double) :: lon_in(nlon_in)
      real(c_double) :: lat_in(nlat_in)
      real(c_double) :: lon_out(nlon_out, nlat_out)
      real(c_double) :: lat_out(nlon_out, nlat_out)
      real(c_double) :: mask_in(nlon_in,nlat_in)
      integer(c_int) :: i_in(MAXXGRID)
      integer(c_int) :: j_in(MAXXGRID)
      integer(c_int) :: i_out(MAXXGRID)
      integer(c_int) :: j_out(MAXXGRID)
      real(c_double) :: xgrid_area(MAXXGRID)
    end function create_xgrid_1Dx2D_order1 

    integer function create_xgrid_2Dx2D_order1(nlon_in, nlat_in, nlon_out, nlat_out, lon_in, lat_in, &
                                         lon_out, lat_out, mask_in, i_in, j_in, i_out, j_out, &
                                         xgrid_area) bind(C, name="create_xgrid_2dx2d_order1") 
      use iso_c_binding, only: c_int, c_double
      integer(c_int), value :: nlon_in
      integer(c_int), value :: nlat_in
      integer(c_int), value :: nlon_out
      integer(c_int), value :: nlat_out
      real(c_double) :: lon_in(nlon_in, nlat_in)
      real(c_double) :: lat_in(nlon_in, nlat_in)
      real(c_double) :: lon_out(nlon_out, nlat_out)
      real(c_double) :: lat_out(nlon_out, nlat_out)
      real(c_double) :: mask_in(nlon_in,nlat_in)
      integer(c_int) :: i_in(MAXXGRID)
      integer(c_int) :: j_in(MAXXGRID)
      integer(c_int) :: i_out(MAXXGRID)
      integer(c_int) :: j_out(MAXXGRID)
      real(c_double) :: xgrid_area(MAXXGRID)
    end function create_xgrid_2Dx2D_order1 
    

    !> Returns the MAXXGRID size, which is the maximum number of grid points to calculate.
    !! Default is 1e6, but can be set via the MAXXGRID cpp macro
    integer function get_maxxgrid() bind(C, name="get_maxxgrid") 
    end function get_maxxgrid 
 
    integer function create_xgrid_great_circle(nlon_in, nlat_in, nlon_out, nlat_out, lon_in, lat_in, &
                                         lon_out, lat_out, mask_in, i_in, j_in, i_out, j_out, &
                                         xgrid_area, xgrid_clon, xgrid_clat) bind(C, name="create_xgrid_great_circle")
      use iso_c_binding, only: c_int, c_double
      integer(c_int), value :: nlon_in
      integer(c_int), value :: nlat_in
      integer(c_int), value :: nlon_out
      integer(c_int), value :: nlat_out
      real(c_double) :: lon_in(nlon_in, nlat_in)
      real(c_double) :: lat_in(nlon_in, nlat_in)
      real(c_double) :: lon_out(nlon_out, nlat_out)
      real(c_double) :: lat_out(nlon_out, nlat_out)
      real(c_double) :: mask_in(nlon_in,nlat_in)
      integer(c_int) :: i_in(MAXXGRID)
      integer(c_int) :: j_in(MAXXGRID)
      integer(c_int) :: i_out(MAXXGRID)
      integer(c_int) :: j_out(MAXXGRID)
      real(c_double) :: xgrid_area(MAXXGRID)
      real(c_double) :: xgrid_clon(MAXXGRID)
      real(c_double) :: xgrid_clat(MAXXGRID)
    end function create_xgrid_great_circle
  end interface

end module
