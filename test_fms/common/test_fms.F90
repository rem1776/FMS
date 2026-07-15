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

! This module allows arrays to be permuted, and provides a data type for the
! purpose of storing permuted array bounds. It provides procedures for
! initializing a 2D or 3D array with random data, and for comparing a 2D or
! 3D array with reference answers.

module fms_test_mod
  use random_numbers_mod, only: randomNumberStream, initializeRandomNumberStream, getRandomNumbers
  use mpp_mod,         only: mpp_error, FATAL
  use mpp_domains_mod, only: domain2d, mpp_define_mosaic
  use platform_mod

  implicit none

  interface arr_init
    module procedure :: arr_init_2d_r4, arr_init_2d_r8, arr_init_2d_i4, arr_init_2d_i8
    module procedure :: arr_init_3d_r4, arr_init_3d_r8, arr_init_3d_i4, arr_init_3d_i8
  end interface arr_init

  interface arr_compare
    module procedure :: arr_compare_2d_r4, arr_compare_2d_r8, arr_compare_2d_i4, arr_compare_2d_i8
    module procedure :: arr_compare_3d_r4, arr_compare_3d_r8, arr_compare_3d_i4, arr_compare_3d_i8
    module procedure :: arr_compare_4d_r4, arr_compare_4d_r8, arr_compare_4d_i4, arr_compare_4d_i8
  end interface arr_compare

  interface arr_compare_tol
    module procedure :: arr_compare_tol_2d_r4, arr_compare_tol_2d_r8
    module procedure :: arr_compare_tol_3d_r4, arr_compare_tol_3d_r8
    module procedure :: arr_compare_tol_4d_r4, arr_compare_tol_4d_r8

    module procedure :: arr_compare_tol_2d_scalar_r4, arr_compare_tol_2d_scalar_r8
    module procedure :: arr_compare_tol_3d_scalar_r4, arr_compare_tol_3d_scalar_r8
    module procedure :: arr_compare_tol_4d_scalar_r4, arr_compare_tol_4d_scalar_r8
  end interface arr_compare_tol

  ! TODO: `permutable_indices` should really be implemented as a parameterized derived type, but because gfortran 13
  ! doesn't support parameterized derived types with type-bound procedures, the following workaround is needed. This
  ! should be changed to a parameterized derived type once gfortran 13 support is no longer needed.

  type permutable_indices_2d
    integer :: lb(2), ub(2)

    contains

    procedure :: permute => permutable_indices_permute_2d
    procedure :: n => permutable_indices_n_2d
  end type permutable_indices_2d

  type permutable_indices_3d
    integer :: lb(3), ub(3)

    contains

    procedure :: permute => permutable_indices_permute_3d
    procedure :: n => permutable_indices_n_3d
  end type permutable_indices_3d

  type permutable_indices_4d
    integer :: lb(4), ub(4)

    contains

    procedure :: permute => permutable_indices_permute_4d
    procedure :: n => permutable_indices_n_4d
  end type permutable_indices_4d

  contains

#define FMS_TEST_TYPE_ real
#define TYPECAST_ real

#define FMS_TEST_KIND_ r4_kind

#define ARR_INIT_2D_ arr_init_2d_r4
#define ARR_INIT_3D_ arr_init_3d_r4
#define ARR_COMPARE_2D_ arr_compare_2d_r4
#define ARR_COMPARE_3D_ arr_compare_3d_r4
#define ARR_COMPARE_4D_ arr_compare_4d_r4
#include "test_fms.inc"
#undef ARR_INIT_2D_
#undef ARR_INIT_3D_
#undef ARR_COMPARE_2D_
#undef ARR_COMPARE_3D_
#undef ARR_COMPARE_4D_

#define ARR_COMPARE_TOL_2D_ arr_compare_tol_2d_r4
#define ARR_COMPARE_TOL_3D_ arr_compare_tol_3d_r4
#define ARR_COMPARE_TOL_4D_ arr_compare_tol_4d_r4
#define ARR_COMPARE_TOL_2D_SCALAR_ arr_compare_tol_2d_scalar_r4
#define ARR_COMPARE_TOL_3D_SCALAR_ arr_compare_tol_3d_scalar_r4
#define ARR_COMPARE_TOL_4D_SCALAR_ arr_compare_tol_4d_scalar_r4
#include "test_fms_real.inc"
#undef ARR_COMPARE_TOL_2D_
#undef ARR_COMPARE_TOL_3D_
#undef ARR_COMPARE_TOL_4D_
#undef ARR_COMPARE_TOL_2D_SCALAR_
#undef ARR_COMPARE_TOL_3D_SCALAR_
#undef ARR_COMPARE_TOL_4D_SCALAR_

#undef FMS_TEST_KIND_
#define FMS_TEST_KIND_ r8_kind

#define ARR_INIT_2D_ arr_init_2d_r8
#define ARR_INIT_3D_ arr_init_3d_r8
#define ARR_COMPARE_2D_ arr_compare_2d_r8
#define ARR_COMPARE_3D_ arr_compare_3d_r8
#define ARR_COMPARE_4D_ arr_compare_4d_r8
#include "test_fms.inc"
#undef ARR_INIT_2D_
#undef ARR_INIT_3D_
#undef ARR_COMPARE_2D_
#undef ARR_COMPARE_3D_
#undef ARR_COMPARE_4D_
#undef ARR_COMPARE_TOL_2D_SCALAR_
#undef ARR_COMPARE_TOL_3D_SCALAR_
#undef ARR_COMPARE_TOL_4D_SCALAR_

#define ARR_COMPARE_TOL_2D_ arr_compare_tol_2d_r8
#define ARR_COMPARE_TOL_3D_ arr_compare_tol_3d_r8
#define ARR_COMPARE_TOL_4D_ arr_compare_tol_4d_r8
#define ARR_COMPARE_TOL_2D_SCALAR_ arr_compare_tol_2d_scalar_r8
#define ARR_COMPARE_TOL_3D_SCALAR_ arr_compare_tol_3d_scalar_r8
#define ARR_COMPARE_TOL_4D_SCALAR_ arr_compare_tol_4d_scalar_r8
#include "test_fms_real.inc"
#undef ARR_COMPARE_TOL_2D_
#undef ARR_COMPARE_TOL_3D_
#undef ARR_COMPARE_TOL_4D_
#undef ARR_COMPARE_TOL_2D_SCALAR_
#undef ARR_COMPARE_TOL_3D_SCALAR_
#undef ARR_COMPARE_TOL_4D_SCALAR_

#undef FMS_TEST_KIND_

#undef FMS_TEST_TYPE_
#undef TYPECAST_

#define FMS_TEST_TYPE_ integer
#define TYPECAST_ int

#define FMS_TEST_KIND_ i4_kind
#define ARR_INIT_2D_ arr_init_2d_i4
#define ARR_INIT_3D_ arr_init_3d_i4
#define ARR_COMPARE_2D_ arr_compare_2d_i4
#define ARR_COMPARE_3D_ arr_compare_3d_i4
#define ARR_COMPARE_4D_ arr_compare_4d_i4
#include "test_fms.inc"
#undef FMS_TEST_KIND_
#undef ARR_INIT_2D_
#undef ARR_INIT_3D_
#undef ARR_COMPARE_2D_
#undef ARR_COMPARE_3D_
#undef ARR_COMPARE_4D_

#define FMS_TEST_KIND_ i8_kind
#define ARR_INIT_2D_ arr_init_2d_i8
#define ARR_INIT_3D_ arr_init_3d_i8
#define ARR_COMPARE_2D_ arr_compare_2d_i8
#define ARR_COMPARE_3D_ arr_compare_3d_i8
#define ARR_COMPARE_4D_ arr_compare_4d_i8
#include "test_fms.inc"
#undef FMS_TEST_KIND_
#undef ARR_INIT_2D_
#undef ARR_INIT_3D_
#undef ARR_COMPARE_2D_
#undef ARR_COMPARE_3D_
#undef ARR_COMPARE_4D_

#undef FMS_TEST_TYPE_
#undef TYPECAST_

  subroutine permutable_indices_permute_2d(self, p)
    class(permutable_indices_2d), intent(inout) :: self
    integer, intent(in) :: p

    call permute_arr(self%lb, p)
    call permute_arr(self%ub, p)
  end subroutine permutable_indices_permute_2d

  subroutine permutable_indices_permute_3d(self, p)
    class(permutable_indices_3d), intent(inout) :: self
    integer, intent(in) :: p

    call permute_arr(self%lb, p)
    call permute_arr(self%ub, p)
  end subroutine permutable_indices_permute_3d

  subroutine permutable_indices_permute_4d(self, p)
    class(permutable_indices_4d), intent(inout) :: self
    integer, intent(in) :: p

    call permute_arr(self%lb, p)
    call permute_arr(self%ub, p)
  end subroutine permutable_indices_permute_4d

  function permutable_indices_n_2d(self, i) result(n)
    class(permutable_indices_2d), intent(inout) :: self
    integer, intent(in) :: i
    integer :: n

    n = self%ub(i) - self%lb(i) + 1
  end function permutable_indices_n_2d

  function permutable_indices_n_3d(self, i) result(n)
    class(permutable_indices_3d), intent(inout) :: self
    integer, intent(in) :: i
    integer :: n

    n = self%ub(i) - self%lb(i) + 1
  end function permutable_indices_n_3d

  function permutable_indices_n_4d(self, i) result(n)
    class(permutable_indices_4d), intent(inout) :: self
    integer, intent(in) :: i
    integer :: n

    n = self%ub(i) - self%lb(i) + 1
  end function permutable_indices_n_4d

  !> Define a six-tile cubic mosaic domain layout used by multiple tests.
  subroutine define_cubic_mosaic(type, domain, ni, nj, global_indices, layout, pe_start, pe_end, use_memsize, halo)
    character(len=*), intent(in)  :: type
    type(domain2d), intent(inout) :: domain
    integer,        intent(in)    :: global_indices(:,:), layout(:,:)
    integer,        intent(in)    :: ni(:), nj(:)
    integer,        intent(in)    :: pe_start(:), pe_end(:)
    logical, optional, intent(in) :: use_memsize
    integer, optional, intent(in) :: halo
    integer, dimension(12)        :: istart1, iend1, jstart1, jend1, tile1
    integer, dimension(12)        :: istart2, iend2, jstart2, jend2, tile2
    integer                       :: ntiles, num_contact, msize(2)
    logical                       :: use_memsize_local
    integer                       :: whalo = 2, shalo = 2, ehalo = 2, nhalo = 2

    use_memsize_local = .true.
    if (present(use_memsize)) use_memsize_local = use_memsize
    if (present(halo)) then
    nhalo = halo
    shalo = halo
    whalo = halo
    ehalo = halo
    endif

    ntiles = 6
    num_contact = 12
    if(size(pe_start(:)) .NE. 6 .OR. size(pe_end(:)) .NE. 6 ) call mpp_error(FATAL, &
      "define_cubic_mosaic: size of pe_start and pe_end should be 6")
    if(size(global_indices,1) .NE. 4) call mpp_error(FATAL, &
      "define_cubic_mosaic: size of first dimension of global_indices should be 4")
    if(size(global_indices,2) .NE. 6) call mpp_error(FATAL, &
      "define_cubic_mosaic: size of second dimension of global_indices should be 6")
    if(size(layout,1) .NE. 2) call mpp_error(FATAL, &
      "define_cubic_mosaic: size of first dimension of layout should be 2")
    if(size(layout,2) .NE. 6) call mpp_error(FATAL, &
      "define_cubic_mosaic: size of second dimension of layout should be 6")
    if(size(ni(:)) .NE. 6 .OR. size(nj(:)) .NE. 6) call mpp_error(FATAL, &
      "define_cubic_mosaic: size of ni and nj should be 6")

    ! Contact line 1, between tile 1 (EAST) and tile 2 (WEST)
    tile1(1) = 1; tile2(1) = 2
    istart1(1) = ni(1);  iend1(1) = ni(1);  jstart1(1) = 1;      jend1(1) = nj(1)
    istart2(1) = 1;      iend2(1) = 1;      jstart2(1) = 1;      jend2(1) = nj(2)
    ! Contact line 2, between tile 1 (NORTH) and tile 3 (WEST)
    tile1(2) = 1; tile2(2) = 3
    istart1(2) = 1;      iend1(2) = ni(1);  jstart1(2) = nj(1);  jend1(2) = nj(1)
    istart2(2) = 1;      iend2(2) = 1;      jstart2(2) = nj(3);  jend2(2) = 1
    ! Contact line 3, between tile 1 (WEST) and tile 5 (NORTH)
    tile1(3) = 1; tile2(3) = 5
    istart1(3) = 1;      iend1(3) = 1;      jstart1(3) = 1;      jend1(3) = nj(1)
    istart2(3) = ni(5);  iend2(3) = 1;      jstart2(3) = nj(5);  jend2(3) = nj(5)
    ! Contact line 4, between tile 1 (SOUTH) and tile 6 (NORTH)
    tile1(4) = 1; tile2(4) = 6
    istart1(4) = 1;      iend1(4) = ni(1);  jstart1(4) = 1;      jend1(4) = 1
    istart2(4) = 1;      iend2(4) = ni(6);  jstart2(4) = nj(6);  jend2(4) = nj(6)
    ! Contact line 5, between tile 2 (NORTH) and tile 3 (SOUTH)
    tile1(5) = 2; tile2(5) = 3
    istart1(5) = 1;      iend1(5) = ni(2);  jstart1(5) = nj(2);  jend1(5) = nj(2)
    istart2(5) = 1;      iend2(5) = ni(3);  jstart2(5) = 1;      jend2(5) = 1
    ! Contact line 6, between tile 2 (EAST) and tile 4 (SOUTH)
    tile1(6) = 2; tile2(6) = 4
    istart1(6) = ni(2);  iend1(6) = ni(2);  jstart1(6) = 1;      jend1(6) = nj(2)
    istart2(6) = ni(4);  iend2(6) = 1;      jstart2(6) = 1;      jend2(6) = 1
    ! Contact line 7, between tile 2 (SOUTH) and tile 6 (EAST)
    tile1(7) = 2; tile2(7) = 6
    istart1(7) = 1;      iend1(7) = ni(2);  jstart1(7) = 1;      jend1(7) = 1
    istart2(7) = ni(6);  iend2(7) = ni(6);  jstart2(7) = nj(6);  jend2(7) = 1
    ! Contact line 8, between tile 3 (EAST) and tile 4 (WEST)
    tile1(8) = 3; tile2(8) = 4
    istart1(8) = ni(3);  iend1(8) = ni(3);  jstart1(8) = 1;      jend1(8) = nj(3)
    istart2(8) = 1;      iend2(8) = 1;      jstart2(8) = 1;      jend2(8) = nj(4)
    ! Contact line 9, between tile 3 (NORTH) and tile 5 (WEST)
    tile1(9) = 3; tile2(9) = 5
    istart1(9) = 1;      iend1(9) = ni(3);  jstart1(9) = nj(3);  jend1(9) = nj(3)
    istart2(9) = 1;      iend2(9) = 1;      jstart2(9) = nj(5);  jend2(9) = 1
    ! Contact line 10, between tile 4 (NORTH) and tile 5 (SOUTH)
    tile1(10) = 4; tile2(10) = 5
    istart1(10) = 1;     iend1(10) = ni(4); jstart1(10) = nj(4); jend1(10) = nj(4)
    istart2(10) = 1;     iend2(10) = ni(5); jstart2(10) = 1;     jend2(10) = 1
    ! Contact line 11, between tile 4 (EAST) and tile 6 (SOUTH)
    tile1(11) = 4; tile2(11) = 6
    istart1(11) = ni(4); iend1(11) = ni(4); jstart1(11) = 1;     jend1(11) = nj(4)
    istart2(11) = ni(6); iend2(11) = 1;     jstart2(11) = 1;     jend2(11) = 1
    ! Contact line 12, between tile 5 (EAST) and tile 6 (WEST)
    tile1(12) = 5; tile2(12) = 6
    istart1(12) = ni(5); iend1(12) = ni(5); jstart1(12) = 1;     jend1(12) = nj(5)
    istart2(12) = 1;     iend2(12) = 1;     jstart2(12) = 1;     jend2(12) = nj(6)

    msize(1) = maxval(ni(:)/layout(1,:)) + whalo + ehalo + 1
    msize(2) = maxval(nj(:)/layout(2,:)) + shalo + nhalo + 1

    if(use_memsize_local) then
    call mpp_define_mosaic(global_indices, layout, domain, ntiles, num_contact, tile1, tile2, &
      istart1, iend1, jstart1, jend1, istart2, iend2, jstart2, jend2,      &
      pe_start, pe_end, symmetry = .true., whalo=whalo, ehalo=ehalo,   &
      shalo=shalo, nhalo=nhalo, name = trim(type), memory_size = msize  )
    else
    call mpp_define_mosaic(global_indices, layout, domain, ntiles, num_contact, tile1, tile2, &
      istart1, iend1, jstart1, jend1, istart2, iend2, jstart2, jend2,      &
      pe_start, pe_end, symmetry = .true., whalo=whalo, ehalo=ehalo,   &
      shalo=shalo, nhalo=nhalo, name = trim(type) )
    endif
  end subroutine define_cubic_mosaic

  pure recursive function factorial(n) result(res)
    integer, intent(in) :: n
    integer :: res

    if (n.eq.0) then
      res = 1
    else
      res = n * factorial(n-1)
    endif
  end function factorial

  subroutine permute_arr(arr, p)
    integer, intent(inout) :: arr(:) !< List to be permuted
    integer, intent(in) :: p !< Which permutation to produce: may range from 1 to size(arr)!
    integer :: choices(size(arr))
    integer :: n, k, i, f, indx

    n = size(arr)
    if (p.lt.1 .or. p.gt.factorial(n)) then
      print *, "Error: p parameter is out of bounds"
      stop 1
    endif

    choices = arr
    k = p - 1

    do i=1,n
      f = factorial(n - i)
      indx = k / f + 1
      k = mod(k, f)

      arr(i) = choices(indx)
      choices(indx) = choices(n + 1 - i)
    enddo
  end subroutine permute_arr
end module fms_test_mod
