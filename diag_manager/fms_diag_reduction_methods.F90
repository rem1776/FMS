!***********************************************************************
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

!> @defgroup fms_diag_reduction_methods_mod fms_diag_reduction_methods_mod
!> @ingroup diag_manager
!! @brief fms_diag_reduction_methods_mod contains routines that are meant to be used for
!! error checking and setting up to do the reduction methods

!> @file
!> @brief File for @ref fms_diag_reduction_methods_mod

!> @addtogroup fms_diag_reduction_methods_mod
!> @{
module fms_diag_reduction_methods_mod
  use platform_mod, only: r8_kind, r4_kind
  use fms_diag_bbox_mod, only: fmsDiagBoundsHalos_type, fmsDiagIbounds_type
  use diag_data_mod, only: debug_diag_manager, time_max, time_min
  use fms_mod, only: fms_error_handler
  use mpp_mod, only: mpp_error, FATAL ! TODO should double check we actually need both error routines (one might be for nonfatals)
  use platform_mod, only: i4_kind, i8_kind, r4_kind, r8_kind

  implicit none
  private

  public :: check_indices_order, init_mask, set_weight
  public :: do_time_none, init_mask_3d, real_copy_set, fms_diag_update_extremum

  !> @brief Does the time_none reduction method. See include/fms_diag_reduction_methods.inc
  !TODO This needs to be extended to integers
  interface do_time_none
    module procedure do_time_none_r4, do_time_none_r8
  end interface do_time_none

 ! helper routines from the include
  interface fms_diag_update_extremum
    module procedure fms_diag_update_extremum_r4
    module procedure fms_diag_update_extremum_r8
  end interface fms_diag_update_extremum

  contains

  !> @brief Compares the corresponding bounding indices of the first set with the second set.
  !> @return .TRUE. if any comparison returns true; i.e. the box bounded by the indices of the first set
  !! is out side the box bounded by the indices of the second set.
  LOGICAL FUNCTION compare_two_sets_of_bounds(bounds_a, bounds_b, error_str)
    integer, intent(in) :: bounds_a(:) !< First array with order: (/imin, imax, jmin, jmax, kmin, kmax/)
    integer, intent(in) :: bounds_b(:) !< Second array with the same order as the first
    character(*), intent(out) :: error_str !< Error message to report back

    compare_two_sets_of_bounds = .FALSE.

    if (size(bounds_a) .ne. size(bounds_b)) then
      compare_two_sets_of_bounds = .TRUE.
      error_str = 'fms_diag_reduction_methods_mod::compare_two_sets_of_bounds Error: sizes of sets do not match'
    else
      if ((size(bounds_a) .ne. 6) .and. (size(bounds_b) .ne. 6)) then
        compare_two_sets_of_bounds = .TRUE.
        error_str = 'fms_diag_reduction_methods_mod::compare_two_sets_of_bounds Error: sizes of sets must be 6'
      end if
    end if

    IF (bounds_a(1) .lt. bounds_b(1) .OR. bounds_a(2) .gt. bounds_b(2) .OR. &
      bounds_a(3) .lt. bounds_b(3) .OR. bounds_a(4) .gt. bounds_b(4) .OR. &
      bounds_a(5) .lt. bounds_b(5) .OR. bounds_a(6) .gt. bounds_b(6)) THEN
      compare_two_sets_of_bounds = .TRUE.
      error_str ='First set of bounds=   :   ,   :   ,   :     Second set of bounds=   :   ,   :   ,   :    '
      WRITE(error_str(21:23),'(i3)') bounds_a(1)
      WRITE(error_str(25:27),'(i3)') bounds_a(2)
      WRITE(error_str(29:31),'(i3)') bounds_a(3)
      WRITE(error_str(33:35),'(i3)') bounds_a(4)
      WRITE(error_str(37:39),'(i3)') bounds_a(5)
      WRITE(error_str(41:43),'(i3)') bounds_a(6)
      WRITE(error_str(68:70),'(i3)') bounds_b(1)
      WRITE(error_str(72:74),'(i3)') bounds_b(2)
      WRITE(error_str(76:78),'(i3)') bounds_b(3)
      WRITE(error_str(80:82),'(i3)') bounds_b(4)
      WRITE(error_str(84:86),'(i3)') bounds_b(5)
      WRITE(error_str(88:90),'(i3)') bounds_b(6)
    ELSE
      compare_two_sets_of_bounds = .FALSE.
      error_str = ''
    END IF
  END FUNCTION compare_two_sets_of_bounds

  !> @brief Checks improper combinations of is, ie, js, and je.
  !! @return The error message, empty string if no errors were found
  !> @note accept_data works in either one or another of two modes.
  !! 1. Input field is a window (e.g. FMS physics)
  !! 2. Input field includes halo data
  !! It cannot handle a window of data that has halos.
  !! (A field with no windows or halos can be thought of as a special case of either mode.)
  !! The logic for indexing is quite different for these two modes, but is not clearly separated.
  !! If both the beggining and ending indices are present, then field is assumed to have halos.
  !! If only beggining indices are present, then field is assumed to be a window.
  !> @par
  !! There are a number of ways a user could mess up this logic, depending on the combination
  !! of presence/absence of is,ie,js,je. The checks below should catch improper combinations.
  pure function check_indices_order(is_in, ie_in, js_in, je_in) &
  result(error_msg)
    integer, intent(in), optional :: is_in, ie_in, js_in, je_in !< Indices passed to fms_diag_accept_data()
    character(len=128) :: error_msg !< An error message used only for testing purpose!!!

    error_msg = ""
    IF ( PRESENT(ie_in) ) THEN
      IF ( .NOT.PRESENT(is_in) ) THEN
        error_msg = 'ie_in present without is_in'
        return
      END IF
      IF ( PRESENT(js_in) .AND. .NOT.PRESENT(je_in) ) THEN
        error_msg = 'is_in and ie_in present, but js_in present without je_in'
        return
      END IF
    END IF

    IF ( PRESENT(je_in) ) THEN
      IF ( .NOT.PRESENT(js_in) ) THEN
        error_msg = 'je_in present without js_in'
        return
      END IF
      IF ( PRESENT(is_in) .AND. .NOT.PRESENT(ie_in) ) THEN
        error_msg = 'js_in and je_in present, but is_in present without ie_in'
        return
      END IF
    END IF
  end function check_indices_order

  !> @brief Copies input data to output data with specific type and precision
  !! if the input data is present else sets the output data to a given value val if it is present.
  !! If the value val and the input data are not present, the output data is untouched.
  subroutine real_copy_set(out_data, in_data, val, err_msg)
    real, intent(out) :: out_data !< Proper type copy of in_data
    class(*), intent(in), optional :: in_data !< Data to copy to out_data
    real, intent(in), optional :: val !< Default value to assign to out_data if in_data is absent
    character(len=*), intent(out), optional :: err_msg !< Error message to pass back to caller

    IF ( PRESENT(err_msg) ) err_msg = ''

    IF ( PRESENT(in_data) ) THEN
      SELECT TYPE (in_data)
      TYPE IS (real(kind=r4_kind))
        out_data = in_data
      TYPE IS (real(kind=r8_kind))
        out_data = real(in_data)
      CLASS DEFAULT
        if (fms_error_handler('fms_diag_reduction_methods_mod::real_copy_set',&
          & 'The in_data is not one of the supported types of real(kind=4) or real(kind=8)', err_msg)) THEN
          return
        end if
      END SELECT
    ELSE
      if (present(val)) then
        out_data = val
      else
        call mpp_error(FATAL, 'fms_diag_reduction_methods_mod::real_copy_set both in_data and val can be absent')
      end if
    END IF
  end subroutine real_copy_set

  !> @brief Allocates `outmask'(second argument) with sizes of the first three dimensions of
  !! the field(first argument).
  !! Initializes the `outmask' depending on presence/absence of `inmask' and `rmask'.
  !! Uses `rmask_threshold' to set the `outmask'.
  subroutine init_mask_3d(field, outmask, rmask_threshold, inmask, rmask, err_msg)
    class(*), intent(in) :: field(:,:,:,:)  !< Dummy variable whose sizes only in the first three
                                            !! dimensions are important
    logical, allocatable, intent(inout) :: outmask(:,:,:) !< Output logical mask
    class(*), intent(in) :: rmask_threshold !< Holds the values 0.5_r4_kind or 0.5_r8_kind, or related threhold values
                                          !! needed to be passed to the math/buffer update functions.
    logical, intent(in), optional :: inmask(:,:,:) !< Input logical mask
    class(*), intent(in), optional :: rmask(:,:,:) !< Floating point input mask value
    character(len=*), intent(out), optional :: err_msg !< Error message to relay back to caller

    character(len=256) :: err_msg_local !< Stores locally generated error message
    integer :: status !< Stores status of memory allocation call

    ! Initialize character strings
    err_msg_local = ''
    if (present(err_msg)) err_msg = ''

    ! Check if outmask is allocated
    if (allocated(outmask)) deallocate(outmask)
    ALLOCATE(outmask(SIZE(field, 1), SIZE(field, 2), SIZE(field, 3)), STAT=status)
    IF ( status .NE. 0 ) THEN
      WRITE (err_msg_local, FMT='("Unable to allocate outmask(",I5,",",I5,",",I5,"). (STAT: ",I5,")")')&
            & SIZE(field, 1), SIZE(field, 2), SIZE(field, 3), status
      if (fms_error_handler('fms_diag_reduction_methods_mod::init_mask_3d', trim(err_msg_local), err_msg)) then
        return
      end if
    END IF

    IF ( PRESENT(inmask) ) THEN
      outmask = inmask
    ELSE
      outmask = .TRUE.
    END IF

    IF ( PRESENT(rmask) ) THEN
      SELECT TYPE (rmask)
        TYPE IS (real(kind=r4_kind))
          select type (rmask_threshold)
          type is (real(kind=r4_kind))
            WHERE (rmask < rmask_threshold) outmask = .FALSE.
          class default
            call mpp_error(FATAL, 'fms_diag_reduction_methods_mod::init_mask_3d'//&
              ' types of rmask and rmask_threshold do not match')
          end select
        TYPE IS (real(kind=r8_kind))
          select type (rmask_threshold)
          type is (real(kind=r8_kind))
            WHERE (rmask < rmask_threshold) outmask = .FALSE.
          class default
            call mpp_error(FATAL, 'fms_diag_reduction_methods_mod::init_mask_3d'//&
              ' types of rmask and rmask_threshold do not match')
          end select
        CLASS DEFAULT
          call mpp_error(FATAL, 'fms_diag_reduction_methods_mod::init_mask_3d'//&
            & ' The rmask is not one of the supported types of real(kind=4) or real(kind=8)')
      END SELECT
    END IF
  end subroutine init_mask_3d

  !> @brief Sets the logical mask based on mask or rmask
  !> @return logical mask
  function init_mask(rmask, mask, field) &
  result(oor_mask)
    LOGICAL,  DIMENSION(:,:,:,:), allocatable, INTENT(in) :: mask  !< The location of the mask
    CLASS(*), DIMENSION(:,:,:,:), allocatable, INTENT(in) :: rmask !< The masking values
    CLASS(*), DIMENSION(:,:,:,:),          intent(in) :: field !< Field_data

    logical, allocatable, dimension(:,:,:,:) :: oor_mask !< mask

    ALLOCATE(oor_mask(SIZE(field, 1), SIZE(field, 2), SIZE(field, 3), SIZE(field, 4)))
    oor_mask = .true.

    if (allocated(mask)) then
      oor_mask = mask
    elseif (allocated(rmask)) then
      select type (rmask)
      type is (real(kind=r8_kind))
        WHERE (rmask < 0.5_r8_kind) oor_mask = .FALSE.
      type is (real(kind=r4_kind))
        WHERE (rmask < 0.5_r4_kind) oor_mask = .FALSE.
      end select
    endif

  end function init_mask

  !> @brief Sets the weight based on the weight passed into send_data (1.0_r8_kind if the weight is not passed in)
  !! The weight will be saved as an r8 and converted to r4 as needed
  !! @return weight to use when averaging
  pure function set_weight(weight) &
  result(out_weight)
    CLASS(*), INTENT(in), OPTIONAL :: weight !< The weight use when averaging

    real(kind=r8_kind) :: out_weight

    out_weight = 1.0_r8_kind
    if (present(weight)) then
      select type(weight)
      type is (real(kind=r8_kind))
        out_weight = real(weight, kind = r8_kind)
      type is (real(kind=r4_kind))
        out_Weight = real(weight, kind = r8_kind)
      end select
    endif
  end function set_weight

#include "fms_diag_reduction_methods_r4.fh"
#include "fms_diag_reduction_methods_r8.fh"

end module fms_diag_reduction_methods_mod
!> @}
! close documentation grouping
