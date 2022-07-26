!> \author Ryan Mulhall 
!> \email ryan.mulhall@noaa.gov
!! \brief Contains routines for the buffer objects used by 
!!
!! \description Holds buffered data for fmsDiagVars_type objects
module fms_diag_buffer_object_mod

use platform_mod
use iso_c_binding
use fms_diag_axis_object_mod, only: diagDomain_t
use time_manager_mod, only: time_type
use mpp_mod, only: mpp_error, FATAL

implicit none

!> \brief Object that holds buffered data 
type fmsDiagBuffer_type
    class(*), dimension(:,:,:,:), pointer  :: buffer !< pointer to remap buffered data
    integer, allocatable, private          :: buffer_id(:)
    type(time_type), private               :: init_time
    character(len=:), allocatable, private :: interp_method
    integer                                :: frequency
    integer, allocatable, private          :: tile_count
    integer, pointer, dimension(:), private :: axis_ids
    class(diagDomain_t), pointer, private :: domain
    integer, allocatable, private :: area, volume
    class(*), allocatable, private :: missing_value
    class(*), allocatable, private :: data_RANGE(:)
        
    contains

    !TODO reductions, get_remapped_buffer_pointer(), get_buffer_data
    !procedure :: get_buffer_data

end type fmsDiagBuffer_type

!> Scalar buffer type to extend fmsDiagBuffer_type
type buffer0d
    class(*), allocatable :: buffer
    !type(fmsDiagBuffer_type), pointer :: obj

    contains

    ! TODO allocatate, flush, init, remap, add, get
    procedure :: allocate_buffer => allocate_buffer_0d

end type buffer0d

!> 1D buffer type to extend fmsDiagBuffer_type
type buffer1d
    class(*), allocatable :: buffer(:)
end type buffer1d

!> 2D buffer type to extend fmsDiagBuffer_type
type buffer2d
    class(*), allocatable :: buffer(:,:)
end type buffer2d

!> 3D buffer type to extend fmsDiagBuffer_type
type buffer3d
    class(*), allocatable :: buffer(:,:,:)
end type buffer3d

!> 4D buffer type to extend fmsDiagBuffer_type
type buffer4d
    class(*), allocatable :: buffer(:,:,:,:)
end type buffer4d

!> 5D buffer type to extend fmsDiagBuffer_type
type buffer5d
    class(*), allocatable :: buffer(:,:,:,:,:)
end type buffer5d

! public types
public :: fmsDiagBuffer_type


! Module variables
logical,private :: module_is_initialized = .false. !< Flag indicating if the module is initialized
TYPE(fmsDiagBuffer_type), private, ALLOCATABLE, target :: buffer_objs(:) !< Array of buffer objects
integer, private :: num_buffers !< Number of registered variables

contains

logical function allocate_buffer_0d (buffobj, mold)
    class (buffer0d), intent(inout) :: buffobj !< scalar buffer object
    class(*),intent(in) :: mold !< allocates to the type of mold
    select type (mold)
        type is (integer(kind=i4_kind))
            allocate(integer(kind=i4_kind) :: buffobj%buffer)
        type is (integer(kind=i8_kind))
            allocate(integer(kind=i8_kind) :: buffobj%buffer)
        type is (real(kind=r4_kind))
            allocate(integer(kind=r4_kind) :: buffobj%buffer)
        type is (real(kind=r8_kind))
            allocate(integer(kind=r8_kind) :: buffobj%buffer)
        class default
             call mpp_error("allocate_buffer_0d", &
                     "The mold value passed to allocate a buffer is not a r8, r4, i8, or i4",&
                     FATAL)
    end select
    allocate_buffer_0d = allocated(buffobj%buffer)
end function

end module fms_diag_buffer_object_mod
