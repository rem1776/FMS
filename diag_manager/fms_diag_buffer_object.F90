!> @author Ryan Mulhall
!> @email ryan.mulhall@noaa.gov
!! @brief Contains buffer types and routines for the diag manager
!!
!! @description Holds buffered data for fmsDiagVars_type objects
module fms_diag_buffer_object_mod

use platform_mod
use iso_c_binding
use fms_diag_axis_object_mod, only: diagDomain_t
use time_manager_mod, only: time_type
use mpp_mod, only: mpp_error, FATAL
use diag_data_mod, only: DIAG_NULL

implicit none

!> @brief Object that holds buffered data and other diagnostics
!! Abstract to ensure use through its extensions(buffer0-5d types)
type, abstract :: fmsDiagBuffer_type
    class(*), dimension(:,:,:,:,:), allocatable :: remap_buffer !< remapped buffer data
    integer, allocatable, private          :: buffer_id ! (:)
    type(time_type), private               :: init_time
    character(len=128), allocatable, private :: interp_method
    integer                                :: frequency
    integer, allocatable, private          :: tile_count
    integer, pointer, dimension(:), private :: axis_ids
    class(diagDomain_t), pointer, private :: domain
    integer, allocatable, private :: area, volume
    class(*), allocatable, private :: missing_value
    class(*), allocatable, private :: data_RANGE(:)

    contains

    !TODO reductions, get_remapped_buffer_pointer()
    procedure :: get_remapped_buffer_pointer
    procedure :: get_area
    procedure :: get_volume
    procedure :: get_missing_value
    procedure :: get_data_RANGE
    ! deferred routines defined by each type
    procedure(allocate_buffer), deferred :: allocate_buffer
    !procedure, deferred :: get_buffer 
    !procedure, deferred :: initialize_buffer

end type fmsDiagBuffer_type


!> Scalar buffer type to extend fmsDiagBuffer_type
type, extends(fmsDiagBuffer_type) :: buffer0d
    class(*), allocatable :: buffer
    contains
    ! TODO flush, add
    procedure :: allocate_buffer => allocate_buffer_0d
    procedure :: get_buffer => get_buffer_0d
    procedure :: initialize_buffer => initialize_buffer_0d

end type buffer0d

!> 1D buffer type to extend fmsDiagBuffer_type
type, extends(fmsDiagBuffer_type) :: buffer1d
    class(*), allocatable :: buffer(:)
    contains
    ! TODO flush, init, remap, add, get
    procedure :: allocate_buffer => allocate_buffer_1d
    procedure :: get_buffer => get_buffer_1d
    procedure :: initialize_buffer => initialize_buffer_1d
end type buffer1d

!> 2D buffer type to extend fmsDiagBuffer_type
type, extends(fmsDiagBuffer_type) :: buffer2d
    class(*), allocatable :: buffer(:,:)
    contains
    ! TODO flush, init, remap, add, get
    procedure :: allocate_buffer => allocate_buffer_2d
    procedure :: get_buffer => get_buffer_2d
    procedure :: initialize_buffer => initialize_buffer_2d
end type buffer2d

!> 3D buffer type to extend fmsDiagBuffer_type
type, extends(fmsDiagBuffer_type) :: buffer3d
    class(*), allocatable :: buffer(:,:,:)
    contains
    ! TODO allocate, flush, init, remap, add, get
    !procedure :: allocate_buffer => allocate_buffer_3d
end type buffer3d

!> 4D buffer type to extend fmsDiagBuffer_type
type, extends(fmsDiagBuffer_type) :: buffer4d
    class(*), allocatable :: buffer(:,:,:,:)
    contains
    !! TODO allocate, flush, init, remap, add, get
    !procedure :: allocate_buffer => allocate_buffer_4d
end type buffer4d

!> 5D buffer type to extend fmsDiagBuffer_type
type, extends(fmsDiagBuffer_type) :: buffer5d
    class(*), allocatable :: buffer(:,:,:,:,:)
    contains
    ! TODO  allocate, flush, init, remap, add, get
    !!procedure :: allocate_buffer => allocate_buffer_5d
end type buffer5d


! public types
public :: buffer0d
public :: buffer1d
public :: buffer2d
public :: buffer3d
public :: buffer4d
public :: buffer5d


! Module variables
logical,private :: module_is_initialized = .false. !< Flag indicating if the module is initialized
integer, private :: num_buffers = 0 !< Number of available buffers

!> this ?
class(fmsDiagBuffer_type), private, ALLOCATABLE, target :: buffer_objs(:,:) !< Array of buffer objects
!> or this?
!class(buffer0d), private, ALLOCATABLE, target :: buffer_objs_0d(:) !< Array of buffer objects
!class(buffer1d), private, ALLOCATABLE, target :: buffer_objs_1d(:) !< Array of buffer objects
!class(buffer2d), private, ALLOCATABLE, target :: buffer_objs_2d(:) !< Array of buffer objects
!class(buffer3d), private, ALLOCATABLE, target :: buffer_objs_3d(:) !< Array of buffer objects
!class(buffer4d), private, ALLOCATABLE, target :: buffer_objs_4d(:) !< Array of buffer objects
!class(buffer5d), private, ALLOCATABLE, target :: buffer_objs_5d(:) !< Array of buffer objects

logical, parameter, private :: DEBUG = .true. !< debugging output

! interface with function declarations needed for deferred routines
interface allocate_buffer
    integer function allocate_buffer_0d(buffobj, mold) &
    result(rslt)
        import buffer0d 
        !import time_type 
        class(buffer0d), intent(inout) :: buffobj !< scalar buffer object
        class(*),intent(in) :: mold !< allocates to the type of mold
        !type(time_type), intent(in), optional :: init_time
    end function
end interface

contains


!!--------module routines

!> Gets a buffer from a given id
function get_buffer_object(id, dimensions) &
result(rslt)
    integer :: id
    integer :: dimensions 
    class(fmsDiagBuffer_type), pointer :: rslt 

    rslt => buffer_objs(dimensions, id)
end function 


!!--------generic routines for any fmsDiagBuffer_type objects 

!> Remaps 0-5d data buffer from the given object onto a 5d array and sets it in the type
subroutine remap_buffer(buffobj)
    class(fmsDiagBuffer_type), intent(inout) :: buffobj
    !class(*), allocatable, target            :: remap_tmp(:,:,:,:,:)
    integer                                  :: buff_bounds(5)

    if( DEBUG) print *, 'remapping buffer'
    ! get num dimensions from type extension
    select type (buffobj)
        type is (buffer0d)
            if (DEBUG) print *, '0d buffer'
            ! get buffer data type to allocate and remap
            select type (buff => buffobj%buffer)
                type is(integer(i4_kind))
                    allocate (integer(kind=i4_kind) :: buffobj%remap_buffer(1,1,1,1,1))
                    buffobj%remap_buffer = buff
                type is(integer(i8_kind))
                    allocate (integer(kind=i8_kind) :: buffobj%remap_buffer(1,1,1,1,1))
                    buffobj%remap_buffer = buff
                type is(real(r4_kind))
                    allocate (real(kind=r4_kind) :: buffobj%remap_buffer(1,1,1,1,1))
                    buffobj%remap_buffer = buff
                type is(real(r8_kind))
                    allocate (real(kind=r8_kind) :: buffobj%remap_buffer(1,1,1,1,1))
                    buffobj%remap_buffer = buff 
            end select
        type is (buffer1d)
            if( DEBUG) print *, '1d buffer'
            buff_bounds(1) = size(buffobj%buffer, 1)
            select type (buff => buffobj%buffer)
                type is(integer(i4_kind))
                    allocate (integer(kind=i4_kind) :: buffobj%remap_buffer(buff_bounds(1),1,1,1,1))
                    buffobj%remap_buffer = RESHAPE( buff, (/ buff_bounds(1),1,1,1,1 /))
                type is(integer(i8_kind))
                    allocate (integer(kind=i8_kind) :: buffobj%remap_buffer(buff_bounds(1),1,1,1,1))
                    buffobj%remap_buffer = RESHAPE( buff, (/ buff_bounds(1),1,1,1,1 /))
                type is(real(r4_kind))
                    allocate (real(kind=r4_kind) :: buffobj%remap_buffer(buff_bounds(1),1,1,1,1))
                    buffobj%remap_buffer = RESHAPE( buff, (/ buff_bounds(1),1,1,1,1 /))
                type is(real(r8_kind))
                    allocate (real(kind=r8_kind) :: buffobj%remap_buffer(buff_bounds(1),1,1,1,1))
                    buffobj%remap_buffer = RESHAPE( buff, (/ buff_bounds(1),1,1,1,1 /))
            end select
        type is (buffer2d)
            buff_bounds(1) = size(buffobj%buffer, 1)
            buff_bounds(2) = size(buffobj%buffer, 2)
            select type (buff => buffobj%buffer)
                type is(integer(i4_kind))
                    allocate (integer(kind=i4_kind) :: buffobj%remap_buffer(buff_bounds(1),buff_bounds(2),1,1,1))
                    buffobj%remap_buffer = RESHAPE( buff, (/ buff_bounds(1),buff_bounds(2),1,1,1 /))
                type is(integer(i8_kind))
                    allocate (integer(kind=i8_kind) :: buffobj%remap_buffer(buff_bounds(1),buff_bounds(2),1,1,1))
                    buffobj%remap_buffer = RESHAPE( buff, (/ buff_bounds(1),buff_bounds(2),1,1,1 /))
                type is(real(r4_kind))
                    allocate (real(kind=r4_kind) :: buffobj%remap_buffer(buff_bounds(1),buff_bounds(2),1,1,1))
                    buffobj%remap_buffer = RESHAPE( buff, (/ buff_bounds(1),buff_bounds(2),1,1,1 /))
                type is(real(r8_kind))
                    allocate (real(kind=r8_kind) :: buffobj%remap_buffer(buff_bounds(1),buff_bounds(2),1,1,1))
                    buffobj%remap_buffer = RESHAPE( buff, (/ buff_bounds(1),buff_bounds(2),1,1,1 /))
            end select
        type is (buffer3d)
        type is (buffer4d)
        type is (buffer5d)
        class default
            call mpp_error( FATAL, 'remap_buffer_pointer: invalid buffer type for remapping')
    end select
   
!    remap_tmp => remap_tmp

end subroutine 

!> @brief Gets the remapped buffer pointer
!! Will do the remapping if not set already
function get_remapped_buffer_pointer (obj) &
result(rslt)
    class(fmsDiagBuffer_type), target, intent(inout) :: obj
    class(fmsDiagBuffer_type), pointer       :: objptr
    class(*), pointer, dimension(:,:,:,:,:)  :: rslt
    ! TODO check for changes to buffer instead of allocation?
    if(.not. allocated(obj%remap_buffer)) then
        call remap_buffer(obj) 
    endif
    objptr => obj
    rslt => objptr%remap_buffer
end function

!> @brief Gets area
!! @return copy of the area or diag_null if not allocated
pure function get_area (obj) &
result(rslt)
     class (fmsDiagBuffer_type), intent(in) :: obj !< diag object
     integer :: rslt
     if (allocated(obj%area)) then
       rslt = obj%area
     else
       rslt = diag_null
     endif
end function get_area
!> @brief Gets volume
!! @return copy of the volume or diag_null if volume is not allocated
pure function get_volume (obj) &
result(rslt)
     class (fmsDiagBuffer_type), intent(in) :: obj !< diag object
     integer :: rslt
     if (allocated(obj%volume)) then
       rslt = obj%volume
     else
       rslt = diag_null
     endif
end function get_volume
!> @brief Gets missing_value
!! @return copy of The missing value
function get_missing_value (obj) &
result(rslt)
     class (fmsDiagBuffer_type), intent(in) :: obj !< diag object
     class(*),allocatable :: rslt
     if (allocated(obj%missing_value)) then
       select type (miss => obj%missing_value)
         type is (integer(kind=i4_kind))
             allocate (integer(kind=i4_kind) :: rslt)
             rslt = miss
         type is (integer(kind=i8_kind))
             allocate (integer(kind=i8_kind) :: rslt)
             rslt = miss
         type is (real(kind=r4_kind))
             allocate (integer(kind=i4_kind) :: rslt)
             rslt = miss
         type is (real(kind=r8_kind))
             allocate (integer(kind=i4_kind) :: rslt)
             rslt = miss
         class default
             call mpp_error ("get_missing_value", &
                     "The missing value is not a r8, r4, i8, or i4",&
                     FATAL)
         end select
       else
         call mpp_error ("get_missing_value", &
                 "The missing value is not allocated", FATAL)
       endif
end function get_missing_value
!> @brief Gets data_range
!! @return copy of the data range
function get_data_RANGE (obj) &
result(rslt)
     class (fmsDiagBuffer_type), intent(in) :: obj !< diag object
     class(*),allocatable :: rslt(:)
     if (allocated(obj%data_RANGE)) then
       select type (r => obj%data_RANGE)
         type is (integer(kind=i4_kind))
             allocate (integer(kind=i4_kind) :: rslt(2))
             rslt = r
         type is (integer(kind=i8_kind))
             allocate (integer(kind=i8_kind) :: rslt(2))
             rslt = r
         type is (real(kind=r4_kind))
             allocate (integer(kind=i4_kind) :: rslt(2))
             rslt = r
         type is (real(kind=r8_kind))
             allocate (integer(kind=i4_kind) :: rslt(2))
             rslt = r
         class default
             call mpp_error ("get_data_RANGE", &
                     "The data_RANGE value is not a r8, r4, i8, or i4",&
                     FATAL)
         end select
       else
         call mpp_error ("get_data_RANGE", &
                 "The data_RANGE value is not allocated", FATAL)
       endif
end function get_data_RANGE


!! -----------Type-specific routines for buffer0-5d 

!! allocations could be done in one routine for 0-5d if buffobj is changed to fmsDiagBuffer_type
!! not sure which approach would be better

!> allocates a scalar buffer to given mold type
!> @returns buffer id for 
integer function allocate_buffer_0d(buffobj, mold) &
result(rslt)
    class(buffer0d), intent(inout) :: buffobj !< scalar buffer object
    class(*),intent(in) :: mold !< allocates to the type of mold
    !type(time_type), intent(in), optional :: init_time

    select type (mold)
        type is (integer(kind=i4_kind))
            allocate(integer(kind=i4_kind) :: buffobj%buffer)
            allocate(integer(kind=i4_kind) :: buffobj%missing_value)
            !allocate(buffobj%data_RANGE) size?
        type is (integer(kind=i8_kind))
            allocate(integer(kind=i8_kind) :: buffobj%buffer)
            allocate(integer(kind=i8_kind) :: buffobj%missing_value)
            !allocate(buffobj%data_RANGE) size?
        type is (real(kind=r4_kind))
            allocate(real(kind=r4_kind) :: buffobj%buffer)
            allocate(real(kind=r4_kind) :: buffobj%missing_value)
            !allocate(buffobj%data_RANGE) size?
        type is (real(kind=r8_kind))
            allocate(real(kind=r8_kind) :: buffobj%buffer)
            allocate(real(kind=r8_kind) :: buffobj%missing_value)
            !allocate(buffobj%data_RANGE) size?
        class default
             call mpp_error("allocate_buffer_0d", &
                     "The mold value passed to allocate a buffer is not a r8, r4, i8, or i4",&
                     FATAL)
    end select

    allocate(buffobj%buffer_id)
    allocate(buffobj%interp_method)
    allocate(buffobj%tile_count)
    allocate(buffobj%area)
    allocate(buffobj%volume)

    if( allocated(buffobj%buffer) ) then
        !! this will need a critical section
        num_buffers = num_buffers + 1
        !buffobj%bu
        rslt = num_buffers 
        buffobj%buffer_id = num_buffers
    else
        rslt = -1
    endif
end function

!> allocates a 1D buffer to given mold type
integer function allocate_buffer_1d(buffobj, mold, size) &
result(rslt)
    class(buffer1d), intent(inout) :: buffobj !< scalar buffer object
    class(*),intent(in) :: mold !< allocates to the type of mold
    integer, intent(in) :: size !< dimension bounds

    select type (mold)
        type is (integer(kind=i4_kind))
            allocate(integer(kind=i4_kind) :: buffobj%buffer(size))
        type is (integer(kind=i8_kind))
            allocate(integer(kind=i8_kind) :: buffobj%buffer(size))
        type is (real(kind=r4_kind))
            allocate(real(kind=r4_kind) :: buffobj%buffer(size))
        type is (real(kind=r8_kind))
            allocate(real(kind=r8_kind) :: buffobj%buffer(size))
        class default
             call mpp_error("allocate_buffer_1d", &
                     "The mold value passed to allocate a buffer is not a r8, r4, i8, or i4",&
                     FATAL)
    end select

    allocate(buffobj%buffer_id)
    allocate(buffobj%interp_method)
    allocate(buffobj%tile_count)
    allocate(buffobj%area)
    allocate(buffobj%volume)

    if( allocated(buffobj%buffer) ) then
        !! this will need a critical section
        num_buffers = num_buffers + 1
        !buffobj%bu
        rslt = num_buffers
        buffobj%buffer_id = num_buffers
    else
        rslt = -1
    endif
end function

!> allocates a 2D buffer to given mold type
logical function allocate_buffer_2d(buffobj, mold, sizes) &
result(rslt)
    class(buffer2d), intent(inout) :: buffobj !< 2D buffer object
    class(*),intent(in) :: mold !< allocates to the type of mold
    integer, intent(in) :: sizes(2) !< dimension bounds

    select type (mold)
        type is (integer(kind=i4_kind))
            allocate(integer(kind=i4_kind) :: buffobj%buffer( sizes(1),sizes(2)))
        type is (integer(kind=i8_kind))
            allocate(integer(kind=i8_kind) :: buffobj%buffer( sizes(1),sizes(2)))
        type is (real(kind=r4_kind))
            allocate(real(kind=r4_kind) :: buffobj%buffer( sizes(1),sizes(2)))
        type is (real(kind=r8_kind))
            allocate(real(kind=r8_kind) :: buffobj%buffer( sizes(1),sizes(2)))
        class default
             call mpp_error("allocate_buffer_2d", &
                     "The mold value passed to allocate a buffer is not a r8, r4, i8, or i4",&
                     FATAL)
    end select
    rslt = allocated(buffobj%buffer)
end function

!> @brief Gets buffer data from buffer0d type
!! @return copy of the buffer data
function get_buffer_0d (obj) &
result(rslt)
    class (buffer0d), intent(in) :: obj
    class(*), allocatable :: rslt
    if (allocated(obj%buffer)) then
        rslt = obj%buffer
    else
        call mpp_error(FATAL, 'get_buffer_0d: buffer not allocated')
    endif
end function
!> @brief Gets buffer data from buffer1d type
!! @return copy of the buffer data
function get_buffer_1d (obj) &
result(rslt)
    class (buffer1d), intent(in) :: obj
    class(*), allocatable :: rslt(:)
    if (allocated(obj%buffer)) then
        rslt = obj%buffer
    else
        call mpp_error(FATAL, 'get_buffer_1d: buffer not allocated')
    endif
end function
!> @brief Gets buffer data from buffer2d type
!! @return copy of the buffer data
function get_buffer_2d (obj) &
result(rslt)
    class (buffer2d), intent(in) :: obj
    class(*), allocatable :: rslt(:,:)
    if (allocated(obj%buffer)) then
        rslt = obj%buffer
    else
        call mpp_error(FATAL, 'get_buffer_2d: buffer not allocated')
    endif
end function
!> @brief Gets buffer data from buffer3d type
!! @return copy of the buffer data
function get_buffer_3d (obj) &
result(rslt)
    class (buffer3d), intent(in) :: obj
    class(*), allocatable :: rslt(:,:,:)
    if (allocated(obj%buffer)) then
        rslt = obj%buffer
    else
        call mpp_error(FATAL, 'get_buffer_3d: buffer not allocated')
    endif
end function
!> @brief Gets buffer data from buffer4d type
!! @return copy of the buffer data
function get_buffer_4d (obj) &
result(rslt)
    class (buffer4d), intent(in) :: obj
    class(*), allocatable :: rslt(:,:,:,:)
    if (allocated(obj%buffer)) then
        rslt = obj%buffer
    else
        call mpp_error(FATAL, 'get_buffer_4d: buffer not allocated')
    endif
end function
!> @brief Gets buffer data from buffer5d type
!! @return copy of the buffer data
function get_buffer_5d (obj) &
result(rslt)
    class (buffer5d), intent(in) :: obj
    class(*), allocatable :: rslt(:,:,:,:,:)
    if (allocated(obj%buffer)) then
        rslt = obj%buffer
    else
        call mpp_error(FATAL, 'get_buffer_5d: buffer not allocated')
    endif
end function

!> @brief Initializes a buffer to a given fill value
!! TODO default to 0?
subroutine initialize_buffer_0d (buffobj, fillval)
    class(buffer0d), intent(inout) :: buffobj !< any buffer object
    class(*) :: fillval

    if(.not. allocated(buffobj%buffer)) call mpp_error(FATAL, 'initialize_buffer_0d:' // &
            'buffer not yet allocated, allocate_buffer() must be called on this object first.')
    ! have to check fill value and buffer types match
    select type(buff => buffobj%buffer)
    type is(real(r8_kind))
        select type(fillval)
        type is(real(r8_kind))
            buff = fillval
        class default
            call mpp_error(FATAL, 'initialize_buffer_0d: mismatch between fill and buffer values')
        end select
    type is(real(r4_kind))
        select type(fillval)
        type is(real(r4_kind))
            buff = fillval
        class default
            call mpp_error(FATAL, 'initialize_buffer_0d: mismatch between fill and buffer values')
        end select
    type is(integer(i8_kind))
        select type(fillval)
        type is(integer(i8_kind))
            buff = fillval
        class default
            call mpp_error(FATAL, 'initialize_buffer_0d: mismatch between fill and buffer values')
        end select
    type is(integer(i4_kind))
        select type(fillval)
        type is(integer(i4_kind))
            buff = fillval
        class default
            call mpp_error(FATAL, 'initialize_buffer_0d: mismatch between fill and buffer values')
        end select
    class default
        call mpp_error(FATAL, 'initialize buffer_0d: buffer allocated to invalid data type, this shouldnt happen')
    end select

end subroutine

!> @brief Initializes a buffer to a given fill value
subroutine initialize_buffer_1d (buffobj, fillval)
    class(buffer1d), intent(inout) :: buffobj !< any buffer object
    class(*) :: fillval

    if(.not. allocated(buffobj%buffer)) call mpp_error(FATAL, 'initialize_buffer_0d:' // &
            'buffer not yet allocated, allocate_buffer() must be called on this object first.')
    ! have to check fill value and buffer types match
    select type(buff => buffobj%buffer)
    type is(real(r8_kind))
        select type(fillval)
        type is(real(r8_kind))
            buff = fillval
        class default
            call mpp_error(FATAL, 'initialize_buffer_0d: mismatch between fill and buffer values')
        end select
    type is(real(r4_kind))
        select type(fillval)
        type is(real(r4_kind))
            buff = fillval
        class default
            call mpp_error(FATAL, 'initialize_buffer_0d: mismatch between fill and buffer values')
        end select
    type is(integer(i8_kind))
        select type(fillval)
        type is(integer(i8_kind))
            buff = fillval
        class default
            call mpp_error(FATAL, 'initialize_buffer_0d: mismatch between fill and buffer values')
        end select
    type is(integer(i4_kind))
        select type(fillval)
        type is(integer(i4_kind))
            buff = fillval
        class default
            call mpp_error(FATAL, 'initialize_buffer_0d: mismatch between fill and buffer values')
        end select
    class default
        call mpp_error(FATAL, 'initialize buffer_0d: buffer allocated to invalid data type, this shouldnt happen')
    end select

end subroutine

!> @brief Initializes a buffer to a given fill value
subroutine initialize_buffer_2d (buffobj, fillval)
    class(buffer2d), intent(inout) :: buffobj !< any buffer object
    class(*) :: fillval

    if(.not. allocated(buffobj%buffer)) call mpp_error(FATAL, 'initialize_buffer_0d:' // &
            'buffer not yet allocated, allocate_buffer() must be called on this object first.')
    ! have to check fill value and buffer types match
    select type(buff => buffobj%buffer)
    type is(real(r8_kind))
        select type(fillval)
        type is(real(r8_kind))
            buff = fillval
        class default
            call mpp_error(FATAL, 'initialize_buffer_0d: mismatch between fill and buffer values')
        end select
    type is(real(r4_kind))
        select type(fillval)
        type is(real(r4_kind))
            buff = fillval
        class default
            call mpp_error(FATAL, 'initialize_buffer_0d: mismatch between fill and buffer values')
        end select
    type is(integer(i8_kind))
        select type(fillval)
        type is(integer(i8_kind))
            buff = fillval
        class default
            call mpp_error(FATAL, 'initialize_buffer_0d: mismatch between fill and buffer values')
        end select
    type is(integer(i4_kind))
        select type(fillval)
        type is(integer(i4_kind))
            buff = fillval
        class default
            call mpp_error(FATAL, 'initialize_buffer_0d: mismatch between fill and buffer values')
        end select
    class default
        call mpp_error(FATAL, 'initialize buffer_0d: buffer allocated to invalid data type, this shouldnt happen')
    end select

end subroutine

end module fms_diag_buffer_object_mod
