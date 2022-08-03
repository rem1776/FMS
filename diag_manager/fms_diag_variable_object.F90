!> @author Ryan Mulhall
!> @email ryan.mulhall@noaa.gov
!! @brief Contains types/routines for diag varaible objects used in fmsDiagObject_type
!!
!! Holds variable data for fmsDiagObject_type objects
module fms_diag_variable_object_mod

use platform_mod
use iso_c_binding
use time_manager_mod, only: time_type
use fms_diag_yaml_mod, only: diagYamlFilesVar_type
use fms_diag_axis_object_mod, only: diagDomain_t
use fms_diag_buffer_object_mod, only: fmsDiagBuffer_type
use diag_data_mod, only: DIAG_NULL, DIAG_NULL_STRING, DIAG_NOT_REGISTERED

implicit none

!> \brief Object to represent a diag variable as part of a diag object
!! holds data about variable, and a buffer type with the numerical actual data
type fmsDiagVariable_type

    type (diagYamlFilesVar_type), pointer :: diag_var
    integer, allocatable, private :: var_id
    character(len=:), allocatable, dimension(:) :: metadata
    logical, allocatable, private :: static
    logical, allocatable, private :: registered
    logical, allocatable, private :: mask_variant
    logical, allocatable, private :: do_not_log
    logical, allocatable, private :: local
    TYPE(time_type), private :: init_time
    integer, allocatable, private :: vartype
    character(len=:), allocatable, private :: varname
    character(len=:), allocatable, private :: longname
    character(len=:), allocatable, private :: standname
    character(len=:), allocatable, private :: units
    character(len=:), allocatable, private :: modname
    integer, pointer, dimension(:), private :: axis_ids
    class(diagDomain_t), pointer, private :: domain
    INTEGER , private :: type_of_domain
    !!!!! dimension and id
    integer, dimension(:,:), allocatable :: buffer_ids
    class(fmsDiagBuffer_type), pointer :: buffer_object!(:)???

    ! not needed or in buffer type
    !integer, allocatable, private :: area, volume
    !class(*), allocatable, private :: missing_value
    !class(*), allocatable, private :: data_RANGE(:)
    !character(len=:), allocatable, private :: realm
    !character(len=:), allocatable, private :: interp_method (in metadata)
    !integer, allocatable, dimension(:), private :: frequency
    !integer, allocatable, private :: tile_count

    contains

    !TODO getters/has
    !TODO diag_register_var_object
    !TODO diag_register_buffer_to_variable(bufferID)

! Check functions TODO
    !procedure :: is_static => diag_obj_is_static
    !procedure :: is_registered => diag_ob_registered
    !procedure :: is_registeredB => diag_obj_is_registered
    !procedure :: is_mask_variant => get_mask_variant
    !procedure :: is_local => get_local
! Is variable allocated check functions
    procedure :: has_var_id
    procedure :: has_metadata
    procedure :: has_static
    procedure :: has_registered
    procedure :: has_mask_variant
    procedure :: has_local
    procedure :: has_vartype
    procedure :: has_varname
    procedure :: has_longname
    procedure :: has_standname
    procedure :: has_units
    procedure :: has_modname
! Get functions
    procedure :: get_var_id
    procedure :: get_metadata
    procedure :: get_static
    procedure :: get_registered
    procedure :: get_mask_variant
    procedure :: get_local
    procedure :: get_vartype
    procedure :: get_varname
    procedure :: get_longname
    procedure :: get_standname
    procedure :: get_units
    procedure :: get_modname

end type fmsDiagVariable_type

! Module variables
logical,private :: module_is_initialized = .false. !< Flag indicating if the module is initialized

!!integer, private :: num_vars !< Number of registered variables

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Get functions

!> @brief Gets metadata string
!! @return copy of metadata string array, or a single space if metadata is not allocated
pure function get_metadata (obj) &
result(rslt)
    class (fmsDiagVariable_type), intent(in) :: obj !< diag object
    character(len=:), allocatable, dimension(:) :: rslt
     if (allocated(obj%metadata)) then
       allocate(character(len=(len(obj%metadata))) :: rslt (size(obj%metadata)) )
       rslt = obj%metadata
     else
       allocate(character(len=1) :: rslt(1:1))
       rslt = diag_null_string
     endif
end function get_metadata
!> @brief Gets static
!! @return copy of variable static
pure function get_static (obj) &
result(rslt)
     class (fmsDiagVariable_type), intent(in) :: obj !< diag object
     logical :: rslt
     rslt = obj%static
end function get_static
!> @brief Gets regisetered
!! @return copy of registered
pure function get_registered (obj) &
result(rslt)
     class (fmsDiagVariable_type), intent(in) :: obj !< diag object
     logical :: rslt
     rslt = obj%registered
end function get_registered
!> @brief Gets mask variant
!! @return copy of mask variant
pure function get_mask_variant (obj) &
result(rslt)
     class (fmsDiagVariable_type), intent(in) :: obj !< diag object
     logical :: rslt
     rslt = obj%mask_variant
end function get_mask_variant
!> @brief Gets local
!! @return copy of local
pure function get_local (obj) &
result(rslt)
     class (fmsDiagVariable_type), intent(in) :: obj !< diag object
     logical :: rslt
     rslt = obj%local
end function get_local
!> @brief Gets initial time
!! @return copy of the initial time
!! TODO
!function get_init_time (obj) &
!result(rslt)
!     class (fmsDiagVariable_type), intent(in) :: obj !< diag object
!     TYPE(time_type) :: rslt
!
!end function get_init_time
!> @brief Gets vartype
!! @return copy of The integer related to the variable type
pure function get_vartype (obj) &
result(rslt)
     class (fmsDiagVariable_type), intent(in) :: obj !< diag object
     integer :: rslt
     rslt = obj%vartype
end function get_vartype
!> @brief Gets varname
!! @return copy of the variable name
pure function get_varname (obj) &
result(rslt)
     class (fmsDiagVariable_type), intent(in) :: obj !< diag object
     character(len=:), allocatable :: rslt
     rslt = obj%varname
end function get_varname
!> @brief Gets longname
!! @return copy of the variable long name or a single string if there is no long name
pure function get_longname (obj) &
result(rslt)
     class (fmsDiagVariable_type), intent(in) :: obj !< diag object
     character(len=:), allocatable :: rslt
     if (allocated(obj%longname)) then
       rslt = obj%longname
     else
       rslt = diag_null_string
     endif
end function get_longname
!> @brief Gets standname
!! @return copy of the standard name or an empty string if standname is not allocated
pure function get_standname (obj) &
result(rslt)
     class (fmsDiagVariable_type), intent(in) :: obj !< diag object
     character(len=:), allocatable :: rslt
     if (allocated(obj%standname)) then
       rslt = obj%standname
     else
       rslt = diag_null_string
     endif
end function get_standname
!> @brief Gets units
!! @return copy of the units or an empty string if not allocated
pure function get_units (obj) &
result(rslt)
     class (fmsDiagVariable_type), intent(in) :: obj !< diag object
     character(len=:), allocatable :: rslt
     if (allocated(obj%units)) then
       rslt = obj%units
     else
       rslt = diag_null_string
     endif
end function get_units
!> @brief Gets modname
!! @return copy of the module name that the variable is in or an empty string if not allocated
pure function get_modname (obj) &
result(rslt)
     class (fmsDiagVariable_type), intent(in) :: obj !< diag object
     character(len=:), allocatable :: rslt
     if (allocated(obj%modname)) then
       rslt = obj%modname
     else
       rslt = diag_null_string
     endif
end function get_modname

!> @brief Returns the variable ID for a given variable object
!> @return the variable ID number
integer function get_var_id (obj) result(var_id)
 class(fmsDiagVariable_type)     , intent(inout)            :: obj

 !> Checks if the diag_object registration has been done
 if (allocated(obj%registered)) then
         !> Returns the diag_id if the variable has been registered
         var_id = obj%var_id
 else
!> If the variable is not registered, then returns the unregistered value
        var_id = DIAG_NOT_REGISTERED
 endif
end function get_var_id
!> @brief Gets axis
!! @return copy of axis information
!! TODO
!function get_axis (obj) &
!result(rslt)
!     class (fmsDiagVariable_type), intent(in) :: obj !< diag object
!     type (diag_axis_type), allocatable, dimension(:) :: rslt
!
!end function get_axis
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!! Allocation checks
!!> @brief Checks if obj%diag_field is allocated
!!! @return true if obj%diag_field is allocated
!logical function has_diag_field (obj)
!  class (fmsDiagVariable_type), intent(in) :: obj !< diag object
!  has_diag_field = allocated(obj%diag_field)
!end function has_diag_field

!> @brief Checks if obj%var_id is allocated
!! @return true if obj%var_id is allocated
pure logical function has_var_id (obj)
  class (fmsDiagVariable_type), intent(in) :: obj !< diag object
  has_var_id = allocated(obj%var_id)
end function has_var_id
!> @brief Checks if obj%metadata is allocated
!! @return true if obj%metadata is allocated
pure logical function has_metadata (obj)
  class (fmsDiagVariable_type), intent(in) :: obj !< diag object
  has_metadata = allocated(obj%metadata)
end function has_metadata
!> @brief Checks if obj%static is allocated
!! @return true if obj%static is allocated
pure logical function has_static (obj)
  class (fmsDiagVariable_type), intent(in) :: obj !< diag object
  has_static = allocated(obj%static)
end function has_static
!> @brief Checks if obj%registered is allocated
!! @return true if obj%registered is allocated
pure logical function has_registered (obj)
  class (fmsDiagVariable_type), intent(in) :: obj !< diag object
  has_registered = allocated(obj%registered)
end function has_registered
!> @brief Checks if obj%mask_variant is allocated
!! @return true if obj%mask_variant is allocated
pure logical function has_mask_variant (obj)
  class (fmsDiagVariable_type), intent(in) :: obj !< diag object
  has_mask_variant = allocated(obj%mask_variant)
end function has_mask_variant
!> @brief Checks if obj%local is allocated
!! @return true if obj%local is allocated
pure logical function has_local (obj)
  class (fmsDiagVariable_type), intent(in) :: obj !< diag object
  has_local = allocated(obj%local)
end function has_local
!!> @brief Checks if obj%init_time is allocated
!!! @return true if obj%init_time is allocated
!logical function has_init_time (obj)
!  class (fmsDiagVariable_type), intent(in) :: obj !< diag object
!  has_init_time = allocated(obj%init_time)
!end function has_init_time
!> @brief Checks if obj%vartype is allocated
!! @return true if obj%vartype is allocated
pure logical function has_vartype (obj)
  class (fmsDiagVariable_type), intent(in) :: obj !< diag object
  has_vartype = allocated(obj%vartype)
end function has_vartype
!> @brief Checks if obj%varname is allocated
!! @return true if obj%varname is allocated
pure logical function has_varname (obj)
  class (fmsDiagVariable_type), intent(in) :: obj !< diag object
  has_varname = allocated(obj%varname)
end function has_varname
!> @brief Checks if obj%longname is allocated
!! @return true if obj%longname is allocated
pure logical function has_longname (obj)
  class (fmsDiagVariable_type), intent(in) :: obj !< diag object
  has_longname = allocated(obj%longname)
end function has_longname
!> @brief Checks if obj%standname is allocated
!! @return true if obj%standname is allocated
pure logical function has_standname (obj)
  class (fmsDiagVariable_type), intent(in) :: obj !< diag object
  has_standname = allocated(obj%standname)
end function has_standname
!> @brief Checks if obj%units is allocated
!! @return true if obj%units is allocated
pure logical function has_units (obj)
  class (fmsDiagVariable_type), intent(in) :: obj !< diag object
  has_units = allocated(obj%units)
end function has_units
!> @brief Checks if obj%modname is allocated
!! @return true if obj%modname is allocated
pure logical function has_modname (obj)
  class (fmsDiagVariable_type), intent(in) :: obj !< diag object
  has_modname = allocated(obj%modname)
end function has_modname

!> @brief TODO ID? 
subroutine diag_register_buffer_to_variable(obj, bufferID)
    class( fmsDiagVariable_type ), intent(inout) :: obj
    integer, intent(in) :: bufferID

     

end subroutine

end module fms_diag_variable_object_mod
