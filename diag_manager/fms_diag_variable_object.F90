!> \author Ryan Mulhall 
!> \email ryan.mulhall@noaa.gov
!! \brief Contains types/routines for the diag varaible objects used by fmsDiagObject_type
!!
!! \description Holds buffered data for fmsDiagObject_type objects
module fms_diag_variable_mod

use platform_mod
use iso_c_binding
use time_manager_mod, only: time_type
use fms_diag_yaml_mod, only: diagYamlFilesVar_type
use fms_diag_axis_object_mod, only: diagDomain_t
use fms_diag_buffer_object_mod, only: fmsDiagBuffer_type

implicit none

!> \brief Object that holds buffered data 
type fmsDiagVars_type

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
    !character(len=:), allocatable, private :: realm 
    !character(len=:), allocatable, private :: interp_method (in metadata)
    !integer, allocatable, dimension(:), private :: frequency
    !integer, allocatable, dimension(:), private :: output_units
    !integer, allocatable, private :: tile_count
    integer, pointer, dimension(:), private :: axis_ids
    class(diagDomain_t), pointer, private :: domain
    INTEGER , private :: type_of_domain
    !integer, allocatable, private :: area, volume
    !class(*), allocatable, private :: missing_value
    !class(*), allocatable, private :: data_RANGE(:)

    !!!!! dimension and id
    integer, dimension(:,:), allocatable :: buffer_ids
    class(fmsDiagBuffer_type), pointer :: buffer_object

    contains

    !TODO getters/has
    !TODO diag_register_var_object
    !TODO diag_register_buffer_to_variable(bufferID)


end type fmsDiagVars_type

! Module variables
logical,private :: module_is_initialized = .false. !< Flag indicating if the module is initialized
TYPE(fmsDiagVars_type), private, ALLOCATABLE, target :: var_objs(:) !< Array of buffer objects
integer, private :: num_vars !< Number of registered variables

contains


end module fms_diag_variable_mod
