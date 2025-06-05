module metadata_transfer_mod
  use platform_mod
  use netcdf
  use mpi
  use mpp_mod, only: mpp_pe, mpp_root_pe, mpp_error, FATAL

  implicit none
  private

  public :: real8_type, real4_type, int8_type, int4_type, str_type
  public :: metadata_type
  public :: fms_metadata_transfer_init, fms_metadata_broadcast

  ! TODO could probably use similar enumerated values from somewhere else
  integer, parameter :: real8_type = 1
  integer, parameter :: real4_type = 2
  integer, parameter :: int8_type = 3
  integer, parameter :: int4_type = 4
  integer, parameter :: str_type = 5

  integer, parameter :: ATTR_NAME_MAX_LENGTH = 50


  ! ordering of fields matters due to how the mpi declaration is done
  type :: metadata_type
    !private
    logical :: metadata_transfer_initialized = .false.
    integer :: metadata_transfer_type_mpi_id = -1
    integer :: attribute_type
    character(len=ATTR_NAME_MAX_LENGTH)  :: attribute_name
    !class(*), allocatable :: attribute_value(:)
    !! TODO allocatables don't seem to work!!
    !! based off the debugger output, it'll send over the memory address
    !! but no actual values
    !! might have to do
    real(r8_kind), allocatable :: attribute_value(:)
    contains
      procedure :: fms_metadata_transfer_init
      procedure :: fms_metadata_broadcast
      procedure :: get_attribute_type
      procedure :: get_attribute_value
      procedure :: set_attribute_value
      procedure :: get_attribute_name
      procedure :: set_attribute_name
  end type

contains

  !> Initialize the mpi datatype for future transfers
  !! TODO can do allocation here instead
  subroutine fms_metadata_transfer_init(this, type, length)
    class(metadata_type), intent(inout) :: this
    integer, intent(in)   :: type !< enumerated type value (real8_type, int4_type, str_type, etc.)
    integer, intent(in)   :: length !< length of the attributes data if array
    integer, dimension(0:4) :: lengths, types
    integer(KIND=MPI_ADDRESS_KIND), dimension(0:4) :: displacements
    integer :: ierror

    ! declares a mpi struct for metadata_type that can be sent between pes
    lengths = (/1, 1, 1, ATTR_NAME_MAX_LENGTH, length/)
    displacements = (/ 0, &
                       sizeof(.true.), &
                       sizeof(.true.)+sizeof(0), &
                       sizeof(.true.)+sizeof(0)+sizeof(0), &
                       sizeof(.true.)+sizeof(0)+sizeof(0)+(sizeof(this%attribute_name))/)
!                       sizeof(.true.)+sizeof(0)+sizeof(0)+(sizeof(' ')*ATTR_NAME_MAX_LENGTH)/)
    types = (/MPI_LOGICAL, MPI_INTEGER, MPI_INTEGER, MPI_CHARACTER, get_mpi_data_type(type)/)
    print *, "displacements:", displacements
    call MPI_Type_create_struct(5, lengths, displacements, types, this%metadata_transfer_type_mpi_id, ierror)
    if(ierror /= MPI_SUCCESS) then
      call mpp_error(FATAL, "fms_metadata_transfer_init: MPI_Type_create_struct failed")
    end if
    call MPI_Type_commit(this%metadata_transfer_type_mpi_id, ierror)
    if(ierror /= MPI_SUCCESS) then
      call mpp_error(FATAL, "fms_metadata_transfer_init: MPI_Type_create_struct failed")
    end if

    ! also need to allocate the data for non root pes
    !select case(type)
    !case(real8_type)
      allocate(real(r8_kind) :: this%attribute_value(length))
    !case(real4_type)
      !allocate(real(r4_kind) :: this%attribute_value(length))
    !case(int8_type)
      !allocate(integer(r8_kind) :: this%attribute_value(length))
    !case(int4_type)
      !allocate(integer(r4_kind) :: this%attribute_value(length))
    !end select

    this%metadata_transfer_initialized = .true.
  end subroutine fms_metadata_transfer_init

  subroutine fms_metadata_broadcast(this)
    class(metadata_type), intent(inout) :: this
    integer :: ierror
    real(r8_kind), allocatable :: tmp(:)
    if (.not. this%metadata_transfer_initialized) then
      call mpp_error(FATAL, "fms_metadata_broadcast: metadata_transfer not initialized")
    end if

    ! Broadcast the metadata transfer type to all processes
    ! TODO should be able to broadcast only to writing pes instead(?)
    call MPI_Bcast(this, 1, this%metadata_transfer_type_mpi_id, mpp_root_pe(), MPI_COMM_WORLD, ierror)
    if (ierror /= MPI_SUCCESS) then
      call mpp_error(FATAL, "fms_metadata_broadcast: MPI_Bcast failed")
    end if

  end subroutine fms_metadata_broadcast

  ! Getter and Setter for attribute_type
  function get_attribute_type(this) result(val)
    class(metadata_type), intent(in) :: this
    integer :: val
    val = this%attribute_type
  end function

  ! Getter and Setter for attribute_value
  function get_attribute_value(this) result(val)
    class(metadata_type), intent(in) :: this
    !class(*), allocatable :: val(:) !< output of copied buffer data
    real(r8_kind), allocatable :: val(:) !< output of copied buffer data
    integer :: length
                                                                       !! must be the same size as the allocated buffer
    if(.not. allocated(this%attribute_value)) call mpp_error(FATAL, 'get_attribute_value:: field not yet allocated')
    length = size(this%attribute_value)

    !select type (buff=>this%attribute_value)
      !type is (real(r4_kind))
        !allocate(real(r4_kind) :: val(length))
        !val = buff
      !type is (real(r8_kind))
        allocate(real(r8_kind) :: val(length))
        val = this%attribute_value !buff
      !type is (integer(i4_kind))
        !allocate(integer(i4_kind) :: val(length))
        !val = buff
      !type is (integer(i8_kind))
        !allocate(integer(i8_kind) :: val(length))
        !val = buff
      !type is (character(len=*))
        !allocate(character(len=length) :: val(1)) ! ???
        !val = buff
      !class default
        !call mpp_error(FATAL, "get_attribute_value:: invalid type")
    !end select
  end function

  subroutine set_attribute_value(this, val)
    class(metadata_type), intent(inout) :: this
    !class(*), intent(in) :: val(:)
    real(r8_kind), intent(in) :: val(:) 
    integer :: input_length

    !select type (val)
      !type is (integer(kind=i4_kind))
        !this%attribute_type = int4_type
        !this%attribute_value = val
      !type is (integer(kind=i8_kind))
        !this%attribute_type = int8_type
        !this%attribute_value = val
      !type is (real(kind=r4_kind))
        !this%attribute_type = real4_type
        !this%attribute_value = val
      !type is (real(kind=r8_kind))
        this%attribute_type = real8_type
        this%attribute_value = val
      !type is (character(len=*))
        !this%attribute_type = str_type
        !this%attribute_value = val
      !class default
        !call mpp_error(FATAL, "set_attribute_value:: invalid type passed in. must be real real/int kind 4 or 8 or str")
    !end select
  end subroutine

  ! Getter and Setter for attribute_name
  function get_attribute_name(this) result(val)
    class(metadata_type), intent(in) :: this
    character(len=50) :: val
    val = this%attribute_name
  end function

  subroutine set_attribute_name(this, val)
    class(metadata_type), intent(inout) :: this
    character(len=*), intent(in) :: val
    this%attribute_name = val
  end subroutine

  pure function get_mpi_data_type(type_value) result(val)
    integer, intent(in) :: type_value
    integer :: val
    select case(type_value)
    case(int4_type)
      val = MPI_INTEGER
    case(int8_type)
      val = MPI_INTEGER
    case(real4_type)
      val = MPI_REAL
    case(real8_type)
      val = MPI_REAL
    case(str_type)
      val = MPI_CHARACTER
    end select

  end function

  !> @returns Size (in bytes) of a given data type enumeration
  pure function get_data_type_size(type_value) result(val)
    integer, intent(in) :: type_value
    integer :: val
    select case(type_value)
    case(int4_type)
      val = sizeof(0_i4_kind) 
    case(int8_type)
      val = sizeof(0_i8_kind) 
    case(real4_type)
      val = sizeof(0.0_r4_kind) 
    case(real8_type)
      val = sizeof(0.0_r8_kind)
    end select

  end function

end module metadata_transfer_mod
