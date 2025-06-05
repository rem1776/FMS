program test_metadata_transfer
  use fms_mod, only: fms_init, fms_end, string
  use mpp_mod
  use metadata_transfer_mod
  use platform_mod
  use mpi
  use, intrinsic :: iso_c_binding

  implicit none

  type(metadata_type) :: file_metadata(2)
  real(r8_kind) :: attr_data(2)

  call fms_init()

  ! need to init to define the data structure for future MPI broadcasts
  call file_metadata(1)%fms_metadata_transfer_init(real8_type, 2)
  call file_metadata(2)%fms_metadata_transfer_init(real8_type, 2)

  ! set metadata only on root PE
  if (mpp_pe() .eq. mpp_root_pe()) then

    attr_data = [-666.0_r8_kind, 666.0_r8_kind]

    call file_metadata(1)%set_attribute_name("_FillValue"//c_null_char)
    call file_metadata(1)%set_attribute_value(attr_data)

    call file_metadata(2)%set_attribute_name("missing_value"//c_null_char)
    call file_metadata(2)%set_attribute_value(attr_data)
  endif

  call mpp_sync()
  ! Broadcast the metadata to all PEs
  call file_metadata(1)%fms_metadata_broadcast()
  call file_metadata(2)%fms_metadata_broadcast()

  call mpp_sync()

  !select type(val => file_metadata(1)%attribute_value)
  !type is (real(r8_kind))
    print *, "pe:", mpp_pe()
    print *, "val after bcast:", allocated(file_metadata(1)%attribute_value)
  !end select

  call mpp_sync()

  call dump_metadata(file_metadata)

  call fms_end()

  contains

  subroutine dump_metadata(this)
    type(metadata_type), intent(in) :: this(:)
    class(*), allocatable :: val(:) 

    integer :: i
    do i = 1, size(this)
      !val = this(i)%get_attribute_value()
      !select type(val)
      !type is(real(r8_kind))
      !  print *, mpp_pe(), " knows that the attribute_name is ", trim(adjustl(this(i)%get_attribute_name())), &
      !    " and the attribute_type is ", string(this(i)%get_attribute_type()) !, &
          !" and the value is ", val 
      !end select
      !deallocate(val)
      print *, mpp_pe(), " knows that the attribute_name is ", trim(adjustl(this(i)%get_attribute_name())), &
          " and the attribute_type is ", string(this(i)%get_attribute_type()) , &
          " and the value is ", string(this(i)%get_attribute_value()) 
    enddo
  end subroutine
end program