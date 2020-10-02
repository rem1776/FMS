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
!> @author Ryan Mulhall
!> @email gfdl.climate.model.info@noaa.gov
!> @brief Unit test for mpp_global_ max and min 
!> @description Test of mpp_global_max and mpp_global_min interfaces 
!> with mixed size integers 
program test_global_minmax

  use platform_mod
  use mpp_mod,         only: mpp_init, mpp_exit, mpp_pe, mpp_npes, mpp_root_pe, stdout
  use mpp_mod,         only: mpp_set_stack_size, mpp_sync, mpp_sync_self
  use mpp_mod,         only: mpp_error, FATAL, NOTE, mpp_send, mpp_recv
  use mpp_io_mod,      only: mpp_io_init
  use mpp_domains_mod, only: mpp_domains_init, mpp_define_domains, domain2d
  use mpp_domains_mod, only: mpp_define_layout, mpp_domains_set_stack_size
  use mpp_domains_mod, only: mpp_get_global_domain, mpp_global_max
  use mpp_domains_mod, only: mpp_global_min, mpp_get_data_domain,mpp_get_compute_domain
  use mpp_domains_mod, only: mpp_domains_exit, mpp_update_domains
  use mpp_domains_mod, only: mpp_get_domain_shift


  implicit none

  integer                       :: nx=360, ny=200,length=64
  integer                       :: id, pe, npes, root, out_unit, i, j
  integer(i4_kind)              :: maxI4, minI4, tmaxI4, tminI4 ,ierr
  integer(i8_kind)              :: maxI8, minI8, tmaxI8, tminI8
  integer(i4_kind), allocatable :: data4(:,:), resI4(:)
  integer(i8_kind), allocatable :: data8(:,:), resI8(:)
  real, allocatable             :: rands(:)
  type(domain2D)                :: domain
  real                          :: rcoef
  integer                       :: isc, iec, jsc, jec !< data/compute domain indices
  integer                       :: isd, ied, jsd, jed 
  character(len=32)             :: res4, res8
  integer(i4_kind)              :: maxInd(2),minInd(2),rminInd(1),rmaxInd(1)
  !logical                       :: checkResultInt

  call mpp_init(0)
  call mpp_io_init()
  call mpp_domains_init()
  call mpp_set_stack_size(3145746)
  call mpp_domains_set_stack_size(3145746)
  pe = mpp_pe()
  npes = mpp_npes()
  root = mpp_root_pe()
  out_unit = stdout()

  !> define domains and allocate
  call mpp_define_domains( (/1,length,1,length/), (/4,2/), domain, xhalo=0)
  call mpp_get_compute_domain(domain, isc, iec, jsc, jec)
  call mpp_get_data_domain(domain, isd, ied, jsd, jed)
  allocate(data4(isd:ied, jsd:jed),data8(isd:ied, jsd:jed), rands(length*length))

  !> make random arrays
  call random_seed()
  call random_number(rands)
  do i=isc, iec
    do j=jsc, jec
      rcoef = rands(j + i*length) * 2 -1
      data4(i, j) = rcoef * huge(data4)
      data8(i, j) = rcoef * huge(data8)
    end do
  end do

  !> test 32-bit integer
  call mpp_error(NOTE, "----------Testing 32-bit int mpp_global_max and mpp_global_min----------")
  call mpp_update_domains(data4, domain)
  maxI4 = mpp_global_max(domain, data4)
  minI4 = mpp_global_min(domain, data4)
  write(res4, *) maxI4
  call mpp_error(NOTE, "mpp_global_max: " // res4)
  write(res4, *) minI4
  call mpp_error(NOTE, "mpp_global_min: " // res4)

  !> test 64-bit integer
  call mpp_error(NOTE, "----------Testing 64-bit int mpp_global_max and mpp_global_min----------")
  call mpp_update_domains(data8, domain)
  maxI8 = mpp_global_max(domain, data8)
  minI8 = mpp_global_min(domain, data8)
  write(res8, *) maxI8
  call mpp_error(NOTE, "mpp_global_max: " // res8)
  write(res8, *)  minI8
  call mpp_error(NOTE, "mpp_global_min: " // res8)

  !> verify results
  if(.NOT. checkResultInt(resI4 = (/minI4, maxI4 /) )) then
    call mpp_error(FATAL, "test_global_minmax: invalid 32-bit integer results")
  endif
  call mpp_sync()
  call mpp_error(NOTE, "test_global_minmax: passed 32-bit integer check")

  if(.NOT. checkResultInt(resI8 = (/minI8, maxI8/) )) then
    call mpp_error(FATAL, "test_global_minmax: invalid long integer results")
  endif
  call mpp_sync()
  call mpp_error(NOTE, "test_global_minmax: passed long integer check")
  
  !> clean up
  deallocate(data4, data8, rands)
  call mpp_domains_exit()
  call MPI_FINALIZE(ierr)
  
  contains

!> takes either size integer array of min and max and 
!> returns true for matching results across pe's
function checkResultInt(resI4, resI8)
  logical                               :: checkResultInt
  integer(i8_kind), optional            :: resI8(2)
  integer(i4_kind), optional            :: resI4(2)
  integer(i8_kind),allocatable          :: res(:), tres(:)

  !> set res to given var and check global max/min with locals
  allocate(tres(2))
  if(present(resI4)) then 
    res = resI4
    checkResultInt = res(2).GE.maxval(data4) .and. res(1).LE.minval(data4)
  else if(present(resI8)) then
    res =  resI8
    checkResultInt = res(2).GE.maxval(data8) .and. res(1).LE.minval(data8)
  else 
    call mpp_error(FATAL, "test_global_minmax: checkResult called with no parameters")
  end if

  !> check that all pes have same results
  if( pe.EQ.root) then
    tres = res
    do i=1, npes-1 
      call mpp_send(tres,2, i)
    end do
    checkResultInt = .true.
  else
    call mpp_recv(tres,2, root)
    checkResultInt = checkResultInt .and. res(1) .EQ. tres(1) .and. res(2) .eq. tres(2)
  end if

  deallocate(tres)
  return 
end function checkResultInt 

end program test_global_minmax
