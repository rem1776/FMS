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
  use mpp_mod,         only: mpp_error, FATAL, NOTE
  use mpp_io_mod,      only: mpp_io_init
  use mpp_domains_mod, only: mpp_domains_init, mpp_define_domains, domain2d
  use mpp_domains_mod, only: mpp_define_layout, mpp_domains_set_stack_size
  use mpp_domains_mod, only: mpp_get_global_domain, mpp_global_max
  use mpp_domains_mod, only: mpp_global_min, mpp_get_data_domain,mpp_get_compute_domain
  use mpp_domains_mod, only: mpp_domains_exit, mpp_update_domains
  use mpp_domains_mod, only: mpp_get_domain_shift


  implicit none
  
  integer                       :: nx=360, ny=200,length=256
  integer                       :: id, pe, npes, root, out_unit, i, j
  integer(i4_kind)              :: max4, min4, ierr
  integer(i8_kind)              :: max8, min8
!  integer                       :: layout(2) =(/2,4/), io_layout(2)=(/1,1/)
  integer(i4_kind), allocatable :: data4(:,:)
  integer(i8_kind), allocatable :: data8(:,:)
  real(i8_kind), allocatable    :: rands(:)
  type(domain2D)                :: domain
  real                          :: rcoef
  integer                       :: isg, ieg, jsg, jeg, isc, iec, jsc, jec !< data/compute domain indices
  integer                       :: isd, ied, jsd, jed, ishift, jshift
  character(len=32)             :: res4, res8
  
  call mpp_init(0)
  call mpp_io_init()
  call mpp_domains_init()
  call mpp_set_stack_size(3145746)
  call mpp_domains_set_stack_size(3145746)
  pe = mpp_pe()
  npes = mpp_npes()
  root = mpp_root_pe()
  out_unit = stdout()

  ! define layout and domains and get data domain to allocate 
  !call mpp_define_layout( (/1,nx,1,ny/), npes, layout)
  call mpp_define_domains( (/1,length,1,length/), (/4,2/), domain, xhalo=0)
  call mpp_get_compute_domain(domain, isc, iec, jsc, jec)
  call mpp_get_data_domain(domain, isd, ied, jsd, jed)
  allocate(data4(isd:ied, jsd:jed), data8(isd:ied, jsd:jed), rands(length))

  !> make random integer arrays
  call random_number(rands)
  do i=isc, iec
    do j=jsc, jec
      rcoef = rands(j) + rands(i) - 1
      data4(i, j) = rcoef * huge(data4)
      data8(i, j) = rcoef * huge(data8)  
      if(pe.EQ.root) print *, rcoef, data4(i,j),data8(i,j)
    end do
  end do

  !> test 32-bit
  call mpp_error(NOTE, "----------Testing 32-bit mpp_global_max and mpp_global_min----------")
  call mpp_update_domains(data4, domain)
  call mpp_get_domain_shift(domain, ishift, jshift)
  max4 = mpp_global_max(domain, data4)
  min4 = mpp_global_min(domain, data4)
  write(res4, *) max4
  call mpp_error(NOTE, "mpp_global_max: " // res4)
  write(res4, *) min4
  call mpp_error(NOTE, "mpp_global_min: " // res4)

  !> test 64-bit
  call mpp_error(NOTE, "----------Testing 64-bit mpp_global_max and mpp_global_min----------")
  call mpp_update_domains(data8, domain)
  call mpp_get_domain_shift(domain, ishift, jshift)
  max8 = mpp_global_max(domain, data8)
  min8 = mpp_global_min(domain, data8)
  write(res8, *) max8
  call mpp_error(NOTE, "mpp_global_max: " // res8)
  write(res8, *) min8
  call mpp_error(NOTE, "mpp_global_min: " // res8)

  !> TODO : verify results(irl and through code)
  
  !> clean up
  deallocate(data4, data8, rands)
  !call mpp_domains_exit()
  !call mpp_exit()
  call MPI_FINALIZE(ierr)
  contains 

  subroutine test4

    call mpp_update_domains(data4, domain)
    !call mpp_get_global_domain(domain, isg, ieg, jsg, jeg)
    call mpp_get_domain_shift(domain, ishift, jshift)
    !iec = iec + jshift
    !jec = jec + jshift
    !print *,"Compute" , pe, isc, iec, jsc, jec
    !print *,"Shift  " , pe, iec-isc+1, size(data4,1)

    !> Get max and min and check matching coef
    max4 = mpp_global_max(domain, data4)
    min4 = mpp_global_min(domain, data4)
    write(res4, *) max4
    call mpp_error(NOTE, "mpp_global_max: " // res4)
    write(res4, *) min4
    call mpp_error(NOTE, "mpp_global_min: " // res4)

  end subroutine test4

end program test_global_minmax
