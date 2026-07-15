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
program test_mpp_global_field_ug

  use platform_mod
   use fms_test_mod, only: define_cubic_mosaic
  use compare_data_checksums
  use compare_data_checksums_int
  use mpp_mod,         only : mpp_init, mpp_error, FATAL, NOTE, mpp_init_test_requests_allocated
  use mpp_mod,         only : mpp_pe, mpp_npes, mpp_root_pe, mpp_broadcast
  use mpp_domains_mod, only : mpp_domains_init,  mpp_domains_set_stack_size, mpp_domains_exit
  use mpp_domains_mod, only : mpp_define_layout, mpp_define_mosaic, mpp_get_compute_domain, &
                           &  mpp_get_compute_domains, mpp_get_data_domain
  use mpp_domains_mod, only : mpp_get_ug_global_domain, mpp_global_field_ug
  use mpp_domains_mod, only : domain2D, domainUG, mpp_define_unstruct_domain, mpp_get_UG_domain_tile_id
  use mpp_domains_mod, only : mpp_get_UG_compute_domain, mpp_pass_SG_to_UG, mpp_pass_UG_to_SG
  use mpp_domains_mod, only : mpp_get_ug_global_domain, mpp_global_field_ug, mpp_get_tile_id

  implicit none

  integer :: pe, npes
  integer :: nx=128, ny=128, nz=40, stackmax=4000000
  integer :: whalo = 2, ehalo=2, shalo=2, nhalo=2
  integer :: nx_cubic=20, ny_cubic=20, layout_cubic(2)=(/0,0/), layout(2)

  type(domain2D) :: SG_domain
  type(domainUG) :: UG_domain

  character(10) :: type='Cubic-Grid'

  integer :: ntiles, npes_per_tile
  integer :: i, j, k, l, n
  integer :: isc, iec, jsc, jec, isd, ied, jsd, jed
  integer :: istart, iend, pos, tile
  integer, allocatable, dimension(:)     :: npts_tile, grid_index, ntiles_grid
  logical, allocatable, dimension(:,:,:) :: lmask
  logical :: cubic_grid

  integer :: ierr

  !> call mpp_init
  call mpp_init(test_level=mpp_init_test_requests_allocated)

  !> get pe info
  pe   = mpp_pe()
  npes = mpp_npes()

  !> initialize mpp domain(s)
  call mpp_domains_init()

  call setup_domains()

  call mpp_global_field_ug_r4()
  call mpp_global_field_ug_r8()
  call mpp_global_field_ug_i4()
  call mpp_global_field_ug_i8()

  call mpp_domains_exit()
  call MPI_FINALIZE(ierr)


contains
  !>
  !> mpp_global_field_ug_r4 BOTH 2D and 3D arrays
  !>
  subroutine mpp_global_field_ug_r4()

    implicit none

    real(kind=r4_kind) :: zero = 0.0, one=1.0
    real(kind=r4_kind), allocatable, dimension(:,:)   :: x1, x2, g1, g2
    real(kind=r4_kind), allocatable, dimension(:,:,:) :: a1, a2, gdata

    integer :: ism, iem, jsm, jem, lsg, leg

    !--- set up data
    allocate(gdata(nx,ny,ntiles))  ;  gdata = -one
    do n = 1, ntiles
       do j = 1, ny
          do i = 1, nx
             if( lmask(i,j,n) ) gdata(i,j,n) = real( n*1.e+3 + i + j*1.e-3, kind=r4_kind )
          end do
       end do
    end do

    !--- test the 2-D data is on computing domain
    allocate( a1(isc:iec, jsc:jec,1), a2(isc:iec,jsc:jec,1 ) )
    tile = mpp_pe()/npes_per_tile + 1
    do j = jsc, jec
       do i = isc, iec
          a1(i,j,1) = gdata(i,j,tile)
       enddo
    enddo
    a2 = -one

    allocate(x1(istart:iend,1), x2(istart:iend,1))
    x1 = -one  ;  x2 = -one
    !--- fill the value of x2
    tile = mpp_get_UG_domain_tile_id(UG_domain)
    pos = 0
    do n = 1, tile-1
       pos = pos + npts_tile(n)
    enddo
    do l = istart, iend
       i = mod((grid_index(pos+l)-1), nx) + 1
       j = (grid_index(pos+l)-1)/nx + 1
       x2(l,1) = gdata(i,j,tile)
    enddo

    call mpp_pass_SG_to_UG(UG_domain, a1(:,:,1), x1(:,1))
    call compare_checksums(x1, x2, type//' SG2UG 2-D compute domain')
    call mpp_pass_UG_to_SG(UG_domain, x1(:,1), a2(:,:,1))

    call compare_checksums(a1(:,:,1:1),a2(:,:,1:1),type//' UG2SG 2-D compute domain')
    deallocate(a1,a2,x1,x2)

    !--- test the 3-D data is on computing domain
    allocate( a1(isc:iec, jsc:jec,nz), a2(isc:iec,jsc:jec,nz ) )

    tile = mpp_pe()/npes_per_tile + 1
    do k = 1, nz
       do j = jsc, jec
          do i = isc, iec
             a1(i,j,k) = gdata(i,j,tile)
             if(a1(i,j,k) .NE. -one ) a1(i,j,k) = real( a1(i,j,k) + k*1.e-6, kind=r4_kind )
          enddo
       enddo
    enddo
    a2 = -one

    allocate(x1(istart:iend,nz), x2(istart:iend,nz))
    x1 = -one  ;  x2 = -one
    !--- fill the value of x2
    tile = mpp_get_UG_domain_tile_id(UG_domain)
    pos = 0
    do n = 1, tile-1
       pos = pos + npts_tile(n)
    enddo
    do l = istart, iend
       i = mod((grid_index(pos+l)-1), nx) + 1
       j = (grid_index(pos+l)-1)/nx + 1
       do k = 1, nz
          x2(l,k) = real( gdata(i,j,tile) + k*1.e-6, kind=r4_kind )
       enddo
    enddo

    call mpp_pass_SG_to_UG(UG_domain, a1, x1)
    call compare_checksums(x1, x2, type//' SG2UG 3-D data domain')
    call mpp_pass_UG_to_SG(UG_domain, x1, a2)

    call compare_checksums(a1,a2,type//' UG2SG 3-D data domain')
    deallocate(a1,a2,x1,x2)

    !----------------------------------------------------------------
    !    test mpp_global_field_ug
    !----------------------------------------------------------------
    call mpp_get_UG_global_domain(UG_domain, lsg, leg)
    tile = mpp_get_UG_domain_tile_id(UG_domain)
    allocate(g1(lsg:leg,nz), g2(lsg:leg,nz), x1(istart:iend,nz))
    g1 = zero ;  g2 =zero  ;  x1 = zero
    do k = 1, nz
       do l = lsg, leg
          g1(l,k) = real( tile*1.e6 + l + k*1.e-3, kind=r4_kind )
       enddo
       do l = istart, iend
          x1(l,k) = g1(l,k)
       enddo
    enddo

    call mpp_global_field_ug(UG_domain, x1, g2)
    call compare_checksums(g1,g2,type//' global_field_ug 3-D')

    g2 = zero
    call mpp_global_field_ug(UG_domain, x1(:,1), g2(:,1))
    call compare_checksums(g1(:,1:1),g2(:,1:1),type//' global_field_ug 2-D')

    deallocate(g1,g2,x1)

  end subroutine mpp_global_field_ug_r4
  !>
  !> mpp_global_field_ug_r8 BOTH 2D and 3D arrays
  !>
  subroutine mpp_global_field_ug_r8()

    implicit none

    real(kind=r8_kind) :: zero = 0.0, one=1.0
    real(kind=r8_kind),allocatable, dimension(:,:)     :: x1, x2, g1, g2
    real(kind=r8_kind),allocatable, dimension(:,:,:)   :: a1, a2, gdata

    integer :: ism, iem, jsm, jem, lsg, leg

    !--- set up data
    allocate(gdata(nx,ny,ntiles))  ;  gdata = -one
    do n = 1, ntiles
       do j = 1, ny
          do i = 1, nx
             if( lmask(i,j,n) ) gdata(i,j,n) = real( n*1.e+3 + i + j*1.e-3, kind=r8_kind )
          end do
       end do
    end do

    !--- test the 2-D data is on computing domain
    allocate( a1(isc:iec, jsc:jec,1), a2(isc:iec,jsc:jec,1 ) )
    tile = mpp_pe()/npes_per_tile + 1
    do j = jsc, jec
       do i = isc, iec
          a1(i,j,1) = gdata(i,j,tile)
       enddo
    enddo
    a2 = -one

    allocate(x1(istart:iend,1), x2(istart:iend,1))
    x1 = -one  ;  x2 = -one
    !--- fill the value of x2
    tile = mpp_get_UG_domain_tile_id(UG_domain)
    pos = 0
    do n = 1, tile-1
       pos = pos + npts_tile(n)
    enddo
    do l = istart, iend
       i = mod((grid_index(pos+l)-1), nx) + 1
       j = (grid_index(pos+l)-1)/nx + 1
       x2(l,1) = gdata(i,j,tile)
    enddo

    call mpp_pass_SG_to_UG(UG_domain, a1(:,:,1), x1(:,1))
    call compare_checksums(x1, x2, type//' SG2UG 2-D compute domain')
    call mpp_pass_UG_to_SG(UG_domain, x1(:,1), a2(:,:,1))

    call compare_checksums(a1(:,:,1:1),a2(:,:,1:1),type//' UG2SG 2-D compute domain')
    deallocate(a1,a2,x1,x2)

    !--- test the 3-D data is on computing domain
    allocate( a1(isc:iec, jsc:jec,nz), a2(isc:iec,jsc:jec,nz ) )

    tile = mpp_pe()/npes_per_tile + 1
    do k = 1, nz
       do j = jsc, jec
          do i = isc, iec
             a1(i,j,k) = gdata(i,j,tile)
             if(a1(i,j,k) .NE. -one ) a1(i,j,k) = real( a1(i,j,k) + k*1.e-6, kind=r8_kind )
          enddo
       enddo
    enddo
    a2 = -one

    allocate(x1(istart:iend,nz), x2(istart:iend,nz))
    x1 = -one  ;  x2 = -one
    !--- fill the value of x2
    tile = mpp_get_UG_domain_tile_id(UG_domain)
    pos = 0
    do n = 1, tile-1
       pos = pos + npts_tile(n)
    enddo
    do l = istart, iend
       i = mod((grid_index(pos+l)-1), nx) + 1
       j = (grid_index(pos+l)-1)/nx + 1
       do k = 1, nz
          x2(l,k) = real( gdata(i,j,tile) + k*1.e-6, kind=r8_kind )
       enddo
    enddo

    call mpp_pass_SG_to_UG(UG_domain, a1, x1)
    call compare_checksums(x1, x2, type//' SG2UG 3-D data domain')
    call mpp_pass_UG_to_SG(UG_domain, x1, a2)

    call compare_checksums(a1,a2,type//' UG2SG 3-D data domain')
    deallocate(a1,a2,x1,x2)

    !----------------------------------------------------------------
    !    test mpp_global_field_ug
    !----------------------------------------------------------------
    call mpp_get_UG_global_domain(UG_domain, lsg, leg)
    tile = mpp_get_UG_domain_tile_id(UG_domain)
    allocate(g1(lsg:leg,nz), g2(lsg:leg,nz), x1(istart:iend,nz))
    g1 = zero ;  g2 =zero  ;  x1 = zero
    do k = 1, nz
       do l = lsg, leg
          g1(l,k) = real( tile*1.e6 + l + k*1.e-3, kind=r8_kind )
       enddo
       do l = istart, iend
          x1(l,k) = g1(l,k)
       enddo
    enddo

    call mpp_global_field_ug(UG_domain, x1, g2)
    call compare_checksums(g1,g2,type//' global_field_ug 3-D')

    g2 = zero
    call mpp_global_field_ug(UG_domain, x1(:,1), g2(:,1))
    call compare_checksums(g1(:,1:1),g2(:,1:1),type//' global_field_ug 2-D')

    deallocate(g1,g2,x1)

  end subroutine mpp_global_field_ug_r8
  !>
  !> mpp_global_field_ug_i4 BOTH 2D and 3D arrays
  !>
  subroutine mpp_global_field_ug_i4()

    implicit none

    integer(kind=i4_kind) :: zero = 0, one=1
    integer(kind=i4_kind),allocatable, dimension(:,:)     :: x1, x2, g1, g2
    integer(kind=i4_kind),allocatable, dimension(:,:,:)   :: a1, a2, gdata

    integer :: ism, iem, jsm, jem, lsg, leg

    !--- set up data
    allocate(gdata(nx,ny,ntiles))  ;  gdata = -one
    do n = 1, ntiles
       do j = 1, ny
          do i = 1, nx
             if( lmask(i,j,n) ) gdata(i,j,n) = int( n*1e+6 + i*1e3 + j*1e2, kind=i4_kind )
          end do
       end do
    end do

    !--- test the 2-D data is on computing domain
    allocate( a1(isc:iec, jsc:jec,1), a2(isc:iec,jsc:jec,1 ) )
    tile = mpp_pe()/npes_per_tile + 1
    do j = jsc, jec
       do i = isc, iec
          a1(i,j,1) = gdata(i,j,tile)
       enddo
    enddo
    a2 = -one

    allocate(x1(istart:iend,1), x2(istart:iend,1))
    x1 = -one  ;  x2 = -one
    !--- fill the value of x2
    tile = mpp_get_UG_domain_tile_id(UG_domain)
    pos = 0
    do n = 1, tile-1
       pos = pos + npts_tile(n)
    enddo
    do l = istart, iend
       i = mod((grid_index(pos+l)-1), nx) + 1
       j = (grid_index(pos+l)-1)/nx + 1
       x2(l,1) = gdata(i,j,tile)
    enddo

    call mpp_pass_SG_to_UG(UG_domain, a1(:,:,1), x1(:,1))
    call compare_checksums_int(x1, x2, type//' SG2UG 2-D compute domain')
    call mpp_pass_UG_to_SG(UG_domain, x1(:,1), a2(:,:,1))

    call compare_checksums_int(a1(:,:,1:1),a2(:,:,1:1),type//' UG2SG 2-D compute domain')
    deallocate(a1,a2,x1,x2)

    !--- test the 3-D data is on computing domain
    allocate( a1(isc:iec, jsc:jec,nz), a2(isc:iec,jsc:jec,nz ) )

    tile = mpp_pe()/npes_per_tile + 1
    do k = 1, nz
       do j = jsc, jec
          do i = isc, iec
             a1(i,j,k) = gdata(i,j,tile)
             if(a1(i,j,k) .NE. -one ) a1(i,j,k) = int( a1(i,j,k) + k*1e6, kind=i4_kind )
          enddo
       enddo
    enddo
    a2 = -one

    allocate(x1(istart:iend,nz), x2(istart:iend,nz))
    x1 = -one  ;  x2 = -one
    !--- fill the value of x2
    tile = mpp_get_UG_domain_tile_id(UG_domain)
    pos = 0
    do n = 1, tile-1
       pos = pos + npts_tile(n)
    enddo
    do l = istart, iend
       i = mod((grid_index(pos+l)-1), nx) + 1
       j = (grid_index(pos+l)-1)/nx + 1
       do k = 1, nz
          x2(l,k) = int( gdata(i,j,tile) + k*1e6, kind=i4_kind )
       enddo
    enddo
    call mpp_pass_SG_to_UG(UG_domain, a1, x1)
    call compare_checksums_int(x1, x2, type//' SG2UG 3-D data domain')
    call mpp_pass_UG_to_SG(UG_domain, x1, a2)

    call compare_checksums_int(a1,a2,type//' UG2SG 3-D data domain')
    deallocate(a1,a2,x1,x2)

    !----------------------------------------------------------------
    !    test mpp_global_field_ug
    !----------------------------------------------------------------
    call mpp_get_UG_global_domain(UG_domain, lsg, leg)
    tile = mpp_get_UG_domain_tile_id(UG_domain)
    allocate(g1(lsg:leg,nz), g2(lsg:leg,nz), x1(istart:iend,nz))
    g1 = zero ;  g2 =zero  ;  x1 = zero
    do k = 1, nz
       do l = lsg, leg
          g1(l,k) = int( n*1e+6 + i*1e3 + j*1e2, kind=i4_kind )
       enddo
       do l = istart, iend
          x1(l,k) = g1(l,k)
       enddo
    enddo

    call mpp_global_field_ug(UG_domain, x1, g2)
    call compare_checksums_int(g1,g2,type//' global_field_ug 3-D')

    g2 = zero
    call mpp_global_field_ug(UG_domain, x1(:,1), g2(:,1))
    call compare_checksums_int(g1(:,1:1),g2(:,1:1),type//' global_field_ug 2-D')

    deallocate(g1,g2,x1)

  end subroutine mpp_global_field_ug_i4
  !>
  !> mpp_global_field_ug_i8 BOTH 2D and 3D arrays
  !>
  subroutine mpp_global_field_ug_i8()

    implicit none

    integer(kind=i8_kind) :: zero = 0, one=1
    integer(kind=i8_kind),allocatable, dimension(:,:)   :: x1, x2, g1, g2
    integer(kind=i8_kind),allocatable, dimension(:,:,:) :: a1, a2, gdata

    integer :: ism, iem, jsm, jem, lsg, leg

    !> interface for mpp_pass_SG_to_UG for i8 does not exist

    call mpp_get_UG_global_domain(UG_domain, lsg, leg)
    tile = mpp_get_UG_domain_tile_id(UG_domain)
    allocate(g1(lsg:leg,nz), g2(lsg:leg,nz), x1(istart:iend,nz))
    g1 = zero ;  g2 =zero  ;  x1 = zero
    do k = 1, nz
       do l = lsg, leg
          g1(l,k) = int( n*1e+6 + i*1e3 + j*1e2, kind=i8_kind )
       enddo
       do l = istart, iend
          x1(l,k) = g1(l,k)
       enddo
    enddo

    call mpp_global_field_ug(UG_domain, x1, g2)
    call compare_checksums_int(g1,g2,type//' global_field_ug 3-D')

    g2 = zero
    call mpp_global_field_ug(UG_domain, x1(:,1), g2(:,1))
    call compare_checksums_int(g1(:,1:1),g2(:,1:1),type//' global_field_ug 2-D')

    deallocate(g1,g2,x1)

  end subroutine mpp_global_field_ug_i8
  !>
  !> setup_domains
  !>
  subroutine setup_domains()

    implicit none

    integer :: num_contact, shift, ntotal_land

    integer, allocatable, dimension(:)   :: isl, iel, jsl, jel
    integer, allocatable, dimension(:)   :: pe_start, pe_end
    integer, allocatable, dimension(:,:) :: layout2D, global_indices
    real,    allocatable, dimension(:,:) :: rmask
    real,    allocatable, dimension(:)   :: frac_crit


    !--- check the type
    select case(type)
    case ( 'Cubic-Grid' )
       if( nx_cubic == 0 ) &
            call mpp_error(FATAL,'test_unstruct_update: for Cubic_grid mosaic, nx_cubic is zero, '//&
            'No test is done for Cubic-Grid mosaic. ' )
       if( nx_cubic .NE. ny_cubic ) &
            call mpp_error(FATAL,'test_unstruct_update: for Cubic_grid mosaic, nx_cubic does not equal ny_cubic, '//&
            'No test is done for Cubic-Grid mosaic. ' )
       nx = nx_cubic
       ny = ny_cubic
       ntiles = 6
       num_contact = 12
       cubic_grid = .true.
       if( mod(npes, ntiles) == 0 ) then
          npes_per_tile = npes/ntiles
          write(*,*)'NOTE from test_unstruct_update ==> For Mosaic "', trim(type), &
               '", each tile will be distributed over ', npes_per_tile, ' processors.'
       else
          call mpp_error(FATAL,'test_unstruct_update: npes should be multiple of ntiles No test is done for '// &
                         & trim(type))
       endif
       if(layout_cubic(1)*layout_cubic(2) == npes_per_tile) then
          layout = layout_cubic
       else
          call mpp_define_layout( (/1,nx,1,ny/), npes_per_tile, layout )
       endif
       allocate(frac_crit(ntiles))
       frac_crit(1) = 0.3; frac_crit(2) = 0.1; frac_crit(3) = 0.6
       frac_crit(4) = 0.2; frac_crit(5) = 0.4; frac_crit(6) = 0.5
    case default
       call mpp_error(FATAL, 'test_group_update: no such test: '//type)
    end select

    allocate(layout2D(2,ntiles), global_indices(4,ntiles), pe_start(ntiles), pe_end(ntiles) )
    do n = 1, ntiles
       pe_start(n) = (n-1)*npes_per_tile
       pe_end(n)   = n*npes_per_tile-1
    end do

    do n = 1, ntiles
       global_indices(:,n) = (/1,nx,1,ny/)
       layout2D(:,n)       = layout
    end do

    !--- define domain
    if( cubic_grid ) call define_cubic_mosaic(type, SG_domain, (/nx,nx,nx,nx,nx,nx/), (/ny,ny,ny,ny,ny,ny/), &
         global_indices, layout2D, pe_start, pe_end )

    !--- setup data
    call mpp_get_compute_domain( SG_domain, isc, iec, jsc, jec )
    call mpp_get_data_domain   ( SG_domain, isd, ied, jsd, jed )

    allocate( lmask(nx,ny,ntiles), npts_tile(ntiles) )
    lmask = .false.
    if(mpp_pe() == mpp_root_pe() ) then
       allocate( rmask(nx,ny) )
       !--- construct gmask.
       do n = 1, ntiles
          call random_number(rmask)
          do j = 1, ny
             do i = 1, nx
                if(rmask(i,j) > frac_crit(n)) lmask(i,j,n) = .true.
             enddo
          enddo
          npts_tile(n) = count(lmask(:,:,n))
       enddo

       ntotal_land = sum(npts_tile)
       allocate(grid_index(ntotal_land))
       allocate(isl(0:mpp_npes()-1), iel(0:mpp_npes()-1))
       allocate(jsl(0:mpp_npes()-1), jel(0:mpp_npes()-1))
       call mpp_get_compute_domains(SG_domain,xbegin=isl,xend=iel,ybegin=jsl,yend=jel)

       l = 0
       do n = 1, ntiles
          do j = 1, ny
             do i = 1, nx
                if(lmask(i,j,n)) then
                   l = l + 1
                   grid_index(l) = (j-1)*nx+i
                endif
             enddo
          enddo
       enddo

       deallocate(rmask, isl, iel, jsl, jel)

    end if

    call mpp_broadcast(npts_tile, ntiles, mpp_root_pe())
    if(mpp_pe() .NE. mpp_root_pe()) then
       ntotal_land = sum(npts_tile)
       allocate(grid_index(ntotal_land))
    endif
    call mpp_broadcast(grid_index, ntotal_land, mpp_root_pe())

    allocate(ntiles_grid(ntotal_land))
    ntiles_grid = 1
    !--- define the unstructured grid domain
    call mpp_define_unstruct_domain(UG_domain, SG_domain, npts_tile, ntiles_grid, mpp_npes(), 1, grid_index, &
                                   &  name="LAND unstruct")
    call mpp_get_UG_compute_domain(UG_domain, istart, iend)

    !--- figure out lmask according to grid_index
    pos = 0
    do n = 1, ntiles
       do l = 1, npts_tile(n)
          pos = pos + 1
          j = (grid_index(pos)-1)/nx + 1
          i = mod((grid_index(pos)-1),nx) + 1
          lmask(i,j,n) = .true.
       enddo
    enddo

  end subroutine setup_domains
end program test_mpp_global_field_ug
