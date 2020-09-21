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
#ifdef SYSTEM_CLOCK
#undef SYSTEM_CLOCK
#endif

program test   !test various aspects of mpp_mod

#ifdef sgi_mipspro
  use shmem_interface
#endif
  use platform_mod
  use mpp_mod, only : mpp_init, mpp_exit, mpp_pe, mpp_npes, mpp_root_pe, stdout
  use mpp_mod, only : mpp_clock_id, mpp_clock_begin, mpp_clock_end, mpp_sync, mpp_malloc
  use mpp_mod, only : mpp_declare_pelist, mpp_set_current_pelist, mpp_set_stack_size
  use mpp_mod, only : mpp_broadcast, mpp_transmit, mpp_sum, mpp_max, mpp_chksum, ALL_PES
  use mpp_mod, only : mpp_gather, mpp_error, FATAL, mpp_sync_self, NOTE
  use mpp_io_mod, only: mpp_io_init, mpp_flush
#ifdef use_MPI_GSM
  use mpp_mod, only : mpp_gsm_malloc, mpp_gsm_free
#endif

  implicit none

  integer, parameter              :: n=1048576
  real, allocatable, dimension(:) :: a, b, c
#ifdef use_MPI_GSM
  real                            :: d(n)
  pointer (locd, d)
#else
  real, allocatable, dimension(:) :: d
  integer(i8_kind) :: locd
#endif
  integer                         :: tick, tick0, ticks_per_sec, id
  integer                         :: pe, npes, root, i, j, k, l, m, n2, istat
  integer                         :: out_unit
  real                            :: dt

  call mpp_init()
  call mpp_io_init()
  call mpp_set_stack_size(3145746)
  pe = mpp_pe()
  npes = mpp_npes()
  root = mpp_root_pe()
  out_unit = stdout()

  call SYSTEM_CLOCK( count_rate=ticks_per_sec )

  if( pe.EQ.root ) print *, '------------------> Calling test_mpp_chksum_int <------------------'
    call test_mpp_chksum_int()
  if( pe.EQ.root ) print *, '------------------> Finished test_mpp_chksum_int <------------------'

  if( pe.EQ.root ) print *, '------------------> Calling test_time_transmit <------------------'
    call test_time_transmit()
  if( pe.EQ.root ) print *, '------------------> Finished test_time_transmit <------------------'

  if( pe.EQ.root ) print *, '------------------> Calling test_mpp_max <------------------'
    call test_mpp_max()
  if( pe.EQ.root ) print *, '------------------> Finished test_mpp_max <------------------'

  if( pe.EQ.root ) print *, '------------------> Calling test_mpp_chksum <------------------'
    call test_mpp_chksum()
  if( pe.EQ.root ) print *, '------------------> Finished test_mpp_chksum <------------------'

!test of pointer sharing
#ifdef use_MPI_GSM
      call mpp_gsm_malloc( locd, sizeof(d) )
#else
  if( pe.EQ.root )then
      allocate( d(n) )
      locd = LOC(d)
  end if
  call mpp_broadcast(locd,root)
#endif
  if( pe.EQ.root )then
      call random_number(d)
  end if
  call mpp_sync()
!  call test_shared_pointers(locd,n)

#ifdef use_MPI_GSM
  call mpp_gsm_free( locd )
#else
  if( pe.EQ.root )then
      deallocate( d )
  end if
#endif


  call mpp_exit()

contains

  subroutine test_time_transmit()

  allocate( a(n), b(n) )
  id = mpp_clock_id( 'Random number' )
  call mpp_clock_begin(id)
  call random_number(a)
  call mpp_clock_end  (id)

  id = mpp_clock_id( 'mpp_transmit' )
  call mpp_clock_begin(id)
  !timing is done for cyclical pass (more useful than ping-pong etc)
  l = n
  do while( l.GT.0 )
     !--- mpp_transmit -------------------------------------------------
     call mpp_sync()
     call SYSTEM_CLOCK(tick0)
     do i = 1,npes
        call mpp_transmit( put_data=a(1), plen=l, to_pe=modulo(pe+npes-i,npes), &
                           get_data=b(1), glen=l, from_pe=modulo(pe+i,npes) )
        call mpp_sync_self()
     end do
     call mpp_sync()
     call SYSTEM_CLOCK(tick)
     dt = real(tick-tick0)/(npes*ticks_per_sec)
     dt = max( dt, epsilon(dt) )
     if( pe.EQ.root ) print *, 'MPP_TRANSMIT length, time, bw(Mb/s)=', l, dt, l*8e-6/dt
     l = l/2
  end do

  end subroutine test_time_transmit

  subroutine test_mpp_max

  a = real(pe+1)
  print *, 'pe,     pe+1 =', pe, a(1)
  call mpp_max( a(1) )
  print *, 'pe, max(pe+1)=', pe, a(1)
  !pelist check
  call mpp_sync()
  call flush(out_unit)
  if( npes.GE.2 )then
     if( pe.EQ.root )print *, 'Test of pelists: bcast, sum and max using PEs 0...npes-2 (excluding last PE)'
     call mpp_declare_pelist( (/(i,i=0,npes-2)/) )
     a = real(pe+1)
     if( pe.NE.npes-1 )call mpp_broadcast( a, n, npes-2, (/(i,i=0,npes-2)/) )
     print *, 'bcast(npes-1) from 0 to npes-2=', pe, a(1)
     a = real(pe+1)
     if( pe.NE.npes-1 )then
        call mpp_set_current_pelist( (/(i,i=0,npes-2)/) )
        id = mpp_clock_id( 'Partial mpp_sum' )
        call mpp_clock_begin(id)
        call mpp_sum( a(1:1000), 1000, (/(i,i=0,npes-2)/) )
        call mpp_clock_end  (id)
     end if
     if( pe.EQ.root )print *, 'sum(pe+1) from 0 to npes-2=', a(1)
     a = real(pe+1)
     if( pe.NE.npes-1 )call mpp_max( a(1), (/(i,i=0,npes-2)/) )
     if( pe.EQ.root )print *, 'max(pe+1) from 0 to npes-2=', a(1)
  end if
  call mpp_set_current_pelist()

   end subroutine test_mpp_max

  subroutine test_mpp_chksum()

   if( modulo(n,npes).EQ.0 )then  !only set up for even division
     n2 = 1024
     a = 0.d0
     if( pe.EQ.root )call random_number(a(1:n2))

     call mpp_sync()
     call mpp_transmit( put_data=a(1), plen=n2, to_pe=ALL_PES, &
                        get_data=a(1), glen=n2, from_pe=root )
     call mpp_sync_self ()

     m= n2/npes

     allocate( c(m) )
     c = a(pe*m+1:pe*m+m)

     if( pe.EQ.root )then
        print *
        print *, '------------------ > Test mpp_chksum <------------------ '
        print *, 'This test shows that a whole array and a distributed array give identical checksums.'
     end if

     if ( mpp_chksum(a(1:n2),(/pe/)) .NE. mpp_chksum(c) ) then
       call mpp_error(FATAL, 'Test mpp_chksum fails: a whole array and a distributed array did not give identical checksums')
     else
       print *, 'For pe=', pe, ' chksum(a(1:1024))=chksum(c(1:1024))='
     endif

   else
     call mpp_error(FATAL, 'Test mpp_chksum: cannot run this test since n cannot be evenly by npes')
   end if

  end subroutine test_mpp_chksum

  subroutine test_mpp_chksum_int
    integer(i8_kind), allocatable  :: data8(:)
    integer(i4_kind), allocatable :: data4(:)
    integer(i8_kind)               :: res4, res8, tres4, tres8
    real, allocatable, dimension(:)  :: rands
    integer                          :: i, length
    !> generate random arrays for both kinds
    length = 1024
    allocate(rands(length), data8(length), data4(length))
    if(pe.EQ.root) call random_number(rands)
    do i = 1, length 
      data8(i) = rands(i) * huge(data4(1))
      data4(i) = rands(i) * huge(data4(1))
    end do
    !> calc chksums  
    res4 = mpp_chksum(data4)
    res8 = mpp_chksum(data8)
    !> check results
    if(res4.NE.res8) then
      call mpp_error(FATAL, 'Test mpp_chksum_int: mixed precision chksums do not match')
    else
      call mpp_error(NOTE, 'Test mpp_chksum_int: mixed precision checksums match')
    endif
    !> print results
    if(pe.EQ.root) print *,res4
    if(pe.EQ.root) print *,res8
    deallocate(rands, data8, data4)
  end subroutine test_mpp_chksum_int

  subroutine test_shared_pointers(locd,n)
    integer(i8_kind), intent(in) :: locd
    integer :: n
    real :: dd(n)
    pointer( p, dd )

    p = locd
    print *, 'TEST_SHARED_POINTERS: pe, locd=', pe, locd
!    print *, 'TEST_SHARED_POINTERS: pe, chksum(d)=', pe, mpp_chksum(dd,(/pe/))
    print *, 'TEST_SHARED_POINTERS: pe, sum(d)=', pe, sum(dd)
    return
  end subroutine test_shared_pointers
end program test
