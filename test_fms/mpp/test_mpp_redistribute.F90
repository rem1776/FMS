program test_mpp_redistribute

  use mpp_mod,         only : FATAL, WARNING, NOTE
  use mpp_mod,         only : mpp_pe, mpp_npes, mpp_node, mpp_root_pe, mpp_error, mpp_set_warn_level
  use mpp_mod,         only : mpp_declare_pelist, mpp_set_current_pelist, mpp_sync, mpp_sync_self
  use mpp_mod,         only : mpp_init, mpp_exit, mpp_chksum, stdout, stderr
  use mpp_mod,         only : mpp_get_current_pelist, mpp_broadcast
  use mpp_mod,         only : MPP_CLOCK_SYNC, MPP_CLOCK_DETAILED
  use mpp_domains_mod, only : domain1D, domain2D, DomainCommunicator2D, BITWISE_EFP_SUM
  use mpp_domains_mod, only : mpp_get_compute_domain, mpp_get_data_domain, mpp_domains_set_stack_size
  use mpp_domains_mod, only : mpp_global_field
  use mpp_domains_mod, only : mpp_domains_init, mpp_domains_exit, mpp_broadcast_domain
  use mpp_domains_mod, only : mpp_update_domains, mpp_check_field, mpp_redistribute, mpp_get_memory_domain
  use mpp_domains_mod, only : mpp_define_layout, mpp_define_domains, mpp_modify_domain
  use mpp_domains_mod, only : mpp_get_neighbor_pe, mpp_define_mosaic, mpp_nullify_domain_list
  use mpp_domains_mod, only : mpp_get_global_domain, ZERO, NINETY, MINUS_NINETY
  use mpp_domains_mod, only : mpp_get_domain_shift, EDGEUPDATE, mpp_deallocate_domain
  use mpp_domains_mod, only : WUPDATE, SUPDATE, mpp_get_compute_domains, NONSYMEDGEUPDATE
  use mpp_domains_mod, only : domainUG, mpp_define_unstruct_domain, mpp_get_UG_domain_tile_id
  
  implicit none
 
  integer :: pe, npes, ierr
  integer :: nx=128, ny=128, nz=40, stackmax=4000000
  call mpp_init()!<TODO 
  pe = mpp_pe()
  npes = mpp_npes()
  call mpp_domains_init()
  call mpp_domains_set_stack_size(stackmax)
  call mpp_error(NOTE, "----------Starting tests----------")
  call test_redistribute_int("complete")
  call mpp_error(NOTE, "full pelist test succeeded")
  call test_redistribute_int("overlap")
  call mpp_error(NOTE, "overlap pelist test succeeded")
  call mpp_error(NOTE, "----------Tests Complete----------")
  call mpi_finalize(ierr)

contains

  !> Test redistribute between two domains with integers
  subroutine test_redistribute_int( petype )
    character(len=*), intent(in) :: petype
    type(domain2D) :: domainx, domainy
    type(DomainCommunicator2D), pointer, save :: dch =>NULL()
    integer, allocatable, dimension(:,:,:)       :: gcheck, glbl
    integer, allocatable, dimension(:,:,:), save :: x, x1, x2, x3, x4, x5, x6
    integer, allocatable, dimension(:,:,:), save :: y, y1, y2, y3, y4, y5, y6
    integer, allocatable                         :: pelist(:)
    integer                                      :: pemax, k, j, i, layout(2), id
    integer                                      :: is, ie, js, je, isd, ied, jsd, jed
    logical                                      :: onList

    ! nullify domain list otherwise it retains memory between calls.
    call mpp_nullify_domain_list(domainx)
    call mpp_nullify_domain_list(domainy)

    !fill in glbl array with kiiijjj
    allocate( gcheck(nx,ny,nz), glbl(nx,ny,nz) )
    do k = 1,nz
      do j = 1,ny
        do i = 1,nx
          glbl(i,j,k) = k*1e6 + i*1e3 + j
        end do
      end do
    end do

    !Allocates pelist based on provided petype
    pemax = npes/2
    if(nx < npes) call mpp_error(FATAL, &
          "test_mpp_redistribute: nx is less than npes, no test will be done for complete pelist")
    select case(petype)
    case("complete")
      allocate( pelist(0:npes-1) )
      pelist = (/ (i,i=0,npes-1) /)
      call mpp_declare_pelist( pelist )
    case("overlap")
      allocate( pelist(0:pemax) )
      pelist = (/ (i,i=0,pemax) /)
      call mpp_declare_pelist( pelist )
    case default
      call mpp_error(FATAL, "test_mpp_redistribute: Invalid petype given")
    end select 

    onList = any(pelist.eq.pe)
    ! set up x arrays
    call mpp_define_layout( (/1,nx,1,ny/), npes, layout )
    call mpp_define_domains( (/1,nx,1,ny/), layout, domainx, name=petype )
    call mpp_get_compute_domain( domainx, is,  ie,  js,  je  )
    call mpp_get_data_domain   ( domainx, isd, ied, jsd, jed )
    allocate( x(isd:ied,jsd:jed,nz) )
    allocate( x2(isd:ied,jsd:jed,nz) )
    allocate( x3(isd:ied,jsd:jed,nz) )
    allocate( x4(isd:ied,jsd:jed,nz) )
    allocate( x5(isd:ied,jsd:jed,nz) )
    allocate( x6(isd:ied,jsd:jed,nz) )
    x = 0.
    x(is:ie,js:je,:) = glbl(is:ie,js:je,:)
    x2 = x;  x3 = x; x4 = x; x5 = x; x6 = x

    ! select pelist from type and define domains for y
    select case(petype)
    case("complete")
      call mpp_define_domains( (/1,nx,1,ny/), (/npes,1/), domainy, name=petype)
    case("overlap")
      if(onList) then
        call mpp_set_current_pelist(pelist)
        call mpp_define_layout( (/1,nx,1,ny/), mpp_npes(), layout )
        call mpp_define_domains( (/1,nx,1,ny/), layout, domainy, name=petype)
      endif
    end select

    !set up y arrays
    if(onList) then 
      ! TODO deadlocks here
      call mpp_get_data_domain   ( domainy, isd, ied, jsd, jed )
      call mpp_get_compute_domain( domainy, is,  ie,  js,  je  )
      allocate( y(isd:ied,jsd:jed,nz) )
      allocate( y2(isd:ied,jsd:jed,nz) )
      allocate( y3(isd:ied,jsd:jed,nz) )
      allocate( y4(isd:ied,jsd:jed,nz) )
      allocate( y5(isd:ied,jsd:jed,nz) )
      allocate( y6(isd:ied,jsd:jed,nz) )
      y = 0.
      y2 = 0.;y3 = 0.;y4 = 0.;y5 = 0.;y6 = 0.
    endif
    !go global and redistribute
    !TODO and here
    call mpp_set_current_pelist()
    call mpp_broadcast_domain(domainx)
    call mpp_broadcast_domain(domainy)
    call mpp_redistribute( domainx, x, domainy, y(:,:,:) )

    !check answers on pelist
    if( ANY(pelist.EQ.pe) )then
      call mpp_set_current_pelist(pelist)
      call mpp_global_field( domainy, y(:,:,:), gcheck )
      if(.not. compare_result( glbl(1:nx,1:ny,:), gcheck, petype )) call mpp_error(FATAL , &
             "test_mpp_redistribute: incorrect results in global array")
    end if
    
    ! redistribute x
    call mpp_set_current_pelist()
    if(ALLOCATED(y))y=0.
    call mpp_redistribute( domainx, x,  domainy, y,  complete=.false. )
    call mpp_redistribute( domainx, x2, domainy, y2, complete=.false. )
    call mpp_redistribute( domainx, x3, domainy, y3, complete=.false. )
    call mpp_redistribute( domainx, x4, domainy, y4, complete=.false. )
    call mpp_redistribute( domainx, x5, domainy, y5, complete=.false. )
    call mpp_redistribute( domainx, x6, domainy, y6, complete=.true., dc_handle=dch )

    !check x answers on pelist
    if( ANY(pelist.EQ.pe) )then
      call mpp_set_current_pelist(pelist)
      call mpp_global_field( domainx, x(:,:,:), gcheck )
      if(.not. compare_result( glbl(1:nx,1:ny,:), gcheck, petype )) call mpp_error(FATAL,&
              "test_mpp_redistribute: global array differs for x")
      call mpp_global_field( domainx, x2, gcheck )
      if(.not. compare_result( glbl(1:nx,1:ny,:), gcheck, petype )) call mpp_error(FATAL,&
              "test_mpp_redistribute: global array differs for x2")
      call mpp_global_field( domainx, x3, gcheck )
      if(.not. compare_result( glbl(1:nx,1:ny,:), gcheck, petype )) call mpp_error(FATAL,&
              "test_mpp_redistribute: global array differs for x3")
      call mpp_global_field( domainx, x4, gcheck )
      if(.not. compare_result( glbl(1:nx,1:ny,:), gcheck, petype )) call mpp_error(FATAL,&
              "test_mpp_redistribute: global array differs for x4")
      call mpp_global_field( domainx, x5, gcheck )
      if(.not. compare_result( glbl(1:nx,1:ny,:), gcheck, petype )) call mpp_error(FATAL,&
              "test_mpp_redistribute: global array differs for x5")
      call mpp_global_field( domainx, x6, gcheck )
      if(.not. compare_result( glbl(1:nx,1:ny,:), gcheck, petype )) call mpp_error(FATAL,&
              "test_mpp_redistribute: global array differs for x6")
    end if

    call mpp_set_current_pelist()

    !check y answers on pelist
    if(ALLOCATED(y))then
      y=0.; y2=0.; y3=0.; y4=0.; y5=0.; y6=0.
    endif
    call mpp_redistribute( domainx, x, domainy, y, complete=.false. )
    call mpp_redistribute( domainx, x2, domainy, y2, complete=.false. )
    call mpp_redistribute( domainx, x3, domainy, y3, complete=.false. )
    call mpp_redistribute( domainx, x4, domainy, y4, complete=.false. )
    call mpp_redistribute( domainx, x5, domainy, y5, complete=.false. )
    call mpp_redistribute( domainx, x6, domainy, y6, complete=.true., dc_handle=dch )

    if( ANY(pelist.EQ.pe) )then
      call mpp_set_current_pelist(pelist)
      call mpp_global_field( domainy, y, gcheck )
      if(.not. compare_result( glbl(1:nx,1:ny,:), gcheck, petype )) call mpp_error(FATAL,&
              "test_mpp_redistribute: global array differs for y")
      call mpp_global_field( domainy, y2, gcheck )
      if(.not. compare_result( glbl(1:nx,1:ny,:), gcheck, petype )) call mpp_error(FATAL,&
              "test_mpp_redistribute: global array differs for y2")
      call mpp_global_field( domainy, y3, gcheck )
      if(.not. compare_result( glbl(1:nx,1:ny,:), gcheck, petype )) call mpp_error(FATAL,&
              "test_mpp_redistribute: global array differs for y3")
      call mpp_global_field( domainy, y4, gcheck )
      if(.not. compare_result( glbl(1:nx,1:ny,:), gcheck, petype )) call mpp_error(FATAL,&
              "test_mpp_redistribute: global array differs for y4")
      call mpp_global_field( domainy, y5, gcheck )
      if(.not. compare_result( glbl(1:nx,1:ny,:), gcheck, petype )) call mpp_error(FATAL,&
              "test_mpp_redistribute: global array differs for y5")
      call mpp_global_field( domainy, y6, gcheck )
      if(.not. compare_result( glbl(1:nx,1:ny,:), gcheck, petype )) call mpp_error(FATAL,&
              "test_mpp_redistribute: global array differs for y6")
    endif

    dch =>NULL()

    call mpp_set_current_pelist()

    deallocate(gcheck, glbl)
    deallocate(pelist)
    call mpp_redistribute( domainx, x, domainy, y, free=.true.,list_size=6 )
    deallocate(x,x2,x3,x4,x5,x6)
    deallocate(y,y2,y3,y4,y5,y6)

  end subroutine test_redistribute_int

  !> returns true if arrays are equivalent 
  function compare_result(a, b, petype )
    integer, intent(in), dimension(:,:,:)  :: a, b
    integer, allocatable, dimension(:,:,:) :: temp
    character(len=*), intent(in)        :: petype 
    logical                             :: compare_result
    
    if(size(a,1).ne.size(b,1) .or. size(a,2).ne.size(b,2) .or. size(a,3).ne.size(b,3)) call &
                        mpp_error(FATAL, "test_mpp_redistribute: comparing different sized arrays")
    temp = a - b
    compare_result = .not.any(temp.ne.0)
  end function compare_result
end program test_mpp_redistribute
