program test_mpp_redistribute

  ! use mpp_domains, only :
  use mpp_mod,         only : FATAL, WARNING, NOTE
  use mpp_mod,         only : mpp_pe, mpp_npes, mpp_node, mpp_root_pe, mpp_error, mpp_set_warn_level
  use mpp_mod,         only : mpp_declare_pelist, mpp_set_current_pelist, mpp_sync, mpp_sync_self
  use mpp_mod,         only : mpp_init, mpp_exit, mpp_chksum, stdout, stderr
  !use mpp_mod,         only : input_nml_file
  use mpp_mod,         only : mpp_get_current_pelist, mpp_broadcast
  use mpp_mod,         only : MPP_CLOCK_SYNC, MPP_CLOCK_DETAILED
  !use mpp_domains_mod, only : GLOBAL_DATA_DOMAIN, BITWISE_EXACT_SUM, BGRID_NE, CGRID_NE, DGRID_NE, AGRID
  !use mpp_domains_mod, only : FOLD_SOUTH_EDGE, FOLD_NORTH_EDGE, FOLD_WEST_EDGE, FOLD_EAST_EDGE
  !use mpp_domains_mod, only : MPP_DOMAIN_TIME, CYCLIC_GLOBAL_DOMAIN, NUPDATE,EUPDATE, XUPDATE, YUPDATE, SCALAR_PAIR
  use mpp_domains_mod, only : domain1D, domain2D, DomainCommunicator2D, BITWISE_EFP_SUM
  use mpp_domains_mod, only : mpp_get_compute_domain, mpp_get_data_domain, mpp_domains_set_stack_size
  use mpp_domains_mod, only : mpp_global_field
  use mpp_domains_mod, only : mpp_domains_init, mpp_domains_exit, mpp_broadcast_domain
  use mpp_domains_mod, only : mpp_update_domains, mpp_check_field, mpp_redistribute, mpp_get_memory_domain
  use mpp_domains_mod, only : mpp_define_layout, mpp_define_domains, mpp_modify_domain
  use mpp_domains_mod, only : mpp_get_neighbor_pe, mpp_define_mosaic, mpp_nullify_domain_list
  !use mpp_domains_mod, only : NORTH, NORTH_EAST, EAST, SOUTH_EAST, CORNER, CENTER
  !use mpp_domains_mod, only : SOUTH, SOUTH_WEST, WEST, NORTH_WEST, mpp_define_mosaic_pelist
  use mpp_domains_mod, only : mpp_get_global_domain, ZERO, NINETY, MINUS_NINETY
  !use mpp_domains_mod, only : mpp_get_boundary, mpp_start_update_domains, mpp_complete_update_domains
  !use mpp_domains_mod, only : mpp_define_nest_domains, nest_domain_type
  !use mpp_domains_mod, only : mpp_get_C2F_index, mpp_update_nest_fine
  !use mpp_domains_mod, only : mpp_get_F2C_index, mpp_update_nest_coarse
  !use mpp_domains_mod, only : mpp_get_nest_coarse_domain, mpp_get_nest_fine_domain
  !use mpp_domains_mod, only : mpp_is_nest_fine, mpp_is_nest_coarse
  !use mpp_domains_mod, only : mpp_get_nest_pelist, mpp_get_nest_npes
  !use mpp_domains_mod, only : mpp_get_nest_fine_pelist, mpp_get_nest_fine_npes
  use mpp_domains_mod, only : mpp_get_domain_shift, EDGEUPDATE, mpp_deallocate_domain
  !use mpp_domains_mod, only : mpp_group_update_type, mpp_create_group_update
  !use mpp_domains_mod, only : mpp_do_group_update, mpp_clear_group_update
  !use mpp_domains_mod, only : mpp_start_group_update, mpp_complete_group_update
  use mpp_domains_mod, only : WUPDATE, SUPDATE, mpp_get_compute_domains, NONSYMEDGEUPDATE
  use mpp_domains_mod, only : domainUG, mpp_define_unstruct_domain, mpp_get_UG_domain_tile_id
  
  implicit none
 
  integer :: pe, npes
  integer :: nx=128, ny=128, nz=40, stackmax=4000000
  integer, allocatable, dimension(:,:,:),save :: temp

  call mpp_init()!<TODO 
  pe = mpp_pe()
  npes = mpp_npes()
  call mpp_domains_init()
  call mpp_domains_set_stack_size(stackmax)
  call test_redistribute_int("Complete pelist")

contains

  !> Test redistribute between two domains
  subroutine test_redistribute_int( type )
    character(len=*), intent(in) :: type
    type(domain2D) :: domainx, domainy
    type(DomainCommunicator2D), pointer, save :: dch =>NULL()
    integer, allocatable, dimension(:,:,:)       :: gcheck, glbl
    integer, allocatable, dimension(:,:,:), save :: x, x1, x2, x3, x4, x5, x6
    integer, allocatable, dimension(:,:,:), save :: y, y1, y2, y3, y4, y5, y6
    integer, allocatable                         :: pelist(:)
    integer                                      :: pemax, k, j, i, layout(2), id
    integer                                      :: is, ie, js, je, isd, ied, jsd, jed

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

    !Allocates pelist (TODO(maybe) include other pe options.. rn sets as global) 
    pemax = npes/2
    if(nx < npes) then
      call mpp_error(FATAL, &
          "test_mpp_redistribute: nx is less than npes, no test will be done for complete pelist")
      return
    endif
    allocate( pelist(0:npes-1) )
    pelist = (/ (i,i=0,npes-1) /)
    call mpp_declare_pelist( pelist )

    ! set up x arrays
    call mpp_define_layout( (/1,nx,1,ny/), npes, layout )
    call mpp_define_domains( (/1,nx,1,ny/), layout, domainx, name=type )
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

    !set up y arrays
    call mpp_define_domains( (/1,nx,1,ny/), (/npes,1/), domainy, name=type )
    call mpp_get_compute_domain( domainy, is,  ie,  js,  je  )
    call mpp_get_data_domain   ( domainy, isd, ied, jsd, jed )
    allocate( y(isd:ied,jsd:jed,nz) )
    allocate( y2(isd:ied,jsd:jed,nz) )
    allocate( y3(isd:ied,jsd:jed,nz) )
    allocate( y4(isd:ied,jsd:jed,nz) )
    allocate( y5(isd:ied,jsd:jed,nz) )
    allocate( y6(isd:ied,jsd:jed,nz) )
    y = 0.
    y2 = 0.;y3 = 0.;y4 = 0.;y5 = 0.;y6 = 0.

    !TODO different pe allocations for above

    !go global and redistribute
    call mpp_set_current_pelist()
    call mpp_broadcast_domain(domainx)
    call mpp_broadcast_domain(domainy)
    call mpp_redistribute( domainx, x, domainy, y(:,:,:) )

    !print *, pe,"y", y(1,1,1)
    !print *, pe,"x", x(isd:ied,jsd:jed,nz)
    !check answers on pelist
    if( ANY(pelist.EQ.pe) )then
      call mpp_set_current_pelist(pelist)
      call mpp_global_field( domainy, y(:,:,:), gcheck )
      if(.not. compare_result( glbl(1:nx,1:ny,:), gcheck, type )) call mpp_error(FATAL , &
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
        if(.not. compare_result( glbl(1:nx,1:ny,:), gcheck, type )) call mpp_error(FATAL,&
                "test_mpp_redistribute: global array differs for x")
        call mpp_global_field( domainx, x2, gcheck )
        if(.not. compare_result( glbl(1:nx,1:ny,:), gcheck, type )) call mpp_error(FATAL,&
                "test_mpp_redistribute: global array differs for x2")
        call mpp_global_field( domainx, x3, gcheck )
        if(.not. compare_result( glbl(1:nx,1:ny,:), gcheck, type )) call mpp_error(FATAL,&
                "test_mpp_redistribute: global array differs for x3")
        call mpp_global_field( domainx, x4, gcheck )
        if(.not. compare_result( glbl(1:nx,1:ny,:), gcheck, type )) call mpp_error(FATAL,&
                "test_mpp_redistribute: global array differs for x4")
        call mpp_global_field( domainx, x5, gcheck )
        if(.not. compare_result( glbl(1:nx,1:ny,:), gcheck, type )) call mpp_error(FATAL,&
                "test_mpp_redistribute: global array differs for x5")
        call mpp_global_field( domainx, x6, gcheck )
        if(.not. compare_result( glbl(1:nx,1:ny,:), gcheck, type )) call mpp_error(FATAL,&
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
      if(.not. compare_result( glbl(1:nx,1:ny,:), gcheck, type )) call mpp_error(FATAL,&
              "test_mpp_redistribute: global array differs for y")
      call mpp_global_field( domainy, y2, gcheck )
      if(.not. compare_result( glbl(1:nx,1:ny,:), gcheck, type )) call mpp_error(FATAL,&
              "test_mpp_redistribute: global array differs for y2")
      call mpp_global_field( domainy, y3, gcheck )
      if(.not. compare_result( glbl(1:nx,1:ny,:), gcheck, type )) call mpp_error(FATAL,&
              "test_mpp_redistribute: global array differs for y3")
      call mpp_global_field( domainy, y4, gcheck )
      if(.not. compare_result( glbl(1:nx,1:ny,:), gcheck, type )) call mpp_error(FATAL,&
              "test_mpp_redistribute: global array differs for y4")
      call mpp_global_field( domainy, y5, gcheck )
      if(.not. compare_result( glbl(1:nx,1:ny,:), gcheck, type )) call mpp_error(FATAL,&
              "test_mpp_redistribute: global array differs for y5")
      call mpp_global_field( domainy, y6, gcheck )
      if(.not. compare_result( glbl(1:nx,1:ny,:), gcheck, type )) call mpp_error(FATAL,&
              "test_mpp_redistribute: global array differs for y6")
    endif

    dch =>NULL()

    call mpp_set_current_pelist()

    deallocate(gcheck, glbl)
    if(ALLOCATED(pelist)) deallocate(pelist)

    if(ALLOCATED(x))then
      call mpp_redistribute( domainx, x, domainy, y, free=.true.,list_size=6 )
      deallocate(x,x2,x3,x4,x5,x6)
    endif
    if(ALLOCATED(y))deallocate(y,y2,y3,y4,y5,y6)
  end subroutine test_redistribute_int

  !> returns true if arrays are equivalent 
  function compare_result(a, b, c)
    integer, intent(in), dimension(:,:,:)  :: a, b
    character(len=*), intent(in)        :: c
    logical                             :: compare_result
    
    if(size(a,1).ne.size(b,1) .or. size(a,2).ne.size(b,2) .or. size(a,3).ne.size(b,3)) call &
                        mpp_error(FATAL, "test_mpp_redistribute: comparing different sized arrays")
    temp = a - b
    compare_result = .not.any(temp.ne.0)
  end function compare_result

#if 0
  subroutine setupArrays(type)
    character(len=*), intent(in) :: type
    integer, allocatable, dimension(:,:,:), save :: x, y
    type(domain2D) :: domainx, domainy
    type(DomainCommunicator2D), pointer, save :: dch =>NULL()
    integer, allocatable, dimension(:,:,:)       :: gcheck, glbl
    integer, allocatable, dimension(:,:,:,:)                  :: y, x
    integer, allocatable ::pelist(:)
    integer :: pemax, k, j, i, layout(2), id
    integer :: is, ie, js, je, isd, ied, jsd, jed
!set up x and y arrays
    select case(type)
    case( 'Complete pelist' )
!set up x array
        call mpp_define_layout( (/1,nx,1,ny/), npes, layout )
        call mpp_define_domains( (/1,nx,1,ny/), layout, domainx, name=type )
        call mpp_get_compute_domain( domainx, is,  ie,  js,  je  )
        call mpp_get_data_domain   ( domainx, isd, ied, jsd, jed )
        allocate( x(isd:ied,jsd:jed,nz) )
        allocate( x2(isd:ied,jsd:jed,nz) )
        allocate( x3(isd:ied,jsd:jed,nz) )
        allocate( x4(isd:ied,jsd:jed,nz) )
        allocate( x5(isd:ied,jsd:jed,nz) )
        allocate( x6(isd:ied,jsd:jed,nz) )
        x = 0.
        x(1,is:ie,js:je,:) = glbl(is:ie,js:je,:)
        x2 = x;  x3 = x; x4 = x; x5 = x; x6 = x
!set up y array
        call mpp_define_domains( (/1,nx,1,ny/), (/npes,1/), domainy, name=type )
        call mpp_get_compute_domain( domainy, is,  ie,  js,  je  )
        call mpp_get_data_domain   ( domainy, isd, ied, jsd, jed )
        allocate( y(isd:ied,jsd:jed,nz) )
        allocate( y2(isd:ied,jsd:jed,nz) )
        allocate( y3(isd:ied,jsd:jed,nz) )
        allocate( y4(isd:ied,jsd:jed,nz) )
        allocate( y5(isd:ied,jsd:jed,nz) )
        allocate( y6(isd:ied,jsd:jed,nz) )
        y(1,:,:,:) = 0.
        y2 = 0.;y3 = 0.;y4 = 0.;y5 = 0.;y6 = 0.
    case( 'Overlap pelist' )
!one pelist from 0...pemax, other from 0...npes-1
!set up x array
        call mpp_define_layout( (/1,nx,1,ny/), npes, layout )
        call mpp_define_domains( (/1,nx,1,ny/), layout, domainx, name=type )
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
!set up y array
        if( ANY(pelist.EQ.pe) )then
            call mpp_set_current_pelist(pelist)
            call mpp_define_layout( (/1,nx,1,ny/), mpp_npes(), layout )
            call mpp_define_domains( (/1,nx,1,ny/), layout, domainy, name=type )
            call mpp_get_compute_domain( domainy, is,  ie,  js,  je  )
            call mpp_get_data_domain   ( domainy, isd, ied, jsd, jed )
            allocate( y(1, isd:ied,jsd:jed,nz) )
            allocate( y2(isd:ied,jsd:jed,nz) )
            allocate( y3(isd:ied,jsd:jed,nz) )
            allocate( y4(isd:ied,jsd:jed,nz) )
            allocate( y5(isd:ied,jsd:jed,nz) )
            allocate( y6(isd:ied,jsd:jed,nz) )
            y = 0.
            y2 = 0.;y3 = 0.;y4 = 0.;y5 = 0.;y6 = 0.
        end if
    case( 'Disjoint pelist' )
!one pelist from 0...pemax, other from pemax+1...npes-1

!set up y array
        if( ANY(pelist.EQ.pe) )then
            call mpp_set_current_pelist(pelist)
            call mpp_define_layout( (/1,nx,1,ny/), mpp_npes(), layout )
            call mpp_define_domains( (/1,nx,1,ny/), layout, domainy, name=type )
            call mpp_get_compute_domain( domainy, is,  ie,  js,  je  )
            call mpp_get_data_domain   ( domainy, isd, ied, jsd, jed )
            allocate( y(1, isd:ied,jsd:jed,nz) )
            allocate( y2(isd:ied,jsd:jed,nz) )
            allocate( y3(isd:ied,jsd:jed,nz) )
            allocate( y4(isd:ied,jsd:jed,nz) )
            allocate( y5(isd:ied,jsd:jed,nz) )
            allocate( y6(isd:ied,jsd:jed,nz) )
            y = 0.
            y2 = 0.;y3 = 0.;y4 = 0.;y5 = 0.;y6 = 0.
        else
!set up x array
            call mpp_set_current_pelist( (/ (i,i=pemax+1,npes-1) /) )
            call mpp_define_layout( (/1,nx,1,ny/), mpp_npes(), layout )
            call mpp_define_domains( (/1,nx,1,ny/), layout, domainx, name=type )
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
         end if
    end select
  end subroutine setupArrays
#endif

end program test_mpp_redistribute
