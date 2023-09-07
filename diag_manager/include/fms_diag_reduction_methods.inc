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

!> @brief Do the time_none reduction method (i.e copy the correct portion of the input data)
subroutine DO_TIME_NONE_ (data_out, data_in, mask, bounds_in, bounds_out)
  real(FMS_TRM_KIND_),       intent(inout) :: data_out(:,:,:,:,:) !< output data
  real(FMS_TRM_KIND_),       intent(in)    :: data_in(:,:,:,:)    !< data to update the buffer with
  logical,                   intent(in)    :: mask(:,:,:,:)       !< mask
  type(fmsDiagIbounds_type), intent(in)    :: bounds_in           !< indices indicating the correct portion
                                                                  !! of the input buffer
  type(fmsDiagIbounds_type), intent(in)    :: bounds_out          !< indices indicating the correct portion
                                                                  !! of the output buffer

  integer :: is_in, ie_in, js_in, je_in, ks_in, ke_in       !< Starting and ending indices of each dimention for
                                                            !! the input buffer
  integer :: is_out, ie_out, js_out, je_out, ks_out, ke_out !< Starting and ending indices of each dimention for
                                                            !! the output buffer

  is_out = bounds_out%get_imin()
  ie_out = bounds_out%get_imax()
  js_out = bounds_out%get_jmin()
  je_out = bounds_out%get_jmax()
  ks_out = bounds_out%get_kmin()
  ke_out = bounds_out%get_kmax()

  is_in = bounds_in%get_imin()
  ie_in = bounds_in%get_imax()
  js_in = bounds_in%get_jmin()
  je_in = bounds_in%get_jmax()
  ks_in = bounds_in%get_kmin()
  ke_in = bounds_in%get_kmax()

  where (mask(is_in:ie_in, js_in:je_in, ks_in:ke_in, :)) &
    data_out(is_out:ie_out, js_out:je_out, ks_out:ke_out, :, 1) = &
    data_in(is_in:ie_in, js_in:je_in, ks_in:ke_in, :)

end subroutine DO_TIME_NONE_

  !> @brief Updates the buffer with the field data based on the value of the flag passed:
  !! time_min for minimum; time_max for maximum.
  subroutine FMS_DIAG_UPDATE_EXTREMUM_(flag, buffer, counter, count_0d, field_data, recon_bounds, l_start, &
    l_end, is_regional, reduced_k_range, sample, mask, fieldName, hasDiurnalAxis, err_msg)
    integer, intent(in) :: flag !< Flag to indicate what to update: time_min for minimum; time_max for maximum
    real(FMS_TRM_KIND_), target, intent(inout) :: buffer(:,:,:,:,:) !< buffer data from output buffer object
    real(r8_kind), intent(inout) :: counter(:,:,:,:,:), count_0d !< counts used for weighted reductions
    real(FMS_TRM_KIND_), intent(in) :: field_data(:,:,:,:) !< Field data
    type(fmsDiagBoundsHalos_type), intent(inout) :: recon_bounds !< Indices of bounds in the first three dimension
                                                                 !! of the field data
    integer, intent(in) :: l_start(:) !< Local starting indices for the first three dimensions
    integer, intent(in) :: l_end(:)   !< Local ending indices for the first three dimensions
    logical, intent(in) :: is_regional !< Flag indicating if the current PE takes part in send_data
    logical, intent(in) :: reduced_k_range !< Flag indicating if the field has zbounds
    integer, intent(in) :: sample !< Index along the diurnal time axis
    logical, intent(in) :: mask(:,:,:,:) !< Must be out of range mask
    character(len=*), intent(in) :: fieldName !< Field name for error reporting
    logical, intent(in) :: hasDiurnalAxis !< Flag to indicate if the buffer has a diurnal axis
    character(len=*), intent(inout), optional :: err_msg !< Error mesage to report back

    integer :: is, js, ks !< Starting indices in the I, J, and K dimensions
    integer :: ie, je, ke !< Ending indices in the I, J, and K dimensions
    integer :: hi, hj !< Halo sizes in the I, and J dimensions
    integer :: f1, f2 !< Updated starting and ending indices in the I dimension
    integer :: f3, f4 !< Updated starting and ending indices in the J dimension
    integer :: ksr, ker !< Reduced indices in the K dimension
    integer :: i, j, k !< For loops
    integer :: i1, j1, k1 !< Intermediate computed indices
    character(len=128) :: err_msg_local !< Stores local error message
    type(fmsDiagIbounds_type) :: IJKBounds !< Bounding object for the I, J, and K indices

    !> Get the `bounds3D` member of the `recon_bounds`
    IJKBounds = recon_bounds%get_bounds3D() !< Assignment of data structure with intrinsic type members may work!!!

    !> Unpack recon_bounds
    is = IJKBounds%get_imin()
    js = IJKBounds%get_jmin()
    ks = IJKBounds%get_kmin()
    ie = IJKBounds%get_imax()
    je = IJKBounds%get_jmax()
    ke = IJKBounds%get_kmax()
    hi = recon_bounds%get_hi()
    f1 = recon_bounds%get_fis()
    f2 = recon_bounds%get_fie()
    hj = recon_bounds%get_hj()
    f3 = recon_bounds%get_fjs()
    f4 = recon_bounds%get_fje()

    if (flag .ne. 3 .and. flag .ne. 4) then
      call mpp_error( FATAL, "fms_diag_reduction_methods_mod::fms_diag_update_extremum: flag must be either 3 or 4.")
    end if

    ! Update buffer
    regional_if: IF (is_regional) THEN
      DO k = l_start(3), l_end(3)
        k1 = k - l_start(3) + 1
        DO j = js, je
          DO i = is, ie
            IF ( l_start(1)+hi <= i .AND. i <= l_end(1)+hi .AND. l_start(2)+hj <= j .AND. &
              & j <= l_end(2)+hj ) THEN
              i1 = i-l_start(1)-hi+1
              j1=  j-l_start(2)-hj+1
              call update_scalar_extremum(flag, field_data, buffer, mask, sample, &
                                          recon_bounds, (/i,j,k/), (/i1,j1,k1/)) ! TODO fix call
            end if
          END DO
        END DO
      END DO
    ELSE !< if not regional
      reduced_k_range_if: IF (reduced_k_range) THEN
        call IJKBounds%set_kbounds(l_start(3), l_end(3))
        call update_array_extremum(flag, field_data, buffer, mask, sample, recon_bounds, reduced_k_range)
      ELSE !< does not have reduced_k_range
        debug_diag_if: IF ( debug_diag_manager ) THEN
          ! Compare bounds {is-hi, ie-hi, js-hj, je-hj, ks, ke} with the bounds of first three dimensions of the buffer
          if (compare_two_sets_of_bounds((/is-hi, ie-hi, js-hj, je-hj, ks, ke/), &
            (/LBOUND(buffer,1), UBOUND(buffer,1), LBOUND(buffer,2), UBOUND(buffer,2), &
            LBOUND(buffer,3), UBOUND(buffer,3)/), err_msg_local)) THEN
            IF ( fms_error_handler('fms_diag_object_mod::fms_diag_update_extremum', err_msg_local, err_msg) ) THEN
              RETURN
            END IF
          END IF
        END IF debug_diag_if

        !> If no error above, do update the buffer
        call update_array_extremum(flag, field_data, buffer, mask, sample, recon_bounds, reduced_k_range)
      END IF reduced_k_range_if
    end if regional_if

    ! Reset counter count_0d of the buffer object
    counter = 1.0_r8_kind
    ! count_0d = 1.0_r8_kind ?

  end subroutine FMS_DIAG_UPDATE_EXTREMUM_

  !> @brief Updates individual element of the buffer associated with indices in running_indx1 and running_indx2
  subroutine UPDATE_SCALAR_EXTREMUM_(flag, field_data, buffer, mask, sample, recon_bounds, &
    running_indx1, running_indx2)
    integer, intent(in) :: flag !< Flag indicating maximum(time_max) or minimum(time_min)
    real(FMS_TRM_KIND_), intent(in) :: field_data(:,:,:,:) !< Field data
    real(FMS_TRM_KIND_), intent(inout) :: buffer(:,:,:,:,:) !< Remapped output buffer
    logical, intent(in) :: mask(:,:,:,:) !< Update mask
    integer, intent(in) :: sample !< diurnal sample index
    type(fmsDiagBoundsHalos_type), intent(in) :: recon_bounds !< Holds starting and ending indices in the
                                                              !! I, J, and K dimensions and
                                                              !! halo sizes in the I, and J dimensions
    integer, intent(in) :: running_indx1(3) !< Holds indices i, j, and k
    integer, intent(in) :: running_indx2(3) !< Holds indices i1, j1, and k1

    type(fmsDiagIbounds_type) :: IJKBounds !< Bounding object for the I, J, and K indices
    integer :: i, j, k !< Unpack running_indx1 to
    integer :: i1, j1, k1 !< Unpack running_indx2 to
    integer :: is, js, ks !< Starting indices in the I, J, and K dimensions
    integer :: ie, je, ke !< Ending indices in the I, J, and K dimensiions
    integer :: hi, hj !< Halo sizes in the I, and J dimensions

    !> Check flag for unsupported operation
    if (flag .ne. time_max .and. flag .ne. time_min) then
      call mpp_error(FATAL, "fms_diag_reduction_methods_mod::fms_diag_scalar_extremum &
        unsupported reduction method")
    endif

    ! Initialize i, j, and k
    i = running_indx1(1)
    j = running_indx1(2)
    k = running_indx1(3)

    ! Initialize i1, j1, and k1
    i1 = running_indx2(1)
    j1 = running_indx2(2)
    k1 = running_indx2(3)

    !> Get the `bounds3D` member of the `recon_bounds`
    IJKBounds = recon_bounds%get_bounds3D() !< Assignment of data structure with intrinsic type members may work!!!

    !> Unpack index bounds
      is = IJKBounds%get_imin()
      js = IJKBounds%get_jmin()
      ks = IJKBounds%get_kmin()
      ie = IJKBounds%get_imax()
      je = IJKBounds%get_jmax()
      ke = IJKBounds%get_kmax()
      hi = recon_bounds%get_hi()
      hj = recon_bounds%get_hj()

    ! Select proper type and update the buffer
    if (flag .eq. time_min) then
      !> Update the buffer with the current minimum
      where (mask(i-is+1+hi,j-js+1+hj,k,:) .AND. &
             field_data(i-is+1+hi,j-js+1+hj,k,:) < buffer(i1,j1,k1,:,sample))
        buffer(i1,j1,k1,:,sample) = field_data(i-is+1+hi,j-js+1+hj,k,:)
      end where
    else !< if not minimum, check for maximum
      !> Update the buffer with the current maximum
      where (mask(i-is+1+hi,j-js+1+hj,k,:) .AND. field_data(i-is+1+hi,j-js+1+hj,k,:) >&
        buffer(i1,j1,k1,:,sample))
        buffer(i1,j1,k1,:,sample) = field_data(i-is+1+hi,j-js+1+hj,k,:)
      end where
    end if

  end subroutine UPDATE_SCALAR_EXTREMUM_

  !> @brief Updates a chunk of the buffer defined by the bounds in recon_bounds
  subroutine UPDATE_ARRAY_EXTREMUM_(flag, field_data, buffer, mask, sample, recon_bounds, reduced_k_range)
    integer :: flag !< Flag indicating maximum(time_max) or minimum(time_min)
    real(FMS_TRM_KIND_), intent(in) :: field_data(:,:,:,:) !< Field data
    real(FMS_TRM_KIND_), intent(inout) :: buffer(:,:,:,:,:) !< Remapped output buffer
    logical, intent(in) :: mask(:,:,:,:) !< Updated mask
    integer, intent(in) :: sample !< diurnal sample index
    type(fmsDiagBoundsHalos_type), intent(in) :: recon_bounds !< Object to hold starting and ending indices
                                                              !! in the I, J, and K dimensions; also holds
                                                              !! halo sizes in the I, and J dimensions
    logical, intent(in) :: reduced_k_range !< Flag indicating if the range in the K dimension is present

    integer :: is, js, ks !< Starting indices in the I, J, and K dimensions
    integer :: ie, je, ke !< Ending indices in the I, J, and K dimensions
    integer :: hi, hj !< Halo sizes in the I, and J dimensions
    integer :: f1, f2 !< Updated starting and ending indices in the I dimension
    integer :: f3, f4 !< Updated starting and ending indices in the J dimension
    type(fmsDiagIbounds_type) :: IJKBounds !< Bounding object for the I, J, and K indices

    !> Check flag for unsupported operation
    if (flag .ne. time_max .and. flag .ne. time_min) then
      call mpp_error(FATAL, "fms_diag_reduction_methods_mod::fms_diag_scalar_extremum &
        unsupported reduction method")
    endif

    !> Get the `bounds3D` member of the `recon_bounds`
    IJKBounds = recon_bounds%get_bounds3D() !< Assignment of data structure with intrinsic type members may work!!!

    !> Unpack bounds (/is, js, ks, ie, je, ke, hi, f1, f2, hj, f3, f4/)
    is = IJKBounds%get_imin()
    js = IJKBounds%get_jmin()
    ks = IJKBounds%get_kmin()
    ie = IJKBounds%get_imax()
    je = IJKBounds%get_jmax()
    ke = IJKBounds%get_kmax()
    hi = recon_bounds%get_hi()
    f1 = recon_bounds%get_fis()
    f2 = recon_bounds%get_fie()
    hj = recon_bounds%get_hj()
    f3 = recon_bounds%get_fjs()
    f4 = recon_bounds%get_fje()

    ! Select proper type and update the buffer
    if (flag .eq. time_min) then
      !> Update the buffer with the current minimum
      if (reduced_k_range) then
        ! recon_bounds must have ks = ksr and ke = ker
        WHERE (mask(f1:f2,f3:f4,ks:ke,:) .AND. field_data(f1:f2,f3:f4,ks:ke,:) <&
          buffer(is-hi:ie-hi,js-hj:je-hj,:,:,sample)) &
          buffer(is-hi:ie-hi,js-hj:je-hj,:,:,sample) = field_data(f1:f2,f3:f4,ks:ke,:)
      else
        WHERE (mask(f1:f2,f3:f4,ks:ke,:) .AND. field_data(f1:f2,f3:f4,ks:ke,:) <&
          buffer(is-hi:ie-hi,js-hj:je-hj,ks:ke,:,sample)) &
          buffer(is-hi:ie-hi,js-hj:je-hj,ks:ke,:,sample) = field_data(f1:f2,f3:f4,ks:ke,:)
      end if
    else
      !> Update the buffer with the current maximum
      if (reduced_k_range) then
        ! recon_bounds must have ks = ksr and ke = ker
        WHERE (mask(f1:f2,f3:f4,ks:ke,:) .AND. field_data(f1:f2,f3:f4,ks:ke,:) <&
          buffer(is-hi:ie-hi,js-hj:je-hj,:,:,sample)) &
          buffer(is-hi:ie-hi,js-hj:je-hj,:,:,sample) = field_data(f1:f2,f3:f4,ks:ke,:)
      else
        WHERE (mask(f1:f2,f3:f4,ks:ke,:) .AND. field_data(f1:f2,f3:f4,ks:ke,:)>&
          buffer(is-hi:ie-hi,js-hj:je-hj,ks:ke,:,sample)) &
          buffer(is-hi:ie-hi,js-hj:je-hj,ks:ke,:,sample) = field_data(f1:f2,f3:f4,ks:ke,:)
      end if
    end if
  end subroutine UPDATE_ARRAY_EXTREMUM_