!***********************************************************************
!*                   GNU Lesser General Public License
!*
!* This file is part of the GFDL Flexible Modeling system (FMS).
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
!* License along with the FV3 dynamical core.
!* If not, see <http://www.gnu.org/licenses/>.
!***********************************************************************

function MPP_CHKSUM_( var, pelist, mask_val )
!mold is a dummy array to be used by TRANSFER()
!must be same TYPE as result
!result is LONG_KIND, which will actually be int ifdef no_8byte_integers
  !mold and mask_val must be same numBytes, otherwise undefined behavior
      integer(LONG_KIND) :: MPP_CHKSUM_
      MPP_TYPE_, intent(in) :: var
      integer, intent(in), optional :: pelist(:)
      integer(LONG_KIND) :: mold(1)
  MPP_TYPE_, intent(in), optional :: mask_val
      pointer( p, mold )

      p = LOC(var)

  if ( PRESENT(mask_val) ) then
     MPP_CHKSUM_ = mpp_chksum( mold, pelist, TRANSFER(mask_val, mold(1)) )
  else
      MPP_CHKSUM_ = mpp_chksum( mold, pelist )
  end if
      return
    end function MPP_CHKSUM_
