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

!> @brief  This programs tests the public subroutines in test_fms_string_utils:
!! fms_array_to_pointer, fms_pointer_to_array, fms_sort_this, fms_find_my_string
program test_fms_string_utils
#ifdef _OPENMP
  use omp_lib

  implicit none


!$OMP PARALLEL
  print *, 'hello from thread', OMP_GET_THREAD_NUM()
!$OMP END PARALLEL

#endif

end program 
