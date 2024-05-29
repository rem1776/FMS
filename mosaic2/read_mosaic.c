/***********************************************************************
 *                   GNU Lesser General Public License
 *
 * This file is part of the GFDL Flexible Modeling System (FMS).
 *
 * FMS is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at
 * your option) any later version.
 *
 * FMS is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FMS.  If not, see <http://www.gnu.org/licenses/>.
 **********************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include "read_mosaic.h"
#include "constant.h"
#include "mosaic_util.h"
#include <netcdf.h>

/** \file
 *  \ingroup mosaic
 *  \brief Support for reading mosaic netcdf grid files.
 */

double get_global_area(void)
{
  double garea;
  garea = 4*M_PI*RADIUS*RADIUS;

  return garea;
}

double get_global_area_(void)
{
  double garea;
  garea = 4*M_PI*RADIUS*RADIUS;

  return garea;
}

