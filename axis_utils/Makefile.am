#***********************************************************************
#*                   GNU Lesser General Public License
#*
#* This file is part of the GFDL Flexible Modeling System (FMS).
#*
#* FMS is free software: you can redistribute it and/or modify it under
#* the terms of the GNU Lesser General Public License as published by
#* the Free Software Foundation, either version 3 of the License, or (at
#* your option) any later version.
#*
#* FMS is distributed in the hope that it will be useful, but WITHOUT
#* ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
#* FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
#* for more details.
#*
#* You should have received a copy of the GNU Lesser General Public
#* License along with FMS.  If not, see <http://www.gnu.org/licenses/>.
#***********************************************************************

# This is an automake file for the axis_utils directory of the FMS
# package.

# Ed Hartnett 2/22/19

# Include .h and .mod files.
AM_CPPFLAGS = -I$(top_srcdir)/include -I$(top_srcdir)/axis_utils/include
AM_FCFLAGS = $(FC_MODINC). $(FC_MODOUT)$(MODDIR)

# Build this uninstalled convenience library.
noinst_LTLIBRARIES = libaxis_utils.la

libaxis_utils_la_SOURCES = \
  axis_utils2.F90 \
  include/axis_utils2_r4.fh \
  include/axis_utils2_r8.fh \
  include/axis_utils2.inc

axis_utils2.$(FC_MODEXT) : \
include/axis_utils2_r4.fh \
include/axis_utils2_r8.fh \
include/axis_utils2.inc

# Mod file depends on its o file, is built and then installed.
nodist_include_HEADERS = axis_utils2_mod.$(FC_MODEXT)
BUILT_SOURCES = axis_utils2_mod.$(FC_MODEXT)

include $(top_srcdir)/mkmods.mk
