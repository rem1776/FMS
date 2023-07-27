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

# This is an automake file for the test_fms/diag_manager directory of the FMS
# package.
# uramirez, Ed Hartnett

# Find the needed mod and .inc files.
AM_CPPFLAGS = -I$(top_srcdir)/include  -I$(top_srcdir)/diag_manager/include -I$(MODDIR)

# Link to the FMS library.
LDADD = $(top_builddir)/libFMS/libFMS.la

# Build this test program.
check_PROGRAMS = test_diag_manager test_diag_manager_time test_diag_update_buffer

# This is the source code for the test.
test_diag_manager_SOURCES = test_diag_manager.F90
test_diag_manager_time_SOURCES = test_diag_manager_time.F90
test_diag_update_buffer_SOURCES= test_diag_update_buffer.F90

if DO_SCALING_TESTS
 TESTS_ENVIRONMENT= TEST_DIAG_MANAGER2_1_RANKS=1028 \
                    TEST_DIAG_MANAGER2_2_RANKS=1028 \
                    TEST_DIAG_MANAGER2_3_RANKS=1028 \
                    TEST_DIAG_MANAGER2_4_RANKS=1028 \
                    TEST_DIAG_MANAGER2_5_RANKS=1028 \
                    TEST_DIAG_MANAGER2_6_RANKS=1028 \
                    TEST_DIAG_MANAGER2_7_RANKS=1028 \
                    TEST_DIAG_MANAGER2_8_RANKS=1028 \
                    TEST_DIAG_MANAGER2_9_RANKS=1028 \
                    TEST_DIAG_MANAGER2_10_RANKS=1028 \
                    TEST_DIAG_MANAGER2_11_RANKS=1028 \
                    TEST_DIAG_MANAGER2_12_RANKS=1028 \
                    TEST_DIAG_MANAGER2_13_RANKS=1028 \
                    TEST_DIAG_MANAGER2_14_RANKS=1028 \
                    TEST_DIAG_MANAGER2_15_RANKS=1028 \
                    TEST_DIAG_MANAGER2_16_RANKS=1028 \
                    TEST_DIAG_MANAGER2_17_RANKS=1028 \
                    TEST_DIAG_MANAGER2_18_RANKS=1028 \
                    TEST_DIAG_MANAGER2_19_RANKS=1028 \
                    TEST_DIAG_MANAGER2_20_RANKS=1028 \
                    TEST_DIAG_MANAGER2_21_RANKS=1028 \
                    TEST_DIAG_MANAGER2_22_RANKS=1028 \
                    TEST_DIAG_MANAGER2_23_RANKS=1028
endif


TEST_EXTENSIONS = .sh
SH_LOG_DRIVER = env AM_TAP_AWK='$(AWK)' $(SHELL) \
                  $(abs_top_srcdir)/test_fms/tap-driver.sh

# Run the test.
TESTS = test_diag_manager2.sh

# Copy over other needed files to the srcdir
EXTRA_DIST = test_diag_manager2.sh

CLEANFILES = input.nml *.nc *.out diag_table *-files/* *.dpi *.spi *.dyn *.spl
