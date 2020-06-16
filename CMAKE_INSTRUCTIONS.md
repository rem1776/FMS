# Instructions for building FMS with cmake
##############################
# 1 Environment Variables
# Set the environment variables FC, CC, FFLAGS and CPPFLAGS. CFLAGS may also be needed. For FFLAGS are many compiler options possible, and these below are some working examples:

# For GNU compilers on linux with the bash shell:

export FC=mpifort

export CC=mpicc

export FFLAGS="-fcray-pointer -fdefault-double-8 -fdefault-real-8 -Waliasing -ffree-line-length-none -fno-range-check `nf-config --fflags ` "

export CPPFLAGS="`nc-config --cflags `"

# For Intel Compilers on Linux with the bash shell:

export FC=mpiifort

export CC=mpiicc

export FFLAGS="-fno-alias -auto -safe-cray-ptr -ftz -assume byterecl -i4 -r8 -sox -traceback  `nf-config --fflags ` "

export CPPFLAGS="`nc-config --cflags `"

export CFLAGS="`nc-config --cflags `"

# For the Cray CCE Compilers, a posible selection of the FFLAGS can be set as this:

export FFLAGS="-s real64 -s integer32 -h byteswapio -h nosecond_underscore -e m -h keepfiles -e0 -ez -N1023 `nf-config --fflags ` "

##############################
# 2 Use FMS Autotools files for generating configuration information for cmake:

cd FMS

autoreconf -i

./configure

Notes: The above commands create the files FMSConfig.cmake.in and FMSConfigVersion.cmake.in. Also, the autotools
command "make" and "make install" are not run.
##########################################
# 3 Have cmake build and install FMS.
# (Below, fms_install_path is the full install directory path name  you select):

cd FMS

mkdir build && cd build

cmake -DCMAKE_INSTALL_PREFIX:PATH=fms_install_path ..

cmake --build . --target install --config Release

# When the above command finishes, the fms_install_path will have an include and a lib directory. The lib directory will have these files:

     libFMS.a
     /cmake/FMS/FMSTargets.cmake
     /cmake/FMS/FMSTargets-noconfig.cmake
     /cmake/FMS/FMSConfig.cmake
     /cmake/FMS/FMSConfigVersion.cmake
######################
