#!/bin/sh
set -e
mkdir coverage-build
cd coverage-build

export FC=mpiifort
export CC=mpiicc
export FCFLAGS="`nf-config --fflags`"
export CFLAGS="`nc-config --cflags`"

autoreconf -i ../configure.ac
../configure --enable-code-coverage --enable-mixed-mode
make check-code-coverage
# read coverage from html (TODO skip html creation if possible and get data directly)
for dir in `find coverage-report -maxdepth 1 -mindepth 1 -type d -printf '%f\n'`
do
  funct_cov="`sed -n '57p' coverage-report/fms/CodeCoverage/__CODE_COVERAGE.HTML | sed 's/<.*>.//' | sed 's/<.*>//'`"
  echo "::notice:: $dir function coverage: ${funct_cov}%"
done
