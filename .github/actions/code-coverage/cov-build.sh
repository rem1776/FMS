#!/bin/sh
mkdir coverage-build
cd coverage-build
echo "::notice:: generating configure script..."
autoreconf -i ../configure.ac
echo "::notice:: configuring build..."
../configure --enable-code-coverage --enable-mixed-mode
echo "::notice:: building and generating report..."
make check-code-coverage
# TODO read report data and output
echo "::notice:: Report done"
