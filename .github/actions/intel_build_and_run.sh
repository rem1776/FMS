#!/bin/sh

autoreconf -if
./configure
make -j distcheck
