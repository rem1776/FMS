#!/bin/sh
export PATH="$PATH:../bats/bin"
bats test_cloud_interpolator.bats
