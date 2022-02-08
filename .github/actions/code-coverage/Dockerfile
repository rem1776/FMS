FROM intel/oneapi-hpckit:2022.1.1-devel-ubuntu18.04

COPY cov-build.sh /build.sh 

RUN apt-get update && \
    apt-get install -y libnetcdf-dev libnetcdff-dev netcdf-bin autoconf libtool

ENTRYPOINT ["/build.sh"]
