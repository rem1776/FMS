FROM intel/oneapi-hpckit:2022.1.1-devel-ubuntu18.04

COPY cov-build.sh /build.sh 

RUN apt-get update && \
    apt-get install -y libnetcdf-dev libnetcdff-dev netcdf-bin autoconf libtool

ENV FC=mpiifort
ENV CC=mpiicc
ENV FCFLAGS="-O0"
ENV CFLAGS="-O0"

ENTRYPOINT ["/build.sh"]
