#!/bin/bash
if ( [ "$1" == "-h" ] || [ "$1" == "--help" ] ) ; then
    echo "Usage: compile_netcdf_windows.sh"
    echo "Used to compile the netCDF libraries under MinGW, for Windows"
    exit
fi

# Set the version of netCDF to get/compile.  3.6.x and 4.1.[23] did not compile
# cleanly (with no further user intervention) under MinGW 20110802.

ver=4.1.1

if [ ! -e netcdf-$ver.tar.gz ]; then
    echo "Downloading netCDF"
echo  wget --quiet https://www.gfd-dennou.org/arch/ucar/netcdf/old/netcdf-$ver.tar.gz
fi
# Unpack the tar file
exit

echo "Unpacking netCDF"
tar zvxf netcdf-$ver.tar.gz
mv netcdf-$ver netcdf-$ver.gfortran
cd netcdf-$ver.gfortran
exit

# Run configure

echo "Configuring netCDF"
./configure --prefix=$PWD.gfortran --enable-f90 --disable-cxx --disable-cxx-4 2>&1 | tee  netcdf.configure.out
exit
./aaa.sh 2>&1 | tee -a log

# Compile

echo "Compiling netCDF"
make >& netcdf.make.out 2>&1

# Install, though it doesn't really install anything in the MS Windows system

echo "Intalling netCDF"
make install >& netcdf.make.install.out 2>&1
