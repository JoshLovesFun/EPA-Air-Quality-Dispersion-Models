
README for MMIF VERSION 4.1.1 2024-10-30, the Mesoscale Model InterFace Program.

The Windows executable file "mmif.exe" was compiled on Windows 7 using
gfortran and the MinGW/MSYS environment.  See Section 3.1 of the MMIF 
Users Manual for full details of how to compile under Windows, but
here's a synopsis:

 - Double-click mingw-get-inst-20110802.exe to install MinGW/MSYS
 - Start the MinGW Shell (Windows Start >> All Programs >> MinGW)
 - cd to the correct directory (this directory)
 - Type "compile_netcdf_windows.sh" to (re-) compile NetCDF
   (will create the directory netcdf-4.1.1-mingw
 - Type "compile_mmif_windows.sh" to compile MMIF for Windows

The Linux executable file "mmif" was compiled on Linux using the
Portland Group FORTRAN compiler pgf90 version 19.10-0 on x86-64 
(CentOS 7.4.1708, kernel 3.10.0-693.el7.x86_64).  To re-compile:

 - make clean
 - make

The makefile contains blocks suitable for using pgf90, ifort and
gfortran on Windows using MinGW (or gfortran on Linux, with some
changes to the NETCDF path).  It also contains compiler flags for 
static libraries using pgf90.  

The Windows executables was created using the NetCDF libraries v4.1.1.
The Linux executable was created using netcdf-c-4.7.2, 
netcdf-fortran-4.5.2, and hdf5-1.8.20, and _should_ support NetCDF4.
Testing with NetCDF4 WRFOUT files will continue. 

See the User's Manual "MMIFv4.0_Users_Manual.pdf" for more information.  

Skip straight to Section 4.0 to learn how to run MMIF.  Example input
files are found in the test_problems directories.  And type 
"mmif --help" for usage:

Usage: mmif [-h | --help] [--force] [--sample] [filename]
Where
  --force   don't stop execution after non-fatal errors
  --sample  write a sample control file to the screen
  --version print the version and exit
  -h        show this help message
  --help    show this help message
 filename   control filename, default is 'mmif.inp'
 
Tip: use 'mmif mmif.inp > mmif.out' to re-direct the screen output to a file.
 
Tip: use 'mmif --sample > mmif.inp' to create a MMIF input file.

Chris Emery & Prakash Karamchandani
cemery@ramboll.com; pkaramchandani@ramboll.com
