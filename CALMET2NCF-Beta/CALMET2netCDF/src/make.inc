#----------------------------------------------------------------------
# Portland fortran 90 compiling options
#----------------------------------------------------------------------
F90 = pgf90
F77 = /pgf77
#FFLAGS = -Mlfs -byteswapio -O2
FFLAGS = -Mlfs -O2
FFLAGS_FREEFORM = -Mfreeform
LFLAGS = -Mlfs
#LIB_PGI_LFS = -L/opt/pgi/linux86/6.0/liblf
LIB_PGI_LFS =

#----------------------------------------------------------------------
# Portland C compiling options
#----------------------------------------------------------------------
PGCC = pgcc
PGCCFLAGS = -c -DUNDERSCORE -DLITTLE -Mlfs

#----------------------------------------------------------------------
# gcc compiling options
#----------------------------------------------------------------------
GCC = gcc
GCCFLAGS = -O2 -Wall -pipe -march=i486
GCC_LDFLAGS = -ldl -pthread

##########################
# Intel compiling options
##########################
F90 = ifort
FFLAGS = -check all -warn all -O2 -traceback
#FFLAGS = -O2 -traceback

##################################
# Windows MINGW compiling options
##################################
F90 = gfortran 
FFLAGS = -O3 -I/mingw/include -Bstatic -L/mingw/lib/w32api -L/mingw/lib/gcc/mingw32/4.5.0 -static-libgcc -static-libgfortran

#----------------------------------------------------------------------
# Models3 ioapi libs/include, netcdf lib, and readmm5 lib
#----------------------------------------------------------------------
LIB_M3API     = -L$(CALMET2NETCDFHOME)/ioapi-30/2011-11-30/mingw_x86gfortran -lioapi
INCLUDE_M3API = -I$(CALMET2NETCDFHOME)/ioapi-30/2011-11-30/ioapi
LIB_NETCDF = -L/mingw/lib -lnetcdf

#----------------------------------------------------------------------
# SDE options
#----------------------------------------------------------------------
#SDE = /home/tap/sde/sdeexe82
#SDEINC = -I$(SDE)/include
#SDELIB = $(SDE)/lib
#SDE_LDFLAGS = $(GCC_LDFLAGS) -Wl,-rpath=$(SDELIB) -L$(SDELIB) -lsde82 -lsg82 -lpe82
