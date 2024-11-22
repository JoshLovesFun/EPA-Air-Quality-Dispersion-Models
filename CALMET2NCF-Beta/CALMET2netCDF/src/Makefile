#
#  Makefile for calmet2netCDF for emerald.
#

include make.inc
TARGETS = CALMET2netCDF.exe
BIN_DIR = ..

$(TARGETS):	CALMET2netCDF.f90
	$(F90) $< -o $@ $(FFLAGS) $(INCLUDE_M3API) $(LIB_M3API) $(LIB_NETCDF)

clean:
	rm -f $(TARGETS)

install:
	cp -f $(TARGETS) $(BIN_DIR)
