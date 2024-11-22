#!/bin/csh -f

########################################################################
# sets the required environment variables for calmet2netCDF & runs script
#
# Be sure that the executable is somewhere on your path or update
# the last line to the full script for your executable
#
# CALMET - Path to CALMET output
# CALGRID2D - Output file name for Static fields
# CALMET2D - Output file path for 2D met output
# CALMET3D - Output file path for 3D met output
########################################################################
# disable prompting for values
setenv PROMPTFLAG FALSE

# CALMET output file to be converted to netcdf
setenv CALMET ../inputs/calmet.2005.01.met

# Static NetCDF output file path
setenv CALGRID2D linux.calmet_grid_2d.nc

# 2D meteorlogical NetCDF output file path
setenv CALMET2D linux.calmet_met_2d.nc

# 3D meteorlogical NetCDF output file path
setenv CALMET3D linux.calmet_met_3d.nc

CALMET2netCDF
