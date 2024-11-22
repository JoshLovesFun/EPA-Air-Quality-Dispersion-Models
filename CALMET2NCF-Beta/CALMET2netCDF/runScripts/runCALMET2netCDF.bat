REM disable prompting for values
SET PROMPTFLAG=TRUE

REM CALMET output file to be converted to netcdf
SET CALMETFILE=calmet.2005.01.met
SET CALMET=../%CALMETFILE%

REM Static NetCDF output file path
SET CALGRID2D=../windows_new.calmet_grid_2d.nc

REM 2D meteorlogical NetCDF output file path
SET CALMET2D=../outputs/windows_new.calmet_met_2d.nc

REM 3D meteorlogical NetCDF output file path
SET CALMET3D=../outputs/windows_new.calmet_met_3d.nc

pause
START CALMET2netCDF.exe
