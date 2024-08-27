set COMPILE_FLAGS=-fbounds-check -Wuninitialized -O2 -static
set LINK_FLAGS=-static-libgfortran -static -O2

gfortran  -c %COMPILE_FLAGS%  mod_StartVars.f
gfortran  -c %COMPILE_FLAGS%  mod_Constants.f
gfortran  -c %COMPILE_FLAGS%  mod_UserParams.f
gfortran  -c %COMPILE_FLAGS%  mod_FileUnits.f
gfortran  -c %COMPILE_FLAGS%  mod_ErrorHandling.f
gfortran  -c %COMPILE_FLAGS%  mod_Geographic.f
gfortran  -c %COMPILE_FLAGS%  mod_ProcCtrlFile.f
gfortran  -c %COMPILE_FLAGS%  mod_LandCoverParams.f
gfortran  -c %COMPILE_FLAGS%  mod_TiffTags.f
gfortran  -c %COMPILE_FLAGS%  mod_TiffParams.f
gfortran  -c %COMPILE_FLAGS%  mod_InitTiffParams.f
gfortran  -c %COMPILE_FLAGS%  mod_GetData.f
gfortran  -c %COMPILE_FLAGS%  mod_AvgParams.f
gfortran  -c %COMPILE_FLAGS%  mod_SfcChars.f
gfortran  -c %COMPILE_FLAGS%  aersurface.f

gfortran -o aersurface.exe %LINK_FLAGS%  mod_StartVars.o  mod_Constants.o  mod_UserParams.o  ^
mod_FileUnits.o  mod_ErrorHandling.o  mod_Geographic.o  ^
mod_ProcCtrlFile.o  mod_LandCoverParams.o  mod_TiffTags.o  ^
mod_TiffParams.o  mod_InitTiffParams.o  mod_GetData.o  mod_AvgParams.o  ^
mod_SfcChars.o  aersurface.o

del *.o *.mod

