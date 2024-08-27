set COMPILE_FLAGS=-O2 -Z3
set LINK_FLAGS=-O2

f95   -c %COMPILE_FLAGS% mod_StartVars.f  
f95   -c %COMPILE_FLAGS% mod_Constants.f       
f95   -c %COMPILE_FLAGS% mod_UserParams.f       
f95   -c %COMPILE_FLAGS% mod_FileUnits.f       
f95   -c %COMPILE_FLAGS% mod_ErrorHandling.f
f95   -c %COMPILE_FLAGS% mod_Geographic.f
f95   -c %COMPILE_FLAGS% mod_ProcCtrlFile.f
f95   -c %COMPILE_FLAGS% mod_LandCoverParams.f
f95   -c %COMPILE_FLAGS% mod_TiffTags.f
f95   -c %COMPILE_FLAGS% mod_TiffParams.f       
f95   -c %COMPILE_FLAGS% mod_InitTiffParams.f
f95   -c %COMPILE_FLAGS% mod_GetData.f  
f95   -c %COMPILE_FLAGS% mod_AvgParams.f       
f95   -c %COMPILE_FLAGS% mod_SfcChars.f        
f95   -c %COMPILE_FLAGS% aersurface.f    

f95   -o aersurface.exe %LINK_FLAGS% mod_StartVars.obj mod_Constants.obj  mod_UserParams.obj mod_FileUnits.obj ^
mod_ErrorHandling.obj mod_Geographic.obj  mod_ProcCtrlFile.obj mod_LandCoverParams.obj ^
mod_TiffTags.obj mod_TiffParams.obj mod_InitTiffParams.obj mod_AvgParams.obj mod_GetData.obj ^
mod_SfcChars.obj  aersurface.obj
 
del *.obj *.mod



