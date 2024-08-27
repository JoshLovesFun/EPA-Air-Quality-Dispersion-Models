@REM
@echo off

setlocal

set COMPILE_FLAGS1=/compile_only /O2 /check:format /Qipo /Qprec-div- /QaxSSE2 /trace
set COMPILE_FLAGS2=/compile_only /O2 /check:format /Qipo /Qprec-div- /QaxSSE2 /trace /assume:byterecl
set LINK_FLAGS=/O2 /Qipo /check:format /Qprec-div- /QaxSSE2

ifort %COMPILE_FLAGS1% mod_StartVars.f
ifort %COMPILE_FLAGS1% mod_Constants.f
ifort %COMPILE_FLAGS1% mod_UserParams.f
ifort %COMPILE_FLAGS1% mod_FileUnits.f
ifort %COMPILE_FLAGS1% mod_ErrorHandling.f
ifort %COMPILE_FLAGS2% mod_Geographic.f
ifort %COMPILE_FLAGS1% mod_ProcCtrlFile.f
ifort %COMPILE_FLAGS1% mod_LandCoverParams.f
ifort %COMPILE_FLAGS2% mod_TiffTags.f
ifort %COMPILE_FLAGS1% mod_TiffParams.f
ifort %COMPILE_FLAGS1% mod_InitTiffParams.f
ifort %COMPILE_FLAGS2% mod_GetData.f
ifort %COMPILE_FLAGS1% mod_AvgParams.f
ifort %COMPILE_FLAGS1% mod_SfcChars.f
ifort %COMPILE_FLAGS1% aersurface.f

ifort -o aersurface.exe %LINK_FLAGS% mod_StartVars.obj mod_Constants.obj mod_UserParams.obj mod_FileUnits.obj mod_ErrorHandling.obj ^
mod_Geographic.obj mod_ProcCtrlFile.obj mod_LandCoverParams.obj mod_TiffTags.obj mod_TiffParams.obj mod_InitTiffParams.obj ^
mod_GetData.obj mod_AvgParams.obj mod_SfcChars.obj aersurface.obj

del *.obj *.mod
