@call "C:\Program Files\Intel\Compiler\11.1\048\bin\ia32\ifortvars_ia32.bat"

ifort /nologo /Od /c met_fields.f
ifort /nologo /Od /c site_fields.f
ifort /nologo /Od /c stat_fields.f

ifort /nologo /Od /c averag_0.f
ifort /nologo /Od /c averag.f
ifort /nologo /Od /c biaserr.f
ifort /nologo /Od /c biaserrw.f
ifort /nologo /Od /c caldate.f
ifort /nologo /Od /c getcalmet.f
ifort /nologo /Od /c getioa.f
ifort /nologo /Od /c getrmse.f
ifort /nologo /Od /c grid_calmet.f
ifort /nologo /Od /c interp.f
ifort /nologo /Od /c isearch15.f
ifort /nologo /Od /c istrln.f
ifort /nologo /Od /c juldate.f
ifort /nologo /Od /c lcpgeo.f
ifort /nologo /Od /c polgeo.f
ifort /nologo /Od /c MMIFstat.f
ifort /nologo /Od /c readasc.f
ifort /nologo /Od /c readcalhdr.f
ifort /nologo /Od /c readcalmet.f
ifort /nologo /Od /c read_statlib.f
ifort /nologo /Od /c read_tdl.f
ifort /nologo /Od /c regress.f
ifort /nologo /Od /c stats.f
ifort /nologo /Od /c uv2spdr.f
ifort /nologo /Od /c wrtfils.f

ifort /exe:"MMIFstat.exe" /Qfreestanding /DEBUG "averag_0.obj" "averag.obj" "biaserr.obj" "biaserrw.obj" "caldate.obj" "getcalmet.obj" "getioa.obj" "getrmse.obj" "grid_calmet.obj" "interp.obj" "isearch15.obj" "istrln.obj" "juldate.obj" "lcpgeo.obj" "polgeo.obj" "met_fields.obj" "MMIFstat.obj" "readasc.obj" "readcalhdr.obj" "readcalmet.obj" "read_statlib.obj" "read_tdl.obj" "regress.obj" "site_fields.obj" "stat_fields.obj" "stats.obj" "uv2spdr.obj" "wrtfils.obj"
 






