fflags=/fp:strict /align:dcommons /noauto

degbridg.exe: DEGBRIDG.obj atmdef.obj dattim.obj limit.obj runname.obj tprop.obj trap.obj zbrent.obj
	xilink DEGBRIDG.obj atmdef.obj dattim.obj limit.obj runname.obj tprop.obj trap.obj zbrent.obj
	rem ifort compiler flag /arch:ia32 will generate code that will only
	rem run in a 32 bit environment

DEGBRIDG.obj: $*.for
	ifort /c $(fflags) $*.for
atmdef.obj: $*.for
	ifort /c $(fflags) $*.for
dattim.obj: $*.for
	ifort /c $(fflags) $*.for
limit.obj: $*.for
	ifort /c $(fflags) $*.for
runname.obj: $*.for
	ifort /c $(fflags) $*.for
tprop.obj: $*.for
	ifort /c $(fflags) $*.for
trap.obj: $*.for
	ifort /c $(fflags) $*.for
zbrent.obj: $*.for
	ifort /c $(fflags) $*.for
