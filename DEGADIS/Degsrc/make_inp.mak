fflags=/fp:strict /noauto /align:dcommon

deginp.exe: DEGINP.obj atmdef.obj dattim.obj dirdir.obj iquery.obj limit.obj rundeg.obj tprop.obj trap.obj upcase.obj zbrent.obj
	xilink DEGINP.obj atmdef.obj dattim.obj dirdir.obj iquery.obj limit.obj rundeg.obj tprop.obj trap.obj upcase.obj zbrent.obj

	rem ifort compiler flag /arch:ia32 will generate code that will only
	rem run in a 32 bit environment

DEGINP.obj: $*.for
	ifort /c $(fflags) $*.for
atmdef.obj: $*.for
	ifort /c $(fflags) $*.for
dattim.obj: $*.for
	ifort /c $(fflags) $*.for
dirdir.obj: $*.for
	ifort /c $(fflags) $*.for
iquery.obj: $*.for
	ifort /c $(fflags) $*.for
limit.obj: $*.for
	ifort /c $(fflags) $*.for
rundeg.obj: $*.for
	ifort /c $(fflags) $*.for
tprop.obj: $*.for
	ifort /c $(fflags) $*.for
trap.obj: $*.for
	ifort /c $(fflags) $*.for
upcase.obj: $*.for
	ifort /c $(fflags) $*.for
zbrent.obj: $*.for
	ifort /c $(fflags) $*.for
