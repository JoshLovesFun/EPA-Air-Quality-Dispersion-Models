fflags=/fp:strict /align:dcommon /noauto

jetpluin.exe: jetpluin.obj atmdef.obj dattim.obj limit.obj runjet.obj runname.obj psif.obj tprop.obj trap.obj zbrent.obj
	xilink jetpluin.obj atmdef.obj dattim.obj limit.obj runjet.obj runname.obj psif.obj tprop.obj trap.obj zbrent.obj

	rem ifort compiler flag /arch:ia32 will generate code that will only
	rem run in a 32 bit environment

jetpluin.obj: $*.for
	ifort /c $(fflags) $*.for
atmdef.obj: $*.for
	ifort /c $(fflags) $*.for
dattim.obj: $*.for
	ifort /c $(fflags) $*.for
limit.obj: $*.for
	ifort /c $(fflags) $*.for
runjet.obj: $*.for
	ifort /c $(fflags) $*.for
runname.obj: $*.for
	ifort /c $(fflags) $*.for
psif.obj: $*.for
	ifort /c $(fflags) $*.for
tprop.obj: $*.for
	ifort /c $(fflags) $*.for
trap.obj: $*.for
	ifort /c $(fflags) $*.for
zbrent.obj: $*.for
	ifort /c $(fflags) $*.for
