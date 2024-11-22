fflags=/fp:strict /align:dcommon /noauto

jetplu.exe: jetplu.obj dattim.obj ellips.obj erf.obj gamma.obj incgamma.obj limit.obj psif.obj rkgst.obj runname.obj simul.obj tprop.obj trap.obj zbrent.obj
	xilink jetplu.obj dattim.obj ellips.obj erf.obj gamma.obj incgamma.obj limit.obj psif.obj rkgst.obj runname.obj simul.obj tprop.obj trap.obj zbrent.obj

	rem ifort compiler flag /arch:ia32 will generate code that will only
	rem run in a 32 bit environment

jetplu.obj: $*.for
	ifort /c $(fflags) $*.for
dattim.obj: $*.for
	ifort /c $(fflags) $*.for
ellips.obj: $*.for
	ifort /c $(fflags) $*.for
erf.obj: $*.for
	ifort /c $(fflags) $*.for
gamma.obj: $*.for
	ifort /c $(fflags) $*.for
incgamma.obj: $*.for
	ifort /c $(fflags) $*.for
limit.obj: $*.for
	ifort /c $(fflags) $*.for
psif.obj: $*.for
	ifort /c $(fflags) $*.for
rkgst.obj: $*.for
	ifort /c $(fflags) $*.for
runname.obj: $*.for
	ifort /c $(fflags) $*.for
simul.obj: $*.for
	ifort /c $(fflags) $*.for
tprop.obj: $*.for
	ifort /c $(fflags) $*.for
trap.obj: $*.for
	ifort /c $(fflags) $*.for
zbrent.obj: $*.for
	ifort /c $(fflags) $*.for
