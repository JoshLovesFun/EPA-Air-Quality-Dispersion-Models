fflags=/align:dcommon /fp:strict /auto
deg4.exe: DEG4.obj dattim.obj dosout.obj estrt3.obj gamma.obj gettd.obj incgamma.obj limit.obj rkgst.obj runname.obj series.obj sorts.obj sorts1.obj strt3.obj tprop.obj trap.obj ts.obj zbrent.obj
	xilink DEG4.obj dattim.obj dosout.obj estrt3.obj gamma.obj gettd.obj incgamma.obj limit.obj rkgst.obj runname.obj series.obj sorts.obj sorts1.obj strt3.obj tprop.obj trap.obj ts.obj zbrent.obj

	rem ifort compiler flag /arch:ia32 will generate code that will only
	rem run in a 32 bit environment

DEG4.obj: $*.for
	ifort /c $(fflags) $*.for
dattim.obj: $*.for
	ifort /c $(fflags) $*.for
dosout.obj: $*.for
	ifort /c $(fflags) $*.for
estrt3.obj: $*.for
	ifort /c $(fflags) $*.for
gamma.obj: $*.for
	ifort /c $(fflags) $*.for
gettd.obj: $*.for
	ifort /c $(fflags) $*.for
incgamma.obj: $*.for
	ifort /c $(fflags) $*.for
limit.obj: $*.for
	ifort /c $(fflags) $*.for
rkgst.obj: $*.for
	ifort /c $(fflags) $*.for
runname.obj: $*.for
	ifort /c $(fflags) $*.for
series.obj: $*.for
	ifort /c $(fflags) $*.for
sorts.obj: $*.for
	ifort /c $(fflags) $*.for
sorts1.obj: $*.for
	ifort /c $(fflags) $*.for
strt3.obj: $*.for
	ifort /c $(fflags) $*.for
tprop.obj: $*.for
	ifort /c $(fflags) $*.for
trap.obj: $*.for
	ifort /c $(fflags) $*.for
ts.obj: $*.for
	ifort /c $(fflags) $*.for
zbrent.obj: $*.for
	ifort /c $(fflags) $*.for
