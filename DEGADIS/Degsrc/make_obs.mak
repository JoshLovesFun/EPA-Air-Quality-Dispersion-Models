fflags=/fp:strict /align:dcommon /noauto

deg2.exe: DEG2.obj afgen.obj afgen2.obj dattim.obj estrt2.obj gamma.obj incgamma.obj limit.obj ob.obj pss.obj pssout.obj riphif.obj rkgst.obj runname.obj series.obj ssg.obj ssgout.obj sssup.obj strt2.obj surfac.obj tprop.obj trans2.obj trap.obj ts.obj tupf.obj uit.obj zbrent.obj
	xilink DEG2 afgen afgen2 dattim estrt2 gamma incgamma limit ob pss pssout riphif rkgst runname series ssg ssgout sssup strt2 surfac tprop trans2 trap ts tupf uit zbrent

	rem ifort compiler flag /arch:ia32 will generate code that will only
	rem run in a 32 bit environment

deg2.obj: $*.for
	ifort /c $(fflags) $*.for
afgen.obj: $*.for
	ifort /c $(fflags) $*.for
afgen2.obj: $*.for
	ifort /c $(fflags) $*.for
dattim.obj: $*.for
	ifort /c $(fflags) $*.for
estrt2.obj: $*.for
	ifort /c $(fflags) $*.for
gamma.obj: $*.for
	ifort /c $(fflags) $*.for
incgamma.obj: $*.for
	ifort /c $(fflags) $*.for
limit.obj: $*.for
	ifort /c $(fflags) $*.for
ob.obj: $*.for
	ifort /c $(fflags) $*.for
pss.obj: $*.for
	ifort /c $(fflags) $*.for
pssout.obj: $*.for
	ifort /c $(fflags) $*.for
riphif.obj: $*.for
	ifort /c $(fflags) $*.for
rkgst.obj: $*.for
	ifort /c $(fflags) $*.for
runname.obj: $*.for
	ifort /c $(fflags) $*.for
series.obj: $*.for
	ifort /c $(fflags) $*.for
ssg.obj: $*.for
	ifort /c $(fflags) $*.for
ssgout.obj: $*.for
	ifort /c $(fflags) $*.for
sssup.obj: $*.for
	ifort /c $(fflags) $*.for
strt2.obj: $*.for
	ifort /c $(fflags) $*.for
surfac.obj: $*.for
	ifort /c $(fflags) $*.for
tprop.obj: $*.for
	ifort /c $(fflags) $*.for
trans2.obj: $*.for
	ifort /c $(fflags) $*.for
trap.obj: $*.for
	ifort /c $(fflags) $*.for
ts.obj: $*.for
	ifort /c $(fflags) $*.for
tupf.obj: $*.for
	ifort /c $(fflags) $*.for
uit.obj: $*.for
	ifort /c $(fflags) $*.for
zbrent.obj: $*.for
	ifort /c $(fflags) $*.for
