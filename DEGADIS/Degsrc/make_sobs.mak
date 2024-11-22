fflags=/fp:strict /align:dcommon /noauto

deg2s.exe: DEG2S.obj afgen.obj dattim.obj estrt2ss.obj gamma.obj incgamma.obj limit.obj pss.obj pssoutss.obj riphif.obj rkgst.obj runname.obj series.obj ssg.obj ssgoutss.obj ssout.obj strt2ss.obj surfac.obj tprop.obj trap.obj zbrent.obj
  xilink DEG2S.obj afgen.obj dattim.obj estrt2ss.obj gamma.obj incgamma.obj limit.obj pss.obj pssoutss.obj riphif.obj rkgst.obj runname.obj series.obj ssg.obj ssgoutss.obj ssout.obj strt2ss.obj surfac.obj tprop.obj trap.obj zbrent.obj

	rem ifort compiler flag /arch:ia32 will generate code that will only
	rem run in a 32 bit environment

DEG2S.obj: $*.for
	ifort /c $(fflags) $*.for
afgen.obj: $*.for
	ifort /c $(fflags) $*.for
dattim.obj: $*.for
	ifort /c $(fflags) $*.for
estrt2ss.obj: $*.for
	ifort /c $(fflags) $*.for
gamma.obj: $*.for
	ifort /c $(fflags) $*.for
incgamma.obj: $*.for
	ifort /c $(fflags) $*.for
limit.obj: $*.for
	ifort /c $(fflags) $*.for
pss.obj: $*.for
	ifort /c $(fflags) $*.for
pssoutss.obj: $*.for
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
ssgoutss.obj: $*.for
	ifort /c $(fflags) $*.for
ssout.obj: $*.for
	ifort /c $(fflags) $*.for
strt2ss.obj: $*.for
	ifort /c $(fflags) $*.for
surfac.obj: $*.for
	ifort /c $(fflags) $*.for
tprop.obj: $*.for
	ifort /c $(fflags) $*.for
trap.obj: $*.for
	ifort /c $(fflags) $*.for
zbrent.obj: $*.for
	ifort /c $(fflags) $*.for
