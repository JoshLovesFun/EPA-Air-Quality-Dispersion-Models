fflags=/fp:strict /align:dcommon /noauto

deg1.exe: DEG1.obj afgen.obj afgen2.obj alph.obj atmdef.obj crfg.obj dattim.obj estrt1.obj gamma.obj head.obj io.obj limit.obj nobl.obj psif.obj riphif.obj rkgst.obj runname.obj surfac.obj szf.obj tprop.obj trans1.obj trap.obj zbrent.obj
	xilink DEG1 afgen afgen2 alph atmdef crfg dattim estrt1 gamma head io limit nobl psif riphif rkgst runname surfac szf tprop trans1 trap zbrent

	rem ifort compiler flag /arch:ia32 will generate code that will only
	rem run in a 32 bit environment

DEG1.obj: DEG1.for
	rem deg1 and src1 are compiled together because compiler becomes confused
	rem over definition of common /parmsc/.  So, SRC1 is left off the
	rem dependencies and link list.
	rem Also, the following command does not work:
	rem ifort /c $(fflags) /object:deg1.obj deg1.for src1.for
	rem so deg1.for and src1.for were merged to a single file.
	ifort /c $(fflags) $*.for
afgen.obj: $*.for
	ifort /c $(fflags) $*.for
afgen2.obj: $*.for
	ifort /c $(fflags) $*.for
alph.obj: $*.for
	ifort /c $(fflags) $*.for
atmdef.obj: $*.for
	ifort /c $(fflags) $*.for
crfg.obj: $*.for
	ifort /c $(fflags) $*.for
dattim.obj: $*.for
	ifort /c $(fflags) $*.for
estrt1.obj: $*.for
	ifort /c $(fflags) $*.for
gamma.obj: $*.for
	ifort /c $(fflags) $*.for 
head.obj: $*.for
	ifort /c $(fflags) $*.for
io.obj: $*.for
	ifort /c $(fflags) $*.for
limit.obj: $*.for
	ifort /c $(fflags) $*.for
nobl.obj: $*.for
	ifort /c $(fflags) $*.for
psif.obj: $*.for
	ifort /c $(fflags) $*.for
riphif.obj: $*.for
	ifort /c $(fflags) $*.for
rkgst.obj: $*.for
	ifort /c $(fflags) $*.for
runname.obj: $*.for
	ifort /c $(fflags) $*.for
src1.obj: $*.for
	ifort /c $(fflags) $*.for
surfac.obj: $*.for
	ifort /c $(fflags) $*.for
szf.obj: $*.for
	ifort /c $(fflags) $*.for
tprop.obj: $*.for
	ifort /c $(fflags) $*.for
trans1.obj: $*.for
	ifort /c $(fflags) $*.for
trap.obj: $*.for
	ifort /c $(fflags) $*.for
zbrent.obj: $*.for
	ifort /c $(fflags) $*.for
