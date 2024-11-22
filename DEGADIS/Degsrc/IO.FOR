	SUBROUTINE IO(gmass,OPNRUP)
c
C..........................................................
C
C	INPUT SUBROUTINE FOR DEGADIS MODEL
C
c..........................................................
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )
c
	include 'DEG1.prm'
C
	common
     ./cgprop/	gasmw,gastem,gasrho,gascpk,gascpp,gasulc,gasllc,gaszzc,
     .		gasnam
     ./comatm/	istab,tamb,pamb,humid,isofl,tsurf,ihtfl,htco,iwtfl,wtco,
     .		humsrc
     ./comss/	ess,slen,swid,outcc,outsz,outb,outl,swcl,swal,senl,srhl
     ./csigx/	sigxco,sigxp,sigxmd,sigxfl
     ./GEN1/	PTIME(igen), ET(igen), R1T(igen), PWC(igen), PTEMP(igen),
     .		PFRACV(igen), PENTH(igen), PRHO(igen)
     ./GEN2/	DEN(5,igen)
     ./ITI/	T1,TINP,TSRC,TOBS,TSRT
     ./NEND/	POUND
     ./oomsin/	oodist,avtime
     ./parm/	u0,z0,zr,rml,ustar,vkc,gg,rhoe,rhoa,deltay,betay,gammaf,
     .		yclow
     ./PARMSC/	rm,szm,emax,rmax,tsc1,aleph,tend
     ./PHLAG/	CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
     ./TITL/	TITLE
c
	character*3 gasnam
	character*80 TITLE(4)
	character*24 TSRC,TINP,TOBS,TSRT
C
	LOGICAL CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
c
C
	character*40 OPNRUP
C
	OPEN(UNIT=9, file=OPNRUP, status='OLD', iostat=ierr)
	if(ierr.ne.0) then
		Stop 'DEG1 could not open the input file.'
	endif
C
	DO 90 I=1,4
	READ(9,2000) TITLE(I)
   90	CONTINUE
 2000	FORMAT(A80)
C
	READ(9,*) U0,Z0,zr
	read(9,*) istab
	read(9,*) oodist, avtime
	READ(9,*) indvel,frml
c
c... set up default values for Sigma's, etc.
c
	call atmdef(zr, istab, avtime, rml, deltay, betay,
     .		deltaz, betaz, gammaz, sigxco, sigxp, sigxmd)

	if(indvel .eq. 2) rml = frml
c
	read(9,*) tamb,pamb,humid,relhum
	humsrc = 0.D0
c
	vp = watvp(tamb)
	sat = wmw/wma * vp/(pamb - vp)
	if(relhum .gt. 0.D0) then
	    humid = relhum/100.D0 * sat
	else if(humid .gt. 0.D0) then
	    relhum = 100.*humid/sat
	else
	    humid  = 0.D0
	    relhum = 0.D0
	endif
c
	read(9,*) isofl,tsurf
	read(9,*) ihtfl,htco
	read(9,*) iwtfl,wtco
	read(9,2020) gasnam
 2020	FORMAT(A3)
	read(9,*) gasmw,gastem,gasrho
	read(9,*) gascpk,gascpp
c
	if(gascpp .eq. 0.D0) then
	    gascpp = 1.D0
	    gascpk = gasmw*gascpk - 3.33D4
	endif
c
	read(9,*) gasulc,gasllc,gaszzc
	gasulc = max(gasulc, 2.D-12)
	gasllc = max(gasllc, 1.D-12)
C
	if(isofl .eq. 0) then
	   rhoe = gasrho
	   rhoa = Pamb*(1.D0+humid)*wmw/(rgas*(wmw/wma + humid))/Tamb
	   goto 105
	endif
	READ(9,*) NP
	DO 100 I=1,NP
  100	READ(9,*) DEN(1,I),DEN(2,I),den(3,I),den(4,i),den(5,i)
	RHOE = DEN(3,NP)
	RHOA = DEN(3,1)
	den(1,np+1) = 2.D0
C
  105	READ(9,*) yclow
	yclow = max(yclow, 1.D-12)
	yclow = min(yclow, gasllc)
C
	read(9,*) gmass
	READ(9,*) NP
	DO 110 I=1,NP
	READ(9,*) PTIME(I),ET(I),R1T(I), PWC(I),PTEMP(I),PFRACV(I)
 110	CONTINUE
	DO 111 I=1,NP-2
	WA = (1.D0 - PWC(I))/(1.D0 + HUMID)
	PENTH(I) = ENTHAL( PWC(I), WA, PTEMP(I))
	CALL TPROP(-1,PWC(I), WA,PENTH(I), YC,YA,WM, PTEMP(I), PRHO(I),CP)
 111	CONTINUE
	do 112 i=np-1,np
	pwc(i)    = pwc(np-2)
	ptemp(i)  = ptemp(np-2)
	pfracv(i) = pfracv(np-2)
	penth(i)  = penth(np-2)
	prho(i)   = prho(np-2)
 112	continue
	TEND = PTIME(NP-2)
	PTIME(NP + 1)  = POUND
C
	read(9,*) check4
	check1 = .false.
	check2 = .false.
	if(et(1).eq.0.D0 .and. gmass.ne.0.D0) check2=.true. ! HSE type spill
	again  = .false.
	check3 = .false.
	check5 = .false.
C
	if(check4) then
	    ess  = et(1)
	    slen = 2.D0 * R1T(1)
	    swid = pi * R1T(1)**2/2.D0/slen
	else
	    yclow = yclow/5.D0
	endif
c
	tobs = ' '
	tsrt = ' '
	READ(9,2010) TINP
 2010	format(a24)

	CLOSE(UNIT=9)
	RETURN
	END
