	SUBROUTINE NOBL(timeout, reflag)
c
C.............................................................
C
C    SUBROUTINE FOR SOURCE EVALUATION WHEN NO GAS BLANKET
C       IS PRESENT.
C
c.............................................................
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

C
	include 'DEG1.prm'
C
	common
     ./ALP/	ALPHA,alpha1
     ./comatm/	istab,tamb,pamb,humid,isofl,tsurf,ihtfl,htco,iwtfl,wtco,
     .		humsrc
     ./comss/	ess,slen,swid,outcc,outsz,outb,outl,swcl,swal,senl,srhl
     ./ERROR/	STPIN,ERBND,WTRG,WTtm,WTya,wtyc,wteb,wtmb,XLI,
     .		XRI,EPS,ZLOW,STPINZ,ERBNDZ,SRCOER,srcss,srccut,
     .		ERNOBL,NOBLpt,crfger,epsilon
     ./GEN1/	PTIME(igen), ET(igen), R1T(igen), PWC(igen), PTEMP(igen),
     .		PFRACV(igen), PENTH(igen), PRHO(igen)
     ./parm/	u0,z0,zr,rml,ustar,vkc,gg,rhoe,rhoa,deltay,betay,gammaf,
     .		yclow
     ./PARMSC/	RM,SZM,EMAX,RMAX,TSC1,ALEPH,TEND
     ./phicom/	iphifl,dellay
     ./PHLAG/	CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
C
	LOGICAL CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
C
	logical reflag
c
	data h/0.D0/,Ri/0.D0/
	data dtmin/0.5D0/
C
c... calculate the time parameters..................................
c
	DELTAT = (TEND - TSC1)/FLOAT(NOBLPT)
	if(deltat .lt. dtmin) then
	    noblpt = int((tend-tsc1)/dtmin) +1
	    deltat = (tend-tsc1)/float(noblpt)
	endif
C
	T0 = TSC1
	IF(DELTAT .LT. 2.) GO TO 100
C
	WRITE(lunlog,1100)
	WRITE(lunlog,*) DELTAT
 1100	FORMAT(5X,'TIME INCREMENT USED ON LAST PORTION OF SOURCE CALC')
C
  100	CONTINUE
C
C... ESTABLISH LOOP TO FINISH SOURCE
C
	DO 110 I = 1,NOBLPT
C
	TIME = T0 + FLOAT(I)*DELTAT
	IF(I .EQ. NOBLPT) TIME = TEND
	rL = 2.0D0*AFGEN2(PTIME,R1T,TIME,'R1T-BL')
	erate = AFGEN2(PTIME,ET,TIME,'ET-BL')
	flux = EraTe/(pi*rL*rL/4.D0)
	PWCP = AFGEN2(PTIME,PWC,TIME,'ET-BL')
	PWAP = (1.D0 - PWCP)/(1.D0 + HUMID)
	HPRIM = AFGEN2(PTIME,PENTH,TIME,'ET-BL')
	CALL SETDEN(PWCP, PWAP, HPRIM)
	RHOP = AFGEN2(PTIME,PRHO,TIME,'RH-BL')
	CCP = PWCP * RHOP
c
	qstar = CCP * vkc*ustar*alpha1*dellay/(dellay-1.D0)/
     .						phihat(RHOP,rL)
	if(abs(flux/qstar) .gt. ernobl .and. reflag) then
	    check3 = .true.
	    timeout = time
	    return
	endif
C
	call szf(flux,rL,PWCP,sz,cclay,wclay,rholay)
	cc = cclay*dellay
	if(cc.gt.ccp) cc=ccp
c
c... Ensure that the SZF calculated values close the material balance
c
	twidth = pi*rL/4.D0
	qqqq = erate/u0/z0*alpha1 * z0**alpha1 / twidth
	cc = qqqq/sz**alpha1
	if(cc.gt.ccp) cc=ccp
	sz = (qqqq/cc)**(1.D0/alpha1)
c
	call adiabat(0,wc,wa,yc,ya,cc,rho,wm,enth,temp)
c
	IF(Erate .ge. EMAX) then
	    EMAX = Erate
	    RM = AFGEN2(PTIME,R1T,TIME,'R1T-BL')
	    SZM = SZ
	endif

	RLIST = AFGEN2(PTIME,R1T,TIME,'R1T-BL')
	RMAX = dMAX1(RMAX,RLIST)
c
	WRITE(9,2000) TIME,RLIST,h,flux,SZ,yc,ya,rho,Ri,wc,wa,enth,temp
c
	if(i.eq.3  .and.  check4) goto 500	! steady
C
  110	CONTINUE
	RETURN
c
c
  500	continue		! steady state completion
	outcc = cc
	swcl = wc
	swal = wa
	senl = enth
	srhl = rho
	outsz = sz
	outl  = 2.D0 * rlist
	outb  = pi * rlist**2 /outl/2.D0
	return
 2000	format(1pg16.9,1x,1pg16.9,11(1x,1pg13.6))
	END
