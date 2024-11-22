	PROGRAM DEG1
C
C***************************************************************************
C***************************************************************************
C***************************************************************************
C
C	Program description:
C
C	DEG1 estimates the ambient wind profile power alpha and
C	characterizes the primary gas source.
C
C
C	Program usage:
C
C	Consult Volume III of the Final Report to U. S. Coast Guard
C	contract DT-CG-23-80-C-20029 entitled "Development of an
C	Atmospheric Dispersion Model for Heavier-than-Air Gas Mixtures".
C
C	J. A. Havens
C	T. O. Spicer
C
C	University of Arkansas
C	Department of Chemical Engineering
C	Fayetteville, AR 72701
C
C	April 1985
C
C
C	This project was sponsored by the U. S. Coast Guard and the Gas
C	Research Institute under contract DT-CG-23-80-C-20029.
C
C
C	Disclaimer:
C
C	This computer code material was prepared by the University of
C	Arkansas as an account of work sponsored by the U. S. Coast Guard
C	and the Gas Research Institute.  Neither the University of Arkansas,
C	nor any person acting on its behalf:
C
C	a.  Makes any warranty or representation, express or implied,
C	    with respect to the accuracy, completeness, or usefulness
C	    of the information contained in this computer code material,
C	    or that the use of any apparatus, method, numerical model,
C	    or process disclosed in this computer code material may not
C	    infringe privately owned rights; or
C
C	b.  Assumes any liability with respect to the use of, or for
C	    damages resulting from the use of, any information,
C	    apparatus, method, or process disclosed in this computer
C	    code material.
C
C
C***************************************************************************
C***************************************************************************
C***************************************************************************
C
C
C	DIMENSIONS/DECLARATIONS
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	include 'DEG1.prm'
c
c  ntab is the dimension of table divided by iousrc
c
	parameter (	ntab0=910,
     .			ntab=ntab0/iousrc,
     .			one=1.D0,
     .			zero=0.D0)
C
C	BLOCK COMMON
C
	common
     ./ALP/	ALPHA,alpha1
     ./alpcom/	ialpfl,alpco
     ./cgprop/	gasmw,gastem,gasrho,gascpk,gascpp,gasulc,gasllc,gaszzc,
     .		gasnam
     ./comatm/	istab,tamb,pamb,humid,isofl,tsurf,ihtfl,htco,iwtfl,wtco,
     .		humsrc
     ./comH/	Hmrte,Harte,Hwrte
     ./comss/	ess,slen,swid,outcc,outsz,outb,outl,swcl,swal,senl,srhl
     ./csigx/	sigxco,sigxp,sigxmd,sigxfl
     ./csprd/	ce, delrmn
     ./ctprop/	cwc,cwa,centh
     ./ERROR/	STPIN,ERBND,WTRG,WTtm,WTya,wtyc,wteb,wtmb,XLI,
     .		XRI,EPS,ZLOW,STPINZ,ERBNDZ,SRCOER,srcss,srccut,
     .		ERNOBL,NOBLpt,crfger,epsilon
     ./GEN1/	PTIME(igen), ET(igen), R1T(igen), PWC(igen), PTEMP(igen),
     .		PFRACV(igen), PENTH(igen), PRHO(igen)
     ./GEN2/	DEN(5,igen)
     ./GEN3/	radg(2,maxl),qstr(2,maxl),srcden(2,maxl),srcwc(2,maxl),
     .		srcwa(2,maxl),srcen(2,maxl)
     ./ITI/	T1,TINP,TSRC,TOBS,TSRT
     ./NEND/	POUND
     ./oomsin/	oodist,avtime
     ./parm/	u0,z0,zr,rml,ustar,vkc,gg,rhoe,rhoa,deltay,betay,gammaf,
     .		yclow
     ./PARMSC/	RM,SZM,EMAX,RMAX,TSC1,ALEPH,TEND
     ./phicom/	iphifl,dellay
     ./PHLAG/	CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
     ./SZFC/	szstp0,szerr,szsz0
     ./TITL/	TITLE
     ./vucom/	vua,vub,vuc,vud,vuflag
c
	character*3 gasnam
	character*80 TITLE(4)
	character*24 TSRC,TINP,TOBS,TSRT
C
	LOGICAL CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
	logical vuflag
C
	real*4 tt1
	logical reflag
C
	DIMENSION PRMT(25),Y(7),DERY(7),AUX(8,7)
	EXTERNAL SRC1,SRC1O
	character*40 opnf1
	character OPNf(40)
	character*4 INP,ER1,TR2,scl
c
	dimension table(ntab0)
c
	equivalence(opnf(1),opnf1)
c
C..........................................................
C
C	DATA
C
	DATA POUND/-1.D-20/
	DATA USTAR/0.D0/,GAMMAF/0.D0/
	DATA gg/9.81D0/, vkc/0.35D0/
	DATA GAMMAF/0.D0/
	DATA PRMT/25*0.D0/
	DATA Y/7*0.D0/, DERY/7*0.D0/
	DATA TIME0/0.D0/,NDIM/0/
	DATA EMAX/0.D0/,TSC1/0.D0/
	DATA PTIME/igen*0.D0/
	DATA ET/igen*0.D0/,R1T/igen*0.D0/
	DATA PWC/igen*0.D0/,PTEMP/igen*0.D0/
	DATA PFRACV/igen*0.D0/,PENTH/igen*0.D0/
	DATA PRHO/igen*0.D0/
	data DEN/igen*0.D0,igen*0.D0,igen*0.D0,igen*0.D0,igen*0.D0/
c
	data reflag/.true./
C
	DATA INP/'.inp'/,ER1/'.er1'/
	DATA TR2/'.tr2'/
	data scl/'.scl'/
C
C.........................................................
C
C... GET THE EXECUTION TIME
C
	t1 = secnds(0.)
	call dattim(tsrc)
C
C... and THE FILE NAME
C
	call runname(opnf1)
	nchar = index(opnf1,' ') - 1
c
	opnf1 = opnf1(1:nchar) // er1(1:4)
c
	CALL ESTRT1(OPNf1)
C
	opnf1 = opnf1(1:nchar) // inp(1:4)
	CALL IO(gmass,OPNf1)
	CALL ALPH
C
	alpha1 = alpha+1.D0
	WRITE(lunlog,1105) ALPHA
 1105	FORMAT(5X,'THE VALUE OF ALPHA IS ',F6.4)
C
	GAMMAF = GAMMA(1.D0/ALPHA1)
	TSC1 = TEND
C
c... set the density and enthalpy functions in TPROP
c
	call setent(hmrte,harte,hwrte)
	call setden(one,zero,hmrte)
C
C... SOURCE INTEGRATION (CA = RHOE).......................................
C
	OPEN(UNIT=9, recl=202, status='SCRATCH', form='FORMATTED')
C
C... START THE GAS BLANKET?
C
	rL    = 2.D0*AFGEN2(PTIME,R1T,TIME0,'R1T-MN')
	QSTRE = AFGEN2(PTIME,ET,TIME0,'ET-MN')/(pi*rL**2/4.D0)
	PWCP  = AFGEN2(PTIME,PWC,TIME0,'PWC')
	HPRIM = AFGEN2(PTIME,PENTH,TIME0,'PENTH')
	RHOP  = AFGEN2(PTIME,PRHO,TIME0,'PRHO')
	CCP   = PWCP*RHOP
c
	qstar = 0.D0
	if(u0 .ne. 0.D0) qstar = CCP*vkc*ustar*alpha1
     .			*dellay/(dellay-1.D0)/phihat(RHOP,rL)
c
	write(lunlog,3010) time0,rl,qstar,qstre
	IF(QSTRE.lt.QSTaR .and. gmass.eq.0.D0) then
		tsc1 = 0.D0
		GOTO 400
		endif
C
  100	CONTINUE
	check3 = .false.
C
c////////////////////////////////////////////////////////////////////////////
C... INITIAL CONDITIONS for gas blanket
c\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
C
	if(time0.ne. 0.D0) then
		rL0 = 2.D0*AFGEN2(PTIME,R1T,TIME0,'R1T-MN')
		QSTRE = AFGEN2(PTIME,ET,TIME0,'ET-MN')/(pi*rL0**2/4.D0)
	PWCP = AFGEN2(PTIME,PWC,TIME0,'PWC')
	HPRIM = AFGEN2(PTIME,PENTH,TIME0,'PENTH')
	RHOP = AFGEN2(PTIME,PRHO,TIME0,'PRHO')
	CCP = PWCP*RHOP
c
	qstar =0.D0
	if(u0.ne. 0.D0) qstar = CCP*vkc*ustar*alpha1*
     .			dellay/(dellay-1.D0)/phihat(RHOP,rL0)
c
		endif
C
C... SET UP INTEGRATION PARAMETERS.......
C
C    VARIABLE	     SUBSCRIPT
C    --------	     ---------
C	RG		Y(1)
C	mass		Y(2)
C	massc		Y(3)
C	massa		Y(4)
C	Enthalpy	Y(5)
c	moe		y(6)
C	TIME		X
c
c.........................................
C
	PRMT(1) = TIME0
	PRMT(2) = 6.023D23
	PRMT(3) = STPIN
	PRMT(4) = ERBND
	PRMT(5) = 6.0D3
C	PRMT(6) = EMAX -- OUTPUT
c
	do 103 iii = 6,25
	prmt(iii) = 0.D0
 103	continue
C
	Y(1) = AFGEN2(PTIME,R1T,TIME0,'R1T-MN')
	rmax = y(1)
	Y(2) = dmax1(gmass/pwcp, (pi*Y(1)**2*1.1D0*srccut*RHOP)) ! fix: 21May92
	vuflag = .false.
	htod = (gmass/pwcp)/RHOP/2.D0/pi/Y(1)**3		! fix: 21May92
	if(gmass .ne. 0.D0) write(lunlog,*) 'height-to-diameter: ', htod
	prmt(22) = htod * 2.D0*Y(1)	! initial height of the tail
	prmt(24) = prmt(22)
	prmt(23) = 0.D0			! initial height of the head
	prmt(25) = prmt(23)
	if(htod .gt. 0.1D0) vuflag = .true.
	Y(3) = Y(2) * PWCP
	PWAP = (1.D0 - PWCP)/(1.D0 + HUMID)	! water from humidity only
	Y(4) = Y(2) * PWAP
	Y(5) = Y(2) * HPRIM
	y(6) = 0.0D0
C
	DERY(1) = WTRG
	DERY(2) = WTtm
	DERY(3) = WTyc
	DERY(4) = WTya
	DERY(5) = WTeb
	dery(6) = wtmb
C
	NDIM = 6
C
C... PERFORM INTEGRATION
C
	WRITE(lunlog,1145)
 1145	FORMAT(5X,'Beginning Integration Step - Gas blanket')
C
	CALL RKGST(PRMT,Y,DERY,NDIM,IHLF,SRC1,SRC1O,AUX)
C
	IF(IHLF .GT. 10) CALL trap(1,IHLF)
	IF(CHECK3) then
		TEND = TSC1
		GO TO 500
	endif
C
C... RESTART THE GAS BLANKET?...........................................
C
	TIME0 = TSC1
	rL = 2.D0*AFGEN2(PTIME,R1T,TIME0,'R1T-MN')
	QSTRE = AFGEN2(PTIME,ET,TIME0,'ET-MN')/(pi*rL**2/4.D0)
	PWCP = AFGEN2(PTIME,PWC,TIME0,'PWC')
	RHOP = AFGEN2(PTIME,PRHO,TIME0,'PRHO')
	CCP = PWCP*RHOP
c
	qstar = 0.D0
	if(u0.ne. 0.D0) qstar = CCP*vkc*ustar*alpha1*
     .			dellay/(dellay-1.D0)/phihat(RHOP,rL)
c
	write(lunlog,3010) time0,rl,qstar,qstre
 3010	format(//,' time0: ',1pg13.5,t40,'l: ',1pg13.5,/,
     .		' qstar: ',1pg13.5,t40,'qstre: ',1pg13.5)

c
c... before restarting, check to see if this may be a problem child........
c	If the current time is less than SRCSS, this may be a problem
c	simulation.  The scheme is to scan the primary source flux for the
c	rest of time.  If the flux is never higher, disable the blanket
c	restart feature in NOBL with the logical flag REFLAG, and
c	force the source to use NOBL.
c
c..........................................................................
c
	IF(QSTRE .Gt. QSTAR .and. time0.ge.srcss) then
	    goto 100

	else if(QSTRE .GT. QSTAR) then

	    do 190 iii=1,igen
	    if(ptime(iii) .eq. pound) goto 100
	    if(time0 .lt. ptime(iii)) goto 200
  190	    continue
	    goto 100

  200	    do 210 ii=iii,igen
	    if(et(ii) .eq. 0.D0) goto 300
	    flux = et(ii)/(pi*r1t(ii)**2)
	    if(flux .gt. qstre) goto 100
  210	    continue

  300	    reflag = .false.
	    write(lunlog,1146)
 1146	    FORMAT(' Forcing NOBL...')
	endif
C
c////////////////////////////////////////////////////////////////////////////
C... SOURCE INTEGRATION -- NO GAS BLANKET
c\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
C
  400	continue
	WRITE(lunlog,1147)
 1147	FORMAT(5X,'Source calculation - No Gas blanket')
C
	CALL NOBL(timeout, reflag)
c
	if(check3) then		! restart blanket calculation
		time0 = timeout
		goto 100
	endif
C
c... Calculate windspeed parameters
c
  500	RMAX = 1.01D0*RMAX	! GUARANTEE A GOOD VALUE
	aleph = 0.D0
	if(u0 .ne. 0.D0) ALEPH = U0/GAMMAF*(SZM/Z0)**ALPHA
     .		/(SQRTPI/2.D0*RM +RMAX)**(ALPHA/ALPHA1)/alpha1
C
c... Prepare the output files
c
	rewind (unit=9)
	opnf1 = opnf1(1:nchar) // scl(1:4)
	open(unit=8, file=opnf1, status='UNKNOWN', form='FORMATTED')
c
	call head(gmass)
	call crfg(table,ntab,crfger)
	call head(gmass)
c
	CLOSE(UNIT=9)
	close(unit=8)
C
	opnf1 = opnf1(1:nchar) // tr2(1:4)
	CALL TRANS(OPNf1)
C
C... CALCULATE EXECUTION TIME
C
	tt1 = t1
	t1 = Secnds(tT1)/60.
	if(t1 .le. 0.) t1 = t1 + 60*24
	WRITE(lunlog,2000) TSRC
	WRITE(lunlog,2010) T1
 2000	FORMAT(1X,'BEGAN AT ',A24)
 2010	FORMAT(5X,' ***** ELAPSED TIME ***** ',1pg13.5,' min ')
C
	STOP
	END
c
C.............................................................
C
	SUBROUTINE SRC1(time,Y,D,PRMT)
c
c................................................................
c
C	SOURCE EQUATIONS -- Gas Blanket present
c
c.................................................................
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	include 'DEG1.prm'

	parameter(	delt= 0.1D0,
     .			delto2= delt/2.D0,
     .			zero= 1.D-10,
     .			rcrit= 0.002D0)
C
	common
     ./ALP/	ALPHA,alpha1
     ./comatm/	istab,tamb,pamb,humid,isofl,tsurf,ihtfl,htco,iwtfl,wtco,
     .		humsrc
     ./comH/	Hmrte,Harte,Hwrte
     ./csprd/	ce, delrmn
     ./ERROR/	STPIN,ERBND,WTRG,WTtm,WTya,wtyc,wteb,wtmb,XLI,
     .		XRI,EPS,ZLOW,STPINZ,ERBNDZ,SRCOER,srcss,srccut,
     .		ERNOBL,NOBLpt,crfger,epsilon
     ./GEN1/	PTIME(igen), ET(igen), R1T(igen), PWC(igen), PTEMP(igen),
     .		PFRACV(igen), PENTH(igen), PRHO(igen)
     ./parm/	u0,z0,zr,rml,ustar,vkc,gg,rhoe,rhoa,deltay,betay,gammaf,
     .		yclow
     ./PARMSC/	RM,SZM,EMAX,RMAX,TSC1,ALEPH,TEND
     ./PHLAG/	CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
     ./phicom/	iphifl,dellay
     ./vucom/	vua,vub,vue,vud,vuflag
C
	LOGICAL CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
	logical vuflag
C
c
	DIMENSION Y(7),D(7),PRMT(25)

	DATA iR/1/,mass/2/,massc/3/,massa/4/,iebal/5/,mbal/6/
c
c... check to see if the momentum balance is to be used
c
	if( prmt(20).lt. 0.D0) vuflag = .false.
c
c... calculate the mass fractions and thermodynamic properties
c
	if(Y(mass) .le. 0.D0) then
	    wc = dmax1(prmt(15),1.d-10)
	    if(wc.gt. 1.) wc=1.d-10
	    wa = 1.D0 - wc
	    enth = wc*hmrte		! air contributes nothing
	else
	    wc = Y(massc)/Y(mass)
	    wa = Y(massa)/Y(mass)
	    enth = Y(iebal)/Y(mass)
	endif

	humsrc = (1.D0 - wc - wa*(1.D0+humid))/wc
	call tprop(1,wc,wa,enth,yc,ya,rmole,temp,rho,cp)

c
c... get ready to estimate the blanket spreading velocity
c
	RADP = AFGEN2(PTIME,R1T,TIME,'R1TSRC')
	hei   = dmax1( Y(mass)/pi/Y(ir)/Y(ir)/rho , 0.0D0 )
	delrho = rho-rhoa
	if(delrho .lt. 0.D0) delrho = 0.D0
	gprime = gg*delrho/rhoa *hei
c
C... CALCULATE D(iR),airrte,vel
c

	D(iR) = 0.D0
	vel = 0.D0
	airrte = 0.D0
	Ri = 0.D0
	D(mbal)  = 0.D0

	IF(Gprime.le. 0.D0) goto 200

	slump = Ce*sqrt(Gprime)
c
c... As necessary, use the momentum balance to get the blanket spreading
c	rate
c
	if(vuflag) then		! momentum balance
	    iii = 0		! initialize loop counter
	    vel = prmt(14)		! old velocity value
	    velmin = 0.D0
	    velmax= dmax1( slump, 0.1D0, vel)

 100	    hh = vel*vel/Ce/Ce/gg/ (delrho/rhoa)
	    rh = Y(ir)-vua*vub*hh
	    value = Y(ir)**2/rh**2

	    if(prmt(25).ge. prmt(24)) then	! hh .ge. ht

		ht = 2.D0*(value*hei - vua*hh*(value-1.D0)) - hh
		velc = Y(mbal)/(0.4D0*pi*rho*(2.D0/3.D0*ht + hh)*rh**3/Y(ir)
     .		   + 2.D0/3.D0*pi*vua*rho*hh* (Y(ir)**2 - rh*rh*rh/Y(ir))
     .		   + vue*pi*Y(ir)*hei**2*rhoa)
		D(mbal) = pi*gg*delrho*Y(ir)*ht**2
     .		   - vua*vud*pi*rhoa*Y(ir)*hh*vel**2

	    else

		ht = value*hei - vua*hh*(value-1.D0)
		velc = Y(mbal)/(2.D0/3.D0*pi*rho*ht*rh**3/Y(ir)
     .		   + 2.D0/3.D0*pi*vua*rho*hh* (Y(ir)**2 - rh*rh*rh/Y(ir))
     .		   + vue*pi*Y(ir)*hei**2*rhoa)
		D(mbal) = pi*gg*delrho*(rh*ht**2 + vua*vub*hh*hh*hh)
     .		   - vua*vud*pi*rhoa*Y(ir)*hh*vel**2

	    endif

	    dif = abs(vel-velc)		! convergence check
	    sum = abs(vel) + abs(velc) + zero

	    if(dif/sum .le. rcrit) then
		vel = (vel+velc)/2.D0
		prmt(13) = vel

		    if(vel .gt. 0.D0) then
			Ri = gprime / vel**2
			airrte= 2.D0*pi* epsilon/Ri *rhoa*Y(ir)*hei* vel
			D(ir) = vel
			prmt(20) = slump
		    endif
	     else

		dif = vel-velc

		if(velc.lt.velmin) velmin= dmax1(velc, 0.D0)
		if(velc.gt.velmax) velmax=velc

		if(dif .gt. 0.D0) then
		    velmax = vel
c		    vel = 0.5D0*(velmax-velmin) + velmin
		    vel = 0.382D0*(velmax-velmin) + velmin
		else
		    velmin = velc
c		    vel = (1.D0-0.5D0)*(velmax-velmin) + velmin
		    vel = (1.D0-0.382D0)*(velmax-velmin) + velmin
		endif

		iii = iii+1
		if(iii .gt. 40) then
		    vel = min(velmin, velmax)
c
c...			see if velocity has accelerated past SLUMP; this is
c			caused by the integration scheme stepping past the
c			cross over point.  If so, set VEL to SLUMP and continue
c
		    if(vel .gt. slump) then
			vel = slump
			prmt(13) = vel
			if(vel .gt. 0.D0) then
			    Ri = gprime/vel**2
			    airrte= 2.D0*pi* epsilon/Ri*
     .					rhoa*Y(ir)*hei* vel
			    D(ir) = vel
			    prmt(20) = slump
			endif
			goto 200
		    endif
		    stop'SRC1 velocity loop'
		endif
		goto 100
	    endif

	else

	    vel= slump	! gravity slumping
	    hh = hei
	    ht = hei
	    Ri = gprime / vel**2
	    airrte= 2.D0*pi* epsilon/Ri *rhoa*Y(ir)*hei* vel
	    D(ir) = vel
	endif

c
c... stop spreading for some cases
c
  200	continue
	IF(delrho.Lt.delrmn .and. u0.ne.0.D0) then
	    D(iR) = 0.D0 ! not for windless cases
	    airrte = 0.D0
	endif
c
c... ensure the blanket is larger than any primary source.  Calculate the
c	spreading rate to be based on the liquid pool rate.  DELTO2 avoids
c	some numerical troubles.
c
	area = pi * (Y(ir)**2 - radp**2)
	IF(Y(iR).Le. RADP) THEN
	    AREA = 0.D0
	    Y(iR) = RADP + zero
	    IF(time.gt. delto2) then ! delt; num prob
		D(iR) = dmax1(0.D0,
     .			((AFGEN2(PTIME,R1T,TIME+delto2,'R1TSRD')- 
     .			AFGEN2(PTIME,R1T, (TIME-delto2),'R1TSRe'))/ delt))
	    else
		D(iR) = dmax1(0.D0,
     .			((AFGEN2(PTIME,R1T,delto2,'R1TSRD')- 
     .			AFGEN2(PTIME,R1T, 0.D0,'R1TSRe'))/ delto2))
	    endif
	ENDIF
c
c... calculate totrteout
c
	erte = AFGEN2(PTIME,ET,TIME,'src1')
	PWCP = AFGEN2(PTIME,PWC,TIME,'src1')
	TOTPRT = eRTE/PWCP
	HPRIM = AFGEN2(PTIME,PENTH,TIME,'src1')
	rL = 2.0D0 * Y(iR)
c
	cc = wc*rho
c
c
	qstrmx = 0.D0
	if(u0 .ne. 0.D0) qstrmx = cc*vkc*USTAR*ALPHA1*
     .			dellay/(dellay-1.D0)/phihat(rho,rL)
c
c
	qstrll = qstrmx * pi*rL*rL/4.D0
	totrteout = qstrll/wc
c
c... surface effects
c
	watrte = 0.D0
	qrte = 0.D0
	yw = 1.D0-ya-yc
	yw = min( max(yw, 0.0D0), 1.0D0)
	call surfac(temp,hei,rho,rmole,cp,yw,watrte,qrte)
	qrte = area * qrte
	if(qrte.lt. 0.D0) qrte = 0.D0 ! don't let the cloud cool
	watrte = area * watrte
c
	totrtein  = airrte + TOTPRT + watrte
c
c... If the blanket is shrinking, allow the radius to decrease.
c... check2=True for HSE type spills; don't allow these to shrink
c
	IF(totrtein.lt.totrteout .and. .not.check2) then
	    D(iR) = 0.
	    if(hei.gt.srccut .and. Y(ir).gt.srccut) then
		dHdt = (totrtein - totrteout)/3.D0/pi/ Y(ir)/Y(ir)/rho
		D(iR) = Y(iR)/Hei * dHdt
c				! Let cloud radius shrink when...
	    endif		! cloud height is decreasing unless...
	    if( Y(iR).le.0.01D0*rmax .and. erte.eq.0.D0)
     .		D(iR) = 0. ! the primary source has stopped. tos;6mar86
	    if( Y(iR).le.hei .and. erte.eq.0.D0)
     .		D(iR) = 0. ! the cloud height > the cloud radius. tos;16nov87
	endif
c
c... CALCULATE D(mass),D(massc),D(massa),D(anything left)
c
	D(mass)  = totrtein - totrteout
	D(massc) = erte - qstrll
	D(massa) = (airrte + erte*(1.D0/PWCP - 1.D0))/(1.D0+humid)
     .					- wa/wc*qstrll
	D(iebal)  = 0.D0

	if(ihtfl.ne. 0)	! equivalent to adiabatic mixing from TPROP for ihtfl=0
     .	    D(iebal)  = HPRIM*TOTPRT + harte*airrte
     .			+ hwrte*watrte - enth*totrteout + qrte
c
	uheff = qstrmx*rL/cc
	sz = 0.D0
	if(u0 .ne. 0.D0) sz = ( uheff*alpha1/u0/z0 )**(1.D0/alpha1) * z0
C
	PRMT(6) = QSTRMX
	prmt(7) = sz
	prmt(8) = hei
	prmt(9) = rho
	prmt(10)= Ri
	prmt(11)= yc
	prmt(12)= ya
	prmt(13)= D(ir)
	prmt(16) = wc
	prmt(17) = wa
	prmt(18) = enth
	prmt(19) = temp
	prmt(21) = erte
	prmt(22) = ht
	prmt(23) = hh

	RETURN
	END
c
C.............................................................
C
	SUBROUTINE SRC1O(TIME,Y,DERY,IHLF,NDIM,PRMT)
c
c...................................................................
c
C	SUBROUTINE FOR OUTPUT FROM SOURCE in the presence of a Blanket
C
c...................................................................
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

C
	include 'DEG1.prm'
C
	COMMON
     ./ALP/	ALPHA,alpha1
     ./comss/	ess,slen,swid,outcc,outsz,outb,outl,swcl,swal,senl,srhl
     ./ERROR/	STPIN,ERBND,WTRG,WTtm,WTya,wtyc,wteb,wtmb,XLI,
     .		XRI,EPS,ZLOW,STPINZ,ERBNDZ,SRCOER,srcss,srccut,
     .		ERNOBL,NOBLpt,crfger,epsilon
     ./parm/	u0,z0,zr,rml,ustar,vkc,gg,rhoe,rhoa,deltay,betay,gammaf,
     .		yclow
     ./PARMSC/	RM,SZM,EMAX,RMAX,TSC1,ALEPH,TEND
     ./PHLAG/	CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
C
	LOGICAL CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
C
C
	DIMENSION Y(6),DERY(6),PRMT(25)
	DIMENSION CURNT(iousrc),BKSP(iousrc),OUTP(iousrc)
C
	DATA I/0/,III/0/
	DATA EMAX/0.D0/,tlast/0.D0/
c
	DATA iR/1/,mass/2/,massc/3/,massa/4/,iebal/5/
c
	data nrec1/0/
c
C
	I = I + 1
	III = III + 1
C
	qstar = prmt(6)
	sz = prmt(7)
	hei = prmt(8)
	rho = prmt(9)
	Ri  = prmt(10)
	yc = prmt(11)
	ya = prmt(12)
	vel = prmt(13)
	prmt(14) = vel
	if(vel .gt. prmt(20)) prmt(20) = -prmt(20)
	prmt(15) = prmt(16)	! wc
	wc = prmt(16)
	cc = wc * rho
	wa = prmt(17)
	enth = prmt(18)
	temp = prmt(19)
	prmt(24) = prmt(22)	! ht
	prmt(25) = prmt(23)	! hh
c
c... stop the integration if the height<0
c
	IF(hei .Le. 0.0D0) GO TO 1000
C
c... check for RM et al.
c
	QSAV = PI*Y(iR)*Y(iR)*qstar
	IF(QSAV .LT. EMAX) GO TO 110
	EMAX = QSAV
	RM = Y(iR)
	SZM = SZ
  110   CONTINUE
	RMAX = dMAX1(RMAX,Y(iR))
C
c... stop the integration if the blanket is tiny, or the concentration is low
c	for no wind cases.
c
	IF(hei .Le. srccut) GO TO 1000
	if(yc .le. yclow .and. u0 .eq. 0.D0) goto 1000	! no wind
	if(time.gt.tend+1. .and. u0.eq.0.D0 .and. vel.eq.0.D0)
     .				goto 1000		! no wind LNG
C
c... load the CURNT responses at the start
c
	IF(I .NE. 1) GO TO 115
	CURNT(1) = TIME
	CURNT(2) = Y(iR)
	CURNT(3) = hei
	CURNT(4) = qstar
	CURNT(5) = sz
	CURNT(6) = yc
	CURNT(7) = ya
	CURNT(8) = rho
	CURNT(9) = ri
	CURNT(10)= wc
	CURNT(11)= wa
	CURNT(12)= enth
	CURNT(13)= temp
	III = 1
	GO TO 125
  115	IF(I .EQ. 0) RETURN
C
	DO 116 II=1,iousrc
  116	BKSP(II) = CURNT(II)
C
	CURNT(1) = TIME
	CURNT(2) = Y(iR)
	CURNT(3) = hei
	CURNT(4) = qstar
	CURNT(5) = sz
	CURNT(6) = yc
	CURNT(7) = ya
	CURNT(8) = rho
	CURNT(9) = ri
	CURNT(10)= wc
	CURNT(11)= wa
	CURNT(12)= enth
	CURNT(13)= temp
C
c... check for an output point
c
	ERM = 0.D0
	ermss = 0.D0
	DO 120 II=2,iousrc
	div = curnt(ii)
	if(div .eq. 0.D0) div = srcoer
	ER1 = ABS( (CURNT(II)-BKSP(II))/div )
	ER2 = ABS( (CURNT(II)-OUTP(II))/div )
	if(II.ne.3 .and. ii.ne.9 .and. ii.ne.12 .and. ii.ne.7
     .		.and. ii.ne.11) ermss = dMAX1(ER1,ER2,ERMss)
c				! ex hei,Ri,enth,wa,ya for SS
  120	ERM = dMAX1(ER1,ER2,ERM)
C
c... see if steady state conditions are met
c
	if(check4) then			! steady state
	    if( .not. (vel.eq. 0.D0.and.time.gt.srcss)) goto 124
  122	    check3 = .true.
	    outcc = wc * rho
	    swcl = wc
	    swal = wa
	    srhl = rho
	    senl = enth
	    outl  = 2.0D0 * Y(ir)
	    Qstar = prmt(21)/pi/Y(ir)**2
	    if(u0.ne. 0.D0)
     .		sz= (alpha1/u0/z0*Qstar*outl/outcc)**(1.D0/alpha1)* z0
	    outsz = sz
	    outb  = pi*Y(ir)**2 /outl/2.D0
	    goto 1000

  124	    if(ermss .gt. srcoer) goto 125

	    if( time-tlast .gt. srcss) goto 122
	    return
	endif
c
	IF(ERM .LT. SRCOER) RETURN
C
c... write an output point as necessary
c
  125	CONTINUE
	tlast = time
	DO 130 II=1,iousrc
	IF(III.EQ.1) BKSP(II) = CURNT(II)
  130	OUTP(II) = BKSP(II)
C
	III = 0
	NREC1 = NREC1 + 1
	WRITE(9,2000) (OUTP(II),II=1,iousrc)
	RETURN
C
c... stop the integration
c
 1000   CONTINUE
	I = -1
	IF(TIME .GE. TEND) CHECK3 = .TRUE.
	NREC1 = NREC1 + 1
	WRITE(lunlog,1100)
	WRITE(lunlog,*) Hei,TIME
	TSC1 = TIME
	if(hei .le. 0.D0) then
	    hei = 0.D0
	    y(ir) = dmin1(rmax,y(ir))
	endif
	WRITE(9,2000) TIME,Y(iR),hei,qstar,sz,yc,ya,rho,ri,wc,wa,enth,temp
	WRITE(lunlog,1110) NREC1
C
	PRMT(5) = 1.D0
C
	RETURN
 1100   FORMAT(5X,'VALUE OF Hei AT SOURCE TERMINATION -- @ TIME')
 1110   FORMAT(5X,'NUMBER OF LINES -->  ',I8)
 2000	format(1pg16.9,1x,1pg16.9,11(1x,1pg13.6))
	END
