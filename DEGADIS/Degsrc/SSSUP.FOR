	SUBROUTINE SSSUP(Hmrte)
c
C..............................................................
C
C	Pseudo-Steady State SUPervisor controls the observer integrations.
C
C..............................................................
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	include 'DEG2.prm'
c
	COMMON
     ./ALP/	ALPHA,alpha1
     ./cedge/	T0L, direc
     ./cgprop/	gasmw,gastem,gasrho,gascpk,gascpp,gasulc,gasllc,gaszzc,
     .		gasnam
     ./CNOBS/	NOBS
     ./comatm/	istab,tamb,pamb,humid,isofl,tsurf,ihtfl,htco,iwtfl,wtco,
     .		humsrc
     ./csprd/	ce, delrmn
     ./ERROR/	SY0ER,ERRO,SZ0ER,WTAIO,WTQOO,WTSZO,ERRP,SMXP,WTSZP,
     .		WTSYP,WTBEP,WTDH,ERRG,SMXG,ERTDNF,ERTUPF,WTRUH,WTDHG
     ./GEN1/	PTIME(igen), ET(igen), R1T(igen), PWC(igen), PTEMP(igen),
     .		PFRACV(igen), PENTH(igen), PRHO(igen)
     ./gen2/	den(5,igen)
     ./GEN3/	radg(2,maxl),qstr(2,maxl),srcden(2,maxl),srcwc(2,maxl),
     .		srcwa(2,maxl),srcen(2,maxl)
     ./nend/	pound
     ./PARM/	U0,Z0,ZR,rml,USTAR,vkc,gg,RHOE,RHOA,DELTA,BETA,GAMMAF,
     .		yclow
     ./PARMSC/	RM,SZM,EMAX,RMAX,TSC1,ALEPH,TEND
     ./phicom/	iphifl,dellay
     ./PHLAG/	CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
     ./SSCON/	NREC(maxnob,2),T0(maxnob),XV(maxnob)
     ./STOPIT/	TSTOP
     ./STP/	STPO,STPP,ODLP,ODLLP,STPG,ODLG,ODLLG
c
	LOGICAL CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
	logical pup,pdn
c
	character*3 gasnam
c
	EXTERNAL PSS,PSSOUT,SSG,SSGOUT,OB,OBOUT
C
	DIMENSION PRMT(22),Y(5),DERY(5),AUX(8,5)
C
	DATA RTOT/0./
C
c
c... Estimate the earliest and latest time an observer can be released
c	over the source.
c
	R = AFGEN(RADG, 0.0D0, 'RADG')
	T01 = T0OB(R, 0.0D0)
c
c... For low wind speed cases which form a blanket, earlier times
c	than T01 may be possible.  Check each of the points in RADG.
c
	do 10 i=2,maxl
	if(radg(1,i).eq.pound .and. radg(2,i).eq.pound) goto 20
	T0F = T0ob( radg(2,i), radg(1,i) )
	if(T0F .lt. T01) T01 = T0F
   10	continue
c
c... Now, calculate the last possible time an observer can be released.
c
 20	continue
	XEND = AFGEN(RADG,TEND,'RADG')
	T0F = T0OB(-XEND,TEND)
C
c... Divide the total possible time among the observers...
c
	DTOB = (T0F-T01)/FLOAT(NOBS)
	T01 = T01 + DTOB/2.D0
C
C... and perform the calculation for each observer.
c
	write(12,1162)
c
	DO 120 I = 1,NOBS
C
C... RESET AGAIN
C
	AGAIN = .FALSE.
C
	T0(I) = DTOB*dble(I-1) + T01
	pup = .true.
	pdn = .true.
C
C... IF(XEND .GT. XIT(TEND,T0(I))) -- IS
C	TRUE WHEN THE SOURCE WILL TERMINATE BEFORE THE OBSERVER
C	CAN REACH THE DOWNWIND EDGE.
C
	IF(XEND .GT. XIT(TEND,T0(I))) then
	    pdn = .false.
	    TDOWN = TEND
	endif
c
C... IF(XIT(0.0D0,T0(I)) .gt. -R) -- IS
C	TRUE WHEN THE SOURCE WILL begin after THE OBSERVER
C	has passed THE DOWNWIND EDGE @ t=0.0
C
	R = AFGEN(RADG,0.0D0,'RADG')
	IF(t0(i).le.0. .and. XIT(0.0D0,T0(I)).gt.-R) then
	    pup = .false.
	    TUP = 0.0D0
	endif
c
	T0L = T0(i)
	if(pup) TUP = TUPF()
	if(pdn) TDOWN = TDNF()
C
	XDOWN = XIT(TDOWN,T0(I))
	XUP = XIT(TUP,T0(I))
	WRITE(lunlog,1160) TUP,XUP,TDOWN,XDOWN
C
C... SET UP INTEGRATION PARAMETERS FOR EACH OBSERVER.
C
	do 200 ijk=1,22
	prmt(ijk) =0.D0
  200	continue

	do 220 ijk=1,5
	y(ijk)   = 0.D0
	dery(ijk)= 0.D0
	do 220 ijkl=1,8
	aux(ijkl,ijk) = 0.D0
  220	continue
c
	PRMT(1) = TUP
	PRMT(2) = TDOWN
	PRMT(3) = STPO
	PRMT(4) = ERRO
	PRMT(5) = dMAX1(1.D0,(TDOWN-TUP)/50.D0)
	PRMT(6) = T0(I)
	prmt(7) = xup
	PRMT(13)= XDOWN - XUP
C
	rrr = prmt(13)/2.D0
	Y(1) = sz0er * rrr			! Hwidth
	Crate= Y(1)  * rrr * AFGEN(qstr,tup,'IC')
	Y(2) = Crate / AFGEN(srcwc,tup,'IC')	! Mrate
	y(3) = Crate			 	! Crate
	y(4) = Y(2)  * AFGEN(srcwa,tup,'IC') 	! BDArate
	y(5) = Y(2)  * AFGEN(srcen,tup,'IC')	! Hrate
C
	DERY(1) = WTAIO
	DERY(2) = WTQOO
	DERY(3) = WTSZO
	DERY(4) = 1.D0
	DERY(5) = 1.D0
C
	NDIM = 4
	if(isofl.eq. 0 .and. ihtfl.ne. 0) ndim=5
C
C... PERFORM OB INTEGRATION.
C
	WRITE(lunlog,1120) I
 1120	FORMAT(/,'  Entering Observer Integration Step for Observer # ',
     .		I3)
C
	CALL RKGST(PRMT,Y,DERY,NDIM,IHLF,OB,OBOUT,AUX)
C
	IF(IHLF .GE. 10) CALL trap(8,IHLF)
c
	write(lunlog,1125)
 1125	format(' ',10x,'Observer Integration complete...')
c
c... Establish initial conditions
c
	cclay  = prmt(14)
	cc     = cclay*dellay
	wclay  = prmt(15)
	walay  = prmt(16)
	enthlay=prmt(17)
	rholay = prmt(18)
C
	RL = XDOWN - XUP
	B = Y(1)
	AREA = B*RL
	QSTR0 = Y(3)/area
	Erate = 2.D0*qstr0*RL*b
	cc = min(cc, rhoe)
	WCP  = AFGEN2(PTIME,PWC,TDOWN,'SSS-WC')
	RHOP = AFGEN2(PTIME,PRHO,TDOWN,'SSS-RH')
	CCP  = WCP*RHOP
	if(cc .gt. ccp) cc = ccp

	sz0 = (qstr0*RL/cc * alpha1/u0/z0)**(1.D0/alpha1) * z0
C
	sy0er = 0.D0
	ratio1= u0*z0/ALPHA1/ Z0**ALPHA1 *Cc /B/qstr0/RL
	ratio = ratio1* sz0**alpha1 * (B + sqrtpi/2.D0*sy0er)
	if(ratio.lt. 1.D0) then
	    sy0er = (1.D0/(RATIO1*sz0**alpha1) - b)*2.D0/sqrtpi
	else
	    sz0 = (1.D0/((B+ sqrtpi/2.D0*sy0er)*ratio1))**(1.D0/alpha1)
	endif

c
c... Establish the thermodynamic properties of mixtures of air and
c	the gas mixture (WCLAY,WALAY,ENTHLAY) assuming adiabatic
c	mixing.  This is accomplished with the call to SETDEN.
c	Then, extrapolate the properties to the centerline,
c	ground level concentration.
c
	humsrc = (1.D0-wclay-walay*(1.D0+humid))/wclay
	call setden(wclay,walay,enthlay)
	if(isofl.eq. 1) then
	    call adiabat(0,wc,wa,yc,ya,cc,rho,wm,enth,temp)
	    temlay = temp
	    goto 400
	endif
c
c... Scan through the array DEN for the last value, and establish a new
c	final value based on the centerline, ground level concentration.
c
	do 300 iii= 1,igen
	if(den(1,iii) .gt. 1.D0) then
	    ii = iii+1
	    rholay = den(3,iii-1)
	    temlay = den(5,iii-1)
	    if(ii .gt. igen .or. iii.le.3) call trap(2,0)
	    rho = cc*(rholay-rhoa)/cclay + rhoa	! assumes gamma=con
	    wc = cc/rho

	    if(wc.gt.1.D0) then
		if(isofl.eq.1) then
		    wc   = 1.D0
		    rho  = cc
		    temp = tamb
		    den(1,iii) = 1.D0
		    den(2,iii) = cc
		    den(3,iii) = rho
		    den(4,iii) = den(4,iii-1)
		    den(5,iii) = den(5,iii-1)
		else if(ihtfl.eq.0) then
		    wc   = 1.D0
		    rho  = cc
		    temp = gastem
		    den(1,iii) = 1.D0
		    den(2,iii) = cc
		    den(3,iii) = rho
		    den(4,iii) = hmrte
		    den(5,iii) = temp
		else
		    wc   = 1.D0
		    wa   = 0.D0
		    rho  = cc
		    hhh  = enthlay/wclay
		    if(abs(hhh).gt.abs(hmrte)) hhh = hmrte
		    temp = min(gastem*gasrho/cc, tamb)
		    den(1,iii) = 1.D0
		    den(2,iii) = cc
		    den(3,iii) = rho
		    den(4,iii) = hhh
		    den(5,iii) = temp
		endif
		den(2,ii) = 2.D0
		goto 400
	    endif
	    w2 = den(2,iii-2)/den(3,iii-2)
	    w1 = den(2,iii-1)/den(3,iii-1)
	    wa = (1.D0-(1.D0+humsrc)*wc)/(1.D0+humid)
	    wm = 1.D0/(wc/gasmw + wa/wma + (1.D0-wa-wc)/wmw)
	    yc = wm/gasmw * wc
	    Yc= dmax1(0.0D0, dmin1(1.0D0, yc) )
	    den(1,iii) = Yc
	    den(2,iii) = cc
	    den(3,iii) = rho
c
c... now, determine the enthalpy and temperature of such a mixture.
c	Base both enthalpy and temperature on the fact that
c	(yc/rho) is proportional to temperature (and therefore enthalpy).
c
	    denom = den(1,iii-2)/den(3,iii-2)-den(1,iii-1)/den(3,iii-1)
	    if(denom.ne. 0.D0) then
		slope = (yc/rho-den(1,iii-1)/den(3,iii-1))/denom
		den(4,iii) = slope*
     .				(den(4,iii-2)-den(4,iii-1))+den(4,iii-1)
		den(4,iii) = dmin1( dmax1(hmrte,den(4,iii)), 0.D0)
		den(5,iii) = slope*
     .				(den(5,iii-2)-den(5,iii-1))+den(5,iii-1)
		den(5,iii) = dmin1( dmax1(gastem,den(5,iii)), tamb)
	    else
		den(4,iii) = den(4,iii-1)
		den(5,iii) = den(5,iii-1)
	    endif
	    temp = den(5,iii)
	    den(1,ii) = 2.D0	! end-of-record
	    goto 400
	endif
  300	continue
C
 400	CONTINUE

	IF(Cc .GT. RHOE) then
	    WRITE(lunlog,1127) QSTR0,SZ0,Cc,rhoe
 1127	    format(/,' ',10('****  '),/,' qstr0: ',1pg13.5,
     .		' sz0: ',1pg13.5,/,
     .		/,' cc: ',1pg13.5,' is greater',
     .		 ' than rhoe: ',1pg13.5,//,' ',10('****  '),/)
	endif
C
C... SHOW THE user WHAT IS GOING ON
C
	WRITE(lunlog,1160) TUP,XUP,TDOWN,XDOWN
	WRITE(lunlog,1170) AREA,RL,B
	WRITE(lunlog,1180) QSTR0,SZ0,sy0er
	write(lunlog,1185) wclay,walay,rholay,cclay,temlay
	write(lunlog,1186) wc,rho,cc,temp

	write(12,1161) i,t0(i),tup,tdown,xdown,RL,B,Erate, wc,temp,Sz0

 1160	FORMAT(/,' TUP: ',1pG13.5,' XUP: ',1pG13.5,' TDOWN: ',
     .		1pG13.5,' XDOWN: ',1pG13.5)
 1161	format(' ',I3,1x,f8.1,1x,2(1x,f7.1,1x),4(1x,f6.1,1x),1x,
     .		f5.3,1x,1x,f5.1,1x,1x,f5.2)
 1162	format(/,' ','obs',4x,'T0',4x,3x,'Tup',3x,2x,'Tdown',2x,
     .	1x,'Xdown',2x,1x,'Length',1x,1x,'HWidth',1x,1x,'E rate',1x,
     .		'Mass fr',1x,'Temp',2x,2x,'Sz0',//)
 1170	FORMAT(' Half AREA: ',1pG13.5,' LENGTH: ',1pG13.5,' B: ',1pG13.5)
 1180	FORMAT(' TAKEUP FLUX: ',1pG12.5,' SZ0: ',1pG12.5,
     .		' sy0: ',1pg12.5)
 1185	format(' wclay: ',1pg12.5,' walay: ',1pg12.5,
     .		' rholay: ',1pg12.5,' Cclay: ',1pg12.5,/,
     .		' temlay: ',1pg13.5)
 1186	format(' wc: ',1pg12.5,
     .		' rho: ',1pg12.5,' Cc: ',1pg12.5,' temp: ',1pg12.5)
C
C... PREPARE FOR PSEUDO-STEADY STATE INTEGRATION.
C
	do 500 ijk=1,22
	prmt(ijk) =0.D0
  500	continue

	do 520 ijk=1,5
	y(ijk) = 0.D0
	dery(ijk)= 0.D0
	do 520 ijkl=1,8
	aux(ijkl,ijk) = 0.D0
  520	continue
c
	PRMT(1) = XDOWN
	PRMT(2) = 6.023D23
	PRMT(3) = STPP
	PRMT(4) = ERRP
	PRMT(5) = SMXP
	PRMT(6) = Erate
	PRMT(7) = Cc	! -- OUTPUT
	PRMT(8) = B	! -- OUTPUT
C
C... PRMT(9) & PRMT(10) ARE CONSTANTS FOR D(SY) & D(SZ)
C
	PRMT(9) = Ce*sqrt(gg*Z0/ALPHA1*GAMMAF)*GAMMAF/U0
	PRMT(10)= Z0**ALPHA*vkc*USTAR*ALPHA1*ALPHA1/U0
	PRMT(11)= NREC(I,1)
C	PRMT(12)= DIST AT COMPLETION -- OUTPUT
	PRMT(13)= T0(I)
c	prmt(14)= yc	! output
c	prmt(15)= rho	! output
c	prmt(16)= temp	! output; not recorded if isofl=1
c	prmt(17)= gamma	! output; not recorded if isofl=1 .or. ihtfl=0
	prmt(18)= u0*z0/alpha1
	prmt(19)= rhoa*vkc*ustar*alpha1
	prmt(20)= rholay
	prmt(21)= sz0
	prmt(22)= sz0
C
	Y(1) = rholay*prmt(18)*(SZ0/z0)**alpha1		! rho*ueff*heff
	Y(2) = SY0ER*SY0ER
	Y(3) = B + sqrtpi/2.D0*sy0er
	y(4) = 0.D0		! added heat
C
	DERY(1) = WTSZP
	DERY(2) = WTSYP
	DERY(3) = WTBEP
	dery(4) = WTDH
C
	NDIM = 4
C
	WRITE(lunlog,1130)
 1130	FORMAT('  Entering Integration Step -- B > 0. ')
C
C... PERFORM PSS INTEGRATION
C
	CALL RKGST(PRMT,Y,DERY,NDIM,IHLF,PSS,PSSOUT,AUX)
C
	IF(IHLF .GE. 10) CALL trap(9,IHLF)
C
	NREC(I,1) = INT(PRMT(11))
	WRITE(lunlog,1100) NREC(I,1),T0(I)
 1100	FORMAT(3X,'NUMBER OF RECORDS IN PSS = ',I10,' FOR T0=',
     .			1pg13.5)
C
	IF(AGAIN) GO TO 119
C
C... GAUSIAN stage
C
C... PSSOUT FORCES THE ABOVE INTEGRATION TO FINISH WHEN B<0 FOR THE
C	FIRST TIME. THE STEP BEFORE THIS OCCURS IS RECORDED ON UNIT 7.
C	Y currently contains interpolated values to b=0.
C
C... THE CALCULATION METHOD CHANGES THE CURRENT VALUE OF SY TO A VALUE
c	CALCULATED AS IF BEFF=sqrtpi*SY/2 RETAINING THE LAST VALUE OF Cc IN THE
C	MATERIAL BALANCE.
C
	heat = y(4)
	rholay = prmt(20)
	Cc = PRMT(7)
	rhouh = Y(1)
	sz = ( rhouh/rholay/prmt(18) )**(1.D0/alpha1) * z0
	SYT = Erate*ALPHA1*(Z0/SZ)**ALPHA/U0/SZ/Cc/SQRTPI
C
	XT = PRMT(12)
	XV(I) = (SYT/RT2/DELTA)**(1.D0/BETA) - XT
C
C... SET UP INTEGRATION FOR THE GAUSSIAN DISPERSION PHASE.
C
	do 600 ijk=1,22
	prmt(ijk) =0.D0
  600	continue

	do 620 ijk=1,5
	y(ijk) = 0.D0
	dery(ijk)= 0.D0
	do 620 ijkl=1,8
	aux(ijkl,ijk) = 0.D0
  620	continue
c
	PRMT(1) = XT
	PRMT(2) = 6.023D23
	PRMT(3) = STPG
	PRMT(4) = ERRG
	PRMT(5) = SMXG
	PRMT(6) = Erate
	PRMT(7) = Cc	! -- OUTPUT
	PRMT(8) = XV(I)
	PRMT(9) = T0(I)
C	PRMT(10)= "BLANK"
	PRMT(11)= NREC(I,2)
C	PRMT(12)= DIST AT COMPLETION -- OUTPUT
c	prmt(13)= "blank"
c	prmt(14)= yc		! output
c	prmt(15)= rho		! output
c	prmt(16)= temp		! output
c	prmt(17)= gamma		! output
	prmt(18)= u0*z0/alpha1
	prmt(19)= rhoa*vkc*ustar*alpha1
	prmt(20)= rholay
	prmt(21)= sz
	prmt(22)= sz
C
	Y(1) = rhouh
	Y(2) = heat
C
	DERY(1) = WTRUH
	dery(2) = WTDHG
C
	NDIM = 2
C
	WRITE(lunlog,1140)
 1140	FORMAT(' Entering Gaussian Stage of Integration ')
C
C... PERFORM INTEGRATION
C
	CALL RKGST(PRMT,Y,DERY,NDIM,IHLF,SSG,SSGOUT,AUX)
C
	IF(IHLF .GE. 10) CALL trap(10,IHLF)
C
	NREC(I,2) = INT(PRMT(11))
	RTOT = RTOT + FLOAT(NREC(I,1) + NREC(I,2))
	WRITE(lunlog,1110) RTOT,I
 1110	FORMAT(5X,'TOTAL NUMBER OF RECORDS = ',1pG13.4,' THROUGH',
     .		' OBS # ',I3)
C
	IF(RTOT .GT. 120000.) CALL trap(11,0)
C
  119	CONTINUE
	write(lunlog, 1150) tstop, Prmt(12)
 1150	format(/,'  Last time Observer was active: ',1pg13.5,' s at ',
     .		1pg13.5,' m')
  120	CONTINUE
C
	RETURN
	END
