	PROGRAM DEG2S
c
C***************************************************************************
C***************************************************************************
C***************************************************************************
C
C	Program description:
C
C	DEG2S is a simplification of DEG2 which performs the downwind
C	dispersion portion of the calculation for a steady state source
C	described by DEG1.
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

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	include 'DEG2.prm'
C
	EXTERNAL PSS,PSSOUT,SSG,SSGOUT
C
C
	common
     ./ALP/	ALPHA,alpha1
     ./cgprop/	gasmw,gastem,gasrho,gascpk,gascpp,gasulc,gasllc,gaszzc,
     .		gasnam
     ./comatm/	istab,tamb,pamb,humid,isofl,tsurf,ihtfl,htco,iwtfl,wtco,
     .		humsrc
     ./comfl/	cflag,clfl,cufl
     ./comss/	ess,slen,swid,outcc,outsz,outb,outl,swcl,swal,senl,srhl
     ./csprd/	ce, delrmn
     ./ctprop/	cwc,cwa,centh
     ./ERR2S/	SY0ER,ERRP,SMXP,WTSZP,WTSYP,WTBEP,WTDH,ERRG,SMXG,
     .		WTRUH,WTDHG
     ./GEN2/	DEN(5,igen)
     ./ITI/	T1,TINP,TSRC,TOBS,TSRT
     ./NEND/	POUND
     ./oomsin/	oodist,avtime
     ./parm/	u0,z0,zr,rml,ustar,vkc,gg,rhoe,rhoa,deltay,betay,gammaf,
     .		yclow
     ./phicom/	iphifl,dellay
     ./PHLAG/	CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
     ./STP/	STPP,ODLP,ODLLP,STPG,ODLG,ODLLG
     ./TITL/	TITLE
c
	character*3 gasnam
	character*80 TITLE(4)
	character*24 TSRC,TINP,TOBS,TSRT
C
	logical cflag
	LOGICAL CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
C
C... DIMENSIONS/DECLARATIONS
C
	real*4 tt1
c
	character*4 TR2,ER2,Sr3
C
	character*40 opnf1
	character opnf(40)
	EQUIVALENCE (OPNf(1),opnf1)
c
	dimension prmt(22),y(6),dery(6),aux(8,6)
C
C
	DATA POUND/-1.D-20/
C
	DATA NDIM/0/
C
	DATA TR2/'.TR2'/,ER2/'.ER2'/
	DATA Sr3/'.Sr3'/
C
C
c... get time, run name, etc.................................................
c
	T1 = SECNDS(0.)
C
C... GET THE FILE NAME
C
	call runname(opnf1)
	nchar = index(opnf1,' ') - 1
C
	opnf1 = opnf1(1:nchar) // ER2(1:4)
	CALL ESTRT2(OPNf1)
C
C... GET THE COMMON VARIABLES CARRIED FROM DEG1
C
	opnf1 = opnf1(1:nchar) // tr2(1:4)
	CALL STRT2(OPNf1,Hmrte,CCP)
c
	call dattim(tobs)
C
	opnf1 = opnf1(1:nchar) // sr3(1:4)
	OPEN(UNIT=8, file=OPNf1, status='UNKNOWN')
c
	cflag = isofl.eq. 1.or. ihtfl.eq. 0
C
c... write the output heading
c
	WRITE(8,1119)
	if(cflag) then
	    WRITE(8,1116) gaszzc,(100.*gasllc),(100.*gasulc)
	    WRITE(8,1118)
	else
	    WRITE(8,1115) gaszzc,(100.*gasllc),(100.*gasulc)
	    WRITE(8,1117)
	endif
	WRITE(8,1119)
c
 1115	FORMAT(1H0,1X,'Distance',2x,3x,'Mole',3x,
     .		'Concentration',1x,'Density',2x,3x,'Gamma',4x,
     .		'Temperature',3x,'Half',4x,4x,'Sz',5x,4x,'Sy',5x,
     .		'Width at z=',0pf6.2,' m to:',/,1x,11x,1x,'Fraction',2x,
     .		11x,11x,11x,11x,3x,'Width',3x,11x,9x,
     .		2(1pg9.3,'mole%',1x))
 1116	FORMAT(1H0,1X,'Distance',2x,3x,'Mole',3x,
     .		'Concentration',1x,'Density',2x,
     .		'Temperature',3x,'Half',4x,4x,'Sz',5x,4x,'Sy',5x,
     .		'Width at z=',0pf6.2,' m to:',/,1x,11x,1x,'Fraction',2x,
     .		11x,11x,11x,3x,'Width',3x,11x,9x,
     .		2(1pg9.3,'mole%',1x))
 1117	FORMAT(1H ,4X,'(m)',4x,11x,
     .		2(1X,'(kg/m**3)',1x),12x,4x,'(K)',
     .		5(8X,'(m)'))
 1118	FORMAT(1H ,4X,'(m)',4x,11x,
     .		2(1X,'(kg/m**3)',1x),4x,'(K)',
     .		5(8X,'(m)'))
 1119	FORMAT(1H )
C
c/////////////////////////////////////////////////////////////////////////
C... STEADY STATE CALCULATIONS
c\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
C
	AGAIN = .FALSE.
C
	rL = OUTL
	bb = OUTB
	SZ0 = OUTSZ
	Erate = ESS
	QSTR0 = Erate/2.D0/rL/bb
	Cc = OUTCc
	wc = swcl
	wa = swal
	enth = senl
	rho = srhl

c
	if(cc.gt. ccp) then
	    write(lunlog,1126) cc,ccp
 1126	    format(/,' ',10('****'),/,' cc: ',1pg13.5,' is greater',
     .		 ' than ccp: ',1pg13.5,/,' ',10('****'),/)
	    cc =ccp
	endif
C
c... ensure the material balance closes
c
	ratio1= u0*z0/alpha1/ z0**alpha1 * cc /bb/qstr0/rl
	ratio = ratio1* sz0**alpha1 * (bb + sqrtpi/2.D0*sy0er)

	if(ratio.lt. 1.D0) then
	   sy0er = (1.D0/(RATIO1*sz0**alpha1) - bb)*2.D0/sqrtpi
	else
	   sz0 = (1.D0/((bb+ sqrtpi/2.D0*sy0er)*ratio1))**(1.D0/alpha1)
	endif

	humsrc = (1.D0-wc-wa*(1.D0+humid))/wc

	call setden(wc,wa,enth)

	if(cflag) then
	   call adiabat(2,twc,twa,gasllc,ya,clfl,r,w,t,tt)
	   call adiabat(2,twc,twa,gasulc,ya,cufl,r,w,t,tt)
	endif
c
	cclay = cc/dellay
	call adiabat(0,wclay,walay,yclay,yalay,cclay,rholay,w,enthlay,t)
C
C... let everyone know the story
C
	WRITE(lunlog,1170) rL,bb
	WRITE(lunlog,1180) QSTR0,SZ0
	write(lunlog,1185) wclay,walay,rholay,cclay,t
	write(lunlog,1186) wc,wa,rho,cc
c
 1170	FORMAT(' LENGTH: ',1pG13.5,' BEFF: ',1pG13.5)
 1180	FORMAT(' TAKEUP FLUX: ',1pG13.5,' SZ0: ',1pG13.5)
 1185	format(' wclay: ',1pg12.5,' walay: ',1pg12.5,
     .		' rholay: ',1pg12.5,' Cclay: ',1pg12.5,/,
     .		' temlay: ',1pg13.5)
 1186	format(' wc: ',1pg12.5,'  wa: ',1pg13.5,/,
     .		' rho: ',1pg12.5,' Cc: ',1pg12.5)
C
C... PREPARE FOR STEADY STATE INTEGRATION..................................
C
	PRMT(1) = rL/2.D0
	PRMT(2) = 6.023D13
	PRMT(3) = STPP
	PRMT(4) = ERRP
	PRMT(5) = SMXP
	PRMT(6) = Erate
	PRMT(7) = Cc	! OUTPUT
	PRMT(8) = bb	! OUTPUT
C
C... PRMT(9) & PRMT(10) ARE CONSTANTS FOR D(SY) & D(SZ)
C
	PRMT(9) = Ce*sqrt(gg*Z0/ALPHA1*GAMMAF) *GAMMAF/U0
	PRMT(10)= Z0**ALPHA*vkc*USTAR*ALPHA1 * ALPHA1/U0
C	PRMT(11)= NREC
C	PRMT(12)=
C	PRMT(13)=
	prmt(18)= u0*z0/alpha1
	prmt(19)= rhoa*vkc*ustar*alpha1
	prmt(20)= rholay
	prmt(21)= sz0
	prmt(22)= sz0
C
	Y(1) = rholay*prmt(18)*(SZ0/z0)**alpha1	! rholay*ueff*heff
	Y(2) = SY0ER*sy0er
	Y(3) = bb + sqrtpi/2.D0*sy0er
	Y(4) = 0.D0			! added heat
	Y(5) = 0.D0			! mass above UFL
	Y(6) = 0.D0			! mass above LFL
C
	DERY(1) = WTSZP
	DERY(2) = WTSYP
	DERY(3) = WTBEP
	dery(4) = wtdh
	dery(5) = 1.D0
	dery(6) = 1.D0
C
	ndim = 6
C
	WRITE(lunlog,1130)
 1130	FORMAT('  Entering Integration Step -- B > 0. ')
C
C... PERFORM INTEGRATION
C
	CALL RKGST(PRMT,Y,DERY,NDIM,IHLF,PSS,PSSOUT,AUX)
C
	IF(IHLF .GE. 10) CALL trap(9,IHLF)
C
	NREC = INT(PRMT(11))
	WRITE(lunlog,1100) NREC
 1100	FORMAT(3X,'NUMBER OF RECORDS IN PSS = ',I10)
C
	IF(AGAIN) THEN
		Y(3) = Y(5)		! mass above UFL
		Y(4) = Y(6)		! mass above LFL
		GO TO 120
		ENDIF
C
C... GAUSIAN COMPLETION OF THE INTEGRATION.............................
C
C... PSSOUT FORCES THE ABOVE INTEGRATION TO FINISH WHEN B<0 FOR THE
C	FIRST TIME. THE STEP BEFORE THIS OCCURS IS RECORDED ON UNIT 7.
C	Y currently contains the interpolated values to b=0.
C
C	THE CALCULATION METHOD CHANGES THE CURRENT VALUE OF SY TO A VALUE
C	CALCULATED AS IF BEFF=SY RETAINING THE LAST VALUE OF Cc IN THE
C	MATERIAL BALANCE.
c
c.......................................................................
C
	heat = Y(4)
	rholay = prmt(20)
	Cc = PRMT(7)
	rhouh = Y(1)
	SZ = ( rhouh/rholay/prmt(18) )**(1.D0/alpha1) * z0
	SYT = Erate*ALPHA1 *(Z0/SZ)**ALPHA/U0/SZ/Cc/SQRTPI
C
	XT = PRMT(12)
	XV = (SYT/RT2/DELTAy)**(1.D0/BETAy) - XT
C
C... SET UP INTEGRATION FOR THE GAUSSIAN DISPERSION PHASE.
C
	do 100 i=1,22
	prmt(i) = 0.D0
 100	continue
c
	PRMT(1) = XT
	PRMT(2) = 6.023D23
	PRMT(3) = STPG
	PRMT(4) = ERRG
	PRMT(5) = SMXG
	PRMT(6) = Erate
	PRMT(7) = Cc		!-- OUTPUT
	PRMT(8) = XV
C	PRMT(9) = "BLANK"
C	PRMT(10)= "BLANK"
C	PRMT(11)= "BLANK"
C	PRMT(12)= DIST AT COMPLETION -- OUTPUT
C
	prmt(18) = u0*z0/alpha1
	prmt(19) = rhoa*vkc*ustar*alpha1
	prmt(20) = rholay
	prmt(21) = sz
	prmt(22) = sz
c
	Y(1) = rhouh
	Y(2) = heat
	Y(3) = Y(5)		! mass above UFL
	Y(4) = Y(6)		! mass above LFL
C
	DERY(1) = wtruh
	dery(2) = wtdhg
	dery(3) = 1.D0
	dery(4) = 1.D0
C
	ndim = 4
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
	NREC = INT(PRMT(11))
C
  120	CONTINUE
c
c... summarize the information about the mass above the LFL,UFL
c
	write(8,8000) 100.*gasulc,100.*gasllc,Y(4)-Y(3),Y(4)
 8000	format(1h0,131('_'),
     .	///,' For the UFL of ',1pg13.5,' mole percent, and',
     .	' the LFL of ',1pg13.5,' mole percent:',
     .	//,' The mass of contaminant between the UFL and LFL is:',
     .	1pg13.5,' kg.',/,' The mass of contaminant above the LFL is: ',
     .	1pg13.5,' kg.')
C
c... completion
C
	CLOSE(UNIT=8)
C
	tt1 = t1
	T1 = SECNDS(tT1)/60.
	if(t1 .le. 0.) t1 = t1 + 60*24
	WRITE(lunlog,4000) TOBS
	WRITE(lunlog,4010) T1
 4000	FORMAT(//,' DEG2S -->',//,3X,'BEGAN AT ',A24)
 4010	FORMAT(3X,'*** ELAPSED TIME *** ',1pG13.5,' min')
C
	STOP
	END
