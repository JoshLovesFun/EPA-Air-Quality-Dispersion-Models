	PROGRAM JETPLU
c
c............................................................................
c
c	This program calculates the trajectory and dilution of a steady
c	gas jet released at right angles to the wind. The zone before Gaussian 
c	profiles can be assumed for the velocity and concentration profiles
c	is termed the zone of flow establishment; calculations for this zone
c	are done in the routine SETJET.  The zone after Gaussian profiles can
c	be assumed to apply is termed the zone of established flow.  The
c	ordinary differential equations describing this zone are included in
c	MODEL; these are integrated with the routine RKGST.  Interactions
c	between the jet/plume and the ground are accounted for using the method
c	of images.
c
c.............................................................................
c
c	T. Spicer
c	University of Arkansas
c	Department of Chemical Engineering
c	Fayetteville, AR  72701
c
c	501-575-6516
C
C
	Implicit real*8(a-h,o-z), integer*4(i-n)
c
	include 'DEG1.prm'
c
	external model,modout

	character*80 opnf1
	character*1 opnf(80)
	equivalence (opnf1,opnf(1))

	character*80 title(4)
	character*24 tinp
	character*1  stabil(6)

	dimension prmt(6)
	dimension aux(8,6)

	DIMENSION YR(6),dYR(6)

	common
     ./cgprop/	gasmw,gastem,gasrho,gascpk,gascpp,gasulc,gasllc,gaszzc,
     .		gasnam
     ./clevel/	culc, cllc
     ./COEFF/	ALFA1,ALFA2, sc,cd,delta
     ./comatm/	istab,tamb,pamb,humid,isofl,tsurf,ihtfl,htco,iwtfl,wtco,
     .		humsrc
     ./GEN2/	DEN(5,igen)
     ./parm/	u0,z0,zr,rml,ustar,vkc,gg,rhoe,rhoa,deltay,betay,gammaf,
     .		yclow
     ./PHYS/	UA, deltaz,betaz,gammaz,rho, temp,yc
     ./rks/	rk1,rk2,rk3,rk4,rk5,rk6, rate,totrte,xmo,zmo
     ./sys/	sysz,sy,sz,sya,sza

c
	character*3 gasnam

	data vkc/0.35D0/
	data gg/9.81D0/

	data sc/1.42D0/
	data cd/0.2D0/
	data delta/2.15D0/

	data stabil/'A','B','C','D','E','F'/

c
C
C... Pick up the run name and open some files .................................

	call runname(opnf1)
	nchar = index(opnf1,' ') - 1

	opnf1 = opnf1(1:nchar) // '.ino'
	OPEN(UNIT=1, file=opnf1, STATUS='OLD')

	opnf1 = opnf1(1:nchar) // '.ind'
	OPEN(UNIT=2, file=opnf1, STATUS='UNKNOWN')

	opnf1 = opnf1(1:nchar) // '.out'
	OPEN(UNIT=3, file=opnf1, STATUS='UNKNOWN')

	opnf1 = opnf1(1:nchar) // '.vel'
	OPEN(UNIT=4, file=opnf1, STATUS='UNKNOWN')

	READ(1,600) (title(j),J=1,4)
	READ(1,*) u0,z0
	READ(1,*) ZR
	read(1,*) istab, rml, ustar
	read(1,*) tamb, pamb, humid, relhum, tsurf
	read(1,*) avtime, deltay, betay
	read(1,*) deltaz, betaz, gammaz

	read(1,*) gasmw
	read(1,*) gastem
	read(1,*) gasulc, gasllc, gaszzc
	read(1,*) gascpk, gascpp
	read(1,*) nden, nden0
	if(nden0 .gt. 0) then
	    do 100 iii = 1,nden0
	    read(1,*) (den(jjj,iii),jjj=1,5)
  100	    continue
	    den(1,nden0+1) = 2.D0
	    rhoa = den(3,1)
	    rhoe = den(3,nden0)
	else
	    rhoa = Pamb*(1.D0+humid)*wmw/(rgas*(wmw/wma + humid))/Tamb
	    rhoe = pamb*gasmw/rgas/gastem
	endif
	gasrho = rhoe

	read(1,*) isofl

	read(1,*) erate
	QINIT = erate/rhoe
	read(1,*) elejet, diajet

	READ(1,*) ALFA1, ALFA2
	read(1,*) distmx, tend
c

	gamma = (rhoe - rhoa)/rhoe
	yclow = gasllc/4.D0
	if(tend.le.0.D0) yclow = gasllc
c
c... initialize DEN if necessary ......................................
c
	wc = 1.D0
	wa = 0.D0
	enth = cpc(gastem)*(gastem - tamb)
	call setden(wc, wa, enth)

	call adiabat(2,twc,twa,gasllc,ya,cllc,r,w,t,p)
	call adiabat(2,twc,twa,gasulc,ya,culc,r,w,t,p)

c
C
C... PRINT VALUES OF PARAMETERS INITIAL CONDITIONS ......................

	call dattim(tinp)
	write(3,700) tinp
	write(3,710) (title(j),J=1,4)
	write(3,720)
	write(3,730) u0,z0,zr
	write(3,740) stabil(istab)
	if(rml.ne. 0.) then
	    WRITE(3,748) rml
	else
	    write(3,749)
	endif
	write(3,750) ustar,tamb,pamb,humid,relhum
	write(3,760) avtime, deltay, betay, deltaz, betaz, gammaz
	write(3,770)
	write(3,780) gasmw,gastem,gasulc,gasllc
	if(gascpp .ne. 1.D0) then
	    write(3,782) gascpk,gascpp
	else
	    cp = (3.33D4 + gascpk)/gasmw
	    write(3,784) cp
	endif
	write(3,790) nden
	if(nden .gt. 0) then
	    write(3,800)
	    do 200 iii = 1,nden
	    write(3,810) den(1,iii), den(2,iii), den(3,iii)
  200	    continue
	endif
	write(3,820) isofl
	write(3,830)
	write(3,840) erate,elejet,diajet
	write(3,850)
	write(3,860) alfa1,alfa2,distmx
	write(3,870) gaszzc,(100.D0*gasllc),(100.D0*gasulc)
C
C
C... CALCULATE WIND SPEED AT THE JET DISCHARGE HEIGHT ..................
c

	UA=USTar/vkc*(dLOG((elejet+ZR)/ZR)-PSIF(elejet,rml))

C
C... CALL SETJET TO CALCULATE THE INITIAL CONDITIONS ....................
C	FOR JETPLU MODEL INTEGRATION.
c
	DO 300 I=1,6
	dyr(i) = 1.D0
  300	YR(I)=0.D0

	YR(1) = rhoe
	CALL SETJET(YR,UA,QINIT,DIAJET,elejet)
	rho  = gasrho
	temp = gastem
C
C
C... CALCULATEd jet parameters:
c	Yr(1) ... CENTERLINE CONCENTRATION
c	Yr(2) ... sysz (product of local lateral and vertical dimension)
c	Yr(3) ... theta (ANGLE OF TRAJECTORY)
c	Yr(4) ... uc (VELOCITY DECREMENT)
c	Yr(5) ... x (downwind distance)
c	Yr(6) ... zj (elevation)
c
	prmt(1) = 0.D0				! lower limit
	prmt(2) = 100000.			! upper limit
	prmt(3) = max(Yr(5)/20.D0, 1.D-30)	! initial step
	prmt(4) = 0.0001			! error criteria
	prmt(5) = distmx			! maximum step size
	prmt(6) = distmx/2.D0			! approximate output step size

	ndim = 6
	call rkgst(prmt,yr,dyr,ndim,ihlf,model,modout,aux)
c
	if(ihlf.ge.10) then
	    write(3,*) '----ihlf.ge.10----'
	    dist = 0.D0
	    write(2,*) dist
	    stop 'ihlf.ge.10'
	endif
c

C

 600	FORMAT(a80)
C

 700	format(1x,1x,'********************',34x,' JETPLU/DEGADIS v2.1  ',
     .		34x,'********************',//,1x,a24,/)
 710	format(1x,a80)
 720	format(//,' Ambient Meteorological Conditions...')
 730	format(/,' Ambient windspeed at reference height: ',1pg13.5,' m/s',
     .		/,'                      Reference height: ',1pg13.5,' m',
     .		/,'                     Surface roughness: ',1pg13.5,' m')
 740	format( /,'              Pasquill stability class: ',12x,A1)
 748	FORMAT( /,'                  Monin-Obukhov length: ',1pg13.5,' m')
 749	FORMAT( /,'                  Monin-Obukhov length:      infinite')
 750	format(/,'                     Friction velocity: ',1pg13.5,' m/s',
     .		/,'                   Ambient temperature: ',1pg13.5,' K',
     .		/,'                      Ambient pressure: ',1pg13.5,' atm',
     .		/,'                      Ambient humidity: ',1pg13.5,
     .		/,'                     Relative humidity: ',1pg13.5,' %')
 760	format( /,'              Specified averaging time: ',1pg13.5,' s',
     .		/,'                                DELTAy: ',1pg13.5,
     .		/,'                                 BETAy: ',1pg13.5,
     .		/,'                                DELTAz: ',1pg13.5,
     .		/,'                                 BETAz: ',1pg13.5,
     .		/,'                                GAMMAz: ',1pg13.5)
 770	format(//,' Contaminant Properties...')
 780	format( /,'         Contaminant molecular weight: ',1pg13.5,
     .		/,'                  Initial temperature: ',1pg13.5,' K',
     .		/,'              Upper level of interest: ',1pg13.5,
     .		/,'              Lower level of interest: ',1pg13.5)
 782	Format(	  '               Heat capacity constant: ',1pg13.5,
     .		/,'                  Heat capacity power: ',1pg13.5)
 784	Format(	  '                Average heat capacity: ',1pg13.5,
     .		' J/kg K',/)
 790	format(/,' NDEN flag: ',I3)
 800	format(1x,1x,'Mole fraction',1x,1x,'Concentration',1x,4x,'Density',
     .		/,1x,15x,4x,'(kg/m3)',4x,4x,'(kg/m3)',4x)
 810	format(1x,3(1x,1pg13.5))
 820	format(/,' ISOFL flag: ',I3)
 830	format(//,' Release Properties...')
 840	format(/,'                         Release rate: ',1pg13.5,' kg/s',
     .		/,'                  Discharge elevation: ',1pg13.5,' m',
     .		/,'                   Discharge diameter: ',1pg13.5,' m')
 850	format(//,' Model Parameters...')
 860	format( /,'                                ALFA1: ',1pg13.5,
     .		/,'                                ALFA2: ',1pg13.5,
     .		/,'                               DISTMX: ',1pg13.5,' m',//)
 870	format(1x,10x,19('-'),1x,'Centerline',1x,19('-'),20x,
     .		'-----',1x,'At z = ',1pg9.3,' m',1x,'-----',1x,
     .		'Maximum',1x,1x,'Elevation',/,
     .		1x,'Downwind',1x,1x,'Elevation',1x,2x,'Mole',3x,1x,
     .		9x,1x,1x,'Density',1x,'Temperature',
     .		1x,'Sigma y',1x,1x,1x,'Sigma z',1x,1x,2x,'Mole',3x,1x,
     .		2x,'Width to mol%:',3x,1x,2x,'Mole',3x,1x,1x,'for Max',1x,/,
     .		1x,'Distance',1x,1x,9x,1x,'Fraction',1x,'Concentration',
     .		38x,'Fraction',1x,1x,1pE8.2,1x,1x,1pE8.2,1x,1x,
     .		'Fraction',1x,1x,'Mol Frac',1x,/,
     .		1x,3x,'(m)',4x,3x,'(m)',3x,10x,1x,'(kg/m3)',2x,
     .		1x,'(kg/m3)',2x,3x,'(K)',4x,
     .		3x,'(m)',4x,3x,'(m)',4x,10x,3x,'(m)',4x,3x,'(m)',4x,
     .		10x,3x,'(m)',4x,/)

C
	stop
	END
c
C.....................................................................
C
	SUBROUTINE model(sss,yr,dyr,prmt)
c
c....................................................................
c
c   Subroutine which calculates the differentials describing the jet/plume
c
c....................................................................
c
	Implicit real*8(a-h,o-z), integer*4(i-n)
c
	include 'DEG1.prm'
	parameter (rt2 = 1.414213562D0)		! sqrt(2.)

	external syfun
c
	common
     ./COEFF/	ALFA1,ALFA2, sc,cd,delta
     ./comatm/	istab,tamb,pamb,humid,isofl,tsurf,ihtfl,htco,iwtfl,wtco,
     .		humsrc
     ./GEN2/	DEN(5,igen)
     ./parm/	u0,z0,zr,rml,ustar,vkc,gg,rhoe,rhoa,deltay,betay,gammaf,
     .		yclow
     ./PHYS/	UA, deltaz,betaz,gammaz,rho, temp,yc
     ./rks/	rk1,rk2,rk3,rk4,rk5,rk6, rate,totrte,xmo,zmo
     ./sys/	sysz,sy,sz,sya,sza

c
	dimension yr(6),dyr(6),prmt(6)
	DIMENSION A(4,5)

C
C
c...integrated variables....................................................
c
	cc    = yr(1)
	sysz  = abs(yr(2))
	theta = yr(3)
	uc    = yr(4)
	dist  = yr(5)
	zj    = yr(6)

c
c... Calculate UENTR so that E1 is set to zero when UC<0
c
	uentr = max(uc,0.D0)

c
c...and some hybrid variables
c
	ST    = SIN(theta)
	ST2   = ST*ST
	CT    = COS(theta)
	CT2   = CT*CT
c
c... Estimate the density
c
	call adiabat(0,wc,wa,yc,ya,cc,rho,wm,enth,temp)
	gamma = (rho-rhoa)/cc
c
c...calculate the ambient sigmas............................................
c
	sya = deltay*dist**betay
	sza = deltaz*dist**betaz * exp(gammaz*log(dist)**2)
	dsya = 0.
	dsza = 0.
	if(dist .gt. 1.D0) then
	    dsya = sya/dist* betay * ct
	    dsza = sza/dist*(betaz + 2.D0*gammaz*log(dist)) * ct
	endif
c
c...calculate the "constants"/properties for the ellipse including:.........
c	- the values for SY and SZ
c	- the curved perimeter and area of the ellipse
c	- the ambient windspeed averaged over z in the ellipse
c	- RK1 through RK6
c
c... Values for SY and SZ
c
	sstart = sqrt(sysz)
	sinc   = sstart/20.D0
	sylow  = 1.D-10
	syhigh = max(sysz, 10.D0)
	call limit(syfun, sstart, sinc, syhigh, sylow)

	tol = 0.0001D0
	call zbrent(sy, syfun, sylow, syhigh, tol, ierr)
	if(ierr .ne. 0) call trap(2, ierr)
	sz = sysz/sy
c
c
c...the curved perimeter,  area of the ellipse
c

	call ellips(sy*delta, sz*delta, sz*delta, pe, area)

c
c...the ambient windspeed averaged over z in the ellipse (UA).
c	Calculate ALPHA from two velocity points calculated using the log
c	wind profile.  The integrated profile based on ALPHA gives UA.
c	Don't let ZJ be less than ZR.  The 0.01 offset for ZTOP gets
c	around a divide by zero when calculating ALPHA.
c
	zj = max(zj, zr)

	ztop = zj + delta*sz*ct + 0.01D0
	utop = ustar/vkc*(dlog((ztop+zr)/zr) - psif(ztop,rml))
	umid = ustar/vkc*(dlog((zj  +zr)/zr) - psif(zj  ,rml))
	alpha = dlog(utop/umid) / dlog(ztop/zj)
	alpha1 = 1.D0+alpha

	zbot = zj - delta*sz*ct
	zbot = max(zbot, zr)
	ua = umid/(alpha1*(ztop-zbot)*zj**alpha)
     .		*(ztop**alpha1 - zbot**alpha1)

	vj = uc+ua*ct
	vj = max(vj, 0.D0)

c
c...some additional hybrid variables
c
	UA2   = UA*UA
	UACT  = UA*CT
	UAST  = UA*ST
	uauc  = ua*uc
	uc2   = uc*uc
	ucct  = uc*ct
	ucst  = uc*st
	ccsy  = cc*sy
	ccsz  = cc*sz
	ccsysz= cc*sy*sz
C
C... Gas Component Mass Balance Equation .....................................
C
	qqq = rk1*uact + rk2*uc
	rate = ccsysz*qqq

	A(1,1) = qqq * sysz
	A(1,2) = qqq * cc
	A(1,3) = - rk1 * ccsysz * uast
	A(1,4) = rk2 * ccsysz
	A(1,5) = 0.D0
C
C... Overall Mass Balance Equation ...........................................
C
	qqq = rk3*uact + rk4*uc
	totrte = rhoa*qqq*sysz + gamma*rate

        A(2,1) = 0.D0
	A(2,2) = rhoa * qqq
	A(2,3) = - rhoa * rk3 * sysz * uast
	A(2,4) = rhoa * rk4 * sysz

	e3 = rk3*ua*(sza*dsya + sya*dsza)
	e2 = alfa2*uact*abs(st)*pe
	A(2,5) = rhoa*(ALFA1*uentr*pe + e2 + e3)
C
C... Z-direction Momentum Balance Equation ....................................
C
	drag = cd * pe * rhoa/2.D0*uast**2

	rrr = 2.D0*rk2*uauc*ct + rk1*ua2*ct2 + rk6*uc2
	qqq = 2.D0*rk4*uauc*ct + rk3*ua2*ct2 + rk5*uc2
	zmo = rhoa*st*qqq*sysz + gamma*st*rrr*ccsysz

	A(3,1) = gamma*sysz*rrr*st
	A(3,2) = rhoa*qqq*st + gamma*cc*rrr*st
	A(3,3) =   rhoa*sysz*(ct*qqq + st*(
     .			- 2.D0*rk4*uauc*st - 2.D0*rk3*ua2*ct*st))
     .	      + gamma*ccsysz*(ct*rrr + st*(
     .			- 2.D0*rk2*uauc*st - 2.D0*rk1*ua2*ct*st))
	A(3,4) =     rhoa*sysz*st*(2.D0*rk4*uact + 2.D0*rk5*uc)
     .		+ gamma*ccsysz*st*(2.D0*rk2*uact + 2.D0*rk6*uc)
	A(3,5) = -RK1*gg*gamma*ccsysz - sign(1.D0,theta)*drag*CT

C
C... X-Direction Momentum Balance Equation ....................................
C
	rrr = rk1*ua2*ct2*ct + 2.D0*rk2*uauc*ct2 + rk6*uc2*ct
	qqq = rk3*ua2*ct2*ct + 2.D0*rk4*uauc*ct2 + rk5*uc2*ct
	xmo = rhoa*qqq*sysz + gamma*rrr*ccsysz

	A(4,1) = gamma*sysz*rrr
	A(4,2) = rhoa*qqq + gamma*cc*rrr
	A(4,3) =   rhoa*sysz*(-3.D0*rk3*ua2*ct2*st
     .		- 4.D0*rk4*uauc*ct*st - rk5*uc2*st)
     .	      + gamma*ccsysz*(-3.D0*rk1*ua2*ct2*st
     .		- 4.D0*rk2*uauc*ct*st - rk6*uc2*st)
	A(4,4) =     rhoa*sysz*(2.D0*rk4*ua*ct2 + 2.D0*rk5*ucct)
     .		+ gamma*ccsysz*(2.D0*rk2*ua*ct2 + 2.D0*rk6*ucct)
	A(4,5) = UA*A(2,5) + drag*ABS(ST)

C
C... SIMUL USED FOR MATRIX INVERSION
C
	nnn = 4
	call SIMUL(nnn,A,dyr)
c
c... Force any stray derivatives to be physically realistic.
c
	if(dyr(1) .gt. 0.D0) dyr(1) = 0.D0
	if(dyr(2) .lt. 0.D0) dyr(2) = 0.D0
c
c...d(x) and d(z) are calculated directly from cos(theta) and sin(theta),
c	respectively.
c
	dyr(5) = ct
	dyr(6) = st
c
	RETURN
	END
c
c......................................................................
c

	subroutine modout(sss,yr,dyr,ihlf,ndim,prmt)
c
c......................................................................
c
c	subroutine which controls the output to the listing file
c
c......................................................................
c
	Implicit real*8(a-h,o-z), integer*4(i-n)
c
	include 'DEG1.prm'
c
	common
     ./cgprop/	gasmw,gastem,gasrho,gascpk,gascpp,gasulc,gasllc,gaszzc,
     .		gasnam
     ./clevel/	culc,cllc
     ./COEFF/	ALFA1,ALFA2, sc,cd,delta
     ./parm/	u0,z0,zr,rml,ustar,vkc,gg,rhoe,rhoa,deltay,betay,gammaf,
     .		yclow
     ./PHYS/	UA, deltaz,betaz,gammaz,rho, temp,yc
     ./rks/	rk1,rk2,rk3,rk4,rk5,rk6, rate,totrte,xmo,zmo
     ./sys/	sysz,sy,sz,sya,sza

	dimension yr(6),dyr(6),prmt(6)

	character*3 gasnam

	data ooo1/0.D0/, ooo2/0.D0/
	data iip/0/
c
c
	cc    = yr(1)
	sysz  = yr(2)
	theta = yr(3)
	uc    = yr(4)
	dist  = yr(5)
	zj    = yr(6)
c
c... Find the maximum concentration elevation and value......................
c	Use a Newton-Raphson
c
	arg1 = 1.D6
	if(zj .gt. 0.D0) arg1 = 2.D0*(sz*cos(theta)/zj)**2
	if(arg1.ge.1.D0/2.31125D0 .and. zj.gt.0.D0) then
	    zm = zj
	    do 100 iii=1,20
		z1 = zj - zm
		az1= (z1/sz)**2
		ez1= exp(-0.5D0*az1)
		z2 = zj + zm
		az2= (z2/sz)**2
		ez2= exp(-0.5D0*az2)
		ff = z1*ez1 - z2*ez2
		fp = (az1 - 1.D0)*ez1 + (az2 - 1.D0)*ez2
		zmn = zm - ff/fp
		if(abs(zmn-zm).le.0.0001D0 .or. abs(ff).le.1.D-14) goto 110
		zm = zmn
 100	    continue
	    stop 'MODOUT Loop 100 failed.'
 110	    continue
	    if(zm .lt. 0.0001D0) then
		zm = 0.D0
		z1 = zj - zm
		az1= (z1/sz)**2
		ez1= exp(-0.5D0*az1)
		z2 = zj + zm
		az2= (z2/sz)**2
		ez2= exp(-0.5D0*az2)
	    endif
	    cm = cc*(ez1+ez2)
	    call adiabat(0,wc,wa,ym,ya,cm,rho,wm,enth,temp)
	else
	    zm = zj
	    cm = cc
	    call adiabat(0,wc,wa,ym,ya,cm,rho,wm,enth,temp)
	endif
c
c... Estimate CZSP and YZSP at y'=0 and z=GASZzc..............................
c	Estimate as though z' and z represent the same distances.  Note that
c	no contribution from either exponential is included if the location
c	is outside the plume (outside DELTA*SZ which means ARG<2.31125).
c
	arg1 = 0.5D0*((zj - gaszzc)/sz)**2
	if(arg1 .lt. 2.31125D0) then
	    arg1 = exp(-arg1)
	else
	    arg1 = 0.D0
	endif

	arg2 = 0.5D0*((zj + gaszzc)/sz)**2
	if(arg2 .lt. 2.31125D0) then
	    arg2 = exp(-arg2)
	else
	    arg2 = 0.D0
	endif

	czsp = cc*(arg1 + arg2)

	call adiabat(0,wc,wa,yzsp,ya,czsp,rho,wm,enth,temp)

	if(czsp .gt. culc) then
	    wulc = sy*sqrt(2.D0*log(czsp/culc))
	    wllc = sy*sqrt(2.D0*log(czsp/cllc))
	else if(czsp .gt. cllc) then
	    wulc = 0.D0
	    wllc = sy*sqrt(2.D0*log(czsp/cllc))
	else
	    wulc = 0.D0
	    wllc = 0.D0
	endif

	wulc = min(wulc, delta*sy)
	wllc = min(wllc, delta*sy)
C
C... IF THE JET REACHES THE GROUND, STOP THE COMPUTATION.......................
C
	IF (zj.LE.0.D0) then
	    cc    = cc    - zj*(occ    - cc   )/(ozj - zj)
	    sy    = sy    - zj*(osy    - sy   )/(ozj - zj)
	    sz    = sz    - zj*(osz    - sz   )/(ozj - zj)
	    theta = theta - zj*(otheta - theta)/(ozj - zj)
	    uc    = uc    - zj*(ouc    - uc   )/(ozj - zj)
	    dist  = dist  - zj*(odist  - dist )/(ozj - zj)
	    ua    = ua    - zj*(oua    - ua   )/(ozj - zj)
	    yzsp  = yzsp  - zj*(oyzsp  - yzsp )/(ozj - zj)
	    wllc  = wllc  - zj*(owllc  - wllc )/(ozj - zj)
	    wulc  = wulc  - zj*(owulc  - wulc )/(ozj - zj)

	    zj = 0.D0
	    cc = 2.D0*cc

	    call adiabat(0,wc,wa,yc,ya,cc,rho,wm,enth,temp)

	    zm = 0.D0
	    ym = yc

	    WRITE(2,*) dist,cc,delta*sy

	    if(wulc.eq.0.D0 .and. wllc.eq.0.D0) then
		WRITE(3,732) dist,zj,yc,cc,rho,temp,sy,sz,yzsp,ym,zm
	    else if(wulc.eq.0.D0 .and. wllc.ne.0.D0) then
		WRITE(3,733) dist,zj,yc,cc,rho,temp,sy,sz,yzsp,wllc,ym,
     .				zm
	    else
		WRITE(3,734) dist,zj,yc,cc,rho,temp,sy,sz,yzsp,wllc,wulc,
     .				ym,zm
	    endif

	    WRITE(4,9998) dist,zj,cc,sy,sz,uc,ua,theta
	    WRITE(6,9999) dist,zj,yc,sy,sz,uc,ua,theta
	    iip = iip + 1
	    if(iip .eq. 3) then
		write(3,735)
		write(6,735)
		iip = 0
	    endif
	    prmt(5) = 1.
	    return
	endif
c
c... store the present values for interpolation when ZJ=0
c
	occ    = cc
	osy    = sy
	osz    = sz
	otheta = theta
	ouc    = uc
	odist  = dist
	ozj    = zj
	oua    = ua
	oyzsp  = yzsp
	owllc  = wllc
	owulc  = wulc
c
c... Calculate the image contribution .........................................
c
	cimage = cc*exp(-0.5D0*(2.D0*zj/sz)**2)
	cc = cc + cimage

	call adiabat(0,wc,wa,yc,ya,cc,rho,wm,enth,temp)
c
C... IF THE JET ym drops below yclow, stop the calculation.....................
C
	IF (ym.Lt.yclow) then
	    if(wulc.eq.0.D0 .and. wllc.eq.0.D0) then
		WRITE(3,732) dist,zj,yc,cc,rho,temp,sy,sz,yzsp,ym,zm
	    else if(wulc.eq.0.D0 .and. wllc.ne.0.D0) then
		WRITE(3,733) dist,zj,yc,cc,rho,temp,sy,sz,yzsp,wllc,
     .				ym,zm
	    else
		WRITE(3,734) dist,zj,yc,cc,rho,temp,sy,sz,yzsp,wllc,wulc,
     .				ym,zm
	    endif

	    WRITE(4,9998) dist,zj,cc,sy,sz,uc,ua,theta
	    WRITE(6,9999) dist,zj,yc,sy,sz,uc,ua,theta

	    iip = iip + 1
	    if(iip .eq. 3) then
		write(3,735)
		write(6,735)
		iip = 0
	    endif
c
c...	set DIST to 0 to preclude DEGADIS from continuing
	    dist = 0.D0
	    WRITE(2,*) dist,cc,delta*sy
	    prmt(5) = 1.
	    return
	endif
c
c... printed output
c
	if(dist .ge. ooo1+ooo2) then
	    ooo1 = dist
	    ooo2 = prmt(6)

	    if(wulc.eq.0.D0 .and. wllc.eq.0.D0) then
		WRITE(3,732) dist,zj,yc,cc,rho,temp,sy,sz,yzsp,
     .				ym,zm
	    else if(wulc.eq.0.D0 .and. wllc.ne.0.D0) then
		WRITE(3,733) dist,zj,yc,cc,rho,temp,sy,sz,yzsp,wllc,
     .				ym,zm
	    else
		WRITE(3,734) dist,zj,yc,cc,rho,temp,sy,sz,yzsp,wllc,wulc,
     .				ym,zm
	    endif
	    WRITE(4,9998) dist,zj,cc,sy,sz,uc,ua,theta
	    WRITE(6,9999) dist,zj,yc,sy,sz,uc,ua,theta

	    iip = iip + 1
	    if(iip .eq. 3) then
		write(3,735)
		write(6,735)
		iip = 0
	    endif
	endif

c
  732	FORMAT(1X, 9(1pg9.3,1x),20x,1pg9.3,1x,1pg9.3)
  733	FORMAT(1X,10(1pg9.3,1x),10x,1pg9.3,1x,1pg9.3)
  734	FORMAT(1X,12(1pg9.3,1x),1pg9.3)
  735	format(1x)
 9998	FORMAT (8(1x,1pg13.6))
 9999	FORMAT (1x,5(1pg9.3,1x),1pg10.3,1pg9.3,1pg10.3,3(1pg9.3,1x),
     .		1pg10.3,1x,1pg9.3)
	return
	end
c
C........................................................................
c
	SUBROUTINE SETJET(YR,ua,QINIT,DIAJET,elejet)
c
c........................................................................
C
C	THIS SUBROUTINE TRANSFORMS THE EXIT 'TOPHAT' VELOCITY PROFILE OF
C	A CONTINUOUS JET TO THE SIMILARITY (GAUSSIAN) FORM REQUIRED FOR
C	INPUT TO THE JETPLU MODEL - USES WIND TUNNEL DATA CORRELATIONS BY
C	Y. KAMOTANI AND I. GREBER, NASA CONTRACTOR REPORT CR-2392, 3/74.
c
c	This has been simplified for vertical releases only.
c
c........................................................................
C

	Implicit real*8(a-h,o-z), integer*4(i-n)
c
	include 'DEG1.prm'
	parameter (rt2 = 1.414213562D0)		! sqrt(2.)
c
	common
     ./COEFF/	ALFA1,ALFA2, sc,cd,delta
     ./parm/	u0,z0,zr,rml,ustar,vkc,gg,rhoe,rhoa,deltay,betay,gammaf,
     .		yclow
     ./rks/	rk1,rk2,rk3,rk4,rk5,rk6, rate,totrte,xmo,zmo
     ./sys/	sysz,sy,sz,sya,sza



	DIMENSION YR(6)
c
c... initial values for CC and UJ .............................................
c
	cc = yr(1)
	uj = qinit/pi/diajet**2*4.D0
c
c... Calculate the length of the zone of flow development based on analysis
c	of Pratte and Baines work in the ASCE Journal of the Hydraulics
c	Division.
c
c.....SOD represents (S/D) or length of development zone (S) nondimensionalized
c	using the jet diameter (D).
c
	sod = 7.7D0*(1.D0 - exp(-0.48D0*sqrt(rhoe*uj/rhoa/ua)))

c
c...Based on RJ ("J" in Kamotani and Grebber) and FROUDE (Froude number), .....
c	calculate "ae" and "be" (represented herein as AJ and BJ).

	rj = (rhoe/rhoa)*(Uj/ua)**2
	delrho = rhoe - rhoa
	froude = 1.688D0
	if(DELRHO .GT. 0.) froude = rhoa*ua**2/gg/delrho/diajet
	froude = min(froude, 1.688D0)

	if(rj .le. 0.036D0) then
	    aj = 18.519D0*rj
	    bj = 0.4D0
	else if(rj .gt. 0.036D0  .and. rj .le. 10.D0) then
	    aj = EXP(0.2476D0 +0.3016D0*log(froude) +0.24386D0*LOG(rj))
	    bj = 0.4D0
	else if(rj .gt. 10.D0  .and. rj .le. 50.D0) then
	    aj =EXP(0.405465D0 +0.131386D0*LOG(rj) +0.054931D0*log(rj)**2)
	    bj = exp(-0.744691D0 - 0.074525D0*log(rj))
	else if(rj .gt. 50.D0  .and. rj .le. 600.D0) then
	    aj = EXP(-2.55104D0 +1.49202D0*log(rj) -0.097623D0*log(rj)**2)
	    bj = exp(-0.446718D0 - 0.150694D0*log(rj))
	else
	    write(6,*) 'J exceeded in SETJET; Aj and Bj are extrapolated.'
	    aj = EXP(1.44099D0 + 0.243045D0*log(rj))
	    bj = exp(-0.446718D0 - 0.150694D0*log(rj))
	endif

	bji = 1.D0/bj
c
c...Estimate (z/D) (as ZOD) from AJ, BJ, and SOD using a Newton-Raphson .......
c	procedure.
c
	zod = sod

	iii = 0
 100	continue
	iii = iii+1
	if(iii .gt. 100) then
		stop 'SETJET loop failed'
	endif

	xod = (zod/aj)**bji
	fff = xod**2 + zod**2 - sod**2
	fffp= xod**2*2.D0/bj/zod + 2.D0*zod
	zodn = zod - fff/fffp
	check = abs((zodn-zod)/zod)
	if(check .gt. 0.00001) then
	    zod = zodn
	    goto 100
	endif
c
c... Calculate DIST, THETA, ZJ, and UC from ZOD and XOD.  Note that XOD is ....
c	subject to underflows which is cured on a VAX by (XOD = XOD).
c
	xod = xod
	dist = xod*diajet
	if(xod .gt. 0.D0) then
	    slope = bj*zod/xod
	    theta = atan(slope)
	else if(xod .eq. 0.D0) then
	    theta = pi/2.D0
	else
	    stop 'XOD < 0. in SETJET.'
	endif
	zj = zod*diajet + elejet
	uc = uj - ua*cos(theta)
c
c...Determine SY and SZ to close the material balance .........................
c
	erate = qinit*rhoe
c
c... Calculate the constants RK1 to RK6; PPP "corrects" the area of
c	the assumed rectangle to the ellipse.
c

	ppp = sqrt(pi)/2.D0

	qqq = erf(delta /rt2 *ppp)
	rk1 = pi * qqq * (2.D0*qqq)

	qqq = erf(delta /rt2*sqrt(1.D0+sc) *ppp)
	rk2 = pi/(1.D0+sc) * qqq * (2.D0*qqq)

	rk3 = pi*delta**2

	qqq = erf(delta /rt2*sqrt(sc) *ppp)
	rk4 = pi/sc * qqq * (2.D0*qqq)

	qqq = erf(delta *sqrt(sc) *ppp)
	rk5 = pi/(2.D0*sc) * qqq * (2.D0*qqq)

	qqq = erf(delta /rt2*sqrt(1.D0+2.D0*sc) *ppp)
	rk6 = pi/(1.D0+2.D0*sc) * qqq * (2.D0*qqq)

c
c... Calculate SYSZ.
c
	sysz = erate/cc/(rk1*ua*cos(theta) + rk2*uc)

	sy = sqrt(sysz)
	sz = sy

	yr(1) = cc
	yr(2) = sysz
	yr(3) = theta
	yr(4) = uc
	yr(5) = max(dist, 1.D-30)
	yr(6) = zj


	write(6,*) 'JETPLU/SETJET initial conditions...'
	write(6,*) ' '
	write(6,*) '       u0: ',u0    ,'     z0: ',z0
	write(6,*) '    erate: ',erate ,'     uj: ',uj
	write(6,*) '     rhoe: ',rhoe  ,'   rhoa: ',rhoa
	write(6,*) '       rj: ',rj    ,' froude: ',froude
	write(6,*) ' '
	write(6,*) '     dist: ',dist  ,'     zj: ',zj
	write(6,*) '   diajet: ',diajet,'     uc: ',uc
	write(6,*) '       sy: ',sy    ,'     sz: ',sz
	write(6,*) '    theta: ',theta ,'    sod: ',sod
	write(6,*) ' '
	write(6,*) '      rk1: ',rk1   ,'    rk2: ',rk2
	write(6,*) '      rk3: ',rk3   ,'    rk4: ',rk4
	write(6,*) '      rk5: ',rk5   ,'    rk6: ',rk6
	write(6,*) ' '
	write(6,*) ' DIST        Zj        ',
     .		'Yc        Sy        Sz        Uc        Ua        Theta'
	write(6,*) ' '
	RETURN
	END
c
c................................................................
c
	function syfun(sytry)
c
c................................................................
c
c	Function which is used to determine SY and SZ from the product
c	SY*SZ
c
c................................................................
c
	implicit real*8(a-h,o-z), integer*4(i-n)

	common
     ./rks/	rk1,rk2,rk3,rk4,rk5,rk6, rate,totrte,xmo,zmo
     ./sys/	sysz,sy,sz,sya,sza


	sztry = sysz/sytry
C
c	syfun = sya**2 - sza**2 - sytry**2 + sztry**2
c... to improve the accuracy of the calculation...
c
	syfun = (sya-sza)*(sya+sza) + (sztry-sytry)*(sztry+sytry)

	return
	end
