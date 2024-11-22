	PROGRAM SLICE
C
c.....................................................................
C
C	SLICE reads the downwind dispersion calculation made in DEG2S and
c	outputs the concentration profile parameters at desired locations.
C
c....................................................................
C

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	include 'DEG1.prm'
C
	COMMON
     ./ALP/	ALPHA,alpha1
     ./cgprop/	gasmw,gastem,gasrho,gascpk,gascpp,gasulc,gasllc,gaszzc,
     .		gasnam
     ./comatm/	istab,tamb,pamb,humid,isofl,tsurf,ihtfl,htco,iwtfl,wtco,
     .		humsrc
     ./comss/	ess,slen,swid,outcc,outsz,outb,outl,swcl,swal,senl,srhl
     ./csprd/	ce, delrmn
     ./dwncmn/	jetplu
     ./GEN2/	DEN(5,igen)
     ./ITI/	T1,TINP,TSRC,TOBS,TSRT
     ./NEND/	POUND
     ./oomsin/	oodist,avtime
     ./PARM/	U0,Z0,ZR,rml,USTAR,vkc,gg,RHOE,RHOA,DELTAy,BETAy,GAMMAF,
     .		yclow
     ./phicom/	iphifl,dellay
     ./PHLAG/	CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
     ./titl/	title
C
	LOGICAL CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
c
	character*24 tsrc,tinp,tobs,tsrt
C
	character*3 gasnam
	character*80 title(4)
c
	logical jetplu, deg
c
	character*80 opnf1
	character opnf(80)
C
	EQUIVALENCE (opnf(1),opnf1)

C.........................................................
C
C	DATA
C
	DATA POUND/-1.D-20/
C
C
C... UNITS:
c
C	9 -- I/O WITH DISK
C
C
C... GET THE FILE NAME FOR FILE CONTROL
C
 100	write(6,1100)
 1100	format(/,' Enter the existing simulation name as a proper',
     .		' file name: ',$)
	read(5,1150) opnf1
 1150	format(a40)
c
c... check to ensure this file exists since there are no error handling
c	provisions in STRT2SS.
c
	nchar = index(opnf1,' ') - 1
	opnf1 = opnf1(1:nchar) // '.tr2'
	open(unit=8, file=opnf1, status='OLD', err=110)
	close(unit=8)
	goto 120

 110	continue
	write(6,1180) (opnf(iii),iii=1,nchar+4)
 1180	format(/,' The specified TR2 file could not be opened: ',40a1)
	write(6,1181)
 1181	format(/,' Attempting to open the INO file...')
	opnf1 = opnf1(1:nchar) // '.ino'
	open(unit=1, file=opnf1, status='OLD', err=100)
c
	READ(1,'(a80)') (title(j),J=1,4)
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
	    do 112 iii = 1,nden0
	    read(1,*) (den(jjj,iii),jjj=1,5)
  112	    continue
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

	close(unit=1)
c
	check4 = .false.
	if(tend.le.0.D0) then
	    check4 = .true.
	    wc = 1.D0
	    wa = 0.D0
	    enth = cpc(gastem) * (gastem - tamb)
	    call setden(wc, wa, enth)
	    goto 122
	endif
	goto 100
C
c... read the TR2 file to get the thermodynamic information...................
c	Note that this also loads DEN.
c
 120	CALL strt2(opnf1, hmrte, ccp)
	if(.not.check4) then
	    write(6,1200)
 1200	    format(//,' This is a transient simulation, and ',
     .		'SLICE can only be used to output',/,
     .		' concentrations for steady-state cases')
	endif
C
c... open and read the LIS file to get some parameters of the run.............
c
 122	opnf1 = opnf1(1:nchar) // '.lis'
	OPEN(UNIT=9, file=opnf1, status='OLD', err=100)
c
c... see which models were run (and set JETPLU and DEG); get XJET AND XDOWN
C
	jetplu = .true.
	read(9,1300) opnf1
 1300	format(a80)
	nchar = index(opnf1,'JETPLU')
	if(nchar.eq.0) then
	    jetplu=.false.
	    xjet = 0.
	    goto 132
	endif
c
c... loop through any initial stuff in the JETPLU output; the next few reads
c	guarantee the comment section is bypassed before checking for '(m)'
c
	do 129 iii=1,8
	read(9,1300) opnf1
 129	continue

 130	read(9,1300) opnf1
	nchar = index(opnf1,'(m)')
	if(nchar.ne.0) then
	    read(9,1300) opnf1
	    call endit(xjet,xxx)
	    goto 132
	endif
	goto 130

 132	read(9,1300,end=135,err=135) opnf1
	read(9,1300,end=135,err=135) opnf1
	deg = .true.
	goto 138

 135	deg = .false.
	goto 200
c
c... loop through the header information from DEGADIS
c
 138	do 139 iii=1,13
	read(9,1300) opnf1
 139	continue

 140	read(9,1300) opnf1
	nchar = index(opnf1,'sec')
	if(nchar.ne.0) then
	    read(9,1300) opnf1
	    call endit(xxx,xrad)
	    goto 142
	endif
	goto 140

 142	xdown = xjet+xrad

 145	if(.not.check4) goto 200
	read(9,1300) opnf1
	nchar = index(opnf1,'(m)')
	if(nchar.ne.0) then
	    read(9,1300) opnf1
	    call endit(xmax,xxx)
	    goto 200
	endif
	goto 145
c
c... input the positions to calculate the concentration........................
c
 200	rewind(unit=9)
	write(6,2150)
 2150	format(//,' Enter the downwind distance (m): ',$)
	read(5,*) xdos
	ydos = 0.D0
	zdos = 0.D0
c
c... go through the file for this downwind distance
c
	if(xdos.le.xjet .and. jetplu) then
	    call jetpnt(xdos, ydos, zdos)
	else if(xdos.lt.xdown .and. deg .and. check4) then
	    write(6,*) 'The chosen position is in the source'
c	    call srcpnt(xdos-xjet, ydos, zdos, xjet)
	else if(xdos.le.xmax .and. deg .and. check4) then
	    call dwnpnt(xdos, ydos, zdos)
	endif

	if(xdos.gt.xjet .and. jetplu .and. .not.deg) write(6,2990)
 2990	format(/,' This point cannot be calculated using SLICE; ',
     .		'the JETPLU calculations were',/,
     .		' not made for low enough concentrations.  Rerun ',
     .		'JETPLUIN with a lower value',/,
     .		' for the lower limit of concern.')

	if(xdos.gt.xmax .and. deg) write(6,2995)
 2995	format(/,' The steady-state simulation was not calculated ',
     .		'to these distances.',/,
     .		' Rerun JETPLUIN with a lower value',/,
     .		' for the lower limit of concern.')
c
c... another point?
c
	write(6,3000)
 3000	format(//,' Do you want information for another point? [y,n] ',
     .		'<Y> ',$)
	read(5,3100) opnf1
 3100	format(a40)
	nchar = index(opnf1,' ')
	if(nchar.eq.1) goto 200
	call upcase(opnf(1))
	nchar = index(opnf1,'N')
	if(nchar.eq.1) goto 800
	goto 200
C
c... close the file
c
 800	CLOSE(UNIT=9)
c
c... another run?
c
	write(6,3001)
 3001	format(//,' Do you want information about another run? [y,n] ',
     .		'<N> ',$)
	read(5,3100) opnf1
	nchar = index(opnf1,' ')
	if(nchar.eq.1) stop
	call upcase(opnf(1))
	nchar = index(opnf1,'Y')
	if(nchar.eq.1) goto 100
C
	STOP
	END
c
c
c
	subroutine endit(xxx,yyy)
c
c......................................................................
c
c	Subroutine to read through and output list and return the last
c	entries in the list from the first two columns of numbers.
c
c......................................................................
c
	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )
c
	character*4 dummy
c
c
	iii = 0
 100	continue
	read(9,*,end=200,err=200) xx,yy
	iii = iii+1
	if(iii.eq.3) then
	    read(9,'(a4)',end=200,err=200) dummy
	    iii = 0
	endif
	xxx = xx
	yyy = yy
	goto 100
c
 200	continue
	return
	end
c
c.............................................................
c
	subroutine jetpnt(xdos,ydos,zdos)
c
c.............................................................
c
c	Subroutine to output the concentration at a given position
c	from the JETPLU output.
c
c.............................................................
c
	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	common
     ./ALP/	ALPHA,alpha1
     ./PARM/	U0,Z0,ZR,rml,USTAR,vkc,gg,RHOE,RHOA,DELTAy,BETAy,GAMMAF,
     .		yclow
c
	character*80 opnf1
	character opnf(80)
C
	EQUIVALENCE (opnf(1),opnf1)

	character*4 dummy
c
c
c... loop through any initial stuff in the JETPLU output; the next few reads
c	guarantee the comment section is bypassed before checking for '(m)'
c
	do 129 iii=1,8
	read(9,1300) opnf1
 1300	format(a80)
 129	continue

 130	read(9,1300) opnf1
	nchar = index(opnf1,'(m)')
	if(nchar.ne.0) then
	    read(9,1300) opnf1
	    goto 140
	endif
	goto 130
c
c... find the two points to use to interpolate the output..................
c
 140	continue
	iii = 0
	disto = -1.D0
 100	continue
	read(9,*,end=2000,err=2000) dist,zj,yc,cc,rho,temp,sy,sz
	iii = iii+1
	if(iii.eq.3) then
	    read(9,'(a4)') dummy
	    iii = 0
	endif
c
c... before moving the values last read to the "o" variables, check to see
c	if the point was just passed
c
	if(xdos .le. dist) goto 200
	disto = dist
	zjo   = zj
	yco   = yc
	cco   = cc
	rhoo  = rho
	tempo = temp
	syo   = sy
	szo   = sz
	goto 100
c
c... interpolate the profile parameters and calculate the desired concentration
c
 200	if(disto.eq.-1.D0) goto 205
	dist = (xdos - disto)/(dist-disto)
	zj   = dist*(zj    - zjo)    + zjo
	yc   = dist*(yc    - yco)    + yco
	cc   = dist*(cc    - cco)    + cco
	rho  = dist*(rho   - rhoo)   + rhoo
	temp = dist*(temp  - tempo)  + tempo
	sz   = dist*(sz    - szo)    + szo
	sy   = dist*(sy    - syo)    + syo
c
c... before determining the concentration at the desired point, take
c	the "image" contribution from CC read from the output listing.
c
 205	cc = cc/(1.D0 + exp(-0.5D0*(2.D0*zj/sz)**2))
c
c... contribution from the "real" jet/plume
c
	arg = 0.5D0*((zj-zdos)/sz)**2
	arg = arg + 0.5D0*(ydos/sy)**2
	if(arg .lt. 2.31125D0) then
	    cdos = cc/exp(arg)
	else
	    cdos = 0.
	endif
c
c... contribution from the "reflected" jet/plume
c
	arg = 0.5D0*((zj+zdos)/sz)**2
	arg = arg + 0.5D0*(ydos/sy)**2
	if(arg .lt. 2.31125D0) then
	    cdos = cdos + cc/exp(arg)
	else
	    cdos = cdos + 0.
	endif
	call adiabat(0,wc,wa,yy,ya,cdos,r,w,t,tt)
c
c... report the results
c
	stop 'sz cannot be calculated yet.'
c
c	alpha and alpha1 may not be set yet
c	sz = ?  ... from E -- same check as in curtain.
c
	write(6,990) xdos, cdos, sz, sqrt(2.D0)*sy
 990	format(//,' From the jet/plume ',
     .		'calculation...',//,' The plume profile parameters',
     .		' at x= ',1pg12.5,' m is estimated as: ')
	return
c
c... This point should never be reached.
c
 2000	continue
	write(6,*) 'JETPNT? Premature end-of-file.'
	stop
	end
c
c
c
	subroutine srcpnt(xdos,ydos,zdos, xjet)
c
c.............................................................
c
c	Subroutine to output the concentration at a given position
c	from the source output.
c
c.............................................................
c
	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )
c
	character*80 opnf1
	character opnf(80)
C
	EQUIVALENCE (opnf(1),opnf1)

	character*4 dummy
c
c
c... loop through any initial stuff in the listing; the next few reads
c	guarantee the comment section is bypassed before checking for '(m)'
c
	do 130 iii=1,8
	read(9,1300) opnf1
 1300	format(a80)
 130	continue

 140	read(9,1300) opnf1
	nchar = index(opnf1,'sec')
	if(nchar.ne.0) then
	    read(9,1300) opnf1
	    goto 150
	endif
	goto 140

 150	continue
c
c... read through the source to find the pertinent info....................
c
	ymin = 2.
	ymax = 0.
	iii = 0
 100	continue
	read(9,*,end=1000,err=200) tt,rr,hh,qstar,sz,yc
	iii = iii+1
	if(iii.eq.3) then
	    read(9,'(a4)') dummy
	    iii = 0
	endif
c
c... record the min and max mole fractions at this location
c
	if(hh.ge.zdos .and. rr.ge.sqrt(xdos**2 + ydos**2)) then
	    ymax = max(ymax, yc)
	    ymin = min(ymin, yc)
	endif
	goto 100
c
c... An error on the read means the source listing is complete.
c	report the results
c
 200	continue
	if(ymin .eq. 2.) ymin = 0.
	write(6,990) xdos+xjet,ydos,zdos, ymax, ymin
 990	format(//,' From the source calculation...',
     .		//' For the position: x= ',
     .		1pg12.5,' m, y= ',1pg12.5,
     .		' m, z= ',1pg12.5,' m',/,
     .		' The maximum mole fraction when the cloud was ',
     .		'at this position was: ',1pg12.5,/,
     .		' The minimum mole fraction when the cloud was ',
     .			'at this position was: ',1pg12.5)
	return
c
 1000	continue
	write(6,*) 'SRCPNT? Premature end-of-file.'
	stop
	end
c
c
c
	subroutine dwnpnt(xdos,ydos,zdos)
c
c.............................................................
c
c	Subroutine to output the concentration at a given position
c	from the downwind output.
c
c.............................................................
c
	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )
c
	include 'DEG1.prm'
C
	COMMON
     ./ALP/	ALPHA,alpha1
     ./cgprop/	gasmw,gastem,gasrho,gascpk,gascpp,gasulc,gasllc,gaszzc,
     .		gasnam
     ./comatm/	istab,tamb,pamb,humid,isofl,tsurf,ihtfl,htco,iwtfl,wtco,
     .		humsrc
     ./comss/	ess,slen,swid,outcc,outsz,outb,outl,swcl,swal,senl,srhl
     ./dwncmn/	jetplu
     ./GEN2/	DEN(5,igen)
     ./PARM/	U0,Z0,ZR,rml,USTAR,vkc,gg,RHOE,RHOA,DELTAy,BETAy,GAMMAF,
     .		yclow
     ./phicom/	iphifl,dellay
C
	character*3 gasnam
c
	character*80 opnf1
	character opnf(80)
C
	EQUIVALENCE (opnf(1),opnf1)

	character*4 dummy
c
	logical cflag, jetplu
c
c
c... set cflag to control calls to ADIABAT
c
	cflag = isofl.eq.1 .or. ihtfl.eq.0
c
c... loop through any initial stuff in the listing; the next few reads
c	guarantee the comment section is bypassed before checking for '(m)'
c
	if(jetplu) then
	    do 100 iii=1,13
 100	    read(9,1300) opnf1
 1300	    format(a80)

 110	    read(9,1300) opnf1
	    nchar = index(opnf1,'(m)')
	    if(nchar.ne.0) goto 120
	    goto 110
	endif

 120	do 130 iii=1,13
 130	read(9,1300) opnf1

 140	read(9,1300) opnf1
	nchar = index(opnf1,'(m)')
	if(nchar.ne.0) then
	    read(9,1300) opnf1
	    goto 150
	endif
	goto 140
c
c... find the two points to use to interpolate the output
c
 150	continue
	iii = 0
	disto = -1.D0
 160	continue
	if(cflag) then
	    read(9,*,end=1000,err=1000) dist,yc,cc,rho,temp,bb,sz,sy
	    gamma = (rhoe - rhoa)/rhoe
	else
	    read(9,*,end=1000,err=1000) dist,yc,cc,rho,gamma,
     .					temp,bb,sz,sy
	endif
	iii = iii+1
	if(iii.eq.3) then
	    read(9,'(a4)') dummy
	    iii = 0
	endif
c
c... before moving the values last read to the "o" variables, check to see
c	if the point was just passed
c
	if(xdos .le. dist) goto 200
	disto = dist
	yco   = yc
	cco   = cc
	rhoo  = rho
	gammao= gamma
	tempo = temp
	bbo   = bb
	szo   = sz
	syo   = sy
	goto 160
c
c... interpolate the profile parameters and calculate the desired concentration
c
 200	if(disto .eq. -1.D0) goto 205
	if(xdos.le.0.D0 .or. dist.le.0.D0 .or. disto.le.0.D0) then
	    offset = 1.1D0 * abs(min(xdos, dist, disto))
	    dist = log((xdos+offset)/(disto+offset))
     .			/log((dist+offset)/(disto+offset))
	else
	    dist = log(xdos/disto)/log(dist/disto)
	endif
	yc    = yco*   exp( log(yc   /yco)    * dist)
	cc    = cco*   exp( log(cc   /cco)    * dist)
	rho   = rhoo*  exp( log(rho  /rhoo)   * dist)
	gamma = gammao*exp( log(gamma/gammao) * dist)
	temp  = tempo* exp( log(temp /tempo)  * dist)
	if(bb.ge.0.D0 .and. bbo.ge.0.D0) then
	    bb    = bbo*   exp( log(bb   /bbo)    * dist)
	else
	    offset = 1.1D0 * abs(min(bb, bbo))
	    bb = (bbo+offset)* exp( log((bb+offset)/(bbo+offset)) * dist)
	    bb = bb - offset
	endif
	sz    = szo*   exp( log(sz   /szo)    * dist)
	sy    = syo*   exp( log(sy   /syo)    * dist)

c	dist = (xdos - disto)/(dist-disto)
c	yc   = dist*(yc    - yco)    + yco
c	cc   = dist*(cc    - cco)    + cco
c	rho  = dist*(rho   - rhoo)   + rhoo
c	gamma= dist*(gamma - gammao) + gammao
c	temp = dist*(temp  - tempo)  + tempo
c	bb   = dist*(bb    - bbo)    + bbo
c	sz   = dist*(sz    - szo)    + szo
c	sy   = dist*(sy    - syo)    + syo
c
 205	arg = (zdos/sz)**alpha1
	if(abs(ydos) .gt. bb) arg = arg + ((abs(ydos)-bb)/sy)**2
	if(arg .lt. 80.) then
	    cdos = cc/exp(arg)
	    if(cflag) then
		call adiabat(0,wc,wa,yy,ya,cdos,r,w,t,tt)
	    else
		call adiabat(-1,wc,wa,yy,ya,cdos,r,w,gamma,tt)
	    endif
	else
	    yy = 0.
	endif
c
c... report the results
c
	write(6,990) xdos, cc, bb, sz, sy
 990	format(//,' From the downwind, ground-level dispersion ',
     .		'calculation...',//' The profile parameters at x= ',
     .		1pg12.5,' m are (log-log interpolation):',//,
     .		'  Concentration: ',1pg13.5,' kg/m3',/,
     .		' Half width (b): ',1pg13.5,' m',/,
     .		'             Sz: ',1pg13.5,' m',/,
     .		'             Sy: ',1pg13.5,' m')

	return
c
c... this point should not be reached.
c
 1000	continue
	write(6,*) 'DWNPNT? Premature end-of-file.'
	stop
	end 
