	PROGRAM DEG4mw
C
C***************************************************************************
C***************************************************************************
C***************************************************************************
C
C	Program description:
C
C	DEG4mw reads the downwind dispersion calculation made in DEG2S and
c	outputs the maximum plume width for a specified concentration level.
c
c	Note that no provision is made for vertical jet calculations.
C
C
C	T. O. Spicer
C
C	University of Arkansas
C	Department of Chemical Engineering
C	Fayetteville, AR 72701
C
C	December 1992
C
C
C
C***************************************************************************
C***************************************************************************
C***************************************************************************
C
c
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
     .		'DEG4S can only be used to output',/,
     .		' concentrations in the jet portion of the ',
     .		'calculations.',//,
     .		' To get concentration time histories for the ',
     .		'"nonjet" portion of this',/,
     .		' transient run, use DEG4.')
	endif
C
c... open and read the LIS file to get some parameters of the run.............
c
 122	opnf1 = opnf1(1:nchar) // '.lis'
	OPEN(UNIT=9, file=opnf1, status='OLD', err=100)
C
c... open the MW output file ..................................................
c
	opnf1 = opnf1(1:nchar) // '.mw'
	OPEN(UNIT=8, file=opnf1, status='UNKNOWN')
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

 132	continue
	read(9,1300,end=135,err=135) opnf1
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
c... input the desired concentration ...........................................
c
 200	rewind(unit=9)
	write(6,2150) gasulc, gasllc
 2150	format(//,' The upper and lower levels of concern were ',
     .		'originally specified as: ',/,
     .		' ',1pg13.5,' and ',1pg13.5,'.',//,
     .		' Enter the desired level of concern for the maximum ',
     .		'width calculations: ',$)
	read(5,*) gasloc
	write(8,*) 'Maximum width for ',gasloc
c
c... go through the file for this downwind distance
c
	if(jetplu) call jetpnt(gasloc, xjet)

	if(deg .and. check4) then
	    call srcpnt(gasloc, xjet)
	    call dwnpnt(gasloc)
	endif
c
c... another point?
c
	write(6,3000)
 3000	format(//,' Do you want information for another ',
     .		'concentration? [y,n] <N> ',$)
	read(5,3100) opnf1
 3100	format(a40)
	nchar = index(opnf1,' ')
	if(nchar.eq.1) goto 800
	call upcase(opnf(1))
	nchar = index(opnf1,'Y')
	if(nchar.eq.1) goto 200
C
c... close the file
c
 800	CLOSE(UNIT=9)
	close(unit=8)
c
c... another run?
c
	write(6,3001)
 3001	format(//,' Do you want information about another run? [y,n] ',
     .		'<Y> ',$)
	read(5,3100) opnf1
	nchar = index(opnf1,' ')
	if(nchar.eq.1) goto 100
	call upcase(opnf(1))
	nchar = index(opnf1,'N')
	if(nchar.eq.1) stop
C
	goto 100
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
	subroutine jetpnt(gasloc, xjet)
c
c.............................................................
c
c	Subroutine to output the width to a given concentration level
c	from the JETPLU output.
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
c... read through the output and process each record ..........................
c	The call to adiabat determines the concentration associated with the
c	mole fraction "gasloc".  Adiabatic mixing (first argument=2) is
c	fine as long as no ground-to-plume heat transfer is assumed.  (This
c	might change if provision for a vertical jet is made.)
c
 140	continue
	iii = 0
	call adiabat(2,twc,twa,gasloc,ya,cloc,r,w,t,p)
 100	continue
	read(9,1000,end=2000,err=2000) dist,zj,yc,cc,rho,temp,sy,sz,ym,zm
1000	format(1x,8(g9.3,1x), 30x, g9.3,1x, g9.3)
	iii = iii+1
	if(iii.eq.3) then
	    read(9,'(a4)',end=2000,err=2000) dummy
	    iii = 0
	endif
c
c... determine the width to "gasloc" at "zm" (which, fortunately, is also
c	the maximum width).  Since the profiles are for the concentration
c	domain, convert the listing maximum mole fraction "ym" to a
c	concentration "cm".
c
c... Note that the TOTAL WIDTH is output!
c
	call adiabat(2,twc,twa,ym,ya,cm,r,w,t,p)

	if(cm .gt. cloc) then
	    wloc = sy*sqrt(2.D0*log(cm/cloc))
	else
	    wloc = 0.D0
	endif

	delta = 2.15D0
	width = min(wloc, delta*sy) * 2.D0	! total width

	write(8,*) dist, width, zm
c
c... return or get another record?
c
	if(dist .eq. xjet) then
	    return
	else
	    goto 100
	endif
c
c... This point should never be reached.
c
 2000	continue
	write(6,*) 'JETPNT? Premature end-of-file; xjet.',xjet
	stop
	end
c
c
c
	subroutine srcpnt(gasloc, xjet)
c
c.............................................................
c
c	Subroutine to output the width to a given concentration level
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
	write(8,*) 'Source calculations'
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
	dist = rr + xjet
	width = 2*rr
	if(yc.ge.gasloc) write(8,*) dist, width
	goto 100
c
c... An error on the read means the source listing is complete.
c	report the results
c
 200	continue
	write(8,*) 'End of the Source'
	return
c
 1000	continue
	write(6,*) 'SRCPNT? Premature end-of-file.'
	stop
	end
c
c
c
	subroutine dwnpnt(gasloc)
c
c.............................................................
c
c	Subroutine to output the width to a given concentration level
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
 1300	    format(a80)
 140	read(9,1300) opnf1
	nchar = index(opnf1,'(m)')
	if(nchar.ne.0) then
	    read(9,1300) opnf1
	    goto 150
	endif
	goto 140
c
c... loop through the output ..................................................
c
 150	continue
	iii = 0
	if(cflag) call adiabat(2,twc,twa,gasloc,ya,cloc,r,w,t,p)
 160	continue
	if(cflag) then
	    read(9,162,end=1000,err=1000) dist,yc,cc,rho,temp,bb,sz,sy,
     .		wllc,wulc
 162	    format(1x,11(1x,g9.3,1x))
	else
	    read(9,164,end=1000,err=1000) dist,yc,cc,rho,gamma,
     .					temp,bb,sz,sy,wllc,wulc
 164	    format(1x,3(1x,g9.3,1x)2x,f7.4,2x,1x,g10.3,1x,6(1x,g9.3,1x))
	endif
	iii = iii+1
	if(iii.eq.3) then
	    read(9,'(a4)') dummy
	    iii = 0
	endif
c
	if(cflag) call adiabat(-2,wc,wa,gasloc,ya,cloc,r,w,gamma,tt)

	hw = 0.D0
	if(cc .ge. cloc) hw = sqrt(-log(cloc/cc)) * sy + bb

	if(gaszzc.eq.0.D0 .and. gasllc.eq.gasloc) hw = wllc ! round off control
	if(gaszzc.eq.0.D0 .and. gasulc.eq.gasloc) hw = wulc

	width = 2.D0 * hw
c
c... report the results
c
	write(8,*) dist, width
	goto 160
c
c... return when the end-of-file is encountered
c
 1000	continue
	return
	end 
