	PROGRAM JETINT
C***************************************************************************
C***************************************************************************
C***************************************************************************
C
C	Program description:
C
C	JETINT acts as an interactive input module to the programs
C	which make up the DEGADIS model.  The user is guided through a
C	series of questions which supply the model with the necessary
C	input information.
C
C
C	Program usage:
C
C	Consult Volume III of the Final Report to U. S. Coast Guard
C	contract DT-CG-23-80-C-20029 entitled "Development of an
C	Atmospheric Dispersion Model for Heavier-than-Air Gas Mixtures".
C
C	J. Havens
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
C
C	INPUT FOR DEGADIS ROUTINES
c
c	This program creates input files for the nonjet portion of the model
c	as needed.  The program is divided into 5 segments performing the
c	following tasks:
c
c		1) File menu
c		2) Input of atmospheric parameters
c		3) Input of material specifications
c		4) Input of release specifics (rate, etc.)
c		5) Creating an IN file
c
c	Several procedures in this program are operating system dependent;
c	these are included the following subroutines:
c
c		DIRDIR	- procedure for looking up the file names of existing
c			  files.
c
C
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

C
	include 'degin.prm'
c
	common
     ./cgprop/	gasmw,temjet,gasrho,gascpk,gascpp,gasulc,gasllc,gaszzc,
     .		gasnam
     ./comss/	erate,slen,swid,outcc,outsz,outb,outl,swcl,swal,senl,srhl
     ./csigx/	sigxco,sigxp,sigxmd,sigxfl
     ./GEN1/	PTIME(igen), ET(igen), R1T(igen), PWC(igen), PTEMP(igen),
     .		PFRACV(igen), PENTH(igen), PRHO(igen)
     ./GEN2/	DEN(5,igen)
     ./ITI/	T1,TINP,TSRC,TOBS,TSRT
     ./NEND/	POUND
     ./oomsin/	oodist,avtime
     ./parm/	u0,z0,zr,rml,ustar,vkc,gg,rhoe,rhoa,deltay,betay,gammaf,
     .		yclow
     ./PHLAG/	CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
     ./TITL/	TITLE
c
	character*3 gasnam
	character*80 TITLE(4)
C
	character*24 TSRC,TINP,TOBS,TSRT

	LOGICAL CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
c
c check1
c check2=t	cloud type release with no liquid source; SRC1	DEG1
c again		local communications in SSSUP			SSSUP
c check3	local communications between SRC1 and NOBL	DEG1
c check4=t	steady state simulation				DEGINP
c check5=t	operator sets sort parameters			ESTRT3
c
C
c... local character variables
c
	character*100 OPNf, runnam
	character*1 OPNf1(100)
	equivalence (opnf1(1),opnf)

	character*3 IN
	character*1 stabil(6)

	character*80 STRING

	character*1 yes(2)
	character*1 cdilut(2)
	character*1 crml(2)

	logical dflag, f830
c
c
	data CHECK1/.false./,CHECK2/.false./,AGAIN/.false./
	data CHECK3/.false./,CHECK4/.false./,CHECK5/.false./
C
	data yes/'n','y'/
	data cdilut/'S','T'/
	data crml/'D','N'/

	data f830/.false./

	DATA POUND/-1.E-20/
C
	DATA PTIME/igen*0.D0/
	DATA ET/igen*0.D0/,R1T/igen*0.D0/
	DATA PWC/igen*0.D0/,PTEMP/igen*0.D0/
	DATA PFRACV/igen*0.D0/,PENTH/igen*0.D0/
	DATA PRHO/igen*0.D0/
	data DEN/igen*0.,igen*0.,igen*0.,igen*0.,igen*0./

	DATA IN/'.IN'/
	data stabil/'A','B','C','D','E','F'/
c
c
	WRITE(6,3005)
 3005	FORMAT(//,16X,'DEGADIS Jet/Plume Model input module.',//,
     .	' When default answers are available to a query, the default',/,
     .	' is given in brackets (<>); for such questions, the following',/,
     .	' responses can also be given:',//,
     .	'        /       move to the previous question',/,
     .	'        ?       help',//,
     .	' Other possible answers may be listed in square brackets ([]).')
c
c////////////////////////////////////////////////////////////////////////////
C                         File handling menu
c\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
C
   10	continue
	runnam = ' '
	close(unit=8)
 6000	format(/,1x,35('.'),3x,'help',3x,35('.'))

	write(6,3010)
 3010	format(/,' File menu...',//,
     .	'    1) Create a new input (IN) file.',/,
     .	'    2) Edit an old input (IN) file.',/,
     .	'    3) Create a new input (IN) file from an old one.',/,
     .	'    4) List presently available input (IN) files.',/,
     .	'    5) Exit.',//,
     .	' Enter your choice: ',$)
	read(5,*,err=10) imenu

	goto (100,200,200,300,20), imenu
	goto 10
c
c... Early stop
c
   20	continue
	stop
c
c... Create a new IN file.  Get the file name, and OPEN the file.............
c
  100	continue
	WRITE(6,3100)
 3100	FORMAT(/,' Enter the new simulation name as a proper ',
     .			'file name: ',$)
	READ(5,3102) opnf
 3102	FORMAT(A40)
 3104	format(' More -- <CR> ',$)
	nchar = index(opnf,'/')
	if(nchar .eq. 1) goto 10
	nchar = index(opnf,' ')
	if(nchar .eq. 1) goto 10
	nchar = nchar-1
	opnf = opnf(1:nchar) // in(1:3)
	nrun = nchar
	runnam(1:nrun) = opnf(1:nchar)
c
c... Load some of the default responses........................................
c
	dflag = .true.
	if(again) goto 350
	title(1) = ' '
	title(2) = ' '
	title(3) = ' '
	title(4) = ' '
	u0 = 6.D0
	z0 = 10.D0
	zr = 0.01D0
	indvel= 1
	istab = 4
	avtime = 0.D0
	call atmdef(zr, istab, avtime, rml, deltay, betay, 
     .		deltaz, betaz, gammaz, sigxco, sigxp, sigxmd)
	tamb = 293.15D0
	pamb = 1.D0
	relhum = 50.D0
	vp = watvp(tamb)
	sat = wmw/wma * vp/(pamb - vp)
	humid = relhum/100.D0 * sat
	tsurf = 0.D0
	temjet = tamb
	indht = 0
	nden = 0
	numden  = 2
	itran = 0
	distmx= 50.
	goto 350
c
c... Manipulate an existing IN file.  Get the file name, and OPEN the file....
c
  200	continue
	WRITE(6,3200)
 3200	FORMAT(/,' Enter the existing simulation name as a proper',
     .			' file name: ',$)
	READ(5,3102) opnf
	nchar = index(opnf,'/')
	if(nchar .eq. 1) goto 10
	nchar = index(opnf,' ')
	if(nchar .eq. 1) goto 10
	nchar = nchar-1
	opnf = opnf(1:nchar) // in(1:3)
	nrun = nchar
	runnam(1:nrun) = opnf(1:nchar)

	OPEN(UNIT=8, file=OPNf, status='OLD', err=210)
	goto 220
c
c... Assume that the error is caused because the file cannot be found..........
c
  210	continue
	write(6,3210) (opnf1(iii),iii=1,nchar+4)
 3210	format(' The specified file cannot be opened: ',40a1)
	goto 10
c
c... There is an error reading the file........................................
c
  215	continue
	write(6,3215) (opnf1(iii),iii=1,nchar+4)
 3215	format(' There is an error reading the specified file: ',40a1)
	goto 10
c
c... Read the contents of the old file.........................................
c
  220	continue

	dflag = .false.
	DO 222 I=1,4
	READ(8,'(a80)',err=215) TITLE(I)
  222	CONTINUE

	READ(8,*,err=215) U0, Z0
	READ(8,*,err=215) zr
		fzr = zr
	read(8,*,err=215) indvel, istab, rml
		indvelf= indvel
		ifstab = istab
		frml = rml
	read(8,*,err=215) tamb, pamb, relhum
	read(8,*,err=215) tsurf
	read(8,'(A3)',err=215) gasnam
		call upcase(gasnam)
	read(8,*,err=215) gasmw
	read(8,*,err=215) avtime
		favtim = avtime
	read(8,*,err=215) temjet
	read(8,*,err=215) gasulc, gasllc, gaszzc
	read(8,*,err=215) indht, gascpk, gascpp
	read(8,*,err=215) nden
C
	vp = watvp(tamb)		! calculate the absolute humidity from the inputs
	sat = wmw/wma * vp/(pamb - vp)
	humid = relhum/100.D0 * sat

	if(nden .le. 0) then
	    rhoe = pamb*gasmw/rgas/tamb
	    rhoa = Pamb*(1.D0+humid)*wmw/(rgas*(wmw/wma + humid))/Tamb
	    goto 226
	endif
	numden = nden
	DO 224 I=1,Numden
  224	READ(8,*,err=215) DEN(1,I),DEN(2,I),den(3,I)
	RHOE = DEN(3,Numden)
	gasrho = rhoe
	RHOA = DEN(3,1)
	den(1,numden+1) = 2.D0
C
  226	continue
C
	read(8,*,err=215) erate
	read(8,*,err=215) elejet, diajet
	read(8,*,err=215) tend
	check4 = .false.
	itran = 1
	if(tend .eq. 0.) then
	    check4 = .true.
	    itran = 0
	endif
C
	CLOSE(UNIT=8)
c
c... get the new file name as appropriate.....................................
c
	if(imenu .eq. 3) then
	    WRITE(6,3100)
	    READ(5,3102) opnf
	    nchar = index(opnf,'/')
	    if(nchar .eq. 1) goto 10
	    nchar = index(opnf,' ')
	    if(nchar .eq. 1) goto 10
	    nchar = nchar-1
	    opnf = opnf(1:nchar) // in(1:3)
	    nrun = nchar
	    runnam(1:nrun) = opnf(1:nchar)
	endif
	goto 350
c
c... Call DIRDIR to look at the existing files.................................
c
  300	call dirdir('.in')
	goto 10
c
c... Enter the Title...........................................................
c
  350	continue
	write(6,3350) (title(iii),iii=1,4)
 3350	format(/,' Title Block.',//,
     .		' The current title is...',//,4(1x,a80,/),/)
	write(6,3352)
 3352	format(' Do you wish to change this? [y,n,/] <y> ',$)
	read(5,3102) string
	nchar = index(string,' ')

	if(string .eq. '/') then
	    goto 10
	else if(string .eq. 'N' .or. string .eq. 'n') then
	    goto 400
	else if(string.ne.'Y' .and. string.ne.'y' .and. nchar.ne.1) then
	    goto 350
	endif

	WRITE(6,3354)
 3354	FORMAT(' Enter Title Block -- 4 lines of up ',
     .			'to 80 characters each',/)
C
	DO 360 iii=1,4
	READ(5,'(a80)') TITLE(iii)
  360	continue
C
c/////////////////////////////////////////////////////////////////////////
c                    Enter the atmospheric parameters
c\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
c
  400	continue
	write(6,3400)
 3400	format(/,' ',20('-'),' Atmospheric parameters',20('-'),//)

c--------------------------------------------------------------------------- u0

  410	iq = iquery('Enter the atmospheric windspeed (m/s)',u0, u0)
	if(iq .lt. 0) then
	    goto 350
	else if(iq .gt. 0) then
	    write(6,6000)
	    write(6,6410)
 6410	    format(/,' The windspeed (u0) must be specified at a ',
     .			'given elevation (z0); z0 is',/,
     .		' chosen to be representative of the depth ',
     .			'of the contaminant layer and is',/,
     .		' typically taken to be 10 m for ground-level ',
     .			'releases.  The windspeed',/,
     .		' is considered constant during the release.')
	    write(6,6411)
 6411	    format(/,' For windspeeds less than about 2 m/s (at 10 m), ',
     .			'some assumptions used',/,
     .		' in the jet-plume model and the downwind dispersion ',
     .			'phase of DEGADIS may',/,
     .		' no longer be valid.  For low windspeed cases when ',
     .			'the initial source',/,
     .		' momentum is not significant, this type of ',
     .			'release can be modeled using',/,
     .		' DEGADIS by inputing a zero windspeed in ',
     .			'the model.  Concentrations will',/,
     .		' then be calculated for the secondary ',
     .			'source cloud only.  The distance',/,
     .		' associated with the calculated concentrations ',
     .			'should be adjusted to reflect',/,
     .		' the movement of the secondary source cloud by ',
     .			'any ambient wind present.')
	    goto 410
	endif

c--------------------------------------------------------------------------- z0

  420	iq = iquery(
     .	 'Enter the elevation where the windspeed is given (m)',z0, z0)
	if(iq .lt. 0) then
	    goto 410
	else if(iq .gt. 0) then
	    write(6,6000)
	    write(6,6410)
	    goto 420
	endif

c--------------------------------------------------------------------------- zr

  430	iq = iquery('Enter the surface roughness (m)',zr, zr)
	if(iq .lt. 0) then
	    goto 420
	else if(iq .gt. 0) then
	    write(6,6000)
	    write(6,6430)
 6430	    format(/,' The surface roughness (along with the ',
     .				'atmospheric stability and',/,
     .			' Monin-Obukhov length) is used to ',
     .				'determine the friction velocity and to',/,
     .			' characterize the windspeed profile.  The ',
     .				'assumed logarithmic windspeed',/,
     .			' profile assumes that the surface roughness ',
     .				'elements are homogeneous.',/,
     .			' The assumed profile may not be reasonable ',
     .				'for elevations which are less',/,
     .			' than the height of the surface elements.',
     .				'  (Typically, the surface roughness',/,
     .			' can be approximated as one-tenth of the ',
     .				'height of the surface elements',/,
     .			' for aerodynamic purposes.)',//,
     .			' When considering dispersion of denser-than',
     .				'-air contaminants, the surface',/,
     .			' roughness used in DEGADIS should reflect ',
     .				'surface element heights which are',/,
     .			' less than the depth of the dispersing ',
     .				'layer.  For typical field scale',/,
     .			' releases, the surface roughness used in ',
     .				'DEGADIS should be less than 0.1 m.')
	    goto 430
	endif
C
c... stability and averaging time for DELTAY and derived parameters
c
c------------------------------------------------------------------------- istab

  440	WRITE(6,3440) stabil(istab)
 3440	FORMAT(/,' Enter the Pasquill stability class: ',
     .			'[A,B,C,D,E,F,/,?] <',a1,'> ',$)
	read(5,3102) string
	nchar = index(string,' ')

	IF(STRING.eq.'A' .or. string.eq.'a') then
	    istab=1
	else IF(STRING.eq.'B' .or. string.eq.'b') then
	    istab=2
	else IF(STRING.eq.'C' .or. string.eq.'c') then
	    istab=3
	else IF(STRING.eq.'D' .or. string.eq.'d') then
	    istab=4
	else IF(STRING.eq.'E' .or. string.eq.'e') then
	    istab=5
	else IF(STRING.eq.'F' .or. string.eq.'f') then
	    istab=6
	else if(string.eq.'/') then
	    goto 430
	else if(string.eq.'?') then
	    write(6,6000)
	    write(6,6440)
 6440	    format(/,' The Pasquill-Gifford stability class is ',
     .				'used to estimate:',//,
     .			'     windspeed profile',/,
     .			'     Monin-Obukhov length',/,
     .			'     friction velocity',/,
     .			'     x-direction dispersion parameters')
	    goto 440
	else if(nchar.eq.1) then
	    goto 450
	else
	    goto 440
	endif

c------------------------------------------------------------------------ avtime

  450	iq = iquery('Enter the averaging time for estimating DELTAY (s)',
     .		avtime, avtime)
	if(iq .lt. 0) then
	    goto 440
	else if(iq .gt. 0) then
	    write(6,6000)
	    write(6,6450)
 6450	    format(/,' The averaging time used to estimate DELTAY ',
     .			'("tdy") represents the time scale',/,
     .			' associated with lateral plume meander ',
     .			'in the model.  Other time scales are',/,
     .			' also pertinent: "trel", "thaz", and "ttrav".')
	    write(6,6872)
	    goto 450
	endif
c
c... Get the default values for RML
c
	call atmdef(zr, istab, avtime, drml, deltay, betay, 
     .		deltaz, betaz, gammaz, sigxco, sigxp, sigxmd)
c
c... Use the default Monin-Obukhov length or set it?
c
c--------------------------------------------------------------------------- rml

  460	if(.not.dflag) then
	    if(indvelf.eq.1) then
		write(6,3460)
 3460		format(//,' The file showed the Monin-Obukhov ',
     .			'length to be set to the default value.')
	    else if(frml.ne.0.D0) then
		write(6,3461) frml
 3461		format(//,' The file showed the Monin-Obukhov ',
     .			'length to be: ',1pg13.5,' m.')
	    else
		write(6,3462)
 3462		format(//,' The file showed the Monin-Obukhov ',
     .			'length to be infinite.')
	    endif
	endif
	if(drml.ne.0.D0) then
	    write(6,3463) drml
 3463	    format(/,' The default Monin-Obukhov ',
     .			'length is: ',1pg13.5,' m.')
	else
	    write(6,3465)
 3465	    format(/,' The default Monin-Obukhov ',
     .			'length is infinite.')
	endif

	write(6,3466) crml(indvel)
 3466	format(/,' Do you wish to: (D) use the default ',
     .			'Monin-Obukhov length; or (N) not?',//,
     .			' [D,N,/,?] <',a1,'> ',$)
	read(5,3102) string
	nchar = index(string,' ')
	if(nchar.eq.1) string = crml(indvel)

	IF(STRING.eq.'N' .or. string.eq.'n') then
	    indvel = 2
	    if(.not.dflag) rml = frml
  490	    iq = iquery(
     .		'Enter the Monin-Obukhov length (m); 0. for infinity',
     .		rml,rml)
	    if(iq .lt. 0) then
		goto 460
	    else if(iq .gt. 0) then
		write(6,6000)
		write(6,6490)
 6490		format(/,' The Monin-Obukhov length is a length scale ',
     .				'which quantifies the vertical',/,
     .			' stability in the atmosphere; it is a ',
     .				'parameter used in the windspeed',/,
     .			' profile.  Unstable atmospheric stabilities ',
     .				'(A, B, and C) correspond to',/,
     .			' negative Monin-Obukhov lengths, while ',
     .				'stable atmospheric stabilities',/,
     .			' (E and F) correspond to positive lengths.  ',
     .				'Neutral atmospheric stability',/,
     .			' (D) corresponds to an infinite Monin-Obukhov',
     .				' length scale (which is',/,
     .			' represented in DEGADIS with a value of ',
     .				'zero input to the Monin-Obukhov',/,
     .			' length).',//,
     .			' If the Monin-Obukhov length scale is ',
     .				'unknown, DEGADIS estimates it from',/,
     .			' the atmospheric stability and the ',
     .				'surface roughness.')
		goto 490
	    endif
	    goto 530
	else if(string.eq.'D' .or. string.eq.'d') then
	    indvel = 1
	    rml = 0.D0
	    goto 530
	else if(string.eq.'/') then
	    goto 450
	else if(string.eq.'?') then
	    write(6,6000)
	    write(6,6490)
	    goto 460
	else
	    goto 460
	endif
c
c------------------------------------------------------------------------  tamb

  530	iq = iquery('Enter the ambient air temperature (K)',tamb,tamb)
	if(iq .lt. 0) then
	    goto 460
	else if(iq .gt. 0) then
	    write(6,6000)
	    write(6,6530)
 6530	    format(/,' The ambient air temperature should reflect ',
     .		'the conditions present during',/,' the release.')
	    goto 530
	endif

c------------------------------------------------------------------------  pamb

  540	iq = iquery('Enter the ambient pressure (atm)',pamb,pamb)
	if(iq .lt. 0) then
	    goto 530
	else if(iq .gt. 0) then
	    write(6,6000)
	    write(6,6540)
 6540	    format(/,' The ambient air pressure should reflect ',
     .		'the conditions present during',/,' the release.')
	    goto 540
	endif

	if(pamb.lt.0.7D0 .or. pamb.gt.1.1D0) write(6,6545) pamb
 6545	    format(/,' NOTICE: The specified ambient pressure of ',
     .		1pg13.5,' atm appears to',/,' be unrealistic.',//)

c---------------------------------------------------------------------- humidity

  550	write(6,3550)
 3550	format(/,' The ambient humidity can be entered as:',//,
     .		'   (R) Relative or',/,'   (A) Absolute.',//,
     .		' [R,A,/] <R>: ',$)
	read(5,3102) string

	vp = watvp(tamb)
	sat = wmw/wma * vp/(pamb - vp)

	if(string .eq. '/') then
	    goto 540
	else if(string.eq.'a' .or. string.eq.'A') then
  554	    iq = iquery('Enter the ambient absolute humidity',humid,
     .			humid)
	    if(iq .lt. 0) then
		goto 550
	    else if(iq .gt. 0) then
		write(6,6000)
		write(6,6554)
 6554		format(/,' The ambient absolute humidity (kg ',
     .			'water/kg dry air) should reflect ',/,
     .			' the conditions present during the release.')
		goto 554
	    endif
	    relhum = 100.*humid/sat
	    write(6,3554) relhum
 3554	    format(/,' This is a relative humidity of ',1pg13.5,' %')
	    goto 560
	else
  556	    iq = iquery('Enter the ambient relative humidity (%)',
     .				relhum,relhum)
	    if(iq .lt. 0) then
		goto 550
	    else if(iq .gt. 0) then
		write(6,6000)
		write(6,6556)
 6556		format(/,' The ambient relative humidity  (%) should ',
     .			'reflect the conditions ',/,
     .			' present during the release.')
		goto 556
	    endif
	    humid = relhum/100.D0 * sat
	    write(6,3556) humid
 3556	    format(/,' This is an absolute humidity of ',1pg13.5)
	endif


  560	rhoa = Pamb*(1.D0+humid)*wmw/(rgas*(wmw/wma + humid))/Tamb
	write(6,3560) rhoa
 3560	format(/,' Ambient Air density is ',1pg15.7,' kg/m**3')
c
c////////////////////////////////////////////////////////////////////////////
c                     Material specifics for this release
c\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
c
c------------------------------------------------------------------- isothermal?

  600	continue
	write(6,3600)
 3600	format(//,' ',20('-'),' Material specifics for this release',
     .			20('-'),//)

  610	write(6,3610) yes( min(abs(nden),1)+1 )
 3610	format(/,' Is this an "isothermal" spill? [y,n,/,?] <',a1,'> ',$)
	read(5,3102) string
	nchar = index(string,' ')
	if(nchar .eq. 1) string(1:1) = yes( min(abs(nden),1)+1 )

	if(string.eq.'Y' .or. string.eq.'y') then
	    indht = 0
	    if(imenu.ne.2 .and. imenu.ne.3) nden = -1
	    tsurf = tamb
	    goto 670
	else if(string.eq.'/') then
	    goto 550
	else if(string.eq.'?') then
	    write(6,6000)
	    write(6,6610)
 6610	    format(/,' An "isothermal" simulation simply does not ',
     .			'use the energy balance in DEGADIS',/,
     .		' to estimate the mixture density (from the ',
     .			'mixture temperature).  For an',/,
     .		' "isothermal" simulation then, the user must ',
     .			'specify the relationship between',/,
     .		' contaminant mole fraction, contaminant ',
     .			'concentration (kg/m**3), and mixture',/,
     .		' density (kg/m**3) via a series of ordered triples.',
     .			'  This relationship is',/,
     .		' normally determined prior to running DEGADIS by ',
     .			'calculating the pertinent',/,
     .		' properties for various mixtures of air and ',
     .			'contaminant assuming the air',/,
     .		' and contaminant mix adiabatically (i.e., without ',
     .			'any heat transfer with',/,' the surroundings.)')
	write(6,6611)
 6611	format(/,' For preliminary hazard assessment purposes, it ',
     .			'may be adequate to approximate',/,
     .		' the density-concentration relationship using ',
     .			'only two triples - one for pure',/,
     .		' air and the other for the released material.  ',
     .			'(See Spicer, Havens, and Key,',/,
     .		' "Extension of DEGADIS for Modeling Aerosol ',
     .			'Releases," in "International',/,
     .		' Conference on Vapor Cloud Modeling," J. ',
     .			'Woodward, ed., AIChE, 1987.)',//,
     .		' A "nonisothermal" simulation uses the energy ',
     .			'balance in DEGADIS to calculate',/,
     .		' the mixture density (from the mixture ',
     .			'temperature).  For a "nonisothermal"',/,
     .		' simulation, the user must specify the ',
     .			'contaminant heat capacity and release',/,
     .		' temperature.  The energy balance in DEGADIS ',
     .			'accounts for heat transfer with',/,
     .		' the surroundings and for water phase changes ',
     .			'but does not account for',/,
     .		' contaminant phase changes.')
	    goto 610
	else if(string.eq.'N' .or. string.eq.'n') then
	    goto 620
	else
	    goto 610
	endif

c--------------------------------------------------------------- heat transfer?

  620	write(6,3620) yes(indht+1)
 3620	format(/,' Is heat transfer to be included in the',
     .		' calculations [y,n,/,?] <',a1,'> ',$)
	read(5,3102) string
	nchar = index(string,' ')
	if(nchar .eq. 1) string(1:1) = yes(indht+1)

	if(tsurf .eq. 0.D0) tsurf = tamb
	if(string.eq.'Y' .or. string.eq.'y') then
	    indht = 1
	    goto 630
	else if(string.eq.'/') then
	    goto 610
	else if(string.eq.'?') then
	    write(6,6000)
	    write(6,6620)
 6620	    format(/,' DEGADIS has provisions for heat transfer ',
     .			'to the contaminant cloud from the',/,
     .			' ground surface.')
	    goto 620
	else if(string.eq.'N' .or. string.eq.'n') then
	    indht = 0
	    goto 670
	else
	    goto 620
	endif

c---------------------------------------------------------------------- tsurf

  630	iq = iquery('Enter the ground temperature (K)',tsurf,tsurf)
	if(iq .lt. 0) then
	    goto 620
	else if(iq .gt. 0) then
	    write(6,6000)
	    write(6,6630)
 6630	    format(/,' The ground (surface) temperature should reflect',
     .		' the conditions present ',/,' during the release.  ',
     .		'If unknown, the surface temperature can normally',/,
     .		' be approximated by the ambient air temperature.')
	    goto 630
	endif

c------------------------------------------------------------ Property data base

  670	if(imenu.eq.2 .or. imenu.eq.3) then
	    f830 = .false.
	    write(6,3670)
 3670	    format(/,' Regarding the contaminant property ',
     .		'specifications, do you wish to:',//,'     (E) ',
     .		'Edit the specifications taken from the old file,',/,
     .		'     (G) Get new property specifications from ',
     .		'the data base, or',/,'     (N) Make no changes?',//,
     .		' [E,G,N,/,?] <E> ',$)
	    read(5,3102) string
	    nchar = index(string,' ')

	    if(string.eq.'E' .or. string.eq.'e' .or. nchar.eq.1) then
		goto 700
	    else if(string.eq.'G' .or. string.eq.'g') then
		goto 671
	    else if(string.eq.'N' .or. string.eq.'n') then
		f830 = .true.
		goto 870
	    else if(string.eq.'/') then
		if(nden.ne.0) goto 610
		if(indht.eq.0) goto 620
		goto 630
	    else if(string.eq.'?') then
		write(6,6000)
		write(6,6670)
 6670		format(/,' The contaminant property specifications ',
     .			'can be made using one of the',/,
     .				' following options:',//,
     .			'     (E) Edit the specifications taken ',
     .				'from the old file.  This option',/,
     .			'         precludes getting any information ',
     .				'from a gas property data file',/,
     .			'         such as EXAMPLE.GAS.  Choose ',
     .			'this option if you want to keep the',/,
     .			'         released material the same as ',
     .			'the old file, but you wish to change',/,
     .			'         some of the responses.',//,
     .			'     (G) Get new property specifications ',
     .			'from a data base file such as',/,
     .			'         EXMAPLE.GAS.  Choose this option ',
     .			'if you want to create an input',/,
     .			'         file for a different contaminant.',//,
     .			'     (N) Make no changes to the contaminant ',
     .			'specification.  This option',/,
     .			'         merely bypasses all of the ',
     .			'questions about the contaminant.')
		goto 670
	    else
		goto 670
	    endif
	endif
  671	write(6,3671)
 3671	format(/,' For the property data base, enter either:',//,
     .		5x,'(NAME) the name of a data base file',/,
     .		5x,'(*) for a directory of files with extension GAS',/,
     .		5x,'(/) go to the previous question',/,
     .		5x,'(?) help',//,' [NAME,*,/,?] <EXAMPLE.GAS> ',$)
	read(5,3102) string
	nchar = index(string,' ')

	if(nchar.eq.1) then
	    string = 'example.gas'
	    goto 672
	else if(string.eq.'*') then
	    call dirdir('.gas')
	    goto 671
	else if(string.eq.'/') then
	    if(imenu.eq.2 .or. imenu.eq.3) goto 670
	    if(nden.ne.0) goto 610
	    if(indht.eq.0) goto 620
	    goto 670
	else if(string.eq.'?') then
	    write(6,6000)
	    write(6,6671)
 6671	format(/,' Pertinent contaminant properties are included ',
     .			'in data files (extension',/,
     .		' GAS).  The contents of a data file can be edited ',
     .			'with your favorite ',/,
     .		' text editor.  An example data file is included ',
     .			'in EXAMPLE.GAS.  Blank',/,
     .		' entries in the data file are defaulted to the ',
     .			'properties of air with',/,
     .		' the exception of the density which is calculated ',
     .			'with the ideal gas law.')
	    goto 671
	endif
c
c... try to open the file with name STRING
c
  672	continue
	open(unit=10, file=string, status='OLD', err=674)
	goto 680

  674	write(6,3674)
 3674	format(' This file can not be opened.  Try "*" at the prompt.')
	goto 671

c-------------------------------------------------------------------- GASNAM

  680	write(6,3680)
 3680	format(/,' Enter the three letter contaminant designation; *',
     .		' for a list of available',/,' materials in the ',
     .		'file: [NAM,?,/,*] <*> ',$)
	read(5,3102) string
	nchar = index(string,' ')

	if(string.eq.'*' .or. nchar.eq.1) then
	    write(6,3681)
 3681	    format(1x)
  681	    continue
	    do 682 iii=1,20
		read(10,3102,err=696,end=684) string
		write(6,3682) string(1:3), string(6:21)
 3682		format(6x,a3,2x,a15)
  682	    continue
	    write(6,3683)
 3683	    format(1x,50('-'),'<CR> for more',$)
	    read(5,3102) string
	    goto 681
  684	    rewind(unit=10)
	    goto 680
	else if(string.eq.'/') then
	    close(unit=10)
	    goto 671
	else if(string.eq.'?') then
	    write(6,6000)
	    write(6,6680)
 6680	format(/,' If the contaminant you are interested in is ',
     .				'not on the list, enter a unique',/,
     .			' three letter "name" for your contaminant',
     .				' and answer the questions which',/,
     .			' follow.  If you wish, this contaminant ',
     .				'can be added to EXAMPLE.GAS, or a',/,
     .			' new data base file can be created from ',
     .				'EXAMPLE.GAS using you favorite text',/,
     .			' editor.')
	    goto 680
	endif
c
c... At this point, STRING should have a valid name in it.
c
	call upcase(string(1:3))
	gasnam = string(1:3)
	read(10,3102,err=696,end=696) string
  690	read(10,3690,err=696,end=693) string,gasmw,temjet,gasrho,gascpk,
     .		gascpp,gasulc,gasllc,gaszzc
 3690	format(a20,8(1x,f8.0))
	call upcase(string(1:3))
	if(string(1:3) .eq. gasnam(1:3)) goto 698
	goto 690
c
c... The specified contaminant could not be found.
c
  693	write(6,3693)
 3693	format(' The specified material could not be found.  Do ',
     .		'you wish to:',//,5x,'(E) enter the properties ',
     .		'directly, or',/,5x,'(T) try another contaminant ',
     .		'designation again',//,' [e,t] <t> ',$)
	read(6,3102) string
	nchar = index(string,' ')

	if(string.eq.'E' .or. string.eq.'e') then
	    close(unit=10)
	    goto 700
	else if(string.eq.'T' .or. string.eq.'t' .or. nchar.eq.1) then
	    rewind(unit=10)
	    goto 680
	else
	    goto 693
	endif
c
c... There is some problem with this file.
c
  696	write(6,3696)
 3696	format(' There is some trouble with reading this file.')
	close(unit=10)
	goto 671
c
c... Make sure all of the variables are set; if not set them to air
c
  698	continue
	close(unit=10)
	write(6,3698)
 3698	format(/,' Note that the specified density will be adjusted ',
     .		'to ambient pressure.')
	if(gasmw .eq.0.D0) gasmw = wma
	if(temjet.eq.0.D0) temjet= tamb
	gasrho = gasrho*pamb
	if(gasrho.eq.0.D0) gasrho= pamb*gasmw/rgas/temjet
	if(gascpk.eq.0.D0 .and. gascpp.eq.0.D0) then
	    gascpk= 1006.3D0
	    gascpp = 0.D0
	endif
	if(gasulc.eq.0.D0) gasulc= 0.05D0
	if(gasllc.eq.0.D0) gasllc= 0.025D0

c------------------------------------------------------------------------- GASMW

  700	iq = iquery('Enter the contaminant molecular weight (kg/kmol)',
     .		gasmw,gasmw)
	if(iq .lt. 0) then
	    goto 670
	else if(iq .gt. 0) then
	    write(6,6000)
	    write(6,6700)
 6700	    format(/,' The contaminant molecular weight is used ',
     .			'to convert between mass fraction,',/,
     .			' mole fraction, and concentration; it ',
     .			'may also be used to estimate the',/,
     .			' density.',//,
     .			' The contaminant molecular weight can be ',
     .			'the molecular weight of a mixture.',/,
     .			' (Any air present in the released material ',
     .			'should not be included when',/,
     .			' determining the contaminant (mixture) ',
     .			'molecular weight for the model.',/,
     .			' If any air is present in the released ',
     .			'material which cannot be attributed',/,
     .			' to atmospheric mixing processes modeled ',
     .			'by DEGADIS, the release is',/,
     .			' considered "diluted.")',//,
     .			' In the model, the contaminant is treated ',
     .			'as a single species.  If a mixture',/,
     .			' is released, the released mixture is treated ',
     .			'as the contaminant.  If only one',/,
     .			' component of the released mixture is ',
     .			'hazardous, the mole fraction of the',/,
     .			' hazardous component can be determined from ',
     .			'the mole fraction of the released',/,
     .			' contaminant in the model output by ',
     .			'multiplying the mole fraction of the',/,
     .			' released contaminant by the initial mole ',
     .			'fraction of the hazardous component',/,
     .			' in the released contaminant.')
	    write(6,6701)
 6701	    format(/,' Because the contaminant molecular weight ',
     .			'converts between mass and mole',/,
     .			' fraction, "pseudo" molecular weights should ',
     .			'not be used such as are used',/,
     .			' at times to represent aerosol mixtures, ',
     .			'for example.  In this model, aerosols',/,
     .			' should be modeled using the "isothermal" ',
     .			'option.  See the HELP under the',/,
     .			' "isothermal" question for further ',
     .			'information about modeling aerosols.')
	    goto 700
	endif

c----------------------------------------------------------------------- TEMjet

  710	iq = iquery('Enter the contaminant release temperature (K)',
     .		temjet,temjet)
	if(iq .lt. 0) then
	    goto 700
	else if(iq .gt. 0) then
	    write(6,6000)
	    write(6,6710)
 6710	    format(/,' The contaminant release temperature should ',
     .			'represent the temperature of the',/,
     .			' contaminant as it enters the atmosphere.',//,
     .			' For a boiling liquid pool, the contaminant ',
     .			'release temperature is the',/,
     .			' normal boiling point.',//,
     .			' For a pressurized liquid release, the ',
     .			'contaminant release temperature is',/,
     .			' the normal boiling point.  (The release ',
     .			'temperature should reflect the',/,
     .			' contaminant"s condition after depressurizing',
     .			' to atmospheric pressure.)',//,
     .			' For an "isothermal" simulation, the ',
     .			'contaminant release temperature is',/,
     .			' not used.')
	    goto 710
	endif


c----------------------------------------------------------------------- GAScpk

  730	iq = iquery(
     .		'Enter the contaminant heat capacity constant - GASCPK',
     .		gascpk,gascpk)
	if(iq .lt. 0) then
	    goto 710
	else if(iq .gt. 0) then
	    write(6,6000)
	    write(6,6730)
 6730	format(/,' For the heat capacity specification, a temperature ',
     .		'dependent heat capacity',/,' can be entered using ',
     .		'the correlation coded into DEGADIS.  If a constant ',
     .		'heat',/,' capacity is adequate, set GASCPK equal to ',
     .		'the heat capacity (in J/kg/K) and',/,' GASCPP equal ',
     .		'to zero; JETINT will recalculate the values as used ',
     .		'in the model.',/)
	    goto 730
	endif

c----------------------------------------------------------------------- GAScpp

  740	iq = iquery(
     .		'Enter the contaminant heat capacity power - GASCPP',
     .		gascpp,gascpp)
	if(iq .lt. 0) then
	    goto 730
	else if(iq .gt. 0) then
	    write(6,6000)
	    write(6,6730)
	    goto 740
	endif

c----------------------------------------------------------------------- GASulc

  750	iq = iquery(
     .		'Enter upper limit of concern as a mole fraction (ULC)',
     .		gasulc,gasulc)
	if(iq .lt. 0) then
	    goto 740
	else if(iq .gt. 0) then
	    write(6,6000)
	    write(6,6750)
 6750	    format(/,' The upper and lower levels of concern ',
     .			'(as mole fractions) can represent',/,
     .			' such levels of concern as the lower ',
     .			'flammability limit (LFL) and LFL/2 or',/,
     .			' the short term exposure limit (STEL) ',
     .			'and STEL/2.',//,
     .			' The upper and lower levels of concern ',
     .			'are used to calculate isopleths',/,
     .			' shown in the model output.  The lower ',
     .			'level of concern (LLC) is used to',/,
     .			' determine the default lowest mole fraction ',
     .			'of interest.')
	    goto 750
	endif

	if(gasllc .gt. gasulc) gasllc = gasulc/2.D0

c----------------------------------------------------------------------- GASllc

  760	iq = iquery(
     .		'Enter lower limit of concern as a mole fraction (LLC)',
     .		gasllc,gasllc)
	if(iq .lt. 0) then
	    goto 750
	else if(iq .gt. 0) then
	    write(6,6000)
	    write(6,6750)
	    goto 760
	endif
c
c... Check to see that GASULC > GASLLC
c
	if(gasllc .gt. gasulc) then
	    write(6,3760)
 3760	    format(' The lower limit of concern must be less than ',
     .		'the upper limit of concern.')
	    goto 760
	endif

c----------------------------------------------------------------------- GASzzc

  770	iq = iquery(
     .		'Enter the elevation for contour (receptor) output (m)',
     .		gaszzc,gaszzc)
	if(iq .lt. 0) then
	    goto 760
	else if(iq .gt. 0) then
	    write(6,6000)
	    write(6,6770)
 6770	    format(/,' The elevation for the contour (receptor) ',
     .			'calculations is used along with',/,
     .			' the upper and lower levels of concern to ',
     .			'calculate the isopleths shown in the',/,
     .			' model output.')
	    goto 770
	endif

c---------------------------------------------------- handle "isothermal" cases

	if(nden .eq. 0) goto 829
  780	write(6,3780)
 3780	format(/,' For "isothermal" cases, there are two ',
     .			'options for entering the concentration/',/,
     .			' density relationship:',//,
     .		' (1) NDEN = -1; The simulation treats the ',
     .			'contaminant as if it were an ideal',/,
     .		'                gas with a constant molal ',
     .			'heat capacity equal to that of air.',/,
     .		'                Water condensation is ignored.',//,
     .		' (2) NDEN > 0;  In this case, NDEN is the ',
     .			'number of triples which',/,
     .		'                are used to specify the ',
     .			'contaminant concentration and mixture',/,
     .		'                density as functions of ',
     .			'the contaminant mole fraction (based',/,
     .		'                on adiabatic mixing',
     .			' between the released contaminant and',/,
     .		'                ambient air).',//)

  785	if(nden .eq. -1) then
	    write(6,3785)
 3785	    format(/,' Do you want to use option (1) ',
     .			'or (2)? [1,2,/] <1> ',$)
	else
	    write(6,3787)
 3787	    format(/,' Do you want to use option (1) ',
     .			'or (2)? [1,2,/] <2> ',$)
	endif
	read(5,3102) string
	nchar = index(string,' ')

	if(string.eq.'/') then
	    goto 770
	else if(string.eq.'1' .or. nchar.eq.1.and.nden.eq.-1) then
	    nden = -1
	    goto 870
	else if(string.eq.'2' .or. nchar.eq.1.and.nden.gt.0) then
	    goto 790
	else
	    goto 780
	endif

c
c... get the density curve if isothermal
c
  790	write(6,3790) rhoa
 3790	FORMAT(/,' The density is determined as a function of ',
     .		'concentration by a listing ',/,' of ordered ',
     .		'triples supplied by the user.  Use the following form:',
     .		//,'     first point    -- pure air  y=0.0,Cc=0.,',
     .		'RHOG=RHOA=',F7.5,' kg/m**3',/,3(15X,'.',/),'     ',
     .		'last point     -- pure gas  y=1.0,Cc=RHOE,RHOG=RHOE',//,
     .		' Do you have an input file for the density function? ',
     .		'[y,n,?,/] <n>',$)
	read(5,3102) string
	nchar = index(string,' ')

	if(string.eq.'N' .or. string.eq.'n' .or. nchar.eq.1) then
	    lunin = 5
	    rrr = numden
  792	    iq = iquery('Enter the number of triples',rrr,rrr)
	    numden = nint(rrr)
	    if(iq .lt. 0) then
		goto 790
	    else if(iq .gt. 0) then
		write(6,6000)
		write(6,6792)
 6792		format(/,' The number of triples is the number of ',
     .				'ordered triples of:',//,
     .			'   (contaminant mole fraction, contaminant ',
     .			'concentration, mixture density)',/)
		goto 792
	    endif
	    goto 800
	else if(string.eq.'/') then
	    goto 780
	else if(string.eq.'?') then
	    write(6,6000)
	    write(6,6790)
 6790	    format(/,' An input file for the density specification ',
     .			'allows you to create a file with',/,
     .			' another program for entry into DEGADIS.  ',
     .				'The entries in the file are',/,
     .			' identical to the questions which are ',
     .			'answered interactively in this program:',//,
     .			'   number of triples',/,
     .			'   (contaminant mole fraction(=0), contaminant ',
     .			'concentration(=0), mixture density(=RHOA))',/,
     .			'   (contaminant mole fraction, contaminant ',
     .			'concentration, mixture density)',/,
     .			3('      .',/),
     .			'   (contaminant mole fraction, contaminant ',
     .			'concentration, mixture density)',/)
	    goto 790
	else if(string.eq.'Y' .or. string.eq.'y') then
	    lunin = 10
	    write(6,3793)
 3793	    format(' Enter the file name and extension: ',$)
	    read(5,3102) string
	    OPEN(UNIT=lunin, file=STRING, status='OLD', err=794)
	    goto 796

  794	    write(6,3794)
 3794	    format(' This file cannot be opened.')
	    goto 790

  796	    continue
	    read(lunin,*,err=798) numden
	    goto 800
	else
	    goto 790
	endif
c
c... Can't read from the file.
c
  798	write(6,3798)
 3798	format(' Troubles reading the file.')
	goto 790
c
c... Read the triples---------------------------------------------- DEN triples
c
  800	continue
	if(numden+1.gt.igen) then
	    write(6,3800) igen-1
 3800	    format(' A maximum of ',i3,' points are allowed in the ',
     .			'density specification.')
	    goto 790
	endif
	if(lunin .eq. 5) write(6,3802)
 3802	FORMAT(/,' Enter as triples:',/,' Mole fraction, Contaminant ',
     .		'concentration (kg/m**3), Mixture density (kg/m**3)')

	den(3,1)    = rhoa
	den(1,numden) = 1.D0
	den(2,numden) = gasrho
	den(3,numden) = gasrho

	nden = numden
	DO 820 iii=1,Numden
	den(4,iii) = 0.D0			! 0.0 by default for isotherm
	den(5,iii) = tamb			! tamb for isotherm
	if(lunin.eq.5) then
	    write(6,3810) iii
 3810	    format(/,' For point number ',i2,' enter the following...')
  810	    iq = iquery('Enter the contaminant mole fraction',
     .		den(1,iii),den(1,iii))
	    if(iq .lt. 0) then
		goto 790
	    else if(iq .gt. 0) then
		write(6,6000)
		write(6,6810)
 6810		format(/,' The contaminant mole fraction is the ',
     .			'first element in each ordered triple.',/,
     .			' The triples must start with pure air ',
     .			'(contaminant mole fraction = 0.) and',/,
     .			' increase to the last point with pure ',
     .			'released contaminant.')
		goto 810
	    endif
	    if(iii.ne.1) then
		if(den(1,iii) .le. den(1,iii-1)) then
		    write(6,3811)
 3811		    format(/,' The contaminant mole fraction must ',
     .			'be entered as a monotonically increasing',/,
     .			' function starting with pure air (i.e., ',
     .			'contaminant mole fraction =0.0).')
		    goto 810
		endif
	    endif

  813	    iq = iquery('Enter the contaminant concentration (kg/m**3)',
     .		den(2,iii),den(2,iii))
	    if(iq .lt. 0) then
		goto 810
	    else if(iq .gt. 0) then
		write(6,6000)
		write(6,6813)
 6813		format(/,' The contaminant concentration (in ',
     .			'kg/m**3) entered here should correspond',/,
     .			' with the contaminant mole fraction ',
     .			'just entered.',//,
     .			' For a contaminant mole fraction of zero, ',
     .			'the contaminant concentration',/,
     .			' must also be zero.',//,
     .			' For a contaminant mole fraction of one, ',
     .			'the contaminant concentration',/,
     .			' must also be equal to the pure ',
     .			'contaminant density.')
		goto 813
	    endif

  816	    iq = iquery('Enter the mixture density (kg/m**3)',
     .		den(3,iii),den(3,iii))
	    if(iq .lt. 0) then
		goto 813
	    else if(iq .gt. 0) then
		write(6,6000)
		write(6,6816)
 6816		format(/,' The mixture density (in kg/m**3) ',
     .			'entered here should correspond with the',/,
     .			' contaminant mole fraction and ',
     .			'contaminant concentration just entered.',//,
     .			' For a contaminant mole fraction of zero, ',
     .			'the mixture density must be the',/,
     .			' ambient air density.',//,
     .			' For a contaminant mole fraction of one, ',
     .			'the mixture density must be equal',/,
     .			' to the pure contaminant density.')
		goto 816
	    endif
	else
	    READ(LUNIN,*,err=798) DEN(1,iii), DEN(2,iii), DEN(3,iii)
	endif
	if(iii.eq.1 .and. (den(3,1)/rhoa.gt.1.005 .or. 
     .				rhoa/den(3,1).gt.1.005)) then
	    den(3,1) = rhoa
	    write(6,3804) rhoa
 3804	    format(/,' Air density corrected to ',1pg13.5,' kg/m**3',/)
	endif
  820	CONTINUE
	IF(LUNIN .EQ. 10) CLOSE(UNIT=10)

c
c/////////////////////////////////////////////////////////////////////////
c                           Release description
c\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
c
c
c--------------------------------------------------------- Steady or transient?

  829	f830 = .false.
  870	write(6,3870) cdilut(itran+1)
 3870	format(/,' Can this release be considered:',//,'   (S) steady ',
     .		'state or',/,'   (T) transient?',//,' [S,T,/,?] <',
     .		a1,'> ',$)
	read(5,3102) string
	nchar = index(string,' ')
	if(nchar.eq.1) string(1:1) = cdilut(itran+1)

	if(string.eq.'S' .or. string.eq.'s') then
	    itran = 0
	    check4 = .true.
	    goto 880
	else if(string.eq.'T' .or. string.eq.'t') then
	    itran = 1
	    check4 = .false.
	    goto 880
	else if(string.eq.'/') then
	    if(f830) goto 670
	    if(nden.eq.0) goto 770
	    goto 780
	else if(string.eq.'?') then
	    write(6,6000)
	    write(6,6870)
 6870	    format(/,' In a transient release, the source ',
     .			'characteristics vary with time; in a',/,
     .			' steady state release, the source ',
     .			'characteristics do not vary with time.  A',/,
     .			' release is also considered transient if ',
     .				'the source characteristics do',/,
     .			' not vary with time but the duration of ',
     .				'the source is limited.',/)
	    write(6,3104)
	    read(5,3102) string
	    write(6,6871)
 6871	    format(/,' There are three time scales which help ',
     .			'determine whether a release is modeled',/,
     .			' as steady state or transient:')
	    write(6,6872)
 6872	    format(/,'   "trel" is the duration of the ',
     .			'(secondary) source,',/,
     .			'   "thaz" is the averaging time ',
     .			'associated with a given contaminant',/,
     .			'           concentration for hazard ',
     .			'assessment purposes, and',/,
     .			'   "ttrav" is the travel time (based on ',
     .			'the mean advection speed of the',/,
     .			'           contaminant) to (1) the lowest ',
     .			'concentration of interest OR',/,
     .			'           (2) the farthest downwind ',
     .			'receptor.',//,
     .			' A release is considered steady state ',
     .			'when "trel" >> "thaz" and "ttrav";',/,
     .			' "tdy" should be set to "thaz".',//,
     .			' A release is considered transient when ',
     .			'it cannot be considered steady state.',/,
     .			' "tdy" should be set to the smaller of ',
     .			'"trel" or "thaz" (to properly represent',/,
     .			' the lateral plume meander).  When "trel" ',
     .			'< "thaz", the concentration time',/,
     .			' history at a point (or receptor) available ',
     .			'from DEG4 should be averaged over',/,
     .			' "thaz" for consequence analysis.')
	    goto 870
	endif

c--------------------------------------------------------------  Erate

  880	iq = iquery(
     .		'Enter the release rate (kg contaminant/s)',erate,erate)
	if(iq .lt. 0) then
	    goto 870
	else if(iq .gt. 0) then
	    write(6,6000)
	    write(6,6880)
 6880	    format(/,' The evolution (release) rate ',
     .			'is the rate contaminant (without',/,
     .			' air) is released to the atmosphere ',
     .			'(in kg contaminant/s).')
	    goto 880
	endif
	if(erate.eq.0.D0) goto 880

c--------------------------------------------------------------  Diajet

  890	iq = iquery('Enter the source diameter (m)',diajet,diajet)
	if(iq .lt. 0) then
	    goto 880
	else if(iq .gt. 0) then
	    write(6,6000)
	    write(6,6890)
 6890	    format(/,' The source diameter ',
     .			'represents the area through which',/,
     .			' the evolution rate passes.')
	    goto 890
	endif
	if(diajet.eq.0.D0) goto 890

c--------------------------------------------------------------  elejet

  900	iq = iquery('Enter the source elevation (m)',elejet,elejet)
	if(iq .lt. 0) then
	    goto 890
	else if(iq .gt. 0) then
	    write(6,6000)
	    write(6,6900)
 6900	    format(/,' The source elevation ',
     .			'is the release height (m).')
	    goto 900
	endif
	if(elejet.lt.0.D0) goto 900

c
	tend = 0.D0
	if(check4) goto 980
c
c... For a transient release
c
c--------------------------------------------------------------  TEND
  930	continue

	iq = iquery(
     .		'Source duration (s)',tend,tend)
	if(iq .lt. 0) then
	    goto 900
	else if(iq .gt. 0) then
	    write(6,6000)
	    write(6,6930)
 6930	    format(/,' The source duration is the ',
     .			'duration of the primary release (s).')
	    goto 930
	endif
	if(tend.le.0.D0) then
	    write(6,6933)
 6933	    format(/,' The source duration must be ',
     .		'specified for a positive time for a transient',/,
     .		' release.  (A source duration of zero is what ',
     .		'the code uses to signal a',/,' steady-state release.)')
	    goto 930
	endif
c
c/////////////////////////////////////////////////////////////////////////
c                         Write the input file
c\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
c
  980	write(6,3980)
 3980	format(/,' Are you satisfied with your answers? [y,n,/,?] <y> '
     .			,$)
	read(5,3102) string
	nchar = index(string,' ')

	if(string.eq.'Y' .or. string.eq.'y' .or. nchar.eq.1) then
	    goto 1000
	else if(string.eq.'N' .or. string.eq.'n') then
	    write(6,3982)
 3982	    format(' Do you wish to exit without writing a file? ',
     .			'[y,n] <n> ',$)
	    read(5,3102) string
	    if(string.eq.'Y' .or. string.eq.'y') stop
	    goto 980
	else if(string.eq.'/') then
	    if(check4) goto 900
	    goto 930
	else if(string.eq.'?') then
	    write(6,6000)
	    write(6,6980)
 6980	    format(/,' If you are satisfied with your answers, ',
     .				'JETINT will now write an input',/,
     .			' (IN) file based on your responses.',//,
     .			' If you are not satisfied with your ',
     .			'answers, you can either back up through',/,
     .			' the questions using "/" to change your ',
     .			'previous responses, OR you can exit',/,
     .			' the program at this point without ',
     .			'creating an input (IN) file.')
	    goto 980
	else
	    goto 980
	endif
c
c... OPEN the new IN file...................................................
c
 1000	continue
	opnf = runnam(1:nrun) // in(1:3)
	OPEN(UNIT=8, file=OPNf, status='UNKNOWN', err=1002)
	goto 1004
c
c... handle the error from the OPEN ........................................
c
 1002	continue
	write(6,4002) (opnf1(iii),iii=1,nrun+4)
 4002	format(/,' The specified file cannot be opened: ',60a1)
	WRITE(6,3100)
	READ(5,3102) opnf
	nchar = index(opnf,'/')
	if(nchar .eq. 1) goto 10
	nchar = index(opnf,' ')
	if(nchar .eq. 1) goto 10
	nchar = nchar-1
	opnf = opnf(1:nchar) // in(1:3)
	nrun = nchar
	runnam(1:nrun) = opnf(1:nchar)
	goto 1000
c
c... write the new IN file...................................................
c
 1004	continue
	do 1005 iii=1,4
 1005	WRITE(8,'(a80)') TITLE(iii)

	string = 'u0 (m/s) at z0 (m)'
	WRITE(8,1032) U0,Z0,string
	string = 'zr - surface roughness (m)'
	write(8,1031) ZR,string

	string = 'indvel, istab, and Monin-Obukhov length'
	WRITE(8,1035) indvel,istab, rml, string
	string = 'ambient temp, press, humidity'
	write(8,1033) tamb,pamb,relhum,string
	string = 'ground surface temperature'
	write(8,1031) tsurf,string
	write(8,'(a3)') gasnam
	string = 'Contaminant molecular weight'
	WRITE(8,1031) gasmw,string
	string = 'Averaging time (s)'
	write(8,1031) avtime,string
	string = 'TEMJET (K)'
	WRITE(8,1031) temjet,string
	string = 'gasulc, gasllc, gaszzc'
	WRITE(8,1033) gasulc, gasllc, gaszzc,string
	string = 'indht, gascpk, gascpp'
	write(8,1036) indht, gascpk, gascpp,string
	string = 'nden'
	if(nden.gt.0) then
	    write(8,1037) numden,string
	    do 1010 i=1,numden
 1010	    WRITE(8,*) DEN(1,I),DEN(2,I),DEN(3,I)
	else
	    write(8,1037) nden,string
	endif
	string = 'Erate (kg/s)'
	WRITE(8,1031) erate,string
	string = 'Jet elevation and diameter'
	write(8,1032) elejet, diajet, string
	string = 'Tend (s)'
	WRITE(8,1031) tend,string
	string = 'Maximum distance between outputs (m)'
	distmx = 50.D0
	write(8,1031) distmx,string

	CLOSE(UNIT=8)
C
 1030	format(1x,5(1pg14.7,1x),1pg14.7)
 1031	format(1x,1pg14.7,t47,a32)
 1032	format(1x,2(1pg14.7,1x),t47,a32)
 1033	format(1x,3(1pg14.7,1x),t47,a32)
 1035	format(1x,i2,1x,i2,1x,1pg14.7,t47,a32)
 1036	format(1x,i2,1x,2(1pg14.7,1x),t47,a32)
 1037	format(1x,i2,t47,a32)
c
c... exit menu
c
 1100	write(6,4100)
 4100	format(/,' ... Exit menu ...',//,'   (A) work on another ',
     .		'IN file or',/,'   (E) exit',/,' [a,e] <e> ',$)
	read(6,3102) string
	nchar = index(string,' ')

	if(string.eq.'A' .or. string.eq.'a') then
	    again = .true.
	    goto 10
	else if(string.eq.'E' .or. string.eq.'e' .or. nchar.eq.1) then
	    stop
	else
	    goto 1100
	endif

	END
