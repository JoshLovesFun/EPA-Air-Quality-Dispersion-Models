	PROGRAM alpha_
C***************************************************************************
C***************************************************************************
C***************************************************************************
C
C
C	Program description:
C
c	ALPHA_ calculates the value of alpha from information about the
c	ambient velocity profile using the same routines as DEGADIS.
c
C.........................................................................
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

C
	include 'DEG1.prm'
C
	common
     ./ALP/	ALPHA,alpha1
     ./alpcom/	ialpfl,alpco
     ./ERROR/	STPIN,ERBND,WTRG,WTtm,WTya,wtyc,wteb,wtmb,XLI,
     .		XRI,EPS,ZLOW,STPINZ,ERBNDZ,SRCOER,srcss,srccut,
     .		ERNOBL,NOBLpt,crfger,epsilon
     ./parm/	u0,z0,zr,rml,ustar,vkc,gg,rhoe,rhoa,deltay,betay,gammaf,
     .		yclow
C
c... local character variables
c
	character*100 OPNf
	character*1 OPNf1(100)
	equivalence (opnf1(1),opnf)

	character*80 STRING

	character*1 stabil(6)
	character*1 crml(2)
	character*1 cweight(2)
	data crml/'D','N'/
	data stabil/'A','B','C','D','E','F'/
	data cweight/'w','u'/

c
c
	WRITE(6,3005)
 3005	FORMAT(//,16X,'alpha... calculate alpha as in DEGADIS.',//,
     .	' When default answers are available to a query, the default',/,
     .	' is given in brackets (<>); for such questions, the following',/,
     .	' responses can also be given:',//,
     .	'        /       move to the previous question',/,
     .	'        ?       help',//,
     .	' Other possible answers may be listed in square brackets ([]).')
c
C
   10	continue
 6000	format(/,1x,35('.'),3x,'help',3x,35('.'))

	STPINZ = -.01
	ERBNDZ = 0.005
	ialpfl = 1
	vkc = 0.35
	u0 = 5.0
	z0 = 10.0
	zr = 0.01
	istab = 4
	indvel = 1
c
c/////////////////////////////////////////////////////////////////////////
c                    Enter the atmospheric parameters
c\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
c
c
  400	continue
	write(6,3400)
 3400	format(/,' ',20('-'),' Atmospheric parameters',20('-'),//)

c--------------------------------------------------------------------------- u0

  410	iq = iquery('Enter the atmospheric windspeed (m/s)',u0, u0)
	if(iq .lt. 0) then
	    goto 400
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

  440	zlow = zr
	WRITE(6,3440) stabil(istab)
 3440	FORMAT(/,' Enter the Pasquill stability class: ',
     .			'[A,B,C,D,E,F,/,?] <',a1,'> ',$)
	read(5,3102) string
 3102	format(a40)
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

  450	avtime = 0.D0
c
c... Get the default values for RML
c
	call atmdef(zr, istab, avtime, drml, deltay, betay, 
     .		deltaz, betaz, gammaz, sigxco, sigxp, sigxmd)
c
c... Use the default Monin-Obukhov length or set it?
c
c--------------------------------------------------------------------------- rml

  460	continue
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
	    goto 500
	else if(string.eq.'D' .or. string.eq.'d') then
	    indvel = 1
	    rml = 0.D0
	    goto 500
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
c------------------------------------------------------- set weight for LS
c
  500	continue
	WRITE(6,3500) cweight(ialpfl)
 3500	FORMAT(/,' Do you want the fit to be weighted or unweighted?',
     .			'[w,u,/,?] <',a1,'> ',$)
	read(5,3102) string
	nchar = index(string,' ')

	IF(STRING.eq.'W' .or. string.eq.'w') then
	    ialpfl=1
	else IF(STRING.eq.'U' .or. string.eq.'u') then
	    ialpfl=2
	else if(string.eq.'/') then
	    goto 460
	else if(string.eq.'?') then
	    write(6,6000)
	    write(6,6500)
 6500	    format(/,' The least-squares fit can be unweighted ',
     .				'or weighted with',//,
     .			'     1/(1+z).',/)
	    goto 500
	else if(nchar.eq.1) then
	    goto 530
	else
	    goto 500
	endif

c------------------------------------------------------- calculate alpha

  530	continue
	xli = 0.05
	xri = 0.50
	eps = 0.00001
	call alph
c
c
	write(6,*) 'The value of alpha for these conditions is: ',alpha

c
c... exit menu
c
 1100	write(6,4100)
 4100	format(/,' ... Exit menu ...',//,'   (A) work on another ',
     .		'set of conditions or',/,'   (E) exit',/,' [a,e] <e> ',$)
	read(6,3102) string
	nchar = index(string,' ')

	if(string.eq.'A' .or. string.eq.'a') then
	    goto 400
	else if(string.eq.'E' .or. string.eq.'e' .or. nchar.eq.1) then
	    stop
	else
	    goto 400
	endif

	END
