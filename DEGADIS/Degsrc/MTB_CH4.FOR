         program mtb_ch4
c
c...............................................................
c
c	Program to calculate the Gaussian Line Source (with 10 min
c	averaging times - MTB) dispersion results for a given set
c	of inputs for methane.
c
c...............................................................
c
	implicit real*8(a-h,o-z), integer*4(i-n)

	common srcrte,wspd,slen,pa,ta,dy,by,dz,bz,gz,cc,sy,sz
C
c... local character variables
c
	character*100 OPNf, string
	character*1 OPNf1(100)
	equivalence (opnf1(1),opnf)

	character*1 stabil(6), stab

	logical file
C
	data stabil/'A','B','C','D','E','F'/
c
c
c... Write the program identifier and determine the source of input
c
 100	write(5,2000)
 2000	format(//,' MTB_CH4 - Gaussian Line Source Model for Methane',
     .		' with 10 min averaging times',/
     .	/,' Data can be input from a file (F) or interactively (I).',/,
     .	' Enter either F, I, or E for EXIT: ',$)

	READ(5,3102,err=100) opnf
 3102	FORMAT(A100)
	call upcase(opnf)

	nchar = index(opnf,'E')
	if(nchar .eq. 1) stop
	nchar = index(opnf,'F')
	if(nchar .eq. 1) goto 200
	nchar = index(opnf,'I')
	if(nchar .eq. 1) goto 300
	goto 100
c
c... Query the file name for the input (and output)
c
 200	continue
	file = .true.
	WRITE(5,3100)
 3100	FORMAT(//,' Rules for file entry...',//,
     .	   ' 1) The following information is required on each line:',/,
     .		'     - Pasquill stability class (A,B,C,D,E,F)',/,
     .		'     - LNG mass release rate (kg/s)',/,
     .	  '     - LNG flux (kg/m**2/s); typically 0.085 kg/m**2/s',/,
     .		'     - Windspeed (m/s)',/,
     .		'     - Ambient pressure (atm)',/,
     .		'     - Ambient temperature (K)',/,
     .		'    Note that this is the order for the ',
     .		'information as well.',/,
     .		' 2) Spacing on any line is unimportant ',
     .		'except that the first three characters',/,
     .		'    in every line must be the Pasquill stability',
     .		' enclosed in single quotes.',/,
     .		' 3) The file name of the input file must ',
     .		'have extension CH4.  The output',/,
     .		'    will have the extension LIS.',//)

	write(5,3110)
 3110	format(/,' Enter the input simulation name as a proper ',
     .			'file name: ',$)
	READ(5,3102,err=100) opnf

	nchar = index(opnf,'/')
	if(nchar .eq. 1) goto 100
	nchar = index(opnf,' ')
	if(nchar .eq. 1) goto 100
	nchar = nchar-1
c
c... open the input and output files
c
	opnf = opnf(1:nchar) // '.CH4'
	open(unit=11, file=opnf,err=100)
	opnf = opnf(1:nchar) // '.LIS'
	open(unit=12, file=opnf)
	opnf = opnf(1:nchar) // '.CH4'

	write(12,3200) (opnf1(iii),iii=1,nchar+4)
 3200	format(//,' MTB_CH4 - Gaussian Line Source Model for Methane',
     .		' with 10 min averaging times.',/
     .		//,'  Input from: ',40A1)
	write(12,3202)
 3202	format(///,
     .		11x,' Source ',12x,10x,'Ambient ','   Ambient   ',
     .		'Distance ','Distance ',/,
     .		' Stability ','  Rate  ',4x,'Flux',4x,'Windspeed ',
     .		'Pressure',' Temperature ',
     .		2x,'to 5%',2x,' to 2.5% ',/,
     .		11x,' (kg/s) ','(kg/m**2/s) ',2x,'(m/s)',3x,
     .		' (atm)  ',5x,'(K)',5x,2(3x,'(m)',4x))
 3210	format(5x,a1,5x,1pg8.2,1x,1pg10.4,1x,
     .		1pg10.4,1pg8.2,3x,0pf6.2,4x,2(1pg9.3,1x))
c
c... read and process each line
c
 210	continue
c	read(11,3102,end=230,err=230) opnf
c	stab = opnf(1:1)
c	read( opnf(2:100), *, err=230) srcrte, flux,wspd, pa, ta
	read(11,*,end=230,err=230) stab, srcrte, flux, wspd, pa, ta
	slen = sqrt(srcrte/flux)
	call mtbch4(stab, d50, d25, file)
	write(12,3210) stab, srcrte, flux, wspd, pa, ta, d50, d25
	goto 210
c
c... exit the proceedings for this file.
c
 230	continue
	close(unit=11)
	close(unit=12)
	stop
c
c... Interactive session...............................................
c	Set some defaults first.
c
 300	continue
	file = .false.
	istab  = 4
	srcrte = 1.D0
	wspd   = 7.0D0
	flux   = 0.085D0
	pa     = 1.0D0
	ta     = 273.15
	write(5,*) ' '
	write(5,*) 'To go to the previous question, enter a "/"'
	write(5,*) 'To get help, enter a "?"'
	write(5,*) ' '
c
c... stability and averaging time for DELTAY and derived parameters
c
c------------------------------------------------------------------------- istab

  400	WRITE(5,3440) stabil(istab)
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
	    goto 100
	else if(string.eq.'?') then
	    write(6,6000)
	    write(6,6440)
 6440	    format(/,' The Pasquill-Gifford stability class is ',
     .				'used to estimate:',//,
     .			'     y- and z-direction dispersion parameters')
	    goto 400
	else if(nchar.eq.1) then
	    goto 450
	else
	    goto 400
	endif

c--------------------------------------------------------------  Ess

  450	iq = iquery('Enter the release rate (kg contaminant/s)',
     .		srcrte,srcrte)
	if(iq .lt. 0) then
	    goto 400
	else if(iq .gt. 0) then
	    write(6,6000)
	    write(6,6880)
 6880	    format(/,' The steady-state evolution (release) rate ',
     .			'is the rate contaminant (without',/,
     .			' air) is released to the atmosphere ',
     .			'(in kg contaminant/s).')
	    goto 450
	endif
	if(srcrte.le.0.D0) goto 450

c--------------------------------------------------------------  Flux

  460	iq = iquery(
     .	  'Enter the release flux rate (kg contaminant/m**2/s)',
     .		flux,flux)
	if(iq .lt. 0) then
	    goto 450
	else if(iq .gt. 0) then
	    write(6,6000)
	    write(6,6881)
 6881	    format(/,' The flux rate ',
     .			'is the rate contaminant ',/,
     .			' is released to the atmosphere ',
     .			'per unit area.',/,
     .			' The flux rate is used to estimate ',
     .			'the source length.')
	    goto 460
	endif
	slen = sqrt(srcrte/flux)

c--------------------------------------------------------------  slen

  470	iq = iquery('Enter the (total) source width (m)',slen,slen)
	if(iq .lt. 0) then
	    goto 460
	else if(iq .gt. 0) then
	    write(6,6000)
	    write(6,6882)
 6882	    format(/,' The source width is the lateral ',
     .		'length scale used in the MTB model.',//,
     .		' The default source width is determined from ',
     .		'the flux just entered by',/,
     .		' assuming the source is a square.')
	    goto 470
	endif

c------------------------------------------------------------------------ wspd

  480	iq = iquery('Enter the atmospheric windspeed (m/s)',wspd,wspd)
	if(iq .lt. 0) then
	    goto 470
	else if(iq .gt. 0) then
	    write(6,6000)
 6000		format(/,1x,35('.'),3x,'help',3x,35('.'))
	    write(6,6410)
 6410	    format(/,' The windspeed must be specified at an ',
     .			'elevation typically between 2 and 30 m.')
	    goto 480
	endif

c
c------------------------------------------------------------------------  ta

  490	iq = iquery('Enter the ambient air temperature (K)',ta,ta)
	if(iq .lt. 0) then
	    goto 480
	else if(iq .gt. 0) then
	    write(6,6000)
	    write(6,6530)
 6530	    format(/,' The ambient air temperature should reflect ',
     .		'the conditions present during',/,' the release.')
	    goto 490
	endif

c------------------------------------------------------------------------  pa

  500	iq = iquery('Enter the ambient pressure (atm)',pa,pa)
	if(iq .lt. 0) then
	    goto 490
	else if(iq .gt. 0) then
	    write(6,6000)
	    write(6,6540)
 6540	    format(/,' The ambient air pressure should reflect ',
     .		'the conditions present during',/,' the release.')
	    goto 500
	endif

	if(pa.lt.0.7D0 .or. pa.gt.1.1D0) write(6,6545) pa
 6545	    format(/,' NOTICE: The specified ambient pressure of ',
     .		1pg13.5,' atm appears to',/,' be unrealistic.',//)

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
 3982	    format(' Do you wish to exit without continuing? ',
     .			'[y,n] <n> ',$)
	    read(5,3102) string
	    if(string.eq.'Y' .or. string.eq.'y') stop
	    goto 980
	else if(string.eq.'/') then
	    goto 500
	else if(string.eq.'?') then
	    write(6,6000)
	    write(6,6980)
 6980	    format(/,' If you are not satisfied with your ',
     .			'answers, you can either back up through',/,
     .			' the questions using "/" to change your ',
     .			'previous responses, OR you can exit',/,
     .			' the program at this point.')
	    goto 980
	else
	    goto 980
	endif
c
c... do the calculations for this point
c
 1000	continue
	write(5,*) 'The Pasquill stability is ',stabil(istab)
	write(5,*) 'The Mass Release Rate   = ',srcrte,' kg/s'
	write(5,*) 'The Wind Speed          = ',wspd,'  m/s'
	write(5,*) 'The Initial Length      = ',slen,'  m'
	write(5,*) 'The Ambient Pressure    = ',pa,' atm'
	write(5,*) 'The Ambient Temperature = ',ta,' K'

	call mtbch4(stabil(istab), d50, d25, file)

	goto 400

	end
c
c...........................................................
c
	subroutine mtbch4(stab, d50, d25, file)
c
c............................................................
c
c	subroutine to do the Gaussian Line Source calculations
c	for methane
c
c.............................................................
c
	implicit real*8(a-h,o-z), integer*4(i-n)

	common srcrte,wspd,slen,pa,ta,dy,by,dz,bz,gz,cc,sy,sz

	external conc

	dimension dely(6), delz(6), betz(6), gamz(6)

	logical file

	character*1 stabil(6), stab
C
	data stabil/'A','B','C','D','E','F'/
c
c... set the Sigma y and Sigma z coefficients for 10 minute averaging
c	times.
c
	data dely/.423D0, .313D0, .210D0, .136D0, .102D0, .0674D0/
	data delz/107.7D0, .1355D0, .09623D0, .04134D0,
     .			.02275D0, .01122D0/
	data betz/-1.7172D0, .8752D0, .9744D0, 1.1737D0,
     .			1.3010D0, 1.4024D0/
	data gamz/.2770D0, .0136D0, -.0020D0, -.0316D0,
     .			-.0450D0, -.0540D0/
c
c... tol is the tolerance in distance (m)
c
	data tol/0.5D0/
c
c
c... ensure STAB is capitalized; then determine ISTAB
c
	call upcase(stab)
c
 100	do 110 istab=1,6
 110	if(stabil(istab) .eq. stab) goto 120
	stop 'MTBCH4.  Stability not recognized; use A,B,C,D,E,or F.'

 120	continue
	dy = dely(istab)
	by = 0.9D0
	dz = delz(istab)
	bz = betz(istab)
	gz = gamz(istab)
c
c... set up the call to limit and zbrent for 5%
c
	cc = 0.05
	xinit = 501.d0
	xincre = 500.d0
	chigh = 100000.d0
	clow = 1.d0

	call limit(conc,xinit,xincre,chigh,clow)
	call zbrent(d50,conc,clow,chigh,tol, ierr)
	if(ierr.ne.0) stop 'ZBRENT return error'
	write(5,*) ' '
	write(5,*) 'Distance to 5%          = ',d50,' m'
	write(5,*) 'Sigma y                 = ',sy,' m'
	write(5,*) 'Sigma z                 = ',sz,' m'

c
c... set up the call to limit and zbrent for 2.5%
c
	cc = 0.025
	xinit = d50 + 99.D0
	xincre = 500.d0
	chigh = 100000.d0
	clow = max(d50 - 1.D0, 1.D0)

	call limit(conc,xinit,xincre,chigh,clow)
	call zbrent(d25,conc,clow,chigh,tol, ierr)
	if(ierr.ne.0) stop 'ZBRENT return error'

	write(5,*) ' '
	write(5,*) 'Distance to 2.5%        = ',d25,' m'
	write(5,*) 'Sigma y                 = ',sy,' m'
	write(5,*) 'Sigma z                 = ',sz,' m'

c
c... set up a loop to output concentration as a function of distance
c
	if(file) return
	cc = 0.D0
 200	write(5,*) ' '
	write(5,'('' Enter the distance for a given concentration '',
     .			''(zero to stop): '',$)')
	read(5,*) dist
	if(dist .le. 0.D0) return
	yc = conc(dist)
	write(5,*) '    CH4 mole fraction = ',yc
	write(5,*) '    Sigma y           = ',sy,' m'
	write(5,*) '    Sigma z           = ',sz,' m'
	goto 200

	end
c
c...............................................................
c
	function conc(xx)
c
c...............................................................
c
c	Function to return the concentration (-cc) as a function
c	of distance.
c
c...............................................................
c
	implicit real*8(a-h,o-z), integer*4(i-n)

	common srcrte,wspd,slen,pa,ta,dy,by,dz,bz,gz,cc,sy,sz

	data pi/3.1415926535824D0/
	data rgas/0.08206D0/
	data gasmw/16.D0/
c
c
	sy = dy*xx**by
	sz = dz*xx**bz*exp(gz*(log(xx))**2)
	aaa = erf(slen/2.d0/sqrt(2.d0)/sy)
	ccc = srcrte/wspd/slen*(2.d0/sqrt(2.d0*pi)/sz)*aaa
	yyy = ccc*rgas*ta/pa/gasmw
	conc = yyy - cc
	return
	end
c
c................................................................
c
	function erf(xxx)
c
c................................................................
c
c	Function to return the ERROR FUNCTION from a call to
c	the incomplete Gamma function.  Functional relationship
c	taken from Spanier and Oldham, "An Atlas of Functions,"
c	pg 438, eqn 45:4:4.
c
c..............................................................
c
	implicit Real*8 (a-h, o-z), integer*4 (i-n)

	parameter (sqrtpi=1.772453851D0)
c
	erf = dsign(gaminc(0.5D0, xxx**2)/sqrtpi, xxx)
	return
	end
