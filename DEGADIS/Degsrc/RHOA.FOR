	PROGRAM RHOa_
C***************************************************************************
C***************************************************************************
C***************************************************************************
C
C
C	Program description:
C
c	RHOA calculates the relative humidity and ambient density from
c	ambient temperature and relative humidity.
c
C.........................................................................
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

C
	include 'degin.prm'
c
	common
     ./cgprop/	gasmw,gastem,gasrho,gascpk,gascpp,gasulc,gasllc,gaszzc,
     .		gasnam
     ./GEN2/	DEN(5,igen)
     ./NEND/	POUND
     ./parm/	u0,z0,zr,rml,ustar,vkc,gg,rhoe,rhoa,deltay,betay,gammaf,
     .		yclow
c
	character*3 gasnam
C
c... local character variables
c
	character*100 OPNf
	character*1 OPNf1(100)
	equivalence (opnf1(1),opnf)

	character*80 STRING

	DATA POUND/-1.E-20/
C
	data DEN/igen*0.,igen*0.,igen*0.,igen*0.,igen*0./

c
c
	WRITE(6,3005)
 3005	FORMAT(//,16X,'RHOA... calculate air density.',//,
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

	tamb = 293.15D0
	pamb = 1.D0
	relhum = 70.D0
	vp = watvp(tamb)
	sat = wmw/wma * vp/(pamb - vp)
	humid = relhum/100.D0 * sat
c
c/////////////////////////////////////////////////////////////////////////
c                    Enter the atmospheric parameters
c\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
c
  400	continue

c
c------------------------------------------------------------------------  tamb

  530	iq = iquery('Enter the ambient air temperature (K)',tamb,tamb)
	if(iq .lt. 0) then
	    goto 400
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
 3102	format(a80)

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


  560	rhoa = pamb*(1.D0+humid)*wmw/(rgas*(wmw/wma+humid))/tamb
	write(6,3560) rhoa
 3560	format(/,' Ambient Air density is ',1pg15.7,' kg/m**3')
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
