	subroutine ellips(yaxis, zaxis, zseg, circum, area)
c
c...........................................................................
c
c	Subroutine to return the properties of an ellipse or ellipse segment.
c	The curved perimeter is returned as CIRCUM and the
c	area as AREA as functions of the axis lengths (YAXIS and ZAXIS)
c	and the length of the z-axis segment (ZSEG).
c
c	ZSEG is defined so that an entire ellipse is dictated when ZSEG=ZAXIS.
c	Likewise, a half ellipse is dictated when ZSEG=0.
c
c	The calculations are based on algorithms presented in Spanier and
c	Oldham's "An Atlas of Functions," Hemisphere, 1987. (page 113)
c
c...........................................................................
c
	Implicit real*8 (a-h, o-z), integer*4 (i-n)

	include 'degin.prm'

c
c... Do the easy cases first - for ZSEG=ZAXIS and ZSEG=0.......................
c	Determine AAA and BBB and do first elliptical integral determination
c
	if(zaxis .le. yaxis) then
	   aaa = yaxis
	   bbb = zaxis/yaxis
	else
	   aaa = zaxis
	   bbb = yaxis/zaxis
	endif
	arg = max(1.D0 - bbb**2, 0.D0)
	arg = sqrt(arg)

	pio2 = pi/2.D0
	call elli(arg, pio2, eee, eeei)
c
c... Check to see if the ellipse is complete.
c
	if(zseg .eq. zaxis) then
	   area = pi*yaxis*zaxis
	   circum = 4.D0*aaa*eee
	   return
	endif
c
c... Check to see if this is a "semi"ellipse
c
	if(zseg .eq. 0.D0) then
	   area = pi*yaxis*zaxis/2.D0
	   circum = 2.D0*aaa*eee
	   return
	endif
c
c... For an incomplete ellipse, reset AAA, BBB, and XXX
c
	aaa = zaxis
	xxx = zseg
	bbb = yaxis/zaxis
	ratio = zseg/zaxis

	phi = asin(ratio)

	area = yaxis*zaxis*(pi/2.D0 + phi + ratio*sqrt(1.D0-ratio**2))

c
c... Now for the curved perimeter
c
	if(bbb .le. 1.D0) then
	   arg = sqrt(1.D0 - bbb**2)
	   call elli(arg, phi, eee, eeei)
	   circum = 2.D0*aaa*(eeei + eee)
	else
	   arg = sqrt(1.D0 - (1.D0/bbb)**2)
	   phi = pi/2.D0 - phi
	   call elli(arg, phi, eee, eeei)
	   circum = 2.D0*aaa*bbb*(2.D0*eee - eeei)
	endif
	return
	end

c
c........................................................................
c
	subroutine elli(arg, phi, eee, eeei)
c
c.........................................................................
c
c	Subroutine to return the evaluation of the complete elliptical integral
c	E(arg) (as EEE) and the incomplete elliptical integral E(arg; phi)
c	(as EEEI).
c
c	The calculations are based on algorithms presented in Spanier and
c	Oldham's "An Atlas of Functions," Hemisphere, 1987. (page 628)
c
c..........................................................................
c
	Implicit real*8 (a-h, o-z), integer*4 (i-n)

	include 'degin.prm'
c
c... initialize some variables
c
	ttt = 1.D0
	aaa = 1.D0
	eeei = 0.D0
	ggg = sqrt(1.D0 - arg**2)
	eee = 2.D0*(1.D0 + ggg**2)
c
c... start the loop to converge to the common mean of G as outlined in Spanier
c	and Oldham.
c
	iii = 0
 100	continue
	tttttt = tan(ttt*phi)
	phi = phi - atan((aaa-ggg)*tttttt/(aaa+ggg*tttttt**2)) /2.D0/ttt
	eee = eee - ttt*(aaa-ggg)**2
	ttt = 2.D0*ttt
	eeei = eeei + (aaa-ggg)*sin(ttt*phi)
	ggg = sqrt(aaa*ggg)
	aaa = (ggg**2/aaa + aaa)/2.D0

	iii = iii+1
	if(iii .gt. 20) then
		write(6,*) 'ELLI? failed to converge'
		stop
	endif

	if(aaa-ggg .ge. 1.D-8) goto 100
c
c... output the results
c
	eee = pi*eee/8.D0/ggg
	eeei = eeei/2.D0 + 2.D0*phi*eee/pi

	return
	end
