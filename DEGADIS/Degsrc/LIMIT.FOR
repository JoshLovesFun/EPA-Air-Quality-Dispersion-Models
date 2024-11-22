	subroutine limit(func, x0, xinc, xhigh, xlow)
c
c.....................................................................
c
c	subroutine to establish the upper and lower limits for ZBRENT
c
c.....................................................................
c

	implicit real*8(a-h,o-z), integer*4(i-n)
c
c... set some initial values
c
	rrr = 1.D0
	aflag = 1.D0
	bflag = 1.D0
	fc = func(x0)
c
c... major loop............................................................
c
100	continue
c
c...	As appropriate, add "xinc" to the limit...........................
c
	if(aflag .eq. 1.D0) then
	    aaa = x0 + xinc*rrr
		if(aaa .ge. xhigh) then
		    aaa = xhigh
		    aflag = 0.D0
		endif
	    fa = func(aaa)
		if(sign(1.D0,fa)*sign(1.D0,fc) .lt. 0.D0) then
		    xhigh = aaa
		    xlow  = aaa - xinc*1.01D0
		    return
		endif
	endif
c
c...	As appropriate, subtract "xinc" from the limit.......................
c
	if(bflag .eq. 1.D0) then
	    bbb = x0 - xinc*rrr
		if(bbb .le. xlow) then
		    bbb = xlow
		    bflag = 0.D0
		endif
	    fb = func(bbb)
		if(sign(1.D0,fb)*sign(1.D0,fc) .lt. 0.D0) then
		    xhigh = bbb + xinc*1.01D0
		    xlow  = bbb
		    return
		endif
	endif
c
c...	Bump RRR and try again..........................................
c
	if(sign(1.D0,fa)*fb .gt. 0.D0) then
	    rrr = rrr + 1.D0
	    if(rrr .gt. 400.D0) then
		write(6,*) 'LIMIT? Failed to find the limits.'
		stop
	    endif
	    goto 100
	endif

	xhigh = aaa
	xlow  = bbb
	return
	end
