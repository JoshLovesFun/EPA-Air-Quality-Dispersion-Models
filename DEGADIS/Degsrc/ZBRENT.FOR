	subroutine zbrent(answer, func, x1, x2, tol, ierr)
c
c......................................................................
c
c	Function to determine the root of FUNC which is between X1 and X2
c	from Press et al., pg 253.  Note that the SIGN function has
c	been used here to avoid underflows corrupting the programs
c	logic.
c
c	ANSWER is the returned result.
c	FUNC is the function whose zero is desired.
c	X1 and X2 constitute the search interval for the independent
c		variable.
c	TOL is the absolute tolerance for the INDEPENDENT variable.
c	IERR is an error flag (set nonzero) in case of failure:
c		1 - failed to converge after the specified number of trials
c		2 - initial interval fails to bracket the root to FUNC.
c
c....................................................................
c
	implicit real*8(a-h,o-z), integer*4(i-n)

	parameter (itmax=100, eps=3.D-8)

c
c... check the passed arguments for validity and set up the procedure.
c	Although the SIGN function traps underflows, it may not give
c	a valid comparison if FA or FB or zero.
c
	aaa = x1
	bbb = x2
	ierr = 0
	fa = func(aaa)
	fb = func(bbb)
	if(fa .eq. 0.D0) then
	   answer = aaa
	   return
	endif
	if(fb .eq. 0.D0) then
	   answer = bbb
	   return
	endif
	if(sign(1.D0,fa)*sign(1.D0,fb) .gt. 0.D0) then
	   ierr = 2
	   return
	endif

	fc = fb

	do 11 iter = 1,itmax
c
c... shuffle A,B,C and adjust the bounding interval D
c
	if(sign(1.D0,fb)*sign(1.D0,fc) .gt. 0.D0) then
	    ccc = aaa
	    fc = fa
	    ddd = bbb-aaa
	    eee = ddd
	endif

	if(abs(fc) .lt. abs(fb)) then
	    aaa = bbb
	    bbb = ccc
	    ccc = aaa
	    fa = fb
	    fb = fc
	    fc = fa
	endif
c
c... convergence check
c
	tol1 = 2.D0*eps*abs(bbb) + 0.5D0*tol
	xm = 0.5D0*(ccc-bbb)
	if(abs(xm).le. tol1 .or. fb.eq.0.D0) then
	    answer = bbb
	    return
	endif
c
c... attempt inverse quadratic interpolation
c
	if(abs(eee).ge.tol1 .and. abs(fa).gt.abs(fb)) then
	    sss = fb/fa

	    if(aaa.eq.ccc) then
		ppp = 2.D0*xm*sss
		qqq = 1.D0-sss
	    else
		qqq = fa/fc
		rrr = fb/fc
		ppp = sss*(2.D0*xm*qqq*(qqq-rrr) - (bbb-aaa)*(rrr-1.D0))
				qqq = (qqq-1.D0)*(rrr-1.D0)*(sss-1.D0)
	    endif
c
c.......... check to insure in bounds
c
	    if(ppp .gt. 0.D0) qqq = -qqq
		ppp = abs(ppp)
		if
     .  (2.D0*ppp .lt. min(3.D0*xm*qqq-abs(tol1*qqq),abs(eee*qqq))) then

c.................. interpolation valid
		    eee = ddd
		    ddd = ppp/qqq
		else

c.................. interpolation failed, use bisection
		    ddd = xm
		    eee = ddd
		endif
	    else

c.............. bounds decreasing too slowly, use bisection.
		ddd = xm
		eee = ddd
	    endif
c
c...... move last best guess to A
c
	aaa = bbb
	fa = fb
c
c... evaluate new trial root
c
	if(abs(ddd) .gt. tol1) then
		bbb = bbb+ddd
	else
		bbb = bbb+sign(tol1,xm)
	endif
	fb = func(bbb)
  11	continue
c
c... loop failed to converge the x range
c
	ierr = 1
	return
	end
