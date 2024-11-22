	function erf(xxx)
c
c.......................................................................
c
c	Function to return the error function of XXX.
c
c.......................................................................
c
	implicit real*8(a-h,o-z), integer*4(i-n)
c
	parameter (half=0.5D0)
c
c
	if(xxx.lt.0.) then
	    stop' argument of erf < 0'
	else if(xxx.eq.0.D0) then
	    erf = 0.D0
	else
	    erf = gaminc(half, xxx*xxx)/gamma(half)
	endif
	return
	end
