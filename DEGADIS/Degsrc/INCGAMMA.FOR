C...........................................................
c
c	gamma and incomplete gamma function
c
c..........................................................
c
c	routines included in this file:
c		GAMINC		Incomplete gamma function
c		GAMMLN		Natural log of the gamma function
c		GSER		computation procedure
c		GCF		computation procedure
c
c..........................................................
c
c	These functions return the value for the incomplete Gamma function
c	with the arguments (ALPHA,BETA).  The functional values are
c	calculated with either a series representation or a continued
c	fraction representation.  These routines are based on:
c
c	Press, W.H., B.P. Flannery, S.A. Teukolsky, and W.T. Vetterling,
c	"Numerical Recipes", Cambridge University Press, Cambridge,1986.
c
c
c
	function gaminc(alpha,beta)
c
c.....................................................................
c

	implicit Real*8 (a-h, o-z), integer*4 (i-n)
c
c
c... ensure the arguments are within the proper bounds.
c
	if( beta.lt.0. .or. alpha.le.0.) then
	    write(6,9000) alpha,beta
 9000	    format(' GAMINC? Arugments are out-of-bounds. ALPHA: ',
     .			1PG13.5,' BETA: ',1PG13.5)
	    STOP
	endif
c
c... determine which of the series or continued fraction representation
c    is more appropriate.
c
	if( beta .lt. alpha+1. ) then
	    call gser( aa, alpha, beta, gln)		! series
	else
	    call gcf( aa, alpha, beta, gln)		! continued fraction
	    aa = 1.D0 - aa
	endif
c
c... multiply the result by GAMMA(ALPHA) to get the final value.
c
	gaminc = dexp( dlog(aa) + gln)
	return
	end
c
c........................................................................
c
	subroutine gser( gamser, alpha, beta, gln)
c
c.........................................................................
c
c	This routine calculates the incomplete gamma function using
c	the series representation.
c
c.........................................................................
c
	implicit real*8 (a-h,o-z), integer*4 (i-n)
c
	parameter (itmax=100, eps=1.d-9)

	gln = gammln(alpha)

	if(beta .lt. 0.) then
	    write(6,*) 'GSER? BETA is out-of-bounds.'
	    stop
	endif
	if(beta .eq. 0.) then
	    gamser = 0.
	    return
	endif

	ap = alpha
	sum = 1.D0/alpha
	del = sum

	do 100 n=1,itmax
	ap = ap + 1.D0
	del = del* beta /ap
	sum = sum + del
	if( dabs(del) .lt. dabs(sum)*eps) then
	    gamser = sum*dexp( -beta+alpha*dlog(beta)-gln)
	    return
	endif
 100	continue

	write(6,*) 'GSER? ALPHA is too large or ITMAX is too small.'
	stop
	end
c
c.....................................................................
c
	subroutine gcf( gamcf, alpha, beta, gln)
c
c....................................................................
c
c	This routine calculates the incomplete gamma function using
c	the continued fraction representation.
c
c...................................................................
c
	implicit real*8 (a-h,o-z), integer*4 (i-n)
c
	parameter (itmax=100, eps=1.d-9)

	gln = gammln(alpha)

c
c... INITIALIZE some variables.
c
c				! previous value tested for convergence
	gold = 0.
	a0 = 1.D0
	a1 = beta
	b0 = 0.
	b1 = 1.D0
c				! fac is a "renormalization factor"
	fac = 1.D0

	do 100 n=1,itmax
	an = dble(n)
	ana = an - alpha
	a0 = (a1 + a0*ana)
	b0 = (b1 + b0*ana)
	anf = an
	a1 = beta *a0 + anf*a1
	b1 = beta *b0 + anf*b1
	if( a1 .ne. 0.) then
	    fac = 1.D0/b1
	    gg = a1*fac
	    a0 = a0*fac
	    a1 = a1*fac
	    b0 = b0*fac
	    b1 = b1*fac
	    if( dabs((gg-gold)/gg) .lt .eps) then
		gamcf = dexp(-beta+alpha*dlog(beta)-gln)/gg
		return
	    endif
	    gold = gg
	endif
 100	continue

	write(6,*) 'GCF? ALPHA is too large or ITMAX is too small.'
	stop
	end
c
c....................................................................
c
	function gammln( alpha )
c
c...................................................................
c
c	This routine calculates ln( gamma(alpha)) for alpha>0.
c
c...................................................................
c
	implicit real*8 (a-h,o-z), integer*4 (i-n)
c
	dimension cof(6)

	data cof,stp/76.18009173D0, -86.50532033D0, 24.01409822D0,
     . -1.231739516D0, 0.120858003D-2, -0.536382D-5, 2.50662827465D0/
	data half, one, fpf/0.5D0, 1.0D0, 5.5D0/
c
	if( alpha .lt. 1) then
	    gam = gamma( alpha )
	    gammln = dlog(gam)
	    return
	endif
c
	xx = alpha - one
	tmp = xx + fpf
	tmp = (xx + half) * dlog(tmp) - tmp
	ser = one

	do 100 j=1,6
	xx = xx + one
	ser = ser + cof(j)/xx
 100	continue

	gammln = tmp + dlog(stp*ser)
	return
	end
