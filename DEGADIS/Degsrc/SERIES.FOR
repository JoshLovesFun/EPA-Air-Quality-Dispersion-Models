	function series(arg)
c
c......................................................................
c
c    This function estimates the infinite series used in the evaluation
c	of the mass above a certain concentration level.
c
c**********************************************************************
c
c*** WARNING: This routine will overflow for arguments greater than 13.8
c
c**********************************************************************
c
c......................................................................
c
	implicit real*8  (a-h,o-z), integer*4 (i-n)
c
	parameter (kmax= 100, error=1.d-4)
c
	common/alp/ alpha,alpha1
c
c... initialize some variables
c
	pp = 1.D0/alpha1
	pprod = pp	! for p*(p+1)*...*(p+k)
c
c... calculate the first term (kk=0)
c
	coeff = 2.D0/3.D0
	term = coeff / pp * arg**1.5D0
	series = term
c
c
	do 200 kk=1,kmax
	term = 2.D0*dble(kk+1)/dble(2*kk+3)/(pp + dble(kk)) * arg * term

	series = series + term

	denom = dmax1( abs(series), error)
	if( abs(term)/denom .le. error) return
  200	continue
c
	write(6,900) arg
 900	format(' SERIES? Number of iterations too small for argument: ',
     .				1pg13.5)
	stop
	end
