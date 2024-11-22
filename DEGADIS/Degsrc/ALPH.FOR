C...............................................................
C
C	SUBROUTINES TO CALCULATE THE VALUE OF ALPHA
c
C...............................................................

	SUBROUTINE ALPH
c
c...............................................................
c
c	ALPH calculates USTAR and sets up the call to ZBRENT which
c	determines ALPHA.
c
c...............................................................
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
C
	EXTERNAL ALPHI
C
c... calculate USTAR
c
	PSI = PSIF(Z0,rml)
	USTAR = U0*vkc/(dLOG((Z0+ZR)/ZR) - PSI)
c
c... catch zero windspeed cases
c
	if(u0 .eq. 0.D0) then
	    alpha = 0.D0
	    ustar = 0.D0
	    return
	endif
c
c... check to see if a particular ALPHA is desired
c
	if(ialpfl.eq. 0) then
	    alpha = alpco
	    return
	endif
C
C... ZBRENT USED TO DETERMINE THE ROOT OF THE REQUIRED INTEGRAL EQUATION
C
	IER = 0
C
	call zbrent(alpha, ALPHI, XLI, XRI, EPS, ier)
	IF( IER .NE. 0 ) CALL trap(19,IER)
C
	RETURN
	END
c
c..................................................................
c
	FUNCTION ALPHI(X)
c
C...................................................................
c
c	Function to evaluate the weighted euclidean norm of the
C	error (ALPHI) associated with the power law fit of the wind
c	profile.  The argument (X) is the trial value of ALPHA.
c
c.....................................................................
C

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

C
	include 'DEG1.prm'
c
	COMMON
     ./ALP/	ALPHA,alpha1
     ./ERROR/	STPIN,ERBND,WTRG,WTtm,WTya,wtyc,wteb,wtmb,XLI,
     .		XRI,EPS,ZLOW,STPINZ,ERBNDZ,SRCOER,srcss,srccut,
     .		ERNOBL,NOBLpt,crfger,epsilon
     ./parm/	u0,z0,zr,rml,ustar,vkc,gg,rhoe,rhoa,deltay,betay,gammaf,
     .		yclow
c
C
	DIMENSION Y(1),DERY(1),PRMT(5),AUX(8)
	EXTERNAL ARG,ARGOUT
C
c... set up to do the integration
c
	ALPHA = X
C
	PRMT(1) = Z0
	PRMT(2) = dmax1(ZLOW,zr)	! to take care of large zr
	PRMT(3) = STPINZ
	PRMT(4) = ERBNDZ
	PRMT(5) = prmt(2) - prmt(1)
C
	Y(1) = 0.0D00
C
	DERY(1) = 1.0D00
C
	NDIM = 1
	IHLF = 0
C
	CALL RKGST(PRMT,Y,DERY,NDIM,IHLF,ARG,ARGOUT,AUX)
C
	IF(IHLF .GE. 10) CALL trap(18,IHLF)
	ALPHI = Y(1)
	RETURN
	END
c
c....................................................................
c
	SUBROUTINE ARG(Z,Y,D,PRMT)
c
C....................................................................
C
C	This function evaluates the integrand needed to determine ALPHA.
C
c....................................................................
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

C
	include 'DEG1.prm'
C
	COMMON
     ./ALP/	ALPHA,alpha1
     ./alpcom/	ialpfl,alpco
     ./parm/	u0,z0,zr,rml,ustar,vkc,gg,rhoe,rhoa,deltay,betay,gammaf,
     .		yclow
C
C
	DIMENSION Y(1),D(1),PRMT(1)
C
C... Weight function used
C
	W = 1.D00/(1.D00 + Z)
	if(ialpfl.eq. 2) w= 1.D00
C
C... log windspeed profile @ z
C
	UBST = USTAR/vkc*(dLOG((Z+ZR)/ZR) - PSIF(Z,rml))
C
C... windspeed profile @ z -- power law approximation
C
	UALP = U0 * (Z/Z0) ** ALPHA
C
	D(1) = W * (UBST - UALP) * dLOG(Z/Z0) * UALP
	RETURN
	END
c
c.......................................................................
C
	SUBROUTINE ARGOUT
c
c.......................................................................
c
c	ARGOUT is a dummy output function required for this application
c	of RKGST.
c
c.......................................................................
c
	RETURN
	END
