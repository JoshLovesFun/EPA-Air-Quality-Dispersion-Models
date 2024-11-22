	subroutine SZF(Q,rL,WCP,sz,cclay,wclay,rholay)
c
c....................................................................
C
C	FUNCTION TO RETURN SZ0 CALCULATED over the source without
c	 a blanket present underneath
c
c	NOTE: Uses the integration package RKGST and cannot be used
c	    with any other routine without a local copy of RKGST.
C
c.....................................................................
c
	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	external szlocal,szloco
C
	include 'DEG1.prm'
C
	COMMON
     ./SZFC/	szstp0,szerr,szsz0
C
	dimension Y(1),D(1),PRMT(17),aux(8,1)
c
c... set up the integration
c
	prmt(1) = 0.D0
	prmt(2) = rL
	prmt(3) = szstp0
	prmt(4) = szerr
	prmt(5) = prmt(2) - prmt(1)
	prmt(6) = Q
	PRMT(7) = WCP
c
	Y(1) = 0.D0	! rho*dellay*u0*z0/(1.+alpha)*(sz/z0)**(1.+alpha)
	D(1) = 1.D0
c
	ndim = 1
c
	call rkgst(prmt,y,d,ndim,ihlf,szlocal,szloco,aux)
c
	if(ihlf.ge. 10) call trap(3,ihlf)
c
	cclay = prmt(13)
	wclay = prmt(14)
	rholay = prmt(15)
	cc = prmt(16)
	sz = prmt(17)
C
	RETURN
	END
c
c.......................................................................
c
	subroutine szlocal(x,y,d,prmt)
c
c.......................................................................
c
c	Routine to calculate the appropriate derivative
c
c.......................................................................
c
	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	include 'DEG1.prm'
C
	COMMON
     ./ALP/	ALPHA,alpha1
     ./parm/	u0,z0,zr,rml,ustar,vkc,gg,rhoe,rhoa,deltay,betay,gammaf,
     .		yclow
     ./phicom/	iphifl,dellay
c

	dimension y(1),d(1),prmt(1)
c
	integer rhouhlay/1/
c
c
	Q = prmt(6)
	WCP = PRMT(7)
c
c... for start up...
c
	if(Y(rhouhlay) .le. 0.D0) then
	    wclay  = 0.D0
	    rholay = rhoa
	    cclay  = 0.D0
	    cc     = 0.D0
	    rho    = rhoa
	else
c
c... from the contaminant material balance over the source, determine WCLAY
c
	    wclay = Q*x/Y(rhouhlay)
c
	    call adiabat(1,wclay,walay,yc,ya,cclay,rholay,wm,enth,temp)
	    cc = cclay*dellay
	    call adiabat(0,wc,wa,yc,ya,cc,rho,wm,enth,temp)	! centerline
c
	endif
c
	uheff = Y(rhouhlay)/rholay/dellay
	sz = ( uheff/u0/z0*(alpha1) )**(1.D0/alpha1) * z0
	heff = gammaf*sz/alpha1
	ristar = rif(rho,heff)
	phi = phif(ristar,0.D0)
	wel = dellay * vkc*ustar*alpha1/phi
	D(rhouhlay) = wel*rhoa + Q/WCP
c
	prmt(8) = cclay
	prmt(9) = wclay
	prmt(10)= rholay
	prmt(11)= cc
	prmt(12) = sz
	return
	end
c
c.........................................................................
c
	subroutine szloco(x,y, dery,ihlf,ndim, prmt)
c
c........................................................................
c
c	"Output" routine which ensures valid numbers are retained
c
c........................................................................
c
	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	dimension y(1), dery(1), prmt(1)
c
	prmt(13) = prmt(8)
	prmt(14) = prmt(9)
	prmt(15) = prmt(10)
	prmt(16) = prmt(11)
	prmt(17) = prmt(12)
	return
	end
