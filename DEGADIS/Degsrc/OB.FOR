	SUBROUTINE OB(time,Y,D,PRMT)
c
C....................................................................
C
C	SUBROUTINES OB AND OBOUT ARE USED IN THE OBSERVER INTEGRATIONS
C	     OVER THE SOURCE.
C
c....................................................................
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )
C
	include 'DEG2.prm'
c
	COMMON
     ./ALP/	ALPHA,alpha1
     ./comatm/	istab,tamb,pamb,humid,isofl,tsurf,ihtfl,htco,iwtfl,wtco,
     .		humsrc
     ./GEN3/	radg(2,maxl),qstr(2,maxl),srcden(2,maxl),srcwc(2,maxl),
     .		srcwa(2,maxl),srcen(2,maxl)
     ./PARM/	U0,Z0,ZR,rml,USTAR,vkc,gg,RHOE,RHOA,DELTA,BETA,GAMMAF,
     .		yclow
     ./PARMSC/	RM,SZM,EMAX,RMAX,TSC1,ALEPH,TEND
     ./phicom/	iphifl,dellay
c
	logical flag
C
	DIMENSION Y(1),D(1),PRMT(1)
	INTEGER HWIDTH,Mrate,Crate,BDArate,Hrate
	DATA HWIDTH/1/,Mrate/2/,Crate/3/,BDArate/4/,Hrate/5/
C
C... PASS T0 IN PRMT(6)
C
	flag = isofl.eq. 1 .or. ihtfl.eq. 0
c
	T0L  = PRMT(6)
	xup  = prmt(7)
	XI   = XIT(TIME,T0L)
	RG   = AFGEN(RADG,TIME,'RADG')
	RLEN = PRMT(13)
C
	BIPR      = 0.D0
	D(HWIDTH) = 0.D0
	D(Crate)  = 0.D0
	D(Mrate)  = 0.D0
	D(BDArate)= 0.D0
	D(Hrate)  = 0.D0

c
c... We must deal with the case when the observer sees no gas, but the.....
c	integration is not over yet.  Since the derivatives have been set
c	to zero above, no work on the derivatives is needed, but the values
c	in PRMT(14-18) must be preserved.  Use PRMT(19) to signal this
c	condition.
c
c..........................................................................
c
	prmt(19) = abs(xi) - rg
	IF(ABS(XI) .GE. RG) return	! use the last values when prmt(19)>0

	BIPR = sqrt(RG*RG - XI*XI)
C
	UI = UIT(TIME,T0l)
C
	Q    = AFGEN(QSTR,TIME,'QSTR')
	wc   = AFGEN(srcwc,time,'srcwc')
	wa   = AFGEN(srcwa,time,'srcwa')
	enth = AFGEN(srcen,time,'srcen')
c
	wclay   = Y(Crate)/Y(Mrate)
	walay   = Y(BDArate)/Y(Mrate)
	if(.not.flag) enthl = Y(Hrate)/Y(Mrate)
c
	call tprop(1,wclay,walay,enthl,yc,ya,wm,temp,rholay,cp)
	cclay = wclay*rholay
c
	prmt(8) = cclay
	prmt(9) = wclay
	prmt(10)= walay
	prmt(11)= enthl
	prmt(12)= rholay
c
	cc  = cclay*dellay
	rho = dellay*(rholay-rhoa) + rhoa
c
	szob = z0*(Y(Crate)/Y(Hwidth)/cc/(u0*z0/alpha1))**(1.D0/alpha1)
c
	HEFF  = GAMMAF/ALPHA1* SZOB
	Hlay  = dellay * Heff
	RISTR = max(RIF(RHO,HEFF), RIF(RHOlay,Hlay), 0.D0)
	PHI   = PHIF(RISTR,0.D0)
	welay = dellay * vkc*USTAR* ALPHA1/PHI
C
	da        = ui * Bipr
	D(HWIDTH) = da / RLEN
	D(Crate)  = Q * da
	D(Mrate)  = (Q/wc + rhoa*welay) * da
	D(BDArate)= (Q*wa/wc + rhoa*welay/(1.D0+humid)) * da
	if(flag) return
	D(Hrate) = Q * enth/wc * da
C
 1000	FORMAT(' ?OB? -- Value of XI ',1pG13.4,'; Value of RG ',
     .		1pG13.4)
	RETURN
	END
c
c
	SUBROUTINE OBOUT( X, Y, DERY, IHLF, NDIM, PRMT)
C
	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	DIMENSION Y(1), DERY(1), PRMT(1)
C
	if(prmt(19) .gt. 0.D0) return
	PRMT(14) = prmt(8)	! cclay
	PRMT(15) = prmt(9)	! wclay
	PRMT(16) = prmt(10)	! walay
	PRMT(17) = prmt(11)	! enthl
	PRMT(18) = prmt(12)	! rholay
	RETURN
	END
