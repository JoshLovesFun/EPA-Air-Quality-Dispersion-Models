	SUBROUTINE SSG(DIST,Y,Dery,PRMT)
c
C.............................................................
C
C	Routine which evaluates the derivatives for the "Gaussian"
c	stage of integration
c
c.............................................................
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

C
	DIMENSION Y(1),Dery(1),PRMT(1)
C
	include 'DEG2.prm'
c
	parameter (zero=1.D-10, rcrit=2.5D-3)
c
	common
     ./ALP/	ALPHA,alpha1
     ./cgprop/	gasmw,gastem,gasrho,gascpk,gascpp,gasulc,gasllc,gaszzc,
     .		gasnam
     ./comatm/	istab,tamb,pamb,humid,isofl,tsurf,ihtfl,htco,iwtfl,wtco,
     .		humsrc
     ./parm/	u0,z0,zr,rml,ustar,vkc,gg,rhoe,rhoa,deltay,betay,gammaf,
     .		yclow
     ./phicom/	iphifl,dellay
     ./PHLAG/	CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
c
	character*3 gasnam
C
	LOGICAL CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
C
C
	DATA irhouh/1/,idh/2/, mhi/3/, mlow/4/
C
c..............................................................
c
C... PRMT(I) I/O
C	I	     VALUE	     IN/OUT
C      --	     -----	     ------
C	6		E		IN
C	7		Cc		OUT
C	8		XV(I)		IN
C	9		T0(I)		IN
C	10		-		-
C	11		NREC(I,2)	OUT -- STARTS OUTPUT UNIT=9
C	12		DIST		OUT
c	13		sz
c	14		yc		out
c	15		rho		out
c	16		temp		out
c	17		gamma		out
c	18
c	19
c	20		rholay
c	21		sz
c
c...............................................................
C
	XVl = PRMT(8)
	SY = RT2*DELTAy*(DIST + XVl)**BETAy
	Erate = PRMT(6)
c
c... start with the last value of SZ
c
	sz0 = prmt(22)
	sz = sz0
C
C... MATERIAL BALANCE loop
C
	iii = 0
 100	Cc = Erate*ALPHA1*(Z0/SZ)**ALPHA/U0/SZ/SQRTPI/SY
c
	cclay = cc/dellay
	call addheat(cclay,Y(idh),rholay,temlay,cp)
	prod = dmax1( Y(irhouh)/rholay/prmt(18), zero)
	sz = ( prod ) **(1.D0/alpha1) * z0
	dif = abs(sz - sz0)/(abs(sz)+abs(sz0)+zero)
	if(dif.gt. rcrit) then
	    iii = iii+1
	    if(iii.gt. 20) call trap(33,0)
	    sz0 = sz
	    goto 100
	endif
	prmt(20) = rholay
	prmt(21) = sz
	HEFF = GAMMAF*SZ/ALPHA1

	call adiabat(0,wc,wa,yc,ya,cc,rho,wm,enth,temp)
	call adiabat(0,wc,wa,yclay,ya,cclay,rholam,wml,enth,temlam)
	rit = 0.
c
	if(isofl.eq.0 .and. ihtfl.ne.0) then
	    call addheat(cc,dellay*Y(idh),rho,temp,cp)
	    rit = rift(temp,heff)
	endif
C
	RISTR = RIF(RHO,HEFF)
	PHI = PHIF(RISTR,rit)
C
	dery(irhouh) = prmt(19)/phi
	heigh = heff*dellay
	yw = 1.D0-yclay-ya
	yw = min( max( yw, 0.0D0), 1.0D0)
	call surfac(temlay,heigh,rholay,wml,cp,yw,watrte,qrte)
	if(temp.ge. tsurf .or. temlay.ge. tamb) qrte = 0.
	dery(idh) = ( qrte/dellay-Y(idh)*Dery(irhouh) )/Y(irhouh)
C
c... Calculate the derivative for the total mass above the UFL and LFL
c
	gamma = (rho-rhoa)/cc			! gamma

	if(check4) then
	   DERY(mlow) = 0.
	   DERY(mhi)  = 0.
	   if( isofl.eq.1 .or. ihtfl.eq.0 ) then
		call adiabat(2,aa,dd,gasulc,ee,chi ,ggg,hh,pp,oo)
		call adiabat(2,aa,dd,gasllc,ee,clow,ggg,hh,pp,oo)
	   else
		call adiabat(-2,aa,dd,gasulc,ee,chi ,ggg,hh,gamma,oo)
		call adiabat(-2,aa,dd,gasllc,ee,clow,ggg,hh,gamma,oo)
	   endif

	   gammax = sqrtpi * Cc * Sz * Sy * GAMMAF / alpha1

	   if(cc.gt.clow) then
		wlow = Dlog(cc/clow)
		DERY(mlow)= 2.D0*clow*Sy*Sz/alpha1*series(wlow)
		DERY(mlow)= DMIN1( DERY(mlow), gammax )
	   endif

	   if(cc.gt.chi ) then
		whi  = Dlog(cc/chi )
		DERY(mhi) = 2.D0*chi *Sy*Sz/alpha1*series(whi )
		DERY(mhi) = DMIN1( DERY(mhi ), gammax )
	   endif
	endif
c
	PRMT(7)  = Cc
	PRMT(12) = DIST
	prmt(14) = yc
	prmt(15) = rho
	prmt(16) = temp
	prmt(17) = gamma

	RETURN
	END
