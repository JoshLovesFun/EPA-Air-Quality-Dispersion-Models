	SUBROUTINE PSS(DIST,Y,DERY,PRMT)
c
C.............................................................
C
C	SUBROUTINES FOR PSEUDO-STEADY STATE INTEGRATION.
C
c.............................................................
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )
C
	include 'DEG2.prm'
c
	parameter (zero=1.D-10, rcrit=2.D-3)
c
	common
     ./ALP/	ALPHA,alpha1
     ./cgprop/	gasmw,gastem,gasrho,gascpk,gascpp,gasulc,gasllc,gaszzc,
     .		gasnam
     ./comatm/	istab,tamb,pamb,humid,isofl,tsurf,ihtfl,htco,iwtfl,wtco,
     .		humsrc
     ./csprd/	ce, delrmn
     ./parm/	u0,z0,zr,rml,ustar,vkc,gg,rhoe,rhoa,deltay,betay,gammaf,
     .		yclow
     ./PHLAG/	CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
     ./phicom/	iphifl,dellay
c
	character*3 gasnam
C
	LOGICAL CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
C
	DIMENSION Y(1),DERY(1),PRMT(1)
	DATA irhouh/1/,iSY2/2/,iBEFF/3/,idh/4/,Mhi/5/,Mlow/6/
C
c..................................................................
c
C... PRMT I/O SETUP
C	I	VALUE		IN/OUT
C     ----	-----		-----
C      6	 E		  IN
C      7	 Cc		 OUT
C      8	 Bb		 OUT
C      9	CON DERY(iBEFF)	  IN
C     10	CON DERY(SZ)	  IN
C     11	NREC(I,1)        OUT -- STARTS OUTPUT COUNTER
C     12	DIST		 OUT
c     13
c     14	yc		 out
c     15	rho		 out
c     16	temp		 out - if recorded
c     17	gamma		 out - if recorded
c     18
c     19
c     20
c     21	sz
c     22	sz
c
c......................................................................
C
	Erate = PRMT(6)
	Bb = Y(iBEFF) - SQrtPI/2.D0*sqrt(Y(iSY2))
c
c... start with the last value for Sz
c
	sz0 = prmt(22)
	sz = sz0
C
C... MATERIAL BALANCE loop
C
	iii = 0
 100	continue
	Cc = Erate*ALPHA1/2.D0/U0*(Z0/SZ)**ALPHA/SZ/Y(iBEFF)
	cclay = cc/dellay
	call addheat(cclay,y(idh),rholay,temlay,cp)
	prod = dmax1( Y(irhouh)/rholay/prmt(18), zero)
	sz = ( prod )**(1.D0/alpha1) * z0
	dif = abs(sz - sz0)/(abs(sz)+abs(sz0)+zero)
	if(dif .gt. rcrit) then
		sz0 = sz
		iii=iii+1
		if(iii .gt. 100) call trap(32,0)
		goto 100
		endif
	prmt(20) = rholay
	prmt(21) = sz
	HEFF = GAMMAF/ALPHA1*SZ

	call adiabat(0,wc,wa,yc,ya,cc,rho,wm,enth,temp)
	call adiabat(0,wc,wa,yclay,ya,cclay,rholam,wml,enth,temlay)
c
	rit = 0.
	if(isofl.eq.0 .and. ihtfl.ne.0) then
		call addheat(cc,dellay*y(idh),rho,temp,cp)
		rit = rift(temp,heff)
		endif
	RISTR = RIF(RHO,HEFF)
	PHI = PHIF(RISTR,rit)
C
C... CALCULATE DERIVATIVES
C
	DERY(iBEFF) = 0.D0
	delrho = rho-rhoa
	IF(delrho .GT. delrmn) DERY(iBEFF) = PRMT(9)*sqrt(delrho/rhoa)
     .		*(SZ/Z0)**(0.5D0 - ALPHA)
C
	if(Y(ibeff) .gt. 0.D0) then		! to handle numerical transients
	    DERY(iSY2) = 8.D0*BETAy/PI*Y(iBEFF)**2 *
     .		(DELTAy*SQPIO2/Y(iBEFF)) ** (1.D0/BETAy)
	else
	    DERY(iSY2) = 0.D0
	endif
C
c
	heigh = heff*dellay
	yw = 1.D0-yclay-ya
	yw = min( max( yw, 0.D0 ), 1.D0)
	call surfac(temlay,heigh,rholay,wml,cp,yw,watrte,qrte)
	if(temp.ge. tsurf .or. temlay.ge. tamb) qrte = 0.D0
	ruhb = rholay* prmt(18) * (sz/z0)**alpha1 * Y(ibeff)
	druhb = prmt(19)*y(ibeff)/phi
	dERY(idh) = (qrte*Y(ibeff)/dellay - Y(idh)*druhb)/ruhb
	dERY(irhouh) = (druhb-Y(irhouh)*DERY(ibeff))/Y(ibeff)
c
c... Calculate the derivative for the total mass above the UFL and LFL
c
	gamma = (rho-rhoa)/cc			! gamma
	if(check4) then
	   DERY(mlow) = 0.D0
	   DERY(mhi)  = 0.D0

	   if( isofl.eq.1 .or. ihtfl.eq.0 ) then
		call adiabat(2,aa,dd,gasulc,ee,chi ,ggg,hh,pp,oo)
		call adiabat(2,aa,dd,gasllc,ee,clow,ggg,hh,pp,oo)
	   else
		call adiabat(-2,aa,dd,gasulc,ee,chi ,ggg,hh,gamma,oo)
		call adiabat(-2,aa,dd,gasllc,ee,clow,ggg,hh,gamma,oo)
	   endif

	   gamhi  = 2.D0* Cc * Bb * Sz / alpha1
	   gammax = 2.D0* Cc * Sz * GAMMAF/alpha1
     .			* (Bb+sqrtpi/2.D0*sqrt(Y(iSy2)))

	   if(cc.gt.clow) then
		wlow = Dlog(cc/clow)
		gamlow = gaminc(1.D0/alpha1, wlow ) * gamhi
		DERY(mlow)= gamlow
     .			+ 2.D0*clow*sqrt(Y(iSy2))*Sz/alpha1*series(wlow)
		DERY(mlow)= DMIN1( DERY(mlow), gammax )
	   endif

	   if(cc.gt.chi) then
		whi  = Dlog(cc/chi )
		gamhi  = gaminc(1.D0/alpha1, whi  ) * gamhi
		DERY(mhi) = gamhi
     .			+ 2.D0*chi *sqrt(Y(iSy2))*Sz/alpha1*series(whi )
		DERY(mhi) = DMIN1( DERY(mhi ), gammax )
	   endif
	endif
C
C... RETURNED VALUES
C
	PRMT(7) = Cc
	PRMT(8) = Bb
	prmt(14)= yc
	prmt(15)= rho
	prmt(16)= temp
	prmt(17)= gamma
	RETURN
	END
