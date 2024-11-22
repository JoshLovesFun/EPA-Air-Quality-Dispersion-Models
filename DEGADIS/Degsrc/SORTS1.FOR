	SUBROUTINE SORTS1(TABLE)
c
C............................................................
C
C   Subroutine to apply the downwind dispersion correction as desired.
c
c............................................................
c
	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )
c
	include 'DEG3.prm'

	external sortsi,sortso
C
	COMMON
     ./ALP/	ALPHA,alpha1
     ./cgprop/	gasmw,gastem,gasrho,gascpk,gascpp,gasulc,gasllc,gaszzc,
     .		gasnam
     ./CNOBS/	NOBS
     ./comatm/	istab,tamb,pamb,humid,isofl,tsurf,ihtfl,htco,iwtfl,wtco,
     .		humsrc
     ./csigx/	sigxco,sigxp,sigxmd,sigxfl
     ./PARM/	U0,Z0,ZR,rml,USTAR,vkc,gg,RHOE,RHOA,DELTAy,BETAy,GAMMAF,
     .		ycLOW
     ./PARMSC/	RM,SZM,EMAX,RMAX,TSC1,ALEPH,TEND
     ./SORTa/	TCc(maxnob,maxnt),TCcSTR(maxnob,maxnt),
     .		Tyc(maxnob,maxnt),Trho(maxnob,maxnt),
     .		Tgamma(maxnob,maxnt),Ttemp(maxnob,maxnt)
     ./SORTb/	TSY(maxnob,maxnt),TSZ(maxnob,maxnt),TB(maxnob,maxnt),
     .		TDIST0(maxnob,maxnt),TDIST(maxnob,maxnt),KSUB(maxnt)
     ./SORTIN/	TIM(maxnt),NTIM,ISTRT
     ./SSCON/	NREC(maxnob,2),T0(maxnob),XV(maxnob)
C
	real*4 tcc,tccstr,tyc,trho,tgamma,ttemp,tsy,tsz,tb,tdist0

	DIMENSION TABLE(1)

	character*3 gasnam
C
	logical cflag
C
c
C... DETERMINE IF ANY TIME VECTORS HAVE NO ENTRIES
C
	DO 192 I=1,NTIM
  192	IF(KSUB(I).GT. 2) GO TO 194	! 1or2 points is of little value
	call trap(23,0)
  194	ISTRT = I
	DO 196 I=ISTRT,NTIM
  196	IF(KSUB(I).LE. 2) GO TO 198
	GO TO 199
  198	NTIM = I - 1
  199	CONTINUE
C
C... REVERSE TIME SORTED VECTORS...........................................
C
C... At this point, the results from Observer #1 is located in the
c	first row of each vector.  But the information contributed by
c	Observer #1 is the information which is the farthest (TDIST)
c	from the source.  If the output is printed by time, the
c	distances will decrease instead of increase.  So, we need to
c	reverse each of the columns so that the downwind distances
c	increase as you move down the column.  At the same time,
c	ensure that all of the nonzero entries in a column are at
c	the top of the column.
c
c....................................................................
c
	DO 200 K1 = ISTRT,NTIM
C
	II = KSUB(K1)

	DO 170 J = 1,NOBS
	IF( Tsz(J,K1).NE.0. ) GO TO 180
  170	CONTINUE
  180	continue
	II = II + J - 1
C
c... II now points to the last nonzero entry in the vectors.  The object
c	is to make the switch between the first half and the last half.
c	Therefore, the loop counter only needs to range over half the list.
c	(If II is odd, the value in the middle of the list will not move.)
c
	DO 190 J = 1,II/2

	xxx             = TCc(J,K1)
	TCc(J,K1)       = TCc(II-J+1, K1)
	TCc(II-J+1, K1) = xxx
	xxx             = Tyc(J,K1)
	Tyc(J,K1)       = Tyc(II-J+1, K1)
	Tyc(II-J+1, K1) = xxx
	xxx              = Trho(J,K1)
	Trho(J,K1)       = Trho(II-J+1, K1)
	Trho(II-J+1, K1) = xxx
	xxx                = Tgamma(J,K1)
	Tgamma(J,K1)       = Tgamma(II-J+1, K1)
	Tgamma(II-J+1, K1) = xxx
	xxx               = Ttemp(J,K1)
	Ttemp(J,K1)       = Ttemp(II-J+1, K1)
	Ttemp(II-J+1, K1) = xxx
	xxx             = TSY(J,K1)
	TSY(J,K1)       = TSY(II-J+1, K1)
	TSY(II-J+1, K1) = xxx
	xxx             = TSZ(J,K1)
	TSZ(J,K1)       = TSZ(II-J+1, K1)
	TSZ(II-J+1, K1) = xxx
	xxx            = TB(J,K1)
	TB(J,K1)       = TB(II-J+1, K1)
	TB(II-J+1, K1) = xxx
	xxx                = TDIST0(J,K1)
	TDIST0(J,K1)       = TDIST0(II-J+1, K1)
	TDIST0(II-J+1, K1) = xxx
	xxx                = TDIST(J,K1)
	TDIST(J,K1)       = TDIST(II-J+1, K1)
	TDIST(II-J+1, K1) = xxx

  190   CONTINUE
C
  200	CONTINUE
c
c
	if(sigxfl.eq. 0.D0) then		! no correction
c
	    write(lunlog,*) ' No X-direction dispersion correction'
	    DO 220 K1 = ISTRT,NTIM
	    II = KSUB(K1)
		DO 220 I = 1,II
		TCcSTR(I,K1) = TCc(i,k1)
  220       CONTINUE
	    goto 400
	endif
C
C... GENERATE TCcSTR -- CENTER LINE CONCENTRATION CORRECTED FOR..............
C	DOWNWIND DISPERSION.
C
	DO 250 K1 = ISTRT,NTIM
C
	II = KSUB(K1)
	DO 240 I = 1,II
C
c... perform the integration for each point in the field at this time as
c	appropriate
c
	TCcSTR(I,K1) = 0.

	xxx = tdist(i,k1) - tdist0(i,k1)
	if(xxx .lt. sigxmd) then
	    tccstr(i,k1) = tcc(i,k1)
	    goto 240
	endif

	SX = sigxco* xxx**sigxp
	xxx = 4.D0 * sx
	table(1) = max(tdist(1,k1),  tdist(i,k1)-xxx)	! lower limit
	table(2) = min(tdist(ii,k1), tdist(i,k1)+xxx)	! upper limit
	table(3) = xxx/20.D0				! initial step size
	table(4) = 0.003D0				! error bound
	table(5) = 2.D0 * sx				! max step size
	table(6) = tdist(i,k1)			! the x value in question
	table(7) = k1				! the time location

	table(8) = 0.D0				! initial value
	table(9) = 1.D0				! error weight

	ndim = 1
	ihlf = 0

	call rkgst(table(1), table(8), table(9), ndim, ihlf, sortsi,
     .		sortso, table(10))

	if(ihlf .gt. 10) then
	    write(6,*) ' SORTS1 error; IHLF= ',ihlf
	    stop
	endif

	TCcSTR(I,K1) = Table(8)/RT2/SQRTPI
c
c... correct yc, rho, and temp values....................................
c
 230	continue
	cc = Tccstr(i,k1)

	if(isofl.eq. 1 .or. ihtfl.eq. 0) then
	    call adiabat(0,wc,wa,yc,ya,cc,rho,wm,enth,temp)
	else
	    enth = Tgamma(i,K1)
	    call adiabat(-1,wc,wa,yc,ya,cc,rho,wm,enth,temp)
	endif
	Tyc(i,K1) = yc
	Trho(i,K1)= rho
C
  240   CONTINUE
  250   CONTINUE
C
C... Estimate the mass between the ULC and LLC.............................
C
 400	continue
c
	cflag = isofl.eq.1 .or. ihtfl.eq.0
	if(cflag) then
	    call adiabat(2,aa,dd,gasulc,ee,chi ,ggg,hh,pp,oo)
	    call adiabat(2,aa,dd,gasllc,ee,clow,ggg,hh,pp,oo)
	endif
c
	DO 430 K1 = ISTRT,NTIM
	kk  = k1+2*maxnob
	kk1 = kk+maxnt
	table(kk) = 0.
	table(kk1)= 0.
C
	II = KSUB(K1)
	DO 460 J = 1,II	! evaluate the function at each point in space
C
c...  initialize some values
c
	TABLE(J) = 0.
	jj = j+maxnob
	TABLE(JJ) = 0.
	cc = TCcstr(j,k1)
	bb = tb(j,k1)
	sy = tsy(j,k1)
	sz = tsz(j,k1)
	gamma = tgamma(j,k1)

	if(.not.cflag) then
	   call adiabat(-2,aa,dd,gasulc,ee,chi ,ggg,hh,gamma,oo)
	   call adiabat(-2,aa,dd,gasllc,ee,clow,ggg,hh,gamma,oo)
	endif
c
c... Calculate the derivative for the total mass above the ULC and LLC
c
	gamhi  = 2.D0* Cc * Bb * Sz / alpha1
	gammax = 2.D0 *Cc *Sz *GAMMAF /alpha1 *(Bb +sqrtpi/2.D0 *Sy)

	if(cc.gt.clow) then
	    wlow = Dlog(cc/clow)
	    gamlow = gaminc(1.D0/alpha1, wlow ) * gamhi
	    table(j)= gamlow + 2.D0*clow*Sy*Sz/alpha1*series(wlow)
	    table(j)= DMIN1( table(j), gammax )
	endif

	if(cc.gt.chi) then
	    whi  = Dlog(cc/chi )
	    gamhi  = gaminc(1.D0/alpha1, whi  ) * gamhi
	    table(jj) = gamhi+ 2.D0*chi *Sy*Sz/alpha1*series(whi)
	    table(jj)= DMIN1( table(jj), gammax )
	endif
c
 460	continue
c
c... now, finish the integration
c
	DO 450 J = 2,II		! integrate in space at one value of time
	xx = (tdist(j,k1) - tdist(j-1,k1))
	arglow = (table(j) + table(j-1))/2.
	arghi  = (table(j+maxnob) + table(j-1+maxnob))/2.D0
	table(kk)  = arghi *xx + table(kk)
	table(kk1) = arglow*xx + table(kk1)
 450	continue
c
 430	continue
C
	RETURN
	END
c
c
c
	subroutine sortsi(xxx, ccstr, dccstr, prmt)
c
C............................................................
C
C   Subroutine that integrates downwind dispersion correction.
c
c............................................................
c
	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )
c
	include 'DEG3.prm'
C
	COMMON
     ./csigx/	sigxco,sigxp,sigxmd,sigxfl
     ./SORTa/	TCc(maxnob,maxnt),TCcSTR(maxnob,maxnt),
     .		Tyc(maxnob,maxnt),Trho(maxnob,maxnt),
     .		Tgamma(maxnob,maxnt),Ttemp(maxnob,maxnt)
     ./SORTb/	TSY(maxnob,maxnt),TSZ(maxnob,maxnt),TB(maxnob,maxnt),
     .		TDIST0(maxnob,maxnt),TDIST(maxnob,maxnt),KSUB(maxnt)
C
	real*4 tcc,tccstr,tyc,trho,tgamma,ttemp,tsy,tsz,tb,tdist0

	DIMENSION prmt(7)
c
c
	k1 = nint(prmt(7))
c
c... sort through the vectors to find the appropriate entries and linearly
c	interpolate between them for the values at XXX
c
	ii = ksub(k1)
	i = 2
 100	if(xxx .le. tdist(i,k1)) then
	    im = i - 1
	    frac = (xxx - tdist(i,k1))/(tdist(im,k1) - tdist(i,k1))
	    dist = frac*(tdist0(im,k1) - tdist0(i,k1)) + tdist0(i,k1)
	    dist = xxx - dist
	    cc   = frac*(tcc(im,k1) - tcc(i,k1)) + tcc(i,k1)
	    goto 200
	endif
	i = i + 1
	if(i .gt. ii) then	! upper limit can be jumped (due to rounding)
	    dist = xxx - tdist0(ii,k1)
	    cc   = tcc(ii,k1)
	    goto 200
	endif
	goto 100
 200	continue

	deltax = ABS(prmt(6) - xxx)
	SX = sigxco* DIST**sigxp
	ARG = (deltax/SX)**2/2.D0
C
	dccstr = 0.D0
c
c... Note that applying the Sx cutoff here can cause unacceptable
c	discontinuities - such as using...
c	if(dist .lt. sigxmd) return
c
	IF(ARG.EQ.0.) then
	    dccstr = cc/SX
	else IF(ARG.LE.80.) then
	    dccstr = cc/SX/EXP(ARG)
	endif
C
	return
	end
c
c
c
	subroutine sortso(x, y, d, ihlf, ndim, prmt)
c
C............................................................
C
C   Dummy subroutine
c
c............................................................
c
	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )
c
C
	return
	end
