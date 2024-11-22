	subroutine tprop(ifl,wc,wa,enth,yc,ya,wm,temp,rho,cp)
c
c..................................................................
c
c	subroutine to return:
c		mole fractions (y's)
c		molecular weight (wm)
c		temperature (temp[=]K)
c		density (rho[=]kg/m**3)
c		heat capacity (cp[=]J/kg/K)
c
c	for a mixture from:
c		mass fractions (w's)
c		temperature (K)		for ifl.lt.0
c
c	for a mixture from:
c		mass fractions (w's)
c		enthalpy (J/kg)		for ifl.ge.0
c
c		adiabatic mixing of:	emitted gas @ gastem
c					entrained ambient humid air @ tamb
c					entrained water	from surface @ tsurf
c				for ifl.eq.0 calculate and return
c
c		adiabatic lookup CALL ADIABAT
c				for isofl.eq.1 .or. ihtfl.eq.0.and.ifl.eq.1
c
c.......................................................................
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	include 'DEGin.prm'
c
	common
     ./cgprop/	gasmw,gastem,gasrho,gascpk,gascpp,gasulc,gasllc,gaszzc,
     .		gasnam
     ./comatm/	istab,tamb,pamb,humid,isofl,tsurf,ihtfl,htco,iwtfl,wtco,
     .		humsrc
     ./ctprop/	cwc,cwa,centh
     ./GEN2/	DEN(5,igen)
c
	character*3 gasnam
c
c
	parameter (acrit=0.001D0)
c
	external enth0
c
c... calculate mass/mole fractions
c
	ww = 1.D0-wc-wa
	wm = 1.D0/(wc/gasmw + wa/wma + ww/wmw)
	yc = wm/gasmw *wc
	ya = wm/wma *wa
	yw = 1.D0 - yc - ya
c
c... if an ISOTHERMAL simulation, use adiabat
c
	if(isofl.eq. 1) then
	    call adiabat(1,wc,wa,yc,ya,cc,rho,wm,enth,temp)
	    return		! interp density from wc
	endif
c
c... As appropriate, calculate the enthalpy
c
	if(ifl.eq. 0)
     .		enth = wc*cpc(gastem)*(gastem - tamb)
     .		+ (ww - wa*humid)*cpw*(tsurf - tamb)
c     .		+ wa*humid*cpw*(tamb - tamb)
c     .		+ wa*cpa*(tamb - tamb)		! TR=tamb
c
c
c... ADIABAT can also be used if the heat transfer is not included
c
	if(ifl.eq. 1 .and. ihtfl.eq.0) then
	    call adiabat(1,wc,wa,yc,ya,cc,rho,wm,enth,temp)
	    return		! interp density from wc
	endif
c
c... calculate the density only
c
	if(ifl .eq. -1) goto 400
c
c... determine the temperature of this mixture from the enthalpy
c
	tmin = min(gastem, tsurf, tamb)
	tmax = max(gastem, tsurf, tamb)

	elow = enthal(wc,wa,tmin)
	if(enth.lt. elow) then	! catch out of bounds numbers
	    temp = tmin
	    enth = elow
	    goto 400
	endif

	elow = enthal(wc,wa,tmax)
	if(enth.gt. elow) then
	    temp = tmax
	    enth = elow
	    goto 400
	endif
c
	cwc = wc
	cwa = wa
	centh = enth

	call zbrent(temp, enth0, tmin, tmax, acrit, ierr)
	if(ierr .ne. 0) call trap(24,0)
c
c... calculate the density
c
 400	continue		! density calculation
	ywsat = watvp(temp)/pamb
	wwsat = wmw/wm * ywsat * (ya+yc)/(1.D0-ywsat)
	conden = dmax1( 0.D0, ww-wwsat)

	rho = 1.D0/(	  wa / (pamb*wma/rgas/temp)
     .			+ (ww-conden) / (pamb*wmw/rgas/temp)
     .			+ conden / rhowl
     .			+ wc * temp/gastem/gasrho )
c
	tmin = temp + 10.D0
	tmax = enthal(wc,wa,tmin)

	cp = (enth - tmax)/(temp - tmin)
	if(cp .lt. cpa) cp = cpa		! nominal value of air
c
	return
	end
c
c...................................................................
c
	function cpc(temp)
c
c...................................................................
c
c	CPC determines the contaminant heat capacity
c
c...................................................................
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	include 'DEGin.prm'
c
	common
     ./cgprop/	gasmw,gastem,gasrho,gascpk,gascpp,gasulc,gasllc,gaszzc,
     .		gasnam
c
	character*3 gasnam
c
c
	data con/3.33D4/
c
c
	cpc = con + gascpk*gascpp * gastem**(gascpp-1.D0)

	if(temp .ne. gastem) then
	    cpc = con + gascpk*
     .		(temp**gascpp - gastem**gascpp)/(temp- gastem)
	endif
	cpc = cpc/gasmw
	return
	end
c
c....................................................................
c
	function enthal(wc,wa,temp)	! used by TPROP
c
c.....................................................................
c
c	ENTHAL calculates the mixture enthalpy
c
c.....................................................................
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )
c
	include 'DEGin.prm'
c
	common
     ./cgprop/	gasmw,gastem,gasrho,gascpk,gascpp,gasulc,gasllc,gaszzc,
     .		gasnam
     ./comatm/	istab,tamb,pamb,humid,isofl,tsurf,ihtfl,htco,iwtfl,wtco,
     .		humsrc
c
	character*3 gasnam
c
	parameter (deltaf=10.D0)
c
c
	ww = 1.D0-wa-wc
	wm = 1.D0/(wc/gasmw + wa/wma + ww/wmw)
	ya = wa * wm/wma
	yc = wc * wm/gasmw
	yw = 1.D0-ya-yc
c
c... DH is the combined latent heat of vaporization and fusion
c
	dh = dhvap
	frac = 0.
	if(temp .lt. 273.15D0) frac = dmin1( (273.15D0-temp)/deltaf,1.D0)
	dh = dhvap + dhfus*frac
	ywsat = watvp(temp)/pamb
	wwsat = wmw/wm * ywsat * (ya+yc)/(1.D0-ywsat)
	conden = dmax1( 0.D0, ww-wwsat)
c
 1000	enthal = wc*cpc(temp)*(temp - tamb)
     .		- conden*dh
     .		+ ww*cpw*(temp - tamb)
     .		+ wa*cpa*(temp - tamb)		! TR=tamb
c
	return
	end
c
c.......................................................................
c
	function enth0(temp)
c
c......................................................................
c
c	ENTH0 is used by TPROP to get the temperature of a mixture with
c	a given enthalpy
c
c......................................................................
c
	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	include 'DEGin.prm'
c
	common
     ./ctprop/	cwc,cwa,centh
c

	enth0 = centh - enthal(cwc,cwa,temp)

	return
	end
c
c.....................................................................
c
	function watvp(temp)
c
c......................................................................
c
c	WATVP calculates the vapor pressure of water (in atm) as a
c	function of temperature (in K).
c
c.....................................................................
c
	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )
c

c watvp = 6.0298D-3* exp(5407.D0 *(1.D0/273.15D0- 1.D0/temp))
c
	watvp = exp(14.683943D0 - 5407.D0/temp)

	return
	end
c
c..................................................................
c
	subroutine adiabat(ifl,wc,wa,yc,ya,cc,rho,wm,enthalpy,temp)
c
c..................................................................
c
c	subroutine to return:
c		mass fractions (w's)
c		mole fractions (y's)
c		concentration (cc[=]kg/m**3)
c		density (rho[=]kg/m**3)
c		molecular weight (wm)
c		enthalpy ([=]J/kg)
c		temperature (temp[=]K)
c
c	for a mixture from DEN lookup of adiabatic mixing calculation
c	den(1,i)	mole fraction (yc)
c	den(2,i)	concentration (cc [=] kg c/m**3)
c	den(3,i)	mixture density (rho [=] kg mix/m**3)
c	den(4,i)	mixture enthalpy (enthalpy [=] J/kg)
c	den(5,i)	mixture temperature (temp [=] K)
c
c	ifl indicates given information:
c	-2)mole fraction (Yc) and assumption of constant gamma in enthalpy
c	-1)concentration (cc) and assumption of constant gamma in enthalpy
c	0) concentration (cc)
c	1) mass fraction c (wc)
c	2) mole fraction (Yc)
c
c.....................................................................
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	include 'DEGin.prm'
c
c
	common
     ./cgprop/	gasmw,gastem,gasrho,gascpk,gascpp,gasulc,gasllc,gaszzc,
     .		gasnam
     ./comatm/	istab,tamb,pamb,humid,isofl,tsurf,ihtfl,htco,iwtfl,wtco,
     .		humsrc
     ./GEN2/	DEN(5,igen)
     ./parm/	u0,z0,zr,rml,ustar,vkc,gg,rhoe,rhoa,deltay,betay,gammaf,
     .		yclow
c
	character*3 gasnam
c
c
	if(ifl.ne. 0) goto 1000
	ccl = cc
	if(cc .lt. 0.D0) ccl=0.D0
	i = 2
 30	if(den(1,i) .gt. 1.D0) then
	    i=i-1
	    if(cc.gt. den(2,i)) ccl=den(2,i)
	    goto 50
	endif
	if(cc.le. den(2,i)) goto 50	! lookup in concentration
	i=i+1
	goto 30
 50	slope = (den(3,i)-den(3,i-1)) / (den(2,i)-den(2,i-1)) ! interp in conc
		rho = (ccl - den(2,i-1))*slope + den(3,i-1)
	wcl = ccl / rho
	wc = wcl
	wa = (1.D0-(1.D0+humsrc)*wc)/(1.D0+humid)
	ww = 1.D0 - wa - wc
	wm = 1.D0/(wc/gasmw + wa/wma + ww/wmw)
	yc = wm/gasmw * wc
	ya = wm/wma * wa
	goto 8000
c
c
 1000	if(ifl.ne. -1) goto 1500
	ccl = cc
	if(ccl.lt. 0.) ccl=0.
	gamma = enthalpy
	wc = ccl/(rhoa+ccl*gamma)
	wa = (1.D0-(1.D0+humsrc)*wc)/(1.D0+humid)
	ww = 1.D0-wa-wc
	wm = 1.D0/(wc/gasmw + wa/wma + ww/wmw)
	yc = wm/gasmw * wc
	ya = wm/wma * wa
	rho = ccl/wc
	return
c
c
 1500	if(ifl.ne. -2) goto 1700
	ycl = yc
	if(ycl.lt. 0.) ycl=0.D0
	gamma = enthalpy
	ya = (1.D0-(1.D0+gasmw*humsrc/wmw)*ycl)/(1.D0+humid*wma/wmw)
	yw = 1.D0-ya-ycl
	wm = ycl*gasmw + ya*wma + yw*wmw
	wc = gasmw/wm * ycl
	wa = wma/wm * ya
	cc = wc*rhoa/(1.D0 - gamma*wc)
	rho = cc/wc
	return
c
c
 1700	if(ifl.ne. 2) goto 2000
	ycl = yc
	if(yc .lt. 0.D0) then
	    ycl = 0.D0
	    wa = 1.D0/(1.D0+humid)
	    ww = 1.D0-wa
	    wm = 1.D0/(wma/wa + wmw/ww)
	    ya = wm/wma * wa
	endif
	if(yc .gt. 1.D0) then
	    ycl = 1.D0
	    ya = 0.D0
	endif
	i = 2
 1730	if(den(1,i) .gt. 1.D0) then
	    i = i-1
	    goto 1750		! extrapolate
	endif
	if(ycl.le. den(1,i)) goto 1750	! lookup in mole frac
	i=i+1
	goto 1730
 1750	continue
	wm = ycl*gasmw + (1.D0-ycl)*wma*wmw*(1.D0+humid)/
     .						(wmw + wma*humid)
	wc = ycl*gasmw / wm
	wa = (1.D0-wc)/(1.D0 + humid)
	ww = 1.D0 - wc - wa

	slope = (den(3,i)-den(3,i-1)) / (den(2,i)-den(2,i-1))	! interp in cc

	cc = wc*(den(3,i-1) - slope*den(2,i-1))/(1.D0 - wc*slope)
	rho = cc/wc

		w1 = den(2,i-1)/den(3,i-1)
		w2 = den(2,i)/den(3,i)
	slope = (den(4,i)-den(4,i-1)) / (w2 - w1)	! interp in w
		enthalpy = (wc - w1) *slope + den(4,i-1)
	slope = (den(5,i)-den(5,i-1)) / (w2 - w1)	! interp in w
		temp = (wc - w1) *slope + den(5,i-1)
c
	return
c
c
 2000	if(ifl.ne. 1) goto 9000
	wcl = wc
	if(wc .lt. 0.D0) then
	    wcl = 0.D0
	    wa = 1.D0/(1.D0+humid)
	endif
	if(wc .gt. 1.D0) then
	    wcl = 1.D0
	    wa = 0.D0
	endif
	ww = 1.D0-wa-wcl
	wm = 1.D0/(wcl/gasmw + wa/wma + ww/wmw)
	yc = wm/gasmw *wcl
	ya = wm/wma *wa
	i = 2
 2030	if(den(1,i) .gt. 1.D0) then
	    i = i-1
	    goto 2050		! extrapolate
	endif
	if(yc.le. den(1,i)) goto 2050	! lookup in mole frac
	i=i+1
	goto 2030
 2050	slope = (den(3,i)-den(3,i-1)) / (den(2,i)-den(2,i-1))
	rho   = (den(3,i-1) - den(2,i-1)*slope) / (1.D0 - slope*wcl)
	cc = rho * wcl
	i = 2
 2060	if(den(1,i) .gt. 1.D0) then
	    i = i-1
	    goto 8000		! extrapolate
	endif
	cwc = den(2,i)/den(3,i)
	if(wcl.le. cwc) goto 8000	! lookup in mass frac
	i=i+1
	goto 2060
c
c
 8000		w1 = den(2,i-1)/den(3,i-1)
		w2 = den(2,i)/den(3,i)
	slope = (den(4,i)-den(4,i-1)) / (w2 - w1)	! interp in w
		enthalpy = (wcl - w1) *slope + den(4,i-1)
	slope = (den(5,i)-den(5,i-1)) / (w2 - w1)	! interp in w
		temp = (wcl - w1) *slope + den(5,i-1)
c
	return
c
 9000	call trap(26,0)
	end
c
c......................................................................
c
	subroutine setent(hmrte,harte,hwrte)
c
c.......................................................................
c
c	subroutine to load /comh/ through passed arguments if needed
c
c......................................................................
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	include 'DEGin.prm'
c
	common
     ./cgprop/	gasmw,gastem,gasrho,gascpk,gascpp,gasulc,gasllc,gaszzc,
     .		gasnam
     ./comatm/	istab,tamb,pamb,humid,isofl,tsurf,ihtfl,htco,iwtfl,wtco,
     .		humsrc
c
	character*3 gasnam
c
c
	hmrte = 0.D0
	harte = 0.D0
	hwrte = 0.D0
c
	if(isofl.eq. 1) return
c
	hmrte = cpc(gastem)*(gastem - tamb)	! TR=tamb
c
c harte = cpa*(tamb - tamb) = 0.
c
	if(iwtfl .eq. 0) return
	hwrte = cpw*(tsurf - tamb)
c
	return
	end
c
c...........................................................................
c
	subroutine setden(wc,wa,enthalpy)
c
c...........................................................................
c
c	subroutine to load /GEN2/ as needed
c
c	adiabatic mixing of:	WC
c				WA
c				WW @ specified enthalpy
c
c		with ambient humid air @ tamb
c
c	den(1,i)	mole fraction (yc)
c	den(2,i)	concentration (cc [=] kg c/m**3)
c	den(3,i)	mixture density (rho [=] kg mix/m**3)
c	den(4,i)	mixture enthalpy (enthalpy [=] J/kg)
c	den(5,i)	mixture temperature (temp [=] K)
c
c.........................................................................
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	include 'DEGin.prm'
c
	common
     ./cgprop/	gasmw,gastem,gasrho,gascpk,gascpp,gasulc,gasllc,gaszzc,
     .		gasnam
     ./comatm/	istab,tamb,pamb,humid,isofl,tsurf,ihtfl,htco,iwtfl,wtco,
     .		humsrc
     ./GEN2/	DEN(5,igen)
c
	character*3 gasnam
c
c
	parameter (tcrit=0.002D0, zero=1.D-20)
	parameter (iils=200, ils=iils-1, iback=25)
c
c
	dimension curnt(5),backsp(5,iback)
c
c
	if(isofl.eq. 1) return
c
c
	k = 1
	den(1,k) = 0.0D0		! yc
	den(2,k) = 0.0D0		! cc
	den(3,k) = Pamb*(1.D0+humid)*wmw/(rgas*(wmw/wma + humid))/Tamb	!rhoa
	den(4,k) = 0.0D0		! enthalpy of ambient air; TR=tamb
	den(5,k) = tamb
c
c
	do 300 i= ils,1,-1
	zbda = (float(i)/float(iils)) / (1.D0+humid)
	zw = zbda*humid
	zg = 1.D0-zbda-zw
c
c	enmix = zg*enthalpy + zbda*cpa*(tamb-tamb) + zw*cpw*(tamb-tamb)	! TR=tamb
	enmix = zg*enthalpy
c
	zbda = zbda + zg*wa
	zg   =        zg*wc
	call tprop(2,zg,zbda,enmix,yc,ya,wm,temp,rho,cp)
	cc = zg*rho
c
c
	curnt(1) = yc
	curnt(2) = cc
	curnt(3) = rho
	curnt(4) = enmix
	curnt(5) = temp
c
	if(i .eq. ils) then
		ind = 1
		do 150 jj= 1,5
 150		backsp(jj,ind) = curnt(jj)
		goto 300
	endif
c
c	ADIABAT interpolation scheme
c
	err = 0.D0
	do 180 iind = 1,ind
	yc    = backsp(1,iind)
	cc    = backsp(2,iind)
	rho   = backsp(3,iind)
	enmix = backsp(4,iind)
	temp  = backsp(5,iind)
	slope = (den(2,k)- curnt(2)) / (den(1,k)- curnt(1))
		ccint = (yc - curnt(1))*slope + curnt(2)
	err = dmax1(err,2.D0* abs(cc - ccint)/(abs(cc + ccint) + zero))
	slope = (den(3,k)- curnt(3)) / (den(1,k)- curnt(1))
		rhoint = (yc - curnt(1))*slope + curnt(3)
	err = dmax1(err,2.D0* abs(rho - rhoint)/
     .			(abs(rho + rhoint) + zero))
	wccal = cc / rhoint
		w1 = curnt(2)/curnt(3)
		w2 = den(2,k)/den(3,k)
	slope = (den(4,k)- curnt(4)) / (w2 - w1)
		entint = (wccal - w1)*slope + curnt(4)
	err = dmax1(err,2.D0* abs(enmix - entint)/
     .				(abs(enmix + entint) + zero))
	slope = (den(5,k) - curnt(5)) / (w2 - w1)
		temint = (wccal - w1)*slope + curnt(5)
	err = dmax1(err,2.D0* abs(temp - temint)/
     .				(abs(temp + temint) + zero))
 180	continue
c
	if(err .le. tcrit) then
		if(ind .ge. iback) goto 200
		ind = ind + 1
		do 190 jj=1,5
 190		backsp(jj,ind) = curnt(jj)
		goto 300
	endif
c
c... record a point in DEN
c
 200	k = k+1
	if(k.ge. igen) call trap(28,0)
	do 250 jj=1,5
	den(jj,k) = backsp(jj,ind)
 250	backsp(jj,1) = curnt(jj)
	ind = 1
c
 300	continue
c
	k = k+1
	if(k.ge. igen) call trap(28,0)
	if(wc.eq. 1.D00) then
		den(1,k) = 1.D00	! yc
		den(2,k) = gasrho	! cc
		den(3,k) = gasrho	! rhoe
		den(4,k) = enthalpy	! enthalpy
		den(5,k) = gastem	! temp
	else
	call tprop(2,wc,wa,enthalpy,den(1,k),ya,wm,den(5,k),den(3,k),cp)
	den(2,k) = wc*den(3,k)	! cc
	den(4,k) = enthalpy
	endif
	den(1,k+1) = 2.D0		! .gt. 1. end-of-record indicator
c
	return
	end
c
c.......................................................................
c
	subroutine addheat(cc,dh,rho,temp,cp)
c
c......................................................................
c
c	ADDHEAT adds energy to the adiabatic mixing condition
c
c......................................................................
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	include 'DEGin.prm'
c
	common
     ./cgprop/	gasmw,gastem,gasrho,gascpk,gascpp,gasulc,gasllc,gaszzc,
     .		gasnam
     ./comatm/	istab,tamb,pamb,humid,isofl,tsurf,ihtfl,htco,iwtfl,wtco,
     .		humsrc
     ./ctprop/	cwc,cwa,centh
     ./GEN2/	DEN(5,igen)
c
	character*3 gasnam
c
c
	parameter (acrit=0.001D0)
c
	external enth0
c
c
	cp = cpa
	rhoa = den(3,1)
c
	call adiabat(0,wc,wa,yc,ya,cc,rho,wm,enth,amt)
	ww = 1.D0 - wc - wa
	yw = 1.D0 - yc - ya
	temp = amt
	IF(isofl.eq.1 .or. ihtfl.eq.0) return	! adiabatic mixing is valid
	if(dh.le. 0.) return		! catch colder surface temperatures
	enth = enth + dh
c
c... Check for positive enthalpy
c
	if(enth.gt. 0.) then
	    temp = tamb
	    goto 400
	endif
c
c... Set the maximum temperature
c
	tmax = max(gastem, tsurf, tamb)
	ehi = enthal(wc,wa,tmax)
	if(enth.gt. ehi) then
	    temp = tmax
	    goto 400
	endif
c
c... The minimum temperature is the adiabatic mixing temperature.
c
	tmin = amt

	cwc = wc
	cwa = wa
	centh = enth
c
	call zbrent(temp, enth0, tmin, tmax, acrit, ierr)

c
c... Check to see if there are troubles.  LIMIT keeps ZBRENT bounded, but it
c	is expensive.
c
	if(ierr .ne. 0) then
	   tinc = (tmin+tmax)/100.D0
	   temp = tmin
	   tmin = tmin/2.D0
	   tmax = 2.D0*tmax
	   call limit(enth0, temp, tinc, tmax, tmin)
	   call zbrent(temp, enth0, tmin, tmax, acrit, ierr)
		if(ierr .eq. 0) goto 400
	   call trap(17,0)
	endif
c
c... Density calculation
c
 400	continue

	ywsat = watvp(temp)/pamb
	wwsat = wmw/wm * ywsat * (ya+yc)/(1.D0-ywsat)
	conden = dmax1( 0.D0, ww-wwsat)

	rho = 1.D0/(	  wa / (pamb*wma/rgas/temp)
     .			+ (ww-conden) / (pamb*wmw/rgas/temp)
     .			+ conden / rhowl
     .			+ wc * temp/gastem/gasrho )

	if(temp.ne.amt) cp = dmax1(dh/(temp-amt),cpa)
c
c
	return
	end
