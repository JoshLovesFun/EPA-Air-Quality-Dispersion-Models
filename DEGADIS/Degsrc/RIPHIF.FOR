C...............................................................
c
c	Routines to calculate Richardson numbers and PHI functions
c
c................................................................
C
	FUNCTION RIF(RHOG,HEFF)
c
c.................................................................
c
C	RICHARDSON NUMBER (RI*)
C
c.................................................................
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )
C
	common
     ./parm/	u0,z0,zr,rml,ustar,vkc,gg,rhoe,rhoa,deltay,betay,gammaf,
     .		yclow
C
c
	RIF = gg*(RHOG-RHOA)/RHOA*HEFF/USTAR/USTAR
C
	RETURN
	END
c
c.............................................................
c
	FUNCTION RIFt(temp,HEFF)
c
C............................................................
C
C	RICHARDSON NUMBER (RIt)
C
c............................................................
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

C
	common
     ./ALP/	ALPHA,alpha1
     ./comatm/	istab,tamb,pamb,humid,isofl,tsurf,ihtfl,htco,iwtfl,wtco,
     .		humsrc
     ./parm/	u0,z0,zr,rml,ustar,vkc,gg,rhoe,rhoa,deltay,betay,gammaf,
     .		yclow
C
C
	wind = u0*(heff/z0)**alpha
	RIFt = dmax1(gg*(tsurf-temp)/temp*HEFF/USTAR/wind,0.D0)
C
	RETURN
	END
C
C....................................................................
C
	FUNCTION PHIF(RI,rit)
c
c.....................................................................
c
C	PHI FUNCTION
C
c....................................................................
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

c
	COMMON
     ./phicom/	iphifl,dellay
C
	phif= 0.
	goto(10,1000,2000,3000,4000,9000),iphifl
	goto 9000
c
  10	IF(RI) 100,200,300
C
 100	PHIF = 0.74D0/(1.D0 + 0.65D0*ABS(RI)**.6D0)
	RETURN
C
 200	PHIF = 0.74D0
	RETURN
C
 300	PHIF = 0.74D0 + 0.25D0*(RI)**0.7D0 + 1.2D-7*RI*RI*RI
	RETURN
c
c
 1000	IF(RI) 1100,1200,1300
C
 1100	PHIF = 0.88D0/(1.D0 + 0.65D0*ABS(RI)**.6D0)
	RETURN
C
 1200	PHIF = 0.88D0
	RETURN
C
 1300	PHIF = 0.88D0 + 9.9D-2*(RI)**1.04D0 + 1.4D-25*RI**5.7D0
	RETURN
c
c
c
 2000	corr1 = 0.25D0* rit**.666666D0 + 1.D0
	corr = sqrt(corr1)
	riw = ri/corr1
	IF(RI) 2100,2200,2300
C
 2100	PHIF = 0.88D0/(1.D0 + 0.65D0*ABS(RIw)**.6D0)/corr
	RETURN
C
 2200	PHIF = 0.88D0/corr
	RETURN
C
 2300	PHIF = (0.88D0 + 9.9D-2*(RIw)**1.04D0 + 1.4D-25*RIw**5.7D0)/corr
	RETURN
c
c
 3000	corr1 = 0.25D0* rit**.666666D0 + 1.D0
	corr = sqrt(corr1)
	riw = ri/corr1
	IF(RI) 3100,3200,3300
C
 3100	PHIF = 0.88D0/corr
	RETURN
C
 3200	PHIF = 0.88D0/corr
	RETURN
C
 3300	PHIF = (0.88D0 + 9.9D-2*(RIw)**1.04D0 + 1.4D-25*RIw**5.7D0)/corr
	RETURN
c
c
 4000	PHIF = 0.88D0
	RETURN
c
c
 9000	call trap(29,0)
	return
	END
c
c...........................................................
c
	function phihat(rho,fetch)
c
c..........................................................
c
c	Function to calculate PHIHAT for QSTRMX calculations
c
c..........................................................
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

C
	COMMON
     ./ALP/	ALPHA,alpha1
     ./parm/	u0,z0,zr,rml,ustar,vkc,gg,rhoe,rhoa,deltay,betay,gammaf,
     .		yclow
     ./phicom/	iphifl,dellay
c
c
	data phic/3.1D0/
c
	if(rho .le. rhoa) then
	    phihat = 0.88D0
	    return
	endif
c
	pow = 1.D0/alpha1
	p1 = 1.04D0/alpha1
	p1i = 1.D0/p1
	p2 = 1.D0 + p1i
	p3 = (alpha - 0.04D0)/1.04D0
	p4 = (1.08D0 - alpha)/1.04D0
	Ci = gg*(rho-rhoa)/rhoa*z0/ustar**2*gammaf/alpha1
	Ci =Ci* (vkc*ustar*alpha1**2 /u0/z0/ phic*dellay/
     .					(dellay-1.D0)) ** pow
c
	Ri = Ci*fetch**pow
	zzz =  - 0.099D0*Ri**1.04D0/0.88D0
c
	if(abs(zzz) .lt. 1.D0) then
	    phihat = 0.88D0/gseries(1.D0, p1i, p2, zzz)
	else
	    zzz = 1.D0/zzz
	    phihat = 0.88D0/(-zzz/(1.D0-p1)*gseries(1.D0, -p3, p4, zzz)
     .		+ gamma(p1i)*gamma(p4)/(p1-1.D0)* (-zzz)**(p1i))
	endif
c
	return
	end

c
c..................................................................
c
	function gseries( aaa, bbb, ccc, zzz)
c
c.................................................................
c
c	function to calculate the hypergeometric series for PHIHAT
c
c.................................................................
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	data crit/7.D-5/
c
c... check "zzz" to see if it is greater than 0.999.  If so, calculate
c	"gseries" for "zzz"=0.999 to avoid excessive execution times.
c
	if( abs(zzz) .gt. 0.999D0) then
	    zzzz = sign(0.999D0, zzz)
	else
	    zzzz = zzz
	endif

	term = aaa*bbb/ccc*zzzz
	sumo = 1.D0 + term

	do 100 k = 1,100000
	   rk = k
	   term = term * (aaa+rk)*(bbb+rk)/(ccc+rk) * zzzz/(rk+1.D0)
	   sum = sumo + term
	   if(abs(term/sum) .le. crit) goto 1000
	   sumo = sum
 100	continue

	write(6,*) 'GSERIES? did not converge in RIPHIF'
	stop

 1000	gseries = (sum + sumo)/2.D0
	return
	end
