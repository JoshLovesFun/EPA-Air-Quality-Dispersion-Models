	SUBROUTINE Surfac(temp,height,rho,wm,cp,yw,watrte,qrte)
c
c......................................................................
c
C	Surface heat (and water) transfer
c
C......................................................................
C

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	include 'DEG1.prm'
C
	COMMON
     ./ALP/	ALPHA,alpha1
     ./comatm/	istab,tamb,pamb,humid,isofl,tsurf,ihtfl,htco,iwtfl,wtco,
     .		humsrc
     ./parm/	u0,z0,zr,rml,ustar,vkc,gg,rhoe,rhoa,deltay,betay,gammaf,
     .		yclow
     ./phicom/	iphifl,dellay
C
c
	watrte = 0.D0
	qrte = 0.D0
	if(isofl.eq.1 .or. ihtfl.eq.0) return
	if(height.le. 0.D0) return
	deltem = tsurf - temp
c
c... calculate the heat transfer correlation
c
	if(deltem .lt. 0.D0) return
	hhh = dmax1( height/2.D0, zr)
	topvel = u0 * (hhh/z0)**alpha
	pnat = ((rho/wm)**2 * abs(deltem)) ** 0.333333333333D0
	if(ihtfl .eq. 1) then		! local correlation
	    hn = 18.D0 * pnat
	    hf = 0.D0
	    if(u0.ne.0.D0) hf = 1.22D0 * rho*cp * ustar**2/topvel
	    ho = dmax1(hn,hf)
	else if(ihtfl .eq. 2) then		! LLNL correlation
	    ho = htco* rho* cp
	else if(ihtfl .eq. 3) then		! Colenbrander's method
	    avtemp= (tsurf + temp)/2.D0
	    hn = 89.D0*(deltem/avtemp**2)**.333333333333D0
	    u10 = u0*(10.D0/z0)**alpha
	    hf = 0.D0
	    if(u0.ne.0.D0) hf = 1.22D0 * rho*cp * ustar**2/u10
	    ho = dmax1(hn,hf)
	else
	    ho = htco	! ihtfl=-1
	endif
	qrte = ho * deltem
	if(qrte .lt. 0.D0) qrte = 0.D0
c				! since correlations are not valid for qrte<0.
	watrte = 0.D0
	if(iwtfl .eq. 0) return
c
c... do the water transfer
c
	fo = wtco
	if(iwtfl .gt. 0) then
	    fn = 9.9D-3 * pnat
	    ff = 20.7D0 * ho /cp /wm
	    fo = dmax1(fn,ff)
	endif
	watrte = min( watvp(temp), yw*pamb )
	watrte = fo * (watvp(tsurf) - watrte)/pamb
	if(watrte .le. 0.D0) watrte = 0.D0
c
	return
	end
