	SUBROUTINE dosOUT
c
c.............................................................................
c
c	Subroutine to output the Concentration time histories.  Output
c	is for a single downwind distance at one time.
c
c.............................................................................
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	include 'DEG4.prm'
C
	COMMON
     ./alp/	alpha,alpha1
     ./cdos/	idos, dosx(ndos), dosyz(4,2,ndos)
     ./cgprop/	gasmw,gastem,gasrho,gascpk,gascpp,gasulc,gasllc,gaszzc,
     .		gasnam
     ./comatm/	istab,tamb,pamb,humid,isofl,tsurf,ihtfl,htco,iwtfl,wtco,
     .		humsrc
     ./csigx/	sigxco,sigxp,sigxmd,sigxfl
     ./oomsin/	oodist,avtime
     ./SORTa/	TCc(maxnob,maxnt),TCcSTR(maxnob,maxnt),
     .		Tyc(maxnob,maxnt),Trho(maxnob,maxnt),
     .		Tgamma(maxnob,maxnt),Ttemp(maxnob,maxnt)
     ./SORTb/	TSY(maxnob,maxnt),TSZ(maxnob,maxnt),TB(maxnob,maxnt),
     .		TDIST0(maxnob,maxnt),TDIST(maxnob,maxnt),KSUB(maxnt)
     ./SORTIN/	TIM(maxnt),NTIM,ISTRT
C
	real*4 tcc,tccstr,tyc,trho,tgamma,ttemp,tsy,tsz,tb,tdist0
	dimension chist(5,maxnt)
c
	logical cflag,cflag1
c
	character*3 gasnam
C
c... Write the main heading
c
	if(sigxfl.eq. 0.) then
	    write(8,1102)
	else
	    write(8,1104)
	    write(8,1105) sigxco,sigxp,sigxmd
	endif
c
	cflag = isofl.eq. 1.or. ihtfl.eq. 0
	cflag1= isofl.eq.1
	if(cflag) then
	    call adiabat(2,wc,wa,gasllc,ya,cclfl,r,w,t,tt)
	    call adiabat(2,wc,wa,gasulc,ya,ccufl,r,w,t,tt)
	endif
C
	WRITE(8,1110) dosx(idos)
c
	WRITE(8,1119)
	WRITE(8,1119)
	if(cflag1) then
	    WRITE(8,1116) gaszzc,(100.*gasllc),(100.*gasulc)
	    WRITE(8,1118)
	else
	    WRITE(8,1115) gaszzc,(100.*gasllc),(100.*gasulc)
	    WRITE(8,1117)
	endif
	WRITE(8,1119)
	ip = 0
c
c... output the time history of the centerline..............................
c
	DO 110 I=ISTRT,NTIM
C
	II = KSUB(I)
c
	j = 1
	disto  = tdist(j,i) + oodist
	cco    = tccstr(j,i)
	rhoo   = Trho(j,i)
	yco    = Tyc(j,i)
	tempo  = Ttemp(j,i)
	gammao = Tgamma(j,i)
	bo     = tb(j,i)
	szo    = tsz(j,i)
	syo    = tsy(j,i)
	if( disto .gt. dosx(idos) ) then
	    write(lunlog,*) ' Extrapolated point for time: ',Tim(i)
	    write(8,3000) tim(i)
 3000	    format(' Records for ',1pg13.5,
     .			' s are missing - see source')
	    goto 110
	endif
c
	DO 120 J=2,II
c
	dist  = tdist(j,i) + oodist
	cc    = tccstr(j,i)
	rho   = Trho(j,i)
	yc    = Tyc(j,i)
	temp  = Ttemp(j,i)
	gamma = Tgamma(j,i)
	b     = tb(j,i)
	sz    = tsz(j,i)
	sy    = tsy(j,i)
	blfl  = 0.
	bufl  = 0.

	if( dist .lt. dosx(idos) ) goto 119
	dist  = (dosx(idos) - disto)/(dist-disto)
	cc    = dist*(cc-cco) + cco
	rho   = dist*(rho-rhoo) + rhoo
	yc    = dist*(yc-yco) + yco
	temp  = dist*(temp-tempo) + tempo
	gamma = dist*(gamma-gammao) + gammao
	b     = dist*(b-bo) + bo
	sz    = dist*(sz-szo) + szo
	sy    = dist*(sy-syo) + syo
c
	if(.not.cflag) then
	    call adiabat(-2,wc,wa,gasllc,ya,cclfl,r,w,gamma,tt)
	    call adiabat(-2,wc,wa,gasulc,ya,ccufl,r,w,gamma,tt)
	endif
c
c... calculate the concentration time histories
c
	chist(1,i) = yc
	do 300 ij=1,4
	chist(ij+1,i) = 0.
	if(dosyz(ij,1,idos).gt.0.D0 .or. dosyz(ij,2,idos).gt.0.D0) then
	    arg = (dosyz(ij,2,idos)/sz)**alpha1
	    if(dosyz(ij,1,idos) .gt. b) arg = arg + 
     .			((dosyz(ij,1,idos)-b)/sy)**2
	    if(arg .lt. 80.d0) then
		chist(ij+1,i) = cc/exp(arg)
		if(cflag) then
		    call adiabat(0,xx,aa,ycc,yaa,chist(ij+1,i),rr,ww,gg,tt)
		else
		    call adiabat(-1,xx,aa,ycc,yaa,chist(ij+1,i),rr,ww,gamma,tt)
		endif
		chist(ij+1,i) = ycc
	     endif
	endif
  300	continue
c
c... write each line with the appropriate footprint included
c
	arg = (gaszzc/sz)**alpha1
	if(arg .ge. 80.) then
	    if(cflag1) then
		WRITE(8,1120) tim(I),yc,Cc,rho,temp,B,SZ,SY
	    else
		WRITE(8,1120) tim(I),yc,Cc,rho,gamma,temp,B,SZ,SY
	    endif
	    goto 121
	endif
c
	ccz = cc/exp(arg)
	if(ccz .lt. cclfl) then
	    if(cflag1) then
		WRITE(8,1120) tim(I),yc,Cc,rho,temp,B,SZ,SY
	    else
		WRITE(8,1120) tim(I),yc,Cc,rho,gamma,temp,B,SZ,SY
	    endif
	    goto 121
	endif
	arg = -(dlog(cclfl/cc) + (gaszzc/sz)**alpha1)
	blfl = sqrt(arg)*sy + b
c
	if(ccz .lt. ccufl) then
	    if(cflag1) then
		WRITE(8,1120) tim(I),yc,Cc,rho,temp,B,SZ,SY,blfl
	    else
		WRITE(8,1120) tim(I),yc,Cc,rho,gamma,temp,B,SZ,SY,blfl
	    endif
	    goto 121
	endif
	arg = -(dlog(ccufl/cc) + (gaszzc/sz)**alpha1)
	bufl = sqrt(arg)*sy + b
	if(cflag1) then
	    WRITE(8,1120) tim(I),yc,Cc,rho,temp,B,SZ,SY,blfl,bufl
	else
	    WRITE(8,1120) tim(I),yc,Cc,rho,gamma,temp,B,SZ,SY,blfl,bufl
	endif
	goto 121
c
  119	continue
	disto  = tdist(j,i) + oodist
	cco    = tccstr(j,i)
	rhoo   = Trho(j,i)
	yco    = Tyc(j,i)
	tempo  = Ttemp(j,i)
	gammao = Tgamma(j,i)
	bo     = tb(j,i)
	szo    = tsz(j,i)
	syo    = tsy(j,i)
  120	CONTINUE
	goto 110
c
  121	ip = ip + 1
	if(ip .eq. 3) then
	    ip = 0
	    write(8,1119)
	endif
  110	CONTINUE
C
c... output the concentration time histories as appropriate
c
	write(8,1119)
	write(8,1119)
	iii = 0
	do 700 ij=1,4
	if(dosyz(ij,2,idos).gt.0.D0 .or. 
     .			dosyz(ij,1,idos).gt.0.D0) iii = ij
  700	continue

	if(iii .eq. 0) return
	iii = iii+1
	ip = 0
c
c... write the heading
c
	write(8,2100) ((dosyz(ij,iij,idos),ij=1,4),iij=1,2)
 2100	format(1H0,11x,'Time',11x,5(4x,'Mole fraction at:',5x),/,
     .		1H ,26x,9x,'y= 0.0 m',9x,4(4x,'y= ',1pg13.5,' m',4x),/,
     .		1H ,11x,'(s)',12x,9x,'z= 0.0 m',9x,4(4x,'z= ',1pg13.5,' m',4x))
	DO 800 I=ISTRT,NTIM
	write(8,2200) tim(i),(chist(ij,i),ij=1,iii)
	ip = ip + 1
	if(ip .eq. 3) then
	    ip = 0
	    write(8,1119)
	endif
  800	continue
C
C
 1102	format(1H0,5x,'X-Direction correction was NOT applied.')
 1104	format(1H0,5x,'X-Direction correction was applied.')
 1105	format(1h ,5x,5x,'Coefficient:      ',1pg13.5,/,
     .		1h ,5x,5x,'Power:            ',1pg13.5,/,
     .		1h ,5x,5x,'Minimum Distance: ',1pg13.5,' m')
 1110	FORMAT(1H0,5X,'Centerline values for the position -->',/,
     .		' x: ',1pg13.5,' m',/)
 1115	FORMAT(1H0,1X,'  Time  ',2x,3x,'Mole',3x,
     .		'Concentration',1x,'Density',2x,3x,'Gamma',3x,
     .		'Temperature',3x,'Half',4x,4x,'Sz',5x,4x,'Sy',5x,
     .		'Width at z=',0pf6.2,' m to:',/,1x,11x,1x,'Fraction',
     .		2x,11x,11x,11x,11x,3x,'Width',3x,11x,9x,
     .		2(1pg9.3,'mole%',1x))
 1116	FORMAT(1H0,1X,'  Time  ',2x,3x,'Mole',3x,
     .		'Concentration',1x,'Density',2x,
     .		'Temperature',3x,'Half',4x,4x,'Sz',5x,4x,'Sy',5x,
     .		'Width at z=',0pf6.2,' m to:',/,1x,11x,1x,'Fraction',
     .		2x,11x,11x,11x,3x,'Width',3x,11x,9x,
     .		2(1pg9.3,'mole%',1x))
 1117	FORMAT(1H ,4X,'(s)',4x,11x,
     .		2(1X,'(kg/m**3)',1x),11x,4x,'(K)',
     .		5(8X,'(m)'))
 1118	FORMAT(1H ,4X,'(s)',4x,11x,
     .		2(1X,'(kg/m**3)',1x),4x,'(K)',
     .		5(8X,'(m)'))
 1119	FORMAT(1H )
 1120	FORMAT(1H ,3(1X,1PG9.3,1X),2x,0pf7.4,2x,1X,1PG10.3,1X,
     .		6(1X,1PG9.3,1X))
 2200	format(1h ,6(6x,1pg14.7,6x))
C
	RETURN
	END
