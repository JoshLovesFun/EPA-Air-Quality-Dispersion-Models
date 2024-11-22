	SUBROUTINE SRTOUT(OPNRUP, table)
c
c...................................................................
c
c	Subroutine to output the time-sorted entries.
c
c...................................................................
c
	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	include 'DEG3.prm'
C
	COMMON
     ./alp/	alpha,alpha1
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
	dimension table(1)
c
	logical cflag,cflag1
c
	character*3 gasnam
	character*40 OPNRUP
C
	OPEN(UNIT=8, file=OPNRUP, status='UNKNOWN')
C
c... write the header
c
	WRITE(8,1100)
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
	    call adiabat(2,wc,wa,gasllc,ya,cc_lfl,r,w,t,tt)
	    call adiabat(2,wc,wa,gasulc,ya,cc_ufl,r,w,t,tt)
	endif
C
c... loop through each time
c
	DO 110 I=ISTRT,NTIM
C
c... write the header for each time
c
	WRITE(8,1119)
	WRITE(8,1119)
	WRITE(8,1110) TIM(I)
	if(cflag1) then
	    WRITE(8,1116) gaszzc,(100.*gasllc),(100.*gasulc)
	    WRITE(8,1118)
	else
	    WRITE(8,1115) gaszzc,(100.*gasllc),(100.*gasulc)
	    WRITE(8,1117)
	endif
	WRITE(8,1119)
	ip = 0
	II = KSUB(I)
c
	DO 120 J=1,II
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
c
	if(.not.cflag) then
	    call adiabat(-2,wc,wa,gasllc,ya,cc_lfl,r,w,gamma,tt)
	    call adiabat(-2,wc,wa,gasulc,ya,cc_ufl,r,w,gamma,tt)
	endif
c
	arg = (gaszzc/sz)**alpha1
	if(arg .ge. 80.) goto 600
c
	ccz = cc/exp(arg)
	if(ccz .lt. cc_lfl) then
	    if(cflag1) then
		WRITE(8,1120) DIST,yc,Cc,rho,temp,B,SZ,SY
	    else
		WRITE(8,1120) DIST,yc,Cc,rho,gamma,temp,B,SZ,SY
	    endif
	    goto 600
	endif
	arg = -(dlog(cc_lfl/cc) + (gaszzc/sz)**alpha1)
	blfl = sqrt(arg)*sy + b
c
	if(ccz .lt. cc_ufl) then
	    if(cflag1) then
		WRITE(8,1120) DIST,yc,Cc,rho,temp,B,SZ,SY,blfl
	    else
		WRITE(8,1120) DIST,yc,Cc,rho,gamma,temp,B,SZ,SY,blfl
	    endif
	    goto 600
	endif
	arg = -(dlog(cc_ufl/cc) + (gaszzc/sz)**alpha1)
	bufl = sqrt(arg)*sy + b
	if(cflag1) then
	    WRITE(8,1120) DIST,yc,Cc,rho,temp,B,SZ,SY,blfl,bufl
	else
	    WRITE(8,1120) DIST,yc,Cc,rho,gamma,temp,B,SZ,SY,blfl,bufl
	endif
c
  600	continue
	ip = ip + 1
	if(ip .eq. 3) then
	    ip = 0
	    write(8,1119)
	endif
  120	CONTINUE
c
c... summarize the mass above the UFL and LFL
c
	aufl = table(i+2*maxnob)
	alfl = table(i+2*maxnob+maxnt)
	write(8,8000) 100.*gasulc,100.*gasllc,alfl-aufl,alfl
 8000	format(//,' For the ULC of ',1pg13.5,' mole percent, and',
     .		' the LLC of ',1pg13.5,' mole percent:',//,
     .		' The mass of contaminant between the ULC and LLC is:'
     .		,1pg13.5,' kg.',/,
     .		' The mass of contaminant above the LLC is: ',
     .		1pg13.5,' kg.')
  110	CONTINUE
C
	endfile(unit=8)
	CLOSE(UNIT=8)
C
C
 1100	FORMAT(1H0,5X,'Sorted values for each specified time.')
 1102	format(1H0,5x,'X-Direction correction was NOT applied.')
 1104	format(1H0,5x,'X-Direction correction was applied.')
 1105	format(1h ,5x,5x,'Coefficient:      ',1pg13.5,/,
     .		1h ,5x,5x,'Power:            ',1pg13.5,/,
     .		1h ,5x,5x,'Minimum Distance: ',1pg13.5,' m')
 1110	FORMAT(1H0,5X,'Time after beginning of spill ',G14.7,' sec')
 1115	FORMAT(1H0,1X,'Distance',2x,3x,'Mole',3x,
     .		'Concentration',1x,'Density',2x,3x,'Gamma',3x,
     .		'Temperature',3x,'Half',4x,4x,'Sz',5x,4x,'Sy',5x,
     .		'Width at z=',0pf6.2,' m to:',/,1x,11x,1x,'Fraction',
     .		2x,11x,11x,11x,11x,3x,'Width',3x,11x,9x,
     .		2(1pg9.3,'mole%',1x))
 1116	FORMAT(1H0,1X,'Distance',2x,3x,'Mole',3x,
     .		'Concentration',1x,'Density',2x,
     .		'Temperature',3x,'Half',4x,4x,'Sz',5x,4x,'Sy',5x,
     .		'Width at z=',0pf6.2,' m to:',/,1x,11x,1x,'Fraction',
     .		2x,11x,11x,11x,3x,'Width',3x,11x,9x,
     .		2(1pg9.3,'mole%',1x))
 1117	FORMAT(1H ,4X,'(m)',4x,11x,
     .		2(1X,'(kg/m**3)',1x),11x,4x,'(K)',
     .		5(8X,'(m)'))
 1118	FORMAT(1H ,4X,'(m)',4x,11x,
     .		2(1X,'(kg/m**3)',1x),4x,'(K)',
     .		5(8X,'(m)'))
 1119	FORMAT(1H )
 1120	FORMAT(1H ,1pg10.3,1x,2(1X,1PG9.3,1X),2x,0pf7.4,2x,1X,1PG10.3,1X,
     .		6(1X,1PG9.3,1X))
C
	RETURN
	END
