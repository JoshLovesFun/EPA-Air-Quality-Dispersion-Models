	PROGRAM DEG4
C
C***************************************************************************
C***************************************************************************
C***************************************************************************
C
C	Program description:
C
C	DEG4 sorts the downwind dispersion calculation made for each of
C	the several observers in DEG2.  The output concentrations at
C	several given times may then be corrected for along-wind dispersion
C	as desired.  DEG4 also outputs the concentration as a function
C	of time at a given position.
C
C
C	Program usage:
C
C	Consult Volume III of the Final Report to U. S. Coast Guard
C	contract DT-CG-23-80-C-20029 entitled "Development of an
C	Atmospheric Dispersion Model for Heavier-than-Air Gas Mixtures".
C
C	J. A. Havens
C	T. O. Spicer
C
C	University of Arkansas
C	Department of Chemical Engineering
C	Fayetteville, AR 72701
C
C	April 1985
C
C
C	This project was sponsored by the U. S. Coast Guard and the Gas
C	Research Institute under contract DT-CG-23-80-C-20029.
C
C
C	Disclaimer:
C
C	This computer code material was prepared by the University of
C	Arkansas as an account of work sponsored by the U. S. Coast Guard
C	and the Gas Research Institute.  Neither the University of Arkansas,
C	nor any person acting on its behalf:
C
C	a.  Makes any warranty or representation, express or implied,
C	    with respect to the accuracy, completeness, or usefulness
C	    of the information contained in this computer code material,
C	    or that the use of any apparatus, method, numerical model,
C	    or process disclosed in this computer code material may not
C	    infringe privately owned rights; or
C
C	b.  Assumes any liability with respect to the use of, or for
C	    damages resulting from the use of, any information,
C	    apparatus, method, or process disclosed in this computer
C	    code material.
C
C
C***************************************************************************
C***************************************************************************
C***************************************************************************
C
c
C

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	include 'DEG4.prm'
C
	parameter (ntab0=2*maxnob+2*maxnt)
C
	COMMON
     ./ALP/	ALPHA,alpha1
     ./cdos/	idos, dosx(ndos), dosyz(4,2,ndos)
     ./cgprop/	gasmw,gastem,gasrho,gascpk,gascpp,gasulc,gasllc,gaszzc,
     .		gasnam
     ./CNOBS/	NOBS
     ./comatm/	istab,tamb,pamb,humid,isofl,tsurf,ihtfl,htco,iwtfl,wtco,
     .		humsrc
     ./csigx/	sigxco,sigxp,sigxmd,sigxfl
     ./ERROR/	ERT1,ERDT,ERNTIM
     ./GEN2/	DEN(5,igen)
     ./ITI/	T1,TINP,TSRC,TOBS,TSRT
     ./NEND/	POUND
     ./oomsin/	oodist,avtime
     ./PARM/	U0,Z0,ZR,rml,USTAR,vkc,gg,RHOE,RHOA,DELTAy,BETAy,GAMMAF,
     .		yclow
     ./PARMSC/	RM,SZM,EMAX,RMAX,TSC1,ALEPH,TEND
     ./PHLAG/	CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
     ./SORTa/	TCc(maxnob,maxnt),TCcSTR(maxnob,maxnt),
     .		Tyc(maxnob,maxnt),Trho(maxnob,maxnt),
     .		Tgamma(maxnob,maxnt),Ttemp(maxnob,maxnt)
     ./SORTb/	TSY(maxnob,maxnt),TSZ(maxnob,maxnt),TB(maxnob,maxnt),
     .		TDIST0(maxnob,maxnt),TDIST(maxnob,maxnt),KSUB(maxnt)
     ./SORTIN/	TIM(maxnt),NTIM,ISTRT
     ./SSCON/	NREC(maxnob,2),T0(maxnob),XV(maxnob)
C
	LOGICAL CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
	real*4 tcc,tccstr,tyc,trho,tgamma,ttemp,tsy,tsz,tb,tdist0

	DIMENSION TABLE(ntab0)
c
	character*24 tsrc,tinp,tobs,tsrt
C
	character*3 gasnam
	character*4 TR3,PSD,Er3,SR4
	character*40 opnf1
	character opnf(40)
C
	EQUIVALENCE (opnf(1),opnf1)

C.........................................................
C
C	DATA
C
	DATA POUND/-1.D-20/
	DATA TCc/maxtnob*0./,TCcSTR/maxtnob*0./,TSY/maxtnob*0./
	data TSZ/maxtnob*0./,KSUB/maxnt*0/
	DATA TB/maxtnob*0./,TDIST0/maxtnob*0./,TDIST/maxtnob*0.D0/
C
	DATA TR3/'.TR3'/,PSD/'.PSD'/
	DATA Er3/'.Er3'/,SR4/'.SR4'/
C
C... UNITS:
c
C	8 -- OUTPUT TO A PRINT FILE
C	9 -- I/O WITH DISK
C
	T1 = SECNDS(0.)
	call dattim(tsrt)
C
C... GET THE FILE NAME FOR FILE CONTROL
C
	call runname(opnf1)
	nchar = index(opnf1,' ') - 1
C
	opnf1 = opnf1(1:nchar) // tr3(1:4)
	CALL STRT3(opnf1)
C
	opnf1 = opnf1(1:nchar) // er3(1:4)
	CALL ESTRT3(opnf1)
C
c... input the positions to calculate the concentration histograms.............
c
	write(6,2000) ndos
 2000	format(//,' Concentration time histories can be generated ',
     .			'at a maximum of ',i3,/,
     .			' downwind distances; time histories at up to ',
     .			'4 off-centerline positions can',/,
     .			' be output at each distance.',/)
	write(6,2100)
 2100	format(' Enter the number of downwind distances desired: ',$)
	read(5,*) jdos
	if(jdos .gt. ndos) jdos=ndos
c
c... loop through the distances and input the desired positions
c
	write(6,2150)
 2150	format(//,' For each x coordinate, the centerline ',
     .			'concentration time history is',/,
     .			' automatically output.  Concentration ',
     .			'time histories at off-centerline',/,
     .			' positions can be determined; the ',
     .			'off-centerline positions are entered',/,
     .			' as y and z coordinate pairs.',/)
	do 200 ii=1,jdos
	write(6,2200)
 2200	format(/,' Enter the x coordinate: ',$)
	read(5,*) dosx(ii)
	write(6,2300)
 2300	format(//,' Enter up to four (y,z) coordinate pairs ',
     .			'at this distance with a carriage return',/,
     .			' between pairs; enter 0,0 to go to',
     .			' the next x coordinate: ',$)
	do 100 ij=1,4
	read(5,*) (dosyz(ij,jj,ii),jj=1,2)
	if(dosyz(ij,2,ii).le.0. .and.
     .			dosyz(ij,1,ii).le.0.) goto 200
  100	continue
  200	continue
c
c... open the output file
c
	opnf1 = opnf1(1:nchar) // SR4(1:4)
	open(unit=8, file=opnf1, status='UNKNOWN')
c
c... loop through each downwind distance
c
	do 500 idos=1,jdos
C
C... TIME SORT SUPERVISOR -- CALCULATE DOWNWIND DISPERSION CORRECTION....
C
	opnf1 = opnf1(1:nchar) // psd(1:4)
	OPEN(UNIT=9, file=opnf1, status='OLD')
c
	do 300 ij=1,maxnt
	KSUB(ij) = 0
	do 300 ijk=1,maxnob
	TSZ(ijk,ij) = 0.
  300	continue
c
	CALL SORTS(TABLE)
C
	CLOSE(UNIT=9)
C
C... OUTPUT SORTED PARAMETERS
C
	CALL dosOUT
  500	continue
c
	endfile(unit=8)
	close(unit=8)
	STOP
	END
