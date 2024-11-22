	PROGRAM DEG2
C
C***************************************************************************
C***************************************************************************
C***************************************************************************
C
C	Program description:
C
C	DEG2 performs the downwind dispersion portion of the calculation
C	for each of several observers released successively over the transient
C	gas source described by DEG1.
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
C
C
C	DIMENSIONS/DECLARATIONS
C
C

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	include 'DEG2.prm'
c
	COMMON
     ./ALP/	ALPHA,alpha1
     ./cedge/	T0L, direc
     ./cgprop/	gasmw,gastem,gasrho,gascpk,gascpp,gasulc,gasllc,gaszzc,
     .		gasnam
     ./CNOBS/	NOBS
     ./comatm/	istab,tamb,pamb,humid,isofl,tsurf,ihtfl,htco,iwtfl,wtco,
     .		humsrc
     ./csigx/	sigxco,sigxp,sigxmd,sigxfl
     ./csprd/	ce, delrmn
     ./ERROR/	SY0ER,ERRO,SZ0ER,WTAIO,WTQOO,WTSZO,ERRP,SMXP,WTSZP,
     .		WTSYP,WTBEP,WTDH,ERRG,SMXG,ERTDNF,ERTUPF,WTRUH,WTDHG
     ./GEN1/	PTIME(igen), ET(igen), R1T(igen), PWC(igen), PTEMP(igen),
     .		PFRACV(igen), PENTH(igen), PRHO(igen)
     ./GEN2/	DEN(5,igen)
     ./GEN3/	radg(2,maxl),qstr(2,maxl),srcden(2,maxl),srcwc(2,maxl),
     .		srcwa(2,maxl),srcen(2,maxl)
     ./ITI/	T1,TINP,TSRC,TOBS,TSRT
     ./NEND/	POUND
     ./oomsin/	oodist,avtime
     ./PARM/	U0,Z0,ZR,RML,USTAR,vkc,gg,RHOE,RHOA,DELTA,BETA,GAMMAF,
     .		yclow
     ./PARMSC/	RM,SZM,EMAX,RMAX,TSC1,ALEPH,TEND
     ./phicom/	iphifl,dellay
     ./PHLAG/	CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
     ./SSCON/	NREC(maxnob,2),T0(maxnob),XV(maxnob)
     ./STOPIT/	TSTOP
     ./STP/	STPO,STPP,ODLP,ODLLP,STPG,ODLG,ODLLG
     ./TITL/	TITLE
c
	character*80 TITLE(4)
c
	character*24 TINP,TSRC,TOBS,TSRT
	character*3 gasnam
c
	real*4 tt1
	LOGICAL CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
C
	character*4 TR2,ER2,PSD,TR3, obs

	character*1 opnf(80)
	character*80 opnf1
	equivalence (opnf(1),opnf1)
C
C..........................................................
C
C	DATA
C
	DATA TSTOP/0./
	DATA POUND/-1.E-20/
C
	DATA RADG/maxl2*0./,QSTR/maxl2*0./,srcden/maxl2*0./
	DATA NREC/maxnob*0,maxnob*0/,T0/maxnob*0./,XV/maxnob*0./
C
	DATA TR2/'.TR2'/,ER2/'.ER2'/
	DATA PSD/'.PSD'/,TR3/'.TR3'/, obs/'.OBS'/
C
C... Get the run name and timing..........................................
C
	T1 = SECNDS(0.)
C
	call runname(opnf1)
	nchar = index(opnf1,' ') - 1
C
	opnf1 = opnf1(1:nchar) // er2(1:4)
	CALL ESTRT2(opnf1)
C
C... GET THE COMMON VARIABLES CARRIED FROM DEGADIS1
C
	opnf1 = opnf1(1:nchar) // tr2(1:4)
	CALL STRT2(opnf1,Hmrte)
c
	call dattim(tobs)
C
c/////////////////////////////////////////////////////////////////////////////
C... PSEUDO STEADY STATE CALCULATIONS INTEGRATION IN SUBROUTINE SUPERVISOR
c\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
C
	opnf1 = opnf1(1:nchar) // psd(1:4)
	OPEN(UNIT=9, file=opnf1, status='UNKNOWN')
c
	opnf1 = opnf1(1:nchar) // obs(1:4)
	OPEN( UNIT=12, file=opnf1, status='UNKNOWN')
C
	CALL SSSUP(Hmrte)
C
	endfile(unit=9)
	CLOSE(UNIT=9)
	endfile(unit=12)
	CLOSE(UNIT=12)
c
c... call setden for output purposes
c
	call setden(1.D0,0.D0,Hmrte)	! adiabatic mixing w/ pure stuff
C
	opnf1 = opnf1(1:nchar) // tr3(1:4)
	CALL TRANS(opnf1)
C
	tt1 = t1
	T1 = SECNDS(tT1)/60.
	if(t1 .le. 0.) t1 = t1 + 60*24
	WRITE(lunlog,4000) TOBS
	WRITE(lunlog,4010) T1
 4000	FORMAT(3X,'BEGAN AT ',A24)
 4010	FORMAT(3X,'*** ELAPSED TIME *** ',1pG13.5,' min')
C
	STOP
	END
