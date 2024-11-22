	PROGRAM DEG3
C
C***************************************************************************
C***************************************************************************
C***************************************************************************
C
C	Program description:
C
C	DEG3 sorts the downwind dispersion calculation made for each of
C	the several observers in DEG2.  The output concentrations at
C	several given times may then be corrected for along-wind dispersion
C	as desired.
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

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	include 'DEG3.prm'
C
C... TABLE is the workhorse storage space.
C
	parameter (ntab0=2*maxnob+2*maxnt)
C
	COMMON
     ./ALP/	ALPHA,alpha1
     ./cgprop/	gasmw,gastem,gasrho,gascpk,gascpp,gasulc,gasllc,gaszzc,
     .		gasnam
     ./CNOBS/	NOBS
     ./comatm/	istab,tamb,pamb,humid,isofl,tsurf,ihtfl,htco,iwtfl,wtco,
     .		humsrc
     ./csigx/	sigxco,sigxp,sigxmd,sigxfl
     ./ERROR/	ERT1,ERDT,ERNTIM
     ./GEN2/	DEN(5,igen)
     ./NEND/	POUND
     ./oomsin/	oodist,avtime
     ./PARM/	U0,Z0,ZR,rml,USTAR,vkc,gg,RHOE,RHOA,DELTAy,BETAy,GAMMAF,
     .		yclow
     ./PARMSC/	RM,SZM,EMAX,RMAX,TSC1,ALEPH,TEND
     ./PHLAG/	CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
     ./ITI/	T1,TINP,TSRC,TOBS,TSRT
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
	character*4 TR3,PSD,Er3,SR3
	character*40 opnf1
	character opnf(40)
C
	EQUIVALENCE (opnf(1),opnf1)
C
C... DATA
C
	DATA POUND/-1.E-20/
	DATA TCc/maxtnob*0.0/, TCcSTR/maxtnob*0.0/
	data TSY/maxtnob*0.0/
	data TSZ/maxtnob*0.0/, KSUB/maxnt*0/
	DATA TB/maxtnob*0.0/, TDIST0/maxtnob*0.D0/
	data TDIST/maxtnob*0./
C
	DATA TR3/'.TR3'/,PSD/'.PSD'/
	DATA Er3/'.Er3'/,SR3/'.SR3'/
C
c
c... Get the time, run name, etc.............................................
c
	T1 = SECNDS(0.)
	call dattim(tsrt)
C
C... GET THE run NAME FOR FILE CONTROL
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
	opnf1 = opnf1(1:nchar) // psd(1:4)
	OPEN(UNIT=9, file=opnf1, status='OLD')
C
C... TIME SORT SUPERVISOR -- CALCULATE DOWNWIND DISPERSION CORRECTION....
C
	CALL SORTS(TABLE)
C
	CLOSE(UNIT=9)
C
C... OUTPUT SORTED PARAMETERS
C
	opnf1 = opnf1(1:nchar) // SR3(1:4)
	CALL SRTOUT(opnf1, table)
C
	STOP
	END
