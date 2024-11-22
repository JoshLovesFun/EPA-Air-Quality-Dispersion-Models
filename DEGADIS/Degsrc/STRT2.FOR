	SUBROUTINE STRT2(OPNRUP,Hmrte)
c
C.............................................................
C
C	STRT2 reads the information left in the TR2 file from DEG1.
c
c..................................................................
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	include 'DEG2.prm'
c
	COMMON
     ./ALP/	ALPHA,alpha1
     ./cgprop/	gasmw,gastem,gasrho,gascpk,gascpp,gasulc,gasllc,gaszzc,
     .		gasnam
     ./comatm/	istab,tamb,pamb,humid,isofl,tsurf,ihtfl,htco,iwtfl,wtco,
     .		humsrc
     ./csigx/	sigxco,sigxp,sigxmd,sigxfl
     ./csprd/	ce, delrmn
     ./GEN1/	PTIME(igen), ET(igen), R1T(igen), PWC(igen), PTEMP(igen),
     .		PFRACV(igen), PENTH(igen), PRHO(igen)
     ./GEN2/	DEN(5,igen)
     ./GEN3/	radg(2,maxl),qstr(2,maxl),srcden(2,maxl),srcwc(2,maxl),
     .		srcwa(2,maxl),srcen(2,maxl)
     ./ITI/	T1,TINP,TSRC,TOBS,TSRT
     ./NEND/	POUND
     ./oomsin/	oodist,avtime
     ./PARM/	U0,Z0,ZR,rml,USTAR,vkc,gg,RHOE,RHOA,DELTAy,BETAy,GAMMAF,
     .		ycLOW
     ./PARMSC/	RM,SZM,EMAX,RMAX,TSC1,ALEPH,TEND
     ./phicom/	iphifl,dellay
     ./PHLAG/	CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
     ./TITL/	TITLE
c
	character*80 TITLE(4)
c
	character*24 TINP,TSRC,TOBS,TSRT
	character*3 gasnam
c
	LOGICAL CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
c
	character*80 OPNRUP
C
	OPEN(UNIT=9, file=OPNRUP, status='OLD', iostat=ierr)
	if(ierr.ne.0) then
		stop 'DEG2 could not open the input file.'
	endif
C
	DO 90 I = 1,4
   90	READ(9,1000) TITLE(I)
 1000	FORMAT(A80)
C
	READ(9,*) NP
	DO 100 I=1,NP
  100	READ(9,*) PTIME(I),ET(I),R1T(I), PWC(I), PTEMP(I),
     .		PFRACV(I), PENTH(I), PRHO(I)
	PTIME(NP + 1)  = POUND
c
	READ(9,*) NP
	DO 220 I=1,NP
  220	READ(9,*) DEN(1,I),DEN(2,I),den(3,I),den(4,i),den(5,i)
	den(1,np+1) = 2.
c
	READ(9,*) NP
	DO 300 I=1,NP
	READ(9,*) radg(1,I),radg(2,I),qstr(2,I),srcden(2,I),srcwc(2,i),
     .		srcwa(2,i),srcen(2,i)
	qstr(1,I) = radg(1,I)
	srcden(1,I) = radg(1,I)
	srcwc(1,i)  = radg(1,i)
	srcwa(1,i)  = radg(1,i)
	srcen(1,i)  = radg(1,i)
  300	continue
	I = NP + 1
	radg(1,I)  = POUND
	radg(2,I)  = POUND
	qstr(1,I) = POUND
	qstr(2,I) = POUND
	srcden(1,I) = POUND
	srcden(2,I) = POUND
	srcwc(1,I) = POUND
	srcwc(2,I) = POUND
	srcwa(1,I) = POUND
	srcwa(2,I) = POUND
	srcen(1,I) = POUND
	srcen(2,I) = POUND
C
	READ(9,1010) TINP,TSRC
	READ(9,1010) tobs,TSRT
c
	read(9,*) oodist,avtime
c
	READ(9,*) U0,Z0,ZR,rml,USTAR
	read(9,*) vkc,gg,RHOE,RHOA,DELTAy
	read(9,*) BETAy,GAMMAF,ycLOW
c
	READ(9,*) RM,SZM,EMAX,RMAX,TSC1
	read(9,*) ALEPH,TEND
c
	READ(9,*) CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
c
	READ(9,*) ALPHA
	alpha1 = alpha + 1.
c
	read(9,1020) gasnam
	read(9,*) gasmw,gastem,gasrho
	read(9,*) gascpk,gascpp
	read(9,*) gasulc,gasllc,gaszzc
c
	read(9,*) istab
	read(9,*) tamb,pamb,humid
	humsrc = 0.D0
	read(9,*) isofl,tsurf
	read(9,*) ihtfl,htco
	read(9,*) iwtfl,wtco
c
	read(9,*) sigxco,sigxp,sigxmd
c
	read(9,*) iphifl,dellay
c
	Hmrte = 0.
	if(isofl.eq. 0) read(9,*) Hmrte
C
	READ(9,*) ce, delrmn
c
 1010	format(2(a24,1x))
 1020	format(a3)
C
	CLOSE(UNIT=9)
C
	RETURN
	END
