	SUBROUTINE STRT3(OPNRUP)
c
C..............................................................
C
C	Get the parameters from DEG2
c
c..............................................................
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	include 'DEG3.prm'
C
C
	COMMON
     ./ALP/	ALPHA,alpha1
     ./cgprop/	gasmw,gastem,gasrho,gascpk,gascpp,gasulc,gasllc,gaszzc,
     .		gasnam
     ./CNOBS/	NOBS
     ./comatm/	istab,tamb,pamb,humid,isofl,tsurf,ihtfl,htco,iwtfl,wtco,
     .		humsrc
     ./csigx/	sigxco,sigxp,sigxmd,sigxfl
     ./GEN2/	DEN(5,igen)
     ./ITI/	T1,TINP,TSRC,TOBS,TSRT
     ./NEND/	POUND
     ./oomsin/	oodist,avtime
     ./PARM/	U0,Z0,ZR,rml,USTAR,vkc,gg,RHOE,RHOA,DELTAy,BETAy,GAMMAF,
     .		ycLOW
     ./PARMSC/	RM,SZM,EMAX,RMAX,TSC1,ALEPH,TEND
     ./PHLAG/	CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
     ./SSCON/	NREC(maxnob,2),T0(maxnob),XV(maxnob)
C
	character*3 gasnam
	character*40 OPNRUP
	character*24 TINP,TSRC,TOBS,TSRT
C
	LOGICAL CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
C
	OPEN(UNIT=9, file=OPNRUP, status='OLD')
C
	READ(9,*) NOBS
	DO 125 I=1,NOBS
  125	READ(9,*) NREC(I,1),NREC(I,2),T0(I),XV(I)
c
	READ(9,*) Npts
	DO 140 I=1,Npts
  140	READ(9,*) den(1,i),den(2,i),den(3,i),den(4,i),den(5,i)
	den(1,npts+1) = 2.
c
	read(9,*) oodist,avtime
c
	READ(9,*) U0,Z0,ZR,rml,USTAR
	read(9,*) vkc,gg,RHOE,RHOA,DELTAy
	read(9,*) BETAy,GAMMAF,ycLOW
c
	READ(9,1010) TINP,TSRC
	read(9,1010) TOBS,TSRT
 1010	format(2(a24,1x))
c
	READ(9,*) RM,SZM,EMAX,RMAX,TSC1
	read(9,*) ALEPH,TEND
c
	read(9,1020) gasnam
	read(9,*) gasmw,gastem,gasrho
	read(9,*) gascpk,gascpp
	read(9,*) gasulc,gasllc,gaszzc
	read(9,*) istab
	read(9,*) tamb,pamb,humid
	humsrc = 0.D0
	read(9,*) isofl,tsurf
	read(9,*) ihtfl,htco
	read(9,*) iwtfl,wtco
	read(9,*) sigxco,sigxp,sigxmd
 1020	format(a3)
c
	READ(9,*) CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
	READ(9,*) ALPHA
	alpha1 = alpha + 1.D0
C
	CLOSE(UNIT=9)
C
	RETURN
	END
