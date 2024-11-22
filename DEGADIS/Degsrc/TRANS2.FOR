	SUBROUTINE TRANS(FILE)
c
C..............................................................
C
C	TRANS2 -- USE WITH DEG2 to transfer info to DEG3
C
C..............................................................
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	include 'DEG2.prm'
c
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
     ./oomsin/	oodist,avtime
     ./PARM/	U0,Z0,ZR,rml,USTAR,vkc,gg,RHOE,RHOA,DELTA,BETA,GAMMAF,
     .		ycLOW
     ./PARMSC/	RM,SZM,EMAX,RMAX,TSC1,ALEPH,TEND
     ./PHLAG/	CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
     ./SSCON/	NREC(maxnob,2),T0(maxnob),XV(maxnob)
c
	character*3 gasnam
	character*24 TINP,TSRC,TOBS,TSRT
	character*(*) file
c
	LOGICAL CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
C
	OPEN(UNIT=9, file=FILE, status='UNKNOWN')
C
	WRITE(9,1040) NOBS
	DO 125 I=1,NOBS
  125	WRITE(9,1010) NREC(I,1),NREC(I,2),T0(I),XV(I)
c
	DO 140 I=1,igen
  140	IF(DEN(1,I).gt. 1.) GOTO 145
	write(6,*) ' density function error in TRANS'
  145	NP = I - 1
	WRITE(9,1040) NP
	DO 150 I=1,NP
  150	WRITE(9,1060) DEN(1,I),DEN(2,I),den(3,i),den(4,i),den(5,i)
C
	write(9,1060) oodist,avtime
c
	WRITE(9,1060) U0,Z0,ZR,rml,USTAR
	write(9,1060) vkc,gg,RHOE,RHOA,DELTA
	write(9,1030) BETA,GAMMAF,ycLOW
c
	WRITE(9,1050) TINP,TSRC
	write(9,1050) TOBS,TSRT
c
	WRITE(9,1060) RM,SZM,EMAX,RMAX,TSC1
	write(9,1020) ALEPH,TEND
c
	write(9,1080) gasnam
	write(9,1030) gasmw,gastem,gasrho
	write(9,1020) gascpk,gascpp
	write(9,1030) gasulc,gasllc,gaszzc
	write(9,1040) istab
	write(9,1030) tamb,pamb,humid
	write(9,1025) isofl,tsurf
	write(9,1025) ihtfl,htco
	write(9,1025) iwtfl,wtco
	write(9,1030) sigxco,sigxp,sigxmd
c
	WRITE(9,*) CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
c
	WRITE(9,1070) ALPHA
c
 1010	format(1x,i8,1x,i8,2(1x,1pg14.7))
 1020	format(2(1x,1pg14.7))
 1025	format(1x,i4,1x,1pg14.7)
 1030	format(3(1x,1pg14.7))
 1040	format(1x,i4)
 1050	format(2(a24,1x))
 1060	format(5(1x,1pg14.7))
 1070	format(1x,1pg14.7)
 1080	format(a3,1x)
C
	CLOSE(UNIT=9)
	RETURN
	END
