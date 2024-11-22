	SUBROUTINE STRT2(OPNRUP,Hmrte,CCP)
c
C.............................................................
C
C	Subroutine to start a steady-state downwind integration
c
c..............................................................
c
	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	INCLUDE 'DEG2.prm'
C
C
	common
     ./ALP/	ALPHA,alpha1
     ./cgprop/	gasmw,gastem,gasrho,gascpk,gascpp,gasulc,gasllc,gaszzc,
     .		gasnam
     ./comatm/	istab,tamb,pamb,humid,isofl,tsurf,ihtfl,htco,iwtfl,wtco,
     .		humsrc
     ./comss/	ess,slen,swid,outcc,outsz,outb,outl,swcl,swal,senl,srhl
     ./csprd/	ce, delrmn
     ./GEN2/	DEN(5,igen)
     ./ITI/	T1,TINP,TSRC,TOBS,TSRT
     ./NEND/	POUND
     ./oomsin/	oodist,avtime
     ./parm/	u0,z0,zr,rml,ustar,vkc,gg,rhoe,rhoa,deltay,betay,gammaf,
     .		yclow
     ./phicom/	iphifl,dellay
     ./PHLAG/	CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
     ./TITL/	TITLE
c
	character*3 gasnam
	character*80 TITLE(4)
	character*24 TSRC,TINP,TOBS,TSRT
C
	LOGICAL CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
C
C
	character*40 opnrup
c
	OPEN(UNIT=9, file=OPNRUP, status='OLD', iostat=ierr)
	if(ierr.ne.0) then
		stop 'DEG2S could not open the input file.'
	endif
C
	DO 90 I = 1,4
   90	READ(9,1000) TITLE(I)
 1000	FORMAT(A80)
C
	read(9,*) np
	do 100 i=1,np
	read(9,*) dummy1,dummy2,dummy3,PWC,PTEMP,PFRACV,PENTH,PRHO
  100	IF(I .EQ. 1) CCP = PWC*PRHO
c
	READ(9,*) NP
	DO 120 I=1,NP
  120	READ(9,*) DEN(1,I),DEN(2,I),den(3,i),den(4,i),den(5,i)
	I = NP + 1
	DEN(1,I) = 2.
c
	read(9,*) np
	do 140 i=1,np
  140	read(9,*) dummy1,dummy2,dummy3,dummy4,dum5,dum6,dum7
C
	read(9,1100) tinp,tsc
	read(9,1100) tobs,tsrt
 1100	format(a24,1x,a24)
c
	read(9,*) oodist,avtime
c
	READ(9,*) U0,Z0,ZR,rML,USTAR
	Read(9,*) vkc,gg,RHOE,RHOA,DELTAy
	read(9,*) BETAy,GAMMAF,ycLOW
c
	read(9,*) dummy1
	read(9,*) dummy1
c
	READ(9,*) CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
c
	READ(9,*) ALPHA
	alpha1 = alpha + 1.
c
	read(9,1200) gasnam
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
	read(9,*) dummy1

	READ(9,*) ESS,SLEN,SWID
	read(9,*,end=1199) OUTCc,OUTSZ,OUTB,OUTL
	read(9,*,end=1199) swcl,swal,senl,srhl
C
	read(9,*,end=1199) iphifl,dellay
	hmrte = 0.
	if(isofl.eq. 0) read(9,*,end=1199) Hmrte
C
	READ(9,*,end=1199) ce, delrmn
 1199	CLOSE(UNIT=9)
	RETURN
 1200	format(a3)
	END
