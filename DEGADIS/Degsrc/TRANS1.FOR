	SUBROUTINE TRANS(FILE)
c
C..............................................................
C
C	FILE NAME TRANS1 -- FOR USE IN DEGADIS1
C
C..............................................................
C
	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	include 'DEG1.prm'
C
	common
     ./ALP/	ALPHA,alpha1
     ./cgprop/	gasmw,gastem,gasrho,gascpk,gascpp,gasulc,gasllc,gaszzc,
     .		gasnam
     ./comatm/	istab,tamb,pamb,humid,isofl,tsurf,ihtfl,htco,iwtfl,wtco,
     .		humsrc
     ./comH/	Hmrte,Harte,Hwrte
     ./comss/	ess,slen,swid,outcc,outsz,outb,outl,swcl,swal,senl,srhl
     ./csigx/	sigxco,sigxp,sigxmd,sigxfl
     ./csprd/	ce, delrmn
     ./ERROR/	STPIN,ERBND,WTRG,WTtm,WTya,wtyc,wteb,wtmb,XLI,
     .		XRI,EPS,ZLOW,STPINZ,ERBNDZ,SRCOER,srcss,srccut,
     .		ERNOBL,NOBLpt,crfger,epsilon
     ./GEN1/	PTIME(igen), ET(igen), R1T(igen), PWC(igen), PTEMP(igen),
     .		PFRACV(igen), PENTH(igen), PRHO(igen)
     ./GEN2/	DEN(5,igen)
     ./GEN3/	radg(2,maxl),qstr(2,maxl),srcden(2,maxl),srcwc(2,maxl),
     .		srcwa(2,maxl),srcen(2,maxl)
     ./ITI/	T1,TINP,TSRC,TOBS,TSRT
     ./NEND/	POUND
     ./oomsin/	oodist,avtime
     ./parm/	u0,z0,zr,rml,ustar,vkc,gg,rhoe,rhoa,deltay,betay,gammaf,
     .		yclow
     ./PARMSC/	RM,SZM,EMAX,RMAX,TSC1,ALEPH,TEND
     ./phicom/	iphifl,dellay
     ./PHLAG/	CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
     ./TITL/	TITLE
c
	character*3 gasnam
	character*80 TITLE(4)
	character*24 TSRC,TINP,TOBS,TSRT
C
	LOGICAL CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
c
c
	character*(*) file
C
c
	OPEN(UNIT=8, file=FILE, status='UNKNOWN')
C
	WRITE(8,1000) (TITLE(I),I=1,4)
 1000	FORMAT(A80)
C
	DO 100 I=1,igen
  100	IF(PTIME(I).EQ.POUND) GO TO 105
	write(6,*) ' POUND WAS NOT DETECTED '
  105	NP = I - 1
	WRITE(8,1040) NP
	DO 110 I=1,NP
  110	WRITE(8,1030) PTIME(I),ET(I),R1T(I), PWC(I),PTEMP(I),
     .		PFRACV(I), PENTH(I), PRHO(I)
c
	DO 120 I=1,igen
  120	IF(DEN(1,I) .gt. 1.) GOTO 125

	DO 122 I=1,igen
  122	WRITE(8,1060) DEN(1,I),DEN(2,I),den(3,i),den(4,i),den(5,i)
	write(6,*) ' density function blew the loop'

  125	NP = I - 1
	WRITE(8,1040) NP
	DO 130 I=1,NP
  130	WRITE(8,1060) DEN(1,I),DEN(2,I),den(3,i),den(4,i),den(5,i)
c
	DO 140 I=1,maxl
  140	IF(radg(1,I).EQ.POUND .AND. radg(2,I).EQ.POUND) GO TO 145
	write(6,*) ' POUND WAS NOT DETECTED '

  145	NP = I - 1
  146	WRITE(8,1040) NP
	DO 150 I=1,NP
  150	WRITE(8,1060) radg(1,i),radg(2,i),qstr(2,i),srcden(2,i),srcwc(2,i)
     .		,srcwa(2,i),srcen(2,i)
c
 1020	format(1x,i4,1x,1pg14.7)
 1030	format(8(1x,1pg14.7))
 1040	format(1x,i4)
 1050	format(2(a24,1x))
 1060	format(1x,1pg23.16,7(1x,1pg14.7))
 1070	format(1x,1pg14.7)
 1080	format(a3)
c
	WRITE(8,1050) TINP,TSRC
	write(8,1050) TOBS,TSRT
	write(8,1030) oodist,avtime
	WRITE(8,1060) U0,Z0,ZR,rml,USTAR
	write(8,1060) vkc,gg,RHOE,RHOA,DELTAy
	write(8,1030) BETAy,GAMMAF,ycLOW
	WRITE(8,1060) RM,SZM,EMAX,RMAX,TSC1
	write(8,1030) ALEPH,TEND
	WRITE(8,*) CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
	WRITE(8,1070) ALPHA
	write(8,1080) gasnam
	write(8,1030) gasmw,gastem,gasrho
	write(8,1030) gascpk,gascpp
	write(8,1030) gasulc,gasllc,gaszzc
	write(8,1040) istab
	write(8,1030) tamb,pamb,humid
	write(8,1020) isofl,tsurf
	write(8,1020) ihtfl,htco
	write(8,1020) iwtfl,wtco
	write(8,1030) sigxco,sigxp,sigxmd
C
	if(check4) then
	    write(8,1030) ess,slen,swid
	    write(8,1060) outcc,outsz,outb,outl
	    write(8,1060) swcl,swal,senl,srhl
	endif
c
	write(8,1020) iphifl,dellay
c
	if(isofl.eq. 0) write(8,1030) Hmrte
c
	WRITE(8,1030) ce, delrmn
C
	endfile(unit=8)
	CLOSE(UNIT=8)
C
	RETURN
	END
