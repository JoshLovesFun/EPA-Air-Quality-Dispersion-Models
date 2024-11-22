	SUBROUTINE SSGOUT(X,Y,D,IHLF,NDIM,PRMT)
c
C..............................................................
C
C	SSGOUT controls the output from SSG/SSSUP.
c
c..............................................................
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	include 'DEG2.prm'
c
	parameter (nssg=7, zero=1.e-10)
c
	DIMENSION Y(1),D(1),PRMT(1),BKSP(nssg),OUT(nssg),CURNT(nssg)
C
	COMMON
     ./comatm/	istab,tamb,pamb,humid,isofl,tsurf,ihtfl,htco,iwtfl,wtco,
     .		humsrc
     ./PARM/	U0,Z0,ZR,rml,USTAR,vkc,gg,RHOE,RHOA,DELTA,BETA,GAMMAF,
     .		yclow
     ./STOPIT/	TSTOP
     ./STP/	STPO,STPP,ODLP,ODLLP,STPG,ODLG,ODLLG
c
C.............................................................
c
C... PARAMETER OUTPUT
C
C     FROM SSG		   OUTPUT TO MODEL
C     --------		   ---------------
C	X			DIST
C	PRMT(7)			Cc
C	Y(1)			SZ
c	prmt(14)		yc
c	prmt(15)		rho
C	prmt(16)		temp
c	prmt(17)		gamma
c
c...............................................................
C
	ERM = 0.D0
	T0L = PRMT(9)
	TSL = TS(T0L,X)
	prmt(22) = prmt(21)	! sz
C
C... STARTUP FOR OUTPUT ROUTINE
C
	IF(PRMT(11) .eq. 0.) then
	    RII = -100./STPG
	    RI = 0.
	    CURNT(1) = X
	    curnt(2) = prmt(14)	! yc
	    CURNT(3) = PRMT(7)	! cc
	    curnt(4) = prmt(15)	! rho
	    curnt(5) = prmt(17)	! gamma
	    curnt(6) = prmt(16)	! temp
	    CURNT(7) = prmt(21)	! sz
	endif
C
C... RECORD THE CURRENT AND PREVIOUS RECORDS
C
	RI = RI + 1.
C
	DO 100 II=1,nssg
 100	bksp(II) = curnt(II)
c
	CURNT(1) = X
	curnt(2) = prmt(14)	! yc
	CURNT(3) = PRMT(7)	! cc
	curnt(4) = prmt(15)	! rho
	curnt(5) = prmt(17)	! gamma
	curnt(6) = prmt(16)	! temp
	CURNT(7) = prmt(21)	! sz
C
c... stop integration when yc<yclow and time is satisfied
C
	IF(PRMT(14).LT.ycLOW .AND. TSL.GE.TSTOP) GO TO 1000
C
C... CHECK FOR OUTPUT
C
	DO 110 II=3,nssg
	ER1 = ABS( (CURNT(II)-BKSP(II))/(CURNT(II)+zero) )
	ER2 = ABS( (CURNT(II)-OUT(II))/(CURNT(II)+zero) )
  110	ERM = dMAX1(ER1,ER2,ERM)
C
C... OUTPUT RECORD IF ODLG IS EXCEEDED OR ODLLG METERS SINCE LAST OUTPUT
C
	DX = CURNT(1) - OUT(1)
	IF( RI.NE.1. .AND. ERM.LT.ODLG .AND. DX.LE.ODLLG) RETURN
C
C... RECORD THE LAST POINT TO BE UNDER THE ERROR CRITERIA. IN CASE
C	THE FIRST POINT AFTER A RECORD EXCEEDS THE ERROR BOUND, RECORD
C	THAT POINT AS WELL.
C
	DO 120 II=1,nssg
	IF(RI .EQ. RII+1.) BKSP(II) = CURNT(II)
  120	OUT(II) = BKSP(II)
C
	RI = RII
	PRMT(11) = PRMT(11) + 1.
C
	WRITE(9,*) (OUT(II),II=1,nssg)
	RETURN
C
 1000	CONTINUE
C
C... STOP INTEGRATION
C
	PRMT(12) = X
	TSTOP = TSL
	PRMT(11) = PRMT(11) + 1.
	WRITE(9,*) (CURNT(II),II=1,nssg)
C
	PRMT(5) = 1.
C
	RETURN
	END
