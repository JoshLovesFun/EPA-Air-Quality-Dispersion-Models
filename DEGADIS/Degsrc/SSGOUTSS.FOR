	SUBROUTINE SSGOUT(X,Y,D,IHLF,NDIM,PRMT)
c
C..............................................................
C
c	Subroutine to control the output from SSG for a steady-state
c	simulation
c
c...............................................................
c
	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	parameter (nssg=9, zero=1.D-10)
C
	DIMENSION Y(1),D(1),PRMT(1),BKSP(nssg),OUT(nssg),CURNT(nssg)
C
	include 'DEG2.prm'
c
	common
     ./ALP/	ALPHA,alpha1
     ./parm/	u0,z0,zr,rml,ustar,vkc,gg,rhoe,rhoa,deltay,betay,gammaf,
     .		yclow
     ./STP/	STPP,ODLP,ODLLP,STPG,ODLG,ODLLG
C
C..................................................................
c
C... PARAMETER OUTPUT
C
C     FROM SSG		   OUTPUT TO MODEL
C     --------		   ---------------
C	X			DIST
C	PRMT(7)			Cc
C	Y(1)			SZ
C	PRMT(8)			XV
c
c...................................................................
C
	ERM = 0.D0
	prmt(22) = prmt(21)
C
	IF(PRMT(11) .NE. 0.) GO TO 90
C
C... STARTUP FOR OUTPUT ROUTINE
C
	RII = -100./STPG
	RI = 0.
	CURNT(1) = X
	CURNT(2) = PRMT(14)	! yc
	CURNT(3) = prmt(7)	! cc
	CURNT(4) = PRMT(15)	! rho
	curnt(5) = prmt(17)	! gamma
	curnt(6) = prmt(16)	! temp
	curnt(7) = 0.0		! b
	curnt(8) = prmt(21)	! sz
	curnt(9) = rt2*deltay*(x+prmt(8))**betay	! sy
C
   90	CONTINUE
C
C... RECORD THE CURRENT AND PREVIOUS RECORDS
C
	RI = RI + 1.
C
	DO 100 II=1,nssg
  100	BKSP(II) = CURNT(II)
	CURNT(1) = X
	CURNT(2) = PRMT(14)	! yc
	CURNT(3) = prmt(7)	! cc
	CURNT(4) = PRMT(15)	! rho
	curnt(5) = prmt(17)	! gamma
	curnt(6) = prmt(16)	! temp
	curnt(7) = 0.0		! b
	curnt(8) = prmt(21)	! sz
	curnt(9) = rt2*deltay*(x+prmt(8))**betay	! sy
C
C... STOP INTEGRATION WHEN yc < ycLOW
C
	IF(PRMT(14).LT.ycLOW) GO TO 1000
C
C... CHECK FOR OUTPUT
C
	DO 110 II=2,nssg
	if(ii.eq.7) goto 110
	ER1 = ABS( (CURNT(II)-BKSP(II))/(CURNT(II)+zero) )
	ER2 = ABS( (CURNT(II)-OUT(II))/(CURNT(II)+zero) )
  110	ERM = dMAX1(ER1,ER2,ERM)
C
C... OUTPUT RECORD IF ODLG IS EXCEEDED OR 100 METERS SINCE LAST OUTPUT
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
	call ssout(out)
	RETURN
C
 1000	CONTINUE
C
C... STOP INTEGRATION
C
	PRMT(12) = X
	PRMT(11) = PRMT(11) + 1.
C
	call ssout(curnt)
C
	PRMT(5) = 1.
C
	RETURN
	END
