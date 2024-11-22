	SUBROUTINE PSSOUT(X,Y,D,IHLF,NDIM,PRMT)
c
C..............................................................
C
C	SUBROUTINE PSSOUT controls the output from PSS/SSSUP.
C
c.............................................................
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

C
	include 'DEG2.prm'
c
	parameter (npss=12, zero=1.e-10)
c
	COMMON
     ./comatm/	istab,tamb,pamb,humid,isofl,tsurf,ihtfl,htco,iwtfl,wtco,
     .		humsrc
     ./PARM/	U0,Z0,ZR,rml,USTAR,vkc,gg,RHOE,RHOA,DELTA,BETA,GAMMAF,
     .		yclow
     ./PHLAG/	CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
     ./STOPIT/	TSTOP
     ./STP/	STPO,STPP,ODLP,ODLLP,STPG,ODLG,ODLLG
c
	LOGICAL CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
	DIMENSION Y(1),D(1),PRMT(1),BKSP(npss),OUT(npss),CURNT(npss)
C
C... OUTPUT PARAMETERS...............................................
C
C... FROM PSS		  OUTPUT TO MODEL
C    --------		  ---------------
C	X			DIST
C	PRMT(7)			Cc
C	Y(1)			SZ
C	Y(2)			SY2
C	PRMT(8)			B
C	PRMT(13)		T0(I)
c	prmt(14)		yc
c	prmt(15)		rho
c	prmt(16)		temp
c	prmt(17)		gamma
c
c.......................................................................
c
	ERM = 0.
	TSL = TS(PRMT(13),X)
	prmt(22) = prmt(21)
	lastp = lastp + 1
C
C... STARTUP FOR THE OUTPUT ROUTINE
C
	IF(PRMT(11) .eq. 0.D0) then
	    lastp = 0
	    CURNT(1) = X
	    curnt(2) = prmt(14)		! yc
	    CURNT(3) = PRMT(7)		! cc
	    curnt(4) = prmt(15)		! rho
	    curnt(5) = prmt(17)		! gamma
	    curnt(6) = prmt(16)		! temp
	    CURNT(7) = prmt(21)		! sz
	    CURNT(8) = sqrt(Y(2))	! sy2
	    CURNT(9) = PRMT(8)		! b
	    curnt(10)= y(4)
	    curnt(11)= prmt(20)		! rholay
	    curnt(12)= y(1)		! rhouh
	endif
C
C... SET THE CURRENT AND PREVIOUS RECORD
C
	DO 100 II=1,npss
  100	BKSP(II) = CURNT(II)
C
	CURNT(1) = X
	curnt(2) = prmt(14)	! yc
	CURNT(3) = PRMT(7)	! cc
	curnt(4) = prmt(15)	! rho
	curnt(5) = prmt(17)	! gamma
	curnt(6) = prmt(16)	! temp
	CURNT(7) = prmt(21)	! sz
	CURNT(8) = sqrt(Y(2))	! sy2
	CURNT(9) = PRMT(8)	! b
	curnt(10)= y(4)
	curnt(11)= prmt(20)	! rholay
	curnt(12)= y(1)		! rhouh
C
C... STOP INTEGRATION WHEN THE HALF WIDTH B < 0.
C
	IF(PRMT(8) .LE. 0.) then
	    frac = bksp(9)/(bksp(9) - curnt(9))
	    prmt(12) = bksp(1)  - frac*(bksp(1)  - curnt(1))
	    prmt(7)  = bksp(3)  - frac*(bksp(3)  - curnt(3))
	    y(4)     = bksp(10) - frac*(bksp(10) - curnt(10))
	    prmt(20) = bksp(11) - frac*(bksp(11) - curnt(11))
	    y(1)     = bksp(12) - frac*(bksp(12) - curnt(12))
	    prmt(5) = 1.
	    return
	endif
C
C... STOP INTEGRATION AND GET A NEW OBSERVER WHEN yc<ycLOW
C
	IF(PRMT(14).GT.ycLOW .OR. TSL.LT.TSTOP) GO TO 95
		if(prmt(11) .lt. 5.) then	! guarantee 5 records
			erm = odlp		! force output
			goto 95
			endif
	AGAIN = .TRUE.
	TSTOP = TSL
	GO TO 1000
   95	CONTINUE
C
c... Check the error criteria's
c
	II = 2			! skip DIST,YC; 1 and 2
  110	II = II + 1
	ER1 = ABS( (CURNT(II)-BKSP(II))/(CURNT(II)+zero) )
	ER2 = ABS( (CURNT(II)-OUT(II))/(CURNT(II)+zero) )
	ERM = dMAX1(ER1,ER2,ERM)
	IF(II .EQ. 3) II = II + 1	! skip RHO;4
	IF(II .EQ. 7) II = II + 1	! skip SY;8
	IF(II .EQ. 5) II = II + 1	! skip TEMP;6
	IF(II .LT. 9) GO TO 110
C
C... RECORD POINT IF ODLP IS EXCEEDED OR 80 METERS SINCE LAST RECORD.
C	Also, RECORD FIRST POINT.
C
	DX = CURNT(1) - OUT(1)
	IF(prmt(11).ne.0.d0 .AND. ERM.LT.ODLP .AND. DX.LE.ODLLP) RETURN
C
C... IF THE NEXT INTEGRATION AFTER A POINT IS RECORDED VIOLATES THE
C	ERROR BOUND, THE CURRENT POINT MUST BE RECORDED. OTHERWISE, THE
C	LAST POINT TO SATISFY THE ERROR LIMITS IS RECORDED.
C
	DO 120 II=1,npss
	IF(lastp .EQ. 0) BKSP(II) = CURNT(II)
  120	OUT(II) = BKSP(II)
C
	PRMT(11) = PRMT(11) + 1.
	lastp = -1
C
	WRITE(9,*) (OUT(II),II=1,9)
	RETURN
C
 1000	CONTINUE
C
C... STOP INTEGRATION
C
	PRMT(12) = X
C
	IF(CURNT(1) .EQ. OUT(1)) GO TO 130
C
	PRMT(11) = PRMT(11) + 1.
	WRITE(9,*) (CURNT(II),II=1,9)
C
  130	CONTINUE
	PRMT(5) = 1.
	RETURN
	END
