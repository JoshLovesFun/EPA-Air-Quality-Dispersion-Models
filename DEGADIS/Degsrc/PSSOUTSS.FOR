	SUBROUTINE PSSOUT(X,Y,DERY,IHLF,NDIM,PRMT)
c
C..............................................................
C
C	SUBROUTINE PSSOUTSS.  Output from PSS for steady-state runs
C
c..............................................................
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

C
	include 'DEG2.prm'
c
	parameter (npss=14, zero=1.D-10)
c
	common
     ./ALP/	ALPHA,alpha1
     ./comfl/	cflag,clfl,cufl
     ./parm/	u0,z0,zr,rml,ustar,vkc,gg,rhoe,rhoa,deltay,betay,gammaf,
     .		yclow
     ./PHLAG/	CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
     ./STP/	STPP,ODLP,ODLLP,STPG,ODLG,ODLLG
C
	LOGICAL CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
	logical cflag
C
C
	DIMENSION Y(1),DERY(1),PRMT(1)
	dimension BKSP(npss),OUT(npss),CURNT(npss)

	data lastp/0/
C
c.....................................................................
c
C... OUTPUT PARAMETERS
C
C    FROM PSS		  OUTPUT TO MODEL
C    --------		  ---------------
C	X			DIST
C	PRMT(7)			Cc
C	Y(1)			SZ
C	Y(2)			SY2
C	PRMT(8)			B
c
c......................................................................
C
	ERM = 0.D0
	prmt(22) = prmt(21)
	lastp = lastp + 1
C
C... STARTUP FOR THE OUTPUT ROUTINE
C
	IF(PRMT(11) .eq. 0.) then
	    lastp = 0
	    CURNT(1) = X
	    CURNT(2) = PRMT(14)	! yc
	    CURNT(3) = prmt(7)	! cc
	    CURNT(4) = prmt(15)	! rho
	    CURNT(5) = PRMT(17)	! gamma
	    curnt(6) = prmt(16)	! temp
	    curnt(7) = prmt(8)	! b
	    curnt(8) = prmt(21)	! sz
	    curnt(9) = sqrt(Y(2))	! sy2
	    curnt(10)= y(4)
	    curnt(11)= prmt(20)	! rholay
	    curnt(12)= y(1)	! rhouh
	    curnt(13)= y(5)	! mass above UFL
	    curnt(14)= y(6)	! mass above LFL
	endif
C
C... SET THE CURRENT AND PREVIOUS RECORD
C
	DO 100 II=1,npss
  100	BKSP(II) = CURNT(II)
C
	CURNT(1) = X
	CURNT(2) = PRMT(14)	! yc
	CURNT(3) = prmt(7)	! cc
	CURNT(4) = prmt(15)	! rho
	CURNT(5) = PRMT(17)	! gamma
	curnt(6) = prmt(16)	! temp
	curnt(7) = prmt(8)	! b
	curnt(8) = prmt(21)	! sz
	curnt(9) = sqrt(Y(2))	! sy2
	curnt(10)= y(4)
	curnt(11)= prmt(20)	! rholay
	curnt(12)= y(1)	! rhouh
	curnt(13)= y(5)	! mass above UFL
	curnt(14)= y(6)	! mass above LFL
C
C... STOP INTEGRATION WHEN THE HALF WIDTH B < 0.
C
	IF( PRMT(8) .LE. 0.) then
	    IF(bksp(1) .ne. OUT(1)) then
		PRMT(11) = PRMT(11) + 1.
		call ssout(bksp)
	    endif
	    frac = bksp(7)/(bksp(7) - curnt(7))
	    prmt(12) = bksp(1)  - frac*(bksp(1)  - curnt(1))
	    prmt(7)  = bksp(3)  - frac*(bksp(3)  - curnt(3))
	    y(4)     = bksp(10) - frac*(bksp(10)  - curnt(10))
	    prmt(20) = bksp(11) - frac*(bksp(11)  - curnt(11))
	    y(1)     = bksp(12) - frac*(bksp(12)  - curnt(12))
	    y(5)     = bksp(13) - frac*(bksp(13)  - curnt(13))
	    y(6)     = bksp(14) - frac*(bksp(14)  - curnt(14))
	    prmt(5) = 1.
	    return
	endif
C
C... STOP INTEGRATION when yc<ycLOW
C
	IF(PRMT(14).GT.ycLOW) GO TO 95
		if(prmt(11) .lt. 5.) then	! force output
			erm = odlp
			goto 95
			endif
	AGAIN = .TRUE.
	GO TO 1000
   95	CONTINUE
C
	II = 1
  110	II = II + 1
	ER1 = ABS( (CURNT(II)-BKSP(II))/(CURNT(II)+zero) )
	ER2 = ABS( (CURNT(II)-OUT(II))/(CURNT(II)+zero) )
	ERM = dMAX1(ER1,ER2,ERM)
	IF(II .EQ. 3) II = 6	! skip density,gamma,temp
	IF(II .LT. 8) GO TO 110	! skip sy
C
C... RECORD POINT IF ODLP IS EXCEEDED OR 80 METERS SINCE LAST RECORD.
C	Also, RECORD FIRST POINT.
C
	DX = CURNT(1) - OUT(1)
	IF(prmt(11).NE.0.d0 .AND. ERM.LT.ODLP .AND. DX.LE.ODLLP) RETURN
C
C... IF THE NEXT INTEGRATION AFTER A POINT IS RECORDED VIOLATES THE
C	ERROR BOUND, THE CURRENT POINT MUST BE RECORDED. OTHERWISE, THE
C	LAST POINT TO SATISFY THE ERROR LIMITS IS RECORDED.
C
	DO 120 II=1,npss
	IF(lastp.EQ.0) BKSP(II) = CURNT(II)
  120	OUT(II) = BKSP(II)
C
	PRMT(11) = PRMT(11) + 1.
	lastp = -1
C
	call ssout(out)
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
	call ssout(curnt)
C
  130	CONTINUE
	PRMT(5) = 1.
C
	RETURN
	END
