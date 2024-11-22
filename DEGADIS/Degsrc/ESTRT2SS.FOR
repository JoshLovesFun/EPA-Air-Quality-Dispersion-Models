	SUBROUTINE ESTRT2(OPNRUP)
c
C...........................................................
C
C	ROUTINE TO GET RUN PARAMETERS for a steady-state run from
c	an ER2 file
C
c...........................................................
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

c
	include 'DEG2.prm'
c
	parameter(	ienda= 18,
     .			ienda1= ienda+1,
     .			iendb= 7,
     .			iendb1= iendb+1)
c

	COMMON
     ./ERR2S/	SY0ER,ERRP,SMXP,WTSZP,WTSYP,WTBEP,WTDH,ERRG,SMXG,
     .		WTRUH,WTDHG
     ./STP/	STPP,ODLP,ODLLP,STPG,ODLG,ODLLG
C
c
	character*40 OPNRUP
	character*1 DUMMY(132)
	character*132 DUMMY1
	equivalence (dummy1,dummy(1))
	DIMENSION RLBUF(ienda),RLBUF1(iendb)
C
	OPEN(UNIT=9, file=OPNRUP, status='OLD')
C
C... FIRST, FILL RLBUF; READ A LINE AND DETERMINE ITS PURPOSE
C
	I = 1
  100	CONTINUE
	READ(9,1000,END=350) DUMMY1
	IF(DUMMY(1) .EQ. '!') GO TO 100
	read(dummy1(1:20), 1010, err=400) rlbuf(i)
	I = I + 1
	IF(I.EQ. ienda1) GOTO 200
	GO TO 100
C
C... NOW, FILL RLBUF1
C
  200	i = 1
  210	READ(9,1000,END=350) DUMMY1
	IF(DUMMY(1) .EQ. '!') GO TO 210
	read(dummy1(1:20), 1010, err=400) rlbuf1(i)
	I = I + 1
	if(i.eq. iendb1) goto 300
	GO TO 210
C
C... EXIT THE PROCEEDINGS
C
  300	CONTINUE
	sy0er = rlbuf(1)  !SSSUP - RKGST - INITIAL SY
	errp  = rlbuf(7)  !SSSUP - RKGST(PSS) - ERROR BOUND
	smxp  = rlbuf(8)  !SSSUP - RKGST(PSS) - MAXIMUM STEP
	wtszp = rlbuf(9)  !SSSUP - RKGST(PSS) - WEIGHT FOR SZ
	wtsyp = rlbuf(10) !SSSUP - RKGST(PSS) - WEIGHT FOR SY
	wtbep = rlbuf(11) !SSSUP - RKGST(PSS) - WEIGHT FOR BEFF
	wtDH  = rlbuf(12) !SSSUP - RKGST(PSS) - WEIGHT FOR BEFF
	errg  = rlbuf(13) !SSSUP - RKGST(SSG) - ERROR BOUND
	smxg  = rlbuf(14) !SSSUP - RKGST(SSG) - MAXIMUM STEP SIZE
	wtRUH = rlbuf(17) !SSSUP - RKGST(PSS) - WEIGHT FOR BEFF
	wtDHG = rlbuf(18) !SSSUP - RKGST(PSS) - WEIGHT FOR BEFF
C
	stpp  = rlbuf1(2) !SSSUP - RKGST(PSS) - INITIAL STEP
	odlp  = rlbuf1(3) !SSSUP - RKGST(PSS) - RELATIVE OUTPUT DELTA
	odllp = rlbuf1(4) !SSSUP - RKGST(PSS) - MAXIMUM DISTANCE TO OUT
	stpg  = rlbuf1(5) !SSSUP - RKGST(SSG) - INITIAL STEP
	odlg  = rlbuf1(6) !SSSUP - RKGST(SSG) - RELATIVE OUTPUT DELTA
	odllg = rlbuf1(7) !SSSUP - RKGST(SSG) - MAXIMUM DISTANCE TO OUT
C
	CLOSE(UNIT=9)
	RETURN
C
  350	call trap(20,0)
c
  400	CALL trap(21,0)
C
 1000	FORMAT(A132)
 1010	FORMAT(10X,G10.4)
C
	END
