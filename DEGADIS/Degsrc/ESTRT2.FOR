	SUBROUTINE ESTRT2(OPNRUP)
c
C...........................................................
C
C	ROUTINE TO GET RUN PARAMETERS FROM the ER2 file.
C
c...........................................................
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

C
	include 'DEG2.prm'
c
	parameter (	ienda= 18,
     .			ienda1= ienda+1,
     .			iendb= 7,
     .			iendb1= iendb+1)
c
	common
     ./ERROR/SY0ER,ERRO,SZ0ER,WTAIO,WTQOO,WTSZO,ERRP,SMXP,
     . WTSZP,WTSYP,WTBEP,WTDH,ERRG,SMXG,ERTDNF,ERTUPF,WTRUH,WTDHG
     ./STP/STPO,STPP,ODLP,ODLLP,STPG,ODLG,ODLLG
     ./CNOBS/NOBS
C
c	EQUIVALENCE
c     .(RLBUF(1),SY0ER),   !SSSUP - RKGST - INITIAL SY
c     .(RLBUF(2),ERRO),    !SSSUP - RKGST(OBS) - ERROR BOUND
c     .(RLBUF(3),SZ0ER),   !SSSUP - RKGST(OBS) - INITIAL SZ
c     .(RLBUF(4),WTAIO),   !SSSUP - RKGST(OBS) - WEIGHT FOR AI
c     .(RLBUF(5),WTQOO),   !SSSUP - RKGST(OBS) - WEIGHT FOR Q
c     .(RLBUF(6),WTSZO),   !SSSUP - RKGST(OBS) - WEIGHT FOR SZ
c     .(RLBUF(7),ERRP),    !SSSUP - RKGST(PSS) - ERROR BOUND
c     .(RLBUF(8),SMXP),    !SSSUP - RKGST(PSS) - MAXIMUM STEP
c     .(RLBUF(9),WTSZP),   !SSSUP - RKGST(PSS) - WEIGHT FOR SZ
c     .(RLBUF(10),WTSYP),  !SSSUP - RKGST(PSS) - WEIGHT FOR SY
c     .(RLBUF(11),WTBEP),  !SSSUP - RKGST(PSS) - WEIGHT FOR BEFF
c     .(RLBUF(12),WTDH),   !SSSUP - RKGST(PSS) - WEIGHT FOR DH
c     .(RLBUF(13),ERRG),   !SSSUP - RKGST(SSG) - ERROR BOUND
c     .(RLBUF(14),SMXG),   !SSSUP - RKGST(SSG) - MAXIMUM STEP SIZE
c     .(RLBUF(15),ERTDNF), !TDNF - CONVERGENCE CRITERIA
c     .(RLBUF(16),ERTUPF), !TUPF - CONVERGENCE CRITERIA
c     .(RLBUF(17),WTruh),  !SSSUP - RKGST(SSG) - WEIGHT FOR RUH
c     .(RLBUF(ienda),WTdhg)!SSSUP - RKGST(SSG) - WEIGHT FOR DH
C
c	EQUIVALENCE
c     .(RLBUF1(1),STPO),   !SSSUP - RKGST(OBS) - INITIAL STEP
c     .(RLBUF1(2),STPP),   !SSSUP - RKGST(PSS) - INITIAL STEP
c     .(RLBUF1(3),ODLP),   !SSSUP - RKGST(PSS) - RELATIVE OUTPUT DELTA
c     .(RLBUF1(4),ODLLP),  !SSSUP - RKGST(PSS) - MAXIMUM DISTANCE TO OUT
c     .(RLBUF1(5),STPG),   !SSSUP - RKGST(SSG) - INITIAL STEP
c     .(RLBUF1(6),ODLG),   !SSSUP - RKGST(SSG) - RELATIVE OUTPUT DELTA
c     .(RLBUF1(iendb),ODLLG)!SSSUP- RKGST(SSG) - MAXIMUM DISTANCE TO OUT
c
c
	EQUIVALENCE
     .(RLBUF(1),SY0ER),
     .(RLBUF(2),ERRO),
     .(RLBUF(3),SZ0ER),
     .(RLBUF(4),WTAIO),
     .(RLBUF(5),WTQOO),
     .(RLBUF(6),WTSZO),
     .(RLBUF(7),ERRP),
     .(RLBUF(8),SMXP),
     .(RLBUF(9),WTSZP),
     .(RLBUF(10),WTSYP),
     .(RLBUF(11),WTBEP),
     .(RLBUF(12),WTDH),
     .(RLBUF(13),ERRG),
     .(RLBUF(14),SMXG),
     .(RLBUF(15),ERTDNF),
     .(RLBUF(16),ERTUPF),
     .(RLBUF(17),WTruh),
     .(RLBUF(ienda),WTdhg)
C
	EQUIVALENCE
     .(RLBUF1(1),STPO),
     .(RLBUF1(2),STPP),
     .(RLBUF1(3),ODLP),
     .(RLBUF1(4),ODLLP),
     .(RLBUF1(5),STPG),
     .(RLBUF1(6),ODLG),
     .(RLBUF1(iendb),ODLLG)
C
	character*40 OPNRUP
	character*1 dummy(132)
	character*132 dummy1
	equivalence (dummy(1),dummy1)
	DIMENSION RLBUF(ienda),RLBUF1(iendb)
C
	OPEN(UNIT=9, file=OPNRUP, status='OLD')
C
C... FIRST, FILL RLBUF; READ A LINE AND DETERMINE ITS PURPOSE
C
	I = 1
  100	CONTINUE
	READ(9,1000,END=300) DUMMY1
	IF(DUMMY(1) .EQ. '!') GO TO 100
	read(dummy1(1:20), 1010, err=400) rlbuf(i)
	I = I + 1
	IF(I .EQ. ienda1) GO TO 200
	GO TO 100
C
C... NOW, FILL RLBUF1
C
  200	I = 1
  210	CONTINUE
	READ(9,1000,END=300) DUMMY1
	IF(DUMMY(1) .EQ. '!') GO TO 210
	read(dummy1(1:20), 1010, err=400) rlbuf1(i)
	I = I + 1
	IF(I .EQ. iendb1) GO TO 260
	GO TO 210
C
C... NOW, PICK UP NOBS
C
  260	CONTINUE
	READ(9,1000,END=300) DUMMY1
	IF(DUMMY(1) .EQ. '!') GO TO 260
	read(dummy1(1:20), 1010, err=400) rbuf
	NOBS = NINT(RBUF)
C
C... EXIT THE PROCEEDINGS
C
	CLOSE(UNIT=9)
	RETURN
C
  300	call trap(20,0)
  400	CALL trap(21,0)
C
 1000	FORMAT(A132)
 1010	FORMAT(10X,G10.4)
	END
