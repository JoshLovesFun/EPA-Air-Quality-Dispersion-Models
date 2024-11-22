	SUBROUTINE ESTRT1(OPNRUP)
c
C............................................................
C
C	ROUTINE TO GET RUN PARAMETERS FROM the ER1 file.
C
c............................................................
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

C
	include 'DEG1.prm'
C
	parameter(	iend= 18,
     .			iend1= iend+1,
     .			iiiend= 2,
     .			iiiend1= iiiend+1,
     .			iiend= 2,
     .			iiend1= iiend+1,
     .			jend= 3,
     .			jend1= jend+1,
     .			mend= 5,
     .			mend1= mend+1)
c
C
	COMMON
     ./alpcom/	ialpfl,alpco
     ./csprd/	ce, delrmn
     ./ERROR/	STPIN,ERBND,WTRG,WTtm,WTya,wtyc,wteb,wtmb,XLI,
     .		XRI,EPS,ZLOW,STPINZ,ERBNDZ,SRCOER,srcss,srccut,
     .		ERNOBL,NOBLpt,crfger,epsilon
     ./phicom/	iphifl,dellay
     ./SZFC/	szstp0,szerr,szsz0
     ./vucom/	vua,vub,vue,vud,vuflag
C
	logical vuflag
C
C
c	EQUIVALENCE
c     .(RLBUF(1),STPIN),   !MAIN - RKGST - INITIAL STEP SIZE
c     .(RLBUF(2),ERBND),   !MAIN - RKGST - ERROR BOUND
c     .(RLBUF(3),WTRG),    !MAIN - RKGST - WEIGHT FOR RG
c     .(RLBUF(4),WTtm),    !MAIN - RKGST - WEIGHT FOR Total mass
c     .(RLBUF(5),WTya),    !MAIN - RKGST - WEIGHT FOR Ya
c     .(RLBUF(6),WTyc),    !MAIN - RKGST - WEIGHT FOR Yc
c     .(RLBUF(7),WTeb),    !MAIN - RKGST - WEIGHT FOR Energy Balance
c     .(RLBUF(8),WTmb),    !MAIN - RKGST - WEIGHT FOR Momentum Balance
c     .(RLBUF(9),XLI),     !ALPH - LOWER LIMIT OF SEARCH FOR ALPHA
c     .(RLBUF(10),XRI),    !ALPH - UPPER LIMIT OF SEARCH FOR ALPHA
c     .(RLBUF(11),EPS)     !ALPH - ERROR BOUND USED BY "ZBRENT"
c	equivalence
c     .(RLBUF(12),ZLOW),   !ALPHI - BOTTOM HEIGHT FOR FIT OF ALPHA
c     .(RLBUF(13),STPINZ), !ALPHI - INITIAL RKGST STEP <0.
c     .(RLBUF(14),ERBNDZ), !ALPHI - ERROR BOUND FOR RKGST
c     .(RLBUF(15),SRCOER), !SRC1O - OUTPUT error criteria
c     .(RLBUF(16),SRCSS),  !SRC1O - min time for Steady;4*STPMX
c     .(RLBUF(17),SRCcut), !SRC1O - min height for blanket
c     .(RLBUF(iend),ERNOBL), !NOBL - CONVERGENCE CRITERIA
c     .(RLBUFi(1),crfger), !CRFG - Error criteria for building tables
c     .(RLBUFi(iiiend),epsilon)!SRC1 - Coefficient in Air entrainment
c
c	equivalence
c     .(rlbufa(1),ce),	!SRC1 - Coefficient gravity slumping EQ
c     .(RLBUFa(iiend),delrmn) ! stop spread for delrho<delrmn
c
c	equivalence
c     .(rlbuf1(1),szstp0),  ! SZF - Initial step size
c     .(rlbuf1(2),szerr),   ! SZF - Error criteria
c     .(rlbuf1(3),szsz0)    ! SZF - Initial value of rho*dellay*UHeff
C
c
c	equivalence
c     .(rlbuf4(1),vua),	! Constant Av in SRC1
c     .(rlbuf4(2),vub),	! Constant Bv in SRC1
c     .(rlbuf4(3),vue),	! Constant Ev in SRC1
c     .(rlbuf4(4),vud),	! Constant Dv in SRC1
c     .(rlbuf4(5),vudelta)	! Constant DELTAv in SRC1
c
	EQUIVALENCE
     .(RLBUF(1),STPIN),
     .(RLBUF(2),ERBND),
     .(RLBUF(3),WTRG),
     .(RLBUF(4),WTtm),
     .(RLBUF(5),WTya),
     .(RLBUF(6),WTyc),
     .(RLBUF(7),WTeb),
     .(RLBUF(8),WTmb),
     .(RLBUF(9),XLI),
     .(RLBUF(10),XRI),
     .(RLBUF(11),EPS)
	equivalence
     .(RLBUF(12),ZLOW),
     .(RLBUF(13),STPINZ),
     .(RLBUF(14),ERBNDZ),
     .(RLBUF(15),SRCOER),
     .(RLBUF(16),SRCSS),
     .(RLBUF(17),SRCcut),
     .(RLBUF(iend),ERNOBL),
     .(RLBUFi(1),crfger),
     .(RLBUFi(iiiend),epsilon)
c
	equivalence
     .(rlbufa(1),ce),
     .(RLBUFa(iiend),delrmn)
c
	equivalence
     .(rlbuf1(1),szstp0),
     .(rlbuf1(2),szerr),
     .(rlbuf1(3),szsz0)
C
c
	equivalence
     .(rlbuf4(1),vua),
     .(rlbuf4(2),vub),
     .(rlbuf4(3),vue),
     .(rlbuf4(4),vud),
     .(rlbuf4(5),vudelta)
c
	character*40 OPNRUP
	character*1 DUMMY(132)
	character*132 DUMMY1
	equivalence (dummy(1), dummy1)
	DIMENSION RLBUF(iend), rlbufi(iiiend), rlbufa(iiend)
	dimension rlbuf1(jend)
	dimension rlbuf4(mend)
c
C
	OPEN(UNIT=9, file=OPNRUP, status='OLD', err=2000)
C
C... READ A LINE AND DETERMINE ITS PURPOSE
C
	I = 1
  100	CONTINUE
	READ(9,1000,END=350) DUMMY1
	IF(DUMMY(1) .EQ. '!') GO TO 100
	read(dummy1(1:20), 1010, err=400) rlbuf(i)
	I = I + 1
	if(i .eq. iend1) goto 110
	GO TO 100

  110	CONTINUE
	READ(9,1000,END=350) DUMMY1
	IF(DUMMY(1) .EQ. '!') GO TO 110
	read(dummy1(1:20), 1010, err=400) ptnobl
	NOBLPT = NINT(PTNOBL)

	I = 1
  120	CONTINUE
	READ(9,1000,END=350) DUMMY1
	IF(DUMMY(1) .EQ. '!') GO TO 120
	read(dummy1(1:20), 1010, err=400) rlbufi(i)
	I = I + 1
	if(i .eq. iiiend1) goto 140
	GO TO 120
C
C... READ A LINE AND DETERMINE ITS PURPOSE for /csprd/
C
  140	I = 1
  150	CONTINUE
	READ(9,1000,END=350) DUMMY1
	IF(DUMMY(1) .EQ. '!') GO TO 150
	read(dummy1(1:20), 1010, err=400) rlbufa(i)
	I = I + 1
	if(i .eq. iiend1) goto 190
	GO TO 150
C
C... READ A LINE AND DETERMINE ITS PURPOSE to fill szfc
C
  190	I = 1
  200	CONTINUE
	READ(9,1000,END=350) DUMMY1
	IF(DUMMY(1) .EQ. '!') GO TO 200
	read(dummy1(1:20), 1010, err=400) rlbuf1(i)
	I = I + 1
	if(i .eq. jend1) goto 230
	GO TO 200
c
C... READ A LINE AND DETERMINE ITS PURPOSE to fill /alpcom/
C
  230	I = 1
  240	CONTINUE
	READ(9,1000,END=350) dummy1
	IF(DUMMY(1) .EQ. '!') GO TO 240
	read(dummy1(1:20), 1010, err=400) ralpfl

	ialpfl = nint(ralpfl)

  250	CONTINUE
	READ(9,1000,END=350) DUMMY1
	IF(DUMMY(1) .EQ. '!') GO TO 250
	read(dummy1(1:20), 1010, err=400) alpco
c
C... READ A LINE AND DETERMINE ITS PURPOSE to fill /phicom/
C
  260	I = 1
  270	CONTINUE
	READ(9,1000,END=350) DUMMY1
	IF(DUMMY(1) .EQ. '!') GO TO 270
	read(dummy1(1:20), 1010, err=400) rphifl

	iphifl = nint(rphifl)

  275	CONTINUE
	READ(9,1000,END=350) DUMMY1
	IF(DUMMY(1) .EQ. '!') GO TO 275
	read(dummy1(1:20), 1010, err=400) dellay
c
C... READ A LINE AND DETERMINE ITS PURPOSE to fill /vucom/
C
  280	I = 1
  290	CONTINUE
	READ(9,1000,END=350) DUMMY1
	IF(DUMMY(1) .EQ. '!') GO TO 290
	read(dummy1(1:20), 1010, err=400) rlbuf4(i)
	I = I + 1
	if(i .eq. mend1) goto 300
	GO TO 290
c
C... EXIT THE PROCEEDINGS
C
  300	CONTINUE
	CLOSE(UNIT=9)
	RETURN
c
  350	call trap(20,0)
C
  400	CONTINUE
	CALL trap(21,0)
C
 1000	FORMAT(A132)
 1010	FORMAT(10X,G10.4)
C
 2000	call trap(22,0)
	END
