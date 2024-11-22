	SUBROUTINE ESTRT3(OPNRUP)
c
C..........................................................
C
C	ROUTINE TO GET RUN PARAMETERS FROM the ER3 file.
C
c..........................................................
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )
C
	COMMON
     ./csigx/	sigxco,sigxp,sigxmd,sigxfl
     ./ERROR/	ERT1,ERDT,ERNTIM
     ./PHLAG/	CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
C
	EQUIVALENCE
     .(RLBUF(1),ERT1),    !FIRST SORT TIME - USER OPTION
     .(RLBUF(2),ERDT),    !SORT TIME DELTA - USER OPTION
     .(RLBUF(3),ERNTIM)   !NUMBER OF SORT TIMES - USER OPTION
C
	LOGICAL CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5

	character*1 DUMMY(132)
	character*132 dummy1
	equivalence (dummy(1),dummy1)

	character*40 opnrup
	DIMENSION RLBUF(3),RBUF(6)
C
	OPEN(UNIT=9, file=OPNRUP, status='OLD')
C
C... READ A LINE AND DETERMINE ITS PURPOSE
c
	I = 1
  100	CONTINUE
	READ(9,1000,END=300) DUMMY1
	IF(DUMMY(1) .EQ. '!') GO TO 100
	read(dummy1(1:20), 1010, err=400) RBUF(I)
	I = I + 1
	GO TO 100
C
C... Exit the PROCEEDINGS AND DETERMINE CHECK5
c
  300	CONTINUE
C
	DO 310 I = 1,3
  310	RLBUF(I) = RBUF(I)
	CHECK5 = .FALSE.		! IN ORDER FOR FLAG TO WORK
	IF(RBUF(4) .EQ. 1.) CHECK5 = .TRUE.
C
	sigxfl = rbuf(5)
	CLOSE(UNIT=9)
	RETURN
C
  400	CALL trap(21,0)
 1000	FORMAT(A132)
 1010	FORMAT(10X,G10.4)
	END
