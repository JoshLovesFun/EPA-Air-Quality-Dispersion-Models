	FUNCTION GAMMA(XX)
c
C.............................................................
C
C	SUBROUTINE GAMMA
C
c.............................................................
c
c	This routine was originally supplied by Digital Equipment
c	Corporation as part of the Scientific Subroutine Package
c	available for RT-11 as part of the Fortran Enhancement
c	Package.  It was upgraded for use in this package.
c
c.............................................................
c
C	PURPOSE
C		COMPUTES THE GAMMA FUNCTION FOR A GIVEN ARGUMENT
C
C	USAGE
C		GF = GAMMA(XX)
C
C	DESCRIPTION OF PARAMETERS
C		XX -THE ARGUMENT FOR THE GAMMA FUNCTION
C
C	IER-RESULTANT ERROR CODE WHERE
C		IER=0  NO ERROR
C		IER=1  XX IS WITHIN .000001 OF BEING A NEGATIVE INTEGER
C		IER=2  XX GT 34.5, OVERFLOW
C		IF IER .NE. 0 PROGRAM TAKES A DIP IN THE POOL!
C
C	REMARKS
C		NONE
C
C	SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C		NONE
C
C	METHOD
C		THE RECURSION RELATION AND POLYNOMIAL APPROXIMATION
C		BY C.HASTINGS,JR., 'APPROXIMATIONS FOR DIGITAL COMPUTERS',
C		PRINCETON UNIVERSITY PRESS, 1955
C
C	MODIFIED TO FUNCTION FORM FROM ORIGINAL SUBROUTINE FORM
C
C     ..................................................................
C

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	IF(XX-34.5)6,6,4
    4	IER=2
	GAMMA=1.E38
	GO TO 1000
    6	X=XX
	ERR=1.0E-6
	IER=0
	GAMMA=1.0
	IF(X-2.0)50,50,15
   10	IF(X-2.0)110,110,15
   15	X=X-1.0
	GAMMA=GAMMA*X
	GO TO 10
   50	IF(X-1.0)60,120,110
C
C        SEE IF X IS NEAR NEGATIVE INTEGER OR ZERO
C
   60	IF(X-ERR)62,62,80
   62	Y=FLOAT(INT(X))-X
	IF(ABS(Y)-ERR)130,130,64
   64	IF(1.0-Y-ERR)130,130,70
C
C        X NOT NEAR A NEGATIVE INTEGER OR ZERO
C
   70	IF(X-1.0)80,80,110
   80	GAMMA=GAMMA/X
	X=X+1.0
	GO TO 70
  110	Y=X-1.0
	GY=1.0+Y*(-0.5771017+Y*(+0.9858540+Y*(-0.8764218+Y*
     .(+0.8328212+Y*(-0.5684729+Y*(+0.2548205+Y*(-0.05149930)))))))
	GAMMA=GAMMA*GY
  120	RETURN
  130	IER=1
 1000	CONTINUE
	IF(IER.EQ.1) WRITE(5,1100)
	IF(IER.EQ.2) WRITE(5,1110)
 1100	FORMAT(5X,'?GAMMA?--ARGUMENT LESS THAN 0.000001')
 1110	FORMAT(5X,'?GAMMA?--ARGUMENT GREATER THAN 34.5--OVERFLOW')
	stop
	END
