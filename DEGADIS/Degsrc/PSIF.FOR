	FUNCTION PSIF(Z,rml)
c
C.............................................................
C
C	FUNCTION PSI. As suggested by Colenbrander --
C
C    THIS FUNCTION HAS BEEN DERIVED FROM BUSINGER,J.A.
C	WORKSHOP ON MICROMETEOROLOGY, CHAPTER 2, HAUGEN,D.A. (ED.)
C	AMERICAN METEOROLOGICAL SOCIETY.
C
c..............................................................
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )
C
	include 'DEG1.prm'
c
	IF( rml ) 10,20,30
C
 10	A = (1.D0-15.D0*Z/rml)**.25D0
	PSIF = 2.D0*dLOG((1.D0+A)/2.D0) + dLOG((1.D0+A*A)/2.D0)
     .		- 2.D0*daTAN(A) + PI/2.D0
	RETURN
C
 20	PSIF = 0.D0
	RETURN
C
 30	PSIF = -4.7D0*Z/rml
	RETURN
	END
