	FUNCTION AFGEN2(XTAB,TAB,X,SPEC)
c
C..............................................................
C
C	This function linearly interpolates from the given
C	    table of data points.  (Modeled after CSMP's AFGEN.)
C
c..............................................................
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

C
	include 'DEG1.prm'

	common/nend/	pound
c
	character*(*) SPEC
	DIMENSION XTAB(1),TAB(1)
C
c... check to see if the value is in (or beyond) the table
c
	IF(X .GE. XTAB(1)) GO TO 95
	WRITE(lunlog,1100) x,spec
	AFGEN2 = TAB(2)
	RETURN
C
c... loop through the table to find the points to interpolate from
c
   95	continue
	ix = 1
  100	Ix = ix + 1
C
	IF( XTAB(IX).EQ.POUND ) GO TO 500
	IF(X .GE. XTAB(IX)) GO TO 100
C
c... perform the interpolation
c
	IXP = IX-1
C
	SL = (TAB(IX) - TAB(IXP))/(XTAB(IX) - XTAB(IXP))
	AFGEN2 = SL*(X - XTAB(IXP)) + TAB(IXP)
	RETURN
C
c... extrapolate if necessary
c
  500	CONTINUE
	IX = IX-1
	IXP = IX-1
C
	SL = (TAB(IX) - TAB(IXP))/(XTAB(IX) - XTAB(IXP))
	AFGEN2 = SL*(X - XTAB(IXP)) + TAB(IXP)
C
 1100	FORMAT(2X,'?AFGEN2? UNDERFLOW; argument: ',1pg13.5,5X,A20)
	RETURN
	END
