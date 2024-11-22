	FUNCTION AFGEN(TAB,X,SPEC)
c
C..............................................................
C
C	This function linearly interpolates from the given
C	    table of data points.  (Modeled after CSMP's AFGEN.)
c
c..............................................................
C

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

C
	include 'DEG1.prm'

	common/nend/	pound
c
	character*(*) SPEC
	DIMENSION TAB(1)
C
c... see if the desired point is less than the first independent value
c
	IF(X .GE. TAB(1)) GO TO 95
	WRITE(lunlog,1100) x,spec
	AFGEN = TAB(2)
	RETURN
C
c... loop to find the position to use for interpolation
c
   95	continue
	ix = 1
  100	Ix = ix + 2
C
	IY = IX + 1
	IF( TAB(IX).EQ.POUND .AND. TAB(IY).EQ.POUND ) GO TO 500
	IF(X .GE. TAB(IX)) GO TO 100
C
c... interpolate the result
c
	IXP = IX-2
	IYP = IXP + 1
C
	SL = (TAB(IY) - TAB(IYP))/(TAB(IX) - TAB(IXP))
	AFGEN = SL*(X - TAB(IXP)) + TAB(IYP)
	RETURN
C
c... if the point is not found, extrapolate from the last two values.
c
  500	CONTINUE
	IX = IX-2
	IY = IY-2
	IXP = IX-2
	IYP = IY-2
C
	SL = (TAB(IY) - TAB(IYP))/(TAB(IX) - TAB(IXP))
	AFGEN = SL*(X - TAB(IXP)) + TAB(IYP)
C
 1100	FORMAT(2X,'?AFGEN? UNDERFLOW; argument: ',1pg13.5,5X,A20)
	RETURN
	END
