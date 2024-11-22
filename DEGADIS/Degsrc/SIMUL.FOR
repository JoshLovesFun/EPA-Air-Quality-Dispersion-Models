	subroutine SIMUL(N,A,X)
C
c.......................................................................
c
C	SIMUL is adapted from 'APPLIED NUMERICAL METHODS' by
C	B. Carnahan, H.A. Luther, AND J.O. Wilkes, published by J. Wiley
C	and Sons, INC. 1969. pages 290 to 291 for a maximum pivot strategy.
c	In CL&W, SIMUL uses an argument switch for control purposes.  In this
c	routine, only the positive flag INDIC is applied which solves the
c	set of equations.
C
C	The set of linear equations is solved but the inverse is not
c	computed in place.  The Gauss-Jordan complete elimination method
C	is employed with the maximum pivot strategy.  Row and column
C	subscripts of successive pivot elements are saved in order in
c	the IROW and JCOL arrays, respectively.  K is the pivot counter,
C	PIVOT the algebraic value of the pivot element, MAX
C	the number of columns in A, and DETER the determinant of the
c	coefficient matrix.  The solutions are computed in the (J+1)st
C	column of A and then unscrambled and put in proper order in
C	X(1)...X(N) using the pivot subscript information available
C	in the IROW and JCOL arrays.  Should the potential
c	pivot or largest magnitude be smaller than eps,
c	the matrix is considered to be singular.
c
c.......................................................................
c

	implicit real*8(a-h,o-z), integer*4(i-n)

c
c...dimensions are set for a 5 by 5 case.....................................
c
	parameter (eps=1.0D-10,
     .              nnn = 5,
     .              np1 = nnn+1)


	DIMENSION IROW(nnn),JCOL(nnn)
	dimension A(n,n+1)
	dimension X(n)
C

	MAX=N+1
C
C... BEGIN ELIMINATION PROCEDURE .........................................

	DO 18 K=1,N
	KM1=K-1
C
C... SEARCH FOR THE PIVOT ELEMENT .........................................

	PIVOT=0.
	DO 11 I=1,N
	DO 11 J=1,N
C
C... SCAN IROW AND JCOL ARRAYS FOR INVALID PIVOT SUBSCRIPTS

	    IF (K.EQ.1) GO TO 9
		DO 8 ISCAN=1,KM1
		DO 8 JSCAN=1,KM1
		    IF (I.EQ.IROW(ISCAN)) GO TO 11
		    IF (J.EQ.JCOL(JSCAN)) GO TO 11
    8		CONTINUE
    9	    IF ( ABS(A(I,J)).LE. ABS(PIVOT)) GO TO 11
	PIVOT=A(I,J)
	IROW(K)=I
	JCOL(K)=J
   11	CONTINUE
C
C... INSURE THAT SELECTED PIVOT IS LARGER THAN EPS

	IF ( ABS(PIVOT).GT.EPS) GO TO 13
	    WRITE(6,202) abs(pivot)
	    WRITE(3,202) abs(pivot)
	    stop
C
C... NORMALIZE PIVOT ROW ELEMENTS
c
   13	IROWK=IROW(K)
	JCOLK=JCOL(K)

	DO 14 J=1,MAX
   14	A(IROWK,J)=A(IROWK,J)/PIVOT
C
C... CARRY OUT ELIMINATION ....................................................
c
	A(IROWK,JCOLK)=1.D0/PIVOT

	DO 18 I=1,N
	AIJCK=A(I,JCOLK)
	IF (I.EQ.IROWK) GO TO 18
	A(I,JCOLK) = -AIJCK/PIVOT
	  DO 17 J=1,MAX
   17	  IF (J.NE.JCOLK) A(I,J)=A(I,J)-AIJCK*A(IROWK,J)
   18	CONTINUE
C
C... ORDER SOLUTION VALUES (IF ANY) ...........................................

	DO 20 I=1,N
	IROWI=IROW(I)
	JCOLI=JCOL(I)
   20	X(JCOLI) = A(IROWI,MAX)
C
	RETURN
  202	FORMAT(' SMALL PIVOT - MATRIX MAY BE SINGULAR; pivot: ',1pg13.5)
	END
