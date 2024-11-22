c
C... GET THE EXECUTION TIME
C
 100	continue
	write(6,2000)
	read (5,1000) q
	t1 = secnds(0.)
	read (5,1000) q
	write(6,2005)
c
C... CALCULATE the elapsed TIME
C
	t1 = Secnds(t1)/60.
	if(t1 .le. 0.) t1 = t1 + 60*24
	WRITE(6,2010) T1
c
 1000	format(a4)
 2000	format(5x,' Hit <CR> to start timing.')
 2005	format(5x,' Hit <CR> to stop timing.')
 2010	FORMAT(5X,' ***** ELAPSED TIME ***** ',1pg13.5,' min ')
C
	goto 100
	END
c
c
c
c
C
      FUNCTION SECNDS(X)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     RETURN CURRENT TIME - MIDNIGHT - X
C
C
      REAL*4    X, secnds
C
C
      INTEGER*2 IHOUR, IMINUT, ISECON, IHUND
      REAL*4    RHOUR, RMINUT, RSECON, RHUND
      REAL*4    X1
C
      CALL GETTIM(IHOUR, IMINUT, ISECON, IHUND)
      RHOUR     =  FLOAT( IHOUR )
      RMINUT    =  FLOAT( IMINUT)
      RSECON    =  FLOAT( ISECON)
      RHUND     =  FLOAT( IHUND )
      X1        =  RHOUR*3600.0 + RMINUT*60.0 + RSECON +
     &             RHUND/100.0    
      SECNDS    =  X1 - X
C
      RETURN
      END
