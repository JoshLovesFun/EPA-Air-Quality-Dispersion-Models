c
c
c... Interface routine for getting the date and time as a character string
c
	subroutine dattim(time)
c
	character*24 time
c
c
	integer*4 ihr, imin, isec, i100, iyr, imon, iday

	character*3 month(12)

	data month/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG',
     .		'SEP','OCT','NOV','DEC'/
c
c... use GETTIM and GETDAT to get the system time and date.
c
	call gettim(ihr, imin, isec, i100)
	call getdat(iyr, imon, iday)
c
	write(time, 100) iday, month(imon), iyr, ihr, imin, isec, i100
 100	format(i2,'-',a3,'-',i4,' ',i2,':',i2,':',i2,'.',i2)

	return
	end
C
C     INTEFRACE ROUTINE FROM DEC TO PC FOR GETTING TIME
C
      FUNCTION SECNDS(X)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     RETURN CURRENT TIME - MIDNIGHT - X
C
C
	real*8 secnds
      REAL*8    X
C
C
      INTEGER*4 IHOUR, IMINUT, ISECON, IHUND
      REAL*8    RHOUR, RMINUT, RSECON, RHUND
      REAL*8    X1
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
