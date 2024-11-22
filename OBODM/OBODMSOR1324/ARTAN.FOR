      Function ARTAN(a,b)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - ARTAN Version 01.0                          ---
CC--- Subroutine description - returns the arctan of a/b, protecting---
CC---                          against a and b equal to zero.       ---
CC---------------------------------------------------------------------
CC
CC    a - numerator.
CC    b - denominator.
CC
CC    returns - ARTAN = arctan(a/b).
CC
CC**********************************************************************
CC
      double precision aa,bb
CC
CC**********************************************************************
CC
      aa = a
      bb = b
      ARTAN = 0.0
      if (b) 20,10,20
   10 if (a) 20,30,20
   20 aa = DATAN2(aa,bb)
      if (aa .lt. 0.0) aa = aa + 6.2831853072
      if (aa .gt. 6.2831853072) aa = aa - 6.2831853072
      ARTAN = aa
   30 return
      end
