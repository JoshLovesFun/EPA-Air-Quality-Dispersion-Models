      Function CMXNQ(a,jj,nn,jsw)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - CMXNQ Version 01.0                          ---
CC--- Subroutine description - determine max. and min. for plot     ---
CC---                          axes.                                ---
CC---------------------------------------------------------------------
CC
CC      a      - array of values to select max and min from.
CC      jj     - max or min flag.
CC               1 = max
CC               2 = min
CC      nn     - number of values in array a.
CC      jsw    - linear or log flag.
CC               0 - linear
CC               1 - log
CC
CC**********************************************************************
CC
      integer*2 ii
CC
      dimension a(100)
CC
      data rmiss/-1.0e20/
CC
CC**********************************************************************
CC
      if (jj .eq. 1) then
        CMXNQ = rmiss
        do 10 ii=1,nn
          CMXNQ = AMAX1(CMXNQ,a(ii))
   10   continue
        if (jsw .ne. 0.and.CMXNQ .le. 0.0) then
          CMXNQ = 1.0
        end if
      else
        CMXNQ = -rmiss
        do 20 ii=1,nn
          if (a(ii) .gt. 0.0.or.jsw .eq. 0) then
            CMXNQ = AMIN1(CMXNQ,a(ii))
          end if
   20   continue
        if (jsw .ne. 0.and.CMXNQ .ge. -rmiss) then
          CMXNQ = 0.1
        end if
      end if
      if (jsw .ne. 0) then
        c = ALOG10(CMXNQ)
CC        get integer part.
        xk = XINTQ(c)
        if (jj .eq. 1) then
CC          maximum.
          if (xk .gt. 0.0) then
            if (ABS(c-xk) .gt. 0.0) then
              if (xk .ge. 0.0) xk = xk+1.0
            end if
          end if
        else
CC          minimum.
          if (xk .lt. 0.0) then
            if (ABS(c-xk) .gt. 0.0) then
              if (xk .le. 0.0) xk = xk-1.0
            end if
          end if
        end if
        CMXNQ = xk
      end if
      return
      end
