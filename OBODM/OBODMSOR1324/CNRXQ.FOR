      Function CNRXQ(cmnmx)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - CNRXQ Version 01.0                          ---
CC--- Subroutine description - determine the next larger or smaller ---
CC---                          number of the same power of 10.      ---
CC---------------------------------------------------------------------
CC
CC*********************************************************************
CC
      integer*2 kk
      integer*4 ii,jj
CC
CC*********************************************************************
CC
      CNRXQ = cmnmx
      cmn = cmnmx
      if (cmn .lt. 0.0.or.cmn .gt. 0.0) then
        sgn = 1.0
        if (cmn .lt. 0.0) then
          sgn = -1.0
          cmn = ABS(cmn)
        end if
        kk = 0
   10   if (cmn .lt. 1.0) then
          kk = kk + 1
          cmn = cmn * 10.0
          go to 10
        end if
        dval = ALOG10(cmn)
        ii = dval
        jj = cmn * 10.0**(-ii)
        dval = jj
        if (dval * 10.0**ii .lt. cmn) dval = dval + 1.0
        CNRXQ = sgn * dval * 10.0**(ii-kk)
      end if
      return
      end
