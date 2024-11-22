      Function IZGET(data,npnts,nptst)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IZGET Version 01.0                          ---
CC--- Subroutine description - determines the number of non-missing ---
CC---                          data points in an array and returns  ---
CC---                          the index of the last non-missing    ---
CC---                          data point.                          ---
CC---------------------------------------------------------------------
CC
CC    data  - array of real data values.
CC    npnts - number of values in array data.
CC    nptst - returned number of non-missing data values.
CC
CC    returns - IZGET - index of last non-missing data value.
CC
CC*********************************************************************
CC
      integer*2 n
CC
      dimension data(1000)
CC
CC*********************************************************************
CC
      IZGET = 0
      nptst = 0
      do 10 n=1,npnts
CC        if missing.
        if (INONE(data(n)) .gt. 0) then
CC          accumulate number of non-missing.
          nptst = nptst + 1
CC          set last point index.
          IZGET = n
        end if
   10 continue
      return
      end
