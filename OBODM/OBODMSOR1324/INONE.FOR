      Function INONE(x)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - INONE Version 01.0                          ---
CC--- Subroutine description - determines if a data item is missing ---
CC---                          or not.                              ---
CC---------------------------------------------------------------------
CC
CC        INONE =  0, if missing (value is > rmissc and <= rmissd).
CC        INONE =  1, if not missing (> rmissd).
CC        INONE = -1, computed if not entered (<= rmissc).
CC
CC*********************************************************************
CC
      data rmissd/-1.0e20/,rmissc/-1.0e21/
CC
CC*********************************************************************
CC
      if (x .le. rmissc) then
        INONE = -1
      else if (x .le. rmissd) then
        INONE = 0
      else
        INONE = 1
      end if
      return
      end
