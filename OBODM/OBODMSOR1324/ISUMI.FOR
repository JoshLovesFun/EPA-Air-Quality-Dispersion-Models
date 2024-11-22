      Function ISUMI(iaray,n)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - ISUMI Version 01.0                          ---
CC--- Subroutine description - sum over n locations of integer      ---
CC---                          array iaray.                         ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      integer*2 i
CC
      dimension iaray(1000)
CC
CC**********************************************************************
CC
      ISUMI = 0
      do 10 i=1,n
        ISUMI = ISUMI + iaray(i)
   10 continue
      return
      end
