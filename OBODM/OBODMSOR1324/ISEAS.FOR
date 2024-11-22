      Function ISEAS(imon,ifsq)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - ISEAS Version 01.1                          ---
CC--- Subroutine description - return the season or quarter number  ---
CC---                          in which the month falls.            ---
CC---------------------------------------------------------------------
CC
CC    imon - month (1 to 12).
CC    ifsq - season/quarter flag.
CC           1 = season.
CC           2 = quarter.
CC
CC    returns - ISEAS - season or quarter number (1 to 4).
CC
CC*********************************************************************
CC
      integer*2 i,j,isean
CC
      dimension isean(12,2)
CC
      data isean/1,1,2,2,2,3,3,3,4,4,4,1,
     *           1,1,1,2,2,2,3,3,3,4,4,4/
CC
CC*********************************************************************
CC
      i = MIN0(MAX0(imon,1),12)
      j = MIN0(MAX0(ifsq,1),2)
      ISEAS = isean(i,j)
      return
      end
