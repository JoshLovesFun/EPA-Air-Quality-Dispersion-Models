      Function IFFOP(mdumy,num)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IFFOP Version 01.0                          ---
CC--- Subroutine description - compare entered alpha character and  ---
CC---                          return index-1.                      ---
CC---------------------------------------------------------------------
CC
CC        mdumy  - character*1 option entered
CC        num    - number of possible characters - a,b,....
CC
CC**********************************************************************
CC
      character*1 mdumy
CC
CC**********************************************************************
CC
      IFFOP = -1
      do 10 i=1,num
        if (IFCHR(mdumy,i) .ne. 0) IFFOP = i - 1
   10 continue
      return
      end
