      Function ICNTR(maray,nchrs)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - ICNTR Version 01.0                          ---
CC--- Subroutine description - center characters in buffer.         ---
CC---------------------------------------------------------------------
CC
CC        maray - character*1 input array to be centered.
CC        nchrs - number of characters in maray.
CC
CC        returns - ICNTR - index of last non-blank character in array.
CC
CC********************************************************************
CC
      integer*2 i
CC
      character*1 maray
CC
      dimension maray(500)
CC
CC********************************************************************
CC
      ICNTR = LRJST(maray,nchrs,1)
      i = (nchrs - ICNTR) / 2
      if (i .gt. 0) then
        ICNTR = ICNTR + i
        i = LRJST(maray,ICNTR,0)
      end if
      return
      end
