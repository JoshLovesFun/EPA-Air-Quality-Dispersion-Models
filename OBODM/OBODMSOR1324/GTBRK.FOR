CCPCITS
      interface to integer*2 Function GETCBRK[C]
      end
      Subroutine GTBRK(ibrk)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - GTBRK Version 01.1                          ---
CC--- Subroutine description - gets CTRL-C break system setting.    ---
CC---------------------------------------------------------------------
CC
CC
CC    returns - ibrk : 0 - break sensed only on i/o to external device
CC                         inc terminal).
CC                     1 - break sensed at all system calls.
CC
CC*********************************************************************
CC
      integer*2 GETCBRK,ibrk
CC
CC*********************************************************************
CC
      ibrk = GETCBRK()
      return
      end
CCPCITE
