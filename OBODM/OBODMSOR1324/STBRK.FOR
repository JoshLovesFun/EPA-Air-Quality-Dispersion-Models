CCPCITS
      interface to integer*2 Function SETCBRK[C](ibrk)
      integer*2 ibrk
      end
      Subroutine STBRK(ibrk)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - STBRK Version 01.1                          ---
CC--- Subroutine description - sets CTRL-C break                    ---
CC---------------------------------------------------------------------
CC
CC
CC    ibrk : 0 - break sensed only on i/o to external device (inc
CC               terminal).
CC           1 - break sensed at all system calls.
CC
CC*********************************************************************
CC
      integer*2 SETCBRK,ibrk,ii
CC
CC*********************************************************************
CC
      ii = SETCBRK(ibrk)
      return
      end
CCPCITE
