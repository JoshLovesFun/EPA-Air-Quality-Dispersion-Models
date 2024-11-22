CCPCITS
      interface to integer*2 Function GETBD[C]
      end
      Subroutine QVIDB(ibrd)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - QVIDB Version 01.1                          ---
CC--- Subroutine description - detects type of installed graphics   ---
CC---                          board.                               ---
CC---------------------------------------------------------------------
CC
CC    ibrd - returned graphics board index number.
CC           0 - unknown video card.
CC           1 - installed CGA card.
CC           2 - installed EGA card.
CC           3 - installed VGA card.
CC
CC**********************************************************************
CC
      integer*2 ibrd,jbrd,GETBD
CC
CC**********************************************************************
CC
      ibrd = 0
      jbrd = GETBD()
      if (jbrd .eq. 2) ibrd = 1
      if (jbrd .eq. 4.or.jbrd .eq. 5) ibrd = 2
      if (jbrd .eq. 7.or.jbrd .eq. 8) ibrd = 3
      return
      end
CCPCITE
