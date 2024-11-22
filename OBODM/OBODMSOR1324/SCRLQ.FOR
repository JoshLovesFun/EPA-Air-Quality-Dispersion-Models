      Subroutine SCRLQ
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - SCRLQ Version 01.0                          ---
CC--- Subroutine description - scroll up the crt screen.            ---
CC---------------------------------------------------------------------
CC
      include 'OWNTRL.INC'
CC
CC**********************************************************************
CC
CCPCITS
      call QSCLR(iswplt(9))
CCPCITE
      return
      end
