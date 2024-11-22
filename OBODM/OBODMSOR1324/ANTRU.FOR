      Function ANTRU(angl)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - ANTRU Version 01.0                          ---
CC--- Subroutine description - returns the positive angle modulo 360---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      ANTRU = AMOD(angl,360.0)
      if (ANTRU .le. 0.0) ANTRU = ANTRU + 360.0
      return
      end
