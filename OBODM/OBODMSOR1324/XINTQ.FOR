      Function XINTQ(x)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - XINTQ Version 01.0                          ---
CC--- Subroutine description - return the integer part of x         ---
CC---------------------------------------------------------------------
CC
CC        (for machines that default to integer*2)
CC
CC**********************************************************************
CC
      integer*4 intx
CC
CC**********************************************************************
CC
      intx = x
      XINTQ = intx
      return
      end
