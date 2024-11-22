CC-OBODM dispersion related routines
      Function FSEXP(x)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - FSEXP Version 01.0                          ---
CC--- Subroutine description - prevent underflow or overflow for    ---
CC---                          exponential.                         ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      FSEXP = EXP(AMAX1(AMIN1(x,50.0),-50.0))
      return
      end
