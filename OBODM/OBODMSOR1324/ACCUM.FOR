      Function ACCUM(index,value)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - ACCUM Version 01.0                          ---
CC--- Subroutine description - accumulate dispersion calcs.         ---
CC---------------------------------------------------------------------
CC
CC        index  - index in array calcs to use.
CC        value  - value to accumulate in calcsa(index)
CC
CC        returns -
CC        ACCUM  - returns the value of argument value.
CC
CC**********************************************************************
CC
      include 'OCALCS.INC'
CC
CC**********************************************************************
CC
      ACCUM = value
      calcsa(index) = calcsa(index) + value
      return
      end
