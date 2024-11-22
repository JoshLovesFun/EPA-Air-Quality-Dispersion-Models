      Function IVGET(idumy,iindx)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IVGET Version 01.0                          ---
CC--- Subroutine description - get a value from an integer that is a---
CC---                          multiple value integer.              ---
CC---------------------------------------------------------------------
CC
CC        idumy  - integer containing the desired value.
CC        iindx  - area to get value from.
CC                 1 = value in 1's position.
CC                 2 = value in 10's position.
CC                 3 = value in 100's position.
CC                 4 = value in 1000's position.
CC
CC        IVGET  - returned integer value.
CC
CC**********************************************************************
CC
      IVGET = idumy / 10**iindx
      IVGET = idumy / 10**(iindx-1) - IVGET * 10
      return
      end
