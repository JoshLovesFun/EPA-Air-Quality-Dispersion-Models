      Function IVSTO(idumy,iindx,ivals)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IVSTO Version 01.0                          ---
CC--- Subroutine description - store a value into an integer that is---
CC---                          a multiple value integer.            ---
CC---------------------------------------------------------------------
CC
CC        idumy  - integer to store the desired value into.
CC        iindx  - area to store into.
CC                 1 = value in 1's position.
CC                 2 = value in 10's position.
CC                 3 = value in 100's position.
CC                 4 = value in 1000's position.
CC        ivals  - integer value to store.
CC
CC        IVSTO  - returned merged multiple value integer
CC
CC**********************************************************************
CC
      IVSTO = IVGET(idumy,iindx) * 10**(iindx-1)
      IVSTO = idumy - IVSTO + ivals * 10**(iindx-1)
      return
      end
