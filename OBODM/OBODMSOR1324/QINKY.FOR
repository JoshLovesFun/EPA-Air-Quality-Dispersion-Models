CCPCITS
      interface to integer*2 Function RDKEY[C]
      end
      Subroutine QINKY(ixtnd,key)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - QINKY Version 01.1                          ---
CC--- Subroutine description - read a key from the keyboard without ---
CC---                          a display on the screen (no echo mode)--
CC---------------------------------------------------------------------
CC
CC    ixtnd - returned integer indicating whether key is normal or
CC            from the extended code.  if ixtnd is zero, the an
CC            "extended code" key has been depressed (e.g. a function
CC            key.
CC    key   - returned ascii value of depressed key.
CC
CC**********************************************************************
CC
      integer*2 ixtnd,key,RDKEY
CC
CC**********************************************************************
CC
      key = RDKEY()
      ixtnd = 1
      if (key .eq. 0) then
        key = RDKEY()
        ixtnd = 0
      end if
      return
      end
CCPCITE
