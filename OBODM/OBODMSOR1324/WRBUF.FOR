CCPCITS
      interface to Subroutine WRKEY[C](iscan,ichar)
      integer*2 iscan,ichar
      end
      Subroutine WRBUF(iscan,ichar)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - WRBUF Version 01.1                          ---
CC--- Subroutine description - write character to keyboard buffer.  ---
CC---------------------------------------------------------------------
CC
CC     iscan - scan number (1 -83)
CC     ichar - character ascii number
CC
CC    behavior appears independent of iscan.  however, ichar is placed
CC    correctly in keyboard buffer.
CC
CC*********************************************************************
CC
      integer*2 iscan,ichar
CC
CC*********************************************************************
CC
      call WRKEY(iscan,ichar)
      return
      end
CCPCITE
