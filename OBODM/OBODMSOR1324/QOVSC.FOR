CCPCITS
      interface to Subroutine SETOV[C](icolr)
      integer*2 icolr
      end
      Subroutine QOVSC(icolr)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - QOVSC Version 01.1                          ---
CC--- Subroutine description - sets the overscan register, which is ---
CC---                          IBM-ese for setting the border color ---
CC---                          in EGA graphics mode.                ---
CC---------------------------------------------------------------------
CC
CC    icolr - value to be placed in overscan register (0 - 63)
CC
CC**********************************************************************
CC
      integer*2 icolr
CC
CC**********************************************************************
CC
      call SETOV(icolr)
      return
      end
CCPCITE
