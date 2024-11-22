CCPCITS
      interface to Subroutine GETDM[C](imode,icol)
      integer*2 imode[REFERENCE],icol[REFERENCE]
      end
      Subroutine QRMOD(imode,icol)
CC
CC--------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - QRMOD Version 01.1                          ---
CC--- Subroutine description - reads mode and resolution (number of ---
CC---                          columns) currently in effect on crt  ---
CC---                          display.                             ---
CC---------------------------------------------------------------------
CC
CC    imode - returned current crt mode.
CC    icol  - returned current number of columns.
CC
CC**********************************************************************
CC
      integer*2 imode,icol
CC
      include 'OCNTRL.INC'
CC
CC**********************************************************************
CC
      call GETDM(imode,icol)
      isvmod = imode
      return
      end
CCPCITE
