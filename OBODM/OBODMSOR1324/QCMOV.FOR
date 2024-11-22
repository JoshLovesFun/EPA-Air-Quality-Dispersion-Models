CCPCITS
      interface to Subroutine CURSP[C](lpage,ix,iy)
      integer*2 lpage,ix,iy
      end
      Subroutine QCMOV(icol,irow)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - QCMOV Version 01.1                          ---
CC--- Subroutine description - cursor is moved to specified column  ---
CC---                          and row.  0,0 is lower left corner   ---
CC---                          of screen.
CC---------------------------------------------------------------------
CC
CC    icol  - text column number to move to.
CC    irow  - text row number to move to.
CC
CC**********************************************************************
CC
      integer*2 lpage,icol,irow,ix,iy
CC
      include 'OCNTRL.INC'
CC
CC**********************************************************************
CC
      lpage = ispage
      ix = icol
      iy = irow
      iy = irowmx - 1 - iy
      irdcol = ix + 1
      irdrow = iy + 1
      call CURSP(lpage,ix,iy)
      return
      end
CCPCITE
