CCPCITS
      interface to Subroutine SETPX[C](icol,irow,icolr)
      integer*2 icol,irow,icolr
      end
      Subroutine QLINE(jcol1,jrow1,jcol2,jrow2,icolr)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - QLINE Version 01.1                          ---
CC--- Subroutine description - draws a line from pixel (column,row) ---
CC---                          = (jcol1,jrow1) to pixel (column,row)---
CC---                          = (jcol2,jrow2) with color number    ---
CC---                          icolr from the current palette       ---
CC---                          choice.                              ---
CC---------------------------------------------------------------------
CC
CC    jcol1  - line start column.
CC    jrow1  - line start row.
CC    jcol2  - line end column.
CC    jrow2  - line end row.
CC    icolr  - color number from current palette.
CC
CC**********************************************************************
CC
      integer*2 jcol1,jrow1,jcol2,jrow2,icol1,irow1,icol2,irow2,
     *          icolr,icol,irow
CC
      include 'OCNTRL.INC'
CC
CC**********************************************************************
CC
      inpix = 1
CC
      icol1 = jcol1 - 1
      icol2 = jcol2 - 1
      irow1 = imypix - (jrow1 + 1)
      irow2 = imypix - (jrow2 + 1)
CC
      idrow = irow2 - irow1
      idcol = icol2 - icol1
      nrow = IABS(idrow) + 1
      ncol = abs(idcol) + 1
CC
      if (nrow .eq. 1) then
        irow = irow1
        icol = icol1
        if (idcol .lt. 0) inpix = -1
        do 10 n=1,ncol
          call SETPX(icol,irow,icolr)
          icol = icol + inpix
   10   continue
        return
      end if
CC
      if (ncol .eq. 1) then
        irow = irow1
        icol = icol1
        if (idrow .lt. 0) inpix = -1
        do 20 n=1,nrow
          call SETPX(icol,irow,icolr)
          irow = irow + inpix
   20   continue
        return
      end if
CC
      if (IABS(idrow) .ge. IABS(idcol)) then
        grad = FLOAT(idcol)/FLOAT(idrow)
        if (idrow .lt. 0) inpix = -1
        irow = irow1
        do 30 n=1,nrow
          icol = icol1 + NINT(grad*(irow-irow1))
          call SETPX(icol,irow,icolr)
          irow = irow + inpix
   30   continue
      else
        grad = FLOAT(idrow)/FLOAT(idcol)
        if (idcol .lt. 0) inpix = -1
        icol = icol1
        do 40 n=1,ncol
          irow = irow1 + NINT(grad*(icol-icol1))
          call SETPX(icol,irow,icolr)
          icol = icol + inpix
   40   continue
      end if
      return
      end
CCPCITE
