CCPCITS
      Subroutine POPBX(icolb,irowb,ncols,nrows,itype,ifore,iback,ishad,
     *                 ifors,ibcks)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - POPBX version 01.1                          ---
CC--- Subroutine description - generate pop-up box outline          ---
CC---------------------------------------------------------------------
CC
CC    icolb - left column
CC    jrowb - bottom row
CC    ncols - number of columns
CC    nrows - number of rows
CC    itype - perimeter line type ( 0 = single, 1 = double )
CC    ifore - foreground color of perimeter
CC    iback - background color of perimeter
CC    ishad - shadow pop-up box ( 0 = no, 1 = yes )
CC    ifors - shadow foreground color (usually grey)
CC    ibcks - shadow background color (usually black)
CC
CC*********************************************************************
CC
      integer*2 icolb,irowb,ncols,nrows,itype,ifore,iback,ishad,
     *          ifors,ibcks,nrept
CC
CC*********************************************************************
CC
      nrept = 1
CC
CC      print basic box outline
      call FRAME(icolb,irowb,ncols,nrows,itype,ifore,iback)
CC
CC      shadow pop-up box
CC
      if (ishad .eq. 1) then
        call SHDOW(icolb,irowb,ncols,nrows,ifors,ibcks)
      end if
CC
      return
      end
CCPCITE
