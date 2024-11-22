CCPCITS
      Subroutine SHDOW(icolb,irowb,ncols,nrows,ifore,iback)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - SHDOW version 01.1                          ---
CC--- Subroutine description - generate shadow for scroll/pop-up box---
CC---------------------------------------------------------------------
CC
CC    icolb - left column of box
CC    irowb - bottom row of box
CC    ncols - number of columns in box
CC    nrows - number of rows in box
CC    ifore - foreground color of shadow area (usually grey)
CC    iback - background color of shadow area (usually black)
CC
CC*********************************************************************
CC
      integer*2 icolb,irowb,ncols,nrows,ifore,iback,jchar,iloc,jloc,
     *          nrept,idum
CC
CC*********************************************************************
CC
      nrept = 1
CC
CC      below box
CC
      ibeg = icolb + 2
      iend = icolb + ncols + 1
      jloc = irowb - 1
CC
      do 10 i=ibeg,iend
        iloc = i
        call QCMOV(iloc,jloc)
        call QRTXT(jchar,idum,idum)
        call QSTXT(jchar,iback,ifore,nrept)
   10 continue
CC
CC      to the right of box
CC
      jbeg = irowb
      jend = irowb + nrows - 2
      ibeg = icolb + ncols
      iend = ibeg + 1
      do 30 j=jbeg,jend
        jloc = j
        do 20 i=ibeg,iend
          iloc = i
          call QCMOV(iloc,jloc)
          call QRTXT(jchar,idum,idum)
          call QSTXT(jchar,iback,ifore,nrept)
   20   continue
   30 continue
      return
      end
CCPCITE
