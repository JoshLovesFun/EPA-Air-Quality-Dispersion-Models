CCPCITS
      Subroutine FRAME(icolb,irowb,ncols,nrows,itype,ifore,iback)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - FRAME version 01.1                          ---
CC--- Subroutine description - generate scroll/pop-up box outline.  ---
CC---------------------------------------------------------------------
CC
CC    icolb - left column
CC    jrowb - bottom row
CC    ncols - number of columns
CC    nrows - number of rows
CC    itype - box line type ( 0 = single, 1 = double )
CC    ifore - foreground color
CC    iback - background color
CC
CC*********************************************************************
CC
      integer*2 icolb,irowb,ncols,nrows,itype,ifore,iback,jchar,iloc,
     *          jloc,nrept
CC
      dimension jchar(12)
CC
      data jchar /192,200,217,188,218,201,191,187,196,205,179,186/
CC
CC********************************************************************* c
CC
CC      print corners
CC
      iinc = ncols - 1
      jinc = nrows - 1
      ibeg = icolb
      iend = icolb + iinc
      jbeg = irowb
      jend = irowb + jinc
      numc = 0
      nrept = 1
      do 20 j=jbeg,jend,jinc
        do 10 i=ibeg,iend,iinc
          numc = numc + 1
          index = (numc - 1) * 2 + 1 + itype
          iloc = i
          jloc = j
          call QCMOV(iloc,jloc)
          call QSTXT(jchar(index),iback,ifore,nrept)
   10   continue
   20 continue
CC
CC      top and bottom
CC
      nrept = iinc - 1
      iloc = ibeg + 1
      jloc = jend
      call QCMOV(iloc,jloc)
      call QSTXT(jchar(9+itype),iback,ifore,nrept)
      jloc = jbeg
      call QCMOV(iloc,jloc)
      call QSTXT(jchar(9+itype),iback,ifore,nrept)
CC
CC      left and right
CC
      nrept = 1
      jbeg = jbeg + 1
      jend = jend - 1
      do 30 j=jbeg,jend
        jloc = j
        iloc = ibeg
        call QCMOV(iloc,jloc)
        call QSTXT(jchar(11+itype),iback,ifore,nrept)
        iloc = iend
        call QCMOV(iloc,jloc)
        call QSTXT(jchar(11+itype),iback,ifore,nrept)
   30 continue
      return
      end
CCPCITE
