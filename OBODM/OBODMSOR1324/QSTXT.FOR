CCPCITS
      interface to Subroutine WRCHR[C](lpage,icnum,iback,ifore,
     *                                 irpet)
      integer*2 lpage,icnum,iback,ifore,irpet
      end
      Subroutine QSTXT(icnum,iback,ifore,irpet)
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - QSTXT Version 01.1                          ---
CC--- Subroutine description - sets text: places repeat characters  ---
CC---                          with character number icnum on       ---
CC---                          screen beginning at current cursor   ---
CC---                          position.  iback and ifore are the   ---
CC---                          background and foreground colors.    ---
CC---------------------------------------------------------------------
CC
CC    icnum - ascii character number.
CC    iback - background color (0 - 7).
CC    ifore - foreground color (0 - 7).
CC    irpet - number of repeat characters.
CC
CC**********************************************************************
CC
CC
      integer*2 lpage,icnum,iback,ifore,irpet
CC
      include 'OCNTRL.INC'
      include 'ONAMES.INC'
CC
CC**********************************************************************
CC
      lpage = ispage
      call WRCHR(lpage,icnum,iback,ifore,irpet)
      mscrnm(irdcol,irdrow) = CHAR(icnum)
      return
      end
CCPCITE
