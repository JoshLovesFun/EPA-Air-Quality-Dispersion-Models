CCPCITS
      interface to Subroutine CLRSC[C](iback,ifore,icolmx,irowmx)
      integer*2 iback,ifore,icolmx,irowmx
      end
      Subroutine QSCLR(iback)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - QSCLR Version 01.1                          ---
CC--- Subroutine description - clear screen and set the background  ---
CC---                          and foreground colors.               ---
CC---------------------------------------------------------------------
CC
CC    iback - back ground color number.
CC
CC**********************************************************************
CC
      integer*2 iback,ifore
CC
      include 'OCNTRL.INC'
CC
CC**********************************************************************
CC
      ifore = iback
      call CLRSC(iback,ifore,icolmx,irowmx)
      return
      end
CCPCITE
