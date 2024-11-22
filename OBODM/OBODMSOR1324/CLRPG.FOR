CC-OBODM PC only related FORTRAN routines.
CCPCITS
      interface to Subroutine CURSP[C](lpage,ix,iy)
      integer*2 lpage,ix,iy
      end
      interface to Subroutine WRCHR[C](lpage,icnum,iback,ifore,
     *                                      irpet)
      integer*2 lpage,icnum,iback,ifore,irpet
      end
      Subroutine CLRPG(ipage,iback)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - CLRPG Version 01.1                          ---
CC--- Subroutine description - home cursor and blank out screen.    ---
CC---------------------------------------------------------------------
CC
CC    ipage - page number.
CC    iback - background color.
CC
CC**********************************************************************
CC
      integer*2 iback,lpage,icnum,irpet,ihome
CC
      include 'OCNTRL.INC'
CC
CC**********************************************************************
CC
      icnum = 32
      lpage = ipage
      irpet = icolmx * irowmx
      ihome = 0
      call CURSP(lpage,ihome,ihome)
      call WRCHR(lpage,icnum,iback,iback,irpet)
CC
      return
      end
CCPCITE
