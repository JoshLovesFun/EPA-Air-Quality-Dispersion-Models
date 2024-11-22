CCPCITS
      interface to Subroutine RDCHR[C](lpage,icnum,iback,ifore)
      integer*2 lpage,icnum[REFERENCE],iback[REFERENCE],
     *          ifore[REFERENCE]
      end
      Subroutine QRTXT(icnum,iback,ifore)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - QRTXT Version 01.1                          ---
CC--- Subroutine description - reads text character at the current  ---
CC---                          cursor position.                     ---
CC---------------------------------------------------------------------
CC
CC    icnum - returned ascii character number.
CC    iback - returned background color register number (0 - 15).
CC    ifore - returned foreground color register number (0 - 15).
CC
CC**********************************************************************
CC
      integer*2 lpage,icnum,iback,ifore
CC>   integer*2 imode
CC
      include 'OCNTRL.INC'
CC
CC**********************************************************************
CC
      lpage = ispage
CC>     turn cursor off.
CC>   imode = 3
CC>   call CRTYP(imode)
      call RDCHR(lpage,icnum,iback,ifore)
CC>     turn cursor on.
CC>   imode = 1
cc>   call CRTYP(imode)
      return
      end
CCPCITE
