CCPCITS
      interface to Subroutine SETPG[C](lpage)
      integer*2 lpage
      end
      Subroutine DISPG(ipage)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - DISPG Version 01.1                          ---
CC--- Subroutine description - set display page.                    ---
CC---------------------------------------------------------------------
CC
CC    ipage - page number.
CC
CC**********************************************************************
CC
      integer*2 lpage
CC
CC**********************************************************************
CC
      lpage = ipage
      call SETPG(lpage)
      return
      end
CCPCITE
