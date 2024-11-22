CCPCITS
      interface to Subroutine MPVGA[C](iprnx,icolr)
      integer*2 iprnx,icolr
      end
      interface to Subroutine GTRGC[C](jprnx,jcolr)
      integer*2 jprnx,jcolr[reference]
      end
      Subroutine QPREG(iprnx,icolr,imode)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - QPREG Version 01.1                          ---
CC--- Subroutine description - change the color table by modifying  ---
CC---                          the pallette registers.  EGA & VGA   ---
CC---------------------------------------------------------------------
CC
CC    iprnx - pallette register index (0-15)
CC    icolr - if imode = 0, then the color icolr (0-63) is placed in
CC            pallette register iprnx.
CC            if imode = 1, then the color (0-63) in the pallette
CC            register given by icolr is moved to the register given
CC            by iprnx.
CC
CC**********************************************************************
CC
      integer*2 iprnx,icolr,imode,jprnx,jcolr
CC
CC**********************************************************************
CC
      if (imode .eq. 1) then
        jprnx = icolr
        call GTRGC(jprnx,jcolr)
        icolr = jcolr
      end if
      call MPVGA(iprnx,icolr)
      return
      end
CCPCITE
