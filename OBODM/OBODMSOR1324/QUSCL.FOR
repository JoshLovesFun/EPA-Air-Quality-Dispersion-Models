CCPCITS
      interface to Subroutine SCRUP[C](nline,ihrow,lwrow,lfcol,
     *                                 ircol,iback,ifore)
      integer*2 nline,ihrow,lwrow,lfcol,ircol,iback,ifore
      end
      interface to Subroutine SCRDN[C](nline,ihrow,lwrow,lfcol,
     *                                 ircol,iback,ifore)
      integer*2 nline,ihrow,lwrow,lfcol,ircol,iback,ifore
      end
      Subroutine QUSCL(nline,nblnk,lfcol,lwrow,iwdth,iback,ifore)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - QUSCL Version 01.1                          ---
CC--- Subroutine description - scrolls up a specified window on crt ---
CC---                          screen, leaving a specified number of---
CC---                          lines blank at bottom of window.     ---
CC---------------------------------------------------------------------
CC
CC    nline - number of lines in the scroll window.
CC    nblnk - number of lines to blank, starting at bottom of scroll
CC            window.  if nblnk = 0, then entire window is blanked.
CC    lfcol - text column at left hand side of scrolled window.
CC    lwrow - text row at bottom of scrolled window.
CC    iwdth - width in columns of scrolled window.
CC    iback - background color in blanked region (0 - 15 (8 - 15 give
CC            blinking foreground))
CC    ifore - foreground color in blanked region.
CC
CC**********************************************************************
CC
      integer*2 nline,lwrow,lfcol,ircol,iback,ifore,nblnk,iwdth,
     *          lower,iuper
CC
      include 'OCNTRL.INC'
CC
CC**********************************************************************
CC
      ircol = lfcol + iwdth
      lower = irowmx - 1 - lwrow
      iuper = lower - nline
      nline = nblnk
      call SCRUP(nline,iuper,lower,lfcol,ircol,iback,ifore)
      return
      end
CCPCITE
