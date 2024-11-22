CCPCITS
      interface to Subroutine MPCGA[C](ipalt,iback)
      integer*2 ipalt,iback
      end
      Subroutine QCOLR(iback,ipalt)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - QCOLR Version 01.1                          ---
CC--- Subroutine description - sets background color and palette    ---
CC---                          number in graphics mode.             ---
CC---------------------------------------------------------------------
CC
CC    iback - background color number (0 - 15) see QCLER for colors
CC    ipalt - palette number (0 - 1)
CC            for palette 1, the foreground colors are green, red
CC            and yellow-brown.  these correspond to foreground
CC            colors 1, 2, and 3, respectively.  for palette 0, the
CC            foreground colors are cyan, magenta, and white.  the
CC            corresponding foreground colors are again 1, 2 and 3.
CC
CC**********************************************************************
CC
      integer*2 iback,ipalt
CC
CC**********************************************************************
CC
      call MPCGA(ipalt,iback)
      return
      end
CCPCITE
