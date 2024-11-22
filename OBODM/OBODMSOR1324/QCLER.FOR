CCPCITS
      interface to Subroutine CLRSC[C](iback,ifore,icolmx,irowmx)
      integer*2 iback,ifore,icolmx,irowmx
      end
      Subroutine QCLER(iback,ifore)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - QCLER Version 01.1                          ---
CC--- Subroutine description - clears screen in text mode.  sets    ---
CC---                          background and foreground colors.    ---
CC---------------------------------------------------------------------
CC
CC    iback  - background color number (0 - 15)
CC    ifore  - foreground color number (0 - 15)
CC
CC         color value    color          color value    color
CC              0         black               8         gray
CC              1         blue                9         light blue
CC              2         green              10         light green
CC              3         cyan               11         light cyan
CC              4         red                12         light red
CC              5         magenta            13         light magenta
CC              6         yellow             14         light yellow
CC              7         dark white         15         light white
CC
CC**********************************************************************
CC
      integer*2 iback,ifore
CC
      include 'OCNTRL.INC'
CC
CC**********************************************************************
CC
      call CLRSC(iback,ifore,icolmx,irowmx)
      return
      end
CCPCITE
