CCPCITS
      interface to integer*2 Function ISMOS[C](ibuttn)
      integer*2 ibuttn[REFERENCE]
      end
      interface to Subroutine SETML[C](ix,iy)
      integer*2 ix, iy
      end
      interface to Subroutine SHOWM[C]
      end
      Subroutine SETMS
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - SETMS Version 01.1                          ---
CC--- Subroutine description - turn mouse on and leave in upper left---
CC---                          corner.                              ---
CC---------------------------------------------------------------------
CC
CC*********************************************************************
CC
      integer*2 ISMOS
CC
      include 'OCNTRL.INC'
CC
CC*********************************************************************
CC
      ifmous = IABS(ISMOS(numbtn))
      if (ifmous .ne. 0) then
        locxms = 0
        locyms = 0
        call SETML(locxms,locyms)
        call SHOWM
      end if
      return
      end
CCPCITE
