      Function IFBKQ(joptn,ibknd,ipmod)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IFBKQ Version 01.0                          ---
CC--- Subroutine description - make sure color joptn is not equal to---
CC---                          the background color ibknd.          ---
CC---------------------------------------------------------------------
CC
CC        joptn  - color option to check.
CC        ibknd  - background color index.
CC        ipmod  - plot mode index.
CC
CC**********************************************************************
CC
      if (joptn .eq. ibknd) joptn = ibknd + 1
      if (ipmod .lt. 16) then
        if (joptn .gt. 3) joptn = 1
      else
        if (joptn .gt. 15) joptn = 1
      end if
      IFBKQ = joptn
      return
      end
