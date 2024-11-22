      Subroutine OBEND(imode)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - OBEND Version 01.1                          ---
CC--- Subroutine description - end text/graphics mode and set mode  ---
CC---                          equal to previous session mode.      ---
CC---------------------------------------------------------------------
CC
CC    imode - mode to return system to.
CC
CC*********************************************************************
CC
CCOMP1S-HP2645A
c     character*1 mescp
CCOMP1E-HP2645A
CCPCITS
      integer*2 jmode
CCPCITE
CC
CC*********************************************************************
CC
CCOMP1S-HP2645A
c     mescp = CHAR(27)
CC    WRITE (*,9000) mescp,'E'
c     WRITE (*,9000) mescp,'H',mescp,'J'
c9000 format ($,80a1)
CCOMP1E-HP2645A
CCPCITS
      jmode = imode
      if (jmode .gt. 3) jmode = 3
      call QSMOD(jmode)
CCPCITE
      return
      end
