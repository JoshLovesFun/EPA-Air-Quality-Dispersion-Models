      Function DRPAV(drpup,drplw,drpmx)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - DRPAV Version 01.0                          ---
CC--- Subroutine description - calculate the mass mean particle     ---
CC---                          diameter or radius.                  ---
CC---------------------------------------------------------------------
CC
CC        drpup  - upper diameter/radius of size category.
CC        drplw  - lower diameter/radius of size category.
CC        drpmx  - maximum possible diameter/radius.
CC
CC**********************************************************************
CC
      if (IFEQU(drpup-drplw,0.0) .ne. 0) then
        DRPAV = AMIN1((0.25 * (drplw + drpup) * (drplw * drplw +
     *          drpup * drpup))**0.3333333, drpmx)
      else
        DRPAV = AMIN1(drpup, drpmx)
      end if
      return
      end
