      Subroutine UVTWD(ucmp,vcmp,spd,dir,iflg)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - UVTWD Version 01.0                          ---
CC--- Subroutine description - converts u and v components into     ---
CC---                          wind speed (m/s) and direction (deg).---
CC---------------------------------------------------------------------
CC
CC        ucmp - u component of speed, direction (m/s).
CC        vcmp - v component of speed, direction (m/s).
CC        spd  - returned speed (m/s), depending on iflg.
CC        dir  - returned direction or azimuth (deg), depending on iflg.
CC        iflg - return direction towards or from.
CC               0 - direction towards (azimuth bearing) is returned.
CC               1 - direction from (wind direction) is returned.
CC
CC*********************************************************************
CC
      data radtod/57.29578/
CC
CC*********************************************************************
CC
      if (IFEQU(ucmp,0.0) .ne. 0.or.IFEQU(vcmp,0.0) .ne. 0) then
        b = ANTRU(ARTAN(ucmp,vcmp) * radtod)
        a = SQRT(ucmp * ucmp + vcmp * vcmp)
      else
        b = 0.0
        a = 0.0
      end if
      if (iflg .ne. 0) b = ANTRU(b + 180.0)
      spd = a
      dir = b
      return
      end
