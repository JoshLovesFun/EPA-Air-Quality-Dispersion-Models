      Function PRSVP(temp,hum)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - PRSVP Version 01.0                          ---
CC--- Subroutine description - calculate vapor pressure.            ---
CC---------------------------------------------------------------------
CC
CC        temp - temperature (deg c)
CC        hum  - humidity (fraction)
CC
CC*********************************************************************
CC
      PRSVP = hum * (575.0466 + temp * (31.82291 + 1.296028 * temp)) /
     *        (93.51611 - temp)
      return
      end
