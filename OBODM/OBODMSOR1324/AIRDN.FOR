CC-OBODM meteorological related routines
      Function AIRDN(airpr,airtc,airhm)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - AIRDN Version 01.1                          ---
CC--- Subroutine description - calculate air density (g/cm**3).     ---
CC---------------------------------------------------------------------
CC
CC        airpr - air pressure (mb).
CC        airtc - air temperature (c).
CC        airhm - air humidity (%).
CC
CC*********************************************************************
CC
CC      molecular wt. of air, and univ. gas. const (j/mol.k)
      data armol/28.9644/,r/8.31432/
CC
CC*********************************************************************
CC
      if (airtc .gt. -60.0) then
        t = airtc
      else
        t = 20.0
      end if
      if (airpr .gt. 0.0) then
        hum = airhm
        if (hum .le. 0.0) hum = 50.0
        vapin = PRSVP(t,0.01*hum)
        AIRDN = 1.0e-4 * armol * (airpr - 0.37803 * vapin) /
     *          (r * (t + 273.15))
      else
        AIRDN = 1.293e-3 / (1.0 + 0.00367 * t)
      end if
      return
      end
