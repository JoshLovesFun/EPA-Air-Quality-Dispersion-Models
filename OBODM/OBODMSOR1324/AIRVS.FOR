      Function AIRVS(airtp)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - AIRVS Version 01.0                          ---
CC--- Subroutine description - calculate air viscosity (poise or    ---
CC---                          g/cm.sec).                           ---
CC---------------------------------------------------------------------
CC
CC        airtp - air temperature (c).
CC
CC*********************************************************************
CC
      tkref = airtp + 273.15
      AIRVS = 1.4978693e-5 * tkref**1.5 / (tkref + 120.0)
      return
      end
