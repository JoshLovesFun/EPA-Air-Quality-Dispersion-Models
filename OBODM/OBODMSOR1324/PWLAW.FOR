      Function PWLAW(refvl,cmplv,reflv,pwrlw)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - PWLAW Version 01.0                          ---
CC--- Subroutine description - compute the power law function.      ---
CC---------------------------------------------------------------------
CC
CC*********************************************************************
CC
      if (cmplv .le. 0.0) then
        PWLAW = 0.0
      else
        PWLAW = refvl * (cmplv / reflv)**pwrlw
      end if
      return
      end
