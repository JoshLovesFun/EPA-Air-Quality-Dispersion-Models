      Function LEAPS(iyr)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - LEAPS Version 01.0                          --- 010400
CC--- Subroutine description - Determine if year is actually leap   ---
CC---                          year                                 ---
CC---------------------------------------------------------------------
CC
CC    iyr   - year.
CC
CC    Returns - 0 if not a leap year
CC              1 if a leap year
CC
CC**********************************************************************
CC
CC**********************************************************************
CC
      LEAPS = 0
      jyr = IABS(iyr)
      if (MOD(jyr,4) .eq. 0) then
        j = jyr / 100
        if (jyr - j * 100 .ne. 0) then
          LEAPS = 1
        else if (MOD(jyr,400) .eq. 0) then
          LEAPS = 1
        end if
      end if
      return
      end
