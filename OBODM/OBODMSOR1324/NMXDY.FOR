      Function NMXDY(iyear,imont)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - NMXDY Version 01.1                          ---
CC--- Subroutine description - determine maximum number of days in  ---
CC---                          given month.                         ---
CC---------------------------------------------------------------------
CC
CC        iyear - year.
CC        imont - month (01 - 12).
CC
CC        returns - NMXDY - number of days in month.
CC
CC**********************************************************************
CC
      if (imont .eq. 4.or.imont .eq. 6.or.imont .eq. 9.or.imont .eq. 11)
     *    then
        NMXDY = 30
      else if (imont .ne. 2) then
        NMXDY = 31
      else
CC      if (MOD(iyear,4) .ne. 0) then                                   010400
        if (LEAPS(iyear) .eq. 0) then                                   010400
          NMXDY = 28
        else
          NMXDY = 29
        end if
      end if
      return
      end
