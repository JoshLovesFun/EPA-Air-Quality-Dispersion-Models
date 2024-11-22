      Function ISVDT(ifclc,inhrs,indys,inmos,inyrs)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - ISVDT Version 01.2                          --- 110198
CC--- Subroutine description - save date/time in compact form.      ---
CC---------------------------------------------------------------------
CC
CC    ifclc - flag indicating whether or not calculations for this date
CC            were performed or not.
CC           -1 - not do to source not operating.
CC            0 - not do to missing data or calm wind.
CC            1 - yes
CC    inhrs - hour (0000-2400)
CC    indys - day (1-31)
CC    inmos - month (1-12)
CC    inyrs - year (##)
CC
CC    ISVDT - returned value of above values compressed into one
CC            integer*4 word.
CC
CC*********************************************************************
CC
      i = inhrs
      if (i .ge. 100) i = i / 100
      j = inyrs
      if (j .ge. 2000) j = j - 2000
      if (j .ge. 1900) j = j - 1900
      ISVDT = j + 100 * (inmos + 100 * (indys + 100 * (i +
     *        100 * IABS(ifclc))))                                      110198
      if (ifclc .lt. 0) ISVDT = -ISVDT                                  110198
      return
      end
