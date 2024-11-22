      Subroutine IGTDT(idate,ifclc,inhrs,indys,inmos,inyrs,iflg)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IGTDT Version 01.2                          --- 110198
CC--- Subroutine description - retrieves date/time from compact form---
CC---------------------------------------------------------------------
CC
CC    idate - variable containing compact date/time.
CC    ifclc - returned flag indicating whether or not calculations for
CC            this date were performed or not.
CC            0 - not
CC            1 - yes
CC    inhrs - returned hour (0000-2400)
CC    indys - returned day (1-31)
CC    inmos - returned month (1-12)
CC    inyrs - returned year (## or ####)
CC    iflg  - return year as 2 or 4 digit value.
CC            0 - 2 digit
CC            1 - 4 digit
CC
CC*********************************************************************
CC
      i = IABS(idate)                                                   110198
      n = 10
      do 10 l=1,4
        n = n - 2
        j = 10**n
        k = i / j
        i = i - k * j
        if (l .eq. 1) then
          ifclc = k
          if (idate .lt. 0) ifclc = -ifclc                              110198
        else if (l .eq. 2) then
          inhrs = k * 100
        else if (l .eq. 3) then
          indys = k
        else if (l .eq. 4) then
          inmos = k
        end if
   10 continue
      inyrs = i
      if (iflg .ne. 0) then
        if (inyrs .lt. 50) then                                         020797
          inyrs = inyrs + 2000
        else if (inyrs .lt. 100) then
          inyrs = inyrs + 1900
        end if
      end if
      return
      end
