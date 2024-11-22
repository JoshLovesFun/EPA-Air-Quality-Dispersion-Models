      Function IDSTM(inyr,inmo,indy)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IDSTM Version 01.0                          ---
CC--- Subroutine description - determines if standard or daylight   ---
CC---                          savings time.                        ---
CC---------------------------------------------------------------------
CC
CC        inyr - year
CC        inmo - month
CC        indy - day
CC
CC     returns -
CC        IDSTM - standard or daylight time flag
CC                0 - standard time
CC               -1 - daylight savings time
CC
CC*********************************************************************
CC
      IDSTM = 0
      jyr = inyr
      jmo = inmo
      if (jyr .gt. 0.and.jmo .gt. 0.and.indy .gt. 0) then
        if (jmo .ge. 4.and.jmo .le. 10) then
          lflg = 0
          if (jmo .le. 4.or.jmo .ge. 10) then
            jdy = 367 * jyr - 7 * (jyr + (jmo + 9) / 12) / 4 - 3 *
     *            ((jyr + (jmo - 9) / 7) / 100 + 1) / 4 + 275 * jmo / 9
            i = 1
   10       if (i .le. 7.and.idy .lt. 7) then
              idy = jdy + 21 + i
              idy = MOD(idy + 2,7) + 1
              i = i + 1
              go to 10
            end if
            jdy = 21 + MIN0(i,7)
            idy = jdy
   20       if (jdy-7 .ge. 1) then
              jdy = jdy - 7
              go to 20
            end if
   30       if (idy+7 .le. 30) then
              idy = idy + 7
              go to 30
            end if
            if (jmo .eq. 4) then
              if (indy .le. jdy) lflg = 1
            else
              if (indy .gt. idy) lflg = 1
            end if
          end if
          if (lflg .eq. 0) IDSTM = -1
        end if
      end if
      return
      end
