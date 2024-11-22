      Subroutine LOCAL(iyr,imo,idy,ihr,ihs)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - LOCAL Version 01.0                          ---
CC--- Subroutine description - add/subtract hours to current date   ---
CC---                          and time.                            ---
CC---------------------------------------------------------------------
CC
CC    iyr   - year.
CC    imo   - month.
CC    idy   - day.
CC    ihr   - hour.
CC    ihs   - number of hours to add to iyr,imo,idy,ihr.
CC
CC**********************************************************************
CC
      integer*2 iflg
CC
      include 'OCDSPM.INC'
CC
CC**********************************************************************
CC
      ihr = ihr + ihs
      if (ihr .lt. vllimt(4)) then
        idy = idy - 1
        ihr = vulimt(4)
        if (idy .le. 0) then
          imo = imo - 1
          if (imo .le. 0) then
            iyr = iyr - 1
            imo = 12
            idy = 31
          else
            if (imo .eq. 2) then
              idy = 28
CC            if (MOD(iyr,4) .eq. 0) idy = 29                           010400
              if (LEAPS(iyr) .ne. 0) idy = 29                           010400
            else if (imo.eq.4.or.imo.eq.6.or.imo.eq.9.or.imo.eq.11) then
              idy = 30
            else
              idy = 31
            end if
          end if
        end if
      else if (ihr .gt. vulimt(4)) then
        ihr = vllimt(4)
        idy = idy + 1
        iflg = 0
        if (imo .eq. 2) then
CC        if (MOD(iyr,4) .eq. 0) then                                   010400
          if (LEAPS(iyr) .ne. 0) then                                   010400
            if (idy .gt. 29) iflg = 1
          else
            if (idy .gt. 28) iflg = 1
          end if
        else if (imo.eq.4.or.imo.eq.6.or.imo.eq.9.or.imo.eq.11) then
          if(idy .gt. 30) iflg = 1
        else if (idy .gt. 31) then
          iflg = 1
        end if
        if (iflg .ne. 0) then
          idy = 1
          imo = imo + 1
          if (imo .gt. 12) then
            imo = 1
            iyr = iyr + 1
          end if
        end if
      end if
      return
CC
      end
