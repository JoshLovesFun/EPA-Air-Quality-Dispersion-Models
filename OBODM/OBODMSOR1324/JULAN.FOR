      Subroutine JULAN(iyr,imo,idy,jdy)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - JULAN Version 01.0                          ---
CC--- Subroutine description - calculate the julian day from the    ---
CC---                          year, month and day or calculate the ---
CC---                          month and day from the julian day.   ---
CC---------------------------------------------------------------------
CC
CC        if the julian day jdy is zero, the julian day is calculated
CC        and returned. if the julian day is greater than zero, the
CC        month and day are calculated and returned.
CC
CC        iyr    - current year
CC        imo    - current month
CC        idy    - current day
CC        jdy    - current julian day
CC
CC**********************************************************************
CC
      integer*2 j,k,l,n,jul2
CC
      dimension jul2(13,2)
CC
      data jul2/0,31,60,91,121,152,182,213,244,274,305,335,366,0,31,59,
     *          90,120,151,181,212,243,273,304,334,365/
CC
CC**********************************************************************
CC
      l = 2
      n = iyr - 4 * (iyr / 4)
      if (n .le. 0) l = 1
      if (jdy .le. 0) then
        k = MIN0(MAX0(imo,1),12)
        jdy = jul2(k,l) + idy
      else
        imo = 1
        idy = 1
        k = 1
        j = 0
   10   if (k .lt. 13.and.j .eq. 0) then
          k = k + 1
          if (jdy .le. jul2(k,l)) then
            j = k
          end if
          go to 10
        end if
        if (j .gt. 1) then
          idy = jdy - jul2(j-1,l)
          imo = j - 1
        end if
      end if
      return
      end
