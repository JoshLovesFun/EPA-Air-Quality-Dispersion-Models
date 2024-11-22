      Function IDATR(iyr,jyr,idim)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IDATR Version 01.0                          ---
CC--- Subroutine description - compare date/time array iyr to jyr.  ---
CC---                          and time.                            ---
CC---------------------------------------------------------------------
CC
CC    iyr   - array of date/time - year,month,day,hour,minute.
CC    jyr   - array of date/time - year,month,day,hour,minute.
CC    idim  - number of parameters in iyr and jyr
CC
CC    returns - IDATR -
CC              <0 - iyr is less than jyr.
CC              =0 - iyr is equal to jyr.
CC              >0 - iyr is greater than jyr.
CC
CC*********************************************************************
CC
      integer*2 i
CC
      dimension iyr(5),jyr(5)
CC
CC*********************************************************************
CC
      IDATR = 0
      i = 0
   10 if (i .lt. idim.and.IDATR .eq. 0) then
        i = i + 1
        if (iyr(i) .lt. jyr(i)) then
          IDATR = -1
        else if (iyr(i) .gt. jyr(i)) then
          IDATR = 1
        end if
        go to 10
      end if
      return
      end
