      Function IFWER(iaray,n)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IFWER Version 01.0                          ---
CC--- Subroutine description - determines if error or warning flag  ---
CC---                          or none are set in an array          ---
CC---------------------------------------------------------------------
CC
CC    returns - IFWER
CC              0 - no errors or warnings.
CC              1 - error flag(s) found, warning flags are ignored.
CC              2 - warning flag(s) found, no errors.
CC
CC**********************************************************************
CC
      integer*2 i
CC
      dimension iaray(1000)
CC
CC**********************************************************************
CC
      IFWER = 0
      i = 0
   10 if (i .lt. n.and.IFWER .ne. 1) then
        i = i + 1
        if (iaray(i) .eq. 1) then
          IFWER = 1
        else if (iaray(i) .eq. 2) then
          IFWER = 2
        end if
        go to 10
      end if
      return
      end
