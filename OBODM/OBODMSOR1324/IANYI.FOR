      Function IANYI(iaray,num,ivalu)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IANYI Version 01.0                          ---
CC--- Subroutine description - determine if the absolute value of   ---
CC---                          any element of iaray equals ivalu.   ---
CC---------------------------------------------------------------------
CC
CC        0 = no
CC        1 = yes, iaray value is positive
CC       -1 = yes, iaray value is negative
CC
CC**********************************************************************
CC
      integer*2 i
CC
      dimension iaray(200)
CC
CC**********************************************************************
CC
      IANYI = 0
      i = 0
   10 if (i .lt. num.and.IANYI .eq. 0) then
        i = i + 1
        if (IABS(iaray(i)) .eq. ivalu) then
          if (iaray(i) .lt. 0) then
            IANYI = -1
          else
            IANYI = 1
          end if
        end if
        go to 10
      end if
      return
      end
