      Function JANYI(iaray,num,ivalu)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - JANYI Version 01.0                          ---
CC--- Subroutine description - determine if a specific value of the ---
CC---                          array iaray equals ivalu.            ---
CC---------------------------------------------------------------------
CC
CC        0 = no
CC       >0 = yes, the index of the value in iaray is returned in
CC                 JANYI.
CC
CC**********************************************************************
CC
      integer*2 i
CC
      dimension iaray(200)
CC
CC**********************************************************************
CC
      JANYI = 0
      i = 0
   10 if (JANYI .eq. 0.and.i .lt. num) then
        i = i + 1
        if (iaray(i) .eq. ivalu) then
          JANYI = i
        end if
        go to 10
      end if
      return
      end
