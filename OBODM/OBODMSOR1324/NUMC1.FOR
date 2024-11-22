      Function NUMC1(maray,nchrs)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - NUMC1 Version 01.0                          ---
CC--- Subroutine description - return the index of the last non-    ---
CC---                          blank character in a string.         ---
CC---------------------------------------------------------------------
CC
CC        maray - character array.
CC        nchrs - number of characters the array maray.
CC
CC        returns - NUMC1 -
CC                  0 = array all blank.
CC                 >0 = index number of last non-blank character.
CC
CC******************************************************************
CC
      integer*2 i
CC
      character*1 maray,mblnk
CC
      dimension maray(2000)
CC
      data mblnk/' '/
CC
CC*****************************************************************
CC
      NUMC1 = 0
      i = nchrs + 1
   10 if (i .gt. 1.and.NUMC1 .eq. 0) then
        i = i - 1
        if (maray(i) .ne. mblnk) NUMC1 = i
        go to 10
      end if
      return
      end
