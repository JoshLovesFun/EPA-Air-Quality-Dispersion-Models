      Function NUMLB(maray,nchrs)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - NUMLB Version 01.0                          ---
CC--- Subroutine description - return the number of leading blanks  ---
CC---                          in a character string.               ---
CC---------------------------------------------------------------------
CC
CC        maray - character string array.
CC        nchrs - number of characters in the array maray.
CC
CC        returns - NUMLB -
CC                  0 = no leading blanks.
CC                 >0 = number of leading blank characters.
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
      NUMLB = nchrs
      do 10 i=1,nchrs
        if (maray(i) .ne. mblnk) then
          NUMLB = i - 1
          go to 20
        end if
   10 continue
   20 return
      end
