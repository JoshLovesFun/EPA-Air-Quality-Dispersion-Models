      Function IFNUL(maray,nchrs)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IFNUL Version 01.0                          ---
CC--- Subroutine description - convert any null characters (oct 0)  ---
CC---                          to blank.                            ---
CC---------------------------------------------------------------------
CC
CC        maray  - character*1 array.
CC        nchrs  - number of characters in mchar.
CC
CC        returns - IFNUL
CC                   0 - no null characters found.
CC                  >0 - number of null characters converted to blank.
CC
CC********************************************************************
CC
      integer*2 i
CC
      character*1 maray,mnull,mblnk
CC
      dimension maray(500)
CC
      data mblnk/' '/
CC
CC********************************************************************
CC
      IFNUL = 0
      mnull = CHAR(0)
      do 10 i=1,nchrs
        if (maray(i) .eq. mnull) then
          maray(i) = mblnk
          IFNUL = IFNUL + 1
        end if
   10 continue
      return
      end
