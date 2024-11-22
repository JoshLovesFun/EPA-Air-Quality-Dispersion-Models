      Function IFNBR(maray,nchrs)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IFNBR Version 01.0                          ---
CC--- Subroutine description - determine if a string of characters  ---
CC---                          is numeric integer.                  ---
CC---------------------------------------------------------------------
CC
CC        maray - string of character*1 characters.
CC        nchrs - number of characters to search.
CC
CC        returns - IFNBR
CC                  0 - not numeric.
CC                  1 - numeric.
CC                  2 - all blanks.
CC
CC**********************************************************************
CC
      integer*2 i
CC
      character*1 maray,mzero,mnine,mblnk,minus,mplus
CC
      dimension maray(500)
CC
      data mzero/'0'/,mnine/'9'/,mblnk/' '/,minus/'-'/,mplus/'+'/
CC
CC**********************************************************************
CC
      IFNBR = 2
      do 10 i=1,nchrs
        if (maray(i) .ne. mblnk) then
          IFNBR = 1
          if (maray(i) .ne. minus.and.maray(i) .ne. mplus) then
            if (maray(i) .lt. mzero.or.maray(i) .gt. mnine) then
              IFNBR = 0
              return
            end if
          end if
        end if
   10 continue
      return
      end
