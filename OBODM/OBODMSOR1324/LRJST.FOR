      Function LRJST(maray,nchrs,iflrj)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - LRJST Version 01.1                          ---
CC--- Subroutine description - justify a character string. blanks   ---
CC---                          are padded on the left or right.     ---
CC---------------------------------------------------------------------
CC
CC        maray - character*1 array to justify.
CC        nchrs - number of characters in array maray.
CC        iflrj - justification direction.
CC                0 - right justify.
CC                1 - left justify.
CC
CC        returns - index of last non-blank character for left justify
CC                  index of first non-blank character for right justify
CC                  zero if all blanks.
CC
CC**********************************************************************
CC
      character*1 maray,mblnk
CC
      dimension maray(500)
CC
      data mblnk/' '/
CC
CC**********************************************************************
CC
      n = NUMC1(maray,nchrs)
      LRJST = n
      if (n .gt. 0) then
        if (iflrj .eq. 0) then
CC          right justified.          
          if (n .lt. nchrs) then
CC            shift characters right.            
            do 10 i=1,n
              maray(nchrs+1-i) = maray(n+1-i)
   10       continue
            call MBLK1(maray,nchrs-n)
          end if
          i = 0
CC          find first non-blank character.          
          LRJST = 0
   20     if (i .lt. nchrs.and.LRJST .eq. 0) then          
            i = i + 1
            if (maray(i) .ne. mblnk) LRJST = i
            go to 20
          end if
        else
CC          left justified.          
          n = NUMLB(maray,nchrs)
          if (n .gt. 0.and.n .lt. nchrs) then
            do 30 i=n+1,nchrs
              maray(i-n) = maray(i)
   30       continue
            call MBLK1(maray(nchrs-n+1),n)
          end if
          LRJST = NUMC1(maray,nchrs)
        end if
      end if
      return
      end
