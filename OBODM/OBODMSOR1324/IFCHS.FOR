      Function IFCHS(mbufr,istc,ienc,nums,nume,num)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IFCHS Version 01.1                          ---
CC--- Subroutine description - replace characters in buffer with    ---
CC---                          another, starting at the end of the  ---
CC---                          string.                              ---
CC---------------------------------------------------------------------
CC
CC     mbufr - buffer containing letter to be blanked out.
CC     istc  - start character in buffer.
CC     ienc  - end character in buffer.
CC     nums  - alphabetic index of letter to search for.
CC     nume  - alphabetic index of replacement character.
CC             if zero, blank is used.
CC     num   - number of characters to replace.
CC
CC     returns - 0 - if no letters changed.
CC              >0 - number of letters changed.
CC
CC**********************************************************************
CC
      character*1 mbufr,mblnk
CC
      dimension mbufr(1000)
CC
      data mblnk/' '/
CC
CC**********************************************************************
CC
      i = ienc + 1
      IFCHS = 0
   10 if (i .gt. istc.and.IFCHS .lt. num) then
        i = i - 1
        if (IFCHR(mbufr(i),nums) .ne. 0) then
          if (nume .le. 0) then
            mbufr(i) = mblnk
          else
            j = IFCHR(mbufr(i),-nume)
          end if
          IFCHS = IFCHS + 1
        end if
        go to 10
      end if
      return
      end
