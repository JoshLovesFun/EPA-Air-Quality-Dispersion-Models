      Function JPACK(mlabl,nchrs)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - JPACK Version 01.0                          ---
CC--- Subroutine description - removes excess blanks from labels.   ---
CC---------------------------------------------------------------------
CC
CC        1) - removes all blanks prior to and including a < symbol and
CC             all leading blanks from the label.
CC        2) - removes all blanks after and including a > symbol.
CC        3) - the end of the label is padded with blanks.
CC
CC        mlabl - array containing the label to be packed. the packed
CC                 label is returned in mlabl.
CC        nchrs - number of characters to scan.
CC        JPACK - index of last non-blank character in packed array.
CC
CC**********************************************************************
CC
      integer*2 i,ig
CC
      character*1 mlabl,mless,mgrtr
CC
      dimension mlabl(400)
CC
      include 'OCNTRL.INC'
      include 'ONAMES.INC'
CC
      data mless/'<'/,mgrtr/'>'/
CC
CC**********************************************************************
CC
      JPACK = 0
      i = 0
      il = 0
      ig = 1
   10 if (i .lt. nchrs) then
        i = i + 1
        if (mlabl(i) .eq. mblnk1) then
          if (ig .le. 0) then
            il = il + 1
          end if
        else if (mlabl(i) .eq. mless) then
          if (ig .le. 0) then
            il = 0
          else
            ig = 0
            if (il .gt. 0) then
              call MBLK1(mlabl(JPACK+1),il)
              JPACK = JPACK + il
              il = 0
            end if
          end if
        else
          if (mlabl(i) .eq. mgrtr) then
            ig = 1
          else
            if (il .gt. 0) then
              call MBLK1(mlabl(JPACK+1),il)
              JPACK = JPACK + il
            end if
            JPACK = JPACK + 1
            mlabl(JPACK) = mlabl(i)
            il = 0
            ig = 0
          end if
        end if
        go to 10
      end if
      if (JPACK .lt. nchrs) call MBLK1(mlabl(JPACK+1),nchrs-JPACK)
      return
      end
