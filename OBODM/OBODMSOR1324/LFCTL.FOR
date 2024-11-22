CCPCITS
      Function LFCTL(iext,ikey)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - LFCTL version 01.1                          ---
CC--- Subroutine description - determine if a keyboard entry is a   ---
CC---                          valid 'Alt' character.               ---
CC---------------------------------------------------------------------
CC
CC    iext - index indicating whether typed key is normal (1) of from
CC           the extended code (0).
CC    ikey - ASCII value of typed key
CC
CC    routine looks for the following control code combinations and
CC    returns the array location under LFCTL.
CC
CC            iext  ikey  key     map #
CC            ----  ---- ------   -----
CC              0    50  Alt-m     1
CC              0    35  Alt-h     2
CC              0    16  Alt-q     3
CC              0    49  Alt-n     4
CC              0    48  Alt-b     5
CC              0    18  Alt-e     6
CC              0    19  Alt-r     7
CC            none of the above   -1
CC
CC*********************************************************************
CC
      integer*2 iext,ikey
CC
      include 'OCNTRL.INC'
CC
CC*********************************************************************
CC
      LFCTL = -1
      if (iext .eq. 0) then
        do 10 i=1,icmnum
          if (ikey .eq. icmkey(i)) then
            LFCTL = i
            return
          end if
   10   continue
      end if
      return
      end
CCPCITE
