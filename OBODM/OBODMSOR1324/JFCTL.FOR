CCPCITS
      Function JFCTL(iext,ikey,numsc)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - JFCTL Version 01.1                          ---
CC--- Subroutine description - determine if a keyboard entry is a   ---
CC---                          control character or function.       ---
CC---------------------------------------------------------------------
CC
CC    iext - index indicating whether typed key is normal (1) or from
CC           the extended code (0).
CC    ikey - ascii value of typed key (0-127, 258 or 260).
CC    numsc - screen menu number.
CC
CC    routine looks for the following control code combinations and
CC    returns the value under JFCTL.
CC
CC            iext  ikey  key    JFCTL
CC            ----  ---- -----   -----
CC              1    13  enter     0
CC              0    59    f1      1
CC              0    60    f2      2
CC              0    61    f3      3
CC              0    62    f4      4
CC              0    63    f5      5
CC              0    64    f6      6
CC              0    65    f7      7
CC              0    66    f8      8
CC              0    67    f9      9
CC              0    68    f10    10
CC              0    73   pgup    11
CC              0    81   pgdn    12
CC              0   258 2 lf clk  13 (mouse double left click)
CC              0   260 2 rt clk  13 (mouse double right click)
CC              1    27    esc     1, 2 or 3 depending on numsc
CC            none of the above   -1
CC
CC*********************************************************************
CC
      integer*2 iext,ikey,i
CC
      include 'OCNTRL.INC'
CC
CC*********************************************************************
CC
      JFCTL = -1
      if (iext .eq. 0.and.(ikey .eq. 258.or.ikey .eq. 260)) then
        JFCTL = 13
      else
        do 10 i=1,13
          if (iext .eq. nkeybd(1,i)) then
            if (ikey .eq. nkeybd(2,i)) then
              JFCTL = i - 1
            end if
          end if
   10   continue
        if (JFCTL .lt. 0.and.iext .eq. nkeybd(1,26).and.ikey .eq.
     *      nkeybd(2,26)) then
          if (numsc .ge. 1.and.numsc .le. 3) then
            JFCTL = 1
          else if ((numsc .ge. 3.and.numsc .le. 35).or.
     *             (numsc .ge. 55.and.numsc .le. 100)) then
            JFCTL = 2
          else if (numsc .ge. 36.and.numsc .le. 54) then
            JFCTL = 3
          end if
        end if
      end if
      return
      end
CCPCITE
