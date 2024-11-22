CCPCITS
      Function KFCTL(iext,ikey)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - KFCTL Version 01.1                          ---
CC--- Subroutine description - determine if a keyboard entry is a   ---
CC---                          sub-control character or function.   ---
CC---------------------------------------------------------------------
CC
CC    iext - index indicating whether typed key is normal (1) or from
CC           the extended code (0).
CC    ikey - ascii value of typed key (0-127, 257 or 259).
CC
CC    routine looks for the following control code combinations and
CC    returns the value under KFCTL.
CC
CC            iext  ikey  key     KFCTL
CC            ----  ---- ------   -----
CC              0    72  up aro     1
CC              0    80  dn aro     2
CC              0    15  lf tab     3
CC              1     9  rt tab     4
CC              0    75  lf aro     5
CC              0    77  rt aro     6
CC              0    83  del        7
CC              0    82  ins        8
CC              1    11  ctrl k     9
CC              1     8  backsp    10
CC              0    71  home      11
CC              0    79  end       12
CC              0   119  ctrl home 13
CC              0   117  ctrl end  14
CC              0   257  1 lft clk 15 (mouse single left click)
CC              0   259  1 rt  clk 15 (mouse single right click)
CC            none of the above    -1
CC
CC*********************************************************************
CC
      integer*2 iext,ikey,i
CC
      include 'OCNTRL.INC'
CC
CC*********************************************************************
CC
      KFCTL = -1
      if (iext .eq. 0.and.(ikey .eq. 257.or.ikey .eq. 259)) then
        KFCTL = 15
      else
        do 10 i=1,12
          if (iext .eq. nkeybd(1,i+13)) then
            if (ikey .eq. nkeybd(2,i+13)) then
              KFCTL = i
            end if
          end if
   10   continue
        if (KFCTL .lt. 0) then
          if (iext .eq. nkeybd(1,27).and.ikey .eq. nkeybd(2,27)) then
            KFCTL = 13
          else if (iext .eq. nkeybd(1,28).and.ikey .eq. nkeybd(2,28))
     *             then
            KFCTL = 14
          end if
        end if
      end if
      return
      end
CCPCITE
