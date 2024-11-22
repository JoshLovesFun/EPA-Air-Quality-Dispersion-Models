      Function IPSQL(radnx,wndsp)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IPSQL Version 01.0                          ---
CC--- Subroutine description - determine Pasquill stability category---
CC---                          from the net radiation index and     ---
CC---                          wind speed.                          ---
CC---------------------------------------------------------------------
CC
CC        radnx - net radiation index (4,3,2,1,0,-1,-2).
CC        wndsp - wind speed (m/s).
CC
CC        returns IPSQL - Pasquill stability category (1=a,2=b,3=c,4=d,
CC                        5=e,6=f).
CC
CC*********************************************************************
CC
      integer*2 i,j,inxis,ispct,iskts
CC
      dimension inxis(9,7),ispct(8)
CC
      data inxis/1,1,1,2,2,2,3,3,3,
     *           1,2,2,2,2,3,3,3,4,
     *           2,2,3,3,3,3,4,4,4,
     *           3,3,4,4,4,4,4,4,4,
     *           4,4,4,4,4,4,4,4,4,
     *           6,6,5,5,4,4,4,4,4,
     *           6,6,6,6,5,5,5,4,4/
     *     ispct/1,3,5,6,7,9,10,11/
CC
CC*********************************************************************
CC
      netrd = radnx
      iskts = wndsp * 1.943844 + 0.5
      do 10 i=1,8
        if (iskts .le. ispct(i)) go to 20
   10 continue
      i = 9
   20 j = 5 - MAX0(MIN0(netrd,4),-2)
      IPSQL = inxis(i,j)
      return
      end
