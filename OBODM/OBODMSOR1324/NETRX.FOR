      Function NETRX(ipasq,wndsp)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - NETRX Version 01.0                          ---
CC--- Subroutine description - approximate net radiation index from ---
CC---                          Pasquill stability category and      ---
CC---                          wind speed. for more accurate calc-  ---
CC---                          ulation use function IRNDX.f or      ---
CC---                          netri.f                              ---
CC---------------------------------------------------------------------
CC
CC        ipasq - Pasquill stability category (1=a,2=b,3=c,4=d,5=e,6=f).
CC        wndsp - wind speed (m/s).
CC
CC        returns NETRX - net radiation index (4,3,2,1,0,-1,-2)
CC
CC*********************************************************************
CC
      integer*2 i,j,inxis,ispct,iskts
CC
      dimension inxis(9,6),ispct(8)
CC
      data inxis/ 4, 4, 4, 4, 4, 4, 4, 4, 4,
     *            2, 3, 3, 3, 3, 4, 4, 4, 4,
     *            1, 1, 2, 2, 2, 3, 4, 4, 4,
     *            0, 0, 1, 1, 0, 0, 0, 0, 0,
     *           -2,-2,-1,-1, 0, 0, 0, 0, 0,
     *           -2,-2,-2,-2,-1,-1,-1, 0, 0/,
     *     ispct/1,3,5,6,7,9,10,11/
CC
CC*********************************************************************
CC
      iskts = wndsp * 1.943844 + 0.5
      do 10 i=1,8
        if (iskts .le. ispct(i)) go to 20
   10 continue
      i = 9
   20 j = MAX0(MIN0(ipasq,6),1)
      NETRX = inxis(i,j)
      return
      end
