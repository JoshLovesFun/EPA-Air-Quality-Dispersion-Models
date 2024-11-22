      Function NBXPQ(xp,yp)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - NBXPQ Version 01.0                          ---
CC--- Subroutine description - determine if a point is inside of any---
CC---                          of the special labels or legends     ---
CC---                          already drawn.                       ---
CC---------------------------------------------------------------------
CC
CC         returns - NBXPQ -
CC                   0 = outside
CC                  >0 = k, inside box k (xbxpsl(k),xbxpel(k),ybxpsl(k),
CC                       ybxpel(k))
CC*********************************************************************
CC
      integer*2 k
CC
      include 'OCNTRL.INC'
      include 'OWNTRL.INC'
CC
CC*********************************************************************
CC
      NBXPQ = 0
      if (nbxpss .gt. 0) then
        do 10 k=1,nbxpss
          if (IFBXQ(xp,yp,xbxpsl(k),xbxpel(k),ybxpsl(k),ybxpel(k))
     *        .ne. 0) NBXPQ = k
   10   continue
      end if
      return
      end
