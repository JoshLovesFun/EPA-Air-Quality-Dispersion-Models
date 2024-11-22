      Function IFANQ(xp,yp,x1,y1)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IFANQ Version 01.0                          ---
CC--- Subroutine description - find if any part of a label overlays ---
CC---                          another box or label.
CC---------------------------------------------------------------------
CC
CC        xp     - x at lower left corner of label.
CC        yp     - y at lower left corner of label.
CC        x1     - x length of label (inches)
CC        y1     - y length of label (inches)
CC
CC        returns - IFSPQ
CC                 0 - no, overlays another label.
CC                 1 - xp,yp location is ok.
CC
CC*********************************************************************
CC
      IFANQ = 0
      dx = 0.02 * x1
      dy = 0.02 * y1
      yps = yp - dy
CC      using xp,yp as lower left corner of legend box loop over
CC      length and height to see if any part of the box is inside
CC      of previously plotted area.
CC
CC      increment y distance.
   10 yps = yps + dy
      If (yps .le. yp+y1) then
        xps = xp - dx
CC        increment x distance.
   20   xps = xps + dx
        if (xps .gt. xp+x1) go to 10
CC          check to see if point xps,yps is inside previously plotted
CC          area.
        if (NBXPQ(xps,yps) .ne. 0) go to 30
        go to 20
      end if
      IFANQ = 1
   30 return
      end
