      Function IFSPQ(xp,yp,x1,y1)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IFSPQ Version 01.1                          ---
CC--- Subroutine description - scan plot area to see where to locate---
CC---                          a label.                             ---
CC---------------------------------------------------------------------
CC
CC        xp     - returned x start position.
CC        yp     - returned y start position.
CC        x1     - x length of label (inches)
CC        y1     - y length of label (inches)
CC
CC        returns - IFSPQ
CC                 0 - no location is available, cannot use xp,yp.
CC                 1 - location found is good, use xp,yp.
CC
CC*********************************************************************
CC
      include 'OCNTRL.INC'
      include 'OWNTRL.INC'
CC
CC*********************************************************************
CC
      IFSPQ = 0
CC      divide plot into hundredths.
      xinc = xnchpl / 100.0
      yinc = ynchpl / 100.0
      yp = 0.02 - yinc
CC      loop over y direction.
   10 yp = yp + yinc
      if (yp .lt. ynchpl-y1) then
        xp = 0.02 - xinc
CC        loop over x direction.
   20   xp = xp + xinc
        if (xp .gt. xnchpl-x1) go to 10
        if (IFANQ(xp,yp,x1,y1) .ne. 0) then
          IFSPQ = 1
        else
          go to 20
        end if
      end if
      return
      end
