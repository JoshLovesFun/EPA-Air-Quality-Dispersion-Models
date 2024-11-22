      Function LBLPQ(xp,yp,x1,y1,jflg)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - LBLPQ Version 01.0                          ---
CC--- Subroutine description - find where to locate a label.        ---
CC---------------------------------------------------------------------
CC
CC        xp     - returned x start position.
CC        yp     - returned y start position.
CC        x1     - x length of label (inches)
CC        y1     - y length of label (inches)
CC        jflg   - preferred location flag
CC                 0 - upper right inside of plot axes
CC                 1 - upper left inside of plot axes
CC                 2 - lower left inside of plot axes
CC                 3 - lower right inside of plot axes
CC                 4 - upper right margin
CC                 5 - upper left margin
CC                 6 - lower left margin
CC                 7 - lower right margin
CC
CC        returns - LBLPQ - where label is actually plotted
CC                -1 - placed in first open space
CC                 0 - upper right inside of plot axes
CC                 1 - upper left inside of plot axes
CC                 2 - lower left inside of plot axes
CC                 3 - lower right inside of plot axes
CC                 4 - upper right margin
CC                 5 - upper left margin
CC                 6 - lower left margin
CC                 7 - lower right margin
CC
CC*********************************************************************
CC
      integer*2 i,j,k,l
CC
      include 'OCNTRL.INC'
      include 'OWNTRL.INC'
CC
CC*********************************************************************
CC
      l = 0
      j = jflg - 1
      k =0
   10 k = k + 1
      if (k .gt. 8) then
        j = jflg
        l = 1
        if (ifspq(xp,yp,x1,y1) .ne. 0) then
          LBLPQ = jflg
          go to 20
        end if
      end if
      j = MOD(j + 1,8)
      i = j + 1
      if (i .eq. 1) then
        xp = xrmrpg - x1 - 0.02
        yp = ytmrpg - y1 - 0.02
      else if (i .eq. 2) then
        xp = xlmrgn + 0.02
        yp = ytmrpg - y1 - 0.02
      else if (i .eq. 3) then
        xp = xlmrgn + 0.02
        yp = ybmrgn + 0.02
      else if (i .eq. 4) then
        xp = xrmrpg - x1 - 0.02
        yp = ybmrgn + 0.02
      else if (i .eq. 5) then
        if (l .eq. 0.and.ytmrgn .lt. y1) go to 10
        xp = xrmrpg - x1 - 0.02
        yp = ytmrpg + 0.01
      else if (i .eq. 6) then
        if (l .eq. 0.and.ytmrgn .lt. y1) go to 10
        xp = xlmrgn + 0.02
        yp = ytmrpg + 0.01
      else if (i .eq. 7) then
        if (l .eq. 0.and.spbotm .lt. y1) go to 10
        xp = xlmrgn + 0.02
        yp = spbotm - y1
        if (yp .lt. 0.0) yp = 0.0
      else if (i .eq. 8) then
        if (l .eq. 0.and.spbotm .lt. y1) go to 10
        xp = xrmrpg - x1 - 0.02
        yp = spbotm - y1
        if (yp .lt. 0.0) yp = 0.0
      end if
      if (l .eq. 0) then
        if (IFANQ(xp,yp,x1,y1) .eq. 0) go to 10
        LBLPQ = j
      else
        LBLPQ = jflg
      end if
   20 return
      end
