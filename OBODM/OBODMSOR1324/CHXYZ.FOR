      Subroutine CHXYZ(xdumx)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - CHXYZ Version 01.0                          ---
CC--- Subroutine description - converts x, y, and z units           ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      include 'OCNTRL.INC'
      include 'OCDSPM.INC'
      include 'OCALCS.INC'
CC
CC*******************************************************************
CC
CC      change grid points to correct output units.
      xdumx = 1.0 / constl(iswopt(15)+1)
      if (nxpnts .gt. 0) then
        do 10 i=1,nxpnts
          xcoord(i) = xcoord(i) * xdumx
   10   continue
        do 30 j=1,nypnts
          do 20 i=1,nxpnts
            n = (j - 1) * nxpnts + i
            zcoord(n) = zcoord(n) * xdumx
   20     continue
   30   continue
      end if
      if (nxypnt .gt. 0) then
        do 40 i=1,nxypnt
          xdscrt(i) = xdscrt(i) * xdumx
          zdscrt(i) = zdscrt(i) * xdumx
   40   continue
      end if
      xorgin = xorgin * xdumx
      yorgin = yorgin * xdumx
      if (igrect .le. 0) then
        if (nypnts .gt. 0) then
          do 50 i=1,nypnts
            ycoord(i) = ycoord(i) * xdumx
   50     continue
        end if
        if (nxypnt .gt. 0) then
          do 60 i=1,nxypnt
            ydscrt(i) = ydscrt(i) * xdumx
   60     continue
        end if
      end if
      ifcnvt = 1
      return
      end
