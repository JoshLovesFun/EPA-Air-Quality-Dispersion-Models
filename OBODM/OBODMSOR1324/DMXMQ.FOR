      Subroutine DMXMQ(iflg,xk,yk,nxk,nyk,jfpol)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - DMXMQ Version 01.2                          ---
CC--- Subroutine description - calculate default axes max and min   ---
CC---                          and axes scale factors.              ---
CC---------------------------------------------------------------------
CC
CC    iflg  - initialize flag.
CC            0 = initialize.
CC            1 = do not initialize.
CC    xk    - x coordinates
CC    yk    - y coordinates
CC    nxk   - number of x coordinates
CC    nyk   - number of y coordinates
CC    jfpol - coordinates are: 
CC            0 - rectangular
CC            1 - polar
CC
CC**********************************************************************
CC
      integer*2 i
CC
      dimension xk(100),yk(100)
CC
      include 'OCNTRL.INC'
      include 'OWNTRL.INC'
      include 'OCALCS.INC'
CC
CC**********************************************************************
CC
      if (iflg .eq. 0) then
        pxmins = -rmissd
        pymins = -rmissd
        pxmaxs = rmissd
        pymaxs = rmissd
        sclxes = 0.0
        sclyes = 0.0
      end if
CC        set lswxax and lswyax - 0 = linear axis, 1 = log axis.
CC        also, determine max and min for x and y axes
      lswxax = 0
      if (iswplt(2) .eq. 1.or.iswplt(2) .eq. 3) lswxax = 1
      if (jfpol .eq. 0) then
        if (pxmaxs .le. rmissd) pxmaxs = CMXNQ(xk,1,nxk,lswxax)
        if (pxmins .ge. -rmissd) pxmins = CMXNQ(xk,2,nxk,lswxax)
      else
        if (pxmaxs .le. rmissd) then
          pxmaxs = xk(nxk)
          if (icontr .ne. 1) pxmaxs = pxmaxs + xorgin
        end if        
        if (pxmins .ge. -rmissd) then
          pxmins = -xk(nxk)      
          if (icontr .ne. 1) pxmins = pxmins + xorgin
        end if
      end if  
      if (pxmaxs .le. pxmins) pxmaxs = pxmins + 1.0
      if (iflg .eq. 0.and.lswxax .ne. 0) then
        if (pxmaxs-pxmins .gt. XINTQ(xnchpl-xlmrgn-xrmrgn)) then
          pxmins = pxmaxs - XINTQ(xnchpl-xlmrgn-xrmrgn)
        end if
      end if
      lswyax = 0
      if (iswplt(2) .eq. 2.or.iswplt(2) .eq. 3) lswyax = 1
      if (jfpol .eq. 0) then
        if (pymaxs .le. rmissd) pymaxs = CMXNQ(yk,1,nyk,lswyax)
        if (pymins .ge. -rmissd) pymins = CMXNQ(yk,2,nyk,lswyax)
      else
        if (pymaxs .le. rmissd) then
          pymaxs = xk(nxk)
          if (icontr .ne. 1) pymaxs = pymaxs + yorgin
        end if
        if (pymins .ge. -rmissd) then
          pymins = -xk(nxk)
          if (icontr .ne. 1) pymins = pymins + yorgin
        end if
      end if
      if (pymaxs .le. pymins) pymaxs = pymins + 1.0
      if (iflg .eq. 0.and.lswyax .ne. 0) then
        if (pymaxs-pymins .gt. XINTQ(ynchpl-ybmrgn-ytmrgn)) then
          pymins = pymaxs - XINTQ(ynchpl-ybmrgn-ytmrgn)
        end if
      end if
      xsmins = pxmins
      ysmins = pymins
CC        determine plot x and y scale factors
      i = 0
      if (sclxes .le. 0.0) then
        xdumy = ABS(pxmaxs - pxmins)
        if (xdumy .le. 0.0) xdumy = 1.0
        sclxes = ABS((xnchpl - xlmrgn - xrmrgn) / xdumy)
        i = 1
      end if
      if (sclyes .le. 0.0) then
        xdumy = ABS(pymaxs - pymins)
        if (xdumy .le. 0.0) xdumy = 1.0
        sclyes = ABS((ynchpl - ytmrgn - ybmrgn) / xdumy)
        i = 1
      end if
      if (i .ne. 0) then
        sclxys = SQRT(sclxes * sclxes + sclyes * sclyes)
      end if
      return
      end
