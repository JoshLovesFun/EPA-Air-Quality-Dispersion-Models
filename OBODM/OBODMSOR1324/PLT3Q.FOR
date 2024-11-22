      Subroutine PLT3Q(xplt,yplt,ipen)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - PLT3Q Version 01.0                          ---
CC--- Subroutine description - controls plot pen movement.          ---
CC---------------------------------------------------------------------
CC
CC       xplt - x coordinate (plotter units)
CC       yplt - y coordinate (plotter units)
CC       ipen - 2 = pen down, 3 = pen up, negative sets new origin,
CC              999 = plot end (calcomp only).
CC
CC**********************************************************************
CC
      character*22 msrch
      integer*2 ix,iy,ixl,iyl,icl,i
CC
      include 'OCNTRL.INC'
      include 'OWNTRL.INC'
      include 'ONAMES.INC'
CC
      data msrch/'SCRATCH plot file'/
CC
CC**********************************************************************
CC
      xp = xplt
      yp = yplt
CC        if reversed x direction.
      if (irxndx .ne. 0.and.jrxndx .eq. 0) xp = xrmrpg - xp + xlmrgn
CC        if reversed y direction.
      if (iryndx .ne. 0.and.jryndx .eq. 0) yp = ytmrpg - yp + ybmrgn
      if (iswplt(5) .ne. 0) then
CC        swap axes.
        a = xp
        xp = yp
        yp = xnchpl - a
      end if
CC
      ix = xp * rastrh + 0.5
      iy = yp * rastrv + 0.5
      if (ipen .eq. 2) then
        ixl = ilstxs
        iyl = ilstys
        icl = icolor
        if (icl .eq. iswplt(9)) icl = icl + 8
CCPCITS
        call QLINE(ix,iy,ixl,iyl,icl)
CCPCITE
      end if
      ilstxs = ix
      ilstys = iy
      if (iplfil .gt. 0) then
CC        write point to be plotted to file.
        i = icolor
        if (i .le. 0) i = 1
        WRITE (iplfil,9000,err=10,iostat=ier) xp,yp,ipen,i
        go to 20
   10   i = KERRS(2,mplnam,ier,0,xdum,xdum,xdum,xdum,0)
        iplfil = 0
      end if
   20 if (iscfil .gt. 0) then
CC       write point to be plotted to scratch file.
        WRITE (iscfil,9000,err=30,iostat=ier) xp,yp,ipen,icolor
        go to 40
   30   call MBLK1(mnpbuf,60)
        i = KERRS(2,msrch,ier,0,xdum,xdum,xdum,xdum,0)
        iscfil = 0
      end if
   40 return
CC
 9000 format (' P',2f8.4,2i2)
      end
