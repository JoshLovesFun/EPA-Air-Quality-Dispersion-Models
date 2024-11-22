      Subroutine BKGDQ(xstr,ystr,xend,yend,jsw)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - BKGDQ Version 01.0                          ---
CC--- Subroutine description - outline and background plot areas.   ---
CC---------------------------------------------------------------------
CC
CC        xstr  - start x coord in inches
CC        ystr  - start y coord in inches
CC        xend  - end x coord in inches
CC        yend  - end y coord in inches
CC        jsw   - 1 = set interior to background
CC                2 = outline with solid line
CC
CC********************************************************************
CC
CC
      character*22 msrch
CC
      include 'OCNTRL.INC'
      include 'OWNTRL.INC'
      include 'ONAMES.INC'
CC
      data msrch/'SCRATCH plot file'/
CC
CC********************************************************************
CC
      call PLT2Q(xstr,ystr,3)
      call PLT2Q(xend,ystr,2)
      call PLT2Q(xend,yend,2)
      call PLT2Q(xstr,yend,2)
      call PLT2Q(xstr,ystr,2)
      if (iscfil .gt. 0.and.jsw .le. 1) then
        WRITE (iscfil,9000,err=10,iostat=ier) xstr,ystr,xend,yend,icolor
        go to 20
   10   call MBLK1(mnpbuf,60)
        i = KERRS(2,msrch,ier,0,xdum,xdum,xdum,xdum,0)
        iscfil = 0
      end if
   20 return
CC
 9000 format (' B',4f8.4,i2)
      end
