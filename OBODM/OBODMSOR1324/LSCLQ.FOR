      Subroutine LSCLQ(jflg,nn,ioptn)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - LSCLQ Version 01.1                          ---
CC--- Subroutine description - display color options                ---
CC---------------------------------------------------------------------
CC
CC        jflg - index to plot type and color parameter
CC              >0 - set default color and color options
CC              <0 - check entered color code against possible
CC               1 - background color
CC               2 - color of axes and labels
CC               3 - nn = 1, color of y vs. x and line of max curve
CC                           for surface 1.
CC                   nn = 2, color of line of maximum curve for
CC                           surface 2.
CC               4 - color of special symbol nn
CC               5 - color of contour line nn
CC
CC**********************************************************************
CC
      dimension ioptn(16)
CC
      include 'OCNTRL.INC'
      include 'OWNTRL.INC'
CC
CC**********************************************************************
CC
      if (jflg .ge. 0) then
CC        turn color options on
        if (ipmode .ge. 16) then
          i = JINIT(ioptn,16,1,0)
        else
          j = 4
          i = JINIT(ioptn,j,1,0)
          i = JINIT(ioptn(j+1),16-j,0,0)
        end if
      end if
      jflgs = IABS(jflg)
      if (jflgs .eq. 1) then
CC        background color.
        call CLRCQ(jflg,iswplt(9),ioptn)
      else if (jflgs .eq. 2) then
CC        axes, labels color.
        call CLRCQ(jflg,iswplt(16),ioptn)
      else if (jflgs .eq. 3) then
        if (nn .le. 2) then
          if (nn .eq. 1) then
CC            y vs. x and line of maximum for surface 1 color.
            call CLRCQ(jflg,iswplt(12),ioptn)
          else
CC            line of maximum for surface 2 color.
            call CLRCQ(jflg,iswplt(23),ioptn)
          end if
        end if
      else if (jflgs .eq. 4) then
CC        special symbol color
        call CLRCQ(jflg,ispclr(nn),ioptn)
      else if (jflgs .eq. 5) then
        call CLRCQ(jflg,icnclr(nn,icontr),ioptn)
      end if
      return
      end
