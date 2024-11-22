      Subroutine PLT2Q(xp,yp,lsw)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - PLT2Q Version 01.0                          ---
CC--- Subroutine description - confines plot points, curves, etc. to---
CC---                          the area inside of the plot margins. ---
CC---------------------------------------------------------------------
CC
CC        xp     - x axis plot position.
CC        yp     - y axis plot position.
CC        lsw    - pen flag.
CC                 2 = pen down before move. drawn line from present
CC                     pen position to xp,yp.
CC                 3 = pen up before move. move pen position to xp,yp.
CC
CC        returns (via common) -
CC        ifbndx - out of bounds flag
CC                 0 = point out of plot boundary
CC                 1 = point inside of plot boundary
CC            2 or 3 = curve crossed over plot boundary
CC                     2 = curve going out.
CC                     3 = curve coming in.
CC        xbndxs = x plot point of intersection, if ifbndx = 2 or 3.
CC        ybndxs = y plot point of intersection, if ifbndx = 2 or 3.
CC
CC**********************************************************************
CC
      integer*2 ifst,jflg,jflgl
CC
      include 'OCNTRL.INC'
      include 'OWNTRL.INC'
CC
      save msw,ifst,xlst,ylst,jflgl
CC
      data msw/3/,ifst/0/,xlst/0.0/,ylst/0.0/,jflgl/0/
CC
CC**********************************************************************
CC
      ifbndx = 0
      if (iswplt(17) .eq. 1) then
        xlft = 0.0
        xrit = xnchpl
        ybot = 0.0
        ytop = ynchpl
      else
        xlft = xlmrgn
        xrit = xrmrpg
        ybot = ybmrgn
        ytop = ytmrpg
      end if
      if (lsw .ne. 2) then
        ifst = 0
        msw = 3
      end if
      xplpnt = xp
      yplpnt = yp
      jflg = 0
CC        if point outside boundaries
      if (IFBXQ(xplpnt,yplpnt,xlft,xrit,ybot,ytop) .eq. 0) then
        jflg = -1
      else
CC        if point inside special label or legend
CC>     jflg = NBXPQ(xplpnt,yplpnt)
        jflg = 0
      end if
      if (jflg .ne. 0) then
CC        point is outside of plot area
CC        if last not out
        if (msw .ne. 3) then
CC        last in, this out
          if (jflg .lt. 0) then
            call ITRPQ(xlst,ylst,xp,yp,xlft,xrit,ybot,ytop)
            ifbndx = 2
            xbndxs = xplpnt
            ybndxs = yplpnt
          else
            call ITRPQ(xp,yp,xlst,ylst,xbxpsl(jflg),xbxpel(jflg),
     *                 ybxpsl(jflg),ybxpel(jflg))
          end if
          jflgl = jflg
          call PLT3Q(xplpnt,yplpnt,msw)
          msw = 3
        else
          jflgl = jflg
        end if
      else
CC        point is inside plot area
        ifbndx = 1
CC        if the last point was out
        if (msw .ne. 2) then
CC          last out, this in
CC          if the first point
          if (ifst .eq. 0) then
            ifst = 1
            jflgl = 0
          else
CC            second or great point
            if (jflgl .le. 0) then
              call ITRPQ(xp,yp,xlst,ylst,xlft,xrit,ybot,ytop)
              ifbndx = 3
              xbndxs = xplpnt
              ybndxs = yplpnt
            else
              call ITRPQ(xlst,ylst,xp,yp,xbxpsl(jflgl),xbxpel(jflgl),
     *                   ybxpsl(jflgl),ybxpel(jflgl))
            end if
            call PLT3Q(xplpnt,yplpnt,msw)
            xplpnt = xp
            yplpnt = yp
            msw = 2
            jflgl = 0
          end if
        end if
        call PLT3Q(xplpnt,yplpnt,msw)
        msw = 2
      end if
      xlst = xp
      ylst = yp
      if (ifst .eq. 0) ifst = 1
      return
      end
