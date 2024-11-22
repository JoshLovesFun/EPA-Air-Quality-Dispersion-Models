      Subroutine ITRPQ(xin,yin,xot,yot,xlft,xrit,ybot,ytop)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - ITRPQ Version 01.0                          ---
CC--- Subroutine description - interpolation routine calculates the ---
CC---                          position on the plot boundary at which--
CC---                          the curve crosses.                   ---
CC---------------------------------------------------------------------
CC
CC        xin   - x point inside area
CC        yin   - y point inside area
CC        xot   - x point outside area
CC        yot   - y point outside area
CC        xlft  - left boundary
CC        xrit  - right boundary
CC        ybot  - bottom boundary
CC        ytop  - top boundary
CC
CC**********************************************************************
CC
      include 'OCNTRL.INC'
      include 'OWNTRL.INC'
CC
CC**********************************************************************
CC
CC        calc. intersection of curve and plot boundary
      a = 0.0
      if (ABS(xin - xot) .gt. 0.0) a = (yin - yot) / (xin - xot)
      b = yot - a * xot
      if (xot .ge. xrit.or.xot .le. xlft) then
        if (xot .ge. xrit) then
          xplpnt = xrit
        else
          xplpnt = xlft
        end if
        yplpnt = a * xplpnt + b
        if (yplpnt .gt. ytop.or.yplpnt .lt. ybot) then
          if (yplpnt .gt. ytop) then
            yplpnt = ytop
          else
            yplpnt = ybot
          end if
          if (ABS(a) .gt. 0.0) xplpnt = (yplpnt - b)/a
        end if
      else
        if (yot .gt. ytop) then
          yplpnt = ytop
        else
          yplpnt = ybot
        end if
        if (ABS(a) .gt. 0.0) xplpnt = (yplpnt - b)/a
      end if
      return
      end
