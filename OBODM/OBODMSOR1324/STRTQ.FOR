      Subroutine STRTQ(xinc,xst,pxmax,pxmin,sclxes)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - STRTQ Version 01.1                          ---
CC--- Subroutine description - determine start value of axes.       ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      xinc = (pxmax - pxmin) * 0.02
      if (xinc .le. 0.0) xinc = 1.0
      nn = ALOG10(xinc)
      if (nn .le. 0) nn = nn - 1
      xinc = 10.0**nn
      nn = 5
   10 if (xinc * 10.0 * sclxes .lt. 1.0) then
        if (nn .ne. 2) then
          nn = 2
        else
          nn = 5
        end if
        xinc = xinc * FLOAT(nn)
        go to 10
      end if
   20 if ((pxmax - pxmin) / (xinc * 10.0) .lt. 2.0) then
        xinc = xinc * 0.5
        go to 20
      end if
      xst = XINTQ(pxmin / xinc) * xinc
      return
      end
