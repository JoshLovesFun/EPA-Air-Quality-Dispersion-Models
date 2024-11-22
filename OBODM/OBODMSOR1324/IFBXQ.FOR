      Function IFBXQ(xp,yp,xstr,xend,ystr,yend)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IFBXQ Version 01.0                          ---
CC--- Subroutine description - determine if the point is inside of  ---
CC---                          the rectangle defined by xstr, xend, ---
CC---                          ystr,yend.                           ---
CC---------------------------------------------------------------------
CC
CC        returns - IFBXQ -
CC                         0 = outside
CC                         1 = inside
CC
CC**********************************************************************
CC
      IFBXQ = 0
      if (xp .ge. xstr.and.xp .le. xend) then
        if (yp .ge. ystr.and.yp .le. yend) IFBXQ = 1
      end if
      return
      end
