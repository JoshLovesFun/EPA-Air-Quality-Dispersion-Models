      Subroutine SPCLQ(x,y,ht,ang,ich)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - SPCLQ Version 01.1                          ---
CC--- Subroutine description - draws special characters centered at ---
CC---                          x,y.                                 ---
CC---------------------------------------------------------------------
CC
CC        x      - x plot position.
CC        y      - y plot position.
CC        ht     - height of symbol.
CC        ang    - angle of symbol.
CC        ich    - special symbol flag index.
CC                 1 = box with line from center to upper right
CC                     corner.
CC                 2 = box with corners cut off with line from
CC                     center to top center.
CC                 3 = triangle with line from center to top
CC                     center peak.
CC                 4 = plus sign.
CC                 5 = x symbol.
CC                 6 = diamond with line from center to top center
CC                     peak.
CC                 7 = upward pointing arrow.
CC                 8 = x symbol with line jointing the upper ends
CC                     of lines.
CC                 9 = z symbol with small line drawn mid-way down
CC                     horizontally.
CC                10 = y symbol.
CC                11 = box with diagonal lines drawn out from each
CC                     corner and with a line from the center of
CC                     the box to the upper right corner.
CC                12 = asterisk symbol.
CC                13 = hourglass symbol.
CC                14 = circle on top of vertical line
CC                15 = flag
CC
CC**********************************************************************
CC
      integer*2 ix,iy,ni,li,ji,jch,ii,jj,nn
CC
      dimension ix(89),iy(89),ni(15),li(15),ji(89)
CC
      include 'OCNTRL.INC'
      include 'OWNTRL.INC'
      include 'OCDSPM.INC'
CC
      data ix/2*6,2*-6,6,2*0,4,2*6,4,-4,2*-6,-4,3*0,6,-6,4*0,-6,2*6,
     *        2*-6,6,0,6,0,-6,4*0,6,-6,0,6,-6,6,2*-6,6,-6,6,-4,4,2*0,
     *        6,-6,0,-6,-3,2*3,2*-3,-6,-3,6,0,6,3,2*0,-6,6,-6,6,0,0,
     *        -6,6,-6,6,-6,3*0,2*0,2*6,0/
      data iy/6,2*-6,2*6,0,2*6,4,-4,2*-6,-4,4,2*6,0,6,2*-6,6,0,6,-6,
     *        2*0,6,-6,6,-6,6,0,-6,0,6,0,-6,6,2*0,6,-6,2*6,-6,2*6,2*-6,
     *        2*0,-6,0,2*6,0,-6,2*-3,2*3,-3,6,3,6,0,-6,-3,6,-6,3,-3,
     *        -3,3,-6,3*6,2*-6,6,3,0,-6,-6,2*6,2*0/
      data ni/6,11,5,2*4,6,5,4,6,5,12,8,5,3,5/
      data li/0,6,17,22,26,30,36,41,45,51,56,68,76,81,84/
      data ji/3,5*2,3,10*2,3,4*2,3,2,3,2,3,2,3,2,3,5*2,3,4*2,3,3*2,
     *        3,3*2,3,2,3,2*2,3,2,3,5*2,3,2,3,2,3,2,3,2,3,2,3,2,3,2,3,
     *        4*2,2*3,2,3,4*2/
CC
CC**********************************************************************
CC
      scl = ht/12.0
      jch = MOD(ich-1,15) + 1
      if (jch .lt. 1.or.jch .gt. 15) jch = 4
      ii = 0
   10 if (ii .lt. ni(jch)) then
        ii = ii + 1
        jj = li(jch)
        jpen = ji(ii+jj)
        xp = ix(ii+jj)
        yp = iy(ii+jj)
        nn = 0
        if (ii .gt. 1.or.jch .ne. 14) go to 30
   20   nn = nn + 5
        cir = nn * dtorad
        xp = 3.0 * COS(cir) + ix(ii+jj)
        yp = 3.0 * SIN(cir) + iy(ii+jj)
   30   if (ABS(ang) .gt. 0.0) then
          r = SQRT(xp * xp + yp * yp)
          t = ARTAN(xp,yp)
          t = t + ang * dtorad
          xp = r * SIN(t)
          yp = r * COS(t)
        end if
        xp = xp * scl
        yp = yp * scl
        call PLT2Q(x+xp,y+yp,jpen)
        jpen = 2
        if (nn .gt. 0.and.nn .lt. 365) go to 20
        go to 10
      end if
      call PLT2Q(x,y,3)
      return
      end
