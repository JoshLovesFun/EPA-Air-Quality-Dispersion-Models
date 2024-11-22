      Subroutine PLT1Q(xp,yp,lsw,dlng)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - PLT1Q Version 01.0                          ---
CC--- Subroutine description - plots lines, dashed lines, dotted    ---
CC---                          lines, dash dot lines, etc.          ---
CC---------------------------------------------------------------------
CC
CC        xp     - x axis plot position.
CC        yp     - y axis plot position.
CC        lsw    - pen flag.
CC                 2 = pen down before move. drawn line from present
CC                     pen position to xp,yp.
CC                 3 = pen up before move. move pen position to xp,yp.
CC        dlng   - length in inches of dashes
CC
CC**********************************************************************
CC
      integer*2 lst,nl
CC
      include 'OCNTRL.INC'
      include 'OWNTRL.INC'
CC
      save xlst,ylst,lst,nl,dlst
CC
CC**********************************************************************
CC
      if (ifdash .eq. 0) then
        call PLT2Q(xp,yp,lsw)
      else if (lsw .ne. 2) then
        dlst = 0.0
        xlst = xp
        ylst = yp
        lst = 1
        nl = 2
        if (ifdash .ge. 3) nl = 2 * ifdash - 2
        call PLT2Q(xp,yp,lsw)
      else
        dx = xp - xlst
        dy = yp - ylst
        dr = SQRT(dx * dx + dy * dy)
        th = ARTAN(dy,dx)
        css = COS(th)
        sss = SIN(th)
   10   dinc = dlng
        if (ifdash .ne. 1) then
          if (ifdash .eq. 2.or.lst .ne. 1) then
            dinc = 0.015
            if (MOD(lst,2) .eq. 0) dinc = 0.035
          end if
        end if
        dinc = dinc - dlst
        if (dinc .gt. dr) then
          dinc = dr
          dlst = dlst + dinc
        else
          dlst = 0.0
        end if
        xn = xlst + dinc * css
        yn = ylst + dinc * sss
        ltsw = 2
        if (MOD(lst,2) .eq. 0) ltsw = 3
        call PLT2Q(xn,yn,ltsw)
        xlst = xn
        ylst = yn
        dr = dr - dinc
        if (dlst .le. 0.0) then
          lst = lst + 1
          if (lst .gt. nl) lst = 1
          if (dr .gt. 0.0) go to 10
        end if
      end if
      return
      end
