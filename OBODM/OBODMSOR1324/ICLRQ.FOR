      Function ICLRQ(iclnd,jflg)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - ICLRQ Version 01.0                          ---
CC--- Subroutine description - controls setting color and color     ---
CC---                          screen.                              ---
CC---------------------------------------------------------------------
CC
CC       iclnd  - color index to set, see icolor in main.
CC       jflg   - background/foreground flag.
CC                0 = background
CC                1 = foreground
CC
CC**********************************************************************
CC
CC
      include 'OCNTRL.INC'
      include 'OWNTRL.INC'
      include 'ONAMES.INC'
CC
CC**********************************************************************
CC
      ICLRQ = iclnd
CC
      if (ipmode .lt. 16.and.jflg .ne. 0) then
CC        foreground.
        ICLRQ = MOD(ICLRQ,4)
        if (ICLRQ .eq. iswplt(9)) then
          ICLRQ = MOD(ICLRQ+1,4)
        end if
      end if
      lcolor = ICLRQ
      return
      end
