      Function JCLRQ(iclnd,ichg,icl,jcl,icwt)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - JCLRQ Version 01.0                          ---
CC--- Subroutine description - controls setting color and color     ---
CC---                          screen.                              ---
CC---------------------------------------------------------------------
CC
CC       iclnd  - color index to set, see icolor in main.
CC       ichg   - invert black and white flag.
CC                0 - no
CC                1 - yes
CC       icl    - returned color index.
CC       jcl    - returned screen index.
CC       icwt   - index of white.
CC
CC**********************************************************************
CC
      include 'OWNTRL.INC'
CC
CC**********************************************************************
CC
      JCLRQ = iclnd
      if (ichg .ne. 0) then
        if (JCLRQ .eq. 0) then
          JCLRQ = icwt
        else if (JCLRQ .eq. icwt) then
          JCLRQ = 0
        end if
      end if
      icl = JCLRQ + 1
      if (JCLRQ .eq. iswplt(9)) then
        jcl = 0
      else if (JCLRQ .eq. iswplt(13)) then
        jcl = 1
      else if (JCLRQ .eq. iswplt(14)) then
        jcl = 2
      else if (JCLRQ .eq. iswplt(15)) then
        jcl = 3
      else
        jcl = MOD(JCLRQ + 1,3) + 1
        icl = iswplt(12+jcl) + 1
      end if
      lcolor = JCLRQ
      return
      end
