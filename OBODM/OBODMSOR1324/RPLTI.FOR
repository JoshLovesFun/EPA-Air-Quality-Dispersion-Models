      Subroutine RPLTI
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - RPLTI Version 01.1                          ---
CC--- Subroutine description - draws special symbols.               ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      include 'OCNTRL.INC'
      include 'OWNTRL.INC'
      include 'OCDSPM.INC'
      include 'OCALCS.INC'
CC
CC*********************************************************************
CC
CC      plot special symbols.
      iswplt(17) = 2
      do 40 i=1,nspcls
        icolor = ICLRQ(ispclr(i),1)
        if (isppol(i) .ne. 0) then
          yp = spclys(i) * dtorad
          xp = spclxs(i) * SIN(yp) + xorgin
          yp = spclxs(i) * COS(yp) + yorgin
        else
          xp = spclxs(i)
          yp = spclys(i)
        end if
        xp = (xp - pxmins) * sclxes + xlmrgn
        yp = (yp - pymins) * sclyes + ybmrgn
        call SPCLQ(xp,yp,spclht(i),spclan(i),ispclc(i))
   40 continue
      return
      end
