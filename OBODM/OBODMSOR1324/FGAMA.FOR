      Function FGAMA(nv)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - FGAMA Version 01.0                          ---
CC--- Subroutine description - determine the surface reflection     ---
CC---                          coefficient depending on fall velocity--
CC---------------------------------------------------------------------
CC
CC       nv      - particle category.
CC
CC**********************************************************************
CC
      include 'OCDSPM.INC'
CC
CC**********************************************************************
CC
      if (gammad(nv) .ge. 0.0) then
        FGAMA = gammad(nv)
      else
        vs = vspart(nv)
        if (vs .ge. 0.3) then
          FGAMA = 0.0
        else if (vs .lt. 0.05) then
          FGAMA = 1.0 - 1.0179066 * vs**0.33333333
        else
          FGAMA = 2.5 * (0.3 - vs)
        end if
      end if
      return
      end
