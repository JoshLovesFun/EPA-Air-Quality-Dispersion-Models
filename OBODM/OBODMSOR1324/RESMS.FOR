CCPCITS
      interface to Subroutine SHOWM[C]
      end
      interface to Subroutine SETML[C](ix,iy)
      integer*2 ix, iy
      end
      interface to integer*2 Function GETCL[C](ibuttn,ix,iy)
      integer*2 ibuttn,ix[REFERENCE],iy[REFERENCE]
      end
      interface to Subroutine GETML[C](ix,iy)
      integer*2 ix[REFERENCE],iy[REFERENCE]
      end
      Subroutine RESMS(iflg)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - RESMS Version 01.1                          ---
CC--- Subroutine description - reset mouse.                         ---
CC---------------------------------------------------------------------
CC
CC    iflg - control flag.
CC           0 - reset and home mouse
CC           1 - save mouse current location.
CC           2 - restore mouse to previous location.
CC
CC*********************************************************************
CC
      integer*2 ihome,iret,GETCL,ix,iy
CC
      include 'OCNTRL.INC'
CC
CC*********************************************************************
CC
      if (ifmous .ne. 0) then
        if (iflg .le. 0) then
          ihome = 0
          iret = GETCL(0,ix,iy)
          call SETML(ihome,ihome)
          call GETML(ix,iy)
          call SHOWM
        else if (iflg .eq. 1) then
          call GETML(ix,iy)
          locxms = ix
          locyms = iy
        else
          call SETML(locxms,locyms)
          call SHOWM
        end if
      end if
      return
      end
CCPCITE
