CCPCITS
      interface to subroutine CSCAN[C](istop,isbot)
      integer*2 istop,isbot
      end
      subroutine CRTYP(imode)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - CRTYP version 01.1                          ---
CC--- Subroutine description - set cursor type (note: a number of   ---
CC---                          other configurations are available by---
CC---                          varying isbot and istop. see CSCAN   ---
CC---------------------------------------------------------------------
CC
CC    imode - 0: partial block
CC            1: underscore
CC            2: full block
CC            3: off
CC
CC*********************************************************************
CC
      integer*2 imode,istop,isbot,jstop,jsbot
CC
      dimension jstop(4),jsbot(4)
CC
       data jstop/3,3,0,16/                                             020797
       data jsbot/6,4,4,0/
CC
CC*********************************************************************
CC
      istop = jstop(imode+1)
      isbot = jsbot(imode+1)
      call CSCAN(istop,isbot)
      return
      end
CCPCITE
