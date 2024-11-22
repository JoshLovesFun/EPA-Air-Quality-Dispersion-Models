      Function IFRSP(ifmen,mscrn,iline,itabs,ispac,iferr,iflgs)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IFRSP Version 01.1                          ---
CC--- Subroutine description - controls display of auxiliary menus. ---
CC---------------------------------------------------------------------
CC
CC    ifmen - control flag
CC            4 - display help menu.
CC            5 - display units menu.
CC            6 - display fuels/explosives menu.
CC            7 - display current disc files.
CC    mscrn - screen menu display buffer.
CC    iline - array of line numbers containing data fields.
CC    itabs - array of start columns of each data field.
CC    ispac - array of number of columns in each field.
CC    iferr - array of erroneous data flags for each field.
CC    iflgs - allow setting of defaults flag.
CC            0 - no.
CC            1 - yes.
CC
CC    returns - IFRSP -
CC              0 - default data returned via common.
CC              1 - terminate program.
CC              2 - go to main menu.
CC              3 - return to current menu
CC
CC**********************************************************************
CC
      character*1 mscrn
CC
      dimension mscrn(80,25),iline(20),itabs(20,10),ispac(20,10),
     *          iferr(20,10)
CC
      include 'OCNTRL.INC'
CC
CC**********************************************************************
CC
CC      determine which auxiliary menu to display.
      IFRSP = ifmen
      if (ifmen .eq. 4) then
CC        display help menus.
        IFRSP = IHLPS(numscs(9),numsce(9),mscrn,iline,itabs,ispac,
     *                iferr,numscs(16),numsce(16))                      010700
      else if (ifmen .eq. 5) then
CC        display units menus.
        IFRSP = IHLPS(numscs(10),numsce(10),mscrn,iline,itabs,ispac,
     *                iferr,0,0)                                        010700
      else if (ifmen .eq. 6) then
CC        display fuels/explosives menus.
        IFRSP = IFULS(numscs(11),numsce(11),mscrn,iline,itabs,ispac,
     *                iferr,iflgs)
      else if (ifmen .eq. 7) then
CC        display current files menus.
        IFRSP = IDISC(numscs(12),mscrn,iline,itabs,ispac,iferr)
      end if
      return
      end
