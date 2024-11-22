      Function IHLPS(numsc,numnd,mscrn,iline,itabs,ispac,iferr,
     *               jumsc,jumnd)                                       010700
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IHLPS Version 01.1                          ---
CC--- Subroutine description - display help screens.                ---
CC---------------------------------------------------------------------
CC
CC    numsc - start screen number to display.
CC    numnd - end screen number to display.
CC    mscrn - screen menu display buffer.
CC    iline - array of line numbers containing data fields.
CC    itabs - array of start columns of each data field.
CC    ispac - array of number of columns in each field.
CC    iferr - array of erroneous data flags for each field.
CC    jumsc - start aux. screen number to display after numnd.
CC    jumnd - end aux. screen number to display.
CC
CC    returns - IHLPS -
CC              1 - terminate.
CC              2 - main menu.
CC              3 - return to current menu.
CC
CC**********************************************************************
CC
      integer*2 nn,numf
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
CC      display auxiliary menus.
CC
      nn = 0
CC      screen numsc+nn.
   10 numf = IRDSC(numsc+nn,1,mscrn,iline,itabs,ispac,iferr,0)
CC      disply screen menu and get users response.
      IHLPS = IOSCN(numsc+nn,mscrn,iline,itabs,ispac,iferr,lstrw,0)
CC      check users response.
      if (IHLPS .eq. 1) then
CC        terminate
        go to 40
      else if (IHLPS .eq. 2) then
CC        go to main menu.
        ifnext = 3
        go to 40
      else if (IHLPS .eq. 3) then
CC        return to current menu.
        go to 40
      else if (IHLPS .eq. 11) then
CC        go back to previous menu.
        nn = nn - 1
        if (nn .lt. 0) then
CCPCITS
        call IBELL(1)
CCPCITE
          nn = 0
        end if
      else if (IHLPS .eq. 0.or.IHLPS .eq. 12) then
CC        go to next menu.
        nn = nn + 1
        if (numsc+nn .gt. numnd) then
          if (jumsc .gt. 0) then                                        010700
            nn = nn - 1                                                 010700
            go to 20                                                    010700
          end if                                                        010700
CC          restart at first.                                           
          nn = 0
        end if
      end if
      go to 10
CC                                                                      010700
   20 jj = 0                                                            010700
CC      screen numsc+nn.                                                010700
   30 numf = IRDSC(jumsc+jj,1,mscrn,iline,itabs,ispac,iferr,0)          010700
CC      disply screen menu and get users response.                      010700
      IHLPS = IOSCN(jumsc+jj,mscrn,iline,itabs,ispac,iferr,lstrw,0)     010700
CC      check users response.                                           010700
      if (IHLPS .eq. 1) then                                            010700
CC        terminate                                                     010700
        go to 40                                                        010700
      else if (IHLPS .eq. 2) then                                       010700
CC        go to main menu.                                              010700
        ifnext = 3                                                      010700
        go to 40                                                        010700
      else if (IHLPS .eq. 3) then                                       010700
CC        return to current menu.                                       010700
        go to 40                                                        010700
      else if (IHLPS .eq. 11) then                                      010700
CC        go back to previous menu.                                     010700
        jj = jj - 1                                                     010700
        if (jj .lt. 0) then                                             010700
          go to 10                                                      010700
        end if                                                          010700
      else if (IHLPS .eq. 0.or.IHLPS .eq. 12) then                      010700
CC        go to next menu.                                              010700
        jj = jj + 1                                                     010700
        if (jumsc+jj .gt. jumnd) then                                   010700
CC          restart at first.                                           010700
          nn = 0                                                        010700
          go to 10                                                      010700
        end if                                                          010700
      end if                                                            010700
      go to 30                                                          010700
CC                                                                      010700
   40 return
      end
