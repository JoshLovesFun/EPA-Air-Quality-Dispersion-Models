CC-OBODM input menu related routines
      Function IDSPL(numsc,numwk,mscrn,iline,itabs,ispac,iferr,iflgs,
     *               lstrw,icont)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IDSPL Version 01.1                          ---
CC--- Subroutine description - read screen menu template file, data ---
CC---                          from work file and send to menu      ---
CC---                          control routine IOSCN and write      ---
CC---                          updated menu back to work file.      ---
CC---------------------------------------------------------------------
CC
CC    numsc - screen number to display.
CC    numwk - index of work file data under screen number numsc.
CC    mscrn - nmxcol by nmxrow array for screen menu.
CC    iline  - array containing the line numbers on which data are found.
CC    itabs   - array containing the start character position for each
CC             data parameter on each data line.
CC    ispac  - array containing the number of characters for each data
CC             parameter for each data line.
CC    iferr  - array of error flags for each data field.
CC             0 - no error.
CC             1 - erroneous data.
CC    iflgs   - flag indicating whether or not the work file is read.
CC             0 - no
CC             1 - yes
CC    lstrw  - last menu row.
CC    icont  - line numbers of the start and end of data fields that
CC             continue over lines.  the start line number is in the
CC             10's position and the end line number is in the units
CC             position of the value.  zero indicates no continued lines
CC
CC    returns - IDSPL -
CC              0 - enter key.
CC              1 - f1 key.
CC              2 - f2 key.
CC              3 - f3 key.
CC              4 - f4 key.
CC              5 - f5 key.
CC              6 - f6 key.
CC              7 - f7 key.
CC              8 - f8 key.
CC              9 - f9 key.
CC             10 - f10 key.
CC             11 - pgup key.
CC             12 - pgdn key.
CC
CC**********************************************************************
CC
      integer*2 i,num
CC
      character*1 mscrn
CC
      dimension mscrn(80,25),iline(20),itabs(20,10),ispac(20,10),
     *          iferr(20,10)
CC
CC**********************************************************************
CC
CC      read screen menu template and work file data for menu.
      num = IRDSC(numsc,numwk,mscrn,iline,itabs,ispac,iferr,iflgs)
CC      let user modify data fields in menu.
      IDSPL = IOSCN(numsc,mscrn,iline,itabs,ispac,iferr,lstrw,icont)
CC      write data fields back to work file.
      if (IDSPL .ne. 10) i = IWTSC(numsc,numwk,mscrn,iline,iferr)
      return
      end
