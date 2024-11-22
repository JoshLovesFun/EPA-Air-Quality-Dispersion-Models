CCPCITS
      Subroutine OBSIO(numsc,mscrn,iline,itabs,ispac,iferr,lstrw,ioscs,
     *                 icont)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - OBSIO Version 01.2  (pc version)            ---
CC--- Subroutine description - controls the display of the screen   ---
CC---                          menu on the users terminal, controls ---
CC---                          reading of data fields from users    ---
CC---                          terminal screen, and returns users   ---
CC---                          response and final cursor position.  ---
CC---------------------------------------------------------------------
CC
CC    numsc - function control or screen menu number.
CC                 -1 - get user response only.  used at the end of
CC                      plots to see what the user wants to do next,
CC                      do not change screen.
CC                  0 - initialize screen graphics mode, initialize
CC                      OBODM screen menu processing mode.  program
CC                      stays in this mode through the duration of
CC                      the session.
CC            1 to 99 - screen menu number.  display the screen menu
CC                      in mscrn, allow user to change data in any
CC                      data fields and return user response after
CC                      user is through editing the menu data fields.
CC                100 - terminate screen graphics mode.  indicates the
CC                      end of OBODM, where other routines actually
CC                      terminate the program and return to system
CC                      control.
CC    mscrn - 80 by 25 character array containing the screen menu.
CC            the first dimension (80) specifies screen columns left to
CC            right, 1 to 80.  the second dimension specifies screen
CC            line number top to bottom, 1 to 25.
CC    iline - array giving the line numbers for lines in mscrn that
CC            contain data fields. if iline(k) > 0, then the line
CC            starting at mscrn(1,iline(k)) contains data fields,
CC            where k = 1 to 20 (maximum number of lines that can
CC            contain data fields = 20).
CC    itabs - array giving the start columns of each data field on the
CC            line given by iline(k). the data field starts at
CC            mscrn(itabs(k,l),iline(k)) for the l'th field on the line,
CC            where l = 1 to 10 (maximum number of data fields per
CC            line = 10).
CC    ispac - array giving the number of columns in each data field.
CC            the l'th data field ends at mscrn(itabs(k,l)+ispac(k,l)-1,
CC            iline(k))
CC    iferr - array indicating if data in each data field is erroneous
CC            or not. if iferr(k,l) =
CC              0 - no error.
CC              1 - erroneous, highlight this field.
CC              2 - warning, highlight this field.
CC    lstrw - if enter is typed by the user, the program expects the
CC            line number of the cursor position returned in this
CC            parameter, lstrw = 1 to 25.
CC    ioscs - returned i/o status -
CC             0 - enter key typed, return lstrw.
CC             1 - f1 key typed.
CC             2 - f2 key typed.
CC             3 - f3 key typed.
CC             4 - f4 key typed.
CC             5 - f5 key typed.
CC             6 - f6 key typed.
CC             7 - f7 key typed.
CC             8 - f8 key typed.
CC             9 - f9 key typed.
CC            10 - f10 key typed.
CC            11 - pgup key typed.
CC            12 - pgdn key typed.
CC    icont - line numbers of the start and end of data fields that
CC            continue over lines.  the start line number is in the
CC            10's position and the end line number is in the units
CC            position of the value.  zero indicates no continued lines.
CC
CC*********************************************************************
CC
      character*1 mscrn
CC
      integer*2 icha,ibac,ifor,ione,icol,irow,imode
CC
      dimension mscrn(80,25),iline(20),itabs(20,10),ispac(20,10),
     *          iferr(20,10),ioptn(16)
CC
      include 'OCNTRL.INC'
      include 'OWNTRL.INC'
      include 'OCDSPM.INC'
CC
      equivalence (ioptn(1),sigzls)
CC
      save istr
CC
      data istr/0/,ione/1/
CC
CC********************************************************************
CC
CC      write menu to crt screen.
CC
CC      note that order of operations is very important.
CC
CC      restore buffer if required
      if (istr .eq. 1) then
        ispage = 2
CC        turn cursor off
        imode = 3
        call CRTYP(imode)
        do 10 icol=0,icolmx-1
        do 10 irow=0,irowmx-1
          call QCMOV(icol,irow)
          call QRTXT(icha,ibac,ifor)
          call QSTXT(icha,ibac,ifor,ione)
   10   continue
CC        turn cursor on
        imode = 1
        call CRTYP(imode)
        istr = 0
      end if
CC
      ispage = ibpage
      if (numsc .lt. 0) ispage = idpage
      call IOCRT(mscrn,iline,itabs,ispac,iferr,ibackt,iforet)
      if (isystm .eq. 2.and.numsc .eq. 68.and.(ipmode .ge. 16.or.
     *    ipmode .eq. 4.or.ISUMI(ioptn,16) .le. 4)) then
        call SPDSP(0)
      end if
      if (numsc .ge. 0) then
        call STATB(numsc)
        call PRBAR
        call RESMS(0)
        call DISPG(ispage)
        idpage = ibpage
        ibpage = MOD(ibpage+1,2)
      end if
CC      read data/control from user keyboard and update screen menu.
      if (numsc .gt. 0.and.iswopt(30) .eq. 0) then
        call RESMS(2)
        if (iflrjs .eq. 0) then
          ioscs = INCRT(numsc,iline,itabs,ispac,iferr,lstrw,icont)
        else
          ioscs = JNCRT(numsc,iline,itabs,ispac,iferr,lstrw,icont)
        end if
        call RESMS(1)
      end if
CC
CC      if 'quit' store current screen on page 2 so it may be recovered
CC
CC      do not save current screen if ioscs=1 and screen number is 2, 3
CC      or 23.  These screens have highlighted areas and the leading
CC      @ and trailing ~ indicating highlight get stripped off.  Also,
CC      do not save if ioscs = -1, because some of the screen data has
CC      been lost.
CC
      if (ioscs .eq. 1.and.numsc .ne. 2.and.numsc .ne. 3.and.
     *    numsc .ne. 23.and.ifctlc .ge. 0) then
        istr = 1
CC        turn cursor off.
        imode = 3
        call CRTYP(imode)
        jspage = ispage
        do 20 icol=0,icolmx-1
        do 20 irow=0,irowmx-1
          ispage = jspage
          call QCMOV(icol,irow)
          call QRTXT(icha,ibac,ifor)
          ispage = 2
          call QCMOV(icol,irow)
          call QSTXT(icha,ibac,ifor,ione)
   20   continue
        ispage = jspage
CC        turn cursor off.
        imode = 1
        call CRTYP(imode)
      else
        istr = 0
      end if
      ifctlc = 0
      return
      end
CCPCITE
