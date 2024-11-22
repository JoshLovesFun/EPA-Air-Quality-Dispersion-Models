      Function IOSCN(numsc,mscrn,iline,itabs,ispac,iferr,lstrw,icont)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IOSCN Version 01.2                          ---
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
CC                >99 - terminate screen graphics mode.  indicates the
CC                      end of OBODM, where other routines actually
CC                      terminate the program and return to system
CC                      control.
CC    mscrn - nmxcol by nmxrow character array containing the screen
CC            menu. the first dimension specifies screen coluumns left
CC            to right, 1 to nmxcol.  the second dimension specifies
CC            screen line number top to bottom, 1 to nmxrow.
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
CC    returns - IOSCN status -
CC               0 - enter key typed, return lstrw.
CC               1 - f1 key typed.
CC               2 - f2 key typed.
CC               3 - f3 key typed.
CC               4 - f4 key typed.
CC               5 - f5 key typed.
CC               6 - f6 key typed.
CC               7 - f7 key typed.
CC               8 - f8 key typed.
CC               9 - f9 key typed.
CC              10 - f10 key typed.
CC              11 - pgup key typed.
CC              12 - pgdn key typed.
CC    icont - line numbers of the start and end of data fields that
CC            continue over lines.  the start line number is in the
CC            10's position and the end line number is in the units
CC            position of the value.  zero indicates no continued lines.
CC
CC*********************************************************************
CC
      integer*2 iback
      character*1 mscrn,mdumy
CC
      dimension mscrn(80,25),iline(20),itabs(20,10),ispac(20,10),
     *          iferr(20,10)
CC
      include 'OCNTRL.INC'
      include 'OWNTRL.INC'
      include 'ONAMES.INC'
CC
CC********************************************************************
CC
      IOSCN = 0
      if (numsc .eq. 0) then
CC        initialize OBODM screen menu control.
        call OBSTR(lmodes,ncolmn,imodes,iboard,ipmode)
CCPCITS
        call SETMS
        idpage = 0
        ibpage = 1
        iback = ibackt
        call CLRPG(idpage,iback)
        call CLRPG(ibpage,iback)
CCPCITE
        if (ipmode .gt. 16) then
          pixelx = 640
          pixely = 480
        else if (ipmode .eq. 16) then
          pixelx = 640.0
          pixely = 350.0
        else if (ipmode .eq. 4.or.ipmode .eq. 5) then
          pixelx = 320.0
          pixely = 200.0
        else
          pixelx = 640.0
          pixely = 200.0
        end if
      else if (numsc .eq. -1.or.numsc .le. 99) then
        if (ifinit .le. 0) then
          if (numsc .gt. 0) then
CCPCITS
            call OBSIO(numsc,mscrn,iline,itabs,ispac,iferr,lstrw,IOSCN,
     *                 icont)
CCPCITE
CCOMP1S
c           call OBSIO(numsc,mscrn,iline,itabs,ispac,iferr,lstrw,IOSCN)
CCOMP1E
            if (IOSCN .eq. 1) then
              ifbtch = 0
              i = KERRS(46,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0)
              if (i .eq. 3) then
                IOSCN = 1
              else
                IOSCN = 10
              end if
            end if
          else
            READ (*,9000) mdumy
            mscrn(1,1) = mdumy
          end if
        else
          IOSCN = 0
        end if
      else if (numsc .ge. 100) then
CC        terminate.
        call OBEND(lmodes)
CCPCITS
        call CLSMS
CCPCITE
CC        set response to terminate OBODM.
        IOSCN = 1
      end if
      return
 9000 format (a1)
      end
