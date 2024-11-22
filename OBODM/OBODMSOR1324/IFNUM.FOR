      Function IFNUM(jmp,mans,ie,is,js,ip,kk)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IFNUM Version 01.0                          ---
CC--- Subroutine description - determine if a character is compat-  ---
CC---                          ible with numeric value.             ---
CC---------------------------------------------------------------------
CC
CC   routine arguments.
CC        jmp    - what to check for flag.
CC                 1 = start of numeric check, ignore exponent form
CC                     check.
CC                 2 = continue numeric check, include exponent form
CC                     check.
CC                 3 = check if numeric (0 to 9) only.
CC        mans   - present character to examine.
CC      note- if jmp = 3, ie,is,js,ip and kk are not referenced.
CC        ie     - exponent form flag.
CC                 0 - no exponent form found.
CC                 1 - number is exponent form.
CC        is     - sign flag.
CC                 0 - number is not signed.
CC                 1 - number is signed (-1 means implied)
CC        js     - sign exponent.
CC                 0 - exponent is not signed.
CC        ip     - decimal point flag.
CC                 0 - no decimal point.
CC                 1 - decimal point found.
CC        kk     - count of number of characters already
CC                 accumulated for current numeric value.
CC
CC   returns
CC        IFNUM  - status of numeric check.
CC                 0 = non compatible with numeric value.
CC                 1 = exponent form detected, 'e', 'g', 'd'.
CC                 2 = sign found, '-' or '+'.
CC                 3 = number found, '0' to '9'.
CC                 4 = period found, '.'.
CC                 5 = exponent sign found, '-' or '+'.
CC
CC
CC**********************************************************************
CC
      integer*2 i
CC
      character*1 mans,mperd,mzero,mnine,minus,mplus
CC
      data mperd/'.'/,mzero/'0'/,mnine/'9'/,mplus/'+'/,minus/'-'/
CC
CC**********************************************************************
CC
      if (jmp .ne. 3) then
        if (jmp .ne. 1) then
CC          continuation of number, check for exponent form.
          if (IFCHR(mans,4) .ne. 0.or.IFCHR(mans,5) .ne. 0.or.
     *        IFCHR(mans,7) .ne. 0) then
            if (ie .eq. 0) then
              i = 1
              if (ip .ne. 0) i = i + 1
              if (is .gt. 0) i = i + 1
              if (kk .ge. i) then
CC                set exponent form flag.
                ie = 1
CC                set exponent indicator to e.
                i = IFCHR(mans,-5)
                IFNUM = 1
              else
CC                error, no number prior to exponent indicator.
                IFNUM = 0
              end if
            else
CC              error, exponent indicator already found.
              IFNUM = 0
            end if
            return
          end if
        end if
CC        check for period
        if (mans .eq. mperd) then
          if (ip .eq. 0) then
CC            set period flag.
            ip = 1
            IFNUM = 4
          else
CC            error, period already found.
            IFNUM = 0
          end if
          return
        end if
CC        check for sign of number or exponent.
        if (mans .eq. minus.or.mans .eq. mplus) then
          if (ie .eq. 0) then
            if (kk .eq. 0) then
              if (is .eq. 0) then
CC                set number is signed flag.
                is = 1
                IFNUM = 2
              else
CC                error, sign of number already found.
                IFNUM = 0
              end if
            else
CC              error, sign is not at start of number.
              IFNUM = 0
            end if
          else
CC            sign of exponent.
            if (js .eq. 0) then
CC              set exponent is signed flag.
              js = 1
              IFNUM = 5
            else
CC              error, exponent sign already found.
              IFNUM = 0
            end if
          end if
          return
        end if
      end if
CC      check for number 0 to 9.
      if (mans .ge. mzero.and.mans .le. mnine) then
        IFNUM = 3
      else
CC        error, not a number.
        IFNUM = 0
      end if
      return
      end
