CCPCITS
      Function JNCRT(numsc,iline,itabs,ispac,iferr,lstrw,icont)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - JNCRT Version 01.1                          ---
CC--- Subroutine description - reads data and control characters    ---
CC---                          typed at the user key board. using   ---
CC---                          left justification edit.             ---
CC---------------------------------------------------------------------
CC
CC    numsc - screen menu number.
CC    iline - array giving the line numbers for lines in mscrnm that
CC            contain data fields. if iline(k) > 0, then the line
CC            starting at mscrnm(1,iline(k)) contains data fields,
CC            where k = 1 to 20 (maximum number of lines that can
CC            contain data fields = 20).
CC    itabs - array giving the start columns of each data field on the
CC            line given by iline(k). the data field starts at
CC            mscrnm(itabs(k,l),iline(k)) for the l'th field on the line,
CC            where l = 1 to 10 (maximum number of data fields per
CC            line = 10).
CC    ispac - array giving the number of columns in each data field.
CC            the l'th data field ends at mscrnm(itabs(k,l)+ispac(k,l)-1,
CC            iline(k))
CC    iferr - array indicating if any data fields are in error.
CC    lstrw - returned last row number processed.
CC    icont - line numbers of the start and end of data fields that
CC            continue over lines.  the start line number is in the
CC            10's position and the end line number is in the units
CC            position of the value.  zero indicates no continued lines.
CC
CC    data entered are placed in mscrnm in fields defined by iline,
CC    itabs and ispac.
CC
CC*********************************************************************
CC
      character*1 mblnk
      integer*2 iext,ibac,ifor,irow,icol,icha,kbac,kfor,jcol,
     *          jrow,lrow,ione,jcha,iflg,i,j,l,n,jmx,iblnk,ins,ii,
     *          ix,iy,kstat,numbr,numsv,nmoff,icols,icole
CC
      dimension iline(20),itabs(20,10),ispac(20,10),iferr(20,10)
CC
      include 'OCNTRL.INC'
CC
      save kncrt
CC
      data ione/1/,mblnk/' '/,nmoff/3/
CC
CC*********************************************************************
CC
      ibac = ibackd
      ifor = ifored
      iblnk = ICHAR(mblnk)
      ibflg = 0
      numbr = -1
      numsv = -1
      jcsav = -1
      JNCRT = -1
CC      see if any data in menu.
      iflg = 0
      i = 0
      j = 1
      if (iline(1) .gt. 0) then
CC        set data flag.
        iflg = 1
CC        find last line.
        l = j
CC        last line.
        jmx = j
   10   l = l + 1
        if (l .le. nmxlin) then
          if (iline(l) .gt. 0) then
            jmx = l
            go to 10
          end if
        end if
      end if
      if (iflg .eq. 0) then
        irow = nmxrow - 1
        icol = 0
        ins = 3
        go to 100
      end if
CC      see if any multiple line fields.
      if (icont .gt. 0) then
        icons = IVGET(icont,2)
        icone = IVGET(icont,1)
        if (icons .le. 0) icone = 0
        if (icone .le. 0) icons = 0
      else
        icons = 0
        icone = 0
      end if
      if (IFWER(iferr,nmxlin*nmxfld) .eq. 1) then
        do 30 n=1,jmx
          if (iline(n) .gt. 0) then
            do 20 l=1,nmxfld
              if (iferr(n,l) .eq. 1.and.itabs(n,l) .gt. 0) then
                j = n
                i = l
                go to 90
              end if
   20       continue
          end if
   30   continue
      end if
CC      next field on current row.
   40 i = i + 1
      if (i .gt. nmxfld) then
        i = nmxfld
CC        go to next row.
        go to 60
      end if
      if (itabs(j,i) .eq. 0) then
        i = i - 1
        go to 60
      end if
      go to 90
CC      previous field on current row.
   50 i = i - 1
      if (i .lt. 1) then
        i = 1
        go to 70
      end if
      go to 90
CC      next row down.
   60 j = j + 1
      if (j .gt. jmx) then
        call IBELL(1)
        j = jmx
        go to 90
      end if
      if (iline(j) .le. 0) then
        j = j - 1
        go to 90
      end if
      if (itabs(j,i) .le. 0.or.ii .eq. 4) i = 1
      go to 90
CC      next row up.
   70 j = j - 1
      if (j .lt. 1) then
        call IBELL(1)
        j = 1
        go to 90
      end if
      if (itabs(j,i) .le. 0.or.ii .eq. 3) then
        i = 1
        if (ii .eq. 3) then
          i = 0
   80     i = i + 1
          if (i .le. nmxfld) then
            if (itabs(j,i) .gt. 0) go to 80
          end if
          i = i - 1
        end if
      end if
CC      set row and column indices.
   90 icols = itabs(j,i) - 1
      icole = icols + ispac(j,i) - 1
      irow = nmxrow - iline(j)
      icol = icols
CC      turn insert on.
      ins = 1
CC      if one column field, turn insert off.
      if (icols .eq. icole) ins = 0
CC      display correct cursor
  100 continue
      if (jcsav .ge. 0) then
        icol = jcsav
        irow = jrsav
        jcsav = -1
      end if
      if (numsv .ge. 0) then
CC        turn cursor off
        call CRTYP(nmoff)
      else
CC        turn cursor on, 0=partial, 1=underscore
        call CRTYP(ins)
      end if
CC      move to current character position.
  110 call QCMOV(icol,irow)
      lstrw = nmxrow - irow
CC      get user response key (character).
      call RDALL(iext,icha,ix,iy,icols,icole,irow)
CC      check for control that ends menu input.
      JNCRT = JFCTL(iext,icha,numsc)
CC
CC      if previous entry indicates a command bar command, set JNCRT to
CC      that command if 'enter' is encountered.  if another command is
CC      entered, override previous command.  ibflg remains set for
CC      JNCRT = -1 to allow for further processing of user input.  in
CC      all cases, command bar is redisplayed using original field
CC      status for the screen number.
      if (ibflg .gt. 0) then
        icmsta(numbr) = kstat
        call PRBAR
        if (JNCRT .ge. 0) ibflg = 0
        if (JNCRT .eq. 0) JNCRT = icmmap(numbr)
      end if
CC      if mouse double click.
      if (JNCRT .eq. 13) then
        if (IFFLD(ix,iy,iline,itabs,ispac,ifld,jfld) .eq. 1) then
          icol = ix
          irow = nmxrow - (iy + 1)
          lstrw = nmxrow - irow
          JNCRT = 0
        else
          JNCRT = INBAR(ix,iy,numbr)
CC          repeat command
          if (numbr .eq. 0) JNCRT = kncrt
          if (JNCRT .lt. 0) then
            call IBELL(1)
            ibflg = 0
            numsv = -1
            go to 100
          end if
        end if
      end if
      if (JNCRT .ge. 0) go to 260
CC      check for control that points to another data field.
      ii = KFCTL(iext,icha)
CC
CC      skip command bar processing if ii > 0, not single click (ii=15)
CC      and command bar flag is not set
CC
      if (ibflg .le. 0.and.ii .ge. 0.and.ii .ne. 15) go to 120
CC
CC    if previous entry was from command bar and has not been converted,
CC    ignore entry if it is a position command other than mouse single
CC    click.
      if (ibflg .gt. 0.and.ii .ge. 0.and.ii .ne. 15) then
        ibflg = 0
        numsv = -1
        go to 100
      end if
CC
CC      if single click, set flag so it is reprocessed
CC
      if (ii .eq. 15) ibflg = 0
CC
CC        check for 'alt' key input
CC
      numbr = LFCTL(iext,icha)
      if (numbr .gt. 0) then
        if (icmsta(numbr) .eq. 0) then
          ibflg = 0
          numbr = -1
          numsv = -1
          go to 100
        end if
      end if
CC
CC      if command bar flag set and input not a valid 'alt' key, ignore
CC
      if (ibflg .gt. 0.and.numbr .le. 0) then
        ibflg = 0
        numbr = -1
        numsv = -1
        go to 100
      end if
CC
CC        if valid 'alt' key, redisplay bar and return for additional
CC        input if bar command is the same as previous chosen, option
CC        is unselected.
      if (numbr .gt. 0) then
        if (numbr .eq. numsv) then
          numsv = -1
          ibflg = 0
          go to 100
        end if
        numsv = numbr
        kstat = icmsta(numbr)
        icmsta(numbr) = 3
        ibflg = 1
        call PRBAR
        go to 100
      end if
  120 continue
CC
CC      check if non-standard character
CC
      if (ii .lt. 0) then
        if (iext .eq. 1.and.(icha .lt. 32.or.icha .gt. 126)) then
          numsv = -1
          go to 110
        end if
        if (iext .eq. 0.and.(icha .ge. 32.or.icha .le. 126)) then
          numsv = -1
          go to 110
        end if
      end if
CC
CC      do not allow left or right bracket
CC
      if (ii .lt. 0.and.(icha .eq. 91.or.icha .eq. 93)) then
        numsv = -1
        go to 110
      end if
CC      if up arrow.
      if (ii .eq. 1) then
        go to 70
CC      if down arrow.
      else if (ii .eq. 2) then
        go to 60
CC      if tab left.
      else if (ii .eq. 3) then
        go to 50
CC      if tab right.
      else if (ii .eq. 4) then
        go to 40
CC      if left arrow.
      else if (ii .eq. 5) then
        if (icol-1 .lt. icols) go to 50
        icol = icol - 1
        go to 100
CC      if right arrow.
      else if (ii .eq. 6) then
        if (icol+1 .gt. icole) go to 40
        icol = icol + 1
        go to 100
CC      if delete.
      else if (ii .eq. 7) then
        call CRTYP(nmoff)
        if (icol .lt. icole) then
          do 130 l=icol+1,icole
CC            move characters icol+1 through icole left 1.
            jcol = l
            call QCMOV(jcol,irow)
            call QRTXT(icha,kbac,kfor)
            jcol = l - 1
            call QCMOV(jcol,irow)
            call QSTXT(icha,ibac,ifor,ione)
  130     continue
        end if
CC        set last character in data field to blank.
        icha = iblnk
        jcol = icole
        call QCMOV(jcol,irow)
        call QSTXT(icha,ibac,ifor,ione)
        jflg = 0
CC        see if line is part of multiple line data field.
        if (j .ge. icons.and.j .lt. icone) then
          lrow = irow
          lcole = icole
          jp = j
  150     jp = jp + 1
          if (jp .le. icone) then
            jcols = itabs(jp,i) - 1
            jcole = jcols + ispac(jp,i) - 1
            jrow = nmxrow - iline(jp)
            jcol = jcols
CC            get character at beginning of field and store at end of
CC            field on previous line.
            call QCMOV(jcol,jrow)
            call QRTXT(icha,kbac,kfor)
            jcol = lcole
            call QCMOV(jcol,lrow)
            call QSTXT(icha,ibac,ifor,ione)
            if (icha .ne. iblnk) jflg = 1
CC            shift all characters in field left one position.
            do 160 l=jcols+1,jcole
              jcol = l
              call QCMOV(jcol,jrow)
              call QRTXT(icha,kbac,kfor)
              jcol = jcol - 1
              call QCMOV(jcol,jrow)
              call QSTXT(icha,ibac,ifor,ione)
              if (icha .ne. iblnk) jflg = 1
  160       continue
            lcole = jcole
            lrow = jrow
            if (jp .eq. icone) then
              jcol = jcole
              call QCMOV(jcol,jrow)
              icha = iblnk
              call QSTXT(icha,ibac,ifor,ione)
            end if
            go to 150
          end if
        end if
CC        if all of data field is blank, turn insert on.
        if (jflg .eq. 0.and.icol .eq. icole.and.icols .ne. icole)
     *      ins = 1
        go to 100
CC        if insert.
      else if (ii .eq. 8) then
        if (ins .eq. 0) then
          ins = 1
        else
          ins = 0
        end if
        go to 100
CC      if ctrl k or k
      else if (ii .eq. 9) then
        icha = iblnk
        do 170 l=icols,icole
          jcol = l
          call QCMOV(jcol,irow)
          call QSTXT(icha,ibac,ifor,ione)
  170   continue
        go to 90
CC      if backspace.
      else if (ii .eq. 10) then
        call CRTYP(nmoff)
        jflg = 0
        if (icons .eq. 0) then
          if (icol .gt. icols) jflg = 1
        else
          if (icol .gt. icols.or.j .gt. icons) jflg =1
        end if
        if (jflg .ne. 0) then
CC          shift characters one position to left.
          jflg = -1
          do 180 l=icol,icole
            jcol = l
            call QCMOV(jcol,irow)
            call QRTXT(icha,kbac,kfor)
            jcol = l - 1
            jrow = irow
            if (jcol .lt. icols) then
              jcols = itabs(j-1,i) - 1
              jcole = jcols + ispac(j-1,i) - 1
              jrow = nmxrow - iline(j-1)
              jcol = jcole
              jcsav = jcole
              jrsav = jrow
              jflg = j - 1
            end if
            call QCMOV(jcol,jrow)
            call QSTXT(icha,ibac,ifor,ione)
  180     continue
          icha = iblnk
          jcol = icole
          call QCMOV(jcol,irow)
          call QSTXT(icha,ibac,ifor,ione)
          icol = icol -1
          if (icons .gt. 0.and.j .ge. icons.and.j .lt. icone) then
            lcole = icole
            lrow = irow
            jp = j
  190       jp = jp + 1
            if (jp .le. icone) then
              jcols = itabs(jp,i) - 1
              jcole = jcols + ispac(jp,i) - 1
              jrow = nmxrow - iline(jp)
              jcol = jcols
CC              move first character of this row to last of previous
              call QCMOV(jcol,jrow)
              call QRTXT(icha,kbac,kfor)
              jcol = lcole
              call QCMOV(jcol,lrow)
              call QSTXT(icha,ibac,ifor,ione)
CC              shift characters one position to the left
              do 200 l=jcols,jcole-1
                jcol = l + 1
                call QCMOV(jcol,jrow)
                call QRTXT(icha,kbac,kfor)
                jcol = l
                call QCMOV(jcol,jrow)
                call QSTXT(icha,ibac,ifor,ione)
  200         continue
              lcole = jcole
              lrow = jrow
              go to 190
            end if
            icha = iblnk
            jcol = jcole
            call QCMOV(jcol,jrow)
            call QSTXT(icha,ibac,ifor,ione)
          end if
          if (jflg .lt. 0) then
            go to 100
          else
            j = jflg
            go to 90
          end if
        else
          call IBELL(1)
        end if
        go to 100
CC        if home.
      else if (ii .eq. 11) then
        icol = icols
        go to 100
CC      if end.
      else if (ii .eq. 12) then
        icol = icole
        go to 100
CC      if ctrl home.
      else if (ii .eq. 13) then
        i = 1
        j = 1
        go to 90
CC      if ctrl end.
      else if (ii .eq. 14) then
        j = jmx
        i = nmxfld
  210   if (itabs(j,i) .eq. 0.and.i .gt. 1) then
          i = i - 1
          go to 210
        end if
        go to 90
CC      if single mouse click.
      else if (ii .eq. 15) then
        if (IFFLD(ix,iy,iline,itabs,ispac,ifld,jfld) .eq. 1) then
          numsv = -1
          if (numsc .eq. 3) then
            icol = ix
            irow = nmxrow - (iy + 1)
            lstrw = nmxrow - irow
            JNCRT = 0
            go to 260
          end if
          icol = ix
          irow = nmxrow - (iy+1)
          i = ifld
          j = jfld
          icols = itabs(j,i) - 1
          icole = icols + ispac(j,i) - 1
          ins = 1
          if (icols .eq. icole) ins = 0
          go to 100
        end if
CC
CC          check if single click is in valid command bar
CC          if bar command is the same as previous chosen option is
CC          unselected
        knctl = INBAR(ix,iy,numbr)
        if(knctl .ge. 0) then
          if (numbr .eq. numsv) then
            numsv = -1
            go to 100
          end if
          numsv = numbr
          kstat = icmsta(numbr)
          icmsta(numbr) = 3
          ibflg = 2
          call PRBAR
          if (knctl .ge. 0) then
            JNCRT = knctl
            go to 260
          end if
          go to 100
        end if
        call IBELL(1)
        ibflg = 0
        numsv = -1
        go to 100
        end if
        if (ins .gt. 0) then
CC        insert character.
        jtmp = j
        if (jtmp .ge. icons.and.jtmp .le. icone) jtmp = icone
        jcol = itabs(jtmp,i) + ispac(jtmp,i) - 2
        jrow = nmxrow - iline(jtmp)
        call CRTYP(nmoff)
        call QCMOV(jcol,jrow)
        call QRTXT(jcha,kbac,kfor)
        if (jcha .ne. iblnk) then
          call IBELL(1)
          ins = 0
          go to 100
        end if
        if (j .ge. icons.and.j .lt. icone) then
          lcols = 0
          jp = icone + 1
  220     jp = jp - 1
          if (jp .ge. j) then
            jcols = itabs(jp,i) - 1
            jcole = jcols + ispac(jp,i) - 1
            jrow = nmxrow - iline(jp)
            if (lcols .gt. 0) then
              jcol = jcole
              call QCMOV(jcol,jrow)
              call QRTXT(jcha,kbac,kfor)
              jcol = lcols
              call QCMOV(jcol,lrow)
              call QSTXT(jcha,ibac,ifor,ione)
              if (jp .eq. j) go to 220
            end if
            k = jcole
CC            shift characters one position to right.
            do 230 l=jcols,jcole-1
              k = k - 1
              jcol = k
              call QCMOV(jcol,jrow)
              call QRTXT(jcha,kbac,kfor)
              jcol = k + 1
              call QCMOV(jcol,jrow)
              call QSTXT(jcha,ibac,ifor,ione)
  230       continue
            lcols = jcols
            lrow = jrow
            go to 220
          end if
        end if
CC        shift characters one position to right.
        k = icole
        do 240 l=icol,icole-1
          k = k - 1
          jcol = k
          call QCMOV(jcol,irow)
          call QRTXT(jcha,kbac,kfor)
          jcol = k + 1
          call QCMOV(jcol,irow)
          call QSTXT(jcha,ibac,ifor,ione)
  240   continue
        call QCMOV(icol,irow)
        call QSTXT(icha,ibac,ifor,ione)
        icol = icol + 1
        if (icol .gt. icole) icol = icole
      else
CC        overtype.
        call QCMOV(icol,irow)
        call QSTXT(icha,ibac,ifor,ione)
        icol = icol + 1
        if (icol .gt. icole) icol = icole
      end if
      go to 100
  260 kncrt = JNCRT
      return
      end
CCPCITE
