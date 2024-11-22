      Function IFSRD(mbufr,nbufr,maray,raray,imxch,imxwd,nchfr,xinit,
     *               iskfm,rmisd)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IFSRD Version 01.2                          --- 030797
CC--- Subroutine description - read data from buffer in free field  ---
CC---                          format.                              ---
CC---------------------------------------------------------------------
CC
CC   arguments -
CC       mbufr - input buffer array.
CC       nbufr - length in characters of array mbufr.
CC       maray - character*1 array used for the return of character
CC               input data.  blanks and commas in the input buffer
CC               are assumed part of a name or label if they are
CC               contained in the nchfr frame length.
CC       raray - real array used to return numeric data values.
CC       imxch - maximum number of characters to read from the input
CC               buffer.  if zero, no alpha data are read.
CC       imxwd - maximum number of numeric values to read from input
CC               buffer.  if zero, no numeric data are read from the
CC               input buffer.
CC       nchfr - variable or constant maximum number of characters per
CC               input frame or field. this value is used to delineate
CC               input values when a blank or comma delineator are not
CC               used. for single character input options, use 1. for
CC               names or labels, use the length of the name or label.
CC               for numeric, use the number of characters displayed in
CC               the menu frame. if entered as a negative value the
CC               arrays maray and raray are initialized to blank
CC               and xinit respectively.
CC       xinit - variable or constant initializing value for the array
CC               raray, if nchfr is entered as a negative value.
CC       iskfm - number of characters prior to the first label field
CC               or frame. when the buffer counter n exceeds iskfm,
CC               the frame or field counter m is started.
CC       rmisd - value used to set real data to missing if an 'x'
CC               is encountered in the input data frame or field.
CC
CC   function returns
CC       IFSRD  - i/o status flag.
CC                0 = no errors, no warnings, no data.                  030797
CC                    imxch and imxwd are returned as zero.
CC                1 = no errors, imxch = number of characters returned  030797
CC                    in maray and imxwd = number of words returned
CC                    in raray.
CC               <0 = FORTRAN error number.                             030797
CC
CC**********************************************************************
CC
      integer*2 nwrds,nchrs,numcr,kchfr,idln,i,itype,m,n,jsve,iskp
CC
      character*1 mbufr,mblnk,mbfs1,maray,mans,mcoma,mperd,mslsh
      character*20 mbfsv
CC
      dimension mbufr(nbufr),maray(nbufr),raray(nbufr),mbfs1(100)
CC
      equivalence (mbfs1(1),mbfsv)
CC
      data mcoma/','/,mperd/'.'/,mslsh/'/'/,nmxs1/100/,mblnk/' '/
CC
CC**********************************************************************
CC
CC      initialize parameters.
CC      number of numeric values returned.
      nwrds = 0
CC      number of characters returned.
      nchrs = 0
CC      number of non-blank leading characters in mbufr.
      numcr = 0
CC      character counter in mbufr.
      n = 0
CC      frame or field length.
      kchfr = IABS(nchfr)
CC      read status.
      IFSRD = 0                                                         030797
CC      frame or field status.
      idln = 0
      if (nchfr .lt. 0) then
CC
CC        initialize arrays maray and/ or raray.
        if (imxch .gt. 0) then
CC          initialize with blanks.
          call MBLK1(maray,imxch)
        end if
        if (IABS(imxwd) .gt. 0) then
CC          initialize with xinit.
          i = JRNIT(raray,imxwd,xinit,0)
        end if
      end if
CC
CC      determine number of non-blank characters in buffer.
      numcr = NUMC1(mbufr,nbufr)
CC      see if input buffer is all blanks.
      if (numcr .le. 0) go to 370
CC
CC      interpret input data.
      j = MAX0(20,numcr)
      call MBLK1(mbfs1,j)
CC      frame type flag, 0 = unknown, 1 = alpha, 2 = numeric.
      itype = 0
CC      period flag, 0 = no period, 1 = period found.
      ip = 0
CC      exponent form flag, 0 = no exponent form, 1 = exponent form.
      ie = 0
CC      sign of numeric item flag, 0 = no sign, 1 = signed.
      is = 0
CC      sign of exponent flag, 0 = no sign, 1 = signed.
      js = 0
CC      characters saved per frame counter.
      k = 0
CC      frame character counter relative to buffer.
      m = 0
CC      total character counter.
      n = 0
CC
CC      begin loop over input buffer with increment n.
   10 n = n + 1
CC      if buffer count exceeded.
      idln = 0
      if (n .gt. numcr) go to 130
      mans = mbufr(n)
CC      increment frame counter.
      if (n .gt. iskfm) m = m + 1
CC      is frame count <= maximum.
   20 if (m .gt. kchfr) then
CC        frame count exceeded.
        idln = 1
        if (itype .ne. 2.or.kchfr .eq. 1.or.k .eq. 0) go to 130
CC        check to see if numeric value spans the frame boundary.
        jmp = IFNUM(2,mans,ie,is,ij,ip,k)
        if (jmp .eq. 0) go to 130
CC        assume number spans frame boundary.
        go to (30,40,90,60,50),jmp
      end if
CC      check for delineator.
CC      if blank.
      idln = 2
      if (mans .eq. mblnk) go to 100
CC      if comma.
      idln = 3
      if (mans .eq. mcoma) go to 110
CC      if slash.
      idln = 4
      if (mans .eq. mslsh) go to 120
CC
CC      has type of frame been determined.
CC      if alpha.
      if (itype .eq. 1) go to 90
CC      if numeric.
      jmp = 2
      if (itype .ne. 2) then
CC        frame type is unknown, determine type.
        itype = 1
CC        if numeric not desired, treat as alpha.
        if (imxwd .eq. 0) go to 90
        itype = 2
        if (imxch .gt. 0) then
CC          numeric, alpha mixed.
CC          if alphabet.
          itype = 1
          if (IFALF(mans) .gt. 0) go to 70
        end if
        itype = 2
        jmp = 1
      end if
CC      if numeric.
      j = IFNUM(jmp,mans,ie,is,js,ip,k)
      if (j .eq. 0) then
        if (jmp .eq. 1) then
CC          numeric item not found. if imxch > 0, then assume alpha.
          itype = 1
          if (imxch .gt. 0) go to 90
          go to 340
        else
CC          numeric item ended with non-numeric character.
          j = 0
          idln = 5
          if (imxch .gt. 0.and.nchrs .lt. imxch) then
            idln = 3
            n = n-1
            go to 110
          end if
          go to 340
        end if
      end if
CC
CC      numeric type, accumulate number.
      go to (30,40,90,60,50),j
CC      set exponent notation found flag.
   30 ie = 1
      go to 90
CC      set numeric value is signed flag.
   40 is = 1
      go to 90
CC      set exponent signed flag.
   50 js = 1
      go to 90
CC      set period found flag.
   60 ip = 1
      go to 90
   70 if (k .gt. 0.or.n .eq. numcr) go to 90
      go to 90
   80 jsve = j
      if (mbufr(n+1) .ne. mblnk.and.mbufr(n+1) .ne. mcoma) then
        j = IFNUM(1,mbufr(n+1),ie,is,js,ip,k)
        if (j .ne. 2.and.j .ne. 3) go to 90
      end if
      idln = 2
      go to (90,90,90,200,320,90,90,320),jsve
CC
CC      numeric or alpha accumulator.
   90 k = k + 1
      if (k .gt. nmxs1) then
        k = nmxs1
      else
        mbfs1(k) = mans
        if (itype .eq. 2) then
          if (is .eq. 0) is = -1
        end if
      end if
CC      go get next character.
      go to 10
CC
CC      blank character, see if delineator.
CC      if alpha go accumulate.
  100 if (itype .eq. 1) go to 90
CC      if unknown type, ignore.
      if (itype .eq. 0) go to 10
CC      go terminate numeric value.
      go to 130
CC
CC      comma, see if delineator.
CC      if alpha go accumulate.
  110 if (itype .eq. 1.or.imxwd .eq. 0) go to 90
CC      assume numeric delineator.
      go to 130
CC
CC      slash, see if delineator.
CC      if alpha, go accumulate.
  120 if (itype .eq. 1) go to 90
      if (itype .eq. 0.and.imxch .gt. 0) go to 90
CC      assume end of data line
      idln = 0
CC
CC      end of accumulated item. see if information or not.
CC      if no data accumulated, go see if skip value is required.
  130 if (k .eq. 0) go to 220
CC      have accumulated data, see if alpha (1) or numeric (2)
      if (itype .eq. 2) go to 210
CC
CC      alpha. see if single character.
      if (k .eq. 1) go to 160
      j = 0
      do 140 i=1,k
        if (mbfs1(i) .ne. mblnk.and.mbfs1(i) .ne. mcoma) then
          j = j + 1
          if (j .gt. 1) go to 180
        else
          j = 0
        end if
  140 continue
CC      array consists of single characters, ignoring blanks and
CC      commas. strip off blanks and commas.
      j = 0
      do 150 i=1,k
        if (mbfs1(i) .ne. mblnk.and.mbfs1(i) .ne. mcoma) then
          j = j + 1
          mbfs1(j) = mbfs1(i)
        end if
  150 continue
      j = j + 1
      call MBLK1(mbfs1(j),k-j+1)
      k = j - 1
      if (k .gt. 1) go to 180
CC      single character.
  160 jmp = 0
      go to 180
  170 if (imxch .le. 0) go to 200
CC
CC      store alpha characters into maray.
  180 do 190 i=1,k
        if (nchrs .ge. imxch) go to 350
        nchrs = nchrs + 1
        IFSRD = 1                                                       030797
        maray(nchrs) = mbfs1(i)
  190 continue
      IFSRD = 1                                                         030797
      go to 250
CC
CC      'x' set numeric frame to rmisd.
  200 if (nwrds .ge. IABS(imxwd)) go to 360
      nwrds = nwrds + 1
      IFSRD = 1                                                         030797
      itype = 2
      raray(nwrds) = rmisd
      idln = 6
      k = 1
      go to 250
CC
CC      value is numeric, store value in raray.
  210 if (nwrds .ge. IABS(imxwd)) go to 360
      nwrds = nwrds + 1
      if (ip .eq. 0.and.ie .eq. 0) then
CC        if no decimal point, add period.
        k = k + 1
        if (k .gt. nmxs1) then
          k = nmxs1
        else
          mbfs1(k) = mperd
        end if
      end if
      ier = 0
      i = LRJST(mbfsv,20,0)
      READ (mbfsv,9000,iostat=ier,err=330) rvalue
      raray(nwrds) = rvalue
      IFSRD = 1                                                         030797
      go to 250
CC
CC      no characters accumulated. see if skip a frame.
  220 if (idln .eq. 0) go to 370
      go to (230,250,240,250,250),idln
CC      frame count exceeded.
  230 iskp = 0
      if (imxch .gt. 0) go to 250
      if ((n - iskfm + kchfr - 2) / kchfr .lt. nwrds + 1) go to 250
      iskp = 1
CC      skip frame.
  240 if (nwrds .ge. IABS(imxwd)) go to 360
      nwrds = nwrds + 1
CC
CC      end of frame. reinitialize for next frame.
  250 if (idln .eq. 0) go to 370
      go to (260,270,310,370,300,270),idln
CC      frame exceeded.
  260 j = 0
      if (mans .eq. mcoma) go to 290
      if (mans .ne. mblnk) go to 310
CC      delineated by blank or frame exceeded, scan for comma.
  270 j = 0
  280 j = j + 1
      if (n + j .gt. numcr) go to 370
      if (mbufr(n+j) .eq. mblnk) go to 280
      if (mbufr(n+j) .ne. mcoma) go to 310
  290 if (imxch .eq. 0.and.k .le. 0) then
        if (iskp .eq. 0) go to 310
      end if
      mbufr(n+j) = mblnk
      go to 310
CC      delineated by character, move pointers back.
  300 n = n - 1
      if (n - 1 .gt. iskfm) m = m - 1
CC      reset counters for next frame.
  310 if (n .gt. iskfm.and.idln .eq. 1) then
        n = n - 1
        m = 0
      end if
      iskp = 0
      ip = 0
      ie = 0
      is = 0
      js = 0
      itype = 0
      if (k .eq. 0) go to 10
      j = MAX0(20,k)
      call MBLK1(mbfs1,j)
      k = 0
      go to 10
CC
CC      '?' entered.
  320 mans = mblnk
      if (IFSRD .eq. 0) IFSRD = -6205                                   030797
      if (n .ge. numcr) go to 370
      iskp = 1
      go to 270
CC
CC      read error.
  330 nwrds = nwrds - 1
      IFSRD = -ier                                                      030797
      go to 370                                                         030797
  340 IFSRD = -6205                                                     030797
      go to 370
CC
CC      max. characters exceeded, warning.
  350 if (imxch .gt. 0) IFSRD = 1                                       030797
      go to 370
CC
CC      max. numeric values exceeded, warning.
  360 if (imxwd .ge. 0) then
        if (IABS(imxwd) .gt. 0) IFSRD = 1                               030797
      end if
  370 imxch = nchrs
      imxwd = nwrds
      return
CC
 9000 format (f20.0)
      end
