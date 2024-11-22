CC-OBODM utility related routines
      Function KERRS(inerr,mflnm,idum1,idum2,xdum1,xdum2,xdum3,xdum4,
     *               ifprt)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - KERRS version 01.2                          ---
CC--- Subroutine description - print warnings and errors.           ---
CC---------------------------------------------------------------------
CC
CC    inerr - error number (if negative message is placed in lower left
CC            corner and shortened as much as possible.)
CC            1 - file read error. error # in idum1, block # in idum2,
CC                file name in mflnm.
CC            2 - file write error. error # in idum1, block # in dum2,
CC                file name in mflnm.
CC            3 - file open error. error # in idum1, file name in
CC                mflnm.
CC            4 - file premature end-of-file. block # in idum1, file
CC                name in mflnm.
CC            5 - install new copy of OBODMENU.OBD. no args.
CC            6 - burn time or burn rate bad. isorc in idum1, taucld in
CC                xdum1.
CC            7 - source burn time not consistent with type.  source #
CC                in idum1, burn time in xdum1, burn rate in xdum2.
CC            8 - data save file has been written. do you wish to
CC                rewrite it?
CC            9 - not enough memory. required memory in idum1, actual
CC                maximum in idum2.
CC           10 - particulate material & complex terrain. no arguments
CC                required.
CC           11 - unable to determine video card. no arguments requ.
CC           12 - source > met. site + mixing depth. receptor elev.
CC                 in xdum1, x coord. in xdum2, y coord. in xdum3,
CC                sum of met. site elev. and mixing depth in xdum4.
CC           13 - unable to calc. particle velocity. particle category
CC                number in idum1.
CC           14 - treat source as volume source. no arguments required.
CC           15 - dispersion may be underestimated. no arguments requ.
CC           16 - missing hour.  month in idum1, day in idum2, year in
CC                xdum1 and hour in xdum2.
CC           17 - hour out of sequence. month in idum1, day in idum2,
CC                year in xdum1, hour in xdum2.
CC           18 - calm wind speed. month in idum1, day in idum2, year
CC                in xdum1, hour in xdum2, wind speed in xdum3.
CC           19 - variable out of range. variable name in mflnm, value
CC                in xdum4, data file block # in idum1, month in idum2,
CC                day in xdum1, year in xdum2 and hour in xdum3.
CC           20 - variable is questionable. value is in xdum1.
CC           21 - variable out of range. value in xdum1.
CC           22 - variable out of range. alpha value in mflnm.
CC           23 - warning data value is questionable in mflnm.
CC           24 - found an old work file. initialize from it? no args.
CC           25 - may not be enough disc room for work file. no args.
CC           26 - do you wish to continue without fuels file? no args.
CC           27 - cannot use terrain data. xdum1 is max, xdum2 is min.
CC           28 - cannot use con., dos., etc. data. xdum1 is max,
CC                xdum2 is min.
CC           29 - warning, data may contain erreous values.  no args.
CC           30 - set temp., press. & density.  no args used.
CC           31 - please wait. no args used.
CC           32 - processing batch file. no args used.
CC           34 - please wait. no args used.
CC           35 - please wait. no args used.
CC           36 - data base file cannot be found. no args used.
CC           37 - Data sections 1 through 4 must be SET
CC                before you can EXECUTE OBODM.
CC           38 - Section 5 (EXECUTE) must be SET before
CC                you can produce GRAPHICS.
CC           39 - Error, value xxxxxxxxxxxx found in date/time. record
CC                number in idum1, value in xdum1.
CC           40 - Press Enter to continue.
CC           41 - Produce hard copy?
CC           42 - Copy to OBD#####.PLT, Please Wait. ##### in idum1.
CC           43 - Copy to LPT#, Please Wait. # in idum1.
CC           44 - Copy to COM#, Please Wait.
CC           45 - Cannot open hard copy plot file. Files OBD00001.PLT
CC                to OBD99999.PLT already exist.
CC           46-  Do you really want to quit OBODM.  no args used.
CC           47-  Execute OBODM?.  no args used.
CC           48-  Error, not enough free disc space.  Program requires
CC                idum additional kilobytes.
CC           49-  Warning, Wind speed units are normally m/s for the
CC                hourly data format being used, double check hourly
CC                data units.  no args used.
CC           50-  Warning, Mixing depth units are normally meters for
CC                the hourly data format being used, check data units.
CC           51-  Warning, the X and/or Y scales are not correct for
CC                plot parameters specified.  Do you want the scales
CC                recalculated?.  no args used.
CC           52-  Do you wish to continue processing?.  no args used.
CC           53-  Do you wish to continue plotting?.  no args used.
CC           54-  Rewind to start of print file?.  no args used.
CC           55-  No data in print file.  no args used.
CC           56-  Are you processing more than one year of met. 
CC                data? no args used.                
CC           57-  Please Wait - Initializing Solution. no args used.  
CC           58-  Please Wait - Processing Solution. no args used.    
CC           59-  Error - Problem with range of hours either 0000-2300    
CC                or 0100-2400, check data. no args used.                                 
CC           60-  Error - longitude, latitude & time zone difference    
CC                are required for daytime only calcs. no args used.                   
CC           61-  Warning - maximum number of data values exceeded. no
CC                args used.
CC    mflnm - file name or alpha string.
CC    idum1 - system i/o error number or integer value to display.
CC    idum2 - file block number or integer value to display.
CC    xdum1 - real value to display.
CC    xdum2 - real valaue to display.
CC    xdum3 - real value to display.
CC    xdum4 - real value to display.
CC    ifprt - print to CRT or output print file.
CC            0 - CRT
CC            1 - print file
CC
CC    returns - KERRS = 0 no errors or problems.
CC                      1 f1 entered, terminate.
CC                      2 f2 or esc entered, go to main menu.
CC                      3 Yes reply entered.
CC                      4 No reply entered.
CC                     <0 value read and retured, use abs value.
CC
CC*********************************************************************
CC
CC
      integer*2 icol,irow,ibak,ifor,iext,icha,ix,iy,ione,iscl,isrw,
     *          ncol,nrow,ityp,jbak,jfor,ishd,ifrs,ibks,icoff,icon,
     *          jcol,jrow,jdum
      character*1 mdumy,mat,map,mdips
      character*22 mbufr,mflnm
      character*40 mdisp,mrepl
CC
      dimension mdisp(5),mrepl(5),ival(8),mdips(40,5)
CC
      include 'OCNTRL.INC'
      include 'ONAMES.INC'
      include 'OWNTRL.INC'
CC
      equivalence (mdisp,mdips(1,1))
CC
      data mrepl/'@                OK [ ]                ~',
     *           '@               N/Y [N]                ~',
     *           '@                 [ ]                  ~',
     *           '@ Cannot open message file OBODMERR.OBD~',
     *           '@ File OBODMERR.OBD is bad, replace it.~'/,
     *     mat/'@'/,map/'~'/,icoff/3/,icon/1/,jdum/-1/
CC
CC**********************************************************************
CC
      jfprt = ifprt
      if (ifbtch .ne. 0) jfprt = 1
      isv29 = ifccok
      ifccok = 0
      KERRS = 0
      nnerr = IABS(inerr)
      if (nnerr .eq. 46.or.nnerr .eq. 53) then
        if (inerr .gt. 0) then
          iflg = 1
        else
          iflg = -1
        end if
      else
        iflg = 0
      end if
      if (ifinit .gt. 0) then
        if (nnerr .ne. 24.and.nnerr .ne. 31) return
      end if
CC      set start column on CRT screen.
      if (inerr .gt. 0.or.iflg .ne. 0) then
        icol = 14
      else
        icol = 0
      end if
      ione = 1
CC      text background color.
      ibak = iforet
CC      text foreground color.
      ifor = ibackt
CC      open error/warning message file.
      ierfil = 17
      ker = 0
      OPEN (unit=ierfil,file=mernam,iostat=ier,err=120,status='old',
     *      access='direct',form='unformatted',recl=40)
CC      error number points to pointer record.
      norec = nnerr
      READ (ierfil,rec=norec,iostat=ier,err=140) ipntr,jrec,ival
CC      number of records in message.
      nrec = jrec / 100
CC      user response code.
CC      0 - no reply
CC      1 - click or return on OK [ ]
CC      2 - N or Y required on N/Y [N]
CC      3 - numeric response required [ ]
CC
      irsp = jrec - nrec * 100
      if (irsp .lt. 0.or.irsp .gt. 3) go to 130
CC      set start record.
      norec = ipntr
CC      read message records.
      do 10 i=1,nrec
        READ (ierfil,rec=norec,iostat=ier,err=140) mdisp(i)
        if (jfprt .eq. 0) then
          mdisp(i)(1:1) = mat
          if (inerr .gt. 0.or.iflg .ne. 0) then
            mdisp(i)(40:40) = map
          end if
        end if
        norec = norec + 1
   10 continue
CC      loop over possible error values.
      do 110 i=1,7
        if (ival(i) .ne. 0) then
          if (ival(i) .gt. 0) then
            is = 1
          else
            is = -1
            ival(i) = IABS(ival(i))
          end if
CC          line number to insert value.
          linen = ival(i) / 100000
          if (linen .lt. 1.or.linen .gt. nrec) go to 130
          j = ival(i) - linen * 100000
CC          start character to insert value.
          jchrs = j / 1000
          if (jchrs .lt. 2.or.jchrs .gt. 39) go to 130
          j = j - jchrs * 1000
CC          number of characters provided for value.
          nchrs = j / 10
          if (jchrs-1+nchrs .gt. 39) go to 130
CC          number of decimal points to print.
          ndec = (j - nchrs * 10) * is
          if (ndec .ge. nchrs) go to 130
CC          which value is it?
          go to (20,30,40,50,60,70,80),i
   20     mbufr = mflnm
          go to 100
   30     xdumy = idum1
          go to 90
   40     xdumy = idum2
          go to 90
   50     xdumy = xdum1
          go to 90
   60     xdumy = xdum2
          go to 90
   70     xdumy = xdum3
          go to 90
   80     xdumy = xdum4
CC          place value in buffer.
   90     j = IWTBF(mbufr,nchrs,xdumy,ndec,0)
CC          left justify buffer.
  100     j = LRJST(mbufr,nchrs,1)
CC          place value buffer in message.
          mdisp(linen)(jchrs:jchrs+nchrs-1) = mbufr(1:nchrs)
        end if
  110 continue
      go to 160
  120 mdisp(1) = mrepl(4)
      go to 150
  130 ier = 1
CC-DEBUGS
CC>   if (ifdbug .gt. 0.and.ifdbug .lt. 7) then                         020797
CC>     write (*,9002) jrec,nrec,irsp,linen,jchrs,nchrs,ndec
CC>     read (*,*) iiii
CC>   end if
CC-DEBUGE
  140 mdisp(1) = mrepl(5)
  150 ker = ier
      nrec = 1
      irsp = 1
  160 continue
CCPCITS
      if ((nnerr .lt. 40.or.nnerr .gt. 45).and.iflg .ge. 0) then
        call RESMS(1)
        call RESMS(0)
      end if
CC      set. start row on CRT screen.
      if (jfprt .eq. 0) then
        irow = 14
        if (nnerr .eq. 24.or.nnerr .eq. 31) irow = irow + 4
        if (nnerr .eq. 32.or.nnerr .eq. 33.or.nnerr .eq. 54.or.
     *      nnerr .eq. 66) irow = irow - 3
        iscl = icol - 1
        ncol = 42
        isrw = irow - nrec
        nrow = nrec + 2
        if (irsp .gt. 0) then
          isrw = isrw - 1
          nrow = nrow + 1
        end if
        ityp = 0
        jbak = ibackd
        jfor = ifored
        ishd = 1
        ifrs = 8
        ibks = 0
        if (inerr .gt. 0.or.iflg .ne. 0) then
          call POPBX(iscl,isrw,ncol,nrow,ityp,ifor,ibak,ishd,ifrs,ibks)
        else
          irow = 1
        end if
      end if
CCPCITE
CC      write message on CRT screen.
      if (inerr .lt. 0) call IBELL(1)
CCPCITS
      call CRTYP(icoff)
CCPCITE
      do 170 i=1,nrec
        j = IPKBK(mdisp(i),39)
        if (inerr .gt. 0.or.iflg .ne. 0) then
          j = ICNTR(mdips(2,i),38)
          k = 40
        else
          k = LRJST(mdips(2,i),39,1) + 2
          if (nnerr .eq. 41) k = k + 1
          mdips(k,i) = map
        end if
        if (jfprt .eq. 0) then
          call IWRSC(icol,irow,k,mdisp(i),ibak,ifor)
          if (inerr .gt. 0.or.iflg .ne. 0) irow = irow - 1
        else
          if (iotfil .gt. 0) then
            WRITE (iotfil,9001) mdisp(i)
          end if
        end if
  170 continue
      if (jfprt .ne. 0.or.ifbtch .ne. 0) go to 220                      030499
CC      write user response prompt to CRT screen.
      if (irsp .eq. 0) go to 220
      if (inerr .gt. 0.or.iflg .ne. 0) then
CCPCITS
        if (irsp .ge. 1.and.irsp .le. 3) then
          call CRTYP(icon)
        end if
CCPCITE
        call IWRSC(icol,irow,40,mrepl(irsp),ibak,ifor)
        icol = 35
      else
        icol = icol + 17
      end if
CC      get user response.
  180 continue
CCPCITS
      if ((nnerr .lt. 40.or.nnerr .gt. 45).and.iflg .ge. 0) then
        call RESMS(2)
      end if
CC      move to current character position.
      jcol = icol
      jrow = irow
      call QCMOV(icol,irow)
CC      get user response key (character).
      call RDALL(iext,icha,ix,iy,jdum,jdum,jdum)
CC      check for control.
      i = JFCTL(iext,icha,4)
      j = KFCTL(iext,icha)
      if (i .ge. 0) then
        if ((i .eq. 0.or.(i .eq. 13.and.icol.eq. ix.and.
     *      irow .eq. nmxrow-(iy+1))).and.irsp .le. 2) then
          go to 190
        else if (i .eq. 1) then
          KERRS = 1
          go to 220
        else if (i .eq. 2.or.i .eq. 3) then
          KERRS = 2
          go to 220
        else
          call IBELL(1)
          icol = jcol
          irow = jrow
          go to 180
        end if
      else if (j .ge. 0) then
        if (j .eq. 15.and.irsp .eq. 2.and.icol-3 .eq. ix.and.
     *      irow .eq. nmxrow-(iy+1)) then
          icha = ichar('Y')
          call QCMOV(icol,irow)
          call QSTXT(icha,jbak,jfor,ione)
          icol = jcol
          irow = jrow
          go to 180
        else if (j .eq. 15.and.irsp .eq. 2.and.icol-5 .eq. ix.and.
     *      irow .eq. nmxrow-(iy+1)) then
          icha = ichar('N')
          call QCMOV(icol,irow)
          call QSTXT(icha,jbak,jfor,ione)
          icol = jcol
          irow = jrow
          go to 180
        else if (j .eq. 15.and.irsp .le. 2.and.icol .eq. ix.and.
     *           irow .eq. nmxrow-(iy+1)) then
          go to 190
        else
          call IBELL(1)
          icol = jcol
          irow = jrow
          go to 180
        end if
      end if
      go to 200
  190 KERRS = 0
      if (irsp .eq. 2) then
        if (nnerr .ne. 41) then
          call QCMOV(icol,irow)
          call QRTXT(icha,icol,irow)
          mdumy = char(icha)
        else
          mdumy = mblnk1
        end if
        if (mdumy .eq. mblnk1) then
          icha = ichar('N')
          mdumy = malpha(14)
          call QCMOV(icol,irow)
          call QSTXT(icha,jbak,jfor,ione)
        end if
      else
        go to 220
      end if
      go to 210
  200 mdumy = char(icha)
      call QCMOV(jcol,jrow)
      call QSTXT(icha,jbak,jfor,ione)
  210 continue
CCPCITE
CCOMP1S
c     mdumy = mblnk1
c     READ (*,9000) mdumy
c     if (mdumy .eq. mblnk1) mdumy = 'n'
CCOMP1E
      if (irsp .eq. 2) then
        if (ifchr(mdumy,14) .ne. 0) then
          KERRS = 4
          go to 220
        else if (ifchr(mdumy,25) .ne. 0) then
          KERRS = 3
          go to 220
        else
          call IBELL(1)
          go to 180
        end if
      else if (irsp .eq. 3) then
        if (mdumy .gt. '0'.and.mdumy .lt. '4') then
          KERRS = ichar('0') - ichar(mdumy)
          go to 220
        else
          call IBELL(1)
          go to 180
        end if
      else
        KERRS = 0
      end if
  220 if (ker .gt. 0) KERRS = 1
      CLOSE (unit=ierfil,status='keep',err=230)
  230 if (KERRS .eq. 1.or.KERRS .eq. 2) then
        iferrs = KERRS
        if (iferrs .eq. 2) ifnext = 3
      end if
      ifccok = isv29
      return
CC
CCOMP1S
c9000 format (a1)
CCOMP1E
 9001 format (a40)
CC-DEBUGS
 9002 format (/' Error - OBODMERR.OBD file parameters are bad?'/
     *' jrec,nrec,irsp,linen,jchrs,nchrs,ndec=',7i5,'  '/
     *' press Enter to continue.')
CC_DEBUGE
      end
