      Subroutine RINPC
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - RINPC Version 01.3                          --- 072497
CC--- Subroutine description - display main menu.                   ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      character*1 mdumy,mscrn,mbufs
      character*4 mno
      character*5 myes
      character*6 mdone,mwarn
      character*20 mesg,mhdg
      character*132 mbufr
      integer*2 ins,nmoff
CC
      dimension mscrn(80,23),mbufs(132),itmp3(6)
CC
      include 'OCNTRL.INC'
      include 'ONAMES.INC'
      include 'OCALCS.INC'
      include 'OWNTRL.INC'
      include 'OCDSPM.INC'
CC
      equivalence (jfnxt,functx(1)),(mbufr,mbufs(1),mscrch(1)),
     *            (mscrn(1,1),mscrnm(1,2))
CC
      save ipwsv
CC
      data myes/'@YES~'/,mno/'@NO~'/,mdone/'@DONE~'/,mwarn/'@WARN~'/,
     *     nmoff/3/,ipwsv/0/,
     *     mesg/'{Ctrl C to interupt}'/,
     *     mhdg/'OBODM (Version xx.x)'/
CC
CC**********************************************************************
CC
      jfnxt = 0
      if (ifcnvt .ne. 0) then
        call GETID(ier)
      end if
      if (ipwsv .gt. 0) iswopt(22) = ipwsv - 1
CC      start screen number.
   10 numsc = numscs(3)
CC      display main menu, determine what to do next.
      numf = IRDSC(numsc,1,mscrnm,ilinem,itabsm,ispacm,iferrm,0)
      ifrtn = 0
CC      see if data sections are 'set', 'none' or 'err'.
      call DTSET(mscrnm,ilinem)
      j = 1
      do 20 i=1,4
        if (ifrdwt(i) .le. 0) j = 0
   20 continue
      nvl = IWHER(5,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
      if (j .eq. 0) then
        call MVCH1(mno,mscrnm(nsc-8,nln),4)
      else
        call MVCH1(myes,mscrnm(nsc-8,nln),5)
      end if
      nvl = IWHER(6,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
      if (ifrdwt(5) .gt. 0) then
        call MVCH1(myes,mscrnm(nsc-8,nln),5)
      else
        call MVCH1(mno,mscrnm(nsc-8,nln),4)
      end if
      nvl = IWHER(8,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
      if (ifrdwt(5) .gt. 0) then
        call MVCH1(myes,mscrnm(nsc-8,nln),5)
      else
        call MVCH1(mno,mscrnm(nsc-8,nln),4)
      end if
      if (ifpsol .eq. 2) then
        call MVCH1(mdone,mscrnm(nsc+6,nln),6)
      else if (ifpsol .eq. 6) then
        call MVCH1(mwarn,mscrnm(nsc+6,nln),6)
      end if
CC      display screen menu.
      iferrs = IOSCN(numsc,mscrnm,ilinem,itabsm,ispacm,iferrm,lstrw,0)
      call DTSET(mscrnm,ilinem)
CC      save screen menu.
      i = IWTSC(numsc,1,mscrnm,ilinem,iferrm)
      if (iferrs .eq. 1) go to 400
      jfers = iferrs
      ifpsol = 0
CC      determine line number.
      ifrtn = -1
      nvl = 0
   30 if (nvl .lt. 9.and.ifrtn .eq. -1) then
        nvl = nvl + 1
        if (lstrw .eq. ilinem(nvl)) then
          ifrtn = nvl
        end if
        go to 30
      end if
      if (ifrtn .lt. 1) ifrtn = 1
CC      check user response.
      if (jfers .eq. 0) then
CC        enter typed, (1)
        if (ifrtn .gt. 0) then
          i = JFJMP(ifrtn)
          if (i .ne. 0) ifrtn = i
        end if
      else if (jfers .eq. 2) then
CC        F2 typed,
        ifrtn = -1
      else if (jfers .ge. 3.and.jfers .le. 7) then
CC        display aux. screens.
        iferrs = IFRSP(jfers,mscrnm,ilinem,itabsm,ispacm,iferrm,0)
CC        if terminate.
        if (iferrs .eq. 1) then
          go to 400
        else
          ifrtn = -1
        end if
      else if (jfers .ge. 8.and.jfers .le. 10) then
CC        n/a.
        ifrtn = -1
      else if (jfers .eq. 11) then
CC        page up, go back to files menu.
        ifnext = 2
        ifrtn = 0
      else if (jfers .eq. 12) then
CC        page down, look for first data area not set.
        if (ifrdwt(2) .ne. 1) then
CC          go to receptor data.
          ifrtn = 1
        else if (ifrdwt(4) .ne. 1) then
CC          go to meteorological data.
          ifrtn = 2
        else if (ifrdwt(3) .ne. 1) then
CC          go to source data.
          ifrtn = 3
        else if (ifrdwt(1) .ne. 1) then
CC          go to control/options data.
          ifrtn = 4
        else if (ifrdwt(5) .ne. 1) then
          i = KERRS(47,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0)
CC          if yes, go execute solution.
          if (i .eq. 3) ifrtn = 5
        else if (ifrdwt(6) .ne. 1) then
CC          graphics
          ifrtn = 6
        else
          ifrtn = -1
        end if
      end if
   40 if (ifrtn .lt. 0.or.ifrtn .gt. 9) then
        call IBELL(1)
        go to 10
      else if (ifrtn .eq. 1) then
CC        go to receptor data.
        ifnext = 4
      else if (ifrtn .eq. 2) then
CC        go to meteorological data.
        ifnext = 6
      else if (ifrtn .eq. 3) then
CC        go to source data.
        ifnext = 5
      else if (ifrtn .eq. 4) then
CC        go to control/options data.
        ifnext = 7
      else if (ifrtn .eq. 5) then
CC        execute specified, see if all data set.
        if (ifrdwt(1) .le. 0.or.ifrdwt(2) .le. 0.or.
     *      ifrdwt(3) .le. 0.or.ifrdwt(4) .le. 0) then
          ifbtch = 0
          if (kerrs(37,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .eq. 1)
     *        go to 400
          go to 10
        end if
CC        all data set, go execute models.
        ifnext = 9
        ascale = 1.0                                                    121098
        bscale = 0.0                                                    121098
CCPCITS
        ispage = ibpage
        call mblk1(mscrnm,2000)
        call MVCH1(mhdg,mscrnm(30,1),20)
        call MVCH1(mesg,mscrnm(3,25),20)
        i = jinit(ilinem,20,0,0)
        i = jinit(itabsm,200,0,0)
        i = jinit(ispacm,200,0,0)
        i = jinit(iferrm,200,0,0)
        call IOCRT(mscrnm,ilinem,itabsm,ispacm,iferrm,ibackt,iforet)
        call STATB(0)
        call PRBAR
        call RESMS(0)
        call DISPG(ispage)
        idpage = ibpage
        ibpage = MOD(ibpage+1,2)
CCPCITE
      else if (ifrtn .eq. 6) then
CC        graphics specified, see if solution/graphic file exists.
        if (ifrdwt(5) .le. 0) then
          if (KERRS(38,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .eq. 1)
     *        go to 400
          go to 10
        end if
        ifnext = 8
      else if (ifrtn .eq. 7) then
CCPCITS
        icl = 1
        if (iswopt(22) .eq. 0) then
          ice = 55
        else
          ice = 1
        end if
        ifhiof = 1
CC        display print file.
CC
CC        turn cursor off
        call CRTYP(nmoff)
CC
        ifrtn = -1
        nrecs = 0
        REWIND (unit=iotfil,err=50)
   50   nrecs = nrecs + 1
        READ (iotfil,9000,err=60,end=60) mdumy
        go to 50
   60   nrecs = nrecs - 1
        if (nrecs .le. 0) then
          i = KERRS(55,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0)
          ifhiof = 0
          REWIND (unit=iotfil,err=10)
          go to 10
        end if
CC        load buffer from file beginning.
   70   REWIND (unit=iotfil,err=80)
   80   irec = 0
CC        blank out buffer.
   90   call MBLK1(mscrnm,2000)
        call MVCH1(mhdg,mscrnm(30,1),20)                                121098
CC        load up to 23 lines.
        nl = 0
  100   if (nl .lt. 23.and.irec .lt. nrecs) then
          nl = nl + 1
          irec = irec + 1
          mbufr = mblnk1
          READ (iotfil,9001,end=110) mbufr
          n = NUMC1(mbufr,132)
          if (n .gt. 0) then
            call MVCH1(mbufs(icl),mscrn(2,nl),78)
          end if
          go to 100
  110     irec = irec -1
          nl = nl - 1
        end if
CC        display buffer.
  120   ispage = ibpage
        call IOCRT(mscrnm,ilinem,itabsm,ispacm,iferrm,ibackt,iforet)
        if (irec .le. 23) then
          call STATB(-2)
        else if (irec .ge. nrecs) then
          call STATB(-3)
        else
          call STATB(-1)
        end if
        call PRBAR
        call RESMS(0)
        call DISPG(ispage)
        idpage = ibpage
        ibpage = MOD(ibpage+1,2)
CC        get user response.
  130   jfers = KNCRT(37)
        if (jfers .eq. 1) then
CC          F1, terminate?
          ifbtch = 0
          i = KERRS(46,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0)
CC          if yes.
          if (i .eq. 3) then
            ifrtn = 9
            go to 250
          end if
CC          redisplay current page.
          n = 0
  140     if (n .lt. 23) then
            n = n + 1
            irec = irec - 1
            BACKSPACE (unit=iotfil,err=70)
            go to 140
          end if
          go to 90
        else if (jfers .eq. 2.or.jfers .eq. 3) then
CC          F2, F3 return to main menu.
          go to 250
        else if (jfers .eq. 4) then
CC          page up
          if (irec-46 .lt. 1) then
CC            already at top.
            call IBELL(1)
            go to 70
          end if
CC          move back to pevious page.
          n = 0
  150     if (n .lt. 46) then
            n = n + 1
            irec = irec - 1
            BACKSPACE (unit=iotfil,err=70)
            go to 150
          end if
          go to 90
        else if (jfers .eq. 5) then
CC          page down
          if (irec .ge. nrecs) then
CC            already at end
            call IBELL(1)
            go to 130
          end if
          go to 90
        else if (jfers .eq. 6) then
CC          down arrow
          if (irec+1 .gt. nrecs) then
CC            already at end
            call IBELL(1)
            go to 130
          end if
CC          move buffer down one line
          if (nl .ge. 23) then
            do 160 i=1,22
              call MVCH1(mscrn(2,i+1),mscrn(2,i),nmxcol-2)
  160       continue
            nl = 23
          else
            nl = nl + 1
          end if
          call MBLK1(mscrn(1,nl),nmxcol)
          irec = irec + 1
          if (irec .le. nrecs) then
            mbufr = mblnk1
            READ (iotfil,9001,end=170) mbufr
            n = NUMC1(mbufr,132)
            if (n .gt. 0) then
              call MVCH1(mbufs(icl),mscrn(2,nl),78)
            end if
            go to 120
          end if
  170     irec = irec - 1
          go to 120
        else if (jfers .eq. 7) then
CC          up arrow
          if (irec-24 .lt. 1) then
CC            already at beginning
            call IBELL(1)
            go to 70
          end if
CC          move up one line
          n = 0
  180     if (n .lt. 24) then
            n = n + 1
            irec = irec - 1
            BACKSPACE (unit=iotfil,err=70)
            go to 180
          end if
          go to 90
        else if (jfers .eq. 8) then
CC          Ctrl home go to beginning
          icl = 1
          go to 70
        else if (jfers .eq. 9) then
CC          Ctrl end
          if (irec .ge. nrecs) then
CC            already at end
            call IBELL(1)
            go to 130
          end if
          icl = 1
CC          go to end of file.
          n = nrecs - 23
  190     if (irec .lt. n) then
            irec = irec + 1
            if (irec .gt. nrecs) go to 200
            READ (iotfil,9000,end=200) mdumy
            go to 190
          else if (irec .gt. n) then
            irec = irec - 1
            BACKSPACE (unit=iotfil,err=70)
            go to 190
          end if
          go to 90
  200     irec = irec -1
          go to 90
        else if (jfers .eq. 10) then
CC          move page left
          if (icl .le. 1) then
            call IBELL(1)
            go to 120
          end if
          icl = icl - 1
CC          redisplay current page.
          n = 0
  210     if (n .lt. 23) then
            n = n + 1
            irec = irec - 1
            BACKSPACE (unit=iotfil,err=70)
            go to 210
          end if
          go to 90
        else if (jfers .eq. 11) then
CC          move page right
          if (icl .ge. ice) then
            call IBELL(1)
            go to 120
          end if
          icl = icl + 1
CC          redisplay current page.
          n = 0
  220     if (n .lt. 23) then
            n = n + 1
            irec = irec - 1
            BACKSPACE (unit=iotfil,err=70)
            go to 220
          end if
          go to 90
        else if (jfers .eq. 12) then
CC          home
          if (icl .le. 1) then
            call IBELL(1)
            go to 120
          end if
          icl = 1
CC          redisplay current page.
          n = 0
  230     if (n .lt. 23) then
            n = n + 1
            irec = irec - 1
            BACKSPACE (unit=iotfil,err=70)
            go to 230
          end if
          go to 90
        else if (jfers .eq. 13) then
CC          end
          if (icl .ge. ice) then
            call IBELL(1)
            go to 120
          end if
          icl = ice
CC          redisplay current page.
          n = 0
  240     if (n .lt. 23) then
            n = n + 1
            irec = irec - 1
            BACKSPACE (unit=iotfil,err=70)
           go to 240
          end if
          go to 90
        end if
CC        end of print file display.
  250   ins = 0
        call CRTYP(ins)
        if (ifrtn .ne. 9) then
          if (KERRS(54,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .eq. 3)
     *        then
CC            position file at start, write over old data.
            REWIND (unit=iotfil,err=280)
            go to 280
          end if
        end if
CC        position file at end of data.
  260   REWIND (unit=iotfil,err=280)
        irec = 0
  270   if (irec .lt. nrecs) then
          irec = irec + 1
          READ (iotfil,9000,end=280) mdumy
          go to 270
        end if
  280   ifhiof = 0
        if (ifrtn .eq. 9) go to 40
CC        return to main menu
        go to 10
CCPCITE
      else if (ifrtn .eq. 8) then
        if (ifrdwt(5) .le. 0) then
          if (KERRS(38,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .eq. 1)
     *        go to 400
          go to 10
        end if
CC
CC        screen 73 or 74 or 78, print options.
  290   if (ifhrly .le. 0) then
          numsc = numscs(14)
        else
          if (ifmode .eq. 0) then
            numsc = numscs(14) + 1
          else
            numsc = numscs(15)
          end if
        end if
        numf = IRDSC(numsc,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
        nvl = 0
        if (ifmode .eq. 0) then
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
            if (ngroup .eq. 0) then
              mscrnm(nsc,nln) = malpha(19)
            else
              mscrnm(nsc,nln) = malpha(7)
            end if
          end if
        end if
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
          if (iswopt(22) .eq. 0) then
            xdumy = 132
          else
            xdumy = 80
          end if
          i = IWTBF(mscrnm(nsc,nln),nch,xdumy,0,iflrjs)
        end if
        if (ifhrly .gt. 0) then
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
            if (iswopt(12) .eq. 0) then
              mscrnm(nsc,nln) = malpha(14)
            else
              mscrnm(nsc,nln) = malpha(25)
            end if
          end if
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
            if (iswopt(18) .eq. 0) then
              mscrnm(nsc,nln) = malpha(14)
            else
              mscrnm(nsc,nln) = malpha(25)
            end if
          end if
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          if (ifmode .ne. 0) then
            l = 2
            do 300 j=1,6
              nsc = JWHER(nvl,itabsm,ispacm,j,nch)
              if (navhrs(j) .gt. 0) then
                xdumy = navhrs(j)
                k = IWTBF(mscrnm(nsc+l,nln-1),nch,xdumy,0,0)
              else
                call MBLK1(mscrnm(nsc+l,nln-1),nch)
              end if
              l = l - 2
  300       continue
          end if
          if (itmp3(1) .ge. 0) then
            do 310 j=1,6
              nsc = JWHER(nvl,itabsm,ispacm,j,nch)
              if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
                go to 330
              end if
  310       continue
            do 320 j=1,6
              nsc = JWHER(nvl,itabsm,ispacm,j,nch)
              if (navhrs(j) .gt. 0) then
                xdumy = navhrs(j)
                k = IWTBF(mscrnm(nsc,nln),nch,xdumy,0,iflrjs)
              end if
  320       continue
          end if
  330     if (ifmode .eq. 0) then
            nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
            if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
              if (iswopt(28) .eq. 0) then
                mscrnm(nsc,nln) = malpha(14)
              else
                mscrnm(nsc,nln) = malpha(25)
              end if
            end if
          end if
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
            if (ifsean .eq. 0) then
              mscrnm(nsc,nln) = malpha(14)
            else if (ifsean .eq. 1) then
              mscrnm(nsc,nln) = malpha(19)
            else
              mscrnm(nsc,nln) = malpha(17)
            end if
          end if
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
            if (ifanul .eq. 0) then
              mscrnm(nsc,nln) = malpha(14)
            else
              mscrnm(nsc,nln) = malpha(1)
            end if
          end if
        end if
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
          call MVCH1(mtitle,mscrnm(nsc,nln),MIN0(nch,50))
        end if
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
          i = IWTBF(mscrnm(nsc,nln),nch,ascale,4,iflrjs)
        end if
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
          i = IWTBF(mscrnm(nsc,nln),nch,bscale,4,iflrjs)
        end if
CC        display screen menu.
        iferrs = IOSCN(numsc,mscrnm,ilinem,itabsm,ispacm,iferrm,
     *                 lstrw,0)
CC        save screen menu.
        i = IWTSC(numsc,1,mscrnm,ilinem,iferrm)
        ker = IFWER(iferrm,nmxlin*nmxfld)
CC        save iferrs.
        jfers = iferrs
        ifwrt = 0
CC
CC        check user response to menu.
        if (jfers .eq. 1) then
CC          terminate.
          go to 400
        else if (jfers .eq. 2) then
CC          main menu.
          go to 10
        else if (jfers .ge. 3.and.jfers .le. 7) then
CC          display aux. menus.
          iferrs = IFRSP(jfers,mscrnm,ilinem,itabsm,ispacm,iferrm,0)
          if (iferrs .eq. 1) then
            go to 400
          else if (iferrs .eq. 2) then
            go to 10
          end if
CC          redisplay menu.
          go to 290
        else if (jfers .ge. 8.and.jfers .le. 10) then
          go to 290
        else if (jfers .eq. 11) then
          go to 10
        end if
CC        turn off error indicators for this menu.
        i = JINIT(iferrm,nmxlin*nmxfld,0,0)
CC
CC        check data.
CC
        nvl = 0
        if (ifmode .eq. 0) then
CC          see if print sum of sources or groups.
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          if (nsourc .gt. 1) then
            if (mscrnm(nsc,nln) .ne. mblnk1) then
              if (IFCHR(mscrnm(nsc,nln),19) .ne. 0) then
                ngroup = 0
              else if (IFCHR(mscrnm(nsc,nln),7) .ne. 0) then
                if (ngroup .le. 0) ngroup = 1
              else
                iferrm(nvl,1) = 1
              end if
            else
              ngroup = 0
            end if
          else
            ngroup = 0
            mscrnm(nsc,nln) = malpha(19)
            ifwrt = 1
          end if
        end if
CC
CC        print page column width.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
          i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
          if (i .gt. 0) then
            iferrm(nvl,1) = 1
          else if (i .lt. 0) then
            iswopt(22) = 1
          else
            if (xdumy .le. 80) then
              iswopt(22) = 1
            else
              iswopt(22) = 0
            end if
          end if
        else
          iswopt(22) = 1
        end if
        ipwsv = iswopt(22) + 1
        if (ifhrly .gt. 0) then
CC
CC          see if print table of highest and second highest.
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          if (mscrnm(nsc,nln) .ne. mblnk1) then
            if (IFCHR(mscrnm(nsc,nln),14) .ne. 0) then
              itmp1 = 0
            else if (IFCHR(mscrnm(nsc,nln),25) .ne. 0) then
              itmp1 = 1
            else
              iferrm(nvl,1) = 1
            end if
          else
            itmp1 = 0
          end if
          if (ifmode .ne. 0.and.iswopt(12) .eq. 0.and.itmp1 .ne. 0) then
            iferrm(nvl,1) = 1
          end if
CC
CC          see if print maximum 50 receptors.
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          if (mscrnm(nsc,nln) .ne. mblnk1) then
            if (IFCHR(mscrnm(nsc,nln),14) .ne. 0) then
              itmp2 = 0
            else if (IFCHR(mscrnm(nsc,nln),25) .ne. 0) then
              itmp2 = 1
            else
              iferrm(nvl,1) = 1
            end if
          else
            itmp2 = 0
          end if
          if (ifmode .ne. 0.and.iswopt(18) .eq. 0.and.itmp2 .ne. 0) then
            iferrm(nvl,1) = 1
          end if
CC
CC          n-hour averaging times.
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          l = 0
          j = JINIT(itmp3,6,0,0)
          m = 0
          do 350 j=1,6
            nsc = JWHER(nvl,itabsm,ispacm,j,nch)
            if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
              k = IRDBF(mscrnm(nsc,nln),nch,xdumy)
              if (k .ne. 0) then
                iferrm(nvl,j) = 1
                m = m + 1
              else
                if (ifmode .eq. 0) then
                  l = l + 1
                  itmp3(l) = xdumy
                  m = m + 1
                else
                  k = xdumy
                  i = 0
                  n = 0
  340             if (n .lt. 6.and.i .eq. 0) then
                    n = n + 1
                    if (navhrs(n) .eq. k) then
                      i = n
                    end if
                    go to 340
                  end if
                  m = m + 1
                  if (i .gt. 0) then
                    itmp3(i) = k
                  else
                    iferrm(nvl,j) = 1
                  end if
                end if
              end if
            end if
  350     continue
          if (m .eq. 0.and.itmp1+itmp2 .gt. 0) then
            iferrm(nvl,1) = 1
            itmp3(1) = -1
          end if
          if (ifmode .eq. 0) then
CC
CC            see if print each averaging time.
            nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
            if (mscrnm(nsc,nln) .ne. mblnk1) then
              if (IFCHR(mscrnm(nsc,nln),14) .ne. 0) then
                iswopt(28) = 0
              else if (IFCHR(mscrnm(nsc,nln),25) .ne. 0) then
                iswopt(28) = 1
              else
                iferrm(nvl,1) = 1
              end if
            else
              iswopt(28) = 0
            end if
          end if
CC
CC          see if print seasonal or quarterly.
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          if (mscrnm(nsc,nln) .ne. mblnk1) then
            if (IFCHR(mscrnm(nsc,nln),14) .ne. 0) then
              itmp4 = 0
            else if (IFCHR(mscrnm(nsc,nln),19) .ne. 0) then
CC              print seasonal.
              itmp4 = 1
            else if (IFCHR(mscrnm(nsc,nln),17) .ne. 0) then
CC              print quarterly.
              itmp4 = 2
            else
              iferrm(nvl,1) = 1
            end if
          else
            itmp4 = 0
          end if
          if (ifmode .ne. 0) then
            if (itmp4 .gt. 0.and.itmp4 .ne. ifsean) iferrm(nvl,1) = 1
          end if
CC
CC          see if print annual tables.
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          if (mscrnm(nsc,nln) .ne. mblnk1) then
            if (IFCHR(mscrnm(nsc,nln),14) .ne. 0) then
              itmp5 = 0
            else if (IFCHR(mscrnm(nsc,nln),1) .ne. 0) then
              itmp5 = 1
            else
              iferrm(nvl,1) = 1
            end if
          else
            itmp5 = 0
          end if
          if (ifmode .ne. 0) then
            if (itmp5 .gt. 0.and.itmp5 .ne. ifanul) iferrm(nvl,1) = 1
          end if
        end if
CC
CC        get heading.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        call MBLK1(mtitle,50)
        call MVCH1(mscrnm(nsc,nln),mtitle,MIN0(nch,50))
        i = LRJST(mtitle,50,1)
        nchtrs = ICNTR(mtitle,50)
        n = NUMC1(mscrnm(nsc,nln),nch)
        if (n .gt. 0.and.n .lt. 50) then
          i = LRJST(mscrnm(nsc,nln),nch,iflrjs)
          ifwrt = 1
        end if
CC
CC        scale factor A multiplier.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
        if (i .gt. 0) then
CC          error.
          iferrm(nvl,1) = 1
        else if (i .lt. 0) then
CC          default.
          ifwrt = 1
          ascale = 1.0
          i = IWTBF(mscrnm(nsc,nln),nch,ascale,1,iflrjs)
        else
CC          set A scale factor.
          ascale = xdumy
        end if
CC
CC        scale factor B add after multiply.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
        if (i .gt. 0) then
CC          error.
          iferrm(nvl,1) = 1
        else if (i .lt. 0) then
CC          default.
          ifwrt = 1
          bscale = 0.0
          i = IWTBF(mscrnm(nsc,nln),nch,bscale,1,iflrjs)
        else
CC          set B scale factor.
          bscale = xdumy
        end if
CC
        ner = IFWER(iferrm,nmxlin*nmxfld)
        if (ifwrt .gt. 0.or.ner .gt. 0.or.(ner .eq. 0.and.ker .ne. 0))
     *      then
CC          rewrite screen data.
          i = IWTSC(numsc,1,mscrnm,ilinem,iferrm)
        end if
        if (ner .gt. 0) then
          if (KERRP(22,0) .ge. 1) go to 400
        end if
        ker = ner
        if (jfers .eq. 0.and.ker .eq. 1) go to 290
CC
        if (ngroup .gt. 0.and.ifmode .eq. 0) then
CC
CC          screen numsc+3 (35), print source groups.
  360     numsc = numscs(7) + 3
          iferrs = IDSPL(numsc,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1,
     *                   lstrw,0)
CC          save iferrs.
          jfers = iferrs
          ifwrt = 0
          ker = IFWER(iferrm,nmxlin*nmxfld)
CC
CC          check user response to menu.
          if (jfers .eq. 1) then
CC            terminate.
            go to 400
          else if (jfers .eq. 2) then
CC            main menu.
            go to 10
          else if (jfers .ge. 3.and.jfers .le. 7) then
CC            display aux. menus.
            iferrs = IFRSP(jfers,mscrnm,ilinem,itabsm,ispacm,iferrm,0)
            if (iferrs .eq. 1) then
              go to 400
            else if (iferrs .eq. 2) then
CC              main menu.
              go to 10
            else
CC              redisplay menu.
              go to 360
            end if
          else if (jfers .ge. 8.and.jfers .le. 10) then
            go to 360
          else if (jfers .eq. 11) then
            if (ifhrly .le. 0) go to 10
            go to 290
          end if
CC          turn off error indicators for this menu.
          i = JINIT(iferrm,nmxlin*nmxfld,0,0)
CC
CC          check data.
CC
CC          get source groups.
          ngroup = 0
          i = JINIT(nsogrp,nmxgrp,0,0)
          i = JINIT(isogrp,2*nmxgrp,0,0)
          m = 0
          n = 0
          do 380 nvl=1,15                                               072497
            nln = ilinem(nvl)
            do 370 i=1,10
              nsc = JWHER(nvl,itabsm,ispacm,i,nch)
              if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
                l = IRDBF(mscrnm(nsc,nln),nch,xdumy)
                if (l .ne. 0) then
                  iferrm(nvl,i) = 1
                else
                  if (ABS(xdumy) .gt. 0.0) then
                    m = m + 1
                    n = n + 1
                    if (m .le. 2*nmxgrp) then
                      isogrp(m) = ABS(xdumy)
                      if (nsourc .gt. 0.and.isogrp(m) .gt. nsourc) then
                        isogrp(m) = nsourc
                      end if
                      if (xdumy .lt. 0.0) isogrp(m) = -isogrp(m)
                    end if
                  else
                    iferrm(nvl,i) = 1
                  end if
                end if
              else
                if (n .gt. 0) then
                  if (ngroup .lt. nmxgrp) then
                    ngroup = ngroup + 1
                    nsogrp(ngroup) = n
                  end if
                  n = 0
                end if
              end if
  370       continue
  380     continue
CC
          ner = IFWER(iferrm,nmxlin*nmxfld)
          if (ifwrt .gt. 0.or.ner .gt. 0.or.(ner .eq. 0.and.
     *        ker .ne. 0)) then
CC            rewrite screen data.
            i = IWTSC(numsc,1,mscrnm,ilinem,iferrm)
          end if
          if (ner .gt. 0) then
            if (KERRP(22,0) .ge. 1) go to 400
          end if
          ker = ner
          if (jfers .eq. 0.and.ker .eq. 1) go to 360
        else
          if (ifmode .eq. 0) ngroup = 0
        end if
CC
        ifnext = 14
        ifrsav = 0
        ifpsol = 1
        iswopt(12) = itmp1
        iswopt(18) = itmp2
        i = jinit(navhrs,6,itmp3,1)
        ifsean = itmp4
        ifanul = itmp5
        if (KERRS(54,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .eq. 3)
     *      then
          REWIND (unit=iotfil,err=400)
        end if
      else if (ifrtn .eq. 9) then
CC        finish specified.
        if (isvfil .gt. 0) then
          if (ifrdwt(1) .gt. 0.or.ifrdwt(2) .gt. 0.or.
     *        ifrdwt(3) .gt. 0.or.ifrdwt(4) .gt. 0) then
CC            see if write input data save file.
            n = 1
            if (ifsave .gt. 1.and.ifstco .eq. 0) then
  390         n = 0
              iferrs = KERRS(8,msvnam,idum,idum,xdum,xdum,xdum,
     *                       xdum,0)
              if (iferrs .eq. 3) then
                n = 1
              else if (iferrs .eq. 1) then
                go to 400
              else if (iferrs .eq. 2) then
                go to 10
              else if (iferrs .ne. 4) then
                call IBELL(4)
                go to 390
              end if
            end if
            if (n .ne. 0) then
CC              write input data save file.
              call STODT
              ifsave = ifsave + 1
            end if
          end if
        end if
        iferrs = 1
        go to 400
      end if
  400 return
CC
 9000 format (a1)
 9001 format (a132)
      end
      Subroutine DTSET(mscrn,iline)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - DTSET Version 01.1                          ---
CC--- Subroutine description - indicates input data are 'set' or    ---
CC---                          'none' in main menu.                 ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      integer*2 nsc,j,nvl,nln
CC
      character*1 mscrn
      character*4 msets,mnone,merrs,mwarn
CC
      dimension mscrn(80,25),iline(20),indx(6)
CC
      include 'OCNTRL.INC'
CC
      data msets/' SET'/,mnone/'NONE'/,merrs/' ERR'/,mwarn/'WARN'/,
     *     indx/2,4,3,1,5,6/
CC
CC**********************************************************************
CC
      nsc = 58
      nch = 4
      do 10 nvl=1,6
        j = indx(nvl)
        nln = iline(nvl)
        if (ifrdwt(j) .gt. 0) then
          if (ifrdwt(j) .eq. 1) then
CC            indicate data 'set'.
            call MVCH1(msets,mscrn(nsc,nln),nch)
          else
CC            indicate warning written to output file.
            call MVCH1(mwarn,mscrn(nsc,nln),nch)
          end if
        else if (ifrdwt(j) .eq. 0) then
CC          indicate no data 'none'.
          call MVCH1(mnone,mscrn(nsc,nln),nch)
        else if (ifrdwt(j) .lt. 0) then
CC          indicate error ' err'.
          call MVCH1(merrs,mscrn(nsc,nln),nch)
        end if
   10 continue
      return
      end
      Function JFJMP(nvls)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - JFJMP Version 01.1                          ---
CC--- Subroutine description - see if number entered for main menu. ---
CC---------------------------------------------------------------------
CC
CC    nvls - menu data field line number.
CC
CC    returns - JFJMP
CC         -1 - error.
CC          0 - no op, field was blank.
CC     1 to 9 - branch to respective area as given in main menu.
CC
CC**********************************************************************
CC
      include 'OCNTRL.INC'
      include 'ONAMES.INC'
CC
CC**********************************************************************
CC
      JFJMP = 0
      nvl = IWHER(nvls,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
        if (i .eq. 0) then
          i = xdumy
          if (i .ge. 1.and.i .le. 9) then
            JFJMP = i
          end if
        else
          call MBLK1(mnpbuf,22)
          call MVCH1(mscrnm(nsc,nln),mnpbuf,nch)
          i = KERRS(22,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0)
          JFJMP = -1
        end if
      end if
      return
      end
CCPCITS
      Function KNCRT(numsc)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - KNCRT Version 01.1                          ---
CC--- Subroutine description - reads commands for control of print  ---
CC---                          file display on crt.                 ---
CC---------------------------------------------------------------------
CC
CC    numsc - screen menu number.
CC
CC*********************************************************************
CC
      integer*2 iext,irow,icha,ii,ix,iy,kstat,numbr,numsv,icols,icole
CC
      include 'OCNTRL.INC'
CC
      save lncrt
CC
CC*********************************************************************
CC
      ix = 0
      iy = 0
      icols = 2
      icole = 78
      irow = 1
      ibflg = 0
      numbr = -1
      numsv = -1
      KNCRT = -1
CC      get user response key (character).
   10 call RDALL(iext,icha,ix,iy,icols,icole,irow)
CC      check for control that ends menu input.
      KNCRT = JFCTL(iext,icha,numsc)
CC
CC      if previous entry indicates a command bar command, set KNCRT to
CC      that command if 'enter' is encountered.  if another command is
CC      entered, override previous command.  ibflg remains set for
CC      KNCRT = -1 to allow for further processing of user input.  in
CC      all cases, command bar is redisplayed using original field
CC      status for the screen number.
      if (ibflg .gt. 0) then
        icmsta(numbr) = kstat
        call PRBAR
        if (KNCRT .ge. 0) ibflg = 0
        if (KNCRT .eq. 0) KNCRT = icmmap(numbr)
      end if
CC      if mouse double click.
      if (KNCRT .eq. 13) then
        KNCRT = INBAR(ix,iy,numbr)
CC        repeat command
        if (numbr .eq. 0) KNCRT = lncrt
        if (KNCRT .lt. 0) then
          call IBELL(1)
          ibflg = 0
          numsv = -1
          go to 10
        end if
      end if
      if (KNCRT .ge. 0) go to 30
CC      check for control that points to another data field.
      ii = KFCTL(iext,icha)
CC
CC      skip command bar processing if ii > 0, not single click (ii=15)
CC      and command bar flag is not set
CC
      if (ibflg .le. 0.and.ii .ge. 0.and.ii .ne. 15) go to 20
CC
CC    if previous entry was from command bar and has not been converted,
CC    ignore entry if it is a position command other than mouse single
CC    click.
      if (ibflg .gt. 0.and.ii .ge. 0.and.ii .ne. 15) then
        ibflg = 0
        numsv = -1
        go to 10
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
          go to 10
        end if
      end if
CC
CC      if command bar flag set and input not a valid 'alt' key, ignore
CC
      if (ibflg .gt. 0.and.numbr .le. 0) then
        ibflg = 0
        numbr = -1
        numsv = -1
        go to 10
      end if
CC
CC        if valid 'alt' key, redisplay bar and return for additional
CC        input if bar command is the same as previous chosen, option
CC        is unselected.
      if (numbr .gt. 0) then
        if (numbr .eq. numsv) then
          numsv = -1
          ibflg = 0
          go to 10
        end if
        numsv = numbr
        kstat = icmsta(numbr)
        icmsta(numbr) = 3
        ibflg = 1
        call PRBAR
        go to 10
      end if
CC
CC      check if non-standard character
   20 if (ii .lt. 0) then
        if (iext .eq. 1.and.(icha .lt. 32.or.icha .gt. 126)) then
          numsv = -1
          go to 10
        end if
        if (iext .eq. 0.and.(icha .ge. 32.or.icha .le. 126)) then
          numsv = -1
          go to 10
        end if
      end if
CC
CC      do not allow left or right bracket
CC
      if (ii .lt. 0.and.(icha .eq. 91.or.icha .eq. 93)) then
        numsv = -1
        go to 10
CC      if up arrow.
      else if (ii .eq. 1) then
        KNCRT = 7
        go to 40
CC      if down arrow.
      else if (ii .eq. 2) then
        KNCRT = 6
        go to 40
CC      if tab left.
      else if (ii .eq. 3) then
        KNCRT = 7
        go to 40
CC      if tab right.
      else if (ii .eq. 4) then
        KNCRT = 6
        go to 40
CC      if left arrow.
      else if (ii .eq. 5) then
        KNCRT = 10
        go to 40
CC      if right arrow.
      else if (ii .eq. 6) then
        KNCRT = 11
        go to 40
CC        if home.
      else if (ii .eq. 11) then
        KNCRT = 12
        go to 40
CC      if end.
      else if (ii .eq. 12) then
        KNCRT = 13
        go to 40
CC      if ctrl home.
      else if (ii .eq. 13) then
        KNCRT = 8
        go to 40
CC      if ctrl end.
      else if (ii .eq. 14) then
        KNCRT = 9
        go to 40
CC      if single mouse click.
      else if (ii .eq. 15) then
CC
CC          check if single click is in valid command bar
CC          if bar command is the same as previous chosen option is
CC          unselected
        knctl = INBAR(ix,iy,numbr)
        if (knctl .ge. 0) then
          if (numbr .eq. numsv) then
            numsv = -1
            go to 10
          end if
          numsv = numbr
          kstat = icmsta(numbr)
          icmsta(numbr) = 3
          ibflg = 2
          call PRBAR
          if (knctl .ge. 0) then
            KNCRT = knctl
            go to 30
          end if
          go to 10
        end if
        call IBELL(1)
        ibflg = 0
        numsv = -1
        go to 10
      end if
      call IBELL(1)
      go to 10
   30 lncrt = KNCRT
      if (KNCRT .ge. 1.and.KNCRT .le. 3) then
        go to 40
      else if (KNCRT .eq. 0) then
        KNCRT = 3
        go to 40
      else if (KNCRT .eq. 11) then
        KNCRT = 4
        go to 40
      else if (KNCRT .eq. 12) then
        KNCRT = 5
        go to 40
      end if
      call IBELL(1)
      go to 10
   40 return
      end
CCPCITE
