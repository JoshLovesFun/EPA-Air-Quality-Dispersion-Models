      Subroutine RINPQ
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - RINPQ Version 01.31                         --- 010400
CC--- Subroutine description - displays and reads program control   ---
CC---                          and print options.                   ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      character*58 mfnam
CC
      dimension ker(3)
CC
      include 'OCNTRL.INC'
      include 'OCDSPM.INC'
      include 'OCALCS.INC'
      include 'ONAMES.INC'
CC
CC********************************************************************
CC
      ifnext = 3
CC      start screen number 32.
      numsc = numscs(7)
      i = JINIT(ker,3,0,0)
      lflag = 0                                                         010400
CC
CC      display and read data for screens numsc to numsc+2.
CC
CC      screen numsc, start of control/print options.
   10 iferrs = IDSPL(numsc,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1,lstrw,
     *               0)
CC      save iferrs.
      jfers = iferrs
      ifwrt = 0
      ker(1) = IFWER(iferrm,nmxlin*nmxfld)
CC
CC      check user response to menu.
      if (jfers .eq. 1) then
CC        terminate.
        go to 140
      else if (jfers .eq. 2.or.jfers .eq. 11) then
CC        main menu.
        go to 120
      else if (jfers .ge. 3.and.jfers .le. 7) then
CC        display aux. menus.
        iferrs = IFRSP(jfers,mscrnm,ilinem,itabsm,ispacm,iferrm,0)
        if (iferrs .eq. 1.or.iferrs .eq. 2) then
CC          terminate or main menu.
          jfers = iferrs
          go to 120
        else
CC          redisplay menu.
          go to 10
        end if
      else if (jfers .ge. 8.and.jfers .le. 10) then
        go to 10
      end if
CC      turn off error indicators for this menu.
      i = JINIT(iferrm,nmxlin*nmxfld,0,0)
CC
CC      check data.
CC
CC      get heading.
      nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
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
CC      check for dosage calcs.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (mscrnm(nsc,nln) .ne. mblnk1) then
        if (IFCHR(mscrnm(nsc,nln),14) .ne. 0) then
          iswopt(1) = 0
        else if (IFCHR(mscrnm(nsc,nln),25) .ne. 0) then
          iswopt(1) = 1
        else
          iferrm(nvl,1) = 1
        end if
      else
        iswopt(1) = 0
      end if
CC
CC      check for concentration calcs.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      iferrm(nvl,1) = 0
      if (mscrnm(nsc,nln) .ne. mblnk1) then
        if (IFCHR(mscrnm(nsc,nln),14) .ne. 0) then
          iswopt(2) = 0
        else if (IFCHR(mscrnm(nsc,nln),25) .ne. 0) then
          iswopt(2) = 1
        else
          iferrm(nvl,1) = 1
        end if
      else
        iswopt(2) = 0
      end if
CC
CC      check for time-mean concentration calcs.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (mscrnm(nsc,nln) .ne. mblnk1) then
        if (IFCHR(mscrnm(nsc,nln),14) .ne. 0) then
          iswopt(3) = 0
        else if (IFCHR(mscrnm(nsc,nln),25) .ne. 0) then
          iswopt(3) = 1
        else
          iferrm(nvl,1) = 1
        end if
      else
        iswopt(3) = 0
      end if
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (iswopt(3) .ne. 0) then
CC
CC        get units of time for time-mean concentration.
        if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
          call UNITS(mscrnm(nsc,nln),nch,i,jdumy,0,0,5)
          if (i .lt. 0) then
            iferrm(nvl,1) = 1
          else
            itmunt = i
          end if
        else
CC          default to seconds.
          itmunt = 0
          if (iflrjs .eq. 0) then
            mscrnm(nsc,nln) = mblnk1
            mscrnm(nsc+1,nln) = malpha(45)
          else
            mscrnm(nsc,nln) = malpha(45)
            mscrnm(nsc+1,nln) = mblnk1
          end if
          ifwrt = 1
        end if
CC
CC        get time for time-mean concentration.
        nsc = JWHER(nvl,itabsm,ispacm,2,nch)
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
   20   if (i .gt. 0) then
CC          error.
          iferrm(nvl,2) = 1
        else if (i .lt. 0) then
CC          default.
          timavg = 600.0
          i = IWTBF(mscrnm(nsc,nln),nch,timavg,1,iflrjs)
          ifwrt = 1
        else
          ydumy = xdumy * constt(itmunt+1)
          if (ydumy .le. 0.0) then
            i = 1
            go to 20
          end if
          timavg = xdumy
          if (iswopt(3) .gt. 0) then
            xdumy = timavg * constt(itmunt+1)
            if (xdumy .lt. 2.5.or.xdumy .gt. 3600.0) then
              iferrm(nvl,2) = 2
              if (ifhrly .gt. 0.and.xdumy .gt. 3600.0) then
                if (ISUMI(navhrs,6) .gt. 1) then
                  j = JINIT(navhrs,6,0,0)                               010400
                  navhrs(1) = 1                                         010400
                end if
              end if
            end if
          end if
        end if
      else                                                              011403
        itmunt = 0                                                      011403
      end if
CC
CC      check for gravitational deposition.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (mscrnm(nsc,nln) .ne. mblnk1) then
        if (IFCHR(mscrnm(nsc,nln),14) .ne. 0) then
          iswopt(4) = 0
        else if (IFCHR(mscrnm(nsc,nln),25) .ne. 0) then
          if (ifgpwn .gt. 0.and.nvsprt .gt. 0) then
            iswopt(4) = 1
          else
            iswopt(4) = 0
            iferrm(nvl,1) = 1
          end if
        else
          iferrm(nvl,1) = 1
        end if
      else
        iswopt(4) = 0
      end if
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      i = 0
      j = 0
   30 if (i .lt. nsourc.and.j .eq. 0) then
        i = i + 1
        if (IVGET(isotyp(i),2) .eq. 2) then
          j = j + 1
        end if
        go to 30
      end if
      if (iswopt(1) .ne. 0.and.j .ne. 0) then
CC
CC        get time increment to integrate for dosage.
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
   40   if (i .gt. 0) then
CC          error.
          iferrm(nvl,2) = 1
        else if (i .lt. 0.or.IFEQU(xdumy,0.0) .eq. 0) then
CC          default.
          timinv = rmissc
          i = IWTBF(mscrnm(nsc,nln),nch,timinv,1,iflrjs)
          ifwrt = 1
        else
          if (xdumy .le. 0.0) then
            i = 1
            go to 40
          end if
          timinv = xdumy
        end if
      end if
CC      reset calculation offset indices for dos, con, etc.             010700
      j = JINIT(jjjoff,4,0,0)                                           010700
      ntotal = -ntotpt                                                  010700
      do i=1,4                                                          010700
        if (iswopt(i) .gt. 0) then                                      010700
          ntotal = ntotal + ntotpt                                      010700
          jjjoff(i) = ntotal                                            010700
        end if                                                          010700
      end do                                                            010700
      ntotal = ntotal + ntotpt                                          010700
CC
CC      check for final or distance dependent cloud rise.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (mscrnm(nsc,nln) .ne. mblnk1) then
        if (IFCHR(mscrnm(nsc,nln),6) .ne. 0) then
          iswopt(8) = 1
        else if (IFCHR(mscrnm(nsc,nln),4) .ne. 0) then
          if (iswopt(6) .eq. 2) then
            iferrm(nvl,1) = 1
            iswopt(8) = 0
          else
            iswopt(8) = 2
          end if
        else
          iferrm(nvl,1) = 1
        end if
      else
        iswopt(8) = 0
      end if
CC
CC      get clearing index.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
      if (i .gt. 0) then
CC        error.
        iferrm(nvl,1) = 1
      else if (i .lt. 0) then
CC        default.
        clrinx = 0.0
        if (ifhrly .gt. 0) then
          i = IWTBF(mscrnm(nsc,nln),nch,clrinx,2,iflrjs)
        else
          call MBLK1(mscrnm(nsc,nln),nch)
        end if
        ifwrt = 1
      else
        clrinx = xdumy
        if (ifhrly .eq. 0) then
          if (clrinx .gt. 0.0) iferrm(nvl,1) = 2
        else
          if (clrinx .gt. 1000.0) iferrm(nvl,1) = 2
        end if
      end if
CC
CC      see if dispersion calcs. are restricted to daytime hours
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (mscrnm(nsc,nln) .ne. mblnk1) then
        if (IFCHR(mscrnm(nsc,nln),14) .ne. 0) then
          iswopt(27) = 0
        else if (IFCHR(mscrnm(nsc,nln),25) .ne. 0) then
          iswopt(27) = 1
          if (ifhrly .eq. 0) iferrm(nvl,1) = 2
          if (ABS(geolon) .gt. 180.0.or.ABS(geolat) .gt. 90.0.or.
     *        ABS(zondif) .gt. 12) then
            i = KERRS(60,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0)
            iferrm(nvl,1) = 1
          end if
        else
          iferrm(nvl,1) = 1
        end if
      else
        iswopt(27) = 0
      end if
CC
CC      check output mass units.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        call UNITS(mscrnm(nsc,nln),nch,i,jdumy,0,0,3)
        if (i .lt. 0) then
          iferrm(nvl,1) = 1
        else
          iswopt(13) = i
        end if
      else
CC        default.
        iswopt(13) = 0
      end if
CC
CC      check output length units.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        call UNITS(mscrnm(nsc,nln),nch,i,jdumy,0,0,0)
        if (i .lt. 0) then
          iferrm(nvl,1) = 1
        else
          iswopt(15) = i
        end if
      else
CC        default.
        iswopt(15) = 0
      end if
CC
CC      check output area units.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        call UNITS(mscrnm(nsc,nln),nch,i,jdumy,0,0,9)
        if (i .lt. 0) then
          iferrm(nvl,1) = 1
        else
          iswopt(16) = i
        end if
      else
CC        default.
        iswopt(16) = 0
      end if
CC
CC      check output volume units.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        call UNITS(mscrnm(nsc,nln),nch,i,jdumy,0,0,10)
        if (i .lt. 0) then
          iferrm(nvl,1) = 1
        else
          if (i .ge. 10) then
CC            for ppm, ppb or ppt output units, airtmp, airprs and
CC            whtmol must be specified.
            xdum1 = constc(itpunt+1,2) * (airtmp - constc(itpunt+1,1))
            xdum2 = airprs * constp(iprunt+1)
            if ((xdum1 .gt. vllimt(12).or.ivarmt(12) .gt. 0).and.
     *          (xdum2 .gt. vllimt(13).or.ivarmt(13) .gt. 0).and.
     *           whtmol .gt. 0.0) then
              iswopt(17) = i
            else
              iferrm(nvl,1) = 1
            end if
          else
            iswopt(17) = i
          end if
        end if
      else
CC        default.
        iswopt(17) = 0
      end if
CC
CC      check output time units.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        call UNITS(mscrnm(nsc,nln),nch,i,jdumy,0,0,5)
        if (i .lt. 0) then
          iferrm(nvl,1) = 1
        else
          iswopt(14) = i
        end if
      else
CC        default.
        iswopt(14) = 0
      end if
CC
CC
      ner = IFWER(iferrm,nmxlin*nmxfld)
      if (ifwrt .gt. 0.or.ner .gt. 0.or.(ner .eq. 0.and.ker(1) .ne. 0))
     *    then
CC        rewrite screen data.
        i = IWTSC(numsc,1,mscrnm,ilinem,iferrm)
      end if
      if (ner .gt. 0) then
        if (KERRP(22,0) .ge. 1) go to 140
      end if
      ker(1) = ner
      if (jfers .eq. 0.and.ker(1) .eq. 1) go to 10
CC
CC      screen numsc+1, print options.
   50 if (ifhrly .le. 0) then
CC        screen 33.
        nn = 1
      else
CC        screen 34.
        nn = 2
      end if
      iferrs = IDSPL(numsc+nn,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1,
     *               lstrw,0)
      ker(2) = IFWER(iferrm,nmxlin*nmxfld)
CC      save iferrs.
      jfers = iferrs
      ifwrt = 0
CC
CC      check user response to menu.
      if (jfers .eq. 1) then
CC        terminate.
        go to 140
      else if (jfers .eq. 2) then
CC        main menu.
        go to 120
      else if (jfers .ge. 3.and.jfers .le. 7) then
CC        display aux. menus.
        iferrs = IFRSP(jfers,mscrnm,ilinem,itabsm,ispacm,iferrm,0)
        if (iferrs .eq. 1.or.iferrs .eq. 2) then
CC          terminate or main menu.
          jfers = iferrs
          go to 120
        else
CC          redisplay menu.
          go to 50
        end if
      else if (jfers .ge. 8.and.jfers .le. 10) then
        go to 50
      else if (jfers .eq. 11) then
        go to 10
      end if
CC      turn off error indicators for this menu.
      i = JINIT(iferrm,nmxlin*nmxfld,0,0)
CC
CC      check data.
CC
      nvl = 0
      if (nn .eq. 2) then
CC        check print output, solution save file mode.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        if (mscrnm(nsc,nln) .ne. mblnk1) then
          if (IFCHR(mscrnm(nsc,nln),19) .ne. 0) then
CC            summary mode.
            ifmode = 1
          else if (IFCHR(mscrnm(nsc,nln),4) .ne. 0) then
CC            detailed mode.
            ifmode = 0
          else
            iferrm(nvl,1) = 1
          end if
        else
          if (ifmode .le. 0.or.ifmode .gt. 1) then
            mscrnm(nsc,nln) = malpha(4)
            ifmode = 0
          else
            mscrnm(nsc,nln) = malpha(19)
          end if
          ifwrt = 1
        end if
      end if
CC      see if print sum of sources or groups.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (mscrnm(nsc,nln) .ne. mblnk1) then
        if (IFCHR(mscrnm(nsc,nln),19) .ne. 0) then
          ngroup = 0
        else if (IFCHR(mscrnm(nsc,nln),7) .ne. 0) then
          if (ngroup .le. 0) ngroup = 1
        else
          iferrm(nvl,1) = 1
        end if
      else
        if (ngroup .le. 0) then
          mscrnm(nsc,nln) = malpha(19)
        else
          mscrnm(nsc,nln) = malpha(7)
        end if
        ifwrt = 1
      end if
CC
CC      print page column width.
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
        xdumy = 80
        i = IWTBF(mscrnm(nsc,nln),nch,xdumy,0,iflrjs)
        ifwrt = 1
      end if
CC
CC      see if print maximum plume rise.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (mscrnm(nsc,nln) .ne. mblnk1) then
        if (IFCHR(mscrnm(nsc,nln),14) .ne. 0) then
          iswopt(21) = 0
        else if (IFCHR(mscrnm(nsc,nln),25) .ne. 0) then
          iswopt(21) = 1
        else
          iferrm(nvl,1) = 1
        end if
      else
        if (iswopt(21) .eq. 0) then
          mscrnm(nsc,nln) = malpha(14)
        else
          mscrnm(nsc,nln) = malpha(25)
        end if
        ifwrt = 1
      end if
CC      see if output Sigmaplot file is desired.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        call MBLK1(msgnam,22)
        call MVCH1(mscrnm(nsc,nln),msgnam,MIN0(nch,22))
        i = LRJST(msgnam,22,1)
        call FNAME(mfnam,58,mdirec,msgnam)
        isgfil = 34
        OPEN (unit=isgfil,file=mfnam,iostat=ier,err=60,status='unknown')
        iswopt(5) = 1
        go to 70
   60     if (KERRS(3,msgnam,ier,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *        go to 140
        iferrm(nvl,1) = 1
        isgfil = 0
        iswopt(5) = 0
      else
        iswopt(5) = 0
      end if
CC
   70 if (ifhrly .gt. 0) then
        lflag = 0                                                       010400
CC
CC        see if print table of highest and second highest.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        if (mscrnm(nsc,nln) .ne. mblnk1) then
          if (IFCHR(mscrnm(nsc,nln),14) .ne. 0) then
            iswopt(12) = 0
          else if (IFCHR(mscrnm(nsc,nln),25) .ne. 0) then
            if (ntotal*2 .gt. nmxcor.and.ifmode .ne. 0) then            010400
              iferrm(nvl,1) = 1                                         010400
              lflag = 1                                                 010400
            else                                                        010400
              iswopt(12) = 1
            end if                                                      010400
          else
            iferrm(nvl,1) = 1
          end if
        else
          if (iswopt(12) .le. 0) then
            mscrnm(nsc,nln) = malpha(14)
          else
            mscrnm(nsc,nln) = malpha(25)
          end if
          ifwrt = 1
        end if
CC
CC        see if print maximum 50 receptors.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        if (mscrnm(nsc,nln) .ne. mblnk1) then
          if (IFCHR(mscrnm(nsc,nln),14) .ne. 0) then
            iswopt(18) = 0
          else if (IFCHR(mscrnm(nsc,nln),25) .ne. 0) then
            iswopt(18) = 1
          else
            iferrm(nvl,1) = 1
          end if
        else
          if (iswopt(18) .le. 0) then
            mscrnm(nsc,nln) = malpha(14)
          else
            mscrnm(nsc,nln) = malpha(25)
          end if
          ifwrt = 1
        end if
CC
CC        n hour averaging times.
        ydumy = timavg * constt(itmunt+1)                               111098
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        l = 0
        do 80 j=1,6
          nsc = JWHER(nvl,itabsm,ispacm,j,nch)
          if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
            k = IRDBF(mscrnm(nsc,nln),nch,xdumy)
            if (k .ne. 0) then
              iferrm(nvl,j) = 1
            else
              if (ydumy .gt. 3600.0.and.xdumy .gt. 1.0) then            111098
                iferrm(nvl,j) = 1                                       111098
              else                                                      111098
                l = l + 1
                navhrs(l) = xdumy
              end if                                                    111098
            end if
          end if
   80   continue
        if (l .lt. 6) i = JINIT(navhrs(l+1),6-l,0,0)
        if (l .le. 0) then
          if (KERRS(62,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *        go to 140
          navhrs(1) = 1
          nsc = JWHER(nvl,itabsm,ispacm,1,nch)
          xdumy = 1.0
          k = IWTBF(mscrnm(nsc,nln),nch,xdumy,0,iflrjs)
          ifwrt = 1
        end if
CC
CC        see if print each averaging time.
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
          if (iswopt(28) .le. 0) then
            mscrnm(nsc,nln) = malpha(14)
          else
            mscrnm(nsc,nln) = malpha(25)
          end if
          ifwrt = 1
        end if
CC
CC        see if print seasonal or quarterly.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        if (mscrnm(nsc,nln) .ne. mblnk1) then
          if (IFCHR(mscrnm(nsc,nln),14) .ne. 0) then
            ifsean = 0
          else if (IFCHR(mscrnm(nsc,nln),19) .ne. 0) then
CC            print seasonal.
            ifsean = 1
            if (timavg * constt(itmunt+1) .gt. 3600.0) then
              iferrm(nvl,1) = 1
            end if
          else if (IFCHR(mscrnm(nsc,nln),17) .ne. 0) then
CC            print quarterly.
            ifsean = 2
            if (timavg * constt(itmunt+1) .gt. 3600.0) then
              iferrm(nvl,1) = 2                                         011403
            end if
          else
            iferrm(nvl,1) = 1
          end if
        else
          if (ifsean .le. 0) then
            mscrnm(nsc,nln) = malpha(14)
          else if (ifsean .eq. 1) then
            mscrnm(nsc,nln) = malpha(19)
          else
            mscrnm(nsc,nln) = malpha(17)
          end if
          ifwrt = 1
        end if
CC
CC        see if print annual tables.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        if (mscrnm(nsc,nln) .ne. mblnk1) then
          if (IFCHR(mscrnm(nsc,nln),14) .ne. 0) then
            ifanul = 0
          else if (IFCHR(mscrnm(nsc,nln),1) .ne. 0) then
            ifanul = 1
            if (timavg * constt(itmunt+1) .gt. 3600.0) then             011403
              iferrm(nvl,1) = 2
            end if
          else
            iferrm(nvl,1) = 1
          end if
        else
          if (ifanul .le. 0) then
            mscrnm(nsc,nln) = malpha(14)
          else
            mscrnm(nsc,nln) = malpha(1)
          end if
          ifwrt = 1
        end if
CC
CC        see if print hourly input meteorological data.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        if (mscrnm(nsc,nln) .ne. mblnk1) then
          if (IFCHR(mscrnm(nsc,nln),14) .ne. 0) then
            iswopt(24) = 0
          else if (IFCHR(mscrnm(nsc,nln),25) .ne. 0) then
            iswopt(24) = 1
          else
            iferrm(nvl,1) = 1
          end if
        else
          if (iswopt(24) .le. 0) then
            mscrnm(nsc,nln) = malpha(14)
          else
            mscrnm(nsc,nln) = malpha(25)
          end if
          ifwrt = 1
        end if
CC
CC        see if print hourly input source data.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        if (mscrnm(nsc,nln) .ne. mblnk1) then
          if (IFCHR(mscrnm(nsc,nln),14) .ne. 0) then
            iswopt(26) = 0
          else if (IFCHR(mscrnm(nsc,nln),25) .ne. 0) then
            iswopt(26) = 1
          else
            iferrm(nvl,1) = 1
          end if
        else
          if (iswopt(26) .le. 0) then
            mscrnm(nsc,nln) = malpha(14)
          else
            mscrnm(nsc,nln) = malpha(25)
          end if
          ifwrt = 1
        end if
CC
CC        see if print calm wind warning messages.                      090500
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        if (mscrnm(nsc,nln) .ne. mblnk1) then
          if (IFCHR(mscrnm(nsc,nln),14) .ne. 0) then
            iswopt(33) = 0
          else if (IFCHR(mscrnm(nsc,nln),25) .ne. 0) then
            iswopt(33) = 1
          else
            iferrm(nvl,1) = 1
          end if
        else
          if (iswopt(33) .le. 0) then
            mscrnm(nsc,nln) = malpha(14)
          else
            mscrnm(nsc,nln) = malpha(25)
          end if
          ifwrt = 1
        end if                                                          051503
CC
CC        see if print met. data error/warning messages.        
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        if (mscrnm(nsc,nln) .ne. mblnk1) then
          if (IFCHR(mscrnm(nsc,nln),14) .ne. 0) then
            iswopt(35) = 0
          else if (IFCHR(mscrnm(nsc,nln),25) .ne. 0) then
            iswopt(35) = 1
          else
            iferrm(nvl,1) = 1
          end if
        else
          if (iswopt(35) .le. 0) then
            mscrnm(nsc,nln) = malpha(14)
          else
            mscrnm(nsc,nln) = malpha(25)
          end if
          ifwrt = 1
        end if                                                          051503
        if (iswopt(12)+iswopt(18)+iswopt(28)+ifsean+ifanul .le. 0) then
          if (KERRS(63,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *        go to 140
        end if
      else
        iswopt(12) = 0
        iswopt(18) = 0
        iswopt(24) = 0
        iswopt(26) = 0
        iswopt(28) = 0
        i = JINIT(navhrs,6,0,0)
        ifsean = 0
        ifanul = 0
      end if
CC
CC      plume rise option for instantaneous sources.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (mscrnm(nsc,nln) .ne. mblnk1) then
        if (IFCHR(mscrnm(nsc,nln),1) .ne. 0) then
          iswopt(25) = 0
        else if (IFCHR(mscrnm(nsc,nln),2) .ne. 0) then
          iswopt(25) = 1
        else
          iferrm(nvl,1) = 1
        end if
      else
        iswopt(25) = 0
        mscrnm(nsc,nln) = malpha(1)
        ifwrt = 1
      end if
      ner = IFWER(iferrm,nmxlin*nmxfld)
      if (ifwrt .gt. 0.or.ner .gt. 0.or.(ner .eq. 0.and.ker(2) .ne. 0))
     *    then
CC        rewrite screen data.
        i = IWTSC(numsc+nn,1,mscrnm,ilinem,iferrm)
      end if
      if (lflag .ne. 0) then                                            010400
        idum = ISUMI(iswopt,4)                                          010400
        xdum = nmxcor                                                   010400
        l = KERRS(69,mnpbuf,ntotal,idum,xdum,xdum,xdum,xdum,0)          010400
        lflag = 0                                                       010400
      end if                                                            010400
      if (ner .gt. 0) then
        if (KERRP(22,0) .ge. 1) go to 140
      end if
      ker(2) = ner
      if (jfers .eq. 0.and.ker(2) .eq. 1) go to 50
      if (ifmode .ne. 0) then
        i = KERRS(56,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0)
        if (i .eq. 3) then
          iswopt(19) = 1
        else
          iswopt(19) = 0
        end if
      end if
CC
CC      screen numsc+3 (35), print source groups.
   90 if (ngroup .gt. 0) then
        nn = 3
        iferrs = IDSPL(numsc+nn,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1,
     *                 lstrw,0)
CC        save iferrs.
        jfers = iferrs
        ifwrt = 0
        ker(3) = IFWER(iferrm,nmxlin*nmxfld)
CC
CC        check user response to menu.
        if (jfers .eq. 1) then
CC          terminate.
          go to 140
        else if (jfers .eq. 2) then
CC          main menu.
          go to 120
        else if (jfers .ge. 3.and.jfers .le. 7) then
CC          display aux. menus.
          iferrs = IFRSP(jfers,mscrnm,ilinem,itabsm,ispacm,iferrm,0)
          if (iferrs .eq. 1.or.iferrs .eq. 2) then
CC            terminate or main menu.
            jfers = iferrs
            go to 120
          else
CC            redisplay menu.
            go to 90
          end if
        else if (jfers .ge. 8.and.jfers .le. 10) then
          go to 90
        else if (jfers .eq. 11) then
          go to 50
        end if
CC        turn off error indicators for this menu.
        i = JINIT(iferrm,nmxlin*nmxfld,0,0)
CC
CC        check data.
CC
CC        get source groups.
        ngroup = 0
        i = JINIT(nsogrp,nmxgrp,0,0)
        i = JINIT(isogrp,2*nmxgrp,0,0)
        m = 0
        n = 0
        do 110 nvl=1,15                                                 072497
          nln = ilinem(nvl)
          do 100 i=1,10
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
                    if (n .gt. 1.and.isogrp(m) .lt. 0) then
                      if (isogrp(m-1) .gt. IABS(isogrp(m))) then
                        iferrm(nvls,is) = 1
                      end if
                    end if
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
            nvls = nvl
            is = i
  100     continue
  110   continue
CC
        ner = IFWER(iferrm,nmxlin*nmxfld)
        if (ifwrt .gt. 0.or.ner .gt. 0.or.(ner .eq. 0.and.
     *      ker(3) .ne. 0)) then
CC          rewrite screen data.
          i = IWTSC(numsc+nn,1,mscrnm,ilinem,iferrm)
        end if
        if (ner .gt. 0) then
          if (KERRP(22,0) .ge. 1) go to 140
        end if
        ker(3) = ner
        if (jfers .eq. 0.and.ker(3) .eq. 1) go to 90
      end if
CC
CC      check if data complete.
  120 if (IFWER(ker,3) .eq. 1) then
        ifset = -1
      else
        ifset = 1
        if (ISUMI(iswopt,4) .le. 0) then
          ifset = 0
          if (KERRS(64,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *        go to 140
        end if
      end if
      if (ifset .eq. 1) then
        j = JINIT(jjjoff,4,0,0)
        ntotal = -ntotpt
        do 130 i=1,4
          if (iswopt(i) .gt. 0) then
            ntotal = ntotal + ntotpt
            jjjoff(i) = ntotal
          end if
  130   continue
        ntotal = ntotal + ntotpt + 1
        if (ntotal .gt. nmxcor) then
          ifset = -1
          if (KERRS(9,mscrch,ntotal,nmxcor,xdum,xdum,xdum,xdum,0) .ge.
     *        1) go to 140
        end if
      end if
      ifrdwt(1) = ifset
  140 return
      end
