      Subroutine RINPO
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - RINPO Version 01.1                          ---
CC--- Subroutine description - displays and reads meteorological    ---
CC---                          data.                                ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      character*1 mslsh
      character*58 mfnam
CC
      dimension ker(10)
CC
      include 'OCNTRL.INC'
      include 'OCDSPM.INC'
      include 'OCALCS.INC'
      include 'ONAMES.INC'
      include 'OWNTRL.INC'
CC
      equivalence (mfnam,mscrch(1)),(jfnxt,functx(1)),
     *            (ker(1),functx(2)),(numsc,functx(37))
CC
      data mslsh/'/'/
CC
CC********************************************************************
CC
      if (jfnxt .eq. 5.and.ifhrly .ne. 0) go to 70
CC      screen numsc+4 (26), meteorological data.
   10 nn = 4
CC      read screen menu and screen data.
      numf = IRDSC(numsc+nn,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
CC
CC      set default met. data.
CC
CC      check wind speed shear.
      nvl = IWHER(0,ilinem,itabsm,ispacm,1,2,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
        spdshr = rmissc
        issunt = 0
        jssunt = 0
        if (iflrjs .eq. 0) then
          mscrnm(nsc+nch-1,nln) = malpha(3)
        else
          mscrnm(nsc,nln) = malpha(3)
        end if
        nsc = JWHER(nvl,itabsm,ispacm,1,nch)
        call MBLK1(mscrnm(nsc,nln),nch)
        if (iflrjs .eq. 0) then
          mscrnm(nsc+nch-3,nln) = malpha(39)
          mscrnm(nsc+nch-2,nln) = mslsh
          mscrnm(nsc+nch-1,nln) = malpha(45)
        else
          mscrnm(nsc,nln) = malpha(39)
          mscrnm(nsc+1,nln) = mslsh
          mscrnm(nsc+2,nln) = malpha(45)
        end if
      end if
CC      check wind direction shear.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,2,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
        dirshr = 0.0
        idsunt = 0
        jdsunt = 0
        i = IWTBF(mscrnm(nsc,nln),nch,dirshr,1,iflrjs)
        nsc = JWHER(nvl,itabsm,ispacm,1,nch)
        call MBLK1(mscrnm(nsc,nln),nch)
        if (iflrjs .eq. 0) then
          mscrnm(nsc+nch-3,nln) = malpha(30)
          mscrnm(nsc+nch-2,nln) = mslsh
          mscrnm(nsc+nch-1,nln) = malpha(39)
        else
          mscrnm(nsc,nln) = malpha(30)
          mscrnm(nsc+1,nln) = mslsh
          mscrnm(nsc+2,nln) = malpha(39)
        end if
      end if
CC      check air temperature.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,2,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
        airtmp = 20.0
        itpunt = 0
        i = IWTBF(mscrnm(nsc,nln),nch,airtmp,1,iflrjs)
        nsc = JWHER(nvl,itabsm,ispacm,1,nch)
        call MBLK1(mscrnm(nsc,nln),nch)
        if (iflrjs .eq. 0) then
          mscrnm(nsc+nch-1,nln) = malpha(3)
        else
          mscrnm(nsc,nln) = malpha(3)
        end if
      end if
CC      surface roughness length.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,2,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
        roughl = 0.0
        irount = 5
        i = IWTBF(mscrnm(nsc,nln),nch,roughl,4,iflrjs)
        nsc = JWHER(nvl,itabsm,ispacm,1,nch)
        call MBLK1(mscrnm(nsc,nln),nch)
        if (iflrjs .eq. 0) then
          mscrnm(nsc+nch-2,nln) = malpha(29)
          mscrnm(nsc+nch-1,nln) = malpha(39)
        else
          mscrnm(nsc,nln) = malpha(29)
          mscrnm(nsc+1,nln) = malpha(39)
        end if
      end if
CC      air pressure.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,2,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
        airprs = 870.0
        i = IWTBF(mscrnm(nsc,nln),nch,airprs,1,iflrjs)
        nsc = JWHER(nvl,itabsm,ispacm,1,nch)
        call MBLK1(mscrnm(nsc,nln),nch)
        if (iflrjs .eq. 0) then
          mscrnm(nsc+nch-2,nln) = malpha(39)
          mscrnm(nsc+nch-1,nln) = malpha(28)
        else
          mscrnm(nsc,nln) = malpha(39)
          mscrnm(nsc+1,nln) = malpha(28)
        end if
      end if
CC      display screen and get user response.
      iferrs = IOSCN(numsc+nn,mscrnm,ilinem,itabsm,ispacm,iferrm,lstrw,
     *               0)
      jfers = iferrs
CC      save screen menu.
      if (iferrs .ne. 10) i = IWTSC(numsc+nn,1,mscrnm,ilinem,iferrm)
CC      if terminate.
      if (iferrs .eq. 1) go to 190
      ker(5) = IFWER(iferrm,nmxlin*nmxfld)
      jfers = iferrs
      ifwrt = 0
CC
CC      check user response to fourth met. menu.
      if (jfers .eq. 2) then
CC        main menu.
        go to 180
      else if (jfers .ge. 3.and.jfers .le. 7) then
CC        display aux. menus.
        iferrs = IFRSP(jfers,mscrnm,ilinem,itabsm,ispacm,iferrm,0)
        if (iferrs .eq. 1.or.iferrs .eq. 2) then
CC          terminate or main menu.
          jfers = iferrs
          go to 180
        else
CC          redisplay menu.
          go to 10
        end if
      else if (jfers .ge. 8.and.jfers .le. 10) then
        go to 10
      else if (jfers .eq. 11) then
        go to 200
      end if
CC      turn of error array.
      i = JINIT(iferrm,nmxlin*nmxfld,0,0)
CC
CC      check data.
CC
CC      get wind speed shear units.
      nvl = IWHER(0,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        call UNITS(mscrnm(nsc,nln),nch,i,j,0,0,7)
        if (i .lt. 0.or.(j .lt. 0.and.i .ne. 9)) then
          iferrm(nvl,1) = 1
        else
          issunt = i
          jssunt = j
        end if
      else
CC          default to meters/second.
        issunt = 0
        jssunt = 0
        ifwrt = 1
        call MBLK1(mscrnm(nsc,nln),nch)
        if (iflrjs .eq. 0) then
          mscrnm(nsc+nch-3,nln) = malpha(39)
          mscrnm(nsc+nch-2,nln) = mslsh
          mscrnm(nsc+nch-1,nln) = malpha(45)
        else
          mscrnm(nsc,nln) = malpha(39)
          mscrnm(nsc+1,nln) = mslsh
          mscrnm(nsc+2,nln) = malpha(45)
        end if
      end if
CC
CC      get wind speed shear.
      nsc = JWHER(nvl,itabsm,ispacm,2,nch)
      i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
   20 if (i .gt. 0) then
CC        error.
        iferrm(nvl,2) = 1
      else if (i .lt. 0) then
CC        if 'x' or blank, default.
        if (i .eq. -1.or.i .eq. -3) then
          spdshr = rmissc
          call MBLK1(mscrnm(nsc,nln),nch)
          if (iflrjs .eq. 0) then
            mscrnm(nsc+nch-1,nln) = malpha(3)
          else
            mscrnm(nsc,nln) = malpha(3)
          end if
          ifwrt = 1
        else if (i .eq. -2) then
CC          if 'c'.
          spdshr = rmissc
        end if
      else
        ydumy = xdumy * constl(issunt+1)
        if (jssunt .ge. 0) ydumy = ydumy / constt(jssunt+1)
        if (ydumy .lt. vllimt(16).or.ydumy .gt. vulimt(16)) then
          i = 1
          go to 20
        end if
CC        set wind speed shear.
        spdshr = xdumy
      end if
CC
CC      get wind direction shear units.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        call UNITS(mscrnm(nsc,nln),nch,i,j,0,0,14)
        if (i .lt. 0.or.j .lt. 0) then
          iferrm(nvl,1) = 1
        else
          idsunt = i
          jdsunt = j
        end if
      else
CC          default to degrees/meter.
        idsunt = 0
        jdsunt = 0
        ifwrt = 1
        call MBLK1(mscrnm(nsc,nln),nch)
        if (iflrjs .eq. 0) then
          mscrnm(nsc+nch-3,nln) = malpha(30)
          mscrnm(nsc+nch-2,nln) = mslsh
          mscrnm(nsc+nch-1,nln) = malpha(39)
        else
          mscrnm(nsc,nln) = malpha(30)
          mscrnm(nsc+1,nln) = mslsh
          mscrnm(nsc+2,nln) = malpha(39)
        end if
      end if
CC
CC      get wind direction shear.
      nsc = JWHER(nvl,itabsm,ispacm,2,nch)
      i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
   30 if (i .gt. 0) then
CC        error.
        iferrm(nvl,2) = 1
      else if (i .lt. 0) then
CC        if 'x' ,'c' or blank, default.
        dirshr = 0.0
        i = IWTBF(mscrnm(nsc,nln),nch,dirshr,1,iflrjs)
        ifwrt = 1
      else
        ydumy = xdumy
        if (idsunt .ne. 0) ydumy = xdumy * radtod
        if (ydumy .lt. vllimt(17).or.ydumy .gt. vulimt(17)) then
          i = 1
          go to 30
        end if
CC        set wind direction shear.
        dirshr = xdumy
      end if
CC      get air temperature units.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        call UNITS(mscrnm(nsc,nln),nch,i,jdumy,0,0,6)
        if (i .lt. 0) then
          iferrm(nvl,1) = 1
        else
          itpunt = i
        end if
      else
CC          default to degrees c.
        itpunt = 0
        ifwrt = 1
        call MBLK1(mscrnm(nsc,nln),nch)
        if (iflrjs .eq. 0) then
          mscrnm(nsc+nch-1,nln) = malpha(3)
        else
          mscrnm(nsc,nln) = malpha(3)
        end if
      end if
CC
CC      get air temperature.
      nsc = JWHER(nvl,itabsm,ispacm,2,nch)
      i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
   40 if (i .gt. 0) then
CC        error.
        iferrm(nvl,2) = 1
      else if (i .lt. 0) then
CC        if 'x' or blank, default.
        airtmp = 20.0
        i = IWTBF(mscrnm(nsc,nln),nch,airtmp,1,iflrjs)
        if (itpunt .ne. 0) then
          nsc = JWHER(nvl,itabsm,ispacm,1,nch)
          call MBLK1(mscrnm(nsc,nln),nch)
          if (iflrjs .eq. 0) then
            mscrnm(nsc+nch-1,nln) = malpha(3)
          else
            mscrnm(nsc,nln) = malpha(3)
          end if
          itpunt = 0
        end if
        ifwrt = 1
      else
        ydumy = constc(itpunt+1,2) * (xdumy - constc(itpunt+1,1))
        if (ydumy .lt. vllimt(12).or.ydumy .gt. vulimt(12)) then
          i = 1
          go to 40
        end if
CC        set air temperature.
        airtmp = xdumy
      end if
CC
CC      roughness length units.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        call UNITS(mscrnm(nsc,nln),nch,i,jdumy,0,0,0)
        if (i .lt. 0) then
          iferrm(nvl,1) = 1
        else
          irount = i
        end if
      else
CC          default to cm.
        irount = 5
        ifwrt = 1
        call MBLK1(mscrnm(nsc,nln),nch)
        if (iflrjs .eq. 0) then
          mscrnm(nsc+nch-2,nln) = malpha(29)
          mscrnm(nsc+nch-1,nln) = malpha(39)
        else
          mscrnm(nsc,nln) = malpha(29)
          mscrnm(nsc+1,nln) = malpha(39)
        end if
      end if
CC
CC      get roughness length.
      nsc = JWHER(nvl,itabsm,ispacm,2,nch)
      i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
   50 if (i .gt. 0) then
CC        error.
        iferrm(nvl,2) = 1
      else if (i .lt. 0) then
CC        if 'x' ,'c' or blank, default.
        roughl = 0.0
        i = IWTBF(mscrnm(nsc,nln),nch,roughl,1,iflrjs)
        ifwrt = 1
        if (irount .gt. 0) then
          nsc = JWHER(nvl,itabsm,ispacm,1,nch)
          call MBLK1(mscrnm(nsc,nln),nch)
          if (iflrjs .eq. 0) then
            mscrnm(nsc+nch-2,nln) = malpha(29)
            mscrnm(nsc+nch-1,nln) = malpha(39)
          else
            mscrnm(nsc,nln) = malpha(29)
            mscrnm(nsc+1,nln) = malpha(39)
          end if
          irount = 0
        end if
      else
        ydumy = xdumy * constl(irount+1) / constl(6)                    092904
        if (ydumy .lt. vllimt(21).or.ydumy .gt. vulimt(21)) then
          i = 1
          go to 50
        end if
CC        set roughness length.
        roughl = xdumy
      end if
CC
CC      get units of air pressure.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        call UNITS(mscrnm(nsc,nln),nch,i,jdumy,0,0,4)
        if (i .lt. 0) then
          iferrm(nvl,1) = 1
        else
          iprunt = i
        end if
      else
CC          default to millibars.
        iprunt = 0
        ifwrt = 1
        call UNITS(mscrnm(nsc,nln),nch,iprunt,0,0,1,4)
      end if
CC
CC      get air pressure.
      nsc = JWHER(nvl,itabsm,ispacm,2,nch)
      i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
   60 if (i .gt. 0) then
CC        error.
        iferrm(nvl,2) = 1
      else if (i .lt. 0) then
CC        if 'x' ,'c' or blank, default.
        airprs = 870.0
        i = IWTBF(mscrnm(nsc,nln),nch,airprs,1,iflrjs)
        if (iprunt .ne. 0) then
          nsc = JWHER(nvl,itabsm,ispacm,1,nch)
          iprunt = 0
          call UNITS(mscrnm(nsc,nln),nch,iprunt,0,0,1,4)
        end if
        ifwrt = 1
      else
        ydumy = xdumy * constp(iprunt+1)
        if (ydumy .lt. vllimt(13).or.ydumy .gt. vulimt(13)) then
          i = 1
          go to 60
        end if
CC        set air pressure.
        airprs = xdumy
      end if
      ner = IFWER(iferrm,nmxlin*nmxfld)
      if (ifwrt .gt. 0.or.ner .gt. 0.or.(ner .eq. 0.and.ker(5) .gt. 0))
     *    then
CC        rewrite screen data.
        i = IWTSC(numsc+nn,1,mscrnm,ilinem,iferrm)
      end if
      if (ner .gt. 0) then
        if (KERRP(22,0) .ge. 1) go to 190
      end if
      ker(5) = ner
      if (jfers .eq. 0.and.ker(5) .eq. 1) go to 10
CC
CC      if hourly input data .
   70 if (ifhrly .ne. 0) then
CC
CC        screen numsc+5 (27), meteorological data.
        nn = 5
CC        read screen menu and screen data.
        iferrs = IDSPL(numsc+nn,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1,
     *                 lstrw,0)
CC        if terminate.
        if (iferrs .eq. 1) go to 190
        ker(6) = IFWER(iferrm,nmxlin*nmxfld)
        jfers = iferrs
        ifwrt = 0
CC
CC        check user response to fifth met. menu.
        if (jfers .eq. 2) then
CC          main menu.
          go to 180
        else if (jfers .ge. 3.and.jfers .le. 7) then
CC          display aux. menus.
          iferrs = IFRSP(jfers,mscrnm,ilinem,itabsm,ispacm,iferrm,0)
          if (iferrs .eq. 1.or.iferrs .eq. 2) then
CC            terminate or main menu.
            jfers = iferrs
            go to 180
          else
            go to 70
          end if
        else if (jfers .ge. 8.and.jfers .le. 10) then
          go to 70
        else if (jfers .eq. 11) then
          go to 10
        end if
CC        turn off error array.
        i = JINIT(iferrm,nmxlin*nmxfld,0,0)
CC
CC        check data
CC
CC        get data format.
        nvl = IWHER(2,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
        if (mscrnm(nsc,nln) .ne. mblnk1) then
          ihrfmt = IFFOP(mscrnm(nsc,nln),7)
          if (ihrfmt .lt. 0) then
            iferrm(nvl,1) = 1
            ihrfmt = 0
          end if
        else
          ihrfmt = 0
        end if
CC
CC        get name of input met. data file.
        nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
        if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
          call MBLK1(mnhnam,22)
          call MVCH1(mscrnm(nsc,nln),mnhnam,MIN0(nch,22))
          i = LRJST(mnhnam,22,1)
CC          open data file as logical units inhfil.
          inhfil = 27
CC          combine file name and directory.
          call FNAME(mfnam,58,mdirec,mnhnam)
          if (ihrfmt .eq. 2) then
CC            unformatted binary.
            OPEN (unit=inhfil,file=mfnam,iostat=ier,err=80,
     *           status='old',form='unformatted')
          else
CC            formatted character.
            OPEN (unit=inhfil,file=mfnam,iostat=ier,err=80,
     *           status='old',form='formatted')
          end if
          go to 90
   80     iferrm(nvl,1) = 1
          if (KERRS(3,mnhnam,ier,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *        go to 190
        else
          iferrm(nvl,1) = 1
        end if
CC
CC        see how calms are to be treated
   90   nvl = IWHER(3,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
        if (mscrnm(nsc,nln) .ne. mblnk1) then
          if (IFCHR(mscrnm(nsc,nln),14) .ne. 0) then
            iswopt(10) = 0
          else if (IFCHR(mscrnm(nsc,nln),19) .ne. 0) then
            iswopt(10) = 1
          else
            iferrm(nvl,1) = 1
          end if
        else
          iswopt(10) = 0
        end if
CC
CC        see how missing data are to be treated.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        if (mscrnm(nsc,nln) .ne. mblnk1) then
          if (IFCHR(mscrnm(nsc,nln),14) .ne. 0) then
            iswopt(11) = 0
          else if (IFCHR(mscrnm(nsc,nln),4) .ne. 0) then
            iswopt(11) = 1
          else
            iferrm(nvl,1) = 1
          end if
        else
          iswopt(11) = 0
        end if
CC
CC        see if minimum of 75% is used for averages or actual count.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        if (mscrnm(nsc,nln) .ne. mblnk1) then
          if (IFCHR(mscrnm(nsc,nln),13) .ne. 0) then
            iswopt(29) = 0
          else if (IFCHR(mscrnm(nsc,nln),1) .ne. 0) then
            iswopt(29) = 1
          else
            iferrm(nvl,1) = 1
          end if
        else
          iswopt(29) = 0
        end if
        ner = IFWER(iferrm,nmxlin*nmxfld)
        if (ifwrt .gt. 0.or.ner .gt. 0.or.(ner .eq. 0.and.
     *      ker(6) .gt. 0)) then
CC          rewrite screen data.
          i = IWTSC(numsc+nn,1,mscrnm,ilinem,iferrm)
        end if
        if (ner .gt. 0) then
          if (KERRP(22,0) .ge. 1) go to 190
        end if
        ker(6) = ner
        if (jfers .eq. 0.and.ker(6) .eq. 1) go to 70
CC
CC          screen numsc+6 (28), meteorological data.
  100   nn = 6
CC        read screen menu and screen data.
        numf = IRDSC(numsc+nn,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
CC        set default year.
        nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
        if (NUMC1(mscrnm(nsc,nln),nch) .le. 0.and.iniyrs .gt. 0) then
          xdumy = iniyrs
          i = IWTBF(mscrnm(nsc,nln),nch,xdumy,0,iflrjs)
        end if
CC        display screen and get user response.
        iferrs = IOSCN(numsc+nn,mscrnm,ilinem,itabsm,ispacm,iferrm,
     *                 lstrw,0)
        jfers = iferrs
CC        save screen menu.
        if (iferrs .ne. 10) i = IWTSC(numsc+nn,1,mscrnm,ilinem,iferrm)
CC        if terminate.
        if (iferrs .eq. 1) go to 190
        ker(7) = IFWER(iferrm,nmxlin*nmxfld)
        jfers = iferrs
        ifwrt = 0
CC
CC        check user response to met. menu.
        if (jfers .eq. 2) then
CC          main menu.
          go to 180
        else if (jfers .ge. 3.and.jfers .le. 7) then
CC          display aux. menus.
          iferrs = IFRSP(jfers,mscrnm,ilinem,itabsm,ispacm,iferrm,0)
          if (iferrs .eq. 1.or.iferrs .eq. 2) then
CC            terminate or main menu.
            jfers = iferrs
            go to 180
          else
            go to 100
          end if
        else if (jfers .ge. 8.and.jfers .le. 10) then
          go to 100
        else if (jfers .eq. 11) then
          go to 70
        end if
CC        turn of error array.
        i = JINIT(iferrm,nmxlin*nmxfld,0,0)
CC
CC        check data.
CC
CC        get year of met. data.
        nvl = IWHER(0,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
        if (i .eq. 0.and.xdumy .gt. 0.0) then
          if (xdumy .lt. vllimt(1).or.xdumy .gt. vulimt(1)) then
            iferrm(nvl,1) = 1
          else
            iscyrs = xdumy
          end if
        else if (i .lt. 0) then
          if (iniyrs .gt. 0) then
            iscyrs = iniyrs
          else
            iferrm(nvl,1) = 1
          end if
        else
          iferrm(nvl,1) = 1
        end if
        if (iferrm(nvl,1) .eq. 0) then
CC        if (MOD(iscyrs,4) .ne. 0) then                                010400
          if (LEAPS(iscyrs) .eq. 0) then                                010400
            nmxdys = 365
          else
            nmxdys = 366
          end if
        end if
CC
CC        see if rural or urban mixing layer depths.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        if (ihrfmt .ge. 2) then
          ischmd = 0
          if (mscrnm(nsc,nln) .ne. mblnk1) then
            if (IFCHR(mscrnm(nsc,nln),18) .ne. 0) then
CC              rural.
              ischmd = 0
            else if (IFCHR(mscrnm(nsc,nln),21) .ne. 0) then
CC              urban.
              ischmd = 1
            else
              iferrm(nvl,1) = 1
            end if
          end if
        end if
CC
CC        see if all days are to be processed.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nlns,nscs,nchs)
        nvls = nvl
        iflg = 0
        if (mscrnm(nscs,nlns) .ne. mblnk1) then
          if (IFCHR(mscrnm(nscs,nlns),25) .ne. 0) then
            mscrnm(nscs,nlns) = mblnk1
            iflg = 1
            i = JINIT(iscdys,nmxdys,1,0)
            ifwrt = 1
CC            set screen menu.
            k = 0
            m = 0
            do 130 j=1,6
              nvl = IWHER(j,ilinem,itabsm,ispacm,3,1,nln,nsc,nch)
              do 120 i=1,2
                m = m + 1
                nsc = JWHER(nvl,itabsm,ispacm,i,nch)
                l = NMXDY(iscyrs,m)
                do 110 n=1,l
                  k = k + 1
                  if (iscdys(k) .eq. 1) then
                    mscrnm(nsc+n-1,nln) = malpha(25)
                  else
                    mscrnm(nsc+n-1,nln) = mblnk1
                  end if
  110           continue
  120         continue
  130       continue
          else if (IFCHR(mscrnm(nscs,nlns),14) .eq. 0) then
            iferrm(nvls,1) = 1
          end if
        end if
        if (iflg .eq. 0) then
CC          get days user wishes to process.
          k = 0
          m = 0
          do 160 j=1,6
            nvl = IWHER(j,ilinem,itabsm,ispacm,3,1,nln,nsc,nch)
            do 150 i=1,2
              m = m + 1
              nsc = JWHER(nvl,itabsm,ispacm,i,nch)
              l = NMXDY(iscyrs,m)
              do 140 n=1,l
                k = k + 1
                if (mscrnm(nsc+n-1,nln) .eq. mblnk1.or.
     *              mscrnm(nsc+n-1,nln) .eq. malpha(14).or.
     *              mscrnm(nsc+n-1,nln) .eq. malpha(40)) then
                  iscdys(k) = 0
                else if (mscrnm(nsc+n-1,nln) .eq. malpha(25).or.
     *                   mscrnm(nsc+n-1,nln) .eq. malpha(51)) then
                  iscdys(k) = 1
                else
                  iferrm(nvl,i) = 1
                  iscdys(k) = 1
                end if
  140         continue
  150       continue
  160     continue
          if (nmxdys .eq. 365) then
            if (iscdys(365) .eq. 0) then
              iscdys(366) = 0
            else
              iscdys(366) = 1
            end if
          end if
        end if
        if (ISUMI(iscdys,nmxdys) .eq. 0) then
          do 170 j=1,6
            nvl = IWHER(j,ilinem,itabsm,ispacm,3,1,nln,nsc,nch)
            iferrm(nvl,1) = 2
            iferrm(nvl,2) = 2
  170     continue
        end if
        ner = IFWER(iferrm,nmxlin*nmxfld)
        if (ifwrt .gt. 0.or.ner .gt. 0.or.(ner .eq. 0.and.
     *      ker(7) .ne. 0)) then
CC          rewrite screen data.
          i = IWTSC(numsc+nn,1,mscrnm,ilinem,iferrm)
        end if
        if (ner .gt. 0) then
          if (KERRP(22,0) .ge. 1) go to 190
        end if
        ker(7) = ner
        if ((jfers .eq. 0.and.ker(7) .eq. 1).or.iflg .ne. 0) go to 100
      end if
      jfnxt = 6
      go to 210
  180 jfnxt = 7
      go to 210
  190 jfnxt = 8
      go to 210
  200 jfnxt = 3
  210 return
      end
