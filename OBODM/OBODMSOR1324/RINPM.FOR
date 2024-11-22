      Subroutine RINPM
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - RINPM Version 01.2                          ---
CC--- Subroutine description - displays and reads meteorological    ---
CC---                          data.                                ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      character*1 mslsh
CC
      dimension ker(10)
CC
      include 'OCNTRL.INC'
      include 'OCDSPM.INC'
      include 'OCALCS.INC'
      include 'ONAMES.INC'
      include 'OWNTRL.INC'
CC
      equivalence (jfnxt,functx(1)),(ker(1),functx(2)),
     *            (numsc,functx(37))
CC
      data mslsh/'/'/
CC
CC********************************************************************
CC
CC        screen numsc+2 (24), meteorological data.
   10 nn = 2
CC      read screen menu and screen data.
      numf = IRDSC(numsc+nn,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
CC      set default met. data.
      nvl = IWHER(1,ilinem,itabsm,ispacm,0,2,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
        if (refspd .le. 0.0) refspd = 2.0
CC        default wind speed.
        i = IWTBF(mscrnm(nsc,nln),nch,refspd,1,iflrjs)
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
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
        if (wnddir .lt. 0.0) wnddir = 270.0
CC        default wind direction.
        i = IWTBF(mscrnm(nsc,nln),nch,wnddir,1,iflrjs)
      end if
CC      Is wind direction From or Towards                               051503
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)             051503
      if (ifhrly .le. 0) then                                           051503
        iswopt(34) = 0                                                  051503
      else if (ihrfmt .gt. 1.and.iswopt(34) .eq. 0) then                051503
        iswopt(34) = 1                                                  051503
      end if                                                            051503
      if (iswopt(34) .eq. 0) then                                       051503
        mscrnm(nsc,nln) = malpha(06)                                    051503
      else                                                              051503
        mscrnm(nsc,nln) = malpha(20)                                    051503
      end if                                                            051503
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
        if (istabl .le. 0) istabl = 3
        stblty = istabl
CC        default stability category.
        if (iflrjs .eq. 0) then
          mscrnm(nsc+nch-1,nln) = malpha(istabl)
        else
          mscrnm(nsc,nln) = malpha(istabl)
        end if
      end if
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (ifhrly .gt. 0.and.iscyrs .gt. 0) iniyrs = iscyrs
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0.and.iniyrs .gt. 0) then
        xdumy = iniyrs
        i = IWTBF(mscrnm(nsc,nln),nch,xdumy,0,iflrjs)
      end if
      nsc = JWHER(nvl,itabsm,ispacm,2,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0.and.inimon .gt. 0) then
        xdumy = inimon
        i = IWTBF(mscrnm(nsc,nln),nch,xdumy,0,iflrjs)
      end if
      nsc = JWHER(nvl,itabsm,ispacm,3,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0.and.inidys .gt. 0) then
        xdumy = inidys
        i = IWTBF(mscrnm(nsc,nln),nch,xdumy,0,iflrjs)
      end if
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0.and.injdys .gt. 0) then
        xdumy = injdys
        i = IWTBF(mscrnm(nsc,nln),nch,xdumy,0,iflrjs)
      end if
      nsc = JWHER(nvl,itabsm,ispacm,2,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
        if (inihrs .eq. irnhrs) then
          ifdstm = IDSTM(iniyrs,inimon,inihrs)
        else
          ifdstm = 0
        end if
        call UNITS(mscrnm(nsc,nln),nch,IABS(ifdstm),0,0,1,16)
      end if
      nsc = JWHER(nvl,itabsm,ispacm,3,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0.and.inihrs .ge. 0) then
        xdumy = (inihrs / 100) * 100 + inimin
        i = IWTBF(mscrnm(nsc,nln),nch,xdumy,-4,iflrjs)
      end if
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
        xdumy = geolon
        i = IWTBF(mscrnm(nsc,nln),nch,xdumy,4,iflrjs)
      end if
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
        xdumy = geolat
        i = IWTBF(mscrnm(nsc,nln),nch,xdumy,4,iflrjs)
      end if
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
        xdumy = zondif
        i = IWTBF(mscrnm(nsc,nln),nch,xdumy,0,iflrjs)
      end if
CC      display screen and get user response.
      iferrs = IOSCN(numsc+nn,mscrnm,ilinem,itabsm,ispacm,iferrm,lstrw,
     *               0)
      jfers = iferrs
CC      save screen menu.
      if (iferrs .ne. 10) i = IWTSC(numsc+nn,1,mscrnm,ilinem,iferrm)
CC      if terminate.
      if (iferrs .eq. 1) go to 190
      ker(3) = IFWER(iferrm,nmxlin*nmxfld)
      ifwrt = 0
CC
CC      check user response to second met. menu.
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
        go to 170
      end if
CC      turn off error indicators.
      i = JINIT(iferrm,nmxlin*nmxfld,0,0)
CC
CC      check data.
CC
CC      check wind speed units.
      nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
      ispunt = 0
      jspunt = 0
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        call UNITS(mscrnm(nsc,nln),nch,i,j,0,0,7)
        if (i .lt. 0.or.(j .lt. 0.and.i .ne. 9)) then
          iferrm(nvl,1) = 1
        else
          ispunt = i
          jspunt = j
        end if
      else
CC          default to meters/second.
        ispunt = 0
        jspunt = 0
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
CC      get wind speed.
      nsc = JWHER(nvl,itabsm,ispacm,2,nch)
      refspd = 2.0
      i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
   20 if (i .gt. 0) then
CC        error.
        iferrm(nvl,2) = 1
      else if (i .lt. 0) then
CC        default.
        ifwrt = 1
        refspd = 2.0
        i = IWTBF(mscrnm(nsc,nln),nch,refspd,1,iflrjs)
      else
        ydumy = xdumy * constl(ispunt+1)
        if (ispunt .ne. 9) ydumy = ydumy / constt(jspunt+1)
        if (ydumy .lt. vllimt(6).or.ydumy .gt. vulimt(6)) then
          i = 1
          go to 20
        end if
CC        set wind speed.
        refspd = xdumy
      end if
      sdumy = refspd * constl(ispunt+1)
      if (ispunt .ne. 9) sdumy = sdumy / constt(jspunt+1)
CC
CC      get wind direction.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
   30 if (i .gt. 0) then
CC        error.
        iferrm(nvl,1) = 1
      else if (i .lt. 0) then
CC        default.
        ifwrt = 1
        wnddir = 270.0
        i = IWTBF(mscrnm(nsc,nln),nch,wnddir,1,iflrjs)
      else
        if (xdumy .lt. vllimt(8).or.xdumy .gt. vulimt(8)) then
          i = 1
          go to 30
        end if
CC        set wind direction.
        wnddir = xdumy
      end if
CC      Is wind direction From or Towards                               051503
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)             051503
      if (mscrnm(nsc,nln) .eq. mblnk1.or.IFCHR(mscrnm(nsc,nln),6) .ne.  051503
     *    0) then                                                       051503
        iswopt(34) = 0                                                  051503
      else if (IFCHR(mscrnm(nsc,nln),20) .ne. 0) then                   051503
        iswopt(34) = 1                                                  051503
        if (ifhrly .le. 0) then                                         051503
          iferrm(nvl,1) = 1                                             051503
          iswopt(34) = 0                                                051503
        end if                                                          051503
      else                                                              051503
        iferrm(nvl,1) = 1                                               051503
        iswopt(34) = 0                                                  051503
      end if                                                            051503
CC
CC      see if Pasquill stability cat. or net radiation index specified.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      radinx = rmissc
      istabl = 0
      stblty = istabl
      iswopt(9) = 0
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
CC        get Pasquill stability category (a,b,...) or net radiation
CC        index (4,3,...).
        i = IFNBR(mscrnm(nsc,nln),nch)
        if (i .eq. 0) then
CC          not numeric, see if Pasquill stability.
          if (mscrnm(nsc,nln) .ne. mblnk1) then
            if (mscrnm(nsc+1,nln) .eq. mblnk1) then
              n = nsc
            else
              n = 0
            end if
          else
            n = nsc + 1
          end if
          if (n .gt. 0) then
            i1 = vllimt(18)
            i2 = vulimt(18)
            do 40 j=i1,i2
              if (IFCHR(mscrnm(n,nln),j) .ne. 0) then
CC                set Pasquill stability category.
                istabl = j
                iswopt(9) = 1
                stblty = j
CC                approximate net radiation index.
                radinx = NETRX(istabl,sdumy)
                go to 50
              end if
   40       continue
CC            not recognized.
            iferrm(nvl,1) = 1
   50       continue
          else
            iferrm(nvl,1) = 1
          end if
        else if (i .eq. 1) then
CC          numeric, see if net radiation index.
          j = IRDBF(mscrnm(nsc,nln),nch,xdumy)
   60     if (j .ne. 0) then
            iferrm(nvl,1) = 1
          else
            if (xdumy .lt. vllimt(19).or.xdumy .gt. vulimt(19)) then
              j = 1
              go to 60
            else
CC              set net radiation index.
              radinx = xdumy
CC              set Pasquill stability category.
              istabl = IPSQL(radinx,sdumy)
              stblty = istabl
            end if
          end if
        end if
      end if
      cldcov = 0.0
      iceunt = 0
      cldhgt = 9999.0
CC
CC      get year of met.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
   70   if (i .gt. 0) then
          iferrm(nvl,1) = 1
        else if (i .lt. 0) then
          iniyrs = irnyrs
          xdumy = iniyrs
          i = IWTBF(mscrnm(nsc,nln),nch,xdumy,0,iflrjs)
          ifwrt = 1
        else
          if (xdumy .lt. vllimt(1).or.xdumy .gt. vulimt(1)) then
            i = 1
            go to 70
          end if
          if (xdumy .lt. 50.0) then                                     020797
            xdumy = xdumy + 2000.0
          else if (xdumy .lt. 100.0) then
            xdumy = xdumy + 1900.0
          end if
          iniyrs = xdumy
        end if
      else
        iniyrs = 0
      end if
CC
CC      get month of met.
      nsc = JWHER(nvl,itabsm,ispacm,2,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
   80   if (i .gt. 0) then
          iferrm(nvl,2) = 1
        else if (i .lt. 0) then
          inimon = irnmon
          xdumy = inimon
          i = IWTBF(mscrnm(nsc,nln),nch,xdumy,0,iflrjs)
          ifwrt = 1
        else
          if (xdumy .lt. vllimt(2).or.xdumy .gt. vulimt(2)) then
            i = 1
            if (IFEQU(xdumy,0.0) .ne. 0) go to 80
          end if
          inimon = xdumy
        end if
      else
        inimon = 0
      end if
CC
CC      get day of met.
      nsc = JWHER(nvl,itabsm,ispacm,3,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
   90   if (i .gt. 0) then
          iferrm(nvl,3) = 1
        else if (i .lt. 0) then
          inidys = irndys
          xdumy = inidys
          i = IWTBF(mscrnm(nsc,nln),nch,xdumy,0,iflrjs)
          ifwrt = 1
        else
          if (iniyrs .gt. 0.and.inimon .gt. 0) then
            xn = NMXDY(iniyrs,inimon)
          else
            xn = vulimt(3)
          end if
          if (xdumy .lt. vllimt(3).or.xdumy .gt. xn) then
            i = 1
            if (IFEQU(xdumy,0.0) .ne. 0) go to 90
          end if
          inidys = xdumy
        end if
      else
        inidys = 0
      end if
CC
CC      get julian day of met.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
  100   if (i .gt. 0) then
          iferrm(nvl,1) = 1
        else if (i .lt. 0) then
          injdys = 0
          if (iferrm(nvl-1,1) .eq. 0.and.
     *        inimon .gt. 0.and.iferrm(nvl-1,2) .eq. 0.and.
     *        inidys .gt. 0.and.iferrm(nvl-1,3) .eq. 0) then
            call JULAN(iniyrs,inimon,inidys,injdys)
          else
            injdys = irnjdy
          end if
          xdumy = injdys
          i = IWTBF(mscrnm(nsc,nln),nch,xdumy,0,iflrjs)
          ifwrt = 1
        else
CC        if (MOD(iniyrs,4) .ne. 0) then                                010400
          if (LEAPS(iniyrs) .eq. 0) then                                010400
            xn = 365
          else
            xn = 366
          end if
          if (xdumy .lt. vllimt(5).or.xdumy .gt. xn) then
            i = 1
            if (IFEQU(xdumy,0.0) .ne. 0) go to 100
          end if
          injdys = xdumy
        end if
      else
        injdys = 0
      end if
      if (inimon .gt. 0.and.iferrm(nvl-1,2) .eq. 0.and.inidys .gt. 0
     *    .and.iferrm(nvl-1,3) .eq. 0.and.iferrm(nvl,1) .eq. 0) then
        i = 0
        call JULAN(iniyrs,inimon,inidys,i)
        if (i .ne. injdys) then
          if (injdys .eq. 0) then
            injdys = i
            xdumy = injdys
            i = IWTBF(mscrnm(nsc,nln),nch,xdumy,0,iflrjs)
            ifwrt = 1
          else
            iferrm(nvl,1) = 1
          end if
        end if
      else if (injdys .gt. 0.and.iferrm(nvl,1) .eq. 0) then
        if (iferrm(nvl-1,2)+iferrm(nvl-1,3) .eq. 0.and.inimon .eq. 0
     *      .and.inidys .eq. 0) then
          call JULAN(iniyrs,inimon,inidys,injdys)
          j = IWHER(nvl-1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
          nsc = JWHER(j,itabsm,ispacm,2,nch)
          xdumy = inimon
          i = IWTBF(mscrnm(nsc,nln),nch,xdumy,0,iflrjs)
          nsc = JWHER(j,itabsm,ispacm,3,nch)
          xdumy = inidys
          i = IWTBF(mscrnm(nsc,nln),nch,xdumy,0,iflrjs)
          ifwrt = 1
        end if
      end if
CC
CC      get units of hour (std, dst)
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
      nsc = JWHER(nvl,itabsm,ispacm,2,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        call UNITS(mscrnm(nsc,nln),nch,i,jdumy,0,0,16)
        if (i .lt. 0) then
          iferrm(nvl,2) = 1
        else
          ifdstm = -i
        end if
      else
        if (inihrs .eq. irnhrs.and.iferrm(nvl-1,1)+iferrm(nvl-1,2)+
     *      iferrm(nvl-1,3) .eq. 0) then
          ifdstm = IDSTM(iniyrs,inimon,inidys)
        else
          ifdstm = 0
        end if
        call UNITS(mscrnm(nsc,nln),nch,IABS(ifdstm),jdumy,0,1,16)
        ifwrt = 1
      end if
CC      get hour of met.
      nsc = JWHER(nvl,itabsm,ispacm,3,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
  110   if (i .gt. 0) then
          iferrm(nvl,3) = 1
        else if (i .lt. 0) then
          inihrs = irnhrs
          xdumy = inihrs
          i = IWTBF(mscrnm(nsc,nln),nch,xdumy,-4,iflrjs)
          ifwrt = 1
        else
          ydumy = xdumy
          if (xdumy .lt. 100.0) xdumy = xdumy * 100.0
          if (xdumy .lt. 0.0.or.xdumy .ge. 2500.0) then
            i = 1
            go to 110
          end if
          inihrs = xdumy
          if (ydumy .lt. xdumy) then
            i = IWTBF(mscrnm(nsc,nln),nch,xdumy,-4,iflrjs)
            ifwrt = 1
          end if
          jhr = (inihrs / 100) * 100
          inimin = inihrs - jhr
          inihrs = jhr
        end if
      else
        inihrs = -1
      end if
CC
CC      get west longitude.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
  120 if (i .gt. 0) then
        iferrm(nvl,1) = 1
      else if (i .eq. 0) then
        if (xdumy .lt. -180.0.or.xdumy .gt. 180.0) then
          i = 1
          go to 120
        end if
CC        set west longitude.
        geolon = xdumy
      else
        if (i .eq. -1.or.i .eq. -3) then
          geolon = rmissd
        else if (i .eq. -2) then
          geolon = 112.8958
        end if
        i = IWTBF(mscrnm(nsc,nln),nch,geolon,5,iflrjs)
        ifwrt = 1
      end if
      if (geolon .ge. -180.0) then
        iang = geolon / 15.0
      else
        iang = 0
      end if
CC
CC      get north latitude.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
  130 if (i .gt. 0) then
        iferrm(nvl,2) = 1
      else if (i .eq. 0) then
        if (xdumy .lt. -90.0.or.xdumy .gt. 90.0) then
          i = 1
          go to 130
        end if
CC        set north latitude.
        geolat = xdumy
      else
        if (i .eq. -1.or.i .eq. -3) then
          geolat = rmissd
        else if (i .eq. -2) then
          geolat = 40.1667
        end if
        i = IWTBF(mscrnm(nsc,nln),nch,geolat,5,iflrjs)
        ifwrt = 1
      end if
CC
CC      get time zone difference between site and Greenwich England.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
  140 if (i .gt. 0) then
        iferrm(nvl,1) = 1
      else if (i .eq. 0) then
        if (xdumy .lt. -12.0.or.xdumy .gt. 12.0) then
          i = 1
          go to 140
        end if
CC        set time zone difference.
        zondif = xdumy
      else
        if (i .eq. -1.or.i .eq. -3) then
          if (ABS(geolon) .ge. 0.0.and.ABS(geolon) .le. 180.0) then
            zondif = iang
          else
            zondif = rmissd
          end if
        else if (i .eq. -2) then
          zondif = 7.0
        end if
        i = IWTBF(mscrnm(nsc,nln),nch,zondif,1,iflrjs)
        ifwrt = 1
      end if
      if (zondif .le. 0.0.and.ifdstm .ne. 0) then
          if (KERRS(65,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *        go to 190
      end if
CC
CC      cloud cover, 8'ths of celestial dome.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
  150 if (i .gt. 0) then
        iferrm(nvl,1) = 1
      else if (i .eq. 0) then
        if (xdumy .lt. vllimt(24).or.xdumy .gt. vulimt(24)) then
          i = 1
          go to 150
        end if
CC        cloud cover.
        cldcov = xdumy
      else if (cldcov .ge. 0.0) then
        i = IWTBF(mscrnm(nsc,nln),nch,cldcov,0,iflrjs)
        ifwrt = 1
      end if
CC
CC      get cloud ceiling units.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        call UNITS(mscrnm(nsc,nln),nch,i,jdumy,0,0,0)
        if (i .lt. 0) then
          iferrm(nvl,1) = 1
        else
          iceunt = i
        end if
      else
        if (iceunt .lt. 0) iceunt = 0
        call UNITS(mscrnm(nsc,nln),nch,iceunt,jdumy,0,1,0)
        ifwrt = 1
      end if
CC
CC      get cloud ceiling.
      nsc = JWHER(nvl,itabsm,ispacm,2,nch)
      i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
  160 if (i .gt. 0) then
        iferrm(nvl,2) = 1
      else if (i .eq. 0) then
        if (xdumy .lt. vllimt(15).or.xdumy .gt. vulimt(25)) then
          i = 1
          go to 160
        end if
CC        cloud ceiling.
        cldhgt = xdumy
      else if (cldhgt .ge. 0.0) then
        i = IWTBF(mscrnm(nsc,nln),nch,cldhgt,1,iflrjs)
        ifwrt = 1
      end if
      if (istabl .le. 0) then
        if (inihrs .ge. 0.and.injdys .gt. 0.and.zondif .ge. -12.0.and.
     *      geolon .ge. -180.0.and.geolat .ge. -90.0) then
CC          determine sun angle.
          snang = SUNAN(inihrs,inimin,zondif,ifdstm,injdys,geolon,
     *                  geolat)
          xdumy = cldhgt * constl(iceunt+1)
          radinx = IRNDX(cldcov,xdumy,snang)
          istabl = IPSQL(radinx,sdumy)
          stblty = istabl
        end if
      end if
      if (istabl .eq. 0) then
        if (sdumy .lt. 1.0) then
          istabl = 1
        else if (sdumy .lt. 3.0) then
          istabl = 2
        else if (sdumy .lt. 5.0) then
          istabl = 3
        else if (sdumy .lt. 7.0) then
          istabl = 4
        else
          istabl = 5
        end if
        stblty = istabl
        radinx = NETRX(istabl,sdumy)
      end if
      ner = IFWER(iferrm,nmxlin*nmxfld)
      if (ifwrt .gt. 0.or.ner .gt. 0.or.(ner .eq. 0.and.ker(3) .gt. 0))
     *    then
CC        rewrite screen data.
        i = IWTSC(numsc+nn,1,mscrnm,ilinem,iferrm)
      end if
      if (ner .gt. 0) then
        if (KERRP(22,0) .ge. 1) go to 190
      end if
      ker(3) = ner
      if (jfers .eq. 0.and.ker(3) .eq. 1) go to 10
CC
      jfnxt = 3
      go to 200
  170 jfnxt = 1
      go to 200
  180 jfnxt = 7
      go to 200
  190 jfnxt = 8
  200 return
      end
