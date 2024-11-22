      Subroutine RINPN
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - RINPN Version 01.1                          ---
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
CC
CC      screen numsc+3 (25), meteorological data.
   10 nn = 3
CC      read screen menu and screen data.
      numf = IRDSC(numsc+nn,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
CC
CC      set default met. data.
CC
CC      check elevation of met. data.
      nvl = IWHER(1,ilinem,itabsm,ispacm,0,2,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
        hgtmet = clddzs(1)
        ihunit = IVGET(isrect(1),1)
        i = IWTBF(mscrnm(nsc,nln),nch,hgtmet,1,iflrjs)
        nsc = JWHER(nvl,itabsm,ispacm,1,nch)
        call UNITS(mscrnm(nsc,nln),nch,ihunit,0,0,1,0)
      end if
CC
CC      check wind speed power law coefficient.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
        pwrlaw = rmissc
        if (iflrjs .eq. 0) then
          mscrnm(nsc+nch-1,nln) = malpha(3)
        else
          mscrnm(nsc,nln) = malpha(3)
        end if
        i = IWTBF(mscrnm(nsc,nln),nch,pwrlaw,4,iflrjs)
      end if
CC      check sigmap measurement time.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,2,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
        tauosa = 600.0
        istunt = 0
        i = IWTBF(mscrnm(nsc,nln),nch,tauosa,1,iflrjs)
        nsc = JWHER(nvl,itabsm,ispacm,1,nch)
        call MBLK1(mscrnm(nsc,nln),nch)
        if (iflrjs .eq. 0) then
          mscrnm(nsc+nch-1,nln) = malpha(45)
        else
          mscrnm(nsc,nln) = malpha(45)
        end if
      end if
CC      check sigmap.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,2,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
        sigmap = rmissc
        isyunt = 0
        if (iflrjs .eq. 0) then
          mscrnm(nsc+nch-1,nln) = malpha(3)
        else
          mscrnm(nsc,nln) = malpha(3)
        end if
        nsc = JWHER(nvl,itabsm,ispacm,1,nch)
        mscrnm(nsc,nln) = malpha(30)
      end if
CC      check sigmep.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,2,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
        sigmep = rmissc
        iszunt = 0
        if (iflrjs .eq. 0) then
          mscrnm(nsc+nch-1,nln) = malpha(3)
        else
          mscrnm(nsc,nln) = malpha(3)
        end if
        nsc = JWHER(nvl,itabsm,ispacm,1,nch)
        mscrnm(nsc,nln) = malpha(30)
      end if
CC      check longitudinal turbulent intensity.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,2,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
        trbixr = rmissc
        isxunt = 0
        if (iflrjs .eq. 0) then
          mscrnm(nsc+nch-1,nln) = malpha(3)
        else
          mscrnm(nsc,nln) = malpha(3)
        end if
        nsc = JWHER(nvl,itabsm,ispacm,1,nch)
        mscrnm(nsc,nln) = malpha(30)
      end if
CC      check mixing layer depth.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,2,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
        hmdpth = rmissc
        ihmunt = 0
        if (iflrjs .eq. 0) then
          mscrnm(nsc+nch-1,nln) = malpha(3)
        else
          mscrnm(nsc,nln) = malpha(3)
        end if
        nsc = JWHER(nvl,itabsm,ispacm,1,nch)
        call MBLK1(mscrnm(nsc,nln),nch)
        if (iflrjs .eq. 0) then
          mscrnm(nsc+nch-1,nln) = malpha(39)
        else
          mscrnm(nsc,nln) = malpha(39)
        end if
      end if
CC      check air humidity.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
        airhum = 50.0
        i = IWTBF(mscrnm(nsc,nln),nch,airhum,2,iflrjs)
      end if
CC      check vertical gradient of potential temperature.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,2,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
        dphidz = rmissc
        ivgunt = 0
        jvgunt = 0
        if (iflrjs .eq. 0) then
          mscrnm(nsc+nch-1,nln) = malpha(3)
        else
          mscrnm(nsc,nln) = malpha(3)
        end if
        nsc = JWHER(nvl,itabsm,ispacm,1,nch)
        call MBLK1(mscrnm(nsc,nln),nch)
        if (iflrjs .eq. 0) then
          mscrnm(nsc+nch-3,nln) = malpha(3)
          mscrnm(nsc+nch-2,nln) = mslsh
          mscrnm(nsc+nch-1,nln) = malpha(39)
        else
          mscrnm(nsc,nln) = malpha(3)
          mscrnm(nsc+1,nln) = mslsh
          mscrnm(nsc+2,nln) = malpha(39)
        end if
      end if
CC      display screen and get user response.
      iferrs = IOSCN(numsc+nn,mscrnm,ilinem,itabsm,ispacm,iferrm,lstrw,
     *                0)
      jfers = iferrs
CC      save screen menu.
      if (iferrs .ne. 10) i = IWTSC(numsc+nn,1,mscrnm,ilinem,iferrm)
CC      if terminate.
      if (iferrs .eq. 1) go to 120
      ker(4) = IFWER(iferrm,nmxlin*nmxfld)
      jfers = iferrs
      ifwrt = 0
CC
CC      check user response to third met. menu.
      if (jfers .eq. 2) then
CC        main menu.
        go to 110
      else if (jfers .ge. 3.and.jfers .le. 7) then
CC        display aux. menus.
        iferrs = IFRSP(jfers,mscrnm,ilinem,itabsm,ispacm,iferrm,0)
        if (iferrs .eq. 1.or.iferrs .eq. 2) then
CC          terminate or main menu.
          jfers = iferrs
          go to 110
        else
CC          redisplay menu.
          go to 10
        end if
      else if (jfers .ge. 8.and.jfers .le. 10) then
        go to 10
      else if (jfers .eq. 11) then
        go to 100
      end if
CC      turn off error indicators for this menu.
      i = JINIT(iferrm,nmxlin*nmxfld,0,0)
CC
CC      check data.
CC
CC      get met. site elevation units
      nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        call UNITS(mscrnm(nsc,nln),nch,i,j,0,0,0)
        if (i .lt. 0) then
          iferrm(nvl,1) = 1
        else
          ihunit = i
        end if
      else
CC          default to meters.
        ihunit = IVGET(isrect(1),1)
        ifwrt = 1
        call UNITS(mscrnm(nsc,nln),nch,ihunit,0,0,1,0)
      end if
CC
CC      get met. site elevation.
      nsc = JWHER(nvl,itabsm,ispacm,2,nch)
      i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
      if (i .gt. 0) then
CC        error.
        iferrm(nvl,2) = 1
      else if (i .lt. 0) then
CC        if 'x','c' or blank, default.
        hgtmet = clddzs(1)
        i = IWTBF(mscrnm(nsc,nln),nch,hgtmet,1,iflrjs)
        ifwrt = 1
      else
CC        set met. elevation.
        hgtmet = xdumy
      end if
CC
CC      get wind speed power law coefficient.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
   20 if (i .gt. 0) then
CC        error.
        iferrm(nvl,1) = 1
      else if (i .lt. 0) then
        if (i .eq. -1) then
          pwrlaw = DFMET(1,radinx,istabl,sdumy,0.0)
          ifwrt = 2
        else
          pwrlaw = rmissc
          ifwrt = 1
        end if
        i = IWTBF(mscrnm(nsc,nln),nch,pwrlaw,1,iflrjs)
      else
        if (xdumy .lt. vllimt(7).or.xdumy .gt. vulimt(7)) then
          i = 1
          go to 20
        end if
CC        set wind speed power law coefficient.
        pwrlaw = xdumy
      end if
CC
CC      get units of std. dev. of the wind direction measurement time.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        call UNITS(mscrnm(nsc,nln),nch,i,jdumy,0,0,5)
        if (i .lt. 0) then
          iferrm(nvl,1) = 1
        else
          istunt = i
        end if
      else
CC          default to seconds.
        istunt = 0
        ifwrt = 1
        call MBLK1(mscrnm(nsc,nln),nch)
        if (iflrjs .eq. 0) then
          mscrnm(nsc+nch-2,nln) = mblnk1
          mscrnm(nsc+nch-1,nln) = malpha(45)
        else
          mscrnm(nsc,nln) = malpha(45)
          mscrnm(nsc+1,nln) = mblnk1
        end if
      end if
CC
CC      get measurement time of std. dev. of the wind direction angle.
      nsc = JWHER(nvl,itabsm,ispacm,2,nch)
      i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
   30 if (i .gt. 0) then
CC        error.
        iferrm(nvl,2) = 1
      else if (i .lt. 0) then
CC        if 'x' ,'c' or blank, default.
        tauosa = 600.0
        i = IWTBF(mscrnm(nsc,nln),nch,tauosa,1,iflrjs)
        nsc = JWHER(nvl,itabsm,ispacm,1,nch)
        call MBLK1(mscrnm(nsc,nln),nch)
        if (iflrjs .eq. 0) then
          mscrnm(nsc+nch-2,nln) = mblnk1
          mscrnm(nsc+nch-1,nln) = malpha(45)
        else
          mscrnm(nsc,nln) = malpha(45)
          mscrnm(nsc+1,nln) = mblnk1
        end if
        ifwrt = 1
      else
        ydumy = xdumy * constt(istunt+1)
        if (ydumy .lt. vllimt(20).or.ydumy .gt. vulimt(20)) then
          i = 1
          go to 30
        end if
CC        set measurement time of the std. dev. wind direction angle.
        tauosa = xdumy
      end if
CC
CC      get units of standard deviation of the wind direction angle.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (mscrnm(nsc,nln) .ne. mblnk1) then
        call UNITS(mscrnm(nsc,nln),nch,i,jdumy,0,0,1)
        if (i .lt. 0) then
          iferrm(nvl,1) = 1
        else
          isyunt = i
        end if
      else
CC          default to degrees.
        isyunt = 0
        ifwrt = 1
        mscrnm(nsc,nln) = malpha(30)
      end if
CC
CC      get standard deviation of the wind direction angle.
      nsc = JWHER(nvl,itabsm,ispacm,2,nch)
      i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
   40 if (i .gt. 0) then
CC        error.
        iferrm(nvl,2) = 1
        iflg = 0
      else if (i .lt. 0) then
CC        if 'x' ,'c' or blank, default.
        if (i .eq. -1) then
          sigmap = DFMET(3,radinx,istabl,sdumy,0.0)
          sigmap = PWLAW(sigmap,tauosa,tautmo,0.2)
          sigmap = sigmap * constg(isyunt+1)
          ifwrt = 2
          iflg = 2
        else
          sigmap = rmissc
          ifwrt = 1
          iflg = 1
        end if
        i = IWTBF(mscrnm(nsc,nln),nch,sigmap,1,iflrjs)
        nsc = JWHER(nvl,itabsm,ispacm,1,nch)
        mscrnm(nsc,nln) = malpha(30)
      else
        ydumy = xdumy
        if (isyunt .ne. 0) ydumy = xdumy * radtod
        if (ydumy .lt. vllimt(9).or.ydumy .gt. vulimt(9)) then
          i = 1
          go to 40
        end if
CC        set std. dev. wind direction angle.
        sigmap = xdumy
        iflg = 3
      end if
CC
CC      get units of standard deviation of the wind elevation angle.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (mscrnm(nsc,nln) .ne. mblnk1) then
        call UNITS(mscrnm(nsc,nln),nch,i,jdumy,0,0,1)
        if (i .lt. 0) then
          iferrm(nvl,1) = 1
        else
          iszunt = i
        end if
      else
CC          default to degrees.
        iszunt = 0
        ifwrt = 1
        mscrnm(nsc,nln) = malpha(30)
      end if
CC
CC      get standard deviation of the wind elevation angle.
      nsc = JWHER(nvl,itabsm,ispacm,2,nch)
      i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
   50 if (i .gt. 0) then
CC        error.
        iferrm(nvl,2) = 1
      else if (i .lt. 0) then
CC        if 'x' ,'c' or blank, default.
        if (i .eq. -1) then
          sigmep = DFMET(2,radinx,istabl,sdumy,0.0)
          sigmep = sigmep * constg(iszunt+1)
          ifwrt = 2
        else
          sigmep = rmissc
          ifwrt = 1
        end if
        i = IWTBF(mscrnm(nsc,nln),nch,sigmep,1,iflrjs)
        nsc = JWHER(nvl,itabsm,ispacm,1,nch)
        mscrnm(nsc,nln) = malpha(30)
      else
        ydumy = xdumy
        if (iszunt .ne. 0) ydumy = xdumy * radtod
        if (ydumy .lt. vllimt(10).or.ydumy .gt. vulimt(10)) then
          i = 1
          go to 50
        end if
CC        set std. dev. wind elevation angle.
        sigmep = xdumy
      end if
CC
CC      get units of standard deviation of the wind longitudinal angle.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (mscrnm(nsc,nln) .ne. mblnk1) then
        call UNITS(mscrnm(nsc,nln),nch,i,jdumy,0,0,1)
        if (i .lt. 0) then
          iferrm(nvl,1) = 1
        else
          isxunt = i
        end if
      else
CC          default to degrees.
        isxunt = 0
        ifwrt = 1
        mscrnm(nsc,nln) = malpha(30)
      end if
CC
CC      get standard deviation of the wind longitudinal angle.
      nsc = JWHER(nvl,itabsm,ispacm,2,nch)
      i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
   60 if (i .gt. 0) then
CC        error.
        iferrm(nvl,2) = 1
      else if (i .lt. 0) then
CC        if 'x' ,'c' or blank, default.
        if (i .eq. -1) then
          if (iflg .eq. 3) then
            trbixr = 1.33 * sigmap
            if (isyunt .ne. isxunt) then
              if (isyunt .eq. 0) then
                trbixr = trbixr / constg(2)
              else
                trbixr = trbixr * constg(2)
              end if
            end if
          else
            trbixr = DFMET(6,radinx,istabl,sdumy,0.0)
            trbixr = PWLAW(trbixr,tauosa,tautmo,0.2)
            trbixr = trbixr * constg(isxunt+1)
          end if
          ifwrt = 2
        else
          trbixr = rmissc
          ifwrt = 1
        end if
        i = IWTBF(mscrnm(nsc,nln),nch,trbixr,1,iflrjs)
        nsc = JWHER(nvl,itabsm,ispacm,1,nch)
        mscrnm(nsc,nln) = malpha(30)
      else
        ydumy = xdumy
        if (isxunt .ne. 0) ydumy = xdumy * radtod
        if (ydumy .lt. vllimt(11).or.ydumy .gt. vulimt(11)) then
          i = 1
          go to 60
        end if
CC        set std. dev. wind longitudinal angle.
        trbixr = xdumy
      end if
CC
CC      get units of mixing layer depth.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        call UNITS(mscrnm(nsc,nln),nch,i,jdumy,0,0,0)
        if (i .lt. 0) then
          iferrm(nvl,1) = 1
        else
          ihmunt = i
        end if
      else
CC          default to meters.
        ihmunt = 0
        ifwrt = 1
        call MBLK1(mscrnm(nsc,nln),nch)
        if (iflrjs .eq. 0) then
          mscrnm(nsc+nch-1,nln) = malpha(39)
        else
          mscrnm(nsc,nln) = malpha(39)
        end if
      end if
CC
CC      get mixing layer depth.
      nsc = JWHER(nvl,itabsm,ispacm,2,nch)
      i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
   70 if (i .gt. 0) then
CC        error.
        iferrm(nvl,2) = 1
      else if (i .lt. 0) then
CC        if 'x' ,'c' or blank, default.
        if (i .eq. -1) then
          hmdpth = DFMET(4,radinx,istabl,sdumy,0.0)
          ifwrt = 2
        else
          hmdpth = rmissc
          ifwrt = 1
        end if
        i = IWTBF(mscrnm(nsc,nln),nch,hmdpth,1,iflrjs)
        nsc = JWHER(nvl,itabsm,ispacm,1,nch)
        call MBLK1(mscrnm(nsc,nln),nch)
        if (iflrjs .eq. 0) then
          mscrnm(nsc+nch-1,nln) = malpha(39)
        else
          mscrnm(nsc,nln) = malpha(39)
        end if
      else
        ydumy = xdumy * constl(ihmunt+1)
        if (ydumy .lt. vllimt(15).or.ydumy .gt. vulimt(15)) then
          i = 1
          go to 70
        end if
CC        set mixing layer depth.
        hmdpth = xdumy
      end if
CC      get humidity.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
   80 if (i .gt. 0) then
CC        error.
        iferrm(nvl,1) = 1
      else if (i .lt. 0) then
CC        if 'x', 'c' or blank, default.
        airhum = 50.0
        i = IWTBF(mscrnm(nsc,nln),nch,airhum,2,iflrjs)
        ifwrt = 1
      else
        if (xdumy .gt. 0.0.and.xdumy .le. 1.0) xdumy = xdumy * 100.0
        if (xdumy .lt. vllimt(22).or.xdumy .gt. vulimt(22)) then
          i = 1
          go to 80
        end if
        airhum = xdumy
      end if
CC
CC      get units of vertical gradient of potential temperature.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        call UNITS(mscrnm(nsc,nln),nch,i,j,0,0,15)
        if (i .lt. 0.or.j .lt. 0) then
          iferrm(nvl,1) = 1
        else
          ivgunt = i
          jvgunt = j
        end if
      else
CC          default to degrees c/meter.
        ivgunt = 0
        jvgunt = 0
        ifwrt = 1
        call MBLK1(mscrnm(nsc,nln),nch)
        if (iflrjs .eq. 0) then
          mscrnm(nsc+nch-3,nln) = malpha(3)
          mscrnm(nsc+nch-2,nln) = mslsh
          mscrnm(nsc+nch-1,nln) = malpha(39)
        else
          mscrnm(nsc,nln) = malpha(3)
          mscrnm(nsc+1,nln) = mslsh
          mscrnm(nsc+2,nln) = malpha(39)
        end if
      end if
CC
CC      get vertical gradient of potential temperature.
      nsc = JWHER(nvl,itabsm,ispacm,2,nch)
      i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
   90 if (i .gt. 0) then
CC        error.
        iferrm(nvl,2) = 1
      else if (i .lt. 0) then
CC        if 'x' ,'c' or blank, default.
        if (i .eq. -1) then
          dphidz = DFMET(5,radinx,istabl,sdumy,airhum) *
     *             constl(jvgunt+1) / constc(ivgunt+1,2)
          ifwrt = 2
        else
          dphidz = rmissc
          ifwrt = 1
        end if
        i = IWTBF(mscrnm(nsc,nln),nch,dphidz,4,iflrjs)
        nsc = JWHER(nvl,itabsm,ispacm,1,nch)
        call MBLK1(mscrnm(nsc,nln),nch)
        if (iflrjs .eq. 0) then
          mscrnm(nsc+nch-3,nln) = malpha(3)
          mscrnm(nsc+nch-2,nln) = mslsh
          mscrnm(nsc+nch-1,nln) = malpha(39)
        else
          mscrnm(nsc,nln) = malpha(3)
          mscrnm(nsc+1,nln) = mslsh
          mscrnm(nsc+2,nln) = malpha(39)
        end if
      else
        ydumy = constc(ivgunt+1,2) * xdumy / constl(jvgunt+1)
        if (ydumy .lt. vllimt(14).or.ydumy .gt. vulimt(14)) then
          i = 1
          go to 90
        end if
CC        set vertical gradient of potential temperature.
        dphidz = xdumy
      end if
      ner = IFWER(iferrm,nmxlin*nmxfld)
      if (ifwrt .gt. 0.or.ner .gt. 0.or.(ner .eq. 0.and.ker(4) .ne. 0))
     *    then
CC        rewrite menu data.
        i = IWTSC(numsc+nn,1,mscrnm,ilinem,iferrm)
      end if
      if (ner .gt. 0) then
        if (KERRP(22,0) .ge. 1) go to 120
      end if
      ker(4) = ner
      if (jfers .eq. 0.and.(ker(4) .eq. 1.or.ifwrt .eq. 2)) go to 10
      jfnxt = 4
      go to 130
  100 jfnxt = 2
      go to 130
  110 jfnxt = 7
      go to 130
  120 jfnxt = 8
  130 return
      end
