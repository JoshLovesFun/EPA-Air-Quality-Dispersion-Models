      Subroutine RINPL
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - RINPL Version 01.3                          --- 072497
CC--- Subroutine description - displays and reads meteorological    ---
CC---                          data.                                ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
CC
      dimension ker(10),ipt(13,2),npt(13,2),vdl(25),vdu(25)             020797
CC
      include 'OCNTRL.INC'
      include 'OCDSPM.INC'
      include 'OCALCS.INC'
      include 'ONAMES.INC'
      include 'OWNTRL.INC'
CC
      equivalence (jfnxt,functx(1)),(ker(1),functx(2)),
     *            (numsc,functx(37))                                    020797
CC
      data ipt/ 0, 0, 0, 4, 6, 8, 0, 7, 0, 9, 0,10,11,
     *         13,14,15,16, 0,17,12,22,20,23,21, 0, 0/,
     *     npt/ 0, 0, 0,-4, 2, 1, 0, 3, 0, 2, 0, 2, 2,
     *          1, 4, 1, 2, 0, 1, 1, 1, 1, 1, 2, 0, 0/,
     *     vdl/1900.0,1.0,1.0,0.0,1.0,1.0,0.0,0.0,1.0,1.0,1.0,-60.0,    072497
     *         600.0,-5.0,1.0,0.0,-45.0,1.0,-2.0,2.5,0.0,0.0,0.0,
     *         0.0,0.0/,
     *     vdu/2099.0,12.0,31.0,2300.0,366.0,50.0,5.0,360.0,80.0,
     *         50.0,106.4,60.0,1100.0,5.0,20000.,20.0,45.0,6.0,4.0,
     *         3600.0,100.0,100.0,20000.,8.0,20000./                    
CC
CC********************************************************************
CC
      ifnext = 3
CC      start screen number (22)
      numsc = numscs(6)
      i = JINIT(ker,10,0,0)
CC
CC      display and read data for screens numsc to numsc+9.
CC
CC      screen numsc, start of meteorological data.
   10 numf = IRDSC(numsc,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
      nvl = IWHER(0,ilinem,itabsm,ispacm,2,1,nln,nsc,nch)
      call MBLK1(mscrnm(nsc,nln),nch)
      if (iflrjs .eq. 0) then
        mscrnm(nsc+nch-1,nln) = malpha(14)
      else
        mscrnm(nsc,nln) = malpha(14)
      end if
CC        display screen and get user response.
      iferrs = IOSCN(numsc,mscrnm,ilinem,itabsm,ispacm,iferrm,lstrw,0)
      jfers = iferrs
CC      save screen menu.
      if (iferrs .ne. 10) i = IWTSC(numsc,1,mscrnm,ilinem,iferrm)
CC      if terminate.
      if (iferrs .eq. 1) go to 60
      ker(1) = IFWER(iferrm,nmxlin*nmxfld)
      ifwrt = 0
CC
CC      check user response to first met. menu.
      if (jfers .eq. 2.or.jfers .eq. 11) then
CC        main menu.
        go to 50
      else if (jfers .ge. 3.and.jfers .le. 7) then
CC        display aux. menus.
        iferrs = IFRSP(jfers,mscrnm,ilinem,itabsm,ispacm,iferrm,0)
        if (iferrs .eq. 1.or.iferrs .eq. 2) then
CC          terminate or main menu.
          jfers = iferrs
          go to 50
        end if
      else if (jfers .ge. 8.and.jfers .le. 10) then
CC        n/a.
        go to 10
      end if
      i = JINIT(iferrm,nmxlin*nmxfld,0,0)
CC
CC      see if single obs. or hourly input.
      nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
      if (mscrnm(nsc,nln) .ne. mblnk1) then
        if (IFCHR(mscrnm(nsc,nln),19) .ne. 0) then
          ifhrly = 0
          ifmode = 0
        else if (IFCHR(mscrnm(nsc,nln),8) .ne. 0) then
          ifhrly = 1
        else
          iferrm(nvl,1) = 1
        end if
      else
        ifhrly = 0
        ifmode = 0
      end if
CC      see if users wants to set met. parameter limits.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (IFCHR(mscrnm(nsc,nln),25) .ne. 0) then
        isetd = 1
      else
        isetd = 0
      end if
      ner = IFWER(iferrm,nmxlin*nmxfld)
      if (ifwrt .gt. 0.or.ner .gt. 0.or.(ner .eq. 0.and.ker(1) .gt. 0))
     *    then
CC        write screen data.
        i = IWTSC(numsc,1,mscrnm,ilinem,iferrm)
      end if
      if (ner .gt. 0) then
        if (KERRP(22,0) .ge. 1) go to 60
      end if
      ker(1) = ner
      if (jfers .eq. 0.and.ker(1) .eq. 1) go to 10
   20 if (isetd .gt. 0) then
CC
CC        screen numsc+1 (23), meteorological data limits.
        nn = 1
CC        read screen menu and screen data.
        numf = IRDSC(numsc+nn,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
CC        show current values.
        nvl = 0
        do 30 k=1,13
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          j = 1
          if (ipt(k,1) .gt. 0) then
            j = 3
            if (k .eq. 4) then
              if (vulimt(ipt(k,1)) .ge. 100.0) then
                npt(k,1) = -4
              else
                npt(k,1) = -2
              end if
            end if
            i = IWTBF(mscrnm(nsc,nln),nch,vllimt(ipt(k,1)),npt(k,1),
     *                iflrjs)
            nsc = JWHER(nvl,itabsm,ispacm,2,nch)
            i = IWTBF(mscrnm(nsc,nln),nch,vulimt(ipt(k,1)),npt(k,1),
     *                iflrjs)
          end if
          if (ipt(k,2) .gt. 0) then
            if (j .ne. 1) nsc = JWHER(nvl,itabsm,ispacm,j,nch)
            i = IWTBF(mscrnm(nsc,nln),nch,vllimt(ipt(k,2)),npt(k,2),
     *                iflrjs)
            nsc = JWHER(nvl,itabsm,ispacm,j+1,nch)
            i = IWTBF(mscrnm(nsc,nln),nch,vulimt(ipt(k,2)),npt(k,2),
     *                iflrjs)
          end if
   30   continue
CC        display screen and get user response.
        iferrs = IOSCN(numsc+nn,mscrnm,ilinem,itabsm,ispacm,iferrm,
     *                 lstrw,0)
        jfers = iferrs
CC        save screen menu.
        if (iferrs .ne. 10) i = IWTSC(numsc+nn,1,mscrnm,ilinem,iferrm)
CC        if terminate.
        if (iferrs .eq. 1) go to 60
        ker(2) = IFWER(iferrm,nmxlin*nmxfld)
        ifwrt = 0
CC
CC        check user response to second met. menu.
        if (jfers .eq. 2) then
CC          main menu.
          go to 50
        else if (jfers .ge. 3.and.jfers .le. 7) then
CC          display aux. menus.
          iferrs = IFRSP(jfers,mscrnm,ilinem,itabsm,ispacm,iferrm,0)
          if (iferrs .eq. 1.or.iferrs .eq. 2) then
CC            terminate or main menu.
            jfers = iferrs
            go to 50
          else
CC            redisplay menu.
            go to 20
          end if
        else if (jfers .ge. 8.and.jfers .le. 10) then
          go to 20
        else if (jfers .eq. 11) then
          go to 10
        end if
CC        turn off error indicators.
        i = JINIT(iferrm,nmxlin*nmxfld,0,0)
CC
CC        check data.
CC
        nvl = 0
        do 40 k=1,13
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          j = 1
          if (ipt(k,1) .gt. 0) then
            j = 3
            i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
            if (i .gt. 0) then
CC              error.
              iferrm(nvl,1) = 1
            else if (i .lt. 0) then
CC              default.
              vllimt(ipt(k,1)) = vdl(ipt(k,1))
              i = IWTBF(mscrnm(nsc,nln),nch,vllimt(ipt(k,1)),npt(k,1),
     *                  iflrjs)
            else
              if (k .eq. 4) then
                if (xdumy .lt. 100.0) xdumy = xdumy * 100.0
                i = xdumy
                if (i .ne. 0.and.i .ne. 100) then
                  iferrm(nvl,1) = 1
                else
                  vllimt(ipt(k,1)) = xdumy
                  if (xdumy .le. 0.0) then
                    vulimt(ipt(k,1)) = 2300.0
                  else
                    vulimt(ipt(k,1)) = 2400.0
                  end if
                end if
              else                                                      092904
                  vllimt(ipt(k,1)) = xdumy                              092904
              end if
            end if
            nsc = JWHER(nvl,itabsm,ispacm,2,nch)
            i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
            if (i .gt. 0) then
CC              error.
              iferrm(nvl,2) = 1
            else if (i .lt. 0) then
CC              default.
              vulimt(ipt(k,1)) = vdu(ipt(k,1))
              i = IWTBF(mscrnm(nsc,nln),nch,vulimt(ipt(k,1)),npt(k,1),
     *                  iflrjs)
            else
              if (k .eq. 4) then
                if (xdumy .lt. 100.0) xdumy = xdumy * 100.0
                i = xdumy
                if (i .ne. 2300.and.i .ne. 2400) then
                  iferrm(nvl,2) = 1
                else
                  vulimt(ipt(k,1)) = xdumy
                  if (i .eq. 2300) then
                    vllimt(ipt(k,1)) = 0.0
                  else
                    vllimt(ipt(k,1)) = 100.0
                  end if
                end if
              else                                                      092904
                  vulimt(ipt(k,1)) = xdumy                              092904
              end if
            end if
          end if
          if (ipt(k,2) .gt. 0) then
            if (j .ne. 1) nsc = JWHER(nvl,itabsm,ispacm,j,nch)
            i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
            if (i .gt. 0) then
CC              error.
              iferrm(nvl,3) = 1
            else if (i .lt. 0) then
CC              default.
              vllimt(ipt(k,2)) = vdl(ipt(k,2))
              i = IWTBF(mscrnm(nsc,nln),nch,vllimt(ipt(k,2)),npt(k,2),
     *                  iflrjs)
            else
              vllimt(ipt(k,2)) = xdumy
            end if
            nsc = JWHER(nvl,itabsm,ispacm,j+1,nch)
            i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
            if (i .gt. 0) then
CC              error.
              iferrm(nvl,4) = 1
            else if (i .lt. 0) then
CC              default.
              vulimt(ipt(k,2)) = vdu(ipt(k,2))
              i = IWTBF(mscrnm(nsc,nln),nch,vulimt(ipt(k,2)),npt(k,2),
     *                  iflrjs)
            else
              vulimt(ipt(k,2)) = xdumy
            end if
          end if
   40   continue
        ner = IFWER(iferrm,nmxlin*nmxfld)
CC        rewrite screen data.
        i = IWTSC(numsc+nn,1,mscrnm,ilinem,iferrm)
        if (ner .gt. 0) then
          if (KERRP(22,0) .ge. 1) go to 60
        end if
        ker(2) = ner
        if (ker(2) .eq. 1) go to 20
      end if
      jfnxt = 2
      go to 70
   50 jfnxt = 7
      go to 70
   60 jfnxt = 8
   70 continue
      return
      end
