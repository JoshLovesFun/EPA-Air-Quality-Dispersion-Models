      Subroutine RINPH
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - RINPH Version 01.3                          --- 072497
CC--- Subroutine description - displays and reads source data.      ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
CC
      dimension jflgs(50),ker(11)
CC
      include 'OCNTRL.INC'
      include 'OCDSPM.INC'
      include 'OCALCS.INC'
      include 'ONAMES.INC'
      include 'OWNTRL.INC'
CC
      equivalence (jfnxt,functx(1)),(jflgs(1),functx(2)),
     *            (ker(1),functx(52)),(numsc,functx(63)),
     *            (isorc,functx(70)),(ifchg,functx(71)),
     *            (ifdtl,functx(72))
CC
CC********************************************************************
CC
CC      screen numsc+5 (15),+6 (16),+7 (17) get source data.
CC      get source emission type.
   10 itype = IVGET(isotyp(isorc),2)
CC      get source type.
      jtype = IVGET(isotyp(isorc),1)
CC      Volume or Line.
      if (jtype .ne. 2) then
        if (itype .ne. 1) then
          nn = 5
        else
          nn = 6
        end if
      else
        nn = 7
      end if
CC      get screen menu and menu data.
      numf = IRDSC(numsc+nn,isorc,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
CC      indicate source number in screen menu.
      xdumy = isorc
      nvl = IWHER(0,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      i = IWTBF(mscrnm(21,nln-5),2,xdumy,0,iflrjs)
CC      place source id in screen menu.
      call MVCH1(msoids(1,isorc),mscrnm(37,nln-5),36)
CC      display screen menu.
      iferrs = IOSCN(numsc+nn,mscrnm,ilinem,itabsm,ispacm,iferrm,lstrw,
     *               0)
CC      write data to work file.
      if (iferrs .ne. 10)i = IWTSC(numsc+nn,isorc,mscrnm,ilinem,iferrm)
CC      if terminate.
      if (iferrs .eq. 1) go to 50
      jfers = iferrs
      ker(6) = IFWER(iferrm,nmxlin*nmxfld)
      ifwrt = 0
CC
CC      check user response to menu.
      if (jfers .eq. 2) then
        go to 40
      else if (jfers .ge. 3.and.jfers .le. 7) then
CC        display aux. menus.
        iferrs = IFRSP(jfers,mscrnm,ilinem,itabsm,ispacm,iferrm,0)
        if (iferrs .eq. 1.or.iferrs .eq. 2) then
CC          terminate or main menu.
          jfers = iferrs
          go to 40
        else
CC          redisplay menu.
          go to 10
        end if
      else if (jfers .ge. 8.and.jfers .le. 10) then
        go to 10
      else if (jfers .eq. 11) then
        go to 60
      end if
CC      turn error array off.
      i = JINIT(iferrm,nmxlin*nmxfld,0,0)
CC
CC      check data.
CC
CC        see if source coordinates are rectangular or polar.
      idumy = IVGET(isrect(isorc),2)
      nvl = IWHER(0,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (mscrnm(nsc,nln) .ne. mblnk1) then
        if (IFCHR(mscrnm(nsc,nln),18) .ne. 0) then
CC          rectangular.
          idumy = 0
        else if (IFCHR(mscrnm(nsc,nln),16) .ne. 0) then
CC          polar.
          idumy = 1
        else
          iferrm(nvl,1) = 1
        end if
      else
CC        default to rectangular.
        idumy = 0
        ifwrt = 1
        mscrnm(nsc,nln) = malpha(18)
      end if
      isrect(isorc) = IVSTO(isrect(isorc),2,idumy)
CC
CC      get units x source coordinates.
      idumy = IVGET(isrect(isorc),1)
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .eq. 0) then
CC        default.
        idumy = 0
        ifwrt = 1
        if (iflrjs .eq. 0) then
          mscrnm(nsc+nch-1,nln) = malpha(39)
        else
          mscrnm(nsc,nln) = malpha(39)
        end if
      else
        call UNITS(mscrnm(nsc,nln),nch,i,jdumy,0,0,0)
        if (i .lt. 0) then
CC          error.
          iferrm(nvl,1) = 1
        else
CC          set units index.
          idumy = i
        end if
      end if
      isrect(isorc) = IVSTO(isrect(isorc),1,idumy)
CC
CC      get source x coordinate.
      nsc = JWHER(nvl,itabsm,ispacm,2,nch)
      i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
      if (i .gt. 0) then
CC        error.
        iferrm(nvl,2) = 1
      else if (i .lt. 0) then
        if (i .eq. -3) then
CC          default x coordinate.
          if (clddxs(isorc) .le. rmissd) then
            if (nxpnts .gt. 0) then
              clddxs(isorc) = xcoord(1)
            else if (nxypnt .gt. 0) then
              clddxs(isorc) = xdscrt(1)
            else
              clddxs(isorc) = 0.0
            end if
          end if
        else
          clddxs(isorc) = 0.0
        end if
        ifwrt = 1
        i = IWTBF(mscrnm(nsc,nln),nch,clddxs(isorc),1,iflrjs)
      else
CC        set x coordinate.
        clddxs(isorc) = xdumy
      end if
CC
CC      get source y coordinate.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
      if (i .gt. 0) then
CC        error.
        iferrm(nvl,1) = 1
      else if (i .lt. 0) then
        if (i .eq. -3) then
CC          default y coordinate.
          if (clddys(isorc) .le. rmissd) then
            if (nypnts .gt. 0) then
              clddys(isorc) = ycoord(1)
            else if (nxypnt .gt. 0) then
              clddys(isorc) = ydscrt(1)
            else
              clddys(isorc) = 0.0
            end if
          end if
        else
          clddys(isorc) = 0.0
        end if
        ifwrt = 1
        i = IWTBF(mscrnm(nsc,nln),nch,clddys(isorc),1,iflrjs)
      else
CC        set y coordinate.
        clddys(isorc) = xdumy
      end if
CC
CC      get source z base elevation.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
      if (i .gt. 0) then
CC        error.
        iferrm(nvl,1) = 1                                               071197
      else if (i .lt. 0) then
        if (i .eq. -3) then
CC          default z elevation.
          if (clddzs(isorc) .le. rmissd.or.iswopt(6) .eq. 2) then       071197
            clddzs(isorc) = 0.0
          end if
        else
          clddzs(isorc) = 0.0                                           071197
        end if
        ifwrt = 1
        i = IWTBF(mscrnm(nsc,nln),nch,clddzs(isorc),1,iflrjs)
      else
CC        set z elevation.
        if ((xdumy .gt. 0.0.and.iswopt(6) .lt. 2).or.                   120702
     *      (xdumy .gt. 0.0.and.iswopt(6) .eq. 3)) then                 120702
          iferrm(nvl,1) = 1                                             071197
          clddzs(isorc) = 0.0                                           071197
        else                                                            071197
          clddzs(isorc) = xdumy
        end if                                                          071197
      end if
CC
CC      get emission height units.
      idumy = IVGET(isrect(isorc),3)
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .eq. 0) then
CC        default.
        idumy = 0
        ifwrt = 1
        if (iflrjs .eq. 0) then
          mscrnm(nsc+nch-1,nln) = malpha(39)
        else
          mscrnm(nsc,nln) = malpha(39)
        end if
      else
        call UNITS(mscrnm(nsc,nln),nch,i,jdumy,0,0,0)
        if (i .lt. 0) then
CC          error.
          iferrm(nvl,1) = 1
        else
CC          set units index.
          idumy = i
        end if
      end if
      isrect(isorc) = IVSTO(isrect(isorc),3,idumy)
CC
CC      get emission height.
      nsc = JWHER(nvl,itabsm,ispacm,2,nch)
      i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
   20 if (i .gt. 0) then
CC        error.
        iferrm(nvl,2) = 1
      else if (i .lt. 0) then
        if (itype .eq. 1) then                                          072497
          if (hetcnt(isorc) .le. 0.0.or.qemiss(isorc) .le. 0) then      072497
            j = KERRS(68,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0)        072497
            iferrm(nvl,2) = 1                                           072497
          else                                                          072497
CC            compute height from diameter (set to radius).
            relhgt(isorc) = rmissc
          end if                                                        072497
        else
          relhgt(isorc) = 0.0
        end if
        ifwrt = 1
        i = IWTBF(mscrnm(nsc,nln),nch,relhgt(isorc),1,iflrjs)
      else
CC        set height.
        if (xdumy .lt. 0.0) then
          i = 1
          go to 20
        end if
        relhgt(isorc) = xdumy
      end if
CC
      if (jtype .eq. 2) then
CC
CC        get end of line source information.
CC
CC        get x coordinate of end of line source.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
        if (i .gt. 0) then
CC          error.
          iferrm(nvl,1) = 1
        else if (i .lt. 0) then
CC          default x coordinate.
          clddxe(isorc) = clddxs(isorc)
          ifwrt = 1
          i = IWTBF(mscrnm(nsc,nln),nch,clddxe(isorc),2,iflrjs)
        else
CC          set x coordinate.
          clddxe(isorc) = xdumy
        end if
CC
CC        get y coordinate of end of line source.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
        if (i .gt. 0) then
CC          error.
          iferrm(nvl,1) = 1
        else if (i .lt. 0) then
CC          default y coordinate.
          clddye(isorc) = clddys(isorc)
          ifwrt = 1
          i = IWTBF(mscrnm(nsc,nln),nch,clddye(isorc),2,iflrjs)
        else
CC          set y coordinate.
          clddye(isorc) = xdumy
        end if
CC        get z elevation of end of line source.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
        if (i .gt. 0) then
CC          error.
          iferrm(nvl,1) = 1
        else if (i .lt. 0) then
CC          default z elevation.
          clddze(isorc) = clddzs(isorc)
          ifwrt = 1
          i = IWTBF(mscrnm(nsc,nln),nch,clddze(isorc),2,iflrjs)
        else
CC          set z elevation.
          if ((xdumy .gt. 0.0.and.iswopt(6) .lt. 2).or.                 120702        
     *        (xdumy .gt. 0.0.and.iswopt(6) .eq. 3)) then               120702   
            iferrm(nvl,1) = 1                                           071197
            clddze(isorc) = 0.0                                         071197
          else
            clddze(isorc) = xdumy                                       071197
          end if
        end if
CC
CC        get height of end of line source.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
   30   if (i .gt. 0) then
CC          error.
          iferrm(nvl,1) = 1
        else if (i .lt. 0) then
CC          default height.
          relhte(isorc) = relhgt(isorc)
          ifwrt = 1
          i = IWTBF(mscrnm(nsc,nln),nch,relhte(isorc),2,iflrjs)
        else
          if (xdumy .lt. 0.0) then
            i = 1
            go to 30
          end if
CC          set height.
          relhte(isorc) = xdumy
        end if
CC
CC        get units of line source width.
        idumy = IVGET(idrect(isorc),1)
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        if (NUMC1(mscrnm(nsc,nln),nch) .eq. 0) then
CC          default.
          idumy = 0
          ifwrt = 1
          if (iflrjs .eq. 0) then
            mscrnm(nsc+nch-1,nln) = malpha(39)
          else
            mscrnm(nsc,nln) = malpha(39)
          end if
        else
          call UNITS(mscrnm(nsc,nln),nch,i,jdumy,0,0,0)
          if (i .lt. 0) then
CC            error.
            iferrm(nvl,1) = 1
          else
CC            set units index.
            idumy = i
          end if
        end if
        idrect(isorc) = IVSTO(idrect(isorc),1,idumy)
CC        get width of line source.
        nsc = JWHER(nvl,itabsm,ispacm,2,nch)
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
        if (i .gt. 0) then
CC          error.
          iferrm(nvl,2) = 1
        else if (i .lt. 0) then
CC          default width.
          if (itype .eq. 2) then                                        072497
            cldsht(isorc) = 10.0                                        072497
          else                                                          072497
            if (hetcnt(isorc) .le. 0.0.or.qemiss(isorc) .le. 0) then    072497
              j = KERRS(68,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0)      072497
              iferrm(nvl,2) = 1                                         072497
            else                                                        072497
              cldsht(isorc) = rmissc                                    072497
            end if                                                      072497
          end if                                                        072497
          ifwrt = 1
          i = IWTBF(mscrnm(nsc,nln),nch,cldsht(isorc),2,iflrjs)
        else
CC          set width.
          if (xdumy .le. 0.0) then                                      071197
            iferrm(nvl,2) = 1                                           071197
          else                                                          071197
            cldsht(isorc) = xdumy
          end if                                                        071197
        end if
CC
CC        get units of line source vertical dimension.
        idumy = IVGET(idrect(isorc),2)
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        if (NUMC1(mscrnm(nsc,nln),nch) .eq. 0) then
CC          default.
          idumy = 0
          ifwrt = 1
          if (iflrjs .eq. 0) then
            mscrnm(nsc+nch-1,nln) = malpha(39)
          else
            mscrnm(nsc,nln) = malpha(39)
          end if
        else
          call UNITS(mscrnm(nsc,nln),nch,i,jdumy,0,0,0)
          if (i .lt. 0) then
CC            error.
            iferrm(nvl,1) = 1
          else
CC            set units index.
            idumy = i
          end if
        end if
        idrect(isorc) = IVSTO(idrect(isorc),2,idumy)
CC        get depth of verical line source dimension.
        nsc = JWHER(nvl,itabsm,ispacm,2,nch)
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
        if (i .gt. 0) then
CC          error.
          iferrm(nvl,2) = 1
        else if (i .lt. 0) then
          if (itype .eq. 2) then                                        072497
CC              default depth.
            clddpt(isorc) = 10.0                                        072497
          else                                                          072497
            if (hetcnt(isorc) .le. 0.0.or.qemiss(isorc) .le. 0) then    072497
              j = KERRS(68,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0)      072497
              iferrm(nvl,2) = 1                                         072497
            else                                                        072497
CC              Calculate from radius.                                  072497
              clddpt(isorc) = rmissc                                    072497
            end if
          end if                                                        072497
          ifwrt = 1
          i = IWTBF(mscrnm(nsc,nln),nch,clddpt(isorc),2,iflrjs)
        else
CC          set depth.
          if (xdumy .le. 0.0) then                                      071197
            iferrm(nvl,2) = 1                                           071197
          else                                                          071197
            clddpt(isorc) = xdumy
          end if                                                        071197
        end if
CC        determine line source length.
        cldlng(isorc) = SQRT((clddxs(isorc)-clddxe(isorc))**2 +
     *                  (clddys(isorc)-clddye(isorc))**2)
CC        calculate angle from north.
        xdum = ARTAN(clddxe(isorc)-clddxs(isorc),clddye(isorc)-
     *               clddys(isorc)) * radtod
        if (xdum .lt. 0.0) xdum = xdum + 360.0
        if (xdum .gt. 360.0) xdum = xdum - 360.0
        if (xdum .gt. 180.0) xdum = xdum - 180.0
        cldang(isorc) = xdum
      else
CC        Volume source                                                 072497
        if (itype .ne. 1) then
CC          Quasi-Continuous.                                           072497
CC
CC          get units source long side.
          idumy = IVGET(idrect(isorc),1)
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          if (NUMC1(mscrnm(nsc,nln),nch) .eq. 0) then
CC            default.
            idumy = 0
            ifwrt = 1
            if (iflrjs .eq. 0) then
              mscrnm(nsc+nch-1,nln) = malpha(39)
            else
              mscrnm(nsc,nln) = malpha(39)
            end if
          else
            call UNITS(mscrnm(nsc,nln),nch,i,jdumy,0,0,0)
            if (i .lt. 0) then
CC              error.
              iferrm(nvl,1) = 1
            else
CC              set units index.
              idumy = i
            end if
          end if
          idrect(isorc) = IVSTO(idrect(isorc),1,idumy)
CC          get length of long source dimension.                        072497
          nsc = JWHER(nvl,itabsm,ispacm,2,nch)
          i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
          if (i .gt. 0) then
CC            error.
            iferrm(nvl,2) = 1
          else if (i .lt. 0) then
CC            default long dimension.
            cldlng(isorc) = 10.0                                        072497
            ifwrt = 1
            i = IWTBF(mscrnm(nsc,nln),nch,cldlng(isorc),2,iflrjs)
          else
CC            set long dimension.
            if (xdumy .le. 0.0) then                                    071197
              iferrm(nvl,2) = 1                                         071197
            else                                                        071197
              cldlng(isorc) = xdumy
            end if                                                      071197
          end if
CC
CC          get length of short source dimension.                       072497
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
          if (i .gt. 0) then
CC            error.
            iferrm(nvl,1) = 1
          else if (i .lt. 0) then
CC            default short dimension.
            cldsht(isorc) = 10.0                                        072497
            ifwrt = 1
            i = IWTBF(mscrnm(nsc,nln),nch,cldsht(isorc),2,iflrjs)
          else
CC            set short dimension.
            if (xdumy .le. 0.0) then                                    071197
              iferrm(nvl,1) = 1                                         071197
            else                                                        071197
              cldsht(isorc) = xdumy
            end if                                                      071197
          end if
CC
CC          get angle to long side of source.
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
          if (i .gt. 0) then
CC            error.
            iferrm(nvl,1) = 1
          else if (i .lt. 0) then
CC            default angle.
            cldang(isorc) = 0.0
            ifwrt = 1
            i = IWTBF(mscrnm(nsc,nln),nch,cldang(isorc),2,iflrjs)
          else
CC            set angle to long side.
            cldang(isorc) = xdumy
          end if
CC
CC          get units of vertical dimension.                            071197
          idumy = IVGET(idrect(isorc),2)
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          if (NUMC1(mscrnm(nsc,nln),nch) .eq. 0) then
CC            default.
            idumy = 0
            ifwrt = 1
            if (iflrjs .eq. 0) then
              mscrnm(nsc+nch-1,nln) = malpha(39)
            else
              mscrnm(nsc,nln) = malpha(39)
            end if
          else
            call UNITS(mscrnm(nsc,nln),nch,i,jdumy,0,0,0)
            if (i .lt. 0) then
CC              error.
              iferrm(nvl,1) = 1
            else
CC              set units index.
              idumy = i
            end if
          end if
          idrect(isorc) = IVSTO(idrect(isorc),2,idumy)
CC          get depth or vertical source dimension.
          nsc = JWHER(nvl,itabsm,ispacm,2,nch)
          i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
          if (i .gt. 0) then
CC            error.
            iferrm(nvl,2) = 1
          else if (i .lt. 0) then
CC            default depth dimension.
            clddpt(isorc) = 10.0                                        072497
            ifwrt = 1
            i = IWTBF(mscrnm(nsc,nln),nch,clddpt(isorc),2,iflrjs)
          else
CC            set depth dimension.
            if (xdumy .le. 0.0) then                                    071197
              iferrm(nvl,2) = 1                                         071197
            else                                                        071197
              clddpt(isorc) = xdumy
            end if
          end if
        else
CC          Source is instantaneous volume.                             072497
CC          get units of source diameter.
          idumy = IVGET(idrect(isorc),1)
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          if (NUMC1(mscrnm(nsc,nln),nch) .eq. 0) then
CC            default.
            idumy = 0
            ifwrt = 1
            if (iflrjs .eq. 0) then
              mscrnm(nsc+nch-1,nln) = malpha(39)
            else
              mscrnm(nsc,nln) = malpha(39)
            end if
          else
            call UNITS(mscrnm(nsc,nln),nch,i,jdumy,0,0,0)
            if (i .lt. 0) then
CC              error.
              iferrm(nvl,1) = 1
            else
CC              set units index.
              idumy = i
            end if
          end if
          idrect(isorc) = IVSTO(idrect(isorc),1,idumy)
CC          get diameter of source immediately after detonation.
          nsc = JWHER(nvl,itabsm,ispacm,2,nch)
          i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
          if (i .gt. 0) then
CC            error.
            iferrm(nvl,2) = 1
          else if (i .lt. 0) then
            if (hetcnt(isorc) .le. 0.0.or.qemiss(isorc) .le. 0) then    072497
              j = KERRS(68,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0)      072497
              iferrm(nvl,2) = 1                                         072497
            else                                                        072497
CC              default diameter
              cldlng(isorc) = rmissc                                    072497
              clddpt(isorc) = rmissc                                    072497
              cldsht(isorc) = rmissc                                    072497
            end if
            cldang(isorc) = 0.0
            ifwrt = 1
            i = IWTBF(mscrnm(nsc,nln),nch,cldlng(isorc),2,iflrjs)
          else
CC            set cloud diameter.
            if (xdumy .le. 0.0) then                                    071197
              if (hetcnt(isorc) .le. 0.0.or.qemiss(isorc) .le. 0) then  072497
                j = KERRS(68,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0)    072497
                iferrm(nvl,2) = 1                                       072497
              else                                                      072497
                xdumy = rmissc                                          071197
                ifwrt = 1                                               071197
                i = IWTBF(mscrnm(nsc,nln),nch,xdumy,2,iflrjs)           071197
              end if                                                    072497
            end if                                                      071197
            cldlng(isorc) = xdumy
            cldsht(isorc) = xdumy
            clddpt(isorc) = xdumy
            cldang(isorc) = 0.0
            idumy = IVGET(idrect(isorc),1)
            idrect(isorc) = IVSTO(idrect(isorc),2,idumy)
          end if
        end if
      end if
CC
CC      see if detailed source inputs are desired.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (mscrnm(nsc,nln) .ne. mblnk1) then
        if (IFCHR(mscrnm(nsc,nln),14) .ne. 0) then
          ifdtl = 0
        else if (IFCHR(mscrnm(nsc,nln),25) .ne. 0) then
          ifdtl = 1
        else
          iferrm(nvl,1) = 1
        end if
      else
        ifdtl = 0
      end if
      ner = IFWER(iferrm,nmxlin*nmxfld)
      if (ifwrt .gt. 0.or.ner .gt. 0.or.(ner .eq. 0.and.ker(6) .gt. 0))
     *    then
CC        rewrite screen data to work file.
        i = IWTSC(numsc+nn,1,mscrnm,ilinem,iferrm)
      end if
      if (ner .gt. 0) then
        if (KERRP(22,0) .ge. 1) go to 50
      end if
      if (itype .eq. 1) then
        if (taucld(isorc) .gt. 15.0) then
          if (KERRS(6,mnpbuf,isorc,idum,taucld(isorc),xdum,xdum,xdum,0)
     *        .ge. 1) go to 50
        end if
      else
        if (taucld(isorc) .le. 15.0) then
          if (KERRS(7,mnpbuf,isorc,idum,taucld(isorc),brnrat(isorc),
     *        xdum,xdum,0) .ge. 1) go to 50
        end if
      end if
      ker(6) = ner
      if (jfers .eq. 0.and.ker(6) .eq. 1) go to 10
CC
      jfnxt = 7
      go to 70
   40 jfnxt = 11
      go to 70
   50 jfnxt = 12
      go to 70
   60 jfnxt = 5
   70 return
      end
