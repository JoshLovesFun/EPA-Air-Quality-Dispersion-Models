      Subroutine RINPP
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - RINPP Version 01.1                          ---
CC--- Subroutine description - displays and reads meteorological    ---
CC---                          data.                                ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
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
CC********************************************************************
CC
CC
      if (jfnxt .eq. 7) go to 110
      if (jfnxt .eq. 8) go to 150
CC
   10 if (ifhrly .ne. 0) then
CC        if user specified formatted or unformatted data.
        if (ihrfmt .le. 1) then
CC
CC            screen numsc+7, meteorological data, screen 29.
          nn = 7
CC          read screen menu and screen data.
          numf = IRDSC(numsc+nn,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
CC          set met. data units.
          nvl = IWHER(6,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
          call UNITS(mscrnm(20,nln),5,ispunt,jspunt,0,1,7)
          nvl = IWHER(9,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
          call UNITS(mscrnm(58,nln),1,isyunt,0,0,1,1)
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          call UNITS(mscrnm(60,nln),1,iszunt,0,0,1,1)
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          call UNITS(mscrnm(62,nln),1,isxunt,0,0,1,1)
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          call UNITS(mscrnm(25,nln),2,itpunt,jdumy,0,1,6)
CC          show defaults.
          do 20 nvl=1,12
            if (nvl .eq. 4) then
              j = 3
            else
              j = 1
            end if
            nln = ilinem(nvl)
            nsc = JWHER(nvl,itabsm,ispacm,j,nch)
            if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
              if (ivarmt(nvl) .lt. 0) then
                xdumy = rmissc
              else
                xdumy = ivarmt(nvl)
              end if
              i = IWTBF(mscrnm(nsc,nln),nch,xdumy,0,iflrjs)
            end if
            if (nvl .eq. 4) then
              nsc = JWHER(nvl,itabsm,ispacm,1,nch)
              i = IWTBF(mscrnm(nsc,nln),nch,vllimt(4),-4,iflrjs)
              nsc = JWHER(nvl,itabsm,ispacm,2,nch)
              i = IWTBF(mscrnm(nsc,nln),nch,vulimt(4),-4,iflrjs)
            end if
   20     continue
CC
CC          display screen and get user response.
          iferrs = IOSCN(numsc+nn,mscrnm,ilinem,itabsm,ispacm,iferrm,
     *                   lstrw,0)
          jfers = iferrs
CC          save screen menu.
          if (iferrs .ne. 10)i = IWTSC(numsc+nn,1,mscrnm,ilinem,iferrm)
CC          if terminate.
          if (iferrs .eq. 1) go to 150
          ker(8) = IFWER(iferrm,nmxlin*nmxfld)
          jfers = iferrs
          ifwrt = 0
CC
CC          check user response to met. menu.
          if (jfers .eq. 2) then
CC            main menu.
            go to 110
          else if (jfers .ge. 3.and.jfers .le. 7) then
CC            display aux. menus.
            iferrs = IFRSP(jfers,mscrnm,ilinem,itabsm,ispacm,iferrm,0)
            if (iferrs .eq. 1.or.iferrs .eq. 2) then
CC              terminate or main menu.
              jfers = iferrs
              go to 110
            else
              go to 10
            end if
          else if (jfers .ge. 8.and.jfers .le. 10) then
            go to 10
          else if (jfers .eq. 11) then
            go to 140
          end if
CC          turn off error array.
          i = JINIT(iferrm,nmxlin*nmxfld,0,0)
CC
CC          check data.
CC
CC          get which variables are in data set.
          do 30 nvl=1,12
            if (nvl .eq. 4) then
              j = 3
            else
              j = 1
            end if
            nln = ilinem(nvl)
            nsc = JWHER(nvl,itabsm,ispacm,j,nch)
            i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
            if (i .gt. 0) then
              iferrm(nvl,j) = 1
            else if (i .eq. 0.and.xdumy .ge. 0.0) then
              ivarmt(nvl) = xdumy
            else if (i .eq. -2) then
              if (nvl .eq. 7.or.(nvl .ge. 9.and.nvl .le.11)) then
                ivarmt(nvl) = -1
              else
                iferrm(nvl,j) = 1
              end if
            else
              ivarmt(nvl) = 0
            end if
            if (nvl .eq. 4) then
              nsc = JWHER(nvl,itabsm,ispacm,1,nch)
              if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
                i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
                if (i .gt. 0) then
                  iferrm(nvl,j) = 1
                else if (i .lt. 0) then
                  i = IWTBF(mscrnm(nsc,nln),nch,vllimt(4),-4,iflrjs)
                  ifwrt = 1
                else
                  i = xdumy
                  if (i .eq. 0.or.i .eq. 1.or.i .eq. 100) then
                    if (xdumy .lt. 100.0) then
                      xdumy = xdumy * 100
                      i = IWTBF(mscrnm(nsc,nln),nch,xdumy,-4,iflrjs)
                      ifwrt = 1
                    end if
                    vllimt(4) = xdumy
                  else
                    iferrm(nvl,j) = 1
                  end if
                end if
              end if
              nsc = JWHER(nvl,itabsm,ispacm,2,nch)
              if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
                i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
                if (i .gt. 0) then
                  iferrm(nvl,j) = 1
                else if (i .lt. 0) then
                  if (vllimt(4) .gt. 0.0) then
                    xdumy = 2400.0
                  else
                    xdumy = 2300.0
                  end if
                  i = IWTBF(mscrnm(nsc,nln),nch,xdumy,-4,iflrjs)
                  ifwrt = 1
                else
                  i = xdumy
                  if (vllimt(4) .lt. 100.0) then
                    if (i .eq. 23.or.i .eq. 2300) then
                      i = xdumy
                    else
                      i = -1
                    end if
                  else
                    if (i .eq. 24.or.i .eq. 2400) then
                      i = xdumy
                    else
                      i = -1
                    end if
                  end if
                  if (i .gt. 0) then
                    if (i .lt. 100) then
                      xdumy = xdumy * 100
                      i = IWTBF(mscrnm(nsc,nln),nch,xdumy,-4,iflrjs)
                      ifwrt = 1
                    end if
                    vulimt(4) = xdumy
                  else
                    iferrm(nvl,j) = 1
                  end if
                end if
              end if
            end if
   30     continue
CC
          ner = IFWER(iferrm,nmxlin*nmxfld)
          if (ifwrt .gt. 0.or.ner .gt. 0.or.(ner .eq. 0.and.
     *        ker(8) .gt. 0)) then
CC            rewrite screen data.
            i = IWTSC(numsc+nn,1,mscrnm,ilinem,iferrm)
          end if
          if (ner .gt. 0) then
            if (KERRP(22,0) .ge. 1) go to 150
          end if
          ker(8) = ner
          if (jfers .eq. 0.and.ker(8) .eq. 1) go to 10
CC
CC        if user specified formatted or unformatted data.
CC
CC          screen numsc+8, meteorological data, screen 30.
   40     nn = 8
CC          read screen menu and screen data.
          numf = IRDSC(numsc+nn,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
CC          set default met. data units.
CC
CC          pressure.
          nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
          call UNITS(mscrnm(18,nln),4,iprunt,jdumy,0,1,4)
CC          vert. pot. temp. gradient.
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          call UNITS(mscrnm(32,nln),5,ivgunt,jvgunt,0,1,15)
CC          mixing layer depth.
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          call UNITS(mscrnm(28,nln),2,ihmunt,jdumy,0,1,0)
CC          wind speed shear.
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          call UNITS(mscrnm(26,nln),5,issunt,jssunt,0,1,7)
CC          direction shear.
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          call UNITS(mscrnm(30,nln),4,idsunt,jdsunt,0,1,14)
CC          measurement time for lateral turbulence intensity.
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,3,1,nln,nsc,nch)
          call UNITS(mscrnm(59,nln),2,istunt,jdumy,0,1,5)
CC          roughness length.
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          call UNITS(mscrnm(26,nln),2,irount,jdumy,0,1,0)
CC          ceiling height.
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,4,1,nln,nsc,nch)
          call UNITS(mscrnm(23,nln),2,iceunt,jdumy,0,1,0)
CC          show defaults.
          do 50 nvl=1,13
            nln = ilinem(nvl)
            nsc = JWHER(nvl,itabsm,ispacm,1,nch)
            if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
              if (ivarmt(nvl+12) .lt. 0) then
                xdumy = rmissc
              else
                xdumy = ivarmt(nvl+12)
              end if
              i = IWTBF(mscrnm(nsc,nln),nch,xdumy,0,iflrjs)
            end if
   50     continue
CC
CC          display screen and get user response.
          iferrs = IOSCN(numsc+nn,mscrnm,ilinem,itabsm,ispacm,iferrm,
     *                   lstrw,0)
          jfers = iferrs
CC          save screen menu.
          if (iferrs .ne. 10)i = IWTSC(numsc+nn,1,mscrnm,ilinem,iferrm)
CC          if terminate.
          if (iferrs .eq. 1) go to 150
          ker(9) = IFWER(iferrm,nmxlin*nmxfld)
          jfers = iferrs
          ifwrt = 0
CC
CC          check user response to met. menu.
          if (jfers .eq. 2) then
CC            main menu.
            go to 110
          else if (jfers .ge. 3.and.jfers .le. 7) then
CC            display aux. menus.
            iferrs = IFRSP(jfers,mscrnm,ilinem,itabsm,ispacm,iferrm,0)
            if (iferrs .eq. 1.or.iferrs .eq. 2) then
CC              terminate or main menu.
              jfers = iferrs
              go to 110
            else
              go to 40
            end if
          else if (jfers .ge. 8.and.jfers .le. 10) then
            go to 40
          else if (jfers .eq. 11) then
            go to 10
          end if
CC          turn off error array.
          i = JINIT(iferrm,nmxlin*nmxfld,0,0)
CC
CC          check data.
CC
CC          get which variables are in data set.
          do 60 nvl=1,13
            nln = ilinem(nvl)
            nsc = JWHER(nvl,itabsm,ispacm,1,nch)
            i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
            if (i .gt. 0) then
              iferrm(nvl,j) = 1
            else if (i .eq. 0.and.xdumy .ge. 0.0) then
              ivarmt(nvl+12) = xdumy
            else if (i .eq. -2) then
              if ((nvl .ge. 2.and.nvl .le. 4).or.nvl .eq. 6.or.
     *            nvl .eq. 7) then
CC>            cannot calculate clearing index (nvl=11) in this version,
CC>            see function IUSDA.
CC>  *            nvl .eq. 7.or.nvl .eq. 11) then
                ivarmt(nvl+12) = -1
              else
                iferrm(nvl,j) = 1
              end if
            else
              ivarmt(nvl) = 0
            end if
   60     continue
          if (ivarmt(18) .lt. 0) then
            if (ivarmt(19) .le. 0) then
              if (zondif .lt. -12.0.or.geolon .lt. -180.0.or.geolat
     *            .lt. -90.0.or.ivarmt(24) .le. 0.or.ivarmt(25) .le.
     *            0) then
                iferrm(6,1) = 1
              end if
            end if
          else if (ivarmt(19) .lt. 0) then
            if (ivarmt(18) .le. 0) then
              if (zondif .lt. -12.0.or.geolon .lt. -180.0.or.geolat
     *            .lt. -90.0.or.ivarmt(24) .le. 0.or.ivarmt(25) .le.
     *            0) then
                iferrm(7,1) = 1
              end if
            end if
          end if
CC
          ner = IFWER(iferrm,nmxlin*nmxfld)
          if (ifwrt .gt. 0.or.ner .gt. 0.or.(ner .eq. 0.and.
     *        ker(9) .gt. 0)) then
CC            rewrite screen data.
            i = IWTSC(numsc+nn,1,mscrnm,ilinem,iferrm)
          end if
          if (ner .gt. 0) then
            if (KERRP(22,0) .ge. 1) go to 150
          end if
          ker(9) = ner
          if (jfers .eq. 0.and.ker(9) .eq. 1) go to 40
CC
CC
CC          if user specified fixed format data.
CC
   70     if (ihrfmt .eq. 1) then
CC
CC            screen numsc+9, meteorological data, screen 31.
            nn = 9
CC            read screen menu and screen data.
            iferrs = IDSPL(numsc+nn,1,mscrnm,ilinem,itabsm,ispacm,
     *                     iferrm,1,lstrw,12)
CC            if terminate.
            if (iferrs .eq. 1) go to 150
            ker(10) = IFWER(iferrm,nmxlin*nmxfld)
            jfers = iferrs
            ifwrt = 0
CC
CC            check user response to met. menu.
            if (jfers .eq. 2) then
CC              main menu.
              go to 110
            else if (jfers .ge. 3.and.jfers .le. 7) then
CC              display aux. menus.
              iferrs = IFRSP(jfers,mscrnm,ilinem,itabsm,ispacm,iferrm,0)
              if (iferrs .eq. 1.or.iferrs .eq. 2) then
CC                terminate or main menu.
                jfers = iferrs
                go to 110
              else
                go to 70
              end if
            else if (jfers .ge. 8.and.jfers .le. 10) then
              go to 70
            else if (jfers .eq. 11) then
              go to 40
            end if
CC            turn off error array.
            i = JINIT(iferrm,nmxlin*nmxfld,0,0)
CC
CC            check data.
CC
CC            get data record format.
            nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
            if (NUMC1(mscrnm(nsc,nln),nch) .ge. 0) then
              call MBLK1(mformt,151)
              call MVCH1(mscrnm(nsc,nln),mformt,nch)
              nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nchs)
              nche = nchs
              if (nch+nchs .gt. 150) nchs = 150 - nch
              call MVCH1(mscrnm(nsc,nln),mformt(nch+1),nchs)
              ier = IFORM(mformt,nch+nchs,nch,nvl)
              if (ier .gt. 0) then
                ifwrt = 1
                call MBLK1(mscrnm(nsc,nln-1),nch)
                call MBLK1(mscrnm(nsc,nln),nche)
                call MVCH1(mformt,mscrnm(nsc,nln-1),nch)
                i = LRJST(mscrnm(nsc,nln-1),nch,iflrjs)
                i = NUMC1(mformt,nch+nchs)
                if (i .gt. nch) then
                  call MVCH1(mformt(nch+1),mscrnm(nsc,nln),nchs-nch)
                  i = LRJST(mscrnm(nsc,nln),nch,1)
                end if
              end if
            else
              iferrm(nvl,1) = 1
            end if
            ner = IFWER(iferrm,nmxlin*nmxfld)
            if (ifwrt .gt. 0.or.ner .gt. 0.or.(ner .eq. 0.and.
     *          ker(10) .ne. 0)) then
CC              rewrite screen data.
              i = IWTSC(numsc+nn,1,mscrnm,ilinem,iferrm)
            end if
            if (ner .gt. 0) then
              if (KERRP(22,0) .ge. 1) go to 150
            end if
            ker(10) = ner
            if (jfers .eq. 0.and.ker(10) .eq. 1) go to 70
CC
          end if
        else
          if (ispunt .ne. 0.or.jspunt .ne. 0) then
            ier = KERRS(49,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0)
            numsc = numscs(6) + 2
CC            read screen menu and screen data.
            numf = IRDSC(numsc,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
            nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
            iferrm(nvl,1) = 2
CC            save screen menu.
            i = IWTSC(numsc,1,mscrnm,ilinem,iferrm)
            if (ker(3) .ne. 1) ker(3) = 2
          end if
          if (ihmunt .ne. 0) then
            ier = KERRS(50,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0)
            numsc = numscs(6) + 3
CC            read screen menu and screen data.
            numf = IRDSC(numsc,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
            nvl = IWHER(7,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
            iferrm(nvl,1) = 2
CC            save screen menu.
            i = IWTSC(numsc,1,mscrnm,ilinem,iferrm)
            if (ker(4) .ne. 1) ker(4) = 2
          end if
          if (itpunt .ne. 1) then
            ier = KERRS(14,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0)
            numsc = numscs(6) + 4
CC            read screen menu and screen data.
            numf = IRDSC(numsc,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
            nvl = IWHER(3,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
            iferrm(nvl,1) = 2
CC            save screen menu.
            i = IWTSC(numsc,1,mscrnm,ilinem,iferrm)
            if (ker(5) .ne. 1) ker(5) = 2
          end if
        end if
      end if
CC
CC
CC
  110 if (IFWER(ker,10) .eq. 1) then
        ifset = -1
      else
        ifset = 1
        if (iniyrs .gt. 0) then
          if (iniyrs .lt. vllimt(1).or.iniyrs .gt. vulimt(1)) then
            ifset = -1
            it = 1
          end if
        end if
        if (inimon .gt. 0) then
          if (inimon .lt. vllimt(2).or.inimon .gt. vulimt(2)) then
            ifset = -1
            it = 2
          end if
        end if
        if (inidys .gt. 0) then
          if (inidys .lt. vllimt(3).or.inidys .gt. vulimt(3)) then
            ifset = -1
            it = 3
          end if
        end if
        if (inihrs .gt. -1) then
          if (inihrs .lt. vllimt(4).or.inihrs .gt. vulimt(4)) then
            ifset = -1
            it = 4
          end if
        end if
        if (injdys .gt. 0) then
          if (injdys .lt. vllimt(5).or.injdys .gt. vulimt(5)) then
            ifset = -1
            it = 5
          end if
        end if
        ydumy = refspd * constl(ispunt+1)
        if (ispunt .ne. 9) ydumy = ydumy / constt(jspunt+1)
        if (ydumy .lt. vllimt(6).or.ydumy .gt. vulimt(6)) then
          ifset = -1
          it = 6
        end if
        if (pwrlaw .gt. rmissd) then
          if (pwrlaw .lt. vllimt(7).or.pwrlaw .gt. vulimt(7)) then
            ifset = -1
            it = 7
          end if
        end if
CC
        if (wnddir .lt. vllimt(8).or.wnddir .gt. vulimt(8)) then
          ifset = -1
          it = 8
        end if
CC
        if (sigmap .gt. rmissd) then
          ydumy = sigmap
          if (isyunt .ne. 0) ydumy = xdumy * radtod
          if (ydumy .lt. vllimt(9).or.ydumy .gt. vulimt(9)) then
            ifset = -1
            it = 9
          end if
        end if
CC
        if (sigmep .gt. rmissd) then
          ydumy = sigmep
          if (iszunt .ne. 0) ydumy = xdumy * radtod
          if (ydumy .lt. vllimt(10).or.ydumy .gt. vulimt(10)) then
            ifset = -1
            it = 10
          end if
        end if
CC
        if (trbixr .gt. rmissd) then
          ydumy = trbixr
          if (isxunt .ne. 0) ydumy = xdumy * radtod
          if (ydumy .lt. vllimt(11).or.ydumy .gt. vulimt(11)) then
            ifset = -1
            it = 11
          end if
        end if
CC
        ydumy = constc(itpunt+1,2) * (airtmp - constc(itpunt+1,1))
        if (ydumy .lt. vllimt(12).or.ydumy .gt. vulimt(12)) then
          ifset = -1
          it = 12
        end if
CC
        ydumy = airprs * constp(iprunt+1)
        if (ydumy .lt. vllimt(13).or.ydumy .gt. vulimt(13)) then
          ifset = -1
          it = 13
        end if
CC
        if (dphidz .gt. rmissd) then
          ydumy = constc(ivgunt+1,2) * dphidz / constl(jvgunt+1)
          if (ydumy .lt. vllimt(14).or.ydumy .gt. vulimt(14)) then
            ifset = -1
            it = 14
          end if
        end if
CC
        if (hmdpth .gt. rmissd) then
          ydumy = hmdpth * constl(ihmunt+1)
          if (ydumy .lt. vllimt(15).or.ydumy .gt. vulimt(15)) then
            ifset = -1
            it = 15
          end if
        end if
CC
        if (spdshr .gt. rmissd) then
          ydumy = spdshr * constl(issunt+1)
          if (jssunt .ge. 0) ydumy = ydumy / constt(jssunt+1)
          if (ydumy .lt. vllimt(16).or.ydumy .gt. vulimt(16)) then
            ifset = -1
            it = 16
          end if
        end if
CC
        if (dirshr .gt. rmissd) then
          ydumy = dirshr
          if (idsunt .ne. 0) ydumy = dirshr * radtod
          if (ydumy .lt. vllimt(17).or.ydumy .gt. vulimt(17)) then
            ifset = -1
            it = 17
          end if
        end if
CC
        if (stblty .gt. rmissd) then
          if (stblty .lt. vllimt(18).or.stblty .gt. vulimt(18)) then
            ifset = -1
            it = 18
          end if
        end if
        if (radinx .gt. rmissd) then
          if (radinx .lt. vllimt(19).or.radinx .gt. vulimt(19)) then
            ifset = -1
            it = 19
          end if
        end if
CC
        ydumy = tauosa * constt(istunt+1)
        if (ydumy .lt. vllimt(20).or.ydumy .gt. vulimt(20)) then
          ifset = -1
          it = 20
        end if
CC
        ydumy = roughl * constl(irount+1) / constl(6)                   092904
        if (ydumy .lt. vllimt(21).or.ydumy .gt. vulimt(21)) then
          ifset = -1
          it = 21
        end if
CC
        if (airhum .gt. 0.0.and.airhum .le. 1.0) airhum = airhum * 100.0
        if (airhum .lt. vllimt(22).or.airhum .gt. vulimt(22)) then
          ifset = -1
          it = 22
        end if
CC>
CC>     if (clearx .lt. vllimt(23).or.clearx .gt. vulimt(23)) then
CC>       ifset = -1
CC>       it = 23
CC>     end if
CC
        if (cldcov .lt. vllimt(24).or.cldcov .gt. vulimt(24)) then
          ifset = -1
          it = 24
        end if
        if (cldhgt .lt. vllimt(25).or.cldhgt .gt. vulimt(25)) then
          ifset = -1
          it = 25
        end if
      end if
  130 ifrdwt(4) = ifset
      jfnxt = 0
      go to 160
  140 jfnxt = 5
      go to 160
  150 jfnxt = 8
  160 return
      end
