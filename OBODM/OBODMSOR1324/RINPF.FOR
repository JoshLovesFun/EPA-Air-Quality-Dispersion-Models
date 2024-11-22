      Subroutine RINPF
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - RINPF Version 01.2                          ---
CC--- Subroutine description - displays and reads source data.      ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      character*1 mslsh
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
     *            (isorc,functx(70))
CC
      data mslsh/'/'/
CC
CC********************************************************************
CC
      if (jfnxt .gt. 0) then
        go to (10,80),jfnxt
      end if
      ifnext = 3
CC      start source screen number 10.
      numsc = numscs(5)
      ner = 0
      i = JINIT(ker,11,0,0)
      i = JINIT(jflgs,50,0,0)
      heatdf = rmissd
      burndf = rmissd
CC
CC      display and read data for screens numsc to numsc+10.
CC
CC      screen numsc, start of source data.
   10 iferrs = IDSPL(numsc,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1,
     *               lstrw,0)
CC      if terminate.
      if (iferrs .eq. 1) go to 160
      ker(1) = IFWER(iferrm,nmxlin*nmxfld)
CC      save iferrs.
      jfers = iferrs
      ifwrt = 0
CC
CC      check user response to first source menu.
      if (jfers .eq. 2.or.jfers .eq. 11) then
CC        main menu.
        go to 150
      else if (jfers .ge. 3.and.jfers .le. 7) then
CC        display aux. menus.
        if (jfers .eq. 6) then
          iflg = 1
        else
          iflg = 0
        end if
        iferrs = IFRSP(jfers,mscrnm,ilinem,itabsm,ispacm,iferrm,iflg)
        if (iferrs .eq. 1.or.iferrs .eq. 2) then
CC          terminate or main menu.
          jfers = iferrs
          go to 150
        else
          if (iferrs .eq. 0) then
            i = JINIT(jflgs,50,0,0)
CC            set default source parameters.
            i = IRDSC(numsc,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
CC            fuel/explosive label.
            nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
            call MVCH1(mpolnt,mscrnm(nsc,nln),MIN0(nch,36))
            i = LRJST(mscrnm(nsc,nln),nch,iflrjs)
CC            pollutant/species label.
            nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
            call MVCH1(mspeci,mscrnm(nsc,nln),MIN0(nch,36))
            i = LRJST(mscrnm(nsc,nln),nch,iflrjs)
CC            gaseous/particulate.
            nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
            mscrnm(nsc,nln) = malpha(7)
CC            molecular weight.
            nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
            i = IWTBF(mscrnm(nsc,nln),nch,whtmol,4,iflrjs)
CC            density.
            nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,2,nln,nsc,nch)
            i = IWTBF(mscrnm(nsc,nln),nch,densty,4,iflrjs)
            nsc = JWHER(nvl,itabsm,ispacm,1,nch)
            call UNITS(mscrnm(nsc,nln),nch,idunit,jdunit,0,1,11)
CC            half-life.
            nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,2,nln,nsc,nch)
            i = IWTBF(mscrnm(nsc,nln),nch,decays,4,iflrjs)
            nsc = JWHER(nvl,itabsm,ispacm,1,nch)
            call UNITS(mscrnm(nsc,nln),nch,idcunt,jdumy,0,1,5)
CC            species mass.
            nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,2,nln,nsc,nch)
            call MBLK1(mscrnm(nsc,nln),nch)
            nsc = JWHER(nvl,itabsm,ispacm,1,nch)
            call MBLK1(mscrnm(nsc,nln),nch)
CC            fuel mass.
            nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,2,nln,nsc,nch)
            call MBLK1(mscrnm(nsc,nln),nch)
            nsc = JWHER(nvl,itabsm,ispacm,1,nch)
            call MBLK1(mscrnm(nsc,nln),nch)
CC            emission factor. mass fraction of species/fuel.
            nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
            i = IWTBF(mscrnm(nsc,nln),nch,qfract,nch-2,iflrjs)
CC            write screen menu data back out.
            i = IWTSC(numsc,1,mscrnm,ilinem,iferrm)
          end if
          go to 10
        end if
      else if (jfers .ge. 8.and.jfers .le. 10) then
        go to 10
      end if
CC      turn off error array.
      i = JINIT(iferrm,nmxlin*nmxfld,0,0)
CC
CC      check specified data.
CC
CC      save fuel/explosive label.
      nvl = IWHER(0,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      call MBLK1(mpolnt,36)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        call MVCH1(mscrnm(nsc,nln),mpolnt,MIN0(nch,36))
        n = NUMC1(mscrnm(nsc,nln),nch)
        if (n .gt. 0.and.n .lt. 36) then
          i = LRJST(mscrnm(nsc,nln),nch,iflrjs)
          ifwrt = 1
        end if
        i = LRJST(mpolnt,36,1)
      end if
CC
CC      save pollutant/species name.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      call MBLK1(mspeci,36)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        call MVCH1(mscrnm(nsc,nln),mspeci,MIN0(nch,36))
        n = NUMC1(mscrnm(nsc,nln),nch)
        if (n .gt. 0.and.n .lt. 36) then
          i = LRJST(mscrnm(nsc,nln),nch,iflrjs)
          ifwrt = 1
        end if
        i = LRJST(mspeci,36,1)
      end if
CC
CC      is species gaseous or particulate.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (IFCHR(mscrnm(nsc,nln),7) .ne. 0.or.
     *    mscrnm(nsc,nln) .eq. mblnk1) then
CC        gaseous
        ifgpwn = 0
        betale = 0.0
        if (mscrnm(nsc,nln) .eq. mblnk1) then
          ifwrt = 1
          mscrnm(nsc,nln) = malpha(7)
        end if
      else if (IFCHR(mscrnm(nsc,nln),16) .ne. 0) then
CC        particulate.
        ifgpwn = 1
        if (iswopt(6) .eq. 2) then                                      120702
          if (KERRS(66,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *        go to 160
          iferrm(nvl,1) = 1
        end if
      else
        iferrm(nvl,1) = 1
      end if
CC
CC      see if pollutant/species molecular weight is specified.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
CC
CC        get species molecular weight.
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
   20   if (i .gt. 0) then
CC          error.
          iferrm(nvl,1) = 1
        else if (i .lt. 0) then
CC          default species molecular weight.
          whtmol = 18.015
          i = IWTBF(mscrnm(nsc,nln),nch,whtmol,3,iflrjs)
          ifwrt = 1
        else
          if (xdumy .le. 0.0) then
            i = 1
            go to 20
          end if
CC          set molecular weight.
          whtmol = xdumy
        end if
      end if
CC
CC      see if pollutant/species density is specified.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,2,nln,nscs,nchs)
      if (NUMC1(mscrnm(nscs,nln),nchs) .gt. 0) then
CC
CC        get density units.
        nsc = JWHER(nvl,itabsm,ispacm,1,nch)
        if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
          call UNITS(mscrnm(nsc,nln),nch,i,j,0,0,11)
          if (i .lt. 0.or.i .gt. 7.or.j .lt. 0.or.j .gt. 9) then
            iferrm(nvl,1) = 1
          else
            idunit = i
            jdunit = j
          end if
        else
CC          default to grams/cm**3.
          idunit = 0
          jdunit = 5
          ifwrt = 1
          call UNITS(mscrnm(nsc,nln),nch,idunit,jdunit,0,1,11)
        end if
CC
CC        get species density.
        i = IRDBF(mscrnm(nscs,nln),nchs,xdumy)
   30   if (i .gt. 0) then
CC          error.
          iferrm(nvl,2) = 1
        else if (i .lt. 0) then
CC          default species density.
          densty = 1.0
          i = IWTBF(mscrnm(nscs,nln),nchs,densty,3,iflrjs)
          ifwrt = 1
        else
          if (xdumy .le. 0.0) then
            i = 1
            go to 30
          end if
CC          set density.
          densty = xdumy
        end if
      end if
CC
CC      see if pollutant/species half-life is specified.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,2,nln,nscs,nchs)
      if (NUMC1(mscrnm(nscs,nln),nchs) .gt. 0) then
CC
CC        get half-life units.
        nsc = JWHER(nvl,itabsm,ispacm,1,nch)
        if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
          call UNITS(mscrnm(nsc,nln),nch,i,jdumy,0,0,5)
          if (i .lt. 0.or.i .gt. 2) then
            iferrm(nvl,1) = 1
          else
            idcunt = i
          end if
        else
CC          default to seconds.
          idcunt = 0
          ifwrt = 1
          call UNITS(mscrnm(nsc,nln),nch,idcunt,jdumy,0,1,5)
        end if
CC
CC        get half-life.
        i = IRDBF(mscrnm(nscs,nln),nchs,xdumy)
   40   if (i .gt. 0) then
CC          error.
          iferrm(nvl,2) = 1
        else if (i .lt. 0) then
CC          default half-life.
          decays = 0.0
          ifwrt = 1
          i = IWTBF(mscrnm(nscs,nln),nchs,decays,1,iflrjs)
        else
          if (xdumy .lt. 0.0) then
            i = 1
            go to 40
          end if
CC          set half-life.
          decays = xdumy
        end if
      end if
CC
CC      see if species/total fuel ratio or fraction is specified.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,2,nln1,nsc1,nch1)
      nsc = JWHER(nvl,itabsm,ispacm,1,nch)
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,2,nln2,nsc2,nch2)
      nscs = JWHER(nvl,itabsm,ispacm,1,nchs)
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln3,nsc3,nch3)
      if (NUMC1(mscrnm(nsc1,nln1),nch1) .gt. 0.and.
     *    NUMC1(mscrnm(nsc2,nln2),nch2) .gt. 0) then
CC
CC       get species mass units.
        if (NUMC1(mscrnm(nsc,nln1),nch) .gt. 0) then
          call UNITS(mscrnm(nsc,nln1),nch,i,jdumy,0,0,3)
          if (i .lt. 0.or.i .gt. 7) then
            iferrm(nvl-2,1) = 1
          else
            isunit = i
          end if
        else
CC          default to grams.
          isunit = 0
          ifwrt = 1
          call MBLK1(mscrnm(nsc,nln1),nch)
          if (iflrjs .eq. 0) then
            mscrnm(nsc+nch-1,nln) = malpha(33)
          else
            mscrnm(nsc,nln) = malpha(33)
          end if
        end if
CC
CC        get species mass.
        i = IRDBF(mscrnm(nsc1,nln1),nch1,xdumy)
   50   if (i .gt. 0) then
CC          error.
          iferrm(nvl-2,2) = 1
        else if (i .lt. 0) then
CC          default species mass.
          spmass = 0.0
          ifwrt = 1
          i = IWTBF(mscrnm(nsc1,nln1),nch1,spmass,1,iflrjs)
        else
          if (xdumy .lt. 0.0) then
            i = 1
            go to 50
          end if
CC          set species mass.
          spmass = xdumy
        end if
CC
CC       get total fuel/explosive mass units.
        if (NUMC1(mscrnm(nscs,nln2),nchs) .gt. 0) then
          call UNITS(mscrnm(nscs,nln2),nchs,i,jdumy,0,0,3)
          if (i .lt. 0.or.i .gt. 7) then
            iferrm(nvl-1,1) = 1
          else
            ifunit = i
          end if
        else
CC          default to same as species units.
          ifunit = isunit
          ifwrt = 1
          call UNITS(mscrnm(nscs,nln2),nchs,ifunit,jdumy,0,1,3)
        end if
CC
CC        get total fuel/explosive mass.
        i = IRDBF(mscrnm(nsc2,nln2),nch2,xdumy)
   60   if (i .gt. 0) then
CC          error.
          iferrm(nvl-1,2) = 1
        else if (i .lt. 0) then
CC          default species mass.
          qtfuel = 0.0
          ifwrt = 1
          i = IWTBF(mscrnm(nsc2,nln2),nch2,qtfuel,1,iflrjs)
        else
          if (xdumy .lt. 0.0.or.xdumy .lt. spmass) then
            i = 1
            go to 60
          end if
CC          set total fuel mass.
          qtfuel = xdumy
        end if
        if (spmass .gt. 0.0.and.qtfuel .gt. 0.0) then
          qfract = spmass * constm(isunit+1) / (qtfuel *
     *             constm(ifunit+1))
          if (NUMC1(mscrnm(nsc3,nln3),nch3) .gt. 0) then
            call MBLK1(mscrnm(nsc3,nln3),nch3)
            ifwrt = 1
          end if
        end if
CC
CC      see if fraction of species per total material is specified.
      else if (NUMC1(mscrnm(nsc3,nln3),nch3) .gt. 0) then
CC
CC        get species fraction of total material.
        i = IRDBF(mscrnm(nsc3,nln3),nch3,xdumy)
   70   if (i .gt. 0) then
CC          error.
          iferrm(nvl,1) = 1
        else if (i .lt. 0) then
CC          default fraction.
          qfract = 1.0
          ifwrt = 1
          i = IWTBF(mscrnm(nsc3,nln3),nch3,qfract,nch3-2,iflrjs)
        else
          if (xdumy .le. 0.0) then
            i = 1
            go to 70
          end if
CC          set fraction of total material.
          qfract = xdumy
        end if
      else
        qfract = 1.0
        ifwrt = 1
        i = IWTBF(mscrnm(nsc3,nln3),nch3,qfract,nch3-2,iflrjs)
      end if
CC
CC      see if any source data varies hourly.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        if (IFCHR(mscrnm(nsc,nln),25) .ne. 0) then
          ifhsly = 1
        else if (IFCHR(mscrnm(nsc,nln),14) .ne. 0) then
          ifhsly = 0
        else
          iferrm(nvl,1) = 1
          ifhsly = 0
        end if
      else
        ifhsly = 0
      end if
CC
      ner = IFWER(iferrm,nmxlin*nmxfld)
      if (ifwrt .gt. 0.or.ner .gt. 0.or.(ner .eq. 0.and.ker(1) .gt. 0))
     *    then
CC        rewrite screen data to work file.
        i = IWTSC(numsc,1,mscrnm,ilinem,iferrm)
      end if
      if (ner .gt. 0) then
        if (KERRP(22,0) .ge. 1) go to 160
      end if
      ker(1) = ner
      if (jfers .eq. 0.and.ker(1) .eq. 1) go to 10
CC
CC      if using particulates.
   80 if (ifgpwn .ne. 0) then
CC
CC        screen numsc+1 (11), particulate units and generation data.
        nn = 1
        numf = IRDSC(numsc+nn,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
        nvl = IWHER(6,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
        mscrnm(nsc,nln) = malpha(14)
        nvl = IWHER(3,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
        if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
          if (nvsprt .le. 0) nvsprt = 20
          xdumy = nvsprt
          i = IWTBF(mscrnm(nsc,nln),nch,xdumy,0,iflrjs)
        end if
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
          if (drpmmd .le. 0.0) drpmmd = 200.0
          i = IWTBF(mscrnm(nsc,nln),nch,drpmmd,1,iflrjs)
        end if
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
          if (drpsig .le. 0.0) drpsig = 1.2
          i = IWTBF(mscrnm(nsc,nln),nch,drpsig,3,iflrjs)
        end if
        iferrs = IOSCN(numsc+nn,mscrnm,ilinem,itabsm,ispacm,iferrm,
     *                 lstrw,0)
        if (iferrs .ne. 10) i = IWTSC(numsc+nn,1,mscrnm,ilinem,iferrm)
        if (iferrs .eq. 1) go to 160
        ker(2) = IFWER(iferrm,nmxlin*nmxfld)
        jfers = iferrs
        ifwrt = 0
CC
CC        check user response to first particulate menu.
        if (jfers .eq. 2) then
CC          main menu.
          go to 150
        else if (jfers .ge. 3.and.jfers .le. 7) then
CC          display aux. menus.
          iferrs = IFRSP(jfers,mscrnm,ilinem,itabsm,ispacm,iferrm,0)
          if (iferrs .eq. 1.or.iferrs .eq. 2) then
CC            terminate or main menu.
            jfers = iferrs
            go to 150
          else
CC            redisplay menu.
            go to 80
          end if
        else if (jfers .ge. 8.and.jfers .le. 10) then
          go to 80
        else if (jfers .eq. 11) then
          go to 10
        end if
CC        turn of error array.
        i = JINIT(iferrm,nmxlin*nmxfld,0,0)
CC
CC        check data.
CC
CC        check particle diameter units.
        nvl = IWHER(0,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
          call UNITS(mscrnm(nsc,nln),nch,i,jdumy,0,0,0)
          if (i .lt. 0.or.i .gt. 8) then
            iferrm(nvl,1) = 1
          else
            ipsunt = i
          end if
        else
CC          default to micrometers.
          ipsunt = 6
          ifwrt = 1
          call MBLK1(mscrnm(nsc,nln),nch)
          if (iflrjs .eq. 0) then
            mscrnm(nsc+nch-2,nln) = malpha(47)
            mscrnm(nsc+nch-1,nln) = malpha(39)
          else
            mscrnm(nsc,nln) = malpha(47)
            mscrnm(nsc+1,nln) = malpha(39)
          end if
        end if
CC
CC        check particle settling velocity units.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
          call UNITS(mscrnm(nsc,nln),nch,i,j,0,0,7)
          if (i .lt. 0.or.i .gt. 9) then
            iferrm(nvl,1) = 1
          else if (i .ne. 9.and.(j .lt. 0.or.j .gt. 2)) then
            iferrm(nvl,1) = 1
          else
            ipvunt = i
            jpvunt = j
          end if
        else
CC          default velocity units.
          ipvunt = 0
          jpvunt = 0
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
CC        see if automatic generation of particle size distribution.
        nvl = IWHER(6,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
        if (IFCHR(mscrnm(nsc,nln),14) .ne. 0.or.
     *      mscrnm(nsc,nln) .eq. mblnk1) then
CC          no automatic generation.
          ifgend = 0
        else if (IFCHR(mscrnm(nsc,nln),25) .ne. 0) then
CC          yes generate categories.
          ifgend = 1
        else
CC          error.
          iferrm(nvl,1) = 1
        end if
CC
CC        get ratio of lagrangian to eulerian time scales.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
   90   if (i .gt. 0) then
CC          error.
          if (ifgpwn .gt. 0) iferrm(nvl,1) = 1
        else if (i .lt. 0) then
CC          default ratio.
          betale = 1.0
          ifwrt = 1
          i = IWTBF(mscrnm(nsc,nln),nch,betale,1,iflrjs)
        else
          if (xdumy .lt. 0.0) then
            i = 1
            go to 90
          end if
CC          set ratio of lagrangian to eulerian time scales.
          betale = xdumy
        end if
CC
CC
CC        get number of size categories.
        nvl = IWHER(3,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
        if (i .gt. 0) then
CC          error.
          iferrm(nvl,1) = 1
        else if (i .lt. 0) then
CC          default.
          ifwrt = 1
          nvsprt = 20
          xdumy = nvsprt
          i = IWTBF(mscrnm(nsc,nln),nch,xdumy,0,iflrjs)
        else
CC          set number of categories.
          nvsprt = xdumy
        end if
CC
CC        get mass-median diameter.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
        if (i .gt. 0) then
CC          error.
          iferrm(nvl,1) = 1
        else if (i .lt. 0) then
CC          default.
          ifwrt = 1
          drpmmd = 1.0e-3 / constl(ipsunt+1)
          i = IWTBF(mscrnm(nsc,nln),nch,drpmmd,2,iflrjs)
        else
          drpmmd = xdumy
        end if
CC
CC        get geometric standard deviation.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
        if (i .gt. 0) then
CC          error.
          iferrm(nvl,1) = 1
        else if (i .lt. 0) then
CC          default.
          ifwrt = 1
          drpsig = 1.2
          i = IWTBF(mscrnm(nsc,nln),nch,drpsig,2,iflrjs)
        else
CC          set std. dev.
          drpsig = xdumy
        end if
        ner = IFWER(iferrm,nmxlin*nmxfld)
        if (ifwrt .gt. 0.or.ner .gt. 0.or.(ner .eq. 0.and.
     *      ker(2) .gt. 0)) then
CC          rewrite screen data.
          i = IWTSC(numsc+nn,1,mscrnm,ilinem,iferrm)
        end if
        if (ner .gt. 0) then
          if (KERRP(22,0) .ge. 1) go to 160
        end if
        ker(2) = ner
        if (ker(2) .eq. 1) then
          if (jfers .eq. 0.or.ifgend .ne. 0) go to 80
        end if
CC
CC        if generate particle size distribution.
        if (ifgend .ne. 0) then
          nn = 2
          nsubs = nsubsc(numsc+nn)
CC          set all parameters to missing or to be computed.
          j = JRNIT(drpupr,nmxdrp,rmissd,0)
          j = JRNIT(drplwr,nmxdrp,rmissd,0)
          j = JRNIT(pctmat,nmxdrp,rmissd,0)
          j = JRNIT(vspart,nmxdrp,rmissc,0)
          j = JRNIT(gammad,nmxdrp,rmissc,0)
CC          convert mass-median to micrometers.
          xdumy = drpmmd * constl(ipsunt+1) / constl(8)
CC          generate distribution.
          call DDST1(xdumy,drpsig,nvsprt,drpupr,drplwr,pctmat)
CC          convert size back to user units.
          if (ipsunt .ne. 7) then
            xdumy = constl(8) / constl(ipsunt+1)
            do 100 i=1,nvsprt
CC              convert micrometers to desired units.
              drpupr(i) = drpupr(i) * xdumy
              drplwr(i) = drplwr(i) * xdumy
  100       continue
          end if
CC          write particle data to work file.
          ner = IPACK(nsubs,drpupr,drplwr,pctmat,vspart,gammad,dumy,
     *                nmxdrp,nvsprt,numsc+nn,mscrnm,ilinem,itabsm,
     *                ispacm,iferrm,4,1)
        end if
CC
CC        screen numsc+2 (12), particulate data.
        nsub = 1
        nsubs = nsubsc(numsc+2)
        nn = 2
CC        read screen menu and screen data.
  110   numf = IRDSC(numsc+nn,nsub,mscrnm,ilinem,itabsm,ispacm,
     *               iferrm,1)
CC        set particle distribution units abbrev.
        nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
        call UNITS(mscrnm(17,nln-2),2,ipsunt,0,0,1,0)
        call MVCH1(mscrnm(17,nln-2),mscrnm(30,nln-2),2)
        call UNITS(mscrnm(55,nln-3),5,ipvunt,jpvunt,0,1,7)
        do 120 i=1,10
          nvl = IWHER(i,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
          xdumy = (nsub - 1) * 10 + i
          j = IWTBF(mscrnm(5,nln),2,xdumy,0,iflrjs)
  120   continue
CC        display screen menu.
        iferrs = IOSCN(numsc+nn,mscrnm,ilinem,itabsm,ispacm,iferrm,
     *                 lstrw,0)
        tmp1 = airtmp
        tmp2 = airprs
        airtmp = constc(itpunt+1,2) * (airtmp - constc(itpunt+1,1))
        airprs = airprs * constp(iprunt+1)
        do 130 i=1,10
          nvl = IWHER(i,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
          nsc4 = JWHER(nvl,itabsm,ispacm,4,nch4)
          j4 = IRDBF(mscrnm(nsc4,nln),nch4,vsp)
          nsc5 = JWHER(nvl,itabsm,ispacm,5,nch5)
          j5 = IRDBF(mscrnm(nsc5,nln),nch5,gam)
          if (j4 .eq. -1.or.j5 .eq. -1) then
            if (airtmp .lt. vllimt(12).or.airtmp .gt. vulimt(12).or.
     *          airprs .lt. vllimt(13).or.airprs .gt. vulimt(13).or.
     *          IFEQU(densty,1.0) .eq. 0) then
              if (KERRS(30,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge.
     *            1) go to 160
              go to 140
            end if
CC            convert diameters to micrometers
            if (j4 .eq. -1.or.vsp .le. 0.0) then
              nsc1 = JWHER(nvl,itabsm,ispacm,1,nch1)
              j1 = IRDBF(mscrnm(nsc1,nln),nch1,drpup)
              nsc2 = JWHER(nvl,itabsm,ispacm,2,nch2)
              j2 = IRDBF(mscrnm(nsc2,nln),nch2,drplw)
              drpup = drpup * constl(ipsunt+1) / constl(8)
              if (drplw .le. 0.0) then
                drplw = drpup
              else
                drplw = drplw * constl(ipsunt+1) / constl(8)
              end if
              davg = DRPAV(drpup * 5.0e-5, drplw * 5.0e-5, 0.1) *
     *               2.0e4
CC                default particle settling velocity.
              vsp = TRMVS(davg * 5.0e-5,i) * 0.01 /
     *                  constl(ipvunt+1) * constt(jpvunt+1)
              if (vsp .lt. 0.0) then
                airtmp = tmp1
                airprs = tmp2
                go to 80
              end if
              if (vsp .gt. 0.0) then
                j = IWTBF(mscrnm(nsc4,nln),nch4,vsp,4,iflrjs)
              end if
            end if
            if (j5 .eq. -1) then
              a = vspart(1)
              vspart(1) = vsp
              gam = FGAMA(1)
              vspart(1) = a
              j = IWTBF(mscrnm(nsc5,nln),nch5,gam,4,iflrjs)
            end if
          end if
  130   continue
  140   airtmp = tmp1
        airprs = tmp2
CC        save screen menu.
        if (iferrs .ne. 10) i = IWTSC(numsc+nn,nsub,mscrnm,ilinem,
     *                                iferrm)
CC        if terminate.
        if (iferrs .eq. 1) go to 160
        ker(3) = IFWER(iferrm,nmxlin*nmxfld)
        jfers = iferrs
        ifwrt = 0
CC
CC        check users response.
        if (jfers .eq. 2) then
          ner = IPACK(nsubs,drpupr,drplwr,pctmat,vspart,gammad,dumy,
     *                nmxdrp,nvsprt,numsc+nn,mscrnm,ilinem,itabsm,
     *                ispacm,iferrm,4,0)
          if (ner .eq. 3) then
            if (KERRS(61,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *          go to 160
          else if (ner .gt. 0) then
            if (KERRS(29,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *          go to 160
            if (ner .eq. 1) ifrdwt(3) = -1
          end if
          go to 150
        else if (jfers .ge. 3.and.jfers .le. 7) then
          iferrs = IFRSP(jfers,mscrnm,ilinem,itabsm,ispacm,iferrm,0)
CC          terminate or main menu.
          if (iferrs .eq. 1.or.iferrs .eq. 2) then
            jfers = iferrs
            go to 150
          else
CC            redisplay menu.
            go to 110
          end if
        else if (jfers .ge. 8.and.jfers .le. 10) then
          go to 110
        else if (jfers .eq. 11) then
CC          go to previous menu.
          nsub = nsub - 1
          ner = IPACK(nsubs,drpupr,drplwr,pctmat,vspart,gammad,dumy,
     *                nmxdrp,nvsprt,numsc+nn,mscrnm,ilinem,itabsm,
     *                ispacm,iferrm,4,0)
          if (ner .eq. 3) then
            if (KERRS(61,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *          go to 160
          else if (ner .gt. 0) then
            if (KERRS(29,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *          go to 160
          end if
          if (nsub .ge. 1) go to 110
          ker(3) = ner
          go to 80
        end if
CC        next page.
        if (nsub .lt. nsubs) then
          ifdat = 0
CC          see if more particle menus needed or if data in next menu.
          nvl = IWHER(10,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
          if (NUMC1(mscrnm(nsc,nln),nch) .eq. 0) then
            if (nvsprt .gt. nsub*10.or.IRDSC(numsc+nn,nsub+1,mscrnm,
     *          ilinem,itabsm,ispacm,iferrm,1) .gt. 0) then
              ifdat = 1
            end if
          else
            ifdat = 1
          end if
CC          go to next data set for screen numsc+nn menu.
          if (ifdat .ne. 0) then
            nsub = nsub + 1
            go to 110
          end if
        end if
CC          pack particulate data and go to next source menu.
        ner = IPACK(nsubs,drpupr,drplwr,pctmat,vspart,gammad,dumy,
     *              nmxdrp,nvsprt,numsc+nn,mscrnm,ilinem,itabsm,
     *              ispacm,iferrm,4,0)
        if (ner .eq. 3) then
          if (KERRS(61,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *        go to 160
        else if (ner .gt. 0) then
          if (KERRS(29,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *        go to 160
        end if
        if (jfers .eq. 0.and.ner .eq. 1) then
          go to 110
        end if
        ker(3) = ner
CC        reset number of categories, nvsprt.
        numf = IRDSC(numsc+1,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
        nvl = IWHER(3,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
        xdumy = nvsprt
        i = IWTBF(mscrnm(nsc,nln),nch,xdumy,0,iflrjs)
        i = IWTSC(numsc+1,1,mscrnm,ilinem,iferrm)
      else
        nvsprt = 0
      end if
      jfnxt = 3
      go to 170
  150 jfnxt = 11
      go to 170
  160 jfnxt = 12
  170 return
      end
      Subroutine DDST1(drmmd,sigdr,nvspr,drupr,drlwr,drfrq)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - DDST1 Version 01.1                          ---
CC--- Subroutine description - computes the distribution of particle---
CC---                          diameters and fraction of material   ---
CC---                          using either a uniform or log-normal ---
CC---                          distribution, depending on sigdr.   ---
CC---------------------------------------------------------------------
CC
CC        drmmd - particle mass-median diameter in micrometers.
CC        sigdr - geometric standard deviation of the distribution
CC                 if <= 1.0, generate uniform distribution.
CC                 if > 1.0, generate lognormal distribution.
CC        nvspr - number of particle size categories.
CC        drupr - returned upper diameter limit of each particle size
CC                 category in micrometers (descending order).
CC        drlwr - returned lower diameter limit of each particle size
CC                 category in micrometers (descending order).
CC        drfrq - returned mass fraction of particles in each category.
CC
CC*********************************************************************
CC
      integer*2 n
CC
      double precision ERFXS,t11,t12
CC
      dimension drupr(nvspr),drlwr(nvspr),drfrq(nvspr)
CC
CC**********************************************************************
CC
      if (sigdr .le. 1.0.or.nvspr .eq. 1) then
CC        uniform distribution.
        cnst1 = 0.0
        cnst2 = 0.0
        cnst3 = 1.0
      else
CC        range of particle sizes is 2.3 standard deviations on each
CC        side of the median for log-normal distribution.
        cnst1 = ALOG(sigdr)
        cnst2 = 4.6 * cnst1 / FLOAT(nvspr)
        cnst3 = 1.0 / sigdr**2.3
      end if
      do 10 n=1,nvspr+1
CC        calculate class interval particle diameters.
        a = drmmd * FSEXP(FLOAT(n-1) * cnst2) * cnst3
        if (n .le. nvspr) then
          drlwr(nvspr+1-n) = a
        end if
        if (n .gt. 1) then
          drupr(nvspr+1-(n-1)) = a
        end if
   10 continue
CC      compute the mass fraction.
      if (nvspr .gt. 1.and.sigdr .gt. 1.0) then
        cnst2 = 0.70710678 / cnst1
        cnst1 = ALOG(drmmd)
        t13 = 0.0
        do 20 n=1,nvspr
          t9 = ALOG(drlwr(n))
          t10 = ALOG(drupr(n))
          t11 = (t9 - cnst1) * cnst2
          t12 = (t10 - cnst1) * cnst2
          drfrq(n) = 0.5 * (ERFXS(t12) - ERFXS(t11))
          if (drfrq(n) .gt. 1.0) drfrq(n) = 1.0
          t13 = t13 + drfrq(n)
   20   continue
        t13 = 1.0 / t13
        do 30 n=1,nvspr
          drfrq(n) = drfrq(n) * t13
   30   continue
      else
        do 40 n=1,nvspr
          drfrq(n) = 1.0 / FLOAT(nvspr)
   40   continue
      end if
CC
      return
      end
