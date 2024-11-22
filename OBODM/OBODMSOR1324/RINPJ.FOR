      Subroutine RINPJ
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - RINPJ Version 01.1                          ---
CC--- Subroutine description - displays and reads source data.      ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
CC
      character*58 mfnam
CC
      dimension jflgs(50),ker(11)
CC
      include 'OCNTRL.INC'
      include 'OCDSPM.INC'
      include 'OCALCS.INC'
      include 'ONAMES.INC'
      include 'OWNTRL.INC'
CC
      equivalence (mfnam,mscrch(1)),(jfnxt,functx(1)),
     *            (jflgs(1),functx(2)),(ker(1),functx(52)),
     *            (numsc,functx(63)),(isorc,functx(70)),
     *            (ifdtl,functx(72))
CC
CC********************************************************************
CC
CC      if using hourly source data.
      if (ifhsly .ne. 0) then
        if (jfnxt .eq. 9) go to 40
CC
CC        screen numsc+9 (19), hourly source data.
   10   nn = 9
        iferrs = IDSPL(numsc+nn,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1,
     *                 lstrw,0)
        if (iferrs .eq. 1) go to 60
        ker(8) = IFWER(iferrm,nmxlin*nmxfld)
        jfers = iferrs
        ifwrt = 0
CC
CC        check user response to hourly source data file name menu.
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
            go to 10
          end if
        else if (jfers .ge. 8.and.jfers .le. 10) then
          go to 10
        else if (jfers .eq. 11) then
          go to 70
        end if
CC        turn of error array.
        i = JINIT(iferrm,nmxlin*nmxfld,0,0)
CC
CC        check data.
CC
CC        check hourly source data file name.
        nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
        if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
          call MBLK1(mshnam,22)
          call MVCH1(mscrnm(nsc,nln),mshnam,MIN0(nch,22))
          i = LRJST(mshnam,22,1)
CC          open data file as logical units ishfil.
          ishfil = 28
CC          combine file name and directory.
          call FNAME(mfnam,58,mdirec,mshnam)
CC            formatted character.
          OPEN (unit=ishfil,file=mfnam,iostat=ier,err=20,
     *         status='old',form='formatted')
          go to 30
   20     iferrm(nvl,1) = 1
          if (KERRS(3,mshnam,ier,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *        go to 60
        else
          iferrm(nvl,1) = 1
        end if
   30   ner = IFWER(iferrm,nmxlin*nmxfld)
        if (ifwrt .gt. 0.or.ner .gt. 0.or.(ner .eq. 0.and.
     *      ker(8) .gt. 0)) then
CC          rewrite screen data.
          i = IWTSC(numsc+nn,1,mscrnm,ilinem,iferrm)
        end if
        if (ner .gt. 0) then
          if (KERRP(22,0) .ge. 1) go to 60
        end if
        ker(8) = ner
        if (ker(8) .eq. 1) then
          if (jfers .eq. 0) go to 10
        end if
CC
CC        screen numsc+10 (20), which source data are hourly source data.
   40   nn = 10
        numf = IRDSC(numsc+nn,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
CC        mass units.
        j1 = 3
CC        heat/mass units.
        j2 = 13
CC        mass/time units.
        j3 = 12
        nvl = IWHER(0,ilinem,itabsm,ispacm,2,1,nln,nsc,nch)
CC        total mass of material or emission rate.
        iunit = IVGET(iqunit(1),2)
        junit = IVGET(iqunit(1),1)
        call UNITS(mscrnm(nsc-29,nln),5,iunit,junit,0,1,j1)
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
CC        material heat content.
        iunit = IVGET(jqunit(1),1)
        if (iunit .gt. 4) iunit = 0
        junit = IVGET(jqunit(1),2)
        call UNITS(mscrnm(nsc-35,nln),5,iunit,junit,0,1,j2)
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
CC        fuel burn rate.
        iunit = IVGET(kqunit(1),1)
        junit = IVGET(kqunit(1),2)
        call UNITS(mscrnm(nsc-57,nln),5,iunit,junit,0,1,j3)
CC        display screen menu.
        iferrs = IOSCN(numsc+nn,mscrnm,ilinem,itabsm,ispacm,iferrm,
     *                 lstrw,0)
CC        save screen menu.
        if (iferrs .ne. 10) i = IWTSC(numsc+nn,1,mscrnm,ilinem,iferrm)
CC        if terminate.
        if (iferrs .eq. 1) go to 60
        ker(9) = IFWER(iferrm,nmxlin*nmxfld)
        jfers = iferrs
        ifwrt = 0
CC
CC        check user response to first set of hourly source parameters
CC        menu.
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
            go to 40
          end if
        else if (jfers .ge. 8.and.jfers .le. 10) then
          go to 40
        else if (jfers .eq. 11) then
          go to 10
        end if
CC        turn of error array.
        i = JINIT(iferrm,nmxlin*nmxfld,0,0)
CC
CC        check data.
CC
CC        check if source number in file.
        nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
        if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
          i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
          if (i .ne. 0) then
            iferrm(nvl,1) = 1
          else if (xdumy .gt. 0.0) then
            ivarso(1) = xdumy
          else
            iferrm(nvl,1) = 1
          end if
        else
          iferrm(nvl,1) = 1
        end if
CC
CC        check if total mass of material is in data file.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
          i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
          if (i .ne. 0) then
            iferrm(nvl,1) = 1
          else if (xdumy .gt. 0.0) then
            ivarso(2) = xdumy
          else
            iferrm(nvl,1) = 1
          end if
        else
          ivarso(2) = 0
        end if
CC
CC        check if heat content is in data file.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
          i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
          if (i .ne. 0) then
            iferrm(nvl,1) = 1
          else if (xdumy .gt. 0.0) then
            ivarso(3) = xdumy
          else
            iferrm(nvl,1) = 1
          end if
        else
          ivarso(3) = 0
        end if
CC
CC        check if burn rate is in data file.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
          i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
          if (i .ne. 0) then
            iferrm(nvl,1) = 1
          else if (xdumy .gt. 0.0) then
            ivarso(4) = xdumy
          else
            iferrm(nvl,1) = 1
          end if
        else
          ivarso(4) = 0
        end if
CC
        ner = IFWER(iferrm,nmxlin*nmxfld)
        if (ifwrt .gt. 0.or.ner .gt. 0.or.(ner .eq. 0.and.
     *      ker(9) .gt. 0)) then
CC          rewrite screen data.
          i = IWTSC(numsc+nn,1,mscrnm,ilinem,iferrm)
        end if
        if (ner .gt. 0) then
          if (KERRP(22,0) .ge. 1) go to 60
        end if
        ker(9) = ner
        if (ker(9) .eq. 1) then
          if (jfers .eq. 0) go to 10
        end if
      end if
      jfnxt = 10
      go to 80
   50 jfnxt = 11
      go to 80
   60 jfnxt = 12
      go to 80
   70 if (ifdtl .ne. 0) then
        jfnxt = 7
      else
        jfnxt = 6
      end if
   80 return
      end
