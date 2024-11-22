      Subroutine RINPG
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - RINPG Version 01.1                          ---
CC--- Subroutine description - displays and reads source data.      ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      character*2 mesg3
      character*6 mesg1
      character*7 mesg2
CC
      dimension jflgs(50),ker(11),isovl(50,7),xsovl(50,25)              012599
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
     *            (isovl(1,1),isrect(1)),(xsovl(1,1),qemiss(1))
CC
      data mesg1/'Copied'/,mesg2/'Deleted'/,mesg3/'to'/
CC
CC********************************************************************
CC
      go to (250,250,10,20,150),jfnxt
CC      screen numsc+3 (13), get source number.
CC
   10 iflg = 0
      lsorc = 0
      ner = 0
   20 nn = 3
      numf = IRDSC(numsc+nn,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
CC      get total number of sources from this screen.
      nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
        if (i .eq. 0) then
          nsourc = xdumy
        else
          nsourc = 0
        end if
      else
        nsourc = 0
      end if
      if (nsourc .eq. 0) then
        xdumy = nsourc
        i = IWTBF(mscrnm(nsc,nln),nch,xdumy,0,iflrjs)
      end if
CC      blank out leading and ending brackets.
      mscrnm(nsc-1,nln) = mblnk1
      mscrnm(nsc+nch,nln) = mblnk1
      if (ner .eq. 0) then
CC        blank out source number.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        if (ifinit .eq. 0) then
          call MBLK1(mscrnm(nsc,nln),nch)
        else
          lsorc = lsorc + 1
          if (lsorc .le. nsourc) then
            xdumy = lsorc
            i = IWTBF(mscrnm(nsc,nln),nch,xdumy,0,iflrjs)
          else
            call MBLK1(mscrnm(nsc,nln),nch)
          end if
        end if
CC        see if previous was source copy.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        call MBLK1(mscrnm(nsc,nln),nch)
        if (iflg .eq. 1.or.iflg .eq. 3) then
CC          indicate source number copied.
          call MVCH1(mesg1,mscrnm(40,nln+1),6)
          xdumy = jsorc
          i = IWTBF(mscrnm(47,nln+1),2,xdumy,0,iflrjs)
          call MBLK1(mscrnm(nsc,nln),nch)
          call MVCH1(mesg3,mscrnm(50,nln+1),2)
          xdumy = isorc
          i = IWTBF(mscrnm(53,nln+1),2,xdumy,0,iflrjs)
        end if
CC        see if previous was delete.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        call MBLK1(mscrnm(nsc,nln),nch)
        if (iflg .eq. 2.or.iflg .eq. 3) then
CC          indicate source number deleted.
          call MVCH1(mesg2,mscrnm(40,nln+1),7)
          xdumy = ksorc
          i = IWTBF(mscrnm(48,nln+1),2,xdumy,0,iflrjs)
        end if
      end if
CC      shift data line indicators down 1.
      i1 = ilinem(1)
      i2 = itabsm(1,1)
      i3 = ispacm(1,1)
      i4 = iferrm(1,1)
      do 30 i=1,3
        ilinem(i) = ilinem(i+1)
        itabsm(i,1) = itabsm(i+1,1)
        ispacm(i,1) = ispacm(i+1,1)
        iferrm(i,1) = iferrm(i+1,1)
   30 continue
      ilinem(4) = 0
      itabsm(4,1) = 0
      ispacm(4,1) = 0
      iferrm(4,1) = 0
CC      display screen menu.
      iferrs = IOSCN(numsc+nn,mscrnm,ilinem,itabsm,ispacm,iferrm,lstrw,
     *               0)
CC      shift data line indicators back 1.
      do 40 i=1,3
        ilinem(5-i) = ilinem(4-i)
        itabsm(5-i,1) = itabsm(4-i,1)
        ispacm(5-i,1) = ispacm(4-i,1)
        iferrm(5-i,1) = iferrm(4-i,1)
   40 continue
      ilinem(1) = i1
      itabsm(1,1) = i2
      ispacm(1,1) = i3
      iferrm(1,1) = i4
CC      write screen data back out.
      if (iferrs .ne. 10) i = IWTSC(numsc+nn,1,mscrnm,ilinem,iferrm)
CC      if terminate.
      if (iferrs .eq. 1) go to 240
      ker(4) = IFWER(iferrm,nmxlin*nmxfld)
      jfers = iferrs
      ifwrt = 0
      iflg = 0
CC
CC      check user response to menu.
      if (jfers .eq. 2) then
CC        main menu.
        go to 230
      else if (jfers .ge. 3.and.jfers .le. 7) then
CC        display aux. menus.
        iferrs = IFRSP(jfers,mscrnm,ilinem,itabsm,ispacm,iferrm,0)
        if (iferrs .eq. 1.or.iferrs .eq. 2) then
CC          terminate or main menu.
          jfers = iferrs
          go to 230
        else
CC          redisplay menu.
          go to 20
        end if
      else if (jfers .ge. 8.and.jfers .le. 10) then
        go to 20
      else if (jfers .eq. 11) then
CC        go to previous menu.
        if (ifgpwn .gt. 0) go to 210
        go to 200
      end if
CC      turn of error array.
      i = JINIT(iferrm,nmxlin*nmxfld,0,0)
CC
CC      check data.
CC
CC      get source number.
      isorc = 0
      jsorc = 0
      ksorc = 0
      nvl = IWHER(2,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
      i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
   50 if (i .gt. 0) then
        iferrm(nvl,1) = 1
      else if (i .lt. 0) then
        if (i .ne. -3) then
          i = 1
          go to 50
        else
CC          default.
          isorc = 0
        end if
      else
CC        check range.
        if (xdumy .le. 0.0.or.xdumy .gt. nmxsor) then
          i = 1
          go to 50
        end if
CC        set source number.
        isorc = xdumy
      end if
CC
CC      get source number to copy data from.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
   60   if (i .ne. 0) then
          iferrm(nvl,1) = 1
        else
CC          check range.
          if (xdumy .le. 0.0.or.xdumy .gt. nsourc) then
            i = 1
            go to 60
          end if
          if (isorc .le. 0) then
CC            source number to copy into is not set.
            iferrm(nvl-1,1) = 1
          end if
CC          set copy source number.
          jsorc = xdumy
        end if
      end if
CC
CC      get source number to delete.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
   70   if (i .ne. 0) then
          iferrm(nvl,1) = 1
        else
CC          check range.
          if (xdumy .le. 0.0.or.xdumy .gt. nsourc) then
            i = 1
            go to 70
          end if
CC          set source number to delete.
          ksorc = xdumy
        end if
      end if
CC      if any errors, redisplay menu.
      ner = IFWER(iferrm,nmxlin*nmxfld)
      if (ner .gt. 0.or.(ner .eq. 0.and.ker(4) .gt. 0)) then
CC        rewrite screen data to work file.
        i = IWTSC(numsc+nn,1,mscrnm,ilinem,iferrm)
      end if
      if (ner .gt. 0) then
        if (KERRP(22,0) .ge. 1) go to 250
      end if
      ker(4) = ner
      if (ker(4) .eq. 1) go to 20
CC
CC      see if source copy.
      if (isorc .gt. 0.and.jsorc .gt. 0) then
CC          copy source jsorc to source isorc.
        if (isorc .gt. nsourc) then
          nsourc = nsourc + 1
          numf = IRDSC(numsc+3,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
CC          set total number of sources.
          nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
          xdumy = nsourc
          i = IWTBF(mscrnm(nsc,nln),nch,xdumy,0,iflrjs)
          i = IWTSC(numsc+3,1,mscrnm,ilinem,iferrm)
          isorc = nsourc
        end if
        do 80 j=1,3
          if (j .eq. 1) then
            n1 = 4
          else if (j .eq. 2) then
            itype = IVGET(isotyp(jsorc),2)
            jtype = IVGET(isotyp(jsorc),1)
            if (jtype .ne. 2) then
              if (itype .ne. 1) then
CC                quasi-continuous.
                n1 = 5
              else
CC                instantaneous
                n1 = 6
              end if
            else
CC              line source
              n1 = 7
            end if
          else
            n1 = 8
          end if
CC          get data from source number jsorc.
          numf = IRDSC(numsc+n1,jsorc,mscrnm,ilinem,itabsm,ispacm,
     *                 iferrm,1)
CC          write data to source number isorc.
          i = IWTSC(numsc+n1,isorc,mscrnm,ilinem,iferrm)
   80   continue
        iflg = 1
        do 90 i=1,25
         xsovl(isorc,i) = xsovl(jsorc,i)
         if (i .gt. 7) go to 90
         isovl(isorc,i) = isovl(jsorc,i)
   90   continue
        do 100 i=1,24
          isofrq(i,isorc) = isofrq(i,jsorc)
  100   continue
      end if
CC      if delete source.
      if (ksorc .gt. 0) then
CC        delete source ksorc.
        n2 = 0
        n = ksorc
  110   n = n + 1
        if (n .le. nsourc+1) then
          do 120 k=1,3
            if (n .lt. nsourc+1) then
              if (k .eq. 1) then
                n1 = 4
              else if (k .eq. 2) then
                itype = IVGET(isotyp(n),2)
                jtype = IVGET(isotyp(n),1)
CC                volume or line source.
                n1 = 5
                if (jtype .eq. 2) then
                  n1 = 6
                end if
                n2 = n1
              else
                n1 = 7
              end if
CC              screen plus data for source n.
              numf = IRDSC(numsc+n1,n,mscrnm,ilinem,itabsm,ispacm,
     *                     iferrm,1)
            else
CC              get screen only, no data.
              if (k .eq. 1) then
                n1 = 4
              else if (k .eq. 2) then
                n1 = n2
                if (n1 .le. 0) n1 = 5
              else
                n1 = 7
              end if
              numf = IRDSC(numsc+n1,n,mscrnm,ilinem,itabsm,ispacm,
     *                     iferrm,0)
            end if
CC            write screen data back out.
            i = IWTSC(numsc+n1,n-1,mscrnm,ilinem,iferrm)
  120     continue
          if (n .le. nsourc) then
            do 130 i=1,25
              xsovl(n-1,i) = xsovl(n,i)
              if (i .gt. 7) go to 130
              isovl(n-1,i) = isovl(n,i)
  130       continue
            do 140 i=1,24
              isofrq(i,n-1) = isofrq(i,n)
  140       continue
          end if
          go to 110
        end if
        nsourc = MAX0(nsourc - 1,0)
        numf = IRDSC(numsc+3,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
CC        set total number of sources.
        nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
        xdumy = nsourc
        i = IWTBF(mscrnm(nsc,nln),nch,xdumy,0,iflrjs)
        i = IWTSC(numsc+3,1,mscrnm,ilinem,iferrm)
        if (iflg .eq. 0) then
          iflg = 2
        else
          iflg = 3
        end if
      end if
      if (iflg .ne. 0) go to 20
      if (isorc .le. 0) then
CC        no source number entered, default source number if no sources
CC        else go to end of source loop.
        if (nsourc .le. 0) then
          isorc = 1
        else
          go to 220
        end if
      else if (isorc .gt. nsourc) then
        isorc = nsourc + 1
      end if
CC
CC      screen numsc+4 (14), get source data for source isorc.
  150 nn = 4
CC      read screen.
      numf = IRDSC(numsc+nn,isorc,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
CC      indicate source number in screen menu.
      xdumy = isorc
      nvl = IWHER(0,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      i = IWTBF(mscrnm(46,nln-3),2,xdumy,0,iflrjs)
      if ((heatdf .gt. 0.0.or.burndf .gt. 0.0).and.jflgs(isorc) .eq. 0)
     *    then
        jflgs(isorc) = 1
CC        default burn rate and heat content.
        nvl = IWHER(5,ilinem,itabsm,ispacm,0,2,nln,nsc,nch)
        i = IWTBF(mscrnm(nsc,nln),nch,heatdf,4,iflrjs)
        nsc = JWHER(nvl,itabsm,ispacm,1,nch)
        call UNITS(mscrnm(nsc,nln),nch,iduhet,jduhet,0,1,13)
        hetcnt(isorc) = heatdf
        iferrm(nvl,1) = 0
        iferrm(nvl,2) = 0
        jqunit(isorc) = IVSTO(jqunit(isorc),1,iduhet)
        jqunit(isorc) = IVSTO(jqunit(isorc),2,jduhet)
        if (burndf .gt. 2.5) then
          nvl = IWHER(6,ilinem,itabsm,ispacm,0,2,nln,nsc,nch)
          kqunit(isorc) = IVSTO(kqunit(isorc),1,idubrn)
          kqunit(isorc) = IVSTO(kqunit(isorc),2,jdubrn)
          i = IWTBF(mscrnm(nsc,nln),nch,burndf,4,iflrjs)
          nsc = JWHER(nvl,itabsm,ispacm,1,nch)
          call UNITS(mscrnm(nsc,nln),nch,idubrn,jdubrn,0,1,12)
          brnrat(isorc) = burndf
          iferrm(nvl,1) = 0
          iferrm(nvl,2) = 0
        else
          nvl = IWHER(6,ilinem,itabsm,ispacm,0,2,nln,nsc,nch)
          call MBLK1(mscrnm(nsc,nln),nch)
          if (iflrjs .eq. 0) then
            mscrnm(nsc+nch-1,nln) = malpha(3)
          else
            mscrnm(nsc,nln) = malpha(3)
          end if
        end if
      end if
CC      display screen menu.
      iferrs = IOSCN(numsc+nn,mscrnm,ilinem,itabsm,ispacm,iferrm,lstrw,
     *               0)
CC      write data to work file.
      if (iferrs .ne. 10)i = IWTSC(numsc+nn,isorc,mscrnm,ilinem,iferrm)
CC      if terminate.
      if (iferrs .eq. 1) go to 240
      ker(5) = IFWER(iferrm,nmxlin*nmxfld)
      jfers = iferrs
      ifwrt = 0
CC
CC      check user response to menu.
      if (jfers .eq. 2) then
        go to 230
      else if (jfers .ge. 3.and.jfers .le. 7) then
CC        display aux. menus.
        iferrs = IFRSP(jfers,mscrnm,ilinem,itabsm,ispacm,iferrm,0)
        if (iferrs .eq. 1.or.iferrs .eq. 2) then
CC          terminate or main menu.
          jfers = iferrs
          go to 230
        else
CC          redisplay menu.
          go to 150
        end if
      else if (jfers .ge. 8.and.jfers .le. 10) then
        go to 150
      else if (jfers .eq. 11) then
        go to 20
      end if
CC      turn of error array.
      i = JINIT(iferrm,nmxlin*nmxfld,0,0)
CC
CC      check data.
CC
CC      get source id name.
      nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
      call MBLK1(msoids(1,isorc),36)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        call MVCH1(mscrnm(nsc,nln),msoids(1,isorc),MIN0(nch,36))
        i = LRJST(msoids(1,isorc),36,1)
        n = NUMC1(mscrnm(nsc,nln),nch)
        if (n .gt. 0.and.n .lt. 36) then
          i = LRJST(mscrnm(nsc,nln),nch,iflrjs)
          ifwrt = 1
        end if
      end if
CC
CC      get source type (1-volume,2-line).
      idumy = IVGET(isotyp(isorc),1)
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (mscrnm(nsc,nln) .ne. mblnk1) then
        if (IFCHR(mscrnm(nsc,nln),22) .ne. 0) then
CC          volume source.
          idumy = 1
        else if (IFCHR(mscrnm(nsc,nln),12) .ne. 0) then
CC          line source.
          idumy = 2
        else
          iferrm(nvl,1) = 1
        end if
      else
CC        default.
        idumy = 1
        ifwrt = 1
        mscrnm(nsc,nln) = malpha(22)
      end if
      isotyp(isorc) = IVSTO(isotyp(isorc),1,idumy)
      jtype = idumy
CC
CC      get source emission type (1-instantaneous,2-quasi-continuous
CC      square wave)
      idumy = IVGET(isotyp(isorc),2)
      jdumy = idumy
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (mscrnm(nsc,nln) .ne. mblnk1) then
        if (IFCHR(mscrnm(nsc,nln),9) .ne. 0) then
CC          instantaneous source.
          idumy = 1
        else if (IFCHR(mscrnm(nsc,nln),17) .ne. 0) then
CC          quasi-continuous source.
          idumy = 2
        else
          iferrm(nvl,1) = 1
        end if
      else
CC        default.
        idumy = 1
        ifwrt = 1
        mscrnm(nsc,nln) = malpha(9)
      end if
      isotyp(isorc) = IVSTO(isotyp(isorc),2,idumy)
      itype = idumy
      if (jdumy .eq. 1.and.idumy .gt. 1) then
        ifchg = 2
      else if (jdumy .gt. 1.and.idumy .le. 1) then
        ifchg = 1
      else
        ifchg = 0
      end if
CC    set indices.
CC      mass units.
      j1 = 3
CC      heat/mass units.
      j2 = 13
CC      mass/time units.
      j3 = 12
CC
CC      get units of total mass or rate of material burned.
      nvl = IWHER(4,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .eq. 0) then
CC        default.
        iunt = 0
        junt = 0
        call UNITS(mscrnm(nsc,nln),nch,iunt,junt,0,1,j1)
        ifwrt = 1
      else
        call UNITS(mscrnm(nsc,nln),nch,iunt,junt,0,0,j1)
        if (iunt .lt. 0.or.junt .lt. 0) then
CC          error.
          iferrm(nvl,1) = 1
        end if
      end if
      if (iunt .ge. 0) then
        if (iunt .eq. 7) then
          if (densty .gt. 0.0) then
            iqunit(isorc) = IVSTO(iqunit(isorc),2,iunt)
          else
            iferrm(nvl,1) = 1
          end if
        else
          iqunit(isorc) = IVSTO(iqunit(isorc),2,iunt)
        end if
      end if
      if (junt .ge. 0) then
        iqunit(isorc) = IVSTO(iqunit(isorc),1,junt)
      end if
      iunt1 = iunt
CC
CC      get total mass of material or emission rate.
      nsc = JWHER(nvl,itabsm,ispacm,2,nch)
      i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
  160 if (i .gt. 0) then
CC        error.
        iferrm(nvl,2) = 1
      else if (i .lt. 0) then
CC        default source mass.
        if (qemiss(isorc) .le. 0.0) then
          qemiss(isorc) = 1.0
        end if
        ifwrt = 1
        i = IWTBF(mscrnm(nsc,nln),nch,qemiss(isorc),1,iflrjs)
      else
        if (xdumy .le. 0.0) then
          i = 1
          go to 160
        end if
CC        set source mass or emission rate.
        qemiss(isorc) = xdumy
      end if
CC
CC      get units of heat content.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .eq. 0) then
CC        default.
        iunt = 0
        junt = 0
        call UNITS(mscrnm(nsc,nln),nch,iunt,junt,0,1,j2)
        ifwrt = 1
      else
        call UNITS(mscrnm(nsc,nln),nch,i,j,0,0,j2)
        if (i .lt. 0.or.j .lt. 0) then
CC          error.
          iferrm(nvl,1) = 1
          iunt = 0
          junt = 0
        else
CC          set units index.
          iunt = i
          junt = j
        end if
      end if
      if (iunt .ge. 0) then
        jqunit(isorc) = IVSTO(jqunit(isorc),1,iunt)
      end if
      if (junt .ge. 0) then
        jqunit(isorc) = IVSTO(jqunit(isorc),2,junt)
      end if
CC
CC      get fuel heat content.
      nsc = JWHER(nvl,itabsm,ispacm,2,nch)
      i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
  170 if (i .gt. 0) then
CC        error.
        iferrm(nvl,2) = 1
      else if (i .lt. 0) then
CC        default heat content.
        if (hetcnt(isorc) .le. 0.0) then
          hetcnt(isorc) = 0.0
        end if
        ifwrt = 1
        i = IWTBF(mscrnm(nsc,nln),nch,hetcnt(isorc),1,iflrjs)
      else
        if (xdumy .lt. 0.0) then
          i = 1
          go to 170
        end if
CC        set source mass or emission rate.
        hetcnt(isorc) = xdumy
      end if
CC
CC
CC      get units of fuel burn rate.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .eq. 0) then
CC        default.
        iunt = 0
        junt = 0
        call UNITS(mscrnm(nsc,nln),nch,iunt,junt,0,1,j3)
        ifwrt = 1
      else
        call UNITS(mscrnm(nsc,nln),nch,i,j,0,0,j3)
        if (i .lt. 0.or.j .lt. 0) then
CC          error.
          iferrm(nvl,1) = 1
          iunt = 0
          junt = 0
        else
CC          set units index.
          iunt = i
          junt = j
        end if
      end if
      if (iunt .ge. 0) then
        kqunit(isorc) = IVSTO(kqunit(isorc),1,iunt)
      end if
      if (junt .ge. 0) then
        kqunit(isorc) = IVSTO(kqunit(isorc),2,junt)
      end if
      iunt2 = iunt
      junt2 = junt
CC
CC      get fuel burn rate.
      nsc = JWHER(nvl,itabsm,ispacm,2,nch)
      i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
      if (IFEQU(xdumy,0.0) .eq. 0.and.i .eq. 0.and.(nn .eq. 5.or.
     *    nn .eq. 6)) then
        i = -3
      end if
  180 if (i .gt. 0) then
CC        error.
        iferrm(nvl,2) = 1
      else if (i .lt. 0.or.itype .eq. 1) then
CC        default burn rate.
        if (itype .eq. 1) then
          xdumy = 2.5
        else
          xdumy = 300.0
        end if
        brnrat(isorc) = qemiss(isorc) * constm(iunt1+1) *
     *                  constt(junt+1) / (xdumy * constm(iunt+1))
        taucld(isorc) = xdumy
        if (itype .ne. 1) then
          i = IWTBF(mscrnm(nsc,nln),nch,brnrat(isorc),1,iflrjs)
        else
          call MBLK1(mscrnm(nsc,nln),nch)
          if (iflrjs .eq. 0) then
            mscrnm(nsc+nch-1,nln) = malpha(3)
          else
            mscrnm(nsc,nln) = malpha(3)
          end if
        end if
        ifwrt = 1
      else
        if (IFEQU(xdumy,0.0) .eq. 0) xdumy = -1.0
        if (xdumy .lt. 0.0) then
          i = 1
          go to 180
        end if
CC        set source burn rate.
        brnrat(isorc) = xdumy
CC        calculate burn time for instantaneous and quasi-continuous
CC        sources.
        if (brnrat(isorc) .gt. 0.0.and.itype .ne. 1) then
          taucld(isorc) = AMAX1(qemiss(isorc) * constm(iunt1+1) /
     *                    (brnrat(isorc) * constm(iunt2+1) /
     *                    constt(junt2+1)),2.5)
          xdumy = taucld(isorc)
          if (xdumy .le. 2.5.or.xdumy .gt. 3600.0) then
            iferrm(nvl,2) = 2
          end if
        else if (itype .eq. 1) then
          taucld(isorc) = 2.5
        else
          n = IWHER(3,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
          iferrm(n,2) = 1
        end if
CC        set source emission time units to seconds.
        iqunit(isorc) = IVSTO(iqunit(isorc),3,0)
      end if
CC
CC      get hours during which source is burned or detonated.
      do 190 j=1,24
        jj = MOD(j,10)
        if (jj .eq. 0) jj = 10
        if (jj .eq. 1) then
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        end if
        nsc = JWHER(nvl,itabsm,ispacm,jj,nch)
        if (IFCHR(mscrnm(nsc,nln),25) .ne. 0.or.mscrnm(nsc,nln) .eq.
     *      mblnk1) then
          isofrq(j,isorc) = 1
        else if (IFCHR(mscrnm(nsc,nln),14) .ne. 0) then
          isofrq(j,isorc) = 0
        else
          iferrm(nvl,jj) = 1
        end if
  190 continue
      ner = IFWER(iferrm,nmxlin*nmxfld)
      if (ifwrt .gt. 0.or.ner .gt. 0.or.(ner .eq. 0.and.ker(5) .ne. 0))
     *    then
CC        rewrite screen data to work file.
        i = IWTSC(numsc+nn,isorc,mscrnm,ilinem,iferrm)
      end if
      if (ner .gt. 0) then
        if (KERRP(22,0) .ge. 1) go to 240
      end if
      ker(5) = ner
      if (jfers .eq. 0.and.ker(5) .eq. 1) go to 150
      jfnxt = 6
      go to 250
  200 jfnxt = 1
      go to 250
  210 jfnxt = 2
      go to 250
  220 jfnxt = 8
      go to 250
  230 jfnxt = 11
      go to 250
  240 jfnxt = 12
  250 return
      end
