      Subroutine RINPR
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - RINPR Version 01.2                          ---
CC--- Subroutine description - get graphics output device and       ---
CC---                          initialize graphics parameters.      ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      character*1 mdumy
      character*2 munxy,muntz
      character*6 mcolr
      character*12 msch3
      character*12 mpldf
      character*22 msnam
      character*58 mfnam
CC
      dimension ker(2),mcolr(6),ztran(10000)
CC
      include 'OCNTRL.INC'
      include 'ONAMES.INC'
      include 'OCALCS.INC'
      include 'OWNTRL.INC'
      include 'OCDSPM.INC'
CC
      equivalence (mfnam,mscrch(1)),(zcoord(1),ztran(1))
CC
      data mpldf/'OBODPLOT.DAT'/,
     *     mcolr/'BLUE  ',
     *           'GREEN ',
     *           'CYAN  ',
     *           'LTCYAN',
     *           'LTMAGE',
     *           'WHITE '/,
     *     msnam/'SCRATCH plot file'/,msch3/'SCRATCH3.OBD'/
CC
CC********************************************************************
CC
      i = JINIT(ker,2,0,0)
CC      start screen number 57.
   10 numsc = numscs(8)
      iswplt(26) = 0
CC
CC      display and read data for screen numsc.
CC
CC      screen numsc, start of graphics options.
      numf = IRDSC(numsc,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
CC
CC      show if CGA, EGA or VGA.
      nvl = IWHER(5,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
      if (mscrnm(nsc,nln) .eq. mblnk1) then
        mdumy = mblnk1
        if (ipmode .eq. 4) then
          mdumy = malpha(3)
        else if (ipmode .eq. 16) then
          mdumy = malpha(5)
        else if (ipmode .eq. 18) then
          mdumy = malpha(22)
        else if (ipmode .lt. 10) then
          xdumy = ipmode
          i = IWTBF(mdumy,1,xdumy,0,iflrjs)
        end if
        if (mdumy .ne. mblnk1) mscrnm(nsc,nln) = mdumy
      end if
CC      show pixels in x and y screen axes.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
        i = IWTBF(mscrnm(nsc,nln),nch,pixelx,0,iflrjs)
      end if
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
        i = IWTBF(mscrnm(nsc,nln),nch,pixely,0,iflrjs)
      end if
CC      display screen menu.
      iferrs = IOSCN(numsc,mscrnm,ilinem,itabsm,ispacm,iferrm,lstrw,0)
CC      save screen menu.
      if (iferrs .ne. 10) i = IWTSC(numsc,1,mscrnm,ilinem,iferrm)
CC      save iferrs.
      jfers = iferrs
      ifwrt = 0
      ker(1) = IFWER(iferrm,nmxlin*nmxfld)
CC
CC      check user response to menu.
      if (jfers .eq. 1) then
CC        terminate.
        go to 290
      else if (jfers .eq. 2.or.jfers .eq. 11) then
CC        main menu.
        ifnext = 3
        go to 290
      else if (jfers .ge. 3.and.jfers .le. 7) then
CC        display aux. menus.
        iferrs = IFRSP(jfers,mscrnm,ilinem,itabsm,ispacm,iferrm,0)
        if (iferrs .eq. 1.or.iferrs .eq. 2) then
CC          terminate or main menu.
          ifnext = 3
          jfers = iferrs
          go to 290
        else
CC          redisplay menu.
          go to 10
        end if
      else if (jfers .eq. 8) then
        iswplt(26) = 1
      else if (jfers .ge. 9.and.jfers .le. 10) then
        go to 10
      end if
CC      turn off error indicators for this menu.
      i = JINIT(iferrm,nmxlin*nmxfld,0,0)
CC
CC      check data.
CC
CC      determine if hard copy desired.
      nvl = IWHER(0,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      ifhcpy = 0
      if (mscrnm(nsc,nln) .ne. mblnk1) then
        do 20 i=1,12
          if (IFCHR(mscrnm(nsc,nln),i) .ne. 0) then
            ifhcpy = i
          end if
   20   continue
CC        ifhcpy - 1 - HP LaserJet xx.
CC                 2 - HP RuggedWriter 480.
CC                 3 - IBM Proprinter.
CC                 4 - Epson FX.
CC                 5 - Epson LQ.
CC                 6 - Alps P2400C
CC                 7 - IBM XL24 AGM.
CC                 8 - Okidata.
CC                 9 - Canon BJ200e.
CC                10 - HP7475A plotter.
CC                11 - DEC Inkjet.
CC                12 - plot file (logical units 35).
        if (ifhcpy .eq. 0) then
          iferrm(nvl,1) = 1
        end if
        if (ifhcpy .eq. 12) then
CC          output plot data to data file.
          iplfil = 29
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          call MBLK1(mplnam,22)
          if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
            call MVCH1(mscrnm(nsc,nln),mplnam,MIN0(nch,22))
          else
            mplnam = mpldf
            call MVCH1(mplnam,mscrnm(nsc,nln),MIN0(nch,22))
            ifwrt = 1
          end if
          i = LRJST(mplnam,22,1)
          call FNAME(mfnam,58,mdirec,mplnam)
          OPEN (unit=iplfil,file=mfnam,iostat=ier,err=30,
     *          status='unknown')
          go to 40
   30     if (KERRS(3,mplnam,ier,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *        go to 320
          iferrm(nvl,1) = 1
          iplfil = 0
          ifhcpy = 0
        else
          nvl = nvl + 1
        end if
   40   if (ifhcpy .gt. 0.and.ifhcpy .lt. 12) then
          iscfil = 30
          OPEN (unit=iscfil,file=msch3,iostat=ier,err=50,
     *          status='unknown',form='formatted')
          ifstcp = 1
          go to 70
   50     if (KERRS(3,msnam,ier,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *        go to 320
          iferrm(nvl,1) = 1
          iscfil = 0
          ifhcpy = 0
          ifstcp = 0
        end if
      else
        if (ifstcp .ne. 0) then
          CLOSE (unit=iscfil,status='delete',err=60)
        end if
   60   ifstcp = 0
        nvl = nvl + 1
        ifhcpy = 0
      end if
   70 ifgrph = 1
      call GETID(ier)
      if (ier .gt. 0) then
        ifgrph = 0
        ifnext = 3
        go to 320
      end if
CC
CC      see if terrain data file is present.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        iftran = 1
        itrfil = 31
        call MBLK1(mtrnam,22)
        call MVCH1(mscrnm(nsc,nln),mtrnam,MIN0(nch,22))
        i = LRJST(mtrnam,22,1)
        call FNAME(mfnam,58,mdirec,mtrnam)
        OPEN (unit=itrfil,file=mfnam,iostat=ier,err=140,
     *        status='old')
        i = KERRS(34,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0)
        irec = 0
        REWIND (unit=itrfil,iostat=ier,err=120)
        irec = irec + 1
        READ (itrfil,9000,iostat=ier,err=120,end=130) nxtran,nytran,
     *                                                munxy,muntz
        irec = irec + 1
        READ (itrfil,9001,iostat=ier,err=120,end=130) (xteran(i),
     *                                                 i=1,nxtran)
        irec = irec + (nxtran + 9) / 10
        nx = nxtran
        nxtran = MIN0(nxtran,nmxxyz)
        READ (itrfil,9001,iostat=ier,err=120,end=130) (yteran(i),
     *                                                 i=1,nytran)
        ny = nytran
        nytran = MIN0(nytran,nmxxyz)
        irec = irec + (nytran + 9) / 10
        do 80 j=1,nytran
          k = (j - 1) * nxtran
          if (j .lt. nytran) then
            n = nx
          else
            n = nxtran
          end if
          if (k+n .gt. nmxxyz*nmxxyz) n = nmxxyz * nmxxyz - k
          READ (itrfil,9001,iostat=ier,err=120,end=130) (ztran(k+i),
     *                                                   i=1,n)
          irec = irec + (nxtran + 9) / 10
   80   continue
        CLOSE (unit=itrfil,status='keep')
        iftran = 1
CC        convert xteran,yteran coordinate units to output units.
        iftpol = 0
        call UNITS(munxy,2,i,jdumy,0,0,0)
        xdumy = constl(i+1) / constl(iswopt(15)+1)
        do 90 i=1,nxtran
          xteran(i) = xteran(i) * xdumy
   90   continue
        do 100 i=1,nytran
          yteran(i) = yteran(i) * xdumy
  100   continue
CC        convert ztran units to output units.
        call UNITS(muntz,2,i,jdumy,0,0,0)
        xdumy = constl(i+1) / constl(iswopt(15)+1)
        do 110 i=1,nxtran*nytran
          ztran(i) = ztran(i) * xdumy
  110   continue
        go to 160
  120   if (KERRS(1,mtrnam,ier,irec,xdum,xdum,xdum,xdum,0) .ge. 1)
     *      go to 320
        go to 150
  130   if (KERRS(4,mtrnam,irec,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *      go to 320
        go to 150
  140   if (KERRS(3,mtrnam,ier,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *      go to 320
  150   iferrm(nvl,1) = 1
        itrfil = 0
        iftran = 0
      else
        if (iswopt(6) .eq. 2) then
          iftran = 2
          nxtran = nxpnts
          nytran = nypnts
        else
          iftran = 0
        end if
      end if
CC
CC        see if digitized map data file is present.
  160 nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        impfil = 32
        call MBLK1(mapnam,22)
        call MVCH1(mscrnm(nsc,nln),mapnam,MIN0(nch,22))
        i = LRJST(mapnam,22,1)
        call FNAME(mfnam,58,mdirec,mapnam)
        OPEN (unit=impfil,file=mfnam,iostat=ier,err=170,status='old')
        go to 180
  170   if (KERRS(3,mapnam,ier,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *      go to 320
        iferrm(nvl,1) = 1
        impfil = 0
  180   continue
      end if
CC      see if change graphics board type.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        if (IFCHR(mscrnm(nsc,nln),3) .ne. 0) then
          ipmode = 4
        else if (IFCHR(mscrnm(nsc,nln),5) .ne. 0) then
          ipmode = 16
        else if (IFCHR(mscrnm(nsc,nln),22) .ne. 0) then
          ipmode = 18
        else
          i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
          if (i .eq. 0) then
            ipmode = xdumy
          else
            iferrm(nvl,1) = 1
          end if
        end if
      end if
CC      see if pixelx entered.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
        if (i .eq. 0) then
          pixelx = xdumy
        else
          iferrm(nvl,1) = 1
        end if
      end if
CC      see if pixely entered.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
        if (i .eq. 0) then
          pixely = xdumy
        else
          iferrm(nvl,1) = 1
        end if
      end if
CC
      ner = IFWER(iferrm,nmxlin*nmxfld)
      if (ifwrt .gt. 0.or.ner .gt. 0.or.(ner .eq. 0.and.ker(1) .ne. 0))
     *    then
CC        rewrite screen data.
        i = IWTSC(numsc,1,mscrnm,ilinem,iferrm)
      end if
      if (ner .gt. 0) then
        if (KERRP(22,0) .ge. 1) go to 320
      end if
      ker(1) = ner
      if (jfers .eq. 0.and.ker(1) .eq. 1) go to 10
CC
CC
  190 if (ifhcpy .gt. 0.and.ifhcpy .lt. 12) then
CC        screen number 58.
        numsc = numscs(8) + 1
CC
CC        screen numsc, graphics hard copy options.
        numf = IRDSC(numsc,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
        if (iswplt(26) .eq. 0) then
          nvl = IWHER(3,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
          if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
            if (paperw .le. 0.0) paperw = 8.5
            i = iwtbf(mscrnm(nsc,nln),nch,paperw,1,iflrjs)
          end if
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
            if (paperl .le. 0.0) paperl = 11.0
            i = iwtbf(mscrnm(nsc,nln),nch,paperl,1,iflrjs)
          end if
CC
CC        display screen menu.
          iferrs = IOSCN(numsc,mscrnm,ilinem,itabsm,ispacm,iferrm,
     *                   lstrw,0)
        else
         iferrs = 0
        end if
CC        save screen menu.
        if (iferrs .ne. 10) i = IWTSC(numsc,1,mscrnm,ilinem,iferrm)
CC        save iferrs.
        jfers = iferrs
        ifwrt = 0
        ker(2) = IFWER(iferrm,nmxlin*nmxfld)
CC
CC        check user response to menu.
        if (jfers .eq. 1) then
CC          terminate.
          go to 290
        else if (jfers .eq. 2) then
CC          main menu.
          ifnext = 3
          go to 290
        else if (jfers .ge. 3.and.jfers .le. 7) then
CC          display aux. menus.
          iferrs = IFRSP(jfers,mscrnm,ilinem,itabsm,ispacm,iferrm,0)
          if (iferrs .eq. 1.or.iferrs .eq. 2) then
CC            terminate or main menu.
            ifnext = 3
            jfers = iferrs
            go to 290
          else
CC            redisplay menu.
            go to 190
          end if
        else if (jfers .eq. 8) then
          iswplt(26) = 1
        else if (jfers .ge. 9.and.jfers .le. 10) then
          go to 190
        else if (jfers .eq. 11) then
          go to 10
        end if
CC        turn off error indicators for this menu.
        i = JINIT(iferrm,nmxlin*nmxfld,0,0)
CC
CC        check data.
CC
CC        determine printer/COM port number.
        nvl = IWHER(0,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        if (mscrnm(nsc,nln) .ne. mblnk1) then
          ifprno = 0
          do 200 i=1,6
            if (IFCHR(mscrnm(nsc,nln),i) .ne. 0) then
              ifprno = i
              go to 210
            end if
  200     continue
          iferrm(nvl,1) = 1
        else
          ifprno = 1
          mscrnm(nsc,nln) = malpha(1)
          ifwrt = 1
        end if
CC        get alternate printer/COM port number.
  210   nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        if (mscrnm(nsc,nln) .ne. mblnk1) then
          i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
          if (i .eq. 0) then
            jdumy = ifprno
            ifprno = xdumy
            if (ifprno .gt. 0.and.ifprno .le. 10) then
              if (jdumy .eq. 1) then
                ifprno = -ifprno
              else if (jdumy .eq. 5) then
                ifprno = -(ifprno + 10)
              else
                ifprno = jdumy
                iferrm(nvl,1) = 1
              end if
            else
              ifprno = jdumy
              iferrm(nvl,1) = 1
            end if
          else
            iferrm(nvl,1) = 1
          end if
        end if
CC        determine paper width.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        i = -1
        if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
          i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
          if (i .eq. 0) then
            paperw = xdumy
          else if (i .lt. 0) then
            paperw = 8.5
          else
            iferrm(nvl,1) = 1
          end if
        else
          paperw = 8.5
        end if
        if (i .lt. 0) then
          i = IWTBF(mscrnm(nsc,nln),nch,paperw,2,iflrjs)
          ifwrt = 1
        end if
CC        determine paper length.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        i = -1
        if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
          i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
          if (i .eq. 0) then
            paperl = xdumy
          else if (i .lt. 0) then
            paperl = 11.0
          else
            iferrm(nvl,1) = 1
          end if
        else
          paperl = 11.0
        end if
        if (i .lt. 0) then
          i = IWTBF(mscrnm(nsc,nln),nch,paperl,2,iflrjs)
          ifwrt = 1
        end if
CC        determine paper left margin.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        i = -1
        if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
          i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
          if (i .eq. 0) then
            xftmrg = xdumy
          else if (i .lt. 0) then
            xftmrg = 0.0
          else
            iferrm(nvl,1) = 1
          end if
        else
          xftmrg = 0.0
        end if
        if (i .lt. 0) then
          i = IWTBF(mscrnm(nsc,nln),nch,xftmrg,2,iflrjs)
          ifwrt = 1
        end if
CC        determine paper right margin.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        i = -1
        if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
          i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
          if (i .eq. 0) then
            rgtmrg = xdumy
          else if (i .lt. 0) then
            rgtmrg = 0.0
          else
            iferrm(nvl,1) = 1
          end if
        else
          rgtmrg = 0.0
        end if
        if (i .lt. 0) then
          i = IWTBF(mscrnm(nsc,nln),nch,rgtmrg,2,iflrjs)
          ifwrt = 1
        end if
CC        determine paper bottom margin.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        i = -1
        if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
          i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
          if (i .eq. 0) then
            botmrg = xdumy
          else if (i .lt. 0) then
            botmrg = 0.0
          else
            iferrm(nvl,1) = 1
          end if
        else
          botmrg = 0.0
        end if
        if (i .lt. 0) then
          i = IWTBF(mscrnm(nsc,nln),nch,botmrg,2,iflrjs)
          ifwrt = 1
        end if
CC        determine paper top margin.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        i = -1
        if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
          i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
          if (i .eq. 0) then
            topmrg = xdumy
          else if (i .lt. 0) then
            topmrg = 0.0
          else
            iferrm(nvl,1) = 1
          end if
        else
          topmrg = 0.0
        end if
        if (i .lt. 0) then
          i = IWTBF(mscrnm(nsc,nln),nch,topmrg,2,iflrjs)
          ifwrt = 1
        end if
CC        determine if color.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        if (mscrnm(nsc,nln) .ne. mblnk1) then
          if (IFCHR(mscrnm(nsc,nln),14) .ne. 0) then
            ifcolr = 0
          else if (IFCHR(mscrnm(nsc,nln),25) .ne. 0) then
            ifcolr = 1
          else
            iferrm(nvl,1) = 1
          end if
        else
          ifcolr = 0
          mscrnm(nsc,nln) = malpha(1)
          ifwrt = 1
        end if
CC        determine plot orientation.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        if (mscrnm(nsc,nln) .ne. mblnk1) then
          if (IFCHR(mscrnm(nsc,nln),1) .ne. 0) then
CC            x horizontal, y vertical.
            ifswxy = 0
          else if (IFCHR(mscrnm(nsc,nln),2) .ne. 0) then
CC            x vertical, y horizontal.
            ifswxy = 1
          else
            iferrm(nvl,1) = 1
          end if
        else
          ifswxy = 0
          mscrnm(nsc,nln) = malpha(1)
          ifwrt = 1
        end if
CC
        ner = IFWER(iferrm,nmxlin*nmxfld)
        if (ifwrt .gt. 0.or.ner .gt. 0.or.(ner .eq. 0.and.
     *      ker(2) .ne. 0)) then
CC          rewrite screen data.
          i = IWTSC(numsc,1,mscrnm,ilinem,iferrm)
        end if
        if (ner .gt. 0) then
          if (KERRP(22,0) .ge. 1) go to 320
        end if
        ker(2) = ner
        if (jfers .eq. 0.and.ker(2) .eq. 1) then
          iswplt(26) = 0
          go to 190
        end if
      else
        ker(2) = 0
      end if
      if (IFWER(ker,2) .eq. 1) ifgrph = 0
      if (ifgrph .gt. 0) then
        if (ifcnvt .eq. 0) then
CC          convert grid system to output units.
          call CHXYZ(xdumx)
        end if
        if (ABS(xorgin)+ABS(yorgin) .gt. 0.0) then
          if (ABS(xorgin)+ABS(yorgin) .gt. 1.0e5) then
            jfutm = 2
          else
            jfutm = 1
          end if
        else
          jfutm = 0
        end if
        ifutm = 0
        if (jfutm .gt. 0) then
          if (nxpnts .gt. 0.and.nypnts .gt. 0) then
            if (ABS(xcoord(1))+ABS(ycoord(1)) .gt. 1.0e5) then
                ifutm = 1
            end if
          end if
        end if
        if (igrect .eq. 0) then
CC          rectangular coordinate system
          if (jfutm .gt. 0) then
            if ((jfutm .ne. 2.or.ifutm .ne. 1).and.iswplt(1) .eq. 0)
     *          then
              do 220 i=1,nxpnts
                xcoord(i) = xcoord(i) + xorgin
  220         continue
              do 230 i=1,nypnts
                ycoord(i) = ycoord(i) + yorgin
  230         continue
              iswplt(1) = 1
            end if
          end if
        end if
        if (iftran .eq. 2) then
          i = JRNIT(xteran,nxtran,xcoord,1)
          i = JRNIT(yteran,nytran,ycoord,1)        
          iftpol = igrect
        end if
        if (nxpnts .gt. 1.and.nypnts .gt. 1) then
CC          grid system calculations.
          nxyfnc = 0
        else
CC          single axis calculations.
          if (nxpnts .le. 1) then
CC            y axis.
            i = JRNIT(functx,nypnts,ycoord,1)
            nxyfnc = nypnts
          else
CC            x axis.
            i = JRNIT(functx,nxpnts,xcoord,1)
            nxyfnc = nxpnts
          end if
        end if
        xymins = -rmissd
        if (nxpnts .gt. 1) then
          do 240 i=2,nxpnts
            xymins = AMIN1(xymins,ABS(xcoord(i)-xcoord(i-1)))
  240     continue
        end if
        if (nypnts .gt. 1) then
          do 250 i=2,nypnts
            xymins = AMIN1(xymins,ABS(ycoord(i)-ycoord(i-1)))
  250     continue
        end if
        if (xymins .le. 0.0) xymins = 100.0
        nplots = 0
        llines = linetl
        i = JINIT(ioffst,2,-1,0)
        npoptn = 0
        i = JINIT(ipoptn,10,0,0)
CC        see if have terrain data.
        if (iftran .ne. 0) ioffst(1) = 0
        kssndx = 0
        ngrpct = 1
        ifgpol = igrect
        ibaudr = 1200
        j = iswplt(26)
        i = JINIT(iswplt,30,0,0)
        iswplt(26) = j
        if (nxyfnc .eq. 0) then
          iswplt(2) = 0
        else
          iswplt(2) = 3
        end if
        if (ipmode .lt. 16) then
          k = 3
          mcolor(2) = mcolr(4)
          mcolor(3) = mcolr(5)
          mcolor(4) = mcolr(6)
        else
          k = 15
          mcolor(2) = mcolr(1)
          mcolor(3) = mcolr(2)
          mcolor(4) = mcolr(3)
        end if
        iswplt(9) = 0
        iswplt(11) = -2
        iswplt(12) = k
        iswplt(13) = k
        iswplt(14) = k
        iswplt(15) = k
        iswplt(16) = k
        iswplt(20) = 1
        iswplt(23) = k
        iswplt(24) = -1
        iswplt(25) = 0
        if (impfil .eq. 0) then
          iswplt(28) = 0
        else
          iswplt(28) = 1
        end if
        i = JINIT(ispclr,nmxspl,k,0)
        i = JINIT(icnclr,2*nmxctr,k,0)
        i = JINIT(icndsh,2*nmxctr,0,0)
        sclxes = 0.0
        sclyes = 0.0        
        xnchpl = 8.0
        ynchpl = 9.5
CC        pixels per inch
        ppin = AMAX1(pixelx / xnchpl , pixely / ynchpl, 60.0)
CC        21 pixels per character gives maximum resolution, but due
CC        to the size of characters on some terminals default 11 pixels
CC        per character 52% resolution.
CC        height/width for axis numbers, contour numbers, etc.
        htnchr = 11.0 / ppin
CC        minimum space between label lines, use 4 pixels
        spmins = 4.0 / ppin
CC        minimum character height, use 70%.
        hcminc = 0.7 * htnchr
CC        special symbol character height
        htpchr = htnchr
CC        axis labels and legend character height
        htlchr = htnchr + spmins
CC        calculate margins
        if (spfrct .le. 0.0) spfrct = 0.8
        xlmrgn = 4.0 * htlchr + htnchr * (2.0 + spfrct)
        ybmrgn = htlchr * (7.0 + spfrct) + htnchr * (2.0 + spfrct)
        if (nxpnts .gt. 1.and.nypnts .gt. 1) then
          i = 2
  260     if (xcoord(nxpnts) .ge. 10.0**i) then
            i = i + 1
            go to 260
          end if
          xrmrgn = i * htnchr / 2.0
          if (ifgpol .eq. 0) then
            i = 2
  270       if (ycoord(nypnts) .ge. 10.0**i) then
              i = i + 1
              go to 270
            end if
          else
            i = 4
          end if
          ytmrgn = i * htnchr / 2.0
        else
          xrmrgn = 2.0 * htnchr
          ytmrgn = 2.0 * htnchr
        end if
        ichars = 4
        lswxax = 0
        lswyax = 0
        xincax = 0.0
        yincax = 0.0
        nincxs = 10
        nincys = 10
        ndgxes = -1
        ndgyes = -1
        ifnewp = 0
        if (nxyfnc .gt. 0) then
          pxmins = functx(1)
          pxmaxs = functx(nxyfnc)
          pymins = rmissc
          pymaxs = rmissc
          iswplt(28) = 0
        else
          if (ifgpol .eq. 0) then
            pxmins = xcoord(1)
            pxmaxs = xcoord(nxpnts)
            if (nypnts .gt. 0) then
              pymins = ycoord(1)
              pymaxs = ycoord(nypnts)
            end if
          else
            pxmins = -xcoord(nxpnts) + xorgin
            pxmaxs = xcoord(nxpnts) + xorgin
            pymins = -xcoord(nxpnts) + yorgin
            pymaxs = xcoord(nxpnts) + yorgin
          end if
        end if
        ifspl1 = 0
        ifspl2 = 0
        nspcls = MAX0(MIN0(nmxspl,nsourc),1)
        do 280 i=1,nspcls
          if (IVGET(isrect(i),2) .le. 0) then
            ifspl1 = 1
            isppol(i) = 0
          else
            ifspl2 = 1
            isppol(i) = 1
          end if
          xdumy = constl(IVGET(isrect(i),1)+1) / constl(iswopt(15)+1)
          spclxs(i) = clddxs(i) * xdumy
          if (isppol(i) .eq. 0) spclys(i) = clddys(i) * xdumy
          if (jfutm .gt. 0.and.isppol(i) .eq. 0) then
            if (ABS(spclxs(i))+ABS(spclys(i)) .gt. 1.0e5) then
                lfutm = 1
            else
                lfutm = 0
            end if
            if (jfutm .ne. 2.or.lfutm .ne. 1) then
              spclxs(i) = spclxs(i) + xorgin
              spclys(i) = spclys(i) + yorgin
            end if
          end if
          spclht(i) = AMAX1(htnchr,spmins)
          spclan(i) = 0.0
          ispclc(i) = 1
  280   continue
        if (ifspl1 .ne. 0) then
          ifspol = 0
          if (ifspl2 .ne. 0) then
            ifspol = 2
          end if
        else
          ifspol = 1
        end if
      end if
CC
CC      check if data complete.
      if (IFWER(ker,2) .eq. 1) then
        ifset = -1
        iswplt(26) = 0
      else
        if (ifgrph .le. 0) then
          ifset = 0
          iswplt(26) = 0
        else
          ifset = 1
          if (ifhrly .gt. 0) then
            norec = istrec + 1
            ier = INRAN(norec,iptbuf,iogfil)
            if (ier .ge. 0) then
              ntothr = iptbuf(1)
              i = jinit(nseasn,4,iptbuf(2),1)
              i = jinit(nquart,4,iptbuf(6),1)
            else
              if (KERRS(1,mognam,IABS(ier),norec,xdum,xdum,xdum,xdum,
     *                  0) .eq. 1) go to 320
              ifset = -1
              ifgrph = 0
              ifnext = 3
            end if
          end if
        end if
      end if
      ifrdwt(6) = ifset
      if (ifset .le. 0) then
        ifnext = 3
      end if
  290 if ((iferrs .eq. 11.or.ifnext .eq. 3).and.iswplt(1) .ne. 0)
     *    then
        do 300 i=1,nxpnts
          xcoord(i) = xcoord(i) - xorgin
  300   continue
        do 310 i=1,nypnts
          ycoord(i) = ycoord(i) - yorgin
  310   continue
      end if
  320 ifrddm = 0
      return
CC
 9000 format (2i4,2a2)
 9001 format (10f8.0)
      end
