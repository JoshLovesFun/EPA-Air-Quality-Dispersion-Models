      Subroutine RINPE
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - RINPE Version 01.2                          ---
CC--- Subroutine description - displays and reads receptor data.    ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      character*41 mesg
      character*17 mlbl                                                 120702
CC
CC
      dimension ker(6),mesg(2),kyfld(6),iesav(160)
CC
      include 'OCNTRL.INC'
      include 'ONAMES.INC'
      include 'OCDSPM.INC'
      include 'OCALCS.INC'
      include 'OWNTRL.INC'
CC
      equivalence (jfnxt,functx(1)),(ker(1),functx(2)),
     *            (iesav(1),functx(38))
CC
      data mesg/'Receptor heights are treated as flagpoles',
     *          ' Receptor heights are treated as terrain '/
      data mlbl/'Flag Pole Heights'/                                    120702
CC
CC********************************************************************
CC
CC        display and read data for screens numsc+4 to numsc+5.
      numsc = numscs(4)
      nsubx = nsubsc(numsc+2)
      nsuby = nsubsc(numsc+3)
      i = JINIT(kyfld,6,0,0)
      kyfld(1) = 1
      i = JINIT(iesav,160,0,0)
      ifnext = 3
      if (iswopt(6) .eq. 2.or.iswopt(6) .eq. 3) then                    120702
CC        save zcoords in calcsa and reorder zcoord array.
        i = JRNIT(calcsa,nmxxyz*nmxxyz,zcoord,1)
        i = JRNIT(zcoord,nmxxyz*nmxxyz,rmissd,0)
        do 20 j=1,nypnts
          l = (j - 1) * nxpnts
          k = (j - 1) * nmxxyz
          do 10 i=1,nxpnts
            zcoord(k+i) = calcsa(l+i)
   10     continue
   20   continue
      end if
      if (jfnxt .gt. 2) go to 180
CC
CC      screen numsc+2 (06), x axis.
   30 nsub = 1
      nn = 2
CC      read screen menu and screen data.
   40 numf = IRDSC(numsc+nn,nsub,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
CC      set receptor length units abbrev.
      nvl = 1
      nln = ilinem(nvl)
      call UNITS(mscrnm(9,nln),3,igunit,0,0,1,0)
CC      display screen menu.
      iferrs = IOSCN(numsc+nn,mscrnm,ilinem,itabsm,ispacm,iferrm,lstrw,
     *               0)
CC      save screen menu data in work file.
      if (iferrs .ne. 10) i = IWTSC(numsc+nn,nsub,mscrnm,ilinem,iferrm)
CC        if terminate.
      if (iferrs .eq. 1) go to 230
      jfers = iferrs
      ker(3) = IFWER(iferrm,nmxlin*nmxfld)
CC
CC      see what user wants to do next.
      if (jfers .eq. 2) then
        ner = IPACK(nsubx,xcoord,xdumy,xdumy,xdumy,xdumy,xdumy,
     *              nmxxyz,nxpnts,numsc+nn,mscrnm,ilinem,itabsm,ispacm,
     *              iferrm,1,0)
        if (ner .eq. 3) then
          if (KERRS(61,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *        go to 230
        else if (ner .gt. 0) then
          if (KERRS(29,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *        go to 230
          if (ner .eq. 1) ifrdwt(2) = -1
        end if
CC        main menu.
        go to 220
      else if (jfers .ge. 3.and.jfers .le. 7) then
CC        aux. menus.
        iferrs = IFRSP(jfers,mscrnm,ilinem,itabsm,ispacm,iferrm,0)
        if (iferrs .eq. 1.or.iferrs .eq. 2) then
CC          terminate or main menu.
          if (iferrs .eq. 2) go to 220
          go to 230
        else
CC          redisplay screen numsc+2 menu.
          go to 40
        end if
      else if (jfers .ge. 8.and.jfers .le. 10) then
        go to 40
      else if (jfers .eq. 11) then
        nsub = nsub - 1
CC        page up, pack x axis data.
        ner = IPACK(nsubx,xcoord,xdumy,xdumy,xdumy,xdumy,xdumy,
     *              nmxxyz,nxpnts,numsc+nn,mscrnm,ilinem,itabsm,ispacm,
     *              iferrm,1,0)
        if (ner .eq. 3) then
          if (KERRS(61,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *        go to 230
        else if (ner .gt. 0) then
          if (KERRS(29,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *        go to 230
        end if
        if (nsub .ge. 1) go to 40
        ker(3) = ner
        if (ifgeng .gt. 0) then
          jfnxt = 1
        else
          jfnxt = 0
        end if
        go to 230
      end if
CC      if first x axis screen menu.
      if (nsub .eq. 1) then
CC        see if data in second x axis screen menu or if second
CC        needed.
        ifdat = 0
CC        check last point on screen.
        nvl = IWHER(14,ilinem,itabsm,ispacm,0,5,nln,nsc,nch)
CC        if no data.
        if (NUMC1(mscrnm(nsc,nln),nch) .eq. 0) then
CC          if more than 70 points or second screen contains data.
          if (nxpnts .gt. 70.or.IRDSC(numsc+2,2,mscrnm,ilinem,itabsm,
     *        ispacm,iferrm,1) .gt. 0) then
CC            need next screen.
            ifdat = 1
          end if
        else        
          ifdat = 1
        end if
        if (ifdat .ne. 0) then
CC          go get second screen of x axis data.
          nsub = 2
          go to 40
        end if
      end if
CC        pack x axis data and go to yaxis data screen numsc+3.
      ner = IPACK(nsubx,xcoord,xdumy,xdumy,xdumy,xdumy,xdumy,
     *            nmxxyz,nxpnts,numsc+nn,mscrnm,ilinem,itabsm,ispacm,
     *            iferrm,1,0)
      if (ner .eq. 3) then
        if (KERRS(61,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *      go to 230
      else if (ner .gt. 0) then
        if (KERRS(29,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *      go to 230
      end if
      if (jfers .eq. 0.and.ner .eq. 1) then
        nsub = 1
        go to 40
      end if
      ker(3) = ner
CC
CC      screen numsc+3 (07), y axis.
   50 jfnxt = 1
      nsub = 1
      nn = 3
CC      read screen menu and screen data.
   60 numf = IRDSC(numsc+nn,nsub,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
CC      set receptor length units abbrev.
      nvl = 1
      nln = ilinem(nvl)
      call UNITS(mscrnm(9,nln),3,igunit,0,igrect,1,0)
CC      display screen menu.
      iferrs = IOSCN(numsc+nn,mscrnm,ilinem,itabsm,ispacm,iferrm,lstrw,
     *               0)
CC      save screen menu data.
      if (iferrs .ne. 10) i = IWTSC(numsc+nn,nsub,mscrnm,ilinem,iferrm)
      if (iferrs .eq. 1) go to 230
      jfers = iferrs
      ker(4) = IFWER(iferrm,nmxlin*nmxfld)
CC
CC      see what user wants to do next.
      if (jfers .eq. 2) then
        ner = IPACK(nsuby,ycoord,xdumy,xdumy,xdumy,xdumy,xdumy,
     *              nmxxyz,nypnts,numsc+nn,mscrnm,ilinem,itabsm,ispacm,
     *              iferrm,2,0)
        if (ner .eq. 3) then
          if (KERRS(61,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *        go to 230
        else if (ner .gt. 0) then
          if (KERRS(29,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *        go to 230
          if (ner .eq. 1) ifrdwt(2) = -1
        end if
CC        main menu.
        go to 220
      else if (jfers .ge. 3.and.jfers .le. 7) then
CC        aux. menus.
        iferrs = IFRSP(jfers,mscrnm,ilinem,itabsm,ispacm,iferrm,0)
        if (iferrs .eq. 1.or.iferrs .eq. 2) then
CC          terminate or main menu.
          if (iferrs .eq. 2) go to 220
          go to 230
        else
CC          redisplay screen numsc+3 menu.
          go to 60
        end if
      else if (jfers .ge. 8.and.jfers .le. 10) then
        go to 60
      else if (jfers .eq. 11) then
        nsub = nsub - 1
CC        go back to previous menu.
        ner = IPACK(nsuby,ycoord,xdumy,xdumy,xdumy,xdumy,xdumy,
     *              nmxxyz,nypnts,numsc+nn,mscrnm,ilinem,itabsm,ispacm,
     *              iferrm,2,0)
        if (ner .eq. 3) then
          if (KERRS(61,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *        go to 230
        else if (ner .gt. 0) then
          if (KERRS(29,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *        go to 230
        end if
        if (nsub .ge. 1) go to 60
        ker(4) = ner
        go to 30
      end if
CC      if first data set see if need second or if data is in second.
      if (nsub .eq. 1) then
        ifdat = 0
CC        check last data point.
        nvl = IWHER(14,ilinem,itabsm,ispacm,0,5,nln,nsc,nch)
CC        if point not set.
        if (NUMC1(mscrnm(nsc,nln),nch) .eq. 0) then
          if (nypnts .gt. 70.or.IRDSC(numsc+nn,2,mscrnm,ilinem,itabsm,
     *        ispacm,iferrm,1) .gt. 0) then
            ifdat = 1
          end if
        else
          ifdat = 1
        end if
        if (ifdat .ne. 0) then
CC          go get second data set of screen numsc+3 menu.
          nsub = 2
          go to 60
        end if
      end if
CC        pack y axis data and go to screen numsc+4 menu discrete data.
      ner = IPACK(nsuby,ycoord,xdumy,xdumy,xdumy,xdumy,xdumy,
     *            nmxxyz,nypnts,numsc+nn,mscrnm,ilinem,itabsm,ispacm,
     *            iferrm,2,0)
      if (ner .eq. 3) then
        if (KERRS(61,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *      go to 230
      else if (ner .gt. 0) then
        if (KERRS(29,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *      go to 230
      end if
      if (jfers .eq. 0.and.ner .eq. 1) then
        nsub = 1
        go to 60
      end if
      ker (4) = ner
      if (nxpnts .lt. nmxxyz) then
        i = JRNIT(xcoord(nxpnts+1),nmxxyz-nxpnts,rmissd,0)
      end if
      if (nypnts .lt. nmxxyz) then
        i = JRNIT(ycoord(nypnts+1),nmxxyz-nypnts,rmissd,0)
      end if
   70 if (iswopt(6) .eq. 2.or.iswopt(6) .eq. 3) then                    120702
        lflg = 0
CC
CC        complex terrain being used or flag pole grid.                 120702
CC        start screen number.
        nn = 4
CC        screen numsc+4 (08), z elevation (height) for complex terrain.
        ny1 = 1
        nx1 = 1
   80   numf = IRDSC(numsc+nn,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
        i = JINIT(iferrm,nmxlin*nmxfld,0,0)
        nx2 = MIN0(nx1 + 4,nmxxyz)
        ny2 = MIN0(ny1 + 13,nmxxyz)
CC        set units
        nvl = IWHER(0,ilinem,itabsm,ispacm,1,3,nln,nsc,nch)
CC        x axis units.
        call UNITS(mscrnm(nsc+nch-3,nln-1),2,igunit,0,0,1,0)
CC        display x coordinates for this screen.
        do 90 i=nx1,nx2
          nsc = JWHER(nvl,itabsm,ispacm,i-nx1+1,nch)
          j = IWTBF(mscrnm(nsc,nln),nch,xcoord(i),1,iflrjs)
   90   continue
CC        y axis units
        nsc = JWHER(nvl,itabsm,ispacm,1,nch)
        call UNITS(mscrnm(nsc-6,nln+1),3,igunit,0,igrect,1,0)
CC        z elevation units        
        call UNITS(mscrnm(nsc+44,nln+2),2,izunit,0,0,1,0)
        do 100 j=ny1,ny2
          nvl = IWHER(j-ny1+2,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
          i = IWTBF(mscrnm(nsc,nln),nch,ycoord(j),1,iflrjs)
  100   continue
        nvl = 1
        do 120 j=ny1,ny2
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          do 110 i=nx1,nx2
            nsc = JWHER(nvl,itabsm,ispacm,i-nx1+2,nch)
            k = (j - 1) * nmxxyz + i
            l = IWTBF(mscrnm(nsc,nln),nch,zcoord(k),1,iflrjs)
            if (xcoord(i) .gt. rmissd.and.ycoord(j) .gt. rmissd) then
              if (zcoord(k) .le. rmissd) then
                iferrm(nvl,i-nx1+2) = 1
              end if
            end if
  110     continue
  120   continue
        if (iswopt(6) .eq. 3) call MVCH1(mlbl,mscrnm(33,9),17)          120702
CC        display screen menu.
        iferrs =IOSCN(numsc+nn,mscrnm,ilinem,itabsm,ispacm,iferrm,lstrw,
     *                0)
CC        save screen menu data in work file.
        if (iferrs .ne. 10) i = IWTSC(numsc+nn,1,mscrnm,ilinem,iferrm)
CC        if terminate.
        if (iferrs .eq. 1) go to 220
        jfers = iferrs
        ker(5) = IFWER(iferrm,nmxlin*nmxfld)
CC
CC        see what user wants to do next.
  130   if (jfers .eq. 2) then
          i = IPACK(nsubx,xcoord,xdumy,xdumy,xdumy,xdumy,xdumy,
     *              nmxxyz,nxpnts,numsc+2,mscrnm,ilinem,itabsm,
     *              ispacm,iferrm,1,1)
          if (nxpnts .lt. nmxxyz) then
            j = JRNIT(xcoord(nxpnts+1),nmxxyz-nxpnts,rmissd,0)
          end if
          if (i .eq. 3) then
            if (KERRS(61,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *          go to 220
          else if (i .gt. 0) then
            if (KERRS(29,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *          go to 220
          end if
          j = IPACK(nsuby,ycoord,xdumy,xdumy,xdumy,xdumy,xdumy,
     *              nmxxyz,nypnts,numsc+3,mscrnm,ilinem,itabsm,
     *              ispacm,iferrm,2,1)
          if (nypnts .lt. nmxxyz) then
            l = JRNIT(ycoord(nypnts+1),nmxxyz-nypnts,rmissd,0)
          end if
          if (j .eq. 3) then
            if (KERRS(61,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *          go to 220
          else if (j .gt. 0) then
            if (KERRS(29,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *          go to 220
            if (i .gt. 0) i = MIN0(i,j)
          end if
          if (i .eq. 1) ifrdwt(2) = -1
CC          main menu.
          go to 200
        else if (jfers .ge. 3.and.jfers .le. 7) then
CC          aux. menus.
          iferrs = IFRSP(jfers,mscrnm,ilinem,itabsm,ispacm,iferrm,0)
          if (iferrs .eq. 1.or.iferrs .eq. 2) then
CC            terminate or main menu.
            if (iferrs .eq. 2) then
              jfers = 2
              go to 130
            end if
            go to 200
          else
CC            redisplay screen numsc+nn menu.
            go to 80
          end if
        else if (jfers .ge. 8.and.jfers .le. 10) then
          go to 80
        else if (jfers .eq. 11) then
          i = IPACK(nsubx,xcoord,xdumy,xdumy,xdumy,xdumy,xdumy,
     *              nmxxyz,nxpnts,numsc+2,mscrnm,ilinem,itabsm,
     *              ispacm,iferrm,-1,0)
          if (i .eq. 3) then
            if (KERRS(61,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *          go to 220
          else if (i .gt. 0) then
            if (KERRS(29,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *          go to 220
          end if
          j = IPACK(nsuby,ycoord,xdumy,xdumy,xdumy,xdumy,xdumy,
     *              nmxxyz,nypnts,numsc+3,mscrnm,ilinem,itabsm,
     *              ispacm,iferrm,-2,0)
          if (j .eq. 3) then
            if (KERRS(61,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *          go to 220
          else if (j .gt. 0) then
            if (KERRS(29,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *          go to 220
            if (i .gt. 0) i = MIN0(i,j)
          end if
          nn = 4
          iesav(((ny1+13)/14-1)*20+(nx1+4)/5) = ker(5)
CC          page up.
          nx1 = nx1 - 5
          if (nx1 .gt. 0) go to 80
          ny1 = ny1 - 14
          if (ny1 .gt. 0) then
            nx1 = 1
            go to 80
          end if
          i = IPACK(nsubx,xcoord,xdumy,xdumy,xdumy,xdumy,xdumy,
     *              nmxxyz,nxpnts,numsc+2,mscrnm,ilinem,itabsm,
     *              ispacm,iferrm,1,1)
          if (i .eq. 3) then
            if (KERRS(61,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *          go to 220
          else if (i .gt. 0) then
            if (KERRS(29,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *          go to 220
          end if
          j = IPACK(nsuby,ycoord,xdumy,xdumy,xdumy,xdumy,xdumy,
     *              nmxxyz,nypnts,numsc+3,mscrnm,ilinem,itabsm,
     *              ispacm,iferrm,2,1)
          if (j .eq. 3) then
            if (KERRS(61,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *          go to 220
          else if (j .gt. 0) then
            if (KERRS(29,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *          go to 220
            if (i .gt. 0) i = MIN0(i,j)
          end if
          if (i .eq. 1) ifrdwt(2) = -1
          if (nxpnts .lt. nmxxyz) then
            j = JRNIT(xcoord(nxpnts+1),nmxxyz-nxpnts,rmissd,0)
          end if
          if (nypnts .lt. nmxxyz) then
            j = JRNIT(ycoord(nypnts+1),nmxxyz-nypnts,rmissd,0)
          end if
          nn = 4
          go to 50
        end if
CC
CC        turn off error array.
        i = JINIT(iferrm,nmxlin*nmxfld,0,0)
CC        check data.
        nvl = IWHER(0,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        iflg = 0
        do 140 i=nx1,nx2
          nsc = JWHER(nvl,itabsm,ispacm,i-nx1+1,nch)
          if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
            l = IRDBF(mscrnm(nsc,nln),nch,xdumy)
            if (l .gt. 0) then
              iferrm(nvl,i-nx1+1) = 1
            else if (l .eq. -1) then
              xcoord(i) = rmissd
            else
              if (l .lt. 0) xdumy = 0.0
              xcoord(i) = xdumy
            end if
            iflg = i
          end if
  140   continue
        nvl = 1
        jflg = 0
        do 150 j=ny1,ny2
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
            l = IRDBF(mscrnm(nsc,nln),nch,xdumy)
            if (l .gt. 0) then
              iferrm(nvl,1) = 1
            else if (l .eq. -1) then
              ycoord(j) = rmissd
            else
              if (l .lt. 0) xdumy = 0.0
              ycoord(j) = xdumy
              if (ycoord(j) .lt. ycoord(ny1)) lflg = 1
            end if
            jflg = j
          end if
  150   continue
        nvl = 1
        do 170 j=1,nmxxyz
          if (j .ge. ny1.and.j .le. ny2) then
            nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          end if
          do 160 i=1,nmxxyz
            if (i .ge. nx1.and.i .le. nx2) then
              nsc = JWHER(nvl,itabsm,ispacm,i-nx1+2,nch)
            end if
            n = (j - 1) * nmxxyz + i
            if (xcoord(i) .gt. rmissd.and.ycoord(j) .gt. rmissd) then
              if (i .ge. nx1.and.i .le. nx2.and.j .ge. ny1.and.
     *            j .le. ny2) then
                l = IRDBF(mscrnm(nsc,nln),nch,xdumy)
                if (l .gt. 0) then
                  iferrm(nvl,i-nx1+2) = 1
                else if (l .lt. 0) then
                  xdumy = rmissd
                  iferrm(nvl,i-nx1+2) = 1
                end if
                zcoord(n) = xdumy
                if (zcoord(n) .le. rmissd) iferrm(nvl,i-nx1+2) = 1
              end if
            else
              zcoord(n) = rmissd
            end if
  160     continue
  170   continue
        nxpnts = IZGET(xcoord,nmxxyz,j)
        nypnts = IZGET(ycoord,nmxxyz,j)
        i = IPACK(nsubx,xcoord,xdumy,xdumy,xdumy,xdumy,xdumy,
     *            nmxxyz,nxpnts,numsc+2,mscrnm,ilinem,itabsm,
     *            ispacm,iferrm,-1,0)
        j = IPACK(nsuby,ycoord,xdumy,xdumy,xdumy,xdumy,xdumy,
     *            nmxxyz,nypnts,numsc+3,mscrnm,ilinem,itabsm,
     *            ispacm,iferrm,-2,0)
        if (j .gt. 0) then
          if (i .gt. 0) i = MIN0(i,j)
        end if
        nn = 4
        nxpnts = IZGET(xcoord,nmxxyz,j)
        nypnts = IZGET(ycoord,nmxxyz,j)
        ker(5) = IFWER(iferrm,nmxlin*nmxfld)
        iesav(((ny1+13)/14-1)*20+(nx1+4)/5) = ker(5)
        if (ker(5) .gt. 0.or.i .gt. 0) then
          if (KERRS(29,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *        go to 220
        end if
        if (i .eq. 1.and.ker(5) .ne. 1) ker(5) = i
        if (jfers .eq. 0.and.ker(5) .eq. 1) go to 80
CC        see where to next.
        if (iflg .ge. nx2) then
          nx1 = nx2 + 1
          if (nx1 .le. nmxxyz) then
            go to 80
          end if
        end if
        if (jflg .ge. ny2) then
          ny1 = ny2 + 1
          if (ny1 .le. nmxxyz) then
            nx1 = 1
            go to 80
          end if
        end if
CC        rewrite x axis screen menus.
        nn = 2
        ner = IPACK(nsubx,xcoord,xdumy,xdumy,xdumy,xdumy,xdumy,
     *              nmxxyz,nxpnts,numsc+2,mscrnm,ilinem,itabsm,
     *              ispacm,iferrm,1,1)
CC        rewrite y axis screen menus.
        ner = IPACK(nsuby,ycoord,xdumy,xdumy,xdumy,xdumy,xdumy,
     *              nmxxyz,nypnts,numsc+3,mscrnm,ilinem,itabsm,
     *              ispacm,iferrm,2,1)
        if (lflg .ne. 0) go to 70
      end if
CC
CC      screen numsc+5 (09), discrete points.
  180 nsub = 1
      nn = 5
      nsubs = nsubsc(numsc+nn)
CC      read screen menu and screen data.
  190 numf = IRDSC(numsc+nn,nsub,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
CC      set receptor length units abbrev.
      nvl = 1
      nln = ilinem(nvl) - 2
      call UNITS(mscrnm(8,nln),2,igunit,0,0,1,0)
CC      set second x column
      call MVCH1(mscrnm(8,nln),mscrnm(46,6),2)
CC      set z units
      call UNITS(mscrnm(33,nln),2,izunit,0,0,1,0)
CC      set second z column
      call MVCH1(mscrnm(33,nln),mscrnm(72,6),2)
CC      set y units
      call UNITS(mscrnm(19,nln),3,igunit,0,igrect,1,0)
CC      set second y column
      call MVCH1(mscrnm(19,nln),mscrnm(58,6),3)
      nsc = 16
      nln = 23
      idi = iswopt(6)                                                   120702
      if (idi .eq. 3) idi = 1                                           120702
      call MVCH1(mesg(MAX0(idi,1)),mscrnm(nsc,nln),41)                  120702
CC      display screen menu.
      iferrs = IOSCN(numsc+nn,mscrnm,ilinem,itabsm,ispacm,iferrm,lstrw,
     *               0)
CC      save screen menu.
      if (iferrs .ne. 10) i = IWTSC(numsc+nn,nsub,mscrnm,ilinem,iferrm)
      if (iferrs .eq. 1) go to 220
      jfers = iferrs
      ifwrt = 0
      ker(6) = IFWER(iferrm,nmxlin*nmxfld)
CC      see what user wants to do next.
      if (jfers .eq. 2) then
        ner = IPACK(nsubs,xdscrt,ydscrt,zdscrt,xdumy,xdumy,xdumy,
     *              nmxxyz,nxypnt,numsc+nn,mscrnm,ilinem,itabsm,ispacm,
     *              iferrm,3,0)
        if (ner .eq. 3) then
          if (KERRS(61,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *        go to 220
        else if (ner .gt. 0) then
          if (KERRS(29,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *        go to 220
          if (ner .eq. 1) ifrdwt(2) = -1
        end if
CC        main menu.
        go to 200
      else if (jfers .ge. 3.and.jfers .le. 7) then
CC        aux. menus.
        iferrs = IFRSP(jfers,mscrnm,ilinem,itabsm,ispacm,iferrm,0)
        if (iferrs .eq. 1.or.iferrs .eq. 2) then
CC          terminate or main menu.
          go to 200
        else
CC          redisplay current menu.
          go to 190
        end if
      else if (jfers .ge. 8.and.jfers .le. 10) then
        go to 190
      else if (jfers .eq. 11) then
CC        go back to previous menu.
        nsub = nsub - 1
CC        if first data set, pack discrete data.
        ner = IPACK(nsubs,xdscrt,ydscrt,zdscrt,xdumy,xdumy,xdumy,
     *              nmxxyz,nxypnt,numsc+nn,mscrnm,ilinem,itabsm,ispacm,
     *              iferrm,3,0)
        if (ner .eq. 3) then
          if (KERRS(61,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *        go to 220
        else if (ner .gt. 0) then
          if (KERRS(29,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *        go to 220
        end if
        if (nsub .ge. 1) go to 190
        ker(6) = ner
        if (iswopt(6) .eq. 2.or.iswopt(6) .eq. 3) go to 70              120702
        jfnxt = 1
        go to 230
      end if
CC        go to next menu.
      if (nsub .lt. nsubs) then
        ifdat = 0
CC        see if more discrete menus needed or if data in next menu.
        nvl = IWHER(15,ilinem,itabsm,ispacm,0,4,nln,nsc,nch)
CC        if last data point not set.
        if (NUMC1(mscrnm(nsc,nln),nch) .eq. 0) then
          if (nxypnt .gt. nsub*30.or.IRDSC(numsc+nn,nsub+1,mscrnm,
     *        ilinem,itabsm,ispacm,iferrm,1) .gt. 0) then
            ifdat = 1
          end if
        else
          ifdat = 1
        end if
CC        go to next data set for screen numsc+4 menu.
        if (ifdat .ne. 0) then
          nsub = nsub + 1
          go to 190
        end if
      end if
CC      pack discrete receptor data.
      ner = IPACK(nsubs,xdscrt,ydscrt,zdscrt,xdumy,xdumy,xdumy,
     *            nmxxyz,nxypnt,numsc+nn,mscrnm,ilinem,itabsm,ispacm,
     *            iferrm,3,0)
      if (ner .eq. 3) then
        if (KERRS(61,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *      go to 220
      else if (ner .gt. 0) then
        if (KERRS(29,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *      go to 220
      end if
      if (jfers .eq. 0.and.ner .eq. 1) then
        nsub = 1
        go to 190
      end if
      ker(6) = ner
CC
CC      check to see if required receptor data are specified.
  200 jfnxt = 5
      if (IFWER(ker,6) .eq. 1.or.IFWER(iesav,160) .eq. 1) then
        ifset = -1
      else
        ifset = 1
CC        if no receptors.
        if (nxpnts .le. 0.and.nxypnt .le. 0) ifset = 0
        if (nxpnts .gt. 0.and.nypnts .le. 0) ifset = 0
        if (nxpnts .le. 0.and.nypnts .gt. 0) ifset = 0
      end if
      if (iswopt(6) .le. 0.and.nxypnt .gt. 0) then
        i = 0
  210   if (i .lt. nxypnt.and.iswopt(6) .eq. 0) then
          i = i + 1
          if (zdscrt(i) .gt. 0.0) then
            if (iswopt(6) .ne. 3) iswopt(6) = 1                         120702
          end if
          go to 210
        end if
      end if
      if (ifset .eq. 1) then
        ngrdpt = nxpnts * nypnts
        ntotpt = ngrdpt + nxypnt
CC        reset number of grid points.                                  010700
        ngrdpt = nxpnts * nypnts                                        010700
CC        reset total number of points.                                 010700
        ntotpt = ngrdpt + nxypnt                                        010700
CC        reset calculation offset indices for dos, con, etc.           010700
        j = JINIT(jjjoff,4,0,0)                                         010700
        ntotal = -ntotpt                                                010700
        do i=1,4                                                        010700
          if (iswopt(i) .gt. 0) then                                    010700
            ntotal = ntotal + ntotpt                                    010700
            jjjoff(i) = ntotal                                          010700
          end if                                                        010700
        end do                                                          010700
        ntotal = ntotal + ntotpt                                        010700
      end if
CC      receptor data are 1='set',0='none',-1='err'.
      ifrdwt(2) = ifset
      go to 230
  220 jfnxt = 5
  230 if (iswopt(6) .eq. 2.or.iswopt(6) .eq. 3) then                    120702
CC        restore correct order to zcoord.
  240   do 260 j=1,nypnts
          l = (j - 1) * nxpnts
          k = (j - 1) * nmxxyz
          do 250 i=1,nxpnts
            zcoord(l+i) = zcoord(k+i)
  250     continue
  260   continue
        if (nxpnts .lt. nmxxyz.or.nypnts .lt. nmxxyz) then
          i = JRNIT(zcoord(nxpnts*nypnts+1),nmxxyz*nmxxyz-nxpnts*nypnts,
     *              rmissd,0)
        end if
      end if
      return
      end
