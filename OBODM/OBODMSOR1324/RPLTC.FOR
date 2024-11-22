      Subroutine RPLTC
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - RPLTC Version 01.2                          ---
CC--- Subroutine description - plot labels and details.             ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      character*1 mdolr,mrprn,mlprn
      character*8 mdist
      character*22 mlabl
      character*190 mgrpb
CC
      dimension ker(10),spcsym(20),spcpol(20),mlabl(2)
CC
      include 'OCNTRL.INC'
      include 'OCDSPM.INC'
      include 'OWNTRL.INC'
      include 'OCALCS.INC'
      include 'ONAMES.INC'
CC
      equivalence (spcsym(1),ispclc(1)),(spcpol(1),isppol(1))
CC
      data mlabl/'Terrain Contour Levels',
     *           'Isopleth Levels for   '/,
     *     mdolr/'$'/,mrprn/')'/,mlprn/'('/,mdist/'Distance'/
CC
CC**********************************************************************
CC
      iferrs = 0
      i = JINIT(ker,10,0,0)
      do 10 i=1,nmxspl
        spcsym(i) = ispclc(i)
        spcpol(i) = isppol(i)
   10 continue
CC        set default scale, max, min, etc.
      if (IANYI(ipoptn,npoptn,1) .ne. 0) then
        if (nplots .eq. 0) iswplt(2) = 3
        call DMXMQ(0,functx,functy,nxyfnc,nxyfnc,0)
      else
        if (nplots .eq. 0) iswplt(2) = 0
        if (JANYI(ipoptn,npoptn,-2) .gt. 0.or.
     *      JANYI(ipoptn,npoptn,-3) .gt. 0.or.
     *      JANYI(ipoptn,npoptn,-4) .gt. 0) then
          call DMXMQ(0,xcoord,ycoord,nxpnts,nypnts,ifgpol)
        else if (JANYI(ipoptn,npoptn,2) .gt. 0.or.
     *           JANYI(ipoptn,npoptn,3) .gt. 0.or.
     *           JANYI(ipoptn,npoptn,4) .gt. 0) then
          call DMXMQ(0,xteran,yteran,nxtran,nytran,iftpol)
        else
          call DMXMQ(0,xcoord,ycoord,nxpnts,nypnts,ifgpol)
        end if
      end if
CC       default isopleth levels.
      if (IANYI(ipoptn,npoptn,3) .ne. 0.or.IANYI(ipoptn,npoptn,4)
     *    .ne. 0) then
        do 40 icontr=1,2
          jj = 1
          joffs1 = -1
          if (icontr .eq. 1) then
            l = JANYI(ipoptn,npoptn,4)
            if (JANYI(ipoptn,npoptn,3)+l .eq. 0) then
              jj = 0
            else
              joffs1 = ioffst(1)
            end if
          else
            l = JANYI(ipoptn,npoptn,-4)
            if (JANYI(ipoptn,npoptn,-3)+l .eq. 0) then
              jj = 0
            else
              joffs1 = ioffst(2)
            end if
          end if
          if (jj .ne. 0) then
            if (l .gt. 0) then
              ymn = csmins(icontr)
              ymx = csmaxs(icontr)
              if (ymn .le. 0.0) then
                if (ymn .lt. 0.0) then
                  zmincs(icontr) = ALOG(ABS(ymn)) + 1.0e-4
                  zmincs(icontr) = FSEXP(zmincs(icontr)) * 2.0
                else
                  zmincs(icontr) = 1.0e-20
                  xx2 = ymx * 1.0e-35
                  if (xx2 .gt. zmincs(icontr)) zmincs(icontr) = xx2
                end if
                zminds(icontr) = 1.0
                if (zmincs(icontr) .lt. 1.0) then
                  zminds(icontr) = 1.0 / zmincs(icontr)
                end if
              else
                zminds(icontr) = 1.0
                if (ymn .lt. 1.0) zminds(icontr) = 1.0 / ymn
                zmincs(icontr) = 0.0
              end if
            end if
CC            set isopleth levels to plot.
            if (ncontr(icontr) .gt. 0) then
              j = 0
              do 20 i=1,ncontr(icontr)
                if (contrs(i,icontr) .ge. csmins(icontr).and.
     *              contrs(i,icontr) .le. csmaxs(icontr)) then
                  j = j + 1
                  contrs(j,icontr) = contrs(i,icontr)
                end if
   20         continue
              ncontr(icontr) = j
              if (ncontr(icontr) .gt. 0.and.ifchlb .eq. 0) go to 40
            end if
CC            calculate default isopleth levels.
            ccmax = CNRXQ(csmaxs(icontr))
            ccmin = CNRXQ(csmins(icontr))
            dd = 0.2 * (ccmax - ccmin)
            if (ccmin+4.0*dd .gt. csmaxs(icontr).or.ccmin+dd .lt.
     *          csmins(icontr)) then
              dd = 0.2 * (csmaxs(icontr) - csmins(icontr))
              ccmin = csmins(icontr)
            end if
            contrs(1,icontr) = ccmin + dd
            do 30 i=2,4
              contrs(i,icontr) = contrs(i-1,icontr) + dd
   30       continue
            ncontr(icontr) = 4
          end if
   40   continue
      end if
      if (ifchlb .ne. 0) then
CC        replace headings in array mscrch.
        if (ifavrg .lt. 2.or.ifavrg .gt. 3) then
          call RLABL(ifavrg,kssndx,nhravg,1)                            120698
        else
          call RLABL(ifavrg,kssndx,iseasn,1)                            120698
        end if
      end if
CC
CC      start screen number 63.
   50 if (IANYI(ipoptn,npoptn,5) .ne. 0.and.iswplt(26) .eq. 0) then
CC
CC      display and read data for special symbols.
CC
        nsub = 1
        numsc = numscs(8) + 6
        nsubs = nsubsc(numsc)
CC        read screen menu and screen data.
   60   numf = IRDSC(numsc,nsub,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
        nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
        if (NUMC1(mscrnm(nsc,nln),nch) .le. 0.and.nspcls .gt. 0.and.
     *      nsub .eq. 1) then
          ner = IPACK(nsubs,spclxs,spclys,spclht,spclan,spcsym,spcpol,
     *                nmxspl,nspcls,numsc,mscrnm,ilinem,itabsm,ispacm,
     *                iferrm,5,1)
          if (ner .eq. 3) then
            if (KERRS(61,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *          go to 320
          end if
          go to 60
        end if
CC        set symbol index numbers.
        nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
        do 70 i=1,10
          xdumy = (nsub - 1) * 10 + i
          j = IWTBF(mscrnm(nsc-7,nln+i-1),3,xdumy,0,iflrjs)
   70   continue
CC        display screen menu.
        iferrs = IOSCN(numsc,mscrnm,ilinem,itabsm,ispacm,iferrm,lstrw,0)
CC        save screen menu.
        if (iferrs .ne. 10) i = IWTSC(numsc,nsub,mscrnm,ilinem,iferrm)
CC        if terminate.
        if (iferrs .eq. 1) go to 320
        ker(1) = IFWER(iferrm,nmxlin*nmxfld)
        jfers = iferrs
        ifwrt = 0
CC
CC        check users response.
        if (jfers .eq. 2) then
          ifnext = 3
          go to 320
        else if (jfers .ge. 3.and.jfers .le. 7) then
          iferrs = IFRSP(jfers,mscrnm,ilinem,itabsm,ispacm,iferrm,0)
CC          terminate or main menu.
          if (iferrs .eq. 1.or.iferrs .eq. 2) then
            ifnext = 3
            jfers = iferrs
            go to 320
          else
CC            redisplay menu.
            go to 60
          end if
        else if (jfers .eq. 8) then
          iswplt(26) = 1
        else if (jfers .ge. 9.and.jfers .le. 10) then
          go to 60
        else if (jfers .eq. 11) then
CC          go to previous menu.
          nsub = nsub - 1
          ner = IPACK(nsubs,spclxs,spclys,spclht,spclan,spcsym,spcpol,
     *                nmxspl,nspcls,numsc,mscrnm,ilinem,itabsm,ispacm,
     *                iferrm,5,0)
          if (ner .eq. 3) then
            if (KERRS(61,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *          go to 320
          end if
          if (nsub .ge. 1) go to 60
          ker(1) = ner
          ifnext = 15
          go to 320
        end if
CC        next page.
        if (nsub .lt. nsubs) then
          ifdat = 0
CC          see if more symbol menus needed or if data in next menu.
          nvl = IWHER(10,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
          if (NUMC1(mscrnm(nsc,nln),nch) .eq. 0) then
            if (nspcls .gt. nsub*10.or.IRDSC(numsc,nsub+1,mscrnm,ilinem,
     *          itabsm,ispacm,iferrm,1) .gt. 0) then
              ifdat = 1
            end if
          else
            ifdat = 1
          end if
CC          go to next data set for screen numsc menu.
          if (ifdat .ne. 0) then
            nsub = nsub + 1
            go to 60
          end if
        end if
CC          pack special symbol data and go to next source menu.
        ner = IPACK(nsubs,spclxs,spclys,spclht,spclan,spcsym,spcpol,
     *              nmxspl,nspcls,numsc,mscrnm,ilinem,itabsm,ispacm,
     *              iferrm,5,0)
        if (ner .eq. 3) then
          if (KERRS(61,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *        go to 320
        else if (ner .gt. 0) then
          if (KERRS(29,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *        go to 320
        end if
        if (jfers .eq. 0.and.ner .eq. 1) then
          iswplt(26) = 0
          go to 50
        end if
        ker(1) = ner
      end if
CC
CC
      ker(2) = 0
   80 if (JANYI(ipoptn,npoptn,3) .gt. 0.or.JANYI(ipoptn,npoptn,4) .gt.
     *    0) then
CC
CC        screen numsc (64), terrain contour levels.
        numsc = numscs(8) + 7
        numf = IRDSC(numsc,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
CC
CC        indicate quantity.
        nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
        call MVCH1(mlabl(1),mscrnm(20,nln-4),22)
CC        indicate min. and max. values.
        i = IWTBF(mscrnm(24,nln-2),12,csmaxs(1),2,iflrjs)
        i = IWTBF(mscrnm(50,nln-2),12,csmins(1),2,iflrjs)
        if (ker(2) .eq. 0) then
CC          display default values.
          if (ncontr(1) .gt. 0) then
            n = 0
            nvl = 0
            do 100 j=1,4
              nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
              do 90 i=1,5
                nsc = JWHER(nvl,itabsm,ispacm,i,nch)
                n = n + 1
                if (n .le. ncontr(1)) then
                  k = IWTBF(mscrnm(nsc,nln),nch,contrs(n,1),2,iflrjs)
                else
                  call MBLK1(mscrnm(nsc,nln),nch)
                end if
   90         continue
  100       continue
          end if
        end if
        if (iswplt(26) .eq. 0) then
CC          display screen menu.
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
          go to 320
        else if (jfers .eq. 2) then
CC          main menu.
          ifnext = 3
          go to 320
        else if (jfers .ge. 3.and.jfers .le. 7) then
CC          display aux. menus.
          iferrs = IFRSP(jfers,mscrnm,ilinem,itabsm,ispacm,iferrm,0)
          if (iferrs .eq. 1.or.iferrs .eq. 2) then
CC            terminate or main menu.
            ifnext = 3
            jfers = iferrs
            go to 320
          else
CC            redisplay menu.
            go to 80
          end if
        else if (jfers .eq. 8) then
          iswplt(26) = 1
        else if (jfers .ge. 9.and.jfers .le. 10) then
          go to 80
        else if (jfers .eq. 11) then
          if (IANYI(ipoptn,npoptn,5) .ne. 0) go to 50
          ifnext = 15
          go to 300
        end if
CC        turn off error indicators for this menu.
        i = JINIT(iferrm,nmxlin*nmxfld,0,0)
CC
CC        check data.
CC
CC        get contour levels.
        n = 0
        do 120 i=1,4
          nvl = IWHER(i,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
          do 110 j=1,5
            nsc = JWHER(nvl,itabsm,ispacm,j,nch)
            if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
              n = n + 1
            end if
  110     continue
  120   continue
        if (n .le. 0) n = -5
        a = (csmaxs(1) - csmins(1)) / FLOAT(IABS(n) + 1)
        ncontr(1) = 0
        if (n .gt. 0) then
          k = 4
        else
          k = 1
        end if
        do 140 i=1,k
          nvl = IWHER(i,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
          do 130 j=1,5
            nsc = JWHER(nvl,itabsm,ispacm,j,nch)
            l = IRDBF(mscrnm(nsc,nln),nch,xdumy)
            if (l .eq. 0) then
              ncontr(1) = ncontr(1) + 1
              contrs(ncontr(1),1) = xdumy
            else if (l .lt. 0) then
              if ((l .eq. -3.and.n .lt. 0).or.l .eq. -2) then
                ncontr(1) = ncontr(1) + 1
                contrs(ncontr(1),1) = csmaxs(1) - FLOAT(i) * a
                l = IWTBF(mscrnm(nsc,nln),nch,contrs(ncontr(1),1),4,
     *                    iflrjs)
                ifwrt = 1
              end if
            else
              iferrm(nvl,j) = 1
            end if
  130     continue
  140   continue
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
        if (ker(2) .eq. 1) then
          iswplt(26) = 0
          go to 80
        end if
      end if
CC
CC
      ker(3) = 0
  150 if (JANYI(ipoptn,npoptn,-3) .gt. 0.or.JANYI(ipoptn,npoptn,-4)
     *    .gt. 0) then
CC
CC        screen numsc (64), calculation isopleth levels.
        numsc = numscs(8) + 7
        numf = IRDSC(numsc,2,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
CC
CC        indicate quantity.
        nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
        call MVCH1(mlabl(2),mscrnm(20,nln-4),22)
        call MVCH1(mscrch(ilbstr(4)),mscrnm(43,nln-4),nlbsch(4))
CC        indicate min. and max. values.
        i = IWTBF(mscrnm(24,nln-2),-12,csmaxs(2),6,iflrjs)
        i = IWTBF(mscrnm(50,nln-2),-12,csmins(2),6,iflrjs)
        if (ker(3) .eq. 0) then
CC          display default values.
          if (ncontr(2) .gt. 0) then
            n = 0
            nvl = 0
            do 170 j=1,4
              nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
              do 160 i=1,5
                nsc = JWHER(nvl,itabsm,ispacm,i,nch)
                n = n + 1
                if (n .le. ncontr(2)) then
                  k = IWTBF(mscrnm(nsc,nln),nch,contrs(n,2),4,iflrjs)
                else
                  call MBLK1(mscrnm(nsc,nln),nch)
                end if
  160         continue
  170       continue
          end if
        end if
        if (iswplt(26) .eq. 0) then
CC          display screen menu.
          iferrs = IOSCN(numsc,mscrnm,ilinem,itabsm,ispacm,iferrm,
     *                   lstrw,0)
        else
          iferrs = 0
        end if
CC        save screen menu.
        if (iferrs .ne. 10) i = IWTSC(numsc,2,mscrnm,ilinem,iferrm)
CC        save iferrs.
        jfers = iferrs
        ifwrt = 0
        ker(3) = IFWER(iferrm,nmxlin*nmxfld)
CC
CC        check user response to menu.
        if (jfers .eq. 1) then
CC          terminate.
          go to 320
        else if (jfers .eq. 2) then
CC          main menu.
          ifnext = 3
          go to 320
        else if (jfers .ge. 3.and.jfers .le. 7) then
CC          display aux. menus.
          iferrs = IFRSP(jfers,mscrnm,ilinem,itabsm,ispacm,iferrm,0)
          if (iferrs .eq. 1.or.iferrs .eq. 2) then
CC            terminate or main menu.
            ifnext = 3
            jfers = iferrs
            go to 320
          else
CC            redisplay menu.
            go to 150
          end if
        else if (jfers .eq. 8) then
          iswplt(26) = 1
        else if (jfers .ge. 9.and.jfers .le. 10) then
          go to 150
        else if (jfers .eq. 11) then
          if (JANYI(ipoptn,npoptn,3) .gt. 0.or.JANYI(ipoptn,npoptn,4)
     *        .gt. 0) go to 80
          if (IANYI(ipoptn,npoptn,5) .ne. 0) go to 50
          ifnext = 15
          go to 300
        end if
CC        turn off error indicators for this menu.
        i = JINIT(iferrm,nmxlin*nmxfld,0,0)
CC
CC        check data.
CC
CC        get isopleth levels.
        n = 0
        do 190 i=1,4
          nvl = IWHER(i,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
          do 180 j=1,5
            nsc = JWHER(nvl,itabsm,ispacm,j,nch)
            if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
              n = n + 1
            end if
  180     continue
  190   continue
        if (n .le. 0) n = -5
        a = (csmaxs(2) - csmins(2)) / FLOAT(IABS(n) + 1)
        ncontr(2) = 0
        if (n .gt. 0) then
          k = 4
        else
          k = 1
        end if
        do 210 i=1,k
          nvl = IWHER(i,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
          do 200 j=1,5
            nsc = JWHER(nvl,itabsm,ispacm,j,nch)
            l = IRDBF(mscrnm(nsc,nln),nch,xdumy)
            if (l .eq. 0) then
              ncontr(2) = ncontr(2) + 1
              contrs(ncontr(2),2) = xdumy
            else if (l .lt. 0) then
              if ((l .eq. -3.and.n .lt. 0).or.l .eq. -2) then
                ncontr(2) = ncontr(2) + 1
                contrs(ncontr(2),2) = csmaxs(2) - FLOAT(i) * a
                l = IWTBF(mscrnm(nsc,nln),nch,contrs(ncontr(2),2),4,
     *                    iflrjs)
                ifwrt = 1
              end if
            else
              iferrm(nvl,j) = 1
            end if
  200     continue
  210   continue
CC
        ner = IFWER(iferrm,nmxlin*nmxfld)
        if (ifwrt .gt. 0.or.ner .gt. 0.or.(ner .eq. 0.and.
     *      ker(3) .ne. 0)) then
CC          rewrite screen data.
          i = IWTSC(numsc,2,mscrnm,ilinem,iferrm)
        end if
        if (ner .gt. 0) then
          if (KERRP(22,0) .ge. 1) go to 320
        end if
        ker(3) = ner
        if (ker(3) .eq. 1) then
          iswplt(26) = 0
          go to 150
        end if
      end if
CC
CC        screen numsc (65), plot axes labels and legend.
  220 numsc = numscs(8) + 8
      numf = IRDSC(numsc,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
      if (ifchlb .ne. 0) then
CC        generate heading labels.
        call MBLK1(mlngnd,360)
        call MBLK1(mlablx,60)
        call MBLK1(mlably,60)
CC        set up source group label.
        call PLABL(ngrplt,ngrpb,mgrpb,mgrpb)
CC        get main legend
        nchlcr = 0
        if (nchtrs .gt. 0) then
          call MVCH1(mtitle,mlngnd,nchtrs)
          nchtcr = LRJST(mlngnd,nchtrs,1)
          mlngnd(nchtcr+1) = mdolr
          nchlcr = nchtcr + 1
        end if
        if (nlbsch(1) .gt. 0) then
          call MVCH1(mscrch(ilbstr(1)),mlngnd(nchlcr+1),nlbsch(1))
          nchlcr = nchlcr + LRJST(mlngnd(nchlcr+1),nlbsch(1),1)
        end if
        if (nchlcr .lt. 360) then
          nchlcr = nchlcr + 1
          mlngnd(nchlcr) = mdolr
          if (nchlcr .lt. 360) then
            if (nchlcr .lt. 360) then
              if (ngrpb .gt. 0) then
CC                get sublegend
                n = ngrpb
                if (nchlcr+n .gt. 360) n = 360 - nchlcr
                call MVCH1(mgrpb,mlngnd(nchlcr+1),n)
                nchlcr = nchlcr + n
                if (nchlcr .lt. 360) then
                  nchlcr = nchlcr + 1
                  mlngnd(nchlcr) = mdolr
                end if
              end if
              if (nchlcr .lt. 360) then
                if (nhrlab .gt. 0) then
CC                  get sublegend
                  n = nhrlab
                  if (nchlcr+n .gt. 360) n = 360 - nchlcr
                  call MVCH1(mhrlab,mlngnd(nchlcr+1),n)
                  nchlcr = nchlcr + n
                  if (nchlcr .lt. 360) then
                    nchlcr = nchlcr + 1
                    mlngnd(nchlcr) = mdolr
                  end if
                end if
                if (nchlcr .lt. 360) then
                  if (nlbsch(5) .gt. 0) then
                    n = LRJST(mscrch(ilbstr(5)),nlbsch(5),1)
                    if (nchlcr+n .gt. 360) n = 360 - nchlcr
                    call MVCH1(mscrch(ilbstr(5)),mlngnd(nchlcr+1),n)
                    nchlcr = nchlcr + n
                    if (nchlcr .lt. 360) then
                      nchlcr = nchlcr + 1
                      mlngnd(nchlcr) = mdolr
                    end if
                  end if
                end if
              end if
            end if
          end if
        end if
        if (nchlcr .lt. 360) then
          nchlcr = nchlcr + 1
          mlngnd(nchlcr) = mdolr
        end if
CC-DEBUGS
CC>     if (ifdbug .gt. 0.and.ifdbug .lt. 7) then                       020797
CC>       WRITE (*,9000) (ipoptn(iiii),iiii=1,npoptn)
CC>       WRITE (*,9001) iplfil,iscfil,itrfil,impfil,iftran,ifgrph
CC>       pause 'Enter or Return to continue. > '
CC>     end if
CC-DEBUGE
CC        set x and y axis labels
        if (IANYI(ipoptn,npoptn,1) .ne. 0) then
          call MVCH1(mdist,mlablx,8)
          mlablx(10) = mlprn
          call UNITS(mlablx(11),2,igunit,0,0,1,0)
          if (mlablx(11) .eq. mblnk1) then
            mlablx(11) = mlablx(12)
            mlablx(12) = mrprn
          else
            mlablx(13) = mrprn
          end if
          call MVCH1(modelt(kssndx),mlably,26)
        else
          mlablx(1) = malpha(24)
          call MVCH1(mdist,mlablx(3),8)
          call MVCH1(mscrch(ilbstr(2)),mlablx(12),nlbsch(2))
          if (ifdbug .eq. 26) then                                      020797
            mlably(1) = malpha(26)                                      020797
          else                                                          020797
            mlably(1) = malpha(25)                                            
          end if                                                        020797
          call MVCH1(mdist,mlably(3),8)
          if (ifgpol .eq. 0) then
            call MVCH1(mscrch(ilbstr(3)),mlably(12),nlbsch(3))
          else
            call MVCH1(mscrch(ilbstr(2)),mlably(12),nlbsch(2))
          end if
        end if
        nchxcr = IPKBK(mlablx,60)
        nchycr = IPKBK(mlably,60)
      end if
CC
CC      set default labels.
      nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
      if (nchycr .gt. 0) then
        call MBLK1(mscrnm(nsc,nln),nch)
        call MVCH1(mlably,mscrnm(nsc,nln),MIN0(nchycr,nch))
        i = LRJST(mscrnm(nsc,nln),nch,iflrjs)
      end if
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (nchxcr .gt. 0) then
        call MBLK1(mscrnm(nsc,nln),nch)
        call MVCH1(mlablx,mscrnm(nsc,nln),MIN0(nchxcr,nch))
        i = LRJST(mscrnm(nsc,nln),nch,iflrjs)
      end if
      if (nchlcr .gt. 0) then
        nl = IDOLQ(mlngnd,1,-nchlcr)
  240   do 250 i=1,6
          nvl = IWHER(i+2,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
          call MBLK1(mscrnm(nsc,nln),nch)
  250   continue
        if (nl .gt. 6) then
          nvl = 2
          ne = 0
  260     ns = ne + 1
          if (ns .le. nchlcr) then
            ne = ns + 59
            if (ne .gt. nchlcr) ne = nchlcr
            nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
            call MVCH1(mlngnd(ns),mscrnm(nsc,nln),MIN0(ne-ns+1,nch))
            go to 260
          end if
        else
          ns = 1
          ne = nchlcr
          iend = 0
          nvl = 2
  270     i = IDOLQ(mlngnd(ns),1,ne)
          if (i .gt. 0.or.ns .eq. 1) then
            if (i .gt. 0.and.i+ns-1 .lt. nchlcr) then
              if (mlngnd(i+ns) .eq. mdolr) then
                i = i + 1
                iend = 1
              end if
            end if
            nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
            if (nvl .gt. 6.or.i .gt. 60) then
              nl = 7
              go to 240
            end if
            call MVCH1(mlngnd(ns),mscrnm(nsc,nln),i)
            j = LRJST(mscrnm(nsc,nln),nch,iflrjs)
            ns = ns + i
            ne = nchlcr - ns + 1
            if (iend .eq. 0.and.ne .gt. 0) go to 270
          end if
        end if
      end if
      if (iswplt(26) .eq. 0) then
CC        display screen menu.
        iferrs = IOSCN(numsc,mscrnm,ilinem,itabsm,ispacm,iferrm,
     *                 lstrw,38)
      else
        iferrs = 0
      end if
CC      save screen menu.
      if (iferrs .ne. 10) i = IWTSC(numsc,1,mscrnm,ilinem,iferrm)
CC      save iferrs.
      jfers = iferrs
      ifwrt = 0
      ker(4) = IFWER(iferrm,nmxlin*nmxfld)
CC
CC      check user response to menu.
      if (jfers .eq. 1) then
CC        terminate.
        go to 320
      else if (jfers .eq. 2) then
CC        main menu.
        ifnext = 3
        go to 320
      else if (jfers .ge. 3.and.jfers .le. 7) then
CC        display aux. menus.
        iferrs = IFRSP(jfers,mscrnm,ilinem,itabsm,ispacm,iferrm,0)
        if (iferrs .eq. 1.or.iferrs .eq. 2) then
CC          terminate or main menu.
          ifnext = 3
          jfers = iferrs
          go to 320
        else
CC          redisplay menu.
          go to 220
        end if
      else if (jfers .eq. 8) then
        iswplt(26) = 1
      else if (jfers .ge. 9.and.jfers .le. 10) then
        go to 220
      else if (jfers .eq. 11) then
        if (JANYI(ipoptn,npoptn,-3) .gt. 0.or.JANYI(ipoptn,npoptn,-4)
     *      .gt. 0) go to 150
        if (JANYI(ipoptn,npoptn,3) .gt. 0.or.JANYI(ipoptn,npoptn,4)
     *      .gt. 0) go to 80
        if (IANYI(ipoptn,npoptn,5) .ne. 0) then
          iswplt(26) = 0
          go to 50
        end if
        ifnext = 15
        go to 300
      end if
CC      turn off error indicators for this menu.
      i = JINIT(iferrm,nmxlin*nmxfld,0,0)
CC
CC      check data.
CC
CC      get y axis label.
      nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
      call MBLK1(mlably,60)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        call MVCH1(mscrnm(nsc,nln),mlably,MIN0(nch,60))
        nchycr = LRJST(mlably,60,1)
      else
        nchycr = 0
      end if
CC
CC      get x axis label.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      call MBLK1(mlablx,60)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        call MVCH1(mscrnm(nsc,nln),mlablx,MIN0(nch,60))
        nchxcr = LRJST(mlablx,60,1)
      else
        nchxcr = 0
      end if
CC
CC      get legend.
      call MBLK1(mlngnd,360)
      nchlcr = 0
      do 280 i=1,6
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        n = NUMC1(mscrnm(nsc,nln),nch)
        if (n .gt. 0) then
          call MVCH1(mscrnm(nsc,nln),mlngnd(nchlcr+1),n)
          j = LRJST(mlngnd(nchlcr+1),n,1)
          nchlcr = NUMC1(mlngnd,nchlcr+n)
        end if
  280 continue
CC
CC      see if detailed plot data is to be specified.
      nvl = IWHER(9,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
      if (mscrnm(nsc,nln) .ne. mblnk1) then
        if (IFCHR(mscrnm(nsc,nln),25) .ne. 0) then
          ifgrph = 2
        else if (IFCHR(mscrnm(nsc,nln),14) .ne. 0) then
          ifgrph = 1
        else
          iferrm(nvl,1) = 1
        end if
      else
        ifgrph = 1
      end if
CC
      ner = IFWER(iferrm,nmxlin*nmxfld)
      if (ifwrt .gt. 0.or.ner .gt. 0.or.(ner .eq. 0.and.
     *    ker(4) .ne. 0)) then
CC        rewrite screen data.
        i = IWTSC(numsc,1,mscrnm,ilinem,iferrm)
      end if
      if (ner .gt. 0) then
        if (KERRP(22,0) .ge. 1) go to 320
      end if
      ker(4) = ner
      if (ker(4) .eq. 1) then
        iswplt(26) = 0
        go to 220
      end if
CC
CC
CC      check if data complete.
  290 if (IFWER(ker,10) .eq. 1) then
        ifset = -1
        iswplt(26) = 0
      else
        ifset = 1
        ifrdwt(6) = ifset
        if (iswplt(26) .gt. 0) ifgrph = 1
        if (ifset .le. 0) then
          ifnext = 3
        end if
      end if
  300 do 310 i=1,nmxspl
        ispclc(i) = spcsym(i)
        isppol(i) = spcpol(i)
  310 continue
  320 return
CC
 9000 format (' RPLTC - ipoptn=',20i4)
 9001 format (' RPLTC - iplfil,iscfil,itrfil,impfil,iftran,ifgrph=',6i5)
      end
      Function IDOLQ(mbufer,ichr,nchr)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IDOLQ Version 01.0                          ---
CC--- Subroutine description - find a dollar sign sentinel symbol in---
CC---                          a string.                            ---
CC---------------------------------------------------------------------
CC
CC        mbufer - character array to search
CC        ichr   - start index in array mbufer to start search
CC        nchr   - end index in array mbufer to end search. also,
CC                 two consecutive $ symbols ($$) end the search.
CC                 if negative, the total number of $ symbols is
CC                 returned. two consecutive $$ equals one in the
CC                 returned count.
CC                 if positive the index of the first $ found is
CC                 returned.
CC
CC        returns - IDOLQ
CC                  0 - no $ symbol found
CC                 >0 - if nchr > 0, then the index from ichr to nchr
CC                                   of the dollar symbol character is
CC                                   returned.
CC                      if nchr < 0, then the number of $ symbols is
CC                                   returned, $$ counts as 1.
CC
CC*********************************************************************
CC
      integer*2 i,j,ldol
CC
      character*1 mbufer,mdol
CC
      dimension mbufer(300)
CC
      data mdol/'$'/
CC
CC*********************************************************************
CC
      IDOLQ = 0
      if (IABS(nchr) .ge. ichr) then
        j = 0
        ldol = 0
        i = ichr - 1
   10   if (i .lt. IABS(nchr).and.j .eq. 0) then
          i = i + 1
          if (mbufer(i) .eq. mdol) then
            ldol = ldol + 1
            if (ldol .gt. 1) then
              j = 1
            else
              IDOLQ = IDOLQ + 1
              if (nchr .gt. 0) then
                IDOLQ = i
                j = 1
              end if
            end if
          else
            ldol = 0
          end if
          go to 10
        end if
      end if
      return
      end
      Subroutine PLABL(ngrpn,ngrpb,mgrpb,mgrpr)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - PLABL Version 01.1                          ---
CC--- Subroutine description - builds label for plot source groups. ---
CC---                                                               ---
CC---------------------------------------------------------------------
CC
CC    ngrpn - source group number desired.
CC    ngrpb - number of characters in returned group label mgrpb.
CC    mgprf - returned group label.
CC
CC**********************************************************************
CC
      character*1 mrprn,mgrpr
      character*4 mbufr
      character*190 mgrpb
CC
      dimension mgrpr(190)
CC
      include 'OCNTRL.INC'
      include 'OCALCS.INC'
      include 'ONAMES.INC'
      include 'OWNTRL.INC'
      include 'OCDSPM.INC'
CC
      data mrprn/')'/
CC
CC*******************************************************************
CC
CC       source group labeling
      ngrpb = 0
      mgrpb = mblnk1
      if (ngrplt .le. 0) then
        if (nsourc .gt. 1) then
CC          due to all sources.
          WRITE (mgrpb,9000)
          i = 20
        else
CC          no label required for single source.
          i = 0
        end if
      else
CC       (Due to source group ii including sources: ##,##,##,...
        WRITE (mgrpb,9001) ngrpn
        i = 34
        do 10 k=1,ngrpct
          m = jsogrp(k)
          if (m .gt. 0) then
            WRITE (mbufr,9002) m
            call MVCH1(mbufr,mgrpr(i+1),3)
            i = i + 3
          else
            j = 0
            if (k .gt. 1) j = jsogrp(k-1)
            if (k .eq. 1.or.j .lt. 0.or.j .gt. IABS(m)) then
              WRITE (mbufr,9003)
              call MVCH1(mbufr,mgrpr(i+1),3)
              i = i + 3
              WRITE (mbufr,9002) IABS(m)
              call MVCH1(mbufr,mgrpr(i+1),3)
              i = i + 3
            else
              WRITE (mbufr,9004) IABS(m)
              call MVCH1(mbufr,mgrpr(i),4)
              i = i + 3
            end if
          end if
   10   continue
        mgrpr(i) = mrprn
      end if
      if (i .gt. 0) then
        ngrpb = JPACK(mgrpr,i)
        call MVCH1(mgrpr,mgrpb,ngrpb)
      end if
      return
CC
 9000 format ('(Due to all sources)')
 9001 format ('(Due to source group >',i2,', sources:')
 9002 format (i2,',')
 9003 format (' 1-')
 9004 format ('-',i2,',')
      end
