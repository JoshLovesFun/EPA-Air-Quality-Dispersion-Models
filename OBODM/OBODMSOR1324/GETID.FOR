      Subroutine GETID(ier)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - GETID Version 01.3                          --- 072497
CC--- Subroutine description - read initial data from graphics/     ---
CC---                          solution file.                       ---
CC---------------------------------------------------------------------
CC
CC    returns ier - error flag
CC                  0 - no errors.
CC                 >0 - read error or no data.
CC
CC**********************************************************************
CC
      integer*2 i,j,n
CC
      include 'OCNTRL.INC'
      include 'ONAMES.INC'
      include 'OCDSPM.INC'
      include 'OCALCS.INC'
CC
      dimension iobuf(5000)
CC
      equivalence (iobuf(1),calcsa(1))
CC
CC**********************************************************************
CC
      if (iogfil .gt. 0) then
CC        output file information required to restart plotting.
        istrec = 0
        n = 999
        j = -127
   10   j = j + 128
        if (j .le. n) then
          istrec = istrec + 1
          ier = INRAN(istrec,iobuf(j),iogfil)
          if (ier .lt. 0) go to 20
          if (j .eq. 1) then
            n = calcsa(1)
            if (n .lt. 100) then
              ier = 1
              go to 20
            end if
          end if
          go to 10
        end if
        ier = INRAN(istrec+1,iptbuf,iogfil)
        if (ier .lt. 0) go to 20
        ntothr = iptbuf(1)
        ier = 0
        go to 30
CC        read error.
   20   if (istrec .gt. 1) then
          ier = IABS(ier)
          if (KERRS(1,mognam,ier,istrec,xdum,xdum,xdum,xdum,0)
     *        .ge. 1) go to 310
        else
          ier = 1
        end if
        if (iferrs .ne. 1) iferrs = 2
        ifrdwt(5) = 0
        ifnext = 3
        go to 310
   30   n = 2
        verson = calcsa(n)
        do 40 i=1,38
          n = n + 1
          iswopt(i) = calcsa(n)
   40   continue
        n = n + 1
        ifmode = calcsa(n)
        do 50 i=1,50
          n = n + 1
          j = calcsa(n)
          mtitle(i) = CHAR(j)
   50   continue
        nchtrs = NUMC1(mtitle,50)
        do 60 i=1,36
          n = n + 1
          j = calcsa(n)
          mpolnt(i) = CHAR(j)
   60   continue
        do 70 i=1,36
          n = n + 1
          j = calcsa(n)
          mspeci(i) = CHAR(j)
   70   continue
        n = n + 1
        nsourc = calcsa(n)
        do 90 j=1,nsourc
          do 80 i=1,36
            n = n + 1
            k = calcsa(n)
            msoids(i,j) = CHAR(k)
   80     continue
   90   continue
        n = n + 1
        igrect = calcsa(n)
        n = n + 1
        igunit = calcsa(n)
        n = n + 1
        igangu = calcsa(n)
        n = n + 1
        nxpnts = calcsa(n)
        n = n + 1
        nypnts = calcsa(n)
        n = n + 1
        nxypnt = calcsa(n)
        n = n + 1
        ngrdpt = calcsa(n)
        n = n + 1
        ntotpt = calcsa(n)
        n = n + 1
        ntotal = calcsa(n)
        do 100 i=1,4
          n = n + 1
          jjjoff(i) = calcsa(n)
  100   continue
        n = n + 1
        ifsean = calcsa(n)
        n = n + 1
        ifanul = calcsa(n)
        n = n + 1
        wnddir = calcsa(n)
        n = n + 1
        spdshr = calcsa(n)
        do 110 i=1,6
          n = n + 1
          navhrs(i) = calcsa(n)
  110   continue
        do 120 i=1,4
          n = n + 1
          nseasn(i) = calcsa(n)
  120   continue
        if (nxpnts .gt. 0) then
          do 130 i=1,nxpnts
            n = n + 1
            xcoord(i) = calcsa(n)
  130     continue
        end if
        if (nypnts .gt. 0) then
          do 140 i=1,nypnts
            n = n + 1
            ycoord(i) = calcsa(n)
  140     continue
        end if
        if (iswopt(6) .eq. 2.or.iswopt(6) .eq. 3) then                  120702
          j = nxpnts * nypnts
        else
          j = 1
        end if
        if (j .gt. 0) then
          do 150 i=1,j
            n = n + 1
            zcoord(i) = calcsa(n)
  150     continue
        end if
        xpsave = zcoord(1)
        if (nxypnt .gt. 0) then
          do 160 i=1,nxypnt
            n = n + 1
            xdscrt(i) = calcsa(n)
  160     continue
          do 170 i=1,nxypnt
            n = n + 1
            ydscrt(i) = calcsa(n)
  170     continue
          do 180 i=1,nxypnt
            n = n + 1
            zdscrt(i) = calcsa(n)
  180     continue
        end if
        n = n + 1
        grdang = calcsa(n)
        n = n + 1
        xorgin = calcsa(n)
        n = n + 1
        yorgin = calcsa(n)
        n = n + 1
        nvsprt = calcsa(n)
        n = n + 1
        ifgpwn = calcsa(n)
        do 190 i=1,nsourc
          n = n + 1
          isrect(i) = calcsa(n)
  190   continue
        n = n + 1
        densty = calcsa(n)
        do 200 i=1,50
          n = n + 1
          drpupr(i) = calcsa(n)
  200   continue
        do 210 i=1,50
          n = n + 1
          drplwr(i) = calcsa(n)
  210   continue
        do 220 i=1,nsourc
          n = n + 1
          clddxs(i) = calcsa(n)
  220   continue
        do 230 i=1,nsourc
          n = n + 1
          clddys(i) = calcsa(n)
  230   continue
        do 240 i=1,nsourc
          n = n + 1
          clddzs(i) = calcsa(n)
  240   continue
        do 250 i=1,nsourc
          n = n + 1
          clddxe(i) = calcsa(n)
  250   continue
        do 260 i=1,nsourc
          n = n + 1
          clddye(i) = calcsa(n)
  260   continue
        do 270 i=1,nsourc
          n = n + 1
          clddze(i) = calcsa(n)
  270   continue
        n = n + 1
        ifhrly = calcsa(n)
        n = n + 1
        ihrfmt = calcsa(n)
        n = n + 1
        timavg = calcsa(n)
        n = n + 1
        nmxdys = calcsa(n)
        n = n + 1
        nmxhrs = calcsa(n)
        do 280 i=1,366
          n = n + 1
          iscdys(i) = calcsa(n)
  280   continue
        n = n + 1
        ngroup = calcsa(n)
        do 290 i=1,nmxgrp                                               072497
          n = n + 1
          nsogrp(i) = calcsa(n)
  290   continue
        do 300 i=1,2*nmxgrp                                             072497
          n = n + 1
          isogrp(i) = calcsa(n)
  300   continue
        ifcnvt = 0
      end if
  310 return
      end
