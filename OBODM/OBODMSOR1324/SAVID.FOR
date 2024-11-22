      Subroutine SAVID
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - SAVID Version 01.3                          --- 072497
CC--- Subroutine description - store input parameters to graphics/  ---
CC---                          solution file.                       ---
CC---------------------------------------------------------------------
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
        n = 2
        calcsa(n) = verson
        do 10 i=1,38
          n = n + 1
          calcsa(n) = iswopt(i)
   10   continue
        n = n + 1
        calcsa(n) = ifmode
        do 20 i=1,50
          j = ICHAR(mtitle(i))
          n = n + 1
          calcsa(n) = j
   20   continue
        do 30 i=1,36
          j = ICHAR(mpolnt(i))
          n = n + 1
          calcsa(n) = j
   30   continue
        do 40 i=1,36
          j = ICHAR(mspeci(i))
          n = n + 1
          calcsa(n) = j
   40   continue
        n = n + 1
        calcsa(n) = nsourc
        do 60 j=1,nsourc
          do 50 i=1,36
            k = ICHAR(msoids(i,j))
            n = n + 1
            calcsa(n) = k
   50     continue
   60   continue
        n = n + 1
        calcsa(n) = igrect
        n = n + 1
        calcsa(n) = igunit
        n = n + 1
        calcsa(n) = igangu
        n = n + 1
        calcsa(n) = nxpnts
        n = n + 1
        calcsa(n) = nypnts
        n = n + 1
        calcsa(n) = nxypnt
        n = n + 1
        calcsa(n) = ngrdpt
        n = n + 1
        calcsa(n) = ntotpt
        n = n + 1
        calcsa(n) = ntotal
        do 70 i=1,4
          n = n + 1
          calcsa(n) = jjjoff(i)
   70   continue
        n = n + 1
        calcsa(n) = ifsean
        n = n + 1
        calcsa(n) = ifanul
        n = n + 1
        calcsa(n) = wnddir
        n = n + 1
        calcsa(n) = spdshr
        do 80 i=1,6
          n = n + 1
          calcsa(n) = navhrs(i)
   80   continue
        do 90 i=1,4
          n = n + 1
          calcsa(n) = nseasn(i)
   90   continue
        if (nxpnts .gt. 0) then
          do 100 i=1,nxpnts
            n = n + 1
            calcsa(n) = xcoord(i)
  100     continue
        end if
        if (nypnts .gt. 0) then
          do 110 i=1,nypnts
            n = n + 1
            calcsa(n) = ycoord(i)
  110     continue
        end if
        if (iswopt(6) .eq. 2.or.iswopt(6) .eq. 3) then                  120702
          j = nxpnts * nypnts
        else
          j = 1
        end if
        if (j .gt. 0) then
          do 120 i=1,j
            n = n + 1
            calcsa(n) = zcoord(i)
  120     continue
        end if
        if (nxypnt .gt. 0) then
          do 130 i=1,nxypnt
            n = n + 1
            calcsa(n) = xdscrt(i)
  130     continue
          do 140 i=1,nxypnt
            n = n + 1
            calcsa(n) = ydscrt(i)
  140     continue
          do 150 i=1,nxypnt
            n = n + 1
            calcsa(n) = zdscrt(i)
  150     continue
        end if
        n = n + 1
        calcsa(n) = grdang
        n = n + 1
        calcsa(n) = xorgin
        n = n + 1
        calcsa(n) = yorgin
        n = n + 1
        calcsa(n) = nvsprt
        n = n + 1
        calcsa(n) = ifgpwn
        do 160 i=1,nsourc
          n = n + 1
          calcsa(n) = isrect(i)
  160   continue
        n = n + 1
        calcsa(n) = densty
        do 170 i=1,50
          n = n + 1
          calcsa(n) = drpupr(i)
  170   continue
        do 180 i=1,50
          n = n + 1
          calcsa(n) = drplwr(i)
  180   continue
        do 190 i=1,nsourc
          n = n + 1
          calcsa(n) = clddxs(i)
  190   continue
        do 200 i=1,nsourc
          n = n + 1
          calcsa(n) = clddys(i)
  200   continue
        do 210 i=1,nsourc
          n = n + 1
          calcsa(n) = clddzs(i)
  210   continue
        do 220 i=1,nsourc
          n = n + 1
          calcsa(n) = clddxe(i)
  220   continue
        do 230 i=1,nsourc
          n = n + 1
          calcsa(n) = clddye(i)
  230   continue
        do 240 i=1,nsourc
          n = n + 1
          calcsa(n) = clddze(i)
  240   continue
        n = n + 1
        calcsa(n) = ifhrly
        n = n + 1
        calcsa(n) = ihrfmt
        n = n + 1
        calcsa(n) = timavg
        n = n + 1
        calcsa(n) = nmxdys
        n = n + 1
        calcsa(n) = nmxhrs
        do 250 i=1,366
          n = n + 1
          calcsa(n) = iscdys(i)
  250   continue
        n = n + 1
        calcsa(n) = ngroup
        do 260 i=1,nmxgrp                                               072497
          n = n + 1
          calcsa(n) = nsogrp(i)
  260   continue
        do 270 i=1,2*nmxgrp                                             072497
          n = n + 1
          calcsa(n) = isogrp(i)
  270   continue
        calcsa(1) = n
CC        output file information required to restart plotting.
        istrec = 0
        j = -127
  280   j = j + 128
        if (j .le. n) then
          istrec = istrec + 1
          ier = IORAN(istrec,iobuf(j),iogfil)
          if (ier .gt. 0) go to 290
          go to 280
        end if
        go to 300
CC        write error.
  290   i = KERRS(2,mognam,ier,istrec,xdum,xdum,xdum,xdum,0)
        if (ier .eq. 6422) then
          k = (n * 128 + nmxhrs * (2 * nsourc + 1) + 9 +
     *        nmxhrs * (nsourc + 1) * ntotal / 2 + 999) / 1000
          i = KERRS(48,mognam,k,idum,xdum,xdum,xdum,xdum,0)
        end if
        if (iferrs .ne. 1) iferrs = 2
        ifnext = 3
  300   continue
      end if
      return
      end
