      Subroutine UNITS(mbufr,nchrs,iunit,junit,ifpol,iflgs,jflgs)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - UNITS Version 01.0                          ---
CC--- Subroutine description - converts units index to abbreviation ---
CC---                          or abbreviation to units index.      ---
CC---------------------------------------------------------------------
CC
CC    mbufr - units abbreviation character string.
CC    nchrs - number of characters in string mbufr.
CC    iunit - numerator units index.
CC             -1 - error return, if iflgs = 0 and abbreviation in error.
CC            >=0 - index number, depends on jflgs, see below.
CC    junit - denominator units index.
CC             -1 - error return, if iflgs = 0 and abbreviation in error.
CC                  or if iunit =9 for velocity in knots.
CC            >=0 - index number.
CC    ifpol - rectangular/polar flag for length units only.
CC            0 - rectangular.
CC            1 - polar.
CC    iflgs  - abbreviation/index flag.
CC            0 - convert abbreviation in mbufr to index returned in
CC                iunit.
CC            1 - convert index iunit to abbreviation returned in
CC                mbufr.
CC    jflgs  - type of units flag.
CC             0 - length
CC                 iunit - length units index.
CC                         0 - meters        m.
CC                         1 - kilometers   km.
CC                         2 - feet         ft.
CC                         3 - yards        yd.
CC                         4 - miles        mi.
CC                         5 - centimeters  cm.
CC                         6 - millimeters  mm.
CC                         7 - micrometers  um.
CC                         8 - inches       in.
CC             1 - direction
CC                 iunit - direction units index.  also, turbulence
CC                         units.
CC                         0 - degrees       d.
CC                         1 - radians       r.
CC             2 - heat
CC                 iunit - heat units index.
CC                         0 - calorie                ca.
CC                         1 - kilocalorie            kc.
CC                         2 - British thermal units  btu.
CC                         3 - Joule                   j.
CC                         4 - erg                     e.
CC             3 - mass
CC                 iunit - mass units index.
CC                         0 - grams         g.
CC                         1 - kilograms    kg.
CC                         2 - pounds       lb.
CC                         3 - ounces       oz.
CC                         4 - tons         tn.
CC                         5 - milligrams   mg.
CC                         6 - micrograms   ug.
CC                         7 - particles    pa.
CC             4 - pressure
CC                 iunit - pressure units index.
CC                         0 - millibars          mb.
CC                         1 - pascals             p.
CC                         2 - inches hg.       inhg.
CC                         3 - millimeters hg.  mmhg.
CC             5 - time
CC                 iunit - time units index.
CC                         0 - seconds       s.
CC                         1 - minutes      mn.
CC                         2 - hours         h.
CC             6 - temperature
CC                 iunit - temperature units index.
CC                         0 - Celsius       c.
CC                         1 - Kelvin        k.
CC                         2 - Fahrenheit    f.
CC                         3 - Rankine      ra.
CC             7 - velocity
CC                 iunit - 0 to 8 - length units.
CC                              9 - knots   kts.
CC                 junit - 0 to 2 - time units.
CC             8 - ratio
CC                 iunit - ratio units index.
CC                         0 - percent                pr.
CC                         1 - fraction               fr.
CC             9 - area
CC                 iunit - length units index.
CC                         0 to 8 - (implied square) length units.
CC                              9 - acres               ac.
CC                             10 - hectares            he.
CC            10 - volume
CC                 iunit - 0 to 8 - (implied cubic) length units.
CC                              9 - liters             li.
CC                             10 - parts per million ppm.
CC                             11 - parts per billion ppb.
CC                             12 - parts per trillion ppt.
CC            11 - mass/volume
CC                 iunit - 0 to 7 - mass units.
CC                 junit - 0 to 9 - volume units.
CC            12 - mass/time
CC                 iunit - 0 to 7 - mass units.
CC                 junit - 0 to 2 - time units.
CC            13 - heat/mass
CC                 iunit - 0 to 4 - heat units.
CC                 junit - 0 to 7 - mass units.
CC            14 - angle/length
CC                 iunit - 0 to 1 - angle units.
CC                 junit - 0 to 8 - length units.
CC            15 - temperature/length
CC                 iunit - 0 to 3 - temperature units.
CC                 junit - 0 to 8 - length units
CC            16 - hour
CC                 iunit - 0 - standard time (std).
CC                         1 - daylight savings time (dst).
CC
CC**********************************************************************
CC
      integer*2 iposle,iposdr,iposhe,iposma,ipospr,iposti,iposte,
     *          iposra,iposar,iposvo,iposhr,numle,ntrle,numdr,ntrdr,
     *          numhe,ntrhe,numma,ntrma,numpr,ntrpr,numti,ntrti,
     *          numte,ntrte,numar,ntrar,numvo,ntrvo,numhr,ntrhr,i,j,l,
     *          n,numra,ntrra
CC
      character*1 mslsh,mbufr,mdumy
CC
      dimension mbufr(10),mdumy(10)
      dimension iposs(13,4),iposle(10,3),iposdr(2,2),iposhe(5,3),
     *          iposma(8,2),ipospr(4,4),iposti(3,2),iposte(4,2),
     *          iposra(2,2),iposar(11,2),iposvo(13,3),iposhr(2,3)
CC
      include 'ONAMES.INC'
CC
      data mslsh/'/'/,num/0/,ntr/0/,iposs/52*0.0/,
     *     numle/10/,ntrle/3/,numdr/2/,ntrdr/2/,numhe/5/,ntrhe/3/,
     *     numma/8/,ntrma/2/,numpr/4/,ntrpr/4/,numti/3/,ntrti/2/,
     *     numte/4/,ntrte/2/,numra/2/,ntrra/2/,numar/11/,ntrar/2/,
     *     numvo/13/,ntrvo/3/,numhr/2/,ntrhr/3/
CC         iposle  m, k, f, y, m, c, m, u, i, k,
CC                  , m, t, d, i, m, m, m, n, t,
CC                  ,  ,  ,  ,  ,  ,  ,  ,  , s,
      data iposle/39,37,32,51,39,29,39,47,35,37,
     *            00,39,46,30,35,39,39,39,40,46,
     *            00,00,00,00,00,00,00,00,00,45/
CC         iposdr  d, r,
CC                  ,  ,
      data iposdr/30,44,
     *            00,00/
CC         iposhe  c, k, b, J, e,
CC                 a, c, t,  ,  ,
CC                  ,  , u,  ,  ,
      data iposhe/29,37,28,10,31,
     *            27,29,46,00,00,
     *            00,00,21,00,00/
CC         iposma  g, k, l, o, t, m, u, p,
CC                  , g, b, z, n, g, g, a,
      data iposma/33,37,38,41,46,39,47,42,
     *            00,33,28,52,40,33,33,27/
CC         ipospr  m, P, i, m,
CC                 b,  , n, m,
CC                  ,  , h, h,
CC                  ,  , g, g,
      data ipospr/39,16,35,39,
     *            28,00,40,39,
     *            00,00,34,34,
     *            00,00,33,33/
CC         iposti  s, m, h,
CC                  , n,  ,
      data iposti/45,39,34,
     *            00,40,00/
CC         iposte  C, K, F, R,
CC                  ,  ,  , a,
      data iposte/03,11,06,18,
     *            00,00,00,27/
CC         iposra  p, f,
CC                 r, r,
      data iposra/42,32,
     *            44,44/
CC         iposar  m, k, f, y, m, c, m, u, i, a, h,
CC                  , m, t, 4, i, m, m, m, n, c, e,
      data iposar/39,37,32,51,39,29,39,47,35,27,34,
     *            00,39,46,30,35,39,39,39,40,29,31/
CC         iposvo  m, k, f, y, m, c, m, u, i, l, p, p, p,
CC                  , m, t, d, i, m, m, m, n, i, p, p, p,
CC                  ,  ,  ,  ,  ,  ,  ,  ,  ,  , m, b, t,
      data iposvo/39,37,32,51,39,29,39,47,35,38,42,42,42,
     *            00,39,46,30,35,39,39,39,40,35,42,42,42,
     *            00,00,00,00,00,00,00,00,00,00,39,28,46/
CC         iposhr  s, d,
CC                 t, s,
CC                 d, t,
      DATA iposhr/45,30,
     *            46,45,
     *            30,46/
CC
CC********************************************************************
CC
      n = 1
      i = JINIT(iposs,48,0,0)
      if (jflgs .eq. 0) then
CC
CC        length.
        num = numle - 1
        ntr = ntrle - 1
        do 20 j=1,ntr
          do 10 i=1,num
            iposs(i,j) = iposle(i,j)
   10     continue
   20   continue
CC
      else if (jflgs .eq. 1) then
CC
CC        angle.
        num = numdr
        ntr = ntrdr
        do 40 j=1,ntr
          do 30 i=1,num
            iposs(i,j) = iposdr(i,j)
   30     continue
   40   continue
CC
      else if (jflgs .eq. 2) then
CC
CC        heat.
        num = numhe
        ntr = ntrhe
        do 60 j=1,ntr
          do 50 i=1,num
            iposs(i,j) = iposhe(i,j)
   50     continue
   60   continue
CC
      else if (jflgs .eq. 3) then
CC
CC        mass.
        num = numma
        ntr = ntrma
        do 80 j=1,ntr
          do 70 i=1,num
            iposs(i,j) = iposma(i,j)
   70     continue
   80   continue
CC
      else if (jflgs .eq. 4) then
CC
CC        pressure.
        num = numpr
        ntr = ntrpr
        do 100 j=1,ntr
          do 90 i=1,num
            iposs(i,j) = ipospr(i,j)
   90     continue
  100   continue
CC
      else if (jflgs .eq. 5) then
CC
CC        time.
        num = numti
        ntr = ntrti
        do 120 j=1,ntr
          do 110 i=1,num
            iposs(i,j) = iposti(i,j)
  110     continue
  120   continue
CC
      else if (jflgs .eq. 6) then
CC
CC        temperature.
        num = numte
        ntr = ntrte
        do 140 j=1,ntr
          do 130 i=1,num
            iposs(i,j) = iposte(i,j)
  130     continue
  140   continue
CC
      else if (jflgs .eq. 7) then
CC
CC        velocity.
        num = numti
        ntr = ntrti
        do 160 j=1,ntr
          do 150 i=1,num
            iposs(i,j) = iposti(i,j)
  150     continue
  160   continue
        if (iflgs .eq. 0) then
CC          determine if abbrev. contains slash.
          l = IFCMP(mbufr,nchrs,mslsh,1,0)
          if (l .gt. 0) then
CC            slash present, determine time units index from abbrev.
            call LUNIT(mbufr(l+1),nchrs-l,junit,0,iflgs,num,ntr,iposs)
            if (junit .gt. 2) junit = -1
            n = 2
          else
CC            slash not found.
            junit = -1
          end if
        else
          if (junit .ge. 0) then
            call LUNIT(mdumy,10,junit,0,iflgs,num,ntr,iposs)
            n = 2
          end if
        end if
        num = numle
        ntr = ntrle
        do 180 j=1,ntr
          do 170 i=1,num
            iposs(i,j) = iposle(i,j)
  170     continue
  180   continue
CC
      else if (jflgs .eq. 8) then
CC
CC        ratio.
        num = numra
        ntr = ntrra
        do 200 j=1,ntr
          do 190 i=1,num
            iposs(i,j) = iposra(i,j)
  190     continue
  200   continue
CC
      else if (jflgs .eq. 9) then
CC
CC        area.
        num = numar
        ntr = ntrar
        do 220 j=1,ntr
          do 210 i=1,num
            iposs(i,j) = iposar(i,j)
  210     continue
  220   continue
CC
      else if (jflgs .eq. 10) then
CC
CC        volume.
        num = numvo
        ntr = ntrvo
        do 240 j=1,ntr
          do 230 i=1,num
            iposs(i,j) = iposvo(i,j)
  230     continue
  240   continue
CC
      else if (jflgs .eq. 11) then
CC
CC        mass/volume.
        num = numvo
        ntr = ntrvo
        do 260 j=1,ntr
          do 250 i=1,num
            iposs(i,j) = iposvo(i,j)
  250     continue
  260   continue
        if (iflgs .eq. 0) then
CC          determine if abbrev. contains slash.
          l = IFCMP(mbufr,nchrs,mslsh,1,0)
          if (l .gt. 0) then
CC            slash present, determine time units index from abbrev.
            call LUNIT(mbufr(l+1),nchrs-l,junit,0,iflgs,num,ntr,iposs)
            if (junit .gt. 11) junit = -1
            n = 2
          else
CC            slash not found.
            junit = -1
          end if
        else
          if (junit .ge. 0) then
            call LUNIT(mdumy,10,junit,0,iflgs,num,ntr,iposs)
            n = 2
          end if
        end if
        num = numma
        ntr = ntrma
        do 280 j=1,ntr
          do 270 i=1,num
            iposs(i,j) = iposma(i,j)
  270     continue
  280   continue
CC
      else if (jflgs .eq. 12) then
CC
CC        mass/time.
        num = numti
        ntr = ntrti
        do 300 j=1,ntr
          do 290 i=1,num
            iposs(i,j) = iposti(i,j)
  290     continue
  300   continue
        if (iflgs .eq. 0) then
CC          determine if abbrev. contains slash.
          l = IFCMP(mbufr,nchrs,mslsh,1,0)
          if (l .gt. 0) then
CC            slash present, determine time units index from abbrev.
            call LUNIT(mbufr(l+1),nchrs-l,junit,0,iflgs,num,ntr,iposs)
            if (junit .gt. 2) junit = -1
            n = 2
          else
CC            slash not found.
            junit = -1
          end if
        else
          if (junit .ge. 0) then
            call LUNIT(mdumy,10,junit,0,iflgs,num,ntr,iposs)
            n = 2
          end if
        end if
        num = numma
        ntr = ntrma
        do 320 j=1,ntr
          do 310 i=1,num
            iposs(i,j) = iposma(i,j)
  310     continue
  320   continue
CC
      else if (jflgs .eq. 13) then
CC
CC        heat/mass.
        num = numma
        ntr = ntrma
        do 340 j=1,ntr
          do 330 i=1,num
            iposs(i,j) = iposma(i,j)
  330     continue
  340   continue
        if (iflgs .eq. 0) then
CC          determine if abbrev. contains slash.
          l = IFCMP(mbufr,nchrs,mslsh,1,0)
          if (l .gt. 0) then
CC            slash present, determine time units index from abbrev.
            call LUNIT(mbufr(l+1),nchrs-l,junit,0,iflgs,num,ntr,iposs)
            if (junit .gt. 7) junit = -1
            n = 2
          else
CC            slash not found.
            junit = -1
          end if
        else
          if (junit .ge. 0) then
            call LUNIT(mdumy,10,junit,0,iflgs,num,ntr,iposs)
            n = 2
          end if
        end if
        num = numhe
        ntr = ntrhe
        do 360 j=1,ntr
          do 350 i=1,num
            iposs(i,j) = iposhe(i,j)
  350     continue
  360   continue
CC
      else if (jflgs .eq. 14) then
CC
CC        angle/length.
        num = numle - 1
        ntr = ntrle - 1
        do 380 j=1,ntr
          do 370 i=1,num
            iposs(i,j) = iposle(i,j)
  370     continue
  380   continue
        if (iflgs .eq. 0) then
CC          determine if abbrev. contains slash.
          l = IFCMP(mbufr,nchrs,mslsh,1,0)
          if (l .gt. 0) then
CC            slash present, determine time units index from abbrev.
            call LUNIT(mbufr(l+1),nchrs-l,junit,0,iflgs,num,ntr,iposs)
            if (junit .gt. 8) junit = -1
            n = 2
          else
CC            slash not found.
            junit = -1
          end if
        else
          if (junit .ge. 0) then
            call LUNIT(mdumy,10,junit,0,iflgs,num,ntr,iposs)
            n = 2
          end if
        end if
        num = numdr
        ntr = ntrdr
        do 400 j=1,ntr
          do 390 i=1,num
            iposs(i,j) = iposdr(i,j)
  390     continue
  400   continue
CC
      else if (jflgs .eq. 15) then
CC
CC        temperature/length.
        num = numle - 1
        ntr = ntrle - 1
        do 420 j=1,ntr
          do 410 i=1,num
            iposs(i,j) = iposle(i,j)
  410     continue
  420   continue
        if (iflgs .eq. 0) then
CC          determine if abbrev. contains slash.
          l = IFCMP(mbufr,nchrs,mslsh,1,0)
          if (l .gt. 0) then
CC            slash present, determine time units index from abbrev.
            call LUNIT(mbufr(l+1),nchrs-l,junit,0,iflgs,num,ntr,iposs)
            if (junit .gt. 8) junit = -1
            n = 2
          else
CC            slash not found.
            junit = -1
          end if
        else
          if (junit .ge. 0) then
            call LUNIT(mdumy,10,junit,0,iflgs,num,ntr,iposs)
            n = 2
          end if
        end if
        num = numte
        ntr = ntrte
        do 440 j=1,ntr
          do 430 i=1,num
            iposs(i,j) = iposte(i,j)
  430     continue
  440   continue
CC
      else if (jflgs .eq. 16) then
CC
CC        hour.
        num = numhr
        ntr = ntrhr
        do 460 j=1,ntr
          do 450 i=1,num
            iposs(i,j) = iposhr(i,j)
  450     continue
  460   continue
CC
      end if
CC      numerator units.
      if (n .ne. 2.or.iflgs .ne. 0) then
        nchr = nchrs
      else
        nchr = l - 1
      end if
      call LUNIT(mbufr,nchr,iunit,ifpol,iflgs,num,ntr,iposs)
      if (iflgs .gt. 0) then
        if (n .eq. 2) then
          i = LRJST(mbufr,nchrs,1)
          j = NUMC1(mbufr,nchrs)
          if (j .lt. nchrs) then
            mbufr(j+1) = mslsh
            i = LRJST(mdumy,10,1)
            k = NUMC1(mdumy,10)
            if (k .gt. nchrs-j-1) k = nchrs - j - 1
            call MVCH1(mdumy,mbufr(j+2),k)
            i = LRJST(mbufr,nchrs,iflrjs)
          end if
        end if
      else
        if (iunit .ge. num) iunit = -1
      end if
      return
      end
