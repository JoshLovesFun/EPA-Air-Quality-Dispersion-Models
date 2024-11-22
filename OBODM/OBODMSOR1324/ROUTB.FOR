      Subroutine ROUTB
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - ROUTB Version 01.1                          ---
CC--- Subroutine description - controls printing of dispersion      ---
CC---                          model calculations for summary mode. ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      character*190 mgrpb
CC
      dimension xrcbuf(128),yrcbuf(128),ircbf(265),vmx50(50,4),
     *          imxdt(50,4),vmxxx(50,4),vmxyy(50,4),nmx50(4),
     *          xmx50(804),vmxhi(20000),jmxdt(20000)                    110198
CC
      include 'OCNTRL.INC'
      include 'OCALCS.INC'
      include 'OCDSPM.INC'
      include 'OWNTRL.INC'
      include 'ONAMES.INC'
CC
      equivalence (xrcbuf(1),ircbuf(1)),(yrcbuf(1),iptbuf(1)),
     *            (xmx50(1),calcsa(1)),(vmx50(1,1),xmx50(1)),
     *            (imxdt(1,1),xmx50(201)),(vmxxx(1,1),xmx50(401)),
     *            (vmxyy(1,1),xmx50(601)),(nmx50(1),xmx50(801)),
     *            (vmxhi(1),calcsa(1)),(jmxdt(1),calcsa(1)),            110198
     *            (mscrnm(72,1),mgrpb),(xbxpel(1),ircbf(1))             110198
CC
CC*******************************************************************
CC
CC-DEBUGS
      if (ifdbug .gt. 0.and.ifdbug .lt. 7) then                         020797
        WRITE (iotfil,9000) iotfil,iogfil,ntothr
      end if
CC-DEBUGE
CC
CC      change grid points to correct output units.
      call CHXYZ(xdumx)
CC-DEBUGS
      if (ifdbug .gt. 3.and.ifdbug .lt. 7) then                         020797
        WRITE (iotfil,9001) xdumx,ntotal,iogfil,nsourc,ngroup,nvsprt,
     *                      ifhrly
      end if
CC-DEBUGE
      if (iotfil .gt. 0) then
CC        summary output mode.
CC
CC        get final number of hours.
        norec = istrec + 1
        ier = INRAN(norec,iptbuf,iogfil)
        if (ier .lt. 0) go to 180
CC        get seasonal number of hours.
        ntothr = iptbuf(1)
        i = JINIT(nseasn,4,iptbuf(2) ,1)
        i = JINIT(nquart,4,iptbuf(6),1)
CC        get record pointers from disk file
        norec = istrec + 2
        ier = INRAN(norec,iptbuf,iogfil)
        if (ier .lt. 0) go to 180
        i = JINIT(ircbf,128,iptbuf,1)
        iogpnt = ircbf(25)
        nrecs = iogpnt
        norec = istrec + 3
        ier = INRAN(norec,iptbuf,iogfil)
        if (ier .lt. 0) go to 180
        i = JINIT(ircbf(129),128,iptbuf,1)
        norec = istrec + 4
        ier = INRAN(norec,iptbuf,iogfil)
        if (ier .lt. 0) go to 180
        i = JINIT(ircbf(257),9,iptbuf,1)
CC          determine where deposition starts.
        ifdep = (jjjoff(4) + 1) * iswopt(4)
CC        number of disk records.
        nrec = (ntotal + 127) / 128
CC        check source groups.
        if (ngroup .gt. 0) then
CC          number of different source combinations to print.
          kgroup = ngroup
        else if (nsourc .gt. 1) then
CC          single combination of all sources.
          kgroup = 1
          nsogrp(1) = 1
          isogrp(1) = -nsourc
        else
          kgroup = 0
        end if
        ngrpb = 0
        ngrps = MAX0(kgroup,1)
CC
CC        if seasonal/quarterly output is desired.
        if (ifsean .gt. 0) then
          do 40 isean=1,4
            if (ifsean .eq. 1) then
              nsean = nseasn(isean)
            else
              nsean = nquart(isean)
            end if
CC            loop over source groups.
            do 30 i=1,ngrps
CC              season start record.
              iogrec = ircbf(ircbf(1+isean)+i-1)
              ier = JDSKR(ntotal,iogrec,calcsa)
              if (kgroup .gt. 0) then
                call GLABL(i,ngrpb,mgrpb,mgrpb)
              end if
CC              output dispersion calculations.
              do 20 kquan=1,4
CC                1) dosage.
CC                2) concentration.
CC                3) time-average concentration.
CC                4) gravitational deposition.
                if (iswopt(kquan) .gt. 0) then
                  ijoff = jjjoff(kquan)
                  xdumc = 1.0
CC                  print calculations.
                  if (kquan .le. 3) then
                    if (iswopt(17) .lt. 10) then
CC                      convert to output mass/volume units.
                      xdumc = constv(iswopt(17)+1) /
     *                        constm(iswopt(13)+1)
                    end if
                    if (kquan .eq. 1) then
CC                      convert dosage to output time units.
                      xdumc = xdumc / constt(iswopt(14)+1)
                    end if
                  else
CC                    convert to output mass/area units.
                    xdumc = consta(iswopt(16)+1) / constm(iswopt(13)+1)
                  end if
                  do 10 j=1,ntotpt
CC                    convert units.
                    calcsa(j+ijoff) = calcsa(j+ijoff) * xdumc * ascale
     *                                + bscale
   10             continue
CC                  generate heading labels.
                  call RLABL(ifsean+1,kquan,isean,0)                    120698
CC                  print averaged or single hour dispersion
CC                  calculations.
                  call OUTPT(ijoff,kquan,0,mgrpb,ngrpb,xdum,xdum,       110198
     *                       xdum,idum,idum,idum)                       122198
                end if
   20         continue
   30       continue
   40     continue
        end if
CC
CC        if annual output is desired.
        if (ifanul .gt. 0) then
CC          loop over source groups.
          do 70 i=1,ngrps
CC            annual start record.
            iogrec = ircbf(ircbf(6)+i-1)
            ier = JDSKR(ntotal,iogrec,calcsa)
            if (kgroup .gt. 0) then
              call GLABL(i,ngrpb,mgrpb,mgrpb)
            end if
CC            output dispersion calculations.
            do 60 kquan=1,4
CC              1) dosage.
CC              2) concentration.
CC              3) time-average concentration.
CC              4) gravitational deposition.
              if (iswopt(kquan) .gt. 0) then
                ijoff = jjjoff(kquan)
                xdumc = 1.0
CC                print calculations.
                if (kquan .le. 3) then
                  if (iswopt(17) .lt. 10) then
CC                    convert to output mass/volume units.
                    xdumc = constv(iswopt(17)+1) /
     *                      constm(iswopt(13)+1)
                  end if
                  if (kquan .eq. 1) then
CC                    convert dosage to output time units.
                    xdumc = xdumc / constt(iswopt(14)+1)
                  end if
                else
CC                  convert to output mass/area units.
                  xdumc = consta(iswopt(16)+1) / constm(iswopt(13)+1)
                end if
                do 50 j=1,ntotpt
CC                  convert units.
                  calcsa(j+ijoff) = calcsa(j+ijoff) * xdumc * ascale +
     *                              bscale
   50           continue
CC                generate heading labels.
                call RLABL(4,kquan,idum,0)                              120698
CC                print averaged or single hour dispersion
CC                calculations.
                call OUTPT(ijoff,kquan,0,mgrpb,ngrpb,xdum,xdum,         110198
     *                     xdum,idum,idum,idum)                         122198
              end if
   60       continue
   70     continue
        end if
CC
        if (iswopt(18) .gt. 0) then
          do 110 nh=1,6
            if (navhrs(nh) .gt. 0) then
CC              loop over source groups.
              do 100 i=1,ngrps
CC                maximum-50 start record.
                iogrec = ircbf(ircbf(12+nh)+i-1)
CC                number of words to read from disk.
                n = (50 * 4 * 4 + 4)
CC                read disk data.
                ier = JDSKR(n,iogrec,xmx50)
                if (ier .lt. 0) go to 180
                if (kgroup .gt. 0) then
                  call GLABL(i,ngrpb,mgrpb,mgrpb)
                end if
CC                output dispersion calculations.
                do 90 kquan=1,4
CC                  1) dosage.
CC                  2) concentration.
CC                  3) time-average concentration.
CC                  4) gravitational deposition.
                  if (iswopt(kquan) .gt. 0) then
                    ijoff = jjjoff(kquan)
                    xdumc = 1.0
CC                    print calculations.
                    if (kquan .le. 3) then
                      if (iswopt(17) .lt. 10) then
CC                        convert to output mass/volume units.
                        xdumc = constv(iswopt(17)+1) /
     *                          constm(iswopt(13)+1)
                      end if
                      if (kquan .eq. 1) then
CC                        convert dosage to output time units.
                        xdumc = xdumc / constt(iswopt(14)+1)
                      end if
                    else
CC                      convert to output mass/area units.
                      xdumc = consta(iswopt(16)+1) /
     *                        constm(iswopt(13)+1)
                    end if
                    do 80 j=1,50
CC                      convert units.
                      vmx50(j,kquan) = vmx50(j,kquan) * xdumc * ascale
     *                                 + bscale
   80               continue
CC                    generate heading labels.
                    call RLABL(1,kquan,navhrs(nh),0)                    120698
CC                    print maximum-50 calculations.
                    call OUTPT(idum,kquan,1,mgrpb,ngrpb,vmx50(1,kquan), 110198
     *                         vmxxx(1,kquan),vmxyy(1,kquan),           110198
     *                         imxdt(1,kquan),idum,nmx50(kquan))        122198
                  end if
   90           continue
  100         continue
            end if
  110     continue
        end if
CC
        if (iswopt(12) .gt. 0) then
          do 170 nh=1,6
            if (navhrs(nh) .gt. 0) then
CC              loop over source groups.
              do 160 ig=1,ngrps
                do 150 l=1,2
CC                  highest, second highest start record.
                  iogrec = ircbf(ircbf(18+nh)+ig-1) - 1
CC                  set pointer high to start reading.
                  ipntr = 128
                  j = 0
  120             if (j .lt. ntotal) then
                    j = j + 1
                    ier = INCPT(ipntr,iogrec,ircbuf,4,iogfil,mognam,0)
                    if (ier .lt. 0) go to 200
                    vmxhi(j) = xrcbuf(ipntr+l-1)
                    jmxdt(j+ntotal) = ircbuf(ipntr+l+1)
                    go to 120
                  end if
                  if (kgroup .gt. 0) then
                    call GLABL(ig,ngrpb,mgrpb,mgrpb)
                  end if
CC                  output dispersion calculations.
                  do 140 kquan=1,4
CC                    1) dosage.
CC                    2) concentration.
CC                    3) time-average concentration.
CC                    4) gravitational deposition.
                    if (iswopt(kquan) .gt. 0) then
                      ijoff = jjjoff(kquan)
                      xdumc = 1.0
CC                      print calculations.
                      if (kquan .le. 3) then
                        if (iswopt(17) .lt. 10) then
CC                          convert to output mass/volume units.
                          xdumc = constv(iswopt(17)+1) /
     *                            constm(iswopt(13)+1)
                        end if
                        if (kquan .eq. 1) then
CC                          convert dosage to output time units.
                          xdumc = xdumc / constt(iswopt(14)+1)
                        end if
                      else
CC                        convert to output mass/area units.
                        xdumc = consta(iswopt(16)+1) /
     *                          constm(iswopt(13)+1)
                      end if
                      do 130 j=1,ntotpt
CC                        convert units.
                        vmxhi(j+ijoff) = vmxhi(j+ijoff) * xdumc * ascale
     *                                   + bscale
  130                 continue
CC                      generate heading labels.
                      call RLABL(l+4,kquan,navhrs(nh),0)                120698
CC                      print highest, second highest.
                      ikoff = ntotal + ijoff                            122198
                      call OUTPT(ijoff,kquan,l+1,mgrpb,ngrpb,xdumy,     122198
     *                           xdumy,xdumy,idumy,ikoff,idumy)         122198
                    end if
  140             continue
  150           continue
  160         continue
            end if
  170     continue
        end if
CC
        ifpsol = ifpsol + 1
      end if
      go to 210
  180 if (KERRS(1,mognam,IABS(ier),norec,xdumy,xdumy,xdumy,xdumy,0)
     *    .eq. 1) go to 220
      go to 200
  190 if (ier .eq. 6422.or.ier .eq. 913.or.ier .eq. 956) then
        i = (4 * (iogpnt * 128 + istrec + 4) + 999) / 1000
        i = KERRS(48,mognam,i,idum,xdum,xdum,xdum,xdum,0)
      end if
  200 ifnext = 3
      ifpsol = ifpsol + 5
  210 call GETID(ier)
      if (ifrdwt(5) .eq. 0) ifrdwt(5) = 1
      if (ifpsol .ge. 5.and.ifrdwt(5) .ge. 0) ifrdwt(5) = 2
  220 return
CC
CC-DEBUGS
 9000 format (/' *-*-* entered ROUTB - iotfil,iogfil,ntothr=',3i8)
 9001 format (' ROUTB - xdumx,ntotal,iogfil,nsourc=',g12.6,3i6/
     *9x,'ngroup,nvsprt,ifhrly=',3i5)
CC-DEBUGE
      end
