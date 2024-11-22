      Subroutine ROUTA
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - ROUTA Version 01.1                          ---
CC--- Subroutine description - controls printing of dispersion      ---
CC---                          model calculations under detail mode ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      integer*2 icol,irow,ibak,ifor,icoff
      character*1 mspac
      character*4 mbufr
      character*6 mbuf6
      character*22 msch4
      character*31 mesg1
      character*56 mhrsf
      character*190 mgrpb
      logical*1 iflg1
CC
      dimension vmx50(50,4),imxdt(50,4),vmxxx(50,4),vmxyy(50,4),        110198
     *          nmx50(4),mspac(2000),vmxhi(20000),jmxdt(20000),         110198
     *          vxbuf(128),ixbuf(128),ifnxh(4)
CC
      include 'OCNTRL.INC'
      include 'ONAMES.INC'
      include 'OCDSPM.INC'
      include 'OCALCS.INC'
      include 'OWNTRL.INC'
CC
      equivalence (jatzr,catzr),(mspac(1),mscrnm(1,1)),                 110198
     *            (mgrpb,mscrnm(72,1)),(mhrsf,mhrlab(1)),               110198
     *            (mbufr,mspac(317)),(vmxhi(1),calcsa(1)),              110198
     *            (jmxdt(1),calcsa(1)),(vxbuf(1),ixbuf(1))              110198
CC
      data msch4/'SCRATCH4.OBD'/,icoff/3/,                              110198
     *     mesg1/'@OBODM processing hour        ~'/
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
        ifm = 0
        imxfil = 0
        ifprt = 1
        jhrin = 1
CC
CC        generate output tables.
        if (ngroup .gt. 0) then
CC          number of different source combinations to print.
          kgroup = ngroup
        else
CC          single combination of all sources.
          kgroup = 1
          nsogrp(1) = 1
          isogrp(1) = -nsourc
        end if
        if (iswopt(12) .gt. 0) then
CC          set offset indices for 1st and 2nd highest tables.
          if (5*ntotal .le. nmxcor) then
            iflg1 = .false.
            ifnxa = ntotal
            ifnxb = ifnxa + ntotal
            ifnxc = ifnxb + ntotal
            ifnxd = ifnxc + ntotal
          else
            iflg1 = .true.
            imxfil = 18
            OPEN (unit=imxfil,iostat=ier,err=260,status='SCRATCH',
     *            access='direct',form='unformatted',recl=512)
            ifnxa = ntotal
            ifnxb = ifnxa
            ifnxc = ifnxb
            ifnxd = ifnxc
          end if
        end if
        lg = 0
        ngrpb = 0
        nhrlab = 0
        indxs = 0
CC
CC        loop over source groups.
        ng = 0
   10   ng = ng + 1
        if (ng .le. kgroup) then
          if (ng .gt. 1) lg = lg + nsogrp(ng-1)
          call GLABL(ng,ngrpb,mgrpb,mgrpb)
CC
CC          loop over n-hourly, seasonal or quarterly, annual averages.
          ihrav = 0
          iseasn = 0
          iquart = 0
CC          pointer to type of average. npont = 1 are all short term
CC          averages, npont = 2 is seasonal or quarterly, npont = 3 is
CC          annual.
          npont = 0
   20     npont = npont + 1
   30     if (ifhrly .gt. 0) then
            if (npont .eq. 1) then
              if (iswopt(12) .eq. 0.and.iswopt(18) .eq. 0.and.
     *            iswopt(28) .eq. 0) go to 20
              ihrav = ihrav + 1
              if (ihrav .le. 6) then
CC                n-hour average or total.
                if (navhrs(ihrav) .le. 0) go to 20
                indxs = 1
                nhrrs = navhrs(ihrav)
              else
                go to 20
              end if
              ifprt = iswopt(28)
CC              n-hour.
              nnavg = navhrs(ihrav)
            else if (npont .eq. 2) then
CC              seasonal or quarterly.
              if (ifsean .le. 0) go to 20
              iseasn = iseasn + 1
              iquart = iquart + 1
              if (iseasn .gt. 4.or.iquart .gt. 4) go to 20
              indxs = ifsean + 1
              ifprt = 1
CC              seasonal or quarterly.
              if (ifsean .eq. 1) then
                nnavg = nseasn(iseasn)
              else
                nnavg = nquart(iquart)
              end if
            else if (npont .eq. 3) then
CC              annual.
              if (ifanul .le. 0) go to 20
              indxs = 4
              ifprt = 1
CC              annual
              nnavg = ntothr
            else
              go to 10
            end if
          end if
CC
CC          zero out accumulation arrays.
          i = JRNIT(calcsa,nmxcor,0.0,0)
          if (ifhrly .gt. 0) then
            if (iswopt(18) .gt. 0.and.npont .eq. 1) then
              i = JRNIT(vmx50,200,0.0,0)
              i = JRNIT(vmxxx,200,0.0,0)
              i = JRNIT(vmxyy,200,0.0,0)
              i = JINIT(imxdt,200,0,0)
              i = JINIT(nmx50,4,0,0)
            end if
            if (iflg1.and.iswopt(12) .gt. 0.and.npont .eq. 1) then
              i = JINIT(ixbuf,128,0,0)
              nrec = (4 * ntotal + 127) / 128
              kq = 0
              do 50 kquan=1,4
                if (iswopt(kquan) .gt. 0) then
                  kq = kq + 1
                  iorec = (kq - 1) * nrec
                  j = 0
   40             if (j .lt. nrec) then
                    j = j + 1
                    iorec = iorec + 1
                    ier = IORAN(iorec,ixbuf,imxfil)
                    if (ier .gt. 0) go to 270
                    go to 40
                  end if
                end if
   50         continue
            end if
          end if
CC
CC          initialize total hours counter.
          ihrin = 0
CC          initialize average hours counter.
          if (ifhrly .ne. 0) jhrin = 0
CC          initialize missing hours counter.
          jhrms = 0
CC          initialize not calculated hours counter.
          jhrex = 0
CC          loop over hours
   60     ihrin = ihrin + 1
          if (ifhrly .gt. 0) then
            if (ifctlc .eq. 2) then
              ifctlc = 3
              if (JFCTC(52) .ne. 0) go to 300
            end if
            if (ihrin .gt. ntothr) then
              if (npont .eq. 1.or.npont .eq. 2) go to 30
              go to 20
            end if
CC            display current hour.
            ibak = ibackt
            ifor = iforet
            icol = 18
            irow = 2
            xdumy = ihrin
            i = IWTBF(mbuf6,6,xdumy,0,1)
CCPCITS
            call CRTYP(icoff)
CCPCITE
            if (ifm .eq. 0) then
              mesg1(24:29) = mbuf6
              call IWRSC(icol,irow,31,mesg1,ibak,ifor)
              ifm = 1
            else
              icol = icol + 23
              ibak = ibackd
              ifor = ifored
              call IWRSC(icol,irow,6,mbuf6,ibak,ifor)
            end if
          end if
CC
CC
CC          loop over sources within source group ng.
          jg = 0
          ifcalc = 1
   70     jg = jg + 1
          if (jg .le. nsogrp(ng)) then
            if (isogrp(lg+jg) .gt. 0) then
              n1 = isogrp(lg+jg)
              n2 = n1
            else
              i = isogrp(lg+jg)
              j = 0
              if (jg .gt. 1) j = isogrp(lg+jg-1)
              if (jg .eq. 1.or.j .lt. 0.or.j .gt. IABS(i)) then
                n1 = 1
              else
                n1 = j + 1
              end if
              n2 = IABS(i)
            end if
CC            loop over sources.
            i = (ihrin - 1) * (2 * nsourc + 1) + 9 + 1
            iptrec = (i + 127) / 128 + istrec
            iptpnt = MOD(i,128)
            if (iptpnt .eq. 0) iptpnt = 128
            norec = iptrec
            ier = INRAN(iptrec,iptbuf,iogfil)
            if (ier .lt. 0) go to 250
            call IGTDT(iptbuf(iptpnt),ifcalc,inihrs,inidys,inimon,
     *                 iniyrs,1)
            injdys = 0
            call JULAN(iniyrs,inimon,inidys,injdys)
            if (npont .eq. 2) then
              if (ifsean .eq. 1) then
                isean = ISEAS(inimon,1)
              else
                isean = ISEAS(inimon,2)
              end if
              if (isean .ne. iseasn) then
                if (ihrin + 1 .gt. ntothr) then
                  ihrin = ihrin + 1
                  go to 100
                end if
                go to 60
              end if
            end if
            do 90 kssndx=1,nsourc
              ier = INCPT(iptpnt,iptrec,iptbuf,1,iogfil,mognam,0)
              if (ier .lt. 0) go to 280
              iogrec = iptbuf(iptpnt)
              ier = INCPT(iptpnt,iptrec,iptbuf,1,iogfil,mognam,0)
              if (ier .lt. 0) go to 280
              if (n1 .le. kssndx.and.kssndx .le. n2) then
                nwrds = iptbuf(iptpnt) / 1000
                iogpnt = iptbuf(iptpnt) - nwrds * 1000 - 1
CC-DEBUGS
                if (ifdbug .gt. 3.and.ifdbug .lt. 7) then               020797
                  WRITE (iotfil,9005) kssndx,iogfil,jg,istrec
                end if
CC-DEBUGE
CC
                if (iogrec .gt. 0.and.nwrds .gt. 0) then
CC                  read mass storage records.
                  norec = iogrec
                  ier = INRAN(iogrec,ircbuf,iogfil)
                  if (ier .lt. 0) go to 250
                  n = 0
                  l = 0
   80             if (n .lt. nwrds) then
                    n = n + 1
                    ier = INCPT(iogpnt,iogrec,ircbuf,1,iogfil,mognam,0)
                    if (ier .lt. 0) go to 280
                    jatzr = ircbuf(iogpnt)
                    if (jatzr .lt. 0) then                              110198
                      l = l + IABS(jatzr)                               110198
                    else
                      l = l + 1
                      calcsa(l) = calcsa(l) + catzr
                    end if
                    go to 80
                  end if
                end if
              end if
   90       continue
            go to 70
          end if
CC
CC          see if end of averaging period.
  100     if (ifhrly .gt. 0) then
            if (ihrin .le. ntothr) then
              if (ifcalc .ne. 0) then                                   110198
CC                increment hours in average.                           110198
                jhrin = jhrin + 1                                       110198
                if (ifcalc .lt. 0) then                                 110198
CC                  increment user excluded hours                       110198
                  jhrex = jhrex + 1                                     110198
                end if                                                  110198
              else                                                      110198
CC                increment missing hours (calm, bad data, missing hour)110198
                jhrin = jhrin + 1                                       110198
                jhrms = jhrms + 1                                       110198
              end if                                                    110198
            end if
CC            if not enough, go get more.
            if (jhrin .lt. nnavg.and.ihrin .lt. ntothr) then
CC              if first hour of average, save date/time, etc.
              if (jhrin .eq. 1) then 
CC                First hour of period                                  110198
                istyr = iniyrs
                istmo = inimon
                istdy = inidys
                isthr = inihrs
                istse = isean
                istjd = injdys
              end if
CC              Save current hour in last hour                          110198
              ienyr = iniyrs
              ienmo = inimon
              iendy = inidys
              ienhr = inihrs
              iense = isean
              ienjd = injdys
              go to 60
            end if
CC            end of averaging period, save last date/time, etc.
            if (ihrin .le. ntothr) then                                 110198
              ienyr = iniyrs
              ienmo = inimon
              iendy = inidys
              ienhr = inihrs
              iense = isean
              ienjd = injdys
              if (nnavg .le. 1) then
                istyr = ienyr
                istmo = ienmo
                istdy = iendy
                istjd = ienjd
                istse = iense
                isthr = ienhr
              end if
            end if
            if (npont .eq. 1) then
              if (jhrin .eq. 1) then                                    110198
CC                Make first hours label                                110198
                if (ihrfmt .ne. 4) then
                  WRITE (mhrsf,9002) istmo,istdy,istyr,istjd,isthr
                  nhrlab = JPACK(mhrlab,29)
                else
                  WRITE (mhrsf,9003) isthr
                  nhrlab = JPACK(mhrlab,15)
                end if
              else
CC                Interval hours label                                  110198
                WRITE (mhrsf,9004) istmo,istdy,istyr,istjd,isthr,
     *                             ienmo,iendy,ienyr,ienjd,ienhr
                nhrlab = JPACK(mhrlab,56)
              end if
            else
              nhrlab = 0
            end if
CC            if greater than hourly.
            if (jhrin-jhrms .gt. 1.0.and.ISUMI(iswopt,3) .gt. 0) then 
CC              determine averages.
              xdumy = jhrin - jhrms
              if (jhrms .gt. 0.and.npont .eq. 1.and.iswopt(29) .eq. 0)
     *            then
CC                EPA 75 percent rule.                                  110198
                xdumy = FLOAT(nnavg) * 0.75 + 0.4
                i = xdumy
CC                Use maximum of actual count and 75 percent            110198
                xdumy = MAX0(jhrin-jhrms,i)
              end if
              if (xdumy .gt. 0.0) then
                xdumy = 1.0 / xdumy
              else
                xdumy = 0.0
              end if
              if (iswopt(1) .gt. 0) then
CC                average dosage.
                do 110 i=1,ntotpt
                  calcsa(i+jjjoff(1)) = calcsa(i+jjjoff(1)) * xdumy
  110           continue
              end if
              if (iswopt(2) .gt. 0) then
CC                average concentration.
                do 120 i=1,ntotpt
                  calcsa(i+jjjoff(2)) = calcsa(i+jjjoff(2)) * xdumy
  120           continue
              end if
              if (iswopt(3) .gt. 0) then
CC                average time-average concentration.
                do 130 i=1,ntotpt
                  calcsa(i+jjjoff(3)) = calcsa(i+jjjoff(3)) * xdumy
  130           continue
              end if
            end if
          end if
CC          If hourly table not calculated, omit print.                 111098
          if (ifhrly .gt. 0.and.npont .eq. 1.and.ihrin .lt. ntothr) then111098
CC            n-hour table user excluded.                               111098
            if (jhrex .ge. jhrin) go to 240                             111098
          end if                                                        111098
CC          If no calculated data, omit                                 111098
          if (jhrin .le. 0.and.ihrin .lt. ntothr) go to 240             111098
CC
CC          output dispersion calculations.
          kq = 0
          do 230 kquan=1,4
CC            1) dosage.
CC            2) concentration.
CC            3) time-average concentration.
CC            4) gravitational deposition.
            if (iswopt(kquan) .gt. 0) then
              kq = kq + 1
              ijoff = jjjoff(kquan)                                     110198
              ifnxh(1) = ifnxa + ijoff                                  110198
              ifnxh(2) = ifnxb + ijoff                                  110198
              ifnxh(3) = ifnxc + ijoff                                  110198
              ifnxh(4) = ifnxd + ijoff                                  110198
              xdumc = 1.0
CC              print calculations.
              if (kquan .le. 3) then
                if (iswopt(17) .lt. 10) then
CC                  convert to output mass/volume units.
                  xdumc = constv(iswopt(17)+1) / constm(iswopt(13)+1)
                end if
                if (kquan .eq. 1) then
CC                  convert dosage to output time units.
                  xdumc = xdumc / constt(iswopt(14)+1)
                end if
              else
CC                convert to output mass/area units.
                xdumc = consta(iswopt(16)+1) / constm(iswopt(13)+1)
              end if
              do 140 i=1,ntotpt
CC                convert units.
                calcsa(i+ijoff) = calcsa(i+ijoff) * xdumc * ascale +
     *                            bscale
  140         continue
CC              generate heading labels.
              if (npont .ne. 2) then
                call RLABL(indxs,kquan,nnavg,0)                         120698
              else
                call RLABL(indxs,kquan,iseasn,0)                        120698
              end if
              if (ifprt .gt. 0.and.jhrin .gt. 0) then
CC                print averaged or single hour dispersion calculations.
                call OUTPT(ijoff,kquan,0,mgrpb,ngrpb,xdumy,xdumy,xdumy, 110198
     *                     idumy,idumy,idumy)                           122198
              end if
              if (ifhrly .gt. 0.and.iswopt(18) .gt. 0.and.npont .eq. 1
     *            .and.jhrin .gt. 0) then
CC                accumulate maximum 50 receptors.
                do 180 i=1,ntotpt
                  if (nmx50(kquan) .gt. 0) then
                    do 160 j=1,nmx50(kquan)
                      if (calcsa(i+ijoff) .gt. vmx50(j,kquan)) then
                        nmx50(kquan) = MIN0(nmx50(kquan) + 1,50)
                        do 150 k=j,nmx50(kquan)-1
                          l = nmx50(kquan) - (k - j)
                          vmx50(l,kquan) = vmx50(l-1,kquan)
                          vmxxx(l,kquan) = vmxxx(l-1,kquan)
                          vmxyy(l,kquan) = vmxyy(l-1,kquan)
                          imxdt(l,kquan) = imxdt(l-1,kquan)
  150                   continue
                        go to 170
                      end if
  160               continue
                  end if
                  if (nmx50(kquan) .ge. 50) go to 180
                  nmx50(kquan) = nmx50(kquan) + 1
                  j = nmx50(kquan)
  170             vmx50(j,kquan) = calcsa(i+ijoff)
                  if (i .gt. ngrdpt) then
                    vmxxx(j,kquan) = xdscrt(i-ngrdpt)
                    vmxyy(j,kquan) = ydscrt(i-ngrdpt)
                  else
                    jp = (i + nxpnts -1) / nxpnts
                    ip = i - (jp - 1) * nxpnts
                    vmxxx(j,kquan) = xcoord(ip)
                    vmxyy(j,kquan) = ycoord(jp)
                  end if
CC                  save date/time in compressed form.
                  idumy = 1                                             110198
                  imxdt(j,kquan) = ISVDT(idumy,ienhr,iendy,ienmo,ienyr)
  180           continue
              end if
              if (ifhrly .gt. 0.and.iswopt(12) .gt. 0.and.npont .eq. 1
     *            .and.jhrin .gt. 0) then
CC                first and second highest at each receptor.
                if (.not.iflg1) then
                  do 190 i=1,ntotpt
                    if (calcsa(i+ijoff) .gt. vmxhi(i+ifnxh(1))) then
                      if (vmxhi(i+ifnxh(1)) .gt. vmxhi(i+ifnxh(2))) then
                        vmxhi(i+ifnxh(2)) = vmxhi(i+ifnxh(1))
                        jmxdt(i+ifnxh(4)) = jmxdt(i+ifnxh(3))
                      end if
                      vmxhi(i+ifnxh(1)) = calcsa(i+ijoff)
                      idumy = 1                                         110198
                      jmxdt(i+ifnxh(3)) = ISVDT(idumy,ienhr,iendy,ienmo,
     *                                          ienyr)
                    else if (calcsa(i+ijoff) .gt. vmxhi(i+ifnxh(2)))
     *                       then
                      vmxhi(i+ifnxh(2)) = calcsa(i+ijoff)
                      idumy = 1                                         110198
                      jmxdt(i+ifnxh(4)) = ISVDT(idumy,ienhr,iendy,ienmo,
     *                                          ienyr)
                    end if
  190             continue
                else
                  ipntr = 128
                  iorec = (kq - 1) * nrec
                  ifwrt = 0
                  i = 0
  200             if (i .lt. ntotpt) then
                    i = i + 1
                    ier = INCPT(ipntr,iorec,ixbuf,4,imxfil,msch4,ifwrt)
                    if (ier .lt. 0) go to 280
                    if (ier .gt. 0) go to 270
                    ifwrt = 1
                    if (calcsa(i+ijoff) .gt. vxbuf(ipntr)) then
                      if (vxbuf(ipntr) .gt. vxbuf(ipntr+1)) then
                        vxbuf(ipntr+1) = vxbuf(ipntr)
                        ixbuf(ipntr+3) = ixbuf(ipntr+2)
                      end if
                      vxbuf(ipntr) = calcsa(i+ijoff)
                      idumy = 1                                         110198
                      ixbuf(ipntr+2) = ISVDT(idumy,ienhr,iendy,ienmo,
     *                                       ienyr)
                    else if (calcsa(i+ijoff) .gt. vxbuf(ipntr+1)) then
                      vxbuf(ipntr+1) = calcsa(i+ijoff)
                      idumy = 1                                         110198
                      ixbuf(ipntr+3) = ISVDT(idumy,ienhr,iendy,ienmo,
     *                                       ienyr)
                    end if
                    go to 200
                  end if
                  ier = IORAN(iorec,ixbuf,imxfil)
                  if (ier .gt. 0) go to 270
                end if
              end if
CC              see if end of averages.
              if (ifhrly .gt. 0.and.ihrin .ge. ntothr) then
                if (iswopt(18) .gt. 0.and.npont .eq. 1) then
CC                  print maximum 50 tables.
                  call OUTPT(ijoff,kquan,1,mgrpb,ngrpb,vmx50(1,kquan),  110198
     *                       vmxxx(1,kquan),vmxyy(1,kquan),             110198
     *                       imxdt(1,kquan),idumy,nmx50(kquan))         122198
                  i = JRNIT(vmx50(1,kquan),50,0.0,0)
                  nmx50(kquan) = 0
                end if
                if (iswopt(12) .gt. 0.and.npont .eq. 1) then
CC                  print 1st and 2nd highest tables.
                  do 220 l=1,2
                    if (.not.iflg1) then
                      ikoff = ifnxh(l+2)                                122198
                      call OUTPT(ifnxh(l),kquan,l+1,mgrpb,ngrpb,xdumy,  122198
     *                           xdumy,xdumy,idumy,ikoff,idumy)         122198
                      i = JRNIT(vmxhi(ifnxh(l)+1),ntotpt,0.0,0)
                      i = JINIT(jmxdt(ifnxh(l+2)+1),ntotpt,0,0)
                    else
                      iorec = (kq - 1) * nrec
                      ipntr = 128
                      i = 0
  210                 if (i .lt. ntotpt) then
                        i = i + 1
                        ier = INCPT(ipntr,iorec,ixbuf,4,imxfil,msch4,0)
                        if (ier .lt. 0) go to 280
                        vmxhi(i) = vxbuf(ipntr+l-1)
                        jmxdt(i+ntotpt) = ixbuf(ipntr+l+1)
                        go to 210
                      end if
                      call OUTPT(0,kquan,l+1,mgrpb,ngrpb,xdumy,         011403
     *                           xdumy,xdumy,idumy,ntotpt,              011403
     *                           idumy)                                 011403

                    end if
  220             continue
                end if
              end if
            end if
  230     continue
CC
CC
  240     if (ifhrly .gt. 0) then
CC            reinitialize for next averaging period.
            i = JRNIT(calcsa,ntotal,0.0,0)
            jhrin = 0
            jhrms = 0
            jhrex = 0
            go to 60                                 
          else
            go to 10
          end if
        end if
        ifpsol = ifpsol + 1
      end if
      go to 290
  250 if (KERRS(1,mognam,IABS(ier),norec,xdumy,xdumy,xdumy,xdumy,0)
     *    .eq. 1) go to 300
      go to 280
  260 if (KERRS(3,msch4,ier,idumy,xdumy,xdumy,xdumy,xdumy,0) .eq. 1)
     *    go to 300
      go to 280
  270 if (ier .eq. 6422.or.ier .eq. 913.or.ier .eq. 956) then
        i = ((nmxhrs * (2 * nsourc + 1) + ntotal * (nsourc + 1) *
     *      nmxhrs / 2 + istrec * 128 + 9 - (norec - 1) * 128) *
     *      4 + 999) / 1000
        i = KERRS(48,mognam,i,idum,xdum,xdum,xdum,xdum,0)
      end if
  280 ifnext = 3
      ifpsol = ifpsol + 5
  290 call GETID(ier)
  300 if (imxfil .gt. 0) CLOSE (unit=imxfil,status='delete',err=310)
  310 if (ifrdwt(5) .eq. 0) ifrdwt(5) = 1
      if (ifpsol .ge. 5.and.ifrdwt(5) .ge. 0) ifrdwt(5) = 2
      return
CC
CC-DEBUGS
 9000 format (/' *-*-* entered ROUTA - iotfil,iogfil,ntothr=',3i8)
 9001 format (' ROUTA - xdumx,ntotal,iogfil,nsourc=',g12.6,3i6/
     *9x,'ngroup,nvsprt,ifhrly=',3i5)
CC-DEBUGE
 9002 format ('(for >',i2,'/>',i2,'/',i4,' (',i3.3,') ',i4.4,')')
 9003 format ('(for hour ',i4.4,')')
 9004 format ('(for >',i2,'/>',i2,'/',i4,' (',i3.3,') ',i4.4,' to >',
     *i2,'/>',i2,'/',i4,' (',i3.3,') ',i4.4,')')
CC-DEBUGS
 9005 format (' ROUTA 120 - kssndx,iogfil,jg,istrec=',4i6)
CC-DEBUGE
      end
