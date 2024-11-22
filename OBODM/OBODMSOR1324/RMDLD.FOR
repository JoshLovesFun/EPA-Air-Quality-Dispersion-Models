      Subroutine RMDLD
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - RMDLD Version 01.1                          ---
CC--- Subroutine description - calculates averages for summary mode ---
CC---                          output.                              ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
CC
      dimension xrcbuf(128),yrcbuf(128),ircbf(265),vmx50(50,4),
     *          imxdt(50,4),vmxxx(50,4),vmxyy(50,4),nmx50(4),
     *          xmx50(804),jhrin(11),jhrms(11),jhrex(11)
CC
      include 'OCNTRL.INC'
      include 'OCALCS.INC'
      include 'OCDSPM.INC'
      include 'OWNTRL.INC'
      include 'ONAMES.INC'
CC
      equivalence (xrcbuf(1),ircbuf(1)),(yrcbuf(1),iptbuf(1)),
     *            (xbxpel(1),ircbf(1)),(vmx50(1,1),xmx50(1)),
     *            (imxdt(1,1),xmx50(201)),(vmxxx(1,1),xmx50(401)),
     *            (vmxyy(1,1),xmx50(601)),(nmx50(1),xmx50(801)),
     *            (jhrin(1),xbxpsl(1)),(jhrms(1),xbxpsl(12)),
     *            (jhrex(1),xbxpsl(23)),(xmx50(1),ixlpnt(1,1))
CC
CC**********************************************************************
CC
CC-DEBUGS
      if (ifdbug .gt. 0.and.ifdbug .lt. 7) then                         020797
        if (iotfil .gt. 0) WRITE (iotfil,9000)
      end if
CC-DEBUGE
CC
CC        summary output mode
      if (iogfil .gt. 0) then
        ifdep = (jjjoff(4) + 1) * iswopt(4)
CC        if not end of data.
        if (ifnext .ne. 14) then
          do 10 i=1,6
            if (ifcalc .ne. 0) then                                     110198
CC              increment hours in average.                             110198
              jhrin(i) = jhrin(i) + 1                                   110198
              if (ifcalc .lt. 0) then                                   110198
CC                user excluded hour                                    110198
                jhrex(i) = jhrex(i) + 1                                 110198
              end if                                                    110198
            else                                                        110198
CC              not calculated due to calm, bad data, etc.              110198
              jhrin(i) = jhrin(i) + 1                                   110198
              jhrms(i) = jhrms(i) + 1                                   110198
            end if                                                      110198
   10     continue
        end if
CC        number of records, except maximum-50
        nrec = (ntotal + 127) / 128
CC        check source groups.
        ngrps = MAX0(ngroup,1)
CC
CC        if seasonal/quarterly output is desired.
        if (ifsean .gt. 0) then
          if (ifnext .ne. 14) then
            if (ifsean .eq. 1) then
CC              season number.
              isean = iseasn
CC              accumulate total hours in season.
              nseasn(isean) = nseasn(isean) + 1
            else
CC              quarter number.
              isean = iquart
CC              accumulate total hours in quarter.
              nquart(isean) = nquart(isean) + 1
            end if
            if (ifcalc .ne. 0) then                                     110198
CC              increment hours in average.                             110198
              jhrin(6+isean) = jhrin(6+isean) + 1                       110198
              if (ifcalc .lt. 0) then                                   110198
CC                user excluded hour                                    110198
                jhrex(6+isean) = jhrex(6+isean) + 1                     110198
              end if                                                    110198
            else                                                        110198
CC              not calculated due to calm, bad data, etc.              110198
              jhrin(6+isean) = jhrin(6+isean) + 1                       110198
              jhrms(6+isean) = jhrms(6+isean) + 1                       110198
            end if                                                      110198
CC            if hour was calculated
            if (ifcalc .gt. 0) then
CC              loop over source groups.
              do 20 ig=1,ngrps
CC                current hour start record.
                iptrec = ircbf(ircbf(1)+ig-1)
CC                season start record.
                iogrec = ircbf(ircbf(1+isean)+ig-1)
CC                add current hour to seasonal accumulation.
                ier = JACUM(nrec,iptrec,iogrec)
                if (ier .lt. 0) go to 200
                if (ier .gt. 0) go to 210
   20         continue
            end if
          else
CC            end of data, loop over seasons or quarters
            do 40 isean=1,4
              if (ifsean .eq. 1) then
                nsean = nseasn(isean)
              else
                nsean = nquart(isean)
              end if
CC              loop over sources groups
              do 30 ig=1,ngrps
CC                season start record
                iogrec = ircbf(ircbf(1+isean)+ig-1)
                ier = JHAVG(nrec,iogrec,jhrin(6+isean),jhrms(6+isean),
     *                      nsean,ifdep,0)
                if (ier .lt. 0) go to 200
                if (ier .gt. 0) go to 210
   30         continue
   40       continue
          end if
        end if
CC
CC        if annual output is desired.
        if (ifanul .gt. 0) then
CC          if not end of data
          if (ifnext .ne. 14) then
            if (ifcalc .ne. 0) then                                     110198
CC              increment hours in average.                             110198
              jhrin(11) = jhrin(11) + 1                                 110198
              if (ifcalc .lt. 0) then                                   110198
CC                user excluded hours                                   110198
                jhrex(11) = jhrex(11) + 1                               110198
              end if                                                    110198
            else                                                        110198
CC              not calculated due to calm, bad data, etc.              110198
              jhrin(11) = jhrin(11) + 1                                 110198
              jhrms(11) = jhrms(11) + 1                                 110198
            end if                                                      110198
CC            if hour calculated
            if (ifcalc .gt. 0) then
CC              loop over source groups.
              do 50 ig=1,ngrps
CC                current hour start record.
                iptrec = ircbf(ircbf(1)+ig-1)
CC                annual start record.
                iogrec = ircbf(ircbf(6)+ig-1)
                ier = JACUM(nrec,iptrec,iogrec)
                if (ier .lt. 0) go to 200
                if (ier .gt. 0) go to 210
   50         continue
            end if
          else
CC            end of data, loop over source groups
            do 60 ig=1,ngrps
CC              annual start record
              iogrec = ircbf(ircbf(6)+ig-1)
              ier = JHAVG(nrec,iogrec,jhrin(11),jhrms(11),ntothr,ifdep,
     *                    0)
              if (ier .lt. 0) go to 200
              if (ier .gt. 0) go to 210
   60       continue
          end if
        end if
CC
        if (iswopt(12) .gt. 0.or.iswopt(18) .gt. 0) then
CC          loop over averaging times.
          nh = 0
   70     nh = nh + 1
          if (nh .le. 6) then
CC            if averaging time specified
            if (navhrs(nh) .gt. 0) then
CC              if averaging time greater than one hour
              if (navhrs(nh) .gt. 1) then
CC                if end of data and no data left over, go to next avg.
                if (ifnext .eq. 14.and.jhrin(nh) .le. 0) go to 70
CC                if hour calculated and not end of data
                if (ifcalc .gt. 0.and.ifnext .ne. 14) then
CC                  loop over source groups and accumulate current hour.
                  do 80 ig=1,ngrps
CC                    current hour start record.
                    iptrec = ircbf(ircbf(1)+ig-1)
CC                    navhrs(nh) hour start record.
                    iogrec = ircbf(ircbf(6+nh)+ig-1)
                    ier = JACUM(nrec,iptrec,iogrec)
                    if (ier .lt. 0) go to 200
                    if (ier .gt. 0) go to 210
   80             continue
                end if
CC                if end of averaging period nh.
                if (mod(ntothr,navhrs(nh)) .eq. 0.or.ifnext .eq. 14)
     *              then
CC                  calculate navhrs(nh) hour average.
                  if (ifnext .eq. 14) then
                    if (jhrin(nh) .lt. navhrs(nh)) then
                      i = navhrs(nh) - jhrin(nh)
                      jhrin(nh) = jhrin(nh) + i
                      jhrms(nh) = jhrms(nh) + i
                    end if
                  end if
CC                  navhrs(nh) hour start record.
                  do 90 ig=1,ngrps
                    iogrec = ircbf(ircbf(6+nh)+ig-1)
                    ier = JHAVG(nrec,iogrec,jhrin(nh),jhrms(nh),
     *                          navhrs(nh),ifdep,1)
                    if (ier .lt. 0) go to 200
                    if (ier .gt. 0) go to 210
   90             continue
                end if
              else
                if (ifnext .eq. 14) go to 70
              end if
              if (navhrs(nh) .eq. 1.or.mod(ntothr,navhrs(nh)) .eq. 0.or.
     *            ifnext .eq. 14) then
CC                check average against maximum-50.
                if (iswopt(18) .gt. 0) then
CC                  loop over source groups.
                  do 150 ig=1,ngrps
CC                    maximum-50 start record.
                    iogrec = ircbf(ircbf(12+nh)+ig-1)
CC                    number of words to read from disk.
                    n = 50 * 4 * 4 + 4
CC                     read disk data.
                    ier = JDSKR(n,iogrec,xmx50)
                    if (ier .lt. 0) go to 200
                    if (navhrs(nh) .eq. 1) then
CC                      current hour start record.
                      iptrec = ircbf(ircbf(1)+ig-1)
                      ier = JDSKR(ntotal,iptrec,calcsa)
                      if (ier .lt. 0) go to 200
                    else
CC                      start record for averaging time navhrs(n).
                      iptrec = ircbf(ircbf(6+nh)+ig-1)
                      ier = JDSKR(ntotal,iptrec,calcsa)
                      if (ier .lt. 0) go to 200
                    end if
CC                     accumulate maximum 50 receptors.
                    do 140 k=1,4
                      if (iswopt(k) .gt. 0) then
                        ijoff = jjjoff(k)
CC                        accumulate maximum 50 receptors.
                        do 130 i=1,ntotpt
                          if (nmx50(k) .gt. 0) then
                            do 110 j=1,nmx50(k)
                              if (calcsa(i+ijoff) .gt. vmx50(j,k)) then
                                nmx50(k) = MIN0(nmx50(k) + 1,50)
                                do 100 m=j,nmx50(k)-1
                                  l = nmx50(k) - (m - j)
                                  vmx50(l,k) = vmx50(l-1,k)
                                  vmxxx(l,k) = vmxxx(l-1,k)
                                  vmxyy(l,k) = vmxyy(l-1,k)
                                  imxdt(l,k) = imxdt(l-1,k)
  100                          continue
                               go to 120
                              end if
  110                       continue
                          end if
                          if (nmx50(k) .ge. 50) go to 130
                          nmx50(k) = nmx50(k) + 1
                          j = nmx50(k)
  120                     vmx50(j,k) = calcsa(i+ijoff)
                          if (i .gt. ngrdpt) then
                            vmxxx(j,k) = xdscrt(i-ngrdpt)
                            vmxyy(j,k) = ydscrt(i-ngrdpt)
                          else
                            jp = (i + nxpnts -1) / nxpnts
                            ip = i - (jp - 1) * nxpnts
                            vmxxx(j,k) = xcoord(ip)
                            vmxyy(j,k) = ycoord(jp)
                          end if
CC                          save date/time in compressed form.
                          imxdt(j,k) = iswopt(7)
  130                   continue
                      end if
  140               continue
CC                    write accumulated data back out.
CC                    maximum-50 start record.
                    iogrec = ircbf(ircbf(12+nh)+ig-1)
CC                    number of words to write to disk.
                    n = 50 * 4 * 4 + 4
CC                    write disk data.
                    ier = JDSKW(n,iogrec,xmx50)
                    if (ier .gt. 0) go to 210
  150             continue
                end if
CC
CC                  highest, second highest
                if (iswopt(12) .gt. 0) then
CC                  check average against highest and second highest.
CC
CC                  loop over source groups.
                  do 170 ig=1,ngrps
                    if (navhrs(nh) .eq. 1) then
CC                      current hour start record.
                      iptrec = ircbf(ircbf(1)+ig-1) - 1
                    else
CC                      start record for averaging time navhrs(n).
                      iptrec = ircbf(ircbf(6+nh)+ig-1) - 1
                    end if
CC                    highest, second highest start record.
                    iogrec = ircbf(ircbf(18+nh)+ig-1) - 1
CC                    start pointers high to start reading records.
                    ipnt1 = 128
                    ipnt2 = 128
CC                    do not allow write first time through.
                    ifwrt = 0
                    i = 0
  160               if (i .lt. ntotal) then
                      i = i + 1
CC                      get N-hour.
                      ier = INCPT(ipnt1,iptrec,iptbuf,1,iogfil,mognam,0)
                      if (ier .lt. 0) go to 200
CC                      get highest and second highest.
                      ier = INCPT(ipnt2,iogrec,ircbuf,4,iogfil,mognam,
     *                            ifwrt)
CC                      allow write from now on.
                      ifwrt = 1
CC                      if N-hour greater than highest
                      if (yrcbuf(ipnt1) .gt. xrcbuf(ipnt2)) then
CC                        if highest greater than second highest.
                        if (xrcbuf(ipnt2) .gt. xrcbuf(ipnt2+1)) then
CC                          replace second highest with highest.
                          xrcbuf(ipnt2+1) = xrcbuf(ipnt2)
                          ircbuf(ipnt2+3) = ircbuf(ipnt2+2)
                        end if
CC                        replace highest with N-hour.
                        xrcbuf(ipnt2) = yrcbuf(ipnt1)
                        ircbuf(ipnt2+2) = iswopt(7)
CC                      if N-hour greater than second highest
                      else if (yrcbuf(ipnt1) .gt. xrcbuf(ipnt2+1)) then
CC                        replace second highest with N-hour.
                        xrcbuf(ipnt2+1) = yrcbuf(ipnt1)
                        ircbuf(ipnt2+3) = iswopt(7)
                      end if
                      go to 160
                    end if
CC                    write last record back out.
                    ier = IORAN(iogrec,ircbuf,iogfil)
                    if (ier .gt. 0) go to 210
  170             continue
                end if
                if (navhrs(nh) .gt. 1) then
CC                  zero out averaged arrays.
                  do 180 ig=1,ngrps
CC                    navhrs(nh) hour start record.
                    iptrec = ircbf(ircbf(6+nh)+ig-1)
                    ier = INITD(nrec,iptrec)
                    if (ier .gt. 0) go to 210
  180             continue
                  jhrin(nh) = 0
                  jhrms(nh) = 0
                  jhrex(nh) = 0
                end if
              end if
            end if
            go to 70
          end if
        end if
CC        zero out current hour arrays.
        do 190 ig=1,ngrps
CC          group start record.
          iptrec = ircbf(ircbf(1)+ig-1)
          ier = INITD(nrec,iptrec)
          if (ier .gt. 0) go to 210
  190   continue
      end if
      go to 230
CC      read error
  200 ier = KERRS(1,mognam,IABS(ier),iogrec,xdum,xdum,xdum,xdum,0)
      go to 220
CC      write error.
  210 ier = KERRS(2,mognam,ier,iogrec,xdum,xdum,xdum,xdum,0)
CC      write error.
      if (ier .eq. 6422.or.ier .eq. 913.or.ier .eq. 956) then
        i = (4 * (iogpnt * 128 + istrec + 4) + 999) / 1000
        i = KERRS(48,mognam,i,idum,xdum,xdum,xdum,xdum,0)
      end if
  220 ifnext = 3
      if (iferrs .ne. 1) iferrs = 2
  230 return
CC
CC-DEBUGS
 9000 format (/' *-*-* entered RMDLD')
CC-DEBUGE
      end
      Function JHAVG(norec,istrc,jhrin,jhrms,navhr,ifdep,iflg)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - JHAVG Version 01.1                          ---
CC--- Subroutine description - determines average n-hour value.     ---
CC---                                                               ---
CC---------------------------------------------------------------------
CC
CC    norec - number of disk records to calc. average for.
CC    istrc - start disk record number.
CC    jhrin - total non-excluded hours in accumulation.
CC    jhrmx - total missing hours in accumulation.
CC    navhr - number of hours in average.
CC    ifdep - if deposition flag, set to start index of deposition
CC            data.
CC    iflg  - flag indicating if short term average or not.
CC            0 = no
CC            1 = yes
CC
CC    returns - JHAVG -
CC               0 - no problems.
CC        >0 or <0 - error number or negative of error number.
CC**********************************************************************
CC
      dimension xrcbuf(128)
CC
      include 'OCNTRL.INC'
      include 'OCALCS.INC'
CC
      equivalence (xrcbuf(1),ircbuf(1))
CC
CC**********************************************************************
CC
      JHAVG = 0
      jorec = 0
      jstrc = istrc - 1
      jdep = 0
   10 if (jorec .lt. norec) then
        jorec = jorec + 1
        jstrc = jstrc + 1
CC        read disk record values.
        ier = INRAN(jstrc,ircbuf,iogfil)
        if (ier .lt. 0) go to 30
        xdum = jhrin - jhrms
        if (jhrms .gt. 0.and.iflg .ne. 0.and.iswopt(29) .eq. 0) then
          xdum = FLOAT(navhr) * 0.75 + 0.4
          i = xdum
          xdum = MAX0(jhrin-jhrms,i)
        end if
        if (xdum .gt. 0.0) then
          xdum = 1.0 / xdum
        else
          xdum = 0.0
        end if
        do 20 j=1,128
          jdep = jdep + 1
          if (ifdep .gt. 0.and.jdep .ge. ifdep) go to 20
          if (xdum .gt. 0.0) then
            xrcbuf(j) = xrcbuf(j) * xdum
          end if
   20   continue
CC         write average value record back out.
        ier = IORAN(jstrc,ircbuf,iogfil)
        if (ier .gt. 0) go to 30
        go to 10
      end if
      go to 40
   30 JHAVG = ier
   40 return
      end
      Function JDSKW(numwd,norec,array)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - JDSKW Version 01.1                          ---
CC--- Subroutine description - write disk data from array.          ---
CC---                                                               ---
CC---------------------------------------------------------------------
CC
CC    numwd - number of words to write.
CC    norec - starting record number.
CC    array - array to write data from.
CC
CC    returns - JDSKW
CC          0 - data written ok.
CC         >0 - error, returns error number.
CC
CC**********************************************************************
CC
      dimension array(20000),xrcbuf(128)
CC
      include 'OCNTRL.INC'
CC
      equivalence (ircbuf(1),xrcbuf(1))
CC
CC**********************************************************************
CC
      JDSKW = 0
      jorec = norec - 1
      ien = 0
   10 ist = ien + 1
      if (ist .le. numwd) then
        ien = ist + 127
        if (ien .gt. numwd) ien = numwd
        jorec = jorec + 1
        do 20 i=ist,ien
          xrcbuf(i-ist+1) = array(i)
   20   continue
CC        write record.
        ier = IORAN(jorec,ircbuf,iogfil)
        if (ier .gt. 0) go to 30
        go to 10
      end if
      go to 40
   30 JDSKW = ier
   40 return
      end
      Function JACUM(nrecs,istr1,istr2)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - JACUM Version 01.1                          ---
CC--- Subroutine description - sums disk records for season, or     ---
CC---                          annual averages.                     ---
CC---------------------------------------------------------------------
CC
CC
CC    nrecs - number of disk records to sum.
CC    istr1 - start record of data to be added.
CC    istr2 - start record of data to be added to.
CC
CC    returns - JACUM -
CC               0 - no problems.
CC        >0 or <0 - error number or negative of error number.
CC**********************************************************************
CC
      dimension xrcbuf(128),yrcbuf(128)
CC
      include 'OCNTRL.INC'
CC
      equivalence (xrcbuf(1),ircbuf(1)),(yrcbuf(1),iptbuf(1))
CC
CC**********************************************************************
CC
      JACUM = 0
      jorec = 0
      jstr1 = istr1 - 1
      jstr2 = istr2 - 1
   10 if (jorec .lt. nrecs) then
        jorec = jorec + 1
        jstr1 = jstr1 + 1
        jstr2 = jstr2 + 1
CC        read record to be added.
        ier = INRAN(jstr1,iptbuf,iogfil)
        if (ier .lt. 0) go to 30
CC        read record to be added to.
        ier = INRAN(jstr2,ircbuf,iogfil)
        if (ier .lt. 0) go to 30
        do 20 i=1,128
          xrcbuf(i) = xrcbuf(i) + yrcbuf(i)
   20   continue
CC        write updated record back out.
        ier = IORAN(jstr2,ircbuf,iogfil)
        if (ier .gt. 0) go to 30
        go to 10
      end if
      go to 40
   30 JACUM = ier
   40 return
      end
