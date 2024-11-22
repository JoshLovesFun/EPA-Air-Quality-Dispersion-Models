      Subroutine RMDLC
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - RMDLC Version 01.3                          --- 110198
CC--- Subroutine description - controls calculations of the         ---
CC---                          dispersion models.                   ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      dimension irgrp(10),xrcbuf(128),ircbf(265)
CC
      include 'OCNTRL.INC'
      include 'OCDSPM.INC'
      include 'OCALCS.INC'
      include 'ONAMES.INC'
      include 'OWNTRL.INC'
CC
      equivalence (jatzr,catzr),(xrcbuf(1),ircbuf(1)),                  110198
     *            (xbxpel(1),ircbf(1))                                  110198
CC
CC**********************************************************************
CC
CC-DEBUGS
      if (ifdbug .gt. 0.and.ifdbug .lt. 7) then                         020797
        if (iotfil .gt. 0) WRITE (iotfil,9000)
      end if
CC-DEBUGE
CC
      if (icpont .eq. 1) then                                           110198
CC          restore parameters.                                         072497
        clddxs(kssndx) = svdxss                                         072497
        clddys(kssndx) = svdyss                                         072497
        clddzs(kssndx) = svdzss                                         072497
        relhgt(kssndx) = svhgts                                         072497
        qemiss(kssndx) = svisss                                         072497
        cldlng(kssndx) = svlngs                                         072497
        cldsht(kssndx) = svshts                                         072497
        clddpt(kssndx) = svdpts                                         072497
        relhte(kssndx) = svhtes                                         072497
      end if
      if (iogfil .gt. 0) then
        if (ifmode .eq. 0) then
CC          detailed output mode.
CC
CC-DEBUGS
          if (ifdbug .gt. 3.and.ifdbug .lt. 7) then                     020797
            WRITE (iotfil,9001) kssndx,iogfil,ntotal,ifcalc,iptpnt,
     *                          iptrec,iogpnt,iogrec,ifhrly
          end if
CC-DEBUGE
CC          output source calculations to scratch file.
          n = ntotal
CC          compress calculation array.
          if (ifcalc .gt. 0) then                                       110198
            n = 0
            j = 0
            i = 0
   10       if (i .lt. ntotal) then
              i = i + 1
              if (IFEQU(calcsa(i),0.0) .eq. 0) then
                j = j + 1
              end if
              if (IFEQU(calcsa(i),0.0) .ne. 0.or.i .eq. ntotal) then
                if (j .gt. 0) then
                  n = n + 1
                  if (j .gt. 1) then
                    jatzr = -j                                          110198
                    calcsa(n) = catzr                                   110198
                  else
                    calcsa(n) = 0.0
                  end if
                  j = 0
                end if
                if (IFEQU(calcsa(i),0.0) .ne. 0) then
                  n = n + 1
                  if (calcsa(i) .lt. 0.0) calcsa(i) = 0.0               110198
                  calcsa(n) = calcsa(i)
                end if
              end if
              go to 10
            end if
          end if
CC          save record pointer for hour ntothr, source kssndx.
          ier = IOCPT(iptpnt,iptrec,iptbuf,1,iogfil,mognam)
          norec = iptrec
          if (ier .gt. 0) go to 100
          if (ifcalc .gt. 0) then                                       110198
            iptbuf(iptpnt) = iogrec
            if (iogpnt .eq. 128) iptbuf(iptpnt) = iptbuf(iptpnt) + 1
          else
            iptbuf(iptpnt) = 0
          end if
CC          save offset to record pointer.
          ier = IOCPT(iptpnt,iptrec,iptbuf,1,iogfil,mognam)
          norec = iptrec
          if (ier .gt. 0) go to 100
          if (ifcalc .gt. 0) then                                       110198
            iptbuf(iptpnt) = iogpnt + 1
            if (iptbuf(iptpnt) .gt. 128) iptbuf(iptpnt) = 1
            iptbuf(iptpnt) = iptbuf(iptpnt) + n * 1000
          else
            iptbuf(iptpnt) = 0
          end if
          if (ifcalc .gt. 0) then                                       110198
            do 20 i=1,n
              ier = IOCPT(iogpnt,iogrec,ircbuf,1,iogfil,mognam)
              norec = iogrec
              if (ier .gt. 0) go to 100
              catzr = calcsa(i)
              ircbuf(iogpnt) = jatzr
   20       continue
          end if
        else
CC          summary output mode
CC
CC          determine source groups.
          igrp = IFGRP(kssndx,irgrp)
CC          update hourly values for each group
          do 70 i=1,igrp
            ngrp = irgrp(i)
            iogrec = ircbf(ircbf(1)+ngrp-1)
            ien = 0
   30       ist = ien + 1
            if (ist .le. ntotal) then
              ien = ist + 127
              if (ien .gt. ntotal) ien = ntotal
              if (ifwrtt(ngrp) .eq. 0) go to 40
CC              read calculation record.
              ier = INRAN(iogrec,ircbuf,iogfil)
              if (ier .lt. 0) go to 80
              go to 50
   40         k = jrnit(xrcbuf,128,0.0,0)
CC              add sources values to record
   50         if (ifcalc .gt. 0) then
                do 60 k=ist,ien
                  xrcbuf(k-ist+1) = xrcbuf(k-ist+1) + calcsa(k)
   60           continue
              end if
CC              write updated record back out.
              ier = IORAN(iogrec,ircbuf,iogfil)
              if (ier .gt. 0) go to 90
              iogrec = iogrec + 1
              go to 30
            end if
            ifwrtt(ngrp) = 1
   70     continue
        end if
      end if
      go to 120
CC      read error
   80 ier = KERRS(1,mognam,IABS(ier),iogrec,xdum,xdum,xdum,xdum,0)
      go to 110
CC      write error.
   90 ier = KERRS(2,mognam,ier,iogrec,xdum,xdum,xdum,xdum,0)
CC      write error.
  100 if (ier .eq. 6422.or.ier .eq. 913.or.ier .eq. 956) then
        if (ifmode .eq. 1) then
          i = ((nmxhrs * (2 * nsourc + 1) + ntotal * (nsourc + 1) *
     *        nmxhrs / 2 + istrec * 128 + 9 - (norec - 1) * 128) *
     *        4 + 999) / 1000
        else
          i = (ircbf(25) * 128 * 4 + 999) / 1000
        end if
        i = KERRS(48,mognam,i,idum,xdum,xdum,xdum,xdum,0)
      end if
  110 ifnext = 3
      if (iferrs .ne. 1) iferrs = 2
  120 return
CC
CC-DEBUGS
 9000 format (/' *-*-* entered RMDLC')
 9001 format (' RMDLC 100 - kssndx,iogfil,ntotal,ifcalc,iptpnt,iptrec=',
     *6i6/12x,'iogpnt,iogrec,ifhrly=',3i6)
CC-DEBUGE
      end
      Function IFGRP(isorc,irgrp)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IFGRP Version 01.1                          ---
CC--- Subroutine description - determines source group for source   ---
CC---                          isorc.                               ---
CC---------------------------------------------------------------------
CC
CC    isorc - source number
CC    irgrp - returned array containing the source group numbers
CC            containing source isorc.
CC
CC    returns - IFGRP
CC          0 - source number isorc is not in group.
CC         >0 - number of groups containing source isorc or number of
CC              values returned in irgrp.
CC
CC**********************************************************************
CC
      dimension irgrp(10)
CC
      include 'OCNTRL.INC'
      include 'OCALCS.INC'
CC
CC**********************************************************************
CC
      IFGRP = 0
      if (ngroup .le. 0) then
CC        single group for all sources.
        IFGRP = 1
        irgrp(1) = 1
      else
CC        loop over source groups.
        lgrps = 0
        ngrps = 0
   10   ngrps = ngrps + 1
        if (ngrps .le. ngroup) then
          if (ngrps .gt. 1) lgrps = lgrps + nsogrp(ngrps-1)
CC          loop over sources within source group ngrps.
          jgrps = 0
   20     jgrps = jgrps + 1
          if (jgrps .le. nsogrp(ngrps)) then
            if (isogrp(lgrps+jgrps) .gt. 0) then
              n1 = isogrp(lgrps+jgrps)
              n2 = n1
            else
              if (jgrps .eq. 1) then
                n1 = 1
              else
                if (isogrp(lgrps+jgrps-1) .gt. 0) then
                  n1 = isogrp(lgrps+jgrps-1)
                else
                  n1 = 1
                end if
              end if
              n2 = IABS(isogrp(lgrps+jgrps))
            end if
            if (isorc .ge. n1.and.isorc .le. n2) then
              IFGRP = IFGRP + 1
              irgrp(IFGRP) = ngrps
              go to 10
            end if
            go to 20
          end if
          go to 10
        end if
      end if
      return
      end
