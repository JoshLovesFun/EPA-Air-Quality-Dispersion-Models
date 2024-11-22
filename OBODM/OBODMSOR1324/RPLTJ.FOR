      Subroutine RPLTJ
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - RPLTJ Version 01.1                          ---
CC--- Subroutine description - controls printing of dispersion      ---
CC---                          model calculations for summary mode. ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
CC
      dimension xrcbuf(128),ircbf(265),ztran(10000)
CC
      include 'OCNTRL.INC'
      include 'OCALCS.INC'
      include 'OCDSPM.INC'
      include 'OWNTRL.INC'
      include 'ONAMES.INC'
CC
      equivalence (xrcbuf(1),ircbuf(1)),(kquan,kssndx),
     *            (xbxpel(1),ircbf(1)),(ztran(1),zcoord(1))
CC
CC*******************************************************************
CC
CC-DEBUGS
      if (ifdbug .gt. 0.and.ifdbug .lt. 7) then                         020797
        WRITE (iotfil,9000) iotfil,iogfil,ntothr
      end if
CC-DEBUGE
CC
CC-DEBUGS
      if (ifdbug .gt. 3.and.ifdbug .lt. 7) then                         020797
        WRITE (iotfil,9001) ntotal,iogfil,nsourc,ngrplt,ifhrly,
     *                      iseasn
      end if
CC-DEBUGE
CC      summary output mode.
CC
      ngrps = MAX0(ngrplt,1)
CC      get final number of hours and pointers.
      norec = istrec + 1
      ier = INRAN(norec,iptbuf,iogfil)
      if (ier .lt. 0) go to 50
CC      get total number of hours.
      ntothr = iptbuf(1)
      i = JINIT(nseasn,4,iptbuf(2) ,1)
      i = JINIT(nquart,4,iptbuf(6),1)
CC      get record pointers from disk file
      norec = istrec + 2
      ier = INRAN(norec,iptbuf,iogfil)
      if (ier .lt. 0) go to 50
      i = JINIT(ircbf,128,iptbuf,1)
      iogpnt = ircbf(25)
      nrecs = iogpnt
      norec = istrec + 3
      ier = INRAN(norec,iptbuf,iogfil)
      if (ier .lt. 0) go to 50
      i = JINIT(ircbf(129),128,iptbuf,1)
      norec = istrec + 4
      ier = INRAN(norec,iptbuf,iogfil)
      if (ier .lt. 0) go to 50
      i = JINIT(ircbf(257),9,iptbuf,1)
CC      number of disk records.
      nrec = (ntotal + 127) / 128
      if (ifavrg .eq. 2.or.ifavrg .eq. 3) then
CC
CC        seasonal/quarterly output is desired.
        if (ifsean .eq. 1) then
          nhravg = nseasn(iseasn)
        else
          nhravg = nquart(iseasn)
        end if
CC        source group ngrps.
CC
CC        season start record.
        iogrec = ircbf(ircbf(1+iseasn)+ngrps-1)
        ier = JDSKR(ntotal,iogrec,calcsa)
        if (ier .lt. 0) go to 50
      else if (ifavrg .eq. 4) then
CC
CC        annual output is desired.
CC        start record.
        iogrec = ircbf(ircbf(6)+ngrps-1)
        ier = JDSKR(ntotal,iogrec,calcsa)
        if (ier .lt. 0) go to 50
      else if (ifavrg .eq. 5.or.ifavrg .eq. 6) then
CC        highest or second highest.
        i = 0
        nh = 0
   10   if (nh .lt. 6.and.i .eq. 0) then
          nh = nh + 1
          if (navhrs(nh) .eq. nhravg) i = 1
          go to 10
        end if
        if (i .eq. 0) nh = 1
        if (ifavrg .eq. 5) then
          l = 1
        else
          l = 2
        end if
CC        highest, second highest start record.
        iogrec = ircbf(ircbf(18+nh)+ngrps-1) - 1
CC        number of words to read from disk.
        ipntr = 128
        j = 0
   20   if (j .lt. ntotal) then
          j = j + 1
          ier = INCPT(ipntr,iogrec,ircbuf,4,iogfil,mognam,0)
          if (ier .lt. 0) go to 60
          calcsa(j) = xrcbuf(ipntr+l-1)
          go to 20
        end if
      end if
CC      output dispersion calculations.
      ijoff = jjjoff(kquan)
      xdumc = 1.0
CC      plot calculations.
      if (kquan .le. 3) then
        if (iswopt(17) .lt. 10) then
CC          convert to output mass/volume units.
          xdumc = constv(iswopt(17)+1) /
     *            constm(iswopt(13)+1)
        end if
        if (kquan .eq. 1) then
CC          convert dosage to output time units.
          xdumc = xdumc / constt(iswopt(14)+1)
        end if
      else
CC        convert to output mass/area units.
        xdumc = consta(iswopt(16)+1) /
     *          constm(iswopt(13)+1)
      end if
      do 30 j=1,ntotpt
CC        convert units.
        calcsa(j) = calcsa(j+ijoff) * xdumc * cscale + dscale
   30 continue
CC
      if (nxyfnc .gt. 0) then
        i = JRNIT(functy,nxyfnc,calcsa,1)
CC-DEBUGS
        if (ifdbug .gt. 0.and.ifdbug .lt. 7) then                       020797
          WRITE (iotfil,9002) (functy(iiii),iiii=1,nxyfnc)
        end if
CC-DEBUGE
      end if
CC-DEBUGS
      if (ifdbug .gt. 0.and.ifdbug .lt. 7) then                         020797
        WRITE (iotfil,9004) csmins,csmaxs,ngrpct,nxpnts,nypnts,igrect,
     *                      ngrdpt,ntotal,grdang,xorgin,yorgin
        WRITE (iotfil,9005) (iswopt(i),i=1,38),(jjjoff(i),i=1,4),
     *                      (xcoord(i),i=1,nxpnts)
        WRITE (iotfil,9006) (ycoord(i),i=1,nypnts)
        WRITE (iotfil,9007) ((calcsa((j-1)*nxpnts+i),i=1,nxpnts),
     *                      j=1,nypnts)
      end if
CC-DEBUGE
      if (IANYI(ipoptn,npoptn,1) .ne. 0) iswplt(27) = 0
CC
      if (IANYI(ipoptn,npoptn,1) .eq. 0.and.(ioffst(1) .ge. 0.or.
     *    ioffst(2) .ge. 0)) then
        do 40 icontr=1,2
          if (icontr .eq. 1.and.ioffst(1) .ge. 0) then
            i = 1
            joffs1 = ioffst(1)
          else if (icontr .eq. 2.and.ioffst(2) .ge. 0) then
            i = 2
            joffs1 = ioffst(2)
          else
            i = 0
          end if
CC-DEBUGS
CC>       if (ifdbug .gt. 0.and.ifdbug .lt. 7) then                     020797
CC>         WRITE (*,9003) icontr,i,joffs1
CC>         pause 'Enter or Return to continue. > '
CC>       end if
CC-DEBUGE
          if (i .gt. 0) then
CC            determine maximum value and location surface 1, surface 2
CC            surface 1 is normally terrain height and surface 2 is
CC            normally dispersion.
            if (icontr .eq. 1) then
              call CMXMN(1,1,nxtran,nxtran,nytran,ztran(joffs1+1),
     *                   csmaxs(icontr),csmins(icontr),idumy,idumy)
            else
              call CMXMN(1,1,nxpnts,nxpnts,nypnts,calcsa(joffs1+1),
     *                   csmaxs(icontr),csmins(icontr),idumy,idumy)
            end if
CC-DEBUGS
CC>         if (ifdbug .gt. 0.and.ifdbug .lt. 7) then                   020797
CC>           WRITE (*,9008) icontr,joffs1,csmins(icontr),
CC>  *                       csmaxs(icontr)
CC>           pause 'Enter or Return to continue. > '
CC>         end if
CC-DEBUGE
            if (csmaxs(icontr) .le. csmins(icontr)) then
              if (icontr .eq. 1.and.(IANYI(ipoptn,npoptn,2) .gt. 0.or.
     *            IANYI(ipoptn,npoptn,3) .gt. 0.or.
     *            IANYI(ipoptn,npoptn,4) .gt. 0)) then
                if (KERRS(27,mnpbuf,idum,idum,csmaxs(1),csmins(1),xdum,
     *              xdum,0) .eq. 1) go to 100
                iswplt(26) = 0
                go to 70
              else if (icontr .eq. 2.and.(IANYI(ipoptn,npoptn,2) .lt. 0
     *                 .or.IANYI(ipoptn,npoptn,3) .lt. 0.or.
     *                 IANYI(ipoptn,npoptn,4) .lt. 0)) then
                call mblk1(mnpbuf,22)
                j = kquan
                if (j .eq. 3) then
                  j = 5
                else if (j .eq. 4) then
                  j = 7
                end if
                call mvch1(modelt(j),mnpbuf,14)
                if (KERRS(28,mnpbuf,idum,idum,csmaxs(2),csmins(2),xdum,
     *              xdum,0) .eq. 1) go to 100
                iswplt(26) = 0
                go to 70
              end if
            end if
          end if
   40   continue
      else if (IANYI(ipoptn,npoptn,1) .ne. 0) then
        csmaxs(1) = functy(nxyfnc)
        csmins(1) = functy(1)
      end if
      go to 100
CC
   50 if (KERRS(1,mognam,IABS(ier),norec,xdum,xdum,xdum,xdum,0)
     *    .eq. 1) go to 100
   60 iswplt(26) = 0
      ifrdwt(6) = -1
      go to 80
   70 kfnxt = 1
      go to 90
   80 kfnxt = 2
   90 ifnext = 15
      iferrs = 0
  100 return
CC
CC-DEBUGS
 9000 format (/' *-*-* entered RPLTJ - iotfil,iogfil,ntothr=',3i8)
 9001 format (' RPLTJ - ntotal,iogfil,nsourc,ngrplt,ifhrly,iseasn=',
     *7i5)
 9002 format (' RPLTJ - functy=',1p,5e13.6)
 9003 format (' RPLTJ - icontr,i,joffs1=',3i4)
 9004 format (' RPLTJ - csmins,csmaxs=',4e13.6/
     *9x,'ngrpct,nxpnts,nypnts=',3i5/
     *9x,'igrect,ngrdpt,ntotal,grdang,xorgin,yorgin=',3i4,3f12.4)
 9005 format (' RPLTJ - iswopt=',38i2/9x,'jjjoff=',4i5/
     *(9x,'xcoord=',5f12.4))
 9006 format (9x,'ycoord=',5f12.4)
 9007 format (' RPLTJ - calcsa=',1p,5e13.6)
 9008 format (' RPLTJ - icontr,joffs1,csmins,csmaxs=',2i4,1p,2e13.6)
CC-DEBUGE
      end
