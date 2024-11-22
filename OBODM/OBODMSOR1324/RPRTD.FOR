      Subroutine RPRTD
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - RPRTD Version 01.3                          --- 041806
CC--- Subroutine description -  initialize loop over meteorological ---
CC---                           input data.                         ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      integer*2 icol,irow,ibak,ifor
      character*48 mdisp                                                060607
      character*1 mbufr,mdash,mnoys
      character*6 mbuf6      
      character*31 mesg1      
      character*22 mscr1,mscr2
      character*80 mbufs
CC
CC
      dimension mbufr(80),iofil(6),mnoys(2),ircbf(265),vmx50(50,4),     121098
     *          imxdt(50,4),vmxxx(50,4),vmxyy(50,4),nmx50(4),           121098
     *          xmx50(804),jhrin(11),jhrms(11),jhrex(11)                121098
CC
      include 'OCNTRL.INC'
      include 'OCDSPM.INC'
      include 'OCALCS.INC'
      include 'ONAMES.INC'
      include 'OWNTRL.INC'
CC
      equivalence (igtfil,iofil(1)),(mbufr(1),mbufs),                   121098
     *            (xbxpel(1),ircbf(1)),(vmx50(1,1),xmx50(1)),           121098
     *            (imxdt(1,1),xmx50(201)),(vmxxx(1,1),xmx50(401)),      121098
     *            (vmxyy(1,1),xmx50(601)),(nmx50(1),xmx50(801)),        121098
     *            (jhrin(1),xbxpsl(1)),(jhrms(1),xbxpsl(12)),           121098
     *            (jhrex(1),xbxpsl(23)),(xmx50(1),ixlpnt(1,1))          121098
CC
      data mdash/'-'/,mnoys/'N','Y'/,
     *     mscr1/'SCRATCH1.OBD'/,mscr2/'SCRATCH2.OBD'/,
     *     mesg1/'@OBODM processing case        ~'/
CC
CC**********************************************************************
CC
CC-DEBUGS
      if (ifdbug .gt. 0.and.ifdbug .lt. 7) then                         020797
        if (iotfil .gt. 0) WRITE (iotfil,9000)
      end if
CC-DEBUGE
CC
      llflg = 0                                                         060607
      ifpsol = 0
      ifcnvt = 1
      if (iotfil .gt. 0) then
CC        other misc. required data.
        j = 0
        if (iswopt(3) .gt. 0) j = j + 1
        if (iswopt(1) .gt. 0) j = j + 1
        if (ifhrly .gt. 0) j = j + 10
        if (j .gt. 0) then
          if (IOPUT(0,2,8,j+3) .eq. 0) then
            WRITE (iotfil,9002)
          else
            llines = llines - 3
          end if
          if (iswopt(3) .gt. 0) then
            call UNITS(mbufr,2,itmunt,0,0,1,5)
            WRITE (iotfil,9003) (mbufr(i),i=1,2),timavg
          end if
          if (iswopt(1) .gt. 0) then
            i = IWTBF(mbufr,9,timinv,4,0)
            WRITE (iotfil,9004) (mbufr(i),i=1,9)
          end if
          if (ifhrly .gt. 0) then
            WRITE (iotfil,9005)
            call MBLK1(mbufr,80)
            WRITE (mbufs,9006) (mnoys(iscdys(i)+1),i=1,60)
CC          if (MOD(iscyrs,4) .ne. 0) then                              010400
            if (LEAPS(iscyrs) .eq. 0) then                              010400
              mbufr(60) = mblnk1
              j = 1
            else
              j = 0
            end if
            WRITE (iotfil,9007) (mbufr(i),i=1,60)
            call MBLK1(mbufr,80)
            WRITE (mbufs,9006) (mnoys(iscdys(i-j)+1),i=61,121)
            WRITE (iotfil,9008) (mbufr(i),i=1,61)
            call MBLK1(mbufr,80)
            WRITE (mbufs,9006) (mnoys(iscdys(i-j)+1),i=122,182)
            WRITE (iotfil,9009) (mbufr(i),i=1,61)
            call MBLK1(mbufr,80)
            WRITE (mbufs,9006) (mnoys(iscdys(i-j)+1),i=183,244)
            WRITE (iotfil,9010) (mbufr(i),i=1,62)
            call MBLK1(mbufr,80)
            WRITE (mbufs,9006) (mnoys(iscdys(i-j)+1),i=245,305)
            WRITE (iotfil,9011) (mbufr(i),i=1,61)
            call MBLK1(mbufr,80)
            WRITE (mbufs,9006) (mnoys(iscdys(i-j)+1),i=306,366)
            WRITE (iotfil,9012) (mbufr(i),i=1,61)
          end if
        end if
CC
CC        determine number of i/o files used.
        j = 0
        do 10 i=1,6
          if (i .ne. 3.and.iofil(i) .gt. 0) j = j + 1
   10   continue
        if (ibcfil .gt. 0) j = j + 1
        if (isgfil .gt. 0) j = j + 1
        if (ihrfmt .gt. 2) j = j + 4
        if (j .gt. 0) then
CC          i/o files used
          if (IOPUT(0,2,6,j+3) .eq. 0) then
            WRITE (iotfil,9013)
          else
            llines = llines - 3
          end if
          if (igtfil .gt. 0) then
            call MVCH1(mgtnam,mbufr,22)
            n = LRJST(mbufr,22,1)
            j = MAX0(49-n,1)
            WRITE (iotfil,9014) (mdash,i=1,j-1),mblnk1,(mbufr(i),i=1,n)
          end if
          if (isvfil .gt. 0) then
            if (msvnam .ne. mscr2) then
              call MVCH1(msvnam,mbufr,22)
              n = LRJST(mbufr,22,1)
              j = MAX0(48-n,1)
              WRITE (iotfil,9015) (mdash,i=1,j-1),mblnk1,(mbufr(i),
     *                             i=1,n)
            end if
          end if
          if (iotfil .gt. 0) then
            call MVCH1(motnam,mbufr,22)
            n = LRJST(mbufr,22,1)
            j = MAX0(43-n,1)
            WRITE (iotfil,9016) (mdash,i=1,j-1),mblnk1,(mbufr(i),i=1,n)
          end if
          if (iogfil .gt. 0) then
            if (mognam .ne. mscr1) then
              call MVCH1(mognam,mbufr,22)
              n = LRJST(mbufr,22,1)
              j = MAX0(39-n,1)
              WRITE (iotfil,9017) (mdash,i=1,j-1),mblnk1,(mbufr(i),
     *                             i=1,n)
            end if
          end if
          if (inhfil .gt. 0) then
            call MVCH1(mnhnam,mbufr,22)
            n = LRJST(mbufr,22,1)
            j = MAX0(37-n,1)
            WRITE (iotfil,9018) (mdash,i=1,j-1),mblnk1,(mbufr(i),i=1,n)
          end if
          if (ishfil .gt. 0) then
            call MVCH1(mshnam,mbufr,22)
            n = LRJST(mbufr,22,1)
            j = MAX0(45-n,1)
            WRITE (iotfil,9019) (mdash,i=1,j-1),mblnk1,(mbufr(i),i=1,n)
          end if
          if (ibcfil .gt. 0) then
            call MVCH1(mbcnam,mbufr,22)
            n = LRJST(mbufr,22,1)
            j = MAX0(47-n,1)
            WRITE (iotfil,9020) (mdash,i=1,j-1),mblnk1,(mbufr(i),i=1,n)
          end if
          if (isgfil .gt. 0) then
            call MVCH1(msgnam,mbufr,22)
            n = LRJST(mbufr,22,1)
            j = MAX0(44-n,1)
            WRITE (iotfil,9021) (mdash,i=1,j-1),mblnk1,(mbufr(i),i=1,n)
          end if
        end if
      end if
CC
CC        set final calculation parameters
CC
      if (decays .gt. 0.0) decays = decays * constt(idcunt+1)
      if (densty .gt. 0.0) densty = (densty / constm(idunit+1)) *       041806
     *                              constv(jdunit+1) * constv(6)        041806
      if (ifgpwn .gt. 0) then
        do 20 i=1,nvsprt
          drpupr(i) = drpupr(i) * constl(ipsunt+1) / constl(8)
          drplwr(i) = drplwr(i) * constl(ipsunt+1) / constl(8)
          if (ipvunt .ne. 9) then
            vspart(i) = vspart(i) * constl(ipvunt+1) /
     *                  constt(jpvunt+1)
          else
            vspart(i) = vspart(i) * constl(ipvunt+1)
          end if
   20   continue
      end if
      airprs = airprs * constp(iprunt+1)
      cldhgt = cldhgt * constl(iceunt+1)
      roughl = roughl * constl(irount+1) / constl(6)
      dphidz = constc(ivgunt+1,2) * dphidz / constl(jvgunt+1)
      if (ispunt .ne. 9) then
        refspd = refspd * constl(ispunt+1) / constt(jspunt + 1)
      else
        refspd = refspd * constl(ispunt+1)
      end if
      airtmp = constc(itpunt+1,2) * (airtmp - constc(itpunt+1,1))
      sigmap = sigmap * constg(isyunt+1)
      sigmep = sigmep * constg(iszunt+1)
      trbixr = trbixr * constg(isxunt+1)
      tauosa = tauosa * constt(istunt+1)
      hmdpth = hmdpth * constl(ihmunt+1)
      hgtmet = hgtmet * constl(ihunit+1)
      dirshr = dirshr * constg(idsunt+1) / constl(jdsunt+1)
      if (spdshr .gt. 0.0) then
        if (issunt .ne. 9) then
          spdshr = spdshr * constl(issunt+1) / constt(jssunt+1)
        else
          spdshr = spdshr * constl(issunt+1)
        end if
      end if
      timavg = timavg * constt(itmunt+1)
CC      convert decay from half-life (s) to rate coefficient (/s)
      if (decays .gt. 0.0) decays = 0.69314718 / decays
      xorgin = xorgin * constl(igunit+1)
      yorgin = yorgin * constl(igunit+1)
CC      convert receptor points to meters
      if (nxpnts .gt. 0) then
        do 30 i=1,nxpnts
          xcoord(i) = xcoord(i) * constl(igunit+1)
   30   continue
      end if
      if (nypnts .gt. 0.and.igrect .le. 0) then
        do 40 i=1,nypnts
          ycoord(i) = ycoord(i) * constl(igunit+1)
   40   continue
      end if
      do 60 j=1,nypnts
        do 50 i=1,nxpnts
          n = (j - 1) * nxpnts + i
          if (zcoord(n) .lt. 0.0) zcoord(n) = 0.0
          zcoord(n) = zcoord(n) * constl(izunit+1)
          if (iswopt(6) .eq. 2.or.n .eq. 1.or.iswopt(6) .eq. 3) then    120702
            if (zcoord(n) .gt. hmdpth+hgtmet) then
              llflg = 1                                                 060607
              if (KERRS(12,mnpbuf,idum,idum,zcoord(n),xcoord(i),
     *                  ycoord(j),hmdpth+hgtmet,0) .ge. 1) then

                go to 270
              end if
            end if
          end if
   50   continue
   60 continue
      if (nxypnt .gt. 0) then
        do 70 i=1,nxypnt
          zdscrt(i) = zdscrt(i) * constl(izunit+1)
          xdscrt(i) = xdscrt(i) * constl(igunit+1)
          if (igrect .le. 0) then                                       071700
            ydscrt(i) = ydscrt(i) * constl(igunit+1)
          end if
          if (zdscrt(i) .gt. hmdpth+hgtmet) then
            llflg = 1                                                   060607
            if (KERRS(12,mnpbuf,idum,idum,zdscrt(i),xdscrt(i),
     *                ydscrt(i),hmdpth+hgtmet,0) .ge. 1) then

              go to 270
            end if
          end if
   70   continue
      end if
      if (igrect .eq. 0) then
CC        adjust rectangular coordinates for origin.
        if (nxpnts .gt. 0) then
          do 80 i=1,nxpnts
            xcoord(i) = xcoord(i) + xorgin
   80     continue
        end if
        if (nypnts .gt. 0) then
          do 90 i=1,nypnts
            ycoord(i) = ycoord(i) + yorgin
   90     continue
        end if
        if (nxypnt .gt. 0) then
          do 100 i=1,nxypnt
            xdscrt(i) = xdscrt(i) + xorgin
            ydscrt(i) = ydscrt(i) + yorgin
  100     continue
        end if
      end if
CC        convert source coordinates to meters, scale source emission
CC        strength and convert to grams and convert source time units
CC        to seconds.
      do 110 i=1,nsourc
        j = IVGET(isrect(i),1)
        xdumy = constl(j+1)
        clddxs(i) = clddxs(i) * xdumy
        clddxe(i) = clddxe(i) * xdumy
        if (IVGET(isrect(i),2) .le. 0) then
          clddys(i) = clddys(i) * xdumy
          clddye(i) = clddye(i) * xdumy
        end if
        clddzs(i) = clddzs(i) * xdumy
        clddze(i) = clddze(i) * xdumy
        if (IVGET(isrect(i),2) .le. 0.and.iswopt(23) .ne. 0) then
CC          adjust rectangular source coordinates for origin.
          clddxs(i) = clddxs(i) + xorgin
          clddys(i) = clddys(i) + yorgin
          clddxe(i) = clddxe(i) + xorgin
          clddye(i) = clddye(i) + yorgin
        end if
        if (cldlng(i) .gt. 0.0) cldlng(i) = cldlng(i) * xdumy           072497
        if (cldsht(i) .gt. 0.0) cldsht(i) = cldsht(i) * xdumy           072497
        j = IVGET(isrect(i),3)
        xdumy = constl(j+1)
        if (relhgt(i) .gt. 0.0) relhgt(i) = relhgt(i) * xdumy
        if (relhte(i) .gt. 0.0) relhte(i) = relhte(i) * xdumy           072497
        if (clddpt(i) .gt. 0.0) clddpt(i) = clddpt(i) * xdumy           072497
        cldang(i) = cldang(i) * dtorad
        xsmlry(i) = xsmlry(i) * constl(IVGET(ismunt,1)+1)
        xsmlrz(i) = xsmlrz(i) * constl(IVGET(ismunt,2)+1)
        xlrgry(i) = xlrgry(i) * constl(IVGET(ismunt,3)+1)
        xlrgrz(i) = xlrgrz(i) * constl(IVGET(ismunt,4)+1)
        hetcnt(i) = hetcnt(i) * consth(IVGET(jqunit(i),1)+1) /
     *              constm(IVGET(jqunit(i),2)+1)
        brnrat(i) = brnrat(i) * constm(IVGET(kqunit(i),1)+1) /
     *              constt(IVGET(kqunit(i),2)+1)
CC        convert source mass units to grams.
        qemiss(i) = qemiss(i) * constm(IVGET(iqunit(i),2)+1)
  110 continue
CC
CC      initialize data save areas used in RMDLD.                       121098
      i = JINIT(jhrin,11,0,0)                                           121098
      i = JINIT(jhrms,11,0,0)                                           121098
      i = JINIT(jhrex,11,0,0)                                           121098
      i = JRNIT(vmx50,50*4,0.0,0)                                       121098
      i = JINIT(imxdt,50*4,0,0)                                         121098
      i = JRNIT(vmxxx,50*4,0.0,0)                                       121098
      i = JRNIT(vmxyy,50*4,0.0,0)                                       121098
      i = JINIT(nmx50,4,0,0)                                            121098
CC      initialize loop over hourly data.
      ntothr = 0
CC      turn calculate dispersion flag on.
      ifcalc = 1
CC      turn read met. data obs. on.
      ifrddm = 1
CC      set no calculation errors.
      ifrdwt(5) = 0
CC      set lateral and vertical dispersion coefficients to zero.
      alphad = 0.0
      betadf = 0.0
CC      set shortz upper and lower turbulence to initial values.
CC      initialize season counters to zero.
      i = JINIT(nseasn,4,0,0)
      i = JINIT(nquart,4,0,0)
CC      set minute to zero.
      inimin = 0
CC      initialize particulate loop flag.
      ifprlp = 0
CC      if particle categories.
      if (ifgpwn .gt. 0.and.nvsprt .gt. 0) then
CC        set flag for particle categories.
CC        ifprlp - controls the looping over particle categories,
CC                 0 - either no particle categories or nvsprt
CC                     > 0 & betale = 0.0.  if nvsprt > 0, loop over
CC                     categories in VERT2 or in VERT3.
CC                 1 - nvsprt > 0 & betale > 0.0 the loop over
CC                     categories starts in function DISPR.
        if (nvsprt .gt. 0.and.betale .gt. 0.0) ifprlp = 1
      end if
CC-DEBUGS
      if (ifdbug .gt. 1.and.ifdbug .lt. 7) then                         020797
        WRITE (iotfil,9001) iogfil,nvsprt,ifgpwn,ifprlp,betale,ntotal,
     *                      nsourc,(iswopt(i),i=1,5)
      end if
CC-DEBUGE
CC
      if (ifhrly .gt. 0) then
CC        met. data are hourly input records.
CC        rewind met. input data file.
        REWIND (unit=inhfil,iostat=ier,err=240)
        inihrs = vllimt(1) - 100.0
        injdys = 1
CC        if source input data are also hourly.
        if (ifhsly .gt. 0) then
CC          set. hourly source data record counter to zero.
          insrec = 0
CC          rewind hourly source input data file.
          REWIND (unit=ishfil,iostat=ier,err=250)
CC          if the source data format is fixed field.
          if (ihsfmt .gt. 0) then
CC            determine number of real values by checking for e, f, g
CC            data edit codes.
            nforms = NFORM(mforms,151)
          end if
        end if
CC        set met. data record counter to zero.
        inhrec = 0
        i = JRNIT(convrt,20,1.0,0)
        i = JRNIT(convrt(1,2),20,0.0,0)
        i = JRNIT(convrt(1,3),20,1.0,0)
CC        if user specified met. data.
        if (ihrfmt .le. 1) then
CC          set units conversion constants for user specified input
CC          met. data.
          convrt(1,1) = constl(ispunt+1)
          if (ispunt .ne. 9) then
            convrt(1,3) = constt(jspunt+1)
          end if
          if (ivarmt(9) .ge. 0) then
            convrt(4,1) = constg(isyunt+1)
          end if
          if (ivarmt(10) .ge. 0) then
            convrt(5,1) = constg(iszunt+1)
          end if
          if (ivarmt(11) .ge. 0) then
            convrt(6,1) = constg(isxunt+1)
          end if
          convrt(7,1) = constc(itpunt+1,2)
          convrt(7,2) = constc(itpunt+1,1)
          convrt(8,1) = constp(iprunt+1)
          if (ivarmt(14) .ge. 0) then
            convrt(9,1) = constc(ivgunt+1,2)
            convrt(9,2) = 0.0
            convrt(9,3) = constl(jvgunt+1)
          end if
          if (ivarmt(15) .ge. 0) then
            convrt(10,1) = constl(ihmunt+1)
          end if
          convrt(11,1) = constl(issunt+1)
          if (issunt .ne. 9) then
            convrt(11,3) = constt(jssunt+1)
          end if
          convrt(12,1) = constg(idsunt+1)
          convrt(12,3) = constl(jdsunt+1)
          convrt(15,1) = constt(istunt+1)
          convrt(16,1) = constl(irount+1)
          convrt(16,3) = constl(6)
          convrt(20,1) = constl(iceunt+1)
CC          if fixed field format determine number of values in rec.
          if (ihrfmt .eq. 1) then
            nformt = NFORM(mformt,151)
          end if
        else if (ihrfmt .ge. 2) then
CC          ISC data file.
CC          initialize hour counter.
          ischrs = 0
CC          initialize day counter.
          iscdct = 0
CC          get station numbers and years
          inhrec = inhrec + 1
          if (ihrfmt .eq. 2) then
            READ (inhfil,iostat=ier,err=120,end=120) issi,isyi,iusi,
     *                                               iuyi
          else
            READ (inhfil,*,iostat=ier,err=120,end=120) issi,isyi,iusi,
     *                                                 iuyi
          end if
          if (iotfil .gt. 0) then
            WRITE (iotfil,9023) issi,isyi,iusi,iuyi
          end if
          go to 130
  120     if (KERRS(1,mnhnam,ier,inhrec,xdum,xdum,xdum,xdum,0) .eq. 1)
     *        go to 280
          go to 270
        end if
      else
        ifmode = 0
      end if
CC      set start record for saving dispersion model calculations.
CC      istrec gives the number of records (128 words or 512 bytes)
CC      used to save all initializing met., receptor, etc. data.
CC
CC      for detailed mode runs, ifmode = 0:
CC
CC      the value ((nmxhrs * (2 * nsourc + 1)) + 1 +127) / 128 is
CC      the maximum amount of disc space required to hold the pointer
CC      records.  The pointers contain the date, packed into
CC      one word, followed by two words for each source.  the two
CC      words for each source give one word for the record number where
CC      the source calculations start and the number of calculation
CC      words to read with the offset packed into the second word.
CC      the offset is which word in the 128 word buffer the data
CC      actually start at.  An extra word is added to the total number
CC      required, to save the total number of hours processed (ntothr).
CC      also, an extra 8 words are added to the total number required,
CC      to save the total number of hours counted in each seasn
CC      followed by the total number of hours counted in each quarter.
CC      the total number of hours (ntothr) and number of hours in each
CC      seasn (nseasn) and quarter (nquart) are the first 9 words of
CC      the pointer records.  the 1 added to the end starts the
CC      calculated data at the record following the pointers records.
CC
CC      for summary mode runs, ifmode = 1:
CC
CC      only seasonal (quarterly), annual, maximum-50, highest and
CC      second highest output tables are produced in the summary mode.
CC      averages are saved in order of source group (ngroup) under
CC      each type of report.  record pointers for each group under
CC      each type of report are saving in ascending order in array
CC      iptbuf.  The first ngroup sets of records are the 1-hour
CC      calculations for source groups 1 through ngroup.  The next
CC      sets of records store the current n-hour (except 1 hour)
CC      average for each group.  The 4 next sets of records store the
CC      seasonal or quarterly accumulated 1-hour values for each group.
CC      The next ngroup sets of records store the accumulated 1-hour
CC      values for each group for the annual average.  The next sets
CC      of records store the maximum-50 values for each averaging time
CC      and each source group.  The last sets of records store the
CC      highest and second highest n-hour averages for each averaging
CC      time and each source group.
CC
  130 if (ifmode .eq. 0) then
CC        detailed mode.
        iogrec = ((nmxhrs * (2 * nsourc + 1)) + 9 + 127) / 128 +
     *           istrec + 1
CC-DEBUGS
        if (ifdbug .gt. 0.and.ifdbug .lt. 7) then                       020797
          write (iotfil,9022) iogrec,nmxhrs,nsourc,istrec,nmxdys
        end if
CC-DEBUGE
CC        initialize dispersion calculation data offset counter -1.
        iogpnt = 0
CC        initialize first pointer record counter.
        iptrec = istrec + 1
CC        initialize first pointer -1.
        iptpnt = 9
      else
CC        summary mode.
CC
CC        ircbf(1) - index pointing to location in ircbf containing
CC                   the start record for the first source group in
CC                   the hourly calculations save area.
CC        ircbf(2) - index pointing to location in ircbf containing
CC                   the start record for the first source group in
CC                   season 1 for the seasonal averages.
CC        ircbf(3) - season 2
CC        ircbf(4) - season 3
CC        ircbf(5) - season 4
CC        ircbf(6) - index pointing to location in ircbf containing
CC                   the start record for the first source group in
CC                   the annual averages.
CC        ircbf(7) - index pointing to location in ircbf containing
CC                   the start record for the first source group in
CC                   the N-hour averages, for the first averaging
CC                   time
CC        ircbf(8) to ircbf(12) - second through sixth N-hour averaging
CC                   times.
CC       ircbf(13) - index pointing to location in ircbf containing
CC                   the start record for the first source group in
CC                   the Maximum-50 for N-hour averages, for the first
CC                   averaging time.
CC       ircbf(14) to ircbf(18) - second through sixth Maximum-50
CC                   averaging times.
CC       ircbf(19) - index pointing to location in ircbf containing
CC                   the start record for the first source group in
CC                   the highest and second highest N-hour averages,
CC                   for the first averaging time.
CC       ircbf(20) to ircbf(24) - second through sixth highest and
CC                   second highest N-hour averaging times.
CC       ircbf(25) - total number of disk records required.
CC       ircbf(26) to ircbf(265) - record numbers pointing to start
CC                   records for each of the above categories.
CC
CC        set up start record pointers for current 1-hour source groups.
        i = JINIT(ircbf,265,0,0)
        ngrps = ngroup
        if (ngrps .le. 0) ngrps = 1
CC        number of records required for calculations.
        n = (ntotal + 127) / 128
        ipnt = 26
CC        set pointer to first record number for current hourly values.
        ircbf(1) = ipnt
        ircbf(ircbf(1)) = istrec + 5
        ier = INITD(n,ircbf(ircbf(1)))
        if (ier .gt. 0) go to 260
        if (ngrps .gt. 1) then
          do 140 i=2,ngrps
            ipnt = ipnt + 1
CC            group starts at record.
            ircbf(ipnt) = ircbf(ipnt-1) + n
            ier = INITD(n,ircbf(ipnt))
            if (ier .gt. 0) go to 260
  140     continue
        end if
CC        set up start record pointers for seasonal or quarterly
CC        averages.
        if (ifsean .gt. 0) then
          do 160 j=1,4
CC            set pointer to first record for each season.
            ircbf(j+1) = ipnt + 1
            do 150 i=1,ngrps
              ipnt = ipnt + 1
              ircbf(ipnt) = ircbf(ipnt-1) + n
              ier = INITD(n,ircbf(ipnt))
              if (ier .gt. 0) go to 260
              nrecs = ircbf(ipnt) + n - 1
  150       continue
  160     continue
        end if
CC        set up start record pointers for annual average.
        if (ifanul .gt. 0) then
CC          set pointer to first record for annual average.
          ircbf(6) = ipnt + 1
          do 170 i=1,ngrps
            ipnt = ipnt + 1
            ircbf(ipnt) = ircbf(ipnt-1) + n
            ier = INITD(n,ircbf(ipnt))
            if (ier .gt. 0) go to 260
            nrecs = ircbf(ipnt) + n - 1
  170     continue
        end if
CC        set up start record pointers for navhrs n-hour averages.
        do 190 j=1,6
          if (navhrs(j) .gt. 1) then
CC            set pointer to first record number each n-hour average.
            ircbf(6+j) = ipnt + 1
            do 180 i=1,ngrps
              ipnt = ipnt + 1
CC              group starts at record.
              ircbf(ipnt) = ircbf(ipnt-1) + n
              ier = INITD(n,ircbf(ipnt))
              if (ier .gt. 0) go to 260
              nrecs = ircbf(ipnt) + n - 1
  180       continue
          end if
  190   continue
CC        maximum-50
        if (iswopt(18) .gt. 0) then
          do 210 j=1,6
            if (navhrs(j) .gt. 0) then
CC              set up start record pointers for maximum-50 tables.
              ircbf(12+j) = ipnt + 1
              do 200 i=1,ngrps
                ipnt = ipnt + 1
                ircbf(ipnt) = ircbf(ipnt-1) + n
CC                provide room for max, x, y, date/time
                n = (50 * 4 * 4 + 4 + 127) / 128
                nrecs = ircbf(ipnt) + n - 1
                ier = INITD(n,ircbf(ipnt))
                if (ier .gt. 0) go to 260
  200         continue
            end if
  210     continue
        end if
CC        set up start record pointers for highest tables.
        if (iswopt(12) .gt. 0) then
          do 230 j=1,6
            if (navhrs(j) .gt. 0) then
              ircbf(18+j) = ipnt + 1
              do 220 i=1,ngrps
                ipnt = ipnt + 1
                ircbf(ipnt) = ircbf(ipnt-1) + n
CC                provide room for highest, date/time, second highest,
CC                date/time.
                n = (4 * ntotal + 127) / 128
                nrecs = ircbf(ipnt) + n - 1
                ier = INITD(n,ircbf(ipnt))
                if (ier .gt. 0) go to 260
  220         continue
            end if
  230     continue
        end if
CC        save total number of records required.
        iogpnt = nrecs
        ircbf(25) = iogpnt
CC        save record pointers in disk file
        i = JINIT(iptbuf,128,ircbf,1)
        norec = istrec + 2
        ier = IORAN(norec,iptbuf,iogfil)
        if (ier .gt. 0) go to 260
        i = JINIT(iptbuf,128,ircbf(129),1)
        norec = istrec + 3
        ier = IORAN(norec,iptbuf,iogfil)
        if (ier .gt. 0) go to 260
        i = JINIT(iptbuf,9,ircbf(257),1)
        norec = istrec + 4
        ier = IORAN(norec,iptbuf,iogfil)
        if (ier .gt. 0) go to 260
      end if
      if (ifhrly .gt. 0.and.ifbtch .eq. 0) then
        i = KERRS(58,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0)
      end if
      go to 280
  240 if (KERRS(1,mnhnam,ier,0,xdum,xdum,xdum,xdum,0) .eq. 1) go to 280
      go to 270
  250 if (KERRS(1,mshnam,ier,0,xdum,xdum,xdum,xdum,0) .eq. 1) go to 280
      go to 270
  260 if (KERRS(2,mognam,ier,norec,xdum,xdum,xdum,xdum,0) .eq. 1)
     *    go to 280
      if (ier .eq. 6422.or.ier. eq. 913.or.ier .eq. 956) then
        i = (4 * (iogpnt * 128 + istrec + 4) + 999) / 1000
        i = KERRS(48,mognam,i,idum,xdum,xdum,xdum,xdum,0)
      end if
  270 iferrs = 0
      ifnext = 3
  280 continue
      if (llflg .gt. 0) then                                            060607
CC      Clean up warning message on crt screen.                         060607
        icol = 10                                                       060607
        k = 48                                                          060607
        irow = 12                                                       060607
        nrec = 9                                                        060607
        ifor = ibackt                                                   060607
        ibak = iforet                                                   060607
        mdisp = mblnk1                                                  060607
        do i=1,nrec                                                     060607
          call IWRSC(icol,irow,k,mdisp,ifor,ibak)                       060607
          irow = irow - 1                                               060607
        end do                                                          060607
      end if                                                            060607
      if (ifhrly .gt. 0.and.ifbtch .gt. 0.and.iferrs .ne. 1) then
CC        display current case.
        ibak = ibackt
        ifor = iforet
        icol = 18
        irow = 3
        dumy = nbtchc
        i = IWTBF(mbuf6,6,dumy,0,1)
        mesg1(24:29) = mbuf6
        call IWRSC(icol,irow,31,mesg1,ibak,ifor)
      end if
      return
CC
CC-DEBUGS
 9000 format (/' *-*-* entered RPRTD')
 9001 format (' RPRTD - iogfil,nvsprt,ifgpwn,ifprlp=',4i6/
     *9x,'betale,ntotal,nsourc=',f6.3,2i6/
     *9x,'(iswopt(i),i=1,5)=',5i4)
CC-DEBUGE
 9002 format (/26x,'- Other Required Data -'/)
 9003 format (' Averaging time (',2a1,') (time-average con.) ',25('-'),
     *1x,f9.2)
 9004 format (' Quasi-cont. (dosage) integration time inc. (sec) ',
     *16('-'),1x,9a1)
 9005 format (' Days calculated -'/
     *' Month',11x,'Day of Month',10x,'Month',11x,'Day of Month'/
     *2(7x,'1234567890123456789012345678901')/1x,76('-'))
 9006 format (62a1)
 9007 format ('  Jan [',31a1,'] Feb [',29a1,']')
 9008 format ('  Mar [',31a1,'] Apr [',30a1,']')
 9009 format ('  May [',31a1,'] Jun [',30a1,']')
 9010 format ('  Jul [',31a1,'] Aug [',31a1,']')
 9011 format ('  Sep [',30a1,']  Oct [',31a1,']')
 9012 format ('  Nov [',30a1,']  Dec [',31a1,']')
 9013 format (/24x,'- Input/Output Files Used -'/)
 9014 format (' Input save data file name ',49a1)
 9015 format (' Output save data file name ',48a1)
 9016 format (' Print solution output file name ',43a1)
 9017 format (' Graphics/solution input/output file ',39a1)
 9018 format (' Hourly meteorological input data file ',37a1)
 9019 format (' Hourly source input data file ',45a1)
 9020 format (' Batch processing input file ',47a1)
 9021 format (' Sigma plot output solution file ',44a1)
CC-DEBUGS
 9022 format (' RPRTD 130 - iogrec,nmxhrs,nsourc,istrec,nmxdys=',5i6)
CC-DEBUGE
 9023 format (' Meteorological surface station number ',29('-'),i8/
     *7x,'"',11x,'"    data year ',38('-'),i4/
     *7x,'"',8x,'upper air station number ',27('-'),i8/
     *7x,'"',10x,'"    "  data year ',36('-'),i4)
      end
      Function NFORM(mform,nch)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - NFORM Version 01.2                          ---
CC--- Subroutine description - determine the number of variables to ---
CC---                          read from the format.                ---
CC---------------------------------------------------------------------
CC
CC    mform - array containing format edit codes.
CC    nch   - maximum number of characters in array
CC
CC    returns - NFORM - >= 0  number of variables to read.
CC                        -1  error.
CC
CC**********************************************************************
CC
      character*1 mlprn,mrprn,mzero,mnine,mbufr,mform
      character*2 mbuf2
CC
CC
      dimension mform(151),mbufr(2)
CC
      include 'ONAMES.INC'
CC
      equivalence (mbufr,mbuf2)
CC
      data mlprn/'('/,mrprn/')'/,mzero/'0'/,mnine/'9'/
CC
CC**********************************************************************
CC
      NFORM = 0
      n = 0
      is = 0
   10 if (n .lt. nch) then
        n = n + 1
CC        check for e, f and g edit codes.
        do 20 i=5,7
          if (IFCHR(mform(n),i) .ne. 0) go to 30
   20   continue
        i = 0
CC        if left paren.
        if (mform(n) .eq. mlprn) go to 30
CC        if right paren.
        if (mform(n) .eq. mrprn) then
          if (is .ne. 0) then
CC            close off right paren
            NFORM = NFORM + nums * nf
            is = 0
            nums = 0
            nf = 0
          end if
        end if
        go to 10
   30   if (n-1 .gt. 0) then
CC          if previous character is numeric
          if (mzero .le. mform(n-1).and.mform(n-1) .le. mnine) then
            mbuf2 = mblnk1
            mbufr(2) = mform(n-1)
            if (n-2 .gt. 0) then
CC              if more than one digit in number
              if (mzero .le. mform(n-2).and.mform(n-2) .le. mnine) then
                mbufr(1) = mform(n-2)
              end if
            end if
CC            get number
            read (mbuf2,9000,err=40) num
            go to 50
   40       num = 1
   50       continue
          else
            if (i .gt. 0) then
              num = 1
            else
              num = 0
            end if
          end if
          if (num .gt. 0) then
CC            if e,f or g edit codes
            if (i .gt. 0) then
CC              if inside parentheses
              if (is .ne. 0) then
CC                accumulate count until right paren
                nf = nf + num
              else
CC                no parentheses, add to total count
                NFORM = NFORM + num
              end if
            else
CC              number preceding left paren
              is = 1
              nums = num
              nf = 0
            end if
          end if
        else
          if (i .gt. 0) NFORM = NFORM + 1
        end if
        go to 10
      end if
      return
 9000 format (i2)
      end
