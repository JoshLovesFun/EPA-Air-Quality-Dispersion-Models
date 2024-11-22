      Subroutine RPLTB
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - RPLTB Version 01.1                          ---
CC--- Subroutine description - get data values to plot under        ---
CC---                          detailed processing mode.            ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      character*22 msch4
      character*56 mhrsf
      logical*1 iflgs,iflg1
CC
      dimension ztran(10000),vmxhi(20000),ixbuf(128),vxbuf(128)         110198
CC
      include 'OCNTRL.INC'
      include 'OCDSPM.INC'
      include 'OWNTRL.INC'
      include 'OCALCS.INC'
      include 'ONAMES.INC'
CC
      equivalence (mhrsf,mhrlab(1)),(jatzr,catzr),(ztran(1),zcoord(1)), 110198
     *            (ixbuf,vxbuf),(vmxhi(1),calcsa(1)),(kquan,kssndx),    110198
     *            (kfnxt,wspmin),(ihrs,pekcon),(jdys,pekdos)            110198
CC
      data msch4/'SCRATCH4.OBD'/                                        110198
CC
CC**********************************************************************
CC
CC-DEBUGS
      if (ifdbug .gt. 0.and.ifdbug .lt. 7) then                         020797
        WRITE (iotfil,9010) iotfil,iogfil,ntothr,ifavrg,nhravg,
     *                      cscale,dscale,ifrddm
      end if
CC-DEBUGE
      if (ifnewp .ne. 0) then
        j = 2
        if (IANYI(ipoptn,npoptn,1) .ne. 0) j = 1
        if (ifnewp .ne. j) then
          lswxax = 0
          lswyax = 0
          xincax = 0.0
          yincax = 0.0
          nincxs = 10
          nincys = 10
          ndgxes = -1
          ndgyes = -1
          ifchlb = 1
        end if
      end if
      if (ngrplt .le. 0.or.ngrpct .le. 0) then
CC        number of different source combinations to print.
        ngrpc = 1
        jsogrp(1) = -nsourc
      else
        ngrpc = ngrpct
      end if
      nhrlab = 0
CC
CC          loop over n-hourly, seasonal or quarterly, annual averages.
      if (ifhrly .gt. 0) then
        if (ifavrg .eq. 5.or.ifavrg .eq. 6) then
CC          set offset indices for 1st and 2nd highest tables.
          if (ifavrg .eq. 5) then
            j = 2
          else
            j = 3
          end if
          if (j*ngrdpt .le. nmxcor) then
            iflg1 = .false.
          else
            iflg1 = .true.
            imxfil = 18
            OPEN (unit=imxfil,file=msch4,iostat=ier,err=180,
     *            status='unknown',access='direct',form='unformatted',
     *            recl=512)
            i = JINIT(ixbuf,128,0,0)
            n = (2 * ngrdpt + 127) / 128
            iorec = 0
   10       if (iorec .lt. n) then
              iorec = iorec + 1
              ier = IORAN(iorec,ixbuf,imxfil)
              if (ier .gt. 0) go to 160
              go to 10
            end if
          end if
        end if
        i = KERRS(35,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0)
      end if
CC
CC      zero out accumulation arrays.
      i = JRNIT(calcsa,nmxcor,0.0,0)
CC
CC      loop over hours.
      if (ifhrly .gt. 0) ifccok = 2
      ihrin = IABS(ifrddm) - 1
      iflgs = .false.
   20 jhrin = 0
      jhrms = 0
      jhrex = 0
CC
CC      loop over averaging time
   30 ihrin = ihrin + 1
      if (ifhrly .gt. 0) then
        if (ifctlc .eq. 2) then
          if (JFCTC(52) .ne. 0) go to 220
        end if
        if (ihrin .gt. ntothr) go to 120
      end if
CC
CC      loop over sources within source group.
      jgrps = 0
   40 jgrps = jgrps + 1
      if (jgrps .le. ngrpc) then
        if (jsogrp(jgrps) .gt. 0) then
          n1 = jsogrp(jgrps)
          n2 = n1
        else
          i = jsogrp(jgrps)
          j = 0
          if (jgrps .gt. 1) j = jsogrp(jgrps-1)
          if (jgrps .eq. 1.or.j .lt. 0.or.j .gt. IABS(i)) then
            n1 = 1
          else
            n1 = j + 1
          end if
          n2 = IABS(i)
        end if
CC        loop over sources.
        i = (ihrin - 1) * (2 * nsourc + 1) + 9 + 1
        iptrec = (i + 127) / 128 + istrec
        iptpnt = MOD(i,128)
        if (iptpnt .eq. 0) iptpnt = 128
        norec = iptrec
        ier = INRAN(iptrec,iptbuf,iogfil)
        if (ier .lt. 0) go to 150
        call IGTDT(iptbuf(iptpnt),ifcalc,inihrs,inidys,inimon,iniyrs,
     *             1)
        injdys = 0
        call JULAN(iniyrs,inimon,inidys,injdys)
        if (.not.iflgs.and.ifavrg .eq. 1) then
          if (ifrddm .lt. 0) then
            if (injdys .ne. jdys) go to 30
            if (inihrs .lt. ihrs) go to 30
          end if
          iflgs = .true.
        else if (ifavrg .eq. 2.or.ifavrg .eq. 3) then
          if (ifavrg .eq. 2) then
            isean = ISEAS(inimon,1)
          else if (ifavrg .eq. 3) then
            isean = ISEAS(inimon,2)
          end if
          if (isean .ne. iseasn) go to 30
        end if
        do 60 ks=1,nsourc
          ier = INCPT(iptpnt,iptrec,iptbuf,1,iogfil,mognam,0)
          if (ier .lt. 0) then
            iswplt(26) = 0
            go to 200
          end if
          iogrec = iptbuf(iptpnt)
          ier = INCPT(iptpnt,iptrec,iptbuf,1,iogfil,mognam,0)
          if (ier .lt. 0) then
            iswplt(26) = 0
            go to 200
          end if
          if (n1 .le. ks.and.ks .le. n2) then
            nwrds = iptbuf(iptpnt) / 1000
            iogpnt = iptbuf(iptpnt) - nwrds * 1000 - 1
CC-DEBUGS
            if (ifdbug .gt. 3.and.ifdbug .lt. 7) then                   020797
              WRITE (iotfil,9000) ks,iogfil,jgrps,istrec,iogrec
            end if
CC-DEBUGE
CC
            if (iogrec .gt. 0.and.nwrds .gt. 0) then
CC              read mass storage records.
              norec = iogrec
              ier = INRAN(iogrec,ircbuf,iogfil)
              if (ier .lt. 0) go to 150
              n = 0
              l = 0
              j = 0
   50         if (n .lt. nwrds.and.j .lt. ngrdpt) then
                n = n + 1
                ier = INCPT(iogpnt,iogrec,ircbuf,1,iogfil,mognam,0)
                if (ier .lt. 0) then
                  iswplt(26) = 0
                  go to 200
                end if
                jatzr = ircbuf(iogpnt)
                if (jatzr .lt. 0) then                                  110198
                  jatzr = IABS(jatzr)                                   110198
                  if (l+jatzr .ge. jjjoff(kquan)+1.and.                 110198
     *                l .le. jjjoff(kquan)+ngrdpt) then
                    if (l .lt. jjjoff(kquan)+1) then
                      if (l+jatzr .ge. jjjoff(kquan)+ngrdpt) then
                        j = j + jatzr                                   110198
                      else
                        j = j + (l + jatzr - jjjoff(kquan))             110198
                      end if
                    else if (l+jatzr .le. jjjoff(kquan)+ngrdpt) then    110198
                      j = j + jatzr                                     110198
                    else
                      j = j + (jjjoff(kquan) + ngrdpt - l + 1)
                    end if
                  end if
                  l = l + jatzr                                         110198
                else
                  l = l + 1
                  if (l .ge. jjjoff(kquan)+1.and.
     *                l .le. jjjoff(kquan)+ngrdpt) then
                    j = j + 1
                    calcsa(j) = calcsa(j) + catzr
                  end if
                end if
                go to 50
              end if
            end if
          end if
   60   continue
        go to 40
      end if
CC
CC      see if end of averaging period.
   70 if (ifhrly .gt. 0) then
CC        increment hours in average.
        if (ifcalc .ne. 0) then                                         110198
          jhrin = jhrin + 1
        else
          if (iscdys(injdys) .gt. 0) then
            jhrin = jhrin + 1
            jhrms = jhrms + 1
          else
            jhrex = jhrex + 1
          end if
        end if
CC        if not enough, go get more.
        if (jhrin+jhrex .lt. nhravg.and.ihrin .lt. ntothr) then
CC          if first hour of average, save date/time, etc.
          if (jhrin .eq. 1) then
            istyr = iniyrs
            istmo = inimon
            istdy = inidys
            isthr = inihrs
            istjd = injdys
          end if
          ienyr = iniyrs
          ienmo = inimon
          iendy = inidys
          ienhr = inihrs
          ienjd = injdys
          go to 30
        end if
CC        end of averaging period, save last date/time, etc.
        if (iscdys(injdys) .ne. 0) then
          ienyr = iniyrs
          ienmo = inimon
          iendy = inidys
          ienhr = inihrs
          ienjd = injdys
          if (nhravg .le. 1) then
            istyr = ienyr
            istmo = ienmo
            istdy = iendy
            istjd = ienjd
            isthr = ienhr
          end if
        end if
        if (jhrin .gt. 0) then
          if (ifavrg .eq. 1) then
            if (jhrin .eq. 1) then
              WRITE (mhrsf,9001) istmo,istdy,istyr,istjd,isthr
              nhrlab = JPACK(mhrlab,29)
            else
              WRITE (mhrsf,9002) istmo,istdy,istyr,istjd,isthr,
     *                           ienmo,iendy,ienyr,ienjd,ienhr
              nhrlab = JPACK(mhrlab,56)
            end if
          else
            nhrlab = 0
          end if
CC          if greater than hourly.
          if (jhrin-jhrms .gt. 1.0.and.kquan .le. 3) then
CC            determine averages.
            xdumy = jhrin - jhrms
            if (jhrms .gt. 0.and.ifavrg .eq. 1.and.iswopt(29) .eq. 0)
     *          then
              xdumy = FLOAT(nhravg) * 0.75 + 0.4
              i = xdumy
              xdumy = MAX0(jhrin-jhrms,i)
            end if
            if (xdumy .gt. 0.0) then
              xdumy = 1.0 / xdumy
            else
              xdumy = 0.0
            end if
CC            average.
            do 80 i=1,ngrdpt
              calcsa(i) = calcsa(i) * xdumy
   80       continue
          end if
        end if
        if (ifavrg .eq. 5.or.ifavrg .eq. 6) then
CC          first and second highest at each receptor.
          if (.not.iflg1) then
            do 90 i=1,ngrdpt
              if (calcsa(i) .gt. vmxhi(i+ngrdpt)) then
                if (ifavrg .eq. 6) then
                  if (vmxhi(i+ngrdpt) .gt. vmxhi(i+2*ngrdpt)) then
                    vmxhi(i+2*ngrdpt) = vmxhi(i+ngrdpt)
                  end if
                end if
                vmxhi(i+ngrdpt) = calcsa(i)
              else if (ifavrg .eq. 6.and.calcsa(i) .gt.
     *                 vmxhi(i+2*ngrdpt)) then
                vmxhi(i+2*ngrdpt) = calcsa(i)
              end if
   90       continue
          else
            ipntr = 128
            iorec = 0
            ifwrt = 0
            i = 0
  100       if (i .lt. ngrdpt) then
              i = i + 1
              ier = INCPT(ipntr,iorec,ixbuf,2,imxfil,msch4,ifwrt)
              if (ier .lt. 0) then
                iswplt(26) = 0
                go to 200
              end if
              if (ier .gt. 0) go to 170
              ifwrt = 1
              if (calcsa(i) .gt. vxbuf(ipntr)) then
                if (vxbuf(ipntr) .gt. vxbuf(ipntr+1)) then
                  vxbuf(ipntr+1) = vxbuf(ipntr)
                end if
                vxbuf(ipntr) = calcsa(i)
              else if (calcsa(i) .gt. vxbuf(ipntr+1)) then
                vxbuf(ipntr+1) = calcsa(i)
              end if
              go to 100
            end if
            ier = IORAN(iorec,ixbuf,imxfil)
            if (ier .gt. 0) go to 160
          end if
          if (ihrin .lt. ntothr) then
            i = JRNIT(calcsa,ngrdpt,0.0,0)
            go to 20
          end if
CC          plot 1st or 2nd highest tables.
          if (.not.iflg1) then
            if (ifavrg .eq. 5) then
              i = JRNIT(calcsa,ngrdpt,vmxhi(ngrdpt+1),1)
            else
              i = JRNIT(calcsa,ngrdpt,vmxhi(2*ngrdpt+1),1)
            end if
          else
            iorec = 0
            ipntr = 128
            i = 0
  110       if (i .lt. ngrdpt) then
              i = i + 1
              ier = INCPT(ipntr,iorec,ixbuf,2,imxfil,msch4,0)
              if (ier .lt. 0) then
                iswplt(26) = 0
                go to 200
              end if
              if (ifavrg .eq. 5) then
                calcsa(i) = vxbuf(ipntr)
              else
                calcsa(i) = vxbuf(ipntr+1)
              end if
              go to 110
            end if
          end if
        end if
      end if
CC
CC      output dispersion calculations.
  120 xdumc = 1.0
CC      convert units.
      if (kquan .le. 3) then
        if (iswopt(17) .lt. 10) then
CC          convert to output mass/volume units.
          xdumc = constv(iswopt(17)+1) / constm(iswopt(13)+1)
        end if
        if (kquan .eq. 1) then
CC          convert dosage to output time units.
          xdumc = xdumc / constt(iswopt(14)+1)
        end if
      else
CC        convert to output mass/area units.
        xdumc = consta(iswopt(16)+1) / constm(iswopt(13)+1)
      end if
      do 130 i=1,ngrdpt
CC        convert units.
        calcsa(i) = calcsa(i) * xdumc * cscale + dscale
  130 continue
      if (nxyfnc .gt. 0) then
        i = JRNIT(functy,nxyfnc,calcsa,1)
CC-DEBUGS
          if (ifdbug .gt. 0.and.ifdbug .lt. 7) then                     020797
            WRITE (iotfil,9003) (functy(iiii),iiii=1,nxyfnc)
          end if
CC-DEBUGE
      end if
CC-DEBUGS
      if (ifdbug .gt. 0.and.ifdbug .lt. 7) then                         020797
        WRITE (iotfil,9005) csmins,csmaxs,ngrpct,nxpnts,nypnts,igrect,
     *                      ngrdpt,ntotal,grdang,xorgin,yorgin
        WRITE (iotfil,9006) (iswopt(i),i=1,38),(jjjoff(i),i=1,4),
     *                      (xcoord(i),i=1,nxpnts)
        WRITE (iotfil,9007) (ycoord(i),i=1,nypnts)
        WRITE (iotfil,9008) ((calcsa((j-1)*nxpnts+i),i=1,nxpnts),
     *                      j=1,nypnts)
      end if
CC-DEBUGE
      if (IANYI(ipoptn,npoptn,1) .ne. 0) iswplt(27) = 0
CC
      if (IANYI(ipoptn,npoptn,1) .eq. 0.and.(ioffst(1) .ge. 0.or.
     *    ioffst(2) .ge. 0)) then
        do 140 icontr=1,2
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
CC>         WRITE (*,9004) icontr,i,joffs1
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
CC>           WRITE (*,9009) icontr,joffs1,csmins(icontr),
CC>  *                       csmaxs(icontr)
CC>           pause 'Enter or Return to continue. > '
CC>         end if
CC-DEBUGE
            if (csmaxs(icontr) .le. csmins(icontr)) then
              if (icontr .eq. 1.and.(IANYI(ipoptn,npoptn,2) .gt. 0.or.
     *            IANYI(ipoptn,npoptn,3) .gt. 0.or.
     *            IANYI(ipoptn,npoptn,4) .gt. 0)) then
                if (KERRS(27,mnpbuf,idum,idum,csmaxs(1),csmins(1),xdum,
     *              xdum,0) .eq. 1) go to 220
                iswplt(26) = 0
                go to 190
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
     *              xdum,0) .eq. 1) go to 220
                iswplt(26) = 0
                go to 190
              end if
            end if
          end if
  140   continue
      else if (IANYI(ipoptn,npoptn,1) .ne. 0) then
        csmaxs(1) = functy(nxyfnc)
        csmins(1) = functy(1)
      end if
      go to 220
CC
  150 if (KERRS(1,mognam,IABS(ier),norec,xdum,xdum,xdum,xdum,0)
     *    .eq. 1) go to 220
      iswplt(26) = 0
      go to 200
  160 i = KERRS(2,msch4,ier,iorec,xdum,xdum,xdum,xdum,0)
  170 if (ier .eq. 6422.or.ier .eq. 913.or.ier .eq. 956) then
        j = (n * 128 * 4 + 999) / 1000
        i = KERRS(48,mnpbuf,j,idum,xdum,xdum,xdum,xdum,0)
        CLOSE (unit=imxfil,status='delete',err=190)
        iswplt(26) = 0
        go to 200
      end if
      if (iferrs .ne. 1.and.iferrs .ne. 2) then
        iswplt(26) = 0
        go to 200
      end if
      ifnext = 3
      go to 220
  180 if (KERRS(3,mognam,ier,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *    go to 220
      iswplt(26) = 0
      go to 200
  190 kfnxt = 1
      ifrdwt(6) = 2
      go to 210
  200 kfnxt = 2
      ifrdwt(6) = -1
  210 ifnext = 15
      iferrs = 0
  220 if (imxfil .gt. 0) CLOSE (unit=imxfil,status='delete',err=230)
  230 return
CC
CC-DEBUGS
 9000 format (' RPLTB 200 - ks,iogfil,jgrps,istrec,iogrec=',5i5)
CC-DEBUGE
 9001 format ('(for >',i2,'/>',i2,'/',i4,' (',i3.3,') ',i4.4,')')
 9002 format ('(for >',i2,'/>',i2,'/',i4,' (',i3.3,') ',i4.4,' to >',
     *i2,'/>',i2,'/',i4,' (',i3.3,') ',i4.4,')')
CC-DEBUGS
 9003 format (' RPLTB - functy=',1p,5e13.6)
 9004 format (' RPLTB - icontr,i,joffs1=',3i4)
 9005 format (' RPLTB - csmins,csmaxs=',4e13.6/
     *9x,'ngrpct,nxpnts,nypnts=',3i5/
     *9x,'igrect,ngrdpt,ntotal,grdang,xorgin,yorgin=',3i4,3f12.4)
 9006 format (' RPLTB - iswopt=',38i2/9x,'jjjoff=',4i5/
     *(9x,'xcoord=',5f12.4))
 9007 format (9x,'ycoord=',5f12.4)
 9008 format (' RPLTB - calcsa=',1p,5e13.6)
 9009 format (' RPLTB - icontr,joffs1,csmins,csmaxs=',2i4,1p,2e13.6)
 9010 format (' RPLTB - iotfil,iogfil,ntothr,ifavrg,nhravg=',5i5/
     *9x,'cscale,dscale,ifrddm=',2f12.4,i4)
CC-DEBUGE
      end
