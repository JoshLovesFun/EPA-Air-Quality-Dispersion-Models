      Subroutine RMDLA
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - RMDLA Version 01.31                         --- 021405
CC--- Subroutine description - controls calculations hourly input   ---
CC---                          met. data for the dispersion models. ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      integer*2 icol,irow,ibak,ifor,icoff,irowm,icolm                   011403
      character*6 mcalc,mbuf6,mbufs6                                    011403
      character*22 mvars,mesgc                                          011403
      character*31 mesg1                                                011403
      character*200 mbufr
      character*4 mfrm(2),mfprt                                         051503
      logical*1 ifchk,ifsth
CC
      dimension istab(24),rlhhm(48),hlhhm(24,2),awspd(24),tempr(24),
     *          afvnd(24),afvrd(24),pwrlw(24),dthdz(24),xdumy(100),
     *          mvars(20),space(350),idat1(6),idat2(6),datal(20),
     *          datak(20),dparm(20),iyr(24),imo(24),idy(24),ihr(24),
     *          hmrur(24),hmurb(24),mesg1(4),mesgc(3)                   011403
CC
      include 'OCNTRL.INC'
      include 'OCDSPM.INC'
      include 'OCALCS.INC'
      include 'ONAMES.INC'
CC
      equivalence (mbufr,mscrch(1)),(dparm(1),refspd),
     *            (space(1),ilinem(1)),(xdumy(1),calcsa(1)),
     *            (rlhhm(1),hlhhm(1,1)),(rlhhm(1),space(1)),
     *            (awspd(1),space(49)),(tempr(1),space(73)),
     *            (afvnd(1),space(97)),(afvrd(1),space(121)),
     *            (pwrlw(1),space(145)),(dthdz(1),space(169)),
     *            (istab(1),space(193)),(idat1(1),space(217)),
     *            (idat2(1),space(223))
      equivalence (idat1(1),llyrs),(idat1(2),llmon),(idat1(3),lldys),
     *            (idat1(4),llhrs),(idat1(5),ljdys),(idat1(6),llmin),
     *            (idat2(1),kkyrs),(idat2(2),kkmon),(idat2(3),kkdys),
     *            (idat2(4),kkhrs),(idat2(5),kjdys),(idat2(6),kkmin),
     *            (datal(1),space(229)),(datak(1),space(249)),
     *            (hlhhm(1,1),hmrur(1)),(hlhhm(1,2),hmurb(1)),
     *            (nthms,space(269)),(nthrx,space(270)),
     *            (nthrd,space(271)),(nthtt,space(272)),
     *            (nthcm,space(273)),(nther,space(274)),
     *            (ilstd,space(275)),(jofhr,space(276)),                031297
     *            (nthca,space(277)),(nthex,space(278))                 031297
CC
      data mcalc/' Calc.'/,icoff/3/
CC      names of met. parameters 6 through 21.
      data mvars/'Wind speed            ',
     *           'Wind power law        ',
     *           'Wind direction        ',
     *           'Lateral turbulence    ',
     *           'Vertical turbulence   ',
     *           'Longitudinal turb.    ',
     *           'Temperature           ',
     *           'Pressure              ',
     *           'Vert. pot. temp. grad.',
     *           'Mixing layer depth    ',
     *           'Wind speed shear      ',
     *           'Wind direction shear  ',
     *           'Pasquill stability    ',
     *           'Net radiation index   ',
     *           'Avg. time lat. turb.  ',
     *           'Roughness length      ',
     *           'Humidity              ',
     *           'Clearing index        ',
     *           'Cloud cover           ',
     *           'Cloud ceiling         '/
      data mesg1/'@OBODM processing hour        ~',
     *           '@OBODM processing case        ~',
     *           '@OBODM skipping hour          ~',
     *           '@OBODM looking for start hour ~'/
      data mesgc/'Calm winds    - ',                                    011403
     *           'Missing data  - ',                                    011403
     *           'Data errors   - '/                                    011403
      data mfrm/'from','vect'/                                          051503
CC
CC**********************************************************************
CC
CC-DEBUGS
      if (ifdbug .gt. 0.and.ifdbug .lt. 7) then                         020797
        if (iotfil .gt. 0) WRITE (iotfil,9000)
      end if
CC-DEBUGE
CC
      jer = 0
      icpont = 0                                                        110198
      if (ifhrly .gt. 0.and.ntothr .le. 0) then
CC        Set hour offset                                               090498
        if (vulimt(4) .gt. 2300.0) then                                 090498
          iofhrs = 0                                                    090498
        else                                                            090498
          iofhrs = 1                                                    090498
        end if                                                          090498
CC        missing hours counter.
        nthms = 0
CC        clearing index greater than threshold counter.
        nthrx = 0
CC        daytime hours counter.
        nthrd = 0
CC        total number of hours of met. data read.
        nthtt = 0
CC        total number of non-excluded/non-missing hours.               031297
        nthca = 0                                                       031297
CC        total number of excluded hours.                               031297
        nthex = 0                                                       031297
CC        calms counter.
        nthcm = 0
CC        error counter.
        nther = 0
CC        display hour count control flag.
        ilstd = 2
CC        first hour to calculate flag.
        ifsth = .false.
CC        determine if should check for sequence and missing data
CC        errors
        i = 0                                                           081298
        j = 0                                                           081298
        do while (i .lt. 6.and.j .eq. 0)                                081298
          i = i + 1                                                     081298
          j = MAX0(navhrs(i),j)                                         081298
        end do                                                          081298
        if (j .gt. 1.or.ifsean .gt. 0.or.ifanul .gt. 0.or.              081298
     *      iswopt(28) .eq. 0.or.ISUMI(iscdys,nmxdys) .ge. 30) then     110198
          ifchk = .true.
        else
          ifchk = .false.
        end if
        jofhr = 0
      end if
CC      if print maximum plume rise.
      if (iswopt(21) .gt. 0.and.ntothr .gt. 0.and.ifcalc .gt. 0) then
CC        print maximum plume rise heights for previous hour.
        i = 0
        j = 0
   10   if (i .lt. nsourc.and.j .eq. 0) then
CC          loop over sources to find height greater than zero.
          i = i + 1
          if (cldmax(i) .gt. 0.0) then
            j = 1
          end if
          go to 10
        end if
        if (j .ne. 0) then
CC          found height greater than zero, print sources.
          if (ntothr .eq. 1) then
CC            first hour.
            k = 1
          else
CC            second through last hour.
            k = 0
          end if
          if (ifhrly .gt. 0.and.iswopt(24) .le. 0) then
CC            hourly input met. data and print hourly data is off.
            i = IOPUT(k,3,1,3)
            WRITE (iotfil,9001) iniyrs,inimon,inidys,inihrs+inimin,
     *                          injdys
          else
            i = IOPUT(k,4,1,5)
          end if
          llines = llines - 1
          if (ifhrly .le. 0) WRITE (iotfil,9002)
          call UNITS(mscrch(3),2,iswopt(15),idumy,0,1,0)
          do 20 k=1,nsourc
            if (IOPUT(0,4,1,1) .ne. 0) then
              if (ifhrly .le. 0) then
                WRITE (iotfil,9002)
                llines = llines + 4
              end if
            end if
            iunit = IVGET(isrect(k),3)
            call UNITS(mscrch,2,iunit,idumy,0,1,0)
            xdumy(1) = 1.0 / constl(iunit+1)
            xdumy(2) = ABS(calhgt(k)) * xdumy(1)                        072497
            xdumy(3) = cldmax(k) * xdumy(1)
            xdumy(4) = xdumy(2) + xdumy(3)
            xdumy(5) = cldmxd(k) / constl(iswopt(15)+1)                 072497
            if (ifhrly .le. 0) then
              WRITE (iotfil,9003) k,(msoids(i,k),i=1,36),xdumy(2),
     *                            (mscrch(i),i=1,2),xdumy(3),
     *                            (mscrch(i),i=1,2),xdumy(4),
     *                            (mscrch(i),i=1,2),xdumy(5),
     *                            (mscrch(i+2),i=1,2)
            else
              WRITE (iotfil,9004) k,xdumy(2),(mscrch(i),i=1,2),
     *                            xdumy(3),(mscrch(i),i=1,2),
     *                            xdumy(4),(mscrch(i),i=1,2),
     *                            xdumy(5),(mscrch(i+2),i=1,2)
            end if
   20     continue
        end if
      end if
CC
CC
CC      if hourly input data.
   30 if (ifhrly .gt. 0) then
CC        if read data flag is off.
        if (ifrddm .le. 0) then
CC          reset previously read hour and data.
          i = JINIT(iniyrs,6,kkyrs,1)
          if (iswopt(11) .gt. 0) then
            i = JRNIT(dparm,20,datak,1)
            istabl = stblty
          end if
          go to 70
        end if
CC        save previous hours wind direction.
        dirsav = wnddir
        if (ihrfmt .le. 1) then
CC
CC
CC          user specified met. data, increment record counter.
          inhrec = inhrec + 1
CC          read input data record.
          ler = 1                                                       010400
          READ (inhfil,9005,iostat=ier,err=110,end=100) mbufr
CC          go get data values from input record.
          ier = IUSDA(mbufr,xdumy,mvars)
          if (ier .gt. 0) then
            if (ier .eq. 1.or.ifhsly .gt. 0) go to 150
            jer = jer + 1
            if (jer .gt. 10) then
              go to 150
            end if
            go to 30
          end if
          jer = 0
CC        Check if wind direction is from or towards                    051503
          mfprt = mfrm(1)                                               051503
          wdir = wnddir                                                 051503
          if (iswopt(34) .ne. 0) then                                   051503
CC            Direction is towards                                      051503
            mfprt = mfrm(2)                                             051503
            wnddir = wnddir + 180.0                                     051503
            if (wnddir .gt. 360.0) wnddir = wnddir - 360.0              051503
          end if                                                        051503
        else
CC
CC
CC          ISC (RAMMET or MPRM processed) data.
CC
CC          if hour counter is zero, read day of data.
          if (ischrs .eq. 0) then
CC            increment day counter.
            iscdct = iscdct + 1
CC            if max. days exceeded.
            if (ifmode .eq. 0.and.iscdct .gt. nmxdys) go to 100
            if (ihrfmt .le. 2.or.ihrfmt .eq. 6) then
CC              ISCST2 "UNFORM" or "SPECIL" 24 hour data.
              inhrec = inhrec + 1
              if (ihrfmt .eq. 2) then
                ler = 2                                                 010400
                READ (inhfil,iostat=ier,err=110,end=100) iniyrs,
     *                inimon,day,istab,awspd,tempr,afvnd,afvrd,hlhhm
              else
                ler = 3                                                 010400
                READ (inhfil,9006,iostat=ier,err=110,end=100) iniyrs,
     *                inimon,day,istab
                inhrec = inhrec + 1
                ler = 4                                                 010400
                READ (inhfil,9007,iostat=ier,err=110,end=100) awspd,
     *                tempr,afvnd,afvrd,hlhhm
              end if
              inidys = day
CC              rearrange mixing depths.
              do 50 k=1,23
                i = 2 * k
                j = i + 1
                do 40 n=1,k
                  x = rlhhm(i)
                  rlhhm(i) = rlhhm(j)
                  rlhhm(j) = x
   40           continue
   50         continue
            else if (ihrfmt .ge. 3) then
CC              ISC formatted hourly met. data format (ISC isw(22) = 2).
              if (jofhr .eq. 0) then
                ihrmn = 9999
                ihrmx = 0
              end if
              do 60 i=1,24
                inhrec = inhrec + 1
                if (ihrfmt .eq. 3) then
CC                  ISC formatted hourly met. data, ISCST2 'FREE' format
                  ler = 5                                               010400
                  READ (inhfil,*,iostat=ier,err=110,end=100) iyr(i),
     *                  imo(i),idy(i),ihr(i),afvrd(i),awspd(i),tempr(i),
     *                  istab(i),hmrur(i),hmurb(i)
                else if (ihrfmt .eq. 4) then
CC                  ISC formatted hourly met. data, ISCST2 "CARD" format
                  ler = 6                                               010400
                  READ (inhfil,9008,iostat=ier,err=110,end=100) iyr(i),
     *                  imo(i),idy(i),ihr(i),afvrd(i),awspd(i),tempr(i),
     *                  istab(i),hmrur(i),hmurb(i),pwrlw(i),dthdz(i)
                else if (ihrfmt .eq. 5) then
CC                  ISC formatted hourly met. data, ISCST2 "ASCII"
CC                  default format.
                  ler = 7                                               010400
                  READ (inhfil,9008,iostat=ier,err=110,end=100) iyr(i),
     *                  imo(i),idy(i),ihr(i),afvrd(i),awspd(i),tempr(i),
     *                  istab(i),hmrur(i),hmurb(i)
                end if
                if (jofhr .eq. 0) then
                  ihrmx = MAX0(ihrmx,ihr(i))
                  ihrmn = MIN0(ihrmn,ihr(i))
                end if
   60         continue
              if (jofhr .eq. 0) then
                if (ihrmn .gt. 100) ihrmn = ihrmn / 100
                if (ihrmx .gt. 100) ihrmx = ihrmx / 100
                if (ihrmn .gt. 0.and.ihrmx .gt. 23) then
                  iofhrs = 0
                  vllimt(4) = 100
                  vulimt(4) = 2400
                else
                  iofhrs = 1
                  vllimt(4) = 0
                  vulimt(4) = 2300
                end if
                jofhr = 1
              end if
            end if
          end if
CC          increment ISC hour counter.
          ischrs = ischrs + 1
          if (ischrs .gt. 24) then
            ischrs = 0
            go to 30
          end if
          if (ihrfmt .le. 2.or.ihrfmt .eq. 6) then
CC          hour of day.
            inihrs = ischrs - iofhrs
          else
CC          year, month, day, hour
            iniyrs = iyr(ischrs)
            inimon = imo(ischrs)
            inidys = idy(ischrs)
            inihrs = ihr(ischrs)
          end if
          if (inihrs .lt. 100) inihrs = inihrs * 100
          if (iniyrs .le. 0) iniyrs = iscyrs
CC          determine julian day.
          injdys = 0
          call JULAN(iniyrs,inimon,inidys,injdys)
CC          stability.
          i = vulimt(18)
          istabl = MIN0(istab(ischrs),i)
          stblty = istabl
CC          wind speed.
          refspd = awspd(ischrs)
CC          wind direction.
          mfprt = mfrm(2)
          wdir = afvrd(ischrs)
          wnddir = afvrd(ischrs) + 180.0
          if (wnddir .gt. 360.0) wnddir = wnddir - 360.0
CC          air temperature.
          airtmp = tempr(ischrs)
          airtmp = constc(2,2) * (airtmp - constc(2,1))
CC          mixing depth.
          hmdpth = hlhhm(ischrs,ischmd+1)
          if (ischmd .eq. 0.and.istabl .gt. 4) then
            hmdpth = 10000.0
          end if
CC          determine net radiation index.
          radinx = NETRX(istabl,refspd)
          if (ihrfmt .ne. 4) then
            pwrlaw = DFMET(1,radinx,istabl,refspd,0.0)
            dphidz = DFMET(5,radinx,istabl,refspd,airhum)
          else
            pwrlaw = pwrlw(ischrs)
            dphidz = dthdz(ischrs)
          end if
          sigmap = DFMET(3,radinx,istabl,refspd,0.0)
          sigmep = DFMET(2,radinx,istabl,refspd,0.0)
          trbixr = DFMET(6,radinx,istabl,refspd,0.0)
        end if
        if (iniyrs .lt. 50) then                                        020797
          iniyrs = iniyrs + 2000                                        020797
        else if (iniyrs .lt. 100) then                                  020797
          iniyrs = iniyrs + 1900                                        020797
        end if                                                          020797
      else
        if (ntothr .gt. 0) go to 100
      end if
   70 ntothr = ntothr + 1
      if (ifmode .eq. 0.and.ntothr .gt. nmxhrs) then
        ntothr = ntothr - 1
        go to 100
      end if
      nthtt = nthtt + 1
      if (ifhrly .gt. 0) then
        if (injdys .le. 0) then
          call JULAN(iniyrs,inimon,inidys,injdys)
        end if
        if (ntothr .le. 1) then
CC          initialize date/time to first hour minus 1 hour.
          i = JINIT(llyrs,6,iniyrs,1)
          call LOCAL(llyrs,llmon,lldys,llhrs,-100)
          ljdys = 0
          call JULAN(llyrs,llmon,lldys,ljdys)
          i = JRNIT(datal,20,dparm,1)
        end if
CC        calculation flag on.
        ifcalc = 1
CC        read next hours data flag on.
        ifrddm = 1
        ifmss = 0
        if (ifchk) then
CC          check for missing hour or hour sequence error.
          call LOCAL(llyrs,llmon,lldys,llhrs,100)
          ljdys = 0
          call JULAN(llyrs,llmon,lldys,ljdys)
          i = IDATR(iniyrs,llyrs,4)
          ifclm = 0
          if (i .gt. 0) then
            if (iniyrs .eq. llyrs+1) then
CC              possible change in year.
              j = vllimt(4)
              if (inihrs .eq. j) go to 80
            end if
CC            current date greater than expected date, missing data.
            if (iswopt(11) .gt. 0) then
CC              default missing values
              i = JRNIT(datak,20,dparm,1)
              i = JRNIT(dparm,20,datal,1)
              istabl = stblty
            else
CC            calculation flag off due to missing hour.
              ifcalc = 0
              nthms = nthms + 1
              ifmss = 1
              i = IOPUT(0,3,1,2)
              i = KERRS(16,mnpbuf,llmon,lldys,FLOAT(llyrs),             081298
     *                  FLOAT(llhrs),xdum,xdum,1)                       081298
              if (ifbtch .eq. 0) then                                   030499
                ier = KERRS(16,mnpbuf,llmon,lldys,FLOAT(llyrs),         081298
     *                      FLOAT(llhrs),xdum,xdum,0)                   081298
                if (ier .gt. 0) go to 150
              end if                                                    030499
              nther = nther + 1
              if (ifhsly .gt. 0) then
CC                source data is hourly, so must terminate.
                go to 150
              end if
            end if
            ifrddm = 0
            i = JINIT(kkyrs,6,iniyrs,1)
            i = JINIT(iniyrs,6,llyrs,1)
          else if (i .lt. 0) then
CC            current date less than expected date, too much data.
            call LOCAL(llyrs,llmon,lldys,llhrs,-100)
            ljdys = 0
            call JULAN(llyrs,llmon,lldys,ljdys)
            i = IOPUT(0,3,1,2)
            i = KERRS(17,mnpbuf,inimon,inidys,FLOAT(iniyrs),
     *                FLOAT(inihrs),xdum,xdum,1)
            if (ifbtch .eq. 0) then                                     030499
              ier = KERRS(17,mnpbuf,inimon,inidys,FLOAT(iniyrs),
     *                    FLOAT(inihrs),xdum,xdum,0)
              if (ier .gt. 0) go to 150
            end if                                                      030499
            nther = nther + 1
            ntothr = ntothr - 1
            wnddir = dirsav
            if (ifhsly .gt. 0) then
              go to 150
            end if
            go to 30
          end if
        end if
CC
CC        see if day is to be processed
   80   lflg = 0                                                        110198
        if (iscdys(injdys) .gt. 0) then
          if (ifcalc .ge. 0) then                                       110198
CC            if clearing index is less than clrinx, then no calc.
            if (clearx .le. clrinx.and.clrinx .gt. 0.0) then
CC              calculation flag off, due to too low clearing index.
              ifcalc = -1
            else if (clrinx .gt. 0.0.and.ifcalc .gt. 0) then
              lflg = 1                                                  110198
              nthrx = nthrx + 1
            end if
CC            if wind speed calm.
            if (refspd .lt. 1.0.and.ifhrly .gt. 0) then
              if (iswopt(10) .gt. 0) then
                ifclm = 2
                refspd = 1.0
                wnddir = dirsav
              else
CC                calculation flag off, due to calm wind.
                if (ifcalc .gt. 0) ifcalc = 0                           110198
                ifclm = 1
                nthcm = nthcm + 1
                if (iswopt(33) .ne. 0) then                             090500
                  i = IOPUT(0,3,1,2)
                  i = KERRS(18,mnpbuf,inimon,inidys,FLOAT(iniyrs),
     *                      FLOAT(inihrs),refspd,xdum,1)
                end if                                                  090500
                if (ifbtch .eq. 0) then                                 030499
                  ier = KERRS(18,mnpbuf,inimon,inidys,FLOAT(iniyrs),
     *                        FLOAT(inihrs),refspd,xdum,0)
                end if                                                  030499
              end if
            end if
            if (iswopt(27) .gt. 0) then
CC              restrict calcs. to day time hours.
              dum = geolat / 57.29578
              sinlat = SIN(dum)
              coslat = COS(dum)
              dum = geolon / 15.0 - zondif
CC              calculate the day no and the time of sunrise and sunset
              day1 = injdys
CC              constant 0.0172028 = 360.0 / 365.242 * 57.29578
CC              determine the angular(radians) fraction of a year for
CC              this date.
              dayno = (day1 - 1.0) * 0.0172028
              tdayno = 2.0 * dayno
              sind = SIN(dayno)
              cosd = COS(dayno)
              sintd = SIN(tdayno)
              costd = COS(tdayno)
CC              account for ellipticity of earth's orbit.
              sigma = 279.9348 + (dayno * 57.29578) + 1.914827 * sind -
     *                0.079525 * cosd + 0.019938 * sintd - 0.00162 *
     *                costd
CC              constant 0.39785=SIN(.4091720193=23.44383/57.29578)
CC              find the sine of the solar declination.
              dsin = 0.39785 * SIN(sigma / 57.29578)
              dcos = SQRT(1.0 - dsin * dsin)
CC              determine time(hrs) of meridian passage
              amm = 12.0 + 0.12357 * sind - 0.004289 * cosd +
     *              0.153809 * sintd + 0.060783 * costd
              hcos = (-sinlat * dsin) / (coslat * dcos)
CC              determine solar hour angle of sunrise-sunset.
              h2 = (ATAN2(SQRT(1.0 - hcos * hcos),hcos) / 15.0) *
     *             57.29578
CC              time of sunrise(tsr) and time of sunset(tss) are
CC              expressed in local standard time since the zone
CC              correction has already been made.  otherwise they
CC              would be in Greenwich mean time.
              isr = (amm - h2 + dum) + 0.5
              iss = (amm + h2 + dum) - 0.5
              if (iofhrs .eq. 0) then
                isr = isr + 1
                iss = iss + 1
              end if
              if (inihrs .lt. isr*100.or.inihrs .gt. iss*100) then
CC                calculation flag off, due to non-daytime hour.
                ifcalc = -1                                             110198
                if (clrinx .gt. 0.0.and.clearx .gt. clrinx.and.         110198
     *              lflg .gt. 0) then                                   110198
                  nthrx = nthrx - 1
                end if
              else
CC                Accumulate day-time hours                             110198
                nthrd = nthrd + 1
              end if
            end if
          end if
        else
          if (isumi(iscdys(injdys),nmxdys-injdys+1) .eq. 0.and.         031297
     *        ifanul .eq. 0.and.ifsean .eq. 0) then                     031297
            if (ifmode .le. 0.or.iswopt(19) .le. 0) then
              ntothr = ntothr - 1
              go to 100
            else
              ifcalc = -1                                               110198
            end if
          end if
        end if
      end if
CC      display current hour.
      ibak = ibackt
      ifor = iforet
      icol = 18
      irow = 2
      j = 2
      k = 1
      if (ifhrly .eq. 0) then
        dumy = 1
        if (ifbtch .gt. 0) dumy = nbtchc
      else
        dumy = nthtt
        if (iscdys(injdys) .ne. 0) then
          j = 1
          if (ilstd .ne. 1) then
            k = 1
          else
            k = 0
          end if
          ilstd = 1
          ifsth = .true.
        else
          if (ifsth) then
            j = 3
            if (ilstd .ne. 0) then
              k = 1
            else
              k = 0
            end if
            ilstd = 0
          else
            j = 4
            if (ilstd .ne. 0) then
              k = 1
            else
              k = 0
            end if
            ilstd = 0
          end if
        end if
      end if
      i = IWTBF(mbuf6,6,dumy,0,1)
CCPCITS
      call CRTYP(icoff)
CCPCITE
      if (ifhrly .gt. 0) then                                           011403
        ibak = ibackd                                                   011403
        ifor = ifored                                                   011403
        dumy = nthcm                                                    011403
        i = IWTBF(mbufs6,6,dumy,0,1)                                    011403
        mesgc(1)(17:22) = mbufs6                                        011403
        irowm = irow + 4                                                011403
        icolm = icol + 5                                                011403
        call IWRSC(icolm,irowm,22,mesgc(1),ibak,ifor)                   011403
        dumy = nthms                                                    011403
        i = IWTBF(mbufs6,6,dumy,0,1)                                    011403
        mesgc(2)(17:22) = mbufs6                                        011403
        irowm = irow + 3                                                011403
        call IWRSC(icolm,irowm,22,mesgc(2),ibak,ifor)                   011403
        dumy = nther                                                    011403
        i = IWTBF(mbufs6,6,dumy,0,1)                                    011403
        mesgc(3)(17:22) = mbufs6                                        011403
        irowm = irow + 2                                                011403
        call IWRSC(icolm,irowm,22,mesgc(3),ibak,ifor)                   011403
      end if                                                            011403
      if (k .eq. 1.or.(isystm .eq. 1.and.j .ne. 4)) then
        if (j .ne. 4) mesg1(j)(24:29) = mbuf6
        call IWRSC(icol,irow,31,mesg1(j),ibak,ifor)
      else if (j .ne. 4) then
        icol = icol + 23
        ibak = ibackd
        ifor = ifored
        i = NUMC1(mbuf6,6)
        call IWRSC(icol,irow,i,mbuf6,ibak,ifor)
      end if
CC     determine season or quarter.
      inimon = MAX0(inimon,1)
      iseasn = ISEAS(inimon,1)
      iquart = ISEAS(inimon,2)
CC>   if (iniyrs .lt. 50) then                                          020797
CC>     iniyrs = iniyrs + 2000                                          020797
CC>   else if (iniyrs .lt. 100) then                                    020797
CC>     iniyrs = iniyrs + 1900                                          020797
cC>   end if                                                            020797
CC      if hourly data and hour not missing.
      if (ifhrly .gt. 0.and.ifmss .eq. 0) then
CC        check for missing or out of range data.
        do 90 i=1,20
          if ((dparm(i) .lt. vllimt(i+5).or.dparm(i) .gt. vulimt(i+5))
     *        .and.dparm(i) .gt. rmissc) then
            if ((iswopt(11) .eq. 0.and.iscdys(injdys) .ne. 0).or.ifclm  031297
     *          .eq. 1) then                                            031297
CC              calculation flag off, due to missing or erroneous data
              if (ifcalc .gt. 0) ifcalc = 0                             110198
              nthms = nthms + 1
              ifmss = 1
              if (i .ne. 1.or.(i .eq. 1.and.ifclm .eq. 0)) then
                if (iscdys(injdys) .ne. 0) then
                  xdum = (dparm(i) / convrt(i,1)) * convrt(i,3) +
     *                    convrt(i,2)
                  if (iswopt(35) .ne. 0) then                           051503
                    k = ioput(0,3,1,3)
                    k = KERRS(19,mvars(i),inhrec,inimon,FLOAT(inidys),
     *                        FLOAT(iniyrs),FLOAT(inihrs),xdum,1)
                  end if                                                051503
                  if (ifbtch .eq. 0) then                               030499
                    k = KERRS(19,mvars(i),inhrec,inimon,FLOAT(inidys),
     *                        FLOAT(iniyrs),FLOAT(inihrs),xdum,0)
                  end if                                                030499
                end if
                nther = nther + 1
              end if
            else
              dparm(i) = datal(i)
            end if
          end if
   90   continue
CC        save current in last array.
        i = JRNIT(datal,20,dparm,1)
        if (iscdys(injdys) .eq. 0) ifcalc = -1
        if (iswopt(24) .gt. 0.and.iscdys(injdys) .gt. 0) then
CC          print hourly met. data.
          if (ntothr .eq. 1) then
            k = 1
          else
            k = 0
          end if
          i = IOPUT(k,3,1,8)
          j = radinx
          if (spdshr .gt. rmissc) then
            k = IWTBF(mbuf6,6,spdshr,2,0)
          else
            call MVCH1(mcalc,mbuf6,6)
          end if
          WRITE (iotfil,9001) iniyrs,inimon,inidys,inihrs+inimin,
     *                        injdys
          WRITE (iotfil,9009) refspd,mfprt,wdir,hmdpth,sigmap,sigmep,   051503
     *                        trbixr,                                   051503
     *                        tauosa,pwrlaw,airtmp,airprs,mbuf6,dphidz,
     *                        dirshr,istabl,j,roughl,airhum,clearx,
     *                        cldcov,cldhgt
        end if
      end if
      if (ifhsly .gt. 0.and.ifhrly .gt. 0) then
CC          read hourly source data.
          if (ISODA(mbufr,mbufr) .eq. 1) go to 150
      end if
CC      convert to radians.
      sigmaq = sigmap / constg(2)
      sigmeq = sigmep / constg(2)
      trbixq = trbixr / constg(2)
CC      adjust sigma-a for measurement time.
      sigmaq = PWLAW(sigmaq,tautmo,tauosa,0.2)
CC      adjust longitudinal turbulence intensity for measurement
CC      time.
      trbixq = PWLAW(trbixq,tautmo,tauosa,0.2)
      if (roughl .gt. 0.0) then
CC        adjust for roughness.
        sigmaq = PWLAW(sigmaq,roughl,10.0,0.2)
        sigmeq = PWLAW(sigmeq,roughl,10.0,0.15)
        trbixq = PWLAW(trbixq,roughl,10.0,0.2)
      end if
CC      Check to see if all sources are excluded                        110198
      i = inihrs                                                        110198
      if (i .ge. 100) i = i / 100                                       110198
      i = i + iofhrs                                                    110198
      j = 0                                                             110198
      do kssndx=1,nsourc                                                110198
        if (isofrq(i,kssndx) .ne. 0) j = j + 1                          110198
      end do                                                            110198
      if (j .eq. 0) ifcalc = -1                                         110198
CC      Accumulate excluded hours                                       110198
      if (ifcalc .lt. 0) then                                           031297
        nthex = nthex + 1                                               031297
      end if                                                            031297
CC      Save date/time and calculation flag.                            110198
      iswopt(7) = ISVDT(ifcalc,inihrs,inidys,inimon,iniyrs)
      if (ifhrly .gt. 0.and.ifcalc .gt. 0) then                         031297
CC        Accumulate calculated hours.                                  110198
        nthca = nthca + 1                                               031297
      end if                                                            031297
      if (iogfil .gt. 0) then
        if (ifmode .eq. 0) then
CC          detailed mode.
CC
CC          save current buffer.
          norec = iptrec
          ier = IORAN(iptrec,iptbuf,iogfil)
          if (ier .gt. 0) go to 130
          norec = istrec + 1
          ier = INRAN(norec,iptbuf,iogfil)
          if (ier .lt. 0) go to 120
CC          save current number of hours.
          iptbuf(1) = ntothr
          i = JINIT(iptbuf(2),4,nseasn,1)
          i = JINIT(iptbuf(6),4,nquart,1)
          ier = IORAN(norec,iptbuf,iogfil)
          if (ier .gt. 0) go to 130
CC          determine where to save the current date and record pointers
          norec = iptrec
          ier = INRAN(iptrec,iptbuf,iogfil)
          if (ier .lt. 0) go to 120
          iptrec = (((ntothr - 1) * (2 * nsourc + 1) + 9) + 127) / 128 +
     *             istrec
          ier = IOCPT(iptpnt,iptrec,iptbuf,1,iogfil,mognam)
          norec = iptrec
          if (ier .gt. 0) go to 140
          iptbuf(iptpnt) = iswopt(7)
          if (iseasn .gt. 0) nseasn(iseasn) = nseasn(iseasn) + 1
          if (iquart .gt. 0) nquart(iquart) = nquart(iquart) + 1
        else
CC          summary mode.
CC
CC          save current number of hours.
          norec = istrec + 1
          ier = INRAN(norec,ircbuf,iogfil)
          if (ier .lt. 0) go to 120
CC          save current number of hours.
          ircbuf(1) = ntothr
          i = JINIT(ircbuf(2),4,nseasn,1)
          i = JINIT(ircbuf(6),4,nquart,1)
          ier = IORAN(norec,ircbuf,iogfil)
          if (ier .gt. 0) go to 130
        end if
      end if
      go to 160
  100 ifnext = 14
      if (ifmode .eq. 0) then
CC          detailed mode.
CC
CC        write final pointer record.
        norec = iptrec
        ier = IORAN(iptrec,iptbuf,iogfil)
        if (ier .gt. 0) go to 130
CC        write final concentration record.
        norec = iogrec
        ier = IORAN(iogrec,ircbuf,iogfil)
        if (ier .gt. 0) go to 130
CC        update final number of hours.
        norec = istrec + 1
        ier = INRAN(norec,iptbuf,iogfil)
        if (ier .lt. 0) go to 120
CC        save final number of hours.
        iptbuf(1) = ntothr
        i = JINIT(iptbuf(2),4,nseasn,1)
        i = JINIT(iptbuf(6),4,nquart,1)
        ier = IORAN(norec,iptbuf,iogfil)
        if (ier .gt. 0) go to 130
      else
CC        summary mode.
CC
CC        update final number of hours.
        norec = istrec + 1
        ier = INRAN(norec,ircbuf,iogfil)
        if (ier .lt. 0) go to 120
CC        save final number of hours.
        ircbuf(1) = ntothr
        i = JINIT(ircbuf(2),4,nseasn,1)
        i = JINIT(ircbuf(6),4,nquart,1)
        ier = IORAN(norec,ircbuf,iogfil)
        if (ier .gt. 0) go to 130
      end if
      if (ifhrly .gt. 0) then
        j = 7                                                           031297
        if (clrinx .gt. 0.0) j = j + 1
        if (iswopt(27) .gt. 0) j = j + 1
        i = IOPUT(0,3,1,j)
        WRITE (iotfil,9010) nthtt,ntothr,nthca,nthms,vllimt(6),nthcm,   031297
     *                      nther,nthex                                 031297
        if (clrinx .gt. 0.0) then
          WRITE (iotfil,9011) clrinx,nthrx
        end if
        if (iswopt(27) .gt. 0) then
          WRITE (iotfil,9012) nthrd
        end if
        if (ifanul .gt. 0.and.ntothr .lt. 8760) then                    031297
          i = IOPUT(0,3,1,4)                                            031297
          WRITE (iotfil,9013) ntothr                                    031297
        end if                                                          031297
        if (ifsean .gt. 0) then                                         031297
          do i=1,4                                                      031297
            if (ifsean .eq. 1) then                                     031297
              if (nseasn(i) .lt. 2160) then                             031297
                j = IOPUT(0,3,1,4)                                      031297
                WRITE (iotfil,9014) i,nseasn(i)                         031297
              end if                                                    031297
            else                                                        031297
              if (nquart(i) .lt. 2160) then                             031297
                j = IOPUT(0,3,1,4)                                      031297
                WRITE (iotfil,9015) i,nquart(i)                         031297
              end if                                                    031297
            end if                                                      031297
          end do                                                        031297
        end if                                                          031297
      end if
CC      display compiling reports.
        j = ifbtch
        ifbtch = 0
        i = KERRS(35,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0)
        ifbtch = j
      go to 160
  110 ker = KERRS(1,mnhnam,ier,inhrec,xdum,xdum,xdum,xdum,0)            010400
      if ((ier .ge. 6099.and.ier .le. 6104).or.(ier .ge. 6205.and.      010400
     *ier .le. 6208)) then                                              010400
        WRITE (iotfil,9025)                                             010400
      end if                                                            010400
      j = KERRS(1,mnhnam,ier,inhrec,xdum,xdum,xdum,xdum,1)              010400
      if (ler .eq. 1) then                                              010400
        WRITE (iotfil,9016) (mscrch(i),i=1,100)                         010400
      else if (ler .eq. 2) then                                         010400
        WRITE (iotfil,9017) iniyrs,inimon,day,istab                     010400
        WRITE (iotfil,9018) awspd                                       010400
        WRITE (iotfil,9019) tempr                                       010400
        WRITE (iotfil,9020) afvnd                                       010400
        WRITE (iotfil,9021) afvrd                                       010400
        WRITE (iotfil,9022) hlhhm                                       010400
      else if (ler .eq. 3) then                                         010400
        WRITE (iotfil,9017) iniyrs,inimon,day,istab                     010400
      else if (ler .eq. 4) then                                         010400
        WRITE (iotfil,9018) awspd                                       010400
        WRITE (iotfil,9019) tempr                                       010400
        WRITE (iotfil,9020) afvnd                                       010400
        WRITE (iotfil,9021) afvrd                                       010400
        WRITE (iotfil,9022) hlhhm                                       010400
      else if (ler .eq. 5) then                                         010400
        WRITE (iotfil,9023) iyr(i),imo(i),idy(i),ihr(i),afvrd(i),       010400
     *                      awspd(i),tempr(i),istab(i),hmrur(i),        010400
     *                      hmurb(i)                                    010400
      else if (ler .eq. 6) then                                         010400
        WRITE (iotfil,9023) iyr(i),imo(i),idy(i),ihr(i),afvrd(i),       010400
     *                      awspd(i),tempr(i),istab(i),hmrur(i),        010400
     *                      hmurb(i)                                    010400
        WRITE (iotfil,9024) pwrlw(i),dthdz(i)                           010400
      else if (ler .eq. 7) then                                         010400
        WRITE (iotfil,9023) iyr(i),imo(i),idy(i),ihr(i),afvrd(i),       010400
     *                      awspd(i),tempr(i),istab(i),hmrur(i),        010400
     *                      hmurb(i)                                    010400
      end if                                                            010400
      if (ker .eq. 1) go to 160                                         010400
      go to 150
  120 if (KERRS(1,mognam,IABS(ier),norec,xdum,xdum,xdum,xdum,0) .eq. 1)
     *    go to 160
      go to 150
  130 if (KERRS(2,mognam,ier,norec,xdum,xdum,xdum,xdum,0)
     *    .eq. 1) go to 160
  140 if (ier .eq. 6422.or.ier. eq. 913.or.ier .eq. 956) then
        if (ifmode .eq. 0) then
          i = ((nmxhrs * (2 * nsourc + 1) + ntotal * (nsourc + 1) *
     *        nmxhrs / 2 + istrec * 128 + 9 - (norec - 1) * 128) *
     *        4 + 999) / 1000
        else
          i = (4 * (iogpnt * 128 + istrec + 4) + 999) / 1000
        end if
        i = KERRS(48,mognam,i,idum,xdum,xdum,xdum,xdum,0)
      end if
  150 ifnext = 3
      ifrdwt(5) = -1
      iferrs = 0
  160 return                                                            072497
CC
CC-DEBUGS
 9000 format (/' *-*-* entered RMDLA')
CC-DEBUGE
 9001 format (/' Year ',i4,', Month ',i2,', Day ',i2,', Hour ',i4.4,
     *', Julian Day ',i3,' -')
 9002 format (/' Source',34x,'Release',2x,'Cloud Rise',5x,'Total Cloud'/
     *' Number',15x,'Name',15x,'Height',5x,'Height',4x,'Height',3x,'Dist
     *ance'/1x,78('-'))
 9003 format (1x,i2,1x,36a1,3(1x,f6.1,2a1,1x),f7.1,2a1)
 9004 format (' *Cld Rise: Sor ',i2,' Ht=',f6.1,2a1,' Rise=',f7.1,2a1,
     *' Max Ht=',f7.1,2a1,' Dist=',f8.1,2a1)
 9005 format (a200)
 9006 format (2i4,f4.0,24i2)
 9007 format (8f10.3)

 9008 format (4i2,2f9.4,f6.1,i2,2f7.1,BZ,3f8.4,BN)
 9009 format (' Wind speed (m/s)=',f6.1,'; Wind dir (d)',a4,'=',f6.1,   051503
     *'; Mixing depth (m) =',f7.1,';'/' Turb (d) lateral=',f6.1,
     *', vertical        =',f6.1,', alongwind        =',f7.1,';'/
     *' Turb avg time(s)=',f6.1,'; Pwr law coeff.  =',f6.3,'; Temp. (C)
     *       =',f7.1,';'/' Pressure (mb)   =',f6.1,'; Wnd spd shr(m/s)='
     *,a6,'; Vert pot tmp(K/m)=',f6.4,';'/' Wnd dir shr(d/m)=',f6.1,
     *'; Stability cat   =',i6,'; Net rad. index   =',i6,';'/' Rough lng
     *th(cm) =',f6.1,'; Humidity %      =',f6.1,'; Clearing index   =',
     *f7.1,';'/' Cloud cov(8ths) =',f6.1,'; Cloud hgt (m)   =',f6.1,':')
 9010 format (/' Total data hours read ',46('-'),i7/                    031297
     *' Total data hours processed ',41('-'),i7/                        031297
     *' Total non-excluded/non-missing data hours (calculated) ',       031297
     *13('-'),i7/                                                       031297
     *' Total missing data hours (includes calms) ',26('-'),i7/         031297
     *' Total calm wind speed (< ',f4.1,' m/s) hours ',27('-'),i7/      031297
     *' Total data errors (out of range) ',35('-'),i7/                  031297
     *' Total user excluded data hours (zero emissions) ',20('-'),i7)   110198
 9011 format (' Total hours with clearing index > ',f6.1,1x,27('-'),i7) 031297
 9012 format (' Total daytime hours ',48('-'),i7)                       031297
 9013 format (/' Warning - User has specified annual average, but less t031297
     *han a year of data'/11x,'(',i4,' hrs) has been processed.  The res031297
     *ults may not represent an'/11x,'annual average.')                 031297
 9014 format (/' Warning - User has specified seasonal average, but seas031297
     *on ',i1,' has less than'/11x,'a season (',i4,' hrs) of data.  The 031297
     *results may not represent a'/11x,'seasonal average.')             031297
 9015 format (/' Warning - user has specified quarterly average, but qua031297
     *rter ',i1,' has less than'/11x,' a quarter (',i4,' hrs) of data.  031297
     *The results may not represent a'/11x,'quarterly average.')        031297
 9016 format (' Input Buffer =',64a1)                                   010400
 9017 format (' yr,mo,dy=',2i4,f4.0,' Stab=',24i2)                      010400
 9018 format (' Awspd=',12f6.1)                                         010400
 9019 format (' tempr=',12f6.1)                                         010400
 9020 format (' afvnd=',12f6.1)                                         010400
 9021 format (' afvrd=',12f6.1)                                         010400
 9022 format (' hlhhm=',12f6.1)                                         010400
 9023 format (' yr,mo,dy,hr=',4i4,' afvrd=',f9.4,' awspd=',f9.4/        010400
     *' tempr=',f6.1,' istab=',i2,' hmrur=',f7.1,' hmurb=',f7.1)        010400
 9024 format (' pwrlw=',f8.4,' dthdz=',f8.4)                            010400
 9025 format (/' Possible data format error.')                          010400
      end
      Function IUSDA(mbufr,xdumy,mvars)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IUSDA Version 01.2                          ---
CC--- Subroutine description - reads chronologically ordered user   ---
CC---                          specified hourly meteorological data.---
CC---------------------------------------------------------------------
CC
CC      mbufr - character buffer containing meteorological data record.
CC      xdumy - dumy array for the input of met. values.
CC      mvars - names of met. variables.
CC
CC      returns -
CC         IUSDA  - returned status of met data.
CC                    0 - no errors.
CC                    1 - fatal read error.
CC                    2 - error in time or data.
CC                   <0 - number of missing or out of range values.
CC
CC      via common -
CC         iniyrs - year 19yy
CC         inimon - month mm
CC         inidys - day dd
CC         inihrs - hour hh00
CC         injdys - julian day jjj
CC         refspd - wind speed (m/s)
CC         pwrlaw - wind speed power law coefficient (unitsless)
CC         wnddir - wind direction (deg)
CC         sigmap - lateral turbulent intensity (rad)
CC         sigmep - vertical turbulent intensity (rad)
CC         trbixr - longitudinal turbulent intensity (rad)
CC         airtmp - air temperature (deg c)
CC         airprs - air pressure (mb)
CC         dphidz - vert. pot. temp. gradient (deg c/m)
CC         hmdpth - mixing layer depth (m)
CC         spdshr - wind speed shear (m/s)
CC         dirshr - wind direction shear (deg/m)
CC         istabl - Pasquill stability category (1 to 6)
CC         radinx - net radiation index (-2 to 4)
CC         tauosa - measurement time for lateral turbulent intensity (s)
CC         roughl - roughness length (cm)
CC         airhum - humidity (% or fraction)
CC         clearx - clearing index.
CC         cldcov - cloud cover
CC         cldhgt - ceiling height
CC
CC**********************************************************************
CC
      integer*2 i,j,ii
CC
      character*1 mdumy
      character*5 mdate
      character*22 mvars,mansw
      character*150 mform
      character*200 mbufr
CC
      dimension xdumy(100),idate(5),dparm(20),mvars(20),space(350),
     *          mdate(6)
CC
      include 'OCNTRL.INC'
      include 'OCDSPM.INC'
      include 'ONAMES.INC'
CC
      equivalence (mform,mformt(1)),(idate(1),iniyrs),(refspd,dparm(1)),
     *            (mansw,mnpbuf),(ilinem(1),space(1)),(nther,space(311))
CC
      data mdate/'year ','month','day  ','hour','Julia','n day'/
CC
CC**********************************************************************
CC
CC      initialize return status.
      IUSDA = 0
      if (ihrfmt .eq. 0) then
CC        data are free field.
        imxch = 0
        imxwd = 100
        ier = IFSRD(mbufr,200,mdumy,xdumy,imxch,imxwd,-30,              021405
     *                 rmissd,0,rmissd)
        if (ier .lt. 0) then                                            030797
CC          read error.
          iferrs = KERRS(1,mnhnam,IABS(ier),inhrec,xdum,xdum,xdum,      030797
     *                   xdum,0)                                        030797
          IUSDA = 1
        end if
      else
CC        data are fixed field.
        READ (mbufr,mform,iostat=ier,err=10) (xdumy(i),i=1,nformt)
        go to 20
CC        read error.
   10   iferrs = KERRS(1,mnhnam,ier,inhrec,xdum,xdum,xdum,xdum,0)
        IUSDA = 1
      end if
   20 iferrs = 0
      if (IUSDA .eq. 0) then
CC
CC        get date/time.
        j = 0
        do 30 i=1,5
          if (ivarmt(i) .gt. 0) then
            if (xdumy(ivarmt(i)) .lt. 0.0.or.xdumy(ivarmt(i)) .gt.
     *          3000) then
              iferrs = KERRS(39,mnpbuf,inhrec,idum,xdumy(ivarmt(i)),
     *                       xdum,xdum,xdum,0)
              nther = nther + 1
              IUSDA = 2
              idate(i) = 0
              go to 60
            else
              idate(i) = xdumy(ivarmt(i))
            end if
            if (i .eq. 1) then
              if (idate(1) .lt. 25) then
                idate(1) = idate(1) + 2000
              else if (idate(1) .lt. 100) then
                idate(1) = idate(1) + 1900
              end if
            else if (i .eq. 4) then
              if (idate(4) .ge. 100) then
                ii = (idate(4) / 100) * 100
                inimin = idate(4) - ii
                idate(4) = ii
              else
                idate(4) = idate(4) * 100
                inimin = 0
              end if
            end if
            if (idate(i) .lt. vllimt(i).or.idate(i) .gt. vulimt(i)) then
              mansw = mdate(i)
              if (i .eq. 5) mansw(6:11) = mdate(i+1)
              k = KERRS(19,mansw,inhrec,inimon,FLOAT(inidys),
     *                  FLOAT(iniyrs),FLOAT(inihrs),FLOAT(idate(i)),0)
              nther = nther + 1
              if (iotfil .gt. 0) then
                k = IOPUT(0,3,1,3)
                write (iotfil,9000) mansw,FLOAT(idate(i)),inhrec,
     *                              inimon,inidys,iniyrs,inihrs
              end if
              IUSDA = 2
              go to 60
            end if
          else
            if (i .eq. 4) then
              inihrs = inihrs + 100
              if (inihrs .gt. vulimt(4)) then
                inihrs = vllimt(4)
                if (ivarmt(5) .eq. 0) injdys = injdys + 1
              end if
            end if
          end if
   30   continue
CC        set month and day or julian day.
        if (ivarmt(5) .gt. 0) then
          call JULAN(iniyrs,inimon,inidys,injdys)
        else if (ivarmt(2) .gt. 0.and.ivarmt(3) .gt. 0) then
          injdys = 0
          call JULAN(iniyrs,inimon,inidys,injdys)
        end if
CC
CC        get met. parameters.
        do 40 i=1,20
          j = ivarmt(i+5)
          if (j .gt. 0) then
            dparm(i) = convrt(i,1) * (xdumy(j) - convrt(i,2))/
     *                 convrt(i,3)
            if (i .eq. 13) then
CC              set G stability to F.
              if (IFEQU(xdumy(j),7.0) .eq. 0)  then
                xdumy(j) = 6.0
                dparm(i) = xdumy(j)
              end if
CC              limit stability cat. to vulimt.
              if (xdumy(j) .lt. vllimt(i+5).or.xdumy(j) .gt.
     *            vulimt(i+5)) then
                if (iscdys(injdys) .gt. 0) then
                  k = KERRS(19,mvars(i),inhrec,inimon,FLOAT(inidys),
     *                      FLOAT(iniyrs),FLOAT(inihrs),xdumy(j),0)
                  nther = nther + 1
                  k = IOPUT(0,3,1,3)
                  write (iotfil,9000) mvars(i),xdumy(j),inhrec,inimon,
     *                                inidys,iniyrs,inihrs
                end if
                if (xdumy(j) .lt. vllimt(i+5)) then
                  xdumy(j) = vllimt(i+5)
                else
                  xdumy(j) = vulimt(i+5)
                end if
                dparm(i) = xdumy(j)
                if (iscdys(injdys) .gt. 0) then
                  write (iotfil,9001) mvars(i),xdumy(j)
                end if
              end if
              istabl = xdumy(j)
            end if
          end if
   40   continue
CC        check for parameters to default.
        if (ivarmt(18) .lt. 0.or.ivarmt(19) .lt. 0) then
          if (ivarmt(24) .gt. 0.and.ivarmt(25) .gt. 0.and.zondif .ge.
     *        -12.0.and.geolon .ge. -180.0.and.geolat .ge. -90.0)
     *        then
CC            determine sun angle.
            snang = SUNAN(inihrs,inimin,zondif,ifdstm,injdys,geolon,
     *                    geolat)
            dparm(14) = IRNDX(cldcov,cldhgt,snang)
            istabl = IPSQL(radinx,refspd)
            stblty = istabl
          else if (ivarmt(18) .lt. 0.and.ivarmt(19) .gt. 0) then
            istabl = IPSQL(radinx,refspd)
            stblty = istabl
          else if (ivarmt(18) .gt. 0.and.ivarmt(19) .lt. 0) then
            istabl = stblty
            radinx = NETRX(istabl,refspd)
          end if
        end if
        ydumy = 0.0
        do 50 i=1,20
          j = ivarmt(i+5)
          if (j .lt. 0) then
            if (i .eq. 2.and.refspd .gt. 0.0) then
CC              power law.
              pwrlaw = DFMET(1,radinx,istabl,refspd,0.0)
            else if (i .eq. 4.and.refspd .gt. 0.0) then
CC              lateral turb.
              sigmap = DFMET(3,radinx,istabl,refspd,0.0)
            else if (i .eq. 5.and.refspd .gt. 0.0) then
CC              vertical turb.
              sigmep = DFMET(2,radinx,istabl,refspd,0.0)
            else if (i .eq. 6) then
CC              longitudinal turb.
              trbixr = 1.33 * sigmap
            else if (i .eq. 9.and.refspd .gt. 0.0) then
CC              vert. pot. temp. grad.
              dphidz = DFMET(5,radinx,istabl,refspd,airhum)
            else if (i .eq. 10.and.refspd .gt. 0.0) then
CC              mixing depth.
              hmdpth = DFMET(4,radinx,istabl,refspd,0.0)
CC>            cannot calculate clearing index in this version.
CC>         else if (i .eq. 18.and.refspd .gt. 0.0) then
CC>             clearing index.
CC>           sp = AMIN1(WSBAR(2.0,hmdpth,refspd,refhgt,pwrlaw,refspd),
CC>  *                   vulimt(6))
CC>           clrinx = (hmdpth * 3.28084 * sp * 1.943844) / 100.0
            end if
          end if
   50   continue
      end if
   60 return
 9000 format (' Warning, ',a22,' out of range = ',g12.6,' at block ',i5/
     *' on ',2(i2.2,'/'),i4,' at ',i4.4)
 9001 format (1x,a22,' defaulted to ',g12.6)
      end
      Function ISODA(mbufr,mbufs)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - ISODA Version 01.1                          ---
CC--- Subroutine description - reads chronologically ordered user   ---
CC---                          specified hourly source data.        ---
CC---------------------------------------------------------------------
CC
CC      mbufr - character buffer for input source data record.
CC      mbufs - character array for print output of source data units.
CC
CC      returns -
CC         ISODA  - returned status of met data.
CC                   0 - no errors.
CC                   1 - fatal read error.
CC                  <0 - number of missing or out of range values.
CC
CC      via common - depending on ivarso.
CC
CC      qemiss - total material or burn rate.
CC      hetcnt - material heat content.
CC      brnrat - burn rate.
CC      taucld - burn time.
CC
CC**********************************************************************
CC                                                                      111098
      character*1 mdumy                                                 111098
      character*5 mbufs
      character*22 mvarn                                                111098
      character*150 mform
      character*200 mbufr
CC
      dimension xdumy(100),mvarn(4),mbufs(3),vdumy(3)                   111098
CC
      include 'ONAMES.INC'
      include 'OCNTRL.INC'
      include 'OCDSPM.INC'
      include 'OCALCS.INC'
CC
      equivalence (mform,mforms(1))
CC
      data mvarn/'Source number         ',
     *           'Emission strength     ',
     *           'Fuel heat content     ',
     *           'Fuel burn rate        '/
CC                                                                      111098
CC**********************************************************************
CC
      if (iswopt(26) .gt. 0) then
CC        Print hourly source data.
        if (iswopt(24) .le. 0) then
          if (ntothr .eq. 1) then
CC            New page.
            ks = 1
          else
            ks = 0
          end if
        else
          ks = 0
        end if
CC          Adjust for hour display.
        if (iswopt(24) .le. 0) then
          js = 2                                                        111098
        else                                                            111098
          js = 0                                                        111098
        end if                                                          111098
      end if
CC      Loop over sources.
      ISODA = 0
      do 90 kssndx=1,nsourc
CC        Initialize status.
        insrec = insrec + 1
CC        Read source record.
        READ (ishfil,9000,iostat=ier,err=10,end=10) mbufr
        if (ihsfmt .eq. 0) then
CC          Data are free field.
          imxch = 0
          imxwd = 100
          ier = IFSRD(mbufr,200,mdumy,xdumy,imxch,imxwd,-30,            021405
     *                rmissd,0,rmissd)                                  111098
          if (ier .lt. 0) then                                          111098
CC            Read error.
            iferrs = KERRS(1,mshnam,IABS(ier),insrec,xdum,xdum,xdum,    030797
     *                     xdum,0)                                      030797
            ISODA = 1
            go to 100
          end if
        else
CC          Data are fixed field.
          READ (mbufr,mform,iostat=ier,err=10) (xdumy(i),i=1,nforms)
        end if
        go to 20
CC        Read error.
   10   iferrs = KERRS(1,mshnam,ier,insrec,xdum,xdum,xdum,xdum,0)
        ISODA = 1
        go to 100
CC
CC        Get source parameters.
   20   j = 1
CC        Source number.
        kss = xdumy(ivarso(1))
        ddumy = 0.0
        if (kss .le. 0.or.kss .gt. nsourc) go to 70
   30   j = 2                                                           111098
CC        Total emission mass.
        iunit = IVGET(iqunit(kss),2)
        junit = IVGET(iqunit(kss),1)
        if (iswopt(26) .gt. 0) then
          call UNITS(mbufs(1),5,iunit,jdumy,0,1,3)                      111098
        end if                                                          111098
        if (ivarso(2) .gt. 0) then
CC          Convert to grams.
          qemiss(kss) = xdumy(ivarso(2)) * constm(iunit+1)
          vdumy(1) = xdumy(ivarso(2))                                   111098
        else                                                            111098
          vdumy(1) = qemiss(kss) / constm(iunit+1)                      111098
        end if
   40   j = 3
CC        Fuel heat content.
        iunit = IVGET(jqunit(kss),1)
        junit = IVGET(jqunit(kss),2)
        if (iswopt(26) .gt. 0) then
          call UNITS(mbufs(2),5,iunit,junit,0,1,13)                     111098
        end if                                                          111098
        if (ivarso(3) .gt. 0) then
CC          Convert to calories per gram.
          hetcnt(kss) = xdumy(ivarso(3)) * consth(iunit+1) /
     *                  constm(junit+1)
          vdumy(2) = xdumy(ivarso(3))
        else
          vdumy(2) = hetcnt(kss) * constm(junit+1) / consth(iunit+1)    111098
        end if
   50   j = 4
CC        Fuel burn rate.
        iunit = IVGET(kqunit(kss),1)
        junit = IVGET(kqunit(kss),2)
        if (iswopt(26) .gt. 0) then
          call UNITS(mbufs(3),5,iunit,junit,0,1,12)                     111098
        end if                                                          111098
        if (ivarso(4) .gt. 0) then
CC          Convert to grams per second.
          brnrat(kss) = xdumy(ivarso(4)) * constm(iunit+1) /
     *                    constt(junit+1)
          vdumy(3) = xdumy(ivarso(4))                                   111098
        else                                                            111098
          vdumy(3) = brnrat(kss) * constt(junit+1) / constm(iunit+1)    111098
        end if
CC        Burn time.                                                    111098
   60   if (ivarso(2) .gt. 0.or.ivarso(4) .gt. 0) then
          if (qemiss(kss) .le. 0.0.or.brnrat(kss) .le. 0.0) then
            taucld(kss) = 0.0                                           021405
          else                                                          021405
            taucld(kss) = AMAX1(qemiss(kss) / brnrat(kss),2.5)
          end if                                                        021405
          taucld(kss) = AMAX1(taucld(kss),2.5)                          021405
        end if
        go to 80
   70   ISODA = ISODA - 1
        if (iotfil .gt. 0) then
          i = IOPUT(0,2,5,4)
          i = KERRS(19,mvarn(j),insrec,inimon,FLOAT(inidys),
     *              FLOAT(iniyrs),FLOAT(inihrs),xdumy(ivarso(j)),1)
          ier = KERRS(19,mvarn(j),insrec,inimon,FLOAT(inidys),
     *                FLOAT(iniyrs),FLOAT(inihrs),xdumy(ivarso(j)),0)
        end if                                                          111098
        if (j .eq. 1) then
          ISODA = 1
          go to 100
        else
          if (ifrdwt(5) .ge. 0) ifrdwt(5) = 2
          go to (30,40,50,60),j
        end if
   80   if (iswopt(26) .gt. 0) then
CC          print hourly source data.
          k = IOPUT(ks,6,1,js+1)                                        111098
          ks = 0                                                        111098
          if (k .gt. 0.or.kssndx .eq. 1) then
            if (iswopt(24) .le. 0) then
CC              new page or hour display not printed.
              WRITE (iotfil,9001) iniyrs,inimon,inidys,inihrs+inimin,
     *                            injdys
              if (js .eq. 0) llines = llines + 2
            end if
          end if
CC          write source data.
          WRITE (iotfil,9002) kss,(vdumy(i),mbufs(i),i=1,3)             111098
          js = 0                                                        111098
        end if                                                          111098
   90 continue
  100 return
CC
 9000 format (a200)
 9001 format (/' year ',i4,', month ',i2,', day ',i2,', hour ',i4.4,
     *',Julian day ',i3,' -')
 9002 format (' Sor=',i2,' Emiss=',g12.6,a5,', Heat=',g11.5,a5,         111098
     *', Rate=',g12.6,a5)                                               111098
      end
