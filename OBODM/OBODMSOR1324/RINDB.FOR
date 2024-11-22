      Subroutine RINDB
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - RINDB Version 01.3                          --- 072497
CC--- Subroutine description - get initializing data from saved     ---
CC---                          data files.                          ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      include 'OCNTRL.INC'
      include 'ONAMES.INC'
      include 'OCALCS.INC'
      include 'OWNTRL.INC'
CC
      equivalence (ifrsd,functx(2)),(isav1,functx(3)),(isav2,functx(4))
CC
CC**********************************************************************
CC
      if (ifrsd .ne. 0) then
        i = JINIT(iferrm,nmxlin*nmxfld,0,0)
        call GETD5(mscrnm,ilinem,itabsm,ispacm,iferrm)
        call GETD6(mscrnm,ilinem,itabsm,ispacm,iferrm)
        if (ifrsav .eq. 1) then
          iogfil = isav1
          igtfil = isav2
          ifrsav = 0
          ifnext = 3
          go to 10
        end if
        llines = 80
        ipages = 0
        itable = 0
        if (ifbtch .gt. 0) then
          ifnext = 9
        else
          ifnext = 3
        end if
      end if
   10 return
      end
      Subroutine GETD5(mscrn,iline,itabs,ispac,iferr)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - GETD5 Version 01.1                          ---
CC--- Subroutine description - reads OBODM save data files.         ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      character*1 mscrn
      character*58 mfnam
      character*80 mbufr
CC
      dimension mscrn(80,25),iline(20),itabs(20,10),ispac(20,10),
     *          iferr(20,10)
CC
      include 'OCNTRL.INC'
      include 'ONAMES.INC'
      include 'OCDSPM.INC'
      include 'OCALCS.INC'
CC
      equivalence (mfnam,mscrch(1)),(mbufr,mnpbuf(1))
CC
CC**********************************************************************
CC
CC        read initial data from save files.
CC
      if (igtfil .gt. 0) then
CC
CC        place meteorological data in work file.
        numsc = numscs(6)
        numf = IRDSC(numsc,1,mscrn,iline,itabs,ispac,iferr,0)
CC        if hourly
        nvl = IWHER(1,iline,itabs,ispac,0,1,nln,nsc,nch)
        if (ifhrly .eq. 0) then
          mscrn(nsc,nln) = malpha(19)
        else
          mscrn(nsc,nln) = malpha(8)
        end if
        i = IWTSC(numsc,1,mscrn,iline,iferr)
        numsc = numscs(6) + 2
        numf = IRDSC(numsc,1,mscrn,iline,itabs,ispac,iferr,0)
CC        wind speed units.
        nvl = IWHER(1,iline,itabs,ispac,0,1,nln,nsc,nch)
        call UNITS(mscrn(nsc,nln),nch,ispunt,jspunt,0,1,7)
CC        wind speed.
        nsc = JWHER(nvl,itabs,ispac,2,nch)
        i = IWTBF(mscrn(nsc,nln),nch,refspd,3,iflrjs)
CC        wind direction.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        i = IWTBF(mscrn(nsc,nln),nch,wnddir,2,iflrjs)
CC          Is wind direction From or Towards                           051503
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)              051503
        if (iswopt(34) .eq. 0) then                                     051503
          mscrn(nsc,nln) = malpha(06)                                   051503
        else                                                            051503
          mscrn(nsc,nln) = malpha(20)                                   051503
        end if                                                          051503
CC        Pasquill stability cat. or net radiation index specified.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        if (istabl .gt. 0.and.istabl .le. 6.and.iswopt(9) .ne. 0) then
          if (iflrjs .eq. 0) then
            mscrn(nsc+nch-1,nln) = malpha(istabl)
          else
            mscrnm(nsc,nln) = malpha(istabl)
          end if
        else if (radinx .ge. -2.and.radinx .le. 4) then
          i = IWTBF(mscrn(nsc,nln),nch,radinx,0,iflrjs)
        end if
CC        year.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
          if (iniyrs .gt. 0) then
            xdumy = iniyrs                           
            i = IWTBF(mscrn(nsc,nln),nch,xdumy,0,iflrjs)
          end if                                        
CC        month of met.
        if (inimon .gt. 0) then
          nsc = JWHER(nvl,itabs,ispac,2,nch)
          xdumy = inimon
          i = IWTBF(mscrn(nsc,nln),nch,xdumy,0,iflrjs)
        end if
CC        day of met.
        if (inidys .gt. 0) then
          nsc = JWHER(nvl,itabs,ispac,3,nch)
          xdumy = inidys
          i = IWTBF(mscrn(nsc,nln),nch,xdumy,0,iflrjs)
        end if
CC        julian day of met.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        if (injdys .gt. 0) then
          xdumy = injdys
          i = IWTBF(mscrn(nsc,nln),nch,xdumy,0,iflrjs)
        end if
CC        units of hour
        if (ifdstm .eq. 0.or.ifdstm .eq. -1) then
          nsc = JWHER(nvl,itabs,ispac,2,nch)
          call UNITS(mscrn(nsc,nln),nch,IABS(ifdstm),jdumy,0,1,16)
        end if
CC        hour of met. data.
        if (inihrs .ge. 0) then
          nsc = JWHER(nvl,itabs,ispac,3,nch)
          xdumy = inihrs + inimin
          i = IWTBF(mscrn(nsc,nln),nch,xdumy,-4,iflrjs)
        end if
CC        west longitude.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        if (geolon .ge. -180.0) then
          i = IWTBF(mscrn(nsc,nln),nch,geolon,7,iflrjs)
        end if
CC            north latitude.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        if (geolat .ge. -90.0) then
          i = IWTBF(mscrn(nsc,nln),nch,geolat,7,iflrjs)
        end if
CC        time zone difference between site and Greenwich England.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        if (zondif .ge. -12.0) then
          i = IWTBF(mscrn(nsc,nln),nch,zondif,0,iflrjs)
        end if
CC        cloud cover, 8'ths of celestial dome.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        if (cldcov .ge. 0.0) then
          i = IWTBF(mscrn(nsc,nln),nch,cldcov,0,iflrjs)
        end if
CC        cloud ceiling units.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        if (cldhgt .ge. 0.0) then
          call UNITS(mscrn(nsc,nln),nch,iceunt,jdumy,0,1,0)
CC          cloud ceiling.
          nsc = JWHER(nvl,itabs,ispac,2,nch)
          i = IWTBF(mscrn(nsc,nln),nch,cldhgt,0,iflrjs)
        end if
        i = IWTSC(numsc,1,mscrn,iline,iferr)
        numsc = numscs(6) + 3
        numf = IRDSC(numsc,1,mscrn,iline,itabs,ispac,iferr,0)
CC        met. site base elevation units.
        nvl = IWHER(1,iline,itabs,ispac,0,1,nln,nsc,nch)
        call UNITS(mscrn(nsc,nln),nch,ihunit,jdumy,0,1,0)
CC        met. site base elevation.
        nsc = JWHER(nvl,itabs,ispac,2,nch)
        i = IWTBF(mscrn(nsc,nln),nch,hgtmet,1,iflrjs)
CC        wind speed power law coefficient.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        i = IWTBF(mscrn(nsc,nln),nch,pwrlaw,4,iflrjs)
CC        units of std. dev. of the wind direction measurement time.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        call UNITS(mscrn(nsc,nln),nch,istunt,jdumy,0,1,5)
CC        measurement time of std. dev. of the wind direction angle.
        nsc = JWHER(nvl,itabs,ispac,2,nch)
        i = IWTBF(mscrn(nsc,nln),nch,tauosa,2,iflrjs)
CC        units of standard deviation of the wind direction angle.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        call UNITS(mscrn(nsc,nln),nch,isyunt,jdumy,0,1,1)
CC        standard deviation of the wind direction angle.
        nsc = JWHER(nvl,itabs,ispac,2,nch)
        i = IWTBF(mscrn(nsc,nln),nch,sigmap,7,iflrjs)
CC        units of standard deviation of the wind elevation angle.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        call UNITS(mscrn(nsc,nln),nch,iszunt,jdumy,0,1,1)
CC        standard deviation of the wind elevation angle.
        nsc = JWHER(nvl,itabs,ispac,2,nch)
        i = IWTBF(mscrn(nsc,nln),nch,sigmep,7,iflrjs)
CC        units of standard deviation of the wind longitudinal angle.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        call UNITS(mscrn(nsc,nln),nch,isxunt,jdumy,0,1,1)
CC        standard deviation of the wind longitudinal angle.
        nsc = JWHER(nvl,itabs,ispac,2,nch)
        i = IWTBF(mscrn(nsc,nln),nch,trbixr,7,iflrjs)
CC        units of mixing layer depth.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        call UNITS(mscrn(nsc,nln),nch,ihmunt,jdumy,0,1,0)
CC        mixing layer depth.
        nsc = JWHER(nvl,itabs,ispac,2,nch)
        i = IWTBF(mscrn(nsc,nln),nch,hmdpth,1,iflrjs)
CC        air humidity.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        i = IWTBF(mscrn(nsc,nln),nch,airhum,2,iflrjs)
CC        units of vertical gradient of potential temperature.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        call UNITS(mscrn(nsc,nln),nch,ivgunt,jvgunt,0,1,15)
CC        vertical gradient of potential temperature.
        nsc = JWHER(nvl,itabs,ispac,2,nch)
        i = IWTBF(mscrn(nsc,nln),nch,dphidz,6,iflrjs)
        i = IWTSC(numsc,1,mscrn,iline,iferr)
        numsc = numscs(6) + 4
        numf = IRDSC(numsc,1,mscrn,iline,itabs,ispac,iferr,0)
CC        wind speed shear units.
        nvl = IWHER(0,iline,itabs,ispac,1,1,nln,nsc,nch)
        call UNITS(mscrn(nsc,nln),nch,issunt,jssunt,0,1,7)
CC        wind speed shear.
        nsc = JWHER(nvl,itabs,ispac,2,nch)
        i = IWTBF(mscrn(nsc,nln),nch,spdshr,4,iflrjs)
CC        wind direction shear units.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        call UNITS(mscrn(nsc,nln),nch,idsunt,jdsunt,0,1,14)
CC        wind direction shear.
        nsc = JWHER(nvl,itabs,ispac,2,nch)
        i = IWTBF(mscrn(nsc,nln),nch,dirshr,4,iflrjs)
CC        air temperature units.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        call UNITS(mscrn(nsc,nln),nch,itpunt,jdumy,0,1,6)
CC        air temperature.
        nsc = JWHER(nvl,itabs,ispac,2,nch)
        i = IWTBF(mscrn(nsc,nln),nch,airtmp,2,iflrjs)
CC        roughness length units.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        call UNITS(mscrn(nsc,nln),nch,irount,jdumy,0,1,0)
CC        roughness length.
        nsc = JWHER(nvl,itabs,ispac,2,nch)
        i = IWTBF(mscrn(nsc,nln),nch,roughl,4,iflrjs)
CC        units of air pressure.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        call UNITS(mscrn(nsc,nln),nch,iprunt,jdumy,0,1,4)
CC        air pressure.
        nsc = JWHER(nvl,itabs,ispac,2,nch)
        i = IWTBF(mscrn(nsc,nln),nch,airprs,2,iflrjs)
CC      
        i = IWTSC(numsc,1,mscrn,iline,iferr)
CC        if hourly input data .
        if (ifhrly .ne. 0) then
          numsc = numscs(6) + 5
          numf = IRDSC(numsc,1,mscrn,iline,itabs,ispac,iferr,0)
CC          name of input met. data file.
          nvl = IWHER(1,iline,itabs,ispac,0,1,nln,nsc,nch)
          call MBLK1(mscrn(nsc,nln),nch)
          call MVCH1(mnhnam,mscrn(nsc,nln),MIN0(nch,22))
          i = LRJST(mscrn(nsc,nln),nch,iflrjs)
          if (NUMC1(mnhnam,22) .le. 0) go to 20                         030499
CC          open data file as logical units inhfil.
          inhfil = 27
CC          combine file name and directory.
          call FNAME(mfnam,58,mdirec,mnhnam)
          CLOSE (unit=inhfil,status='keep',err=10)
   10     if (ihrfmt .eq. 2) then
CC            unformatted binary.
            OPEN (unit=inhfil,file=mfnam,iostat=ier,err=20,
     *            status='old',form='unformatted')
          else
CC            formatted character.
            OPEN (unit=inhfil,file=mfnam,iostat=ier,err=20,
     *           status='old',form='formatted')
          end if
          go to 30
   20     iferr(nvl,1) = 1
          ifrdwt(4) = -1
          inhfil = 0                                                    030499
          if (KERRS(3,mnhnam,ier,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *        go to 70
          
CC          data file format.
   30     nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
          if (ihrfmt .ge. 0) mscrn(nsc,nln) = malpha(ihrfmt+1)
CC          treatment of calms.
          nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
          if (iswopt(10) .eq. 0) then
            mscrn(nsc,nln) = malpha(14)
          else
            mscrn(nsc,nln) = malpha(19)
          end if
CC          treatment of missing data.
          nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
          if (iswopt(11) .eq. 0) then
            mscrn(nsc,nln) = malpha(14)
          else
            mscrn(nsc,nln) = malpha(4)
          end if
CC          minimum of 75% or actual count for short term averages
          nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
          if (iswopt(29) .eq. 0) then
            mscrn(nsc,nln) = malpha(13)
          else
            mscrn(nsc,nln) = malpha(1)
          end if
CC          save screen
          i = IWTSC(numsc,1,mscrn,iline,iferr)
CC          year of hourly data.
          numsc = numscs(6) + 6
          numf = IRDSC(numsc,1,mscrn,iline,itabs,ispac,iferr,0)
CC          year of met. data.
          nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
          xdumy = iscyrs
          i = IWTBF(mscrn(nsc,nln),nch,xdumy,0,iflrjs)
CC            if rural or urban mixing layer depths.
          nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
          if (ihrfmt .gt. 1) then
            if (ischmd .eq. 0) then
CC              rural.
              mscrn(nsc,nln) = malpha(18)
            else
CC              urban.
              mscrn(nsc,nln) = malpha(21)
            end if
          end if
          k = 0
          m = 0
          do 60 j=1,6
            nvl = IWHER(j,iline,itabs,ispac,3,1,nln,nsc,nch)
            do 50 i=1,2
              m = m + 1
              nsc = JWHER(nvl,itabs,ispac,i,nch)
              l = NMXDY(iscyrs,m)
              do 40 n=1,l
                k = k + 1
                if (iscdys(k) .eq. 1) then
                  mscrn(nsc+n-1,nln) = malpha(25)
                else
                  mscrn(nsc+n-1,nln) = mblnk1
                end if
   40         continue
   50       continue
   60     continue
          i = IWTSC(numsc,1,mscrn,iline,iferr)
        end if
      end if
   70 return
      end
      Subroutine GETD6(mscrn,iline,itabs,ispac,iferr)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - GETD6 Version 01.1                          --- 082497
CC--- Subroutine description - reads OBODM save data files.         ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      character*1 mscrn
      character*58 mfnam
      character*80 mbufr
CC
      dimension mscrn(80,25),iline(20),itabs(20,10),ispac(20,10),
     *          iferr(20,10)
CC
      include 'OCNTRL.INC'
      include 'ONAMES.INC'
      include 'OCDSPM.INC'
      include 'OCALCS.INC'
CC
      equivalence (mbufr,mnpbuf(1))
CC
CC**********************************************************************
CC
CC        set initial data from save files.
      if (igtfil .gt. 0) then
        if (ifhrly .gt. 0) then
CC          if user specified.
          if (ihrfmt .le. 1) then
            numsc = numscs(6) + 7
            numf = IRDSC(numsc,1,mscrn,iline,itabs,ispac,iferr,0)
CC            get which variables are in data set.
            do 10 nvl=1,12
              if (nvl .eq. 4) then
                j = 3
              else
                j = 1
              end if
              nln = iline(nvl)
              nsc = JWHER(nvl,itabs,ispac,j,nch)
              xdumy = ivarmt(nvl)
              if (xdumy .lt. 0.0) xdumy = rmissc
              i = IWTBF(mscrn(nsc,nln),nch,xdumy,0,iflrjs)
   10       continue
            i = IWTSC(numsc,1,mscrn,iline,iferr)
            numsc = numscs(6) + 8
            numf = IRDSC(numsc,1,mscrn,iline,itabs,ispac,iferr,1)
CC            set which variables are in data set.
            do 20 nvl=1,13
              nln = iline(nvl)
              nsc = JWHER(nvl,itabs,ispac,1,nch)
              xdumy = ivarmt(nvl+12)
              if (xdumy .lt. 0.0) xdumy = rmissc
              i = IWTBF(mscrn(nsc,nln),nch,xdumy,0,iflrjs)
   20       continue
            i = IWTSC(numsc,1,mscrn,iline,iferr)
            if (ihrfmt .eq. 1) then
              numsc = numscs(6) + 9
              numf = IRDSC(numsc,1,mscrn,iline,itabs,ispac,iferr,0)
              nvl = IWHER(1,iline,itabs,ispac,0,1,nln,nsc,nch)
              n = NUMC1(mformt,150)
              call MBLK1(mscrn(nsc,nln),nch)
              call MVCH1(mformt,mscrn(nsc,nln),nch)
              if (n .gt. nch) then
                nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nchs)
                call MBLK1(mscrn(nsc,nln),nch)
                call MVCH1(mformt(nch+1),mscrn(nsc,nln),MAX0(n-nch,
     *                     nchs))
              end if
              i = IWTSC(numsc,1,mscrn,iline,iferr)
            end if
          end if
        end if
CC
CC        set initial data from save files.
        i = JINIT(iferr,nmxlin*nmxfld,0,0)
        nchtrs = NUMC1(mtitle,50)
CC
CC        place control/options data in work file, screen menus
CC        numscs(7) to numsce(7).
        numsc = numscs(7)
        numf = IRDSC(numsc,1,mscrn,iline,itabs,ispac,iferr,1)
CC        heading.
        nvl = IWHER(1,iline,itabs,ispac,0,1,nln,nsc,nch)
        call MVCH1(mtitle,mscrn(nsc,nln),MIN0(50,nch))
CC        dosage calcs.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        if (iswopt(1) .eq. 0) then
          mscrn(nsc,nln) = malpha(14)
        else
          mscrn(nsc,nln) = malpha(25)
        end if
CC        concentration calcs.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        if (iswopt(2) .eq. 0) then
          mscrn(nsc,nln) = malpha(14)
        else
          mscrn(nsc,nln) = malpha(25)
        end if
CC        for time-mean concentration calcs.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        if (iswopt(3) .eq. 0) then
          mscrn(nsc,nln) = malpha(14)
        else
          mscrn(nsc,nln) = malpha(25)
        end if
CC        time-mean concentration units.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        if (iswopt(3) .ne. 0) then
CC          set units of time for time-mean concentration.
          call UNITS(mscrn(nsc,nln),nch,itmunt,jdumy,0,1,5)
        end if
CC        set time for time-mean concentration.
        nsc = JWHER(nvl,itabs,ispac,2,nch)
        i = IWTBF(mscrn(nsc,nln),nch,timavg,2,iflrjs)
CC        for gravitational deposition.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        if (iswopt(4) .eq. 0) then
          mscrn(nsc,nln) = malpha(14)
        else
          mscrn(nsc,nln) = malpha(25)
        end if
CC        integration time for quasi-cont. dosage.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        i = IWTBF(mscrn(nsc,nln),nch,timinv,4,iflrjs)
CC        final plume rise or distance dependent.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        if (iswopt(8) .le. 1) then
          mscrn(nsc,nln) = malpha(6)
        else
          mscrn(nsc,nln) = malpha(4)
        end if
CC        get clearing index.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        i = IWTBF(mscrn(nsc,nln),nch,clrinx,2,iflrjs)
CC        daytime hours restriction.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        if (iswopt(27) .eq. 0) then
          mscrn(nsc,nln) = malpha(14)
        else
          mscrn(nsc,nln) = malpha(25)
        end if
CC        output mass units.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        call UNITS(mscrn(nsc,nln),nch,iswopt(13),jdumy,0,1,3)
CC        output length units.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        call UNITS(mscrn(nsc,nln),nch,iswopt(15),jdumy,0,1,0)
CC        output area units.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        call UNITS(mscrn(nsc,nln),nch,iswopt(16),jdumy,0,1,9)
CC        output volume units.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        call UNITS(mscrn(nsc,nln),nch,iswopt(17),jdumy,0,1,10)
CC        output time units.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        call UNITS(mscrn(nsc,nln),nch,iswopt(14),jdumy,0,1,5)
CC        write data to work file.
        i = IWTSC(numsc,1,mscrn,iline,iferr)
        if (ifhrly .le. 0) then
          numsc = numscs(7) + 1
        else
          numsc = numscs(7) + 2
        end if
        numf = IRDSC(numsc,1,mscrn,iline,itabs,ispac,iferr,0)
        nvl = 0
        if (ifhrly .gt. 0) then
          nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
          if (ifmode .eq. 0) then
            mscrn(nsc,nln) = malpha(4)
          else
            mscrn(nsc,nln) = malpha(19)
          end if
        else
          ifmode = 0
        end if
CC        if print sum of sources.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        if (ngroup .eq. 0) then
          mscrn(nsc,nln) = malpha(19)
        else
          mscrn(nsc,nln) = malpha(7)
        end if
CC        print page column width.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        if (iswopt(22) .eq. 0) then
          xdumy = 132.0
        else
          xdumy = 80.0
        end if
        i = IWTBF(mscrn(nsc,nln),nch,xdumy,0,iflrjs)
CC        print maximum plume rise.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        if (iswopt(21) .eq. 0) then
          mscrn(nsc,nln) = malpha(14)
        else
          mscrn(nsc,nln) = malpha(25)
        end if
CC        Sigmaplot file.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        if (iswopt(5) .gt. 0.and.NUMC1(msgnam,22) .gt. 0) then
          call MVCH1(msgnam,mscrn(nsc,nln),MIN0(nch,22))
          i = LRJST(mscrn(nsc,nln),nch,iflrjs)
CC          open data file as logical unit isgfil.
          isgfil = 34
CC          combine file name and directory.
          call FNAME(mfnam,58,mdirec,msgnam)
          CLOSE (unit=isgfil,status='keep',err=30)
CC          formatted character.
   30     OPEN (unit=isgfil,file=mfnam,iostat=ier,err=40,
     *          status='unknown',form='formatted')
          go to 50
   40     iferr(nvl,1) = 1
          ifrdwt(1) = -1
          isgfil = 0                                                    030499
          if (KERRS(3,msgnam,ier,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *        go to 90
        else
          iswopt(5) = 0
          isgfil = 0                                                    030499
        end if
   50   if (ifhrly .gt. 0) then
CC          print table of highest and second highest.
          nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
          if (iswopt(12) .eq. 0) then
            mscrn(nsc,nln) = malpha(14)
          else
            mscrn(nsc,nln) = malpha(25)
          end if
CC          print maximum 50 receptors.
          nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
          if (iswopt(18) .eq. 0) then
            mscrn(nsc,nln) = malpha(14)
          else
            mscrn(nsc,nln) = malpha(25)
          end if
CC          see if print n hour averages.
          nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
          do 60 j=1,6
            nsc = JWHER(nvl,itabs,ispac,j,nch)
            if (navhrs(j) .gt. 0.0) then
              xdumy = navhrs(j)
              i = IWTBF(mscrn(nsc,nln),nch,xdumy,0,iflrjs)
            end if
   60     continue
CC          print each n-hour average.
          nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
          if (iswopt(28) .eq. 0) then
            mscrn(nsc,nln) = malpha(14)
          else
            mscrn(nsc,nln) = malpha(25)
          end if
CC          print seasonal or quarterly.
          nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
          if (ifsean .eq. 0) then
            mscrn(nsc,nln) = malpha(14)
          else if (ifsean .eq. 1) then
CC            print seasonal.
            mscrn(nsc,nln) = malpha(19)
          else if (ifsean .eq. 2) then
CC            print quarterly.
            mscrn(nsc,nln) = malpha(17)
          end if
CC          print annual tables.
          nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
          if (ifanul .eq. 0) then
            mscrn(nsc,nln) = malpha(14)
          else
            mscrn(nsc,nln) = malpha(1)
          end if
CC          print hourly input met. data.
          nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
          if (iswopt(24) .eq. 0) then
            mscrn(nsc,nln) = malpha(14)
          else
            mscrn(nsc,nln) = malpha(25)
          end if
CC          print hourly input source data.
          nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
          if (iswopt(26) .eq. 0) then
            mscrn(nsc,nln) = malpha(14)
          else
            mscrn(nsc,nln) = malpha(25)
          end if
CC          print calm wind warning messages
          nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
          if (iswopt(33) .eq. 0) then
            mscrn(nsc,nln) = malpha(14)
          else
            mscrn(nsc,nln) = malpha(25)
          end if
CC          print met. data error/warning messages
          nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
          if (iswopt(35) .eq. 0) then                                   051503
            mscrn(nsc,nln) = malpha(14)                                 051503
          else                                                          051503
            mscrn(nsc,nln) = malpha(25)                                 051503
          end if                                                        051503
        else
          iswopt(12) = 0
          iswopt(18) = 0
          iswopt(24) = 0
          iswopt(26) = 0
          iswopt(28) = 0
          iswopt(35) = 1
          i = JINIT(navhrs,6,0,0)
          ifsean = 0
          ifanul = 0
        end if
CC        plume rise option for instantaneous sources.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        if (iswopt(25) .eq. 0) then
          mscrn(nsc,nln) = malpha(1)
        else
          mscrn(nsc,nln) = malpha(2)
        end if
        i = IWTSC(numsc,1,mscrn,iline,iferr)
        if (ngroup .gt. 0) then
          numsc = numscs(7) + 3
          numf = IRDSC(numsc,1,mscrn,iline,itabs,ispac,iferr,0)
CC          source groups.
          j = 10
          l = 0
          nvl = 0
          do 80 n=1,ngroup
            do 70 k=1,nsogrp(n)
              l = l + 1
              xdumy = isogrp(l)
              j = j + 1
              if (j .gt. 10) then
                nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
                j = 1
              else
                nsc = JWHER(nvl,itabs,ispac,j,nch)
              end if
              i = IWTBF(mscrn(nsc,nln),nch,xdumy,0,iflrjs)
   70       continue
            j = j + 1
            if (j .gt. 10) then
              nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
              j = 1
            else
              nsc = JWHER(nvl,itabs,ispac,j,nch)
            end if
            call MBLK1(mscrn(nsc,nln),nch)
   80     continue
          i = IWTSC(numsc,1,mscrn,iline,iferr)
        end if
   90   ifset = 1                                                       082497
        if (nsourc .le. 0.or.qfract .le. 0.0) then
          ifset = -1
        else
          if (ifhsly .gt. 0) then
            if (ivarso(1) .le. 0.or.ISUMI(ivarso(2),3) .le. 0)          060607
     *          ifset = -1
          end if
          if (ifgpwn .gt. 0.and.iswopt(6) .eq. 2) ifset = -1
          if (ifset .gt. 0) then
            i = 0
  100       i = i + 1
            itype = IVGET(isotyp(i),2)
            jtype = IVGET(isotyp(i),1)
            if (itype .le. 0.or.jtype .le. 0) ifset = -1
            if (ifhsly .le. 0.or.ivarso(2) .le. 0) then
              if (qemiss(i) .le. 0.0) ifset = -1
            end if
            if (relhgt(i) .gt. rmissc.and.relhgt(i) .lt. 0.0) then
              relhgt(i) = 0.0
              if (ifset .gt. 0) then
                ifset = -2
              end if
            end if
            if (relhte(i) .gt. rmissc.and.relhte(i) .lt. 0.0) then      072497
              if (relhgt(i) .gt. 0.0.or.relhgt(i) .le. rmissc) then     072497
                relhte(i) = relhgt(i)                                   072497
              else if (ifset .gt. 0) then
                ifset = -2
              end if
            end if
            if (clddxs(i) .le. rmissd) then
              if (nxpnts .gt. 0) then
                clddxs(i) = xcoord(1)
              else if (nxypnt .gt. 0) then
                clddxs(i) = xdscrt(1)
              else
                clddxs(i) = 0.0
              end if
              if (ifset .gt. 0) then
                ifset = -2
              end if
            end if
            if (clddxe(i) .le. rmissd) then
              clddxe(i) = clddxs(i)
              if (ifset .gt. 0) then
                ifset = -2
              end if
            end if
            if (clddys(i) .le. rmissd) then
              if (nypnts .gt. 0) then
                clddys(i) = ycoord(1)
              else if (nxypnt .gt. 0) then
                clddys(i) = ydscrt(1)
              else
                clddys(i) = 0.0
              end if
              if (ifset .gt. 0) then
                ifset = -2
              end if
            end if
            if (clddye(i) .le. rmissd) then
              clddye(i) = clddys(i)
              if (ifset .gt. 0) then
                ifset = -2
              end if
            end if
            if (clddzs(i) .le. rmissd) then
              clddzs(i) = 0.0
              if (ifset .gt. 0) then
                ifset = -2
              end if
            end if
            if (clddze(i) .le. rmissd) then
              clddze(i) = clddzs(i)
              if (ifset .gt. 0) then
                ifset = -2
              end if
            end if
            if (itype .eq. 1) then
              if (jtype .eq. 1) then
CC                Correct previous versions used cldlng to indicate     072497
CC                default.  This version uses clddpt.                   072497
                if (cldlng(i) .le. rmissc) then                         072497
                  clddpt(i) = cldlng(i)                                 072497
                  cldsht(i) = cldlng(i)                                 072497
                end if                                                  072497
                if (clddpt(i) .gt. rmissc.and.clddpt(i) .le. 0.0)       072497
     *              ifset = -1                                          072497
              else
                if (cldsht(i) .gt. rmissc.and.cldsht(i) .le. 0.0)       072497
     *              ifset = -1                                          072497
                if (clddpt(i) .gt. rmissc.and.clddpt(i) .le. 0.0)       072497
     *              ifset = -1                                          072497
              end if
            else
              if (jtype .eq. 1) then
                if (cldlng(i) .le. 0.0) ifset = -1
                if (cldsht(i) .le. 0.0) ifset = -1
                if (cldang(i) .lt. 0.0.or.cldang(i) .gt. 360.0) 
     *              ifset = -1
                if (clddpt(i) .le. 0.0) ifset = -1
              else
                if (cldsht(i) .le. 0.0) ifset = -1
                if (clddpt(i) .le. 0.0) ifset = -1
              end if
            end if
            if (xsmlry(i) .le. 0.0) then
              xsmlry(i) = 50.0
              if (ifset .gt. 0) then
                ifset = -2
              end if
            end if
            if (xsmlrz(i) .le. 0.0) then
              xsmlrz(i) = 50.0
              if (ifset .gt. 0) then
                ifset = -2
              end if
            end if
            if (xlrgry(i) .lt. 0.0) then
              xlrgry(i) = 0.0
              if (ifset .gt. 0) then
                ifset = -2
              end if
            end if
            if (xlrgrz(i) .lt. 0.0) then
              xlrgrz(i) = 0.0
              if (ifset .gt. 0) then
                ifset = -2
              end if
            end if
            if (ifhsly .le. 0.or.ivarso(3) .le. 0) then
              if (hetcnt(i) .lt. 0.0) ifset = -1
            end if
            if (ifhsly .le. 0.or.ivarso(4) .le. 0) then
              if (brnrat(i) .lt. 0.0) ifset = -1
            end if
            if (ifhsly .le. 0) then
              if (taucld(i) .le. 0.0) ifset = -1
            end if
            if (gammat(i) .le. 0.0) then
              if (IVGET(isotyp(i),2) .eq. 1) then
                gammat(i) = gammai
              else
                gammat(i) = gammac
              end if
              if (ifset .gt. 0) then
                ifset = -2
              end if
            end if
            if (alphas(i) .le. 0.0) then
              if (taucld(i) .le. 15.0) then
                alphas(i) = 1.0
              else
                alphas(i) = 0.9
              end if
              if (ifset .gt. 0) then
                ifset = -2
              end if
            end if
            if (betass(i) .le. 0.0) then
              betass(i) = 1.0
              if (ifset .gt. 0) then
                ifset = -2
              end if
            end if
            if (i .lt. nsourc.and.ifset .ne. 0) go to 100
            if (ifset .gt. 0.and.ifgpwn .gt. 0) then
              if (nvsprt .gt. 0) then
                i = 0
  110           i = i + 1
                if (drpupr(i) .le. 0.0) ifset = -1
                if (drplwr(i) .le. 0.0) then
                  drplwr(i) = drpupr(i)
                  if (ifset .gt. 0) ifset = -2
                end if
                if (pctmat(i) .lt. 0.0) ifset = -1
                if (vspart(i) .gt. rmissc.and.vspart(i) .le. 0.0)
     *              ifset = -1
                if (gammad(i) .gt. rmissc.and.gammad(i) .lt. 0.0)
     *              ifset = -1
                if (i .lt. nvsprt.and.ifset .gt. 0) go to 110
              else
                ifset = -1
              end if
            end if
          end if
        end if
        if (ifrdwt(3) .ne. -1) ifrdwt(3) = ifset                        090498
        ifset = 1
        if (iniyrs .gt. 0) then
          if (iniyrs .lt. vllimt(1).or.iniyrs .gt. vulimt(1)) 
     *        ifset = -1
        end if
        if (inimon .gt. 0) then
          if (inimon .lt. vllimt(2).or.inimon .gt. vulimt(2)) 
     *        ifset = -1
        end if
        if (inidys .gt. 0) then
          if (inidys .lt. vllimt(3).or.inidys .gt. vulimt(3)) 
     *        ifset = -1
        end if
        if (inihrs .gt. -1) then
          if (inihrs .lt. vllimt(4).or.inihrs .gt. vulimt(4))
     *        ifset = -1
        end if
        if (injdys .gt. 0) then
          if (injdys .lt. vllimt(5).or.injdys .gt. vulimt(5)) 
     *        ifset = -1
        end if
        ydumy = refspd * constl(ispunt+1)
        if (ispunt .ne. 9) ydumy = ydumy / constt(jspunt+1)
        if (ydumy .lt. vllimt(6).or.ydumy .gt. vulimt(6)) ifset = -1
        if (pwrlaw .gt. rmissd) then
          if (pwrlaw .lt. vllimt(7).or.pwrlaw .gt. vulimt(7)) 
     *        ifset = -1
        end if
CC        
        if (wnddir .lt. vllimt(8).or.wnddir .gt. vulimt(8)) ifset = -1
CC        
        if (sigmap .gt. rmissd) then
          ydumy = sigmap
          if (isyunt .ne. 0) ydumy = xdumy * radtod
          if (ydumy .lt. vllimt(9).or.ydumy .gt. vulimt(9)) ifset = -1
        end if
CC        
        if (sigmep .gt. rmissd) then
          ydumy = sigmep
          if (iszunt .ne. 0) ydumy = xdumy * radtod
          if (ydumy .lt. vllimt(10).or.ydumy .gt. vulimt(10)) 
     *        ifset = -1
        end if
CC
        if (trbixr .gt. rmissd) then
          ydumy = trbixr
          if (isxunt .ne. 0) ydumy = xdumy * radtod
          if (ydumy .lt. vllimt(11).or.ydumy .gt. vulimt(11)) 
     *        ifset = -1
        end if
CC
        ydumy = constc(itpunt+1,2) * (airtmp - constc(itpunt+1,1))
        if (ydumy .lt. vllimt(12).or.ydumy .gt. vulimt(12)) ifset = -1
CC
        ydumy = airprs * constp(iprunt+1)
        if (ydumy .lt. vllimt(13).or.ydumy .gt. vulimt(13)) ifset = -1
CC
        if (dphidz .gt. rmissd) then
          ydumy = constc(ivgunt+1,2) * dphidz / constl(jvgunt+1)
          if (ydumy .lt. vllimt(14).or.ydumy .gt. vulimt(14)) 
     *        ifset = -1
        end if
CC
        if (hmdpth .gt. rmissd) then
          ydumy = hmdpth * constl(ihmunt+1)
          if (ydumy .lt. vllimt(15).or.ydumy .gt. vulimt(15)) 
     *        ifset = -1
        end if
CC
        if (spdshr .gt. rmissd) then
          ydumy = spdshr * constl(issunt+1)
          if (jssunt .ge. 0) ydumy = ydumy / constt(jssunt+1)
          if (ydumy .lt. vllimt(16).or.ydumy .gt. vulimt(16)) 
     *        ifset = -1
        end if
CC
        if (dirshr .gt. rmissd) then
          ydumy = dirshr
          if (idsunt .ne. 0) ydumy = dirshr * radtod
          if (ydumy .lt. vllimt(17).or.ydumy .gt. vulimt(17)) 
     *        ifset = -1
        end if
CC
        if (stblty .gt. rmissd) then
          if (stblty .lt. vllimt(18).or.stblty .gt. vulimt(18)) 
     *        ifset = -1
        end if
        if (radinx .gt. rmissd) then
          if (radinx .lt. vllimt(19).or.radinx .gt. vulimt(19)) 
     *        ifset = -1
        end if
CC
        ydumy = tauosa * constt(istunt+1)
        if (ydumy .lt. vllimt(20).or.ydumy .gt. vulimt(20)) ifset = -1
CC
        ydumy = roughl * constl(irount+1) / constl(6)                   092904
        if (ydumy .lt. vllimt(21).or.ydumy .gt. vulimt(21)) ifset = -1
CC
        if (airhum .gt. 0.0.and.airhum .le. 1.0) airhum = airhum * 
     *      100.0
        if (airhum .lt. vllimt(22).or.airhum .gt. vulimt(22)) 
     *      ifset = -1
CC>
CC>     if (clearx .lt. vllimt(23).or.clearx .gt. vulimt(23)) 
CC>  *      ifset = -1
CC    
        if (cldcov .lt. vllimt(24).or.cldcov .gt. vulimt(24)) 
     *      ifset = -1
        if (cldhgt .lt. vllimt(25).or.cldhgt .gt. vulimt(25)) 
     *      ifset = -1
        if (ifrdwt(4) .ne. -1) ifrdwt(4) = ifset                        090498
        ifset = 1
        if (ISUMI(iswopt,5) .le. 0) ifset = -1
        if (ifrdwt(1) .ne. -1) ifrdwt(1) = ifset                        090498
      end if                                                            082497E
      return
      end
