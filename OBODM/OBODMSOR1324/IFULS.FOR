      Function IFULS(nums1,nums2,mscrn,iline,itabs,ispac,iferr,iflgs)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IFULS Version 01.1                          ---
CC--- Subroutine description - display help screens for fuels       ---
CC---------------------------------------------------------------------
CC
CC    nums1 - screen number for fuels/explosives menu template.
CC    nums2 - screen number for pollutant/species menu template.
CC    mscrn - screen menu display buffer.
CC    iline - array of line numbers containing data fields.
CC    itabs - array of start columns of each data field.
CC    ispac - array of number of columns in each field.
CC    iferr - array of erroneous data flags for each field.
CC    iflgs - allow setting of defaults flag.
CC            0 - no.
CC            1 - yes.
CC
CC    returns - IFULS -
CC              0 - default source data set, return to current menu.
CC              1 - terminate.
CC              2 - main menu.
CC              3 - return to current menu.
CC
CC**********************************************************************
CC
      integer*2 nstrc,numf,i,kback,kstrc
CC
      character*1 mscrn
      character*8 minfi
      character*35 mlabl
      character*36 mfuel,mspec
CC
      dimension mscrn(80,25),iline(20),itabs(20,10),ispac(20,10),
     *          iferr(20,10),mfuel(12),dheat(12),dburn(12),istrc(50),
     *          nspec(12),nextr(12),jstrc(50),mspec(6),dsmol(6),
     *          dsden(6),dhalf(6),dfctr(6)
CC
      include 'OCNTRL.INC'
      include 'OCDSPM.INC'
      include 'OCALCS.INC'
      include 'ONAMES.INC'
CC
      data minfi/'Infinite'/,
     *     mlabl/'*** No Fuel/Explosive Data File ***'/
CC
CC**********************************************************************
CC
CC      display fuels and species menus.
CC
      IFULS = 3
CC      if fuels/explosive data.
      if (ifdfil .gt. 0) then
CC        index over groups of 12 displayed fuels/explosives.
        nstrc = 1
CC        start disc record in file mwknam.
        istrc(nstrc) = iflstr
      end if
CC        screen nums1, fuel/explosive screen menu.
   10 numf = IRDSC(nums1,1,mscrn,iline,itabs,ispac,iferr,0)
      if (ifdfil .le. 0) then
        nvl = IWHER(0,iline,itabs,ispac,1,1,nln,nsc,nch)
        call MVCH1(mlabl,mscrn(nsc,nln),35)
      else
CC        initialize menu line counter.
        nvl = 0
CC        start record for this group.
        nrec = istrc(nstrc)
CC          read up to 12 fuel/explosive records.
   20   nvl = nvl + 1
        READ (iwkfil,rec=nrec,err=30) mfuel(nvl),dheat(nvl),dburn(nvl),
     *                                nspec(nvl),nextr(nvl)
CC        fill line nvl with fuel/explosive name, etc.
        i = IWHER(nvl,iline,itabs,ispac,0,1,nln,nsc,nch)
        call MVCH1(mfuel(nvl),mscrn(nsc-37,nln),36)
        nsc = nsc + 4
        nch = 15
CC        default fuel/explosive heat content.
        if (dheat(nvl) .gt. 0.0) then
          i = IWTBF(mscrn(nsc,nln),nch,dheat(nvl),2,iflrjs)
        end if
        nsc = nsc + 18
CC        default fuel/explosive burn rate.
        if (dburn(nvl) .gt. 0.0) then
          i = IWTBF(mscrn(nsc,nln),nch,dburn(nvl),2,iflrjs)
        end if
        if (nvl .lt. 12) then
          nrec = nextr(nvl)
          if (nrec .lt. iflend) go to 20
        end if
        go to 40
   30   nvl = nvl - 1
        if (nvl .le. 0) then
          if (nstrc .gt. 1) then
            nstrc = 1
            go to 10
          end if
          IFULS = 3
          go to 120
        end if
      end if
CC        display screen menu.
   40 IFULS = IOSCN(nums1,mscrn,iline,itabs,ispac,iferr,lstrw,0)
      if (ifdfil .le. 0) then
        if (IFULS .ne. 1.and.IFULS .ne. 2) IFULS = 3
      end if
CC      check users response.
      if (IFULS .eq. 1) then
CC        terminate.
        go to 120
      else if (IFULS .eq. 2) then
CC        go to main menu.
        ifnext = 3
        go to 120
      else if (IFULS .eq. 3) then
CC        return to current menu.
        go to 120
      else if (IFULS .eq. 11) then
CC        go back to previous group of fuels.
        i = nstrc
        nstrc = MAX0(nstrc - 1,1)
        if (nstrc .eq. i) call IBELL(1)
      else if (IFULS .eq. 12) then
CC        go to next group of fuels.
        if (nextr(nvl) .lt. iflend) then
          nstrc = MAX0(nstrc + 1,50)
          istrc(nstrc) = nextr(nvl)
        else
          nstrc = 1
          call IBELL(1)
        end if
      else
CC        enter typed, go get defaults for selected fuel.
        do 50 i=1,nvl
CC          check cursor row against data row.
          if (lstrw .le. iline(i)) go to 60
   50   continue
        go to 10
CC
CC          display pollutant/species for the selected fuel.
   60   nvls = i
        kback = 0
        kstrc = 1
        if (nvls .eq. 1) then
          jstrc(kstrc) = istrc(nstrc) + 1
        else
          jstrc(kstrc) = nextr(nvls-1) + 1
        end if
        jend = MIN0(nextr(nvls),iflend)
CC        screen nums2, pollutant/species for selected fuel.
   70   numf = IRDSC(nums2,1,mscrn,iline,itabs,ispac,iferr,0)
        nvl = 0
        nrec = jstrc(kstrc)
CC        get up to 6 species.
   80   nvl = nvl + 1
        READ (iwkfil,rec=nrec,iostat=ier,err=90) mspec(nvl),dsmol(nvl),
     *                                           dsden(nvl),dhalf(nvl),
     *                                           dfctr(nvl)
CC        fill screen menu with data.
        i = IWHER(nvl,iline,itabs,ispac,0,1,nln,nsc,nch)
        if (nvl .eq. 1) then
          j = NUMC1(mfuel(nvls),36)
          call MVCH1(mfuel(nvls),mscrn((nmxcol-j)/2-3,nln-4),j)
        end if
CC        pollutant/species name.
        call MVCH1(mspec(nvl),mscrn(nsc-37,nln),36)
        if (dsmol(nvl) .gt. 0.0) then
CC          default molecular weight.
          i = IWTBF(mscrn(63,nln),15,dsmol(nvl),4,iflrjs)
        end if
        if (dsden(nvl) .gt. 0.0) then
CC          default density.
          i = IWTBF(mscrn(28,nln+1),15,dsden(nvl),4,iflrjs)
        end if
        if (dhalf(nvl) .gt. 0.0) then
CC          default half-life.
          i = IWTBF(mscrn(63,nln+1),15,dhalf(nvl),4,iflrjs)
        else
CC          infinite half-life.
          call MVCH1(minfi,mscrn(63,nln+1),8)
          i = LRJST(mscrn(63,nln+1),15,iflrjs)
        end if
        if (dfctr(nvl) .gt. 0.0) then
CC          pollutant/species emission factor.
          i = IWTBF(mscrn(63,nln+2),15,dfctr(nvl),4,iflrjs)
        end if
        if (nvl .lt. 6) then
          nrec = nrec + 1
          if (nrec .lt. jend) go to 80
        end if
        go to 100
   90   nvl = nvl - 1
        if (nvl .le. 0) then
          if (kstrc .gt. 1) then
            kstrc = 1
            go to 70
          end if
          go to 10
        end if
CC      display screen menu and get user response.
  100   IFULS = IOSCN(nums2,mscrn,iline,itabs,ispac,iferr,lstrw,0)
CC        check users response.
        if (IFULS .eq. 1) then
CC          terminate.
          go to 120
        else if (IFULS .eq. 2) then
CC          go to main menu.
          ifnext = 3
          go to 120
        else if (IFULS .eq. 3) then
CC          return to fuels menu.
          go to 10
        else if (IFULS .eq. 11) then
CC          go back to previous group of species.
          if (kstrc .eq. 1) then
            kback = kback + 1
            if (kback .gt. 0) go to 10
            kstrc = 1
          else
            kstrc = kstrc - 1
            if (kstrc .eq. 1) kback = -1
          end if
        else if (IFULS .eq. 12) then
CC          go to next group of species.
          nrec = nrec + 1
          if (nrec .lt. jend) then
            kstrc = kstrc + 1
            jstrc(kstrc) = nrec
          else
            kstrc = 1
            call IBELL(1)
          end if
        else
          if (iflgs .ne. 0) then
CC            go get defaults for selected species.
            do 110 i=1,nvl
              if (lstrw .le. iline(i)) then
                call MVCH1(mfuel(nvls),mpolnt,36)
                call MVCH1(mspec(i),mspeci,36)
                qtfuel = 0.0
                ifunit = 0
                spmass = 0.0
                isunit = 0
                qfract = dfctr(i)
                densty = dsden(i)
                idunit = 0
                jdunit = 5
                whtmol = dsmol(i)
                decays = dhalf(i)
                idcunt = 2
                heatdf = dheat(nvls)
                iduhet = 0
                jduhet = 0
                burndf = dburn(nvls)
                idubrn = 0
                jdubrn = 0
                IFULS = 0
                go to 120
              end if
  110       continue
          end if
        end if
        go to 70
      end if
      go to 10
  120 return
      end
