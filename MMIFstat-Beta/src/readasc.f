      subroutine readasc

c-----READASC reads an ASCII formatted OBS file and extracts data for the 
c     specified date and space window

      USE stat_fields
      USE site_fields
      implicit none

      include 'common.inc'

      integer nvar
      parameter(nvar=7)


      real varobs(10)
      real lon_dif,eps,e0,lv,rv,pi,r2d
      real slon,slat,selv,rio,rjo,ws,wd,tp,qv,td,sp,ev,rh,es,uu,vv,
     &     sign,rotate
      integer ifound(nvar),vunits(nvar)
      integer year,date,hour
      integer ncnt,num,nval,idum
      integer iyr,imo,idy,ihour,iobsdat,iolddat
      integer iday,ihr,n,isite,is,i,k,j
      integer nsitescan
      integer nsitetmp
      real xloc,yloc
      character*200 inrec
      character*15 asite
      character*15, allocatable :: namsitscan( : )
      logical luse

      data eps/0.622/, e0/6.11/, lv/2.5e6/, rv/461./

      nsitescan = 2000
 50   continue
      allocate (namsitscan(nsitescan))
      nsite = 0
      iolddat = 0
      pi = acos(-1.0)
      r2d = 180./pi

c-----Scan the obsfile to determine how many stations, this must be done
c      in order to allocate memory to hold the data.
c
 101  continue
      read(10,'(a)',end=998) inrec
      read(inrec,*) num
      if (num.eq.999999) then
        call header(nval,ifound,vunits)
        goto 101
      endif
      read(inrec,*) iyr,imo,idy,ihour,asite,slat,slon,selv,
     &              (varobs(n),idum,n=1,nval)
      ncnt = ncnt + 1
c-----Convert date/time to user-defined time zone and check against
c     input date range

      year = iyr
      date = imo*100 + idy
      call juldate(iyr,date)
      hour = int(float(ihour)/100.)
      hour = hour + itzon
      if (hour.lt.0) then
        hour = 24 + hour
        date = date - 1
        if (date .lt. 1) then
           year = year - 1
           date = 365
           if (mod(year,4) .eq. 0) date = 366
        endif
      endif
      iobsdat = 100000*year + 100*date + hour

      if (iobsdat.lt.ibegdat) goto 101
      if (iobsdat.gt.ienddat) goto 998
      if (iobsdat.ne.iolddat) then
        iolddat = iobsdat
        ibegday = (ibegdat-100000*year)/100
        iday = date - ibegday + 1
        ihr = hour + 1
        write(*,*) 'Prefound OBS day/hour:',date,hour,iday,ihr
      endif

c-----Valid date/time found; check for sites

      luse = .false.
      if (ninsite.gt.0) then

c-----A specific site list must be checked

        do n = 1,ninsite
          if (asite.eq.insite(n)) then
            if (llcp) then
               call lcpgeo(0,clat,clon,tlat1,tlat2,xloc,yloc,slon,slat)
            elseif (lpol) then
               call polgeo(0,clat,clon,tlat1,tlat2,xloc,yloc,slon,slat)
            endif
            rio = 1 + (xloc - x0mm5)/deltax
            rjo = 1 + (yloc - y0mm5)/deltax
            luse = .true.
          endif
        enddo
      else

c-----A range of coordinates must be checked

        if (llcp) then
           call lcpgeo(0,clat,clon,tlat1,tlat2,xloc,yloc,slon,slat)
        elseif (lpol) then
           call polgeo(0,clat,clon,tlat1,tlat2,xloc,yloc,slon,slat)
        endif
        rio = 1 + (xloc - x0mm5)/deltax
        rjo = 1 + (yloc - y0mm5)/deltax
        if (rio.ge.float(ibeg) .and. rio.le.float(iend) .and.
     &      rjo.ge.float(jbeg) .and. rjo.le.float(jend)) luse =.true.
      endif
      if (.not.luse) goto 101

c-----Found a site we need -- load location data

      if (nsite.eq.0) then
        nsite = nsite + 1
        isite = 1
        write(*,*)'****PREFOUND:',isite,' ',asite
      else
        do is = 1,nsite
          if (asite.eq.namsitscan(is)) then
            isite = is
            goto 201
          endif
        enddo
        nsite = nsite + 1
        isite = nsite
        if (nsite.gt.nsitescan) then
          nsitescan = nsitescan*2
          deallocate ( namsitscan )
          write(*,*)'Increasing internal site list sizes ',nsitescan
          if (nsitescan .gt. 100000) then
            write(*,*)'Internal site list exceeded bounds '
            stop
          end if
          goto 50
        endif
        namsitscan(isite) = asite
        write(*,*)'****PREFOUND:',isite,' ',asite
      endif
 201  continue
      goto 101

 998  if (nsite.eq.0) then
        write(*,*)'No OBS sites found, STOPPING'
        stop
      endif

      write(*,*)'Station Scan found ',nsite,' stations '
      call alloc_site(nsite)
      call alloc_stat(nsite,mxhr,nday,-999.)
      nsitescan = nsite

      nsite = 0
      rewind(10)
c-----Top of record reading loop

      ncnt = 0
 100  continue
      read(10,'(a)',end=999) inrec
      read(inrec,*) num
      if (num.eq.999999) then
        call header(nval,ifound,vunits)
        goto 100
      endif
      read(inrec,*) iyr,imo,idy,ihour,asite,slat,slon,selv,
     &              (varobs(n),idum,n=1,nval)
      ncnt = ncnt + 1

c-----Convert date/time to user-defined time zone and check against
c     input date range

      year = iyr
      date = imo*100 + idy
      call juldate(iyr,date)
      hour = int(float(ihour)/100.)
      hour = hour + itzon
      if (hour.lt.0) then
        hour = 24 + hour
        date = date - 1
        if (date .lt. 1) then
           year = year - 1
           date = 365
           if (mod(year,4) .eq. 0) date = 366
        endif
      endif
      iobsdat = 100000*year + 100*date + hour

      if (iobsdat.lt.ibegdat) goto 100
      if (iobsdat.gt.ienddat) goto 999
      if (iobsdat.ne.iolddat) then
        iolddat = iobsdat
        ibegday = (ibegdat-100000*year)/100
        iday = date - ibegday + 1
        ihr = hour + 1
        write(*,*) 'Found OBS day/hour:',date,hour,iday,ihr
      endif

c-----Valid date/time found; check for sites

      luse = .false.
      if (ninsite.gt.0) then

c-----A specific site list must be checked

        do n = 1,ninsite
          if (asite.eq.insite(n)) then
            if (llcp) then
               call lcpgeo(0,clat,clon,tlat1,tlat2,xloc,yloc,slon,slat)
            elseif (lpol) then
               call polgeo(0,clat,clon,tlat1,tlat2,xloc,yloc,slon,slat)
            endif
            rio = 1 + (xloc - x0mm5)/deltax
            rjo = 1 + (yloc - y0mm5)/deltax
            luse = .true.
          endif
        enddo
      else

c-----A range of coordinates must be checked

        if (llcp) then
           call lcpgeo(0,clat,clon,tlat1,tlat2,xloc,yloc,slon,slat)
        elseif (lpol) then
           call polgeo(0,clat,clon,tlat1,tlat2,xloc,yloc,slon,slat)
        endif
        rio = 1 + (xloc - x0mm5)/deltax
        rjo = 1 + (yloc - y0mm5)/deltax
        if (rio.ge.float(ibeg) .and. rio.le.float(iend) .and.
     &      rjo.ge.float(jbeg) .and. rjo.le.float(jend)) luse =.true.
      endif
      if (.not.luse) goto 100

c-----Found a site we need -- load location data

      if (nsite.eq.0) then
        nsite = nsite + 1
        isite = 1
        namsit(1) = asite
        write(*,*)'****FOUND:',isite,' ',namsit(isite)
        rj(1) = rjo
        ri(1) = rio
        rk(1) = 1
        xlatlist(1) = slat
        ylonlist(1) = slon
      else
        do is = 1,nsite
          if (asite.eq.namsit(is)) then
            isite = is
            goto 200
          endif
        enddo
        nsite = nsite + 1
        if (nsite.gt.nsitescan) then
          write(*,*)'Number of sites exceeds internal dimensions'
          write(*,*)'Sites read: ',nsite
          write(*,*)'Dimension: ',nsitescan
          stop
        endif
        isite = nsite
        namsit(isite) = asite
        write(*,*)'****FOUND:',isite,' ',namsit(isite)
        rj(isite) = rjo
        ri(isite) = rio
        rk(isite) = 1
        xlatlist(isite) = slat
        ylonlist(isite) = slon
      endif
 200  continue

c-----Check for units conversion

c-----Wind speed

      ws = -999.
      wd = -999.
      if (ifound(1).ne.0 .and. 
     &    varobs(ifound(1)).ne.-999. .and. 
     &    varobs(ifound(2)).ne.-999.) then
          ws = varobs(ifound(1))
          wd = varobs(ifound(2))
        if (vunits(1).eq.2) then
          ws = ws*0.51444444
        elseif (vunits(1).eq.3) then
          ws = ws*0.44704
        elseif (vunits(1).eq.4) then
          ws = ws*0.27777778
        endif
      endif

c-----Temperature

      tp = -999.
      if (ifound(3).ne.0 .and. varobs(ifound(3)).ne.-999.) then
        tp = varobs(ifound(3))
        if (vunits(3).eq.1) then
          tp = tp + 273.15
        elseif (vunits(3).eq.2) then
          tp = 5.*(tp - 32.)/9. + 273.15
        endif
      endif
    
c-----Humidity

cdem modification to analyze relative humidity instead of mixing ratio
      qv = -999.
      if (ifound(6).ne.0 .and. varobs(ifound(6)).ne.-999.) then
        qv = varobs(ifound(6))
        if (vunits(6).eq.2) then
         qv = qv*100.
        endif
      end if
cdem old code
c     if (ifound(4).ne.0 .and. varobs(ifound(4)).ne.-999.) then
c       qv = varobs(ifound(4))
c       if (vunits(4).eq.3) then
c         qv = qv/1000.
c       endif
c     elseif (ifound(5).ne.0 .and.
c    &        varobs(ifound(5)).ne.-999. .and.
c    &        varobs(ifound(7)).ne.-999.) then
c       td = varobs(ifound(5))
c       sp = varobs(ifound(7))
c       if (vunits(5).eq.1) then
c         td = td + 273.15
c       elseif (vunits(5).eq.2) then
c         td = 5.*(td - 32.)/9. + 273.15
c       endif
c       if (vunits(7).eq.1) then
c         sp = sp/100.
c       elseif (vunits(7).eq.3) then
c         sp = sp*1013.25/29.92
c       endif
c       ev = e0*exp((lv/rv)*(1./273.15 - 1./td))
c       qv = ev*eps/(sp - ev) 
c     elseif (ifound(6).ne.0 .and.
c    &        varobs(ifound(6)).ne.-999. .and.
c    &        varobs(ifound(3)).ne.-999. .and.
c    &        varobs(ifound(7)).ne.-999.) then
c       rh = varobs(ifound(6))
c       sp = varobs(ifound(7))
c       if (vunits(6).eq.1) then
c         rh = rh/100.
c       endif
c       if (vunits(7).eq.1) then
c         sp = sp/100.
c       elseif (vunits(7).eq.3) then
c         sp = sp*1013.25/29.92
c       endif
c       es = e0*exp((lv/rv)*(1./273. - 1./tp))
c       ev = rh*es
c       qv = ev*eps/(sp - ev) 
c     endif
cdem end mod

c-----Rotate winds to grid-relative and decompose to U/V components

      if (ws.eq.-999. .or. wd.eq.-999.) then
        uu = -999.
        vv = -999.
      else
        if (clat.lt.0) then
          sign = -1.
        else
          sign = 1.
        endif
        lon_dif = slon - clon
        if ( lon_dif .gt. 180. ) lon_dif = lon_dif - 360.
        if ( lon_dif .lt. -180. ) lon_dif = lon_dif + 360.
        rotate  = - ( lon_dif ) * cone_factor * sign
        wd = wd + rotate
        if (wd .gt. 360.) wd = wd - 360.
        if (wd .lt.   0.) wd = 360. + wd
        uu = -ws*sin(wd/r2d)
        vv = -ws*cos(wd/r2d)
      endif

c-----Load met data variables

      uobs(isite,ihr,iday) = uu
      vobs(isite,ihr,iday) = vv
      tobs(isite,ihr,iday) = tp
      qobs(isite,ihr,iday) = qv
      goto 100

 999  if (nsite.eq.0) then
        write(*,*)'No OBS sites found, STOPPING'
        stop
      endif 

      return
      end

c-----------------------------------------------------------------------

      subroutine header(nval,ifound,vunits)

      implicit none

      integer nval

      integer nvar,nunits,i,j,n,k
      parameter(nvar=7,nunits=4)
      character*20 varnam,units,varlst(nvar),unitlst(nunits,nvar)
      character*40 inrec
      integer ifound(nvar),vunits(nvar)

      data varlst /'WINDSPEED','WIND_DIRECTION','TEMPERATURE',
     &             'MIX_RATIO','DEWPOINT','REL_HUMIDITY','STN_PRES'/
      data unitlst /'m/s','knots','mph','km/hr',
     &               'deg',' ',' ',' ',
     &               'C','F','K',' ',
     &               'g/g','kg/kg','g/kg',' ',
     &               'C','F','K',' ',
     &               '%','fraction',' ',' ',
     &               'Pa','mb','in',' '/

      do j = 1,nvar
        ifound(j) = 0
        vunits(j) = 0
      enddo

c-----Read number of variables on file in this section

      read(10,*) nval
      write(*,*)
      write(*,*)'New OBS file section found'
      write(*,*)'Number of variables on file: ',nval

c-----Loop to read and check variables/units

      do 10 n = 1,nval
        read(10,'(a)') inrec
        read(inrec,*) varnam
        do k = 1,40
          if (inrec(k:k).eq.' ' .and. inrec(k+1:k+1).ne.' ') then
            read(inrec(k+1:),'(a)') units
            goto 5
          endif
        enddo
  5     do j = 1,nvar
          if (varnam.eq.varlst(j)) then
            ifound(j) = n
            do i = 1,nunits
              if (units.eq.unitlst(i,j)) then
                write(*,*)'Found Parameter: ',varnam
                write(*,*)'          Units: ',units
                vunits(j) = i
                goto 10
              endif
            enddo
            write(*,*)'Units not allowed'
            write(*,*)'Parameter: ',varnam
            write(*,*)'    Units: ',units
            stop
          endif
        enddo
  10  continue

c-----Check that needed fields are available

      if (ifound(1).eq.0 .and. ifound(2).eq.0) then
        write(*,*)'WINDS WILL NOT BE PROCESSED'
      endif

      if (ifound(1).ne.0 .and. ifound(2).eq.0) then
        ifound(1) = 0
        write(*,*)'Wind speed requires direction'
        write(*,*)'WINDS WILL NOT BE PROCESSED'
      endif

      if (ifound(2).ne.0 .and. ifound(1).eq.0) then
        ifound(2) = 0
        write(*,*)'Wind direction requires speed'
        write(*,*)'WINDS WILL NOT BE PROCESSED'
      endif

      if (ifound(3).eq.0) then
        write(*,*)'TEMPERATURE WILL NOT BE PROCESSED'
      endif

      if (ifound(4).eq.0 .and. ifound(5).eq.0 .and. ifound(6).eq.0) then
        write(*,*)'HUMIDITY WILL NOT BE PROCESSED'
      endif

cdem changed from 4 to 6 to change the analysis from MIXR to RH.
      if (ifound(6).ne.0) return

      if (ifound(5).ne.0 .and. ifound(7).eq.0) then
        ifound(5) = 0
        write(*,*)'Dewpoint requires station pressure'
        write(*,*)'HUMIDITY WILL NOT BE PROCESSED'
      endif

      if (ifound(6).ne.0 .and.
     &    (ifound(3).eq.0 .or. ifound(7).eq.0)) then
        ifound(6) = 0
        write(*,*)'RH requires temperature and station pressure'
        write(*,*)'HUMIDITY WILL NOT BE PROCESSED'
      endif

      return
      end
