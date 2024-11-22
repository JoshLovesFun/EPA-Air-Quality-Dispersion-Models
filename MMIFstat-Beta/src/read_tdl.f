      subroutine readtdl(iUnit)
c
c------------------------------------------------------------------------------
c     MESOSCALE MODEL INTERFACE STATISTICS PROGRAM (MMIFStat)
c     VERSION 1.0 2009-10-28
c
c     readtdl reads a series of DS472 files and extracts the observations for
c     the date selected
c
c     Development History:
c     2009-10-28  Original Development ( Alpine Geophysics, LLC)
c
c------------------------------------------------------------------------------
c
      USE stat_fields
      USE site_fields
      implicit none

      include 'common.inc'

      integer nf,iUnit
      integer ndathr            ! YYYYJJJHH
      integer ndat,ndatin       ! YYYYJJJ
      integer ibtz              ! time zone shift
c Date variable
      integer jday,yyyy,jjj,iyr,imo,idy,ihr
      integer hh,iday
      integer date,year,hour
      integer isearch15

      integer istat, istt, nrec
      integer isearch4, isearchint
      integer tdlval(38)
      integer i
      integer ndattdl, ndatlocal

      character*512 nbf
      character*15  asta

      real pi,r2d
c variables in mixing ratio calculation
      real pp1,an2,p22,p23,p24,press,akdewp
      real airtempC,dptempC,ed,es
c variables in wind conversion/rotation  
      real ws,wd,sign,lon_dif,rotate,wsnorot,wdnorot
      real qdem
 
      logical eof,valid
  
      pi = acos(-1.0)
      r2d = 180./pi

      eof = .false.
      nrec = 0
      do while (.not. eof)
        read(iUnit,'(a)',iostat=istat) nbf
        if (istat .ne. 0) then
          eof = .true.
          close(iUnit)
        else 
          nrec = nrec + 1
          read(nbf,'(i4,3i2,a4,i2,i7,i8,i4,i3,i3,i3,i3,i3,i6,i3,
     &               i3,i3,i5,i4,i2,i3,i2,i3,i2,i3,i2,i3,i2,i3,i2,
     &               i3,i5,i5,i5,i5,i3,i3,i3,i3,i5,i4,i5)',
     &               iostat=istat)iyr,imo,idy,hour,asta(1:4),tdlval
          if (istat .ne. 0) then
            write(*,*)'Error Reading DS472 File '
            write(*,*)' At record number ',nrec
            write(*,'(a)')nbf
            stop
          end if
          asta(5:15) = '           '
c
c check if this is a day of interest
c
c----Convert date/time to user-defined time zone and check against
c     input date range
          year = iyr
          date = imo*100 + idy
          call juldate(iyr,date)
          ndattdl = 100000*year + 100*date + hour
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
          ndatin = 100000*year + 100*date + hour
          ihr = hour
          ndatlocal = ndatin

          ibegday = (ibegdat-100000*year)/100
          iday = date - ibegday + 1

          if ( (ndatin .ge. ibegdat) .and.
     &         (ndatin .le. ienddat) )  then
c
c check if this is a station of interest
c
            istt = isearch15(namsit,nsite,asta)
            if (istt .ne. 0) then
              if (ndatin .eq. ibegdat) then
                write(*,*)'Processing data for ',asta,istt,ihr+1,iday
              end if
c
c fill air temperature
c
              if (tdlval(5) .ne. 999) then
                tobs(istt,ihr+1,iday) =
     &               5./9.*(float(tdlval(5))-32.)+273.15
              end if
c
c moisture
c 
c mixing ratio
c
c if the station has valid data, compute station pressure the
c  reference is from the smithsonian meteorological tables thanks
c  to http://www.srh.noaa.gov/elp/wxcalc/altpresssc.html 
c  timbrice@hotmail.com
c             if (tdlval(15).ne.999 .and. tdlval(15).ne.9999 .and. tdlval(6).ne.999 ) then
c               pp1 = float(tdlval(15))/100.*33.86 
c               an2 = 0.190284
c               p22 = pp1**an2
c               p23 = 8.4288E-05*altlist(istt)
c               p24 = 1./an2
c               press = (p22 - p23)**p24 + 0.3
c               akdewp = 5./9.*(float(tdlval(6))-32.) + 273.15
c note units are g water/kg dry air
c               qobs(istt,ihr+1,iday) = 2.53E08*0.622/((press)/10.)*exp(-5.42E03/akdewp)
c             endif
c
c relative humidity these relations comes from the oh so useful
c  http://www.dss.ucar.edu/docs/equations/moisture.html
c  temperatures are in DEGC
c
c air temperatur is ds472 variable 5
c dew point temperature is ds472 variable 6
c
              if ( (tdlval(5) .ne. 999) .and. 
     &             (tdlval(6) .ne. 999) ) then
                airtempC = (5./9.*(float(tdlval(5))-32.))
                dptempC  = (5./9.*(float(tdlval(6))-32.))
                ed = 6.1078 * 10 ** ((dptempC*7.5)/
     &               (dptempC+237.5))
                es = 6.1078 * 10 ** ((airtempC*7.5)/
     &               (airtempC+237.5))
                qobs(istt,ihr+1,iday) = 100. * ed/es
              end if

c wind speed and direction
              wdnorot = -999.
              wsnorot = -999.
              if ((tdlval(11) .ne. 999) .and. 
     &            (tdlval(11) .le. 360) .and. 
     &            (tdlval(12) .ne. 999)) then
                if ((tdlval(11) .ne. 000) .and. 
     &              (tdlval(12) .ne. 000)) then
                  wd = float(tdlval(11))
                  ws = float(tdlval(12))
                  wdnorot = wd
                  wsnorot = ws*.5144
                  if (clat.lt.0) then
                    sign = -1.
                  else
                    sign = 1.
                  endif
                  lon_dif = ylonlist(istt) - clon
                  if ( lon_dif .gt. 180. ) lon_dif = lon_dif - 360.
                  if ( lon_dif .lt. -180. ) lon_dif = lon_dif + 360.
                  rotate  = - ( lon_dif ) * cone_factor * sign
                  wd = wd + rotate
                  if (wd .gt. 360.) wd = wd - 360.
                  if (wd .lt.   0.) wd = 360. + wd
                  uobs(istt,ihr+1,iday) = -ws*sin(wd/r2d)*.5144 !knots to mps
                  vobs(istt,ihr+1,iday) = -ws*cos(wd/r2d)*.5144 !knots to mps
                endif
              endif
            endif      ! Station of interest
          endif      ! day of interest 
        endif      ! istat 0
      enddo    ! not eof    
      return
      end subroutine readtdl


