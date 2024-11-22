       program ocdpro
C***********************************************************************
C      Offshore and Coastal Dispersion Model Preprocessor (OCDPRO)
C
C      Level: 970124
C
C      Author:
C         Donald C. DiCristofaro
C         Joseph C. Chang
C         EARTH TECH
C         196 Baker Avenue
C         Concord, MA 01742
C
C         Questions and comments should be addressed to Joseph Chang
C         Phone:  (508) 371-4256
C         Fax:    (508) 371-4280
C         E-mail: chang@src.com  or
C                 jchang@earthtech.com
C
C      Development of OCDPRO was funded by the Minerals Management
C      Service of the U.S. Department of the Interior under
C      Contract No. 14-12-0001-30396 and Purchase Order No. 13347
C
C      Project Officer:
C         Jeff Zippin
C         Chief of Environmental Operations
C         Minerals Management Service
C         Parkway Atrium Building
C         381 Elden Street
C         Herndon, VA   22070-4817
C
C      Description:
C         The OCD model requires an overwater hourly meteorological
C         data file (WMET.DAT) that does not contain any missing data
C         for the following overwater parameters:
C               relative humidity
C               air temperature
C               air minus water temperature
C               mixing height
C         OCDPRO is designed to assist the user in creating the
C         overwater input file to OCD without missing data for
C         the above listed parameters by:
C
C      1) Persistence for short time periods (less than six hours)
C      2) Preparing a report notifying the user to manually edit
C         the data for time periods greater than five hours but less
C         than two days (It is the responsibility of the user to
C         replace the missing data from previous days that are
C         representative)
C      3) For longer time periods (greater than two days), the
C         missing overwater values of relative humidity, air
C         temperature, air minus water temperature, or mixing height
C         are replaced with the following default values:
C             Parameter                      Default
C           relative humidity                  80%
C           air temperature                    overland air temperature
C           air minus water temperature        0 C
C           mixing height                      500 m
C
C      Input Requirements:
C
C         OCDPRO.DAT (Control file)
C           Contains only one parameter, IOPT5, that indicates the
C           type of overland met. data file.
C           = 0, From separate binary PCRAMMET file
C           = 1, ASCII met data included in the control file
C           = 2, From separate ASCII PCRAMMET file
C
C         WMETIN.DAT (Input meteorological overwater file)
C           See Table 3-7 of the OCD (Version 4) User's Guide
C           for format descriptions
C           Air minus Water Temperature must be input rather
C           than Surface Water Temperature
C           Relative Humidity must be input rather than Wet Bulb
C           or Dew Point Temperature
C
C         LMETPRO.DAT (OCD/4 overland meteorological file)
C           ASCII or BINARY format
C           See Section 3.2.3 of the OCD (Version 4) User's Guide
C           for format descriptions
C
C       Output:
C
C         WMETOUT.DAT
C           Overwater input file to OCD/4
C
C         OCDPRO.OUT
C           List of dates and parameters which need to be
C           manually replaced
C
C  OCDPRO           Revision History:
C     DCD 901231    Created.
C     JCC 970124    Revised
C
C***********************************************************************
      real def(4)
      integer iyr(8784),jul(8784),ihr(8784),ichk(8784,4),is(8784)
      real ow(8784,4),tland(8784),ddum(24),ddum2(2,24),ttland(24)
      real iyw,izw,iyl,izl,lljul
      data def/ 500., 80., -99., 0.0/
c
      write (*,*) 'OCDPRO'
      write (*,*) 'Offshore and Coastal Dispersion Model Preprocessor'
      write (*,*)
      write (*,*) 'OCDPRO should only be used after all sources of overw
     &ater and representative'
      write (*,*) 'overland data have been exhausted.'
      write (*,*)
c
      open (unit=1,file='ocdpro.dat',status='old')
c
c     Read format indicator, IOPT5, for overland met. data file.
c     IOPT5 has the same meaning of IOPT(5) in OCD/5.
c          = 0, From separate binary PCRAMMET file
c          = 1, ASCII met data included in the control file
c          = 2, From separate ASCII PCRAMMET file
c
      read (1,*) iopt5
c
      if (iopt5.ne.0 .and. iopt5.ne.1 .and. iopt5.ne.2) then
        write (*,*) 'The format indicator for overland met data file mus
     &t be 0, 1, or 2.'
        stop 'PREMATURE OCDPRO STOP'
      end if
c
      if (iopt5 .eq. 1 .or. iopt5 .eq. 2) then
         open (unit=4,file='lmetpro.dat',status='old')
         if (iopt5 .eq. 2) read (4,'(4(i6,1x))') iyr1,id1,iyr2,id2
      else if (iopt5 .eq. 0) then
         open (unit=4,file='lmetpro.dat',form='unformatted',
     &         status='old')
         read (4) iyr1,id1,iyr2,id2
      endif
c
      open (unit=2,file='wmetin.dat' ,status='old')
      open (unit=3,file='wmetout.dat',status='unknown')
      open (unit=7,file='ocdpro.out' ,status='unknown')
c
C  Initialize ichk array
c
      do 100 i=1,8784
         do 110 k=1,4
            ichk(i,k) = 1
110      continue
100   continue
      nn = 0
c
      do 1000 i=1,8784
c
c   Input the overwater meteorological data
c   See Table 3-7 of the OCD User's Guide for descriptions
c   OW - Overwater Values to be checked
c      1 - mixing height
c      2 - relative humidity
c      3 - air temperature
c      4 - air minus water temperature
c
         read (2,*,end=1001) iyr(i), jul(i), ihr(i), dum, dum,
     >             ow(i,1), ow(i,2), ow(i,3), ow(i,4)
c
c   Input overland temperature from ASCII LMET.DAT FILE
c   (OCD/4 format)
c
         if (iopt5 .eq. 1) then
            read (4,*) lyr,ljul,lhr,dum,dum,tland(i)
            if(lyr .ne. iyr(i) .or. ljul .ne. jul(i) .or.
     >         lhr .ne. ihr(i)) then
               write (*,*) 'Dates of WMETIN.DAT and LMETPRO.DAT files do
     > not match.'
               write (*,*) 'Questionable date for LMETPRO.DAT:',
     >                     lyr,ljul,lhr
               write (*,*) 'Questionable date for WMETIN.DAT:',
     >                     iyr(i),jul(i),ihr(i)
               stop 'PREMATURE OCDPRO STOP'
            endif
         else if (iopt5 .eq. 0) then
c
c   Input overland temperature from BINART LMET.DAT FILE
c   (PCRAMMET format)
c
            if (mod(i-1,24) .eq. 0) then
               read (4) lyr,lmo,lljul,ddum,ddum,ttland,ddum,ddum,ddum2
               if(lyr .ne. iyr(i) .or. int(lljul) .ne. jul(i)) then
                  write (*,*) 'Dates of WMETIN.DAT and LMETPRO.DAT files
     > do not match.'
                  write (*,*) 'Questionable date for LMETPRO.DAT:',
     >                         lyr,int(lljul)
                  write (*,*) 'Questionable date for WMETIN.DAT:',
     >                         iyr(i),jul(i)
                  stop 'PREMATURE OCDPRO STOP'
               endif
               do 200 ii=1,24
                  j = nn+ii
                  tland(j) = ttland(ii)
200            continue
            endif
c
c   Input overland temperature from ASCII LMET.DAT FILE
c   (PCRAMMET format)
c
         else if (iopt5 .eq. 2) then
            read (4,'(4i2,2f9.4,f6.1)') lyr,lmo,lday,lhr,
     >                                  dum,dum,tland(i)
            call julday (lyr,lmo,lday,ljul)
            if(lyr .ne. iyr(i) .or. ljul .ne. jul(i) .or.
     >         lhr .ne. ihr(i)) then
               write (*,*) 'Dates of WMETIN.DAT and LMETPRO.DAT files do
     > not match.'
               write (*,*) 'Questionable date for LMETPRO.DAT:',
     >                     lyr,ljul,lhr
               write (*,*) 'Questionable date for WMETIN.DAT:',
     >                     iyr(i),jul(i),ihr(i)
               stop 'PREMATURE OCDPRO STOP'
            endif
         endif
         nn = nn + 1
c
c    Find missing or out of range values for mixing height, relative
c    humidity, air temperature, and air minus water temperature
c
         if (ow(i,1) .lt. 1.0 .OR. ow(i,1) .gt. 10000.) ichk(i,1) = 0
         if (ow(i,2) .lt. 0.0 .OR. ow(i,2) .gt. 100.0) ichk(i,2) = 0
         if (ow(i,3) .lt. 200. .OR. ow(i,3) .gt. 330.0) ichk(i,3) = 0
         if (ABS(ow(i,4)) .gt. 10.0) ichk(i,4) = 0
1000  continue
c
c  Check for missing data
c
1001  do 1050 i=1,nn
         do 1060 k=1,4
             if(ichk(i,k) .EQ. 0) goto 1100
1060     continue
1050  continue
c
c  If no missing data, then indicate in the OCDPRO.OUT file that the
c  WMETIN.DAT file is adequate as WMET.DAT
c
      WRITE (7,20)
20    FORMAT(///,' There are no missing overwater data.')
      WRITE (7,2001)
2001  FORMAT('Therefore, the overwater met data file, WMETIN.DAT, can be
     & directly used',/,'to run OCD.')
      stop 'OCDPRO NORMAL STOP'
c
c    Fill in missing data
c
1100  do 2000 k=1,4
         do 300 i=1,nn
            if(ichk(i,k) .EQ. 0) then
               n = n + 1
               is(n) = i
c
c  If last data value is bad, go on to replacement section
c
               if(i .EQ. nn) go to 1101
cjc  JC modifications of 1/24/03.  To prevent jumping into an IF block.
               goto 1102
            end if
cjc            else
cjc  End of JC modifications.
1101           if(n .GT. 0.) then
                  do 400 ii=is(1),is(1)+n-1
                     if(n .LT. 6) then
c
c  Persist from last good value if less than 6 hours missing
c
                        ow(ii,k) = ow(is(1)-1, k)
                     else if(n .GE. 6 .AND. n .LT. 48) then
c
c  Report the data if 6 hours to 47 hours are missing
c
                        if (k .EQ. 1) then
                           WRITE (7,1) iyr(ii),jul(ii),ihr(ii),ow(ii,k)
                        elseif (k .EQ. 2) then
                           WRITE (7,2) iyr(ii),jul(ii),ihr(ii),ow(ii,k)
                        elseif (k .EQ. 3) then
                           WRITE (7,3) iyr(ii),jul(ii),ihr(ii),ow(ii,k)
                        else
                          WRITE (7,4) iyr(ii),jul(ii),ihr(ii),ow(ii,k)
                        endif
1                       FORMAT(i3,i4,i3,' Mixing Height = ',f10.3)
2                       FORMAT(i3,i4,i3,' Relative Humidity = ',f10.3)
3                       FORMAT(i3,i4,i3,' Air Temperature = ',f10.3)
4                       FORMAT(i3,i4,i3,' Air-Water Temperature = ',
     >                         f10.3)
                     else
c
c  Substitute with defaults if two days or more of data are missing
c  Use land temperature for missing overwater temperature
c
                        if(k .EQ. 3) then
                           ow(ii,k) = tland(ii)
                        else
                           ow(ii,k) = def(k)
                        endif
                     endif
400               continue
c
c  Reinitialize n counter
c
                  n = 0
               endif
cjc  JC modifications of 1/24/03.  To prevent jumping into an IF block.
cjc            endif
1102        continue
cjc  End of JC modifications.
300      continue
2000  continue
c
c  Output the results
c
      rewind (2)
      do 3000 i=1,nn
         read (2,*)iiyr, jjul, iihr, wd, ws,
     >             dum, dum, dum, dum, wdshr,
     >             iyw, izw, iyl, izl, wdthdz
         WRITE (3,140) iiyr, jjul, iihr, wd, ws,
     >             ow(i,1), ow(i,2), ow(i,3), ow(i,4), wdshr,
     >             iyw, izw, iyl, izl, wdthdz
140      FORMAT(i3,i4,i3,1x,f5.1,1x,f5.1,1x,f7.1,1x,f5.1,1x,f5.1,1x,
     >   f7.3,1x,f5.1,1x,4(f7.2,1x),f9.3)
3000  continue
      stop 'OCDPRO NORMAL STOP'
      end
c
c-----------------------------------------------------------------------
      subroutine julday (iyr, imo, iday, julian)
c-----------------------------------------------------------------------
c
c JULDAY        Version: 1.0            Level: 961126
c               Joseph C. Chang
c               EARTH TECH
c               196 Baker Avenue
c               Concord, MA 01742
c               Tel. (508)371-4256
c               Fax. (508)371-4280
c               E-mail chang@src.com, or jchang@earthtech.com
c
c PURPOSE:      Determine the Julian day.
c
c ARGUMENTS:
c   Input
c   -----
c   iyr    integer    year
c   imo    integer    month
c   iday   integer    day
c
c   Output
c   ------
c   julian integer    Julian day
c
c CALLING ROUTINES:     FILPOS, DAYLOOP
c
c EXTERNAL ROUTINES:    none
c
c-----------------------------------------------------------------------
        integer nodays(12)
        data nodays/31,28,31,30,31,30,31,31,30,31,30,31/
        julian=0
        if(imo.ne.1) goto 20
        julian=iday
        return
20      do 10 i=1,imo-1
        julian=julian+nodays(i)
10      continue
        julian=julian+iday
        if(mod(iyr,4).ne.0) return
        if(imo.eq.2) return
        julian=julian+1
        return
        end
