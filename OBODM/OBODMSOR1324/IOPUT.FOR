      Function IOPUT(io,jo,ko,nladd)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IOPUT Version 01.3                          --- 072497
CC--- Subroutine description - print main output heading and other  ---
CC---                          subheadings.                         ---
CC---------------------------------------------------------------------
CC
CC      arguments -
CC        io - main heading flag.
CC             0 = increment lines by nladd, continuation of table.
CC             1 = force new page, new table.
CC        jo - secondary heading flag.
CC             1 = input phase diagnostic messages
CC             2 = program input data
CC             3 = hourly meteorological data
CC             4 = calculation tables
CC             5 = graphics
CC             6 = hourly source data
CC        ko - sub heading flag.
CC             if jo = 2
CC               1 = program models selected.
CC               2 = print output options.
CC               3 = receptor grid system geometry.
CC               4 = source geometry/emission strength.-
CC               5 = pollutant/species material characteristics.
CC               6 = meteorological data.
CC               7 = n/a
CC               8 = other required data.
CC               9 = input/output files used.
CC              10 = plot output options.
CC             if jo = 3
CC               0 = no secondary heading.
CC               1 = hourly meteorological data.
CC             if jo = 4
CC               0 = no secondary heading.
CC               1 = cloud maximum plume rise.
CC               2 = concentration, dosage, deposition, etc. depending
CC                   on calculated parameter.
CC               3 = model calculations.
CC               4 = maximum 50 concentration, dosage, etc.
CC               5 = highest concentration, dosage, etc.
CC               6 = second highest concentration, dosage, etc.
CC             if jo = 5
CC               0 = no secondary heading
CC               2 = graphics generation
CC             if jo = 6
CC               0 = no secondary heading
CC               1 = hourly source data
CC        nladd  = number of lines to add.
CC
CC      returns -
CC        llines - updated line count. (common)
CC        ipages - updated page count. (common)
CC        IOPUT  - status of paging -
CC                 0 = same page
CC                 1 = new page started
CC
CC**********************************************************************
CC
      character*1 mlabl
      character*15 mlbls,mff
CC
      dimension mlbls(3),mlabl(15,3)
CC
      include 'OCDSPM.INC'
      include 'OCNTRL.INC'
      include 'ONAMES.INC'
CC
      equivalence (mlbls(1),mlabl(1,1))
CC
      data mlbls/'Maximum 50     ',
     *           'Highest        ',
     *           'Second Highest '/
CC
CC**********************************************************************
CC
      IOPUT = 0
      if (iotfil .gt. 0) then
        if (io .eq. 0.and.ipages .ne. 0) then
          llines = llines + nladd
          if (llines .lt. linetl) then
            js = 0
          else
            js = 1
          end if
        else
          js = 1
        end if
        if (js .ne. 0) then
CCOMP1S
c         mff = CHAR(49)
CCOMP1E
CCPCITS
          mff = CHAR(12)
CCPCITE
          ipages = ipages + 1
          if (iswopt(22) .eq. 0) then
            WRITE (iotfil,9000) mff,versod,(mtitle(i),i=1,50),irnmon,
     *                          irndys,irnyrs,ipages
          else
            WRITE (iotfil,9001) mff,versod,(mtitle(i),i=1,50),irnmon,
     *                          irndys,irnyrs,ipages                   
          end if
          llines = nladd + 2
          IOPUT = 1
          if (io .ne. 1) then
            if (iswopt(22) .eq. 0) then
              WRITE (iotfil,9002) itable
            else
              WRITE (iotfil,9003) itable
            end if
          else
            itable = itable + 1
            if (iswopt(22) .eq. 0) then
              WRITE (iotfil,9004) itable
            else
              WRITE (iotfil,9005) itable
            end if
          end if
          llines = llines + 1
          if (jo .eq. 1) then
            WRITE (iotfil,9006)
            llines = llines + 4
          else if (jo .eq. 2) then
            WRITE (iotfil,9007)
            llines = llines + 4
            if (ko .eq. 1) then
              WRITE (iotfil,9008)
            else if (ko .eq. 2) then
              WRITE (iotfil,9009)
            else if (ko .eq. 3) then
              WRITE (iotfil,9010)
            else if (ko .eq. 4) then
              WRITE (iotfil,9011)
            else if (ko .eq. 5) then
              WRITE (iotfil,9012)
            else if (ko .eq. 6) then
              WRITE (iotfil,9013)
              lines = lines + 1
            else if (ko .eq. 8) then
              WRITE (iotfil,9015)
            else if (ko .eq. 9) then
              WRITE (iotfil,9016)
            else if (ko .eq. 10) then
              WRITE (iotfil,9017)
            end if
            if (ko .gt. 0) llines = llines + 3
          else if (jo .eq. 3) then
            if (ko .eq. 1) then
              WRITE (iotfil,9018)
              llines = llines + 2
            end if
          else if (jo .eq. 4) then
            if (ko .gt. 0) then
              if (ko .eq. 1) then
                WRITE (iotfil,9019)
              else if (ko .eq. 2.or.(ko .ge. 4.and.ko .le. 6)) then
                if (iswopt(22) .eq. 0) then
                  n = 130
                else
                  n = 78
                end if
                j = 0
                if (ko .eq. 4) then
                  j = 11
                else if (ko .eq. 5) then
                  j = 8
                else if (ko .eq. 6) then
                  j = 15
                end if
                if (j+nlbsch(1) .gt. n) then
                  i = n - j
   10             i = i - 1
                  if (i .gt. n/2) then
                    if (mscrch(ilbstr(1)+i) .ne. mblnk1) go to 10
                  end if
                  n1 = i
                  n2 = ilbstr(1) + nlbsch(1) - 1
                else
                  n1 = ilbstr(1) + nlbsch(1) - 1
                  n2 = 0
                end if
                l = (n - (j + n1 - ilbstr(1) + 1)) / 2
                if (ko .ne. 2.and.(ifmode .eq. 0.or.ko .eq. 4)) then    110198
                  WRITE (iotfil,9020) (mblnk1,i=1,l),(mlabl(i,ko-3),i=1,
     *                                j),(mscrch(i),i=ilbstr(1),n1)
                else
                  WRITE (iotfil,9020) (mblnk1,i=1,l),(mscrch(i),i=
     *                                ilbstr(1),n1)
                end if
                if (n2 .gt. 0) then
                  WRITE (iotfil,9020) (mblnk1,i=1,l),(mscrch(i),i=n1+1,
     *                                n2)
                  llines = llines + 1
                end if
                WRITE (iotfil,9021)
              else if (ko .eq. 3) then
                WRITE (iotfil,9022)
              end if
              llines = llines + 2
            end if
          else if (jo .eq. 5) then
            if (ko .gt. 0) then
              if (ko .eq. 2) then
                WRITE (iotfil,9023)
              end if
              llines = llines + 2
            end if
          else if (jo .eq. 6) then
            if (ko .eq. 1) then
              WRITE (iotfil,9025)
              llines = llines + 2
            end if
          end if
        end if
      end if
      return
CC
 9000 format (a1,'OBODM V',f4.1,'-',24x,50a1,24x,'-',i2.2,'/',i2.2,'/', 072497
     *i4,' pg',i6/)                                                     072497
 9001 format (a1,'OBODM',f4.1,50a1,i2.2,'/',i2.2,'/',i4,' pg',i5/)      110198                                                        110198
 9002 format (58x,'Table',i5,' (cont.)')                                110198
 9003 format (31x,'Table',i5,' (cont.)')                                110198
 9004 format (62x,'Table',i5)                                           110198
 9005 format (35x,'Table',i5)                                           110198
 9006 format (/20x,'- Input Phase Diagnostic Messages -'//)
 9007 format (/27x,'- Program Input Data -'//)
 9008 format (/24x,'- Program Models Selected -'/)
 9009 format (/26x,'- Print Output Options -'/)
 9010 format (/21x,'- Receptor Grid System Geometry -'/)
 9011 format (/19x,'- Source Geometry/Emission Strength -'/)
 9012 format (/14x,'- Pollutant/Species Material Characteristics -'/)
 9013 format (/26x,'- Meteorological Data -'/26x,'( [#-#] min-max limits
     * )'/)
 9015 format (/26x,'- Other Required Data -'/)
 9016 format (/24x,'- Input/Output Files Used -'/)
 9017 format (/26x,'- Plot Output Options -'/)
 9018 format (/23x,'- Hourly Meteorological Data -')
 9019 format (/24x,'- Maximum Cloud Plume Rise -')
 9020 format (1x,130a1)
 9021 format ()
 9022 format (/26x,'- Model Calculations -')
 9023 format (/26x,'- Graphics Generation -')
 9025 format (/26x,'- Hourly Source Data -')
      end
