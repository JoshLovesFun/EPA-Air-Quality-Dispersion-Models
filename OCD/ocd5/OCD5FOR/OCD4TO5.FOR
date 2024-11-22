      program ocd4to5
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c PURPOSE:                                      Level 970224
c --------
c   To convert OCD 4.1 input file to OCD 5.0 input file.
c
c Author:
c -------
c   Joseph C. Chang
c   EARTH TECH
c   196 Baker Avenue
c   Concord, MA 01742
c   Tel (508) 371-4256       Fax (508) 371-4280
c   E-mail chang@src.com
c
c   Developed for:
c   Minerals Management Service
c   U.S. Department of the Interior
c
c   Note that free-format read will sometimes cause round-off errors.
c   In order to prevent this from happening, all real numbers that
c   are read with free format are declared as double precision!
c
c   First created: 961113
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c      parameter (maxp=250)        ! max. no. of point sources
c      parameter (maxrec=180)      ! max. no. of receptors
      parameter (maxp=8500)        ! max. no. of point sources
      parameter (maxrec=2000)      ! max. no. of receptors
      parameter (maxmap=120)      ! max. no. of grids in x- and y-axis
c                                 ! for shoreline geometry
      integer mps(25),iopt(25),jopt(9),idate(2)
      double precision decay(12)
      double precision radil(5),elrdum(36,5)
      double precision source(11,maxp),elp(maxp)
      double precision contwo,celm,hane,z0l,zmin,slat,xstop,ystop
      double precision centx,centy,hwane,hwt,alat,along,tzone
      double precision x0,y0,delx,dely,wmin,avgdist
      real rrec(maxrec),srec(maxrec),zr(maxrec),elr(maxrec),hter(maxrec)
      character*2 cmps(25)
      character*12 cradil(5),celrdum(36,5)
      character*8 rname(maxrec)
      character*80 line1,line2,line3
      character*12 pname(maxp)
      character*4 dum,endp,endr,flag,ends,xlet
      character*20 ifmt,ifmt2,ifmt3
      character*1 xymap(maxmap,maxmap),blnk,letw
      character*3 ender
      character*10 b10
      character*160 buffer
      character*2 dls,dle
      character*2 dls_real,dle_real        ! Delimiters for input required
      character*2 dls_comment,dle_comment  ! Delimiters for input not
c                                            required, just for comments
c
      data in/1/                ! unit number for OCD 4.1 input file
      data in2/2/               ! unit number for OCD 5 input file
      data endp/'ENDP'/
      data endr/'ENDR'/
      data ends/'ENDS'/
      data blnk/' '/
      data letw/'W'/
      data ender/'END'/
      data b10/'          '/
      data dls_real/'! '/
      data dle_real/' !'/
      data dls_comment/'# '/
      data dle_comment/' #'/
c
c     Initializations
c
      npt=0
      nrecep=0
      ja=0
c
      call fopen (in,'ocd4inpt.dat','old','formatted')
      call fopen (in2,'ocd5inpt.dat','unknown','formatted')
c
c     Group 1 (Mandatory)
c ////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
c     Title lines
c
      read (in,1180) line1,line2,line3
      write (in2,1180) line1,line2,line3
      write (in2,*) '---------------- Run title (first 3 lines, INPUT GR
     &OUP 1) ---------------'
      write (in2,*) b10,'(OCD takes the first three line of this file as
     & the run title)'
      write (in2,*)
      write (in2,*) '                    OCD MODEL CONTROL FILE'
      write (in2,*) '                    ----------------------'
      write (in2,*)
      write (in2,*) '---------------------------------------------------
     &---------------------------'
      write (in2,*)
      write (in2,*) b10,'Only those data that are enclosed by proper del
     &imiters, the'
      write (in2,*) b10,'exclamation marks, will be retrieved by OCD.  A
     &ll other'
      write (in2,*) b10,'information is used for comments.  For the sake
     & of completeness,'
      write (in2,*) b10,'other data that are not required by the current
     & run will'
      write (in2,*) b10,'still be included in the control file; however,
     & they are'
      write (in2,*) b10,'enclosed by different delimiters, the number si
     &gns.'
      write (in2,*) b10,'Therefore the user can simply "activate" those 
     &input'
      write (in2,*) b10,'fields accordingly when the model options are c
     &hanged.'
      write (in2,*)
      write (in2,*) b10,'The "default values" mentioned below will only 
     &be specified by the'
      write (in2,*) b10,'Graphical User Interface (GUI) program.  OCD st
     &ill requires all'
      write (in2,*) b10,'necessary inputs be explicitly specified in the
     & control file.'
      write (in2,*) b10,'If you run OCD without the GUI, then all input 
     &variables whose'
      write (in2,*) b10,'values are not assigned in the control file wil
     &l be given a value'
      write (in2,*) b10,'of zero, which is not necessarily the default.'
      write (in2,*)
c
c     Group 2 (Mandatory)
c ////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
c     Control flag for normal run (=1) vs. test run (=0)
c
      dls=dls_real
      dle=dle_real
      write (in2,*) 'INPUT GROUP 2 -- Flag for normal run vs. test run (
     &mandatory)'
      write (in2,*) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     &%%%%%%%%%%%%%%%%%%%%%%%%'
      write (in2,*) dls,'INORMAL  = ',1       ,dle
      write (in2,*) b10,'INORMAL -- Flag for normal run vs. test run (0 
     &or 1)'
      write (in2,*) b10,'           = 1, Normal Run'
      write (in2,*) b10,'           = 0, Test   Run'
      write (in2,*) b10,'Default = 1'
      write (in2,*) dls,ender,dle
      write (in2,*)
c     Add an additional modeling option to decide whether a test run
c     is to be performed.  INORMAL=1 if actual run is to be performed,
c     INORMAL=0 if only test run is to be performed.  Default is
c     INORMAL=1
c
c     Group 3 (Mandatory)
c ////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
c     Control parameters and constants
c
      dls=dls_real
      dle=dle_real
      read (in,*) idate(1),idate(2),ihstrt,nper,navg,ipol,nsigp,nav5,
     &   contwo,celm
      write (in2,*) 'INPUT GROUP 3 -- Control parameters and constants (
     &mandatory)'
      write (in2,*) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     &%%%%%%%%%%%%%%%%%%%%%%%%'
      write (in2,*) dls,'IDATE(1) = ',idate(1),dle
      write (in2,*) b10,'IDATE(1) -- Starting year for this run (2 digit
     &s)'
      write (in2,*) b10,'            Only effective if PCRAMMET data fil
     &e is used (IOPT(5)'
      write (in2,*) b10,'            =0 or 2); if IOPT(5)=1, i.e., overl
     &and meteorological'
      write (in2,*) b10,'            data are included in the control fi
     &le, then the first'
      write (in2,*) b10,'            meteorological data record determin
     &es the starting date'
      write (in2,*) b10,'            and hour'
      write (in2,*) b10,'No default'
      write (in2,*) dls,'IDATE(2) = ',idate(2),dle
      write (in2,*) b10,'IDATE(2) -- Starting Julian day for this run (1
     & to 366)'
      write (in2,*) b10,'            Only effective if PCRAMMET data fil
     &e is used (IOPT(5)'
      write (in2,*) b10,'            =0 or 2); if IOPT(5)=1, i.e., overl
     &and meteorological'
      write (in2,*) b10,'            data are included in the control fi
     &le, then the first'
      write (in2,*) b10,'            meteorological data record determin
     &es the starting date'
      write (in2,*) b10,'            and hour'
      write (in2,*) b10,'No default'
      write (in2,*) dls,'IHSTRT   = ',ihstrt  ,dle
      write (in2,*) b10,'IHSTRT   -- Starting hour for this run (1 to 24
     &)'
      write (in2,*) b10,'No default'
      write (in2,*) dls,'NPER     = ',nper    ,dle
      write (in2,*) b10,'NPER     -- Length of run in no. of averaging p
     &eriods (>0)'
      write (in2,*) b10,'            NPER*NAVG is the length of run in h
     &ours.'
      write (in2,*) b10,'No default'
      write (in2,*) dls,'NAVG     = ',navg    ,dle
      write (in2,*) b10,'NAVG     -- No. of hours in an averaging period
     & (1 to 24)'
      write (in2,*) b10,'            NAVG is only used for output purpos
     &es.  (See IOPT(10),'
      write (in2,*) b10,'            IOPT(17), and IOPT(18) below.)  OCD
     & still performs'
      write (in2,*) b10,'            hourly dispersion calculations.'
      write (in2,*) b10,'            Note that NAVG is not to be confuse
     &d with the averaging'
      write (in2,*) b10,'            times (1, 3, 8, 24 hours, and NAV5 
     &hours if necessary)'
      write (in2,*) b10,'            used in the summary high-give table
     &s.  With the'
      write (in2,*) b10,'            appropriate output options selected
     &, comprehensive'
      write (in2,*) b10,'            printout for each NAVG-hour period 
     &will be generated.'
      write (in2,*) b10,'            For example, for a one-year (8760 h
     &ours) simulation,'
      write (in2,*) b10,'            then NAVG = 3 would mean that 2920 
     &(8760/3) tables will'
      write (in2,*) b10,'            be generated.  On the other hand, f
     &or the averaging'
      write (in2,*) b10,'            times considered in the summary hig
     &h-five tables,'
      write (in2,*) b10,'            the same amount of information is a
     &lways generated'
      write (in2,*) b10,'            regardless of the length of run.'
      write (in2,*) b10,'Default = 1'
      write (in2,*) dls,'IPOL     = ',ipol    ,dle
      write (in2,*) b10,'IPOL     -- Pollutant indicator (3 to 7)'
      write (in2,*) b10,'            = 3, SO2'
      write (in2,*) b10,'            = 4, TSP'
      write (in2,*) b10,'            = 5, NOX'
      write (in2,*) b10,'            = 6, CO'
      write (in2,*) b10,'            = 7, Others'
      write (in2,*) b10,'Default = 7'
      write (in2,*) dls,'NSIGP    = ',nsigp   ,dle
      write (in2,*) b10,'NSIGP    -- No. of significant point sources (0
     & to 25)'
      write (in2,*) b10,'            NSIGP = 0 means that no significant
     & sources will be'
      write (in2,*) b10,'            considered.  When NSIGP >0, OCD wil
     &l automatically'
      write (in2,*) b10,'            determine the ordering of the NSIGP
     & significant sources.'
      write (in2,*) b10,'            If necessary, the user can override
     & this selection and'
      write (in2,*) b10,'            specify a new ordering of the NSIGP
     & significant sources'
      write (in2,*) b10,'            through IOPT(7) and Input Group 7 d
     &escribed below.'
      write (in2,*) b10,'Default = 0'
      write (in2,*) dls,'NAV5     = ',nav5    ,dle
      write (in2,*) b10,'NAV5     -- An optional fifth averaging time, i
     &n hours, to be'
      write (in2,*) b10,'            included in the high-five table (0 
     &to 24, but'
      write (in2,*) b10,'            excluding 1, 3, 8, and 24 already c
     &onsidered by OCD)'
      write (in2,*) b10,'            0 means a fifth averaging time will
     & not be added.'
      write (in2,*) b10,'            Note that this averaging time is no
     &t to be confused'
      write (in2,*) b10,'            with NAVG (see notes above).'
      write (in2,*) b10,'Default = 0'
      write (in2,*) dls,'CONTWO   = ',contwo  ,dle
      write (in2,*) b10,'CONTWO   -- Conversion factor that converts use
     &r horizontal length'
      write (in2,*) b10,'            units (by multiplication) to kilome
     &ters (>0)'
      write (in2,*) b10,'            For example, CONTWO=0.001 if meters
     & are used as'
      write (in2,*) b10,'            user horizontal length units; CONTW
     &O=0.0003048'
      write (in2,*) b10,'            if feet are used as user horizontal
     & length units.'
      write (in2,*) b10,'No default'
      write (in2,*) dls,'CELM     = ',celm    ,dle
      write (in2,*) b10,'CONTWO   -- Conversion factor that converts use
     &r height'
      write (in2,*) b10,'            units (by multiplication) to meters
     & (>0)'
      write (in2,*) b10,'            For example, CELM=0.3048 if feet ar
     &e used as'
      write (in2,*) b10,'            user height units.'
      write (in2,*) b10,'No default'
      write (in2,*) dls,ender,dle
      write (in2,*)
c
c     NAVG cannot exceed 24; input met arrays can hold up to 24 hours
c
      if(navg.gt.24) then
         write (*,1255) navg
         stop
      endif
      if (nsigp.gt.25) then
         write (*,1250) nsigp
         stop
      endif
c
c     Group 4 (Mandatory)
c ////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
c     25 main modeling switches
c
      dls=dls_real
      dle=dle_real
      read (in,*) (iopt(i),i=1,25)
      write (in2,*) 'INPUT GROUP 4 -- Main model options (mandatory)'
      write (in2,*) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     &%%%%%%%%%%%%%%%%%%%%%%%%'
      write (in2,*) dls,'IOPT(1)  = ',iopt(1) ,dle
      write (in2,*) b10,'IOPT(1)  -- Switch for terrain adjustment (0 or
     & 1)'
      write (in2,*) b10,'            = 0, Do not use terrain adjustment'
      write (in2,*) b10,'            = 1, Use terrain adjustment'
      write (in2,*) b10,'Default = 0'
      write (in2,*) dls,'IOPT(2)  = ',iopt(2) ,dle
      write (in2,*) b10,'IOPT(2)  -- Switch for stack-tip downwash (0 or
     & 1)'
      write (in2,*) b10,'            = 0, Use stack-tip downwash'
      write (in2,*) b10,'            = 1, Do not use stack-tip downwash'
      write (in2,*) b10,'Default = 1'
      write (in2,*) dls,'IOPT(3)  = ',iopt(3) ,dle
      write (in2,*) b10,'IOPT(3)  -- Switch for gradual plume rise (0 or
     & 1)'
      write (in2,*) b10,'            = 0, Use gradual plume rise'
      write (in2,*) b10,'            = 1, Do not use gradual plume rise'
      write (in2,*) b10,'Default = 1'
      write (in2,*) dls,'IOPT(4)  = ',iopt(4) ,dle
      write (in2,*) b10,'IOPT(4)  -- Switch for buoyancy-induced dispers
     &ion (0 or 1)'
      write (in2,*) b10,'            = 0, Do not use buoyancy-induced di
     &spersion'
      write (in2,*) b10,'            = 1, Use buoyancy-induced dispersio
     &n'
      write (in2,*) b10,'Default = 0'
      write (in2,*) dls,'IOPT(5)  = ',iopt(5) ,dle
      write (in2,*) b10,'IOPT(5)  -- Switch for overland met data (0 to 
     &2)'
      write (in2,*) b10,'            = 0, From separate binary PCRAMMET 
     &file'
      write (in2,*) b10,'            = 1, ASCII met data included in the
     & control file'
      write (in2,*) b10,'            = 2, From separate ASCII PCRAMMET f
     &ile'
      write (in2,*) b10,'            If binary file is to be used, make 
     &sure that'
      write (in2,*) b10,'            the file is compatible with the For
     &tran compiler'
      write (in2,*) b10,'            used to compile OCD.'
      write (in2,*) b10,'No default'
      write (in2,*) dls,'IOPT(6)  = ',iopt(6) ,dle
      write (in2,*) b10,'IOPT(6)  -- Switch for emission rate (0 or 1)'
      write (in2,*) b10,'            = 0, Use constant emission rate spe
     &cified in the'
      write (in2,*) b10,'            control file'
      write (in2,*) b10,'            = 1, Use hourly emissions from a fi
     &le called EMIS.DAT'
      write (in2,*) b10,'No default'
      write (in2,*) dls,'IOPT(7)  = ',iopt(7) ,dle
      write (in2,*) b10,'IOPT(7)  -- Switch for user-specified significa
     &nt sources (0 or 1)'
      write (in2,*) b10,'            = 0, No significant sources specifi
     &ed by the user, and'
      write (in2,*) b10,'            OCD will determine significant sour
     &ces if necessary'
      write (in2,*) b10,'            = 1, Significant sources specified 
     &by the user in'
      write (in2,*) b10,'            Input Group 7'
      write (in2,*) b10,'            (IOPT(7) can = 1 only if NSIGP > 0)
     &'
      write (in2,*) b10,'Default = 0'
      write (in2,*) dls,'IOPT(8)  = ',iopt(8) ,dle
      write (in2,*) b10,'IOPT(8)  -- Switch for receptor type (0 to 6)'
      write (in2,*) b10,'            = 0, Discrete receptors only'
      write (in2,*) b10,'            = 1, Discrete and polar receptors'
      write (in2,*) b10,'            = 2, Discrete and Cartesian recepto
     &rs'
      write (in2,*) b10,'            = 3, Discrete, polar, and Cartesian
     & receptors'
      write (in2,*) b10,'            = 4, Polar receptors only'
      write (in2,*) b10,'            = 5, Cartesian receptors only'
      write (in2,*) b10,'            = 6, Polar and Cartesian receptors'
      write (in2,*) b10,'No default'
      write (in2,*) dls,'IOPT(9)  = ',iopt(9) ,dle
      write (in2,*) b10,'IOPT(9)  -- Switch for printout of emissions wi
     &th height table'
      write (in2,*) b10,'            (0 or 1)'
      write (in2,*) b10,'            = 0, Generate output'
      write (in2,*) b10,'            = 1, Do not generate output'
      write (in2,*) b10,'Default = 1'
      write (in2,*) dls,'IOPT(10) = ',iopt(10),dle
      write (in2,*) b10,'IOPT(10) -- Switch for printout of resultant me
     &t data summary'
      write (in2,*) b10,'            for averaging period (0 or 1)'
      write (in2,*) b10,'            = 0, Generate output'
      write (in2,*) b10,'            = 1, Do not generate output'
      write (in2,*) b10,'Default = 1'
      write (in2,*) dls,'IOPT(11) = ',iopt(11),dle
      write (in2,*) b10,'IOPT(11) -- Switch for printout of hourly contr
     &ibutions of'
      write (in2,*) b10,'            significant sources (0 or 1)'
      write (in2,*) b10,'            The printout lists the hourly contr
     &ibution from EACH'
      write (in2,*) b10,'            significant source for each recepto
     &r.  It also lists'
      write (in2,*) b10,'            the hourly concentration at each re
     &ceptor due to'
      write (in2,*) b10,'            (1) ALL significant sources, and (2
     &) ALL sources.'
      write (in2,*) b10,'            = 0, Generate output'
      write (in2,*) b10,'            = 1, Do not generate output'
      write (in2,*) b10,'            (IOPT(11) can = 0 only if NSIGP > 0
     &)'
      write (in2,*) b10,'Default = 1'
      write (in2,*) dls,'IOPT(12) = ',iopt(12),dle
      write (in2,*) b10,'IOPT(12) -- Switch for printout of met data on 
     &hourly contributions'
      write (in2,*) b10,'            (0 or 1)'
      write (in2,*) b10,'            = 0, Generate output'
      write (in2,*) b10,'            = 1, Do not generate output'
      write (in2,*) b10,'Default = 1'
      write (in2,*) dls,'IOPT(13) = ',iopt(13),dle
      write (in2,*) b10,'IOPT(13) -- Switch for case-study printout of p
     &lume transport and'
      write (in2,*) b10,'            dispersion on hourly contributions 
     &(0 or 1)'
      write (in2,*) b10,'            = 0, Generate output'
      write (in2,*) b10,'            = 1, Do not generate output'
      write (in2,*) b10,'Default = 1'
      write (in2,*) dls,'IOPT(14) = ',iopt(14),dle
      write (in2,*) b10,'IOPT(14) -- Switch for printout of hourly summa
     &ry of receptor'
      write (in2,*) b10,'            concentrations (0 or 1)'
      write (in2,*) b10,'            The printout lists the hourly conce
     &ntration at each'
      write (in2,*) b10,'            receptor due to (1) ALL significant
     & sources, and'
      write (in2,*) b10,'            (2) ALL sources.  Individual source
     & contributions are'
      write (in2,*) b10,'            not listed.  The receptor location,
     & height, and'
      write (in2,*) b10,'            elevation are also printed.'
      write (in2,*) b10,'            = 0, Generate output'
      write (in2,*) b10,'            = 1, Do not generate output'
      write (in2,*) b10,'Default = 1'
      write (in2,*) dls,'IOPT(15) = ',iopt(15),dle
      write (in2,*) b10,'IOPT(15) -- Switch for printout of met data on 
     &hourly summary'
      write (in2,*) b10,'            (0 or 1)'
      write (in2,*) b10,'            = 0, Generate output'
      write (in2,*) b10,'            = 1, Do not generate output'
      write (in2,*) b10,'            (This option currently has the same
     & effects as IOPT(12).'
      write (in2,*) b10,'            Thus, for consistence, IOPT(15) sho
     &uld equal IOPT(12).)'
      write (in2,*) b10,'Default = 1'
      write (in2,*) dls,'IOPT(16) = ',iopt(16),dle
      write (in2,*) b10,'IOPT(16) -- Switch for case-study printout of p
     &lume transport and'
      write (in2,*) b10,'            dispersion on hourly summary (0 or 
     &1)'
      write (in2,*) b10,'            = 0, Generate output'
      write (in2,*) b10,'            = 1, Do not generate output'
      write (in2,*) b10,'            (This option currently has the same
     & effects as IOPT(13).'
      write (in2,*) b10,'            Thus, for consistence, IOPT(16) sho
     &uld equal IOPT(13).)'
      write (in2,*) b10,'Default = 1'
      write (in2,*) dls,'IOPT(17) = ',iopt(17),dle
      write (in2,*) b10,'IOPT(17) -- Switch for printout of averaging pe
     &riod contributions'
      write (in2,*) b10,'            (0 or 1)'
      write (in2,*) b10,'            The printout is similar to the one 
     &generated by'
      write (in2,*) b10,'            IOPT(11), but is for each averaging
     & period (NAVG hours).'
      write (in2,*) b10,'            = 0, Generate output'
      write (in2,*) b10,'            = 1, Do not generate output'
      write (in2,*) b10,'            (IOPT(11) and IOPT(17) both = 0 is 
     &meaningful only if'
      write (in2,*) b10,'            NAVG > 1.)'
      write (in2,*) b10,'Default = 1'
      write (in2,*) dls,'IOPT(18) = ',iopt(18),dle
      write (in2,*) b10,'IOPT(18) -- Switch for printout of averaging pe
     &riod summary (0 or 1)'
      write (in2,*) b10,'            The printout is similar to the one 
     &generated by'
      write (in2,*) b10,'            IOPT(14), but is for each averaging
     & period (NAVG hours).'
      write (in2,*) b10,'            = 0, Generate output'
      write (in2,*) b10,'            = 1, Do not generate output'
      write (in2,*) b10,'            (IOPT(14) and IOPT(18) both = 0 is 
     &meaningful only if'
      write (in2,*) b10,'            NAVG > 1.)'
      write (in2,*) b10,'Default = 1'
      write (in2,*) dls,'IOPT(19) = ',iopt(19),dle
      write (in2,*) b10,'IOPT(19) -- Switch for printout of average conc
     &entrations and'
      write (in2,*) b10,'            high-five table for the entire run 
     &(0 or 1)'
      write (in2,*) b10,'            = 0, Generate output'
      write (in2,*) b10,'            = 1, Do not generate output'
      write (in2,*) b10,'Default = 0'
      write (in2,*) dls,'IOPT(20) = ',iopt(20),dle
      write (in2,*) b10,'IOPT(20) -- Switch for source type (0 to 2)'
      write (in2,*) b10,'            = 0, Point source'
      write (in2,*) b10,'            = 1, Area  source'
      write (in2,*) b10,'            = 2, Line  source'
      write (in2,*) b10,'Default = 0'
      write (in2,*) dls,'IOPT(21) = ',iopt(21),dle
      write (in2,*) b10,'IOPT(21) -- Switch for summary output file EXTR
     &A.OUT (0 or 1)'
      write (in2,*) b10,'            There is one line of output in the 
     &extra.out file for'
      write (in2,*) b10,'            each receptor/source pair and each 
     &hour when nonzero'
      write (in2,*) b10,'            concentration was predicted'
      write (in2,*) b10,'            = 0, Do not create the file'
      write (in2,*) b10,'            = 1, Create the file'
      write (in2,*) b10,'Default = 0'
      write (in2,*) dls,'IOPT(22) = ',iopt(22),dle
      write (in2,*) b10,'IOPT(22) -- Switch for hourly binary concentrat
     &ion file'
      write (in2,*) b10,'            CONC.BIN (0 or 1)'
      write (in2,*) b10,'            = 0, Do not create the file'
      write (in2,*) b10,'            = 1, Create the file'
      write (in2,*) b10,'Default = 1'
      write (in2,*) dls,'IOPT(23) = ',iopt(23),dle
      write (in2,*) b10,'IOPT(23) -- Switch for printout of table of ann
     &ual impact'
      write (in2,*) b10,'            assessment from non-permanent activ
     &ities (0 or 1)'
      write (in2,*) b10,'            For example, assume a facility oper
     &ates only 30 days'
      write (in2,*) b10,'            a year, and OCD is applied for that
     & 30-day period.'
      write (in2,*) b10,'            Then the annual impact due to that 
     &facility will be'
      write (in2,*) b10,'            30/365 of the averaging concentrati
     &on for the entire'
      write (in2,*) b10,'            run (run length = 30 days).'
      write (in2,*) b10,'            = 0, Do not generate output'
      write (in2,*) b10,'            = 1, Generate output'
      write (in2,*) b10,'Default = 0'
      write (in2,*) dls,'IOPT(24) = ',iopt(24),dle
      write (in2,*) b10,'IOPT(24) -- Switch for land source (do not modi
     &fy wind speed)'
      write (in2,*) b10,'            (0 or 1)'
      write (in2,*) b10,'            = 0, Modify wind speed'
      write (in2,*) b10,'            = 1, Do not modify wind speed'
      write (in2,*) b10,'            IOPT(24) should = 1 for land source
     &.'
      write (in2,*) b10,'No default'
      write (in2,*) dls,'IOPT(25) = ',iopt(25),dle
      write (in2,*) b10,'IOPT(25) -- Switch for pollutant decay rate via
     & chemical'
      write (in2,*) b10,'            transformation (0 or 1)'
      write (in2,*) b10,'            = 0, Do not specify decay rate (i.e
     &., no decay)'
      write (in2,*) b10,'            = 1, Specify decay rate'
      write (in2,*) b10,'Default = 0'
      write (in2,*) dls,ender,dle
      write (in2,*)
c
c     Group 5 (Mandatory)
c ////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
c     Surface roughness length over land, the minimum miss distance,
c     and the approximate latitude of the source region
c
      dls=dls_real
      dle=dle_real
      read (in,*) hane,z0l,zmin,slat
      write (in2,*) 'INPUT GROUP 5 -- Overland wind and terrain (mandato
     &ry)'
      write (in2,*) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     &%%%%%%%%%%%%%%%%%%%%%%%%'
      write (in2,*) dls,'HANE     = ',hane    ,dle
      write (in2,*) b10,'HANE     -- Overland anemometer height (m) (>0)
     &'
      write (in2,*) b10,'No default'
      write (in2,*) dls,'Z0L      = ',z0l     ,dle
      write (in2,*) b10,'Z0L      -- Overland surface roughness length (
     &m) (>0)'
      write (in2,*) b10,'No default'
      write (in2,*) dls,'ZMIN     = ',zmin    ,dle
      write (in2,*) b10,'ZMIN     -- Minimum miss distance for a plume a
     &bove the ground at the'
      write (in2,*) b10,'            receptor location (m) (>0)'
      write (in2,*) b10,'            Currently, EPA recommends setting Z
     &MIN = 10 m.  However,'
      write (in2,*) b10,'            before running OCD in complex terra
     &in, the local MMS'
      write (in2,*) b10,'            agency should be contacted for guid
     &ance in setting a'
      write (in2,*) b10,'            value for ZMIN.'
      write (in2,*) b10,'No default'
      write (in2,*) dls,'SLAT     = ',slat    ,dle
      write (in2,*) b10,'SLAT     -- Latitude of the source region (deg)
     & (-90 to 90)'
      write (in2,*) b10,'            (decimal notation; e.g., 25.675 deg
     & ='
      write (in2,*) b10,'            25 deg 40 min 30 sec)'
      write (in2,*) b10,'            (>0 for northern hemisphere)'
      write (in2,*) b10,'No default'
      write (in2,*) dls,ender,dle
      write (in2,*)
c
c     Group 6 (6a, 6b, and 6c) (Mandatory)
c ////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
c     Emission data
c
      dls=dls_real
      dle=dle_real
      last = 0
c
c     Begin loop to read the point source information
c
      do 200 j=1,maxp+1
c
c        +1 above is used to catch the number of point sources > maxp
c
      if(last.eq.0) then
c
      npt=npt+1
      if (npt.gt.maxp) then
         read (in,1200) dum
         if (dum.ne.endp) then
            write (*,1275) maxp
            stop
         else
            go to 239
         endif
      endif
c
      read (in,1215) pname(npt)
c
c     line with  'endp' in col 1-4 is used to signify end of sources
c
      if (pname(npt)(1:4).eq.endp) then
         last = 1
         go to 239
      endif
      read (in,*) (source(i,npt),i=1,9),elp(npt),source(11,npt)
c
c     Read extra line source information
c
      if (iopt(20).eq.2) read (in,*) xstop,ystop
c
239   continue
c
      else
c
         goto 230
c
      endif
c
200   continue
230   npt=npt-1
c
      write (in2,*) 'INPUT GROUP 6a -- Source description, no. of source
     &s (mandatory)'
      write (in2,*) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     &%%%%%%%%%%%%%%%%%%%%%%%%'
      write (in2,*) dls,'NPT      = ',npt    ,dle
      write (in2,*) b10,'NPT      -- No. of sources (>0)'
      write (in2,*) b10,'            For point sources, NPT cannot > MAX
     &P specified'
      write (in2,*) b10,'            in PARAMS.CMN'
      write (in2,*) b10,'            For area  sources, NPT cannot > 5'
      write (in2,*) b10,'            For line  sources, NPT must = 1'
      write (in2,*) b10,'No default'
      write (in2,*) dls,ender,dle
      write (in2,*)
      write (in2,*) 'INPUT GROUP 6b -- Source description, source parame
     &ters (mandatory)'
      write (in2,*) 'Any combination of stationary sources located offsh
     &ore or on land near the'
      write (in2,*) 'coast (with coastline resolution limitations taken 
     &into account) is allowed.'
      write (in2,*) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     &%%%%%%%%%%%%%%%%%%%%%%%%'
      write (in2,*) b10,'In the following, sets of source description re
     &cords'
      write (in2,*) b10,'are repeated NPT times, one set for each source
     &.'
      write (in2,*) b10,'Each set of source description records contains
     & at least'
      write (in2,*) b10,'two records, SRCNAM and SRCPAR.  A third record
     &, containing'
      write (in2,*) b10,'variables XSTOP and YSTOP, is required for line
     & source.'
      write (in2,*) b10,'SRCNAM   -- Source name (at most 12 characters,
     & no space)'
      write (in2,*) b10,'SRCPAR   -- Contains the following 11 parameter
     &s:'
      write (in2,*) b10,'       (1 ) X-coord of point source, user units
     &,'
      write (in2,*) b10,'            X-coord of circle center for area s
     &ource, user units,'
      write (in2,*) b10,'            X-coord of starting point for line 
     &source,  user units,'
      write (in2,*) b10,'       (2 ) Y-coord of point source, user units
     &,'
      write (in2,*) b10,'            Y-coord of circle center for area s
     &ource, user units,'
      write (in2,*) b10,'            Y-coord of starting point for line 
     &source,  user units,'
      write (in2,*) b10,'       (3 ) Pollutant emission rate (g/s) (>=0)
     &,'
      write (in2,*) b10,'            will not be used if IOPT(6)=1'
      write (in2,*) b10,'       (4 ) Height of building or obstacle at o
     &r near stack'
      write (in2,*) b10,'            location (m, >=0) relative to platf
     &orm or ground'
      write (in2,*) b10,'            level, depending upon base elevatio
     &n specified below'
      write (in2,*) b10,'       (5 ) Height of stack-top (m, >0.1) above
     & ground (if on land),'
      write (in2,*) b10,'            or above platform level (if at sea 
     &on "stilts"),'
      write (in2,*) b10,'            or above sea level (if floating on 
     &water)'
      write (in2,*) b10,'       (6 ) Stack gas temperature (K) (must be 
     &at least as high as'
      write (in2,*) b10,'            the ambient temperature)' 
      write (in2,*) b10,'       (7 ) Stack-top inside diameter (m, >0) f
     &or point or line'
      write (in2,*) b10,'            sources, circle diameter (m, >0) fo
     &r area sources'
      write (in2,*) b10,'       (8 ) Stack gas exit velocity (m/s) (>0)'
      write (in2,*) b10,'       (9 ) Deviation of stack angle from the v
     &ertical (deg)'
      write (in2,*) b10,'            (0 to 180)'
      write (in2,*) b10,'            0 means the stack is vertical, <90 
     &means the stack'
      write (in2,*) b10,'            has an upward pointing component, >
     &90 means the stack'
      write (in2,*) b10,'            has a downward pointing component.
     &'
      write (in2,*) b10,'       (10) Elevation of ground or platform bas
     &e at stack'
      write (in2,*) b10,'            location relative to the water surf
     &ace, user units'
      write (in2,*) b10,'       (11) Building width used to computer pla
     &tform downwash'
      write (in2,*) b10,'            (m) (>=0)'
      write (in2,*) b10,'            (=0 means no downwash)'
      write (in2,*) b10,'No default'
      write (in2,*)
      do i=1,npt
      write (in2,*) dls,'SRCNAM   = ',pname(i),dle
      write (in2,61) dls,(source(j,i),j=1,9),elp(i),source(11,i),dle
      write (in2,*) dls,ender,dle
      write (in2,*)
      end do
c
      write (in2,*) 'INPUT GROUP 6c -- Source description, line source p
     &arameters (conditinal)'
      write (in2,*) '(Only necessary if IOPT(20)=2)'
      write (in2,*) 'Modeling of line sources is limited to screening ty
     &pe analyses of 24 hours'
      write (in2,*) 'or less.'
      write (in2,*) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     &%%%%%%%%%%%%%%%%%%%%%%%%'
      if (iopt(20).eq.2) then
        dls=dls_real
        dle=dle_real
      else
        dls=dls_comment
        dle=dle_comment
        xstop=0.   ! Fake data
        ystop=0.
      end if
c
      write (in2,*) dls,'XSTOP    = ',xstop  ,dle
      write (in2,*) b10,'XSTOP    -- X-coord of ending point for line so
     &urce, user units'
      write (in2,*) b10,'No default'
      write (in2,*) dls,'YSTOP    = ',ystop  ,dle
      write (in2,*) b10,'YSTOP    -- Y-coord of ending point for line so
     &urce, user units'
      write (in2,*) b10,'No default'
      write (in2,*) dls,ender,dle
      write (in2,*)
c
c     For area sources, check to make sure the number of circles is not
c     greater than 5
c
      if ( iopt(20) .eq. 1 .and. npt .gt. 5) then
         write (*,1605) npt
         stop
      endif
c
c     Check for npt < or = 0
c
      if (npt.le.0) then
         write (*,1280) npt
         stop
      endif
c
c     Group 7 (Conditional)
c ////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
c     Significant source
c
      if (iopt(7).eq.1) then
c
         read (in,*) inpt,(mps(i),i=1,inpt)
c
         do i=1,inpt
           if (mps(i).eq.0) then
              write (*,1300)
              stop
           end if
           write (cmps(i),'(i2)') mps(i)
         end do
c
         if (inpt.ne.nsigp) then
            write (*,1290) inpt,nsigp
            stop
         endif
c
      endif
c
      if (iopt(7).eq.1) then
         dls=dls_real
         dle=dle_real
      else
         dls=dls_comment
         dle=dle_comment
         inpt=1    ! Fake a number for significant sources
         cmps(1)='1'
      end if
c
      write (in2,*) 'INPUT GROUP 7 -- User-specified significant sources
     & (conditinal)'
      write (in2,*) '(Only necessary if IOPT(7)=1 and NSIGP > 0)'
      write (in2,*) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     &%%%%%%%%%%%%%%%%%%%%%%%%'
      write (in2,*) dls,'SRCNUM   = ',(cmps(i),', ',i=1,inpt-1),
     &                 cmps(inpt),dle
      write (in2,*) b10,'SRCNUM   -- Source numbers for the NSIGP signif
     &icant sources, where'
      write (in2,*) b10,'            NSIGP was previously specified in t
     &he control file.'
      write (in2,*) b10,'            For example, SRCNUM = 2, 1, 5 means
     & that source'
      write (in2,*) b10,'            number 2 is the first significant s
     &ource, source'
      write (in2,*) b10,'            number 1 is the second significant 
     &source, and'
      write (in2,*) b10,'            source number 5 is the third signif
     &icant source.'
      write (in2,*) dls,ender,dle
      write (in2,*)
c
c     Group 8 (Conditional)
c ////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
c     Overland met data identifiers
c
      if (iopt(5).eq.0) then
         read (in,*) isfcd,isfcyr,imxd,imxyr
      end if
c
      if (iopt(5).eq.0) then
         dls=dls_real
         dle=dle_real
      else
         dls=dls_comment
         dle=dle_comment
         isfcd=99999  ! Fake a station number
         isfcyr=97    ! Fake a year
         imxd=99999
         imxyr=97
      end if
c
      write (in2,*) 'INPUT GROUP 8 -- Overland meteorological data ident
     &ifiers (conditinal)'
      write (in2,*) '(Only necessary if IOPT(5)=0 or 2)'
      write (in2,*) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     &%%%%%%%%%%%%%%%%%%%%%%%%'
      write (in2,*) dls,'ISFCD    = ',isfcd  ,dle
      write (in2,*) b10,'ISFCD    -- surface station identifier code (5 
     &digit)'
      write (in2,*) b10,'No default'
      write (in2,*) dls,'ISFCYR   = ',isfcyr ,dle
      write (in2,*) b10,'ISFCYR   -- year of surface data (2 digit)'
      write (in2,*) b10,'No default'
      write (in2,*) dls,'IMXD     = ',imxd   ,dle
      write (in2,*) b10,'IMXD     -- upper air station identifier code (
     &5 digit)'
      write (in2,*) b10,'No default'
      write (in2,*) dls,'IMXYR    = ',imxyr  ,dle
      write (in2,*) b10,'IMXYR    -- year of upper air data (2 digit)'
      write (in2,*) b10,'No default'
      write (in2,*) dls,ender,dle
      write (in2,*)
c
c     Group 9 (Conditional)
c ////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
c     Polar coordinate receptors
c
      if (iopt(8).eq.1) then
c
      read (in,*) (radil(i),i=1,5),centx,centy
c
      do 460 j=1,5
         if (radil(j).ne.0) ja=ja+1
460   continue
c
      end if
c
      if (iopt(8).eq.1) then
         dls=dls_real
         dle=dle_real
      else
         dls=dls_comment
         dle=dle_comment
         ja=5    ! Fake a number for rings
         centx=0.
         centy=0.
         radil(1)=1.
         radil(2)=2.
         radil(3)=3.
         radil(4)=4.
         radil(5)=5.
      end if
c
      write (in2,*) 'INPUT GROUP 9 -- Polar coordinate receptors, no. of
     & rings (conditional)'
      write (in2,*) '(Only necessary if IOPT(8)=1, 3, 4, or 6)'
      write (in2,*) 'Any combination of receptors located at sea or on l
     &and is allowed.'
      write (in2,*) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     &%%%%%%%%%%%%%%%%%%%%%%%%'
      write (in2,*) dls,'NRING    = ',ja     ,dle
      write (in2,*) b10,'NRING    -- No. of rings for polar receptors'
      write (in2,*) b10,'            (1 to MAXRING specified in PARAMS.C
     &MN)'
      write (in2,*) b10,'            The total number of polar receptors
     & is NRING*36.'
      write (in2,*) b10,'            The sum of polar, Cartesian, and di
     &screte receptors'
      write (in2,*) b10,'            cannot > MAXREC specified in PARAMS
     &.CMN.'
      write (in2,*) b10,'Default = 5'
      write (in2,*) dls,'CENTX    = ',centx  ,dle
      write (in2,*) b10,'CENTX    -- X-coord of the center of rings, use
     &r units'
      write (in2,*) b10,'No default'
      write (in2,*) dls,'CENTY    = ',centy  ,dle
      write (in2,*) b10,'CENTY    -- Y-coord of the center of rings, use
     &r units'
      write (in2,*) b10,'No default'
      do i=1,ja
        write (cradil(i),'(f12.3)') radil(i)
      end do
      write (in2,*) dls,'RADII    = ',(cradil(i),', ',i=1,ja-1),
     &                               cradil(ja) ,dle
      write (in2,*) b10,'RADII    -- Radial distances for the NRING ring
     &s, user units (>0)'
      write (in2,*) b10,'No default'
      write (in2,*) dls,ender,dle
      write (in2,*)
c
c     Group 10 (Conditional)
c ////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
c     Polar coordinate receptor elevations
c
      if (iopt(8).eq.1 .and. iopt(1).eq.1) then
c
      do 510 i=1,36
         read (in,*) isector,(elrdum(i,j),j=1,ja)
         if (isector.ne.i) then
            write (*,1330) isector,i
            stop
         endif
         do j=1,ja
           write (celrdum(i,j),'(f12.3)') elrdum(i,j)
         end do
510   continue
c
      end if
c
      if (iopt(8).eq.1 .and. iopt(1).eq.1) then
         dls=dls_real
         dle=dle_real
      else
         dls=dls_comment
         dle=dle_comment
         ja=5    ! Fake a number for rings
         do i=1,36
         do j=1,ja
         celrdum(i,j)='0.'
         end do
         end do
      end if
c
      write (in2,*) 'INPUT GROUP 10 -- Polar coordinate receptor elevati
     &ons (conditional)'
      write (in2,*) '(Only necessary if IOPT(8)=1, 3, 4, or 6; and IOPT(
     &1)=1)'
      write (in2,*) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     &%%%%%%%%%%%%%%%%%%%%%%%%'
      write (in2,*) b10,'In the following, for each azimuth (sector), th
     &e ground elevations'
      write (in2,*) b10,'for the receptors along that azimuth are specif
     &ied in user height'
      write (in2,*) b10,'units in the order that the ring distances are 
     &specified above.'
      write (in2,*) b10,'ISECTOR  -- Azimuth indicator (1 to 36)'
      write (in2,*) b10,'            1 = the 10-degree sector,'
      write (in2,*) b10,'            2 = the 20-degree sector,'
      write (in2,*) b10,'            3 = the 30-degree sector, etc.'
      write (in2,*) b10,'            Must be in the order of 1 to 36.'
      write (in2,*) b10,'ELEV     -- Ground elevations relative to the w
     &ater surface for the'
      write (in2,*) b10,'            NRING receptors along the ISECTOR-t
     &h radial,'
      write (in2,*) b10,'            user height units'
      write (in2,*) b10,'No default'
      write (in2,*)
c
      do 520 i=1,36
         write (in2,*) dls,'ISECTOR  = ',i,dle
         write (in2,*) dls,'ELEV     = ',(celrdum(i,j),', ',j=1,ja-1),
     &                                  celrdum(i,ja) ,dle
         write (in2,*) dls,ender,dle
         write (in2,*)
520   continue
c
c     Group 11 (Conditional) (A new group added to OCD5)
c ////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
c     Cartesian receptors
c
      dls=dls_comment
      dle=dle_comment
      x0_c=0.  ! Fake some data
      y0_c=0.
      nx_c=20
      ny_c=20
      delx_c=1.
      dely_c=1.
      write (in2,*) 'INPUT GROUP 11 -- Cartesian coordinate receptors (c
     &onditional)'
      write (in2,*) '(Only necessary if IOPT(8)=2, 3, 5, or 6)'
      write (in2,*) 'Any combination of receptors located at sea or on l
     &and is allowed.'
      write (in2,*) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     &%%%%%%%%%%%%%%%%%%%%%%%%'
      write (in2,*) dls,'X0_C     = ',x0_c    ,dle
      write (in2,*) b10,'X0_C     -- X-coord of the NORTHWEST corner of 
     &the Cartesian'
      write (in2,*) b10,'            receptor network, user units'
      write (in2,*) b10,'No default'
      write (in2,*) dls,'Y0_C     = ',y0_c    ,dle
      write (in2,*) b10,'Y0_C     -- Y-coord of the NORTHWEST corner of 
     &the Cartesian'
      write (in2,*) b10,'            receptor network, user units'
      write (in2,*) b10,'No default'
      write (in2,*) dls,'NX_C     = ',nx_c    ,dle
      write (in2,*) b10,'NX_C     -- Number of receptors along the x-axi
     &s'
      write (in2,*) b10,'            (1 to MAXCAR specified in PARAMS.CM
     &N)'
      write (in2,*) b10,'Default = 20'
      write (in2,*) dls,'NY_C     = ',ny_c    ,dle
      write (in2,*) b10,'NY_C     -- Number of receptors along the y-axi
     &s'
      write (in2,*) b10,'            (1 to MAXCAR specified in PARAMS.CM
     &N)'
      write (in2,*) b10,'            The total number of Cartesian recep
     &tors is NX_C*NY_C.'
      write (in2,*) b10,'            The sum of polar, Cartesian, and di
     &screte receptors'
      write (in2,*) b10,'            cannot > MAXREC specified in PARAMS
     &.CMN.'
      write (in2,*) b10,'Default = 20'
      write (in2,*) dls,'DELX_C   = ',delx_c  ,dle
      write (in2,*) b10,'DELX_C   -- The distance between successive Car
     &tesian receptors'
      write (in2,*) b10,'            along the x-axis, user units (>0)'
      write (in2,*) b10,'No default'
      write (in2,*) dls,'DELY_C   = ',dely_c  ,dle
      write (in2,*) b10,'DELY_C   -- The distance between successive Car
     &tesian receptors'
      write (in2,*) b10,'            along the y-axis, user units (>0)'
      write (in2,*) b10,'No default'
      write (in2,*) dls,ender,dle
      write (in2,*)
c
c     Group 12 (Conditional) (A new group added to OCD5)
c ////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
c     Cartesian receptors elevations
c
      write (in2,*) 'INPUT GROUP 12 -- Cartesian coordinate receptors el
     &evations (conditional)'
      write (in2,*) '(Only necessary if IOPT(8)=2, 3, 5, or 6; and IOPT(
     &1)=1)'
      write (in2,*) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     &%%%%%%%%%%%%%%%%%%%%%%%%'
      write (in2,*) b10,'In the following, for each row (from north to s
     &outh, NY_C rows'
      write (in2,*) b10,'in total) of receptors, a row indicator and NX_
     &C values, representing'
      write (in2,*) b10,'elevations of the receptors along that row (fro
     &m west to east),'
      write (in2,*) b10,'are specified.'
      write (in2,*) b10,'IROW     -- Row indicator (1 to NY_C, must be i
     &n that order)'
      write (in2,*) b10,'ELEV     -- Ground elevations relative to the w
     &ater surface for the'
      write (in2,*) b10,'            NX_C receptors along the IROW-th ro
     &w, user height units'
      write (in2,*) b10,'No default'
      write (in2,*)
c
      do 525 i=1,ny_c
         write (in2,*) dls,'IROW     = ',i,dle
         write (in2,*) dls,'ELEV     = ',('0, ',j=1,nx_c-1),'0',dle
         write (in2,*) dls,ender,dle
         write (in2,*)
525   continue
c
c     Group 13 (13a and 13b) (old Group 12) (Conditional)
c ////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
c     Discrete receptors
c
      dls=dls_real
      dle=dle_real
      DO 545 I=1,maxrec+1
c
c     +1 above is used to catch the number of receptors > maxrec
c
         nrecep=nrecep+1
         if (nrecep.gt.maxrec) then
            read (in,1200,end=530) dum
            if (dum.eq.endr) go to 550
530         write (*,1340) maxrec
            stop
         endif
c
         read (in,1240) rname(nrecep),rrec(nrecep),srec(nrecep),
     &                  zr(nrecep),elr(nrecep),hter(nrecep)
c
c        place 'ENDR' in cols 1 to 4 on card following last receptor
c        to end reading group 12.
c
         if (rname(nrecep)(1:4).eq.endr) go to 550
545   continue
550   nrecep=nrecep-1
c
      if (nrecep.le.0) then
         write (*,1350) nrecep
         stop
      endif
c
      if (nrecep.eq.0) then
         print *,'OCD4TO5 cannot find any discrete receptors in the orig
     &inal control file.'
         print *,'As a result, the program cannot be successfully comple
     &ted.'
         stop
      end if
c
      write (in2,*) 'INPUT GROUP 13a -- Discrete receptors (conditional)
     &'
      write (in2,*) '(Only necessary if IOPT(8)=0, 1, 2, or 3)'
      write (in2,*) 'Any combination of receptors located at sea or on l
     &and is allowed.'
      write (in2,*) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     &%%%%%%%%%%%%%%%%%%%%%%%%'
      write (in2,*) dls,'NDISC    = ',nrecep ,dle
      write (in2,*) b10,'NDISC    -- No. of discrete receptors (>0)'
      write (in2,*) b10,'            The sum of discrete, polar, and Car
     &tesian receptors'
      write (in2,*) b10,'            cannot > MAXREC specified in PARAMS
     &.CMN'
      write (in2,*) b10,'No default'
      write (in2,*) dls,ender,dle
      write (in2,*)
      write (in2,*) 'INPUT GROUP 13b -- Discrete receptors (conditional)
     &'
      write (in2,*) '(Only necessary if IOPT(8)=0, 1, 2, or 3)'
      write (in2,*) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     &%%%%%%%%%%%%%%%%%%%%%%%%'
      write (in2,*) b10,'In the following, sets of discrete receptor des
     &cription records'
      write (in2,*) b10,'are repeated NDISC times, one set for each disc
     &rete receptor.'
      write (in2,*) b10,'Each set contains two records, RCPNAM and RCPPA
     &R.'
      write (in2,*) b10,'RCPNAM   -- Receptor name (at most 8 characters
     &, no space)'
      write (in2,*) b10,'RCPPAR   -- Contains the following 5 parameters
     &:'
      write (in2,*) b10,'        (1) X-coord of receptor, user units'
      write (in2,*) b10,'        (2) Y-coord of receptor, user units'
      write (in2,*) b10,'        (3) Receptor height above local ground 
     &level (m) (>=0)'
      write (in2,*) b10,'        (4) Ground elevation relative to the wa
     &ter surface at'
      write (in2,*) b10,'            receptor location, user units'
      write (in2,*) b10,'        (5) Terrain elevation toward which sour
     &ce to receptor'
      write (in2,*) b10,'            is aligned (used for Hc calculation
     &) (m)'
      write (in2,*) b10,'No default'
      write (in2,*)
      do i=1,nrecep
      write (in2,*) dls,'RCPNAM   = ',rname(i),dle
      write (in2,71) dls,rrec(i),srec(i),zr(i),elr(i),hter(i),dle
      write (in2,*) dls,ender,dle
      write (in2,*)
      end do
c
c     Groups 14 (old group 13) (Mandatory)
c ////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
c     Special options
c
      dls=dls_real
      dle=dle_real
      read (in,*) (jopt(i),i=1,9),hwane,hwt
      write (in2,*) 'INPUT GROUP 14 -- Special options for additional me
     &t data (mandatory)'
      write (in2,*) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     &%%%%%%%%%%%%%%%%%%%%%%%%'
      write (in2,*) dls,'JOPT(1)  = ',jopt(1) ,dle
      write (in2,*) b10,'JOPT(1)  -- Switch for overwater wind direction
     & data (0 or 1)'
      write (in2,*) b10,'            = 0, Not provided or do not use'
      write (in2,*) b10,'            = 1, Provided'
      write (in2,*) b10,'No default'
      write (in2,*) dls,'JOPT(2)  = ',jopt(2) ,dle
      write (in2,*) b10,'JOPT(2)  -- Switch for overwater wind speed dat
     &a (0 or 1)'
      write (in2,*) b10,'            = 0, Not provided or do not use'
      write (in2,*) b10,'            = 1, Provided'
      write (in2,*) b10,'No default'
      write (in2,*) dls,'JOPT(3)  = ',jopt(3) ,dle
      write (in2,*) b10,'JOPT(3)  -- Switch for overwater vertical poten
     &tial temperature'
      write (in2,*) b10,'            gradient data (0 or 1)'
      write (in2,*) b10,'            = 0, Not provided or do not use'
      write (in2,*) b10,'            = 1, Provided'
      write (in2,*) b10,'No default'
      write (in2,*) dls,'JOPT(4)  = ',jopt(4) ,dle
      write (in2,*) b10,'JOPT(4)  -- Switch for overwater humidity data 
     &(1 to 3)'
      write (in2,*) b10,'            = 1, Relative humidity (%) is provi
     &ded'
      write (in2,*) b10,'            = 2, Web bulb temperature (K) is pr
     &ovided'
      write (in2,*) b10,'            = 3, Dew point temperature (K) is p
     &rovided'
      write (in2,*) b10,'No default'
      write (in2,*) dls,'JOPT(5)  = ',jopt(5) ,dle
      write (in2,*) b10,'JOPT(5)  -- Switch for overland horizontal and 
     &vertical turbulence'
      write (in2,*) b10,'            intensity data (0 or 1)'
      write (in2,*) b10,'            = 0, Not provided or do not use'
      write (in2,*) b10,'            = 1, Provided'
      write (in2,*) b10,'No default'
      write (in2,*) dls,'JOPT(6)  = ',jopt(6) ,dle
      write (in2,*) b10,'JOPT(6)  -- Switch for water surface temperatur
     &e data (1 or 2)'
      write (in2,*) b10,'            = 1, Water surface temperature (K) 
     &is provided'
      write (in2,*) b10,'            = 2, Air minus water surface temper
     &ature (K) is provided'
      write (in2,*) b10,'No default'
      write (in2,*) dls,'JOPT(7)  = ',jopt(7) ,dle
      write (in2,*) b10,'JOPT(7)  -- Switch for overwater wind direction
     & shear data (0 or 1)'
      write (in2,*) b10,'            = 0, Not provided or do not use'
      write (in2,*) b10,'            = 1, Provided'
      write (in2,*) b10,'No default'
      write (in2,*) dls,'JOPT(8)  = ',jopt(8) ,dle
      write (in2,*) b10,'JOPT(8)  -- Switch for overwater horizontal tur
     &bulence intensity'
      write (in2,*) b10,'            data (0 or 1)'
      write (in2,*) b10,'            = 0, Not provided or do not use'
      write (in2,*) b10,'            = 1, Provided'
      write (in2,*) b10,'No default'
      write (in2,*) dls,'JOPT(9)  = ',jopt(9) ,dle
      write (in2,*) b10,'JOPT(9)  -- Switch for overwater vertical turbu
     &lence intensity'
      write (in2,*) b10,'            data (0 or 1)'
      write (in2,*) b10,'            = 0, Not provided or do not use'
      write (in2,*) b10,'            = 1, Provided'
      write (in2,*) b10,'No default'
      write (in2,*) dls,'HWANE    = ',hwane   ,dle
      write (in2,*) b10,'HWANE    -- Height above water level of overwat
     &er anemometer, m (>0)'
      write (in2,*) b10,'No default'
      write (in2,*) dls,'HWT      = ',hwt     ,dle
      write (in2,*) b10,'HWT      -- Height above water level of overwat
     &er air'
      write (in2,*) b10,'            temperature sensor, m (>0)'
      write (in2,*) b10,'No default'
      write (in2,*) dls,ender,dle
      write (in2,*)
c
c     Groups 15 (old group 14) (Conditional)
c ////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
c     Chemical transformation rate specifications
c
      if(iopt(25).eq.1) then
c
         read (in,*) alat,along,tzone
         if(alat.le.90.0.and.alat.ge.-90.0) go to 623
         write (*,1830) alat
         stop
623      if(along.le.180.0.and.along.ge.-180.0) go to 625
         write (*,1840) along
         stop
625      if(tzone.le.12.and.tzone.ge.-13.) go to 628
         write (*,1850) tzone
         stop
628      continue
         read (in,*) (decay(i),i=1,12)
c
      end if
c
      if(iopt(25).eq.1) then
         dls=dls_real
         dle=dle_real
      else
         dls=dls_comment
         dle=dle_comment
         alat=slat
         along=75.    ! Fake a longitude
         tzone=5.
         do i=1,12
         decay(i)=0.
         end do
      end if
c
      write (in2,*) 'INPUT GROUP 15 -- Chemical transformation rates'
      write (in2,*) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     &%%%%%%%%%%%%%%%%%%%%%%%%'
      write (in2,*) '(Only necessary if IOPT(25)=1)'
      write (in2,*) dls,'ALAT     = ',alat    ,dle
      write (in2,*) b10,'ALAT     -- Latitude of the site (deg) (-90 to 
     &90)'
      write (in2,*) b10,'            (decimal notation)'
      write (in2,*) b10,'            (>0 for northern hemisphere)'
      write (in2,*) b10,'            (should be the same as SLAT previou
     &sly defined)'
      write (in2,*) b10,'No default'
      write (in2,*) dls,'ALONG    = ',along   ,dle
      write (in2,*) b10,'ALONG    -- Longitude of the site (deg) (-180 t
     &o 180)'
      write (in2,*) b10,'            (decimal notation)'
      write (in2,*) b10,'            (>0 for western hemisphere)'
      write (in2,*) b10,'No default'
      write (in2,*) dls,'TZONE    = ',tzone   ,dle
      write (in2,*) b10,'TZONE    -- Time zone of the site'
      write (in2,*) b10,'            Time zone is defined as the number 
     &of hours behind GMT'
      write (in2,*) b10,'            (-12 to 11)'
      write (in2,*) b10,'            For example, TZONE = 5. for EST.'
      write (in2,*) b10,'No default'
      write (in2,81) dls,(decay(i),i=1,12),dle
      write (in2,*) b10,'DECAY    -- 12 monthly values of the pollutant 
     &decay rate'
      write (in2,*) b10,'            (%/hour) (>=0)'
      write (in2,*) b10,'            =0 means no decay (or chemical tran
     &sformation)'
      write (in2,*) b10,'            No chemical transformation is consi
     &dered during'
      write (in2,*) b10,'            nighttime hours'
      write (in2,*) b10,'No default'
      write (in2,*) dls,ender,dle
      write (in2,*)
c
c     Group 16 (old group 15) (Mandatory)
c ////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
c     Shoreline geometry
c
c     Construct format statement
c
      dls=dls_real
      dle=dle_real
      write (ifmt,111) maxmap
c
      read (in,*) x0,y0,nx,ny,delx,dely,wmin,avgdist
      if (nx.gt.maxmap .or. ny.gt.maxmap) then
         write (*,1395) nx,ny,maxmap
         stop
      endif
      write (in2,*) 'INPUT GROUP 16a -- Shoreline geometry (mandatory)'
      write (in2,*) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     &%%%%%%%%%%%%%%%%%%%%%%%%'
      write (in2,*) dls,'X0       = ',x0      ,dle
      write (in2,*) b10,'X0       -- X-coord of the NORTHWEST corner of 
     &the mapped area,'
      write (in2,*) b10,'            user units'
      write (in2,*) b10,'No default'
      write (in2,*) dls,'Y0       = ',y0      ,dle
      write (in2,*) b10,'Y0       -- Y-coord of the NORTHWEST corner of 
     &the mapped area,'
      write (in2,*) b10,'            user units'
      write (in2,*) b10,'No default'
      write (in2,*) dls,'NX       = ',nx      ,dle
      write (in2,*) b10,'NX       -- Number of grid rectangles along the
     & x-axis'
      write (in2,*) b10,'            (1 to MAXMAP specified in PARAMS.CM
     &N)'
      write (in2,*) b10,'No default'
      write (in2,*) dls,'NY       = ',ny      ,dle
      write (in2,*) b10,'NY       -- Number of grid rectangles along the
     & y-axis'
      write (in2,*) b10,'            (1 to MAXMAP specified in PARAMS.CM
     &N)'
      write (in2,*) b10,'No default'
      write (in2,*) dls,'DELX     = ',delx    ,dle
      write (in2,*) b10,'DELX     -- The length of each grid delta-x, us
     &er units (>0)'
      write (in2,*) b10,'No default'
      write (in2,*) dls,'DELY     = ',dely    ,dle
      write (in2,*) b10,'DELY     -- The length of each grid delta-y, us
     &er units (>0)'
      write (in2,*) b10,'No default'
      write (in2,*) dls,'WMIN     = ',wmin    ,dle
      write (in2,*) b10,'WMIN     -- The minimum along wind width for a 
     &land or water body'
      write (in2,*) b10,'            to be considered significant, user 
     &units (>0)'
      write (in2,*) b10,'            WMIN allows OCD to neglect insignif
     &icant water bodies'
      write (in2,*) b10,'            or land masses (e.g., a very narrow
     & barrier beach or'
      write (in2,*) b10,'            a very narrow lagoon).  As a guidan
     &ce value, a maximum'
      write (in2,*) b10,'            of ten times the estimated plume he
     &ight is suggested'
      write (in2,*) b10,'            for WMIN.'
      write (in2,*) b10,'No default'
      write (in2,*) dls,'AVGDIST  = ',avgdist ,dle
      write (in2,*) b10,'AVGDIST  -- The average distance from source to
     & shoreline,'
      write (in2,*) b10,'            user units (>0)'
      write (in2,*) b10,'            AVGDIST is only used to determine t
     &he range of acceptable'
      write (in2,*) b10,'            values for DELX and DELY, and is no
     &t used in actual'
      write (in2,*) b10,'            calculations.'
      write (in2,*) b10,'No default'
      write (in2,*) dls,ender,dle
      write (in2,*)
c
c   Check to make sure grid lengths meet the required grid
c   standardization criteria
c     delxk = delx*contwo
c     delyk = dely*contwo
c     if(avgdist*contwo .le. 2.0) then
c        if(delxk.lt.0.05 .or. delxk.gt.0.08) then
c           write (*,1000) delxk,avgdist
c           stop
c        elseif(delyk.lt.0.03 .or. delyk.gt.0.06) then
c           write (*,1010) delyk,avgdist
c           stop
c        endif
c     else
c        if(delxk.lt.0.2 .or. delxk.gt.0.4) then
c           write (*,1000) delxk,avgdist
c           stop
c        elseif(delyk.lt.0.1 .or. delyk.gt.0.3) then
c           write (*,1010) delyk,avgdist
c           stop
c        endif
c     endif
c
      do 660 i = 1,ny
         read (in,ifmt,err=670) (xymap(i,j),j=1,nx)
         xlet = xymap(i,1)
         do 655 j = 2,nx
            if(xymap(i,j).ne.blnk) then
               xlet = xymap(i,j)
            else
               xymap(i,j) = xlet
            endif
655      continue
660   continue
c
      read (in,1200) flag
      if(flag.ne.ends) then
         write (*,1397)
         stop
      endif
c
      write (ifmt2,121) nx
      write (ifmt3,131) nx
      write (in2,*) 'INPUT GROUP 16b -- Shoreline geometry (mandatory)'
      write (in2,*) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     &%%%%%%%%%%%%%%%%%%%%%%%%'
      write (in2,*) b10,'In the following, for each row (from north to s
     &outh) of grid'
      write (in2,*) b10,'rectangles to be mapped, a series of L''s or W'
     &'s (from west to east)'
      write (in2,*) b10,'is used to represent the distribution of (L)and
     & vs. (W)ater.'
      write (in2,*) b10,'Note that only the letters L and W are allowed.
     &'
      write (in2,*) b10,'There should be NY rows of data, and each row s
     &hould have NX letters'
      write (in2,*) b10,'of either L or W.'
      write (in2,*)
      write (in2,*) b10,'Although it is not necessary for receptors or s
     &ources to be located'
      write (in2,*) b10,'on the grid, it is very important that a gridde
     &d land sector be'
      write (in2,*) b10,'included between all sources and overland recep
     &tors.  Otherwise,'
      write (in2,*) b10,'the model assumes the receptor is over water.'
      write (in2,*)
      write (in2,*) dls,'LWFLAG ='
      do i=1,ny-1
         write (in2,ifmt2) (xymap(i,j),j=1,nx)
      end do
      write (in2,ifmt3) (xymap(ny,j),j=1,nx)
      write (in2,*) dls,ender,dle
      write (in2,*)
c
c     Group 17 header (old group 16) (Conditional)
c ////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
c     Overland met data embedded in control file
c
c
      if(iopt(5).eq.1) then
         dls=dls_real
         dle=dle_real
      else
         dls=dls_comment
         dle=dle_comment
      end if
c
      write (in2,*) b10,'If overland met data are to be included in the 
     &control file,'
      write (in2,*) b10,'i.e., IOPT(5) = 1, then the following line, in 
     &its exact form with'
      write (in2,*) b10,'the proper delimiters used, must be present, an
     &d immediately'
      write (in2,*) b10,'followed (i.e., no blank lines) by the sequenti
     &al overland met data.'
      write (in2,*)
      write (in2,*) dls,'INPUT GROUP 17',dle
c
      if (iopt(5).eq.1) then 
c
500     continue
c        read (in,*,end=501) jyr,day1,khr,ikst,qu,qtemp,qtheta,qhl
c        write (in2,141) jyr,day1,khr,ikst,qu,qtemp,qtheta,qhl
        read (in,'(a160)',end=501) buffer
        call clen(lenbuf,160,buffer)
        write (in2,*) buffer(1:lenbuf)
        go to 500
501     continue
c
      else
c
        write (in2,*) '85 265  1  3   2.500 292.150 215.000 500.000'
c
      end if
c
c     Check for overwater receptors with elevations above water surface
c     then print warning and reset elevation to zero (water surface)
c
      if(iopt(1).eq.1) then
         do 700 l=1,nrecep
            if(elr(l).ne.0.) then
               delxr=rrec(l)-x0
               delyr=y0-srec(l)
               lx=int(delxr/delx+0.9999)
               ly=int(delyr/dely+0.9999)
               if(lx.ge.1 .and. ly.ge.1) then
                  if(lx.le.nx .or. ly.le.ny) then
                     if(xymap(ly,lx).eq.letw) then
                        write (*,27) l,elr(l)
                        elr(l)=0.
                     endif
                  endif
               endif
            endif
700      continue
      endif
      stop
670   write (*,1397)
      stop
c
c *** Format statements
c
27    FORMAT(//,'   ***** WARNING -- ILLEGAL ELEVATION FOR OVERWATER',
     &         ' RECEPTOR NUMBER ',I3,', ELEVATION = ',F8.0,/,
     &         ' ---> ELEVATION IS RESET TO ZERO (WATER SURFACE)',//)
61    format(a2,'SRCPAR   = ',3(f11.3,', '),2(f7.3,', '),f8.3,', ',
     &        4(f7.3,', '),f7.3,a2)
71    format(a2,'RCPPAR   = ',2(f11.3,', '),2(f7.3,', '),f7.3,a2)
81    format(a2,'DECAY    = ',11(f7.4,', '),f7.4,a2)
111   format ('(',i3,'a1)')
121   format ('(',i3,'a1,'','')')
131   format ('(',i3,'a1,''!'')')
141   format (i2,i4,i3,i2,3f8.2,f9.2)
1000  FORMAT(//,'   ***** FATAL ERROR -- OCD GROUP 15',/
     &        'DELX (KM) = ',F9.3,' IS INAPPROPRIATE FOR AVGDIST (KM)',
     &        ' = ',F9.3,//)
1010  FORMAT(//,'   ***** FATAL ERROR -- OCD GROUP 15',/
     &        'DELY (KM) = ',F9.3,' IS INAPPROPRIATE FOR AVGDIST (KM)',
     &        ' = ',F9.3,//)
1180  FORMAT (A80/A80/A80)
1200  FORMAT (A4)
1215  FORMAT (A12)
1240  FORMAT (A8,5F10.3)
1250  FORMAT (1X,' NSIGP (THE NO. OF SIGNF POINT SOURCES) WAS FOUND',' T
     &O EXCEED THE LIMIT (25).  USER TRIED TO INPUT ',I3,' SOURCES'/'
     & *********EXECUTION TERMINATED**********')
1255  FORMAT( / ,' NAVG (THE LENGTH OF AN AVERAGING PERIOD) WAS INPUT ',
     & 'AS ',I4,' HOURS; IS NOT ALLOWED TO EXCEED 24 HOURS.')
1275  FORMAT (' USER TRIED TO INPUT MORE THAN ',I5,' POINT SOURCES. THIS
     & GOES BEYOND THE CURRENT PROGRAM DIMENSIONS.')
1280  FORMAT (1X,'NPT =  ',I3,'I.E., EQUAL OR LESS THAN ZERO'/' RUN TERM
     &INATED----CHECK INPUT DATA')
1290  FORMAT ( 1x,'***ERROR---USER TRIED TO SPECIFY ',I4,' SIGNIFICANT S
     &OURCES, BUT SHOULD SPECIFY ',I4,' SIGNIFICANT SOURCES IN THIS RUN.
     &',/2X,'***RUN TERMINATED-CHECK INPUT DATA***')
1300  FORMAT (' (MPS) THE INPUT SIGNIFICANT SOURCE NUMBER ','WAS FOUND T
     &O EQUAL ZERO - USER CHECK INPUT DATA.')
1330  FORMAT ( / ,' WRONG RECEPTOR ELEVATION CARD READ.',' READ CARD ',
     &  'FOR AZIMUTH ',I3,' SHOULD HAVE BEEN ',I3,'.')
1340  FORMAT (1X,'****USER EITHER TRIED TO INPUT MORE THAN ',i5,' RECEPT
     1ORS OR ENDR WAS NOT PLACED AFTER THE LAST RECEPTOR ','CARD****'/
     2'********EXECUTION TERMINATED*******')
1350  FORMAT (1X,'NO RECEPTORS HAVE BEEN CHOSEN')
1395  FORMAT( / ,30X,'FATAL ERROR--NX (',I3,') AND/OR NY (',I3,
     & ') ARE LARGER THAN ',i3,' FOR LAND-WATER MAP')
1397  FORMAT( / ,10X,'DELIMITER CARD "ENDS" NOT FOUND OR FOUND ',
     &  'PREMATURELY AT END OF SHORELINE GEOMETRY SECTION')
1605  FORMAT (1X,'NPT =  ',I3,'I.E., NUMBER OF AREA CIRCLES GREATER
     &  THAN FIVE'/' RUN TERMINATED----CHECK INPUT DATA')
1830  FORMAT( / ,T31,'INPUT LATITUDE VALUE OUT OF BOUNDS: ',F8.3)
1840  FORMAT( / ,T31,'INPUT LONGITUDE VALUE OUT OF BOUNDS: ',F8.3)
1850  FORMAT( / ,T31,'INPUT TIME ZONE VALUE OUT OF BOUNDS: ',F5.1)
c
      end
c-----------------------------------------------------------------------
      subroutine clen(len,lena,a)
c-----------------------------------------------------------------------
c
c CLEN          Version: 1.0            Level: 910530
c
c Author:
c -------
c   Joseph C. Chang
c   EARTH TECH
c   196 Baker Avenue
c   Concord, MA 01742
c   Tel (508) 371-4256       Fax (508) 371-4280
c   E-mail chang@src.com
c
c PURPOSE:      Determine the lenght of a character string "a" after
c               ignoring all the trailing blanks.
c
c ARGUMENTS:
c  len     integer   length of the character "a" neglecting all the
c                    trailing blanks
c  lena    integer   original length of the character "a" declared in
c                    the calling program
c  a       character the character string whose length is to be determined
c
c CALLING ROUTINES:
c
c EXTERNAL ROUTINES:    none
c
c NOTE:
c  Leading blanks will be left intact.
c-----------------------------------------------------------------------
        character *(*) a
        do 10 i=lena,1,-1
        if (a(i:i).ne.' ') goto 11
10      continue
        print *,'WARNING IN CLEN:, "a" is all blanks!'
        len=0
        return
11      len=i
        return
        end
c
c-----------------------------------------------------------------------
c subroutine: fopen
c
c purpose:  A file-open utility program
c
c Author:
c -------
c   Joseph C. Chang
c   EARTH TECH
c   196 Baker Avenue
c   Concord, MA 01742
c   Tel (508) 371-4256       Fax (508) 371-4280
c   E-mail chang@src.com
c
c   Developed for:
c   Minerals Management Service
c   U.S. Department of the Interior
c
c arguments passed:
c    variable   type    description
c      iunit    int     logical unit number of the file to be opened
c      cname    char    name of the file to be opened
c      cstatus  char    status of the file to be opened,
c                       'new', 'old', or 'unknown'
c      cform    char    form of the file to be opened,
c                       'formatted' or 'unformatted'
c
c calling routines: analysis
c
c-----------------------------------------------------------------------
c
      subroutine fopen (iunit,cname,cstatus,cform)
      character*(*) cname,cstatus,cform
      integer       iunit
      logical       lflag
      character*1   ians
c
c Check the validity of cstatus and cform
c
        if (cstatus.ne.'NEW' .and. cstatus.ne.'OLD' .and.
     1      cstatus.ne.'new' .and. cstatus.ne.'old' .and.
     2      cstatus.ne.'unknown' .and. cstatus.ne.'UNKNOWN') then
          print *,'Error in fopen: cstatus = ',cstatus,' not recognized'
          stop
        end if
c
        if (cform.ne.'FORMATTED' .and. cform.ne.'UNFORMATTED' .and.
     1      cform.ne.'formatted' .and. cform.ne.'unformatted') then
          print *,'Error in fopen: cform = ',cform,' not recognized'
          stop
        end if
c
        if (cstatus.eq.'NEW' .or. cstatus.eq.'new') then
c
c  Check whether the file already exists or not
          inquire (file=cname,exist=lflag)
          if (.not.lflag) then
            open (unit=iunit,file=cname,status=cstatus,form=cform)
          else
            print *,'File ',cname,' already exists, overwrite it ? (y/n,
     1 default=y)  '
            read 11,ians
            if (ians.eq.'N' .or. ians.eq.'n') stop
            open (unit=iunit,file=cname,status='unknown',form=cform)
          end if
c
        else if (cstatus.eq.'OLD' .or. cstatus.eq.'old') then
c
c  Check whether the file already exists or not
          inquire (file=cname,exist=lflag)
          if (lflag) then
            open (unit=iunit,file=cname,status=cstatus,form=cform)
          else
            print *,'File ',cname,' does not exist!'
            stop
          end if
c
        else if (cstatus.eq.'UNKNOWN' .or. cstatus.eq.'unknown') then
c
          open (unit=iunit,file=cname,status=cstatus,form=cform)
c
        end if
c
11      format(a1)
        return
        end
