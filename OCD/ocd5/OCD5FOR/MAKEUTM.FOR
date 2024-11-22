      program makeutm
c
c ****************************************************************
c
c  Level: 970128                First created: 970228
c
c  A program that reads in separate files for sources or receptors
c  and converts them into OCD/5 input file format, so that the
c  information can be directly inserted into the OCD control file
c
c      Author:
c         Joseph C. Chang
c         EARTH TECH
c         196 Baker Avenue
c         Concord, MA 01742
c         Phone:  (508) 371-4256
c         Fax:    (508) 371-4280
c         E-mail: chang@src.com  or
c                 jchang@earthtech.com
c
c  Input files:
c  ------------
c
c    MAKEUTM.DAT:
c      The control file that contains only one parameter, ILL
c      ILL=0, receptor or source locations are in user units
c      ILL=1, receptor or source locations are in north latitudes
c             and west longitudes
c
c    MAKEGEO.REF:
c       The file that contains the UTM-coordinates and the UTM zone
c       for the ORIGIN of the modeling domain.  This file is needed
c       only when ILL = 1.
c
c    MAKEUTM.INP:
c       The file that contains the receptor or source information.
c       Refer to Tables 3-2 and 3-5 of the OCD/4 User's Guide for
c       the source and receptor file formats, recpectively.
c       The exceptions are (1) there is one line for each record
c       (i.e., source or receptor), (2) the file is read with
c       free-format, (3) the receptor or source locations can optionally
c       be expressed in latitudes (positive for north) and longitudes
c       (positive for west), and (4) there is no need to include the
c       termination card such as "ENDP" or "ENDR".
c
c  Output files:
c  -------------
c
c     MAKEUTM.OUT:
c        The file, containing the source and receptor information,
c        that can be directly included into an OCD/5 control file
c
c ****************************************************************
c
      include 'params.cmn'
      real source(13,maxp),receptor(5,maxp),work(13)
      character*12 pname(maxp),dummy
      character*8  rname(maxrec)
      character*240 line
      character*2 dls,dle
      character*3 ender
      character*10 b10
      data ender/'END'/
      data dls/'! '/
      data dle/' !'/
      data b10/'          '/
c
      print *,'MAKEUTM'
      print *,'A Processor for Offshore and Coastal Dispersion Model'
      print *
      print *,'MAKEUTM is used to convert the receptor and source inform
     &ation stored in'
      print *,'separate ASCII files in generic format into a form that c
     &an be directly'
      print *,'inserted into an OCD/5 control file.  The user is referre
     &d to Tables 3-2 and'
      print *,'3-5 in the OCD/4 User''s Guide for the ASCII input file f
     &ormats for the source'
      print *,'and receptor information, recpectively, with the followin
     &g exceptions:'
      print *,'(1) There is one line for each record (i.e., source or re
     &ceptor).'
      print *,'(2) The file is free-formatted.'
      print *,'(3) The receptor or source locations can optionally be ex
     &pressed in latitudes'
      print *,'    (positive for north) and longitudes (positive for wes
     &t).'
      print *,'(4) The termination cards such as "ENDP" and "ENDR" are n
     &ot allowed.'
      print *,'If the receptor or source locations are expressed in lati
     &tudes/longitudes'
      print *,'in the ASCII input file, then it is assumed that the user
     & had just run the'
      print *,'MAKEGEO preprocessor, whereby the modeling demain was spe
     &cified.'
      print *,'If the receptor or source locations are expressed in user
     & units in the ASCII'
      print *,'input file, then it is assumed that the user units are co
     &nsistent with those'
      print *,'already used in the control file.'
      print *
c
c      open (unit=1,file='makeutm.dat',status='old')
      open (unit=2,file='makeutm.inp',status='old')
      in2=4
      open (unit=in2,file='makeutm.out',status='unknown')
c
c      read (1,*) ill
      ill = 1   ! The user must use lat/long for coordinates.
      if (ill.ne.0 .and. ill.ne.1) then
        print *,'The ILL indicator must be either 0 or 1.'
        print *,'ILL = 0: locations are in UTM-coordinates.'
        print *,'ILL = 1: locations are in north latitudes and west long
     &itudes.'
        stop
      end if
c
      if (ill.eq.1) then
        open (unit=3,file='makegeo.ref',status='old')
      end if
c
c *** Find out what is included in the file MAKEUTM.DAT by reading
c     the first line.
c
c     If 14 parameters per line: line source;           ITYPE = 1
c        12 parameters per line: point or area sources; ITYPE = 2
c         6 parameters per line: discrete receptors;    ITYPE = 3
c
      read (2,11) line
11    format(a240)
c
      read (line,*,err=51,end=51) dummy,(work(i),i=1,13)
      itype=1            ! 14 parameters per line; line source
      imany=13
      goto 60
c
51    continue
      read (line,*,err=52,end=52) dummy,(work(i),i=1,11)
      itype=2            ! 12 parameters per line; point/area sources
      imany=11
      goto 60
c
52    continue
      read (line,*,err=53,end=53) dummy,(work(i),i=1,5)
      itype=3            ! 6  parameters per line; discrete receptors
      imany=5
      goto 60
c
c *** Something wrong with the data
c
53    continue
      print *,'Verify that the format for the file MAKEUTM.DAT is correc
     &t.'
      print *,'There should be 14 parameters per line for line source,'
      print *,'12 parameters per line for point or area sources, and'
      print *,'6 parameters per line for discrete receptors.'
      print *,'Note that the file will be read with free format.'
      print *,'Premature MAKEUTM stop'
      stop
c
60    continue
      rewind (2)
c
      if (ill.eq.0) then
      print *,'You have specified that user units are used for source/re
     &ceptor locations'
      print *,'in the input file.'
      else if (ill.eq.1) then
      print *,'You have specified that latitudes/longitudes are used for
     & source/receptor'
      print *,'locations in the input file.'
      end if
c
      if (itype.eq.1) then
      print *,'You have specified line source information in the input f
     &ile.'
      else if (itype.eq.2) then
      print *,'You have specified point/area source information in the i
     &nput file.'
      else if (itype.eq.3) then
      print *,'You have specified discrete receptor information in the i
     &nput file.'
      end if
c
c *** Read data from MAKEUTM.DAT
c
      if (itype.eq.1) then  ! Line source
c
        npt=0
        do i=1,1            ! max. one line source allowed
          read (2,11,end=200) line
          npt=npt+1
          read (line,*) pname(npt),(source(j,npt),j=1,imany)
        end do
200     continue
c
      else if (itype.eq.2) then  ! point or area sources
c
        npt=0
        do i=1,maxp         ! max. MAXP point sources allowed
                            ! the code currently cannot distinguish
                            ! between point and area sources
          read (2,11,end=210) line
          npt=npt+1
          read (line,*) pname(npt),(source(j,npt),j=1,imany)
        end do
210     continue
c
      else if (itype.eq.3) then  ! discrete receptors
c
        npt=0
        do i=1,maxrec       ! max. MAXREC receptors allowed
          read (2,11,end=220) line
          npt=npt+1
          read (line,*) rname(npt),(receptor(j,npt),j=1,imany)
        end do
220     continue
c
      end if
c
      nrecep = npt
c
c *** Convert lat/long to UTM if necessary.
c
      if (ill.eq.1) then
        read (3,*) xsw,ysw,iz0
        if (itype.eq.1 .or. itype.eq.2) then
          do i=1,npt
            call ll2utm(source(1,i),-source(2,i),iz0,utme,utmn,iz)
            ! Note that subroutine LL2UTM wants negative values
            ! for western longitudes.
            source(1,i)=utme-xsw
            source(2,i)=utmn-ysw
          end do
c
          if (itype.eq.1) then  ! line source      
            call ll2utm(source(12,1),-source(13,1),iz0,utme,utmn,iz)
            ! Note that subroutine LL2UTM wants negative values
            ! for western longitudes.
            source(12,1)=utme-xsw
            source(13,1)=utmn-ysw
          end if
        else
          do i=1,nrecep
            call ll2utm(receptor(1,i),-receptor(2,i),iz0,utme,utmn,iz)
            ! Note that subroutine LL2UTM wants negative values
            ! for western longitudes.
            receptor(1,i)=utme-xsw
            receptor(2,i)=utmn-ysw
          end do
        end if
      end if
c
c *** Write information to the output file MAKEUTM.OUT, that can be
c     directly included in the OCD/5 control file.
c
      if (itype.eq.1 .or. itype.eq.2) then
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
      write (in2,*) b10,'are repated NPT times, one set for each source.
     &'
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
     &orm or water'
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
      write (in2,*) b10,'            0 means the stack is veritcal, <90 
     &means the stack'
      write (in2,*) b10,'            has an upward pointing component, >
     &90 means the stack'
      write (in2,*) b10,'            has a downward pointing component.
     &'
      write (in2,*) b10,'       (10) Elevation of ground, water, or plat
     &form base at stack'
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
      write (in2,61) dls,(source(j,i),j=1,11),dle
61    format(a2,'SRCPAR   = ',3(f11.3,', '),2(f7.3,', '),f8.3,', ',
     &        4(f7.3,', '),f7.3,a2)
      write (in2,*) dls,ender,dle
      write (in2,*)
      end do
c
      end if
c
      if (itype.eq.1) then
c
      write (in2,*) 'INPUT GROUP 6c -- Source description, line source p
     &arameters (conditinal)'
      write (in2,*) '(Only necessary if IOPT(20)=2)'
      write (in2,*) 'Modeling of line sources is limited to screening ty
     &pe analyses of 24 hours'
      write (in2,*) 'or less.'
      write (in2,*) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     &%%%%%%%%%%%%%%%%%%%%%%%%'
c
      xstop=source(12,1)
      ystop=source(13,1)
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
      end if
c
      if (itype.eq.3) then
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
      write (in2,71) dls,(receptor(j,i),j=1,5),dle
71    format(a2,'RCPPAR   = ',2(f11.3,', '),2(f7.3,', '),f7.3,a2)
      write (in2,*) dls,ender,dle
      write (in2,*)
      end do
c
      end if
c
      end
c        
c----------------------------------------------------------------------
      subroutine ll2utm(rlat,rlon,iz0,x,y,iz)
c----------------------------------------------------------------------
c
c --- TERREL   Version: 2.0       Level: 961004                  LL2UTM
c
c --- PURPOSE:  Converts latitude/longitude to UTM coordinates
c           *** Universal Transverse Mercator (UTM) grid system divides
c           *** the world into 60 north-south zones, covering a 6 degree
c           *** strip of longitude. Zone 1 begins between 180 and 174 degree
c           *** west longitude and progresses eastward to zone 60.
c               Works in both Northern & Southern Hemispheres
c               Reference --
c                 "Map Projections--A Working Manual", p61,
c                  U.S. Geological Survey Professional Paper 1395,
c                    Note: assumes the Clarke 1866 ellipsoid
c               Adapted from --
c                  EPS version 2.0; subroutine mapgtu
c
c --- INPUTS:
c               RLAT - Real        - N Latitude in decimal degrees
c                                    (use negative for southern hemisphere)
c               RLON - Real        - E Longitude in decimal degrees
c                                    (use negative for western hemisphere)
c                IZ0 - Integer     - UTM zone override
c
c --- OUTPUT:
c                  X - Real        - UTM easting in km
c                  Y - Real        - UTM northing in km
c                 IZ - Integer     - UTM zone
c
c --- LL2UTM called by:  RDUSGSHD, CORNERS, LOAD
c --- LL2UTM calls:      none
c----------------------------------------------------------------------

      real k0
      real N,M

      parameter (k0=0.9996)
      parameter (a=6378206.4)
      parameter (e2=0.00676866)
      parameter (ep2=0.0068148)
      parameter (false_e=500000.0)
      parameter (dtr=3.141592654/180.0)

      if (iz0 .eq. 0) then
c ---   Locate natural zone
          iz = int((180.0+rlon)/6.0) + 1
      else
c ---   Zone override
          iz = iz0
      endif

c --- Compute delta longitude in radians
      dl = dtr*(rlon - (6.0*iz-183.0))

c --- Convert phi (latitude) to radians
      p = dtr*rlat

      sinp = sin(p)
      N = a/sqrt(1.0-e2*sinp*sinp)
      tanp = tan(p)
      T = tanp*tanp
      cosp = cos(p)
      C = ep2*cosp*cosp
      A1 = dl*cosp
      M = 111132.0894*rlat - 16216.94*sin(2.0*p) + 17.21*sin(4.0*p)
     &  - 0.02*sin(6.0*p)

      A2 = A1**2
      A3 = A2*A1
      A4 = A2**2
      A5 = A4*A1
      A6 = A4*A2
      T2 = T**2

c --- Compute UTM x and y (km)
      x = 0.001*(k0*N*(A1+(1.0-T+C)*A3/6.0
     &  + (5.0-18.0*T+T2+72.0*C-58.0*ep2)*A5/120.0)
     &  + false_e)
      y = (M+N*tanp * (A2/2.0 + (5.0-T+9.0*C+4.0*C*C)*A4/24.0
     &  + (61.0-58.0*T+T2+600.0*C-330.0*ep2)*A6/720.0))
      false_n = 0.
      if (rlat .lt. 0.) then
c --- in km, unlike false_e
        false_n = 10000.
      endif
      y = 0.001*k0*y + false_n

      return
      end
