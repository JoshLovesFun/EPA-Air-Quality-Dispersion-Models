      program mmifstat
               
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c This program is partially based on the METSTAT program developed
c by ENVIRON Corporation.  
c
c Copyright (C) 2010  Alpine Geophysics, LLC
c Copyright (C) 2009  ENVIRON
c               
c This program is free software; you can redistribute it and/or
c modify it under the terms of the GNU General Public License
c as published by the Free Software Foundation; either version 2
c of the License, or (at your option) any later version.
c         
c This program is distributed in the hope that it will be useful,
c but WITHOUT ANY WARRANTY; without even the implied warranty of 
c MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
c GNU General Public License for more details. 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc 
 
c-----MMIFSTAT calculates performance statistics by comparing model predictions
c     with observations.  Variables include wind speed, wind direction, 
c     temperature, and humidity.  Several statistical measures are determined 
c     and output to allow further plotting and reporting.
c
c     Inputs:
c        -CALMET  output file(s)
c        -Surface observation file (text file)
c        -User-defined parameters
c     Outputs:
c        -Hourly mean predictions, observations, and statistics
c        -Daily summary statistics
c        -Daily station statistics
c
      USE met_fields
      USE stat_fields
      USE site_fields

      implicit none

      include 'common.inc'

      character*256 infile,outfile
      character*60 note
      character*60 inrec
      character*10 begdate,enddate
      character*5  obsfmt
      character*3 inptype
      logical lmeteof,lfirst,lcnteof,datafull
      logical outvector
      integer iunit,ifile,iommif,ncid,itime,ibeghr,iendday,iendhr
      integer ibegdy,ienddy,ibegmo,iendmo
      integer n,j,i,nmet,nf_status,ndims,nvars,ngatts,unlimdimid,dimid
      integer iargc,istat
      integer nobsfile

      real    progver
      integer proglev
      data    progver,proglev/1.1,20100226/

      mxhr = 24
      llcp = .false.
      lpol = .false.
      lmeteof = .true.

c-----Read user inputs

      write(*,'("Program Version ",f5.2)')progver
      write(*,'("Program Level   ",i8)')  proglev
      if (iargc() .eq. 0) then
        iunit = 2
        open(iunit,file='MMIFstat.inp')
      else
        iunit = 2
        call getarg(1,infile)
        if (infile(1:2) .eq. "-h" .or. infile(1:6) .eq. "--help")
     &      goto 999
        open(iunit,file=infile)
      endif

      iommif = 20

      read(iunit,'(20x,a)',err=999) note
      write(*,*) note

      read(iunit,'(20x,a)') infile
      open(11,file = infile)
      write(*,*) 'Opened output hourly stats file:',infile

      read(iunit,'(20x,a)') infile
      open(12,file = infile)
      write(*,*) 'Opened output daily summary stats file:',infile

      read(iunit,'(20x,a)') infile
      open(13,file = infile)
      write(*,*) 'Opened output daily station stats file:',infile

      read(iunit,'(20x,a)') infile
      if ( (infile(1:4) .ne. 'None') .and.
     &     (infile(1:4) .ne. 'NONE') .and.
     &     (infile(1:4) .ne. 'none') ) then
        outvector = .true.
        open(79,file = infile)
        write(*,*) 'Opened output obs/model vector file:',infile
      else
        outvector = .false.
        write(*,*) 'Not outputting obs/model vector file'
      end if
      write(*,*)

      read(iunit,'(20x,a)') obsfmt
      if (obsfmt .eq. 'RALPH') then
        read(iunit,'(20x,a)') infile
        open(10,file = infile,status='old')
        write(*,*) 'Opened ASCII observation file:',infile
      else if (obsfmt .eq. 'DS472') then
        read(iunit,'(20x,a)') infile
        open(21,file = infile,status='old')
        write(*,*) 'Opened DS472 station file:',infile
        read(iunit,'(20x,i5)')nobsfile
        write(*,*) 'Number of DS472 files to process:',nobsfile
        do i = 1, nobsfile
          read(iunit,'(20x,a)') infile
          open(21+i,file = infile,status='old')
          write(*,*) 'Opened DS472 observation file:',infile
        end do
      else
        write(*,*)'Unable to recognize observation format ',obsfmt
        write(*,*)'  Program supports RALPH and DS472 '
        stop
      end if
 
c     read(iunit,'(20x,a10)') begdate
c     write(*,*) 'Starting date/time YYYYMMDDHH:',begdate
c     read(iunit,'(20x,a10)') enddate
c     write(*,*) '  Ending date/time YYYYMMDDHH:',enddate

c     read(iunit,'(20x,a10)') begdate
c     write(*,*) 'Starting date/time YYYYMMDDHH:',begdate
c     read(iunit,'(20x,a10)') enddate
c     write(*,*) '  Ending date/time YYYYMMDDHH:',enddate

c     read(begdate(1:4),'(i4)')  iyear
c     read(begdate(5:8),'(i4)')  ibegday
c     read(begdate(9:10),'(i2)') ibeghr
c     read(enddate(5:8),'(i4)')  iendday
c     read(enddate(9:10),'(i2)') iendhr

      read(iunit,'(20x,a)')infile
      read(infile,*)iyear,ibegmo,ibegdy,ibeghr
      ibegday = 100*ibegmo+ibegdy

      read(iunit,'(20x,a)')infile
      read(infile,*)iyear,iendmo,ienddy,iendhr
      iendday = 100*iendmo+ienddy

      if (ibegday .gt. iendday) then
        write(*,*)'Ending day before beginning day '
        stop 333
      end if

      if ( (ibeghr .lt. 0) .or. (ibeghr .gt. 23) ) then
        write(*,*)'Starting hour must be between 0 and 23'
        stop 334 
      end if 

      if ( (iendhr .lt. 0) .or. (iendhr .gt. 23) ) then
        write(*,*)'Ending hour must be between 0 and 23'
        stop 335 
      end if 

      if ( (ibegday .eq. iendday) .and. (iendhr .lt. ibeghr) ) then
        write(*,*)'Ending hour less than starting hour '
        stop 336
      end if

      call juldate(iyear,ibegday)
      call juldate(iyear,iendday)
      ibegdat = iyear*100000 + ibegday*100 + ibeghr
      ienddat = iyear*100000 + iendday*100 + iendhr

      write(*,*) 'Starting date/time JJJHH:',ibegdat
      write(*,*) '  Ending date/time JJJHH:',ienddat

      nday = iendday - ibegday + 1
      nhr = 24*nday
      if (ibeghr.gt.0) nhr = nhr - ibeghr
      if (iendhr.lt.23) nhr = nhr - (23 - iendhr)
      write(*,*) 'Processing duration:'
      write(*,*) 'Days:',nday
      write(*,*) 'Hours: ',nhr

      read(iunit,'(20x,i10)') itzon
      write(*,*) 'Time zone (0=UTC,-5=EST,-6=CST,-7=MST,-8=PST):',itzon

      llcp = .true.

      read(iunit,'(20x,i10)') ninsite
      if (ninsite.gt.0) then
        allocate( insite(ninsite) )
        do n = 1,ninsite
          read(iunit,'(20x,a)') insite(n)
          write(*,*) 'Site:',n,' ',insite(n)
        enddo
      elseif (ninsite.lt.0) then
        read(iunit,'(20x,a)') infile
        read(infile,*) xbeg,xend
        write(*,*) 'X coordinate range:',xbeg,xend
        read(iunit,'(20x,a)') infile
        read(infile,*) ybeg,yend
        write(*,*) 'Y coordinate range:',ybeg,yend
      else
        write(*,*)'Processing entire domain '
      endif

      lfirst  = .true.
      lcnteof = .false.
      ifile = 0
      do while (.not. lcnteof)
        read (iunit,'(a)',iostat=istat) infile
        if (istat .ne. 0) then
          lcnteof = .true.
        else
          open(iommif,file=infile,status='old',form='unformatted')
          write(*,*) 'Opened CALMET file:',iommif,infile
c-----Get grid info from the met files and extract all OBS data for
c     given spatial and time range
          if (lfirst) then
            lfirst = .false.
            call grid_calmet(iommif)
            if (ninsite .eq. 0) then
              ninsite = -1
              xbeg = x0mm5
              xend = xfmm5
              ybeg = y0mm5
              yend = yfmm5
              write(*,*) 'X coordinate range:',xbeg,xend
              write(*,*) 'Y coordinate range:',ybeg,yend
            end if


            if (obsfmt .eq. 'RALPH') then
              call readasc
            else if (obsfmt .eq. 'DS472') then
              call readstatlib(21)
              do i = 1, nobsfile
                call readtdl(21+i)
              end do
            else
              write(*,*)'Unable to recognize observation format ',obsfmt
              write(*,*)'  Program supports RALPH and DS472 '
              stop
            endif

            if (nsite .eq. 0) then
              write(*,*)'Error no valid stations on list or in region '
              write(*,*)'Check subregion definition or station list'
              stop 445
            end if

            call alloc_met(nx,ny,nz)
          endif
          ifile = ifile + 1
c-----Read and extract all met model data at extracted OBS sites
          lmeteof = .false.
          call getcalmet(iommif,lmeteof)
          close(iommif)
        end if
      end do
c-----Make sure that the entire period requested has data available
      datafull = .false. 
      do j = 1,nday
        do i = 1,mxhr
          if (ibegdat .eq. idatime(i,j)) then  
            datafull = .true.
          end if
        end do
      end do
      if (.not. datafull) then 
        write(*,*)'CALMET input files does not cover requested ',
     &            'time period '
        write(*,*)'Requested begin time (Local,YYYYJJJHH) ',ibegdat
        write(*,*)'Time in CALMET files (Local) '
        do j = 1, nday
          do i = 1, mxhr
            write(*,*)j,i,idatime(i,j)
          end do
        end do
        stop 4478
      end if

      datafull = .false. 
      do j = 1,nday
        do i = 1,mxhr
          if (ienddat .eq. idatime(i,j)) then  
            datafull = .true.
          end if
        end do
      end do
      if (.not. datafull) then 
        write(*,*)'CALMET input files does not cover requested ',
     &            'time period '
        write(*,*)'Requested ending time (Local,YYYYJJJHH) ',ienddat
        write(*,*)'Time in CALMET files (Local) '
        do j = 1, nday
          do i = 1, mxhr
            write(*,*)j,i,idatime(i,j)
          end do
        end do
        stop 4479
      end if

c-----Calculate hourly and daily stats
      call stats(outvector,note,progver,proglev)

c-----Output data to files
      call wrtfils(note,progver,proglev)


      call flush(6)
      call dealloc_site
      call dealloc_stat
      call dealloc_met
      if (ninsite.gt.0) then
        deallocate( insite )
      end if

      stop 'MMIFSTAT finished normally'
 999  write(*,*) 'Usage: MMIFstat filename.inp'
      write(*,*) 'If you do not supply an input file as an argument'
      write(*,*) 'MMIFstat will attempt to read a file called '     
      write(*,*) 'MMIFstat.inp '

      stop
      end
