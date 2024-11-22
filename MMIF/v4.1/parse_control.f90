MODULE parse_control
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     This module contains subroutines to open, read, and process control files 
!     for various versions of MMIF.  Each routine has is own Development History.
!     This module contains:
!
! subroutine parse_control_file
! subroutine get_word(line,which_word,word)
! subroutine count_words(line,num_words)
! subroutine command_line(string,force_run,debug)
! subroutine usage
! subroutine sample_input
!
!-----Declarations for global (within this scope) variables
!
  implicit none
  character (len=256), allocatable, dimension(:) :: fnameU,fname1,fname2! <  v3.0
  character (len=256)               :: ctrl_fname ! Control file filename
  character (len=1),   dimension(3) :: comment   = (/ "#", ";", "!" /)
  character (len=1),   dimension(5) :: delim     = (/ " ", ",", "#", ";", "!" /)
  character (len=1),   dimension(3) :: datedelim = (/ "-", "_", ":" /)

  integer :: iBegDat,iEndDat              ! beginning and ending time-stamps
  integer :: LastOut                      ! timestamp of ending in ANY timezone
  integer :: NumMETfiles                  ! Number of MM5/WRF (MET) files to read
  integer :: iMetFile                     ! MM5/WRF (MET) index for file number
  integer :: iOut                         ! index for NumOuts
  integer, parameter :: iCtrlUnit = 5     ! FORTRAN unit for control file
  integer, allocatable, dimension(:) :: iOutUnit,iOutUnit2,iUseful ! Output units
  integer, allocatable, dimension(:) :: iUppFreq ! How often AERMET UPA written
  integer, allocatable, dimension(:) :: kzin,kz1,kz2   ! for aggregation
  
  real,    allocatable, dimension(:) :: calmet_version ! 5.8, or anything for 6.x


CONTAINS
!
!******************************************************************************
!
  subroutine parse_control_file(force_run)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     This subroutine parses a MMIFv3.x style control file.
!
!     Development History:
!     2013-09-05  Original Development (ENVIRON International Corp.)
!     2014-07-30  Better support for PtZone(iOut) at ending of run.
!     2014-09-18  Add optional minimum mixing height and abs(L) in AERMOD modes.
!     2014-09-22  Improved error reporting when parsing control file.
!     2014-10-14  Added BEGIN/END as synonyms for START/STOP.
!     2015-07-24  Uppercase just the filename, not path, for ONSITE output.
!     2016-03-16  Bug fix: ZMID not the middle when using LAYERS MID keywords
!     2016-03-17  PBL_recalc changed to MIXHT, with possible values WRF or MMIF
!     2016-03-17  Added AERMET_MIXHT option, possible values WRF, MMIF or AERMET
!     2016-07-06  Detect Xlcc > max possible - probably means Xlcc givin in meters
!     2016-08-05  Changed MIXHT to CALSCI_MIXHT to avoid confusion with AER_MIXHT
!     2016-12-08  Added range checks to ORIGIN keyword values.
!     2017-06-20  New output: BLN, BNA, DAT or KML for QA.
!     2018-12-27  New keyword: CLOUDCOVER controlling the source for Cloud vals
!     2020-10-17  New keyword: OVER, and optional keyword in POINT line (deferred)
!     2020-10-18  New Keyword: AER_USE_TSKC (not BULKRN method, ALPHA option)
!     2021-09-10  New Keyword: AER_USE_NEW (AERMET 21112 and later versions)

    USE met_fields
    USE functions
    integer               :: iOut, ios, i,k       ! used for flow control
    integer               :: num_words            ! number of "words" in line
    integer, dimension(2) :: nzPt_tmp             ! apply to next nzPt()
    integer               :: iUppFreq_tmp         ! apply to next PFL file
    integer               :: iyr,imo,idy,ihr      ! temporary time-stamp
    character (len=999)   :: line                 ! used to read the control file
    character (len=999)   :: line_aer_loc         ! line containing location
    character (len=100)   :: word,word2,word3     ! one word of the above line
    logical               :: force_run            ! Don't stop at optional errors

999 format(a999)
!
!-----Entry point
!
    open(unit=iCtrlUnit,file=ctrl_fname,status='old',err=80)
    write(*,*)
    write(*,*)'========== MMIF VERSION 4.1 2023-10-30 =========='
    write(*,*)
    write(*,*) '   Reading control file: ',trim(ctrl_fname)
!
!-----Count the number of Input MM5/WRF files, Output files, and Layers
!
    NumMETfiles =  0 ! initialize
    NumOuts     =  0 ! initialize
    nzOut       = -1 ! flag for "use default", if not included in control file
    ibtz        = 0  ! default to GMT

    read(iCtrlUnit,999,iostat=ios) line ! 1st line
    call count_words(line,num_words)
    ios = 0
    do while (num_words == 0 .and. ios == 0)
       read(iCtrlUnit,999,iostat=ios) line
       call count_words(line,num_words)
    end do
    if (ios /= 0) then ! found the end of the file while looking for 1st line
       write(*,*) 
       write(*,*) "*** Error parsing control file, only comments found."
       write(*,*) 
       stop
    end if
    do while (ios == 0)
       call get_word(line,1,word)
       call uppercase(word)
       if (word == "INPUT")  NumMETfiles = NumMETfiles + 1
       if (word == "OUTPUT") NumOuts     = NumOuts     + 1
       if (word == "LAYERS") then
          call count_words(line,nzOut)
          nzOut = nzOut - 2 ! account for "LAYERS TOP" or "LAYERS K" or ...
          if (nzOut <= 0) then
             write(*,*)
             write(*,*) '*** Error: Found zero values following keyword LAYERS.'
             write(*,*) '    Problematic line:'
             write(*,*) trim(line)
             write(*,*) '    Program stopping.'
             stop
          end if
       end if
       if (word == "TIMEZONE") then   ! need this to we can set PtZone below
          call get_word(line,2,word2) 
          read(word2,*,err=83) ibtz
       end if
       read(iCtrlUnit,999,iostat=ios) line  ! read to next non-comment,
       call count_words(line,num_words)     ! non-blank line
       do while (num_words == 0 .and. ios == 0)
          read(iCtrlUnit,999,iostat=ios) line
          call count_words(line,num_words)
       end do
    end do
    rewind(iCtrlUnit)       ! back to the beginning of the file
!
!-----Set the defaults
!
    ibyr = -9999 ! invalid default, so we can detect missing START keyword
    ieyr = -9999 ! invalid default, so we can detect missing STOP  keyword

    ijlatlon = 1 ! specify by IJ
    iBeg = -5 ; jBeg = -5 ; iEnd = -5 ; jEnd = -5 ! whole grid

    if (nzOut == -1) then ! Flag for "use defaults"
       iVertMap = 1 ! verical mapping using interpolation given TOPs of layers
       nzOut = 10
       allocate( zface(0:nzOut))  ! Output levels
       allocate( zmid(0:nzOut) )  ! Output layer mid-points
       allocate( zfaceS(0:nzOut)) ! Output levels for interpolation (SCICHEM)
       zface = (/0.,  20.0, 40.0, 80.0, 160.0, 320.0, 640.0, 1200.0, 2000.0, &
            3000.0, 4000.0/) ! *** FIXME: change to EPA MMIF Guidance
       zmid(0)  = 0.              ! the ground
       do k = 1,nzOut
          zmid(k) = (zface(k) + zface(k-1)) / 2.
       end do
    else ! user-specified, allocate now (so it's near the above alloc.)
       allocate( zface(0:nzOut))  ! Output levels
       allocate( zmid(0:nzOut) )  ! Output layer mid-points
       allocate( zfaceS(0:nzOut)) ! Output levels for interpolation (SCICHEM)
    end if
!
!-----Allocate for the number of Input MM5/WRF files and Output files
!
    allocate(METfile(NumMETfiles),  &
         OutForm(NumOuts),          &
         OutType(NumOuts),          &
         OutFile(0:NumOuts),        &
         iOutUnit(NumOuts),         &
         calmet_version(NumOuts),   &
         iUppFreq(NumOuts),         &
         PtZone(NumOuts),           &
         PtOver(NumOuts),           &
         iPt(NumOuts),              &
         jPt(NumOuts),              &
         ijPt(NumOuts),             &
         nzPt(NumOuts,2),           &
         PtLat(NumOuts),            &
         PtLon(NumOuts),            &
         PtXlcc(NumOuts),           &
         PtYLcc(NumOuts),           &
         aersfc(12,3,2,NumOuts),    &
         related_out(NumOuts,3),    &
         aer_use_BULKRN(NumOuts) )
!
!-----Set default values
!
    aer_use_BULKRN = .true.  ! default: appropriate for over land
    aer_use_TSKC   = .false. ! default: using TSKC is an ALPHA option
    aer_use_NEW    = .false. ! default: use older version of AERMET configuration syntax
    AER_MIXHT  = "AERMET"    ! default: use AERMET's mixing height in AERMET mode
    PGtype     = "GOLDER"    ! default: use GOLDER not SRDT.
    CalcPBL    = "WRF"       ! default: use WRF's mixing height for 3-D outputs
    CloudCover = "WRF"       ! default: use WRF's CLDFRA 3D field, collapsed to 2D
    iOver      = 0           ! default: detect from WRF's land-use category water_cat
    origlat    = -999.       ! "bad" value flag: don't over-ride the values of
    origlon    = -999.       ! LCC origin found in the WRFOUT file.
    ijPt = 0                 ! default code for "this iOut is gridded data, not point data"
    PtLat = -99999.          ! default is "don't use" flag
    PtLon = -99999.          ! default is "don't use" flag
    PtXlcc = -99999.         ! default is "don't use" flag
    PtYlcc = -99999.         ! default is "don't use" flag
    nzPt(:,1) = 1            ! default to include the lowest MM5/WRF level
    nzPt(:,2) = nzOut        ! default to writing all layers given above
    nzPt_tmp(1) = 1          ! default to include the lowest MM5/WRF level
    nzPt_tmp(2) = nzOut      ! default to writing all layers given above
    iUppFreq  = 12           ! default to UPPER AIR (FSL) output every 12 hours
    iUppFreq_tmp = 12        ! default to UPPER AIR (FSL) output every 12 hours
    PtZone = ibtz            ! default to global time zone
    PtOver = iOver           ! default to auto-detect land/water from on landuse
    line_aer_loc = ""        ! default to blank (no location, triggers an error)
    aersfc = 0               ! default to zero
    related_out = 0          ! default to zero
    OutFile(0) = "none"      ! default to a flag for no output
    ipt = 0                  ! initialize to zero
    jpt = 0                  ! initialize to zero

    iMetFile = 0
    iOut     = 0
!
!-----Parse the contol file, one line at a time, until an error reading
!
    read(iCtrlUnit,999,iostat=ios) line ! 1st line
    call count_words(line,num_words)    ! read to first non-comment non-blank
    do while ((any(line(1:1) == comment) .or. num_words == 0) .and. ios == 0)
       read(iCtrlUnit,999,iostat=ios) line
       call count_words(line,num_words)
    end do

    do while (ios == 0)

       call count_words(line,num_words)
       call get_word(line,1,word)
       call uppercase(word)     ! all keywords converted to uppercase

       if (pdebug) print*,"Processing line: ",trim(line)
!
!-----Special processing for dates to turn 2013-09-30_12 into 2013 09 30 12.
!     This allow WRF-style time-stamps (e.g. 2011-07-02_00:00:00).
!
       if (word == "START" .or. word == "BEGIN" .or. &
           word == "STOP"  .or. word == "END" ) then
          i = 1
          k = len_trim(line) ! like this_len
          do while (i < k)
             if (any(line(i:i) == comment)) k = i - 1
             if (any(line(i:i) == datedelim)) line(i:i) = " " !  
             i = i + 1
          end do
          call count_words(line,num_words) ! re-count them
       end if

       select case (trim(word))
       case ("BEGIN","START")

          if (num_words >= 5) then      ! like 'start 2008 07 04 01'
             call get_word(line,2,word2) ; read(word2,*,err=81) ibyr
             call get_word(line,3,word2) ; read(word2,*,err=81) ibmo
             call get_word(line,4,word2) ; read(word2,*,err=81) ibdy
             call get_word(line,5,word2) ; read(word2,*,err=81) ibhr
          else if (num_words == 2) then ! like 'start 2008070401'
             call get_word(line,2,word2)
             read(word2,'(i4,3i2)',err=81) ibyr,ibmo,ibdy,ibhr
          end if
          call legal_timestamp(ibyr,ibmo,ibdy,ibhr,23)
          call ymdh2nDatHr(ibyr,ibmo,ibdy,ibhr,iBegDat)

          if (pdebug) print*,"Start time = ",ibyr,ibmo,ibdy,ibhr

       case ("STOP","END")

          if (num_words >= 5) then      ! like 'stop 2008 07 04 01'
             call get_word(line,2,word2) ; read(word2,*,err=82) ieyr
             call get_word(line,3,word2) ; read(word2,*,err=82) iemo
             call get_word(line,4,word2) ; read(word2,*,err=82) iedy
             call get_word(line,5,word2) ; read(word2,*,err=82) iehr
          else if (num_words == 2) then ! like 'stop 2008070401'
             call get_word(line,2,word2)
             read(word2,'(i4,3i2)',err=82) ieyr,iemo,iedy,iehr
          end if
          call legal_timestamp(ieyr,iemo,iedy,iehr,23)
          call ymdh2nDatHr(ieyr,iemo,iedy,iehr,iEndDat)

          if (pdebug) print*,"Stop  time = ",ieyr,iemo,iedy,iehr

       case ("TIMEZONE")

          call get_word(line,2,word2)
          read(word2,*,err=83) ibtz

       case ("GRID")

          call get_word(line,2,word2)
          call uppercase(word2)

          if (word2 == "IJ") then 
             ijlatlon = 1
             call get_word(line,3,word3) ; read(word3,*,err=84) iBeg
             call get_word(line,4,word3) ; read(word3,*,err=84) jBeg
             call get_word(line,5,word3) ; read(word3,*,err=84) iEnd
             call get_word(line,6,word3) ; read(word3,*,err=84) jEnd
             if (iBeg > iEnd .and. iBeg > 0 .and. iEnd > 0) then ! sanity checks
                write(*,*)
                write(*,*) '*** Error: Beginning I coordinate > ', &
                     'ending I coordinate.'
                write(*,*) '    Program stopping.'
                stop
             endif
             if (jBeg > jEnd .and. jBeg > 0 .and. jEnd > 0) then
                write(*,*)
                write(*,*) '*** Error: Beginning J coordinate > ', &
                     'ending J coordinate.'
                write(*,*) '    Program stopping.'
                stop
             endif
         else if (word2 == "LL" .or. word2 == "LATLON") then
             ijlatlon = 2
             call get_word(line,3,word3) ; read(word3,*,err=84) BegLat
             call get_word(line,4,word3) ; read(word3,*,err=84) BegLon
             call get_word(line,5,word3) ; read(word3,*,err=84) EndLat
             call get_word(line,6,word3) ; read(word3,*,err=84) EndLon
          else if (word2 == "KM" .or. word2 == "PROJ"        & ! some synonyms
               .or. word2 == "LCC" .or. word2 == "PS" .or. word2 == "EM") then
             ijlatlon = 3
             call get_word(line,3,word3) ; read(word3,*,err=84) BegXlcc
             call get_word(line,4,word3) ; read(word3,*,err=84) BegYlcc
             call get_word(line,5,word3) ; read(word3,*,err=84) EndXlcc
             call get_word(line,6,word3) ; read(word3,*,err=84) EndYlcc
             if (BegXlcc > EndXlcc) then ! sanity checks
                write(*,*)
                write(*,*) '*** Error: beginning X LCC coordinate > ', &
                     'ending coordinate.'
                write(*,*) '    Program stopping.'
                stop
             endif
             if (BegYlcc > EndYlcc) then
                write(*,*)
                write(*,*) '*** Error: beginning Y LCC coordinate > ', &
                     'ending coordinate.'
                write(*,*) '    Program stopping.'
                stop
             endif
!
! Circumference of the earth is about 40,000 km, so the max possible (although
! ridiculous) coordinate of an LCC plane would be 20,000 km. 
!
             if ( BegXlcc >= 2.e5 .or. EndYlcc >= 2.e5 .or.  &
                  BegYlcc >= 2.e5 .or. EndYlcc >= 2.e5) then
                write(*,*) 
                write(*,*) '*** Error: it looks like you gave and X or Y LCC', &
                     'coodinate in meters,'
                write(*,*) '    but MMIF expects coords in kilometers.'
                write(*,*) '    Program stopping.'
                stop
             end if
          else
             write(*,*) 'Unrecognized Keyword after "GRID": ',trim(word2)
             write(*,*) 'Next keyword must be one of:'
             write(*,*) '   IJ, LL (or LATLON), or KM (or PROJ, LCC, PS EM)'
             stop
          end if

       case ("LAYERS")

          call get_word(line,2,word2)
          call uppercase(word2)

          if (word2 == "K" .or. word2 == "AVG" .or. word2 == "MEAN") then
             iVertMap = 0 ! call aggregate for vertical coordinate mapping
             allocate( kzin(nzOut)   ) ! layer mappings
             allocate( kz1(nzOut)    ) ! beg MET layer for each OUTPUT layer
             allocate( kz2(nzOut)    ) ! end MET layer for each OUTPUT layer
             do k = 1,nzOut            ! read all the layers
                call get_word(line,k+2,word3) ! k+2: skip "LAYERS K"
                read(word3,*,iostat=i) kzin(k)    ! read K layers
                if (i /= 0) goto 86               ! error reading line
             end do
             zface = -999.             ! flag to call avg_zface()
          else if (word2 == "TOP" .or. word2 == "ZFACE") then
             iVertMap = 1 ! call interpolate for vertical coordinate mapping
             do k = 1,nzOut            ! read all the layers
                call get_word(line,k+2,word3) ! k+2: skip "LAYERS K"
                read(word3,*,iostat=i) zface(k)   ! read TOP of layers
                if (i /= 0) goto 86               ! error reading line
             end do
             zface(0) = 0.                        ! the ground
             zmid(0)  = 0.                        ! the ground
             do k = 1,nzOut
                zmid(k) = (zface(k) + zface(k-1)) / 2.
             end do
          else if (word2 == "MID" .or. word2 == "ZMID") then
             iVertMap = 2 ! call interpolate for vertical coordinate mapping
             do k = 1,nzOut            ! read all the layers
                call get_word(line,k+2,word3) ! k+2: skip "LAYERS K"
                read(word3,*,iostat=i) zmid(k)    ! read MIDdle of layers
                if (i /= 0) goto 86               ! error reading line
             end do
             zmid(0)  = 0.                        ! the ground
             zface(0) = 0.                        ! the ground
             do k = 1,nzOut-1
                zface(k) = (zmid(k) + zmid(k+1)) / 2.
             end do
             ! extrapolate to find the top of the top layer
             zface(nzOut) = zmid(nzOut) + (zmid(nzOut) - zface(nzOut-1))
             ! now that we have zface, make sure zmid is actually in the middle
             do k = 1,nzOut
                zmid(k) = (zface(k) + zface(k-1)) / 2.
             end do
          else
             write(*,*) 'Unrecognized Keyword after "LAYERS": ',trim(word2)
             write(*,*) 'Next keyword must be one of:'
             write(*,*) '   K, TOP, or MID'  ! left off AVG/MEAN
             stop
          end if

       case ("STABILITY") 

          call get_word(line,2,word2)
          call uppercase(word2)
          if (word2 == "SRDT") then
             PGtype = "SRDT"
          else if (word2 == "GOLDER") then
             PGtype = "GOLDER"
          else
             write(*,*) 
             write(*,*) 'Unrecognized Keyword after "STABILITY": ',trim(word2)
             write(*,*) 'Next keyword must be one of:'
             write(*,*) '   SRDT or GOLDER'
             write(*,*) 
             stop
          endif

       case ("MIXHT")

          write(*,*) 
          write(*,*) "The keyword MIXHT was changed to CALSCI_MIXHT to avoid confusion"
          write(*,*) "with the keyword AER_MIXHT. Please edit your mmif.inp file."
          write(*,*) 
          stop
          

       case ("PBL_RECALC","RECALC_PBL","PBL",   &
            "CAL_MIXHT","SCI_MIXHT","CALSCI_MIXHT","SCICAL_MIXHT")

          call get_word(line,2,word2)
          call uppercase(word2)

          if (word2 == "T" .or. word2 == "TRUE" .or. word2 == ".TRUE.") then
             CalcPBL = "MMIF" ! the true/false is for backward compatibility
          else if (word2 == "F" .or. word2 == "FALSE" .or. word2 == ".FALSE.") & 
               then
             CalcPBL = "WRF"  ! the true/false is for backward compatibility
          else if (word2 == "WRF") then
             CalcPBL = "WRF"
          else if (word2 == "MMIF" .or. word2 == "RECALC") then
             CalcPBL = "MMIF"
          else
             write(*,*) 
             write(*,*) 'Unrecognized Keyword after "CALSCI_MIXHT" (AKA ',trim(word), &
                  '): ',trim(word2)
             write(*,*) 'Next keyword must be one of:'
             write(*,*) '   WRF (default) or MMIF'
             write(*,*) 
             stop
          endif

       case ("ORIGIN")

          call get_word(line,2,word2)
          read(word2,*,err=89) origlat
          if (origlat < -90. .or. origlat > 90.)then
             write(*,*)
             write(*,*) 'Error in keyword ORIGIN: lat must be between -90 and 90.'
             write(*,*)
             stop
          endif

          call get_word(line,3,word2)
          read(word2,*,err=89) origlon
          if (origlon < -180. .or. origlon > 360.)then
             write(*,*)
             write(*,*) 'Error in keyword ORIGIN: lon must be between -180 and 360.'
             write(*,*)
             stop
          endif

       case ("METFORM")

          call get_word(line,2,MetForm)

       case ("CLOUDCOVER","CC")

          call get_word(line,2,word2)
          call uppercase(word2)

          if (word2 == "WRF" .or. word2 == "CLDFRA") then
             cloudCover = "WRF"
          else if (word2 == "ANGEVINE" .or. word2 == "COAMPS") then
             cloudCover = "ANGEVINE"
          else if (word2 == "RANDALL" .or. word2 == "MM5AERMOD") then
             cloudCover = "RANDALL"
          else
             write(*,*) 
             write(*,*) 'Unrecognized Keyword after "CLOUDCOVER" (', & 
                  trim(word),'): ',trim(word2)
             write(*,*) 'Next keyword must be one of:'
             write(*,*) '   WRF, ANGEVINE, or RANDALL'
             write(*,*) 
             stop
          endif

       case ("AER_MIXHT","AERMET_MIXHT","AERMET_PBL")

          call get_word(line,2,word2)
          call uppercase(word2)

          if (word2 == "WRF") then
             aer_mixht = "WRF"
          else if (word2 == "MMIF" .or. word2 == "RECALC") then
             aer_mixht = "MMIF"
          else if (word2 == "AERMET") then
             aer_mixht = "AERMET"
          else
             write(*,*) 
             write(*,*) 'Unrecognized Keyword after "AER_MIXHT" (', & 
                  trim(word),'): ',trim(word2)
             write(*,*) 'Next keyword must be one of:'
             write(*,*) '   WRF, MMIF, or AERMET'
             write(*,*) 
             stop
          endif

       case ("OVER") 

          call get_word(line,2,word2)
          call uppercase(word2)

          if (word2 == "AUTO" .or. word2 == "DETECT") then
             iOver = 0 ! Detect using iswater
          else if (word2 == "LAND") then
             iOver = 1 ! assume all POINTs below are over LAND
          else if (word2 == "WATER") then
             iOver = 2 ! assume all POINTs below are over WATER
          else
             write(*,*) 
             write(*,*) 'Unrecognized Keyword after "OVER" (', & 
                  trim(word),'): ',trim(word2)
             write(*,*) 'Next keyword must be one of:'
             write(*,*) '   LAND, WATER, or AUTO (or DETECT)'
             write(*,*) 
             stop
          endif
          PtOver = iOver !  set new global default

       case ("AER_MIN_MIXHT")

          call get_word(line,2,word2)
          read(word2,*,err=94) aer_min_MixHt

       case ("AER_MIN_OBUK")

          call get_word(line,2,word2)
          read(word2,*,err=95) aer_min_obuk

       case ("AER_MIN_SPEED")

          call get_word(line,2,word2)
          read(word2,*,err=90) aer_min_speed

       case ("AER_USE_TSKC")

          call get_word(line,2,word2)
          read(word2,*,err=96) aer_use_TSKC

       case ("AER_USE_NEW")

          call get_word(line,2,word2)
          read(word2,*,err=97) aer_use_NEW

       case ("AER_LAYERS")

          call get_word(line,2,word2)
          read(word2,*,err=91) nzPt_tmp(1)
          call get_word(line,3,word2)
          read(word2,*,err=91) nzPt_tmp(2)
          if (nzPt_tmp(1) < 0 .or. nzPt_tmp(1) > nzOut .or. &
              nzPt_tmp(2) < 0 .or. nzPt_tmp(2) > nzOut) then
             write(*,*) 
             write(*,'(a,i4)') " *** Error: AER_LAYERS values must be between 0 and",nzOut
             write(*,*) "    Program stopping."
             write(*,*) 
             stop
          end if
          if (nzPt_tmp(1) == 0 .and. nzPt_tmp(2) /= 0) then
             write(*,*) 
             write(*,*) "*** Error: if the top layer value of AER_LAYERs is > 0, then the "
             write(*,*) "    lower layer value must also be > 0."
             write(*,*) "    Program stopping."
             write(*,*) 
             stop
          end if

       case ("FSL_INTERVAL")

          call get_word(line,2,word2)
          read(word2,*,err=92) iUppFreq_tmp

       case ("POINT")

          line_aer_loc = line  ! save location from this line for the next AERout

       case ("INPUT")

          iMetFile = iMetFile + 1
          call get_word(line,2,METfile(iMetFile))

       case ("OUTPUT")

          iOut = iOut + 1

          call get_word(line,2,OutForm(iOut))
          call uppercase(OutForm(iOut))
          iOutUnit(iOut)  = 10 + iOut  ! 11, 12, 13, etc.

          call get_word(line,4,OutFile(iOut)) ! for all lines containing "output"

          if (OutForm(iOut) == 'QAPLOT') then

             call get_word(line,3,OutType(iOut))
             call uppercase(OutType(iOut))

             if      (OutType(iOut) == "BLN") then
             else if (OutType(iOut) == "BNA") then
             else if (OutType(iOut) == "DAT") then
             else if (OutType(iOut) == "KML") then
             else
                write(*,*) 
                write(*,*) 'Unrecognized Keyword after "QAPLOT": ',  &
                     trim(OutType(iOut))
                write(*,*) 'Next keyword must be one of:'
                write(*,*) '   BLN, BNA, DAT, or KML'
                write(*,*) 
                stop
             endif

          else if (OutForm(iOut) == 'CALPUFF') then

             calmet_version(iOut) = 5.8

             call get_word(line,3,OutType(iOut))
             call uppercase(OutType(iOut))

             if      (OutType(iOut) == "USEFUL") then
             else if (OutType(iOut) == "CALMET") then
             else if (OutType(iOut) == "TERRAIN") then
             else
                write(*,*) 
                write(*,*) 'Unrecognized Keyword after "CALPUFF": ',  &
                     trim(OutType(iOut))
                write(*,*) 'Next keyword must be one of:'
                write(*,*) '   USEFUL, CALMET, or TERRAIN'
                write(*,*) 
                stop
             endif

          else if (OutForm(iOut) == 'CALPUFFV6') then

             calmet_version(iOut) = 6.4 ! need only be different from "5.8"

             call get_word(line,3,OutType(iOut))
             call uppercase(OutType(iOut))

             if      (OutType(iOut) == "USEFUL") then
             else if (OutType(iOut) == "CALMET") then
             else if (OutType(iOut) == "AUX" .or. &
                      OutType(iOut) == "CALMET.AUX") then
                OutType(iOut) = "AUX" ! remove synonyms
             else if (OutType(iOut) == "TERRAIN") then
             else
                write(*,*) 
                write(*,*) 'Unrecognized Keyword after "CALPUFFV6": ',  &
                     trim(OutType(iOut))
                write(*,*) 'Next keyword must be one of:'
                write(*,*) '   USEFUL, CALMET, AUX (or CALMET.AUX), or TERRAIN'
                write(*,*) 
                stop
             endif

          else if (OutForm(iOut) == 'SCICHEM') then

             call get_word(line,3,OutType(iOut))
             call uppercase(OutType(iOut))

             if      (OutType(iOut) == "USEFUL") then
             else if (OutType(iOut) == "BINARY") then
             else if (OutType(iOut) == "ASCII") then
             else if (OutType(iOut) == "SAMPLER") then
             else if (OutType(iOut) == "TERRAIN") then
             else
                write(*,*) 
                write(*,*) 'Unrecognized Keyword after "SCICHEM": ',  &
                     trim(OutType(iOut))
                write(*,*) 'Next keyword must be one of:'
                write(*,*) '   USEFUL, BINARY, ASCII, SAMPLER or TERRAIN'
                write(*,*) 
                stop
             endif

          else if (OutForm(iOut) == 'AERMET') then

             call get_word(line,3,OutType(iOut))
             call uppercase(OutType(iOut))

             if      (OutType(iOut) == "USEFUL") then
             else if (OutType(iOut) == "CSH")    then
             else if (OutType(iOut)(1:3) == "BAT") then
                OutType(iOut) = "BAT" ! remove synonyms
             else if (OutType(iOut) == "ONSITE") then
                ! AERMET requires uppercase filenames
                call uppercase_filename(OutFile(iOut)) 
             else if (OutType(iOut) == "UPPERAIR" .or. &
                  OutType(iOut) == "FSL") then
                OutType(iOut) = "FSL" ! remove synonyms
                iUppFreq(iOut) = iUppFreq_tmp
                ! AERMET requires uppercase filenames
                call uppercase_filename(OutFile(iOut))
             else if (OutType(iOut) == "AERSFC") then
                ! AERMET requires uppercase filenames
                call uppercase_filename(OutFile(iOut))
             else
                write(*,*) 
                write(*,*) 'Unrecognized Keyword after "AERMET": ',  &
                     trim(OutType(iOut))
                write(*,*) 'Next keyword must be one of:'
                write(*,*) '   USEFUL, ONSITE, FSL (or UPPERAIR) or AERSFC'
                write(*,*) 
                stop
             endif

          else if (OutForm(iOut) == 'AERMOD') then

             call get_word(line,3,OutType(iOut))
             call uppercase(OutType(iOut))
             if     (OutType(iOut) == "USEFUL") then
             else if(OutType(iOut) == "SURFACE" .or. OutType(iOut) == "SFC") then
                OutType(iOut) = "SFC" ! remove synonyms
             else if(OutType(iOut) == "PROFILE" .or. OutType(iOut) == "PFL") then
                OutType(iOut) = "PFL" ! remove synonyms
            else
                write(*,*) 
                write(*,*) 'Unrecognized Keyword after "AERMOD": ',  &
                     trim(OutType(iOut))
                write(*,*) 'Next keyword must be one of:'
                write(*,*) '   USEFUL, SFC (SURFACE), or PFL (PROFILE)'
                write(*,*) 
                stop
             endif

          else if (OutForm(iOut) == 'AERCOARE') then

             call get_word(line,3,OutType(iOut))
             call uppercase(OutType(iOut))
             if (OutType(iOut) == "USEFUL") then
             else if (OutType(iOut) == "DATA") then
             else
                write(*,*) 
                write(*,*) 'Unrecognized Keyword after "AERCOARE": ',  &
                     trim(OutType(iOut))
                write(*,*) 'Next keyword must be one of:'
                write(*,*) '   USEFUL or DATA'
                write(*,*) 
                stop
             end if

          else
             write(*,*) 
             write(*,*) 'Unrecognized Keyword after "OUTPUT": ',trim(OutForm(iOut))
             write(*,*) 'Next keyword must be one of:'
             write(*,*) '   CALPUFF, CALPUFFv6, AERMET, AERMOD, AERCOARE, or SCICHEM'
             write(*,*) 
             stop
          end if
!
!-----For the single-point (AER*) OUTPUTs, read line_aer_loc to get the location
!
          if (OutForm(iOut)(1:3) == "AER" .and. word == "OUTPUT") then

             nzPt(iOut,1) = nzPt_tmp(1)              ! use until next val given
             nzPt(iOut,2) = nzPt_tmp(2)              ! use until next val given

             call count_words(line_aer_loc,num_words)
             if (num_words == 0) then
                write(*,*) 
                write(*,*) "*** Error in control file: no POINT line ",&
                     "before first OUTPUT AER... line."
                write(*,*) 
                stop
             end if
             if (num_words < 4) then                 ! sanity check
                write(*,*) 
                write(*,*) "*** Error in control file: POINT line "// &
                     "needs (at least) 4 words."
                write(*,'(a,i3,a)') "     Problematic line (has",num_words, &
                     " words):"
                write(*,*) 
                write(*,*) trim(line_aer_loc)
                write(*,*) 
                stop
             end if

             call get_word(line_aer_loc,2,word2)
             call uppercase(word2)

             if (word2 == "IJ") then 
                ijPt(iOut) = 1  ! means user gave I,J for this point

                call get_word(line_aer_loc,3,word3) ! get I,J coords
                read(word3,*,err=85) iPt(iOut)
                
                call get_word(line_aer_loc,4,word3)
                read(word3,*,err=85) jPt(iOut)
             else if (word2 == "LL" .or. word2 == "LATLON") then
                ijPt(iOut) = 2  ! means user gave Lat,Lon for this point

                call get_word(line_aer_loc,3,word3) ! get Lat,Lon coords
                read(word3,*,err=85) PtLat(iOut)

                call get_word(line_aer_loc,4,word3)
                read(word3,*,err=85) PtLon(iOut)
             else if (word2 == "KM" .or. word2 == "PROJ"        & ! some synonyms
                  .or. word2 == "LCC" .or. word2 == "PS" .or. word2 == "EM") then
                ijPt(iOut) = 3  ! means user gave X,Y for this point

                call get_word(line_aer_loc,3,word3) ! get X,Y (projected) coords
                read(word3,*,err=85) PtXlcc(iOut)

                call get_word(line_aer_loc,4,word3)
                read(word3,*,err=85) PtYlcc(iOut)
             else
                write(*,*) 
                write(*,*) 'Unrecognized 2nd Keyword in line: '
                write(*,*) trim(line_aer_loc)
                write(*,*) '2nd keyword must be one of:'
                write(*,*) '    IJ, LL (or LATLON), or KM (or PROJ, LCC, PS, EM)'
                write(*,*) 
                stop
             end if

             if (num_words > 4) then
                call get_word(line_aer_loc,5,word3)
                call uppercase(word3) ! should have no effect on numbers
                if (word3 == "OL" .or. word3 == "LAND") then
                   PtOver(iOut) = 1   ! over land
                else if (word3 == "OW" .or. word3 == "WATER") then
                   PtOver(iOut) = 2   ! over water
                else ! must be giving a time zone
                   read(word3,*,err=93) PtZone(iOut)
                endif
             end if

             if (num_words > 5) then
                call get_word(line_aer_loc,6,word3)
                call uppercase(word3)
                if (word3 == "OL" .or. word3 == "LAND") then
                   PtOver(iOut) = 1   ! over land
                else if (word3 == "OW" .or. word3 == "WATER") then
                   PtOver(iOut) = 2   ! over water
                else 
                   write(*,*) 
                   write(*,*) 'Unrecognized 6th Keyword in line: '
                   write(*,*) trim(line_aer_loc)
                   write(*,*) '6th keyword must be one of:'
                   write(*,*) '    LAND (or OL), or WATER (or OL)'
                   write(*,*) 
                   stop
                endif
             end if

          end if ! if (OutForm(iOut)(1:3) == "AER" .and. word == "OUTPUT") then
          
       case DEFAULT
          
          write(*,*) 
          write(*,*) "Unrecognized 1st keyword in line:"
          write(*,*) trim(line)
          write(*,*) 
          stop

       end select

       read(iCtrlUnit,999,iostat=ios) line ! read next line, skipping comments
       call count_words(line,num_words)    ! and blank lines
       do while ((any(line(1:1) == comment) .or. num_words == 0) .and. ios == 0)
          read(iCtrlUnit,999,iostat=ios) line
          call count_words(line,num_words)
       end do

    end do  ! do while (ios == 0)

    close(iCtrlUnit) ! close the control file, we're done with it
!
!-----Do some sanity checks for missing required keywords, mis-matches, etc.
!
    if (ibyr == -9999) then
       write(*,*) 
       write(*,*) "*** Error: Required keyword START not found in control file."
       write(*,*) "    Program stopping."
       write(*,*) 
       stop
    end if

    if (ieyr == -9999) then
       write(*,*) 
       write(*,*) "*** Error: Required keyword STOP not found in control file."
       write(*,*) "    Program stopping."
       write(*,*) 
       stop
    end if

    do iOut = 1, NumOuts
       if (ijPt(iOut) /= 0 .and. nzPt(iOut,1) > nzPt(iOut,2)) then
          write(*,*) 
          write(*,*) "*** Error: MIN_LAYERS > MAX_LAYERS."
          write(*,*) "    Program stopping."
          write(*,*) 
          stop
       end if
    end do

    if (NumMetFiles == 0) then
       write(*,*) 
       write(*,*) "*** No MM5/WRF files specified, nothing to do!"
       write(*,*) "    Program stopping."
       write(*,*) 
       stop
    end if

    if (NumOuts == 0) then
       write(*,*) 
       write(*,*) "*** Warning: No OUTPUT files specified.  Maybe you just want"
       write(*,*) "    to make sure you can read the MM5/WRF files?"
       write(*,*) 
       if (.not. force_run) then
          write(*,*) "    Use the --force command-line option to proceed."
          write(*,*) "    Program stopping."
          stop
       end if
    end if
!
!-----Write all the stuff to the screen (copied from previous versions of mmif)
!
    write(*,'(a,i4.4,3i2.2)') '         Start date/hour:    ',ibyr,ibmo,ibdy,ibhr
    write(*,'(a,i4.4,3i2.2)') '           End date/hour:    ',ieyr,iemo,iedy,iehr
    if (iBegDat > iEndDat) then
       write(*,*) 'Starting date/hour is after Ending date/hour!'
       stop
    endif
    call TimeDiff(ibyr,ibmo,ibdy,ibhr, ieyr,iemo,iedy,iehr, irlg)
    irlg = irlg + 1         ! We count (and output) the last hour too
    write(*,'(a,i13,a)')'     Processing duration: ',irlg,' hours'

    write(*,'(a,i13)') '               Time zone: ',ibtz
    if (ibtz < -12 .or. ibtz > 12) then
       write(*,*) 'Time zone cannot be > +/-12! Stopping!'
       stop
    endif

    if (ijlatlon == 1) then
       write(*,*)' Set 3-D sub-grid using:             I            J'
       write(*,'(a,2i13)')   'Low-Lft cell of sub-grid: ',iBeg,jBeg
       write(*,'(a,2i13)')   'Upr-Rgt cell of sub-grid: ',iEnd,jEnd
    elseif (ijlatlon == 2) then
       write(*,*)' Set 3-D sub-grid using:           LAT          LON'
       write(*,'(a,2f13.5)') '     LL cell of sub-grid: ',BegLat,BegLon
       write(*,'(a,2f13.5)') '     UR cell of sub-grid: ',EndLat,EndLon
    elseif (ijlatlon == 3) then
       write(*,*)' Set 3-D sub-grid using:             X            Y'
       write(*,'(a,2f13.5)') '     LL cell of sub-grid: ',BegXlcc,BegYlcc
       write(*,'(a,2f13.5)') '     UR cell of sub-grid: ',EndXlcc,EndYlcc
    endif

    write(*,'(a,i13)') '        Number of layers: ',nzOut
    write(*,'(a,a13)') '   Pasquill-Gifford type: ',adjustr(PGtype)
    write(*,'(a,a13)') '      Cloud Cover source: ',adjustr(CloudCover)
    write(*,'(a,a13)') '   CAL/SCI Mixing height: ',adjustr(CalcPBL)
    write(*,'(a,a13)') '    AERMET Mixing height: ',adjustr(AER_mixht)
    write(*,'(a,a13)') '     AERMET Over default: ',adjustr(OverWhat(iOver))
    write(*,'(a,i13)') '       Number of  inputs: ',NumMetfiles
    write(*,'(a,i13)') '       Number of outputs: ',NumOuts
!
!-----Figure out the last time-stamp (in GMT-ibtz zone) we need to read to get
!     data from the worst-case PtZone(iOut).  Call that LastOut.
!
    call nDatHr2ymdh(iEndDat,iyr,imo,idy,ihr,23)
    ihr = ihr + ibtz - minval(PtZone)         ! latest time-stamp in ANY timezone
    call legal_timestamp(iyr,imo,idy,ihr,23)
    call ymdh2nDatHr(iyr,imo,idy,ihr,LastOut) ! last time-stamp we need

    return
!
!-----Errors land here
!
78  format(/,a)
79  format(5a)
80  write(*,'(/,3a)') "*** Error: Control file '",trim(ctrl_fname),"' not found."
    write(*,'(a)')    "           Use 'mmif --sample' to create a sample control file."
    write(*,'(a,/)')  "           Use 'mmif --help' to see a help message."
    stop
81  write(*,78) "*** Error reading control file values after keyword START."
    write(*,79) "    Trying to read: ",trim(word2)
    stop
82  write(*,78) "*** Error reading control file values after keyword STOP."
    write(*,79) "    Trying to read: ",trim(word2)
    stop
83  write(*,78) "*** Error reading control file values after keyword TIMEZONE."
    write(*,79) "    Trying to read: ",trim(word2)
    stop
84  write(*,78) "*** Error reading control file values after keyword GRID."
    write(*,79) "    Trying to read: ",trim(word2)
    stop
85  write(*,78) "*** Error reading control file values for coordinates."
    write(*,79) "    Problematic line:"
    write(*,79) trim(line)
    stop
86  write(*,78) "*** Error reading control file values after keyword LAYERS."
    write(*,79) "    Problematic line:"
    write(*,79) trim(line)
    stop
89  write(*,78) "*** Error reading control file value for keyword ORIGIN."
    write(*,79) "    Trying to read: ",trim(word2)
    stop
90  write(*,78) "*** Error reading control file value for keyword AER_MIN_SPEED."
    write(*,79) "    Trying to read: ",trim(word2)
    stop
91  write(*,78) "*** Error reading control file value for keyword AER_LAYERS."
    write(*,79) "    Problematic line:"
    write(*,79) trim(line)
    stop
92  write(*,78) "*** Error reading control file value for keyword FSL_INTERVAL."
    write(*,79) "    Trying to read: ",trim(word2)
    stop
93  write(*,78) "*** Error reading control file TIMEZONE value for keyword POINT."
    write(*,79) "    Problematic line:"
    write(*,79) trim(line)
    stop
94  write(*,78) "*** Error reading control file value for keyword AER_MIN_MIXHT."
    write(*,79) "    Trying to read: ",trim(word2)
    stop
95  write(*,78) "*** Error reading control file value for keyword AER_MIN_OBUK."
    write(*,79) "    Trying to read: ",trim(word2)
    stop
96  write(*,78) "*** Error reading control file value for keyword AER_USE_TSKC."
    write(*,79) "    Trying to read: ",trim(word2)
97  write(*,78) "*** Error reading control file value for keyword AER_USE_NEW."
    write(*,79) "    Trying to read: ",trim(word2)
    stop

  end subroutine parse_control_file
!
!******************************************************************************
!
  subroutine get_word(line,which_word,word)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     This subroutine returns the Nth word from a line.
!
!     Development History:
!     2013-09-09  Original Development (ENVIRON International Corp.), following
!                 smp2post (developed by ENVIRON for EPRI and SCICHEM).

    character (len=*) :: line       ! input string
    character (len=*) :: word       ! output string
    integer           :: which_word ! which word to return
    
    integer           :: i,j, this_word, this_len  ! local variables
!
!-----Entry point
!
    i = 1
    this_len = len_trim(line)
    do while (i < this_len)
       if (any(line(i:i) == comment)) this_len = i - 1 ! ignore commented parts
       i = i + 1
    end do
    
    this_word = 0
    i = 1

    do while (this_word < which_word)
       if (any(line(i:i) == delim)) then ! advance to next non-space
          do while (any(line(i:i) == delim) .and. i < this_len)
             i = i + 1
          end do
       end if                           ! line(i:i) now the beginnig of next word
       if (i < this_len) then
          j = i + 1                     ! next character
       else
          j = i                         ! this character
       end if

       if (line(i:i) == '"') then       ! advance to matching double quote
          do while (line(j:j) /= '"' .and. j < this_len)
             j = j + 1                  ! advance to end of this word (next ")
          end do
       else if (line(i:i) == "'") then  ! advance to matching single quote
          do while (line(j:j) /= "'" .and. j < this_len)
             j = j + 1                  ! advance to end of this word (next ')
          end do
       else                             ! word doesn't start with a quote
          do while ((.not. any(line(j:j) == delim)) .and. j < this_len)
             j = j + 1                  ! advance to end of this word
          end do
          if (j > i .and. any(line(j:j) == delim)) j = j - 1
       end if
       this_word = this_word + 1
       if (this_word == which_word) then
          word = trim(line(i:j))
          if (word(1:1) == '"' .or. word(1:1) == "'") then     
             word = word(2:(len_trim(word)-1)) ! remove the quotes
          end if
          return                        ! done, got the right word, exit
       else if (j == this_len) then
          write(*,*) "*** Error: Asked for word ",which_word,", but only ", &
               this_word," words found."
          write(*,*) "           Problematic line is:"
          write(*,*) trim(line)
          stop
       end if
       i = j + 1                        ! start at next character
    end do
  
  end subroutine get_word
!
!*************************************************************************
!
  subroutine count_words(line,num_words)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     This subroutine counts the number of words in a line.
!
!     Development History:
!     2013-09-05  Original Development (ENVIRON International Corp.)

    implicit none

    character (len=*) :: line       ! input string
    integer           :: num_words  ! the number of words found
    
    integer           :: i,j,this_len
!
!-----Entry point
!
    i = 1
    this_len = len_trim(line)
    do while (i < this_len)
       if (any(line(i:i) == comment)) this_len = i - 1 ! ignore commented parts
       i = i + 1
    end do

    num_words = 0
    i = 1

    do while (i < this_len)
       if (any(line(i:i) == delim)) then ! advance to next non-space
          do while (any(line(i:i) == delim) .and. i < this_len)
             i = i + 1
          end do
           ! what if there's a delimiter at the end of the line?
          if (i == this_len .and. any(line(i:i) == delim) ) return
       end if                           ! line(i:i) now the beginnig of nextword
       
       j = i + 1                        ! next character
       if (line(i:i) == '"') then       ! advance to matching double quote
          do while (line(j:j) /= '"' .and. j < this_len)
             j = j + 1                  ! advance to end of this word (next ")
          end do
       else if (line(i:i) == "'") then  ! advance to matching single quote
          do while (line(j:j) /= '"' .and. j < this_len)
             j = j + 1                  ! advance to end of this word (next ')
          end do
       else                             ! word doesn't start with a quote
          do while ((.not. any(line(j:j) == delim)) .and. j < this_len)
             j = j + 1                  ! advance to end of this word
          end do
       end if
       num_words = num_words + 1
       i = j                            ! start at next character
    end do
  
  end subroutine count_words
!
!******************************************************************************
!
  subroutine command_line(force_run,lsfc_recalc,debug)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     This subroutine gets the command line.  When Fortran 2003 is finally
!     implemented, this entire routine can be discarded in favor of
!     GET_COMMAND_ARGUMENT() and COMMAND_ARGUMENT_COUNT().  In the mean time,
!     one could also use http://www.winteracter.com/f2kcli.
!
!     Development History:
!     2009-05-26  Original Development (ENVIRON International Corp.)
!     2012-01-31  Added --debug flag, updated sample control file.
!     2013-02-25  Lengthened file/path strings from 120 to 256 characters, 
!                 based on user feedback.
!     2013-09-23  Fixed problem with "mmif --force" trying to open the file
!                 named "--force" as a control file.
!     2014-04-24  Added option to force re-diagnosis of u10,T2,q2.
!     2020-10-17  Conform to Fortran 2003 standard, discard iargc and getarg
!
!------------------------------------------------------------------------------
!
    implicit none

    character (len=256) :: string
    logical             :: force_run   ! don't stop at non-fatal errors
    logical             :: lsfc_recalc ! Always calculate u10,t2,q2
    logical             :: debug
    integer             :: i

    ctrl_fname = 'mmif.inp'   ! default control filename

    i = 0
    do while (i < command_argument_count())
       i = i + 1

       call get_command_argument(i, string)

       if (adjustl(string) == "--help" .or. adjustl(string) == "-h") then
          call usage
       elseif (adjustl(string) == "--version") then
          write(*,*) "MMIF VERSION 4.1 2023-10-30"
          stop
       elseif (adjustl(string) == "--force") then
          force_run = .true.
       elseif (adjustl(string) == "--recalc") then
          lsfc_recalc = .true.
       elseif (adjustl(string) == "--debug") then
          debug = .true.
       elseif (adjustl(string) == "--sample") then
          call sample_input
       else
          ctrl_fname = string
       endif

    end do

    return
  end subroutine command_line
!
!******************************************************************************
!
  subroutine usage
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     Prints some helpful information on how to use this program
!
!     Development History:
!     2011-09-30  New with MMIF v2.0
!     2013-03-05  Added --version switch
!     2013-09-26  Added second tip, and clarified the language
!     2013-05-14  Added --recalc switch
!
!------------------------------------------------------------------------------
!
1   format(2a)
!
!-----Entry point
!
     write(*,1) "Usage: mmif [-h | --help] [--force] [--sample] [filename]"
     write(*,1) "Where"
     write(*,1) "  --force   don't stop execution after non-fatal errors"
     write(*,1) "  --recalc  re-calculate u10,T2,q2 from lowest MET layer"
     write(*,1) "  --sample  write a sample control file to the screen"
     write(*,1) "  --version print the version and exit"
     write(*,1) "  -h        show this help message"
     write(*,1) "  --help    show this help message"
     write(*,1) " filename   control filename, default is 'mmif.inp'"
     write(*,*)
     write(*,1) "Tip: use 'mmif mmif.inp > mmif.out' to re-direct the screen",&
          " output to a file."
     write(*,*)
     write(*,1) "Tip: use 'mmif --sample > mmif.inp' to create a MMIF input",&
          " file."
     stop

   end subroutine usage
!
!******************************************************************************
!
   subroutine sample_input
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     Prints a sample mmif keyword-driven input file
!
!     Development History:
!     2011-09-30  New with MMIF v2.0
!     2013-09-26  Adjusted to match MMIF v3.0 keyword format
!     2014-09-18  Added AER_MIN_MIXHT, AER_MIN_OBUK, and some general clean-up
!                 and clarification - based on user's feedback.
!     2016-03-17  PBL_recalc changed to MIXHT, Added AER_MIXHT option
!     2016-08-05  Changed MIXHT to CALSCI_MIXHT to avoid confusion with AER_MIXHT
!     2018-12-27  New keyword: CLOUDCOVER 
!     2020-10-17  New keyword: OVER, and some new default values (deferred)
!
!------------------------------------------------------------------------------
!
1    format(a)
!
!-----Entry point
!
     write(*,1) "; This file can be space-delimited or comma-delimited, or a mixture."
     write(*,1) "; Comment characters are #, ;, and !.  Blank lines are ignored."
     write(*,1) "; Omitting optional keywords is the same as giving their default values."
     write(*,1) "; START, STOP, and TimeZone are the only required keywords, the rest are optional."
     write(*,1) "; Keywords are case in-sensitve, filenames are not (depends on your OS)."
     write(*,1) "; Filenames may contain spaces, if enclosed in quotes. "
     write(*,1) ""
     write(*,1) "# START and STOP can be either of the forms below, or YYYY-MM-DD_HH:mm:ss."
     write(*,1) ""
     write(*,1) "start      2008 07 04 01 ; start time in LST for TimeZone, hour-ending format"
     write(*,1) "stop       2008070600    ; end   time in LST for TimeZone, hour-ending format"
     write(*,1) ""
     write(*,1) "# TimeZone is relative to GMT, i.e. -5 (GMT-05) is the US East Coast"
     write(*,1) ""
     write(*,1) "TimeZone   -10   ! default is zero, i.e. GMT-00"
     write(*,1) ""
     write(*,1) "# MMIFv3.x auto-detects if INPUT files are MM5 or WRF files, so METFORM"
     write(*,1) "# needs to be included only if MMIF guesses wrong, and you need to over-ride."
     write(*,1) ""
     write(*,1) "# MetForm WRF"
     write(*,1) ""
     write(*,1) "# ORIGIN (optional) can be used to OVER-RIDE the origin of X,Y projected "
     write(*,1) "# coordinate system, which is normally set from the parameters of the MM5/WRF"
     write(*,1) "# file.  This keyword is REQUIRED for Mercator projections."
     write(*,1) ""
     write(*,1) "# origin 40.0 -97.0  ! RPO Projection"
     write(*,1) ""
     write(*,1) "# GRID has three options: IJ, LL (or latlon), or KM (or PROJ,LCC,PS,EM),"
     write(*,1) "# followed by two lower-left coordinates, and two upper-right coordinates."
     write(*,1) "# Default is to output the whole grid, after trimming 5 points off each edge."
     write(*,1) ""
     write(*,1) "grid       IJ -5,-5 -5,-5   ! default"
     write(*,1) ""
     write(*,1) "# LAYERS has three options: TOP, MID, or K; followed by the values to be used."
     write(*,1) "# TOP and MID are in meters. Default is from the EPA/FLM 2009 Guidance."
     write(*,1) "# TOP is preferred: MMIF interpolates between MID points to get TOPs."
     write(*,1) ""
     write(*,1) "layers top 20 40 80 160 320 640 1200 2000 3000 4000   ! FLM CALMET Guidance (2009)"
     write(*,1) ""
     write(*,1) "# PG STABILITY class calculation method is either SRDT or GOLDER (default)"
     write(*,1) "# PG stability is used only for CALPUFF output."
     write(*,1) ""
     write(*,1) "stability  GOLDER      ! default"
     write(*,1) ""
     write(*,1) "# CLOUDCOVER source is one of WRF, ANGEVINE, or RANDALL"
     write(*,1) "#   WRF      use WRF's internal CLDFRA variable     (default in MMIF >= 3.2.2)"
     write(*,1) "#   ANGEVINE use Angevine et al. (2012) RH function (default in MMIF >= 2.2)"
     write(*,1) "#   RANDALL  use Randall (1994)/Zhao (1995) method  (default in MMIF < 2.2)"
     write(*,1) ""
     write(*,1) "CLOUDCOVER WRF         ! new default for MMIF-3.2.2"
     write(*,1) ""
     write(*,1) "# CALSCI_MIXHT is either WRF (default) or MMIF, to pass-through or "
     write(*,1) "# re-calculate the WRF mixing height for CALPUFF and SCICHEM outputs. "
     write(*,1) "# Use AER_MIXHT (below) for AERMET and AERMOD modes."
     write(*,1) ""
     write(*,1) "CALSCI_MIXHT WRF       ! default"
     write(*,1) ""
     write(*,1) "# AER_MIXHT (WRF, MMIF, or AERMET) controls the source of mixing height"
     write(*,1) "#     values you want to use in AERMET mode."
     write(*,1) "# AER_MIN_MIXHT is the lower bound on both Convective and Mechanical"
     write(*,1) "#     Mixing Heights in AERMOD mode."
     write(*,1) "# AER_MIN_OBUK  is the lower bound on Monin-Obukhov length, such that"
     write(*,1) "#     ABS(L) > AER_min_Obuk, in AERMOD mode."
     write(*,1) "# AER_MIN_SPEED is the lower bound on windspeed in AERMOD mode,"
     write(*,1) "#     passed through to THRESHOLD in AERMET mode."
     write(*,1) "# AER_USE_TSKC  is an ALPHA option to use cloud info instead of BULKRN."
     write(*,1) "# AER_USE_NEW introduced in MMIF 4.0 for AERMET 21112 and later versions."
     write(*,1) ""
     write(*,1) "aer_mixht  AERMET      ! default"
     write(*,1) "aer_min_mixht 1.0      ! default (same as AERMET)"
     write(*,1) "aer_min_obuk  1.0      ! default (same as AERMET)"
     write(*,1) "aer_min_speed 0.0      ! default (following Apr 2018 MMIF Guidance)"
     write(*,1) "aer_use_TSKC  F        ! default (using TSKC is an ALPHA option)"
     write(*,1) "aer_use_NEW   F        ! default (set to T for AERMET 21112 and later versions)"
     write(*,1) ""
     ! write(*,1) "# For AERMET mode, all POINT(s) given below are over what land surface:"
     ! write(*,1) "#   AUTO     (or DETECT) use the grid cell's land-use type (default)"
     ! write(*,1) "#   LAND     (or OL)     force all POINT output to be over land"
     ! write(*,1) "#   WATER    (or OW)     force all POINT output to be over water"
     ! write(*,1) "# Can also be specified per-POINT (see the Users Guide)"
     ! write(*,1) ""
     ! write(*,1) "OVER AUTO              ! default"
     ! write(*,1) ""
     write(*,1) "# See the Users Guide for the OUTPUT keyword details"
     write(*,1) ""
     write(*,1) "Output qaplot     BLN      domain.bln"
     write(*,1) "Output qaplot     BNA      domain.bna"
     write(*,1) "Output qaplot     DAT      points.dat"
     write(*,1) "Output qaplot     KML      qaplot.kml"    
     write(*,1) ""
     write(*,1) "Output calpuff    useful   calmet.info.txt"
     write(*,1) "Output calpuff    calmet   calmet.met"
     write(*,1) "Output calpuff    terrain  terrain.grd"
     write(*,1) ""
     write(*,1) "Output calpuffv6  useful   calmetv6.info.txt"
     write(*,1) "Output calpuffv6  calmet   calmetv6.met"
     write(*,1) "Output calpuffv6  aux      calmetv6.aux ! (basename must match calmet file)"
     write(*,1) "Output calpuffv6  terrain  terrainv6.grd"
     write(*,1) ""
     write(*,1) "Output scichem    useful   scichem.info.txt"
     write(*,1) "Output scichem    binary   scichem.bin.mcw"
     write(*,1) "Output scichem    ascii    scichem.asc.mcw"
     write(*,1) "Output scichem    sampler  scichem.smp  ! (not useful for SCICHEM-3 or newer)"
     write(*,1) "Output scichem    terrain  scichem.ter"
     write(*,1) ""
     write(*,1) "point  latlon     21.203   -157.925"
     write(*,1) "Output aercoare   useful   aercoare.near.Honolulu.info.inp"
     write(*,1) "Output aercoare   data     aercoare.near.Honolulu.csv"
     write(*,1) ""
     ! write(*,1) "point  latlon     21.28421 -157.87669  OW    ! force to be Over Water"
     ! write(*,1) "Output aermet     BAT      AERMET.OW.BAT"
     ! write(*,1) "Output aermet     CSH      AERMET.OW.csh"
     ! write(*,1) "Output aermet     useful   AERMET.OW.useful.txt"
     ! write(*,1) "Output aermet     onsite   AERMET.OW.dat"
     ! write(*,1) "Output aermet     upperair AERMET.OW.fsl"
     ! write(*,1) "Output aermet     aersfc   AERMET.OW.aersfc.dat"
     ! write(*,1) ""
     write(*,1) "POINT  LL         21.324   -157.929  -9   ! in GMT-9 timezone"
     write(*,1) "AER_layers        1        4  ! write 2m, 10m, and the 4 lowest WRF layers"
     write(*,1) "Output aermet     BAT      PHNL.BAT ! basename for SFC/PFL files" 
     write(*,1) "Output aermet     CSH      PHNL.csh ! basename for SFC/PFL files" 
     write(*,1) "Output aermet     useful   PHNL.useful.txt"
     write(*,1) "Output aermet     onsite   PHNL.dat"
     write(*,1) "Output aermet     upperair PHNL.fsl"
     write(*,1) "Output aermet     aersfc   PHNL.aersfc.dat"
     write(*,1) ""
     write(*,1) "FSL_INTERVAL      6        ! output every 6 hours, not 12 (the default)"
     write(*,1) "POINT  IJ         73       32"
     write(*,1) "Output aermet     FSL      'Upper air at PHTO.FSL'"
     write(*,1) "POINT  KM         60.0     -12.0"
     write(*,1) 'Output aermet     FSL      "Upper air at PHOG.FSL"'
     write(*,1) ""
     write(*,1) "POINT  latlon     20.963   -156.675  -9 ! in GMT-9 timezone"
     write(*,1) "AER_layers        0        0            ! write only 2m and 10m data"
     write(*,1) "Output aermod     useful   PJHJ.info.txt"
     write(*,1) "Output aermod     sfc      PJHJ.sfc"
     write(*,1) "Output aermod     PFL      PJHJ.pfl"
     write(*,1) ""
     write(*,1) "# INPUT gives filenames of either MM5 or WRF files"
     write(*,1) ""
     write(*,1) "INPUT test_problems\\wrf\\wrfout_d02_2008-07-04_00_00_00"
     write(*,1) "INPUT test_problems\\wrf\\wrfout_d02_2008-07-04_12_00_00"
     write(*,1) "INPUT test_problems\\wrf\\wrfout_d02_2008-07-05_00_00_00"
     write(*,1) "INPUT test_problems\\wrf\\wrfout_d02_2008-07-05_12_00_00"
     write(*,1) "INPUT test_problems\\wrf\\wrfout_d02_2008-07-06_00_00_00"
     write(*,1) "INPUT test_problems\\wrf\\wrfout_d02_2008-07-06_12_00_00"

     stop
   end subroutine sample_input
!
!******************************************************************************
!
END MODULE parse_control
