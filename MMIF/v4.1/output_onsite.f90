!Contains:
!
! subroutine onsite_useful(iUseful,iUPA,fname1,fname2,iOut)
! subroutine onsite_hour(iUnit,iOut)
! subroutine upperair_hour(iUnit,iUppFreq,iOut)
! subroutine write_aersfc(iUnit,fname1,iOut)
!
!******************************************************************************
!
subroutine onsite_batch(iUnit,fnameB,fname1,fname2,fname3,iOut)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     Writes an hour of data to the ONSITE file.  Use only MM5/WRF-generated
!     levels for temperature (skip the 10m level) and wind speed (skip 2m level).
!
!     Development History:
!     2011-09-30  New with MMIF v2.0.
!     2012-01-31  Write out the final zmid values to screen, like calmet_header.
!     2013-02-25  Lengthened file/path strings from 120 to 256 characters, 
!                 based on user feedback.
!     2013-05-02  Added support for multiple output points.
!     2013-07-12  For aggregation, use zPt(,) instead of zmid(), because the 
!                 latter is the average over the full 3-D domain.
!     2013-07-16  If useful filename ends in .csh, make a csh script instead
!                 of a batch file.
!     2013-07-16  Fixed ending XDATES time, so AERMET runs stop when data does.
!     2013-09-20  Renamed this subroutine from onsite_header to onsite_useful.
!     2015-01-29  Added terrain elevation (topo) at the screen output for points.
!     2015-03-25  The *.IN1's UPPERAIR block's LOCATION timezone should be 
!                 point-specific, not the global time zone.
!     2015-07-24  The bat/csh file should not include the path to the *.IN1 files.
!     2015-09-14  MMIF's *.IN2 file incorrect when no FSL output requested.
!     2016-03-16  Set THRESHOLD equal to AER_MIN_SPEED rather than always 0.5
!     2016-03-17  Updates for AER_MIXHT keyword to minimize AERMET warnings.
!     2016-08-14  New BAT and CSH keywords, useful file now same as AERMOD's
!     2018-05-04  Turned on STABLEBL ADJ_U* by default.
!     2019-09-05  Added "UAWINDOW -6 6 " keyword: at high latitudes, the morning
!                 sounding fell outside the default "UAWINDOW -1 1" so no Ziconv.
!     2021-09-28  Adapted for overhauled version of AERMET with over
!                 water processing.
!     2023-10-30  Added additional variables to ONSITE file for use in over
!                 water processing with COARE algorithms inside AERMET
!------------------------------------------------------------------------------
!
  USE met_fields
  USE functions
  implicit none
!
!-----Variable declaration
!
  character (len=*), intent(in) :: fnameB  ! filename of batch/csh       file
  character (len=*), intent(in) :: fname1  ! filename of output ONSITE   file
  character (len=*), intent(in) :: fname2  ! filename of output UPPERAIR file
  character (len=*), intent(in) :: fname3  ! filename of output AERSFC   file
  character (len=256)   :: basename        ! basename of the output SFC/PFL files
  character (len=256)   :: basepath        ! path (dir) of the output SFC/PFL
  character (len=256)   :: line            ! temporary string for output
  integer,  intent(in)  :: iUnit           ! Logical unit for output
  integer,  intent(in)  :: iOut            ! location to be output
  real                  :: ulat, ulon      ! temporary location
  character (len=1)     :: EW, NS          ! E or W, N or S
  character (len=1)     :: slash,backslash ! for Linux vs. DOS paths
  integer,  parameter   :: zone = 0        ! data aleady in local time zone
  integer               :: n10             ! 0 or 1: number of 10m levels to use
  integer               :: i,j,k,l,n       ! local index vars
  integer               :: islash,ibas     ! char positions within filename
  integer               :: idot,istr       ! char positions within filename
  integer               :: iy,im,id,ih     ! ending year-mo-dy, with hours 1-24
  logical               :: ok              ! used to detect missing path

  slash     = char(47) ! AKA "forward slash"
  backslash = char(92)
!
!-----FORMAT statements
!
1 format(a,2(" ",i4,"/",i2.2,"/",i2.2))
2 format(a,i6,1x,f8.3,a,1x,f8.3,a,1x,i3,1x,f10.2)
3 format(a,i2,a)
4 format(a,i2,5(a,i2.2))
5 format(a)
6 format(a,99f7.1)
7 format(a,i2,2f9.2)
!
!-----Entry point
!
  i = iPt(iOut)  ! short-hand
  j = jPt(iOut) 

! AERMOD uses hours 1-24, but MMIF will accept CALPUFF-like 0-23.  
! Here, this makes the XDATES ending time-stamp be 1 day past then end of data.

  iy = ieyr ; im = iemo ; id = iedy ; ih = iehr
  call legal_timestamp(iy,im,id,ih,24) 

  if (xlon(i,j) >= 0.) then ! AERMET requires positive values
     EW = "E"                     ! with a character E or W
     ulon = xlon(i,j)
  else
     EW = "W"
     ulon = -xlon(i,j)
  endif
  if (ylat(i,j) >= 0.) then ! FSL format requires positive values
     NS = "N"                     ! with a character N or S
     ulat = ylat(i,j)
  else
     NS = "S"
     ulat = -ylat(i,j)
  endif

  if ((zPt(iOut,nzPt(iOut,1)) > 13. .and. zPt(iOut,nzPt(iOut,2)) > 13.) .or. &
       nzPt(iOut,2) == 0) then
     n10 = 1 ! do include the 10m level
  else
     n10 = 0 ! don't include 10m level, lowest MM5/WRF level is already < 13m
  endif

! write the final levels being written to the screen

  write(*,'(a,i6,2a)') " Output #",iOut," MET-to-ONSITE layers for ",trim(fname1)
  write(*,'(a)')      "   Level  Interface(m)     Center(m)"
  write(*,'(8x,f14.2)') zface(0)
  write(*,'(8x,14x,f14.2)') 2.
  if (n10 == 1) write(*,'(8x,14x,f14.2)') 10.
  if (nzPt(iOut,2) > 0) then
     do k = nzPt(iOut,1), nzPt(iOut,2)
        write(*,'(i8,2f14.2)') k,zface(k),zPt(iOut,k)
     end do
  end if
  write(*,*)

! write the Land-use code, and initial z0, Bowen, Albedo for this point

  write(*,'(a,i6,a)') " For ONSITE output #",iOut,", at initial output time:"
  write(*,'(a,f9.4)') "        Elevation (m): ",topo(i,j)
  write(*,'(a,i9)')   "         Landuse code: ",ilu(i,j)
  write(*,'(a,f9.6)') " Roughness length (m): ",z0(i,j)
  write(*,'(a,f9.4)') "          Bowen ratio: ",bowen(i,j)
  write(*,'(a,f9.4)') "     Noon-time Albedo: ",albedo(i,j)
  write(*,*)

! find some important characters within the filename

  istr = len_trim(OutFile(iOut))             ! len of non-blank part

  idot = index(OutFile(iOut),".",.true.)-1   ! last char before .ext
  if (idot <= 0) idot = istr                 ! might not be one

  islash = index(fnameB,slash,.true.)        ! look for a path ending in "/"
  if (islash == 0) &
     islash = index(fnameB,backslash,.true.) ! look for a path ending in "\"

  ibas = islash + 1                          ! islash might be zero (no path)

  basename = fnameB(ibas:idot)               ! base name for *.IN1, IN2, etc.
  basepath = fnameB(1:islash)

  if (islash > 0) then
     inquire(file=basepath, exist=ok)
     if (.not. ok) then
        write(*,*) "*** Error: directory does not exist: ",trim(fnameB(1:islash))
        write(*,*)
        stop
     end if
  end if

! Write a little batch file, or script, to run AERMET.
! If output is to a directory, the bat/csh should not include the path.

  open(iUnit, file=trim(fnameB), status='unknown')

!  if (OutType(iOut) == "CSH") then ! aermet v18081 now accepts command-line inps
     write(iUnit,5) "aermet " // trim(basename) // ".IN1"
     if (.not. aer_use_NEW) then
        write(iUnit,5) "aermet " // trim(basename) // ".IN2"
        write(iUnit,5) "aermet " // trim(basename) // ".IN3"
     endif
!  else ! BAT file
!     write(iUnit,5) "copy   " // trim(basename) //  ".IN1 AERMET.INP"
!     write(iUnit,5) "aermet.exe AERMET.INP"
!     if (.not. aer_use_NEW) then
!       write(iUnit,5) "copy   " // trim(basename) // ".IN2 AERMET.INP"
!       write(iUnit,5) "aermet.exe AERMET.INP"
!       write(iUnit,5) "copy   " // trim(basename) // ".IN3 AERMET.INP"
!       write(iUnit,5) "aermet.exe AERMET.INP"
!     endif
!     write(iUnit,5) "del    AERMET.INP"
!  end if

  close(iUnit)

! Write the AERMET Stage 1 input file

  open(iUnit,file=trim(basepath) // trim(basename) // ".IN1",status='unknown')
  write(iUnit,5) "JOB"
  write(iUnit,5) "    MESSAGES " // trim(basename)// ".ER1"
  write(iUnit,5) "    REPORT   " // trim(basename)// ".OU1"
  write(iUnit,*)

  if (fname2 /= 'none') then
     write(iUnit,5) "UPPERAIR"
     write(iUnit,5) "    DATA     " // trim(fname2(ibas:))// "      FSL"
     write(iUnit,5) "    EXTRACT  " // trim(fname2(ibas:))// ".IQA"
     write(iUnit,5) "    QAOUT    " // trim(fname2(ibas:))// ".OQA"
     write(iUnit,*)
     write(iUnit,2) "    LOCATION",99999,ulat,NS,ulon,EW,-PtZone(iOut)!FSL in GMT
     write(iUnit,1) "    XDATES  ",ibyr,ibmo,ibdy,iy,im,id
     write(iUnit,5) "    AUDIT    UAPR UAHT UATT UATD UAWD UAWS"
     write(iUnit,*)
  endif
     
  if (aer_use_NEW) then
     write(iunit,5)    "PROG"
  else
     write(iUnit,5)    "ONSITE"
  endif
  if (aer_use_NEW) then
     if (iswater(i,j)) then  
        write(iUnit,5)    "    DATA     " // trim(fname1(ibas:)) // " OW"
     else
        write(iUnit,5)    "    DATA     " // trim(fname1(ibas:)) // " OL"
     endif
  else
     write(iUnit,5)    "    DATA     " // trim(fname1(ibas:))
  endif
  write(iUnit,5)    "    QAOUT    " // trim(fname1(ibas:))// ".OQA"
  write(iUnit,1)    "    XDATES  ",ibyr,ibmo,ibdy,iy,im,id
  write(iUnit,2)    "    LOCATION",99999,ulat,NS,ulon,EW,zone,topo(i,j)
  l = 1
  if (aer_use_NEW) then
!    CAM 11/29/22 add RDOW, TSEA, ZDEP
    if (iswater(i,j)) then 
     write(iUnit,5) &
      "    READ     1 OSYR OSMO OSDY OSHR INSO PRCP PRES MHGT TSKC HFLX LFLX USTR ZOHT VPTG MOBL WSTR RDOW TSEA ZDEP"
    else
     write(iUnit,5) &
      "    READ     1 OSYR OSMO OSDY OSHR INSO PRCP PRES MHGT TSKC HFLX LFLX USTR ZOHT VPTG MOBL WSTR"
    endif
  else
     if (aer_mixht == "AERMET") then
        if (aer_use_TSKC) then
           write(iUnit,5)    "    READ     1 OSYR OSMO OSDY OSHR INSO PRCP PRES TSKC"
        else
           write(iUnit,5)    "    READ     1 OSYR OSMO OSDY OSHR INSO PRCP PRES"
        endif
     else
        if (aer_use_TSKC) then
           write(iUnit,5)    "    READ     1 OSYR OSMO OSDY OSHR INSO PRCP PRES MHGT TSKC"
        else
           write(iUnit,5)    "    READ     1 OSYR OSMO OSDY OSHR INSO PRCP PRES MHGT"
        endif
     endif
  endif
  l = l + 1
  write(iUnit,5)    "    READ     2 HT01 TT01 RH01 DT01" ! 2m
  n = 1        ! level number in READ ?? statements
  if (n10 > 0) then
     l = l + 1
     write(iUnit,5) "    READ     3 HT02 WS02 WD02"      ! 10m
     n = n + 1 ! level number in READ ?? statements
  endif

  if (nzPt(iOut,2) > 0) then
     do k = nzPt(iOut,1), nzPt(iOut,2)
        l = l + 1
        n = k - nzPt(iOut,1) + n10 + 1 + 1 ! one of the +1's is for the 2m level
        write(iUnit,4) "    READ    ",l," HT",n," WS",n," WD",n," TT",n," RH",n
     end do
  end if
  write(iUnit,*) 
  
  if (aer_use_NEW) then
!   JAT 11/18/21 change the format statement to accomodate more digits to the
!   right of the decimal for zO.
!    line = " (I4,3I2.2,4F10.3,F10.0,7F10.3)"     

!    JAT 1/18/22 change the format statement to accomodate more digits to the
!    right of the decimal for u*
!    line = " (I4,3I2.2,4F10.3,F10.0,3F10.3,F10.5,3F10.3)"

!    CAM 11/29/22 change format statement to accomodate new variables for
!    aercoare   
!     line = " (I4,3I2.2,4F10.3,F10.0,2F10.3,2F10.5,3F10.3)"     ! OSYR OSMO OSDY OSHR INSO PRCP PRES MHGT TSKC HFLX
    if(iswater(i,j))then                                               ! LFLX USTR ZOHT VPTG MOBL WSTR
     line = " (I4,3I2.2,4F10.3,F10.0,2F10.3,2F10.5,6F10.3)" ! OSYR OSMO OSDY
                                                   !  OSHR INSO PRCP PRES MHGT TSKC HFLX LFLX USTR ZOHT VPTG MOBL
                                                   ! WSTR RDOW TSEA ZDEP
    else
     line = " (I4,3I2.2,4F10.3,F10.0,2F10.3,2F10.5,3F10.3)"! OSYR OSMO OSDY OSHR INSO PRCP PRES MHGT TSKC HFLX 
                                                           !LFLX USTR ZOHT VPTG MOBL WSTR 
    endif
  else
     line = " (2x,4I2.2,3F10.3"      ! OSYR OSMO OSDY OSHR INSO PRCP PRES
     if (aer_mixht /= "AERMET") then
        line = trim(line) // ",F10.3" ! use  WRF/MMIF mixing height 
     else
        line = trim(line) // ",10x"   ! skip WRF/MMIF mixing height 
     end if
     if (aer_use_TSKC) then
        line = trim(line) // ",F10.0" ! use  TSKC cloud info
     else
        line = trim(line) // ",10x"   ! skip TSKC cloud info 
     end if
     line = trim(line) // ")"         ! close the parenthesis
  endif

  l = 1                            ! line number
  write(iUnit,3)    "    FORMAT  ",l,trim(line) ! Time, sfc params

  l = l + 1
  write(iUnit,3)    "    FORMAT  ",l," (10x,F10.2,20x,3F10.3)"  ! 2m level, DT01

  if (n10 > 0) then
     l = l + 1
!    JAT 11/11/21 change format label from 5 to 3 to get level number
!    to write to AERMET control file
!    write(iUnit,5) "    FORMAT  ",l," (10x,F10.2,2F10.3)"      ! 10m level
     write(iUnit,3) "    FORMAT  ",l," (10x,F10.2,2F10.3)"      ! 10m level
  end if

  if (nzPt(iOut,2) > 0) then
     do k = nzPt(iOut,1), nzPt(iOut,2)
        l = l + 1
        write(iUnit,3) "    FORMAT  ",l," (10x,F10.2,4F10.3)"
     end do
  end if
  write(iUnit,*)
!
!-----AERMET only uses DT01, and currently ignores DT02 and DT03, etc.
!     Here's where to add them later, should AERMET be changed in the future.
!
  write(iUnit,7)    "    DELTA_TEMP   ",1,  2., zPt(iOut,1) ! must be a constant
!
!-----Because AERMET issues a warning each time HTnn changes, and because
!     the zmid changes with each hour (if using aggregation instead of 
!     interpolation for the vertical mapping), override the HTnn with OSHEIGHTS.
!     Other programs might be able to use that data, and it helps us humans
!     looking at the file to see the heights.
!
  if (n10 > 0) then
     write(iUnit,6) "    OSHEIGHTS  ",2.,10.
  else
     write(iUnit,6) "    OSHEIGHTS  ",2.
  end if
  if (nzPt(iOut,2) > 0) then
     write(iUnit,'(a,$)') "    OSHEIGHTS " ! no CR/LF yet
     do k = nzPt(iOut,1), nzPt(iOut,2)
        write(iUnit,'(f10.2,$)') zPt(iOut,k)
        if (mod(k,6) == 0 .and. k /= nzPt(iOut,2)) then   ! 6 values per line
           write(iUnit,*)                        ! CR/LF
           write(iUnit,'(a,$)') "    OSHEIGHTS " ! no CR/LF yet
        end if
     end do
  end if
  write(iUnit,*)
  write(iUnit,6)    "    THRESHOLD ",aer_min_speed
  if(aer_use_NEW) then
     write(iUnit,5)    "    RANGE WS 0 <= 50 99"
     write(iUnit,5)    "    RANGE WD 0 <= 360 999"
     write(iUnit,5)    "    RANGE TT -30 < 40 99"
     write(iUnit,5)    "    RANGE DT01 -2 < 5 9"
     write(iUnit,5)    "    RANGE INSO 0 <= 1250 9999"
     write(iUnit,5)    "    RANGE PRCP 0 <= 25400 -9"
     write(iUnit,5)    "    RANGE PRES 7500 < 10999 99999"
     write(iUnit,5)    "    RANGE MHGT 0 < 4000 9999"
     write(iUnit,5)    "    RANGE TSKC 0 <= 10 99"
     write(iUnit,5)    "    RANGE HFLX -100 < 800 -999"
     write(iUnit,5)    "    RANGE LFLX -100 < 800 -999"
     write(iUnit,5)    "    RANGE USTR 0 < 2 -9"
     write(iUnit,5)    "    RANGE ZOHT 0 < 2 999"
!    JAT 05/04/22  change range for VPTG to be 5 to 100 due to 
!    incorrect setting of VPTG in subroutine onsite_hour
!    write(iUnit,5)    "    RANGE VPTG 0 <= 5 -9"
     write(iUnit,5)    "    RANGE VPTG 5 <= 100 -9"
     write(iUnit,5)    "    RANGE MOBL -8888 <= 8888 -99999"
     write(iUnit,5)    "    RANGE WSTR 0 <= 2 -9"
!   CAM 11/29/22 add ranges for TSEA, ZDEP, and RDOW
    if(iswater(i,j))then 
     write(iunit,5)    "    RANGE TSEA -30 <= 50 999"
     write(iUnit,5)    "    RANGE ZDEP -10 <= 10 9999"
     write(iUnit,5)    "    RANGE RDOW  0 <= 1000 -9999"
    endif
  else
     write(iUnit,5)    "    RANGE WS 0 <= 50 999"
     write(iUnit,5)    "    RANGE WD 0 <= 360 999"
     write(iUnit,5)    "    RANGE TT -49 < 49 999"
     write(iUnit,5)    "    RANGE DT01 -2 < 5 999"
     write(iUnit,5)    "    RANGE INSO -1 < 1250 9999"
     write(iUnit,5)    "    RANGE PRES 7500 < 10999 99999"
     if (aer_mixht /= "AERMET") then
        write(iUnit,5)    "    RANGE MHGT 0 < 4000 9999"
     endif
  endif
  write(iUnit,*)
  if(aer_use_NEW) then
    if (iswater(i,j)) then
!    CAM 11/29/22 add RDOW, TSEA, ZDEP
     write(iUnit,5) "    AUDIT TT01 DT01 RH01 INSO PRCP PRES MHGT TSKC HFLX LFLX USTR ZOHT VPTG MOBL WSTR RDOW TSEA ZDEP"
    else
     write(iUnit,5) "    AUDIT TT01 DT01 RH01 INSO PRCP PRES MHGT TSKC HFLX LFLX USTR ZOHT VPTG MOBL WSTR"
    endif
  else
     if (aer_mixht == "AERMET") then
        write(iUnit,5) "    AUDIT TT01 DT01 RH01 INSO PRCP PRES"
     else
        write(iUnit,5) "    AUDIT TT01 DT01 RH01 INSO PRCP PRES MHGT"
     endif
  endif
  if(.not. aer_use_NEW) then
     write(iUnit,*)
     close(iUnit)

! Write the AERMET Stage 2 input file

     open(iUnit,file=trim(basepath) // trim(basename) // ".IN2",status='unknown')
     write(iUnit,5) "JOB"
     write(iUnit,5) "    MESSAGES " // trim(basename)// ".ER2"
     write(iUnit,5) "    REPORT   " // trim(basename)// ".OU2"
     write(iUnit,*)
     if (fname2 /= 'none') then
        write(iUnit,5) "UPPERAIR"
        write(iUnit,5) "    QAOUT    " // trim(fname2(ibas:))// ".OQA"
        write(iUnit,*)
     end if
     write(iUnit,5) "ONSITE"
     write(iUnit,5) "    QAOUT    " // trim(fname1(ibas:))// ".OQA"
     write(iUnit,*)
     write(iUnit,5) "MERGE"
     write(iUnit,1) "    XDATES  ",ibyr,ibmo,ibdy,iy,im,id
     write(iUnit,5) "    OUTPUT   " // trim(fname1(ibas:))// ".MER"
     close(iUnit)

! Write the AERMET Stage 3 input file

     open(iUnit,file=trim(basepath) // trim(basename) // ".IN3",status='unknown')
     write(iUnit,5) "JOB"
     write(iUnit,5) "    MESSAGES " // trim(basename) // ".ER3"
     write(iUnit,5) "    REPORT   " // trim(basename) // ".OU3"
     write(iUnit,*)
     write(iUnit,5) "METPREP"
     write(iUnit,5) "    DATA     " // trim(fname1(ibas:)) // ".MER"
     write(iUnit,1) "    XDATES  ",ibyr,ibmo,ibdy,iy,im,id
     write(iUnit,5) "    MODEL    AERMOD"
     if (aer_use_BULKRN(iOut)) & 
        write(iUnit,5) "    METHOD   STABLEBL BULKRN"
     write(iUnit,5) "    METHOD   STABLEBL ADJ_U*"  ! regulatory-default option
     write(iUnit,5) "    METHOD   WIND_DIR NORAND"
     write(iUnit,5) "    METHOD   ASOS_ADJ NO_ADJ"
     if (fname2 /= 'none') then
        write(iUnit,5) "    METHOD   UASELECT SUNRISE"
        write(iUnit,5) "    UAWINDOW -6 6"
     endif
     write(iUnit,5) "    AERSURF  " // trim(fname3(ibas:))
     write(iUnit,5) "    OUTPUT   " // trim(basename) // ".SFC"
     write(iUnit,5) "    PROFILE  " // trim(basename) // ".PFL"
  else
! Combine 3 stages into one file
     write(iUnit,5) "METPREP"
     write(iUnit,1) "    XDATES  ",ibyr,ibmo,ibdy,iy,im,id
     write(iUnit,5) "    MODEL    AERMOD"
     if (aer_use_BULKRN(iOut)) & 
        write(iUnit,5) "    METHOD   STABLEBL BULKRN"
     write(iUnit,5) "    METHOD   STABLEBL ADJ_U*"  ! regulatory-default option
     write(iUnit,5) "    METHOD   WIND_DIR NORAND"
     write(iUnit,5) "    METHOD   ASOS_ADJ NO_ADJ"
     if (fname2 /= 'none') then
        write(iUnit,5) "    METHOD   UASELECT SUNRISE"
        write(iUnit,5) "    UAWINDOW -6 6"
     endif
     write(iUnit,5) "    AERSURF  " // trim(fname3(ibas:))
     write(iUnit,5) "    OUTPUT   " // trim(basename) // ".SFC"
     write(iUnit,5) "    PROFILE  " // trim(basename) // ".PFL"
  endif
  close(iUnit)

end subroutine onsite_batch
!
!******************************************************************************
!
subroutine onsite_hour(iUnit,iOut,iAERSFC)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     Writes an hour of data to the ONSITE file.
!
!     Development History:
!     2011-09-30  New with MMIF v2.0.
!     2012-02-21  Bug fix: wind direction changed to MET convention.
!     2013-05-02  Added support for multiple output points.
!     2013-07-12  For aggregation, use zPt(,) instead of zmid(), because the 
!                 latter is the average over the FULL 3-D domain.
!     2014-03-06  New version of uv2sd to support PS/EM projections.
!     2014-07-30  Prevent output before requested 1st timestamp in this timezone.
!     2014-10-09  Bug fix: only first AERSFC output file contained values.
!     2014-11-26  No reason RH needs to be rounded to nearest integer.
!     2016-10-10  Add flush() calls for easier re-starts
!     2020-04-17  Change Bowen ratio calc to be day-time average, was 24-hour avg.
!     2020-12-15  Set MIXHT to be not less than Venkatram (1980) mechanical 
!                 mixing height for stable hours (MFED, equation 27, page 22).
!     2021-09-28  Adapted for overhauled version of AERMET with over
!                 water processing.
!
!------------------------------------------------------------------------------
!
  USE met_fields
  USE functions
  USE parse_control, ONLY : iBegDat, iEndDat
  implicit none
!
!-----Variable declaration
!
  integer, intent(in) :: iUnit             ! Logical unit for output
  integer, intent(in) :: iOut              ! location to be output
  integer, intent(in) :: iAERSFC           ! the iOut of related AERSFC output
  integer   :: iyr,imo,idy,ihr,ThisHour    ! time stamps
  integer   :: n10                         ! 0 or 1: include 10m level?
  integer   :: i,j,k, jday
  real      :: rh,speed,dir,SunAngle
  real      :: ZiOut, ZiMech, ZiConv, wstarOut
  real      :: shfluxOut, ustarOut, lhfluxOut, VPTGout, z0Out, Lout 
  real      :: rdow, tsea, sstd

!
!-----Constants, conversion factors
!  
  integer, parameter :: wban    = 99999    ! dummy wban and wmo numbers,
  integer, parameter :: wmo     = 999999   ! maybe change to IIIJJJ?
  real,    parameter :: SBLMAX=4000.0, CBLMAX=4000.0 ! from AERMET's MP2.INC
!
!-----Entry point
!
  i = iPt(iOut)  ! short-hand
  j = jPt(iOut) 

  call nDatHr2ymdh(nDatHr,iyr,imo,idy,ihr,24) ! AERMOD uses hours 1-24, LST
  ihr = ihr - ibtz + PtZone(iOut)             ! Back to GMT, then to LST for iOut
  call legal_timestamp(iyr,imo,idy,ihr,24)    ! Back to 1-24, LST for this iOut

  call ymdh2nDatHr(iyr,imo,idy,ihr,ThisHour)
  if (ThisHour < iBegDat) return              ! before 1st output in this zone
  if (ThisHour > iEndDat) return              ! after last output in this zone

  call dat2jul(iyr,imo,idy,jday)              ! get julian day

  if ((zPt(iOut,nzPt(iOut,1)) > 13. .and. zPt(iOut,nzPt(iOut,2)) > 13.) .or. &
       nzPt(iOut,2) == 0) then
     n10 = 1 ! do include the 10m level
  else
     n10 = 0 ! don't include 10m level, lowest MM5/WRF level is already < 13m
  endif

  rh = min(100.,q2(i,j)/qs_fn(t2(i,j),psfc(i,j))*100.)

  wstarOut = wstar(i,j)                    ! protect from change

  !call venkatram_mech_mixh(ustar(i,j),ZiMech)
  !call pbl_limits(AER_min_MixHt,SBLMAX, mol(i,j),ustar(i,j),wstarOut, ZiMech)

  ZiConv      = aerpbl(i,j)                ! protect from change
  call pbl_limits(AER_min_MixHt,CBLMAX, mol(i,j),ustar(i,j),wstarOut, ZiConv)
  
!
!-----Define temporary output variables for new AERMET
!
! JAT 11/22/21 if absolute value of L > 8888 then reset to 8888 with 
! appropriate sign
!
! PKK 12/15/2021: move ZiOut definition outside the if block to make sure it is defined
!                 for backward compatibility with the MMIF or WRF mixing height option
  ZiOut     = ZiConv
  if (aer_use_NEW) then
     shfluxOut = shflux(i,j)
     lhfluxOut = lhflux(i,j)
     ustarOut  = ustar(i,j)
     Lout      = mol(i,j)
     if (Lout < -8888.) Lout=-8888.
     if (Lout > 8888.) Lout=8888.
     VPTGout   = VPTG
!pkk12152021     ZiOut     = ZiConv
     z0Out     = z0(i,j)
! CAM adding new variabeles
     rdow      = qlw(i,j)
     tsea      = sst(i,j)-273.15
     sstd      = 0.002
     
!
!-----Limit the range of VPTG to between 0.0 and 0.005
!
!    JAT VPTG is supposed to be larger than 0.005, not less than so changing
!    min/max checks and fits the methodology in subroutine aermod_sfc_hour

     VPTGout = MAX(VPTGout,0.0) !first make sure not negative
     VPTGout = MAX(VPTGout,0.005) !minimum value is 0.005
     VPTGout = MIN(VPTGout,0.1) !do not exceed 0.1

!    VPTGout = MIN(VPTGout,0.005)
     if (Lout > 0.) then    ! stable conditions when Monin-Obukhov length is positive
!       ZiOut = 9999.       ! missing value flag for mixing height ! not reqd for onsite file
!       VPTGout = -.009     ! missing value flag for VPTG (divided by 1000 since output is multiplied by
!                           ! 1000
!       wstarOut = -9.      ! missing value flag for wstar
     endif
  endif

  !if (CalcPBL == "MMIF") then              ! same as AERMOD MFED eqn 27, page 22
  !   if (mol(i,j) < 0.) then               ! convective hours
  !      ZiOut = max( ZiMech, ZiConv )
  !   else                                  ! stable hours
  !      ZiOut = ZiMech
  !   endif
  !endif

  if (aer_use_NEW) then
!   JAT 11/18/21 change the format to accomodate more digits to the
!   right of the decimal for zo.  use f10.5 as done for the AERSURFACE
!   file instead of f10.3
!    write(iUnit,'(i4,3i2.2,12f10.3)') &

!   JAT 1/19/2022 change format of u* to accomodate more digits
!   to the right of the decimal.  use same as zo, f10.5
!   if using only 3 digits to right of decimal, can have u*=0
!   seems to occur at low wind speeds
!   CAM 11/29/22 modified for now variables for COARE in AERMET     
!    write(iUnit,'(i4,3i2.2,8f10.3,f10.5,3f10.3)') &
!   CAM 4/23/23 modified cldcvr to *10 for AERMET compatibility
     if(iswater(i,j))then
      write(iUnit,'(i4,3i2.2,7f10.3,2f10.5,6f10.3)') &
          iyr,imo,idy,ihr,               &    ! OSYR OSMO OSDY OSHR time-stamp
          qsw(i,j),                      &    ! INSO solar radiation (W/m^2) 
          rain(i,j)*100,                 &    ! PRCP rain rate (mm/hr*100)
          psfc(i,j)*10.,                 &    ! PRES (mb*10)
          ZiOut,                         &    ! MGHT PBL height (m)
          cldcvr(i,j)*10.,               &    ! TSKC cloud cover (tenths)
          shfluxOut,                     &    ! HFLX sensible heat flux (W/m^2)
          lhfluxOut,                     &    ! LFLX latent heat flux (W/m^2)
          ustarOut,                      &    ! USTR friction velocity (m/s)
          z0Out,                         &    ! ZOHT surface roughtness (m)
          VPTGOut*1000.,                 &    ! VPTG theta lapse rate (K/m*1000)
          Lout,                          &    ! MOBL Monin-Obukhov length (m)
          wstarOut,                      &    ! WSTR convective velocity scale (m/s)
          rdow,                          &    ! longwave downward radiation 
          tsea,                          &    ! sea surface temperature
          sstd                                ! buoy depth - hard coded
      else
      write(iUnit,'(i4,3i2.2,7f10.3,2f10.5,3f10.3)') &
          iyr,imo,idy,ihr,               &    ! OSYR OSMO OSDY OSHR time-stamp
          qsw(i,j),                      &    ! INSO solar radiation (W/m^2)
          rain(i,j)*100,                 &    ! PRCP rain rate (mm/hr*100)
          psfc(i,j)*10.,                 &    ! PRES (mb*10)
          ZiOut,                         &    ! MGHT PBL height (m)
          cldcvr(i,j)*10.,               &    ! TSKC cloud cover (tenths)
          shfluxOut,                     &    ! HFLX sensible heat flux (W/m^2)
          lhfluxOut,                     &    ! LFLX latent heat flux (W/m^2)
          ustarOut,                      &    ! USTR friction velocity (m/s)
          z0Out,                         &    ! ZOHT surface roughtness (m)
          VPTGOut*1000.,                 &    ! VPTG theta lapse rate (K/m*1000)
          Lout,                          &    ! MOBL Monin-Obukhov length (m)
          wstarOut                            ! WSTR convective velocity scale (m/s)
      endif
  else
     write(iUnit,'(i4,3i2.2, 3f10.3,$)') &
          iyr,imo,idy,ihr,               &    ! OSYR OSMO OSDY OSHR time-stamp
          qsw(i,j),                      &    ! INSO solar radiation (W/m^2) 
          rain(i,j)*100,                 &    ! PRCP rain rate (mm/hr*100)
          psfc(i,j)*10.                       ! PRES (mb*10)
     if (aer_mixht /= "AERMET") &
        write(iUnit,'(f10.3,$)')  ZiOut  ! MGHT PBL height (m)
     if (aer_use_TSKC) &
        write(iUnit,'(f10.3,$)')  cldcvr(i,j) ! cloud cover (tenths)
     write(iUnit,*)                           ! trailing CF/LF
  endif

  write(iUnit,'(10x,f10.2,20x,3f10.3)') &
       2.,                            &    ! HT01 2 meters
       t2(i,j)-273.15,                &    ! TT01 2m temperature (C)
       rh,                            &    ! RH01 relative humidity (%)
       tOut(i,j,1)-t2(i,j)                 ! DT01 temp diff (C)

  if (n10 > 0) then
!
!-----Rotate the wind direction from the projection to true north
!
     call uv2sd(u10(i,j),v10(i,j),cosalpha(i,j),sinalpha(i,j), &
          .true.,speed,dir)

     write(iUnit,'(10x,f10.2,2f10.3)') &
       10.,                            &   ! HT02 10 meters
       speed,                          &   ! WS02 wind speed (m/s)
       dir                                 ! WD02 wind dir (deg)
  end if

  if (nzPt(iOut,2) > 0) then
     do k = nzPt(iOut,1), nzPt(iOut,2)

        call uv2sd(uOut(i,j,k),vOut(i,j,k),cosalpha(i,j),sinalpha(i,j), &
          .true.,speed,dir)
        rh = nint(min(100., qOut(i,j,k) / & 
             qs_fn(tOut(i,j,k),pOut(i,j,k)) * 100.))
        
        write(iUnit,'(10x,f10.2,4f10.3)') &
             zPt(iOut,k),                & ! HTnn height (m)
             speed,                      & ! WSnn wind speed (m/s)
             dir,                        & ! WDnn wind dir (deg)
             tOut(i,j,k)-273.15,         & ! TTnn 10m temperature (C)
             rh                            ! RHnn relative humidity (%)
     end do
  end if

  call flush(iUnit)                        ! make for easier re-starts
!
!-----Calculate the run-averaged z0, albedo, and Bowen ratio, to 
!     support AERMET (mimic AERSURFACE).  We choose to use the run-average
!     values rather than the output of geodat(), to account for snow cover.
!     For example, in WRF, ALBEDO includes snow-cover effects but is equal 
!     to ALBBCK when there is no snow.  Note that this albedo is the 
!     noon-time albedo, not corrected for sun angle (as AERMET does).!

  if (iAERSFC > 0) then

     call sundat(ylat(i,j),-xlon(i,j),-PtZone(iOut),jday,ihr,SunAngle)

     aersfc(imo,1,1,iAERSFC) = aersfc(imo,1,1,iAERSFC) + albedo(i,j)   ! sum albedo
     aersfc(imo,1,2,iAERSFC) = aersfc(imo,1,2,iAERSFC) + 1             ! num albedo

     if (SunAngle > 0.) then
        aersfc(imo,2,1,iAERSFC) = aersfc(imo,2,1,iAERSFC) + bowen(i,j) ! sum bowen
        aersfc(imo,2,2,iAERSFC) = aersfc(imo,2,2,iAERSFC) + 1          ! num bowen
     endif

     aersfc(imo,3,1,iAERSFC) = aersfc(imo,3,1,iAERSFC) + z0(i,j)       ! sum z0
     aersfc(imo,3,2,iAERSFC) = aersfc(imo,3,2,iAERSFC) + 1             ! num z0
  endif

  return
end subroutine onsite_hour
!
!******************************************************************************
!
subroutine upperair_hour(iUnit,iUppFreq,iOut)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     Writes the header and data to the UPPERAIR file in FSL format.
!
!     Development History:
!     2011-09-30  New with MMIF v2.0.
!     2012-02-21  Bug fix: wind direction changed to MET convention.
!     2013-03-18  Bug fix: surface level report was in kts, now in 10th of a m/s.
!     2013-05-02  Added support for multiple output points.
!     2014-03-06  New version of uv2sd to support PS/EM projections.
!     2016-10-10  Add flush() calls for easier re-starts
!
!------------------------------------------------------------------------------
!
  USE met_fields
  USE functions
  implicit none
!
!-----Variable declaration
!
  integer, intent(in) :: iUnit               ! Logical unit for output
  integer, intent(in) :: iOut                ! location to be output
  integer, intent(in) :: iUppFreq            ! Output every iUppFreq hours
  integer   :: iyr,imo,idy,ihr,i,j,k         ! time stamps, index vars
  real      :: ulat, ulon                    ! temporary location
  real      :: rh, tdew, t2dew, speed, dir
  character :: EW*1, NS*1, Mth*3             ! E or W, N or S, JAN or DEC
!
!-----Constants, conversion factors
!  
  integer, parameter :: wban    = 99999      ! dummy wban and wmo numbers,
  integer, parameter :: wmo     = 999999     ! maybe change to IIIJJJ?
  integer, parameter :: bad     = 32767      ! FSL bad/missing values flag
!
!-----Entry point
!
  i = iPt(iOut)  ! short-hand
  j = jPt(iOut) 

  call nDatHr2ymdh(nDatHr,iyr,imo,idy,ihr,23) ! get date from current time-stamp
  call add_hour(iyr,imo,idy,ihr,-ibtz)        ! FSL is in GMT, 0 - 23

  call Mth_by_num(imo,.true.,Mth)             ! find the 3-char month

  if (mod(ihr,iUppFreq) /= 0) return          ! skip these hours

  if (xlon(i,j) >= 0.) then                   ! FSL format requires positive 
     EW = "E"                                 ! values with a character E or W
     ulon = xlon(i,j)
  else
     EW = "W"
     ulon = -xlon(i,j)
  endif
  if (ylat(i,j) >= 0.) then                   ! FSL format requires positive 
     NS = "N"                                 ! values with a character N or S
     ulat = ylat(i,j)
  else
     NS = "S"
     ulat = -ylat(i,j)
  endif

  write(iUnit,5) 254,ihr,idy,Mth,iyr          ! FSL Header
5 format(3i7,a9,i8)

                !  WBAN WMO LAT     LON     Elev            Release_time
  write(iUnit,6) 1,wban,wmo,ulat,NS,ulon,EW,nint(topo(i,j)),ihr*100
6 format(3i7,f7.2,a,f6.2,a,i6,i7)

                !  Hydro,MXWD, TROPL,LINES,   TINDEX,SOURCE
  write(iUnit,7) 2,32767,32767,32767,nzOut+5+0,32767,32767 
! note: nzOut+5+0 = nzOut levels + 5 header lines + 0 mandadory levels
7 format(7i7) ! used by most FSL lines
  
                ! Station,sonde,ws_units
  write(iUnit,8) 3,"NONE",32767,"ms"                 
8 format(i7,a14,i21,a7)

  call uv2sd(u10(i,j),v10(i,j),cosalpha(i,j),sinalpha(i,j), &
          .true.,speed,dir)
  rh = min(100., q2(i,j)/qs_fn(t2(i,j),psfc(i,j))*100.)
  t2dew = dewpoint_fn(rh,t2(i,j))

  write(iUnit,7) 9,                      & ! 9 = surface level
       nint(psfc(i,j)),                  & ! pressure (mb)
       nint(topo(i,j)),                  & ! height (m) (elevation)
       nint((t2(i,j)-273.15)*10.),       & ! temperature (1/10 of deg C)
       nint((t2dew-273.15)*10.),         & ! dew point (1/10 of deg C)
       nint(dir),                        & ! wind direction (deg)
       nint(speed*10.)                     ! wind speed (10th of m/s)
  
! FIXME: Maybe we should interpolate to mandatory levels?

  do k = 1, nzOut

     call uv2sd(uOut(i,j,k),vOut(i,j,k),cosalpha(i,j),sinalpha(i,j), &
          .true.,speed,dir)
     rh = min(100., qOut(i,j,k) / &
          qs_fn(tOut(i,j,k), pOut(i,j,k))*100.)
     tdew = dewpoint_fn(rh,tOut(i,j,k))

     write(iUnit,'(i7,6(1x,i6))') 5,     & ! 5:significant level, 6:wind level
          nint(pOut(i,j,k)),             & ! pressure (mb)
          nint(topo(i,j)+zPt(iOut,k)),   & ! height above MSL (m)
          nint((tOut(i,j,k)-273.15)*10), & ! temperature (10th of C)
          nint((tdew-273.15)*10),        & ! dew point (10th of C)
          nint(dir),                     & ! wind direction (deg)
          nint(speed*10.)                  ! wind speed (10ths of m/s)

  end do

  call flush(iUnit)                        ! make for easier re-starts

  return
end subroutine upperair_hour
!
!******************************************************************************
!
subroutine write_aersfc(iUnit,fname,iOut)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     Writes the header and data to an AERSURFACE output file.  Because
!     MM5 and WRF typically have much coarser resolution than a typical
!     NLCD92-based AERSURFACE run, we won't bother with making sectors.
!     All sectors would be the same here, because we don't have any sub-grid
!     scale land-use data.  Even though a MMIF run may be for only part of
!     a year, AERMET will bomb if all the months aren't present in the 
!     AERSURFACE output file.  So we'll fill in all the values with some sort
!     of missing value flag.
!
!     Development History:
!     2011-09-30  New with MMIF v2.0.
!     2013-09-22  Now gets its own output filename in the control file.
!     2014-10-09  Bug fix: only first AERSFC output file contained values.
!     2014-11-06  Require average Bowen Ratio be positive, or AERMET will bomb.
!
!------------------------------------------------------------------------------
!
  USE met_fields
  USE functions
  implicit none
!
!-----Variable declaration
!
  character (len=*), intent(in) :: fname ! filename of output #1, ONSITE
  integer,   intent(in) :: iUnit         ! Logical unit for output
  integer,   intent(in) :: iOut          ! location to be output
  integer               :: imo, i,j,k
!
!-----FORMAT statements
!
1 format(2a)
2 format(a,1x,f12.5)
3 format(3x,a,i5,i8,2f9.2,f10.5)
!
!-----Entry point
!
  i = iPt(iOut)  ! short-hand
  j = jPt(iOut) 

  do imo = 1,12 ! 12 months
     do k = 1,3 ! 1=albedo, 2=bowen, 3=z0
        if (aersfc(imo,k,2,iOut) > 0) then ! aersfc(,,1,) is sum, (,,2,) is count
           aersfc(imo,k,1,iOut) = aersfc(imo,k,1,iOut) / aersfc(imo,k,2,iOut)
        else
           aersfc(imo,k,1,iOut) = .99 ! some sort of missing value flag
        end if
     end do
     if (aersfc(imo,1,1,iOut) < 0.01) aersfc(imo,1,1,iOut) = 0.01 ! min albedo
     if (aersfc(imo,2,1,iOut) < 0.01) aersfc(imo,2,1,iOut) = 0.01 ! pos bowen
  end do

  open(iUnit,file=fname,status='unknown')

  write(iUnit,1) "** Generated by MMIF VERSION 4.1 2023-10-30"
  write(iUnit,2) "** Center Latitude  (decimal degrees):",ylat(i,j)
  write(iUnit,2) "** Center Longitude (decimal degrees):",xlon(i,j)
  write(iUnit,1) "** Datum: ",datum
  write(iUnit,2) "** Study radius (km) for surface roughness:",deltax
  write(iUnit,1) "** The rest of the AERSURFACE inputs are not applicable"
  write(iUnit,1) "**"
  write(iUnit,1) "   FREQ_SECT  MONTHLY  1"
  write(iUnit,1) "   SECTOR   1    0  360"
  write(iUnit,1) "**           Month    Sect    Alb      Bo        Zo"
  do imo = 1, 12
     write(iUnit,3) "SITE_CHAR",imo,1, &
          aersfc(imo,1,1,iOut),        & ! albedo
          aersfc(imo,2,1,iOut),        & ! bowen ratio
          aersfc(imo,3,1,iOut)           ! roughness length
  end do

  close(iUnit)

  return
end subroutine write_aersfc
