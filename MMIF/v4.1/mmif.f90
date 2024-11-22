program mmif
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     MMIF processes raw MM5 (v3) and WRF/ARW (v2.1+) output directly to
!     CALPUFF/AERMET/AERMOD/AERCOARE/SCICHEM input file formats.
!
!     Development History (see also the file mmif_change_log.txt):
!     2009-05-26  Original Development: EPA R7, ENVIRON International Corp.
!                 Initially only output CALPUFF format (mimic CALMET).
!     2011-09-30  ENVIRON Added output for several other models, lat-lon/LCC
!                 subsetting, etc.  Required a major re-write and 
!                 generalization of the core code.  A placeholder for vertical
!                 interpolaction to fixed levels is included.
!     2011-12-12  Minor fix: clear up confusion between central and std lat/lon.
!     2012-01-31  Added support for WRF's 50-category NLCD land-use method.
!                 Removed user control of ZiMin, ZiMax.  Set to sane values.
!     2012-02-21  Bug fix: wind direction changed to MET convention (0 deg is N)
!                 for AERMOD modes.  Resulted in MMIFv2.1 patch 1.
!     2012-03-07  Bug fix: CALPUFFv6 time-stamps were off by 1 hour.
!                 Simplified the calculation of irlg, ndathr, and jday.
!     2012-09-04  Added more logic to detect un-supported LANDUSE versions,
!                 when z0,Albedo, and/or LAI were _not_ included in the MET data.
!     2012-10-01  Removed ZiMin,ZiMax from call to scichem_useful().
!     2013-01-22  Aggregation no longer supported for SCICHEM output.
!                 Added sanity check for SCICHEM output: zmid(nzOut) > max(topo).
!     2013-02-25  Lengthened file/path strings from 120 to 256 characters, 
!                 based on user feedback.
!     2013-03-05  Changed sanity check for AERCOARE output: instead of stopping,
!                 issue a warning for each hour and continue.
!     2013-05-01  Implement minimum AERMOD wind speed, following AERMET v12345.
!     2013-05-02  Tidied up the declarations, with better comments
!     2013-05-02  Moved sanity check for CALMET and WRF using same projection to
!                 mmif.f90, to facilitate moving OutForm from met_fields.f90.
!     2013-05-02  Added fixed-control-file-format supporting multiple outs.
!     2013-07-12  For aggregation, use zPt(,) instead of zmid(), because the 
!                 latter is the average over the full 3-D domain.
!     2013-07-15  Moved call to calc_vptg() so only called when needed.
!     2013-07-16  Made screen output more regular and pretty.
!     2013-07-16  In AER* modes, always write 2m and 10m levels, but never
!                 write MM5/WRF levels below 15m to avoid conflict between
!                 U10,V10 speed and speed at lower levels.
!     2013-09-05  Auto-detect MM5 vs. WRF files, no need to specify MetForm.
!     2013-09-08  Moved parsing of control file to module parse_control.f90.
!     2013-09-21  Bug fix: endif in the wrong place near the USGS/NLCD checks for
!                 missing z0,bowen,LAI (thanks to Bret Anderson for the report).
!     2013-09-23  Added support for several other land-use methods: NLCD50,
!                 MODIS, and IGBP_MODIS.  Also updated USGS to support 33 cats.
!     2014-02-30  Added support for SCICHEM 3.0 headers in the MEDOC file.
!     2014-03-06  Added calculation of cosalpha,sinalpha for PS/EM wind rotation.
!     2014-03-06  In SCICHEM 3.0: LAT,LON now where X,Y = 0,0 (cenlat,cenlon).
!     2014-03-06  Added MetForm, z0, ustar, albedo, and bowen to MEDOC output.
!     2014-04-24  Added option to force re-diagnosis of u10,T2,q2.
!     2014-07-30  Now prints both LST and GMT of each output timestamp.
!     2014-09-18  Add optional minimum mixing height and abs(L) in AERMOD modes.
!     2014-09-18  Move calls to pbl_limits() to output routines, instead of here.
!     2014-10-09  Bug fix: only first AERSFC output file contained values.
!     2014-10-09  Improved detection of related outputs (useful needs filenames)
!     2017-02-16  Set default values for tlat1,tlat2 to support EM projections.
!     2017-06-20  New output: BLN, BNA, or KML of 3D sub-domain.
!     2017-06-22  Changed "GMT" to "UTC" to be more correct.
!     2018-07-17  Added detection of skipped/missing hours in MMIF output.
!     2018-12-26  Read WRF's CLDFRA output and use for cldcvr if WRFv3.6 or newer.
!     2019-09-05  Added "UAWINDOW -6 6 " keyword for AERMOD mode. At high latitudes,
!                 the morning sounding falls outside the default "UAWINDOW -1 1" so
!                 no convective mixing heights were being calculated by AERMET. Only
!                 affects "aer_mixht AERMET" modes.
!     2019-11-08  Added reading CLDFRA, if it exists in MM5 files.
!     2019-11-08  Bug fix: some older versions of WRF don't have CLDFRA, so test
!                 and skip if it's not found. Error if CLOUDCOVER WRF has been chosen.
!     2020-04-17  Change Bowen ratio calculation to use day-time hours only. It was
!                 using all hours, which conflicts with the AERMET User Guide.
!     2020-07-09  Use lowest layer instead of U10, V10 if Z(1) < 13, to match the
!                 methodology when running in AERMET mode.
!     2020-10-17  Add ALPHA options to use TSKY in AERMET mode, and BULKRN option.
!     2020-11-30  Changed format statement for ASCII MEDOC files to avoid rounding
!                 roughness length values to 0.0000 (impossible value).
!     2020-12-15  When using MMIF’s re-diagnosis of the mixing height, use temporal
!                 smoothing – the same as AERMET does.
!     2021-06-24  Add ability to use WRF's hybrid vertical coordinate.
!     2021-09-28  Adapted for overhauled version of AERMET with over
!                 water processing.
!     2023-10-30  Added additional variables required for overwater processing
!                 using COARE algorithms in AERMET
!------------------------------------------------------------------------------
!     This program is free software; you can redistribute it and/or 
!     modify it under the terms of the GNU General Public License 
!     as published by the Free Software Foundation; either version 2 
!     of the License, or (at your option) any later version. 
!  
!     This program is distributed in the hope that it will be useful, 
!     but WITHOUT ANY WARRANTY; without even the implied warranty of 
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
!     GNU General Public License for more details. 
!------------------------------------------------------------------------------
! 
  USE wrf_netcdf
  USE met_fields
  USE functions
  USE parse_control
  implicit none
  include 'netcdf.inc'
!
!-----Variable declaration
!
  integer, parameter :: iMetUnit = 9      ! MM5 file unit number
  integer :: i,j,k, nzFinal               ! nzFinal only meaningful for CALMET
  integer :: iFlag                        ! Used to test if MM5 file

  real    :: ruff,alb,leaf                ! Temporary containers

  logical :: lNewHr,lFirst,ok             ! For program control
  logical :: lsfc_recalc                  ! Always calculate u10,t2,q2
  logical :: lu10,lt2,lq2                 ! Whether to re-diagnose these
  logical :: lz0,lalb,lmol,llai,lCLDFRA   ! Whether MM5/WRF has these
  logical :: force_run = .false.          ! Don't stop at optional errors
  logical :: scichem_warned = .false.     ! Ony nag about scichem errors once
  logical :: warnings = .false.           ! There were warnings, go back and look
!
!-----Local time variables
!      
  integer :: iErr,iLST,iUTC               ! Temporary 
  integer :: idot,istr                    ! char positions within filename
  integer :: wrote_header                 ! flag for 1st time thru loop &  errors
  integer :: iLastHr,iGetNext             ! Last hour read, next hour needed
  integer :: iTime                        ! index for time() stamps
  integer :: iy,im,id,ih,jday             ! Used to calculate WRF times.
  integer :: iy1,im1,id1,ih1,DiffHrs      ! Used to detect missing hours in WRF
  integer, external :: iymdh2idate        ! in TimeSubs.f90
!
!-----Local NetCDF variables
!
  logical :: nc_debug
  integer :: n_times,cdfid,rcode
  character (len=19), allocatable, dimension(:) :: times ! times in a WRF file
  character (len=256)   :: basename        ! to create names of SFC/PFL files
!
!-----Data statements
!
  data nc_debug       /.FALSE./ ! debug NetCDF (WRF) files?
!
!-----Set some defaults
!
  wrote_header   =  0      ! flag for writing OUTPUT file header
  lsfc_recalc    = .false. ! Don't re-calculate u10,T2,q2 by default
  lu10           = .false. ! assume we DON'T have 10m winds in the MM5/WRF data
  lt2            = .true.  !           DO    have temperature at 2m (K)
  lq2            = .false. !           DON'T have humitidy at 2m (kg/kg)
  lz0            = .false. !           DON'T have roughness length (m)
  lalb           = .false. !           DON'T have albedo (unitless)
  lmol           = .false. !           DON'T have Monin-Obukhov length (m)
  lCLDFRA        = .false. !           DON'T have cloud fraction (unitless)
  tlat1          = 0.      ! default
  tlat2          = 0.      ! default
  OverWhat       = (/"AUTO ","LAND ","WATER"/)  ! the values of PtOver
!
!-----Entry point
!
!-----Open and read control file
!
  call command_line(force_run,lsfc_recalc,pdebug)
  call parse_control_file(force_run)
  write(*,*) ! blank line
!
!-----Do some sanity checks before we proceed too far.
!
  if (zface(1) == 0) then
     write(*,*) "*** Error: lowest level entered in the control file is zero."
     write(*,*) "           MMIF already assumes the ground is 0.  First entry "
     write(*,*) "           should be the TOP level of the lowest level."
     write(*,*) "    Program stopping."
     write(*,*)
     stop
  end if
  if (iVertMap > 0 .and. any(index(OutForm,"CALPUFF") > 0)) then
     if (zface(1) /= 20.) then
        write(*,*) 
        write(*,*) "*** Error: CALPUFF requires the first ZFACE (interface) value = 20."
        write(*,*) "    Either use TOP and first entry = 20, or MID and first entry = 10."
        write(*,*) "    Currently defined ZFACE values are:"
        write(*,*) zface
        write(*,*) "    Program stopping."
        write(*,*)
        stop
     end if
  endif
  if (any(index(OutForm,"SCICHEM") > 0) .and. iVertMap == 0) then
     write(*,*) "*** Error: Aggregation is not available for SCICHEM (MEDOC) output"
     write(*,*) "    Program stopping."
     stop
  endif
!
!-----Determine initial OUTPUT layer mapping
!
  if (iVertMap == 0) then ! aggregation, not interpolation
     if (any(index(OutForm,"CALPUFF") > 0)) then
        write(*,*)'Initial MM5/WRF-to-OUTPUT layer mapping:'
        write(*,*)'  (This may change depending on depth of first MM5/WRF layer'
        write(*,*)'  relative to 20m, the first CALPUFF layer.)'
     else
        write(*,*)'MM5/WRF-to-OUTPUT layer mapping:'
     endif
     do k = 1,nzOut
        kz2(k) = kzin(k)
        if (k  ==  1) then
           kz1(k) = 1
        else
           kz1(k) = kz2(k-1) + 1
        endif
        write(*,'(a,i2,a,i3,a,i3,a,i3,a)') ' OUTPUT layer: ',k, &
             ' contains MM5/WRF layers ', kz1(K),' to ',kz2(K), &
             ' (',kz2(k)-kz1(k)+1,' layers).'
     end do
     write(*,*)
  end if
!
!-----Initialize some things
!
  lNewHr = .true.
  lFirst = .true.
  iLastHr = -999
!
!-----A few FORMAT statements used below
!
10 format(1x,3a,/,a)
11 format(1x,4a)
12 format(a,i6,a,i11," LST =",i11," UTC")
!
!###################################################
!-----MAIN loop over input meteorological files
!###################################################
!
  do iMETfile = 1,NumMETfiles
     inquire(file=METfile(iMetfile),exist=ok)
     if (.not. ok) then
        write(*,*) 
        write(*,*) '*** Error: file ',trim(METfile(iMETfile)),' not found.'
        write(*,*) '    Program stopping.'
        stop
     endif
!
!-----Auto-detect MM5 vs. WRF files
!
     if (MetForm == "duh") then ! not over-ridden in control file
        rcode = nf_open(METfile(iMETfile),NF_NOWRITE,cdfid)
        if (rcode == 0) then
           MetForm = 'WRF'
           rcode = nf_close(cdfid)

        else ! should be an MM5 file

           open(unit=iMetUnit,file=METfile(iMETfile),form='unformatted', &
                convert='big_endian',status='old')

           read(iMetUnit) iflag ! should be 0, big header
           close(iMetUnit)

           if (iflag == 0) then
              iErr = 0
              MetForm = 'MM5'
              rewind(iMetUnit)
           else
              write(*,*) "*** Error: Not a WRF file, but Flag for Big Header ", &
                   "not zero,"
              write(*,*) "           so probably not an MM5 file either.  "
              write(*,*) "           Auto-detection failed for ", &
                   trim(METfile(iMETfile))
              stop
           end if

        end if ! if (rcode == 0) then
     end if

     if (MetForm == "WRF") then

        rcode = nf_open(METfile(iMETfile),NF_NOWRITE,cdfid)

!
!-----Get the times in WRF file
!
        call get_max_times_cdf(METfile(iMETfile),n_times,nc_debug)
        if (allocated(times)) deallocate(times)
        allocate(times(n_times))
        call get_times_cdf(METfile(iMETfile),times,n_times,nc_debug)
!
!-----Pick the first time-stamp to read from this file, just for precip
!
        if (iLastHr > 0) then                   ! not the first WRF file
           call nDatHr2ymdh(iLastHr,iy,im,id,ih,23)
        else                                    ! the very first WRF file
           call nDatHr2ymdh(iBegDat,iy,im,id,ih,23)
           call add_hour(iy,im,id,ih,-1)        ! read one time-stamp before
        endif
        call ymdh2nDatHr(iy,im,id,ih,iGetNext)  ! next hour's precip we need

     else if (MetForm == "MM5") then

        open(unit=iMetUnit,file=METfile(iMETfile),form='unformatted', &
             convert='big_endian',status='old')

     else
        write(*,*) 
        write(*,*) "*** Error: auto-detection of MM5 vs. WRF file failed for file"
        write(*,*) trim(METfile(iMETfile))
        write(*,*) "   Program stopping."
        stop
     end if
!
!-----Print to the screen when we open a new file
!
     if (len_trim(METfile(iMETfile)) > 60) then
        write(*,10) 'Opened '//MetForm//' file: ',trim(METfile(iMETfile))
     else
        write(*,11) 'Opened '//MetForm//' file: ',trim(METfile(iMETfile))
     endif
           !
!-----Loop over times in the file
!
     iTime = 0                              ! iTime used only for WRF files
100  continue
     if (MetForm == 'MM5') then             ! processing MM5 files

        call read_mm5(iMetUnit,lFirst,lNewHr,lu10,lq2,lz0,lalb,lmol,llai,lCLDFRA,&
             iErr)

        if (iErr == 1)         goto 200     ! error, likely end of MM5 file
        if (iErr == 2)         goto 99      ! error, it's a wrf file, not mm5
        if (nDatHr > LastOut)  goto 999     ! done, exit
        if (nDatHr < iBegDat)  goto 100     ! read another
        if (nDatHr <= iLastHr) goto 100     ! already processed, read another
     else                                   ! processing WRF files
        iTime = iTime + 1
        if (iTime > n_times) goto 200       ! finished with all the timestamps
        if (pdebug) print*,"Considering ",Times(iTime), iTime
        call TimeStamp2ymdh(times(iTime),iy,im,id,ih)
        call add_hour(iy,im,id,ih,ibtz)     ! time zone shift
        call ymdh2nDatHr(iy,im,id,ih,nDatHr)! current time in YYDDDHH LST format
        if (nDatHr > LastOut)   goto 999    ! done, exit
        if (nDatHr < iGetNext)  goto 100    ! skip too-early time-stamps
        if (pdebug) print*,"Reading ",times(iTime)

        call read_wrf(cdfid,lFirst,iTime,times(iTime),(nDatHr==iGetNext),   &
             lu10,lq2,lz0,lalb,lmol,llai,lCLDFRA, nc_debug)

        if (nDatHr == iGetNext) goto 100    ! just needed to precip from Hr-1
        if (nDatHr <= iLastHr)  goto 100    ! already wrote this hour, get more
     endif ! MetForm == 'MM5' vs. 'WRF'
!
!-----Option to force MMIF to re-diagnose u10,T2, and q2
!
     if (lsfc_recalc) then
        lmol = .false.
        lu10 = .false.
        lt2  = .false.
        lq2  = .false.
     end if
!
!-----If this is the first time we've read some MM5/WRF data, and the user
!     didn't specify the zface values (either chose aggregation, or chose
!     interpolation but gave "avg" for the zface levels) then calculate them.
!
     if (zface(1) == -999.) call avg_zface(kzin)
!
!-----For multiple AER* outputs when using aggregation, we need the mid-zh 
!     of the MM5/WRF levels at each output point.  Store this in zPt(iPt,nz).
!     If using interpolation, this is the same as zmid.  If using aggregation,
!     zmid() is the AVERAGE zmid over the larger 3-D output domain.
!
     if (.not. allocated(zPt)) then
        allocate( zPt(NumOuts,0:nzOut) )
        if (iVertMap == 0) then        ! aggregation, not interpolation
           do i = 1, NumOuts
              if (ijPt(i) /= 0) then   ! ijPt(i) == 0 means non-point output
                 zPt(i,0) = 0.         ! the ground, 0m AGL
                 zPt(i,1) = zh(iPt(i),jPt(i),kzin(1))/2.
                 do k = 2, nzOut       ! calc mid-point of each OUTPUT layer
                    zPt(i,k) = (zh(iPt(i),jPt(i),kzin(k-1)) + &
                         zh(iPt(i),jPt(i),kzin(k)))/2.
                 end do
                 do k = 1, nzOut       ! round to 1 cm
                    zPt(i,k) = 0.01 * nint(100*zPt(i,k))
                 end do
              end if
           end do
        else                           ! interpolation, not aggregation
           do i = 1, NumOuts
              zPt(i,0) = 0.            ! the ground, 0m AGL
              do k = 1, nzOut          ! calc mid-point of each OUTPUT layer
                 zPt(i,k) = zmid(k)    ! the same as zmid for all points
              end do
           end do
        end if
!         do i = 1, NumOuts
!            do k = nzPt(i,1), nzPt(i,2) ! only output level above 15m
!               if (zmid(k) < 15. .and. ijPt(i) /= 0) then
!                  nzPt(i,1) = k + 1     ! skip the lowest layers
!               end if
!            end do
!            nzPt(i,2) = nzPt(i,2) + nzPt(i,1) - 1    ! conserve number of layers
!            if (nzPt(i,2) > nzOut) nzPt(i,2) = nzOut ! not more more than we have
!         end do
     end if
!
!----Sanity check for projections that CALMET and WRF/MM5 have in common.
!    Note the use of any() that matching partial strings CALPUFF and CALPUFFv6.
!
     if (any(index(OutForm,"CALPUFF") > 0) .and. pmap == "OTHER") then
        write(*,'(2a)') "*** Error: WRF files use a projection ", &
             " which is not supported by CALMET."
        write(*,*) "    CALMET and WRF have these projections in common:"
        write(*,*) "      1 = Lambert Conformal Conic (LCC)"
        write(*,*) "      2 = Polar Stereographic (PS)"
        write(*,*) "      3 = Equatorial Mercator (EM)"
        write(*,*) "    Program stopping!"  
        stop
     endif
!
!-----Sanity check for AERCOARE output: only makes sense over water
!
     do iOut = 1, NumOuts
        if ((OutForm(iOut) == "AERCOARE") .and. &
             (iBegDat == nDatHr .or. iEndDat == nDatHr)) then
           if (.not. iswater(iPt(iOut),jPt(iOut))) then
              call nDatHr2ymdh(ndathr,iy,im,id,ih,23)
              write(*,'(a,i4.4,3i2.2,a,i5,a,i3)') " *** WARNING: ",iy,im,id,ih, &
                   ": landuse at AERCOARE output #",iOut,                &
                   " not water, but code",ilu(iPt(iOut),jPt(iOut))
              warnings = .true.
           end if
        end if
     end do
!
!-----Sanity check for SCICHEM, H (aka D, aka SZ(kmax)) = zface(nzOut) should
!     be greater than 2 times the maximum elevation.
!
     if (any(index(OutForm,"SCICHEM") > 0) .and. &
          (zmid(nzOut) < 2*maxval(topo(iBeg:iEnd,jBeg:jEnd))) .and. &
          .not. scichem_warned) then
        write(*,*) 
        write(*,*) "*** Error: For SCICHEM output, you must set the highest extracted layer"
        write(*,*) "    mid-point to be at least 2 times the maximum terrain elevation."
        write(*,*) "      Highest layer center (m): ",zmid(nzOut)
        write(*,*) "      Maximum terrain elev (m): ",maxval(topo(iBeg:iEnd,jBeg:jEnd))
        if (force_run) then
           write(*,*) "    Proceeding anyway, at your peril!"
           write(*,*)
           scichem_warned = .true.
        else
           write(*,*) "    Program stopping!"  
           stop
        endif
     end if
!
!-----Default is AER_MIXHT = "WRF"
!
     aerpbl = pbl
!
!-----These are used in the loop over output grid below
!
     call nDatHr2ymdh(nDatHr, iy, im, id, ih, 23)
     call dat2jul(iy,im,id,jday) ! get julian day (day of the year)
!
!-----Detect missed/skipped hours
!
     if (iLastHr > 0) then
        call nDatHr2ymdh(iLastHr,iy1,im1,id1,ih1,23) ! iLastHr was last written
        call TimeDiff(iy1,im1,id1,ih1, iy,im,id,ih, DiffHrs)
        if (DiffHrs > 1) then
           write(*,*)             
           write(*,'(a,i4.4,3i2.2,a,i5,a,i4.4,3i2.2,a)') &
                " *** WARNING: ",iy,im,id,ih," LST is ",DiffHrs,  &
                " hours after last written: ",iy1,im1,id1,ih1," LST."
           write(*,'(a)') "              Missing WRF data files or time-stamps?"
           warnings = .true.
        end if
     end if
!
!-----Loop over output grid, calculate many of the derived fields
!
     do j = jBeg,jEnd
        do i = iBeg,iEnd

           if (pdebug) then
              print*
              print*,"I,J of point  = ",i,j
              print*,"X,Y of center = ",x0met+deltax/2. + (i-iBeg)*deltax &
                                       ,y0met+deltax/2. + (j-jBeg)*deltax
           endif
!
!-----If we didn't read z0, albedo, and LAI, calculate them.
!
           if (lucat(1:4) == "USGS") then
              call landuse_usgs(jday,ilu(i,j),ruff,alb,leaf)
              if (.not. lz0)     z0(i,j)  = ruff
              if (.not. lalb) albedo(i,j) = alb
              if (.not. llai)    lai(i,j) = leaf
           else if (lucat(1:6) == "NLCD  ") then
              call landuse_NLCD50(jday,ilu(i,j),ruff,leaf)
              if (.not. lz0)  z0(i,j)  = ruff
              if (.not. llai) lai(i,j) = leaf
              if (.not. lalb) then ! We have no source for NLCD50 Albedos.
                 ! Only recent versions of WRF (no versions of MM5) support
                 ! the NLCD 50-category land-use methodology.  These versions
                 ! are recent enough that they will always include albedo.  So
                 ! this program _should_ never get here.  Kick an error and
                 ! exit if we do.
                 write(*,*) 
                 write(*,*) "*** Error: No ALBEDO, this should never happen!"
                 write(*,*) "    Program stopping!"
                 stop
              end if
           else if (lucat(1:6) == "NLCD40") then
              call landuse_NLCD40(jday,ilu(i,j),ruff,alb,leaf)
              if (.not. lz0)  z0(i,j)     = ruff
              if (.not. lalb) albedo(i,j) = alb
              if (.not. llai) lai(i,j)    = leaf
           else if (lucat(1:5) == "MODIS") then
              call landuse_MODIS(jday,ilu(i,j),ruff,alb,leaf)
              if (.not. lz0)  z0(i,j)     = ruff
              if (.not. lalb) albedo(i,j) = alb
              if (.not. llai) lai(i,j)    = leaf
           else if (lucat(1:25) == "MODIFIED_IGBP_MODIS_NOAH") then
              call landuse_IGBP_MODIS(jday,ilu(i,j),ruff,alb,leaf)
              if (.not. lz0)  z0(i,j)     = ruff
              if (.not. lalb) albedo(i,j) = alb
              if (.not. llai) lai(i,j)    = leaf
           else
              if (.not. lz0 .or. .not. lalb .or. .not. llai) then
                 write(*,*)
                 write(*,*) "*** Error: MMIF supports MMINLU = USGS, NLCD50,", &
                      "NLCD40, MODIS, and IGBP_MODIS, but this file used ",    &
                      trim(lucat),","
                 write(*,*) "    and yet does not include values for ",        &
                      "one of (z0, Albedo, LAI)."
                 if (force_run) then
                    write(*,*) "    Proceeding anyway, at your peril!"
                    write(*,*)
                 else
                    write(*,*) "    Program stopping!"  
                    stop
                 endif
              end if
           end if  ! if (lucat(1:4) == "USGS") then
!
!-----Calculate the Bowen ratio -- never use the USGS look-up tables.
!     Follow R. Brode's Bowen limits in mm5aermod.
!
           if (lhflux(i,j) == 0.) then
              if (shflux(i,j) < 0.) then
                 bowen(i,j) = -1.
              elseif (shflux(i,j) == 0.) then
                 bowen(i,j) = 0.
              elseif (shflux(i,j) > 0.) then
                 bowen(i,j) = 10.
              endif
           else
              bowen(i,j) = shflux(i,j) / lhflux(i,j) ! Bowen ratio
              if (shflux(i,j) < 0. .and. lhflux(i,j) > 0.)   bowen(i,j) = -1.
              if (shflux(i,j) > 0. .and. lhflux(i,j) < 0.)   bowen(i,j) =  1.
              if (bowen(i,j)  > 0. .and. bowen(i,j)  < 0.01) bowen(i,j) = 0.01
              if (bowen(i,j)  > 10.) bowen(i,j) =  10.
           endif

           if (pdebug) then
              print*,"shflux  = ",shflux(i,j)
              print*,"lhflux  = ",lhflux(i,j)
              print*,"bowen   = ",bowen(i,j)
           end if
!
!-----We'll need these for the next two subroutines
!
           do k = 1,nz
              zm(k) = (zh(i,j,k) + zh(i,j,k-1))/2. ! midpoint of INPUT layers
              ulev(k) = uu(i,j,k)
              vlev(k) = vv(i,j,k)
              tlev(k) = tt(i,j,k)
              plev(k) = pa(i,j,k)
              qlev(k) = qq(i,j,k)
           end do
!
!-----Calculate any missing surface layer parameters
!
           if (pdebug) then
              print*,"t2      = ",t2(i,j)
              print*,"q2      = ",q2(i,j)
           end if

           call sfc_layer(iswater(i,j),mol(i,j),ustar(i,j),z0(i,j),tsfc(i,j), &
                smois(i,j), psfc(i,j),shflux(i,j),lhflux(i,j),                &
                zm(1),uu(i,j,1),vv(i,j,1),tt(i,j,1),qq(i,j,1),pa(i,j,1),      &
                lu10,lt2,lq2,lmol, u10(i,j),v10(i,j),t10(i,j),t2(i,j),q2(i,j))

           if (pdebug) then
              print*,"ustar   = ",ustar(i,j)
              print*,"z0      = ",z0(i,j)
              print*,"mol     = ",mol(i,j)
              print*,"u10     = ",u10(i,j)
              print*,"v10     = ",v10(i,j)
              print*,"z(lev1) = ",zm(1)
              print*,"u(lev1) = ",uu(i,j,1)
              print*,"v(lev1) = ",vv(i,j,1)
              print*,"p(lev1) = ",pa(i,j,1)
              print*,"t(lev1) = ",tt(i,j,1)
              print*,"Theta(1)= ",Theta_fn(tt(i,j,1),pa(i,j,1),qq(i,j,1))
              print*,"q(lev1) = ",qq(i,j,1)
              print*,"tsfc    = ",tsfc(i,j)
              print*,"psfc    = ",psfc(i,j)
              print*,"smois   = ",smois(i,j)
              print*,"t2      = ",t2(i,j)
              print*,"t10     = ",t10(i,j)
              print*,"q2      = ",q2(i,j)
           end if
!
!-----Re-calculate MMIF PBL height is requested
!
           
           if (CalcPBL == "MMIF") then     ! CalcPBL is CALSCI_MIXHT
!              print*,"calling pbl_height cuz CalcPBL = MMIF",i,j
              call pbl_height(nz,zm,ulev,vlev,tlev,qlev,plev,                 &
                   u10(i,j),v10(i,j),t2(i,j),q2(i,j),psfc(i,j),ustar(i,j),    & 
                   iswater(i,j), pbl(i,j), pbl_last(i,j))
           end if
           if (aer_mixht == "MMIF") then
              if (CalcPBL == "MMIF") then
!                 print*,"NOT calling pbl_height cuz CalcPBL = aer_mixht = MMIF",i,j
                 aerpbl(i,j) = pbl(i,j) ! already called pbl_height()
              else
!                 print*,"calling pbl_height cuz aer_mixht = MMIF",i,j
                 call pbl_height(nz,zm,ulev,vlev,tlev,qlev,plev,              &
                      u10(i,j),v10(i,j),t2(i,j),q2(i,j),psfc(i,j),ustar(i,j), & 
                      iswater(i,j), aerpbl(i,j), pbl_last(i,j))
              end if
           end if

           if (pdebug) then
              print*,"iswater = ",iswater(i,j)
              print*,"pbl     = ",pbl(i,j)
              print*,"aerpbl  = ",aerpbl(i,j)
           end if
!              
!-----Calculate Pasquill-Gifford (PG) stability class
!     
           if (PGtype == "SRDT") then       ! Solar Raditation Delta Temperature
              call pg_srdt(qsw(i,j),t2(i,j),tt(i,j,1),u10(i,j),v10(i,j),   &
                   ipgt(i,j))
           elseif (PGtype == "GOLDER") then ! GOLDER Nomogram
              call pg_golder(mol(i,j),z0(i,j),ipgt(i,j))
           endif
           if (pdebug) print*,"ipgt    = ",ipgt(i,j)

        enddo ! End I-loop
     enddo    ! End J-loop
!
!-----Derive the fractional cloud cover for each point (used by AER*, SCICHEM)
!
     if (CloudCover == "WRF") then
        if (.not. lCLDFRA) then
           write(*,*) "This WRF file does not contain the field CLDFRA, and so"
           write(*,*) "the keyword combination 'CLOUDCOVER WRF' is not valid."
           write(*,*) "Change to using ANGEVINE or RANDALL."
           write(*,*) "Stopping."
           stop
        endif
        call collapse_cloud_cover
     elseif (CloudCover == "ANGEVINE") then
        call cloud_cover_COAMPS    ! default in MMIF >= 2.2
     elseif (CloudCover == "RANDALL") then
        call cloud_cover_mm5aermod ! default in MMIF < 2.2 and before
     end if
!
!-----Convert from pressure (sigma) to height as a vertical coordinate
!
     if (iVertMap == 0) then
        call aggregate(kz1,kz2)      ! can't be SCICHEM, we checked above
     else
        call interpolate(.false.)    ! interp to Zface
     endif
!
!-----Open and write output file header, if it's the first output time
!
     call nDatHr2iDate10(nDatHr,iLST) ! for writing timestamp to the screen
     call idate2ymdh(iLST,iy,im,id,ih)
     call add_hour(iy,im,id,ih,-ibtz) ! back to UTC
     iUTC = iymdh2idate(iy,im,id,ih)  ! for writing timestamp to the screen

     if (wrote_header == 0) then      ! flag for first output

        if (any(index(OutForm,"CALPUFF") > 0)) call calmet_layers(nzFinal)
        if (any(index(OutForm,"SCICHEM") > 0)) call scichem_layers
        
        do iOut = 1, NumOuts

           if (pdebug) print*,"Output unit ",iOutUnit(iOut)," to file ", &
                trim(OutFile(iOut))

           if (PtOver(iOut) == 0) then ! set the auto-detection of land/water
              if ( iPt(iOut) > 0 .and. jPt(iOut) > 0 ) then
                 if ( iswater(iPt(iOut),jPt(iOut)) ) then
                    PtOver(iOut) = 2      ! over water (OW)
                 else
                    PtOver(iOut) = 1      ! over land (OL)
                 end if
              endif
           endif

           if (PtOver(iOut) == 1) then ! over land
              if (aer_use_TSKC) then
                 aer_use_BULKRN(iOut) = .false. ! used internally only
              else
                 aer_use_BULKRN(iOUt) = .true. 
              endif
           else                        ! over water
              aer_use_BULKRN(iOut) = .false. 
           end if

           if (OutForm(iOut) == "QAPLOT") then

              write(*,'(a,i6,3a)') ' Output #',iOut,    &
                   ' QAPLOT ',trim(OutType(iOut)),' file'
              call qa_plots(iOutUnit(iOut),OutFile(iOut),OutType(iOut))

              
           elseif (OutForm(iOut) == "CALPUFF") then

!      Find which DATA unit/filename goes with this point
                 
              do i = iOut,NumOuts       ! search forward in other outs
                 if (i /= iOut .and.  & ! don't match yourself
                      OutForm(i) == "CALPUFF" .and. &
                      OutType(i) == "CALMET"  .and. &
                      related_out(iOut,1) == 0) related_out(iOut,1) = i
              end do

              if (OutType(iOut) == "USEFUL") then
                 write(*,'(a,i6,a,i12)') ' Output #',iOut,    &
                      ' CALMET USEFUL INFO file'
                 call calmet_useful(iOutUnit(iOut),OutFile(iOut),  &
                      Outfile(related_out(iOut,1)),nzFinal)

              else if (OutType(iOut) == "CALMET") then

                 write(*,12) ' Output #',iOut,    &
                      ' CALMET     header   at',iLST,iUTC
                 open(iOutUnit(iOut),file=OutFile(iOut),  &
                      form='unformatted',status='unknown')
                 call calmet_header(iOutUnit(iOut),.false.,   &
                      calmet_version(iOut),nzFinal)

              else if (OutType(iOut) == "TERRAIN") then

                 write(*,12) ' Output #',iOut,    &
                      ' CALMET TERRAIN file'
                 call calmet_terrain(iOutUnit(iOut),Outfile(iOut))

              end if
              
           elseif (OutForm(iOut) == "CALPUFFV6") then

!      Find which DATA unit/filename goes with this point
                 
              do i = iOut,NumOuts       ! search forward in other outs
                 if (i /= iOut .and.  & ! don't match self
                      OutForm(i) == "CALPUFFV6" .and. &
                      OutType(i) == "CALMET"    .and. &
                      related_out(iOut,1) == 0) related_out(iOut,1) = i
              end do
              
              if (OutType(iOut) == "USEFUL") then

                 write(*,12) ' Output #',iOut,    &
                      ' CALMET USEFUL INFO file'
                 call calmet_useful(iOutUnit(iOut),OutFile(iOut), &
                      Outfile(related_out(iOut,1)),nzFinal)

              else if (OutType(iOut) == "CALMET") then

                 write(*,12) ' Output #',iOut,    &
                      ' CALMETv6   header   at',iLST,iUTC
                 open(iOutUnit(iOut),file=OutFile(iOut),  &
                      form='unformatted',status='unknown')
                 call calmet_header(iOutUnit(iOut),.false.,   &
                      calmet_version(iOut),nzFinal)

              else if (OutType(iOut) == "TERRAIN") then

                 write(*,12) ' Output #',iOut,    &
                      ' CALMET TERRAIN file'
                 call calmet_terrain(iOutUnit(iOut),Outfile(iOut))

              else if (OutType(iOut) == "AUX") then

                 write(*,12) ' Output #',iOut,    &
                      ' CALMET.AUX header   at',iLST,iUTC
                 open(iOutUnit(iOut),file=OutFile(iOut),  &
                      form='unformatted',status='unknown')
                 call calmet_header(iOutUnit(iOut),.true.,   &
                      calmet_version(iOut),nzFinal)

              end if

           elseif (OutForm(iOut) == 'SCICHEM') then

              if (OutType(iOut) == "USEFUL") then

!      Find which DATA unit/filename goes with this point
                 
                 do i = iOut,NumOuts       ! search forward in other outs
                    if (i /= iOut .and.  & ! don't match self
                         OutForm(i)  == "SCICHEM" .and. &
                         (OutType(i) == "BINARY" .or.  &
                         OutType(i)  == "ASCII") .and. &
                         related_out(iOut,1) == 0) related_out(iOut,1) = i
                 end do

                 write(*,12) ' Output #',iOut,   &
                      ' SCICHEM USEFUL INFO file'
                 call scichem_useful(iOutUnit(iOut),Outfile(iOut), &
                      Outfile(related_out(iOut,1)))

              else if (OutType(iOut) == "TERRAIN") then

                 write(*,12) ' Output #',iOut,   &
                      ' SCICHEM TERRAIN file'
                 call scichem_terrain(iOutUnit(iOut),Outfile(iOut))

              else if (OutType(iOut) == "SAMPLER") then

                 write(*,12) ' Output #',iOut,   &
                      ' SCICHEM SAMLPER file'
                 call scichem_sampler(iOutUnit(iOut),Outfile(iOut))

              else if (OutType(iOut) == "BINARY") then

                 write(*,12) ' Output #',iOut,    &
                      ' MEDOC (BIN) file    at',iLST,iUTC
                 open(iOutUnit(iOut),file=Outfile(iOut), &
                      form='unformatted',status='unknown')
                 call medoc_header(iOutUnit(iOut),.true.)

              else if (OutType(iOut) == "ASCII") then

                 write(*,12) ' Output #',iOut,    &
                      ' MEDOC (ASCII) file  at',iLST,iUTC
                 open(iOutUnit(iOut),file=Outfile(iOut),status='unknown')
                 call medoc_header(iOutUnit(iOut),.false.)

              end if
               
           elseif (OutForm(iOut) == 'AERMET') then

              if (OutType(iOut) == "CSH" .or. OutType(iOut) == "BAT") then

!      Find which ONSITE, FSL, and AERSFC filenames go with this point

                 do i = 1,NumOuts       ! search in all other outs
                    if (i /= iOut) then ! don't match yourself

                       if (iPt(iOut) == iPt(i) .and. jPt(iOut) == jPt(i) .and. &
                            OutType(i) == "ONSITE" .and. &
                            related_out(iOut,1) == 0) related_out(iOut,1) = i

                       if (iPt(iOut) == iPt(i) .and. jPt(iOut) == jPt(i) .and. &
                            OutType(i) == "FSL"    .and. &
                            related_out(iOut,2) == 0) related_out(iOut,2) = i

                       if (iPt(iOut) == iPt(i) .and. jPt(iOut) == jPt(i) .and. &
                            OutType(i) == "AERSFC" .and. &
                            related_out(iOut,3) == 0) related_out(iOut,3) = i

                    end if
                 end do

                 write(*,12) ' Output #',iOut,    &
                      ' ONSITE '//trim(OutType(iOut))//' file'
                 call onsite_batch(iOutUnit(iOut),Outfile(iOut), &
                      Outfile(related_out(iOut,1)),               &
                      Outfile(related_out(iOut,2)),               &
                      Outfile(related_out(iOut,3)),iOut)

              elseif (OutType(iOut) == "USEFUL") then

!      Find which ONSITE, FSL, and AERSFC filenames go with this point

                 do i = iOut-1,NumOuts    ! search forward in other outs
                    if (i /= iOut .and. iOut > 0) then ! don't match this output

                       if (iPt(iOut) == iPt(i) .and. jPt(iOut) == jPt(i) .and. &
                            (OutType(i) == "BAT" .or. OutType(i) == "CSH") .and. &
                            related_out(iOut,1) == 0) related_out(iOut,1) = i

                    end if
                 end do

                 basename = Outfile(related_out(iOut,1))
                 istr = len_trim(basename)            ! len of non-blank part
                 idot = index(basename,".",.true.)-1  ! last char before .ext
                 if (idot <= 0) idot = istr           ! might not be one

                 write(*,12) ' Output #',iOut,    &
                      ' ONSITE USEFUL INFO file'
                 call aermod_useful(iOutUnit(iOut),Outfile(iOut), &
                      trim(basename(1:idot)) // ".SFC", &
                      trim(basename(1:idot)) // ".PFL",iOut)

              else if (OutType(iOut) == "ONSITE") then

!      Find which AERSFC unit goes with this output

                 do i = iOut,NumOuts       ! search forward in other outs
                    if (i /= iOut .and.  & ! don't match this output
                         iPt(iOut) == iPt(i) .and. jPt(iOut) == jPt(i) .and. &
                         OutType(i) == "AERSFC" .and. &
                         related_out(iOut,1) == 0) related_out(iOut,1) = i
                 end do

                 write(*,12) ' Output #',iOut,    &
                      ' ONSITE DATA  file   at',iLST,iUTC
                 open(iOutUnit(iOut),file=Outfile(iOut),status='unknown')
                 ! no header to write for this output type

              else if (OutType(iOut) == "FSL") then

                 write(*,12) ' Output #',iOut,    &
                      ' UPPERAIR FSL file   at',iLST,iUTC
                 open(iOutUnit(iOut),file=Outfile(iOut),status='unknown')
                 ! no header to write for this output type

              end if

           elseif (OutForm(iOut) == 'AERMOD') then

              if (OutType(iOut) == "USEFUL") then

!      Find which SFC and PFL units/filenames go with this output
                 
                 do i = iOut,NumOuts       ! search forward in other outs
                    if (i /= iOut) then ! don't match this output

                       if (iPt(iOut) == iPt(i) .and. jPt(iOut) == jPt(i) .and. &
                            OutType(i) == "SFC" .and. &
                            related_out(iOut,1) == 0) related_out(iOut,1) = i

                       if (iPt(iOut) == iPt(i) .and. jPt(iOut) == jPt(i) .and. &
                            OutType(i) == "PFL" .and. &
                            related_out(iOut,2) == 0) related_out(iOut,2) = i

                    end if
                 end do

                 write(*,12) ' Output #',iOut,    &
                      ' AERMET USEFUL INFO file'
                 call aermod_useful(iOutUnit(iOut),Outfile(iOut), &
                      Outfile(related_out(iOut,1)),               &
                      Outfile(related_out(iOut,2)),iOut)

              else if (OutType(iOut) == "SFC") then

                 write(*,12) ' Output #',iOut,    &
                   ' AERMET SFC header   at',iLST,iUTC
                 open(iOutUnit(iOut),file=Outfile(iOut),status='unknown')
                 call aermod_sfc_header(iOutUnit(iOut),Outfile(iOut),iOut)

              else if (OutType(iOut) == "PFL") then

                 write(*,12) ' Output #',iOut,    &
                   ' AERMET PFL opened   at',iLST,iUTC
                 open(iOutUnit(iOut),file=Outfile(iOut),status='unknown')

              end if

           elseif (OutForm(iOut) == 'AERCOARE') then
              
              if (OutType(iOut) == "USEFUL") then

!      Find which DATA unit/filename goes with this output
                 
                 do i = iOut,NumOuts       ! search forward in other outs
                    if (i /= iOut .and.  & ! don't match this output
                         iPt(iOut) == iPt(i) .and. jPt(iOut) == jPt(i) .and. &
                         OutType(i) == "DATA" .and. &
                         related_out(iOut,1) == 0) related_out(iOut,1) = i
                 end do

                 write(*,12) ' Output #',iOut,   &
                      ' AERCOARE USEFUL INFO file'
                 call aercoare_useful(iOutUnit(iOut),Outfile(iOut), &
                      Outfile(related_out(iOut,1)),iOut)

              else if (OutType(iOut) == "DATA") then

                 write(*,12) ' Output #',iOut,    &
                      ' AERCOARE   header   at',iLST,iUTC
                 open(iOutUnit(iOut),file=Outfile(iOut),status='unknown')
                 call aercoare_header(iOutUnit(iOut))

              end if

           end if ! select output format
        end do    ! do iOut = 1, NumOuts

        wrote_header = 1                    ! no longer the 1st output time
        if (nDatHr > iBegDat) then
           write(*,*) 
           write(*,*) '*** WARNING: first date/time written is past requested'
           write(*,*) '    beginning date/time.'
           write(*,*) 
           warnings = .true.
        end if
     end if  ! if (wrote_header == 0) then
!
!-----Write this hour's output data to file
!
     write(*,'(a,i11," LST =",i11," UTC")')  &
          ' Hourly output written to all files at',iLST,iUTC

     do iOut = 1, NumOuts

        if (OutForm(iOut) == 'CALPUFF' .and. OutType(iOut) == "CALMET") then

           call calmet_hour(iOutUnit(iOut),.false.,calmet_version(iOut),nzFinal)

        elseif (OutForm(iOut) == 'CALPUFFV6' .and. OutType(iOut) == "CALMET")then

           call calmet_hour(iOutUnit(iOut),.false.,calmet_version(iOut),nzFinal)

        elseif (OutForm(iOut) == 'CALPUFFV6' .and. OutType(iOut) == "AUX") then

           call calmet_hour(iOutUnit(iOut),.true.,calmet_version(iOut),nzFinal)

        elseif (OutForm(iOut) == 'AERMET' .and. OutType(iOut) == "ONSITE") then

           call calc_vptg(iPt(iOut),jPt(iOut)) ! required for overwater
           ! related_out(iOut,1) is the iOut for AERSFC output
           call onsite_hour(iOutUnit(iOut),iOut,related_out(iOut,1))

        elseif (OutForm(iOut) == 'AERMET' .and. OutType(iOut) == "FSL") then

           call upperair_hour(iOutUnit(iOut),iUppFreq(iOut),iOut)

        elseif (OutForm(iOut) == 'AERMOD' .and. OutType(iOut) == "SFC") then

           call calc_vptg(iPt(iOut),jPt(iOut))
           call aermod_sfc_hour(iOutUnit(iOut),iOut)

        elseif (OutForm(iOut) == 'AERMOD' .and. OutType(iOut) == "PFL") then

           call aermod_pfl_hour(iOutunit(iOut),iOut)

        elseif (OutForm(iOut) == 'AERCOARE' .and. OutType(iOut) == "DATA") then

           call calc_vptg(iPt(iOut),jPt(iOut))
           call aercoare_hour(iOutUnit(iOut),iOut)

        elseif (OutForm(iOut) == 'SCICHEM') then

           call interpolate(.true.)  ! interpolate to transformed ( ZfaceS)
           if (OutType(iOut) == "ASCII") call medoc_hour(iOutUnit(iOut), .false.)
           if (OutType(iOut) == "BINARY") call medoc_hour(iOutUnit(iOut), .true.)
           call interpolate(.false.) ! interpolate back to Zface

        endif

     end do

     iLastHr = nDatHr                       ! This is the last hour written

     if (nDatHr >= LastOut) goto 999        ! done, exit
     goto 100                               ! loop to reading next hour

200  continue
     if (len_trim(METfile(iMETfile)) > 60) then
        write(*,10) 'Done  with file: ',trim(METfile(iMETfile))
     else
        write(*,11) 'Done  with file: ',trim(METfile(iMETfile))
     endif
     write(*,*) ! blank line to make the output look nice
     if (MetForm == 'MM5') then
        close(iMetUnit)
     else
        rcode = nf_close(cdfid)
        if (rcode  /=  0) then
           write(*,*) '*** Error: problem closing netcdf file', &
                trim(METfile(iMETfile))
           write(*,*) '    Program stopping.'
           stop
        end if
     end if

  end do       ! End loop over MM5/WRF files: do iMETfile = 1,NumMETfiles
!
!-----Done, clean up and exit
!
999 continue

  do iOut = 1, NumOuts
     inquire(unit=iOutUnit(iOut), opened=ok)
     if (ok) close(iOutUnit(iOut))
!
!-----If creating data for AERMET's ONSITE pathway, mimic AERSURFACE output
!
     if (OutForm(iOut) == 'AERMET' .and. OutType(iOut) == 'AERSFC')  &
          call write_aersfc(iOutUnit(iOut),Outfile(iOut),iOut)
  end do

  if (scichem_warned) then ! there was a scichem warning before, repeat it
     write(*,*) 
     write(*,*) "*** Error: For SCICHEM output, you must set the highest extracted layer"
     write(*,*) "    mid-point to be at least 2 times the maximum terrain elevation."
     write(*,*) "      Highest layer center (m): ",zmid(nzOut)
     write(*,*) "      Maximum terrain elev (m): ",maxval(topo(iBeg:iEnd,jBeg:jEnd))
  end if

99  if (iErr == 2) then
     write(*,*) 
     write(*,*) "*** Error Reading the very first MM5 file."
     write(*,*) "    Maybe it's really a a WRF file, and auto-detect failed?"
     write(*,*) 
  else if (nDatHr < LastOut) then
     write(*,*) 
     write(*,*) '*** WARNING: last date/hour found was before requested'
     write(*,*) '    ending date/hour (probably ran out of WRF/MM5 files).'
     write(*,*) 
  else if (nDatHr == LastOut) then
     write(*,*) 
     if (warnings) then
        write(*,*) 'Reached requested ending date/hour; run completed WITH WARNINGS!'
        write(*,*) '*** WARNINGS DETECTED, READ THE ABOVE OUTPUT! ***'
     else
        write(*,*) 'Reached requested ending date/hour; run completed normally.'
     end if
     write(*,*) 
  endif

  if (MetForm == "MM5") then
     call dealloc_mm5
  else
     if (allocated(times)) deallocate(times)
     call dealloc_wrf
     call flush(6)
  endif
  call dealloc_met
  if (allocated( kzin ))         deallocate( kzin )
  if (allocated( kz1  ))         deallocate( kz1  )
  if (allocated( kz2  ))         deallocate( kz2  )
  if (allocated(zfaceC))         deallocate(zfaceC)
  if (allocated(zfaceS))         deallocate(zfaceS)
  if (allocated(OutForm))        deallocate(OutForm)
  if (allocated(iOutUnit))       deallocate(iOutUnit)
  if (allocated(iOutUnit2))      deallocate(iOutUnit2)
  if (allocated(iUseful))        deallocate(iUseful)
  if (allocated(calmet_version)) deallocate(calmet_version)
  if (allocated(iUppFreq))       deallocate(iUppFreq)
  if (allocated(PtZone))         deallocate(PtZone)
  if (allocated(nzPt))           deallocate(nzPt)
  if (allocated(ijPt))           deallocate(ijPt)
  if (allocated(iPt))            deallocate(iPt)
  if (allocated(jPt))            deallocate(jPt)
  if (allocated(zPt))            deallocate(zPt)
  if (allocated(PtLat))          deallocate(PtLat)
  if (allocated(PtLon))          deallocate(PtLon)
  if (allocated(PtXlcc))         deallocate(PtXlcc)
  if (allocated(PtYlcc))         deallocate(PtYlcc)
  if (allocated(aersfc))         deallocate(aersfc)

!  stop ! Done, bye bye!

end program mmif
