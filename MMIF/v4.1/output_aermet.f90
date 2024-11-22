! Contains:
!
! subroutine aermod_useful(iUnit,fnameU,fname1,fname2,iOut)
! subroutine aermod_sfc_header(iUnit,fname,iOut)
! subroutine aermod_sfc_hour(iUnit,iOut)
! subroutine aermod_pfl_hour(iUnit,iOut)
! subroutine calc_vptg(i,j)
! subroutine sundat(flat,flon,tzone,julday,hour,elevang)
!
!******************************************************************************
!
subroutine aermod_useful(iUnit,fnameU,fname1,fname2,iOut)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     Development History:
!     2013-09-21  Original Development (ENVIRON International Corp), extracted
!                 from aremod_sfc_header().
!     2014-07-30  Don't correct iyr for time-zone shifts.
!     2016-08-14  Don't write PROFILE-related stuff when none requested.
!
!------------------------------------------------------------------------------
!
  USE met_fields
  USE functions
  implicit none
!
!-----Variable declaration
!
  character (len=*), intent(in) :: fnameU  ! filename of Useful Info file
  character (len=*), intent(in) :: fname1  ! filename of output #1, SFC
  character (len=*), intent(in) :: fname2  ! filename of output #2, PFL
  character (len=1)     :: slash,backslash ! for Linux vs. DOS paths
  integer,   intent(in) :: iUnit           ! Logical unit for output
  integer,   intent(in) :: iOut            ! location to be output
  integer,   parameter  :: UA_ID = 99999   ! upper air station
  integer,   parameter  :: SF_ID = 99999   ! surface station
  integer               :: islash,ibas     ! char positions within filename
  integer               :: i,j, iyr,imo,idy,ihr
!
!-----Format statements
!
1 format(2a)
2 format(a,i4,a)
3 format(a,i7,i5)
!
!-----Entry point
!

  i = iPt(iOut)  ! short-hand
  j = jPt(iOut) 

  call nDatHr2ymdh(nDatHr,iyr,imo,idy,ihr,24) ! AERMOD uses hours 1-24

! Find some important characters within the filename, and remove the path

  slash     = char(47)                       ! AKA "forward slash"
  backslash = char(92)

  islash = index(fname1,slash,.true.)        ! look for a path ending in "/"
  if (islash == 0) &
     islash = index(fname1,backslash,.true.) ! look for a path ending in "\"
  ibas = islash + 1                          ! islash might be zero (no path)

!
!-----Write a little block for the AERMOD.INP file
!
  open(iUnit,file=fnameU,status='unknown')
  
  write(iUnit,1) "ME STARTING"
  write(iUnit,1) "ME SURFFILE ",trim(fname1(ibas:))
  write(iUnit,3) "ME SURFDATA ",SF_ID,iyr
  if (trim(fname2) /= "none") then
     islash = index(fname2,slash,.true.)        ! look for a path ending in "/"
     if (islash == 0) &
          islash = index(fname2,backslash,.true.) ! look for a path ending in "\"
     ibas = islash + 1                          ! islash might be zero (no path)

     write(iUnit,1) "ME PROFFILE ",trim(fname2(ibas:))
     write(iUnit,2) "ME PROFBASE ",nint(topo(i,j))," METERS"
     write(iUnit,3) "ME UAIRDATA ",UA_ID,iyr
  end if
  write(iUnit,1) "ME FINISHED"

  close(iUnit)

end subroutine aermod_useful
!
!******************************************************************************
!
subroutine aermod_sfc_header(iUnit,fname,iOut)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     Writes the 1-line header to an AERMOD SFC file
!
!     Development History:
!     2011-09-30  New with MMIF v2.0
!     2012-01-31  Write out the final zmid values to screen, like calmet_header.
!     2012-03-14  Fix SFC header extra space, set OS_ID to blanks, not 99999.
!     2013-02-25  Lengthened file/path strings from 120 to 256 characters, 
!                 based on user feedback.
!     2013-02-28  Added (I,J) output point as OS_ID, and extra string with 
!                 MMIF version to the AERMET *.SFC file header line.
!     2013-05-06  Increased printing of Roughness lenght from F7.4 to F9.6.
!     2013-07-12  For aggregation, use zPt(,) instead of zmid(), because the 
!                 latter is the average over the full 3-D domain.
!     2014-05-19  Changed AERMET version to v14134.
!     2015-01-29  Added terrain elevation (topo) at the screen output for points.
!     2015-07-24  Changed AERMET version to v15181.
!
!------------------------------------------------------------------------------
!
  USE met_fields
  USE functions
  implicit none
!
!-----Variable declaration
!
  character (len=*), intent(in) :: fname  ! filename of output #1, SFC
  integer,   intent(in) :: iUnit          !  Logical unit for output
  integer,   intent(in) :: iOut           ! location to be output
  integer               :: n10            ! 0 or 1: number of 10m levels to use
  real                  :: ulat, ulon     ! temporary location
  character             :: EW*1, NS*1     ! E or W, N or S
  integer,   parameter  :: UA_ID = 99999  ! upper air station
  integer,   parameter  :: SF_ID = 99999  ! surface station
  integer,   parameter  :: aermet_version = 15181
  integer               :: i,j,k          ! local
!
!-----Format statements
!
9 format(2(f9.3,a1),8x,2(a9,i8),a9,2i4.4,a13,i6,a)
!
!-----Entry point
!
  i = iPt(iOut)  ! short-hand
  j = jPt(iOut) 

  if ((zPt(iOut,nzPt(iOut,1)) > 13. .and. zPt(iOut,nzPt(iOut,2)) > 13.) .or. &
       nzPt(iOut,2) == 0) then
     n10 = 1 ! do include the 10m level
  else
     n10 = 0 ! don't include 10m level, lowest MM5/WRF level is already < 13m
  endif

! write the final levels being written, to the screen

  write(*,'(a,i6,2a)') " Output #",iOut," MET-to-AERMET layers for ", &
       trim(fname)
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

  write(*,'(a,i6,a)') " For AERMET output #",iOut,", at initial output time:"
  write(*,'(a,f9.4)') "        Elevation (m): ",topo(i,j)
  write(*,'(a,i9)')   "         Landuse code: ",ilu(i,j)
  write(*,'(a,f9.6)') " Roughness length (m): ",z0(i,j)
  write(*,'(a,f9.4)') "          Bowen ratio: ",bowen(i,j)
  write(*,'(a,f9.4)') "     Noon-time Albedo: ",albedo(i,j)
  write(*,*)

  if (xlon(i,j) >= 0.) then ! AERMET format requires positive values
     EW = "E"                     ! with a character E or W
     ulon = xlon(i,j)
  else
     EW = "W"
     ulon = -xlon(i,j)
  endif
  if (ylat(i,j) >= 0.) then ! AERMET format requires positive values
     NS = "N"                     ! with a character N or S
     ulat = ylat(i,j)
  else
     NS = "S"
     ulat = -ylat(i,j)
  endif

  write(iUnit,9) ulat,NS,ulon,EW,     &
       "  UA_ID: ",UA_ID,             &
       "  SF_ID: ",SF_ID,             &
       "  OS_ID: ",i,j,         &
       "  VERSION:",aermet_version,   &
       "  MMIF VERSION 4.1 2023-10-30"

  return
end subroutine aermod_sfc_header
!
!******************************************************************************
!
subroutine aermod_sfc_hour(iUnit,iOut)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     Writes an hour of surface data in AERMET output (SFC) file format
!
!     Development History:
!     2011-09-30  New with MMIF v2.0
!     2012-02-21  Bug fix: wind direction changed to MET convention.
!     2013-02-06  Moved checks/limits on VPTG from calc_vptg() to this
!                 routine, so aercoare_hour() is free of these limits.
!     2013-03-15  Increase sig figs for roughness length (z0) to F9.6 format,
!     2013-03-18  Set Mixing Height > 4000m to -999. (bad/missing values flag).
!     2013-05-01  Reduce number of AERMOD warnings for stable conditions.
!     2013-05-01  Implement AERMOD minimum wind speed, following AERMET v12345.
!     2013-05-02  Added support for multiple output points.
!     2013-07-12  For aggregation, use zPt(,) instead of zmid(), because the 
!                 latter is the average over the full 3-D domain.
!     2014-03-06  New version of uv2sd to support PS/EM projections.
!     2014-06-05  Added new WSADJ flag, after discussions with EPA's Roger Brode.
!     2014-07-30  Prevent output before requested 1st timestamp in this timezone.
!     2014-09-18  Add optional minimum mixing height and abs(L) in AERMOD modes.
!     2015-03-31  Change AER_MIN_SPEED to be calm, not set to min. speed.
!     2016-10-10  Add flush() calls for easier re-starts
!------------------------------------------------------------------------------
!
  USE met_fields
  USE functions
  USE parse_control, ONLY : iBegDat, iEndDat
  implicit none
!
!-----Variable declaration
!
  integer, intent(in) :: iUnit            ! Logical unit for output
  integer, intent(in) :: iOut             ! location to be output
  integer             :: iyr,iyr2, imo,idy,jday,ihr, ThisHour  ! time stamps
  integer             :: i,j
  integer             :: n10              ! 0 or 1: include 10m level?
  real                :: anem             ! ht of wind speed used based on n10
  real                :: rh, speed, dir, L, SunAngle, Alb, B1
  real                :: ZiMech, ZiConv   ! Mechanical & Convective mixing hgts
  real                :: shfluxOut, ustarOut, wstarOut, VPTGout
  character           :: WSADJ*12
  integer             :: ipcode
  real, parameter     :: SBLMAX=4000.0, CBLMAX=4000.0 ! from AERMET's MP2.INC
!
!-----Format statement the same as in mpout.for from AERMET v14134, except
!     for z0 changed from F7.4 to F9.6 to allow for smaller z0 values.
!
!           yr mo dy jday     hr     shflux   ustar    wstar
1800 FORMAT( 3(I2,1X), I3,1X, I2,1X, F6.1,1X, F6.3,1X, F6.3,1X,           &
!                VPTG     ZiConv ZiMech
                 F6.3,1X, 2(F5.0,1X),                                     &
!                mol      z0       bowen    albedo   speed    dir
                 F8.1,1X, F9.6,1X, F6.2,1X, F6.2,1X, F7.2,1X, F6.1,       &
!              Zwind,T,Ztemp ipcode    rain  rh   press      cloud   WSADJ
                 3(1X,F6.1), 1X,I5, 1X,F6.2, 2(1X, F6.0), 1X, I5, 1X, A12)
!
!-----Entry point
!
  i = iPt(iOut)  ! short-hand
  j = jPt(iOut) 

  call nDatHr2ymdh(nDatHr,iyr,imo,idy,ihr,24) ! AERMOD uses hours 1-24
  ihr = ihr - ibtz + PtZone(iOut)             ! Back to GMT, then to LST for iOut
  call legal_timestamp(iyr,imo,idy,ihr,24)    ! Back to 1-24, LST for this iOut

  call ymdh2nDatHr(iyr,imo,idy,ihr,ThisHour)
  if (ThisHour < iBegDat) return              ! before 1st output in this zone
  if (ThisHour > iEndDat) return              ! after last output in this zone

  call dat2jul(iyr,imo,idy,jday)              ! get julian day
  iyr2 = iyr - 2000                           ! convert to 2-digit year
  if (iyr2 < 0) iyr2 = iyr - 1900             ! should work for the 1900s

  if ((zPt(iOut,nzPt(iOut,1)) > 13. .and. zPt(iOut,nzPt(iOut,2)) > 13.) .or. &
       nzPt(iOut,2) == 0) then
     n10 = 1 ! do include the 10m level
  else
     n10 = 0 ! don't include 10m level, lowest MM5/WRF level is already < 13m
  endif

  WSADJ = 'MIFF-Mod    '   ! Per discussions with EPA's Roger Brode.

  if (rain(i,j) > 0.) then ! See aermet_userguide_addendum_v11059_draft.pdf
     if (tOut(i,j,1) < 273.15) then ! page C-13
        ipcode = 22        ! frozen
     else
        ipcode = 11        ! liquid
     endif
  else
     ipcode = 0            ! none
  endif
!
!-----Define temporary output variables, so we can over-ride them later
!
  shfluxOut = shflux(i,j)
  ustarOut  = ustar(i,j)
  L         = mol(i,j)
  VPTGout   = VPTG
  ZiConv    = aerpbl(i,j)                   ! Convective mixing height
  ZiMech    = ZiConv                        ! Mechanical mixing height
!
!-----Calculate relative humidity, q's are in kg/kg, file format requires nint()
!
  rh = nint(min(100.,q2(i,j)/qs_fn(t2(i,j),psfc(i,j))*100.))
!
!-----Follow Roger Brode's changes to mm5aermod to limit Monin-Obukhov length
!
  if (shflux(i,j) == 0.) L = 8888.0 ! fits in f8.1
  if (L < 0) then
     if (L < -8888.) L = -8888.     ! fits in f8.1
     if (L > -aer_min_Obuk)    L = -aer_min_Obuk
  else ! L > 0
     if (L >  8888.) L =  8888.
     if (L <  aer_min_Obuk)    L =  aer_min_Obuk
  endif
!
!-----Limit Wstar to be < 99.999, or we'll get ****** in the SFC file (f6.3)
!
  wstarOut = min(wstar(i,j), 99.999) ! not more positive than fits
  wstarOut = max(wstar(i,j), -9.000) ! not more negative than bad value flag
!
!-----AERMET writes not the noon-time albedo, but the albedo corrected for
!     solar angle.  Copy the code from AERMET v11059.
!
  call sundat(ylat(i,j),-xlon(i,j),-PtZone(iOut),jday,ihr,SunAngle)
  if (SunAngle <= 0.) then ! set night-time albedo to 1.0
     Alb = 1.
  else
     B1 = 1. - albedo(i,j)
     Alb = albedo(i,j) + B1*EXP(-0.1*SunAngle - 0.5*B1**2) ! Eq. 3
  endif
!
!-----Limit the range of VPTG to between 0.005 and 0.1.
!
  if (VPTGout < 0.005) VPTGout = 0.005
  if (VPTGout > 0.1)   VPTGout = 0.1
!
!-----Rotate the wind direction from the projection to true north
!
  if (n10 > 0) then
     call uv2sd(u10(i,j),v10(i,j),cosalpha(i,j),sinalpha(i,j), &
          .true.,speed,dir)
     anem = 10.
  else
     call uv2sd(uOut(i,j,1),vOut(i,j,1),cosalpha(i,j),sinalpha(i,j), &
          .true.,speed,dir)
     anem = zPt(iOut,1)
  end if
!
!-----Limit ZiConv and ZiMech to 4000m, following AERMET (MPPBL.FOR).
!     Note that WRF/MM5/MMIF only produce one estimate for mixing height, 
!     so we use aerpbl(:,:) for both Convective and Mechanical mixing height.
!
  if ((ZiConv > CBLMAX) .and. (L < 0.)) &
       write(*,'(a,i4.4,3i2.2,a,g10.2,a,i5,a)') "*** WARNING: ",         &
       iyr,imo,idy,ihr,": PBL Ht was ",ZiConv,"m, which exceeds ", &
       nint(CBLMAX),"m."
  if ((ZiMech > SBLMAX) .and. (L > 0.)) &
       write(*,'(a,i4.4,3i2.2,a,g10.2,a,i5,a)') "*** WARNING: ",         &
       iyr,imo,idy,ihr,": PBL Ht was ",ZiMech,"m, which exceeds ", &
       nint(SBLMAX),"m."

  call pbl_limits(AER_min_MixHt,SBLMAX, L,ustarOut,wstarOut, ZiMech)
  call pbl_limits(AER_min_MixHt,CBLMAX, L,ustarOut,wstarOut, ZiConv)
!
!-----To minimize the number of annoying warnings printed by AERMOD, set a
!     few parameters to missing when stable (L > 0).
!
  if (L > 0.) then    ! stable conditions when Monin-Obukhov length is positive
     ZiConv = -999.   ! print -999.  in SFC file
     VPTGout = -9.    ! print -9.000 in SFC file
     wstarOut = -9.   ! print -9.000 in SFC file
  endif
!
!-----Prevent MMIF from writing wind speeds that are small, causing large
!     concentrations in AERMOD.  Use AERMET v12345's value of 0.5.  See:
!       1. Section 2.3.2 of the AERMET User Guide about THRESH_1MIN.
!       2. 20130308_Met_Data_Clarification.pdf available on SCRAM, page 12.
!
  if (speed < aer_min_speed) then ! minimum wind speed, aka calm threshold
     shfluxOut = -999.
     ustarOut  = -9.000
     wstarOut  = -9.000
     ZiConv    = -999.
     ZiMech    = -999.
     L         = -99999.
     speed     = 0.
     dir       = 0.
  endif
!
!-----OUTPUT: write a line for this hour
!
  write(iUnit,1800) iyr2,imo,idy,jday,ihr,shfluxOut,ustarOut,wstarOut,  &
       VPTGout,ZiConv,ZiMech,L,z0(i,j),bowen(i,j),Alb,        &
       speed,dir,anem, t2(i,j),2., ipcode,rain(i,j), rh,psfc(i,j), &
       nint(10.*cldcvr(i,j)),WSADJ

  call flush(iUnit)                        ! make for easier re-starts

  return
end subroutine aermod_sfc_hour
!
!******************************************************************************
!
subroutine aermod_pfl_hour(iUnit,iOut)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     Writes an hour of upper-air data in AERMET output (PFL) file format
!
!     Development History:
!     2011-09-30  New with MMIF v2.0
!     2012-02-21  Bug fix: wind direction changed to MET convention.
!     2013-05-01  Implement minimum AERMOD wind speed, following AERMET v12345.
!     2013-05-02  Added support for multiple output points.
!     2013-07-12  For aggregation, use zPt(,) instead of zmid(), because the 
!                 latter is the average over the full 3-D domain.
!     2014-03-06  New version of uv2sd to support PS/EM projections.
!     2014-07-30  Prevent output before requested 1st timestamp in this timezone.
!     2016-10-10  Add flush() calls for easier re-starts
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
  integer, intent(in) :: iUnit            ! Logical unit for output
  integer, intent(in) :: iOut             ! location to be output
  integer             :: iyr,iyr2,imo,idy,ihr,ThisHour ! time stamps
  integer             :: i,j,k            ! local indexes
  integer             :: n10              ! 0 or 1: include 10m level?
  real                :: speed, dir       ! used to convert U,V 
  real, parameter :: SigTheta = 99, SigW = 99 ! neither available in MM5/WRF
!
!-----Format statement copied from mpout.for from AERMET v11059
!
!        yr mo dy hr  height   last   dir      speed
1990 FORMAT(4(I2,1X), F7.1,1X, I1,1X, F7.1,1X, F8.2,1X, &
!         temp    SigTheta  SigW
          F8.2,1X, F8.2,1X, F8.2)
!
!-----Entry point
!
  i = iPt(iOut)  ! short-hand
  j = jPt(iOut) 

  call nDatHr2ymdh(nDatHr,iyr,imo,idy,ihr,24) ! AERMOD uses hours 1-24
  ihr = ihr - ibtz + PtZone(iOut)             ! Back to GMT, then to LST for iOut
  call legal_timestamp(iyr,imo,idy,ihr,24)    ! Back to 1-24, LST for this iOut

  call ymdh2nDatHr(iyr,imo,idy,ihr,ThisHour)
  if (ThisHour < iBegDat) return              ! before 1st output in this zone
  if (ThisHour > iEndDat) return              ! after last output in this zone

  iyr2 = iyr - 2000                           ! convert to 2-digit year
  if (iyr2 < 0) iyr2 = iyr - 1900             ! should work for the 1900s

  if ((zPt(iOut,nzPt(iOut,1)) > 13. .and. zPt(iOut,nzPt(iOut,2)) > 13.) .or. &
       nzPt(iOut,2) == 0) then
     n10 = 1 ! do include the 10m level
  else
     n10 = 0 ! don't include 10m level, lowest MM5/WRF level is already < 13m
  endif
!
!-----Write 2m level (temperature OK, but missing winds)
!
  write(iUnit,1990) iyr2,imo,idy,ihr, 2., 0, 999.,999.,&
       t2(i,j)-273.15,SigTheta,SigW
!
!-----Possibly write 10m level (temperature and winds)
!
  if (nzPt(iOut,2) == 0) then
     k = 1 ! no more layers after the 10m level
  else
     k = 0 ! more layers coming
  endif
  if (n10 > 0) then 
     call uv2sd(u10(i,j),v10(i,j),cosalpha(i,j),sinalpha(i,j), &
          .true.,speed,dir)
     write(iUnit,1990) iyr2,imo,idy,ihr, 10., k, dir,speed,999.,SigTheta,SigW
  end if
!
!-----Write all but the top level, with last = 0
!
  if (nzPt(iOut,1) <= nzPt(iOut,2)-1) then ! if more than 1 level to be written
     do k = nzPt(iOut,1), nzPt(iOut,2)-1
        call uv2sd(uOut(i,j,k),vOut(i,j,k),cosalpha(i,j),sinalpha(i,j), &
             .true., speed,dir)
        if (speed < aer_min_speed) then ! minimum wind speed, aka threshold
           speed = 999.
           dir   = 999.
        endif
        write(iUnit,1990) iyr2,imo,idy,ihr,zPt(iOut,k), 0, dir,speed,&
             tOut(i,j,k)-273.15,SigTheta,SigW
     end do
  end if
!
!-----Write the top level, with last = 1
!
  if (nzPt(iOut,2) > 0) then
     k = nzPt(iOut,2)
     call uv2sd(uOut(i,j,k),vOut(i,j,k),cosalpha(i,j),sinalpha(i,j), &
          .true., speed,dir)
     if (speed < aer_min_speed) then ! minimum wind speed, aka threshold
        speed = 999.
        dir   = 999.
     endif
     write(iUnit,1990) iyr2,imo,idy,ihr,zPt(iOut,k), 1, dir,speed,&
          tOut(i,j,k)-273.15,SigTheta,SigW
  end if

  call flush(iUnit)                        ! make for easier re-starts

  return
end subroutine aermod_pfl_hour
!
!******************************************************************************
!
subroutine calc_vptg(i,j)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     Calculate the vertical potential temperature gradient above the top of
!     the inversion layer (above AERPBL(i,j)+500m).  Use MM5/WRF fields!
!
!     Note that this routine must be called before aggregate(), which changes
!     the values of tt, pa, and qq.
!
!     Development History:
!     2011-09-30  New with MMIF v2.0, adapted/simplified from mm5aermod
!     2013-02-06  Moved check at end to aermod_sfc_hour().
!     2013-05-02  Added support for multiple output points.
!
!------------------------------------------------------------------------------
!
  USE met_fields
  USE functions
  implicit none
!
!-----Variable declaration
!
  integer, intent(in) :: i,j       ! location to be output
  integer             :: k,kB,kT   ! kBottom and kTop, the layer to find dTh/dz
  real, parameter     :: DZ = 200. ! meters, how far above PBL height to look

  do k = 1,nz
     zm(k) = (zh(i,j,k) + zh(i,j,k-1))/2. ! midpoint of input layers
  end do

  kB = 1
  do while (zm(kB) < aerpbl(i,j) .and. kB < nz)
     kB = kB + 1
  end do
  if (kB > 1) kB = kB - 1 ! start below PBL, but not below lowest level

  kT = kB
  do while (zm(kT) < aerpbl(i,j)+DZ .and. kT < nz)
     kT = kT + 1
  end do

  VPTG = (theta_fn(tt(i,j,kT),pa(i,j,kT),qq(i,j,kT))  - &
          theta_fn(tt(i,j,kB),pa(i,j,kB),qq(i,j,kB))) / &
         (zm(kT) - zm(kB)) ! -delta_theta over delta_z

  return
end subroutine calc_vptg
!
!******************************************************************************
!
subroutine sundat(flat,flon,tzone,julday,hour,elevang)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     Compute solar elevation angle for the given location and time.
!
!     Development History:
!     2011-09-30  New with MMIF v2.0, adapted from AERMET v11059.  
!
!------------------------------------------------------------------------------
!
  IMPLICIT NONE
!
!-----Variable declaration
!
  REAL,    INTENT(IN)  :: fLAT,fLON
  INTEGER, INTENT(IN)  :: TZONE, JULDAY, HOUR
  REAL,    INTENT(OUT) :: ELEVANG

  REAL :: DAYNO,TDAYNO,SIND,COSD,SINTD,COSTD,SIGMA
  REAL :: HI,ALFSN,DEG_PER_RAD,AMM,TEMPZ,DUM,SINLAT,COSLAT,DSIN,DCOS
!
!-----Entry point
!
  DEG_PER_RAD = 57.29578
  DUM    = fLON/15.0 - TZONE
  TEMPZ  = 15.0 * TZONE - fLON
  SINLAT = SIN( fLAT/DEG_PER_RAD )
  COSLAT = COS( fLAT/DEG_PER_RAD )

!---- Determine the fraction of a year for this date.
!        (0.0172028 = 360.0/365.242*57.29578)

  DAYNO  = (JULDAY - 1.0) * 0.0172028
  TDAYNO = 2.0 * DAYNO
  SIND   = SIN(DAYNO)
  COSD   = COS(DAYNO)
  SINTD  = SIN(TDAYNO)
  COSTD  = COS(TDAYNO)


!---- Account for ellipticity of earth's orbit.

  SIGMA = 279.9348 + (DAYNO*DEG_PER_RAD) + 1.914827*SIND - &
          0.079525*COSD + 0.019938*SINTD - 0.00162*COSTD

!---- Find the sine of the solar declination.
!        0.39785 = sin(0.4091720193) = sin(23.44383/57.29578)

  DSIN = 0.39785*SIN(SIGMA/DEG_PER_RAD)
  DCOS = SQRT(1.0-DSIN*DSIN)


!---- Determine time(hrs) of meridian passage

  AMM = 12.0 + 0.12357*SIND - 0.004289*COSD + 0.153809*SINTD + &
        0.060783*COSTD

!---- Determine solar hour angle(in radians) for this hour of the day

  HI = (15.0 * (HOUR-AMM) + TEMPZ) / DEG_PER_RAD
  ALFSN = SINLAT*DSIN + DCOS*COSLAT*COS(HI)

! Calculate solar angle

  ELEVANG = ATAN2(ALFSN,SQRT(1.0-ALFSN*ALFSN)) * DEG_PER_RAD ! degrees

  RETURN
END SUBROUTINE SUNDAT
