!Contains:
!
! subroutine aercoare_useful(iUnit,fname1,iOut)
! subroutine aercoare_header(iUnit,iOut)
! subroutine aercoare_hour(iUnit,iOut)
!
!******************************************************************************
!
subroutine aercoare_useful(iUnit,fnameU,fname1,iOut)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     Writes the Useful Info file for aercoare.
!
!     Development History:
!     2013-09-20  Original Development (ENVIRON International Corp), extracted
!                 from aercoare_header().
!
!------------------------------------------------------------------------------
!
  USE met_fields
  USE functions
  implicit none
!
!-----Variable declaration
!
  character (len=*), intent(in) :: fnameU ! filename of Useful Info file
  character (len=*), intent(in) :: fname1 ! filename of output #1, CSV
  integer,   intent(in)         :: iUnit  ! Logical unit for output
  integer,   intent(in)         :: iOut   ! location to be output
  integer                       :: i,j,k  ! local index vars
!
!-----Format statements
!
1 format(2a)
2 format(f8.3,a)
3 format(i2,a)
!
!-----Entry point
!
  i = iPt(iOut)  ! short-hand
  j = jPt(iOut) 

  k = index(fname1,".",.true.)-1   ! find .ext
  if (k <= 0) k = len_trim(fname1) ! might be none
!
!-----Write a block for running AERCOARE
!
  open(iUnit,file=fnameU,status='unknown')

  write(iUnit,1) trim(fname1),"      | input met file"
  write(iUnit,1) trim(fname1(1:k))//".sfc      | output sfc file"
  write(iUnit,1) trim(fname1(1:k))//".pfl      | output pfl file"
  write(iUnit,1) trim(fname1(1:k))//".out      | output listing/debug file"
  write(iUnit,2) ylat(i,j), "      | lat (degN)"
  write(iUnit,2) -xlon(i,j),"      | lon (degW)"
  write(iUnit,3) -PtZone(iOut), "            | time zone (pos for western himisphere)"
  write(iUnit,1) "600.          | mix height (m) for COARE gustiness calc"
  write(iUnit,1) "25.           | min mix height (m)"
  write(iUnit,1) "5.            | min abs(monin-obukhov length) (m)"
  write(iUnit,1) "0.5           | calms threshold (m/s) winds < this are calm"
  write(iUnit,1) "0.01          | default vert pot temp gradient (degC/m)"
  write(iUnit,1) "10.0          | default buoy wind measurement height (m)"
  write(iUnit,1) "2.0           | default buoy temp measurement height (m)"
  write(iUnit,1) "2.0           | default buoy RH measurement height (m)"
  write(iUnit,1) "0.002         | default buoy water temp depth (m)"
  write(iUnit,1) "0             | mix ht opt (0-obs for zic & zim),1-obs for zic, venk zim; "
  write(iUnit,1) "0             | warm layer (1-yes, 0-no)"
  write(iUnit,1) "0             | cool skin (1-yes, 0-no)"
  write(iUnit,1) "0             | 0=Charnock,1=Oost et al,2=Taylor and Yelland"
  write(iUnit,1) "'end',1,0,0   | 'variable', scale, min, max"

  close(iUnit)

  return
end subroutine aercoare_useful
!
!******************************************************************************
!
subroutine aercoare_header(iUnit)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     Writes the 1-line header to an AERCARE *.CSV file
!
!     Development History:
!     2011-09-30  New with MMIF v2.0
!     2013-02-25  Lengthened file/path strings from 120 to 256 characters, 
!                 based on user feedback.
!     2013-03-18  Added tsky = cloud cover to output for AERCOARE, just so it
!                 appears in the eventual SFC file (used only for deposition).
!     2013-09-21  Moved most the contents to aercoare_useful() above.
!     2014-05-15  Prettied up the header line.
!
!------------------------------------------------------------------------------
!
  implicit none
!
!-----Variable declaration
!
  integer,   intent(in)         :: iUnit ! Logical unit for output
!
!-----Format statements
!
1 format(2x,a2,',',2(a2,','),a2,$)
2 format(16(',',a12),$)
!
!-----Entry point
!-----Write header on AERCOARE data file
!
  write(iUnit,1) "yr","mo","dy","hr"
  write(iUnit,2) "wspd","wdir","tsea","tair","relh","pres","srad", &
       "rdow","rain","tsky","mixh","vptg","zwsp","ztem","zrel","zdep"
  write(iUnit,*)                   ! end of line

end subroutine aercoare_header
!
!******************************************************************************
!
subroutine aercoare_hour(iUnit,iOut)
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
!     2013-03-18  Added tsky = cloud cover to output; removed *100 from rain
!     2013-05-02  Added fixed-control-file-format supporting multiple outs.
!     2013-07-12  For aggregation, use zPt(,) instead of zmid(), because the 
!                 latter is the average over the full 3-D domain.
!     2014-07-30  Prevent output outside requested timestamps in THIS timezone.
!     2014-11-26  No reason RH needs to be rounded to nearest integer.
!     2020-07-09  Use lowest layer instead of U10, V10 if Z(1) < 13, to match
!                 the methodology when running in AERMET mode.
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
  integer   :: iyr,imo,idy,ihr, i,j, ThisHour
  real      :: rh,speed,dir
  real      :: ZiOut, ZiMech, ZiConv
!
!-----Format statements
!
1 format(i4,',',2(i2,','),i2,$)
2 format(',',f12.5,$)
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
!
!-----U and V are aligned with the local grid -- need to rotate them to 
!     true East and North.
!   
  if (zPt(iOut,1) < 13.) then ! lowest level < 10m, use it instead
     call uv2sd(uOut(i,j,1),vOut(i,j,1),cosalpha(i,j),sinalpha(i,j), &
          .true.,speed,dir)
  else
     call uv2sd(u10(i,j),v10(i,j),cosalpha(i,j),sinalpha(i,j), &
          .true.,speed,dir)
  endif
  rh = min(100., q2(i,j) / qs_fn(t2(i,j),psfc(i,j)) * 100. )

  ZiOut = pbl(i,j)
  !call venkatram_mech_mixh(ustar(i,j),ZiMech)
  !ZiConv = pbl(i,j) 
  !ZiOut = ZiConv
  !if (CalcPBL == "MMIF") then           ! same as AERMOD MFED eqn 27, page 22
  !   if (mol(i,j) < 0.) then            ! convective hours
  !      ZiOut = max( ZiMech, ZiConv )
  !   else                               ! stable hours
  !      ZiOut = ZiMech
  !   endif
  !endif
!
!-----Write the actual output
!  
  write(iUnit,1) iyr,imo,idy,ihr   ! yr,mo,dy,hr: time stamp
  write(iUnit,2) speed             ! wspd: wind speed (m/s)
  write(iUnit,2) dir               ! wdir: wind direction (deg)
  write(iUnit,2) sst(i,j)-273.15   ! tsea: sea surface temperature (C)
  write(iUnit,2) t2(i,j)-273.15    ! tair: air temperature at 2m (C)
  write(iUnit,2) rh                ! relh: relative humidity at 2m (%)
  write(iUnit,2) psfc(i,j)         ! pres: surface pressure (mb)
  write(iUnit,2) qsw(i,j)          ! srad: downward solar radiation (W/m^2)
  write(iUnit,2) qlw(1,j)          ! rdow: downward longwave rad. (W/m^2)
  write(iUnit,2) rain(i,j)         ! rain: precipitation rate (mm/hr)
  write(iUnit,2) 10.*cldcvr(i,j)   ! tsky: cloud cover (10ths)
  write(iUnit,2) ZiOut             ! mixh: mixing height (m)
  write(iUnit,2) VPTG              ! vptg: vertical potential temp gradient (K/m)
  if (zPt(iOut,1) < 10.) then                    
     write(iUnit,2) zPt(iOut,1)    ! zwsp: height of wind measurement (m)
  else
     write(iUnit,2) 10.            ! zwsp: height of wind measurement (m)
  endif
  write(iUnit,2) 2.                ! ztem: height of temperature measurement (m)
  write(iUnit,2) 2.                ! zrel: height of RHmeasurement (m)
  write(iUnit,2) 0.002             ! zdep: depth of SST measurement (m)
  write(iUnit,*)                   ! end of line

end subroutine aercoare_hour
