!Contains:
!
! subroutine landuse_USGS(jday,lu,z0,alb,lai)
! subroutine landuse_NLCD50(jday,lu,z0,lai)
! subroutine landuse_NLCD40(jday,lu,z0,lai)
! subroutine landuse_MODIS(jday,lu,z0,lai)
! subroutine landuse_IGBP_MODIS(jday,lu,z0,lai)
!
!******************************************************************************
!
subroutine landuse_USGS(jday,lu,z0,alb,lai)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     Sets various surface parameters based on input season and landuse 
!     code.  This routine assumes that the input landuse code is consistent
!     with the 24/33-cat USGS dataset used in MM5 and WRF.
!
!     Development History:
!     2009-05-26  Original Development (EPA/Region 7)
!     2011-09-30  Bug fix: albedo for water (cat. 16) was 0.8, should be 0.08.  
!     2012-01-31  New subroutine names (landuse_usgs and landuse_nlcd) and 
!                 remove unused vars.
!     2013-09-23  Updated the values to match WRFv3.5 and MCIPv4.1, and added
!                 support for 33-category USGS land-use.
!
!------------------------------------------------------------------------------
!
  implicit none
!
!-----Variable declaration
!
  integer :: iseason,jday,l,lu
  real    :: sfz0(2,33),albd(2,33),leafarea(2,33)
  real    :: z0,        alb,       lai
!
!-----Summer/winter albedo values
!     These are the old values, used in MMIFv2.3 and earlier.
!
!  data (albd(1,l),l=1,24) /.18,.17,3*.18,.16,.19,.22,.20, &   ! fraction,
!       .20,.16,.14,.12,.12,.13,.08,.14,.14,.25,3*.15,.25,.55/ ! not percent
!  data (albd(2,l),l=1,24) /.18,4*.23,.20,.23,.25,.24,  &
!       .20,.17,.15,.12,.12,.14,.08,.14,.14,.25,.60,.50, &
!       .55,.70,.70 /
!
!-----Summer/winter albedo values from WRFv3.5, see run/LANDUSE.TBL.
!
  DATA (albd(1,l),l=1,33)     /  15.0,  17.0,  18.0,  18.0,  18.0,  16.0,   &
                                 19.0,  22.0,  20.0,  20.0,  16.0,  14.0,   &
                                 12.0,  12.0,  13.0,   8.0,  14.0,  14.0,   &
                                 25.0,  15.0,  15.0,  25.0,  15.0,  55.0,   &
                                 30.0,  18.0,  70.0,  15.0,  15.0,  15.0,   &
                                 10.0,  10.0,  10.0 /  ! summer [percent]

  DATA (albd(2,l),l=1,33)     /  15.0,  20.0,  20.0,  20.0,  20.0,  20.0,   &
                                 23.0,  23.0,  22.0,  20.0,  17.0,  15.0,   &
                                 12.0,  12.0,  14.0,   8.0,  14.0,  14.0,   &
                                 23.0,  15.0,  15.0,  15.0,  25.0,  70.0,   &
                                 40.0,  18.0,  70.0,  15.0,  15.0,  15.0,   &
                                 10.0,  10.0,  10.0 /  ! winter [percent]
!
!-----Summer/winter surface roughness (m)
!     These are the old values, used in MMIFv2.3 and earlier.
!
! data (sfz0(1,l),l=1,24) /.50,3*.15,.14,.20,.12,.10, &
!      .11,.15,5*.50,.0001,.20,.40,.10,.10,.30,.15,.10,.05/
! data (sfz0(2,l),l=1,24) /.50,4*.05,.20,3*.10,.15,5*.50,.0001,.20, &
!      .40,.10,.10,.30,.15,.05,.05/  ! mods to Zo for units correction
!
!-----Summer/winter surface roughness from WRFv3.5, see run/LANDUSE.TBL.
!     Identical values can be found in MCIPv4.1, rdwrfem.f90.
!
  DATA (sfz0(1,l),l=1,33)     /  80.0,  15.0,  10.0,  15.0,  14.0,  20.0,   &
                                 12.0,   5.0,   6.0,  15.0,  50.0,  50.0,   &
                                 50.0,  50.0,  50.0,   0.1,  20.0,  40.0,   &
                                  1.0,  10.0,  30.0,  15.0,  10.0,   5.0,   &
                                  1.0,  15.0,   1.0,  80.0,  80.0,  80.0,   &
                                 80.0,  80.0,  80.0 /  ! summer [cm]

  DATA (sfz0(2,l),l=1,33)     /  80.0,   5.0,   2.0,   5.0,   5.0,  20.0,   &
                                 10.0,   1.0,   1.0,  15.0,  50.0,  50.0,   &
                                 50.0,  50.0,  20.0,   0.1,  20.0,  40.0,   &
                                  1.0,  10.0,  30.0,  15.0,   5.0,   5.0,   &
                                  1.0,  15.0,   1.0,  80.0,  80.0,  80.0,   &
                                 80.0,  80.0,  80.0 /  ! winter [cm]
!
!-----Summer/winter LAI values by JKV, after CMAQ Sci Alg Doc Tables 4-3, 4-4
!     These are the old values, used in MMIFv2.3 and earlier.
!
! data (leafarea(1,l), l=1,24) /1.5,0,1,0.5,0,3,0,2,1,2,6,5,5,8,4,0, &
!      2,5,0,1,1,1,0,0/
! data (leafarea(2,l), l=1,24) /1,4*0,0.5,0,0.5,0.25,0.5,0,0,5, &
!      7,2,0,1,3,6*0/
!
!-----Summer/winter Leaf Area Index from MCIPv4.1, see metvars2ctm.f90, arrays
!     laiusgs and laimnusgs.
!
  DATA (leafarea(1,l),l=1,33)  / 2.0,    3.0,    3.0,    3.0,    2.5,    4.0,  &
                                 2.5,    3.0,    3.0,    2.0,    5.0,    5.0,  &
                                 5.0,    4.0,    5.0,    0.0,    2.0,    5.0,  &
                                 0.5,    1.0,    1.0,    1.0,    0.1,    0.1,  &
                                 0.1,    0.1,    0.1,    0.0,    0.0,    0.0,  &
                                 2.2,    2.1,    2.0 /  ! summer


  DATA (leafarea(2,l),l=1,33)  / 0.5,    0.5,    0.5,    0.5,    1.0,    0.5,  &
                                 1.0,    1.0,    1.0,    1.0,    1.0,    1.0,  &
                                 4.0,    3.0,    2.0,    0.0,    1.0,    3.0,  &
                                 0.2,    0.5,    0.5,    0.5,    0.1,    0.1,  &
                                 0.1,    0.1,    0.1,    0.0,    0.0,    0.0,  &
                                 0.7,    0.6,    0.5 /  ! winter
!
!-----Entry point
!     Set season according to Julian day
!
  if (jday.ge.105 .and. jday.le.287) then
     iseason = 1
  else
     iseason = 2
  endif
!
!-----Set surface values
!
  alb = albd(iseason,lu)/100. ! look-up table in percent, output in fraction
  z0  = sfz0(iseason,lu)/100. ! look-up table in cm, output in m
  lai = leafarea(iseason,lu)

  return
end subroutine landuse_USGS
!
!******************************************************************************
!
subroutine landuse_NLCD50(jday,lu,z0,lai)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     Sets various surface parameters based on input season and landuse 
!     code.  This routine assumes that the input landuse code is consistent
!     with the 50-cat NLCD dataset used in WRF.
!
!     Development History:
!     2011-08-01  (FWS) Support for 50-cat NLCD data used in EPA 2008 WRF.
!     2012-01-31  Renamed
!     2013-09-23  Renamed again, now that NLCD40 has appeared in WRFv3.5.
!
!------------------------------------------------------------------------------
!
  implicit none
!
!-----Variable declaration
!
  integer :: iseason,jday,l,lu
  real    :: sfz0(2,50),leafarea(2,50)
  real    :: z0,        lai
!
!-----Summer/winter surface roughness from MCIP v3.6, 50-category NLCD
!
  DATA (sfz0(1,l),l=1,50)     /  0.1,   1.2,  30.0,  40.0,  60.0, 100.0,   &
                                 5.0,   5.0, 100.0, 100.0, 100.0,  10.0,   &
                                30.0,   7.0,   7.0,   5.0,   5.0,   5.0,   &
                                 7.0,  10.0,  55.0,  80.0,  30.0,  60.0,   &
                                30.0,  11.0,  11.0,  11.0,   5.0,   5.0,   &
                                 0.1, 100.0,  90.0, 100.0, 100.0, 100.0,   &
                                30.0,  20.0,  25.0,  15.0,   7.0,  20.0,   &
                                10.0,  80.0,  10.0,   1.2,   5.0,   0.1,   &
                                 0.1,   0.1 /  ! summer [cm]

  DATA (sfz0(2,l),l=1,50)     /  0.1,   1.2,  30.0,  40.0,  60.0, 100.0,   &
                                 5.0,   5.0, 100.0, 100.0, 100.0,  10.0,   &
                                30.0,   7.0,   7.0,   5.0,   5.0,   5.0,   &
                                 7.0,  10.0,  55.0,  80.0,  30.0,  60.0,   &
                                30.0,  11.0,  11.0,  11.0,   5.0,   5.0,   &
                                 0.1, 100.0,  90.0, 100.0, 100.0, 100.0,   &
                                30.0,  20.0,  25.0,  15.0,   7.0,  20.0,   &
                                10.0,  80.0,  10.0,   1.2,   5.0,   0.1,   &
                                 0.1,   0.1 /  ! winter [cm]

!
!-----Summer/winter Leaf Area Index from MCIP v3.6, 50-category NLCD
!
  DATA (leafarea(1,l),l=1,50)  / 0.0,    0.1,    3.0,    3.0,    3.0,    3.0,  &
                                 1.0,    0.5,    5.0,    4.0,    5.0,    2.0,  &
                                 2.5,    2.5,    2.0,    1.0,    1.0,    1.0,  &
                                 3.0,    3.0,    5.0,    5.0,    3.0,    5.0,  &
                                 3.0,    2.0,    2.0,    2.0,    1.0,    1.0,  &
                                 0.0,    5.0,    5.0,    5.0,    5.0,    5.0,  &
                                 3.0,    2.0,    2.5,    2.0,    2.5,    3.0,  &
                                 3.0,    3.0,    3.0,    0.1,    1.0,    0.0,  &
                                 0.0,    0.0 /  ! summer

  DATA (leafarea(2,l),l=1,50)  / 0.0,    0.1,    1.0,    1.0,    1.0,    1.0,  &
                                 0.5,    0.2,    1.0,    2.5,    2.0,    1.0,  &
                                 1.0,    1.0,    1.0,    1.0,    1.0,    1.0,  &
                                 1.0,    0.5,    2.0,    2.0,    1.0,    2.0,  &
                                 1.0,    1.0,    1.0,    1.0,    0.5,    0.5,  &
                                 0.0,    3.0,    4.0,    1.0,    1.0,    2.0,  &
                                 1.0,    1.0,    1.0,    1.0,    1.0,    1.0,  &
                                 0.5,    1.0,    1.0,    0.1,    0.5,    0.0,  &
                                 0.0,    0.0 /  ! winter
!
!-----Entry point
!     Set season according to Julian day
!
  if (jday.ge.105 .and. jday.le.287) then
     iseason = 1
  else
     iseason = 2
  endif
!
!-----Set surface values
!
  z0  = sfz0(iseason,lu)/100. ! look-up table in cm, output in m
  lai = leafarea(iseason,lu)

  return
end subroutine landuse_NLCD50
!
!******************************************************************************
!
subroutine landuse_NLCD40(jday,lu,z0,alb,lai)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     Sets various surface parameters based on input season and landuse 
!     code.  This routine assumes that the input landuse code is consistent
!     with the 40-cat NLCD dataset used in WRF.
!
!     Development History:
!     2013-09-23  Original development (ENVIRON International Corp) following 
!                 code submitted by Bret Anderson.
!
!------------------------------------------------------------------------------
!
  implicit none
!
!-----Variable declaration
!
  integer :: iseason,jday,l,lu
  real    :: sfz0(2,40),albd(2,40),leafarea(2,40)
  real    :: z0,        alb,       lai
!
!-----Summer/winter surface roughness from WRFv3.5, see run/LANDUSE.TBL
!
  DATA (sfz0(1,l),l=1,40)     / 100.0, 100.0, 100.0, 100.0, 100.0,  15.0,   &
                                 15.0,  25.0,  15.0,   7.0,  20.0,  10.0,   &
                                 80.0,  30.0,   1.2,   5.0,  0.01,  0.01,   &
                                 0.01,  0.01,  0.01,   1.2,  30.0,  40.0,   &
                                 60.0, 100.0,   5.0, 100.0, 100.0, 100.0,   &
                                 10.0,  15.0,   7.0,   7.0,   5.0,   5.0,   &
                                 7.0,   10.0,  55.0,  11.0 /  ! summer [cm]

  DATA (sfz0(2,l),l=1,40)     / 100.0, 100.0, 100.0, 100.0, 100.0,  15.0,   &
                                 15.0,  25.0,  15.0,   7.0,  20.0,  10.0,   &
                                 80.0,  30.0,   1.2,   5.0,  0.01,  0.01,   &
                                 0.01,  0.01,  0.01,   1.2,  30.0,  40.0,   &
                                 60.0, 100.0,   5.0, 100.0, 100.0, 100.0,   &
                                 10.0,  15.0,   7.0,   7.0,   5.0,   5.0,   &
                                 7.0,   10.0,  55.0,  11.0 /  ! winter [cm]
!
!-----Summer/winter surface roughness provided by Bret Anderson (FWS)
!
!  DATA (sfz0(1,l),l=1,40)     / 100.0,  90.0, 100.0, 100.0, 100.0,  30.0,   &
!                                 20.0,  25.0,  15.0,   7.0,  20.0,  10.0,   &
!                                 80.0,  10.0,   1.2,   5.0,   0.1,   0.1,   &
!                                  0.1,   0.1,   0.1,   1.2,  30.0,  40.0,   &
!                                 60.0, 100.0,   5.0, 100.0, 100.0, 100.0,   &
!                                 10.0,  30.0,   7.0,   7.0,   5.0,   5.0,   &
!                                 7.0,   10.0,  55.0,  15.0 /  ! summer [cm]

!   DATA (sfz0(2,l),l=1,40)     / 100.0,  90.0, 100.0, 100.0, 100.0,  30.0,   &
!                                  20.0,  25.0,  15.0,   7.0,  20.0,  10.0,   &
!                                  80.0,  10.0,   1.2,   5.0,   0.1,   0.1,   &
!                                   0.1,   0.1,   0.1,   1.2,  30.0,  40.0,   &
!                                  60.0, 100.0,   5.0, 100.0, 100.0, 100.0,   &
!                                  10.0,  30.0,   7.0,   7.0,   5.0,   5.0,   &
!                                  7.0,   10.0,  55.0,  15.0 /  ! winter [cm]
!
!-----Summer/winter albedo values from WRFv3.5, see run/LANDUSE.TBL.
!
  DATA (albd(1,l),l=1,40)     /  12.0,  12.0,  14.0,  16.0,  13.0,  22.0,   &
                                 20.0,  22.0,  20.0,  19.0,  14.0,  18.0,   &
                                 11.0,  18.0,  60.0,  25.0,   8.0,   8.0,   &
                                  8.0,   8.0,   8.0,  60.0,  12.0,  11.0,   &
                                 10.0,  10.0,  20.0,  15.0,  12.0,  13.0,   &
                                 20.0,  20.0,  19.0,  23.0,  20.0,  20.0,   &
                                 18.0,  18.0,  15.0,  18.0 /  ! summer [percent]

  DATA (albd(2,l),l=1,40)     /  12.0,  12.0,  14.0,  16.0,  13.0,  22.0,   &
                                 20.0,  22.0,  20.0,  19.0,  14.0,  18.0,   &
                                 11.0,  18.0,  60.0,  25.0,   8.0,   8.0,   &
                                  8.0,   8.0,   8.0,  60.0,  12.0,  11.0,   &
                                 10.0,  10.0,  20.0,  15.0,  12.0,  13.0,   &
                                 20.0,  20.0,  19.0,  23.0,  20.0,  20.0,   &
                                 18.0,  18.0,  15.0,  18.0 /  ! winter [percent]
!
!-----Summer/winter Leaf Area Index provided by Bret Anderson (FWS)
!
  DATA (leafarea(1,l),l=1,40)  / 5.0,    5.0,    5.0,    5.0,    5.0,    3.0,  &
                                 2.0,    2.5,    2.0,    2.5,    3.0,    3.0,  &
                                 3.0,    3.0,    0.1,    1.0,    0.0,    0.0,  &
                                 0.0,    0.0,    0.0,    0.1,    3.0,    3.0,  &
                                 3.0,    3.0,    1.0,    5.0,    4.0,    5.0,  &
                                 2.0,    2.5,    2.5,    2.0,    1.0,    1.0,  &
                                 3.0,    3.0,    5.0,    2.0 /  ! summer


  DATA (leafarea(2,l),l=1,40)  / 3.0,    4.0,    1.0,    1.0,    2.0,    1.0,  &
                                 1.0,    1.0,    1.0,    1.0,    1.0,    0.5,  &
                                 1.0,    1.0,    0.1,    0.5,    0.0,    0.0,  &
                                 0.0,    0.0,    0.0,    0.1,    1.0,    1.0,  &
                                 1.0,    1.0,    0.5,    1.0,    2.5,    2.0,  &
                                 1.0,    1.0,    1.0,    1.0,    1.0,    1.0,  &
                                 1.0,    0.5,    2.0,    1.0 /  ! winter
!
!-----Entry point
!     Set season according to Julian day
!
  if (jday.ge.105 .and. jday.le.287) then
     iseason = 1
  else
     iseason = 2
  endif
!
!-----Set surface values
!
  alb = albd(iseason,lu)/100. ! look-up table in percent, output in fraction
  z0  = sfz0(iseason,lu)/100. ! look-up table in cm, output in m
  lai = leafarea(iseason,lu)

  return
end subroutine landuse_NLCD40
!
!******************************************************************************
!
subroutine landuse_MODIS(jday,lu,z0,alb,lai)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     Sets various surface parameters based on input season and landuse 
!     code.  This routine assumes that the input landuse code is consistent
!     with the 20/33-cat MODIS dataset used in WRF.
!
!     Development History:
!     2013-09-23  Original development (ENVIRON International Corp).
!
!------------------------------------------------------------------------------
!
  implicit none
!
!-----Variable declaration
!
  integer :: iseason,jday,l,lu
  real    :: sfz0(2,33),albd(2,33),leafarea(2,33)
  real    :: z0,        alb,       lai
!
!-----Summer/winter surface roughness from WRFv3.5, see run/LANDUSE.TBL
!
  DATA (sfz0(1,l),l=1,33)     /  50.0,  50.0,  50.0,  50.0,  50.0,  10.0,   &
                                 10.0,  15.0,  15.0,   7.5,  30.0,   7.5,   &
                                 50.0,   6.5,   1.0,   6.5,  0.01,  15.0,   &
                                 10.0,   6.0,  0.01,  80.0,  80.0,  80.0,   &
                                 80.0,  80.0,  80.0,  80.0,  80.0,  80.0,   &
                                 80.0,  80.0,  80.0 /  ! summer [cm]

  DATA (sfz0(2,l),l=1,33)     /  50.0,  50.0,  50.0,  50.0,  50.0,  10.0,   &
                                 10.0,  15.0,  15.0,   7.5,  30.0,   7.5,   &
                                 50.0,   6.5,   1.0,   6.5,  0.01,  15.0,   &
                                 10.0,   6.0,  0.01,  80.0,  80.0,  80.0,   &
                                 80.0,  80.0,  80.0,  80.0,  80.0,  80.0,   &
                                 80.0,  80.0,  80.0 /  ! winter [cm]
!
!-----Summer/winter albedo values from WRFv3.5, see run/LANDUSE.TBL.
!
  DATA (albd(1,l),l=1,33)     /  12.0,  12.0,  14.0,  16.0,  13.0,  22.0,   &
                                 20.0,  20.0,  20.0,  19.0,  14.0,  18.0,   &
                                 18.0,  16.0,  55.0,  25.0,   8.0,  15.0,   &
                                 15.0,  15.0,   8.0,  15.0,  15.0,  15.0,   &
                                 15.0,  15.0,  15.0,  15.0,  15.0,  15.0,   &
                                 10.0,  10.0,  10.0 /  ! summer [percent]

  DATA (albd(2,l),l=1,33)     /  12.0,  14.0,  14.0,  16.0,  13.0,  22.0,   &
                                 20.0,  20.0,  20.0,  19.0,  14.0,  18.0,   &
                                 18.0,  16.0,  55.0,  25.0,   8.0,  15.0,   &
                                 15.0,  15.0,   8.0,  15.0,  15.0,  15.0,   &
                                 15.0,  15.0,  15.0,  15.0,  15.0,  15.0,   &
                                 10.0,  10.0,  10.0 /  ! winter [percent]
!
!-----Summer/winter Leaf Area Index from MCIPv4.1, see metvars2ctm.f90, arrays
!     laimod and laimnmod.  Same for both MODIS and MODIFIED_IGBP_MODIS_NOAH.
!
  DATA (leafarea(1,l),l=1,33)  / 5.0,    5.0,    5.0,    5.0,    5.0,    3.0,  &
                                 2.0,    2.5,    2.0,    2.5,    3.0,    3.0,  &
                                 3.0,    3.0,    0.1,    1.0,    0.0,    3.4,  &
                                 2.4,    1.4,    0.0,    0.0,    0.0,    0.0,  &
                                 0.0,    0.0,    0.0,    0.0,    0.0,    0.0,  &
                                 2.2,    2.1,    2.0 /  ! summer


  DATA (leafarea(2,l),l=1,33)  / 3.0,    4.0,    1.0,    1.0,    2.0,    1.0,  &
                                 1.0,    1.0,    1.0,    1.0,    1.0,    0.5,  &
                                 1.0,    1.0,    0.1,    0.5,    0.0,    2.0,  &
                                 1.0,    0.1,    0.0,    0.0,    0.0,    0.0,  &
                                 0.0,    0.0,    0.0,    0.0,    0.0,    0.0,  &
                                 0.7,    0.6,    0.5 /  ! winter
!
!-----Entry point
!     Set season according to Julian day
!
  if (jday.ge.105 .and. jday.le.287) then
     iseason = 1
  else
     iseason = 2
  endif
!
!-----Set surface values
!
  alb = albd(iseason,lu)/100. ! look-up table in percent, output in fraction
  z0  = sfz0(iseason,lu)/100. ! look-up table in cm, output in m
  lai = leafarea(iseason,lu)

  return
end subroutine landuse_MODIS
!
!******************************************************************************
!
subroutine landuse_IGBP_MODIS(jday,lu,z0,alb,lai)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     Sets various surface parameters based on input season and landuse 
!     code.  This routine assumes that the input landuse code is consistent
!     with the 20/33-cat MODIFIED_IGBP_MODIS_NOAH dataset used in WRF.
!
!     Development History:
!     2013-09-23  Original development (ENVIRON International Corp).
!
!------------------------------------------------------------------------------
!
  implicit none
!
!-----Variable declaration
!
  integer :: iseason,jday,l,lu
  real    :: sfz0(2,33),albd(2,33),leafarea(2,33)
  real    :: z0,        alb,       lai
!
!-----Summer/winter surface roughness from WRFv3.5, see run/LANDUSE.TBL
!
  DATA (sfz0(1,l),l=1,33)     /  50.0,  50.0,  50.0,  50.0,  50.0,   5.0,   &
                                  6.0,   5.0,  15.0,  12.0,  30.0,  15.0,   &
                                 80.0,  14.0,   0.1,   0.1,  0.01,  30.0,   &
                                 15.0,  10.0,  80.0,  80.0,  80.0,  80.0,   &
                                 80.0,  80.0,  80.0,  80.0,  80.0,  80.0,   &
                                 80.0,  80.0,  80.0 /  ! summer [cm]

  DATA (sfz0(2,l),l=1,33)     /  50.0,  50.0,  50.0,  50.0,  20.0,   1.0,   &
                                  1.0,   1.0,  15.0,  10.0,  30.0,   5.0,   &
                                 80.0,   5.0,   0.1,   1.0,  0.01,  30.0,   &
                                 15.0,   5.0,  0.01,  80.0,  80.0,  80.0,   &
                                 80.0,  80.0,  80.0,  80.0,  80.0,  80.0,   &
                                 80.0,  80.0,  80.0 /  ! winter [cm]
!
!-----Summer/winter albedo values from WRFv3.5, see run/LANDUSE.TBL.
!
  DATA (albd(1,l),l=1,33)     /  12.0,  12.0,  14.0,  16.0,  13.0,  22.0,   &
                                 20.0,  20.0,  20.0,  19.0,  14.0,  18.0,   &
                                 18.0,  16.0,  55.0,  25.0,   8.0,  15.0,   &
                                 15.0,  15.0,   8.0,  15.0,  15.0,  15.0,   &
                                 15.0,  15.0,  15.0,  15.0,  15.0,  15.0,   &
                                 10.0,  10.0,  10.0 /  ! summer [percent]

  DATA (albd(2,l),l=1,33)     /  12.0,  14.0,  14.0,  16.0,  13.0,  22.0,   &
                                 20.0,  20.0,  20.0,  19.0,  14.0,  18.0,   &
                                 18.0,  16.0,  55.0,  25.0,   8.0,  15.0,   &
                                 15.0,  15.0,   8.0,  15.0,  15.0,  15.0,   &
                                 15.0,  15.0,  15.0,  15.0,  15.0,  15.0,   &
                                 10.0,  10.0,  10.0 /  ! winter [percent]
!
!-----Summer/winter Leaf Area Index from MCIPv4.1, see metvars2ctm.f90, arrays
!     laimod and laimnmod.  Same for both MODIS and MODIFIED_IGBP_MODIS_NOAH.
!
  DATA (leafarea(1,l),l=1,33)  / 5.0,    5.0,    5.0,    5.0,    5.0,    3.0,  &
                                 2.0,    2.5,    2.0,    2.5,    3.0,    3.0,  &
                                 3.0,    3.0,    0.1,    1.0,    0.0,    3.4,  &
                                 2.4,    1.4,    0.0,    0.0,    0.0,    0.0,  &
                                 0.0,    0.0,    0.0,    0.0,    0.0,    0.0,  &
                                 2.2,    2.1,    2.0 /  ! summer


  DATA (leafarea(2,l),l=1,33)  / 3.0,    4.0,    1.0,    1.0,    2.0,    1.0,  &
                                 1.0,    1.0,    1.0,    1.0,    1.0,    0.5,  &
                                 1.0,    1.0,    0.1,    0.5,    0.0,    2.0,  &
                                 1.0,    0.1,    0.0,    0.0,    0.0,    0.0,  &
                                 0.0,    0.0,    0.0,    0.0,    0.0,    0.0,  &
                                 0.7,    0.6,    0.5 /  ! winter
!
!-----Entry point
!     Set season according to Julian day
!
  if (jday.ge.105 .and. jday.le.287) then
     iseason = 1
  else
     iseason = 2
  endif
!
!-----Set surface values
!
  alb = albd(iseason,lu)/100. ! look-up table in percent, output in fraction
  z0  = sfz0(iseason,lu)/100. ! look-up table in cm, output in m
  lai = leafarea(iseason,lu)

  return
end subroutine landuse_IGBP_MODIS
