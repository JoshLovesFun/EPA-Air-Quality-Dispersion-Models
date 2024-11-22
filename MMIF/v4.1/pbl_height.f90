! Contains:
!
! subroutine pbl_height(nz,zm,ulev,tlev,qlev,plev,u10,v10,t2,q2,p0,ustar,   &
!     iswater,pbl)
! subroutine pbl_limits(limit,ZiMin,ZiMax,mol,ustar, pbl,wstar)

subroutine pbl_height(nz,zm,ulev,vlev,tlev,qlev,plev,u10,v10,t2,q2,p0,ustar,    &
     iswater,pbl,pbl_last)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     Calculates the mixing height based on the critical Richardson
!     number according to Vogelezang and Holtslag (1996).            
!     Variation of critical Richardson number based upon Gryning and         
!     Batchvarova (2003) to account for overwater boundary heights.          
!     The bulk Richardson scheme was adapted from FLEXPART v6.2.
!
!     Literature:                                                            
!     * Vogelezang D. and Holtslag AAM (1996): Evaluation and model impacts    
!       of alternative boundary-layer height formulations. Boundary-Layer      
!       Meteor. 81, 245-269.                                                   
!     * Gryning, S.E. and Batchvarova (2003): Marine Boundary-Layer Height     
!       Estimated from NWP Model Output.  Int. J. Environ. Pollut. 20,         
!       147-153.  
!
!     Outputs: 
!         pbl           Depth of the Planetary Boundary Layer (m)
!                                                                            
!     Development History:
!     2009-05-26  Original Development (EPA R7, ENVIRON International Corp.)
!     2012-01-31  Moved PBL height calculation from pblmet() to this routine.
!     2014-04-24  Interpolate to 1/20 of a layer, or < 10m, whichever is less.
!     2020-12-20  Added temporal smoothing following AERMOD MFED Eqn 26.
!
  USE functions
  implicit none
!
!-----Variable declarations
!
! Output variables
  real pbl                           ! Planetary boundary layer height (m)
  real pbl_last                      ! Last hour's PBL height, for smoothing (m)
! Input variables
  logical iswater                    ! Flag for over-water or over-land
  integer nz                         ! Number of layers in MM5/WRF data
  real u10,v10                       ! Winds at 10m
  real t2,q2,p0                      ! Near-surface reference state for Ri
  real ulev(nz),vlev(nz)             ! Winds from MM5/WRF layer middles
  real tlev(nz),plev(nz),qlev(nz)    ! Temperature, pressure, humidity from WRF
  real zm(nz)                        ! MM5/WRF layer mid-points (m)
  real ustar                         ! Wind speed scaling parameter U*
! Local variables
  real Ri,Ric                        ! Richardson number, and critical Ri
  real theta,theta0                  ! potential temperature
  real z0,u0,v0                      ! lowest mid-layer values
  real frac,zl,ThetaL,ul,vl          ! used to interpolate crit Rich ht
  real ThetaBot,ThetaTop             ! used to interpolate crit Rich ht
  real tau                           ! defined after AERMOD MFED Eqn 25
  real, parameter :: vk = 0.4        ! von karman's constant
  real, parameter :: g  = 9.806      ! m/s**2, acceleration due to gravity
  real, parameter :: RicLand  = 0.25 ! critical Richardson number over land
  real, parameter :: RicWater = 0.05 ! critical Richardson number over water
  real, parameter :: B = 100         ! convective bit of Richardson number
  real, parameter :: MinSpeed = 0.01 ! minimum wind speed for numerical stability
  real, parameter :: beta = 2.       ! See AERMOD MFED equation 25
!  real, parameter :: z10 = 10        ! reference height (m)
  integer num_i                      ! number of sub-layers for interpolation
  integer i,k                        ! indexes
!
!-----Entry point
!
  if (iswater) then
     Ric = RicWater
  else
     Ric = RicLand
  endif
!
!-----Use 2m Theta to avoid reliance on calculcated 10m temperature.
!     The 2/8 is from the old rule of thumb: 8 m/mb near the surface
!
!  Theta2 = Theta_fn(t2,p0-2./8.,q2)
!
!-----Better yet, use the lowest gridded layer rather than a derived
!     value like the 2m or 10m layer.
!
  z0 = zm(1)
  Theta0 = Theta_fn(tlev(1),plev(1),qlev(1)) ! lowest model level
  u0 = ulev(1)
  v0 = vlev(1)
  Ri = 0.
  k = 1
  do while (k < nz .and. Ri <= Ric)
     k = k + 1
!
!-----Calculate Richardson number at each level
!
     Theta = Theta_fn(tlev(k),plev(k),qlev(k)) ! for this layer
! Old method, which used 2m level as the reference level:
!     Ri = g/Theta2 * (theta - Theta2) * (zm(k) - z10) / &
!          max(((ulev(k) - u10)**2 + (vlev(k) - v10)**2 + B*ustar**2),0.1)
! New method, which uses the lowest model level
     Ri = g/Theta0 * (theta - Theta0) * (zm(k) - z0) / &
          ((ulev(k) - u0)**2 + (vlev(k) - v0)**2 + B*ustar**2 + MinSpeed**2)
  end do
!
!-----Find level with Ri = Ri_crit.  Interpolate to 1/20th of MM5/WRF level, 
!     or to the nearest 10m, whichever is less.
!
  Ri    = 0.
  i     = 0
  num_i = 20
  do while ( (zm(k) - zm(k-1))/float(num_i) > 10. .and. num_i < 100 )
     num_i = num_i + 1
  end do

  ThetaBot = Theta_fn(tlev(k-1),plev(k-1),qlev(k-1)) ! of the level for interp
  ThetaTop = Theta_fn(tlev(k),  plev(k),  qlev(k))   ! of the level for interp

  do while (i <= num_i .and. Ri <= Ric)
     i = i + 1
     frac = float(i)/float(num_i)                    ! linear interpolation
     zL = zm(k-1)      + frac*(zm(k)   - zm(k-1))    ! zl is this level, not z/L
     uL = ulev(k-1)    + frac*(ulev(k) - ulev(k-1))  ! ul, vl are winds
     vL = vlev(k-1)    + frac*(vlev(k) - vlev(k-1))  ! at this level
     ThetaL = ThetaBot + frac*(ThetaTop - ThetaBot)  ! ThetaL at this level
     Ri = g/Theta0 * (ThetaL - Theta0) * (zL - z0) / &
          ((uL - u0)**2 + (vL - v0)**2 + B*ustar**2 + MinSpeed**2)
  end do
!
!-----Temporally smooth the mixing height following AERMOD MFED Section 3.4.2
!     "Mechanical mixing height (Zim). zl is this hour's value. 
!
  tau = zl/beta/ustar

! pbl_last == 0 only for the first hour of a run, the 50 prevents underflow

  if (pbl_last == 0. .or. 3600./tau > 50.) then 
     pbl = zl
  else
     pbl = pbl_last * exp(-3600./tau) + zl * (1. - exp(-3600./tau))
  endif

  pbl_last = pbl ! save for next hour

  return
end subroutine pbl_height
!
!******************************************************************************
!
subroutine venkatram_mech_mixh(ustar,ZiMech)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     Calculates the mechanical mixing height, based on Venkatram (1980)
!     approximation to Zilitinkevich (1972) formula.
!
!     Development History:
!     2020-12-15  New with MMIF-3.4.2, for testing purposes.
!
!------------------------------------------------------------------------------
!
  implicit none
!
!-----Variable declaration
!
! Output variables
  real, intent(out)   :: ZiMech      ! mechanical mixing height (m)
! Input variables
  real, intent(in)    :: ustar       ! friction velocity (m/s)
!
!-----Entry point
!
  ZiMech = 2400. * ustar**1.5        ! Venkatram (BLM, 1980) uses 2400, not 2300.

  return
end subroutine venkatram_mech_mixh
!
!******************************************************************************
!
subroutine pbl_limits(ZiMin,ZiMax, mol,ustar,wstar, pbl)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     Sets limits on the PBL height, if necessary, and calculates wstar:
!     the convective velocity scale, in m/s. 
!
!     Development History:
!     2012-01-31  New with MMIF v2.1, moved from old pblmet() routine
!     2012-09-18  Removed logical "limit or not", no longer needed.
!
!------------------------------------------------------------------------------
!
  implicit none
!
!-----Variable declaration
!
! Output variables
  real, intent(inout) ::  pbl        ! Planetary boundary layer height (m)
  real, intent(out)   ::  wstar      ! Vertical velocity scaling parameter (m/s)
! Input variables
  real, intent(in)    :: ZiMin,ZiMax ! Minimum and maximum allowed PBL height
  real, intent(in)    :: mol         ! Monin-Obukhov lenght (m)
  real, intent(in)    :: ustar       ! friction velocity (m/s)
  real, parameter     :: vk = 0.4    ! von karman's constant
!
!-----Entry point
!
!-----Ensure that PBL heights are within the specified range.  CALPUFF will
!     bomb if the PBL height is greater than the top of the modeling domain.
!
  pbl = amax1(pbl, ZiMin) ! prevent too small PBL
  pbl = amin1(pbl, ZiMax) ! prevent too large PBL
!
!-----Re-calculate convective velocity scale, which depends on PBL height.
!     Alternative definition: g * shflux * pbl / (rho * cp * T10)**1/3
!
  wstar = 0.
  if (mol < 0.) wstar = (pbl * ustar**3. / (vk * abs(mol) ))**(1./3.)

  return
end subroutine pbl_limits
