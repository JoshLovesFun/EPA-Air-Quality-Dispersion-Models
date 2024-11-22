subroutine sfc_layer(iswater,mol,Ustar,z0,tsfc,smois,psfc,shflux,lhflux,        &
     zlev,ulev,vlev,tlev,qlev,plev,                                       &
     lu10,lt2,lq2,lmol, u10,v10,t10,t2,q2)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     Calculates any missing surface layer theory parameters:
!      mol        Monin-Obukhov length (m), often written as L
!      u10,v10    Winds at 10m (m/s)
!      t10        Temperature at 10m (K)
!
!     Development History:
!     2012-01-31  New with MMIF v2.1, following MCIPv3.6 pblsup.f90 procedure
!     2012-02-21  Account for uv2sd()'s support for MET convention.
!     2012-03-16  Calculate q2 if it's missing.
!     2014-03-06  New version of uv2sd to support PS/EM projections.
!     2014-04-24  Added option to (re-)diagnose T2 from lowest model level.
!     2014-09-18  After limiting L, need to find matching Ustar.
!
!------------------------------------------------------------------------------
!
  USE functions
  implicit none
!
!-----Variable declaration
!
! Outputs:
  real,   intent(inout):: mol            ! Monin-Obukhov lenght (m), output too
  real,   intent(out)  :: u10,v10        ! 10m winds, output only if lu10
  real,   intent(out)  :: t10            ! 10m temperature (K), always output
  real,   intent(inout):: t2             !  2m temperature (K), only if lt2
  real,   intent(inout):: q2             !  2m humidity (g/kg), only if lq2
  real,   intent(inout):: Ustar          ! friction velocity (m/s)
! Inputs:
  real,   intent(in)   :: z0             ! roughness length (m)
  real,   intent(in)   :: zlev           ! height of lowest MM5/WRF mid-point
  real,   intent(in)   :: ulev,vlev      ! lowest MM5/WRF winds (m/s)
  real,   intent(in)   :: tlev,qlev,plev ! MM5/WRF temp, humidity, pressure
  real,   intent(in)   :: shflux         ! sensible heat flux (W/m^2)
  real,   intent(in)   :: lhflux         ! latent   heat flux (W/m^2)
  real,   intent(in)   :: tsfc           ! surface temperature (K)
  real,   intent(in)   :: psfc           ! surface pressure (mb)
  real,   intent(in)   :: smois          ! surface moisture if over land (kg/kg)
  logical,intent(in)   :: iswater        ! T if over water, F if over land
  logical,intent(in)   :: lu10,lt2,lq2   ! diagnose u10,v10,T2,q2 if .true.
  logical,intent(in)   :: lmol           ! diagnose L (Monin-Obukhov length)?
! Local:
  real                 :: slev,dlev,s10  ! wind speed and direction at zlev,10m
  real                 :: p2,p10         ! pressure at 2m and 10m, from plev
  real                 :: Theta,Theta0   ! potential temperature at zlev,sfc
  real                 :: Theta2,Theta10 ! potential temperature at 2m,10m
  real                 :: ThetaV,VirtFlux! virtual potential temp and flux
  real                 :: Tstar,Qstar    ! scaling params for T,q (like Ustar)
  real                 :: q0             ! mixing ratio at 0m (g/kg)
  real                 :: Cp             ! specific heat (J/K/kg)
  real                 :: rho            ! air density (kg/m^3)
  real                 :: psim_diff,psih_diff ! stractification corrections
  real                 :: old_mol        ! used to recalc ustar if MOL is limited
  real                 :: z00            ! just to appease gfortran's pickiness
  real, external       :: psiu,psit      ! COARE versions of stratification fns
  real, parameter      :: vk  = 0.4      ! von karman's constant
  real, parameter      :: vkH = 0.45     ! von karman's constant for T
  real, parameter      :: vkE = 0.45     ! von karman's constant for Q
  real, parameter      :: g   = 9.81     ! acceleration due to gravity (m/s^2)
  real, parameter      :: pro = 0.95     ! from Hogstrom (1988)
  real, parameter      :: z10 = 10.      ! 10m, the output height of u10,t10
  real, parameter      :: z2  = 2.       !  2m, the output height of q2
  real, parameter      :: molmin = 1.    ! (AERMOD's) minimum allowed abs(L)
!
!-----Entry point
!
  Cp     = 1004.67 * (1.0 + 0.84 * qlev) ! calc specific heat
  Theta  = Theta_fn(tlev,plev,qlev)      ! potential temperature at zlev
  Theta0 = Theta_fn(tsfc,psfc,q0)        ! potential temperature at sfc
  ThetaV = Theta * (1. + 0.608 * qlev)   ! virtual potential temperature at zlev
  rho    = density_fn(tlev,plev,qlev)    ! air density at zlev

  if (iswater) then
     q0 = qs_fn(tsfc,psfc)               ! over water is saturated (g/kg)
  else
     q0 = smois * 1.e-3                  ! over water is from WRF (now g/kg)
  end if
     
  if (.not. lmol) then                   ! if MOL missing, calculate it
     if (Ustar > 1.e-7) then             ! prevent underflow

        VirtFlux = shflux / rho / Cp * (1. + 0.608 * qlev) +  &
             0.608 * Theta * lhflux / rho / L_fn(tlev) 

        if (abs(VirtFlux) > 1.e-7) then  ! prevent underflow
           mol = -1. * ThetaV * Ustar**3 / (vk * g * VirtFlux) 
        else
           mol = -1. * ThetaV * Ustar**3 / (vk * g * 1.e-7)*sign(1.,VirtFlux)
        endif

     else       ! Ustar is very small, so MOL is very small
        mol = molmin
     endif
  end if        !   if (.not. lmol) then
!
!-----Limit extreme values of L, regardless of where it came from
!
  if (abs(mol) < molmin) then
     old_mol = mol
     mol = sign( max( abs(mol), molmin), mol )
     Ustar = Ustar*(mol/old_mol)**(1./3.) ! adjust Ustar to revised MO Length
  endif
!
!-----Calculate the temperature and moisture MO scaling parameters.
!
!     To do this correctly, we'd really have to re-solve the whole MO equations,
!     which requires iteration and has convergence problems at low wind speeds.
!     The method here makes a Tstar/Qstar that is consistent with WRF's surface
!     fluxes, even if we re-calculate L (mol) above.  The problems is, that L
!     (those fluxes) are not consistent with the lowest level (U,T,q). In WRF, 
!     the surface fluxes are calculated first, then the effect (on the lowest
!     model level) of those fluxes, and the dynamics, is calulated.  WRF never
!     goes back to re-diagnose the MO layer, and make (L,Ustar,z0,fluxes) that
!     are consistent with the output lowest level (U,T,q).
!
!     We'll leave this in for now, but may come back to it later.  My tests 
!     show that the calculated 10m speed is very good, but that t10,t2,q2 have
!     biases. Now that we have TWO answers, we have to decide which is right.
!     Is the t2 based solely on last time-step's surface fluxes right, or the
!     t2 based on a re-diagnosis of the MO layer, using the surface and the 
!     lowest model level right?
!
  Tstar = - Ustar*Ustar * ThetaV / vk / g / mol
  Qstar = - Ustar*Ustar * q0     / vk / g / mol
!
!-----Find stratification correction to log-layer profile for 10m level
!
  call stratify(zlev,z10,mol,psim_diff,psih_diff)
!
!----Optionally calculate 10m wind components
!
  if (.not. lu10) then
     call uv2sd(ulev,vlev,1.,0.,.false.,slev,dlev)        ! get speed & direction
     s10 = slev - Ustar/vk * (alog(zlev/z10) - psim_diff) ! 10m speed
     u10 = s10 * cos(dlev * d2r) ! u10,v10 are the vectors, the direction the
     v10 = s10 * sin(dlev * d2r) ! wind is heading, not from which it's blowing.
  end if
!
!-----Always calculate 10m temperature
!
  Theta10 = Theta + pro*shflux/rho/Cp /vk/Ustar * (alog(zlev/z10) - psih_diff)
!  Theta10 = Theta0 - pro*Tstar/vk * (alog(z10/z0) - psit(z10/mol))
  p10 = psfc - g * rho * z10 / 100. ! in mb
  t10 = temp_fn(Theta10,p10,qlev)   ! using qlev is an approximation
!
!-----Find stratification correction to log-layer profile for 2m level
!
  if (.not. lq2 .or. .not. lt2) &
       call stratify(zlev,z2,mol,psim_diff,psih_diff)
!
!-----Optionally calcuate 2m humidity (missing in some versions of MM5)
!
  if (.not. lq2) then
     q2 = qlev + lhflux/rho/L_fn(tlev)/pro/vk/Ustar * (alog(zlev/z2) - psih_diff)
!     q2 = q0 - pro*Qstar/vk * (alog(z2/z0) - psit(z2/mol))
  end if
!
!-----Optionally calculate 2m temperature
!
  if (.not. lt2) then
     Theta2 = Theta + pro*shflux/rho/Cp /vk/Ustar * (alog(zlev/z2) - psih_diff)
!     Theta2 = Theta0 - pro*Tstar/vk * (alog(z2/z0) - psit(z2/mol))
     p2 = psfc - g * rho * z2 / 100. ! in mb
     t2 = temp_fn(Theta2,p2,q2)
  end if

!-----The GNU Fortran compiler (gfortran) complains when z0 is not used
  
  z00 = z0

  return
end subroutine sfc_layer
!
!******************************************************************************
!
subroutine stratify(zIn,zOut,mol,psim_diff,psih_diff)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     Calculates the stratification correction function ratio for winds
!     (Psi-sub-m) and for temperature/humidity (Psi-sub-h) following 
!     Hogstrom (1988).  Ratio is for zIn (known value) to zOut.
!
!     Development History:
!     2012-01-31  New with MMIF v2.1, following MCIPv3.6 pblsup.f90 
!     2014-05-21  Clarified that output is the difference between two levels
!
!------------------------------------------------------------------------------
!
  implicit none
!
!-----Variable declaration
!
! Outputs:
  real, intent(out) :: psim_diff,psih_diff ! stractification corrections
! Inputs:
  real, intent(in)  :: mol               ! Monin-Obukhov length (m)
  real, intent(in)  :: zIn               ! height of lowest MM5/WRF mid-point
  real, intent(in)  :: zOut              ! height to be evaluated at
  real, parameter   :: vk = 0.4          ! von karman's constant
  real, parameter   :: g  = 9.81         ! acceleration due to gravity (m/s^2)
  real, parameter   :: betam =  6.00     ! the rest are from Hogstrom (1988)
  real, parameter   :: betah =  8.21
  real, parameter   :: gamam = 19.30
  real, parameter   :: gamah = 11.60
  real              :: psim,psih,psim0,psih0, x1,x2
!
!-----Entry point
!
  if (zIn/mol >= 0.) then ! stable or neutral
     if ( zIn/mol > 1.0 ) then
        psim0 = 1.0 - betam - zIn/mol
     else
        psim0 = - betam * zIn/mol
     endif
     
     if ( zOut/mol > 1.0 ) then
        psim = psim0 - (1.0 - betam - zOut/mol)
     else
        psim = psim0 + betam * zOut/mol
     endif
  else ! unstable
     x1   = (1.0 - gamam * zIn /mol)**0.25
     x2   = (1.0 - gamam * zOut/mol)**0.25
     psim = 2.0 * ALOG( (1.0+x1) / (1.0+x2) ) +         &
                  ALOG( (1.0+x1*x1) / (1.0+x2*x2)) -    &
          2.0 * ATAN(x1) + 2.0 * ATAN(x2)
  endif
!
!-----Done with Psim, get Psih
!
  if (zIn/mol >= 0.) then ! stable or neutral
     if ( zIn/mol > 1.0 ) then
        psih0 = 1.0 - betah - zIn/mol
     else
        psih0 = -betah * zIn/mol
     endif
     
     if ( zOut/mol > 1.0 ) then
        psih = psih0 - (1.0 - betah - zOut/mol)
     else
        psih = psih0 + betah * zOut/mol
     endif
  else ! unstable
     psih = 2.0 * ALOG( (1.0 + SQRT(1.0 - gamah * zIn /mol)) /  &
                        (1.0 + SQRT(1.0 - gamah * zOut/mol)) )
  endif

  psim_diff = psim
  psih_diff = psih

  return
end subroutine stratify
!
!******************************************************************************
!
real function psiu(zL)
!
!------------------------------------------------------------------------------
!     AERCOARE
!     VERSION 1.0 2012-10-01
!
!     psiu and psit evaluate stability function for wind speed and scalars
!     matching Kansas and free convection forms with weighting f
!     convective form follows Fairall et al (1996) with profile constants
!     from Grachev et al (2000) BLM
!     stable form from Beljaars and Holtslag (1991)
!
!     Development History:
!     adapted from cor3_0f.for acccessed at:
!     ftp://ftp1.esrl.noaa.gov/users/cfairall/wcrp_wgsf/computer_programs/cor3_0/
!     2012-10-01  ENVIRON International
!
!------------------------------------------------------------------------------
!
  implicit none
  real*8 zL,x,y,psik,psic,f,c

  if (zL < 0) then
     x    = (1 - 15.*zL)**.25                        !Kansas unstable
     psik = 2.*dlog((1. + x)/2.) + dlog((1. + x*x)/2.) - 2.*datan(x) &
          + 2.*datan(1.d0)
     y    = (1. - 10.15*zL)**.3333                   !Convective
     psic = 1.5*dlog((1. + y + y*y)/3.)                &
          - dsqrt(3.d0)*datan((1. + 2.*y)/dsqrt(3.d0)) &
          + 4.*datan(1.d0)/dsqrt(3.d0)
     f    = zL*zL/(1. + zL*zL)
     psiu = real( (1. - f)*psik + f*psic )
  else
     c    = min(50.d00,0.35*zL)                      !Stable
     psiu = real( -((1. + 1.*zL)**1. + .6667*(zL - 14.28)/dexp(c) + 8.525) )
  endif

  return
end function psiu
!
!******************************************************************************
!
real function psit(zL)
!
!------------------------------------------------------------------------------
!     AERCOARE
!     VERSION 1.0 2012-10-01
!
!     psiu and psit evaluate stability function for wind speed and scalars
!     matching Kansas and free convection forms with weighting f
!     convective form follows Fairall et al (1996) with profile constants
!     from Grachev et al (2000) BLM
!     stable form from Beljaars and Holtslag (1991)
!
!     Development History:
!     adapted from cor3_0f.for acccessed at:
!     ftp://ftp1.esrl.noaa.gov/users/cfairall/wcrp_wgsf/computer_programs/cor3_0/
!     2012-10-01  ENVIRON International
!
!------------------------------------------------------------------------------
!
  real*8 zL,x,y,psik,psic,f,c

  if (zL < 0) then
     x    = (1. - 15.*zL)**.5                          !Kansas unstable
     psik = 2.*dlog((1. + x)/2.)
     y    = (1. - 34.15*zL)**.3333                    !Convective
     psic = 1.5*dlog((1. + y + y*y)/3.)                &
          - dsqrt(3.d0)*datan((1. + 2.*y)/dsqrt(3.d0)) &
          + 4.*datan(1.d0)/dsqrt(3.d0)
     f    = zL*zL/(1. + zL*zL)
     psit = real( (1. - f)*psik + f*psic )
  else
     c    = min(50.d00,0.35*zL)                        !Stable
     psit = real( -((1. + 2.*zL/3.)**1.5 + .6667*(zL - 14.28)/dexp(c) + 8.525) )
  endif

  return
end function psit
!
!******************************************************************************
!
