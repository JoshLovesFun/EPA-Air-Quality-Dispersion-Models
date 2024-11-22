! Contains:
!
! subroutine pg_srdt(swd,t2,tlev,u,v,kst)
! subroutine pg_golder(mol,z0in,istab)
! subroutine ltopg(sfcz0,obulen,istab) ! from MMIFv2.0, not used in MMIFv2.1
! 
subroutine pg_srdt(swd,t2,tlev,u,v,kst)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     Computes the Pasquill-Gifford (PG) stability category using the SRDT 
!     estimation scheme.  The scheme uses solar radiation (watts/m^2) and wind
!     speed (m/s) during the day, and delta-t (deg-C) and wind speed at night.
!
!     Taken from code developed in part from:
!         J. Paumeir, 7/27/95
!         D. Bailey, 12/14/95
!
!     Development History:
!     2009-05-26  Original Development (EPA/Region 7)
!     2011-09-30  Minor code cleanup
!     2012-01-31  Renamed subroutine from pgstb() to srdt().
!                 Changed delt from t10-t0 to tt(i,j,1)-t2, to follow guidance,
!                 and remove dependence on t10 (a calculated variables).  It
!                 is only used at night, and then only the sign is used, so we
!                 don't need to be too precise about it.
!
!------------------------------------------------------------------------------
!
  implicit none
!
!-----Variable declaration
!
  integer lsrdt(7,7),kst,ii,jj
  real swd,u,v,ws,solar,t2,tlev,delt,wsl(5),srl(3)
!
!-----Data statements
!
  data wsl /2.,2.5,3.,5.,6./
  data srl /925.,675.,175./
!
!-----Define stability class as a function of wind speed (index 1) and
!     either solar radiation in the day or temperature gradient in the
!     night (index 2).  "0" indicates an undefined condition, which is
!     keyed to either II=7 or JJ=7.

  data lsrdt /1, 1, 1, 2, 3, 3, 0,   &
              1, 2, 2, 2 ,3, 4, 0,   &
              2, 3, 3, 3, 4, 4, 0,   &
              4, 4, 4, 4, 4, 4, 0,   &
              5, 4, 4, 4, 4, 4, 0,   &
              6, 5, 4, 4, 4, 4, 0,   &
              0, 0, 0, 0, 0, 0, 0/
!
!-----Entry point
!
  ws = sqrt(u**2 + v**2)
  delt = tlev - t2
!
!     Initialize stability before it is calculated
!
  kst = 0
!
!-----Process solar radiation and delta-t data
!     Set solar radiation if hour is during the day (-999. if night).
!     Use RAMMET convention for determining daytime period.
!
  solar = -999.
  if (swd > 0.) solar = swd
!
!-----Determine index values for stability class table.
!     Note that wind speed less than or equal to zero is not
!     expected, and solar radiation less than zero is not expected,
!     so any such values produce an undefined stability class.
!
  ii = 7
  if (ws >= 0.0)    ii = 1
  if (ws >= wsl(1)) ii = 2
  if (ws >= wsl(2)) ii = 3
  if (ws >= wsl(3)) ii = 4
  if (ws >= wsl(4)) ii = 5
  if (ws >= wsl(5)) ii = 6
!
!-----Set the solar radiation index (day) or the temperature
!     gradient index (night) : jj
!
  if (solar /= -999.) then           ! Day
     jj = 7
     if (solar >= 0.0)    jj = 4
     if (solar >= srl(3)) jj = 3
     if (solar >= srl(2)) jj = 2
     if (solar >= srl(1)) jj = 1
  else                               ! Night
     jj = 5
     if (delt >= 0.0)     jj = 6
  endif
!
!-----Set stability index
!
  kst = lsrdt(ii,jj)

  return
end subroutine pg_srdt
!
!******************************************************************************
!
subroutine pg_golder(mol,z0in,istab)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     Converts Monin-Obukhov length to PG stability class, based on
!     Golder, 1972: "Relations among stability parameters in the surface 
!     layer", Boundary-Layer Meteorology, 3:56.
!
!     Development History:
!     2012-01-31  New with MMIF v2.1, following CTDMplus's LSTAB routine,
!                 which is also used by CALPUFF.
!
!------------------------------------------------------------------------------
!
  implicit none
!
!-----Variable declaration
!
! Outputs:
  integer, intent(out) :: istab  ! PG Stablity Class
! Inputs:
  real,    intent(in)  :: mol    ! Monin-Obukhov lenght (m)
  real,    intent(in)  :: z0in   ! Roughness length (m)
! Local:
  real                 :: z0     ! z0 limited to be 0.01 <= z0 <= 0.5 m
!
!-----Entry point
!
!-----Limit z0 to be within applicable range
!
  z0 = max(0.01,z0in)
  z0 = min(0.5, z0)
!
!-----Do the 
!
  if (mol < 0.) then
     if (      mol >=   70.0 / ( alog(z0) - 4.350)) then
        istab = 1 ! Stability A
     else if ( mol >=   85.2 / ( alog(z0) - 0.502)) then
        istab = 2 ! Stability B
     else if ( mol >=  245.0 / ( alog(z0) - 0.050)) then
        istab = 3 ! Stability C
     else
        istab = 4 ! Stability D
     endif
  else ! MOL >= 0.
     if (      mol >= -327.0 / ( alog(z0) - 0.627)) then
        istab = 4 ! Stability D
     else if ( mol >=  -70.0 / ( alog(z0) - 0.295)) then
        istab = 5 ! Stability E
     else
        istab = 6 ! Stability F
     endif

  endif

  return
end subroutine pg_golder
!
!******************************************************************************
!
subroutine ltopg(sfcz0,obulen,istab)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     Converts Monin-Obukhov length to PG stability class, based on
!     Golder (1972).
!
!     NOTE: this routine taken from AERMOD (R. Brode, 11/21/97).
!
!     Development History:
!     2009-05-26  Original Development (EPA/Region 7)
!     2012-01-31  Tested and discarded after it didn't match either
!                 Fig 4 or 5 in the 1972 paper.
!
!------------------------------------------------------------------------------
!
  implicit none
!
!-----Variable declaration
!
  integer :: istab
  real    :: sfcz0,lnz0,lnz02,obulen,obuinv
  real    :: ab,bc,cd,de,ef,aa,bb,cc,dd,ee,ff
!
!-----Entry point
!     Initialize local variables
!
  lnz0 = alog(sfcz0)
  lnz02 = lnz0*lnz0
  aa = -0.1360107  + 0.0118433*lnz0   + 0.00021242*lnz02
  bb = -0.08608128 + 0.0118433*lnz0   + 0.00021242*lnz02
  cc = -0.0390887  + 0.009030514*lnz0 - 0.0005869182*lnz02
  dd = -0.0116834  + 0.00182343*lnz0  - 0.000002247867*lnz02
  ee = -dd
  ff = -cc
!
!-----Interpolate to get 1./L values to define boundaries between
!     stability classes.
!
  ab = (aa + bb)/2.
  bc = (bb + cc)/2.
  cd = (cc + dd)/2.
  de = (dd + ee)/2.
  ef = (ee + ff)/2.
!
!-----Calculate stability class ISTAB
!
  obuinv = 1./obulen

  if (obuinv .le. ab) then
     istab = 1
  else if (obuinv .le. bc) then
     istab = 2
  else if (obuinv .le. cd) then
     istab = 3
  else if (obuinv .le. de) then
     istab = 4
  else if (obuinv .le. ef) then
     istab = 5
  else
     istab = 6
  end iF

  return
end subroutine ltopg
