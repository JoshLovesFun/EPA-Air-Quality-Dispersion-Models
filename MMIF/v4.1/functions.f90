MODULE functions
!
! Contains:
! real function es_fn(T)              ! saturation vapor pressure, mb
! real function qs_fn(T,p)            ! saturation specific humidity, kg/kg
! real function dewpoint_fn(RH,T)     ! dew point temperature, K
! real function Theta_fn(T,p,q)       ! potential temperature, K
! real function Temp_fn(Theta,p,q)    ! invserse of Theta_fn, K
! real function density_fn(T,p,q)     ! air density, kg/m**3
! real function L_fn(T,Bad)           ! latent heat of vaporization, J/kg
! subroutine uv2sd(u,v,lon,MET_convention,speed,dir) ! convert wind U,V to 
!                                                      Speed, Direction
! subroutine uppercase(string)        ! convert lowercase to uppercase
! subroutine uppercase_filename(string) ! uppercase just the filename, not path
! subroutine print_met_levels         ! print the MM5/WRF vertical levels
! subroutine grid_in_grid(iBeg,iEnd,jBeg,jEnd,nx,ny) ! verify sub-grid range
! subroutine point_in_grid(i,j,iMin,iMax,jMin,jMax,model,iOut) ! verify point
!
!--------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     Useful functions, mostly thermodynamic.
!
!     Development History:
!     2011-09-30  New with MMIF v2.0.  Mostly taken from routines written
!     by Bart Brashers during his PhD work.  Some functions new with MMIFv2.0.
!     2011-12-12  Minor fix: clear up confusion between central and std lat/lon
!     2012-02-21  uv2sd now supports wind direction in MET convention 
!                 (0 deg is North)
!     2013-05-02  Added more sanity checks to inside_grid(), which gets called
!                 by both read_mm5() and read_wrf() after iBeg etc. calculated.
!     2013-07-15  Added point_in_grid() checking routine, and renamed 
!                 inside_grid() to grid_in_grid().
!     2014-03-06  Renamed uv2sd to uv2sd_lcc, and added a more general version
!                 of uv2sd that supports multiple projections (LCC,PS,EM).
!     2015-07-24  Uppercase just the filename, not path (for ONSITE output).
!
!--------------------------------------------------------------------------
!
! Global variables:
!
  real, parameter :: pi = 3.14159265
  real, parameter :: d2r = pi/180.

CONTAINS
!
!--------------------------------------------------------------------------
!
  real function es_fn(T) ! mb

!     Finds saturation water vapor pressure as a function of T,
!     relative to liquid water (T > -40 C) or ice (T < -40 C).

    real    T                 ! input Temperature (C or K)
    real    Tc                ! temperature in C
    real    Tk                ! temperature in Kelvin

    Tc = T
    if (T  .gt. 100.) Tc = T  - 273.15 ! Tc in C
    Tk = Tc + 273.15                   ! Tk in K

!   Some functions for saturated vapor pressure over water and ice
!
!      esw(Tk) = 6.112 * exp(17.67 * (Tk - 273.15) / (Tk - 29.65) ) 
!      esi(Tk) = 6.1078 * 10** (9.32 * (Tk - 273.16) / (Tk - 11.92) ) 
!      esw(T) = 6.112 * exp( 17.67 * T / (T + 243.5) ) 
!      esw(Tc) = 6.1078 * 10**( 7.5 * Tc / (Tc + 237.3))
!
!   Tetens' formula for saturation vp Buck(1981) JAM 20, 1527-1532 
!     
!      es(Tc,p) = (1.0007+3.46e-6*P)*6.1121*dexp(17.502*Tc/(240.97+Tc))
!
!   See also ftp://ncardata.ucar.edu/docs/equations/moisture

    if (Tc .ge. -40.) then
       es_fn = 6.1078 * 10**( 7.5 * Tc / (Tc + 237.3) ) ! over water (mb)
    else
       es_fn = 6.1078 * 10**( 9.5 * Tc / (Tc + 265.5) ) ! over ice (mb)
    end if

    return

  end function es_fn
!
!--------------------------------------------------------------------------
!
  real function qs_fn(T,p)    ! kg/kg

!     Finds saturation specific humidity as a function of p and T.

    real    p,T               ! Pressure (mb) and Temperature (C or K)
    real    es                ! saturation vapor pressure (mb)

    es = es_fn(T)             ! over water or ice, depends on T in es_fn().
    qs_fn = 0.622 * es / (p - 0.378*es)

    return
  end function qs_fn
!
!--------------------------------------------------------------------------
!
  real function dewpoint_fn(RH,T) ! K

!     From http://meted.ucar.edu/awips/validate/dewpnt.htm.
!
    real    RH                ! relative Humidity, 1 < RH < 100. (percent)
    real    T                 ! temperature (C or K)
    real    e                 ! vapor pressure (mb)
    real, parameter :: c15 = 26.66082
    real, parameter :: c3  = 223.1986
    real, parameter :: c4  = 0.0182758048

    Tc = T
    if (T .gt. 100.) Tc = T  - 273.15  ! Tc in C 
    Tk = Tc + 273.15                   ! Tk in K

    R = min(1., RH / 100.) ! require RH <= 100%
    R = max(0.001,R)       ! require RH >= 0.1%, else math below NaN's

    e  = R * es_fn(Tk)
    b = c15 - log(e)

    dewpoint_fn = (b - sqrt(b*b - c3)) / c4

    return

  end function dewpoint_fn
!
!--------------------------------------------------------------------------
!
  real function Theta_fn(T,p,q) ! K
!
!     Finds potential temperature as a function of T, p, and q
!
    real    T                 ! Temperature (C or K)
    real    p                 ! pressure (mb)
    real    q                 ! specific humidity (kg/kg)
    real    Tc                ! temperature in C
    real    Tk                ! temperature in Kelvin
    real    w                 ! mixing ratio (kg/kg)
    parameter (gamma=.2854)   ! Rd/Cp
    
    Tc = T
    if (T  .gt. 100.) Tc = T - 273.15 ! Tc in C
    Tk = Tc + 273.15                  ! Tk in K
    
    w = q / (1 - q)
      
!   function for Potential Temperature (from Bolton 1980, eqn 7)

    Theta_fn = Tk * (1000./p)**(gamma*(1 - .28*w)) ! (K)

    return
  end function Theta_fn
!
!--------------------------------------------------------------------------
!
  real function Temp_fn(Theta,p,q) ! K
!
!     Finds temperature as a function of Theta, p, and q 
!       (the inverse of Theta_fn)
!
    real    Theta             ! Potential Temperature (K)
    real    p                 ! pressure (mb)
    real    q                 ! specific humidity (kg/kg)
    real    w                 ! mixing ratio (kg/kg)
    parameter (gamma=.2854)   ! Rd/Cp
    
    w = q / (1 - q)
      
!  Function for Temperature (inverse of previous function)

    Temp_fn = Theta * (p/1000.)**(gamma*(1 - .28*w)) ! (K)

    return
  end function Temp_fn
!
!--------------------------------------------------------------------------
!
  real function density_fn(T,p,q) ! kg/m**3
!
!     Finds density as a function of T, p, and q 
!
    real    T                 ! Temperature (C or K)
    real    p                 ! pressure (mb)
    real    q                 ! specific humidity (kg/kg)
    real    Tc                ! temperature in C
    real    Tk                ! temperature in Kelvin
    real    w                 ! mixing ratio (kg/kg)
    parameter (Rd=287.04)     ! ideal gas const for dry air

    Tc = T
    if (T  .gt. 100.) Tc = T  - 273.15 ! Tc in C now
    Tk = Tc + 273.15                   ! Tk in K
    
    w = q / (1 - q)
      
!     function for Density (kg/m^3)

    density_fn = p*100. / Rd / Tk / (1+.6077*w) ! 100 is mb to Pa

    return
  end function density_fn
!
!--------------------------------------------------------------------------
!
  real function L_fn(T) ! J / kg
!
!     Finds the latent heat of vaporization as a function of T
!
    real    T                 ! Temperature (C or K)
    real    Tc                ! Temperature in C

    Tc = T
    if (T  .gt. 100.) Tc = T  - 273.15 ! Tc in C now

    L_fn =  2.501e6 - 2.37e3*Tc ! J / kg

    return
  end function L_fn

!
!--------------------------------------------------------------------------
!
  subroutine uv2sd(u,v,cosalpha,sinalpha,met_convention,speed,dir)
!
! Converts U and V (vector components) to speed and direction.
! if cosalpha = 1 and sinalpha = 0, then no rotation is performed, 
! otherwise rotate the winds from the local projection to E-N.
!
    implicit none
!
!-----Variable declaration
!
    real    :: u,v               ! Input vector components
    real    :: cosalpha,sinalpha ! local cosine/sine of map rotation
    real    :: speed,dir         ! Output speed and direction
    logical :: met_convention    ! True for direction wind is coming from, 
                                 ! false for direction wind is flowing (vector).
    real    :: u_rot,v_rot       ! Local: rotated wind components
!
!-----Entry point
!
    speed = sqrt(u**2 + v**2)

! Rotate the WRF winds from grid-relative to earth-relative.  This works
! for all WRF projections, LCC, PS, EM, whatever.

    u_rot = u * cosalpha - v * sinalpha
    v_rot = v * cosalpha + u * sinalpha

    dir   = atan2(v_rot,u_rot) / d2r     ! in degrees, not radians

    if (met_convention) dir = 270. - dir ! From the North is 0 degrees, etc.

    if (dir <   0.) dir = dir + 360.
    if (dir > 360.) dir = dir - 360.

  end subroutine uv2sd
!
!--------------------------------------------------------------------------
!
  subroutine uv2sd_lcc(u,v,lon,met_convention,speed,dir)
!
! Converts U and V (vector components) to speed and direction.
! If lon > -999., rotate the winds from the local projection to E-N.
!
    USE met_fields            ! need only cenlon and conefact
    implicit none
!
!-----Variable declaration
!
    real    :: u,v,lon        ! Vector components, location's Longitude
    real    :: speed,dir      ! Output speed and direction
    real    :: lon_diff       ! Diff between point and std longitude
    logical :: met_convention ! True for direction wind is coming from, 
                              ! false for direction wind is flowing (vector).
!
!-----Entry point
!
    speed = sqrt(u**2 + v**2)
    dir   = atan2(v,u) / d2r  ! in degrees, not radians

    if (lon > -999.) then

       lon_diff = lon - cenlon
       if ( lon_diff >  180. ) lon_diff = lon_diff - 360.
       if ( lon_diff < -180. ) lon_diff = lon_diff + 360.

       dir = dir + lon_diff * conefact * sign(1.,cenlat)

    end if

    if (met_convention) dir = 270. - dir ! From the North is 0 degrees, etc.

    if (dir <   0.) dir = dir + 360.
    if (dir > 360.) dir = dir - 360.

  end subroutine uv2sd_lcc
!
!--------------------------------------------------------------------------
!
  subroutine uppercase(string)
!
! Converts a string to uppercase.
!
    implicit none
!
!-----Variable declaration
!
    character :: string*(*)
    integer   :: i   ! index of the string
    integer   :: ich ! ascii number
    
    do i = 1, len(string)
       ich = ichar(string(i:i))
       if (ich >= 97 .and. ich <= 122) string(i:i) = char(ich-32)
    end do

    return

  end subroutine uppercase
!
!--------------------------------------------------------------------------
!
  subroutine uppercase_filename(string)
!
! Converts just the last part (past the last '/' or '\') to uppercase.
!
    implicit none
!
!-----Variable declaration
!
    character :: string*(*)  ! input filename
    integer   :: i           ! index of the string
    integer   :: ich         ! ascii number
    integer   :: islash      ! index of the last '/' or '\'
    integer   :: ibase       ! index of the 1st char of the filename
    character (len=1) :: slash,backslash ! for Linux vs. DOS paths

    slash     = char(47) ! AKA "forward slash"
    backslash = char(92)

    islash = index(string,slash,.true.)          ! look for a path ending in "/"
    if (islash == 0) &
         islash = index(string,backslash,.true.) ! look for a path ending in "\"
    ibase = islash + 1                           ! islash might be zero (no path)

    if (ibase < len(string)) then
       do i = ibase, len(string)
          ich = ichar(string(i:i))
          if (ich >= 97 .and. ich <= 122) string(i:i) = char(ich-32)
       end do
    end if

    return

  end subroutine uppercase_filename
!
!--------------------------------------------------------------------------
!
  subroutine print_met_levels
!
! Print vertical MM5 or WRF levels.  calmet_header() does this, but other
! output formats might find this information useful.
!
    USE met_fields
    implicit none
!
!-----Variable declaration
!
    integer   :: i,j,k
    real      :: zmean, zlast ! don't use zavg, it's in met_fields

    zlast = 0.
    write(*,'(a)') "   Level         Sigma  Interface(m)     Center(m)"
    do k = 0, nz
       zmean = 0.
       do j = jBeg,jEnd
          do i = iBeg,iEnd
             zmean = zmean + zh(i,j,k) ! zh supplied by MM5/WRF
          enddo
       enddo
       zmean = zmean / ((iEnd-iBeg+1)*(jEnd-jBeg+1)) ! mean

       if (k == 0) then
          write(*,'(i8,f14.6,2f14.2)') k,sigma(k),zmean
       else
          write(*,'(i8,f14.6,2f14.2)') k,sigma(k),zmean,(zmean+zlast)/2.
       endif

       zlast = zmean
    end do
    write(*,*)

  end subroutine print_met_levels
!
!--------------------------------------------------------------------------
!
  subroutine grid_in_grid(iBeg,iEnd,jBeg,jEnd,nx,ny,model)
!
! Verify that the requested sub-grid is inside the 1:nx, 1:ny range, and
! stop with an error if it's outside the correct range.  This is also a
! good place to do some sanity checks that the iBeg, etc. are OK.
!
    implicit none
!
!-----Variable declaration
!
    integer   :: iBeg,iEnd,jBeg,jEnd,nx,ny
    character (len=3) :: model
!
!-----Entry point
!
    if (iBeg < 1) then
       write(*,*) "*** Error: Requested Left Corner < ",model," minimum -- Stopping."
       write(*,*) "           Requested Imin =",iBeg," < 1"
       write(*,*) '    Program stopping in subroutine grid_in_grid().'
       stop
    endif

    if (iEnd < 1) then
       write(*,*) "*** Error: Requested Right Corner < ",model," minimum -- Stopping."
       write(*,*) "           Requested Imax =",iEnd," < 1"
       write(*,*) '    Program stopping in subroutine grid_in_grid().'
       stop
    endif

    if (jBeg < 1) then
       write(*,*) "*** Error: Requested Lower Corner < ",model," minimum -- Stopping."
       write(*,*) "           Requested Jmin =",jBeg," < 1"
       write(*,*) '    Program stopping in subroutine grid_in_grid().'
       stop
    endif

    if (jEnd < 1) then
       write(*,*) "*** Error: Requested Upper Corner < ",model," minimum -- Stopping."
       write(*,*) "           Requested Jmax =",jEnd," < 1"
       write(*,*) '    Program stopping in subroutine grid_in_grid().'
       stop
    endif

    if (iBeg > nx) then
       write(*,*) "*** Error: Requested Left Corner > ",model," maximum -- Stopping."
       write(*,*) "           Requested Imin =",iBeg," > ",nx
       write(*,*) '    Program stopping in subroutine grid_in_grid().'
       stop
    endif

    if (iEnd > nx) then
       write(*,*) "*** Error: Requested Right Corner > ",model," maximum -- Stopping."
       write(*,*) "           Requested Imax =",iEnd," > ",nx
       write(*,*) '    Program stopping in subroutine grid_in_grid().'
       stop
    endif

    if (jBeg > ny) then
       write(*,*) "*** Error: Requested Lower Corner > ",model," maximum -- Stopping."
       write(*,*) "           Requested Jmin =",jBeg," > ",ny
       write(*,*) '    Program stopping in subroutine grid_in_grid().'
       stop
    endif
    
    if (jEnd > ny) then
       write(*,*) "*** Error: Requested Upper Corner > ",model," maximum -- Stopping."
       write(*,*) "           Requested Jmax =",jEnd," > ",ny
       write(*,*) '    Program stopping in subroutine grid_in_grid().'
       stop
    endif

    if (iBeg > iEnd) then
       write(*,*)
       write(*,*) '*** Error: Beginning I coordinate > ending I coordinate.'
       write(*,*) '           Requested Imin, Imax = ',iBeg,iEnd
       write(*,*) '    Program stopping in grid_in_grid .'
       stop
    end if

    if (jBeg > jEnd) then
       write(*,*)
       write(*,*) '*** Error: Beginning J coordinate > ending J coordinate.'
       write(*,*) '           Requested Jmin, Jmax = ',jBeg,jEnd
       write(*,*) '    Program stopping in subroutine grid_in_grid().'
       stop
    endif

  end subroutine grid_in_grid
!
!--------------------------------------------------------------------------
!
  subroutine point_in_grid(i,j,iMin,iMax,jMin,jMax,iOut)
!
! Verify that the requested point is inside the output 3D sub-grid, and
! stop with an error if it's outside the correct range.  This is also a
! good place to do some sanity checks that the iPt(iOut) are ok.
!
    implicit none
!
!-----Variable declaration
!
    integer   :: i,j,iMin,iMax,jMin,jMax,iOut
!
!-----Entry point
!
    if (i < iMin) then
       write(*,'(a,i6,a)') " *** Error: Requested point for output number ", &
            iOut," is outide the"
       write(*,*) "           requested 3-D output domain."
       write(*,*) "           Requested I =",i," < ",iMin
       write(*,*) '    Program stopping in point_in_grid.'
       stop
    endif

    if (j < jMin) then
       write(*,'(a,i6,a)') " *** Error: Requested point for output number ", &
            iOut," is outide the"
       write(*,*) "           requested 3-D output domain."
       write(*,*) "           Requested J =",j," < ",jMin
       write(*,*) '    Program stopping in point_in_grid.'
       stop
    endif

    if (i > iMax) then
       write(*,'(a,i6,a)') " *** Error: Requested point for output number ", &
            iOut," is outide the"
       write(*,*) "           requested 3-D output domain."
       write(*,*) "           Requested I =",i," > ",iMax
       write(*,*) '    Program stopping in point_in_grid.'
       stop
    endif

    if (j > jMax) then
       write(*,'(a,i6,a)') " *** Error: Requested point for output number ", &
            iOut," is outide the"
       write(*,*) "           requested 3-D output domain."
       write(*,*) "           Requested J =",j," > ",jMax
       write(*,*) '    Program stopping in point_in_grid.'
       stop
    endif
    
  end subroutine point_in_grid

END MODULE functions
