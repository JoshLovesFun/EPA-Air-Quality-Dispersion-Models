subroutine interpolate(SCICHEM)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     Interpolates MM5/WRF vertical levels to OUTPUT levels.
!     Inputs are uu,vv,tt,pa,qq,qc, outputs are uOut,vOut,tOut,pOut,qOut,qcOut.
!
!     Development History:
!     2011-09-30  New with MMIF v2.1 (ENVIRON).
!     2012-02-21  Account for uv2sd() changes in support of AERMOD modes.
!     2013-01-22  Bug fix: interpolate to _transformed_ layers for SCICHEM.
!     2014-03-06  New version of uv2sd to support PS/EM projections.
!
!------------------------------------------------------------------------------
!
  USE met_fields
  USE functions
  implicit none
!
!-----Variable declaration
!
  logical, intent(in) :: SCICHEM   ! use SCICHEM's transformed layers?
  real    :: sigma_bot, sigma_top  ! output layer heights in sigma coords
  integer :: i,j,k                 ! indexes or output variables
  integer :: b,m,t                 ! bottom, middle and top indexes
  real    :: zlow,spd,dir,Theta    ! values at lowest MM5/WRF level
  real    :: zl,sl,thl             ! values of log-layer profile at zl
  real    :: SumIn, SumOut         ! integrate T(z) for in and out soundings
  real               :: Cp         ! specific heat (J/K/kg)
  real               :: L          ! latent heat of vaporization (J/K/kg)
  real               :: rho        ! air density (kg/m^3)
  real               :: psim,psih  ! stability corrections for wind and T/Q
  real,    parameter :: vk = 0.4   ! von karman's constant
  real,    parameter :: g  = 9.8   ! acceleration due to gravity (m/s^2)
  real,    parameter :: pro = 0.95 ! from Hogstrom (1988)
  real,    parameter :: sml = 1.e-7! small number
!
!-----Entry point
!
  do j = jBeg,jEnd
     do i = iBeg,iEnd
!
!-----Initialize some values
!
        sumOut = 0.      ! Integral of Output Temperature sounding
        wOut(i,j,0) = 0. ! zero vertical velocity at ground level
        sigma_bot = 1.   ! start at the ground, which is always sigma = 1
        b = 1 ! b = index of the input level above bottom of output layer
        t = 1 ! t = index of the input level above top    of output layer

!-----If requested output is for SCICHEM, we need to transform the output 
!     heights for interpolation, following Sections 10.3.2 and 13.2.2 of the 
!     SCIPUFF Technical Documentation, or Section 10.3.2 of the SCICHEM 
!     Technical Document (which lacks the details on the MEDOC file format 
!     that can be found in Section 13.2.2 of the SCIPUFF Tech Doc).  
!     The user has specified what the SCICHEM Tech Doc calls "SZ" (or sigma),  
!     we've stored those levels in zmid (eq. zface).  For each grid point (i,j),
!     we need to find the transformed levels, and interpolate to those levels,
!     following this formula:
!     
!        Z(i,j,k) = SZ(k) * [1 - topo(i,j)/H] 
! 
!     where 
!
!        Z is the height above ground.  Note: the SCICHEM Tech Doc uses "z"
!             to mean the height above mean sea level, so Z = z - h, where
!             "h" is the elevation, i.e. topo(i,j).
!        H = SZ(kmax).  H is written as D in the SCICHEM Tech Doc. H is the
!             level where the vertical velocity is zero.
!        SZ is the un-transformed height, the user-requested heights.
!
!     Note that the actual (un-transformed) levels we're interpolation to
!     will change with elevation (location).  This is a requirement of SCICHEM,
!     a fundamental aspect of the model -- all calculations are done in this
!     transformed coordinate system.  
!
        if (SCICHEM) then
           do k = 0,nzOut
              zfaceS(k) = zface(k) * (1. - topo(i,j)/zmid(nzOut))
           end do
        else
           zfaceS = zface       ! all other models do not use transformed levels
        end if

        do k = 1,nzOut
!
!-----Find t, the index of the input leval above the top of the output layer.
!     We already know b, the index of the level just above the bottom of the
!     output layer -- either because it's the ground, or from last time 
!     through this loop.  
!
           do while (zh(i,j,t) < zfaceS(k) .and. t < nz)
              t = t + 1
           end do
           if (t == nz .and. zh(i,j,t) < zfaceS(k)) then
              write(*,*) 
              write(*,*) "*** Error: Top of OUTPUT domain is higher than ",  &
                   "top of MM5/WRF domain."
              write(*,*) "           Impossible to interpolate.  Pick a ",   &
                   "lower value for the"
              write(*,*) "           top level, or output fewer layers."
              write(*,*) "           Current height is ",zh(i,j,t)
              write(*,*) "    Stopping!"
              write(*,*) 
              stop
           end if
!
!-----Calculate the sigma values of the zface (zfaceS) heights
!
           sigma_top = sigma(t-1) + (sigma(t)-sigma(t-1)) * &
                (zfaceS(k)-zh(i,j,t-1))/(zh(i,j,t)-zh(i,j,t-1)) ! linear interp

!-----Case 1: If the output layer is fully inside the lowest input layer, then
!     evaluate the log-layer profiles between the known lower boundary to the
!     top of the output layer.  All the state variables except Qcloud have 
!     known values at the surface.  Because there is no vertical flux of 
!     Qcloud, its profile will not be logrythmic.  Just treat Qcloud as a 
!     constant within the layer.
!
!     Case 2: Output layer fully inside a single input layer
!
!           area      = (sigma_top - sigma_bottom) * x(b)
!
!     But when you normalize, you'd just divide by delta-sigma again, so
!     skip it to avoid numerical problems and run faster.
!
!     Case 3: Output layer spans at least two (top and bottom) and maybe
!     more layers in between (middle)
!
!           top    area = (sigma_top - sigma(t-1)) * x(t)
!           middle area = sigma                    * x    (for each layer)
!           bottom area = (sigma(b) - sigma_bot)   * x(b)
!
!     Sum the area inside the output "box" 
!
!----- Case 1: in log layer
!
           if (t == 1 .and. sigma_top > sigma(t)+sml) then

              zlow = zh(i,j,1)/2.     ! mid-point of lowest input layer
              call uv2sd(uu(i,j,1),vv(i,j,1),1.,0.,.false.,spd,dir) ! speed & dir
              Cp = 1004.67 * (1.0 + 0.84 * qq(i,j,1)) ! specific heat
              L = L_fn(tt(i,j,1))                     ! latent heat
              Theta  = Theta_fn(tt(i,j,1),pa(i,j,1),qq(i,j,1))   ! potential temp
              rho    = density_fn(tt(i,j,1),pa(i,j,1),qq(i,j,1)) ! air density

              zl = 0.5*(zfaceS(k)+zfaceS(k-1)) ! mid-point of output layer
              call stratify(zlow,zl,mol(i,j),psim,psih)   ! eval at this height
              sl  = spd - ustar(i,j)/vk * (alog(zlow/zl) - psim) ! speed
              uOut(i,j,k) = sl * cos(dir * d2r)           ! no turning in 
              vOut(i,j,k) = sl * sin(dir * d2r)           ! the sfc layer
              qOut(i,j,k) = qq(i,j,1) + pro*lhflux(i,j)/rho/L  /vk/ustar(i,j) * &
                   (alog(zlow/zl)-psih)
              Thl         = Theta + pro*shflux(i,j)/rho/Cp /vk/ustar(i,j) * &
                   (alog(zlow/zl)-psih)
              tOut(i,j,k) = temp_fn(Thl,pa(i,j,1),qq(i,j,1)) 
!
!-----Qcloud is not logrythmic, just constant
!
              qcOut(i,j,k) = qcloud(i,j,t)
!
!-----Pressure in log layer is vertically interpolated, using sigma weights
!
              pOut(i,j,k)  = psfc(i,j) + (pa(i,j,1)-psfc(i,j)) *     &
                (sigma(t-1)-sigma_top)/(sigma(t-1)-sigma(t))
!
!-----Case 2: output layer fully inside input layer
!
           else if (t == b) then

              uOut(i,j,k)  = uu(i,j,t) ! top
              vOut(i,j,k)  = vv(i,j,t)
              tOut(i,j,k)  = tt(i,j,t)
              pOut(i,j,k)  = pa(i,j,t)
              qOut(i,j,k)  = qq(i,j,t)
              qcOut(i,j,k) = qcloud(i,j,t)
!
!-----Case 3: output layer fully spans 1+ input level(s)
!
           else ! t > b       

              uOut(i,j,k)  = (sigma_bot - sigma(b)) * uu(i,j,b) ! bottom
              vOut(i,j,k)  = (sigma_bot - sigma(b)) * vv(i,j,b)
              tOut(i,j,k)  = (sigma_bot - sigma(b)) * tt(i,j,b)
              pOut(i,j,k)  = (sigma_bot - sigma(b)) * pa(i,j,b)
              qOut(i,j,k)  = (sigma_bot - sigma(b)) * qq(i,j,b)
              qcOut(i,j,k) = (sigma_bot - sigma(b)) * qcloud(i,j,b)
              if (b+1 <= t-1) then                              ! any middle?
                 do m = b+1, t-1
                    uOut(i,j,k)  = uOut(i,j,k)  + (sigma(m-1)-sigma(m))*uu(i,j,m)
                    vOut(i,j,k)  = vOut(i,j,k)  + (sigma(m-1)-sigma(m))*vv(i,j,m)
                    tOut(i,j,k)  = tOut(i,j,k)  + (sigma(m-1)-sigma(m))*tt(i,j,m)
                    pOut(i,j,k)  = pOut(i,j,k)  + (sigma(m-1)-sigma(m))*pa(i,j,m)
                    qOut(i,j,k)  = qOut(i,j,k)  + (sigma(m-1)-sigma(m))*qq(i,j,m)
                    qcOut(i,j,k) = qcOut(i,j,k) + (sigma(m-1)-sigma(m))*qcloud(i,j,m)
                 end do
              end if
                                                                ! top
              uOut(i,j,k)  = uOut(i,j,k)  + (sigma(t-1) - sigma_top) * uu(i,j,t)
              vOut(i,j,k)  = vOut(i,j,k)  + (sigma(t-1) - sigma_top) * vv(i,j,t)
              tOut(i,j,k)  = tOut(i,j,k)  + (sigma(t-1) - sigma_top) * tt(i,j,t)
              pOut(i,j,k)  = pOut(i,j,k)  + (sigma(t-1) - sigma_top) * pa(i,j,t)
              qOut(i,j,k)  = qOut(i,j,k)  + (sigma(t-1) - sigma_top) * qq(i,j,t)
              qcOut(i,j,k) = qcOut(i,j,k) + (sigma(t-1) - sigma_top) * qcloud(i,j,t)
!
!-----Normalize by dividing by the delta-sigma of output "box"
!
              uOut(i,j,k)  = uOut(i,j,k)  / (sigma_bot - sigma_top)
              vOut(i,j,k)  = vOut(i,j,k)  / (sigma_bot - sigma_top)
              tOut(i,j,k)  = tOut(i,j,k)  / (sigma_bot - sigma_top)
              pOut(i,j,k)  = pOut(i,j,k)  / (sigma_bot - sigma_top)
              qOut(i,j,k)  = qOut(i,j,k)  / (sigma_bot - sigma_top)
              qcOut(i,j,k) = qcOut(i,j,k) / (sigma_bot - sigma_top)

           end if ! if (t == b) then else
!
!-----Use sigma-linear interpolation for the vertical velocity at output faces.
!     Only have to do this at the top of each output layer, because bottom was
!     set during the last loop over k.
!
           wOut(i,j,k) = ww(i,j,t-1) + (ww(i,j,t)-ww(i,j,t-1)) *     &
                (sigma(t-1)-sigma_top)/(sigma(t-1)-sigma(t))
!
!-----Last step is to save the top of this output layer as the bottom of next
!
           b = t
           sigma_bot = sigma_top
           sumOut = (sigma_bot - sigma_top) * tOut(i,j,k)

        end do ! do k = 1,nzOut
!
!-----Q/A check: does the sum of d(sigma)*temp(k) add up?  This only works if
!     nzOut = nz, else the input and output integrals will cover a different 
!     height range.  So turn it on only if PDEBUG is set (mmif --debug), 
!     presuming the user found this note and wishes to test the method.
!
        if (pdebug) then
           sumIn  = 0. ! 
           do k = 1, nz
              sumIn = (sigma(k-1) - sigma(k)) * tt(i,j,k)
           end do

           if (abs(sumIn - sumOut)/(sumIn + sumOut) > 0.01) then ! 1%
              write(*,*)
              write(*,*) "*** WARNING: Interpolation did not conserve Temperature."
              write(*,*) "             I,J = ",i,j
              write(*,*) "             Integrated Input   = ",SumIn
              write(*,*) "             Integrated Output  = ",SumOut
              write(*,*) "             Percent difference = ",&
                   abs(sumIn - sumOut)/(sumIn + sumOut) * 100.
              write(*,*)
           end if
        end if

     end do ! do i = iBeg,iEnd
  end do    ! do j = jBeg,jEnd

  return
end subroutine interpolate
