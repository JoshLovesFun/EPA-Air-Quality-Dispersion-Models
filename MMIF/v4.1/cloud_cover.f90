! Contains:
!
! subroutine collapse_cloud_cover
! subroutine cloud_cover_COAMPS
! subroutine cloud_cover_mm5aermod
!
subroutine collapse_cloud_cover
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     Takes the max over layers from WRF's CLDFRA (Cloud Fraction) variable,
!     essentially making a 3D field into a 2D field.
!
!     Development History:
!     2018-12-26  New with MMIF v3.4.1
!
!------------------------------------------------------------------------------
!
  USE met_fields
  USE functions
  implicit none
!
!-----Variable declaration
!
  integer   :: i,j,k
  real      :: cloud_max

  do j = jBeg,jEnd
     do i = iBeg,iEnd

        cloud_max = 0. ! initialize the maximum cloud cover in any layer

        do k = 1,nz

           if (sigmid(k) > 0.25) then ! ignore cloud above tropopause
              cloud_max = amin1(amax1( cldfra(i,j,k), cloud_max), 1.)
!              print*,"i,j,k,cldfra = ",i,j,k,cldfra(i,j,k),cloud_max
           endif

        end do

        cldcvr(i,j) = cloud_max ! fractional, not in 10ths
!        print*,"cloud_max = ",cloud_max
!        print*

!        cldcvr(i,j) = MAXVAL( cldfra(i,j,:) ) ! would this work instead?

     end do ! end i-loop
  end do    ! end j-joop

  return

end subroutine collapse_cloud_cover

subroutine cloud_cover_COAMPS
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     Estimates cloud cover following a method used in:
!
!     Angevine, Wayne M., Lee Eddington, Kevin Durkee, Chris Fairall, 
!        Laura Bianco, Jerome Brioude, 2012: "Meteorological Model Evaluation 
!        for CalNex 2010". Mon. Wea. Rev., 140, 3885-3906. 
!
!     Development History:
!     2013-01-25  New with MMIF v2.3
!
!------------------------------------------------------------------------------
!
  USE met_fields
  USE functions
  implicit none
!
!-----Variable declaration
!
  integer   :: i,j,k
  real      :: RH, cloud_max, cloud_fraction

  do j = jBeg,jEnd
     do i = iBeg,iEnd

        cloud_max = 0. ! initialize the maximum cloud cover in any layer

        do k = 1,nz
!
!-----Calculate the relative humidity at this grid cell center
!
           RH = amin1(qq(i,j,k) / qs_fn(tt(i,j,k),pa(i,j,k))*100., 100.)
!
!----This is essentially the COAMPS method
!
           if (iswater(i,j)) then  ! over water
              cloud_fraction = amax1(0.,(RH-80.)/20.)
           else                    ! over land
              cloud_fraction = amax1(0.,(RH-70.)/30.)
           endif
!
!-----Calculate total cloud fraction in this model column
!
           cloud_max = amin1(amax1( cloud_fraction, cloud_max), 1.)

        enddo

        cldcvr(i,j) = cloud_max ! fractional, not in 10ths

     end do ! end i-loop
  end do    ! end j-joop

  return
end subroutine cloud_cover_COAMPS

subroutine cloud_cover_mm5aermod
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     Estimates cloud cover following the method used in mm5aermod,
!     originally written by EPA's T. Otte, modified by Bret Anderson.
!     Refs: Randall, 1994; Hong et al., 1998.  
!     Note: Code adapted directly from WRF ETA Radiation Scheme.
!
!     Note: this code appears to produce only 0 or 10 tenths cloud fractions,
!           when using real WRF and MM5 data.  Not very useful, or right...
!
!     Development History:
!     2011-09-30  New with MMIF v2.0, taken from program mm5aermod. 
!     2013-01-25  Bug fix: output was in tens, wanted a fraction.
!
!------------------------------------------------------------------------------
!
  USE met_fields
  USE functions
  implicit none
!
!-----Variable declaration
!
  integer   :: i,j,k
  real      :: RH, qs, cldmax, QCLD, CFRCSTRAT, CFRCCUM, arg

  REAL, PARAMETER :: QCLDMIN  = 1.E-12  ! Cloud mixing ratio minimum
  REAL, PARAMETER :: H0       = 0.      ! no clouds
  REAL, PARAMETER :: H1       = 1.      ! 100% cloud cover
  REAL, PARAMETER :: GAMMA    = 0.49 
  REAL, PARAMETER :: RHGRID   = 1.0
  REAL, PARAMETER :: PEXP     = 0.25

  do j = jBeg,jEnd
     do i = iBeg,iEnd

        cldmax = 0. ! maximum cloud cover in any layer
        do k = 1,nz

           qs = qs_fn(tt(i,j,k),pa(i,j,k))
           RH = qq(i,j,k) / qs
!
!-----Compute grid-scale cloud cover first, then convective.
!-----Total "cloud" mixing ratio is QCLD.  Rain and snow are not part of
!     cloud, only cloud water + cloud ice
!
           QCLD = qcloud(i,j,k) + qice(i,j,k)
!
!-----Determine cloud fraction (modified from original algorithm)
!
           if (QCLD < QCLDMIN) then
!
!-----Assume zero cloud fraction if there is no cloud mixing ratio
!
              CFRCSTRAT = H0
          
           elseif (RH >= RHGRID) then
!
!--- Assume cloud fraction of unity if near saturation and the cloud
!    mixing ratio is at or above the minimum threshold
!
              CFRCSTRAT= H1

           else
!
!--- Adaptation of original algorithm (Randall, 1994; Zhao, 1995)
!    modified based on assumed grid-scale saturation at RH=RHgrid.
!    EXP(-6.9) = .001, a functional minimum.
!
              arg = MAX(-6.9, -100.*QCLD / ((RHGRID * qs - qq(i,j,k))**GAMMA))
              CFRCSTRAT = (RH/RHGRID)**PEXP*(1.-EXP(arg))
   
           endif

! Derive convective cloud cover
! * Based on relative humidity at half-sigma closest to 850 mb.
! * Borrowed from James Thurman's Philedephia project.
! * Modified for 3D cloud treatment (B. Anderson).

           if (RH > 0.99) then
              CFRCCUM = 1.0
           else
              CFRCCUM = (0.02*(-1.0 + SQRT(1.0+100.0*(1.0-RH)))/(1.0-RH))
           endif
           
! Calculate total cloud fraction in this model column
! First,  find maximum of convective and stratiform clouds diagnosed.
! Second, find total cloud fraction based upon simple maximum overlap function.

           cldmax = amax1( amax1(CFRCCUM, CFRCSTRAT), cldmax)

        enddo

        cldcvr(i,j) = cldmax ! fractional, not in 10ths

     end do ! end i-loop
  end do    ! end j-joop

  return
end subroutine cloud_cover_mm5aermod
