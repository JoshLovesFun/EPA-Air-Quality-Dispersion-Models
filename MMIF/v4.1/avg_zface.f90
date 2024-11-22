subroutine avg_zface(kzin)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     Calculates average zface values based on first MM5/WRF data found.
!
!     Development History:
!     2012-01-31  New with version 2.1, moved from the top of aggregate.f90.
!     2013-01-17  Moved allocation of zavg from met_fields.f90 to this file.
!
!------------------------------------------------------------------------------
!
  USE met_fields
  implicit none
!
!-----Variable declaration
!
  integer, intent(in)             :: kzin(nzOut)
  integer                         :: i,j,k
  real                            :: zhmin ! minimum found zh(,,nz)
  real, allocatable, dimension(:) :: zavg  ! average cell height (m)
!
!-----Entry point
!
  allocate( zavg(nz) ) 
  zhmin = 0.
  do k = 1,nz
     zavg(k) = 0.
     do j = jBeg,jEnd
        do i = iBeg,iEnd
           zavg(k) = zavg(k) + zh(i,j,k) ! zh supplied by MM5/WRF
           if (k == nz .and. zh(i,j,k) > zhmin) zhmin = zh(i,j,k) 
        enddo
     enddo
     zavg(k) = zavg(k)/((iEnd-iBeg+1)*(jEnd-jBeg+1)) ! average height
  enddo
!
!-----Calculate OUTPUT layer heights based on layer mapping
!
  if (iVertMap == 0) then                 ! Specify layers via K

     zface(0) = 0.                        ! the ground, 0m AGL
     do k = 1,nzOut
        zface(k) = zavg(kzin(k))          ! interface of each OUTPUT layer
     enddo

  else ! if (iVertMap >= 1) then          ! Specify layers via Z

! This case should never happen, let's insert a check to make sure

     write(*,*) "*** ERROR: unexpected value fo iVertMap in avg_zface()."
     stop

     zface(0) = 0.                        ! the ground, 0m AGL
     do k = 1,nzOut
        zface(k) = zavg(k)                ! interface of each OUTPUT layer
     enddo

  end if
  deallocate( zavg )
!
!-----Fix the special case when processing all input layers.
!     The avgerage zface(nzOut) will be above some of the zh(,,nz) in 
!     the domain.  Set it to the minimum instead.  This might not fix
!     the problem for WRF, for which zh changes in time.
!
  if (nzOut == nz) zface(nz) = zhmin
!
!-----Round the zface values to 2 decimal places (1 cm)
!
  do k = 1,nzOut
     zface(k) = 0.01 * nint(100*zface(k))
  end do
!
!-----Layer mid-points are the average (in height, not pressure) of levels
!
  zmid(0) = 0.                            ! the ground, 0m AGL
  do k = 1,nzOut
     zmid(k) = (zface(k-1) + zface(k))/2. ! mid-point of each OUTPUT layer
     zmid(k) = 0.01 * nint(100*zmid(k))   ! round to 1 cm
  end do

end subroutine avg_zface
