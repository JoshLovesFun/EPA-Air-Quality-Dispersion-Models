!Contains:
!
!subroutine aggregate(kz1,kz2)
!subroutine vertmap(ibeg,iend,jbeg,jend,nx,ny,nzIn,nzOut,kz1,kz2,sigma,xin,xout)
!
!******************************************************************************
!
subroutine aggregate(kz1,kz2)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     Aggregates MM5/WRF vertical levels to OUTPUT levels.
!
!     Development History:
!     2011-09-30  New with MMIF v2.0 ; code moved from mmif.f90.
!     2012-01-31  Moved definition of zmid to earlier in the flow.
!                 Put vertmap() in here, eliminated vertmap.f90.
!     2021-06-24  Add ability to use WRF's hybrid vertical coordinate
!------------------------------------------------------------------------------
!
  USE met_fields
  implicit none
!
!-----Variable declaration
!
  integer, intent(in) :: kz1(nzOut),kz2(nzOut)
  integer             :: i,j,k
!
!-----Entry point
!              
!-----Vertical aggregation to OUTPUT vertical structure requires 
!     met variables to couple to Pstar.
!
  do k = 1,nz
     do j = jBeg,jEnd
        do i = iBeg,iEnd
           uu(i,j,k) = uu(i,j,k)*psax(i,j,k)
           vv(i,j,k) = vv(i,j,k)*psax(i,j,k)
           tt(i,j,k) = tt(i,j,k)*psax(i,j,k)
           pa(i,j,k) = pa(i,j,k)*psax(i,j,k)
           qq(i,j,k) = qq(i,j,k)*psax(i,j,k)
           qcloud(i,j,k) = qcloud(i,j,k)*psax(i,j,k)
        enddo
     enddo
  enddo
!
!-----Map momentum and thermodynamic variables onto vertical grid structure 
!
  call vertmap(iBeg,iEnd,jBeg,jEnd,nx,ny,nz,nzOut,kz1,kz2,sigma,psax,psTmp)
  call vertmap(iBeg,iEnd,jBeg,jEnd,nx,ny,nz,nzOut,kz1,kz2,sigma,uu,uOut)
  call vertmap(iBeg,iEnd,jBeg,jEnd,nx,ny,nz,nzOut,kz1,kz2,sigma,vv,vOut)
  call vertmap(iBeg,iEnd,jBeg,jEnd,nx,ny,nz,nzOut,kz1,kz2,sigma,tt,tOut)
  call vertmap(iBeg,iEnd,jBeg,jEnd,nx,ny,nz,nzOut,kz1,kz2,sigma,pa,pOut)
  call vertmap(iBeg,iEnd,jBeg,jEnd,nx,ny,nz,nzOut,kz1,kz2,sigma,qq,qOut)
  call vertmap(iBeg,iEnd,jBeg,jEnd,nx,ny,nz,nzOut,kz1,kz2,sigma,qcloud,qcOut)
!
!-----Decouple vertically aggregated variables from Pstar (p*)
!     and map vertical velocity to OUTPUT vertical grid.  
!     The vertical velocity (wOut) is not weighted -- use the values
!     at the top and bottom of the aggregated cells.
!
  do k = 1,nzOut
     do j = jBeg,jEnd
        do i = iBeg,iEnd
           wOut(i,j,k)  = ww(i,j,kz2(k))            ! W is special
           uOut(i,j,k)  = uOut(i,j,k)/psTmp(i,j,k)
           vOut(i,j,k)  = vOut(i,j,k)/psTmp(i,j,k)
           tOut(i,j,k)  = tOut(i,j,k)/psTmp(i,j,k)
           pOut(i,j,k)  = pOut(i,j,k)/psTmp(i,j,k)
           qOut(i,j,k)  = qOut(i,j,k)/psTmp(i,j,k)
           qcOut(i,j,k) = qcOut(i,j,k)/psTmp(i,j,k)
           if (qOut(i,j,k)  < 0.) qOut(i,j,k)  = 0. ! sanity checks
           if (qcOut(i,j,k) < 0.) qcOut(i,j,k) = 0.
        enddo
     enddo
  enddo
  wOut(iBeg:iEnd,jBeg:jEnd,0) = 0.        ! vert vel at ground is zero

  return
end subroutine aggregate
!
!******************************************************************************
!
subroutine vertmap(ibeg,iend, jbeg,jend, nx,ny,nzIn, nzOut, & 
                   kz1,kz2,sigma, xin,xOut)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     VERTMAP vertically aggregates MM5/WRF data on the met model 
!     grid to the output grid.
! 
!     NOTE: the ouput physical height grid is a coarser set of 
!           the MM5/WRF sigma-p coordinate system  
!
!     Development History:
!     2009-05-26  Original Development (ENVIRON International Corp.)
!
!------------------------------------------------------------------------------
!
  implicit none
!
!-----Variable declaration
!
  integer :: ibeg,iend,jbeg,jend,nx,ny,nzIn,nzOut
  integer :: i,j,k,kk, kz1(nzOut),kz2(nzOut)

  real    :: xin(nx,ny,nzIn),xout(nx,ny,nzOut)
  real    :: sigma(0:nzIn), sum, dsigma
!
!-----Entry point
!
  do j = jbeg,jend
     do i = ibeg,iend
        do k = 1,nzOut
           sum = 0.
           do kk = kz1(k),kz2(k) 
              dsigma = sigma(kk-1) - sigma(kk)
              sum = sum + xin(i,j,kk)*dsigma
           enddo
           dsigma = sigma(kz1(k)-1) - sigma(kz2(k))
           xout(i,j,k) = sum/dsigma
        enddo
     enddo
  enddo

  return
end subroutine vertmap
