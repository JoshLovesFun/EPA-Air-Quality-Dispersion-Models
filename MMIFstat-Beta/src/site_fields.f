      MODULE site_fields

      character*15, allocatable :: insite(:)
      character*15, allocatable :: namsit(:)

      real, allocatable :: ri(:)
      real, allocatable :: rj(:)
      real, allocatable :: rk(:)
      real, allocatable :: ylonlist(:)
      real, allocatable :: xlatlist(:)

      CONTAINS

      subroutine alloc_site(nsite)
      implicit none

      integer nsite

      allocate(  namsit(nsite) )
      allocate(      ri(nsite) )
      allocate(      rj(nsite) )
      allocate(      rk(nsite) )
      allocate(ylonlist(nsite) )
      allocate(xlatlist(nsite) )

      end subroutine alloc_site

      subroutine dealloc_site
     
      deallocate( namsit )
      deallocate( ri )
      deallocate( rj )
      deallocate( rk )
      deallocate( ylonlist )
      deallocate( xlatlist )

      end subroutine dealloc_site

      end module site_fields
      
      
