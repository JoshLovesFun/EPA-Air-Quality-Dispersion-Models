      MODULE met_fields

      real*4, allocatable:: tempk(:,:,:)
      real*4, allocatable:: wvap(:,:,:)
      real*4, allocatable:: uwind(:,:,:)
      real*4, allocatable:: vwind(:,:,:)

      CONTAINS

      subroutine alloc_met(nx,ny,nz)
      implicit none

      integer nx,ny,nz

      allocate( tempk(nx,ny,nz) )
      allocate( wvap(nx,ny,nz)  )
      allocate( uwind(nx,ny,nz) )
      allocate( vwind(nx,ny,nz) )

      end subroutine alloc_met 

      subroutine dealloc_met

      deallocate( tempk )
      deallocate( wvap  )
      deallocate( uwind )
      deallocate( vwind )

      end subroutine dealloc_met

      END MODULE met_fields
      
