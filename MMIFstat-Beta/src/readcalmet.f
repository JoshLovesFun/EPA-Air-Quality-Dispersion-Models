      subroutine readcalmet(iocal,calyr,calday,calhr,lcaleof,lcalgrd,
     &                      npsta,nssta)

c-----READMM5 reads hourly CALMET data from file, assumes file
c      time invariant section has already been read

      USE met_fields
      implicit none

      include 'common.inc'

      integer iocal,calyr,calday,calhr
      integer i,j,k
      integer ndathr,ndathr2,ndathr3
      integer istat
      integer npsta,nssta
      integer i2dmet
      integer, allocatable :: iwvap(:,:)
      logical lcaleof,lcalgrd
      character*8 clabel,clabex
      character*9 card

      i2dmet = 1

      allocate( iwvap(nx,ny) )
c-----Read in the CALMET wind data
      do k = 1, nz
        read(iocal,iostat=istat)clabel,ndathr,
     &                          ((uwind(i,j,k),i=1,nx),j=1,ny)
        if (istat .ne. 0) then
          lcaleof = .true.
          return
        endif
        clabex = 'U-LEV   '
        write(clabex(6:8),'(i3)')k
        if (clabel .ne. clabex) then
          write(*,500)clabel,clabex
 500       format('CALMET file out of sync.  Read ',a8,
     1            ' expecting ',a8)
          stop
        endif
        read(iocal)clabel,ndathr2,((vwind(i,j,k),i=1,nx),j=1,ny)
        clabex(1:1) = 'V'
        if (clabel .ne. clabex) then
          write(*,500)clabel,clabex
          stop
        endif
        if (lcalgrd) then
          read(iocal)clabel,ndathr3
          clabex = 'WFACE   '
          write(clabex(6:8),'(i3)')k
          if (clabel .ne. clabex) then
            write(*,500)clabel,clabex
            stop
          endif
        endif
        if ((ndathr .ne. ndathr2) .or. (ndathr .ne. ndathr3)) then
          write(*,*)'Times in wind portion of CALMET do not agree '
          write(*,*)ndathr,ndathr2,ndathr3
          stop
        endif
      end do
      write(card,'(i9)')ndathr
      read(card,'(i4,i3,i2)')calyr,calday,calhr
c     write(*,*)ndathr,calyr,calday,calhr

c-----Read the 3-D temperature field
      if (lcalgrd) then
        do k = 1, nz
          read(iocal)clabel,ndathr2,((tempk(i,j,k),i=1,nx),j=1,ny)
          clabex = 'T-LEV   '
          write(clabex(6:8),'(i3)')k
          if (clabel .ne. clabex) then
            write(*,500)clabel,clabex
            stop
          endif
          if (ndathr .ne. ndathr2) then
            write(*,*)
     &       'Times in temperature of CALMET do not agree '
            write(*,*)ndathr,ndathr2,ndathr3
            stop
          endif
        end do
      end if
c-----Read other 2-D meteorological fields
c
c-----PGT stability class
      read(iocal)clabel,ndathr2
      clabex = 'IPGT    '
      if (clabel .ne. clabex) then
        write(*,500)clabel,clabex
        stop
      endif
      if (ndathr .ne. ndathr2) then
        write(*,*)
     &   'Times in PGT of CALMET do not agree '
        write(*,*)ndathr,ndathr2
        stop
      endif
c-----Friction Velocity (ustar)
      read(iocal)clabel,ndathr2
      clabex = 'USTAR   '
      if (clabel .ne. clabex) then
        write(*,500)clabel,clabex
        stop
      endif
      if (ndathr .ne. ndathr2) then
        write(*,*)
     &   'Times in USTAR portion of CALMET do not agree '
        write(*,*)ndathr,ndathr2
        stop
      endif
c-----Mixing Height 
      read(iocal)clabel,ndathr2
      clabex = 'ZI      '
      if (clabel .ne. clabex) then
        write(*,500)clabel,clabex
        stop
      endif
      if (ndathr .ne. ndathr2) then
        write(*,*)
     &   'Times in Mixing Height portion of CALMET do not agree '
        write(*,*)ndathr,ndathr2
        stop
      endif
c-----Monin-Obukhov Length
      read(iocal)clabel,ndathr2
      clabex = 'EL      '
      if (clabel .ne. clabex) then
        write(*,500)clabel,clabex
        stop
      endif
      if (ndathr .ne. ndathr2) then
        write(*,*)
     &   'Times in Monin-Obukhov portion of CALMET do not agree '
        write(*,*)ndathr,ndathr2
        stop
      endif
c-----Convective Velocity Scale (wstar)
      read(iocal)clabel,ndathr2
      clabex = 'WSTAR   '
      if (clabel .ne. clabex) then
        write(*,500)clabel,clabex
        stop
      endif
      if (ndathr .ne. ndathr2) then
        write(*,*)
     &   'Times in Wstar portion of CALMET do not agree '
        write(*,*)ndathr,ndathr2
        stop
      endif
c-----Precipitation Data
cmmif if (npsta .gt. 0) then
        read(iocal)clabel,ndathr2
        clabex = 'RMM     '
        if (clabel .ne. clabex) then
          write(*,500)clabel,clabex
          stop
        endif
        if (ndathr .ne. ndathr2) then
          write(*,*)
     &     'Times in precip. data portion of CALMET do not agree '
          write(*,*)ndathr,ndathr2
          stop
        endif
cmmif endif
c-----Noobs CALMET varibles
      if (i2dmet .eq. 1) then
c-------Temperature
        read(iocal)clabel,ndathr2
        clabex = 'TEMPK   '
        if (clabel .ne. clabex) then
          write(*,500)clabel,clabex
          stop
        endif
        if (ndathr .ne. ndathr2) then
          write(*,*)
     &     'Times in NOOBS T portion of CALMET do not agree '
          write(*,*)ndathr,ndathr2
          stop
        endif
c-------Rho           
        read(iocal)clabel,ndathr2
        clabex = 'RHO     '
        if (clabel .ne. clabex) then
          write(*,500)clabel,clabex
          stop
        endif
        if (ndathr .ne. ndathr2) then
          write(*,*)
     &     'Times in NOOBS RHO portion of CALMET do not agree '
          write(*,*)ndathr,ndathr2
          stop
        endif
c-------QSW           
        read(iocal)clabel,ndathr2
        clabex = 'QSW     '
        if (clabel .ne. clabex) then
          write(*,500)clabel,clabex
          stop
        endif
        if (ndathr .ne. ndathr2) then
          write(*,*)
     &     'Times in NOOBS QSW portion of CALMET do not agree '
          write(*,*)ndathr,ndathr2
          stop
        endif
c-------IRH           
        read(iocal)clabel,ndathr2,((iwvap(i,j),i=1,nx),j=1,ny)
        do j = 1, ny
          do i = 1, nx
            wvap(i,j,1) = iwvap(i,j)
          end do
        end do
        clabex = 'IRH     '
        if (clabel .ne. clabex) then
          write(*,500)clabel,clabex
          stop
        endif
        if (ndathr .ne. ndathr2) then
          write(*,*)
     &     'Times in NOOBS IRH portion of CALMET do not agree '
          write(*,*)ndathr,ndathr2
          stop
        endif
c-------Precipitation code at surface stations
cmmif   if (npsta .ne. 0) then
          read(iocal)clabel,ndathr2
          clabex = 'IPCODE  '
          if (clabel .ne. clabex) then
            write(*,500)clabel,clabex
            stop
          endif
          if (ndathr .ne. ndathr2) then
            write(*,*)
     &       'Times in NOOBS IRH portion of CALMET do not agree '
            write(*,*)ndathr,ndathr2
            stop
          endif
cmmif   endif
      else if (i2dmet .eq. 0) then
c-----Read 1-D meteorological fields
c-------Temperature
        read(iocal)clabel,ndathr2
        clabex = 'TEMPK   '
        if (clabel .ne. clabex) then
          write(*,500)clabel,clabex
          stop
        endif
        if (ndathr .ne. ndathr2) then
          write(*,*)
     &     'Times in Station T portion of CALMET do not agree '
          write(*,*)ndathr,ndathr2
          stop
        endif
c-------Rho
        read(iocal)clabel,ndathr2
        clabex = 'RHO     '
        if (clabel .ne. clabex) then
          write(*,500)clabel,clabex
          stop
        endif
        if (ndathr .ne. ndathr2) then
          write(*,*)
     &     'Times in Station RHO portion of CALMET do not agree '
          write(*,*)ndathr,ndathr2
          stop
        endif
c-------QSW
        read(iocal)clabel,ndathr2
        clabex = 'QSW     '
        if (clabel .ne. clabex) then
          write(*,500)clabel,clabex
          stop
        endif
        if (ndathr .ne. ndathr2) then
          write(*,*)
     &     'Times in Station RHO portion of CALMET do not agree '
          write(*,*)ndathr,ndathr2
          stop
        endif
c-------IRH
        read(iocal)clabel,ndathr2
        clabex = 'IRH     '
        if (clabel .ne. clabex) then
          write(*,500)clabel,clabex
          stop
        endif
        if (ndathr .ne. ndathr2) then
          write(*,*)
     &     'Times in Station IRH portion of CALMET do not agree '
          write(*,*)ndathr,ndathr2
          stop
        endif
c-------Precipitation code
        if (npsta .gt. 0) then
          read(iocal)clabel,ndathr2
          clabex = 'IRH     '
          if (clabel .ne. clabex) then
            write(*,500)clabel,clabex
            stop
          endif
          if (ndathr .ne. ndathr2) then
            write(*,*)
     &       'Times in Station Precip. portion of CALMET do not agree '
            write(*,*)ndathr,ndathr2
            stop
          endif
        end if
      endif
      deallocate( iwvap )
 
      return
      end 
