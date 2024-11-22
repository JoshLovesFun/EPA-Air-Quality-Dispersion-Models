      subroutine readcalhdr(iocal,lcalgrd,npsta,nssta)

c-----READCALHDR reads time invariant CALMET data from file

      implicit none

      include 'common.inc'


      character*64  datamod
      character*16  dataset,dataver
      character*12  datenm
      character*8   pmapm,datumm
      character*8   clabel,clabex
      logical lcalgrd
      integer ndathr
      integer iocal
      integer i,j,ncom
      integer ibyr,ibmo,ibdy,ibhr,ibtzm,irlg,irtype
      integer iwfcod,nssta,nusta,npsta,nowsta,nlu,iwat1
      integer iwat2,iutmznm
      real    xgridm,xorigm,yorigm
      real    feastm,fnorthm,utmhemm,rnlat0m,relon0m

c-----Read Record #1 File Declaration
      read(iocal) dataset,dataver,datamod
c-----Read Record #2 Number of comment lines
      read(iocal) ncom
c-----Loop over the comment lines
      do i = 1, ncom
        read(iocal) 
      end do
c-----Read the domain definition record
      read(iocal)
     & ibyr,ibmo,ibdy,ibhr,ibtzm,irlg,irtype,
     & nx, ny, nz, deltax, x0mm5, y0mm5, iwfcod, nssta,
     & nusta, npsta, nowsta, nlu, iwat1, iwat2, lcalgrd,
     & pmapm,datumm,datenm,feastm,fnorthm,utmhemm,iutmznm,
     & clat,clon,tlat1,tlat2
      
c-----Read the cell face heights
      read(iocal)
c-----Read x and y coords of surface stations
      if (nssta .gt. 0) then
        read(iocal)clabel
        clabex = 'XSSTA   '
        if (clabel .ne. clabex) then
          write(*,500)clabel,clabex 
 500       format('CALMET file out of sync.  Read ',a8,
     1            ' expecting ',a8)
          stop
        endif
        read(iocal)clabel
        clabex = 'YSSTA   '
        if (clabel .ne. clabex) then
          write(*,500)clabel,clabex 
          stop
        endif
      end if
c-----Read x anx y coords of upper air stations
      if (nusta .gt. 0) then
        read(iocal)clabel
        clabex = 'XUSTA   '
        if (clabel .ne. clabex) then
          write(*,500)clabel,clabex 
          stop
        endif
        read(iocal)clabel
        clabex = 'YUSTA   '
        if (clabel .ne. clabex) then
          write(*,500)clabel,clabex
          stop
        endif
      end if
c-----Read x anx y coords of precipitation stations
      if (npsta .gt. 0) then
        read(iocal)clabel
        clabex = 'XPSTA   '
        if (clabel .ne. clabex) then
          write(*,500)clabel,clabex 
          stop
        endif
        read(iocal)clabel
        clabex = 'YPSTA   '
        if (clabel .ne. clabex) then
          write(*,500)clabel,clabex
          stop
        endif
      end if
c-----Surface roughness lengths
      read(iocal)clabel,ndathr
      clabex = 'Z0      '
      if (clabel .ne. clabex) then
        write(*,500)clabel,clabex
        stop
      endif
c-----Land use categories
      read(iocal)clabel
      clabex = 'ILANDU  '
      if (clabel .ne. clabex) then
        write(*,500)clabel,clabex
        stop
      endif
c-----Elevations
      read(iocal)clabel,ndathr
      clabex = 'ELEV    '
      if (clabel .ne. clabex) then
        write(*,500)clabel,clabex
        stop
      endif
c-----Leaf area index
      read(iocal)clabel
      clabex = 'XLAI    '
      if (clabel .ne. clabex) then
        write(*,500)clabel,clabex
        stop
      endif
c-----Nearest surface station to each grid point
      if (nssta .ge. 1) then
        read(iocal)clabel
        clabex = 'NEARS   '
        if (clabel .ne. clabex) then
          write(*,500)clabel,clabex
          stop
        endif
      end if

      return
      end
