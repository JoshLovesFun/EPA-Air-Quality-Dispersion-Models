      subroutine grid_calmet(iocal)
 
c-----GRID reads the CALPUFF header record and extracts domain information
 
      implicit none

      include 'common.inc'
 
      integer iocal

c     integer iflag,nxcrs,nycrs
c     integer bhi(50,20)
c     character*80 bhic(50,20)
c     real bhr(20,20)
c     character*80 bhrc(20,20)
c     real xcen,ycen,dxrat,xfmm5,yfmm5
c     logical lfirst

      character*132 comment1
      character*64  datamod
      character*16  dataset,dataver
      character*12  datenm
      character*8   pmapm,datumm
      logical lcalgrd
      integer ibyr,ibmo,ibdy,ibhr,ibtzm,irlg,irtype
      integer iwfcod,nssta,nusta,npsta,nowsta,nlu,iwat1
      integer iwat2,iutmznm
      integer ncom
      integer i
      real    xgridm,xorigm,yorigm
      real    feastm,fnorthm,utmhemm,rnlat0m,relon0m
      real    pi,rad_per_deg
  
      pi = 3.141592653589793
      rad_per_deg = pi/180.

c-----Read Record #1 File Declaration
      read(iocal) dataset,dataver,datamod
c     write(*,*)dataset
c     write(*,*)dataver
c     write(*,*)datamod

c-----Read Record #2 Number of comment lines
      read(iocal) ncom
c     write(*,*) ncom
c-----Loop over the comment lines
      do i = 1, ncom
        read(iocal) comment1
c       write(*,*)comment1
      end do

c-----Read the domain definition record
      read(iocal) 
     & ibyr,ibmo,ibdy,ibhr,ibtzm,irlg,irtype,
     & nx, ny, nz, deltax, x0mm5, y0mm5, iwfcod, nssta,
     & nusta, npsta, nowsta, nlu, iwat1, iwat2, lcalgrd,
     & pmapm,datumm,datenm,feastm,fnorthm,utmhemm,iutmznm,
     & clat,clon,tlat1,tlat2

      write(*,*)'MMIF domain Definition Record '
      write(*,*) 
     & ibyr,ibmo,ibdy,ibhr,ibtzm,irlg,irtype,
     & nx, ny, nz, deltax, x0mm5, y0mm5, iwfcod, nssta,
     & nusta, npsta, nowsta, nlu, iwat1, iwat2, lcalgrd,
     & pmapm,datumm,datenm,feastm,fnorthm,utmhemm,iutmznm,
     & clat,clon,tlat1,tlat2

       if (pmapm(1:3) .ne. 'LCC') then
         write(*,*)'This program only supports CALMET files with ',
     &             'a LCC (Lambert Conformal) projection '
         write(*,*)'The input file has a ',pmapm,' projection '
      end if

      if (abs(tlat1-tlat2) .gt. 0.1) then
        cone_factor = (alog(cos(tlat1*rad_per_deg)) -
     &                 alog(cos(tlat2*rad_per_deg)))/
     &                (alog(tan((90. - abs(tlat1))*rad_per_deg*0.5)) -  
     &                 alog(tan((90. - abs(tlat2))*rad_per_deg*0.5)))
      else
        cone_factor = sin(abs(tlat1)*rad_per_deg)
      endif

c     write(*,*)'   Cone factor = ',cone_factor

c     write(*,*) 
c    & ibyr,ibmo,ibdy,ibhr,ibtzm,irlg,irtype,
c    & nx, ny, nz, deltax, x0mm5, yorigm, iwfcod, nssta,
c    & nusta, npsta, nowsta, nlu, iwat1, iwat2, lcalgrd,
c    & pmapm,datumm,datenm,feastm,fnorthm,utmhemm,iutmznm,
c    & clat,clon,tlat1,tlat2

      rewind(iocal)

      deltax = deltax/1000.
      x0mm5 = x0mm5/1000.
      y0mm5 = y0mm5/1000.

      xfmm5 = x0mm5 + float(nx-1)*deltax
      yfmm5 = y0mm5 + float(ny-1)*deltax
      write(*,*)
      write(*,*)'Grid parameters for the input CALMET domain'
      write(*,'(a,3i10)')  '         NX,NY,NZ:',nx,ny,nz
      write(*,'(a,f10.0)') '               DX:',deltax
      write(*,'(a,2f10.1)')'    SW x/y corner:',x0mm5,y0mm5
      write(*,'(a,2f10.1)')'    NE x/y corner:',xfmm5,yfmm5

c-----Calculate beginning/ending grid indices for OBS extraction

      if (ninsite.le.0) then
        if (ninsite.eq.0) then
          ibeg = 1
          iend = nx
          jbeg = 1
          jend = ny
        else
          ibeg = 1 + nint((xbeg - x0mm5)/deltax)
          iend = 1 + nint((xend - x0mm5)/deltax)
          jbeg = 1 + nint((ybeg - y0mm5)/deltax)
          jend = 1 + nint((yend - y0mm5)/deltax)
        endif
        write(*,*)
        write(*,*)'Observation data will be extracted for the following'
        write(*,*)'grid point coordinate box:'
        write(*,*)'I-index range: ',ibeg,iend
        write(*,*)'J-index range: ',jbeg,jend
      endif

      return
      end
