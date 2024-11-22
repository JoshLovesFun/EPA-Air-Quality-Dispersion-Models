      subroutine readstatlib(iStatUnit)
c
c     Development History:
c     2009-10-28  Original Development ( Alpine Geophysics, LLC)
c
c------------------------------------------------------------------------------
c
       USE stat_fields
       USE site_fields
       implicit none

       include 'common.inc'

       character*1   clatt,clonn
       character*4   asta

       integer istat,iStatUnit,itmpdem,n,nsitescan

       real x,y,xlat,ylon,rio,rjo

       logical eof

c
c------ scan through file to determine number of stations
c
      nsite = 0
      eof = .false.
      do while (.not. eof)
        read(iStatUnit,480,iostat=istat)asta,clatt,xlat,clonn,ylon
480      format(a4,4x,10x,20x,1x,2x,8x,5x,1x,a1,f7.4,1x,a1,f8.4,1x,i3)
        if (istat .ne. 0) then
          eof = .true.
        else
c------ if the user supplied a list of stations
          if (ninsite .gt. 0) then
            do n = 1, ninsite
c--------make sure the user specified station is inside the domain
              if (asta .eq. insite(n)) then
                if (clatt .eq. 'S') then
                  xlat = -xlat
                end if
                if (clonn .eq. 'W') then
                  ylon = -ylon
                end if
                call lcpgeo(0,clat,clon,tlat1,tlat2,x,y,ylon,xlat)
                rio = 1 + (x - x0mm5)/deltax      
                rjo = 1 + (y - y0mm5)/deltax
                if (rio.ge.float(1) .and. rio.le.float(nx-1) .and.
     &              rjo.ge.float(1) .and. rjo.le.float(ny-1)) then
                  nsite = nsite + 1
                else
                  write(*,*)'User specified station ',insite(n)
                  write(*,*)'at i,j ',rio,rjo
                  write(*,*)' Is outside domain bounds of ',
     &                       1,nx,1,ny
                  write(*,*)'Station will not be used'
                  write(*,*)
                end if
              end if
            end do
          else  
c------  the user supplied a range of coordinates
            if (clatt .eq. 'S') then
              xlat = -xlat
            end if
            if (clonn .eq. 'W') then
              ylon = -ylon
            end if
            call lcpgeo(0,clat,clon,tlat1,tlat2,x,y,ylon,xlat)
c note I and J are relative to the whole grid, not the subset grid
            rio = 1 + (x - x0mm5)/deltax
            rjo = 1 + (y - y0mm5)/deltax
            if ( rio.ge.float(ibeg) .and. rio.le.float(iend) .and.
     &           rjo.ge.float(jbeg) .and. rjo.le.float(jend) .and.
     &           rio.le.float(nx-1) .and. rjo.le.float(ny-1) ) then
              nsite = nsite + 1
            end if
          end if
        end if ! istat
      end do ! not eof
      write(*,*)'Station Scan found ',nsite,' stations '
      call alloc_site(nsite)
      call alloc_stat(nsite,mxhr,nday,-999.)
      nsitescan = nsite
c------------------------------------
c 
c read the file and add the data to the arrays
c
c------------------------------------
      rewind(IStatUnit)
      nsite = 0
      eof = .false.
      do while (.not. eof)
        read(iStatUnit,480,iostat=istat)asta,clatt,xlat,clonn,ylon
c490     format(a4,4x,10x,a20,1x,a2,8x,f5.0,1x,a1,f7.4,1x,a1,f8.4,1x,i3)
        if (istat .ne. 0) then
          eof = .true.
        else
c----- if the user supplied a list of stations
          if (ninsite .gt. 0) then
            do n = 1, ninsite
c-------make sure the user specified station is inside the domain
              if (asta .eq. insite(n)) then
                if (clatt .eq. 'S') then
                  xlat = -xlat
                end if
                if (clonn .eq. 'W') then
                  ylon = -ylon
                end if
                call lcpgeo(0,clat,clon,tlat1,tlat2,x,y,ylon,xlat)
                rio = 1 + (x - x0mm5)/deltax      
                rjo = 1 + (y - y0mm5)/deltax
                if (rio.ge.float(1) .and. rio.le.float(nx-1) .and.
     &              rjo.ge.float(1) .and. rjo.le.float(ny-1)) then
                  nsite = nsite + 1
                  rj(nsite) = rjo
                  ri(nsite) = rio
                  rk(nsite) = 1
                  namsit(nsite) = asta
                  ylonlist(nsite) = ylon
                  xlatlist(nsite) = xlat
                else
c                 write(*,*)'User specified station ',insite(n)
c                 write(*,*)'at i,j ',rio,rjo
c                 write(*,*)' Is outside domain bounds of ',
c    &                       1,nx,1,ny
c                 write(*,*)'Station will not be used'
c                 write(*,*)
                end if
              end if
            end do
          else  
c-----  the user supplied a range of coordinates
            if (clatt .eq. 'S') then
              xlat = -xlat
            end if
            if (clonn .eq. 'W') then
              ylon = -ylon
            end if
            call lcpgeo(0,clat,clon,tlat1,tlat2,x,y,ylon,xlat)
c noteI and J are relative to the whole grid, not the subset grid
            rio = 1 + (x - x0mm5)/deltax
            rjo = 1 + (y - y0mm5)/deltax
            if ( rio.ge.float(ibeg) .and. rio.le.float(iend) .and.
     &           rjo.ge.float(jbeg) .and. rjo.le.float(jend) .and.
     &           rio.le.float(nx-1) .and. rjo.le.float(ny-1) ) then
              nsite = nsite + 1
              rj(nsite) = rjo
              ri(nsite) = rio
              rk(nsite) = 1
              namsit(nsite) = asta
              ylonlist(nsite) = ylon
              xlatlist(nsite) = xlat
            end if
          end if
        end if ! istat
      end do ! not eof
      return
      end subroutine readstatlib
