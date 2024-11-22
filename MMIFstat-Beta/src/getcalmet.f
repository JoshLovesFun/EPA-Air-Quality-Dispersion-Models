      subroutine getcalmet(iocal,lmm5eof)

c-----GETCALMET interpolates model output to the OBS sites and times that
c     were identified in READOBS

      USE met_fields
      USE stat_fields
      USE site_fields
      implicit none

      include 'common.inc'

      integer iocal
      logical lmm5eof,lcalgrd

      integer year,date,hour
      integer mm5yr,mm5jdy,mm5hr,imm5dat
      integer iday,ihr,i,j,n,kk,nxcrs,nycrs
      integer npsta,nssta
      real ur,vr,tr,rid,rjd,ric,rjc,uu,vv,tt,qq

c-----Read the time invariant calmet data
      call readcalhdr(iocal,lcalgrd,npsta,nssta)
c-----Read a single time period from the CALMET output
 100  continue
      call readcalmet(iocal,mm5yr,mm5jdy,mm5hr,lmm5eof,lcalgrd,
     &                npsta,nssta)
      year = mm5yr
      date = mm5jdy
      hour = mm5hr

c-----Convert date/time to user-defined time zone and check against
c     input date range

C code if the met data is in GMT
c     hour = hour - itzon
c     if (hour.lt.0) then
c       hour = 24 + hour
c       date = date - 1
c       if (date .lt. 1) then
c          year = year - 1
c          date = 365
c          if (mod(year,4) .eq. 0) date = 366
c       endif 
c     endif
      imm5dat = 100000*year + 100*date + hour

      if (lmm5eof) goto 999
      if (imm5dat.lt.ibegdat) then
        write(*,*) 'Met date ', imm5dat, ' Before proc. date ',
     &             ibegdat
        goto 100
      end if
      if (imm5dat.gt.ienddat) then
        lmm5eof = .true.
        goto 999
      endif
      iday = date - ibegday + 1
      ihr = hour + 1
      write(*,*) 'Found CALMET day/hour:',date,hour,iday,ihr 

c-----Valid date/time found; extrapolate to standard probe heights

c     do j = 1,ny-1
c       do i = 1,nx-1
c         call micromet(tempk(i,j,nz),temp0(i,j),press(i,j),
c    &                  press0(i,j),uwind(i,j,nz),vwind(i,j,nz),
c    &                  zhght(i,j),z0(i,j),ur,vr,tr)
c         if (zhght(i,j).gt.10.) then
c           uwind(i,j,nz) = ur
c           vwind(i,j,nz) = vr
c         endif
c         if (zhght(i,j).gt.2.) then
c           tempk(i,j,nz) = tr
c         endif
c       enddo
c     enddo

c-----Interpolate to OBS site locations

      idatime(ihr,iday) = imm5dat
      do n = 1,nsite
        rid = ri(n)
        rjd = rj(n)
        ric = ri(n) - 0.5
        rjc = rj(n) - 0.5
        kk = rk(n)
        call interp(.false.,ric,rjc,nx,ny,uwind(1,1,kk),uu)
        call interp(.false.,ric,rjc,nx,ny,vwind(1,1,kk),vv)
        call interp(.false.,ric,rjc,nx,ny,tempk(1,1,kk),tt)
        call interp(.false.,ric,rjc,nx,ny,wvap(1,1,kk),qq)
        uprd(n,ihr,iday) = uu
        vprd(n,ihr,iday) = vv
        tprd(n,ihr,iday) = tt
        qprd(n,ihr,iday) = qq
      enddo
      goto 100

 999  return
      end
