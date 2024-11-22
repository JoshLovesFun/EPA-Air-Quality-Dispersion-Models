      subroutine interp(ldot,ri,rj,nx,ny,array,outval)

c-----INTERP interpolates met data fields to the input spatial coordinate

      implicit none

      integer nx,ny
      real ri,rj,outval
      real array(nx,ny)
      logical ldot

      integer ii,jj
      real dx,dy,cc1,cc2

      ii = int(ri)
      jj = int(rj)
c     write(78,*)'cdem interp ',ii,jj,array(ii,jj),array(ii+1,jj),
c    &                          array(ii+1,jj+1),array(ii,jj+1)
      if (ldot .and. 
     &  (ii.lt.1 .or. jj.lt.1 .or. ii.gt.nx-1 .or. jj.gt.ny-1)) then
        outval = -999.
        return
      elseif (.not.ldot .and.
     &  (ii.lt.1 .or. jj.lt.1 .or. ii.gt.nx-2 .or. jj.gt.ny-2)) then
        outval = -999.
        return
      endif
      dx = ri - float(ii)
      dy = rj - float(jj)
      cc1 = array(ii,jj) + dx*(array(ii+1,jj) - array(ii,jj))
      cc2 = array(ii,jj+1) + dx*(array(ii+1,jj+1) - array(ii,jj+1))
      outval = cc1 + dy*(cc2 - cc1)

      return
      end
