      subroutine biaserr(x,y,bias,gerr,ndata)

c-----BIASERR calculates the signed error (bias) and gross error between
c     two input vectors X and Y.  Missing values (-999) in either field
c     will not be included.

      implicit none

      integer ndata
      real bias,gerr
      real x(ndata),y(ndata)

      integer ncnt,n
      real, allocatable :: diff(:),adiff(:)

      allocate( diff(ndata) )
      allocate( adiff(ndata) )

c-----Calculate differences

      ncnt = 0
      do n = 1,ndata
        if (x(n).ne.-999. .and. y(n).ne.-999.) then
          ncnt = ncnt + 1
          diff(ncnt) = y(n) - x(n)
          adiff(ncnt) = abs(diff(ncnt))
        endif
      enddo

c-----Average differences

      if (ncnt.eq.0) then
        bias = -999.
        gerr = -999.
      else
        call averag(diff,bias,ncnt)
        call averag(adiff,gerr,ncnt)
      endif

      deallocate( diff )
      deallocate( adiff )

      return
      end
