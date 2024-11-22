      subroutine averag(x,avg,ndata)

c-----AVERAG calculates the average value of the input vector, but does
c     not include any missing values (-999).

      implicit none

      integer ndata
      real avg
      real x(ndata)

      integer ncnt,n

      ncnt = 0
      avg = 0.
      do n = 1,ndata
        if (x(n).ne.-999. .and. x(n) .lt. 999.) then
          ncnt = ncnt + 1
          avg = avg + x(n)
        endif
      enddo
      if (ncnt.eq.0) then
        avg = -999.
      else
        avg = avg/float(ncnt)
      endif

      return
      end
