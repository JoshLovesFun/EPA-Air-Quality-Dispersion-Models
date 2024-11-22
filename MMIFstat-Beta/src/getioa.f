      subroutine getioa(x,y,omean,rmse,ioa,ndata)

c-----GETIOA calculates the "Index of Agreement" between two input vectors X
c     and Y.  Missing values (-999) in either field will not be included.

      implicit none

      integer ndata
      real omean,rmse,ioa
      real x(ndata),y(ndata)

      integer ncnt,n
      real sum,diffp,diffo

c-----Calculate differences

      ncnt = 0
      sum = 0.
      do n = 1,ndata
        if (x(n).ne.-999. .and. y(n).ne.-999.) then
          ncnt = ncnt + 1
          diffp = abs(y(n) - omean)
          diffo = abs(x(n) - omean)
          sum = sum + (diffp + diffo)**2
        endif
      enddo

c-----Calculate IOA

      if (rmse.eq.-999. .or. ncnt.lt.3) then
        ioa = -999.
      else
        ioa = 1. - float(ncnt)*rmse*rmse/sum
      endif

      return
      end
