      subroutine getrmse(x,y,rmse,rmses,rmseu,ndata)

c-----GETRMSE calculates the systematic, unsystematic, and total 
c     root-mean-square error between two input vectors X and Y.  Missing 
c     values (-999) in either field will not be included.

      implicit none

      integer ndata
      real rmse,rmses,rmseu
      real x(ndata),y(ndata)

      integer ncnt,isame,n
      real xold,rmseu2,rmses2
      real, allocatable :: xtmp(:),ytmp(:),z(:)
      real, allocatable :: diffs(:),diffu(:)

      allocate(  xtmp(ndata) )
      allocate(  ytmp(ndata) )
      allocate(     z(ndata) )
      allocate( diffs(ndata) )
      allocate( diffu(ndata) )

c-----Load temporary vectors for regression calculation

      ncnt = 0
      isame = 1
      do n = 1,ndata
        if (x(n).ne.-999. .and. y(n).ne.-999.) then
          ncnt = ncnt + 1
          if (ncnt.eq.1) then
            xold = x(n)
          elseif (xold.ne.x(n)) then
            isame = 0
          endif
          xold = x(n)
          xtmp(ncnt) = x(n)
          ytmp(ncnt) = y(n)
        endif
      enddo
      if (isame.eq.1 .or. ncnt.lt.3) then
        rmse = -999.
        rmses = -999.
        rmseu = -999.
        return
      endif

c-----Regress Y onto X, to get linear approx Z

      call regress(xtmp,ytmp,z,ncnt)

c-----Calculate systematic and unsystematic error

      do n = 1,ncnt
        diffs(n) = (z(n) - xtmp(n))**2
        diffu(n) = (ytmp(n) - z(n))**2
      enddo
      call averag(diffs,rmses2,ncnt)
      call averag(diffu,rmseu2,ncnt)

c-----Calculate total RMSE

      rmse = sqrt(rmses2 + rmseu2)
      rmses = sqrt(rmses2)
      rmseu = sqrt(rmseu2)

      deallocate(  xtmp )
      deallocate(  ytmp )
      deallocate(     z )
      deallocate( diffs )
      deallocate( diffu )

      return
      end
