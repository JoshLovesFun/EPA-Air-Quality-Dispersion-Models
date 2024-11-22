      subroutine biaserrw(so,sp,do,dp,sbias,sgerr,dbias,dgerr,ndata)

c-----BIASERRW calculates the signed error (bias) and gross error in 
c     speed and direction between two input sets of wind vectors.  Missing 
c     values (-999) in any S/D field will not be included.

      implicit none

      integer ndata
      real sbias,sgerr,dbias,dgerr
      real so(ndata),sp(ndata),do(ndata),dp(ndata)

      integer ncnts,n,ncntd
      real, allocatable ::  sdiff(:),ddiff(:),asdiff(:),addiff(:)

      allocate(  sdiff(ndata) )
      allocate(  ddiff(ndata) )
      allocate( asdiff(ndata) )
      allocate( addiff(ndata) )

c-----Calculate differences

      ncnts = 0
      do n = 1,ndata
        if (so(n).ne.-999. .and. sp(n).ne.-999.) then
          ncnts = ncnts + 1
          sdiff(ncnts) = sp(n) - so(n)
          asdiff(ncnts) = abs(sdiff(ncnts))
        endif
      enddo

      ncntd = 0
      do n = 1,ndata
        if (do(n).ne.-999. .and. dp(n).ne.-999.) then
          ncntd = ncntd + 1
          ddiff(ncntd) = dp(n) - do(n)
          if (ddiff(ncntd).gt.180.) then
            ddiff(ncntd) = dp(n) - (do(n) + 360.)
          elseif (ddiff(ncntd).lt.-180.) then
            ddiff(ncntd) = (dp(n) + 360.) - do(n)
          endif
          addiff(ncntd) = abs(ddiff(ncntd))
        endif
      enddo

c-----Average differences

      if (ncnts.eq.0) then
        sbias = -999.
        sgerr = -999.
      else
        call averag(sdiff,sbias,ncnts)
        call averag(asdiff,sgerr,ncnts)
      endif

      if (ncntd.eq.0) then
        dbias = -999.
        dgerr = -999.
      else
        call averag(ddiff,dbias,ncntd)
        call averag(addiff,dgerr,ncntd)
      endif

      deallocate(  sdiff )
      deallocate(  ddiff )
      deallocate( asdiff )
      deallocate( addiff )

      return
      end
