      subroutine uv2spdr(uu,vv,ws,wd)

c-----UV2SDIR converts u/v wind components into speed/direction

      implicit none

      real uu,vv,ws,wd

      real r2d

      r2d = 180./3.1415927

      if (uu.ne.-999. .and. vv.ne.-999.) then
        ws   = sqrt(uu**2 + vv**2)
        if (uu.gt.0.) then
          wd = 270. - r2d*atan(vv/uu)
        elseif (uu.lt.0.) then
          wd =  90. - r2d*atan(vv/uu)
        else
          if (vv.gt.0.) then
            wd = 180.
          elseif (vv.lt.0.) then
            wd = 0.
          else
            wd = -999.
          endif
        endif
      else
        ws = -999.
        wd = -999.
      endif

      return
      end
