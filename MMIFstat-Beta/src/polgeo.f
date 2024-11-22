      subroutine polgeo(iway,phic,xlonc,truelat1,truelat2,xloc,yloc,
     &                  xlon,ylat)

c-----POLGEO performs Polar Stereographic to geodetic (lat/lon) translation
c
c     Code based on the TERRAIN preprocessor for MM5 v3.0,
c     developed by Yiqin Jia, ENVIRON International Corporation. 
c     09/08/2004
c
c     Modifications:
c        none
c
c     Input arguments:
c        iway                Conversion type
c                            0 = geodetic to Polar Stereographic
c                            1 = Polar Stereographic to geodetic
c        phic                Central latitude (deg, neg for southern hem)
c        xlonc               Central longitude (deg, neg for western hem)
c        truelat1            First true latitute (deg, neg for southern hem)
c        truelat2            Second true latitute (deg, neg for southern hem)
c        xloc/yloc           Projection coordinates (km)
c        xlon/ylat           Longitude/Latitude (deg)
c
c     Output arguments:
c        xloc/yloc           Projection coordinates (km)
c        xlon/ylat           Longitude/Latitude (deg)
c
c     Routines called:
c        none

      implicit none

      integer iway
      real phic,xlonc,truelat1,truelat2,xloc,yloc,xlon,ylat,ylon

      real conv,a,sign,pole,xn,psi1,psi0,xc,yc,flp,flpp,r
      real cell,cel1,cel2,psx

      data conv/57.29578/, a/6370./

c-----Entry Point

      if (phic.lt.0) then
        sign = -1.
      else
        sign = 1.
      endif
      pole = 90.
      if (abs(truelat1).gt.90.) then
        truelat1 = 60.
        truelat2 =  0.
        truelat1 = sign*truelat1
        truelat2 = sign*truelat2
      endif
      xn = 1.0
      psi1 = 90. - sign*truelat1
      psi1 = psi1/conv
      if (phic.lt.0.) then
        psi1 = -psi1
        pole = -pole
      endif
      psi0 = (pole - phic)/conv
      xc = 0.
      yc = -a/xn*sin(psi1)*(tan(psi0/2.)/tan(psi1/2.))**xn

c-----Calculate lat/lon of the point (xloc,yloc)

      if (iway.eq.1) then
        xloc = xloc + xc
        yloc = yloc + yc
        if (yloc.eq.0.) then
          if (xloc.ge.0.) flp = 90./conv
          if (xloc.lt.0.) flp = -90./conv
        else
          if (phic.lt.0.) then
            flp = atan2(xloc,yloc)
          else
            flp = atan2(xloc,-yloc)
          endif
        endif
        flpp = (flp/xn)*conv + xlonc
        if (flpp.lt.-180.) flpp = flpp + 360.
        if (flpp.gt. 180.) flpp = flpp - 360. 
        xlon = flpp 

        r = sqrt(xloc*xloc + yloc*yloc)
        if (phic.lt.0.) r = -r

        cell = r/a 
        cel1 = cell/(1.0 + cos(psi1))

        cel2 = atan(cel1)
        psx  = 2.*cel2*conv
        ylat = pole - psx

c-----Calculate x/y from lat/lon

      else
        ylon = xlon - xlonc
        if (ylon.gt. 180.) ylon = ylon - 360.
        if (ylon.lt.-180.) ylon = ylon + 360.
        flp = xn*ylon/conv
        psx = (pole - ylat)/conv
        r = -a/xn*sin(psi1)*(tan(psx/2.)/tan(psi1/2.))**xn
        if (phic.lt.0.) then
          xloc = r*sin(flp)
          yloc = r*cos(flp)
        else
          xloc = -r*sin(flp)
          yloc =  r*cos(flp)
        endif
      endif

      xloc = xloc - xc
      yloc = yloc - yc

      return
      end
