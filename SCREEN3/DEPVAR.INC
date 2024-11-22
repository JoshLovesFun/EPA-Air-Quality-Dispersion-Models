c-----------------------------------------------------------------------
c --- COMMON BLOCK /DEPVAR/ --- Source Depletion Variables       DEPCOR
c-----------------------------------------------------------------------

      LOGICAL rural,urban,debug
      common/depvar/vd,vs,zd,ap,bp,cp,ar,br,cr,xr,xv,h,hmix,onebyu,
     &              rtpiby2,rt2,rtpi,igrav,rural,urban,kst,sgz,sgz0,
     &              debug,iounit
c-----------------------------------------------------------------------
c     DEFINITIONS       [i]=integer     [r]=real       [l]=logical
c-----------------------------------------------------------------------
c vd (m/s)       deposition velocity                                 [r]
c vs (m/s)       gravitational settling velocity                     [r]
c zd (m)         reference height for deposition calculation         [r]
c ap,bp,cp       coefficients for analytical form of profile fcn     [r]
c ar,br,cr       coefficients for resistance function                [r]
c xr (m)         distance from source to receptor                    [r]
c xv (m)         virtual source distance upwind of actual source     [r]
c h (m)          plume centerline height (no gravitational settling) [r]
c hmix (m)       mixing height                                       [r]
c onebyu (s/m)   reciprocal of wind speed                            [r]
c rtpiby2        square root of pi/2                                 [r]
c rt2            square root of 2                                    [r]
c rtpi           square root of pi                                   [r]
c igrav          flag for treatment of gravitational settling        [i]
c                  0:use analytical approximation to profile integral
c                  1:use numerical solution to profile integral
c rural,urban    logical indicators for dispersion site character    [l]
c kst            P-G stability class                                 [i]
c sgz (m)        sigma-z                                             [r]
c sgz0 (m)       initial sigma-z (e.g. for BID)                      [r]
c debug          logical indicator for debug option                  [l]
c iounit         unit number for debug output                        [i]
c-----------------------------------------------------------------------
