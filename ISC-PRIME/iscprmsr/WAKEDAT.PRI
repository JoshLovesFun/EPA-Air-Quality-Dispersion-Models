c----------------------------------------------------------------------
c --- COMMON BLOCK /WAKEDAT/ -- Parameters used in the            PRIME
c                               PRIME wake and streamline
c                               subroutines
c----------------------------------------------------------------------
c
      logical lrurl
      common/WAKEDAT/Hb,Wb,xLb,Rb,HR,xLR,xLC,
     &               xbadj,ybadj,Ub,Urh,
     &               nwak,xwak(mxntr),szwak(mxntr),sywak(mxntr),
     &               drwak(mxntr),xzvwak,xyvwak,
     &               ncav,xcav(mxntr),szcav(mxntr),sycav(mxntr),
     &               xzvcav,xyvcav,fqcav,istab,lrurl
c
c --- COMMON BLOCK /WAKEDAT/ Variables:
c
c            HB - real    - Building height (m)
c            WB - real    - Building width (crosswind) - (m)
c           XLB - real    - Building length (alongwind) - (m)
c            RB - real    - Scale length (m)
c            HR - real    - Maximum cavity height (m) above ground
c           XLR - real    - Length of downwind cavity (m) from
c                           downwind face of building
c           XLC - real    - Length of roof cavity (m)
c         XBADJ - real    - Distance along the wind from the stack to
c                           the origin of the building (upwind center
c                           of effective building)
c         YBADJ - real    - Distance crosswind from the stack to
c                           the origin of the building (upwind center
c                           of effective building)
c            Ub - real    - Wind speed (m/s) at the height of bldg
c           Urh - real    - Wind speed (m/s) at release height
c
c          NWAK - integer - Number of downwind distances at which
c                           wake properties are tabulated (LE mxntr)
c   XWAK(mxntr) - real    - Downwind distance (m) from source
c  SZWAK(mxntr) - real    - Sigma-z (m) at position XWAK
c  SYWAK(mxntr) - real    - Sigma-y (m) at position XWAK
c  DRWAK(mxntr) - real    - Plume growth rate at position XWAK expressed
c                           as d/dx(plume radius) for equivalent top-hat
c        XZVWAK - real    - Virtual distance (m) for sigma-z beyond wake
c        XYVWAK - real    - Virtual distance (m) for sigma-y beyond wake
c          NCAV - integer - Number of downwind distances at which
c                           wake properties of cavity source are
c                           tabulated (LE mxntr)
c   XCAV(mxntr) - real    - Downwind distance (m) from primary source
c  SZCAV(mxntr) - real    - Sigma-z (m) for cavity source
c  SYCAV(mxntr) - real    - Sigma-y (m) for cavity source
c        XZVCAV - real    - Virtual distance (m) for sigma-z beyond wake
c                           for cavity source
c        XYVCAV - real    - Virtual distance (m) for sigma-y beyond wake
c                           for cavity source
c         FQCAV - real    - Fraction of plume mass captured by cavity
c         ISTAB - integer - PG stability class
c         LRURL - logical - Rural dispersion when .TRUE.
