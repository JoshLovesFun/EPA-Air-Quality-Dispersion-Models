c----------------------------------------------------------------------
c --- COMMON BLOCK /AMBIENT/ -- Selected met. data at one         PRIME
c                               grid cell;  used in numerical
c                               plume rise computation
c----------------------------------------------------------------------
c
      common/AMBIENT/nza,uamb(mxnz),ramb(mxnz),dedz(mxnzp1),tamb(mxnz),
     1 zfacea(mxnzp1),zgpta(mxnz),tamb0,ramb0,adia,ptgrad0
c
c --- COMMON BLOCK /AMBIENT/ Variables:
c
c                    NZA - integer - Number of layers
c             UAMB(mxnz) - real    - Wind speed profile (m/s) - winds
c                                    defined at cell CENTERS
c             RAMB(mxnz) - real    - Ambient air density profile
c                                    (kg/m**3) - defined at cell CENTERS
c           DEDZ(mxnzp1) - real    - Pot. temperature gradient profile
c                                    (deg. K/m) - defined at cell FACES
c             TAMB(mxnz) - real    - Temperature profile (deg .K) -
c                                    defined at cell CENTERS
c         ZFACEA(mxnzp1) - real    - Heights of layer faces (m)
c            ZGPTA(mxnz) - real    - Heights of layer centers (m)
c                  TAMB0 - real    - Surface air temperature (deg. K)
c                  RAMB0 - real    - Surface air density (kg/m**3)
c                   ADIA - real    - Dry adiabatic lapse rate (deg. K/m)
c                PTGRAD0 - real    - Minimum potential temperature lapse
c                                    rate (deg. K/m)
