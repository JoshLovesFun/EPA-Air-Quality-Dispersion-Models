c----------------------------------------------------------------------
c --- COMMON BLOCK /DFSN/ -- Parameters used in the            PRIME
c                            PRIME turbulence and diffusion
c                            subroutines
c----------------------------------------------------------------------
c
      real rurliz(6),rurliy(6),urbniz(6),urbniy(6)
      common/DFSN/afac,xbyrmax,wiz0,wiy0,wfz,wfy,
     &            dua_ua,xdecay,xdecayi,
     &            rurliz,rurliy,
     &            urbniz,urbniy
c
c --- COMMON BLOCK /DFSN/ Variables:
c
c          AFAC - real    - Diffusion transitions to ambient (with
c                           virtual source) when wake turbulence decays
c                           to AFAC*(ambient turbulence intensity) for
c                           PG classes 4, 5, and 6
c       XBYRMAX - real    - Upper limit on distance from upwind face
c                           of bldg to transition point for ambient
c                           diffusion
c       WIZ,WIY - real    - Base Turbulence intensities in wake
c       WFZ,WFY - real    - Scaling factors for sigmaz and sigmay
c        DUA_UA - real    - [Ua-U]/Ua in wake at downwind face of bldg
c                                U: average speed in wake
c                               Ua: ambient speed
c         DECAY - real    - Exponent for turbulence intensity change
c                           with distance from downwind face of bldg
c        DECAYI - real    - 1/DECAY
c     RURLIZ(6) - real    - Rural turbulence intensities in z
c     RURLIY(6) - real    - Rural turbulence intensities in y
c     URBNIZ(6) - real    - Urban turbulence intensities in z
c     URBNIY(6) - real    - Urban turbulence intensities in y
c --- Ambient turbulence intensities are inferred from Briggs (1973)
c --- "Diffusion estimation for small emissions", ATDL-106;
