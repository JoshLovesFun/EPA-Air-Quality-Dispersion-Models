c----------------------------------------------------------------------
c --- COMMON BLOCK /NUMPARM/ -- Parameters used in the            PRIME
c                               numerical plume rise algorithm
c----------------------------------------------------------------------
c
      common/NUMPARM/gravi,rgas,zmin,ds,nstep,slast,rp,
     1               alphap(mxent),betap(mxent),xcat(mxentp1),nent
c
c --- COMMON BLOCK /NUMPARM/ Variables:
c
c         GRAVI - real    - Acceleration due to gravity (m/s**2)
c          RGAS - real    - Gas constant (m**2/s**2/deg. K)
c          ZMIN - real    - Minimum plume centerline height (m)
c            DS - real    - Step size (m) in the numerical plume
c                           rise algorithm
c         NSTEP - integer - Internal save frequency of plume rise
c                           calculations (i.e., every DS*NSTEP meters)
c                           (NOTE: this the frequency with which the
c                           results are saved internally -- not that
c                           passed back from the NUMRISE routine)
c         SLAST - real    - Termination distance (m) of the plume rise
c                           calculation
c            RP - real    - Radiation coefficient (kg/m**2/deg. K**3/s)
c   ALPHAP(mxent) - real array - Perturbed entrainment coefficients
c                                (parallel)
c    BETAP(mxent) - real array - Perturbed entrainment coefficients
c                                (normal)
c   XCAT(mxentp1) - real array - Downwind distances (m) for which each
c                                perturbed entrainment coefficient
c                                (ALPHAP, BETAP) is valid (NENT+1 values
c                                for NENT entrainment coefficients).
c            NENT - integer    - Number of perturbed entrainment
c                                coefficients entered
