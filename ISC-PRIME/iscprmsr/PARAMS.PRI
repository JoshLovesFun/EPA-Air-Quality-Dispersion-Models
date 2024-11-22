c----------------------------------------------------------------------
c --- PARAMETER statements                                        PRIME
c----------------------------------------------------------------------
c --- Specify model version
      character*12 mver, mlevel
      parameter(mver='alpha',mlevel='950610')
c
c --- Specify parameters
      parameter(mxnz=100)
      parameter(mxntr=50)
      parameter(mxnw=5000)
      parameter(mxent=10)
      parameter(io5=5,io6=6)
c
c --- Compute derived parameters
      parameter(mxnzp1=mxnz+1)
      parameter(mxentp1=mxent+1)
c
c --- GENERAL PARAMETER definitions:
c          MXNZ - Maximum number of vertical layers in
c                 the meteorological data
c         MXNTR - Maximum number of downwind distances for which
c                 numerical plume rise will be reported
c          MXNW - Maximum number of downwind distances for numerical
c                 plume rise integration (should be set equal to
c                 SLAST/DS)
c         MXENT - Maximum number of perturbed entrainment coefficients
c                 entered
c
c --- FORTRAN I/O unit numbers:
c           IO5 - Control file                  - input  - formatted
c           IO6 - List file                     - output - formatted
