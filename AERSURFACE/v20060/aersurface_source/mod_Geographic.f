C=======================================================================
      module Geographic

C     Variable declarations and procedures used to convert between 
C     geographic projections and coordinate systems.

C     Uses the following modules: 
C     - Constants

C     Contains the following procedures:
C     - subroutine ConfirmBounds
C     - subroutine LATLON2ALBERS
C     - subroutine ALBERS2LATLON
C     - subroutine NADCON (and associated procedures)
C     - subroutine UTMGEO (and associated procedures)
C     - double precision function ComputeQ

C=======================================================================

      USE Constants, only: Deg2Rad,Rad2Deg

C     Albers Projection Variables

      IMPLICIT NONE
      SAVE

      DOUBLE PRECISION :: Esquared    ! Eccentricity^2
      DOUBLE PRECISION :: EarthRadius ! Radius of the earth
      
      DOUBLE PRECISION :: CentMeridian  ! (Central Meridian) Center Longitude
      DOUBLE PRECISION :: ProjOrigin    ! Origin Latitude
      
      DOUBLE PRECISION :: StdParallel1  ! Southernmost Latitude
      DOUBLE PRECISION :: StdParallel2  ! Northernmost Latitude
      
      ! Spheroid for call to UTMGEO for conversion of UTM to Lat/Lon
      ! Based on datum entered by user NAD27 or NAD83
      ! NAD27 => Clark 1866 = 0; NAD83 => GRS80 = 4
      INTEGER (kind=4) :: iSphere
      
      contains

C=======================================================================
      SUBROUTINE ConfirmBounds(RadiusAsCells, CenterCol, CenterRow,
     &                         tiffCols, tiffRows, OK2Proceed)

C    Purpose: To check that the study area is within the boundaries of
C             the NLCD file
C
C     Called By: main program
C
C     Calls To:  <none>
C
C=======================================================================
     
      IMPLICIT NONE

      INTEGER (KIND=8),  INTENT(IN) :: CenterCol
      INTEGER (KIND=8),  INTENT(IN) :: CenterRow
      INTEGER (KIND=8),  INTENT(IN) :: tiffCols
      INTEGER (KIND=8),  INTENT(IN) :: tiffRows
      INTEGER (KIND=8),  INTENT(IN) :: RadiusAsCells
      LOGICAL,           INTENT(INOUT):: OK2Proceed

      OK2Proceed = .true.

      IF( CenterCol - RadiusAsCells .LT. 0 )THEN
         OK2Proceed = .false.
      ENDIF

      IF( CenterCol + RadiusAsCells .GT. tiffCols )THEN
         OK2Proceed = .false.
      ENDIF

      IF( CenterRow - RadiusAsCells .LT. 0 )THEN
         OK2Proceed = .false.
      ENDIF

      IF( CenterRow + RadiusAsCells .GT. tiffRows )THEN
         OK2Proceed = .false.
      ENDIF

      RETURN
      END SUBROUTINE ConfirmBounds


C=======================================================================
      DOUBLE PRECISION FUNCTION ComputeQ(Xlat)
C
C     Purpose: To compute the Q term in the lat/lon conversion to Albers
C
C     Called By: LATLON2ALBERS, ALBERS2LATLON
C
C=======================================================================

      IMPLICIT NONE

      DOUBLE PRECISION :: Xlat, Term1, Term2
      
      Term1 = dsin(XLat) / (1.0D0 - (Esquared * dsin(XLat)*dsin(XLat)))
      Term2 = (1.0D0 / (2.0D0 * dsqrt(Esquared))) *
     &        dlog((1.0D0 - dsqrt(Esquared) * dsin(Xlat)) /
     &            (1.0D0 + dsqrt(Esquared) * dsin(Xlat)) )
      ComputeQ = (1.0D0 - Esquared) * (Term1 - Term2)

      RETURN
      END FUNCTION ComputeQ

C=======================================================================
      SUBROUTINE LATLON2ALBERS(CLat, CLon, CAlbersX, CAlbersY)
C
C    Purpose: To convert latutude and longitude expressed in decimal
C             degrees to x,y coordinates using Albers Equal Area Conic
C             projection
C
C    Assumptions:  All States use the same projection parameters
C                  (see Modules.for for the parameters)
C
C    Input:  CLat = Latitude (decimal degrees)
C            CLon = Longitude (decimal degrees)
C
C    Output: CAlbersX = Albers projection X-coordinate
C            CAlbersY = Albers projection Y-coordinate
C
C    Reference: Map Projections - A Working Manual, John P. Snyder,
C               Geological Survey Professional Paper 1395, 1987

C=======================================================================
     
      IMPLICIT NONE
      
      DOUBLE PRECISION,  INTENT(IN)  :: CLat
      DOUBLE PRECISION,  INTENT(IN)  :: CLon
      DOUBLE PRECISION,  INTENT(OUT) :: CAlbersX
      DOUBLE PRECISION,  INTENT(OUT) :: CAlbersY
      
      DOUBLE PRECISION :: Xlat
c      DOUBLE PRECISION :: ComputeQ
      DOUBLE PRECISION :: Q, Q0, Q1, Q2, M1, M2, XN, C, Rho0, Theta, Rho


C---- Intermediate calculation: Q
      Xlat = Deg2Rad * CLat            ! latitude of site in radians
      Q = ComputeQ(Xlat)

C---- Intermediate calculation: Q0
      Xlat = Deg2Rad * ProjOrigin      ! projection origin in radians
      Q0 = ComputeQ(Xlat)

C---- Intermediate calculations: Q1, M1
      Xlat = Deg2Rad * StdParallel1    ! 1st std parallel in radians
      Q1 = ComputeQ(Xlat)
      M1 = dcos(Xlat) / dsqrt(1.0D0 - Esquared * dsin(XLat)*dsin(XLat) )

C---- Intermediate calculations: Q2, M2
      Xlat = Deg2Rad * StdParallel2    ! 2nd std parallel in radians
      Q2 = ComputeQ(Xlat)
      M2 = dcos(Xlat) / dsqrt(1.0D0 - Esquared * dsin(XLat)*dsin(XLat) )

C---- Intermediate calculation: N (variable name = XN)
      XN = (M1 * M1 - M2 * M2) / (Q2 - Q1)

C---- Intermediate calculation: C
      C = M1 * M1 + XN * Q1

C---- Intermediate calculation: Theta (in degrees)
      Theta = XN * (CLon - CentMeridian)

C---- Intermediate calculation: Rho0
      Rho0 = EarthRadius * dsqrt(C - XN * Q0) / XN

C---- Intermediate calculation: Rho
      Rho = EarthRadius * dsqrt(C - XN * Q) / XN

C---- Final calculation - x,y returned to calling program
      CAlbersX = Rho * dsin(Deg2Rad * Theta)
      CAlbersY = Rho0 - Rho * dcos(Deg2Rad * Theta)

      RETURN
      END SUBROUTINE LATLON2ALBERS


C=======================================================================
      SUBROUTINE ALBERS2LATLON(CAlbersX, CAlbersY, XLat, XLon)
C
C    Purpose: To convert latutude and longitude expressed in decimal
C             degrees to x,y coordinates using Albers Equal Area Conic
C             projection
C
C    Assumptions:  All Statesa use the same projection parameters
C                  (see Modules.for for the parameters)
C
C    Input:  CAlbersX = Albers projection X-coordinate
C            CAlbersY = Albers projection Y-coordinate
C
C    Output: XLat = Latitude (decimal degrees)
C            XLon = Longitude (decimal degrees)
C
C    Reference: Map Projections - A Working Manual, John P. Snyder,
C               Geological Survey Professional Paper 1395, 1987
C               See page 102 for equations used below
C
C***********************************************************************

      IMPLICIT NONE

      DOUBLE PRECISION,  INTENT(IN)  :: CAlbersX
      DOUBLE PRECISION,  INTENT(IN)  :: CAlbersY
      DOUBLE PRECISION,  INTENT(OUT) :: Xlat
      DOUBLE PRECISION,  INTENT(OUT) :: Xlon

c      DOUBLE PRECISION :: ComputeQ
      DOUBLE PRECISION :: Beta, temp
      DOUBLE PRECISION :: Q, Q0, Q1, Q2, M1, M2, XN, C, Rho0, Theta, Rho


C---- Q0, Q1, Q2, M1, M2, XN, C, and Rho are as in LatLon2Albers

C---- Intermediate calculation: Q0
      Xlat = Deg2Rad * ProjOrigin      ! projection origin in radians
      Q0 = ComputeQ(Xlat)

C---- Intermediate calculations: Q1, M1
      Xlat = Deg2Rad * StdParallel1    ! 1st std parallel in radians
      Q1 = ComputeQ(Xlat)
      M1 = dcos(Xlat) / dsqrt(1.0D0 - Esquared * dsin(XLat)*dsin(XLat) )

C---- Intermediate calculations: Q2, M2
      Xlat = Deg2Rad * StdParallel2    ! 2nd std parallel in radians
      Q2 = ComputeQ(Xlat)
      M2 = dcos(Xlat) / dsqrt(1.0D0 - Esquared * dsin(XLat)*dsin(XLat) )

C---- Intermediate calculation: N (variable name = XN)
      XN = (M1 * M1 - M2 * M2) / (Q2 - Q1)

C---- Intermediate calculation: C
      C = M1 * M1 + XN * Q1

C---- Intermediate calculation: Rho0
      Rho0 = EarthRadius * dsqrt(C - XN * Q0) / XN

C---- Intermediate calculation: Rho
      Rho = dsqrt(CAlbersX**2 + (Rho0 - CAlbersY)**2)

C---- Intermediate calculation: Q
      Q = (C - (Rho**2) * (XN**2) / (EarthRadius**2)) / XN

C---- Intermediate calculation: Theta
      Theta = DATAN(CAlbersX / (Rho0 - CAlbersY)) * Rad2Deg

C     Compute the denominator
      Temp = 1.0D0-((1.0D0-Esquared)/(2.0D0*dsqrt(Esquared))) * 
     *       DLOG((1.0D0-dsqrt(Esquared))/(1.0D0+dsqrt(Esquared)))

C---- Beta is the authalic latitude to be used in the computations of
C     latitude by the series method
      Beta = DASIN(Q/Temp)

C---- Final calculation - latitude, longitude returned to calling program
C     We are using the series solution rather than the iterative
C     method to get the latitude
      XLon = CentMeridian + Theta / XN
      XLat = Beta + (1.0D0/3.0D0 * Esquared +
     &        31.0D0/180.0D0 * Esquared**2 +
     &        517.0D0/5040.0D0 * Esquared**3) * dsin(2.0D0*Beta) +
     &        (23.0D0/360.0D0 * Esquared**2 + 
     &        251.0D0/3780.0D0 * Esquared**3) * dsin(4.0D0*Beta) +
     &        (761.0D0/45360.0D0 * Esquared**3) * dsin(6.0D0*Beta)

      XLat= XLat * Rad2Deg

      RETURN
      END SUBROUTINE ALBERS2LATLON
      

C=======================================================================     
 
C**   PROGRAM NADCON
C**
C**   THE NATIONAL GEODETIC SURVEY NADCON PROGRAM (version 2.10)
C**   HAS BEEN ADAPTED FOR THE AERMAP TERRAIN PROCESSOR.

C=======================================================================

      SUBROUTINE NADCON(XPT,YPT,XPT2,YPT2,DLOS,DLAS,DLOM,DLAM,KEY)

***********************************************************************
*                                                                     *
* PROGRAM :   NADCON                                                  *
*                                                                     *
* PURPOSE:    COMPUTATION PROGRAM TO CONVERT (OR TRANSFORM)           *
*             POSITIONAL DATA (E.G., LATITUDES AND LONGITUDES) FROM   *
*             THE NORTH AMERICAN DATUM OF 1927 (NAD 27) TO THE        *
*             NORTH AMERICAN DATUM OF 1983 (NAD 83).  THIS PROGRAM    *
*             CAN COMPUTE FROM FROM EITHER DATUM TO THE OTHER.        *
*                                                                     *
*             THE ACTUAL COMPUTATION IS PERFORMED AS AN INTERPOLATION *
*             FROM A REGULARLY-SPACED GRID OF POINTS OBTAINED FROM THE*
*             FITTING OF A MINIMUM-CURVATURE SURFACE TO THE ACTUAL    *
*             SHIFT DATA RESULTING FROM THE NAD 83 ADJUSTMENT.        *
*                                                                     *
*             THE INTERPOLATION IS ACCOMPLISHED BY LOCALLY FITTING    *
*             A CURVED POLYNOMIAL SURFACE TO THE FOUR DATA POINTS     *
*             DEFINING THE SQUARE WHICH SURROUND THE (X,Y) PAIR       *
*             WHERE THE INTERPOLATION IS TO TAKE PLACE.               *
*                                                                     *
*             THE POLYNOMIAL SURFACE IS DEFINED BY:                   *
*                                                                     *
*                         A+BX+CY+DXY=Z                               *
*                                                                     *
*             THE PROGRAM REQUIRES THAT THE USER SPECIFY:             *
*                                                                     *
*             1)  THE NAME OF AN OUTPUT FILE                          *
*                                                                     *
*             2)  THE NAME OF AN INPUT FILE (IF AVAILABLE).           *
*                                                                     *
*                                                                     *
*                                                                     *
*             ESTIMATES OF DATUM SHIFTS IN TERMS OF METERS ARE        *
*             COMPUTED FROM THE SHIFT ESTIMATES USING ELLIPSOIDAL     *
*             SCALING.                                                *
*                                                                     *
*             THIS PROGRAM ALLOWS FOR EITHER NGS STANDARD HORIZONTAL  *
*             DATA FORMATS AS SPECIFIED IN THE FGCC PUBLICATION,      *
*             COMMONLY KNOWN AS THE 'HORIZONTAL BLUE BOOK' (SEE       *
*             SUBROUTINE TYPE3), OR IN A GENERIC FILE FORMAT (SEE     *
*             SUBROUTINE TYPE1 OR SUBROUTINE TYPE2).                  *
*                                                                     *
*             THE CODE CAN BE EASILY MODIFIED TO ACCOMMODATE CUSTOM   *
*             FILE SPECIFICATIONS BY MODIFYING SUBROUTINES: ENDREP,   *
*             GETPT, IPARMS, WRTPT, AND (OPTIONALLY) FHELP.           *
*                                                                     *
*                                                                     *
* VERSION CODE:  1.03                                                 *
*                                                                     *
* VERSION DATE:  APRIL 1, 1991                                        *
*                                                                     *
*        AUTHOR:   WARREN T. DEWHURST, PH.D.                          *
*                    LIEUTENANT COMMANDER, NOAA                       *
*                  ALICE R. DREW                                      *
*                    SENIOR GEODESIST, HORIZONTAL NETWORK BRANCH      *
*                  NATIONAL GEODETIC SURVEY, NOS, NOAA                *
*                  ROCKVILLE, MD   20852                              *

c version 2.10 - 1/20/92
c      added option to select HPGN grids and compute NAD 83 - HPGN
c      conversions - jmb
***********************************************************************

***********************************************************************
*                                                                     *
*                  DISCLAIMER                                         *
*                                                                     *
*   THIS PROGRAM AND SUPPORTING INFORMATION IS FURNISHED BY THE       *
* GOVERNMENT OF THE UNITED STATES OF AMERICA, AND IS ACCEPTED AND     *
* USED BY THE RECIPIENT WITH THE UNDERSTANDING THAT THE UNITED STATES *
* GOVERNMENT MAKES NO WARRANTIES, EXPRESS OR IMPLIED, CONCERNING THE  *
* ACCURACY, COMPLETENESS, RELIABILITY, OR SUITABILITY OF THIS         *
* PROGRAM, OF ITS CONSTITUENT PARTS, OR OF ANY SUPPORTING DATA.       *
*                                                                     *
*   THE GOVERNMENT OF THE UNITED STATES OF AMERICA SHALL BE UNDER NO  *
* LIABILITY WHATSOEVER RESULTING FROM ANY USE OF THIS PROGRAM.  THIS  *
* PROGRAM SHOULD NOT BE RELIED UPON AS THE SOLE BASIS FOR SOLVING A   *
* PROBLEM WHOSE INCORRECT SOLUTION COULD RESULT IN INJURY TO PERSON   *
* OR PROPERTY.                                                        *
*                                                                     *
*   THIS PROGRAM IS PROPERTY OF THE GOVERNMENT OF THE UNITED STATES   *
* OF AMERICA.  THEREFORE, THE RECIPIENT FURTHER AGREES NOT TO ASSERT  *
* PROPRIETARY RIGHTS THEREIN AND NOT TO REPRESENT THIS PROGRAM TO     *
* ANYONE AS BEING OTHER THAN A GOVERNMENT PROGRAM.                    *
*                                                                     *
***********************************************************************

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      DOUBLE PRECISION VRSION
      INTEGER MXAREA
c     PARAMETER (VRSION = 1.03D0, MXAREA = 8)
      PARAMETER (VRSION = 2.10D0, MXAREA = 8)

      DOUBLE PRECISION ADLAM, VDLAM, ADLOM, VDLOM
      DOUBLE PRECISION ADLAS, VDLAS, ADLOS, VDLOS
      DOUBLE PRECISION SDLAM, SDLAM2, SDLOM, SDLOM2
      DOUBLE PRECISION SDLAS, SDLAS2, SDLOS, SDLOS2
      DOUBLE PRECISION XSMALL, XBIG, YSMALL, YBIG
      DOUBLE PRECISION DLAM, DLOM, DLAS, DLOS
      DOUBLE PRECISION SMDLAM, BGDLAM, SMDLOM, BGDLOM
      DOUBLE PRECISION SMDLAS, BGDLAS, SMDLOS, BGDLOS
      DOUBLE PRECISION XPT, YPT, XPT2, YPT2
      INTEGER KEY, IPAGE, ITYPE, IFILE
      LOGICAL PAGE, NODATA, SCREEN,dsel

      DOUBLE PRECISION DX, DY, XMAX, XMIN, YMAX, YMIN
      INTEGER NC, NAREA
      COMMON /GDINFO/ DX(MXAREA), DY(MXAREA), XMAX(MXAREA),
     +                XMIN(MXAREA), YMAX(MXAREA), YMIN(MXAREA),
     +                NC(MXAREA), NAREA

      INTEGER LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA
      COMMON /INOUT/ LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA(2*MXAREA)

**********************
* INITIALIZE VARIABLES
**********************
      CALL INITL (SCREEN, PAGE, IPAGE, ITYPE,
     +            SMDLAM, BGDLAM, SMDLOM, BGDLOM,
     +            SMDLAS, BGDLAS, SMDLOS, BGDLOS,
     +            ADLAM, VDLAM, SDLAM, SDLAM2,
     +            ADLOM, VDLOM, SDLOM, SDLOM2,
     +            ADLAS, VDLAS, SDLAS, SDLAS2,
     +            ADLOS, VDLOS, SDLOS, SDLOS2,
     +            XSMALL, XBIG, YSMALL, YBIG,dsel)


******************************************************** 
* OPEN NADCON DATA FILES (LATITUDE AND LONGITUDE GRIDS)
*******************************************************

      LUOUT = 22 ! Information is written to AERSURFACE Log File  ! ccrt

      dsel = .TRUE.

      ITYPE = 3      ! rwb

*********************************
* LOOP (ONCE FOR EACH CONVERSION)
*********************************

      CALL MLOOP (IPAGE, ITYPE, KEY, VRSION,
     +            XPT, YPT, XPT2, YPT2, DLAM, DLOM, DLAS, DLOS,
     +            SDLAM, SDLAM2, SDLOM, SDLOM2,
     +            SDLAS, SDLAS2, SDLOS, SDLOS2,
     +            SMDLAM, BGDLAM, SMDLOM, BGDLOM,
     +            SMDLAS, BGDLAS, SMDLOS, BGDLOS,
     +            XSMALL, XBIG, YSMALL, YBIG,
     +            SCREEN,dsel)

      CLOSE (NIN, STATUS='KEEP')
      CLOSE (NOUT, STATUS='KEEP')
      CLOSE (NAPAR, STATUS='KEEP')
      
crwb  check for values of DLOS and DLAS out-of-range;
crwb  very large values have been seen with corrupted 
crwb  grid files.  write message to main output file
      IF (DABS(DLOS).GT.100.0D0 .OR. DABS(DLAS).GT.100.0D0) THEN
         WRITE(LUOUT,*) ' '
         WRITE(LUOUT,*) 'POTENTIAL NADCON ERROR!!'
         WRITE(LUOUT,*) 'NAD shift values are out-of-range.'
         WRITE(LUOUT,*) 'NAD Grid files may be corrupted.'
         WRITE(LUOUT,*) 'NAD Shifts in arc-seconds:'
         WRITE(LUOUT,*) '    DLOS = ', DLOS
         WRITE(LUOUT,*) '    DLAS = ', DLAS
         WRITE(LUOUT,*) ' '
      END IF

 9999 RETURN

      END SUBROUTINE NADCON


      SUBROUTINE INITL (SCREEN, PAGE, IPAGE, ITYPE,
     +                  SMDLAM, BGDLAM, SMDLOM, BGDLOM,
     +                  SMDLAS, BGDLAS, SMDLOS, BGDLOS,
     +                  ADLAM, VDLAM, SDLAM, SDLAM2,
     +                  ADLOM, VDLOM, SDLOM, SDLOM2,
     +                  ADLAS, VDLAS, SDLAS, SDLAS2,
     +                  ADLOS, VDLOS, SDLOS, SDLOS2,
     +                  XSMALL, XBIG, YSMALL, YBIG,dsel)

*** This subroutine initializes all the variables needed in NADCON

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      INTEGER MXAREA
      PARAMETER (MXAREA = 8)
      CHARACTER*20 B20
      CHARACTER*80 B80
      PARAMETER (B20 = '                   ', B80 = B20//B20//B20//B20)

      DOUBLE PRECISION ADLAM, VDLAM, ADLOM, VDLOM
      DOUBLE PRECISION ADLAS, VDLAS, ADLOS, VDLOS
      DOUBLE PRECISION SDLAM, SDLAM2, SDLOM, SDLOM2
      DOUBLE PRECISION SDLAS, SDLAS2, SDLOS, SDLOS2
      DOUBLE PRECISION XSMALL, XBIG, YSMALL, YBIG
      DOUBLE PRECISION SMDLAM, BGDLAM, SMDLOM, BGDLOM
      DOUBLE PRECISION SMDLAS, BGDLAS, SMDLOS, BGDLOS
      INTEGER IPAGE, ITYPE
      LOGICAL PAGE, SCREEN, dsel

      CHARACTER*80 CARD
      COMMON /CURNT/ CARD

      INTEGER LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA
      COMMON /INOUT/ LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA(2*MXAREA)

* Initialize card variable in common CURNT to blank

      CARD = B80


******************************************************************
*                             INITIALIZE
******************************************************************

* Defaults: SCREEN = .TRUE. => send results to screen
*           PAGE = .FALSE.  => don't start a new page in the output file
*           IPAGE = 0       => current output file page number is 0
*           ITYPE = 0       => interactive input of points
*           dsel  = .FALSE. => select NAD 83, HPGN datum conversion

      SCREEN = .TRUE.
      PAGE = .FALSE.
      IPAGE = 0
      ITYPE = 0
      dsel = .FALSE.

      SMDLAM =  1.0D10
      BGDLAM = -1.0D10
      SMDLOM =  1.0D10
      BGDLOM = -1.0D10
      SMDLAS =  1.0D10
      BGDLAS = -1.0D10
      SMDLOS =  1.0D10
      BGDLOS = -1.0D10

      ADLAM = 0.0D0
      VDLAM = 0.0D0
      SDLAM = 0.0D0
      SDLAM2 = 0.0D0
      ADLOM = 0.0D0
      VDLOM = 0.0D0
      SDLOM = 0.0D0
      SDLOM2 = 0.0D0

      ADLAS = 0.0D0
      VDLAS = 0.0D0
      SDLAS = 0.0D0
      SDLAS2 = 0.0D0
      ADLOS = 0.0D0
      VDLOS = 0.0D0
      SDLOS = 0.0D0
      SDLOS2 = 0.0D0

      XSMALL =  1.0D10
      XBIG   = -1.0D10
      YSMALL =  1.0D10
      YBIG   = -1.0D10

      RETURN
      END SUBROUTINE INITL
      
      SUBROUTINE COEFF (TEE1, TEE2, TEE3, TEE4, AY, BEE, CEE, DEE)

**********************************************************************
** SUBROUTINE COEFF: GENERATES COEFFICIENTS FOR SURFACE FUNCTION     *
**********************************************************************

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      DOUBLE PRECISION AY, BEE, CEE, DEE
      DOUBLE PRECISION TEE1, TEE2, TEE3, TEE4

      AY = TEE1
      BEE = TEE3 - TEE1
      CEE = TEE2 - TEE1
      DEE = TEE4 - TEE3 - TEE2 + TEE1

      RETURN
      END SUBROUTINE COEFF
      

      SUBROUTINE DGRIDS(ILEN_FLD,NGPATH)

* This subroutine opens the NADCON grids using the default grid
* names and locations.  The default names of the grid areas are
* given in DAREAS and the default base file locations are in DFILES

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      INTEGER ILEN_FLD
      INTEGER MXAREA, MXDEF
      PARAMETER (MXAREA = 8, MXDEF = MXAREA)

      CHARACTER*80 B80
      CHARACTER*65 B65
      CHARACTER*20 B20
      PARAMETER (B20 = '                   ', B80 = B20//B20//B20//B20)
      PARAMETER (B65 = B20//B20//B20//'     ')

      DOUBLE PRECISION XMAX1, XMIN1, YMAX1, YMIN1
      DOUBLE PRECISION DX1, DY1
      INTEGER IDEF, ITEMP, NC1
      CHARACTER*80 DUM
      CHARACTER*15 DFILES(MXAREA)
      CHARACTER (LEN=ILEN_FLD+15) :: AFILE
      CHARACTER (LEN=ILEN_FLD) :: NGPATH
      CHARACTER*15 DAREAS(MXDEF)
      LOGICAL NOGO, GFLAG

      CHARACTER*15 AREAS
      COMMON /AREAS/ AREAS(MXAREA)

      DOUBLE PRECISION DX, DY, XMAX, XMIN, YMAX, YMIN
      INTEGER NC, NAREA
      COMMON /GDINFO/ DX(MXAREA), DY(MXAREA), XMAX(MXAREA),
     +                XMIN(MXAREA), YMAX(MXAREA), YMIN(MXAREA),
     +                NC(MXAREA), NAREA

      INTEGER LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA
      COMMON /INOUT/ LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA(2*MXAREA)

      DATA DUM / B80 /

* DFILES contains the default locations (pathname) of the grid files
* without the .las and .los extensions. (For example 'conus' would
* indicate that the conus.las and conus.los grid files are in the
* current working directory.)  The length of each entry in DFILES may
* be up to 65 characters.  DAREAS contains the default names of these
* areas.  The names are used internally in the program and in the
* program output.  They may be no longer than 15 characters.  They
* must correspond on a one-to-one basis with the file locations in
* the DFILES array.  That is, the first area name in DAREAS must
* be the name that you wish for the first data file set in the
* DFILES array.  You may, of course, have the arrays the same if
* the location of the data file is no longer than 15 characters.
* The locations of the grid files may be differ for each
* installation.  If the pathnames are not correct DFILES (and, possibly,
* DAREAS) may be changed and the program recompiled.

      DATA DFILES /'conus', 'hawaii', 'prvi',
     +             'stlrnc', 'stgeorge', 'stpaul', 'alaska', ' '/
      DATA DAREAS /'Conus', 'Hawaii', 'P.R. and V.I.',
     +             'St. Laurence I.', 'St. George I.', 'St. Paul I.',
     +             'Alaska', ' '/

      GFLAG = .FALSE.
      WRITE (LUOUT, 80) NGPATH
   80 FORMAT (/, '      Default Data Grids', /,
     +           '      NADGRIDS Pathname: ', A,/,
     +           '   #  AREA NAME', /, 1X, 79('=') )

      DO 140 IDEF = 1, MXDEF
        AFILE = NGPATH//DFILES(IDEF)
        IF (AFILE .EQ. B65) GOTO 999

* Try to open a set of default files.
* Do not print error messages for non-existing files.

        ITEMP = NAREA + 1
        CALL OPENFL (AFILE, ITEMP, GFLAG, NOGO, DX1, DY1,
     +               XMAX1, XMIN1, YMAX1, YMIN1, NC1, DUM,
     +               ILEN_FLD+15)

        IF (.NOT. NOGO) THEN

* Set of files opened OK and variables read

          NAREA = ITEMP
          AREAS(NAREA) = DAREAS(IDEF)
          DX(NAREA) = DX1
          DY(NAREA) = DY1
          XMAX(NAREA) = XMAX1
          XMIN(NAREA) = XMIN1
          YMAX(NAREA) = YMAX1
          YMIN(NAREA) = YMIN1
          NC(NAREA) = NC1

          WRITE (LUOUT,120) NAREA, AREAS(NAREA)
  120     FORMAT (2X, I2, 2X, A15/)

        ENDIF

  140 CONTINUE

  999 RETURN
      END SUBROUTINE DGRIDS
      

      SUBROUTINE FGRID (XPT, YPT, DX, DY, XMAX, XMIN,
     +                  YMAX, YMIN, XGRID, YGRID, IROW, JCOL, NOGO)

**********************************************************************
** SUBROUTINE FGRID: IDENTIFIES THE LOCAL GRID SQUARE FOR INTRP.     *
**********************************************************************

* This subroutine is designed to identify the grid square in which a
* particular point is located and get the corner coordinates
* converted into the index coordinate system.

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      DOUBLE PRECISION XPT, YPT, XGRID, YGRID
      DOUBLE PRECISION XMAX, XMIN, YMAX, YMIN
      DOUBLE PRECISION DX, DY
      INTEGER IROW, JCOL
      LOGICAL NOGO

      NOGO = .FALSE.

* Check to see it the point is outside the area of the gridded data

      IF (XPT .GE. XMAX  .OR.  XPT .LE. XMIN   .OR.
     +    YPT .GE. YMAX  .OR.  YPT .LE. YMIN ) THEN
        NOGO = .TRUE.
*       WRITE (*,*) '***THE POINT IS OUT OF BOUNDS***'
        GOTO 200
      ENDIF

* Calculate the coordinate values for the point to be interpolated
* in terms of grid indices

      XGRID = ( XPT - XMIN )/DX + 1.D0
      YGRID = ( YPT - YMIN )/DY + 1.D0

* Find the I,J values for the SW corner of the local square

      IROW = IDNINT(YGRID)
      JCOL = IDNINT(XGRID)

  200 RETURN
      END SUBROUTINE FGRID
      

      SUBROUTINE HMS (DD, ID, IM, S)

* Use this to change from decimal degrees (double precision)
* to integer degrees, integer minutes, and decimal seconds (double prec)
* Seconds are assumed to have no more than 5 decimal places

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      DOUBLE PRECISION SMALL
      PARAMETER (SMALL = 1.D-5)

      DOUBLE PRECISION DD, TEMP
      DOUBLE PRECISION S
      INTEGER ID, IM

      ID = IDINT(DD)
      TEMP = ( DD - DBLE(ID) )*60.0D0
      IM = IDINT(TEMP)
      S = ( TEMP - DBLE(IM) )*60.0D0

      IF (IM .EQ. 60) THEN
        IM = 0
        ID = ID + 1
      ENDIF

      IF (S .LT. SMALL) S = 0.0D0

      IF (S .GT. (60.0D0-SMALL)  ) THEN
        S = 0.0D0
        IM = IM + 1
      ENDIF

      RETURN
      END SUBROUTINE HMS


      SUBROUTINE INTRP (IAREA, IROW, NC, JCOL, XGRID, YGRID,
     +                  XPT, YPT, XPT2, YPT2, DLOS, DLAS, DLAM, DLOM)

**********************************************************************
** DETERMINE SURFACE FUNCTION FOR THIS GRID SQUARE                   *
** AND INTERPOLATE A VALUE, ZEE, FOR XPT, YPT                        *
**********************************************************************

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      INTEGER MXAREA, MAXCOL
      PARAMETER (MAXCOL = 600, MXAREA = 8)

      DOUBLE PRECISION XPT, YPT, XPT2, YPT2, XGRID, YGRID
      DOUBLE PRECISION DLOS, DLAS, DLAM, DLOM
      DOUBLE PRECISION TEE1, TEE2, TEE3, TEE4, ZEE
      INTEGER IROW, JCOL, NC, IAREA, IFILE, IDUM, J
      REAL BUF(MAXCOL)
      
      CHARACTER*8 DUMMY

      INTEGER LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA
      COMMON /INOUT/ LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA(2*MXAREA)

      DOUBLE PRECISION AY1, BEE1, CEE1, DEE1, AY2, BEE2, CEE2, DEE2
      SAVE AY1, BEE1, CEE1, DEE1, AY2, BEE2, CEE2, DEE2
      INTEGER IROWL, JCOLL, IAREAL
      SAVE IROWL, JCOLL, IAREAL

      DATA IROWL / 0 /, JCOLL / 0 /, IAREAL / 0 /

**********
* LATITUDE
**********

      IF ( IROW .NE. IROWL  .OR.  JCOL .NE. JCOLL  .OR.
     +    IAREA .NE. IAREAL ) THEN

* Lower boundary

        IFILE = LUAREA( 2*IAREA - 1 )
        READ (IFILE,REC=IROW+1, ERR=99) IDUM, (BUF(J), J=1,NC)
        TEE1 = DBLE( BUF(JCOL) )
*       TEE4 = DBLE( BUF(JCOL+1) )
        TEE3 = DBLE( BUF(JCOL+1) )

* Upper boundary

        READ (IFILE,REC=IROW+2, ERR=99) IDUM, (BUF(J), J=1,NC)
        TEE2 = DBLE( BUF(JCOL) )
*       TEE3 = DBLE( BUF(JCOL+1) )
        TEE4 = DBLE( BUF(JCOL+1) )

        CALL COEFF (TEE1, TEE2, TEE3, TEE4, AY1, BEE1, CEE1, DEE1)

      ENDIF


      CALL SURF (XGRID, YGRID, ZEE, AY1, BEE1, CEE1, DEE1, IROW, JCOL)
      DLAS = ZEE

***********
* LONGITUDE
***********

      IF ( IROW .NE. IROWL  .OR.  JCOL .NE. JCOLL  .OR.
     +    IAREA .NE. IAREAL ) THEN


* Lower boundary

        IFILE = LUAREA( 2*IAREA )
        READ (IFILE,REC=IROW+1, ERR=99) IDUM, (BUF(J), J=1,NC)
        TEE1 = DBLE( BUF(JCOL) )
*       TEE4 = DBLE( BUF(JCOL+1) )
        TEE3 = DBLE( BUF(JCOL+1) )

* Upper boundary

        READ (IFILE,REC=IROW+2, ERR=99) IDUM, (BUF(J), J=1,NC)
        TEE2 = DBLE( BUF(JCOL) )
*       TEE3 = DBLE( BUF(JCOL+1) )
        TEE4 = DBLE( BUF(JCOL+1) )

        CALL COEFF (TEE1, TEE2, TEE3, TEE4, AY2, BEE2, CEE2, DEE2)

      ENDIF

      CALL SURF (XGRID, YGRID, ZEE, AY2, BEE2, CEE2, DEE2, IROW, JCOL)
C*    Since we are using negative for West longitude, reverse sign of DLOS, rwb
      DLOS = -1.0D0*ZEE

**************************
* COMPUTE THE NAD 83 VALUES
**************************

      YPT2 = YPT + DLAS/3600.0D0

* Longitude is negative West in this subroutine, rwb

      XPT2 = XPT + DLOS/3600.0D0

*********************************************************************
* USE THE NEW ELLIPSOIDAL VARIABLES TO COMPUTE THE SHIFTS IN METERS
*********************************************************************

      CALL METERS (YPT, XPT, YPT2, XPT2, DLAM, DLOM)

* Update the last-value variables

      IROWL = IROW
      JCOLL = JCOL
      IAREAL = IAREA

      GO TO 90

 99   CONTINUE
      WRITE(DUMMY,'("NGRID",I3.3)') IFILE

 90   CONTINUE

      RETURN
      END SUBROUTINE INTRP
      

      SUBROUTINE METERS (LAT1, LONG1, LAT2, LONG2, LATMTR, LONMTR)

* This subroutine computes the difference in two positions in meters.
*
* This method utilizes ellipsoidal rather than spherical
* parameters.  I believe that the original approach and code
* for this came from Ed McKay.
* The reference used by Ed McKay for this was:
*       'A Course in Higher Geodesy' by P.W. Zakatov, Israel Program
*       for Scientific Translations, Jerusalem, 1962
*
*       Warren T. Dewhurst
*       11/1/89
* Note that this subroutine is set up for +west longitude

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

* I think that these are GRS80 parameters

      DOUBLE PRECISION AXIS, E2, RHOSEC
      PARAMETER (AXIS = 6378137.0D0)
      PARAMETER (E2 = 0.0066943800229D0)
      PARAMETER (RHOSEC = 206264.806247D0)

      DOUBLE PRECISION W, LM, LP, AVLAT
      DOUBLE PRECISION LAT1S, LAT2S, LONG1S, LONG2S, LAT1, LAT2
      DOUBLE PRECISION LONG1, LONG2, DLAT, DLONG
      DOUBLE PRECISION LATMTR, LONMTR


*     LAT1  = (LATSEC + 60.D0*( LATMIN + 60.D0*LATDEG) )/RHOSEC
*     LONG1 = (LONSEC + 60.D0*( LONMIN + 60.D0*LONDEG) )/RHOSEC
*     LAT2  = (LATSEC + 60.D0*( LATMIN + 60.D0*LATDEG) )/RHOSEC
*     LONG2 = (LONSEC + 60.D0*( LONMIN + 60.D0*LONDEG) )/RHOSEC

* Change into sec.ddd and convert to +west longitude

      LAT1S =    LAT1*60.D0*60.D0/RHOSEC
      LONG1S =  LONG1*60.D0*60.D0/RHOSEC       ! adjusted for negative West, rwb
      LAT2S =    LAT2*60.D0*60.D0/RHOSEC
      LONG2S =  LONG2*60.D0*60.D0/RHOSEC       ! adjusted for negative West, rwb

      DLAT  = ( LAT2S -  LAT1S)*RHOSEC
      DLONG = (LONG2S - LONG1S)*RHOSEC

      AVLAT = (LAT1S + LAT2S)/2.0D0

      W  = DSQRT(1.0D0 - E2*DSIN(AVLAT)**2)
      LM = AXIS*(1.0D0 - E2)/(W**3*RHOSEC)
      LP = AXIS*DCOS(AVLAT)/(W*RHOSEC)

      LATMTR = LM*DLAT
      LONMTR = LP*DLONG

      RETURN
      END SUBROUTINE METERS
      
      SUBROUTINE MLOOP (IPAGE, ITYPE, KEY, VRSION,
     +                  XPT, YPT, XPT2, YPT2, DLAM, DLOM, DLAS, DLOS,
     +                  SDLAM, SDLAM2, SDLOM, SDLOM2,
     +                  SDLAS, SDLAS2, SDLOS, SDLOS2,
     +                  SMDLAM, BGDLAM, SMDLOM, BGDLOM,
     +                  SMDLAS, BGDLAS, SMDLOS, BGDLOS,
     +                  XSMALL, XBIG, YSMALL, YBIG,
     +                  SCREEN,dsel)

**********************************************************************
* THIS SUBROUTINE LOOPS THROUGH THE INPUT DATA (EITHER AN INPUT DATA *
* FILE OR INTERACTIVELY), CALCULATES THE TRANSFORMATION VALUES,      *
* UPDATES THE MINIMUM, MAXIMUM, AND STATISTICAL SUMMATIONS, AND THEN *
* PRINTS THE RESULTS TO THE OUTPUT FILE AND/OR THE SCREEN.           *
**********************************************************************

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      INTEGER MXAREA
      PARAMETER (MXAREA = 8)

      DOUBLE PRECISION DLAM2, DLOM2, DLAS2, DLOS2
      DOUBLE PRECISION SDLAM, SDLAM2, SDLOM, SDLOM2
      DOUBLE PRECISION SDLAS, SDLAS2, SDLOS, SDLOS2
      DOUBLE PRECISION XSMALL, XBIG, YSMALL, YBIG, XPT, XPT2, YPT, YPT2
      DOUBLE PRECISION VRSION
      DOUBLE PRECISION SLA, SLO, SLA2, SLO2
      DOUBLE PRECISION DLAM, DLOM, DLAS, DLOS
      DOUBLE PRECISION SMDLAM, BGDLAM, SMDLOM, BGDLOM
      DOUBLE PRECISION SMDLAS, BGDLAS, SMDLOS, BGDLOS
      INTEGER IPAGE, ITYPE, KEY, IFMT, IPREC
      INTEGER IDLA, IMLA, IDLO, IMLO
      INTEGER IDLA2, IMLA2, IDLO2, IMLO2
      CHARACTER*80 NAME
      CHARACTER*44 FIRST
      CHARACTER*30 LAST
      CHARACTER*15 RESP
      LOGICAL NOGO, SCREEN, NOPT, EOF,dsel

      DOUBLE PRECISION DX, DY, XMAX, XMIN, YMAX, YMIN
      INTEGER NC, NAREA
      COMMON /GDINFO/ DX(MXAREA), DY(MXAREA), XMAX(MXAREA),
     +                XMIN(MXAREA), YMAX(MXAREA), YMIN(MXAREA),
     +                NC(MXAREA), NAREA

      INTEGER LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA
      COMMON /INOUT/ LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA(2*MXAREA)

* set defaults for those variables not used by every format type

      DATA NAME /' '/, FIRST /' '/, LAST /' '/, IFMT /0/

*******************************************************************
* BEGIN THE COMPUTATION LOOP FOR EACH CONVERSION
* DO UNTIL END OF FILE OR NO MORE CONVERSIONS REQUESTED
*******************************************************************

C*        Longitude is negative West, rwb

          CALL HMS (YPT, IDLA, IMLA, SLA)    ! rwb
          CALL HMS (XPT, IDLO, IMLO, SLO)    ! rwb


************************
* DO THE TRANSFORMATION
************************
        NOGO = .FALSE.
        CALL TRANSF (NOGO, RESP, XPT, YPT, XPT2, YPT2,
     +               DLAM, DLOM, DLAS, DLOS, KEY, ITYPE)

****************************************************
* CHECK TO SEE IF THIS POINT CAN BE TRANSFORMED
* IF NOGO IS TRUE THEN GET ANOTHER POINT AND DON'T
* DO THE COMPUTATION - POINT IS OUT OF BOUNDS
* IF NOGO IS NOT TRUE THEN PROCEED - ESTIMATE MADE
****************************************************

        IF (NOGO) GOTO 9999

        IF (KEY .EQ. 1) THEN

**********************
* FOR NAD 27 TO NAD 83
**********************

          CALL HMS (YPT2, IDLA2, IMLA2, SLA2)
          CALL HMS (XPT2, IDLO2, IMLO2, SLO2)

        ELSEIF (KEY .EQ. -1) THEN

**********************
* FOR NAD 83 TO NAD 27
**********************

          IDLA2 = IDLA
          IMLA2 = IMLA
          SLA2 = SLA
          IDLO2 = IDLO
          IMLO2 = IMLO
          SLO2 = SLO
          CALL HMS (YPT, IDLA, IMLA, SLA)
          CALL HMS (XPT, IDLO, IMLO, SLO)
        ENDIF

 9999 RETURN
      END SUBROUTINE MLOOP
      


      SUBROUTINE NGRIDS (NODATA,dsel,ILEN_FLD,NGPATH)

* This subroutine opens the NADCON grids which contain datum shifts.
* A total of two files are necessary for each area; 1 for each latitude
* and longitude shift table (gridded data set) expressed in arc seconds.

* If a file named AREA.PAR exists it will be read for the names and
* locations of the gridded data.  The format of the data in
* the AREA.PAR file is given in the GRIDS subroutine.

* If the AREA.PAR file does not exist, or there is still room in the
* arrays in the GDINFO common then the default area names used.

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      INTEGER ILEN_FLD
      INTEGER MXAREA
      PARAMETER (MXAREA = 8)
      
      CHARACTER*1 ANS
      LOGICAL NODATA,dsel
      
      CHARACTER (LEN=ILEN_FLD) :: NGPATH

      DOUBLE PRECISION DX, DY, XMAX, XMIN, YMAX, YMIN
      DOUBLE PRECISION VRSION
      INTEGER NC, NAREA
      COMMON /GDINFO/ DX(MXAREA), DY(MXAREA), XMAX(MXAREA),
     +                XMIN(MXAREA), YMAX(MXAREA), YMIN(MXAREA),
     +                NC(MXAREA), NAREA

      INTEGER LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA
      COMMON /INOUT/ LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA(2*MXAREA)

* Initialize

      NODATA = .FALSE.
      NAREA = 0
      LUOUT = 22 ! To AERSURFACE Log File

* If NAREA>=MXAREA, then skip the section that opens the default files.
* If NAREA<MXAREA or no 'AREA.PAR' file exists, then open default names
* in the subroutine DGRIDS.

c if state hpgn chosen(dsel=false) then only 1 file can be open at 
c a time.  If an hpgn file is not in area.par, then the user can 
c choose a state in SGRIDS.
       if(dsel) then
c default grids chosen
          IF (NAREA .LT. MXAREA) THEN

              CALL DGRIDS(ILEN_FLD,NGPATH)

          ENDIF
       end if


      IF (NAREA .EQ. 0) THEN
        NODATA = .TRUE.
        WRITE (LUOUT, 970)
  970   FORMAT (/, ' ******* ERROR *********', /,
     +          ' No grid files were opened -- program ending!')
      ENDIF

      RETURN
      END SUBROUTINE NGRIDS
      
      SUBROUTINE OPENFL (AFILE, ITEMP, GFLAG, NOGO, DX, DY,
     +                   XMAX1, XMIN1, YMAX1, YMIN1, NC1, CARD,
     +                   ILEN_FLD)

*** Given base name of gridded data files, open the two data files

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      INTEGER MXAREA
      PARAMETER (MXAREA = 8)
      CHARACTER*23 B23
      PARAMETER (B23 = '                       ')
      CHARACTER*69 B69
      PARAMETER (B69 = B23//B23//B23)

      DOUBLE PRECISION XMAX1, XMIN1, YMAX1, YMIN1, DX, DY
      REAL DX1, DY1, DX2, DY2
      REAL X01, Y01, ANGLE1, X02, Y02, ANGLE2
      INTEGER IFLAG1, IFLAG2, N1, N2, N3, N4
      INTEGER ITEMP, LRECL, ILA, ILO, IFILE, IOS
      INTEGER NC1, NR1, NZ1, NC2, NR2, NZ2
      CHARACTER*80 CARD
      INTEGER ILEN_FLD
      CHARACTER (LEN=ILEN_FLD+4) :: ALAS, ALOS
      CHARACTER (LEN=ILEN_FLD) :: AFILE
      CHARACTER*56 RIDENT
      CHARACTER*8 PGM
      LOGICAL GFLAG, NOGO, OFLAG, EFLAG1, EFLAG2

      INTEGER LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA
      COMMON /INOUT/ LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA(2*MXAREA)

      DATA IFLAG1 /1/, IFLAG2 /2/
      DATA OFLAG /.FALSE./, EFLAG1 /.FALSE./, EFLAG2 /.FALSE./


* Initialize

      NOGO = .FALSE.

* Form complete names of grid files

      N2 = LEN_TRIM(AFILE)
      IF (N2 .EQ. 0) STOP 'Logical Coding Error in OPENF'

      ALAS = B69
      ALAS(1:N2) = AFILE

      ALAS(N2+1:N2+4) = '.las'
      ALOS = B69
      ALOS(1:N2) = AFILE
      ALOS(N2+1:N2+4) = '.los'

*******************************************************
* DIRECT ACCESS GRID FILES
* Each file is opened once to get the grid variables.
* The file is then closed and reopened to ensure that
* the record length is correct
*******************************************************

* Seconds of latitude grid file

      LRECL = 256
      ILA = 2*ITEMP - 1
      IFILE = ILA + 10
      LUAREA(ILA) = IFILE
      INQUIRE (FILE=ALAS, EXIST=EFLAG1, OPENED=OFLAG)
      IF (.NOT. EFLAG1) GOTO 100

ccrt  If grid file is already open from a previous use,
ccrt    close the file so it can be opened and initialized
ccrt    for reuse.
      IF (OFLAG) CLOSE(IFILE)
      OPEN (IFILE,FILE=ALAS,FORM='UNFORMATTED',STATUS='OLD',
     +       ACCESS='DIRECT',RECL=LRECL,ERR=940,IOSTAT=IOS,
     +       ACTION='READ')
      READ (IFILE,REC=1) RIDENT, PGM, NC1, NR1, NZ1, X01, DX1,
     +                   Y01, DY1, ANGLE1
      CLOSE (IFILE)

      LRECL = 4*(NC1+1)
      OPEN (IFILE,FILE=ALAS,FORM='UNFORMATTED',STATUS='OLD',
     +       ACCESS='DIRECT',RECL=LRECL,ERR=940,IOSTAT=IOS,
     +       ACTION='READ')

* Seconds of longitude grid file

  100 LRECL = 256
      ILO = 2*ITEMP
      IFILE = ILO + 10
      LUAREA(ILO) = IFILE
      INQUIRE (FILE=ALOS, EXIST=EFLAG2, OPENED=OFLAG)
      IF (.NOT. EFLAG1) GOTO 910
      IF (.NOT. EFLAG2) GOTO 920

ccrt  If grid file is already open from a previous use,
ccrt    close the file so it can be opened and initialized
ccrt    for reuse.
      IF (OFLAG) CLOSE(IFILE)
      OPEN (IFILE,FILE=ALOS,FORM='UNFORMATTED',STATUS='OLD',
     +       ACCESS='DIRECT',RECL=LRECL,ERR=940,IOSTAT=IOS,
     +       ACTION='READ')
      READ (IFILE,REC=1) RIDENT, PGM, NC2, NR2, NZ2, X02, DX2,
     +                   Y02, DY2, ANGLE2
      CLOSE (IFILE)

      LRECL = 4*(NC2+1)
      OPEN (IFILE,FILE=ALOS,FORM='UNFORMATTED',STATUS='OLD',
     +       ACCESS='DIRECT',RECL=LRECL,ERR=940,IOSTAT=IOS,
     +       ACTION='READ')

* Check to see if the two files have the same variables

      IF ( (NC2 .NE. NC1)  .OR.  (NR2 .NE. NR1)  .OR.
     +     (NZ2 .NE. NZ1)  .OR.
     +     (X02 .NE. X01)  .OR.  (DX2 .NE. DX1)  .OR.
     +     (Y02 .NE. Y01)  .OR.  (DY2 .NE. DY1)  .OR.
     +     (ANGLE2 .NE. ANGLE1) ) GOTO 960

* Calculate values used in this program

      XMIN1 = DBLE(X01)
      YMIN1 = DBLE(Y01)
      XMAX1 = DBLE(X01) + DBLE(NC1-1)*DBLE(DX1)
      YMAX1 = DBLE(Y01) + DBLE(NR1-1)*DBLE(DY1)
      DX = DBLE( ABS(DX1) )
      DY = DBLE( ABS(DY1) )

*****************************************
* REPORT SOMETHING ABOUT THE GRIDDED DATA
*****************************************
      WRITE (LUOUT,4050) RIDENT, PGM, NC1, NR1
 4050 FORMAT (1X, A56, /, 1X, A8, /, I5, I5)
      WRITE (LUOUT,*) 'DX,DY,NR,NC', DX1, DY1, NR1, NC1
      WRITE (LUOUT,4055) -XMAX1, -XMIN1, YMIN1, YMAX1
 4055 FORMAT (' MIN Longitude = ', F10.4, ' MAX Longitude = ', F10.4, /,
     +        ' MIN Latitude  = ', F10.4, ' MAX Latitude  = ', F10.4, /)
*****************************************

 9999 RETURN

****************************
* WARNING AND ERROR MESSAGES
****************************

* Grid files do not exist

  910 CONTINUE
      NOGO = .TRUE.
      CLOSE ( LUAREA(ILA) )
      CLOSE ( LUAREA(ILO) )
      GOTO 9999

  920 CONTINUE
      NOGO = .TRUE.
      CLOSE ( LUAREA(ILA) )
      CLOSE ( LUAREA(ILO) )
      GOTO 9999

* Grid file(s) already open

  940 CONTINUE
      NOGO = .TRUE.
      CLOSE ( LUAREA(ILA) )
      CLOSE ( LUAREA(ILO) )
      GOTO 9999

* Grid files do not agree

  960 CONTINUE
      NOGO = .TRUE.
      CLOSE ( LUAREA(ILA) )
      CLOSE ( LUAREA(ILO) )
      GOTO 9999

* Grid files already open

  980 CONTINUE
      NOGO = .TRUE.
      GOTO 9999
      
      END SUBROUTINE OPENFL
      
      SUBROUTINE SURF (XGRID, YGRID, ZEE, AY, BEE, CEE, DEE, IROW, JCOL)

**********************************************************************
** SUBROUTINE SURF: INTERPOLATES THE Z VALUE                         *
**********************************************************************

* Calculated the value of the grid at the point XPT, YPT.  The
* interpolation is done in the index coordinate system for convenience.

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      DOUBLE PRECISION XGRID, YGRID
      DOUBLE PRECISION AY, BEE, CEE, DEE
      DOUBLE PRECISION ZEE, ZEE1, ZEE2, ZEE3, ZEE4
      INTEGER IROW, JCOL

      ZEE1 = AY
      ZEE2 = BEE*(XGRID - DBLE(JCOL) )
      ZEE3 = CEE*(YGRID - DBLE(IROW) )
      ZEE4 = DEE*(XGRID - DBLE(JCOL) )*(YGRID - DBLE(IROW) )
      ZEE  = ZEE1 + ZEE2 + ZEE3 + ZEE4

      RETURN
      END SUBROUTINE SURF
      
      SUBROUTINE TO83 (NOGO, RESP, XPT, YPT, XPT2, YPT2,
     +                 DLAM, DLOM, DLAS, DLOS, ITYPE)

* This subroutine predicts the NAD 83 latitude and longitude values
* given the NAD 27 latitude and longitude values in degree decimal
* format.  In addition, the program returns the shift values between
* The datums in both arc secs and meters.

* All of the predictions are based upon a straight-forward interpolation
* of a gridded data set of datum shifts.  The datum shifts are assumed
* to be provided in the files opened in the NGRIDS subroutine.  The
* common AREAS contains the names of the valid areas while the common
* GDINFO contains the grid variables.  NAREA is the number of areas
* which had data files opened.  A total of two files are necessary for
* each area: one latitude and one longitude shift table (gridded data
* set) expressed in arc seconds.

*       Author:     Warren T. Dewhurst, PH. D.
*                   National Geodetic Survey
*                   November 1, 1989

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      INTEGER MXAREA
      PARAMETER (MXAREA = 8)

      DOUBLE PRECISION XPT, YPT, XPT2, YPT2
      DOUBLE PRECISION XGRID, YGRID
      DOUBLE PRECISION DLAM, DLOM, DLAS, DLOS
      DOUBLE PRECISION DX0, DY0, XMAX0, XMIN0, YMAX0, YMIN0
      INTEGER IROW, JCOL, IAREA, I, NC0, ITYPE
      INTEGER IFLAG1, IFLAG2, N1, N2
      CHARACTER*15 RESP
      LOGICAL NOGO, FLAG

      CHARACTER*15 AREAS
      COMMON /AREAS/ AREAS(MXAREA)

      DOUBLE PRECISION DX, DY, XMAX, XMIN, YMAX, YMIN
      INTEGER NC, NAREA
      COMMON /GDINFO/ DX(MXAREA), DY(MXAREA), XMAX(MXAREA),
     +                XMIN(MXAREA), YMAX(MXAREA), YMIN(MXAREA),
     +                NC(MXAREA), NAREA

      INTEGER LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA
      COMMON /INOUT/ LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA(2*MXAREA)

      CHARACTER*80 CARD
      COMMON /CURNT/ CARD

      SAVE FLAG

      DATA IFLAG1 /1/, IFLAG2 /2/, FLAG /.FALSE./

******************************************************************
*                             INITIALIZE
******************************************************************

      NOGO  =  .FALSE.

****************************************************
* READ WHERE TO GET THE DATA AND HOW IT IS ORGANIZED
****************************************************

* Check to see which set of gridded files XPT,YPT is in.

      DO IAREA = 1, NAREA

        DX0 = DX(IAREA)
        DY0 = DY(IAREA)
        XMAX0 = XMAX(IAREA)
        XMIN0 = XMIN(IAREA)
        YMAX0 = YMAX(IAREA)
        YMIN0 = YMIN(IAREA)
        NC0 = NC(IAREA)

        CALL FGRID (XPT, YPT, DX0, DY0, XMAX0, XMIN0,
     +              YMAX0, YMIN0, XGRID, YGRID, IROW, JCOL, NOGO)

        IF (.NOT. NOGO) EXIT
        
      END DO

      IF (NOGO) GO TO 9999
      
* Point in area number IAREA and named AREAS(IAREA)

        RESP = AREAS(IAREA)
        CALL INTRP (IAREA, IROW, NC0, JCOL, XGRID, YGRID,
     +              XPT, YPT, XPT2, YPT2, DLOS, DLAS, DLAM, DLOM)

* Error Messages

9999  RETURN
      END SUBROUTINE TO83

      SUBROUTINE TRANSF (NOGO, RESP, XPT, YPT, XPT2, YPT2,
     +                   DLAM, DLOM, DLAS, DLOS, KEY, ITYPE)

* This subroutine computes either the forward or inverse coordinate
* transformation depending upon the value of the integer variable 'key'
c 1/20/92 - IF the HPGN option is chosen, statements in this subroutine
c which refer to NAD 27 apply to NAD 83; 
c statements which refer to NAD 83 apply to HPGN -jmb

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      IMPLICIT INTEGER (I-N)
*     IMPLICIT UNDEFINED (A-Z)

      INTEGER MXAREA, ITMAX
      DOUBLE PRECISION SMALL
      PARAMETER (MXAREA = 8, ITMAX = 10, SMALL = 1.0D-9 )

      DOUBLE PRECISION XPT, YPT, XPT2, YPT2
      DOUBLE PRECISION XTEMP, YTEMP, XDIF, YDIF
      DOUBLE PRECISION DLAM, DLOM, DLAS, DLOS
      DOUBLE PRECISION DXLAST, DYLAST
      INTEGER KEY, NUM, ITYPE
      CHARACTER*15 RESP
      LOGICAL NOGO

      INTEGER LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA
      COMMON /INOUT/ LUIN, LUOUT, NOUT, NIN, NAPAR, LUAREA(2*MXAREA)

      IF (KEY .EQ. 1) THEN

**********************
* FOR NAD 27 TO NAD 83
**********************

        CALL TO83 (NOGO, RESP, XPT, YPT, XPT2, YPT2,
     +             DLAM, DLOM, DLAS, DLOS, ITYPE)

      ELSEIF (KEY .EQ. -1) THEN

***************************
* FOR NAD 83 TO NAD 27)
* THIS IS DONE BY ITERATION
***************************

        NUM = 0

**************************************************
* SET THE XPT,YPT TO TEMPORARY VALUES
* (REMEMBER, XPT AND YPT ARE REALLY NAD 83 VALUES)
**************************************************

        XTEMP = XPT
        YTEMP = YPT

**************************************************************
* PRETEND THAT THESE TEMPORARY VALUES ARE REALLY NAD 27 VALUES
* FOR A FIRST GUESS AND COMPUTE PSEUDO-NAD 83 COORDINATES
**************************************************************

  200   CONTINUE
          NUM = NUM + 1

          CALL TO83 (NOGO, RESP, XTEMP, YTEMP, XPT2, YPT2,
     +               DLAM, DLOM, DLAS, DLOS, ITYPE)
          DXLAST = DLOS
          DYLAST = DLAS

**************************************
* COMPARE TO ACTUAL NAD 83 COORDINATES
**************************************

          XDIF = XPT - XPT2
          YDIF = YPT - YPT2

****************************************************************
* COMPUTE A NEW GUESS UNLESS THE DIFFERENCES ARE LESS THAN SMALL
* WHERE SMALL IS DEFINED (ABOVE) TO BE;  SMALL = 1.0D-9
****************************************************************

          IF (NUM .EQ. 1) THEN
            IF (DABS(XDIF) .GT. SMALL) THEN
              XTEMP = XPT - DLOS/3600.0D0
            ENDIF
            IF (DABS(YDIF) .GT. SMALL) THEN
              YTEMP = YPT - DLAS/3600.0D0
            ENDIF
          ELSE
            IF (DABS(XDIF) .GT. SMALL) THEN
              XTEMP = XTEMP - (XPT2 - XPT)
            ENDIF
            IF (DABS(YDIF) .GT. SMALL) THEN
              YTEMP = YTEMP - (YPT2 - YPT)
            ENDIF

          ENDIF

          IF (NUM.GE.ITMAX .OR. (DABS(YDIF).LE.SMALL .AND.
     &                           DABS(XDIF).LE.SMALL)) THEN

******************************
* IF CONVERGED THEN LEAVE LOOP
******************************

            XPT = XTEMP
            YPT = YTEMP
            GOTO 1000
          ENDIF

*******************************
* IF NOT CONVERGED THEN ITERATE
*******************************

        GOTO 200

      ENDIF
 1000 RETURN
      END SUBROUTINE TRANSF



C=======================================================================

C                UTMGEO

C=======================================================================

      SUBROUTINE UTMGEO (KOD,IZONIN,IZONOUT,EAST,NORTH,SLON,SLAT,
     &                   ISPHER)
C***********************************************************************
C     SUBROUTINE UTMGEO (KOD,IZONE,EAST,NORTH,SLON,SLAT,IS)
C
C     The latitude and longitude are returned seconds, not decimal 
C     degrees or other variation
C
C     The exact origins of this routine are unknown, but it is
C     believed to be based on some USGS conversion programs.  RWB
C
C     THIS PROGRAM PERFORMS THE FOLLOWING COMPUTATIONS--
C
C     1.  GEOGRAPHIC TO UTM COORDINATES
C         (ANY ZONE MAY BE EXTENDED THE FULL ZONE WIDTH OF EITHER
C         Adjacent ZONE--9 DEGREES FROM THE CENTRAL MERIDIAN--BY
C         PUNCHING THE ZONE NUMBER WITH THE GEOGRAPHIC COORDINATES.
C         IF NO ZONE NUMBER IS INPUT, THE NORMAL 6-DEGREE BAND IS USED)
C
C     2.  UTM TO GEOGRAPHIC COORDINATES
C         (WHEN CONVERTING PTS IN THE SOUTHERN HEMISPHERE, THE ZONE
C         NUMBER IS PRECEDED BY A MINUS SIGN.  EXTENDED ZONE BOUNDARIES
C         MAY BE USED, AS ABOVE)
C
C     FOR ANY OF THE ABOVE, ONE OF FIVE SHEROIDS MAY BE SELECTED BY
C     AND CONVERGENCE ANGLE MAY BE REQUESTED BY A 1 IN COL 80.
C     AN INTEGER IN COL 60 (CLARKE 1866 IS DEFAULT).  SCALE FACTOR
C
C
C     NB:
C     WITHIN UTMGEO, SOUTHERN HEMISPHERE LATITUDES AND EASTERN HEMISPHERE 
C     LONGITUDES ARE CONSIDERED NEGATIVE.  CALLING ROUTINES IN AERMOD
C     MODEL COMPONENTS ASSUME NEGATIVE FOR WEST LONGITUDE. THE ADJUSTMENT
C     TO POSITIVE FOR WEST LONGITUDE IS MADE UPON ENTERING UTMGEO, AND 
C     RESULTS ARE ADJUSTED BACK TO NEGATIVE FOR WEST BEFORE LEAVING UTMGEO.
C-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER (KIND=4)   :: KOD
      INTEGER (KIND=4)   :: IZONIN
      INTEGER (KIND=4)   :: IZONOUT
      DOUBLE PRECISION   :: EAST
      DOUBLE PRECISION   :: NORTH
      DOUBLE PRECISION   :: SLON
      DOUBLE PRECISION   :: SLAT
      INTEGER (KIND=4)   :: ISPHER  
            
      DOUBLE PRECISION :: A(16),B(4),UTZ,SK,THET
      INTEGER :: IZONE, IZON2, IERR, IDLON


c      DOUBLE PRECISION :: SLAT,SLON,NORTH,EAST,A(16),B(4),UTZ,SK,THET
c      INTEGER :: ISPHER, KOD, IZONE, IZON2, IERR, IDLON
c      INTEGER :: IZONIN, IZONOUT

C     DETERMINE SPHEROID PARAMETERS FROM ISPHER INPUT
C                            0=CLARK 1866(DEFAULT) / NAD 27
C                            1=CLARK1880
C                            2=BESSEL
C                            3=MODIFIED MURCURY 1968
C                            4=GRS 80 / NAD 83

C     If a spheroid is required that is not defined here, it is rec-
C     ommended that the appropriate semi-major and semi-minor axes
C     values be substituted for one of the unused parameter sets.
C
C

      IF (KOD .EQ. 555) THEN
C----    CONVERSION IS FROM LAT/LON TO UTM
         IZONE  = IZONIN
         IZON2  = IZONE

C----    Adjust for longitudes that cross the 180E/180W meridian.
C        Include tolerance of about 1m (0.00001 deg) to avoid 
C        reversing sign due to precision.
         if (slon .lt. -180.00001D0*3600.0D0) then
            slon = 360.0D0*3600.0D0 + slon
         else if (slon .gt. 180.00001D0*3600.0D0) then
            slon = -1.0D0*(360.0D0*3600.0D0 - slon)
         end if
         
C----    Reverse sign of input longitude;
C        UTMGEO uses positive for west longitude; calling 
C        routine uses standard convention of negative for west longitude
         SLON   = -1.0D0*SLON
         IDLON  = ABS(IDINT(SLON / 3600.0D0))
         
      ELSE IF (KOD .EQ. 333) THEN
C----    CONVERSION IS FROM UTM TO LAT/LON; ASSIGN ZONE
         IZONE  = IZONIN
         IZON2  = IZONE
      END IF

      A(5)=5.0D5
      A(6)=0.0D0

C---- Adjust A(6) value for South Latitude (SLAT<0 or IZONE<0)
      IF(KOD.EQ.555 .AND. SLAT.LT.0.0D0) A(6)=10.0D6
      IF(IZONE.LT.0) A(6)=10.0D6

      A(7) = 0.0D0
      A(8) = 0.9996D0

C---- Define axes for the datum
      SELECT CASE (ISPHER)

         CASE (0)
            A(15) = 6378206.4D0
            B(1)  = 6356583.8D0

         CASE (1)
            A(15) = 6378249.1450D0
            B(1)  = 6356514.86955D0

         CASE (2)
            A(15) = 6377397.155D0
            B(1)  = 6356078.96284D0

         CASE (3)
            A(15) = 6378150.0D0
            B(1)  = 6356768.337D0

         CASE (4)
            A(15) = 6378137.0D0
            B(1)  = 6356752.31414D0

      END SELECT

      A(16) = ((A(15)-B(1))/A(15))*((A(15)+B(1))/A(15))

C---- Compute coefficients for conversions
      CALL TMCOF(A)

C---- Test for type of conversion, geodetic to UTM or UTM to geodetic
      IF (KOD .EQ. 555) THEN
C----    Convert geodetic to UTM coordinates

C----    Test for zone input on geodetic to UTM indicating over ride of
C        normal 6 degree longitude band.

         IF(IZONE.EQ.0)GO TO 35
         IZONOUT = IZONE
         GO TO 22

C----    Compute central meridiam in seconds for enforced zone.
         IZONE=IZON2
  
  22     CONTINUE

         IF(ABS(IZONE) .LE. 30) THEN
            UTZ=30.0D0-DBLE(ABS(IZONE))
            A(9)=((UTZ*6.0D0)+3.0D0)*3600.0D0

         ELSE
            UTZ=DBLE(ABS(IZONE))-30.0D0
            A(9)=((UTZ*6.0D0)-3.0D0)*(-3600.0D0)
         END IF

         IF(IZON2.NE.0)GO TO 50
         GO TO 40

C----    Compute UTM zone (IZONE) and central meridian in seconds (A9)
C        for geodetic to UTM conversion where zone is not input.

  35     IZONE=30-(IDLON/6)
         IF(SLON.LT.0.0D0)IZONE=IDLON/6+31
         UTZ=30.0D0-DBLE(IZONE)
         A(9)=((UTZ*6.0D0)+3.0D0)*3600.0D0
         IF( SLAT.LT.0.0D0 )THEN
C----       Assign negative zone for south latitudes         
            IZONE = IZONE*(-1)
         END IF

  40     CONTINUE

  50     CALL TMFWD(SLAT,SLON,NORTH,EAST,A,IERR,SK,THET)
  
         IF( SLAT.LT.0.0D0 .AND. IZONE.GT.0 )THEN
C----       Assign negative zone for south latitudes         
            IZONOUT = IZONE*(-1)
         ELSE
            IZONOUT = IZONE
         END IF
         
C----    Reverse sign of longitude back to match input (negative for W)
         SLON = -1.0D0*SLON
                  
         GO TO 150

      ELSE IF( KOD .EQ. 333) THEN
C----    Convert UTM coordinates to geodetic

C----    Compute central meridian in seconds from IZONE input
         UTZ=30.0D0-DBLE(ABS(IZONE))
         A(9)=((UTZ*6.0D0)+3.0D0)*3600.0D0

         CALL TMINV(NORTH,EAST,SLAT,SLON,A,IERR,SK,THET)
         
C----    Adjust for longitudes that cross the 180E/180W meridian         
         if (slon .lt. -180.0D0*3600.0D0) then
            slon = 360.0D0*3600.0D0 + slon
         else if (slon .gt. 180.0D0*3600.0D0) then
            slon = -1.0D0*(360.0D0*3600.0D0 - slon)
         end if

C----    Reverse sign of longitude before returning (output negative for W)         
         SLON = -1.0D0*SLON
         
      END IF

  150 CONTINUE
  
      RETURN
      END SUBROUTINE UTMGEO


C***********************************************************************
C     SUBROUTINE TMCOF(A)
C
C     Purpose: To set up the coefficients to convert geodetic to
C              rectifying latitude and conversely
C
C     Called By:  UTMGEO
C
C     Calls To: <none>
C-----------------------------------------------------------------------
      SUBROUTINE TMCOF(A)

      IMPLICIT NONE

      DOUBLE PRECISION A(16),FAC
      A(10) = (((A(16)*(7.0D0/3.2D1)+(5.0D0/1.6D1))*A(16)+0.5D0)*A(16)+
     &         1.0D0)*A(16)*0.25D0

      A(1)= -(((A(10)*(1.95D2/6.4D1)+3.25D0)*A(10)+3.75D0)*A(10)+3.0D0)*
     &     A(10)

      A(2)=(((1.455D3/3.2D1)*A(10)+(7.0D1/3.0D0))*A(10)+7.5D0)*A(10)**2

      A(3)=-((7.0D1/3.0D0)+A(10)*(9.45D2/8.0D0))*A(10)**3

      A(4)=(3.15D2/4.0D0)*A(10)**4

      A(11)=(((7.75D0-(6.57D2/6.4D1)*A(10))*A(10)-5.25D0)*A(10)+3.0D0)*
     &      A(10)

      A(12)=(((5.045D3/3.2D1)*A(10)-(1.51D2/3.0D0))*A(10)+10.5D0)*
     &      A(10)**2

      A(13)=((1.51D2/3.0D0)-(3.291D3/8.0D0)*A(10))*A(10)**3

      A(14)=(1.097D3/4.0D0)*A(10)**4
C     A(1) to A(4) are for geodetic to rectifying latitude
C        conversion while A(11) to A(14) are coefficients for
C        rectifying to geodetic conversion.

      FAC=A(10)*A(10)

      A(10)=(((2.25D2/6.4D1)*FAC+2.25D0)*FAC+1.0D0)*(1.0D0-FAC)*
     &      (1.0D0-A(10))*A(15)

C     A(10) is now set to radius of sphere with great circle length
C        equal to spheroid meridian length.

      RETURN
      END SUBROUTINE TMCOF


C***********************************************************************
C     SUBROUTINE TMINV(NORTH,EAST,SLAT,SLON,A,IERR,SK,THET)
C
C     PURPOSE: To compute latitude and longitude IN SECONDS (slat and
C              slon) from given rectangular coordinates x and y for
C              transverse mercator projection.
C
C     A = array of parameters used in computation, described by comments
C          for SUBR.TMFWD
C     IERR is set to 1 if grid distance from central meridian exceeds
C          0.2 of spheroid semimajor axis numerically or if absolute
C          value of rectifying latitude exceeds 1.47 radians.
C
C     ASSUMPTIONS
C          South latitudes and east longitude are negative.
C
C     MODIFICATIONS:
C          Moved calculation of B(10) to precede the IF test on values
C          out of range.   R.W. Brode, USEPA/OAQPS/AQMG, 01/12/09
C
C     Called By:  UTMGEO
C
C     Calls to:  <none>
C-----------------------------------------------------------------------
      SUBROUTINE TMINV(NORTH,EAST,SLAT,SLON,A,IERR,SK,THET)

      IMPLICIT NONE

      DOUBLE PRECISION SLAT,SLON,A(16),B(12),SINW,COSW,RN,T,TS,ETAS,
     & NORTH,EAST,X,Y,SK,BN,BNS,THET
      INTEGER IERR

      Y=NORTH
      X=EAST

      IERR=0
      B(9)=((A(5)-X)*1.0D-6)/A(8)
      B(10)=((Y-A(6))/A(8)+A(7))/A(10)

      IF ((DABS(B(9))-1.0D-7*A(15)*2.0D0 .GT. 0.0D0) .OR.
     &    (DABS(B(10))-1.47D0 .GT. 0.0D0)) THEN
         IERR=1
         SLAT=0.0D0
         SLON=0.0D0

      ELSE
         SINW=DSIN(B(10))
         COSW=DCOS(B(10))

         B(12)=COSW*COSW

         B(11)=(((A(14)*B(12)+A(13))*B(12)+A(12))*B(12)+A(11))
     &         *SINW*COSW+B(10)

         SINW=DSIN(B(11))
         COSW=DCOS(B(11))

         RN=DSQRT(1.0D0-A(16)*SINW*SINW)*1.0D6/A(15)

         T=SINW/COSW
         TS=T*T

         B(12)=COSW*COSW

         ETAS=A(16)*B(12)/(1.0D0-A(16))

         B(1)=RN/COSW

         B(2)=-T*(1.0D0+ETAS)*RN*RN/2.0D0

         B(3)=-(1.0D0+2.0D0*TS+ETAS)*B(1)*RN*RN/6.0D0

         B(4)=(((-6.0D0-ETAS*9.0D0)*ETAS+3.0D0)*TS+(6.0D0-ETAS*3.0D0)
     &         *ETAS+5.0D0)*T*RN**4/24.0D0

         B(5)=((TS*24.0D0+ETAS*8.0D0+28.0D0)*TS+ETAS*6.0D0+5.0D0)*
     &       B(1)*RN**4/120.0D0

         B(6)=(((ETAS*45.0D0-45.0D0)*TS+ETAS*162.0D0-90.0D0)*TS
     &        -ETAS*107.0D0-61.0D0)*T*RN**6/720.0D0

         B(7)=-(((TS*720.0D0+1320.0D0)*TS+662.0D0)*TS+61.0D0)*B(1)
     &         *RN**6/5040.0D0

         B(8)=(((TS*1575.0D0+4095.0D0)*TS+3633.0D0)*TS+1385.0D0)*T
     &         *RN**8/40320.0D0

         B(10)=B(9)*B(9)

C----    The values of SLAT and SLON are in seconds, not decimal degrees
         SLAT=((((B(8)*B(10)+B(6))*B(10)+B(4))*B(10)+B(2))*B(10)
     &         +B(11))*206264.8062470964D0
         SLON= (((B(7)*B(10)+B(5))*B(10)+B(3))*B(10)+B(1))*B(9)*
     &          206264.8062470964D0 + A(9)

crwb---  The following variables are not needed for AERMOD components:
crwb     Assign dummy values and return
         
         BN   = 0.0D0
         BNS  = 0.0D0 
         SK   = 0.0D0
         THET = 0.0D0
         
      END IF

      RETURN
      END SUBROUTINE TMINV

C***********************************************************************
C     SUBROUTINE TMFWD(SLAT,SLON,NORTH,EAST,A,IERR,SK,THET)

C     PURPOSE: To convert latitude and longitude in seconds (slat and
C              slon) to X and Y on transverse mercator projection.
C
C     A(1) to A(4) are coefficients used to convert geodetic latitude
C          to rectifying latitude,
C     A(5) is false easting,
C     A(6) is false northing,
C     A(8) is scale factor at central meridian
C     A(9) is central meridian in seconds
C     A(10) is radius of sphere having a great circle length equal to
C           spheroid meridian length
C     A(11) to A(14) are coefficients to convert rectifying latitude to
C           geodetic latitude
C     A(15) is semimajor axis of spheroid
C     A(16) is eccentricity squared.

C     IERR is set to 1 if lat exceeds 84 degrees, or
C          long exceeds 0.16 radians
C
C     MODIFICATIONS:
C          Moved calculation of B(10) to precede the IF test on values
C          out of range.   R.W. Brode, USEPA/OAQPS/AQMG, 01/12/09
C
C     Called By: UTMGEO
C
C     Calls To: <none>
C-----------------------------------------------------------------------
      SUBROUTINE TMFWD(SLAT,SLON,NORTH,EAST,A,IERR,SK,THET)

      IMPLICIT NONE

      DOUBLE PRECISION SLAT,SLON,A(16),B(12),SINP,COSP,RN,T,TS,ETAS,
     &                 NORTH,EAST,SK,THET
      INTEGER IERR

      IERR=0

      B(10)=(A(9)-SLON) *4.84813681109536D-6
      
      IF((DABS(SLAT)-302400.0D0 .GT. 0.0D0) .OR.
     &   (DABS(B(10))-0.16D0).GT. 0.0D0) THEN
         IERR=1
         EAST=0.0D0
         NORTH=0.0D0

      ELSE
        B(9)=SLAT*4.84813681109536D-6

        SINP=DSIN(B(9))
        COSP=DCOS(B(9))

        RN=A(15)/DSQRT(1.0D0-A(16)*SINP*SINP)

        T=SINP/COSP
        TS=T*T

        B(11)=COSP*COSP

        ETAS=A(16)*B(11)/(1.0D0-A(16))

        B(1)=RN*COSP

        B(3)=(1.0D0-TS+ETAS)*B(1)*B(11)/6.0D0

        B(5)=((TS-18.0D0)*TS+5.0D0+(14.0D0-58.0D0*TS)*ETAS)*B(1)*
     &       B(11)*B(11)/120.0D0

        B(7)=(((179.0D0-TS)*TS-479.0D0)*TS+61.0D0)*B(1)*B(11)**3
     &       /5040.0D0

        B(12)=B(10)*B(10)

        EAST=(((B(7)*B(12)+B(5))*B(12)+B(3))*B(12)+B(1))*B(10)*A(8)
     &       +A(5)

        B(2)=RN*B(11)*T/2.0D0

        B(4)=(ETAS*(9.0D0+4.0D0*ETAS)+5.0D0-TS)*B(2)*B(11)/12.0D0

        B(6)=((TS-58.0D0)*TS+61.0D0+(270.0D0-330.0D0*TS)*ETAS)*B(2)*
     &       B(11)*B(11)/360.0D0

        B(8)=(((543.0D0-TS)*TS-3111.0D0)*TS+1385.0D0)*B(2)*B(11)**3/
     &        20160.0D0

        NORTH=(((B(8)*B(12)+B(6))*B(12)+B(4))*B(12)+B(2))*B(12)+
     &      ((((A(4)*B(11)+A(3))*B(11)+A(2))*B(11)+A(1))
     &      *SINP*COSP+B(9))*A(10)

        NORTH=(NORTH-A(7))*A(8)+A(6)

      END IF

      RETURN
      END SUBROUTINE TMFWD
      
      END module Geographic

