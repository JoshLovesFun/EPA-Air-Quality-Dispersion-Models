
        PROGRAM MTXCALC

C***********************************************************************
C Version "%W% %P% %G% %U% $Id: mtxcalc.f 607 2010-02-19 12:57:18Z coats@bdsl $"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2002-2010 Baron Advanced Meteorological Systems. LLC.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  program body starts at line  176
C
C  DESCRIPTION:
C       Take GRIDDESC names for input and output grids, and produce
C       a sparse matrix file that has the transform matrix to convert
C       data from input to output, based on input-area fractions.
C
C  PRECONDITIONS REQUIRED:
C       setenv <logical names>  <path-names>
C       input and output grids have same coordinate system
C       output grid is entirely contained in the input grid
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       I/O API
C       STRLIST()
C
C  REVISION  HISTORY:
C       Prototype 11/2000 by Carlie J. Coats, Jr., NCSC
C       Revised   11/2001 by CJC for I/O API Version 2.1
C       Revised   10/2005 by CJC for I/O API Version 2.1
C       Version   10/2002 by CJC for I/O API Version 2.2:  support for
C       many additional coordinate transformations, direct calls to GCTP
C       Version   10/2005 by CJC:  handle negative XCELL, YCELL
C       Version   11/2005 by CJC:  eliminate unused vbles
C       Version    1/2006 by CJC:  Bug-fix to off-by-one-half error
C       reported by Dr. Michael Bane, UManchester, UK, at approx
C       Lines 732-2
C       Version   12/2006 by CJC:  Bug-fix to GTYPES menu
C       Version    6/2008 by CJC:  Albers support
C       Version    8/2008 by CJC:  USE M3UTILIO, MATXATTS to put
C       grid-attributes into matrix.
C       Version   12/2008 by CJC:  heuristic to compensate for WMO screw-up
C       that declares all longitudes should be positive
C       Version   02/2010 by CJC:  bug-fix for output transverse or eqatorial
C       Mercator projections.  Problem reported by Steve Kim, UCambridge, UK.
C***********************************************************************

      USE M3UTILIO
      USE MATXATTS

      IMPLICIT NONE


C...........   PARAMETERS and their descriptions:

      REAL*8      PI
      REAL*8      PI180
      REAL*8      REARTH
      REAL*8      DG2M   ! latitude degrees to meters

      PARAMETER ( PI     = 3.14159 26535 89793 23846 26433 83279 D0 ,
     &            PI180  = PI / 180.0,
     &            REARTH = 6367333.0D0 ,
     &            DG2M   = REARTH * PI180 )


C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER     IARGC
C       EXTERNAL    IARGC


C...........   LOCAL VARIABLES and their descriptions:

        INTEGER         LOGDEV  !  unit number for log file
        INTEGER         ARGCNT  !  number of command-line args, from IARGC()
        INTEGER         STATUS
        INTEGER         MATDEV  ! optional ASCII output file

        CHARACTER*16    IGRID   !  GRIDDESC name, parameters for  input grid
        INTEGER         NCOLS1
        INTEGER         NROWS1
        INTEGER         NTHIK1
        INTEGER         GDTYP1
        REAL*8          P_ALP1      ! first, second, third map
        REAL*8          P_BET1      ! projection descriptive
        REAL*8          P_GAM1      ! parameters.
        REAL*8          XCENT1      ! lon for coord-system X=0
        REAL*8          YCENT1      ! lat for coord-system Y=0
        REAL*8          XORIG1      ! X-coordinate origin of grid (map units)
        REAL*8          YORIG1      ! Y-coordinate origin of grid
        REAL*8          XCELL1      ! X-coordinate cell dimension
        REAL*8          YCELL1      ! Y-coordinate cell dimension

        CHARACTER*16    OGRID   !  GRIDDESC name, parameter for output grid
        INTEGER         NCOLS2
        INTEGER         NROWS2
        INTEGER         NTHIK2
        INTEGER         GDTYP2
        REAL*8          P_ALP2      ! first, second, third map
        REAL*8          P_BET2      ! projection descriptive
        REAL*8          P_GAM2      ! parameters.
        REAL*8          XCENT2      ! lon for coord-system X=0
        REAL*8          YCENT2      ! lat for coord-system Y=0
        REAL*8          XORIG2      ! X-coordinate origin of grid (map units)
        REAL*8          YORIG2      ! Y-coordinate origin of grid
        REAL*8          XCELL2      ! X-coordinate cell dimension
        REAL*8          YCELL2      ! Y-coordinate cell dimension
        REAL*8          DDX2      ! 1/xcell
        REAL*8          DDY2      ! 1/ycell

C.......   Arguments for GTPZ0:

        REAL*8          DSCR            !  scratch variables
        INTEGER         DEG, MNT        !  scratch variables
        REAL*8          CRDIN( 2 )      !  input coordinates x,y
        INTEGER*4       INSYS           !  input projection code
        INTEGER*4       INZONE          !  input utm zone, etc.
        REAL*8          TPAIN( 15 )     !  input projection parameters
        INTEGER*4       INUNIT          !  input units code
        INTEGER*4       INSPH           !  spheroid code
        INTEGER*4       IPR             !  error print flag
        INTEGER*4       JPR             !  projection parameter print flag
        INTEGER*4       LEMSG           !  error message unit number
        INTEGER*4       LPARM           !  projection parameter unit number
        REAL*8          CRDIO( 2 )      !  output coordinates x,y
        INTEGER*4       IOSYS           !  output projection code
        INTEGER*4       IOZONE          !  output utm zone, etc.
        REAL*8          TPOUT( 15 )     !  output projection parameters
        INTEGER*4       IOUNIT          !  output units code
        INTEGER*4       IOSPH           !  spheroid code
        INTEGER*4       LN27            !  NAD1927 file unit number
        INTEGER*4       LN83            !  NAD1983 file unit number
        CHARACTER*128   FN27            !  NAD1927 file name
        CHARACTER*128   FN83            !  NAD1983 file name
        INTEGER*4       LENGTH          !  NAD* record-length
        INTEGER*4       IFLG            !  error flag

        INTEGER         NX, NY, NFRACS
        INTEGER         I, J, K, N, C, R
        INTEGER         COL, ROW
        INTEGER         COLMAX, ROWMAX
        INTEGER         COLMIN, ROWMIN
        INTEGER         ISTAT
        LOGICAL         EFLAG
        CHARACTER*16    SCRBUF
        CHARACTER*256   MESG

        REAL            DDXY
        REAL            NUMERX, DENOMX      !  worst-case XCELL1,2 in meters
        REAL            NUMERY, DENOMY      !  worst-case YCELL1,2 in meters
        REAL*8          DX, DY
        REAL*8          XX0, YY0, X, Y, XADJ

        CHARACTER*24::  GTYPES( 0:10 ) = (/
     &                  'Unknown/Invalid       ',
     &                  'Latitude-Longitude    ',
     &                  'Lambert               ',
     &                  'General Mercator      ',
     &                  'General Stereographic ',
     &                  'UTM                   ',
     &                  'Polar Stereographic   ',
     &                  'Equatorial Mercator   ',
     &                  'Transverse Mercator   ',
     &                  'Albers Equal-Area     ',
     &                  'Unknown/Invalid       ' /)

        INTEGER, ALLOCATABLE::   ISCR( :, : )
        INTEGER, ALLOCATABLE::   ICEL( : )
        INTEGER, ALLOCATABLE::   OCEL( : )
        REAL,    ALLOCATABLE::   FRAC( : )
        REAL,    ALLOCATABLE::   MATX( : )

C***********************************************************************
C   begin body of program MTXCALC

        LOGDEV = INIT3()
        ARGCNT = IARGC()
        EFLAG = .FALSE.
        WRITE( *, '( 5X, A )' )
     & ' ',
     & 'Program MTXCALC to produce a sparse grid-to-grid transform',
     & 'matrix for specified input and output grids, and write the',
     & 'result to a sparse-matrix file.  The algorithm employed is',
     & 'to sub-sample each input cell using a subsampled grid of size',
     & 'COL_REFINEMENT by ROW_REFINEMENT in each input grid cell,',
     & 'where COL_REFINEMENT and ROW_REFINEMENT are environment',
     & 'variables set by the user (with defaults to a 100 by 100',
     & 'refinement, in case they are not set).',
     & ' ',
     & 'Usage:  MTXCALC [INGRID OUTGRID] <and follow the prompts>',
     & ' ',
     & 'where <INGRID> and <OUTGRID> are the GRIDDESC names of the',
     & 'input and output grids for the transformation.  If these are',
     & 'not entered at the command line, the program will prompt you',
     & 'for them.',
     & ' ',
     & 'PRECONDITIONS REQUIRED:',
     & ' ',
     & '    setenv COL_REFINEMENT col-sampling factor (default 100)',
     & '    setenv ROW_REFINEMENT row-sampling factor (default 100)',
     & '    setenv GRIDDESC   <path name for grid descriptions file>',
     & '    setenv MATRIX        <path name for I/O API matrix file>',
     & '    setenv MATTXT        <path name for  ASCII  matrix file,',
     & '                         or "NONE" to suppress ASCII output>',
     & ' ',
     & 'The following input and output coordinate systems are',
     & 'supported:',
     &  ' ',
     & '    Latitude-Longitude    ',
     & '    Lambert Conformal Conic',
     & '    UTM                   ',
     & '    Polar Stereographic   ',
     & '    Transverse Mercator   ',
     & '    Equatorial Mercator   ',
     & '    Albers Equal-Area Conic',
     &  ' ',
     &'See URL  http://www.emc.mcnc.org/products/ioapi/AA.html#tools',
     &' ',
     &'Program copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.',
     &'and (C) 2002-2008 Baron Advanced Meteorological Systems, LLC',
     &'Released under Version 2 of the GNU General Public License.',
     &'See enclosed GPL.txt, or URL',
     &'http://www.gnu.org/copyleft/gpl.html',
     &' ',
     &'Comments and questions are welcome and can be sent to',
     &' ',
     &'    Carlie J. Coats, Jr.    coats@baronams.com',
     &'    920 Main Campus Drive, Suite 101',
     &'    Raleigh, NC 27606',
     &' ',
     &'See URL  http://www.baronams.com/products/ioapi/AA.html#tools',
     &' ',
     &'Program version: ',
     &'$Id:: mtxcalc.f 607 2010-02-19 12:57:18Z coats@bdsl           $',
     &'Program release tag: $Name$',
     &' '

        IF ( .NOT. GETYN( 'Continue with program?', .TRUE. ) ) THEN
            CALL M3EXIT( 'MTXCALC', 0, 0,
     &                   'Program terminated at user request', 2 )
        ELSE IF ( ARGCNT .EQ. 2 ) THEN
            CALL GETARG( 1, IGRID )
            CALL GETARG( 2, OGRID )
        ELSE IF ( ARGCNT .EQ. 0 ) THEN
            CALL GETSTR( 'Enter name for  input grid', 'IGRID', IGRID )
            CALL GETSTR( 'Enter name for output grid', 'OGRID', OGRID )
        ELSE
            MESG= 'Usage error:  incorrect number of arguments'
            CALL M3EXIT( 'MTXCALC', 0, 0, MESG, 2 )
        END IF

        MATDEV = PROMPTFFILE(
     &  'Enter logical name for ASCII MATRIX file, or "NONE"',
     &           .FALSE., .TRUE., 'MATTXT', 'MTXCALC' )
        IF ( MATDEV .EQ. -1 ) THEN
            MESG= 'Error opening ASCII matrix file'
            CALL M3EXIT( 'MTXCALC', 0, 0, MESG, 2 )
        ELSE IF ( MATDEV .GE. 0 ) THEN
            WRITE( MATDEV, '(A)' )
     &        '# Fractions for sparse transform matrix'
            WRITE( MATDEV, '(A)' )
     &        '# FRAC=Area(INCELL&OUTCELL)/Area(INCELL)'
            WRITE( MATDEV, '(A)' )
     &        '# INGRID    ' // IGRID
            WRITE( MATDEV, '(A)' )
     &        '# OUTGRID   ' // OGRID
        END IF


C.......   Read the input and output grid parameters from GRIDDESC:

        IF ( .NOT. DSCGRID( IGRID, SCRBUF, GDTYP1,
     &              P_ALP1, P_BET1,P_GAM1, XCENT1, YCENT1,
     &              XORIG1, YORIG1, XCELL1, YCELL1,
     &              NCOLS1, NROWS1, NTHIK1 ) ) THEN

            MESG   = 'Grid "' // TRIM( IGRID ) //
     &               '" not found in GRIDDESC file'
            CALL M3EXIT( 'MTXCALC', 0, 0, MESG, 2 )

        END IF  ! if dscgrid() failed for input grid

        CALL M3MSG2( ' ' )
        I = MAX( 0, MIN( GDTYP1, 9 ) )
        MESG = 'Input grid:  "' // TRIM( IGRID ) //
     &          '" is of type "' // TRIM( GTYPES( I ) ) // '"'
        CALL M3MSG2( MESG )
        WRITE( MESG, '( A, 2X, 3( 1PE15.7, 2X ) )' )
     &      'Defining angles', P_ALP1, P_BET1,P_GAM1
        CALL M3MSG2( MESG )
        WRITE( MESG, '( A, 2X, 2( 1PE15.7, 2X ) )' )
     &      'Coordinate Origin', XCENT1, YCENT1
        CALL M3MSG2( MESG )
        WRITE( MESG, '( A, 2X, 2( 1PE15.7, 2X ) )' )
     &      '(1,1) Grid Corner', XORIG1, YORIG1
        CALL M3MSG2( MESG )
        WRITE( MESG, '( A, 2X, 2( 1PE15.7, 2X ) )' )
     &      'Cell Size', XCELL1, YCELL1
        CALL M3MSG2( MESG )
        WRITE( MESG, '( A, 2X, 2( I6, 2X, A ) )' )
     &      'Dimensions:', NCOLS1, 'cols', NROWS1, 'rows'
        CALL M3MSG2( MESG )

      IF ( .NOT. DSCGRID( OGRID, SCRBUF, GDTYP2,
     &              P_ALP2, P_BET2,P_GAM2, XCENT2, YCENT2,
     &              XORIG2, YORIG2, XCELL2, YCELL2,
     &              NCOLS2, NROWS2, NTHIK2 ) ) THEN

            MESG   = 'Grid "' // TRIM( OGRID ) //
     &               '" not found in GRIDDESC file'
            CALL M3EXIT( 'MTXCALC', 0, 0, MESG, 2 )

        END IF                  ! if dscgrid() failed for output grid

        CALL M3MSG2( ' ' )
        I = MAX( 0, MIN( GDTYP2, 9 ) )
        MESG = 'Output grid:  "' // TRIM( OGRID ) //
     &          '" is of type "' // TRIM( GTYPES( I ) ) // '"'
        CALL M3MSG2( MESG )
        WRITE( MESG, '( A, 2X, 3( 1PE15.7, 2X ) )' )
     &      'Defining angles', P_ALP2, P_BET2,P_GAM2
        CALL M3MSG2( MESG )
        WRITE( MESG, '( A, 2X, 2( 1PE15.7, 2X ) )' )
     &      'Coordinate Origin', XCENT2, YCENT2
        CALL M3MSG2( MESG )
        WRITE( MESG, '( A, 2X, 2( 1PE15.7, 2X ) )' )
     &      '(1,1) Grid Corner', XORIG2, YORIG2
        CALL M3MSG2( MESG )
        WRITE( MESG, '( A, 2X, 2( 1PE15.7, 2X ) )' )
     &      'Cell Size', XCELL2, YCELL2
        CALL M3MSG2( MESG )
        WRITE( MESG, '( A, 2X, 2( I6, 2X, A ) )' )
     &      'Dimensions:', NCOLS2, 'cols', NROWS2, 'rows'
        CALL M3MSG2( MESG )


C.......   Compute longitude adjustment to compensate for WMO screw-up
C.......   that declares all longitudes should be positive:

        XADJ = 0.0D0
        IF       ( GDTYP1.EQ.LATGRD3 .AND. XORIG1.GT.0.0D0 ) THEN     !  WMO LL input
            IF ( ( GDTYP2.EQ.LATGRD3 .AND. XORIG2.LT.0.0D0 ) .OR.
     &           ( GDTYP2.GT.0       .AND. XCENT2.LT.0.0D0 ) ) THEN
                XADJ = 360.0D0
                CALL M3MSG2( 'Longitude adjustment:  360.0D0' )
            END IF
        ELSE IF  ( GDTYP2 .EQ. LATGRD3 .AND. XORIG2 .GT. 0.0D0 ) THEN  !  WMO LL output
            IF ( ( GDTYP1 .EQ. LATGRD3 .AND. XORIG1 .LT. 0.0D0 ) .OR.
     &           ( GDTYP1 .GT. 0       .AND. XCENT1 .LT. 0.0D0 ) ) THEN
                XADJ = -360.0D0
                CALL M3MSG2( 'Longitude adjustment: -360.0D0' )
            END IF
        END IF

C.......   Compute sampling ratios:

        IF ( XCELL1 .LE. XCELL2 ) THEN
            C = 100
        ELSE
            C = 100 * ( 1 + NINT( XCELL2 / XCELL1 ) )
        END IF

        IF ( YCELL1 .LE. YCELL2 ) THEN
            R = 100
        ELSE
            R = 100 * ( 1 + NINT( YCELL2 / YCELL1 ) )
        END IF
        NX = ENVINT( 'COL_REFINEMENT', 'column-factor', C, STATUS )
        IF ( STATUS .GT. 0 ) THEN
            CALL M3EXIT( 'IC_ADJUST', 0, 0,
     &          'Bad environment variable "COL_REFINEMENT"', 2 )
        END IF

        NY = ENVINT( 'ROW_REFINEMENT','row-factor', R, STATUS )
        IF ( STATUS .GT. 0 ) THEN
            CALL M3EXIT( 'IC_ADJUST', 0, 0,
     &          'Bad environment variable "ROW_REFINEMENT"', 2 )
        END IF

        DX = XCELL1 / DBLE( NX )
        DY = YCELL1 / DBLE( NY )

        CALL M3MSG2( ' ' )
        WRITE( MESG, '( A, 2( I8, 2X, A ) )' )
     &  'Sampling ratios:', NX, 'per input column', NY, 'per input row'
        CALL M3MSG2( MESG )
        IF ( GDTYP1 .EQ. LATGRD3 ) THEN
            WRITE( MESG, '( A, 2( 1PD15.7, 2X, A ) )' )
     &          'Sampling cellsize:', DX, 'by', DY, 'degrees'
        ELSE
            WRITE( MESG, '( A, 2( 1PD15.7, 2X, A ) )' )
     &          'Sampling cellsize:', DX, 'by', DY, 'meters'
        END IF
        CALL M3MSG2( MESG )


C.......   Set up arguments for GTP0:

        TPAIN = 0.0D0           !  array assignments
        TPOUT = 0.0D0
        IF ( GDTYP1 .EQ. LAMGRD3 ) THEN

            DENOMX = SNGL( XCELL1 )
            DENOMY = SNGL( YCELL1 )

            DSCR = P_ALP1
            DEG  = INT( DSCR )                              !  int degrees
            DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
            MNT  = INT( DSCR )                              !  int minutes
            DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
            TPAIN( 3 ) = DSCR + 1000.0D0 * ( MNT + 1000*DEG ) !  dddmmmsss.sssD0

            DSCR = P_BET1
            DEG  = INT( DSCR )                              !  int degrees
            DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
            MNT  = INT( DSCR )                              !  int minutes
            DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
            TPAIN( 4 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

            DSCR = P_GAM1
            DEG  = INT( DSCR )                              !  int degrees
            DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
            MNT  = INT( DSCR )                              !  int minutes
            DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
            TPAIN( 5 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

            DSCR = YCENT1
            DEG  = INT( DSCR )                              !  int degrees
            DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
            MNT  = INT( DSCR )                              !  int minutes
            DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
            TPAIN( 6 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

            INSYS  = 4       !  Lambert conformal conic
            INZONE = 70
            INUNIT = 2       !  input units:  meters
            INSPH  = 8       !  GRS 1980 spheroid
            IPR    = 0       !  print error messages, if any
            JPR    = 1       !  do NOT print projection parameters
            LEMSG  = INIT3() !  unit number for log file
            LPARM  = LEMSG   !  projection parameters file
            YCENT1 = 0.0

        ELSE IF ( GDTYP1 .EQ. UTMGRD3 ) THEN

            DENOMX = SNGL( XCELL1 )
            DENOMY = SNGL( YCELL1 )

            INSYS  = 1       !  Universal Transverse Mercator
            INZONE = NINT( P_ALP1 )
            INUNIT = 2       !  input units:  meters
            INSPH  = 8       !  GRS 1980 spheroid
            IPR    = 0       !  print error messages, if any
            JPR    = 1       !  do NOT print projection parameters
            LEMSG  = INIT3() !  unit number for log file
            LPARM  = LEMSG   !  projection parameters file

        ELSE IF ( GDTYP1 .EQ. LATGRD3 ) THEN

            Y      = YORIG1 + DBLE( NCOLS1 )*YCELL1
            DENOMX = SNGL( DG2M * XCELL1 * COS( Y * PI180 ) )
            DENOMY = SNGL( DG2M * YCELL1 )

            INSYS  = 0       !  geographic (=Lat-Lon)
            INZONE = 0
            INUNIT = 4       !  input units:  meters
            INSPH  = 8       !  GRS 1980 spheroid
            IPR    = 0       !  print error messages, if any
            JPR    = 1       !  do NOT print projection parameters
            LEMSG  = INIT3() !  unit number for log file
            LPARM  = LEMSG   !  projection parameters file

        ELSE IF ( GDTYP1 .EQ. POLGRD3 ) THEN

            DENOMX = SNGL( XCELL1 )
            DENOMY = SNGL( YCELL1 )

            DSCR = P_GAM1
            DEG  = INT( DSCR )                              !  int degrees
            DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
            MNT  = INT( DSCR )                              !  int minutes
            DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
            TPAIN( 5 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

            DSCR = P_BET1
            DEG  = INT( DSCR )                              !  int degrees
            DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
            MNT  = INT( DSCR )                              !  int minutes
            DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
            TPAIN( 6 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

            INSYS  = 6       !  Polar stereographic
            INZONE = 71
            INUNIT = 2       !  input units:  meters
            INSPH  = 8       !  GRS 1980 spheroid
            IPR    = 0       !  print error messages, if any
            JPR    = 1       !  do NOT print projection parameters
            LEMSG  = INIT3() !  unit number for log file
            LPARM  = LEMSG   !  projection parameters file
            YCENT1 = 0.0D0

        ELSE IF ( GDTYP1 .EQ. TRMGRD3 ) THEN

            DENOMX = SNGL( XCELL1 )
            DENOMY = SNGL( YCELL1 )

            TPAIN( 3 ) = P_BET1

            DSCR = P_GAM1
            DEG  = INT( DSCR )                              !  int degrees
            DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
            MNT  = INT( DSCR )                              !  int minutes
            DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
            TPAIN( 5 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

            DSCR = P_ALP1
            DEG  = INT( DSCR )                              !  int degrees
            DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
            MNT  = INT( DSCR )                              !  int minutes
            DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
            TPAIN( 6 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

            INSYS  = 9       !  Transverse Mercator
            INZONE = 72
            INUNIT = 2       !  input units:  meters
            INSPH  = 8       !  GRS 1980 spheroid
            IPR    = 0       !  print error messages, if any
            JPR    = 1       !  do NOT print projection parameters
            LEMSG  = INIT3() !  unit number for log file
            LPARM  = LEMSG   !  projection parameters file

        ELSE IF ( GDTYP1 .EQ. EQMGRD3 ) THEN

            DENOMX = SNGL( XCELL1 )
            DENOMY = SNGL( YCELL1 )

            DSCR = P_GAM1
            DEG  = INT( DSCR )                              !  int degrees
            DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
            MNT  = INT( DSCR )                              !  int minutes
            DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
            TPAIN( 5 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

            DSCR = P_ALP1
            DEG  = INT( DSCR )                              !  int degrees
            DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
            MNT  = INT( DSCR )                              !  int minutes
            DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
            TPAIN( 6 ) = DSCR + 1000.0D0 * ( MNT + 1000*DEG ) !  dddmmmsss.sssD0

            INSYS  = 5       !  Equatorial Mercator
            INZONE = 73
            INUNIT = 2       !  input units:  meters
            INSPH  = 8       !  GRS 1980 spheroid
            IPR    = 0       !  print error messages, if any
            JPR    = 1       !  do NOT print projection parameters
            LEMSG  = INIT3() !  unit number for log file
            LPARM  = LEMSG   !  projection parameters file

        ELSE IF ( GDTYP1 .EQ. ALBGRD3 ) THEN

            DENOMX = SNGL( XCELL1 )
            DENOMY = SNGL( YCELL1 )

            DSCR = P_ALP1
            DEG  = INT( DSCR )                              !  int degrees
            DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
            MNT  = INT( DSCR )                              !  int minutes
            DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
            TPAIN( 3 ) = DSCR + 1000.0D0 * ( MNT + 1000*DEG ) !  dddmmmsss.sssD0

            DSCR = P_BET1
            DEG  = INT( DSCR )                              !  int degrees
            DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
            MNT  = INT( DSCR )                              !  int minutes
            DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
            TPAIN( 4 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

            DSCR = P_GAM1
            DEG  = INT( DSCR )                              !  int degrees
            DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
            MNT  = INT( DSCR )                              !  int minutes
            DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
            TPAIN( 5 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

            DSCR = YCENT1
            DEG  = INT( DSCR )                              !  int degrees
            DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
            MNT  = INT( DSCR )                              !  int minutes
            DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
            TPAIN( 6 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

            INSYS  = 3       !  Albers Equal-Area  conic
            INZONE = 74
            INUNIT = 2       !  input units:  meters
            INSPH  = 8       !  GRS 1980 spheroid
            IPR    = 0       !  print error messages, if any
            JPR    = 1       !  do NOT print projection parameters
            LEMSG  = INIT3() !  unit number for log file
            LPARM  = LEMSG   !  projection parameters file
            YCENT1 = 0.0

        ELSE

            MESG =
     &       'MTXCALC does Lat-Lon, Lambert, UTM, TRM, POL, and EQM'
            CALL M3MSG2( MESG )
            WRITE( MESG, '( A, I4, 2X, A, A )' )
     &          'Grid type', GDTYP1,'not supported'
            CALL M3EXIT( 'MTXCALC', 0, 0, MESG, 2 )

        END IF  ! if non-Lam grid

        IF ( GDTYP2 .EQ. LAMGRD3 ) THEN

            NUMERX = SNGL( XCELL2 )
            NUMERY = SNGL( YCELL2 )

            DSCR = P_ALP2
            DEG  = INT( DSCR )                              !  int degrees
            DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
            MNT  = INT( DSCR )                              !  int minutes
            DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
            TPOUT( 3 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

            DSCR = P_BET2
            DEG  = INT( DSCR )                              !  int degrees
            DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
            MNT  = INT( DSCR )                              !  int minutes
            DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
            TPOUT( 4 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

            DSCR = P_GAM2
            DEG  = INT( DSCR )                              !  int degrees
            DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
            MNT  = INT( DSCR )                              !  int minutes
            DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
            TPOUT( 5 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

            DSCR = YCENT2
            DEG  = INT( DSCR )                              !  int degrees
            DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
            MNT  = INT( DSCR )                              !  int minutes
            DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
            TPOUT( 6 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

            IOSYS  = 4       !  Lambert conformal conic
            IOZONE = 81
            IOUNIT = 2       !  output units:  meters
            IOSPH  = 8       !  GRS 1980 spheroid

        ELSE IF ( GDTYP2 .EQ. UTMGRD3 ) THEN

            NUMERX = SNGL( XCELL2 )
            NUMERY = SNGL( YCELL2 )

            IOSYS  = 1       !  Universal Transverse Mercator
            IOZONE = NINT( P_ALP2 )
            IOUNIT = 2       !  input units:  meters
            IOSPH  = 8       !  GRS 1980 spheroid

        ELSE IF ( GDTYP2 .EQ. LATGRD3 ) THEN

            NUMERX = SNGL( DG2M * XCELL2 * COS( PI180 * YORIG2 ) )
            NUMERY = SNGL( DG2M * YCELL2 )

            IOSYS  = 0       !  geographic coords (=Lat-Lon)
            IOZONE = 0
            IOUNIT = 4       !  output units:  degrees
            IOSPH  = 8       !  GRS 1980 spheroid

        ELSE IF ( GDTYP2 .EQ. POLGRD3 ) THEN

            NUMERX = SNGL( XCELL2 )
            NUMERY = SNGL( YCELL2 )

            DSCR = P_GAM2
            DEG  = INT( DSCR )                              !  int degrees
            DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
            MNT  = INT( DSCR )                              !  int minutes
            DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
            TPOUT( 5 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

            DSCR = P_BET2
            DEG  = INT( DSCR )                              !  int degrees
            DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
            MNT  = INT( DSCR )                              !  int minutes
            DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
            TPOUT( 6 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

            IOSYS  = 6       !  Polar stereographic
            IOZONE = 82
            IOUNIT = 2       !  output units:  meters
            IOSPH  = 8       !  GRS 1980 spheroid

        ELSE IF ( GDTYP2 .EQ. TRMGRD3 ) THEN

            NUMERX = SNGL( XCELL2 )
            NUMERY = SNGL( YCELL2 )

            TPOUT( 3 ) = P_BET2

            DSCR = P_GAM2
            DEG  = INT( DSCR )                              !  int degrees
            DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
            MNT  = INT( DSCR )                              !  int minutes
            DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
            TPOUT( 5 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

            DSCR = P_ALP2
            DEG  = INT( DSCR )                              !  int degrees
            DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
            MNT  = INT( DSCR )                              !  int minutes
            DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
            TPOUT( 6 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

            IOSYS  = 9       !  Transverse Mercator
            IOZONE = 83
            IOUNIT = 2       !  output units:  meters
            IOSPH  = 8       !  GRS 1980 spheroid

        ELSE IF ( GDTYP2 .EQ. EQMGRD3 ) THEN

            NUMERX = SNGL( XCELL2 )
            NUMERY = SNGL( YCELL2 )

            DSCR = P_GAM2
            DEG  = INT( DSCR )                              !  int degrees
            DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
            MNT  = INT( DSCR )                              !  int minutes
            DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
            TPOUT( 5 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

            DSCR = P_ALP2
            DEG  = INT( DSCR )                              !  int degrees
            DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
            MNT  = INT( DSCR )                              !  int minutes
            DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
            TPOUT( 6 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

            IOSYS  = 5       !  Equatorial Mercator
            IOZONE = 84
            IOUNIT = 2       !  output units:  meters
            IOSPH  = 8       !  GRS 1980 spheroid

        ELSE IF ( GDTYP2 .EQ. ALBGRD3 ) THEN

            NUMERX = SNGL( XCELL2 )
            NUMERY = SNGL( YCELL2 )

            DSCR = P_ALP2
            DEG  = INT( DSCR )                              !  int degrees
            DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
            MNT  = INT( DSCR )                              !  int minutes
            DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
            TPOUT( 3 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

            DSCR = P_BET2
            DEG  = INT( DSCR )                              !  int degrees
            DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
            MNT  = INT( DSCR )                              !  int minutes
            DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
            TPOUT( 4 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

            DSCR = P_GAM2
            DEG  = INT( DSCR )                              !  int degrees
            DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
            MNT  = INT( DSCR )                              !  int minutes
            DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
            TPOUT( 5 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

            DSCR = YCENT2
            DEG  = INT( DSCR )                              !  int degrees
            DSCR = 60.0D0 * ( DSCR - DBLE( DEG ) )          !  minutes
            MNT  = INT( DSCR )                              !  int minutes
            DSCR = 60.0D0 * ( DSCR - DBLE( MNT ) )          !  seconds
            TPOUT( 6 ) = DSCR + 1000.0D0*( MNT + 1000*DEG ) !  dddmmmsss.sssD0

            IOSYS  = 3       !  Albers Equal-Area  conic
            IOZONE = 85
            IOUNIT = 2       !  output units:  meters
            IOSPH  = 8       !  GRS 1980 spheroid

        ELSE

            MESG = 'Suppported grid types are:'
            CALL M3MSG2( MESG )
            MESG =
     &'LATLON (1), LAM (2), UTM (5), POL (6), EQM (7), TRM (8), ALB (9)'
            CALL M3MSG2( MESG )
            WRITE( MESG, '( A, I4, 2X, A )' )
     &      'Requested grid type', GDTYP2, 'not supported'
            CALL M3EXIT( 'MTXCALC', 0, 0, MESG, 2 )

        END IF                  ! if dscgrid() failed, or if non-Lambert grid


C.......   Allocate scratch buffers:

        NFRACS = ( 2 + 2*NINT( ABS( NUMERX / DENOMX ) ) )*
     &           ( 2 + 2*NINT( ABS( NUMERY / DENOMY ) ) )*NCOLS2*NROWS2

        CALL M3MSG2( MESG )

        ALLOCATE( ISCR( NCOLS2, NROWS2 ),
     &            ICEL( NFRACS ),
     &            OCEL( NFRACS ),
     &            FRAC( NFRACS ), STAT = ISTAT )

        IF ( ISTAT .NE. 0 ) THEN
            MESG   = 'Work-buffer allocation failure'
            CALL M3EXIT( 'MTXCALC', 0, 0, MESG, 2 )
        END IF                  ! if allocate failed


C.......   Compute fractions:

        DO  ROW = 1, NROWS2
        DO  COL = 1, NCOLS2
            ISCR( COL,ROW ) = 0
        END DO
        END DO

        DDX2 = 1.0 / XCELL2
        DDY2 = 1.0 / YCELL2
        DDXY = 1.0 / FLOAT( NX * NY )
        N    = 0
        K    = 0

        DO  R = 1, NROWS1       !  traverse input grid
        DO  C = 1, NCOLS1

            K = K + 1
            XX0 = XORIG1 - 0.5D0*DX + DBLE( C-1 )*XCELL1
            YY0 = YORIG1 - 0.5D0*DY + DBLE( R-1 )*YCELL1

            COLMAX = 0
            ROWMAX = 0
            COLMIN = NCOLS2 + 1
            ROWMIN = NROWS2 + 1
            DO  J = 1, NY
            DO  I = 1, NX

                CRDIN( 1 ) = XX0 + DBLE( I )*DX
                CRDIN( 2 ) = YY0 + DBLE( J )*DY
                CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,
     &                  IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,
     &                  TPOUT, IOUNIT, LN27, LN83, FN27, FN83, LENGTH,
     &                  IFLG )

                IF ( IFLG .NE. 0 ) THEN
                    MESG = 'Failure in GTPZ0  for ' // IGRID
                    CALL M3EXIT( 'MTXCALC', 0, 0, MESG, 2 )
                END IF

                X = CRDIO( 1 )  -  XORIG2  -  XADJ
                Y = CRDIO( 2 )  -  YORIG2
                IF ( X .GE. 0.0D0  .AND. Y .GE. 0.0 ) THEN
                    COL = 1 + INT( DDX2 * X )
                    ROW = 1 + INT( DDY2 * Y )
                    IF ( COL .LE. NCOLS2 .AND. ROW .LE. NROWS2 ) THEN
                        ISCR( COL,ROW ) = ISCR( COL,ROW ) + 1
                        IF ( COL .GT. COLMAX ) COLMAX = COL
                        IF ( COL .LT. COLMIN ) COLMIN = COL
                        IF ( ROW .GT. ROWMAX ) ROWMAX = ROW
                        IF ( ROW .LT. ROWMIN ) ROWMIN = ROW
                    END IF
                END IF

            END DO      !  end loop on sub-sampled cols in input grid
            END DO      !  end loop on sub-sampled rows in input grid

            DO ROW = ROWMIN, ROWMAX
            DO COL = COLMIN, COLMAX
                IF ( ISCR( COL,ROW ) .GT. 0 ) THEN
                    N = N + 1
                    IF ( N .LE. NFRACS ) THEN
                        FRAC( N ) = DDXY * FLOAT( ISCR( COL,ROW ) )
                        ICEL( N ) =  C  +  ( R  - 1 ) * NCOLS1  !  input cell #
                        OCEL( N ) = COL + ( ROW - 1 ) * NCOLS2  ! output cell #
                        ISCR( COL,ROW ) = 0   ! reset to zero for next pass
                    END IF
                END IF
            END DO
            END DO

        END DO
        END DO          !  end traversal of input grid

        IF ( N .GT. NFRACS ) THEN
            WRITE( MESG, '( A, I12, 2X, A, I12 )' )
     &            'Size-allocation overflow:  allocated', NFRACS,
     &            'actual', N
            CALL M3EXIT( 'MTXCALC', 0, 0, MESG, 2 )
        ELSE IF ( N .EQ. 0 ) THEN
            MESG = 'NO INTERSECTION FOUND:  Number of coeffs = 0'
            CALL M3EXIT( 'MTXCALC', 0, 0, MESG, 2 )
        END IF
        WRITE( MESG, '( A, I12 )' ) 'Number of coeffs', N
        CALL M3MSG2( MESG )


C.......   Allocate and organize the output sparse matrix:
C.......   (writing ASCII output file, if requested)

        K = NCOLS2 * NROWS2
        ALLOCATE( MATX( K + 2*N ), STAT = ISTAT )

        IF ( ISTAT .NE. 0 ) THEN
            MESG   = 'Output-buffer allocation failure'
            CALL M3EXIT( 'MTXCALC', 0, 0, MESG, 2 )
        END IF                  ! if allocate failed

        CALL BLDMATRIX( K, N, ICEL, OCEL, FRAC,
     &                  MATDEV, NCOLS1, NCOLS2,
     &                  MATX, MATX( K+1 ), MATX( N+K+1 ) )

C.......   Open output file and write out the output sparse matrix:

        GDNAM3D = OGRID
        NROWS3D = K
        NCOLS3D = N
        FTYPE3D = SMATRX3
        NVARS3D = 1
        NLAYS3D = 1
        VGTYP3D = IMISS3
        VGTOP3D = BADVAL3
        VGLVS3D( 1 ) = BADVAL3
        VGLVS3D( 2 ) = BADVAL3

        SDATE3D = 0
        STIME3D = 0
        TSTEP3D = 0

        VNAME3D( 1 ) = 'COEF'
        VTYPE3D( 1 ) = M3REAL
        UNITS3D( 1 ) = 'n/a'
        VDESC3D( 1 ) = 'Sparse matrix coefficient'
        FDESC3D( 1 ) = 'Sparse transform matrix'
        FDESC3D( 2 ) = '#INGRID  ' // IGRID
        FDESC3D( 3 ) = '#OUTGRID ' // OGRID
        DO  N = 4, MXDESC3
            FDESC3D( N ) = ' '
        END DO

        IF ( .NOT. OPEN3( 'MATRIX', FSUNKN3, 'MTXCALC' ) ) THEN
            EFLAG = .TRUE.
            MESG = 'Could not open output SPARSE MATRIX file'
            CALL M3MESG( MESG )

        ELSE IF ( .NOT. SETMTXATT( 'MATRIX', 1, IGRID, GDTYP1,
     &                      P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1,
     &                      XORIG1, YORIG1, XCELL1, YCELL1,
     &                      NCOLS1, NROWS1 ) ) THEN
            EFLAG = .TRUE.
            MESG  = 'Could not set input-grid description for "MATRIX"'
            CALL M3MESG( MESG )

        ELSE IF ( .NOT. SETMTXATT( 'MATRIX', 2, OGRID, GDTYP2,
     &                      P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2,
     &                      XORIG2, YORIG2, XCELL2, YCELL2,
     &                      NCOLS2, NROWS2 ) ) THEN
            EFLAG = .TRUE.
            MESG  = 'Could not set output-grid description for "MATRIX"'
            CALL M3MESG( MESG )

        ELSE IF ( .NOT. WRITE3( 'MATRIX', ALLVAR3, 0, 0, MATX ) ) THEN

            EFLAG = .TRUE.
            MESG = 'Could not write output SPARSE MATRIX'
            CALL M3MESG( MESG )

        END IF


C...............  Shut down program:

        IF ( EFLAG ) THEN
            MESG  = 'Failure in program MTXCALC'
            ISTAT = 2
        ELSE
            MESG  = 'Success in program MTXCALC'
            ISTAT = 0
        END IF
        CALL M3EXIT( 'MTXCALC', 0, 0, MESG, ISTAT )

        END PROGRAM MTXCALC

C===========================================================================

	SUBROUTINE  BLDMATRIX( M, N, ICEL, OCEL, FRAC,
     &                         MATDEV, NCOLS1, NCOLS2,
     &                         ICNT, INDX, COEF )

        IMPLICIT NONE

        INTEGER   M               !  number of output rows
        INTEGER   N               !  number of input coefficients
        INTEGER   ICEL( N )       !  list of in-cells
        INTEGER   OCEL( M )       !  list of out-cells
        REAL      FRAC( N )       !  list of coefficients
        INTEGER   MATDEV          !  optional ASCII output unit
        INTEGER   NCOLS1          !  input  grid-column
        INTEGER   NCOLS2          !  output grid-column
        INTEGER   ICNT( M )       !  matrix: output row-count
        INTEGER   INDX( N )       !  matrix: output row-index
        REAL      COEF( N )       !  matrix: coefficients

        INTEGER   I, J, K, L, JJ

        !!..................  body of BLDMATRIX  ...........................

        K = 0

        IF ( MATDEV .LT. 0 ) THEN	!  then do NOT write ASCII coeffs

            DO  I = 1, M

                L = 0
                DO  J = 1, N
                    IF ( OCEL( J ) .EQ. I ) THEN
                        K = K + 1
                        L = L + 1
                        INDX( K ) = ICEL( J )
                        COEF( K ) = FRAC( J )
                    END IF
                END DO          !  end loop on coeffs J
                ICNT( I ) = L

            END DO              !  end loop on output rows I

        ELSE		!  matdev > 0:  write ASCII coeffs-file to unit MATDEV

            WRITE( MATDEV, '( A4, 5A12 )' )
     &              '#    ',
     &              'OUTROW',
     &              'OUTCOL',
     &              'INROW',
     &              'INCOL',
     &              'FRAC'

            DO  I = 1, M

                L = 0
                DO  J = 1, N
                    IF ( OCEL( J ) .EQ. I ) THEN
                        K = K + 1
                        L = L + 1
                        JJ = ICEL( J )
                        INDX( K ) = JJ
                        COEF( K ) = FRAC( J )
                        WRITE( MATDEV, '( 4I12, 2X, 1PE13.6 )' )
     &                            1 + (I -1)/NCOLS2,        ! output row #
     &                            1 + MOD(  I-1, NCOLS2 ),  ! output col #
     &                            1 + (JJ-1)/NCOLS1,        !  input row #
     &                            1 + MOD( JJ-1, NCOLS1 ),  !  input col #
     &                            FRAC( J )
                    END IF
                END DO          !  end loop on coeffs J
                ICNT( I ) = L

            END DO              !  end loop on output rows I

        END IF		!  if matdev < 0, or not

        RETURN

        END SUBROUTINE BLDMATRIX

