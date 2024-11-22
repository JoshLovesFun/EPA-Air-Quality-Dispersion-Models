
        PROGRAM M3FAKE

C***********************************************************************
C Version "@(#)$Header$ $Id: m3fake.f 250 2008-06-13 19:26:21Z coats@bdsl $"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr, and
C (C) 2002-2008 Baron Advanced Meteorological Systems, LLC.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line  160
C
C  FUNCTION:
C	Generate new EDSS/Models-3 I/O API file with the user-sepcified
C	structure and "fake" data.
C
C  PRECONDITIONS REQUIRED:
C	none
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C	FAKESTEP
C
C  REVISION  HISTORY:
C	prototype 8/1995 by CJC
C       Modified  8/1997 by CJC:  additional file types supported
C       Modified  1/2000 by CJC:  additional coordinate types supported
C       Version  11/2001 by CJC for I/O API Version 2.1
C       Version   1/2002 by CJC for I/O API Version 2.2:  bug-fix in
C           "timestep" formula; enhanced opening-screen text.
C       Version   6/2008 by CJC:  Albers map-projection support
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'  !  I/O parameter definitions
        INCLUDE 'FDESC3.EXT'  !  file header data structures
        INCLUDE 'IODECL3.EXT' !  I/O definitions and declarations


C...........   EXTERNAL FUNCTIONS and their descriptions:

        LOGICAL          GETYN, DSCGRID
        CHARACTER*16     PROMPTMFILE
        INTEGER          GETMENU, GETNUM, IARGC, INDEX1, PROMPTFFILE,
     &                   SECSDIFF, SEC2TIME, TIME2SEC, TRIMLEN
        REAL             GETREAL
        DOUBLE PRECISION GETDBLE

        EXTERNAL  GETDBLE, GETMENU, GETNUM, GETREAL, GETYN,
     &            INDEX1, DSCGRID, PROMPTFFILE, PROMPTMFILE,
     &            SECSDIFF, SEC2TIME, TIME2SEC, TRIMLEN


C...........   PARAMETERS and their descriptions:

        DOUBLE PRECISION SIXTH
        PARAMETER      ( SIXTH = 1.0D0 / 6.0D0 )

        CHARACTER*72 PROGVER
        DATA         PROGVER /
     &'$Id:: m3fake.f 250 2008-06-13 19:26:21Z coats@bdsl            $'
     &  /


C...........   LOCAL VARIABLES and their descriptions:

        INTEGER		IDUM	!  value from INIT3()
        CHARACTER*60    AMENU( 6 )
        DATA            AMENU
     &  /
     &  'Column   number (1...NCOLS)',
     &  'Row      number (1...NROWS)',
     &  'Layer    number (1...NLAYS)',
     &  'Timestep number (1...NSTEPS)',
     &  'User-specified fill value',
     &  'Input from user-specified file'
     &  /
        INTEGER		ATYPES( 6 )
        DATA            ATYPES      / 1, 2, 3, 4, 5, 6 /
        INTEGER		OPTYPES( MXVARS3 )
        REAL		FILLVAL( MXVARS3 )

        CHARACTER*60    FMENU( 7 )
        DATA            FMENU
     &  /
     &  'File type CUSTOM',
     &  'File type GRIDDED',
     &  'File type BOUNDARY',
     &  'File type IDDATA',
     &  'File type PROFILE',
     &  'File type GRID-NEST',
     &  'File type SPARSE MATRIX'
     &  /
        INTEGER          FTYPES( 7 )
        DATA             FTYPES
     &  /CUSTOM3, GRDDED3, BNDARY3, IDDATA3, PROFIL3, GRNEST3, SMATRX3/

        CHARACTER*60    VMENU( 3 )
        DATA            VMENU
     &  /
     &  'Primitive data type REAL',
     &  'Primitive data type DOUBLE PRECISION',
     &  'Primitive data type INTEGER'
     &  /
        INTEGER          VTYPES( 3 )
        DATA             VTYPES  / M3REAL, M3DBLE, M3INT /

        CHARACTER*60    GMENU( 10 )
        DATA            GMENU
     &  /
     &  'Map projection type LAT-LON',
     &  'Map projection type LAMBERT CONFORMAL CONIC',
     &  'Map projection type MERCATOR( general)',
     &  'Map projection type (tangent) STEREOGRAPHIC',
     &  'Map projection type UTM',
     &  'Map projection type (secant) POLAR STEREOGRAPHIC',
     &  'Map projection type EQUATORIAL MERCATOR',
     &  'Map projection type TRANSVERSE MERCATOR',
     &  'Map projection type ALBERS EQUAL-AREA CONIC',
     &  'Map projection type MISSING or NOT APPLICABLE'
     &  /
        INTEGER         GTYPES( 10 )
        DATA            GTYPES
     &  / LATGRD3, LAMGRD3, MERGRD3, STEGRD3, UTMGRD3, POLGRD3,
     &    EQMGRD3, TRMGRD3, ALBGRD3, IMISS3 /

        CHARACTER*60    VGMENU( 7 )
        DATA            VGMENU
     &  /
     &  'Vertical coordinate type PRESSURE (mb)',
     &  'Vertical coordinate type Z (height a.g.l., m)',
     &  'Vertical coordinate type H (height a.s.l., m)',
     &  'Vertical coordinate type HYDROSTATIC SIGMA-P',
     &  'Vertical coordinate type NONHYDROSTATIC SIGMA-P',
     &  'Vertical coordinate type SIGMA-Z',
     &  'Vertical coordinate type MISSING or NOT APPLICABLE'
     &  /
        INTEGER          VGTYPES( 7 )
        DATA             VGTYPES
     &  /VGPRES3, VGZVAL3, VGHVAL3, VGSGPH3, VGSGPN3, VGSIGZ3, IMISS3/


        REAL*8         XSCR, YSCR
        REAL           VSCR
        INTEGER        V, L, T
        INTEGER        NSTEPS, JDATE, JTIME, TSTEP
        INTEGER        IDEV
        INTEGER        ARGCNT
        CHARACTER*16   FNAME, CNAME
        CHARACTER*256  MESG
        CHARACTER*256  ENVBUF  !  value from command line arguments


C***********************************************************************
C   begin body of program M3FAKE

        IDUM = INIT3()
        
        WRITE( *,92000 )
     &  ' ', ' ', ' ',
     &  'Program "M3FAKE" to create "dummied-up" files according to',
     &  'the user-specified file description.  Before running the',
     &  'program, you need to have assigned an appropriate path name',
     &  'for the output file (using "setenv <lname> <path-name>").  ',
     &  'You will be asked for the logical name and the specifications',
     &  'for the input file.',
     &  ' ',
     &  'Usage:  "m3fake [<lname>]" and follow the prompts.',
     &  ' ',
     &'See URL  http://www.baronams.com/products/ioapi/AA.html#tools',
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
     &'    Baron Advanced Meteorological Systems, LLC.',
     &'    920 Main Campus Drive, Suite 101',
     &'    Raleigh, NC 27606',
     &' ',
     &'Program version: ',
     &PROGVER,
     &'Program release tag: $Name$',
     &' '

        IF ( .NOT.GETYN( 'Continue with program?', .TRUE. ) )
     &          CALL M3EXIT( 'M3FAKE', 0, 0, 'Program aborted', 2 )

        WRITE( *,92000 )
     &  ' ',
     &  'First, let us define the file type and get the dimensions.',
     &  ' '

        FTYPE3D = FTYPES( GETMENU( 3, 2,  'Enter file type', FMENU ) )


C...............   Get file, grid description:

        WRITE( *,95000 ) 'Enter the grid/coord system name >> '
        READ ( *,93000 ) GDNAM3D

        IF ( DSCGRID( GDNAM3D, CNAME, GDTYP3D,      !  "standard" grid name
     &                P_ALP3D, P_BET3D, P_GAM3D,
     &                XCENT3D, YCENT3D, XORIG3D, YORIG3D,
     &                XCELL3D, YCELL3D,
     &                NCOLS3D, NROWS3D, NTHIK3D ) ) THEN

            IF( FTYPE3D .EQ. CUSTOM3 ) THEN

                NCOLS3D = GETNUM( 1, 1999999999, 100, 'Enter BLOBSIZE' )

            ELSE IF( FTYPE3D .EQ. IDDATA3 ) THEN

                NCOLS3D = IMISS3
                NROWS3D = GETNUM( 1, 1999999999, 100,
     &                            'Enter NUMBER OF SITES' )

            ELSE IF( FTYPE3D .EQ. PROFIL3 ) THEN

                NCOLS3D = GETNUM( 1, 1999999999, 100,
     &                            'Enter MAX LEVEL-COUNT' )
                NROWS3D = GETNUM( 1, 1999999999, 100,
     &                            'Enter NUMBER OF SITES' )

            ELSE IF( FTYPE3D .EQ. GRNEST3 ) THEN

                NCOLS3D = GETNUM( 1, 1999999999, 10,
     &                            'Enter MAX NEST-COUNT' )
                NROWS3D = GETNUM( 1, 1999999999, 1000,
     &                            'Enter NUMBER OF CELLS PER NEST' )

            ELSE IF( FTYPE3D .EQ. SMATRX3 ) THEN

                NCOLS3D = GETNUM( 1, 1999999999, 10000,
     &                            'Enter MAX NUMBER OF COEFFS' )
                NROWS3D = GETNUM( 1, 1999999999, 1000,
     &                            'Enter number of matrix rows' )

            END IF		!  through ftype3d's (where dscgrid() worked)

        ELSE		!  else dscgrid() failed

            IF( FTYPE3D .EQ. CUSTOM3 ) THEN

                NCOLS3D = GETNUM( 1, 1999999999, 100, 'Enter BLOBSIZE' )
                NROWS3D = IMISS3
                NTHIK3D = IMISS3

            ELSE IF( FTYPE3D .EQ. GRDDED3 ) THEN

                NCOLS3D = GETNUM( 1, 1999999999, 100,
     &                            'Enter number of GRID COLUMNS' )
                NROWS3D = GETNUM( 1, 1999999999, 100,
     &                            'Enter number of GRID ROWS' )
                NTHIK3D = IMISS3

            ELSE IF( FTYPE3D .EQ. BNDARY3 ) THEN

                NCOLS3D = GETNUM( 1, 1999999999, 100,
     &                            'Enter number of GRID COLUMNS' )
                NROWS3D = GETNUM( 1, 1999999999, 100,
     &                            'Enter number of GRID ROWS' )
                NTHIK3D = MIN( NCOLS3D / 2, NROWS3D / 2 ) - 1
                NTHIK3D = GETNUM( -NTHIK3D, NTHIK3D, 1,
     &                            'Enter PERIMETER THICKNESS (cells)' )

            ELSE IF( FTYPE3D .EQ. IDDATA3 ) THEN

                NCOLS3D = IMISS3
                NROWS3D = GETNUM( 1, 1999999999, 100,
     &                            'Enter NUMBER OF SITES' )
                NTHIK3D = IMISS3

            ELSE IF( FTYPE3D .EQ. PROFIL3 ) THEN

                NCOLS3D = GETNUM( 1, 1999999999, 100,
     &                            'Enter MAX LEVEL-COUNT' )
                NROWS3D = GETNUM( 1, 1999999999, 100,
     &                            'Enter NUMBER OF SITES' )
                NTHIK3D = IMISS3

            ELSE IF( FTYPE3D .EQ. GRNEST3 ) THEN

                NCOLS3D = GETNUM( 1, 1999999999, 10,
     &                            'Enter MAX NEST-COUNT' )
                NROWS3D = GETNUM( 1, 1999999999, 1000,
     &                            'Enter NUMBER OF CELLS PER NEST' )
                NTHIK3D = IMISS3

            ELSE IF( FTYPE3D .EQ. SMATRX3 ) THEN

                NCOLS3D = GETNUM( 1, 1999999999, 10000,
     &                            'Enter MAX NUMBER OF COEFFS' )
                NROWS3D = GETNUM( 1, 1999999999, 1000,
     &                            'Enter number of matrix rows' )
                NTHIK3D = IMISS3

            END IF		!  through all the ftype3d's

            WRITE( *,92000 )
     &      ' ',
     &      'Next, let us construct the horizontal coordinate system',
     &      '("map projection") for the file', ' '

            GDTYP3D = GTYPES( GETMENU( 10, 2,
     &                                 'Enter coordinate system type',
     &                                 GMENU ) )

            IF ( GDTYP3D .EQ. LATGRD3 ) THEN

                XCELL3D = GETDBLE( 0.0D0, 1.0D36, 0.25D0,
     &                             'Enter cell size DX (deg.E)' )
                YCELL3D = GETDBLE( 0.0D0, 1.0D36, SIXTH,
     &                             'Enter cell size DY (deg.N)' )
                XSCR    = -90.0D0
                YSCR    =  27.0D0

            ELSE IF ( GDTYP3D .EQ. LAMGRD3 ) THEN

                P_ALP3D = GETDBLE( -90.0D0, 90.0D0, 30.0D0,
     &                             'Enter first  latitude for cone' )
                P_BET3D = GETDBLE( P_ALP3D, 90.0D0, 60.0D0,
     &                             'Enter second latitude for cone' )
                P_GAM3D = GETDBLE( -180.0D0, 180.0D0, -90.0D0,
     &                             'Enter central longitude for cone' )
                XCENT3D = GETDBLE( -180.0D0, 180.0D0, P_GAM3D,
     &                             'Enter longitude for X-Y origin' )
                YCENT3D = GETDBLE( P_ALP3D, P_BET3D,
     &                             0.5D0 * ( P_ALP3D + P_BET3D ),
     &                             'Enter latitude for X-Y origin' )
                XCELL3D = GETDBLE( 0.0D0, 1.0D36, 540000.0D0,
     &                             'Enter cell size DX (m)' )
                YCELL3D = GETDBLE( 0.0D0, 1.0D36, 540000.0D0,
     &                             'Enter cell size DY (m)' )
                XSCR    = XCELL3D * DBLE( -NCOLS3D / 2 )
                YSCR    = YCELL3D * DBLE( -NROWS3D / 2 )

            ELSE IF ( GDTYP3D .EQ. MERGRD3 ) THEN

                P_ALP3D = GETDBLE( -90.0D0, 90.0D0, 30.0D0,
     &                    'Enter latitude  for cylinder origin' )
                P_BET3D = GETDBLE( -180.0D0, 180.0D0, -90.0D0,
     &                    'Enter longitude for cylinder origin' )
                P_GAM3D = GETDBLE( -180.0D0, 180.0D0, -90.0D0,
     &                    'Enter angle between axis and N pole' )
                XCENT3D = GETDBLE( 0.0D0, 90.0D0, 30.0D0,
     &                    'Enter latitude for X-Y origin' )
                YCENT3D = GETDBLE( -180.0D0, 180.0D0, -90.0D0,
     &                    'Enter longitude for X-Y origin' )
                XCELL3D = GETDBLE( 0.0D0, 1.0D36, 50000.0D0,
     &                    'Enter cell size DX (m)' )
                YCELL3D = GETDBLE( 0.0D0, 1.0D36, 50000.0D0,
     &                    'Enter cell size DY (m)' )
                XSCR    = XCELL3D * DBLE( -NCOLS3D / 2 )
                YSCR    = YCELL3D * DBLE( -NROWS3D / 2 )

          ELSE IF ( GDTYP3D .EQ. STEGRD3 ) THEN

                P_ALP3D = GETDBLE( -90.0D0, 90.0D0, 60.0D0,
     &                    'Enter latitude for point of tangency' )
                P_BET3D = GETDBLE( -180.0D0, 180.0D0, -90.0D0,
     &                    'Enter longitude for point of tangency')
                P_GAM3D = GETDBLE( -180.0D0, 180.0D0, 0.0D0,
     &                    'Enter angle between Y and true N at origin')
                XCENT3D = GETDBLE( 0.0D0, 90.0D0, P_ALP3D,
     &                    'Enter latitude for X-Y origin' )
                YCENT3D = GETDBLE( -180.0D0, 180.0D0, P_BET3D,
     &                    'Enter longitude for X-Y origin' )
                XCELL3D = GETDBLE( 0.0D0, 1.0D36, 50000.0D0,
     &                    'Enter cell size DX (m)' )
                YCELL3D = GETDBLE( 0.0D0, 1.0D36, 50000.0D0,
     &                    'Enter cell size DY (m)' )
                XSCR    = XCELL3D * DBLE( -NCOLS3D / 2 )
                YSCR    = YCELL3D * DBLE( -NROWS3D / 2 )

          ELSE IF ( GDTYP3D .EQ. UTMGRD3 ) THEN

                P_ALP3D = DBLE( GETNUM( 1, 36, 17, 'Enter UTM zone' ) )
                P_BET3D = DBLE( BADVAL3 )
                P_GAM3D = DBLE( BADVAL3 )
                XCENT3D = GETDBLE( DBLE( BADVAL3 ), -DBLE( BADVAL3 ),
     &                    0.0D0,
     &                    'Enter UTM X offset for origin (m)' )
                YCENT3D = GETDBLE( DBLE( BADVAL3 ), -DBLE( BADVAL3 ),
     &                    0.0D0,
     &                    'Enter UTM Y offset for origin (m)' )
                XCELL3D = GETDBLE( 0.0D0, 1.0D36, 5000.0D0,
     &                    'Enter cell size DX (m)' )
                YCELL3D = GETDBLE( 0.0D0, 1.0D36, 5000.0D0,
     &                    'Enter cell size DY (m)' )
                XSCR    =  350.0D3
                YSCR    = 3785.0D3

          ELSE IF ( GDTYP3D .EQ. POLGRD3 ) THEN

                WRITE( *,92000 )
     &          'NORTH POLAR projections project from the SOUTH POLE',
     &          'and contain the NORTH POLE in the projected plane'
                IF ( GETYN( 'Is this projection NORTH POLAR',
     &                      .TRUE. ) ) THEN
                    P_ALP3D = 1.0D0
                    YSCR    = 90.0D0
                ELSE
                    P_ALP3D = -1.0D0
                    YSCR    = -90.0D0
                END IF
                P_BET3D = GETDBLE( -90.0D0, 90.0D0, 60.0D0,
     &                    'Enter SECANT LATITUDE')
                P_GAM3D = GETDBLE( -180.0D0, 180.0D0, 0.0D0,
     &                    'Enter CENTRAL LONGITUDE')
                XCENT3D = GETDBLE( -180.0D0, 180.0D0, P_GAM3D,
     &                    'Enter longitude for X-Y origin' )
                YCENT3D = GETDBLE( -90.0D0, 90.0D0, YSCR,
     &                    'Enter latitude for X-Y origin' )
                XCELL3D = GETDBLE( 0.0D0, 1.0D36, 50000.0D0,
     &                    'Enter cell size DX (m)' )
                YCELL3D = GETDBLE( 0.0D0, 1.0D36, 50000.0D0,
     &                    'Enter cell size DY (m)' )
                XSCR    = XCELL3D * DBLE( -NCOLS3D / 2 )
                YSCR    = YCELL3D * DBLE( -NROWS3D / 2 )

          ELSE IF ( GDTYP3D .EQ. EQMGRD3 ) THEN

                P_ALP3D = GETDBLE( -180.0D0, 180.0D0, 0.0D0,
     &                             'Enter latitude of true scale' )
                P_BET3D = 0.0D0
                P_GAM3D = GETDBLE( -90.0D0, 90.0D0, 0.0D0,
     &              'Enter longitude of the central meridian' )
                XCENT3D = GETDBLE( -180.0D0, 180.0D0, P_GAM3D,
     &                             'Enter longitude for X-Y origin' )
                YCENT3D = GETDBLE( -90.0D0, 90.0D0, P_ALP3D,
     &                             'Enter latitude for X-Y origin' )
                XCELL3D = GETDBLE( 0.0D0, 1.0D36, 540000.0D0,
     &                             'Enter cell size DX (m)' )
                YCELL3D = GETDBLE( 0.0D0, 1.0D36, 540000.0D0,
     &                             'Enter cell size DY (m)' )
                XSCR    = XCELL3D * DBLE( -NCOLS3D / 2 )
                YSCR    = YCELL3D * DBLE( -NROWS3D / 2 )

          ELSE IF ( GDTYP3D .EQ. TRMGRD3 ) THEN

                P_ALP3D = GETDBLE( -180.0D0, 180.0D0, 0.0D0,
     &                             'Enter latitude of the origin' )
                P_BET3D = GETDBLE( 0.0D0, 9.99D36, 1.0D0,
     &               'Enter scale factor at the central meridian' )
                P_GAM3D = GETDBLE( -90.0D0, 90.0D0, 0.0D0,
     &              'Enter longitude of the central meridian' )
                XCENT3D = GETDBLE( -180.0D0, 180.0D0, P_GAM3D,
     &                             'Enter longitude for X-Y origin' )
                YCENT3D = GETDBLE( P_ALP3D, P_BET3D,
     &                             0.5D0 * ( P_ALP3D + P_BET3D ),
     &                             'Enter latitude for X-Y origin' )
                XCELL3D = GETDBLE( 0.0D0, 1.0D36, 540000.0D0,
     &                             'Enter cell size DX (m)' )
                YCELL3D = GETDBLE( 0.0D0, 1.0D36, 540000.0D0,
     &                             'Enter cell size DY (m)' )
                XSCR    = XCELL3D * DBLE( -NCOLS3D / 2 )
                YSCR    = YCELL3D * DBLE( -NROWS3D / 2 )

            ELSE IF ( GDTYP3D .EQ. ALBGRD3 ) THEN

                P_ALP3D = GETDBLE( -90.0D0, 90.0D0, 30.0D0,
     &                             'Enter first  latitude for cone' )
                P_BET3D = GETDBLE( P_ALP3D, 90.0D0, 60.0D0,
     &                             'Enter second latitude for cone' )
                P_GAM3D = GETDBLE( -180.0D0, 180.0D0, -90.0D0,
     &                             'Enter central longitude for cone' )
                XCENT3D = GETDBLE( -180.0D0, 180.0D0, P_GAM3D,
     &                             'Enter longitude for X-Y origin' )
                YCENT3D = GETDBLE( P_ALP3D, P_BET3D,
     &                             0.5D0 * ( P_ALP3D + P_BET3D ),
     &                             'Enter latitude for X-Y origin' )
                XCELL3D = GETDBLE( 0.0D0, 1.0D36, 540000.0D0,
     &                             'Enter cell size DX (m)' )
                YCELL3D = GETDBLE( 0.0D0, 1.0D36, 540000.0D0,
     &                             'Enter cell size DY (m)' )
                XSCR    = XCELL3D * DBLE( -NCOLS3D / 2 )
                YSCR    = YCELL3D * DBLE( -NROWS3D / 2 )

          ELSE

                WRITE( *,92000 )
     &          ' ',
     &          'Grid/coordinate type:  "UNKNOWN" or "MISSING"',
     &          ' '
                P_ALP3D = GETDBLE( DBLE( BADVAL3 ), -DBLE( BADVAL3 ),
     &                             0.0D0,
     &                             'Enter projection parameter ALPHA' )
                P_BET3D = GETDBLE( DBLE( BADVAL3 ), -DBLE( BADVAL3 ),
     &                             0.0D0,
     &                             'Enter projection parameter BETA ' )
                P_GAM3D = GETDBLE( DBLE( BADVAL3 ), -DBLE( BADVAL3 ),
     &                             0.0D0,
     &                             'Enter projection parameter GAMMA' )
                XCENT3D = GETDBLE( DBLE( BADVAL3 ), -DBLE( BADVAL3 ),
     &                             0.0D0,
     &                             'Enter X-Y origin' )
                YCENT3D = GETDBLE( DBLE( BADVAL3 ), -DBLE( BADVAL3 ),
     &                             0.0D0,
     &                             'Enter X-Y origin' )
                XCELL3D = GETDBLE( DBLE( BADVAL3 ), -DBLE( BADVAL3 ),
     &                             0.0D0,
     &                             'Enter cell size DX (m)' )
                YCELL3D = GETDBLE( 0.0D0, 1.0D36, 50000.0D0,
     &                             'Enter cell size DY (m)' )
                XSCR    = 0.0D0
                YSCR    = 0.0D0

            END IF		!  through grdtyp3d's (where dscgrid failed)

            WRITE( *,92000 )
     &      'Grid corners are given in terms of map-projection units',
     &      '(meters, for everything except LL) from the Cartesian',
     &      'origin, and specify the lower-left corner of cell (1,1).'
            XORIG3D = GETDBLE( -1.0D36, 1.0D36, XSCR,
     &                         'Enter starting grid corner X' )
            YORIG3D = GETDBLE( -1.0D36, 1.0D36, YSCR,
     &                         'Enter starting grid corner Y' )

        END IF            !  if dscgrid() worked, or not


C...............   Get vertical structure description:

        WRITE( *,92000 )
     &  ' ',
     &  'Next, let us construct the vertical coordinate system ',
     &  'for the file',
     &  ' '

        NLAYS3D = GETNUM( 1, MXLAYS3, 30, 'Enter number of layers' )

        IF ( NLAYS3D .GT. 1 ) THEN

            VGTYP3D = VGTYPES( GETMENU( 7, 4,
     &                         'Enter vertical coordinate type',
     &                         VGMENU ) )

            IF ( ( VGTYP3D .EQ. VGSGPH3 ) .OR.
     &           ( VGTYP3D .EQ. VGSGPN3 ) .OR.
     &           ( VGTYP3D .EQ. VGSIGZ3 )    ) THEN
                VGTOP3D = GETREAL( 0.0, 9.99E36, 100.0,
     &                    'Enter MODEL TOP for sigma coordinate' )
            ELSE
                VGTOP3D = BADVAL3
            END IF
            DO  22  L = 1, NLAYS3D+1
                WRITE( MESG,94000 ) 'Enter value for layer', L-1
                VSCR = FLOAT( L-1 ) / FLOAT( NLAYS3D )
                VGLVS3D( L ) = GETREAL( 0.0, 9.99E36, VSCR, MESG )
22          CONTINUE
            DO  23  L = NLAYS3D+2, MXLAYS3
                VGLVS3D( L ) = BADVAL3
23          CONTINUE

        ELSE

            VGTYP3D = IMISS3
            VGTOP3D = BADVAL3
            DO  24  L = 0,MXLAYS3
                VGLVS3D( L ) = BADVAL3
24          CONTINUE

        END IF		!  if nlays3d > 1, or not


C...............   Get timestep structure:

        WRITE( *,92000 )
     &  ' ',
     &  'Next, let us construct the time step structure for the file',
     &  '    TSTEP > 0:  "timestepped" file;',
     &  '    TSTEP = 0   "time-independent" file;',
     &  '    TSTEP > 0   "circular-buffer" file.', ' '

        TSTEP3D = GETNUM( -999999999, 999999999, 10000,
     &                    'Enter TSTEP (hhmmss)' )
        IF ( TSTEP3D .EQ. 0 ) THEN
            SDATE3D = 0
            STIME3D = 0
            NSTEPS  = 1
        ELSE
            SDATE3D = GETNUM( 1900000, 9999366, 1988200,
     &                        'Enter STARTING DATE (yyyyddd)' )
            STIME3D = GETNUM( 0, 235959, 0,
     &                        'Enter STARTING TIME (hhmmss)' )
            NSTEPS = GETNUM( 0, 999999, 24,
     &                       'Enter number of time steps' )
        END IF

        JDATE = SDATE3D
        JTIME = STIME3D
        TSTEP = TSTEP3D


C...............   Get variables and their descriptions:

        NVARS3D = GETNUM( 0, MXVARS3, 1,
     &                    'Enter number of variables' )

        WRITE( *,92000 )
     &  ' ',
     &  'Now enter the variable descriptions',
     &  ' '
        DO  11  V = 1, NVARS3D

            WRITE( MESG,94000 ) 'Enter NAME for variable', V, ' > '
            WRITE( *,95000 ) MESG ( 1 : 1+TRIMLEN( MESG ) )
            READ(  *,93000 ) VNAME3D( V )

            WRITE( MESG,94000 ) 'Enter UNITS for variable', V, ' > '
            WRITE( *,95000 ) MESG ( 1 : 1+TRIMLEN( MESG ) )
            READ(  *,93000 ) UNITS3D( V )

            WRITE( MESG,94000 )
     &              'Enter DESCRIPTION for variable', V, ' > '
            WRITE( *,95000 ) MESG ( 1 : 1+TRIMLEN( MESG ) )
            READ(  *,93000 ) VDESC3D( V )

            WRITE( MESG,94000 ) 'Enter TYPE for variable', V
            VTYPE3D( V ) = VTYPES( GETMENU( 3, 1, MESG, VMENU ) )

            WRITE( MESG,94000 ) 'Enter FILLER FORMULA for variable', V
            OPTYPES( V ) = ATYPES( GETMENU( 6, 1, MESG, AMENU ) )
            IF ( OPTYPES( V ) .EQ. 5 ) THEN
                FILLVAL( V ) = GETREAL( BADVAL3, -BADVAL3, 0.0,
     &                                  'Enter fill value' )
            ELSE IF ( OPTYPES( V ) .EQ. 6 ) THEN

                WRITE( *,92000 )
     &      ' ',
     &      'The input data file must be a list-directed (e.g.,',
     &      'comma-delimited) ASCII file containing all of the',
     &      'desired input data to be copied into the output',
     &      'file, ordered and formatted as follows:',
     &      ' ',
     &      '    The fastest subscript should be grid-column',
     &      '    There should be a line-break after each column',
     &      '    The   next  subscript should be grid-row',
     &      '    The   next  subscript should be grid-layer',
     &      '    The slowest subscript should be time-step',
     &      ' '
                IDEV = PROMPTFFILE('Enter logical name for input file',
     &                             .TRUE., .TRUE., VNAME3D( V ),
     &                             'M3FAKE' )
                OPTYPES( V ) = -IDEV

            END IF

11      CONTINUE

        WRITE( *,92000 )
     &  ' ',
     &  'Now enter the file description (end with a BLANK LINE)',
     &  ' '

        DO 44 L = 1, MXDESC3
            WRITE( *,95000 ) ' > '
            READ ( *,93000, END=45 ) FDESC3D( L )
            IF ( FDESC3D( L ) .EQ. ' ' ) THEN
                GO TO  46
            END IF
44      CONTINUE
45      CONTINUE
        CALL M3MESG( ' ' )
46      CONTINUE

C...........   Open output file (prompting as necessary)

        ARGCNT = IARGC()

55      CONTINUE

            IF ( ARGCNT .EQ. 0 ) THEN	!  prompt for file name

                FNAME = PROMPTMFILE(
     &              'Enter logical name for output file',
     &              FSUNKN3, 'OUTFILE', 'M3FAKE' )

            ELSE IF ( ARGCNT .EQ. 1 ) THEN	!  file name from command line

                CALL GETARG( 1, ENVBUF )
                FNAME = ENVBUF( 1:16 )
                IF ( .NOT. OPEN3( FNAME, FSUNKN3, 'M3FAKE' ) ) THEN

                    MESG = 'Could not open "' // FNAME // '"'
                    CALL M3WARN( 'M3FAKE', 0, 0, MESG )
                    IF ( GETYN( 'Prompt for logical file name?',
     &                          .TRUE. ) ) THEN
                        ARGCNT = 0
                        GO TO  55
                    END IF

                END IF	!  if open3() failed

            ELSE

                CALL M3WARN( 'M3FAKE', 0, 0,
     &                       'Usage:  "m3fake [file]"' )
                IF ( GETYN( 'Prompt for logical file name?',
     &                      .TRUE. ) ) THEN
                    ARGCNT = 0
                    GO TO  55
                END IF

            END IF	!  if argcnt 0, 1, or "wrong"

        DO  111  T = 1, NSTEPS

            CALL FAKESTEP( FNAME, JDATE, JTIME, OPTYPES, FILLVAL )
            CALL NEXTIME ( JDATE, JTIME, TSTEP )

111     CONTINUE


      CALL M3EXIT( 'M3FAKE', 0, 0,
     &             'Program completed successfully', 0 )

C******************  FORMAT  STATEMENTS   ******************************

C...........   Informational (LOG) message formats... 92xxx

92000	FORMAT( 5X, A )


C...........   Formatted file I/O formats............ 93xxx

93000	FORMAT( A )

C...........   Internal buffering formats............ 94xxx

94000	FORMAT( A, I4, :, A )


C...........   Miscellaneous formats................. 95xxx

95000   FORMAT ( /5X , A , $ )          !  generic prompt format.


        END

