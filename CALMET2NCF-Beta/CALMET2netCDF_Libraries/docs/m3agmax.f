
        PROGRAM  M3AGMAX

C***********************************************************************
C Version "@(#)$Header$ $Id: m3agmax.f 49 2007-07-06 16:20:50Z coats@borel $"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr, and
C (C) 2002-2007 Baron Advanced Meteorological Systems, LLC.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  program body starts at line 157
C
C  FUNCTION:
C       Sums, give max, or gives average over a specified time period
C       for a subset of variables from the input file, and writes the
C       processed data to the output file.
C       Logs sorted list of max values, their locations, and times.
C
C  PRECONDITIONS REQUIRED:
C       Machine with stack-allocated AUTO local variables (e.g., CRAY)
C       consistency with FORIO:PARMS3.EXT for name and description lengths.
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       GETNUM, NEXTIME, Models-3 I/O.
C
C  REVISION  HISTORY:
C       Prototype 5/1997 by M Houyoux
C       Modified 11/1999 by Carlie J. Coats, Jr.:  max-value report
C       Version  11/2001 by CJC for I/O API Version 2.1
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'  !  I/O parameter definitions
        INCLUDE 'FDESC3.EXT'  !  file header data structures
        INCLUDE 'IODECL3.EXT' !  I/O definitions and declarations


C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER       ENVINT
        LOGICAL       ENVYN
        INTEGER       GETDATE
        INTEGER       GETMENU
        INTEGER       GETNUM
        REAL          GETREAL
        LOGICAL       GETYN
        INTEGER       IARGC
        INTEGER       INDEX1
        INTEGER       JULIAN
        CHARACTER*16  PROMPTMFILE
        INTEGER       SEC2TIME
        INTEGER       SECSDIFF
        INTEGER       TIME2SEC
        INTEGER       TRIMLEN

        EXTERNAL  ENVINT, ENVYN, GETDATE, GETMENU, GETNUM, GETREAL,
     &            GETYN, INDEX1, JULIAN, PROMPTMFILE,
     &            SEC2TIME, SECSDIFF, TIME2SEC, TRIMLEN

C...........   PARAMETERS and their descriptions:

        CHARACTER*16    BLANK16
        INTEGER         M3AVE
        INTEGER         M3MAX
        INTEGER         M3SUM
        INTEGER         MAXITEMS
        INTEGER         MAXRECS

        PARAMETER     ( BLANK16  = ' ',
     &                  M3SUM    =    1,
     &                  M3AVE    =    2,
     &                  M3MAX    =    3,
     &                  MAXRECS  = 5000,
     &                  MAXITEMS =    3  )

        CHARACTER*80    PROGVER
        DATA PROGVER /
     &'$Id:: m3agmax.f 49 2007-07-06 16:20:50Z coats@borel           $'
     &  /

C...........   LOCAL VARIABLES and their descriptions:

        CHARACTER*16    ANAME   !  scratch buffer for variable names
        CHARACTER*16    INAME   !  logical name of the  input file
        CHARACTER*16    ONAME   !  logical name of the aggregate-output file
        CHARACTER*16    MNAME   !  logical name of the max-output file
        INTEGER         VTYPE ( MXVARS3 ) !  data-type of variables
        CHARACTER*16    UNITS ( MXVARS3 ) !  list of vble units
        CHARACTER*16    VNAMEI( MXVARS3 ) !  list of vble names, from user
        CHARACTER*16    VNAMEO( MXVARS3 ) !  list of vble names, from user
        CHARACTER*80    ALINE   !  scratch buffer for prompt
        CHARACTER*80    VDESC ( MXVARS3 ) !  list of vble descs
        CHARACTER*256   ENVBUF  !  value from command line arguments
        CHARACTER*256   MESG    !  for M3WARN(), M3EXIT()

        INTEGER         I, T, V !  loop counter (time step #)

        INTEGER         ARGCNT  !  number of command-line args, from IARGC()
        INTEGER         BDATE   !  beginning analysis window date
        INTEGER         BTIME   !  beginning analysis window time
        INTEGER         DAY     !  temporary day
        INTEGER         DMAX    !  string length for descriptions
        INTEGER         IOS     !  I/O status
        INTEGER         JDATE   !  current  input date
        INTEGER         JTIME   !  current  input time
        INTEGER         LOGDEV  !  unit number for log file
        INTEGER         MON     !  temporary month
        INTEGER         NVSAV   !  number of variables in input file
        INTEGER         NVARS   !  number of vbles in ONAME
        INTEGER         PERIOD  !  Period length (HHMMSS) for repetitive analysis
        INTEGER         PERSEC  !  Period length in seconds
        INTEGER         RUNSEC  !  Run length in seconds
        INTEGER         PDATE   !  current output date
        INTEGER         PTIME   !  current output time
        INTEGER         NCOLS   !  dimension
        INTEGER         NROWS   !  dimension
        INTEGER         NLAYS   !  dimension
        INTEGER         SDATE   !  starting  input date, from user
        INTEGER         STIME   !  starting  input time, from user
        INTEGER         RUNLEN  !  duration, HHMMSS from user
        INTEGER         NOUTS   !  Number of output time steps
        INTEGER         TSTEP   !  time step, from INAME header
        INTEGER         INSECS  !  time2sec(tstep)
        INTEGER         ATYPE   !  type of aggregation being performed
        INTEGER         UMAX    !  string length for units
        INTEGER         WSTEPS  !  window duration in TSTEPs
        INTEGER         VMAX    !  string length for names
        INTEGER         WINLEN  !  duration (HHMMSS) of analysis window
        INTEGER         YR      !  temporary year

        REAL,    ALLOCATABLE::  CMAX ( : )        !  data structures for
        INTEGER, ALLOCATABLE::  CDATE( : )        !  computing and sorting
        INTEGER, ALLOCATABLE::  CTIME( : )        !  hourly max of the
        INTEGER, ALLOCATABLE::  CCOL ( : )        !  agg. concentrations
        INTEGER, ALLOCATABLE::  CROW ( : )
        INTEGER, ALLOCATABLE::  CLAY ( : )
        INTEGER, ALLOCATABLE::  CDEX ( : )

        LOGICAL         NPFLAG  !  iff no prompting for variables

        CHARACTER*80    MENUITMS( MAXITEMS )!  buffer for operations menu items
        DATA            MENUITMS /
     &           'Sum over time window',
     &           'Calculate average over time window',
     &           'Determine maximum over time window'   /

        CHARACTER*2     SUFFIX( 6 )
        DATA            SUFFIX / '_1', '_2', '_3', '_4', '_5', '_6' /

C.........................................................................
C   begin body of program  M3AGMAX

        LOGDEV = INIT3()
        WRITE ( *,92000 )
     &  ' ',
     &  'Program M3AGMAX to sum, average, or find the maximum values',
     &  'over a repeating time period from a selected time window.',
     &  'The time period and starting time window set the start and',
     &  'duration of all subsequent time windows. The program inputs',
     &  'and outputs Models-3 files.',
     &  ' ',
     &  'This version of the program will also report to the program',
     &  'log a sorted list of the hourly maxes of the output, by ',
     &  'variable, together with their times and locations.',
     &  ' ',
     &  'You need to have set environment variables for the input',
     &  'and output file logical names.  You will be asked to select',
     &  'the time period to be copied and the start of the time ',
     &  'period to receive the results.',
     &  ' ',
     &  'USAGE:  M3AGMAX [INFILE OUTFILE MAXFILE] ',
     &  '(and then answer the prompts).',
     &  ' ',
     &'See URL  http://www.baronams.com/products/ioapi/AA.html#tools',
     &' ',
     &'Program copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.',
     &'and (C) 2002-2007 Baron Advanced Meteorological Systems, LLC',
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

        ARGCNT = IARGC()

        IF ( ARGCNT .NE. 0  .AND.  ARGCNT .NE. 3 ) THEN
            CALL M3EXIT( 'M3AGMAX', 0, 0,
     &                   'usage:  M3AGMAX [INFILE OUTFILE MAXFILE ]',
     &                   2 )
        END IF

        IF ( ARGCNT .EQ. 0 ) THEN       !  get names from user

            INAME = PROMPTMFILE( 'Enter logical name for  INPUT FILE',
     &                           FSREAD3, 'INFILE', 'M3AGMAX' )

        ELSE		!  argcnt 3

            CALL GETARG( 1, ENVBUF )
            INAME = ENVBUF( 1:16 )
            IF ( .NOT. OPEN3( INAME, FSREAD3, 'M3AGMAX' ) ) THEN
                MESG = 'Could not open input file "'
     &                       // INAME( 1:TRIMLEN( INAME ) ) // '"'
                CALL M3EXIT( 'M3AGMAX', 0, 0, MESG, 2 )
            END IF

            CALL GETARG( 2, ENVBUF )
            ONAME = ENVBUF( 1:16 )

            CALL GETARG( 3, ENVBUF )
            MNAME = ENVBUF( 1:16 )

        END IF


        IF ( .NOT. DESC3( INAME ) ) THEN
            MESG = 'Could not get description of input file "' //
     &             INAME( 1:TRIMLEN( INAME ) ) // '"'
            CALL M3EXIT( 'M3AGMAX', 0, 0, MESG, 2 )
        END IF

        IF ( FTYPE3D .EQ. GRDDED3 ) THEN
            NCOLS = NCOLS3D
            NROWS = NROWS3D
            NLAYS = NLAYS3D
        ELSE
            WRITE( MESG, 94011 )
     &      'Input file "', INAME( 1:TRIMLEN( INAME ) ),
     &      '" has unsupported type', FTYPE3D
            CALL M3EXIT( 'M3AGMAX', 0, 0, MESG, 2 )
        END IF

        NVSAV  = NVARS3D
        SDATE  = SDATE3D
        STIME  = STIME3D
        TSTEP  = TSTEP3D

C.......   Get max string-lengths for use in variables-listing:

        VMAX = TRIMLEN( VNAME3D( 1 ) )
        UMAX = TRIMLEN( UNITS3D( 1 ) )
        DMAX = TRIMLEN( VDESC3D( 1 ) )
        DO  11  V = 1, NVARS3D
            VMAX = MAX( VMAX , TRIMLEN( VNAME3D( V ) ) )
            UMAX = MAX( UMAX , TRIMLEN( UNITS3D( V ) ) )
            DMAX = MAX( DMAX , TRIMLEN( VDESC3D( V ) ) )
11      CONTINUE

C.......  Determine if all variables are to be used

        NPFLAG = ENVYN( 'M3AGMAX_ALLV',
     &                  'true if no prompting for variables to output',
     &                  .FALSE., IOS )

C.......  If no prompting set to total number of vars, or prompt

        IF( NPFLAG ) THEN

            NVARS = NVARS3D

            DO 22 V = 1, NVARS3D
               VNAMEI( V )  = VNAME3D( V )
               VNAMEO( V )  = VNAME3D( V )
               UNITS ( V )  = UNITS3D( V )
               VDESC ( V )  = VDESC3D( V )
               VTYPE ( V )  = VTYPE3D( V )
22          CONTINUE

        ELSE

            NVARS = 0
            V     = 0

111         CONTINUE        !  loop getting variables-list for extraction

                IF( MOD( NVARS,10 ) .EQ. 0 ) THEN
                    WRITE( *,92000 )
     &              ' ', 'The list of variables in file "'
     &              // INAME( 1:TRIMLEN( INAME ) ) // '" is:', ' '
                    WRITE( *,92010 )
     &              ( I,
     &                VNAME3D( I )( 1:VMAX ) // ' (' //
     &                UNITS3D( I )( 1:UMAX ) // '): ' //
     &                VDESC3D( I )( 1:DMAX ), I = 1, NVSAV  )
                END IF

                V = GETNUM( 0, NVSAV, 1 + MOD( V, NVSAV ),
     &            'Enter number for variable to extract (0 to quit)' )

                IF ( V .EQ. 0 ) GO TO  199      !  to end of loop

                NVARS = NVARS + 1

C...............   Optional renaming of this variable:

122             CONTINUE
                    ALINE = 'Enter output-name for this variable [' //
     &                  VNAME3D( V )( 1 : TRIMLEN( VNAME3D( V ) ) ) //
     &                  '] >> '
                    WRITE( *,95000 ) ALINE( 1:1+TRIMLEN( ALINE ) )
                    READ ( *,93010,IOSTAT=IOS ) ANAME

                    IF ( IOS .GT. 0 ) THEN
                        CALL M3WARN( 'M3AGMAX', 0, 0,
     &                  'Error reading output-name; please try again' )
                        GO TO 122
                    END IF

                IF( ANAME .EQ. BLANK16 ) THEN
                    VNAMEO( NVARS ) = VNAME3D( V )
                ELSE
                    VNAMEO( NVARS ) = ANAME
                END IF
                VNAMEI( NVARS ) = VNAME3D( V )
                UNITS ( NVARS ) = UNITS3D( V )
                VDESC ( NVARS ) = VDESC3D( V )
                VTYPE ( NVARS ) = VTYPE3D( V )

                IF( NVARS .EQ. NVARS3D ) GO TO 199

                IF ( NVARS .LT. MXVARS3 )  GO TO  111   !  to head of loop

199         CONTINUE        !  end loop getting variables-list for analysis

        END IF  ! If prompting or not

        IF ( NVARS .EQ. 0 ) THEN
            CALL M3EXIT( 'M3AGMAX', 0, 0,
     &                  'No variables selected', 2 )
        END IF

C.......   Get starting date and time, and duration:

        IF ( TSTEP .EQ. 0 ) THEN        !  time-independent file

            MESG = 'Input file "' // INAME( 1:TRIMLEN( INAME ) ) //
     &             '" is only one time step- no output written.'
            CALL M3EXIT( 'M3AGMAX', 0, 0, MESG, 2 )

        ELSE                            !  time-dependent file

C.......   Prompt for parameters if defaults aren't set by specific env.
C.......   variable setting. Get default settings from the environment.

C...........  Period length, Start date&time BDATE:BTIME, duration WINLEN
C...........  Number NOUTS of periods to process, analysis type ATYPE

            PERIOD = ENVINT( 'M3AGMAX_PLEN', 'Output time step',
     &                        TSTEP, IOS )
            PERSEC = TIME2SEC( PERIOD )
            INSECS = TIME2SEC( TSTEP )
            IF ( MOD( PERSEC, INSECS ) .NE. 0 ) THEN
                MESG =  'Output time step mismatch'
                CALL M3EXIT( 'M3AGMAX', 0, 0, MESG, 2 )
            END IF

            RUNSEC = MXREC3D * TIME2SEC( TSTEP )
            RUNLEN = SEC2TIME( RUNSEC )

            IF( IOS .NE. 0 ) PERIOD = GETNUM( TSTEP, RUNLEN, TSTEP,
     &       'Enter output time step (HHMMSS) for repeating analysis' )

            BDATE = ENVINT( 'M3AGMAX_BDATE', 'Window start date',
     &                      SDATE, IOS )

            IF( IOS .NE. 0 ) THEN
                MESG = 'Enter starting date for run (YYYYDDD|YYYYMMDD)'
                BDATE  = GETDATE( BDATE, MESG )

            ELSE IF( BDATE .GT. 9999366 ) THEN   ! Convert to Julian

                YR    = BDATE/10000
                MON   = ( BDATE-YR*10000 ) / 100
                DAY   = MOD( BDATE-YR*10000, 100 )
                BDATE = YR*1000 + JULIAN( YR, MON, DAY )

            END IF

            BTIME = ENVINT( 'M3AGMAX_BTIME', 'Window start time',
     &                       STIME, IOS )

            IF( IOS .NE. 0 )  THEN
                I = BTIME
                BTIME  = GETNUM( 0, 239999, I,
     &                  'Enter starting time for run (HHMMSS)' )
            END IF

            T = SECSDIFF( SDATE, STIME, BDATE, BTIME )
            IF ( MOD( T, INSECS ) .NE. 0 ) THEN
                MESG =  'Run-start not exact time step from file start'
                CALL M3EXIT( 'M3AGMAX', 0, 0, MESG, 2 )
            END IF

            RUNSEC = RUNSEC - T
            T = SEC2TIME( RUNSEC )

            I = MIN( SEC2TIME( 8*PERSEC ), T )
            WINLEN = ENVINT( 'M3AGMAX_WLEN',
     &                       'Aggregation-period duration',
     &                        I, IOS )

            IF( IOS .NE. 0 ) THEN
                I = WINLEN
                WINLEN = GETNUM( 1, T, I,
     &                'Enter duration of aggregation-period (HHMMSS)' )
            END IF

            RUNSEC = RUNSEC - TIME2SEC( WINLEN )

            NOUTS = ENVINT( 'M3AGMAX_NPER',
     &                      'Number of output time steps',
     &                       RUNSEC / PERSEC, IOS )

            IF( IOS .NE. 0 )  THEN
                I = NOUTS
                NOUTS = GETNUM( 1, I, T,
     &             'Enter number of output time steps to analyze' )
            END IF

            WSTEPS = TIME2SEC( WINLEN )  / INSECS

            ATYPE = ENVINT( 'M3AGMAX_TYPE',
     &                      'Type of analysis',
     &                       M3AVE, IOS )

            IF( IOS .NE. 0 ) THEN
                ATYPE = GETMENU( MAXITEMS, ATYPE,
     &                        'Enter type of operation to perform',
     &                        MENUITMS )
            END IF

        END IF          !  time-independent file, or not



C.......   Build description for the output file, and create accordingly:
C.......   Re-use all but the starting date&time of the input-file description.

        SDATE3D = BDATE
        STIME3D = BTIME
        TSTEP3D = PERIOD

        IF ( ARGCNT .EQ. 0 ) THEN

            NVARS3D = NVARS
            DO   V = 1, NVARS
                VNAME3D( V ) = VNAMEO( V )
                UNITS3D( V ) = UNITS ( V )
                VDESC3D( V ) = VDESC ( V )
                VTYPE3D( V ) = VTYPE ( V )
            END DO
            ONAME = PROMPTMFILE( 'Enter logical name for AGG FILE',
     &                           FSUNKN3, 'OUTFILE', 'M3AGMAX' )

            NVARS3D = 8 * NVARS
            I = 0
            DO  V = 1, NVARS
                DO  T = 1, 6
                    I = I + 1
                    VNAME3D( I ) = TRIM( VNAMEO( V ) ) // SUFFIX( T )
                    UNITS3D( I ) = UNITS ( V )
                    VDESC3D( I ) = SUFFIX( T )(2:2)//'th highest value'
                    VTYPE3D( I ) = VTYPE ( V )
                END DO
                I = I + 1
                VNAME3D( I ) = TRIM( VNAMEO( V ) ) // '_1_125'
                UNITS3D( I ) = 'none'
                VDESC3D( I ) = '1-hour 125 PPB exceedance count'
                VTYPE3D( I ) = M3INT
                I = I + 1
                VNAME3D( I ) = TRIM( VNAMEO( V ) ) // '_8_85'
                UNITS3D( I ) = 'none'
                VDESC3D( I ) = '8-hour 85 ppb exceedance count'
                VTYPE3D( I ) = M3INT
            END DO

            MNAME = PROMPTMFILE( 'Enter logical name for AGG-MAX FILE',
     &                           FSUNKN3, 'MAXFILE', 'M3AGMAX' )

        ELSE	!  argcnt = 3:

            NVARS3D = NVARS
            DO   V = 1, NVARS
                VNAME3D( V ) = VNAMEO( V )
                UNITS3D( V ) = UNITS ( V )
                VDESC3D( V ) = VDESC ( V )
                VTYPE3D( V ) = VTYPE ( V )
            END DO
            IF ( .NOT. OPEN3( ONAME, FSUNKN3, 'M3AGMAX' ) ) THEN
                MESG = 'Could not open output AGG-file "' //
     &                 ONAME( 1:TRIMLEN( ONAME ) ) // '"'
                CALL M3EXIT( 'M3AGMAX', SDATE, STIME, MESG, 2 )
            END IF

            NVARS3D = 8 * NVARS
            I = 0
            DO  V = 1, NVARS
                DO  T = 1, 6
                    I = I + 1
                    VNAME3D( I ) = TRIM( VNAMEO( V ) ) // SUFFIX( T )
                    UNITS3D( I ) = UNITS ( V )
                    VDESC3D( I ) = SUFFIX( T )(2:2)//'th highest value'
                    VTYPE3D( I ) = VTYPE ( V )
                END DO
                I = I + 1
                VNAME3D( I ) = TRIM( VNAMEO( V ) ) // '_1_125'
                UNITS3D( I ) = 'none'
                VDESC3D( I ) = '1-hour 125 PPB exceedance count'
                VTYPE3D( I ) = M3INT
                I = I + 1
                VNAME3D( I ) = TRIM( VNAMEO( V ) ) // '_8_85'
                UNITS3D( I ) = 'none'
                VDESC3D( I ) = '8-hour 85 ppb exceedance count'
                VTYPE3D( I ) = M3INT
            END DO

            IF ( .NOT. OPEN3( MNAME, FSUNKN3, 'M3AGMAX' ) ) THEN
                MESG = 'Could not open output AGG-MAX file "' //
     &                 MNAME( 1:TRIMLEN( MNAME ) ) // '"'
                CALL M3EXIT( 'M3AGMAX', SDATE, STIME, MESG, 2 )
            END IF

        END IF		!  if argcnt zero, or 2


C.......   Allocate aggregation arrays:

        ALLOCATE ( CMAX ( RUNLEN ),
     &             CDATE( RUNLEN ),
     &             CTIME( RUNLEN ),
     &             CCOL ( RUNLEN ),
     &             CROW ( RUNLEN ),
     &             CLAY ( RUNLEN ),
     &             CDEX ( RUNLEN ), STAT = I )
        IF ( I .NE. 0 ) THEN
            MESG = 'Allocation failure'
            CALL M3EXIT( 'M3AGMAX', SDATE, STIME, MESG, 2 )
        END IF


C.......   Log run-characteristics:

        WRITE( MESG, '(A, I9, A, I6.6)' )
     &          'Starting date and time:', BDATE, ':', BTIME
        CALL M3MSG2( MESG )

        WRITE( MESG, '(A, 2X, I6.6)' )
     &          'Output timestep (HHMMSS)', PERIOD
        CALL M3MSG2( MESG )

        WRITE( MESG, '(A, I6)' ) 'Number of output time steps', RUNLEN
        CALL M3MSG2( MESG )

        WRITE( MESG, '(A, I6)' ) 'Number of aggregation steps', WSTEPS
        CALL M3MSG2( MESG )

        WRITE( MESG, '(A, I4)' ) 'Number of variables', NVARS
        CALL M3MSG2( MESG )

        MESG = 'Time-Stepped output file "' // TRIM( ONAME ) // '"'
        CALL M3MSG2( MESG )

        MESG = 'Cumulative output file "' // TRIM( MNAME ) // '"'
        CALL M3MSG2( MESG )


C.......   Process this period in the input file:

        DO  322  V = 1, NVARS

            PDATE = BDATE
            PTIME = BTIME
            MESG  = 'Processing variable "'//TRIM( VNAMEI( V ) )//'"'
            CALL M3MSG2( MESG )

            DO  311  T = 1, NOUTS

                JDATE = PDATE
                JTIME = PTIME

                CALL INITAGG( NCOLS, NROWS, NLAYS, T, JDATE, JTIME,
     &                        INAME, VNAMEI( V ), LOGDEV )

                CALL NEXTIME( JDATE, JTIME, TSTEP )

                DO  301 I = 2, WSTEPS


                    CALL AGGREG( NCOLS, NROWS, NLAYS, JDATE, JTIME,
     &                           INAME, VNAMEI( V ), ATYPE, LOGDEV )


                    CALL NEXTIME( JDATE, JTIME, TSTEP )

301             CONTINUE    !  end loop on window time steps

                CDATE( T ) = PDATE
                CTIME( T ) = PTIME
                CDEX ( T ) = T
                CALL OUTAGG( NCOLS, NROWS, NLAYS, T, PDATE, PTIME,
     &                       ONAME, VNAMEO( V ), ATYPE, WSTEPS,
     &                       CMAX(T), CCOL(T),  CROW(T),
     &                       CLAY(T), LOGDEV )

            CALL NEXTIME( PDATE, PTIME, PERIOD )

311         CONTINUE            !  end loop on analysis periods


C...........   Sort the aggregate-maxes:

            MESG = 'Processing ranked cumulative run-statistics'
            CALL M3MSG2( MESG )

            CALL MAXAGG( BDATE, BTIME, MNAME, VNAMEO( V ) )

            WRITE( LOGDEV, '(/5X, 2 A, /)' ) 'Variable ', VNAMEO( V )
            CALL SORTR1( NOUTS, CDEX, CMAX )
            DO  I = NOUTS, 1, -1
                T = CDEX( I )
                WRITE( LOGDEV, '( 5X, A, 2X, I4, 2X,
     &                          A, 2X, 1PE14.7, 2X,
     &                          A, I7.7, A, I6.6, 2X,
     &                          A, 3( I3, A ) )' )
     &                   'Rank:', I,
     &                   'Max:', CMAX( T ),
     &                   'Date&Time:', CDATE( T ), ':', CTIME( T ),
     &                   'at (C,R,L)=(', CCOL( T ), ',', CROW( T ),
     &                                              ',', CLAY( T ), ')'
            END DO

322     CONTINUE            !  end loop on variables

        CALL M3EXIT( 'M3AGMAX', 0, 0,
     &               'Program  M3AGMAX  completed successfully', 0 )


C..............  FORMAT STATEMENTS:  ....................................

C...........   Informational (LOG) message formats... 92xxx

92000   FORMAT ( 5X , A )

92010   FORMAT ( 1X , I5, ':  ', A )

C...........   Formatted file I/O formats............ 93xxx

93010   FORMAT( A16 )

C...........   Internal buffering formats............ 94xxx

94010   FORMAT ( 100( A, :, 2X, I5, :, 2X ) )

94011   FORMAT ( 3A, I5 )

C...........   Miscellaneous formats................. 95xxx

95000   FORMAT ( /5X , A , $ )          !  generic prompt format.

        END

