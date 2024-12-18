
        PROGRAM KFXTRACT

C***********************************************************************
C Version "@(#)$Header$ $Id: kfxtract.f 49 2007-07-06 16:20:50Z coats@borel $"
C EDSS/Models-3 M3TOOLS. 
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr, and
C (C) 2002-2007 Baron Advanced Meteorological Systems, LLC.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  program body starts at line  88
C
C  DESCRIPTION:
C       Extracts a specified time period from a specified KF file,
C       creating a new KF file for the requested time period
C
C  PRECONDITIONS REQUIRED:
C       "setenv"s for the input, output files.
C       Input must be a valid KF file.
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       I/O API
C
C  REVISION  HISTORY:
C       Prototype 5/98 by Carlie J Coats, Jr., NCSC
C       Version  11/2001 by CJC for I/O API Version 2.1
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

      INCLUDE 'PARMS3.EXT'      ! I/O API constants
      INCLUDE 'FDESC3.EXT'      ! I/O API file description data structure
      INCLUDE 'IODECL3.EXT'     ! I/O API function declarations


C...........   PARAMETERS and their descriptions:

        INTEGER         BUFSIZ
        PARAMETER     ( BUFSIZ = 10000 )
        CHARACTER*80 PROGVER
       
        DATA PROGVER /
     &'$Id:: kfxtract.f 49 2007-07-06 16:20:50Z coats@borel          $'
     &  /


C...........   EXTERNAL FUNCTIONS and their descriptions:

        LOGICAL         GETYN
        INTEGER         GETNUM, INDEX1, IARGC, KFWRITE, TRIMLEN
        INTEGER         SEC2TIME, TIME2SEC, SECSDIFF
        LOGICAL         KFOPEN, KFREAD
        REAL            GETREAL

        EXTERNAL  GETNUM, GETYN, GETREAL, INDEX1, 
     &            KFOPEN, KFREAD, KFWRITE, TRIMLEN,  
     &            SEC2TIME, TIME2SEC, SECSDIFF


C...........   LOCAL VARIABLES and their descriptions:

        INTEGER         ARGCNT  !  number of command-line args, from IARGC()
        INTEGER         LOGDEV  !  unit number for log file

        CHARACTER*256   ENVBUF  !  value from command line arguments
        CHARACTER*16    INAME   !  logical name of the  input file
        CHARACTER*16    ONAME   !  logical name of the output file

        INTEGER         NRECS   !  input file extent, in records
        INTEGER         NTHIK   !  input file NTHIK3D
        INTEGER         REC     !  loop counter
        INTEGER         IEV     !  event counter
        LOGICAL         EFLAG
        
        INTEGER         JDATE, JTIME, KFLEN, COL, ROW
        INTEGER         SDATE, STIME, EDATE, ETIME
        INTEGER         RUNLEN, NRPCELL
        REAL            VARS  ( BUFSIZ )
        INTEGER         EVCNTS( BUFSIZ )
        LOGICAL         OFLAG

        CHARACTER*256   MESG

C***********************************************************************
C   begin body of program KFXTRACT

        WRITE( *,92000 )
     & ' ',
     & 'Program KFXTRACT to copy a specified time period from an',
     & 'existing KF file from an to a new file.  You need to have',
     & 'assigned logical names to the input and output files',
     & 'according to I/O API conventions, using the operation',
     & ' ',
     & '   setenv <lname> <pname>',
     & ' ',
     & 'USAGE:  KFXTRACT [INFILE OUTFILE]', ' ',
     & 'and then answer the prompts for the time period and the',
     & 'dimension of events-per-cell.',
     & ' ',
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

C.........  Get file names:

        LOGDEV = INIT3()
        ARGCNT = IARGC()

        IF ( ARGCNT .EQ. 2 ) THEN
            CALL GETARG( 1, ENVBUF )
            INAME = ENVBUF( 1:16 )
            CALL GETARG( 2, ENVBUF )
            ONAME = ENVBUF( 1:16 )
        ELSE IF ( ARGCNT .EQ. 0 ) THEN
            CALL GETSTR( 'Enter  INPUT KF file logical name',  
     &                   'INFILE', INAME )
            CALL GETSTR( 'Enter OUTPUT KF file logical name',  
     &                   'OUTFILE', ONAME )
        ELSE
            CALL M3EXIT( 'KFXTRACT', 0, 0, 
     &                   'USAGE:  KFXTRACT [INFILE OUTFILE]', 2 )
        END IF

C.........  Open files:

        IF ( .NOT.KFOPEN( INAME, FSREAD3, 'KFXTRACT', EVCNTS ) ) THEN
            MESG = 'Could not open input file "' //
     &             INAME( 1:TRIMLEN( INAME ) ) // '"'
            CALL M3EXIT( 'KFXTRACT', 0, 0, MESG, 2 )
        END IF

        IF ( .NOT. DESC3( INAME ) ) THEN
            MESG = 'Could not describe input file "' //
     &             INAME( 1:TRIMLEN( INAME ) ) // '"'
            CALL M3EXIT( 'KFXTRACT', 0, 0, MESG, 2 )
        END IF
        
        EFLAG = .FALSE.
        IF ( NCOLS3D * NROWS3D .GT. BUFSIZ ) THEN
            EFLAG = .TRUE.
            WRITE( MESG,94010 ) 
     &      'Buffer overflow. Dimensioned', BUFSIZ,  
     &      'required', NCOLS3D * NROWS3D
            CALL M3MSG2( MESG )
        END IF 
        IF ( NLAYS3D * NVARS3D .GT. BUFSIZ ) THEN
            EFLAG = .TRUE.
            WRITE( MESG,94010 ) 
     &      'Buffer overflow. Dimensioned', BUFSIZ,  
     &      'required', NLAYS3D * NVARS3D
            CALL M3MSG2( MESG )
        END IF
        
        IF ( EFLAG ) THEN
            CALL M3EXIT( 'KFXTRACT', 0, 0, 'Dimensioning error', 2 )
        END IF
        
        NRECS = MXREC3D
        NTHIK = NTHIK3D
        
        JDATE = SDATE3D
        JTIME = STIME3D
        CALL NEXTIME( JDATE, JTIME, -10000 )
        
        SDATE = GETNUM( JDATE, 9999999, JDATE,
     &                  'Enter starting date for run (YYYYDDD)' )

        STIME = GETNUM( 0, 9999999, JTIME,
     &                  'Enter starting time for run  (HHMMSS)' )

        RUNLEN = SEC2TIME( NTHIK3D*1800
     &                   - SECSDIFF( SDATE3D, STIME3D, SDATE, STIME ) )
        RUNLEN = GETNUM( 10000, 99999999, RUNLEN, 
     &                  'Enter duration for run       (HHMMSS)' )

        EDATE = SDATE
        ETIME = STIME
        CALL NEXTIME( EDATE, ETIME, RUNLEN )

        CALL M3MSG2(   
     & 'The output-file dimension for EVENTS_PER_CELL must not' //  
     & 'exceed PARAMETER MXKFPC from KFBMRS_SCHED' )
        NRPCELL = TIME2SEC( RUNLEN ) / 1200 !  default-- 3 events per hour
        NRPCELL = GETNUM( 1, 9999999, NRPCELL,
     &                    'Enter dimension EVENTS_PER_CELL' )

        NTHIK3D = NRPCELL
        IF ( .NOT. KFOPEN( ONAME, FSUNKN3, 'KFXTRACT', EVCNTS ) ) THEN
            MESG = 'Could not open output file "' //
     &             ONAME( 1:TRIMLEN( ONAME ) ) // '"'
            CALL M3EXIT( 'KFXTRACT', 0, 0, MESG, 2 )
        END IF
        
C.........  Log run-parameters:
        
        WRITE( *,92010 ) 
     &     'Input file parameters', 
     &     'Starting date & time:', SDATE3D, STIME3D, 
     &     'Number of records:',    MXREC3D, 
     &     'Max records-per-cell',  NTHIK

        WRITE( *,92020 ) 
     &     'Output file parameters', 
     &     'Starting date & time:', SDATE, STIME, 
     &     'Ending   date & time:', EDATE, ETIME, 
     &     'Max records-per-cell',  NTHIK3D

C.........  Copy all event-records:

        DO  REC = 1, NRECS

            IF ( .NOT.KFREAD( INAME, ALLVAR3, REC, COL, ROW, 
     &                        JDATE, JTIME, KFLEN, VARS ) ) THEN
                WRITE( MESG, 94010 ) 
     &         'Error reading event', REC, 'from file ' // INAME
                CALL M3EXIT( 'KFXTRACT', 0, 0, MESG, 2 )
            END IF

            OFLAG = ( ( JDATE .GT. SDATE ) .OR.         !  JDATE:JTIME is
     &                ( JDATE .EQ. SDATE .AND.          !  _between SDATE:STIME
     &                  JTIME .GE. STIME) ) .AND.       ! and EDATE:ETIME
     &              ( ( JDATE .LT. EDATE ) .OR.  
     &                ( JDATE .EQ. EDATE .AND.    
     &                  JTIME .LT. ETIME ) )

            IF ( OFLAG ) THEN
                IEV = KFWRITE( ONAME, COL, ROW,  
     &                         JDATE, JTIME, KFLEN, VARS )
                IF ( IEV .LE. 0 ) THEN
                    WRITE( MESG, 94010 ) 
     &             'Error writing event', REC, 'to file ' // ONAME
                    CALL M3EXIT( 'KFXTRACT', 0, 0, MESG, 2 )
                END IF
            END IF
        END DO

        CALL M3EXIT( 'KFXTRACT', 0, 0, 
     &               'Successful completion of program KFXTRACT', 0 )
C      STOP

C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx
C...........   Informational (LOG) message formats... 92xxx

92000   FORMAT ( 5X, A )

92010   FORMAT ( /5X, A, /10X, A, I9.7, ':', I6.6, 2(/10X, A, I10 ) )

92020   FORMAT ( /5X, A, 2( /10X, A, I9.7, ':', I6.6 ), /10X, A, I10 )


C...........   Formatted file I/O formats............ 93xxx
C...........   Internal buffering formats............ 94xxx

94010   FORMAT( 5( A, :, I7, :, 2X ) )

C...........   Miscellaneous formats................. 95xxx


        END

