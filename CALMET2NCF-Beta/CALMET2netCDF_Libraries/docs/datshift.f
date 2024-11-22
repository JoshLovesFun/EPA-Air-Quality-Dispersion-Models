
        PROGRAM DATSHIFT

C***********************************************************************
C Version "@(#)$Header$ $Id: datshift.f 49 2007-07-06 16:20:50Z coats@borel $"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr, and
C (C) 2002-2007 Baron Advanced Meteorological Systems, LLC.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  program body starts at line  67
C
C  DESCRIPTION:
C	interactively month, day, year;
C	get julian date YYYYDD back.
C
C  PRECONDITIONS REQUIRED:
C       none
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C	JULIAN, GETNUM
C
C  REVISION  HISTORY:
C	Prototype  8/95 by CJC
C       Version   11/2001 by CJC for I/O API Version 2.1
C***********************************************************************

      IMPLICIT NONE

C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER		IARGC
        INTEGER		GETDATE
        INTEGER		GETNUM
        INTEGER		JULIAN
        INTEGER		STR2INT

        EXTERNAL	GETDATE, GETNUM, JULIAN, STR2INT

C...........   PARAMETERS and their descriptions:

        CHARACTER*80    PROGVER
        DATA PROGVER /
     &'$Id:: datshift.f 49 2007-07-06 16:20:50Z coats@borel          $'
     &  /

C...........   LOCAL VARIABLES and their descriptions:

        INTEGER         ARGCNT  !  number of command-line args, from IARGC()
        INTEGER		DAY
        INTEGER		EGDATE  !  output gregorian date
        INTEGER		MON
        INTEGER		JDATE   !  internal julian date
        INTEGER		JTIME   !  internal julian time
        INTEGER		SGDATE  !  input gregorian date
        INTEGER		TSTEP   !  time step for changing dates
        INTEGER		YR
        CHARACTER*80    DATBUF
        CHARACTER*80    STEPBUF


C***********************************************************************
C   begin body of program DATSHIFT

        ARGCNT = IARGC()
        IF ( ARGCNT .EQ. 2 ) THEN

            CALL GETARG( 1, DATBUF )
            CALL UPCASE( DATBUF )
            SGDATE = STR2INT( DATBUF )

            CALL GETARG( 2, STEPBUF )
            TSTEP = STR2INT( STEPBUF )

            READ( DATBUF, * ) SGDATE

        END IF

        IF ( ARGCNT .NE.        2 .OR.
     &       SGDATE .GT. 99999999 .OR.
     &       SGDATE .LT.        1      ) THEN

            WRITE( *,92000 ) ' ', ' ',
     & 'Program DATSHIFT takes calendar date (in form YYYYMMDD)',
     & 'and a number of days as an increment and returns the date',
     & 'in Gregorian-date form "YYYYMMDD".',
     & ' ',
     & '    Usage:  "datshift [<YYYYMMDD> <+/- days>]" ', ' ',
     & '(if the command-line arguments are missing, prompts the ',
     & 'user for them)',
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

            SGDATE = GETDATE( 19950701,
     &                       'Enter date (YYYYMMDD) or (YYYYDDD)' )

            TSTEP  = GETNUM( -9000, 9000, 1,
     &                       'Enter days increment' )

            WRITE( DATBUF, 92010 ) SGDATE

        END IF	!  if argcnt=2, or not

C.........  Convert date formats

        IF( SGDATE .GT. 9999366 ) THEN
            YR  = STR2INT( DATBUF( 1:4 ) )
            MON = STR2INT( DATBUF( 5:6 ) )
            DAY = STR2INT( DATBUF( 7:8 ) )

            JDATE = 1000 * YR  +  JULIAN( YR, MON, DAY )
            JTIME = 120000

        ELSE
            JDATE = SGDATE
            JTIME = 120000

        ENDIF

        TSTEP = TSTEP * 240000

        CALL NEXTIME( JDATE, JTIME, TSTEP )

        YR = JDATE / 1000

        CALL DAYMON( JDATE, MON, DAY )

        EGDATE = YR * 10000 + MON*100 + DAY

        WRITE( *,92010 ) EGDATE

      CALL EXIT( EGDATE )

C******************  FORMAT  STATEMENTS   ******************************

C...........   Informational (LOG) message formats... 92xxx

92000	FORMAT( 5X, A )

92010	FORMAT( I8.8 )


        END

