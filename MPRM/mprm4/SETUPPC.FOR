      SUBROUTINE FLOPEN( DEVNUM,DEVNAM,KOUNT,CARD,TYPE,FRM,ISTAT )
C=======================================================================
C     PURPOSE:  To open a disk file.
C
C-----------------------------------------------------------------------
C     Variable declarations
C
      LOGICAL   LVAR
      INTEGER   DEVNUM,ISTAT,ITEST,TYPE,FRM
      CHARACTER DEVNAM*48,CARD*80,CTYPE(4)*8,FTYPE(2)*12
      CHARACTER CVAR1*48
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'
C
      DATA CTYPE/'NEW     ','OLD     ','UNKNOWN ','SCRATCH '/
      DATA FTYPE/'FORMATTED   ','UNFORMATTED '/

C     DEVNUM = DEVICE NUMBER
C     DEVNAM = DEVICE NAME
C     CARD   = CARD 'IMAGE' FILE OPEN DATA
C     CTYPE  = TYPE OF FILE 1 = 'NEW'
C                           2 = 'OLD'
C                           3 = 'UNKNOWN'
C                           4 = 'SCRATCH' (A TEMPORARY FILE)
C     FRM    = FORMAT OF RDISK FILE 1 = 'FORMATTED'
C                                   2 = 'UNFORMATTED'
C     ISTAT  = 1 FAILURE IN ATTEMPT
C              2 OPEN WAS SUCCESSFUL
C     FTYPE  = ALL FILES HAVE A FORM OF FORMATTED EXCEPT THE
C              OUTPUT FILES CREATED BY MERGE AND THE METPROCESSOR.
C
C     Data Initialization

      PATH = PATHWD(IRD1)
      LOC  = 'FLOPEN'
      ISTAT = 0

C     1. Fetch third word on card, should match with 'DISK' or 'USER'

      BUF04(1) = BLNK04
      BUF08(1) = ' FORM  '
      CALL GETWRD( 3,KOUNT,CARD,4,4,1,BUF08(1),BUF04(1),ISTAT )

C     Check for word 'DISK' or 'USER'

      IF( BUF04(1) .NE. FORM(2) .AND. BUF04(1) .NE. FORM(3) ) THEN
         ECODE = 'E06'
         MESS = BLNK40
         WRITE( MESS,1000 ) BUF04(1)
1000     FORMAT(1X,'MUST BE DISK OR USER, ',A4,' WAS GIVEN')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      END IF

C     2. Form does equal 'DISK' OR 'USER', read for filename

      BUF48 = BLNK48
      BUF08(1) = 'FILENAME'
      CALL GETWRD( 4,KOUNT,CARD,2,48,2,BUF08(1),BUF48,ISTAT )
      DEVNAM = BUF48

C     Insure this filename has not been used before

      INQUIRE( FILE=DEVNAM,OPENED=LVAR,NAME=CVAR1 )
      IF( LVAR ) THEN
         MESS = BLNK40
         WRITE( MESS,1500 )
1500     FORMAT(1X,'SAME FILENAME USED IN PREVIOUS OPEN')
         ECODE = 'E07'
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      END IF

C     3.  Attempt to open if the status doesn't indicate errors 

        IF( ISTAT.NE.1 )THEN

           IF( TYPE.EQ.4 ) THEN
              OPEN(UNIT=DEVNUM,IOSTAT=IRD4)
           ELSE
              OPEN(UNIT=DEVNUM, FILE=DEVNAM, STATUS=CTYPE(TYPE),
     1             FORM=FTYPE(FRM), IOSTAT=IRD4)
           ENDIF

C          UNIT            = file device number
C          FILE            = filename (NOTE on VAX this does not
C                            have to be a complete definition of
C                            the filename)
C          STATUS          = in this program we use 'UNKNOWN' and
C                            'SCRATCH'
C          IOSTAT          = error/status of open
C
C          Check open status
C
           IF(IRD4.NE.0) THEN
              MESS = BLNK40
              WRITE( MESS,2000 ) IRD4,DEVNUM
2000          FORMAT(1X,'IOSTAT= ',I8,' OPENING DISK: ',I2)
              ECODE = 'E07'
              CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
              ISTAT = 1

           ELSE
              ISTAT = 2

           ENDIF

        ENDIF

        RETURN
        END
C
C
      SUBROUTINE TPOPEN( DEVNUM,DEVNAM,KOUNT,CARD,DEVFMT,ISTAT )
C=======================================================================
C     PURPOSE:  To open access to a tape drive.
C
C-----------------------------------------------------------------------
C     LOCAL VARIABLES
C
      LOGICAL   LVAR
      INTEGER   DEVNUM,ISTAT,ITEST
      CHARACTER DEVNAM*48,CARD*80,DEVFMT*8
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'
C
C     DEVNUM = DEVICE NUMBER
C     DEVNAM = DEVICE NAME
C     CARD   = CARD 'IMAGE' WITH TAPE OPEN DATA
C     ISTAT  = 1 FAILURE IN ATTEMPT
C              3 OPEN WAS SUCCESSFUL (ASCII DATA)
C              4 OPEN WAS SUCCESSFUL (EBCDIC DATA)
C
C     INITALIZE VALUES
C
      PATH = 'JB'
      LOC  = 'TPOPEN'
      ISTAT = 0
C
C     1. FETCH THIRD WORD ON CARD, SHOULD MATCH WITH 'TAPE'
C
      BUF04(1) = BLNK04
      BUF08(1) = ' FORM  '
      CALL GETWRD( 3,KOUNT,CARD,4,4,1,BUF08(1),BUF04(1),ISTAT )
C
C     CHECK FOR WORD 'TAPE'
C
      IF( BUF04(1) .NE. FORM(1) ) THEN
         ECODE = 'E06'
         MESS = BLNK40
         WRITE( MESS,1000 ) BUF04(1)
1000     FORMAT(1X,'FILE-FORM MUST BE TAPE, ',A4,' WAS GIVEN')
         ISTAT = 1
      END IF
C
C     2. READ FOR TAPENAME
C
      BUF48 = BLNK48
      BUF08(1) = 'TAPENAME'
      CALL GETWRD( 4,KOUNT,CARD,2,48,2,BUF08(1),BUF48,ISTAT )
C
      DEVNAM = BUF48
C
C     INSURE THIS FILENAME HAS NOT BEEN USED BEFORE
C
      INQUIRE( FILE=DEVNAM,OPENED=LVAR )
      IF( LVAR ) THEN
         MESS = BLNK40
         WRITE( MESS,1500 )
1500     FORMAT(1X,'SAME TAPENAME USED IN PREVIOUS OPEN')
         ECODE = 'E07'
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      END IF
C
C     3.  FETCH FORMAT
C
      BUF80(2) = BLNK80
      BUF08(2) = ' DEVFMT '
      CALL GETWRD( 5,KOUNT,CARD,6,7,1,BUF08(2),BUF80(2),ISTAT )
C
      DEVFMT = BUF80(2)

C
C     3.1 MAKE SURE THE FORMAT SPECIFIED IS ALLOWED
C
      IWORK1(1) = 0
      IWORK1(2) = 0
      IWORK1(1) = INDEX(DEVFMT,'FB')
      IWORK1(2) = INDEX(DEVFMT,'VB')
C
      IWORK1(3) = 0
      IWORK1(4) = 0
      IWORK1(5) = 0
C
      IF( IRD1.EQ.2 )THEN
         IWORK1(4) = INDEX(DEVFMT,'9689')
         IWORK1(5) = INDEX(DEVFMT,'5600')
      END IF
C
      IF( IRD1.EQ.3) IWORK1(3) = INDEX(DEVFMT,'CD144')
      IWORK1(10) = 0
      DO 10 I=1,5
         IF( IWORK1(I).GT.0 ) THEN
            IWORK1(10) = IWORK1(10) + 1
         END IF
10    CONTINUE
C
C     IF ALL IS WELL, THE USER HAS SPECIFIED:
C     EITHER 'VB' OR 'FB', AS WELL AS ONE
C     OF THE SET OF VALUES (CD144,9689,5600).
C     HENCE, IWORK1(10) SHOULD EQUAL '2'.
C
      IF( IWORK1(10).NE.2 ) THEN
         ISTAT = 1
         MESS = BLNK40
         WRITE( MESS,2000 )
2000     FORMAT(1X,'NO MATCH WITH TAPE FORMAT ')
         ECODE = 'E06'
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
      END IF
C
C     4.  FETCH TAPE CHARACTER TYPE (EBCDIC/ASCII)
C
      BUF08(1) = BLNK08
      BUF08(2) = 'CHARTYPE'
      CALL GETWRD( 6,KOUNT,CARD,5,6,2,BUF08(2),BUF08(1),ISTAT )
C
      IWORK1(1) = 0
      IWORK1(2) = 0
      IWORK1(1) = INDEX(BUF08(1),'ASCII')
      IWORK1(2) = INDEX(BUF08(1),'EBCDIC')
      IF( IWORK1(1).EQ.0 .AND. IWORK1(2).EQ.0) THEN
         ISTAT = 1
         MESS = BLNK40
         WRITE( MESS,3000 )
3000     FORMAT(1X,'(ASCII/EBCDIC) INCORRECT/MISSING')
         ECODE = 'E06'
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      END IF
C
C     5.  ATTEMPT OPEN TO TAPE UNIT
C
      IF( ISTAT.NE.1 ) THEN
C
C
         OPEN( UNIT=DEVNUM,          FILE=DEVNAM,     IOSTAT=IRD4,
     1         FORM='FORMATTED',     STATUS='OLD' )
C
C        CHECK OPEN STATUS
C
         IF( IRD4.NE.0 )THEN
            MESS = BLNK40
            WRITE( MESS,4000 ) IRD4,DEVNUM
4000        FORMAT(1X,'IOSTAT= ',I8,' ERROR OPENING TAPE: ',I2)
            ECODE = 'E07'
            CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         END IF
C
      END IF
C
      IF( ISTAT.NE.1 ) THEN
C
C        OPEN WAS SUCCESSFUL
C
         IRD4 = INDEX(BUF08(1),'ASCII')
         IF( IRD4.EQ.0 ) THEN
C           MUST BE EBCDIC TAPEFORM
            ISTAT = 4
         ELSE
            ISTAT = 3
         END IF
C
      END IF
C
      RETURN
      END
C
      SUBROUTINE LWRUPR(BUFR)
C***********************************************************************
C                 LWRUPR Module of MPRM
C
C        PURPOSE: Transfer All Characters From Lower Case To
C                 Upper Case (Using INDEX Intrinsic Function)
C
C                 NOTE: This routine replaces one in the original MPRM
C                       of the same name to remove system-dependent
C                       character positions
C
C        PROGRAMMER: Roger Brode, Kevin Stroupe for the ISC2 model
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Card Image (80 Character Array)
C                 Number of Characters in String (assumed to be 80)
C
C        OUTPUTS: Input Runstream Card Image (Array) in Uppercase
C
C        CALLED FROM:   SETUP
C-----------------------------------------------------------------------
C
C     Variable Declarations

      CHARACTER*26  UPCASE,LWCASE
      CHARACTER*1   BUFR(80)

C     Variable Initializations
      DATA UPCASE/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      DATA LWCASE/'abcdefghijklmnopqrstuvwxyz'/

      DO 20 I = 1, 80
         IF (BUFR(I) .NE. ' ') THEN
            INDCHK = INDEX(LWCASE,BUFR(I))
            IF (INDCHK .NE. 0) THEN
               BUFR(I) = UPCASE(INDCHK:INDCHK)
            END IF
         END IF
 20   CONTINUE

      RETURN
      END


C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C  SUBROUTINE CHAR3
C
C  PURPOSE   THIS ROUTINE DETERMINES IF THE NEXT LETTER IN THE ASCII
C            POSITIONAL CODES IS AN UPPER CASE LETTER
C            FOR ASCII THE ALLOWABLE  POSITIONAL (NUMERIC) CODES ARE
C            65 THROUGH 90
C            THIS  SIMPLE ROUTINE IS REQUIRED BECAUSE THE IBM MAINFRAME
C            USES EBCDIC POSITIONAL CODES WHICH ARE NOT CONTIGUOUS AND
C            WE ARE ATTEMPTING TO MINIMIZE CODE CHANGES (SEE SETUPIBM)
C
C  VERSION DATE: 14 JUNE 1988
C
C  CALLED BY:    HDPROC
C
C=======================================================================
C
      SUBROUTINE CHAR3(I)
      INTEGER   I
C
      IF (I .GT. 90) I = 90
C
      RETURN
      END
