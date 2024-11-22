      SUBROUTINE SETUP
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C
C     PURPOSE:  Controls the processing of the user supplied run control
C               records
C
C-----------------------------------------------------------------------

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'
C
C---- 1.  Initialize values
C
      PATH = 'JB'
      LOC  = ' SETUP'
      UASTAT = 0
      SFSTAT = 0
      OSSTAT = 0
      KOUNT = 0
      BUF80(1) = BLNK80
C
C---- 2.  Process setup cards
C
10    KOUNT = KOUNT + 1
      BUF80(1) = BLNK80
      READ( DEVIN,1000,ERR=70,IOSTAT=IRD4,END=80 ) (BUF01(I),I=1,80)
1000  FORMAT(80A1)

C---- 3.  Define beginning and ending column of each       ---- CALL DEFINE
C         field on the record

      CALL DEFINE( KOUNT,80,BUF80(1) )

C---- 4.  Check IRD3 (returned through common from SUBR.DEFINE)
C         IRD3 contains the number of words found.  If it zero,
C         the record is blank.

      IF( IRD3.EQ.0 ) THEN
         GO TO 10
      END IF

C---- 5.  Image is not blank, is this a comment record (asterisk in
C         column 1 or 2)?  If so, read another record

      IF( BUF01(1) .eq. '*'  .OR.  BUF01(2) .eq. '*' )THEN
         ECODE = 'I00'
         WRITE( MESS,1100 )
1100     FORMAT(' COMMENT CARD FOUND, SKIP TO NEXT IMAGE')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         GO TO 10
      ENDIF

C---- 6.  The record is not blank or a comment record,     ---- CALL FDPATH
C         define pathway id

      CALL FDPATH( KOUNT,BUF80(1) )
C
C---- 7.  Check IRD1 (returned through common from SUBR.FDPATH)
C         IRD1 contains the pathway id number; valid values are
C         1,2,3,4, or 5.
C
      IF( IRD1.LE.0 .OR. IRD1.GT.5 ) THEN
         GO TO 10
      END IF

C---- 8.  Found valid pathway, check for valid keyword     ---- CALL FDKEY

      CALL FDKEY( KOUNT,BUF80(1) )

C---- 9.  Check IRD2 (returned through common from SUBR.FDKEY)
C         IRD2 contains the keywrd value found.  If no valid
C         match was found, IRD2 will equal 0.

      IF( IRD2.EQ.0 ) THEN
         GO TO 10
      END IF

C---- 10.  Write 'card' image to temporary file for        ---- CALL WRTCRD
C          possible later use.

      BUF03 = '   '
      CALL WRTCRD( KOUNT,BUF03,BUF80(1) )
C
C---- 11. Check for 'STA'rting keyword for this pathway
C
      IF( IRD2.EQ.1 ) THEN
C
         IF( STATUS(IRD1,IRD2).NE.0 ) THEN
C           This card has been seen before, something is amiss
            STATUS(IRD1,IRD2) = 1
            ECODE = 'E01'
            WRITE( MESS,2000 ) PATHWD(IRD1)
2000        FORMAT(1X,'DUPLICATE STA CARD FOR PATHWAY-',A2)
            CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
C
         ELSE
            STATUS(IRD1,IRD2) = 2

         END IF
C
         GO TO 10
      END IF

C---- 12.  Check to see if this is a 'FIN'ished card

      IF( IRD2.EQ.2 ) THEN
C        Has this keyword been seen before?

         IF( STATUS(IRD1,IRD2).NE.0 ) THEN
C           Yes, write error message
            STATUS(IRD1,IRD2) = 1
            ECODE = 'E01'
            MESS = BLNK40
            WRITE( MESS,3000 ) PATHWD(IRD1)
3000        FORMAT(1X,'DUPLICATE FIN CARD FOR PATHWAY-',A2)
            CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )

         ELSE
C           Process previously written header records, as
C           appropriate, for this pathway.
            STATUS(IRD1,IRD2) = 2
            CALL HDPROC( KOUNT )

         END IF

         GO TO 10

      END IF
C
C---- 13.  Is this record the 'END OF JOB/RUN CARD'?
C
      IF( IRD1.EQ.1 .AND. IRD2.EQ.6 )THEN
         STATUS(IRD1,IRD2) = 2
         GO TO 90
      END IF

C---- 14.  Call appropriate routine to decipher this record for the
C          pathway.

C   GO TO(20,30,40,50,60),IRD1    original structure removed: JOP
      IF( IRD1 .EQ. 1 )THEN
C        JB image read                                     ---- CALL JBCARD
         CALL JBCARD( KOUNT,BUF80(1) )

      ELSEIF( IRD1 .EQ. 2 )THEN
C        UA image read                                     ---- CALL UACARD
         CALL UACARD( KOUNT,BUF80(1) )

      ELSEIF( IRD1 .EQ. 3 )THEN
C        SF image read                                     ---- CALL SFCARD
         CALL SFCARD( KOUNT,BUF80(1) )

      ELSEIF( IRD1 .EQ. 4 )THEN
C        OS image read                                     ---- CALL OSCARD
         CALL OSCARD( KOUNT,BUF80(1) )

      ELSEIF( IRD1 .EQ. 5 )THEN
C        MR image read                                     ---- CALL MRCARD
         CALL MRCARD( KOUNT,BUF80(1) )

      ENDIF

C     Process the next record
      GO TO 10

70    CONTINUE

C     We get here because a read error in fetching the next record
C     into BUF80(1) was encountered.
      ECODE = 'E02'
      WRITE( MESS,4000 ) IRD4
4000  FORMAT(1X,'IOSTAT= ',I8,' READING INPUT CARD' )
      CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
      GO TO 10

80    CONTINUE

C     An end-of-file was encountered reading the control records
      ECODE = 'I19'
      MESS = BLNK40
      WRITE( MESS,5000 ) DEVIN
5000  FORMAT(1X,'FOUND "END OF FILE" ON DEVICE DEVIN ',I2)
      CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
C
      GO TO 100

90    CONTINUE

C     The 'END OF JOB/RUN CARD' was encountered
      ECODE = 'I19'
      MESS = BLNK40
      WRITE( MESS,6000 )
6000  FORMAT(1X,'ENCOUNTERED END OF "JOB/RUN CARD" ')
      CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )


C---- 15.  Now test the status array for completeness      ---- CALL TEST
C
100   CALL TEST
C
C---- 16.  Construct and write header records to           ---- CALL HEADER
C          appropriate output files.  (NOTE: only if
C          call to 'TEST' shows no errors detected on
C          all (REPEAT: ALL) pathways, will
C          header(s) be constructed and written)
C
      CALL HEADER
C
      RETURN
      END
C
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
      SUBROUTINE JBCARD( KOUNT,CARD )
C
C        PURPOSE:        THIS ROUTINE PROCESSES THE
C                        CARD IMAGES AVIALABLE FOR
C                        PATHWAY JB.
C
C     THESE ARE THE AVAILABLE KEYWORDS:
C
C     VALUE    KEYWRD   ACTION
C
C        3       OUT        ASSIGN DISK FILE DEV50
C        4       RUN        STOP AFTER SETUP PROCESS
C        5       ERR        ASSIGN DISK FILE DEV60
C
C
C        LOCAL VARIABLE
C
      INTEGER WIDTH,ISTAT
      CHARACTER CARD*80,NAME*48
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'
C
C        WIDTH IS THE COMPUTED LENGTH OF A WORD WITHIN THE
C        'CARD' IMAGE.  NAME IS A TEMPORARY STORAGE LOCATION
C        FOR FILE/TAPE-NAMES TO BE OPENED.  ISTAT IS A
C        TEMPORARY LOCATION TO REPORT THE STATUS OF AN ATTEMPT
C        TO OPEN A FILE/TAPE.
C
C        INITIALIZE
C
      PATH = 'JB'
      LOC  = 'JBCARD'
C
C        INITIAL CHECK ON KEYWRD VALUE JUST TO INSURE
C        WE DO NOT HAVE A PROGRAM LOGIC ERROR
C
      IF(IRD2.LE.2 .OR. IRD2.GT.6 ) THEN
C
C        WE HAVE A PROGRAM LOGIC ERROR, STOP PROCESSING
C
         ECODE = '***'
         MESS = BLNK40
         WRITE( MESS,900 ) IRD1,IRD2
 900     FORMAT(1X,'LOGIC ERROR, IRD1 & IRD2 ARE: ',2I3)
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         STOP
      END IF
C
C        1.  CHECK STATUS TO SEE IF WE HAVE SEEN
C           THIS KEYWORD BEFORE.
C
      IF( STATUS(IRD1,IRD2).NE.0 ) THEN
C        ERROR, WE HAVE SEEN THIS KEYWORD BEFORE
         ECODE = 'E01'
         MESS = BLNK40
         WRITE( MESS,1000 ) KEYWRD(IRD1,IRD2)
1000     FORMAT(1X,'REDUNDANT SPECIFICATION OF KEYWRD ',A3)
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         STATUS(IRD1,IRD2) = 1
      END IF
C
C        2.  DECIPHER CARD.
C
      GO TO(10,20,30),IRD2-2
C
10    CONTINUE
C        OUT CARD - ASSIGN GENERAL REPORT FILE
C
      NAME = BLNK48
      ISTAT = 0
      CALL FLOPEN( DEV50,NAME,KOUNT,CARD,3,1,ISTAT )
      IF( STATUS(IRD1,IRD2).EQ.1 ) THEN
         CONTINUE
      ELSE
         DISK50 = NAME
         STATUS(IRD1,IRD2) = ISTAT
      END IF
      RETURN
C
20    CONTINUE
C        RUN CARD - STOP AFTER PROCESSING SETUP CARDS
C
      IF( STATUS(IRD1,IRD2).EQ.1 ) THEN
         CONTINUE
      ELSE
         STATUS(IRD1,IRD2) = 2
      END IF
      RETURN
C
30    CONTINUE
C        ERR CARD - ASSIGN ERROR REPORT FILE
C
      NAME = BLNK48
      ISTAT = 0
      CALL FLOPEN( DEV60,NAME,KOUNT,CARD,3,1,ISTAT )
      IF( STATUS(IRD1,IRD2).EQ.1 ) THEN
         CONTINUE
      ELSE
         DISK60 = NAME
         STATUS(IRD1,IRD2) = ISTAT
      END IF
      RETURN
C
      END
C
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
      SUBROUTINE UACARD( KOUNT,CARD )
C
C        PURPOSE:        THIS ROUTINE PROCESSES THE
C                        CARD IMAGES AVIALABLE FOR
C                        PATHWAY UA.
C
C     THESE ARE THE AVAILABLE KEYWORDS:
C
C     VALUE    KEYWRD   ACTION
C
C        3       EXT        DEFINE EXTRACT INFORMATION
C        4       IQA        ASSIGN DISK FILE DEV12
C        5       OQA        ASSIGN DISK FILE DEV13
C        6       LOC        DEFINE LOCATION INFORMATION
C        7       TOP        OVERRIDE DEFAULT CLIPPING HEIGHT
C        8       IN1        ASSIGN TAPE FOR UA SOUNDING DATA
C        9       IN2        ASSIGN DISK/TAPE FOR ZI DATA
C       10       OFF        TURN OFF AUTOMATIC UA PROCESSING
C       11       CHK        OVERRIDE A DEFAULT QA LIMIT CHECK
C       12       AUD        ADD VARIABLES TO AUDIT LIST
C       13       TRA        TURN ON TRACE NOTES FOR VARIABLE
C
C
C
C        LOCAL VARIABLE
C
      INTEGER WIDTH,ISTAT
      CHARACTER CARD*80,NAME*48
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'UA1.INC'
      INCLUDE 'UA2.INC'
      INCLUDE 'WORK1.INC'
C
C        WIDTH IS THE COMPUTED LENGTH OF A WORD WITHIN THE
C        'CARD' IMAGE.  NAME IS A TEMPORARY STORAGE LOCATION
C        FOR FILE/TAPE-NAMES TO BE OPENED.  ISTAT IS A
C        TEMPORARY LOCATION TO REPORT THE STATUS OF AN ATTEMPT
C        TO OPEN A FILE/TAPE.
C
C        INITIALIZE
C
      PATH = 'UA'
      LOC  = 'UACARD'
C
C        INITIAL CHECK ON KEYWRD VALUE JUST TO INSURE
C        WE DO NOT HAVE A PROGRAM LOGIC ERROR
C
      IF(IRD2.LE.2 .OR. IRD2.GT.NUMKEY(IRD1) ) THEN
C
C        WE HAVE A PROGRAM LOGIC ERROR, STOP PROCESSING
C
         ECODE = '***'
         MESS = BLNK40
         WRITE( MESS,900 ) IRD1,IRD2
 900     FORMAT(1X,'LOGIC ERROR, IRD1 & IRD2 ARE: ',2I3)
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         STOP
      END IF
C
C        1.  CHECK STATUS TO SEE IF WE HAVE SEEN
C           THIS KEYWORD BEFORE.  NOTE, WE ALLOW DUPLICATE
C            'CHK','AUD', AND 'TRA' CARDS TO PASS.
C
      IF( IRD2.GE.11 .AND. IRD2.LE.13) THEN
C
         CONTINUE
C
      ELSE
C
         IF( STATUS(IRD1,IRD2).NE.0 ) THEN
C        ERROR, WE HAVE SEEN THIS KEYWORD BEFORE
            ECODE = 'E01'
            MESS = BLNK40
            WRITE( MESS,1000 ) KEYWRD(IRD1,IRD2)
1000        FORMAT(1X,'REDUNDANT SPECIFICATION OF KEYWRD ',A3)
            CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
            STATUS(IRD1,IRD2) = 1
         END IF
C
      END IF
C
C        2.  DECIPHER CARD.
C
      GO TO(10,20,30,40,50,60,70,80,90,100,110),IRD2-2
C
10    CONTINUE
C        EXT CARD - DEFINE EXTRACT INFORMATION
C
      ISTAT = 0
C
      CALL EXCARD( KOUNT,CARD,UAYR1,UAGMO1,UAGDY1,
     1 UAYR2,UAGMO2,UAGDY2,UADAY1,UADAY2,ISTAT )
C
      IF( STATUS(IRD1,IRD2).EQ.1 ) THEN
         CONTINUE
      ELSE
         STATUS(IRD1,IRD2) = ISTAT
      END IF
      RETURN
C
20    CONTINUE
C        IQA CARD - ATTEMPT TO ASSIGN INPUT DISK FILE FOR QA
C
      ISTAT = 0
      CALL FLOPEN( DEV12,DISK12,KOUNT,CARD,3,1,ISTAT )
      IF( STATUS(IRD1,IRD2) .EQ. 1 ) THEN
         CONTINUE
      ELSE
         STATUS(IRD1,IRD2) = ISTAT
      END IF
      RETURN
C
30    CONTINUE
C        OQA CARD - ATTEMPT TO ASSIGN OUTPUT DISK FILE FOR QA
C
      ISTAT = 0
      CALL FLOPEN( DEV13,DISK13,KOUNT,CARD,3,1,ISTAT )
      IF( STATUS(IRD1,IRD2).EQ.1 ) THEN
         CONTINUE
      ELSE
         STATUS(IRD1,IRD2) = ISTAT
      END IF
      RETURN
C
40    CONTINUE
C        LOC CARD - DEFINE LOCATION INFORMATION
C
      UALOC = BLNK08
      UALON = BLNK08
      UALAT = BLNK08
      ISTAT = 0
      CALL LOCCRD( KOUNT,CARD,UALOC,UALAT,UALON,UALST,ISTAT )
C
      IF( STATUS(IRD1,IRD2).EQ.1 ) THEN
         CONTINUE
      ELSE
         STATUS(IRD1,IRD2) = ISTAT
      END IF
      RETURN
C
50    CONTINUE
C        TOP CARD - REDEFINE CLIPPING HEIGHT
C
      BUF04(1) = BLNK04
      BUF08(1) = 'CLIP HGT'
      CALL GETWRD( 3,KOUNT,CARD,4,4,1,BUF08(1),BUF04(1),ISTAT )
      IF( ISTAT.EQ.2 ) THEN
         READ( BUF04(1),3000,IOSTAT=IRD4) UATOP
3000     FORMAT(I4)
         IF( IRD4.NE.0 ) THEN
            MESS = BLNK40
            WRITE( MESS,4000 ) IRD4
4000        FORMAT(1X,'IOSTAT= ',I8,' READING UATOP')
            ECODE = 'E03'
            CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         ELSE IF( UATOP.LE.0 .OR. UATOP.GE.9999 ) THEN
C
C          SEE IF REASONABLE
C
            MESS = BLNK40
            ECODE = 'E06'
            WRITE( MESS,4500 ) BUF04(1)
4500        FORMAT(1X,A4,' IS NOT REASONABLE FOR UATOP')
            CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         ELSE
            ISTAT = 2
         END IF
C
      END IF
C
      IF( STATUS(IRD1,IRD2).EQ.1 ) THEN
         CONTINUE
      ELSE
         STATUS(IRD1,IRD2) = ISTAT
      END IF
C
      RETURN
C
60    CONTINUE
C        IN1 CARD - Assign tape for UA sounding data
C
      UAFMT  = BLNK08
      UALOC1 = BLNK08
      CALL IN1CRD( KOUNT,CARD,UAFMT,UALOC1,ISTAT )
      IF( STATUS(IRD1,IRD2).EQ.1 ) THEN
         CONTINUE
      ELSE
         STATUS(IRD1,IRD2) = ISTAT
      END IF
      RETURN
C
70    CONTINUE
C        IN2 CARD - ATTEMPT TO OPEN DISK OR TAPE FOR ZI DATA
C
      CALL IN2CRD( KOUNT,CARD,ZIFRMT,ZILOC1,ISTAT )
      IF( STATUS(IRD1,IRD2).EQ.1 ) THEN
         CONTINUE
      ELSE
         STATUS(IRD1,IRD2) = ISTAT
      END IF
C
      RETURN
C
80    CONTINUE
C        OFF CARD - TURN OFF AUTOMATIC PROCESSING ON UA PATHWAY
C
      IF( STATUS(IRD1,IRD2).EQ.1 ) THEN
         CONTINUE
      ELSE
         STATUS(IRD1,IRD2) = 2
      END IF
      RETURN
C
90    CONTINUE
C         CHK CARD - ALTER DEFAULT RANGE CHECKS FOR UA VARIABLE
C
      CALL UACHK( KOUNT,CARD,ISTAT )
      IF( STATUS(IRD1,IRD2).EQ.1 ) THEN
         CONTINUE
      ELSE
         STATUS(IRD1,IRD2) = ISTAT
      END IF
      RETURN
C
100   CONTINUE
C        AUD CARD - ADD AUDIT VARIABLES FOR THIS PATHWAY
C
      CALL UAAUT( KOUNT,CARD,ISTAT )
      IF( STATUS(IRD1,IRD2).EQ.1 ) THEN
         CONTINUE
      ELSE
         STATUS(IRD1,IRD2) = ISTAT
      END IF
      RETURN
C
110   CONTINUE
C        TRA CARD - ADD TRACE NOTES FOR VARIABLES ON THIS PATHWAY
C
      CALL UATRA( KOUNT,CARD,ISTAT )
      IF( STATUS(IRD1,IRD2).EQ.1 ) THEN
         CONTINUE
      ELSE
         STATUS(IRD1,IRD2) = ISTAT
      END IF
      RETURN
C
      END
C
      SUBROUTINE SFCARD( KOUNT,CARD )
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C
C   PURPOSE:  Processes the records for the SF pathway
C
C   The available keywords are:
C
C     VALUE    KEYWRD   ACTION
C
C        3       EXT        Define extract information (dates, etc)
C        4       IQA        Assign disk file DEV12
C        5       OQA        Assign disk file DEV13
C        6       LOC        Define station location information
C        7       IN2        Controls CD-144 and SCRAM extraction
C        8       CHK        Override a default QA limit check
C        9       AUD        Add audit variables to this pathway
C       10       TRA        Turn on trace notes for variable
C       11       IN1        Controls TD-3280 extraction
C       12       IN3        Controls TD-3240 extraction
C       13       IN4        Controls SAMSON extraction
C
C-----------------------------------------------------------------------
C     Variable declarations
C
      INTEGER WIDTH,ISTAT
      CHARACTER CARD*80,NAME*48
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'WORK1.INC'
C
C     WIDTH is the computed length of a word within the
C     'card' image.  NAME is a temporary storage location
C     for file/tape-names to be opened.  ISTAT is a
C     temporary location to report the status of an attempt
C     to open a file/tape.
C
C     Initialize data

      PATH = 'SF'
      LOC  = 'SFCARD'

C     Initial check on KEYWRD value just to insure there isn't a
C     program logic error

      IF(IRD2.LE.2 .OR. IRD2.GT.NUMKEY(IRD1) ) THEN
C        There is a program logic error, stop processing
         ECODE = '***'
         MESS = BLNK40
         WRITE( MESS,900 ) IRD1,IRD2
 900     FORMAT(1X,'LOGIC ERROR, IRD1 & IRD2 ARE: ',2I3)
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         STOP
      END IF
C
C---- 1.  Check status to see if this keyword has been seen before.
C         NOTE: duplicate 'CHK', 'AUD' and 'TRA' records are allowed.
C
      IF( IRD2.LE.7 .OR. IRD2.GE.11 ) THEN
         IF( STATUS(IRD1,IRD2).NE.0 ) THEN
C           Error, this keyword has been seen before
            ECODE = 'E01'
            MESS = BLNK40
            WRITE( MESS,1000 ) KEYWRD(IRD1,IRD2)
1000        FORMAT(1X,'REDUNDANT SPECIFICATION OF KEYWRD ',A3)
            CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
            STATUS(IRD1,IRD2) = 1
         END IF
      END IF
C
C---- 2.  Decipher the record
C
C  GO TO(10,20,30,40,50,60,70,80),IRD2-2  original structure removed: JOP

      IF( (IRD2-2)  .EQ.  1 )THEN
C        EXT record - define extract information           ---- CALL EXCARD
         ISTAT = 0
         CALL EXCARD( KOUNT,CARD,SFYR1,SFGMO1,SFGDY1,
     1                SFYR2,SFGMO2,SFGDY2,SFDAY1,SFDAY2,ISTAT )
         IF( STATUS(IRD1,IRD2).NE.1 ) THEN
            STATUS(IRD1,IRD2) = ISTAT
         END IF

      ELSEIF( (IRD2-2)  .EQ.  2 )THEN
C        IQA record - assign input disk file for QA        ---- CALL FLOPEN
         ISTAT = 0
         CALL FLOPEN( DEV21,DISK21,KOUNT,CARD,3,1,ISTAT )
         IF( STATUS(IRD1,IRD2).NE.1 ) THEN
            STATUS(IRD1,IRD2) = ISTAT
         END IF

      ELSEIF( (IRD2-2)  .EQ.  3 )THEN
C        OQA record - assign output disk file for QA       ---- CALL FLOPEN
         ISTAT = 0
         CALL FLOPEN( DEV22,DISK22,KOUNT,CARD,3,1,ISTAT )
         IF( STATUS(IRD1,IRD2).NE.1 ) THEN
            STATUS(IRD1,IRD2) = ISTAT
         END IF

      ELSEIF( (IRD2-2)  .EQ.  4 )THEN
C        LOC record - define location information          ---- CALL LOCCRD
         SFLOC = BLNK08
         SFLON = BLNK08
         SFLAT = BLNK08
         ISTAT = 0
         CALL LOCCRD( KOUNT,CARD,SFLOC,SFLAT,SFLON,SFLST,ISTAT )
         IF( STATUS(IRD1,IRD2) .NE. 1 ) THEN
            STATUS(IRD1,IRD2) = ISTAT
         END IF

      ELSEIF( (IRD2-2)  .EQ.  5 )THEN
C        IN2 record - open disk or tape for SF data        ---- CALL IN2CRD
         CALL IN2CRD( KOUNT,CARD,SFFMT,SFLOC1,ISTAT )
         IF( STATUS(IRD1,IRD2).NE.1 ) THEN
            STATUS(IRD1,IRD2) = ISTAT
         END IF

      ELSEIF( (IRD2-2)  .EQ.  6 )THEN
C        CHK record - alter default QA parameters          ---- CALL SFCHK
         CALL SFCHK( KOUNT,CARD,ISTAT )
         IF( STATUS(IRD1,IRD2).NE.1 ) THEN
            STATUS(IRD1,IRD2) = ISTAT
         END IF

      ELSEIF( (IRD2-2)  .EQ.  7 )THEN
C        AUD record - add audit variables for this pathway ---- CALL SFAUT
         CALL SFAUT( KOUNT,CARD,ISTAT )
         IF( STATUS(IRD1,IRD2).NE.1 ) THEN
            STATUS(IRD1,IRD2) = ISTAT
         END IF

      ELSEIF( (IRD2-2)  .EQ.  8 )THEN
C        TRA record - add trace notes for variable         ---- CALL SFTRA
         CALL SFTRA( KOUNT,CARD,ISTAT )
         IF( STATUS(IRD1,IRD2).NE.1 ) THEN
            STATUS(IRD1,IRD2) = ISTAT
         END IF

      ELSEIF( (IRD2-2)  .EQ.  9 )THEN
C        IN1 record - process TD-3280 formatted data       ---- CALL IN1CRD
         CALL IN1CRD( KOUNT,CARD,SFFMT,SFLOC1,ISTAT )
         IF( STATUS(IRD1,IRD2).NE.1 ) THEN
            STATUS(IRD1,IRD2) = ISTAT
         END IF

      ELSEIF( (IRD2-2)  .EQ.  10 )THEN
C        IN3 record - process TD-3240 formatted data       ---- CALL IN3CRD
         CALL IN3CRD( KOUNT,CARD,PPTFMT,PPTLOC,ISTAT )
         IF(ISTAT .EQ. 2 )THEN
            READ( PPTLOC,'(I8)' ) IDPPT
         ENDIF
         IF( STATUS(IRD1,IRD2).NE.1 ) THEN
            STATUS(IRD1,IRD2) = ISTAT
         END IF

      ENDIF
C
      RETURN
      END


      SUBROUTINE SFCHK( KOUNT,CARD,ISTAT )
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C
C        PURPOSE:        THIS ROUTINE PROCESSES THE SF PATHWAY
C                        REDEFINITIONS OF QA RANGE CHECK BOUNDS.
C
C
C
C        LOCAL VARIABLES
C
      CHARACTER CARD*80, NAME*4
      INTEGER  ISTAT,KEY,MISS,LBOUND,UBOUND,ITEST
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'WORK1.INC'
C
C        ISTAT              PROCESS STATUS 1 = ERROR IN PROCESSING
C                                          2 = PROCESSING OK
C        CARD               'IMAGE' WITH NEW QA RANGE CHECK DATA
C        NAME               VNAME ON IMAGE
C        KEY                TYPE OF RANGE CHECK
C        MISS               NEW MISSING VALUE
C        LBOUND             NEW LOWER BOUND
C        UBOUND             NEW UPPER BOUND
C
C        INITIALIZE VALUES
C
      PATH = 'SF'
      LOC  = ' SFCHK'
      ISTAT = 0
C
C        1.  CHECK IRD3.
C
C        CHECK IRD3 (PASSED THROUGH WORK COMMON) AS IT TELLS US
C        THE NUMBER OF 'WORDS' WITHIN THE IMAGE 'CARD'.
C        IN ORDER FOR US TO HAVE A COMPLETE SET OF QA RANGE CHECK
C        INFORMATION, IRD3 SHOULD EQUAL 7.  LESS THAN 7 MEANS
C        TOO LITTLE INFORMATION HAS BEEN GIVEN; MORE THAN 7
C        MEANS TOO MUCH INFROMATION HAS BEEN GIVEN.
C       IN EITHER CASE, WE CALL IT AN ERROR CONDITION.
C
      IF( IRD3.NE.7 ) THEN
         ECODE = 'E04'
         MESS = BLNK40
         IF( IRD3.LT.7 ) THEN
            WRITE( MESS,1000 )
1000        FORMAT(1X,'INCOMPLETE INFORMATION ON CHK CARD')
         ELSE
            WRITE( MESS,2000 )
2000        FORMAT(1X,'SUPERFLUOUS DATA ON CHK CARD')
         END IF
C
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      END IF
C
      CALL CHKCRD( KOUNT,CARD,NAME,KEY,MISS,LBOUND,UBOUND,ITEST )
      IF( ISTAT.EQ.1 ) THEN
         CONTINUE
      ELSE
         ISTAT = ITEST
      END IF
C
C
C        4.  IDENTIFY WHICH VARIABLE HAS NEW QA DATA
C
      IF( NAME.EQ.BLNK04) THEN
         GO TO 15
      END IF
C
      DO 10 I=30,52
         IF( NAME.EQ.VNAMES(I) ) THEN
            GO TO 20
         END IF
10    CONTINUE
C        IF WE GET HERE THERE WAS NO MATCH
15    ECODE = 'E06'
      MESS = BLNK40
      WRITE( MESS,5000 ) NAME
5000  FORMAT(1X,A4,' NO MATCH WITH SF NAMES')
      CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
      ISTAT = 1
C
20    CONTINUE
C
C        5. REDEFINE QA VALUES
C
      IF( ISTAT.EQ.1 ) THEN
         CONTINUE
      ELSE
         SFQA(I,1) = KEY
         SFQA(I,2) = MISS
         SFQA(I,3) = LBOUND
         SFQA(I,4) = UBOUND
C
         ISTAT = 2
      END IF
C
      RETURN
      END
C
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
      SUBROUTINE LOCCRD( KOUNT,CARD,DDLOC,DDLAT,DDLON,DDLST,ISTAT )
C
C        PURPOSE:        THIS ROUTINE PROCESSES THE LOCATION
C                        TYPE CARD IMAGES.
C
C
C
C        LOCAL VARIABLES
C
      CHARACTER CARD*80,DDLOC*8,DDLAT*8,DDLON*8
      INTEGER  ISTAT,DDLST,ITEST
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'
C
C        DDLOC                LOCATION ID (E.G. STATION NUMBER)
C        DDLAT                LATITUDE (E.G. 30.00N)
C        DDLON                LONGITUDE (E.G. 10.00E)
C        DDLST                CONVERSION FACTOR EQUAL TO (GMT-LST)
C        ISTAT                PROCESS STATUS 1 = ERROR IN PROCESSING
C                                            2 = PROCESSING OK
C        CARD                'IMAGE' WITH LOCATION DATA
C
C        INITIALIZE VALUES
C
      PATH = PATHWD(IRD1)
      LOC  = 'LOCCRD'
      ISTAT = 0
C
C        1.  CHECK IRD3.
C
C        CHECK IRD3 (PASSED THROUGH WORK COMMON) AS IT TELLS US
C        THE NUMBER OF 'WORDS' WITHIN THE IMAGE 'CARD'.
C        IN ORDER FOR US TO HAVE A COMPLETE SET OF LOCATION
C        INFORMATION, IRD3 SHOULD EQUAL 6.  LESS THAN 6 MEANS
C        TOO LITTLE INFORMATION HAS BEEN GIVEN; MORE THAN 6
C        MEANS TOO MUCH INFROMATION HAS BEEN GIVEN.
C       IN EITHER CASE, WE CALL IT AN ERROR CONDITION.
C
      IF( IRD3.NE.6 ) THEN
         ECODE = 'E04'
         MESS = BLNK40
         IF( IRD3.LT.6 ) THEN
            WRITE( MESS,1000 )
1000        FORMAT(1X,'INCOMPLETE INFORMATION ON LOC CARD')
         ELSE
            WRITE( MESS,2000 )
2000        FORMAT(1X,'SUPERFLUOUS DATA ON LOC CARD')
         END IF
C
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      END IF
C
C        2.  STORE OFF LOCATION DATA
C
C        FETCH STATION ID
C
      BUF08(1) = '  SITEID'
      BUF08(2) = BLNK08
      CALL GETWRD( 3,KOUNT,CARD,1,8,1,BUF08(1),BUF08(2),ITEST )
      IF( ISTAT.EQ.1 ) THEN
         CONTINUE
      ELSE
         ISTAT = ITEST
      END IF
C
      DDLOC = BUF08(2)
C
C        FETCH FIRST LAT/LONG DATA GROUP
C
  
      BUF08(1) = 'LAT/LONG'
      BUF08(2) = BLNK08
      CALL GETWRD( 4,KOUNT,CARD,3,8,1,BUF08(1),BUF08(2),ITEST )
      IF( ISTAT.EQ.1 ) THEN
         CONTINUE
      ELSE
         ISTAT = ITEST
      END IF
C
C        FETCH SECOND LAT/LONG GROUP
C
      BUF08(1) = 'LAT/LONG'
      BUF08(3) = BLNK08
      CALL GETWRD( 5,KOUNT,CARD,3,8,1,BUF08(1),BUF08(3),ITEST )
      IF( ISTAT.EQ.1 ) THEN
         CONTINUE
      ELSE
         ISTAT = ITEST
      END IF
C
C        FETCH CONVERSION FACTOR (GMT-LST)
C
      BUF08(1) = ' LST/GMT'
      BUF08(4) = BLNK08
      CALL GETWRD( 6,KOUNT,CARD,1,8,1,BUF08(1),BUF08(4),ITEST )
      IF( ISTAT.EQ.1 ) THEN
         CONTINUE
      ELSE
         ISTAT = ITEST
      END IF
C
C        3.  ATTEMPT CONVERSION OF GMT TO LST FACTOR TO INTEGER VALUE
C
      READ( BUF08(4),3000,IOSTAT=IRD4 ) DDLST
3000  FORMAT( I8 )
      IF( IRD4.NE.0 ) THEN
         MESS = BLNK40
         ECODE = 'E03'
         WRITE( MESS,4000 ) IRD4
4000     FORMAT(1X,'IOSTAT = ',I8,' READ ERROR FOR DDLST')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      ELSE
C
C        CHECK DDLST VALUE
C
         IF( DDLST.LT.-12 .OR. DDLST.GT.12 ) THEN
            MESS = BLNK40
            ECODE = 'E06'
            WRITE( MESS,4500 ) DDLST
4500        FORMAT(1X,I6,' DDLST NOT REASONABLE')
            CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         END IF
C
      END IF
C
C        4.  INSURE LAT/LONG DATA ARE REASONABLE.
C           CHECK BUF08(2) AND BUF08(3)
C                (1) ONE SHOULD HAVE EITHER AN 'E' OR AN 'W',
C                    THIS IS A VALID LONGITUDE IF THE
C                    NUMERIC VALUE IS BETWEEN 0 AND 180.
C                (2) THE OTHER SHOULD HAVE AN 'N' OR AN 'S',
C                   THIS IS A VALID LATTIDUE IF THE
C                    NUMERIC VALUE IS BETWEEN 0 AND 90.
C
      IF( BUF08(2)(8:8).EQ.'E' .OR. BUF08(2)(8:8).EQ.'W') THEN
C
C        A LONGITUDE HAS BEEN FOUND IN BUF08(2)
C
         DDLON      = BUF08(2)
         CALL LATLON( KOUNT,2,DDLON,XRD2,ITEST )
         IWORK1(5) = ITEST
C
C        BUF08(3) SHOULD CONTAIN THE LATITUDE
C
         DDLAT      = BUF08(3)
         CALL LATLON( KOUNT,1,DDLAT,XRD3,ITEST )
         IWORK1(6) = ITEST
C
      ELSE IF( BUF08(2)(8:8).EQ.'N' .OR. BUF08(2)(8:8).EQ.'S') THEN
C
C       A LATITUDE HAS BEEN FOUND IN BUF08(2)
C
         DDLAT      = BUF08(2)
         CALL LATLON( KOUNT,1,DDLAT,XRD3,ITEST )
         IWORK1(5) = ITEST
C
C        BUF08(3) SHOULD CONTAIN A LONGITUDE
C
         DDLON      = BUF08(3)
         CALL LATLON( KOUNT,2,DDLON,XRD2,ITEST )
         IWORK1(6) = ITEST
C
      ELSE
C        ERROR CONDITION
C
         MESS = BLNK40
         ECODE = 'E06'
         WRITE( MESS,5000 ) BUF08(2),BUF08(3)
5000     FORMAT(1X,A8,1X,A8,' CHECK BOTH LAT & LONG')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         IWORK1(5) = 1
         IWORK1(6) = 1
      END IF
C
C        5.  CHECK STORED STATUS CHECKS (IWORK1(5) AND IWORK1(6))
C            BOTH SHOULD EQUAL 2, IF ALL CHECKS WERE PASSED.
C
      IF( IWORK1(5).EQ.2 .AND. IWORK1(6).EQ.2 ) THEN
         ISTAT = 2
      ELSE
         ISTAT = 1
      END IF
C
C        6.  CHECK DDLST AGAINST LONGITUDE (XRD2)
C
      IF( ISTAT.EQ.1 ) THEN
         CONTINUE
      ELSE
C
         ISTAT = 2
C
         IF( XRD2.LT.0.0 .AND. DDLST.GT.0 ) THEN
C
            MESS = BLNK40
            ECODE = 'E06'
            WRITE( MESS,6000 )
6000        FORMAT(1X,'GMT TO LST CONVERSION SHOULD BE NEG.')
            CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
C
         ELSE IF( XRD2.GE.0.0 .AND. DDLST.LT.0 ) THEN
C
            MESS = BLNK40
            ECODE = 'E06'
            WRITE( MESS,7000 )
7000        FORMAT(1X,'GMT TO LST CONVERSION SHOULD BE POS.')
            CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         END IF
C
      END IF
C
      RETURN
      END
C
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
      SUBROUTINE SFAUT( KOUNT,CARD,ISTAT )
C
C        PURPOSE:        THIS ROUTINE PROCESSES THE SF PATHWAY
C                        DEFINITIONS OF VARIABLES TO BE
C                        SUMMARIZED IN THE FINAL AUDIT REPORT.
C
C        LOCAL VARIABLES
C
      CHARACTER CARD*80, NAME*8
      INTEGER  ISTAT,ITEST,N
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'WORK1.INC'
C
C        ISTAT              PROCESS STATUS 1 = ERROR IN PROCESSING
C                                          2 = PROCESSING OK
C        CARD               'IMAGE' WITH AUDIT VARIABLES LISTED
C        NAME               VNAME ON IMAGE
C
C        INITIALIZE VALUES
C
      PATH = 'SF'
      LOC  = ' SFAUT'
C
C        1.  CHECK IRD3.
C
C        CHECK IRD3 (PASSED THROUGH WORK COMMON) AS IT TELLS US
C        THE NUMBER OF 'WORDS' WITHIN THE IMAGE 'CARD'.
C        IN ORDER FOR US TO HAVE A COMPLETE SET OF INFORMATION,
C        IRD3 SHOULD .GE. 3.  LESS THAN 3 MEANS
C        TOO LITTLE INFORMATION HAS BEEN GIVEN; MORE THAN 3
C        MEANS MORE THAN ONE VARIABLE NAME IS LISTED.
C
      IF( IRD3.LT.3 ) THEN
         ECODE = 'E04'
         MESS = BLNK40
         WRITE( MESS,1000 )
1000     FORMAT(1X,'INCOMPLETE INFORMATION ON AUD CARD')
C
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      END IF
C
C        2.  IDENTIFY WHICH VARIABLE(S) ARE LISTED
C
      DO 50 N=3,IRD3
C
C       FETCH VARIABLE NAME (NOTE ALL VARIABLE NAMES ARE SUPPOSED
C       TO BE 4-CHARACTERS IN LENGTH).
C
         NAME =  '  VAR-  '
         IRD5 = N-2
         WRITE( NAME(7:8),2000 ) IRD5
2000     FORMAT( I2.0 )
         BUF04(1) = BLNK04
         CALL GETWRD( N,KOUNT,CARD,4,4,1,NAME,BUF04(1),ITEST )
         IF( ITEST.NE.2 ) THEN
            ISTAT = 1
            GO TO 50
         END IF
C
         IF( BUF04(1).EQ.BLNK04) THEN
            GO TO 50
         END IF
C
C       SEARCH FOR MATCH WITHIN SF-PATHWAY VARIABLE LIST
C
         DO 10 I=30,52
            IF( BUF04(1).EQ.VNAMES(I) ) THEN
               GO TO 20
            END IF
10       CONTINUE
C       IF WE GET HERE THERE WAS NO MATCH
15       ECODE = 'E06'
         MESS = BLNK40
         WRITE( MESS,3000 ) BUF04(1)
3000     FORMAT(1X,A4,' NO MATCH WITH SF NAMES')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
C
20       CONTINUE
C
C        5. SET AUDIT FLAG FOR THIS VARIABLE, IF NO ERRORS HAVE OCCURRED
C           IN PROCESSING AUD-CRD IMAGES.
C
         IF( ISTAT.EQ.1 ) THEN
            CONTINUE
         ELSE
            SFSAUD(I-29) = 1
C
            ISTAT = 2
         END IF
C
50    CONTINUE
C
      RETURN
      END
C
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
      SUBROUTINE UAAUT( KOUNT,CARD,ISTAT )
C
C        PURPOSE:        THIS ROUTINE PROCESSES THE UA PATHWAY
C                        DEFINITIONS OF VARIABLES TO BE
C                        SUMMARIZED IN THE FINAL AUDIT REPORT.
C
C        LOCAL VARIABLES
C
      CHARACTER CARD*80, NAME*8
      INTEGER  ISTAT,ITEST,N
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'UA1.INC'
      INCLUDE 'UA2.INC'
      INCLUDE 'WORK1.INC'
C
C        ISTAT              PROCESS STATUS 1 = ERROR IN PROCESSING
C                                          2 = PROCESSING OK
C        CARD               'IMAGE' WITH  AUDIT VARIABLES LISTED
C        NAME               VNAME ON IMAGE
C
C        INITIALIZE VALUES
C
      PATH = 'UA'
      LOC  = ' UAAUT'
C
C        1.  CHECK IRD3.
C
C        CHECK IRD3 (PASSED THROUGH WORK COMMON) AS IT TELLS US
C        THE NUMBER OF 'WORDS' WITHIN THE IMAGE 'CARD'.
C        IN ORDER FOR US TO HAVE A COMPLETE SET OF INFORMATION,
C        IRD3 SHOULD .GE. 3.  LESS THAN 3 MEANS
C        TOO LITTLE INFORMATION HAS BEEN GIVEN; MORE THAN 3
C        MEANS MORE THAN ONE VARIABLE NAME IS LISTED.
C
      IF( IRD3.LT.3 ) THEN
         ECODE = 'E04'
         MESS = BLNK40
         WRITE( MESS,1000 )
1000     FORMAT(1X,'INCOMPLETE INFORMATION ON AUD CARD')
C
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      END IF
C
C        2.  IDENTIFY WHICH VARIABLE(S) ARE LISTED
C
      DO 50 N=3,IRD3
C
C       FETCH VARIABLE NAME (NOTE ALL VARIABLE NAMES ARE SUPPOSED
C       TO BE 4-CHARACTERS IN LENGTH).
C
         NAME =  '  VAR-  '
         IRD5 = N-2
         WRITE( NAME(7:8),2000 ) IRD5
2000     FORMAT( I2.0 )
         BUF04(1) = BLNK04
         CALL GETWRD( N,KOUNT,CARD,4,4,1,NAME,BUF04(1),ITEST )
         IF( ITEST.NE.2 ) THEN
            ISTAT = 1
            GO TO 50
         END IF
C
         IF( BUF04(1).EQ.BLNK04) THEN
            GO TO 50
         END IF
C
C       SEARCH FOR MATCH WITHIN UA-PATHWAY VARIABLE LIST
C
         DO 10 I=1,UAVR
            IF( BUF04(1).EQ.UAVAR(I) ) THEN
               GO TO 20
            END IF
10       CONTINUE
C       IF WE GET HERE THERE WAS NO MATCH
15       ECODE = 'E06'
         MESS = BLNK40
         WRITE( MESS,3000 ) BUF04(1)
3000     FORMAT(1X,A4,' NO MATCH WITH UA NAMES')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
C
20       CONTINUE
C
C        5. SET AUDIT FLAG FOR THIS VARIABLE, IF NO ERRORS HAVE OCCURRED
C           IN PROCESSING AUD-CRD IMAGES.
C
         IF( ISTAT.EQ.1 ) THEN
            CONTINUE
         ELSE
            IF( I.GE.11 ) THEN
               UASAUD(I-10) = 1
            ELSE
               UAVAUD(I) = 1
            END IF
C
            ISTAT = 2
         END IF
C
50    CONTINUE
C
      RETURN
      END
C
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
      SUBROUTINE OSAUT( KOUNT,CARD,ISTAT )
C
C        PURPOSE:        THIS ROUTINE PROCESSES THE OS PATHWAY
C                        DEFINITIONS OF VARIABLES TO BE
C                        SUMMARIZED IN THE FINAL AUDIT REPORT.
C
C        LOCAL VARIABLES
C
      CHARACTER CARD*80, NAME*8
      INTEGER  ISTAT,ITEST
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'WORK1.INC'
C
C        ISTAT              PROCESS STATUS 1 = ERROR IN PROCESSING
C                                          2 = PROCESSING OK
C        CARD               'IMAGE' WITH AUDIT VARIABLES LISTED
C        NAME               VNAME ON IMAGE
C
C        INITIALIZE VALUES
C
      PATH = 'OS'
      LOC  = ' OSAUT'
C
C        1.  CHECK IRD3.
C
C        CHECK IRD3 (PASSED THROUGH WORK COMMON) AS IT TELLS US
C        THE NUMBER OF 'WORDS' WITHIN THE IMAGE 'CARD'.
C        IN ORDER FOR US TO HAVE A COMPLETE SET OF INFORMATION,
C        IRD3 SHOULD .GE. 3.  LESS THAN 3 MEANS
C        TOO LITTLE INFORMATION HAS BEEN GIVEN; MORE THAN 3
C        MEANS MORE THAN ONE VARIABLE NAME IS LISTED.
C
      IF( IRD3.LT.3 ) THEN
         ECODE = 'E04'
         MESS = BLNK40
         WRITE( MESS,1000 )
1000     FORMAT(1X,'INCOMPLETE INFORMATION ON AUD CARD')
C
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      END IF
C
C        2.  IDENTIFY WHICH VARIABLE(S) ARE LISTED
C
      DO 50 N=3,IRD3
C
C       FETCH VARIABLE NAME (NOTE ALL VARIABLE NAMES ARE SUPPOSED
C       TO BE 4-CHARACTERS IN LENGTH).
C
         NAME =  '  VAR-  '
         IRD5 = N-2
         WRITE( NAME(7:8),2000 ) IRD5
2000     FORMAT( I2.0 )
         BUF04(1) = BLNK04
         CALL GETWRD( N,KOUNT,CARD,2,4,2,NAME,BUF04(1),ITEST )
         IF( ITEST.NE.2 ) THEN
            ISTAT = 1
            GO TO 50
         END IF
C
         IF( BUF04(1).EQ.BLNK04) THEN
            GO TO 50
         END IF
C
C       SEARCH FOR MATCH WITHIN OS-PATHWAY VARIABLE LIST
C
         DO 10 I=1,34
            IF( BUF04(1).EQ.VNAMES(I) ) THEN
               GO TO 30
            END IF
10       CONTINUE
C
C       USER MAY HAVE INCLUDED HEIGHT INDICATOR IN THE VECTOR
C       VARIABLE'S NAME.  SEARCH FOR A MATCH OF FIRST TWO LETTERS
C       OF BUF04(1) IN VECTOR VARIABLE NAMES.
C
         BUF04(2) = BLNK04
         BUF04(2)(1:2) = BUF04(1)(1:2)
         DO 20 I=15,29
            IF( BUF04(2).EQ.VNAMES(I) ) THEN
               GO TO 30
            END IF
20       CONTINUE
C
C       IF WE GET HERE THERE WAS NO MATCH
         ECODE = 'E06'
         MESS = BLNK40
         WRITE( MESS,3000 ) BUF04(1)
3000     FORMAT(1X,A4,' NO MATCH WITH OS NAMES')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
C
30       CONTINUE
C
C        5. SET AUDIT FLAG FOR THIS VARIABLE, IF NO ERRORS HAVE OCCURRED
C           IN PROCESSING AUD-CRD IMAGES.
C
         IF( ISTAT.EQ.1 ) THEN
            CONTINUE
         ELSE
            OSSAUD(I) = 1
            ISTAT = 2
         END IF
C
50    CONTINUE
C
      RETURN
      END
C
C
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
      SUBROUTINE GETWRD( NUM,KOUNT,CARD,
     1 LENMIN,LENMAX,JUST,NAME,VALUE,ISTAT )
C
C        PURPOSE:        ROUTINE ATTEMPS TO FETCH THE 'VALUE' (IN
C                        CHARACTERS) OF THE 'NUM'-TH WORD WITHIN
C                        THE IMAGE 'CARD'.
C
C
C        LOCAL VARIABLES
C
      INTEGER   WIDTH,NUM,LENMAX,LENMIN,ISTAT,JUST
      CHARACTER CARD*80,NAME*8
      CHARACTER*(*) VALUE
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'
C
C        NUM    = NUMBER (POSITION) OF WORD WITHIN IMAGE
C        CARD   = THE 80 CHARACTER 'IMAGE'
C        LENMIN = MINIMUM ACCEPTABLE LENGTH OF THE WORD
C        LENMAX = MAXIMIMUM ACCEPTABLE LENGTH OF THE WORD
C        JUST   = HOW TO STORE WITHIN VALUE
C                 IF 1 THEN RIGHT JUSTIFY
C                 IF 2 THEN LEFT  JUSTIFY
C        NAME   = NAME OF WORD WE ARE ATTEMPTING TO FETCH
C                 (USED IN ERROR HANDLING)
C        VALUE  = the NUM-th word, IN ASCII CHARACTERS
C                 (HAS LENGTH AS DEFINED IN THE CALLING PROGRAM)
C                 I HAVE ASSUMED THAT LENMAX IS THE ACTUAL
C                 LENGTH OF VALUE.  IF THIS IS NOT CORRECT, ERRORS
C                 MAY OCCUR.
C        ISTAT  = STATUS OF FETCH, 1 = ERRORS OCCURRED
C                                  2 = FETCH WORKED
C
C        INITIALIZE VALUES
C
      PATH = PATHWD(IRD1)
      LOC  = 'GETWRD'
      ISTAT = 0
C
C        BLANK 'VALUE'
C
      DO 10 I=1,LENMAX
         VALUE(I:I) = ' '
10    CONTINUE
C
C        1. CHECK TO INSURE REQUESTED FIELD EXIST ON IMAGE
C
      IF( IC1(NUM).EQ.0 .AND. IC2(NUM).EQ.0 ) THEN
         ISTAT = 1
         ECODE = 'E00'
         MESS = BLNK40
         WRITE( MESS,1000 ) NAME,NUM
1000     FORMAT(1X,A8,'(FIELD ',I2,' OF IMAGE) IS BLANK')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         RETURN
      END IF
C
C        2.  COMPUTE WIDTH OF NUM-TH WORD
C
      WIDTH = IC2(NUM) - IC1(NUM) + 1
      IF( WIDTH.LT.LENMIN .OR. WIDTH.GT.LENMAX) THEN
         ISTAT = 1
         MESS = BLNK40
         WRITE( MESS,2000 ) LENMIN,LENMAX,WIDTH,NAME
2000     FORMAT(1X,I2,' TO ',I2,' LIMIT; WIDTH=',I2,'; FOR ',A8)
         ECODE = 'E05'
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         RETURN
      END IF
C
C        3.  CHECK VALUES OF LENMIN,LENMAX AND JUST
C
      IF( LENMIN.LE.0 .OR. LENMAX.LE.0 ) THEN
         ISTAT = 1
         MESS = BLNK40
         WRITE( MESS,3000 ) LENMIN,LENMAX
3000     FORMAT(1X,'LENMIN/LENMAX ARE LE. ZERO: ',2I3)
         ECODE = 'E05'
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         RETURN
      END IF
C
      IF( JUST.LE.0 .OR. JUST.GT.2 ) THEN
         ISTAT = 1
         MESS = BLNK40
         WRITE( MESS,4000 ) JUST
4000     FORMAT(1X,' CODING ERROR ON VALUE OF JUST: ',I3)
         ECODE = 'E05'
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         RETURN
      END IF
C
C        4. CHECK JUST AND STORE ACCORDINGLY
C
      IF( JUST.EQ.1 ) THEN
         IRD4 = LENMAX - WIDTH + 1
         VALUE(IRD4:LENMAX) = CARD( IC1(NUM):IC2(NUM) )
C
      ELSE
         VALUE(1:WIDTH) = CARD( IC1(NUM):IC2(NUM) )
      END IF
C
C        ALL SEEMS WELL TO THIS POINT
C
      ISTAT = 2
      RETURN
      END
C
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
      SUBROUTINE UACHK( KOUNT,CARD,ISTAT )
C
C        PURPOSE:        THIS ROUTINE PROCESSES THE UA PATHWAY
C                        REDEFINITIONS OF QA RANGE CHECK BOUNDS.
C
C
C
C        LOCAL VARIABLES
C
      CHARACTER CARD*80, NAME*4
      INTEGER  ISTAT,KEY,MISS,LBOUND,UBOUND,ITEST
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'UA1.INC'
      INCLUDE 'UA2.INC'
      INCLUDE 'WORK1.INC'
C
C        ISTAT           PROCESS STATUS 1 = ERROR IN PROCESSING
C                                       2 = PROCESSING OK
C        CARD            'IMAGE' WITH NEW QA RANGE CHECK DATA
C        NAME            VNAME ON IMAGE
C        KEY             TYPE OF RANGE CHECK
C        MISS            NEW MISSING VALUE
C        LBOUND          NEW LOWER BOUND
C        UBOUND          NEW UPPER BOIUND
C
C        INITIALIZE VALUES
C
      PATH = 'UA'
      LOC  = ' UACHK'
      ISTAT = 0
C
C        1.  CHECK IRD3.
C
C        CHECK IRD3 (PASSED THROUGH WORK COMMON) AS IT TELLS US
C        THE NUMBER OF 'WORDS' WITHIN THE IMAGE 'CARD'.
C        IN ORDER FOR US TO HAVE A COMPLETE SET OF QA RANGE CHECK
C        INFORMATION, IRD3 SHOULD EQUAL 7.  LESS THAN 7 MEANS
C        TOO LITTLE INFORMATION HAS BEEN GIVEN; MORE THAN 7
C        MEANS TOO MUCH INFROMATION HAS BEEN GIVEN.
C        IN EITHER CASE, WE CALL IT AN ERROR CONDITION.
C
      IF( IRD3.NE.7 ) THEN
         ECODE = 'E04'
         MESS = BLNK40
         IF( IRD3.LT.7 ) THEN
            WRITE( MESS,1000 )
1000        FORMAT(1X,'INCOMPLETE INFORMATION ON CHK CARD')
         ELSE
            WRITE( MESS,2000 )
2000        FORMAT(1X,'SUPERFLUOUS DATA ON CHK CARD')
         END IF
C
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      END IF
C
      CALL CHKCRD( KOUNT,CARD,NAME,KEY,MISS,LBOUND,UBOUND,ITEST )
      IF( ISTAT.EQ.1 ) THEN
         CONTINUE
      ELSE
         ISTAT = ITEST
      END IF
C
C
C        4.  IDENTIFY WHICH VARIABLE HAS NEW QA DATA
C
      IF( NAME.EQ.BLNK04) THEN
         GO TO 15
      END IF
C
      DO 10 I=1,12
         IF( NAME.EQ.UAVAR(I) ) THEN
            GO TO 20
         END IF
10    CONTINUE
C        IF WE GET HERE THERE WAS NO MATCH
15    ECODE = 'E06'
      MESS = BLNK40
      WRITE( MESS,5000 ) NAME
5000  FORMAT(1X,A4,' NO MATCH WITH UA NAMES')
      CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
      ISTAT = 1
C
20    CONTINUE
C
C        5. REDEFINE QA VALUES
C
      IF( ISTAT.EQ.1 ) THEN
         CONTINUE
      ELSE
         UAQA(I,1) = KEY
         UAQA(I,2) = MISS
         UAQA(I,3) = LBOUND
         UAQA(I,4) = UBOUND
C
         ISTAT = 2
      END IF
C
      RETURN
      END
C
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
      SUBROUTINE CHKCRD(KOUNT,CARD,NAME,KEY,MISS,LBOUND,UBOUND,ISTAT)
C
C        PURPOSE:        ROUTINE READS IMAGE FOR VARIABLE NAME,
C                        MISSING VALUE, LOWER AND UPPER BOUND.
C                        VALUES ARE PASSED BACK TO CALLING
C                        PROGRAM FOR FURTHER PROCESSING.
C
C
C        LOCAL VARIABLES
C
      CHARACTER NAME*4,CARD*80
      INTEGER   KEY,MISS,LBOUND,UBOUND,ISTAT,ITEST
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'
C
C        NAME       CHAR*4 NAME DECIPHERED FROM IMAGE
C        KEY        TYPE OF RANGE CHECK (1 OR 2)
C        MISS       NEW MISSING VALUE
C        LBOUND     NEW LOWER BOUND FOR RANGE CHECKS
C        UBOUND     NEW UPPER BOUND FOR RANGE CHECKS
C
C        INITIALIZE
C
      PATH = PATHWD(IRD1)
      LOC  = 'CHKCRD'
      ISTAT = 0
C
C        1.  STORE OFF QA RANGE CHECK DATA.
C
      BUF08(1) = '   VNAME'
      BUF08(2) = BLNK08
      CALL GETWRD( 3,KOUNT,CARD,2,8,2,BUF08(1),BUF08(2),ITEST )
      IF( ISTAT.EQ.1 ) THEN
         CONTINUE
      ELSE
         ISTAT = ITEST
      END IF
C
      BUF08(1) = '     KEY'
      BUF08(3) = BLNK08
      CALL GETWRD( 4,KOUNT,CARD,1,8,1,BUF08(1),BUF08(3),ITEST )
      IF( ISTAT.EQ.1 ) THEN
         CONTINUE
      ELSE
         ISTAT = ITEST
      END IF
C
      BUF08(1) = '    MISS'
      BUF08(4) = BLNK08
      CALL GETWRD( 5,KOUNT,CARD,1,8,1,BUF08(1),BUF08(4),ITEST )
      IF( ISTAT.EQ.1 ) THEN
         CONTINUE
      ELSE
         ISTAT = ITEST
      END IF
C
      BUF08(1) = '  LBOUND'
      BUF08(5) = BLNK08
      CALL GETWRD( 6,KOUNT,CARD,1,8,1,BUF08(1),BUF08(5),ITEST )
      IF( ISTAT.EQ.1 ) THEN
         CONTINUE
      ELSE
         ISTAT = ITEST
      END IF
C
      BUF08(1) = '  UBOUND'
      BUF08(6) = BLNK08
      CALL GETWRD( 7,KOUNT,CARD,1,8,1,BUF08(1),BUF08(6),ITEST )
      IF( ISTAT.EQ.1 ) THEN
         CONTINUE
      ELSE
         ISTAT = ITEST
      END IF
  
C
C        2.  STORE OFF NAME, AND ATTEMPT TO CONVERT OTHER
C            VALUES TO INTEGER VALUES.
C
      NAME = BUF08(2)(1:4)
C
      READ( BUF08(3),1000,IOSTAT=IRD4 ) KEY
1000  FORMAT( I8 )
      IF( IRD4.NE.0 ) THEN
         MESS = BLNK40
         ECODE = 'E03'
         WRITE( MESS,2000 ) IRD4,  BUF08(3)
2000     FORMAT(1X,'IOSTAT = ',I8,' READING KEY: ',A8)
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      END IF
C
      IF( KEY.LE.0 .OR. KEY.GT.2 ) THEN
         MESS = BLNK40
         ECODE = 'E06'
         WRITE( MESS,2500 ) BUF08(3)
2500     FORMAT(1X,'KEY-VALUE ',A8,' OUT OF RANGE')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      END IF
C
      READ( BUF08(4),1000,IOSTAT=IRD4 ) MISS
      IF( IRD4.NE.0 ) THEN
         MESS = BLNK40
         ECODE = 'E03'
         WRITE( MESS,3000 ) IRD4,BUF08(4)
3000     FORMAT(1X,'IOSTAT= ',I8,' READING MISS: ',A8)
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      END IF
C
C
      READ( BUF08(5),1000,IOSTAT=IRD4 ) LBOUND
      IF( IRD4.NE.0 ) THEN
         MESS = BLNK40
         ECODE = 'E03'
         WRITE( MESS,4000 ) IRD4,BUF08(5)
4000     FORMAT(1X,'IOSTAT=',I8,' READING BOUND: ',A8)
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      END IF
C
      READ( BUF08(6),1000,IOSTAT=IRD4 ) UBOUND
      IF( IRD4.NE.0 ) THEN
         MESS = BLNK40
         ECODE = 'E03'
         WRITE( MESS,5000 ) IRD4,BUF08(6)
5000     FORMAT(1X,'IOSTAT= ',I8,' READING BOUND: ',A8)
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      END IF
C
      IF( ISTAT.EQ.1 ) THEN
         CONTINUE
      ELSE
C
C        ALL SEEMS WELL
C
         ISTAT = 2
      END IF
C
      RETURN
      END


      SUBROUTINE DEFINE( KOUNT,ICOL,CARD )
C=====================================================================**
C
C   PURPOSE:  Search columns 1 through 80 of input control record,
C             noting in array IC1 the column where fields begin and
C             in array IC2 where those fields end.
C
C             A field is a group of letters or numbers separated by
C             one or more *spaces* or a *comma*.
C
C-----------------------------------------------------------------------
C     Data declarations
C
      INTEGER MODE,FLDMAX,ICOL
      CHARACTER*(*) CARD
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'
C
C     FLDMAX      MAXIMUM NUMBER OF 'FIELDS' WE DEFINE
C     ICOL        DIMENSION OF CARD 'IMAGE'
C     FLDMAX        Maximum number of fields to define
C
C     Data initialization
      DATA FLDMAX/30/
C
C
      PATH = 'JB'
      LOC  = 'DEFINE'
C
C     Convert any lower case letters to upper case
C
      BUF80(1) = CARD
      CALL LWRUPR(BUF01)
      CARD = BUF80(1)


C     1.  Initialize IC1, IC2, IRD3 and MODE.
C         IRD3 keeps a count on the number of fields found.
C         MODE keeps track of searching for a field beginning or ending
C              0 = searching for field beginning
C              1 = searching for field ending
C
C
      DO 10 I=1,FLDMAX
         IC1(I) = 0
         IC2(I) = 0
   10 CONTINUE
C
      IRD3 = 1
      MODE  = 0
C
C     2.  Loop on first ICOL columns of record
C
      DO 20 I=1,ICOL
C
         IF( MODE.EQ.0 ) THEN
C           Searching for beginning of field
            IF( BUF01(I).NE.' ' .AND. BUF01(I).NE.',' ) THEN
C              Found beginning of a field
               IC1(IRD3) = I
               MODE = 1
            END IF
C
         ELSE
C           Searching for ending of field
            IF ( BUF01(I).EQ.' ' .OR. BUF01(I).EQ.',' ) THEN
C              Found ending of a field
               IC2(IRD3) = I - 1
               IRD3 = IRD3 + 1
               IF( IRD3.GT.FLDMAX ) THEN
                  IRD3 = IRD3 - 1
                  RETURN
               END IF
               MODE = 0
            END IF
C
         END IF
C
   20 CONTINUE
C
      IRD3 = IRD3 - 1
C
C     Test number of fields found, if blank card write message
C
      IF(IRD3.EQ.0) THEN
         ECODE = 'I00'
         WRITE( MESS,1000 )
1000     FORMAT(1X,'BLANK CARD FOUND, SKIP TO NEXT IMAGE')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
      END IF
C
      RETURN
      END


     
      SUBROUTINE FDPATH( KOUNT,CARD )
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C
C        PURPOSE:        ROUTINE ATTEMPTS TO FIND THE PATHWAY
C                        NAME AS THE FIRST WORD ON THE DATA
C                        SETUP CARD IMAGE.
C
C                        THE INPUT IMAGE IS STORED IN CARD WHICH
C                        IS EQUIVALENCED TO BUF80(1).
C
C
C        LOCAL VARIABLES
C
      INTEGER WIDTH
      CHARACTER*80 CARD
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'
C
C        INITIALIZE VALUES
C
      PATH = 'JB'
      LOC  = 'FDPATH'
      MESS = BLNK40
      BUF80(1) = CARD
      BUF02 = '  '
      DO 10 I=1,10
         BUF08(I) = BLNK08
10    CONTINUE
C
C        WIDTH IS THE COMPUTED LENGTH OF THE FIRST WORD FOUND
C              IN THE IMAGE.
C
C        IRD1   WILL BE THE PATHWAY 'MATCH'
C
      WIDTH = IC2(1) - IC1(1) + 1
      IRD1  = 0
C
      IF( WIDTH.EQ.0 ) THEN
C        NOT A LIKELY PROSPECT AS THIS MEANS THE IMAGE IS BLANK
         ECODE = 'I00'
         WRITE( MESS,2000 )
2000     FORMAT(1X,' INPUT CARD IS BLANK ')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
      END IF
C
      IF( WIDTH.NE.2 ) THEN
C        FIRST WORD IS NOT A PATHWAY NAME FOR SURE
         ECODE = 'W01'
         WRITE( MESS,3000 )
3000     FORMAT(1X,' ERROR, IMPROPER PATHWAY ID ' )
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
      END IF
C
C        WELL WE GOT THIS FAR, SO AT LEAST WE MUST HAVE A 2-LETTER
C        FIRST WORD.  LET US SEE IF IT MATCHES GIVEN PATHWAY NAMES.
C
      BUF02 = BUF80(1)( IC1(1):IC2(1) )
C
      DO 20 I=1,6
C
         IF( BUF02.EQ.PATHWD(I) ) THEN
            IRD1 = I
            RETURN
         END IF
C
20    CONTINUE
C
C        NO MATCH FOUND
C
      WRITE( MESS,5000 ) BUF02
5000  FORMAT(' ',A2,' IS AN IMPROPER PATHWAY ID' )
      ECODE = 'W01'
      CALL ERROR ( KOUNT,PATH,ECODE,LOC,MESS )
C
      RETURN
      END
C
C
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
      SUBROUTINE FDKEY( KOUNT,CARD )
C
C        PURPOSE:        ROUTINE ATTEMPTS TO FIND THE KEYWRD
C                        GIVEN IN THE SECOND WORD ON THE DATA
C                        SETUP CARD IMAGE.
C
C                        THE INPUT IMAGE IS STORED IN CARD WHICH
C                        IS EQUIVALENCED TO BUF80(1).
C
C
C       LOCAL VARIABLES
C
      INTEGER WIDTH
      CHARACTER*80 CARD
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'
C
C        INITIALIZE VALUES
C
      PATH = 'JB'
      LOC  = ' FDKEY'
      BUF03 = '   '
      DO 10 I=1,10
         BUF08(I) = BLNK08
10    CONTINUE
C
C        WIDTH IS THE COMPUTED LENGTH OF THE SECOND WORD FOUND
C              IN THE IMAGE.
C
C        IRD1 IS THE PATHWAY
C        IRD2 WILL BE THE KEYWRD FOUND BY THIS SEARCH
C
      WIDTH = IC2(2) - IC1(2) + 1
      IRD2  = 0
C
      IF( WIDTH.EQ.0 ) THEN
C        NOT A LIKELY PROSPECT AS THIS MEANS THE KEYWORD IS BLANK
         ECODE = 'I00'
         WRITE( MESS,2000 )
2000     FORMAT(1X,' BLANK KEYWRD ')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         RETURN
      END IF
C
      IF( WIDTH.NE.3 ) THEN
C        SECOND WORD IS NOT A KEYWRD NAME FOR SURE
         IF(WIDTH.GT.8) WIDTH = 8
         IRD4 = 8 - WIDTH + 1
C
         BUF08(1) = BLNK08
         CARD(IRD4:8) = CARD( IC1(2):IC2(2) )
C
         WRITE( MESS,3000 ) BUF08(1)
3000     FORMAT(1X,A8,' IS  IMPROPER KEYWRD ' )
         ECODE = 'W01'
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         RETURN
      END IF
C
C        WELL WE GOT THIS FAR, SO AT LEAST WE MUST HAVE A 3-LETTER
C        SECOND WORD.  LET US SEE IF IT MATCHES PATHWAY KEYWRDS.
C
      BUF03 = CARD( IC1(2):IC2(2) )
C
      DO 20 I=1,NUMKEY(IRD1)
C
         IF( BUF03.EQ.KEYWRD(IRD1,I) ) THEN
            IRD2 = I
            RETURN
         END IF
C
20    CONTINUE
C
C        NO MATCH FOUND
C
      ECODE = 'W01'
      WRITE( MESS,5000 ) BUF03
5000  FORMAT(' ',A3,' IS AN IMPROPER KEYWRD' )
      CALL ERROR ( KOUNT,PATH,ECODE,LOC,MESS )
C
      IRD2 = 0
      RETURN
      END
