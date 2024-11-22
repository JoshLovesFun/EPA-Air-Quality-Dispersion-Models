      SUBROUTINE WRTCRD( NUMBER,PREP,CARD )
C=====================================================================**
C        PURPOSE:        ROUTINE WRITES 'CARD' IMAGE TO A
C                        TEMPORARY FILE FOR POSSIBLE USE
C                        LATER EITHER IN CONSTRUCTING
C                        HEADERS FOR OUTPUT FILE(S), OR
C                        FOR POSSIBLE USE IN REPORTING
C                        PROCESSING ERRORS.
C
C-----------------------------------------------------------------------
C        LOCAL VARIABLES
C
      INTEGER   ITEST,NUMBER,ISTAT
      CHARACTER CARD*80,NAME*48,PREP*3
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'
C
      DATA ITEST/-1/
C
C        ITEST   = USING THIS TO KEY WHETHER WE HAVE
C                  EVER OPENED THE TEMPORARY FILE.
C        NUMBER  = IF IMAGE IS FROM CURRENT RUN-FILE,
C                  THIS WILL BE CARD IMAGE'S POSITION.
C                  IF IMAGE IS FROM A FILE'S HEADER,
C                  THIS WILL BE POSITION IN HEADER.
C        CARD    = IMAGE TO BE WRITTEN TO TEMPORARY FILE
C        PREP    = 3-CHARACTER PREFIX OF HEADER RECORD
C        ISTAT   = STATUS FROM FLOPEN
C
C
C        INITIALIZE
C
      PATH = PATHWD(IRD1)
      LOC  = 'WRTCRD'
C
C           1.  CHECK ITEST.  IF LESS THAN ZERO
C                          OPEN TEMPORARY FILE.
C
      IF( ITEST.LT.0 ) THEN
C
         ITEST = 1
C
C        SO WE ONLY OPEN FILES USING SUBROUTINE FLOPEN
C        WE WILL CREATE A FAKE CARD IMAGE TO FLOPEN TO
C        PROCESS IN ORDER TO OPEN THE TEMPORARY FILE.
C
         BUF80(5) = BLNK80
         WRITE( BUF80(5),1000 ) PATHWD(IRD1)
1000     FORMAT(A2,' XXX  DISK  XXXXXX.XXX ')
C
         CALL DEFINE( NUMBER,80,BUF80(5) )
         CALL FLOPEN( DEV70,NAME,KOUNT,BUF80(5),4,1,ISTAT )
C
         CALL DEFINE( NUMBER,80,CARD )
C
C        CHECK STATUS OF OPEN
C
         IF( ISTAT.EQ.1 ) THEN
            ITEST = 0
            RETURN
         END IF
C
      END IF
C
C        2. CHECK ITEST.  IF ITEST EQUALS 0, WE HAVE NO TEMPORARY FILE
C           GET OUT OF ROUTINE.
C
      IF( ITEST.EQ.0 ) THEN
         RETURN
      END IF
C
C        3.  SCAN THE CARD IMAGE TO SEE IF THIS IS ONE OF THOSE
C            INPUT CONTROL CARDS THAT ARE TO ALWAYS BE PROCESSED.
C
C        USING THE KEYWRD ARRAY
C        FOR PATHWAYS 2(UA) AND 3(SF) LOOK FOR 'CHK'
C        FOR PATHWAY  4(OS) LOOK FOR 'DT1' THROUGH 'SFC'
C
C        INITIALIZE PREFIX-WORD
C
      BUF08(1) = BLNK08
C
      WRITE( BUF08(1),2000 ) PATHWD(IRD1),NUMBER,PREP(3:3)
2000  FORMAT( A2,1X,I3,1X,A1 )
C
C        PROCESS UA, SF, AND OS PATHWAYS
      IF( IRD1.GE.2 .AND. IRD1.LE.4 ) THEN
C
         IF( IRD1.EQ.2 ) THEN
            IF(KEYWRD(IRD1,11).EQ.CARD(IC1(2):IC2(2))) BUF08(1)(3:3)='$'
         ELSEIF( IRD1.EQ. 3 )THEN
            IF(KEYWRD(IRD1,8).EQ.CARD(IC1(2):IC2(2))) BUF08(1)(3:3)='$'
         ELSEIF( IRD1.EQ. 4 )THEN
            DO 10 I=8,16
             IF(KEYWRD(IRD1,I).EQ.CARD(IC1(2):IC2(2))) BUF08(1)(3:3)='$'
10          CONTINUE
C
         ENDIF
C
C        PROCESS 'LOC' CARDS FOR THESE PATHWAYS
         IF( KEYWRD(IRD1,6).EQ.CARD(IC1(2):IC2(2))
     1    .AND. PREP(3:3).EQ.' ') BUF08(1)(3:3) = '@'
C
      END IF
C
C        4.  WRITE TO TEMPORARY FILE
C
      WRITE( DEV70,3000,IOSTAT=IRD5 ) BUF08(1),CARD
3000  FORMAT( A8,A80 )
C
C        CHECK WRITE STATUS
C
      IF( IRD5.NE.0 ) THEN
         MESS = BLNK40
         ECODE = 'W10'
         WRITE( MESS,4000 ) IRD5
4000     FORMAT(1X,'IOSTAT= ',I8,' WRITE TO DEVICE 70')
         CALL ERROR( NUMBER,PATH,ECODE,LOC,MESS )
      END IF
C
      RETURN
      END
C


C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
      SUBROUTINE EXCARD( KOUNT,CARD,I4YR1,MO1,DY1,I4YR2,MO2,DY2,
     1 DAY1,DAY2,ISTAT )
C
C     PURPOSE:  THIS ROUTINE PROCESSES THE EXTRACT TYPE CARD IMAGES.
C
C     REVISED FOR Y2K COMPLIANCE IN DECEMBER 1999.
C
C     LOCAL VARIABLES
C
      CHARACTER CARD*80
      INTEGER  I2YR1,I4YR1,I2YR2,I4YR2, CENTURY
      INTEGER  YR1,MO1,DY1,YR2,MO2,DY2,ISTAT,WIDTH
      INTEGER  DAY1,DAY2,JULIAN
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'
C
C        JULIAN                FUNCTION (CONVERTS GREGORIAN TO JULIAN)
C
C        I4YR1,I4YR2                START AND STOP YEARS FOR EXTRACT
C        MO1,MO2                START AND STOP MONTHS FOR EXTRACT
C        DY1,DY2                START AND STOP DAYS (GREGORIAN) FOR
C                               EXTRACT
C        DAY1,DAY2              START AND STOP DAYS (JULIAN( FOR EXTRACT
C        ISTAT                  PROCESS STATUS 1 = ERROR IN PROCESSING
C                                              2 = PROCESSING OK
C        WIDTH                  COMPUTED WIDTH OF A WORD (VALUE)
C        CARD                   'IMAGE' WITH EXTRACT DATA
C
C        INITIALIZE VALUES
C
      PATH = PATHWD(IRD1)
      LOC  = 'EXCARD'
      ISTAT = 0
C
C        1.  CHECK IRD3.
C
C        CHECK IRD3 (PASSED THROUGH WORK COMMON) AS IT TELLS US
C        THE NUMBER OF 'WORDS' WITHIN THE IMAGE 'CARD'.
C        IN ORDER FOR US TO HAVE A COMPLETE SET OF EXTRACT DATES,
C        IRD3 SHOULD EQUAL 8.  LESS THAN 8 MEANS TOO LITTLE
C        INFORMATION HAS BEEN GIVEN; MORE THAN 8 MEANS TOO MUCH
C        INFORMATION IS GIVEN.  IN EITHER CASE, WE CALL IT AN
C        ERROR CONDITION.
C
      IF( IRD3.NE.8 ) THEN
         ECODE = 'E04'
         MESS = BLNK40
         IF( IRD3.LT.8 ) THEN
            WRITE( MESS,1000 )
1000        FORMAT(1X,'INCOMPLETE INFORMATION ON EXTRACT CARD')
         ELSE
            WRITE( MESS,2000 )
2000        FORMAT(1X,'SUPERFLUOUS DATA ON EXTRACT CARD')
         END IF
C
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      END IF
C
C        2.  ATTEMPT FOR DATES
C
      BUF08(2) = BLNK08
      IRD4 = 8 - ( IC2(3)-IC1(3) )
      BUF08(2)(IRD4:8) = CARD( IC1(3):IC2(3) )
      READ( BUF08(2),2500,IOSTAT=IRD4 ) YR1
2500  FORMAT(I8)
C
C        CHECK READ STATUS
C
      IF( IRD4.NE.0 ) THEN
         ECODE = 'E03'
         MESS = BLNK40
         WRITE( MESS,3000 ) IRD4
3000     FORMAT(1X,'IOSTAT= ',I8,' READING EXTRACT DATES')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      END IF
C
      BUF08(2) = BLNK08
      IRD4 = 8 - ( IC2(4)-IC1(4) )
      BUF08(2)(IRD4:8) = CARD( IC1(4):IC2(4) )
      READ( BUF08(2),2500,IOSTAT=IRD4 ) MO1
C
C        CHECK READ STATUS
C
      IF( IRD4.NE.0 ) THEN
         ECODE = 'E03'
         MESS = BLNK40
         WRITE( MESS,3000 ) IRD4
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      END IF
C
      BUF08(2) = BLNK08
      IRD4 = 8 - ( IC2(5)-IC1(5) )
      BUF08(2)(IRD4:8) = CARD( IC1(5):IC2(5) )
      READ( BUF08(2),2500,IOSTAT=IRD4 ) DY1
C
C        CHECK READ STATUS
C
      IF( IRD4.NE.0 ) THEN
         ECODE = 'E03'
         MESS = BLNK40
         WRITE( MESS,3000 ) IRD4
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      END IF
C
      BUF08(2) = BLNK08
      IRD4 = 8 - ( IC2(6)-IC1(6) )
      BUF08(2)(IRD4:8) = CARD( IC1(6):IC2(6) )
      READ( BUF08(2),2500,IOSTAT=IRD4 ) YR2
C
C        CHECK READ STATUS
C
      IF( IRD4.NE.0 ) THEN
         ECODE = 'E03'
         MESS = BLNK40
         WRITE( MESS,3000 ) IRD4
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      END IF
C
      BUF08(2) = BLNK08
      IRD4 = 8 - ( IC2(7)-IC1(7) )
      BUF08(2)(IRD4:8) = CARD( IC1(7):IC2(7) )
      READ( BUF08(2),2500,IOSTAT=IRD4 ) MO2
C
C        CHECK READ STATUS
C
      IF( IRD4.NE.0 ) THEN
         ECODE = 'E03'
         MESS = BLNK40
         WRITE( MESS,3000 ) IRD4
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      END IF
C
      BUF08(2) = BLNK08
      IRD4 = 8 - ( IC2(8)-IC1(8) )
      BUF08(2)(IRD4:8) = CARD( IC1(8):IC2(8) )
      READ( BUF08(2),2500,IOSTAT=IRD4 ) DY2
C
C        CHECK READ STATUS
C
      IF( IRD4.NE.0 ) THEN
         ECODE = 'E03'
         MESS = BLNK40
         WRITE( MESS,3000 ) IRD4
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      END IF
C
C        3.  READ OK, CONVERT DATES TO JULIAN IF DATES REASONABLE
C           A.  CONVERT 2-DIGIT YEAR TO 4-DIGIT YEAR AS NECESSARY
C           B.  CHECK I4YR1 AND I4YR2 , AND MO1 AND MO2
C                (1) IF I4YR1 GT. I4YR2        - ERROR CONDITION
C                (2) IF MO1 OR MO2 GT.12       - ERROR CONDITION
C           C.   CONVERT DATES TO JULIAN VALUES
C                (1) IF I4YR1 EQ. I4YR2, TEST THAT DAY1 LE. DAY2
C

      CALL Y2K(PATH, YR1, I2YR1, I4YR1, CENTURY)
      CALL Y2K(PATH, YR2, I2YR2, I4YR2, CENTURY)

      IF( I4YR1.GT.I4YR2 ) THEN
         ECODE = 'E06'
         MESS = BLNK40
         WRITE( MESS,4000 )
4000     FORMAT(1X,'START YEAR LATER THAN STOP YEAR')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      END IF
C
      IF( I4YR1.GT.2099 .OR. I4YR2.GT.2099
     1 .OR. I4YR1.LT.1900 .OR. I4YR2.LT.1900 ) THEN
         ECODE = 'E06'
         MESS = BLNK40
         WRITE( MESS,4500 )
4500     FORMAT(1X,'ERROR IN EXTRACT YEARS ')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      END IF
C
      IF( MO1.GT.12 .OR. MO2.GT.12
     1 .OR. MO1.LT.1 .OR. MO2.LT.1 ) THEN
         ECODE = 'E06'
         MESS = BLNK40
         WRITE( MESS,5000 )
5000     FORMAT(1X,'ERROR IN EXTRACT MONTHS ')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      END IF
C
      DAY1 = JULIAN( I4YR1,MO1,DY1 )
      DAY2 = JULIAN( I4YR2,MO2,DY2 )
C
      IF( I4YR1.EQ.I4YR2 .AND. DAY1.GT.DAY2 ) THEN
         ECODE = 'E06'
         MESS = BLNK40
         WRITE( MESS,6000 )
6000     FORMAT(1X,'START DATE IS OLDER THAN STOP DATE')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      END IF
C
C        4.  ALL LOOKS OK, RETURN
C
      IF( ISTAT.EQ.1 ) THEN
         CONTINUE
      ELSE
         ISTAT = 2
      END IF
C
      RETURN
      END


      SUBROUTINE IN1CRD( KOUNT,CARD,IN1FMT,IN1LOC,ISTAT )
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C
C     PURPOSE:   To process the IN1 setup data, opening (if possible)
C                either a magnetic tape or disk file for input data.
C
C-----------------------------------------------------------------------
C     Variable declarations
C
      INTEGER   ITEST,ISTAT
      CHARACTER CARD*80,IN1FMT*8,IN1LOC*8
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'UA1.INC'
      INCLUDE 'UA2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'WORK1.INC'
C
C     Data initialization
C
      PATH = PATHWD(IRD1)
      LOC  = 'IN1CRD'
      IN1LOC = BLNK08
      IN1FMT = BLNK08

C     Determine which media: DISK or TAPE
C                            (position 3 on record)        ---- CALL GETWRD

      BUF04(1) = BLNK04
      BUF08(1) = ' FORM   '
      CALL GETWRD( 3,KOUNT,CARD,4,4,1,BUF08(1),BUF04(1),ISTAT )
      IF(ISTAT.EQ.1 ) THEN
         RETURN
      END IF

C     Check the result and process accordingly
      IF( BUF04(1) .EQ. FORM(1) ) THEN
C        'TAPE' specified - open it (according to pathway) ---- CALL TPOPEN

         IF( IRD1 .EQ. 2 )THEN
            CALL TPOPEN( DEV10,TAPE10,KOUNT,CARD,IN1FMT,ISTAT )

         ELSEIF( IRD1 .EQ. 3 )THEN
            CALL TPOPEN( DEV20,TAPE10,KOUNT,CARD,IN1FMT,ISTAT )

         ELSE
C           FOR INITIAL DEBUGGING: PRINT AN ERROR MESSAGE
            ISTAT=1
         ENDIF

C        Decipher site location id (position 7 on record)  ---- CALL GETWRD
         IN1LOC = BLNK08
         BUF08(2) = ' IN1LOC '
         CALL GETWRD( 7,KOUNT,CARD,1,8,1,BUF08(2),IN1LOC,ITEST )
C
         IF( ISTAT .NE. 1 )THEN
            ISTAT = ITEST
         END IF

      ELSEIF( BUF04(1) .EQ. FORM(2) ) THEN
C        'DISK' specified - open it (according to pathway) ---- CALL FLOPEN

         IF( IRD1 .EQ. 2 )THEN
            CALL FLOPEN( DEV10,TAPE10,KOUNT,CARD,3,1,ISTAT )

         ELSEIF( IRD1 .EQ. 3 )THEN
C           SF pathway, TD-3280 data on disk file
            CALL FLOPEN( DEV20,UNIT20,KOUNT,CARD,3,1,ISTAT )

         ELSE
C           FOR INITIAL DEBUGGING: PRINT AN ERROR MESSAGE
            print *,'IN1CRD: Incorrect pathway for opening disk file'
            ISTAT=1
         ENDIF

C        Decipher card for disk file's format              ---- CALL GETWRD
C        (position 5 on the record)
         IN1FMT = BLNK08
         BUF08(2) = ' IN1FMT '
         CALL GETWRD( 5,KOUNT,CARD,6,7,1,BUF08(2),IN1FMT,ITEST )
         IF( ISTAT.NE.1) THEN
            ISTAT = ITEST
         END IF

C        Check IN1FMT to insure it is a valid format

         IWORK1(1) = 0
         IF( IRD1 .EQ. 2 )THEN
            IWORK1(1) = INDEX( IN1FMT,'5600')
         ELSEIF( IRD1 .EQ. 3 )THEN
            IWORK1(1) = INDEX( IN1FMT,'3280')
         ENDIF
         IF( IWORK1(1).EQ.0 )THEN
            MESS = BLNK40
            WRITE( MESS,2250 ) IN1FMT
 2250       FORMAT(1X,A8,' :IMPROPER FORMAT SPECIFIED')
            ECODE = 'E06'
            CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         END IF

         IF( ISTAT.NE.1) THEN
            ISTAT = 2
         END IF

C        Decipher site location id (position 6 on record)  ---- CALL GETWRD
         IN1LOC = BLNK08
         BUF08(2) = ' IN1LOC '
         CALL GETWRD( 6,KOUNT,CARD,1,8,1,BUF08(2),IN1LOC,ITEST )

         IF( ISTAT.NE.1) THEN
            ISTAT = ITEST
         END IF

      ELSE
C        Reach this point only if file-form is in error (not DISK or TAPE)

         MESS = BLNK40
         WRITE( MESS,2750 ) BUF04(1)
2750     FORMAT(1X,A4,' :MUST BE "DISK OR TAPE" ')
         ECODE = 'E01'
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1

C        ENDIF for BUF04() = FORM()
      END IF

      RETURN
      END



      SUBROUTINE IN2CRD( KOUNT,CARD,IN2FMT,IN2LOC,ISTAT )
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C
C     PURPOSE:  To process the IN2 setup data, opening (if possible)
C               either a disk or tape for input data.
C
C-----------------------------------------------------------------------
C     Variable declarations
C
      INTEGER   ITEST,ISTAT
      CHARACTER CARD*80,IN2FMT*(*),IN2LOC*8
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'UA1.INC'
      INCLUDE 'UA2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'WORK1.INC'
C
C     Data initialization
C
      PATH = PATHWD(IRD1)
      LOC  = 'IN2CRD'
      IN2LOC = BLNK08

C     Determine which format: DISK or TAPE or USER
C                            (position 3 on the record)    ---- CALL GETWRD

      BUF04(1) = BLNK04
      BUF08(1) = ' FORM   '
      CALL GETWRD( 3,KOUNT,CARD,4,4,1,BUF08(1),BUF04(1),ISTAT )

C     If the format cannot be determined, do not process any more
C     fields onm this record
      IF(ISTAT.EQ.1 ) THEN
         RETURN
      END IF

C     Process the data according to the format
      IF( BUF04(1) .EQ. FORM(1) ) THEN
C        'TAPE' specified - open according to pathway      ---- CALL TPOPEN

         IF( IRD1.EQ.2 ) THEN
            CALL TPOPEN( DEV11,UNIT11,KOUNT,CARD,IN2FMT,ISTAT )
         ELSE
            CALL TPOPEN( DEV20,UNIT20,KOUNT,CARD,IN2FMT,ISTAT )
         END IF

C        Decipher site location id (position 7 on record)  ---- CALL GETWRD

         IN2LOC = BLNK08
         BUF08(2) = ' IN2LOC '
         CALL GETWRD( 7,KOUNT,CARD,1,8,1,BUF08(2),IN2LOC,ITEST )

C        Fields on the record are processed without regard to errors in
C        previous fields, allowing the program to report any additional
C        errors that might occur.  Hence, the use of ITEST and the next
C        IF statement.  Once the status (ISTAT) is set to 1, (indicating
C        errors have been encountered), do not change its value.

         IF( ISTAT .NE. 1 )THEN
            ISTAT = ITEST
         ENDIF

      ELSEIF( BUF04(1) .EQ. FORM(2) ) THEN
C        'DISK' specified - open according to pathway      ---- CALL FLOPEN
         IF( IRD1.EQ.2 ) THEN
            CALL FLOPEN( DEV11,UNIT11,KOUNT,CARD,3,1,ISTAT )
         ELSE
            CALL FLOPEN( DEV20,UNIT20,KOUNT,CARD,3,1,ISTAT )
         END IF

C        Determine file format (position 5 on record)      ---- CALL GETWRD
         IN2FMT = BLNK08
         BUF08(2) = ' IN2FMT '
         CALL GETWRD( 5,KOUNT,CARD,6,7,1,BUF08(2),IN2FMT,ITEST )

         IF( ISTAT.NE.1 )THEN
            ISTAT = ITEST
         END IF

C        Check IN2FMT to insure it is a valid format
         IWORK1(1) = 0
         IF( IRD1.EQ.2 ) THEN
            IWORK1(1) = INDEX( IN2FMT,'9689FB')
         ELSE
            IWORK1(1) = INDEX(IN2FMT,'CD144FB') +
     &                  INDEX(IN2FMT,'SCRAMFB') +                       DGSSEP92
     &                  INDEX(IN2FMT,'SAMSON' )                         JOPOCT95
         END IF

C        If the format is not a valid format, write a message and
C        set the status flag
         IF( IWORK1(1).EQ.0 ) THEN
            MESS = BLNK40
            WRITE( MESS,2250 ) IN2FMT
2250        FORMAT(1X,A8,' :IMPROPER FORMAT SPECIFIED')
            ECODE = 'E06'
            CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         END IF
C
         IF( ISTAT.NE.1 )THEN
            ISTAT = 2
         END IF
C
C        Decipher site location id (position 6 on record)  ---- CALL GETWRD
C
         IN2LOC = BLNK08
         BUF08(2) = ' IN2LOC '
         CALL GETWRD( 6,KOUNT,CARD,1,8,1,BUF08(2),IN2LOC,ITEST )
C
         IF( ISTAT.NE.1 )THEN
            ISTAT = ITEST
         END IF

      ELSEIF( BUF04(1) .EQ. FORM(3) ) THEN
C        A user-specified format was give (for mixing ht data only);
C        open a disk file                                  ---- CALL FLOPEN

         IF( IRD1.EQ.2 ) THEN
            CALL FLOPEN( DEV11,UNIT11,KOUNT,CARD,3,1,ISTAT )

         ELSE
C           Should not be here - valid only for the UA pathway
            ECODE = 'E06'
            MESS = BLNK40
            WRITE( MESS,1500 )
1500        FORMAT(1X,'USER FORMATS NOT ALLOWED FOR INPUT DATA')
            CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
            RETURN
         END IF

C        Decipher card for disk file's format; compute width of
C        IN2FMT and insure it is gt. zero and less than 81 chars.

C        In defining the number of fields on a record, the program
C        searches for blanks and commas.  In a user-specified format,
C        commas are used.  Therefore, all the remaining fields are
C        searched *backward* until the closing parenthesis is
C        encountered.

         IRD5 = IRD3
10       CONTINUE
         IWORK1(1) = IC2(IRD5) - IC1(5) + 1
         IF( IWORK1(1).LE.0 .OR. IWORK1(1).GT.80 ) THEN
            ECODE = 'E05'
            MESS = BLNK40
            WRITE( MESS,2000 )
2000        FORMAT(1X,'ERROR DECIPHERING IN2-CRD FORMAT')
            CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1

         ELSE
C           Fetch IN2FMT and check first and last characters
            IN2FMT = BLNK80
            IN2FMT( 1:IWORK1(1) ) = CARD( IC1(5):IC2(IRD5) )
            IF( IN2FMT( 1:1 ).EQ.'(' .AND.
     &          IN2FMT( IWORK1(1):IWORK1(1) ).EQ.')' ) THEN
               ITEST = 2

            ELSE IF( IRD5.LE.5 ) THEN
               MESS = BLNK40
               WRITE( MESS,2500 )
2500           FORMAT(1X,'IMPROPER SPECIFICATION OF IN2FMT')
               ECODE = 'E06'
               CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
               ITEST = 1
            ELSE
               IRD5 = IRD5 - 1
               GO TO 10
            END IF
C
         END IF
C
         IF( ISTAT.NE.1 )THEN
            ISTAT = ITEST
         END IF
C
C        Decipher site location id starting from the last position
C        in fetching the IN2FMT, at 'field' IRD5, rather than using
C        SUBR.GETWRD
C
         IF( IC1(IRD5+1).EQ.0 .AND. IC2(IRD5+1).EQ.0 ) THEN
            ECODE = 'E05'
            MESS = BLNK40
            WRITE( MESS,2550 )
2550        FORMAT(1X,'IN2LOC FIELD OF IN2-CRD IS BLANK')
            CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
            ITEST = 1

         ELSE
            IWORK1(1) = IC2(IRD5+1) - IC1(IRD5+1) + 1
            IF( IWORK1(1).LT.1 .OR. IWORK1(1).GT.8 ) THEN
               ECODE = 'E05'
               MESS = BLNK40
               WRITE( MESS,2600 )
2600           FORMAT(1X,'ERROR IN SITE ID ON IN2-CRD')
               CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
               ITEST = 1

            ELSE
C              Fetch IN2LOC
               IN2LOC((9-IWORK1(1)):8) = CARD(IC1(IRD5+1):IC2(IRD5+1))
               ITEST = 2
            END IF

         END IF

         IF( ISTAT.NE.1 )THEN
            ISTAT = ITEST
         END IF

      ELSE
C        This point is reached only if file-form is in error

         MESS = BLNK40
         WRITE( MESS,2750 ) BUF04(1)
2750     FORMAT(1X,A4,' :MUST BE "USER,DISK OR TAPE" ')
         ECODE = 'E01'
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1

      ENDIF

      RETURN
      END



      SUBROUTINE IN3CRD( KOUNT,CARD,IN3FMT,IN3LOC,ISTAT )
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C
C     PURPOSE:  To process the IN3 (precipitation) setup data, opening
C               (if possible) a disk file of the input data.
C
C-----------------------------------------------------------------------
C     Variable declarations
C
      INTEGER   ITEST,ISTAT
      CHARACTER CARD*80,IN3FMT*(*),IN3LOC*8
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'UA1.INC'
      INCLUDE 'UA2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'WORK1.INC'
C
C     Data initialization
C
      PATH = PATHWD(IRD1)
      LOC  = 'IN3CRD'
      IN3LOC = BLNK08

C     The precipitation data in the TD-3240 format will only be processed
C     from a disk file.

C     Open  the file, insuring the record came from the SF pathway
C                                                          ---- CALL FLOPEN
      IF( IRD1.EQ.3 ) THEN
         CALL FLOPEN( DEV25,DISK25,KOUNT,CARD,3,1,ISTAT )
      END IF

C     Determine file format (position 5 on record)         ---- CALL GETWRD
      IN3FMT = BLNK08
      BUF08(2) = ' IN3FMT '
      CALL GETWRD( 5,KOUNT,CARD,6,8,1,BUF08(2),IN3FMT,ITEST )

C     Each field on the record is processed without regard to errors in
C     previous fields, allowing the program to report any additional
C     errors that might occur.  Hence, the use of ITEST and the next
C     IF statement.  Once the status (ISTAT) is set to 1 (indicating
C     errors have been encounterd), do not change its value.

      IF( ISTAT.NE.1 )THEN
         ISTAT = ITEST
      END IF

C     Check IN3FMT to insure it is a valid format
      IWORK1(1) = 0
      IWORK1(1) = INDEX( IN3FMT,'3240')

C     If the format is not a valid format, write a message and
C     set the status flag
      IF( IWORK1(1).EQ.0 ) THEN
         MESS = BLNK40
         WRITE( MESS,2250 ) IN3FMT
 2250    FORMAT(1X,A8,' :IMPROPER FORMAT SPECIFIED')
         ECODE = 'E06'
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      END IF

      IF( ISTAT.NE.1 )THEN
         ISTAT = 2
      END IF

C     Decipher site location id (position 6 on record)     ---- CALL GETWRD

      IN3LOC = BLNK08
      BUF08(2) = ' IN3LOC '
      CALL GETWRD( 6,KOUNT,CARD,1,8,1,BUF08(2),IN3LOC,ITEST )

      IF( ISTAT.NE.1 )THEN
         ISTAT = ITEST
      END IF

      RETURN
      END


      SUBROUTINE MRCARD( KOUNT,CARD )
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C
C        PURPOSE:        PROCESS MERGE CONTROL CARDS
C
C        THESE ARE THE AVAILABLE KEYWORDS:
C
C        VALUE        KEYWRD        ACTION
C
C          3          EXT        DEFINE EXTRACT DATES FOR MERGED DATA
C                                SET
C          4          OUT        ASSIGN DISK FILE DEV40 FOR MERGED DATA
C                                SET
C
C-----------------------------------------------------------------------
C
C        LOCAL VARIABLES
C
      INTEGER   ISTAT
      CHARACTER CARD*80
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'
C
C        ISTAT        IS A TEMPORARY STATUS WORD
C        CARD        IS 'CARD' IMAGE FROM INPUT DATA SET
C
C        INITIALIZE
C
      PATH = 'MR'
      LOC  = 'MRCARD'
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
C           THIS KEYWORD BEFORE.
C
      IF( STATUS(IRD1,IRD2).NE.0 ) THEN
C       ERROR, WE HAVE SEEN THIS KEYWORD BEFORE
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
      GO TO(10,20),IRD2-2
C
10    CONTINUE
C       EXT CARD - DEFINE EXTRACT INFORMATION
C
      ISTAT = 0
C
      CALL EXCARD( KOUNT,CARD,MRYR1,MRGMO1,MRGDY1,
     1 MRYR2,MRGMO2,MRGDY2,MRDAY1,MRDAY2,ISTAT )
C
      IF( STATUS(IRD1,IRD2).EQ.1 ) THEN
         CONTINUE
      ELSE
         STATUS(IRD1,IRD2) = ISTAT
      END IF
      RETURN
C
20    CONTINUE
C       OUT CARD - ATTEMPT TO ASSIGN INPUT DISK FILE FOR MERGE
C
      ISTAT = 0
      CALL FLOPEN( DEV40,DISK40,KOUNT,CARD,3,2,ISTAT )
      IF( STATUS(IRD1,IRD2) .EQ. 1 ) THEN
         CONTINUE
      ELSE
         STATUS(IRD1,IRD2) = ISTAT
      END IF
C
      RETURN
      END
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
      SUBROUTINE SFTRA( KOUNT,CARD,ISTAT )
C
C        PURPOSE:        THIS ROUTINE PROCESSES THE SF PATHWAY
C                        DEFINITIONS OF VARIABLES TO BE TRACED
C                        IN THE ERROR REPORT.
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
C        CARD               'IMAGE' WITH TRACE VARIABLES LISTED
C        NAME               VNAME ON IMAGE
C
C        INITIALIZE VALUES
C
      PATH = 'SF'
      LOC  = ' SFTRA'
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
1000     FORMAT(1X,'INCOMPLETE INFORMATION ON TRA CARD')
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
C        5. SET TRACE FLAG FOR THIS VARIABLE, IF NO ERRORS HAVE OCCURRED
C           IN PROCESSING TRA-CRD IMAGES.  ALSO, SET THE AUDIT FLAG AS
C           WELL SINCE THE TRACE APPLIES TO AUDITED VARIABLES.
C
         IF( ISTAT.EQ.1 ) THEN
            CONTINUE
         ELSE
            SFSTRA(I-29) = 1
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
      SUBROUTINE UATRA( KOUNT,CARD,ISTAT )
C
C        PURPOSE:        THIS ROUTINE PROCESSES THE UA PATHWAY
C                        DEFINITIONS OF VARIABLES TO BE TRACED
C                        IN THE ERROR REPORT.
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
C        CARD               'IMAGE' WITH  TRACE VARIABLES LISTED
C        NAME               VNAME ON IMAGE
C
C        INITIALIZE VALUES
C
      PATH = 'UA'
      LOC  = ' UATRA'
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
1000     FORMAT(1X,'INCOMPLETE INFORMATION ON TRA CARD')
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
C        5. SET TRACE FLAG FOR THIS VARIABLE, IF NO ERRORS HAVE OCCURRED
C           IN PROCESSING TRA-CRD IMAGES.  ALSO, SET THE AUDIT FLAG AS
C           WELL SINCE THE TRACE APPLIES TO AUDITED VARIABLES.
C
         IF( ISTAT.EQ.1 ) THEN
            CONTINUE
         ELSE
            IF( I.GE.11 ) THEN
               UASTRA(I-10) = 1
               UASAUD(I-10) = 1
            ELSE
               UAVTRA(I) = 1
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
      SUBROUTINE OSTRA( KOUNT,CARD,ISTAT )
C
C        PURPOSE:        THIS ROUTINE PROCESSES THE OS PATHWAY
C                        DEFINITIONS OF VARIABLES TO BE TRACED
C                        IN THE ERROR REPORT.
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
C        CARD               'IMAGE' WITH TRACE VARIABLES LISTED
C        NAME               VNAME ON IMAGE
C
C        INITIALIZE VALUES
C
      PATH = 'OS'
      LOC  = ' OSTRA'
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
1000     FORMAT(1X,'INCOMPLETE INFORMATION ON TRA CARD')
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
C        5. SET TRACE FLAG FOR THIS VARIABLE, IF NO ERRORS HAVE OCCURRED
C           IN PROCESSING TRA-CRD IMAGES.  ALSO, SET THE AUDIT FLAG AS
C           WELL SINCE THE TRACE APPLIES TO AUDITED VARIABLES.
C
         IF( ISTAT.EQ.1 ) THEN
            CONTINUE
         ELSE
            OSSTRA(I) = 1
            OSSAUD(I) = 1
            ISTAT = 2
         END IF
C
50    CONTINUE
C
      RETURN
      END


C---- The next two subroutines are used to set up the SAMSON data and are
C     called from SFEXT.

      SUBROUTINE DEF256( ICOL, CARD, IFC )
C=====================================================================**
C
C   PURPOSE:  Search columns (up to 256) of the SAMSON header record
C             defining the variables in the data file; array IC1
C             contains the column where fields begin and IC2 where
C             those fields end.
C
C             A field is a group of letters or numbers separated by
C             one or more spaces.
C
C-----------------------------------------------------------------------
C     Data declarations
C
      INTEGER MODE,FLDMAX,ICOL
      CHARACTER*(*) CARD

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'
C
C     ICOL        Maximum length of card record
C     FLDMAX      Maximum number of fields to define
C
C     Data initialization
      DATA FLDMAX/30/
C
      PATH = 'SF'
      LOC  = 'DEF256'

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
C     2.  Loop on ICOL columns of record
C
      DO 20 I=1,ICOL
C
         IF( MODE.EQ.0 ) THEN
C           Searching for beginning of field
            IF( CARD(I:I).NE.' ' ) THEN
C              Found beginning of a field
               IC1(IRD3) = I
               MODE = 1
            END IF
C
         ELSE
C           Searching for ending of field
            IF ( CARD(I:I).EQ.' ' ) THEN
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
      IFC = IRD3 - 1
C
C     Test number of fields found, if less than 5, then at most only
C     the date and observation flage fields are in the file
C
      IF(IRD3.LE.5) THEN
         ECODE = 'E48'
         WRITE( MESS,1000 )
1000     FORMAT(' NO DATA FIELDS DEFINED FOR SAMSON FILE')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         SFSTAT = -1
      END IF
C
      RETURN
      END



      SUBROUTINE GETFLD( IFC, ALINE, FIELD )
C***********************************************************************
C
C  PURPOSE: Gets Contents of Fields on the second SAMSON header record
C
C  PROGRAMMER: Jim Paumer, based on the routine for ISC2
C
C  DATE:    April 28, 1995
C
C  INPUTS:  Fields on a line
C
C  OUTPUTS: Contents of Fields on Card
C
C  CALLED FROM: SETSAM
C
C***********************************************************************
C
C     Variable Declarations
      CHARACTER*(*) ALINE
      character*1   aline1(256)
      CHARACTER*40  FIELD(IFC)
      INCLUDE 'MAIN2.INC'

      llen = ic2(ifc)
      do 15 i=1,llen
         aline1(i) = aline(i:i)
  15  continue

      DO 25 I = 1, IFC
         IF (IC2(I)-IC1(I) .LE. 39) THEN
C           Field Satisfies Limit of 40 Characters

            WRITE(FIELD(I),9004) (ALINE1(J),J=IC1(I),IC2(I))
         ELSE
            WRITE(FIELD(I),9004) (ALINE1(J),J=IC1(I),IC2(I))
         END IF
 25   CONTINUE

 9004 FORMAT(40(A1:))

      RETURN
      END
