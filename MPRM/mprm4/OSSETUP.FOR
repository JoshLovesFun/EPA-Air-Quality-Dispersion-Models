      SUBROUTINE OSCARD( KOUNT,CARD )
C=====================================================================**
C
C   PURPOSE:  Processes the OS pathway records
C
C     The valid keywords are:
C
C       VALUE     KEYWORD    ACTION
C
C        3        EXT        DEFINE EXTRACT INFORMATION
C        4        IQA        ASSIGN DISK FILE DEV12
C        5        OQA        ASSIGN DISK FILE DEV13
C        6        LOC        DEFINE LOCATION INFORMATION
C        7        AVG        DEFINE DATA FREQUENCY PER HOUR
C        8        DT1        DEFINE DELTA-T-1 HEIGHTS
C        9        DT2        DEFINE DELTA-T-2 HEIGHTS
C       10        DT3        DEFINE DELTA-T-3 HEIGHTS
C       11        HGT        DEFINE TOWER HEIGHTS
C       12        CLM        DEFINE CALM CRITERIA
C       13        CHK        OVERRIDE A DEFAULT QA LIMIT CHECK
C       14        MAP        DEFINE DATA MAP
C       15        FMT        DEFINE DATA FORMAT
C       16        SFC        DEFINE SURFACE CHARACTERISTICS
C       17        AUD        ADD AUDIT VARIABLES TO THIS PATHWAY
C       18        TRA        TURN ON TRACE NOTES FOR VARIABLE
C
C-----------------------------------------------------------------------
C     Data declarations
C
      INTEGER WIDTH,ISTAT,ITEST
      CHARACTER CARD*80
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'WORK1.INC'
C
C     WIDTH is the computed length of a word within the runstream record
C     NAME  is a temporary storage location for files/tapes to be opened
C     ISTAT is a temporary location to report the status of an attempt
C           to open a file/tape.
C
C     Initialize variables
C
        PATH = 'OS'
        LOC  = 'OSCARD'
        ISTAT = 0
C
C     Initial check on keyword value just to insure there isn't a
C     program logic error.
C
        IF( IRD2.LE.2 .OR. IRD2.GT.NUMKEY(IRD1) ) THEN
C
C     There is a program logic error, stop processing
C
        ECODE = '***'
        MESS = BLNK40
        WRITE( MESS,900 ) IRD1,IRD2
 900    FORMAT(1X,'LOGIC ERROR, IRD1 & IRD2 ARE: ',2I3)
        CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
        STOP
        END IF
C
C     1.  Check status to see if this keyword has been seen before.
C         NOTE: on the OS pathway, the 'LOC','CHK','MAP','FMT','SFC',   DTB96025
C         'AUD', and 'TRA' CARDS can be repeated, so skip the test
C         for a repeated keyword.
C
        IF( IRD2.EQ.6 .OR. IRD2.GE.13 .AND. IRD2.LE.18 ) THEN           DTB96025
C
           CONTINUE
C
        ELSE IF( STATUS(IRD1,IRD2).NE.0 ) THEN
C          ERROR, WE HAVE SEEN THIS KEYWORD BEFORE
           ECODE = 'E01'
           MESS = BLNK40
           WRITE( MESS,1000 ) KEYWRD(IRD1,IRD2)
1000       FORMAT(1X,'REDUNDANT SPECIFICATION OF KEYWRD ',A3)
           CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
           STATUS(IRD1,IRD2) = 1
        END IF
C
C     2.  Decipher keyword
C
Cjop  GO TO(10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160) IRD2-2
C
      IF( IRD2-2 .EQ. 1 )THEN
C------- EXT keyword - define extract information
C
         CALL EXCARD( KOUNT,CARD,OSYR1,OSGMO1,OSGDY1,
     1               OSYR2,OSGMO2,OSGDY2,OSDAY1,OSDAY2,ISTAT )
         IF( STATUS(IRD1,IRD2) .EQ. 1 ) THEN
            CONTINUE
         ELSE
            STATUS(IRD1,IRD2) = ISTAT
         END IF
         RETURN
C
      ELSEIF( IRD2-2 .EQ. 2 )THEN
C------- IQA keyword - attempt to assign input disk file for QA
         CALL FLOPEN( DEV31,DISK31,KOUNT,CARD,3,1,ISTAT )
         IF( STATUS(IRD1,IRD2) .EQ. 1 ) THEN
            CONTINUE
         ELSE
            STATUS(IRD1,IRD2) = ISTAT
         END IF
         RETURN
C
      ELSEIF( IRD2-2 .EQ. 3 )THEN
C------- OQA keyword - attempt to assign output disk file for QA
         CALL FLOPEN( DEV32,DISK32,KOUNT,CARD,3,1,ISTAT )
         IF( STATUS(IRD1,IRD2) .EQ. 1 ) THEN
            CONTINUE
         ELSE
            STATUS(IRD1,IRD2) = ISTAT
         END IF
         RETURN
C
      ELSEIF( IRD2-2 .EQ. 4 )THEN
C------- LOC keyword - define site location information
         OSLOC = BLNK08
         OSLON = BLNK08
         OSLAT = BLNK08
         CALL LOCCRD( KOUNT,CARD,OSLOC,OSLAT,OSLON,OSLST,ISTAT )
         IF( STATUS(IRD1,IRD2) .EQ. 1 ) THEN
            CONTINUE
         ELSE
            STATUS(IRD1,IRD2) = ISTAT
         END IF
         RETURN
C
      ELSEIF( IRD2-2 .EQ. 5 )THEN
C------- AVG keyword - define number of observation periods per hour
         CALL AVGCRD( KOUNT,CARD,ISTAT )
         IF( STATUS(IRD1,IRD2) .EQ. 1 ) THEN
            CONTINUE
         ELSE
            STATUS(IRD1,IRD2) = ISTAT
         END IF
         RETURN
C
      ELSEIF( IRD2-2 .EQ. 6 )THEN
C------- DT1 keyword - define the heights for the 1st delta-T; increment OSNDT
         ITEST = 1
         CALL DTCRD( KOUNT,CARD,ITEST,ISTAT )
         IF( STATUS(IRD1,IRD2) .EQ. 1 ) THEN
            CONTINUE
         ELSE
            STATUS(IRD1,IRD2) = ISTAT
         END IF
         RETURN

      ELSEIF( IRD2-2 .EQ. 7 )THEN
C------- DT2 keyword - define the heights for the 2nd delta-T; increment OSNDT
         ITEST = 2
          CALL DTCRD( KOUNT,CARD,ITEST,ISTAT )
         IF( STATUS(IRD1,IRD2) .EQ. 1 ) THEN
            CONTINUE
         ELSE
            STATUS(IRD1,IRD2) = ISTAT
         END IF
         RETURN

      ELSEIF( IRD2-2 .EQ. 8 )THEN
C------- DT3 keyword - define the heights for the 3rd delta-T; increment OSNDT
         ITEST = 3
         CALL DTCRD( KOUNT,CARD,ITEST,ISTAT )
         IF( STATUS(IRD1,IRD2) .EQ. 1 ) THEN
            CONTINUE
         ELSE
            STATUS(IRD1,IRD2) = ISTAT
         END IF
         RETURN

      ELSEIF( IRD2-2 .EQ. 9 )THEN
C------- HGT keyword - define tower heights
         CALL HGTCRD( KOUNT,CARD,ISTAT )
         IF( STATUS(IRD1,IRD2) .EQ. 1 ) THEN
            CONTINUE
         ELSE
            STATUS(IRD1,IRD2) = ISTAT
         END IF
         RETURN

      ELSEIF( IRD2-2 .EQ. 10 )THEN
C------- CLM keyword - define calm threshold
         CALL CLMCRD( KOUNT,CARD,ISTAT )
         IF( STATUS(IRD1,IRD2) .EQ. 1 ) THEN
            CONTINUE
         ELSE
            STATUS(IRD1,IRD2) = ISTAT
         END IF
         RETURN

      ELSEIF( IRD2-2 .EQ. 11 )THEN
C------- CHK keyword - alter default range checks for OS variable
         CALL OSCHK( KOUNT,CARD,ISTAT )
         IF( STATUS(IRD1,IRD2).EQ.1 ) THEN
            CONTINUE
         ELSE
            STATUS(IRD1,IRD2) = ISTAT
         END IF
         RETURN

      ELSEIF( IRD2-2 .EQ. 12 )THEN
C------- MAP keyword - define data map (split logic on scalar versus tower)
         CALL MAPCRD( KOUNT,CARD,ISTAT )
         IF( STATUS(IRD1,IRD2).EQ.1 ) THEN
            CONTINUE
         ELSE
            STATUS(IRD1,IRD2) = ISTAT
         END IF
         RETURN

      ELSEIF( IRD2-2 .EQ. 13 )THEN
C------- FMT keyword - define formats (split logic on scalar versus tower)
         CALL FMTCRD( KOUNT,CARD,ISTAT )
         IF( STATUS(IRD1,IRD2).EQ.1 ) THEN
            CONTINUE
         ELSE
            STATUS(IRD1,IRD2) = ISTAT
         END IF
         RETURN

      ELSEIF( IRD2-2 .EQ. 14 )THEN
C------- SFC keyword - define site surface characteristics
         CALL SFCCRD( KOUNT,CARD,ISTAT )
         IF( STATUS(IRD1,IRD2).EQ.1 ) THEN
            CONTINUE
         ELSE
            STATUS(IRD1,IRD2) = ISTAT
Cdbg    print *, ' ISTAT from SFCCRD = ',istat
Cdbg    print *, ' '
         END IF
         RETURN
C
      ELSEIF( IRD2-2 .EQ. 15 )THEN
C------- AUD keyword - add audit variables to this pathway
         CALL OSAUT( KOUNT,CARD,ISTAT )
         IF( STATUS(IRD1,IRD2).EQ.1 ) THEN
            CONTINUE
         ELSE
            STATUS(IRD1,IRD2) = ISTAT
         END IF
         RETURN
C
      ELSEIF( IRD2-2 .EQ. 16 )THEN
C------- TRA keyword - add trace notes for variables on this pathway
         CALL OSTRA( KOUNT,CARD,ISTAT )
         IF( STATUS(IRD1,IRD2).EQ.1 ) THEN
            CONTINUE
         ELSE
            STATUS(IRD1,IRD2) = ISTAT
         END IF
         RETURN

      ENDIF

      END


      SUBROUTINE OSCHK( KOUNT,CARD,ISTAT )
C=====================================================================**
C
C        PURPOSE:        THIS ROUTINE PROCESSES THE OS PATHWAY
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
      PATH = 'OS'
      LOC  = ' OSCHK'
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
         GO TO 30
      END IF
C
C       LOOP THROUGH ACCEPTABLE DATA VARIABLES
C
      DO 10 I=1,34
         IF( NAME.EQ.VNAMES(I) ) THEN
            GO TO 40
         END IF
10    CONTINUE
C
C       LOOP THROUGH OS-DATE VARIABLES
C
      DO 20 I=56,60
         IF( NAME.EQ.VNAMES(I) ) THEN
            GO TO 40
         END IF
20    CONTINUE
C
C       IF WE GET HERE THERE WAS NO MATCH
C
30    ECODE = 'E06'
      MESS = BLNK40
      WRITE( MESS,5000 ) NAME
5000  FORMAT(1X,A4,' NO MATCH WITH OS NAMES')
      CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
      ISTAT = 1
C
40    CONTINUE
C
C        5. REDEFINE QA VALUES
C
      IF( ISTAT.EQ.1 ) THEN
         CONTINUE
      ELSE
C       SPECIAL TRAP FOR 'TSKY' ON OS-PATHWAY
         IF( I.EQ.34 ) THEN
            OSTSKY(1) = KEY
            OSTSKY(2) = MISS
            OSTSKY(3) = LBOUND
            OSTSKY(4) = UBOUND
         ELSE
            SFQA(I,1) = KEY
            SFQA(I,2) = MISS
            SFQA(I,3) = LBOUND
            SFQA(I,4) = UBOUND
         END IF
C
         ISTAT = 2
      END IF
C
      RETURN
      END
 

      SUBROUTINE DTCRD( KOUNT,CARD,NN,ISTAT )
C=====================================================================**
C
C        PURPOSE:        THIS ROUTINE PROCESSES THE OS PATHWAY
C                        DEFINITIONS OF THE DELTA-T MEASUREMENT
C                        HEIGHTS.
C
C-----------------------------------------------------------------------
C        LOCAL VARIABLES
C
      CHARACTER CARD*80
      INTEGER  ISTAT,NN
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'WORK1.INC'
C
C        ISTAT           PROCESS STATUS 1 = ERROR IN PROCESSING
C                                       2 = PROCESSING OK
C        CARD            'IMAGE' WITH DELTA-T DATA
C        NN              DELTA-T MEASURMENT PAIR (E.G. 1,2 OR 3)
C
C        INITIALIZE VALUES
C
      PATH = 'OS'
      LOC  = ' DTCRD'
      ISTAT = 0
C
C        1.  CHECK IRD3.
C
C        CHECK IRD3 (PASSED THROUGH WORK COMMON) AS IT TELLS US
C        THE NUMBER OF 'WORDS' WITHIN THE IMAGE 'CARD'.
C        IN ORDER FOR US TO HAVE A COMPLETE SET OF DT MEASURMENT
C        HEIGHTS, IRD3 SHOULD EQUAL 4.  LESS THAN 4 MEANS
C        TOO LITTLE INFORMATION HAS BEEN GIVEN; MORE THAN 4
C        MEANS TOO MUCH INFROMATION HAS BEEN GIVEN.
C       IN EITHER CASE, WE CALL IT AN ERROR CONDITION.
C
      IF( IRD3.NE.4 ) THEN
         ECODE = 'E04'
         MESS = BLNK40
         IF( IRD3.LT.4 ) THEN
            WRITE( MESS,1000 ) NN
1000        FORMAT(1X,'INCOMPLETE INFORMATION ON DT-',I1,' CARD')
         ELSE
            WRITE( MESS,2000 ) NN
2000        FORMAT(1X,'SUPERFLUOUS DATA ON DT-',I1,' CARD')
         END IF
C
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      END IF
C
C        2.  CONSTRUCT A FORMAT TO READ THE GIVEN HEIGHTS
      BUF40 = BLNK40
      IWORK1(1) = IC1(3) - 1
      IWORK1(2) = IC2(3) - IC1(3) + 1
      IWORK1(3) = IC1(4) - IC2(3) - 1
      IWORK1(4) = IC2(4) - IC1(4) + 1
      WRITE( BUF40,3000 ) (IWORK1(J),J=1,4)
3000  FORMAT( '(',I2,'X,F',I1,'.0,',I2,'X,F',I1,'.0)' )
C
C        3.  ATTEMPT READ OF MEASURMENT HEIGHT DATA VALUES
      READ( CARD,BUF40,IOSTAT=IRD4 ) OSLL(NN),OSUL(NN)
C
C        CHECK READ STATUS
C
      IF( IRD4.NE.0 ) THEN
         ECODE = 'E03'
         MESS = BLNK40
         WRITE( MESS,4000 ) IRD4,NN
4000     FORMAT( 1X,'IOSTAT= ',I8,' READING: DT-',I1,' HEIGHTS')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      END IF
C
C        4.  READ OK, MAKE SURE UPPER LEVEL IS ABOVE LOWER LEVEL
C
      IF( OSLL(NN).GE.OSUL(NN) .OR. OSLL(NN).LE.0  .OR.
     &    OSUL(NN).LE.0) THEN
         ECODE = 'E06'
         MESS = BLNK40
         WRITE( MESS,5000 ) NN,OSLL(NN),OSUL(NN)
5000     FORMAT(1X,'DT-',I1,' HEIGHTS IN ERROR: ',2F5.1 )
         CALL ERROR ( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      END IF
C
C        ALL SEEMS OK
C       INCREASE COUNTER FOR NUMBER OF DT VALUES
C
      IF( ISTAT.EQ.1 ) THEN
         CONTINUE
      ELSE
         OSNDT = OSNDT + 1
         ISTAT = 2
      END IF
C
      RETURN
      END


      SUBROUTINE HGTCRD( KOUNT,CARD,ISTAT )  
C=====================================================================**
C       PURPOSE:        THIS ROUTINE PROCESSES THE OS PATHWAY
C                        DEFINITIONS OF THE TOWER MEASUREMENT
C                        HEIGHTS.
C
C
C
C        LOCAL VARIABLES
C
      CHARACTER CARD*80
      INTEGER  ISTAT
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'WORK1.INC'
C
C        ISTAT           PROCESS STATUS 1 = ERROR IN PROCESSING
C                                       2 = PROCESSING OK
C        CARD            'IMAGE' WITH TOWER HEIGHT DATA
C
C        INITIALIZE VALUES
C
      PATH = 'OS'
      LOC  = 'HGTCRD'
      ISTAT = 0
C
C        1.  CHECK IRD3.
C
C        CHECK IRD3 (PASSED THROUGH WORK COMMON) AS IT TELLS US
C        THE NUMBER OF 'WORDS' WITHIN THE IMAGE 'CARD'.
C        IN ORDER FOR US TO HAVE A COMPLETE SET OF MEASURMENT
C        HEIGHTS, IRD3 SHOULD BE AT LEAST 4.  LESS THAN 4 MEANS
C        TOO LITTLE INFORMATION HAS BEEN GIVEN; MORE THAN 4
C        MEANS WE HAVE MORE THAN ONE TOWER MEASUREMENT HEIGHT.
C
      IF( IRD3.LT.4 ) THEN
         ECODE = 'E04'
         MESS = BLNK40
         WRITE( MESS,1000 )
1000     FORMAT(1X,'INCOMPLETE INFORMATION ON HGT CARD')
C
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      END IF
C
C        2.  CONSTRUCT A FORMAT TO READ NUMBER OF TOWER LEVELS
      BUF40 = BLNK40
      IWORK1(1) = IC1(3) - 1
      IWORK1(2) = IC2(3) - IC1(3) + 1
C
C        CHECK FIELD WIDTH
C
      IF( IWORK1(2).LE.0 ) THEN
         ECODE = 'E06'
         MESS = BLNK40
         WRITE( MESS,2500 )
2500     FORMAT( 1X,'NUMBER OF TOWER LEVELS MISSING: HGT CARD')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
C
C        SKIP REST OF LOGIC IF OSNL IS MISSING
C
         RETURN
      END IF
C
      WRITE( BUF40,3000 ) (IWORK1(J),J=1,2)
3000  FORMAT( '(',I2,'X,I',I1,')' )
C
C        3.  ATTEMPT TO READ NUMBER OF LEVELS (OSNL)
      READ( CARD,BUF40,IOSTAT=IRD4 ) OSNL
C
C        CHECK READ STATUS
C
      IF( IRD4.NE.0 ) THEN
         ECODE = 'E03'
         MESS = BLNK40
         WRITE( MESS,4000 ) IRD4
4000     FORMAT( 1X,'IOSTAT= ',I8,' READING FOR OSNL')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
C
C        IF READ ERROR ON FETCH OF OSNL, SKIP REST
C        OF LOGIC
C
         RETURN
      END IF
C
C        4.  READ OK, NOW READ REST OF LEVELS
C
      DO 10 I=1,OSNL
         BUF40 = BLNK40
         IWORK1(1) = IC1(3+I) - 1
         IWORK1(2) = IC2(3+I) - IC1(3+I) + 1
C
C        CHECK FIELD WIDTH
C
         IF( IWORK1(2).LE.0 ) THEN
            ECODE = 'E04'
            MESS = BLNK40
            WRITE( MESS,5000 ) I
5000        FORMAT( 1X,'NO HEIGHT GIVEN FOR LEVEL ',I2)
            CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         ELSE
C
            WRITE( BUF40,6000 ) (IWORK1(J),J=1,2)
6000        FORMAT( '(',I2,'X,F',I1,'.0)' )
C
            READ( CARD,BUF40,IOSTAT=IRD4 ) OSHT(I)
C
C        CHECK READ STATUS
C
            IF( IRD4.NE.0 ) THEN
               ECODE = 'E03'
               MESS = BLNK40
               WRITE( MESS,7000 ) IRD4,I
7000           FORMAT( 1X,'IOSTAT= ',I8,' READING FOR LEVEL ',I1)
               CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
               ISTAT = 1
            END IF
C
         END IF
C
10    CONTINUE
C
      IF( ISTAT.EQ.1 ) THEN
         CONTINUE
      ELSE
C
C        LOOKS LIKE ALL IS WELL
C
         ISTAT = 2
      END IF
C
      RETURN
      END


      SUBROUTINE AVGCRD( KOUNT,CARD,ISTAT )
C=====================================================================**
C
C        PURPOSE:        THIS ROUTINE PROCESSES THE OS PATHWAY
C                        DEFINITIONS OF THE NUMBER OF OBSERVATIONS
C                        REPORTED DURING EACH HOUR.
C
C
C
C        LOCAL VARIABLES
C
      CHARACTER CARD*80
      INTEGER  ISTAT
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'WORK1.INC'
C
C        ISTAT       PROCESS STATUS 1 = ERROR IN PROCESSING
C                                   2 = PROCESSING OK
C        CARD        'IMAGE' AVG DATA (DEFINE NUMBER OF 'OBS'/HR)
C
C        INITIALIZE VALUES
C
      PATH = 'OS'
      LOC  = 'AVGCRD'
      ISTAT = 0
C
C        1.  CHECK IRD3.
C
C        CHECK IRD3 (PASSED THROUGH WORK COMMON) AS IT TELLS US
C        THE NUMBER OF 'WORDS' WITHIN THE IMAGE 'CARD'.
C        IN ORDER FOR US TO HAVE A COMPLETE AVG CARD,
C        IRD3 SHOULD BE EQUAL TO 3.  LESS THAN 3 MEANS
C        TOO LITTLE INFORMATION HAS BEEN GIVEN; MORE THAN 3
C        MEANS WE HAVE MORE INFORMATION ON THIS CARD THAN WE
C        SHOULD.
C
      IF( IRD3.NE.3 ) THEN
         ECODE = 'E04'
         MESS = BLNK40
         IF( IRD3.LT.3 ) THEN
            WRITE( MESS,1000 )
1000        FORMAT(1X,'INCOMPLETE INFORMATION ON AVG CARD')
         ELSE
            WRITE( MESS,2000 )
2000        FORMAT(1X,'SUPERFLUOUS DATA ON AVG CARD')
         END IF
C
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      END IF
C
C        2.  CONSTRUCT A FORMAT TO READ NUMBER OF OBSERVATIONS PER HOUR
      BUF40 = BLNK40
      IWORK1(1) = IC1(3) - 1
      IWORK1(2) = IC2(3) - IC1(3) + 1
C
C        CHECK FIELD WIDTH
C
      IF( IWORK1(2).LE.0 ) THEN
         ECODE = 'E04'
         MESS = BLNK40
         WRITE( MESS,2500 )
2500     FORMAT( 1X,'NUMBER OF OBS/HOUR MISSING: AVG CARD ')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      END IF
C
      WRITE( BUF40,3000 ) (IWORK1(J),J=1,2)
3000  FORMAT( '(',I2,'X,I',I1,')' )
C
C        3.  ATTEMPT TO READ NUMBER OF OBSERVATIONS PER HOUR (OSAVG)
      READ( CARD,BUF40,IOSTAT=IRD4 ) OSAVG
C
C        CHECK READ STATUS
C
      IF( IRD4.NE.0 ) THEN
         ECODE = 'E03'
         MESS = BLNK40
         WRITE( MESS,4000 ) IRD4
4000     FORMAT( 1X,'IOSTAT= ',I8,' READING FOR OSAVG')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      END IF
C
C        4.  MAKE SURE OSAVG IS GREATER THAN ZERO
C
      IF( OSAVG.LE.0 ) THEN
         ECODE = 'E06'
         MESS = BLNK40
         WRITE( MESS,5000 ) OSAVG
5000     FORMAT( 1X,I8,' UNREASONABLE VALUE FOR OSAVG')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      END IF
C
C        LOOKS LIKE ALL IS WELL
C
      IF( ISTAT.EQ.1 ) THEN
         CONTINUE
      ELSE
         IWORK1(10) = MOD( OSAVG,2 )
         IF( IWORK1(10).EQ.0 ) THEN
            OSMIN = OSAVG/2
         ELSE
            OSMIN = OSAVG/2 + 1
         END IF
         ISTAT = 2
      END IF
C
      RETURN
      END


      SUBROUTINE MAPCRD( KOUNT,CARD,ISTAT )
C=====================================================================**
C
C        PURPOSE:        THIS ROUTINE PROCESSES THE OS PATHWAY
C                        DEFINITIONS OF THE VARIABLE LIST(S).
C                        IT IS PRIMARILY AS A RESULT OF THE MAP
C                        STATEMENTS THAT WE DEFINE THE OS INPUT
C                        DATA.
C
C
C        LOCAL VARIABLES
C
      CHARACTER CARD*80, NAME*5
      INTEGER  ISTAT,ITEST
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'WORK1.INC'
C
C        ISTAT        PROCESS STATUS 1 = ERROR IN PROCESSING
C                                    2 = PROCESSING OK
C        CARD         'IMAGE' WITH MAP DATA ('MAPS' INPUT DATA LIST(S))
C        NAME         MAP IMAGE ID (OF THE FORM DATXX OR LVLXX)
C
C        INITIALIZE VALUES
C
      PATH = 'OS'
      LOC  = 'MAPCRD'
      ISTAT = 0
C
C        1.  READ THIRD DATA FIELD WITHIN 'CARD'
C            THIS SHOULD BE 5 CHARACTERS, WITH THE
C            FIRST 3 LETTERS OF 'DAT' OR LVL.
C
      NAME = '     '
      BUF08(1) = '  MAPID '
      CALL GETWRD( 3,KOUNT,CARD,5,5,1,BUF08(1),NAME,ITEST )
      IF( ISTAT.EQ.1 ) THEN
         CONTINUE
      ELSE
         ISTAT = ITEST
      END IF
C
      BUF03 = NAME(1:3)
C
C        2.  TEST FOR DAT STATMENT
C
      IF( BUF03.EQ.'DAT' ) THEN
         READ( NAME(4:5),1000,IOSTAT=IRD4 ) IWORK1(1)
1000     FORMAT( I2 )
C
C         CHECK READ STATUS
C
         IF( IRD4.NE.0 ) THEN
            MESS = BLNK40
            ECODE = 'E02'
            WRITE( MESS,2000 ) NAME,BUF03
2000        FORMAT( 1X,A5,' VIOLATES ',A3,'-CARD FORMAT')
            CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         END IF
C
C        CHECK THAT WE ARE WITHIN LIMITS ON NUMBER OF
C        MAP STATEMENTS OF THIS TYPE
C
         IF( IWORK1(1).GT.OSMRDS ) THEN
            MESS = BLNK40
            ECODE = 'E06'
            WRITE( MESS,3000 ) NAME,BUF03
3000        FORMAT( 1X,A5,' TOO MANY ',A3,'-CARDS' )
            CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
            RETURN
         END IF
C
C        DECIPHER VARIABLE LIST ON REST OF 'CARD'
C
         CALL DATCRD( KOUNT,CARD,ITEST )
         IF( ISTAT.EQ.1 ) THEN
            CONTINUE
         ELSE
            ISTAT = ITEST
         END IF
C
      ELSE
C
         MESS = BLNK40
         ECODE = 'E01'
         WRITE( MESS,4000 ) NAME
4000     FORMAT( 1X,A5,' :WAS GIVEN, MUST HAVE DAT INFO')
         CALL ERROR( KOUNT,CARD,ECODE,LOC,MESS )
         ISTAT = 1
      END IF
C
      IF( ISTAT.EQ.1 ) THEN
         CONTINUE
      ELSE
         ISTAT = 2
      END IF
C
      RETURN
      END


      SUBROUTINE DATCRD( KOUNT,CARD,ISTAT )
C=====================================================================**
C
C        PURPOSE:        THIS ROUTINE PROCESSES THE OS PATHWAY
C                        DEFINITIONS OF THE VARIABLE LIST(S), FOR
C                        MAPID CARDS OF THE 'DAT' TYPE.
C
C
C        LOCAL VARIABLES
C
      CHARACTER CARD*80
      INTEGER  ISTAT,NFLD,NUM,NRDS,ITEST
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'WORK1.INC'
C
C        ISTAT         PROCESS STATUS 1 = ERROR IN PROCESSING
C                                     2 = PROCESSING OK
C        CARD          'IMAGE' WITH MAP DATA ('MAPS' INPUT DATA LIST(S))
C        NFLD          TEMPORARY STORAGE LOCATION FOR NUMBER OF
C                      VARIABLES TO BE DEFINED
C        NUM           NUMBER OF VARIABLES ALREADY DEFINED FOR THIS
C                      SCALAR READ
C        NRDS          TEMPORARY STORAGE LOCATION FOR READ NUMBER
C                      ASSOCIATED WITH THIS MAPID CARD
C
C        INITIALIZE VALUES
C
      PATH = 'OS'
      LOC  = 'DATCRD'
      ISTAT = 0
  
C
C        1.  DEFINE NUMBER OF DATA FIELDS ON 'CARD'
C
      NFLD = IRD3 - 3
      IF( NFLD.LE.0 ) THEN
         ISTAT = 1
         RETURN
      END IF
C
C        2.  DEFINE NUMBER OF VARIABLES ALREADY ASSOCIATED WITH
C            THIS SCALAR READ, AND WILL BE (IF ALL GOES WELL), AND
C            READ NUMBER FOR THIS MAPID CARD
C
      NRDS = IWORK1(1)
      IF( NRDS.GT.OSMRDS ) THEN
         ISTAT = 1
         RETURN
      END IF
C
      NUM = OSDNUM(NRDS)
      OSDNUM(NRDS) = OSDNUM(NRDS) + NFLD
C
C        3.  TEST TOTAL NUMBER NOW ASSOCIATED WITH THIS MAPID CARD
C
      IF( OSDNUM(NRDS).GT.OSMDAT ) THEN
         MESS = BLNK40
         ECODE = 'E06'
         WRITE( MESS,1000 ) NRDS
1000     FORMAT(1X,'VARIABLE LIST TOO LONG FOR DAT-',I2.0)
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
C
C       REDEFINE NUMBER OF VARIABLES TO BE WITHIN
C       ALLOWABLE RANGE
C
         NUM = NUM - (OSDNUM(NRDS)-OSMDAT)
         OSDNUM(NRDS) = OSMDAT
         ISTAT = 1
      END IF
C
C        4.  DECIPHER VARIABLE LIST
C
      DO 200 K=4,3+NFLD
C
C        I       IS THE POSITION IN THE INPUT LIST
C        K       IS THE POSITION IN THE CARD IMAGE
C
         I = NUM + K - 3
C
         WRITE( BUF08(1),1500 ) NRDS,K
1500     FORMAT( 'DAT',I2.0,':',I2.0)
         BUF04(1) = BLNK04
         CALL GETWRD( K,KOUNT,CARD,4,4,1,BUF08(1),BUF04(1),ITEST )
         IF( ISTAT.EQ.1 ) THEN
            CONTINUE
         ELSE
            ISTAT = ITEST
         END IF
C
C        SEARCH FOR MATCH ON VARIABLE NAME
C
         DO 50 J=1,14
C
            IF( BUF04(1).EQ.VNAMES(J) ) THEN
C
               IF( I.LE.OSMDAT ) THEN
                  OSDVAR(NRDS,I,1) = J
                  OSDVAR(NRDS,I,2) = 0
               END IF
               GO TO 200
C
            END IF
C
50       CONTINUE
C
         DO 75 J=30,34
C
            IF( BUF04(1).EQ.VNAMES(J) ) THEN
C
               IF( I.LE.OSMDAT ) THEN
                  OSDVAR(NRDS,I,1) = J
                  OSDVAR(NRDS,I,2) = 0
               END IF
               GO TO 200
C
            END IF
C
75       CONTINUE
C
         DO 100 J=56,60
C
            IF( BUF04(1).EQ.VNAMES(J) ) THEN
C
               IF( I.LE.OSMDAT ) THEN
                  OSDVAR(NRDS,I,1) = J
                  OSDVAR(NRDS,I,2) = 0
               END IF
               GO TO 200
C
            END IF
C
100      CONTINUE
C
C     PARSE VARIABLE NAME INTO PARTS AND TEST THAT WE ARE WITHIN LIMITS
C     ON THE NUMBER OF TOWER LEVELS ALLOWED
C
         BUF04(2) = BLNK04
         BUF04(2) = BUF04(1)(1:2)
         READ( BUF04(1)(3:4),2000,IOSTAT=IRD4 ) IRD5
2000     FORMAT( I2 )
C
C         CHECK READ STATUS
C
         IF( IRD4.NE.0 ) THEN
            MESS = BLNK40
            ECODE = 'E03'
            WRITE( MESS,4000 ) BUF04(1)
            CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
            GO TO 200
         END IF
C
         IF( IRD5.LE.0 .OR. IRD5.GT.OSML ) THEN
            MESS = BLNK40
            ECODE = 'E06'
            WRITE( MESS,3000 ) BUF04(1),IRD5
3000        FORMAT(1X,A4,' LVL INDEX ',I8,' INVALID')
            CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
            GO TO 200
         END IF
C
C        TEST BUF04(2) FOR VARIABLE NAME MATCH
C
         DO 125 J=15,29
C
            IF( BUF04(2).EQ.VNAMES(J) ) THEN
C
               IF( I.LE.OSMDAT ) THEN
                  OSDVAR(NRDS,I,1) = J
                  OSDVAR(NRDS,I,2) = IRD5
               END IF
               GO TO 200
C
            END IF
C
125      CONTINUE
C
C        PROBLEMS, NO MATCH FOUND
C
         MESS = BLNK40
         ECODE = 'E06'
         WRITE( MESS,4000 ) BUF04(1)
4000     FORMAT(1X,A4,' NO MATCH ON VARIABLE NAMES')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
C
200   CONTINUE
C
      IF( ISTAT.EQ.1 ) THEN
         CONTINUE
      ELSE
         ISTAT = 2
      END IF
C
      RETURN
      END


      SUBROUTINE CLMCRD( KOUNT,CARD,ISTAT )
C=====================================================================**
C
C        PURPOSE:        THIS ROUTINE PROCESSES THE OS PATHWAY
C                        DEFINITIONS OF THE THRESHOLD WIND SPEED
C
C
C        LOCAL VARIABLES
C
      CHARACTER CARD*80
      INTEGER  ISTAT
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'WORK1.INC'
C
C        ISTAT           PROCESS STATUS 1 = ERROR IN PROCESSING
C                                       2 = PROCESSING OK
C        CARD            'IMAGE' THRESHOLD WIND SPEED DATA
C
C        INITIALIZE VALUES
C
      PATH = 'OS'
      LOC  = 'CLMCRD'
      ISTAT = 0
C
C        1.  CHECK IRD3.
C
C        CHECK IRD3 (PASSED THROUGH WORK COMMON) AS IT TELLS US
C        THE NUMBER OF 'WORDS' WITHIN THE IMAGE 'CARD'.
C        IN ORDER FOR US TO HAVE A COMPLETE AVG CARD,
C        IRD3 SHOULD BE EQUAL TO 3.  LESS THAN 3 MEANS
C        TOO LITTLE INFORMATION HAS BEEN GIVEN; MORE THAN 3
C        MEANS WE HAVE MORE INFORMATION ON THIS CARD THAN WE
C        SHOULD.
C
      IF( IRD3.NE.3 ) THEN
         ECODE = 'E04'
         MESS = BLNK40
         IF( IRD3.LT.3 ) THEN
            WRITE( MESS,1000 )
1000        FORMAT(1X,'INCOMPLETE INFORMATION ON CLM CARD')
         ELSE
            WRITE( MESS,2000 )
2000        FORMAT(1X,'SUPERFLUOUS DATA ON CLM CARD')
         END IF
C
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      END IF
C
C        2.  CONSTRUCT A FORMAT TO READ THRESHOLD WIND SPEED DATA
      BUF40 = BLNK40
      IWORK1(1) = IC1(3) - 1
      IWORK1(2) = IC2(3) - IC1(3) + 1
C
C        CHECK FIELD WIDTH
C
      IF( IWORK1(2).LE.0 ) THEN
         ECODE = 'E04'
         MESS = BLNK40
         WRITE( MESS,2500 )
2500     FORMAT( 1X,'THRESHOLD SPEED IS MISSING ON CLM CARD ')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      END IF
C
      WRITE( BUF40,3000 ) (IWORK1(J),J=1,2)
3000  FORMAT( '(',I2,'X,F',I3.0,'.0)' )
C
C        3.  ATTEMPT TO READ THRESHOLD WIND SPEED (M/S)
      READ( CARD,BUF40,IOSTAT=IRD4 ) OSCALM
C
C        CHECK READ STATUS
C
      IF( IRD4.NE.0 ) THEN
         ECODE = 'E03'
         MESS = BLNK40
         WRITE( MESS,4000 ) IRD4
4000     FORMAT( 1X,'IOSTAT= ',I8,' READING FOR OSCALM')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         RETURN
      END IF
C
C        4.  CHECK OSCALM (CRUDELY) TO SEE IF REASONABLE
C
      IF( OSCALM.LE.0 .OR. OSCALM.GE.1.0 ) THEN
         MESS = BLNK40
         ECODE = 'W06'
         WRITE( MESS,5000 ) OSCALM
5000     FORMAT(1X,I5,' CHECK INPUT THRESHOLD SPEED')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      END IF
C
      IF( ISTAT.EQ.1 ) THEN
         CONTINUE
      ELSE
         ISTAT = 2
      END IF
C
      RETURN
      END
  
  
      SUBROUTINE SFCCRD( KOUNT,CARD,ISTAT )
C=====================================================================**
C
C   PURPOSE:  Processes the OS pathway definitions of the surface
C             characteristics (albedo, bowen ratio, roughness length)
C             as a function of wind direction sector and month of year.
C
C   Modifications   J. Paumier, PES                    April 28, 1995
C                   Additional site parameters were added: displacement
C                   height, fraction of net radiation absorbed at the
C                   ground, minimum Obukhov length, and anthropogenic
C                   heat flux.
C
C                   J. Paumier, PES                    May 15, 1996
C                   Added leaf area index as a site-specific parameter
C-----------------------------------------------------------------------
C        LOCAL VARIABLES
C
      CHARACTER CARD*80
      REAL      ALBED, BOWEN, ROUGH, XLEAF
      INTEGER   K,ISTAT,IFNDX,ISECT,ITEST, IFREQ
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'WORK1.INC'
C
C        ISTAT    PROCESS STATUS 1 = ERROR IN PROCESSING
C                                2 = PROCESSING OK
C        CARD     'IMAGE' SURFACE CHARACTERISTICS DATA
C        ALBEDO   VALUE READ FOR ALBEDO
C        BOWEN    VALUE READ FOR BOWEN RATIO
C        ROUGH    VALUE READ FOR SURFACE ROUGHNESS LENGTH (M)
C        IFNDX    FREQUENCY INDEX,
C                        IF MONTHLY, THEN IFNDX IS 1 THROUGH 12
C                        IF SEASONAL, THEN IFNDX IS 1 THROUGH 4
C                        (1 = WINTER MONTHS 12,1,2 AND
C                        2 = SPRING MONTHS 3,4,5 ETC)
C                        IF ANNUAL, IFNDX IS IGNORED
C        ISECT    WIND DIRECTION SECTOR, MUST BE LE. OSNWDS VALUE
C        IFREQ    SAVE FREQUENCY (FROM SFC SETUP CARD), SO WE
C                 CAN TEST ITS VALUE WHILE PROCESSING OTHER
C                 SFC CARDS.
C                    0 = NO SETUP CARD SEEN
C                    1 = SETUP CARD SEEN, BUT HAD ERRORS
C                    2 = MONTHLY VALUES
C                    3 = SEASONAL VALUES
C                    4 = ANNUAL VALUES
C
      DATA IFREQ/0/
C
C     INITIALIZE VALUES
C
      PATH = 'OS'
      LOC  = 'SFCCRD'
      ISTAT = 0
C
C==== 1. Check IRD3, the number of 'WORDS' on the record.
C        For OS SFC SETUP, the number of 'words' is 5
C                   SECTORS, the number of 'words' is 6
C                   VALUES, the number of 'words' is NSITCH+5
C                           where NSITCH is defined in OS2.INC
C
      IF( IRD3 .LT. 5 ) THEN
C        Too few parameters
         ECODE = 'E04'
         MESS = BLNK40
         WRITE( MESS,1000 )
1000     FORMAT(' INCOMPLETE INFORMATION ON SFC CARD')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1

      ELSEIF( IRD3 .GT. NSITCH+5 ) THEN
C        Too many parameters
         ECODE = 'E04'
         MESS = BLNK40
         WRITE( MESS,1001 )
1001     FORMAT(' TOO MUCH INFORMATION ON SFC CARD')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      END IF
C
C==== 2.  Fetch the third word on the record (SETUP, VALUE, or SECTOR)
C
      BUF08(1) = ' SFC-CRD'
      BUF08(2) = BLNK08
      CALL GETWRD( 3,KOUNT,CARD,5,8,2,BUF08(1),BUF08(2),ITEST )

Cdbg  print *, ' SFC card 3rd word = ',buf08(2)
      IF( ISTAT.EQ.1 ) THEN
         CONTINUE
      ELSE
         ISTAT = ITEST
      END IF
C
C     Store the first three letters for testing
      BUF03 = BUF08(2)(1:3)
C
C==== 3.  Test for the keyword 'SETUP'
C
      IF( BUF03.EQ.'SET') THEN
C
C        NOTE: the surface 'SETUP' must be defined before all other
C              definitions are made.
C
C------- 3A. User is over-riding internal default values:
C            Initialize the site surface characteristics
C             Index I = frequency
C             Index J = sector
C             Index K = characteristic number
C                       1 = albedo
C                       2 = bowen ratio
C                       3 = roughness length, measurement site
C                       4 = roughness length, application site
C                       5 = minimum Obukhov length, stable atmos.
C                       6 = frac. radiation absorbed by ground
C                       7 = anthropogenic heat flux
C                       8 = leaf area index
C
         OSNWDS = 0
         DO 40 I=1,MAXFRQ
            DO 30 J=1,MAXSEC
               DO 20 K=1,NSITCH
                  OSSFC(I,J,K) = 0.0
20             CONTINUE
30          CONTINUE
40       CONTINUE

C      Initialize the wind direction sector definition
         DO 60 I=1,MAXSEC
            DO 50 J=1,2
               OSWDS(I,J) = 0.0
50          CONTINUE
60       CONTINUE
C
C------- 3B. Fetch the fourth word (ANNUAL, SEASONAL, or MONTHLY)
C
         BUF08(1) = 'SFC-FREQ'
         BUF08(2) = BLNK08
         CALL GETWRD( 4,KOUNT,CARD,6,7,2,BUF08(1),BUF08(2),ITEST )
         IF( ISTAT.EQ.1 ) THEN
            CONTINUE
         ELSE
            ISTAT = ITEST
         END IF
C
         IF( ISTAT .NE. 1 )THEN
            IF( BUF08(2)(1:3).EQ.'MON' ) THEN
               IFREQ = 2
            ELSE IF( BUF08(2)(1:3).EQ.'SEA' ) THEN
               IFREQ = 3
            ELSE IF( BUF08(2)(1:3).EQ.'ANN' ) THEN
               IFREQ = 4
            ELSE
C              Error condition
               ECODE = 'E01'
               MESS = BLNK40
               WRITE( MESS,1500 ) BUF08(2)
1500           FORMAT(1X,A8,' UNKNOWN ACTION FOR SETUP CARD')
               CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
               ISTAT = 1
               IFREQ = 1
            END IF
         ENDIF
C
C------- 3C. Fetch fifth word (new value for OSNWDS)
C
         BUF08(1) = '  OSNWDS'
         BUF02    = '  '
         CALL GETWRD( 5,KOUNT,CARD,1,2,1,BUF08(1),BUF02,ITEST )
         IF( ISTAT.EQ.1 ) THEN
            CONTINUE
         ELSE
            ISTAT = ITEST
         END IF
C
         READ( BUF02,2000,IOSTAT=IRD4 ) IRD5
2000     FORMAT( I2 )
C
C        CHECK IOSTAT
C
         IF( IRD4.NE.0 ) THEN
            ECODE = 'E03'
            MESS = BLNK40
            WRITE( MESS,2500 ) IRD4
2500        FORMAT(1X,'IOSTAT = ',I8,' READING FOR OSNWDS')
            CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1

         ELSE
C           Check value, see if reasonable
            IF( IRD5.LE.0 .OR. IRD5.GT.MAXSEC ) THEN
               ECODE = 'E06'
               MESS = BLNK40
               WRITE( MESS,3000 ) IRD5
3000           FORMAT(1X,I2,' IS AN INVALID VALUE FOR OSNWDS')
               CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
               ISTAT = 1
               IFREQ = 1
            END IF
         ENDIF
C
         IF( ISTAT.EQ.1 ) THEN
            CONTINUE
         ELSE
            ISTAT = 2
            OSNWDS = IRD5
         END IF
C
         RETURN
      END IF
C
C==== 4.  Test for the keyword 'VALUE'
C
      IF( BUF03.EQ.'VAL') THEN
C
C------- 4A.  Check to make sure that 'SETUP' for surface characteristics
C             has been processed without errors.
         IF( IFREQ.EQ.0 ) THEN
            ECODE = 'E12'
            MESS = BLNK40
            WRITE( MESS,3500 )
3500        FORMAT(' ''OS SFC VALUE'' APPEARS BEFORE ''SETUP'' ')
            CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
            OSNWDS = 0
C
         ELSEIF( IFREQ.EQ.1 ) THEN
            ECODE = 'E12'
            MESS = BLNK40
            WRITE( MESS,3600 )
3600        FORMAT(' SFC SETUP CARD HAD ERRORS')
            CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
            OSNWDS = 0
         END IF
C

C------- 4B.  Initialize and decipher values
C
         IFNDX = 0
         ISECT = 0
         ALBED  = ossfc(1,1,1)
         BOWEN  = ossfc(1,1,2)
         ROUGH  = ossfc(1,1,3)
         SFCRLA = ossfc(1,1,4)
         ELMIN  = ossfc(1,1,5)
         CG     = ossfc(1,1,6)
         AHF    = ossfc(1,1,7)
         XLEAF  = ossfc(1,1,8)
         
C        Retrieve the frequency and sector indices and the first 3
C        surface characteristics
         CALL VALCRD( KOUNT,CARD,IFNDX,ISECT,ALBED,BOWEN,ROUGH,ITEST )
         IF( ISTAT.EQ.1 ) THEN
            CONTINUE
         ELSE
            ISTAT = ITEST
         END IF

C        Retrieve the remaining surface characteristics
         CALL ISCCRD( KOUNT,CARD,SFCRLA,CG,ELMIN,AHF,XLEAF, NSITCH,
     &                ITEST )
         IF( ISTAT.EQ.1 ) THEN
            CONTINUE
         ELSE
            ISTAT = ITEST
         END IF

C        Check value of SECT
C
         IF( ISECT.LE.0 ) THEN
            ECODE = 'E06'
            MESS = BLNK40
            WRITE( MESS,4000 ) ISECT
4000        FORMAT(' INVALID # SECTORS: ',I4, ' < 0')
            CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         ELSEIF( ISECT.GT.OSNWDS ) THEN
            ECODE = 'E06'
            MESS = BLNK40
            WRITE( MESS,4001 ) ISECT,OSNWDS
4001        FORMAT(' # SECTORS (',I4, ') EXCEEDS MAX (',I4,')' )
            CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         END IF

C        Check data values
C
         IF( ALBED.LE.0.0 .OR. ALBED.GT.1.0  .OR. BOWEN.LE.0.0  .OR.
     1       ROUGH.LE.0.0 ) THEN
            ECODE = 'E06'
            MESS = BLNK40
            WRITE( MESS,4500 ) ALBED,BOWEN,ROUGH
4500        FORMAT(1X,3F5.2,' INVALID SFC VALUE(S)')
            CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         END IF
C
C        4C. STORE VALUES
C
         IF( IFREQ.EQ.2 ) THEN
C           SETUP card indicates monthly values;
C           check the index on the VALUES image
            IF( IFNDX.LE.0 .OR. IFNDX.GT.MAXFRQ ) THEN
               ECODE = 'E06'
               MESS = BLNK40
               WRITE( MESS,5000 ) IFNDX,BUF08(2)(1:6)
5000           FORMAT(1X,I4,': INVALID FREQ. INDEX FOR ',A6)
               CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
               ISTAT = 1
            END IF
C
            IF( ISTAT.EQ.1 ) THEN
               CONTINUE
            ELSE
               OSSFC(IFNDX,ISECT,1) = ALBED
               OSSFC(IFNDX,ISECT,2) = BOWEN
               OSSFC(IFNDX,ISECT,3) = ROUGH
               OSSFC(IFNDX,ISECT,4) = SFCRLA
               OSSFC(IFNDX,ISECT,5) = ELMIN
               OSSFC(IFNDX,ISECT,6) = CG
               OSSFC(IFNDX,ISECT,7) = AHF
               OSSFC(IFNDX,ISECT,8) = XLEAF
               ISTAT = 2
            END IF
C
            RETURN
C
         ELSE IF( IFREQ.EQ.3 ) THEN
C           SETUP card indicates seasonal values;
C           check the index on the VALUES image
            IF( IFNDX.LE.0 .OR. IFNDX.GT.4 ) THEN
               ECODE = 'E06'
               MESS = BLNK40
               WRITE( MESS,5000 ) IFNDX,BUF08(2)(1:6)
               CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
               ISTAT = 1
            END IF
C
            IF( ISTAT.EQ.1 ) THEN
               CONTINUE

            ELSE
C              Loop on proper months for given 'SEASON'
               IRD4 = 12 + (IFNDX-1)*3
               IRD5 = IRD4 + 2
               DO 70 I=IRD4,IRD5
                  IF( I.GT.12 ) THEN
                     J = I - 12
                  ELSE
                     J = I
                  END IF
                  OSSFC(J,ISECT,1) = ALBED
                  OSSFC(J,ISECT,2) = BOWEN
                  OSSFC(J,ISECT,3) = ROUGH
                  OSSFC(J,ISECT,4) = SFCRLA
                  OSSFC(J,ISECT,5) = ELMIN
                  OSSFC(J,ISECT,6) = CG
                  OSSFC(J,ISECT,7) = AHF
                  OSSFC(J,ISECT,8) = XLEAF
   70          CONTINUE
               ISTAT = 2
            END IF
C
            RETURN
C
         ELSE IF( IFREQ.EQ.4 ) THEN
C           ANNUAL input data; do not have to check IFNDX
C
            IF( ISTAT.EQ.1 ) THEN
               CONTINUE

            ELSE
               DO 80 I=1,12
                  OSSFC(I,ISECT,1) = ALBED
                  OSSFC(I,ISECT,2) = BOWEN
                  OSSFC(I,ISECT,3) = ROUGH
                  OSSFC(I,ISECT,4) = SFCRLA
                  OSSFC(I,ISECT,5) = ELMIN
                  OSSFC(I,ISECT,6) = CG
                  OSSFC(I,ISECT,7) = AHF
                  OSSFC(I,ISECT,8) = XLEAF
80             CONTINUE
C
               ISTAT = 2
            END IF
C
            RETURN
C
         ELSE
C           Must have encountered errors elsewhere: IFREQ is not valid
            ISTAT = 1
            RETURN
         END IF
C
      END IF
C
C==== 5. Check for the keyword 'SECTOR'
C
      IF( BUF03.EQ.'SEC' ) THEN
C
C------- 5A.  Check to make sure that 'SETUP' for surface
C             characteristics has been processed without errors.
C
         IF( IFREQ.EQ.0 ) THEN
C           'SECTOR' card not processed
            ECODE = 'E12'
            MESS = BLNK40
            WRITE( MESS,3500 )
            CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
            OSNWDS = 0
C
         ELSEIF( IFREQ.EQ.1 ) THEN
C           'SECTOR' card processed, but contained errors
            ECODE = 'E12'
            MESS = BLNK40
            WRITE( MESS,3600 )
            CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
            OSNWDS = 0
         END IF
C
C------- 5B.  Decipher sector wind direction values:
C             fetch fourth data field (should be sector index)
C
         BUF08(1) = ' SECT-ID'
         BUF02 = '  '
         CALL GETWRD( 4,KOUNT,CARD,1,2,1,BUF08(1),BUF02,ITEST )
         IF( ISTAT.EQ.1 ) THEN
            CONTINUE
         ELSE
            ISTAT = ITEST
         END IF
C
         READ( BUF02,5500,IOSTAT=IRD4 ) ISECT
5500     FORMAT( I2 )
C
         IF( IRD4.NE.0 ) THEN
            ECODE = 'E03'
            MESS = BLNK40
            WRITE( MESS,6000 ) BUF08(2)
6000        FORMAT(' ERROR READING SECTOR INDEX ON ',A8)
            CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         END IF

C        Check sector index value
C
         IF( ISECT.LE.0 )THEN
C           Invalid value for a sector
            ECODE = 'E06'
            MESS = BLNK40
            WRITE( MESS,6500 ) ISECT
6500        FORMAT(' SECTOR INDEX (', I4, ') < 0')
            CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1

         ELSEIF( ISECT.GT.OSNWDS )THEN
C           Sector number exceeds the value defined on 'SECTOR' card
            ECODE = 'E06'
            MESS = BLNK40
            WRITE( MESS,6501 ) ISECT
6501        FORMAT(' SECTOR INDEX (',I4, ') > # ON SECTOR CARD' )
            CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         END IF

         BUF08(1) = ' WDSTART'
         BUF08(2) = BLNK08
         CALL GETWRD( 5,KOUNT,CARD,1,8,1,BUF08(1),BUF08(2),ITEST )
         IF( ISTAT.EQ.1 ) THEN
            CONTINUE
         ELSE
            ISTAT = ITEST
         END IF
C
         BUF08(1) = ' WDSTOP'
         BUF08(3) = BLNK08
         CALL GETWRD( 6,KOUNT,CARD,1,8,1,BUF08(1),BUF08(3),ITEST )
         IF( ISTAT.EQ.1 ) THEN
            CONTINUE
         ELSE
            ISTAT = ITEST
         END IF
C
C        Check start and stop values of sector
C
         READ( BUF08(2),7000,IOSTAT=IRD4 ) XRD1
7000     FORMAT( F8.0 )
C
C        CHECK IOSTAT
C
         IF( IRD4.NE.0 ) THEN
            ECODE = 'E03'
            MESS = BLNK40
            WRITE( MESS,7500 ) IRD4,ISECT
7500        FORMAT(1X,'IOSTAT= ',I8,' READING WD ',I3,' BOUNDS')
            CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         END IF
C
         READ( BUF08(3),7000,IOSTAT=IRD4) XRD2
C
C        Check IOSTAT
C
         IF( IRD4.NE.0 ) THEN
            ECODE = 'E03'
            MESS = BLNK40
            WRITE( MESS,7500 ) IRD4,ISECT
            CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         END IF
C
         IF( XRD1.LT.0.0 .OR. XRD1.GT.360.0 .OR.
     1    XRD2.LT.0.0 .OR. XRD2.GT.360.0 .OR.
     1    XRD1.EQ.XRD2 ) THEN
            ECODE = 'E06'
            MESS = BLNK40
            WRITE( MESS,8000 ) ISECT,BUF08(2),BUF08(3)
8000        FORMAT(1X,'CHECK WD ',I2,' BOUNDS: ',A8,2X,A8)
            CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         END IF
C
C------- 5C. Store sector definition values
C
         IF( ISTAT.EQ.1 ) THEN
            CONTINUE
         ELSE
C
            OSWDS(ISECT,1) = XRD1
            OSWDS(ISECT,2) = XRD2
            ISTAT = 2
         END IF
C
         RETURN
      END IF
C
C     Error condition: third word was not 'SETUP, VALUE, or SECTOR'
C
      ECODE = 'E01'
      MESS = BLNK40
      WRITE( MESS,8500 ) BUF08(2)
8500  FORMAT(1X,A8,' NOT PROPER SFC-CRD KEYWORD')
      CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
      ISTAT = 1

      RETURN
      END


      SUBROUTINE VALCRD(KOUNT,CARD,FREQ,SECT,ALBED,BOWEN,ROUGH,ISTAT)
C=====================================================================**
C
C         PURPOSE:       PROCESS VALUES FOR ALBEDO, BOWEN RATIO, AND
C                        SURFACE ROUGHNESS LENGTH.
C
C
C        LOCAL VARIABLES
C
      CHARACTER CARD*80
      REAL      ALBED,BOWEN,ROUGH
      INTEGER   ISTAT,FREQ,SECT,ITEST
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'WORK1.INC'
C
C        ISTAT          PROCESS STATUS 1 = ERROR IN PROCESSING
C                                      2 = PROCESSING OK
C        CARD           'IMAGE' SFC CHARACTERISTICS DATA
C        ALBEDO         VALUE READ FOR ALBEDO
C        BOWEN          VALUE READ FOR BOWEN RATIO
C        ROUGH          VALUE READ FOR SURFACE ROUGHNESS LENGTH (M)
C        FREQ           FREQUENCY INDEX,
C                         IF MONTHLY, THEN FREQ IS 1 THROUGH 12
C                         IF SEASONAL, THEN FREQ IS 1 THROUGH 4
C                           1 = WINTER MONTHS 12,1,2
C                           2 = SPRING MONTHS 3,4,5
C                           3 = SUMMER MONTHS 6,7,8
C                           4 = AUTUMN MONTHS 9,10,11
C                         IF ANNUAL, FREQ IS IGNORED
C        SECT           WIND DIRECTION SECTOR, MUST BE LE. OSNWDS VALUE
C
C        INITIALIZE VALUES
C
      PATH = 'OS'
      LOC  = 'VALCRD'
      ISTAT = 0
C
C        1.  STORE OFF SFC VALUES
C
      BUF08(1) = ' FREQ-ID'
      BUF08(2) = BLNK08
      CALL GETWRD( 4,KOUNT,CARD,1,8,1,BUF08(1),BUF08(2),ITEST )
      IF( ISTAT.EQ.1 ) THEN
         CONTINUE
      ELSE
         ISTAT = ITEST
      END IF
C
      BUF08(1) = '   WD-ID'
      BUF08(3) = BLNK08
      CALL GETWRD( 5,KOUNT,CARD,1,8,1,BUF08(1),BUF08(3),ITEST )
      IF( ISTAT.EQ.1 ) THEN
         CONTINUE
      ELSE
         ISTAT = ITEST
      END IF
C
      BUF08(1) = '  ALBEDO'
      BUF08(4) = BLNK08
      CALL GETWRD( 6,KOUNT,CARD,1,8,1,BUF08(1),BUF08(4),ITEST )
      IF( ISTAT.EQ.1 ) THEN
         CONTINUE
      ELSE
         ISTAT = ITEST
      END IF
C
      BUF08(1) = '   BOWEN'
      BUF08(5) = BLNK08
      CALL GETWRD( 7,KOUNT,CARD,1,8,1,BUF08(1),BUF08(5),ITEST)
      IF( ISTAT.EQ.1 ) THEN
         CONTINUE
      ELSE
         ISTAT = ITEST
      END IF
C
      BUF08(1) = '      ZO'
      BUF08(6) = BLNK08
      CALL GETWRD( 8,KOUNT,CARD,1,8,1,BUF08(1),BUF08(6),ITEST )
      IF( ISTAT.EQ.1 ) THEN
         CONTINUE
      ELSE
         ISTAT = ITEST
      END IF
C
C        2.  CONVERT VALUES TO NUMBERS
C
1000  FORMAT( I8 )
2500  FORMAT( F8.0 )

      READ( BUF08(2),1000,IOSTAT=IRD4 ) FREQ
      IF( IRD4.NE.0 ) THEN
         ECODE = 'E03'
         MESS = BLNK40
         WRITE( MESS,1500 ) IRD4
1500     FORMAT(1X,'IOSTAT= ',I8,' READING FOR FREQ')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      END IF
C
      READ( BUF08(3),1000,IOSTAT=IRD4 ) SECT
      IF( IRD4.NE.0 ) THEN
         ECODE = 'E03'
         MESS = BLNK40
         WRITE( MESS,2000 ) IRD4
2000     FORMAT(1X,'IOSTAT= ',I8,' READING FOR SECT')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      END IF
C
      READ( BUF08(4),2500,IOSTAT=IRD4 ) ALBED
      IF( IRD4.NE.0 ) THEN
         ECODE = 'E03'
         MESS = BLNK40
         WRITE( MESS,3000 ) IRD4
3000     FORMAT(1X,'IOSTAT= ',I8,' READING FOR ALBEDO')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      END IF
C
      READ( BUF08(5),2500,IOSTAT=IRD4 ) BOWEN
      IF( IRD4.NE.0 ) THEN
         ECODE = 'E03'
         MESS = BLNK40
         WRITE( MESS,3500 ) IRD4
3500     FORMAT(1X,'IOSTAT= ',I8,' READING FOR BOWEN RATIO')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      END IF
C
      READ( BUF08(6),2500,IOSTAT=IRD4 ) ROUGH
      IF( IRD4.NE.0 ) THEN
         ECODE = 'E03'
         MESS = BLNK40
         WRITE( MESS,4000 ) IRD4
4000     FORMAT(1X,'IOSTAT= ',I8,' READING FOR ROUGHNESS')
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



      SUBROUTINE ISCCRD( KOUNT,CARD,SSPZ02,SSPCG,SSPMOL,SSPQF,SSPLAI,
     &                   NSITCH, ISTAT )
C=====================================================================**
C   Purpose:  Processes the user's specification of site-specific
C             parameters for ISCSTDRY, ISCSTWET.
C             These parameters are:
C              - roughness length, application site (SSPZ02)
C              - radiation absorbed by the ground (SSPCG)
C              - minimum Monin-Obukhov length (SSPMOL)
C              - anthropogenic heat flux (SSPQF)
C              - leaf area index (SSPLAI)
C
C             These values apply for all hours and all days, but do
C             vary by wind direction (through the definition of wind
C             direction sectors) and temporal period (annual, seasonal,
C             or monthly)
C
C   Modifications:
C             J. Paumier, PES                              May 15, 1996
C             Added leaf area index as one of the site-specific values
C-----------------------------------------------------------------------
C     Variable declarations
C
        INTEGER      ISTAT
        CHARACTER*80 CARD
        CHARACTER*10 DUMMY
C
        INCLUDE 'MAIN1.INC'
        INCLUDE 'MAIN2.INC'
        INCLUDE 'MP1.INC'
        INCLUDE 'WORK1.INC'
C
C       Data intialization
C
        PATH = PATHWD(6)
        LOC  = 'ISCCRD'
C
C        CARD     record with processor control information
C        ISTAT    status of processing record
C                    1 = error occurred
C                    2 = all OK
C
        ISTAT = 2
C
C------ Check IRD3 (passed through work common), which contains
C       the number of 'words' within the record.  For this to be a
C       valid 'VALUE' record (for the ISCST3 model), there must
C       be exactly NSITCH+5 fields.  NSITCH is defined in OS2.INC
C       (and is currently set to 8). Fewer than this number means
C       too little information has been given; more than this value
C       means too much information has been provided.  In either
C       case, it is an error condition.

C       NOTE: The number of words should have been checked previously,
C       so the following code is redundant.
C
        IF( IRD3 .LT. NSITCH+5 ) THEN
           ECODE = 'E04'
           MESS = BLNK40
           WRITE( MESS,1000 )
1000       FORMAT(' INSUFFICIENT DATA ON ''VALUE'' CARD')
           CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
           ISTAT = 1
           RETURN
C
        ELSEIF( IRD3.GT.NSITCH+5 ) THEN
           WRITE( MESS,2000 )
2000       FORMAT(' SUPERFLUOUS DATA ON ISC-VALUE CARD')
           CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
           ISTAT = 1
           RETURN

        END IF

C------ Decode each of the parameters; any error returns immediately
C       to the calling program with an error condition; the parameter
C       value is returned as a character string through DUMMY and then
C       converted to a real number through SUBR.STONUM

C------ Roughness length for the application site
        DUMMY = '          '
        IPARM = 9
        CALL GETWRD( 9,KOUNT,CARD, 1,8,1,'  SSPZ02',DUMMY,ISTAT )
        IF( ISTAT .EQ. 1) THEN
           GO TO 7040
        ELSE
           CALL STONUM( DUMMY, 10, SSPZ02, IDUM )
        ENDIF

C------ Minimum Obukhov length for a stable atmosphere
        DUMMY = '          '
        IPARM = 10
        CALL GETWRD( 10,KOUNT,CARD, 1,8,1,'  SSPMOL',DUMMY,ISTAT )
        IF( ISTAT .EQ. 1) THEN
           GO TO 7040
        ELSE
           CALL STONUM( DUMMY, 10, SSPMOL, IDUM )
        ENDIF
        
C------ Fraction of net radiation absorbed by the ground
        DUMMY = '          '
        IPARM = 11
        CALL GETWRD( 11,KOUNT,CARD, 1,8,1,'   SSPCG',DUMMY,ISTAT )
        IF( ISTAT .EQ. 1) THEN
           GO TO 7040
        ELSE
           CALL STONUM( DUMMY, 10, SSPCG, IDUM )
        ENDIF

C------ Anthropogenic heat flux
        DUMMY = '          '
        IPARM = 12
        CALL GETWRD( 12,KOUNT,CARD, 1,8,1,'   SSPQF',DUMMY,ISTAT )
        IF( ISTAT .EQ. 1) THEN
           GO TO 7040
        ELSE
           CALL STONUM( DUMMY, 10, SSPQF, IDUM )
        ENDIF

C------ Leaf area index
        DUMMY = '          '
        IPARM = 13
        CALL GETWRD( 13,KOUNT,CARD, 1,8,1,'  SSPLAI',DUMMY,ISTAT )
        IF( ISTAT .EQ. 1) THEN
           GO TO 7040
        ELSE
           CALL STONUM( DUMMY, 10, SSPLAI, IDUM )
        ENDIF

        RETURN

 7040   WRITE( MESS, 7045) IPARM
 7045   FORMAT( ' ERROR DECODING SITE PARAMETER # ',I2)
        CALL ERROR( 0,PATH,'E40',LOC,MESS )
        ISTAT = 1

        RETURN
        END


      SUBROUTINE FMTCRD( KOUNT,CARD,ISTAT )
C=====================================================================**
C
C   PURPOSE:  Processes the OS pathway definitions of the formats
C             associated with the input data.
C
C-----------------------------------------------------------------------
C     LOCAL VARIABLES
C
      CHARACTER CARD*80
      INTEGER   ISTAT,ITEST
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'WORK1.INC'
C
C        ISTAT          PROCESS STATUS 1 = ERROR IN PROCESSING
C                                      2 = PROCESSING OK
C        CARD           'IMAGE' SURFACE CHARACTERISTICS DATA
C
C        INITIALIZE VALUES
C
      PATH = 'OS'
      LOC  = 'FMTCRD'
      ISTAT = 0
C
C        1.  CHECK IRD3.
C
C        CHECK IRD3 (PASSED THROUGH WORK COMMON) AS IT TELLS US
C        THE NUMBER OF 'WORDS' WITHIN THE IMAGE 'CARD'.
C        IN ORDER FOR US TO HAVE A COMPLETE FMT CARD,
C        IRD3 SHOULD BE AT LEAST EQUAL TO 4.  LESS THAN 4 MEANS
C        TOO LITTLE INFORMATION HAS BEEN GIVEN; MORE THAN 4
C        IS POSSIBLE AND NOT IMMEDIATELY KNOWN TO BE IN ERROR.
C
      IF( IRD3.LT.4 ) THEN
         ECODE = 'E04'
         MESS = BLNK40
1000     FORMAT(1X,'INCOMPLETE INFORMATION ON FMT CARD')
C
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      END IF
C
C          2.  FETCH THE THIRD WORD ON THE CARD (DATXX)
C
      BUF08(1) = '  IDFMT '
      BUF08(2) = BLNK08
      CALL GETWRD( 3,KOUNT,CARD,5,8,2,BUF08(1),BUF08(2),ITEST )
      IF( ISTAT.EQ.1 ) THEN
         CONTINUE
      ELSE
         ISTAT = ITEST
      END IF
C
C        STORE OFF FIRST THREE LETTERS FOR TESTING
C        (NOTE VALUE WAS STORED LEFT JUSTIFIED)
C
      BUF03 = BUF08(2)(1:3)
C
C        2.  TEST FOR 'DAT'
C
      IF( BUF03.EQ.'DAT' ) THEN
C
C        2A.  INTEROGATE 'XX' PORTION OF IDFMT TO SEE WHICH
C             READ THIS FORMAT IS ASSOCIATED WITH.
C
         BUF02 = BUF08(2)(4:5)
         READ( BUF02,2000,IOSTAT=IRD4 ) IRD5
2000     FORMAT( I2 )
C
         IF( IRD4.NE.0 ) THEN
            MESS = BLNK40
            ECODE = 'E03'
            WRITE( MESS,2500 ) IRD4
2500        FORMAT(1X,'IOSTAT= ',I8,' READING DAT-FMT NUMBER')
            CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         END IF
C
C        2B.  CHECK IRD5 TO SEE THAT FMT-NUMBER IS REASONABLE
C
         IF( IRD5.GT.OSMRDS ) THEN
C
            MESS = BLNK40
            ECODE = 'E06'
            WRITE( MESS,3000 ) IRD5
3000        FORMAT(1X,'TOO MANY (DAT) READS: ',I8)
            CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
         END IF
C
C        2C. CHECK FIRST AND LAST CHARACTERS OF FORMAT.
C            THEY ARE SUPPOSED TO BE '(' AND ')' RESPECTIVELY.
C
         IF( CARD( IC1(4):IC1(4) ) .EQ. '(' .AND.
     1    CARD( IC2(IRD3):IC2(IRD3) ) .EQ. ')' ) THEN
C
C        FORMAT APPEARS TO BE OK.  STORE OFF FORMAT.
C
            IF( ISTAT.EQ.1 ) THEN
               CONTINUE
C
            ELSE
               OSFRMT(IRD5) = CARD( IC1(4):IC2(IRD3) )
               ISTAT = 2
            END IF
C
         END IF
C
      ELSE
C
C        3.  IF WE REACH HERE, THEN FIRST THREE LETTERS WERE NOT
C            'DAT'.  USER INPUT ERROR.
C
         MESS = BLNK40
         ECODE = 'E06'
         WRITE( MESS,5500 ) BUF08(2)
5500     FORMAT(1X,A8,' IMPROPER KEYWORD FOR FMT-CARD')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
      END IF
C
      RETURN
      END
