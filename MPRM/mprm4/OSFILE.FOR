        SUBROUTINE OSQA
C---------------------------------------------------------------------**
C   SUBROUTINE OSQA
C
C   PURPOSE:        ROUTINE PROCESSES OS-IQA DATA INTO
C                   OS-OQA DATA FILE.  TWO CHECKS ARE MADE
C                   1) ONLY DATA WITHIN THE EXTRACT DATE
C                   WINDOW ARE OUTPUT TO THE OS-OQA FILE,
C                   AND 2) HOURLY AVERAGES ARE COMPUTED
C                   IF NEED BE.
C
C   CALLED BY: OSPATH
C
C   VERSION DATE:  94148                                                DTBAUG94
C
C-----------------------------------------------------------------------
C
C        LOCAL VARIABLES
C
        INTEGER  OS2YR, OS4YR, CENTURY
        INTEGER NUMBER,NHRS,ISTAT,KEY,QARSLT
        REAL    RMISS,REALUP,REALDN,RVALUE
        CHARACTER*4  NAME
C
        INCLUDE 'MAIN1.INC'
        INCLUDE 'MAIN2.INC'
        INCLUDE 'SF1.INC'
        INCLUDE 'SF2.INC'
        INCLUDE 'OS1.INC'
        INCLUDE 'OS2.INC'
        INCLUDE 'WORK1.INC'
C
C        NUMBER      LAST RECORD PROCESSED FROM OS-IQA FILE
C        NHRS        COUNTER FOR HOURLY DATA VALUES PROCESSED
C        ISTAT       STATUS RETURNED FROM OSNEXT
C                1 = FATAL ERRORS HAVE OCCURED, SET OSSTAT = -1
C                2 = SO FAR SO GOOD
C                3 = EITHER EOF FOUND OR DATA BEYOND EXTRACT
C                    WINDOW.
C
C        KEY,RMISS...NAME ARE USED IN PERFORMING RANGE CHECKS
C
C-----------------------------------------------------------------------
C       INITIALIZE VALUES
C
        PATH = PATHWD(4)
        LOC  = '  OSQA'
        NUMBER = 0
        NHRS   = 1
C
C       CHECK FOR EXTRACT DATES, IF PRESENT, CONVERT START AND
C       STOP EXTRACT DATES TO EQUIVALENTS IN NUMBER OF HOURS
C       SINCE BEGINNING OF 1900.
C
        IF( OSDAY1.GT.0 .AND. OSDAY2.GT.0 ) THEN
           CALL CHROND( PATH,OSYR1,OSDAY1,IWORK1(1200) )
           CALL CHROND( PATH,OSYR2,OSDAY2,IWORK1(1201) )
        END IF
C
C       READ TO END OF HEADERS (IF ANY ARE PRESENT): IQA FILE
        REWIND DEV31
    5   CONTINUE
        BUF03 = '   '
        READ( DEV31,1000,IOSTAT=IRD4,END=10 ) BUF03
1000    FORMAT( A3 )
C
C       CHECK READ STATUS
        IF( IRD4.NE.0 ) THEN
           MESS = BLNK40
           ECODE = 'E50'
           WRITE( MESS,2000 ) IRD4,DEV31
2000       FORMAT(1X,'IOSTAT= ',I8,' PREPPING ',I2)
           CALL ERROR( NUMBER,PATH,ECODE,LOC,MESS )
           GO TO 100
        END IF
C
        IF( BUF03(1:1).EQ.'*' ) THEN
           GO TO 5
        ELSE
           BACKSPACE DEV31
           GO TO 15
        END IF
C
C       READ TO END OF HEADERS (IF ANY ARE PRESENT): OQA FILE
C
   10   IF( BACK40 ) THEN
           BACKSPACE DEV31
        END IF
C
   15   REWIND DEV32
   20   CONTINUE
        BUF03 = '   '
        READ( DEV32,1000,IOSTAT=IRD4,END=30 ) BUF03
C
C       CHECK READ STATUS
           IF( IRD4.NE.0 ) THEN
           MESS = BLNK40
           ECODE = 'E50'
           WRITE( MESS,2000 ) IRD4,DEV32
           CALL ERROR( NUMBER,PATH,ECODE,LOC,MESS )
           GO TO 100
        END IF
C
        IF( BUF03(1:1).EQ.'*' ) THEN
           GO TO 20
        ELSE
           BACKSPACE DEV32
           GO TO 33
        END IF
C
C       PROCESS DATA
   30   IF( BACK40 ) THEN
           BACKSPACE DEV32
        END IF
C
C       INSERT MASTER HEADERS TO OUTPUT QA FILE
   33   WRITE(DEV32,1001) IVDATE                                        DTBAUG94
 1001   FORMAT('*  OS     ON-SITE QUALITY ASSESSMENT, MPRM DATED ', I5) DTBAUG94
C
35      CONTINUE
C
        CALL OSNEXT( NUMBER,NHRS,ISTAT )

C       CHECK ISTAT
C
        IF( ISTAT.EQ.1 ) THEN
           OSSTAT = -1
           RETURN
C
        ELSE
C          QA HOURLY VALUES; SET QARSLT = 5 FOR THE HOURLY QA;
C          COMPUTE THE JULIAN DAY * 100 + HOUR
C
           IWORK1(1210) = OSDAYC*100 + OSGHR


        CALL Y2K(PATH, OSGYR, OS2YR, OS4YR, CENTURY)

        WRITE(*, 1210) OSGMO, OSGDY, OS4YR
 1210   FORMAT('+  Stage 1: QA''ing on-site data for ',
     &             'month-day-year ', 2(I2.2,:'-'),I4)

C
C          FIRST THE SCALAR VARIABLES
           DO 40 I=1,14
              KEY   = SFQA(I,1)
              RMISS  = FLOAT( SFQA(I,2) )
              REALDN = FLOAT( SFQA(I,3) )
              REALUP = FLOAT( SFQA(I,4) )
              RVALUE = OSSOBS(1,I)
              NAME   = VNAMES(I)
              IF( RVALUE.EQ.RMISS ) THEN
                 OSAUD1(I,1) = OSAUD1(I,1) + 1
                 IF(OSSTRA(I) .EQ. 1) THEN
                    MESS = BLNK40
                    WRITE(MESS,450) NAME,OSGYR,OSGMO,OSGDY,OSGHR
                    CALL ERROR(IWORK1(1210),PATH,'   ',LOC,MESS)
  450               FORMAT(1X,A4,' MISSING ON ',I2,3('/',I2.2))
                 ENDIF
              ELSE
                 IF( I.GE.9 .AND. I.LE.11 ) THEN
                    RVALUE = RVALUE*100/( OSUL(I-8)-OSLL(I-8) )
                 END IF
                 QARSLT = 5
                 CALL REALCK( 4,IWORK1(1210),KEY,RMISS,REALDN,REALUP,
     &                        RVALUE,NAME,QARSLT )
                 OSAUD1(I,QARSLT) = OSAUD1(I,QARSLT) + 1
                 IF(QARSLT .EQ. 2 .OR. QARSLT .EQ. 3) THEN
                    MESS = BLNK40
                    WRITE(MESS,451) OSGYR,OSGMO,OSGDY,OSGHR
                    CALL ERROR(IWORK1(1210),PATH,'   ','      ',MESS)
  451               FORMAT(' ON ',I2,3('/',I2.2))
                 ENDIF
              END IF
   40      CONTINUE
C
           DO 50 I=30,34
C
              IF( I.EQ.34 ) THEN
                 KEY   = OSTSKY(1)
                 RMISS  = FLOAT( OSTSKY(2) )
                 REALDN = FLOAT( OSTSKY(3) )
                 REALUP = FLOAT( OSTSKY(4) )
                 ELSE
                 KEY   = SFQA(I,1)
                 RMISS  = FLOAT( SFQA(I,2) )
                 REALDN = FLOAT( SFQA(I,3) )
                 REALUP = FLOAT( SFQA(I,4) )
              END IF
C
              RVALUE = OSSOBS(1,14+I-29)
              NAME   = VNAMES(I)
              QARSLT = 5
              CALL REALCK( 4,IWORK1(1210),KEY,RMISS,REALDN,REALUP,
     &                     RVALUE,NAME,QARSLT)
              IF(QARSLT .EQ. 2 .OR. QARSLT .EQ. 3) THEN
                 MESS = BLNK40
                 WRITE(MESS,451) OSGYR,OSGMO,OSGDY,OSGHR
                 CALL ERROR(IWORK1(1210),PATH,'   ','      ',MESS)
              ENDIF

              IF(QARSLT .EQ. 1 .AND. OSSTRA(I) .EQ. 1) THEN
                 MESS = BLNK40
                 WRITE(MESS,450) NAME,OSGYR,OSGMO,OSGDY,OSGHR
                 CALL ERROR(IWORK1(1210),PATH,'   ',LOC,MESS)
              ENDIF
              OSAUD1(I,QARSLT) = OSAUD1(I,QARSLT) + 1
50         CONTINUE
C
C          THEN THE VECTOR VARIABLES
           DO 70 I=15,29
C
              KEY   = SFQA(I,1)
              RMISS  = FLOAT( SFQA(I,2) )
              REALDN = FLOAT( SFQA(I,3) )
              REALUP = FLOAT( SFQA(I,4) )
              NAME   = VNAMES(I)
C
              DO 60 J=1,OSNL
C
                 RVALUE = OSVOBS(1,J,I-14)
                 QARSLT = 5
                 CALL REALCK( 4,IWORK1(1210),KEY,RMISS,REALDN,REALUP,
     &                        RVALUE,NAME,QARSLT)
                 IF(QARSLT .EQ. 1 .AND. OSSTRA(I) .EQ. 1) THEN
                    MESS = BLNK40
                    WRITE(MESS,452) NAME,OSGYR,OSGMO,OSGDY,OSGHR,J
  452               FORMAT(1X,A4,' MISSING ON ',I2,3('/',I2.2),
     &                           ', LEVEL:',I2)
                    CALL ERROR(IWORK1(1210),PATH,'   ',LOC,MESS)
                 ENDIF
                 IF(QARSLT .EQ. 2 .OR. QARSLT .EQ. 3) THEN
                    MESS = BLNK40
                    WRITE(MESS,453) OSGYR,OSGMO,OSGDY,OSGHR,J
  453               FORMAT(' ON ',I2,3('/',I2.2),', LEVEL:',I2)
                    CALL ERROR(IWORK1(1210),PATH,'   ','      ',MESS)
                 ENDIF
                 OSAUD2(J,I-14,QARSLT) = OSAUD2(J,I-14,QARSLT) + 1
   60         CONTINUE
   70      CONTINUE
C
C          WRITE HOURLY DATA TO OS-OQA FILE
           CALL OSWRTE( NUMBER )
C
C          INCREMENT HOURLY COUNTER
           NHRS = NHRS + 1
C
           IF( ISTAT.EQ.2 ) THEN
              GO TO 35
           ENDIF
C
        ENDIF
C
100     RETURN
        END
C
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
        SUBROUTINE OSNEXT( NUMBER,NHRS,ISTAT )
C
C        PURPOSE:        THIS ROUTINE FETCHES FROM THE OS-IQA FILE
C                        THE NEXT OS OBSERVATION, USING SUBROUTINE
C                        OSFILL.  IT THEN QA'S VALUES, IF
C                        APPROPRIATE.  IT COMPARES THE DATE AND TIME
C                        GROUP OF THESE DATA, TO THE DATE AND TIME
C                        GROUP IT IS 'PROCESSING'.  IF THE BUFFER DATA
C                        IS FOR THE SAME HOUR, IT USES IT.  IF NOT,
C                        WE CHECK TO MAKE SURE IT IS FOR A LATER TIME,
C                        ELSE WE HAVE DATA OUT OF SEQUENCE PROBLEMS.
C                        ASSUMING THE DATA ARE OK, WE COMPUTE THE HOUR'S
C                        AVERAGES, SIGNAL THE CALLING ROUTINE WE ARE
C                        READY TO OUTPUT.
C
C        LOCAL VARIABLES
C
C        *** GROUP 1
        INTEGER NUMBER,ISTAT,RCSTAT,NHRS,JULIAN
C
C       *** GROUP 2
        INTEGER KEY,MISS,UPPER,LOWER,VALUE,QARSLT
        REAL    RMISS,REALDN,REALUP,RVALUE
C  The following statment had been commented out by DTB, but is needed
C  in this routine where NAME=VNAMES is used
        CHARACTER NAME*4                                                DTB94243
C
        INCLUDE 'MAIN1.INC'
        INCLUDE 'MAIN2.INC'
        INCLUDE 'SF1.INC'
        INCLUDE 'SF2.INC'
        INCLUDE 'OS1.INC'
        INCLUDE 'OS2.INC'
        INCLUDE 'WORK1.INC'
C
C        NUMBER  RECORD NUMBER OF FILE
C        NHRS    HOUR WE ARE PROCESSING
C        ISTAT   STATUS OF PROCESS  0 = INITIAL VALUE
C                                   1 = WE HAVE PROBLEMS, FORCE
C                                       OSSTAT TO STOP PROCESSING
C                                   2 = COMPLETED ALL ACTIONS, WITH
C                                       NO FATAL ERRORS
C                                   3 = NO FURTHER DATA TO BE
C                                       PROCESSED (EITHER EOF OR
C                                       REST OF DATA IS PRESUMED TO
C                                       BE BEYOND EXTRACT DATE WINDOW.
C
C       RCSTAT  STATUS RETURNED BY OSFILL
C
C       Group 2 variables are used in passing values for RANGE checks.
C-----------------------------------------------------------------------
C       Initialize values
C
        PATH = PATHWD(4)
        LOC  = 'OSNEXT'

C       Set all values in 'processing' to missing flag values
C
C       First the scalar variables
C
        DO 10 I=1,14
           OSSOBS(1,I) = FLOAT( SFQA(I,2) )
           IWORK1(1400+I) = 0
10      CONTINUE
C
        DO 20 I=30,34
           IF( I.EQ.34 ) THEN
              OSSOBS(1,14+I-29) = FLOAT( OSTSKY(2) )
           ELSE
              OSSOBS(1,14+I-29) = FLOAT( SFQA(I,2) )
           END IF
           IWORK1(1400+I) = 0
20      CONTINUE
C
C       Then the vector variables
C
        DO 40 I=15,29
C
           DO 30 J=1,OSNL
C
              OSVOBS(1,J,I-14) = FLOAT( SFQA(I,2) )
              IWORK2(100+J,I-14) = 0
30         CONTINUE
40      CONTINUE
C
C       Finally the date
C
        OSGDY = SFQA(56,2)
        OSGMO = SFQA(57,2)
        OSGYR = SFQA(58,2)
        OSGHR = SFQA(59,2)
C
C       Test to see this is first time in OSNEXT
C
        IF( NHRS.LE.1 ) GO TO 85
C
C       Swap buffer in for processing - this section of code is
C         processed only once per call to OSNEXT
C
45      OSGDY = IWORK1(1400+56)
        OSGMO = IWORK1(1400+57)
        OSGYR = IWORK1(1400+58)
        OSGHR = IWORK1(1400+59)
C
C       Swap from work array to the OSSOBS or OSVOBS arrays if the
C         observation is not missing.  If the observation is missing,
C         initialize to zero
C
        DO 50 I=1,14
           OSSOBS(1,I) = WORK1(1400+I)
           RMISS = FLOAT( SFQA(I,2) )
           IF( WORK1(1400+I).EQ.RMISS ) THEN
              CONTINUE
           ELSE
              IWORK1(1400+I) = 1
           END IF
50      CONTINUE
C
        DO 60 I=30,34
           IF( I.EQ.34 ) THEN
              RMISS = FLOAT( OSTSKY(2) )
           ELSE
              RMISS = FLOAT( SFQA(I,2) )
           END IF
C
           OSSOBS(1,14+I-29) = WORK1(1400+I)
           IF( WORK1(1400+I).EQ.RMISS ) THEN
              CONTINUE
           ELSE
              IWORK1(1400+I) = 1
           END IF
60      CONTINUE
C
        DO 80 I=15,29
C
         RMISS = FLOAT( SFQA(I,2) )
         DO 70 J=1,OSNL
C
          IF( WORK2(100+J,I-14).EQ.RMISS ) THEN
           OSVOBS(1,J,I-14) = WORK2(100+J,I-14)
           CONTINUE
          ELSE
           IF( (I-14).GE.2 .AND. (I-14).LE.6 ) THEN
            OSVOBS(1,J,I-14) = WORK2(100+J,I-14)*WORK2(100+J,I-14)
           ELSE
            OSVOBS(1,J,I-14) = WORK2(100+J,I-14)
           END IF
C
C     IN ORDER TO PROPERLY COMPUTE THE AVERAGE WIND DIRECTION
C     WE NEED TO SAVE THE VALUE AND COMPARE IT TO THE NEXT
C     VALUE (WHEN WE ARE PROCESSING INTERHOUR AVERAGES.
C
           IF( I .EQ. 22 ) WORK1(100+J) = WORK2(100+J,I-14)
C
C     NOW INCREMENT COUNTER
C
           IWORK2(100+J,I-14) = 1
Cdbg       PRINT *, 'OSNEXT: ',I,WORK2(100+J,I-14),OSVOBS(1,J,I-14),
Cdbg  &              IWORK2(100+J,I-14),RMISS
          END IF
70       CONTINUE
80      CONTINUE
C
C        FILL BUFFER WITH NEXT OBSERVATION
C
  85    CALL OSFILL( 4,NUMBER,DEV31,RCSTAT )
C
C        RCSTAT  RECORD STATUS   0 = FILL TO BUFFER WORKED OK
C                                1 = FILLED BUFFER BUT HAD READ ERRORS
C                                2 = WE HAVE EXCEEDED THE ALLOWABLE
C                                    NUMBER OF READ ERRORS (MAXERR)
C                                3 = END OF FILE ENCOUNTERED SOONER THAN
C                                    EXPECTED.
C                                4 = END OF FILE ENCOUNTERED
C
C        CHECK RCSTAT
C
        IF( RCSTAT.EQ.1 ) THEN
         GO TO 85
        END IF
C
        IF( RCSTAT.EQ.2 .OR. RCSTAT.EQ.3 ) THEN
         ISTAT = 1
         RETURN
        END IF
C
        IF( RCSTAT.EQ.4 ) THEN
         CALL OSHOUR( NHRS,ISTAT )
         ISTAT = 3
         RETURN
        END IF
C
        IF( RCSTAT.EQ.0 ) THEN
C
C        CHECK BUFFER DATE/TIME, IF ANY ELEMENTS ARE MISSING
C        REFILL BUFFER WITH NEW DATA.
C
         DO 90 I=56,59
          IF( IWORK1(1400+I).EQ.SFQA(I,2) ) GO TO 85
90       CONTINUE
C
C          CONVERT CURRENT BUFFER DATE TO NUMBER OF DAYS
C          SINCE 1900.
C
         IWORK1(1202) = JULIAN(IWORK1(1458),IWORK1(1457),IWORK1(1456) )
         CALL CHROND( PATH,IWORK1(1458),IWORK1(1202),IWORK1(1203) )
C
C        CHECK CURRENT BUFFER DATE/TIME AGAINST EXTRACT DATES.
C
        IF( OSDAY1.GT.0 .AND. OSDAY2.GT.0 ) THEN
C
C        IF BUFFER DATA IS EARLIER THAN EXTRACT DATES,
C        GO GET NEXT OS OBSERVATION
C
         IF( IWORK1(1203).LT.IWORK1(1200) ) THEN
          GO TO 85
         END IF
C
C        IF BUFFER DATE IS BEYOND EXTRACT WINDOW, SET ISTAT = 3 AND
C        RETURN.
C
         IF( IWORK1(1203).GT.IWORK1(1201) ) THEN
          CALL OSHOUR ( NHRS,ISTAT )
          ISTAT = 3
          RETURN
         END IF
C
        END IF
C
C      QA CHECK BUFFER VALUES IF 'OSAVG', THE NUMBER OF OBSERVATIONS
C       THAT CONSTITUTE A ONE HOUR AVERAGE, IS GREATER THAN 1
C       COMPUTE JULIAN DAY*100 + HOUR FIRST
C
       IF (OSAVG .GT. 1) THEN
C
        IWORK1(1210) = IWORK1(1202)*100 + IWORK1(1459)
C
C          FIRST THE SCALAR VARIABLES
C
        DO 95 I=1,34
C
         IF( I.EQ.34 ) THEN
          KEY   = OSTSKY(1)
          RMISS  = FLOAT( OSTSKY(2) )
          REALDN = FLOAT( OSTSKY(3) )
          REALUP = FLOAT( OSTSKY(4) )
         ELSE
          KEY   = SFQA(I,1)
          RMISS  = FLOAT( SFQA(I,2) )
          REALDN = FLOAT( SFQA(I,3) )
          REALUP = FLOAT( SFQA(I,4) )
         END IF
C
         RVALUE = WORK1(1400+I)
C
         IF( RVALUE.NE.RMISS ) THEN
          IF( I.GE.9 .AND. I.LE.11 .AND. RVALUE.NE.RMISS ) THEN
           RVALUE = RVALUE*100/( OSUL(I-8)-OSLL(I-8) )
          END IF
          NAME   = VNAMES(I)
          QARSLT = 6
          CALL REALCK( 4,IWORK1(1210),KEY,RMISS,REALDN,REALUP,RVALUE,
     &                 NAME,QARSLT)
         END IF
C
95      CONTINUE
C
C          THEN THE VECTOR VARIABLES
C
        DO 110 I=15,29
C
         KEY   = SFQA(I,1)
         RMISS  = FLOAT( SFQA(I,2) )
         REALDN = FLOAT( SFQA(I,3) )
         REALUP = FLOAT( SFQA(I,4) )
         NAME   = VNAMES(I)
C
         DO 100 J=1,OSNL
C
          RVALUE = WORK2(100+J,I-14)
          QARSLT = 6
          CALL REALCK( 4,IWORK1(1210),KEY,RMISS,REALDN,REALUP,RVALUE,
     &                 NAME,QARSLT)
100      CONTINUE
110     CONTINUE
C
C          FINALLY, THE INTEGER VARIABLES
C
        DO 120 I=56,60
         KEY   = SFQA(I,1)
         MISS  = SFQA(I,2)
         LOWER = SFQA(I,3)
         UPPER = SFQA(I,4)
         VALUE = IWORK1(1400+I)
         NAME  = VNAMES(I)
         QARSLT = 6
         CALL INTECK( 4,IWORK1(1210),KEY,MISS,LOWER,UPPER,VALUE,NAME,
     &                QARSLT)
120     CONTINUE
C
       ENDIF
C
C          COMPARE TIMES
C
       IF( IWORK1(1456).EQ.OSGDY .AND. IWORK1(1457).EQ.OSGMO
     1     .AND. IWORK1(1458).EQ.OSGYR
     2     .AND. IWORK1(1459).EQ.OSGHR ) THEN
C
C          DATA FOR CURRENT PROCESSING HOUR, INCREMENT SUMS
C
        CALL OSSUMS
        GO TO 85
C
       ELSE IF( OSGDY.EQ.FLOAT(SFQA(56,2)) ) THEN
C
C       CURRENT DATE GROUP IS NOT DEFINED, GO TO TOP
C       SWAP BUFFER DATA INTO CURRENT PROCESSING
C       ARRAYS
C
        GO TO 45
C
       ELSE
C
C          NEW HOUR'S DATA IN BUFFER, COMPUTE AVERAGES AND RETURN
C
        CALL OSHOUR( NHRS,ISTAT)
        IF( ISTAT.EQ.1 ) THEN
         CONTINUE
        ELSE
         ISTAT = 2
        END IF
C
C          CHECK BUFFER DATE WITH CURRENT HOUR'S DATE.  INSURE
C          THAT BUFFER DATA IS FOR LATER TIME.  IF NOT PRINT
C          ERROR MESSAGE, SET ISTAT = 1 TO KILL FURTHER
C          PROCESSING.
C
        OSDAYC = JULIAN( OSGYR,OSGMO,OSGDY )
        CALL CHROND( PATH,OSGYR,OSDAYC,IWORK1(1204) )
        IF( (IWORK1(1204)-1)*24+OSGHR   .LE.
     &      (IWORK1(1203)-1)*24+IWORK1(1459) ) THEN
           CONTINUE
        ELSE
           ISTAT = 1
           MESS = BLNK40
           ECODE = 'E54'
           WRITE( MESS,1000 )
1000       FORMAT(1X,'OBSERVATIONS ARE NOT SEQUENCED IN TIME')
           CALL ERROR( NUMBER,PATH,ECODE,LOC,MESS )
           RETURN
        END IF
C
       END IF
C
      END IF
C
      RETURN
      END
C
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C   SUBROUTINE OSSUMS
C
C   PURPOSE:        ROUTINE SUMS BUFFER DATA WITH THAT IN
C                        PROCESSING ARRAYS (IF DATA ARE NOT MISSING)
C                        INCREMENTS SUMS AND RETURNS.
C
C   NOTE:           THE PURPOSE OF SETTING THE ARRAY ELEMENTS TO
C                        ZERO IS TO PROPERLY SUM THE OBSERVED VALUES.
C                        IF THE YOU HAVE MULTIPLE OBSERVATIONS PER HOUR,
C                        AND THE FIRST OBSERVATION IN THE HOUR, WHICH IS
C                        ALREADY SWAPPED INTO OS_OBS, IS MISSING, THE
C                        SUM WILL BE IN ERROR IF OS_OBS IS NOT SET TO 0.
C
C   VERSION DATE:  1 APRIL 1988
C
C   CALLED BY: OSNEXT
C-----------------------------------------------------------------------
        SUBROUTINE OSSUMS
C
        REAL RMISS
        INCLUDE 'SF1.INC'
        INCLUDE 'SF2.INC'
        INCLUDE 'OS1.INC'
        INCLUDE 'OS2.INC'
        INCLUDE 'WORK1.INC'
C
C     FIRST THE SCALAR VARIABLES
C
        DO 10 L=1,14
         IF( WORK1(1400+L) .EQ. FLOAT(SFQA(L,2)) ) THEN
          CONTINUE
         ELSE
          IF(OSSOBS(1,L) .EQ. FLOAT(SFQA(L,2)) ) OSSOBS(1,L) = 0.0
          OSSOBS(1,L) = OSSOBS(1,L) + WORK1(1400+L)
          IWORK1(1400+L) = IWORK1(1400+L) + 1
         END IF
10      CONTINUE
C
        DO 20 L=30,34
         IF( L.EQ.34 ) THEN
          RMISS = FLOAT( OSTSKY(2) )
         ELSE
          RMISS = FLOAT( SFQA(L,2) )
         END IF
C
         IF( WORK1(1400+L) .EQ. RMISS ) THEN
          CONTINUE
         ELSE
          IF(OSSOBS(1,L-15) .EQ. RMISS) OSSOBS(1,L-15) = 0.0
          OSSOBS(1,L-15) = OSSOBS(1,L-15) + WORK1(1400+L)
          IWORK1(1400+L) = IWORK1(1400+L) + 1
         END IF
20      CONTINUE
C
C     THEN THE VECTOR VARIABLES
C
C     LOOP ON LEVELS
C
        DO 40 N=1,OSNL
C
C     FIRST PROCESS WIND SPEED
C
        IF(WORK2(100+N,9) .EQ. FLOAT(SFQA(23,2)) ) THEN
C       WIND SPEED IS MISSING
        CONTINUE
        ELSE
        IF(OSVOBS(1,N,9) .EQ. FLOAT(SFQA(23,2)) ) OSVOBS(1,N,9) = 0.0
C
        IF(WORK2(100+N,9) .LT. OSCALM) THEN
C       WIND LESS THAN THRESHOLD (SET OBS. WS=OSCALM/2; IGNORE WD)
        OSVOBS(1,N,9) = OSVOBS(1,N,9) + OSCALM/2.0
        WORK2(100+N,8) = FLOAT( SFQA(22,2) )
        IWORK2(100+N,9) = IWORK2(100+N,9) + 1
        ELSE
C       WIND EQUAL TO OR ABOVE THRESHOLD
        OSVOBS(1,N,9) = OSVOBS(1,N,9) + WORK2(100+N,9)
        IWORK2(100+N,9) = IWORK2(100+N,9) + 1
        END IF
        END IF
C
C    NEXT PROCESS WIND DIRECTION
C
        IF(WORK2(100+N,8) .EQ. FLOAT(SFQA(22,2)) ) THEN
C       WIND DIRECTION IS MISSING
        CONTINUE
        ELSE
        IF(OSVOBS(1,N,8) .EQ. FLOAT(SFQA(22,2)) ) OSVOBS(1,N,8) = 0.0
C
        IF(IWORK2(100+N,8) .EQ. 0 ) THEN
C       FIRST VALUE FOR HOUR TREATMENT
        OSVOBS(1,N,8) = WORK2(100+N,8)
        IWORK2(100+N,8) = 1
        WORK1(100+N) = WORK2(100+N,8)
        ELSE
C       ANOTHER VALUE FOR THE HOUR ENCOUNTERED
        XRD3 = WORK2(100+N,8) - WORK1(100+N)
        IF(XRD3 .LT. -180.0) THEN
        XRD3 = XRD3 + 360.0
        ELSE IF(XRD3 .GT. 180.0 ) THEN
        XRD3 = XRD3 - 360.0
        END IF
C
        WORK1(100+N) = WORK1(100+N) + XRD3
        OSVOBS(1,N,8) = OSVOBS(1,N,8) + WORK1(100+N)
        IWORK2(100+N,8) = IWORK2(100+N,8) + 1
        END IF
        END IF
C
C       FINALLY PROCESS ALL THE REST, SKIPPING
C       WD AND WS AS WE JUST PROCESSED THESE
C
        DO 30 L=15,29
        IF( L.EQ.22 .OR. L.EQ.23 ) GO TO 30
C
         IF( WORK2(100+N,L-14) .EQ. FLOAT(SFQA(L,2)) ) THEN
          CONTINUE
         ELSE
          IF(OSVOBS(1,N,L-14) .EQ. FLOAT(SFQA(L,2)))OSVOBS(1,N,L-14)=0.0
          IF( (L-14).GE.2 .AND. (L-14).LE.6 ) THEN
           OSVOBS(1,N,L-14) = OSVOBS(1,N,L-14)
     1                       + WORK2(100+N,L-14)*WORK2(100+N,L-14)
           IWORK2(100+N,L-14) = IWORK2(100+N,L-14) + 1
Cdbg        PRINT *, 'OSSUMS',L,WORK2(100+N,L-14),OSVOBS(1,N,L-14),
Cdbg  &              IWORK2(100+N,L-14),FLOAT(SFQA(L,2))
          ELSE
           OSVOBS(1,N,L-14) = OSVOBS(1,N,L-14) + WORK2(100+N,L-14)
           IWORK2(100+N,L-14) = IWORK2(100+N,L-14) + 1
          END IF
         END IF
30      CONTINUE
40      CONTINUE
C
        RETURN
        END
C
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C   SUBROUTINE OSHOUR( NHRS,ISTAT )
C
C   PURPOSE:        ROUTINE COMPUTES HOUR AVERAGES OF DATA
C                        CONTAINED IN PROCESSING ARRAYS.  WE
C                        CHECK THAT THERE ARE SUFFICIENT NUMBER
C                        OF VALUES FOR A VALID AVERAGE.
C
C   VERSION DATE: 1 APRIL 1988
C
C   CALLED BY: OSNEXT
C
C-----------------------------------------------------------------------
        SUBROUTINE OSHOUR( NHRS,ISTAT )
C
C     LOCAL VARIABLES
C
        INTEGER NHRS,ISTAT
        REAL RMISS
C
        INCLUDE 'MAIN1.INC'
        INCLUDE 'MAIN2.INC'
        INCLUDE 'SF1.INC'
        INCLUDE 'SF2.INC'
        INCLUDE 'OS1.INC'
        INCLUDE 'OS2.INC'
        INCLUDE 'WORK1.INC'
C
C        NHRS         HOUR NUMBER, PASSED IN FROM CALLING PROGRAM
C        ISTAT        ALWAYS ZERO, UNLESS WE FIND TOO MANY VALUES
C                     WITHIN AN HOUR, THEN IT IS SET TO 1.
C
C-----------------------------------------------------------------------
C        INITIALIZE VALUES
C
        PATH = PATHWD(4)
        LOC  = 'OSHOUR'
C        FIRST THE SCALAR VARIABLES
C
        DO 10 L=1,14
         IF( OSSOBS(1,L).EQ.FLOAT(SFQA(L,2)) ) THEN
          CONTINUE
         ELSE IF( IWORK1(1400+L).LT.OSMIN ) THEN
          OSSOBS(1,L) = FLOAT( SFQA(L,2) )
         ELSE IF( IWORK1(1400+L).GT.OSAVG ) THEN
          MESS = BLNK40
          ECODE = 'E55'
          WRITE( MESS,1000 ) IWORK1(1400+L),VNAMES(L)
1000      FORMAT(1X,'FOUND ',I8,' VALUES, VARIABLE: ',A4)
          CALL ERROR( NHRS,PATH,ECODE,LOC,MESS )
          ISTAT = 1
          OSSOBS(1,L) = OSSOBS(1,L)/FLOAT( IWORK1(1400+L) )
         ELSE IF( (OSSOBS(1,L).EQ.0.0) .AND. (IWORK1(1400+L).EQ.0) )THEN
          OSSOBS(1,L) = FLOAT(SFQA(L,2))
         ELSE
          OSSOBS(1,L) = OSSOBS(1,L)/FLOAT( IWORK1(1400+L) )
         END IF
10      CONTINUE
C
        DO 20 L=30,34
         IF( L.EQ.34 ) THEN
          RMISS = FLOAT( OSTSKY(2) )
         ELSE
          RMISS = FLOAT( SFQA(L,2) )
         END IF
C
         IF( OSSOBS(1,L-15).EQ.RMISS ) THEN
          CONTINUE
         ELSE IF( IWORK1(1400+L).LT.OSMIN ) THEN
          OSSOBS(1,L-15) = RMISS
         ELSE IF( IWORK1(1400+L).GT.OSAVG ) THEN
          MESS = BLNK40
          ECODE = 'E55'
          WRITE( MESS,1000 ) IWORK1(1400+L),VNAMES(L)
          CALL ERROR( NHRS,PATH,ECODE,LOC,MESS )
          ISTAT = 1
          OSSOBS(1,L-15) = OSSOBS(1,L-15)/FLOAT( IWORK1(1400+L) )
         ELSE IF((OSSOBS(1,L-15).EQ.0.0).AND.(IWORK1(1400+L).EQ.0))THEN
          OSSOBS(1,L-15) = RMISS
         ELSE
          OSSOBS(1,L-15) = OSSOBS(1,L-15)/FLOAT( IWORK1(1400+L) )
         END IF
20      CONTINUE
C
C        THEN THE VECTOR VARIABLES
C
        DO 40 L=15,29
C
         DO 30 N=1,OSNL
          IF( OSVOBS(1,N,L-14).EQ.FLOAT(SFQA(L,2)) ) THEN
           CONTINUE
          ELSE IF( IWORK2(100+N,L-14).LT.OSMIN ) THEN
           OSVOBS(1,N,L-14) = FLOAT( SFQA(L,2) )
          ELSE IF( IWORK2(100+N,L-14).GT.OSAVG ) THEN
           MESS = BLNK40
           ECODE = 'E55'
           WRITE( MESS,1000 ) IWORK2(100+N,L-14),VNAMES(L)
           CALL ERROR( NHRS,PATH,ECODE,LOC,MESS )
           ISTAT = 1
C
           IF( (L-14).GE.2 .AND. (L-14).LE.6 ) THEN
            OSVOBS(1,N,L-14) =
     1       SQRT( OSVOBS(1,N,L-14)/FLOAT( IWORK2(100+N,L-14) ) )
           ELSE
            OSVOBS(1,N,L-14) =
     1             OSVOBS(1,N,L-14)/FLOAT( IWORK2(100+N,L-14) )
           END IF
          ELSE IF( (OSVOBS(1,N,L-14) .EQ. 0.0) .AND.
     1               (IWORK2(100+N,L-14) .EQ. 0) )THEN
           OSVOBS(1,N,L-14) = FLOAT(SFQA(L,2))
          ELSE
           IF( (L-14).GE.2 .AND. (L-14).LE.6 ) THEN
            OSVOBS(1,N,L-14) =
     1       SQRT( OSVOBS(1,N,L-14)/FLOAT( IWORK2(100+N,L-14) ) )
           ELSE
            OSVOBS(1,N,L-14) =
     1             OSVOBS(1,N,L-14)/FLOAT( IWORK2(100+N,L-14) )
           END IF
C
          END IF
C
C  CHECK WD VALUE (NORMALIZE VALUE BTWN 0 AND 360)
C
         IF(L .EQ. 22 ) THEN
         IF(OSVOBS(1,N,8) .NE. FLOAT(SFQA(22,2)) ) THEN
C
25       CONTINUE
C
         IF(OSVOBS(1,N,8) .GT. 360.0 ) THEN
         OSVOBS(1,N,8) = OSVOBS(1,N,8) - 360.0
         ELSE IF(OSVOBS(1,N,8) .LT. 0.0 ) THEN
         OSVOBS(1,N,8) = OSVOBS(1,N,8) + 360.0
         END IF
C
         IF( OSVOBS(1,N,8) .GT. 360.0 .OR.
     1       OSVOBS(1,N,8) .LT.   0.0     ) GO TO 25
C
Cdbg      PRINT *, 'LVL,AVG: ',N,OSVOBS(1,N,8)
         END IF
         END IF
C
30       CONTINUE
40      CONTINUE
C
        RETURN
        END
C
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
        SUBROUTINE OSWRTE( NUMBER )
C
C        PURPOSE:         ROUTINE WRITES DATA STORED IN IST HOUR
C                         POSITION OF OSSOBS AND OSVOBS TO
C                         OS-OQA FILE.
C
C        LOCAL VARIABLES
C
        INTEGER NUMBER,IFLAG
C
        INCLUDE 'MAIN1.INC'
        INCLUDE 'MAIN2.INC'
        INCLUDE 'SF1.INC'
        INCLUDE 'SF2.INC'
        INCLUDE 'OS1.INC'
        INCLUDE 'OS2.INC'
        INCLUDE 'WORK1.INC'
C
C        NUMBER  COUNTER FOR OBSERVATION BEING PROCESSED TO
C                OUTPUT FILE
C        IFLAG   INITIALLY = 0, IF ERRORS OCCUR IN WRITING TO OUTPUT
C                FILE, RESET TO -1.  IF IFLAG = -1, NO MORE ATTEMPTS ARE
C                MADE TO WRITE TO OUTPUT FILE.
C
        DATA IFLAG/0/
C
C        INITIALIZE VALUES
C
        PATH = PATHWD(4)
        LOC  = 'OSWRTE'
C
C        CHECK IFLAG
C
        IF( IFLAG.EQ.-1 ) THEN
        RETURN
        END IF
C
C        1.  SWAP DATA TO BUFFER FOR OUTPUT PROCESSING
C        SWAP BUFFER IN FOR PROCESSING
C
        IWORK1(1300+56) = OSGDY
        IWORK1(1300+57) = OSGMO
        IWORK1(1300+58) = OSGYR
        IWORK1(1300+59) = OSGHR
        IWORK1(1300+60) = SFQA(60,2)
C
        DO 10 I=1,14
        WORK1(1300+I) = OSSOBS(1,I)
10      CONTINUE
C
        DO 20 I=30,34
        WORK1(1300+I) = OSSOBS(1,14+I-29)
20      CONTINUE
C
        DO 40 I=15,29
C
        DO 30 J=1,OSNL
C
        WORK2(150+J,I-14) = OSVOBS(1,J,I-14)
30      CONTINUE
40      CONTINUE
C
C        2.  WRITE FIRST RECORD
C
        IF( OSDNUM(1).EQ.OSTIME) THEN
        WRITE( DEV32,OSFRMT(1),IOSTAT=IRD5 )
     1              ( IWORK1(1300+OSDVAR(1,J,1)),J=1,OSTIME )
        ELSE
        DO 50 J=OSTIME+1,OSDNUM(1)
C
        IF( OSDVAR(1,J,2) .LE. 0 ) THEN
C       SCALAR VARIABLE
        WORK1(J) = WORK1(1300+OSDVAR(1,J,1))
        ELSE
C       VECTOR VARIABLE
        WORK1(J) = WORK2(150+OSDVAR(1,J,2),OSDVAR(1,J,1)-14)
        END IF
C
50      CONTINUE
        WRITE( DEV32,OSFRMT(1),IOSTAT=IRD5 )
     1          ( IWORK1(1300+OSDVAR(1,J,1)),J=1,OSTIME ),
     2          ( WORK1(J),J=OSTIME+1,OSDNUM(1) )
        END IF
C
C        CHECK WRITE STATUS
C
        IF( IRD5.NE.0 ) THEN
C
        MESS = BLNK40
        ECODE = 'E53'
        WRITE( MESS,1000 ) IRD5
1000    FORMAT(1X,'IOSTAT= ',I8,':WRITING OS DATA RECORD')
        CALL ERROR( NUMBER,PATH,ECODE,LOC,MESS )
C
C        RESET IFLAG
C
        IFLAG = -1
        RETURN
C
        END IF
C
C        LOOP ON REST OF RECORDS
C
        DO 60 I=2,OSDCRD
C
        DO 55 J=1,OSDNUM(I)
C
        IF( OSDVAR(I,J,2) .LE. 0 ) THEN
C       SCALAR VARIABLE
        WORK1(J) = WORK1(1300+OSDVAR(I,J,1))
        ELSE
C       VECTOR VARIABLE
        WORK1(J) = WORK2(150+OSDVAR(I,J,2),OSDVAR(I,J,1)-14)
        END IF
C
55      CONTINUE
C
        WRITE( DEV32,OSFRMT(I),IOSTAT=IRD5 )
     1              ( WORK1(J),J=1,OSDNUM(I) )
C
C        CHECK WRITE STATUS
C
        IF( IRD5.NE.0 ) THEN
C
        MESS = BLNK40
        ECODE = 'E53'
        WRITE( MESS,1000 ) IRD5
        CALL ERROR( NUMBER,PATH,ECODE,LOC,MESS )
C
        IFLAG = -1
        RETURN
        END IF
C
60      CONTINUE
C
C        APPARENTLY WE MADE IT!
C
        RETURN
        END
