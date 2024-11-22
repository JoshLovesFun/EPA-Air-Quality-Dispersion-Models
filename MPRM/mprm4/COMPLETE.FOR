      SUBROUTINE UAEXST( ITEST )
C=====================================================================**
C
C     PURPOSE: This routine checks to see if sufficient
C              data are present (and correct) to extract UA data.
C
C-----------------------------------------------------------------------
C     Data Declarations
C
      INTEGER ITEST
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'
C
C     Data initialization
C
      PATH = 'JB'
      LOC  = 'UAEXST'
      ITEST = 0

C     Check for sufficient data by examining the STATUS array; if the
C     STATUS is not equal to 2, then an error was encountered

C     Is there a valid EXTract card?

      IF( STATUS(2,3).NE.2 ) THEN
         MESS = BLNK40
         ECODE = 'W12'
         WRITE( MESS,1000 )
1000     FORMAT(1X,'SUMMARY: MISSING/ERRORS IN UA-EXT CARD')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ITEST = 1
      END IF

C     Is there an output file for 'extracted' data?

      IF( STATUS(2,4).NE.2 ) THEN
         MESS = BLNK40
         ECODE = 'W12'
         WRITE( MESS,2000 )
2000     FORMAT(1X,'SUMMARY: MISSING/ERRORS IN UA-IQA CARD')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ITEST = 1
      END IF

C     Is there valid LOCation data?

      IF( STATUS(2,6).NE.2 ) THEN
         MESS = BLNK40
         ECODE = 'W12'
         WRITE( MESS,3000 )
3000     FORMAT(1X,'SUMMARY: MISSING/ERRORS IN UA-LOC CARD')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ITEST = 1
      END IF

C     Check if the input data records, if any, were processed correctly.
C     First, the upper air soundings

      IF( STATUS(2,8).EQ.1 ) THEN
         MESS = BLNK40
         ECODE = 'E12'
         WRITE( MESS,4000 )
4000     FORMAT(1X,'SUMMARY: ERRORS IN UA-IN1 CARD')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ITEST = 1
      END IF

C     Then the mixing heights
      IF( STATUS(2,9).EQ.1 ) THEN
         MESS = BLNK40
         ECODE = 'E12'
         WRITE( MESS,5000 )
5000     FORMAT(1X,'SUMMARY: ERRORS IN UA-IN2 CARD')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ITEST = 1
      END IF
C
C     If neither were specified, then the data are missing
      IF( STATUS(2,8).EQ.0 .AND. STATUS(2,9).EQ.0 ) THEN
         MESS = BLNK40
         ECODE = 'E12'
         WRITE( MESS,6000 )
6000     FORMAT(1X,'SUMMARY: MISSING UA INPUT DATA')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ITEST = 1
      END IF
C
      RETURN
      END


      SUBROUTINE UAQAST( ITEST )
C=====================================================================**
C
C     PURPOSE:  This routine checks to see if sufficient
C               data are present (and correct) to qa UA data.
C
C-----------------------------------------------------------------------
C     LOCAL VARIABLE
C
      INTEGER ITEST
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'
C
C     INITIALIZE VALUES
C
      PATH = 'JB'
      LOC  = 'UAQAST'
      ITEST = 0
C
C     HAVE WE THE QA OUTPUT FILE
C
      IF( STATUS(2,5).EQ.2 ) THEN
         CONTINUE
      ELSE
         MESS = BLNK40
         ECODE = 'W12'
         WRITE( MESS,1000 )
1000     FORMAT(1X,'SUMMARY: MISSING/ERRORS IN UA-OQA CARD')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ITEST = 1
      END IF
C
C     MAKE SURE WE HAVE VALID LOCATION DATA
C
      IF( STATUS(2,6).EQ.2 ) THEN
         CONTINUE
      ELSE
         MESS = BLNK40
         ECODE = 'W12'
         WRITE( MESS,2000 )
2000     FORMAT(1X,'SUMMARY: MISSING/ERRORS IN UA-LOC CARD')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ITEST = 1
      END IF
C
C     MAKE SURE WE HAVE QA INPUT DATA FILE
C
      IF( STATUS(2,4).EQ.2 ) THEN
         CONTINUE
      ELSE
         MESS = BLNK40
         ECODE = 'E12'
         WRITE( MESS,3000 )
3000     FORMAT(1X,'SUMMARY: ERRORS IN UA-INQ CARD')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ITEST = 1
      END IF
C
      RETURN
      END


      SUBROUTINE SFEXST( ITEST )
C=====================================================================**
C
C     Purpose:  This routine checks to see if sufficient
C               data are present (and correct) to extract SF data.
C
C     Called by: SUBR.TEST
C
C
C     Modified by:  J. Paumier, Pacific Environmental Services
C         2/13/95:  Added status checks for the new formats (TD3280,
C                   SAMSON, TD3240) and a check for valid combinations
C                   of all the SF pathway formats; general cleanup
C-----------------------------------------------------------------------
C     Data declarations

      INTEGER  ITEST
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'WORK1.INC'

C     Data initialization

      PATH = 'JB'
      LOC  = 'SFEXST'
      ITEST = 0

C     Check for sufficient data by examining the STATUS array; if the
C     STATUS is not equal to 2, then an error was encountered

C     Is there a valid EXTract card?

      IF( STATUS(3,3).NE.2 ) THEN
         MESS = BLNK40
         ECODE = 'W12'
         WRITE( MESS,1000 )
1000     FORMAT(' SUMMARY: MISSING/ERRORS IN SF-EXT CARD')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ITEST = 1
      END IF

C     Is there an output file for 'extracted' data?

      IF( STATUS(3,4).NE.2 ) THEN
         MESS = BLNK40
         ECODE = 'W12'
         WRITE( MESS,2000 )
2000     FORMAT(' SUMMARY: MISSING/ERRORS IN SF-IQA CARD')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ITEST = 1
      END IF

C     Is there valid LOCation data?

      IF( STATUS(3,6).NE.2 ) THEN
         MESS = BLNK40
         ECODE = 'W12'
         WRITE( MESS,3000 )
3000     FORMAT(' SUMMARY: MISSING/ERRORS IN SF-LOC CARD')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ITEST = 1
      END IF

C     Check if the input data records, if any, were processed correctly.
C
C     First, the IN2 record: CD-144 & SCRAM & SAMSON
      IF( STATUS(3,7).EQ.1 ) THEN
         MESS = BLNK40
         ECODE = 'E12'
         WRITE( MESS,4020 )
4020     FORMAT(' SUMMARY: ERRORS IN SF-IN2 CARD')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ITEST = 1
      END IF

C     Second, the IN1 record: TD-3280
      IF( STATUS(3,11).EQ.1 ) THEN
         MESS = BLNK40
         ECODE = 'E12'
         WRITE( MESS,4010 )
4010     FORMAT(' SUMMARY: ERRORS IN SF-IN1 CARD')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ITEST = 1
      END IF

C     Third, the IN3 record: TD-3240
      IF( STATUS(3,12).EQ.1 ) THEN
         MESS = BLNK40
         ECODE = 'E12'
         WRITE( MESS,4030 )
4030     FORMAT(' SUMMARY: ERRORS IN SF-IN3 CARD')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ITEST = 1
      END IF

C     Check that there are hourly weather obs (STATUS(3,7) or STATUS(3,11))
C     when precip data are specified (STATUS(3,12))
C     NOTE:  STATUS(3,11) refers to TD-3280, which has not been implemented
      IF( (STATUS(3,12) .EQ. 2)  .AND.  (STATUS(3,7) .EQ. 0)  .AND.
     &    (STATUS(3,11) .EQ. 0) )THEN
         MESS = BLNK40
         ECODE = 'E12'
         WRITE( MESS,5000 )
5000     FORMAT(' SUMMARY: CANNOT HAVE PRECIP DATA ALONE')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ITEST = 1
      END IF

C     If there are both hourly weather obs and precip, verify valid
C     combinations, as follows:
C       CD-144 and TD3240: valid
C       SCRAM and TD3240 : not valid
C       SAMSON and TD3240: valid
C       TD3280 and TD3240: valid  (TD3280 not implemented)
C
      IF( INDEX(SFFMT,'SCRAM') .NE. 0  .AND.
     &    INDEX(PPTFMT,'3240') .NE. 0 ) THEN

C        The combination of files is not valid
         MESS = BLNK40
         ECODE = 'E15'
         WRITE( MESS,5050 )
5050     FORMAT(' SUMMARY: CANNOT USE SCRAM & TD-3240')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ITEST = 1
      ENDIF

      RETURN
      END
C


      SUBROUTINE SFQAST( ITEST )
C=====================================================================**
C
C     PURPOSE:  This routine checks to see if sufficient
C               data are present (and correct) to qa SF data.
C
C-----------------------------------------------------------------------
C     LOCAL VARIABLE
C
      INTEGER ITEST
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'
C
C     INITIALIZE VALUES
C
      PATH = 'JB'
      LOC  = 'SFQAST'
      ITEST = 0
C
C     HAVE WE THE QA OUTPUT FILE
C
      IF( STATUS(3,5).EQ.2 ) THEN
         CONTINUE
      ELSE
         MESS = BLNK40
         ECODE = 'W12'
         WRITE( MESS,1000 )
1000     FORMAT(1X,'SUMMARY: MISSING/ERRORS IN SF-OQA CARD')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ITEST = 1
      END IF
C
C     MAKE SURE WE HAVE VALID LOCATION DATA
C
      IF( STATUS(3,6).EQ.2 ) THEN
         CONTINUE
      ELSE
         MESS = BLNK40
         ECODE = 'W12'
         WRITE( MESS,2000 )
2000     FORMAT(1X,'SUMMARY: MISSING/ERRORS IN SF-LOC CARD')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ITEST = 1
      END IF
C
C     MAKE SURE WE HAVE QA INPUT DATA FILE
C
      IF( STATUS(3,4).EQ.2 ) THEN
         CONTINUE
      ELSE
         MESS = BLNK40
         ECODE = 'E12'
         WRITE( MESS,3000 )
3000     FORMAT(1X,'SUMMARY: ERRORS IN SF-INQ CARD')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ITEST = 1
      END IF
C
      RETURN
      END


      SUBROUTINE OSEXST( ITEST )
C=====================================================================**
C
C     PURPOSE:   Checks to see if sufficient data are present (and
C                correct) to attempt to extract OS data.
C
C-----------------------------------------------------------------------
C     LOCAL VARIABLE
C
      INTEGER ITEST
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'WORK1.INC'
C
C     INITIALIZE VALUES
C
      PATH = 'JB'
      LOC  = 'OSEXST'
      ITEST = 0
C
C     HAVE WE A VALID EXTRACT CARD
C
      IF( STATUS(4,3).EQ.2 ) THEN
         CONTINUE
      ELSE
         MESS = BLNK40
         ECODE = 'W12'
         WRITE( MESS,1000 )
1000     FORMAT(1X,'SUMMARY: MISSING/ERRORS IN OS-EXT CARD')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ITEST = 1
      END IF
C
C     HAVE WE AN OUTPUT FILE FOR 'EXTRACTED' DATA
C
      IF( STATUS(4,4).EQ.2 ) THEN
         CONTINUE
      ELSE
         MESS = BLNK40
         ECODE = 'E12'
         WRITE( MESS,2000 )
2000     FORMAT(1X,'SUMMARY: MISSING/ERRORS IN OS-IQA CARD')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ITEST = 1
      END IF
C
C     HAVE WE A VALID LOCATION DATA
C
      IF( STATUS(4,6).EQ.2 ) THEN
         CONTINUE
      ELSE
         MESS = BLNK40
         ECODE = 'W12'
         WRITE( MESS,3000 )
3000     FORMAT(1X,'SUMMARY: MISSING/ERRORS IN OS-LOC CARD')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ITEST = 1
      END IF
C
C     HAVE WE INPUT DATA
C
      IF( OSDCRD.EQ.0 ) THEN
         MESS = BLNK40
         WRITE( MESS,4000 )
4000     FORMAT(1X,'SUMMARY: NO FMT/MAP DEFINITIONS')
         ECODE = 'E13'
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ITEST = 1
      END IF
C
      RETURN
      END
C
      SUBROUTINE OSQAST( ITEST )
C=====================================================================**
C
C     PURPOSE:   Checks to see if sufficient data are present (and
C                correct) to attempt to qa OS data.
C
C-----------------------------------------------------------------------
C     LOCAL VARIABLE
C
      INTEGER ITEST
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'WORK1.INC'
C
C     INITIALIZE VALUES
C
      PATH = 'JB'
      LOC  = 'OSQAST'
      ITEST = 0
C
C     HAVE WE THE QA OUTPUT FILE
C
      IF( STATUS(4,5).EQ.2 ) THEN
         CONTINUE
      ELSE
         MESS = BLNK40
         ECODE = 'W12'
         WRITE( MESS,1000 )
1000     FORMAT(1X,'SUMMARY: MISSING/ERRORS IN OS-OQA CARD')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ITEST = 1
      END IF
C
C     MAKE SURE WE HAVE VALID LOCATION DATA
C
      IF( STATUS(4,6).EQ.2 ) THEN
         CONTINUE
      ELSE
         MESS = BLNK40
         ECODE = 'W12'
         WRITE( MESS,2000 )
2000     FORMAT(1X,'SUMMARY: MISSING/ERRORS IN OS-LOC CARD')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ITEST = 1
      END IF
C
C     MAKE SURE WE HAVE QA INPUT DATA FILE
C
      IF( STATUS(4,4).EQ.2 ) THEN
         CONTINUE
      ELSE
         MESS = BLNK40
         ECODE = 'E12'
         WRITE( MESS,3000 )
3000     FORMAT(1X,'SUMMARY: ERRORS IN OS-INQ CARD')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ITEST = 1
      END IF
C
C     HAVE WE INPUT DATA
C
      IF( OSDCRD.EQ.0 ) THEN
         MESS = BLNK40
         WRITE( MESS,4000 )
4000     FORMAT(1X,'SUMMARY: NO FMT/MAP DEFINITIONS')
         ECODE = 'E13'
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ITEST = 1
      END IF
C
      RETURN
      END
C


      SUBROUTINE TEST
C=====================================================================**
C
C     PURPOSE:  This routine processes the STATUS array to determine
C               what input (without errors) is available on each
C               pathway and sets the pathway status word accordingly.
C
C-----------------------------------------------------------------------
C     LOCAL VARIABLES
C
      INTEGER ISTAT
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'WORK1.INC'
C
C     INITIALIZE VALUES
C
      PATH = 'JB'
      LOC  = '  TEST'
C
C     PROCESS 'JOB' PATHWAY
C
      JBSTAT = 0
      ISTAT  = 0
C
      DO 10 I=1,NUMKEY(1)
C
         IF( STATUS(1,I).EQ.1 ) THEN
            ISTAT = ISTAT + 1
         END IF
C
10    CONTINUE
C
      IF( ISTAT.GT.0 ) THEN
         MESS = BLNK40
         ECODE = 'E11'
         WRITE( MESS,1000 ) ISTAT, PATHWD(1)
1000     FORMAT(1X,'HAVE DETECTED ',I3,' ERRORS ON PATHWAY-',A2)
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         JBSTAT = -1
         ISTAT = 1
      END IF
C
C     MAKE SURE WE HAVE OPENED AN ERROR REPORT FILE
C
      IF( STATUS(1,5).EQ.2 ) THEN
         CONTINUE
      ELSE
         MESS = BLNK40
         ECODE = 'E07'
         WRITE( MESS,1500 )
1500     FORMAT(1X,'SUMMARY: NO ACCESS TO AN ERROR FILE')
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         JBSTAT = -1
      END IF
C
      IF(ISTAT.NE.0) THEN
         CONTINUE
      ELSE
         JBSTAT = 1
      END IF
C
C     PROCESS PATHWAYS UA, SF AND OS
C
      DO 100 J=2,4
C
C        INITIALIZE PATHWAY'S STATUS
C
         IF( J.EQ.2 ) THEN
            UASTAT = 0
         ELSE IF( J.EQ.3 ) THEN
            SFSTAT = 0
         ELSE
            OSSTAT = 0
         END IF
C
C        INITIALIZE ERROR STATUS INDICATOR
C
         ISTAT  = 0
C
         DO 20 I=1,NUMKEY(J)
C
C           LOOK FOR ANY ERRORS ANYWHERE ON THIS PATHWAY
C
            IF( STATUS(J,I).EQ.1 ) THEN
               ISTAT = ISTAT + 1
            END IF
C
20       CONTINUE
C
         IF( ISTAT.GT.0 ) THEN
            MESS = BLNK40
            ECODE = 'E11'
            WRITE( MESS,1000 ) ISTAT, PATHWD(J)
            CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
            ISTAT = 1
C
            IF( J.EQ.2 ) THEN
               UASTAT = -1
               JBSTAT = -1
            ELSE IF( J.EQ.3 ) THEN
               SFSTAT = -1
               JBSTAT = -1
            ELSE
               OSSTAT = -1
               JBSTAT = -1
            END IF
C
         END IF
C
C        FOR OS-PATHWAY CALL ROUTINE TO PERFORM
C        VARIOUS CHECKS TO INSURE USER HAS
C        FORMATS FOR ALL READS, ETC.
C
         IF( J.EQ.4 ) THEN
            ISTAT = 0
            CALL OSTEST( ISTAT )
            IF( ISTAT.EQ.1 ) THEN
               OSSTAT = -1
               JBSTAT = -1
            END IF
         END IF
C
         IF( ISTAT.EQ.1 ) THEN
            CONTINUE
         ELSE
C
C           SEE IF EXTRACT IS POSSIBLE ON THIS PATHWAY
C
            IF( STATUS(J,3).EQ.0 ) THEN
C              NO EXTRACT CARD.  WE ASSUME NO EXTRACT CALLED FOR
               MESS = BLNK40
               ECODE = 'I10'
               WRITE( MESS,2000 ) PATHWD(J)
2000           FORMAT(1X,'SUMMARY: NO ',A2,'-EXT CARD, NULL EXTRACT')
               CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
               CONTINUE
C
            ELSE
C
C              MUST WANT TO EXTRACT - TEST EXTRACT SETUP
C
C
               IF( J.EQ.2 ) THEN
                  CALL UAEXST( ISTAT )
               ELSE IF( J.EQ.3 ) THEN
                  CALL SFEXST( ISTAT )
               ELSE
                  CALL OSEXST( ISTAT )
               END IF
C
               IF( ISTAT.EQ.1 ) THEN
C
                  IF( J.EQ.2 ) THEN
                     UASTAT = -1
                     JBSTAT = -1
                  ELSE IF( J.EQ.3 ) THEN
                     SFSTAT = -1
                     JBSTAT = -1
                  ELSE
                     OSSTAT = -1
                     JBSTAT = -1
                  END IF
C
               ELSE
C
                  IF( J.EQ.2 ) THEN
                     UASTAT = 1
                  ELSE IF( J.EQ.3 ) THEN
                     SFSTAT = 1
                  ELSE
                     OSSTAT = 1
                  END IF
C
               END IF
C
            END IF
C
         END IF
C
         IF( ISTAT.EQ.1 ) THEN
            CONTINUE
         ELSE
C
C           SEE IF QA CALLED FOR ON THIS PATHWAY
C
            IF( STATUS(J,4).EQ.0 ) THEN
C              NO INPUT FILE AVIALABLE FOR QA
               MESS = BLNK40
               ECODE = 'I11'
               WRITE( MESS,2500 ) PATHWD(J)
2500           FORMAT(1X,'SUMMARY: NO ',A2,'-IQA CARD, NULL QA ')
               CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
               CONTINUE
C
            ELSE
C
C              MUST WANT TO QA - TEST QA SETUP
C
               IF( J.EQ.2 ) THEN
                  CALL UAQAST( ISTAT )
               ELSE IF( J.EQ.3 ) THEN
                  CALL SFQAST( ISTAT )
               ELSE
                  CALL OSQAST( ISTAT )
               END IF
C
               IF( ISTAT.EQ.1 ) THEN
C
                  IF( J.EQ.4 ) THEN
                     OSSTAT = -1
                     JBSTAT = -1
                  END IF
C
               ELSE
C
                  IF( J.EQ.2 ) THEN
                     UASTAT = UASTAT + 2
                  ELSE IF( J.EQ.3 ) THEN
                     SFSTAT = SFSTAT + 2
                  ELSE
                     OSSTAT = OSSTAT + 2
                  END IF
C
               END IF
C
            END IF
C
         END IF
C
         IF( ISTAT.EQ.1 ) THEN
            CONTINUE
         ELSE
C
C           SEE IF MERGE POSSIBLE ON THIS PATHWAY
C
            IF( STATUS(J,5).EQ.0 ) THEN
C              NO INPUT FILE AVAILABLE FOR MERGE
               MESS = BLNK40
               ECODE = 'I12'
               WRITE( MESS,3000 ) PATHWD(J)
3000           FORMAT(1X,'SUMMARY: NO ',A2,'-OQA CARD, NULL MERGE ')
               CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
               CONTINUE
            ELSE
C
               IF( STATUS(J,5).EQ.2 ) THEN
C
C                 MERGE IS POSSIBLE
C
                  IF( J.EQ.2 ) THEN
                     UASTAT = UASTAT + 4
                  ELSE IF( J.EQ.3 ) THEN
                     SFSTAT = SFSTAT + 4
                  ELSE
C                    CHECK THAT WE HAVE DEFINED THE DATA STRUCTURE (FMT/DAT INPUT)
                     IF( OSDCRD        .EQ.0 ) THEN
                        MESS = BLNK40
                        WRITE( MESS,3250 )
3250                    FORMAT(1X,'SUMMARY: NO FMT/MAP DEFINITIONS')
                        ECODE = 'E13'
                        CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
                        ISTAT = 1
                        JBSTAT = -1
                        OSSTAT = -1
                     ELSE
C
                        OSSTAT = OSSTAT + 4
                     END IF
C
                  END IF
C
               END IF
C
               IF( STATUS(J,5).NE.2 ) THEN
C
C                 MERGE NOT POSSIBLE (ERROR IN OQA CARD)
C
                  MESS = BLNK40
                  ECODE = 'E12'
                  WRITE( MESS,3500 ) PATHWD(J)
3500              FORMAT(1X,'SUMMARY: ERRORS IN ',A2,'-OQA CARD')
                  CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
                  ISTAT = 1
C
               END IF
C
            END IF
C
         END IF
C
100   CONTINUE
C
C     WE HAVE LOOPED THROUGH ALL PATHS BUT MERGE
C     CHECK MERGE PATHWAY FOR PROBLEMS.
C
C     INITIALIZE ERROR STATUS INDICATOR
C
      ISTAT  = 0
C
      DO 200 I=1,NUMKEY(5)
C
C        LOOK FOR ANY ERRORS ANYWHERE ON MERGE PATHWAY
C
         IF( STATUS(5,I).EQ.1 ) THEN
            ISTAT = ISTAT + 1
         END IF
C
200   CONTINUE
C
      IF( ISTAT.GT.0 ) THEN
         MESS = BLNK40
         ECODE = 'E11'
         WRITE( MESS,1000 ) ISTAT, PATHWD(5)
         CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
         ISTAT = 1
         JBSTAT = -1
C
      END IF
C
      IF( ISTAT.EQ.1 ) THEN
         CONTINUE
      ELSE
C
C        SEE IF MERGE OUTPUT DATA FILE IS PRESENT,
C        AND IF NOT, REDUCE ANY PATHWAY SET FOR MERGE
C        BY FOUR (4) TO AVOID MERGE.
C
         IF( STATUS(5,4).GT.1 )THEN
C           MAKE SURE WE HAVE AN OS LOC CARD WHEN WE GO TO MERGE
C
            IF( STATUS(4,6).NE.2 ) THEN
C
C              MERGE NOT POSSIBLE (ERRORS IN OS LOC CARD)
C
               MESS = BLNK40
               ECODE = 'W12'
               WRITE( MESS,4000 )
4000           FORMAT(1X,'SUMMARY: ERROR/OR MISSING OS-LOC CARD')
               CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
               ISTAT = 1
C
            END IF
C
         ELSE
C
            IF( UASTAT.GE.4 ) UASTAT = UASTAT - 4
            IF( SFSTAT.GE.4 ) SFSTAT = SFSTAT - 4
            IF( OSSTAT.GE.4 ) OSSTAT = OSSTAT - 4
         END IF
  
      END IF
C
      RETURN
      END
C
      SUBROUTINE OSTEST( ITEST )
C=====================================================================**
C
C     PURPOSE:  This routine checks the STATUS array, looking for
C               inconsistencies in the user specified OS-input
C               setup data.
C
C-----------------------------------------------------------------------
C     LOCAL VARIABLES
C
      INTEGER ITEST,NUMBER,HMIN,HMAX
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'WORK1.INC'
C
C     ITEST   STATUS OF COMPLETENESS CHECKS,
C     0 MEANS ALL OK,
C     1 MEANS WE FOUND AN INCONSISTANCY.
C     NUMBER  A DUMMUY COUNTER INDEX USED FOR
C     THOSE CALLS REQUIRING A COUNTER
C     BUT MAKES NO SENSE TO THIS
C     PROCESS.
C     HMIN    MINIMUM LEVEL INDEX FOUND
C     HMAX    MAXIMUM LEVEL INDEX FOUND
C
C     INITIALIZE VALUES
C
      PATH = 'JB'
      LOC  = 'OSTEST'
      NUMBER = 0
      HMIN = 100
      HMAX = 0
      OSTIME = 0
      DO 5 N=1,250
         IWORK1(N) = 0
5     CONTINUE

C     0. Check for completeness of the surface characteristics

      IF( STATUS(4,16).LT.0 ) THEN

         ITEST = 1

      END IF

C
C     1.  SEE IF USER SPECIFIED THE DATA MAP AND
C     FORMATS FOR READING THE OS DATA.
C
      IF( STATUS(4,14).EQ.0 .AND. STATUS(4,15).EQ.0 ) THEN
C
C        NEITHER DATA MAP OF VARIABLES OR FORMATS ARE PROVIDED.
C        THIS IS ALLOWABLE IF ALL THE USER WISHES TO DO IS
C        US THE OS-PATHWAY TO DEFINE THE 'LOCATION'.  SO FOR NOW
C        WE WILL ASSUME WE HAVE NO PROBLEMS.
C
C        HOWEVER, WE MAY RETHINK THIS AT A LATER TIME, SO WE WILL
C        LEAVE THIS IF-THEN BLOCK HERE FOR POSSIBLE LATER USE.
C
         RETURN
      END IF
C
C     2.  SEE IF FORMATS WERE GIVEN BUT NO DATA MAP.
C
      IF( STATUS(4,14).EQ.0 .AND. STATUS(4,15).NE.0 ) THEN
C
C        ERROR, DATA MAP NOT GIVEN BUT FORMATS PROVIDED.
C        WE WILL HAVE TO ASSUME, USER FORGOT TO GIVE THE
C        DATA MAP STATEMENTS.
C
         MESS = BLNK40
         ECODE = 'E04'
         WRITE( MESS,1000 )
1000     FORMAT(1X,'ERROR: HAVE FORMATS, BUT NO DATA MAP')
         CALL ERROR( NUMBER,PATH,ECODE,LOC,MESS )
         ITEST = 1
         RETURN
      END IF
C
C     3.  SEE IF DATA MAP WAS GIVEN, BUT NO FORMATS.
C
      IF( STATUS(4,14).NE.0 .AND. STATUS(4,15).EQ.0 ) THEN
C
C        ERROR, WE HAVE A DATA MAP BUT NO FORMATS FOR READING THE
C        INPUT DATA.
C
         MESS = BLNK40
         ECODE = 'E04'
         WRITE( MESS,2000 )
2000     FORMAT(1X,'ERROR: HAVE DATA MAP, BUT NO FORMATS')
         CALL ERROR( NUMBER,PATH,ECODE,LOC,MESS )
         ITEST = 1
      END IF
C
C     4.  CHECK COMPLETENESS OF FORMATS FOR READS,
C     ASSUMING FORMATS WERE GIVEN AND NO ERROR WERE
C     DETECTED IN FORMAT GIVEN.  AS WE GO, STORE OFF
C     OCCURRANCES OF DELTA-T VALUES FOR LATER USE IN
C     COMPLETENESS CHECKS.
C
      IF( STATUS(4,15).GE.2 ) THEN
C
         N = 1
10       CONTINUE
C
         IF( N.GT.OSMRDS .AND. OSDNUM(N).GT.0 ) THEN
C
C           TOO MANY READS
C
            MESS = BLNK40
            ECODE = 'E06'
            WRITE( MESS,2500 )
2500        FORMAT(1X,'TOO MANY READS ON OS-PATHWAY')
            CALL ERROR( NUMBER,PATH,ECODE,LOC,MESS )
            ITEST = 1
C
         ELSE IF( OSDNUM(N).GT.OSMDAT ) THEN
C
C           TOO MANY VARIABLES IN THIS READ
C
            MESS = BLNK40
            ECODE = 'E06'
            WRITE( MESS,2600 ) N,OSDNUM(N)
2600        FORMAT(1X,'ERROR: READ ',I2,' HAS ',I2,' VALUES')
            CALL ERROR( NUMBER,PATH,ECODE,LOC,MESS )
            ITEST = 1
C
         ELSE IF( OSDNUM(N).EQ.0 ) THEN
            OSDCRD = N - 1
            DO 20 N=1,OSDCRD
C
               CALL DEFINE( NUMBER,80,OSFRMT(N) )
               IF( OSFRMT(N)(IC1(1):IC1(1)).EQ.'(' .AND.
     1          OSFRMT(N)(IC2(IRD3):IC2(IRD3)).EQ.')' ) THEN
                  CONTINUE
               ELSE
                  MESS = BLNK40
                  ECODE = 'E06'
                  WRITE( MESS,3000 ) N
3000              FORMAT(1X,'MISSING/? IN FORMAT FOR READ: ',I2)
                  CALL ERROR( NUMBER,PATH,ECODE,LOC,MESS )
                  ITEST = 1
               END IF
C
20          CONTINUE
C
            GO TO 65
         END IF
C
         IF( OSDNUM(N).LE.OSMDAT ) THEN
C
            DO 30 J=1,OSDNUM(N)
C
C              STORE OFF OCCURRENCES OF DELTA-T INFO.
C
               IF( OSDVAR(N,J,1).GE.9 .AND. OSDVAR(N,J,1).LE.11 ) THEN
                  IRD5 = 100 + OSDVAR(N,J,1) - 9
                  IWORK1(IRD5) = 1
               END IF
C
C              STORE OFF OCCURRENCES OF DATE AND TIME INFO (IN POSITIONS
C              200-204)
C
               IF( OSDVAR(N,J,1).GE.56 .AND. OSDVAR(N,J,1).LE.60 ) THEN
                  IRD5 = 200 + OSDVAR(N,J,1) - 56
                  OSTIME = OSTIME + 1
                  IWORK1(IRD5) = 1
C
                  IF( N.NE.1 ) THEN
                     MESS = BLNK40
                     ECODE = 'E06'
                     WRITE( MESS,3500 )
3500                 FORMAT(1X,'DATE/TIME MUST BE 1ST PART OF 1ST READ')
                     CALL ERROR( NUMBER,PATH,ECODE,LOC,MESS )
                     ITEST = 1
                  END IF
C
               END IF
C
C              AS WE CHECK THE  STATEMENTS, STORE OFF
C              INFORMATION REGARDING
C              TOWER HEIGHTS FOR LATER QA CHECKS.
C
C              LOOKING FOR MINIMUM AND MAXIMUM LEVEL INDEX
C              REFERENCED IN DATA MAP.
C
               IF( OSDVAR(N,J,2).GT.0 .AND.
     1          OSDVAR(N,J,2).LT.HMIN ) HMIN = OSDVAR(N,J,2)
               IF( OSDVAR(N,J,2).GT.0 .AND.
     1          OSDVAR(N,J,2).GT.HMAX ) HMAX = OSDVAR(N,J,2)
C
C              Looking for heights for the various levels referenced
C              in the variable list.
C              Use IWORK1 array to 'flag' values found.
C              OSDVAR(N,J,1) = variable type
C              OSDVAR(N,J,2) = tower level index
C
               IF( OSDVAR(N,J,1).EQ.15 ) THEN
                  IWORK1( OSDVAR(N,J,2) ) = 1
               END IF
C
30          CONTINUE
C
         END IF
C
         N = N + 1
         GO TO 10
C
      END IF

C
C     5.  Check to insure all tower levels are defined with a height.
C         Tower heights can be defined two ways, either using the
C         'HGT' card, or they can be read in along with the other
C         tower data.  If the 'HGT' card was used, these data were
C         checked then.
C
C         All we need to check here is HMIN .EQ. 1 and HMAX .LE. OSNL
C         (if OSNL is non zero). If OSNL is 0, then the work array
C         (IWORK1) to insure that height data is coming through the
C         tower data records.
C
65    IF( OSNL.GT.0 ) THEN
         IF( HMIN.LT.1 ) THEN
            MESS = BLNK40
            ECODE = 'E06'
            WRITE( MESS, 5000 ) HMIN
5000        FORMAT(1X,'LOWEST TOWER LEVEL REFERENCED IS: ',I3)
            CALL ERROR( NUMBER,PATH,ECODE,LOC,MESS )
            ITEST = 1
         END IF
C
         IF( HMAX.GT.OSNL ) THEN
            MESS = BLNK40
            ECODE = 'E06'
            WRITE( MESS,6000 ) HMAX,OSNL
6000        FORMAT(1X,'LEVEL ',I3,' REFERENCED, ONLY ',I3,' DEFINED')
            CALL ERROR( NUMBER,PATH,ECODE,LOC,MESS )
            ITEST = 1
         END IF
C
      ELSE
C
C        MAKE SURE WE HAVE THE ABILITY TO DEFINE ALL HEIGHTS
C
         IF( HMIN.NE.1 ) THEN
            MESS = BLNK40
            ECODE = 'E06'
            WRITE( MESS, 5000 ) HMIN
            CALL ERROR( NUMBER,PATH,ECODE,LOC,MESS )
            ITEST = 1
         END IF
C
         IF( HMAX.GE.HMIN ) THEN
            OSNL = HMAX
            DO 70 N=HMIN,HMAX
               IF( IWORK1(N).EQ.1 ) THEN
                  CONTINUE
               ELSE
                  MESS = BLNK40
                  ECODE = 'E06'
                  WRITE( MESS,7000 ) N
7000              FORMAT(1X,'HEIGHT FOR LEVEL ',I3,' UNDEFINED')
                  CALL ERROR( NUMBER,PATH,ECODE,LOC,MESS )
                  ITEST = 1
               END IF
70          CONTINUE
C
         ELSE
            MESS = BLNK40
            ECODE = '***'
            WRITE( MESS,8000 )
8000        FORMAT(1X,'PROGRAM LOGIC ERROR HMAX LT. HMIN')
            CALL ERROR( NUMBER,PATH,ECODE,LOC,MESS )
         END IF
C
      END IF
C
C     6.  CHECK DELTA-T DATA DEFINITIONS, ALSO DEFINE OSNDT AS WE GO.
C
      DO 80 J=8,10
C
         IF( STATUS(4,J).EQ.0 ) THEN
C           STATUS SAYS NO DT-(J-7) HEIGHTS ARE DEFINED
C           MAKE SURE WE ARE NOT READING IN DT-(J-7) VALUES.
            IF( IWORK1(100+J-8).EQ.0 ) THEN
               CONTINUE
            ELSE
               MESS = BLNK40
               ECODE = 'E04'
               IRD5 = J-7
               WRITE( MESS,9000 ) IRD5
9000           FORMAT(1X,'DT0',I1,' IS INPUT, BUT NO HEIGHTS GIVEN')
               CALL ERROR( NUMBER,PATH,ECODE,LOC,MESS )
               ITEST = 1
            END IF
C
         ELSE
C
C           MAKE SURE WE HAVE DT-VALUES FOR EACH SET OF DT-LEVELS INPUT
            IF( IWORK1(100+J-8).EQ.1 ) THEN
               OSNDT = OSNDT + 1
               CONTINUE
            ELSE
               MESS = BLNK40
               ECODE = 'E04'
               IRD5 = J-7
               WRITE( MESS,9050 ) IRD5
9050           FORMAT(1X,'DT0',I1,' HEIGHTS GIVEN, BUT NO DATA INPUT')
               CALL ERROR( NUMBER,PATH,ECODE,LOC,MESS )
               ITEST = 1
            END IF
C
         END IF
C
80    CONTINUE
C
C     7.  CHECK FOR DATE (YEAR, MONTH AND DAY) AND TIME (AT LEAST HOUR)
C     VARIABLES WITHIN THE SCALAR DATA VARIABLE LIST.  WE NEED TO
C     BE ABLE TO LABEL EACH OS DATA OBSERVATION WITH A YEAR, MONTH, DAY
C     AND HOUR.
C
      DO 90 J=200,203
         IF( IWORK1(J).EQ.1 ) THEN
            CONTINUE
         ELSE
            MESS = BLNK40
            WRITE( MESS,9075 ) VNAMES(56+J-200)
9075        FORMAT(1X,'OS DATE/TIME INCOMPLETE, MISSING: ',A4)
            ITEST = 1
            ECODE = 'E06'
            CALL ERROR( NUMBER,PATH,ECODE,LOC,MESS )
         END IF
90    CONTINUE
C
C     8.  REVIEW AUDITS REQUESTED VERSUS DEFINED DATA MAP
C
      CALL AUTCHK
C
      RETURN
      END
C
      SUBROUTINE AUTCHK
C=====================================================================**
C
C     PURPOSE:  This routine processes the OS pathway definitions
C               of variables that are to be summarized in the final
C               audit report.  It searches the user defined 'DATA
C               MAP' and resets the auditing summary to only be active
C               when the requested variable occurs in the 'DATA MAP'.
C
C-----------------------------------------------------------------------
C     LOCAL VARIABLES
C
      INTEGER  VARID,LEVEL,FLAG
C
C   VARID  VARIABLE ID WE ARE WORKING WITH
C   LEVEL  TOWER LEVEL TO BE ASSOCIATED WITH VECTOR VARIABLE
C   FLAG   CONDITION FLAG USED TO COUNT NUMBER OF OCCURRANCES
C          OF A VECTOR VARIABLE THAT IS TO BE AUDITED
C
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'WORK1.INC'
C
C     INITIALIZE VALUES
C
      PATH = 'OS'
      LOC  = 'AUTCHK'
C
C     CHECK SCALAR VARIABLES
C
C     1.  LOOP ON ALLOWABLE SCALAR VARIABLES ON THE OS-PATHWAY
C
      DO 30 VARID=1,14
C
C        IF AUDID IS REQUESTED FOR THIS VARIABLE, SEACH
C        INPUT DATA MAP TO SEE IF VARIABLE OCCURS.
C
         IF( OSSAUD(VARID).EQ.0 ) GO TO 30
C
C        LOOP ON USER DEFINED DATA MAP FOR OCCURRANCE OF VARIABLE
C
         DO 20 I=1,OSDCRD
C
            DO 10 J=1,OSDNUM(I)
C
               IF( OSDVAR(I,J,1).EQ.VARID ) GO TO 30
C
10          CONTINUE
20       CONTINUE
C
C        WE ONLY GET HERE IF AUDIT WAS TURNED ON FOR A SCALAR
C        VARIABLE THAT DOES NOT OCCUR WITHIN THE USER DEFINED
C        DATA MAP.
C
         OSSAUD(VARID) = 0
         OSSTRA(VARID) = 0
         MESS = BLNK40
         ECODE = 'W15'
         WRITE( MESS,1000 ) VNAMES(VARID)
1000     FORMAT(1X,A4,' NOT INPUT: AUDIT & TRACE DISABLED')
         CALL ERROR( 0,PATH,ECODE,LOC,MESS )
C
30    CONTINUE
C
C     LOOP ON REST OF OS-SCALAR VARIABLES
C
      DO 60 VARID=30,34
C
C        IF AUDID IS REQUESTED FOR THIS VARIABLE, SEACH
C        INPUT DATA MAP TO SEE IF VARIABLE OCCURS.
C
         IF( OSSAUD(VARID).EQ.0 ) GO TO 60
C
C        LOOP ON USER DEFINED DATA MAP FOR OCCURRANCE OF VARIABLE
C
         DO 50 I=1,OSDCRD
C
            DO 40 J=1,OSDNUM(I)
C
               IF( OSDVAR(I,J,1).EQ.VARID ) GO TO 60
C
40          CONTINUE
50       CONTINUE
C
C        WE ONLY GET HERE IF AUDIT WAS TURNED ON FOR A SCALAR
C        VARIABLE THAT DOES NOT OCCUR WITHIN THE USER DEFINED
C        DATA MAP.
C
         OSSAUD(VARID) = 0
         OSSTRA(VARID) = 0
         MESS = BLNK40
         ECODE = 'W15'
         WRITE( MESS,1000 ) VNAMES(VARID)
         CALL ERROR( 0,PATH,ECODE,LOC,MESS )
C
60    CONTINUE
C
C     CHECK VECTOR VARIABLES
C
C     1.  LOOP ON ALLOWABLE VECTOR VARIABLES ON THE OS-PATHWAY
C
      DO 100 VARID=15,29
C
C        IF AUDID IS REQUESTED FOR THIS VARIABLE, SEACH
C        INPUT DATA MAP TO SEE IF VARIABLE OCCURS.
C
         IF( OSSAUD(VARID).EQ.0 ) GO TO 100
C
C        SET CONDITION FLAG TO ZERO (COUNTS NUMBER OF TIMES
C        THIS VECTOR VARIABLE OCCURS WITHIN USER DEFINED
C        DATA MAP)
C
         FLAG = 0
C
C        LOOP ON USER DEFINED DATA MAP FOR OCCURRANCE OF VARIABLE
C
         DO 90 K=1,OSDCRD
C
            DO 80 J=1,OSDNUM(K)
C
               IF( OSDVAR(K,J,1).EQ.VARID ) THEN
C
                  LEVEL = OSDVAR(K,J,2)
                  OSVAUD(LEVEL,VARID-14) = 1
                  FLAG = FLAG + 1
C
               END IF
C
80          CONTINUE
90       CONTINUE
C
C        CHECK CONDITION FLAG.  IF EQUAL TO ZERO, THEN VECTOR
C        VARIABLE IS NOT WITHIN USER DEFINED DATA MAP.
C
         IF( FLAG.GT.0 ) GO TO 100
C
C        WE ONLY GET HERE IF AUDIT WAS TURNED ON FOR A VECTOR
C        VARIABLE THAT DOES NOT OCCUR WITHIN THE USER DEFINED
C        DATA MAP.
C
         OSSTRA(VARID) = 0
         MESS = BLNK40
         ECODE = 'W15'
         WRITE( MESS,1000 ) VNAMES(VARID)
         CALL ERROR( 0,PATH,ECODE,LOC,MESS )
C
100   CONTINUE
C
      RETURN
      END
