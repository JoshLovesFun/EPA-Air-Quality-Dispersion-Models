      PROGRAM  MPRM12
C                                 MPRM
C
C           METEOROLOGICAL PROCESSOR FOR REGULATORY APPROVED
C     AIR QUALITY DISPERSION MODELS - STAGE 1 AND STAGE 2 PROCESSING
C
C        PURPOSE:  THIS ROUTINE CONTROLS THE GENERAL FLOW
C                  OF STAGES 1 & 2 PROCESSING.  BASICALLY, WE PROCESS
C                  THREE DATA TYPES:
C                        UA - U.S. NWS UPPER AIR OBSERVATIONS AND
C                             TWICE-DAILY MIXING HEIGHT VALUES.
C                        SF - U.S. NWS HOURLY WEATHER OBSERVATIONS.
C                        OS - USER SUPPLIED ON-SITE METEOROLOGICAL
C                             DATA.
C
C                1.  FIRST WE PROCESS THE USER SUPPIED SETUP DATA.
C
C                2.  THEN WE PROCESS THE UA DATA, IF APPROPRIATE TO
C                    DO SO.
C
C                3.  THEN WE PROCESS THE SF DATA, IF APPROPRIATE TO
C                    DO SO.
C
C                4.  THEN WE PROCESS THE OS DATA, IF APPROPRIATE
C                    TO DO SO.
C
C                5.  LASTLY WE GENERATE ANY SUMMARY REPORTS NEEDED.
C
Cjop $NOLIST
        INCLUDE 'MAIN1.INC'
        INCLUDE 'MAIN2.INC'
Cjop $LIST
C

      CHARACTER*12 INPFILE
      WRITE(6, *)'Enter the name of the input file:     '
      WRITE(6, *)
      READ(5, '(A12)') INPFILE
      WRITE(6, *)'The input file is:  ', INPFILE
      OPEN (DEVIN, FILE=INPFILE, STATUS='OLD', ERR=900)

C        1.  PROCESS USER SUPPLIED SET UP DATA.
C
        CALL SETUP
C
C        CALL STATUS SUMMARY, SUMRY1
C        THIS ROUTINE IS CALLED BEFORE ANY DATA PROCESSING BECAUSE
C        ALL PATHWAY STATUSES ARE SET AT THIS POINT.  ERRORS IN DATA
C        PROCESSING MAY CHANGE THE STATUS AND, HENCE, THE ORIGINAL
C        INTENT FOR DATA PROCESSING IS LOST
C
        CALL SUMRY1

C        TEST STATUS WORDS (ALL MUST BE OK, OR WE SKIP PROCESSING)
C
        IF(JBSTAT.LT.0 .OR. UASTAT.LT.0 .OR. SFSTAT.LT.0
     1     .OR. OSSTAT.LT.0 ) THEN
        GO TO 10
        END IF
C
C       REWIND THE TEMPORARY FILE (DEV70) FOR LATER USE
C
        REWIND DEV70
C
C        2.  UA DATA PATHWAY.
C
          CALL UAPATH
C
C        3.  SF DATA PATHWAY.
C
          CALL SFPATH
C
C        4.  OS DATA PATHWAY.
C
        CALL OSPATH
C
C        5.  MR DATA PATHWAY.
C
          CALL MRPATH
C
C        6.  CLOSE OUT RUN, GENERATING SUCH REPORTS AS NEEDED.
C
      WRITE( *, 8 )
    8 FORMAT('   Processing completed; writing summary files'/ )
   10 CALL FINISH
      GO TO 910
  900 WRITE(6, '(A12)')' Error opening:  ', INPFILE
  910 CONTINUE

        STOP
        END
C
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C   SUBROUTINE UAPATH
C
C        PURPOSE:  THIS ROUTINE CONTROLS THE PROCESSING CALLS FOR
C                  THE UA DATA PATHWAY.
C
C        1.  FIRST WE CHECK TO SEE IF THERE ARE ANY ACTIONS
C            TO BE TAKEN.  IF NONE, WE RETURN TO THE MAIN.
C
C        2.  IF THERE ARE ACTIONS TO BE COMPLETED, WE FIRST CHECK
C            TO SEE IF WE ARE SUPPOSE TO EXTRACT AND PROCESS DATA
C            FROM THEIR RAW DATA FILES AND TAPES.  IF SO, WE CALL
C            DATA PATHWAY'S EXTRACT ROUTINE.  IF NOT, WE PROCEED
C            ON TO THE NEXT STEP.
C
C        3.  WE NOW CHECK TO SEE IF WE ARE SUPPOSE TO CHECK THE
C            DATA FOR CONSISTENCY.  IF SO, WE CALL DATA PATHWAY'S
C            QUALITY ASSURANCE (QA) ROUTINE.  IF NOT, WE RETURN
C            TO THE MAIN.
C
C-----------------------------------------------------------------------
        SUBROUTINE UAPATH
C
Cjop $NOLIST
        INCLUDE 'MAIN1.INC'
        INCLUDE 'MAIN2.INC'
Cjop $LIST
C
C        1.  ANY ACTIONS TO COMPLETE?
C
        IF( UASTAT.GE.1 .AND. UASTAT.LE.6 ) THEN
        CONTINUE
        ELSE
        RETURN
        END IF
C
        IF( STATUS(1,4).GT.0 ) THEN
        RETURN
        END IF
C
C        2.  SHOULD WE EXTRACT FROM RAW DATA FILES?
C
        IF( UASTAT.EQ.1 .OR. UASTAT.EQ.3 ) THEN
        CALL UAEXT
        END IF
C
C        3.  SHOULD WE QA THE DATA?
C
        IF( UASTAT.EQ.2 .OR. UASTAT.EQ.3 .OR. UASTAT.EQ.6 ) THEN
         CALL UAQASM
        END IF
C
C
        RETURN
        END
C
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C   SUBROUTINE SFPATH
C
C        PURPOSE:  THIS ROUTINE CONTROLS THE PROCESSING CALLS FOR
C                  THE SF DATA PATHWAY.
C
C        1.  FIRST WE CHECK TO SEE IF THERE ARE ANY ACTIONS
C            TO BE TAKEN.  IF NONE, WE RETURN TO THE MAIN.
C
C        2.  IF THERE ARE ACTIONS TO BE COMPLETED, WE FIRST CHECK
C            TO SEE IF WE ARE SUPPOSE TO EXTRACT AND PROCESS DATA
C            FROM THEIR RAW DATA FILES AND TAPES.  IF SO, WE CALL
C            DATA PATHWAY'S EXTRACT ROUTINE.  IF NOT, WE PROCEED
C            ON TO THE NEXT STEP.
C
C        3.  WE NOW CHECK TO SEE IF WE ARE SUPPOSE TO CHECK THE
C            DATA FOR CONSISTENCY.  IF SO, WE CALL DATA PATHWAY'S
C            QUALITY ASSURANCE (QA) ROUTINE.  IF NOT, WE RETURN
C            TO THE MAIN.
C
C-----------------------------------------------------------------------
        SUBROUTINE SFPATH
C
Cjop $NOLIST
        INCLUDE 'MAIN1.INC'
        INCLUDE 'MAIN2.INC'
Cjop $LIST
C
C        1.  ANY ACTIONS TO COMPLETE?
C
        IF( SFSTAT.GE.1 .AND. SFSTAT.LE.6 ) THEN
        CONTINUE
        ELSE
        RETURN
        END IF
C
        IF( STATUS(1,4).GT.0 ) THEN
        RETURN
        END IF
C
C        2.  SHOULD WE EXTRACT FROM RAW DATA FILES?
C
        IF( SFSTAT.EQ.1 .OR. SFSTAT.EQ.3 ) THEN
        CALL SFEXT
        END IF
C
C        3.  SHOULD WE QA THE DATA?
C
        IF( SFSTAT.EQ.2 .OR. SFSTAT.EQ.3 .OR. SFSTAT.EQ.6 ) THEN
        CALL SFQASM
        END IF
C
        RETURN
        END
C
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C  SUBROUTINE OSPATH
C
C        PURPOSE:  THIS ROUTINE CONTROLS THE PROCESSING CALLS FOR
C                  THE OS DATA PATHWAY.
C
C        1.  FIRST WE CHECK TO SEE IF THERE ARE ANY ACTIONS
C            TO BE TAKEN.  IF NONE, WE RETURN TO THE MAIN.
C
C        2.  IF THERE ARE ACTIONS TO BE COMPLETED, WE CHECK
C            CHECK TO SEE IF WE ARE SUPPOSE TO CHECK THE
C            DATA FOR CONSISTENCY.  IF SO, WE CALL DATA PATHWAY'S
C            QUALITY ASSURANCE (QA) ROUTINE.  IF NOT, WE RETURN
C            TO THE MAIN.
C
C-----------------------------------------------------------------------
        SUBROUTINE OSPATH
C
Cjop $NOLIST
        INCLUDE 'MAIN1.INC'
        INCLUDE 'MAIN2.INC'
Cjop $LIST
C
C        1.  ANY ACTIONS TO COMPLETE?
C
        IF( OSSTAT.GE.2 .AND. OSSTAT.LE.7 ) THEN
        CONTINUE
        ELSE
        RETURN
        END IF
C
        IF( STATUS(1,4).GT.0 ) THEN
        RETURN
        END IF
C
C        2.  SHOULD WE QA THE DATA?
C
        IF( OSSTAT.EQ.2 .OR. OSSTAT.EQ.3 .OR. OSSTAT.EQ.6 ) THEN
         CALL OSQA
        END IF
C
        RETURN
        END
C
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C  SUBROUTINE MRPATH
C
C        PURPOSE:  THIS ROUTINE CONTROLS THE MERGE PROCESSING CALLS
C
C        1.  FIRST WE CHECK TO SEE IF THERE ARE ANY ACTIONS
C            TO BE TAKEN.  IF NONE, WE RETURN TO THE MAIN.
C
C-----------------------------------------------------------------------
        SUBROUTINE MRPATH
C
        INTEGER PSTAT(3)
C
Cjop $NOLIST
        INCLUDE 'MAIN1.INC'
        INCLUDE 'MAIN2.INC'
Cjop $LIST
C
        MRSTAT = 0
C
C       IF A STATUS IS 4 OR 6, PUT DATA PATH STATUS IN A HOLDING
C        ARRAY SO THE ORIGINAL STATUS WILL BE RETAINED AND KEEP THE
C        IF..THEN..ELSE  BLOCK TO A REASONABLE SIZE
C
        IF((UASTAT .EQ. 4) .OR. (UASTAT .EQ. 6)) THEN
         PSTAT(1) = 4
        ELSE
         PSTAT(1) = 0
        ENDIF
C
        IF((SFSTAT .EQ. 4) .OR. (SFSTAT .EQ. 6)) THEN
         PSTAT(2) = 4
        ELSE
         PSTAT(2) = 0
        ENDIF
C
        IF((OSSTAT .EQ. 4) .OR. (OSSTAT .EQ. 6)) THEN
         PSTAT(3) = 4
        ELSE
         PSTAT(3) = 0
        ENDIF
C
C        1.  ANY ACTIONS TO COMPLETE?
C
      IF( PSTAT(1).EQ.4 .AND. PSTAT(2).EQ.4 .AND. PSTAT(3).EQ.4 ) THEN
        CONTINUE
      ELSE IF(PSTAT(1).EQ.4 .AND. PSTAT(2).EQ.4 .AND. OSSTAT.EQ.0 ) THEN
        CONTINUE
      ELSE IF(PSTAT(1).EQ.4 .AND. SFSTAT.EQ.0 .AND. PSTAT(3).EQ.4 ) THEN
        CONTINUE
      ELSE IF(UASTAT.EQ.0 .AND. PSTAT(2).EQ.4 .AND. PSTAT(3).EQ.4 ) THEN
        CONTINUE
      ELSE IF(UASTAT.EQ.0 .AND. SFSTAT.EQ.0 .AND. PSTAT(3).EQ.4 ) THEN
        CONTINUE
      ELSE IF(UASTAT.EQ.0 .AND. PSTAT(2).EQ.4 .AND. OSSTAT.EQ.0 ) THEN
        CONTINUE
      ELSE
        RETURN
      END IF
C
        IF( STATUS(1,4).GT.0 ) THEN
        RETURN
        END IF
C
C      SET THE MERGE STATUS AND CALL THE MERGE SUBROUTINE
        MRSTAT = 2
        CALL MERGE
        RETURN
        END
C
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C  SUBROUTINE FINISH
C
C        PURPOSE: TO PREPARE THE FINAL SUMMARY REPORT TO THE USER
C
C-----------------------------------------------------------------------
        SUBROUTINE FINISH
C
Cjop $NOLIST
        INCLUDE 'MAIN1.INC'
        INCLUDE 'MAIN2.INC'
        INCLUDE 'SF1.INC'
        INCLUDE 'SF2.INC'
        INCLUDE 'WORK1.INC'
Cjop $LIST
C
C        1.  LIST THE STATUS ARRAY FOR INSPECTION
C
C        CHECK TO MAKE SURE THAT THE REPORT FILE IS AVAILABLE.
C        IF NOT, USE DEVIO.
C
        IF( STATUS(1,3).EQ.2 ) THEN
         IRD5 = DEV50
        ELSE
         IRD5 = DEVIO
        END IF
C
C-----------------------------------------------------------------------
C  DEBUG SECTION
C
C        WRITE( IRD5,1000 ) (I,I=1,5)
C1000    FORMAT( 10X,5(5X,I1,4X),/ )
C
C       WRITE THE STATUS ARRAY
C        DO 20 I=1,20
C
C          WRITE( IRD5,2000 ) I,(STATUS(J,I),J=1,5)
C2000      FORMAT( 5X,I2,3X,5(4X,I2,4X) )
C
C20      CONTINUE
C
C        WRITE( IRD5,3000 ) JBSTAT,UASTAT,SFSTAT,OSSTAT,MRSTAT
C3000    FORMAT(//,5X,'JBSTAT ',I2,/,
C     1            5X,'UASTAT ',I2,/,
C     1            5X,'SFSTAT ',I2,/,
C     1            5X,'OSSTAT ',I2,/,
C     1            5X,'MRSTAT ',I2)
C
C        LIST CONTENTS OF DEVICE 70 (TEMPORARY FILE)
C
C        REWIND DEV70
C30      READ( DEV70,4000,END=100 ) BUF08(1),BUF80(1)
C4000    FORMAT( A8,A80 )
C        WRITE( IRD5,4000 ) BUF08(1),BUF80(1)
C        GO TO 30
C100     DO 150 I=1,SFMV
C        WRITE( IRD5,5000 ) VNAMES(I),( SFQA(I,J),J=1,4)
C5000    FORMAT(5X,A4,5X,4I8)
C150     CONTINUE
C
C-----------------------------------------------------------------------
C
C     CALL THE SUMMARY ROUTINE (THE ARGUMENT DEFINES STAGES 1 & 2
C      PROCESSING)
        CALL SUMRY2 (1)
C
C   CALL THE QA AUDIT ROUTINE IF THERE WAS A QA ON ANY PATHWAY
C
      IF((UASTAT .EQ. 2) .OR. (UASTAT .EQ. 3) .OR. (UASTAT .EQ. 6) .OR.
     & (SFSTAT .EQ. 2) .OR. (SFSTAT .EQ. 3) .OR. (SFSTAT .EQ. 6) .OR.
     & (OSSTAT .EQ. 2) .OR. (OSSTAT .EQ. 3) .OR. (OSSTAT .EQ. 6) ) THEN
       CALL AUDIT
      ENDIF
C
        RETURN
        END
C
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C  SUBROUTINE SUMRY1
C
C  PURPOSE
C     THIS SUBROUTINE WRITES INFORMATION REGARDING THE ANTICIPATED
C     PROCESSOR RUN.  STATUS BY PATHWAY AND FILE NAMES ARE LISTED
C     TO THE REPORT FILE.
C
C  CALLED BY: PRE_MAIN
C
C  VERSION DATE: 1 JULY 1988
C  REVISED:      24 JANUARY 1996 (D. BAILEY)
C                Cosmetic changes to format statements for report file
C
C=======================================================================
C
      SUBROUTINE SUMRY1
C
      CHARACTER*3  MONTHS(12)
      CHARACTER*12  DMY1,DMY2
      CHARACTER*16 PATHNM(3)
      CHARACTER*44 MESSAG(-1:8)
      INTEGER PSTAT(3),IP
C
Cjop $NOLIST
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'UA1.INC'
      INCLUDE 'UA2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'WORK1.INC'
Cjop $LIST
C
      DATA PATHNM/'UPPER AIR DATA  ','NWS SURFACE DATA',
     &               'ON-SITE DATA   '/
      DATA MESSAG/'NONE- ERROR(S) ON INPUT IMAGES FOR THIS PATH',
     1            'NONE - NO DATA TO BE PROCESSED ON THIS PATH ',
     2            'EXTRACT ONLY                                ',
     3            'QUALITY ASSESSMENT ONLY                     ',
     4            'EXTRACT AND QUALITY ASSESSMENT              ',
     5            'MERGE ONLY                                  ',
     6            'OPTION NOT ALLOWED - EXTRACT AND MERGE      ',
     7            'QUALITY ASSESSMENT AND MERGE                ',
     8            'OPTION NOT ALLOWED - EXTRACT, QA AND MERGE  ',
     9            'THIS RUN IS ONLY A CHECK OF THE INPUT IMAGES'/
      DATA MONTHS/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG',
     1            'SEP','OCT','NOV','DEC'/
C
C-----------------------------------------------------------------------
C *** PUT THE PATH STATUSES IN AN ARRAY; DETERMINE WHAT DEVICE TO WRITE
C      TO AND WRITE THE FIRST PAGE HEADER
C
      PSTAT(1) = UASTAT
      PSTAT(2) = SFSTAT
      PSTAT(3) = OSSTAT
C
      IF(STATUS(1,3) .EQ. 2) THEN
       IRD5 = DEV50
      ELSE
       IRD5 = DEVIO
      ENDIF
C
      CALL BANNER (IRD5)
C      WRITE(IRD5,5001)

      IF (STATUS(5,4) .GT.0) THEN
      WRITE (IRD5, 5002)
      ELSE
      WRITE (IRD5, 5001)
      ENDIF

      IF(STATUS(1,4) .EQ. 0) THEN                                       DTB95025

         IF(JBSTAT .LT. 0 .OR. UASTAT.LT.0 .OR. SFSTAT.LT.0 .OR.
     &   OSSTAT .LT. 0) THEN
         WRITE(IRD5,5210)
         ELSE
         WRITE(IRD5,5220)
         ENDIF

      ENDIF                                                             DTB95025
C
C-----------------------------------------------------------------------
C *** JOB SETUP: WRITE ERROR FILE NAME OR ERROR MESSAGE IF NONE
C                WRITE REPORT FILE NAME
C
      IF(STATUS(1,4) .GT. 0) THEN
       WRITE(IRD5,5020) MESSAG(8)
      ELSE
       WRITE(IRD5,5021)
      ENDIF
C
      WRITE(IRD5,5005)
      IF(STATUS(1,5) .EQ. 2)THEN
       WRITE(IRD5,5010) DISK60
      ELSE
       WRITE(IRD5,5015)
      ENDIF
C
      IF(STATUS(1,3) .EQ. 2) THEN
       WRITE(IRD5,5011) DISK50
      ELSE
       WRITE(IRD5,5016) DEVIO
      ENDIF
C-----------------------------------------------------------------------
C *** UA, SF, OS SETUP: DETERMINE STATUS; WRITE MESSAGES AND FILE NAMES
C
      DO 1 IP = 2,4
       WRITE(IRD5,5025) IP,PATHNM(IP-1)
C
C ***  OPTION NOT ALLOWED
       IF(PSTAT(IP-1) .EQ. 5 .OR. PSTAT(IP-1) .EQ. 7) THEN
        GO TO 1
C
C ***  UPPER AIR DATA
       ELSE IF (IP .EQ. 2) THEN
        IF(UALOC .NE. '        ') THEN
         WRITE(IRD5,5050)
         WRITE(IRD5,5055) UALOC,UALAT,UALON
        ENDIF
C
        WRITE(IRD5,5028) MESSAG(PSTAT(IP-1))
C
        IF(STATUS(IP,8) .NE. 0) THEN
         IF(STATUS(IP,8) .GE. 2) THEN
          BUF08(3) = '    OPEN'
         ELSE IF(STATUS(IP,8) .EQ. 1) THEN
          BUF08(3) = 'NOT OPEN'
         ENDIF
         WRITE(IRD5,5030) BUF08(3),TAPE10
        ENDIF
C
        IF(STATUS(IP,9) .NE. 0) THEN
         IF(STATUS(IP,9) .GE. 2) THEN
          BUF08(3) = '    OPEN'
         ELSE IF(STATUS(IP,9) .EQ. 1) THEN
          BUF08(3) = 'NOT OPEN'
         ENDIF
         WRITE(IRD5,5030) BUF08(3),UNIT11
        ENDIF
C
        IF(STATUS(IP,4) .NE. 0) THEN
         IF(STATUS(IP,4) .GE. 2) THEN
          BUF08(3) = '    OPEN'
         ELSE IF(STATUS(IP,4) .EQ. 1) THEN
          BUF08(3) = 'NOT OPEN'
         ENDIF
         WRITE(IRD5,5035) BUF08(3),DISK12
        ENDIF
C
        IF(STATUS(IP,5) .NE. 0) THEN
         IF(STATUS(IP,5) .GE. 2) THEN
          BUF08(3) = '    OPEN'
         ELSE IF(STATUS(IP,5) .EQ. 1) THEN
          BUF08(3) = 'NOT OPEN'
         ENDIF
         WRITE(IRD5,5040) BUF08(3),DISK13
        ENDIF
C
        IF( STATUS(2,8) .GE. 2 .OR. STATUS(2,9) .GE. 2 ) THEN
         IF(STATUS(IP,3) .EQ. 2) THEN
          WRITE(DMY1,5080) MONTHS(UAGMO1), UAGDY1, UAYR1
          WRITE(DMY2,5080) MONTHS(UAGMO2), UAGDY2, UAYR2
          WRITE(IRD5,5060) DMY1,DMY2
         ELSE IF(STATUS(IP,3) .EQ. 1) THEN
          WRITE(IRD5,5061)
         ENDIF
        ENDIF
C
        IF(STATUS(IP,8) .GE. 2) THEN
         IF(STATUS(IP,7) .EQ. 0 .OR. STATUS(IP,7) .EQ. 2) THEN
          WRITE(IRD5,5065) UATOP
         ELSE IF(STATUS(IP,7) .EQ. 1) THEN
          WRITE(IRD5,5066)
         ENDIF
C
C
C        REPORT ON STATUS OF AUTOMATIC SOUNDING CHECKS
C
         IF(STATUS(IP,10) .EQ. 0) THEN
          BUF03 = ' ON'
          WRITE(IRD5,5070) BUF03
         ELSE IF(STATUS(IP,10) .EQ. 2) THEN
          BUF03 = 'OFF'
          WRITE(IRD5,5070) BUF03
         ENDIF
C
        ENDIF
C
C ***  SURFACE DATA
       ELSE IF (IP .EQ. 3) THEN
        IF(SFLOC .NE. '        ') THEN
         WRITE(IRD5,5050)
         WRITE(IRD5,5055) SFLOC,SFLAT,SFLON
        ENDIF
C
        WRITE(IRD5,5028) MESSAG(PSTAT(IP-1))
C
        IF(STATUS(IP,7) .NE. 0) THEN
         IF(STATUS(IP,7) .GE. 2) THEN
          BUF08(3) = '    OPEN'
         ELSE IF(STATUS(IP,7) .EQ. 1) THEN
          BUF08(3) = 'NOT OPEN'
         ENDIF
         WRITE(IRD5,5030) BUF08(3),UNIT20
        ENDIF
C
        IF(STATUS(IP,4) .NE. 0) THEN
         IF(STATUS(IP,4) .GE. 2) THEN
          BUF08(3) = '    OPEN'
         ELSE IF(STATUS(IP,4) .EQ. 1) THEN
          BUF08(3) = 'NOT OPEN'
         ENDIF
         WRITE(IRD5,5035) BUF08(3),DISK21
        ENDIF
C
        IF(STATUS(IP,5) .NE. 0) THEN
         IF(STATUS(IP,5) .GE. 2) THEN
          BUF08(3) = '    OPEN'
         ELSE IF(STATUS(IP,5) .EQ. 1) THEN
          BUF08(3) = 'NOT OPEN'
         ENDIF
         WRITE(IRD5,5040) BUF08(3),DISK22
        ENDIF
C
        IF( STATUS(IP,7) .GE. 2 ) THEN
         IF(STATUS(IP,3) .EQ. 2) THEN
          WRITE(DMY1,5080) MONTHS(SFGMO1), SFGDY1, SFYR1
          WRITE(DMY2,5080) MONTHS(SFGMO2), SFGDY2, SFYR2
          WRITE(IRD5,5060) DMY1,DMY2
         ELSE IF(STATUS(IP,3) .EQ. 1) THEN
          WRITE(IRD5,5061)
         ENDIF
        ENDIF
C
C ***  ON-SITE DATA
       ELSE IF (IP .EQ. 4) THEN
        IF(OSLOC .NE. '        ') THEN
         WRITE(IRD5,5050)
         WRITE(IRD5,5055) OSLOC,OSLAT,OSLON
        ENDIF
C
        WRITE(IRD5,5028) MESSAG(PSTAT(IP-1))
C
        IF(STATUS(IP,4) .NE. 0) THEN
         IF(STATUS(IP,4) .GE. 2) THEN
          BUF08(3) = '    OPEN'
         ELSE IF(STATUS(IP,4) .EQ. 1) THEN
          BUF08(3) = 'NOT OPEN'
         ENDIF
         WRITE(IRD5,5035) BUF08(3),DISK31
        ENDIF
C
        IF(STATUS(IP,5) .NE. 0) THEN
         IF(STATUS(IP,5) .GE. 2) THEN
          BUF08(3) = '    OPEN'
         ELSE IF(STATUS(IP,5) .EQ. 1) THEN
          BUF08(3) = 'NOT OPEN'
         ENDIF
         WRITE(IRD5,5040) BUF08(3),DISK32
        ENDIF
C
        IF(STATUS(IP,3) .EQ. 2) THEN
          WRITE(DMY1,5080) MONTHS(OSGMO1), OSGDY1, OSYR1
          WRITE(DMY2,5080) MONTHS(OSGMO2), OSGDY2, OSYR2
         WRITE(IRD5,5060) DMY1,DMY2
        ELSE IF(STATUS(IP,3) .EQ. 1) THEN
         WRITE(IRD5,5061)
        ENDIF
C
       ENDIF
C
    1 CONTINUE
C
C-----------------------------------------------------------------------
C *** MERGE SETUP
C
        IF(STATUS(5,4) .NE. 0) THEN
         IF(STATUS(5,4) .GE. 2) THEN
          BUF08(3) = '    OPEN'
         ELSE IF(STATUS(5,4) .EQ. 1) THEN
          BUF08(3) = 'NOT OPEN'
         ENDIF
         WRITE(IRD5,5042)
         WRITE(IRD5,5045) BUF08(3),DISK40
        ENDIF
C
      RETURN
C
C-----------------------------------------------------------------------
 5001 FORMAT(/16X,'STAGE 1 EXTRACTION AND QA OF METEOROLOGICAL DATA')   DTB96024
 5002 FORMAT(/16X,'        STAGE 2  MERGE METEOROLOGICAL DATA      ')   DTB96024
 5005 FORMAT(/1X,'1.  REPORT FILE NAMES')
 5010 FORMAT(/5X,' ERROR MESSAGES: ',A48)
 5011 FORMAT(5X,' SUMMARY OF RUN: ',A48)
 5015 FORMAT(/5X,' THERE IS NO FILE OPEN TO RECEIVE THE ERROR MESSAGES')
 5016 FORMAT(5X,' SUMMARY OF RUN: STANDARD OUTPUT DEVICE, UNIT ',I2)
 5020 FORMAT(//14X,56('*'),/14X,'***   ',A44,'   ***',/14X,56('*')/)
 5021 FORMAT(/18X,'STATUS REPORT PRIOR TO BEGINNING PROCESSOR ',
     &       'RUN'/)
 5025 FORMAT(//1X,I1,'.  ',A16)
 5028 FORMAT(/5X,'THE PROCESS(ES) MPRM ANTICIPATES TO PERFORM ARE:',    DTB96024
     &       //11X,A44/)                                                DTB96024
 5030 FORMAT(10X,'EXTRACT INPUT -',A8,': ',A48)                         DTB96024
 5035 FORMAT(10X,'EXTRACT OUTPUT-',A8,': ',A48)                         DTB96024
 5040 FORMAT(10X,'QA OUTPUT     -',A8,': ',A48)
 5042 FORMAT(//1X,'5.  MERGED DATA'/)
 5045 FORMAT(6X,'MERGE OUTPUT  -',A8,': ',A48)
 5050 FORMAT(/6X,'SITE ID ',3X,'LATITUDE(DEG.)',3X,'LONGITUDE(DEG.)')
 5055 FORMAT(5X,A8,5X,A8,10X,A8)
 5060 FORMAT(/10X,'THE EXTRACT DATES ARE:    STARTING: ',A12/           DTB99342
     &        10X,'                            ENDING: ',A12)           DTB99342
 5061 FORMAT(/10X,'THE EXTRACT DATES ARE: *** IN ERROR ***')            DTB96024
 5065 FORMAT( 10X,'ALL UPPER AIR DATA ABOVE ',I5,' METERS ARE CLIPPED', DTB96024
     &           ' (DISREGARDED)')
 5066 FORMAT( 10X,'THE CLIPPING HEIGHT (UATOP) IS IN ERROR')            DTB96024
 5070 FORMAT( 10X,'UPPER AIR AUTOMATIC DATA CHECKS ARE: ',A3,           DTB96024
     &           ' (DEFAULT: ON)')
 5080 FORMAT(A3, I3, ',', I5)
 5210 FORMAT(//13X,56('*'),
     & /13X,'***             ABNORMAL JOB TERMINATION             ***',
     & /13X,'********************************************************')
 5220 FORMAT(//13X,56('*'),
     & /13X,'***             JOB TERMINATED NORMALLY              ***',
     & /13X,'********************************************************')

      END

C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C  SUBROUTINE AUDIT
C
C  PURPOSE
C     THIS SUBROUTINE INTERROGATES THE AUDIT ARRAYS AND PRODUCES A
C     TABLE OF AUDIT RESULTS IN THE SUMMARY FILE
C
C  CALLED BY: FINISH
C
C  VERSION DATE: 1 JULY 1988
C
C
C-----------------------------------------------------------------------
      SUBROUTINE AUDIT
C
      LOGICAL LVAR
      INTEGER NTOT,NTOT1,NTOT2,AUDTOT,IOST70,CONCAT(22)
      REAL PERCEN,PERCN1,PERCN2
      CHARACTER*48 CVAR1
      CHARACTER*12 UAHTS(10),DTYPE
      CHARACTER*4 SFVBL1,SFVBL2
C
C     NTOT, NTOT1,NTOT2     TOTAL NUMBER OF OBSERVATIONS AUDITED
C     AUDTOT                SUM OF THE AUDIT INDICATORS - COMPUTED FOR
C                            EACH PATH, FOR SCALARS AND FOR VECTORS
C     PERCEN,PERCN1,PERCN2  PERCENT CAPTURE
C     UAHTS                 CHARACTER ARRAY DEFINING SOUNDING LAYERS
C     DTYPE                 VARIABLE DEFINING AUDIT CATEGORY
C
Cjop $NOLIST
      INCLUDE 'MAIN1.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'UA1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'UA2.INC'
      INCLUDE 'WORK1.INC'
Cjop $LIST
C
      DATA CONCAT/4*1,6*100,4*1000,100,7*1/
C
C-----------------------------------------------------------------------
C *** CHECK FOR EXISTENCE OF A REPORT FILE AND TEMPORARY FILE
C      AND WRITE INTRODUCTORY COMMENTS
C
      IF(STATUS(1,3) .EQ. 2) THEN
       IRD5 = DEV50
      ELSE
       IRD5 = DEVIO
      ENDIF
C
      INQUIRE (UNIT=DEV70,OPENED=LVAR,NAME=CVAR1)
C
C      CALL BANNER(IRD5)
      IF(STATUS(1,4) .EQ. 0) THEN

       IF(JBSTAT .LT. 0 .OR. UASTAT.LT.0 .OR. SFSTAT.LT.0 .OR.
     &    OSSTAT .LT. 0) THEN
        WRITE(IRD5,5210)
       ELSE
        WRITE(IRD5,5220)
       ENDIF
      ELSE IF(STATUS(1,4) .GT. 0) THEN
       WRITE(IRD5,5050)

      ENDIF
C
      WRITE(IRD5,5005)
C
C-----------------------------------------------------------------------
C *** CHECK THE UPPER AIR PATH FOR A SUCCESSFUL QA DURING THIS RUN
C
      IF((UASTAT .EQ. 2) .OR. (UASTAT .EQ.3) .OR. (UASTAT .EQ.6)) THEN
C ***  FIRST THE MIXING HEIGHTS
       AUDTOT = UASAUD(1) + UASAUD(2)
C
C ***  IF THERE WAS NO MIXING HTS AUDIT (AND THIS SHOULD NOT HAPPEN
C       IN THIS VERSION OF THE MPRM) GO TO THE SOUNDINGS
C
       DTYPE = '  MIXING HTS'
       IF(AUDTOT .GT. 0) THEN
        IF(STATUS(1,4) .EQ. 0) THEN
         WRITE(IRD5,5009) DTYPE
         WRITE(IRD5,5010)
         WRITE(IRD5,5011)
        ELSE
         WRITE(IRD5,5109) DTYPE
         WRITE(IRD5,5110)
         WRITE(IRD5,5111)
        ENDIF
       ELSE IF(AUDTOT .EQ. 0) THEN
        WRITE(IRD5,5400) DTYPE
        GO TO 205
       ENDIF
C
       DO 100 I = 1,2
C
C *     KEY ON AUDIT INDEX OF EACH VARIABLE: 0 = NO AUDIT, 1 = AUDIT
        IF(UASAUD(I) .EQ. 1) THEN
         PERCEN = 0.0
         NTOT   = 0
C
C *      COMPUTE THE TOTAL NUMBER OF OBSERVATIONS
         DO 110 J = 0,3
          NTOT = NTOT + UAAUD1(I,J)
  110    CONTINUE
C
         IF(NTOT .EQ. 0) THEN
          PERCEN = 0.0
         ELSE
          PERCEN = (FLOAT(UAAUD1(I,0))/NTOT) * 100.0
         ENDIF
         WORK1(200) = FLOAT(UAQA(I+10,2))
         WORK1(201) = FLOAT(UAQA(I+10,3))
         WORK1(202) = FLOAT(UAQA(I+10,4))
         IF( STATUS(1,4) .EQ. 0) THEN
          WRITE(IRD5,5100) UAVAR(I+10),NTOT,(UAAUD1(I,N),N=1,3),PERCEN,
     &       WORK1(200),WORK1(201),WORK1(202)
         ELSE
          WRITE(IRD5,5200) UAVAR(I+10),WORK1(200),WORK1(201),WORK1(202)
         ENDIF
        ENDIF
  100  CONTINUE
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C ***  NEXT THE SOUNDINGS
C
  205  AUDTOT = 0
       DO 206 I = 1,UAVR-2
        AUDTOT = AUDTOT + UAVAUD(I)
  206  CONTINUE
C
       DTYPE = '   SOUNDINGS'
       IF(AUDTOT .GT. 0) THEN
        IF(STATUS(1,4) .EQ. 0) THEN
         WRITE(IRD5,5009) DTYPE
         WRITE(IRD5,5010)
         WRITE(IRD5,5011)
        ELSE
         WRITE(IRD5,5109) DTYPE
         WRITE(IRD5,5110)
         WRITE(IRD5,5111)
        ENDIF
       ELSE
        WRITE(IRD5,5400) DTYPE
        GO TO 305
       ENDIF
C
C *** DEFINE THE LAYER HEADERS
C
      WRITE(UAHTS(1),5500)
      DO 225 I=2,9
       WRITE(UAHTS(I),5501) (I-2)*UAINC,(I-1)*UAINC
  225 CONTINUE
      WRITE(UAHTS(10),5502) 8*UAINC
C
C *** PROCESS FOR EACH HEIGHT LAYER FIRST, THEN VARIABLE
C
       DO 210 K = 1,10
        WRITE(IRD5,5600) UAHTS(K)
        DO 220 I = 1,10
         IF(K.EQ.1 .AND. I.GE.7) GO TO 220
         IF(UAVAUD(I) .EQ. 1) THEN
           PERCEN = 0.0
           NTOT   = 0
           DO 230 J=0,3
            NTOT = NTOT + UAAUD2(I,K,J)
  230      CONTINUE
           IF(NTOT .EQ. 0) THEN
            GO TO 220
           ELSE
            PERCEN = (UAAUD2(I,K,0)/FLOAT(NTOT)) * 100.0
           ENDIF
           WORK1(200) = FLOAT(UAQA(I,2))
           WORK1(201) = FLOAT(UAQA(I,3))
           WORK1(202) = FLOAT(UAQA(I,4))
           IF(STATUS(1,4) .EQ. 0) THEN
            WRITE(IRD5,5100) UAVAR(I),NTOT,(UAAUD2(I,K,N),N=1,3),PERCEN,
     &        WORK1(200),WORK1(201),WORK1(202)
           ELSE
            WRITE(IRD5,5200) UAVAR(I),WORK1(200),WORK1(201),WORK1(202)
           ENDIF
         ENDIF
  220   CONTINUE
  210  CONTINUE
C
C *** WRITE A NOTE TO THE USER ABOUT THE SCALED VARIABLES
       WRITE(IRD5,5020)
C
C *** WRITE THE SUPPLEMENTARY QA INFORMATION STORED ON DEV70
C      IF THERE WAS AN AUDIT
C
       IF(STATUS(1,4) .GT. 0) GO TO 305
C
       IF (LVAR) THEN
C
       REWIND DEV70
  240  BUF80(1) = BLNK80
       READ(DEV70,700,END=710,ERR=720,IOSTAT=IOST70) BUF80(1)
       IF( INDEX(BUF80(1),'$UAUA$') .NE. 0) THEN
C
C ***   SUPPLEMENTARY INFORMATION TO FOLLOW
  245   BUF80(1) = BLNK80
        READ(DEV70,700,END=710,ERR=720,IOSTAT=IOST70) BUF80(1)
        IF( INDEX(BUF80(1),'$UAUA$') .NE. 0) THEN
C
C ***    SUPPLEMENTARY INFORMATION COMPLETED
         GO TO 305
        ELSE
C
C ***    WRITE SUPPLEMENTARY INFORMATION
         WRITE(IRD5,700) BUF80(1)
         GO TO 245
        ENDIF
       ELSE
        GO TO 240
       ENDIF
C
  710  WRITE(IRD5,705)
       GO TO 305
  720  WRITE(IRD5,706) IOST70
       BUF40 = BLNK40
       WRITE(MESS,707) IOST70
       CALL ERROR(0,'UA','III','AUDIT',MESS)
C
      ELSE
      WRITE(IRD5,704)
      ENDIF
      END IF
C
C-----------------------------------------------------------------------
C *** SURFACE DATA
C
  305 IF((SFSTAT .EQ. 2) .OR. (SFSTAT .EQ. 3) .OR. (SFSTAT .EQ. 6)) THEN
       DTYPE = 'SURFACE DATA'
       IF(STATUS(1,4) .EQ. 0) THEN
        WRITE(IRD5,5009) DTYPE
        WRITE(IRD5,5010)
        WRITE(IRD5,5011)
       ELSE
        WRITE(IRD5,5109) DTYPE
        WRITE(IRD5,5110)
        WRITE(IRD5,5111)
       ENDIF
C
       DO 310 I=1,22
        IF(SFSAUD(I) .EQ. 1) THEN
         IF((I .GE. 5) .AND. (I .LE. 15)) THEN
C
C ***     CONCATENATED VARIABLES
          PERCN1 = 0.0
          PERCN2 = 0.0
          NTOT1  = 0
          NTOT2  = 0
C
          DO 320 J=0,3
           NTOT1 = NTOT1 + SFAUD1(I-4,J)
           NTOT2 = NTOT2 + SFAUD2(I-4,J)
  320     CONTINUE
C
          IF(NTOT1 .EQ. 0) THEN
           PERCN1 = 0.0
          ELSE
           PERCN1 = (SFAUD1(I-4,0)/FLOAT(NTOT1)) * 100.0
          ENDIF
          IF(NTOT2 .EQ. 0) THEN
           PERCN2 = 0.0
          ELSE
           PERCN2 = (SFAUD2(I-4,0)/FLOAT(NTOT2)) * 100.0
          ENDIF
          SFVBL1 = VNAMES(I+29)(1:2)//'  '
          SFVBL2 = '  '//VNAMES(I+29)(3:4)
C
          IWORK1(200) = SFQA(I+29,2)/CONCAT(I)
          WORK1(200)  = FLOAT(IWORK1(200))
          WORK1(201)  = FLOAT(SFQA(I+29,2)) - WORK1(200)*CONCAT(I)
C
          IWORK1(202) = SFQA(I+29,3)/CONCAT(I)
          WORK1(202)  = FLOAT(IWORK1(202))
          WORK1(203)  = FLOAT(SFQA(I+29,3)) - WORK1(202)*CONCAT(I)
C
          IWORK1(204) = SFQA(I+29,4)/CONCAT(I)
          WORK1(204)  = FLOAT(IWORK1(204))
          WORK1(205)  = FLOAT(SFQA(I+29,4)) - WORK1(204)*CONCAT(I)
C
          IF(STATUS(1,4) .EQ. 0) THEN
           WRITE(IRD5,5100) SFVBL1,NTOT1,(SFAUD1(I-4,N),N=1,3),PERCN1,
     &      WORK1(200),WORK1(202),WORK1(204)
           WRITE(IRD5,5100) SFVBL2,NTOT2,(SFAUD2(I-4,N),N=1,3),PERCN2,
     &      WORK1(201),WORK1(203),WORK1(205)
          ELSE
           WRITE(IRD5,5200) SFVBL1,WORK1(200),WORK1(202),WORK1(204)
           WRITE(IRD5,5200) SFVBL2,WORK1(201),WORK1(203),WORK1(205)
          ENDIF
C
         ELSE
C
C ***     REGULAR VARIABLE
          PERCEN = 0.0
          NTOT   = 0
          DO 330 J = 0,3
           NTOT = NTOT + SFAUD(I,J)
  330     CONTINUE
          IF(NTOT .EQ. 0) THEN
           PERCEN = 0.0
          ELSE
           PERCEN = (SFAUD(I,0)/FLOAT(NTOT)) * 100.0
          ENDIF
          WORK1(200) = FLOAT(SFQA(I+29,2))
          WORK1(201) = FLOAT(SFQA(I+29,3))
          WORK1(202) = FLOAT(SFQA(I+29,4))
          IF(STATUS(1,4) .EQ. 0) THEN
           WRITE(IRD5,5100) VNAMES(I+29),NTOT,(SFAUD(I,N),N=1,3),PERCEN,
     &      WORK1(200),WORK1(201),WORK1(202)
          ELSE
           WRITE(IRD5,5200)VNAMES(I+29),WORK1(200),WORK1(201),WORK1(202)
          ENDIF
         ENDIF
        ENDIF
  310  CONTINUE
C
C ***  WRITE A NOTE TO THE USER ABOUT THE SCALED VARIABLES
       WRITE(IRD5,5020)
C
C ***  WRITE THE SUPPLEMENTARY QA INFORMATION STORED ON DEV70
C       IF THERE WAS AN AUDIT
C
       IF(STATUS(1,4) .GT. 0) GO TO 405
C
       IF (LVAR) THEN
       REWIND DEV70
       WRITE(IRD5,700)
  340  BUF80(1) = BLNK80
       READ(DEV70,700,END=750,ERR=760,IOSTAT=IOST70) BUF80(1)
       IF( INDEX(BUF80(1),'$SFSF$') .NE. 0) THEN
C
C ***   SUPPLEMENTARY INFORMATION TO FOLLOW
  345   BUF80(1) = BLNK80
        READ(DEV70,700,END=750,ERR=760,IOSTAT=IOST70) BUF80(1)
        IF( INDEX(BUF80(1),'$SFSF$') .NE. 0) THEN
C
C ***    SUPPLEMENTARY INFORMATION COMPLETED
         GO TO 405
        ELSE
C
C ***    WRITE SUPPLEMENTARY INFORMATION
         WRITE(IRD5,700) BUF80(1)
         GO TO 345
        ENDIF
       ELSE
        GO TO 340
       ENDIF
C
  750  WRITE(IRD5,705)
       GO TO 405
  760  WRITE(IRD5,706) IOST70
       BUF40 = BLNK40
       WRITE(MESS,707) IOST70
       CALL ERROR(0,'SF','III','AUDIT',MESS)
C
      ELSE
      WRITE(IRD5,704)
      ENDIF
      END IF
C
C-----------------------------------------------------------------------
C *** ON-SITE DATA
C
  405 IF((OSSTAT.EQ.2) .OR. (OSSTAT.EQ.3) .OR. (OSSTAT.EQ.6) ) THEN
       IF((UASTAT.EQ.2) .OR. (UASTAT.EQ.3) .OR. (UASTAT.EQ.6) .OR.
     &    (SFSTAT.EQ.2) .OR. (SFSTAT.EQ.3) .OR. (SFSTAT.EQ.6))  THEN
        WRITE(IRD5,5007)
        CALL BANNER(IRD5)
        IF(STATUS(1,4) .EQ. 0) THEN
         IF(JBSTAT .LT. 0 .OR. UASTAT.LT.0 .OR. SFSTAT.LT.0 .OR.
     &      OSSTAT .LT. 0) THEN
          WRITE(IRD5,5210)
         ELSE
          WRITE(IRD5,5220)
         ENDIF
        ELSE IF(STATUS(1,4) .GT. 0) THEN
         WRITE(IRD5,5050)
        ENDIF
C
        WRITE(IRD5,5006)
       ENDIF
C
C ***  BEGIN WITH THE SCALARS
       AUDTOT = 0
       DO 400 I = 1,14
        AUDTOT = AUDTOT + OSSAUD(I)
  400  CONTINUE
       DO 401 I = 30,34
        AUDTOT = AUDTOT + OSSAUD(I)
  401  CONTINUE
C
C ***  IF THERE ARE AUDIT VARIABLES, WRITE THE HEADER
C ***  IF THERE ARE NO AUDITS ON SCALARS, GO TO THE MULTI-LEVEL DATA
C
       DTYPE = 'SITE SCALARS'
       IF(AUDTOT .GT. 0) THEN
        IF(STATUS(1,4) .EQ. 0) THEN
         WRITE(IRD5,5009) DTYPE
         WRITE(IRD5,5010)
         WRITE(IRD5,5011)
        ELSE
         WRITE(IRD5,5109) DTYPE
         WRITE(IRD5,5110)
         WRITE(IRD5,5111)
        ENDIF
       ELSE IF(AUDTOT .EQ. 0) THEN
        WRITE(IRD5,5400) DTYPE
        GO TO 500
       ENDIF
C
C ***  PROCESS THE AUDIT DATA - BY VARIABLE
       DO 410 I = 1,14
        IF(OSSAUD(I) .EQ. 1) THEN
         PERCEN = 0.0
         NTOT   = 0
         DO 420 J =0,3
          NTOT = NTOT + OSAUD1(I,J)
  420    CONTINUE
         IF(NTOT .EQ. 0) THEN
          PERCEN = 0.0
         ELSE
          PERCEN = (OSAUD1(I,0)/FLOAT(NTOT)) * 100.0
         ENDIF
         WORK1(200) = FLOAT(SFQA(I,2))
         WORK1(201) = FLOAT(SFQA(I,3))
         WORK1(202) = FLOAT(SFQA(I,4))
         IF(STATUS(1,4) .EQ. 0) THEN
          WRITE(IRD5,5100) VNAMES(I),NTOT,(OSAUD1(I,N),N=1,3),PERCEN,
     &     WORK1(200),WORK1(201),WORK1(202)
         ELSE
          WRITE(IRD5,5200) VNAMES(I),WORK1(200),WORK1(201),WORK1(202)
         ENDIF
        ENDIF
  410  CONTINUE
C
       DO 415 I=30,34
        IF(OSSAUD(I) .EQ. 1) THEN
         PERCEN = 0.0
         NTOT   = 0
         DO 425 J=0,3
          NTOT = NTOT + OSAUD1(I,J)
  425    CONTINUE
         IF(NTOT .EQ. 0) THEN
          PERCEN = 0.0
         ELSE
          PERCEN = (OSAUD1(I,0)/FLOAT(NTOT)) * 100.0
         ENDIF
         IF(I .EQ. 34) THEN
          SFQA(I,2) = OSTSKY(2)
          SFQA(I,3) = OSTSKY(3)
          SFQA(I,4) = OSTSKY(4)
         ENDIF
         WORK1(200) = FLOAT(SFQA(I,2))
         WORK1(201) = FLOAT(SFQA(I,3))
         WORK1(202) = FLOAT(SFQA(I,4))
         IF(STATUS(1,4) .EQ. 0) THEN
          WRITE(IRD5,5100) VNAMES(I),NTOT,(OSAUD1(I,N),N=1,3),PERCEN,
     &     WORK1(200),WORK1(201),WORK1(202)
         ELSE
          WRITE(IRD5,5200) VNAMES(I),WORK1(200),WORK1(201),WORK1(202)
         ENDIF
        ENDIF
  415  CONTINUE
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C *** MULTI-LEVEL ON-SITE DATA
C ***  CHECK FOR AUDITS
C
  500  AUDTOT = 0
       DO 506 J = 1,OSML
        DO 505 I = 1,OSMVEC
         AUDTOT = AUDTOT + OSVAUD(J,I)
  505   CONTINUE
  506  CONTINUE
C
       DTYPE = 'SITE VECTORS'
       IF(AUDTOT .GT. 0) THEN
        IF(STATUS(1,4) .EQ. 0) THEN
         WRITE(IRD5,5009) DTYPE
         WRITE(IRD5,5010)
         WRITE(IRD5,5011)
        ELSE
         WRITE(IRD5,5109) DTYPE
         WRITE(IRD5,5110)
         WRITE(IRD5,5111)
        ENDIF
C
       ELSE IF(AUDTOT .EQ. 0) THEN
        WRITE(IRD5,5400) DTYPE
        GO TO 600
       ENDIF
C
C ***  PROCESS THE AUDIT DATA - BY LEVEL, BY VARIABLE
C
       DO 510 K = 1,OSNL
C
C ***   DETERMINE IF THE HEIGHTS ARE IN THE ARRAY OSHT OR OSVOBS
C
        IF( (OSHT(K).EQ.FLOAT(SFQA(15,2))) .OR. (OSHT(K).EQ.0.0) )THEN
         IF( (OSVOBS(1,K,1).NE.FLOAT(SFQA(15,2))) .OR.
     &        (OSVOBS(1,K,1).NE.0.0) ) THEN
          OSHT(K) = OSVOBS(1,K,1)
         ELSE
          OSHT(K) = 0.0
         ENDIF
        ENDIF
C
        WRITE(IRD5,860) OSHT(K)
        DO 520 I = 1,OSMVEC
         IF(OSVAUD(K,I) .EQ. 1) THEN
           PERCEN = 0.0
           NTOT   = 0
           DO 530 J = 0,3
            NTOT = NTOT + OSAUD2(K,I,J)
  530      CONTINUE
C
           IF(NTOT .EQ. 0) THEN
            PERCEN = 0.0
           ELSE
            PERCEN = (OSAUD2(K,I,0)/FLOAT(NTOT)) * 100.0
           ENDIF
           WORK1(200) = FLOAT(SFQA(I+14,2))
           WORK1(201) = FLOAT(SFQA(I+14,3))
           WORK1(202) = FLOAT(SFQA(I+14,4))
           IF(STATUS(1,4) .EQ. 0) THEN
            WRITE(IRD5,5100) VNAMES(I+14),NTOT,(OSAUD2(K,I,N),N=1,3),
     &       PERCEN,WORK1(200),WORK1(201),WORK1(202)
           ELSE
            WRITE(IRD5,5200) VNAMES(I+14),WORK1(200),WORK1(201),
     &                        WORK1(202)
           ENDIF
         ENDIF
  520   CONTINUE
  510  CONTINUE
C
C *** ENDIF FOR ON-SITE DATA
      ENDIF
C
C-----------------------------------------------------------------------
C *** CONCLUSION AND FORMAT STATEMENTS
C
  600 WRITE(IRD5,900)
C
      RETURN
C
  700 FORMAT(A80)
  704 FORMAT('  SUPPLEMENTARY DATA ARE NOT AVAILABLE FOR PROCESSING')
  705 FORMAT('  END-OF-FILE ON DEV70 BEFORE COMPLETING SUPPLEMENTAL',
     &      ' INFORMATION')
  706 FORMAT('  ERROR READING DEV70 WHILE PROCESSING FOR SUPPLEMENTAL',
     &      ' INFORMATION, IOSTAT= ',I8)
  707 FORMAT(' ERROR READING DEV70, IOSTAT= ',I8)
  860 FORMAT(2X,F8.2,' M')
  900 FORMAT(/5X,'THIS CONCLUDES THE AUDIT TRAIL')
 5005 FORMAT(//24X,'**** SUMMARY OF THE QA AUDIT ****')
 5006 FORMAT(//19X,'**** SUMMARY OF THE QA AUDIT, CONTINUED ****' )
 5007 FORMAT(/10X,'THE AUDIT IS CONTINUED ON THE NEXT PAGE')
 5009 FORMAT(//1X,A12,9X,'|------VIOLATION SUMMARY------|',
     &       3X,'|-----TEST VALUES-----|')
 5010 FORMAT(15X,'TOTAL',5X,'#',5X,'LOWER',2X,'UPPER',6X,'%',6X,
     &        'MISSING',3X,'LOWER',3X,'UPPER')
 5011 FORMAT(15X,'# OBS',2X,'MISSING',2X,'BOUND',2X,'BOUND',
     &        2X,'ACCEPTED',5X,'FLAG',4X,'BOUND',3X,'BOUND')
 5020 FORMAT(/1X,'NOTE: TEST VALUES MATCH INTERNAL SCALING APPLIED TO ',
     & 'VARIABLES'/7X,'(SEE APPENDIX C OF THE USER''S GUIDE)'/)
 5050 FORMAT(/18X,'**********************************************',/
     &        18X,'***   NO AUDIT; INPUT IMAGES CHECKED ONLY  ***',/
     &        18X,'**********************************************'/)
 5100 FORMAT(7X,A4,3X,I5,4X,I5,2X,I5,2X,I5,4X,F6.2,3X,F8.1,',',
     &        F7.1,',',F7.1)
 5109 FORMAT(//,1X,A12,43X,'|-----TEST VALUES-----|')
 5110 FORMAT(56X,'MISSING',3X,'LOWER',3X,'UPPER')
 5111 FORMAT(58X,'FLAG',4X,'BOUND',3X,'BOUND')
 5200 FORMAT(7X,A4,3X,40('.'),1X,F8.1,',',F7.1,',',F7.1)
 5210 FORMAT(//13X,56('*'),
     & /13X,'***             ABNORMAL JOB TERMINATION             ***',
     & /13X,'********************************************************')
 5220 FORMAT(//13X,56('*'),
     & /13X,'***             JOB TERMINATED NORMALLY              ***',
     & /13X,'********************************************************')
 5400 FORMAT(5X,'THERE IS NO AUDIT TRAIL FOR ',A12)
 5500 FORMAT('     SURFACE')
 5501 FORMAT(I4,' - ',I4,'M')
 5502 FORMAT('     > ',I4,'M')
 5600 FORMAT(8X,A12)
C
C-----------------------------------------------------------------------
C *** END OF SUBROUTINE
      END
C----------------------------------------------------------------------
        BLOCK DATA
C
C       LOCAL DO LOOP VARIABLES
C
C        INTEGER I,J,K,L,M,N,II
C
        INCLUDE 'MAIN1.INC'
        INCLUDE 'MAIN2.INC'
        INCLUDE 'MP1.INC'
        INCLUDE 'OS1.INC'
        INCLUDE 'OS2.INC'
        INCLUDE 'SF1.INC'
        INCLUDE 'SF2.INC'
        INCLUDE 'UA1.INC'
        INCLUDE 'UA2.INC'
        INCLUDE 'BLOCK1.INC'
        INCLUDE 'BLOCK2.INC'
C
        END
