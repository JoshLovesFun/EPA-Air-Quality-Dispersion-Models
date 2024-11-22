      SUBROUTINE OSSRPG( AUDIT,HR0124,KST )
C=====================================================================**
C      PURPOSE:    THIS ROUTINE COMPUTES THE PASQUILL-GIFFORD (P-G)
C                  STABILITY CATEGORY USING THE SRDT ESTIMATION SCHEME.
C                  THE SCHEME USES SOLAR RADIATION (watts/m^2) AND
C                  WIND SPEED (m/s) DURING THE DAY, AND DELTA-T
C                  (deg-C) AND WIND SPEED AT NIGHT.
C                  ALL THREE VARIABLES ARE DEFINED FROM OS DATA.
C
C     Revision History:
C         July 27, 1995  J. Paumier, PES 
C                      - Replaced the IF statement for the solar
C                        radiation index for nighttime - previous
C                        version used a variable that was never
C                        assigned a value
C                      - Removed the computation of VPTG
C
C         Dec. 14, 1995  D. Bailey
C                      - Corrected Data Statement for LSRDT by changing
C                        record 48 from  6, 5, 5, 4, 4, 4, 0,
C                                    to  6, 5, 4, 4, 4, 4, 0,
C-----------------------------------------------------------------------
C     LOCAL VARIABLES
C
      INTEGER LSRDT(7,7),HR0124,KST,AUDIT,I210,II,JJ,IDT
      REAL VPTG,WS,SOLAR,DELT,WSL(5),SRL(3)
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'WORK1.INC'
C
      DATA WSL/2., 2.5, 3., 5., 6./
      DATA SRL/925., 675., 175./

C *** Define stability class as a function of wind speed (index 1) and
C *** either solar radiation in the day or temperature gradient in the
C *** night (index 2).  "0" indicates an undefined condition, which is
C *** keyed to either II=7 or JJ=7.

      DATA LSRDT /1, 1, 1, 2, 3, 3, 0,
     *            1, 2, 2, 2 ,3, 4, 0,
     *            2, 3, 3, 3, 4, 4, 0,
     *            4, 4, 4, 4, 4, 4, 0,
     *            5, 4, 4, 4, 4, 4, 0,
     *            6, 5, 4, 4, 4, 4, 0,                                  DTB95348
     *            0, 0, 0, 0, 0, 0, 0/

C
C    KST     P-G STABILITY CATEGORY, AS DETERMINED BY THIS ROUTINE.
C    LSRDT   LOOK-UP TABLE OF P-G CLASS
C    HR0124  LOCAL STANDARD TIME, 1 - 24 HOUR CLOCK
C    AUDIT   IF 0, DO NOT MAKE CALLS TO ERROR, DETAILING
C            REASONS FOR NO P-G DETERMINATION. IF .NE. ZERO,
C            MAKE CALLS TO ERROR.
C    I210    FLAG INDICATING DELTA-T RANGE
C                       I210=1          2m-10m METHOD
C                       I210=0          10m-60m METHOD
C    II,JJ   INDEX VARIABLES FOR LSRDT LOOK-UP TABLE
C    IDT     INDEX FOR DELTA-T VALUE IN OSSOBS ARRAY
C    VPTG    POTENTIAL TEMPERATURE LAPSE RATE (deg-C/m)
C    WS      WIND SPEED (m/s)
C    SOLAR   SOLAR RADIATION (watts/m^2)
C    DELT    DELTA-T (deg-C)
C    WSL     BOUNDARIES FOR WIND SPEED CATEGORIES (m/s)
C    DELTAT  BOUNDARY FOR VPTG SET TO ZERO
C    SRL     BOUNDARIES BETWEEN SOLAR RADIATION CLASSES (watts/m^2)

C      INITIALIZE VALUES
C
      PATH = 'MP'
      LOC  = 'OSSRPG'
C
C***INITIALIZE STABILITY BEFORE IT IS CALCULATED
C
      KST = 0
C
C     SEE IF KEYLVL HAS EVER BEEN DEFINED
C
      IF( KEYLVL .EQ. 0 ) THEN
         MESS = BLNK40
         ECODE = 'T75'
         WRITE( MESS,1000 ) HR0124
1000     FORMAT(1X,'KEYLVL MISSING, HR ',I2,' PGSTAB .EQ. 0')
         IF(AUDIT.NE.0) CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
         RETURN
      END IF
C
C
C***SET WIND SPEED
C
      IF( OSVOBS(HR0124,KEYLVL,9) .EQ. FLOAT(SFQA(23,2)) ) THEN
C        WIND SPEED MISSING SKIP TO NEXT HOUR
         MESS = BLNK40
         ECODE = 'T75'
         WRITE( MESS,3000 ) HR0124
3000     FORMAT(1X,'WS MISSING, HR ',I2,' PGSTAB .EQ. 0')
         IF(AUDIT.NE.0) CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
         RETURN
      ELSE
C     PASS WIND SPEED (m/s) TO VARIABLE WS
         WS=OSVOBS(HR0124,KEYLVL,9)
      END IF

C
C***PROCESS SOLAR RADIATION AND DELTA-T DATA
C
C
C***SET SOLAR RADIATION IF HOUR IS DURING THE DAY (-999. IF NIGHT)
C   USE RAMMET CONVENTION FOR DETERMINING DAYTIME PERIOD
      SOLAR=-999.
      IF(HR0124 .GT. TSR .AND. HR0124 .LT. TSS) THEN
C        [DAY]
         IF( OSSOBS(HR0124,7) .EQ. FLOAT(SFQA(7,2)) ) THEN
C           SOLAR RADIATION MISSING SKIP TO NEXT HOUR
            MESS = BLNK40
            ECODE = 'T75'
            WRITE( MESS,3001 ) HR0124
3001        FORMAT(1X,'SOLAR MISSING, HR ',I2,' PGSTAB .EQ. 0')
            IF(AUDIT.NE.0) CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
            RETURN
         ELSE
C        PASS SOLAR RADIATION (watts/m^2) TO VARIABLE SOLAR
            SOLAR=OSSOBS(HR0124,7)
         ENDIF
C        [END DAY]
      ELSE
C        [NIGHT]
C
C***SET TEMPERATURE DIFFERENCE AND VPTG IF HOUR IS DURING THE NIGHT
C   START WITH FIRST DELTA-T MEASUREMENT, AND LOOK TO OTHERS IF MISSING
C        (INDEX FOR DT1 IS 9; DT2 IS 10; AND DT3 IS 11)
         DO 50 I=1,OSNDT
            IDT=8+I
            IF( OSSOBS(HR0124,IDT) .NE. FLOAT(SFQA(IDT,2)) ) GOTO 51
            IF( I .EQ. OSNDT ) THEN
C              ALL DELTA-Ts MISSING SKIP TO NEXT HOUR
               MESS = BLNK40
               ECODE = 'T75'
               WRITE( MESS,3002 ) HR0124
3002           FORMAT(1X,'DELTA-T MISSING, HR ',I2,' PGSTAB .EQ. 0')
               IF(AUDIT.NE.0) CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
               RETURN
            ENDIF
50       CONTINUE
C        PASS DELTA-T (deg C) TO VARIABLE DELT
51       DELT=OSSOBS(HR0124,IDT)
C
C        [END NIGHT]
      ENDIF

C
C***DETERMINE INDEX VALUES FOR STABILITY CLASS TABLE
C   NOTE THAT WIND SPEED LESS THAN OR EQUAL TO ZERO IS NOT
C   EXPECTED, AND SOLAR RADIATION LESS THAN ZERO IS NOT EXPECTED,
C   SO ANY SUCH VALUES PRODUCE AN UNDEFINED STABILITY CLASS.

C  SET THE WIND SPEED INDEX : II
      II = 7
      IF(WS .GE. 0.0)    II = 1
      IF(WS .GE. WSL(1)) II = 2
      IF(WS .GE. WSL(2)) II = 3
      IF(WS .GE. WSL(3)) II = 4
      IF(WS .GE. WSL(4)) II = 5
      IF(WS .GE. WSL(5)) II = 6

C  SET THE SOLAR RADIATION INDEX (DAY) OR THE TEMPERATURE
C  GRADIENT INDEX (NIGHT) : JJ

      IF(SOLAR .NE. -999.) THEN
C        (DAY)
         JJ = 7
         IF(SOLAR .GE. 0.0)    JJ = 4
         IF(SOLAR .GE. SRL(3)) JJ = 3
         IF(SOLAR .GE. SRL(2)) JJ = 2
         IF(SOLAR .GE. SRL(1)) JJ = 1

      ELSE
C        (NIGHT)

            JJ = 5
            IF(DELT .GE. 0.0)    JJ = 6

      ENDIF

C
C***DETERMINE STABILITY.
C
      KST = LSRDT(II,JJ)
C
      IF (KST .LT. 1) THEN
         MESS = BLNK40
         ECODE = 'T75'
         WRITE( MESS,4000 ) II,JJ,HR0124
4000     FORMAT(1X,2I2,' II,JJ: PGSTAB(',I2,') .LT. 1')
         IF(AUDIT.NE.0) CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
      END IF
C
      RETURN
      END
