      SUBROUTINE MIXHGT( AUDIT,IMIX,TEST )
C=====================================================================**
C      PURPOSE:      ROUTINE COMPUTES MIXING HEIGHT.
C                    DEPENDING ON MIXING HEIGHT MODEL CHOSEN,
C                    THIS MAY BE AS SIMPLE THE METHODOLOGY USED
C                    IN CRSTER, OR MAY BE MORE COMPLEX THROUGH
C                    THE USE OF A RISE/RATE EQUATION, AS EMPLOYED
C                    IN SOME MESOSCALE MODELS.
C                    THE MIXING HEIGHT MODEL USED IS 'KEYED'
C                    BY THE VALUE IF IMIX.  CURRENTLY WE SUPPORT
C                    THE FOLLOWING OPTIONS:
C                    IMIX  = 1  CRSTER INTERPOLATION SCHEME
C                          = 2  USE VALUE REPORTED THROUGH
C                               OS-PATHWAY'S DATA.
C-----------------------------------------------------------------------
C     LOCAL VARIABLES
C
      INTEGER IMIX,TEST,AUDIT
C
C     IMIX    MIXING HEIGHT METHODOLOGY
C     TEST    STATUS OF PROCESSING
C     AUDIT   IF 0, DO NOT MAKE CALLS TO ERROR, DETAILING
C             REASONS FOR NO ZI DETERMINATION. IF NE. ZERO,
C             MAKE CALLS TO ERROR.
C
      INCLUDE 'MP1.INC'
      INCLUDE 'WORK1.INC'
C
C     INITIALIZE VALUES
C
      PATH = 'MP'
      LOC  = 'MIXHGT'
C
      GO TO(10,20,30,30,30,30,30,30,30),IMIX
C
10    CALL ZI1NWS( AUDIT )
C
      GO TO 50
20    CALL ZI1OS( AUDIT )
C
      GO TO 50
30    CALL ZI2OS( AUDIT )
C
50    CONTINUE
C
      RETURN
      END
C


      SUBROUTINE ZI1NWS( AUDIT )
C=====================================================================**
C     PURPOSE:  COMPUTE ZI-VALUES FOR GIVEN DAY USING
C               INTERPOLATION SCHEME DISCUSSED IN
C               CRSTER USER'S GUIDE.
C-----------------------------------------------------------------------
C     LOCAL VARIABLES
C
      INTEGER TEST,AUDIT,HR0124,PGSAVE,IND
      REAL    XHR,ZI1,ZI2
C
C     TEST    STATUS OF PROCESSING
C     AUDIT   IF 0, DO NOT MAKE CALLS TO ERROR, DETAILING
C             REASONS FOR NO ZI DETERMINATION. IF NE. ZERO,
C             MAKE CALLS TO ERROR.
C     HR0124  HOUR OF DAY
C     PGSAVE  USED TO STORE PG CATEGORY ASSOCIATED WITH HOUR
C             JUST BEFORE SUNRISE.
C     IND     SWITCH CONTROLLING WETHER WE COMPUTE A RURAL
C             OR URBAN MIXING HEIGHT VALUE.
C     XHR     HOUR CONVERTED TO REAL-VALUE
C     ZI1     RURAL MIXING HEIGHT COMPUTED
C     ZI2     URBAN MIXING HEIGHT COMPUTED
C
      INCLUDE 'UA1.INC'
      INCLUDE 'UA2.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'WORK1.INC'
C
C     INITIALIZE VALUES
C
      PATH = 'MP'
      LOC  = 'ZI1NWS'
      TEST = 0
C
      DO 50 HR0124=1,24
C
C     INITIALIZE VALUES
C
      ZI1 = -999
      ZI2 = -999
C
      IF( PGSTAB(HR0124) .LE. 0 ) GO TO 20
C
      XHR = FLOAT( HR0124 )
      IF (HR0124 .GT. 14 .AND. XHR .LE. TSS) GO TO 300
      IND = 2
      IF (XHR .LE. TSS) GO TO 310
      IF (PGSTAB(HR0124) .EQ. 4) GO TO 290
      IF( MIXC2 .EQ. -9999 .OR. MIXN1 .EQ. -9999 ) GO TO 20
      ZI2 = MIXC2+(MIXN1-MIXC2)*((XHR-TSS)/(24.-TSS))
      IND = 1
290   IF( MIXC2 .EQ. -9999 .OR. MIXN2 .EQ. -9999 ) GO TO 20
C
      IF( IND .EQ. 1 ) THEN
      ZI1 = MIXC2+(MIXN2-MIXC2)*((XHR-TSS)/(38.-TSS))
      ELSE
      ZI2 = MIXC2+(MIXN2-MIXC2)*((XHR-TSS)/(38.-TSS))
      ZI1 = ZI2
      END IF
C
      GO TO 360
300   ZI1 = MIXC2
      ZI2 = MIXC2
      GO TO 360
310   IF (XHR .GT. TSR) GO TO 330
      PGSAVE = PGSTAB(HR0124)
      IF (PGSTAB(HR0124) .EQ. 4) GO TO 320
      ZI2 = MIXC1
      IND = 1
320   IF( MIXP2 .EQ. -9999 .OR. MIXC2 .EQ. -9999 ) GO TO 20
C
      IF( IND .EQ. 1 ) THEN
      ZI1 = MIXP2
     1            +(MIXC2-MIXP2)*((24.-TSS+XHR)/(24.-TSS+14.))
      ELSE
      ZI2 = MIXP2
     1            +(MIXC2-MIXP2)*((24.-TSS+XHR)/(24.-TSS+14.))
      ZI1 = ZI2
      END IF
C
      GO TO 360
330   IF (PGSAVE .EQ. 4) GO TO 350
      IF( MIXC1 .EQ. -9999 .OR. MIXC2 .EQ. -9999 ) GO TO 20
      ZI2 = MIXC1+(MIXC2-MIXC1)*((XHR-TSR)/(14.-TSR))
      ZI1 = MIXC2*(XHR-TSR)/(14.-TSR)
      GO TO 360
350   IF( MIXP2 .EQ. -9999 .OR. MIXC2 .EQ. -9999 ) GO TO 20
      ZI1 = MIXP2+(MIXC2-MIXP2)*((24.-TSS+XHR)/(24.-TSS+14.))
      ZI2 = ZI1
360   CONTINUE
C
20    CONTINUE
      IF( ZI1 .EQ. -9999 .OR. ZI1 .EQ. -999) THEN
      ZI1 = -999
      TEST = TEST + 1
        IF( AUDIT.NE.0 ) THEN
        MESS = BLNK40
        ECODE = 'T72'
        WRITE( MESS,1000 ) HR0124
1000    FORMAT( 1X,'HOUR ',I2,' NO RURAL ZI VALUE ')
        CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
        END IF
      END IF
      IF( ZI2 .EQ. -9999 ) ZI2 = -999
C
      ZIHGTS(1,HR0124) = ZI1
      ZIHGTS(2,HR0124) = ZI2
C
50    CONTINUE
C
C     CHECK RESULTS FOR DAY, IF ANY HOURS HAVE MISSING
C     MIXING HEIGHTS, PRINT A SUMMARY MESSAGE FOR THE DAY.
C
      IF( TEST .GT. 0 ) THEN
      MESS = BLNK40
      ECODE = 'W72'
      WRITE( MESS,2000 ) TEST
2000  FORMAT(1X,I2,' HOURS HAVE MISSING ZI VALUES')
      CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
      END IF
C
      RETURN
      END
C


      SUBROUTINE ZI1OS( AUDIT )
C=====================================================================**
C     PURPOSE:   FILL IN MIXING HEIGHTS AS REPORTED ON OS-PATHWAY
C                ASSUMPTION: VARIABLE US01 CONTAINS DAYTIME VALUE
C                            VARIABLE US02 CONTAINS NIGHTTIME VALUE.
C-----------------------------------------------------------------------
C     LOCAL VARIABLES
C
      INTEGER TEST,AUDIT,HR0124
      REAL    MISS
C
C     TEST    STATUS OF PROCESSING
C     AUDIT   IF 0, DO NOT MAKE CALLS TO ERROR, DETAILING
C             REASONS FOR NO ZI DETERMINATION. IF NE. ZERO,
C             MAKE CALLS TO ERROR.
C     HR0124  HOUR OF DAY
C
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'WORK1.INC'
C
C     INITIALIZE VALUES
C
      PATH = 'MP'
      LOC  = ' ZI1OS'
      MISS = FLOAT( SFQA(3,2) )
      TEST = 0
C
      DO 20 HR0124=1,24
C
      IF( OSSOBS(HR0124,3) .EQ. MISS ) THEN
      ZIHGTS(1,HR0124) = -999
      ZIHGTS(2,HR0124) = -999
      TEST = TEST + 1
        IF( AUDIT.NE.0 ) THEN
        MESS = BLNK40
        ECODE = 'T72'
        WRITE( MESS,1000 ) HR0124
1000    FORMAT( 1X,'HOUR ',I2,' NO RURAL ZI VALUE ')
        CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
        END IF
      ELSE
      ZIHGTS(1,HR0124) = OSSOBS( HR0124,3 )
      ZIHGTS(2,HR0124) = OSSOBS( HR0124,3 )
      END IF
C
20    CONTINUE
C
C     CHECK RESULTS FOR DAY, IF ANY HOURS HAVE MISSING
C     MIXING HEIGHTS, PRINT A SUMMARY MESSAGE FOR THE DAY.
C
      IF( TEST .GT. 0 ) THEN
      MESS = BLNK40
      ECODE = 'W72'
      WRITE( MESS,2000 ) TEST
2000  FORMAT(1X,I2,' HOURS HAVE MISSING ZI VALUES')
      CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
      END IF
C
      RETURN
      END
C


      SUBROUTINE ZI2OS( AUDIT )
C=====================================================================**
C     PURPOSE:   FILL IN MIXING HEIGHTS AS REPORTED ON OS-PATHWAY
C                (NOTE, WE ARE SUPPLIED BY USER A DAY AND NIGHT
C                VALUE IN SCALAR (DUMMY) VALUES US01 & US02.)
C-----------------------------------------------------------------------
C     LOCAL VARIABLES
C
      INTEGER TEST,AUDIT,HR0124
      REAL    MISS
C
C     TEST    STATUS OF PROCESSING
C     AUDIT   IF 0, DO NOT MAKE CALLS TO ERROR, DETAILING
C             REASONS FOR NO ZI DETERMINATION. IF NE. ZERO,
C             MAKE CALLS TO ERROR.
C     HR0124  HOUR OF DAY
C
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'WORK1.INC'
C
C     INITIALIZE VALUES
C
      PATH = 'MP'
      LOC  = ' ZI2OS'
      MISS = FLOAT( SFQA(3,2) )
      TEST = 0
C
      DO 20 HR0124=1,24
C
      IF( OSSOBS(HR0124,12) .EQ. MISS
     1    .OR. OSSOBS(HR0124,13) .EQ. MISS ) THEN
      ZIHGTS(1,HR0124) = -999
      ZIHGTS(2,HR0124) = -999
      TEST = TEST + 1
        IF( AUDIT.NE.0 ) THEN
        MESS = BLNK40
        ECODE = 'T72'
        WRITE( MESS,1000 ) HR0124
1000    FORMAT( 1X,'HOUR ',I2,' NO RURAL ZI VALUE ')
        CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
        END IF
      ELSE
       IF( HR0124 .LT. TSS .OR. HR0124 .GT. TSS ) THEN
       ZIHGTS(1,HR0124) = OSSOBS( HR0124,13 )
       ELSE
       ZIHGTS(2,HR0124) = OSSOBS( HR0124,12 )
       END IF
C
      END IF
C
20    CONTINUE
C
C     CHECK RESULTS FOR DAY, IF ANY HOURS HAVE MISSING
C     MIXING HEIGHTS, PRINT A SUMMARY MESSAGE FOR THE DAY.
C
      IF( TEST .GT. 0 ) THEN
      MESS = BLNK40
      ECODE = 'W72'
      WRITE( MESS,2000 ) TEST
2000  FORMAT(1X,I2,' HOURS HAVE MISSING ZI VALUES')
      CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
      END IF
C
      RETURN
      END
C


      SUBROUTINE MPTEMP( AUDIT,ITEMP,IGRAD,TEST )
C=====================================================================**
C      PURPOSE:      ROUTINE COMPUTES/ESTIMATES THE TEMPERATURES.
C                    DEPENDING ON  TEMPERATURE MODEL CHOSEN,
C                    THIS MAY BE AS SIMPLE THE METHODOLOGY USED
C                    IN CRSTER, OR MAY BE MORE COMPLEX THROUGH
C                    THE USE OF BOUNDARY LAYER MODEL, AS EMPLOYED
C                    IN SOME MESOSCALE MODELS.
C                    THE TEMPERATURE MODEL USED IS 'KEYED'
C                    BY THE VALUE ITEMP.  CURRENTLY WE SUPPORT
C                    THE FOLLOWING OPTIONS:
C                    ITEMP  = 1  USE VALUE REPORTED FROM NWS WEATHER
C                                OBSERVATION,
C                           = 2  USE VALUE NEAREST ANEHGT HEIGHT
C                                REPORTED THROUGH OS-PATHWAY'S DATA.
C                    IGRAD  =    (CURRENTLY NOT ACTIVATED, IS INCLUDED
C                                FOR POSSIBLE FUTURE USE FOR MODELS
C                                REQUIRING TEMPERATURE GRADIENT DATA.)
C-----------------------------------------------------------------------
C     LOCAL VARIABLES
C
      INTEGER ITEMP,IGRAD,TEST,AUDIT
C
C     ITEMP   TEMPERATURE METHODOGY
C     TEST    STATUS OF PROCESSING
C     AUDIT   IF 0, DO NOT MAKE CALLS TO ERROR, DETAILING
C             REASONS FOR NO TEMP. DETERMINATION. IF NE. ZERO,
C             MAKE CALLS TO ERROR.
C
      INCLUDE 'MP1.INC'
      INCLUDE 'WORK1.INC'
C
C     INITIALIZE VALUES
C
      PATH = 'MP'
      LOC  = 'MPTEMP'
C
      GO TO(10,20),ITEMP
C
10    CALL TT1NWS( AUDIT )
C
      GO TO 50
20    CALL TT1OS( AUDIT )
C
50    CONTINUE
C
      RETURN
      END
C


      SUBROUTINE TT1NWS( AUDIT )
C=====================================================================**
C     PURPOSE:  FILL TEMP-VALUES FOR GIVEN DAY USING
C               NWS WEATHER OBSERVATIONS.
C-----------------------------------------------------------------------
C     LOCAL VARIABLES
C
      INTEGER TEST,AUDIT,MISS,HR0124
      REAL    TT
C
C     TEST    STATUS OF PROCESSING
C     AUDIT   IF 0, DO NOT MAKE CALLS TO ERROR, DETAILING
C             REASONS FOR NO TEMP. DETERMINATION. IF NE. ZERO,
C             MAKE CALLS TO ERROR.
C     MISS    MISSING VALUE INDICATOR
C     HR0124  HOUR OF DAY
C     TT      TEMPERATURE FOR HOUR (KELVIN)
C
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'WORK1.INC'
C
C     INITIALIZE VALUES
C
      PATH = 'MP'
      LOC  = 'TT1NWS'
      MISS = SFQA(46,2)
      TEST = 0
C
      DO 50 HR0124=1,24
C
      IF( SFOBS(HR0124,46) .EQ. MISS ) THEN
      TT = -99.0
      TEST = TEST + 1
        IF( AUDIT.NE.0 ) THEN
        MESS = BLNK40
        ECODE = 'T73'
        WRITE( MESS,1000 ) HR0124
1000    FORMAT( 1X,'HOUR ',I2,' NO TEMPERATURE VALUE ')
        CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
        END IF
      ELSE
      TT = 273.15 + FLOAT( SFOBS(HR0124,46) )/10.0
      END IF
C
      TEMP(1,HR0124) = TT
C
50    CONTINUE
C
C     CHECK RESULTS FOR DAY, IF ANY HOURS HAVE MISSING
C     TEMPERATURES, PRINT A SUMMARY MESSAGE FOR THE DAY.
C
      IF( TEST .GT. 0 ) THEN
      MESS = BLNK40
      ECODE = 'W73'
      WRITE( MESS,2000 ) TEST
2000  FORMAT(1X,I2,' HOURS HAVE MISSING TT VALUES')
      CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
      END IF
C
      RETURN
      END
C


      SUBROUTINE TT1OS( AUDIT )
C=====================================================================**
C     PURPOSE:   FILL IN TEMPERATURE USING OS-PATHWAY DATA
C                VALUE IS TO BE SELECTED NEAREST TO TMPHGT
C                HEIGHT.  BY DEFAULT THIS HEIGHT IS 2 METERS,
C                OTHERWISE IT IS THE HEIGHT SPECIFIED BY THE
C                USER ON THE "MP VBL TEMP action height"
C                KEYWORD STATEMENT
C-----------------------------------------------------------------------
C     LOCAL VARIABLES
C
      INTEGER TEST,AUDIT,HR0124
      REAL    MISS,TT
C
C     TEST    STATUS OF PROCESSING
C     AUDIT   IF 0, DO NOT MAKE CALLS TO ERROR, DETAILING
C             REASONS FOR NO ZI DETERMINATION. IF NE. ZERO,
C             MAKE CALLS TO ERROR.
C     MISS    MISSING VALUE INDICATOR
C     HR0124  HOUR OF DAY
C     TT      TEMPERATURE DETERMINED FOR HOUR (KELVIN)
C     KEYHGT  LOCAL INTEGER VARIABLE THAT DENOTES LEVEL
C             WITHIN OS-TOWER LEVELS CLOSEST TO THE REQUESTED
C             TEMPERATURE-MEASURMENT LEVEL (TMPLVL).
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'WORK1.INC'
C
C     INITIALIZE VALUES
C
      PATH = 'MP'
      LOC  = ' TT1OS'
      MISS = FLOAT( SFQA(21,2) )
      TEST = 0
C
      DO 20 HR0124=1,24
C
      IF( OSVOBS(HR0124,TMPLVL,7) .EQ. MISS .OR. TMPLVL .EQ. 0 ) THEN
      TT = -99.
      TEST = TEST + 1
        IF( AUDIT.NE.0 ) THEN
        MESS = BLNK40
        ECODE = 'T73'
        WRITE( MESS,1000 ) HR0124
1000    FORMAT( 1X,'HOUR ',I2,' NO TEMPERATURE VALUE ')
        CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
        END IF
      ELSE
      TT = 273.15 + OSVOBS(HR0124,TMPLVL,7)
      END IF
C
      TEMP(1,HR0124) = TT
C
20    CONTINUE
C
C     CHECK RESULTS FOR DAY, IF ANY HOURS HAVE MISSING
C     TEMPERATURES, PRINT A SUMMARY MESSAGE FOR THE DAY.
C
      IF( TEST .GT. 0 ) THEN
      MESS =  BLNK40
      ECODE = 'W73'
      WRITE( MESS,2000 ) TEST
2000  FORMAT(1X,I2,' HOURS HAVE MISSING TT VALUES')
      CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
      END IF
C
      RETURN
      END
C


      SUBROUTINE MPWIND( AUDIT,IWIND,TEST )
C=====================================================================**
C      PURPOSE:      ROUTINE COMPUTES/ESTIMATES THE WIND (SPD,DIR).
C                    DEPENDING ON WIND MODEL CHOSEN,
C                    THIS MAY BE AS SIMPLE THE METHODOLOGY USED
C                    IN CRSTER, OR MAY BE MORE COMPLEX THROUGH
C                    THE USE OF BOUNDARY LAYER MODEL, AS EMPLOYED
C                    IN SOME MESOSCALE MODELS.
C                    THE WIND MODEL USED IS 'KEYED'
C                    BY THE VALUE IWIND.  CURRENTLY WE SUPPORT
C                    THE FOLLOWING OPTIONS:
C                    IWIND  = 1  USE VALUE REPORTED FROM NWS WEATHER
C                                OBSERVATION,
C                           = 2  USE VALUE NEAREST STKHGT HEIGHT
C                                REPORTED THROUGH OS-PATHWAY'S DATA.
C                                (NOTE, IF OS-DATA DOES NOT EXTEND TO
C                                STKHGT,M WE USE DATA CLOEST TO 100 M.)
C-----------------------------------------------------------------------
C     LOCAL VARIABLES
C
      INTEGER IWIND,TEST,AUDIT
C
C     IWIND   WIND MODEL METHODOGY
C     TEST    STATUS OF PROCESSING
C     AUDIT   IF 0, DO NOT MAKE CALLS TO ERROR, DETAILING
C             REASONS FOR NO TEMP. DETERMINATION. IF NE. ZERO,
C             MAKE CALLS TO ERROR.
C
      INCLUDE 'MP1.INC'
      INCLUDE 'WORK1.INC'
C
C     INITIALIZE VALUES
C
      PATH = 'MP'
      LOC  = 'MPWIND'
C
      GO TO(10,20),IWIND
C
10    CALL WS1NWS( AUDIT )
C
      GO TO 50
20    CALL WS1OS( AUDIT )
C
50    CONTINUE
C
      RETURN
      END



      SUBROUTINE WS1NWS( AUDIT )
C=====================================================================**
C     PURPOSE:  Fill wind speed and direction for given day using
C               NWS weather observations.
C-----------------------------------------------------------------------
C     LOCAL VARIABLES
C
      INTEGER   LWD,TEST1,TEST2,AUDIT,MDIR,MSPD,HR0124
      REAL      WDIR,WSPD
      LOGICAL   CALM                                                    DTB94250
C
C     LWD     Last-wind-direction reported that was not
C             missing or zero
C     TEST1,2 Counter missing speed (1) and direction (2)
C     AUDIT   If 0, do not make calls to error, detailing
C             reasons for no wind determination. If ne. zero,
C             make calls to error.
C     MDIR    Missing direction indicator
C     MSPD    Missing speed indicator
C     HR0124  Hour of day
C     WDIR    Wind direction for hour (degrees)
C     WSPD    Wind speed for hour (m/s)
C
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'WORK1.INC'
C
      DATA LWD/0/
C
C---- Initialize values
C
      PATH = 'MP'
      LOC  = 'WS1NWS'
      MDIR = SFQA(50,2)
      MSPD = SFQA(51,2)
      TEST1 = 0
      TEST2 = 0
C
      DO 50 HR0124=1,24
         CALM = .FALSE.                                                    DTB94250
C
C------- Check wind direction
         IF( SFOBS(HR0124,50) .NE. MDIR )THEN
            WDIR = 10 * FLOAT( SFOBS(HR0124,50) )

         ELSE
            WDIR  = -99.0
            TEST2 = TEST2 + 1
            IF( AUDIT .NE. 0 )THEN
               MESS = BLNK40
               ECODE = 'T74'
               WRITE( MESS, 1500 ) HR0124
1500           FORMAT(' WIND DIR/FLOW VECTOR MISSING: HOUR ', I2)
               CALL ERROR( MPJDAY, PATH, ECODE, LOC, MESS )
            ENDIF

         ENDIF
C
C------- Check wind speed
         IF( SFOBS(HR0124,51) .NE. MSPD ) THEN
            WSPD = FLOAT( SFOBS(HR0124,51) )/10.0

         ELSE
            WSPD = -9.0
            TEST1 = TEST1 + 1
            IF( AUDIT.NE.0 ) THEN
                 MESS = BLNK40
                 ECODE = 'T74'
                 WRITE( MESS,1000 ) HR0124
1000             FORMAT(' WIND SPEED MISSING: HOUR ', I2)
                 CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
            ENDIF

         ENDIF
C
C------- Prepare the nonmissing data; assume 0.0 to indicate NWS calm wind 
         IF( WSPD .GT. 0.0 ) THEN
C---------- Winds are NOT calm
            SPEED(1,HR0124)   = WSPD
C           NOTE: The wind direction in WDIR already multiplied by 10
            IF (WDIR .GT. 180.0) THEN
               FLWVEC(1,HR0124) = WDIR - 180.0
            ELSE
               FLWVEC(1,HR0124) = WDIR + 180.0
            END IF

         ELSE
C---------- Winds are calm
C           Calm processing: 
C           RAMMET (unformatted file) convention:
C              Set wind speed equal to 1.0 m/s
C              Set direction equal to the nonrandomized LWD
C              Randomize LWD for the current hour
C           ISCST3 (ASCII) convention
C              Set wind speed equal to 0.0 M/S, then
C              Set direction equal to the nonrandomized LWD
C              Randomize LWD for the current hour
C
            CALM = .TRUE.                                                  DTB94250
            WSNUM(3) = WSNUM(3) + 1
            IF( MDSTAT .LE. 8 ) THEN
C              CRSTER format: for short-term hourly models with binary output
               SPEED(1,HR0124) = 1.0
               IF( LWD .GT. 0 ) THEN
                  WDIR = 10*FLOAT( LWD )
                  IF (WDIR .GT. 180.0) THEN
                     FLWVEC(1,HR0124) = WDIR - 180.0
                  ELSE
                     FLWVEC(1,HR0124) = WDIR + 180.0
                  END IF

               ELSE
                  FLWVEC(1,HR0124) = -99.0
               ENDIF

            ELSEIF( MDSTAT .GE. 14) THEN
C              ISCST model (ASCII output)
               SPEED(1,HR0124) = 0.0  
               IF( LWD .GT. 0 ) THEN
                  WDIR = 10*FLOAT( LWD )
                  IF (WDIR .GT. 180.0) THEN
                     FLWVEC(1,HR0124) = WDIR - 180.0
                  ELSE
                     FLWVEC(1,HR0124) = WDIR + 180.0
                  ENDIF
               ELSE
                  FLWVEC(1,HR0124) = -99.0
               END IF

            ELSE
C              For long-term climatological models, set calm to an
C              indicator value so it can be distributed properly in
C              the STAR output array
               SPEED(1,HR0124)  = -8
               FLWVEC(1,HR0124) = -8
               RANFLW(HR0124)   = -8
            END IF
         ENDIF
C        End processing for this hour: randomize flow vector
C
         IF( FLWVEC(1,HR0124) .GT. 0.0 )THEN
            RANFLW(HR0124) = FLWVEC(1,HR0124) +
     &                          IRND(HR0124,MPJDY) - 4.0
            IF( RANFLW(HR0124) .GT. 360.0 ) THEN
               RANFLW(HR0124) = RANFLW(HR0124) - 360.0
            ELSE IF( RANFLW(HR0124) .LE. 0) THEN
               RANFLW(HR0124) = RANFLW(HR0124) + 360.0
            END IF
         ENDIF

C        If the wind direcion was not missing or 0, set LWD equal to it
         IF(SFOBS(HR0124,50) .NE. MDIR  .AND.
     &      SFOBS(HR0124,50) .NE. 0 )THEN
            LWD = SFOBS(HR0124,50)
         ENDIF
C
   50 CONTINUE
C
C     Check results for the day; If any hours have missing
C     WS or WD, print a summary message for the day.
C
      IF( TEST1 .GT. 0 ) THEN
        MESS =  BLNK40
        ECODE = 'W74'
        WRITE( MESS,2000 ) TEST1
2000    FORMAT(1X,I2,' HOURS HAVE MISSING WD VALUES')
        CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
      END IF
C
      IF( TEST2 .GT. 0 ) THEN
        MESS = BLNK40
        ECODE = 'W74'
        WRITE( MESS,2500 ) TEST2
2500    FORMAT(1X,I2,' HOURS HAVE MISSING WS VALUES')
        CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
      END IF
C
      RETURN
      END


      SUBROUTINE WS1OS( AUDIT )
C=====================================================================**
C     PURPOSE:   FILL IN WIND USING OS-PATHWAY DATA
C                VALUE IS TO BE SELECTED NEAREST TO STKHGT
C                HEIGHT.
C-----------------------------------------------------------------------
C     LOCAL VARIABLES
C
      INTEGER   TEST1,TEST2,AUDIT,HR0124,ISHIFT
      REAL      SHGT,MDIR,MSPD,WFDIR,WSPD,LFD
C
C     TEST1,2 STATUS OF PROCESSING-  #1: NUMBER OF MISSING SPEEDS
C                                    #2: NUMBER OF MISSING DIRECTIONS
C             IN OUTPUT FILE, AFTER PROCESSING.
C     AUDIT   IF 0, DO NOT MAKE CALLS TO ERROR, DETAILING
C             REASONS FOR NO WIND DETERMINATION. IF NE. ZERO,
C             MAKE CALLS TO ERROR.
C     MDIR    MISSING VALUE INDICATOR FOR WIND DIRECTION
C     MSPD    MISSING VALUE INDICATOR FOR WIND SPEED
C     HR0124  HOUR OF DAY
C     SHGT    HEIGHT SE SEARCH FOR, EITHER STKHGT OR 100 M.
C     STKLVL  INDEX IN OS-DATA VECTOR DATA CLOSEST TO SHGT.
C     LFD     LAST DEFINED VALID FLOW DIRECTION (DEGREES)
C     WFDIR   WIND FLOW DIRECTION DETERMINED FOR HOUR (DEGREES)
C     WSPD    WIND SPEED FOR HOUR (M/S)
C     ISHIFT  SHIFT APPLIED TO WFDIR (+/- 1 DEGREE) TO AVOID A
C             PSEUDO-CALM CONDITION.
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'WORK1.INC'
C
      DATA LFD/-99.0/
C
C     INITIALIZE VALUES
C
      PATH = 'MP'
      LOC  = ' WS1OS'
      MDIR = FLOAT( SFQA(22,2) )
      MSPD = FLOAT( SFQA(23,2) )
      TEST1 = 0
      TEST2 = 0
      ISHIFT = 1
C
      DO 50 HR0124=1,24
C
C     SET 1-DEGREE SHIFT TO AVOID PSEUDO-CALMS (SIGN CHANGES EACH HOUR)
C
      ISHIFT=ISHIFT*(-1)
C
C     SEE IF WE EVEN HAVE ON SITE DATA
C
      IF( OSNL .LE. 0 .OR. STKLVL .LE. 0 ) THEN
         WFDIR = -99.0
         WSPD = -9.0
         GO TO 75
      END IF
C
C     TRANSFER SPEED & DIRECTION TO LOCAL VARIABLES
C
      WSPD=OSVOBS(HR0124,STKLVL,9)
      WFDIR=OSVOBS(HR0124,STKLVL,8)
C
C     CHECK FOR MISSING WIND SPEED
      IF(WSPD .EQ. MSPD)THEN
         WSPD=-9.0
      ENDIF
C
C     CHECK FOR MISSING DIRECTION
      IF(WFDIR .EQ. MDIR) THEN
         WFDIR=-99.0

      ELSE
C        CONVERT TO FLOW DIRECTION
         IF(WFDIR .GE. 180.5) THEN
            WFDIR = WFDIR-180.0
         ELSE
            WFDIR = WFDIR+180.0
         ENDIF
      ENDIF
C
C     TREATMENT FOR CALMS
C
      IF(WSPD .LT. AMIN1(OSCALM,1.0) .AND. WSPD .GE. 0.0) THEN
         WSNUM(3)=WSNUM(3)+1
         IF( MDSTAT .GE. 14 )THEN
C           Process output for ISCST (ASCII output)
            WSPD=0.0
            WFDIR=0.0

         ELSEIF( MDSTAT .LE. 8 )THEN
C           Process output for RAMMET-type models (unformatted)
            WSPD=1.0
            WFDIR=LFD

         ELSE
C           Process output for climatological models.  Place calms
C           in the lowest speed category of STAR output array.
            WSPD=-8.0
            WFDIR=-8.0
         ENDIF
C
      ELSEIF(WSPD .LE. 1.0 .AND. WSPD .GE. 0.) THEN
C        Treatment for "good" speeds less than or equal to 1.0 m/s
         WSPD=1.0
         IF(MDSTAT .LE. 8) THEN
C           RAMMET-TYPE MODELS --
C           SHIFT DIRECTION BY +/- 1 DEGREE IF WFDIR=LFD (AVOIDS
C           APPEARING AS A CALM HOUR) -- COMPARISON MUST USE "NEAREST
C           INTEGER" BECAUSE RAMMET OUTPUT IS IN WHOLE DEGREES
            IF(LFD .NE. -99.0) THEN
               IF(NINT(WFDIR) .EQ. NINT(LFD)) WFDIR=WFDIR+ISHIFT
            ENDIF
         ENDIF
      ENDIF
C
C     DEFINE WIND SPEED IN OUTPUT ARRAY
C
75    SPEED(1,HR0124)  = WSPD
      IF( WSPD .EQ. -9.0 ) THEN
         TEST1 = TEST1 + 1
         IF( AUDIT.NE.0 ) THEN
           MESS = BLNK40
           ECODE = 'T74'
           WRITE( MESS,1000 ) HR0124
1000       FORMAT(1X,'HOUR ',I2,' NO WIND SPEED VALUE ')
           CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
        END IF
      END IF
C
      FLWVEC(1,HR0124) = WFDIR
C
      IF( FLWVEC(1,HR0124) .EQ. -99.0 ) THEN
         RANFLW(HR0124) = -99.0
         TEST2 = TEST2 + 1
         IF( AUDIT.NE.0 ) THEN
            MESS = BLNK40
            ECODE = 'T74'
            WRITE( MESS,1500 ) HR0124
1500        FORMAT(1X,'HOUR ',I2,' NO FLOW VECTOR VALUE ')
            CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
         END IF
      ELSE IF( RANFLW(HR0124) .EQ. -8.0  ) THEN
         CONTINUE
      ELSE
         RANFLW(HR0124) = FLWVEC(1,HR0124)
      END IF
C
C     SET LFD FOR USE NEXT HOUR.  ANY VALUE OF WFDIR > 0. WILL APPEAR
C     VALID FOR THE PURPOSES OF DETECTING CALM CONDITIONS.
C
      IF(WFDIR .GT. 0.) LFD=WFDIR
C
C     END PROCESSING FOR THIS HOUR.
C
50    CONTINUE
C
C     CHECK RESULTS FOR DAY, IF ANY HOURS HAVE MISSING
C     WS OR WD, PRINT A SUMMARY MESSAGE FOR THE DAY.
C
      IF( TEST1 .GT. 0 ) THEN
         MESS =  BLNK40
         ECODE = 'W74'
         WRITE( MESS,2000 ) TEST1
2000     FORMAT(1X,I2,' HOURS HAVE MISSING WS VALUES')
         CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
      END IF
C
      IF( TEST2 .GT. 0 ) THEN
         MESS = BLNK40
         ECODE = 'W74'
         WRITE( MESS,2500 ) TEST2
2500     FORMAT(1X,I2,' HOURS HAVE MISSING WD VALUES')
         CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE SFSTAB( AUDIT,IHLX,IUST,IPGT,TEST )
C=====================================================================**
C      PURPOSE:      ROUTINE COMPUTES SURFACE LAYER STABILITY.
C                    DEPENDING ON DIFFUSION MODEL, THIS MAY BE
C                    AS SIMPLE TO ONLY COMPUTING A PASQUILL
C                    CATEGORY, TO DETERMINATION OF HEAT FLUX AND
C                    FRICTION VELOCITY.
C
C      PRESENTLY, WE HAVE ONLY ACTIVATED DETERMINATIONS OF
C      PASQUILL CATEGORY.
C-----------------------------------------------------------------------
C     LOCAL VARIABLES
C
      INTEGER I,J,IHLX,IUST,IPGT,TEST,HOUR,LST,KST,AUDIT,COUNT,HR0124
      INTEGER ORDER(7,6), CYCLE, SCHEME
C
C     IHLX    HEAT FLUX METHOD
C     IUST    FRICTION VELOCITY METHOD
C     IPGT    PG CATEGORY METHOD
c               1:PGTNWS
c               2:OS1PGT
c               3:OSSEPG
c               4:OSSAPG
c               5:OS2PGT
c               6:OSSRPG
c               7:OSINPG
C     TEST    STATUS OF PROCESSING
C     HR0124  HOUR OF DAY
C     ORDER   CALLING SEQUENCE TO USE FOR PG CATEGORY
C             DETERMINATIONS.
C     LST     LAST NONZERO PG CATEGORY COMPUTED
C     AUDIT   IF 0, DO NOT MAKE CALLS TO ERROR, DETAILING
C             REASONS FOR NO PG DETERMINATION. IF NE. ZERO,
C             MAKE CALLS TO ERROR.
C     COUNT   NUMBER OF ZERO PG CATEGORIES FOR THE DAY
C
      INCLUDE 'MP1.INC'
      INCLUDE 'WORK1.INC'
C
      DATA  (ORDER(1,I),I=1,6) /1,2,3,4,5,6/
      DATA  (ORDER(2,I),I=1,6) /2,3,4,5,6,0/
      DATA  (ORDER(3,I),I=1,6) /3,2,4,5,6,0/
      DATA  (ORDER(4,I),I=1,6) /4,2,3,5,6,0/
      DATA  (ORDER(5,I),I=1,6) /5,2,3,4,6,0/
      DATA  (ORDER(6,I),I=1,6) /6,2,3,4,5,0/
      DATA  (ORDER(7,I),I=1,6) /7,2,3,4,5,6/
C
      DATA LST/0/

C     INITIALIZE VALUES
C
      PATH = 'MP'
      LOC  = 'SFSTAB'
      COUNT = 0
C
C     ONLY CYCLE THROUGH OS-METHODS, IF IPGT .GT. 1
C     DO NOT PUT USER-SUPPLIED STAB CLASS INTO CYCLE
C
      IF( IPGT .GT. 1 .AND. IPGT .NE. 7 ) THEN
         CYCLE = 5
      ELSE
         CYCLE = 6
      END IF
C
      DO 100 J=1,24
C
      HR0124 = J
C
      DO 180  I=1,CYCLE
C
      SCHEME = ORDER(IPGT,I)
C
      GO TO(10,20,30,40,50,60,70),ORDER(IPGT,I)
C
10    CALL PGTNWS( AUDIT,HR0124,KST )
C
      GO TO 170
20    CALL OS1PGT( AUDIT,HR0124,KST )
C
      GO TO 170
30    CALL OSSEPG( AUDIT,HR0124,KST )
C
      GO TO 170
40    CALL OSSAPG( AUDIT,HR0124,KST )
C
      GO TO 170
50    CALL OS2PGT( AUDIT,HR0124,KST )
C
      GO TO 170
60    CALL OSSRPG( AUDIT,HR0124,KST )
C
      GO TO 170
70    CALL OSINPG( AUDIT,HR0124,KST )
C
170   CONTINUE
C
      IF( KST .EQ. 0 ) THEN
         GO TO 180
      ELSE
         GO TO 190
      END IF
C
180   CONTINUE
C
190   CONTINUE
C
C     STORE OFF METHODOLOGY EMPLOYED FOR THIS HOUR
C
      IF( KST .GT. 0 ) THEN
         PGSCHM( SCHEME ) = PGSCHM( SCHEME ) + 1
         MESS = BLNK40
         ECODE = 'I75'
         WRITE( MESS,1000 ) HR0124,ACTION(SCHEME)
1000     FORMAT(1X,'HOUR ',I2,' USED STAB. SCHEME ',A6)
         IF(AUDIT.NE.0 .AND. IPGT.NE.SCHEME)
     1           CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
      END IF
C
C     SAVE STABILITY CLASS VALUE RETURNED FROM SUBROUTINE IN PGSTAB0
      PGSTAB0(J) = KST
C
C     CHECK KST IF ZERO  FOR THIS HOUR, PRINT MESSAGE
C
      IF( KST .EQ. 0 ) THEN
         COUNT = COUNT + 1
         MESS = BLNK40
         ECODE = 'T75'
         WRITE( MESS,2000 ) HR0124
2000     FORMAT(1X,'HOUR ',I2,' NO PG CATEGORY POSSIBLE ')
         IF(AUDIT.NE.0) CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
C
C     'ZERO' MEMORY OF LAST KST VALUE
C
         LST = 0
C
      ELSE
C***DO NOT ALLOW STABILITY TO VARY RAPIDLY
         IF (LST .EQ. 0) LST = KST
         IF ( (KST-LST) .GT. 1 ) KST = LST + 1
         IF ( (LST-KST) .GT. 1 ) KST = LST - 1
         LST = KST
C
      END IF
C
      PGSTAB(J) = KST
C
100   CONTINUE
C
C     CHECK RESULTS FOR DAY, IF ANY HOURS HAVE A ZERO
C     PG CATEGORY, PRINT A SUMMARY MESSAGE FOR THE DAY.
C
      IF( COUNT .GT. 0 ) THEN
         MESS = BLNK40
         ECODE = 'W75'
         WRITE( MESS,3000 ) COUNT
3000     FORMAT(1X,I2,' HOURS HAVE 0 FOR PG CATEGORY ')
         CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
      END IF
C
      RETURN
      END
C


      SUBROUTINE PGTNWS( AUDIT,HR0124,KST )
C=====================================================================**
C      PURPOSE:      THIS ROUTINE COMPUTES THE PASQUILL STABILITY
C                    CATEGORY USING THE ESTIMATION SCHEME DESCRIBED
C                    BY TURNER(1964).  THE SCHEME USES TIME OF DAY,
C                    CLOUD AMOUNT AND COVERAGE, AND WIND SPEED AS
C                    INPUT.  IN THIS SUBROUTINE, DEFINE THE CLOUD
C                    DATA USING VARIABLES 'TSKY' AND 'CLHT', AND WE
C                    DEFINE THE WIND SPEED FROM 'WIND'.
C-----------------------------------------------------------------------
C     LOCAL VARIABLES
C
      INTEGER ISKY,TSKY,CLHT,LSTAB(12,7),IRADX,ICN,ISPD,
     1        HR0124,KST,AUDIT
      REAL    ANGL(3),ALF
C
C    KST     PASQUILL STABILITY CATEGORY AS DETERMINED
C            BY THIS ROUTINE
C    ISKY    SKY COVER (TENTHS), DEFINED USING OPAQUE SKY COVER
C            IF PRESENT, ELSE WE USE TOTAL SKY COVER.
C    TSKY    SET EQUAL TO SFOBS(HOUR,5),  'TSKY'
C    CLHT    SET EQUAL TO SFOBS(HOUR,4),  'CLHT'
C    LSTAB   IS TABLE 2-5 OF THE CRSTER USER'S GUIDE
C    IRADX   IS EQUIVALENT TO THE RADIATION INDEX FOR TABLE 2-5
C    ICN     AN INTERMEDIATE VARIABLE USED IN DEFINING IRADX
C    ISPD    IS THE SPEED INDEX TO TABLE 2-5, COMPUTED USING
C            SFOBS(HOUR,22),  'WIND'
C    HR0124  LOCAL STANDARD TIME, 1 - 24 HOUR CLOCK
C    ANGL    CRITICAL SUN ANGLES USED IN DEFINING IRADX
C    ALF     COMPUTED ELEVATION ANGLE OF SUN (DEGREES)
C    AUDIT   IF 0, DO NOT MAKE CALLS TO ERROR, DETAILING
C            REASONS FOR NO PG DETERMINATION. IF NE. ZERO,
C            MAKE CALLS TO ERROR.
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'WORK1.INC'
C
      DATA ANGL /60.,35.,15./
      DATA LSTAB /7,7,7,6,6,6,5,5,5,5,4,4,
     1            6,6,6,5,5,5,4,4,4,4,4,4,
     1            4,4,4,4,4,4,4,4,4,4,4,4,
     1            3,3,3,4,4,4,4,4,4,4,4,4,
     1            2,2,2,3,3,3,3,3,3,4,4,4,
     1            1,2,2,2,2,2,2,3,3,3,3,4,
     1            1,1,1,1,1,2,2,2,2,3,3,3/
C
C      INITIALIZE VALUES
C
      PATH = 'MP'
      LOC  = 'PGTNWS'
C
C***INITIALIZE STABILITY BEFORE IT IS CALCULATED
C
      KST = 0
      ISPD      = 0
      IRADX     = 0
C
C     SET WIND SPEED
C
      ISPD = SFOBS(HR0124,51)
      IF( ISPD .EQ. SFQA(51,2) ) THEN
C     IF WIND SPEED MISSING SKIP TO NEXT HOUR
         MESS = BLNK40
         ECODE = 'T75'
         WRITE( MESS,1000 ) HR0124
1000     FORMAT(1X,'ISPD MISSING HR ',I2,' PGSTAB .EQ. 0')
         IF(AUDIT.NE.0) CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
         RETURN
      ELSE
C        CONVERT WIND SPEED TO NEAREST KNOT
         ISPD = NINT( 1.94254*FLOAT(ISPD)/10.0 )
         IF( ISPD .LT. 1 )  ISPD = 1
         IF( ISPD .GT. 12 ) ISPD = 12
      END IF
C
C     INITIALIZE SKY COVER FOR HOUR
C
      TSKY = SFOBS(HR0124,34)
C
C   DEFINE SKY COVER
C
      ISKY = 99
      IRD4 = TSKY/100
      IRD3 = TSKY - 100*IRD4
      IF( IRD3 .LT. 99 ) THEN
         ISKY = IRD3
      ELSE
         ISKY = IRD4
      END IF
C
      IF( ISKY .EQ. 99 ) THEN
C     IF SKY COVER MISSING SKIP TO NEXT HOUR
         MESS = BLNK40
         ECODE = 'T75'
         WRITE( MESS,2000 ) HR0124
2000     FORMAT(1X,'ISKY MISSING HR ',I2,' PGSTAB .EQ. 0')
         IF(AUDIT.NE.0) CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
         RETURN
      END IF
C
C     Convert cloud height to nearest hundreds of feet:
C     the conversion from 100's of feet in the CD-144 & SCRAM formats
C     to tens of kilometers and back to 100's of feet is not especially
C     accurate.  Therefore, the conversion is explicit for 21 and 49
C     (kilometers * 10) because these values correspond to the critical
C     heights of 070 and 160 (hundreds of feet)
C
      CLHT = SFOBS(HR0124,33)
      IF( CLHT .EQ. SFQA(33,2) ) THEN
         CLHT = 998
      ELSEIF( CLHT .EQ. 21 )THEN
         CLHT = 70
      ELSEIF( CLHT .EQ. 49 )THEN
         CLHT = 160
      ELSE
         CLHT = NINT (CLHT/0.3048)
      END IF
C
C***DETERMINE RADIATION INDEX.
C
      IF (ISKY .EQ. 10 .AND. CLHT .LT. 70) THEN
C        LOW/MIDDLE OVERCAST CLOUD LAYER: NEUTRAL CONDITIONS
         IRADX = 3
      ELSE IF (HR0124 .GT. TSR .AND. HR0124 .LT. TSS) THEN
C        DAYTIME CONDITIONS
         ALF = SOLANG(HR0124)
Cjop     CALL SOLAR(HR0124,ALF)
         DO 220 I=1,3
         IF (ALF .GT. ANGL(I)) GO TO 230
220      CONTINUE
         I=4
230      ICN=5-I
         IF (ISKY .GT. 5) GO TO 240
         IRADX=ICN+3
         GO TO 280
240      IRADX=ICN-1
         IF (CLHT .LT. 70) GO TO 250
         IF (CLHT .LT. 160) GO TO 260
         IF (ISKY .EQ. 10) GO TO 270
         IRADX=ICN
         GO TO 270
250      IRADX=ICN-2
         GO TO 270
260      IF (ISKY .EQ. 10) IRADX=IRADX-1
270      IF (IRADX .LT. 1) IRADX=1
         IRADX=IRADX+3
      ELSE
C        NIGHTTIME CONDITIONS
         IRADX=2
         IF (ISKY .LE. 4) IRADX=1
      END IF
C
C***DETERMINE STABILITY.
C
280   KST = LSTAB(ISPD,IRADX)
C
      IF (KST .LT. 1) THEN
         MESS = BLNK40
         ECODE = 'T75'
         WRITE( MESS,3000 ) ISPD,IRADX,HR0124
3000     FORMAT(1X,2I2,' ISPD,IRADX: PGSTAB(',I2,') .LT. 1')
         IF(AUDIT.NE.0) CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
      END IF
C
      RETURN
      END
C


      SUBROUTINE OSSEPG( AUDIT,HR0124,KST )
C=====================================================================**
C      PURPOSE:      THIS ROUTINE COMPUTES THE PASQUILL STABILITY
C                    CATEGORY USING THE ON SITE MEASUREMENTS OF
C                    VERTICAL TURBULENCE FLUCTUATIONS( EITHER SE OR
C                    SW/US).
C-----------------------------------------------------------------------
C     LOCAL VARIABLES
C
      INTEGER HR0124,KST,KSTF,ISPD,ISTAT,AUDIT
      REAL PGSIGE(5),PE(5),TOP,BOTTOM,MISS,SEHR,US,ZSE
C
C    AUDIT   IF 0, DO NOT MAKE CALLS TO ERROR, DETAILING
C            REASONS FOR NO PG DETERMINATION. IF NE. ZERO,
C            MAKE CALLS TO ERROR.
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'WORK1.INC'
C
      DATA PGSIGE/11.5,10.0,7.8,5.0,2.4/
      DATA PE/0.02,0.04,0.01,-0.14,-0.31/
Cjop  DATA CONST/57.29578/           ADDED TO BLOCK DATA AS RAD2DG      JOP95APR
C
C      INITIALIZE VALUES
C
      PATH = 'MP'
      LOC  = 'OSSEPG'
C
C***INITIALIZE STABILITY BEFORE IT IS CALCULATED
C
      KST       = 0
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
C     1.  DETERMINE THE SD OF THE VERTICAL
C         WIND DIRECTION FLUCTUATIONS.  THIS CAN
C         BE MEASURED DIRECTLY:
C              SEHR = OSVOBS(HOUR,LVL,3),
C         OR CAN BE ESTIMATED AS:
C              SEHR = SW/US, WHERE
C                   SW = OSVOBS(HOUR,LVL,5),
C                   US = OSVOBS(HOUR,LVL,9).
C
C         A. FIRST DEFINE THE SURFACE ROUGHNESS LENGTH, AS
C            THIS IS USED TO DEFINE THE 'ACCEPTABLE' HEIGHTS
C            RANGE FOR ON SITE DATA.
C
      CALL ROUGH( AUDIT,HR0124,ISTAT )
C
C     CHECK ISTAT
C
      IF( ISTAT .EQ. 1 ) THEN
C        SURFACE ROUGHNESS MISSING
         MESS = BLNK40
         ECODE = 'T75'
         WRITE( MESS,1500 ) HR0124
1500     FORMAT(1X,'ZO MISSING FOR HOUR: ',I2)
         IF(AUDIT.NE.0) CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
      ELSE
C
C    DEFINE 'ACCEPTABLE' HEIGHT RANGE FOR MEASURED WINDS AND TURBULENCE
C
         TOP=AMAX1(100.*MPZO,10.)
         BOTTOM=AMAX1(20.*MPZO,1.)
C
C     TEST THAT TOWER DATA IS AT A SUITABLE HEIGHT
C
         IF( OSHT(KEYLVL) .LT. BOTTOM ) THEN
C           DATA NOT AT SUITABLE HEIGHT FOR ANALYSES
            MESS = BLNK40
            ECODE = 'T75'
            WRITE( MESS,2000 ) BOTTOM,HR0124
2000        FORMAT(1X,'ANEHGT BELOW ',F6.2,', HOUR: ',I2)
            IF(AUDIT.NE.0) CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
         END IF
C
         IF( OSHT(KEYLVL) .GT. TOP ) THEN
C           DATA NOT AT SUITABLE HEIGHT FOR ANALYSES
            MESS = BLNK40
            ECODE = 'T75'
            WRITE( MESS,2500 ) TOP,HR0124
2500        FORMAT(1X,'ANEHGT ABOVE ',F6.2,', HOUR: ',I2)
            IF(AUDIT.NE.0) CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
         END IF
C
      END IF
C
C         B. NEXT, WE CHECK THAT WE HAVE NONMISSING VALUES FOR
C            SE AND US, OR NOMISSING VALUES OF SW AND US, AT
C            THE USER'S DEFINED ANEHGT.
C
      IF( OSVOBS(HR0124,KEYLVL,9) .EQ. FLOAT(SFQA(23,2)) ) THEN
C        WIND SPEED MISSING SKIP TO NEXT HOUR
         MESS = BLNK40
         ECODE = 'T75'
         WRITE( MESS,3000 ) HR0124
3000     FORMAT(1X,'ISPD MISSING, HR ',I2,' PGSTAB .EQ. 0')
         IF(AUDIT.NE.0) CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
         RETURN
      END IF
C
      IF( OSVOBS( HR0124,KEYLVL,3) .EQ. FLOAT( SFQA(17,2))) THEN
C        NO SE DATA AT KEYLVL FOR THIS HOUR
         IF( OSVOBS( HR0124,KEYLVL,5) .EQ. FLOAT( SFQA(19,2))) THEN
C           NO SE OR SW DATA AT KEYLVL FOR THIS HOUR
            MESS = BLNK40
            ECODE = 'T75'
            WRITE( MESS,3500 ) HR0124
3500        FORMAT(1X,'SE & SW MISSING, HR ',I2,' PGSTAB .EQ. 0')
            IF(AUDIT.NE.0) CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
            RETURN
         ELSE
            SEHR = RAD2DG * OSVOBS( HR0124,KEYLVL,5 ) /
     &                      OSVOBS( HR0124,KEYLVL,9 )
            US = OSVOBS( HR0124,KEYLVL,9 )
            ZSE = OSHT(KEYLVL)
         END IF
      ELSE
         SEHR = OSVOBS( HR0124,KEYLVL,3 )
         US = OSVOBS( HR0124,KEYLVL,9 )
         ZSE = OSHT(KEYLVL)
      ENDIF
C
C     LOOP OVER STABILITY CLASSES TO ADJUST SIGMA-E RANGES FOR SITE
C     ROUGHNESS AND ANEMOMETER HEIGHT, AND COMPARE MEASURED SIGMA-E
C     WITH MODIFIED RANGE BOUNDARIES TO DETERMINE STABILITY CLASS
C     BASED ON SIGMA-E ALONE.
C
      DO 60 I=1,5
      IF( MPZO .GT. 0.15 ) THEN
         BOTTOM = PGSIGE(I) * (MPZO/0.15)**0.2 * (ZSE/10.0)**PE(I)
      ELSE
         BOTTOM = PGSIGE(I) * (ZSE/10.0)**PE(I)
      END IF
      IF(SEHR .GE. BOTTOM) THEN
         KST = I
         GO TO 70
      END IF
60    CONTINUE
      KST = 6
70    CONTINUE
C
      KSTF = KST
C
C     NOW ADJUST STABILITY CLASS FOR WIND SPEED STRENGTH AND TIME OF DAY
C     RAMMET TEST FOR DAY/NIGHT IMPLEMENTED ALTHOUGH THIS DOES NOT
C     CONFORM TO GUIDANCE FOR ON-SITE SIGMA-A SCHEME.
C
      IF( HR0124 .GT. TSR .AND. HR0124 .LT. TSS ) THEN
C          ADJUSTMENTS FOR DAYTIME:
           IF( KST .GE. 4 .OR. US .GE. 6.0 ) THEN
                KSTF = 4
           ELSE IF( US .GE. 4.0 ) THEN
                KSTF = 3
           ELSE IF( KST .EQ. 1 .AND. US .GE. 3.0 ) THEN
                KSTF = 2
           END IF
      ELSE
C          ADJUSTMENTS FOR NIGHTTIME:
           IF( KST .LE. 4 .OR. US .GE. 5.0 ) THEN
                KSTF = 4
           ELSE IF( KST .EQ. 6 .AND. US .GE. 3.0 ) THEN
                KSTF = 5
           END IF
      END IF
C
      KST = KSTF
C
      RETURN
      END
C


      SUBROUTINE OS2PGT( AUDIT,HR0124,KST )
C=====================================================================**
C      PURPOSE:      THIS ROUTINE COMPUTES THE PASQUILL STABILITY
C                    CATEGORY USING THE ESTIMATION SCHEME DESCRIBED
C                    BY TURNER(1964).  THE OS2PGT METHOD USES NWS       DTBMAY93
C                    CLOUD COVER, 'TSKY' AND CEILING, 'CLHT' IN         DTBMAY93
C                    COMBINATION WITH ON-SITE WIND SPEED.               DTBMAY93
C-----------------------------------------------------------------------
C     LOCAL VARIABLES
C
      INTEGER ISKY,TSKY,CLHT,LSTAB(12,7),IRADX,ICN,ISPD,
     1        ISTAT,HR0124,KST,AUDIT
      REAL    ANGL(3),ALF,TOP,BOTTOM
C
C    KST     PASQUILL STABILITY CATEGORY AS DETERMINED
C            BY THIS ROUTINE
C    ISKY    SKY COVER (TENTHS), DEFINED USING OPAQUE SKY COVER
C            IF PRESENT, ELSE WE USE TOTAL SKY COVER.
C    TSKY    SET EQUAL TO SFOBS(HOUR,5),  'TSKY'
C    CLHT    SET EQUAL TO SFOBS(HOUR,4),  'CLHT'
C    LSTAB   IS TABLE 2-5 OF THE CRSTER USER'S GUIDE
C    IRADX   IS EQUIVALENT TO THE RADIATION INDEX FOR TABLE 2-5
C    ICN     AN INTERMEDIATE VARIABLE USED IN DEFINING IRADX
C    ISPD    IS THE SPEED INDEX TO TABLE 2-5, COMPUTED USING
C            OSVOBS (HOUR, LVL, 9)                                      DTBMAY93
C            *** disregard old comment ***  SFOBS(HOUR,22), 'WIND'      DTBMAY93
C
C    HR0124  LOCAL STANDARD TIME, 1 - 24 HOUR CLOCK
C    ANGL    CRITICAL SUN ANGLES USED IN DEFINING IRADX
C    ALF     COMPUTED ELEVATION ANGLE OF SUN (DEGREES)
C    AUDIT   IF 0, DO NOT MAKE CALLS TO ERROR, DETAILING
C            REASONS FOR NO PG DETERMINATION. IF NE. ZERO,
C            MAKE CALLS TO ERROR.
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'WORK1.INC'
C
      DATA ANGL /60.,35.,15./
      DATA LSTAB /7,7,7,6,6,6,5,5,5,5,4,4,
     1            6,6,6,5,5,5,4,4,4,4,4,4,
     1            4,4,4,4,4,4,4,4,4,4,4,4,
     1            3,3,3,4,4,4,4,4,4,4,4,4,
     1            2,2,2,3,3,3,3,3,3,4,4,4,
     1            1,2,2,2,2,2,2,3,3,3,3,4,
     1            1,1,1,1,1,2,2,2,2,3,3,3/
C
C      INITIALIZE VALUES
C
      PATH = 'MP'
      LOC  = 'OS2PGT'
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
      ISPD      = 0
      IRADX     = 0
C
C     SET WIND SPEED
C
      IF( OSVOBS(HR0124,KEYLVL,9) .EQ. FLOAT(SFQA(23,2)) ) THEN
C        WIND SPEED MISSING SKIP TO NEXT HOUR
         MESS = BLNK40
         ECODE = 'T75'
         WRITE( MESS,3000 ) HR0124
3000     FORMAT(1X,'ISPD MISSING, HR ',I2,' PGSTAB .EQ. 0')
         IF(AUDIT.NE.0) CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
         RETURN
      ELSE
C     CONVERT WIND SPEED TO NEAREST KNOT
         ISPD = 1.94254*OSVOBS(HR0124,KEYLVL,9) + 0.5
         IF( ISPD .LT. 1 )  ISPD = 1
         IF( ISPD .GT. 12 ) ISPD = 12
      END IF
C
C     INITIALIZE SKY COVER FOR HOUR
C
      TSKY = SFOBS(HR0124,34)
C
C   DEFINE SKY COVER
C
      ISKY = 99
      IRD4 = TSKY/100
      IRD3 = TSKY - 100*IRD4
      IF( IRD3 .LT. 99 ) THEN
         ISKY = IRD3
      ELSE
         ISKY = IRD4
      END IF
C
      IF( ISKY .EQ. 99 ) THEN
C        SKY COVER MISSING SKIP TO NEXT HOUR
         MESS = BLNK40
         ECODE = 'T75'
         WRITE( MESS,3500 ) HR0124
3500     FORMAT(1X,'ISKY MISSING, HR ',I2,' PGSTAB .EQ. 0')
         IF(AUDIT.NE.0) CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
         RETURN
      END IF
C
C     CONVERT CLOUD HEIGHT TO NEAREST HUNDREDS OF FEET
C
      CLHT = SFOBS(HR0124,33)
      IF( CLHT .EQ. SFQA(33,2) ) THEN
         CLHT = 998
      ELSEIF( CLHT .EQ. 21 )THEN
         CLHT = 70
      ELSEIF( CLHT .EQ. 49 )THEN
         CLHT = 160
      ELSE
         CLHT = NINT (CLHT/0.3048)
      END IF
C
C***DETERMINE RADIATION INDEX.
C
      IF (ISKY .EQ. 10 .AND. CLHT .LT. 70) THEN
C        LOW/MIDDLE OVERCAST CLOUD LAYER: NEUTRAL CONDITIONS
         IRADX = 3
      ELSE IF (HR0124 .GT. TSR .AND. HR0124 .LT. TSS) THEN
C        DAYTIME CONDITIONS
         ALF = SOLANG(HR0124)
Cjop     CALL SOLAR(HR0124,ALF)
         DO 220 I=1,3
         IF (ALF .GT. ANGL(I)) GO TO 230
220      CONTINUE
         I=4
230      ICN=5-I
         IF (ISKY .GT. 5) GO TO 240
         IRADX=ICN+3
         GO TO 280
240      IRADX=ICN-1
         IF (CLHT .LT. 70) GO TO 250
         IF (CLHT .LT. 160) GO TO 260
         IF (ISKY .EQ. 10) GO TO 270
         IRADX=ICN
         GO TO 270
250      IRADX=ICN-2
         GO TO 270
260      IF (ISKY .EQ. 10) IRADX=IRADX-1
270      IF (IRADX .LT. 1) IRADX=1
         IRADX=IRADX+3
      ELSE
C        NIGHTTIME CONDITIONS
         IRADX=2
         IF (ISKY .LE. 4) IRADX=1
      END IF
C
C***DETERMINE STABILITY.
C
280   KST = LSTAB(ISPD,IRADX)
C
      IF (KST .LT. 1) THEN
         MESS = BLNK40
         ECODE = 'T75'
         WRITE( MESS,4000 ) ISPD,IRADX,HR0124
4000     FORMAT(1X,2I2,' ISPD,IRADX: PGSTAB(',I2,') .LT. 1')
         IF(AUDIT.NE.0) CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
      END IF
C
      RETURN
      END
C



      SUBROUTINE OS1PGT( AUDIT,HR0124,KST )
C=====================================================================**
C      PURPOSE:      THIS ROUTINE COMPUTES THE PASQUILL STABILITY
C                    CATEGORY USING THE ESTIMATION SCHEME DESCRIBED
C                    BY TURNER(1964).  THE SCHEME USES TIME OF DAY,
C                    CLOUD AMOUNT AND COVERAGE, AND WIND SPEED AS
C                    INPUT.  IN THIS SUBROUTINE, DEFINE THE CLOUD
C                    DATA USING THE OS VARIABLES FOR 'TSKY' AND
C                    'CLHT', AND WE DEFINE THE WIND SPEED FROM
C                    THE OS TOWER DATA.
C-----------------------------------------------------------------------
C     LOCAL VARIABLES
C
      INTEGER ISKY,TSKY,CLHT,LSTAB(12,7),IRADX,ICN,ISPD,
     1        ISTAT,HR0124,KST,AUDIT
      REAL    ANGL(3),ALF,TOP,BOTTOM
C
C    KST     PASQUILL STABILITY CATEGORY, AS DETERMINED BY
C            THIS ROUTINE.
C    ISKY    SKY COVER (TENTHS), DEFINED USING OPAQUE SKY COVER
C            IF PRESENT, ELSE WE USE TOTAL SKY COVER.
C    TSKY    SET EQUAL TO OSSOBS(HOUR,19),  'TSKY'
C    CLHT    SET EQUAL TO OSSOBS(HOUR,33),  'CLHT'
C    LSTAB   IS TABLE 2-5 OF THE CRSTER USER'S GUIDE
C    IRADX   IS EQUIVALENT TO THE RADIATION INDEX FOR TABLE 2-5
C    ICN     AN INTERMEDIATE VARIABLE USED IN DEFINING IRADX
C    ISPD    IS THE SPEED INDEX TO TABLE 2-5, COMPUTED USING
C            OSVOBS(HOUR,LVL,9)
C    HR0124  LOCAL STANDARD TIME, 1 - 24 HOUR CLOCK
C    ANGL    CRITICAL SUN ANGLES USED IN DEFINING IRADX
C    ALF     COMPUTED ELEVATION ANGLE OF SUN (DEGREES)
C    TOP     USED IN SEARCHES FOR SUITABLE WIND DATA
C    BOTTOM  USED IN SEARCHES FOR SUITABLE WIND DATA
C    AUDIT   IF 0, DO NOT MAKE CALLS TO ERROR, DETAILING
C            REASONS FOR NO PG DETERMINATION. IF NE. ZERO,
C            MAKE CALLS TO ERROR.
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'WORK1.INC'
C
      DATA ANGL /60.,35.,15./
      DATA LSTAB /7,7,7,6,6,6,5,5,5,5,4,4,
     1            6,6,6,5,5,5,4,4,4,4,4,4,
     1            4,4,4,4,4,4,4,4,4,4,4,4,
     1            3,3,3,4,4,4,4,4,4,4,4,4,
     1            2,2,2,3,3,3,3,3,3,4,4,4,
     1            1,2,2,2,2,2,2,3,3,3,3,4,
     1            1,1,1,1,1,2,2,2,2,3,3,3/
C
C      INITIALIZE VALUES
C
      PATH = 'MP'
      LOC  = 'OS1PGT'
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
      ISPD      = 0
      IRADX     = 0
C
C     SET WIND SPEED
C
      IF( OSVOBS(HR0124,KEYLVL,9) .EQ. FLOAT(SFQA(23,2)) ) THEN
C        WIND SPEED MISSING SKIP TO NEXT HOUR
         MESS = BLNK40
         ECODE = 'T75'
         WRITE( MESS,3000 ) HR0124
3000     FORMAT(1X,'ISPD MISSING, HR ',I2,' PGSTAB .EQ. 0')
         IF(AUDIT.NE.0) CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
         RETURN
      ELSE
C     CONVERT WIND SPEED TO NEAREST KNOT
         ISPD = 1.94254*OSVOBS(HR0124,KEYLVL,9) + 0.5
         IF( ISPD .LT. 1 )  ISPD = 1
         IF( ISPD .GT. 12 ) ISPD = 12
      END IF
C
C     INITIALIZE SKY COVER FOR HOUR
C
      ISKY = OSSOBS(HR0124,19)
C
C   DEFINE SKY COVER
C
      IF( ISKY .EQ. OSTSKY(2) ) THEN
C     TSKY MISSING SKIP TO NEXT HOUR
         MESS = BLNK40
         ECODE = 'T75'
         WRITE( MESS,3500 ) HR0124
3500     FORMAT(1X,'OSTSKY MISSING, HR ',I2,' PGSTAB .EQ. 0')
         IF(AUDIT.NE.0) CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
         RETURN
      END IF
C
C     CONVERT CLOUD HEIGHT TO NEAREST HUNDREDS OF FEET
C
      CLHT = OSSOBS(HR0124,33)
      IF( CLHT .EQ. SFQA(33,2) ) THEN
         CLHT = 998
      ELSEIF( CLHT .EQ. 21 )THEN
         CLHT = 70
      ELSEIF( CLHT .EQ. 49 )THEN
         CLHT = 160
      ELSE
         CLHT = NINT (CLHT/0.3048)
      END IF
C
C***DETERMINE RADIATION INDEX.
C
      IF (ISKY .EQ. 10 .AND. CLHT .LT. 70) THEN
C        LOW/MIDDLE OVERCAST CLOUD LAYER: NEUTRAL CONDITIONS
         IRADX = 3
      ELSE IF (HR0124 .GT. TSR .AND. HR0124 .LT. TSS) THEN
C        DAYTIME CONDITIONS
         ALF = SOLANG(HR0124)
Cjop     CALL SOLAR(HR0124,ALF)
         DO 220 I=1,3
         IF (ALF .GT. ANGL(I)) GO TO 230
220      CONTINUE
         I=4
230      ICN=5-I
         IF (ISKY .GT. 5) GO TO 240
         IRADX=ICN+3
         GO TO 280
240      IRADX=ICN-1
         IF (CLHT .LT. 70) GO TO 250
         IF (CLHT .LT. 160) GO TO 260
         IF (ISKY .EQ. 10) GO TO 270
         IRADX=ICN
         GO TO 270
250      IRADX=ICN-2
         GO TO 270
260      IF (ISKY .EQ. 10) IRADX=IRADX-1
270      IF (IRADX .LT. 1) IRADX=1
         IRADX=IRADX+3
      ELSE
C        NIGHTTIME CONDITIONS
         IRADX=2
         IF (ISKY .LE. 4) IRADX=1
      END IF
C
C***DETERMINE STABILITY.
C
280   KST = LSTAB(ISPD,IRADX)
C
      IF (KST .LT. 1) THEN
         MESS = BLNK40
         ECODE = 'T75'
         WRITE( MESS,4000 ) ISPD,IRADX,HR0124
4000     FORMAT(1X,2I2,' ISPD,IRADX: PGSTAB(',I2,') .LT. 1')
         IF(AUDIT.NE.0) CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
      END IF
C
      RETURN
      END
C


      SUBROUTINE OSSAPG( AUDIT,HR0124,KST )
C=====================================================================**
C      PURPOSE:      THIS ROUTINE COMPUTES THE PASQUILL STABILITY
C                    CATEGORY USING THE ON SITE MEASUREMENTS OF
C                    LATERAL TURBULENCE FLUCTUATIONS( EITHER SA OR
C                    SA/US).
C---------------------------------------------------------------------
C     LOCAL VARIABLES
C
      INTEGER HR0124,KST,KSTF,ISPD,ISTAT,AUDIT
      REAL PGSIGA(5),PA(5),TOP,BOTTOM,MISS,SAHR,US,ZSA
C
C    AUDIT   IF 0, DO NOT MAKE CALLS TO ERROR, DETAILING
C            REASONS FOR NO PG DETERMINATION. IF NE. ZERO,
C            MAKE CALLS TO ERROR.
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'WORK1.INC'
C
      DATA PGSIGA/22.5,17.5,12.5,7.5,3.8/
      DATA PA/-0.06,-0.15,-0.17,-0.23,-0.38/
Cjop  DATA CONST/57.29578/             ADDED TO BLOCK DATA AS RAD2DG    JOP95APR
C
C      INITIALIZE VALUES
C
      PATH = 'MP'
      LOC  = 'OSSAPG'
C
C***INITIALIZE STABILITY BEFORE IT IS CALCULATED
C
      KST       = 0
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
C     1.  DETERMINE THE SD OF THE LATERAL
C         WIND DIRECTION FLUCTUATIONS.  THIS CAN
C         BE MEASURED DIRECTLY:
C              SAHR = OSVOBS(HOUR,LVL,2),
C         OR CAN BE ESTIMATED AS: (NO, DEACTIVATE UNTIL APPROVED)
C              SAHR = SV/US, WHERE
C                   SV = OSVOBS(HOUR,LVL,4),
C                   US = OSVOBS(HOUR,LVL,9).
C
C         A. FIRST DEFINE THE SURFACE ROUGHNESS LENGTH, AS
C            THIS IS USED TO DEFINE THE 'ACCEPTABLE' HEIGHTS
C            TO SEARCH ON IN DETERMINING SA.
C
      CALL ROUGH( AUDIT,HR0124,ISTAT )
C
C     CHECK ISTAT
C
      IF( ISTAT .EQ. 1 ) THEN
C        SURFACE ROUGHNESS MISSING
         MESS = BLNK40
         ECODE = 'T75'
         WRITE( MESS,1500 ) HR0124
1500     FORMAT(1X,'ZO MISSING FOR HOUR: ',I2)
         IF(AUDIT.NE.0) CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
      ELSE
C
C    DEFINE 'ACCEPTABLE' HEIGHT RANGE FOR MEASURED WINDS AND TURBULENCE
C
         TOP=AMAX1(100.*MPZO,10.)
         BOTTOM=AMAX1(20.*MPZO,1.)
C
C     TEST THAT TOWER DATA IS AT A SUITABLE HEIGHT
C
         IF( OSHT(KEYLVL) .LT. BOTTOM ) THEN
C           DATA NOT AT SUITABLE HEIGHT FOR ANALYSES
            MESS = BLNK40
            ECODE = 'T75'
            WRITE( MESS,2000 ) BOTTOM,HR0124
2000        FORMAT(1X,'ANEHGT BELOW ',F6.2,', HOUR: ',I2)
            IF(AUDIT.NE.0) CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
         END IF
C
         IF( OSHT(KEYLVL) .GT. TOP ) THEN
C           DATA NOT AT SUITABLE HEIGHT FOR ANALYSES
            MESS = BLNK40
            ECODE = 'T75'
            WRITE( MESS,2500 ) TOP,HR0124
2500        FORMAT(1X,'ANEHGT ABOVE ',F6.2,', HOUR: ',I2)
            IF(AUDIT.NE.0) CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
         END IF
C
      END IF
C
C         B. NEXT, WE CHECK THAT WE HAVE NONMISSING VALUES FOR
C            SA AND US, OR NOMISSING VALUES OF SV AND US, AT
C            THE USER'S DEFINED ANEHGT.
C
      IF( OSVOBS(HR0124,KEYLVL,9) .EQ. FLOAT(SFQA(23,2)) ) THEN
C        WIND SPEED MISSING SKIP TO NEXT HOUR
         MESS = BLNK40
         ECODE = 'T75'
         WRITE( MESS,3000 ) HR0124
3000     FORMAT(1X,'ISPD MISSING, HR ',I2,' PGSTAB .EQ. 0')
         IF(AUDIT.NE.0) CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
         RETURN
      END IF
C
      IF( OSVOBS( HR0124,KEYLVL,2) .EQ. FLOAT( SFQA(16,2))) THEN
C        NO SA AT KEYLVL FOR THIS HOUR
         MESS = BLNK40
         ECODE = 'T75'
         WRITE( MESS,3501 ) HR0124
3501     FORMAT(1X,'SA MISSING, HR ',I2,' PGSTAB .EQ. 0')
         IF(AUDIT.NE.0) CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
         RETURN
C
C ****  NEXT SECTION OF LOGIC COMPUTES SA = SV/U IF
C       SV DATA ARE AVAILABLE.   THIS IS NOT APPROVED
C       AS YET IN THE GUIDANCE.  HENCE, IT IS DEACTIVATED
C       FOR NOW.  THE "RETURN" LINE ABOVE SHOULD BE REMOVED
C       WHEN ACTIVATED.
C
C        IF( OSVOBS( HR0124,KEYLVL,4) .EQ. FLOAT( SFQA(19,2) ) THEN
C           NO SA OR SV DATA AT KEYLVL FOR THIS HOUR
C           MESS = BLNK40
C           ECODE = 'T75'
C           WRITE( MESS,3500 ) HR0124
C3500       FORMAT(1X,'SA & SV MISSING, HR ',I2,' PGSTAB .EQ. 0')
C           IF(AUDIT.NE.0) CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
C           RETURN
C        ELSE
C           SAHR = RAD2DG * OSVOBS( HR0124,KEYLVL,4 ) /
C    &                      OSVOBS( HR0124,KEYLVL,9 )
C           US = OSVOBS( HR0124,KEYLVL,9 )
C           ZSA = OSHT(KEYLVL)
C        END IF
C
      ELSE
         SAHR = OSVOBS( HR0124,KEYLVL,2 )
         US = OSVOBS( HR0124,KEYLVL,9 )
         ZSA = OSHT(KEYLVL)
      ENDIF
C
C     LOOP OVER STABILITY CLASSES TO ADJUST SIGMA-A RANGES FOR SITE
C     ROUGHNESS AND ANEMOMETER HEIGHT, AND COMPARE MEASURED SIGMA-A
C     WITH MODIFIED RANGE BOUNDARIES TO DETERMINE STABILITY CLASS
C     BASED ON SIGMA-A ALONE.
C
      DO 60 I=1,5
      IF( MPZO .GT. 0.15 ) THEN
         BOTTOM = PGSIGA(I) * (MPZO/0.15)**0.2 * (ZSA/10.0)**PA(I)
      ELSE
         BOTTOM = PGSIGA(I) * (ZSA/10.0)**PA(I)
      END IF
      IF(SAHR .GE. BOTTOM) THEN
         KST = I
         GO TO 70
      END IF
60    CONTINUE
      KST = 6
70    CONTINUE
C
      KSTF = KST
C
C     NOW ADJUST STABILITY CLASS FOR WIND SPEED STRENGTH AND TIME OF DAY
C     RAMMET TEST FOR DAY/NIGHT IMPLEMENTED ALTHOUGH THIS DOES NOT
C     CONFORM TO GUIDANCE FOR ON-SITE SIGMA-A SCHEME.
C
      IF( HR0124 .GT. TSR .AND. HR0124 .LT. TSS ) THEN
C          ADJUSTMENTS FOR DAYTIME:
           IF( KST .GE. 4 .OR. US .GE. 6.0 ) THEN
                KSTF = 4
           ELSE IF( US .GE. 4.0 ) THEN
                KSTF = 3
           ELSE IF( KST .EQ. 1 .AND. US .GE. 3.0 ) THEN
                KSTF = 2
           END IF
      ELSE
C          ADJUSTMENTS FOR NIGHTTIME:
           IF( KST .EQ. 1 .AND. US .LT. 2.9 ) THEN
                KSTF = 6
           ELSE IF( KST .EQ. 1 .AND. US .LT. 3.6 ) THEN
                KSTF = 5
           ELSE IF( KST .EQ. 2 .AND. US .LT. 2.4 ) THEN
                KSTF = 6
           ELSE IF( KST .EQ. 2 .AND. US .LT. 3.0 ) THEN
                KSTF = 5
           ELSE IF( KST .EQ. 3 .AND. US .LT. 2.4 ) THEN
                KSTF = 5
           ELSE IF( KST .EQ. 5 .AND. US .LT. 5.0 ) THEN
                KSTF = 5
           ELSE IF( KST .EQ. 6 .AND. US .LT. 3.0 ) THEN
                KSTF = 6
           ELSE IF( KST .EQ. 6 .AND. US .LT. 5.0 ) THEN
                KSTF = 5
           ELSE
                KSTF = 4
           END IF
      END IF
C
      KST = KSTF
C
      RETURN
      END
C
      SUBROUTINE HTLVLS( N,NTEST )
      NTEST = 2
      RETURN
      END
C
C
      SUBROUTINE OSINPG( AUDIT,HR0124,KST )
C=====================================================================**
C      PURPOSE:      THIS ROUTINE ACCEPTS THE PASQUILL STABILITY
C                    CATEGORY PROVIDED BY THE USER THROUGH THE
C                    OS DATA PATHWAY.
C-----------------------------------------------------------------------
C     LOCAL VARIABLES
C
      INTEGER HR0124,KST,AUDIT,KVAR
C
C    KST     PASQUILL STABILITY CATEGORY, AS DETERMINED BY
C            THIS ROUTINE.
C    HR0124  LOCAL STANDARD TIME, 1 - 24 HOUR CLOCK
C    AUDIT   IF 0, DO NOT MAKE CALLS TO ERROR, DETAILING
C            REASONS FOR NO PG DETERMINATION. IF NE. ZERO,
C            MAKE CALLS TO ERROR.
C    KVAR    INDEX FOR VARIABLE USED FOR STABILITY CLASS
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'WORK1.INC'
C
C
C      INITIALIZE VALUES
C
      PATH = 'MP'
      LOC  = 'OSINPG'
C
C***INITIALIZE STABILITY
C
      KST = 0
C
C***SET INDEX FOR VARIABLE USED FOR STABILITY CLASS --
C   (NOTE THAT VARIABLE MUST BE EITHER US01, US02, OR US03)
C
      KVAR = ISCVAR+11
C
C***CHECK FOR VALID STABILITY CLASS
C
      ITEST = NINT(OSSOBS(HR0124,KVAR))
      IF( ITEST .EQ. SFQA(KVAR,2) ) THEN
C        STABILITY CLASS MISSING SKIP TO NEXT HOUR
         MESS = BLNK40
         ECODE = 'T75'
         WRITE( MESS,3001 ) HR0124
3001     FORMAT(1X,'MISSING VALUE, HR ',I2,' PGSTAB .EQ. 0')
         IF(AUDIT.NE.0) CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
         RETURN
      ELSEIF (ITEST .LT. 1 .OR. ITEST .GT. 6) THEN
C        INVALID STABILITY CLASS, SKIP TO NEXT HOUR
         MESS = BLNK40
         ECODE = 'T75'
         WRITE( MESS,4000 ) HR0124,ITEST
4000     FORMAT(1X,' : PGSTAB(',I2,') NOT VALID  ',I2)
         IF(AUDIT.NE.0) CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
         RETURN
      ENDIF
C
C***ASSIGN STABILITY.
C
      KST = ITEST
C
      RETURN
      END


      SUBROUTINE HTKEY( KOUNT,ITEST )
C=====================================================================**
C      PURPOSE:     FILL IN ANY MISSING VALUES WITHIN ARRAY
C                   OSHT, AND IDENTIFY KEYLVL IF POSSIBLE.
C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C
      INTEGER ITEST,NREC
      REAL    MISS,SHGT
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'WORK1.INC'
C
C     INITIALIZE VALUES
C
      PATH = 'JB'
      LOC  = 'HTKEY '
      ITEST = 2
      NREC = 0
C
C      1) IF ANY ON-SITE HEIGHTS (osht) ARE MISSING, ATTEMPT TO FILL
C         IN THE MISSING VALUES BY SEARCHING THROUGH THE ARRAY
C         OSVOBS(-,-,1) FOR ALL HOURS OF THE DAY.
C
      IF( OSNL .GT. 0 ) THEN
         MISS = FLOAT( SFQA(15,2) )
         DO 20 I=1,OSNL
         IF( OSHT(I).EQ.MISS .OR. OSHT(I).EQ.0 ) THEN
C           SEE IF WE HAVE DEFINED THIS HEIGHT SOMEWHERE TODAY
            DO 10 J=1,24
            IF( OSVOBS(J,I,1).NE.MISS .AND. OSVOBS(J,I,1).GT.0.0 ) THEN
               OSHT(I) = OSVOBS(J,I,1)
            END IF
10          CONTINUE
         END IF
20       CONTINUE
      END IF

C-----------------------------------------------------------------------
C     2) Define KEYLVL, the on-site level associated with the anemometer
C        height (ANEHGT) for the P-G stability computation defined
C        on the "MP VBL STAB action" keyword statement.
C
      IF( CONTRL(4) .EQ. 1 ) THEN
C        The "action" is to use NWSXXX to compute P-G category
         IF( OSNL .LE. 0 ) THEN
            GO TO 60
         ELSE
            CONTINUE
         END IF
      END IF
C
      I = 1
      XRD1 = ABS( OSHT(1) - ANEHGT )
30    I = I + 1
C
      IF( I.GT.OSNL ) THEN
         IF( OSHT(OSNL).GT.0.0 ) THEN
            KEYLVL = OSNL
         END IF
      ELSE
         XRD2 = ABS( OSHT(I) - ANEHGT )
         IF( XRD1.GT.XRD2 ) THEN
            XRD1 = XRD2
            GO TO 30
         ELSE
            IF( OSHT(I-1).GT.0.0 ) THEN
               KEYLVL = I - 1
            END IF
         END IF
      END IF
C
C     DO WE HAVE WIND SPEED
C     AT THIS TOWER LEVEL?
C
      BUF04(1) = BLNK04
      BUF04(1)(1:2) = 'WS'
      WRITE( BUF04(1)(3:4),1000 ) KEYLVL
1000  FORMAT( I2.2 )
C
C     REWIND TEMPORARY FILE-70 WHERE WE PRESUMABLY STILL
C     HAVE A RECORD OF ALL THE INPUT IMAGES
C
      REWIND DEV70
C
C     NOW SEARCH FIRST FOR 'MAP' THEN FOR 'BUF04(1)'
C     IF SUCCESSFUL, THEN ALL IS OK.  IF NOT, THEN
C     USER HAS SPECIFIED A TOWER LEVEL NOT HAVING
C     THE REQUISITE WIND DATA.
C
40    CONTINUE
      IWORK1(10) = 0
      BUF80(1)   = BLNK80
      NREC = NREC + 1
      READ( DEV70,2000,END=50,ERR=50 ) BUF80(1)
2000  FORMAT( 8X,A80 )
      IWORK1(10) = INDEX( BUF80(1),'MAP' )
      IF( IWORK1(10).GT.0 ) THEN
         IWORK1(10) = 0
         IWORK1(10) = INDEX( BUF80(1),BUF04(1) )
         IF( IWORK1(10).GT.0 ) GO TO 60
      END IF
      GO TO 40
50    CONTINUE
C     DID NOT FIND WIND SPEED
      MESS = BLNK40
      WRITE( MESS,1050 ) BUF04(1)(1:2), KEYLVL, ANEHGT
1050  FORMAT(1X,'NO ',A2,' AT LVL',I2.2,'.  ANEHGT IS:',F6.1)
      IF( CONTRL(4).EQ.1 ) THEN
         ECODE = 'W06'
      ELSE
         ECODE = 'E06'
         ITEST = 1
      END IF
      CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
60    CONTINUE
C
Cdbg   PRINT *, 'OSHT   ',OSHT
Cdbg   PRINT *, 'OSNL,ANEHGT,KEYLVL ',OSNL,ANEHGT,KEYLVL

C-----------------------------------------------------------------------
C     Define TMPLVL, the on-site data level associated with the height
C     of the temperature sensor (TMPHGT), which is defined on the
C     "MP VBL TEMP action" keyword statement
C
      IF( CONTRL(2) .EQ. 1 ) THEN
C        The "action" is to use NWSXXX for the temperature
         IF( OSNL .LE. 0 ) THEN
            GO TO 180
         ELSE
            CONTINUE
         END IF
      END IF
C
      I = 1
      XRD1 = ABS( OSHT(1) - TMPHGT )
110   I = I + 1
C
      IF( I .GT. OSNL ) THEN
         IF( OSHT(OSNL) .GT. 0.0 ) THEN
            TMPLVL = OSNL
         END IF
      ELSE
         XRD2 = ABS( OSHT(I) - TMPHGT )
         IF( XRD1 .GT. XRD2 ) THEN
            XRD1 = XRD2
            GO TO 110
         ELSE
            IF( OSHT(I-1) .GT. 0.0 ) THEN
               TMPLVL = I - 1
            END IF
         END IF
      END IF
C
C     Did the user define a temperature variable for this level?
C
      NREC = 0
      BUF04(1) = BLNK04
      BUF04(1)(1:2) = 'TT'
      WRITE( BUF04(1)(3:4),1000 ) TMPLVL
C
C     REWIND TEMPORARY FILE-70 WHERE WE PRESUMABLY STILL
C     HAVE A RECORD OF ALL THE INPUT IMAGES
C
      REWIND DEV70
C
C     NOW SEARCH FIRST FOR 'MAP' THEN FOR 'BUF04(1)'
C     IF SUCCESSFUL, THEN ALL IS OK.  IF NOT, THEN
C     USER HAS SPECIFIED A TOWER LEVEL NOT HAVING
C     THE REQUESITE TEMPERATURE DATA.
C
165   CONTINUE
      IWORK1(10) = 0
      BUF80(1)   = BLNK80
      NREC = NREC + 1
      READ( DEV70,2000,END=170,ERR=170 ) BUF80(1)
      IWORK1(10) = INDEX( BUF80(1),'MAP' )
      IF( IWORK1(10).GT.0 ) THEN
         IWORK1(10) = 0
         IWORK1(10) = INDEX( BUF80(1),BUF04(1) )
         IF( IWORK1(10).GT.0 ) GO TO 180
      END IF
      GO TO 165
170   CONTINUE
C     DID NOT FIND TEMPERATURE IN DATA MAP
      MESS = BLNK40
      WRITE( MESS,1070 ) BUF04(1)(1:2), TMPLVL, TMPHGT
1070  FORMAT(1X,'NO ',A2,' AT LVL',I2.2,'.  TMPHGT IS:',F6.1)
      IF( CONTRL(2) .EQ. 1 ) THEN
         ECODE = 'W06'
      ELSE
         ECODE = 'E06'
         ITEST = 1
      END IF
      CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
180   CONTINUE
C
Cdbg   PRINT *, 'OSNL,TMPHGT,TMPLVL ',OSNL,TMPHGT,TMPLVL

C-----------------------------------------------------------------------
C     If the source height is defined, save it in a temporary variable;
C     if not, use the anemometer height, which defaults to 10 meters
C     if the user does not specify it on a MP VBL STAB card
      IF( STKHGT.GT.0.0 ) THEN
         SHGT = STKHGT
      ELSE
         SHGT = ANEHGT
         STKHGT = ANEHGT
      END IF
C
C     Define STKLVL, the level associated with the source height (STKHGT)
C     that is defined on the "MP VBL WIND action" keyword statement
C
      IF( CONTRL(1) .EQ. 1 ) THEN
C        The "action" is to use NWSXXX for the 
         IF( OSNL.LE.0 .OR. SHGT.LE.0.0 ) THEN
            GO TO 245
         ELSE
           CONTINUE
         END IF
      END IF
C
      I = 1
      XRD1 = ABS( OSHT(1) - SHGT )
210   I = I + 1
C
      IF( I .GT. OSNL ) THEN
         IF( OSHT(OSNL) .GT. 0.0 ) THEN
            STKLVL = OSNL
         END IF
      ELSE
         XRD2 = ABS( OSHT(I) - SHGT )
         IF( XRD1 .GT. XRD2 ) THEN
            XRD1 = XRD2
            GO TO 210
         ELSE
            IF( OSHT(I-1) .GT. 0.0 ) THEN
               STKLVL = I - 1
            END IF
         END IF
      END IF
C
C     Did the user define a wind speed variable for this level?
C
      NREC = 0
      BUF04(1) = BLNK04
      BUF04(1)(1:2) = 'WS'
      WRITE( BUF04(1)(3:4),1000 ) STKLVL
C
C     REWIND TEMPORARY FILE-70 WHERE WE PRESUMABLY STILL
C     HAVE A RECORD OF ALL THE INPUT IMAGES
C
      REWIND DEV70
C
C     NOW SEARCH FIRST FOR 'MAP' THEN FOR 'BUF04(1)'
C     IF SUCCESSFUL, THEN ALL IS OK.  IF NOT, THEN
C     USER HAS SPECIFIED A TOWER LEVEL NOT HAVING
C     THE REQUESITE WIND DATA.
C
220   CONTINUE
      IWORK1(10) = 0
      BUF80(1)   = BLNK80
      NREC = NREC + 1
      READ( DEV70,2000,END=225,ERR=225 ) BUF80(1)
      IWORK1(10) = INDEX( BUF80(1),'MAP' )
      IF( IWORK1(10).GT.0 ) THEN
         IWORK1(10) = 0
         IWORK1(10) = INDEX( BUF80(1),BUF04(1) )
         IF( IWORK1(10).GT.0 ) GO TO 230
      END IF
      GO TO 220
225   CONTINUE
C     DID NOT FIND WIND SPEED IN DATA MAP
      MESS = BLNK40
      WRITE( MESS,1080 ) BUF04(1)(1:2), STKLVL, SHGT
1080  FORMAT(1X,'NO ',A2,' AT LVL',I2.2,'.  SHGT IS:',F6.1)
      IF( CONTRL(1) .EQ. 1 ) THEN
         ECODE = 'W06'
      ELSE
         ECODE = 'E06'
         ITEST = 1
      END IF
      CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
230   CONTINUE
C
C     Did the user define a wind direction variable at this level?
C
      NREC = 0
      BUF04(1) = BLNK04
      BUF04(1)(1:2) = 'WD'
      WRITE( BUF04(1)(3:4),1000 ) STKLVL
C
C     REWIND TEMPORARY FILE-70 WHERE WE PRESUMABLY STILL
C     HAVE A RECORD OF ALL THE INPUT IMAGES
C
      REWIND DEV70
C
C     NOW SEARCH FIRST FOR 'MAP' THEN FOR 'BUF04(1)'
C     IF SUCCESSFUL, THEN ALL IS OK.  IF NOT, THEN
C     USER HAS SPECIFIED A TOWER LEVEL NOT HAVING
C     THE REQUESITE WIND DATA.
C
235   CONTINUE
      IWORK1(10) = 0
      BUF80(1)   = BLNK80
      NREC = NREC + 1
      READ( DEV70,2000,END=240,ERR=240 ) BUF80(1)
      IWORK1(10) = INDEX( BUF80(1),'MAP' )
      IF( IWORK1(10).GT.0 ) THEN
         IWORK1(10) = 0
         IWORK1(10) = INDEX( BUF80(1),BUF04(1) )
         IF( IWORK1(10).GT.0 ) GO TO 245
      END IF
      GO TO 235
240   CONTINUE
C     DID NOT FIND WIND DIRECTION IN DATA MAP
      MESS = BLNK40
      WRITE( MESS,1080 ) BUF04(1)(1:2), STKLVL, SHGT
      IF( CONTRL(1) .EQ. 1 ) THEN
         ECODE = 'W06'
      ELSE
         ECODE = 'E06'
         ITEST = 1
      END IF
      CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
C
245   CONTINUE
C
Cdbg   PRINT *, 'OSNL,SHGT,STKLVL ',OSNL,SHGT,STKLVL
C
      RETURN
      END
C
C
      SUBROUTINE ROUGH( AUDIT,HR0124,ISTAT )
C=====================================================================**
C      PURPOSE:      THIS ROUTINE DETERMINES THE LOCAL SURFACE ROUGHNESS
C                    LENGTH, USING THE LOWEST OBTAINABLE LEVEL OF
C                    NONMISSING ON-SITE MEASUREMENTS OF WIND DIRECTION.
C
C     NOTE:  THE SITE ROUGHNESS IS CONTAINED IN ARRAY OSSFC(IMONTH,
C            IWIND,3) WHERE IWIND IS A WIND SECTOR.  MPZO IS IN METERS.
C            A DEFAULT VALUE OF 0.15 m IS PLACED IN WIND SECTOR 1, AND
C            VALUES IN ALL OTHER SECTORS ARE ZERO.  WE ASSUME THAT ANY
C            VALID ROUGHNESS WILL BE GREATER THAN ZERO.
C
C-----------------------------------------------------------------------
C     LOCAL VARIABLES
C
      INTEGER I,HR0124,ISTAT,AUDIT
      REAL    WD,MISS
C
C     HR0124  LOCAL STANDARD TIME IN 1 - 24 HOUR CLOCK
C     ISTAT   STATUS KEYWORD
C     WD      TEMPORARY LOCATION FOR OS WIND DIRECTION
C     AUDIT   IF 0, DO NOT MAKE CALLS TO ERROR, DETAILING
C             REASONS FOR NO PG DETERMINATION. IF NE. ZERO,
C             MAKE CALLS TO ERROR.
C
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'WORK1.INC'
C
C      INITIALIZE VALUES
C
      PATH = 'MP'
      LOC  = ' ROUGH'
      ISTAT = 2
C
C     INITIALIZE ROUGHNESS TO THAT LISTED FOR WIND SECTOR 1
C
      MPZO=OSSFC(MPCMO,1,3)
C
C     RETURN IF ROUGHNESS FOR ANOTHER WIND SECTOR IS NOT NEEDED
C     (NO SECTOR IS NEEDED IF THE ROUGNESS IS UNIFORM)
C
      IF(OSSFC(MPCMO,2,3) .EQ. 0.0) RETURN
C
C
C     1.  FIND LOWEST LEVEL OF ON SITE WIND DIRECTION MEASUREMENTS
C         THAT ARE NONMISSING.   NOTE, WE SKIP LEVELS BELOW 1 METER.
C
      I = 0
10    I = I + 1
      IF( OSHT(I) .GT. 1.0 ) GO TO 20
C
15    IF( I .LT. OSNL ) GO TO 10
C
C     NO DATA AT SUITABLE HEIGHT FOR ANALYSES
C
      MESS = BLNK40
      ECODE = 'T76'
      WRITE( MESS,1000 ) HR0124
1000  FORMAT(1X,' NO WD FOR ZO, HOUR ',I2)
      IF(AUDIT.NE.0) CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
      ISTAT = 1
      RETURN
C
20    CONTINUE
C
      MISS = FLOAT( SFQA(22,2) )
      IF( OSVOBS( HR0124,I,8) .EQ. MISS ) GO TO 15
      WD = OSVOBS( HR0124,I,8 )
C
C  DEBUG STATEMENT
Cdbg   PRINT *,'WD =',WD,' AT LEVEL',I,' ON DAY',MPJDY,' AT HOUR',HR0124
C
C     2.  NOW SEARCH WITHIN OSWDS ARRAY, TO FIND WHICH
C         WIND DIRECTION SECTOR WE ARE IN.
C
      IRD4 = 0
30    IRD4 = IRD4 + 1
C
C     PROVIDE FOR SPECIAL CASE WHEN USER DEFINED WIND DIRECTION
C     SECTORS, SPANS ACROSS NORTH ( 0 DEGREES ).
C
      IF( OSWDS(IRD4,1) .LT. OSWDS(IRD4,2) ) THEN
         IF( WD .GT. OSWDS(IRD4,1) .AND. WD .LE. OSWDS(IRD4,2) ) THEN
            MPZO = OSSFC( MPCMO,IRD4,3 )
            RETURN
         END IF
      ELSE
         IF( WD .GT. OSWDS(IRD4,1) .AND. WD .LE. 360.0
     1       .OR. WD .GE. 0.0 .AND. WD .LE. OSWDS(IRD4,2) ) THEN
            MPZO = OSSFC( MPCMO,IRD4,3 )
            RETURN
         END IF
      END IF
C
      IF( IRD4 .LT. OSNWDS ) GO TO 30
C
C    ERROR CONDITION, GIVEN WIND DIRECTION IS NOT
C    FOUND TO BE INCLUDED WITHIN ANY OF THE USER
C    DEFINED WIND DIRECTION SECTORS.
C
      MESS = BLNK40
      ECODE = 'W76'
      WRITE( MESS,2000 ) HR0124,WD
2000  FORMAT(1X,'WD(',I2,') = ',F5.1,' NOT IN WD-SECTORS')
      CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
      ISTAT = 1
      RETURN
      END