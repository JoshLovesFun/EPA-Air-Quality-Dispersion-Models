      SUBROUTINE MPMET( ISTAT )
C=====================================================================**
C      PURPOSE:      THIS ROUTINE CONTROLS THE CALLS TO THREE
C                    SUBROUTINES THAT IN TURN PROVIDE THE
C                    FOLLOWING ACTIONS:
C                    1) FETCH ONE DAY'S MERGED METEOROLOGY
C                    2) PROCESS (AS REQUIRED BY USER'S
C                       DEFINED DIFFUSION MODEL) METEOROLOGY
C                    3) WRITE RESULTS TO OUTPUT MET FILE
C                       ACCORDING TO REQUIREMENTS LEVIED BY
C                       USER'S DEFINED DIFFUSION MODEL.
C
C      THE FIRST ACTION TAKEN BY THIS ROUTINE IS TO READ
C      PAST THE HEADERS (IF ANY) WITHIN THE MERGED INPUT
C      METEOROLOGY.  WE DO NOT NEED TO STORE OFF THESE AS
C      THIS HAS BEEN ACCOMPLISHED DURING THE SETUP
C      PROCESSING.
C-----------------------------------------------------------------------
C---- Data declarations
C
      INTEGER MP2YR, MP4YR, CENTURY
      INTEGER NUMBER,NHDR,ISTAT,NDUM,MPCRDY
C
C      NUMBER   DAY BEING PROCESSED (JULIAN)
C      NHDR     NUMBER OF HEADERS PROCESSED
C      NDUM     NUMBER OF HOURS NEEDED TO CONVERT  SOLAR TIME
C               TO GMT TIME.  NDUM = INT( GMT TIME - SOLAR TIME),
C               IN HOURS.  NOTE IN USA, LOCAL STANDARD TIME IS
C               FAIRLY CLOSE TO SOLAR TIME (POSITION OF SUN IN
C               SKY).
C      ISTAT    STATUS OF PROCESSING
C               0   SITUATION OK
C               1   ERRORS HAVE OCCURRED
C               2   REACHED EOF OF MERGE DATA
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'UA1.INC'
      INCLUDE 'UA2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'WORK1.INC'
C
C---- Initialize values
C
      PATH = 'MP'
      LOC  = ' MPMET'
Cjop  CONST =  57.29578  ADDED TO BLOCK DATA AS RAD2DG                  JOP95APR
C
C---- Read the headers
C
      REWIND DEV40
      CALL EHANDL( 213 )
      NHDR = 0
10    READ(DEV40,END=40) BUF03
      IF(BUF03(1:1) .EQ. '*') THEN
         NHDR = NHDR + 1
         GO TO 10
      ENDIF
C
C---- Position for read of data
C
20    BACKSPACE DEV40
C
C     Using on-site location data, retrieve the station
C     latitude for later use in determining sunrise, sunset,
C     and angle of sun to horizon.
C
      CALL LATLON( 1,1,OSLAT,MPLAT,ISTAT )
C
C---- Check status of computation
      IF( ISTAT.EQ.1 ) THEN
         MESS = BLNK40
         ECODE = 'E70'
         WRITE( MESS,1000 ) OSLAT
1000     FORMAT(' ERROR CNVRT OSLAT (',A8,') TO REAL #')
         CALL ERROR( 0,PATH,ECODE,LOC,MESS )
         RETURN
      END IF
C
      CALL LATLON( 1,2,OSLON,MPLON,ISTAT )
C
C---- Check status of computation
      IF( ISTAT.EQ.1 ) THEN
         MESS = BLNK40
         ECODE = 'E70'
         WRITE( MESS,2000 ) OSLON
2000     FORMAT(' ERROR CNVRT OSLON (',A8,') TO REAL #')
         CALL ERROR( 0,PATH,ECODE,LOC,MESS )
         RETURN
      END IF
C
C---- Check that our 'time-zone' conversion factor
C     makes sense (i.e. the correct sign and magnitude)
C
      IF( MPLON .GT. 0 ) THEN
         NDUM = INT( (MPLON+7.5)/15.0 )
      ELSE
         NDUM = INT( (MPLON-7.5)/15.0 )
      END IF
C
      IF( ABS( ZONE-NDUM ) .GT. 2 ) THEN
         MESS = BLNK40
         ECODE = 'W70'
         WRITE( MESS,3000 )
3000     FORMAT(1X,'WARNING: GMT-LST CONV. LOOKS SUSPECT')
         CALL ERROR( 0,PATH,ECODE,LOC,MESS )
      END IF
C
C
C---- Fetch day's worth of meteorology from merge data file
C
  30  CALL FETCH( ISTAT )
      NUMBER = MPJDY

      CALL Y2K(PATH, MPYR, MP2YR, MP4YR, CENTURY)
      WRITE( *, 35 ) MPJDY, MP4YR
  35  FORMAT( '+  Stage 3: Processing Julian day and year ',I3.3, I6 )
C
C---- Compute chronological day
C
      CALL CHROND( PATH,MPYR,MPJDY,MPCRDY )
C
C---- Check of computation
C
      IF( ISTAT.NE.2 ) THEN
         RETURN
      END IF
C
C---- Check dates and see if the data are within
C     processing 'window'; the dates are defined
C     only if user included the 'MP EXT ... ' card,
C     which would set STATUS(6,5) to 2.
C
      IF( STATUS(6,5).EQ.2 ) THEN
C
         IF( MPCRDY.LT.JBCDY1 ) THEN
            GO TO 30
         END IF
C
         IF( MPCRDY.GT.JBCDY2 ) THEN
            MESS = BLNK40
            ECODE = 'I79'
            WRITE( MESS,5000 )
5000        FORMAT(1X,' END OF EXTRACT WINDOW FOUND ')
            CALL ERROR( NUMBER,PATH,ECODE,LOC,MESS )
            ISTAT = 3
            RETURN
         END IF
C
      END IF
C
C---- So long as KEYLVL remains undefined, call
C     subroutine HTKEY to 1) make sure we have defined
C     values within array OSHT, and 2) define KEYLVL if
C     possible.
C
      IF( KEYLVL.EQ.0 ) THEN
         CALL HTKEY( NUMBER,ISTAT )
         IF( ISTAT.EQ.1 ) THEN
            JBSTAT = -1
            RETURN
         END IF
      END IF
C
C---- Process day's meteorology
      CALL MODEL( ISTAT )
C
C---- Check status of the processing
      IF( ISTAT.NE.2 ) THEN
         RETURN
      END IF
C
C---- Write to met output file as needed
C
C---- Call output for hourly models
C     model 14 = ISCST (ASCII),
C           15 = ISCSTDRY,
C           16 = ISCSTWET,
C           17 = ISCGASD (gas dry deposition),
C           18 = ISCGASW (gas wet deposition)

      IF( MDSTAT.LE.8 .OR. MDSTAT.GE.14 ) THEN
         CALL MPOUT( ISTAT )
      END IF
C
C---- Check status of the output processing
      IF( ISTAT.NE.2 ) THEN
         RETURN
      END IF
C
C---- Go back and attempt fetch of next day's meteorology
C
      GO TO 30
C
C---- This point is reached if an end of file is encountered
C     while reading headers of merge file.
C
40    CONTINUE
      MESS = BLNK40
      ECODE = 'E70'
      WRITE( MESS,6000 )
6000  FORMAT(1X,'HEADERS PROCESSED.  THEN FOUND EOF')
      CALL ERROR( NHDR,PATH,ECODE,LOC,MESS )
      ISTAT =  1
      RETURN
C
      END
C


      SUBROUTINE MODEL( TEST )
C=====================================================================**
C      PURPOSE:      Calls the routines to process the day's meteorology
C                    as needed to formulate the met output for the
C                    specified diffusion model.
C-----------------------------------------------------------------------
C     LOCAL VARIABLES
C
      INTEGER TEST,AUDIT
C
C     TEST  STATUS OF PROCESSING
C           1   ERRORS HAVE OCCURRED
C           2   ALL OK
C     AUDIT IF ZERO, ONLY PRINT SUMMARY MESSAGES REGARDING
C           PROCESSING RESULTS.  IF NE. ZERO, PRINT DETAILED
C           AUDIT TRAIL OF RESULTS.
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'WORK1.INC'
C
C      INITIALIZE VALUES
C
      PATH = 'MP'
      LOC  = ' MODEL'
C
C---- Check the STATUS array to determine if the TRA option is set
      IF( STATUS(6,8).EQ.2 ) THEN
         AUDIT = 1
      ELSE
         AUDIT = 0
      END IF
C
C---- Compute sunrise and sunset and solar elevation angle for
C     all 24 hours of the day
      CALL SUNDAT
C
C---- Define surface layer stability
      CALL SFSTAB( AUDIT,CONTRL(7),CONTRL(8),CONTRL(4),TEST )
C
C---- Define mixing heights
C     NOTE: if model is CALINE-3, force use of on site mixhgt
      IF( MDSTAT .EQ. 7 ) THEN
         CONTRL(3) = 2
      END IF
      CALL MIXHGT( AUDIT,CONTRL(3),TEST )
C
C---- Define array of output heights (if need be)
      CALL HTLVLS( CONTRL(9),TEST )
C
C---- Define wind speeds and directions
      CALL MPWIND( AUDIT,CONTRL(1),TEST )
C
C---- Define temperatures
      CALL MPTEMP( AUDIT,CONTRL(2),CONTRL(5),TEST )
C
C---- Define turbulence values - currently SUBR.MPTURB does nothing-----
      CALL MPTURB( CONTRL(6),TEST )

C---- Call the routine to develop the boundary layer parameters
C     (friction velocity, Monin-Obukhov length, precipitation
C     data) for particle and gas deposition processes in ISCST

      IF( MDSTAT .GE. 15 )THEN
         CALL DEPMET ( TEST )
      ENDIF
C
C---- Save off statistics
      CALL MPSTAT( 0 )
C
      RETURN
      END
C


      SUBROUTINE SUNDAT
C=====================================================================**
C      PURPOSE:    Compute sunrise and sunset, solar elevation
C                  angle for all hours of the day
C-----------------------------------------------------------------------
C      LOCAL VARIABLES
C
      REAL   DAYNO,TDAYNO,SIND,COSD,SINTD,COSTD,SIGMA,
     1       H2,HCOS,HI,ALFSN
      REAL   AMM,TEMPZ,DUM,SINLAT,COSLAT,DSIN,DCOS
C
      INCLUDE 'MP1.INC'
C
C---- Determine the fraction of a year for this date.
C        (0.0172028 = 360.0/365.242*57.29578)
      DUM    = MPLON/15.0 - ZONE
      TEMPZ  = 15.0*ZONE - MPLON
      SINLAT = SIN( MPLAT/RAD2DG )
      COSLAT = COS( MPLAT/RAD2DG )
C
      DAYNO  = (MPJDY-1.0)*0.0172028
      TDAYNO = 2.0*DAYNO
      SIND   = SIN(DAYNO)
      COSD   = COS(DAYNO)
      SINTD  = SIN(TDAYNO)
      COSTD  = COS(TDAYNO)
C
C---- Account for ellipticity of earth's orbit.
      SIGMA = 279.9348 + (DAYNO*RAD2DG) + 1.914827*SIND -
     &        0.079525*COSD + 0.019938*SINTD - 0.00162*COSTD

C---- Find the sine of the solar declination.
C        0.39785 = sin(0.4091720193) = sin(23.44383/57.29578)
      DSIN = 0.39785*SIN(SIGMA/RAD2DG)
      DCOS = SQRT(1.0-DSIN*DSIN)

C---- Determine time(hrs) of meridian passage
      AMM = 12.0 + 0.12357*SIND - 0.004289*COSD + 0.153809*SINTD +
     &      0.060783*COSTD
      HCOS = (-SINLAT*DSIN)/(COSLAT*DCOS)

C---- Determine solar hour angle of sunrise-sunset.
      H2 = (ATAN2(SQRT(1.0-HCOS*HCOS),HCOS)/15.0)*RAD2DG

C---- Time of sunrise(TSR) and time of sunset(TSS) are expressed in
C     local standard time since the zone correction has already been made.
C     Otherwise they would be in Greenwich Mean Time.

      TSR = AMM - H2 + DUM
      TSS = AMM + H2 + DUM
C
C---- Determine solar hour angle(in radians) for each hour of the day
      DO 100 IHOUR = 1,24
          HI = (15.0 * (IHOUR-AMM) + TEMPZ) / RAD2DG
          ALFSN = SINLAT*DSIN + DCOS*COSLAT*COS(HI)

C----     Determine solar elevation angle(in degrees)
          SOLANG(IHOUR) = ATAN2(ALFSN,SQRT(1.0-ALFSN*ALFSN)) * RAD2DG
 100  CONTINUE
C
      RETURN
      END
C


      SUBROUTINE FETCH( ITEST )
C=====================================================================**
C     PURPOSE:    Routine fetchs next day's data.  This includes
C                 NWS upper air soundings and NCDC mixing heights,
C                 NWS sfc hourly observations, and on-site hourly
C                 observations, to the extent these data are present.
C-----------------------------------------------------------------------
C     Data declarations

      INTEGER JV,JLEV,IVBL,ITEST,HOUR,COUNT
C
C     JV,JLEV,IVBL      ARE USED AS DUMMY INDEX VALUES
C     ITEST             STATUS OF PROCESS
C                       1 = ERROR
C                       2 = ALL OK
C                       3 = EOF FOUND
C     COUNT             INTERNAL COUNTER, 1 = FIRST DAY
C                       PROCESSED, 2 = SECOND, ETC.  USEFUL
C                       IF READ ERROR OCCURS WHILE FETCHING
C                       A MASTER HEADER, AS MPJDY IS THEN IN
C                       ERROR.
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'UA1.INC'
      INCLUDE 'UA2.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'WORK1.INC'
C
      DATA COUNT/0/
C
C     INITALIZE VALUES
C
      PATH = 'MP'
      LOC  = ' FETCH'
      COUNT = COUNT + 1
C
C     'FLUSH', SET OT MISSING, ZI-ARRAYS, WORK-ARRAYS,
C     SF-ARRAYS AND OS-ARRAYS.
C
      CALL FLZI
      CALL FLIWK1
      CALL FLSDG(UAMH)
      CALL FLSFC(SFMH)
      CALL FLOS(OSMH)
C
      DO 5 M=1,24
         SFYR(M)  = 0
         SFMO(M)  = 0
         SFDAY(M) = 0
         SFHR(M)  = 0
5     CONTINUE
C
C
C     1.  READ CURRENT DAY'S MASTER HEADER
C
      READ(DEV40,END=700,ERR=701,IOSTAT=IRD4)  MPYR,MPCMO,MPCDY,MPJDY,
     & MIXP1,MIXP2,MIXC1,MIXC2,MIXN1,MIXN2,IWORK1(20),
     & IWORK1(21),IWORK1(22)
C
C    IWORK1(20)   = NUMBER OF UPPER AIR OBS. TO WRITE FOR THE MERGE DAY
C    IWORK1(21)   = NUMBER OF SFC. OBS. TO WRITE FOR THE MERGE DAY
C    IWORK1(22)   = NUMBER OF ONSITE OBS. TO WRITE FOR THE MERGE DAY
C
C  DEBUG STATEMENTS USEFUL FOR 'WATCHING' HEADER RECORDS
C        FOR EACH DAY GO BY.  USEFUL FOR INSURING THAT PROPER
C        DATA IS EXTRACTED.
C
C      WRITE(DEV50,5599) MPYR,MPCMO,MPCDY,MPJDY,IWORK1(20),IWORK1(21),
C     &                  IWORK1(22),MIXP1,MIXP2,MIXC1,MIXC2,MIXN1,MIXN2
C
C     2.  READ CURRENT DAY'S UA DATA, IF PRESENT.
C
      IF(IWORK1(20).GT.0) THEN
C
         DO 10 M=1,IWORK1(20)
            READ(DEV40,END=710,ERR=711,IOSTAT=IRD4) UAYR(M),UAMO(M),
     &                                      UADAY(M),UAHR(M),UALEV(M)
C
C DEBUG WRITE STATEMENT
C
C        WRITE(DEV50,5500) UAYR(M),UAMO(M),UADAY(M),UAHR(M),UALEV(M)
C
            IF( UALEV(M).GT.0 ) THEN
C
               READ(DEV40,END=712,ERR=713,IOSTAT=IRD4)
     &          ((UAOBS(M,JLEV,JV),JV=1,UAMV),JLEV=1,UALEV(M))
C
C DEBUG WRITE STATEMENTS
C
C        DO 2 JV = 1,UALEV(M)
C         WRITE(DEV50,5501) (UAOBS(M,JV,IVBL),IVBL=1,6)
C    2   CONTINUE
C
            END IF
C
10       CONTINUE
C
      ENDIF
C
C     3.  READ CURRENT DAY'S SF DATA, IF PRESENT.
C
      IF(IWORK1(21) .GT. 0) THEN
C
         DO 20 M = 1,IWORK1(21)
            READ(DEV40,END=720,ERR=721,IOSTAT=IRD4) (IWORK1(I),I=1,4)
C
            HOUR        = IWORK1(4)
C
C       CHECK HOUR, MUST BE LESS THAN 25, OR WE HAVE AN ERROR
C
            IF( HOUR.LT.1 .OR. HOUR.GT.24 ) THEN
               MESS = BLNK40
               ECODE = 'E71'
               WRITE( MESS,1000 )
1000           FORMAT(1X,'SF OBS. NOT ON 1 - 24 HOUR CLOCK')
               CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
               ITEST = 1
               RETURN
            END IF
C
            SFYR(HOUR)  = IWORK1(1)
            SFMO(HOUR)  = IWORK1(2)
            SFDAY(HOUR) = IWORK1(3)
            SFHR(HOUR)  = IWORK1(4)
C
            READ(DEV40,END=722,ERR=723,IOSTAT=IRD4)
     &       (SFOBS(HOUR,IVBL+29),IVBL=1,23)
C
C  DEBUG WRITE STATMENTS
C
C      WRITE(DEV50,5502) SFYR(HOUR),SFMO(HOUR),SFDAY(HOUR),SFHR(HOUR),
C     &    (SFOBS(HOUR,IVBL+29),IVBL=1,23)
C
20       CONTINUE
C
C     SPECIAL LOGIC TO HANDLE MISSING DATA FOR HOUR 24
C     WHEN OTHER 23 HOURS ARE PRESENT.  THIS IS A 'NICE'
C     TO HAVE FEATURE FOR MODELS FEEDING ON 'RAMMET' TYPE
C     METEOROLOGY.
C
         IF( MDSTAT.LE.5 .OR. MDSTAT.GE.14 ) THEN
C
            IF( IWORK1(21).EQ.23 .AND. SFYR(24).EQ.0 ) THEN
C
               SFYR(24)  = SFYR(23)
               SFMO(24)  = SFMO(23)
               SFDAY(24) = SFDAY(23)
               SFHR(24)  = 24
C
               DO 25 IVBL=1,23
                  SFOBS(24,IVBL+29) = SFOBS(23,IVBL+29)
25             CONTINUE
C
C       WRITE WARNING TO ERROR FILE
C
               MESS = BLNK40
               ECODE = 'W70'
               WRITE( MESS,1500 )
1500           FORMAT(1X,'SWAPPED HR 23 INTO HR 24 FOR SF-DATA')
               CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
C
            END IF
         END IF
      ENDIF
C
C     4.  READ CURRENT DAY'S OS DATA, IF PRESENT.
C
      IF(IWORK1(22) .GT. 0) THEN
C
         DO 30 M =1,IWORK1(22)
            CALL OSREAD(6,MPJDY,HOUR,ITEST)
C
C  DEBUG WRITE STATEMENTS
C
C      CALL OSPRNT( MPJDY )
C
C       CHECK STATUS
C
            IF(ITEST .EQ. 2) THEN
               CALL OSSWAP(HOUR)
C
            ELSE IF (ITEST .EQ. 3) THEN
               GO TO 730
            ELSE
               GO TO 731
            ENDIF
C
30       CONTINUE
C
      ENDIF
C
      ITEST = 2
      RETURN
C
C=======================================================================
C-FORMAT STATEMENTS
C
 5500 FORMAT(1X,4I2,1X,I3,6(1X,I5))
 5501 FORMAT(6(1X,I5))
 5502 FORMAT(1X,4I2,10(1X,I5),/,8X,12(1X,I5))
 5598 FORMAT(1X,A3,A80)
 5599 FORMAT(1X,7(1X,I10))
C
C=======================================================================
C- EOF AND ERROR CONDITIONS
  700 CONTINUE
      MESS = BLNK40
      ECODE = 'I79'
      WRITE( MESS,2000 )
2000  FORMAT(1X,'EOF FOUND ON MERGED INPUT DATA FILE')
      CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
      ITEST = 3
      RETURN
C
  701 CONTINUE
      MESS = BLNK40
      ECODE = 'E71'
      WRITE( MESS,2500 ) IRD4
2500  FORMAT(1X,'IOSTAT= ',I8,' FETCHING MASTER HEADER')
      CALL ERROR( COUNT,PATH,ECODE,LOC,MESS )
      ITEST = 1
      RETURN
C
  710 CONTINUE
      MESS = BLNK40
      ECODE = 'E71'
      WRITE( MESS,2600 )
2600  FORMAT(1X,'EOF FOUND FETCHING UA HEADER')
      CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
      ITEST = 1
      RETURN
C
  711 CONTINUE
      MESS = BLNK40
      ECODE = 'E71'
      WRITE( MESS,2700 ) IRD4
2700  FORMAT(1X,'IOSTAT= ',I8,' FETCHING UA HEADER')
      CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
      ITEST = 1
      RETURN
C
  712 CONTINUE
      MESS = BLNK40
      ECODE = 'E71'
      WRITE( MESS,2800 )
2800  FORMAT(1X,'EOF FOUND FETCHING UA DATA')
      CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
      ITEST = 1
      RETURN
C
  713 CONTINUE
      MESS = BLNK40
      ECODE = 'E71'
      WRITE( MESS,2900 ) IRD4
2900  FORMAT(1X,'IOSTAT= ',I8,' FETCHING UA DATA')
      CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
      ITEST = 1
      RETURN
C
  720 CONTINUE
      MESS = BLNK40
      ECODE = 'E71'
      WRITE( MESS,3000 )
3000  FORMAT(1X,'EOF FOUND FETCHING SF HEADER')
      CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
      ITEST = 1
      RETURN
C
  721 CONTINUE
      MESS = BLNK40
      ECODE = 'E71'
      WRITE( MESS,3500 ) IRD4
3500  FORMAT(1X,'IOSTAT= ',I8,' FETCHING SF HEADER')
      CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
      ITEST = 1
      RETURN
C
  722 CONTINUE
      MESS = BLNK40
      ECODE = 'E71'
      WRITE( MESS,3600 )
3600  FORMAT(1X,'EOF FOUND FETCHING SF DATA')
      CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
      ITEST = 1
      RETURN
C
  723 CONTINUE
      MESS = BLNK40
      ECODE = 'E71'
      WRITE( MESS,3700 ) IRD4
3700  FORMAT(1X,'IOSTAT= ',I8,' FETCHING SF DATA')
      CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
      ITEST = 1
      RETURN
C
  730 CONTINUE
      MESS = BLNK40
      ECODE = 'E71'
      WRITE( MESS,4000 )
4000  FORMAT(1X,'EOF FOUND FETCHING OS DATA')
      CALL ERROR( MPJDY,PATH,ECODE,LOC,MESS )
      ITEST = 1
      RETURN
C
  731 CONTINUE
      ITEST = 1
      RETURN
      END
C
      SUBROUTINE MPTURB(N,NTEST)
      NTEST = 2
      RETURN
      END
