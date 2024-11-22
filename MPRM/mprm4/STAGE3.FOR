      PROGRAM STAGE3
C=====================================================================**
C           METEOROLOGICAL PROCESSOR FOR REGULATORY MODELS (MPRM)
C
C
C           A meteorological preprocessor for regulatory-approved
C                     air quality dispersion models
C
C     PURPOSE:  The main program to control the general flow
C               of the preprocessor and generate the meteorology
C               for the user-specified dispersion model.
C               Basically, three data types are processed:
C
C                  UA - U.S. NWS upper air observations and
C                       twice-daily mixing height values.
C                  SF - U.S. NWS hourly weather observations.
C                  OS - user supplied on-site meteorological data.
C
C               These data have been preprocessed into one data
C               file.  In this program we are interested in
C               developing the proper dispersion meteorology
C               for driving particular dispersion simulation
C               models.
C
C                1.  FIRST WE PROCESS THE USER SUPPIED SETUP DATA.
C                    THIS INVOLVES NOT ONLY PROCESSING THE USER
C                    SUPPLIED DATA RECORDS, BUT ALSO, WE PROCESS
C                    THE HEADER RECORDS WHICH ARE AT THE TOP OF
C                    THE PREPROCESSED METEOROLOGY DATA FILE.
C
C                2.  IF NO ERRORS HAVE OCCURRED AT THIS STAGE, WE
C                    PROCESS THE DATA AND GENERATE THE REQUIRED
C                    METEOROLOGY DATA FILE FOR THE PARTICULAR
C                    DISPERISON MODEL (SELECTED BY THE USER).
C
C                3.  LASTLY WE GENERATE ANY SUMMARY REPORTS NEEDED.
C-----------------------------------------------------------------------
C     Variable declarations
C
      INTEGER ISTAT
C
C     ISTAT is the status for the climatological dispersion models
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MP1.INC'
C
      CHARACTER*12 INPFILE
      WRITE(6, *)'Enter the name of the input file:     '
      WRITE(6, *)
      READ(5, '(A12)') INPFILE
      WRITE(6, *)'The input file is:  ', INPFILE
      OPEN (DEVIN, FILE=INPFILE, STATUS='OLD', ERR=900)

C------- 1.  Process set up data.
C
      CALL MPSTUP
C
C     Test status (all must be ok, or we skip processing)
C
      IF(JBSTAT.LT.0 ) THEN
C        An error has been detected processing the runstream
         GO TO 10
      END IF
C
      IF( STATUS(1,4).EQ.2 ) THEN
C        RUN keyword detected - no data are to be processed
         GO TO 10
      END IF
C
C------- 2.  Develop specified meteorology data set.
C
      CALL MPMET( ISTAT )
C
C     CHECK ISTAT
C
      IF( ISTAT.EQ.1 ) THEN
C        An error occurred while processing the meteorological data
         GO TO 10
      ELSE
C        Finalize summary statistics
         CALL MPSTAT( 1 )
C
      END IF
C
C     Output meteorological data for climatological models
C
      IF( MDSTAT .GE. 9  .AND.  MDSTAT .LE. 13 ) THEN
         CALL MPOUT( ISTAT )
      END IF
C
C------- 3.  Close out run, generating such reports as needed.
C
      WRITE( *, 8 )
    8 FORMAT('   Processing completed; writing summary files'/ )
   10 CALL MPFIN( ISTAT )
C
C------- 4.  Call the message summary routine in the library;
C            the argument represents the stage of processing
      CALL SUMRY2 (3)

      GO TO 910
  900 WRITE(6, '(A12)')' Error opening:  ', INPFILE
  910 CONTINUE

C
      STOP
      END
C
C=====================================================================**
      BLOCK DATA
  
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
  
      END
  

      SUBROUTINE MPOUT( TEST )
C=====================================================================**
C      Purpose:  Writes to met output file as required by the user's
C                defined diffusion model.
C
C      This routine is called only when output is to be written to
C      the met output file.  Hence, this routine is called every
C      day for models requiring hourly output, and only once at the
C      end of a run when climatological output is required.
C
C      Revised: January 24, 1996  (D. Bailey)
C               Cosmetic changes to report file format.
C-----------------------------------------------------------------------
C     Data declarations

      INTEGER TEST,COUNT,HR0124
      CHARACTER*2 LPG(6),LPG1(6),LPG2(6)
      CHARACTER*4 D16(32),D36(72)

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'UA1.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'WORK1.INC'

C     Data initialization

      DATA LPG1/' A',' B',' C','DD','DN','EF'/
      DATA LPG2/' A',' B',' C',' D',' E','FG'/
      DATA COUNT/0/

      DATA D16/' N  ','    ',' NNE','    ',' NE ','    ',' ENE','    ',
     1 ' E  ','    ',' ESE','    ',' SE ','    ',' SSE','    ',
     1 ' S  ','    ',' SSW','    ',' SW ','    ',' WSW','    ',
     1 ' W  ','    ',' WNW','    ',' NW ','    ',' NNW','    '/

      DATA D36/'355-','005 ','005-','015 ','015-','025 ','025-','035 ',
     1 '035-','045 ','045-','055 ','055-','065 ','065-','075 ',
     1 '075-','085 ','085-','095 ','095-','105 ','105-','115 ',
     1 '115-','125 ','125-','135 ','135-','145 ','145-','155 ',
     1 '155-','165 ','165-','175 ','175-','185 ','185-','195 ',
     1 '195-','205 ','205-','215 ','215-','225 ','225-','235 ',
     1 '235-','245 ','245-','255 ','255-','265 ','265-','275 ',
     1 '275-','285 ','285-','295 ','295-','305 ','305-','315 ',
     1 '315-','325 ','325-','335 ','335-','345 ','345-','355 '/

      PATH = 'MP'
      LOC  = ' MPOUT'

C-----------------------------------------------------------------------
C---- For the ISCLT and VALLEY models we have not tallied
C     using a split (day/night) neutral.
C     Hence, define the appropriate labels (for stability)

      DO 10 I=1,6
         IF( (MDSTAT.EQ.11) .OR. (MDSTAT.EQ.9) ) THEN
            LPG(I) = LPG2(I)
         ELSE
            LPG(I) = LPG1(I)
         END IF
   10 CONTINUE

C     Check model and write to met output file
C     hourly/daily meteorology as required by the model.

      IF( MDSTAT.LE.6 .OR. MDSTAT.GE.14 ) THEN
C------- RAMMET-style met output file

         COUNT = COUNT + 1

C------- Check counter.  If this is the first write to the
C        output file, create the header record expected by models
C        employing RAMMET-type processed meteorology.

         IF( COUNT.EQ.1 ) THEN

            READ( UALOC,1000,IOSTAT=IRD4 ) IWORK1(25)
1000        FORMAT( I8 )

C           Check status of read

            IF( IRD4.NE.0 ) THEN
               MESS = BLNK40
               ECODE = 'I71'
               WRITE( MESS,1500 ) IRD4,UALOC
1500           FORMAT(1X,'IOSTAT= ',I8,' ID FOR UALOC: ',A8)
               CALL ERROR( 0,PATH,ECODE,LOC,MESS )
               IWORK1(25) = 0
            END IF

            READ( SFLOC,1000,IOSTAT=IRD4 ) IWORK1(26)

C           Check status of read

            IF( IRD4.NE.0 ) THEN
               MESS = BLNK40
               ECODE = 'I71'
               WRITE( MESS,1600 ) IRD4,SFLOC
1600           FORMAT(1X,'IOSTAT= ',I8,' ID FOR SFLOC: ',A8)
               CALL ERROR( 0,PATH,ECODE,LOC,MESS )
               IWORK1(26) = 0
            END IF

            READ( OSLOC,1000,IOSTAT=IRD4 ) IWORK1(27)

C           Check status of read

            IF( IRD4.NE.0 ) THEN
               MESS = BLNK40
               ECODE = 'I71'
               WRITE( MESS,1700 ) IRD4,OSLOC
1700           FORMAT(1X,'IOSTAT= ',I8,' ID FOR OSLOC: ',A8)
               CALL ERROR( 0,PATH,ECODE,LOC,MESS )
               IWORK1(27) = 0
            END IF

C           Check to make sure a report file exists.
C           If not, use DEVIO for printed output.

            IF( STATUS(1,3).EQ.2 ) THEN
               IRD4 = DEV50
            ELSE
               IRD4 = DEVIO
            END IF

            CALL BANNER( DEV60 )                                        DTB96024
C            WRITE( IRD4,5001 )                                         DTB96024
             WRITE (DEV60, 5001)                                        DTB96024

 5001  FORMAT(/16X,'STAGE-3 PROCESSING OF MERGED METEOROLOGICAL DATA '/)DTB96024

C            WRITE( IRD4,1800 ) IWORK1(26),MPYR,IWORK1(25),MPYR         DTB96024
             WRITE (DEV60, 1800 ) IWORK1(26),MPYR,IWORK1(25),MPYR       DTB96024
 1800        FORMAT(//,1X,'*** HEADER ON OUTPUT MP-DATA FILE:',4I10,/)

            IF( CONTRL(6).EQ.1 ) THEN
               IF( MDSTAT.LE.6 ) THEN
                  WRITE( DEV80 ) IWORK1(26),MPYR,IWORK1(25),MPYR
               ELSE
                  WRITE( DEV80,* ) IWORK1(26),MPYR,IWORK1(25),MPYR
               ENDIF

            ELSE
               WRITE( IRD4,1800 ) IWORK1(26),MPYR,IWORK1(25),MPYR
               IF( MDSTAT.LE.7 ) THEN
                  WRITE( DEV80 ) IWORK1(27),MPYR,IWORK1(25),MPYR
               ELSE
                  WRITE( DEV80,* ) IWORK1(27),MPYR,IWORK1(25),MPYR
               ENDIF
            END IF

         END IF

C        Unformatted output for RAMMET (MDSTAT .LE. 6),

C        ASCII formatted output for ISCST    (MDSTAT .eq. 14)
C                                   ISCSTDRY (MDSTAT .eq. 15)
C                                   ISCSTWET (MDSTAT .eq. 16)
C                                   ISCGASD  (MDSTAT .eq. 17)
C                                   ISCGASW  (MDSTAT .eq. 18)
C
C        The NWS surface data for wind speed and temperature were stored
C        with only one digit after the decimal and is independent of
C        where the data came from (CD-144, SAMSON).  When processing with
C        NWS data (CONTRL(1) and CONTRL(2) both equal to 1), convert the
C        winds back to knots then to m/s and convert temperature to deg F.
C        then back to deg. Celsius to make the output equivalent to
C        PCRAMMET.
C        Note: for P-G stability categories, the winds were converted to
C        knots before determining stability category.
C
         IF( CONTRL(1) .EQ. 1 )THEN
            DO 1825 J=1,24
               KSPEED = NINT( SPEED(1,J)/0.51444 )
               SPEED(1,J) = KSPEED * 0.51444
 1825       CONTINUE
         ENDIF

         IF( CONTRL(2) .EQ. 1 )THEN
            DO 1826 J=1,24
               TFAHR = NINT( (TEMP(1,J)-273.15)/0.5556 ) + 32.0
               TEMP(1,J) = 0.5556 * (TFAHR - 32.0) + 273.15
 1826       CONTINUE
         ENDIF

         XRD1 = FLOAT( MPJDY )
         IF( MDSTAT.LE.6 ) THEN
            WRITE( DEV80 ) MPYR,MPCMO,XRD1,PGSTAB,
     1       (SPEED(1,J),J=1,24), (TEMP(1,J),J=1,24),
     1       (FLWVEC(1,J),J=1,24),(RANFLW(J),J=1,24),
     1       ZIHGTS

         ELSEIF( MDSTAT .EQ. 14 )THEN
            DO 1850 J=1,24
               WRITE( DEV80,1845 ) MPYR,MPCMO,MPCDY,J,RANFLW(J),
     1          SPEED(1,J),TEMP(1,J),PGSTAB(J),
     1          ZIHGTS(1,J),ZIHGTS(2,J)                                 DTB92365
 1845          FORMAT(4I2,2F9.4,F6.1,I2,2F7.1)
 1850       CONTINUE

         ELSEIF( MDSTAT .EQ. 15 )THEN
            DO 1860 J=1,24
               WRITE( DEV80,1855 ) MPYR,MPCMO,MPCDY,J,RANFLW(J),
     &         SPEED(1,J),TEMP(1,J),PGSTAB(J),ZIHGTS(1,J),ZIHGTS(2,J),
     &         USTAR(J), MOL(J), Z0APPL(J)
 1855          FORMAT(4I2,2F9.4,F6.1,I2,2F7.1, F9.4,F10.1,F8.4 )
 1860       CONTINUE

         ELSEIF( MDSTAT .EQ. 16 )THEN
            DO 1870 J=1,24
               WRITE( DEV80,1865 ) MPYR,MPCMO,MPCDY,J,RANFLW(J),
     &         SPEED(1,J),TEMP(1,J),PGSTAB(J),ZIHGTS(1,J),ZIHGTS(2,J),
     &         USTAR(J), MOL(J), Z0APPL(J), IPPTYP(J), PRECIP(J)
 1865          FORMAT(4I2,2F9.4,F6.1,I2,2F7.1, F9.4,F10.1,F8.4, I4,F7.2)
 1870       CONTINUE

         ELSEIF( MDSTAT .EQ. 17 )THEN
            DO 1880 J=1,24
               WRITE( DEV80,1875 ) MPYR,MPCMO,MPCDY,J,RANFLW(J),
     &         SPEED(1,J),TEMP(1,J),PGSTAB(J),ZIHGTS(1,J),ZIHGTS(2,J),
     &         USTAR(J), MOL(J), Z0APPL(J), QR(J), XLAI(J)
 1875          FORMAT(4I2,2F9.4,F6.1,I2,2F7.1, F9.4,F10.1,F8.4, F8.1,
     &                F8.3)
 1880       CONTINUE

         ELSEIF( MDSTAT .EQ. 18 )THEN
            DO 1890 J=1,24
               WRITE( DEV80,1885 ) MPYR,MPCMO,MPCDY,J,RANFLW(J),
     &         SPEED(1,J),TEMP(1,J),PGSTAB(J),ZIHGTS(1,J),ZIHGTS(2,J),
     &         USTAR(J), MOL(J), Z0APPL(J), QR(J), XLAI(J),
     &         IPPTYP(J), PRECIP(J)
 1885          FORMAT(4I2,2F9.4,F6.1,I2,2F7.1, F9.4,F10.1,F8.4,
     &                F8.1, F8.3, I4,F7.2)
 1890       CONTINUE
         ENDIF
C
         IF( PCNTRL.GT.0 ) THEN
C
C        Check to see if a report file exits.
C        If not, use DEVIO for printed output.
C
            IF( STATUS(1,3).EQ.2 ) THEN
               IRD4 = DEV50
            ELSE
               IRD4 = DEVIO
            END IF
C
C           New page?
C
            IF( COUNT.GT.1 ) THEN
               IWORK1(100) = MOD( COUNT,3 )
               IF( IWORK1(100).EQ.1 ) THEN
                  CALL BANNER( IRD4 )
                  WRITE( IRD4,5001 )
               END IF
            END IF
C
            WRITE ( IRD4,1900) MPYR,MPCMO,MPCDY,MPJDY,TSR,TSS
C
            WRITE ( IRD4,2000 ) (PGSTAB(J),J=1,12)
C
            WRITE ( IRD4,2100 ) (SPEED(1,J),J=1,12),
     &                          (TEMP(1,J),J=1,12),
     &                          (FLWVEC(1,J),J=1,12),
     &                          (RANFLW(J),J=1,12),
     &                          ((ZIHGTS(I,J),J=1,12),I=1,2)

            WRITE ( IRD4,2000 ) (PGSTAB(J),J=13,24)
C
            WRITE ( IRD4,2100 ) (SPEED(1,J),J=13,24),
     &                          (TEMP(1,J),J=13,24),
     &                          (FLWVEC(1,J),J=13,24),
     &                          (RANFLW(J),J=13,24),
     &                          ((ZIHGTS(I,J),J=13,24),I=1,2)
C
         END IF
C
      END IF

1900  FORMAT (/,' YEAR= ' ,I2,' MONTH= ',I2,' DAY= ',I2,
     &          ' JULIAN DAY= ',I3,' SUNRISE= ',F7.3,
     &          ' SUNSET= ',F7.3)
2000  FORMAT (' PGSTAB=',3X,I1,1X,11(4X,I1,1X))
2100  FORMAT (' SPEED= ',F4.1,1X,11(1X,F4.1,1X)/
     1        ' TEMP=  ',12(1X,F4.0,1X)/
     1        ' FLWVEC=',12(1X,F4.0,1X)/' RANFLW=',12(1X,F4.0,1X)/
     1        ' RURAL= ',12(F5.0,1X)/
     1        ' URBAN= ',12(F5.0,1X),/)


C------------------------
C***  CALINE-3 FORMAT ***
C
      IF( MDSTAT.EQ.7 ) THEN
C
C     CHECK TO MAKE SURE THE THE REPORT FILE IS AVAILABLE.
C     IF NOT, USE DEVIO.
C
         IF( STATUS(1,3).EQ.2 ) THEN
            IRD4 = DEV50
         ELSE
            IRD4 = DEVIO
         END IF
C
         DO 20 HR0124=1,24
C
C **  INCREMENT COUNTER
            COUNT = COUNT + 1
C
C     CONVERT VALUES FOR OUPUT, STORING CONVERTED
C     VALUES IN WORK1-ARRAY.
C
C **  CONVERT RANDOMIZED FLOW VECTOR TO WIND DIRECTION
            IF( RANFLW(HR0124) .GT. 0 ) THEN
               WORK1(101) = RANFLW(HR0124) + 180.0
               IF( WORK1(101) .GT. 360.0 ) THEN
                  WORK1(101) = WORK1(101) - 360.0
               ENDIF
            ELSE
               WORK1(101) = -99.
            END IF
C
C **  WIND SPEED
            IF( SPEED(1,HR0124) .GT. 0. ) THEN
               WORK1(100) = SPEED(1,HR0124)
               IF( WORK1(100).LT.1.0 ) WORK1(100) = 1.0
            ELSE
               WORK1(100) = -9.
            END IF
C
C **  CHECK FOR MISSING RURAL MIXING HEIGHT
            IF( ZIHGTS(1,HR0124) .GE. 0. ) THEN
               WORK1(102) = ZIHGTS(1,HR0124)
            ELSE
               WORK1(102) = 1000.0
            END IF
C
C **  SET BACKGROUND CONCENTRATION TO ZERO
            WORK1(103) = 0.0
C
C **  OUTPUT DATA FOR HOUR TO DATA FILE FOR USE BY CALINE-3
            WRITE( DEV80,2200 ) WORK1(100),WORK1(101),
     1       PGSTAB(HR0124),WORK1(102),WORK1(103)
2200        FORMAT(F3.0,F4.0,I1,F6.0,F4.0)
C
C **  CHECK PRINT STATUS AND SEE IF USER WANTS THESE DATA LISTED
C     TO THE PRINTED REPORT FILE
C
            IF( PCNTRL.GT.0 ) THEN
C
C **  DO WE NEED A BANNER AT TOP OF PAGE
               IWORK1(100) = MOD( COUNT,48 )
               IF( IWORK1(100).EQ.1 ) THEN

                  CALL BANNER( IRD4 )
                  WRITE( IRD4,5001 )

               END IF
C
C **  PUT EXTRA LINE FEED AHEAD OF HOUR 1 AND BEHIND HOUR 24
C     TO SEPARATE EACH DAY'S OUTPUT
               IF( HR0124.EQ.1 ) THEN
                  WRITE( IRD4,2305 ) MPYR,MPJDY,HR0124,WORK1(100),
     &                               WORK1(101),PGSTAB(HR0124),
     &                               WORK1(102),WORK1(103)

               ELSE IF( HR0124.EQ.24 ) THEN
                  WRITE( IRD4,2310 ) MPYR,MPJDY,HR0124,WORK1(100),
     &                               WORK1(101),PGSTAB(HR0124),
     &                               WORK1(102),WORK1(103)

               ELSE
                  WRITE( IRD4,2315 ) MPYR,MPJDY,HR0124,WORK1(100),
     &                               WORK1(101),PGSTAB(HR0124),
     &                               WORK1(102),WORK1(103)
               END IF
2305           FORMAT(/,5X,I2,I3,I2,1X,2F6.1,I3,2F7.1)
2310           FORMAT(5X,I2,I3,I2,1X,2F6.1,I3,2F7.1,/)
2315           FORMAT(5X,I2,I3,I2,1X,2F6.1,I3,2F7.1)
C
            END IF
C
20       CONTINUE
C
      END IF


C--------------------
C *** RTDM FORMAT ***
C
      IF( MDSTAT.EQ.8 ) THEN
C
C     CHECK TO MAKE SURE THE THE REPORT FILE IS AVAILABLE.
C     IF NOT, USE DEVIO.
C
         IF( STATUS(1,3).EQ.2 ) THEN
            IRD4 = DEV50
         ELSE
            IRD4 = DEVIO
         END IF
C
         DO 50 HR0124=1,24
C
C **  INCREMENT COUNTER
            COUNT = COUNT + 1
  
C     CONVERT VALUES FOR OUPUT, STORING CONVERTED
C     VALUES IN WORK1-ARRAY.
C
C **  CONVERT RANDOMIZED FLOW VECTOR TO WIND DIRECTION
            IF( RANFLW(HR0124) .GT. 0 ) THEN
               WORK1(100) = RANFLW(HR0124) + 180.0
               IF( WORK1(100) .GT. 360.0 ) THEN
                  WORK1(100) = WORK1(100) - 360.0
               ENDIF
            ELSE
               WORK1(100) = -999.
            END IF
C
C **  CONVERT WIND SPEED TO MILES PER HOUR
            IF( SPEED(1,HR0124) .GT. 0. ) THEN
               WORK1(101) = SPEED(1,HR0124)/0.44707
            ELSE
               WORK1(101) = -999.
            END IF
C
C **  CHECK FOR MISSING RURAL MIXING HEIGHT
            IF( ZIHGTS(1,HR0124) .GE. 0. ) THEN
               WORK1(102) = ZIHGTS(1,HR0124)
            ELSE
               WORK1(102) = -999.
            END IF
C
C **  CONVERT STABILITY CATEGORY TO INTEGER VALUE
            IF( PGSTAB(HR0124) .GT. 0 ) THEN
               WORK1(103) = PGSTAB(HR0124)
            ELSE
               WORK1(103) = -999.
            END IF
C
C ** CONVERT TEMPERATURE TO DEG F
            IF( TEMP(1,HR0124) .GT. -99. ) THEN
               WORK1(104) = 1.8*TEMP(1,HR0124) - 459.67
            ELSE
               WORK1(104) = -999.
            END IF
C
C **  OUTPUT DATA FOR HOUR TO DATA FILE FOR USE BY RTDM
            WRITE( DEV80,2500 ) MPYR,MPJDY,HR0124,(WORK1(J),J=100,104)
2500        FORMAT(I2,I3,I2,1X,F6.0,F6.1,2F6.0,F6.1)
C
C **  CHECK PRINT STATUS AND SEE IF USER WANTS THESE DATA LISTED
C     TO THE PRINTED REPORT FILE
C
            IF( PCNTRL.GT.0 ) THEN
C
C **  DO WE NEED A BANNER AT TOP OF PAGE
               IWORK1(100) = MOD( COUNT,48 )
               IF( IWORK1(100).EQ.1 ) THEN
                  CALL BANNER( IRD4 )
                  WRITE( IRD4,5001 )
               END IF
C
C **           Put extra line feed ahead of hour 1 and behind hour 24
C              to separate each day's output
               IF( HR0124.EQ.1 ) THEN
                  WRITE( IRD4,2505 ) MPYR,MPJDY,HR0124,
     &                               (WORK1(J),J=100,104)

               ELSE IF( HR0124.EQ.24 ) THEN
                  WRITE( IRD4,2510 ) MPYR,MPJDY,HR0124,
     &                               (WORK1(J),J=100,104)

               ELSE
                  WRITE( IRD4,2515 ) MPYR,MPJDY,HR0124,
     &                               (WORK1(J),J=100,104)
               END IF
2505           FORMAT(/,5X,I2,I3,I2,1X,F6.0,F6.1,2F6.0,F6.1)
2510           FORMAT(5X,I2,I3,I2,1X,F6.0,F6.1,2F6.0,F6.1,/)
2515           FORMAT(5X,I2,I3,I2,1X,F6.0,F6.1,2F6.0,F6.1)
C
            END IF
C
50       CONTINUE
C
      END IF
C
      IF( MDSTAT.GE.9 .AND. MDSTAT.LE.13 ) THEN
C
C      CHECK MODEL AND WRITE TO MET OUTPUT FILE
C      CLIMATOLOGICAL/SEASONAL FREQUENCY MET DATA
C      AS DESIRED BY MODEL.
C
C
         IF( MDSTAT.EQ.13 ) THEN
            IRD5 = 36
         ELSE
            IRD5 = 16
         END IF
C
C        Write output to DEV80 for use as input to CDM-type models
C
         DO 70 I=1,6
C           Loop over stability category

            DO 60 K=1,IRD5
C              Loop over  wind direction sector
               WRITE( DEV80,3000 ) (FREQ(I,J,K),J=1,6)
3000           FORMAT( 9X,6F9.6 )
60          CONTINUE
70       CONTINUE
C
C        Do we want a printed report?
C
         IF( PCNTRL.GT.0 ) THEN
C
C           Check to make sure the the report file is available.
C           If not, use DEVIO.
C
            IF( STATUS(1,3).EQ.2 ) THEN
               IRD4 = DEV50
            ELSE
               IRD4 = DEVIO
            END IF
C
            DO 90 I=1,6
C              Loop over stability;
C              see if a banner header (new page) is needed
C
               IF( IRD5.EQ.16 .AND. (I/2)*2.NE.I ) THEN
                  CALL BANNER( IRD4 )
                  WRITE( IRD4,5001 )
                  WRITE( IRD4,4000 )

               ELSE IF ( IRD5.EQ.36 ) THEN
                  CALL BANNER( IRD4 )
                  WRITE( IRD4,5001 )
                  WRITE( IRD4,4000 )
               END IF
C
               WRITE( IRD4,4100 ) LPG(I)
               DO 80 K=1,IRD5
C                 Loop on wind direction sector
                  IF( IRD5.EQ.36 ) THEN
                     WRITE( IRD4,4200 ) D36(2*K-1),D36(2*K),K,
     &                                 (FREQ(I,J,K),J=1,6)
                  ELSE
                     WRITE( IRD4,4200 ) D16(2*K-1),D16(2*K),K,
     &                                 (FREQ(I,J,K),J=1,6)
                  END IF
C
80             CONTINUE
90          CONTINUE
C
4000        FORMAT(//,54X,'METEOROLOGICAL JOINT FREQUENCY FUNCTION')
4100        FORMAT(//,1X,'STABILITY CATEGORY ',A2,43X,
     &                   'WIND SPEED CLASS',//,
     &                5X,'WIND DIRECTION    SECTOR',
     &               11X,'1',12X,'2',12X,'3',12X,'4',12X,'5',12X,'6',/)
4200        FORMAT( 8X,2A4,9X,I2,9X,6(F8.6,5X))

         END IF
C
      END IF
C
      RETURN
      END

  

      SUBROUTINE MPFIN( ISTAT )

C     Revised:  January 24, 1996 (D. Bailey)
C               Cosmetic changes to report file format

C     LOCAL VARIABLES

      INTEGER ISTAT
      CHARACTER*2 LPG(6),LPG1(6),LPG2(6)

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'UA1.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'WORK1.INC'

      DATA LPG1/' A',' B',' C','DD','DN','EF'/
      DATA LPG2/' A',' B',' C',' D',' E','FG'/
      DATA PATH/'MP'/, LOC/' MPFIN'/

C *** GET THE DATE FROM THE SYSTEM

C      FOR THE ISCLT AND VALLEY MODELS WE HAVE NOT TALLIED
C      USING A SPLIT (DAY/NIGHT) NEUTRAL.
C      HENCE, DEFINE THE LABELS (FOR STATBILITY), TO
C      BE APPROPRIATE.

      DO 5 I=1,6
         IF( (MDSTAT.EQ.11) .OR. (MDSTAT.EQ.9) ) THEN
            LPG(I) = LPG2(I)
         ELSE
            LPG(I) = LPG1(I)
         END IF
    5 CONTINUE


C        CHECK TO MAKE SURE THAT THE REPORT FILE IS AVAILABLE.
C        IF NOT, USE DEVIO.

      IF( STATUS(1,3).EQ.2 ) THEN
         IRD5 = DEV50
      ELSE
         IRD5 = DEVIO
      END IF

      CALL BANNER (IRD5)
      WRITE (IRD5, 5001)
 5001 FORMAT(/16X,'STAGE-3 PROCESSING OF MERGED METEOROLOGICAL DATA '/) DTB96024

C        1.  CHECK JBSTAT AND 'JB RUN'-CARD STATUS

      IF( STATUS(1,4).EQ.2 ) THEN
C **    USER RAN A CHECKOUT RUN ON THE SETUP DATA
         WRITE( IRD5,1000 )
1000     FORMAT(//,21X,'CHECKOUT RUN OPTION SELECTED BY USER',/,
     1    21X,'   SETUP DATA HAVE BEEN PROCESSED',/,
     1    21X, '  AND THEN JOB WAS TERMINATED.',/)

C      ELSE                                                             DTB96024
C         WRITE( IRD5,1010 )                                            DTB96024
C 1010     FORMAT(//,21X,' ATTEMPT PROCESSING OF METEOROLOGICAL',/ ,    DTB96024
C     1    21X,'  DATA FOR USE BY A DISPERSION MODEL.',/)               DTB96024

      END IF

C **    CHECK JBSTAT

      IF( JBSTAT.LT.0 ) THEN
         WRITE( IRD5,1020 )
1020     FORMAT( 23X,' ********************************',/,
     1    23X,' *** ABNORMAL JOB TERMINATION ***',/,
     1    23X,' ********************************',/)
      ELSE
         WRITE( IRD5,1030 )
1030     FORMAT( 23X,' ********************************',/,
     1    23X,' *** JOB TERMINATED NORMALLY  ***',/,
     1    23X,' ********************************',/)
      END IF

C       2. LIST WHATEVER IS MEANINGFUL OF THE SETUP DATA
C          FOR DEFINING THIS RUN.

      BUF80(1)(1:21) = 'OPENED UNSUCCESSFULLY'
      BUF80(2)(1:21) = 'OPENED   SUCCESSFULLY'
      BUF80(3)(1:21) = 'STANDARD OUTPUT UNIT '

      IWORK1(100) = STATUS(1,3)+STATUS(1,5)+STATUS(6,3)
     1 +STATUS(6,4)

      IF( IWORK1(100).EQ.0 ) THEN
         WRITE( IRD5,1040)
1040     FORMAT( /,5X,'1. NO INPUT OR OUTPUT FILES WERE DEFINED',/,
     1    5X,'   DURING SETUP PROCESSING.  THIS HAS RESULTED',/,
     1    5X,'   IN AN ABNORMAL JOB TERMINATION.',//)
      ELSE
         WRITE( IRD5,1050 )
1050     FORMAT( //,5X,'1. FILENAMES AS DETERMINED DURING SETUP',/)     DTB96024

         BUF48 = BLNK48
         BUF48(1:19) = 'GENERAL REPORT FILE'
         IF( STATUS(1,3).GE.2 ) THEN
            WRITE( IRD5,1060 ) DISK50,BUF80(2)(1:21)
         ELSE IF( STATUS(1,3) .EQ. 0) THEN
            WRITE( IRD5,1060 ) BUF48,BUF80(3)(1:21)
         ELSE
            WRITE( IRD5,1060 ) BUF48,BUF80(1)(1:21)
1060        FORMAT( 8X,A48,1X,A21)
         END IF

         BUF48 = BLNK48
         BUF48(1:32) = 'ERROR LISTINGS AND WARNINGS FILE'
         IF( STATUS(1,5).GE.2 ) THEN
            WRITE( IRD5,1060 ) DISK60,BUF80(2)(1:21)
         ELSE
            WRITE( IRD5,1060 ) BUF48,BUF80(1)(1:21)
         END IF

         BUF48 = BLNK48
         BUF48(1:40) = 'INPUT FILE OF MERGED METEOROLOGICAL DATA'
         IF( STATUS(6,3).GE.2 ) THEN
            WRITE( IRD5,1060 ) DISK40,BUF80(2)(1:21)
         ELSE
            WRITE( IRD5,1060 ) BUF48,BUF80(1)(1:21)
         END IF

         BUF48 = BLNK48
         BUF48(1:43) = 'OUTPUT FILE OF DISPERSION MODEL METEOROLOGY'
         IF( STATUS(6,4).GE.2 ) THEN
            WRITE( IRD5,1060 ) DISK80,BUF80(2)(1:21)
         ELSE
            WRITE( IRD5,1060 ) BUF48,BUF80(1)(1:21)
         END IF

      END IF

C **  WHAT DISPERSION MODEL CALLED FOR

      IF( MDSTAT.EQ.0 ) THEN
         WRITE( IRD5,1070 )
1070     FORMAT( /,5X,'2. THE DISPERSION MODEL WAS NOT SUCCESSFULLY',/,
     1    5X,'   DETERMINED DURING SETUP PROCESSING.  THIS',/,
     1    5X,'   HAS RESULTED IN AN ABNORMAL JOB END.')

      ELSE
         WRITE( IRD5,1080 ) DISPMD( MDSTAT )
1080     FORMAT(//,5X,'2. DISPERSION MODEL DEFINED DURING SETUP:   ',   DTB96024
     1    A8/)                                                          DTB96024
      END IF

C WRITE A MESSAGE IF LONGZ OR SHORTZ HAS BEEN SELECTED

      IF( (MDSTAT.EQ.6) .OR. (MDSTAT.EQ.10) ) THEN
         MPLAT = 0.0
         MPLON = 0.0
         WRITE( IRD5,1085 ) IVDATE
      ENDIF
1085  FORMAT(/,5X,' THIS MODEL IS NOT SUPPORTED IN MPRM DATED ',I5)

      WRITE( IRD5,1090 )
1090  FORMAT( /,5X,'3. PROCESSING OPTIONS SELECTED DURING SETUP',/,     DTB96024
     1 5X,'   PROCESS        SCHEME',/)

      BUF80(1)(1:15) = 'WIND           '
      BUF80(2)(1:15) = 'TEMPERATURE    '
      BUF80(3)(1:15) = 'MIXING HEIGHTS '
      BUF80(4)(1:15) = 'STABILITY      '
      BUF80(5)(1:15) = 'TEMP. GRADIENT '
      BUF80(6)(1:15) = 'TURBULENCE     '
      BUF80(7)(1:15) = 'HEAT FLUX      '
      BUF80(8)(1:15) = 'FRICTION VELO. '
      BUF80(9)(1:15) = 'OUTPUT HEIGHTS '

      DO 10 I=1,4
         WRITE( IRD5,2000 ) BUF80(I)(1:15),ACTION(CONTRL(I))
2000     FORMAT( 8X,A15,A6 )
10    CONTINUE


         WRITE( IRD5,3850 ) (PGSCHM(J),J=1,7)                           DTB96024
3850     FORMAT(//,5X,'4. STABILITY METHODS USED',//,                   DTB96024
     1    10X, ' NWSWXX', T20, I4/ 10X, ' ONSITE', T20, I4/             DTB96024
     1    10X, ' SESITE', T20, I4/ 10X, ' SASITE', T20, I4/             DTB96024
     1    10X, ' WNDWXX', T20, I4/ 10X, ' TTDIFF', T20, I4/             DTB96024
     1    10X, ' USERIN', T20, I4//)                                    DTB96024


      WRITE( IRD5,2010 )
2010  FORMAT (5X,'5. PROCESSING ASSUMPTIONS ',/)                        DTB96024

      IF( ANEHGT.GT.0 ) THEN
         BUF80(1)(1:40) = 'WIND SPEED/TURB. MEASUREMENT HEIGHT (M):'
         WRITE( IRD5,2020 ) BUF80(1)(1:40),ANEHGT
2020     FORMAT(8X,A40,2X,F6.2)
      END IF

      IF( STKHGT.GT.0 ) THEN
         BUF80(1)(1:40) = 'STACK HEIGHT (M)                        '
         WRITE( IRD5,2020 ) BUF80(1)(1:40),STKHGT
      END IF

      IF( TMPHGT.GT.0 ) THEN
         BUF80(1)(1:40) = 'TEMPERATURE HEIGHT (M)                  '
         WRITE( IRD5,2020 ) BUF80(1)(1:40),TMPHGT
      END IF

      WRITE( IRD5,2030 )
2030  FORMAT( //,5X,'6. LOCATIONS SPECIFIED IN SETUP ',//,              DTB96024
     1 5X,'    DATA      SITE     LONGITUDE   LATITUDE',/,
     1 5X,'   PATHWAY     ID      (DEGREES)   (DEGREES)',/)

      IF( UALOC.NE.BLNK08 ) THEN
         WRITE( IRD5,2040 ) PATHWD(2),UALOC,UALON,UALAT
2040     FORMAT(10X,A2,5X,A8,3X,A8,4X,A8)
      END IF

      IF( SFLOC.NE.BLNK08 ) THEN
         WRITE( IRD5,2040 ) PATHWD(3),SFLOC,SFLON,SFLAT
      END IF

      IF( OSLOC.NE.BLNK08 ) THEN
         WRITE( IRD5,2040 ) PATHWD(4),OSLOC,OSLON,OSLAT
      END IF

      WRITE( IRD5,2050 ) MPLON, MPLAT
2050  FORMAT(/,8X,'*****************************************',/,
     1 8X,'* LONGITUDE AND LATITUDE FOR PROCESSING *',/,
     1 8X,'*       ',F8.2,'   ',F8.2,'             *',/,
     1 8X,'*****************************************',/)

      IF( STATUS(1,5).EQ.2 .OR. STATUS(6,4).EQ.2 ) THEN
         WRITE( IRD5,2060 )
2060     FORMAT( /,5X,'7. OUTPUT FILE NAMES.')                          DTB96024

         IF( STATUS(1,5).EQ.2 ) THEN
            WRITE( IRD5,2070 ) DISK60
2070        FORMAT(/,8X,'ERROR REPORT FILE: ', T35, A48)                DTB96024
         END IF

         IF( STATUS(6,4).EQ.2 ) THEN
            WRITE( IRD5,2080 ) DISK80
            WRITE( IRD5,2081 ) UALOC, MPYR, SFLOC, MPYR                 DTB96024
2080        FORMAT (8X,'MET DATA FOR MODELING: ', T35, A48)             DTB96024
2081        FORMAT (8X, 'HEADER ON OUTPUT FILE:  ', 2(2X, A8, I10))     DTB96024

         END IF
      END IF

C     LIST SUMMARY STATISTICS IF PROCESSING COMPLETED NORMALLY

      IF( JBSTAT.EQ.1 ) THEN

         WRITE( IRD5,3700 )
3700     FORMAT(//,5X,'8. SUMMARY OF DATA PROCESSING RESULTS ',//,      DTB96024
     1    5X,'   VARIABLE ', T28, ' # VALID  # MISSING ',/)             DTB96024

         WRITE( IRD5,3710 ) (PGNUM(J),J=1,2)
3710     FORMAT (7X,' STABILITY ', T30, I5, 4X, I5)                     DTB96024

         IF( MDSTAT.LE.8 .OR. MDSTAT.GE.14 ) THEN
            WSNUM(1) = WSNUM(1) - WSNUM(3)
         END IF

         WRITE( IRD5,3720 ) (WSNUM(J),J=1,3)
3720     FORMAT (7X,' WIND SPEED ', T30, I5, 4X, I5, 8X, I5, ' (Calms)')DTB96024
         WRITE( IRD5,3730 ) (WDNUM(J),J=1,2)
3730     FORMAT (7X,' WIND DIRECTION ', T30, I5, 4X, I5)                DTB96024

         IRD3 = 0
         IRD4 = 0
         DO 30 I=1,7
            IRD3 = IRD3 + ZINUM(1,1,I)
            IRD4 = IRD4 + ZINUM(2,1,I)
30       CONTINUE
         WRITE( IRD5,3740 ) IRD3,IRD4
3740     FORMAT (7X,' RURAL MIXING HEIGHT ', T30, I5, 4X, I5)           DTB96024

         IRD3 = 0
         IRD4 = 0
         DO 40 I=1,7
            IRD3 = IRD3 + ZINUM(1,2,I)
            IRD4 = IRD4 + ZINUM(2,2,I)
40       CONTINUE
         WRITE( IRD5,3750 ) IRD3,IRD4
3750     FORMAT (7X,' URBAN MIXING HEIGHT', T30, I5, 4X, I5)            DTB96024

         TTAVG = TTAVG - 273.15
         WRITE( IRD5,3800 ) (TTNUM(J),J=1,2),TTAVG
3800     FORMAT (7X, ' TEMPERATURE ', T30, I5, 4X, I5, 8X, F5.2,        DTB96024
     1   ' (Average)')                                                  DTB96024

         WRITE( IRD5,3900 ) ((WSAVG(J,I),I=1,6),J=1,2)
3900     FORMAT(//,5X,'9. DISTRIBUTION OF WIND SPEEDS',//,              DTB96024
     1    6X,'WS CLASS    1       2       3       4       5       6',/, DTB96024
     1    6X,'# HOURS', 6F8.0,/,6X,'AVERAGE', 6F8.2)                    DTB96024

C *****  Report Stability Classification Results for Rural Modeling

         IF( MDSTAT.EQ.1 .OR. MDSTAT.EQ.12 .OR. MDSTAT.EQ.13 ) THEN
            CONTINUE
         ELSE
            WRITE( IRD5,4000 )  (LPG(I),I=1,6)
4000        FORMAT(//,5X, '10. RURAL STABILITY CATEGORY RESULTS  ',     DTB96024
     1      '(# HOURS)' //6X,6(6X,A2))                                  DTB96024

            WRITE( IRD5,4070 ) (ZINUM(1,1,I),I=1,6)                     DTB96024
4070        FORMAT( 8X, 6(1X,I5,2X))                                    DTB96024

         END IF


C *****  Report Stability Classification Results for Urban Modeling

C         IF( MDSTAT.EQ.1 .OR. MDSTAT.EQ.5 .OR.
C     1       MDSTAT.EQ.8 .OR. MDSTAT.EQ.9 ) THEN
C            CONTINUE
C         ELSE
C            WRITE( IRD5,4080 )  (LPG(I),I=1,6)
C4080        FORMAT(//,5X, '10. URBAN STABILITY CATAGEORY RESULTS ',    DTB96024
C     1      '(# HOURS)' //6X,6(6X,A2) // )                             DTB96024
C
C            WRITE( IRD5,4070 )  (ZINUM(1,2,I),I=1,6)                   DTB96024
C         END IF


         IF( MDSTAT .ge. 15 )THEN
            write(ird5,1204)
            do 1201  isect = 1,osnwds
              if( isect .gt. 1 ) write( ird5,'(1x)' )
              do 1202 imnth = 1,12
                write(ird5,1203) imnth,isect,
     &                        (ossfc(imnth,isect,issp),issp=1,nsitch)
 1202         continue
 1201       continue
         ENDIF
 1203    format(2i6,8f9.4)
 1204    format(//5X, '11. SURFACE CHARACTERISTICS USED',//
     &    1x,'Month ', 'Sector','  Albedo ','  Bowen  ',' z0(meas)',
     &                          ' z0(appl)','  Min. L ','   Cg    ',
     &                          'Anth Heat','   LAI   '/
     &    1x,83('-') )

      END IF
C
      RETURN
      END
C

      SUBROUTINE MPSTAT( ISTAT )
C=====================================================================**
C   PURPOSE:  To store sufficient information that 'CDM' output data
C             can be generated, if so directed by the processing controls.
C
C             If istat le. zero, process the variables and accumulate
C             results in summary arrays.
C
C             If istat gt. zero, compute averages and finalize results
C             for output.
C-----------------------------------------------------------------------
C     Data declarations
C
      INTEGER HR0124, TOTAL
      REAL    ZIRANG(5), SUMLOW
C
C     HR0124   HOUR OF DAY, 1-24 HOUR CLOCK ASSUMED.
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'WORK1.INC'
C
C     Data initialization
C
      DATA ZIRANG/250.,500.,1000.,1500.,2000./
      DATA TOTAL/0/
C
C       CHECK ISTAT, IF GT. ZERO, SKIP TOP HALF OF
C       SUBROUTINE AND GO TO SECOND HALF FOR
C       FINIALIZING SUMMARY ARRAYS.
C
      IF( ISTAT.LE.0 ) THEN
C
C       LOOP ON HOUR OF DAY
C
         DO 100 HR0124=1,24
C
C       INITIALIZE VALUES
C
            IRD5 = 0
            IRD4 = 0
            IRD3 = 0
C
C       PROCESS EACH VARIABLE
C
C       1. PASQUILL STABILITY CATEGORIES (USE ACTUAL STABILITIES FROM
C          STABILITY CLASS ROUTINES, STORED IN PGSTAB0)
C
            IF( PGSTAB0(HR0124).LE.0 ) THEN
C
C     STABILITY CLASS IS MISSING, INCREMENT APPROPRIATE COUNTERS
C
               PGNUM(2) = PGNUM(2) + 1
C
C     CHECK MIXING HEIGHTS
C
               DO 10 J=1,2
                  IF( ZIHGTS(J,HR0124).LE.-999.0 ) THEN
                     ZINUM(2,J,7) = ZINUM(2,J,7) + 1
                  ELSE
                     ZINUM(1,J,7) = ZINUM(1,J,7) + 1
                  END IF
10             CONTINUE
C
            ELSE
C
C        FOR ALL MODELS ** EXCEPT ** ISCLT  (MDSTAT 11)
C                             AND    VALLEY (MDSTAT 9)
C        ADJUST STABILITY CLASSES TO DENOTE DAY/NIGHT
C        1=A, 2=B, 3=C, 4=DD, 5=DN, 6=E-G
C
C        FOR ISCLT AND VALLEY WE HAVE
C        1=A, 2=B, 3=C, 4=D, 5=E, 6=F-G
C
               IRD5 = PGSTAB0(HR0124)
C
               IF( (MDSTAT.NE.11) .AND. (MDSTAT.NE.9) ) THEN
C
                  IF( HR0124.LE.TSR .OR. HR0124.GE.TSS ) THEN
                     IRD5 = PGSTAB0(HR0124) + 1
                  END IF
C
               END IF
               IF( IRD5.GT.6 ) IRD5 = 6
C
               PGNUM(1) = PGNUM(1) + 1
C
C
C     CHECK MIXING HEIGHTS
C
               DO 50 J=1,2
                  IF( ZIHGTS(J,HR0124).LE.-999.0 ) THEN
                     ZINUM(2,J,IRD5) = ZINUM(2,J,IRD5) + 1
                  ELSE
                     ZINUM(1,J,IRD5) = ZINUM(1,J,IRD5) + 1
C
C         DETERMINE HEIGHT RANGE
C
                     DO 20 I=1,5
                        IF( ZIHGTS(J,HR0124).LE.ZIRANG(I) ) GO TO 30
20                   CONTINUE
                     I = 6
30                   CONTINUE
                     ZIFREQ(J,IRD5,I) = ZIFREQ(J,IRD5,I) + 1
                     ZIFREQ(J,IRD5,7) = ZIFREQ(J,IRD5,7) +
     &                                  ZIHGTS(J,HR0124)
                  END IF
50             CONTINUE
C
            END IF
C
C       CHECK TEMPERATURES
C
            IF( TEMP(1,HR0124).LE.-99.0 ) THEN
               TTNUM(2) = TTNUM(2) + 1
            ELSE
               TTNUM(1) = TTNUM(1) + 1
               TTAVG    = TTAVG + TEMP(1,HR0124)
            END IF
C
C       CHECK WIND SPEEDS
C
            IF( SPEED(1,HR0124).LE.-9.0 ) THEN
               WSNUM(2) = WSNUM(2) + 1
            ELSE IF( SPEED(1,HR0124).LT.0.0 ) THEN
               CONTINUE
            ELSE IF( SPEED(1,HR0124).EQ.0.0 ) THEN
               WSNUM(1) = WSNUM(1) + 1
            ELSE
C
C       DETERMINE WIND SPEED CLASS
C       (IWORK1(1) IS CURRENT WIND SPEED TO NEAREST KNOT)
C
               IWORK1(1) = 1.9425*SPEED(1,HR0124) + 0.5
               DO 60 IRD4=1,5
                  IF( IWORK1(1).LE.WSCLSS(IRD4) ) GO TO 70
60             CONTINUE
               IRD4 = 6
70             CONTINUE
C
            END IF
C
            IF( IRD4.GT.0 ) THEN
               WSNUM(1) = WSNUM(1) + 1
               WSAVG(1,IRD4) = WSAVG(1,IRD4) + 1
               WSAVG(2,IRD4) = WSAVG(2,IRD4) + 1.0/SPEED(1,HR0124)
            END IF
C
C       CHECK WIND DIRECTIONS - USE FLWVEC TO BE CONSISTENT WITH STAR
C       (WORK1(1) SET TO WIND DIRECTION IN DEGREES)
C
            IF( FLWVEC(1,HR0124).LE.-99.0 ) THEN
               WDNUM(2) = WDNUM(2) + 1
            ELSE IF( FLWVEC(1,HR0124).LT.0.0 ) THEN
               CONTINUE
            ELSE
               WDNUM(1) = WDNUM(1) + 1
C
               WORK1(1) = FLWVEC(1,HR0124) - 180.0
               IF( WORK1(1).LT.0.0 ) WORK1(1) = WORK1(1) + 360.0
C
C       DETERMINE WIND SECTOR
C
               IF( MDSTAT.EQ.13 ) THEN
                  IWORK1(1) = 36
                  WORK1(2)  = 10.0
               ELSE
                  IWORK1(1) = 16
                  WORK1(2)  = 22.5
               END IF
C
               IRD3 = WORK1(1)/WORK1(2) + 1.5
               IF( IRD3.GT.IWORK1(1) ) IRD3 = 1
C
            END IF
C
C       IF ALL THREE (IRD3,IRD4,IRD5) ARE GT. ZERO, WE CAN
C       INCREMENT COUNTERS IN FREQ-ARRAY
C
            IF( IRD3.GT.0 .AND. IRD4.GT.0 .AND. IRD5.GT.0 ) THEN
               FREQ(IRD5,IRD4,IRD3) = FREQ(IRD5,IRD4,IRD3) + 1
C
               TOTAL = TOTAL + 1
            END IF
C
100      CONTINUE
C
         RETURN
      END IF
C
C       SUMMARY SECTION
C
C       COMPUTE AVERGE TEMPERATURE
C
      IF( TTNUM(1).GT.0 ) THEN
         TTAVG = TTAVG/FLOAT( TTNUM(1) )
      END IF
C
C       COMPUTE AVERAGE HARMONIC WIND SPEEDS FOR EACH WIND
C       SPEED CLASS
C
      DO 110 I=1,6
         IF( WSAVG(1,I).GT.0 ) THEN
            WSAVG(2,I) = WSAVG(2,I)/ WSAVG(1,I)
            WSAVG(2,I) = 1.0/WSAVG(2,I)
         END IF
110   CONTINUE
C
C       FINIALIZE SUMMARY OF MIXING HEIGHTS
C
C       FIRST COMPUTE AVERAGES FOR EACH STABILITY CATEGORY
C
      DO 140 J=1,2
C       LOOP ON RURAL, URBAN
         DO 130 I=1,6
C       LOOP ON STABILITY CATEGORY
            IRD5 = 0
C
            DO 120 K=1,6
C       LOOP ON HEIGHT RANGES
               IRD5 = IRD5 + ZIFREQ(J,I,K)
120         CONTINUE
            IF(ZIFREQ(J,I,7).GT.0.0 .AND. IRD5.GT.0 ) THEN
               ZIFREQ(J,I,7) = ZIFREQ(J,I,7)/FLOAT( IRD5 )
            END IF
130      CONTINUE
140   CONTINUE
C
C       NOW COMPUTE NUMBER OF CASES WITHIN EACH HEIGHT RANGE
C
      DO 170 J=1,2
C       LOOP ON RURAL, URBAN
         DO 160 K=1,6
C       LOOP ON HEIGHT RANGES
            IRD5 = 0
C
            DO 150 I=1,6
C       LOOP ON STABILTY CATEGORY
               IRD5 = IRD5 + ZIFREQ(J,I,K)
150         CONTINUE
            ZIFREQ(J,7,K) = IRD5
160      CONTINUE
170   CONTINUE
C
C       FINIALIZE JOINT FREQUENCY FUNCTION ARRAY
C
      IF( MDSTAT.EQ.13) THEN
         IRD5 = 36
      ELSE
         IRD5 = 16
      END IF
C
C       TO DISTRIBUTE CALMS WE NEED TO KNOW HOW
C       MANY CASES WE HAVE IN WITHIN THE LOWEST
C       WIND SPEED CLASS
C
      IF( WSNUM(3).GT.0 ) THEN
         L = 1
         SUMLOW = 0.0
C
         DO 240 I=1,6
            DO 230 K=1,IRD5
               WORK2(I,K) = 0.0
230         CONTINUE
240      CONTINUE
C
250      DO 270 I=1,6
C       LOOP ON STABILITY CATEGORY
            DO 260 K=1,IRD5
C       LOOP ON WIND DIRECTION SECTORS
               SUMLOW = SUMLOW + FREQ(I,L,K)
               WORK2(I,K) = WORK2(I,K) + FREQ(I,L,K)
260         CONTINUE
270      CONTINUE
C
         IF( SUMLOW.LE.0.0 ) THEN
            L = L + 1
            IF( L.LE.6 ) GO TO 250
            IF( L.GT.6 ) L = 6
         END IF
C
      END IF
C
      DO 300 I=1,6
C       LOOP ON STABILITY CATEGORY
         DO 290 J=1,6
C       LOOP ON WIND SPEED CLASS
            DO 280 K=1,IRD5
C       LOOP ON WIND DIRECTION SECTORS
C
               IF( TOTAL.GT.0 ) THEN
                  IF( J.EQ.1 .AND. SUMLOW.GT.0 ) THEN
                     FREQ(I,J,K) = FREQ(I,J,K) +
     1                FLOAT(WSNUM(3)) * WORK2(I,K)/SUMLOW
                     FREQ(I,J,K) = FREQ(I,J,K)/( FLOAT(TOTAL)+WSNUM(3))
                  ELSE
                     FREQ(I,J,K) = FREQ(I,J,K)/( FLOAT(TOTAL)+WSNUM(3))
                  END IF
                  SUMALL = SUMALL + FREQ(I,J,K)
               END IF
C
280         CONTINUE
290      CONTINUE
300   CONTINUE
C
      RETURN
      END
