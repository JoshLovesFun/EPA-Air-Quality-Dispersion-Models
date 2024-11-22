      SUBROUTINE SFEXT
C=====================================================================**
C
C  Purpose
C     This subroutine is the main routine to retrieve the surface
C     observation and precipitation data and write it to a disk file
C     for the QA program.  Data are read and written in 1-day blocks.
C
C  Called by: SFPATH
C
C  Modified by:   Pacific Environmental Services
C      4/28/95:   Added processing for SAMSON and TD3240 data
C
C  Version date:  AUGUST 1994                                           DTBAUG94
C                 AUGUST 1995   (95227)                                 JOPAUG95
C
C-----------------------------------------------------------------------
C     Data declaration

      INTEGER  SF2YR, SF4YR, CENTURY
      INTEGER  SFARG, NCHREC, SFBFST, PPBFST, IVBL
      INTEGER  JULIAN, MULT(23), SFKNT, PPKNT
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'
C
C     Data initializations
C
      DATA PATH/'SF'/, LOC/' SFEXT'/
      DATA SFKNT/0/, PPKNT/0/, MULT/4*1,6*100,4*1000,100,8*1/
C
C     IVBL      Loop index
C     MULT( )   Multipliers to retain significant digits
C     SFBFST    Buffer status returned from RD144? or RDSAMS
C     PPBFST    Buffer status retruned from RD3240
C     SFARG     Integer variable to use for whatever purpose
C     SFKNT     Number of hourly weather obs processed
C     PPKNT     Number of precipitation obs processed
C-----------------------------------------------------------------------

C     Write the standard headers to the error/message file
      MESS = BLNK40
      WRITE(MESS,2100)
      CALL ERROR(0,PATH,'I40',LOC,MESS)

C     Write the headers to the output file
      WRITE(DEV21,2101) IVDATE                                          DTBAUG94

C     Initialize the work arrays; set the individual missing indicators
C     for the concatenated variables (excludes present weather)
      CALL FLWRK1
      CALL FLWRK2
      CALL FLIWK1
      CALL FLIWK2
      CALL FLSFC(24)
      CALL FLPPT(24)

      DO 100 K=34,43
         SFQA1(K,1) = INT(SFQA(K,2)/MULT(K-29))
         SFQA1(K,2) = SFQA(K,2) - SFQA1(K,1)*MULT(K-29)
  100 CONTINUE

C     Compute the beginning and ending chronological
C     days to extract                                      ---- CALL CHROND

      SFARG = JULIAN(SFYR1,SFGMO1,SFGDY1)
      CALL CHROND(PATH,SFYR1,SFARG,SFDAY1)
      SFARG = JULIAN(SFYR2,SFGMO2,SFGDY2)
      CALL CHROND(PATH,SFYR2,SFARG,SFDAY2)

C---- Check the surface format and set the parameters
C        - CD144 for card image data in 144 format
C        - SCRAM for TTN compressed 28 character format
C        - SAMSON for data from SAMSON CD

      NHRSF = 0
      IF(INDEX(SFFMT,'SCRAM') .NE. 0) THEN
         SFBLK = 'FB'
         NCHREC = 28
      ELSE IF(INDEX(SFFMT,'CD144') .NE. 0) THEN
         SFBLK = 'FB'
         NCHREC = 80
      ELSE IF(INDEX(SFFMT,'SAMSON') .NE. 0) THEN
         SFBLK =  'FB'
         NCHREC = 120
         CALL SETSAM( NVARS, ISTAT )
Cdbg     PRINT *, SAMFMT, NVARS, (IDVAR(J),J=1,NVARS)
         IF( ISTAT .EQ. 1 ) THEN
            SFSTAT = -1
            RETURN
         ENDIF
      ENDIF

C     Now check the precipitation format; if it is a variable format file,
C     convert it to a fixed format before processing.  The output is
C     written to a temporary (scratch) file connected to unit number 26.
      IF( INDEX(PPTFMT,'TD3240') .NE. 0 )THEN
         IF( INDEX(PPTFMT,'VB') .NE. 0 )THEN
            CALL FIXPPT( DEV25, ISTAT )
            IF( ISTAT .EQ. 1 )THEN
C              An error occurred while writing the fixed format file
               SFSTAT = -1
               RETURN
            ENDIF
         ENDIF
      ENDIF

C---- Check the status of the buffer and retrieve the first
C     available observation within the station/date window

      IF(STATUS(3,7) .GE. 2) THEN
C        Working with CD144 or SCRAM                       ---- CALL RD144~
C                           or SAMSON format               ---- CALL RDSAMS
         SFBFST = 1
         IF( INDEX(SFFMT,'CD144') .NE. 0 )THEN
            IF( STATUS(3,7) .EQ. 2 )THEN
               CALL RD144D( SFBFST, NCHREC, NHRSF )
            ELSE
               CALL RD144T( SFBFST, NCHREC, NHRSF )
            ENDIF

         ELSEIF( INDEX(SFFMT,'SCRAM') .NE. 0 )THEN
            CALL RD144D( SFBFST, NCHREC, NHRSF )

         ELSEIF( INDEX(SFFMT,'SAMSON') .NE. 0 )THEN
            CALL RDSAMS( SFBFST, NVARS, NHRSF )
         ENDIF

C     ELSEIF(STATUS(3,11) .GE. 2) THEN
C        Working with TD3280 format                        ---- CALL RD328~
C        SFBFST = 1
C        IF( STATUS(3,11) .EQ. 2 )THEN
C           CALL RD328D( SFBFST, NCHREC, NHRSF )
C        ELSE
C           CALL RD328T( SFBFST, NCHREC, NHRSF )
C        ENDIF

      ELSE
C        No match on the format
         SFSTAT = -1
      ENDIF
      SFKNT = SFKNT + NHRSF

      IF(STATUS(3,12) .EQ. 2) THEN
C        Working with the precipitation data (TD3240)      ---- CALL RD3240
C        Note that the "current day" for precipitation is
C        used differently than for sfc weather obs.  PPDAYC
C        is an incremented variable, whereas SFDAYC is
C        computed from the data.
         PPBFST = 1
         PPDAYC = SFDAY1
         CALL RD3240( PPBFST,PPDAYC,PPKNT )

      ELSE
         PPBFST = -1
      ENDIF

C---- Begin processing the remaining data by checking the status and
C     writing the data to the output file

      DO WHILE( SFSTAT .GT. 0  .AND.
     &         (SFBFST .NE. -1  .OR.  PPBFST .NE. -1 ))
C-------\/
         IF(SFBFST .EQ. 2  .AND.  PPBFST .EQ. 2) THEN
C           Data available from both sources: CD-144/SCRAM/SAMSON (SFBFST)
C           and TD-3240 (PPBFST)

            CALL Y2K(PATH, SFGYR, SF2YR, SF4YR, CENTURY)

            WRITE(*, 610 ) SFGMO, SFGDY, SF4YR
  610       FORMAT('+  Stage 1: Extracting surface data for ',
     &             'month-day-year ', 2(I2.2,:'-'),I4)

            IF( SFDAYC .EQ. PPDAYC ) THEN
C              The data are for the same day
               DO 200 IHR = 1,24

                  IF( INDEX(SFFMT,'SAMSON') .NE. 0 )THEN
C                    SAMSON data being used (and stored in the SFOBS array);
C                    substitute precipitation data from TD3240 (in the PPTAMT
C                    array) only if SAMSON precip (SFOBS(ihr,52)) is missing
C                    (i.e., SAMSON precip takes precedence)
                     IF( SFOBS(IHR,52) .GE. 0) THEN
                        PPTAMT(IHR) = SFOBS(IHR,52)
                     ENDIF
                  ENDIF
                  WRITE(DEV21,2110) SFGYR,SFGMO,SFGDY,IHR,
     &                        (SFOBS(IHR,IVBL),IVBL=30,51),PPTAMT(IHR)
  200          CONTINUE
C              Reinitialize arrays and read data
               CALL FLSFC(24)
               CALL FLPPT(24)
               NHRSF = 0
               IF( STATUS(3,7) .GE. 2 )THEN
C                 Read CD144 or SCRAM                      ---- CALL RD144~
C                            or SAMSON format              ---- CALL RDSAMS
                  SFBFST = 1
                  IF( INDEX(SFFMT,'CD144') .NE. 0 )THEN
                     IF( STATUS(3,7) .EQ. 2 )THEN
                        CALL RD144D( SFBFST, NCHREC, NHRSF )
                     ELSE
                        CALL RD144T( SFBFST, NCHREC, NHRSF )
                     ENDIF

                  ELSEIF( INDEX(SFFMT,'SCRAM') .NE. 0 )THEN
                     CALL RD144D( SFBFST, NCHREC, NHRSF )

                  ELSEIF( INDEX(SFFMT,'SAMSON') .NE. 0 )THEN
                     CALL RDSAMS( SFBFST, NVARS, NHRSF )
                  ENDIF


C              ELSEIF(STATUS(3,11) .GE. 2) THEN
C                 Read TD3280 format                       ---- CALL RD328~
C                 SFBFST = 1
C                 IF( STATUS(3,11) .EQ. 2 )THEN
C                    CALL RD328D(SFBFST, NCHREC, NHRSF )
C                 ELSE
C                    CALL RD328T(SFBFST, NCHREC, NHRSF )
C                 ENDIF

               ENDIF
               SFKNT = SFKNT + NHRSF

C              Read precipitation data (TD3240)            ---- CALL RD3240
               PPBFST = 1
               PPDAYC = PPDAYC + 1
               CALL RD3240( PPBFST, PPDAYC, PPKNT )

            ELSE IF ( PPDAYC .LT. SFDAYC ) THEN
C              The precipitation data are before the hourly weather obs
               DO 210 IHR = 1,24
                  WRITE(DEV21,2110) PPTGYR,PPTGMO,PPTGDY,IHR,
     &                             (SFQA(IVBL,2),IVBL=30,51),
     &                              PPTAMT(IHR)
  210          CONTINUE
               CALL FLPPT(24)
               PPDAYC = PPDAYC + 1
               CALL RD3240( PPBFST, PPDAYC, PPKNT )

            ELSE IF ( SFDAYC .LT. PPDAYC ) THEN
C              The hourly weather obs are before the precipitation data
               DO 220 IHR = 1,24
                  WRITE(DEV21,2110) SFGYR,SFGMO,SFGDY,IHR,
     &                           (SFOBS(IHR,IVBL),IVBL=30,51),SFQA(52,2)
  220          CONTINUE
               CALL FLSFC(24)
               NHRSF = 0
               IF( STATUS(3,7) .GE. 2 )THEN
C                 Read CD144 or SCRAM                      ---- CALL RD144~
C                            or SAMSON format              ---- CALL RDSAMS
                   SFBFST = 1
                   IF( INDEX(SFFMT,'CD144') .NE. 0 )THEN
                      IF( STATUS(3,7) .EQ. 2 )THEN
                         CALL RD144D( SFBFST, NCHREC, NHRSF )
                      ELSE
                         CALL RD144T( SFBFST, NCHREC, NHRSF )
                      ENDIF

                   ELSEIF( INDEX(SFFMT,'SCRAM') .NE. 0 )THEN
                      CALL RD144D( SFBFST, NCHREC, NHRSF )

                   ELSEIF( INDEX(SFFMT,'SAMSON') .NE. 0 )THEN
                      CALL RDSAMS( SFBFST, NVARS, NHRSF )
                   ENDIF


C              ELSEIF(STATUS(3,11) .GE. 2) THEN
C                 Read TD3280 format                       ---- CALL RD328~
C                 SFBFST = 1
C                 IF( STATUS(3,11) .EQ. 2 )THEN
C                    CALL RD328D( SFBFST, NCHREC, NHRSF )
C                 ELSE
C                    CALL RD328T( SFBFST, NCHREC, NHRSF )
C                 ENDIF

               ENDIF
               SFKNT = SFKNT + NHRSF
            ENDIF

C-------\/
         ELSEIF( SFBFST .EQ. -1 .AND. PPBFST .EQ. 2 )THEN
C           No hourly surface obs, but precipitation from a TD3240
C           file is available
            WRITE(*, 610)  pptgmo,pptgdy,pptgyr
            DO 230 IHR = 1,24
               WRITE(DEV21,2110) PPTGYR,PPTGMO,PPTGDY,IHR,
     &                          (SFOBS(IHR,IVBL),IVBL=30,51),PPTAMT(IHR)
  230       CONTINUE
            CALL FLPPT(24)
            CALL FLSFC(24)
            NHRSF = 0
            PPDAYC = PPDAYC + 1
            CALL RD3240( PPBFST, PPDAYC, PPKNT )

C-------\/
         ELSEIF( (SFBFST.EQ.2) .AND. (PPBFST .EQ. -1) )THEN
C           Hourly surface obs available; no precipitation file
C           to process, but there may be precip data from SAMSON
C           (the reason for the next assignment stmt)

            CALL Y2K(PATH, SFGYR, SF2YR, SF4YR, CENTURY)
            WRITE(*, 610 ) SFGMO, SFGDY, SF4YR

            DO 240 IHR = 1,24
               PPTAMT(IHR) = SFOBS(IHR,52)
               WRITE(DEV21,2110) SFGYR,SFGMO,SFGDY,IHR,
     &                        (SFOBS(IHR,IVBL),IVBL=30,51),PPTAMT(IHR)
  240       CONTINUE
            CALL FLSFC(24)
            CALL FLPPT(24)
            NHRSF = 0
            IF( STATUS(3,7) .GE. 2 )THEN
C               Read CD144 or SCRAM                        ---- CALL RD144~
C                          or SAMSON format                ---- CALL RDSAMS
                SFBFST = 1
                IF( INDEX(SFFMT,'CD144') .NE. 0 )THEN
                   IF( STATUS(3,7) .EQ. 2 )THEN
                      CALL RD144D( SFBFST, NCHREC, NHRSF )
                   ELSE
                      CALL RD144T( SFBFST, NCHREC, NHRSF )
                   ENDIF

                ELSEIF( INDEX(SFFMT,'SCRAM') .NE. 0 )THEN
                   CALL RD144D( SFBFST, NCHREC, NHRSF )

                ELSEIF( INDEX(SFFMT,'SAMSON') .NE. 0 )THEN
                   CALL RDSAMS( SFBFST, NVARS, NHRSF )
                ENDIF

C           ELSEIF(STATUS(3,11) .GE. 2) THEN
C              Read TD3280 format                          ---- CALL RD328~
C              SFBFST = 1
C              IF( STATUS(3,11) .EQ. 2 )THEN
C                 CALL RD328D( SFBFST, NCHREC, NHRSF )
C              ELSE
C                 CALL RD328T( SFBFST, NCHREC, NHRSF )
C              ENDIF

            ENDIF
            SFKNT = SFKNT + NHRSF
C-------\/
         ENDIF

      ENDDO

      IF( SFSTAT .GT. 0  .AND.
     &   ((SFBFST.EQ.-1) .AND. (PPBFST.EQ.-1)) ) THEN
C        No (more) data for both; print message;
C        if no data extracted, set SFSTAT to -1
         MESS = BLNK40
         WRITE(MESS,600) SFKNT, PPKNT
         CALL ERROR(0,PATH,'I49',LOC,MESS)
         IF( (SFKNT+PPKNT) .EQ. 0) THEN
            SFSTAT = -1
            WRITE( MESS, 601)
            CALL ERROR(0,PATH,'W48',LOC,MESS)
         ELSEIF( NHRSF .GT. 0 )THEN

C           Write the last day of data
            DO 250 IHR = 1,24
               IF( INDEX(SFFMT,'SAMSON') .NE. 0 )THEN
C                 SAMSON data being used; substitute precipitation
C                 data from TD3240 only if SAMSON precip is missing
C                 (i.e., SAMSON precip takes precedence)
                  IF( SFOBS(IHR,52) .GE. 0) THEN
                     PPTAMT(IHR) = SFOBS(IHR,52)
                  ENDIF
               ENDIF
               WRITE(DEV21,2110) SFGYR,SFGMO,SFGDY,IHR,
     &                     (SFOBS(IHR,IVBL),IVBL=30,51),PPTAMT(IHR)
  250       CONTINUE
         ENDIF
      ENDIF

C---- FORMAT STATMENTS

  600 FORMAT(I5,' HLY WX &',I5,' PRECIP OBS EXTRACTED')
  601 FORMAT(' NO HOURLY WX or PRECIP DATA EXTRACTED')
 2100 FORMAT(' *** HLY SFC OBS & PRECIP EXTRACTION ***')
 2101 FORMAT('*  SF     SURFACE EXTRACTION  -  MPRM DATED ', I5)        DTBAUG94
 2110 FORMAT(1X,4I2,4(1X,I5),6(1X,I5.5),/,8X,5(1X,I5.5),7(1X,I5),1X,I6)

      RETURN
      END


      SUBROUTINE RD144D(SFBFST,NCHREC,NHRSF)
C=====================================================================**
C
C  PURPOSE
C     To retrieve a block of 24 hourly surface obs. in the station/date
C     window and return it to the calling program, SFMAIN.
C
C  VERSION DATE: 30 SEPT 1992
C
C-----------------------------------------------------------------------
C     Data declarations

      INTEGER       JULIAN, NRECS, LDAYC, JDAY, NHRSF
      INTEGER       SFBFST, ISTAT, IOST20, NCHREC
C
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'
C
C     Data Initialization
      DATA PATH/'SF'/, LOC/'RD144D'/, NRECS/0/


C     Processing begins here for surface obs. data on disk that
C     are either 80 or 28 characters per record, 1 hour per record.
C     If SCRAM bbs data are input, record length is 28, so read it
C     into BUF28, then pass into first 28 characters of buf80(1) to
C     make use of some of the CD144 structure.
C
  300 BUF80(1) = BLNK80
      NHRSF = 0
      NRECS = NRECS + 1
      IF( NCHREC.EQ.80 ) THEN
         READ(DEV20,2010,END=310,ERR=320,IOSTAT=IOST20) BUF80(1)

      ELSEIF( NCHREC.EQ.28 ) THEN
         BUF28 = BLNK28
         READ(DEV20,2011,END=310,ERR=320,IOSTAT=IOST20) BUF28
         WRITE(BUF80(1),2011) BUF28
      ENDIF

C     Decode station date group;                           ---- CALL D144HD
      CALL D144HD(BUF80(1),ISTAT,IOST20,NRECS)
      IF(ISTAT .EQ. 1) GO TO 520

C     Check for inclusion in the station/date window;
C     the index function is used to insure a correct match on station
C     identifiers - BUF08 is left justified, SFLOC1 is right justified;
C     if the identifier field requires 8 characters there is no blank

      IWORK1(20) = INDEX(BUF08(1),' ')
      IF(IWORK1(20) .EQ. 0) THEN
         IWORK1(21) = INDEX(SFLOC1,BUF08(1)(1:8))
      ELSE
         IWORK1(21) = INDEX(SFLOC1,BUF08(1)(1:IWORK1(20)-1))
      ENDIF

C     The stations do not match
      IF(IWORK1(21) .EQ. 0 )THEN
C        Station mismatch
         MESS = BLNK40
         WRITE(MESS,420)
  420    FORMAT( ' STATION MISMATCH IN SFC OBS FILE' )
         CALL ERROR(NPREAD,PATH,'E40',LOC,MESS)
         MESS = BLNK40
         WRITE(MESS,430) BUF08(1),SFLOC1
  430    FORMAT( ' FILE: ',A8,'  USER EXPECTS: ',A8 )
         CALL ERROR(0,'  ','+++','      ',MESS)
         SFSTAT = -1
         RETURN
       ENDIF

C     Compute the Julian day                               ---- FUNC.JULIAN
      JDAY = JULIAN(SFGYR,SFGMO,SFGDY)

C     Compute chronological day (days since Jan 1, 1900)   ---- CALL CHROND
      CALL CHROND(PATH,SFGYR,JDAY,SFDAYC)

C     If this is hour 0, set it to hour 24 of the previous day
      IF( SFGHR .EQ. 0 )THEN
         SFGHR = 24
         SFDAYC = SFDAYC - 1
      ENDIF

      IF( SFDAYC .LT. SFDAY1 )THEN
C        The stations match but the data are not within the
C        extraction window
         GO TO 300

      ELSEIF( SFDAYC .GE. SFDAY1  .AND.  SFDAYC .LE. SFDAY2 )THEN
C        The correct time span and station have been located.
C        Process 24 hours of data
         DO WHILE (SFGHR .LE. 23  .AND.  SFSTAT .GT.  0  .AND.
     &                                   SFBFST .NE. -1 )
            LDAYC = SFDAYC
            IF(INDEX(SFFMT,'CD144') .NE. 0) THEN
C              CD144 format                                ---- CALL D144LV
               CALL D144LV(BUF80(1),ISTAT,IOST20,NRECS)

            ELSEIF(INDEX(SFFMT,'SCRAM') .NE. 0) THEN
C              SCRAM format                                ---- CALL D028LV
               CALL D028LV(BUF80(1),ISTAT,IOST20,NRECS)
            ENDIF
            IF( ISTAT .EQ. 1 )THEN
               GO TO 521

            ELSE
C              Increment counters:
C                 NHRSF tracks the # of hours retirved for the day
C                 KOUNT tracks # records in extract window
C                 NRECS tracks total number of records processed
               NHRSF = NHRSF + 1
               KOUNT = KOUNT + 1
               NRECS = NRECS + 1
            ENDIF

            IF( NCHREC.EQ.80 ) THEN
               READ(DEV20,2010,END=310,ERR=320,IOSTAT=IOST20) BUF80(1)

            ELSEIF( NCHREC.EQ.28 ) THEN
               BUF28 = BLNK28
               READ(DEV20,2011,END=310,ERR=320,IOSTAT=IOST20) BUF28
               WRITE(BUF80(1),2011) BUF28
            ENDIF

C           Decode station & date group                    ---- CALL D144HD
            CALL D144HD(BUF80(1),ISTAT,IOST20,NRECS)
            IF(ISTAT .EQ. 1) GO TO 520

C           Compute the Julian day                         ---- FUNC.JULIAN
            JDAY = JULIAN(SFGYR,SFGMO,SFGDY)

C           Compute chron. day (days since Jan 1, 1900)    ---- CALL CHROND
            CALL CHROND(PATH,SFGYR,JDAY,SFDAYC)

            IF( SFDAYC .NE. LDAYC )THEN
               IF( SFGHR .EQ. 0 )THEN
C                 Adjust the sequential day so it can be properly
C                 compared to precipitation when necessary
                  SFDAYC = SFDAYC - 1

C                 Convert HOUR=0 to HOUR=24 of previous day---- CALL HR0024
                  CALL HR0024( SFGYR, JDAY, SFGHR )

C                 As well as the month and day             ---- CALL GREG
C                 (to get correct date for last hour of the day)
                  CALL GREG( SFGYR, JDAY, SFGMO, SFGDY )

                  IF(INDEX(SFFMT,'CD144') .NE. 0) THEN
C                    CD144 format                          ---- CALL D144LV
                     CALL D144LV(BUF80(1),ISTAT,IOST20,NRECS)

                  ELSEIF(INDEX(SFFMT,'SCRAM') .NE. 0) THEN
C                    SCRAM format                          ---- CALL D028LV
                     CALL D028LV(BUF80(1),ISTAT,IOST20,NRECS)
                  ENDIF

                  IF( ISTAT .EQ. 1 ) THEN
                     GO TO 521
                  ELSE
                     KOUNT = KOUNT + 1
                     NHRSF = NHRSF + 1
                     SFBFST = 2
                  ENDIF

               ELSE
                  MESS = BLNK40
                  WRITE( MESS, 500 ) NRECS-1
                  CALL ERROR(KOUNT,PATH,'E41',LOC,MESS)
                  MESS = BLNK40
                  WRITE(MESS,608) SFGYR,SFGMO,SFGDY,SFGHR
                  CALL ERROR(KOUNT,'  ','+++','      ',MESS)
                  SFSTAT = -1
               ENDIF

C           endif for SFDAYC ne LDAYC
            ENDIF

C        enddo for processing 24 hours
         ENDDO

      ELSE IF ( SFDAYC .GT. SFDAY2 ) THEN
C        The data are after the extraction window
C        *** EXTRACTION PROCESS COMPLETED ***
C
         MESS = BLNK40
         WRITE(MESS,601) NRECS-1
         CALL ERROR(KOUNT,PATH,'I49',LOC,MESS)
         SFBFST = -1

C     endif for extraction date window
      ENDIF

      RETURN
C
C-----------------------------------------------------------------------
C     Processing continues here if an end of file was encountered
C     while reading from the file
C
  310 SFBFST = -1
      MESS = BLNK40
      WRITE(MESS,600) NRECS-1
      CALL ERROR(KOUNT,PATH,'I49',LOC,MESS)
      IF(KOUNT .LT. 1) THEN
         MESS = BLNK40
         WRITE(MESS,701)
         CALL ERROR(KOUNT,PATH,'W48',LOC,MESS)
      ENDIF
      RETURN

C-----------------------------------------------------------------------
C     Processing continues here if an error was encountered reading the
C     file.  SFSTAT is set to -1 to prevent further processing if the
C     maximum number of errors is reached.

  320 MESS = BLNK40
      WRITE(MESS,603) NRECS
      CALL ERROR(KOUNT,PATH,'E42',LOC,MESS)
      SFSTAT = -1
      RETURN
C
C-----------------------------------------------------------------------
C     Processing continues here if an error occurred with decoding:
C     A header

  520 CONTINUE
      SFSTAT = -1
      RETURN

C     An observation

  521 CONTINUE
      SFSTAT = -1
      RETURN


C-    Format statements

  500 FORMAT(' DATE CHANGE, EXPECTED HR=0, REC #', I5)
  600 FORMAT(' END-OF-FILE AFTER RECORD', I5)
  601 FORMAT(' END-OF DATA WINDOW AFTER RECORD', I5)
  603 FORMAT(' ERROR READING DATA, REC #',I5)
  608 FORMAT('   ON (YR/MO/DA/HR)',I2,'/',I2,'/',I2,'/',I2)
  701 FORMAT(' NO OBS EXTRACTED-CHECK INPUT STN/DATES')
 2010 FORMAT(A80)
 2011 FORMAT(A28)
C
      END



      SUBROUTINE D144HD( CARD,ISTAT,IOST20,NRECNO )
C=====================================================================**
C
C  PURPOSE
C     THIS SUBROUTINE DECODES THE STATION AND DATE GROUP FROM THE SURFACE
C     OBSERVATIONS AND RETURNS THEM TO THE MAIN PROGRAM.  THERE ARE ENTRY
C     POINTS FOR THE VARIOUS NWS FORMATS AVAILABLE.
C
C     ENTRY D144HD  = DECODED THE TD-1440 FIXED BLOCK STAION/DATE GROUP
C
C  VERSION DATE: 17 SEPTEMBER 1987
C
C=======================================================================
C
      INTEGER ISTAT, IOST20
      CHARACTER*80 CARD
C
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'WORK1.INC'
      DATA LOC/'D144LV'/
C
C
      READ(CARD,2050,ERR=20001,IOSTAT=IOST20) BUF08(1),
     &                                SFGYR,SFGMO,SFGDY,SFGHR
      ISTAT = 0
      RETURN
C-----------------------------------------------------------------------
20001 CONTINUE
      MESS = BLNK40
      WRITE(MESS,605) NRECNO
      CALL ERROR(KOUNT,PATH,'E42',LOC,MESS)
      ISTAT = 1
      RETURN
C
  605 FORMAT(' ERROR DECODING HEADER, REC #',I5)
 2050 FORMAT(A5,4I2)
C
      END



      SUBROUTINE D144LV(CARD,ISTAT,IOST20,NRECNO)
C=====================================================================**
C
C  PURPOSE
C     Reads the surface data from a character string and stores the
C     data in the appropriate variable.  This routine works with the
C     CD144 data.
C
C  VERSION DATE: 20 MAY 1993
C
C-----------------------------------------------------------------------
C
C     Data declarations

      INTEGER IABSNT, OPAQUE, IPW1, IPW2, IVIS(20)
      INTEGER IWETH, IVSBY, ISVRE, ILIQPC, IFRZPC, IOBSVS, ICEIL, ISCON
      INTEGER ISKY, ITYPE, ISTAT, IV, LL, IISK, ISUM2, ISUM3
      INTEGER ISUMTT, ISUMOP, IOST20
      INTEGER IDRYTMP
      REAL ABSENT, VISDIS(20), PRESSO, DEWPT, WDIR, WSP, STAPR, RH
      REAL WETTMP, DRYTMP, VSBY
      CHARACTER*80 CARD
      CHARACTER*1 OVRPCH(35),OVR11(10),OVR12(10),OVRNOR(10)
      CHARACTER*2 AWDIR,ATOT,AOPQ
      CHARACTER*3 AWSP,ADRYTMP,ACLG
C
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'
C
      DATA OVR11/'[','A','B','C','D','E','F','G','H','I'/,
     &     OVR12/']','J','K','L','M','N','O','P','Q','R'/,
     &     OVRNOR/'0','1','2','3','4','5','6','7','8','9'/
      DATA IVIS/0,1,2,3,4,5,6,7,8,9,10,12,14,16,17,18,19,20,24,27/
      DATA VISDIS/0.0, 0.067, 0.125, 0.188, 0.25, 0.312, 0.375, 0.50,
     &            0.625, 0.75, 1.0, 1.125, 1.25, 1.375, 1.5, 1.625,
     &            1.75, 2.0, 2.25, 2.5/
      DATA IABSNT/-9999/, ABSENT/-9999.0/
      DATA PATH/'SF'/, LOC/'D144LV'/
C
C-----------------------------------------------------------------------
C     Decode overpunches, processing the 35 possible overpunch characters
C
       READ(CARD,141,ERR=20001,IOSTAT=IOST20) (OVRPCH(I),I=1,7),
     &          IVSBY, IWETH, PRESSO, OVRPCH(8),
     &          DEWPT, WDIR, OVRPCH(9), WSP, STAPR, OVRPCH(10), DRYTMP,
     &          OVRPCH(11), WETTMP, RH, (OVRPCH(J),J=12,35)
141    FORMAT (13X,7A1,I3,I8,F4.0,A1,2F2.0,A1,F1.0,F4.0,A1,
     &         F2.0,A1,F2.0,F3.0,24A1)

C     Convert weather number into TD3280 weather codes     ---- CALL SFCWXX
      CALL SFCWXX(IWETH,ISVRE,ILIQPC,IFRZPC,IOBSVS)

C     Go through each overpunch and decode its meaning and make
C     the appropriate transformation; add 10 to the overpunch value
C     to more easily identify them.
C     (The flag for X or - is -8888 (and is interpreted later))

       DO 380 I=1,35
          SFOVR(I) = -99999
          IF(OVRPCH(I) .EQ. 'X'  .OR.  OVRPCH(I) .EQ. '-') THEN
             SFOVR(I) = -8888
          ELSE IF(OVRPCH(I) .EQ. ' ') THEN
             SFOVR(I) = IABSNT
          ELSE
             DO 385 J=0,9
               IF(OVRPCH(I) .EQ. OVRNOR(J+1)) THEN
                  SFOVR(I)=J
                  GO TO 386
               ELSE IF(OVRPCH(I) .EQ. OVR11(J+1)) THEN
                  SFOVR(I)=J+10
                  GO TO 386
               ELSE IF(OVRPCH(I) .EQ. OVR12(J+1)) THEN
                  SFOVR(I)=J+10
                  GO TO 386
               ENDIF
  385        CONTINUE
  386        CONTINUE
          ENDIF

C     Check to make sure a proper overpch character was found

          IF(SFOVR(I) .EQ. -99999) THEN
            MESS = BLNK40
            WRITE(MESS,390) I,SFGYR,SFGMO,SFGDY,SFGHR
  390       FORMAT('OVRPNCH ',I2,' IMPROPERLY DECODED: ',3i2.2,'/',I2)
            CALL ERROR(0,PATH,'W43',LOC,MESS)
          ENDIF
  380  CONTINUE

C     Convert selected "blank" overpunches to "zero" -- we assume that
C     such blanks are intentional, since missing values will be caught
C     by checking for blanks in the data fields.

C     - Ceiling height:
       ITEST=SFOVR(1)+SFOVR(2)+SFOVR(3)
       IF(ITEST .NE. (3*IABSNT)) THEN
          DO 381 I=1,3
 381      IF(SFOVR(I) .EQ. IABSNT) SFOVR(I)=0
       ENDIF

C     - Temps and wind speed:
       DO 382 I=8,11
 382   IF(SFOVR(I) .EQ. IABSNT) SFOVR(I)=0

C-----------------------------------------------------------------------
C     Altimeter pressure, not available on CD-144 format

      SFOBS(SFGHR,30) = SFQA(30,2)

C     Decode the data in the order it will appear on output

C     Sea level pressure (mb)
      IF(CARD(32:35).EQ.'    ') THEN
        SFOBS(SFGHR,31) = SFQA(31,2)

      ELSE
        IF (PRESSO.LT.800.) THEN
          PRESSO = PRESSO/10.0 + 1000.
        ELSE
          PRESSO = PRESSO/10.0
        ENDIF
        SFOBS(SFGHR,31) = INT(PRESSO *10)
      ENDIF

C     Station pressure (mb)
      IF(CARD(43:46).EQ.'    ') THEN
        SFOBS(SFGHR,32) = SFQA(32,2)
      ELSE
        STAPR = STAPR/100.0
C       Convert inches to millibars                        ---- CALL P2MMBR
        CALL P2MMBR(STAPR,XRD1)
        SFOBS(SFGHR,32) = INT((XRD1 *10) + 0.5)
      ENDIF

C     Ceiling height (km & tenths)                         ---- CALL CLHT
C     ( 300 (30.0 km) == unlimited)
      CALL CLHT(1,ICEIL,IABSNT,SFQA(33,2))
      SFOBS(SFGHR,33) = ICEIL
C
C***  Start concatenating variables ***
C
C     Total and opaque sky cover (tenths)                  ---- CALL CVG
      CALL CVG(12,ISUMTT,IABSNT,SFQA1(34,1))
      CALL CVG(35,ISUMOP,IABSNT,SFQA1(34,2))
      SFOBS(SFGHR,34) = ISUMTT*100 + ISUMOP
C
C     Sky cover for the lowest 2, 3 layers (tenths)        ---- CALL CVG
      CALL CVG(23,ISUM2,IABSNT,SFQA1(35,1))
      CALL CVG(29,ISUM3,IABSNT,SFQA1(35,2))
      SFOBS(SFGHR,35) = ISUM2*100 + ISUM3

C---- Sky condition and coverage at 4 levels (overpunches 4-7);
C     sky conditions are mapped to the TD-3280 codes (conditions
C     0, 1, and 2 are the same in both formats).  A check of the
C     sky conditions above the current level is made to determine
C     if there is any obscuration of the sky (ISCON=8), if the
C     indicator for the current level is a blank (IABSNT).
C
      DO 420 IISK = 1,4
         IF(SFOVR(IISK+3) .EQ. IABSNT) THEN
            ISCON = 9
            DO 421 JISK = 4, IISK+1, -1
               IF( SFOVR(JISK+3) .NE. IABSNT )THEN
                  ISCON = 8
               ENDIF
  421       CONTINUE

         ELSE IF(SFOVR(IISK+3) .EQ. -8888) THEN
            ISCON = 7

         ELSE
            ISCON = SFOVR(IISK+3)

            IF( (ISCON.EQ.0) .OR. (ISCON.EQ.1) .OR. (ISCON.EQ.2) ) THEN
              CONTINUE
            ELSE IF(ISCON .EQ. 4) THEN
              ISCON = 3
            ELSE IF (ISCON .EQ. 5) THEN
              ISCON = 4
            ELSE IF (ISCON .EQ. 7) THEN
              ISCON = 5
            ELSE IF (ISCON .EQ. 8) THEN
              ISCON = 6
            ELSE
              ISCON = SFQA1(IISK+35,1)
            ENDIF

         ENDIF

C------- Determine the proper overpunch to use for coverage
         IF(IISK .GE. 2) THEN
          LL = 18 + (IISK - 2)*6
         ELSE
          LL = 13
         ENDIF

         CALL CVG(LL,ISKY,IABSNT,SFQA1(IISK+35,2))

         SFOBS(SFGHR,IISK+35) = ISCON*100 + ISKY
  420 CONTINUE

C     Cloud types and height, 4 layers (if the type was overpunched
C     with an 'X', then the value is reported as 12, 14, 15, 16, 17,
C     or 19                                                ---- CALL CLOUDS
C                                                          ---- CALL CLHT

C--   First layer
        CALL CLOUDS(14,ITYPE,IABSNT,SFQA1(40,1))
        CALL CLHT(15,ICEIL,IABSNT,SFQA1(40,2))
        SFOBS(SFGHR,40) = ITYPE*1000 + ICEIL
C
C--   Second layer
        CALL CLOUDS(19,ITYPE,IABSNT,SFQA1(41,1))
        CALL CLHT(20,ICEIL,IABSNT,SFQA1(41,2))
        SFOBS(SFGHR,41) = ITYPE*1000 + ICEIL
C
C--   Third layer
        CALL CLOUDS(25,ITYPE,IABSNT,SFQA1(42,1))
        CALL CLHT(26,ICEIL,IABSNT,SFQA1(42,2))
        SFOBS(SFGHR,42) = ITYPE*1000 + ICEIL
C
C--   Fourth layer
        CALL CLOUDS(31,ITYPE,IABSNT,SFQA1(43,1))
        CALL CLHT(32,ICEIL,IABSNT,SFQA1(43,2))
        SFOBS(SFGHR,43) = ITYPE*1000 + ICEIL
C
C-    Present weather: save the liquid and frozen precip only
      IPW1 = 0
      IPW2 = 0

c     IF(ILIQPC .NE. 0) THEN
c        IPW1 = ILIQPC
c        IF(ISVRE .NE. 0) THEN
c           IPW2 = ISVRE
c           GO TO 440
c        ELSE IF(IFRZPC .NE. 0) THEN
c           IPW2 = IFRZPC
c           GO TO 440
c        ELSE IF(IOBSVS .NE. 0) THEN
c           IPW2 = IOBSVS
c           GO TO 440
c        ELSE
c           IPW2 = 0
c           GO TO 440
c        ENDIF
c
c     ELSE IF(IFRZPC .NE. 0) THEN
c        IPW1 = IFRZPC
c        IF(ISVRE .NE. 0) THEN
c           IPW2 = ISVRE
c           GO TO 440
c        ELSE IF(IOBSVS .NE. 0) THEN
c           IPW2 = IOBSVS
c           GO TO 440
c        ELSE
c           IPW2 = 0
c           GO TO 440
c        ENDIF
c
c     ELSE IF(ISVRE .NE. 0) THEN
c        IPW1 = ISVRE
c        IF(IOBSVS .NE. 0) THEN
c           IPW2 = IOBSVS
c           GO TO 440
c        ELSE
c           IPW2 = 0
c           GO TO 440
c        ENDIF
c
c     ELSE IF(IOBSVS .NE. 0) THEN
c        IPW1 = IOBSVS
c        IPW2 = 0
c     ENDIF

      IPW1 = ILIQPC
      IPW2 = IFRZPC
  440 SFOBS(SFGHR,44) = IPW1*100 + IPW2

C***  Completion of concatened variables


C-    Horizontal visiblity (km)  (1609.3 meters/mile conversion used)
C     to the (nearest whole kilometer * 10)
      IF(CARD(21:23) .EQ. '   ') THEN
         VSBY = -9999.0

      ELSE IF(IVSBY .LT. 30) THEN
         DO 20 IV = 1,20
            IF(IVSBY .EQ. IVIS(IV)) THEN
               VSBY = VISDIS(IV)
               GO TO 450
            ENDIF
   20    CONTINUE

      ELSE IF (IVSBY .LT. 990) THEN
         VSBY = IVSBY/10.0

      ELSE IF(IVSBY .GE. 990) THEN
         VSBY = 100.0

      ELSE
         VSBY = ABSENT
      ENDIF
C
  450 IF(VSBY .LT. 0.0) THEN
         SFOBS(SFGHR,45) = SFQA(45,2)

      ELSE
         VSBY = VSBY*1609.3/1000.0
         SFOBS(SFGHR,45) = INT(VSBY*10 + 0.5)
      ENDIF
C
C     Dry bulb temperature (deg C and tenths)
      IF(CARD(48:49) .EQ. '  '   .OR.  SFOVR(10) .EQ. -99999) THEN
         SFOBS(SFGHR,46) = SFQA(46,2)

      ELSE IF(SFOVR(10).EQ.-8888) THEN
         DRYTMP = -DRYTMP
         CALL P2MCEN(DRYTMP,XRD2)
         SFOBS(SFGHR,46) =  NINT(XRD2 * 10.0)

      ELSE
         IF(SFOVR(10).GE.10) THEN
            DRYTMP = -100.0 - DRYTMP
            CALL P2MCEN(DRYTMP,XRD2)
            SFOBS(SFGHR,46) = NINT(XRD2 * 10.0)

         ELSE
            DRYTMP = SFOVR(10)*100.0 + DRYTMP
            CALL P2MCEN(DRYTMP,XRD2)
            SFOBS(SFGHR,46) = NINT(XRD2 * 10.0)
         ENDIF
      ENDIF
C
C     Wet bulb temperature (deg C and tenths)
      IF(CARD(51:52) .EQ. '  '   .OR.  SFOVR(11) .EQ. -99999) THEN
         SFOBS(SFGHR,47) = SFQA(47,2)

      ELSE IF(SFOVR(11).EQ.-8888) THEN
         WETTMP = -WETTMP
         CALL P2MCEN(WETTMP,XRD2)
         SFOBS(SFGHR,47) = NINT(XRD2 * 10.0)

      ELSE
         WETTMP = SFOVR(11)*100.0 + WETTMP
         CALL P2MCEN(WETTMP,XRD2)
         SFOBS(SFGHR,47) = NINT(XRD2 * 10.0)
      ENDIF
C
C     Dew point (deg C and tenths)
      IF(CARD(37:38) .EQ. '  '  .OR.  SFOVR(8) .EQ. -99999) THEN
         SFOBS(SFGHR,48) = SFQA(48,2)
      ELSE IF(SFOVR(8).EQ.-8888) THEN
         DEWPT = -DEWPT
         CALL P2MCEN(DEWPT,XRD2)
         SFOBS(SFGHR,48) = NINT(XRD2 * 10.0)
      ELSE
         DEWPT = SFOVR(8)*100.0 + DEWPT
         CALL P2MCEN(DEWPT,XRD2)
         SFOBS(SFGHR,48) = NINT(XRD2 *10.0)
      ENDIF
C
C     Relative humidity (whole percent)
      IF(CARD(53:55) .EQ. '   ') THEN
         SFOBS(SFGHR,49) = SFQA(49,2)
      ELSE
         SFOBS(SFGHR,49) = RH
      ENDIF
C
C     Wind direction (tens of degrees from north)
      IF(CARD(39:40) .EQ. '  ') THEN
         SFOBS(SFGHR,50) = SFQA(50,2)
      ELSE
         SFOBS(SFGHR,50) = WDIR
      ENDIF
C
C     Wind speed (meters/sec and tenths)
      IF(CARD(42:42) .EQ. ' '   .OR.
     1      SFOVR(9) .EQ. -8888  .OR.
     2      SFOVR(9) .EQ. -99999) THEN
         SFOBS(SFGHR,51) = SFQA(51,2)
      ELSE
         WSP = WSP + SFOVR(9)*10.
         CALL P2MMSC(WSP,XRD2)
         SFOBS(SFGHR,51) = NINT(XRD2 *10.0)
      ENDIF
C
      RETURN
C-----------------------------------------------------------------------
C- PROCESSING CONTINUES HERE IF THERE IS AN ERROR DECODING THE STRING
C
20001 CONTINUE
      MESS = BLNK40
      WRITE(MESS,607) NRECNO
      CALL ERROR(KOUNT,PATH,'E42',LOC,MESS)
      MESS = BLNK40
      WRITE(MESS,608) SFGYR,SFGMO,SFGDY,SFGHR
      CALL ERROR(KOUNT,'  ','+++','      ',MESS)

      ISTAT = 1
      RETURN

  607 FORMAT(' ERROR DECODING DATA, REC #',I5)
  608 FORMAT('   ON (YR/MO/DA/HR)',I2,'/',I2,'/',I2,'/',I2)

      END


      SUBROUTINE D028LV(CARD,ISTAT,IOST20,NRECNO)
C=====================================================================**
C
C  PURPOSE
C     Reads the surface data from a character string and stores the
C     data in the appropriate variable.  This routine works with the
C     SCRAM compressed format.
C
C  VERSION DATE: 20 MAY 1993
C
C-----------------------------------------------------------------------
C
C     Data declarations

      INTEGER IABSNT
      INTEGER ISTAT, IOST20
      INTEGER ICEIL, IDRYTMP, ISUMTT, ISUMOP
      REAL    ABSENT, DEWPT, WDIR, WSP, DRYTMP

      CHARACTER*80 CARD
      CHARACTER*1 OVRPCH(35),OVR11(10),OVR12(10),OVRNOR(10)
      CHARACTER*2 AWDIR,ATOT,AOPQ
      CHARACTER*3 AWSP,ADRYTMP,ACLG
C
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'
C
      DATA OVR11/'[','A','B','C','D','E','F','G','H','I'/,
     &     OVR12/']','J','K','L','M','N','O','P','Q','R'/,
     &     OVRNOR/'0','1','2','3','4','5','6','7','8','9'/
      DATA IABSNT/-9999/, ABSENT/-9999.0/
      DATA PATH/'SF'/, LOC/'D028LV'/
C
C
C     Read SCRAM record
      READ(CARD,1001,ERR=20001,IOSTAT=IOST20) ACLG,
     &                         AWDIR, AWSP, ADRYTMP, ATOT, AOPQ
1001  FORMAT (13X,A3,A2,2A3,2A2)

C     Process CLG, TOT, and OPQ as overpunch fields
C     CLG=OVRPCH(I),I=1,3
C     TOT=OVRPCH(4)
C     OPQ=OVRPCH(5)

C     Translate SCRAM representation of CLG,TOT,OPQ to CD144 representation
C     Fill missing OPQ following procedures used in MET144 program.

C      Ceiling:
       OVRPCH(1) = ACLG(1:1)
       OVRPCH(2) = ACLG(2:2)
       OVRPCH(3) = ACLG(3:3)

C      Opaque cover:
       IF(AOPQ .EQ. '  ') THEN
          IF(ATOT .NE. '  ') THEN
             AOPQ = ATOT
          ELSEIF(ACLG .EQ. '   ') THEN
             AOPQ = '  '
          ELSEIF(ACLG .EQ. '---') THEN
             AOPQ = '00'
          ELSEIF(ACLG .LT. '070') THEN
             AOPQ = '07'
          ELSE
             AOPQ = '00'
          ENDIF
       ELSEIF(AOPQ .EQ. '10') THEN
          AOPQ='--'
       ENDIF
       OVRPCH(5) = AOPQ(2:2)

C      Total cover:
       IF(ATOT .EQ. '10') ATOT='--'
       OVRPCH(4) = ATOT(2:2)
C
C     Process each overpunch, decoding the meaning and making the
C     appropriate transformation; add 10 to the overpunch value to
C     more easily identify them
C     (The flag for X or - is -8888 (and is interpreted later))
C
      DO 1380 I=1,5
         SFOVR(I)=-99999
         IF(OVRPCH(I).EQ.'X'.OR.OVRPCH(I).EQ.'-') THEN
            SFOVR(I)=-8888
         ELSE IF(OVRPCH(I).EQ.' ') THEN
            SFOVR(I)=IABSNT
         ELSE
            DO 1385 J=0,9
              IF(OVRPCH(I).EQ.OVRNOR(J+1)) THEN
                 SFOVR(I)=J
                 GO TO 1386
              ELSE IF(OVRPCH(I).EQ.OVR11(J+1)) THEN
                 SFOVR(I)=J+10
                 GO TO 1386
              ELSE IF(OVRPCH(I).EQ.OVR12(J+1)) THEN
                 SFOVR(I)=J+10
                 GO TO 1386
              ENDIF
 1385       CONTINUE
 1386       CONTINUE
         ENDIF
C
C        Check to make sure a proper ovrpch character was found
C
         IF(SFOVR(I).EQ.-99999) THEN
            MESS = BLNK40
            WRITE(MESS,1390) I,SFGYR,SFGMO,SFGDY,SFGHR
 1390       FORMAT('OVRPNCH ',I2,' IMPROPERLY DECODED: ',3i2.2,'/',I2)
            CALL ERROR(0,PATH,'W43',LOC,MESS)
         ENDIF
 1380 CONTINUE
C
C     Convert selected "blank" overpunches to "zero" -- we assume that
C     such blanks are intentional, since missing values will be caught
C     by checking for blanks in the data fields.
C
C     - CEILING HEIGHT:
       ITEST=SFOVR(1)+SFOVR(2)+SFOVR(3)
       IF(ITEST .NE. (3*IABSNT)) THEN
          DO 1381 I=1,3
1381      IF(SFOVR(I) .EQ. IABSNT) SFOVR(I)=0
       ENDIF

C-----------------------------------------------------------------------
C     Process the variables in the order of the SFOBS array
C
C     Altimeter, sea level, and station pressure: missing (not available)
       SFOBS(SFGHR,30) = SFQA(30,2)
       SFOBS(SFGHR,31) = SFQA(31,2)
       SFOBS(SFGHR,32) = SFQA(32,2)

C     Ceiling height (km & tenths) - ( 300 (30.0 km) == unlimited)
       CALL CLHT(1,ICEIL,IABSNT,SFQA(33,2))
       SFOBS(SFGHR,33) = ICEIL
C
C     Concatenate total and opaque sky cover (tenths):
       CALL CVG(4,ISUMTT,IABSNT,SFQA1(34,1))
       CALL CVG(5,ISUMOP,IABSNT,SFQA1(34,2))
       SFOBS(SFGHR,34) = ISUMTT*100 + ISUMOP
C
C     Next block of concatenated variables are missing (not available)

       DO 1420 J = 35, 39                                               DTBMAY93
          SFOBS(SFGHR,J) = SFQA1(J,1)*100 + SFQA1(J,2)                  DTBMAY93
 1420  CONTINUE

       DO 1421 J = 40, 43                                               DTBMAY93
          SFOBS(SFGHR,J) = SFQA1(J,1)*1000 + SFQA1(J,2)                 DTBMAY93
 1421  CONTINUE
C
C     Present weather missing (not available)
       SFOBS(SFGHR,44) = SFQA(44,2)                                     JOPAPR95
C
C     Horizontal visibility missing (not available)
       SFOBS(SFGHR,45) = SFQA(45,2)
C
C     Dry bulb temperature (deg c and tenths)
       IF(ADRYTMP .EQ. '   ') THEN
         SFOBS(SFGHR,46) = SFQA(46,2)

       ELSE
          READ(ADRYTMP,1002) IDRYTMP
1002      FORMAT(I3)
          DRYTMP = FLOAT(IDRYTMP)
          CALL P2MCEN(DRYTMP,XRD2)
          SFOBS(SFGHR,46) = NINT(XRD2 *10.)
       ENDIF
C
C     Wet bulb, dew point, and rel humidity missing (not available)
       SFOBS(SFGHR,47) = SFQA(47,2)
       SFOBS(SFGHR,48) = SFQA(48,2)
       SFOBS(SFGHR,49) = SFQA(49,2)
C
C     Wind direction (tens of degrees from north)
       IF(AWDIR .EQ. '  ') THEN
          SFOBS(SFGHR,50) = SFQA(50,2)
       ELSE
          READ(AWDIR,1003) WDIR
1003      FORMAT(F2.0)                                                  DTBMAY93
          SFOBS(SFGHR,50) = WDIR
       ENDIF
C
C     Wind speed (meters/sec and tenths)
       IF(AWSP .EQ. '   ') THEN
          SFOBS(SFGHR,51) = SFQA(51,2)
       ELSE
          READ(AWSP,1004) WSP                                           DTBMAY93
1004      FORMAT( F3.0 )                                                DTBMAY93
          CALL P2MMSC(WSP,XRD2)
          SFOBS(SFGHR,51) = NINT(XRD2 *10.)
       ENDIF
C
       RETURN
C-----------------------------------------------------------------------
C- PROCESSING CONTINUES HERE IF THERE IS AN ERROR DECODING THE STRING
C
20001 CONTINUE
      MESS = BLNK40
      WRITE(MESS,607) NRECNO
      CALL ERROR(KOUNT,PATH,'E42',LOC,MESS)
      MESS = BLNK40
      WRITE(MESS,608) SFGYR,SFGMO,SFGDY,SFGHR
      CALL ERROR(KOUNT,'  ','+++','      ',MESS)

      ISTAT = 1
      RETURN

  607 FORMAT(' ERROR DECODING DATA, REC #',I5)
  608 FORMAT('   ON (YR/MO/DA/HR)',I2,'/',I2,'/',I2,'/',I2)

      END



      SUBROUTINE SFLIB
C=====================================================================**
C  SUBROUTINE SFLIB
C
C  PURPOSE
C     THIS SUBROUTINE CONTAINS SEVERAL ENTRY POINTS TO ASSIST IN
C      DECODING TD-1440 DATA
C
C     ENTRY SFCWXX  = DECODES AND TRANSFORMS THE PRESENT WEATHER CODES
C     ENTRY CLHT    = COMPUTES CEILING HEIGHT
C     ENTRY CVG     = COMPUTES SKY COVER IN TENTHS
C     ENTRY CLOUDS  = COMPUTES THE CLOUD TYPE
C
C  VERSION DATE: 30 SEPT 1992
C
C=======================================================================
C
C     Data declarations

      INTEGER P1, P2, P3, IABSNT, ICHT, ICVG, ICLTYP
      INTEGER ISVRE, ILIQPC, IFRZPC, IOBSVS, IWETH, ICL, MISS
      CHARACTER*8 XIWETH
      CHARACTER*1 XWETH(8)
      EQUIVALENCE (XIWETH,XWETH(1))
C
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
C
C=======================================================================
C  ENTRY SFCWXX                                      AUGUST  8, 1987
C
C    THIS  ROUTINE CONVERTS 8-DIGIT WEATHER OBSERVATIONS (COLUMNS
C     24-31) INTO TD3280 PRESENT WEATHER CODES AS BEST AS POSSIBLE
C
C*****************  VARIABLE DESCRIPTIONS  ***********************
C  IFRZPC   = FROZEN PRECIP CODE (WMO PRESENT WEATHER CODE)
C  ILIQPC   = LIQUID PRECIP CODE (WMO PRESENT WEAHTER CODE)
C  IWETH    = 8 DIGIT INTEGER CODE TO BE TRANSLATED
C  XIWETH   = COLUMNS 24-32 OF NOAA WEATHER CODE(IN CHAR FORM)
C  XWETH    = CONATINS A SINGLE COLUMN OF NOAA CODE(IN CHAR FORM)
C
C-----------------------------------------------------------------------
      ENTRY SFCWXX(IWETH,ISVRE,ILIQPC,IFRZPC,IOBSVS)
C
      ISVRE=0
      ILIQPC=0
      IFRZPC=0
      IOBSVS=0
      WRITE(XIWETH,1) IWETH
    1 FORMAT(I8.8)
C
C    CLASSIFY SEVERE WEATHER
C
      IF(XWETH(1).EQ.'1') ISVRE=10
      IF(XWETH(1).EQ.'2') ISVRE=11
      IF(XWETH(1).EQ.'3') ISVRE=12
      IF(XWETH(1).EQ.'5') ISVRE=13
C
C    CLASSIFY LIQUID PRECIPITATION
C     Liquid precipitation: backward search
C     drizzle
      IF(XWETH(3).EQ.'4') ILIQPC=33
      IF(XWETH(3).EQ.'5') ILIQPC=34
      IF(XWETH(3).EQ.'6') ILIQPC=35
      IF(XWETH(3).EQ.'7') ILIQPC=36
      IF(XWETH(3).EQ.'8') ILIQPC=37
      IF(XWETH(3).EQ.'9') ILIQPC=38
C     rain
      IF(XWETH(2).EQ.'1') ILIQPC=20
      IF(XWETH(2).EQ.'2') ILIQPC=21
      IF(XWETH(2).EQ.'3') ILIQPC=22
      IF(XWETH(2).EQ.'4') ILIQPC=23
      IF(XWETH(2).EQ.'5') ILIQPC=24
      IF(XWETH(2).EQ.'6') ILIQPC=25
      IF(XWETH(2).EQ.'7') ILIQPC=26
      IF(XWETH(2).EQ.'8') ILIQPC=27
      IF(XWETH(2).EQ.'9') ILIQPC=28
C
C     Frozen precipitation:
C     ice forms
      IF(XWETH(6).EQ.'1') IFRZPC=90
      IF(XWETH(6).EQ.'2') IFRZPC=91
      IF(XWETH(6).EQ.'3') IFRZPC=92
      IF(XWETH(6).EQ.'5') IFRZPC=64
      IF(XWETH(6).EQ.'8') IFRZPC=66

C     snow showers
      IF(XWETH(5).EQ.'1') IFRZPC=50
      IF(XWETH(5).EQ.'2') IFRZPC=51
      IF(XWETH(5).EQ.'3') IFRZPC=52
      IF(XWETH(5).EQ.'7') IFRZPC=56
      IF(XWETH(5).EQ.'8') IFRZPC=57
      IF(XWETH(5).EQ.'9') IFRZPC=58
      IF(XWETH(4).EQ.'1') IFRZPC=40
      IF(XWETH(4).EQ.'2') IFRZPC=41
      IF(XWETH(4).EQ.'3') IFRZPC=42
      IF(XWETH(4).EQ.'4') IFRZPC=43
      IF(XWETH(4).EQ.'5') IFRZPC=44
      IF(XWETH(4).EQ.'6') IFRZPC=45
      IF(XWETH(4).EQ.'8') IFRZPC=47
C
C    TRANSLATE OBSTRUCTION TO VISION CODE
C
      IF(XWETH(8).EQ.'1') IOBSVS=80
      IF(XWETH(8).EQ.'2') IOBSVS=81
      IF(XWETH(8).EQ.'3') IOBSVS=82
      IF(XWETH(8).EQ.'4') IOBSVS=83
      IF(XWETH(8).EQ.'5') IOBSVS=84
      IF(XWETH(8).EQ.'6') IOBSVS=85
      IF(XWETH(7).EQ.'1') IOBSVS=70
      IF(XWETH(7).EQ.'2') IOBSVS=71
      IF(XWETH(7).EQ.'3') IOBSVS=72
      IF(XWETH(7).EQ.'4') IOBSVS=73
      IF(XWETH(7).EQ.'5') IOBSVS=74
      RETURN
C
C=======================================================================
C  ENTRY CLHT                                         28 DECEMBER 1987
C
C    THIS ROUTINE COMPUTES THE CEILING HEIGHT IN KILOMETERS
C    A VALUE OF 990 = 990000 FEET = 300 KM = UNLIMITED CEIL. HT.)
C
C-----------------------------------------------------------------------
      ENTRY CLHT(P1,ICHT,IABSNT,MISS)
C
        ICHT = MISS
C
C     INTERPRET ONLY EXPECTED OVERPUNCHES
C
        IF(SFOVR(P1).EQ.-8888) THEN
          ICHT = 300
        ELSE IF(SFOVR(P1) .GE. 0 .AND. SFOVR(P1) .LE. 9 .AND.
     1          SFOVR(P1+1) .GE. 0 .AND. SFOVR(P1+1) .LE. 9 .AND.
     2          SFOVR(P1+2) .GE. 0 .AND. SFOVR(P1+2) .LE. 9) THEN
          ICL = SFOVR(P1)*100 + SFOVR(P1+1)*10 + SFOVR(P1+2)
          IF(ICL .EQ. 888) THEN
            ICHT = MISS
          ELSE
            ICL = ICL * 100
            CALL P2MMTR(ICL,ICHT)
            ICHT = (((ICHT/1000.0) *10.) + 0.5)
          ENDIF
        ENDIF
        RETURN
C=======================================================================
C  ENTRY CVG                                      28 DECEMBER 1987
C
C    THIS ROUTINE COMPUTES THE SKY COVERAGE IN TENTHS
C
C-----------------------------------------------------------------------
      ENTRY CVG(P2,ICVG,IABSNT,MISS)
C
        IF(SFOVR(P2) .EQ. IABSNT .OR.
     1     SFOVR(P2) .EQ. -99999 .OR.
     2     SFOVR(P2) .GE. 10)     THEN
         ICVG = MISS
        ELSE IF (SFOVR(P2) .EQ. -8888) THEN
         ICVG = 10
        ELSE
         ICVG= SFOVR(P2)
        ENDIF
        RETURN
C=======================================================================
C- THIS ROUTINE COMPUTES THE CLOUD TYPE;
C-  CLOUD TYPE IS MAPPED TO THE TD-3280 CODES
C
      ENTRY CLOUDS(P3,ICLTYP,IABSNT,MISS)
C
        IF(SFOVR(P3) .EQ. IABSNT) THEN
         ICLTYP = MISS
        ELSE IF (SFOVR(P3) .EQ. -8888) THEN
         ICLTYP = 98
        ELSE
         ICLTYP= SFOVR(P3)
         IF( ICLTYP .EQ. 0) THEN
          CONTINUE
         ELSE IF(ICLTYP .EQ. 1) THEN
          ICLTYP = 45
         ELSE IF(ICLTYP .EQ. 2) THEN
          ICLTYP = 16
         ELSE IF(ICLTYP .EQ. 3) THEN
          ICLTYP = 15
         ELSE IF(ICLTYP .EQ. 4) THEN
          ICLTYP = 11
         ELSE IF(ICLTYP .EQ. 5) THEN
          ICLTYP = 18
         ELSE IF(ICLTYP .EQ. 6) THEN
          ICLTYP = 21
         ELSE IF(ICLTYP .EQ. 7) THEN
          ICLTYP = 23
         ELSE IF(ICLTYP .EQ. 8) THEN
          ICLTYP = 32
         ELSE IF(ICLTYP .EQ. 9) THEN
          ICLTYP = 37
         ELSE IF(ICLTYP .EQ. 12) THEN
          ICLTYP = 13
         ELSE IF(ICLTYP .EQ. 14) THEN
          ICLTYP = 17
         ELSE IF(ICLTYP .EQ. 15) THEN
          ICLTYP = 19
         ELSE IF(ICLTYP .EQ. 16) THEN
          ICLTYP = 22
         ELSE IF(ICLTYP .EQ. 17) THEN
          ICLTYP = 28
         ELSE IF(ICLTYP .EQ. 19) THEN
          ICLTYP = 39
         ELSE
          ICLTYP = MISS
         ENDIF
        ENDIF
        RETURN
C
        END


      SUBROUTINE RDSAMS( SFBFST, NVARS, KOUNT )
C***********************************************************************
C*       RDSAMS Module of MPRM Meteorological Pre-processor
C*
C*    PURPOSE:    Fills in the Appropriate Met Variable Arrays
C*                with Raw Data from SAMSON
C*
C*    PROGRAMMER: PES Inc.
C*
C*    DATE:       April 28, 1995
C*     
C*    INPUTS:     Surface Data from SAMSON in character format
C*
C*    OUTPUTS:    24 Hours of Met Parameters
C*        
C*    CALLED FROM: SFEXT
C***********************************************************************
C*
C*    Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'

      INTEGER     IMULT(8), LDAYC, IOST
      INTEGER     SFBFST, NVARS
      CHARACTER*9 ASAM(MAXVAR)
      CHARACTER*6 WFMT(MAXVAR)

      INCLUDE 'WORK1.INC'
      
C     Data initialization
      DATA LOC/'RDSAMS'/ , PATH/'SF'/

C     Data formats:
C     The first five elements are for the date, time and obs. flag
C     The second five are for radiation measurements
C     The next are for weather variables:
C          11 = Total sky cover
C          12 = Opaque sky cover
C          13 = Dry bulb temperature
C          14 = Dew point temperature
C          15 = Relative humidity
C          16 = Station pressure
C          17 = Wind direction
C          18 = Wind speed
C          19 = Visibility
C          20 = Ceiling height
C          21 = Present weather
C          22 = Precipitable water
C          23 = Optical depth
C          24 = Snow depth
C          25 = Days since last snowfall
C          26(1-6) = Precipitation
C          27(7)   = Precipitation flag
C
      DATA WFMT /'(I3)','(I3)','(I3)','(I3)','(I1)',
     &           '(A4)','(A4)','(A7)','(A7)','(A7)',
     &           '(F2.0)','(F2.0)','(F5.0)','(F5.0)','(F3.0)',
     &           '(F4.0)','(F3.0)','(F5.0)','(F6.0)','(F6.0)',
     &           '(A9)','(F4.0)','(F6.0)','(F4.0)','(F3.0)','(F6.0)'/

      DATA IMULT /10,10, 1,10, 1,10,10,10/

C     Note: The station ID appears only once - in a header, so it cannot
C           be checked here as is done in the CD144 retrieval process.
C           The station is checked when the header is read.

  300 NRECS = NRECS + 1

C     Read one record from the SAMSON data file
      READ( DEV20, SAMFMT, END=310 , ERR=320 ) (ASAM(IV),IV=1,NVARS+5)

      IF( ASAM(1)(1:1) .EQ. '~' )THEN
C        There is more than one year of data in this file; stop
C        processing and let the user know.
         MESS = BLNK40
         WRITE(MESS, 3900)
 3900    FORMAT(' SECOND SET OF SAMSON HEADERS IN FILE')
         CALL ERROR( KOUNT,PATH,'W46',LOC,MESS )
         SFBFST = -1
         RETURN
      ENDIF

C     Decode the date and time information from the record
      READ (ASAM(1),WFMT(1)) SFGYR
      READ (ASAM(2),WFMT(2)) SFGMO
      READ (ASAM(3),WFMT(3)) SFGDY
      READ (ASAM(4),WFMT(4)) SFGHR

      JDAY = JULIAN(SFGYR,SFGMO,SFGDY)
      CALL CHROND( PATH,SFGYR,JDAY,SFDAYC )

      IF( SFDAYC .LT. SFDAY1 )THEN
C        The date is prior to the extract window
         GO TO 300

      ELSEIF( SFDAYC .GE. SFDAY1  .AND.  SFDAYC .LE. SFDAY2 )THEN
C        Data are in the extract window
         LDAYC  = SFDAYC
         DO WHILE( LDAYC  .EQ. SFDAYC  .AND.
     &             SFSTAT .GT. 0       .AND.
     &             SFBFST .NE. -1 )

C           Initialize the cloud cover variables
            ITOT = 99
            IOPQ = 99

C           Some data may be modeled rather than observed; if so, write
C           a message to the user
            IF( ASAM(5) .EQ. '9' )THEN
C              Data are modeled, not observed; warn the user
               MESS = BLNK40
               WRITE(MESS, 4000) SFGYR,SFGMO,SFGYR,SFGHR
 4000          FORMAT(' DATA ARE *MODELED* ON',4(1X,I2) )
               CALL ERROR( KOUNT,PATH,'W47',LOC,MESS )
            ENDIF


C*          Loop Over the SAMSON Variables
            DO 500 IV = 1,NVARS

C*             Process the variables, skipping variables 1-5 and
C              17-20; SAMSON and old MPRM fortunately use the metric
C              system for all variables, so no SAMSON variables
C              have to have units converted other than precipitation

               READ( ASAM(IV+5), WFMT(IDVAR(IV)+5), ERR= 330) VTEMP
               IF (IDVAR(IV) .GE. 6  .AND. IDVAR(IV) .LE. 15 )THEN
                  IF( IDVAR(IV) .EQ. 6 )THEN
C                    Total sky cover: 99 = missing, same as MPRM default
                     ITOT = INT(VTEMP)

                  ELSEIF( IDVAR(IV) .EQ. 7 )THEN
C                    Opaque sky cover: 99 = missing, same as MPRM default
                     IOPQ = INT(VTEMP)

                  ELSEIF( IDVAR(IV) .EQ. 8 )THEN
                     IVNDX = IDVAR(IV) - 7
C                    Dry bulb temperature: 9999=missing in SAMSON;
C                                           999=missing in MPRM default
C                    The conversion from Celsius to Fahrenheit then
C                    back to Celsius is to maintain some consistency
C                    with PCRAMMET
                     IF( VTEMP .LT. 9000.0 )THEN
                        TFAHR = NINT( VTEMP/0.5556 ) + 32.0
                        VTEMP = 0.5556 * (TFAHR - 32.0)
                        SFOBS(SFGHR,46) = NINT((VTEMP)*IMULT(IVNDX))
                     ELSE
                        SFOBS(SFGHR,46) = SFQA(46,2)
                     ENDIF

                  ELSEIF( IDVAR(IV) .EQ. 9 )THEN
                     IVNDX = IDVAR(IV) - 7
C                    Dew point temperature: 9999=missing in SAMSON;
C                                            999=missing in MPRM default
                     IF( VTEMP .LT. 9000.0 )THEN
                        DEWF = NINT( VTEMP/0.5556 ) + 32.0
                        VTEMP = 0.5556 * (DEWF - 32.0)
                        SFOBS(SFGHR,48) = NINT((VTEMP)*IMULT(IVNDX))
                     ELSE
                        SFOBS(SFGHR,48) = SFQA(48,2)
                     ENDIF

                  ELSEIF( IDVAR(IV) .EQ. 10 )THEN
                     IVNDX = IDVAR(IV) - 7
C                    Relative humidity: 999=missing in SAMSON;
C                                       999=missing in MPRM default
                     IF( VTEMP .LT. 900.0 )THEN
                        SFOBS(SFGHR,49) = NINT((VTEMP)*IMULT(IVNDX))
                     ELSE
                        SFOBS(SFGHR,49) = SFQA(49,2)
                     ENDIF

                  ELSEIF( IDVAR(IV) .EQ. 11 )THEN
                     IVNDX = IDVAR(IV) - 7
C                    Station pressure: 9999=missing in SAMSON;
C                                     99999=missing in MPRM default
                     IF( VTEMP .LT. 9000.0 )THEN
                        SFOBS(SFGHR,32) = NINT((VTEMP)*IMULT(IVNDX))
                     ELSE
                        SFOBS(SFGHR,32) = SFQA(32,2)
                     ENDIF

                  ELSEIF( IDVAR(IV) .EQ. 12 )THEN
                     IVNDX = IDVAR(IV) - 7
C                    Wind direction: 999=missing in SAMSON;
C                                     99=missing in MPRM default
                     IF( VTEMP .LE. 360.0 )THEN
                        SFOBS(SFGHR,50) =
     &                              NINT((VTEMP/10.0)*IMULT(IVNDX))
                     ELSE
                        SFOBS(SFGHR,50) = SFQA(50,2)
                     ENDIF

                  ELSEIF( IDVAR(IV) .EQ. 13 )THEN
                     IVNDX = IDVAR(IV) - 7
C                    Wind speed: 9999 or 99=missing in SAMSON;
C                                     -9999=missing in MPRM default
C                    The conversion from m/s to knots then back to
C                    m/s is to maintain some consistency with PCRAMMET
                     IF( VTEMP .LT. 100.0 )THEN
                        KSPEED = NINT( VTEMP/0.51444 )
                        VTEMP  = KSPEED * 0.51444
                        SFOBS(SFGHR,51) = NINT((VTEMP)*IMULT(IVNDX))
                     ELSE
                        SFOBS(SFGHR,51) = SFQA(51,2)
                     ENDIF

                  ELSEIF( IDVAR(IV) .EQ. 14 )THEN
                     IVNDX = IDVAR(IV) - 7
C                    Visibility: 99999=missing in SAMSON;
C                                99999=missing in MPRM default
                     IF( VTEMP .LT. 99990.0 )THEN
                        SFOBS(SFGHR,45) = NINT((VTEMP)*IMULT(IVNDX))
                     ELSE
                        SFOBS(SFGHR,45) = SFQA(45,2)
                     ENDIF

                  ELSEIF( IDVAR(IV) .EQ. 15 )THEN
                     IVNDX = IDVAR(IV) - 7
C                    Ceiling height: 999999=missing in SAMSON;
C                                       999=missing in MPRM
                     IF( VTEMP .GT. 77776.0  .AND.
     &                              VTEMP .LT. 77778.0 )THEN
                        SFOBS(SFGHR,33) = 300
                     ELSEIF( VTEMP .LT. 99990.0 )THEN
                        SFOBS(SFGHR,33) =
     &                             NINT((VTEMP/1000.0)*IMULT(IVNDX))
                     ELSE
                        SFOBS(SFGHR,33) = SFQA(33,2)
                     ENDIF
                  ENDIF

               ELSEIF( IDVAR(IV) .EQ. 16 )THEN
                  CALL SAMWX (ASAM(IV+5), ILIQ, IFRZ)
                  SFOBS(SFGHR,44) = ILIQ*100 + IFRZ

               ELSEIF( IDVAR(IV) .EQ. 21 )THEN
C                 Precipitation: inches and hundredths converted to
C                 millimeters; integerized by multiplying by 1000
C                 factor of 10 = 1000 (integerize) / 100 (hundredths)
                  SFOBS(SFGHR,52) = (VTEMP * 25.4)*10
               ENDIF

C*          End Loops Over Variables
500         CONTINUE

C           Concatenate the sky covers
            SFOBS(SFGHR,34) = ITOT*100 + IOPQ

C           Increment the Total Record Counter (NRECS) and the Extracted
C           Record Counter (KOUNT) and read the next record
            KOUNT = KOUNT + 1
            NRECS = NRECS + 1
            READ( DEV20, SAMFMT, END=310, ERR=320 )
     &                                   (ASAM(IV),IV=1,NVARS+5)

            IF( ASAM(1)(1:1) .EQ. '~' )THEN
C              There is more than one year of data in this file;
C              stop processing and let the user know.
               MESS = BLNK40
               WRITE(MESS, 3900)
               CALL ERROR( KOUNT,PATH,'W46',LOC,MESS )
               SFBFST = -1
               RETURN
            ENDIF

C           Decode the date and time information from the record
            READ (ASAM(1),WFMT(1)) SFGYR
            READ (ASAM(2),WFMT(2)) SFGMO
            READ (ASAM(3),WFMT(3)) SFGDY
            READ (ASAM(4),WFMT(4)) SFGHR

            JDAY = JULIAN(SFGYR,SFGMO,SFGDY)
            CALL CHROND( PATH,SFGYR,JDAY,SFDAYC )
            IF( SFDAYC .NE. LDAYC )THEN
C              Convert the previous day's (LDAYC) chronological day
C              back to the year, month and day; set the status and
C              backspace one record; note that NRECS was incremented
C              above when the next record was read, so decrement it.

               CALL ICHRND( PATH, LDAYC, SFGYR, JDAY)
               CALL GREG( SFGYR, JDAY, SFGMO, SFGDY )
               SFBFST = 2
               BACKSPACE(UNIT=DEV20 ,IOSTAT=IOST )
               NRECS = NRECS - 1
               IF( IOST .NE. 0 )THEN
                  MESS = BLNK40
                  WRITE(MESS, 4100) NRECS
                  CALL ERROR( KOUNT,PATH,'E46',LOC,MESS )
 4100             FORMAT(' BACKSPACE ERROR ON SAMSON, REC # ',I4 )
                  SFSTAT = -1
                  SFBFST = -1
               ENDIF
C     stop
            ENDIF

         ENDDO

C        The day has changed; reset SFDAYC so SFEXT can
C        properly manipulate (date align) the data;
         SFDAYC = LDAYC

      ELSEIF( SFDAYC .GT. SFDAY2 )THEN
C        The data are after the extraction window
C        *** EXTRACTION PROCESS COMPLETE ***
         MESS = BLNK40
         WRITE( MESS, 601 ) NRECS-1
         CALL ERROR( KOUNT, PATH, 'I49',LOC,MESS )
         SFBFST = -1

      ENDIF
      RETURN

C     Processing continues here if an end of file was encountered
C     while reading from the file

  310 SFBFST = -1
      MESS = BLNK40
      WRITE(MESS,600) NRECS-1
      CALL ERROR(KOUNT,PATH,'I49',LOC,MESS)
      IF(KOUNT .LT. 1) THEN
         MESS = BLNK40
         WRITE(MESS,701)
         CALL ERROR(KOUNT,PATH,'W48',LOC,MESS)
      ENDIF
      RETURN

C     Processing continues here if an error was encountered reading the
C     file.  SFSTAT is set to -1 to prevent further processing if the
C     maximum number of errors is reached.

  320 MESS = BLNK40
      WRITE(MESS,603) NRECS
      CALL ERROR(KOUNT,PATH,'E42',LOC,MESS)
      SFSTAT = -1
      RETURN

C     Processing continues here if an error was encountered decoding
C     one of the fields.  SFSTAT is set to -1 to prevent further
C     processing

  330 MESS = BLNK40
      WRITE(MESS,607) NRECS
      CALL ERROR(KOUNT,PATH,'E42',LOC,MESS)
      SFSTAT = -1
      RETURN
C----
  600 FORMAT(' END-OF-FILE AFTER RECORD', I5)
  601 FORMAT(' END-OF DATA WINDOW AFTER RECORD', I5)
  603 FORMAT(' ERROR READING SAMSON, REC #',I5)
  607 FORMAT(' ERROR DECODING SAMSON, REC #',I5)
  701 FORMAT(' NO OBS EXTRACTED-CHECK EXTRACT DATES')

      END



      SUBROUTINE SAMWX( ATEMP, ILIQ, IFRZN )
C***********************************************************************
C*       SAMWX  Module of MPRM Meteorological Pre-processor
C*
C*    PURPOSE:    Interprets the present weather character varaible and
C*                returns with codes for liquid and frozen precipitation
C*
C*    PROGRAMMER: Jim Paumier
C*                PES Inc.
C*
C*    DATE:       April 28, 1995
C*     
C*    INPUTS:     9-character present weather code for the hour
C*
C*    OUTPUTS:    Liquid and frozen precipitation codes
C*        
C*    CALLED FROM: RDSAMS
C***********************************************************************
C*
C*    Variable Declarations
      CHARACTER*9  ATEMP
      CHARACTER*1  IP
      INTEGER      ILIQ, IFRZN

      ILIQ = 0
      IFRZN = 0
      DO 5 ICODE = 6,2,-1
         IP = ATEMP(ICODE:ICODE)
         IF( IP .EQ. '9' )THEN
            GO TO 5
         ELSE
            READ( IP,100 ) NUMIP
            IPCODE = ICODE*10 + NUMIP
         ENDIF

         IF( IPCODE .Le. 39 )THEN
            ILIQ = IPCODE
         ELSE
            IFRZN = IPCODE
         ENDIF
   5  CONTINUE

  100 FORMAT(I1)

      RETURN
      END


      SUBROUTINE SFQASM
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C
C  PURPOSE
C     THIS ROUTINE ASSESSES THE QUALITY OF THE NWS SURFACE DATA
C     BY PERFORMING UPPER AND LOWER BOUND CHECKS ON ALL THE
C     VARIABLES
C
C  CALLED BY: SFPATH
C
C  VERSION DATE:  30 SEPT 1992
C
C  Revisions:
C    1/17/96  Pacific Environmental Services, Inc.
C             Corrected the code to properly report "precipitation
C             without weather" errors
C=======================================================================
C

      INTEGER  SF2YR, SF4YR, CENTURY
      INTEGER JULIAN,READHO,IPART1,IPART2,CONCAT(23),IVBL,MISS1,NUM,
     1        IQA1L,IQA1U,MISS2,IQA2U,IQA2L,QARSLT,SFCALM,SFWSWD,
     2        TDGTT,PPTERR
C     REAL
C
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'
C
      DATA CONCAT/4*1,6*100,4*1000,100,8*1/
      DATA PATH/'SF'/,LOC/'SFQASM'/
C
C-----------------------------------------------------------------------
C *** VARIABLE DESCRIPTIONS
C
C      READHO        = OBSERVATION # COUNTER
C      CONCAT        = ARRAY OF MULTIPLIERS USED TO SEPARATE
C                      CONCATENATED VARIABLES
C      NUM           = PATHWAY NUMBER, HEADER STATUS
C
C     THE FOLLOWING VARIABLES ARE DEFINED FOR THE CONCATENATED VARIABLES
C        --1 = LEADING VARIABLE; --2 = TRAILING VARIABLE
C      MISS1,MISS2   = MISSING VALUE INDICATORS
C      IQA1L,IQA2L   = LOWER QA BOUNDS
C      IQA1U,IQA2U   = UPPER QA BOUNDS
C      IPART1,IPART2 = OBSERVATIONS 'UN'CONCATENATED
C
C=======================================================================
C *** INITIALIZE COUNTERS
C
      READHO = 0
      NUM = 3
C
      SFCALM = 0
      SFWSWD = 0
      TDGTT  = 0
      PPTERR = 0
C
C *** READ THE FILE HEADERS; IF THERE IS A PROBLEM WRITE A MESSAGE
C      AND DO NOT QA THE DATA
C
      CALL OTHHDR(PATH,LOC,NUM,DEV21,DEV22,DEV50)
      IF(NUM .LT. 0) THEN
         MESS = BLNK40
         WRITE(MESS,135)
         CALL ERROR(0,PATH,'E48',LOC,MESS)
         SFSTAT = -1
         RETURN
      ENDIF
      WRITE(DEV22,2200) IVDATE                                          DTBAUG94
C
      WRITE(*,609)
  609 FORMAT( ' ' )
C
C-----------------------------------------------------------------------
C ***   READ THE FIRST RECORD OF AN OBSERVATION INTO A CHARACTER STRING
C
   20   READHO = READHO + 1
C
        READ(DEV21,300,END=410,ERR=400) (BUF01(K),K=1,80)
C
C ***   Decode the data after initializing the arrays
        SFGYR = SFQA(58,2)
        SFGMO = SFQA(57,2)
        SFGDY = SFQA(55,2)
        SFGHR = SFQA(59,2)
        CALL FLSFC(1)

        READ(BUF80(1),301,ERR=420) SFGYR,SFGMO,SFGDY,SFGHR,
     1                            (SFOBS(1,IVBL),IVBL=30,39)

        CALL Y2K(PATH, SFGYR, SF2YR, SF4YR, CENTURY)

        WRITE(*, 610 ) SFGMO, SFGDY, SF4YR
  610   FORMAT('+  Stage 1: QA''ing surface data for ',
     &             'month-day-year ', 2(I2.2,:'-'),I4)

C ***   Read the second record
        READ(DEV21,307,ERR=430,END=440)(SFOBS(1,IVBL),IVBL=40,52)
C
C ***   Write the data to the output file without modification
        WRITE(DEV22,301)  SFGYR,SFGMO,SFGDY,SFGHR,
     1                   (SFOBS(1,IVBL),IVBL=30,39)
        WRITE(DEV22,307) (SFOBS(1,IVBL),IVBL=40,52)
C
C ***   BEGIN PERFORMING THE QA
        IWORK1(1210) = JULIAN(SFGYR,SFGMO,SFGDY)*100 + SFGHR
C
   30   DO 40 I = 30,52
C
C ***      VARIABLE NUMBERS 34 THROUGH 44 ARE A CONCATENATION OF TWO
C          VARIABLES; THE DATA, MISSING VALUE INDICATORS AND BOUNDS
C          MUST BE SEPARATED BEFORE QA'ING
C
           IF(I.GE.34 .AND. I.LE.44)THEN
C
C ***         PART 1
C
               MISS1  = SFQA(I,2)/CONCAT(I-29)
               IQA1L  = SFQA(I,3)/CONCAT(I-29)
               IQA1U  = SFQA(I,4)/CONCAT(I-29)
               IPART1 = SFOBS(1,I)/CONCAT(I-29)
C
               QARSLT = 5
               CALL INTECK(3,IWORK1(1210),SFQA(I,1),MISS1,IQA1L,
     1                     IQA1U,IPART1,VNAMES(I),QARSLT)
               IF((QARSLT .EQ. 2) .OR. (QARSLT .EQ. 3)) THEN
                MESS = BLNK40
                WRITE(MESS,350) SFGYR,SFGMO,SFGDY,SFGHR
                CALL ERROR(IWORK1(1210),PATH,'   ','      ',MESS)
               ENDIF
               SFAUD1(I-33,QARSLT) = SFAUD1(I-33,QARSLT) + 1
               IF(QARSLT .EQ. 1 .AND. SFSTRA(I-29) .EQ. 1) THEN
                  MESS = BLNK40
                  WRITE(MESS,360) VNAMES(I),SFGYR,SFGMO,SFGDY,SFGHR
                  CALL ERROR(IWORK1(1210),PATH,'   ',LOC,MESS)
               ENDIF

C
C ***         PART 2
C
               MISS2  = SFQA(I,2) - MISS1*CONCAT(I-29)
               IQA2L  = SFQA(I,3) - IQA1L*CONCAT(I-29)
               IQA2U  = SFQA(I,4) - IQA1U*CONCAT(I-29)
               IPART2 = SFOBS(1,I) - IPART1*CONCAT(I-29)
C
               QARSLT = 5
               CALL INTECK(3,IWORK1(1210),SFQA(I,1),MISS2,IQA2L,
     1                     IQA2U,IPART2,VNAMES(I),QARSLT)
               IF((QARSLT .EQ. 2) .OR. (QARSLT .EQ. 3)) THEN
                MESS = BLNK40
                WRITE(MESS,350) SFGYR,SFGMO,SFGDY,SFGHR
                CALL ERROR(IWORK1(1210),PATH,'   ','      ',MESS)
               ENDIF
               SFAUD2(I-33,QARSLT) = SFAUD2(I-33,QARSLT) + 1
               IF(QARSLT .EQ. 1 .AND. SFSTRA(I-29) .EQ. 1) THEN
                  MESS = BLNK40
                  WRITE(MESS,360) VNAMES(I),SFGYR,SFGMO,SFGDY,SFGHR
                  CALL ERROR(IWORK1(1210),PATH,'   ',LOC,MESS)
               ENDIF
C
C ***       THE REMAINING VARIABLES ARE NOT CONCATENATED
C           NOTICE THAT SFAUD(5,-) THRU SFAUD(15,-) ARE NOT USED HERE
C
            ELSE
               QARSLT = 5
               CALL INTECK(3,IWORK1(1210),SFQA(I,1),SFQA(I,2),SFQA(I,3),
     1          SFQA(I,4),SFOBS(1,I),VNAMES(I),QARSLT)
               IF((QARSLT .EQ. 2) .OR. (QARSLT .EQ. 3)) THEN
                MESS = BLNK40
                WRITE(MESS,350) SFGYR,SFGMO,SFGDY,SFGHR
                CALL ERROR(IWORK1(1210),PATH,'   ','      ',MESS)
               ENDIF
               SFAUD(I-29,QARSLT) = SFAUD(I-29,QARSLT) + 1
               IF(QARSLT .EQ. 1 .AND. SFSTRA(I-29) .EQ. 1) THEN
                  MESS = BLNK40
                  WRITE(MESS,360) VNAMES(I),SFGYR,SFGMO,SFGDY,SFGHR
                  CALL ERROR(IWORK1(1210),PATH,'   ',LOC,MESS)
               ENDIF
            END IF
40        CONTINUE
C
C ***   Check for calm winds & dew point > temperature
C
        IF( SFOBS(1,51) .EQ. 0) THEN
         IF(SFOBS(1,50) .EQ. 0) THEN
C
C *       WINDS ARE CALM
          SFCALM = SFCALM + 1
          MESS = BLNK40
          WRITE(MESS,320) SFGYR,SFGMO,SFGDY,SFGHR
          CALL ERROR(IWORK1(1210),PATH,'CLM',LOC,MESS)
         ELSE IF(SFOBS(1,50) .GT. 0) THEN
C
C *       ZERO WIND SPEED, NONZERO WIND DIRECTION
          SFWSWD = SFWSWD + 1
          MESS = BLNK40
          WRITE(MESS,330) SFGYR,SFGMO,SFGDY,SFGHR
          CALL ERROR(IWORK1(1210),PATH,'ZNZ',LOC,MESS)                  DTBMAY93
         ENDIF
        ENDIF

        IF(( SFOBS(1,48) .NE. SFQA(48,2)) .AND.
     &       (SFOBS(1,46).NE.SFQA(46,2)))THEN
         IF(SFOBS(1,48) .GT. SFOBS(1,46) ) THEN
C
C *       Dew-point exceeds the dry bulb temperature
          TDGTT = TDGTT + 1
          MESS = BLNK40
          WRITE(MESS,340) SFGYR,SFGMO,SFGDY,SFGHR
          CALL ERROR(IWORK1(1210),PATH,'DTT',LOC,MESS)                  DTBMAY93
         ENDIF
        ENDIF
C
C     Compare precipitation amount (if it is not missing) and present
C     weather for consistency (if precip <==> if proper weather)
C     The present weather is stored in element 44, the precip in 52.

        MISS1  = SFQA(44,2)/CONCAT(44-29)
        IPART1 = SFOBS(1,44)/CONCAT(44-29)
        MISS2  = SFQA(44,2) - MISS1*CONCAT(44-29)
        IPART2 = SFOBS(1,44) - IPART1*CONCAT(44-29)

        IF( SFOBS(1,52) .NE. SFQA(52,2) )THEN
           PPT = FLOAT(SFOBS(1,52))/CONCAT(23)
           IF( PPT .GT. 0.0 )THEN
               IF( IPART1 .EQ. 0  .AND.  IPART2 .EQ. 0 )THEN
C                 Precipitation without weather!
                  PPTERR = PPTERR + 1
                  MESS = BLNK40
                  WRITE(MESS,370) SFGYR,SFGMO,SFGDY,SFGHR
                  CALL ERROR(IWORK1(1210),PATH,'PPT',LOC,MESS)
               ELSEIF( IPART1 .EQ. MISS1  .AND.  IPART2 .EQ. MISS2 )THEN
C                 Precipitation but weather is missing!
                  PPTERR = PPTERR + 1
                  MESS = BLNK40
                  WRITE(MESS,390) SFGYR,SFGMO,SFGDY,SFGHR
                  CALL ERROR(IWORK1(1210),PATH,'PPT',LOC,MESS)
               ENDIF

           ELSE
              IF( (IPART1 .GT. 0  .AND. IPART1 .LT. 70)  .OR.
     &             IPART1 .GE. 90 )THEN
C                Weather without precipitation !
                 PPTERR = PPTERR + 1
                 MESS = BLNK40
                 WRITE(MESS,380) SFGYR,SFGMO,SFGDY,SFGHR
                 CALL ERROR(IWORK1(1210),PATH,'PPT',LOC,MESS)
              ENDIF
           ENDIF
        ENDIF
C
C ***   GET NEXT OBSERVATION
C
        GO TO 20
C
C-----------------------------------------------------------------------
C *** PROCESSING CONTINUES HERE IF AN EOF OR ERROR IS ENCOUNTERED
C
400   MESS=BLNK40
      WRITE(MESS,800) READHO
      CALL ERROR(READHO,PATH,'E42',LOC,MESS)
800   FORMAT(' ERROR READING RECORD 1 OF OBS # ',I5)
      RETURN
C
410   MESS=BLNK40
      WRITE(MESS,810) READHO - 1
      CALL ERROR(READHO,PATH,'I49',LOC,MESS)
810   FORMAT(' END OF FILE AFTER OBS # ',I5)
      WRITE(DEV70,601)
      WRITE(DEV70,600) READHO-1,SFCALM,SFWSWD,TDGTT,PPTERR
      WRITE(DEV70,601)
      RETURN
C
420   MESS=BLNK40
      WRITE(MESS,820) READHO
      CALL ERROR(READHO,PATH,'E42',LOC,MESS)
820   FORMAT(' ERROR DECODING RECORD 1 OF OBS # ',I5)
      RETURN
C
430   MESS=BLNK40
      WRITE(MESS,830) READHO
      CALL ERROR(READHO,PATH,'E42',LOC,MESS)
830   FORMAT(' ERROR READING RECORD 2 OF OBS # ',I5)
      RETURN
C
440   MESS=BLNK40
      WRITE(MESS,840)READHO
      CALL ERROR(READHO,PATH,'E49',LOC,MESS)
840   FORMAT(' END OF FILE AT RECORD 2 AT OBS # ',I5)
      RETURN
C
C-----------------------------------------------------------------------
  135 FORMAT(' ERROR PROCESSING HEADERS, NO QA')
  300 FORMAT(80A1)
  301 FORMAT(1X,I2,I2,I2,I2,4(1X,I5),6(1X,I5.5))
  307 FORMAT(8X,5(1X,I5.5),7(1X,I5),1x,I6)
  320 FORMAT(' CALM WINDS ON ',I2,3('/',I2.2))
  330 FORMAT(' WS .EQ. 0, WD .NE. 0 ON ',I2,3('/',I2.2))
  340 FORMAT(' DRY BULB .GT. DEW-POINT ON ',I2,3('/',I2.2))
  350 FORMAT(' ON  ',I2,3('/',I2.2))
  360 FORMAT(1X,A4,' MISSING ON  ',I2,3('/',I2.2))
  370 FORMAT(' PRECIP WITHOUT WEATHER ON ',I2,3('/',I2.2))
  380 FORMAT(' WEATHER WITHOUT PRECIP ON ',I2,3('/',I2.2))
  390 FORMAT(' PRECIP WITH MISSING WX ON ',I2,3('/',I2.2))
  600 FORMAT('     THE FOLLOWING CHECKS WERE ALSO PERFORMED FOR THE',
     &      ' SURFACE QA',/'       OF ',I5,' REPORTS, THERE WERE',/,
     &      10X,I5,' CALM WIND CONDITIONS (WS=0, WD=0)'/
     &      10X,I5,' ZERO WIND SPEEDS WITH NONZERO WIND DIRECTIONS'/
     &      10X,I5,' DEW-POINT GREATER THAN DRY BULB TEMPERATURES'/
     &      10X,I5,' PRECIPITATION & WEATHER MISMATCH'/
     &       5X,'THE TIMES OF THESE OCCURRENCES CAN BE FOUND IN THE',
     &          ' MESSAGE FILE'/5X,' WITH QUALIFIERS CLM, ZNZ, DTT ',   DTBMAY93
     &            '& PPT (RESP.)'//)
  601 FORMAT('$SFSF$')
 2200 FORMAT('*  SF     SURFACE DATA QA,    MPRM DATED ', I5)           DTBAUG94
C
C-----------------------------------------------------------------------
C *** END OF SUBROUTINE
      END

      subroutine rd144t(l,m,n)
      return
      end



      SUBROUTINE SETSAM( NVARS, ISTAT )
C***********************************************************************
C*       SETSAM Module of the MPRM Meteorological Pre-processor
C*
C*    PURPOSE:    To determine the data structure of the SAMSON data
C                 (up to 21 variables may be in the file)
C*
C*    PROGRAMMER: Jayant Hardikar
C*                PES Inc.
C*
C*    DATE:       April 28, 1995
C*     
C*    INPUTS:     SAMSON met data file (only first 2 records are needed
C                 in this routine)
C*
C*    OUTPUTS:    SAMFMT - format of the data (in COMMON block)
C                 NVARS  - number of variables that will be read
C                 IDVAR  - variable number (e.g., wind speed is 13)
C                 JVALUE -
C*        
C     Assumptions:  The first record of the SAMSON file contains station
C                   information and the second contains format information.
C                   These are as written by the program that retrieves
C                   data from the CD (run by the user *prior* to MPRM)
C*
C*    CALLED FROM: SFEXT
C***********************************************************************
C*
C*    Variable Declarations

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'

      CHARACTER*256 ALINE
      CHARACTER*40  FIELD(MAXVAR)
      INTEGER IWBAN
C      INTEGER IDVAR(MAXVAR-5)
      CHARACTER*2 VFMT(MAXVAR-5)

      INCLUDE 'WORK1.INC'
      
      DATA VFMT /'A4','A4','A7','A7','A7','A2','A2','A5','A5','A3',
     &           'A4','A3','A5','A6','A6','A9','A4','A6','A4','A3',
     &           'A6'/

       PATH = 'SF'
       LOC  = 'SETSAM'
       NCOL = 256

C*     Read station header record for the station ID
       READ (DEV20,6000) IWBAN

C      Compare the station ID to the station-to-extract ID
C
       READ( SFLOC, 2000, IOSTAT=IOST20 ) ISTN
 2000  FORMAT(I8)

       IF( IOST20 .NE. 0 )THEN
          MESS = BLNK40
          WRITE (MESS, 6600) IWBAN, SFLOC
 6600     FORMAT(' CANNOT COMPARE STN ID''S ',I5,', ',A8 )
          CALL ERROR(2,PATH,'W47',LOC,MESS)

       ELSEIF( ISTN .NE. IWBAN )THEN
          ISTAT = 1
          MESS = BLNK40
          WRITE (MESS, 6700) IWBAN
 6700     FORMAT(' SAMSON STN (' ,I5, ') .NE. STN ID ON ''LOC''' )
          CALL ERROR(2,PATH,'E48',LOC,MESS)
          RETURN
       ENDIF

C*     Read extracted-variables header record
       READ (DEV20,'(A)') ALINE

C*     Parse the line to determine the individual variables
       CALL DEF256 (NCOL,ALINE,IFC)

C      Get contents of each field as a character variable (FIELD)
       CALL GETFLD( IFC,ALINE,FIELD )

C*     Decrement the counter for number of variables to exclude
C*     (the yr,mo,dy,hr,i fields)
       NVARS = IFC-5

C*     Determine the format and the variables to read  
       WRITE (SAMFMT(1:15),'(A)') "(4A3,1X,A1,1X,"
       IPOS = 14

       DO 200 IVAR = 1,NVARS

C*        Get the ID of the variable
          CALL STONUM(FIELD(IVAR+5),40,FNUM,IMIT)
          IF (IMIT .NE. 1) THEN
             MESS = BLNK40
             WRITE (MESS, 6500) FIELD(IVAR+5)(1:5)
             CALL ERROR(2,PATH,'E47',LOC,MESS)
             ISTAT = 1
             GO TO 999
          ELSE
             IDVAR(IVAR) = INT(FNUM)
          END IF

          IPOS = IPOS+1
          WRITE (SAMFMT(IPOS:IPOS+1),'(A2)') VFMT(IDVAR(IVAR))
          IPOS = IPOS + 2
          IF (IVAR .NE. NVARS) THEN
             WRITE (SAMFMT(IPOS:IPOS+3),'(A4)') ",1X,"
             IPOS = IPOS +3
          ELSE
            WRITE (SAMFMT(IPOS:IPOS),'(A1)') ")"
          ENDIF

200    CONTINUE

      
 6000 FORMAT (T2,I5,:,T8,A22,T31,A2,T34,I3,T39,A1,T40,I2,T43,I2,
     &        T47,A1,T48,I3,T52,I2,T56,I4)
 6500 FORMAT('ERROR CONVERTING SAMSON VBL ID ', A5)
 
999   RETURN
      END



      SUBROUTINE RD3240( PPBFST, KURDAY, NPRECS )
C***********************************************************************
C*
C*    PURPOSE:    Processes 24 hours of precipitation data
C*
C*    PROGRAMMER: Jim Paumier, PES
C*
C*    DATE:       April 28, 1995
C*     
C*    INPUTS:     Sequential day (KURDAY) precipitation is needed
c*
C*    OUTPUTS:    24 Hours of precipitation data
C*                Status of the read
C*                Number of hours of precipitation retrieved (cumulative)
C*        
C*    CALLED FROM: SFEXT
C***********************************************************************
C*
C*    Variable Declarations
      INTEGER PPBFST, IDSTA
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'WORK1.INC'

      DATA NPREAD/0/
      PATH = 'SF'
      LOC  = 'RD3240'


C     Convert the sequential day to year and Julian day
      CALL ICHRND(PATH, KURDAY, KYEAR, JULDAY )
      CALL GREG( KYEAR, JULDAY, PPTGMO, PPTGDY )
      PPTGYR = KYEAR
      IHR = 1

C     The routines used below to retrieve the precipitation data
C     are from another preprocessor, PMERGE.  The logic in that
C     code will automatically keep searching until the extract
C     window is found because PPDAYC = SFDAY1 for the first call
C     to SUBR.RD3240
      IF( PPDAYC .GE. SFDAY1  .AND.  PPDAYC .LE. SFDAY2 )THEN
         DO WHILE( IHR .LE. 24  .AND.
     &             SFSTAT .NE. -1  .AND.  PPBFST .GT. 0)
            NDATE = KYEAR*100000 + JULDAY*100 + IHR
            CALL UNCDP( DEV25, NDATE, 24, 1, PMM, ICODE, NPREAD, IDSTA)
            IF( IDSTA .EQ. IDPPT )THEN
               IF( ICODE .GE. 1   .AND.  ICODE .LE. 2) THEN
C                 Valid precip value returned
                  PPBFST = 2
                  PPTAMT(IHR) = NINT( PMM * 1000 )
                  NPRECS = NPRECS + 1
               ELSEIF( ICODE .GE. 3  .AND.  ICODE .LE. 5 )THEN
C                 Error encountered
                  MESS = BLNK40
                  WRITE(MESS,400) ICODE,NDATE
                  CALL ERROR(NPREAD,PATH,'E45',LOC,MESS)
                  SFSTAT = -1
                  PPTAMT(IHR) = -9
               ELSEIF( ICODE .EQ. 6 )THEN
C                 End of file encountered
                  MESS = BLNK40
                  WRITE(MESS,410) NDATE
                  CALL ERROR(NPREAD,PATH,'I49',LOC,MESS)
                  PPBFST = -1
                  PPTAMT(IHR) = -9
               ELSEIF( ICODE .EQ. 7 )THEN
C                 Error in accumulation, missing or deleted period or a date
                  MESS = BLNK40
                  WRITE(MESS,400) ICODE,NPREAD
                  CALL ERROR(NPREAD,PATH,'E45',LOC,MESS)
                  SFSTAT = -1
                  PPTAMT(IHR) = -9
               ELSE
C                 Should not be able to reach this point
Cdbg           print *,' Incorrect ICODE returned from UNCDP for',ndate
               ENDIF
Cdbg           print *, ' RD3240: ',ndate,pmm
               IHR = IHR + 1
            ELSE
C              Station mismatch
               MESS = BLNK40
               WRITE(MESS,420)
               CALL ERROR(NPREAD,PATH,'E40',LOC,MESS)
               MESS = BLNK40
               WRITE(MESS,430) IDSTA,IDPPT
               CALL ERROR(0,'  ','+++','      ',MESS)
               SFSTAT = -1
            ENDIF
         ENDDO

      ELSEIF( PPDAYC .GT. SFDAY2 )THEN
         PPBFST = -1

      ENDIF

  400 FORMAT( ' ERROR CODE ',I1,' FOR PRECIP ON ',I7 )
  410 FORMAT( ' E-O-F FOR PRECIP ON ', I7 )
  420 FORMAT( ' STATION MISMATCH IN PRECIP FILE')
  430 FORMAT( ' FILE: ',I8,'  USER EXPECTS: ',I8)
      RETURN
      END

      SUBROUTINE FIXPPT (DEVICE, ISTAT)
C***********************************************************************
C*
C*    PURPOSE:    To convert a variable format structure to a fixed
C                 block structure of the TD3240 precipitation data
C*
C*    PROGRAMMER: Jayant Hardikar; modified by Jim Paumier for MPRM
C*                PES Inc.
C*
C*    DATE:       April 28, 1995
C*     
C*    INPUTS:     Variable formatted TD-3240 precipitation data
C*
C*    OUTPUTS:    A file of precipitation data with fixed block records
C*        
C     Assumptions:  No other file is connected to unit number 26
C*
C*    CALLED FROM: SFEXT
C***********************************************************************
C*
C*    Variable Declarations
      CHARACTER*27 TEXT1
      CHARACTER*12 TEXT2(100)
      CHARACTER*3  BTEST
      INTEGER NUMTXT,DEVICE
      INCLUDE 'WORK1.INC'

      PATH = 'SF'
      LOC  = 'FIXPPT'
      
C*    Check To See If The File Is REALLY A Variable format
      READ (DEVICE,'(T28,A3)',ERR=891) BTEST
      IF (BTEST .EQ. '   ')THEN
C        The blanks in columns 28-30 suggest a fixed format - assume
C        such and continue processing
         MESS = BLNK40
         WRITE( MESS, 600 )
         CALL ERROR(0,PATH,'W44',LOC,MESS)
         ISTAT = 2
         REWIND (DEVICE)
         RETURN
      ENDIF
      REWIND (DEVICE)

      OPEN (26, STATUS='scratch')
      write (*,*) 'pptfile opened - temporary'

      DO 500 I = 1,9999
         READ (DEVICE, 2500, END=2510, ERR=2520) TEXT1,NUMTXT,
     &                                         (TEXT2(J),J=1,NUMTXT)
         DO 400 II = 1,NUMTXT
            WRITE (26,2600) TEXT1,TEXT2(II)
  400    CONTINUE
  500 CONTINUE

  891 ISTAT = 1
      MESS = BLNK40
      WRITE( MESS, 610 )
      CALL ERROR(0,PATH,'E44',LOC,MESS)
      RETURN

 2510 ISTAT = 2
C     Normal EOF: close the variable format file, swap the unit number,
C                 rewind the file
      CLOSE(UNIT=DEVICE)
      DEVICE = 26
      REWIND( DEVICE )
      RETURN

 2520 ISTAT = 1
      MESS = BLNK40
      WRITE( MESS, 620 ) I
      CALL ERROR(0,PATH,'E45',LOC,MESS)
      RETURN

C---- Format statements
  600 FORMAT( ' PRECIP NOT VARIABLE FMT, ASSUMING FIXED' )
  610 FORMAT( ' READ FAILED FOR PRECIP FORMAT, REC # 1' )
  620 FORMAT( ' ERROR CONVERTING REC# ',I4,' TO FIXED FMT' )
 2500 FORMAT( A27,I3,100(A12))
 2600 FORMAT( A27,3X,A12)

      END



      SUBROUTINE UNCDP (IO, NDATE, MAXAP, K, PMM, ICODE, NPREAD, IDSTA)
C***********************************************************************
C*
C*    PURPOSE:    To retrieve one hour of precipitation data from a
C                 fixed format TD3240 file
C*
C*    PROGRAMMER: Sigma Research; modified by Jim Paumier, PES, Inc.
C*
C*    DATE:       April 28, 1995
C*     
C*    INPUTS:     Fixed format TD-3240 precipitation data
C*
C*    OUTPUTS:    One hour of precipitation data
C*        
C     Assumptions:
C*
C*    CALLED FROM: SFEXT
C***********************************************************************

c --- Determine the precipitation rate for a given date/hour by:
c        (a) reading a character variable storing a previously read
c            record
c        (b) reading a precipitation record from a TD-3240 file
c        (c) resolving an accumulation period
c        (d) resolving a missing data period
c
c --- INPUTS:
c                IO - integer    - Fortran unit no. of precip. input
c                                  file
c             NDATE - integer    - Coded date/time field (YYJJJHH) of
c                                  current hour
c             MAXAP - integer    - Maximum allowed length (hrs) of an
c                                  accumulation period
c                 K - integer    - Array index of precip. arrays
c                                  (K = station no.)
c --- OUTPUT:
c               PMM - real       - Precipitation rate (mm/hr)
c                                  (missing value indicator = 9999.)
c             ICODE - integer    - Data status code:
c                                  1 = valid hourly value,
c                                  2 = valid accumulation period,
c                                  3 = missing data flag (labeled missing),
c                                  4 = missing due to excessive length
c                                      of accumulation period,
c                                  5 = missing data before first valid
c                                      record in file
c                                  6 = missing data after last valid
c                                      record in file (i.e., EOF)
c                                  7 = accumulation, missing or deleted
c                                      period error, or date error
c --- UNCDP called by:  RDWRIT
c --- UNCDP calls:      PREAD
c
c --- IFLAG -- Flag indicating precip. data status:
c              IFLAG = -99 if this is the first pass for this station
c              IFLAG =   0 if date/hr of first precip. record has not
c                          been reached yet (data is assumed missing
c                          up to date/hr of first valid record)
c              IFLAG =  +1 if current date/hr > date/hr of first record
c                          (precip. rate is assumed = 0.0 between time
c                          of valid records)
c ------------------------------------------------------------------------------
C*
C*    Variable Declarations

      parameter(maxps=1)
c
      real         pmmsav(maxps)
      integer      icodsv(maxps),ibdat(maxps),iedat(maxps),iflag(maxps)
      integer      iprev(maxps)
      character*42 cdat(maxps)
c
      data icodsv/maxps*0/,ibdat/maxps*0/,iedat/maxps*0/
      data iflag/maxps*-99/,cdat/maxps*' '/
      data iprev/maxps*0/
c
c --- determine if current date/hr is within range previously stored
c --- pmmsav array
10    continue
      if(ndate.lt.ibdat(k))then
c
c ---    Date/hr between valid records -- precip. rate = 0.0
         pmm=0.0
         icode=1
         return
      else if(ndate.le.iedat(k))then
c
c ---    Current date/hr is within period of validity of pmmsav
         pmm=pmmsav(k)
         icode=icodsv(k)
         return
      endif
c
c --- Current date/hr is after end of period of validity of pmmsav
25    continue
      call pread(io,ndate,maxap,iflag(k),cdat(k),iprev(k),
     1           icodsv(k),ibdat(k),iedat(k),pmmsav(k),NPREAD,IDSTA)
      go to 10
      end
c ------------------------------------------------------------------------------
      subroutine pread(io,ndate,maxap,iflag,cdat,iprev,
     1 icode,ibdat,iedat,pmmsav,NPREAD,IDSTA)
c ------------------------------------------------------------------------------
c
c --- PMERGE   Version: 1.2       Level: 921022                    PREAD
c ---          J. Scire, SRC
c
c --- Read a precipitation record -- if necessary, read a second record
c --- to resolve a missing data or accumulation period
c
c --- INPUTS:
c                IO - integer    - Fortran unit no. of precip. input
c                                  file
c             NDATE - integer    - Coded date/time field (YYJJJHH) of
c                                  current hour
c             MAXAP - integer    - Maximum allowed length (hrs) of an
c                                  accumulation period
c             IFLAG - integer    - Flag indicating precip. data status:
c                                  IFLAG = -99 if this is the first pass
c                                              for this station
c                                  IFLAG =   0 if date/hr of first
c                                              precip. record has not
c                                              been reached yet (data is
c                                              assumed missing up to
c                                              date/hr of first valid
c                                              record)
c                                  IFLAG =  +1 if current date/hr >
c                                              date/hr of first record
c                                              (precip. rate is assumed
c                                              = 0.0 between time of
c                                              valid records)
c              CDAT - char.*42   - A character string to store a TD-3240
c                                  data record (an input only if IFLAG = 0)
c             IPREV - integer    - Coded date/time field (YYJJJHH) of
c                                  previously read TD-3240 record
c
c --- OUTPUT:
c             ICODE - integer    - Data status code:
c                                  1 = valid hourly value,
c                                  2 = valid accumulation period,
c                                  3 = missing data flag (labeled missing)
c                                  4 = missing due to excessive length
c                                      of accumulation period
c                                  5 = missing data before first valid
c                                      record in file
c                                  6 = missing data after last valid
c                                      record in file
c                                  7 = accumualtion, missing or deleted
c                                      period error, or date error
c             IBDAT - integer    - Beginning date/time of data (YYJJJHH)
c             IEDAT - integer    - Ending date/time of data (YYJJJHH)
c            PMMSAV - real       - Precipitation rate (mm/hr)
c                                  (missing value indicator = 9999.)
c              CDAT - char.*42   - A character string storing a TD-3240
c                                  data record (an output only if
c                                  IFLAG = -99)
c             IPREV - integer    - Updated coded date/time field of
c                                  last TD-3240 record read
c
c --- PREAD called by:  UNCDP
c --- PREAD calls:   JULIAN
c                    INDECR
c ------------------------------------------------------------------------------
      parameter(io6=6)
c
      character*42 cdat
      character*1 cflag,cflag2
      cflag=' '
      cflag2=' '
c
      if(iflag.le.0)then
c
c ---    If first time through for this station, read TD-3240 record &
c ---    store in character string (CDAT)
         if(iflag.eq.-99)then
            NPREAD = NPREAD + 1
            read(io,18)cdat
18          format(a42)
            iflag=0
c
c ---       extract station id, date/hr
            read(cdat,20)idsta,iyr,imo,iday,ihr
20          format(3x,i6,10x,i2,i2,i4,3x,i2,2x,i6,a1)
            ijul = julian(iyr,imo,iday)
            idate=iyr*100000 + ijul*100 + ihr
            iprev=idate
c
c
c ---       all data up to time of first record in file is considered
c ---       missing
            icode=5
            ibdat=0
c ---       subtract one hour from yr/Julian day/hr
            call indecr(iyr,ijul,ihr,-1,1,24)
            iedat=iyr*100000 + ijul*100 + ihr
            pmmsav=9999.0
            return
         endif
c
c ---    extract data from a previously read character string
         read(cdat,20,end=995)idsta,iyr,imo,iday,ihr,ihinch,cflag
         iflag=1
         ijul = julian(iyr,imo,iday)
         idate=iyr*100000+ijul*100+ihr
c

      else
c
c ---    read a new record
         NPREAD = NPREAD + 1
         read(io,20,end=995)idsta,iyr,imo,iday,ihr,ihinch,cflag
         ijul = julian(iyr,imo,iday)
         idate=iyr*100000+ijul*100+ihr
c
c
c ---    check if date/hr of record is out of order
         if(idate.gt.iprev)then
            iprev=idate
         else
            go to 1040
         endif
      endif
c
      if(cflag.eq.' ' .or. cflag.eq.'E')then
c
c ---    valid hourly data value -- convert to mm/hr
         icode=1
         ibdat=idate
         iedat=idate
         pmmsav=0.254*float(ihinch)
         return
      else if(cflag.eq.'A')then
c
c ---    beginning of accumulation period -- read next record
c ---    with ending date/time of accumulation period & accum. amount
         NPREAD = NPREAD + 1
         read(io,20,end=996)jdsta,jyr,jmo,jday,jhr,jhinch,cflag2
         jjul = julian(jyr,jmo,jday)
         jdate=jyr*100000+jjul*100+jhr
         if(jdate.gt.iprev)then
            iprev=jdate
         else
            go to 1050
         endif
c
         if(cflag2.ne.'A')then
c
c ---       ERROR -- unpaired accumulation period
            go to 1000
         else
c
c ---       paired accumulation records -- resolve accumulation
c ---       period precip. rate
            call deltt(iyr,ijul,ihr,jyr,jjul,jhr,idelt)
            nhrs=idelt+1
c
c ---       if length of the accumulation period exceeds max.
c ---       allowed, consider data as missing
            if(nhrs.gt.maxap)then
               icode=4
               ibdat=idate
               iedat=jdate
               pmmsav=9999.
               return
            endif
c
c ---       valid accumulation period -- resolve & save results
            icode=2
            ibdat=idate
            iedat=jdate
            pmmsav=0.254*float(jhinch)/float(nhrs)
            return
         endif
c
      else if(cflag.eq.'M' .or. cflag.eq.'D')then
c
c ---    beginning of missing or deleted data period -- read next record
c ---    with ending date/time of missing or deleted data period
         NPREAD = NPREAD + 1
         read(io,20,end=998)jdsta,jyr,jmo,jday,jhr,jhinch,cflag2
         jjul = julian(jyr,jmo,jday)
         jdate=jyr*100000+jjul*100+jhr
         if(jdate.gt.iprev)then
            iprev=jdate
         else
            go to 1050
         endif
c
c **     if(cflag2.ne.'M')then
         if(cflag2.ne.cflag)then
c
c ---       ERROR -- unpaired missing or deleted data records
            go to 1030
         else
c
c ---       paired missing data records
            icode=3
            ibdat=idate
            iedat=jdate
            pmmsav=9999.
            return
         endif
      endif
c
c --- invalid precipitation flag encountered
      write(io6,990)idsta,ndate,io,iyr,imo,iday,ihr,cflag,cflag2
990   format(/1x,'Error in subr. PREAD -- invalid ',
     1 'precipitation flag encountered'//1x,'ID = ',i6,3x,
     2 //1x,'Requested date/hr (YYJJJHH) = ',i7,3x,'io = ',i5
     3 //1x,'Yr: ',i2,2x,'Month: ',i2,2x,'Day: ',i2,
     4 2x,'Hr: ',i2//1x,'CFLAG = ',a1,3x,'CFLAG2 = ',a1)
      stop
c
c --- end of file encountered
995   continue
      icode=6
      ibdat=0
      iedat=9999999
      pmmsav=9999.
      return
c
c --- end of file encountered -- unpaired accumulation period
996   continue
      icode = 7
      write(io6,997)idsta,ndate,io,iyr,imo,iday,ihr,cflag
997   format(/1x,'Error in subr. PREAD -- unpaired ',
     1 'accumulation period'//1x,'ID = ',i6,3x,
     2 //1x,'Requested date/hr (YYJJJHH) = ',i7,3x,'io = ',i5
     3 //1x,'1st record: ',2x,'Yr: ',i2,2x,'Month: ',i2,2x,
     4 'Day: ',i2,2x,'Hr: ',i2,2x,' Flag: ',a1
     5 //1x,'2nd record: ',2x,'END OF FILE REACHED')
       return
c
c --- end of file encountered -- unpaired missing or deleted period
998   continue
      icode = 7
      write(io6,999)idsta,ndate,io,iyr,imo,iday,ihr,cflag
999   format(/1x,'Error in subr. PREAD -- unpaired ',
     1 'missing or deleted data period'//1x,'ID = ',i6,3x,
     2 //1x,'Requested date/hr (YYJJJHH) = ',i7,3x,'io = ',i5
     3 //1x,'1st record: ',2x,'Yr: ',i2,2x,'Month: ',i2,2x,
     4 'Day: ',i2,2x,'Hr: ',i2,2x,' Flag: ',a1
     5 //1x,'2nd record: ',2x,'END OF FILE REACHED')
      return
c
c --- write error message -- unpaired accumulation period
1000  continue
      icode = 7
      write(io6,1002)idsta,ndate,io,iyr,imo,iday,ihr,cflag,
     1 jyr,jmo,jday,jhr,cflag2
1002  format(/1x,'Error in subr. PREAD -- unpaired ',
     1 'accumulation period'//1x,'ID = ',i6,3x,
     2 //1x,'Requested date/hr (YYJJJHH) = ',i7,3x,'io = ',i5
     3 //1x,'1st record: ',2x,'Yr: ',i2,2x,'Month: ',i2,2x,
     4 'Day: ',i2,2x,'Hr: ',i2,2x,' Flag: ',a1
     5 //1x,'2nd record: ',2x,'Yr: ',i2,2x,'Month: ',i2,2x,
     6 'Day: ',i2,2x,'Hr: ',i2,2x,' Flag: ',a1)
      return
c
c --- write error message -- unpaired missing data period
1030  continue
      icode = 7
      write(io6,1032)idsta,ndate,io,iyr,imo,iday,ihr,cflag,
     1 jyr,jmo,jday,jhr,cflag2
1032  format(/1x,'Error in subr. PREAD -- unpaired ',
     1 'missing or deleted data period'//1x,3x,
     2 //1x,'Requested date/hr (YYJJJHH) = ',i7,3x,'io = ',i5
     3 //1x,'1st record: ',2x,'Yr: ',i2,2x,'Month: ',i2,2x,
     4 'Day: ',i2,2x,'Hr: ',i2,2x,' Flag: ',a1
     5 //1x,'2nd record: ',2x,'Yr: ',i2,2x,'Month: ',i2,2x,
     6 'Day: ',i2,2x,'Hr: ',i2,2x,' Flag: ',a1)
      return
c
c --- write error message -- invalid date/hr ( <= previous value)
1040  continue
      icode =  7
      write(io6,1042)idsta,ndate,io,iyr,imo,iday,ihr,cflag,
     1               idate,iprev
1042  format(/1x,'Error in subr. PREAD -- invalid date/hr ( <= ',
     1 'previous value)'//1x,'ID = ',i6,3x,
     2 //1x,'Requested date/hr (YYJJJHH) = ',i7,3x,'io = ',i5
     3 //1x,'Yr: ',i2,2x,'Month: ',i2,2x,
     4 'Day: ',i2,2x,'Hr: ',i2,2x,' Flag: ',a1
     5 //1x,'Date/hr (YYJJJHH) = ',i10,'  (Current record)'
     6 //1x,'Date/hr (YYJJJHH) = ',i10,' (Previous record)')
      return
c
1050  continue
      icode = 7
      write(io6,1042)jdsta,ndate,io,jyr,jmo,jday,jhr,cflag,
     1  jdate,iprev
      return
      end


c ------------------------------------------------------------------------------
      subroutine indecr(iyr,ijul,ihr,idelt,ihrmin,ihrmax)
c ------------------------------------------------------------------------------
c
c --- PMERGE   Version: 1.2       Level: 901130                    INDECR
c ---          J. Scire, SRC
c
c --- Increment or decrement a date/time by "IDELT" hours
c --- (-24 <= IDELT <= 24)
c --- Allows specification of 0-23 or 1-24 hour clock
c
c --- INPUTS:
c               IYR - integer    - Input Year
c              IJUL - integer    - Input Julian day
c               IHR - integer    - Input hour (ihrmin <= IHR <= ihrmax)
c             IDELT - integer    - Change in time (hours) -- must be
c                                  between -24 to +24, inclusive
c            IHRMIN - integer    - Minimum hour (i.e., either  0 or  1)
c            IHRMAX - integer    - Maximum hour (i.e., either 23 or 24)
c
c --- OUTPUT:
c               IYR - integer    - Year after change of "IDELT" hours
c              IJUL - integer    - Julian day after change of "IDELT" hours
c               IHR - integer    - Hour after change of "IDELT" hours
c
c --- INDECR called by:  RDWRIT, PREAD
c --- INDECR calls:      none
c ------------------------------------------------------------------------------
      parameter(io6=6)
c
      if(iabs(idelt).gt.24)then
         write(io6,10)'IDELT',iyr,ijul,ihr,idelt,ihrmin,ihrmax
10       format(/1x,'ERROR in subr. INDECR -- invalid "',a,'" -- ',
     1   ' iyr,ijul,ihr,idelt,ihrmin,ihrmax = ',6i10)
         stop
      endif
      if(ihr.lt.ihrmin.or.ihr.gt.ihrmax)then
         write(io6,10)'IHR',iyr,ijul,ihr,idelt,ihrmin,ihrmax
         stop
      endif
c
      if(idelt.lt.0)then
c ---    idelt is negative
         ihr=ihr+idelt
         if(ihr.lt.ihrmin)then
            ihr=ihr+24
            ijul=ijul-1
            if(ijul.lt.1)then
               iyr=iyr-1
               if(mod(iyr,4).eq.0)then
                  ijul=366
               else
                  ijul=365
               endif
            endif
         endif
      else
c ---    idelt is positive or zero
         ihr=ihr+idelt
         if(ihr.gt.ihrmax)then
            ihr=ihr-24
            ijul=ijul+1
            if(mod(iyr,4).eq.0)then
               ndays=366
            else
               ndays=365
            endif
            if(ijul.gt.ndays)then
               ijul=1
               iyr=iyr+1
            endif
         endif
      endif
c
      return
      end


      subroutine deltt(j1yr,j1jul,j1hr,j2yr,j2jul,j2hr,jleng)
c-----------------------------------------------------------------------
c
c --- PMERGE   Version: 1.2       Level: 901130                    DELTT
c ---          J. Scire, SRC
c
c --- Compute the difference (in hours) between two dates & times
c ---    (time #2 - time #1)
c
c --- INPUTS:
c              J1YR - integer    - Year of date/time #1
c             J1JUL - integer    - Julian day of date/time #1
c              J1HR - integer    - Hour of date/time #1
c              J2YR - integer    - Year of date/time #2
c             J2JUL - integer    - Julian day of date/time #2
c              J2HR - integer    - Hour of date/time #2
c
c --- OUTPUT:
c             JLENG - integer    - Difference (#2 - #1) in hours
c
c --- DELTT called by:  RDWRIT
c --- DELTT calls:      none
c-----------------------------------------------------------------------
c
      jmin=min0(j1yr,j2yr)
c
c --- find the number of hours between Jan. 1 of the "base" year and
c --- the first date/hour
      if(j1yr.eq.jmin)then
         j1=0
      else
         j1=0
         j1yrm1=j1yr-1
         do 10 i=jmin,j1yrm1
         if(mod(i,4).eq.0)then
            j1=j1+8784
         else
            j1=j1+8760
         endif
10       continue
      endif
      j1=j1+(j1jul-1)*24+j1hr
c
c --- find the number of hours between Jan. 1 of the "base" year and
c --- the second date/hour
      if(j2yr.eq.jmin)then
         j2=0
      else
         j2=0
         j2yrm1=j2yr-1
         do 20 i=jmin,j2yrm1
         if(mod(i,4).eq.0)then
            j2=j2+8784
         else
            j2=j2+8760
         endif
20       continue
      endif
      j2=j2+(j2jul-1)*24+j2hr
c
c --- compute the time difference (in hours)
      jleng=j2-j1

      return
      end
