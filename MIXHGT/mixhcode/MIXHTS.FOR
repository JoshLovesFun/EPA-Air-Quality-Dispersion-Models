      PROGRAM MIXHTS
C=====================================================================**
C     MIXING HEIGHTS:  A Mixing Height Computation Program for
C                      EPA Dispersion Models
C
C     VERSION:         98340
C
C     Purpose:  To compute morning and evening mixing heights
C               according to the technique presented by Holzworth (1967)
C               in J. Appl. Meteor., 6, 1039-1044.
C
C               The program reads upper air sounding data and surface
C               data (the format is the output format produced by the
C               Meteorological Processor for Regulatory Models (MPRM)
C               Stage 1 processor), and computes the morning and
C               evening mixing heights.
C
C     I/O units:     5      = USER INPUT (SEE BELOW)
C                    DEV13  = INPUT UNIT FOR UPPER AIR DATA (INITIALIZED
C                             to 13 IN THE COMMON BLOCKS)
C                    DEV22  = INPUT UNIT FOR SURFACE DATA (INITIALIZED
C                             to 22 IN THE COMMON BLOCKS)
C                    DEV40  = OUTPUT UNIT (INITIALIZED TO 40 IN THE
C                             common blocks)
C
C     Formats:       INPUT : REFER TO THE MPRM USER'S GUIDE
C                    OUTPUT: (A5,3I2,1X,I5,13X,I5)
C
C     Calls to: CHROND,GMTLST,GREG,ICHRND,JULIAN,READUA,INTRP2,LWRUPR
C
C     Initial Release:  April 1998
C
C     Revision History:  Formerly released as HOLZI in the non-EPA
C                        section of SCRAM
C                    
C
C-----------------------------------------------------------------------

C---- Variable declarations
      IMPLICIT     NONE
      INCLUDE      'mixhts.inc'

      INTEGER      ISTRG, IFC, MAXLEN
      PARAMETER    (ISTRG=6)
      PARAMETER    (MAXLEN=150)
      REAL         TMIN,TMAX,TMIN5,P1,P2,RATIO,AMMIX,PMMIX,
     &             TKELV,TTEMPN,TTEMPX,FCTR,SFCTH,
     &             SFSTPR, SFAMBT,
     &             PTEM(MAXLEV),  Z(MAXLEV),
     &             UAPR(MAXLEV),  UAHT(MAXLEV),  UATT(MAXLEV),
     &             UATD(MAXLEV),  UAWD(MAXLEV),  UAWS(MAXLEV)

      INTEGER      MRYR1, MRYR2, MRGMO1, MRGMO2, MRGDY1, MRGDY2, MRDAY1,
     &             MRDAY2, UAJDAY, UACHRON, SFJDAY, SFCHRON,
     &             ISTART, ISTOP, TODAY, ISAVYR, ISAVJUL, ISAVMO, ISAVDY
      INTEGER      UARED, SFRED, IAMMIX, IPMMIX, IFLAG
      INTEGER      UAGYR, UAGMO, UAGDY, UAGHR,
     &             SFGYR, SFGMO, SFGDY, SFGHR
      INTEGER      JULIAN, IWBAN, ISTATUA, ISTATSF, LEV,
     &             ILEV, NDAYS, NLEV, ISFLAG
      INTEGER      ISFHDR, JTODAY, IGYR, IGMON, IGDAY

      LOGICAL      UAEOF, SFEOF, UADAT, EXIST13, EXIST22, FIRSTW

      CHARACTER    CHSTR1*MAXLEN
      CHARACTER*48 FIL13, FIL22, FIL40, FIL30, FIL35
      CHARACTER    SFCTYP*ISTRG


C---- Data initialization
      DATA SFEOF/.FALSE./, UADAT/.FALSE./, FIRSTW/.TRUE./
      DATA TMIN/9999.0/, TMIN5/9999.0/, TMAX/-9999.0/
      DATA IAMMIX/-9999/, IPMMIX/-9999/, PTEM/MAXLEV*-9999.0/

      DATA UARED /0/, SFRED/0/, ISFLAG/0/
      DATA FIL30 /'MIXHTS.INP'/
      DATA FIL35 /'MIXHTS.LOG'/

C-----------------------------------------------------------------------
C    UARED,SFRED    = counters tracking the number of times the upper
C                     air and surface obs files are read
C    SFEOF          = end-of-file flags
C    UADAT          = indicates if a 12z sounding was found
C
C=======================================================================
C---- Open the log (unit 35) and input control (unit 30) files
      OPEN (UNIT=30, FILE=FIL30, STATUS='UNKNOWN')
      OPEN (UNIT=35, FILE=FIL35, STATUS='UNKNOWN')


C---- Prompt for file names; open the files; initialize variables

      WRITE (*,*) ' Enter the name of file of upper air data: '
      READ(5, '(A48 )') FIL13

      WRITE (*,*) ' Enter the name of file of surface obs: '
      READ(5, '(A48 )') FIL22

      WRITE (*,*) ' Enter the surface obs file type '
      WRITE (*,*) '              (SAMSON or HUSWO): '
      READ (5,*) SFCTYP
C---- Convert the file type to upper case for later checks on file type
      CALL LWRUPR(ISTRG,SFCTYP)
      IF( SFCTYP .NE. 'SAMSON'  .and.  SFCTYP .NE. 'HUSWO' )THEN
         WRITE(35, 3400) SFCTYP
         WRITE( 6, 3400) SFCTYP
         STOP
      ENDIF
 3400 FORMAT( ' Invalid surface data format specified: ',A6,/
     &        ' Valid data types are: SAMSON and HUSWO' )

      WRITE (*,*) ' Enter the output file name for mixing heights: '
      READ(5, '(A48 )') FIL40

      INQUIRE(FILE=FIL13, EXIST=EXIST13)
      INQUIRE(FILE=FIL22, EXIST=EXIST22)
      IF( EXIST13 .and. EXIST22 )THEN
         OPEN (UNIT=13, FILE=FIL13, STATUS='OLD')
         OPEN (UNIT=22, FILE=FIL22, STATUS='OLD')
      ELSE
         IF( .NOT. EXIST13 ) THEN
            WRITE(35, 3410) FIL13
            WRITE( 6, 3410) FIL13
         ENDIF
         IF( .NOT. EXIST22 ) THEN
            WRITE(35, 3410) FIL22
            WRITE( 6, 3410) FIL22
         ENDIF
         STOP
      END IF
 3410 FORMAT(/,'  The following input file could not be opened:',
     &       /,10X,A48)


      OPEN (UNIT=40, FILE=FIL40, STATUS='UNKNOWN')

      PRINT *, ' '
      PRINT *, ' FILES OPENED SUCCESSFULLY'
      PRINT *, '    SOUNDINGS  : ', FIL13
      PRINT *, '    SURFACE OBS: ', FIL22
      PRINT *, '    OUTPUT     : ', FIL40
      PRINT *, '    RESPONSES  : ', FIL30
      PRINT *, '    LOG        : ', FIL35
      PRINT *, ' '


C---- Write the names of the input and output data files to the response
C     (unit 30) and log (unit 35) files
      WRITE(30,300)FIL13,FIL22,SFCTYP,FIL40
      WRITE(35,300)FIL13,FIL22,SFCTYP,FIL40
  300 FORMAT(A48,T50,'Upper air soundings',/
     &       A48,T50,'Hourly surface observations',/
     &       A6, T50,'Surface obs data type',/
     &       A48,T50,'Output file' )


C---- Enter the 5 character station id (used only when writing the output
C     record in this program, but it is checked by PCRAMMET), the year
C     (2-digit), month, and day to start and end the computations,
C     and the number of hours from greenwich (+ for stations west of
C     greenwich).  This last entry is required to convert the data to
C     gmt. therefore, if the upper air data are given in local standard
C     time, enter the appropriate value; however if the data are already
C     given in gmt, enter a zero (0) for this value.

C---- Station ID
c     PRINT *,' Enter the station ID (5 characters): '
c     READ(5,'(A5)' ) STNID

c     WRITE(30,305 ) STNID
c 305 FORMAT(A5, T50, 'Station ID')

c     WRITE (35, 3500) STNID
c3500 FORMAT( ' Station ID: ', A5)

C---- Date to start processing
      PRINT *,' Enter 2-digit year, month, and day to start '
      PRINT *,'       the processing (free format, integer): '
      READ(5,*) MRYR1,MRGMO1,MRGDY1

      WRITE(30,310) MRYR1,MRGMO1,MRGDY1
  310 FORMAT(I4,2(',',I4), T50,'Date to begin processing')

      WRITE(35,3510) MRYR1,MRGMO1,MRGDY1
 3510 FORMAT(' Date to begin processing: ' I4,2('/',I4) )

C---- Date to stop processing
      PRINT *,' Enter 2-digit year, month, and day to stop '
      PRINT *,'       the processing (free format, integer): '
      READ(5,*) MRYR2,MRGMO2,MRGDY2

      WRITE(30,315) MRYR2,MRGMO2,MRGDY2
  315 FORMAT(I4,2(',',I4), T50,'Date to stop processing')

      WRITE(35,3515) MRYR2,MRGMO2,MRGDY2
 3515 FORMAT(' Date to stop processing : ' I4,2('/',I4) )

C     Set the factor to add to the minimum temperature to 5 Celsius
      FCTR = 5.0

      print *, ' '
      print *, ' '

C================================= Compute Julian and chronological days

C---- Compute the Julian and chronological days corresponding to the start date
      MRDAY1 = JULIAN(MRYR1,MRGMO1,MRGDY1)
      CALL CHROND( MRYR1,MRDAY1,ISTART )
      IF (ISTART .LT. 0) THEN
         WRITE( 6, 605)
         WRITE(35, 605)
  605    FORMAT(' Error computing chronological day of start date' )
         STOP
      ENDIF

C---- Compute the Julian and chronological days corresponding to the stop date
      MRDAY2 = JULIAN(MRYR2,MRGMO2,MRGDY2)
      CALL CHROND( MRYR2,MRDAY2,ISTOP )
      IF (ISTOP .LT. 0) THEN
         WRITE( 6, 610)
         WRITE(35, 610)
  610    FORMAT(' Error computing chronological day of stop date' )
         STOP
      ENDIF

C---- Compute the difference; if it exceeds 1 year, let the user know
      NDAYS = ISTOP - ISTART + 1
      IF( NDAYS .GT. 366 )THEN
         WRITE (35, 615 ) NDAYS
  615    FORMAT(/,' The number of days requested to process is ', i4,
     &          /,'    Note, however, that PCRAMMET only processes one',
     &            ' year of data',/)

      ELSEIF (NDAYS .LT. 0 )THEN
         WRITE ( 6, 616 ) NDAYS
         WRITE (35, 616 ) NDAYS
  616    FORMAT(/,' The number of days to process is < 0 (', i4, '). ',
     &            ' Is start date after stop date?',
     &          /,' STOPPING PROCESSING')
         STOP
      END IF


C================================== Read headers, if any, from each file

C     There are no headers to read from the upper air file, so read
C     from the file of hourly surface observations

      ISFHDR = 0
      IF( SFCTYP .EQ. 'SAMSON' )THEN
C        Read the first two records,which contains the station specific
C        data and list of variables that appear in the file
         READ (22,40,ERR=2210,END=2220) CHSTR1
         ISFHDR = ISFHDR + 1
         READ (22,40,ERR=2210,END=2220) CHSTR1
         CALL DEFINE( CHSTR1, MAXLEN, IFC )
         IF( IFC .NE. 26 )THEN
            WRITE( 6, 3450 )
            WRITE(35, 3450 )
 3450       FORMAT( /,'  Too few fields in the surface data file;',
     &              /,'  Return to the SAMSON CD, extract ALL fields',
     &                ' and try again' )
            STOP
         ELSE
            ISFHDR = ISFHDR + 1
         ENDIF
      ELSEIF( SFCTYP .EQ. 'HUSWO' )THEN
         READ (22,40,ERR=2210,END=2220) CHSTR1
         CALL DEFINE( CHSTR1, MAXLEN, IFC )
         IF( IFC .NE. 20 )THEN
            WRITE( 6, 3460 )
            WRITE(35, 3460 )
 3460       FORMAT( /,'  Too few fields in the surface data file;',
     &              /,'  Return to the HUSWO CD, extract ALL fields',
     &                ' and try again' )
            STOP
         ELSE
            ISFHDR = ISFHDR + 1
         ENDIF
      ENDIF
  40  FORMAT( A150 )

C============================ Read first sounding and hourly surface obs

C---- The program has to read data before it can make a comparison to
C     the processing (or computational) day


C---- Read and process the first observation of upper air data
      CALL READUA( IWBAN, UAGYR, UAGMO, UAGDY, UAGHR, NLEV, UAPR, UAHT,
     &             UATT, UATD, UAWD, UAWS, ISTATUA, LEV, IFLAG )

      IF( ISTATUA .EQ. 0 )THEN
C------- Sounding read successfully
         UARED = UARED + 1

C------- Compute the Julian day of the record just read and convert it
C        to the chronological day (1 = 1 January 1900)
         UAJDAY = JULIAN(UAGYR, UAGMO, UAGDY)
         CALL CHROND( UAGYR, UAJDAY, UACHRON )
         IF (UACHRON .LT. 0) THEN
            WRITE( 6, 620) UARED
            WRITE(35, 620) UARED
  620       FORMAT(' Error computing chronological day for sounding',i4)
            STOP
         ENDIF

      ELSE
C------- EOF on input file or sounding read UNsuccessfully -
C        go to error handler in either case
         GO TO 13100

      END IF


C---- Read and process the first observation of surface data
      CALL INITSF ( SFSTPR, SFAMBT )
C     READ(22,2265,IOSTAT=ISTATSF)
C    &              SFGYR, SFGMO, SFGDY, SFGHR, ISFSTPR, ISFAMBT

      CALL READSF ( SFCTYP, SFGYR, SFGMO, SFGDY, SFGHR, SFAMBT, SFSTPR,
     &              ISTATSF )
      IF( ISTATSF .EQ. 0 )THEN
C------- Observation read successfully
         SFRED = SFRED + 1

C------- Compute the julian day of the record just read, and convert
C        to chronological day
C        Note: NWS data are on 0-23 clock and hour 0 is equivalent to
C              hour 24 of the previous day; however, since the periods
C              of interest are 0200-0600 and 1200-1600, there is no
C              no need to convert hour 0 to hour 24 in this program
         SFJDAY = JULIAN(SFGYR,SFGMO,SFGDY)

         CALL CHROND( SFGYR,SFJDAY,SFCHRON )
         IF (SFCHRON .LT. 0) THEN
            WRITE( 6, 625)
            WRITE(35, 625)
  625       FORMAT(' Error computing chronological day at 1st sfc obs')
            STOP
         ENDIF

      ELSE
C------- EOF on input file or observation read UNsuccessfully -
C        go to error handler in either case
         GO TO 22100

      END IF

 2265 FORMAT(1X,4I2,13X,I5,/,T46,I5)


C=============================================== BEGIN ITERATION ON DAYS

      DO 9000 TODAY = ISTART, ISTOP

C------- Compute the current day from the chronological day
         CALL ICHRND( TODAY, ISAVYR, ISAVJUL )
         CALL GREG( ISAVYR, ISAVJUL, ISAVMO, ISAVDY )

C------- Initialize mixing heights for this day
         IAMMIX = -9999
         IPMMIX = -9999

C-------------------------------------------> Process the upper air data
13000    CONTINUE


C------- Check the date of the sounding currently in the arrays;
C        if it is before the processing day, then read another sounding
C        if it is the same day, is it the 1200 GMT sounding?
C        if it is after the processing day, hold and move on to surface obs

         IF( UAEOF ) GO TO 8500

         IF (UACHRON .LT. TODAY) THEN
C---------- The sounding day is before the day being processed;
C           read the next sounding (initialization is in subr.readua)
            CALL READUA( IWBAN, UAGYR, UAGMO, UAGDY, UAGHR, NLEV, UAPR,
     &           UAHT, UATT, UATD, UAWD, UAWS, ISTATUA, LEV, IFLAG )

            IF( ISTATUA .EQ. 0 )THEN
C------------- Sounding read successfully
               UARED = UARED + 1

C------------- Compute the Julian day of the record just read and convert
C              it to the chronological day (1 = 1 January 1900)
               UAJDAY = JULIAN(UAGYR, UAGMO, UAGDY)
               CALL CHROND( UAGYR, UAJDAY, UACHRON )
               IF (UACHRON .LT. 0) THEN
                  WRITE( 6, 630 ) UARED, ISAVYR,ISAVMO, ISAVDY
                  WRITE(35, 630 ) UARED, ISAVYR,ISAVMO, ISAVDY
  630             FORMAT(' Error computing chronological day at'
     &                   ' sounding ', I4, ' on (yy-mm-dd) ',
     &                   I2.2,2('-',I2.2))
                  STOP
               ENDIF


            ELSEIF( ISTATUA .GT. 0 )THEN
C------------- Sounding read UNsuccessfully - go to error handler
               GO TO 13100

            ELSEIF( ISTATUA .LT. 0 )THEN
C------------- EOF reading sounding - set flag
               UAEOF = .TRUE.
               IF( IFLAG .EQ. 1 )THEN
C---------------- EOF reading a header record: may be ok or abnormal
                  WRITE(35,3550) UARED+1, ISAVYR, ISAVMO, ISAVDY
 3550             FORMAT(' EOF reading header for sounding ',I5,
     &                   ' on (yy-mm-dd) ', I2.2,2('-',I2.2) )
               ELSEIF( IFLAG .EQ. 2 )THEN
C---------------- EOF reading a level from the sounding: abnormal
                  WRITE(35,3555) UARED+1, LEV, ISAVYR,ISAVMO, ISAVDY
 3555             FORMAT(' EOF reading data for sounding ',I5,
     &                   ' ,  level ', I3,
     &                   ' on (yy-mm-dd) ',I2.2,2('-',I2.2) )
               ENDIF

            END IF

            GO TO 13000

C-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
         ELSE IF( UACHRON .EQ. TODAY )THEN
C---------- The sounding day is equal to the computational day

            IF( UAGHR .GE. 10  .AND.  UAGHR .LE. 15 ) THEN
C------------- The upper air day is equal to the computational day;
C              and the sounding time is in the five-hour window from
C              1000 - 1500 GMT (same as NCDC)

               IF ( NLEV .GT. 0 ) THEN
C---------------- And it appears that there are data in the sounding
                  UADAT = .TRUE.

               ELSE
C---------------- A sounding at the right time but there are no data
                  UADAT = .FALSE.
               END IF

            ELSE
C------------- Right day, wrong time; read another sounding
               CALL READUA( IWBAN, UAGYR, UAGMO, UAGDY, UAGHR, NLEV,
     &              UAPR, UAHT, UATT, UATD, UAWD, UAWS, ISTATUA, LEV,
     &              IFLAG )

               IF( ISTATUA .EQ. 0 )THEN
C---------------- Sounding read successfully
                  UARED = UARED + 1

C---------------- Compute the Julian day of the record just read and
C                 convert it to the chronological day
                  UAJDAY = JULIAN(UAGYR, UAGMO, UAGDY)
                  CALL CHROND( UAGYR, UAJDAY, UACHRON )
                  IF (UACHRON .LT. 0) THEN
                     WRITE( 6, 630 ) UARED, ISAVYR,ISAVMO, ISAVDY
                     STOP
                  ENDIF


               ELSEIF( ISTATUA .GT. 0 )THEN
C---------------- Sounding read UNsuccessfully - go to error handler
                  GO TO 13100

               ELSEIF( ISTATUA .LT. 0 )THEN
C---------------- EOF reading sounding - set flag
                  UAEOF = .TRUE.
                  IF( IFLAG .EQ. 1 )THEN
C------------------- EOF reading a header record: may be ok or abnormal
                     WRITE(35, 3550) UARED+1, ISAVYR, ISAVMO, ISAVDY
                  ELSEIF( IFLAG .EQ. 2 )THEN
C------------------- EOF reading a level from the sounding: abnormal
                     WRITE(35,3555) UARED+1, LEV, ISAVYR,ISAVMO, ISAVDY
                  ENDIF

               END IF

C------------- Go back to check the sounding date and time
               GO TO 13000

            END IF

C-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
         ELSE IF( UACHRON .GT. TODAY )THEN
C---------- The sounding is after the computational day

            UADAT = .FALSE.
         ENDIF


C--------------------------------------> Process the hourly surface data
22000    CONTINUE

C------- Whereas the search was for a single upper air sounding, we now
C        need all the hourly surface observations for a single day

         IF( SFEOF ) GO TO 8500

         IF( SFCHRON .LT. TODAY )THEN
C---------- The surface obs. day is before the computational day; read
C           the next hourly observation

            CALL INITSF(  SFSTPR, SFAMBT )
            CALL READSF ( SFCTYP, SFGYR, SFGMO, SFGDY, SFGHR, SFAMBT,
     &                    SFSTPR, ISTATSF )

            IF( ISTATSF .EQ. 0 )THEN
               SFRED = SFRED + 1
               SFJDAY = JULIAN(SFGYR,SFGMO,SFGDY)
               CALL CHROND( SFGYR,SFJDAY,SFCHRON )

               IF (SFCHRON .LT. 0) THEN
                  WRITE( 6, 635 ) SFRED, ISAVYR,ISAVMO, ISAVDY
                  WRITE(35, 635 ) SFRED, ISAVYR,ISAVMO, ISAVDY
  635             FORMAT(' Error computing chronological day for',
     &                   ' surface obs', I4, ' on (yy-mm-dd) ',
     &                   I2.2,2('-',I2.2))
                  STOP
               ENDIF

            ELSEIF( ISTATSF .GT. 0 )THEN
C------------- Reading surface obs was UNsuccessful
               GO TO 22100

            ELSEIF( ISTATSF .LT. 0 )THEN
C------------- End of file encountered
               SFEOF = .TRUE.
               WRITE(35,3560) SFRED, ISAVYR, ISAVMO, ISAVDY
 3560          FORMAT(' EOF reading surface data after obs ',I5,
     &                ' on (yy-mm-dd) ',I2.2,2('-',I2.2))

            END IF

            GO TO 22000

C-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
         ELSEIF( SFCHRON .EQ. TODAY )THEN
C---------- Only the date and hour and temperature and pressure data are
C           needed to comute the twice daily mixing heights.
C           Temperature must be in degrees Celsius, pressure in millibars;
C           missing values are denoted by -9999

C---------- As each hour of data is read, check the minimum
C           temperature (TTEMPN) for the day between the hours of
C           0200 LST and 0600 LST, inclusive

            IF ((SFGHR .GE. 2) .AND. (SFGHR .LE. 6)) THEN
               IF (SFAMBT .GT. -9000.0) THEN
                  TTEMPN = SFAMBT + 273.16
                  IF (TTEMPN .LT. TMIN) THEN
                     TMIN = TTEMPN
                     P1   = SFSTPR
                  ENDIF
               ENDIF

C---------- As each hour of data is read, determine the maximum
C           temperature (TTEMPX) for the day between the hours of
C           1200 LST and 1600 LST, inclusive
C
            ELSE IF ((SFGHR .GE. 12) .AND. (SFGHR.LE.16)) THEN
               IF (SFAMBT .GT. -9000.0) THEN
                  TTEMPX = SFAMBT + 273.16
                  IF (TTEMPX .GT. TMAX) THEN
                     TMAX = TTEMPX
                     P2   = SFSTPR
                  ENDIF
               ENDIF

            ENDIF

C---------- If an end of file was not read on the last read, read and
C           process the next observation; this structure allows the
C           program to compute mixing heights as long as the hour in the
C           surface obs file was 16 or greater
            IF( .NOT. SFEOF )THEN

               CALL INITSF(  SFSTPR, SFAMBT )
               CALL READSF ( SFCTYP, SFGYR, SFGMO, SFGDY, SFGHR, SFAMBT,
     &                       SFSTPR, ISTATSF )

C------------- Determine status of read: 0 => ok, negative => end of file,
C              positive => error
               IF( ISTATSF .EQ. 0 )THEN
                  SFRED = SFRED + 1
                  SFJDAY = JULIAN(SFGYR,SFGMO,SFGDY)
                  CALL CHROND( SFGYR,SFJDAY,SFCHRON )

                  IF (SFCHRON .LT. 0) THEN
                     WRITE( 6, 635 ) SFRED, ISAVYR,ISAVMO, ISAVDY
                     STOP
                  ENDIF

               ELSEIF( ISTATSF .GT. 0 )THEN
                  GO TO 22100

               ELSEIF( ISTATSF .LT. 0 )THEN
C---------------- End-of file; if there is sufficient data to compute the
C                 daily mixing heights, proceed with the calculations;
C                 otherwise, stop processing

                  SFEOF = .TRUE.
                  WRITE(35,3560) SFRED, ISAVYR, ISAVMO, ISAVDY

                  IF( SFGHR .GE. 16 )THEN
C------------------- There are sufficient hourly obs to calculate BOTH
C                    mixing heights for this day
                     GO TO 8000
                  ELSE
                     ISFLAG = 1
                  END IF

               END IF

C------------- Go up and check the date and temperatures
               GO TO 22000
            END IF

C-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
         ELSE IF (SFCHRON .GT. TODAY) THEN
C---------- Nothing to do here;
C           counters are incremented, data for an hour after the
C           computational day are in the surface obs array, and
C           the check for morning and afternoon temperature
C           and pressure are made below.

         ENDIF


C-------------------------------> Compute the twice-daily mixing heights

 8000    CONTINUE

         WRITE( *, 6000 ) ISAVYR, ISAVMO, ISAVDY
 6000    FORMAT( '+   Now processing: ', I2.2, 2('-',I2.2) )

C------- If there are upper air data for the day, compute the potential
C        temperature profile then interpolate the minimum and maximum
C        (potential) temperatures to determine the morning and evening
C        mixing heights.  The interpolation is equivalent to finding
C        the intersection of the surface dry adiabat with the sounding

         IF ( UADAT ) THEN
            DO 350 ILEV = 1,NLEV
               IF ((UAPR(ILEV) .GT. -900.0)  .AND.
     &             (UATT(ILEV) .GT. -900.0) ) THEN
                  RATIO      = 1000.0/UAPR(ILEV)
                  TKELV      = UATT(ILEV) + 273.16
                  PTEM(ILEV) = TKELV*(RATIO**0.286)
                  Z(ILEV)    = UAHT(ILEV)

               ELSE
                  PTEM(ILEV) = -9999.0
               ENDIF
  350       CONTINUE

         ELSE
C---------- No sounding data
            WRITE(35,3565) ISAVYR, ISAVMO, ISAVDY
 3565       FORMAT(' No sounding data for (yy-mm-dd) ',
     &            I2.2,2('-',I2.2) )
            GO TO 8500
         ENDIF

C------- Interpolate the min and max temperatures to find the mixing hts.
C        NOTE: An error condition will occur if the potential temperature
C           at the first level is less than the surface potential
C           temperature in the sounding; an error condition will
C           occur if the maximum potential temperature is greater
C           than any potential temperature in the sounding, i.e.,
C           the dry adiabat defined by the surface temperature does
C           not intersect the sounding.

         IF ((TMIN .LT. 900.0) .AND. (P1 .GT. -900.0)) THEN
C---------- Temperature and pressure are not missing; compute the
C           morning surface potential temperature and mixing height
            TMIN5 = TMIN + FCTR
            SFCTH = TMIN5 * (1000.0/(UAPR(1)))**0.286
            CALL INTRP2(NLEV, PTEM, Z, SFCTH, AMMIX, IFLAG)
            IF (IFLAG .NE. 0) THEN
               WRITE(35,655) ISAVYR, ISAVMO, ISAVDY
               AMMIX = -9999.0

               IF( IFLAG .EQ. 1 )THEN
C---------------- Surface potential temp < all sounding potential temp
                  WRITE( 35,656 )
               ELSEIF( IFLAG .EQ. 2 )THEN
C---------------- Upper height undefined in the interpolation
                  WRITE (35,657 )
               ELSEIF( IFLAG .EQ. 3 )THEN
C---------------- Lower height undefined in the interpolation
                  WRITE (35,658 )
               ENDIF

            ENDIF

  655       FORMAT( ' Could not compute morning mixing height on',
     &                 ' (yy-mm-dd) ', I2.2, 2('-',I2.2) )
  656       FORMAT( '    Sfc potential temperature < potential'
     &              ' temperature of 1st level in sounding')
  657       FORMAT( '    Could not define upper height in sounding to'
     &              ' use in the interpolation of the',/,'    surface'
     &              ' potential temperature')
  658       FORMAT( '    Could not define lower height in sounding to'
     &              ' use in the interpolation of the',/,'    surface'
     &              ' potential temperature')

         ELSE
            WRITE(35,660) ISAVYR, ISAVMO, ISAVDY, tmin, p1
  660       FORMAT(' Missing  morning  temperature or pressure on ',
     &             I2.2, 2('-',I2.2), ';  T = ', F8.2,', P = ',F8.2)
            AMMIX = -9999.0

         ENDIF

         IF ((TMAX .GT. -900.0) .AND. (P2 .GT. -900.0)) THEN
C---------- Temperature and pressure are not missing; compute the
C           afternoon surface potential temperature and mixing height
C           In reading the sounding data, the program is limited to MAXLEV
C           levels; therefore, the call to INTRP2 is similarly limited

            SFCTH = TMAX * (1000.0/UAPR(1))**0.286

            CALL INTRP2( MIN(MAXLEV,NLEV),PTEM,Z,SFCTH,PMMIX,IFLAG )
            IF (IFLAG .NE. 0) THEN
               WRITE(35,665) ISAVYR, ISAVMO, ISAVDY
  665          FORMAT( ' Could not compute afternoon mixing height on',
     &                 ' (yy-mm-dd) ', I2.2, 2('-',I2.2) )
               PMMIX = -9999.

               IF( IFLAG .EQ. 1 )THEN
C---------------- Surface potential temp < all sounding potential temp
                  WRITE( 35,656 )
               ELSEIF( IFLAG .EQ. 2 )THEN
C---------------- Upper height undefined in the interpolation
                  WRITE (35,657 )
               ELSEIF( IFLAG .EQ. 3 )THEN
C---------------- Lower height undefined in the interpolation
                  WRITE (35,658 )
               ENDIF

            ENDIF


         ELSE
            WRITE(35,670) ISAVYR, ISAVMO, ISAVDY, tmax, p2
  670       FORMAT( ' Missing afternoon temperature or pressure on ',
     &              I2.2, 2('-',I2.2), ';  T = ', F8.2,', P = ',F8.2)
            PMMIX = -9999.

         ENDIF

C------- Round the mixing heights to the nearest whole integer
         IF (AMMIX .GT. 0.0) IAMMIX = AMMIX +0.5
         IF (PMMIX .GT. 0.0) IPMMIX = PMMIX +0.5

 8500    CONTINUE

C----------------------------------> Write mixing heights to output file

         JTODAY = JULIAN(ISAVYR, ISAVMO, ISAVDY )
         IF( FIRSTW .AND. JTODAY .EQ. 1 )THEN
C---------- First time writing to the output file and it is January 1 -
C           duplicate the data to be compatible with data on SCRAM
            IGYR = ISAVYR -1
            IGMON = 12
            IGDAY = 31
            WRITE(40,4000) IWBAN, IGYR, IGMON, IGDAY, IAMMIX, IPMMIX
            FIRSTW = .FALSE.
         END IF

         WRITE(40,4000) IWBAN, ISAVYR, ISAVMO, ISAVDY, IAMMIX, IPMMIX
 4000    FORMAT(I5,3I2,1X,I5,13X,I5)



C------- Reinitialize data for next computational day
C        ! don't initialize sfc obs - next hour's data already in place

         IF( UADAT )THEN
            DO 5000 ILEV = 1,MAXLEV
               PTEM(ILEV) = -9999.0
 5000       CONTINUE

            UADAT = .FALSE.
            NLEV = 0
         END IF

C------- Initialize minimum and maximum tmeperatures
         TMIN =  9999.0
         TMAX = -9999.0


 9000 CONTINUE

C---- Exiting the computational day loop - duplicate the last day
C     processed if it is December 31

      IF( ISAVMO .EQ. 12  .AND.  ISAVDY .EQ. 31 )THEN
         WRITE(40,4000) IWBAN, ISAVYR, ISAVMO, ISAVDY, IAMMIX, IPMMIX
      ENDIF

      STOP

C-------------------------------------- Error and end-of-file conditions

C---> Reading upper air data


13100 CONTINUE

      IF( ISTATUA .EQ. -99 )THEN
C        Error sorting a sounding
         WRITE(35,3580) ISAVYR,ISAVMO, ISAVDY
 3580    FORMAT( ' ERROR sorting upper air arrays on (yy-mm-dd) ',
     &             I2.2, 2('-',I2.2) )

      ELSEIF( ISTATUA .LT. 0 )THEN
C------- EOF encountered reading first sounding: abnormal termination
         IF( IFLAG .EQ. 1 )THEN
C---------- EOF reading a header record from the first sounding
            WRITE(35,3590) UARED+1, ISAVYR,ISAVMO, ISAVDY
 3590       FORMAT( ' EOF reading headers for sounding ',I5,
     &              ' on (yy-mm-dd) ', I2.2, 2('-',I2.2) )
         ELSEIF( IFLAG .EQ. 2 )THEN
C---------- EOF reading a level from the first sounding
            WRITE(35 3595) UARED+1, ISAVYR,ISAVMO, ISAVDY
 3595       FORMAT(' EOF reading data for sounding ',I5,
     &             ' ,  level ', I3,
     &             ' on (yy-mm-dd) ', I2.2, 2('-',I2.2) )
         ENDIF

      ELSEIF( ISTATUA .GT. 0 )THEN
C------- Error encountered reading the sounding data
         IF( IFLAG .EQ. 1 )THEN
C---------- Error reading a header record
            WRITE(35,3600) UARED+1, ISAVYR,ISAVMO, ISAVDY
 3600       FORMAT( ' Error reading headers for sounding ',I5,
     &              ' on (yy-mm-dd) ', I2.2, 2('-',I2.2) )

         ELSEIF( IFLAG .EQ. 2 )THEN
C---------- Error reading a level from the sounding
            WRITE(35,3605) UARED+1, LEV, ISAVYR,ISAVMO, ISAVDY
 3605       FORMAT(' Error reading data for sounding ',I5,
     &             ' ,  level ', I3,
     &             ' on (yy-mm-dd) ', I2.2, 2('-',I2.2) )
         ENDIF

      END IF

      STOP


C---> Reading surface observations

 2210 WRITE(35,3610) ISFHDR,SFCTYP
 3610 FORMAT( ' Error reading surface obs file while reading ',A6,
     &         ' header records after record ', i5 )
      STOP

 2220 WRITE(35,3615) ISFHDR,SFCTYP
 3615 FORMAT(' EOF reading surface obs file while reading ',A6,
     &         ' header records after record ', i5 )
      STOP

22100 CONTINUE

      IF( ISTATSF .LT. 0 )THEN
C------- End--of-file encountered
         WRITE(35,3620) SFRED, ISAVYR, ISAVMO, ISAVDY
 3620    FORMAT( ' EOF reading surface data after obs ',i5,
     &           ' on (yy-mm-dd) ', i2.2, 2('-',i2.2) )
         IF( ISFLAG .EQ. 1 )THEN
            WRITE(35, 3625)
 3625       FORMAT(' Mixing heights not calculated on last day'
     &               ' due to insufficient data in surface obs' )
         END IF
      ELSEIF( ISTATSF .GT. 0 )THEN
C------- Error encountered reading the sounding data
         WRITE(35, 3630) SFRED+1, ISAVYR, ISAVMO, ISAVDY
 3630    FORMAT( ' Error reading surface data for obs ',i5,
     &           ' on (yy-mm-dd) ', i2.2, 2('-',i2.2) )
      END IF

      STOP

      END
