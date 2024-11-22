      PROGRAM PCRAMMET
C***********************************************************************
C*    PCRAMMET    A Meteorological Pre-processor for EPA Dispersion Models
C*
C*    VERSION :   99169
C*
C*    PURPOSE:    Reads raw meteorological data - surface, mixing height
C*                and precipitation data from a variety of NCDC/SCRAM
C*                formats and prepares unformatted or ASCII met data file
C*                for EPA dispersion models.
C*
C*    PROGRAMMER: Jayant Hardikar, Jim Paumier
C*                PES Inc.
C*
C*    DATE:       August 15, 1995
C*
C*    INPUTS:     Surface data       - CD144 or SAMSON or SCRAM
C*                Mixing Height data - SCRAM expanded format
C*                Precipitation data - SAMSON or TD3240
C*                Other user options
C*
C*    OUTPUTS:    Unformatted or ASCII meteorological data ready for
C*                EPA dispersion models.
C*
C*    ASSUMPTIONS:
C*                All hours are contiguous
C*                All input files have same date/time
C*                24 hours of data per day
C*
C*    REVISIONS:
C*      06/30/98  Added necessary code to process Hourly United States
C*                Weather observations (HUSWO) data retrieved from CD.
C*                NOTE: Since the data retrieved from CD can either be 
C*                      in English or metric units and since the
C*                      default is English and since there is no 
C*                      indication of the units in the data file, 
C*                      all processing in PCRAMMET will assume English 
C*                      units.  This assumption is consistent with 
C*                      the processing that is performed in the program
C*                      that computes the twice daily mixing heights.
C*
C*     06/18/99  Added code to write a message to log file when 
C*               processing ASOS data begins
C*
C*               Made corrections for Y2K (subr. HR0024); these
C*               corrections will work only through the year 2049
C*
C*               Removed all conditional compile statements (Lahey 
C*               Fortran 90 does not support conditional compiling)
C*
C***********************************************************************
C*
C*    Variable Declarations
      INCLUDE 'PCRAM.INC'
      INTEGER ISAMWX(9), IHUSWX(4), IP5(5), NPROC

      DATA NPROC /0/

C*    Process the command line options
      CALL GETCMD (IFCNT)

C*    Get user options
      CALL SETUP
      WRITE(*,*) ' '

C*    Process Input and Output Files
      CALL FILES

C*    Begin Loop Over Days (Use the End-of-File in Surface
C*    Data as the indicator of the processing period)
      IDAY = 0
      EOFSFC = .FALSE.
      DO WHILE (.NOT. EOFSFC)
         IDAY = IDAY + 1

C*       Initialize data to missing
         CALL VARINI

C*       Read Surface Data For This Day (24 Hours)
         CALL READSF (NREAD)
         NPROC = NPROC + NREAD

         IF (NREAD .EQ. 24 )THEN
C*          24 hours of surface observations were read, continue

C*          Read Mixing Height Data For This Day (AM/PM)
            CALL READMX

C*          Read Hourly Precipitation Data For This Day (24 Hours)
            CALL READPP

C*          Fill Up The Appropriate Meteorological Arrays, 
C*          including the date and time
            CALL FILMET (NREAD)

            Write (*,500 ) ISYR(1),ISMO(1),ISDY(1)

C*          Determine the Julian day for the current date;
C*          use the year, month and day from the first hour of the day
            CALL JULIAN (ISYR(1),ISMO(1),ISDY(1),JDAY,0)

C*          Calculate Sunrise/Sunset Times For This Day as well as
C*          the solar elevation for all hours of this day
            CALL SUN(JDAY)

C*          Begin Loop over All Hours For This Day
            DO 100 IHR = 1, 24

C*             Calculate date-hr integer (YYJJJHH)
               KDATHR = ISYR(IHR)*100000 + JDAY*100 + IHR

C*             Check Date/Time Stamps Between All Types Of Data
               CALL CHKDAT(IHR)

C*             Determine If The Hour Is 'Calm'
               CALL CHKCLM(IHR)

C*             Calculate Flow Vector (Randomized and
C*             Non Randomized) from Wind Direction
               CALL FLOVEC(IHR,JDAY)

C*             Convert the precipitation codes if WETFLG is set
               IF( WETFLG )THEN
                  IF( SFCTYP .EQ. 'CD144' .OR. SFCTYP .EQ. 'SCRAM' )THEN
C*                   Convert the CD144 codes to the precip code for the hour
                     DO 40 I = 1,5
                        IP5(I) = IPREC(I,IHR)
   40                CONTINUE
                     CALL PCODES(IDIAG,KDATHR,IP5,IPCODE(IHR),
     &                           NOWARN,SFCTYP)

                  ELSEIF (SFCTYP .EQ. 'SAMSON') THEN
C*                   Convert SAMSON Precip Codes to CD144 Types
                     DO 50 JX = 1,9
                        ISAMWX(JX) = IWXSAM(JX,IHR)
   50                CONTINUE

                     CALL SAMWX(ISAMWX,IP5)
                     CALL PCODES(IDIAG,KDATHR,IP5,IPCODE(IHR),
     &                           NOWARN,SFCTYP)

                  ELSEIF (SFCTYP .EQ. 'HUSWO') THEN
C*                   Convert HUSWO Precip Codes to CD144 Types

                     DO 55 JX = 1,4
                        IHUSWX(JX) = IWXHUS(JX,IHR)
   55                CONTINUE
                     CALL HUSWX(IHUSWX,IP5)
                     CALL PCODES(IDIAG,KDATHR,IP5,IPCODE(IHR),
     &                           NOWARN,SFCTYP)

                  ENDIF

               ELSE
                  IPCODE(IHR) = 0

               ENDIF

C*             Convert all Data to Appropriate Units
               CALL UNITS(IHR)

C*             Calculate the Stability Class For This Hour
               CALL STABIT(IDAY,IHR)

C*             Call Interpolated Mixing Height For This Hour
               CALL MIXHIT(IHR)

C*             Calculate U* And L For This Hour When Applicable
               IF( DRYFLG  .OR.  WETFLG )THEN
                  CALL USTARL(IHR)
               ENDIF

C*          End Loop over All Hours For This Day
100         CONTINUE

C*          Write Out Data for All Hours of This Day
            CALL WRITIT (JDAY)

         ELSE
            IF( NREAD .GT. 0  )THEN
               WRITE(IDIAG, 600) NREAD, ISYR(1),ISMO(1),ISDY(1)
  600          FORMAT( ' WARNING: ONLY',I3,' HRS DATA ON THE',
     &                 ' DAY FOLLOWING ',3I3,
     &               /,'          NO DATA WERE PROCESSED FOR THE',
     &                 ' ENTIRE DAY')
            ENDIF
         ENDIF

C*    End Loop Over Days
      END DO

C*    Write the # of hours processed
      IF( NREAD .EQ. 24 )THEN
         WRITE( IDIAG,750 )NPROC,SFCTYP
      ELSE
         WRITE( IDIAG,750 )NPROC-NREAD,SFCTYP
      ENDIF

      IF( IFCNT .LT.2 )THEN
         WRITE ( *, 1100 )
      ENDIF

C---- NOWARN was initialized as TRUE; if any warning message was written
C     to the log file, then NOWARN was set to FALSE
      WRITE( * ,760 )
      IF( .NOT. NOWARN )THEN
         WRITE( *,765 )
      ENDIF

C*    End Program
      STOP

  500 FORMAT( '+ Now processing day (yy/mm/dd) ' , 3(i2.2:,'/') )
  750 FORMAT( /,' PCRAMMET processed ',i6,' hours of ',a6,' data'/)
  760 FORMAT(//,4X, ' See the file PCRAM.LOG for a review of this run ')
  765 FORMAT( /,4X, ' *** Warning messages were generated by PCRAMMET',
     &              ' and are in PCRAM.LOG' )
 1100 FORMAT( //,' Your responses to the program prompts have been',
     &           ' written to the file "INPUTS."' )
      END


      SUBROUTINE GETCMD (IFCNT)
C***********************************************************************
C*    GETCMD Module of PCRAMMET Meteorological Pre-processor
C*
C*    PURPOSE: Controls Retrieving Input and Output File Names From
C*             the Command Line for PCs, and OPENs the Files
C*
C*    PROGRAMMER: Roger Brode
C*
C*    DATE:       March 2, 1992
C*
C*    MODIFIED:   February 13, 1995
C*                Adapted for PCRAMMET to allow for zero
C*                command line parameters
C*
C*    INPUTS:  Command Line
C*
C*    OUTPUTS: Input Runstream File Name
C*             Output Print File Name
C*
C*    CALLED FROM:   MAIN PROGRAM
C***********************************************************************
C
C*    Variable Declarations
      INCLUDE 'PCRAM.INC'

C*    Declare the COMLIN Variable to Hold Contents of Command Line for Lahey
      CHARACTER*120 COMLIN
      INTEGER*2 LOCB(120),LOCE(120)
      LOGICAL INFLD

C*    Initialize variables
      COMDLN = .FALSE.

C*    Use Lahey Function GETCL To Retrieve Contents of Command Line.
C*    Retrieve Input and Output File Names From the COMLIN Variable.
      CALL GETCL(COMLIN)
      INFLD = .FALSE.
      IFCNT = 0
      DO 100 I = 1, 120
         IF (.NOT.INFLD .AND. COMLIN(I:I) .NE. ' ') THEN
            INFLD = .TRUE.
            IFCNT = IFCNT + 1
            LOCB(IFCNT) = I
         ELSE IF (INFLD .AND. COMLIN(I:I) .EQ. ' ') THEN
            INFLD = .FALSE.
            LOCE(IFCNT) = I - 1
         END IF
 100  CONTINUE

C*    If no command line arguments specified, set the logical
C*    flag to false, else set it to true and pass back the
C*    file names

      IF (IFCNT .EQ. 0) THEN
         COMDLN = .FALSE.
         INPFIL = ' '
         OUTFIL = ' '
      ELSE IF (IFCNT .EQ. 2) THEN
         COMDLN = .TRUE.
         INPFIL = COMLIN(LOCB(1):LOCE(1))
         OUTFIL = COMLIN(LOCB(2):LOCE(2))
      ELSE

C*       Error on Command Line.  Write Error Message and STOP
         COMDLN = .FALSE.
         WRITE(*,660)
         STOP

      END IF

660   FORMAT(' COMMAND ERROR: PCRAMMET [input_control output_for_model]'
     &    ,/,'                          __________ optional __________')

      RETURN
      END


      SUBROUTINE SETUP
C***********************************************************************
C*    SETUP Module of PCRAMMET Meteorological Pre-processor  
C*
C*    PURPOSE:    Obtains user inputs needed for the PCRAMMET run - either
C*                from the file specified on the command line or the screen.
C*
C*    PROGRAMMER: Jayant Hardikar
C*                PES Inc.
C*
C*    DATE:       February 13, 1995
C*
C*    INPUTS:     Command Line Logical Flag and Filenames if Applicable
C*
C*    OUTPUTS:    Various input parameters
C*
C*    CALLED FROM:  MAIN program
C***********************************************************************
C*
C*    Variable Declarations
      INCLUDE 'PCRAM.INC'

C*    Open Program Diagnostics Output File and Write Info
      OPEN (IDIAG,FILE='PCRAM.LOG')
      WRITE (IDIAG,7000) VERSON
7000  FORMAT (30X,'********************',/,
     &        30X,'      PCRAMMET      ',/,
     &        30X,'   VERSION: ',A5     ,/,
     &        30X,'********************',/)

C*    If the User Specified Files on the Command Line,
C*    Then Open the Input File and Read Data from it.
      IF (COMDLN) THEN
         OPEN (INOPT,FILE=INPFIL,STATUS='OLD',ERR=1001)
         CALL READIN(INOPT)

      ELSE
C*       Otherwise, Read Get Inputs from Screen
         CALL READIN(-9)
      ENDIF
      GO TO 999

C*    Error Processing
1001  CALL ERRHDL('E','1001',0)

999   RETURN
      END


      SUBROUTINE READIN(INFROM)
C***********************************************************************
C*    READIN Module of PCRAMMET Meteorological Pre-processor
C*
C*    PURPOSE:    Reads user inputs needed for the PCRAMMET run - either
C*                from the file specified on the command line or the
C*                screen.
C*
C*    PROGRAMMER: Jayant Hardikar, Jim Paumier
C*                PES Inc.
C*
C*    DATE:       August 15, 1995
C*
C*    INPUTS:     Unit Number of File to Read From. A '-9' Means Read
C*                from Screen
C*
C*    OUTPUTS:    Various input parameters
C*
C*    CALLED FROM: SETUP
C***********************************************************************
C*
C*    Variable Declarations
      INCLUDE 'PCRAM.INC'
      CHARACTER*40 ANSWER
      LOGICAL      XFILE

      IF (INFROM .EQ. -9) THEN

C*       Open the file to capture the user's responses
         OPEN (INEW,FILE='INPUTS',STATUS='unknown',ERR=1002)

C*       Read Inputs From Screen

C*----------------------------------------------------------------------
    1    WRITE (*,*) 'Will you be making any dry or wet ',
     &               'deposition calculations? '
         WRITE (*,*) 'None / Dry / Wet : '
         READ  (*,'(A)',ERR=1) ANSWER
         CALL LWRUPR (ANSWER)

         IF (ANSWER .EQ. 'DRY'  .OR.  ANSWER .EQ. 'D' )THEN
            DRYFLG = .TRUE.
            WETFLG = .FALSE.
         ELSE IF (ANSWER .EQ. 'WET'  .OR.  ANSWER .EQ. 'W' )THEN
            DRYFLG = .FALSE.
            WETFLG = .TRUE.
         ELSE IF (ANSWER .EQ. 'NONE'  .OR.  ANSWER .EQ. 'N' )THEN
            DRYFLG = .FALSE.
            WETFLG = .FALSE.
         ELSE
            WRITE (*,*) 'Invalid response, please answer appropriately'
            WRITE (*,*)
            GO TO 1
         ENDIF
         WRITE (INEW,110) ANSWER
  110    FORMAT( A,T41,' No/Dry/Wet Deposition calculations')

C*----------------------------------------------------------------------
    2    WRITE (*,*) 'Enter the OUTPUT filename: '
         READ  (*,'(A)',IOSTAT=IOS) OUTFIL
         IF( IOS .NE. 0 )THEN
            WRITE( *,* ) '  ERROR ON INPUT'
            GO TO 2
         ENDIF

         IF (OUTFIL(1:1) .EQ. ' ') THEN
            WRITE (*,*) 'Invalid response, please answer appropriately'
            WRITE (*,*)
            GO TO 2
         ENDIF

C*----------------------------------------------------------------------
C*       Force The Output File Type to be ASCII if DRY or WET
C*       Depletion is Desired
         IF (DRYFLG .OR. WETFLG) THEN
            OUTTYP = 'ASCII'
            WRITE (*,*) '    For Dry and Wet Deposition Calculations,'
            WRITE (*,*) '   the Output File Type Is Forced to be ASCII'
            WRITE (*,*)
         ELSE
3           WRITE (*,*)
            WRITE (*,*) 'Enter the output file type: '
            WRITE (*,*) 'Unform / Ascii : '
            READ  (*,'(A)',ERR=3) ANSWER
            CALL LWRUPR (ANSWER)
            IF (ANSWER(1:1) .EQ. 'U' ) THEN
               OUTTYP = 'UNFORM'
            ELSE
               OUTTYP = 'ASCII'
            ENDIF
            WRITE (INEW,310) ANSWER
  310       FORMAT( A, T41, ' Unform/Ascii output type')
         ENDIF

C*----------------------------------------------------------------------
    6    WRITE (*,*) 'Enter MIXING HEIGHT data filename: '
  600    READ  (*,'(A)',ERR=6) MIXNAM
         IF (MIXNAM(1:1) .EQ. ' ') THEN
            WRITE (*,*) 'Invalid response, please answer appropriately'
            WRITE (*,*)
            GO TO 6
         ENDIF
C*       Determine if the mixing height data file exists
         XFILE = .false.
         INQUIRE( FILE=MIXNAM, EXIST=XFILE )
         IF( .NOT. XFILE )THEN
            WRITE( *,* ) ' Mixing height data file does not exist -'
            WRITE( *,* ) '     re-enter the mixing height filename: '
            GO TO 600
         ENDIF
         WRITE (INEW,610) MIXNAM
  610    FORMAT( A, T41, ' Mixing height data file')

C*----------------------------------------------------------------------
    4    WRITE (*,*) 'Enter the HOURLY SURFACE DATA filename: '
  400    READ  (*,'(A)',ERR=4) SFCNAM
         IF (SFCNAM(1:1) .EQ. ' ') THEN
            WRITE (*,*) 'Invalid response, please answer appropriately'
            WRITE (*,*)
            GO TO 4
         ENDIF

C*       Determine if the input surface data file exists
         XFILE = .false.
         INQUIRE( FILE=SFCNAM, EXIST=XFILE )
         IF( .NOT. XFILE )THEN
            WRITE( *,* ) ' Surface data file does not exist -'
            WRITE( *,* ) '   re-enter the surface data filename: '
            GO TO 400
         ENDIF
         WRITE (INEW,410) SFCNAM
  410    FORMAT(A,T41,' Hourly surface data file')

C*----------------------------------------------------------------------
    5    WRITE (*,*) 'Enter surface data format: '
         WRITE (*,*) 'CD144 or SAMSON or HUSWO or SCRAM : '
         READ (*,'(A)',ERR=5) ANSWER
         CALL LWRUPR (ANSWER)

         IF (ANSWER .NE. 'CD144' .AND.  ANSWER .NE. 'SCRAM'  .AND.
     &       ANSWER .NE. 'SAMSON' .AND. ANSWER .NE. 'HUSWO') THEN
             WRITE (*,*) 'Invalid response, please answer appropriately'
             WRITE (*,*)
             GO TO 5
         ENDIF
         SFCTYP = ANSWER

         IF( SFCTYP .EQ. 'SCRAM'  .AND.  WETFLG )THEN
            CALL ERRHDL('E','1043',0)
         ENDIF

         WRITE (INEW,510) ANSWER
  510    FORMAT( A, T41, ' Surface data format')

C*----------------------------------------------------------------------
         IF( SFCTYP .EQ. 'CD144'  .OR.  SFCTYP .EQ. 'SCRAM'  .OR.
     &       SFCTYP .EQ. 'HUSWO' )THEN

            WRITE( *, 4000 )
 4000       FORMAT( ' For CD-144, SCRAM, and HUSWO data, additional',
     &              ' station information is needed'/)

   20       WRITE (*,2000)
 2000       FORMAT(3x, ' Enter the latitude of the surface station ',
     &                 'in decimal degrees',/,
     &         4x, '   - positive for stations north of the equator: ')

            READ (*,*,IOSTAT=IOS) XLAT

            IF( IOS .NE. 0 )THEN
               WRITE( *,* ) '  ERROR ON INPUT'
               GO TO 20
            ENDIF

            IF( ABS(XLAT) .GT. 90.0 )THEN
               WRITE( *,2015 )
               GO TO 20
            ENDIF

            WRITE (INEW,2010) XLAT
 2010       FORMAT( f10.3, T41, ' Station latitude (decimal degrees)' )
 2015       FORMAT('   INVALID VALUE: |Latitude| must be <= 90.0')

C*----------------------------------------------------------------------
   21       WRITE (*,2100)
 2100       FORMAT(3x, ' Enter the longitude of the surface station ',
     &                 'in decimal degrees',/,
     &           4x,'   - positive for stations WEST of Greenwich: ')
            READ (*,*,IOSTAT=IOS) XLON

            IF( IOS .NE. 0 )THEN
               WRITE( *,* ) '  ERROR ON INPUT'
               GO TO 21
            ENDIF

            IF( ABS(XLON) .GT. 180.0 )THEN
               WRITE( *,2106 )
               GO TO 21
            ENDIF

            WRITE (INEW, 2105) XLON
 2105       FORMAT( f10.3, T41, ' Station longitude (decimal degrees)' )
 2106       FORMAT('   INVALID VALUE: |Longitude| must be <= 180.0')

C*----------------------------------------------------------------------
  210       WRITE (*,2110)
 2110       FORMAT(3x, ' Enter the time zone of the surface station ',
     &         /,4x,'   - positive for stations WEST of Greenwich: ')
            READ (*,*,IOSTAT=IOS) ITZONE
            IF( IOS .NE. 0 )THEN
               WRITE( *,* ) '  ERROR ON INPUT'
               GO TO 210
            ENDIF

            IF( ITZONE .GT. 12 )THEN
               WRITE( *,2120 )
               GO TO 210
            ENDIF

            WRITE (INEW,2115) ITZONE
 2115       FORMAT( I10, T41, ' Station time zone' )
 2120       FORMAT( '  INVALID VALUE: |Time Zone| must be <= 12')
         ENDIF

C*----------------------------------------------------------------------
         IF (WETFLG) THEN
            IF (SFCTYP .EQ. 'SAMSON' .OR. SFCTYP .EQ. 'HUSWO' ) THEN
7             WRITE (*,*)'SAMSON/HUSWO data may already contain hourly',
     &                   ' precip'
              WRITE (*,*)'Do you want to supplement with TD-3240 data? '
              WRITE (*,*) 'Yes / No : '
              READ (*,'(A)',ERR=7) ANSWER
              CALL LWRUPR (ANSWER)

              IF (ANSWER .EQ. 'YES'  .OR.  ANSWER .EQ. 'Y') THEN
                 PPTFLG = .TRUE.
                 WRITE (INEW,710) ANSWER
              ELSEIF( ANSWER .EQ. 'NO'  .OR.  ANSWER .EQ. 'N' )THEN
                 PPTFLG = .FALSE.
                 WRITE (INEW,710) ANSWER
                 GO TO 1000
              ELSE
                 WRITE (*,*) 'Invalid response, please answer',
     &                       ' appropriately'
                 WRITE (*,*)
                 GO TO 7
              ENDIF
            ELSE
                 PPTFLG = .TRUE.
            ENDIF
  710       FORMAT(A,T41,' Supplement SAMSON/HUSWO precip with TD3240?')

C*----------------------------------------------------------------------
    8       WRITE (*,*) 'Enter HOURLY PRECIP DATA filename: '
  800       READ  (*,'(A)',ERR=8) PPTNAM
            IF (PPTNAM(1:1) .EQ. ' ') THEN
               WRITE (*,*) 'Invalid response, please answer',
     &                     ' appropriately'
               WRITE (*,*)
               GO TO 8
            ENDIF
C*          Determine if the precipitation data file exists
            XFILE = .false.
            INQUIRE( FILE=PPTNAM, EXIST=XFILE )
            IF( .NOT. XFILE )THEN
               WRITE( *,* ) ' Precipitation data file does not exist -'
               WRITE( *,* ) '     re-enter the precipitation filename: '
               GO TO 800
            ENDIF
            WRITE (INEW,810) PPTNAM
  810       FORMAT( A,T41, ' Precipitation data file')

C*----------------------------------------------------------------------
    9       WRITE (*,*) 'Enter precipitation file format'
            WRITE (*,*) 'Variable or Fixed: '
            READ (*,'(A)',ERR=9) ANSWER
            CALL LWRUPR (ANSWER)

            IF ((ANSWER .NE. 'VARIABLE' .AND.  ANSWER .NE. 'V')  .AND.
     &          (ANSWER .NE. 'FIXED'    .AND.  ANSWER .NE. 'F' ))THEN
               WRITE (*,*) 'Invalid response, please answer',
     &                     ' appropriately'
               WRITE (*,*)
               GO TO 9
            ENDIF
            PPTFMT = ANSWER
            WRITE (INEW,910) ANSWER
  910       FORMAT( A,T41,' Precip data format: Variable / Fixed')
         ENDIF

C*----------------------------------------------------------------------
 1000    IF( DRYFLG  .OR.  WETFLG )THEN

C---------- Enter the site characteristics for deposition processes

            WRITE (*,4100)
 4100       FORMAT (' For deposition, site characteristics are needed'/)

C---------- Minimum Monin-Obukhov length

   22       WRITE (*,2200)
 2200       FORMAT(4x,'Enter the minimum Monin-Obukhov length ',
     &                'for stable conditions (meters)',/
     &          ,4x,'  2m for open land to 100-150m for city centers: ')
            READ (*,*,IOSTAT=IOS) ELMINM

            IF( IOS .NE. 0 )THEN
               WRITE( *,* ) '  ERROR ON INPUT'
               GO TO 22
            ENDIF

            IF( ELMINM .LE. 0.0 )THEN
               WRITE( *,2215 )
               GO TO 22
            ENDIF

            WRITE (INEW,2210) ELMINM
 2210       FORMAT(f10.3, T41, ' Min. Obukhov length (m)')
 2215       FORMAT('   INVALID VALUE: Minimum L for a stable',
     &             ' atmosphere must be > 0.0')

C---------- Minimum Monin-Obukhov length - application site: NOT ACTIVATED

C   42       WRITE (*,2250)
C 2250       FORMAT(4x,'Enter the minimum Monin-Obukhov length ',
C     &                'for stable conditions (meters)',/
C     &             4x,'AT THE APPLICATION SITE',
C     &         /,4x,'  2m for open land to 100-150m for city centers: ')
C            READ (*,*,IOSTAT=IOS) ELMINA
C            IF( IOS .NE. 0 )THEN
C               WRITE( *,* ) '  ERROR ON INPUT'
C               GO TO 42
C            ENDIF
C            WRITE (INEW,2260) ELMINA
C 2260       FORMAT(f10.3, t41, ' Min. Obukhov length (m) - application')

C---------- Anemometer height

   23       WRITE (*,2300)
 2300       FORMAT(4x, 'Enter the anemometer height (meters) ', /
     &           4x,'    (measurement height of the wind): ' )
            READ (*,*,IOSTAT=IOS) ANEMHT

            IF( IOS .NE. 0 )THEN
               WRITE( *,* ) '  ERROR ON INPUT'
               GO TO 23
            ENDIF

            IF( ANEMHT .LE. 0.0 )THEN
               WRITE( *,* ) '  INVALID VALUE: Height must be > 0.0'
               GO TO 23
            ENDIF

            WRITE (INEW,2310) ANEMHT
 2310       FORMAT(F10.3, T41, ' Anemometer height (m)')

C---------- Surface roughness length for the
C           site where the measurements were taken.

   29       WRITE (*,2900)
 2900       FORMAT(4x, 'Enter the surface roughness length (meters) ',
     &                 'AT THE ** MEASUREMENT SITE **',/,
     &             4x, '   0.0001 for open water to 1.3 for forests: ')
            READ (*,*,IOSTAT=IOS) Z0MEAS

            IF( IOS .NE. 0 )THEN
               WRITE( *,* ) '  ERROR ON INPUT'
               GO TO 29
            ENDIF

            IF( Z0MEAS .LE. 0.0 )THEN
               WRITE( *,* ) '  INVALID VALUE: Sfc. rough. must be > 0.0'
               GO TO 29
            ENDIF

            IF( ANEMHT .LE. Z0MEAS )THEN
               WRITE( *,2905 )
 2905          FORMAT('   WARNING: Roughness length must be < '
     &                'anemometer ht')
            ENDIF

            WRITE (INEW,2910) Z0MEAS
 2910       FORMAT( f10.4,t41,' Roughness length (m), measurement site')

C---------- For this version of PCRAMMET, set the displacement height
C           to 0.0 meters; the computations below will still 'use' the
C           displacement height
            ZDMEAS = 0.0

C  24       WRITE (*,2400)
C2400       FORMAT(4x,'Enter the displacement height (meters)', /,
C      &             4x,' at the measurement site',/,
C      &             4x,'   - must be less than the anemometer height: ')
C           READ (*,*,IOSTAT=IOS) ZDMEAS
C           IF( IOS .NE. 0 )THEN
C              WRITE( *,* ) 'ERROR ON INPUT'
C              GO TO 24
C           ENDIF
C           WRITE (INEW,2410) ZDMEAS
C2410       FORMAT(f10.3,t41,' Displacement ht (m), measurement site')

C           IF( ANEMHT .LE. ZDMEAS )THEN
C              CALL ERRHDL( 'E','1110',0 )
C           ENDIF

C---------- Surface roughness length and displacement height at the
C           site where the output data are to be applied.

   39       WRITE (*,3900)
 3900       FORMAT(4x, 'Enter the surface roughness length (meters) ',
     &                 'AT THE >> APPLICATION SITE <<',/,
     &             4x, '   0.0001 for open water to 1.3 for forests: ')
            READ (*,*,IOSTAT=IOS) Z0APPL

            IF( IOS .NE. 0 )THEN
               WRITE( *,* ) '  ERROR ON INPUT'
               GO TO 39
            ENDIF

            IF( Z0APPL .LE. 0.0 )THEN
               WRITE( *,* ) '  INVALID VALUE: Sfc. rough. must be > 0.0'
               GO TO 39
            ENDIF

            IF( ANEMHT .LE. Z0MEAS )THEN
               WRITE( *,2905 )
            ENDIF

            WRITE (INEW,3910) Z0APPL
 3910       FORMAT( f10.4,t41,' Roughness length (m), application site')

C34         WRITE (*,3400)
C3400       FORMAT(4x,'Enter the displacement height (meters)', /,
C     &            4x,' at the application site',/,
C     &            4x,'   - must be less than the anemometer height: ')
C           READ (*,*,IOSTAT=IOS) ZDAPPL
C           IF( IOS .NE. 0 )THEN
C              WRITE( *,* ) 'ERROR ON INPUT'
C              GO TO 24
C           ENDIF
C           WRITE (INEW,3410) ZDAPPL
C0          FORMAT(f10.3,t41,' Displacement ht (m), application site')
C
C           IF( ANEMHT .LE. ZDAPPL )THEN
C              CALL ERRHDL( 'E','1110',0 )
C           ENDIF

C---------- Noon-time albedo

   25       WRITE (*,2500)
 2500       FORMAT(4x, 'Enter the noon time albedo (as a fraction) ', /,
     &             4x, '       0.1 for water to 0.9 for fresh snow: ')
            READ (*,*,IOSTAT=IOS) ALBEDO

            IF( IOS .NE. 0 )THEN
               WRITE( *,* ) '  ERROR ON INPUT'
               GO TO 25
            ENDIF

            IF( ALBEDO .LT. 0.0  .OR.  ALBEDO .GT. 1.0 )THEN
               WRITE( *,2515 )
               GO TO 25
            ENDIF

            WRITE (INEW,2510) ALBEDO
 2510       FORMAT(f10.3,T41,' Noon time albedo')
 2515       FORMAT('   INVALID VALUE: Allowable values for albedo',
     &             ' are 0.0 to 1.0')
C---------- Bowen ratio

   26       WRITE (*,2600)
 2600       FORMAT(4x, 'Enter the Bowen ratio ', /,
     &           4x,'  0.1 for very moist to 10.0 for desert: ')
            READ (*,*,IOSTAT=IOS) BOWEN

            IF( IOS .NE. 0 )THEN
               WRITE( *,* ) '  ERROR ON INPUT'
               GO TO 26
            ENDIF

            IF( BOWEN .LT. 0.1  .OR.  BOWEN .GT. 10.0 )THEN
               WRITE( *,2615 )
            ENDIF

            WRITE (INEW,2610) BOWEN
 2610       FORMAT(f10.3,t41,' Bowen ratio')
 2615       FORMAT('   WARNING: Bowen ratio outside reasonable',
     &             ' range: 0.1 to 10.0'/)

C---------- Anthropogenic heat flux

   27       WRITE (*,2700)
 2700       FORMAT(4x,'Enter the anthopogenic heat flux (W/m^2)', /,
     &          4x,'  0.0 for rural to 100.0 for large cities: ')
            READ (*,*,IOSTAT=IOS) QF

            IF( IOS .NE. 0 )THEN
               WRITE( *,* ) '  ERROR ON INPUT'
               GO TO 27
            ENDIF

            IF( QF .LT. 0.0 )THEN
               WRITE( *,2715 )
            ENDIF

            WRITE (INEW,2710) QF
 2710       FORMAT( f10.3,t41,' Anthropogenic heat flux (W/m^2)')
 2715       FORMAT('   WARNING: Anthropogenic heat flux should be',
     &             ' greater than or equal to zero'/)

C---------- Fraction of net radiation absorbed by the ground

   28       WRITE (*,2800)
 2800       FORMAT(4x,'Enter the fraction of net radiation absorbed by',
     &             ' the ground' ,/,29x,
     &             ' 0.15 for rural to 0.27 for urban: ')
            READ (*,*,IOSTAT=IOS) CSUBG

            IF( IOS .NE. 0 )THEN
               WRITE( *,* ) '  ERROR ON INPUT'
               GO TO 28
            ENDIF

            IF( CSUBG .LT. 0.0  .OR.  CSUBG .GT. 1.0 )THEN
               WRITE( *,2815 )
               GO TO 28
            ENDIF

            WRITE (INEW,2810) CSUBG
 2810       FORMAT( f10.3,t41,' Fraction net rad''n absorbed by ground')
 2815       FORMAT('   INVALID VALUE: Allowable values for the'
     &             ' fraction are 0.0 to 1.0')

         ENDIF

C*       Close the New Inputs file
         CLOSE (INEW)

      ELSE
C*       Read the Inputs From the Response File

         READ  (INOPT,'(A)',ERR=1003) ANSWER
         CALL LWRUPR (ANSWER)

         IF (ANSWER .EQ. 'DRY'  .OR.  ANSWER .EQ. 'D' ) THEN
            DRYFLG = .TRUE.
            WETFLG = .FALSE.
         ELSE IF (ANSWER .EQ. 'WET'  .OR.  ANSWER .EQ. 'W') THEN
            DRYFLG = .FALSE.
            WETFLG = .TRUE.
         ELSE IF (ANSWER .EQ. 'NONE' .OR.  ANSWER .EQ. 'N') THEN
            DRYFLG = .FALSE.
            WETFLG = .FALSE.
         ELSE
            GO TO 1004
         ENDIF

         IF (DRYFLG .OR. WETFLG) THEN
            OUTTYP = 'ASCII'
         ELSE
            READ  (INOPT,'(A)',ERR=1006) ANSWER
            CALL LWRUPR (ANSWER)
            IF (ANSWER(1:1) .EQ. 'U') THEN
               OUTTYP = 'UNFORM'
            ELSE
               OUTTYP = 'ASCII'
            ENDIF
         ENDIF

         READ  (INOPT,'(A)',ERR=1009) MIXNAM

         READ  (INOPT,'(A)',ERR=1007) SFCNAM
         READ (INOPT,'(A)',ERR=1008) ANSWER
         CALL LWRUPR (ANSWER)
         SFCTYP = ANSWER
         IF( SFCTYP .EQ. 'SCRAM'  .AND.  WETFLG )THEN
            CALL ERRHDL('E','1043',0)
         ENDIF

         IF( SFCTYP .EQ. 'CD144'  .OR.  SFCTYP .EQ. 'SCRAM'  .OR.
     &       SFCTYP .EQ. 'HUSWO' )THEN
            READ (INOPT,*,ERR=1013) XLAT
            READ (INOPT,*,ERR=1014) XLON
            READ (INOPT,*,ERR=1101) ITZONE
         ENDIF

         IF (WETFLG) THEN
            IF (SFCTYP .EQ. 'SAMSON'  .OR.  SFCTYP .EQ. 'HUSWO') THEN
              READ (INOPT,'(A)',ERR=1010) ANSWER
              CALL LWRUPR (ANSWER)

              IF (ANSWER .EQ. 'YES'  .OR.  ANSWER .EQ. 'Y') THEN
                 PPTFLG = .TRUE.
              ELSE
                 PPTFLG = .FALSE.
                 GO TO 40
              ENDIF
            ELSE
                 PPTFLG = .TRUE.
            ENDIF

            READ  (INOPT,'(A)',ERR=1011) PPTNAM
            READ  (INOPT,'(A)',ERR=1012) ANSWER
            CALL LWRUPR (ANSWER)
            PPTFMT = ANSWER
         ENDIF

   40    IF( DRYFLG  .OR.  WETFLG )THEN
            READ (INOPT,*,ERR=1015) ELMINM
            READ (INOPT,*,ERR=1016) ANEMHT
            IF( ANEMHT .LE. 0.0 )THEN
               CALL ERRHDL('E', '1115', 0)
            ENDIF

            READ (INOPT,*,ERR=1022) Z0MEAS
            IF( Z0MEAS .LE. 0.0 )THEN
               CALL ERRHDL('E', '1110', 0)
            ENDIF

            READ (INOPT,*,ERR=1022) Z0APPL
            IF( Z0APPL .LE. 0.0 )THEN
               CALL ERRHDL('E', '1110', 0)
            ENDIF

            READ (INOPT,*,ERR=1018) ALBEDO
            IF( ALBEDO .LT. 0.0  .OR.  ALBEDO .GT. 1.0)THEN
               CALL ERRHDL('E', '1130', 0)
            ENDIF

            READ (INOPT,*,ERR=1019) BOWEN
            IF( BOWEN .LT. 0.1 .OR.  BOWEN .GT. 10.0 )THEN
               WRITE( IDIAG,2615 )
               WRITE( *,2615 )
            ENDIF

            READ (INOPT,*,ERR=1020) QF
            IF( QF .LT. 0.0 )THEN
               WRITE( IDIAG,2715 )
               WRITE( *,2715)
            ENDIF

            READ (INOPT,*,ERR=1021) CSUBG
            IF( CSUBG .LE. 0.0 )THEN
               CALL ERRHDL('E', '1135', 0)
            ENDIF
         ENDIF

      ENDIF

C*    Write some of the responses to the LOG file, PCRAM.LOG

      WRITE (IDIAG, 55) OUTFIL
  55  FORMAT ( ' The following data were used for output file: ',A40/ )
      IF( DRYFLG )THEN
         WRITE( IDIAG,155 )
      ELSEIF( WETFLG )THEN
         WRITE( IDIAG,156 )
      ELSE
         WRITE( IDIAG,157 )
      ENDIF
 155  FORMAT ( 'DRY',T41, ' Dry deposition calculations')
 156  FORMAT ( 'WET',T41, ' Wet deposition calculations')
 157  FORMAT ( 'NONE',T41, ' No deposition calculations')

      WRITE (IDIAG,610) MIXNAM
      WRITE (IDIAG,410) SFCNAM
      WRITE (IDIAG,510) SFCTYP

      IF( SFCTYP .EQ. 'CD144'  .OR.  SFCTYP .EQ. 'SCRAM'  .OR.
     &    SFCTYP .EQ. 'HUSWO' )THEN
         WRITE (IDIAG,2010) XLAT
         WRITE (IDIAG, 2105) XLON
         WRITE (IDIAG,2115) ITZONE
      ENDIF
      IF ( PPTFLG )THEN
         WRITE (IDIAG,810) PPTNAM
      ENDIF
      IF( DRYFLG .OR. WETFLG )THEN
         WRITE (IDIAG,2210) ELMINM
         WRITE (IDIAG,2310) ANEMHT
         WRITE (IDIAG,2910) Z0MEAS
         WRITE (IDIAG,3910) Z0APPL
         WRITE (IDIAG,2510) ALBEDO
         WRITE (IDIAG,2610) BOWEN
         WRITE (IDIAG,2710) QF
         WRITE (IDIAG,2810) CSUBG
      ENDIF
      WRITE( IDIAG, 3000 )
 3000 FORMAT( //,' The following messages were written by PCRAMMET:'/)



      GO TO 999

C*    Error Handling - the error handler stops PCRAMMET
1002  CALL ERRHDL('E','1002',0)
1003  CALL ERRHDL('E','1003',0)
1004  CALL ERRHDL('E','1004',0)
1006  CALL ERRHDL('E','1006',0)
1007  CALL ERRHDL('E','1007',0)
1008  CALL ERRHDL('E','1008',0)
1009  CALL ERRHDL('E','1009',0)
1010  CALL ERRHDL('E','1010',0)
1011  CALL ERRHDL('E','1011',0)
1012  CALL ERRHDL('E','1012',0)
1013  CALL ERRHDL('E','1013',0)
1014  CALL ERRHDL('E','1014',0)
1015  CALL ERRHDL('E','1015',0)
1016  CALL ERRHDL('E','1016',0)
1017  CALL ERRHDL('E','1017',0)
1018  CALL ERRHDL('E','1018',0)
1019  CALL ERRHDL('E','1019',0)
1020  CALL ERRHDL('E','1020',0)
1021  CALL ERRHDL('E','1021',0)
1022  CALL ERRHDL('E','1022',0)
1101  CALL ERRHDL('E','1101',0)

999   RETURN
      END


      SUBROUTINE FILES
C***********************************************************************
C*    FILES Module of PCRAMMET Meteorological Pre-processor
C*
C*    PURPOSE:    Checks to See If All the Required Files Are Present
C*                and Opens Them.
C*
C*    PROGRAMMER: Jayant Hardikar, Jim Paumier
C*                PES Inc.
C*
C*    DATE:       August 15, 1995
C*
C*    INPUTS:     Filenames
C*
C*    OUTPUTS:    ----
C*
C*    CALLED FROM: MAIN program
C***********************************************************************
C*
C*    Variable Declarations
      INCLUDE 'PCRAM.INC'
      LOGICAL EXISTS
      CHARACTER ATEST*1, BTEST*3, SCTEST*80

C*    First the Surface File
      EXISTS = .FALSE.
      INQUIRE (FILE=SFCNAM,EXIST=EXISTS)
      IF (EXISTS) THEN
         OPEN (INSFC,FILE=SFCNAM,ERR=1023)
      ELSE
         GO TO 1023
      ENDIF

C*    The Mixing Height File
      EXISTS = .FALSE.
      INQUIRE (FILE=MIXNAM,EXIST=EXISTS)
      IF (EXISTS) THEN
         OPEN (INMIX,FILE=MIXNAM,ERR=1024)
      ELSE
         GO TO 1024
      ENDIF

C*    The Precip File - Open Only if Wet Deposition is Desired
C*    And Either the Surface Data is Not from Samson or When
C*    a TD3240 File Supplements the SAMSON Precip.

      IF (WETFLG)THEN
         IF (PPTFLG) THEN
            EXISTS = .FALSE.
            INQUIRE (FILE=PPTNAM,EXIST=EXISTS)
            IF (EXISTS) THEN
               OPEN (INPPT,FILE=PPTNAM,ERR=1025)

C*             IF THE TD3240 File Is A Variable Format
C*             File, Then First Rewrite All Precip Data To
C*             A Scratch Formatted File (VAR2FIX logic)
               IF (PPTFMT .EQ. 'VARIABLE'  .OR.  PPTFMT .EQ. 'V') THEN

C*                Check To See If The File Is REALLY A Variable FMT
                  READ (INPPT,'(T28,A3)',ERR=1026) BTEST
                  IF (BTEST .EQ. '   ') GO TO 1026
                  REWIND (INPPT)

C*****            OPEN (INPPT+50,file='azbycxdw', ERR=1027)
                  OPEN (INPPT+50,STATUS='SCRATCH', ERR=1027)
C                 write (*,*) 'pptfile opened - temporary'
                  CALL VAR2FIX(INPPT)
                  INPPT = INPPT + 50
                  REWIND (INPPT)

               ELSE
C*                Check To See If The File Is REALLY A Fixed FMT
                  READ (INPPT,'(T28,A3)',ERR=1028) BTEST
                  IF (BTEST .NE. '   ') GO TO 1028
                  REWIND (INPPT)

               ENDIF

            ELSE
               GO TO 1029
            ENDIF
         ENDIF
      ENDIF

C*    The Output File
      IF (OUTTYP .EQ. 'UNFORM') THEN
         OPEN (IOUT,FILE=OUTFIL,FORM='UNFORMATTED',
     &         ACCESS='SEQUENTIAL',ERR=1030)

      ELSE IF (OUTTYP .EQ. 'ASCII') THEN
         OPEN (IOUT,FILE=OUTFIL,ERR=1031)

      ENDIF

C*    Check to See If The File Are Of The Type That The User Says
C*    They Are; a difficulty may arise if the first record in the
C     CD-144 file has 28 characters or less - the program will
C     determine the file is not of type CD144 when, in fact, it is.

      IF (SFCTYP .EQ. 'CD144') THEN
         READ(INSFC, '(A80)', ERR=1032 ) SCTEST
         NCH = 80
         DO WHILE( NCH .GT. 0  .AND.  SCTEST(NCH:NCH) .EQ. ' ')
            NCH = NCH -1
         ENDDO

         IF( NCH .LE. 28 )THEN
C---------- Likely not a CD144 file; records should be > 28 characters
            GO TO 1032
         ENDIF


      ELSEIF( SFCTYP .EQ. 'SCRAM' )THEN
         READ(INSFC, '(A80)', ERR=1034 ) SCTEST
         NCH = 80
         DO WHILE( NCH .GT. 0  .AND.  SCTEST(NCH:NCH) .EQ. ' ')
            NCH = NCH -1
         ENDDO

         IF( NCH .GT. 28 )THEN
C---------- Not a SCRAM file, which should have <= 28-character records
            GO TO 1034
         ENDIF

      ELSE IF (SFCTYP .EQ. 'SAMSON') THEN
         READ (INSFC,'(A1)',ERR=1033)ATEST
         IF (ATEST .NE. '~') THEN
C---------- Not a SAMSON file
            GO TO 1033
         ENDIF

      ELSE IF (SFCTYP .EQ. 'HUSWO') THEN
         READ (INSFC,'(A80)',ERR=1033)SCTEST
         IF (INDEX(SCTEST,'1     2') .EQ. 0 ) THEN
C---------- Not likely a HUSWO file
            GO TO 1044
         ENDIF

      ENDIF

      REWIND (INSFC)
      GO TO 999

1023  CALL ERRHDL('E','1023',0)
1024  CALL ERRHDL('E','1024',0)
1025  CALL ERRHDL('E','1025',0)
1026  CALL ERRHDL('E','1026',0)
1027  CALL ERRHDL('E','1027',0)
1028  CALL ERRHDL('E','1028',0)
1029  CALL ERRHDL('E','1029',0)
1030  CALL ERRHDL('E','1030',0)
1031  CALL ERRHDL('E','1031',0)
1032  CALL ERRHDL('E','1032',0)
1033  CALL ERRHDL('E','1033',0)
1034  CALL ERRHDL('E','1034',0)
1044  CALL ERRHDL('E','1044',0)

999   RETURN
      END


      SUBROUTINE VARINI
C***********************************************************************
C*    VARINI Module of the PCRAMMET Meteorological Preprocessor
C*
C*    PURPOSE:    Initializes the input and output data
C*
C*    PROGRAMMER: J. Paumier
C*                PES Inc.
C*
C*    DATE:       April 28, 1995
C*
C*    INPUTS:     None
C*
C*    OUTPUTS:    Initialized data
C*
C*    CALLED FROM: MAIN program
C***********************************************************************
C*
C*    Variable Declarations
      INCLUDE 'PCRAM.INC'

      DO 100 IH = 1,24
         KST(IH)    = 9
         ICOVER(IH) = 99
         IDIR(IH)   = 999
         ICEIL(IH)  = 9999
         IGRAD(IH)  = 9999
         IRHUM(IH) = -99

         RSOL(IH)   = 0.0
         SHF(IH)    = 0.0
         FCOVER(IH) = 99.0
         WSPEED(IH) = 99.0
         AFV(IH)    = 999.0
         FVR(IH)    = 999.0
         HLH(1,IH)  = 9999.0
         HLH(2,IH)  = 9999.0
         TEMP(IH)   = 9999.0
         PRESS(IH)  = 9999.0
         PRECIP(IH) = 9999.0
         P3240(IH)  = 9999.0

         USTAR(IH)  = -9.0
         XMONIN(IH) = -999.0

  100 CONTINUE

      RETURN
      END


      SUBROUTINE WRITIT(JDAY)
C***********************************************************************
C*    USTARL Module of PCRAMMET Meteorological Pre-processor
C*
C*    PURPOSE:    This Routine Writes Out 24 Hours Worth of Data
C*
C*    PROGRAMMER: Jayant Hardikar
C*                PES Inc.
C*
C*    DATE:       February 23, 1995
C*
C*    INPUTS:     Atmospheric boundary layer variables for ISCST-type
C*                models
C*
C*    OUTPUTS:    ---
C*
C*    CALLED FROM: MAIN program
C***********************************************************************
C*
C*    Variable Declarations
C*    (IWRI is Used to Check If This Routine is Being Called
C*    for the First Time or Not)
      INTEGER ISAVWX, ISAVMIX
      SAVE IWRI
      INCLUDE 'PCRAM.INC'

C*    Save the year to another variable so it can be converted to a 
C     4-digit year

      ISAVWX = ISYR(1)
      ISAVMIX = IYRMIX(2)

      IF (OUTTYP .EQ. 'ASCII') THEN

C*       Write Header Info the First Time
C*       Recall that the second record of mixing height data is for
C*       the current day and the first data record is for the
C*       previous day, so write the year corresponding to the second
C*       data record.
         IF (IWRI .EQ. 0) THEN
C           Convert the years in the header record from 2-digit to 4-digit

            IF( ISAVWX .GE. 0  .AND.  ISAVWX .LE. 49 ) THEN
               ISAVWX = 2000 + ISAVWX
            ELSEIF( ISAVWX .GE. 50  .AND.  ISAVWX .LE. 99 ) THEN
               ISAVWX = 1900 + ISAVWX
            ENDIF

            IF( ISAVMIX .GE. 0  .AND.  ISAVMIX .LE. 49 ) THEN
               ISAVMIX = 2000 + ISAVMIX
            ELSEIF( ISAVMIX .GE. 50  .AND.  ISAVMIX .LE. 99 ) THEN
               ISAVMIX = 1900 + ISAVMIX
            ENDIF

            WRITE(IOUT,200) IDSFC, ISAVWX, IDMIX(1), ISAVMIX
            IWRI = IWRI + 1
         ENDIF

C*       Loop Over 24 Hours
         DO 20 I = 1,24

C*          If the Hour is Calm, then set the direction to 0 and the
C*          speed to 0.0 ( for ASCII output only); this is the method
C           used in ISC2 to identify calm winds in an ASCII file.
            IF (CALM(I)) THEN
               WSPEED(I) = 0.0
            ENDIF

C*          Write Hourly Met Data
            IF( WETFLG )THEN

               IF( SFCTYP .EQ. 'SAMSON'  .OR. SFCTYP .EQ. 'HUSWO' )THEN
                  WRITE(IOUT,250) ISYR(I),ISMO(I),ISDY(I),ISHR(I),
     &                            FVR(I),WSPEED(I),TEMP(I),KST(I),
     &                            HLH(1,I),HLH(2,I),USTAR(I),XMONIN(I),
     &                            Z0APPL,IPCODE(I),PRECIP(I), 
     &                            FLOAT(IGRAD(I)), IRHUM(I)
               ELSE
                  WRITE(IOUT,250) ISYR(I),ISMO(I),ISDY(I),ISHR(I),
     &                            FVR(I),WSPEED(I),TEMP(I),KST(I),
     &                            HLH(1,I),HLH(2,I),USTAR(I),XMONIN(I),
     &                            Z0APPL,IPCODE(I),PRECIP(I)
               ENDIF

            ELSEIF( DRYFLG )THEN
               IF( SFCTYP .EQ. 'SAMSON'  .OR. SFCTYP .EQ. 'HUSWO' )THEN
                  WRITE(IOUT,255) ISYR(I),ISMO(I),ISDY(I),ISHR(I),
     &                            FVR(I),WSPEED(I),TEMP(I),KST(I),
     &                            HLH(1,I),HLH(2,I),USTAR(I),XMONIN(I),
     &                            Z0APPL, FLOAT(IGRAD(I)), IRHUM(I)
               ELSE
                  WRITE(IOUT,255) ISYR(I),ISMO(I),ISDY(I),ISHR(I),
     &                            FVR(I),WSPEED(I),TEMP(I),KST(I),
     &                            HLH(1,I),HLH(2,I),USTAR(I),XMONIN(I),
     &                            Z0APPL
               ENDIF

            ELSE
               WRITE(IOUT,250) ISYR(I),ISMO(I),ISDY(I),ISHR(I),FVR(I),
     &                      WSPEED(I),TEMP(I),KST(I),HLH(1,I),HLH(2,I)
            ENDIF

20       CONTINUE

      ELSE IF (OUTTYP .EQ. 'UNFORM') THEN

C*       Write Header Info the First Time
         IF (IWRI .EQ. 0) THEN
C           Convert the year in the header record from 2-digit to 4-digit

            IF( ISAVWX .GE. 0  .AND.  ISAVWX .LE. 49 ) THEN
               ISAVWX = 2000 + ISAVWX
            ELSEIF( ISAVWX .GE. 50  .AND.  ISAVWX .LE. 99 ) THEN
               ISAVWX = 1900 + ISAVWX
            ENDIF

            IF( ISAVMIX .GE. 0  .AND.  ISAVMIX .LE. 49 ) THEN
               ISAVMIX = 2000 + ISAVMIX
            ELSEIF( ISAVMIX .GE. 50  .AND.  ISAVMIX .LE. 99 ) THEN
               ISAVMIX = 1900 + ISAVMIX
            ENDIF

            WRITE(IOUT) IDSFC, ISAVWX, IDMIX(1), ISAVMIX
            IWRI = IWRI + 1
         ENDIF


         WRITE (IOUT) ISYR(1),ISMO(1),FLOAT(JDAY),KST,WSPEED,TEMP,
     &                AFV,FVR,HLH

      ENDIF

C     NOTE: The colons in the format statement below is an edit descriptor 
C           that tells the program to terminate format control when the
C           output list associated with the WRITE statement is exhausted.

200   FORMAT(4(I6,1X))
250   FORMAT(I2.2, 3I2,2F9.4,F6.1,I2,2F7.1:,F9.4,F10.1,F8.4:,I4,F7.2:,
     &       T102,F9.0,T111,I3)
255   FORMAT(I2.2, 3I2,2F9.4,F6.1,I2,2F7.1,F9.4,F10.1,F8.4:,
     &       T102,F9.0,T111,I3)

      RETURN
      END


      BLOCK DATA
C***********************************************************************
C*    BLOCK Block Data for the PCRAMMET Meteorological Pre-processor
C*
C*    PURPOSE:    Initializes Block Data
C*
C*    PROGRAMMER: Jayant Hardikar
C*                PES Inc.
C*
C*    DATE:       February 13, 1995
C*
C*    INPUTS:
C*
C*
C*    OUTPUTS:
C*
C***********************************************************************
C*
C*    Variable Declarations
      INCLUDE 'PCRAM.INC'

      INTEGER IRAND,KRAND(8784)
      COMMON /BDATA/ IRAND(24,366)
      EQUIVALENCE (IRAND(1,1), KRAND(1))

      DATA CONST /57.29578/                                                                                                             MET40800
      DATA INSFC/11/, INOPT/12/, INMIX/13/, INPPT/14/, IOUT/15/
      DATA INEW /77/, IDIAG/88/
      DATA NOWARN /.TRUE./
      DATA VERSON/'99169'/

      DATA (KRAND(I),I=1,456)
     1/5,2,8,7,7,6,9,7,1,5,8,0,7,3,6,8,5,1,8,1,4,6,4,4,
     20,6,6,4,0,1,3,0,4,5,9,5,3,1,4,8,2,5,6,9,8,9,1,2,
     34,0,8,4,1,3,0,3,2,8,6,2,5,6,1,4,3,5,1,2,1,3,5,1,
     40,0,2,3,5,6,0,2,0,1,1,7,9,6,5,4,7,3,1,3,1,3,6,2,
     59,8,1,4,6,9,6,9,3,2,2,3,2,4,9,2,3,0,1,1,6,4,2,6,
     60,7,9,7,6,0,2,6,2,2,9,0,5,9,6,6,6,9,2,5,6,0,6,0,
     72,7,4,9,7,0,3,8,4,8,7,6,5,0,1,9,8,8,5,6,2,6,5,9,
     83,5,2,9,1,9,9,5,4,4,3,1,3,8,6,3,2,4,7,6,7,2,4,8,
     99,0,0,8,0,3,7,8,4,8,5,1,8,9,4,7,5,8,0,5,3,4,8,9,
     15,7,4,9,2,9,8,8,0,6,5,7,9,0,0,2,2,1,7,3,9,6,2,5,
     24,8,7,8,7,8,6,5,6,9,1,4,9,8,1,5,7,0,3,8,0,0,5,3,
     32,3,8,0,1,2,2,6,7,5,2,3,2,2,5,3,0,3,6,9,0,1,9,4,
     43,2,7,3,8,4,1,7,5,7,6,5,4,1,5,0,1,8,4,0,2,5,4,2,
     58,7,2,4,6,0,6,8,4,4,2,1,7,5,1,0,6,0,9,7,0,1,1,4,
     64,6,2,4,9,4,3,6,1,1,6,4,8,4,7,0,7,1,5,6,8,7,3,4,
     70,3,1,3,8,6,6,2,6,1,5,6,6,4,5,9,0,5,3,4,0,0,3,6,
     81,2,3,8,7,1,4,5,8,6,0,9,0,2,8,6,2,3,3,1,2,2,5,3,
     94,3,3,7,9,6,3,2,7,6,1,1,5,9,5,9,3,3,9,4,3,5,7,0,
     11,1,9,0,7,2,6,6,1,5,8,3,4,4,5,7,8,2,4,8,1,5,2,0/
      DATA (KRAND(I),I=457, 912)
     */3,7,1,8,1,7,9,2,4,9,2,0,7,4,6,7,5,3,1,9,3,8,3,2,
     *3,4,3,4,1,5,3,1,9,4,6,9,3,0,4,2,2,8,0,3,0,1,2,3,
     *6,5,4,8,9,6,1,6,2,8,2,7,1,1,9,8,3,0,9,1,4,9,5,7,
     *4,1,4,4,3,5,3,1,9,3,0,8,4,3,4,7,6,9,8,5,1,1,1,4,
     *0,3,1,2,3,6,1,7,2,8,1,2,3,1,7,2,2,6,6,6,9,9,7,3,
     *4,6,8,9,5,9,0,5,8,6,5,2,7,6,6,4,0,5,5,4,2,4,1,4,
     *6,9,2,2,8,0,7,0,7,8,6,0,0,6,0,4,9,5,8,2,0,0,8,2,
     *5,6,5,5,2,5,1,4,5,6,7,4,0,1,8,0,2,5,0,0,2,5,0,0,
     *1,4,1,8,6,1,0,3,3,8,8,0,2,9,5,6,5,2,2,4,1,5,2,2,
     *1,6,9,5,2,8,7,8,7,3,7,4,7,5,9,8,4,1,7,1,3,6,8,2,
     *9,6,1,7,4,2,6,9,1,6,6,3,8,4,2,7,6,5,8,7,2,5,5,2,
     *8,2,6,5,7,1,9,6,0,8,2,0,1,8,3,0,2,3,0,3,5,2,0,7,
     *7,2,1,3,1,0,3,0,4,8,0,0,8,3,0,0,2,6,0,3,9,6,5,6,
     *6,9,7,9,3,7,0,0,9,0,5,0,7,0,6,4,2,8,2,1,7,4,3,7,
     *5,9,0,2,0,0,4,6,8,7,9,4,2,2,6,1,1,9,0,6,7,2,7,9,
     *1,1,0,9,6,2,1,3,8,0,9,7,7,1,9,6,0,6,9,8,0,8,6,2,
     *0,3,6,6,3,0,3,2,4,9,6,6,9,0,8,9,7,1,9,6,3,6,8,1,
     *2,7,4,0,2,1,8,3,8,2,2,0,8,3,9,6,6,6,2,5,3,6,1,1,
     *2,9,8,2,5,4,6,8,8,7,2,5,6,4,5,5,8,7,2,9,4,9,7,8/
      DATA (KRAND(I),I=913, 1368)
     */4,5,9,5,4,6,5,9,5,8,0,7,8,6,6,7,2,3,9,1,7,3,0,6,
     *3,8,0,1,5,2,0,1,4,5,5,8,3,0,1,2,6,1,1,3,0,9,9,5,
     *9,3,4,5,0,9,5,3,1,4,5,6,8,3,7,9,2,8,4,1,9,8,1,5,
     *0,7,8,6,1,0,7,2,7,9,5,5,2,6,9,9,8,1,5,7,5,0,0,8,
     *3,3,8,5,3,6,7,6,0,6,9,1,2,5,2,8,2,6,9,4,6,9,2,9,
     *8,9,3,1,8,6,1,4,3,5,8,5,3,1,4,3,4,9,8,0,3,3,2,0,
     *4,6,0,0,2,9,4,8,7,7,2,8,6,1,9,7,7,0,3,1,5,2,2,7,
     *7,6,6,4,2,1,5,8,0,2,0,9,2,6,2,0,8,0,0,9,5,8,4,8,
     *9,5,6,8,5,3,9,4,2,8,9,6,7,9,0,4,6,3,6,8,8,3,5,8,
     *0,6,5,8,5,1,9,5,2,5,9,8,2,8,8,6,3,3,8,2,4,6,7,0,
     *7,7,7,8,1,4,4,8,2,2,3,1,0,1,5,0,3,7,4,8,2,7,6,9,
     *3,3,2,0,3,2,4,1,8,5,4,7,6,6,1,1,8,1,6,4,2,3,1,2,
     *2,7,8,5,2,7,6,9,6,1,7,6,2,4,8,0,2,1,9,5,9,0,0,4,
     *4,8,2,2,8,2,8,0,8,0,6,8,5,7,2,2,2,5,6,8,3,7,6,0,
     *0,4,4,0,8,4,9,4,8,1,8,0,3,8,8,6,7,5,2,0,0,3,9,1,
     *0,2,4,3,5,7,9,5,6,5,3,0,0,9,0,1,0,0,9,6,6,4,2,7,
     *7,6,6,5,8,1,0,3,5,6,4,3,5,9,9,7,6,8,8,8,5,9,1,1,
     *8,9,2,1,9,7,6,7,6,0,7,0,0,4,9,5,2,5,2,1,4,0,8,8,
     *5,2,4,6,9,4,7,6,3,5,4,4,2,5,0,6,1,8,7,8,9,5,9,8/
      DATA (KRAND(I),I=1369, 1824)
     */2,1,7,6,9,8,7,7,9,3,5,3,2,9,9,9,0,5,2,3,0,9,2,1,
     *3,3,0,1,4,3,0,4,1,0,3,3,4,9,6,3,6,7,1,4,3,9,6,9,
     *3,3,2,1,8,2,9,6,2,8,8,1,9,9,8,9,5,9,3,3,3,3,7,4,
     *9,3,3,4,4,9,0,9,7,1,1,2,5,0,0,2,4,0,7,2,9,8,4,3,
     *3,2,3,7,9,8,3,2,7,8,1,9,0,0,7,7,6,3,7,0,6,2,5,9,
     *4,9,7,0,2,0,1,3,1,8,4,0,7,2,7,3,8,8,7,2,3,4,3,1,
     *4,4,0,1,4,1,9,5,0,8,3,5,9,8,4,5,3,7,3,8,0,2,8,1,
     *0,4,4,1,4,5,4,9,2,5,2,4,6,1,5,3,5,3,0,9,1,4,6,4,
     *8,6,5,0,1,5,0,5,0,2,0,7,3,6,2,4,6,6,5,3,2,4,9,4,
     *6,2,2,0,6,8,2,4,4,8,5,4,7,9,5,6,3,8,8,4,5,0,1,8,
     *3,7,2,6,4,6,9,4,6,0,9,0,6,8,4,8,9,9,0,0,2,1,1,2,
     *3,4,3,4,1,5,3,1,9,4,6,9,3,0,4,2,2,8,0,3,0,1,2,3,
     *6,5,4,8,9,6,1,6,2,8,2,7,1,1,9,8,3,0,9,1,4,9,5,7,
     *4,1,4,4,3,5,3,1,9,3,0,8,4,3,4,7,6,9,8,5,1,1,1,4,
     *0,3,1,2,3,6,1,7,2,8,1,2,3,1,7,2,2,6,6,6,9,9,7,3,
     *4,6,8,9,5,9,0,5,8,6,5,2,7,6,6,4,0,5,5,4,2,4,1,4,
     *6,9,2,2,8,0,7,0,7,8,6,0,0,6,0,4,9,5,8,2,0,0,8,2,
     *5,6,5,5,2,5,1,4,5,6,7,4,0,1,8,0,2,5,0,0,2,5,0,0,
     *1,4,1,8,6,1,0,3,3,8,8,0,2,9,5,6,5,2,2,4,1,5,2,2/
      DATA (KRAND(I),I=1825, 2280)
     */1,6,9,5,2,8,7,8,7,3,7,4,7,5,9,8,4,1,7,1,3,6,8,2,
     *9,6,1,7,4,2,6,9,1,6,6,3,8,4,2,7,6,5,8,7,2,5,5,2,
     *8,2,6,5,7,1,9,6,0,8,2,0,1,8,3,0,2,3,0,3,5,2,0,7,
     *7,2,1,3,1,0,3,0,4,8,0,0,8,3,0,0,2,6,0,3,9,6,5,6,
     *6,9,7,9,3,7,0,0,9,0,5,0,7,0,6,4,2,8,2,1,7,4,3,7,
     *5,9,0,2,0,0,4,6,8,7,9,4,2,2,6,1,1,9,0,6,7,2,7,9,
     *1,1,0,9,6,2,1,3,8,0,9,7,7,1,9,6,0,6,9,8,0,8,6,2,
     *0,3,6,6,3,0,3,2,4,9,6,6,9,0,8,9,7,1,9,6,3,6,8,1,
     *2,7,4,0,2,1,8,3,8,2,2,0,8,3,9,6,6,6,2,5,3,6,1,1,
     *2,9,8,2,5,4,6,8,8,7,2,5,6,4,5,5,8,7,2,9,4,9,7,8,
     *4,5,9,5,4,6,5,9,5,8,0,7,8,6,6,7,2,3,9,1,7,3,0,6,
     *3,8,0,1,5,2,0,1,4,5,5,8,3,0,1,2,6,1,1,3,0,9,9,5,
     *9,3,4,5,0,9,5,3,1,4,5,6,8,3,7,9,2,8,4,1,9,8,1,5,
     *0,7,8,6,1,0,7,2,7,9,5,5,2,6,9,9,8,1,5,7,5,0,0,8,
     *3,3,8,5,3,6,7,6,0,6,9,1,2,5,2,8,2,6,9,4,6,9,2,9,
     *8,9,3,1,8,6,1,4,3,5,8,5,3,1,4,3,4,9,8,0,3,3,2,0,
     *4,6,0,0,2,9,4,8,7,7,2,8,6,1,9,7,7,0,3,1,5,2,2,7,
     *7,6,6,4,2,1,5,8,0,2,0,9,2,6,2,0,8,0,0,9,5,8,4,8,
     *9,5,6,8,5,3,9,4,2,8,9,6,7,9,0,4,6,3,6,8,8,3,5,8/
      DATA (KRAND(I),I=2281, 2736)
     */0,6,5,8,5,1,9,5,2,5,9,8,2,8,8,6,3,3,8,2,4,6,7,0,
     *7,7,7,8,1,4,4,8,2,2,3,1,0,1,5,0,3,7,4,8,2,7,6,9,
     *3,3,2,0,3,2,4,1,8,5,4,7,6,6,1,1,8,1,6,4,2,3,1,2,
     *2,7,8,5,2,7,6,9,6,1,7,6,2,4,8,0,2,1,9,5,9,0,0,4,
     *4,8,2,2,8,2,8,0,8,0,6,8,5,7,2,2,2,5,6,8,3,7,6,0,
     *0,4,4,0,8,4,9,4,8,1,8,0,3,8,8,6,7,5,2,0,0,3,9,1,
     *0,2,4,3,5,7,9,5,6,5,3,0,0,9,0,1,0,0,9,6,6,4,2,7,
     *7,6,6,5,8,1,0,3,5,6,4,3,5,9,9,7,6,8,8,8,5,9,1,1,
     *8,9,2,1,9,7,6,7,6,0,7,0,0,4,9,5,2,5,2,1,4,0,8,8,
     *5,2,4,6,9,4,7,6,3,5,4,4,2,5,0,6,1,8,7,8,9,5,9,8,
     *2,1,7,6,9,8,7,7,9,3,5,3,2,9,9,9,0,5,2,3,0,9,2,1,
     *3,3,0,1,4,3,0,4,1,0,3,3,4,9,6,3,6,7,1,4,3,9,6,9,
     *3,3,2,1,8,2,9,6,2,8,8,1,9,9,8,9,5,9,3,3,3,3,7,4,
     *9,3,3,4,4,9,0,9,7,1,1,2,5,0,0,2,4,0,7,2,9,8,4,3,
     *3,2,3,7,9,8,3,2,7,8,1,9,0,0,7,7,6,3,7,0,6,2,5,9,
     *4,9,7,0,2,0,1,3,1,8,4,0,7,2,7,3,8,8,7,2,3,4,3,1,
     *4,4,0,1,4,1,9,5,0,8,3,5,9,8,4,5,3,7,3,8,0,2,8,1,
     *0,4,4,1,4,5,4,9,2,5,2,4,6,1,5,3,5,3,0,9,1,4,6,4,
     *8,6,5,0,1,5,0,5,0,2,0,7,3,6,2,4,6,6,5,3,2,4,9,4/
      DATA (KRAND(I),I=2737, 3192)
     */6,2,2,0,6,8,2,4,4,8,5,4,7,9,5,6,3,8,8,4,5,0,1,8,
     *3,7,2,6,4,6,9,4,6,0,9,0,6,8,4,8,9,9,0,0,2,1,1,2,
     *3,4,3,4,1,5,3,1,9,4,6,9,3,0,4,2,2,8,0,3,0,1,2,3,
     *6,5,4,8,9,6,1,6,2,8,2,7,1,1,9,8,3,0,9,1,4,9,5,7,
     *4,1,4,4,3,5,3,1,9,3,0,8,4,3,4,7,6,9,8,5,1,1,1,4,
     *0,3,1,2,3,6,1,7,2,8,1,2,3,1,7,2,2,6,6,6,9,9,7,3,
     *4,6,8,9,5,9,0,5,8,6,5,2,7,6,6,4,0,5,5,4,2,4,1,4,
     *6,9,2,2,8,0,7,0,7,8,6,0,0,6,0,4,9,5,8,2,0,0,8,2,
     *5,6,5,5,2,5,1,4,5,6,7,4,0,1,8,0,2,5,0,0,2,5,0,0,
     *1,4,1,8,6,1,0,3,3,8,8,0,2,9,5,6,5,2,2,4,1,5,2,2,
     *1,6,9,5,2,8,7,8,7,3,7,4,7,5,9,8,4,1,7,1,3,6,8,2,
     *9,6,1,7,4,2,6,9,1,6,6,3,8,4,2,7,6,5,8,7,2,5,5,2,
     *8,2,6,5,7,1,9,6,0,8,2,0,1,8,3,0,2,3,0,3,5,2,0,7,
     *7,2,1,3,1,0,3,0,4,8,0,0,8,3,0,0,2,6,0,3,9,6,5,6,
     *6,9,7,9,3,7,0,0,9,0,5,0,7,0,6,4,2,8,2,1,7,4,3,7,
     *5,9,0,2,0,0,4,6,8,7,9,4,2,2,6,1,1,9,0,6,7,2,7,9,
     *1,1,0,9,6,2,1,3,8,0,9,7,7,1,9,6,0,6,9,8,0,8,6,2,
     *0,3,6,6,3,0,3,2,4,9,6,6,9,0,8,9,7,1,9,6,3,6,8,1,
     *2,7,4,0,2,1,8,3,8,2,2,0,8,3,9,6,6,6,2,5,3,6,1,1/
      DATA (KRAND(I),I=3193, 3648)
     */2,9,8,2,5,4,6,8,8,7,2,5,6,4,5,5,8,7,2,9,4,9,7,8,
     *4,5,9,5,4,6,5,9,5,8,0,7,8,6,6,7,2,3,9,1,7,3,0,6,
     *3,8,0,1,5,2,0,1,4,5,5,8,3,0,1,2,6,1,1,3,0,9,9,5,
     *9,3,4,5,0,9,5,3,1,4,5,6,8,3,7,9,2,8,4,1,9,8,1,5,
     *0,7,8,6,1,0,7,2,7,9,5,5,2,6,9,9,8,1,5,7,5,0,0,8,
     *3,3,8,5,3,6,7,6,0,6,9,1,2,5,2,8,2,6,9,4,6,9,2,9,
     *8,9,3,1,8,6,1,4,3,5,8,5,3,1,4,3,4,9,8,0,3,3,2,0,
     *4,6,0,0,2,9,4,8,7,7,2,8,6,1,9,7,7,0,3,1,5,2,2,7,
     *7,6,6,4,2,1,5,8,0,2,0,9,2,6,2,0,8,0,0,9,5,8,4,8,
     *9,5,6,8,5,3,9,4,2,8,9,6,7,9,0,4,6,3,6,8,8,3,5,8,
     *0,6,5,8,5,1,9,5,2,5,9,8,2,8,8,6,3,3,8,2,4,6,7,0,
     *7,7,7,8,1,4,4,8,2,2,3,1,0,1,5,0,3,7,4,8,2,7,6,9,
     *3,3,2,0,3,2,4,1,8,5,4,7,6,6,1,1,8,1,6,4,2,3,1,2,
     *2,7,8,5,2,7,6,9,6,1,7,6,2,4,8,0,2,1,9,5,9,0,0,4,
     *4,8,2,2,8,2,8,0,8,0,6,8,5,7,2,2,2,5,6,8,3,7,6,0,
     *0,4,4,0,8,4,9,4,8,1,8,0,3,8,8,6,7,5,2,0,0,3,9,1,
     *0,2,4,3,5,7,9,5,6,5,3,0,0,9,0,1,0,0,9,6,6,4,2,7,
     *7,6,6,5,8,1,0,3,5,6,4,3,5,9,9,7,6,8,8,8,5,9,1,1,
     *8,9,2,1,9,7,6,7,6,0,7,0,0,4,9,5,2,5,2,1,4,0,8,8/
      DATA (KRAND(I),I=3649, 4104)
     */5,2,4,6,9,4,7,6,3,5,4,4,2,5,0,6,1,8,7,8,9,5,9,8,
     *2,1,7,6,9,8,7,7,9,3,5,3,2,9,9,9,0,5,2,3,0,9,2,1,
     *3,3,0,1,4,3,0,4,1,0,3,3,4,9,6,3,6,7,1,4,3,9,6,9,
     *3,3,2,1,8,2,9,6,2,8,8,1,9,9,8,9,5,9,3,3,3,3,7,4,
     *9,3,3,4,4,9,0,9,7,1,1,2,5,0,0,2,4,0,7,2,9,8,4,3,
     *3,2,3,7,9,8,3,2,7,8,1,9,0,0,7,7,6,3,7,0,6,2,5,9,
     *4,9,7,0,2,0,1,3,1,8,4,0,7,2,7,3,8,8,7,2,3,4,3,1,
     *4,4,0,1,4,1,9,5,0,8,3,5,9,8,4,5,3,7,3,8,0,2,8,1,
     *0,4,4,1,4,5,4,9,2,5,2,4,6,1,5,3,5,3,0,9,1,4,6,4,
     *8,6,5,0,1,5,0,5,0,2,0,7,3,6,2,4,6,6,5,3,2,4,9,4,
     *6,2,2,0,6,8,2,4,4,8,5,4,7,9,5,6,3,8,8,4,5,0,1,8,
     *3,7,2,6,4,6,9,4,6,0,9,0,6,8,4,8,9,9,0,0,2,1,1,2,
     *3,4,3,4,1,5,3,1,9,4,6,9,3,0,4,2,2,8,0,3,0,1,2,3,
     *6,5,4,8,9,6,1,6,2,8,2,7,1,1,9,8,3,0,9,1,4,9,5,7,
     *4,1,4,4,3,5,3,1,9,3,0,8,4,3,4,7,6,9,8,5,1,1,1,4,
     *0,3,1,2,3,6,1,7,2,8,1,2,3,1,7,2,2,6,6,6,9,9,7,3,
     *4,6,8,9,5,9,0,5,8,6,5,2,7,6,6,4,0,5,5,4,2,4,1,4,
     *6,9,2,2,8,0,7,0,7,8,6,0,0,6,0,4,9,5,8,2,0,0,8,2,
     *5,6,5,5,2,5,1,4,5,6,7,4,0,1,8,0,2,5,0,0,2,5,0,0/
      DATA (KRAND(I),I=4105, 4560)
     */1,4,1,8,6,1,0,3,3,8,8,0,2,9,5,6,5,2,2,4,1,5,2,2,
     *1,6,9,5,2,8,7,8,7,3,7,4,7,5,9,8,4,1,7,1,3,6,8,2,
     *9,6,1,7,4,2,6,9,1,6,6,3,8,4,2,7,6,5,8,7,2,5,5,2,
     *8,2,6,5,7,1,9,6,0,8,2,0,1,8,3,0,2,3,0,3,5,2,0,7,
     *7,2,1,3,1,0,3,0,4,8,0,0,8,3,0,0,2,6,0,3,9,6,5,6,
     *6,9,7,9,3,7,0,0,9,0,5,0,7,0,6,4,2,8,2,1,7,4,3,7,
     *5,9,0,2,0,0,4,6,8,7,9,4,2,2,6,1,1,9,0,6,7,2,7,9,
     *1,1,0,9,6,2,1,3,8,0,9,7,7,1,9,6,0,6,9,8,0,8,6,2,
     *0,3,6,6,3,0,3,2,4,9,6,6,9,0,8,9,7,1,9,6,3,6,8,1,
     *2,7,4,0,2,1,8,3,8,2,2,0,8,3,9,6,6,6,2,5,3,6,1,1,
     *2,9,8,2,5,4,6,8,8,7,2,5,6,4,5,5,8,7,2,9,4,9,7,8,
     *4,5,9,5,4,6,5,9,5,8,0,7,8,6,6,7,2,3,9,1,7,3,0,6,
     *3,8,0,1,5,2,0,1,4,5,5,8,3,0,1,2,6,1,1,3,0,9,9,5,
     *9,3,4,5,0,9,5,3,1,4,5,6,8,3,7,9,2,8,4,1,9,8,1,5,
     *0,7,8,6,1,0,7,2,7,9,5,5,2,6,9,9,8,1,5,7,5,0,0,8,
     *3,3,8,5,3,6,7,6,0,6,9,1,2,5,2,8,2,6,9,4,6,9,2,9,
     *8,9,3,1,8,6,1,4,3,5,8,5,3,1,4,3,4,9,8,0,3,3,2,0,
     *4,6,0,0,2,9,4,8,7,7,2,8,6,1,9,7,7,0,3,1,5,2,2,7,
     *7,6,6,4,2,1,5,8,0,2,0,9,2,6,2,0,8,0,0,9,5,8,4,8/
      DATA (KRAND(I),I=4561, 5016)
     */9,5,6,8,5,3,9,4,2,8,9,6,7,9,0,4,6,3,6,8,8,3,5,8,
     *0,6,5,8,5,1,9,5,2,5,9,8,2,8,8,6,3,3,8,2,4,6,7,0,
     *7,7,7,8,1,4,4,8,2,2,3,1,0,1,5,0,3,7,4,8,2,7,6,9,
     *3,3,2,0,3,2,4,1,8,5,4,7,6,6,1,1,8,1,6,4,2,3,1,2,
     *2,7,8,5,2,7,6,9,6,1,7,6,2,4,8,0,2,1,9,5,9,0,0,4,
     *4,8,2,2,8,2,8,0,8,0,6,8,5,7,2,2,2,5,6,8,3,7,6,0,
     *0,4,4,0,8,4,9,4,8,1,8,0,3,8,8,6,7,5,2,0,0,3,9,1,
     *0,2,4,3,5,7,9,5,6,5,3,0,0,9,0,1,0,0,9,6,6,4,2,7,
     *7,6,6,5,8,1,0,3,5,6,4,3,5,9,9,7,6,8,8,8,5,9,1,1,
     *8,9,2,1,9,7,6,7,6,0,7,0,0,4,9,5,2,5,2,1,4,0,8,8,
     *5,2,4,6,9,4,7,6,3,5,4,4,2,5,0,6,1,8,7,8,9,5,9,8,
     *2,1,7,6,9,8,7,7,9,3,5,3,2,9,9,9,0,5,2,3,0,9,2,1,
     *3,3,0,1,4,3,0,4,1,0,3,3,4,9,6,3,6,7,1,4,3,9,6,9,
     *3,3,2,1,8,2,9,6,2,8,8,1,9,9,8,9,5,9,3,3,3,3,7,4,
     *9,3,3,4,4,9,0,9,7,1,1,2,5,0,0,2,4,0,7,2,9,8,4,3,
     *3,2,3,7,9,8,3,2,7,8,1,9,0,0,7,7,6,3,7,0,6,2,5,9,
     *4,9,7,0,2,0,1,3,1,8,4,0,7,2,7,3,8,8,7,2,3,4,3,1,
     *4,4,0,1,4,1,9,5,0,8,3,5,9,8,4,5,3,7,3,8,0,2,8,1,
     *0,4,4,1,4,5,4,9,2,5,2,4,6,1,5,3,5,3,0,9,1,4,6,4/
      DATA (KRAND(I),I=5017, 5472)
     */8,6,5,0,1,5,0,5,0,2,0,7,3,6,2,4,6,6,5,3,2,4,9,4,
     *6,2,2,0,6,8,2,4,4,8,5,4,7,9,5,6,3,8,8,4,5,0,1,8,
     *3,7,2,6,4,6,9,4,6,0,9,0,6,8,4,8,9,9,0,0,2,1,1,2,
     *3,4,3,4,1,5,3,1,9,4,6,9,3,0,4,2,2,8,0,3,0,1,2,3,
     *6,5,4,8,9,6,1,6,2,8,2,7,1,1,9,8,3,0,9,1,4,9,5,7,
     *4,1,4,4,3,5,3,1,9,3,0,8,4,3,4,7,6,9,8,5,1,1,1,4,
     *0,3,1,2,3,6,1,7,2,8,1,2,3,1,7,2,2,6,6,6,9,9,7,3,
     *4,6,8,9,5,9,0,5,8,6,5,2,7,6,6,4,0,5,5,4,2,4,1,4,
     *6,9,2,2,8,0,7,0,7,8,6,0,0,6,0,4,9,5,8,2,0,0,8,2,
     *5,6,5,5,2,5,1,4,5,6,7,4,0,1,8,0,2,5,0,0,2,5,0,0,
     *1,4,1,8,6,1,0,3,3,8,8,0,2,9,5,6,5,2,2,4,1,5,2,2,
     *1,6,9,5,2,8,7,8,7,3,7,4,7,5,9,8,4,1,7,1,3,6,8,2,
     *9,6,1,7,4,2,6,9,1,6,6,3,8,4,2,7,6,5,8,7,2,5,5,2,
     *8,2,6,5,7,1,9,6,0,8,2,0,1,8,3,0,2,3,0,3,5,2,0,7,
     *7,2,1,3,1,0,3,0,4,8,0,0,8,3,0,0,2,6,0,3,9,6,5,6,
     *6,9,7,9,3,7,0,0,9,0,5,0,7,0,6,4,2,8,2,1,7,4,3,7,
     *5,9,0,2,0,0,4,6,8,7,9,4,2,2,6,1,1,9,0,6,7,2,7,9,
     *1,1,0,9,6,2,1,3,8,0,9,7,7,1,9,6,0,6,9,8,0,8,6,2,
     *0,3,6,6,3,0,3,2,4,9,6,6,9,0,8,9,7,1,9,6,3,6,8,1/
      DATA (KRAND(I),I=5473, 5928)
     */2,7,4,0,2,1,8,3,8,2,2,0,8,3,9,6,6,6,2,5,3,6,1,1,
     *2,9,8,2,5,4,6,8,8,7,2,5,6,4,5,5,8,7,2,9,4,9,7,8,
     *4,5,9,5,4,6,5,9,5,8,0,7,8,6,6,7,2,3,9,1,7,3,0,6,
     *3,8,0,1,5,2,0,1,4,5,5,8,3,0,1,2,6,1,1,3,0,9,9,5,
     *9,3,4,5,0,9,5,3,1,4,5,6,8,3,7,9,2,8,4,1,9,8,1,5,
     *0,7,8,6,1,0,7,2,7,9,5,5,2,6,9,9,8,1,5,7,5,0,0,8,
     *3,3,8,5,3,6,7,6,0,6,9,1,2,5,2,8,2,6,9,4,6,9,2,9,
     *8,9,3,1,8,6,1,4,3,5,8,5,3,1,4,3,4,9,8,0,3,3,2,0,
     *4,6,0,0,2,9,4,8,7,7,2,8,6,1,9,7,7,0,3,1,5,2,2,7,
     *7,6,6,4,2,1,5,8,0,2,0,9,2,6,2,0,8,0,0,9,5,8,4,8,
     *9,5,6,8,5,3,9,4,2,8,9,6,7,9,0,4,6,3,6,8,8,3,5,8,
     *0,6,5,8,5,1,9,5,2,5,9,8,2,8,8,6,3,3,8,2,4,6,7,0,
     *7,7,7,8,1,4,4,8,2,2,3,1,0,1,5,0,3,7,4,8,2,7,6,9,
     *3,3,2,0,3,2,4,1,8,5,4,7,6,6,1,1,8,1,6,4,2,3,1,2,
     *2,7,8,5,2,7,6,9,6,1,7,6,2,4,8,0,2,1,9,5,9,0,0,4,
     *4,8,2,2,8,2,8,0,8,0,6,8,5,7,2,2,2,5,6,8,3,7,6,0,
     *0,4,4,0,8,4,9,4,8,1,8,0,3,8,8,6,7,5,2,0,0,3,9,1,
     *0,2,4,3,5,7,9,5,6,5,3,0,0,9,0,1,0,0,9,6,6,4,2,7,
     *7,6,6,5,8,1,0,3,5,6,4,3,5,9,9,7,6,8,8,8,5,9,1,1/
      DATA (KRAND(I),I=5929, 6384)
     */8,9,2,1,9,7,6,7,6,0,7,0,0,4,9,5,2,5,2,1,4,0,8,8,
     *5,2,4,6,9,4,7,6,3,5,4,4,2,5,0,6,1,8,7,8,9,5,9,8,
     *2,1,7,6,9,8,7,7,9,3,5,3,2,9,9,9,0,5,2,3,0,9,2,1,
     *3,3,0,1,4,3,0,4,1,0,3,3,4,9,6,3,6,7,1,4,3,9,6,9,
     *3,3,2,1,8,2,9,6,2,8,8,1,9,9,8,9,5,9,3,3,3,3,7,4,
     *9,3,3,4,4,9,0,9,7,1,1,2,5,0,0,2,4,0,7,2,9,8,4,3,
     *3,2,3,7,9,8,3,2,7,8,1,9,0,0,7,7,6,3,7,0,6,2,5,9,
     *4,9,7,0,2,0,1,3,1,8,4,0,7,2,7,3,8,8,7,2,3,4,3,1,
     *4,4,0,1,4,1,9,5,0,8,3,5,9,8,4,5,3,7,3,8,0,2,8,1,
     *0,4,4,1,4,5,4,9,2,5,2,4,6,1,5,3,5,3,0,9,1,4,6,4,
     *8,6,5,0,1,5,0,5,0,2,0,7,3,6,2,4,6,6,5,3,2,4,9,4,
     *6,2,2,0,6,8,2,4,4,8,5,4,7,9,5,6,3,8,8,4,5,0,1,8,
     *3,7,2,6,4,6,9,4,6,0,9,0,6,8,4,8,9,9,0,0,2,1,1,2,
     *3,4,3,4,1,5,3,1,9,4,6,9,3,0,4,2,2,8,0,3,0,1,2,3,
     *6,5,4,8,9,6,1,6,2,8,2,7,1,1,9,8,3,0,9,1,4,9,5,7,
     *4,1,4,4,3,5,3,1,9,3,0,8,4,3,4,7,6,9,8,5,1,1,1,4,
     *0,3,1,2,3,6,1,7,2,8,1,2,3,1,7,2,2,6,6,6,9,9,7,3,
     *4,6,8,9,5,9,0,5,8,6,5,2,7,6,6,4,0,5,5,4,2,4,1,4,
     *6,9,2,2,8,0,7,0,7,8,6,0,0,6,0,4,9,5,8,2,0,0,8,2/
      DATA (KRAND(I),I=6385, 6840)
     */5,6,5,5,2,5,1,4,5,6,7,4,0,1,8,0,2,5,0,0,2,5,0,0,
     *1,4,1,8,6,1,0,3,3,8,8,0,2,9,5,6,5,2,2,4,1,5,2,2,
     *1,6,9,5,2,8,7,8,7,3,7,4,7,5,9,8,4,1,7,1,3,6,8,2,
     *9,6,1,7,4,2,6,9,1,6,6,3,8,4,2,7,6,5,8,7,2,5,5,2,
     *8,2,6,5,7,1,9,6,0,8,2,0,1,8,3,0,2,3,0,3,5,2,0,7,
     *7,2,1,3,1,0,3,0,4,8,0,0,8,3,0,0,2,6,0,3,9,6,5,6,
     *6,9,7,9,3,7,0,0,9,0,5,0,7,0,6,4,2,8,2,1,7,4,3,7,
     *5,9,0,2,0,0,4,6,8,7,9,4,2,2,6,1,1,9,0,6,7,2,7,9,
     *1,1,0,9,6,2,1,3,8,0,9,7,7,1,9,6,0,6,9,8,0,8,6,2,
     *0,3,6,6,3,0,3,2,4,9,6,6,9,0,8,9,7,1,9,6,3,6,8,1,
     *2,7,4,0,2,1,8,3,8,2,2,0,8,3,9,6,6,6,2,5,3,6,1,1,
     *2,9,8,2,5,4,6,8,8,7,2,5,6,4,5,5,8,7,2,9,4,9,7,8,
     *4,5,9,5,4,6,5,9,5,8,0,7,8,6,6,7,2,3,9,1,7,3,0,6,
     *3,8,0,1,5,2,0,1,4,5,5,8,3,0,1,2,6,1,1,3,0,9,9,5,
     *9,3,4,5,0,9,5,3,1,4,5,6,8,3,7,9,2,8,4,1,9,8,1,5,
     *0,7,8,6,1,0,7,2,7,9,5,5,2,6,9,9,8,1,5,7,5,0,0,8,
     *3,3,8,5,3,6,7,6,0,6,9,1,2,5,2,8,2,6,9,4,6,9,2,9,
     *8,9,3,1,8,6,1,4,3,5,8,5,3,1,4,3,4,9,8,0,3,3,2,0,
     *4,6,0,0,2,9,4,8,7,7,2,8,6,1,9,7,7,0,3,1,5,2,2,7/
      DATA (KRAND(I),I=6841, 7296)
     */7,6,6,4,2,1,5,8,0,2,0,9,2,6,2,0,8,0,0,9,5,8,4,8,
     *9,5,6,8,5,3,9,4,2,8,9,6,7,9,0,4,6,3,6,8,8,3,5,8,
     *0,6,5,8,5,1,9,5,2,5,9,8,2,8,8,6,3,3,8,2,4,6,7,0,
     *7,7,7,8,1,4,4,8,2,2,3,1,0,1,5,0,3,7,4,8,2,7,6,9,
     *3,3,2,0,3,2,4,1,8,5,4,7,6,6,1,1,8,1,6,4,2,3,1,2,
     *2,7,8,5,2,7,6,9,6,1,7,6,2,4,8,0,2,1,9,5,9,0,0,4,
     *4,8,2,2,8,2,8,0,8,0,6,8,5,7,2,2,2,5,6,8,3,7,6,0,
     *0,4,4,0,8,4,9,4,8,1,8,0,3,8,8,6,7,5,2,0,0,3,9,1,
     *0,2,4,3,5,7,9,5,6,5,3,0,0,9,0,1,0,0,9,6,6,4,2,7,
     *7,6,6,5,8,1,0,3,5,6,4,3,5,9,9,7,6,8,8,8,5,9,1,1,
     *8,9,2,1,9,7,6,7,6,0,7,0,0,4,9,5,2,5,2,1,4,0,8,8,
     *5,2,4,6,9,4,7,6,3,5,4,4,2,5,0,6,1,8,7,8,9,5,9,8,
     *2,1,7,6,9,8,7,7,9,3,5,3,2,9,9,9,0,5,2,3,0,9,2,1,
     *3,3,0,1,4,3,0,4,1,0,3,3,4,9,6,3,6,7,1,4,3,9,6,9,
     *3,3,2,1,8,2,9,6,2,8,8,1,9,9,8,9,5,9,3,3,3,3,7,4,
     *9,3,3,4,4,9,0,9,7,1,1,2,5,0,0,2,4,0,7,2,9,8,4,3,
     *3,2,3,7,9,8,3,2,7,8,1,9,0,0,7,7,6,3,7,0,6,2,5,9,
     *4,9,7,0,2,0,1,3,1,8,4,0,7,2,7,3,8,8,7,2,3,4,3,1,
     *4,4,0,1,4,1,9,5,0,8,3,5,9,8,4,5,3,7,3,8,0,2,8,1/
      DATA (KRAND(I),I=7297, 7752)
     */0,4,4,1,4,5,4,9,2,5,2,4,6,1,5,3,5,3,0,9,1,4,6,4,
     *8,6,5,0,1,5,0,5,0,2,0,7,3,6,2,4,6,6,5,3,2,4,9,4,
     *6,2,2,0,6,8,2,4,4,8,5,4,7,9,5,6,3,8,8,4,5,0,1,8,
     *3,7,2,6,4,6,9,4,6,0,9,0,6,8,4,8,9,9,0,0,2,1,1,2,
     *3,4,3,4,1,5,3,1,9,4,6,9,3,0,4,2,2,8,0,3,0,1,2,3,
     *6,5,4,8,9,6,1,6,2,8,2,7,1,1,9,8,3,0,9,1,4,9,5,7,
     *4,1,4,4,3,5,3,1,9,3,0,8,4,3,4,7,6,9,8,5,1,1,1,4,
     *0,3,1,2,3,6,1,7,2,8,1,2,3,1,7,2,2,6,6,6,9,9,7,3,
     *4,6,8,9,5,9,0,5,8,6,5,2,7,6,6,4,0,5,5,4,2,4,1,4,
     *6,9,2,2,8,0,7,0,7,8,6,0,0,6,0,4,9,5,8,2,0,0,8,2,
     *5,6,5,5,2,5,1,4,5,6,7,4,0,1,8,0,2,5,0,0,2,5,0,0,
     *1,4,1,8,6,1,0,3,3,8,8,0,2,9,5,6,5,2,2,4,1,5,2,2,
     *1,6,9,5,2,8,7,8,7,3,7,4,7,5,9,8,4,1,7,1,3,6,8,2,
     *9,6,1,7,4,2,6,9,1,6,6,3,8,4,2,7,6,5,8,7,2,5,5,2,
     *8,2,6,5,7,1,9,6,0,8,2,0,1,8,3,0,2,3,0,3,5,2,0,7,
     *7,2,1,3,1,0,3,0,4,8,0,0,8,3,0,0,2,6,0,3,9,6,5,6,
     *6,9,7,9,3,7,0,0,9,0,5,0,7,0,6,4,2,8,2,1,7,4,3,7,
     *5,9,0,2,0,0,4,6,8,7,9,4,2,2,6,1,1,9,0,6,7,2,7,9,
     *1,1,0,9,6,2,1,3,8,0,9,7,7,1,9,6,0,6,9,8,0,8,6,2/
      DATA (KRAND(I),I=7753, 8208)
     */0,3,6,6,3,0,3,2,4,9,6,6,9,0,8,9,7,1,9,6,3,6,8,1,
     *2,7,4,0,2,1,8,3,8,2,2,0,8,3,9,6,6,6,2,5,3,6,1,1,
     *2,9,8,2,5,4,6,8,8,7,2,5,6,4,5,5,8,7,2,9,4,9,7,8,
     *4,5,9,5,4,6,5,9,5,8,0,7,8,6,6,7,2,3,9,1,7,3,0,6,
     *3,8,0,1,5,2,0,1,4,5,5,8,3,0,1,2,6,1,1,3,0,9,9,5,
     *9,3,4,5,0,9,5,3,1,4,5,6,8,3,7,9,2,8,4,1,9,8,1,5,
     *0,7,8,6,1,0,7,2,7,9,5,5,2,6,9,9,8,1,5,7,5,0,0,8,
     *3,3,8,5,3,6,7,6,0,6,9,1,2,5,2,8,2,6,9,4,6,9,2,9,
     *8,9,3,1,8,6,1,4,3,5,8,5,3,1,4,3,4,9,8,0,3,3,2,0,
     *4,6,0,0,2,9,4,8,7,7,2,8,6,1,9,7,7,0,3,1,5,2,2,7,
     *7,6,6,4,2,1,5,8,0,2,0,9,2,6,2,0,8,0,0,9,5,8,4,8,
     *9,5,6,8,5,3,9,4,2,8,9,6,7,9,0,4,6,3,6,8,8,3,5,8,
     *0,6,5,8,5,1,9,5,2,5,9,8,2,8,8,6,3,3,8,2,4,6,7,0,
     *7,7,7,8,1,4,4,8,2,2,3,1,0,1,5,0,3,7,4,8,2,7,6,9,
     *3,3,2,0,3,2,4,1,8,5,4,7,6,6,1,1,8,1,6,4,2,3,1,2,
     *2,7,8,5,2,7,6,9,6,1,7,6,2,4,8,0,2,1,9,5,9,0,0,4,
     *4,8,2,2,8,2,8,0,8,0,6,8,5,7,2,2,2,5,6,8,3,7,6,0,
     *0,4,4,0,8,4,9,4,8,1,8,0,3,8,8,6,7,5,2,0,0,3,9,1,
     *0,2,4,3,5,7,9,5,6,5,3,0,0,9,0,1,0,0,9,6,6,4,2,7/
      DATA (KRAND(I),I=8209, 8664)
     */7,6,6,5,8,1,0,3,5,6,4,3,5,9,9,7,6,8,8,8,5,9,1,1,
     *8,9,2,1,9,7,6,7,6,0,7,0,0,4,9,5,2,5,2,1,4,0,8,8,
     *5,2,4,6,9,4,7,6,3,5,4,4,2,5,0,6,1,8,7,8,9,5,9,8,
     *2,1,7,6,9,8,7,7,9,3,5,3,2,9,9,9,0,5,2,3,0,9,2,1,
     *3,3,0,1,4,3,0,4,1,0,3,3,4,9,6,3,6,7,1,4,3,9,6,9,
     *3,3,2,1,8,2,9,6,2,8,8,1,9,9,8,9,5,9,3,3,3,3,7,4,
     *9,3,3,4,4,9,0,9,7,1,1,2,5,0,0,2,4,0,7,2,9,8,4,3,
     *3,2,3,7,9,8,3,2,7,8,1,9,0,0,7,7,6,3,7,0,6,2,5,9,
     *4,9,7,0,2,0,1,3,1,8,4,0,7,2,7,3,8,8,7,2,3,4,3,1,
     *4,4,0,1,4,1,9,5,0,8,3,5,9,8,4,5,3,7,3,8,0,2,8,1,
     *0,4,4,1,4,5,4,9,2,5,2,4,6,1,5,3,5,3,0,9,1,4,6,4,
     *8,6,5,0,1,5,0,5,0,2,0,7,3,6,2,4,6,6,5,3,2,4,9,4,
     *6,2,2,0,6,8,2,4,4,8,5,4,7,9,5,6,3,8,8,4,5,0,1,8,
     *3,7,2,6,4,6,9,4,6,0,9,0,6,8,4,8,9,9,0,0,2,1,1,2,
     *3,4,3,4,1,5,3,1,9,4,6,9,3,0,4,2,2,8,0,3,0,1,2,3,
     *6,5,4,8,9,6,1,6,2,8,2,7,1,1,9,8,3,0,9,1,4,9,5,7,
     *4,1,4,4,3,5,3,1,9,3,0,8,4,3,4,7,6,9,8,5,1,1,1,4,
     *0,3,1,2,3,6,1,7,2,8,1,2,3,1,7,2,2,6,6,6,9,9,7,3,
     *4,6,8,9,5,9,0,5,8,6,5,2,7,6,6,4,0,5,5,4,2,4,1,4/
      DATA (KRAND(I),I=8665, 8784)
     */6,9,2,2,8,0,7,0,7,8,6,0,0,6,0,4,9,5,8,2,0,0,8,2,
     *5,6,5,5,2,5,1,4,5,6,7,4,0,1,8,0,2,5,0,0,2,5,0,0,
     *1,4,1,8,6,1,0,3,3,8,8,0,2,9,5,6,5,2,2,4,1,5,2,2,
     *1,6,9,5,2,8,7,8,7,3,7,4,7,5,9,8,4,1,7,1,3,6,8,2,
     *9,6,1,7,4,2,6,9,1,6,6,3,8,4,2,7,6,5,8,7,2,5,5,2/

      DATA ERRCOD(1)/'1001'/,
     &ERRMSG(1)/'ERROR OPENING THE INPUT FILE:'/

      DATA ERRCOD(2)/'1002'/,
     &ERRMSG(2)/'ERROR OPENING FILE FOR SCREEN INPUT RESPONSES'/

      DATA ERRCOD(3)/'1003'/,
     &ERRMSG(3)/'ERROR READING THE INPUT FILE: DEPOSITION TYPE'/

      DATA ERRCOD(4)/'1004'/,
     &ERRMSG(4)/'INCORRECT DEPOSITION TYPE SPECIFIED:'/

C     The error code 1005 has been removed

      DATA ERRCOD(6)/'1006'/,
     &ERRMSG(6)/'ERROR READING THE INPUT FILE: OUTPUT FILE TYPE'/

      DATA ERRCOD(7)/'1007'/,
     &ERRMSG(7)/'ERROR READING THE INPUT FILE: SURFACE FILE NAME'/

      DATA ERRCOD(8)/'1008'/,
     &ERRMSG(8)/'ERROR READING THE INPUT FILE: SURFACE FILE TYPE'/

      DATA ERRCOD(9)/'1009'/,
     &ERRMSG(9)/'ERROR READING THE INPUT FILE: MIXING HEIGHT FILE NAME'/

      DATA ERRCOD(10)/'1010'/,
     &ERRMSG(10)/'ERROR READING THE INPUT FILE: PRECIPITATION YES|NO'/

      DATA ERRCOD(11)/'1011'/,
     &ERRMSG(11)/'ERROR READING THE INPUT FILE: PRECIP FILE NAME'/

      DATA ERRCOD(12)/'1012'/,
     &ERRMSG(12)/'ERROR READING THE INPUT FILE: PRECIP FILE TYPE'/

      DATA ERRCOD(13)/'1013'/,
     &ERRMSG(13)/'ERROR READING THE INPUT FILE: SITE LATITUDE'/

      DATA ERRCOD(14)/'1014'/,
     &ERRMSG(14)/'ERROR READING THE INPUT FILE: SITE LONGITUDE'/

      DATA ERRCOD(15)/'1015'/,
     &ERRMSG(15)/'ERROR READING THE INPUT FILE: MIN OBUKHOV LENGTH'/

      DATA ERRCOD(16)/'1016'/,
     &ERRMSG(16)/'ERROR READING THE INPUT FILE: ANEMOMETER HGT'/

      DATA ERRCOD(23)/'1023'/,
     &ERRMSG(23)/'ERROR OPENING THE SURFACE FILE:'/

      DATA ERRCOD(24)/'1024'/,
     &ERRMSG(24)/'ERROR OPENING THE MIXING HEIGHT FILE:'/

      DATA ERRCOD(25)/'1025'/,
     &ERRMSG(25)/'ERROR OPENING THE PRECIPITATION FILE:'/

      DATA ERRCOD(26)/'1026'/,
     &ERRMSG(26)/'PRECIP FILE NOT VARIABLE FORMAT AS SPECIFIED BY USER'/

      DATA ERRCOD(27)/'1027'/,
     &ERRMSG(27)/'ERROR OPENING TEMPORARY PRECIPITATION FILE'/

      DATA ERRCOD(28)/'1028'/,
     &ERRMSG(28)/'PRECIP FILE NOT FIXED FORMAT AS SPECIFIED BY USER'/

      DATA ERRCOD(29)/'1029'/,
     &ERRMSG(29)/'ERROR OPENING PRECIPITATION FILE:'/

      DATA ERRCOD(30)/'1030'/,
     &ERRMSG(30)/'ERROR OPENING OUTPUT FILE (UNFORM):'/

      DATA ERRCOD(31)/'1031'/,
     &ERRMSG(31)/'ERROR OPENING OUTPUT FILE (ASCII):'/

      DATA ERRCOD(32)/'1032'/,
     &ERRMSG(32)/'SURFACE FILE NOT OF TYPE CD-144 AS SPECIFIED BY USER'/

      DATA ERRCOD(33)/'1033'/,
     &ERRMSG(33)/'SURFACE FILE NOT OF TYPE SAMSON AS SPECIFIED BY USER'/

      DATA ERRCOD(34)/'1034'/,
     &ERRMSG(34)/'SURFACE FILE NOT SCRAM FORMAT AS SPECIFIED BY USER'/

      DATA ERRCOD(35)/'1035'/,
     &ERRMSG(35)/'ERROR READING HEADER RECORD OF THE SURFACE FILE:'/

      DATA ERRCOD(36)/'1036'/,
     &ERRMSG(36)/'ILLEGAL VARIABLE ID IN HEADER RECORD:'/

      DATA ERRCOD(37)/'1037'/,
     &ERRMSG(37)/'ERROR READING SAMSON SURFACE DATA:'/

      DATA ERRCOD(38)/'1038'/,
     &ERRMSG(38)/'ERROR READING CD144 SURFACE DATA:'/

      DATA ERRCOD(39)/'1039'/,
     &ERRMSG(39)/'ERROR READING MIXING HEIGHT DATA:'/

      DATA ERRCOD(40)/'1040'/,
     &ERRMSG(40)/'SURFACE AND MIXING HEIGHT TIME STAMP MISMATCH:'/

      DATA ERRCOD(41)/'1041'/,
     &ERRMSG(41)/'SURFACE AND PRECIP DATA TIME STAMP MISMATCH:'/

      DATA ERRCOD(42)/'1042'/,
     &ERRMSG(42)/'STABILITY CALCULATED TO BE LESS THAN 1:'/

      DATA ERRCOD(43)/'1043'/,
     &ERRMSG(43)/'SCRAM DATA and WET DEPOSITION NOT COMPATIBLE'/

      DATA ERRCOD(44)/'1101'/,
     &ERRMSG(44)/'ERROR READING TIME ZONE FROM THE INPUT FILE'/

      DATA ERRCOD(45)/'1110'/,
     &ERRMSG(45)/'ANEMOMETER HT < ROUGHNESS LENGTH       '/

      DATA ERRCOD(46)/'1115'/,
     &ERRMSG(46)/'ANEMOMETER HT < 0.0                    '/

      DATA ERRCOD(47)/'1120'/,
     & ERRMSG(47)/'INSUFFICIENT DATA IN SAMSON FILE FOR PROCESSING'/

      DATA ERRCOD(48)/'1125'/,
     & ERRMSG(48)/'INSUFFICIENT DATA IN SAMSON FILE FOR WET DEPOSITION'/

      DATA ERRCOD(49)/'1130'/,
     & ERRMSG(49)/'ALBEDO OUT OF RANGE                               '/

      DATA ERRCOD(50)/'1135'/,
     & ERRMSG(50)/'NET RADIATION ABSORBED BY GROUND OUT OF RANGE     '/

      DATA ERRCOD(51)/'1140'/,
     & ERRMSG(51)/'INSUFFICIENT DATA IN HUSWO FILE FOR PROCESSING'/

      DATA ERRCOD(52)/'1145'/,
     & ERRMSG(52)/'INSUFFICIENT DATA IN HUSWO FILE FOR WET DEPOSITION'/

      DATA ERRCOD(53)/'1044'/,
     &ERRMSG(53)/'SURFACE FILE NOT OF TYPE HUSWO AS SPECIFIED BY USER'/

      DATA ERRCOD(54)/'1045'/,
     &ERRMSG(54)/'ERROR READING HUSWO SURFACE DATA:'/

      DATA ERRCOD(55)/'1126'/,
     &ERRMSG(55)( 1:43)/'GLOBAL RAD''N OR REL HUMIDITY NOT EXTRACTED '/,
     &ERRMSG(55)(44:61)/'FOR DEPOSITION RUN'/

C     The following 'error message' is a place holder for messages that
C     do not conform to the standard PCRAMMET format (e.g., in SUBR.PREAD)

      DATA ERRCOD(60)/'0000'/,
     & ERRMSG(60)/'                                                  '/

      END
