C***********************************************************************SCR00005
C                                                                       SCR00006
C                           SCREEN3 (DATED 13043)                       SCR00010
C                                                                       SCR00011
C             *** SEE SCREEN MODEL CHANGE BULLETIN MCB#3 ***            SCR00012
C                                                                       SCR00013
C     ON THE SUPPORT CENTER FOR REGULATORY AIR MODELS BULLETIN BOARD    SCR00014
C                                                                       SCR00015
C                      http://www.epa.gov/ttn/scram/                    SCR00016
C                                                                       SCR00017
C***********************************************************************SCR00018
C
C                       **************************
C                       ***       SCREEN3      ***
C                       ***   (DATED  13043)   ***
C                       **************************
C        MODIFIED FROM:           SCREEN3      ***
C                             (DATED  96043)   ***
C                             (DATED  95250)   ***
C                             (DATED  92245)   ***
C                       **************************
C                       *** SCREEN-1.1 PROGRAM ***
C                       ***   (DATED  88300)   ***
C                       **************************
C
C***********************************************************************
C
C                           MODIFICATION HISTORY
C
C        MODIFIED BY:
C
C              PETER ECKHOFF
C              EPA
C              RESEARCH TRIANGLE PARK, NC 27711
C
C        MODIFICATION DATE:
C
C              FEBRUARY 12, 2013
C
C        MODIFICATIONS INCLUDE:
C
C              1)  Recompiled to 32-bit.  We are finding that our recompiled 
C                  32-bit executables will run on Windows 7, x64 operating 
C                  systems.
C      
C              2)  The length of CINP had to be increased from 50 to 80.
C
C              3)  The capability to open a fixed filename, SCREEN.INP, was 
C                  added but not implemented.  The following line was 
C                  added to initialize the input filename variable, INPFIL:
C
C                     INPFIL = 'SCREEN.INP'
C
C              4)  If a fixed filename is required under another operating 
C                  system, the following statement needs to be uncommented
C                  before recompiling:
C
C                     C       OPEN(IRD,FILE=INPFIL,STATUS='OLD')
C
C              5)  There were too many commas in Format statement 103 near 
C                  line number 1943.  The commas were removed.
C
C              6)  The version number, VERSN, was changed in Block Init 
C                  from 96043 to 13043.
C
C              7)  In the CALL PRISE statement, the scalar variable, xnw, was 
C                  being passsed to the dimensioned variable, xdown. xdown was 
C                  always dimensioned to "1".  In Subroutine PRISE, xdown was 
C                  changed to the scalar, xnw.  The same was true for z and 
C                  zeff.  These two variables were changed to znw and zefflgl.
C
C              8)  xnw, znw, and zefflgl were equated to their dimensioned 
C                  counterparts.
C
C              9)  The compiler objected to mixed integer and floating point 
C                  variable arguments in the function AMAX1.  The value 10 was
C                  changed to a float point value by adding a decimal point.  
C                  There were two other instances of this happening.  All 
C                  three statements were edited.
C
C             10)  G10.4 formats were widened to G11.4 at the suggestion of 
C                  compiler.
C
C             11)  Source code SCREEN3C.FOR was not modified.
C
C             12)  Further code change details are in Model Change Bulletin #3.
C      
C***********************************************************************
C      
C      C        MODIFIED BY:
C
C              PETER ECKHOFF
C              EPA
C              MD-14
C              RESEARCH TRIANGLE PARK, NC 27711
C
C        MODIFICATION DATE:
C
C              FEBRUARY 12, 1996
C
C        MODIFICATIONS INCLUDE:
C
C              THE NEW ANEMOMETER HEIGHT ROUTINE WAS MOVED TOWARD THE
C              BEGINNING OF THE CODE.  THE ROUTINE WAS PREVIOUSLY PLACED
C              AFTER THE COMPLEX TERRAIN ALGORITHM WHICH NEEDS AN ANEMOMETER
C              HEIGHT.  THE INPUT SEQUENCE WAS ALSO ALTERED TO ACCOMODATE THE
C              NEW ALGORITHM ROUTINE.  SEVERAL OUTPUT STATEMENTS WERE ADDED
C              OR ENHANCED TO REFLECT THE USE OF THE NEW OPTIONS.
C
C              THE ADDITION OF THREE NON-REGULATORY OPTIONS:
C                1. AN ANEMOMETER HEIGHT OTHER THAN 10 METERS CAN NOW BE
C                   ENTERED AS AN OPTION, AND
C                2. THE ABILITY TO PRODUCE RESULTS THAT ARE CONSERVATIVE
C                   WITH RESPECT TO ISCST3 RESULTS.  OPTION 2, FROM THE
C                   R. BRODE PAPER GIVEN AT THE 1991 SEVENTH JOINT CONFERENCE
C                   ON APPLICATIONS OF AIR POLLUTION METEOROLOGY WITH AWMA,
C                   WAS ADDED AS AN OPTION.  THIS OPTION MINIMIZES THE MIXING
C                   HEIGHTS FOR UNSTABLE CASES THUS CREATING RESULTS THAT ARE
C                   MORE CONSERVATIVE WITH RESPECT TO ISCST2 RESULTS.
C                3. THE ADDITION OF THE SCHULMAN-SCIRE (1993) CAVITY
C                   FORMULATION FROM AN ARTICLE IN THE AUGUST 1993 OF AIR AND
C                   WASTE (JOURNAL OF THE AIR AND WASTE MANAGEMENT
C                   ASSOCIATION).
C
C              ALSO PROGRAMMED INTO THIS SOURCE CODE IS THE ABILITY TO USE
C                   PREVIOUS DATA SETS WITHOUT HAVING TO RE-EDIT THE DATA
C                   FOR CASES WHERE THE OPTIONS ARE NOT EXERCISED.
C
C***********************************************************************
C
C        MODIFIED BY:
C
C              ROGER W. BRODE
C              PACIFIC ENVIRONMENTAL SERVICES, Inc.
C              5001 SOUTH MIAMI BLVD., SUITE 300
C              P.O. BOX 12077
C              RESEARCH TRIANGLE PARK, NORTH CAROLINA 27709
C
C        MODIFICATION DATE:
C
C              SEPTEMBER 7, 1995
C
C        MODIFICATIONS INCLUDE:
C
C              INCORPORATION OF THE MODIFIED AREA SOURCE ALGORITHM
C              BASED ON THE ISCST3 MODEL (DATED 95250).
C              THIS NEW ALGORITHM USES A NUMERICAL INTEGRATION OF THE
C              POINT SOURCE FUNCTION OVER THE AREA TO ESTIMATE IMPACTS.
C              IT ALLOWS FOR RECTANGULAR-SHAPED AREAS, AND ALSO WILL
C              ESTIMATE IMPACTS WITHIN THE AREA ITSELF.  SINCE MAXIMUM
C              IMPACTS AT A GIVEN DISTANCE ARE A FUNCTION OF WIND
C              DIRECTION FOR RECTANGULAR AREAS, THE MODEL SEARCHES
C              THROUGH A RANGE OF WIND DIRECTIONS BASED ON A SERIES OF
C              LOOK-UP TABLES (AS A FUNCTION OF STABILITY CLASS, ASPECT
C              RATIO AND NORMALIZED DOWNWIND DISTANCE) TO DETERMINE THE
C              MAXIMUM IMPACT.  THE USER ALSO HAS THE OPTION OF SPECIFYING
C              A WIND DIRECTION ORIENTATION RELATIVE TO THE LONG DIMENSION
C              OF THE AREA.
C
C              A DRY DEPOSITION ALGORITHM BASED ON THE DEPST MODEL HAS ALSO
C              BEEN ADDED, BUT HAS NOT BEEN ACTIVATED.  THIS ALGORITHM IS
C              NOT COMPLETELY CONSISTENT WITH THE DRY DEPOSITION ALGORITHM
C              IN THE ISCST3 MODEL.
C
C              TWO MINOR BUGS WERE ALSO CORRECTED.  ONE BUG ALLOWED
C              FUMIGATION CALCULATIONS TO BE PERFORMED FOR FLARE SOURCES
C              UNDER URBAN CONDITIONS.  THE OTHER BUG INVOLVED USE OF
C              AN ERRORNEOUS VARIABLE NAME WHEN CHECKING FOR A NEGATIVE
C              SOURCE WIDTH FOR AREA SOURCES.
C
C***********************************************************************
C
C        MODIFIED BY:
C
C              ROGER W. BRODE
C              PACIFIC ENVIRONMENTAL SERVICES, Inc.
C              5001 SOUTH MIAMI BLVD., SUITE 300
C              P.O. BOX 12077
C              RESEARCH TRIANGLE PARK, NORTH CAROLINA 27709
C
C        MODIFICATION DATE:
C
C              SEPTEMBER 1, 1992
C
C        MODIFICATIONS INCLUDE:
C
C              UPDATING CODE FOR COMPATIBILITY WITH ISCST2 (92062).
C              THIS ESPECIALLY INCLUDES DOWNWASH ALGORITHMS.  SCREEN
C              MODEL CODE, ESPECIALLY SUBROUTINE USERX, HAS BEEN
C              RESTRUCTURED SLIGHTLY TO MAKE MAXIMUM USE OF ISC2
C              CALCULATION ROUTINES.
C
C              AREA SOURCE ALGORITHM IN SCREEN HAS BEEN MODIFIED
C              TO USE FINITE LINE SEGMENT APPROACH FOR COMPATIBILITY
C              WITH ISCST2 (92062).  DISTANCES ARE NOW MEASURED FROM
C              THE CENTER OF THE AREA (RATHER THAN FROM DOWNWIND EDGE
C              AS DONE WITH PREVIOUS VIRTUAL POINT SOURCE IMPLEMENTATION).
C              INPUT EMISSIONS ARE NOW IN GRAMS/(SEC-M**2), FOR
C              CONSISTENCY WITH ISCST2.
C
C              A NEW VOLUME SOURCE OPTION HAS BEEN ADDED FOR CONSISTENCY
C              WITH ISCST2 (92062).  USES A VIRTUAL POINT SOURCE APPROACH.
C
C              MODIFICATIONS HAVE BEEN MADE TO THE ITERATION ROUTINE TO
C              SEARCH FOR THE PEAK CONCENTRATION, AND F STABILITY HAS
C              BEEN ADDED TO THE URBAN OPTION.  ADDITIONAL WIND SPEEDS
C              IN 0.5 M/S INCREMENTS HAVE BEEN ADDED FOR SPEEDS < 5 M/S.
C              THESE CHANGES ARE INTENDED TO IMPROVE THE PERFORMANCE OF
C              SCREEN AS A CONSERVATIVE SCREENING TOOL RELATIVE TO ISCST2.
C
C              AN OPTION HAS BEEN ADDED TO INPUT THE VOLUMETRIC FLOW
C              RATE FOR STACK (i.e. POINT) RELEASES IN LIEU OF STACK
C              GAS EXIT VELOCITY.  FLOW RATE CAN BE INPUT IN AFCM OR
C              M**3/S.
C
C***********************************************************************
C
C        PC VERSION OF THE SCREENING PROCEDURES DOCUMENT, EPA-450/
C        4-88-010, FOR ESTIMATING MAXIMUM SHORT-TERM CONCENTRATIONS
C        FROM STATIONARY SOURCES.  THIS VERSION OF THE SCREEN MODEL
C        OFFERS AN ARRAY OF PRE-SELECTED DISTANCES AND ALSO
C        ACCEPTS USER-SPECIFIED DISTANCES.
C
C        BUILDING DOWNWASH, FUMIGATION, AND COMPLEX TERRAIN SCREENING
C        CALCULATIONS ARE INCLUDED.  SIMPLE ROUTINES ARE ALSO
C        INCLUDED TO HANDLE RELEASES FROM FLARES AND FOR
C        SIMPLE AREA SOURCES.
C
C        A MECHANISM IS PROVIDED TO ACCOUNT FOR THE EFFECTS OF
C        TERRAIN BELOW STACK HEIGHT ON THE SIMPLE TERRAIN SCREENING
C        CALCULATIONS.
C
C        PROGRAMMED BY:
C
C                ROGER W. BRODE               THOMAS E. PIERCE
C                U.S. EPA (MD-14)     AND     U.S. EPA (MD-80)
C                RTP, NC 27711                RTP, NC 27711
C
C        INPUT VARIABLES
C           Q    EMISSION RATE (G/S)
C           HS   STACK HT (M)
C           DS   STACK INSIDE DIAMETER (M)
C           VS   STACK GAS EXIT VELOCITY (M/S)
C           TS   STACK GAS TEMPERATURE (K)
C           TA   AMBIENT AIR TEMPERATURE (K)
C           ZR   RECEPTOR HEIGHT ABOVE GROUND (FLAGPOLE RECEPTOR) (M)
C           HB   BUILDING HEIGHT (M)
C           HL   MINIMUM HORIZ. BUILDING DIMENSION (M)
C           HW   MAXIMUM HORIZ. BUILDING DIMENSION (M)
C           IOPT URBAN/RURAL OPTION (U = URBAN, R = RURAL)
C           HTER MAXIMUM TERRAIN HEIGHT ABOVE STACK BASE (M)
C           X    DOWNWIND DISTANCE (M)
C
C        PARAMETERS
C           XAUTO ARRAY OF AUTOMATED DISTANCES
C           IRD   UNIT NUMBER FOR INPUT FROM KEYBOARD
C           IPRT  UNIT NUMBER FOR OUTPUT TO TERMINAL
C           IOUT  UNIT NUMBER FOR OUTPUT TO DISK FILE (SCREEN.OUT)
C           IDAT  UNIT NUMBER FOR CREATING INPUT DATA FILE (SCREEN.DAT)
C
C        ROUTINES CALLED
C           INPUTP RETURNS INPUT DATA FOR POINT SOURCE
C           INPUTF RETURNS INPUT DATA FOR FLARE RELEASE
C           INPUTA RETURNS INPUT DATA FOR AREA SOURCE
C           INPUTV RETURNS INPUT DATA FOR VOLUME SOURCE
C           CHOICE SETS PARAMETERS TO CONTROL CHOICE OF METEOROLOGY
C           AUTOX  EXECUTES AUTOMATED DISTANCE OPTION
C           DISCX  EXECUTES DISCRETE DISTANCE OPTION
C           USERX  RETURNS MAX CONC AND THE CONDITIONS ASSOCIATED
C                  WITH THE MAX AT DISTANCE X (CALLED FROM SUB. AUTOX
C                  AND DISCX)
C           CAVITY RETURNS MAX CONC, CAVITY HT, AND LENGTH OF
C                  CAVITY IF PLUME IS FOUND TO BE ENTRAPPED IN
C                  THE CAVITY RECIRCULATION REGION
C2         CAVITY2 RETURNS MAX CONC, AND LENGTH OF CAVITY IF
C2                 SCHULMAN-SCIRE FORMULATION IS SELECTED FOR
C2                 THE CAVITY REGION (POINT & FLARE SOURCES)
C           FUMI   RETURNS MAX CONC AND DISTANCE TO MAX DUE TO
C                  INVERSION BREAK-UP FUMIGATION CONDITIONS
C           FUMS   RETURNS MAX CONC AND DISTANCE TO MAX DUE TO
C                  SHORELINE FUMIGATION CONDITIONS (IF APPLICABLE)
C           VALLEY RETURNS 24-HR CONCENTRATION FOR RECEPTORS ABOVE
C                  STACK HEIGHT USING VALLEY MODEL SCREENING TECHNIQUE
C                  AND SIMPLE TERRAIN SCREENING PROCEDURES FOLLOWING
C                  GUIDANCE (CALLED FROM SUB. INPUTP AND INPUTF)
C
C
      INCLUDE 'MAIN.INC'
      CHARACTER*1 QZI
      CHARACTER*2 QSS
      CHARACTER*80 CINP
      INTEGER*2 IPTHR, IPTMIN, IPTSEC, IPTHUN, IPTYR, IPTMON, IPTDAY
      character*27 cavdef1,cavdef2
      logical lwind, lroof
C
C        INITIALIZE VARIABLES
C
      INPFIL = 'SCREEN.INP'
      OUTFIL = 'SCREEN.OUT'
      SOURCE = '    '
      SYINIT = 0.
      SZINIT = 0.
      HB   = 0.
      HL   = 0.
      HW   = 0.
      HWP  = 0.
      DX   = 0.
      POINT  = .FALSE.
      FLARE  = .FALSE.
      AREA   = .FALSE.
      VOLUME = .FALSE.
      DISC   = .FALSE.
      DEBUG  = .FALSE.
C
C        VALUES ASSOCIATED WITH MAXIMUM CONCENTRATIONS FOR EACH
C        CALCULATION PROCEDURE: -ST FOR SIMPLE TERRAIN, -CT FOR
C        COMPLEX TERRAIN, -IF FOR INVERSION BREAKUP FUMIGATION,
C        AND -SF FOR SHORELINE FUMIGATION.
C
      CMAXST = 0.0
      XMAXST = 0.0
      TMAXST = 0.0
      DMAXST = 0.0
      XMXDST = 0.0
      TMXDST = 0.0
      CMAXCT = 0.0
      XMAXCT = 0.0
      TMAXCT = 0.0
      CMAXIF = 0.0
      XMAXIF = 0.0
      CMAXSF = 0.0
      XMAXSF = 0.0
      CAVCHI(1) = 0.0
      CAVCHI(2) = 0.0
      do i=1,4
         CAVCHI(i) = 0.0
         XR(i) = 0.0
      enddo
C2
C2c --- New input variables for CAVITY2: stack location on roof
C2c
C2c                                HW
C2c
C2c                  +--------------.---------------+
C2c                  |              .               |
C2c   (x/HW = .4)--> |  S           .               |
C2c                  |              .               |      HL
C2c                  |              .               |
C2c                  +--------------.---------------+
C2c                 .5              0              .5
C2c
C2c
C2c                                HL
C2c
C2c                          +------.------+
C2c                          |      .      |
C2c         (x/HL = .15)---> |      . S    |
C2c                          |      .      |
C2c                          |      .      |
C2c                          |      .      |        HW
C2c                          |      .      |
C2c                          |      .      |
C2c                          |      .      |
C2c                          |      .      |
C2c                          +------.------+
C2c                         .5      0     .5
C2c
C2c     xstkl  : position of stack x/HL from CENTER of building
C2c     xstkw  : position of stack x/HW from CENTER of building
        xstkl = 0.0
        xstkw = 0.0
        lroof = .TRUE.
C
C        HT, RMIN, AND RMAX TO KEEP TRACK OF TERRAIN HEIGHTS AND
C        DISTANCE RANGES FOR SIMPLE TERRAIN PROCEDURES.
C
      IT = 0
      DO 60 I=1,50
         HT(I) = 0.0
         RMIN(I) = 0.0
         RMAX(I) = 0.0
60    CONTINUE
C
C        CALL DATE AND TIME FUNCTIONS, EITHER LAHEY OR MICROSOFT.
C
$IF DEFINED (LAHEY)
      CALL DATE(RUNDAT)
      CALL TIME(RUNTIM)
$ELSE
      CALL GETDAT(IPTYR, IPTMON, IPTDAY)
      CALL GETTIM(IPTHR, IPTMIN, IPTSEC, IPTHUN)
C     Convert Year to Two Digits
      IPTYR = IPTYR - 100*INT(IPTYR/100)
C     Write Date and Time to Character Variables, RUNDAT & RUNTIM
      WRITE(RUNDAT,'(2(I2.2,1H/),I2.2)') IPTMON, IPTDAY, IPTYR
      WRITE(RUNTIM,'(2(I2.2,1H:),I2.2)') IPTHR, IPTMIN, IPTSEC
$ENDIF
C
C***********************************************************************
C        BEGIN USER INPUT
C***********************************************************************
C
      WRITE(IPRT,108)
      WRITE(IPRT,109) VERSN
 108  FORMAT(1X,' ******  SCREEN3 MODEL  ******')
 109  FORMAT(1X,' **** VERSION DATED ',A5,' ****')
C      WRITE(IPRT,*) ' '
C94    WRITE(IPRT,*) 'ENTER NAME FOR OUTPUT FILE'
C      READ(IRD,95) OUTFIL
C95    FORMAT(A12)
C
C      OPEN(IRD,FILE=INPFIL,STATUS='OLD')
      OPEN(IOUT,FILE=OUTFIL,STATUS='UNKNOWN')
      OPEN(IDAT,FILE='SCREEN.DAT',STATUS='UNKNOWN')
C
      WRITE(IPRT,*) ' '
      WRITE(IPRT,*) 'ENTER TITLE FOR THIS RUN (UP TO 79 CHARACTERS):'
      READ(IRD,79) TITLE
      WRITE(IDAT,79) TITLE
79     FORMAT(A79)
      IF (TITLE(75:79) .EQ. 'DEBUG') THEN
C        Debug output option selected if last five columns of TITLE = 'DEBUG'
         DEBUG = .TRUE.
         OPEN(IDBG,FILE='SCREEN.DBG',STATUS='UNKNOWN')
      END IF
      WRITE(IPRT,*) ' '
2     WRITE(IPRT,81)
CDEP  Change FORMAT statement to remove deposition options.
81    FORMAT(' ENTER SOURCE TYPE: P    FOR POINT  ',/
     &       '                    F    FOR FLARE  ',/
     &       '                    A    FOR AREA   ',/
     &       '                    V    FOR VOLUME ')
CRWB81    FORMAT(' ENTER SOURCE TYPE: P    FOR POINT  - NO DEPOSITION ',/
CRWB     &       '                    F    FOR FLARE  - NO DEPOSITION ',/
CRWB     &       '                    A    FOR AREA   - NO DEPOSITION ',/
CRWB     &       '                    V    FOR VOLUME - NO DEPOSITION ',/
CRWB     &       '                    PDEP FOR POINT  - WITH DEPOSITION',/
CRWB     &       '                    FDEP FOR FLARE  - WITH DEPOSITION',/
CRWB     &       '                    ADEP FOR AREA   - WITH DEPOSITION',/
CRWB     &       '                    VDEP FOR VOLUME - WITH DEPOSITION')
      WRITE(IPRT,*) '   ALSO ENTER ANY OF THE FOLLOWING OPTIONS ',
     +              'ON THE SAME LINE:'
      WRITE(IPRT,*) ' '
      WRITE(IPRT,*) '     N    - TO USE THE NON-REGULATORY BUT ',
     +              'CONSERVATIVE BRODE 2'
      WRITE(IPRT,*) '            MIXING HEIGHT OPTION,'
      WRITE(IPRT,*) '     nn.n - TO USE AN ANEMOMETER HEIGHT OTHER ',
     +              'THAN THE REGULATORY'
      WRITE(IPRT,*) '            (DEFAULT) 10 METER HEIGHT.'
      WRITE(IPRT,*) '     SS   - TO USE A NON-REGULATORY ',
     +              'CAVITY CALCULATION ALTERNATIVE'
      WRITE(IPRT,*) '  Example - PN 7.0 SS (entry for a point source)'
      WRITE(IPRT,*) ' '
      WRITE(IPRT,*) ' ENTER SOURCE TYPE AND ANY OF THE ABOVE OPTIONS:'

C    READ INPUT LINE AS A SOLID BLOCK OF CHARACTERS, THEN PARSE INTO
C     SEPARATE VALUES

      READ(IRD,400) CINP
400     FORMAT(A50)
         ILEN = LEN_TRIM(CINP)
       CALL LWRUPR(CINP, ILEN)
        IFLG = 0
        ICI = 0
        ISS = 0

C     PARSE AND PROCESS FIRST LINE OF INPUT DATA
C      DATA CAN CONSIST OF SOURCE TYPE, AND NON-/REGULATORY MIXING HEIGHT,
C      ANEMOMETER HEIGHT AND BUILDING DOWNWASH OPTIONS.
C      NO OPTIONS EQUATES TO REGULATORY DEFAULT.

C          DETERMINE THE SOURCE TYPE

         SOURCE(1:1) = CINP(1:1)
         IF (CINP(2:4) .EQ. 'DEP' .OR. CINP(2:4) .EQ. 'dep') THEN
           SOURCE = CINP(1:4)
         END IF

C         DETERMINE MIXING HEIGHT OPTION (BRODE 2 YES/NO)
C          DETECT AN 'N' UNDER THE OLD SCREEN2 OPTIONAL FORMAT

        DO I = 1, ILEN
           IF (CINP(I:I) .EQ. 'N' .OR. CINP(I:I) .EQ. 'n') THEN
             QZI = CINP(I:I)
             ICI = 1
           END IF
        END DO

C        BASED ON POSITION OF DECIMAL POINT, CONVERT ASCII INTEGER VALUES TO
C         AN ANEMOMETER HEIGHT VALUE.

          HANE = 0.0
          IDOT = ILEN + 1
          DO K = 1, ILEN
            IF (CINP(K:K) .EQ. '.') THEN
              IDOT = K
            END IF
          END DO
          DO K = 1, ILEN
            ICN = ICHAR(CINP(K:K)) - 48
            IF (ICN .GE. 0 .AND. ICN .LE. 9) THEN
              IDOTK = IDOT - K
              IF (IDOTK .GT. 0) IDOTK = IDOTK - 1
              HANE = HANE + ICN * 10. ** (IDOTK)
            END IF
          END DO

        ZREF = 10.0
        IF (HANE .GT. 0.0001) THEN
          ZREF = HANE
         ELSE
          HANE = 10.0
        END IF

C     SEARCH FOR A SCHULMAN-SCIRE ALTERNATIVE BUILDING DOWNWASH ALTERNATIVE
C      ALGORITHM FLAG (SS)

        DO I = 1, ILEN-1
           IF (CINP(I:I+1) .EQ. 'SS'.OR. CINP(I:I+1) .EQ. 'ss') THEN
             QSS = 'SS'
             ISS = 1
           END IF
        END DO

      WRITE(IDAT,101) SOURCE, QZI, HANE, QSS
101     FORMAT(A4,1X,A1,1X,F8.1,1X,A2)

      IF (SOURCE.EQ.'P   ') THEN
         POINT = .TRUE.
C         WRITE(IDAT,40) SOURCE
         CALL INPUTP
      ELSE IF (SOURCE.EQ.'F   ') THEN
         FLARE = .TRUE.
C         WRITE(IDAT,40) SOURCE
         CALL INPUTF
      ELSE IF (SOURCE.EQ.'A   ') THEN
         AREA = .TRUE.
C         WRITE(IDAT,40) SOURCE
         CALL INPUTA
      ELSE IF (SOURCE.EQ.'V   ') THEN
         VOLUME = .TRUE.
C         WRITE(IDAT,40) SOURCE
         CALL INPUTV
CDEP  Comment out the deposition source options.
CRWB      ELSE IF (SOURCE.EQ.'PDEP') THEN
CRWB         POINT  = .TRUE.
CRWB         LDEP   = .TRUE.
CRWB         WRITE(IDAT,40) SOURCE
CRWB         CALL INPDEP
CRWB         CALL VDP1
CRWB      ELSE IF (SOURCE.EQ.'FDEP') THEN
CRWB         FLARE  = .TRUE.
CRWB         LDEP   = .TRUE.
CRWB         WRITE(IDAT,40) SOURCE
CRWB         CALL INFDEP
CRWB         CALL VDP1
CRWB      ELSE IF (SOURCE.EQ.'ADEP') THEN
CRWB         AREA   = .TRUE.
CRWB         LDEP   = .TRUE.
CRWB         WRITE(IDAT,40) SOURCE
CRWB         CALL INADEP
CRWB         CALL VDP1
CRWB      ELSE IF (SOURCE.EQ.'VDEP') THEN
CRWB         VOLUME = .TRUE.
CRWB         LDEP   = .TRUE.
CRWB         WRITE(IDAT,40) SOURCE
CRWB         CALL INVDEP
CRWB         CALL VDP1
      ELSE
         GO TO 2
      END IF
C
C    END OF SOURCE, MIXING HEIGHT OPTION, AND OPTIONAL ANEMOMETER HEIGHT
C      INPUT ALGORITHM

C
C        SEE IF USER WANTS TO STOP AFTER COMPLEX TERRAIN CALCS-STP=.TRUE.
C
      IF (STP) GO TO 4
C
C***********************************************************************
C        CHECK FOR USE OF SIMPLE ELEVATED TERRAIN
C***********************************************************************
C
      IF (POINT .OR. FLARE .OR. VOLUME) THEN
25       WRITE(IPRT,*) 'USE SIMPLE TERRAIN SCREEN WITH TERRAIN',
     &                 ' ABOVE STACK BASE?'
         WRITE(IPRT,*) 'ENTER Y OR N:'
         READ(IRD,100) QUERY
100        FORMAT(A1)
         IF (QUERY .EQ. 'Y' .OR. QUERY .EQ. 'y') THEN
            ELEV = .TRUE.
            FLAT = .FALSE.
         ELSE IF (QUERY .EQ. 'N' .OR. QUERY .EQ. 'n') THEN
            FLAT = .TRUE.
            ELEV = .FALSE.
         ELSE
            GO TO 25
         END IF
         WRITE(IDAT,100) QUERY
      ELSE
         FLAT = .TRUE.
         ELEV = .FALSE.
      END IF
C
C        CALCULATE BUOYANCY AND MOMENTUM FLUX AND WRITE TO FILE.
C
      IF (TA .GT. TS) THEN
         TS = TA
         FB = 0.0
         FM = TA*VS*VS*DS*DS/(4.*TS)
         WRITE(IPRT,*) 'TA > TS!!!  BUOY. FLUX SET = 0.0'
         WRITE(IOUT,*) 'TA > TS!!!  BUOY. FLUX SET = 0.0'
      ELSE
         FB = G*VS*DS*DS*(TS-TA)/(4.*TS)
         FM = TA*VS*VS*DS*DS/(4.*TS)
      END IF
      WRITE(IOUT,110) FB,FM
110   FORMAT(/,1X,'BUOY. FLUX =',F9.3,' M**4/S**3;  MOM. FLUX =',
     &       F9.3,' M**4/S**2.',/)
C***********************************************************************
C        SPECIFY CHOICE OF METEOROLOGY
C***********************************************************************
3     WRITE(IPRT,*) 'ENTER CHOICE OF METEOROLOGY;'
      WRITE(IPRT,*) '1 - FULL METEOROLOGY (ALL STABILITIES & WIND',
     &                                    ' SPEEDS)'
      WRITE(IPRT,*) '2 - INPUT SINGLE STABILITY CLASS'
      WRITE(IPRT,*) '3 - INPUT SINGLE STABILITY CLASS AND WIND SPEED'
      READ(IRD,*,ERR=3) IMET
      IF (IMET.NE.1 .AND. IMET.NE.2 .AND. IMET.NE.3) GO TO 3
C
      CALL CHOICE(IMET)
C
C**********************************************************************
C        AUTOMATED DISTANCE SECTION
C**********************************************************************
C
5     WRITE(IPRT,*) 'USE AUTOMATED DISTANCE ARRAY?',
     &              ' ENTER Y OR N:'
      READ(IRD,100) QUERY
      IF (QUERY .EQ. 'Y' .OR. QUERY .EQ. 'y') THEN
         WRITE(IDAT,100) QUERY
         CALL AUTOX(IT,CMAXST,XMAXST,TMAXST,DMAXST,XMXDST,TMXDST)
      ELSE IF (QUERY .EQ. 'N' .OR. QUERY .EQ. 'n') THEN
         WRITE(IDAT,100) QUERY
         CONTINUE
      ELSE
         GO TO 5
      END IF
C
C**********************************************************************
C        DISCRETE (USER-SPECIFIED) DISTANCE SECTION
C**********************************************************************
C
      WRITE(IPRT,*) ' '
7     WRITE(IPRT,*) 'USE DISCRETE DISTANCES?',
     &              '  ENTER Y OR N: '
      READ(IRD,100) QUERY
      IF (QUERY .EQ. 'Y' .OR. QUERY .EQ. 'y') THEN
         WRITE(IDAT,100) QUERY
         DISC = .TRUE.
         CALL DISCX(IT,CMAXST,XMAXST,TMAXST,DMAXST,XMXDST,TMXDST)
      ELSE IF (QUERY .EQ. 'N' .OR. QUERY .EQ. 'n') THEN
         WRITE(IDAT,100) QUERY
         CONTINUE
      ELSE
         GO TO 7
      END IF
C***********************************************************************
C        SUMMARIZE TERRAIN HEIGHTS ENTERED FOR SIMPLE ELEVATED
C        TERRAIN PROCEDURE.
C***********************************************************************
      IF (.NOT.FLAT) THEN
         WRITE(IOUT,510)
510      FORMAT(/1X,' ********************************************',
     &         /,1X,' *  SUMMARY OF TERRAIN HEIGHTS ENTERED FOR  *',
     &         /,1X,' *    SIMPLE ELEVATED TERRAIN PROCEDURE     *',
     &         /,1X,' ********************************************',
     &        //,1X,'      TERRAIN        DISTANCE RANGE (M)',
     &         /,1X,'       HT (M)       MINIMUM     MAXIMUM',
     &         /,1X,'      -------      --------    --------')
         DO 80 I = 1,50
            IF (RMIN(I) .GT. 0.0) THEN
               IF (RMAX(I) .GT. 0.0) THEN
                  WRITE(IOUT,520) HT(I),RMIN(I),RMAX(I)
520               FORMAT(8X,F6.0,7X,F7.0,5X,F7.0)
               ELSE
                  WRITE(IOUT,530) HT(I),RMIN(I)
530               FORMAT(8X,F6.0,7X,F7.0,9X,'--')
               END IF
            END IF
80       CONTINUE
      END IF
C
C***********************************************************************
C        PERFORM CAVITY CALCULATIONS & PRINT RESULTS FOR TWO
C        ORIENTATIONS - HL ALONGWIND FIRST, THEN HW ALONGWIND.
C***********************************************************************
C
      IF (HB.GT.0. .AND. HW.GT.0. .AND. HL.GT.0.) THEN
        IF (ISS .EQ. 0) THEN
C          PREFORM THE REGULATORY CAVITY CALCULATIONS

            write(iout,*)
            write(iout,*) '****************************************'
            write(iout,*) '     *** REGULATORY (Default) ***  '
            write(iout,*) '    PERFORMING CAVITY CALCULATIONS '
            write(iout,*) '  WITH ORIGINAL SCREEN CAVITY MODEL'
            write(iout,*) '          (BRODE, 1988) '
            write(iout,*) '****************************************'
            write(iout,*)

            CALL CAVITY

c ---       Redefine summary output vaariables to be consistent with
c ---       CAVITY2
            cavchi(3)=cavchi(2)
            xr(3)=xr(2)
            cavchi(2)=cavchi(1)
            xr(2)=xr(1)
            cavchi(4)=cavchi(3)
            xr(4)=xr(3)

            write(iout,*)
            write(iout,*) '****************************************'
            write(iout,*) '      END OF CAVITY CALCULATIONS '
            write(iout,*) '****************************************'
            write(iout,*)

         ELSE IF (ISS .EQ. 1) THEN

            write(iout,*)
            write(iout,*) '****************************************'
            write(iout,*) '       ***  NON-REGULATORY ***     '
            write(iout,*) '    PERFORMING CAVITY CALCULATIONS '
            write(iout,*) '   WITH SCHULMAN-SCIRE (1993) MODEL'
            write(iout,*) '****************************************'
            write(iout,*)

            write(iprt,*) 'Print concentration for all modeled speeds',
     &                    ' in addition to maximum concentration? '
            read(ird,100) QUERY
            write(idat,100) QUERY
            if (QUERY .EQ. 'Y' .OR. QUERY .EQ. 'y') then
               lwind=.TRUE.
            else
               lwind=.FALSE.
            endif

            write(iprt,*)
            write(iprt,*) 'Enter stack location (divided ',
     &                    'by along-wind building scale, L).'
            write(iprt,*) 'Origin for this is the CENTER of building,',
     &                    ' so (absolute) ',
     &                    'value lies between 0.0 and 0.5 if the stack',
     &                    ' is on the roof.'
            write(iprt,*) '(It is reset in the model to 0.5 if larger)'
            write(iprt,*)
            write(iprt,*) 'Case 1:  LONGER side ALONG flow'
            write(iprt,*)
            write(iprt,*) '         Example for (x/L = .4):'
            write(iprt,*)
            write(iprt,*) '             x/L'
            write(iprt,*) '        :-----------: '
            write(iprt,*) '     +--------------.---------------+ '
            write(iprt,*) '     |              .               | '
            write(iprt,*) '     |  S           .               | '
            write(iprt,*) '     |              .               | '
            write(iprt,*) '     |              .               | '
            write(iprt,*) '     +--------------.---------------+ '
            write(iprt,*) '    .5              0              .5 '
            write(iprt,*)
            write(iprt,*) '             <--- Wind --->           '
            write(iprt,*)
            write(iprt,*)
            write(iprt,*) 'ENTER x/L (LONGER side ALONG flow) = '
            read(ird,*) xstkw
            xstkw = ABS(xstkw)
            if(xstkw .GT. .5) then
               xstkw = .5
               lroof = .FALSE.
               write(iprt,*)
            endif
            write(idat,*) xstkw

            write(iprt,*)
            write(iprt,*)
            write(iprt,*)
            write(iprt,*) 'Case 2:  SHORTER side ALONG flow '
            write(iprt,*)
            write(iprt,*) '         Example for (x/L = .15)'
            write(iprt,*)
            write(iprt,*) '                    x/L  '
            write(iprt,*) '                    :-:           '
            write(iprt,*) '             +------.------+      '
            write(iprt,*) '             |      .      |      '
            write(iprt,*) '             |      . S    |      '
            write(iprt,*) '             |      .      |      '
            write(iprt,*) '             |      .      |      '
            write(iprt,*) '             |      .      |      '
            write(iprt,*) '             |      .      |      '
            write(iprt,*) '             |      .      |      '
            write(iprt,*) '             |      .      |      '
            write(iprt,*) '             |      .      |      '
            write(iprt,*) '             +------.------+      '
            write(iprt,*) '            .5      0     .5      '
            write(iprt,*) '                              '
            write(iprt,*) '             <--- Wind --->       '
            write(iprt,*)
            write(iprt,*) 'x/L (SHORTER side ALONG flow) = '
            read(ird,*) xstkl
            xstkl = ABS(xstkl)
            if(xstkl .GT. .5) then
               xstkl = .5
               lroof = .FALSE.
               write(iprt,*)
            endif
            write(idat,*) xstkl

c ---       Report stack location
            if(.not. LROOF) then
               write(iprt,*)
               write(iprt,*) 'Stack was placed at location away from',
     &                       ' the building'
               write(iprt,*) 'Stack is REPOSITIONED to EDGE of',
     &                       ' the building'
            endif
            write(iprt,*) 'Stack x/L (LONGER side ALONG flow) = ',xstkw
            write(iprt,*) 'Stack x/L (SHORTER side ALONG flow)= ',xstkl
            write(iout,*) 'Stack x/L (LONGER side ALONG flow) = ',xstkw
            write(iout,*) 'Stack x/L (SHORTER side ALONG flow)= ',xstkl

c ---       Make sure that stack height (AGL) is .GE. building height IF
c ---       stack is on building
            if(LROOF .AND. hs .LT. hb) then
               write(iprt,*) 'FATAL:  Stack-top was placed INSIDE ',
     &                       'building:'
               write(iprt,*) 'Building Ht = ',hb
               write(iprt,*) 'Stack-top   = ',hs
               stop
            endif

c ---       Process SHORTER side ALONG flow first (2 wind directions)
            write(iout,*)
            write(iout,*)
            write(iout,*) '1)  SHORTER Side ALONG flow, STACK nearer ',
     &                    'UPWIND edge of building'
            xstack = (.5-xstkl)*HL
            call CAVITY2(iprt,iout,xstack,hs,vs,ds,ts,ta,q,
     &                   hb,hw,hl,lwind,
     &                   cavchi(1),xr(1))
c ---       Skip second wind direction if stack is at center of roof
            if(xstkl .NE. 0.0) then
               write(iout,*)
               write(iout,*)
               write(iout,*) '2)  SHORTER Side ALONG flow, STACK ',
     &                       'nearer DOWNWIND edge of building'
               xstack = HL-xstack
               call CAVITY2(iprt,iout,xstack,hs,vs,ds,ts,ta,q,
     &                      hb,hw,hl,lwind,
     &                      cavchi(2),xr(2))
            else
               cavchi(2)=cavchi(1)
               xr(2)=xr(1)
            endif

c ---       Process LONGER side ALONG flow (2 wind directions)
            write(iout,*)
            write(iout,*)
            write(iout,*) '3)  LONGER Side ALONG flow, STACK nearer ',
     &                    'UPWIND edge of building'
            xstack = (.5-xstkw)*HW
            call CAVITY2(iprt,iout,xstack,hs,vs,ds,ts,ta,q,
     &                   hb,hl,hw,lwind,
     &                   cavchi(3),xr(3))
c ---       Skip second wind direction if stack is at center of roof
            if(xstkw .NE. 0.0) then
               write(iout,*)
               write(iout,*)
               write(iout,*) '4)  LONGER Side ALONG flow, STACK nearer',
     &                       ' DOWNWIND edge of building'
               xstack = HW-xstack
               call CAVITY2(iprt,iout,xstack,hs,vs,ds,ts,ta,q,
     &                      hb,hl,hw,lwind,
     &                      cavchi(4),xr(4))
            else
               cavchi(4)=cavchi(3)
               xr(4)=xr(3)
            endif

            write(iout,*)
            write(iout,*) '****************************************'
            write(iout,*) '      END OF CAVITY CALCULATIONS '
            write(iout,*) '****************************************'
            write(iout,*)

        END IF
      END IF
C
C***********************************************************************
C        PERFORM OPTIONAL FUMIGATION CALCULATIONS IF RURAL CONDITIONS
C        APPLY.  INVERSION BREAK-UP FUMIGATION CALCULATION FIRST.
C***********************************************************************
C
      IF (RURAL .AND. HS.GE.10. .AND. (POINT .OR. FLARE)) THEN
         WRITE(IPRT,*) ' '
8        WRITE(IPRT,*) 'DO YOU WISH TO MAKE A FUMIGATION CALCULATION?',
     &                 ' ENTER Y OR N:'
         READ(IRD,100) QUERY
         IF (QUERY .EQ. 'Y' .OR. QUERY .EQ. 'y') THEN
            WRITE(IDAT,100) QUERY
            CALL FUMI(CMAXIF,XMAXIF)
C
C***********************************************************************
C        DETERMINE MAXIMUM SHORELINE FUMIGATION CONCENTRATION
C        FOR SITES WITHIN 3000 M OF LARGE BODY OF WATER.
C***********************************************************************
C
            WRITE(IPRT,*) ' '
9           WRITE(IPRT,*) 'CONSIDER SHORELINE FUMIGATION (<=3000M FROM',
     &                    ' SHORE)?  ENTER Y OR N:'
            READ(IRD,100) QUERY2
            IF (QUERY2 .EQ. 'Y' .OR. QUERY2 .EQ. 'y') THEN
               WRITE(IDAT,100) QUERY2
50             WRITE(IPRT,*) 'ENTER SHORTEST DISTANCE TO SHORELINE (M):'
               READ(IRD,*,ERR=50) XS
               WRITE(IDAT,*) XS
               CALL FUMS(CMAXSF,XMAXSF)
            ELSE IF (QUERY2 .EQ. 'N' .OR. QUERY2 .EQ. 'n') THEN
               WRITE(IDAT,100) QUERY2
               CONTINUE
            ELSE
               GO TO 9
            END IF
         ELSE IF (QUERY .EQ. 'N' .OR. QUERY .EQ. 'n') THEN
            WRITE(IDAT,100) QUERY
            CONTINUE
         ELSE
            GO TO 8
         END IF
      END IF
C
C***********************************************************************
C        PRINT SUMMARY OF RESULTS
C***********************************************************************
C
4     CONTINUE
C
      WRITE(IPRT,900)
      WRITE(IOUT,900)
900   FORMAT(/,6X,
     &   '***************************************',/,6X,
     &   '*** SUMMARY OF SCREEN MODEL RESULTS ***',/,6X,
     &   '***************************************')
      IF (LDEP) THEN
       WRITE(IPRT,911)
       WRITE(IOUT,911)
911    FORMAT(/2X,'CALCULATION',6X,'MAX CONC',3X,'DIST TO',2X,'TERRAIN',
     &                         5X,'MAX DEPOS',3X,'DIST TO',2X,'TERRAIN',
     &    /,3X,'PROCEDURE',6X,'(UG/M**3)',3X,'MAX (M)',3X,'HT (M)',
     &                     4X,'(G/M**2-HR)',2X,'MAX (M)',3X,'HT (M)',
     &    /,1X,'--------------  -----------  -------  --------',
     &                      4X,'-----------  -------  -------')
       IF (CMAXST .GT. 0.0 .OR. DMAXST .GT. 0.0) THEN
          WRITE(IPRT,921) CMAXST,XMAXST,TMAXST,DMAXST,XMXDST,TMXDST
          WRITE(IOUT,921) CMAXST,XMAXST,TMAXST,DMAXST,XMXDST,TMXDST
921       FORMAT(1X,'SIMPLE TERRAIN',3X,G11.4,2X,F7.0,4X,F5.0,
     &                               5X,G11.4,2X,F7.0,4X,F5.0,/)
       END IF

      ELSE
       WRITE(IPRT,910)
       WRITE(IOUT,910)
910    FORMAT(/2X,'CALCULATION',8X,'MAX CONC',4X,'DIST TO',3X,'TERRAIN',
     &    /,3X,'PROCEDURE',8X,'(UG/M**3)',4X,'MAX (M)',4X,'HT (M)',
     &    /,1X,'--------------    -----------   ---------   -------')
       IF (CMAXST .GT. 0.0) THEN
          WRITE(IPRT,920) CMAXST,XMAXST,TMAXST
          WRITE(IOUT,920) CMAXST,XMAXST,TMAXST
920       FORMAT(1X,'SIMPLE TERRAIN',5X,G11.4,3X,F7.0,5X,F5.0,/)
       END IF
      END IF
      IF (CMAXCT .GT. 0.0) THEN
         WRITE(IPRT,930) CMAXCT,XMAXCT,TMAXCT
         WRITE(IOUT,930) CMAXCT,XMAXCT,TMAXCT
930      FORMAT(1X,'COMPLEX TERRAIN',4X,G11.4,3X,F7.0,5X,F5.0,
     &          ' (24-HR CONC)',/)
      END IF
      IF (ISS .EQ. 0) THEN
        IF (CAVCHI(1) .GT. 0.0 .OR. CAVCHI(2) .GT. 0.0) THEN
           WRITE(IPRT,940) (I,CAVCHI(I),XR(I),I=1,1)
           WRITE(IPRT,940) (I,CAVCHI(3),XR(3),I=2,2)
           WRITE(IOUT,940) (I,CAVCHI(I),XR(I),I=1,1)
           WRITE(IOUT,940) (I,CAVCHI(3),XR(3),I=2,2)
940        FORMAT(1X,'BLDG. CAVITY-',I1,5X,G11.4,3X,F7.0,7X,'--',
     &            '  (DIST = CAVITY LENGTH)'/)
           WRITE(IPRT,*) ' '
           WRITE(IOUT,*) ' '
        END IF
       ELSE
        IF (CAVCHI(1) .GT. 0.0 .OR. CAVCHI(3) .GT. 0.0) THEN
         cavdef1='  (SHORTER side ALONG flow;'
         cavdef2='  stack nearer upwind face)'
         WRITE(IPRT,942) CAVCHI(1),XR(1),cavdef1
         WRITE(IPRT,941) cavdef2
         WRITE(IOUT,942) CAVCHI(1),XR(1),cavdef1
         WRITE(IOUT,941) cavdef2
         cavdef1='  (SHORTER side ALONG flow;'
         cavdef2='  stack nearer dnwind face)'
         WRITE(IPRT,942) CAVCHI(2),XR(2),cavdef1
         WRITE(IPRT,941) cavdef2
         WRITE(IOUT,942) CAVCHI(2),XR(2),cavdef1
         WRITE(IOUT,941) cavdef2
         cavdef1='  (LONGER  side ALONG flow;'
         cavdef2='  stack nearer upwind face)'
         WRITE(IPRT,942) CAVCHI(3),XR(3),cavdef1
         WRITE(IPRT,941) cavdef2
         WRITE(IOUT,942) CAVCHI(3),XR(3),cavdef1
         WRITE(IOUT,941) cavdef2
         cavdef1='  (LONGER  side ALONG flow;'
         cavdef2='  stack nearer dnwind face)'
         WRITE(IPRT,942) CAVCHI(4),XR(4),cavdef1
         WRITE(IPRT,941) cavdef2
         WRITE(IOUT,942) CAVCHI(4),XR(4),cavdef1
         WRITE(IOUT,941) cavdef2
942      FORMAT(1X,'BUILDING CAVITY ',3X,G11.4,3X,F7.0,7X,'--',a27)
941      FORMAT(48X,'--',a27)
         WRITE(IPRT,*) ' '
         WRITE(IOUT,*) ' '
       END IF
      END IF
      IF (CMAXIF .GT. 0.0) THEN
         WRITE(IPRT,950) CMAXIF,XMAXIF
         WRITE(IOUT,950) CMAXIF,XMAXIF
950      FORMAT(1X,'INV BREAKUP FUMI',3X,G11.4,3X,F7.0,7X,'--',/)
      END IF
      IF (CMAXSF .GT. 0.0) THEN
         WRITE(IPRT,960) CMAXSF,XMAXSF
         WRITE(IOUT,960) CMAXSF,XMAXSF
960      FORMAT(1X,'SHORELINE FUMI',5X,G11.4,3X,F7.0,7X,'--')
      END IF
C
      WRITE(IPRT,990)
      WRITE(IOUT,990)
990   FORMAT(/1X,'***************************************************',
     &       /1X,'** REMEMBER TO INCLUDE BACKGROUND CONCENTRATIONS **',
     &       /1X,'***************************************************'/)
C
11    WRITE(IPRT,*) 'DO YOU WANT TO PRINT A HARDCOPY OF THE RESULTS?',
     &           '  ENTER Y OR N:'
      READ(IRD,100) QUERY
      IF (QUERY .EQ. 'Y' .OR. QUERY .EQ. 'y') THEN
         WRITE(IDAT,100) QUERY
         CALL PRTOUT(IOUT)
         WRITE(IPRT,1000) OUTFIL
1000     FORMAT(' THE OUTPUT FILE, "',A12,'", HAS BEEN PRINTED.'/'1')
      ELSE IF (QUERY .EQ. 'N' .OR. QUERY .EQ. 'n') THEN
         WRITE(IDAT,100) QUERY
         WRITE(IPRT,1001) OUTFIL
1001     FORMAT(' THE RESULTS OF THIS RUN ARE IN FILE, "',A12,'".'/'1')
      ELSE
         GO TO 11
      END IF

      STOP
      END

      SUBROUTINE INPUTP
C
C        DATA ENTRY ROUTINE FOR POINT SOURCES.
C
      INCLUDE 'MAIN.INC'
      NPD = 0

      STP = .FALSE.
10    WRITE(IPRT,*) 'ENTER EMISSION RATE (G/S): '
      READ(IRD,*,ERR=10) Q
      WRITE(IDAT,100) Q
      IF (Q  .LE. 0.) GO TO 90
100   FORMAT(G14.6)
20    WRITE(IPRT,*) 'ENTER STACK HEIGHT (M): '
      READ(IRD,*,ERR=20) HS
      WRITE(IDAT,100) HS
      IF (HS .LT. 0.) GO TO 90
30    WRITE(IPRT,*) 'ENTER STACK INSIDE DIAMETER (M): '
      READ(IRD,*,ERR=30) DS
      WRITE(IDAT,100) DS
      IF (DS .LT. 0.) GO TO 90
C*==========
  401 WRITE(IPRT,*) 'ENTER STACK GAS EXIT VELOCITY OR FLOW RATE:'
      WRITE(IPRT,41)
   41 FORMAT(' OPTION 1 : EXIT VELOCITY (M/S):',
     &     /,'  DEFAULT - ENTER NUMBER ONLY   ')
      WRITE(IPRT,42)
   42 FORMAT(' OPTION 2 : VOLUME FLOW RATE (M**3/S):',
     &     /,'            EXAMPLE "VM=20.00"      ')
      WRITE(IPRT,43)
   43 FORMAT(' OPTION 3 : VOLUME FLOW RATE (ACFM):',
     &     /,'            EXAMPLE "VF=1000.00"     ')
  402 READ(IRD,9044) OPTG
       ILEN = LEN_TRIM(OPTG)
 9044 FORMAT(A80)

C     Convert Lower Case to Upper Case
      CALL LWRUPR(OPTG, ILEN)
C
      IF (OPTG(1:3) .EQ. 'VM=') THEN
         READ(OPTG(4:),'(F20.0)',ERR=48) VM
         IF (VM .LT. 0.) GO TO 90
         WRITE(IDAT,9044) OPTG
         VS = 4.0*VM/(PI*DS**2)
      ELSE IF (OPTG(1:3) .EQ. 'VF=') THEN
         READ(OPTG(4:),'(F20.0)',ERR=48) VF
         IF (VF.LT.0.) GO TO 90
         WRITE(IDAT,9044) OPTG
         VS = (0.3048**3)*VF/(15.0*PI*DS**2)
      ELSE
         READ(OPTG,'(F20.0)',ERR=48) VS
         WRITE(IDAT,100) VS
      END IF

      GO TO 50
   48 WRITE(IPRT,999)
 999  FORMAT(15X,'*************************************',
     &     /,15X,'*  THE OPTION CAN NOT BE PROCESSED  *',
     &     /,15X,'*    PLEASE RE-ENTER YOUR OPTION    *',
     &     /,15X,'*************************************')
      GO TO 401
C*==========
C
50    WRITE(IPRT,*) 'ENTER STACK GAS EXIT TEMPERATURE (K): '
      READ(IRD,*,ERR=50) TS
      WRITE(IDAT,100) TS
      IF (TS .LT. 0.) GO TO 90
60    WRITE(IPRT,*) 'ENTER AMBIENT AIR TEMPERATURE (USE 293 FOR ',
     &              'DEFAULT) (K): '
      READ(IRD,*,ERR=60) TA
      WRITE(IDAT,100) TA
      IF (TA .LT. 0.) GO TO 90
70    WRITE(IPRT,*) 'ENTER RECEPTOR HEIGHT ABOVE GROUND (FOR',
     &              ' FLAGPOLE RECEPTOR) (M): '
      READ(IRD,*,ERR=70) ZR
      WRITE(IDAT,100) ZR
      IF (ZR .LT. 0.) GO TO 90
80    WRITE(IPRT,*) 'ENTER URBAN/RURAL OPTION (U=URBAN, R=RURAL): '
C     Read Input as Character String - First Check for Old IOPT = 1 or 2
C     Then Check for 'U' or 'R'
      READ(IRD,9044) OPTU
      READ(OPTU,'(I20)',ERR=85) IOPT
      WRITE(IDAT,*) IOPT
      IF (IOPT .EQ. 1) THEN
         KOPT = 'U'
         KPRT = 'URBAN'
         URBAN = .TRUE.
         RURAL = .FALSE.
      ELSE IF (IOPT .EQ. 2) THEN
         KOPT = 'R'
         KPRT = 'RURAL'
         RURAL = .TRUE.
         URBAN = .FALSE.
      ELSE
         GO TO 80
      END IF
      GO TO 33
85    CONTINUE
      READ(OPTU,200) KOPT
      IF (KOPT.EQ.'1' .OR. KOPT.EQ.'U' .OR. KOPT.EQ.'u') THEN
         KOPT = 'U'
         KPRT = 'URBAN'
         URBAN = .TRUE.
         RURAL = .FALSE.
      ELSE IF (KOPT.EQ.'2' .OR. KOPT.EQ.'R' .OR. KOPT.EQ.'r') THEN
         KOPT = 'R'
         KPRT = 'RURAL'
         RURAL = .TRUE.
         URBAN = .FALSE.
      ELSE
         GO TO 80
      END IF
      WRITE(IDAT,200) KOPT

33    CONTINUE
3     WRITE(IPRT,*)'CONSIDER BUILDING DOWNWASH IN CALCS?  ENTER Y OR N:'
      READ(IRD,200) QUERY
200   FORMAT(A1)
      IF(QUERY .EQ. 'Y' .OR. QUERY .EQ. 'y') THEN
         WRITE(IDAT,200) QUERY
110      WRITE(IPRT,*) 'ENTER BUILDING HEIGHT (M):'
         READ(IRD,*,ERR=110) HB
         WRITE(IDAT,100) HB
         IF(HB .LT. 0.) GO TO 90
120      WRITE(IPRT,*) 'ENTER MINIMUM HORIZ BLDG DIMENSION (M):'
         READ(IRD,*,ERR=120) HL
         WRITE(IDAT,100) HL
         IF(HL .LT. 0.) GO TO 90
130      WRITE(IPRT,*) 'ENTER MAXIMUM HORIZ BLDG DIMENSION (M):'
         READ(IRD,*,ERR=130) HW
         WRITE(IDAT,100) HW
         IF(HW .LT. 0.) GO TO 90
         IF(HL .GT. HW) THEN
            HLSAV = HL
            HL = HW
            HW = HLSAV
         ENDIF
         HWP = SQRT(HL*HL + HW*HW)
      ELSEIF(QUERY .EQ. 'N' .OR. QUERY .EQ. 'n') THEN
         WRITE(IDAT,200) QUERY
         CONTINUE
      ELSE
         GO TO 3
      ENDIF

C***********************************************************************
C        CHECK FOR COMPLEX TERRAIN SCREENING OPTION
C***********************************************************************
4     WRITE(IPRT,*) 'USE COMPLEX TERRAIN SCREEN FOR ',
     &   'TERRAIN ABOVE STACK HEIGHT?'
      WRITE(IPRT,*) 'ENTER Y OR N:'
      READ(IRD,200) QUERY
      IF (QUERY .EQ. 'Y' .OR. QUERY .EQ. 'y') THEN
         WRITE(IDAT,200) QUERY
         CALL VALLEY
         IF (STP) RETURN
      ELSE IF (QUERY .EQ. 'N' .OR. QUERY .EQ. 'n') THEN
         WRITE(IDAT,200) QUERY
         CONTINUE
      ELSE
         GO TO 4
      END IF
C
C        WRITE DATE, TIME, AND INPUT VALUES TO OUTPUT FILE
C
      WRITE(IOUT,102) RUNDAT, RUNTIM
102   FORMAT(70X,A8/70X,A8)

      WRITE(IOUT,103) VERSN, TITLE, Q, HS, DS, VS, TS, TA, ZR, KPRT, HB,
     &                HL, HW
103   FORMAT(' ',1X,'***  SCREEN3 MODEL RUN  ***',
     &         /,2X,'*** VERSION DATED ',A5,' ***',//,1X,A79,//,
     &       1X,'SIMPLE TERRAIN INPUTS:',/,
     &       1X,'   SOURCE TYPE            =        POINT',/,
     &       1X,'   EMISSION RATE (G/S)    = ',G16.6,/,
     &       1X,'   STACK HEIGHT (M)       = ',F12.4,/,
     &       1X,'   STK INSIDE DIAM (M)    = ',F12.4,/,
     &       1X,'   STK EXIT VELOCITY (M/S)= ',F12.4,/,
     &       1X,'   STK GAS EXIT TEMP (K)  = ',F12.4,/,
     &       1X,'   AMBIENT AIR TEMP (K)   = ',F12.4,/,
     &       1X,'   RECEPTOR HEIGHT (M)    = ',F12.4,/,
     &       1X,'   URBAN/RURAL OPTION     = ',7X,A5,/,
     &       1X,'   BUILDING HEIGHT (M)    = ',F12.4,/,
     &       1X,'   MIN HORIZ BLDG DIM (M) = ',F12.4,/,
     &       1X,'   MAX HORIZ BLDG DIM (M) = ',F12.4,/)
C
      IF (ICI .EQ. 1) THEN
         WRITE(IOUT, 105)
        ELSE
         WRITE(IOUT, 106)
      END IF
      IF (HANE .EQ. 10.0) THEN
         WRITE(IOUT, 107) HANE
        ELSE
         WRITE(IOUT, 108) HANE
      END IF
105   FORMAT(' THE NON-REGULATORY BUT CONSERVATIVE BRODE 2 MIXING'
     +       ' HEIGHT OPTION WAS SELECTED.')
106   FORMAT(' THE REGULATORY (DEFAULT) MIXING HEIGHT OPTION WAS',
     +       ' SELECTED.')
107   FORMAT(' THE REGULATORY (DEFAULT) ANEMOMETER HEIGHT OF',F5.1,
     +       ' METERS WAS ENTERED.'/)
108   FORMAT(' A NON-REGULATORY ANEMOMETER HEIGHT (HANE) OF',F6.1,
     +       ' METERS WAS ENTERED.'/)

C*=====
      IF (OPTG(1:3) .EQ. 'VM=') THEN
         WRITE(IOUT,201) VM
  201    FORMAT(1X,'   STACK EXIT VELOCITY WAS CALCULATED FROM',
     &        /,1X,'   VOLUME FLOW RATE =',G16.8,' (M**3/S) ')
      ELSE IF (OPTG(1:3) .EQ. 'VF=') THEN
         WRITE(IOUT,202) VF
  202    FORMAT(1X,'   STACK EXIT VELOCITY WAS CALCULATED FROM',
     &        /,1X,'   VOLUME FLOW RATE =',G16.8,' (ACFM) ')
      END IF
C*=====
C

C
C        FOR SMALL VS, DS, TS, AND TA SET=1.0E-05 TO AVOID ZERO DIVIDE
C        ERROR AND UNDERFLOW
C
      IF (VS .LT. 1.0E-05) VS=1.0E-05
      IF (DS .LT. 1.0E-05) DS=1.0E-05
      IF (TS .LT. 1.0E-05) TS=1.0E-05
      IF (TA .LT. 1.0E-05) TA=1.0E-05
C
      RETURN
90    WRITE(IPRT,*) 'YOU HAVE ENTERED AN UNACCEPTABLE VALUE.  START',
     &              ' OVER.'
      GO TO 10

      END

      SUBROUTINE INPUTF
C
C        DATA ENTRY ROUTINE FOR FLARES.  CALCULATES
C        EFFECTIVE STACK DIAMETER ASSUMING VS=20.0, TS=1273.
C        ALSO CALCULATES EFFECTIVE RELEASE HEIGHT BASED ON THE
C        LENGTH OF THE FLAME.
C
      INCLUDE 'MAIN.INC'
      NPD = 0

      STP = .FALSE.
10    WRITE(IPRT,*) 'ENTER EMISSION RATE (G/S): '
      READ(IRD,*,ERR=10) Q
      WRITE(IDAT,100) Q
      IF (Q  .LE. 0.) GO TO 90
100   FORMAT(1X,G11.4)
20    WRITE(IPRT,*) 'ENTER FLARE STACK HEIGHT (M): '
      READ(IRD,*,ERR=20) HSTK
      WRITE(IDAT,100) HSTK
      IF (HSTK .LT. 0.) GO TO 90
30    WRITE(IPRT,*) 'ENTER TOTAL HEAT RELEASE RATE (CAL/S):'
      READ(IRD,*,ERR=30) H
      WRITE(IDAT,100) H
      IF (H  .LT. 0.) GO TO 90
40    WRITE(IPRT,*) 'ENTER RECEPTOR HEIGHT ABOVE GROUND (FOR',
     &              ' FLAGPOLE RECEPTOR) (M): '
      READ(IRD,*,ERR=40) ZR
      WRITE(IDAT,100) ZR
      IF (ZR .LT. 0.) GO TO 90
50    WRITE(IPRT,*) 'ENTER URBAN/RURAL OPTION (U=URBAN, R=RURAL): '
C     Read Input as Character String - First Check for Old IOPT = 1 or 2
C     Then Check for 'U' or 'R'
      READ(IRD,9044) OPTU
 9044 FORMAT(A80)
      READ(OPTU,'(I20)',ERR=55) IOPT
      WRITE(IDAT,*) IOPT
      IF (IOPT .EQ. 1) THEN
         KOPT = 'U'
         KPRT = 'URBAN'
         URBAN = .TRUE.
         RURAL = .FALSE.
      ELSE IF (IOPT .EQ. 2) THEN
         KOPT = 'R'
         KPRT = 'RURAL'
         RURAL = .TRUE.
         URBAN = .FALSE.
      ELSE
         GO TO 50
      END IF
      GO TO 33
55    CONTINUE
      READ(OPTU,200) KOPT
      IF (KOPT.EQ.'1' .OR. KOPT.EQ.'U' .OR. KOPT.EQ.'u') THEN
         KOPT = 'U'
         KPRT = 'URBAN'
         URBAN = .TRUE.
         RURAL = .FALSE.
      ELSE IF (KOPT.EQ.'2' .OR. KOPT.EQ.'R' .OR. KOPT.EQ.'r') THEN
         KOPT = 'R'
         KPRT = 'RURAL'
         RURAL = .TRUE.
         URBAN = .FALSE.
      ELSE
         GO TO 50
      END IF
      WRITE(IDAT,200) KOPT

33    CONTINUE
C
C        SET EFFECTIVE STACK PARAMETERS
C
      VS = 20.0
      TS = 1273.
      TA = 293.
      DS = 9.88E-04*(0.45*H)**0.5
      IF (DS .LT. 1E-05) DS=1.0E-05
      HS = HSTK + 4.56E-03 * H**0.478
      WRITE(IPRT,*) 'EFFECTIVE RELEASE HEIGHT =',HS
C
3     WRITE(IPRT,*)'CONSIDER BUILDING DOWNWASH IN CALCS?  ENTER Y OR N:'
      READ(IRD,200) QUERY
      IF(QUERY .EQ. 'Y' .OR. QUERY .EQ. 'y') THEN
         WRITE(IDAT,200) QUERY
60       WRITE(IPRT,*) 'ENTER BUILDING HEIGHT (M):'
         READ(IRD,*,ERR=60) HB
         WRITE(IDAT,100) HB
         IF(HB  .LT. 0.) GO TO 90
70       WRITE(IPRT,*) 'ENTER MINIMUM HORIZ BLDG DIMENSION (M):'
         READ(IRD,*,ERR=70) HL
         WRITE(IDAT,100) HL
         IF(HL  .LT. 0.) GO TO 90
80       WRITE(IPRT,*) 'ENTER MAXIMUM HORIZ BLDG DIMENSION (M):'
         READ(IRD,*,ERR=80) HW
         WRITE(IDAT,100) HW
         IF(HW  .LT. 0.) GO TO 90
         IF(HL .GT. HW) THEN
            HLSAV = HL
            HL = HW
            HW = HLSAV
         ENDIF
         HWP = SQRT(HL*HL + HW*HW)
      ELSEIF(QUERY .EQ. 'N' .OR. QUERY .EQ. 'n') THEN
         WRITE(IDAT,200) QUERY
         CONTINUE
      ELSE
         GO TO 3
      ENDIF

C***********************************************************************
C        CHECK FOR COMPLEX TERRAIN SCREENING OPTION
C***********************************************************************
4     WRITE(IPRT,*) 'USE COMPLEX TERRAIN SCREEN FOR TERRAIN ABOVE',
     &   ' STACK HEIGHT?'
      WRITE(IPRT,*) 'ENTER Y OR N:'
      READ(IRD,200) QUERY
200   FORMAT(A1)
      IF (QUERY .EQ. 'Y' .OR. QUERY .EQ. 'y') THEN
         WRITE(IDAT,200) QUERY
         CALL VALLEY
         IF (STP) RETURN
      ELSE IF (QUERY .EQ. 'N' .OR. QUERY .EQ. 'n') THEN
         WRITE(IDAT,200) QUERY
         CONTINUE
      ELSE
         GO TO 4
      END IF
C
C        WRITE DATE, TIME, AND INPUT VALUES TO OUTPUT FILE
C
      WRITE(IOUT,102) RUNDAT, RUNTIM
102   FORMAT(70X,A8/70X,A8)
      WRITE(IOUT,103) VERSN, TITLE, Q, HSTK, H, ZR, KPRT, HS, HB, HL, HW
103   FORMAT(' ',1X,'***  SCREEN3 MODEL RUN  ***',
     &         /,2X,'*** VERSION DATED ',A5,' ***',//,1X,A79,//,
     &       1X,'SIMPLE TERRAIN INPUTS:',/,
     &       1X,'   SOURCE TYPE            =        FLARE',/,
     &       1X,'   EMISSION RATE (G/S)    = ',G16.6,/,
     &       1X,'   FLARE STACK HEIGHT (M) = ',F12.4,/,
     &       1X,'   TOT HEAT RLS (CAL/S)   = ',G16.6,/,
     &       1X,'   RECEPTOR HEIGHT (M)    = ',F12.4,/,
     &       1X,'   URBAN/RURAL OPTION     = ',7X,A5,/,
     &       1X,'   EFF RELEASE HEIGHT (M) = ',F12.4,/,
     &       1X,'   BUILDING HEIGHT (M)    = ',F12.4,/,
     &       1X,'   MIN HORIZ BLDG DIM (M) = ',F12.4,/,
     &       1X,'   MAX HORIZ BLDG DIM (M) = ',F12.4,/)
C
      IF (ICI .EQ. 1) THEN
         WRITE(IOUT, 105)
        ELSE
         WRITE(IOUT, 106)
      END IF
      IF (HANE .EQ. 10.0) THEN
         WRITE(IOUT, 107) HANE
        ELSE
         WRITE(IOUT, 108) HANE
      END IF
105   FORMAT(' THE NON-REGULATORY BUT CONSERVATIVE BRODE 2 MIXING'
     +       ' HEIGHT OPTION WAS SELECTED.')
106   FORMAT(' THE REGULATORY (DEFAULT) MIXING HEIGHT OPTION WAS',
     +       ' SELECTED.')
107   FORMAT(' THE REGULATORY (DEFAULT) ANEMOMETER HEIGHT OF',F5.1,
     +       ' METERS WAS ENTERED.'/)
108   FORMAT(' A NON-REGULATORY ANEMOMETER HEIGHT (HANE) OF',F6.1,
     +       ' METERS WAS ENTERED.'/)

      RETURN
90    WRITE(IPRT,*) 'YOU HAVE ENTERED AN UNACCEPTABLE VALUE.  START',
     &              ' OVER.'
      GO TO 10
      END

      SUBROUTINE INPUTA
C
C        DATA ENTRY ROUTINE FOR AREA SOURCES.  CALCULATES
C        CONCENTRATIONS USING NUMERICAL INTEGRATION APPROACH.
C
      INCLUDE 'MAIN.INC'
      NPD = 0

      FSTCAL = .TRUE.
      STP = .FALSE.
      VS = 1.0E-05
      DS = 1.0E-05
      TS = 293.
      TA = 293.
10    WRITE(IPRT,*) 'ENTER EMISSION RATE (G/(S-M**2)): '
      READ(IRD,*,ERR=10) Q
      WRITE(IDAT,100) Q
      IF (Q  .LE. 0.) GO TO 90
100   FORMAT(1X,G11.4)
20    WRITE(IPRT,*) 'ENTER SOURCE RELEASE HEIGHT (M): '
      READ(IRD,*,ERR=20) HS
      WRITE(IDAT,100) HS
      IF (HS .LT. 0.) GO TO 90
30    WRITE(IPRT,*) 'ENTER LENGTH OF LARGER SIDE FOR AREA (M):'
      READ(IRD,*,ERR=30) XINIT
      WRITE(IDAT,100) XINIT
      IF (XINIT .LE. 0.) GO TO 90
35    WRITE(IPRT,*) 'ENTER LENGTH OF SMALLER SIDE FOR AREA (M):'
      READ(IRD,*,ERR=35) YINIT
      WRITE(IDAT,100) YINIT
      IF (YINIT .LE. 0.) GO TO 90
      IF (XINIT .LT. YINIT) THEN
         XSAV = XINIT
         XINIT = YINIT
         YINIT = XSAV
      END IF
      ASPECT = XINIT/YINIT
      IF (ASPECT .GT. 10.0) THEN
         WRITE(IPRT,*) 'ASPECT RATIO EXCEEDS 10.  SUBDIVIDE AREA AND',
     &                 ' ENTER DIMENSIONS AGAIN!'
         GO TO 30
      END IF
40    WRITE(IPRT,*) 'ENTER RECEPTOR HEIGHT ABOVE GROUND (FOR',
     &              ' FLAGPOLE RECEPTOR) (M): '
      READ(IRD,*,ERR=40) ZR
      WRITE(IDAT,100) ZR
      IF (ZR .LT. 0.) GO TO 90
50    WRITE(IPRT,*) 'ENTER URBAN/RURAL OPTION (U=URBAN, R=RURAL): '
C     Read Input as Character String - First Check for Old IOPT = 1 or 2
C     Then Check for 'U' or 'R'
      READ(IRD,9044) OPTU
 9044 FORMAT(A80)
      READ(OPTU,'(I20)',ERR=55) IOPT
      WRITE(IDAT,*) IOPT
      IF (IOPT .EQ. 1) THEN
         KOPT = 'U'
         KPRT = 'URBAN'
         URBAN = .TRUE.
         RURAL = .FALSE.
      ELSE IF (IOPT .EQ. 2) THEN
         KOPT = 'R'
         KPRT = 'RURAL'
         RURAL = .TRUE.
         URBAN = .FALSE.
      ELSE
         GO TO 50
      END IF
      GO TO 33
55    CONTINUE
      READ(OPTU,200) KOPT
200   FORMAT(A1)
      IF (KOPT.EQ.'1' .OR. KOPT.EQ.'U' .OR. KOPT.EQ.'u') THEN
         KOPT = 'U'
         KPRT = 'URBAN'
         URBAN = .TRUE.
         RURAL = .FALSE.
      ELSE IF (KOPT.EQ.'2' .OR. KOPT.EQ.'R' .OR. KOPT.EQ.'r') THEN
         KOPT = 'R'
         KPRT = 'RURAL'
         RURAL = .TRUE.
         URBAN = .FALSE.
      ELSE
         GO TO 50
      END IF
      WRITE(IDAT,200) KOPT

33    CONTINUE

4     WRITE(IPRT,*) 'SEARCH THROUGH RANGE OF DIRECTIONS TO FIND',
     &   ' THE MAXIMUM? '
      WRITE(IPRT,*) 'ENTER Y OR N:'
      READ(IRD,200) QUERY
      IF (QUERY .EQ. 'Y' .OR. QUERY .EQ. 'y') THEN
         WRITE(IDAT,200) QUERY
         MAXWD = .TRUE.
         WDIR = 270.
         ANGLE = 0.0
         ANGRAD = 0.0
      ELSE IF (QUERY .EQ. 'N' .OR. QUERY .EQ. 'n') THEN
         WRITE(IDAT,200) QUERY
         MAXWD = .FALSE.
65       WRITE(IPRT,*) 'ENTER DIRECTION RELATIVE TO THE DIRECTION',
     &      ' PERPENDICULAR TO THE SMALLER SIDE: '
         READ(IRD,*,ERR=65) ANGLE
         WRITE(IDAT,100) ANGLE
         WDMAX = ANGLE
         ANGRAD = -1.0 * ANGLE * DTORAD
         WDIR = 270.
      ELSE
         GO TO 4
      END IF
C
C     Set Vertices (in km) for Rectangular Area with Center at (0,0).
C     Rotate Area About the Center to Accommodate User-Input Direction.
      NVERT = 4
      AXVERT(1) =  (0.5*YINIT*SIN(ANGRAD)-0.5*XINIT*COS(ANGRAD))/1000.
      AYVERT(1) = (-0.5*YINIT*COS(ANGRAD)-0.5*XINIT*SIN(ANGRAD))/1000.
      AXVERT(2) = (-0.5*YINIT*SIN(ANGRAD)-0.5*XINIT*COS(ANGRAD))/1000.
      AYVERT(2) =  (0.5*YINIT*COS(ANGRAD)-0.5*XINIT*SIN(ANGRAD))/1000.
      AXVERT(3) = (-0.5*YINIT*SIN(ANGRAD)+0.5*XINIT*COS(ANGRAD))/1000.
      AYVERT(3) =  (0.5*YINIT*COS(ANGRAD)+0.5*XINIT*SIN(ANGRAD))/1000.
      AXVERT(4) =  (0.5*YINIT*SIN(ANGRAD)+0.5*XINIT*COS(ANGRAD))/1000.
      AYVERT(4) = (-0.5*YINIT*COS(ANGRAD)+0.5*XINIT*SIN(ANGRAD))/1000.
      AXVERT(5) = AXVERT(1)
      AYVERT(5) = AYVERT(1)

C     Determine SIN and COS of WDIR
      WDRAD = WDIR * DTORAD
      WDSIN = SIN(WDRAD)
      WDCOS = COS(WDRAD)

      WRITE(IOUT,102) RUNDAT, RUNTIM
102   FORMAT(70X,A8/70X,A8)
      WRITE(IOUT,103) VERSN, TITLE, Q, HS, XINIT, YINIT, ZR, KPRT
103   FORMAT(' ',1X,'***  SCREEN3 MODEL RUN  ***',
     &         /,2X,'*** VERSION DATED ',A5,' ***',//,1X,A79,//,
     &       1X,'SIMPLE TERRAIN INPUTS:',/,
     &       1X,'   SOURCE TYPE                 =         AREA',/,
     &       1X,'   EMISSION RATE (G/(S-M**2))  = ',G16.6,/,
     &       1X,'   SOURCE HEIGHT (M)           = ',F12.4,/,
     &       1X,'   LENGTH OF LARGER SIDE (M)   = ',F12.4,/,
     &       1X,'   LENGTH OF SMALLER SIDE (M)  = ',F12.4,/,
     &       1X,'   RECEPTOR HEIGHT (M)         = ',F12.4,/,
     &       1X,'   URBAN/RURAL OPTION          = ',7X,A5)
      IF (ICI .EQ. 1) THEN
         WRITE(IOUT, 105)
        ELSE
         WRITE(IOUT, 106)
      END IF
      IF (HANE .EQ. 10.0) THEN
         WRITE(IOUT, 107) HANE
        ELSE
         WRITE(IOUT, 108) HANE
      END IF
105   FORMAT(' THE NON-REGULATORY BUT CONSERVATIVE BRODE 2 MIXING'
     +       ' HEIGHT OPTION WAS SELECTED.')
106   FORMAT(' THE REGULATORY (DEFAULT) MIXING HEIGHT OPTION WAS',
     +       ' SELECTED.')
107   FORMAT(' THE REGULATORY (DEFAULT) ANEMOMETER HEIGHT OF',F5.1,
     +       ' METERS WAS ENTERED.'/)
108   FORMAT(' A NON-REGULATORY ANEMOMETER HEIGHT (HANE) OF',F6.1,
     +       ' METERS WAS ENTERED.'/)

      IF (MAXWD) THEN
         WRITE(IOUT,104)
104      FORMAT('    MODEL ESTIMATES DIRECTION TO MAX CONCENTRATION',/)
      ELSE
         WRITE(IOUT,109) ANGLE
109      FORMAT('    ANGLE RELATIVE TO LONG AXIS = ',F12.4,/)
      END IF

      RETURN
90    WRITE(IPRT,*) 'YOU HAVE ENTERED AN UNACCEPTABLE VALUE.  START',
     &              ' OVER.'
      GO TO 10
      END

      SUBROUTINE INPUTV
C
C        DATA ENTRY ROUTINE FOR VOLUME SOURCES.  CALCULATES
C        CONCENTRATIONS USING A VIRTUAL POINT SOURCE ALGORITHM.
C
      INCLUDE 'MAIN.INC'
      NPD = 0

      STP = .FALSE.
      VS = 1.0E-05
      DS = 1.0E-05
      TS = 293.
      TA = 293.
10    WRITE(IPRT,*) 'ENTER EMISSION RATE (G/S): '
      READ(IRD,*,ERR=10) Q
      WRITE(IDAT,100) Q
      IF (Q  .LE. 0.) GO TO 90
100   FORMAT(1X,G11.4)
20    WRITE(IPRT,*) 'ENTER SOURCE RELEASE HEIGHT (M): '
      READ(IRD,*,ERR=20) HS
      WRITE(IDAT,100) HS
      IF (HS .LT. 0.) GO TO 90
30    WRITE(IPRT,*) 'ENTER INITIAL LATERAL DIMENSION OF VOLUME ',
     &              'SOURCE (M):'
      READ(IRD,*,ERR=30) SYINIT
      WRITE(IDAT,100) SYINIT
      IF (SYINIT .LT. 0.) GO TO 90
35    WRITE(IPRT,*) 'ENTER INITIAL VERTICAL DIMENSION OF VOLUME ',
     &              'SOURCE (M):'
      READ(IRD,*,ERR=35) SZINIT
      WRITE(IDAT,100) SZINIT
      IF (SZINIT .LT. 0.) GO TO 90
40    WRITE(IPRT,*) 'ENTER RECEPTOR HEIGHT ABOVE GROUND (FOR',
     &              ' FLAGPOLE RECEPTOR) (M): '
      READ(IRD,*,ERR=40) ZR
      WRITE(IDAT,100) ZR
      IF (ZR .LT. 0.) GO TO 90
50    WRITE(IPRT,*) 'ENTER URBAN/RURAL OPTION (U=URBAN, R=RURAL): '
C     Read Input as Character String - First Check for Old IOPT = 1 or 2
C     Then Check for 'U' or 'R'
      READ(IRD,9044) OPTU
 9044 FORMAT(A80)
      READ(OPTU,'(I20)',ERR=55) IOPT
      WRITE(IDAT,*) IOPT
      IF (IOPT .EQ. 1) THEN
         KOPT = 'U'
         KPRT = 'URBAN'
         URBAN = .TRUE.
         RURAL = .FALSE.
      ELSE IF (IOPT .EQ. 2) THEN
         KOPT = 'R'
         KPRT = 'RURAL'
         RURAL = .TRUE.
         URBAN = .FALSE.
      ELSE
         GO TO 50
      END IF
      GO TO 33
55    CONTINUE
      READ(OPTU,200) KOPT
200   FORMAT(A1)
      IF (KOPT.EQ.'1' .OR. KOPT.EQ.'U' .OR. KOPT.EQ.'u') THEN
         KOPT = 'U'
         KPRT = 'URBAN'
         URBAN = .TRUE.
         RURAL = .FALSE.
      ELSE IF (KOPT.EQ.'2' .OR. KOPT.EQ.'R' .OR. KOPT.EQ.'r') THEN
         KOPT = 'R'
         KPRT = 'RURAL'
         RURAL = .TRUE.
         URBAN = .FALSE.
      ELSE
         GO TO 50
      END IF
      WRITE(IDAT,200) KOPT

33    CONTINUE

      WRITE(IOUT,102) RUNDAT, RUNTIM
102   FORMAT(70X,A8/70X,A8)
      WRITE(IOUT,103) VERSN, TITLE, Q, HS, SYINIT, SZINIT, ZR, KPRT
103   FORMAT(' ',1X,'***  SCREEN3 MODEL RUN  ***',
     &         /,2X,'*** VERSION DATED ',A5,' ***',//,1X,A79,//,
     &       1X,'SIMPLE TERRAIN INPUTS:',/,
     &       1X,'   SOURCE TYPE              =       VOLUME',/,
     &       1X,'   EMISSION RATE (G/S)      = ',G16.6,/,
     &       1X,'   SOURCE HEIGHT (M)        = ',F12.4,/,
     &       1X,'   INIT. LATERAL DIMEN (M)  = ',F12.4,/,
     &       1X,'   INIT. VERTICAL DIMEN (M) = ',F12.4,/,
     &       1X,'   RECEPTOR HEIGHT (M)      = ',F12.4,/,
     &       1X,'   URBAN/RURAL OPTION       = ',7X,A5,/)

      IF (ICI .EQ. 1) THEN
         WRITE(IOUT, 105)
        ELSE
         WRITE(IOUT, 106)
      END IF
      IF (HANE .EQ. 10.0) THEN
         WRITE(IOUT, 107) HANE
        ELSE
         WRITE(IOUT, 108) HANE
      END IF
105   FORMAT(' THE NON-REGULATORY BUT CONSERVATIVE BRODE 2 MIXING'
     +       ' HEIGHT OPTION WAS SELECTED.')
106   FORMAT(' THE REGULATORY (DEFAULT) MIXING HEIGHT OPTION WAS',
     +       ' SELECTED.')
107   FORMAT(' THE REGULATORY (DEFAULT) ANEMOMETER HEIGHT OF',F5.1,
     +       ' METERS WAS ENTERED.'/)
108   FORMAT(' A NON-REGULATORY ANEMOMETER HEIGHT (HANE) OF',F6.1,
     +       ' METERS WAS ENTERED.'/)

      RETURN
90    WRITE(IPRT,*) 'YOU HAVE ENTERED AN UNACCEPTABLE VALUE.  START',
     &              ' OVER.'
      GO TO 10
      END

      SUBROUTINE INPDEP
C
C        DATA ENTRY ROUTINE FOR POINT SOURCES WITH DEPOSITION
C
      INCLUDE 'MAIN.INC'

      STP = .FALSE.
10    WRITE(IPRT,*) 'ENTER EMISSION RATE (G/S): '
      READ(IRD,*,ERR=10) Q
      WRITE(IDAT,100) Q
      IF (Q  .LE. 0.) GO TO 90
100   FORMAT(G14.6)
20    WRITE(IPRT,*) 'ENTER STACK HEIGHT (M): '
      READ(IRD,*,ERR=20) HS
      WRITE(IDAT,100) HS
      IF (HS .LT. 0.) GO TO 90
30    WRITE(IPRT,*) 'ENTER STACK INSIDE DIAMETER (M): '
      READ(IRD,*,ERR=30) DS
      WRITE(IDAT,100) DS
      IF (DS .LT. 0.) GO TO 90
C*==========
  401 WRITE(IPRT,*) 'ENTER STACK GAS EXIT VELOCITY OR FLOW RATE:'
      WRITE(IPRT,41)
   41 FORMAT(' OPTION 1 : EXIT VELOCITY (M/S):',
     &     /,'  DEFAULT - ENTER NUMBER ONLY   ')
      WRITE(IPRT,42)
   42 FORMAT(' OPTION 2 : VOLUME FLOW RATE (M**3/S):',
     &     /,'            EXAMPLE "VM=20.00"      ')
      WRITE(IPRT,43)
   43 FORMAT(' OPTION 3 : VOLUME FLOW RATE (ACFM):',
     &     /,'            EXAMPLE "VF=1000.00"     ')
  402 READ(IRD,9044) OPTG
       ILEN = LEN_TRIM(OPTG)
 9044 FORMAT(A80)

C     Convert Lower Case to Upper Case
      CALL LWRUPR(OPTG, ILEN)
C
      IF (OPTG(1:3) .EQ. 'VM=') THEN
         READ(OPTG(4:),'(F20.0)',ERR=48) VM
         IF (VM .LT. 0.) GO TO 90
         WRITE(IDAT,9044) OPTG
         VS = 4.0*VM/(PI*DS**2)
      ELSE IF (OPTG(1:3) .EQ. 'VF=') THEN
         READ(OPTG(4:),'(F20.0)',ERR=48) VF
         IF (VF.LT.0.) GO TO 90
         WRITE(IDAT,9044) OPTG
         VS = (0.3048**3)*VF/(15.0*PI*DS**2)
      ELSE
         READ(OPTG,'(F20.0)',ERR=48) VS
         WRITE(IDAT,100) VS
      END IF

      GO TO 50
   48 WRITE(IPRT,999)
 999  FORMAT(15X,'*************************************',
     &     /,15X,'*  THE OPTION CAN NOT BE PROCESSED  *',
     &     /,15X,'*    PLEASE RE-ENTER YOUR OPTION    *',
     &     /,15X,'*************************************')
      GO TO 401
C*==========
C
50    WRITE(IPRT,*) 'ENTER STACK GAS EXIT TEMPERATURE (K): '
      READ(IRD,*,ERR=50) TS
      WRITE(IDAT,100) TS
      IF (TS .LT. 0.) GO TO 90
60    WRITE(IPRT,*) 'ENTER AMBIENT AIR TEMPERATURE (USE 293 FOR ',
     &              'DEFAULT) (K): '
      READ(IRD,*,ERR=60) TA
      WRITE(IDAT,100) TA
      IF (TA .LT. 0.) GO TO 90
70    WRITE(IPRT,*) 'ENTER RECEPTOR HEIGHT ABOVE GROUND (FOR',
     &              ' FLAGPOLE RECEPTOR) (M): '
      READ(IRD,*,ERR=70) ZR
      WRITE(IDAT,100) ZR
      IF (ZR .LT. 0.) GO TO 90
80    WRITE(IPRT,*) 'ENTER URBAN/RURAL OPTION (U=URBAN, R=RURAL): '
C     Read Input as Character String - First Check for Old IOPT = 1 or 2
C     Then Check for 'U' or 'R'
      READ(IRD,9044) OPTU
      READ(OPTU,'(I20)',ERR=85) IOPT
      WRITE(IDAT,*) IOPT
      IF (IOPT .EQ. 1) THEN
         KOPT = 'U'
         KPRT = 'URBAN'
         URBAN = .TRUE.
         RURAL = .FALSE.
      ELSE IF (IOPT .EQ. 2) THEN
         KOPT = 'R'
         KPRT = 'RURAL'
         RURAL = .TRUE.
         URBAN = .FALSE.
      ELSE
         GO TO 80
      END IF
      GO TO 33
85    CONTINUE
      READ(OPTU,200) KOPT
      IF (KOPT.EQ.'1' .OR. KOPT.EQ.'U' .OR. KOPT.EQ.'u') THEN
         KOPT = 'U'
         KPRT = 'URBAN'
         URBAN = .TRUE.
         RURAL = .FALSE.
      ELSE IF (KOPT.EQ.'2' .OR. KOPT.EQ.'R' .OR. KOPT.EQ.'r') THEN
         KOPT = 'R'
         KPRT = 'RURAL'
         RURAL = .TRUE.
         URBAN = .FALSE.
      ELSE
         GO TO 80
      END IF
      WRITE(IDAT,200) KOPT

33    CONTINUE

      IF (RURAL) THEN
         WRITE(IPRT,*) 'ENTER SURFACE ROUGHNESS LENGTH (USE 0.3 FOR ',
     &                 'DEFAULT) (M): '
      ELSE IF (URBAN) THEN
         WRITE(IPRT,*) 'ENTER SURFACE ROUGHNESS LENGTH (USE 1.0 FOR ',
     &                 'DEFAULT) (M): '
      END IF
      READ(IRD,*,ERR=33) ZROUGH
      WRITE(IDAT,100) ZROUGH
      IF (ZROUGH .GT. 10.) GO TO 33
      IF (ZROUGH .LT. 0.) GO TO 90
      Z0M = ZROUGH

3     WRITE(IPRT,*)'CONSIDER BUILDING DOWNWASH IN CALCS?  ENTER Y OR N:'
      READ(IRD,200) QUERY
200   FORMAT(A1)
      IF(QUERY .EQ. 'Y' .OR. QUERY .EQ. 'y') THEN
         WRITE(IDAT,200) QUERY
110      WRITE(IPRT,*) 'ENTER BUILDING HEIGHT (M):'
         READ(IRD,*,ERR=110) HB
         WRITE(IDAT,100) HB
         IF(HB .LT. 0.) GO TO 90
120      WRITE(IPRT,*) 'ENTER MINIMUM HORIZ BLDG DIMENSION (M):'
         READ(IRD,*,ERR=120) HL
         WRITE(IDAT,100) HL
         IF(HL .LT. 0.) GO TO 90
130      WRITE(IPRT,*) 'ENTER MAXIMUM HORIZ BLDG DIMENSION (M):'
         READ(IRD,*,ERR=130) HW
         WRITE(IDAT,100) HW
         IF(HW .LT. 0.) GO TO 90
         IF(HL .GT. HW) THEN
            HLSAV = HL
            HL = HW
            HW = HLSAV
         ENDIF
         HWP = SQRT(HL*HL + HW*HW)
      ELSEIF(QUERY .EQ. 'N' .OR. QUERY .EQ. 'n') THEN
         WRITE(IDAT,200) QUERY
         CONTINUE
      ELSE
         GO TO 3
      ENDIF

C***********************************************************************
C        ENTER DEPOSITION PARAMETERS
C***********************************************************************
39    WRITE(IPRT,*)'CONSIDER PLUME DEPLETION EFFECTS?  ENTER Y OR N:'
      READ(IRD,200) QUERY
      IF (QUERY .EQ. 'Y' .OR. QUERY .EQ. 'y') THEN
         WRITE(IDAT,200) QUERY
         DPLETE = .TRUE.
      ELSE IF (QUERY .EQ. 'N' .OR. QUERY .EQ. 'n') THEN
         WRITE(IDAT,200) QUERY
         DPLETE = .FALSE.
      ELSE
         GO TO 39
      END IF

150   WRITE(IPRT,*) 'ENTER PARTICLE DENSITY (USE 1.0 FOR ',
     &              'DEFAULT) (G/CM**3) : '
      READ(IRD,*,ERR=150) PARDEN
      WRITE(IDAT,100) PARDEN
      IF (PARDEN .LT. 0.) GO TO 90
155   WRITE(IPRT,*) 'ENTER NUMBER OF PARTICLE SIZE CATEGORIES (<= 20): '
      READ(IRD,*,ERR=155) NPD
      WRITE(IDAT,*) NPD
      IF (NPD .GT. 20) GO TO 155
      IF (NPD .LT. 0) GO TO 90
160   WRITE(IPRT,*) 'ENTER PARTICLE DIAMETER FOR EACH CATEGORY ',
     &              '(MICRONS): '
      READ(IRD,*,ERR=160) (PDIAM(I), I=1,NPD)
      WRITE(IDAT,*) (PDIAM(I), I=1,NPD)
170   WRITE(IPRT,*) 'ENTER MASS FRACTION FOR EACH CATEGORY ',
     &              '(MUST SUM TO 1.0): '
      READ(IRD,*,ERR=170) (PHI(I), I=1,NPD)
      WRITE(IDAT,*) (PHI(I), I=1,NPD)
      PHISUM = 0.0
      DO 176 I = 1, NPD
         PHISUM = PHISUM + PHI(I)
176   CONTINUE
      IF (PHISUM .LT. 0.98 .OR. PHISUM .GT. 1.02) GO TO 170
C     Assign Particle Density to Each Category
      DO 177 I = 1, NPD
         PDENS(I) = PARDEN
177   CONTINUE

C***********************************************************************
C        CHECK FOR COMPLEX TERRAIN SCREENING OPTION
C***********************************************************************
4     WRITE(IPRT,*) 'USE COMPLEX TERRAIN SCREEN FOR ',
     &   'TERRAIN ABOVE STACK HEIGHT?'
      WRITE(IPRT,*) 'ENTER Y OR N:'
      READ(IRD,200) QUERY
      IF (QUERY .EQ. 'Y' .OR. QUERY .EQ. 'y') THEN
         WRITE(IDAT,200) QUERY
         CALL VALLEY
         IF (STP) RETURN
      ELSE IF (QUERY .EQ. 'N' .OR. QUERY .EQ. 'n') THEN
         WRITE(IDAT,200) QUERY
         CONTINUE
      ELSE
         GO TO 4
      END IF
C
C        WRITE DATE, TIME, AND INPUT VALUES TO OUTPUT FILE
C

      WRITE(IOUT,102) RUNDAT, RUNTIM
102   FORMAT(70X,A8/70X,A8)

      WRITE(IOUT,103) VERSN, TITLE, Q, HS, DS, VS, TS, TA, ZR, KPRT,
     &                ZROUGH, HB, HL, HW
103   FORMAT(' ',1X,'***  SCREEN3 MODEL RUN  ***'
     &         /2X,'*** VERSION DATED ',A5,' ***'//1X,A79//
     &       1X,'SIMPLE TERRAIN INPUTS:',/,
     &       1X,'   SOURCE TYPE                  =        POINT ',
     &                                  'w/ DRY DEPOSITION'/
     &       1X,'   EMISSION RATE (G/S)          = ',G16.6,/
     &       1X,'   STACK HEIGHT (M)             = ',F12.4,/
     &       1X,'   STK INSIDE DIAM (M)          = ',F12.4,/
     &       1X,'   STK EXIT VELOCITY (M/S)      = ',F12.4,/
     &       1X,'   STK GAS EXIT TEMP (K)        = ',F12.4,/
     &       1X,'   AMBIENT AIR TEMP (K)         = ',F12.4,/
     &       1X,'   RECEPTOR HEIGHT (M)          = ',F12.4,/
     &       1X,'   URBAN/RURAL OPTION           = ',7X,A5,/
     &       1X,'   SURFACE ROUGHNESS LENGTH (M) = ',F12.4,/
     &       1X,'   BUILDING HEIGHT (M)          = ',F12.4,/
     &       1X,'   MIN HORIZ BLDG DIM (M)       = ',F12.4,/
     &       1X,'   MAX HORIZ BLDG DIM (M)       = ',F12.4)
      IF (DPLETE) THEN
         WRITE(IOUT,113)
113      FORMAT(1X,'   PLUME DEPLETION OPTION       =           ON',/)
      ELSE
         WRITE(IOUT,114)
114      FORMAT(1X,'   PLUME DEPLETION OPTION       =          OFF',/)
      END IF

      WRITE(IOUT,109) PARDEN
109   FORMAT(1X,'   PARTICLE DENSITY (G/CM**3)   = ',F12.4)
      WRITE(IOUT,111) (PDIAM(I),PHI(I),I=1,NPD)
111   FORMAT(1X,'   PARTICLE DIAMETERS (MICRONS)      MASS FRACTIONS',/
     &         '    ----------------------------      --------------',/
     &      (10X,F12.4,13X,F12.4))

C*=====
      IF (OPTG(1:3) .EQ. 'VM=') THEN
         WRITE(IOUT,201) VM
  201    FORMAT(1X,'   STACK EXIT VELOCITY WAS CALCULATED FROM',
     &        /,1X,'   VOLUME FLOW RATE =',G16.8,' (M**3/S) ')
      ELSE IF (OPTG(1:3) .EQ. 'VF=') THEN
         WRITE(IOUT,202) VF
  202    FORMAT(1X,'   STACK EXIT VELOCITY WAS CALCULATED FROM',
     &        /,1X,'   VOLUME FLOW RATE =',G16.8,' (ACFM) ')
      END IF
C*=====
C

C
C        FOR SMALL VS, DS, TS, AND TA SET=1.0E-05 TO AVOID ZERO DIVIDE
C        ERROR AND UNDERFLOW
C
      IF (VS .LT. 1.0E-05) VS=1.0E-05
      IF (DS .LT. 1.0E-05) DS=1.0E-05
      IF (TS .LT. 1.0E-05) TS=1.0E-05
      IF (TA .LT. 1.0E-05) TA=1.0E-05
C
      RETURN
90    WRITE(IPRT,*) 'YOU HAVE ENTERED AN UNACCEPTABLE VALUE.  START',
     &              ' OVER.'
      GO TO 10

      END

      SUBROUTINE INFDEP
C
C        DATA ENTRY ROUTINE FOR FLARES WITH DEPOSITION.  CALCULATES
C        EFFECTIVE STACK DIAMETER ASSUMING VS=20.0, TS=1273.
C        ALSO CALCULATES EFFECTIVE RELEASE HEIGHT BASED ON THE
C        LENGTH OF THE FLAME.
C
      INCLUDE 'MAIN.INC'

      STP = .FALSE.
10    WRITE(IPRT,*) 'ENTER EMISSION RATE (G/S): '
      READ(IRD,*,ERR=10) Q
      WRITE(IDAT,100) Q
      IF (Q  .LE. 0.) GO TO 90
100   FORMAT(1X,G11.4)
20    WRITE(IPRT,*) 'ENTER FLARE STACK HEIGHT (M): '
      READ(IRD,*,ERR=20) HSTK
      WRITE(IDAT,100) HSTK
      IF (HSTK .LT. 0.) GO TO 90
30    WRITE(IPRT,*) 'ENTER TOTAL HEAT RELEASE RATE (CAL/S):'
      READ(IRD,*,ERR=30) H
      WRITE(IDAT,100) H
      IF (H  .LT. 0.) GO TO 90
40    WRITE(IPRT,*) 'ENTER RECEPTOR HEIGHT ABOVE GROUND (FOR',
     &              ' FLAGPOLE RECEPTOR) (M): '
      READ(IRD,*,ERR=40) ZR
      WRITE(IDAT,100) ZR
      IF (ZR .LT. 0.) GO TO 90
50    WRITE(IPRT,*) 'ENTER URBAN/RURAL OPTION (U=URBAN, R=RURAL): '
C     Read Input as Character String - First Check for Old IOPT = 1 or 2
C     Then Check for 'U' or 'R'
      READ(IRD,9044) OPTU
 9044 FORMAT(A80)
      READ(OPTU,'(I20)',ERR=55) IOPT
      WRITE(IDAT,*) IOPT
      IF (IOPT .EQ. 1) THEN
         KOPT = 'U'
         KPRT = 'URBAN'
         URBAN = .TRUE.
         RURAL = .FALSE.
      ELSE IF (IOPT .EQ. 2) THEN
         KOPT = 'R'
         KPRT = 'RURAL'
         RURAL = .TRUE.
         URBAN = .FALSE.
      ELSE
         GO TO 50
      END IF
      GO TO 33
55    CONTINUE
      READ(OPTU,200) KOPT
      IF (KOPT.EQ.'1' .OR. KOPT.EQ.'U' .OR. KOPT.EQ.'u') THEN
         KOPT = 'U'
         KPRT = 'URBAN'
         URBAN = .TRUE.
         RURAL = .FALSE.
      ELSE IF (KOPT.EQ.'2' .OR. KOPT.EQ.'R' .OR. KOPT.EQ.'r') THEN
         KOPT = 'R'
         KPRT = 'RURAL'
         RURAL = .TRUE.
         URBAN = .FALSE.
      ELSE
         GO TO 50
      END IF
      WRITE(IDAT,200) KOPT

33    CONTINUE

      IF (RURAL) THEN
         WRITE(IPRT,*) 'ENTER SURFACE ROUGHNESS LENGTH (USE 0.3 FOR ',
     &                 'DEFAULT) (M): '
      ELSE IF (URBAN) THEN
         WRITE(IPRT,*) 'ENTER SURFACE ROUGHNESS LENGTH (USE 1.0 FOR ',
     &                 'DEFAULT) (M): '
      END IF
      READ(IRD,*,ERR=33) ZROUGH
      WRITE(IDAT,100) ZROUGH
      IF (ZROUGH .GT. 10.) GO TO 33
      IF (ZROUGH .LT. 0.) GO TO 90
      Z0M = ZROUGH

C
C        SET EFFECTIVE STACK PARAMETERS
C
      VS = 20.0
      TS = 1273.
      TA = 293.
      DS = 9.88E-04*(0.45*H)**0.5
      IF (DS .LT. 1E-05) DS=1.0E-05
      HS = HSTK + 4.56E-03 * H**0.478
      WRITE(IPRT,*) 'EFFECTIVE RELEASE HEIGHT =',HS
C
3     WRITE(IPRT,*)'CONSIDER BUILDING DOWNWASH IN CALCS?  ENTER Y OR N:'
      READ(IRD,200) QUERY
      IF(QUERY .EQ. 'Y' .OR. QUERY .EQ. 'y') THEN
         WRITE(IDAT,200) QUERY
60       WRITE(IPRT,*) 'ENTER BUILDING HEIGHT (M):'
         READ(IRD,*,ERR=60) HB
         WRITE(IDAT,100) HB
         IF(HB  .LT. 0.) GO TO 90
70       WRITE(IPRT,*) 'ENTER MINIMUM HORIZ BLDG DIMENSION (M):'
         READ(IRD,*,ERR=70) HL
         WRITE(IDAT,100) HL
         IF(HL  .LT. 0.) GO TO 90
80       WRITE(IPRT,*) 'ENTER MAXIMUM HORIZ BLDG DIMENSION (M):'
         READ(IRD,*,ERR=80) HW
         WRITE(IDAT,100) HW
         IF(HW  .LT. 0.) GO TO 90
         IF(HL .GT. HW) THEN
            HLSAV = HL
            HL = HW
            HW = HLSAV
         ENDIF
         HWP = SQRT(HL*HL + HW*HW)
      ELSEIF(QUERY .EQ. 'N' .OR. QUERY .EQ. 'n') THEN
         WRITE(IDAT,200) QUERY
         CONTINUE
      ELSE
         GO TO 3
      ENDIF

C***********************************************************************
C        ENTER DEPOSITION PARAMETERS
C***********************************************************************
39    WRITE(IPRT,*)'CONSIDER PLUME DEPLETION EFFECTS?  ENTER Y OR N:'
      READ(IRD,200) QUERY
      IF (QUERY .EQ. 'Y' .OR. QUERY .EQ. 'y') THEN
         WRITE(IDAT,200) QUERY
         DPLETE = .TRUE.
      ELSE IF (QUERY .EQ. 'N' .OR. QUERY .EQ. 'n') THEN
         WRITE(IDAT,200) QUERY
         DPLETE = .FALSE.
      ELSE
         GO TO 39
      END IF

150   WRITE(IPRT,*) 'ENTER PARTICLE DENSITY (USE 1.0 FOR ',
     &              'DEFAULT) (G/CM**3) : '
      READ(IRD,*,ERR=150) PARDEN
      WRITE(IDAT,100) PARDEN
      IF (PARDEN .LT. 0.) GO TO 90
155   WRITE(IPRT,*) 'ENTER NUMBER OF PARTICLE SIZE CATEGORIES (<= 20): '
      READ(IRD,*,ERR=155) NPD
      WRITE(IDAT,*) NPD
      IF (NPD .GT. 20) GO TO 155
      IF (NPD .LT. 0) GO TO 90
160   WRITE(IPRT,*) 'ENTER PARTICLE DIAMETER FOR EACH CATEGORY ',
     &              '(MICRONS): '
      READ(IRD,*,ERR=160) (PDIAM(I), I=1,NPD)
      WRITE(IDAT,*) (PDIAM(I), I=1,NPD)
170   WRITE(IPRT,*) 'ENTER MASS FRACTION FOR EACH CATEGORY ',
     &              '(MUST SUM TO 1.0): '
      READ(IRD,*,ERR=170) (PHI(I), I=1,NPD)
      WRITE(IDAT,*) (PHI(I), I=1,NPD)
      PHISUM = 0.0
      DO 176 I = 1, NPD
         PHISUM = PHISUM + PHI(I)
176   CONTINUE
      IF (PHISUM .LT. 0.98 .OR. PHISUM .GT. 1.02) GO TO 170
C     Assign Particle Density to Each Category
      DO 177 I = 1, NPD
         PDENS(I) = PARDEN
177   CONTINUE

C***********************************************************************
C        CHECK FOR COMPLEX TERRAIN SCREENING OPTION
C***********************************************************************
4     WRITE(IPRT,*) 'USE COMPLEX TERRAIN SCREEN FOR TERRAIN ABOVE',
     &   ' STACK HEIGHT?'
      WRITE(IPRT,*) 'ENTER Y OR N:'
      READ(IRD,200) QUERY
200   FORMAT(A1)
      IF (QUERY .EQ. 'Y' .OR. QUERY .EQ. 'y') THEN
         WRITE(IDAT,200) QUERY
         CALL VALLEY
         IF (STP) RETURN
      ELSE IF (QUERY .EQ. 'N' .OR. QUERY .EQ. 'n') THEN
         WRITE(IDAT,200) QUERY
         CONTINUE
      ELSE
         GO TO 4
      END IF
C
C        WRITE DATE, TIME, AND INPUT VALUES TO OUTPUT FILE
C
      WRITE(IOUT,102) RUNDAT, RUNTIM
102   FORMAT(70X,A8/70X,A8)
      WRITE(IOUT,103) VERSN, TITLE, Q, HSTK, H, ZR, KPRT, ZROUGH,
     &                HS, HB, HL, HW
103   FORMAT(' ',1X,'***  SCREEN3 MODEL RUN  ***',
     &         /,2X,'*** VERSION DATED ',A5,' ***',//,1X,A79,//,
     &       1X,'SIMPLE TERRAIN INPUTS:',/,
     &       1X,'   SOURCE TYPE                  =        FLARE ',
     &                                           'w/ DRY DEPOSITION',/,
     &       1X,'   EMISSION RATE (G/S)          = ',G16.6,/,
     &       1X,'   FLARE STACK HEIGHT (M)       = ',F12.4,/,
     &       1X,'   TOT HEAT RLS (CAL/S)         = ',G16.6,/,
     &       1X,'   RECEPTOR HEIGHT (M)          = ',F12.4,/,
     &       1X,'   URBAN/RURAL OPTION           = ',7X,A5,/,
     &       1X,'   SURFACE ROUGHNESS LENGTH (M) = ',F12.4,/,
     &       1X,'   EFF RELEASE HEIGHT (M)       = ',F12.4,/,
     &       1X,'   BUILDING HEIGHT (M)          = ',F12.4,/,
     &       1X,'   MIN HORIZ BLDG DIM (M)       = ',F12.4,/,
     &       1X,'   MAX HORIZ BLDG DIM (M)       = ',F12.4,/)
      IF (DPLETE) THEN
         WRITE(IOUT,113)
113      FORMAT(1X,'   PLUME DEPLETION OPTION       =           ON',/)
      ELSE
         WRITE(IOUT,114)
114      FORMAT(1X,'   PLUME DEPLETION OPTION       =          OFF',/)
      END IF
      WRITE(IOUT,109) PARDEN
109   FORMAT(1X,'   PARTICLE DENSITY (G/CM**3)   = ',F12.4)
      WRITE(IOUT,111) (PDIAM(I),PHI(I),I=1,NPD)
111   FORMAT(1X,'   PARTICLE DIAMETERS (MICRONS)      MASS FRACTIONS',/
     &         '    ----------------------------      --------------',/
     &      (10X,F12.4,13X,F12.4))

C

      RETURN
90    WRITE(IPRT,*) 'YOU HAVE ENTERED AN UNACCEPTABLE VALUE.  START',
     &              ' OVER.'
      GO TO 10
      END

      SUBROUTINE INADEP
C
C        DATA ENTRY ROUTINE FOR AREA SOURCES WITH DEPOSITION.
C        CALCULATES CONCENTRATIONS USING NUMERICAL INTEGRATION APPROACH.
C
      INCLUDE 'MAIN.INC'

      FSTCAL = .TRUE.
      STP = .FALSE.
      VS = 1.0E-05
      DS = 1.0E-05
      TS = 293.
      TA = 293.
10    WRITE(IPRT,*) 'ENTER EMISSION RATE (G/(S-M**2)): '
      READ(IRD,*,ERR=10) Q
      WRITE(IDAT,100) Q
      IF (Q  .LE. 0.) GO TO 90
100   FORMAT(1X,G11.4)
20    WRITE(IPRT,*) 'ENTER SOURCE RELEASE HEIGHT (M): '
      READ(IRD,*,ERR=20) HS
      WRITE(IDAT,100) HS
      IF (HS .LT. 0.) GO TO 90
30    WRITE(IPRT,*) 'ENTER LENGTH OF LARGER SIDE FOR AREA (M):'
      READ(IRD,*,ERR=30) XINIT
      WRITE(IDAT,100) XINIT
      IF (XINIT .LE. 0.) GO TO 90
35    WRITE(IPRT,*) 'ENTER LENGTH OF SMALLER SIDE FOR AREA (M):'
      READ(IRD,*,ERR=35) YINIT
      WRITE(IDAT,100) YINIT
      IF (YINIT .LE. 0.) GO TO 90
      IF (XINIT .LT. YINIT) THEN
         XSAV = XINIT
         XINIT = YINIT
         YINIT = XSAV
      END IF
      ASPECT = XINIT/YINIT
      IF (ASPECT .GT. 10.0) THEN
         WRITE(IPRT,*) 'ASPECT RATIO EXCEEDS 10.  SUBDIVIDE AREA AND',
     &                 ' ENTER DIMENSIONS AGAIN!'
         GO TO 30
      END IF
40    WRITE(IPRT,*) 'ENTER RECEPTOR HEIGHT ABOVE GROUND (FOR',
     &              ' FLAGPOLE RECEPTOR) (M): '
      READ(IRD,*,ERR=40) ZR
      WRITE(IDAT,100) ZR
      IF (ZR .LT. 0.) GO TO 90
50    WRITE(IPRT,*) 'ENTER URBAN/RURAL OPTION (U=URBAN, R=RURAL): '
C     Read Input as Character String - First Check for Old IOPT = 1 or 2
C     Then Check for 'U' or 'R'
      READ(IRD,9044) OPTU
 9044 FORMAT(A80)
      READ(OPTU,'(I20)',ERR=55) IOPT
      WRITE(IDAT,*) IOPT
      IF (IOPT .EQ. 1) THEN
         KOPT = 'U'
         KPRT = 'URBAN'
         URBAN = .TRUE.
         RURAL = .FALSE.
      ELSE IF (IOPT .EQ. 2) THEN
         KOPT = 'R'
         KPRT = 'RURAL'
         RURAL = .TRUE.
         URBAN = .FALSE.
      ELSE
         GO TO 50
      END IF
      GO TO 33
55    CONTINUE
      READ(OPTU,200) KOPT
200   FORMAT(A1)
      IF (KOPT.EQ.'1' .OR. KOPT.EQ.'U' .OR. KOPT.EQ.'u') THEN
         KOPT = 'U'
         KPRT = 'URBAN'
         URBAN = .TRUE.
         RURAL = .FALSE.
      ELSE IF (KOPT.EQ.'2' .OR. KOPT.EQ.'R' .OR. KOPT.EQ.'r') THEN
         KOPT = 'R'
         KPRT = 'RURAL'
         RURAL = .TRUE.
         URBAN = .FALSE.
      ELSE
         GO TO 50
      END IF
      WRITE(IDAT,200) KOPT

33    CONTINUE

      IF (RURAL) THEN
         WRITE(IPRT,*) 'ENTER SURFACE ROUGHNESS LENGTH (USE 0.3 FOR ',
     &                 'DEFAULT) (M): '
      ELSE IF (URBAN) THEN
         WRITE(IPRT,*) 'ENTER SURFACE ROUGHNESS LENGTH (USE 1.0 FOR ',
     &                 'DEFAULT) (M): '
      END IF
      READ(IRD,*,ERR=33) ZROUGH
      WRITE(IDAT,100) ZROUGH
      IF (ZROUGH .GT. 10.) GO TO 33
      IF (ZROUGH .LT. 0.) GO TO 90
      Z0M = ZROUGH

4     WRITE(IPRT,*) 'SEARCH THROUGH RANGE OF DIRECTIONS TO FIND',
     &   ' THE MAXIMUM? '
      WRITE(IPRT,*) 'ENTER Y OR N:'
      READ(IRD,200) QUERY
      IF (QUERY .EQ. 'Y' .OR. QUERY .EQ. 'y') THEN
         WRITE(IDAT,200) QUERY
         MAXWD = .TRUE.
         WDIR = 270.
         ANGLE = 0.0
         ANGRAD = 0.0
      ELSE IF (QUERY .EQ. 'N' .OR. QUERY .EQ. 'n') THEN
         WRITE(IDAT,200) QUERY
         MAXWD = .FALSE.
65       WRITE(IPRT,*) 'ENTER DIRECTION RELATIVE TO THE DIRECTION',
     &      ' PERPENDICULAR TO THE SMALLER SIDE: '
         READ(IRD,*,ERR=65) ANGLE
         WRITE(IDAT,100) ANGLE
         WDMAX = ANGLE
         ANGRAD = -1.0 * ANGLE * DTORAD
         WDIR = 270.
      ELSE
         GO TO 4
      END IF
C
C     Set Vertices (in km) for Rectangular Area with Center at (0,0).
C     Rotate Area About the Center to Accommodate User-Input Direction.
      NVERT = 4
      AXVERT(1) =  (0.5*YINIT*SIN(ANGRAD)-0.5*XINIT*COS(ANGRAD))/1000.
      AYVERT(1) = (-0.5*YINIT*COS(ANGRAD)-0.5*XINIT*SIN(ANGRAD))/1000.
      AXVERT(2) = (-0.5*YINIT*SIN(ANGRAD)-0.5*XINIT*COS(ANGRAD))/1000.
      AYVERT(2) =  (0.5*YINIT*COS(ANGRAD)-0.5*XINIT*SIN(ANGRAD))/1000.
      AXVERT(3) = (-0.5*YINIT*SIN(ANGRAD)+0.5*XINIT*COS(ANGRAD))/1000.
      AYVERT(3) =  (0.5*YINIT*COS(ANGRAD)+0.5*XINIT*SIN(ANGRAD))/1000.
      AXVERT(4) =  (0.5*YINIT*SIN(ANGRAD)+0.5*XINIT*COS(ANGRAD))/1000.
      AYVERT(4) = (-0.5*YINIT*COS(ANGRAD)+0.5*XINIT*SIN(ANGRAD))/1000.
      AXVERT(5) = AXVERT(1)
      AYVERT(5) = AYVERT(1)

C     Determine SIN and COS of WDIR
      WDRAD = WDIR * DTORAD
      WDSIN = SIN(WDRAD)
      WDCOS = COS(WDRAD)

C***********************************************************************
C        ENTER DEPOSITION PARAMETERS
C***********************************************************************
39    WRITE(IPRT,*)'CONSIDER PLUME DEPLETION EFFECTS?  ENTER Y OR N:'
      READ(IRD,200) QUERY
      IF (QUERY .EQ. 'Y' .OR. QUERY .EQ. 'y') THEN
         WRITE(IDAT,200) QUERY
         DPLETE = .TRUE.
C        Write caution about long runtime for AREA source w/ depletion
         WRITE(IPRT,*) '********************************************'
         WRITE(IPRT,*) 'YOU HAVE SELECTED THE PLUME DEPLETION OPTION'
         WRITE(IPRT,*) 'FOR AN AREA SOURCE.  THIS OPTION MAY TAKE A '
         WRITE(IPRT,*) 'VERY LONG TIME TO EXECUTE.  FOR A FASTER AND'
         WRITE(IPRT,*) 'MORE CONSERVATIVE ANSWER, CONSIDER PRESSING '
         WRITE(IPRT,*) 'CTRL+BREAK TO EXIT AND RERUN WITHOUT USING  '
         WRITE(IPRT,*) 'THE DEPLETION OPTION.                       '
         WRITE(IPRT,*) '********************************************'
      ELSE IF (QUERY .EQ. 'N' .OR. QUERY .EQ. 'n') THEN
         WRITE(IDAT,200) QUERY
         DPLETE = .FALSE.
      ELSE
         GO TO 39
      END IF

150   WRITE(IPRT,*) 'ENTER PARTICLE DENSITY (USE 1.0 FOR ',
     &              'DEFAULT) (G/CM**3) : '
      READ(IRD,*,ERR=150) PARDEN
      WRITE(IDAT,100) PARDEN
      IF (PARDEN .LT. 0.) GO TO 90
155   WRITE(IPRT,*) 'ENTER NUMBER OF PARTICLE SIZE CATEGORIES (<= 20): '
      READ(IRD,*,ERR=155) NPD
      WRITE(IDAT,*) NPD
      IF (NPD .GT. 20) GO TO 155
      IF (NPD .LT. 0) GO TO 90
160   WRITE(IPRT,*) 'ENTER PARTICLE DIAMETER FOR EACH CATEGORY ',
     &              '(MICRONS): '
      READ(IRD,*,ERR=160) (PDIAM(I), I=1,NPD)
      WRITE(IDAT,*) (PDIAM(I), I=1,NPD)
170   WRITE(IPRT,*) 'ENTER MASS FRACTION FOR EACH CATEGORY ',
     &              '(MUST SUM TO 1.0): '
      READ(IRD,*,ERR=170) (PHI(I), I=1,NPD)
      WRITE(IDAT,*) (PHI(I), I=1,NPD)
      PHISUM = 0.0
      DO 176 I = 1, NPD
         PHISUM = PHISUM + PHI(I)
176   CONTINUE
      IF (PHISUM .LT. 0.98 .OR. PHISUM .GT. 1.02) GO TO 170
C     Assign Particle Density to Each Category
      DO 177 I = 1, NPD
         PDENS(I) = PARDEN
177   CONTINUE

      WRITE(IOUT,102) RUNDAT, RUNTIM
102   FORMAT(70X,A8/70X,A8)
      WRITE(IOUT,103) VERSN, TITLE, Q, HS, XINIT, YINIT, ZR, KPRT,
     &                ZROUGH
103   FORMAT(' ',1X,'***  SCREEN3 MODEL RUN  ***',
     &         /,2X,'*** VERSION DATED ',A5,' ***',//,1X,A79,//,
     &       1X,'SIMPLE TERRAIN INPUTS:',/,
     &       1X,'   SOURCE TYPE                  =         AREA ',
     &                                           'w/ DRY DEPOSITION',/,
     &       1X,'   EMISSION RATE (G/(S-M**2))   = ',G16.6,/,
     &       1X,'   SOURCE HEIGHT (M)            = ',F12.4,/,
     &       1X,'   LENGTH OF LARGER SIDE (M)    = ',F12.4,/,
     &       1X,'   LENGTH OF SMALLER SIDE (M)   = ',F12.4,/,
     &       1X,'   RECEPTOR HEIGHT (M)          = ',F12.4,/,
     &       1X,'   URBAN/RURAL OPTION           = ',7X,A5,/,
     &       1X,'   SURFACE ROUGHNESS LENGTH (M) = ',F12.4)
      IF (MAXWD) THEN
         WRITE(IOUT,104)
104      FORMAT('    MODEL ESTIMATES DIRECTION TO MAX CONCENTRATION')
      ELSE
         WRITE(IOUT,105) ANGLE
105      FORMAT(1X,'   ANGLE RELATIVE TO LONG AXIS  = ',F12.4)
      END IF

      IF (DPLETE) THEN
         WRITE(IOUT,113)
113      FORMAT(1X,'   PLUME DEPLETION OPTION       =           ON',/)
      ELSE
         WRITE(IOUT,114)
114      FORMAT(1X,'   PLUME DEPLETION OPTION       =          OFF',/)
      END IF
      WRITE(IOUT,109) PARDEN
109   FORMAT(1X,'   PARTICLE DENSITY (G/CM**3)   = ',F12.4)
      WRITE(IOUT,111) (PDIAM(I),PHI(I),I=1,NPD)
111   FORMAT(1X,'   PARTICLE DIAMETERS (MICRONS)      MASS FRACTIONS',/
     &         '    ----------------------------      --------------',/
     &      (10X,F12.4,13X,F12.4))

      RETURN
90    WRITE(IPRT,*) 'YOU HAVE ENTERED AN UNACCEPTABLE VALUE.  START',
     &              ' OVER.'
      GO TO 10
      END

      SUBROUTINE INVDEP
C
C        DATA ENTRY ROUTINE FOR VOLUME SOURCES WITH DEPOSITION.
C        CALCULATES CONCENTRATIONS USING A VIRTUAL POINT SOURCE ALGORITHM.
C
      INCLUDE 'MAIN.INC'

      STP = .FALSE.
      VS = 1.0E-05
      DS = 1.0E-05
      TS = 293.
      TA = 293.
10    WRITE(IPRT,*) 'ENTER EMISSION RATE (G/S): '
      READ(IRD,*,ERR=10) Q
      WRITE(IDAT,100) Q
      IF (Q  .LE. 0.) GO TO 90
100   FORMAT(1X,G11.4)
20    WRITE(IPRT,*) 'ENTER SOURCE RELEASE HEIGHT (M): '
      READ(IRD,*,ERR=20) HS
      WRITE(IDAT,100) HS
      IF (HS .LT. 0.) GO TO 90
30    WRITE(IPRT,*) 'ENTER INITIAL LATERAL DIMENSION OF VOLUME ',
     &              'SOURCE (M):'
      READ(IRD,*,ERR=30) SYINIT
      WRITE(IDAT,100) SYINIT
      IF (SYINIT .LT. 0.) GO TO 90
35    WRITE(IPRT,*) 'ENTER INITIAL VERTICAL DIMENSION OF VOLUME ',
     &              'SOURCE (M):'
      READ(IRD,*,ERR=35) SZINIT
      WRITE(IDAT,100) SZINIT
      IF (SZINIT .LT. 0.) GO TO 90
40    WRITE(IPRT,*) 'ENTER RECEPTOR HEIGHT ABOVE GROUND (FOR',
     &              ' FLAGPOLE RECEPTOR) (M): '
      READ(IRD,*,ERR=40) ZR
      WRITE(IDAT,100) ZR
      IF (ZR .LT. 0.) GO TO 90
50    WRITE(IPRT,*) 'ENTER URBAN/RURAL OPTION (U=URBAN, R=RURAL): '
C     Read Input as Character String - First Check for Old IOPT = 1 or 2
C     Then Check for 'U' or 'R'
      READ(IRD,9044) OPTU
 9044 FORMAT(A80)
      READ(OPTU,'(I20)',ERR=55) IOPT
      WRITE(IDAT,*) IOPT
      IF (IOPT .EQ. 1) THEN
         KOPT = 'U'
         KPRT = 'URBAN'
         URBAN = .TRUE.
         RURAL = .FALSE.
      ELSE IF (IOPT .EQ. 2) THEN
         KOPT = 'R'
         KPRT = 'RURAL'
         RURAL = .TRUE.
         URBAN = .FALSE.
      ELSE
         GO TO 50
      END IF
      GO TO 33
55    CONTINUE
      READ(OPTU,200) KOPT
200   FORMAT(A1)
      IF (KOPT.EQ.'1' .OR. KOPT.EQ.'U' .OR. KOPT.EQ.'u') THEN
         KOPT = 'U'
         KPRT = 'URBAN'
         URBAN = .TRUE.
         RURAL = .FALSE.
      ELSE IF (KOPT.EQ.'2' .OR. KOPT.EQ.'R' .OR. KOPT.EQ.'r') THEN
         KOPT = 'R'
         KPRT = 'RURAL'
         RURAL = .TRUE.
         URBAN = .FALSE.
      ELSE
         GO TO 50
      END IF
      WRITE(IDAT,200) KOPT

33    CONTINUE

      IF (RURAL) THEN
         WRITE(IPRT,*) 'ENTER SURFACE ROUGHNESS LENGTH (USE 0.3 FOR ',
     &                 'DEFAULT) (M): '
      ELSE IF (URBAN) THEN
         WRITE(IPRT,*) 'ENTER SURFACE ROUGHNESS LENGTH (USE 1.0 FOR ',
     &                 'DEFAULT) (M): '
      END IF
      READ(IRD,*,ERR=33) ZROUGH
      WRITE(IDAT,100) ZROUGH
      IF (ZROUGH .GT. 10.) GO TO 33
      IF (ZROUGH .LT. 0.) GO TO 90
      Z0M = ZROUGH

C***********************************************************************
C        ENTER DEPOSITION PARAMETERS
C***********************************************************************
39    WRITE(IPRT,*)'CONSIDER PLUME DEPLETION EFFECTS?  ENTER Y OR N:'
      READ(IRD,200) QUERY
      IF (QUERY .EQ. 'Y' .OR. QUERY .EQ. 'y') THEN
         WRITE(IDAT,200) QUERY
         DPLETE = .TRUE.
      ELSE IF (QUERY .EQ. 'N' .OR. QUERY .EQ. 'n') THEN
         WRITE(IDAT,200) QUERY
         DPLETE = .FALSE.
      ELSE
         GO TO 39
      END IF

150   WRITE(IPRT,*) 'ENTER PARTICLE DENSITY (USE 1.0 FOR ',
     &              'DEFAULT) (G/CM**3) : '
      READ(IRD,*,ERR=150) PARDEN
      WRITE(IDAT,100) PARDEN
      IF (PARDEN .LT. 0.) GO TO 90
155   WRITE(IPRT,*) 'ENTER NUMBER OF PARTICLE SIZE CATEGORIES (<= 20): '
      READ(IRD,*,ERR=155) NPD
      WRITE(IDAT,*) NPD
      IF (NPD .GT. 20) GO TO 155
      IF (NPD .LT. 0) GO TO 90
160   WRITE(IPRT,*) 'ENTER PARTICLE DIAMETER FOR EACH CATEGORY ',
     &              '(MICRONS): '
      READ(IRD,*,ERR=160) (PDIAM(I), I=1,NPD)
      WRITE(IDAT,*) (PDIAM(I), I=1,NPD)
170   WRITE(IPRT,*) 'ENTER MASS FRACTION FOR EACH CATEGORY ',
     &              '(MUST SUM TO 1.0): '
      READ(IRD,*,ERR=170) (PHI(I), I=1,NPD)
      WRITE(IDAT,*) (PHI(I), I=1,NPD)
      PHISUM = 0.0
      DO 176 I = 1, NPD
         PHISUM = PHISUM + PHI(I)
176   CONTINUE
      IF (PHISUM .LT. 0.98 .OR. PHISUM .GT. 1.02) GO TO 170
C     Assign Particle Density to Each Category
      DO 177 I = 1, NPD
         PDENS(I) = PARDEN
177   CONTINUE

      WRITE(IOUT,102) RUNDAT, RUNTIM
102   FORMAT(70X,A8/70X,A8)
      WRITE(IOUT,103) VERSN, TITLE, Q, HS, SYINIT, SZINIT, ZR, KPRT,
     &                ZROUGH
103   FORMAT(' ',1X,'***  SCREEN3 MODEL RUN  ***',
     &         /,2X,'*** VERSION DATED ',A5,' ***',//,1X,A79,//,
     &       1X,'SIMPLE TERRAIN INPUTS:',/,
     &       1X,'   SOURCE TYPE                  =       VOLUME ',
     &                                           'w/ DRY DEPOSITION',/,
     &       1X,'   EMISSION RATE (G/S)          = ',G16.6,/,
     &       1X,'   SOURCE HEIGHT (M)            = ',F12.4,/,
     &       1X,'   INIT. LATERAL DIMEN (M)      = ',F12.4,/,
     &       1X,'   INIT. VERTICAL DIMEN (M)     = ',F12.4,/,
     &       1X,'   RECEPTOR HEIGHT (M)          = ',F12.4,/,
     &       1X,'   URBAN/RURAL OPTION           = ',7X,A5,/,
     &       1X,'   SURFACE ROUGHNESS LENGTH (M) = ',F12.4)
      IF (DPLETE) THEN
         WRITE(IOUT,113)
113      FORMAT(1X,'   PLUME DEPLETION OPTION       =           ON',/)
      ELSE
         WRITE(IOUT,114)
114      FORMAT(1X,'   PLUME DEPLETION OPTION       =          OFF',/)
      END IF
      WRITE(IOUT,109) PARDEN
109   FORMAT(1X,'   PARTICLE DENSITY (G/CM**3)   = ',F12.4)
      WRITE(IOUT,111) (PDIAM(I),PHI(I),I=1,NPD)
111   FORMAT(1X,'   PARTICLE DIAMETERS (MICRONS)      MASS FRACTIONS',/
     &         '    ----------------------------      --------------',/
     &      (10X,F12.4,13X,F12.4))

      RETURN
90    WRITE(IPRT,*) 'YOU HAVE ENTERED AN UNACCEPTABLE VALUE.  START',
     &              ' OVER.'
      GO TO 10
      END

c----------------------------------------------------------------------
      subroutine vdp1
c----------------------------------------------------------------------
c
c --- ISC2LT     Version:  1.0     Level:  930215                  VDP1
c                J. Scire, SRC
c
c --- PURPOSE:  Setup routine for PARTICLE dry deposition.
c               Completes particle common block /SOURC4/.  Performs
c               initialization and time-invariant calculations.
c
c --- INPUTS:
c     Common block /SOURC4/ variables:
c              INPD - integer    - Number of particle size categories
c            APDIAM - real array - Mean diameter (microns) of each
c                                  particle size category
c              APHI - real array - Mass fraction in each size category
c            APDENS - real       - Particle density (g/cm**3)
c
c --- OUTPUT:
c     Common block /SOURC4/ variables:
c               ASC - real array - Schmidt number
c            AVGRAV - real array - Gravitational settling velocity (m/s)
c            ATSTOP - real array - Stopping time (s)
c            VAIRMS - real       - Viscosity of air (m**2/s)
c             ZRDEP - real       - Reference height (m) for Deposition
c            VDPHOR - real       - Phoretic effects term (m/s)
c
c --- VDP1 called by:  SOCARD
c --- VDP1 calls:      none
c----------------------------------------------------------------------
c
      INCLUDE 'MAIN.INC'

      data a1/1.257/,a2/0.4/,a3/0.55/,xmfp/6.5e-6/
      data vcon/1.81e-4/,xk/1.38e-16/
      data vair/0.15/,gcgs/981./,rhoair/1.2e-3/,tair/293.15/
c
cxxx      IDBG=iounit
c ***
cxxx      if(DEBUG)then
cxxx         write(IDBG,*)
cxxx         write(IDBG,*)'SUBR. VDP1 -- INPUTS'
cxxx         write(IDBG,*)
cxxx         do 5 i=1,numsrc
cxxx         write(IDBG,*)'SOURCE          = ',i
cxxx         write(IDBG,*)'INPD            = ',inpd(i)
cxxx         write(IDBG,*)'APDIAM (um)     = ',(apdiam(n,i),n=1,inpd(i))
cxxx         write(IDBG,*)'APDIAM (um)     = ',(apdiam(n,i),n=1,inpd(i))
cxxx         write(IDBG,*)'APHI            = ',(aphi(n,i),n=1,inpd(i))
cxxx         write(IDBG,*)'APDENS(g/cm**3) = ',(apdens(n,i),n=1,inpd(i))
cxxx         write(IDBG,*)
cxxx5        continue
cxxx      endif
c ***
c
c --- Convert viscosity of air (at 20 deg C) from cm**2/s to m**2/s
      vairms=1.e-4*vair
c
c --- Set reference height for aerodynamic resistance calculation
      zrdep=1.0
c
c --- Define phoretic effects term (m/s)
      vdphor=0.0001
c
cxxxc --  LOOP over sources
cxxx      do 25 j=1,numsrc
c
cxxxc --- LOOP over "INPD" size intervals if non-zero
cxxx         if(inpd(j) .LE. 0) goto 25
cxxx         do 20 i=1,inpd(j)
         do 20 i=1,npd
c
c ---       Slip correction factor
            diamcm=1.e-4*pdiam(i)
            scf=1.+2.0*xmfp*(a1+a2*exp(-a3*diamcm/xmfp))/diamcm
c
c ---       Stokes friction coefficient
            sfc=3.*pi*vcon*diamcm/scf
c
c ---       Diffusivity (cm**2/s)
            diff=xk*tair/sfc
c ***
            if(DEBUG)then
               write(IDBG,*)'i = ',i,' diamcm = ',diamcm,' scf = ',scf,
     1         ' sfc = ',sfc,' diff = ',diff
            endif
c ***
c
c ---       Schmidt number
c ---       (vair = viscosity of air at 20 deg. c = 0.15 cm**2/s)
            sc(i)=vair/diff
c
c ---       Gravitational settling velocity (m/s)
c ---       (rhoair is approx. density of air -- 1.2e-3 g/cm**3)
            vgrav(i)=0.01*(pdens(i)-rhoair)*gcgs*diamcm**2
     1                     *scf/(18.*vcon)
c
c ---       Stopping times
            tstop(i)=vgrav(i)/(0.01*gcgs)
20       continue
cxxx25    continue
c ***
      if(DEBUG)then
         write(IDBG,*)
         write(IDBG,*)'SUBR. VDP1 -- Outputs'
         write(IDBG,*)
cxxx         do 30 i=1,numsrc
cxxx         write(IDBG,*)'SOURCE          = ',i
         write(IDBG,*)'ASC             = ',(sc(n),n=1,npd)
         write(IDBG,*)'AVGRAV (m/s)    = ',(vgrav(n),n=1,npd)
         write(IDBG,*)'ATSTOP (s)      = ',(tstop(n),n=1,npd)
         write(IDBG,*)'VAIRMS (m**2/s) = ',vairms
         write(IDBG,*)'ZRDEP (m)       = ',zrdep
         write(IDBG,*)'VDPHOR (m/s)    = ',vdphor
         write(IDBG,*)
cxxx30       continue
      endif
c ***
c
      return
      end

      SUBROUTINE LWRUPR(OPTG, ILEN)
C***********************************************************************
C                 LWRUPR Module of SCREEN2 Model
C
C        PURPOSE: Transfer All Characters From Lower Case To
C                 Upper Case (Using INDEX Intrinsic Function)
C
C        PROGRAMMER: Roger Brode, Kevin Stroupe
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Option String (80 Characters)
C
C        OUTPUTS: Option String in Uppercase
C
C        CALLED FROM:   INPUTP
C***********************************************************************
C
C     Variable Declarations
C      INCLUDE 'MAIN.INC'
      CHARACTER UPCASE*26
      CHARACTER LWCASE*26
      CHARACTER OPTG*80

C     Variable Initializations
      DATA UPCASE/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      DATA LWCASE/'abcdefghijklmnopqrstuvwxyz'/
CISC2      MODNAM = 'LWRUPR'

      DO 20 I = 1, ILEN
         IF (OPTG(I:I) .NE. ' ') THEN
            INDCHK = INDEX(LWCASE,OPTG(I:I))
            IF (INDCHK .NE. 0) THEN
               OPTG(I:I) = UPCASE(INDCHK:INDCHK)
            END IF
         END IF
 20   CONTINUE

      RETURN
      END

      SUBROUTINE CHOICE(IMET)
C
C     SUBROUTINE CHOICE SETS PARAMETERS TO CONTROL RANGE OF
C     METEOROLOGICAL CONDITIONS EXAMINED BASED ON USER SUPPLIED
C     CHOICE OF METEOROLOGY
C
C     INPUT:
C             IMET  - CHOICE OF METEOROLOGY:
C                     1 - FULL METEOROLOGY
C                     2 - INPUT SINGLE STABILITY CLASS
C                     3 - INPUT SINGLE STAB CLASS AND WIND SPEED
C             ZREF  - ANEMOMETER HEIGHT (M)
C
C     OUTPUT:
C             KMAX  - NUMBER OF STABILITY CLASSES TO EXAMINE
C             IST   - ARRAY OF STABILITY CLASSES TO EXAMINE
C             UINP  - USER SUPPLIED 10M WIND SPEED FOR IMET = 3
C             WSINP - LOGICAL VARIABLE FOR USER SUPPLIED WS
C
      INCLUDE 'MAIN.INC'
      REAL UINMAX(6)
      DATA UINMAX/3.,5.,10.,20.,5.,4./
C
      KSTINP = .FALSE.
      WSINP = .FALSE.
      UINP  = 1.0
C
      IF (IMET .EQ. 1) THEN
C***********************************************************************
C        FULL METEOROLOGY
C***********************************************************************
         WRITE(IDAT,*) IMET
         WRITE(IOUT,*) '*** FULL METEOROLOGY ***'
         KMAX = 6
         DO 20 I=1,KMAX
            IST(I) = I
20       CONTINUE
C
      ELSE IF (IMET .EQ. 2) THEN
C***********************************************************************
C        INPUT SINGLE STABILITY CLASS
C***********************************************************************
         WRITE(IDAT,*) IMET
         KMAX = 1
         KSTINP = .TRUE.
31       WRITE(IPRT,*) 'ENTER STABILITY CLASS, 1(=A) TO 6(=F):'
         READ(IRD,*,ERR=31) IST(1)
         IF (IST(1) .LT. 1 .OR. IST(1) .GT. 6) THEN
            WRITE(IPRT,*) 'NOT A VALID STABILITY CLASS! TRY AGAIN.'
            GO TO 31
         END IF
         WRITE(IDAT,*) IST(1)
         WRITE(IOUT,120) IST(1)
120      FORMAT(1X,'*** STABILITY CLASS ',I2,' ONLY ***')
C
      ELSE IF (IMET .EQ. 3) THEN
C***********************************************************************
C        INPUT SINGLE STABILITY CLASS AND 10-METER WIND SPEED
C***********************************************************************
         WRITE(IDAT,*) IMET
         KMAX = 1
         KSTINP = .TRUE.
         WSINP = .TRUE.
33       WRITE(IPRT,*) 'ENTER STABILITY CLASS, 1(=A) TO 6(=F):'
         READ(IRD,*,ERR=33) IST(1)
         IF (IST(1) .LT. 1 .OR. IST(1) .GT. 6) THEN
            WRITE(IPRT,*) 'NOT A VALID STABILITY CLASS!  TRY AGAIN.'
            GO TO 33
         END IF
         WRITE(IDAT,*) IST(1)
         WRITE(IOUT,125) IST(1)
125      FORMAT(1X,'*** STABILITY CLASS ',I2,' ONLY ***')
34       WRITE(IPRT,*) 'ENTER ANEMOMETER HEIGHT WIND SPEED (M/S):'
         READ(IRD,*,ERR=34) UINP
         IND = IST(1)
         IF (UINP .LT. 1.0 .OR. UINP .GT. UINMAX(IND)) THEN
            WRITE(IPRT,*) 'NOT A VALID WIND SPEED!  TRY AGAIN.'
            GO TO 34
         END IF
         WRITE(IDAT,*) UINP
         WRITE(IOUT,130) UINP
130      FORMAT(1X,'*** ANEMOMETER HEIGHT WIND SPEED OF ',F6.2,
     +             ' M/S ONLY ***')
C
      END IF

      IF (.NOT.LDEP .AND. AREA .AND. HS.LE.2.0 .AND. .NOT.KSTINP) THEN
C        Limit Stability Loop to Stable Classes for Low-level AREA Sources
         IF (RURAL) THEN
            KMAX = 2
            IST(1) = 5
            IST(2) = 6
         ELSE IF (URBAN) THEN
            KMAX = 1
            IST(1) = 6
         END IF
      END IF
C
      RETURN
      END

      SUBROUTINE AUTOX(IT,CMAXST,XMAXST,TMAXST,DMAXST,XMXDST,TMXDST)
C
C     SUBROUTINE AUTOX EXERCISES THE AUTOMATED DISTANCE OPTION TO
C     CALCULATE THE MAXIMUM GROUND LEVEL CONCENTRATION AS A FUNCTION
C     OF DISTANCE, AND TO DETERMINE THE OVERALL MAXIMUM CONCENTRATION
C
C     INPUT:
C            KMAX,IST,UINP,WSINP - PARAMETERS TO CONTROL THE RANGE OF
C                                  METEOROLOGY TO EXAMINE, SPECIFIED
C                                  BY SUBROUTINE CHOICE
C            FLAT    -  LOGICAL VARIABLE TO INDICATE FLAT OR ELEVATED
C                          TERRAIN
C
C     OUTPUT:
C            CMAXST  -  OVERALL MAXIMUM GROUND LEVEL CONCENTRATION
C            XMAXST  -  DOWNWIND DISTANCE ASSOCIATED WITH CMAXST
C            TMAXST  -  TERRAIN ELEVATION ABOVE STACK BASE ASSOCITATED
C                          WITH CMAXST AND XMAXST
C
      INCLUDE 'MAIN.INC'
      CONTIN = .FALSE.
C
14    CONTINUE
C
      KSTSAV = 0
      KSTSVD = 0

      IF (FLAT) THEN
         HTER = 0.0
      ELSE
15       WRITE(IPRT,*)'ENTER TERRAIN HEIGHT ABOVE STACK BASE (M):'
         READ(IRD,*,ERR=15) HTER
         IF (CONTIN .AND. HTER .LT. HTRLST) THEN
            WRITE(IPRT,*) 'NEW HEIGHT < PREVIOUS HEIGHT.  TRY AGAIN.'
            GO TO 15
         END IF
         IF (HTER .GT. HS) THEN
            WRITE(IPRT,*)'TERRAIN HEIGHT > STACK HEIGHT!'
            WRITE(IPRT,*)'  TERRAIN HEIGHT HAS BEEN SET = STACK',
     &                   ' HEIGHT.'
            WRITE(IPRT,*)'  USE COMPLEX TERRAIN SCREENING PROCEDURE'
            WRITE(IPRT,*)'  FOR TERRAIN ABOVE STACK HEIGHT.'
            HTER = HS
         END IF
         IF (HTER .LT. 0.0) THEN
            WRITE(IPRT,*)'TERRAIN HEIGHT < 0.0!  TRY AGAIN.'
            GO TO 15
         END IF
         WRITE(IDAT,*) HTER
      END IF
C
40    WRITE(IPRT,*) 'ENTER MIN AND MAX DISTANCES TO USE (M):'
      READ(IRD,*,ERR=40) XMIN,XMAX
      IF (XMIN .LT. 1.0) XMIN = 1.0
      IF (CONTIN .AND. XMIN .LT. XMXLST) THEN
         WRITE(IPRT,*) ' MIN DISTANCE < PREVIOUS MAX.  RANGES'
         WRITE(IPRT,*) ' SHOULD NOT OVERLAP.  TRY AGAIN. '
         GO TO 40
      END IF
      WRITE(IDAT,44) XMIN,XMAX
44    FORMAT(1X,F8.2,',',F8.2)
C
C        INITIALIZE -CNT VARIABLES ASSOCIATED WITH MAX FROM AUTO ARRAY,
C        AND STORE TERRAIN HT AND DISTANCE RANGE FOR LATER SUMMARY
C
      CHICNT = 0.
      XCNT   = 1.
      UCNT   = 0.
      USCNT  = 0.
      HECNT  = 0.
      ZICNT  = 0.
      KSTCNT = 0
      SYCNT  = 0.
      SZCNT  = 0.
      WDCNT  = 0.
      IFGCNT = 5
      DEPCNT = 0.
      XCNTD  = 1.
      UCNTD  = 0.
      USCNTD = 0.
      HECNTD = 0.
      ZICNTD = 0.
      KSTCTD = 0
      SYCNTD = 0.
      SZCNTD = 0.
      WDCNTD = 0.
      IFGCTD = 5
      IT = IT + 1
      HT(IT) = HTER
      RMIN(IT) = XMIN
      RMAX(IT) = XMAX
C
      WRITE(IPRT,200)
      WRITE(IOUT,200)
200   FORMAT(/1X,'**********************************',/,
     &        1X,'*** SCREEN AUTOMATED DISTANCES ***',/,
     &        1X,'**********************************',/)
      WRITE(IPRT,210) HTER
      WRITE(IOUT,210) HTER
210   FORMAT(1X,'*** TERRAIN HEIGHT OF ',F5.0,' M ABOVE STACK BASE',
     &          ' USED FOR FOLLOWING DISTANCES ***',/)
      IF (LDEP) THEN
         IF (HANE .EQ. 10) THEN
            WRITE(IPRT,317)
            WRITE(IOUT,317)
           ELSE
            WRITE(IPRT,318)
            WRITE(IOUT,318)
         END IF
       ELSE
        IF (AREA) THEN
         IF (HANE .EQ. 10) THEN
            WRITE(IPRT,319)
            WRITE(IOUT,319)
           ELSE
            WRITE(IPRT,320)
            WRITE(IOUT,320)
         END IF
       ELSE
        IF (HANE .EQ. 10.0) THEN
           WRITE(IPRT,300)
           WRITE(IOUT,300)
         ELSE
           WRITE(IPRT,301)
           WRITE(IOUT,301)
        END IF
       END IF
      END IF
300   FORMAT(3X,'DIST',5X,'CONC',13X,'U10M',3X,'USTK',2X,'MIX HT',
     &       3X,'PLUME',3X,'SIGMA',3X,'SIGMA',/,4X,'(M)',3X,
     &       '(UG/M**3)',3X,'STAB',2X,'(M/S)',2X,'(M/S)',4X,'(M)',3X,
     &       'HT (M)',3X,'Y (M)',3X,'Z (M)',2X,'DWASH',/,1X,
     &       '-------',2X,'----------',2X,'----',2X,'-----',2X,
     &       '-----',2X,'------',2X,'------',2X,'------',2X,
     &       '------',2X,'-----')
301   FORMAT(3X,'DIST',5X,'CONC',12X,'UHANE',3X,'USTK',2X,'MIX HT',
     &       3X,'PLUME',3X,'SIGMA',3X,'SIGMA',/,4X,'(M)',3X,
     &       '(UG/M**3)',3X,'STAB',2X,'(M/S)',2X,'(M/S)',4X,'(M)',3X,
     &       'HT (M)',3X,'Y (M)',3X,'Z (M)',2X,'DWASH',/,1X,
     &       '-------',2X,'----------',2X,'----',2X,'-----',2X,
     &       '-----',2X,'------',2X,'------',2X,'------',2X,
     &       '------',2X,'-----')
317      FORMAT(23X,'DEPOS AT ',28X,'CONC AT  ',/
     &          3X,'DIST',4X,'MAX CONC',4X,'MAX CONC ',7X,'U10M',
     &                    4X,'MAX DEPOS',4X,'MAX DEPOS',7X,'U10M',/
     &   4X,'(M)',3X,'(UG/M**3)',2X,'(G/M**2-HR)',1X,'STAB',1X,'(M/S)',
     &            3X,'(G/M**2-HR)',2X,'(UG/M**3)',2X,'STAB',1X,'(M/S)',
     &          /,1X,'-------',2X,'----------',2X,'----------',1X,
     &               '----',1X,'-----',4X,'----------',2X,'----------',
     &               1X,'----',1X,'-----')
318      FORMAT(23X,'DEPOS AT ',28X,'CONC AT  ',/
     &          3X,'DIST',4X,'MAX CONC',4X,'MAX CONC ',6X,'UHANE',
     &                    4X,'MAX DEPOS',4X,'MAX DEPOS',7X,'UHANE',/
     &   4X,'(M)',3X,'(UG/M**3)',2X,'(G/M**2-HR)',1X,'STAB',1X,'(M/S)',
     &            3X,'(G/M**2-HR)',2X,'(UG/M**3)',2X,'STAB',1X,'(M/S)',
     &          /,1X,'-------',2X,'----------',2X,'----------',1X,
     &               '----',1X,'-----',4X,'----------',2X,'----------',
     &               1X,'----',1X,'-----')
319      FORMAT(3X,'DIST',5X,'CONC',13X,'U10M',3X,'USTK',2X,'MIX HT',
     &          3X,'PLUME',2X,'MAX DIR',/,4X,'(M)',3X,
     &          '(UG/M**3)',3X,'STAB',2X,'(M/S)',2X,'(M/S)',4X,'(M)',3X,
     &          'HT (M)',3X,'(DEG)',/,1X,
     &          '-------',2X,'----------',2X,'----',2X,'-----',2X,
     &          '-----',2X,'------',2X,'------',2X,
     &          '-------')
320      FORMAT(3X,'DIST',5X,'CONC',12X,'UHANE',3X,'USTK',2X,'MIX HT',
     &          3X,'PLUME',2X,'MAX DIR',/,4X,'(M)',3X,
     &          '(UG/M**3)',3X,'STAB',2X,'(M/S)',2X,'(M/S)',4X,'(M)',3X,
     &          'HT (M)',3X,'(DEG)',/,1X,
     &          '-------',2X,'----------',2X,'----',2X,'-----',2X,
     &          '-----',2X,'------',2X,'------',2X,
     &          '-------')
C
C        LOOP THROUGH AUTOMATED DISTANCE ARRAY FROM XMIN OUT TO XMAX
C
      DO 10 IX = 1,51
         IF (IX.EQ.1) THEN
            X = AMAX1(XAUTO(1),XMIN)
         ELSE IF (XAUTO(IX).GT.XMIN.AND.XAUTO(IX).LE.XMAX) THEN
            X = XAUTO(IX)
         ELSE
            GO TO 10
         END IF
         CALL USERX
         IF (LDEP) THEN
            CALL MAXX(KSTMAX,UMAX)
            CALL MAXXD(KSTMXD,UMAXD)
            WRITE(IPRT,519) X,CHIMAX,DEPSEC,KSTMAX,UMAX,
     &                        DEPMAX,CONSEC,KSTMXD,UMAXD
            WRITE(IOUT,519) X,CHIMAX,DEPSEC,KSTMAX,UMAX,
     &                        DEPMAX,CONSEC,KSTMXD,UMAXD

         ELSE IF (AREA) THEN
            WRITE(IPRT,419) X,CHIMAX,KSTMAX,UMAX,USMAX,ZIMAX,HEMAX,
     &                      WDMAX
            WRITE(IOUT,419) X,CHIMAX,KSTMAX,UMAX,USMAX,ZIMAX,HEMAX,
     &                      WDMAX
         ELSE
            WRITE(IPRT,400) X,CHIMAX,KSTMAX,UMAX,USMAX,ZIMAX,HEMAX,
     &                      SYMAX,SZMAX,DWASH(IFGMAX)
            WRITE(IOUT,400) X,CHIMAX,KSTMAX,UMAX,USMAX,ZIMAX,HEMAX,
     &                      SYMAX,SZMAX,DWASH(IFGMAX)
CXXX            IF (LDEP) THEN
CXXX               WRITE(IPRT,400) X,DEPMAX,KSTMXD,UMAXD,USMAXD,ZIMAXD,
CXXX     &                         HEMAXD,SYMAXD,SZMAXD,DWASH(IFGMXD)
CXXX               WRITE(IOUT,400) X,DEPMAX,KSTMXD,UMAXD,USMAXD,ZIMAXD,
CXXX     &                         HEMAXD,SYMAXD,SZMAXD,DWASH(IFGMXD)
CXXX               WRITE(IOUT,*) ' '
CXXX            END IF
         END IF

400      FORMAT(1X,F7.0,2X,G11.4,4X,I1,4X,F4.1,3X,F4.1,1X,F7.1,
     &          3(1X,F7.2),4X,A2)
419      FORMAT(1X,F7.0,2X,G11.4,4X,I1,4X,F4.1,3X,F4.1,1X,F7.1,
     &          1X,F7.2,4X,F4.0)
519      FORMAT(1X,F7.0,2(2X,G11.4),3X,I1,3X,F4.1,2X,
     &                  2(2X,G11.4),3X,I1,3X,F4.1)
         IF (CHIMAX .GT. CHICNT) THEN
            CHICNT = CHIMAX
            XCNT   = X
            UCNT   = UMAX
            USCNT  = USMAX
            HECNT  = HEMAX
            ZICNT  = ZIMAX
            KSTCNT = KSTMAX
            SYCNT  = SYMAX
            SZCNT  = SZMAX
            WDCNT  = WDMAX
            IFGCNT = IFGMAX
         END IF
         IF (DEPMAX .GT. DEPCNT) THEN
            DEPCNT = DEPMAX
            XCNTD  = X
            UCNTD  = UMAXD
            USCNTD = USMAXD
            HECNTD = HEMAXD
            ZICNTD = ZIMAXD
            KSTCTD = KSTMXD
            SYCNTD = SYMAXD
            SZCNTD = SZMAXD
            WDCNTD = WDMAXD
            IFGCTD = IFGMXD
         END IF
10    CONTINUE
C
C        ITERATE TO FIND MAXIMUM CONCENTRATION BEYOND XMIN.
C        DO NOT LET CPEAK BE < CHICNT.
C
      WRITE(IPRT,*) 'ITERATING TO FIND MAXIMUM CONCENTRATION . . .'
      IF (AREA) THEN
         CALL TPMXA(XCNT,CPEAK,XPEAK,XMIN)
      ELSE
         CALL TPMX(XCNT,CPEAK,XPEAK,XMIN)
      END IF
      KPEAK = KSTMAX
      UPEAK = UMAX
      USPEAK = USMAX
      ZIPEAK = ZIMAX
      HEPEAK = HEMAX
      SYPEAK = SYMAX
      SZPEAK = SZMAX
      WRITE(IPRT,440) XMIN
      WRITE(IOUT,440) XMIN
440   FORMAT(/1X,'MAXIMUM 1-HR CONCENTRATION AT OR BEYOND ',F6.0,
     &        ' M:')
      IF (CHICNT .GE. CPEAK) THEN
         IF (LDEP) THEN
            CALL MAXX(KSTMAX,UMAX)
            WRITE(IPRT,517) XPEAK,CPEAK,DEPSEC,KPEAK,UPEAK
            WRITE(IOUT,517) XPEAK,CPEAK,DEPSEC,KPEAK,UPEAK
517         FORMAT(1X,F7.0,2(2X,G11.4),3X,I1,3X,F4.1,1X,
     &                     2(5X,'----',3X),3X,'--',4X,'---')
            WRITE(IPRT,*)
            WRITE(IPRT,*) 'ITERATING TO FIND MAXIMUM ',
     &                    'DRY DEPOSITION . . .'
            IF (AREA) THEN
               CALL TPMXAD(XCNTD,DPEAK,XPEAKD,XMIN)
            ELSE
               CALL TPMXD(XCNTD,DPEAK,XPEAKD,XMIN)
            END IF
            UPEAKD = UMAXD
            KPEAKD = KSTMXD
            CALL MAXXD(KSTMXD,UMAXD)
            WRITE(IPRT,445) XMIN
            WRITE(IOUT,445) XMIN
445         FORMAT(/1X,'MAXIMUM 1-HR DRY DEPOSITION AT OR BEYOND ',F6.0,
     &             ' M:')
            WRITE(IPRT,518) XPEAKD,DPEAK,CONSEC,KPEAKD,UPEAKD
            WRITE(IOUT,518) XPEAKD,DPEAK,CONSEC,KPEAKD,UPEAKD
518         FORMAT(1X,F7.0,2(4X,'----',4X),2X,'--',4X,'---',2X,
     &                     2(2X,G11.4),3X,I1,3X,F4.1)

         ELSE IF (AREA) THEN
            WRITE(IPRT,419) XCNT,CHICNT,KSTCNT,UCNT,USCNT,ZICNT,
     &            HECNT,WDCNT
            WRITE(IOUT,419) XCNT,CHICNT,KSTCNT,UCNT,USCNT,ZICNT,
     &            HECNT,WDCNT
         ELSE
            WRITE(IPRT,400) XCNT,CHICNT,KSTCNT,UCNT,USCNT,ZICNT,
     &            HECNT,SYCNT,SZCNT,DWASH(IFGCNT)
            WRITE(IOUT,400) XCNT,CHICNT,KSTCNT,UCNT,USCNT,ZICNT,
     &            HECNT,SYCNT,SZCNT,DWASH(IFGCNT)
         END IF
         IF (CHICNT .GT. CMAXST) THEN
            CMAXST = CHICNT
            XMAXST = XCNT
            TMAXST = HTER
         END IF
         IF (DEPCNT .GT. DMAXST) THEN
            DMAXST = DEPCNT
            XMXDST = XCNTD
            TMXDST = HTER
         END IF
      ELSE
         IF (LDEP) THEN
            CALL MAXX(KSTMAX,UMAX)
            WRITE(IPRT,517) XPEAK,CPEAK,DEPSEC,KPEAK,UPEAK
            WRITE(IOUT,517) XPEAK,CPEAK,DEPSEC,KPEAK,UPEAK
            WRITE(IPRT,*)
            WRITE(IPRT,*) 'ITERATING TO FIND MAXIMUM ',
     &                    'DRY DEPOSITION . . .'
            IF (AREA) THEN
               CALL TPMXAD(XCNTD,DPEAK,XPEAKD,XMIN)
            ELSE
               CALL TPMXD(XCNTD,DPEAK,XPEAKD,XMIN)
            END IF
            UPEAKD = UMAXD
            KPEAKD = KSTMXD
            CALL MAXXD(KSTMXD,UMAXD)
            WRITE(IPRT,445) XMIN
            WRITE(IOUT,445) XMIN
            WRITE(IPRT,518) XPEAKD,DPEAK,CONSEC,KPEAKD,UPEAKD
            WRITE(IOUT,518) XPEAKD,DPEAK,CONSEC,KPEAKD,UPEAKD

         ELSE IF (AREA) THEN
            WRITE(IPRT,419) XPEAK,CPEAK,KSTMAX,UPEAK,USPEAK,ZIPEAK,
     &            HEPEAK,WDMAX
            WRITE(IOUT,419) XPEAK,CPEAK,KSTMAX,UPEAK,USPEAK,ZIPEAK,
     &            HEPEAK,WDMAX
         ELSE
            WRITE(IPRT,400) XPEAK,CPEAK,KSTMAX,UPEAK,USPEAK,ZIPEAK,
     &            HEPEAK,SYPEAK,SZPEAK,DWASH(IFGMAX)
            WRITE(IOUT,400) XPEAK,CPEAK,KSTMAX,UPEAK,USPEAK,ZIPEAK,
     &            HEPEAK,SYPEAK,SZPEAK,DWASH(IFGMAX)
         END IF
         IF (CPEAK .GT. CMAXST) THEN
            CMAXST = CPEAK
            XMAXST = XPEAK
            TMAXST = HTER
         END IF
         IF (DPEAK .GT. DMAXST) THEN
            DMAXST = DPEAK
            XMXDST = XPEAKD
            TMXDST = HTER
         END IF
      END IF
      IF (.NOT.FLAT) THEN
         WRITE(IPRT,*) ' '
13       WRITE(IPRT,*) 'CONTINUE SIMPLE TERRAIN AUTOMATED CALCS',
     &                 ' WITH NEW TERRAIN HEIGHT?'
         WRITE(IPRT,*) 'ENTER Y OR N:'
         READ(IRD,100) QUERY
100      FORMAT(A1)
         IF (QUERY .EQ. 'Y' .OR. QUERY .EQ. 'y') THEN
            WRITE(IDAT,100) QUERY
            CONTIN = .TRUE.
            HTRLST = HTER
            XMXLST = XMAX
            GO TO 14
         ELSE IF (QUERY .EQ. 'N' .OR. QUERY .EQ. 'n') THEN
            WRITE(IDAT,100) QUERY
            WRITE(IPRT,*) ' '
            CONTINUE
         ELSE
            GO TO 13
         END IF
      END IF
C
      IF (.NOT.AREA .AND. .NOT.LDEP) THEN
         WRITE(IOUT,450)
450      FORMAT(/,1X,' DWASH=   MEANS NO CALC MADE (CONC = 0.0)',
     &          /,1X,' DWASH=NO MEANS NO BUILDING DOWNWASH USED',
     &          /,1X,' DWASH=HS MEANS HUBER-SNYDER DOWNWASH USED',
     &          /,1X,' DWASH=SS MEANS SCHULMAN-SCIRE DOWNWASH USED',
     &          /,1X,' DWASH=NA MEANS DOWNWASH NOT APPLICABLE, X<3*LB')
      END IF
C
      RETURN
      END

      SUBROUTINE DISCX(IT,CMAXST,XMAXST,TMAXST,DMAXST,XMXDST,TMXDST)
C
C     SUBROUTINE DISCX EXERCISES THE DISCRETE DISTANCE OPTION TO
C     CALCULATE THE MAXIMUM GROUND LEVEL CONCENTRATION AS A FUNCTION
C     OF USER-SPECIFIED DISTANCE.
C
C     INPUT:
C            KMAX,IST,UINP,WSINP - PARAMETERS TO CONTROL THE RANGE OF
C                                  METEOROLOGY TO EXAMINE, SPECIFIED
C                                  BY SUBROUTINE CHOICE
C            FLAT    -  LOGICAL VARIABLE TO INDICATE FLAT OR ELEVATED
C                          TERRAIN
C
C     OUTPUT:
C            CMAXST  -  OVERALL MAXIMUM GROUND LEVEL CONCENTRATION
C            XMAXST  -  DOWNWIND DISTANCE ASSOCIATED WITH CMAXST
C            TMAXST  -  TERRAIN ELEVATION ABOVE STACK BASE ASSOCITATED
C                          WITH CMAXST AND XMAXST
C
      INCLUDE 'MAIN.INC'
C
      WRITE(IPRT,*) 'TO CEASE, ENTER A DISTANCE OF ZERO (0).'
      WRITE(IPRT,500)
C
24    CONTINUE
C
      IF (FLAT) THEN
         HTER = 0.0
      ELSE
115      WRITE(IPRT,*)'ENTER TERRAIN HEIGHT ABOVE STACK BASE (M):'
	 READ(IRD,*,ERR=115) HTER
         IF (HTER .GT. HS) THEN
            WRITE(IPRT,*)'TERRAIN HEIGHT > STACK HEIGHT!'
            WRITE(IPRT,*)'  TERRAIN HEIGHT HAS BEEN SET = STACK',
     &                   ' HEIGHT.'
            WRITE(IPRT,*)'  USE COMPLEX TERRAIN SCREENING PROCEDURE'
            WRITE(IPRT,*)'  FOR TERRAIN ABOVE STACK HEIGHT.'
            HTER = HS
         END IF
         IF (HTER .LT. 0.0) THEN
            WRITE(IPRT,*)'TERRAIN HEIGHT < 0.0!  TRY AGAIN.'
            GO TO 115
         END IF
         WRITE(IDAT,*) HTER
      END IF
C
      WRITE(IOUT,500)
500   FORMAT(/1X,'*********************************',/,
     &        1X,'*** SCREEN DISCRETE DISTANCES ***',/,
     &        1X,'*********************************',/)
      WRITE(IPRT,210) HTER
      WRITE(IOUT,210) HTER
210   FORMAT(1X,'*** TERRAIN HEIGHT OF ',F5.0,' M ABOVE STACK BASE',
     &          ' USED FOR FOLLOWING DISTANCES ***',/)
      IF (LDEP) THEN
         IF (HANE .EQ. 10) THEN
            WRITE(IPRT,317)
            WRITE(IOUT,317)
           ELSE
            WRITE(IPRT,318)
            WRITE(IOUT,318)
         END IF
       ELSE
        IF (AREA) THEN
         IF (HANE .EQ. 10) THEN
            WRITE(IPRT,319)
            WRITE(IOUT,319)
           ELSE
            WRITE(IPRT,320)
            WRITE(IOUT,320)
         END IF
       ELSE
        IF (HANE .EQ. 10.0) THEN
           WRITE(IPRT,300)
           WRITE(IOUT,300)
         ELSE
           WRITE(IPRT,301)
           WRITE(IOUT,301)
        END IF
       END IF
      END IF
300   FORMAT(3X,'DIST',5X,'CONC',13X,'U10M',3X,'USTK',2X,'MIX HT',
     &       3X,'PLUME',3X,'SIGMA',3X,'SIGMA',/,4X,'(M)',3X,
     &       '(UG/M**3)',3X,'STAB',2X,'(M/S)',2X,'(M/S)',4X,'(M)',3X,
     &       'HT (M)',3X,'Y (M)',3X,'Z (M)',2X,'DWASH',/,1X,
     &       '-------',2X,'----------',2X,'----',2X,'-----',2X,
     &       '-----',2X,'------',2X,'------',2X,'------',2X,
     &       '------',2X,'-----')
301   FORMAT(3X,'DIST',5X,'CONC',12X,'UHANE',3X,'USTK',2X,'MIX HT',
     &       3X,'PLUME',3X,'SIGMA',3X,'SIGMA',/,4X,'(M)',3X,
     &       '(UG/M**3)',3X,'STAB',2X,'(M/S)',2X,'(M/S)',4X,'(M)',3X,
     &       'HT (M)',3X,'Y (M)',3X,'Z (M)',2X,'DWASH',/,1X,
     &       '-------',2X,'----------',2X,'----',2X,'-----',2X,
     &       '-----',2X,'------',2X,'------',2X,'------',2X,
     &       '------',2X,'-----')
317      FORMAT(23X,'DEPOS AT ',28X,'CONC AT  ',/
     &          3X,'DIST',4X,'MAX CONC',4X,'MAX CONC ',7X,'U10M',
     &                    4X,'MAX DEPOS',4X,'MAX DEPOS',7X,'U10M',/
     &   4X,'(M)',3X,'(UG/M**3)',2X,'(G/M**2-HR)',1X,'STAB',1X,'(M/S)',
     &            3X,'(G/M**2-HR)',2X,'(UG/M**3)',2X,'STAB',1X,'(M/S)',
     &          /,1X,'-------',2X,'----------',2X,'----------',1X,
     &               '----',1X,'-----',4X,'----------',2X,'----------',
     &               1X,'----',1X,'-----')
318      FORMAT(23X,'DEPOS AT ',28X,'CONC AT  ',/
     &          3X,'DIST',4X,'MAX CONC',4X,'MAX CONC ',6X,'UHANE',
     &                    4X,'MAX DEPOS',4X,'MAX DEPOS',7X,'UHANE',/
     &   4X,'(M)',3X,'(UG/M**3)',2X,'(G/M**2-HR)',1X,'STAB',1X,'(M/S)',
     &            3X,'(G/M**2-HR)',2X,'(UG/M**3)',2X,'STAB',1X,'(M/S)',
     &          /,1X,'-------',2X,'----------',2X,'----------',1X,
     &               '----',1X,'-----',4X,'----------',2X,'----------',
     &               1X,'----',1X,'-----')
319      FORMAT(3X,'DIST',5X,'CONC',13X,'U10M',3X,'USTK',2X,'MIX HT',
     &          3X,'PLUME',2X,'MAX DIR',/,4X,'(M)',3X,
     &          '(UG/M**3)',3X,'STAB',2X,'(M/S)',2X,'(M/S)',4X,'(M)',3X,
     &          'HT (M)',3X,'(DEG)',/,1X,
     &          '-------',2X,'----------',2X,'----',2X,'-----',2X,
     &          '-----',2X,'------',2X,'------',2X,
     &          '-------')
320      FORMAT(3X,'DIST',5X,'CONC',12X,'UHANE',3X,'USTK',2X,'MIX HT',
     &          3X,'PLUME',2X,'MAX DIR',/,4X,'(M)',3X,
     &          '(UG/M**3)',3X,'STAB',2X,'(M/S)',2X,'(M/S)',4X,'(M)',3X,
     &          'HT (M)',3X,'(DEG)',/,1X,
     &          '-------',2X,'----------',2X,'----',2X,'-----',2X,
     &          '-----',2X,'------',2X,'------',2X,
     &          '-------')

      N = 0
1     WRITE(IPRT,*) 'ENTER DISTANCE (M) (0 TO EXIT): '
      READ(IRD,*,ERR=1) X
      IF (X .GT. 100000.) THEN
         WRITE(IPRT,*) 'DISTANCE IS > 100 KM!  TRY AGAIN.'
         GO TO 1
      END IF
      WRITE(IDAT,*) X
C
      IF (X .GE. 1. .OR. (AREA .AND. X.GT.0.0)) THEN
         N = N + 1
         IT = IT + 1
         HT(IT) = HTER
         RMIN(IT) = X
         CALL USERX
         IF (N.EQ.8.OR.N.EQ.15.OR.N.EQ.22.OR.N.EQ.29) WRITE(IPRT,300)
         IF (LDEP) THEN
            CALL MAXX(KSTMAX,UMAX)
            CALL MAXXD(KSTMXD,UMAXD)
            WRITE(IPRT,519) X,CHIMAX,DEPSEC,KSTMAX,UMAX,
     &                        DEPMAX,CONSEC,KSTMXD,UMAXD
            WRITE(IOUT,519) X,CHIMAX,DEPSEC,KSTMAX,UMAX,
     &                        DEPMAX,CONSEC,KSTMXD,UMAXD

         ELSE IF (AREA) THEN
            WRITE(IPRT,419) X,CHIMAX,KSTMAX,UMAX,USMAX,ZIMAX,HEMAX,
     &                      WDMAX
            WRITE(IOUT,419) X,CHIMAX,KSTMAX,UMAX,USMAX,ZIMAX,HEMAX,
     &                      WDMAX
         ELSE
            WRITE(IPRT,400) X,CHIMAX,KSTMAX,UMAX,USMAX,ZIMAX,HEMAX,
     &                      SYMAX,SZMAX,DWASH(IFGMAX)
            WRITE(IOUT,400) X,CHIMAX,KSTMAX,UMAX,USMAX,ZIMAX,HEMAX,
     &                      SYMAX,SZMAX,DWASH(IFGMAX)
CXXX            IF (LDEP) THEN
CXXX               WRITE(IPRT,400) X,DEPMAX,KSTMXD,UMAXD,USMAXD,ZIMAXD,
CXXX     &                         HEMAXD,SYMAXD,SZMAXD,DWASH(IFGMXD)
CXXX               WRITE(IOUT,400) X,DEPMAX,KSTMXD,UMAXD,USMAXD,ZIMAXD,
CXXX     &                         HEMAXD,SYMAXD,SZMAXD,DWASH(IFGMXD)
CXXX               WRITE(IOUT,*) ' '
CXXX            END IF
         END IF

400      FORMAT(1X,F7.0,2X,G11.4,4X,I1,4X,F4.1,3X,F4.1,1X,F7.1,
     &          3(1X,F7.2),4X,A2)
419      FORMAT(1X,F7.0,2X,G11.4,4X,I1,4X,F4.1,3X,F4.1,1X,F7.1,
     &          1X,F7.2,4X,F4.0)
519      FORMAT(1X,F7.0,2(2X,G11.4),3X,I1,3X,F4.1,2X,
     &                  2(2X,G11.4),3X,I1,3X,F4.1)
         IF (CHIMAX .GT. CMAXST) THEN
            CMAXST = CHIMAX
            XMAXST = X
            TMAXST = HTER
         END IF
         IF (DEPMAX .GT. DMAXST) THEN
            DMAXST = DEPMAX
            XMXDST = X
            TMXDST = HTER
         END IF
         GO TO 1
      END IF
C
      IF (.NOT.FLAT) THEN
         WRITE(IPRT,*) ' '
23       WRITE(IPRT,*) 'CONTINUE SIMPLE TERRAIN DISCRETE CALCS',
     &                 ' WITH NEW TERRAIN HEIGHT?'
         WRITE(IPRT,*) 'ENTER Y OR N:'
	 READ(IRD,100) QUERY
100	 FORMAT(A1)
         IF (QUERY .EQ. 'Y' .OR. QUERY .EQ. 'y') THEN
            WRITE(IDAT,100) QUERY
            GO TO 24
         ELSE IF (QUERY .EQ. 'N' .OR. QUERY .EQ. 'n') THEN
            WRITE(IDAT,100) QUERY
            WRITE(IPRT,*) ' '
            CONTINUE
         ELSE
            GO TO 23
         END IF
      END IF
C
      IF (.NOT.AREA .AND. .NOT.LDEP) THEN
         WRITE(IOUT,450)
450      FORMAT(/,1X,' DWASH=   MEANS NO CALC MADE (CONC = 0.0)',
     &          /,1X,' DWASH=NO MEANS NO BUILDING DOWNWASH USED',
     &          /,1X,' DWASH=HS MEANS HUBER-SNYDER DOWNWASH USED',
     &          /,1X,' DWASH=SS MEANS SCHULMAN-SCIRE DOWNWASH USED',
     &          /,1X,' DWASH=NA MEANS DOWNWASH NOT APPLICABLE, X<3*LB')
      END IF
C
      RETURN
      END

      SUBROUTINE USERX
C
C        DESIGNED FOR ONE USER-SPECIFIED DISTANCE AT A TIME.
C        ROUTINE COMPUTES THE MAXIMUM CONC (AND DEPOS) FOR THAT DISTANCE
C        FROM A RANGE OF METEOROLOGICAL CONDITIONS.
C
C        INPUTS:
C           FB    BUOYANCY FLUX PARAMETER (M**4/S**3)
C           FM    MOMENTUM FLUX PARAMETER (M**4/S**2)
C           Q     EMISSION RATE (G/S)
C           HS    STACK HT (M)
C           DS    STACK DIAMETER (M)
C           VS    EXIT VELOCITY (M/S)
C           TS    STACK TEMPERATURE (K)
C           TA    AMBIENT TEMPERATURE (K)
C           HB    BLDG HT (M)
C           HL    BLDG LENGTH (MIN HORIZ DIMENSION) (M)
C           HW    BLDG WIDTH (MAX HORIZ DIMENSION) (M)
C           ZR    RECEPTOR HT ABOVE GROUND (M)
C           IOPT  URBAN/RURAL OPTION (U = URBAN, R = RURAL)
C           X     DOWNWIND DISTANCE (M)
C           WSINP WIND SPEED INPUT OPTION FLAG (T OR F)
C           UINP  WIND SPEED INPUT BY USER IF WSINP = .TRUE.
C
C        ROUTINES USED:
C           DELH    COMPUTES FINAL PLUME RISE FOR NO DOWNWASH SCENARIO
C           SIGY    COMPUTES RURAL OR URBAN SIGMA-Y
C           SIGZ    COMPUTES RURAL OR URBAN SIGMA-Z
C           SYSS    COMPUTES SIGMA-Y DURING DOWNWASH SCENARIOS
C           SZSS    COMPUTES SIGMA-Z DURING DOWNWASH SCENARIOS
C           HM      COMPUTES MOMENTUM PLUME RISE
C           DHHS    COMPUTES PLUME RISE FOR HUBER-SNYDER SCENARIO
C           DHSS    COMPUTES PLUME RISE FOR SCHULMAN-SCIRE SCENARIO
C           CONC    COMPUTES GAUSSIAN PLUME GROUND-LEVEL CONCENTRATION
C           HSPRM   COMPUTES STACK HEIGHT WITH STACK TIP DOWNWASH
C
C        OUTPUTS
C          (ALL ASSOCIATED WITH MAX CONC):
C           CHIMAX  MAXIMUM CONC (UG/M**3)
C           UMAX    10M WIND SPEED (M/S)
C           USMAX   STACK TOP WIND SPEED (M/S)
C           HEMAX   EFFECTIVE PLUME HT (M)
C           ZIMAX   MIXING HEIGHT (M)
C           KSTMAX  STABILITY CLASS
C           SYMAX   HORIZONTAL DISPERSION (M)
C           SZMAX   VERTICAL DISPERSION (M)
C           IFGMAX  FLAG TO IDENTIFY DOWNWASH SCENARIO
C          (ALL ASSOCIATED WITH MAX DEPOS):
C           DEPMAX  MAXIMUM DEPOS (G/(M**2-HR))
C           UMAXD   10M WIND SPEED (M/S)
C           USMAXD  STACK TOP WIND SPEED (M/S)
C           HEMAXD  EFFECTIVE PLUME HT (M)
C           ZIMAXD  MIXING HEIGHT (M)
C           KSTMXD  STABILITY CLASS
C           SYMAXD  HORIZONTAL DISPERSION (M)
C           SZMAXD  VERTICAL DISPERSION (M)
C           IFGMXD  FLAG TO IDENTIFY DOWNWASH SCENARIO
C
      INCLUDE 'MAIN.INC'
      REAL    U(13), PURBAN(6), PRURAL(6), ADTDZ(6)
      INTEGER IUHI(6)
      DATA    U /1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0,8.,10.,15.,20./
      DATA    PURBAN /0.15,0.15,0.20,0.25,0.30,0.30/
      DATA    PRURAL /0.07,0.07,0.10,0.15,0.35,0.55/
      DATA    IUHI /5,9,11,13,9,7/
      DATA    ADTDZ /0.0, 0.0, 0.0, 0.0, 0.02, 0.035/
C
C        INITIALIZATIONS
C
      IOUNIT = IDBG
      WAKE   = .FALSE.
      WAKESS = .FALSE.
      Y = 0.0
      ZLB  = AMIN1(HB,HWP)
      CHIMAX = -999.
      UMAX   = 0.
      USMAX  = 0.
      HEMAX  = 0.
      ZIMAX  = 0.
      KSTMAX = 0
      SYMAX  = 0.
      SZMAX  = 0.
      WDMAX  = 0.
      IFGMAX = 5
      DEPMAX = -999.
      UMAXD  = 0.
      USMAXD = 0.
      HEMAXD = 0.
      ZIMAXD = 0.
      KSTMXD = 0
      SYMAXD = 0.
      SZMAXD = 0.
      WDMAXD = 0.
      IFGMXD = 5
C
C        LOOP ON METEOROLOGICAL CONDITIONS
C
C     Begin LOOP on Stability Classes
      DO 10 K = 1, KMAX
         KST = IST(K)
C        Optimize by skipping KST <= controlling KST for previous distance
C        assuming the distances are increasing (DX >= 0) for AREAs and VOLUMEs.
         IF (.NOT.DISC .AND. (AREA.OR.VOLUME)) THEN
            IF (.NOT.LDEP .AND. DX.GE.0.0 .AND. KST.LT.KSTSAV) THEN
C              Skip to next stability class
               GO TO 10
            ELSE IF (LDEP .AND. DX.GE.0.0 .AND. KST.LT.KSTSAV .AND.
     &                                          KST.LT.KSTSVD) THEN
C              Skip to next stability class
               GO TO 10
            END IF
         END IF
         IF (WSINP .OR. (.NOT.LDEP .AND. (AREA .OR. VOLUME)) ) THEN
C           Limit Search for Area and Volume Sources to Single Wind Speed (1m/s)
            IMAX = 1
         ELSE
            IMAX = IUHI(KST)
         END IF
         UNSTAB = .FALSE.
         NEUTRL = .FALSE.
         STABLE = .FALSE.
         IF (KST .LT. 4) THEN
            UNSTAB = .TRUE.
         ELSE IF (KST .EQ. 4) THEN
            NEUTRL = .TRUE.
         ELSE IF (KST .GT. 4) THEN
            STABLE = .TRUE.
         END IF

         DTDZ = ADTDZ(KST)
         IF (DTDZ .GT. 0.0 .AND. TA .NE. 0.0) THEN
            S = G*DTDZ/TA
            RTOFS = SQRT(S)
         ELSE
            S = 1.0E-10
            RTOFS = 1.0E-10
         END IF

C        Begin LOOP on Wind Speeds
         DO 20 IU = 1,IMAX
C           Reset value of KST
            KST = IST(K)
            IF (WSINP .OR. (.NOT.LDEP .AND. (AREA .OR. VOLUME)) ) THEN
               UREF = UINP
            ELSE
               UREF = U(IU)
            END IF
C
            IF (X .GT. 50000. .AND. UREF .LT. 2.0) THEN
               UREF = 2.0
            END IF
C
C
C           ADJUST WIND SPEED FROM REFERENCE (ANEMOMETER) HEIGHT, ZREF,
C           TO STACK HEIGHT
C
            IF (RURAL) THEN
               P = PRURAL(KST)
            ELSE IF (URBAN) THEN
               P = PURBAN(KST)
            END IF
            CALL WSADJ

C           Calculate Monin-Obukhov Length, Friction Velocity, and
C           Deposition Velocities for this Source
            IF (LDEP) THEN
               CALL OBUKHOV
               CALL U_STAR
               CALL VDP
            ENDIF

            IF (POINT .OR. FLARE) THEN
C              Calculate Distance to Final Rise
               CALL DISTF
C
C        DETERMINE TYPE OF DOWNWASH SCENARIO, IF ANY
C
               DSBH = HB
               DSBW = HWP
               WAKE   = .FALSE.
               WAKESS = .FALSE.
C              Determine Wake Flags
               CALL WAKFLG
               FSTREC = .TRUE.
               NOBID  = .FALSE.
               NOSTD  = .FALSE.
               GRDRIS = .FALSE.
C              Calculate Effective Plume Height
               CALL PHEFF
C              Set HEFLAT = HE to avoid plume above ZI in PCHI
               HEFLAT = HE
C              Calculate Dispersion Parameters
               CALL PDIS
               IF (.NOT. WAKE) THEN
                  IFLG = 1
               ELSE IF (WAKE .AND. X .LT. 3.*ZLB) THEN
                  HRVAL = 0.0
                  UREF  = 0.0
                  US    = 0.0
                  HE    = 0.0
                  ZI    = 0.0
                  KST   = 0
                  SY    = 0.0
                  SZ    = 0.0
                  IFLG  = 4
                  IF (HRVAL .GT. CHIMAX) THEN
                     CHIMAX = HRVAL
                     UMAX   = UREF
                     USMAX  = US
                     HEMAX  = HE
                     ZIMAX  = ZI
                     KSTMAX = KST
                     SYMAX  = SY
                     SZMAX  = SZ
                     IFGMAX = IFLG
                  END IF
                  IF (LDEP) THEN
                     IF (HRVAL .GT. DEPMAX) THEN
                        DEPMAX = HRVAL
                        UMAXD  = UREF
                        USMAXD = US
                        HEMAXD = HE
                        ZIMAXD = ZI
                        KSTMXD = KST
                        SYMAXD = SY
                        SZMAXD = SZ
                        IFGMXD = IFLG
                     END IF
                  END IF
                  GO TO 100
               ELSE IF (WAKE .AND. WAKESS) THEN
                  IFLG = 2
               ELSE IF (WAKE) THEN
                  IFLG = 3
               END IF

            ELSE IF (VOLUME) THEN
C              Calculate Effective Radius
               XRAD = 2.15*SYINIT
C              Initialize SBID to 0.0 for call to DEPCOR
               SBID = 0.0
               IF ((X-XRAD) .LT. 0.99) THEN
C                 Receptor Upwind of Downwind Edge
                  HRVAL = 0.0
                  UREF  = 0.0
                  US    = 0.0
                  HE    = 0.0
                  ZI    = 0.0
                  KST   = 0
                  SY    = 0.0
                  SZ    = 0.0
                  IFLG  = 5
                  IF (HRVAL .GT. CHIMAX) THEN
                     CHIMAX = HRVAL
                     UMAX   = UREF
                     USMAX  = US
                     HEMAX  = HE
                     ZIMAX  = ZI
                     KSTMAX = KST
                     SYMAX  = SY
                     SZMAX  = SZ
                     IFGMAX = IFLG
                  END IF
                  IF (LDEP) THEN
                     IF (HRVAL .GT. DEPMAX) THEN
                        DEPMAX = HRVAL
                        UMAXD  = UREF
                        USMAXD = US
                        HEMAXD = HE
                        ZIMAXD = ZI
                        KSTMXD = KST
                        SYMAXD = SY
                        SZMAXD = SZ
                        IFGMXD = IFLG
                     END IF
                  END IF
                  GO TO 100
               ELSE
C                 Calculate Effective Plume Height
                  CALL VHEFF
C                 Set HEFLAT = HE to avoid plume above ZI in PCHI
                  HEFLAT = HE
C                 Calculate Dispersion Parameters
                  CALL VDIS
                  IFLG = 1
               END IF

            ELSE IF (AREA) THEN
C              Set Effective Source Height
               HE = HS
C              Initialize XZ, XY, and SBID for call to DEPCOR
               XY = 0.0
               XZ = 0.0
               SBID = 0.0
               IFLG = 1
            END IF
C
C        THE MINIMUM MIXING HEIGHT DUE TO MECHANICAL MIXING IS
C        FOUND BY SETTING ZI EQUAL TO 0.3 * USTAR / F WHERE F IS
C        THE CORIOLIS PARAMETER.  FOR THIS ALGORITHM, USTAR IS
C        ASSUMED TO BE EQUAL TO 0.1 * U10M.  TO BE CONSERVATIVE, IF
C        THIS ZI IS BELOW HE (NO CONTRIBUTION CASE), THEN ZI IS SET
C        EQUAL TO HE + 1 IN ORDER TO SET UP MAXIMUM REFLECTION.
C
            ZI = 320. * UREF
            IF (ZI .GT. 10000.) ZI = 10000.
            IF (ZI .LT. HE+1.)  ZI = HE + 1.
C  FROM R. BRODE 1991 AMS CONFERENCE PREPRINT.  ADJUSTS MIXING HEIGHTS SO
C   CALCULATED CONCENTRATIONS ARE MORE CONSERVATIVE WITH RESPECT TO ISCST2
C   RESULTS.
         U10 = UREF * (10.0/HANE)**P
           IF (KST .LE. 4 .AND. ICI .EQ. 1) THEN
             ZI = MAX(ZIMIN(KST), (HE *(1.0 + ZIFACT(KST) * U10)))
           END IF

C
C        MIXING HTS ARE NOT USED IN COMPUTING CONCENTRATIONS
C        DURING STABLE CONDITIONS.  SET TO 10000 M FOR E AND F.
C
            IF (KST .GT. 4 ) ZI = 10000.
            IF (HE .LT. 1.0E-10) HE = 0.0
            QTK = Q * 1.0E06
            ZFLAG = ZR

            IF (POINT .OR. FLARE .OR. VOLUME) THEN
C              Determine deposition correction factors   ---   CALL DEPCOR
               IF (LDEP) THEN
C                 Loop over particle sizes
                  DO 150 I = 1, NPD
                     IF (DPLETE) THEN
                        CALL DEPCOR(VDEP(I),VGRAV(I),ZRDEP,ZFLAG,X,
     &                       XZ,HE,ZI,US,RURAL,URBAN,KST,SZ,SBID,
     &                       DEBUG,IOUNIT,QCOR(I),PCORZR(I),
     &                       PCORZD(I))
                     ELSE
                        QCOR(I) = 1.
                        PCORZR(I) = 1.
                        PCORZD(I) = 1.
                     END IF
150               CONTINUE
               END IF

               CONC  = .TRUE.
               DEPOS = .FALSE.
               QTK = Q * 1.0E06
               CALL PCHI
               IF (HRVAL .GT. CHIMAX) THEN
                  CHIMAX = HRVAL
                  UMAX   = UREF
                  USMAX  = US
                  HEMAX  = HE
                  ZIMAX  = ZI
                  KSTMAX = KST
                  SYMAX  = SY
                  SZMAX  = SZ
                  IFGMAX = IFLG
               END IF
               IF (LDEP) THEN
                  CONC  = .FALSE.
                  DEPOS = .TRUE.
                  QTK = Q * 3600.
                  CALL PDEP
                  IF (HRVAL .GT. DEPMAX) THEN
                     DEPMAX = HRVAL
                     UMAXD  = UREF
                     USMAXD = US
                     HEMAXD = HE
                     ZIMAXD = ZI
                     KSTMXD = KST
                     SYMAXD = SY
                     SZMAXD = SZ
                     IFGMXD = IFLG
                  END IF
               END IF

            ELSE IF (AREA) THEN
               IF (MAXWD .AND. IU.EQ.1) THEN
C                 Option to search for Maximum Wind Direction selected.
C                 Calculate normalized distance, XNORM, and get range
C                 of wind directions to search from SUB. FINDMX.
                  XNORM = X/(0.5*XINIT)
                  CALL FINDMX(XNORM)
C                 Find direction to maximum concentration from arrays
                  DO 919 IWD = MINDIR, MAXDIR
C                    Loop through range of degrees from MINDIR to MAXDIR
                     ANGRAD = -1.0 * IWD * DTORAD
                     AXVERT(1) =  (0.5*YINIT*SIN(ANGRAD)-
     &                             0.5*XINIT*COS(ANGRAD))/1000.
                     AYVERT(1) = (-0.5*YINIT*COS(ANGRAD)-
     &                             0.5*XINIT*SIN(ANGRAD))/1000.
                     AXVERT(2) = (-0.5*YINIT*SIN(ANGRAD)-
     &                             0.5*XINIT*COS(ANGRAD))/1000.
                     AYVERT(2) =  (0.5*YINIT*COS(ANGRAD)-
     &                             0.5*XINIT*SIN(ANGRAD))/1000.
                     AXVERT(3) = (-0.5*YINIT*SIN(ANGRAD)+
     &                             0.5*XINIT*COS(ANGRAD))/1000.
                     AYVERT(3) =  (0.5*YINIT*COS(ANGRAD)+
     &                             0.5*XINIT*SIN(ANGRAD))/1000.
                     AXVERT(4) =  (0.5*YINIT*SIN(ANGRAD)+
     &                             0.5*XINIT*COS(ANGRAD))/1000.
                     AYVERT(4) = (-0.5*YINIT*COS(ANGRAD)+
     &                             0.5*XINIT*SIN(ANGRAD))/1000.
                     AXVERT(5) = AXVERT(1)
                     AYVERT(5) = AYVERT(1)

C                    Determine Coordinates of Vertices in WDIR Coord. System
                     CALL AVERTS
                     CONC  = .TRUE.
                     DEPOS = .FALSE.
                     QTK = Q * 1.0E06
C                    Calculate Area Source Integral
                     CALL AREAIN
                     IF (HRVAL .GT. CHIMAX) THEN
                        CHIMAX = HRVAL
                        UMAX   = UREF
                        USMAX  = US
                        HEMAX  = HE
                        ZIMAX  = ZI
                        KSTMAX = KST
                        SYMAX  = SY
                        SZMAX  = SZ
                        IFGMAX = IFLG
                        WDMAX  = IWD
                     END IF
919               CONTINUE
                  IWD = WDMAX
                  ANGRAD = -1.0 * IWD * DTORAD
                  AXVERT(1) =  (0.5*YINIT*SIN(ANGRAD)-
     &                          0.5*XINIT*COS(ANGRAD))/1000.
                  AYVERT(1) = (-0.5*YINIT*COS(ANGRAD)-
     &                          0.5*XINIT*SIN(ANGRAD))/1000.
                  AXVERT(2) = (-0.5*YINIT*SIN(ANGRAD)-
     &                          0.5*XINIT*COS(ANGRAD))/1000.
                  AYVERT(2) =  (0.5*YINIT*COS(ANGRAD)-
     &                          0.5*XINIT*SIN(ANGRAD))/1000.
                  AXVERT(3) = (-0.5*YINIT*SIN(ANGRAD)+
     &                          0.5*XINIT*COS(ANGRAD))/1000.
                  AYVERT(3) =  (0.5*YINIT*COS(ANGRAD)+
     &                          0.5*XINIT*SIN(ANGRAD))/1000.
                  AXVERT(4) =  (0.5*YINIT*SIN(ANGRAD)+
     &                          0.5*XINIT*COS(ANGRAD))/1000.
                  AYVERT(4) = (-0.5*YINIT*COS(ANGRAD)+
     &                          0.5*XINIT*SIN(ANGRAD))/1000.
                  AXVERT(5) = AXVERT(1)
                  AYVERT(5) = AYVERT(1)

C                 Determine Coordinates of Vertices in WDIR Coord. System
                  CALL AVERTS
                  IF (LDEP) THEN
                     CONC  = .FALSE.
                     DEPOS = .TRUE.
                     QTK = Q * 3600.
                     CALL AREAIN
                     IF (HRVAL .GT. DEPMAX) THEN
                        DEPMAX = HRVAL
                        UMAXD  = UREF
                        USMAXD = US
                        HEMAXD = HE
                        ZIMAXD = ZI
                        KSTMXD = KST
                        SYMAXD = SY
                        SZMAXD = SZ
                        IFGMXD = IFLG
                        WDMAXD = IWD
                     END IF
                  END IF
c                  if (wdmax .ne. wdmaxd) then
c                     write(iprt,927) WDMAX, WDMAXD
c927                  format(1x,' WDMAX = ',f4.0,';     WDMAXD = ',f4.0)
c                     write(iout,927) WDMAX, WDMAXD
c                  end if
               ELSE IF (MAXWD) THEN
                  IF (LDEP) THEN
                     IWD = WDMAX
                     ANGRAD = -1.0 * IWD * DTORAD
                     AXVERT(1) =  (0.5*YINIT*SIN(ANGRAD)-
     &                             0.5*XINIT*COS(ANGRAD))/1000.
                     AYVERT(1) = (-0.5*YINIT*COS(ANGRAD)-
     &                             0.5*XINIT*SIN(ANGRAD))/1000.
                     AXVERT(2) = (-0.5*YINIT*SIN(ANGRAD)-
     &                             0.5*XINIT*COS(ANGRAD))/1000.
                     AYVERT(2) =  (0.5*YINIT*COS(ANGRAD)-
     &                             0.5*XINIT*SIN(ANGRAD))/1000.
                     AXVERT(3) = (-0.5*YINIT*SIN(ANGRAD)+
     &                             0.5*XINIT*COS(ANGRAD))/1000.
                     AYVERT(3) =  (0.5*YINIT*COS(ANGRAD)+
     &                             0.5*XINIT*SIN(ANGRAD))/1000.
                     AXVERT(4) =  (0.5*YINIT*SIN(ANGRAD)+
     &                             0.5*XINIT*COS(ANGRAD))/1000.
                     AYVERT(4) = (-0.5*YINIT*COS(ANGRAD)+
     &                             0.5*XINIT*SIN(ANGRAD))/1000.
                     AXVERT(5) = AXVERT(1)
                     AYVERT(5) = AYVERT(1)

C                    Determine Coordinates of Vertices in WDIR Coord. System
                     CALL AVERTS
                     CONC  = .FALSE.
                     DEPOS = .TRUE.
                     QTK = Q * 3600.
                     CALL AREAIN
                     IF (HRVAL .GT. DEPMAX) THEN
                        DEPMAX = HRVAL
                        UMAXD  = UREF
                        USMAXD = US
                        HEMAXD = HE
                        ZIMAXD = ZI
                        KSTMXD = KST
                        SYMAXD = SY
                        SZMAXD = SZ
                        IFGMXD = IFLG
C                        WDMAXD = IWD
                     END IF
                  END IF
               ELSE
C                 Option to specify wind direction orientation (ANGLE) selected.
                  IF (WSINP .OR. UREF.EQ.1.0) THEN
C                    Determine Coordinates of Vertices in WDIR Coord. System
                     CALL AVERTS
C                    Calculate Area Source Integral
                     CONC  = .TRUE.
                     DEPOS = .FALSE.
                     QTK = Q * 1.0E06
                     CALL AREAIN
                     WDMAX = ANGLE
                     IF (HRVAL .GT. CHIMAX) THEN
                        CHIMAX = HRVAL
                        UMAX   = UREF
                        USMAX  = US
                        HEMAX  = HE
                        ZIMAX  = ZI
                        KSTMAX = KST
                        SYMAX  = SY
                        SZMAX  = SZ
                        IFGMAX = IFLG
C                        WDMAX  = IWD
                     END IF
                  END IF
                  IF (LDEP) THEN
                     CONC  = .FALSE.
                     DEPOS = .TRUE.
                     QTK = Q * 3600.
                     CALL AREAIN
                     IF (HRVAL .GT. DEPMAX) THEN
                        DEPMAX = HRVAL
                        UMAXD  = UREF
                        USMAXD = US
                        HEMAXD = HE
                        ZIMAXD = ZI
                        KSTMXD = KST
                        SYMAXD = SY
                        SZMAXD = SZ
                        IFGMXD = IFLG
C                        WDMAXD = IWD
                     END IF
                  END IF
               END IF
C               IF (HRVAL .GT. CHIMAX) THEN
C                  CHIMAX = HRVAL
C                  UMAX   = UREF
C                  USMAX  = US
C                  HEMAX  = HE
C                  ZIMAX  = ZI
C                  KSTMAX = KST
C                  SYMAX  = SY
C                  SZMAX  = SZ
C                  IFGMAX = IFLG
C               END IF
            END IF

 100        CONTINUE

20       CONTINUE
C        End LOOP on Wind Speeds

10    CONTINUE
C     End LOOP on Stability Classes

C     Save controlling stability class(es) for this distance
      KSTSAV = KSTMAX
      KSTSVD = KSTMXD

      RETURN
      END

      SUBROUTINE MAXX(KSTIN,UREFIN)
C
C        CALCULATES DEPOSITION (DEPSEC) FOR MET CONDITIONS ASSOCIATED
C        WITH THE MAXIMUM CONCENTRATION VALUE AT A GIVEN DISTANCE.
C
C        INPUTS:
C           KST   STABILITY CLASS FOR MAX DEPOSITION
C           UREF  10-METER WIND SPEED FOR MAX DEPOSITION
C
C        ROUTINES USED:
C           DELH    COMPUTES FINAL PLUME RISE FOR NO DOWNWASH SCENARIO
C           SIGY    COMPUTES RURAL OR URBAN SIGMA-Y
C           SIGZ    COMPUTES RURAL OR URBAN SIGMA-Z
C           SYSS    COMPUTES SIGMA-Y DURING DOWNWASH SCENARIOS
C           SZSS    COMPUTES SIGMA-Z DURING DOWNWASH SCENARIOS
C           HM      COMPUTES MOMENTUM PLUME RISE
C           DHHS    COMPUTES PLUME RISE FOR HUBER-SNYDER SCENARIO
C           DHSS    COMPUTES PLUME RISE FOR SCHULMAN-SCIRE SCENARIO
C           CONC    COMPUTES GAUSSIAN PLUME GROUND-LEVEL CONCENTRATION
C           HSPRM   COMPUTES STACK HEIGHT WITH STACK TIP DOWNWASH
C
C        OUTPUT:
C           DEPSEC  SECONDARY DEPOSITION (UG/M**3)
C
      INCLUDE 'MAIN.INC'
      REAL    PURBAN(6), PRURAL(6), ADTDZ(6)
      DATA    PURBAN /0.15,0.15,0.20,0.25,0.30,0.30/
      DATA    PRURAL /0.07,0.07,0.10,0.15,0.35,0.55/
      DATA    ADTDZ /0.0, 0.0, 0.0, 0.0, 0.02, 0.035/
C
C        INITIALIZATIONS
C
      KST = KSTIN
      UREF = UREFIN
      IF (KST .EQ. 0) RETURN
      WAKE   = .FALSE.
      WAKESS = .FALSE.
      Y = 0.0
      ZLB  = AMIN1(HB,HWP)
C
      UNSTAB = .FALSE.
      NEUTRL = .FALSE.
      STABLE = .FALSE.
      IF (KST .LT. 4) THEN
         UNSTAB = .TRUE.
      ELSE IF (KST .EQ. 4) THEN
         NEUTRL = .TRUE.
      ELSE IF (KST .GT. 4) THEN
         STABLE = .TRUE.
      END IF

      DTDZ = ADTDZ(KST)
      IF (DTDZ .GT. 0.0 .AND. TA .NE. 0.0) THEN
         S = G*DTDZ/TA
         RTOFS = SQRT(S)
      ELSE
         S = 1.0E-10
         RTOFS = 1.0E-10
      END IF

C
      IF (X .GT. 50000. .AND. UREF .LT. 2.0) THEN
         UREF = 2.0
      END IF
C
C
C     ADJUST WIND SPEED FROM REFERENCE (ANEMOMETER) HEIGHT, ZREF,
C     OF 10-METERS, TO STACK HEIGHT
C
      IF (RURAL) THEN
         P = PRURAL(KST)
      ELSE IF (URBAN) THEN
         P = PURBAN(KST)
      END IF
      CALL WSADJ

C     Calculate Deposition Velocities for this Source
      IF (LDEP) THEN
         CALL OBUKHOV
         CALL U_STAR
         CALL VDP
      ENDIF

      IF (POINT .OR. FLARE) THEN
C        Calculate Distance to Final Rise
         CALL DISTF
C
C     DETERMINE TYPE OF DOWNWASH SCENARIO, IF ANY
C
         DSBH = HB
         DSBW = HWP
         WAKE   = .FALSE.
         WAKESS = .FALSE.
C        Determine Wake Flags
         CALL WAKFLG
         FSTREC = .TRUE.
         NOBID  = .FALSE.
         NOSTD  = .FALSE.
         GRDRIS = .FALSE.
C        Calculate Effective Plume Height
         CALL PHEFF
C        Set HEFLAT = HE to avoid plume above ZI in PCHI
         HEFLAT = HE
C        Calculate Dispersion Parameters
         CALL PDIS
         IF (.NOT. WAKE) THEN
            IFLG = 1
         ELSE IF (WAKE .AND. X .LT. 3.*ZLB) THEN
            HRVAL = 0.0
            UREF  = 0.0
            US    = 0.0
            HE    = 0.0
            ZI    = 0.0
            KST   = 0
            SY    = 0.0
            SZ    = 0.0
            IFLG  = 4
            GO TO 100
         ELSE IF (WAKE .AND. WAKESS) THEN
            IFLG = 2
         ELSE IF (WAKE) THEN
            IFLG = 3
         END IF

      ELSE IF (VOLUME) THEN
C        Calculate Effective Radius
         XRAD = 2.15*SYINIT
         IF ((X-XRAD) .LT. 0.99) THEN
C           Receptor Upwind of Downwind Edge
            HRVAL = 0.0
            UREF  = 0.0
            US    = 0.0
            HE    = 0.0
            ZI    = 0.0
            KST   = 0
            SY    = 0.0
            SZ    = 0.0
            IFLG  = 5
            GO TO 100
         ELSE
C           Calculate Effective Plume Height
            CALL VHEFF
C           Set HEFLAT = HE to avoid plume above ZI in PCHI
            HEFLAT = HE
C           Calculate Dispersion Parameters
            CALL VDIS
            IFLG = 1
         END IF

      ELSE IF (AREA) THEN
C        Set Effective Source Height
         HE = HS
         IFLG = 1
      END IF
C
C     THE MINIMUM MIXING HEIGHT DUE TO MECHANICAL MIXING IS
C     FOUND BY SETTING ZI EQUAL TO 0.3 * USTAR / F WHERE F IS
C     THE CORIOLIS PARAMETER.  FOR THIS ALGORITHM, USTAR IS
C     ASSUMED TO BE EQUAL TO 0.1 * U10M.  TO BE CONSERVATIVE, IF
C     THIS ZI IS BELOW HE (NO CONTRIBUTION CASE), THEN ZI IS SET
C     EQUAL TO HE + 1 IN ORDER TO SET UP MAXIMUM REFLECTION.
C
      ZI = 320. * UREF
      IF (ZI .GT. 10000.) ZI = 10000.
      IF (ZI .LT. HE+1.)  ZI = HE + 1.
C  FROM R. BRODE 1991 AMS CONFERENCE PREPRINT.  ADJUSTS MIXING HEIGHTS SO
C    CALCULATED CONCENTRATIONS ARE MORE CONSERVATIVE WITH RESPECT TO ISCST2
C    RESULTS.
         U10 = UREF * (10.0/HANE)**P
           IF (KST .LE. 4 .AND. ICI .EQ. 1) THEN
             ZI = MAX(ZIMIN(KST), (HE *(1.0 + ZIFACT(KST) * U10)))
           END IF

C
C     MIXING HTS ARE NOT USED IN COMPUTING CONCENTRATIONS
C     DURING STABLE CONDITIONS.  SET TO 10000 M FOR E AND F.
C
      IF (KST .GT. 4 ) ZI = 10000.
      IF (HE .LT. 1.0E-10) HE = 0.0
      QTK = Q * 1.0E06
      ZFLAG = ZR

      IF (POINT .OR. FLARE .OR. VOLUME) THEN
         IF (LDEP) THEN
C           Determine deposition correction factors   ---   CALL DEPCOR
C           Loop over particle sizes
            DO 150 I=1,NPD
               IF (DPLETE) THEN
                  CALL DEPCOR( VDEP(I),VGRAV(I),ZRDEP,ZFLAG,X,
     &                 XZ,HE,ZI,US,RURAL,URBAN,KST,SZ,SBID,
     &                 DEBUG,IOUNIT,QCOR(I),PCORZR(I),
     &                 PCORZD(I))
               ELSE
                  QCOR(I) = 1.
                  PCORZR(I) = 1.
                  PCORZD(I) = 1.
               ENDIF
150         CONTINUE

            CONC  = .FALSE.
            DEPOS = .TRUE.
            QTK = Q * 3600.
            CALL PDEP
         END IF

      ELSE IF (AREA) THEN
         IF (MAXWD) THEN
            ANGRAD = -1.0 * WDMAX * DTORAD
            AXVERT(1) =  (0.5*YINIT*SIN(ANGRAD)-
     &                    0.5*XINIT*COS(ANGRAD))/1000.
            AYVERT(1) = (-0.5*YINIT*COS(ANGRAD)-
     &                    0.5*XINIT*SIN(ANGRAD))/1000.
            AXVERT(2) = (-0.5*YINIT*SIN(ANGRAD)-
     &                    0.5*XINIT*COS(ANGRAD))/1000.
            AYVERT(2) =  (0.5*YINIT*COS(ANGRAD)-
     &                    0.5*XINIT*SIN(ANGRAD))/1000.
            AXVERT(3) = (-0.5*YINIT*SIN(ANGRAD)+
     &                    0.5*XINIT*COS(ANGRAD))/1000.
            AYVERT(3) =  (0.5*YINIT*COS(ANGRAD)+
     &                    0.5*XINIT*SIN(ANGRAD))/1000.
            AXVERT(4) =  (0.5*YINIT*SIN(ANGRAD)+
     &                    0.5*XINIT*COS(ANGRAD))/1000.
            AYVERT(4) = (-0.5*YINIT*COS(ANGRAD)+
     &                    0.5*XINIT*SIN(ANGRAD))/1000.
            AXVERT(5) = AXVERT(1)
            AYVERT(5) = AYVERT(1)

C           Determine Coordinates of Vertices in WDIR Coord. System
            CALL AVERTS
            IF (LDEP) THEN
               CONC  = .FALSE.
               DEPOS = .TRUE.
               QTK = Q * 3600.
            END IF
C           Calculate Area Source Integral
            CALL AREAIN
         ELSE
C           Determine Coordinates of Vertices in WDIR Coord. System
            CALL AVERTS
            IF (LDEP) THEN
               CONC  = .FALSE.
               DEPOS = .TRUE.
               QTK = Q * 3600.
            END IF
C           Calculate Area Source Integral
            CALL AREAIN
         END IF
      END IF

 100  CONTINUE

      DEPSEC = HRVAL

CXXX      WRITE(IPRT,400) X,HRVAL,KST,UREF,US,ZI,
CXXX     &                HE,SY,SZ,DWASH(IFLG)
CXXX      WRITE(IOUT,400) X,HRVAL,KST,UREF,US,ZI,
CXXX     &                HE,SY,SZ,DWASH(IFLG)
CXXX      WRITE(IOUT,*) ' '
CTEMP400      FORMAT(1X,F7.0,2X,G10.4,4X,I1,4X,F4.1,3X,F4.1,1X,F7.1,
CTEMP     &          3(1X,F7.2),4X,A2)

      RETURN
      END

      SUBROUTINE MAXXD(KSTIN,UREFIN)
C
C        CALCULATES CONCENTRATION (CONSEC) FOR MET CONDITIONS ASSOCIATED
C        WITH THE MAXIMUM DEPOSITION VALUE AT A GIVEN DISTANCE.
C
C        INPUTS:
C           KST   STABILITY CLASS FOR MAX DEPOSITION
C           UREF  10-METER WIND SPEED FOR MAX DEPOSITION
C
C        ROUTINES USED:
C           DELH    COMPUTES FINAL PLUME RISE FOR NO DOWNWASH SCENARIO
C           SIGY    COMPUTES RURAL OR URBAN SIGMA-Y
C           SIGZ    COMPUTES RURAL OR URBAN SIGMA-Z
C           SYSS    COMPUTES SIGMA-Y DURING DOWNWASH SCENARIOS
C           SZSS    COMPUTES SIGMA-Z DURING DOWNWASH SCENARIOS
C           HM      COMPUTES MOMENTUM PLUME RISE
C           DHHS    COMPUTES PLUME RISE FOR HUBER-SNYDER SCENARIO
C           DHSS    COMPUTES PLUME RISE FOR SCHULMAN-SCIRE SCENARIO
C           CONC    COMPUTES GAUSSIAN PLUME GROUND-LEVEL CONCENTRATION
C           HSPRM   COMPUTES STACK HEIGHT WITH STACK TIP DOWNWASH
C
C        OUTPUT:
C           CONSEC  SECONDARY CONCENTRATION (UG/M**3)
C
      INCLUDE 'MAIN.INC'
      REAL    PURBAN(6), PRURAL(6), ADTDZ(6)
      DATA    PURBAN /0.15,0.15,0.20,0.25,0.30,0.30/
      DATA    PRURAL /0.07,0.07,0.10,0.15,0.35,0.55/
      DATA    ADTDZ /0.0, 0.0, 0.0, 0.0, 0.02, 0.035/
C
C        INITIALIZATIONS
C
      KST = KSTIN
      UREF = UREFIN
      IF (KST .EQ. 0) RETURN
      WAKE   = .FALSE.
      WAKESS = .FALSE.
      Y = 0.0
      ZLB  = AMIN1(HB,HWP)
C
      UNSTAB = .FALSE.
      NEUTRL = .FALSE.
      STABLE = .FALSE.
      IF (KST .LT. 4) THEN
         UNSTAB = .TRUE.
      ELSE IF (KST .EQ. 4) THEN
         NEUTRL = .TRUE.
      ELSE IF (KST .GT. 4) THEN
         STABLE = .TRUE.
      END IF

      DTDZ = ADTDZ(KST)
      IF (DTDZ .GT. 0.0 .AND. TA .NE. 0.0) THEN
         S = G*DTDZ/TA
         RTOFS = SQRT(S)
      ELSE
         S = 1.0E-10
         RTOFS = 1.0E-10
      END IF

C
      IF (X .GT. 50000. .AND. UREF .LT. 2.0) THEN
         UREF = 2.0
      END IF
C
C
C     ADJUST WIND SPEED FROM REFERENCE (ANEMOMETER) HEIGHT, ZREF,
C     OF 10-METERS, TO STACK HEIGHT
C
      IF (RURAL) THEN
         P = PRURAL(KST)
      ELSE IF (URBAN) THEN
         P = PURBAN(KST)
      END IF
      CALL WSADJ

C     Calculate Deposition Velocities for this Source
      IF (LDEP) THEN
         CALL OBUKHOV
         CALL U_STAR
         CALL VDP
      ENDIF

      IF (POINT .OR. FLARE) THEN
C        Calculate Distance to Final Rise
         CALL DISTF
C
C     DETERMINE TYPE OF DOWNWASH SCENARIO, IF ANY
C
         DSBH = HB
         DSBW = HWP
         WAKE   = .FALSE.
         WAKESS = .FALSE.
C        Determine Wake Flags
         CALL WAKFLG
         FSTREC = .TRUE.
         NOBID  = .FALSE.
         NOSTD  = .FALSE.
         GRDRIS = .FALSE.
C        Calculate Effective Plume Height
         CALL PHEFF
C        Set HEFLAT = HE to avoid plume above ZI in PCHI
         HEFLAT = HE
C        Calculate Dispersion Parameters
         CALL PDIS
         IF (.NOT. WAKE) THEN
            IFLG = 1
         ELSE IF (WAKE .AND. X .LT. 3.*ZLB) THEN
            HRVAL = 0.0
            UREF  = 0.0
            US    = 0.0
            HE    = 0.0
            ZI    = 0.0
            KST   = 0
            SY    = 0.0
            SZ    = 0.0
            IFLG  = 4
            GO TO 100
         ELSE IF (WAKE .AND. WAKESS) THEN
            IFLG = 2
         ELSE IF (WAKE) THEN
            IFLG = 3
         END IF

      ELSE IF (VOLUME) THEN
C        Calculate Effective Radius
         XRAD = 2.15*SYINIT
         IF ((X-XRAD) .LT. 0.99) THEN
C           Receptor Upwind of Downwind Edge
            HRVAL = 0.0
            UREF  = 0.0
            US    = 0.0
            HE    = 0.0
            ZI    = 0.0
            KST   = 0
            SY    = 0.0
            SZ    = 0.0
            IFLG  = 5
            GO TO 100
         ELSE
C           Calculate Effective Plume Height
            CALL VHEFF
C           Set HEFLAT = HE to avoid plume above ZI in PCHI
            HEFLAT = HE
C           Calculate Dispersion Parameters
            CALL VDIS
            IFLG = 1
         END IF

      ELSE IF (AREA) THEN
C        Set Effective Source Height
         HE = HS
         IFLG = 1
      END IF
C
C     THE MINIMUM MIXING HEIGHT DUE TO MECHANICAL MIXING IS
C     FOUND BY SETTING ZI EQUAL TO 0.3 * USTAR / F WHERE F IS
C     THE CORIOLIS PARAMETER.  FOR THIS ALGORITHM, USTAR IS
C     ASSUMED TO BE EQUAL TO 0.1 * U10M.  TO BE CONSERVATIVE, IF
C     THIS ZI IS BELOW HE (NO CONTRIBUTION CASE), THEN ZI IS SET
C     EQUAL TO HE + 1 IN ORDER TO SET UP MAXIMUM REFLECTION.
C
      ZI = 320. * UREF
      IF (ZI .GT. 10000.) ZI = 10000.
      IF (ZI .LT. HE+1.)  ZI = HE + 1.
C  FROM R. BRODE 1991 AMS CONFERENCE PREPRINT.  ADJUSTS MIXING HEIGHTS SO
C    CALCULATED CONCENTRATIONS ARE MORE CONSERVATIVE WITH RESPECT TO ISCST2
C    RESULTS.
         U10 = UREF * (10.0/HANE)**P
           IF (KST .LE. 4 .AND. ICI .EQ. 1) THEN
             ZI = MAX(ZIMIN(KST), (HE *(1.0 + ZIFACT(KST) * U10)))
           END IF

C
C     MIXING HTS ARE NOT USED IN COMPUTING CONCENTRATIONS
C     DURING STABLE CONDITIONS.  SET TO 10000 M FOR E AND F.
C
      IF (KST .GT. 4 ) ZI = 10000.
      IF (HE .LT. 1.0E-10) HE = 0.0
      QTK = Q * 1.0E06
      ZFLAG = ZR

      IF (POINT .OR. FLARE .OR. VOLUME) THEN
C        Determine deposition correction factors   ---   CALL DEPCOR
         IF (LDEP) THEN
C           Loop over particle sizes
            DO 150 I=1,NPD
               IF (DPLETE) THEN
                  CALL DEPCOR( VDEP(I),VGRAV(I),ZRDEP,ZFLAG,X,
     &                 XZ,HE,ZI,US,RURAL,URBAN,KST,SZ,SBID,
     &                 DEBUG,IOUNIT,QCOR(I),PCORZR(I),
     &                 PCORZD(I))
               ELSE
                  QCOR(I) = 1.
                  PCORZR(I) = 1.
                  PCORZD(I) = 1.
               ENDIF
150         CONTINUE
         ENDIF

         CONC  = .TRUE.
         DEPOS = .FALSE.
         QTK = Q * 1.0E06
         CALL PCHI

      ELSE IF (AREA) THEN
         IF (MAXWD) THEN
            ANGRAD = -1.0 * WDMAXD * DTORAD
            AXVERT(1) =  (0.5*YINIT*SIN(ANGRAD)-
     &                    0.5*XINIT*COS(ANGRAD))/1000.
            AYVERT(1) = (-0.5*YINIT*COS(ANGRAD)-
     &                    0.5*XINIT*SIN(ANGRAD))/1000.
            AXVERT(2) = (-0.5*YINIT*SIN(ANGRAD)-
     &                    0.5*XINIT*COS(ANGRAD))/1000.
            AYVERT(2) =  (0.5*YINIT*COS(ANGRAD)-
     &                    0.5*XINIT*SIN(ANGRAD))/1000.
            AXVERT(3) = (-0.5*YINIT*SIN(ANGRAD)+
     &                    0.5*XINIT*COS(ANGRAD))/1000.
            AYVERT(3) =  (0.5*YINIT*COS(ANGRAD)+
     &                    0.5*XINIT*SIN(ANGRAD))/1000.
            AXVERT(4) =  (0.5*YINIT*SIN(ANGRAD)+
     &                    0.5*XINIT*COS(ANGRAD))/1000.
            AYVERT(4) = (-0.5*YINIT*COS(ANGRAD)+
     &                    0.5*XINIT*SIN(ANGRAD))/1000.
            AXVERT(5) = AXVERT(1)
            AYVERT(5) = AYVERT(1)

C           Determine Coordinates of Vertices in WDIR Coord. System
            CALL AVERTS
            CONC  = .TRUE.
            DEPOS = .FALSE.
            QTK = Q * 1.0E06
C           Calculate Area Source Integral
            CALL AREAIN
         ELSE
C           Determine Coordinates of Vertices in WDIR Coord. System
            CALL AVERTS
            CONC  = .TRUE.
            DEPOS = .FALSE.
            QTK = Q * 1.0E06
C           Calculate Area Source Integral
            CALL AREAIN
         END IF
      END IF

 100  CONTINUE

      CONSEC = HRVAL

CXXX      WRITE(IPRT,400) X,HRVAL,KST,UREF,US,ZI,
CXXX     &                HE,SY,SZ,DWASH(IFLG)
CXXX      WRITE(IOUT,400) X,HRVAL,KST,UREF,US,ZI,
CXXX     &                HE,SY,SZ,DWASH(IFLG)
CTEMP400      FORMAT(1X,F7.0,2X,G10.4,4X,I1,4X,F4.1,3X,F4.1,1X,F7.1,
CTEMP     &          3(1X,F7.2),4X,A2)

      RETURN
      END

      SUBROUTINE TPMX(XCNT,CMAX,XMAX,XMIN)
C
C        SUBROUTINE TPMX LOCATES THE MAXIMUM CONCENTRATION.
C        AN ITERATIVE PROCEDURE IS EMPLOYED TO PINPOINT THE
C        DISTANCE TO MAX CONCENTRATION TO WITHIN ONE METER.
C
      INCLUDE 'MAIN.INC'
      REAL FACT(25)
      DATA FACT/-0.95,-0.90,-0.85,-0.8,-0.75,-0.7,-0.65,-0.6,-0.55,-0.5,
     &          -0.45,-0.4,-0.35,-0.3,-0.25,-0.2,-0.15,-0.1,-0.05,
     &           0.05,0.1,0.2,0.3,0.4,0.5/
      IF (CHICNT .EQ. 0.0) THEN
         CMAX = 0.0
         XMAX = XCNT
         KSTMAX = KSTCNT
         RETURN
      END IF
      CLST = CHICNT
      XLST = XCNT
      KSTLST = KSTCNT
      DO 100 I = 1, 25
         IF (I .EQ. 1) THEN
C           Set DX < 0 for controlling stability check
            DX = -1.0
         ELSE
            DX = 0.0
         END IF
         X = XCNT + (XCNT * FACT(I))
         IF (X .LT. 1.0) X = 1.0
         CALL USERX
         IF (CHIMAX .GT. CLST .AND. X .GE. XMIN) THEN
            XLST = X
            CLST = CHIMAX
            KSTLST = KSTMAX
         END IF
100   CONTINUE
      X = XLST
C
      IF (X .LE. 1000.) THEN
         DX = 100.0
      ELSE IF (X .LE. 10000.) THEN
         DX = 1000.0
      ELSE
         DX = 10000.
      END IF
      IF (X .EQ. XMIN) THEN
         DX = -1.0 * DX
      END IF
C
C        THE FOLLOWING INITIAL INCREMENTS ARE USED:
C           .01 KM FOR X LESS THAN 1 KM
C           0.1 KM FOR X 1 KM TO 10 KM
C           1.0 KM FOR X 10 KM TO 50 KM
C
      N = 1
8     DX = -0.1 * DX
      IF (ABS(DX) .LT. 1.0) THEN
         IF (XMAX .LT. XMIN) THEN
            XMAX = XCNT
            CMAX = CHICNT
            KSTMAX = KSTCNT
         END IF
         RETURN
      END IF
C
C        REVERSE DIRECTIONS, REDUCE STEPPING INCREMENT.
C        THE ITERATIVE PROCESS CONTINUES IN THIS MANNER
C        WITH CALCULATIONS GOING BACKWARDS AND FORWARDS
C        IN SMALLER AND SMALLER INCREMENTS UNTIL THE
C        INCREMENT IS LESS THAN ONE METER.
C        IF X REACHES 50 KM CEASE COMPUTATIONS FOR THIS WIND SPEED.
C        DISTANCE TO THE MAXIMUM IS NOT ALLOWED TO BE LESS THAN THE
C        MINIMUM DISTANCE INPUT BY THE USER, XMIN.
C
7     X=X+DX
      IF (X .LT. 1.0) X = 1.0
      IF (N .EQ. 1) THEN
         KSTSAV = 0
         KSTSVD = 0
      END IF
      CALL USERX
      N = N + 1
      IF (N .GT. 50) THEN
         IF (XMAX .LT. XMIN) THEN
            XMAX = XCNT
            CMAX = CHICNT
            KSTMAX = KSTCNT
            RETURN
         ELSE
            WRITE(IPRT,*)'ITERATION STOPPED AT 50 - MAX NOT FOUND!!!'
            WRITE(IOUT,*)' '
            WRITE(IOUT,*)'ITERATION STOPPED AT 50 - MAX NOT FOUND!!!'
            RETURN
         END IF
      ELSE
         IF (CHIMAX.LT.CLST) THEN
            CMAX = CLST
            XMAX = XLST
            KSTMAX = KSTLST
            CLST = CHIMAX
            XLST = X
            KSTLST = KSTMAX
            GO TO 8
         END IF
         CMAX = CHIMAX
         XMAX = X
         KSTMAX = KSTMAX
         CLST = CHIMAX
         XLST = X
         KSTLST = KSTMAX
         IF (X .GT. 50000.) RETURN
         IF (X .EQ. 1.0) THEN
            IF (XMAX .LT. XMIN) THEN
               XMAX = XCNT
               CMAX = CHICNT
               KSTMAX = KSTCNT
            END IF
            RETURN
         END IF
         GO TO 7
      END IF

      END

      SUBROUTINE TPMXD(XCNTD,DMAX,XMAXD,XMIN)
C
C        SUBROUTINE TPMXD LOCATES THE MAXIMUM DEPOSITION.
C        AN ITERATIVE PROCEDURE IS EMPLOYED TO PINPOINT THE
C        DISTANCE TO MAX CONCENTRATION TO WITHIN ONE METER.
C
      INCLUDE 'MAIN.INC'
      REAL FACT(25)
      DATA FACT/-0.95,-0.90,-0.85,-0.8,-0.75,-0.7,-0.65,-0.6,-0.55,-0.5,
     &          -0.45,-0.4,-0.35,-0.3,-0.25,-0.2,-0.15,-0.1,-0.05,
     &           0.05,0.1,0.2,0.3,0.4,0.5/
      IF (DEPCNT .EQ. 0.0) THEN
         DMAX = 0.0
         XMAXD = XCNTD
         KSTMXD = KSTCTD
         RETURN
      END IF
      DLST = DEPCNT
      XLST = XCNTD
      KSTLST = KSTCTD
      DO 100 I = 1, 25
         IF (I .EQ. 1) THEN
C           Set DX < 0 for controlling stability check
            DX = -1.0
         ELSE
            DX = 0.0
         END IF
         X = XCNTD + (XCNTD * FACT(I))
         IF (X .LT. 1.0) X = 1.0
         CALL USERX
         IF (DEPMAX .GT. DLST .AND. X .GE. XMIN) THEN
            XLST = X
            DLST = DEPMAX
            KSTLST = KSTMXD
         END IF
100   CONTINUE
      X = XLST
C
      IF (X .LE. 1000.) THEN
         DX = 100.0
      ELSE IF (X .LE. 10000.) THEN
         DX = 1000.0
      ELSE
         DX = 10000.
      END IF
      IF (X .EQ. XMIN) THEN
         DX = -1.0 * DX
      END IF
C
C        THE FOLLOWING INITIAL INCREMENTS ARE USED:
C           .01 KM FOR X LESS THAN 1 KM
C           0.1 KM FOR X 1 KM TO 10 KM
C           1.0 KM FOR X 10 KM TO 50 KM
C
      N = 1
8     DX = -0.1 * DX
      IF (ABS(DX) .LT. 1.0) THEN
         IF (XMAXD .LT. XMIN) THEN
            XMAXD = XCNTD
            DMAX = DEPCNT
            KSTMXD = KSTCTD
         END IF
         RETURN
      END IF
C
C        REVERSE DIRECTIONS, REDUCE STEPPING INCREMENT.
C        THE ITERATIVE PROCESS CONTINUES IN THIS MANNER
C        WITH CALCULATIONS GOING BACKWARDS AND FORWARDS
C        IN SMALLER AND SMALLER INCREMENTS UNTIL THE
C        INCREMENT IS LESS THAN ONE METER.
C        IF X REACHES 50 KM CEASE COMPUTATIONS FOR THIS WIND SPEED.
C        DISTANCE TO THE MAXIMUM IS NOT ALLOWED TO BE LESS THAN THE
C        MINIMUM DISTANCE INPUT BY THE USER, XMIN.
C
7     X=X+DX
      IF (X .LT. 1.0) X = 1.0
      IF (N .EQ. 1) THEN
         KSTSAV = 0
         KSTSVD = 0
      END IF
      CALL USERX
      N = N + 1
      IF (N .GT. 50) THEN
         IF (XMAXD .LT. XMIN) THEN
            XMAXD = XCNTD
            DMAX = DEPCNT
            KSTMXD = KSTCTD
            RETURN
         ELSE
            WRITE(IPRT,*)'ITERATION STOPPED AT 50 - MAX NOT FOUND!!!'
            WRITE(IOUT,*)' '
            WRITE(IOUT,*)'ITERATION STOPPED AT 50 - MAX NOT FOUND!!!'
            RETURN
         END IF
      ELSE
         IF (DEPMAX.LT.DLST) THEN
            DMAX = DLST
            XMAXD = XLST
            KSTMXD = KSTLST
            DLST = DEPMAX
            XLST = X
            KSTLST = KSTMXD
            GO TO 8
         END IF
         DMAX = DEPMAX
         XMAXD = X
         KSTMXD = KSTMXD
         DLST = DEPMAX
         XLST = X
         KSTLST = KSTMXD
         IF (X .GT. 50000.) RETURN
         IF (X .EQ. 1.0) THEN
            IF (XMAXD .LT. XMIN) THEN
               XMAXD = XCNTD
               DMAX = DEPCNT
               KSTMXD = KSTCTD
            END IF
            RETURN
         END IF
         GO TO 7
      END IF

      END

      SUBROUTINE TPMXA(XCNT,CMAX,XMAX,XMIN)
C
C        SUBROUTINE TPMXA LOCATES THE MAXIMUM CONCENTRATION FOR AREA SOURCES.
C        AN ITERATIVE PROCEDURE IS EMPLOYED TO PINPOINT THE DISTANCE TO MAX
C        CONCENTRATION TO WITHIN ONE METER.  FOR AREA SOURCES, THE INITIAL
C        RANGE OF DISTANCES IS MODIFIED, AND ONLY 1.0 M/S WIND SPEEDS ARE USED.
C
      INCLUDE 'MAIN.INC'
      REAL FACT(12)
      DATA FACT/-0.75,-0.5,-0.25,-0.1,
     &           0.1,0.25,0.5,0.75,1.0,2.0,5.0,10.0/
      IF (CHICNT .EQ. 0.0) THEN
         CMAX = 0.0
         XMAX = XCNT
         KSTMAX = KSTCNT
         RETURN
      END IF
      CLST = CHICNT
      XLST = XCNT
      KSTLST = KSTCNT
      DO 100 I = 1, 12
         IF (I .EQ. 1) THEN
C           Set DX < 0 for controlling stability check
            DX = -1.0
         ELSE
            DX = 0.0
         END IF
         X = XCNT + (XCNT * FACT(I))
         CALL USERX
         IF (CHIMAX .GT. CLST .AND. X .GE. XMIN) THEN
            XLST = X
            CLST = CHIMAX
            KSTLST = KSTMAX
         END IF
100   CONTINUE
      X = XLST
C
      IF (X .LE. 1000.) THEN
         DX = 100.0
      ELSE IF (X .LE. 10000.) THEN
         DX = 1000.0
      ELSE
         DX = 10000.
      END IF
C      IF (X .EQ. XMIN) THEN
      IF (X .EQ. XMIN .OR. AREA) THEN
         DX = -1.0 * DX
      END IF
C
C        THE FOLLOWING INITIAL INCREMENTS ARE USED:
C           .01 KM FOR X LESS THAN 1 KM
C           0.1 KM FOR X 1 KM TO 10 KM
C           1.0 KM FOR X 10 KM TO 50 KM
C
      N = 1
8     DX = -0.1 * DX
      IF (ABS(DX) .LT. 1.0) THEN
         IF (XMAX .LT. XMIN) THEN
            XMAX = XCNT
            CMAX = CHICNT
            KSTMAX = KSTCNT
         END IF
         RETURN
      END IF
C
C        REVERSE DIRECTIONS, REDUCE STEPPING INCREMENT.
C        THE ITERATIVE PROCESS CONTINUES IN THIS MANNER
C        WITH CALCULATIONS GOING BACKWARDS AND FORWARDS
C        IN SMALLER AND SMALLER INCREMENTS UNTIL THE
C        INCREMENT IS LESS THAN ONE METER.
C        IF X REACHES 50 KM CEASE COMPUTATIONS FOR THIS WIND SPEED.
C        DISTANCE TO THE MAXIMUM IS NOT ALLOWED TO BE LESS THAN THE
C        MINIMUM DISTANCE INPUT BY THE USER, XMIN.
C
7     X=X+DX

      IF (N .EQ. 1) THEN
         KSTSAV = 0
         KSTSVD = 0
      END IF
      CALL USERX
      N = N + 1
      IF (N .GT. 50) THEN
         IF (XMAX .LT. XMIN) THEN
            XMAX = XCNT
            CMAX = CHICNT
            KSTMAX = KSTCNT
            RETURN
         ELSE
            WRITE(IPRT,*)'ITERATION STOPPED AT 50 - MAX NOT FOUND!!!'
            WRITE(IOUT,*)' '
            WRITE(IOUT,*)'ITERATION STOPPED AT 50 - MAX NOT FOUND!!!'
            RETURN
         END IF
      ELSE
         IF (CHIMAX.LT.CLST) THEN
            CMAX = CLST
            XMAX = XLST
            KSTMAX = KSTLST
            CLST = CHIMAX
            XLST = X
            KSTLST = KSTMAX
            GO TO 8
         END IF
         CMAX = CHIMAX
         XMAX = X
         KSTMAX = KSTMAX
         CLST = CHIMAX
         XLST = X
         KSTLST = KSTMAX
         IF (X .GT. 50000.) RETURN
         IF (X .EQ. 1.0) THEN
            IF (XMAX .LT. XMIN) THEN
               XMAX = XCNT
               CMAX = CHICNT
               KSTMAX = KSTCNT
            END IF
            RETURN
         END IF
         GO TO 7
      END IF

      END

      SUBROUTINE TPMXAD(XCNTD,DMAX,XMAXD,XMIN)
C
C        SUBROUTINE TPMXAD LOCATES THE MAXIMUM DEPOSITION FOR AREA SOURCES.
C        AN ITERATIVE PROCEDURE IS EMPLOYED TO PINPOINT THE DISTANCE TO MAX
C        CONCENTRATION TO WITHIN ONE METER.  FOR AREA SOURCES, THE INITIAL
C        RANGE OF DISTANCES IS MODIFIED, AND ONLY 1.0 M/S WIND SPEEDS ARE USED.
C
      INCLUDE 'MAIN.INC'
      REAL FACT(12)
      DATA FACT/-0.75,-0.5,-0.25,-0.1,
     &           0.1,0.25,0.5,0.75,1.0,2.0,5.0,10.0/
      IF (DEPCNT .EQ. 0.0) THEN
         DMAX = 0.0
         XMAXD = XCNTD
         KSTMXD = KSTCTD
         RETURN
      END IF
      DLST = DEPCNT
      XLST = XCNTD
      KSTLST = KSTCTD
      DO 100 I = 1, 12
         IF (I .EQ. 1) THEN
C           Set DX < 0 for controlling stability check
            DX = -1.0
         ELSE
            DX = 0.0
         END IF
         X = XCNTD + (XCNTD * FACT(I))
         CALL USERX
         IF (DEPMAX .GT. DLST .AND. X .GE. XMIN) THEN
            XLST = X
            DLST = DEPMAX
            KSTLST = KSTMXD
         END IF
100   CONTINUE
      X = XLST
C
      IF (X .LE. 1000.) THEN
         DX = 100.0
      ELSE IF (X .LE. 10000.) THEN
         DX = 1000.0
      ELSE
         DX = 10000.
      END IF
C      IF (X .EQ. XMIN) THEN
      IF (X .EQ. XMIN .OR. AREA) THEN
         DX = -1.0 * DX
      END IF
C
C        THE FOLLOWING INITIAL INCREMENTS ARE USED:
C           .01 KM FOR X LESS THAN 1 KM
C           0.1 KM FOR X 1 KM TO 10 KM
C           1.0 KM FOR X 10 KM TO 50 KM
C
      N = 1
8     DX = -0.1 * DX
      IF (ABS(DX) .LT. 1.0) THEN
         IF (XMAXD .LT. XMIN) THEN
            XMAXD = XCNTD
            DMAX = DEPCNT
            KSTMXD = KSTCTD
         END IF
         RETURN
      END IF
C
C        REVERSE DIRECTIONS, REDUCE STEPPING INCREMENT.
C        THE ITERATIVE PROCESS CONTINUES IN THIS MANNER
C        WITH CALCULATIONS GOING BACKWARDS AND FORWARDS
C        IN SMALLER AND SMALLER INCREMENTS UNTIL THE
C        INCREMENT IS LESS THAN ONE METER.
C        IF X REACHES 50 KM CEASE COMPUTATIONS FOR THIS WIND SPEED.
C        DISTANCE TO THE MAXIMUM IS NOT ALLOWED TO BE LESS THAN THE
C        MINIMUM DISTANCE INPUT BY THE USER, XMIN.
C
7     X=X+DX

      IF (N .EQ. 1) THEN
         KSTSAV = 0
         KSTSVD = 0
      END IF
      CALL USERX
      N = N + 1
      IF (N .GT. 50) THEN
         IF (XMAXD .LT. XMIN) THEN
            XMAXD = XCNTD
            DMAX = DEPCNT
            KSTMXD = KSTCTD
            RETURN
         ELSE
            WRITE(IPRT,*)'ITERATION STOPPED AT 50 - MAX NOT FOUND!!!'
            WRITE(IOUT,*)' '
            WRITE(IOUT,*)'ITERATION STOPPED AT 50 - MAX NOT FOUND!!!'
            RETURN
         END IF
      ELSE
         IF (DEPMAX.LT.DLST) THEN
            DMAX = DLST
            XMAXD = XLST
            KSTMXD = KSTLST
            DLST = DEPMAX
            XLST = X
            KSTLST = KSTMXD
            GO TO 8
         END IF
         DMAX = DEPMAX
         XMAXD = X
         KSTMXD = KSTMXD
         DLST = DEPMAX
         XLST = X
         KSTLST = KSTMXD
         IF (X .GT. 50000.) RETURN
         IF (X .EQ. 1.0) THEN
            IF (XMAXD .LT. XMIN) THEN
               XMAXD = XCNTD
               DMAX = DEPCNT
               KSTMXD = KSTCTD
            END IF
            RETURN
         END IF
         GO TO 7
      END IF

      END

      BLOCK DATA INIT
C
C     BLOCK DATA INIT SUBPROGRAM TO INITIALIZE DATA IN COMMON BLOCKS.
C
      INCLUDE 'MAIN.INC'

      DATA XAUTO/1.,100.,  200.,  300.,  400.,  500.,
     &              600.,  700.,  800.,  900., 1000.,
     &             1100., 1200., 1300., 1400., 1500.,
     &             1600., 1700., 1800., 1900., 2000.,
     &             2100., 2200., 2300., 2400., 2500.,
     &             2600., 2700., 2800., 2900., 3000.,
     &             3500., 4000., 4500., 5000., 5500.,
     &             6000., 6500., 7000., 7500., 8000.,
     &             8500., 9000., 9500.,10000.,15000.,
     &            20000.,25000.,30000.,40000.,50000./

      DATA DWASH/'NO','SS','HS','NA','  '/

      DATA VERSN/'13043'/

C     Initialize File Units
      DATA IRD/5/, IPRT/6/, IOUT/9/, IDAT/7/, IDBG/13/
      DATA ZIMIN /300.0, 100.0, 30.0, 30.0/
      DATA ZIFACT /0.01, 0.02, 0.03, 0.04/

      END

c----------------------------------------------------------------------
      subroutine CAVITY2(iprt,iout,xstack,hslgl,exitw,diam,ts,ta,qemit,
     &                   hbldg,wcross,xldown,lwind,
     &                   chimax,xlrmax)
c----------------------------------------------------------------------
c --- CAVITY-- Building downwash -- screening model for near wake
c----------------------------------------------------------------------
c
c --- SCREEN2C   Version: 1.0       Level: 940325               CAVITY2
c
c --- Written by:  J. Scire & L. Schulman
c                  Sigma Research Corporation
c                  196 Baker Avenue
c                  Concord, MA  01742
c                  (508) 371-4200
c
c --- Modified for use in SCREEN2(C) by D. Strimaitis, SRC
c
c --- PURPOSE:
c     This is a screening version of the cavity model proposed by Schulman
c     and Scire (J. Air Waste Manage. Assoc., 43, 1122-1127, August 1993).
c     The model concepts are:
c        1. Momentum rise and bending over of the plume leads to increased
c           entrainment , and hence an initial dilution, near the stack.
c        2. Buoyancy and momentum rise lead to a fractional capture of the
c           plume by the near-wake, rather than an all-or-nothing approach.
c           The plume height and vertical plume dimensions at the end of
c           the near wake are used to estimate the fraction captured.
c        3. A virtual source is used for sigma-z to account for the plume
c           diameter near the source. The minimum vertical dimension at x=0
c           is the stack diameter and a top hat distribution is assumed.
c        4. The portion of the plume captured by the near-wake is assumed
c           to be a diffusing plume that follows a concentration-distance
c           dilution relationship. The maximum concentration, for screening
c           purposes, is calculated at the closest ground-level location in
c           the near-wake (the base of the lee wall) using the stretched-string
c           distance from the top of the stack.
c        5. The concentration is calculated as the sum of two dilution
c           components acting on the plume, an initial dilution D1 and
c           a distance dilution DS.
c                                     C = C0/(D1+DS)
c                                     D1= 1+B1(Ta/Ts)*(w/u(hs))**2
c                                     DS= u(hb)*s**2/(B0*w*A)
c
c
c           At large s:
c                                     C=B0*Q/(u(hb)*s**2)
c
c           At s=0 and no momentum:
c                                     C=Q/(w*A)
c
c           At small s with momentum:
c                                     C=Q/(w*A(1+B1(Ta/Ts)*(w/u(hs))**2))
c
c                 where u(hb) is incident wind speed at roof-level,
c                       u(hs) is incident wind speed at stack-top,
c                       s is stretched-string distance, C is concentration,
c                       C0 is stack gas concentration, w is exhaust speed,
c                       A is stack area, Ta is ambient temperature,
c                       Ts is exhaust temperature, and B0 and
c                       B1 are constants.
c
c --- INPUTS:
c             IPRT - integer - unit # for output to list file
c             IOUT - integer - unit # for output to disk file
c           XSTACK - real    - dist (m) from upwind edge of building to stack
c            HSLGL - real    - dist (m) from ground to stack-top
c            EXITW - real    - exit velocity (m/s)
c             DIAM - real    - stack diameter (m)
c               TS - real    - stack-gas temperature (K)
c               TA - real    - ambient temperature (K)
c            QEMIT - real    - mass emission rate (g/s)
c            HBLDG - real    - height of building (m)
c           WCROSS - real    - width (crosswind) of building (m)
c           XLDOWN - real    - length (alongwind) of building (m)
c            LWIND - logical - flag for writing results for all winds (TRUE)
c
c --- OUTPUT:
c           CHIMAX - real    - maximum concentration found (g/m^3)
c           XLRMAX - real    - length of bldg. recirc. cavity (m)
c      (also direct reporting of cavity concentration maxima to list-file)
c
c --- CAVITY2 called by: SCREEN2C
c --- CAVITY2 calls:     PRISE, FRGAUSS
c
c --- Other Definitions:
c     XDOWN  is the horizontal distance from the stack to receptor
c     SDOWN  is the "stretched-string" distance from the top of the
c            stack to a receptor at the base of the downwind wall of
c            the building
c-------------------------------------------------------------------------------
c
      character*8 ver,level
      logical lbuoy, debug, lwind
c
      data pi/3.1415927/
      data xsmall/1.e-5/
      data b0/16.0/
      data g/9.81/
c      data debug/.TRUE./
      data debug/.FALSE./

c --- Set and write model Version number and Level number
      ver='1.0'
      level='940325'
      write(iout,10)ver,level
10    format(/,'*** CAVITY2 -- Version: ',a8,3x,'Level: ',a8,' ***')

c --- Stability class set equal to neutral
      istab=4

c --- Turn on buoyancy flag
      lbuoy=.TRUE.
c
c --- Compute time-independent parameters
c
c --- Stretched-string dilution distance from the stack-top to the base
c --- of the lee wall is used to calculate maximum cavity concentration
      xdown=xldown-xstack
      pdown=SQRT((AMAX1(0.,hslgl-hbldg))**2+xdown**2)
      sdown=pdown+hbldg
      if(sdown.eq.0.0)sdown=xsmall
c
c --- Momentum flux parameters -- FMTERM
      fmterm=exitw**2*diam**2/(4.*ts)
c
c --- Buoyant rise term -- FBTERM
      if(lbuoy)then
         fbterm=g*exitw*diam**2/(4.*ts)
      else
         fbterm=0.0
      endif
c
c --- Building parameters
      h=hbldg
      w=wcross
      xl=xldown
c
      if(h.gt.8.0*w)h=8.0*w
      if(w.gt.8.0*h)w=8.0*h
c
c --- Scaling length, R
      R=(amin1(h,w))**0.6666667*(amax1(h,w))**0.3333333
c
c --- Compute exit concentration in stack
      vf=pi*diam**2*exitw/4.
      chistack=qemit/vf
c
c --- XLC is the length of the reattached roof recirculation region
      XLC=0.9*r
c
c --- HC is the max. height of the roof recirculation region
      HC=0.22*r
c
c --- XLR is the length of the downwind bldg. recirculation cavity
      if(w/h.le.0.2)then
         write(iout,*)'WARNING -- W/H le 0.2 -- Fackrell (1984) ',
     1   'relationships for Lr and Wr do not apply -- w = ',w,
     2   ' h = ',h,' w/h = ',w/h
         write(iprt,*)'WARNING -- W/H le 0.2 -- Fackrell (1984) ',
     1   'relationships for Lr and Wr do not apply -- w = ',w,
     2   ' h = ',h,' w/h = ',w/h
         chimax=-999.
         xlrmax=-999.
         return
      endif
      xldh=xl/HBLDG
      xldh=amax1(xldh,0.3)
      xldh=amin1(xldh,3.0)
      XLR=1.8*w/(xldh**0.3*(1.+0.24*w/HBLDG))
c
c --- WR is the width of the downwind bldg. recirculation cavity
      if(xlc.lt.xl)then
c ---    REATTACHED flow
         WR=w
      else
c ---    NON-reattached flow
         WR=0.6*hbldg+1.1*w
      endif
c
c --- Compute ht. of downwind cavity (HR)
      if(xlc.lt.xl)then
c ---    REATTACHED flow
         HR=hbldg
      else
c ---    NON-reattached flow
         HR=hbldg+hc
      endif
c
c --- Compute the vertical dispersion coefficient, SIGZ
      temp1=0.21*r**0.25

c --- Compute sigma z at END of near-wake region (SIGZNW)
c --- Sigma-z can be significantly affected near the source for large
c --- exhausts. The minimum plume dimension must be the stack diameter.
c --- Virtual source distance is calculated assuming that the value of
c --- sigma-z at x=0 can be derived from a top-hat distribution with
c --- a depth equal to the stack diameter. Thus, sigma-z at x=0 is
c --- (diameter/sqrt(2*pi)). The virtual source distance is XVZ.
c
      xnw=(xl-xstack)+xlr
      xvz=2.35*(diam/r**0.25)**1.333333
      sigznw=temp1*(xnw+xvz)**0.75
c
c --- Write time-independent computed parameters -----------------
      if(xlc.lt.xl)then
         write(iout,*)'FLOW IS REATTACHED'
      else
         write(iout,*)'FLOW IS NOT REATTACHED'
      endif
      write(iout,*)'Stack distance from upwind face (m)   = ',xstack
      write(iout,*)'Cavity Length (m)                     = ',xlr
      write(iout,*)'Cavity Height (m)                     = ',hr

c --- Write other related parameters in DEBUG mode
      if(DEBUG) then
         write(iout,*)
         write(iout,*)'Buoyant rise included ',lbuoy
         write(iout,*)'ISTAB   = ',istab,'    (stability class)'
         write(iout,*)'TA      = ',ta,' (ambient temperature)'
         write(iout,*)'VF      = ',vf,' (exhaust flow rate)'
         write(iout,*)'CHISTACK= ',chistack,' (exhaust concentration)'
         write(iout,*)'HSLGL   = ',hslgl,' (stack height)'
         write(iout,*)'H       = ',h,' (building height)'
         write(iout,*)'W       = ',w,' (building width)'
         write(iout,*)'XL      = ',xl,' (building length)'
         write(iout,*)'R       = ',r,' (building length scale)'
         write(iout,*)'SDOWN   = ',sdown,' (dilution distance)'
         write(iout,*)'XLC     = ',xlc,' (roof-top cavity length)'
         write(iout,*)'HC      = ',hc,' (cavity height above roof)'
         write(iout,*)'XLR     = ',xlr,' (downwind recirculation',
     1                                 ' cavity length)'
         write(iout,*)'WR      = ',wr,' (downwind recirculation cavity',
     1                                ' width)'
         write(iout,*)'XNW     = ',xnw,
     &                ' (Distance from stack to end of downwind cavity)'
         write(iout,*)'XVZ     = ',xvz,' (virtual source distance)'
         write(iout,*)'SIGZNW  = ',sigznw,
     &                ' (sigma-z at end of downwind cavity)'
         write(iout,*)
      endif
c
c --- Loop over met. data:  wind speed at 10m between 1 and 20 m/s
      ws10=0.
      chimax=0.
101   continue
      ws10=ws10+1.
      if(ws10.gt.20.1)then
        go to 999
      else
        wshb=ws10*(hbldg/10.)**0.2
        wshs=ws10*(AMAX1(hslgl,hbldg)/10.)**0.2
      endif
c ---
c ---   Initial dilution constant D1= 1+7*(ta/ts)(w/u)**2 if w/u<=2
c ---      and 1+7*(ta/ts)*4 if w/u >2.
c ---
      if(exitw/wshs.le.2.)then
           D1=1.+7.*(ta/ts)*(exitw/wshs)**2
      else
           D1=1.+28.*(ta/ts)
      endif

c --- Compute plume rise at END of near-wake region (SIGZNW)
      call PRISE(istab,wshs,ta,ts,hslgl,exitw,diam,fmterm,xnw,1,
     1           fbterm,znw,zefflgl,xmaxnw,zmaxnw)
c
c --- Fraction of plume within bldg. wake recirculation zone is
c --- determined with sigma z, plume ht. at END of recirc. zone
      call FRGAUSS(zefflgl,sigznw,HR,fnwbz2,fnwaz2)
c
c --- Only fraction below cavity height contributes to near-wake concentration
      fnwaz2 = 0.
c
c
c --------------------------------------------------------
c ---    Receptor is on ground at the base of the lee wall
c ---    in the recirculation zone for max. screening conc.
c --------------------------------------------------------
c
c ---    Concentration due to portion of plume within the bldg.
c ---    wake recirulation cavity ...
c ---    Use fraction of mass within recirc. region (based on sigz at
c ---    END of recirc. zone, FNWBZ2) to scale emission rate into cavity
c ---    NOTE..  !  Convert from g/s to ug/s  !
         qcav=qemit*fnwbz2*1.0e06
c
c ---    Total concentration at the receptor -- initial dilution plus
c ---    distance dilution--
         chi=qcav/(VF*D1+wshb*(sdown**2)/B0)
c
c ---    Save results for the wind speed that produces the largest conc.
         if(chi .GE. chimax) then
            chimax=chi
            xlrmax=xlr
            wsmax=ws10
         endif

c ---    Report results for this meteorology
         if(LWIND) then
            write(iout,*)'-------------------------'
            write(iout,*)'WS @ 10m, Hb, Hs (m/s)    = ',ws10,wshb,wshs
            write(iout,*)'Mass fraction in cavity   =',fnwbz2
            write(iout,*)'Concentration in cavity   =',chi
         endif
c
c ---    Write additional data for this wind speed (DEBUG mode)
         if(debug) then
            write(iout,*)
            write(iout,*)'D1     = ',d1,' (Initial dilution term)'
            write(iout,*)'ZNW    = ',znw,
     &                   ' (plume rise at end of downwind cavity)'
            write(iout,*)
         endif

      go to 101
999   continue
c
c --- Write receptor-dependent data in a table for max concentration
      write(iout,409) chimax,wsmax
409   format(/1x,'MAX Concentration =',f10.1,' (ug/m**3) for ws(10m) ',
     &       ' = ',f5.1,' (m/s)')
c
      return
      end
c----------------------------------------------------------------------
      subroutine PRISE(istab,ws,ta,ts,hs,exitw,diam,fmterm,xnw,ndown,
     1 fbterm,znw,zefflgl,xmax,zmax)
c  Previous subroutine:
c      subroutine PRISE(istab,ws,ta,ts,hs,exitw,diam,fmterm,xdown,ndown,
c     1 fbterm,z,zeff,xmax,zmax)
c        Note change from dimensioned variables xdown, z, zeff to scalar
c             xwn, znw, zefflgl as found by compiler error statement
c             -- Peter Eckhoff 5/5/2010
c----------------------------------------------------------------------
c
c --- BDMOD      Version:  1.0     Level:  901201                 PRISE
c                J. Scire, SRC
c
c --- PURPOSE:  Compute momentum plume rise from a stack at specified
c               downwind distances
c
c --- INPUTS:
c            ISTAB - integer - PGT stability class
c               WS - real    - Wind speed (m/s)
c               TA - real    - Air temperature (deg. K)
c               TS - real    - Stack gas exit temperature (deg. K)
c               HS - real    - Stack height (m)
c            EXITW - real    - Exit velocity (m/s)
c             DIAM - real    - Diameter (m)
c           FMTERM - real    - Time-independent portion of momentum
c                              flux (i.e., exitw**2*diam**2/(4.*ts)
c                              where ts is the stack temperature)
c     XDOWN(ndown) - real    - Array of downwind distances (m) for
c                              each receptor
c            NDOWN - integer - Number of downwind distances
c           FBTERM - logical - Time-independent portion of the
c                              buoyancy flux (g*exitw*diam**2/(4.*ts)
c
c --- OUTPUT:
c         Z(ndown) - real    - Array of total plume rise (m) at each
c                              receptor (momentum+buoyant components)
c      ZEFF(ndown) - real    - Array of effective stack heights (m) at
c                              each receptor
c             XMAX - real    - Distance to final momentum rise (m)
c             ZMAX - real    - Final momentum rise (m)
c
c --- PRISE called by: CAVITY2
c --- PRISE calls:     none
c----------------------------------------------------------------------
c
      real dtdz(6)
      real xdown(ndown),z(ndown),zeff(ndown)
c
      data dtdz/4*0.0,0.02,0.035/
      data halfpi/1.570796327/,wsmin/1.0/
c     scalar arguments (xnw,znw,zefflgl) are passed by 'call prise' to
c     dimensional constants (xdown, z, and zeff) with a value of 1 (ndown).
c     The following three lines were added as well as a change to the 
c     sub args to address a message citing this previous coding 
c     as a compiler error: 
      xdown(ndown) = xnw
      z(ndown) = znw
      zeff(ndown) = zefflgl
      
c
c --- Restrict very low wind speeds to avoid computational problems &
c --- stability class (1-6)
      u=amax1(ws,wsmin)
      jstab=min0(istab,6)
c
c --- Compute momentum flux, FM
      fm=ta*fmterm
c
c --- Compute buoyant flux, FB
      fb=fbterm*(ts-ta)
c
c --- Compute entrainment parameter, BJ
      bj=(0.3333333+u/exitw)
c
c --- Determine distance-independent parameters
      if(jstab.le.4)then
         tn1=3.*fm/(bj**2*u**2)
      else
         srt=sqrt(9.80616*dtdz(jstab)/ta)
         ts1=3.*fm/(bj**2*u*srt)
         ts2=srt/u
      endif
c
c --- Compute distance to final momentum rise, XMAX, and
c --- final plume rise height
      if(jstab.le.4)then
         xmax=4.*diam*(exitw+3.*u)**2/(exitw*u)
         zmax=(tn1*xmax)**0.3333333
      else
         xmax=halfpi*u/srt
         zmax=ts1**0.3333333
      endif
c
c --- Compute momentum plume rise and effective stack height at
c --- each downwind distance
      if(jstab.le.4)then
c
c ------ Neutral/unstable conditions
         do 100 i=1,ndown
         if(xdown(i).le.0.0)then
            z(i)=0.0
            zeff(i)=0.0
         else if(xdown(i).lt.xmax)then
            z(i)=((1.6)**3*fb*xdown(i)**2/u**3+tn1*xdown(i))**0.3333333
            zeff(i)=hs+z(i)
         else
            z(i)=((1.6)**3*fb*xdown(i)**2/u**3+zmax**3)**0.33333333
            zeff(i)=hs+z(i)
         endif
100      continue
      else
c
c ------ Stable conditions
         do 200 i=1,ndown
         if(xdown(i).le.0.0)then
            z(i)=0.0
            zeff(i)=0.0
         else if(xdown(i).lt.xmax)then
            z(i)=((1.6)**3*fb*xdown(i)**2/u**3+
     1            ts1*sin(ts2*xdown(i)))**0.3333333
            zeff(i)=hs+z(i)
         else
            z(i)=((1.6)**3*fb*xdown(i)**2/u**3+zmax**3)**0.3333333
            zeff(i)=hs+z(i)
         endif
200      continue
      endif
c      
c     added to pass results back through call statement as scalar
c     arguments:
      znw = z(ndown)
      zefflgl = zeff(ndown) 
c
      return
      end
c----------------------------------------------------------------------
      subroutine FRGAUSS(heff,sigma,hcrit,fractb,fracta)
c----------------------------------------------------------------------
c
c --- BDMOD      Version:  1.0     Level:  901201               FRGAUSS
c                J. Scire, SRC
c
c --- PURPOSE:  Compute the fraction of a Gaussian distribution below
c               at particular height
c
c --- INPUTS:
c             HEFF - real    - Effective stack height (m)
c            SIGMA - real    - Standard deviation (m) of the
c                              distribution
c            HCRIT - real    - Height up to which the distribution
c                              is to be integrated
c
c --- OUTPUT:
c           FRACTB - real    - Fraction of the Gaussain distribution
c                              below HCRIT
c           FRACTA - real    - Fraction of the Gaussain distribution
c                              above HCRIT
c
c --- FRGAUSS called by: CAVITY2
c --- FRGAUSS calls:     none
c----------------------------------------------------------------------
c
      data srt2/1.4142136/,small/1.e-5/
c
c --- Prevent numerical problems with very small sigmas
      sig=amax1(sigma,small)
c
      z=(hcrit-heff)/sig
c
      fractb=0.5*(1.+erf(z/srt2))
      fracta=1.0-fractb
c
      return
      end
c----------------------------------------------------------------------

