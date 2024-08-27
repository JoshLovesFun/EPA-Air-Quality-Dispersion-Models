      PROGRAM MAKEMET   ! DRAFT VERSION (DATED 16216)

C=======================================================================
C     PURPOSE:  To generate the matrix of meteorology for the AERMOD
C               model in a screening mode (AERSCREEN).
C     
C     PROGRAMMED BY: Roger W. Brode, MACTEC Federal Programs, Inc.
C
C     DATE:    April 12, 2006 (version dated 06102)
C
C     DESCRIPTION:
C
C     The program generates surface and profile meteorolgoical data files
C     for AERMOD based on a matrix of wind speeds, cloud covers, ambient
C     temperatures, solar elevation angles, and wstar's (conv.) for user-
C     specified surface characteristics (Bo, r, Zo).  For stable cases, a
C     loop through values of zimech is also included, using factors applied
C     to the value calculated based on ustar.  The zimech loop is included
C     to account for smoothing of zimech that occurs with refined AERMET.
C     Stable transition cases with solar angle greater than zero but
C     less than ACRIT are also included in the matrix.
C
C     The program calculates ustar, Monin-Obukhov length, and zimech
C     for each combination in the matrix, and calculates ziconv based on
C     wstar for convective cases.  The program uses subroutines from AERMET
C     to calculate boundary layer parameters for each combination in the
C     matrix.
C
C     This program prompts for:
C
C        surface filename
C        profile filename
C        minimum wind speed (m/s) (default of 0.5 m/s should be used)
C        anemometer height (m) (default of 10m should be used)
C        number of wind directions:
C           starting WD and WD increment if > 1 WD, or
C           assigned value for single WD
C        minimum and maximum ambient temperatures (K)
C        surface roughness length (m)
C        Bowen ratio
C        albedo
C
C     The program will generate a log file, called MAKEMET.LOG, that
C     summarizes the inputs selected for that run.
C
C     The program also allows for multiple sets of data to be appended
C     to a single set of surface and profile files, such as for
C     seasonal variations in temperature and surface characteristics.
C     The default starting year for the data is 10, and the year is
C     incremented by 10 for each additional data set generated.
C     Thus, for seasonal data files, the first season date will start
C     with a '1', second season will start with a '2', etc.
C     The hour is used to distinguish between stable and convective
C     conditions, with hours 01 through 11 indicating stable hours,
C     and hours 12 through 24 indicating convective hours.
C     Note that for monthly data sets and/or for large numbers of wind
C     directions (greater than 36), duplicate dates may be generated.
C     A warning message is written to the log file in these cases.
C
C     In addition to the required variables for input to AERMOD, the
C     surface file also includes five columns of integer variables
C     that provide the loop indices for each of the loops in the met
C     matrix corresponding to each "hour" of screening meteorology.
C     These indices can be used to analyze the frequency of occurrence
C     of various combinations within the meteorological matrix.
C
C     NOTE:  The prompts for minimum wind speed and anemometer height
C     are included for purposes of comparing screening to refined model
C     results, and may be removed later.  Defaults of 0.5 m/s for minimum
C     wind speed and 10m for anemometer height should be used for most
C     applications.
C
C=======================================================================
C
C     HISTORY:
C
C     The MAKEMET program was originally developed in 1995 by MACTEC
C     (formerly known as PES), Inc. under contract to the U.S. EPA.
C     The original draft included a limited matrix compared to the
C     current draft, and was set aside for some time as work on the
C     refined AERMOD model continued.  The original MAKEMET was later
C     modified by Herman Wong of U.S. EPA, Region 10, which included
C     expanding the matrix and providing additional flexibility.
C     The current version incorporates some of the changes introduced
C     by Herman Wong.  The original version and Herman Wong's modified
C     version include primary loops through wind speed and Monin-Obukhov
C     length.  The current version differs from the earlier version by
C     replacing the Monin-Obukhov length loop with loops through cloud
C     cover and solar elevation angle.  An ambient temperature loop was
C     also added, and further refinements were made to account for
C     transitional stable hours with the sun above the horizon but below
C     the critical elevation angle (ACRIT), and for smoothing of the
C     mechanical mixing height (zimech) performed by AERMET.
C
C=======================================================================
C
C     REVISIONS:
C
C     Version 16216:  Updated ustar adjustment calculations to match
C                     AERMET 16216
C     Version 15181:  Added ustar adjustment from AERMET
C
C                     Modified by James Thurman U.S. EPA/AQAD/AQMG
C                     July 8, 2015
C
C     Version 09183:  Added capability to include a minimum wind speed
C                     less than 0.5 m/s
C
C                     Modified by James Thurman U.S. EPA/AQAD/AQMG
C                     July 2, 2009
C
C     Version 07214: Modified subroutine NR_ANG to include bug fixes
C                    incorporated into AERMET, version 06341.  Included
C                    'SCREEN' as "version date" for header of surface
C                    file, consistent with AERMOD version 07214.
C                    Changed order of surface characteristic inputs to
C                    be consistent with AERMET, i.e., albedo, Bowen
C                    ratio, and surface roughness.  Changed order of
C                    surface and upper air IDs to match ASCREEN.
C                    Also output MAKEMET version date in on-site ID in
C                    surface file header record.
C
C                    Modified by: Roger W. Brode
C                                 U.S. EPA/OAQPS/AQMG
C                                 August 2, 2007
C
C=======================================================================
C
C     *******
C
C     EXCLUSIONS FROM THE MATRIX:
C
C        The only trap to eliminate potentially unrealistic combinations
C        of meteorology is to exclude cases when the convective mixing
C        height (ZICONV) is less than twice the heat flux (HFLUX) for
C        values of WSTAR greater than 0.1m/s, and to exclude cases when
C        the convective mixing height is less than heat flux for a value
C        of 0.1m/s for WSTAR.  These limits are based on empirical data
C        from several AERMET applications.
C
C        Additional combinations of meteorology have been excluded from
C        the matrix as being unnecessary, based on an analysis of the
C        frequency of occurrence of controlling meteorological conditions
C        from a large number of screening model runs.  These exclusions
C        are intended to optimize the number of meteorological simulations
C        required for the screening mode of AERMOD, while maintaining a
C        reasonable level of conservatism.
C
C     *******
C
C=======================================================================
C
C     DEFINITIONS:
C
C       WSPDS   - array of wind speeds for stable
C       WSPDU   - array of wind speeds for unstable
C       XOBUS   - Monin-Obukhov length for stable (calculated)
C       XOBUU   - Monin-Obukhov length for unstable (calculated)
C       WSTAR   - array of convective velocity scales
C       CCVR    - array of cloud covers
C       SOLANG  - array of solar elevation angles
C       SZIMECH - array of factors to apply to mechnical mixing heights
C                 during stable conditions to account for smoothing in
C                 the refined met data
C       ZIMAX   - maximum unstable mixing height
C       NWSPDS  - number of wind speeds under stable
C       NWSPDU  - number of wind sppeds under unstable
C       NWSTAR  - number of wstars
C       NCCVR   - number of cloud covers
C       NANG    - number of solar angles used for convective conditions
C       NAS     - number of solar angles used for stable conditions
C                 (including 0 for sun below the horizon)
C       NZI     - number of mechnical mixing height factors for stable
C       NTMP    - number of ambient temperatures (user-specified values)
C       XLAT    - latitude, required for the first record of the surface
C                 file, dummy value of 0 assigned
C       XLONG   - longitude, required for the first record of the surface
C                 file, dummy value of 0 assigned
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


C     Specify version date for MAKEMET program, VERSN
!      CHARACTER (LEN=5) :: VERSN = '09183'
      CHARACTER (LEN=5) :: VERSN = '16216'
C     Specify version "date" for use in header of surface file, METVER
      CHARACTER (LEN=6) :: METVER = 'SCREEN'


      LOGICAL  LFLAG1, LFLAG2,ADJU

      INTEGER  IYR, ICOUNT, NHOURS,nwspds1,nwspdu1
      INTEGER, PARAMETER :: IYR0 = 10
      INTEGER, PARAMETER :: NWSPDS=15, NWSPDU=15, NAS=3
      INTEGER, PARAMETER :: NWSTAR=5, NCCVR=3, NANG=4, NZI=3, NTMP=3
                                    
      REAL  WSPDS1(NWSPDS)
      real, allocatable, dimension(:) :: wspds
      real, allocatable, dimension(:) :: wspdu
      REAL  CCVR(NCCVR), SOLANG(NANG)
      REAL  SZIMECH(NZI)
      REAL  WSPDU1(NWSPDU), WSTAR(NWSTAR)
      REAL  USTAR, HTMIX, AMBT(NTMP), ROUGH
      REAL  HFLUX
      REAL  STAWD,WDINC,KWD

      REAL, PARAMETER :: KAPPA=0.4, RHO=1.2
      REAL, PARAMETER :: CP=1004.0, GRAV=9.80655

      CHARACTER  ANS*3, FNAME1*40, FNAME2*40
      CHARACTER  RUNDAT*8, RUNTIM*8,AU*1

CHW***Stable Wind Speeds
      DATA WSPDS1/  0.5,
     &             1.0,
     &             1.5,
     &             2.0,
     &             2.5,
     &             4.0,
     &             5.0,
     &             7.0,
     &            10.0,
     &            18.0,
     &            66.0,
     &            66.0,
     &            66.0,
     &            66.0,
     &            66.0 /
 
C     Assign Cloud Cover Array
      DATA CCVR /1., 7., 10./

C     Assign Solar Angle Array
      DATA SOLANG /15., 30., 60., 90./

C     Assign ZIMECH Factor Array
      DATA SZIMECH / 1., 5., 10./

CHW***Unstable wind speeds
      DATA WSPDU1/  0.5,
     &             1.0,
     &             1.5,
     &             2.0,
     &             3.0,
     &             4.0,
     &             6.0,
     &            10.0,
     &            66.0,
     &            66.0,
     &            66.0,
     &            66.0,
     &            66.0,
     &            66.0,
     &            66.0 /

C     Assign WSTAR Array
      DATA WSTAR/0.1, 0.3, 0.6, 1.2, 1.8/

      DATA ZIMAX/4000.0/, PI/3.14159/

      DATA VPTGZI, XLAT, XLONG/0.02, 0, 0 /

C     Open log file
      OPEN(UNIT=8, FILE='MAKEMET.LOG', STATUS='REPLACE')

C     Get Date and Time using system-specific functions  ---   CALL DATIME
      CALL DATIME (RUNDAT, RUNTIM)

CHW   PROMPT USER FOR LOCATION SPECIFIC INFORMATION
      WRITE(*,5)
    5 FORMAT(/' MAKEMET GENERATES MET DATA SET(S) PER USER-SPECIFIED'
     &       /' SURFACE CHARACTERISTICS AND TEMPERATURES.'/)

      WRITE(*,10)
   10 FORMAT(/' ENTER SFC MET FILE NAME  '/)
      READ(*,*) FNAME1

      WRITE(*,15)
   15 FORMAT(/' ENTER PFL MET FILE NAME  '/)
      READ(*,*) FNAME2

      WRITE(*,*) ' '
      WRITE(*,*) 'ENTER MIN. WS (M/S) '
      WRITE(*,*) ' '
      READ(*,*) WSMIN
c JAT set minimum wind speeds equal to entered min speed if entered
c speed < min wind speeds
      if (wspds1(1) .gt. wsmin) then
        allocate(wspds(NWSPDS+1))
        wspds(1)=wsmin
        do i=1,nwspds
          wspds(i+1)=wspds1(i)
        end do
        nwspds1=nwspds+1
      else
         allocate(wspds(NWSPDS))
         wspds=wspds1
         nwspds1=nwspds
      endif
      if (wspdu1(1) .gt. wsmin) then
        allocate(wspdu(NWSPDU+1))
        wspdu(1)=wsmin
        do i=1,nwspdu
          wspdu(i+1)=wspdu1(i)
        end do
        nwspdu1=nwspdu+1
      else
         allocate(wspdu(NWSPDU))
         wspdu=wspdu1
         nwspdu1=nwspdu
      endif
      WRITE(*,*) ' '
      WRITE(*,*) 'ENTER ANEM HT (M) '
      WRITE(*,*) ' '
      READ(*,*) ANEMHT

      ICOUNT = 1
c add ustar adjustment
 34   WRITE(*,33)
 33   FORMAT(/' ENTER OPTION TO ADJUST U* (Y=adjust,N=no adjustment)'/)
      READ(*,*) AU
      if (AU .EQ. 'Y' .or. AU .eq. 'y') THEN
          ADJU=.TRUE.
      ELSE IF (AU .EQ. 'N' .or. AU .EQ. 'n') then
          ADJU=.FALSE.
      ELSE
          GOTO 34
      ENDIF

C---- Open the output files: surface and profile
C     Write the header line to the surface file.

      OPEN(UNIT=10,FILE=FNAME1,status='replace')
      OPEN(UNIT=11,FILE=FNAME2,status='replace')
      if (ADJU) THEN
          WRITE (10,801) XLAT, XLONG, VERSN, METVER,'ADJ_U*'
      ELSE
          WRITE (10,800) XLAT, XLONG, VERSN, METVER
      ENDIF
  800 FORMAT (T5,F5.2,'N',T14,F6.2,'W', 8X,                                
     &                       '  UA_ID: ','00022222',
     &                       '  SF_ID: ','00011111',
     &                       '  OS_ID: ','MM_',A5,
     &                       T85,'VERSION:',T93,A6)
     
  801 FORMAT (T5,F5.2,'N',T14,F6.2,'W', 8X,                                
     &                       '  UA_ID: ','00022222',
     &                       '  SF_ID: ','00011111',
     &                       '  OS_ID: ','MM_',A5,
     &                       T85,'VERSION:',T93,A6,3X,A6)

CHW***05.14.03:  ADD FEATURE FOR MULTIPLE WIND DIRECTIONS
      WRITE(*,40)
   40 FORMAT(/' ENTER NUMBER OF WIND DIRECTIONS '/)
      READ(*,*) NUMWD

      IF (NUMWD .GT. 1 .AND. NUMWD .LE. 360) THEN
         WRITE(*,45)
   45    FORMAT(/' ENTER STARTING WIND DIRECTION '/)
         READ(*,*) STAWD

  710    WRITE(*,50)
   50    FORMAT(/' ENTER CLOCKWISE WIND DIRECTION INCREMENT'/)
         READ(*,*) WDINC

C        Repeat prompt if WDINC too small or too large
         IF (WDINC.LT.1 .OR. WDINC.GT.90) GOTO 710

      ELSE IF (NUMWD .EQ. 1) THEN
         WRITE(*,54)
   54    FORMAT(/' ENTER WIND DIRECTION '/)
         READ(*,*) STAWD
         WDINC = 0.0
      ELSE IF (NUMWD .LT. 1 .OR. NUMWD .GT. 360) THEN
         WRITE(*,*) 'INVALID ENTRY!  DEFAULT WD = 270 WILL BE USED. '
         WRITE(*,*)
         STAWD = 270.
         WDINC = 0.0
      END IF

      WRITE(8,9001) RUNDAT, RUNTIM, FNAME1, FNAME2,
     &              WSMIN, ANEMHT, NUMWD, STAWD, WDINC,AU
9001  FORMAT(T25,'MAKEMET LOG FILE',T60,A8,
     &     /,T25,'  (DATED 16216) ',T60,A8,
     &       //'SURFACE FILENAME: ',A40,
     &        /'PROFILE FILENAME: ',A40,
     &        /'MIN WS (M/S):    ',F7.2,
     &        /'ANEM HT (M):     ',F7.2,
     &        /'NUMBER OF WDs:   ',I7,
     &        /'STARTING WD:     ',F7.2,
     &        /'WD INCREMENT:    ',F7.2,
     &        /'ADJUST U*:             ',A1)

  705 CONTINUE

      NYRS = 0
      NHOURS = 0
      LFLAG1 = .FALSE.
      LFLAG2 = .FALSE.

      WRITE(*,25)
   25 FORMAT(/' ENTER MIN AND MAX AMBIENT TEMPS IN KELVIN '/)
      READ(*,*) AMBTmin, AMBTmax

      AMBT(1) = AMBTmin
      AMBT(2) = (AMBTmax + AMBTmin)/2.
      AMBT(3) = AMBTmax

      WRITE(*,30)
   30 FORMAT(/' ENTER ALBEDO '/)
      READ(*,*) ALBEDO
      
      WRITE(*,31)
   31 FORMAT(/' ENTER BOWEN RATIO '/)
      READ(*,*) BOWEN

      WRITE(*,32)
   32 FORMAT(/' ENTER SURFACE ROUGHNESS LENGTH IN METERS '/)
      READ(*,*) ROUGH


      

C*****Specify starting year of 10 for assigning dates
      IYR = IYR0 + 10*(ICOUNT-1)
      IF (IYR .GE. 100) THEN
         IYR = IYR - 100
         LFLAG1 = .TRUE.
      END IF
      IYRSAV = IYR
C
      WRITE(8,9002) ICOUNT, AMBTmin, AMBTmax, ROUGH, BOWEN, ALBEDO
9002  FORMAT(//'SUMMARY OF INPUTS FOR DATA SET NO. ',I3,
     &        /'  MIN AMB TEMP(K): ',F7.2,
     &        /'  MAX AMB TEMP(K): ',F7.2,
     &        /'  ROUGHNESS  (M):  ',F7.4,
     &        /'  BOWEN RATIO:     ',F7.3,
     &        /'  ALBEDO:          ',F7.3)

C---- Initialize nonchanging data

      TREF   = 2.0
      SIGV   = 99.0
      SIGW   = 99.0
      IEND   =  1 

CHW   21 JANUARY 2003:  FOR USE WITH HLIM AND CUBIC SUBROUTINE
      CDN = KAPPA / (ALOG(ANEMHT / ROUGH) )
      BETAM = 5.0

C---- WSTAR is not defined for the stable atmosphere
      WSTARS  = -9.0

C---- Initialize data that can change
      IHR    = 1
      JDAY   = 0
      MDAY   = 0 
      XOBUS  = 0
  

      
c      if (wspdu(1) .gt. wsmin) wspdu(1)=wsmin
c      write(*,*)wspds(1),wspdu(1)
c      pause
C---- Create meteorology for STABLE atmosphere:

      YOSPD_LOOP:  DO  NW = 1, NWSPDS1
         YOCCVR_LOOP:  DO  NC = 1, NCCVR

C----       Skip middle cloud cover (7) for stable based on frequency analysis
            IF (NC .EQ. 2) CYCLE YOCCVR_LOOP

               YOTEMP_LOOP:  DO NT = 1, NTMP

                  YOANG_LOOP: DO NA = 1, NAS


C----    Skip middle temperature and certain cases based on frequency analysis

         IF (NT .EQ. 2) CYCLE YOTEMP_LOOP
         IF ((WSPDS(NW).GT.4.0) .AND. (NT.NE.NC)) CYCLE YOTEMP_LOOP


CHW***BYPASS UNUSED VALUES IN ARRAY        
         IF (WSPDS(NW).EQ.66.) GOTO  6660
C----    Skipped wind speeds below user-specified minimum
         IF (WSPDS(NW).LT.WSMIN) GOTO  6660

C----    Assign solar angle for stable conditions, including 0 for sun
C        below the horizon and two cases for sun above horizon but below
C        critical solar angle (ACRIT)

         IF (NA .EQ. 1) THEN
            ANGLE = 0.0
            ACRIT = 0.0

         ELSE IF (NA .GT. 1) THEN

            ANGLE = 10.
            ACRIT = 0.0
            ALDO = ALBEDO
            CALL NR_ANG(ALDO, angle, ccvr(nc), AMBT(NT), acrit)

C----       Assign solar angle to be specified fraction of ACRIT
            IF (NA .EQ. 2) THEN

               ANGLE = 0.4 * ACRIT

            ELSE IF (NA .EQ. 3) THEN

               ANGLE = 0.95 * ACRIT

            END IF

C----       Adjust the albedo for later use using the average solar angle:
            ALDO = ALBEDO
            B1 = 1 - ALDO

            IF( ANGLE .LE. 0.0 )THEN      !  Set the nighttime albedo to 1.0

               ALDO = 1.0

            ELSE                          !  Adjust albedo for solar elevation

               ALDO = ALDO + B1*EXP(-0.1*ANGLE+(-0.5*B1*B1))      ! Eq. (3)
            ENDIF

         END IF

C----    Call routine to calculate stable ustar and theta-star for stable
C        conditions
c     15188 add ustar adjustment from AERMET 15181
        CALL UCALST(ADJU,WSPDS(NW),ANEMHT,AMBT(NT),ROUGH,CCVR(NC),ANGLE,
     &               ACRIT,USTAR,THSTAR)
!         CALL UCALST(WSPDS(NW),ANEMHT,AMBT(NT),ROUGH,CCVR(NC),ANGLE,
!     &               ACRIT,USTAR,THSTAR)


CHW   21 JANUARY 2003: FROM AERMET V02222, UCALST.FOR
C------- For the case of strong winds, the product USTAR*THSTAR,
C        and thus HFLUX, may become unrealistically large.
C        To avoid this situation, the heat flux (HLIM) is limited
C        to a minumum of -64 Watts/meter**2.
C        XLIM is the kinematic heat flux (m/s * K)

         HLIM = -64.0
         XLIMIT = -HLIM / (RHO * CP)
         IF( (USTAR * THSTAR) .GT. XLIMIT )THEN
            AA = -CDN * WSPDS(NW)
            BB = 0.0
            CC = BETAM * ANEMHT * GRAV * XLIMIT * CDN / AMBT(NT)
            CALL CUBIC ( AA, BB, CC, USTROUT )

            IF(USTROUT .NE. -9.)THEN      ! Recalculate THSTAR
               USTAR  = USTROUT
               THSTAR = XLIMIT / USTAR
            ELSE    ! Keep USTAR, and Recalculate THSTAR to give H = HLIM
C
               THSTAR = -HLIM /(RHO*CP*USTAR)
            ENDIF

         ENDIF

C----    Calculate Monin-Obukhov length (XOBUS) for stable conditions
         XOBUS = AMBT(NT) * USTAR * USTAR /
     &            ( KAPPA * GRAV * THSTAR )                  !MFD Eqs. 8 & 25

         HFLUX  = -THSTAR*RHO*CP*USTAR     ! Eq. (25)

         IF(XOBUS.GT.8888.) XOBUS=8888.
         IF(XOBUS.LT.1.0) XOBUS=1.0

         IF(HFLUX.LT.-64.0) HFLUX = -64.0
 
CHW      EQN 24 IN MFD
         ZIMECH = 2300. * USTAR**1.5

         IF(ZIMECH.GT.4000) ZIMECH=ZIMAX

         ZIMECH1 = ZIMECH

         ZIMECH_LOOP:  DO L = 1, NZI

            ZIMECH = ZIMECH1 * SZIMECH(L)

C           Skip cases with ZIMECH > 400m when ZIM-factor is > 1
            IF (L .GT. 1 .AND. ZIMECH.GT.400.0) GO TO 6660
                 
C           Specify starting direction and loop through directions.
C           If NUMWD = 1, use 270 degrees by default.
            KWD  = STAWD - WDINC

            YOWD_LOOP: DO K = 1, NUMWD
      
               IF(NUMWD.GT.1)THEN
                 KWD = KWD + WDINC
                 IF(KWD.GT.360)KWD = KWD - 360
                 WD  = KWD
               ELSE
                 WD  = STAWD
               ENDIF

               JDAY = JDAY + 1
               MDAY = MDAY + 1
		 
CHW***05.14.03:  INCREMENT BY ONE HOUR IF MORE THAN 365 MET CONDITIONS 
C       IN SAME YEAR WITH A MAX OF 1998 HOURS; IF MORE THAN 1998 HOURS,
C       THE PROGRAM WILL INCREMENT UP ONE YEAR AND SO ON.  IF PROGRAM 
C       WILL BE GENERATING LOTS OF HOURS WHETHER IT BE BECAUSE OF USING 
C       SEASONAL OR MONTHLY MET, OR INCREMENTING WIND DIRECTIONS
C       EVERY ONE DEGREE, START YEAR AT LOW NUMBER, E.G. 10.

               JDAYMX = 365
               IF (MOD(IYR,4)   .EQ. 0) JDAYMX=366
               IF (MOD(IYR,100) .EQ. 0) JDAYMX=365
               IF (MOD(IYR,400) .EQ. 0) JDAYMX=366

               IF (JDAY.GT.JDAYMX) THEN
                  IHR = IHR + 1
                  JDAY = 1
               END IF
               IF (IHR .GT. 11) THEN
                  IHR = 1
                  IYR = IYR + 1
                  IF (IYR .GE. 100) THEN
                     IYR = IYR - 100
                     LFLAG1 = .TRUE.
                  END IF
                  NYRS = NYRS + 1
                  IF (NYRS .GE. 10) THEN
                     LFLAG2 = .TRUE.
                  END IF
               END IF

               CALL GREGOR(IYR, JDAY, IMO, IDA)
               ZICONV = -999.0

               NHOURS = NHOURS + 1

               WRITE( 10, 1000) IYR, IMO, IDA, JDAY, IHR, HFLUX, USTAR,
     &                          WSTARS, VPTGZI, ZICONV, ZIMECH, XOBUS,
     &                          ROUGH, BOWEN, ALBEDO, WSPDS(NW), WD,
     &                          ANEMHT, AMBT(NT), TREF,
     &                          NW, NC, NT, NA, L
 1000          FORMAT( 3(I2,1X), I3,1X, I2,1X, F10.2,1X, F6.3,1X, F6.3,
     &                 1X, F6.3,1X, 2(F10.0,1X),
     &                 F8.1,1X, F6.4,1X, F6.2,1X, F6.2,1X, F7.2,1X,
     &                 F5.0, 3(1X,F6.1), 5I4 )

               WRITE( 11, 1100 ) IYR, IMO, IDA, IHR, ANEMHT, IEND, WD,
     &                           WSPDS(NW), AMBT(NT)-273.16, SIGV, SIGW
 1100          FORMAT(4(I2,1X), F6.1,1X, I1,1X, F5.0,1X, F7.2,1X,
     &                F7.1,1X, F6.1,1X, F7.2)

            ENDDO YOWD_LOOP

         ENDDO ZIMECH_LOOP
    
 6660    CONTINUE

               ENDDO YOANG_LOOP
            ENDDO YOTEMP_LOOP
         ENDDO  YOCCVR_LOOP
      ENDDO  YOSPD_LOOP


C---- Reinitialize unchanging data for unstable hours
      SBLHT = -999.0

CHW   Reinitialize data that can change
      IYR  = IYRSAV
      IHR  = 12
      JDAY = 0
      MDAY = 0

C---- Create meteorology for UNSTABLE atmosphere:
                                        
      SPD_LOOP:   DO NW = 1, NWSPDU1
         WSTAR_LOOP: DO NWST = 1,NWSTAR
            CCVR_LOOP: DO NC = 1,NCCVR
              TEMP_LOOP: DO NT = 1, NTMP
                 ANG_LOOP:   DO NA = 1,NANG

                    ANGLE = SOLANG(NA)

CHW***BYPASS UNUSED VALUES IN ARRAY
         IF(WSPDU(NW).EQ.66.) CYCLE SPD_LOOP
         IF(WSPDU(NW).LT.WSMIN) CYCLE SPD_LOOP

C        Skip lower WSTARs for higher Wind Speeds based on frequency analysis
         IF (WSPDU(NW).GT.1.5 .AND. NWST.LT.3) CYCLE WSTAR_LOOP

         ALDO = ALBEDO
         CALL NR_ANG(ALDO, angle, ccvr(nc), AMBT(NT), acrit)


C----    Adjust the albedo for later use using the average solar angle:

         ALDO = ALBEDO
         B1 = 1 - ALDO

         IF( ANGLE .LE. 0.0 )THEN      !  Set the nighttime albedo to 1.0

            ALDO = 1.0

         ELSE                          !  Adjust albedo for solar elevation

            ALDO = ALDO + B1*EXP(-0.1*ANGLE+(-0.5*B1*B1))      ! Eq. (3)
         ENDIF

C----    Check for solar angle less than acrit and adjust
         IF(ANGLE .LT. ACRIT .AND. NA .EQ. 1)THEN
            ANGLE = (ACRIT + SOLANG(NA+1))/2.
         ELSE IF (ANGLE .LT. ACRIT .AND. NA .GT. 1) THEN
            CYCLE ANG_LOOP
         ENDIF

         CALL INCRAD(ANGLE,ccvr(nc),QR)

         CALL NETRAD(CCVR(NC),ALDO,AMBT(NT),QR,RN)

         CALL HEAT(RN,BOWEN,HFLUX)

         IF (HFLUX .LT. 0.0) GO TO 6666

C----    Call routine to calculate stable ustar and Monin-Obukhov length
C        (XOBUU) for convective conditions

         CALL UCALCO(WSPDU(NW),ANEMHT,AMBT(NT),ROUGH,HFLUX,USTAR,XOBUU)

C----    Apply limits on Monin-Obukhov length
         IF (XOBUU .LT. -8888.0)  XOBUU = -8888.0
         IF (XOBUU .GT. -1.0)  XOBUU = -1.0


CHW      EQN 12 IN MFD
         ZIMECH = 2300. * USTAR**1.5

CHW      12 AUGUST 2002
         IF (ZIMECH.GT.3000) go to 6666

CHW      EQN 8 AND 10 IN MFD

         ZICONV = -((WSTAR(NWST)/USTAR)**3) * KAPPA * XOBUU

CHW      20 JANUARY 2003
         IF (ZICONV.GT.3000) GOTO 6666

C----    Check for unrealistically low ZICONV vs. HFLUX, based on empirical data
         IF (NWST .EQ. 1) THEN
            IF (ZICONV .LT. HFLUX) GO TO 6666
         ELSE
            IF (ZICONV .LT. 2.*HFLUX) GO TO 6666
         END IF

C----    Specify starting direction and loop through directions.
         KWD = STAWD - WDINC

         WD_LOOP: DO K = 1, NUMWD

            IF (NUMWD.GT.1) THEN
              KWD = KWD + WDINC
              IF (KWD.GT.360) KWD = KWD - 360
              WD  = KWD
            ELSE
              WD = STAWD
            ENDIF

            JDAY = JDAY + 1
            MDAY = MDAY + 1


CHW***05.14.03:  INCREMENT BY ONE HOUR IF MORE THAN 365 MET CONDITIONS 
C       IN SAME YEAR WITH A MAX OF 1998 HOURS; IF MORE THAN 1998 HOURS,
C       THE PROGRAM WILL INCREMENT UP ONE YEAR AND SO ON.  IF PROGRAM 
C       WILL BE GENERATING LOTS OF HOURS WHETHER IT BE BECAUSE OF USING 
C       SEASONAL OR MONTHLY MET, OR INCREMENTING WIND DIRECTIONS
C       EVERY ONE DEGREE, START YEAR AT LOW NUMBER, E.G. 10.

            JDAYMX = 365
            IF (MOD(IYR,4)   .EQ. 0) JDAYMX=366
            IF (MOD(IYR,100) .EQ. 0) JDAYMX=365
            IF (MOD(IYR,400) .EQ. 0) JDAYMX=366

            IF (JDAY.GT.JDAYMX) THEN
               IHR = IHR + 1
               JDAY = 1
            END IF
            IF (IHR .GT. 24) THEN
               IHR = 12
               IYR = IYR + 1
               IF (IYR .GE. 100) THEN
                  IYR = IYR - 100
                  LFLAG1 = .TRUE.
               END IF
               NYRS = NYRS + 1
               IF (NYRS .GE. 10) THEN
                  LFLAG2 = .TRUE.
               END IF
            END IF

            CALL GREGOR(IYR, JDAY, IMO, IDA)

            NHOURS = NHOURS + 1

            WRITE( 10, 1000) IYR, IMO, IDA, JDAY, IHR, HFLUX,
     &                 USTAR, WSTAR(NWST), VPTGZI, ZICONV, ZIMECH,
     &                 XOBUU, ROUGH, BOWEN, ALBEDO, WSPDU(NW),
     &                 WD, ANEMHT, AMBT(NT), TREF,
     &                 NW, NC, NT, NA, NWST

            WRITE( 11, 1100 ) IYR, IMO, IDA, IHR, ANEMHT, IEND,
     &                        WD, WSPDU(NW), AMBT(NT)-273.16, SIGV,
     &                        SIGW

         ENDDO WD_LOOP


 6666    CONTINUE

                  ENDDO ANG_LOOP
               ENDDO TEMP_LOOP
            ENDDO CCVR_LOOP
         ENDDO WSTAR_LOOP
      ENDDO SPD_LOOP
      
CHW   CYCLE PROGRAM FOR MULTIPLE DATASETS (E.G. SEASONS)
      WRITE(*,55)
   55 FORMAT(/' DO YOU WANT TO GENERATE ANOTHER MET SET THAT WILL BE',
     &       /' APPENDED TO CURRENT FILE?',
     &       /' [TYPE EITHER "Y" OR "y" FOR YES; OR HIT "ENTER" EXIT]')
      READ(*,905) ANS
  905 FORMAT(A)

      IF (ANS.EQ."Y".OR.ANS.EQ."y") THEN
C        Write number of "hours" of met data to log file and check for
C        possible duplicate dates.
         WRITE(8,9004) NHOURS
9004     FORMAT(/'  NUMBER OF MET DATA COMBINATIONS: ', I8)
         IF (LFLAG1 .OR. LFLAG2) THEN
            WRITE(8,9005)
9005        FORMAT(/'  WARNING!!!  DUPLICATE DATES MAY OCCUR!')
         END IF
         ICOUNT = ICOUNT + 1
         GOTO 705
      ELSE
         WRITE(8,9004) NHOURS
         IF (LFLAG1 .OR. LFLAG2) THEN
            WRITE(8,9005)
         END IF
      END IF

C     Close surface and profile files
      CLOSE(10)
      CLOSE(11)

      STOP
      END


      SUBROUTINE GREGOR( YEAR,JDAY,MNTH,MDAY )
C=====================================================================**
C          GREGOR Module of the AERMOD SCREEN met generator
C
C  Purpose:  To compute the gregorian month and day given the year
C            and julian day
C
C-----------------------------------------------------------------------
C
      INTEGER*4 YEAR,JDAY,MNTH,MDAY,L,J
C
C      YEAR = CALENDAR YEAR
C      JDAY = DAY OF YEAR -- 1, 365, OR 366
C      MNTH = MONTH OF YEAR -- 1, 12
C      MDAY = DAY OF MONTH -- 1, 31
C
C-----------------------------------------------------------------------
      L=365

      IF(MOD(YEAR,4).EQ.  0) L=366
      IF(MOD(YEAR,100).EQ.0) L=365
      IF(MOD(YEAR,400).EQ.0) L=366

      J=MOD(JDAY+305,L)
      J=MOD(J,153)/61+(J/153)*2+J
      MNTH=MOD(J/31+2,12)+1
      MDAY=MOD(J,31)+1

      RETURN
      END


CHW   21 JANUARY 2003

      SUBROUTINE CUBIC( A,B,C,Z )
C=====================================================================**
C          CUBIC Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  To solve a cubic equation to get the proper solution for
C               the friction velocity (UST), where the cubic equation in
C               this case looks similar to: Z**3 + A*Z**2 + B*Z + C = 0.
C
C     Calling Arguments:
C        A         Input     Real      Coefficient of UST**2
C        B         Input     Real      Coefficient of UST**1
C        C         Input     Real      Coefficient of UST**0
C        Z         Output    Real      New friction velocity
C
C     Initial release:  December 1992
C
C     Maintained by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C     Revision History:
C        <none>
C
C-----------------------------------------------------------------------

C---- Solve for one root of the cubic equation.

       DOUBLE PRECISION CM,SGN,A3,AP,BP,AP3,TROOT,BP2,APP,BSV
       DOUBLE PRECISION ALPHA,SIGN,TR,BPP,ONE
       REAL A,B,C,Z
       DATA ONE/1.0/

       A3=A/3.
       AP=B-A*A3
       BP=2.*A3**3-A3*B+C
       AP3=AP/3.
       BP2=BP/2.
       TROOT=BP2*BP2+AP3*AP3*AP3
       IF(TROOT.LE.0.0) GO TO 150
       TR=DSQRT(TROOT)

CHW   23 JANUARY 2003
CHW   ELIMINATE IN APP CALCULATION BELOW TAKING THE ROOT OF NEGATIVE NUMBER
      IF(TR.GT.BP2)GOTO 140
      IF(BP2.GE.TR)Z=-9
      RETURN

 140  CONTINUE

       APP=(-BP2+TR)**0.333333
C     OPEN(18,FILE='APP+.DUM',STATUS='UNKNOWN')
C     WRITE(18,*)APP, BP2, TR
       BSV=-BP2-TR
      IF(BSV.EQ.0.0)GO TO 145
       SGN=DSIGN(ONE,BSV)
       BPP=SGN*(DABS(BSV))**0.333333
       Z=APP+BPP-A3
       RETURN

 145   CONTINUE
       Z=APP-A3
       RETURN

 150   CM=2.*DSQRT(-AP3)
       ALPHA=DACOS(BP/(AP3*CM))/3.
       Z=CM*DCOS(ALPHA)-A3
       RETURN
       END


!      SUBROUTINE UCALST(WSPD,ZREF,T,Z0,CCVR,ANGLE,ACRIT,USTAR,THSTAR)
      SUBROUTINE UCALST(ADJ_USTAR,WSPD,ZREF,T,Z0,CCVR,ANGLE,ACRIT,
     +USTAR,THSTAR)
C=====================================================================**
C     UCALST Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  Calculate USTAR (friction velocity) and THSTAR (the
C               temperature scale for profiling).  A check is used to see
C               if the solution to USTAR will be either real or complex.
C               If the solution is real, the computation for USTAR
C               follows the solution proposed by Venkatram.  If the
C               solution is complex, a linear interpolation, used by
C               van Ulden and Holtslag, is applied to find the solution.
C
C     Input:
C      IHR       Integer   Hour of day
C      ANGLE     Real      Solar elevation angle

C      ACRIT     Real      Solar elevation angle above which net radiation
C                          is positive

C      WSPD      Real      Wind speed at reference height
C      ZO        Real      Surface roughness length
C      ZREF      Real      Reference height for wind speed
C      CCVR      Real      Cloud cover
C      T         Real      Ambient temperature at reference height
C
C     Output:
C      USTAR     Real      Surface friction velocity
C      THSTAR    Real      Temperature scale for profiling
C
C     Initial release: December 1992
C
C     Maintained by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C     Code which can be traced to an equation or equations in the AERMOD
C     Model Formulation Document is indicated by including the equation
C     number in the right hand margin of the record; e.g., ! Eq (1)
C
C
C     Revision history:
C        07/30/95 (PES)
C          - added the computation for theta_star when the solution to
C            the quadratic equation is complex-valued;
C
C        09/05/97 (PES)
C          - changed the constant BETAM from 4.7 to 5.0
c
c        04/24/01  (JSI)
c          - added code implementing Holtslag's correction term for theta-star.
c
c        05/01/01  (RJP)
c          - altered treatment of case in stable conditions where the
c            quadratic equation has no real solution.  Although u* and 
c            theta* may vanish at low speeds, the manner in which they
c            both vary is currently assumed to be linear with speed below
c            the threshold required for a real solution, which may not be
c            correct, and which may lead to unrealistically low M-O lengths. 
c            Furthermore, many low speed measurements are questionable 
c            because they may be near or below the instrument starting speed.  
c            Instead of having u* and theta* vanish at low wind speeds, 
c            theta* is retained in this treatment and a minimum 
c            u* equivalent to the lowest wind speed leading to a real 
c            solution is used.

c        05/18/01  (JSI)
c          - revised implementation of theta-star correction so that it applies
c            only for low solar elevations.
C
c        08/14/01  (DTB)
c          - removed code for the calculation of heat flux;  redundent code
c            is located in the calling program MPPBL.
c
c        08/28/01  (PES, RWB)
c          - reverted to original linear scaling for estimating ustar
c            and thstar for cases when quadratric equation has no real
c            solution.
c
c        07/23/04  (MACTEC/PES, RWB)
c          - included check for missing wind speed.
C
C-----------------------------------------------------------------------

      INTEGER  IHR, JJJ
      REAL     THSTR1,AA,BB,CC,CDN,UNOT,VONK,G
      REAL     CP, CHEK, UCR, USTCR, BETAM, HLIM, XLIMIT
      REAL     ANGLE, ACRIT
      LOGICAL ADJ_USTAR
C     VONK   = von Karman constant
C     GRAV   = acceleration due to gravity
C     BETAM  = constant used for profile relationships in the SBL
C     CP     = specific heat capacity of dry air, and

      DATA VONK/0.4/,  GRAV/9.80655/

      CP = 1004.0
      IF (ADJ_USTAR) THEN
          BETAM=4.7
      ELSE
          BETAM = 5.0
      ENDIF

ctmp  Skip AERMET code not needed for MAKEMET:
ctmp      IF( WSPD .EQ. 0.0 )THEN     ! Calm, Set USTAR and THSTAR to missing
ctmp         USTAR  = -9.
ctmp         THSTAR = -9.
ctmp
ctmp      ELSEIF (WSPD.EQ.FLOAT(OSQA(23,2)))THEN ! Missing wind speed
ctmp         THSTAR =  -9.
ctmp         USTAR  =  -9.
ctmp         MESS =  BLNK80
ctmp         ECODE='W-1'
ctmp         WRITE(MESS,490) IHR
ctmp  490 FORMAT(' Missing wind speed for hour: ', I2.2)
ctmp         CALL ERRHDL(JJJ,PATH,ECODE,LOC,MESS)
ctmp
ctmp      ELSE

C 15188 JAT ADD USTAR ADJUSTMENT FORM 15181 AERMET UCALST
      IF (ADJ_USTAR) THEN
          CDN = VONK /(ALOG( (ZREF-5.*Z0)/Z0 ) ) 
C ---    Use constant THSTAR = 0.08 per Qian and Venkatram (2011)
         THSTR1 = 0.08

         IF( ANGLE.GT.0.0 .and. ANGLE.LT.ACRIT )THEN                    ! jsi033 01138
 
C           Correct TSTAR1 for low solar elevation; see Holtslag(1984),     ! jsi031 01114
c           BLM(29):225-350  Equation A-11                                  ! jsi031 01114
            THSTR1 = THSTR1*(1.0 - (ANGLE/ACRIT)**2)                        ! jsi031 01114
         ENDIF                                                          ! jsi033 01138
          THSTAR = THSTR1
          UNOT = SQRT( (BETAM * (ZREF-5.*Z0-Z0) * GRAV * 
     &                                           THSTAR)/T )
C  The equation for UNOT (U0) is from Q&V eqn. 22, where the zero-plane displacement is now 4 * Z0.

C        Check to see if USTAR has a real or imaginary solution by
C        checking the square root part (CHEK) of the USTAR equation.

         CHEK = ( ( 2.0 * UNOT) / (SQRT( CDN ) * WSPD ) )**2
C  CHEK is r^2, r is defined in Q&V eqn. 20.

C        If the solution to USTAR is real, proceed with the computation
C        of USTAR.

C  This next equation in the Version 12345 code is Q&V eqn. 26, which is “tentative", used for all
C  conditions (real & imaginary) for no discontinuity

           
         USTAR = (CDN * WSPD / 2.0) * 
     &                ( (1.0 + EXP(-1.0*CHEK/2.0)) /
     &                  (1.0 - EXP(-2.0/SQRT(CHEK))) )
C ---  Removed comparison against CHEK, as u* equation above is always valid, but
C ---  not supposed to be greater than USTCR

         UCR = (2.0 * UNOT) / SQRT(CDN)         ! added 1/8/13  aecom
         USTCR = CDN * UCR / 4.0                ! added v15181  EPA
C ---       Use larger of USTCR and original USTAR      
         USTAR = MAX( USTCR, USTAR )  ! added v15181  EPA

         IF( CHEK.GT.1.0 )THEN
            THSTAR = THSTR1 * WSPD / UCR ! added 1/8/13  aecom    
         ENDIF
         
      ELSE !no adjustment
         CDN = VONK /(ALOG( ZREF/Z0 ) )          !  See note to Eq. 20

         THSTR1 = 0.09 * (1.0 - 0.5*( (CCVR/10.0)**2) )         ! Eq. (24)

         IF( ANGLE.GT.0.0 .and. ANGLE.LT.ACRIT ) THEN                    #jsi033 01138

C        Correct TSTAR1 for low solar elevation; see Holtslag(1984),     #jsi031 01114
c        BLM(29):225-350  Equation A-11                                  #jsi031 01114
         THSTR1 = THSTR1*(1.0 - (ANGLE/ACRIT)**2)                        #jsi031 01114

         END IF                                                          #jsi033 01138

         THSTAR = THSTR1
         UNOT = SQRT( (BETAM * ZREF * GRAV * THSTAR)/T )

C        Check to see if USTAR has a real or imaginary solution by
C        checking the square root part (CHEK) of the USTAR equation.

         CHEK = ( ( 2.0 * UNOT) / (SQRT( CDN ) * WSPD ) )**2

C        If the solution to USTAR is real, proceed with the computation
C        of USTAR.

         IF( CHEK.LE.1.0 )THEN                   !  Real solution

            USTAR = (CDN * WSPD / 2.0) *
     &                   (1.0 + SQRT(1.0-CHEK))

         ELSE                                    !  Imaginary solution

c           For the imaginary solution, we define critical values for wind speed
c           (UCR) and friction velocity (USTCR); we then scale USTCR with the
c           observed wind speed divided by UCR to obtain u*.  We apply the same
c           scaling to obtain  theta*.   Reverted to original treatment: #rwb038 01240

            UCR = (2.0 * UNOT) / SQRT(CDN)
            USTCR = CDN * UCR / 2.0
            USTAR = USTCR * WSPD / UCR
            THSTAR = THSTR1 * WSPD / UCR

         ENDIF
      ENDIF
ctmp      ENDIF

      RETURN
      END


      subroutine nr_ang(albedoin,angle, ccvrin,tempin, acrit)           #jsi030 01110

c     Compute the critical solar elevation angle (acrit) at which net radiation
c     is theoritically zero.  Routine has been modified to perform iterative
c     computation since acrit is a function of albedo and albedo is a function
c     of solar angle.  R.W. Brode, MACTEC/PES, 07/23/04

c     it is assumed that none of the passed variables are 'missing'.

c     Modified 12/07/2006                                               ! rwb #522 06341
c        Added variable BA1 to resolved variable name conflict with b1. ! rwb #522 06341
c        Added check for sinacrit < 0 and assigned acrit=0 for such     ! rwb #522 06341
c        cases.                                                         ! rwb #522 06341

c     Called by:  MPPBL

ctmp      integer ccvrin, iter                     ! ccvrin is REAL in MAKEMET
      integer iter
      real sky, acrit, alb, tt                                          #dtb105 02023
      real lastacrit, eps

      sb = 5.67e-08
      c1 = 5.31e-13
      c2 = 60.0
      b1 = 0.75
      b2 = 3.4
      a1 = 990.0
      a2 = 30.0
      pi = 3.14159
      rad2deg = 180.0/pi

      iter = 0
      eps = 0.01

c     Adjust albedo initially based on solar angle
      BA1 = 1. - albedoin                                               ! rwb #522 06341

      IF( ANGLE .LE. 0.0 )THEN      !  Set the nighttime albedo to 1.0

         ALB = 1.0
      ELSE                          !  Adjust albedo for solar elevation
                                                                        ! rwb #522 06341
         ALB = albedoin + BA1 * EXP(-0.1*ANGLE + (-0.5*BA1*BA1))           ! Eq. (3)
                                                                        ! rwb #522 06341
      ENDIF


c     As necessary we replace missing value flags (for cloud cover
c     and temperature) with fixed values for use locally.


      if(ccvrin .eq. 99)then                                            #dtb105 02023
         sky = 5.                                                       #dtb105 02023
      else                                                              #dtb105 02023
ctmp         sky = float(ccvrin)                                            #dtb105 02023
         sky = ccvrin
      endif                                                             #dtb105 02023

      if(tempin.eq.-999 .or. tempin.eq.999)then                         ! dtb #515  06305
         tt = 288.                                                      #dtb111 02045
      else                                                              #dtb111 02045
         tt = tempin                                                    #dtb111 02045
      endif                                                             #dtb111 02045

      if(alb.lt.1.0 .and. alb.ge.0.0) then

c        Initialize acrit = angle
         acrit = angle
         lastacrit = 0.

c        Peform iteration to calculate acrit, with limit of 20          #rwb400 04205
c        iterations                                                     #rwb400 04205
         do while (abs(acrit-lastacrit).gt.abs(eps*acrit) .and.         #rwb400 04205
     &                                      iter .le. 20)               #rwb400 04205

            iter = iter + 1                                             #rwb400 04205
            lastacrit = acrit
            term1 = sb*tt**4.0 - c1*tt**6.0 - c2*sky/10.0               #dtb111 02045
            term2 = (1.0-b1*(sky/10.0)**b2)*(1.0-alb)*a1                #dtb111 02045
            
            sinacrit = term1/term2 + a2/a1

            if(sinacrit .le. 0.0) then
c              Assign acrit = 0 and exit loop                           ! rwb #522 06341
               acrit = 0.0                                              ! rwb #522 06341
               exit                                                     ! rwb #522 06341
            elseif(sinacrit .gt. 1.0) then
c              Assign acrit = 92 and exit loop                          ! rwb #522 06341
               acrit = 92.
               exit                                                     ! rwb #522 06341
            else
               acrit = rad2deg*asin(sinacrit)
            endif

c           Adjust albedo to current value of acrit
            IF( ACRIT .LE. 0.0 )THEN                                    ! rwb #522 06341
c              Program should not reach this point;                     ! rwb #522 06341
c              just in case assign acrit=0 and exit                     ! rwb #522 06341
               acrit = 0.0                                              ! rwb #522 06341
               exit                                                     ! rwb #522 06341

            ELSE                          !  Adjust albedo for solar elevation
                                                                        ! rwb #522 06341
               ALB = albedoin + BA1 * EXP(-0.1*ACRIT + (-0.5*BA1*BA1))      ! Eq. (3)
                                                                        ! rwb #522 06341
            ENDIF

         END DO

      elseif( alb.ge.1.0 ) then

         acrit = 94.

      endif

      return
      end


      SUBROUTINE INCRAD(ANGLE,CCVR,QR)
C======================================================================
C     INCRAD Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  To compute the total incoming solar radiation from
C               fractional cloud cover and solar elevation angle.
C
C     Calling Arguments:
C             IHR      In    Integer   In  Hour of day
C
C     Called by:  MPPBL
C
C     Calls to:   none
C
C     Maintained by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C     Revision History:
C        <none>
C
c         05/18/01  (JSI)
c          - added code to check for solar elevation angle less than 
c            ArcSin(30/990) = 1.74 degrees
c
c         08/28/01 (PES, RWB)
c          - coverted angle from degrees to radians (ANGLE/CONST) for 
c            SIN argument.
C
C     Code which can be traced to an equation or equations in the AERMOD
C     Model Formulation Document is indicated by including the equation
C     number in the right hand margin of the record; e.g., ! Eq (1)
C
C
C-----------------------------------------------------------------------

C     ANGLE  is the elevation angle of the sun in degrees.  The average hourly 
c            value is calculated in MPPBL as the mean of the elevation angles 
c            for the current and previous hours, since hour is interpreted as
c            the end of the observation period.                          #rwb038 01240


C     Data declarations

      REAL ANGLE, SKY                                                    #dtb105 02123

      CONST = 57.2958

ctmp      INCLUDE 'WORK1.INC.'                                               #dtb126 02107
ctmp      INCLUDE 'MP2.INC'
      
ctmp      PATH = 'METPREP '
ctmp      LOC  = 'INCRAD'


c     The solar insolation is set to zero for solar elevations           #jsi030 01110
c     less than ArcSin (30/990) = 1.74.                                  #jsi030 01110

      IF(ANGLE .LE. 1.74)THEN                                            #jsi030 01110

         QR = 0.0

      ELSE
         
         QR=  990.0*SIN(ANGLE/CONST) - 30.0           ! Eq. (5)     #rwb038 01240

C        Adjust for cloud cover (CCVR).
         SKY = CCVR
         QR=QR*(1.0 - 0.75*((SKY/10.0)**3.4))    ! Eq. (4)     #dtb105 02123

      ENDIF

c     IF(CCVR .EQ. NO_SKY)THEN  ! Missing cloud cover
c        SKY = 0                                                         #dtb105 02123
c     ELSE                                                               #dtb105 02123
c        SKY = CCVR
c     END IF                                                             #dtb105 02123


      RETURN

      END


      SUBROUTINE NETRAD(CCVR,ALBEDO,T,QR,RN)
C====================================================================**
C     NETRAD Module of the AERMET Meteorological Preprocessor
C
C     Purpose: To compute the net radiation (RN) from solar insolation,
C              albedo, cloud cover, and temperature.
C
C     Calling Arguments:
C             IHR      In    INTEGER    HOUR OF DAY
C
C     Called by:  MPPBL
C
C     Calls to:   ---
C
C     Maintained by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C     Revision History:
C        <none>
C
C     Code which can be traced to an equation or equations in the AERMOD
C     Model Formulation Document is indicated by including the equation
C     number in the right hand margin of the record; e.g., ! Eq (1)
C
C
C-----------------------------------------------------------------------


C---- Data declarations

ctmp      INTEGER IHR
      REAL C1, STEFB, SKY                                                #dtb105 02123

ctmp      INCLUDE 'MP2.INC'

C---- Constants used in the computation
      C1 = 5.31E-13
      C2 = 60.0
      C3 = 1.12
      STEFB = 5.67E-08

      SKY = CCVR


C---- Compute the net radiation from solar insolation, albedo, surface
C     temperature and cloud fraction.

      RN = ((1.0-ALBEDO)*QR+C1*(T**6)-STEFB*
     &          (T**4) + C2*(SKY/10.0))/C3                ! Eq. (2) #dtb105 02123


      RETURN
      END


      SUBROUTINE HEAT(RN,BOWEN,HFLUX)
C=====================================================================**
C          HEAT Module of the AERMET Meteorological Preprocessor
C
C     Purpose: To calculate the heat flux, HFLUX, from the net radiation
C              and Bowen ratio.  The equation for the heat flux is:
C              (CONSTANT*NET RADIATION) / (1+(1/BOWEN RATIO))
C
C     Calling Arguments:
C             IHR      In    INTEGER   HOUR OF DAY
C
C     Called by:  MPPBL
C
C     Calls to:   ---
C
C     Maintained by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C     Revision History:
C        <none>
C
C     Code which can be traced to an equation or equations in the AERMOD
C     Model Formulation Document is indicated by including the equation
C     number in the right hand margin of the record; e.g., ! Eq (1)
C
C-----------------------------------------------------------------------
ctmp      INTEGER IHR
ctmp      INCLUDE 'MP1.INC'
ctmp      INCLUDE 'MP2.INC'

      REAL CSUBG
      PARAMETER (CSUBG = 0.1)


C---- Calculate the HEAT FLUX

      HFLUX = ( (1.0-CSUBG) * RN )/(1.0+(1.0/BOWEN))  ! Eq. (1)

      RETURN
      END


      SUBROUTINE UCALCO(WSPD, ZREF, T, Z0, HFLUX, USTAR, MOL)
C=====================================================================**
C          UCALCO Module of the AERMET Meteorological Preprocessor
C
C     Purpose:  This subroutine will set up the iteration needed to
C               calculate the friction velocity during unstable
C               atmospheric cases in which HFLUX > 0.0.
C               NOTE: to complete this calculation, HFLUX has already
C               been calculated in SUBR.HEAT, which uses the Holtslag
C               and van Ulden method.
C
C     Input:
C      IHR       Integer   Hour of day
C      WSPD      Real      Wind speed at the reference height ZREF
C      ZREF      Real      Reference wind speed height
C      HFLUX     Real      Sensible heat flux
C      T         Real      Ambient temperature at ZTREF
C      ZO        Real      Roughness length
C      RHO       Real      Density of air
C
C     Output:
C      USTAR     Real      Surface friction velocity
C      MOL       Real      Monin-Obukhov length
C
C     Initial release:  December 1992
C
C     Maintained by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C     Code which can be traced to an equation or equations in the AERMOD
C     Model Formulation Document is indicated by including the equation
C     number in the right hand margin of the record; e.g., ! Eq (1)
C
C
C     Revision history:
C        10/18/96 (PES)
C          - subprogram restructured to eliminate GO TO statements
C
C-----------------------------------------------------------------------

C---- Variable declarations

ctmp      INTEGER IHR
      REAL PSIZL, PSIZOL, EPS, VONK, LASTL
      REAL MU, MU0, CP, PI, G, MOL

ctmp      INCLUDE 'MP1.INC'
ctmp      INCLUDE 'MP2.INC'

C---- Data initialization
      PARAMETER (CP=1004.0, PI=3.14159, GRAV=9.80655,
     &           EPS=0.01, VONK=0.4)

C       CP   = specific volume of dry air at constant pressure
C       PI   = 3.14159
C       GRAV = gravitational acceleration (m/sec*sec)
C       VONK = von Karman constant
C       EPS  = convergence criterion (1% here) for Monin-Obukov length

      RHO = 1.2

C==== Begin Processing =================================================

C---- Make first guess for iteration initialize PSIZL and PSIZOL to zero.
C     Initialize other parameters:
C        LASTL =  previous value of the Monin-Obukhov length

      PSIZL = 0.0
      PSIZOL = 0.0
      LASTL = 0.0

      IF( WSPD .EQ. 0.0 ) THEN
C------- Calm conditions for convective boundary layer.
         USTAR = 0.0
         MOL = 0.0

      ELSE
C------- Set up iteration loop over Monin-Obukhov length (MOL) and
C        friction velocity (USTAR).  The first guess for the
C        iteration is with PSIZL and PSIZOL set equal to zero.
C        The next guess is made by reevaluating PSIZL and PSIZOL.

         USTAR = VONK * WSPD /
     &                ( ALOG( (ZREF) / Z0 ) -
     &                PSIZL + PSIZOL)                                ! Eq. (6)

         MOL = -RHO * CP * T * (USTAR**3) /
     &               ( VONK * GRAV * HFLUX )                    ! Eq. (8)

         DO WHILE( ABS(MOL-LASTL) .GT. ABS(EPS*MOL ) )
            LASTL = MOL

C---------- Calculate new MU, MU0, PSIZL and PSIZOL values.

c            MU  = (1.0 - 16.0 *
c     &            ( ZREF / MOL ) ) **0.25

            MU  = (1.0 - 16.0*(ZREF/MOL) )**0.25

            MU0 = (1.0 - 16.0  * ( Z0 / MOL ) ) **0.25

            PSIZL = 2.0 * ALOG( (1.0 + MU) / 2.0) +
     &              ALOG( (1.0 + MU * MU) / 2.0) -
     &              2.0 * ATAN(MU) + PI/2.0                          ! Eq. (7)

            PSIZOL = 2.0 * ALOG( (1.0 + MU0) / 2.0) +
     &               ALOG( (1.0 + MU0 * MU0) / 2.0 ) -
     &               2.0 * ATAN(MU0) + PI/2.0                        ! Eq. (7)

C---------- Recompute USTAR and MOL

            USTAR = VONK * WSPD /
     &                   ( ALOG( ZREF / Z0 ) -
     &                   PSIZL + PSIZOL)                             ! Eq. (6)

            MOL = -RHO * CP * T * (USTAR**3) /
     &                  ( VONK * GRAV * HFLUX )                 ! Eq. (8)

         ENDDO

      ENDIF

      RETURN
      END


      SUBROUTINE DATIME ( DCALL, TCALL )
C***********************************************************************
C                 DATIME Module
C
C        PURPOSE: Obtain the system date and time
C
C        PROGRAMMER: Jim Paumier, PES, Inc.
C
C        DATE:    April 15, 1994
C
C        MODIFIED:   Uses Fortran 90 DATE_AND_TIME routine.
C                    R.W. Brode, PES, 8/14/98
C
C        INPUTS:  none
C
C        OUTPUTS: Date and time in character format
C
C        CALLED FROM:  RUNTIME
C***********************************************************************
C
C     Variable Declarations
      IMPLICIT NONE

      CHARACTER DCALL*8, TCALL*8
      CHARACTER CDATE*8, CTIME*10, CZONE*5
      INTEGER  :: IDATETIME(8)
      INTEGER  :: IPTYR, IPTMON, IPTDAY, IPTHR, IPTMIN, IPTSEC

      DCALL = ' '
      TCALL = ' '

C     Call Fortran 90 date and time routine
      CALL DATE_AND_TIME (CDATE, CTIME, CZONE, IDATETIME)

C     Convert year to two digits and store array variables
      IPTYR  = IDATETIME(1) - 100 * INT(IDATETIME(1)/100)
      IPTMON = IDATETIME(2)
      IPTDAY = IDATETIME(3)
      IPTHR  = IDATETIME(5)
      IPTMIN = IDATETIME(6)
      IPTSEC = IDATETIME(7)

C     Write Date and Time to Character Variables, DCALL & TCALL
      WRITE(DCALL, '(2(I2.2,"/"),I2.2)' ) IPTMON, IPTDAY, IPTYR
      WRITE(TCALL, '(2(I2.2,":"),I2.2)' ) IPTHR, IPTMIN, IPTSEC

      RETURN
      END
