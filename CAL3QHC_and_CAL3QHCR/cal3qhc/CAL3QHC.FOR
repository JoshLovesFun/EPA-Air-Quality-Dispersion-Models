C  **************************************************************************
C  *                                                                        *
C  *                         CAL3QHC (DATED 04244)                          *
C  *                                                                        *
C  *                       *** SEE CAL3QHC MCB#6 ***                        *
C  *                                                                        *
C  *        ON THE SUPPORT CENTER FOR REGULATORY AIR MODELS WEB SITE        *
C  *                                                                        *
C  *                      http://www.epa.gov/scram001                       *
C  *                                                                        *
C  **************************************************************************
C
C      ================================================================
C     |                                                                |
C     | CAL3QHC (Dated 04244) was recompiled using Compaq Visual       |
C     |         Fortran Version 6.6C.  This should alleviate memory    |
C     |         management and stack segment errors when Windows XP    |
C     |         is the Operating System.  Some minor comments were     |
C     |         added or modified.
C     |                                                                |
C      ================================================================
C
C      ================================================================
C     |                                                                |
C     | CAL3QHC was modified to accept free format input, and to be    |
C     |         compilable using a Microsoft Fortran compiler.  The    |
C     |         program was recompiled using Microsoft Fortran.        |
C     |         Some minor alterations were made to the READ, WRITE,   |
C     |         and FORMAT statements and to the output file structure.|
C     |         A Lahey Fortran File Input/Output file name processing |
C     |         algorithm has been saved, in part, as a separate file  |
C     |         and by commenting out Lahey specific code with 'CL'    |
C     |         added to the first two columns of each respective line |
C     |         of code.  Lahey date and time library functions have   |
C     |         also been retained by commenting out these functions   |
C     |         in the same manner.  A bubble sort subroutine was not  |
C     |         being used and was therefore deleted.                  |
C     |                                                                |
C      ================================================================
C     |                                                                |
C     | PROGRAM: CAL3QHC - A modeling methodology for predicting       |
C     | -------            pollutant concentrations near roadway       |
C     |                    intersections.                              |
C     |                                                                |
C     |          CAL3QHC is a consolidation of the CALINE-3 dispersion |
C     |          model and a queuing algorithm that internally         |
C     |          estimates the length of the queues formed by idling   |
C     |          vehicles at a signalized intersections. It calculates |
C     |          the contribution of the emissions from these idling   |
C     |          vehicles, and internally converts these sources into  |
C     |          CALINE-3 link format.                                 |
C     |                                                                |
C     | MODIFIED BY: TEREZA STRATOU                                    |
C     | -----------  PARSONS BRINCKERHOFF QUADE & DOUGLAS, INC.        |
C     |                                                                |
C     | DATE: AUGUST 1990                                              |
C     | ----                                                           |
C      ================================================================
C     |                                                                |V2
C     | CAL3QHC - (dated 93157)                                        |V2
C     |                                                                |V2
C     |           The difference between the original CAL3QHC model    |V2
C     |           and this revised version (dated 93157) pertains to   |V2
C     |           the calculation of intersection capacity, delay,     |V2
C     |           and queue lengths.  This version includes three new  |V2
C     |           optional traffic parameters that could be specified  |V2
C     |           by the user:  Saturation Flow Rate, Signal Type,     |V2
C     |           and Arrival Type.  This revised version also         |V2
C     |           replaces the stopped delay (used in the queue        |V2
C     |           calculation) with the approach delay.                |V2
C     |                                                                |V2
C     | MODIFIED BY: STEVEN WARSHAW                                    |V2
C     | -----------  PARSONS BRINCKERHOFF QUADE & DOUGLAS, INC.        |V2
C     |                                                                |V2
C     | DATE: OCTOBER 1991                                             |V2
C     | ----                                                           |V2
C     |----------------------------------------------------------------|
C     |                                                                |V2EC
C     | CAL3QHC - (dated 93157)  EXPANDED CAPACITY                     |V2EC
C     |                                                                |V2EC
C     |           The capacity of the program has been increased from  |V2EC
C     |           55 links to 120 links and from 20 receptors to 60    |V2EC
C     |           receptors.  The output has the option to be          |V2EC
C     |           printed in either Metric or English units.  The      |V2EC
C     |           idle emission factor must be input in grams per hour |V2EC
C     |           (instead of the original grams per minute).  All     |V2EC
C     |           input queue parameters are printed in the output.    |V2EC
C     |                                                                |V2EC
C     | MODIFIED BY: INGRID ENG                                        |V2EC
C     | -----------  PARSONS BRINCKERHOFF QUADE & DOUGLAS, INC.        |V2EC
C     |                                                                |V2EC
C     | DATE: JANUARY 1992                                             |V2EC
C     | ----                                                           |V2EC
C      ================================================================
C
C  **************************************************************************
C
C                                INPUT FORMAT
C
C      Note:  Input is in free format.  Single quotes need to be placed around
C             'character string input'.  All data that could be entered
C             optionally under the old fixed input format, needs to be entered.
C
C           ***********************************************************
C           ****       ALL OPTIONAL DATA NEEDS TO BE ENTERED.      ****
C           ****   (eg saturation rates, wind range increments.)   ****
C           ***********************************************************
C
C    'Title (up to 40 Char.)' ATIM, ZO, VS, VD, NR, SCAL, IOPT, IDEBUG.
C    Receptor name, X- and Y-coordinate, elevation.
C      (The last line is repeated for each receptor)
C    'Run name', number of links, lines of MET data, PRINT2 flag, mode.
C    One entry for each link:
C    Link flow type (IQ, 1 - free flow, 2 - queue).
C      For IQ = 1:
C        'Link name', 'type', beginning X, Y-coords, ending X, Y-Coords,
C           link volume, emission rate, source height, mixing zone width.
C      For IQ = 2:
C        'Link name', 'type', beginning X, Y-coords, ending X, Y-Coords,
C          source height, mixing zone width, number of lanes.
C        Traffic light cycle time, average red, yellow factor, approach volume,
C          idle emiss. factor, saturation flow rate, signal type, arrival rate.
C    Wind speed, actual wind direction, stability class, mixing height,
C      ambient background concentration, 'wind direction variation flag',
C      direction variation increment,
C      lower boundary of the wind dir. range variation increment multiplier,
C      upper boundary of the wind dir. range variation increment multiplier.
C
C ***************************************************************************
C
C      =================================
C     | VARIABLE DECLARATION STATEMENTS |
C      =================================
C
      CHARACTER    X1(60)*15,VAR*1,REC(60)*5,RUN*40,JOB*40,STB(6)*1,    V2EC
     +             LNK(120)*20, MODE*1, RCP(60)*20, TYP(120)*2          V2EC

CL      CHARACTER  IDATE*8, ITIME*5, FILE5*40, FILE6*40                 V2EC
C
      REAL         BRGV(361),MOWT,NE,LIM,KZ,LB,LBRG(120),               V2EC
     +             INC,MIXH,K1,IDLFAC(120),PSCALE                       V2ECF
C
      INTEGER      DEGR,ANGMAX(60),VA(361),VAI(2),PRINT2,               V2EC
     +             PGCT,PCLAS,CLAS,COD(120),KQ,CAVG(120),RAVG(120)      V2EC
C
      INTEGER      SFR(120), ST(120), AT(120), IDC, IORDER(60)          V2EC
C                                                                       V2EC
      DOUBLE PRECISION HYP,SIDE,FAC2,PD,A,B,L,D,LL(120),INTG(6),LL1,    V2EC
     +                 XPRI,YPRI,APRI,BPRI,LPRI,DPRI,XD,YD,D1,D2,ABSQD, V2EC
     +                 XW1,YW1,XW2,YW2,LLW,HLW,WLW,XWR,YWR,ZWR          V2EF
C
C      ==================
C     | DIMENSION ARRAYS |
C      ==================
C
      DIMENSION CRMAX(60),CON(60),C(120,60),                            V2EC
     +          COMAX(120,60),XR(120),YR(120),ZR(120),XL1(120),XL2(120),V2EC
     +          YL1(120),YL2(120),VPHL(120),EFL(120),HL(120),WL(120),   V2EC
     +          AZ(6),AY1(6),AY2(6),Y(6),WT(5),QAVG4(120),GAVG(120),    V2EC
     +          VJ(120),DJ(120),V(120),IMU(120),YFAC(120),IQ(120),      V2EC
     +          IV(120),RC(120),X(120),ZFAC(120),DC(120),XQD(120),      V2EC
     +          YQD(120),NLANES(120),TER(120),THETA(120)                V2EC
C
      DIMENSION DDJ(120)                                                V2EC
C
C      ================================                                 V2
C     | LOOKUP TABLE ARRAY DECLARATION |                                V2
C      ================================                                 V2
C                                                                       V2
      REAL      PAF(5, 3, 3)                                            V2
C
C      ============
C     | DATA INPUT |
C      ============
C
      DATA AZ/1112.,566.,353.,219.,124.,56./
      DATA AY1/0.46,0.29,0.18,0.11,0.087,0.057/
      DATA AY2/1831.,1155.,717.,438.,346.,227./
      DATA WT/0.25,0.75,1.,0.75,0.25/
C
C
      DATA STB/'A','B','C','D','E','F'/
      DATA REC/'REC1 ','REC2 ','REC3 ','REC4 ','REC5 ','REC6 ','REC7 ',
     +         'REC8 ','REC9 ','REC10','REC11','REC12','REC13',
     +         'REC14','REC15','REC16','REC17','REC18','REC19',
     +         'REC20','REC21','REC22','REC23','REC24','REC25',         V2EC
     +         'REC26','REC27','REC28','REC29','REC30','REC31',         V2EC
     +         'REC32','REC33','REC34','REC35','REC36','REC37',         V2EC
     +         'REC38','REC39','REC40','REC41','REC42','REC43',         V2EC
     +         'REC44','REC45','REC46','REC47','REC48','REC49',         V2EC
     +         'REC50','REC51','REC52','REC53','REC54','REC55',         V2EC
     +         'REC56','REC57','REC58','REC59','REC60'/                 V2EC
      DATA X1/60*'---------------'/                                     V2EC
C                                                                       V2EC
C      =========================                                        V2
C     | LOOKUP TABLE DEFINITION |                                       V2
C      =========================                                        V2
C                                                                       V2
      DATA PAF /1.85, 1.35, 1.00, 0.72, 0.53,                           V2
     +          1.50, 1.22, 1.00, 0.82, 0.67,                           V2
     +          1.40, 1.18, 1.00, 0.90, 0.82,                           V2
     +          1.54, 1.08, 0.85, 0.62, 0.40,                           V2
     +          1.25, 0.98, 0.85, 0.71, 0.50,                           V2
     +          1.16, 0.94, 0.85, 0.78, 0.61,                           V2
     +          1.85, 1.35, 1.00, 0.72, 0.42,                           V2
     +          1.50, 1.22, 1.00, 0.82, 0.53,                           V2
     +          1.40, 1.18, 1.00, 0.90, 0.65/                           V2
C                                                                       V2EC
C     ==========================================================        V2EC
C    | MAXIMUM NUMBER OF RECEPTORS AND LINKS ALLOWED BY PROGRAM |       V2EC
C     ==========================================================        V2EC
C                                                                       V2EC
      MAXR=60                                                           V2EC
      MAXL=120                                                          V2EC
      IN = 7
      IOUT = 6
      IPO = 8
C
        CALL GETDAT (IYR, IMON, IDAY)
        CALL GETTIM (IIHR, IMIN, ISEC, IX)
          IYR = MOD(IYR,100)
CL
CL      CALL TIME(ITIME)                                                  V2EC
CL      CALL DATE(IDATE)                                                  V2EC
CLC
CL      CALL GETFIL(FILE5,FILE6)                                          V2EC
CLC
CL      OPEN (IN,FILE=FILE5 ,STATUS='OLD')                                V2EC
CL      OPEN (IPO,FILE=FILE6 ,STATUS='UNKNOWN')
CLC
C
C      ==========================================
C     | INITIALIZATION OF CONSTANTS AND COUNTERS |
C      ==========================================
C
      DO 3 I=1,120
        COD(I)=I
    3 CONTINUE
C
      PGCT=0
      PI=3.1415926
      RAD=PI/180.
      DEG=180./PI
      PSCALE=1/0.3048                                                   V2EF

C     --- MOLECULAR WEIGHT OF CO --
      MOWT=28.
C
      DREF=ALOG(10000.)
C
C      ============
C     | INPUT DATA |
C      ============
C
   10 READ (IN,*,ERR=5,END=9999) JOB,ATIM,Z0,VS,VD,NR,SCAL,IOPT,IDEBUG  V2ECF
        IF (JOB(1:1) .NE. CHAR(39)) GOTO 4
    5 WRITE(IOUT,6)
    6 FORMAT('                      NOTICE'/
     +5x,'CAL3QHC has been made user-friendlier by using a ',/5X,
     +'free format.  However, the program has detected the',
     +   ' possible use ',/5X,
     +'of the old format.  If this is so, please review the Input ',
     +   'Format section',/5X,
     +'of the CAL3QHC source code AND Readme file.  The program will',
     +   ' continue',/5X,
     +'until another error is detected.'/)
      PAUSE '            Please press any key to continue'
    4 CONTINUE
      IF(IDEBUG.EQ.1)WRITE(IOUT,120)JOB,ATIM,Z0,VS,VD,NR,SCAL
C     -------------------------------------
C     ATIM = AVERAGING TIME (MINUTES)
C       Z0 = ROUGHNESS (CM)
C       VS = SETTLING VELOCITY (CM/SEC)
C       VD = DEPOSITION VELOCITY (CM/SEC)
C       NR = NUMBER OF RECEPTORS
C     -------------------------------------
C
      VS1=VS
      VD1=VD
C
C      ---------------------
C     | CONVERT  M/S TO M/S |
C      ---------------------
      VS=VS/100.
      VD=VD/100.
      V1=VD-VS/2.
C
C      --------------------
C     | RECEPTOR LOCATIONS |
C      --------------------                                             V2EC
C                                                                       V2EC
C     CHECK NUMBER OF RECEPTORS INPUT DOES NOT EXCEED MAXIMUM ALLOWED.  V2EC
C                                                                       V2EC
      IF(NR.GT.MAXR)THEN                                                V2EC
         WRITE(IOUT,167)NR,MAXR                                         V2EC
 167     FORMAT(' NUMBER OF RECEPTORS INPUT =',I4, ' > MAXIMUM ALLOWED',V2EC
     *  ' BY PROGRAM =',I4,'.  PROGRAM IS TERMINATED!')                 V2EC
         STOP                                                           V2EC
      ENDIF                                                             V2EC
C                                                                       V2EC
      DO 1000 I=1,NR
        READ (IN,*) RCP(I), XR(I),YR(I),ZR(I)                           V2EC
          IF(IDEBUG.EQ.1) WRITE(IOUT,130) RCP(I), XR(I), YR(I), ZR(I)   V2EC
        XR(I)=SCAL*XR(I)
        YR(I)=SCAL*YR(I)
        ZR(I)=SCAL*ZR(I)
 1000 CONTINUE
C
C      -----------------------
C     | LINK & MET CONDITIONS |
C      -----------------------
C     NL = NUMBER OF LINKS
C     NM = NUMBER OF MET CONDITIONS
C     ------------------------------
      READ (IN,*) RUN,NL,NM,PRINT2,MODE
      IF(IDEBUG.EQ.1)WRITE(IOUT,150) RUN,NL,NM,PRINT2,MODE              V2EC

        MODET = 2

          IF (MODE .EQ. 'c') MODET = 0
          IF (MODE .EQ. 'C') MODET = 0
          IF (MODE .EQ. 'p') MODET = 1
          IF (MODE .EQ. 'P') MODET = 1

            IF (MODET .EQ. 2) THEN
              WRITE(IOUT,*) 'The MODE variable, ',MODE,
     +                      ' was incorrectly entered.'
              STOP
            END IF

         IF (MODET .EQ. 0) THEN
            FPPM=0.0245/MOWT
          ELSE
            FPPM = 1
         END IF
C
C    CHECK NUMBER OF LINKS INPUT DOES NOT EXCEED MAXIMUM ALLOWED.
C
      IF(NL.GT.MAXL)THEN                                                V2EC
         WRITE(IOUT,168)NL,MAXL                                         V2EC
 168     FORMAT(' NUMBER OF LINKS INPUT = ', I4, '> MAXIMUM ALLOWED',   V2EC
     * ' BY PROGRAM = ', I4, '.  PROGRAM IS TERMINATED!')               V2EC
         STOP                                                           V2EC
      ENDIF                                                             V2EC
C                                                                       V2EC
      DO 1051 I=1,NL
        READ(IN,*) IQ(I)                                                V2EC
        IF(IDEBUG.EQ.1)WRITE(IOUT,14) IQ(I)                             V2EC
C       -----------------------------------------------------
C       IF IQ=1 FREE FLOW LINK. IF IQ=2 QUEUE LINK.
C       -----------------------------------------------------
C       IF IQ(I) = 1 PROGRAM SKIPS QUEUE LINK READ STATEMENTS
C       -----------------------------------------------------
C
        IF(IQ(I).EQ.1) GOTO 2
C
C       -----------------------------------------------------------------
C       XL1, YL1 = COORDINATES FOR START OF QUEUE.
C       XL2, YL2 = COORDINATES FOR THE END OF AN ASSUMED QUEUE.
C
C       The length defined by these coordinates can be anything except
C       zero. It is used by the program to orient the calculated queue.
C
C       WL(I)    = WIDTH OF LINK.
C       NLANES(I)= NUMBER OF LANES IN LINK.
C       IUFAC(I) = FREE FLOW SPEED ON LINK.
C       CAVG(I)  = AVERAGE SIGNAL CYCLE LENGTH.
C       RAVG(I)  = AVERAGE RED TIME.
C       IWIDTH(I)= DISTANCE ACROSS INTERSECTION TO CLEAR VEHICLE.
C       IV(I)    = APPROACH VOLUME ON LINK.
C       IDLFAC(I)= IDLE EMISSION FACTOR. (INPUT IN gm/hr UNITS.         V2EF
C                  INTERNAL CONVERSION WILL BE MADE TO gm/min UNITS.    V2EF
C       SFR(I)   = SATURATION FLOW RATE (DEFAULT = 1600)                V2
C       ST(I)    = SIGNAL TYPE (DEFAULT = 1)                            V2
C                     1: PRETIMED                                       V2
C                     2: ACTUATED                                       V2
C                     3: SEMIACTUATED                                   V2
C       AT(I)    = ARRIVAL RATE (DEFAULT = 3)                           V2
C                     1: WORST PROGRESSION                              V2
C                     2: BELOW AVERAGE                                  V2
C                     3: AVERAGE                                        V2
C                     4: ABOVE AVERAGE                                  V2
C                     5: BEST PROGRESSION                               V2
C
C       TYP      = HIGHWAY TYPE
C                     AG: AT-GRADE
C                     DP: DEPRESSED (CUT)
C                     FL: FILL
C                     BR: BRIDGE
C       VPHL     = TRAFFIC VOLUME (VEHICLES/HR)
C       HL       = SOURCE HEIGHT (M)
C       WL       = MIXING ZONE WIDTH (M)
C       ------------------------------------------------------------------
C
        READ(IN,*)  LNK(I),TYP(I),XL1(I),YL1(I),XL2(I),YL2(I),          V2EC
     +              HL(I),WL(I),NLANES(I)                               V2EC
        IF(IDEBUG.EQ.1) WRITE(IOUT,15) LNK(I),TYP(I),XL1(I),            V2EC
     +              YL1(I),XL2(I),YL2(I),HL(I),WL(I),NLANES(I)          V2EC
        READ(IN,*)  CAVG(I),RAVG(I),YFAC(I),IV(I),IDLFAC(I)             V2
     +             ,SFR(I),ST(I),AT(I)                                  V2
        IF(IDEBUG.EQ.1)WRITE(IOUT,1) CAVG(I),RAVG(I),YFAC(I),IV(I)      V2EC
     +             ,IDLFAC(I),SFR(I),ST(I),AT(I)                        V2EC
C        ------------------                                             V2
C       | ENFORCE DEFAULTS |                                            V2
C        ------------------                                             V2
C                                                                       V2
      IF (SFR(I) .EQ. 0) SFR(I) = 1600                                  V2
      IF (ST(I)  .EQ. 0) ST(I)  = 1                                     V2
      IF (AT(I)  .EQ. 0) AT(I)  = 3                                     V2
C                                                                       V2
        GOTO 5555
    2   READ(IN,*)  LNK(I),TYP(I),XL1(I),YL1(I),XL2(I),YL2(I),
     +               VPHL(I),EFL(I),HL(I),WL(I)
        IF(IDEBUG.EQ.1) WRITE(IOUT,160) LNK(I), TYP(I), XL1(I),         V2EC
     +               YL1(I),XL2(I),YL2(I),VPHL(I),EFL(I),HL(I),WL(I)    V2EC
C
C        -------------------
C       | SCALE ADJUSTEMENT |
C        -------------------
 5555   XL1(I)=SCAL*XL1(I)
        XL2(I)=SCAL*XL2(I)
        YL1(I)=SCAL*YL1(I)
        YL2(I)=SCAL*YL2(I)
        HL(I) =SCAL*HL(I)
        WL(I) =SCAL*WL(I)
C
C        -------------
C       | LINK LENGTH |
C        -------------
        LL(I) =SQRT((XL1(I)-XL2(I))**2+(YL1(I)-YL2(I))**2)
        IF (LL(I).GE.WL(I)) GOTO 1025
        WRITE (IOUT,170)
        STOP
 1025   IF (ABS(HL(I)).LE.10.) GOTO 1050
        WRITE (IOUT,180)
        STOP
C
C       ----------------------------------------------------------
C      | CALCULATION OF THETA(I), THE ANGLE FORMED BY THE ASSUMED |
C      | QUEUE AND THE CHOSEN COORDINATE SYSTEM.                  |
C       ----------------------------------------------------------
 1050   XQD(I)=XL2(I)-XL1(I)
        YQD(I)=YL2(I)-YL1(I)
        ABSQD=ABS(XQD(I)/LL(I))
        IF(ABSQD.GT.1.0) ABSQD=1.0
        THETA(I)=DEG*DACOS(ABSQD)
        IF (XQD(I).GT.0. .AND.
     +      YQD(I).GE.0.) THETA(I)=90.-THETA(I)
        IF (XQD(I).GE.0. .AND.
     +      YQD(I).LT.0.) THETA(I)=90.+THETA(I)
        IF (XQD(I).LT.0. .AND.
     +      YQD(I).LE.0.) THETA(I)=270.-THETA(I)
        IF (XQD(I).LE.0. .AND.
     +      YQD(I).GT.0.) THETA(I)=270.+THETA(I)
C
C       ------------------------------------------
C       SKIPS QUEUE CALCULATION IF FREE FLOW LINK.
C       ------------------------------------------
        IF(IQ(I).EQ.1) GOTO 1051
C
C        ===================
C       | QUEUE CALCULATION |
C        ===================
        V(I)=IV(I)/NLANES(I)
C       ------------------------------------------------------------------
C       To start the queue calculations, assume time lost getting queue in
C       motion is maximum (K1= 2.0 seconds).
C       ------------------------------------------------------------------
C       RC(I)  = RED TO CYCLE RATIO.
C       ---------------------------
        K1=2.0
C
        GAVG(I)=FLOAT(CAVG(I)-RAVG(I))
        RC(I)=FLOAT(RAVG(I))/FLOAT(CAVG(I))
C
 
C       CALCULATE INTERSECTION CAPACITY, X(I).
C
C       CONVERT MPH TO FEET PER SECOND.
C            SFR(I) IS IN VEHICLES PER HOUR                             V2
C            CMAX IS SECONDS
C           ((3600 IS SECONDS PER HOUR))                                V2
C           ((2.0 IS SECONDS OF HEADWAY PER VEHICLE.))                  V2
C       ------------------------------------------------
C
        X(I)=SFR(I)/FLOAT(CAVG(I))                                      V2
C
C       ----------------------------------------------------
C       ZFAC(I)=TIME (sec) AVAILABLE MINUS START DELAY MINUS
C       TIME FOR VEHICLE TO CLEAR INTERSECTION (YFAC).
C       ----------------------------------------------------
C
  110   ZFAC(I)=GAVG(I)-K1-YFAC(I)
C
C       ------------------------------
C       IMU(I)=INTERSECTIONAL CAPACITY
C       (VEH/HOUR)/SEC*SEC
C       ------------------------------
        IMU(I)=X(I)*ZFAC(I)
C
C       -------------------------------------
C       CALCULATION OF DEMAND-CAPACITY RATIO.
C       -------------------------------------
        DC(I)=V(I)/FLOAT(IMU(I))
C
C       -----------------------------------------------------           V2
C       CALCULATE THE PAF LOOKUP LINE BASED ON THE V/C RATIO.           V2
C       -----------------------------------------------------           V2
C                                                                       V2
        IF     (DC(I) .LE. 0.6)  THEN                                   V2
          IDC = 1                                                       V2
        ELSE IF (DC(I) .LE. 0.8) THEN                                   V2
          IDC = 2                                                       V2
        ELSE                                                            V2
          IDC = 3                                                       V2
        ENDIF                                                           V2
C
C        --------------------------------------------------------
C       | DELAY CALCULATIONS FOR QAVG4 ACCORDING TO 1985 HIGHWAY |
C       | CAPACITY MANUAL FORMULA.                               |
C        --------------------------------------------------------
C
  111   A1=0.38*CAVG(I)
        A2=GAVG(I)/FLOAT(CAVG(I))
        A3=(1-A2)**2
        PECK1=AMIN1(DC(I),1.0)
        A4=1-A2*PECK1
        A5=A1*(A3/A4)
        A6=(PECK1-1)+SQRT((PECK1-1)**2+16*(PECK1/IMU(I)))
        DJ(I)=(A5+(173*PECK1**2)*A6) * PAF(AT(I),IDC,ST(I))             V2
C                                                                       V2
C       -------------------------------------------------               V2
C       APPROACH DELAY:DDJ(I) = STOPPED DELAY DJ(I) * 1.3               V2
C       -------------------------------------------------               V2
C                                                                       V2
        DDJ(I)=DJ(I)*1.3                                                V2
C
C       -----------------------------------------------
C       CONVERT APPROACH VOLUME TO VEHICLES PER SECOND.
C       -----------------------------------------------
        VJ(I)=V(I)/3600.
C
C       ------------------------
C       COMPUTE TOTAL LOST TIME.
C       ------------------------
C
        QAVG4(I)=MAX( (VJ(I)*RAVG(I)/2)+(VJ(I)*DDJ(I)), VJ(I)*RAVG(I) ) V2
C
C       **************
C       UNDER-CAPACITY
C       **************
C
C       ------------------------------------------------------
C       COMPUTE NEW LINE LENGTH ASSUMING 6 METERS PER VEHICLE.
C       ------------------------------------------------------
C
        LL(I)=QAVG4(I)*6
C
        IF (DC(I).LE.1.0) GOTO 1111
C       *************
C       OVER-CAPACITY
C       *************
        LL1=3*(V(I)-IMU(I))
        LL(I)=LL1+LL(I)
        QAVG4(I)=LL1/6.0+QAVG4(I)
C
C        --------------------------------------------
C       | COMPUTE NEW XL2 AND YL2 COORDINATES TO     |
C       | TELL PROGRAM END OF QUEUE.                 |
C        --------------------------------------------
 1111   IF(THETA(I).GT.90.AND.THETA(I).LE.180)
     +  XL2(I)=XL1(I)+LL(I)*SIN(RAD*(180.-THETA(I)))
C
        IF(THETA(I).GT.90.AND.THETA(I).LE.180)
     +  YL2(I)=YL1(I)-LL(I)*COS(RAD*(180.-THETA(I)))
C
        IF(THETA(I).GT.180.AND.THETA(I).LE.270)
     +  XL2(I)=XL1(I)-LL(I)*COS(RAD*(270.-THETA(I)))
C
        IF(THETA(I).GT.180.AND.THETA(I).LE.270)
     +  YL2(I)=YL1(I)-LL(I)*SIN(RAD*(270.-THETA(I)))
C
        IF(THETA(I).GT.270.AND.THETA(I).LE.360)
     +  XL2(I)=XL1(I)-LL(I)*SIN(RAD*(360.-THETA(I)))
C
        IF(THETA(I).GT.270.AND.THETA(I).LE.360)
     +  YL2(I)=YL1(I)+LL(I)*COS(RAD*(360.-THETA(I)))
C
        IF(THETA(I).GT.0.AND.THETA(I).LE.90)
     +  XL2(I)=XL1(I)+LL(I)*SIN(RAD*THETA(I))
C
        IF(THETA(I).GT.0.AND.THETA(I).LE.90)
     +  YL2(I)=YL1(I)+LL(I)*COS(RAD*THETA(I))
C
C        --------------------------------------
C       | COMPUTE TOTAL EMISSION RATE FOR LINK.|
C        --------------------------------------
        TER(I)=IDLFAC(I)*1000000./3600./6.*NLANES(I)*RC(I)              V2EF
C
C       ----------------------------
C       SET ASSUMED EMISSION FACTOR.
C       ----------------------------
        EFL(I)=100.
C
C       --------------------------------------------------------------
C       COMPUTE NUMBER OF VEHICLES THAT MULTIPLIED BY THE E.F. OF 100.
C       WILL GIVE THE REQUIRED EMISSION RATE.
C       CALINE 3 USES Q=0.1726*VPHL*E.F., THEREFORE-
C       ---------------------------------------------------------------
        VPHL(I)=TER(I)/17.26
C
 1051 CONTINUE
C
C      ==========================
C     | METEOROLOGICAL DATA LOOP |
C      ==========================
C        U = WIND SPEED (M/S)
C      BRG = WIND DIRECTION (DIRECTION WIND IS BLOWING FROM - IN DEGREES)
C     CLAS = STABILITY CLASS (A-F)
C     MIXH = MIXING HEIGHT (M)
C      AMB = AMBIENT CONCENTRATION (PPM or ug/m^3)
C     -----------------------------------
C
      DO 9000 IM=1,NM
C
        IF(IM.NE.1) THEN
          NEW=0
          PU    =U
          PCLAS =CLAS
          PMIXH =MIXH
          PAMB  =AMB
        ELSE
          NEW=1
        ENDIF
C
 9060   READ (IN,*) U, BRG, CLAS, MIXH, AMB, VAR, DEGR, (VAI(I),I=1,2)  V2EC
        IF(IDEBUG.EQ.1)WRITE(IOUT,190) U,BRG,CLAS,MIXH,AMB,VAR,DEGR,    V2EC
     +       (VAI(I),I=1,2)                                             V2EC
C
        IF(IM.EQ.1) GOTO 9061
          IF(PU.NE.U) THEN
            NEW=1
            GOTO 9061
          ENDIF
          IF(PCLAS.NE.CLAS) THEN
            NEW=1
            GOTO 9061
          ENDIF
          IF(PMIXH.NE.MIXH) THEN
            NEW=1
            GOTO 9061
          ENDIF
          IF(PAMB.NE.AMB) THEN
            NEW=1
          ENDIF
C
 9061   IF(VAR.EQ.'N')    GOTO 9066
C
C       ------------------------------------------------
C       DETERMINE THE CONSECUTIVE WIND ANGLE MULTIPLIERS
C       ------------------------------------------------
        DO 9059 J=1,361
         IF(J.EQ.1) THEN
           VA(J)=VAI(1)
           GOTO 9059
         ENDIF
         VA(J)=VA(J-1)+1
         IF(VA(J).EQ.VAI(2)) THEN
           NANGLE=J
           GOTO 9065
         ENDIF
 9059   CONTINUE
C
C       ----------------------------------------------------
C       SET DEFAULT WIND DIRECTIONS IF NOT GIVEN BY THE USER
C       ----------------------------------------------------
 9065   IF(DEGR.EQ.0) THEN
          DEGR=3
          NANGLE=9
          VA(1)=-4
          DO 9064 K3=2,NANGLE
            VA(K3)=VA(K3-1)+1
 9064     CONTINUE
        ENDIF
C
 9066   IF(VAR.EQ.'N') NANGLE=1
C
C     OPEN TEMPORARY FILE.                                              V2EC
C                                                                       V2EC
        OPEN(16,STATUS='UNKNOWN',FILE='CAL3QHC.U16')                    V2EC
C                                                                       V2EC
C                                                                       V2EC
C       *******************
C       WIND DIRECTION LOOP
C       *******************
C
        BRG2=BRG
        DO 8999 K4=1,NANGLE
          BRG=BRG2+(DEGR*VA(K4))
          IF (BRG .GT. 360) BRG = BRG - 360.
C
          WRITE(IOUT,112) RUN,BRG
  112     FORMAT(1X,A40,' -  ANGLE: ',F4.0,'(degrees)')
C
          BRGV(K4)=BRG
 8020     BRG1=BRG
C
C         ---------------------
C         WIND ANGLE FOR OUTPUT
C         ---------------------
          BRG=BRG+180.
          IF (BRG.GE.360.) BRG=BRG-360.
C
C         --------------------------------
C         CONVERSION TO VECTOR ORIENTATION
C         --------------------------------
C         VIRTUAL DISPLACEMENT VECTORS
C         --------------------------------
          XVEC=COS(RAD*(450.-BRG))
          YVEC=SIN(RAD*(450.-BRG))
C
C         ----------------------------------------------------
C         CORRECTIONS FOR AVERAGING TIME AND SURFACE ROUGHNESS
C         ----------------------------------------------------
          AFAC=(ATIM/3.0)**.2
C
C         *** ALOG(SIGMA Y) AT 1 M AND 10 M
          SY1=ALOG(AY1(CLAS)*((Z0/3.)**.2)*AFAC)
          SY10=ALOG(AY2(CLAS)*((Z0/3.)**.07)*AFAC)
C
          PY1=EXP(SY1)
          PY2=(SY10-SY1)/DREF
          SZ10=ALOG(AZ(CLAS)*((Z0/10.)**.07)*AFAC)
C
C         -------------------------
C         ZERO CONCENTRATION MATRIX
C         -------------------------
          DO 720 I=1,NL
            DO 720 J=1,NR
              C(I,J)=0.
  720     CONTINUE
C
C          *********
C          LINK LOOP
C          *********
C
          DO 8000 IL=1,NL
            VPH=VPHL(IL)
            EF=EFL(IL)
            IF (TYP(IL).EQ.'DP'.OR.TYP(IL).EQ.'FL') GOTO 870
            H=HL(IL)
            GOTO 880
  870       H=0.
  880       W=WL(IL)
C
C           ------------
C           LINK ROUTINE
C           ------------
            W2=W/2.
C
C           *** LINEAL SOURCE STRENGTH PARALLEL TO HIGHWAY
C               IN MICRO-GRAMS/(METER*SEC)
            Q1=0.1726*VPH*EF
C
            XD=XL2(IL)-XL1(IL)
            YD=YL2(IL)-YL1(IL)
            ABSXD=DABS(XD)
            ABSQD=ABSXD/LL(IL)
            IF(ABSQD.GT.1.0) ABSQD=1.0
C
C           *** LINK BEARING
            LB=DEG*DACOS(ABSQD)
C
C           *** LINK BEARING MATRIX FOR OUTPUT
            IF (XD.GT.0. .AND.YD.GE.0.) LB=90.-LB
            IF (XD.GE.0. .AND.YD.LT.0.) LB=90.+LB
            IF (XD.LT.0. .AND.YD.LE.0.) LB=270.-LB
            IF (XD.LE.0. .AND.YD.GT.0.) LB=270.+LB
            LBRG(IL)=LB
C
C           *** WIND ANGLE WITH RESPECT TO LINK
            PHI=ABS(BRG-LB)
C
C           *** SET ELEMENT GROWTH BASE
            IF (PHI.LE.90.) GOTO 7600
            IF (PHI.GE.270.) GOTO 5000
            PHI=ABS(PHI-180.)
            GOTO 7600
 5000       PHI=ABS(PHI-360.)
 7600       IF (PHI.LT.20.) GOTO 7630
            IF (PHI.LT.50.) GOTO 7620
            IF (PHI.LT.70.) GOTO 7610
            BASE=4.
            GOTO 7650
 7610       BASE=2.
            GOTO 7650
 7620       BASE=1.5
            GOTO 7650
 7630       BASE=1.1
C
C           *** CONVERSION OF PHI FROM DEGREES TO RADIANS
 7650       PHI=RAD*(PHI)
C
            IF (PHI.GT.1.5706)  PHI=1.5706
            IF (PHI.LT.0.00017) PHI=0.00017
C
C           -----------------
C           DEPRESSED SECTION
C           -----------------
            IF (HL(IL).LT.-1.5) GOTO 7700
            DSTR=1.
            HDS=1.
            GOTO 7800
 7700       HDS=HL(IL)
            DSTR=0.72*ABS(HDS)**0.83
C
C           *** RESIDENCE TIME
 7800       TR=DSTR*W2/U
C
C           *** SIGMA Z POWER CURVE
            SGZ1=ALOG((1.8+0.11*TR)*(ATIM/30.)**0.2)
C
C           *** ALOG(SIGMA Z) AT W2
            PZ2=(SZ10-SGZ1)/(DREF-ALOG(W2))
            PZ1=EXP((SZ10+SGZ1-PZ2*(DREF+ALOG(W2)))/2.)
C
C           *************
C           RECEPTOR LOOP
C           *************
C
           DO 6000 IR=1,NR
C
C            *** OFFSET LENGTH
             A=(XR(IR)-XL1(IL))**2+(YR(IR)-YL1(IL))**2
             B=(XR(IR)-XL2(IL))**2+(YR(IR)-YL2(IL))**2
             L=(B-A-LL(IL)**2)/(2.*LL(IL))
C
C            *** RECEPTOR DISTANCE
             IF (A.GT.L**2) D=DSQRT(A-L**2)
             IF (A.LE.L**2) D=0.
C
C            *** UPWIND AND DOWNWIND LENGTH
             UWL=LL(IL)+L
             DWL=L
C
             IF(D.EQ.0.D0) DVIR=1.D0
             IF(D.NE.0.D0) DVIR=D
             XPRI=XR(IR)+DVIR*XVEC
             YPRI=YR(IR)+DVIR*YVEC
             APRI=(XPRI-XL1(IL))**2+(YPRI-YL1(IL))**2
             BPRI=(XPRI-XL2(IL))**2+(YPRI-YL2(IL))**2
             LPRI=(BPRI-APRI-LL(IL)**2)/(2.*LL(IL))
             IF (APRI.GT.LPRI**2) DPRI=DSQRT(APRI-LPRI**2)
             IF (APRI.LE.LPRI**2) DPRI=0.
             IF (DPRI.LT.D) D=-D
             IF (LPRI-L) 5725,5735,5735
 5725        TEMP=UWL
             UWL=-DWL
             DWL=-TEMP
 5735        IF (TYP(IL).EQ.'AG' .OR.TYP(IL).EQ.'BR') GOTO 5750
C
             D1=W2+2.*ABS(HL(IL))
             D2=W2
C
C            *** SINGLE PRECISION TO DOUBLE PRECISION FOR LOGICAL 'IF'
             IF (DABS(D).GE.D1) GOTO 5750
C
C            *** 2:1 SLOPE ASSUMED
             IF (DABS(D).LE.D2) Z=ZR(IR)-HL(IL)
             IF (DABS(D).GT.D2)
     +       Z=ZR(IR)-HL(IL)*(1.-(DABS(D)-W2)/(2.*ABS(HL(IL))))
             GOTO 3050
 5750        Z=ZR(IR)
C
C            ---------------
C            CALINE3 ROUTINE
C            ---------------
C            DETERMINES DIRECTION ALONG LINK
C            +1 --> UPWIND ELEMENTS;  -1 --> DOWNWIND ELEMENTS
C            -------------------------------------------------
 3050        SGN=1.
C
C            *** ELEMENT NUMBER, STEP FACTOR AND LOOP END INITIALIZATION
 3060        NE=0.
             STP=1.
             FINI=1.
C
            IF (SGN.EQ.1. .AND.
     +          UWL.LE.0. .AND.
     +          DWL.LT.0.) SGN=-1.
 3080       IF (SGN.EQ.-1. .AND.
     +          UWL.GT.0. .AND.
     +          DWL.GE.0.) GOTO 6000
C
C           ------------
C           ELEMENT LOOP
C           ------------
C
C           *** INITIALIZATION OF ELEMENT LIMITS
            ED1=0.
            ED2=SGN*W
C
 3110       IF (SGN.EQ.-1.) GOTO 3160
            IF (ED1.LE.DWL .AND. ED2.LE.DWL) GOTO 3770
            IF (ED1.GT.DWL .AND. ED2.LT.UWL) GOTO 3250
            IF (ED1.LE.DWL) ED1=DWL
            IF (ED2.LT.UWL) GOTO 3250
            ED2=UWL
            SGN=-1.
            NE=-1.
            GOTO 3250
 3160       IF (ED1.GE.UWL .AND. ED2.GE.UWL) GOTO 3770
            IF (ED1.LT.UWL .AND. ED2.GT.DWL) GOTO 3250
            IF (ED1.GE.UWL) ED1=UWL
            IF (ED2.GT.DWL) GOTO 3250
            ED2=DWL
            FINI=0.
C
C           *** ELEMENT HALF-DISTANCE
 3250       EL2=ABS(ED2-ED1)/2.
C
C           *** ELEMENT CENTERLINE DISTANCE
            ECLD=(ED1+ED2)/2.
C
C           *** EQUIVALENT LINE HALF-LENGTH
            ELL2=W2/COS(PHI)+(EL2-W2*TAN(PHI))*SIN(PHI)
C
C           *** CENTRAL SUB-ELEMENT HALF-LENGTH
            IF (PHI.GE.ATAN(W2/EL2)) CSL2=W2/SIN(PHI)
            IF (PHI.LT.ATAN(W2/EL2)) CSL2=EL2/COS(PHI)
C
C           *** CENTRAL SUB-ELEMENT HALF-WIDTH
            EM2=ABS((EL2-W2/TAN(PHI))*SIN(PHI))
C
C           *** PERIPHERAL SUB-ELEMENT WIDTH
            EN2=(ELL2-EM2)/2.
C
C           ----------------------
C           RECEPTOR DISTANCE LOOP
C           ----------------------
C
C           *** CENTRAL SUB-ELEMENT LINEAL SOURCE STRENGTH
            QE=Q1*CSL2/W2
C
C           *** ELEMENT FETCH
            FET=(ECLD+D*TAN(PHI))*COS(PHI)
C
C           *** Y DISTANCE FROM ELEMENT CENTER TO RECEPTOR
            HYP=ECLD**2+D**2
            SIDE=FET**2
            IF (SIDE.GT.HYP) YE=0.
            IF (SIDE.LE.HYP) YE=DSQRT(HYP-SIDE)
C
            IF (FET.LE.-CSL2) GOTO 3830
C
C           *** ELEMENT DOES NOT CONTRIBUTE
            IF (FET.GE.CSL2) GOTO 3320
C
C           *** RECEPTOR WITHIN ELEMENT
C               DETERMINE SIGMA Y AND SIGMA Z
            QE=QE*(FET+CSL2)/(2.*CSL2)
            FET=(CSL2+FET)/2.
 3320       SGZ=PZ1*FET**PZ2
            KZ=SGZ**2*U/(2.*FET)
C
C           *** VERTICAL DIFFUSIVITY ESTIMATE
            SGY=PY1*FET**PY2
C
C           *** SOURCE STRENGTH - WIND SPEED FACTOR
            FAC1=0.399/(SGZ*U)
C
C           ----------------------------------
C           ADJUSTMENT FOR ELEMENT END EFFECT
C           (POLYNOMIAL APPROXIMATION)
C           ----------------------------------
            Y(1)=YE+ELL2
            Y(2)=Y(1)-EN2
            Y(3)=Y(2)-EN2
            Y(4)=Y(3)-2*EM2
            Y(5)=Y(4)-EN2
            Y(6)=Y(5)-EN2
C
C           --------------------------------
C           SUB-ELEMENT SOURCE STRENGTH LOOP
C           --------------------------------
C
            DO 3480 I=1,6
              LIM=ABS(Y(I)/SGY)
              T=1./(1.+0.23164*LIM)
              ARG=LIM**2/(-2.)
              IF (LIM.GT.5.) INTG(I)=0.
              IF (LIM.LE.5.) INTG(I)=0.3989*EXP(ARG)*(0.3194*T-0.3566*T
     +                               **2+1.7815*T**3-1.8213*T**4+1.3303
     +                               *T**5)
 3480       CONTINUE
C
            FAC2=0.
            DO 3530 I=1,5
              IF ((SIGN(1.,Y(I))).EQ.(SIGN(1.,Y(I+1))))
     +        PD=DABS(INTG(I+1)-INTG(I))
              IF ((SIGN(1.,Y(I))).NE.(SIGN(1.,Y(I+1))))
     +        PD=1.-INTG(I)-INTG(I+1)
C
C             *** NORMAL PROBABILITY DENSITY FUNCTION
              FAC2=FAC2+PD*QE*WT(I)
 3530       CONTINUE
C
            FACT=FAC1*FAC2
C
C           -----------------
C           DEPRESSED SECTION
C           -----------------
            IF (HDS.LT.-1.5 .AND.DABS(D).LT.(W2-3.*HDS)) GOTO 3560
            GOTO 3580
 3560       IF (DABS(D).LE.W2) FACT=FACT*DSTR
            IF (DABS(D).GT.W2) FACT=FACT*(DSTR-(DSTR-1.)*(DABS(D)-W2)/
     +                              (-3.*HDS))
C
C           *** ADJUST FOR DEPRESSED SECTION WIND SPEED
C
C           *** DEPOSITION CORRECTION
 3580       FAC3=0.
            IF (V1.EQ.0.) GOTO 3670
            ARG=V1*SGZ/(KZ*SQRT(2.))+(Z+H)/(SGZ*SQRT(2.))
            IF (ARG.GT.5.) GOTO 3770
            T=1./(1.+0.47047*ARG)
            EFRC=(.3480242*T-.0958798*T**2+.7478556*T**3)*
     +            EXP(-1.*ARG**2)
            FAC3=(SQRT(2.*PI)*V1*SGZ*EXP(V1*(Z+H)/KZ+.5*(V1*SGZ/KZ)**2)
     +            *EFRC)/KZ
            IF (FAC3.GT.2.) FAC3=2.
C
C
C           *** SETTLING CORRECTION
 3670       IF (VS.EQ.0.) GOTO 3710
            FAC4=EXP(-VS*(Z-H)/(2.*KZ)-(VS*SGZ/KZ)**2/8.)
            FACT=FACT*FAC4
C
C           *** INCREMENTAL CONCENTRATION
 3710       FAC5=0.
            CNT=0.
 3720       EXLS=0.
 3730       ARG1=-0.5*((Z+H+2.*CNT*MIXH)/SGZ)**2
            IF (ARG1.LT.-44.) EXP1=0.
            IF (ARG1.GE.-44.) EXP1=EXP(ARG1)
            ARG2=-0.5*((Z-H+2.*CNT*MIXH)/SGZ)**2
            IF (ARG2.LT.-44.) EXP2=0.
            IF (ARG2.GE.-44.) EXP2=EXP(ARG2)
            FAC5=FAC5+EXP1+EXP2
C
C           --------------------------------
C           BYPASS MIXING HEIGHT CALCULATION
C           --------------------------------
            IF (MIXH.GE.1000.) GOTO 3760
C
            IF ((EXP1+EXP2+EXLS).EQ.0. .AND. CNT.LE.0.) GOTO 3760
 3740       IF (CNT.GT.0.) GOTO 3750
            CNT=ABS(CNT)+1.
            GOTO 3720
 3750       CNT=-1.*CNT
            EXLS=EXP1+EXP2
            GOTO 3730
C
C           *** INCREMENTAL CONCENTRATION FROM ELEMENT
 3760       INC=FACT*(FAC5-FAC3)
C
C           *** SUMMATION OF CONCENTRATIONS
            C(IL,IR)=C(IL,IR)+INC
C
 3770       IF (FINI.EQ.0.) GOTO 6000
            NE=NE+1.
C
C           *** STEP FACTOR
            STP=BASE**NE
C
C           *** INCREMENT TO NEXT ELEMENT
            IF (NE.EQ.0.) GOTO 3080
            ED1=ED2
            ED2=ED2+SGN*STP*W
C
            GOTO 3110
 3830       IF (SGN.EQ.1.) GOTO 3770
C
C
 6000      CONTINUE
 8000     CONTINUE
C
C         -------------------------------------------------------
C         CONVERT CONCENTRATION OF CO FROM MICROGRAMS/M**3 TO PPM
C         -------------------------------------------------------
          DO 1020 I=1,NL
            DO 1010 J=1,NR
              C(I,J)=C(I,J)*FPPM
 1010       CONTINUE
 1020     CONTINUE
C
C
C          ========
C         | OUTPUT |
C          ========
C
          IF(K4.GT.1) GOTO 1249
C
C         *****************************************
C         * PRINT SITE & METEOROLOGICAL VARIABLES *
C         *****************************************
C
          IF(IM.NE.1) THEN
            IF(NEW.EQ.1) THEN
               PGCT=PGCT+1
               ICOUNT=7
               WRITE(IPO,202) CHAR(12), PGCT
               WRITE(IPO,210) JOB,RUN
               WRITE(IPO,221)
               IF (VAR.EQ.'Y') GOTO 1005
                 IF ( FPPM .NE. 1) THEN
                    WRITE(IPO,240) U,CLAS,STB(CLAS),ATIM,MIXH,AMB,BRG1
                   ELSE
                    WRITE(IPO,241) U,CLAS,STB(CLAS),ATIM,MIXH,AMB,BRG1
                 ENDIF
                 GOTO 1249
 1005          IF ( FPPM .NE. 1) THEN
                  WRITE(IPO,231) U,CLAS,STB(CLAS),ATIM,MIXH,AMB
                 ELSE
                  WRITE(IPO,232) U,CLAS,STB(CLAS),ATIM,MIXH,AMB
               END IF
               GOTO 1249
            ENDIF
          ENDIF
C
          IF(IM.NE.1) GOTO 1249
          PGCT=PGCT+1
          WRITE (IPO,200) CHAR(12), PGCT
          WRITE (IPO,210)JOB,RUN
          WRITE (IPO, 462) IMON, IDAY, IYR
          WRITE (IPO, 463) IIHR, IMIN, ISEC
CL          WRITE (IPO,'(6X,A6,A8,2X,A7,A5/)')                            V2EC
CL     +            'DATE: ',IDATE,' TIME: ',ITIME                        V2EC
            IF (MODET .EQ. 0) THEN
              WRITE(IPO,23) MODE(1:1)
            ENDIF
            IF (MODET .EQ. 1) THEN
              WRITE(IPO,24) MODE(1:1)
            ENDIF
          WRITE(IPO,220)
          WRITE(IPO,230)VS1,VD1,Z0
C
          IF(VAR.EQ.'Y') GOTO 1205
 1211          IF ( FPPM .NE. 1) THEN
                    WRITE(IPO,240) U,CLAS,STB(CLAS),ATIM,MIXH,AMB,BRG1
                   ELSE
                    WRITE(IPO,241) U,CLAS,STB(CLAS),ATIM,MIXH,AMB,BRG1
                 ENDIF
                 GOTO 1206
 1205          IF ( FPPM .NE. 1) THEN
                  WRITE(IPO,231) U,CLAS,STB(CLAS),ATIM,MIXH,AMB
                 ELSE
                  WRITE(IPO,232) U,CLAS,STB(CLAS),ATIM,MIXH,AMB
               END IF
C
C         *************************
C         * PRINT LINK VARIABLES  *
C         *************************
C
 1206     ICOUNT=ICOUNT+12
          KQ=1
          DO 1250 I=1,NL
            IF(KQ.EQ.1) THEN
              WRITE(IPO,250)
             IF(IOPT.EQ.1)THEN                                          V2EF
              WRITE(IPO,265)                                            V2EF
              WRITE(IPO,275)                                            V2EF
             ELSE                                                       V2EF
              WRITE(IPO,260)
              WRITE(IPO,270)                                            V2EF
             ENDIF                                                      V2EF
              WRITE(IPO,280)
              ICOUNT=ICOUNT+4
              KQ=0
            ENDIF
C
            IF(ICOUNT.GE.60) THEN
              PGCT=PGCT+1
              WRITE(IPO,202) CHAR(12), PGCT
              WRITE (IPO,210)JOB,RUN
              WRITE (IPO, 462) IMON, IDAY, IYR
              WRITE (IPO, 463) IIHR, IMIN, ISEC
CL          WRITE (IPO,'(6X,A6,A8,2X,A7,A5/)')                            V2EF
CL     +            'DATE: ',IDATE,' TIME: ',ITIME                        V2EF
              WRITE(IPO,250)
             IF(IOPT.EQ.1)THEN                                          V2EF
              WRITE(IPO,265)                                            V2EF
              WRITE(IPO,275)                                            V2EF
             ELSE                                                       V2EF
              WRITE(IPO,260)
              WRITE(IPO,270)                                            V2EF
             ENDIF                                                      V2EF
              WRITE(IPO,280)
              ICOUNT=9
            ENDIF
C
         IF(IOPT.NE.1)THEN                                              V2EF
C                                                                       V2EF
            IF(IQ(I).EQ.2) THEN
            WRITE(IPO,290) COD(I),LNK(I),XL1(I),YL1(I),XL2(I),
     +                   YL2(I),LL(I),LBRG(I),TYP(I),VPHL(I),
     +                   EFL(I),HL(I),WL(I),DC(I),QAVG4(I)
            ELSE
            WRITE(IPO,290) COD(I),LNK(I),XL1(I),YL1(I),XL2(I),
     +                   YL2(I),LL(I),LBRG(I),TYP(I),VPHL(I),
     +                   EFL(I),HL(I),WL(I)
            ENDIF
C
         ELSE                                                           V2EF
C                                                                       V2EF
               XW1=XL1(I)*PSCALE                                        V2EF
               XW2=XL2(I)*PSCALE                                        V2EF
               YW1=YL1(I)*PSCALE                                        V2EF
               YW2=YL2(I)*PSCALE                                        V2EF
               LLW=LL(I)*PSCALE                                         V2EF
               HLW=HL(I)*PSCALE                                         V2EF
               WLW=WL(I)*PSCALE                                         V2EF
            IF(IQ(I).EQ.2)THEN                                          V2EF
            WRITE(IPO,290) COD(I),LNK(I),XW1,YW1,XW2,                   V2EF
     +                   YW2,LLW,LBRG(I),TYP(I),VPHL(I),                V2EF
     +                   EFL(I),HLW,WLW,DC(I),QAVG4(I)                  V2EF
C                                                                       V2EF
            ELSE                                                        V2EF
C                                                                       V2EF
            WRITE(IPO,290) COD(I),LNK(I),XW1,YW1,XW2,                   V2EF
     +                   YW2,LLW,LBRG(I),TYP(I),VPHL(I),                V2EF
     +                   EFL(I),HLW,WLW                                 V2EF
            ENDIF                                                       V2EF
C                                                                       V2EF
         ENDIF                                                          V2EF
            ICOUNT=ICOUNT+1
C
 1250   CONTINUE
C                                                                       V2EF
C         ******************************************                    V2EF
C         * PRINT ADDITIONAL QUEUE LINK PARAMETERS *                    V2EF
C         ******************************************                    V2EF
C                                                                       V2EF
          ICOUNT=61                                                     V2EF
          DO 1154 I=1,NL                                                V2EF
          IF(ICOUNT.GE.60) THEN                                         V2EF
            PGCT=PGCT+1                                                 V2EF
            WRITE(IPO,202) CHAR(12), PGCT                               V2EF
            WRITE (IPO,210)JOB,RUN                                      V2EF
            WRITE (IPO, 462) IMON, IDAY, IYR                            V2EF
            WRITE (IPO, 463) IIHR, IMIN, ISEC                           V2EF
CL          WRITE (IPO,'(6X,A6,A8,2X,A7,A5/)')                          V2EF
CL     +            'DATE: ',IDATE,' TIME: ',ITIME                      V2EF
            ICOUNT = 4                                                  V2EF
            IF (MODET .EQ. 0) THEN                                      V2EF
              WRITE(IPO,401)                                            V2EF
              WRITE(IPO,402)                                            V2EF
              WRITE(IPO,403)                                            V2EF
              WRITE(IPO,404)                                            V2EF
              WRITE(IPO,405)                                            V2EF
              ICOUNT = ICOUNT + 6                                       V2EF
            END IF                                                      V2EF
          ENDIF                                                         V2EF
C                                                                       V2EF
          IF(IQ(I).EQ.2)THEN                                            V2EF
            WRITE(IPO,406)COD(I),LNK(I),CAVG(I),RAVG(I),YFAC(I),        V2EF
     +                  IV(I),SFR(I),IDLFAC(I),ST(I),AT(I)              V2EF
            ICOUNT=ICOUNT+1                                             V2EF
          ENDIF                                                         V2EF
 1154     CONTINUE                                                      V2EF
C                                                                       V2EF
C
C         *****************************************
C         * SUMMARY OUTPUT FOR RECEPTOR LOCATIONS *
C         *****************************************
C
          ICOUNT=ICOUNT+1                                               V2EF
          KQ=1                                                          V2EF
          DO 1151 I=1,NR                                                V2EF
           IF(KQ.EQ.1)THEN                                              V2EF
             WRITE(IPO,300)                                             V2EF
            IF(IOPT.EQ.1)THEN                                           V2EF
             WRITE(IPO,310)                                             V2EF
            ELSE                                                        V2EF
             WRITE(IPO,309)                                             V2EF
            ENDIF                                                       V2EF
             WRITE(IPO,321)                                             V2EF
             WRITE(IPO,331)                                             V2EF
           ICOUNT=ICOUNT+4                                              V2EF
           KQ=0                                                         V2EF
           ENDIF                                                        V2EF
C                                                                       V2EF
          IF(ICOUNT.GE.60) THEN
            PGCT=PGCT+1
            WRITE(IPO,202) CHAR(12), PGCT
            WRITE (IPO,210)JOB,RUN
            WRITE (IPO, 462) IMON, IDAY, IYR
            WRITE (IPO, 463) IIHR, IMIN, ISEC
CL          WRITE (IPO,'(6X,A6,A8,2X,A7,A5/)')                            V2EF
CL     +            'DATE: ',IDATE,' TIME: ',ITIME                        V2EF
            WRITE(IPO,300)
           IF(IOPT.EQ.1)THEN                                            V2EF
            WRITE(IPO,310)
           ELSE                                                         V2EF
            WRITE(IPO,309)
           ENDIF                                                        V2EF
            WRITE(IPO,321)
            WRITE(IPO,331)
            ICOUNT=10                                                   V2EF
          ENDIF                                                         V2EF
C
          IF(IOPT.EQ.1)THEN                                             V2EF
            XWR=XR(I)*PSCALE                                            V2EF
            YWR=YR(I)*PSCALE                                            V2EF
            ZWR=ZR(I)*PSCALE                                            V2EF
            WRITE (IPO,441) I,RCP(I),XWR,YWR,ZWR                        V2EF
           ELSE                                                         V2EF
            WRITE (IPO,441) I,RCP(I),XR(I),YR(I),ZR(I)
           ENDIF                                                        V2EF
            ICOUNT=ICOUNT+1
C
 1151     CONTINUE
C
C         *****************
C         * MODEL RESULTS *
C         *****************
C
C         ----------------------------------------------------------
C         CALCULATION OF THE TOTAL CO CONCENTRATION AT EACH RECEPTOR
C         PRODUCED BY ALL THE LINKS, FOR EACH WIND ANGLE.
C         ----------------------------------------------------------
 1249     DO 1152 I=1,NR
            CSUM=0.0
C
            DO 1252 J=1,NL
              C(J,I)=10.*C(J,I)+.5
              K=C(J,I)
              C(J,I)=K/10.
              CSUM=CSUM+C(J,I)
C
 1252       CONTINUE
C
            CSUM=CSUM+AMB
            CON(I)=CSUM
C
C             *** Keep in memory only the concentrations (for
C                 each receptor per link) for the wind angle that
C                 produces the maximum sums.
C
              IF (K4.EQ.1) THEN
                CRMAX(I)=CSUM
                DO 1153 J=1,NL
                  COMAX(J,I)=C(J,I)
 1153           CONTINUE
                ANGMAX(I)=BRGV(K4)
                IF (CSUM .GT. CMAX) THEN
                  CMAX = CSUM
                  IRMAX = I
                END IF
              ELSE
                IF(CSUM.GT.CRMAX(I)) THEN
                  DO 1254 J=1,NL
                    COMAX(J,I)=C(J,I)
 1254             CONTINUE
                 ANGMAX(I)=BRGV(K4)
                 CRMAX(I) =CSUM
                ENDIF
                IF (CSUM .GT. CMAX) THEN
                  CMAX = CSUM
                  IRMAX = I
                END IF
              END IF
C
 1152     CONTINUE
C
          IF(K4.GT.1) THEN
            IF(ICOUNT.GE.60) THEN
              PGCT=PGCT+1
              WRITE(IPO,202) CHAR(12), PGCT
              WRITE(IPO,210) JOB,RUN
              WRITE(IPO,8500)
              IF (FPPM .NE. 1) THEN
                 WRITE(IPO,8510)
                ELSE
                 WRITE(IPO,8520)
              END IF
              WRITE(IPO,8511) (REC(J),J=1,NR)
              WRITE(IPO,8512) (X1(J),J=1,NR)
              ICOUNT=8
            ENDIF
            GOTO 1189
          ENDIF
C
          LIN=ICOUNT+13+NANGLE+4
          IF(LIN.GE.60) THEN
            PGCT=PGCT+1
            WRITE(IPO,202) CHAR(12), PGCT
            WRITE(IPO,210) JOB,RUN
            ICOUNT=3
          ENDIF
C
          WRITE(IPO,8399)
          WRITE(IPO,8400)
          ICOUNT=ICOUNT+9
C
 1189     IF(K4.GT.1) GOTO 8998
          IF(VAR.EQ.'N') GOTO 1190
C
          TRANGE=BRGV(1)
          BRANGE=BRG2+(DEGR*VA(NANGLE))
          WRITE(IPO,8499) TRANGE,BRANGE
          ICOUNT=ICOUNT+2
C
 1190     WRITE(IPO,8500)
            IF (FPPM .NE. 1) THEN
               WRITE(IPO,8510)
              ELSE
               WRITE(IPO,8520)
            END IF
          IF(NR.LE.20)THEN                                              V2EC
            WRITE(IPO,8511) (REC(J),J=1,NR)
            WRITE(IPO,8512) (X1(J),J=1,NR)
           ELSE                                                         V2EC
            WRITE(IPO,8511) (REC(J),J=1,20)
            WRITE(IPO,8512) (X1(J),J=1,20)
          ENDIF                                                         V2EC
          ICOUNT=ICOUNT+4
C
 8998     IF (NR.LE.20)THEN                                             V2EC
            IF (FPPM .NE. 1) THEN
              WRITE(IPO,8530) BRG1,(CON(J),J=1,NR)
             ELSE
              WRITE(IPO,8531) BRG1,(CON(J),J=1,NR)
            END IF
          ELSE                                                          V2EF
            IF (FPPM .NE. 1) THEN
              WRITE(IPO,8530) BRG1,(CON(J),J=1,20)                      V2EF
             ELSE
              WRITE(IPO,8531) BRG1,(CON(J),J=1,20)
            END IF
            WRITE(16,165) BRG1,(CON(J),J=21,NR)                         V2EF
          ENDIF                                                         V2EC
          ICOUNT=ICOUNT+1                                               V2EC
C
 8999   CONTINUE                                                        V2EC
C
C    ***************************************************
C     CHECK AND PRINT MAXIMUM OF 20 RECEPTORS PER PAGE.                 V2EC
C    ***************************************************
C                                                                       V2EC
        MAXRPT=20                                                       V2EC
        IRP=1                                                           V2EC
C
C    ---------------------------------------
C     CHECK IF RECEPTORS EQUALS 20 OR LESS.
C    ---------------------------------------
C
        IF (NR.LE.MAXRPT) THEN                                          V2EC
           NRP=NR                                                       V2EC
          ELSE                                                          V2EC
           NRP=MAXRPT                                                   V2EC
        ENDIF                                                           V2EC
C
 9019   IF(VAR.EQ.'Y') THEN                                             V2EC
C
        WRITE(IPO,8512) (X1(J),J=IRP,NRP)                               V2EC
        IF (FPPM .NE. 1) THEN
          WRITE(IPO,9040) (CRMAX(I) ,I=IRP,NRP)                         V2EC
         ELSE
          WRITE(IPO,9050) (CRMAX(I) ,I=IRP,NRP)                         V2EC
        END IF
        WRITE(IPO,9042) (ANGMAX(I),I=IRP,NRP)                           V2EC
        WRITE(IPO,'(1X)')
        ENDIF                                                           V2EC
C
        IF(NR.LE.MAXRPT)GO TO 9023                                      V2EC
        MAXRPT=MAXRPT+20                                                V2EC
        IRP=IRP+20                                                      V2EC
         IF(NR.LE.MAXRPT)THEN                                           V2EC
            NRP=NR                                                      V2EC
           ELSE                                                         V2EC
            NRP=MAXRPT                                                  V2EC
         ENDIF                                                          V2EC
        REWIND 16                                                       V2EC
C                                                                       V2EC
        ICOUNT=61                                                       V2EC
        DO 9021 K4=1,NANGLE                                             V2EC
        READ(16,165) BRG1,(CON(J),J=21,NRP)                             V2EC
        IF(ICOUNT.GE.60)THEN                                            V2EC
          PGCT=PGCT+1                                                   V2EC
          WRITE(IPO,202) CHAR(12), PGCT                                 V2EC
          WRITE(IPO,210)JOB,RUN                                         V2EC
          ICOUNT=3                                                      V2EC
C                                                                       V2EC
        IF(K4.EQ.1) THEN                                                V2EC
        WRITE(IPO,8399)                                                 V2EC
        WRITE(IPO,8400)                                                 V2EC
        ICOUNT=ICOUNT+9                                                 V2EC
        ENDIF                                                           V2EC
C                                                                       V2EC
        IF(VAR.EQ.'N') GOTO 1191                                        V2EC
C                                                                       V2EC
        TRANGE=BRGV(1)                                                  V2EC
        BRANGE=BRG2+(DEGR*VA(NANGLE))                                   V2EC
        WRITE(IPO,8499) TRANGE,BRANGE                                   V2EC
        ICOUNT=ICOUNT+2                                                 V2EC
C                                                                       V2EC
 1191   WRITE(IPO,8500)                                                 V2EC
          IF (FPPM .NE. 1) THEN
             WRITE(IPO,8510)                                            V2EC
            ELSE
             WRITE(IPO,8520)                                            V2EC
          END IF
        WRITE(IPO,8511) (REC(J),J=IRP,NRP)                              V2EC
        WRITE(IPO,8512) (X1(J),J=IRP,NRP)                               V2EC
        ICOUNT=ICOUNT+4                                                 V2EC
        ENDIF                                                           V2EC
C                                                                       V2EC
          IF (FPPM .NE. 1) THEN                                         V2EC
            WRITE(IPO,8530) BRG1,(CON(J),J=IRP,NRP)                     V2EC
           ELSE
            WRITE(IPO,8531) BRG1,(CON(J),J=IRP,NRP)
          END IF
        ICOUNT=ICOUNT+1                                                 V2EC
C                                                                       V2EC
 9021   CONTINUE                                                        V2EC
        GO TO 9019                                                      V2EC
C                                                                       V2EC
 9023   CLOSE(16,STATUS='DELETE')                                       V2EC
C                                                                       V2EC
C     PRINT OUT TOP THREE VALUES OF MAXIMUM CONCENTRATION WITH          V2EC
C          CORRESPONDING WIND ANGLE AND RECEPTOR NUMBERS.               V2EC
C                                                                       V2EC
        DO 9026 I=1,NR                                                  V2EC
           IORDER(I)=I                                                  V2EC
 9026   CONTINUE                                                        V2EC

        IF (VAR .EQ. 'N') WRITE(IPO,8512) (X1(J),J=IRP,NRP)
        IF (FPPM .NE. 1) THEN
          WRITE(IPO,9043) CMAX,REC(IRMAX)                               V2EC
         ELSE
          WRITE(IPO,9046) CMAX,REC(IRMAX)
        END IF
        CMAX = 0.0
C
        IF(PRINT2.NE.1) GOTO 9000
C
C    -----------------------------------------------                    V2EC
C     IRP=INITIAL RECEPTOR NUMBER TO PRINT ON PAGE.                     V2EC
C     NRP=LAST RECEPTOR NUMBER TO PRINT ON PAGE.                        V2EC
C    -----------------------------------------------
C                                                                       V2EC
        IRP=1                                                           V2EC
        MAXRPT=20                                                       V2EC
 9025   IF (NR.LE.MAXRPT) THEN                                          V2EC
           NRP=NR                                                       V2EC
          ELSE                                                          V2EC
           NRP=MAXRPT                                                   V2EC
        ENDIF                                                           V2EC
C
          PGCT=PGCT+1
          WRITE(IPO,202) CHAR(12), PGCT
          WRITE(IPO,210) JOB,RUN
          WRITE (IPO, 462) IMON, IDAY, IYR
          WRITE (IPO, 463) IIHR, IMIN, ISEC
CL          WRITE (IPO,'(6X,A6,A8,2X,A7,A5/)')                            V2EC
CL     +            'DATE: ',IDATE,' TIME: ',ITIME                        V2EC
          WRITE(IPO,9032)
 9032     FORMAT (/,6X,'RECEPTOR - LINK MATRIX FOR THE ANGLE PRODUCING',
     +           /,6X,'THE MAXIMUM CONCENTRATION FOR EACH RECEPTOR')
C
          IF (FPPM .NE. 1) THEN
            WRITE(IPO,9031) (REC(LLL),LLL=IRP,NRP)
           ELSE
            WRITE(IPO,9035) (REC(LLL),LLL=IRP,NRP)
          END IF
 9031     FORMAT(/,10X,'*',4X,'CO/LINK  (PPM) ',/,
     +             10X,'*',4X,'ANGLE (DEGREES)',/,
     +             10X,'* ',20(1X,A5))
 9035     FORMAT(/,10X,'*',4X,'PM/LNK(ug/m**3)',/,
     +             10X,'*',4X,'ANGLE (DEGREES)',/,
     +             10X,'* ',20(1X,A5))
          WRITE(IPO,9034) (ANGMAX(LLL),LLL=IRP,NRP)
 9034     FORMAT (3X,'LINK # ','*',20(3X,I3))
C
          WRITE(IPO,9033) (X1(L1),L1=IRP,NRP)
 9033     FORMAT(3X,'-------*',20A6)
C
          ICOUNT=15                                                     V2EC
          DO 9051 I=1,NL
            IF(ICOUNT.GE.60) THEN
              PGCT=PGCT+1
              WRITE(IPO,202) CHAR(12), PGCT
              WRITE(IPO,210) JOB,RUN
          IF (FPPM .NE. 1) THEN
            WRITE(IPO,9031) (REC(LLL),LLL=IRP,NRP)
           ELSE
            WRITE(IPO,9035) (REC(LLL),LLL=IRP,NRP)
          END IF
              WRITE(IPO,9034) (ANGMAX(LLL),LLL=IRP,NRP)
              WRITE(IPO,9033) (X1(L1),L1=IRP,NRP)
              ICOUNT=7
            ENDIF
C
            WRITE(IPO,9041) I,(COMAX(I,J),J=IRP,NRP)
 9041       FORMAT (5X,I3,'  *',20(1X,F5.1))
            ICOUNT=ICOUNT+1
 9051     CONTINUE
C
          IF(NR.LE.MAXRPT) GO TO 9000                                   V2EC
          IRP=MAXRPT+1                                                  V2EC
          MAXRPT=MAXRPT+20                                              V2EC
          GO TO 9025
C
 9000 CONTINUE
      GOTO 10
C
C       -----------------
C       FORMAT STATEMENTS
C       -----------------
C
C       *** INPUT FORMATS
   1    FORMAT(5X,2(I5,5X),F5.1,I5,F7.2,1X,I4,1X,I1,1X,I1)              V2
   11   FORMAT(2F4.0,2F5.0,I2,F10.4)
   12   FORMAT(3F10.0)
   14   FORMAT(I3)
   15   FORMAT(1X,A20,A2,4F7.0,F8.0,F4.0,I4)
   16   FORMAT(4F4.0,F8.0,3F4.0)
   23  FORMAT(9X,'The MODE flag has been set to ',A1,' for calculating',
     *   ' CO averages.')
   24  FORMAT(9X,'The MODE flag has been set to ',A1,' for calculating',
     *   ' PM averages.')
  120   FORMAT (1X,A40,2F4.0,2F5.0,I2,F10.0,4X,I1,4X,I1)                V2ECF
  130   FORMAT (1X,A20,3F10.0)
  150   FORMAT (1X,A40,2I3,2X,I2,2X,A1)
  160   FORMAT (1X,A20,A2,4F7.0,F8.0,3F4.0)
  165   FORMAT (F6.1,40F6.2)                                            V2EC
  170   FORMAT (1X,//,22HPROGRAM RUN TERMINATED,///,
     + 61H * * LINK LENGTH MUST BE GREATER THAN OR EQUAL TO LINK WIDTH.)
 
  180   FORMAT (1X,//,22HPROGRAM RUN TERMINATED,///,
     +    46H * * SOURCE MUST BE WITHIN 10 METERS OF DATUM.)
  190   FORMAT(1X,F3.0,F4.0,I3,F6.0,F4.0,1X,A1,I3,2I3)
C
C       ------------------------
C       OUTPUT FORMAT STATEMENTS
C       ------------------------
  200   FORMAT (A1,24X,'CAL3QHC: LINE SOURCE DISPERSION MODEL',
     +    ' - VERSION 2.0 Dated 95221',24X,'PAGE ',I2,/)
  202   FORMAT (A1,112X,'PAGE ',I2)
C
  210   FORMAT (6X,5HJOB: ,A40,13X,5HRUN: ,A40)
C
  220   FORMAT (/,7X,'SITE & METEOROLOGICAL VARIABLES  ',/,
     +            7X,'-------------------------------')
C
  230   FORMAT (7X,5HVS = ,F5.1,5H CM/S,7X,5HVD = ,F5.1,5H CM/S,
     +          7X,5HZ0 = ,F4.0,3H CM)
  231   FORMAT (8X, 4HU = ,F4.1,4H M/S,9X,9HCLAS =   ,I1,3H  (,A1,1H),
     +        5X,7HATIM = ,F4.0,8H MINUTES,5X,7HMIXH = ,F6.0,2H M,
     +         3X,6HAMB = ,F4.1,4H PPM/)
  232   FORMAT (8X, 4HU = ,F4.1,4H M/S,9X,9HCLAS =   ,I1,3H  (,A1,1H),
     +        5X,7HATIM = ,F4.0,8H MINUTES,5X,7HMIXH = ,F6.0,2H M,
     +         3X,6HAMB = ,F4.1,' ug/m**3'/)
C
  221   FORMAT (/,7X,'METEOROLOGICAL VARIABLES  ',/,
     +            7X,'------------------------')
  240   FORMAT (8X, 4HU = ,F4.1,4H M/S,9X,9HCLAS =   ,I1,3H  (,A1,1H),
     +        5X,7HATIM = ,F4.0,8H MINUTES,5X,7HMIXH = ,F6.0,2H M,
     +    3X,6HAMB = ,F4.1,4H PPM,2X,6HBRG = ,F4.0,8H DEGREES/)
  241   FORMAT (8X, 4HU = ,F4.1,4H M/S,9X,9HCLAS =   ,I1,3H  (,A1,1H),
     +        5X,7HATIM = ,F4.0,8H MINUTES,5X,7HMIXH = ,F6.0,2H M,
     +    3X,6HAMB = ,F4.1,' ug/m**3',2X,6HBRG = ,F4.0,8H DEGREES/)
  250   FORMAT (7X,'LINK VARIABLES',/,
     +          7X,'--------------' )
  260   FORMAT (9X,16HLINK DESCRIPTION,5X,
     +  42H*         LINK COORDINATES (M)           *,4X,6HLENGTH,
     +    2X,3HBRG,1X,4HTYPE,3X,3HVPH,4X,2HEF,6X,1HH,
     +    3X,1HW,4X,3HV/C,1X,5HQUEUE)
  265   FORMAT (9X,16HLINK DESCRIPTION,5X,
     +  42H*         LINK COORDINATES (FT)          *,4X,6HLENGTH,
     +    2X,3HBRG,1X,4HTYPE,3X,3HVPH,4X,2HEF,6X,1HH,
     +    3X,1HW,4X,3HV/C,1X,5HQUEUE)
  270   FORMAT(30X,42H*   X1        Y1        X2        Y2     *,5X,3H(M
     +),3X,5H(DEG),12X,6H(G/MI),3X,3H(M),1X,3H(M),7X,5H(VEH))
  275   FORMAT(30X,42H*   X1        Y1        X2        Y2     *,5X,4H(FV2EF
     +T),2X,5H(DEG),12X,6H(G/MI),2X,4H(FT),1X,4H(FT),7X,5H(VEH))        V2EF
  280   FORMAT (6X,24(1H-),1H*,40(1H-),1H*,58(1H-))
C
  290   FORMAT (5X,I3,2H. ,A20,1H*,4(1X,F8.1,1X),1H*,4X,F5.0,
     +    3X,F4.0,1X,A2,1X,F7.0,1X,F5.1,1X,F5.1,1X,F4.1,1X,F4.2,1X,F5.1)
C
  300   FORMAT (/,7X,'RECEPTOR LOCATIONS',/,
     +            7X,'------------------')
  309   FORMAT (30X,39H*           COORDINATES (M)           *)
  310   FORMAT (30X,39H*           COORDINATES (FT)          *)
  321   FORMAT (9X,8HRECEPTOR,13X,
     + 39H*      X          Y          Z        *)
  331   FORMAT (5X,25(1H-),1H*,37(1H-),1H*)
C
  401   FORMAT (7X,'ADDITIONAL QUEUE LINK PARAMETERS',/,                V2EF
     +          7X,'--------------------------------' )                 V2EF
  402   FORMAT (9X,16HLINK DESCRIPTION,5X,                          5H* V2EF
     +    ,5HCYCLE,4X,3HRED,5X,9HCLEARANCE,2X,8HAPPROACH,               V2EF
     +2X,10HSATURATION,3X,4HIDLE,3X,6HSIGNAL,3X,7HARRIVAL)              V2EF
  403   FORMAT(30X,4H*   ,1X,6HLENGTH,3X,4HTIME,4X,9HLOST TIME,4X,      V2EF
     +  3HVOL,5X,9HFLOW RATE,3X,6HEM FAC,3X,4HTYPE,5X,4HRATE)           V2EF
  404   FORMAT(30X,4H*   ,2X,5H(SEC),3X,5H(SEC),4X,5H(SEC),6X,          V2EF
     +  5H(VPH),6X,5H(VPH),4X,7H(gm/hr))                                V2EF
  405   FORMAT (6X,24(1H-),1H*,80(1H-))                                 V2EF
C                                                                       V2EF
  406   FORMAT (5X,I3,2H. ,A20,1H*,2(3X,I5,1X),4X,F5.1,                 V2EF
     +    5X,I5,7X,I4,4X,F7.2,6X,I1,8X,I1)                              V2EF
C                                                                       V2EF
  441   FORMAT (5X,I2,2H. ,A20,1X,1H*,4X,F8.1,3X,F8.1,3X,F8.1,
     +    3X,1H*)
  461   FORMAT (30X,27H*           COORDINATES (M),11X,1H*)
  462   FORMAT(/6X,'DATE : ',I2,'/',I2,'/',I2)
  463   FORMAT(6X,'TIME : ',I2,':',I2,':',I2/)
C
 8399   FORMAT(/,7X,'MODEL RESULTS',/,
     +           7X,'-------------')
 8400   FORMAT(/,7X,'REMARKS : In search of the angle corresponding to',
     +        /,17x,'the maximum concentration, only the first',/,
     +          17x,'angle, of the angles with same maximum',/,
     +          17x,'concentrations, is indicated as maximum.')
 8499   FORMAT(/,1X,'WIND ANGLE RANGE: ',F4.0,'-',F4.0)
 8500   FORMAT(/,1X,'WIND  ',1H*,1X,'CONCENTRATION ')
 8510   FORMAT(  1X,'ANGLE ',1H*,6X,'(PPM)')
 8511   FORMAT(  1X,'(DEGR)',1H*,20(1X,A5))
 8512   FORMAT(  1X,'------',1H*,20A6)
 8520   FORMAT(  1X,'ANGLE ',1H*,6X,'(ug/m**3)')
 8530   FORMAT(   1X,F4.0,2X,1H*,20(1X,F5.1))
 8531   FORMAT(   1X,F4.0,2X,1H*,20(1X,F5.0))
 9040   FORMAT(    ' MAX   ',1H*,20(1X,F5.1))
 9050   FORMAT(    ' MAX   ',1H*,20(1X,F5.0))
 9042   FORMAT(    ' DEGR. ',1H*,20(1X,I4,1X))
 9043   FORMAT(' THE HIGHEST CONCENTRATION OF  ',F6.2,' PPM',           V2EC
     +    ' OCCURRED AT RECEPTOR ',A5,'.')                              V2EC
 9046   FORMAT(' THE HIGHEST CONCENTRATION OF  ',F6.0,' ug/m**3',
     +    ' OCCURRED AT RECEPTOR ',A5,'.')
C
 9999   STOP
        END
