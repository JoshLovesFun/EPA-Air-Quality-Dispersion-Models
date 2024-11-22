C        METPROCESSOR COMMON BLOCKS
C
         CHARACTER*4 ITEM
         CHARACTER*6 ACTION
         CHARACTER*8 DISPMD
C
         INTEGER     NITEM,NACT,NDISP
         INTEGER     MPYR,MPCMO,MPCDY,MPJDY,MPMLVL,MPMHRS,CONTRL,MDSTAT
         INTEGER     PGSTAB0,PGSTAB,KEYLVL,ZONE,PGSCHM,TMPLVL,STKLVL
         INTEGER*2   IRND
         INTEGER     WDNUM,WSNUM,PGNUM,ZINUM,TTNUM,WSCLSS,PCNTRL,ISCVAR
C
         REAL        STKHGT,ANEHGT,PLMTOP,HILLHT,MPHGTS,FRHGTS
         REAL        MPLAT, MPLON, TSR,TSS,SOLANG
         REAL        MPZO,SPEED,TEMP,FLWVEC,RANFLW,ZIHGTS,SE,SA
         REAL        FREQ, ZIFREQ, TTAVG, WSAVG, TMPHGT
C
         PARAMETER   (MPMLVL=20,MPMHRS=24,NITEM=9,NACT=9,NDISP=18)
C
         COMMON/MPBK01/ ITEM(NITEM),ACTION(NACT),DISPMD(NDISP)
         COMMON/MPBK02/ MPYR,MPCMO,MPCDY,MPJDY,MDSTAT,KEYLVL,
     1                  CONTRL(NITEM),PGSTAB0(MPMHRS),
     1                  PGSTAB(MPMHRS),ZONE,TMPLVL,STKLVL
         COMMON/MPBK03/ STKHGT,ANEHGT,PLMTOP,HILLHT,TMPHGT,ISCVAR,
     1                  MPLAT, MPLON, TSR,TSS,SOLANG(24),MPZO,
     1                  MPHGTS(MPMLVL),FRHGTS(MPMLVL),
     1                  SPEED(MPMLVL,MPMHRS),TEMP(MPMLVL,MPMHRS),
     1                  FLWVEC(MPMLVL,MPMHRS),
     1                  RANFLW(MPMHRS),ZIHGTS(2,MPMHRS),
     1                  SE(MPMLVL,MPMHRS),SA(MPMLVL,MPMHRS)
         COMMON/MPBK04/ IRND(24,366)
         COMMON/MPBK05/ WDNUM(2),WSNUM(3),PGNUM(2),ZINUM(2,2,7),
     1                  TTNUM(2),WSCLSS(5),PCNTRL,PGSCHM(7)
         COMMON/MPBK06/ FREQ(6,6,36),ZIFREQ(2,7,7),TTAVG, WSAVG(2,6)
C
C     MPMLVL    MAXIMUM NUMBER OF LEVELS PROVIDED FOR IN OUTPUT
C               METEOROLOGY (PRESENTLY NOT ACTIVATED)
C
C     MPMHRS    MAXIMUM NUMBER OF HOURS PROCESSED WITHIN A DAY
C
C     NITEM     NUMBER OF METHODOLOGIES
C
C     NACT      NUMBER OF ACTIONS
C
C     NDISP     NUMBER OF DISPERSION MODELS
C
C                EXAMPLE INPUT LINE TO METPROCESSOR
C
C               JB  VBL  ITEM  ACTION  XXXX  XXXX  XXXX
C
C      VBL      3-CHARACTER KEYWORD THAT INSTRUCTS METPROCESSOR
C               THAT USER WISHES TO OVERRIDE AN INTERNAL DEFAULT
C               PROCESSING METHODOLOGY( ITEM).
C
C      ITEM     4-CHARACTER KEYWORD USED TO INDICATE WHICH
C               METPROCESSOR METHODOLOGY IS TO BE REDEFINED.
C
C     ACTION    A 6-CHARACTER KEYWORD THAT INSTRUCTS THE METPROCESSOR
C               HOW 'ITEM' IS TO BE ACCOMPLISHED.
C
C               WIND - DEFINES HOW THE WIND DIRECTION AND WIND
C                      SPEED ARE TO BE DETERMINED.
C
C                      NWSWXX - (DEFAULT) USE NWS SFC HOURLY DATA.
C                      ONSITE - USE ON SITE OBSERVATIONS, USER IS
C                               TO PROVIDE XXXX WHICH REPRESENTS
C                               STACK HEIGHT OF SOURCE(S).
C
C               TEMP - DEFINES HOW THE AMBIENT AIR TEMPERATURE
C                      IS TO BE DEFINED.
C
C                      NWSWXX - (DEFAULT) USE NWS SFC HOURLY DATA.
C                      ONSITE - USE ON SITE OBSERVATIONS, USER IS
C                               TO PROVIDE XXXX WHICH REPRESENTS
C                               STACK HEIGHT OF SOURCE(S).
C
C               TURB - DEFINES HOW THE VERTICAL AND LATERAL TURBULENCE
C                      INTENSITIES (USED FOR DISPERSION ESTIMATION) ARE
C                      TO BE DEFINED.
C
C                      (NOT ACTIVATED)
C
C               MHGT - DEFINES HOW THE MIXING HEIGHT IS TO BE DEFINED.
C
C                      NWSWXX - (DEFAULT) USE NWS TWICE-DAILY MIXING
C                      ONSITE - USE ON SITE OBSERVATIONS
C
C               TGRD - DEFINES HOW THE TWO TEMPERATURE GRADIENTS (USED
C                      IN RTDM) ARE TO BE DEFINED.
C
C                      (NOT ACTIVATED)
C
C               STAB - DEFINES HOW THE PASQUIL STABILITY CATEGORY IS TO
C                      BE DEFINED.
C
C                      NWSWXX - (DEFAULT) USE NWS SFC HOURLY DATA
C                      ONSITE - USE ON SITE WIND AND CLOUD DATA
C                      SESITE - USE ON SITE SIGMA-E DATA
C                      SASITE - USE ON SITE SIGMA-A DATA
C                      WNDXXX - USE ON SITE WIND AND NWS CLOUD DATA
C                      TTDIFF - USE ON SITE WIND, SOLAR RADIATION,
C                               AND TEMPERATURE DIFFERENCE DATA
C                      USERIN - USE ON SITE STABILITY CLASS PROVIDED
C                               BY USER
C
C               HFLX - DEFINES HOW THE SURFACE HEAT FLUX IS TO BE
C                      DEFINED.
C
C                      (NOT ACTIVATED)
C
C               USTR - DEFINES HOW THE SURFACE FRICTION VELOCITY IS TO
C                      BE DEFINED.
C
C                      (NOT ACTIVATED)
C
C               HGTS - DEFINES HOW THE LEVELS AT WHICH METEOROLOGY ARE
C                      TO BE OUTPUT ARE TO BE DETERMINED.
C
C                      (NOT ACTIVATED)
C
C     DISPMD    ARRAY OF POSSIBLE REGUALTORY DISPERSION MODEL FOR WHICH
C               METEOROLOGY DATA CAN BE PREPROCESSED.
C
C     MDSTAT    INDEX WITHIN DISPMD, DISPERSION MODEL DEFINED BY USER
C
C     CONTRL
C      1        WIND PROCESSING METHODOLOGY
C      2        TEMPERATURE PROCESSING METHODOLOGY
C      3        MIXING HEIGHT PROCESSING METHODOLOGY
C      4        PASQUILL STABILITY PROCESSING METHODOLOGY
C      5        TEMPERATURE GRADIENT PROCESSING METHODOLOGY (NOT
C               ACTIVATED)
C      6        TURBULENCE PROCESSING METHODOLOGY (NOT ACTIVATED)
C      7        HEAT FLUX PROCESSING METHODOLOGY (NOT ACTIVATED)
C      8        FRICTION VELOCITY PROCESSING METHODOLOGY (NOT ACTIVATED)
C      9        METHOD FOR DETERMINING OUTPUT LEVELS OF METEOROLOGICAL
C               DATA. (NOT ACTIVATED)
C
C     PGSTAB0   PASQUILL STABILITY CATEGORY DETERMINED FOR HOUR(S)
C     PGSTAB    PASQUILL STABILITY CATEGORY DETERMINED FOR HOUR(S) WITH
C               THE RAMMET CONDITION THAT CHANGE BETWEEN HOURS .LE. 1
C     SPEED     OUTPUT WIND SPEED FOR HOUR(S), M/S, AND MPGHTS.
C               CURRENTLY ONLY ONE LEVEL IS OUTPUT.
C     TEMP      OUTPUT AMBIENT TEMPERATURE FOR HOUR(S), DEG-C UNLESS
C               DISPERSION MODEL REQUIRES KELVIN.
C               CURRENTLY ONLY ONE LEVEL IS OUTPUT.
C     FLWVEC    WIND DIRECTION/OR FLOW VECTOR (DIRECTION TOWARDS WHICH
C               WIND IS BLOWING), DEG, FOR HOUR(S) AND MPHGTS.
C               CURRENTLY ONLY ONE LEVEL IS OUTPUT.
C     SE        SIGMA-E VALUES, RADIANS, FOR HOUR(S) AND MPHGTS,
C               (NOT ACTIVATED)
C     SA        SIGMA-A VALUES, RADIANS, FOR HOUR(S) AND MPHGTS,
C               (NOT ACTIVATED)
C     RANFLW    WIND DIRECTION/ OR WIND FLOW VECTOR (RANDOMIZED +5 OR
C               -4 DEGREES), DEG, FOR HOUR(S).
C     ZIHGTS    MIXING HEIGHTS (M) FOR HOUR(S). SUBSCRIPT 1) YIELDS
C               RURAL VALUES, AND SUBSCRIPT 2) YIELDS URBAN VALUES.
C     IRND      RANDOM NUMBERS GENERATED FOR HOUR AND JULIAN DAY
C     STKHGT    USER SPECIFIED STACK HEIGHT (M)
C     ANEHGT    USER SPECIFIED ANEMOMETER HEIGHT (M), DEFINES LEVEL
C               OF ON SITE DATA TO BE USED FOR DETERMINING THE
C               STABILITY CATEGORY, DEFAULT IS 10 M.
C     KEYLVL    INDEX POINTING WITHIN OSHT-ARRAY, TO LEVEL CLOSEST
C               TO ANEHGT
C     ISCVAR    KEY IDENTIFYING WHICH USER-DEFINED OS VARIABLE IS THE
C               STABILITY CLASS
C     MPZO      SURFACE ROUGHNESS LENGTH FOR GIVEN HOUR, THIS IS
C               DETERMINED HOURLY, AS NEEDED
C     PLMTOP    USER SPECIFIED HEIGHT FOR 'PLUME TOP' (M), USED AS
C               UPPER LEVEL IN DEFINING TEMPERATURE GRADIENT FOR
C               RTDM (NOT ACTIVATED)
C     HILLHT    USER SPECIFIED HEIGHT FOR 'HILL TOP' (M), USED AS
C               UPPER LEVEL IN DEFINING TEMPERATURE GRADIENT FOR
C               RTDM (NOT ACTIVATED)
C     MPHGTS    METPROCESSOR HEIGHTS, M, USED IN OUTPUT METEOROLOGY
C               (NOT ACTIVATED)
C     FRHGTS    FRACTIONS OF CURRENT HOUR'S MIXING HEIGHT, ONE
C               POSSIBLE METHOLDOLOGY FOR SPECIFYING MPHGTS
C
C     WDNUM     1 = NUMBER OF VALID WIND DIRECTIONS STORED IN
C                   ARRAY RANFLW
C               2 = NUMBER OF MISSING VALUES OF WIND DIRECTION
C                   FOUND WITHIN ARRAY RANFLW
C
C     WSNUM     1 = NUMBER OF VALID WIND SPEEDS STORED IN
C                   ARRAY SPEED(1,J)
C               2 = NUMBER OF MISSING VALUES OF WIND SPEED FOUND
C                   WITHIN ARRAY SPEED(1,J)
C               3 = NUMBER OF CALMS
C
C     PGNUM     1 = NUMBER OF VALID PASQUILL STABILITY CATEGORIES
C                   STORED IN ARRAY PGSTAB
C               2 = NUMBER OF MISSING VALUES OF PASQUILL CATEGORIES
C                   FOUND WITHIN ARRAY PGSTAB
C
C     ZINUM     1 = NUMBER OF VALID MIXING HEIGHTS STORED IN
C                   ARRAY ZIHGTS(J,K), I= 1(RURAL); 2(URBAN);
C                   K(HOUR)
C                     ZINUM(J,K,L) WHERE J = 1(VALID), 2(MISSING);
C                                        K = 1(RURAL), 2(URBAN);
C                                        L = PASQUILL CATEGORY, WITH 7 USED
C                                            FOR CASES WHEN PASQUILL
C                                            CATEGORY IS MISSING.
C               2 = NUMBER OF MISSING VALUES OF MIXING HEIGHTS FOUND
C                   WITHIN ARRAY ZIHGTS
C
C     TTNUM     1 = NUMBER OF VALID TEMPERATURES
C                   STORED IN ARRAY TEMP(1,J)
C               2 = NUMBER OF MISSING VALUES OF TEMPERATURE
C                   FOUND WITHIN ARRAY TEMP(1,J)
C
C     ZIFREQ(L,I,J) SUMMARY OF ZI VALUES BY PASQUILL CATEGORY (I) AND
C                   ZI HEIGHT-RANGE (J), FOR THOSE CASES WHEN BOTH
C                   PASQUILL CATEGORY AND MIXING HEIGHT ARE VALID.
C                   L = 1 (RURAL) AND 2 ( URBAN)
C                   I = 1-6, WITH 7 REPRESENTING SUMMATION OF ALL
C                       VALUES FOR GIVEN HEIGHT RANGE.
C                   J = 1-6, WITH 7 REPRESENTING AVERAGE FOR GIVEN
C                       PASQUILL CATEGORY.  HEIGHT RANGES ARE
C                       0-250,250-500,500-1000,1000-1500,1500-2000,
C                       >2000 METERS.
C
C     FREQ(I,J,L)   JOINT FREQUENCY FUNCTION OF OCCURRANCES OF WIND SPEED
C                   WIND DIRECTION AND PASQUILL STABILITY CATEGORIES.
C                   I = (1-6, PASQUILL CATEGORY)
C                   K = (1-6, WIND SPEED CLASS), WIND SPEED CLASSES ARE
C                       DEFINED IN TERMS OF INTEGER VALUES OF WIND SPEED
C                       IN KNOTS (0-3,4-6,7-10,17-21,>21)
C                   J = (EITHER 1-16, OR 1-36, WIND DIRECTION RANGE), THE
C                       RANGE IS EITHER 22.5 OR 10 DEGREES, WITH THE FIRST
C                       RANGE CENTERED ON NORTH.
C
C    TTAVG          AVERAGE TEMPERATURE (FOR TOTAL PERIOD)
C
C    WSAVG(I)       I=1-6 (FOR SIX STABILITY CLASSES), AND 7 FOR CASES
C                   WHEN STABILITY CLASSES IS MISSING.
C
C    WSCLSS         UPPER BOUND VALUES FOR WIND SPEED CLASSES
C
C    PCNTRL         CONTROL FLAG FOR PRINTED OUTPUT.
C                   LE. 0 = NO OUTPUT
C                   GT. 0 = LIST GENERATED METEOROLOGY
C                   (DEFAULT IS PCNTRL = 0)

C      mp2.inc
C
C     METPROCESSOR COMMON BLOCKS (CONTINUED)
C
      INTEGER CCVR,IPPTYP
      REAL AMBT,WSPEED,WDIRN,P,RN,QR,PRECIP
      REAL BOWEN,ALBEDO,RHO,ZO, Z0APPL, CSUBG, OBUMIN, ANTHRO, XLAI
      REAL ZREF
      REAL HFLUX,USTAR,MOL,ZTREF
      
      COMMON /MPCONS/ RAD2DG,DCOEFF
      COMMON /MPSUB1/ AMBT(MPMHRS),WSPEED(MPMHRS),WDIRN(MPMHRS),
     &                P(MPMHRS)
      COMMON /MPSUB2/ RN(MPMHRS),QR(MPMHRS),CCVR(MPMHRS)
      COMMON /MPSFC1/ HFLUX(MPMHRS),USTAR(MPMHRS),MOL(MPMHRS)
      COMMON /MPSFC2/ BOWEN(MPMHRS), ALBEDO(MPMHRS), RHO(MPMHRS)
      COMMON /MPSFC3/ ZO(MPMHRS), ZREF(MPMHRS), ZTREF(MPMHRS)
      COMMON /MPISC/  Z0APPL(MPMHRS), CSUBG(MPMHRS), OBUMIN(MPMHRS),
     &                ANTHRO(MPMHRS), XLAI(MPMHRS)
      COMMON /MPPPT1/ IPPTYP(MPMHRS),PRECIP(MPMHRS)

C
C  COMMON MPCONS :  RAD2DG: Constant to convert between radians & degrees
C                           (=57.29578 degrees per radian)
C                   DCOEFF: Coefficient used in computing displacement ht.
C
C  COMMON MPSUB1 :  T      : Surface temperature
C                   WSPEED : Surface wind speed (meters/second)
C                   WDIRN  : Surface wind direction (degrees from north)
C                   P      : Surface pressure array (millibars)
C
C  COMMON MPSUB2 :  RN     : Net radiation (Watts/sq.meter)
C                   QR     : Incoming radiation (Watts/sq.meter)
C                   CCVR   : Cloud cover
C    
C  COMMON MPSFC1 :  HFLUX  : Sensible heat flux (Watts/sq.meter)
C                   USTAR  : Friction velocity (meters/second)
C                   MOL    : Monin-Obukhov length (meters)
C                   
C  COMMON MPSFC2 :  BOWEN  : Bowen ratio
C                   ALBEDO : Noon-time surface albedo
C                   RHO    : Density of air (kilograms/cu.meter)
C                   
C  COMMON MPSFC3 :  ZO     : Surface roughness length at the site where
C                            meteorological observations are taken (meters)
C                   ZREF   : Reference height array for wind speed and
C                            wind direction (meters)
C                   ZTREF  : Reference height array for surface temp. (meters)
C
C  COMMON MPISC  :  Z0APPL : Roughness length at the source location (meters)
C                   CSUBG  : Fraction of the net radiation absorbed by the
C                            ground
C                   OBUMIN : Minimum Monin-Obukhov length for stable
C                            atmospheric conditions, L > 0 (meters)
C                   ANTHRO : Contribution to the heat flux for urban
C                            applications (Watts/sq.meter)
C                   XLAI   : Leaf area index (for use in gas deposition)
C
C  COMMON MPPPT1 :  IPPTYP : Precipitation type (liquid takes precedence)
C                   PRECIP : Hourly procipitation amounts