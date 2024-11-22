C              OFFSHORE AND COASTAL DISPERSION MODEL (OCD)              OCD00010
C                          --VERSION 5.0--                              OCD00020
C       LEVEL:  00006  (January 6, 2000)                                OCD00030
C******************************************************************************C
C                                                                              C
C                                 Level 00006                                  C
C                                                                              C
C                           SEE MCB # 3 FOR OCD                                C
C                                                                              C
C                EPA SUPPORT CENTER FOR REGULATORY AIR MODELS                  C
C                                                                              C
C******************************************************************************C
C       Major program limits are specified in the include file,
C       PARAMS.CMN, via the following parameters:
c       maxrec:   Maximum number of receptors
c       maxp:     Maximum number of point sources
c       maxmap:   Maximum no. of grids, in x- and y-directions, to
c                 represent shoreline geometry (recommended value=120)
c       maxring:  Maximum number of rings for polar receptors
c       maxcar:   Maximum number of Cartesian receptors along
c                 the x- and y-axis
C
C       CAN USE OVERWATER RECEPTORS                                     OCD0003B                   
C                                                                       OCD00040
C       SECTION A -  GENERAL REMARKS                                    OCD00050
C                                                                       OCD00060
C                                                                       OCD00070
C       OCD PROGRAM ABSTRACT                                            OCD00080
C                                                                       OCD00090
C            THE OCD MODEL CAN SIMULATE THE TRANSPORT AND DISPERSION    OCD00100
C       OF POLLUTANTS EMITTED FROM OFFSHORE POINT SOURCES THAT AFFECT   OCD00110
C       COASTAL AREAS.  A MIXTURE OF OFFSHORE AND ONSHORE SOURCES MAY   OCD00120
C       BE SPECIFIED.  THE MODEL IS BASED UPON THE MPTER SOURCE CODE,   OCD00130
C       WHICH HAS BEEN MODIFIED TO ACCOUNT FOR THE FOLLOWING PHENOMENA: OCD00140
C                                                                       OCD00150
C       *  AERODYNAMIC DOWNWASH BEHIND PLATFORMS, SHIPS, AND OTHER      OCD00160
C          OFFSHORE STRUCTURES ASSOCIATED WITH A POINT SOURCE;          OCD00170
C       *  DIFFERENCES BETWEEN METEOROLOGICAL PARAMETERS AND DISPERSION OCD00180
C          CHARACTERISTICS OVER LAND AND WATER;                         OCD00190
C       *  CHEMICAL TRANSFORMATIONS OF POLLUTANTS AS A FUNCTION OF      OCD00200
C          MONTH AND DAY VS. NIGHT;                                     OCD00210
C       *  ON-SITE ESTIMATES OF VERTICAL AND HORIZONTAL DISPERSION BASEDOCD00220
C          UPON MEASUREMENTS OF TURBULENCE INTENSITIES, OVER BOTH WATER OCD00230
C          AND LAND;                                                    OCD00240
C       *  CHANGE IN DISPERSION AT THE SHORELINE (WHERE PLUME ENTERS THEOCD00250
C          OVERLAND LAYER OR TIBL - THERMAL INTERNAL BOUNDARY LAYER);   OCD00260
C       *  COASTLINE FUMIGATION;                                        OCD00270
C       *  PARTIAL PLUME PENETRATION OF ELEVATED INVERSIONS;            OCD00280
C       *  EFFECTS OF COMPLEX TERRAIN ON PLUME PATH AND TIBL HEIGHT.    OCD00290
C       *  ABILITY TO MODEL POINT, LINE, OR AREA SOURCES                OCD00300
C                                                                       OCD00310
C            A TOTAL OF MAXP POINT SOURCES, OR FIVE AREA SOURCES, OR    OCD00320
c       (MAXP is defined in PARAMS.CMN.)
C       ONE LINE SOURCE SEGMENT WITH MAXREC RECEPTORS MAY BE USED       OCD00330
c       (MAXREC is defined in PARAMS.CMN.)
C       IN THE OCD MODEL.  SIMULATION IS DONE ON AN HOURLY BASIS.       OCD00340
C       METEOROLOGICAL INPUT CONSISTS OF A DATA SET REPRESENTATIVE OF   OCD00350
C       CONDITIONS OVER THE LAND AND A SECOND DATA SET FOR              OCD00360
C       CONDITIONS OVER THE WATER.  HOURLY CONCENTRATION ESTIMATES CAN  OCD00370
C       BE SUMMED TO DETERMINE AVERAGES FOR MULTIPLE-HOUR PERIODS.      OCD00380
C                                                                       OCD00390
C                                                                       OCD00400
C       OCD MODEL AUTHORS:                                              OCD00410
C       EQUATIONS:  STEVEN R. HANNA                                     OCD00420
C       CODING:     DONALD C. DICRISTOFARO AND JOSEPH C. CHANG          OCD00430
C           SIGMA RESEARCH CORPORATION                                  OCD00440
C           196 BAKER AVENUE                                            OCD00450
C           CONCORD, MA 01742                                           OCD00460
C                                                                       OCD00470
C           PHONE: (508) 371-4200    FAX: (508) 371-4280                OCD00480
C                                                                       OCD00490
C       DEVELOPMENT OF THE OCD MODEL WAS FUNDED BY THE                  OCD00500
C       MINERALS MANAGEMENT SERVICE OF THE U.S. DEPARTMENT OF THE       OCD00510
C       INTERIOR UNDER CONTRACT NO. 14-12-0001-30396                    OCD00520
C                                                                       OCD00530
C       PROJECT OFFICER:                                                OCD00540
C                                                                       OCD00550
C          JEFF ZIPPIN                                                  OCD00560
C          MINERALS MANAGEMENT SERVICE                                  OCD00570
C          U.S. DEPARTMENT OF THE INTERIOR                              OCD00580
C          PARKWAY ATRIUM BUILDING                                      OCD00590
C          381 ELDEN STREET                                             OCD00600
C          HERNDON, VA  22070-4817                                      OCD00610
C                                                                       OCD00620
C      ************************************************************     OCD00630
C      *            IMPORTANT MESSAGE NO. 1 ]]]                   *     OCD00640
C      *       CALCULATIONS SUBMITTED TO SATISFY REGULATORY       *     OCD00650
C      *  REQUIREMENTS MAY REQUIRE CERTAIN PARAMETER VALUES       *     OCD00660
C      *  FOR WIND PROFILE POWER-LAW EXPONENTS AND TERRAIN        *     OCD00670
C      *  ADJUSTMENT FACTORS AND USE OF CERTAIN OPTIONS. CHECK    *     OCD00680
C      *  WITH THE APPROPRIATE MMS REGIONAL OFFICE TO INSURE THAT *     OCD00690
C      *  ACCEPTABLE PARAMETER VALUES ARE USED IN YOUR RUN.       *     OCD00700
C      ************************************************************     OCD00710
C                                                                       OCD00720
C***   TWO SYSTEMS OF LENGTH AND COORDINATES ARE USED IN OCD:           OCD00730
C                                                                       OCD00740
C             THE FIRST SYSTEM, USER UNITS, IS SELECTED BY THE USER AND OCD00750
C        NORMALLY USES THE COORDINATE SYSTEM OF THE EMISSION INVENTORY. OCD00760
C        ALL LOCATIONS INPUT BY THE USER (SUCH AS SOURCES AND RECEPTORS)OCD00770
C        ARE IN THIS SYSTEM. ALSO AS A CONVENIENCE TO THE USER, ALL     OCD00780
C        LOCATIONS ON OUTPUT ARE ALSO IN THIS SYSTEM.                   OCD00790
C                                                                       OCD00800
C             THE SECOND SYSTEM, X, Y, IS AN UPWIND, CROSSWIND          OCD00810
C        COORDINATE SYSTEM RELATIVE TO EACH RECEPTOR.  THE X-AXIS IS    OCD00820
C        DIRECTED UPWIND (SAME AS WIND DIRECTION FOR THE HOUR).  IN     OCD00830
C        ORDER TO DETERMINE DISPERSION PARAMETER VALUES AND EVALUATE    OCD00840
C        EQUATIONS FOR CONCENTRATION ESTIMATES, DISTANCES IN THIS       OCD00850
C        SYSTEM MUST BE IN KILOMETERS. THIS SYSTEM IS INTERNAL AND IS   OCD00860
C        NOT APPARENT TO THE USER.                                      OCD00870
C                                                                       OCD00880
C  DATA INPUT LISTS.                                                    OCD00890
C                                                                       OCD00900
C      THE REQUIRED AND OPTIONAL INPUT GROUPS TO OCD ARE                OCD00910
C       DESCRIBED BELOW:                                                OCD00920
C                                                                       OCD00930
C***   GROUPS 1 - 3  ALPHANUMERIC DATA FOR TITLES. FORMAT(A80)          OCD00940
C                                                                       OCD00950
C        LINE1 - 80 ALPHANUMERIC CHARACTERS.                            OCD00960
C        LINE2 - 80 ALPHANUMERIC CHARACTERS.                            OCD00970
C        LINE3 - 80 ALPHANUMERIC CHARACTERS.                            OCD00980
C                                                                       OCD00990
C***   GROUP 4  CONTROL AND CONSTANTS.  FORMAT(FREE)                    OCD01000
C                                                                       OCD01010
C        IDATE(1) -  2-DIGIT YEAR FOR THIS RUN.                         OCD01020
C        IDATE(2) -  STARTING JULIAN DAY FOR THIS RUN.                  OCD01030
C        IHSTRT   -  STARTING HOUR FOR THIS RUN.                        OCD01040
C        NPER     -  NUMBER OF AVERAGING PERIODS TO BE RUN.             OCD01050
C        NAVG     -  NUMBER OF HOURS IN AN AVERAGING PERIOD.            OCD01060
C        IPOL     -  POLLUTANT INDICATOR; (3 FOR SO2, 4 FOR SUSPENDED   OCD01070
C                     PARTICULATES, 5 FOR NITROGEN OXIDES, 6 FOR CARBON OCD01080
C                     MONOXIDE, 7 FOR BLANK).                           OCD01090
C        NSIGP    -  NUMBER OF SOURCES FROM WHICH CONC. CONTRIBUTIONS   OCD01100
C                     ARE DESIRED (MAX = 25).                           OCD01110
C        NAV5     -  ADDITIONAL AVERAGING TIME FOR HIGH-FIVE TABLE;     OCD01120
C                     MOST LIKELY EQUAL TO 2, 4, 6, OR 12.              OCD01130
C        CONTWO   -  MULTIPLIER TO CONVERT USER UNITS TO KILOMETERS.    OCD01140
C                       EXAMPLE MULTIPLIERS:                            OCD01150
C                         FEET TO KM    3.048E-04                       OCD01160
C                         MILES TO KM   1.609344                        OCD01170
C                         METERS TO KM  1.0E-03                         OCD01180
C        CELM     -  MULTIPLIER TO CONVERT USER HEIGHT UNITS TO METERS. OCD01190
C                       EXAMPLE MULTIPLIER:                             OCD01200
C                         FEET TO METERS  0.3048                        OCD01210
C       SPECIFICATION OF POLLUTANT HALF-LIFE IS TREATED IN A SPECIAL    OCD01220
C       SECTION (GROUP 14) BELOW.                                       OCD01230
C                                                                       OCD01240
C      ************************************************************     OCD01250
C      *            IMPORTANT MESSAGE NO. 2 ]]]                   *     OCD01260
C      *       CALCULATIONS SUBMITTED TO SATISFY REGULATORY       *     OCD01270
C      *  REQUIREMENTS MAY REQUIRE THAT CERTAIN OPTIONS BE USED]  *     OCD01280
C      *  CHECK WITH THE APPROPRIATE MMS REGIONAL OFFICE TO       *     OCD01290
C      *  INSURE THAT ACCEPTABLE OPTIONS ARE USED IN YOUR RUN.    *     OCD01300
C      ************************************************************     OCD01310
C                                                                       OCD01320
C        ******************************************************         OCD01330
C        *      THE USER IS REFERRED TO THE USERS GUIDE FOR   *         OCD01340
C        *  MORE DETAILED INFORMATION ON OPTIONS. ESPECIALLY  *         OCD01350
C        *  IMPORTANT IS AN UNDERSTANDING OF PRINTED OUTPUT   *         OCD01360
C        *  AND USE OF OPTIONS 9 THROUGH 19 TO DELETE UNNEEDED*         OCD01370
C        *  INFORMATION. OCD IS CAPABLE OF GENERATING A       *         OCD01380
C        *  LARGE QUANTITY OF PRINTED INFORMATION UNLESS SOME *         OCD01390
C        *  OF THESE OPTIONS TO DELETE OUTPUT ARE USED        *         OCD01400
C        *  LIBERALLY.                                        *         OCD01410
C        ******************************************************         OCD01420
C                                                                       OCD01430
C                                                                       OCD01440
C***   GROUP 5.  OPTIONS.  FORMAT(FREE)                                 OCD01450
C                                                                       OCD01460
C        1 = EMPLOY OPTION;  0 = DON'T USE OPTION.                      OCD01470
C                                                                       OCD01480
C       TECHNICAL OPTIONS:                                              OCD01490
C        IOPT(1)  -  USE TERRAIN ADJUSTMENTS.                           OCD01500
C        IOPT(2)  -  NO STACK DOWNWASH.                                 OCD01510
C        IOPT(3)  -  NO GRADUAL PLUME RISE.                             OCD01520
C        IOPT(4)  -  USE BUOYANCY INDUCED DISPERSION.                   OCD01530
C                                                                       OCD01540
C       INPUT OPTIONS:                                                  OCD01550
C        IOPT(5)  -  SOURCE OF MET. DATA
C                    IF 0, MET. DATA ARE FROM SEPARATE BINARY PCRAMMET
C                          FILE;
C                    IF 1, ASCII MET. DATA ARE INCLUDED IN CONTROL FILE;
C                    IF 2, MET. DATA ARE FROM SEPARATE ASCII PCRAMMET
C                          FILE
C        IOPT(6)  -  READ HOURLY EMISSIONS.                             OCD01570
C        IOPT(7)  -  SPECIFY SIGNIFICANT SOURCES.                       OCD01580
C        IOPT(8)  -  Receptor types
C                    If 0, discrete receptors only
C                    If 1, discrete receptors and polar receptor network
C                    If 2, discrete receptors and Cartesian receptor
C                          network
C                    If 3, discrete receptors and polar and Cartesian
C                          receptor networks
C                    If 4, polar receptor network
C                    If 5, Cartesian receptor network
C                    If 6, polar and Cartesian receptor networks
C                                                                       OCD01610
C       PRINTED OUTPUT OPTIONS:                                         OCD01620
C        IOPT(9)  -  DELETE EMISSIONS WITH HEIGHT TABLE.                OCD01630
C        IOPT(10) -  DELETE RESULTANT MET. DATA SUMMARY FOR AVG. PERIOD.OCD01640
C        IOPT(11) -  DELETE HOURLY CONTRIBUTIONS.                       OCD01650
C        IOPT(12) -  DELETE MET. DATA ON HOURLY CONTRIBUTIONS.          OCD01660
C        IOPT(13) -  DELETE CASE STUDY PRINTOUT OF PLUME TRANSPORT AND  OCD01670
C                     DISPERSION ON HOURLY CONTRIBUTIONS.               OCD01680
C        IOPT(14) -  DELETE HOURLY SUMMARY.                             OCD01690
C        IOPT(15) -  DELETE MET. DATA ON HOURLY SUMMARY.                OCD01700
C        IOPT(16) -  DELETE CASE STUDY PRINTOUT OF PLUME TRANSPORT AND  OCD01710
C                     DISPERSION ON HOURLY SUMMARY.                     OCD01720
C        IOPT(17) -  DELETE AVERAGING-PERIOD CONTRIBUTIONS.             OCD01730
C        IOPT(18) -  DELETE AVERAGING-PERIOD SUMMARY.                   OCD01740
C        IOPT(19) -  DELETE AVERAGE CONCENTRATIONS AND HIGH-FIVE TABLE. OCD01750
C                                                                       OCD01760
C       OTHER CONTROL AND OUTPUT OPTIONS:                               OCD01770
C        IOPT(20) -  SOURCE TYPE                                        OCD01780
C                    0 = POINT SOURCE (DEFAULT)                         OCD01790
C                    1 = AREA SOURCE                                    OCD01800
C                    2 = LINE SOURCE                                    OCD01810
C        IOPT(21) -  CREATE SUMMARY OUTPUT FILE CALLED EXTRA.OUT.       OCD01820
C        IOPT(22) -  WRITE HOURLY CONCENTRATIONS TO DISK.               OCD01830
C        IOPT(23) -  CREATE A TABLE OF ANNUAL IMPACT ASSESSMENT FOR     OCD01840
C                     NON-PERMANENT ACTIVITIES.                         OCD01850
C        IOPT(24) -  LAND SOURCE (DO NOT MODIFY WINDSPEED).             OCD01860
C        IOPT(25) -  CONSIDER POLLUTANT REMOVAL VIA CHEMICAL            OCD01870
C                     TRANSFORMATION  (IF 1, NEED CARD TYPE 15).        OCD01880
C        IOPT(26) -  PERFORM NORMAL RUN (IF 1) OR TEST RUN (IF 0)
C                                                                       OCD01890
C      ************************************************************     OCD01900
C      *            IMPORTANT MESSAGE NO. 3 ]]]                   *     OCD01910
C      *       CALCULATIONS SUBMITTED TO SATISFY REGULATORY       *     OCD01920
C      *  REQUIREMENTS MAY REQUIRE THAT CERTAIN PARAMETER VALUES  *     OCD01930
C      *  BE USED FOR THE VARIABLES ON THIS CARD.  CHECK WITH THE *     OCD01940
C      *  APPROPRIATE MMS REGIONAL OFFICE TO INSURE THAT          *     OCD01950
C      *  ACCEPTABLE PARAMETER VALUES ARE USED IN YOUR RUN]       *     OCD01960
C      ************************************************************     OCD01970
C                                                                       OCD01980
C***   GROUP 6.  WIND AND TERRAIN. FORMAT(FREE)                         OCD01990
C                                                                       OCD02000
C       ANEMOMETER HEIGHT FOR LAND-BASED INSTRUMENT IS ASSIGNED FOR THE OCD02010
C       HEIGHT OF THE OVER-WATER INSTRUMENT IF THE LATTER IS MISSING.   OCD02020
C                                                                       OCD02030
C       HANE  - LAND ANEMOMETER HEIGHT (METERS)                         OCD02040
C                                                                       OCD02050
C        Z0L  -  SURFACE ROUGHNESS LENGTH OVER LAND NEAR SHORE (METERS) OCD02060
C                                                                       OCD02070
C       OCD ALLOWS TERRAIN ABOVE STACK TOP ELEVATION.  THIS MODEL       OCD02080
C       USES OPTION 1 FROM THE EPA COMPLEX MODELS (THEIR IOPT(25)=1)    OCD02090
C       TO SIMULATE PLUME BEHAVIOR.  A MINIMUM MISS DISTANCE, ZMIN,     OCD02100
C       IS SPECIFIED IN CARD TYPE 6.                                    OCD02110
C                                                                       OCD02120
C       ZMIN - MINIMUM HEIGHT OF THE PLUME ABOVE TERRAIN (METERS)       OCD02130
C                                                                       OCD02140
C       SLAT - APPROXIMATE LATITUDE OF SOURCE REGION                    OCD02150
C              FOR COMPUTATION OF CORIOLIS PARAMETER                    OCD02160
C                                                                       OCD02170
C***   GROUP 7.  SOURCE.                                                OCD02180
C       LINE 1    FORMAT(A12)     MANDATORY                             OCD02190
C       LINE 2    FREE FORMAT     MANDATORY                             OCD02200
C       LINE 3    FREE FORMAT     OPTIONAL                              OCD02210
C                                                                       OCD02220
C       ONLY 1 POLLUTANT EMISSION RATE IS SPECIFIED                     OCD02230
C                                                                       OCD02240
C       (UP TO MAXP POINT SOURCE CARDS, OR FIVE AREA SOURCES            OCD02250
C        OR ONE LINE SOURCE SEGMENT ARE ALLOWED)                        OCD02260
c       (MAXP is defined in PARAMS.CMN.)
C                                                                       OCD02270
C   LINE 1 - FORMAT(A12)                                                OCD02280
C        PNAME(NPT)     -  12 CHARACTER SOURCE IDENTIFICATION.          OCD02290
C   LINE 2 - FREE FORMAT                                                OCD02300
C        SOURCE(1,NPT)  -  EAST COORDINATE OF POINT SOURCE (USER UNITS) OCD02310
C                          EAST COORDINATE OF CIRCLE CENTER FOR AREA    OCD02320
C                          SOURCE (USER UNITS)                          OCD02330
C                          EAST COORDINATE OF STARTING POINT FOR LINE   OCD02340
C                          SOURCE (USER UNITS)                          OCD02350
C        SOURCE(2,NPT)  -  NORTH COORDINATE OF POINT SOURCE (USER UNITS)OCD02360
C                          NORTH COORDINATE OF STARTING POINT FOR LINE  OCD02370
C                          SOURCE (USER UNITS)                          OCD02380
C                          NORTH COORDINATE OF CIRCLE CENTER FOR AREA   OCD02390
C                          SOURCE (USER UNITS)                          OCD02400
C        SOURCE(3,NPT)  -  POLLUTANT EMISSION RATE (G/SEC).             OCD02410
C        SOURCE(4,NPT)  -  HEIGHT OF BUILDING OR OTHER STRUCTURE (IN    OCD02420
C                          METERS) TO WHICH STACK IS ATTACHED; HEIGHT ISOCD02430
C                          RELATIVE TO SOURCE "GROUND-LEVEL" :ELP(NPT): OCD02440
C        SOURCE(5,NPT)  -  PHYSICAL STACK HEIGHT (METERS).              OCD02450
C        SOURCE(6,NPT)  -  STACK GAS TEMPERATURE (KELVIN).              OCD02460
C        SOURCE(7,NPT)  -  STACK INSIDE DIAMETER (METERS) FOR POINT     OCD02470
C                          OR LINE SOURCES                              OCD02480
C                          CIRCLE DIAMETER (METERS) FOR AREA SOURCE     OCD02490
C        SOURCE(8,NPT)  -  STACK GAS EXIT VELOCITY (M/SEC).             OCD02500
C        SOURCE(9,NPT)  -  DEVIATION OF STACK ELEVATION ANGLE FROM      OCD02510
C                          VERTICAL, DEGREES (0 = VERTICAL, 90 =        OCD02520
C                          HORIZONTAL)                                  OCD02530
C        ELP(NPT)       -  SOURCE GROUND-LEVEL ELEVATION (USER HT UNITS)OCD02540
C        SOURCE(11,NPT) -  BUILDING WIDTH (METERS)                      OCD02550
C                                                                       OCD02560
C        FOR OCD APPLICATIONS, THE "GROUND-LEVEL" ELEVATION WILL        OCD02570
C        BE REFERENCED FROM WATER SURFACE LEVEL.  A SOURCE GROUND-LEVEL OCD02580
C        ELEVATION EQUAL TO THE HEIGHT OF THE BASE OF A PLATFORM SHOULD OCD02590
C        BE USED FOR OFFSHORE ELEVATED PLATFORMS.  FOR SHIPS OR OTHER   OCD02600
C        STRUCTURES IN CONTACT WITH THE WATER SURFACE, THE SPECIFIED    OCD02610
C        ELEVATION SHOULD BE ZERO.                                      OCD02620
C                                                                       OCD02630
C   LINE 3    ***LINE SOURCES ONLY***   FORMAT(FREE)                    OCD02640
C                                                                       OCD02650
C        XSTOP - EAST COORDINATE OF ENDING POINT FOR LINE SOURCE        OCD02660
C                (USER UNITS)                                           OCD02670
C        YSTOP - NORTH COORDINATE OF ENDING POINT FOR LINE SOURCE       OCD02680
C                (USER UNITS)                                           OCD02690
C                                                                       OCD02700
C         CARD WITH 'ENDP' IN COLS 1-4 IS USED TO SIGNIFY THE           OCD02710
C          END OF THE SOURCES.                                          OCD02720
C                                                                       OCD02730
C***   GROUP  8.  SPECIFIED SIGNIFICANT SOURCES.  FORMAT(FREE)          OCD02740
C                                                                       OCD02750
C       (USED IF OPTION 7 = 1)                                          OCD02760
C        NPT           -  NUMBER OF USER SPECIFIED SIGNIFICANT SOURCES  OCD02770
C        MPS(I),I=1,NPT -  POINT SOURCE NUMBERS USER WANTS CONSIDERED   OCD02780
C                           SIGNIFICANT.                                OCD02790
C                                                                       OCD02800
C***   GROUP  9. MET. DATA IDENTIFIERS.  FORMAT(FREE)                   OCD02810
C                                                                       OCD02820
C       (USED IF OPTION 5 = 0)                                          OCD02830
C        ISFCD    -  SFC MET STATION IDENTIFIER    (5 DIGITS)           OCD02840
C        ISFCYR   -  YEAR OF SFC MET DATA          (2 DIGITS)           OCD02850
C        IMXD     -  UPPER-AIR STATION IDENTIFIER  (5 DIGITS)           OCD02860
C        IMXYR    -  YEAR OF MIXING HEIGHT DATA    (2 DIGITS)           OCD02870
C                                                                       OCD02880
C***   GROUP 10.  POLAR COORDINATE RECEPTORS.  FORMAT(FREE)             OCD02890
C                                                                       OCD02900
C       (USED IF OPTION 8 = 1)                                          OCD02910
C                                                                       OCD02920
c       First line (free format):
c        NRING    -  Number of rings
c
c       Second line (free format):
C        RADIL(I),I= 1,NRING  -  NRING RADIAL DISTANCES (USER UNITS),
C                    EACH OF WHICH GENERATES 36 RECEPTORS AROUND POINT
c                    CENTX, CENTY ON AZIMUTHS 10 TO 360 DEGREES.
C        CENTX    -  EAST COORDINATE ABOUT WHICH RADIALS ARE CENTERED.  OCD02970
C                    (USER UNITS)
C        CENTY    -  NORTH COORDINATE ABOUT WHICH RADIALS ARE CENTERED. OCD02990
C                    (USER UNITS)
C                                                                       OCD03010
C***   GROUP 11.  POLAR COORDINATE RECEPTOR ELEVATIONS.  FORMAT(FREE)   OCD03020
C                                                                       OCD03030
C                 (USED IF OPTIONS 1 AND 8 ARE BOTH 1)                  OCD03040
C        IDUM     -  AZIMUTH INDICATOR (1 TO 36)                        OCD03050
C        ELRDUM(I),I=1,NRING  -  RECEPTOR ELEVATIONS FOR THIS AZIMUTH
C                 FOR NRING DISTANCES (USER HEIGHT UNITS).
C                                                                       OCD03080
C***   GROUP 12.  RECEPTOR.  FORMAT(A8,5F10.3)                          OCD03090
C                                                                       OCD03100
C       (UP TO MAXREC RECEPTORS MAY BE GENERATED INCLUDING POLAR        OCD03110
C        COORDINATE RECEPTORS IF OPTION 8 = 1.)                         OCD03120
c       (MAXREC is defined in PARAMS.CMN.)
C        RNAME    -  8 DIGIT ALPHANUMERIC STATION IDENTIFICATION.       OCD03130
C        RREC     -  EAST COORDINATE OF RECEPTOR (USER UNITS)           OCD03140
C        SREC     -  NORTH COORDINATE OF RECEPTOR (USER UNITS)          OCD03150
C        ZR       -  RECEPTOR HEIGHT ABOVE LOCAL GROUND-LEVEL (METERS)  OCD03160
C        ELR      -  RECEPTOR GROUND-LEVEL ELEVATION (USER HT UNITS)    OCD03170
C        HTER     -  MOUNTAIN/HILL ELEVATION IN THE VICINITY OF         OCD03180
C                    RECEPTOR (METERS)                                  OCD03190
C         CARD WITH 'ENDR' IN COLS 1-4 IS USED TO SIGNIFY THE END OF    OCD03200
C          THE RECEPTOR CARDS.                                          OCD03210
C                                                                       OCD03220
C       GROUP 13.  SPECIFICATION OF ADDITIONAL METEOROLOGICAL DATA      OCD03230
C                                                                       OCD03240
C         ONE CARD: FREE FORMAT; CONTAINS SWITCHES FOR USE OF           OCD03250
C         ADDITIONAL METEOROLOGICAL PARAMETERS (UNLESS OTHERWISE        OCD03260
C         SPECIFIED, 1=PROVIDED, 0=NOT PROVIDED, OR DO NOT USE).        OCD03270
C                                                                       OCD03280
C       JOPT(1):  OVERWATER WIND DIRECTION PROVIDED                     OCD03290
C       JOPT(2):  OVERWATER WIND SPEED PROVIDED                         OCD03300
C       JOPT(3):  OVERWATER VERTICAL POTENTIAL TEMPERATURE GRADIENT     OCD03310
C          DATA PROVIDED, WILL BE USED IN DISPERSION CALCULATIONS       OCD03320
C       JOPT(4):  OVERWATER HUMIDITY, SPECIFIED AS FOLLOWS:             OCD03330
C          1 = RELATIVE HUMIDITY ( ) IS PROVIDED,                       OCD03340
C          2 = WET BULB TEMPERATURE (DEG K) IS PROVIDED,                OCD03350
C          3 = DEW POINT TEMPERATURE (DEG K) IS PROVIDED.               OCD03360
C       JOPT(5):  OVERLAND TURBULENCE DATA PROVIDED, WILL BE USED IN    OCD03370
C          DISPERSION CALCULATIONS.                                     OCD03380
C       JOPT(6):  WATER SURFACE TEMPERATURE, SPECIFIED AS FOLLOWS:      OCD03390
C          1 = WATER SURFACE TEMPERATURE (DEG K) IS PROVIDED,           OCD03400
C          2 = AIR MINUS WATER TEMPERATURE (DEG K) IS PROVIDED.         OCD03410
C       JOPT(7):  OVERWATER WIND DIRECTION SHEAR IS PROVIDED            OCD03420
C       JOPT(8):  OVERWATER HORIZONTAL TURBULENCE DATA PROVIDED, WILL BEOCD03430
C          USED IN DISPERSION CALCULATIONS.                             OCD03440
C       JOPT(9):  OVERWATER VERTICAL TURBULENCE DATA PROVIDED, WILL BE  OCD03450
C          USED IN DISPERSION CALCULATIONS.                             OCD03460
C                                                                       OCD03470
C  HWANE, ELEVATION ABOVE SEA LEVEL (METERS) OF OVERWATER ANEMOMETER    OCD03480
C  HWT,   ELEVATION ABOVE SEA LEVEL (METERS) OF OVERWATER AIR TEMP SENSOOCD03490
C                                                                       OCD03500
C       GROUP 14.   CHEMICAL TRANSFORMATION RATES                       OCD03510
C          (OMIT IF IOPT(25) = 0)                                       OCD03520
C                                                                       OCD03530
C       FIRST LINE (FREE FORMAT): LATITUDE, LONGITUDE, AND TIME ZONE    OCD03540
C          (LATITUDE AND LONGITUDE ARE IN DEGREES, AND ARE POSITIVE     OCD03550
C          FOR THE WESTERN AND NORTHERN HEMISPHERES.  TIME ZONE IS THE  OCD03560
C          NUMBER OF HOURS BEHIND GMT, POSITIVE IN THE U.S.)            OCD03570
C                                                                       OCD03580
C       SECOND LINE (FREE FORMAT): DAYTIME CHEMICAL TRANSFORMATION      OCD03590
C          (DECAY) RATES OF THE MODELED POLLUTANT,   PER HOUR, BY       OCD03600
C          MONTH  (12 VALUES FOR JAN-DEC, RESPECTIVELY).  DECAY RATES   OCD03610
C          ARE ASSUMED TO BE ZERO AT NIGHT.                             OCD03620
C                                                                       OCD03630
C       GROUP 15.  SHORELINE GEOMETRY (MANDATORY)                       OCD03640
C                                                                       OCD03650
C       COORDINATES OF THE UPPER LEFT (NORTHWEST) CORNER OF AN AREA TO  OCD03660
C       MAPPED ARE SPECIFIED.  THE MAPPED AREA NEED NOT INCLUDE SOURCE  OCD03670
C       AND RECEPTOR LOCATIONS.  THE RECTANGULAR AREA                   OCD03680
C       IS DIVIDED INTO GRID RECTANGLES; THE NUMBER OF RECTANGLES IN THEOCD03690
C       X AND Y DIRECTIONS CANNOT EXCEED 60.  FOR EACH RECTANGLE, THE   OCD03700
C       USER SPECIFIES DOMINANCE BY EITHER LAND OR WATER.               OCD03710
C                                                                       OCD03720
C       FIRST CARD (FREE FORMAT): X0,Y0,NX,NY,DELX,DELY,WMIN            OCD03730
C                                                                       OCD03740
C         X0 = X COORDINATE OF THE NORTHWEST CORNER OF THE MAPPED AREA  OCD03750
C                                       (USER HORIZ. UNITS)             OCD03760
C         Y0 = Y COORDINATE OF THE NORTHWEST CORNER OF THE MAPPED AREA  OCD03770
C                                       (USER HORIZ. UNITS)             OCD03780
C         NX = NUMBER OF GRID RECTANGLES (MAP COLUMNS) ALONG THE X AXIS OCD03790
C         NY = NUMBER OF GRID RECTANGLES (MAP ROWS) ALONG THE Y AXIS    OCD03800
C         DELX = X LENGTH OF EACH GRID RECTANGLE IN USER HORIZ. UNITS   OCD03810
C         DELY = Y LENGTH OF EACH GRID RECTANGLE IN USER HORIZ. UNITS   OCD03820
C         WMIN = MINIMUM ALONG WIND WIDTH OF LAND OR WATER BODIES TO    OCD03830
C                BE CONSIDERED IN USER HORIZ. UNITS                     OCD03840
C         AVGDIST = AVERAGE DISTANCE BETWEEN SOURCE AND SHORELINE IN    OCD03850
C                   USER HORIZ. UNITS                                   OCD03860
C                                                                       OCD03870
C       NEXT NY CARDS:                                                  OCD03880
C                                                                       OCD03890
C       FOR EACH ROW OF GRID RECTANGLES,STARTING AT THE TOP (NORTH EDGE)OCD03900
C       OF THE MAPPED AREA, SPECIFY LAND (L) OR WATER (W) FOR EACH REC- OCD03910
C       TANGLE.  THE STATUS (L OR W) OF THE FIRST (WESTERNMOST)         OCD03920
C       RECTANGLE MUST BE INDICATED.  STATUS OF EACH RECTANGLE  MAY BE  OCD03930
C       INDICATED, OR PERSISTENCE MAY BE USED IN AN EASTWARD DIRECTION  OCD03940
C       FOR EACH ROW (LINE OF INPUT).                                   OCD03950
C                                                                       OCD03960
C       LAST CARD WITH 'ENDS' IN COLUMNS 1-4 IS USED TO SIGNIFY THE     OCD03970
C       END OF THE SHORELINE DEFINITION.                                OCD03980
C                                                                       OCD03990
C***   GROUP 16.  METEOROLOGY.  FORMAT(FREE)                            OCD04000
C                                                                       OCD04010
C       (ONE CARD FOR EACH HOUR OF THE SIMULATION.)                     OCD04020
C       (USED IF OPTION 5 = 1)                                          OCD04030
C        JYR      -  YEAR OF MET DATA. (2 DIGITS)                       OCD04040
C        DAY1     -  JULIAN DAY OF MET DATA.                            OCD04050
C        JHR      -  HOUR OF MET DATA.                                  OCD04060
C        IKST     -  STABILITY CLASS FOR THIS HOUR.                     OCD04070
C        QU       -  WIND SPEED FOR THIS HOUR (M/SEC).                  OCD04080
C        QTEMP    -  AMBIENT AIR TEMPERATURE FOR THIS HOUR (KELVIN).    OCD04090
C        QTHETA   -  WIND DIRECTION FOR THIS HOUR (DEGREES AZIMUTH FROM OCD04100
C                     WHICH THE WIND BLOWS).                            OCD04110
C        QHL      -  MIXING HEIGHT FOR THIS HOUR (METERS).              OCD04120
C                                                                       OCD04130
C  COMMON, DIMENSION, AND DATA STATEMENTS.                              OCD04140
                                                                        OCD04150
      include 'params.cmn'
      INCLUDE 'exps.cmn'                                                OCD04160
      INCLUDE 'funcs.cmn'                                               OCD04170
      INCLUDE 'opt.cmn'                                                 OCD04180
      INCLUDE 'met.cmn'                                                 OCD04200
      INCLUDE 'src.cmn'                                                 OCD04210
      INCLUDE 'store.cmn'                                               OCD04220
      INCLUDE 'shr.cmn'                                                 OCD04240
      INCLUDE 'count.cmn'                                               OCD04260
      INCLUDE 'const.cmn'                                               OCD04270
      INCLUDE 'linesrc.cmn'                                             OCD04280
C                                                                       OCD04290
      DIMENSION IFREQ(7)                                                OCD04310
      REAL LLAND(24)                                                    OCD04320
      DATA LLAND/-4.,-6.,-8.,-10.,-8.,-10.,-16.,-20.,                   OCD04330
     &  -15.,-25.,-50.,-100.,4*9999.,                                   OCD04340
     &  15.,25.,50.,100.,5.,10.,15.,20./                                OCD04350
      DATA IFREQ /7*0/                                                  OCD04360
                                                                        OCD04430
C  RUN SET-UP AND READ FIRST 6 INPUT CARDS.                             OCD04440
C                                                                       OCD04450
C    INITIALIZE VARIABLES                                               OCD04460
      CALL INIT                                                         OCD04470
C
C    SET UP OCD FILES AND READ DATA (GROUPS 1 THROUGH 15) FROM THE
C    CONTROL FILE
C
      call readcf2(ihstrt,nper,nav5,day1a,hr1,navt,elow,alat,along,
     &             tzone)
C
C    SAVE STARTING HOUR BEFORE BEING REDEFINED                          OCD04500
      NBB = IHSTRT                                                      OCD04510
C                                                                       OCD04940
C  POSITION FILES AS REQUIRED.                                          OCD04950
C                                                                       OCD04960
C       IF INPUT LAND METEOROLOGY IS READ FROM A BINARY DISK FILE,      OCD04970
C       AND THE RUN DOES NOT BEGIN ON THE FIRST DAY, THEN RECORDS ARE   OCD04980
C       SKIPPED IN THE FOLLOWING DATA INPUT FILES:  LAND METEOROLOGY,   OCD04990
C       OVERWATER METEOROLOGY, AND HOURLY EMISSIONS.  IF INPUT LAND     OCD05000
C       METEOROLOGY IS READ FROM CARDS, THEN ALL OF THE ABOVE FILES     OCD05010
C       ARE ASSUMED TO START AT THE SAME TIME, AND NO RECORDS ARE       OCD05020
C       SKIPPED.                                                        OCD05030
C                                                                       OCD05040
      IDAY=IDATE(2)-1                                                   OCD05050
      IF (IDAY.GT.0 .AND. (IOPT(5).EQ.0 .or. IOPT(5).eq.2)) CALL FILPOS
C                                                                       OCD05070
C  START LOOPS FOR DAY AND AVG TIME; READ MET DATA.                     OCD05080
C                                                                       OCD05090
      IEND = 0                                                          OCD05100
720   IDAY=IDAY+1                                                       OCD05110
      WRITE(*,*) 'PROCESSING DAY: ',IDAY                                OCD05120
      CALL DAYLOOP(IEND,ALAT,ALONG,TZONE,IHRRIS,IHRSET,                 OCD05130
     &             NE,IHSTRT,DAY1,NHRS,JYR)                             OCD05140
      IF(IEND.EQ.1) GOTO 1050                                           OCD05150
770   CALL AVGLOOP(IEND,DELN,DELM,IFREQ,NE,IHSTRT,DAY1,DAY2,JYR)        OCD05160
      IF(IEND.EQ.1) GOTO 1050                                           OCD05170
      IF (IOPT(10).EQ.0) CALL METRES(DELN,DELM,IFREQ,URES)              OCD05180
                                                                        OCD05190
C        REDEFINE NB AND NE IN CASE NON-CONSECUTIVE DAYS ARE BEING RUN  OCD05200
      IF (IOPT(5).EQ.1) THEN                                            OCD05210
         NB=IHSTRT                                                      OCD05220
         NE=IHSTRT+NAVG-1                                               OCD05230
      ENDIF                                                             OCD05240
C                                                                       OCD05250
C  INITIALIZE FOR HOURLY LOOP.                                          OCD05260
C                                                                       OCD05270
C  INITIALIZE SUMS FOR CONC AND PARTIAL CONC FOR AVG PERIOD.            OCD05280
      DO 890 K=1,NRECEP                                                 OCD05290
         PCHI(K)=0.0                                                    OCD05300
         DO 880 I=1,26                                                  OCD05310
            PSIGS(K,I)=0.0                                              OCD05320
880      CONTINUE                                                       OCD05330
890   CONTINUE                                                          OCD05340
C                                                                       OCD05350
C  BEGIN HOURLY LOOP.                                                   OCD05360
C                                                                       OCD05370
      IF(IOPT(20).EQ.2) NE=NSEGS                                        OCD05380
      DO 1020 ILH=NB,NE                                                 OCD05390
         CALL HRCON(DAY1,LLAND,IHRRIS,IHRSET,NAVT,NAV5,NPER)            OCD05400
1020  CONTINUE                                                          OCD05410
                                                                        OCD05420
      IF (NE.GT.24) IDATE(2)=DAY1                                       OCD05430
C        OUTPUT FINAL RESULTS                                           OCD05440
      CALL OUTAVG                                                       OCD05450
      NP=NP+1                                                           OCD05460
      NHRS=NHRS+NAVG                                                    OCD05470
C        NEXT STATEMENT IS BRANCH FOR END OF RUN.                       OCD05480
      IF (NP.GE.NPER) GO TO 1050                                        OCD05490
      IF (NHRS.LT.24) GO TO 1030                                        OCD05500
      IF(IOPT(5).EQ.1) IHSTRT = MOD(IHSTRT+NAVG,24)                     OCD05510
      GO TO 720                                                         OCD05520
C                                                                       OCD05530
C          END OF LOOP FOR CALENDAR DAYS                                OCD05540
C                                                                       OCD05550
1030  NB=NB+NAVG                                                        OCD05560
      NE=NE+NAVG                                                        OCD05570
      IF (NB.LE.24) GO TO 770                                           OCD05580
      NB=MOD(NB,24)                                                     OCD05590
      NE=NB+NAVG-1                                                      OCD05600
      GO TO 770                                                         OCD05610
C                                                                       OCD05620
C        END OF LOOP FOR AVERAGING PERIOD.                              OCD05630
C  WRITE AVERAGE CONC. AND HIGH-FIVE TABLES.                            OCD05640
C                                                                       OCD05650
C        IF OPTION 19 = 0, WRITE AVERAGE CONCENTRATION.                 OCD05660
C         FOR LENGTH OF RECORD AND HIGH-FIVE TABLE.                     OCD05670
                                                                        OCD05680
1050  IF (IOPT(19).EQ.0) CALL AVCON(DAY1A,HR1,DAY2,NAVT)                OCD05690
C                                                                       OCD05700
C  SECTION S - CLOSE OUT FILES.                                         OCD05710
C                                                                       OCD05720
      write (io,11)
      print 11
11    format(//,' *** Run Successfully Completed *** ')
      IF (IOPT(22).EQ.1) CLOSE(UNIT=12)                                 OCD05730
      CLOSE(UNIT=IN)                                                    OCD05740
      CLOSE(UNIT=IO)                                                    OCD05750
C                                                                       OCD05760
C  INPUT AND OUTPUT FILE DESCRIPTIONS.                                  OCD05770
C                                                                       OCD05780
C***     INPUT AND OUTPUT FILE DESCRIPTIONS.                            OCD05790
C                                                                       OCD05800
C***  INPUT FILE (UNIT 11) METEOROLOGICAL DATA (USED IF IOPT(5)=0,
c     BINARY PCRAMMET FILE; OR IOPT(5)=2, ASCII PCRAMMET FILE)
C                                                                       OCD05820
C      RECORD 1                                                         OCD05830
C                                                                       OCD05840
C        ID          SFC STATION IDENTIFIER, 5 DIGITS                   OCD05850
C        IYEAR       YEAR OF SURFACE DATA, 2 DIGITS                     OCD05860
C        IDM         MIX HT STATION IDENTIFIER, 5 DIGITS                OCD05870
C        IYR         YEAR OF MIX HT DATA, 2 DIGITS                      OCD05880
C                                                                       OCD05890
C      RECORD TYPE 2 (ONE FOR EACH DAY OF YEAR)                         OCD05900
C                                                                       OCD05910
C        JYR         YEAR                                               OCD05920
C        IMO         MONTH                                              OCD05930
C        DAY1        JULIAN DAY                                         OCD05940
C        IKST(24)    STABILITY CLASS                                    OCD05950
C        QU(24)      WIND SPEED, METERS PER SECOND                      OCD05960
C        QTEMP(24)   AMBIENT AIR TEMPERATURE, KELVIN                    OCD05970
C        DUMR(24)    FLOW VECTOR TO 10 DEG, DEGREES AZIMUTH             OCD05980
C        QTHETA(24)  RANDOMIZED FLOW VECTOR, DEGREES AZIMUTH            OCD05990
C        HLH(2,24)   MIXING HEIGHT, METERS                              OCD06000
C                                                                       OCD06010
C***  INPUT FILE(UNIT 15) EMISSION DATA (USED IF IOPT(6)=1)             OCD06020
C                                                                       OCD06030
C      RECORD TYPE 1 (ONE FOR EACH SOURCE, NPT FOR EACH HOUR)           OCD06040
C                                                                       OCD06050
C       FORMAT(FREE)                                                    OCD06060
C                                                                       OCD06070
C        LYR,LDAY,   DATE-TIME VARIABLES CONSISTING OF YEAR, JULIAN DAY,OCD06080
C        LHR          AND HOUR: YY,DDD,HH.                              OCD06090
C        SOURCE(3,K),K=1,NPT     EMISSION RATE FOR THE POLLUTANT IPOL   OCD06100
C                                 FOR THE SOURCE, GRAMS PER SECOND.     OCD06110
C        SOURCE(8,K),K=1,NPT     STACK GAS EXIT VELOCITY, M/SEC         OCD06120
C        SOURCE(6,K),K=1,NPT     STACK GAS TEMPERATURE, DEG K           OCD06130
C                                                                       OCD06140
C       INPUT FILE (UNIT 13) OF ADDITIONAL MET  FORMAT(FREE)            OCD06150
C                                                                       OCD06160
C       ONE RECORD PER HOUR                                             OCD06170
C                                                                       OCD06180
C                  VARIABLE            CONTENTS                         OCD06190
C                                                                       OCD06200
C                   KYR                2-DIGIT YEAR                     OCD06210
C                   KDAY               JULIAN DAY                       OCD06220
C                   KHR                TIME AT END OF HOUR              OCD06230
C                   WD                 OVERWATER WIND DIRECTION         OCD06240
C                   WS                 OVERWATER WIND SPEED (M/SEC)     OCD06250
C                   HLW                OVERWATER MIXING HEIGHT (M)      OCD06260
C                   WHUM               OVERWATER HUMIDITY (EXPRESSED AS OCD06270
C                                      RELATIVE HUMIDITY, WET BULB TEMP.OCD06280
C                                      OR DEW POINT TEMPERATURE, DEG K) OCD06290
C                   WTA                OVERWATER AIR TEMP (DEG K)       OCD06300
C                   WTS                SEA SURFACE TEMP, OR AIR MINUS   OCD06310
C                                      SEA TEMP (DEG K)                 OCD06320
C                   WDSHR              WIND DIRECTION SHEAR IN THE      OCD06330
C                                      VERTICAL (DEG PER METER)         OCD06340
C                   IYW                HORIZ. TURB. INTENSITY OVER WATEROCD06350
C                   IZW                VERT. TURB. INTENSITY OVER WATER OCD06360
C                   IYL                HORIZ. TURB. INTENSITY OVER LAND OCD06370
C                   IZL                VERT. TURB. INTENSITY OVER LAND  OCD06380
C                   WDTHDZ             OVERWATER VERTICAL POTENTIAL     OCD06390
C                                      TEMPERATURE GRADIENT (DEG K/M)   OCD06400
C                                                                       OCD06410
C***  OUTPUT FILE (UNIT 12) HOURLY CONCENTRATIONS (USED IF IOPT(22)=1)  OCD06420
C                                                                       OCD06430
C     RECORD 1    (ONE FOR EACH RECEPTOR, NRECEP PER HOUR)              OCD06440
C                                                                       OCD06450
C        MIXING HEIGHT (M), WIND DIRECTION, LAND STABILITY CLASS, AND   OCD06460
C        OVERWATER WIND SPEED (M/SEC)  FOR THE SIMULATED HOUR           OCD06470
C                                                                       OCD06480
C        PHCHI(I),I=1,NRECEP  HOURLY CONCENTRATION FOR EACH RECEPTOR,   OCD06490
C                              G/M**3.                                  OCD06500
C                                                                       OCD06510
                                                                        OCD06520
                                                                        OCD06540
      STOP                                                              OCD06550
      END                                                               OCD06560
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCACS00010
      real function acos_x (arg,callname)                               ACS00020
C                                                                       ACS00030
C Purpose:  Calculate the arc-cosine of ARG.  In theory, the magnitude  ACS00040
C           of ARG can never be greater than one.  However, because of  ACS00050
C           round-off error, the magnitude of ARG can sometimes be      ACS00060
C           slightly larger than one.  This routine prevents the above  ACS00070
C           from happening.                                             ACS00080
C                                                                       ACS00090
C IO:       ARG, argument of ACOS (real)                                ACS00100
C           CALLNAME, name of the calling routine (character)           ACS00110
C                                                                       ACS00120
C Called by:  CALC, VIRT                                                ACS00130
C Calls: ACOS                                                           ACS00140
C       MINERALS MANAGEMENT SERVICE                                     ACS00150
C       U.S. DEPARTMENT OF THE INTERIOR                                 ACS00160
C                                                                       ACS00170
C OCD             REVISION HISTORY:                                     ACS00180
C    JCC 930310   CREATED.                                              ACS00190
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCACS00200
      include 'params.cmn'
      include 'store.cmn'
      real arg                                                          ACS00210
      character*(*) callname                                            ACS00220
      if (abs(arg).le.1.) then                                          ACS00230
        acos_x = acos(arg)                                              ACS00240
      else                                                              ACS00250
        if (abs(arg).le.1.0001) then                                    ACS00260
C  The reason that 1.0001  is used here is to prevent round-off error.  ACS00270
C  Theoretically, ARG cannot be greater than one.  However, sometimes   ACS00280
C  due to round-off error, ARG can be slightly greater than one.        ACS00290
           acos_x = acos(sign(1.,arg))                                  ACS00300
        else                                                            ACS00310
           write (io,11) callname
           write (ierr,11) callname,arg
11         format(' Error in ACOS_X called by ',a8,':',/,
     &' An unexpected error has occurred.  The argument for ACOS =',/,
     &f9.6,' is out of bound.  Contact model developer for assistance.')
            stop 'Error encountered.  See ERROR.OUT for more details.'
          end if                                                        ACS00390
        end if                                                          ACS00400
      return                                                            ACS00410
      end                                                               ACS00420
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCAADD00010
      SUBROUTINE ADDMET(JYR,JULDAY,JHR)                                 ADD00020
C                                                                       ADD00030
C PURPOSE:  INPUT AND CALCULATE ADDITIONAL METEOROLOGICAL DATA          ADD00040
C           CHECK IF DATA IS VALID                                      ADD00050
C                                                                       ADD00060
C I/O:  JYR, YEAR                                                       ADD00070
C      JULDAY, DAY                                                      ADD00080
C       JHR, HOUR                                                       ADD00090
C                                                                       ADD00100
C CALLED BY: MAIN                                                       ADD00110
C                                                                       ADD00120
C CALLS:  HUMID                                                         ADD00130
C         PROFILE                                                       ADD00140
C                                                                       ADD00150
C       MINERALS MANAGEMENT SERVICE                                     ADD00160
C       U.S. DEPARTMENT OF THE INTERIOR                                 ADD00170
C                                                                       ADD00180
C OCD             REVISION HISTORY:                                     ADD00190
C    DCD 880902   CREATED.                                              ADD00200
C    JCC 930421   UPDATED.                                              ADD00201
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCADD00210
                                                                        ADD00220
      include 'params.cmn'
      INCLUDE 'store.cmn'                                               ADD00230
      INCLUDE 'opt.cmn'                                                 ADD00240
      INCLUDE 'const.cmn'                                               ADD00250
                                                                        ADD00260
      REAL IYW,IZW,IYL,IZL                                              ADD00270
                                                                        ADD00280
C     JYR,JULDAY, AND JHR MUST MATCH THE YEAR, JULIAN DAY AND HOUR      ADD00290
C     READ FROM THE OVERWATER MET FILE (WMET.DAT)                       ADD00300
      READ(13,*,END=9500)KYR,KDAY,KHR,WD,WS,HLW,WHUM,WTA,WTS,WDSHR,     ADD00310
     &     IYW,IZW,IYL,IZL,WDTHDZ                                       ADD00320
      IF(KYR.NE.JYR.OR.KDAY.NE.JULDAY.OR.KHR.NE.JHR) THEN               ADD00330
         WRITE(IO,2) JYR,JULDAY,JHR,KYR,KDAY,KHR                        ADD00340
         WRITE(ierr,2) JYR,JULDAY,JHR,KYR,KDAY,KHR
         STOP 'Error encountered.  See ERROR.OUT for more details.'
      ENDIF                                                             ADD00360
C     STORE INPUT WDTHDZ VALUE FOR OUPUT                                ADD00370
      WWDTHDZ = WDTHDZ                                                  ADD00380
                                                                        ADD00390
C     CHECK EACH PARAMETER; USE ALTERNATE IF MISSING OR TO BE IGNORED   ADD00400
C                                                                       ADD00410
C     WIND DIRECTION  (READ IN AS AZIMUTH, SO NO ALTERATION NEEDED)     ADD00420
C                                                                       ADD00430
      IF(WD.LT.360.1.AND.WD.GT.0.0.AND.JOPT(1).EQ.1) QTHETA(JHR)=WD     ADD00440
C                                                                       ADD00450
C   IF NO OVERWATER WIND DIRECTION, USE LAND DIRECTION IN QTHETA(JHR)   ADD00460
C                                                                       ADD00470
C     WIND SPEED                                                        ADD00480
C                                                                       ADD00490
C     IF OVERLAND SOURCE, DO NOT MODIFY WIND SPEED                      ADD00500
                                                                        ADD00510
      IF(IOPT(24).EQ.1) THEN                                            ADD00520
         WU(JHR)=QU(JHR)                                                ADD00530
      ELSE                                                              ADD00540
         IF(WS.LT.99.0.AND.WS.GT.0.0.AND.JOPT(2).EQ.1) THEN             ADD00550
            WU(JHR) = WS                                                ADD00560
         ELSE                                                           ADD00570
C                                                                       ADD00580
C       IF NO OVERWATER WIND SPEED, MODIFY LAND SPEED                   ADD00590
C                                                                       ADD00600
            WU(JHR) = 3.0 * (QU(JHR)**0.6666667)                        ADD00610
         ENDIF                                                          ADD00620
      ENDIF                                                             ADD00630
C  DO NOT ALLOW WIND SPEEDS LESS THAN 1 M/S                             ADD00640
      IF(WU(JHR).LT.1.0) WU(JHR)=1.0                                    ADD00650
                                                                        ADD00660
C                                                                       ADD00670
C       MIXING HEIGHT                                                   ADD00680
C                                                                       ADD00690
      IF(HLW.GE.1.0.AND.HLW.LE.10000) THEN                              ADD00700
         WHL(JHR) = HLW                                                 ADD00710
      ELSE                                                              ADD00720
         WRITE(IO,6) JYR,JULDAY,JHR                                     ADD00730
         WRITE(IO,10) HLW                                               ADD00740
         WRITE(ierr,6) JYR,JULDAY,JHR
         WRITE(ierr,10) HLW
         STOP 'Error encountered.  See ERROR.OUT for more details.'
      ENDIF                                                             ADD00760
                                                                        ADD00770
C                                                                       ADD00780
C       AIR TEMPERATURE                                                 ADD00790
C                                                                       ADD00800
      IF(WTA.LE.330.0.AND.WTA.GE.200.0) THEN                            ADD00810
         WTAIR(JHR) = WTA                                               ADD00820
      ELSE                                                              ADD00830
         WRITE(IO,6) JYR,JULDAY,JHR                                     ADD00840
         WRITE(IO,11) WTA                                               ADD00850
         WRITE(ierr,6) JYR,JULDAY,JHR
         WRITE(ierr,11) WTA
         STOP 'Error encountered.  See ERROR.OUT for more details.'
      ENDIF                                                             ADD00870
                                                                        ADD00880
C     CALCULATE THE BRUNT-VAISALA FREQUENCY (BVN) IF                    ADD00890
C     WDTHDZ IS AVAILABLE                                               ADD00900
C                                                                       ADD00910
      BVN = -99.99                                                      ADD00920
      IF(JOPT(3).EQ.1) THEN                                             ADD00930
         IF(WDTHDZ.GE.0.) BVN = SQRT((GRAV/WTAIR(JHR))*WDTHDZ)          ADD00940
      ENDIF                                                             ADD00950
                                                                        ADD00960
C                                                                       ADD00970
C     HUMIDITY VARIABLE (RH, WET BULB, OR DEW POINT)                    ADD00980
C                                                                       ADD00990
      CALL HUMID(JYR,JULDAY,JHR,WHUM)                                   ADD01000
                                                                        ADD01010
C                                                                       ADD01020
C       READ IN WATER TEMPERATURE                                       ADD01030
C                                                                       ADD01040
      IF(JOPT(6).EQ.1) THEN                                             ADD01050
         IF(WTS.GE.260.0.AND.WTS.LE.320.0                               ADD01060
     &     .AND.WTA.GE.200.0.AND.WTA.LE.330.0) THEN                     ADD01070
            WTDIFF(JHR) = WTA - WTS                                     ADD01080
         ELSE                                                           ADD01090
            WRITE(IO,6) JYR,JULDAY,JHR                                  ADD01100
            WRITE(IO,12) WTA,WTS                                        ADD01110
            WRITE(ierr,6) JYR,JULDAY,JHR
            WRITE(ierr,12) WTA,WTS
            STOP 'Error encountered.  See ERROR.OUT for more details.'
         ENDIF                                                          ADD01130
      ELSEIF( JOPT(6).EQ.2) THEN                                        ADD01140
         IF(ABS(WTS).LE.50.0) THEN                                      ADD01150
            WTDIFF(JHR) = WTS                                           ADD01160
         ELSE                                                           ADD01170
            WRITE(IO,6) JYR,JULDAY,JHR                                  ADD01180
            WRITE(IO,13) WTS                                            ADD01190
            WRITE(ierr,6) JYR,JULDAY,JHR
            WRITE(ierr,13) WTS
            STOP 'Error encountered.  See ERROR.OUT for more details.'
         ENDIF                                                          ADD01210
      ELSE                                                              ADD01220
         WRITE(IO,14) JOPT(6)                                           ADD01230
         WRITE(ierr,14) JOPT(6)
         STOP 'Error encountered.  See ERROR.OUT for more details.'
      ENDIF                                                             ADD01250
                                                                        ADD01260
C                                                                       ADD01270
C       READ WIND DIRECTIONAL SHEAR IN THE VERTICAL (DEGREES/METER)     ADD01280
C                                                                       ADD01290
      IF(JOPT(7).EQ.1.AND.WDSHR.GE.0.0.AND.WDSHR.LE.180.0) THEN         ADD01300
         WSH(JHR)=WDSHR                                                 ADD01310
      ELSE                                                              ADD01320
         WDSHR = 0.0                                                    ADD01330
      ENDIF                                                             ADD01340
                                                                        ADD01350
C                                                                       ADD01360
C       COMPUTE VALUES OF CRITICAL PROFILE VARIABLES: L AND USTAR       ADD01370
C                                                                       ADD01380
      CALL PROFILE(JHR,Z0W,EL,THSTAR,USTAR)                             ADD01390
                                                                        ADD01400
C                                                                       ADD01410
C       COMPUTE VPTG FROM MONIN-OBUKHOV LENGTH IF NO OBSERVED VALUE     ADD01420
C                                                                       ADD01430
      IF(WDTHDZ.LT.0.0.OR.WDTHDZ.GT.0.5.OR.JOPT(3).EQ.0) THEN           ADD01440
         IF(EL.LE.0.0) THEN                                             ADD01450
             WDTHDZ = 0.0                                               ADD01460
         ELSEIF(EL.GT.5.0) THEN                                         ADD01470
             WDTHDZ = 12.037*THSTAR/EL                                  ADD01480
         ELSEIF(EL.EQ.5.0) THEN                                         ADD01490
             WDTHDZ = 0.05                                              ADD01500
         ENDIF                                                          ADD01510
      ENDIF                                                             ADD01520
                                                                        ADD01530
C                                                                       ADD01540
C       ESTIMATE STABILITY CATEGORY OVER WATER FOR COMPUTATION OF SIGMASADD01550
C                                                                       ADD01560
      IF(EL.LT.0.0.AND.EL.GE.-10.) KWIST = 2                            ADD01570
      IF(EL.LT.-10.0.AND.EL.GE.-25.) KWIST = 3                          ADD01580
      IF(ABS(EL).GT.25.) KWIST = 4                                      ADD01590
      IF(EL.GT.10.0.AND.EL.LE.25.) KWIST = 5                            ADD01600
      IF(EL.GT.0.0.AND.EL.LE.10.) KWIST = 6                             ADD01610
      IF(WDTHDZ.GE.0.04.AND.JOPT(3).EQ.1) KWIST = 7                     ADD01620
C                                                                       ADD01630
      KWST(JHR)=KWIST                                                   ADD01640
      WEL(JHR)=EL                                                       ADD01650
      WUSTAR(JHR)=USTAR                                                 ADD01660
      WDTDZ(JHR)=WDTHDZ                                                 ADD01670
      WZ0(JHR)=Z0W                                                      ADD01680
C                                                                       ADD01690
C       TURBULENCE INTENSITY DATA OVER WATER: ONE OR BOTH OF IY AND IZ  ADD01700
C       MAY BE OBSERVED.  MISSING DATA IS PARAMETERIZED FROM USTAR, EL, ADD01710
C       WHL, AND STACK-TOP HEIGHT.                                      ADD01720
C                                                                       ADD01730
C       SET TURBULENCE INTENSITIES OVER WATER TO -999. IF MISSING       ADD01740
C                                                                       ADD01750
      IF(JOPT(8).EQ.0.OR.IYW.LE.0.0.OR.IYW.GT.2.0) IYW = -.999          ADD01760
      IF(JOPT(9).EQ.0.OR.IZW.LE.0.0.OR.IZW.GT.1.0) IZW = -.999          ADD01770
C                                                                       ADD01780
C       TURBULENCE INTENSITY DATA OVER LAND: EITHER OF IY AND IZ MAY BE ADD01790
C       PROVIDED;  IF MISSING, THEN BRIGGS RURAL DEFAULTS ARE USED TO   ADD01800
C       DETERMINE SIGMA-Y AND SIGMA-Z WITHIN THE CODE.                  ADD01810
                                                                        ADD01820
C                                                                       ADD01830
      IF(JOPT(5).EQ.0) THEN                                             ADD01840
         IYL = -.999                                                    ADD01850
         IZL = -.999                                                    ADD01860
      ELSE                                                              ADD01870
         IF(IYL.LE.0.0.OR.IYL.GT.2.0) IYL = -.999                       ADD01880
         IF(IZL.LE.0.0.OR.IZL.GT.1.0) IZL = -.999                       ADD01890
      ENDIF                                                             ADD01900
      WIY(JHR)=IYW                                                      ADD01910
      WIZ(JHR)=IZW                                                      ADD01920
      QIY(JHR)=IYL                                                      ADD01930
      QIZ(JHR)=IZL                                                      ADD01940
                                                                        ADD01950
      RETURN                                                            ADD01960
                                                                        ADD01970
9500  WRITE(IO,5)                                                       ADD01980
      WRITE(ierr,5)
      STOP 'Error encountered.  See ERROR.OUT for more details.'
                                                                        ADD02000
2     FORMAT(' DATE/HOUR OF LAND MET FILE (',I2,1X,I3,1X,I2,') DOES',/,
     &' NOT AGREE WITH DATE/HOUR OF OVERWATER MET FILE (',I2,1X,I3,1X,
     &I2,')')                                            
5     FORMAT(' END-OF-FILE ENCOUNTERED IN ADDITIONAL (OVERWATER)',
     & ' METEOROLOGICAL DATA.')
6     FORMAT(' YEAR: ',I4,' DAY: ',I4,' HOUR: ',I4)                     
10    FORMAT(' INVALID OVERWATER MIXING HEIGHT VALUE: ',F10.3,' M')     
11    FORMAT(' INVALID OVERWATER TEMPERATURE VALUE: ',F10.3,' K')       
12    FORMAT(' INVALID OVERWATER AIR TEMPERATURE VALUE: ',F10.3,' K  OR'
     &  ,/,' INVALID OVERWATER SURFACE TEMPERATURE VALUE: ',F10.3,' K') 
13    FORMAT(' INVALID OVERWATER SURFACE TEMPERATURE VALUE: ',F10.3,    
     &  ' K')                                                           
14    FORMAT(' INVALID VALUE FOR JOPT(6):  ',I3)                       
                                                                        ADD02150
      END                                                               ADD02160
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCANG00010
      FUNCTION ANGARC (DELM,DELN)                                       ANG00020
C                                                                       ANG00030
C PURPOSE: DETERMINES APPROPRIATE ANGLE OF TAN(ANG) = DELM/DELN         ANG00040
C          WHICH IS REQUIRED FOR CALCULATION OF RESULTANT               ANG00050
C          WIND DIRECTION.                                              ANG00060
C                                                                       ANG00070
C I/O:   DELM, AVERAGE WIND COMPONENT IN THE EAST DIRECTION.            ANG00080
C        DELN, AVERAGE WIND COMPONENT IN THE NORTH DIRECTION.           ANG00090
C                                                                       ANG00100
C CALLED BY:  METRES                                                    ANG00110
C                                                                       ANG00120
C CALLS:  NONE                                                          ANG00130
C                                                                       ANG00140
C       MINERALS MANAGEMENT SERVICE                                     ANG00150
C       U.S. DEPARTMENT OF THE INTERIOR                                 ANG00160
C                                                                       ANG00170
C OCD             REVISION HISTORY:                                     ANG00180
C    DCD 880906   CREATED.                                              ANG00190
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCANG00200
                                                                        ANG00210
      INCLUDE 'const.cmn'                                               ANG00220
                                                                        ANG00230
      IF (DELN) 10,40,80                                                ANG00240
10    IF (DELM) 20,30,20                                                ANG00250
                                                                        ANG00260
20    ANGARC=RAD2DG*ATAN(DELM/DELN)+180.                                ANG00270
      RETURN                                                            ANG00280
30    ANGARC=180.                                                       ANG00290
      RETURN                                                            ANG00300
40    IF (DELM) 50,60,70                                                ANG00310
50    ANGARC=270.                                                       ANG00320
      RETURN                                                            ANG00330
60    ANGARC=0.                                                         ANG00340
                                                                        ANG00350
C     ANGARC=0. INDICATES INDETERMINATE ANGLE                           ANG00360
                                                                        ANG00370
      RETURN                                                            ANG00380
70    ANGARC=090.                                                       ANG00390
      RETURN                                                            ANG00400
80    IF (DELM) 90,100,110                                              ANG00410
90    ANGARC=RAD2DG*ATAN(DELM/DELN)+360.                                ANG00420
      RETURN                                                            ANG00430
100   ANGARC=360.                                                       ANG00440
      RETURN                                                            ANG00450
110   ANGARC=RAD2DG*ATAN(DELM/DELN)                                     ANG00460
                                                                        ANG00470
      RETURN                                                            ANG00480
      END                                                               ANG00490
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCAIP00010
      SUBROUTINE ANNIMP                                                 AIP00020
C                                                                       AIP00030
C PURPOSE:  PRODUCE ANNUAL IMPACT ASSESSMENT FROM NON-PERMANENT         AIP00040
C           ACTIVITIES.                                                 AIP00050
C                                                                       AIP00060
C I/O:  NONE                                                            AIP00070
C                                                                       AIP00080
C CALLED BY:  AVCON                                                     AIP00090
C                                                                       AIP00100
C CALLS:  NONE                                                          AIP00110
C                                                                       AIP00120
C       MINERALS MANAGEMENT SERVICE                                     AIP00130
C       U.S. DEPARTMENT OF THE INTERIOR                                 AIP00140
C                                                                       AIP00150
C OCD             REVISION HISTORY:                                     AIP00160
C    MAP 881208   CREATED.                                              AIP00170
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCAIP00180
                                                                        AIP00190
      include 'params.cmn'
      INCLUDE 'store.cmn'                                               AIP00200
      INCLUDE 'count.cmn'                                               AIP00230
      INCLUDE 'const.cmn'                                               AIP00250
      INCLUDE 'opt.cmn'                                                 AIP00260
                                                                        AIP00270
      REAL AIMPAC(maxrec)                                               AIP00280
                                                                        AIP00290
      DO 100 K=1,maxrec                                                 AIP00300
         AIMPAC(K)=  0.0                                                AIP00310
100   CONTINUE                                                          AIP00320
                                                                        AIP00330
C -- DETERMINE IF THE YEAR BEING RUN IS A LEAP YEAR                     AIP00340
      IF( MOD(IDATE(1),4) .EQ. 0)THEN                                   AIP00350
       ANHOURS = 8784                                                   AIP00360
       IYRD = 366                                                       AIP00370
      ELSE                                                              AIP00380
       ANHOURS = 8760                                                   AIP00390
       IYRD = 365                                                       AIP00400
      ENDIF                                                             AIP00410
                                                                        AIP00420
      DO 200 K=1,NRECEP                                                 AIP00430
         AIMPAC(K)=SUM(K)/ANHOURS                                       AIP00440
200   CONTINUE                                                          AIP00450
                                                                        AIP00460
      WRITE(IO,800) pb                                                  AIP00470
      WRITE(IO,500)IYRD                                                 AIP00480
C###                                                                    AIP00490
C###    SPECIAL FORMAT FOR THE FIRST JAR RECEPTORS                      AIP00500
C###                                                                    AIP00510
      IF(JAR.NE.0) THEN                                                 AIP00520
         DO 300  K = 1,JAR                                              AIP00530
            WRITE(IO,600) K,RNAME(K),RREC(K),SREC(K),                   AIP00540
     &         ZR(K),ELR(K),STAR(1,K),AIMPAC(K)                         AIP00550
300      CONTINUE                                                       AIP00560
      ENDIF                                                             AIP00570
                                                                        AIP00580
      IF(JAR.NE.NRECEP) THEN                                            AIP00590
         DO 400  K=JAR+1,NRECEP                                         AIP00600
            WRITE (IO,700) K,RNAME(K),RREC(K),SREC(K),                  AIP00610
     &         ZR(K),ELR(K),STAR(1,K),AIMPAC(K)                         AIP00620
400      CONTINUE                                                       AIP00630
      ENDIF                                                             AIP00640
                                                                        AIP00650
      WRITE(IO,*) ' '                                                   AIP00660
      WRITE(IO,*) ' '                                                   AIP00670
      WRITE(IO,*) ' '                                                   AIP00680
                                                                        AIP00690
800   FORMAT ( a1,15X,'ANNUAL IMPACT ASSESSMENT FROM NON-PERMANENT ',   AIP00700
     &'ACTIVITIES'//)                                                   AIP00710
500   FORMAT (1X,'RECEPTOR    IDENTIFICATION  ',                        AIP00720
     &'EAST     NORTH     RECEPTOR HT    RECEPTOR GROUND LEVEL',T99,'   AIP00730
     & CONC FOR PERIOD'/1X,T30,'COORD',T39,'COORD  ABV LOCAL GRD LVL    AIP00740
     &   ELEVATION',T94,'DAY 1.HR 1. TO DAY ',I3,'.HR 24.'/1X           AIP00750
     &,T31,'(USER UNITS)        (METERS)         (USER HT UNITS)',T100,'AIP00760
     &(MICROGRAMS/M**3)'/1X)                                            AIP00770
600   FORMAT (1X,T3,I3,10X,A8,5X,F8.2,2X,F8.2,F10.1,F20.1,T110,A1,      AIP00780
     & 6PF7.2)                                                          AIP00790
700   FORMAT (1X,T3,I3,10X,A8,5X,F8.2,2X,F8.2,F10.1,F20.1,T110,A1,      AIP00800
     & 6PF7.2)                                                          AIP00810
                                                                        AIP00820
      RETURN                                                            AIP00830
      END                                                               AIP00840
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCAVC00010
      SUBROUTINE AVCON( DAY1A,HR1,DAY2,NAVT)                            AVC00020
C                                                                       AVC00030
C PURPOSE:  PRODUCE AVERAGE CONCENTRATIONS AND HI-5 TABLES              AVC00040
C                                                                       AVC00050
C I/O:  DAY1A, STARTING DAY                                             AVC00060
C         HR1, STARTNG HOUR                                             AVC00070
C        DAY2, ENDING DAY                                               AVC00080
C        NAVT, ADDITIONAL AVERAGING TIME FOR HIGH-5 TABLE               AVC00090
C                                                                       AVC00100
C CALLED BY:  MAIN                                                      AVC00110
C                                                                       AVC00120
C CALLS:  HIFIVE                                                        AVC00130
C CALLS:  ANNIMP                                                        AVC00140
C                                                                       AVC00150
C       MINERALS MANAGEMENT SERVICE                                     AVC00160
C       U.S. DEPARTMENT OF THE INTERIOR                                 AVC00170
C                                                                       AVC00180
C OCD             REVISION HISTORY:                                     AVC00190
C    DCD 880714   CREATED.                                              AVC00200
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCAVC00210
                                                                        AVC00220
      include 'params.cmn'
      include 'version.cmn'
      INCLUDE 'store.cmn'                                               AVC00230
      INCLUDE 'count.cmn'                                               AVC00260
      INCLUDE 'const.cmn'                                               AVC00280
      INCLUDE 'opt.cmn'                                                 AVC00290
                                                                        AVC00300
      DO 1060 J=1,NRECEP                                                AVC00310
         STAR(1,J)=BLNK                                                 AVC00320
         STAR(2,J)=BLNK                                                 AVC00330
1060  CONTINUE                                                          AVC00340
                                                                        AVC00350
C    CREATE A TABLE OF ANNUAL IMPACT ASSESSMENT                         AVC00360
C    FROM NON-PERMANENT ACTIVITIES                                      AVC00370
      IF(IOPT(23) .EQ. 1) CALL ANNIMP                                   AVC00380
                                                                        AVC00390
      WRITE (IO,1400) pb,model_version,LINE1,LINE2,LINE3                AVC00400
      HR2=NB                                                            AVC00410
      SUM(1)=SUM(1)/NHR                                                 AVC00420
      HIMAX=SUM(1)                                                      AVC00430
      KMX=1                                                             AVC00440
C        INITIALIZE PERIODIC CONC TO BEGIN RANKING FOR PERIODIC MAX     AVC00450
      DO 1070 K=2,NRECEP                                                AVC00460
         SUM(K)=SUM(K)/NHR                                              AVC00470
         IF (SUM(K).GT.HIMAX) THEN                                      AVC00480
            KMX=K                                                       AVC00490
            HIMAX=SUM(K)                                                AVC00500
         ENDIF                                                          AVC00510
1070  CONTINUE                                                          AVC00520
                                                                        AVC00530
      STAR(1,KMX)=STR                                                   AVC00540
C        FIND HIGHEST AVERAGE CONC. AMONG RECEPTORS.                    AVC00550
      WRITE (IO,1740) DAY1A,HR1,DAY2,HR2                                AVC00560
                                                                        AVC00570
C       SPECIAL FORMAT FOR THE FIRST JAR RECEPTORS                      AVC00580
                                                                        AVC00590
        IF(JAR.NE.0) THEN                                               AVC00600
           DO 1075 K = 1,JAR                                            AVC00610
              WRITE(IO,1745) K,RNAME(K),RREC(K),SREC(K),                AVC00620
     &           ZR(K),ELR(K),STAR(1,K),SUM(K)                          AVC00630
1075       CONTINUE                                                     AVC00640
        ENDIF                                                           AVC00650
                                                                        AVC00660
      IF(JAR.NE.NRECEP) THEN                                            AVC00670
         DO 1080 K=JAR+1,NRECEP                                         AVC00680
            WRITE (IO,1750) K,RNAME(K),RREC(K),SREC(K),                 AVC00690
     1         ZR(K),ELR(K),STAR(1,K),SUM(K)                            AVC00700
1080     CONTINUE                                                       AVC00710
      ENDIF                                                             AVC00720
                                                                        AVC00730
      CALL HIFIVE(NAVT,KMX)                                             AVC00740
                                                                        AVC00750
1400  FORMAT ( a1,20X,a,//,21X,A80,/,21X,A80,/,21X,A80)                 AVC00770
1740  FORMAT (1X,'RECEPTOR    IDENTIFICATION  ',                        AVC00780
     &'EAST     NORTH     RECEPTOR HT    RECEPTOR GROUND LEVEL',T99,'AVGAVC00790
     & CONC FOR PERIOD'/1X,T30,'COORD',T39,'COORD  ABV LOCAL GRD LVL    AVC00800
     &   ELEVATION',T94,'DAY',F4.0,'HR',F3.0,' TO DAY',F4.0,'HR',F3.0/1XAVC00810
     &,T31,'(USER UNITS)        (METERS)         (USER HT UNITS)',T100,'AVC00820
     &(MICROGRAMS/M**3)'/1X)                                            AVC00830
1745  FORMAT (1X,T3,I3,10X,A8,5X,F8.2,2X,F8.2,F10.1,F20.1,T110,A1,      AVC00840
     & 6PF7.2)                                                          AVC00850
1750  FORMAT (1X,T3,I3,10X,A8,5X,F8.2,2X,F8.2,F10.1,F20.1,T110,A1,      AVC00860
     & 6PF7.2)                                                          AVC00870
                                                                        AVC00880
      RETURN                                                            AVC00890
      END                                                               AVC00900
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCAVG00010
      SUBROUTINE AVGCNTR(IPOLU)                                         AVG00020
C                                                                       AVG00030
C PURPOSE:  OUTPUT AVERAGING PERIOD CONTRIBUTION                        AVG00040
C                                                                       AVG00050
C I/O:  IPOLU, POLLUTANT LABEL                                          AVG00060
C                                                                       AVG00070
C CALLED BY:  OUTAVG                                                    AVG00080
C                                                                       AVG00090
C CALLS:  NONE                                                          AVG00100
C                                                                       AVG00110
C       MINERALS MANAGEMENT SERVICE                                     AVG00120
C       U.S. DEPARTMENT OF THE INTERIOR                                 AVG00130
C                                                                       AVG00140
C OCD             REVISION HISTORY:                                     AVG00150
C    DCD 881026   CREATED.                                              AVG00160
C    JCC 920819   UPDATED.                                              XXX00160
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCAVG00170
                                                                        AVG00180
      include 'params.cmn'
      INCLUDE 'store.cmn'                                               AVG00200
      INCLUDE 'const.cmn'                                               XXX00200
      character*4 ipolu
                                                                        AVG00210
      WRITE (IO,350) pb,LINE1,LINE2,LINE3                               AVG00220
      WRITE (IO,520) NAVG,IPOLU,IDATE,NB                                AVG00230
      IF (NSIGP.LE.10) THEN                                             AVG00240
C        PRINT FIRST PAGE OF OUTPUT AND TOTALS FOR 10 OR LESS SIGNIF SOUAVG00250
         WRITE (IO,380) (I,I=1,NSIGP)                                   AVG00260
         WRITE (IO,390)                                                 AVG00270
         WRITE (IO,380) (MPS(I),I=1,NSIGP)                              AVG00280
         WRITE (IO,400)                                                 AVG00290
         DO 200 K=1,NRECEP                                              AVG00300
            WRITE (IO,410) K,STAR(1,K),STAR(2,K),(PSIGS(K,I),I=1,NSIGP) AVG00310
C        PRINT TOTALS                                                   AVG00320
            WRITE (IO,420) PSIGS(K,26),PCHI(K)                          AVG00330
200      CONTINUE                                                       AVG00340
         RETURN                                                         AVG00350
      ENDIF                                                             AVG00360
                                                                        AVG00370
C        PRINT FIRST PAGE FOR MORE THAN 10 SIGNIF SOURCES               AVG00380
      WRITE (IO,380) (I,I=1,10)                                         AVG00390
      WRITE (IO,430) (MPS(I),I=1,10)                                    AVG00400
      WRITE (IO,400)                                                    AVG00410
      DO 220 K=1,NRECEP                                                 AVG00420
         WRITE (IO,410) K,STAR(1,K),STAR(2,K),(PSIGS(K,I),I=1,10)       AVG00430
220   CONTINUE                                                          AVG00440
      IF (NSIGP.LE.20) THEN                                             AVG00450
C        PRINT SECOND PAGE AND TOTALS FOR 11 TO 20 SIGNIF SOURCES       AVG00460
         WRITE (IO,350) pb,LINE1,LINE2,LINE3                            AVG00470
         WRITE (IO,520) NAVG,IPOLU,IDATE,NB                             AVG00480
         WRITE (IO,380) (I,I=11,NSIGP)                                  AVG00490
         WRITE (IO,390)                                                 AVG00500
         WRITE (IO,380) (MPS(I),I=11,NSIGP)                             AVG00510
         WRITE (IO,400)                                                 AVG00520
         DO 230 K=1,NRECEP                                              AVG00530
            WRITE (IO,410) K,STAR(1,K),STAR(2,K),(PSIGS(K,I),I=11,NSIGP)AVG00540
            WRITE (IO,420) PSIGS(K,26),PCHI(K)                          AVG00550
230      CONTINUE                                                       AVG00560
         RETURN                                                         AVG00570
      ENDIF                                                             AVG00580
                                                                        AVG00590
C        WRITE SECOND PAGE FOR MORE THAN 20 SIGNIF SOURCES              AVG00600
      WRITE (IO,350) pb,LINE1,LINE2,LINE3                               AVG00610
      WRITE (IO,520) NAVG,IPOLU,IDATE,NB                                AVG00620
      WRITE (IO,380) (I,I=11,20)                                        AVG00630
      WRITE (IO,430) (MPS(I),I=11,20)                                   AVG00640
      WRITE (IO,400)                                                    AVG00650
      DO 250 K=1,NRECEP                                                 AVG00660
         WRITE (IO,410) K,STAR(1,K),STAR(2,K),(PSIGS(K,I),I=11,20)      AVG00670
250   CONTINUE                                                          AVG00680
      WRITE (IO,350) pb,LINE1,LINE2,LINE3                               AVG00690
      WRITE (IO,520) NAVG,IPOLU,IDATE,NB                                AVG00700
                                                                        AVG00710
C        WRITE LAST PAGE AND TOTALS FOR MORE THAN 20 SIGNIF SOURCES     AVG00720
      WRITE (IO,380) (I,I=21,NSIGP)                                     AVG00730
      WRITE (IO,390)                                                    AVG00740
      WRITE (IO,380) (MPS(I),I=21,NSIGP)                                AVG00750
      WRITE (IO,400)                                                    AVG00760
      DO 260 K=1,NRECEP                                                 AVG00770
         WRITE (IO,410) K,STAR(1,K),STAR(2,K),(PSIGS(K,I),I=21,NSIGP)   AVG00780
         WRITE (IO,420) PSIGS(K,26),PCHI(K)                             AVG00790
260   CONTINUE                                                          AVG00800
      RETURN                                                            AVG00810
                                                                        AVG00820
350   FORMAT ( a1,A80/1X,A80/1X,A80)                                    AVG00830
380   FORMAT (1X,'+',T12,10(I3,7X))                                     AVG00840
390   FORMAT (1X,'+',T113,'TOTAL     TOTAL'/1X,T113,'SIGNIF    ',       AVG00850
     &  'ALL POINT',/,1X,T113,'POINT     SOURCES'/1X,'SOURCE #')        AVG00860
400   FORMAT (1X,'RECEP #')                                             AVG00870
410   FORMAT (1X,I3,2A1,6P10F10.3)                                      AVG00880
420   FORMAT (1X,'+',T109,6P2F10.3)                                     AVG00890
430   FORMAT (1X,'SOURCE #',T12,10(I3,7X))                              AVG00900
520   FORMAT ( /,T22,I2,'-HOUR AVERAGE ',A4,' CONTRIBUTION (MICROGRAMS/MAVG00910
     &**3) FROM SIGNIFICANT POINT SOURCES',5X,I2,'/',I3,'  START HOUR:' AVG00920
     &,I2//1X,T5,'RANK')                                                AVG00930
                                                                        AVG00940
      END                                                               AVG00950
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCAVL00010
      SUBROUTINE AVGLOOP(IEND,DELN,DELM,IFREQ,NE,IHSTRT,DAY1,DAY2,JYR)  AVL00020
C                                                                       AVL00030
C PURPOSE:  AVERAGING PERIOD LOOP                                       AVL00040
C                                                                       AVL00050
C I/O:  IEND, END OF FILE INDICATOR                                     AVL00060
C       DELN, AVERAGE WIND COMPONENT IN NORTH DIRECTION                 AVL00070
C       DELM, AVERAGE WIND COMPONENT IN EAST DIRECTION                  AVL00080
C      IFREQ, STABILITY CLASS FREQUENCY IN NUMBER OF HOURS              AVL00090
C         NE, ENDING HOUR LOOP VALUE                                    AVL00100
C     IHSTRT, STARTING HOUR                                             AVL00110
C       DAY1, STARTING DAY                                              AVL00120
C       DAY2, ENDING DAY                                                AVL00130
C        JYR, YEAR                                                      AVL00140
C                                                                       AVL00150
C CALLED BY:  MAIN                                                      AVL00160
C                                                                       AVL00170
C CALLS:  ADDMET                                                        AVL00180
C         METSMRY                                                       AVL00190
C                                                                       AVL00200
C       MINERALS MANAGEMENT SERVICE                                     AVL00210
C       U.S. DEPARTMENT OF THE INTERIOR                                 AVL00220
C                                                                       AVL00230
C OCD             REVISION HISTORY:                                     AVL00240
C    DCD 880729   CREATED.                                              AVL00250
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCAVL00260
                                                                        AVL00270
      include 'params.cmn'
      INCLUDE 'src.cmn'                                                 AVL00280
      INCLUDE 'store.cmn'                                               AVL00290
      INCLUDE 'count.cmn'                                               AVL00310
      INCLUDE 'opt.cmn'                                                 AVL00320
                                                                        AVL00330
      DIMENSION IFREQ(7)                                                AVL00340
                                                                        AVL00350
      U=0.0                                                             AVL00360
      TEMP=0.0                                                          AVL00370
      DELN=0.0                                                          AVL00380
      DELM=0.0                                                          AVL00390
      DO 780 I=1,7                                                      AVL00400
         IFREQ(I)=0.0                                                   AVL00410
780   CONTINUE                                                          AVL00420
      DO 800 I=NB,NE                                                    AVL00430
         JHR=I                                                          AVL00440
         IF(JHR.GT.24) JHR = MOD(JHR,24)                                AVL00450
         DAY2=IDATE(2)                                                  AVL00460
C                                                                       AVL00470
C        IOPT(5) =1.  INPUT HOURLY OVERLAND MET DATA                    AVL00480
C                                                                       AVL00490
         IF (IOPT(5).EQ.1) THEN                                         AVL00500
            READ (IN, * ,END=791) JYR,DAY1,KHR,IKST(JHR),QU(JHR),       AVL00510
     &       QTEMP(JHR),QTHETA(JHR),QHL(JHR)                            AVL00520
C                                                                       AVL00530
C       CHECK FOR CORRECT HOUR SEQUENCE                                 AVL00540
C                                                                       AVL00550
            IF(KHR.NE.JHR) THEN                                         AVL00560
               WRITE(IO,1396) JYR,DAY1,KHR                              AVL00570
               WRITE(ierr,1396) JYR,DAY1,KHR
               STOP 'Error encountered.  See ERROR.OUT for more details.
     &'
            ENDIF                                                       AVL00590
            IF (I.EQ.NB) THEN                                           AVL00600
C        REDEFINE START HOURS AND DATES AT FIRST HOUR OF EACH           AVL00610
C         AVERAGING PERIOD IF READING HOURLY MET DATA.                  AVL00620
               IDATE(1)=JYR                                             AVL00630
               IHSTRT=JHR                                               AVL00640
               IDATE(2)=DAY1                                            AVL00650
               DAY2=IDATE(2)                                            AVL00660
            ENDIF                                                       AVL00670
            IF (IKST(JHR).EQ.7) IKST(JHR)=6                             AVL00680
         ENDIF                                                          AVL00690
C                                                                       AVL00700
C       DETERMINE OVERWATER AND/OR TURBULENCE MET PARAMETERS            AVL00710
C                                                                       AVL00720
        JULDAY = DAY1                                                   AVL00730
        CALL ADDMET(JYR,JULDAY,JHR)                                     AVL00740
        IF(IOPT(10).EQ.0) CALL METSMRY(I,JHR,DELN,DELM,IFREQ,URES)      AVL00750
                                                                        AVL00760
800   CONTINUE                                                          AVL00770
      RETURN                                                            AVL00780
                                                                        AVL00790
791   IEND=1                                                            AVL00800
                                                                        AVL00810
1396  FORMAT(' HOUR READ IN LAND METEOROLOGY INPUT FILE IS NOT IN',
     &' SEQUENCE (',I2,1X,F4.0,1X,I2,')')         
                                                                        AVL00840
      RETURN                                                            AVL00850
      END                                                               AVL00860
                                                                        OCD06570
      BLOCK DATA                                                        OCD06580
                                                                        OCD06590
      include 'params.cmn'
      INCLUDE 'exps.cmn'                                                OCD06600
      INCLUDE 'funcs.cmn'                                               OCD06610
      INCLUDE 'store.cmn'                                               OCD06620
      INCLUDE 'const.cmn'                                               OCD06630
                                                                        OCD06640
C  CONSTANT DEFINITIONS                                                 OCD06650
      DATA NTIME /1,3,8,24,0/ ,ATIME /1.,3.,8.,24.,0./                  OCD04370
      DATA GRAV/ 9.81/,MISS/ -999./,RAD2DG/ 57.29578/,DG2RAD/0.017453/, OCD06660
     &     KM2M/ 1000./,RT2PI/ 2.5066283/,PI/ 3.1415927/,LETL/'L'/,     OCD06670
     &     LETW/'W'/                                                    OCD06680
      DATA STR/'*'/,BLNK/' '/,                                          OCD06690
     &     TITLE/'SO2 ','TSP ','NOX ','CO  ','    '/                    OCD06700
                                                                        OCD06710
                                                                        OCD06720
      DATA STAR/max2*' '/                                               OCD06730
                                                                        OCD06740
C   COEFFICIENTS GENERATED WITH RURAL SIGMAS USING PGYZ                 OCD06750
C   RELATIVE CONC. NORMALIZED FOR WIND SPEED FROM PT SOURCE, CHI*U/Q, = OCD06760
C        PXUCOF(KST,IH)*H**PXUEXP(KST,IH)                               OCD06770
C      IH=1 FOR H LESS THAN 20 METERS.                                  OCD06780
C      IH=2 FOR H FROM 20 TO 30 METERS.                                 OCD06790
C      IH=3 FOR H FROM 30 TO 50 METERS.                                 OCD06800
C      IH=4 FOR H FROM 50 TO 70 METERS.                                 OCD06810
C      IH=5 FOR H FROM 70 TO 100 METERS.                                OCD06820
C      IH=6 FOR H FROM 100 TO 200 METERS.                               OCD06830
C      IH=7 FOR H FROM 200 TO 300 METERS.                               OCD06840
C      IH=8 FOR H FROM 300 TO 500 METERS.                               OCD06850
C      IH=9 FOR H GREATER THAN 500 METERS.                              OCD06860
      DATA PXUCOF /.10401E+00,.12133E+00,.14273E+00,.15351E+00,.18855E+0OCD06870
     &0,.18668E+00,.77533E-01,.11728E+00,.14120E+00,.18239E+00,.20458E+0OCD06880
     &0,.34326E+00,.67228E-01,.10013E+00,.13963E+00,.19162E+00,.38998E+0OCD06890
     &0,.76271E+00,.40484E-01,.75308E-01,.13784E+00,.54357E+00,.72550E+0OCD06900
     &0,.22936E+01,.28539E-01,.66936E-01,.13615E+00,.52790E+00,.12908E+0OCD06910
     &1,.56943E+01,.14792E-01,.65799E-01,.13315E+00,.74832E+00,.28818E+0OCD06920
     &1,.40940E+03,.12403E-01,.64321E-01,.12927E+00,.10826E+01,.77020E+0OCD06930
     &2,.23011E+05,.12340E-01,.62874E-01,.12546E+00,.15580E+01,.68810E+0OCD06940
     &3,.46522E+06,.12245E-01,.60615E-01,.11952E+00,.22517E+01,.42842E+0OCD06950
     &3,.00000E+00/                                                     OCD06960
      DATA PXUEXP /-.19460E+01,-.19774E+01,-.20086E+01,-.20742E+01,-.218OCD06970
     &22E+01,-.22176E+01,-.18479E+01,-.19661E+01,-.20050E+01,-.21317E+01OCD06980
     &,-.22094E+01,-.24209E+01,-.18060E+01,-.19196E+01,-.20017E+01,-.214OCD06990
     &62E+01,-.23991E+01,-.26556E+01,-.16763E+01,-.18468E+01,-.19984E+01OCD07000
     &,-.24128E+01,-.25578E+01,-.29371E+01,-.15940E+01,-.18191E+01,-.199OCD07010
     &55E+01,-.24059E+01,-.26934E+01,-.31511E+01,-.14513E+01,-.18153E+01OCD07020
     &,-.19907E+01,-.24817E+01,-.28678E+01,-.40795E+01,-.14181E+01,-.181OCD07030
     &11E+01,-.19851E+01,-.25514E+01,-.34879E+01,-.48399E+01,-.14172E+01OCD07040
     &,-.18071E+01,-.19799E+01,-.26152E+01,-.38719E+01,-.53670E+01,-.141OCD07050
     &60E+01,-.18012E+01,-.19721E+01,-.26744E+01,-.37956E+01,-.17020E+02OCD07060
     &/                                                                 OCD07070
      DATA HC1 /10.,20.,30.,50.,70.,100.,200.,300.,500.,1000./          OCD07080
C                                                                       OCD07090
C       DATA FOR COMPUTATION OF VIRTUAL DISTANCES USING P-G COEFFICIENTSOCD07100
C                                                                       OCD07110
      DATA AS/122.8,158.08,170.22,179.52,217.41,258.89,346.75,453.85,   OCD07120
     &90.673,98.483,109.3,61.141,34.459,32.093,32.093,33.504,36.65,44.05OCD07130
     &3,24.26,23.331,21.628,21.628,22.534,24.703,26.97,35.42,47.618,    OCD07140
     &15.209,14.457,13.953,13.953,14.823,16.187,17.836,22.651,27.074,   OCD07150
     &34.219/                                                           OCD07160
      DATA BS/.9447,1.0542,1.0932,1.1262,1.2644,1.4094,1.7283,2.1166,   OCD07170
     &.93198,.98332,1.0971,.91465,.86974,.81066,.64403,.60486,.56589,   OCD07180
     &.51179,.8366,.81956,.7566,.63077,.57154,.50527,.46713,.37615,     OCD07190
     &.29592,.81558,.78407,.68465,.63227,.54503,.4649,.41507,.32681,    OCD07200
     &.27436,.21716/                                                    OCD07210
      DATA PS/209.14,154.46,103.26,68.26,51.06,33.92/                   OCD07220
      DATA QS/.89,.902,.917,.919,.921,.919/                             OCD07230
      DATA IA/1,9,12,13,19,28,38/                                       OCD07240
      DATA DST/0.1,0.15,0.2,0.25,0.3,0.4,0.5,9999.,0.2,0.4,9999.,9999., OCD07250
     &0.3,1.0,3.0,10.0,30.0,9999.,0.1,0.3,1.0,2.0,4.0,10.0,20.0,40.0,   OCD07260
     &9999.,0.2,0.7,1.0,2.0,3.0,7.0,15.0,30.0,60.0,9999./               OCD07270
                                                                        OCD07280
      END                                                               OCD07290
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCAL00010
      SUBROUTINE CALC(XD,HTERRAIN,EFFHT,WIDTHB,WDIR)                    CAL00020
C                                                                       CAL00030
C PURPOSE:  DETERMINE RELATIVE CONCENTRATIONS (CHI/Q) FOR EACH SOURCE   CAL00040
C                                                                       CAL00050
C I/O:   XD, DISTANCE FROM SOURCE TO RECEPTOR, in KM                    CAL00060
C  HTERRAIN, MOUNTAIN/HILL HEIGHT IN VICINITY OF RECEPTOR               CAL00070
C     EFFHT, EFFECTIVE STACK HEIGHT DUE TO MOMENTUM RISE                CAL00080
C    WIDTHB, BUILDING WIDTH                                             CAL00090
C      WDIR, HOURLY WIND DIRECTION                                      CAL00100
C                                                                       CAL00110
C CALLED BY:  RECEP                                                     CAL00120
C                                                                       CAL00130
C CALLS: SIGMA                                                          CAL00140
C     DOWNWASH                                                          CAL00150
C          VDF                                                          CAL00160
C      SIGTHTA                                                          CAL00170
C         VIRT                                                          CAL00180
C      TERRAIN                                                          CAL00190
C     VERTICAL                                                          CAL00200
C                                                                       CAL00210
C       MINERALS MANAGEMENT SERVICE                                     CAL00220
C       U.S. DEPARTMENT OF THE INTERIOR                                 CAL00230
C                                                                       CAL00240
C OCD             REVISION HISTORY:                                     CAL00250
C    DCD 880914   CREATED.                                              CAL00260
C    JCC 930310   UPDATED.                                              XXX00260
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCAL00270
                                                                        CAL00280
      include 'params.cmn'
      INCLUDE 'opt.cmn'                                                 CAL00290
      INCLUDE 'met.cmn'                                                 CAL00300
      INCLUDE 'store.cmn'                                               CAL00320
      INCLUDE 'funcs.cmn'                                               CAL00330
      INCLUDE 'ptrrcp.cmn'                                              CAL00340
      INCLUDE 'const.cmn'                                               CAL00350
      INCLUDE 'hrdat.cmn'                                               CAL00360
      INCLUDE 'linesrc.cmn'                                             CAL00370
      INCLUDE 'count.cmn'                                               CAL00380
                                                                        CAL00390
C   BRIGGS(1973) DEFAULT RURAL IZ VALUES                                CAL00400
      REAL BRIGGSZ(7)/0.20,0.12,0.08,0.06,0.03,0.016,0.016/             CAL00410
                                                                        CAL00420
C       ALL DISTANCES ARE IN KM                                         CAL00430
C                                                                       CAL00440
C         RC   RELATIVE CONCENTRATION (SEC/M**3) ,CHI/Q                 CAL00450
C        THE FOLLOWING EQUATION IS SOLVED --                            CAL00460
C        RC = (1/(2*PI*UPL*SIGMA Y*SIGMA Z)) *                          CAL00470
C          (EXP(-0.5*((DTHETA)/SIGTHC)**2)))*                           CAL00480
C          (EXP(-0.5*((Z-H)/SIGMA Z)**2) + EXP(-0.5*((Z+H)/SIGMA Z)**2) CAL00490
C             PLUS THE SUM OF THE FOLLOWING 4 TERMS K TIMES (N=1,K) --  CAL00500
C                  FOR NEUTRAL OR UNSTABLE CASES:                       CAL00510
C              TERM 1- EXP(-0.5*((Z-H-2NL)/SIGMA Z)**2)                 CAL00520
C              TERM 2- EXP(-0.5*((Z+H-2NL)/SIGMA Z)**2)                 CAL00530
C              TERM 3- EXP(-0.5*((Z-H+2NL)/SIGMA Z)**2)                 CAL00540
C              TERM 4- EXP(-0.5*((Z+H+2NL)/SIGMA Z)**2)                 CAL00550
c        where dtheta is the angle bounded by the wind direction and    XXX00550
c        the line connecting receptor and the virtual source.           XXX00551
C        THE ABOVE EQUATION IS SIMILAR TO EQUATION (5.8) P 36 IN        CAL00560
C        WORKBOOK OF ATMOSPHERIC DISPERSION ESTIMATES WITH THE ADDITION CAL00570
C        OF THE EXPONENTIAL INVOLVING Y.                                CAL00580
                                                                        CAL00590
      RCVIR = 0.0                                                       CAL00600
C                                                                       CAL00610
C     IF PLUME IS ABOVE THE OVERWATER MIXING LID, THEN CHANGE THE OVER- CAL00620
C     WATER STABILITY CLASS FOR THIS SOURCE TO 5 TO ALLOW PLUME ENTRY   CAL00630
C     INTO THE TIBL                                                     CAL00640
C                                                                       CAL00650
      HLR=HL                                                            CAL00660
      KWSST = KWST(IHR)                                                 CAL00670
      IF(HA.GT.HLR) KWSST = 5                                           CAL00680
C     USE CLASS D SIGMA-Z FUNCTION FOR OVERWATER CLASSES A - D          CAL00690
      IF(KWSST.LE.4) THEN                                               CAL00700
         KDUM = 4                                                       CAL00710
      ELSE                                                              CAL00720
         KDUM = KWSST                                                   CAL00730
      ENDIF                                                             CAL00740
C                                                                       CAL00750
C        IF X IS LESS THAN 1 METER, SET RC=0. AND RETURN.  THIS AVOIDS  CAL00760
C         PROBLEMS OF INCORRECT VALUES NEAR THE SOURCE.                 CAL00770
      IF (X.LT.0.001) THEN                                              CAL00780
         RC=0.                                                          CAL00790
         RETURN                                                         CAL00800
      ENDIF                                                             CAL00810
                                                                        CAL00820
C   CALCULATE INITIAL SIGMA-Y AND SIGMA-Z DUE TO                        CAL00830
C   BUILDING DOWNWASH                                                   CAL00840
      IF(WIDTHB .GT. 0. .AND. HB .GT. 0.) THEN                          CAL00850
         CALL DOWNWASH(XD,HB,WIDTHB,EFFHT,SIGZ0,SIGY0)                  CAL00860
      ELSE                                                              CAL00870
         SIGZ0 = 0.0                                                    CAL00880
         SIGY0 = 0.0                                                    CAL00890
      ENDIF                                                             CAL00900
                                                                        CAL00910
C     CALCULATE PLUME PATH CORRECTION FACTOR                            CAL00920
C       AND CRITICAL DIVIDING STREAMLINE HEIGHT                         CAL00930
      CALL TERRAIN( HTERRAIN,PPC,HCRIT)                                 CAL00940
                                                                        CAL00950
C     COMPUTE CONTRIBUTION TO SIGMA-Y DUE TO DIRECTIONAL SHEAR          CAL00960
C     SIGYS BELOW NEEDS TO BE MULTIPLIED BY X AND SIGMA-Z TO BE         CAL00970
C     FINALIZED                                                         CAL00980
C     NOTE:  SQRT( .03) = 0.17, WSH INPUT AS DEG/M                      CAL00990
C                                                                       CAL01000
      IF(JOPT(7).EQ.1) THEN                                             CAL01010
         SIGYS = 0.17 * WSH(IHR) * DG2RAD                               CAL01020
      ELSE                                                              CAL01030
         SIGYS = 0.0                                                    CAL01040
      ENDIF                                                             CAL01050
                                                                        CAL01060
C     COMPUTE SIGMA-THETA                                               CAL01070
      CALL SIGTHTA( SIGTHS,SIGTHL)                                      CAL01080
                                                                        CAL01090
C     DETERMINE NATURE OF PLUME BEHAVIOR AS IT CROSSES FROM WATER       CAL01100
C     TO LAND.                                                          CAL01110
      VIRTY = 0.0                                                       CAL01120
      VIRTZ = 0.0                                                       CAL01130
      SB = 0.0                                                          CAL01140
      SY0 = 0.0                                                         CAL01150
      SZ0 = 0.0                                                         CAL01160
      SIGYW = 0.0                                                       CAL01170
      SIGZW = 0.0                                                       CAL01180
      SIGYWA = 0.0                                                      CAL01190
      SIGZWA = 0.0                                                      CAL01200
      SIGYSW = 0.0                                                      CAL01210
                                                                        CAL01220
C     X2 = DISTANCE FROM LAND TO RECEPTOR (M)                           CAL01230
      IF( XLAND .LE. 0.) XLAND = 0.0                                    CAL01240
      X2 = (XD-XLAND)*1000.                                             CAL01250
C     CALCULATE TIBL HEIGHT (HT) AND DISTANCE FROM WHERE THE PLUME      CAL01260
C     CROSSES THE SHORE TO WHERE THE PLUME ENTERS THE TIBL(XTIBL) IN KM CAL01270
C                                                                       CAL01280
C     THE STABLE INTERNAL BOUNDARY LAYER IS CAPPED AT                   CAL01290
C            0.4*SQRT((U*L)/F)                                          CAL01300
      CAP = 99999.                                                      CAL01310
      IF(KST.GE.5) THEN                                                 CAL01320
        CAP = 0.4*SQRT(USTARL*ELLAND/FCOR)                              CAL01330
C     PLUME NEVER ENTERS THE TIBL                                       CAL01340
        IF(HA.GT.CAP) THEN                                              CAL01350
           XL=X+1.                                                      CAL01360
           HT=-1                                                        CAL01370
        ENDIF                                                           CAL01380
      ENDIF                                                             CAL01390
      CALL TIBL(HT,XTIBL,X2,CAP)                                        CAL01400
C   THE EFFECT OF TERRAIN IS PARAMETERIZED BY TERRAIN ADJUSTMENT        CAL01410
C   FACTORS ACCORDING TO STABILITY CLASS AS IN COMPLEX I/II             CAL01420
C   AND THE USE OF HCRIT                                                CAL01430
      HPS=HA                                                            CAL01440
      IF(XLAND.GT.0) HPS=HA+EPM                                         CAL01450
C   IF OFFSHORE RECEPTOR USE RECEPTOR HEIGHT ABOVE WATER                CAL01460
      DUM2=ZER                                                          CAL01470
      IF(XLAND.LE.0.)DUM2=ZER-EPM                                       CAL01480
                                                                        CAL01490
C   CALCULATE H = ADJUSTED PLUME HEIGHT ABOVE LOCAL TERRAIN             CAL01500
C   HPS = PLUME HEIGHT ABOVE STACK BASE                                 CAL01510
      IF( BVN .LE. 0.0) HCRIT = 0.0                                     CAL01520
      IF( DUM2 .LE. HPS) THEN                                           CAL01530
         H = HPS - (1.-PPC)*(DUM2+HCRIT)                                CAL01540
      ELSE                                                              CAL01550
         H = PPC*(HPS-HCRIT)                                            CAL01560
      ENDIF                                                             CAL01570
                                                                        CAL01580
      H=AMAX1(H,ZMIN)                                                   CAL01590
                                                                        CAL01600
      XL=XLAND+XTIBL                                                    CAL01610
                                                                        CAL01620
C   IF NO WATER BETWEEN SOURCE & RECEPTOR (XL<0), SKIP VIRTUAL          CAL01630
C   DISTANCE CALCULATIONS.                                              CAL01640
      IF(XLAND.LE.0.0.OR.XL.LE.0.0) THEN                                CAL01650
         XL = 0.0                                                       CAL01660
         XTIBL = 0.0                                                    CAL01670
         DENOM = 1.0                                                    CAL01680
         GO TO 1000                                                     CAL01690
      ENDIF                                                             CAL01700
                                                                        CAL01710
C    USE CLASS D FOR OVERWATER CLASSES A-D                              CAL01720
      CALL SIGMA(XL,XL,KDUM,IYWS,IZWS,SIGYWA,SIGZWA,UPL,0)              CAL01730
                                                                        CAL01740
C    IF STABILITY G OVER WATER, COMPUTE SIG-Z BASED UPON VPTG           CAL01750
      IF(KWSST.GE.7) THEN                                               CAL01760
         SZX = 1.0/SQRT(1.0 + SQRT(S) * XL * KM2M/(0.32*UPL))           CAL01770
         SIGZWA = IZWS * XL * KM2M * SZX                                CAL01780
      ENDIF                                                             CAL01790
      SIGYSW = SIGYS * XL * KM2M * SIGZWA                               CAL01800
      SIGYW = SQRT(SIGYWA*SIGYWA + SIGB*SIGB + SIGYSW*SIGYSW +          CAL01810
     &  SIGY0*SIGY0)                                                    CAL01820
      SIGZW = SQRT(SIGZWA*SIGZWA + SIGB*SIGB + SIGZ0*SIGZ0)             CAL01830
      SB = SIGB                                                         CAL01840
      SY0 = SIGY0                                                       CAL01850
      SZ0 = SIGZ0                                                       CAL01860
                                                                        CAL01870
C       FOR POINT SOURCES,                                              CAL01880
C         IF RECEPTOR IS NOT BEYOND XL (DISTANCE FROM SOURCE TO TIBL)   CAL01890
C         THEN DO NOT CALCULATE VIRTUAL DISTANCES                       CAL01900
C       FOR AREA AND LINE SOURCES, CALCULATE VIRTUAL DISTANCES          CAL01910
C                                                                       CAL01920
      IF((IOPT(20).EQ.0 .AND. XD.GT.XL) .OR. (IOPT(20).EQ.1) .OR.       CAL01930
     &    (IOPT(20).EQ.2)) GO TO 490                                    CAL01940
C                                                                       CAL01950
C      CALCULATE SIGMAS BASED ON SOURCE RECEPTOR ON LAND OR OVER WATER  CAL01960
                                                                        CAL01970
         CALL SIGMA(XD,XD,KDUM,IYWS,IZWS,SIGYWA,SIGZWA,UPL,0)           CAL01980
                                                                        CAL01990
C     IF STABILITY G OVER WATER, COMPUTE SIG-Z BASED UPON VPTG          CAL02000
         IF(KWSST.GE.7) THEN                                            CAL02010
            SZX = 1.0/SQRT(1.0 + SQRT(S) * XD * KM2M/(0.32*UPL))        CAL02020
            SIGZWA = IZWS * XD * KM2M * SZX                             CAL02030
         ENDIF                                                          CAL02040
         SIGYSX = SIGYS * XD * KM2M * SIGZWA                            CAL02050
         SY = SQRT(SIGYWA*SIGYWA + SIGB*SIGB + SIGYSX*SIGYSX +          CAL02060
     &        SIGY0*SIGY0)                                              CAL02070
         SZ = SQRT(SIGZWA*SIGZWA + SIGB*SIGB + SIGZ0*SIGZ0)             CAL02080
                                                                        CAL02090
C     USED IN HORIZONTAL PORTION OF DIFFUSION EQUATION                  CAL02100
         XSR = XD*KM2M                                                  CAL02110
c                                                                       XXX02110
c  Calculate SIGTHC at XSR using source sigma theta with                XXX02111
c  Draxler method.                                                      XXX02112
c                                                                       XXX02113
        time = amin1(xsr,10000.)/upl                                    XXX02114
        sigthc = sigths/(1.+0.9*sqrt(time/1000.))                       XXX02115
C-- FOR PRINTOUT                                                        CAL02130
         SIGYA=SIGYWA                                                   CAL02140
         SIGZA=SIGZWA                                                   CAL02150
         XTIBL = -99.9                                                  CAL02160
         HT = -999.                                                     CAL02170
         SIGYWA = -999.9                                                CAL02180
         SIGYSW = -999.9                                                CAL02190
         SIGYW = -999.9                                                 CAL02200
         SIGZWA = -999.9                                                CAL02210
         SIGZW = -999.9                                                 CAL02220
                                                                        CAL02230
      GO TO 1100                                                        CAL02240
                                                                        CAL02250
C   CALCULATE VIRTUAL DISTANCES                                         CAL02260
490   CALL VIRT( DENOM,XL,XD,SIGZWA,SIGZA,                              CAL02270
     &           SIGTHS,SIGTHL,SIGTHC,XSR,VIRTY,VIRTZ)                  CAL02280
      GO TO 1050                                                        CAL02290
                                                                        CAL02300
                                                                        CAL02310
C   OVERLAND SOURCE - DO NOT USE VIRTUAL DISTANCES                      CAL02320
C   USE BRIGGS(1973) RURAL IZ AS DEFAULT IF IZLS MISSING                CAL02330
1000  IF(QIZ(LH).LE.0.0) IZLS = BRIGGSZ(KST)                            CAL02340
      CALL SIGMA(XD,XD,KST,IYLS,IZLS,DUM,SIGZA,UPL,2)                   CAL02350
      XSR = XD*KM2M                                                     CAL02360
c                                                                       XXX02360
c  Calculate SIGTHC at XSR using land sigma theta with                  XXX02361
c  Draxler method.                                                      XXX02362
        time = amin1(xsr,10000.)/upl                                    XXX02363
        sigthc = sigthl/(1.+0.9*sqrt(time/1000.))                       XXX02364
c                                                                       CAL02380
1050  IF(IOPT(20) .EQ. 1 .OR. IOPT(20) .EQ. 2) THEN                     CAL02390
         FY = 1./(1.+ 0.9*SQRT(VIRTY*KM2M/(1000.*UPL)))                 CAL02400
         SIGYA = SIGTHS*VIRTY*KM2M*FY                                   CAL02410
      ELSE                                                              CAL02420
         SIGYA = SIGTHC * XSR                                           CAL02430
      ENDIF                                                             CAL02440
      SIGYSX = SIGYS * XD * KM2M * SIGZA                                CAL02450
                                                                        CAL02460
C                                                                       CAL02470
C-------------------SIGMA-Y AND SIGMA-Z-----------------                CAL02480
C                                                                       CAL02490
      SY = SQRT(SIGYA*SIGYA + SIGB*SIGB + SIGYSX*SIGYSX + SIGY0*SIGY0)  CAL02500
      SZ = SQRT(SIGZA*SIGZA + SIGB*SIGB + SIGZ0*SIGZ0)                  CAL02510
C                                                                       CAL02520
C  DETERMINE MIXING DEPTH FOR VERTICAL MIXING                           CAL02530
C                                                                       CAL02540
1100  CALL MIXHT( HLR,INTIBL,XD,XL,X2,XTIBL,SIGZW,CAP)                  CAL02550
                                                                        CAL02560
                                                                        CAL02570
C SET CONCENTRATION TO ZERO IF PLUME OR RECEPTOR IS ABOVE MIXING HEIGHT CAL02580
      IF(H.GT.HLR.OR.Z.GT.HLR) THEN                                     CAL02590
         RC = 0.0                                                       CAL02600
         RETURN                                                         CAL02610
      ENDIF                                                             CAL02620
                                                                        CAL02630
C    COMPUTE HORIZONTAL GAUSSIAN TERM                                   CAL02640
c    Note that because of the virtual source, the lateral distance      XXX02640
c    (in polar coordinates) of the plume is no longer equal to          XXX02641
c    (wdir-angr2s), the proper lateral distance should be the angle     XXX02642
c    bounded by the wind direction and the line connecting receptor     XXX02643
c    and the virtual source.                                            XXX02644
c    First the units for some of the variables:                         XXX02645
c    X:     km                                                          XXX02646
c    XD:    km                                                          XXX02647
c    XL:    km                                                          XXX02648
c    XSR:   m                                                           XXX02649
c    VIRTY: km                                                          XXX02650
c                                                                       XXX02651
c  Note that XL > VIRTY if SIGTHL > SIGTHS                              XXX02652
c            XL < VIRTY if SIGTHL < SIGTHS                              XXX02653
c                                                                       XXX02654
      IF (XD.GT.XL) THEN                                                XXX02655
C  That is, the receptor is in the TIBL, and the virtual source is      XXX02656
C  Used.                                                                XXX02657
        arg = (x-(xl-virty))/(xsr/km2m)                                 XXX02658
        thta2 = acos_x (arg,'CALC')                                     XXX02659
      ELSE                                                              XXX02660
C  That is , the receptor is before the TIBL, and the real source       XXX02661
C  is used                                                              XXX02662
        THTA2 = ACOS_X (X/XD,'CALC')                                    XXX02663
      ENDIF                                                             XXX02664
      dum = thta2/sigthc                                                XXX02665
      TMP = 0.5*DUM*DUM                                                 CAL02660
                                                                        CAL02670
      IF (TMP.GE.50.) THEN                                              CAL02680
         RC = 0.0                                                       CAL02690
         RETURN                                                         CAL02700
      ENDIF                                                             CAL02710
      HORIZ = EXP( -TMP)/(RT2PI*SY)                                     CAL02720
      ICHK = 0                                                          CAL02730
      SZSAVE = SZ                                                       CAL02740
                                                                        CAL02750
C  COMPUTE GAUSSIAN VERTICAL TERM AND RELATIVE CONCENTRATION            CAL02760
      CALL VERTICAL( HLR,HORIZ,VERT)                                    CAL02770
      VERTSAVE = VERT                                                   CAL02780
                                                                        CAL02790
C  IF PLUME IS IN THE TIBL, CALCULATE CHI USING VIRTUAL SOURCE METHOD   CAL02800
C  AND BY ASSUMING THAT THE PLUME IS COMPLETELY MIXED IN THE TIBL.      CAL02810
C  CHI IS THEN THE MAXIMUM OF BOTH CALCULATIONS                         CAL02820
                                                                        CAL02830
C   ICHECK = 0  TURNER'S METHOD USED                                    CAL02840
C   ICHECK = 1  VIRTUAL METHOD USED                                     CAL02850
      ICHECK = 0                                                        CAL02860
c                                                                       XXX02860
c  Save the original sz at the interface of TIBL.                       XXX02861
      sztmp=sz                                                          XXX02862
c                                                                       XXX02863
      IF(INTIBL .EQ. 1 .AND. ICHK .EQ. 0) THEN                          CAL02870
         ICHK = 1                                                       CAL02880
         SZ = 999.                                                      CAL02890
         RCVIR = RC                                                     CAL02900
         CALL VERTICAL( HLR,HORIZ,VERT)                                 CAL02910
         IF(RC .GT. RCVIR) ICHECK = 1                                   CAL02920
      ENDIF                                                             CAL02930
                                                                        CAL02940
      IF( RCVIR .GT. RC) THEN                                           CAL02950
         SZ = SZSAVE                                                    CAL02960
         VERT = VERTSAVE                                                CAL02970
         RC = RCVIR                                                     CAL02980
      ENDIF                                                             CAL02990
                                                                        CAL03000
      IF(IOPT(21).EQ.1) THEN                                            CAL03010
         CHI = RC * Q * DECAYF * 1000000. * PMASS                       CAL03020
         WRITE(7,501) IDAY,IHR,JS,K,X,Y,ZER,H,HA,XLAND,                 CAL03030
     &    XSR/KM2M,XD,XTIBL,HT,SY,SZtmp,HLR,VIRTY,VIRTZ,XL,INTIBL,      CAL03040
     &    ICHECK,CHI                                                    CAL03050
      ENDIF                                                             CAL03060
                                                                        CAL03070
      IF(IOPT(13).EQ.1.AND.IOPT(16).EQ.1) RETURN                        CAL03080
                                                                        CAL03090
C       PRINT PLUME DISPERSION CHARACTERISTICS FOR EACH RECEPTOR        CAL03100
      VERT=VERT*RT2PI*SZ                                                CAL03110
      HORIZ=HORIZ*RT2PI*SY                                              CAL03120
      CHI = RC * Q * DECAYF * 1000000. * PMASS                          CAL03130
      WRITE(IO,1) K,X,Y,XLAND,ZER,HA,SIGYWA,SB,                         CAL03140
     &   SY0,SIGYSW,SIGYW,SIGZWA,SB,SZ0,SIGZW,HORIZ,CHI,DECAYF,         CAL03150
     &   H,SIGYA,SIGB,SIGY0,SIGYSX,SY,SIGZA,SIGB,SIGZ0,SZ,VERT,HT,      CAL03160
     &   HLR,XTIBL,HCRIT                                                CAL03170
                                                                        CAL03180
                                                                        CAL03190
1     FORMAT(1X,I3,F6.2,F7.2,F6.2,F6.1,F7.1,F7.1,F6.1,F6.1,F6.1,        CAL03200
     &  F7.1,1X,F7.1,F6.1,F6.1,F7.1,1X,E9.4,2X,F7.1,1X,'DF:',F5.3,/,    CAL03210
     &  T30,2F7.1,F6.1,2F6.1,F7.1,1X,F7.1,2F6.1,F7.1,1X,F9.6,10X,'HT:', CAL03220
     &  F5.0,' HL:',F6.0,/,115X,'XTIBL:',F5.1,/,115X,'HCRIT: ',F6.0,/)  CAL03230
501   FORMAT(4I3,2F5.1,3F6.1,4F7.3,F7.1,f6.0,F6.1,f6.0,3F7.3,2I2,1p,
     &       e9.2)
                                                                        CAL03250
      RETURN                                                            CAL03260
      END                                                               CAL03270
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCUB00010
      SUBROUTINE CUBIC(A,B,C,Z)                                         CUB00020
C                                                                       CUB00030
C PURPOSE: SOLVES FOR ONE ROOT OF THE CUBIC EQUATION:                   CUB00040
C          Z**3 + A*Z**2 + B*Z + C = 0                                  CUB00050
C                                                                       CUB00060
C I/O:  A,B,C,Z COEFFICIENTS OF CUBIC EQUATION                          CUB00070
C                                                                       CUB00080
C CALLED BY:  PTR                                                       CUB00090
C             RECEP                                                     CUB00100
C                                                                       CUB00110
C CALLS: NONE                                                           CUB00120
C                                                                       CUB00130
C       MINERALS MANAGEMENT SERVICE                                     CUB00140
C       U.S. DEPARTMENT OF THE INTERIOR                                 CUB00150
C                                                                       CUB00160
C OCD             REVISION HISTORY:                                     CUB00170
C    DCD 880916   CREATED.                                              CUB00180
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCUB00190
                                                                        CUB00200
      IMPLICIT REAL*8 (A-H,O-Z)                                         CUB00210
      REAL A,B,C,Z                                                      CUB00220
      DATA ONE/1.D0/                                                    CUB00230
                                                                        CUB00240
      A3=A/3.                                                           CUB00250
      AP=B-A*A3                                                         CUB00260
      BP=2.*A3**3-A3*B+C                                                CUB00270
      AP3=AP/3.                                                         CUB00280
      BP2=BP/2.                                                         CUB00290
      TROOT=BP2*BP2+AP3*AP3*AP3                                         CUB00300
      IF(TROOT.GT.0.0) THEN                                             CUB00310
         TR=DSQRT(TROOT)                                                CUB00320
         APP=(-BP2+TR)**0.333333                                        CUB00330
         BSV=-BP2-TR                                                    CUB00340
         IF(BSV.NE.0.0) THEN                                            CUB00350
            SGN=DSIGN(ONE,BSV)                                          CUB00360
            BPP=SGN*(DABS(BSV))**0.333333                               CUB00370
            Z=APP+BPP-A3                                                CUB00380
            RETURN                                                      CUB00390
         ENDIF                                                          CUB00400
C     BSV (& BPP) = 0.0                                                 CUB00410
         Z=APP-A3                                                       CUB00420
         RETURN                                                         CUB00430
      ENDIF                                                             CUB00440
                                                                        CUB00450
      CM=2.*DSQRT(-AP3)                                                 CUB00460
      ALPHA=DACOS(BP/(AP3*CM))/3.                                       CUB00470
      Z=CM*DCOS(ALPHA)-A3                                               CUB00480
                                                                        CUB00490
      RETURN                                                            CUB00500
      END                                                               CUB00510
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCDAY00010
      SUBROUTINE DAYLOOP(IEND,ALAT,ALONG,TZONE,IHRRIS,IHRSET,           DAY00020
     &                   NE,IHSTRT,DAY1,NHRS,JYR)                       DAY00030
C                                                                       DAY00040
C PURPOSE:  DAILY LOOP, READ MET DATA                                   DAY00050
C                                                                       DAY00060
C I/O:  IEND, FILE END INDICATOR                                        DAY00070
C       ALAT, LATITUDE POSITIVE NORTH OF EQUATOR  (DEGREES)             DAY00080
C      ALONG, LONGITUDE POSITIVE IN WESTERN HEMISPHERE (DEGREES)        DAY00090
C      TZONE, NUMBER OF HOURS LATER THAN GMT                            DAY00100
C     IHRRIS, SUNRISE (HOUR)                                            DAY00110
C     IHRSET, SUNSET (HOUR)                                             DAY00120
C         NE, ENDING INDEX                                              DAY00130
C    IHRSTRT, STARTING HOUR                                             DAY00140
C       DAY1, JULIAN DAY                                                DAY00150
C       NHRS, NUMBER OF HOURS                                           DAY00160
C        JYR, YEAR                                                      DAY00170
C                                                                       DAY00180
C CALLED BY:  MAIN                                                      DAY00190
C                                                                       DAY00200
C CALLS:  NONE                                                          DAY00210
C                                                                       DAY00220
C       MINERALS MANAGEMENT SERVICE                                     DAY00230
C       U.S. DEPARTMENT OF THE INTERIOR                                 DAY00240
C                                                                       DAY00250
C OCD             REVISION HISTORY:                                     DAY00260
C    DCD 880728   CREATED.                                              DAY00270
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCDAY00280
                                                                        DAY00290
      include 'params.cmn'
      INCLUDE 'store.cmn'                                               DAY00300
      INCLUDE 'count.cmn'                                               DAY00320
      INCLUDE 'opt.cmn'                                                 DAY00330
      INCLUDE 'const.cmn'                                               DAY00340
      INCLUDE 'linesrc.cmn'                                             DAY00350
                                                                        DAY00360
      DIMENSION MONTHS(12),HLH(2,24),dumr(24)                           DAY00370
      DATA MONTHS/ 31,28,31,30,31,30,31,31,30,31,30,31/                 DAY00380
                                                                        DAY00390
      DAY=IDAY                                                          DAY00400
      IF(IOPT(25).EQ.1) THEN                                            DAY00410
C                                                                       DAY00420
C       COMPUTE HOURS OF SUNRISE AND SUNSET                             DAY00430
C                                                                       DAY00440
C      TZONE = # HOURS LATER THAN GMT                                   DAY00450
C      ALAT IS POSITIVE NORTH OF THE EQUATOR; INPUT IN DEGREES          DAY00460
C      ALONG IS POSITIVE IN THE WESTERN HEMISPHERE                      DAY00470
C                                                                       DAY00480
C       COMPUTATIONS OF SUNRISE AND SUNSET ARE DERIVED FROM RAMMET, THE DAY00490
C       EPA METEOROLOGICAL PRE-PROCESSOR.                               DAY00500
C                                                                       DAY00510
         RLAT = ALAT*DG2RAD                                             DAY00520
         D = IDAY/365.242 * 360.                                        DAY00530
         DR = D*DG2RAD                                                  DAY00540
         DR2 = 2.0 * DR                                                 DAY00550
         SIGMA = D + 273.9348 + 1.914827*SIN(DR) - 0.079525*COS(DR) +   DAY00560
     &      0.019938*SIN(DR2) - 0.00162*COS(DR2)                        DAY00570
         AMM = 12.0 + 0.12357*SIN(DR) - 0.004289*COS(DR) +              DAY00580
     &      0.153809*SIN(DR2) + 0.060783*COS(DR2)                       DAY00590
C   0.39785=SIN(23.44383)                                               DAY00600
         DELTA = ASIN(0.39785*SIN(SIGMA*DG2RAD))                        DAY00610
         H2 = ACOS(-TAN(RLAT)*TAN(DELTA)) * RAD2DG                      DAY00620
         SUNRIS = AMM - (H2 - ALONG)/15. - TZONE                        DAY00630
         SUNSET = AMM + (H2 + ALONG)/15. - TZONE                        DAY00640
         IHRRIS = INT(SUNRIS + 0.5)                                     DAY00650
         IHRSET = INT(SUNSET + 0.5)                                     DAY00660
      ENDIF                                                             DAY00670
C                                                                       DAY00680
C       DETERMINE MONTH                                                 DAY00690
C                                                                       DAY00700
      MONTHS(2) = 28                                                    DAY00710
      IF(IDATE(1)/4*4.eq.IDATE(1)) MONTHS(2) = 29                       DAY00720
      ISUM = 0                                                          DAY00730
      DO 723 IMONTH = 1,12                                              DAY00740
         ISUM = ISUM + MONTHS(IMONTH)                                   DAY00750
         IF(IDAY.LE.ISUM) GO TO 725                                     DAY00760
723   CONTINUE                                                          DAY00770
725   NHRS=0                                                            DAY00780
      if (iopt(5).eq.0 .or. iopt(5).eq.2) then
c
c        If option 5 = 0, input binary met data off disk (unit 11)
c        If option 5 = 2, input ASCII  met data off disk (unit 11)
c
         if (iopt(5).eq.0) then          ! Binary PCRAMMET file
           read (11,end=791) jyr,imo,day1,ikst,qu,qtemp,dumr,qtheta,hlh
         else if (iopt(5).eq.2) then     ! ASCII  PCRAMMET file
           do j = 1,24
           read (11,'(4i2,2f9.4,f6.1,i2,2f7.1)',end=791) jyr,imo,mday,
     &           mhour,qtheta(j),qu(j),qtemp(j),ikst(j),
     &           hlh(1,j),hlh(2,j)
           end do
           call julday (jyr,imo,mday,julian)
           day1=float(julian)
         end if
c
         IF (JYR.NE.IDATE(1) .OR. DAY1.NE.DAY) THEN                     DAY00820
C        DATE ON MET FILE DOES NOT MATCH INTERNAL DATE                  DAY00830
            WRITE (IO,1370) JYR,IDATE(2),IDATE(1),IDAY                  DAY00840
            WRITE (ierr,1370) JYR,IDATE(2),IDATE(1),IDAY
            STOP 'Error encountered.  See ERROR.OUT for more details.'
         ENDIF                                                          DAY00860
C        MODIFY WIND VECTOR BY 180 DEGREES. SINCE FLOW VECTORS WERE     DAY00870
C        OUTPUT FROM RAMMET. THIS CONVERTS BACK TO WIND DIRECTIONS.     DAY00880
         IDATE(2)=DAY1                                                  DAY00890
         DO 750 IQ=1,24                                                 DAY00900
            IF (IKST(IQ).EQ.7) IKST(IQ)=6                               DAY00910
            QTHETA(IQ)=QTHETA(IQ)+180.                                  DAY00920
            IF (QTHETA(IQ).GT.360.) QTHETA(IQ)=QTHETA(IQ)-360.          DAY00930
C        RURAL MIXING HEIGHTS ARE USED FOR THIS APPLICATION             DAY00940
            QHL(IQ)=HLH(1,IQ)                                           DAY00950
750      CONTINUE                                                       DAY00960
      ENDIF                                                             DAY00970
      NB=IHSTRT                                                         DAY00980
      NE=NB+NAVG-1                                                      DAY00990
C   LINE SOURCE OPTION                                                  DAY01000
      IF(IOPT(20).EQ.2) NE=NSEGS                                        DAY01010
      IF (NB.LE.0) THEN                                                 DAY01020
         WRITE (IO,1380) IHSTRT                                         DAY01030
         WRITE (ierr,1380) IHSTRT
         STOP 'Error encountered.  See ERROR.OUT for more details.'
      ENDIF                                                             DAY01050
      RETURN                                                            DAY01060
                                                                        DAY01070
791   IEND=1                                                            DAY01080
                                                                        DAY01090
1370  FORMAT (' DATE ON MET. FILE, ',I2,I3,' ,DOES NOT MATCH INTERNAL', DAY01100
     & 'DATE, ',I2,I3)                                                  DAY01110
1380  FORMAT (' HOUR ',I3,' IS NOT PERMITTED. HOURS MUST BE DEFINED',   DAY01120
     & ' BETWEEN 1 AND 24')                                             DAY01130
                                                                        DAY01140
      RETURN                                                            DAY01150
      END                                                               DAY01160
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCDIS00010
      SUBROUTINE DIST( J,RQ,SQ,XD)                                      DIS00020
C                                                                       DIS00030
C PURPOSE:  CALCULATE DISTANCES                                         DIS00040
C                                                                       DIS00050
C I/O:  J, SOURCE INDEX                                                 DIS00060
C      RQ, X COORDINATE OF SOURCE                                       DIS00070
C      SQ, Y COORDINATE OF SOURCE                                       DIS00080
C      XD, DISTANCE FROM SOURCE TO RECEPTOR                             DIS00090
C                                                                       DIS00100
C CALLED BY: RECEP                                                      DIS00110
C                                                                       DIS00120
C CALLS:  PLUME                                                         DIS00130
C         GEOM                                                          DIS00140
C                                                                       DIS00150
C       MINERALS MANAGEMENT SERVICE                                     DIS00160
C       U.S. DEPARTMENT OF THE INTERIOR                                 DIS00170
C                                                                       DIS00180
C OCD             REVISION HISTORY:                                     DIS00190
C    DCD 880909   CREATED.                                              DIS00200
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCDIS00210
                                                                        DIS00220
      include 'params.cmn'
      INCLUDE 'met.cmn'                                                 DIS00240
      INCLUDE 'src.cmn'                                                 DIS00250
      INCLUDE 'shr.cmn'                                                 DIS00260
      INCLUDE 'store.cmn'                                               DIS00270
      INCLUDE 'ptrrcp.cmn'                                              DIS00280
      INCLUDE 'const.cmn'                                               DIS00290
                                                                        DIS00300
      REAL DL(10), DW(10)                                               DIS00310
      character*1 wls
                                                                        DIS00320
C       DETERMINE UPWIND DISTANCE                                       DIS00330
C        XDUM,YDUM IN USER UNITS. X,Y IN KM.                            DIS00340
C          RREC - EAST COORDINATE OF THE RECEPTOR                       DIS00350
      XDUM=RQ-RREC(K)                                                   DIS00360
                                                                        DIS00370
C          SREC - NORTH COORDINATE OF THE RECEPTOR                      DIS00380
      YDUM=SQ-SREC(K)                                                   DIS00390
                                                                        DIS00400
C        SINT AND COST ARE THE SIN AND COS OF THE WIND DIRECTION        DIS00410
C        CONTWO - MULTIPLIER CONSTANT TO CONVERT USER UNITS TO KM       DIS00420
      X=(YDUM*COST+XDUM*SINT)*CONTWO                                    DIS00430
                                                                        DIS00440
C         X IS THE UPWIND DISTANCE  OF THE SOURCE FROM THE RECEPTOR.    DIS00450
C        IF X IS NEGATIVE, INDICATING THAT THE SOURCE IS DOWNWIND OF    DIS00460
C        THE RECEPTOR, THE CALCULATION IS TERMINATED ASSUMING NO        DIS00470
C        CONTRIBUTION FROM THAT SOURCE.                                 DIS00480
      IF (X.LE.0.0) RETURN                                              DIS00490
                                                                        DIS00500
C   GEOM FINDS THE NEAREST SHORELINE ALONG THE PLUME TO THIS RECEPTOR.  DIS00510
C   CALCULATE ANGLE FROM SOURCE TO RECEPTOR                             DIS00520
      ANGR2S = 180./PI*ATAN2(XDUM,YDUM)                                 DIS00530
      IF(ANGR2S.LT.0.) ANGR2S=ANGR2S+360.                               DIS00540
      CALL PLUME(J,ANGR2S,DL,DW,NTW,NTL,ISKIP,WLS,io,ierr)              DIS00550
                                                                        DIS00560
C   USE XD = DISTANCE FROM SOURCE TO RECEPTOR                           DIS00570
      XD = SQRT( XDUM**2 + YDUM**2)*CONTWO                              DIS00580
      CALL GEOM(XD,RREC(K),SREC(K),DL,DW,NTL,NTW,WLS,J)                 DIS00590
                                                                        DIS00600
C   DETERMINE CROSSWIND DISTANCE                                        DIS00610
      Y=(YDUM*SINT-XDUM*COST)*CONTWO                                    DIS00620
                                                                        DIS00630
      RETURN                                                            DIS00640
      END                                                               DIS00650
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCDNW00010
      SUBROUTINE DOWNWASH(XD,HB,W,HE,SIGZ0,SIGY0)                       DNW00020
C                                                                       DNW00030
C PURPOSE:  COMPUTE PLATFORM DOWNWASH EFFECTS                           DNW00040
C           BASED ON PETERSON, 1986                                     DNW00050
C                                                                       DNW00060
C I/O:  XD  DOWNWIND DISTANCE (KM)                                      DNW00070
C       HB  BUILDING HEIGHT ABOVE WATER SURFACE (M)                     DNW00080
C        W  BUILDING WIDTH (M)                                          DNW00090
C       HE  EFFECTIVE STACK HEIGHT (M)                                  DNW00100
C    SIGZ0  INITIAL SIGMA-Z                                             DNW00110
C    SIGY0  INITIAL SIGMA-Y                                             DNW00120
C                                                                       DNW00130
C CALLED BY:  PTR                                                       DNW00140
C             CALC                                                      DNW00150
C                                                                       DNW00160
C CALLS:  NONE                                                          DNW00170
C                                                                       DNW00180
C       MINERALS MANAGEMENT SERVICE                                     DNW00190
C       U.S. DEPARTMENT OF THE INTERIOR                                 DNW00200
C                                                                       DNW00210
C OCD             REVISION HISTORY:                                     DNW00220
C    DCD 880906   CREATED.                                              DNW00230
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCDNW00240
                                                                        DNW00250
      INCLUDE 'const.cmn'                                               DNW00260
                                                                        DNW00270
      REAL LY, LZ                                                       DNW00280
                                                                        DNW00290
      AY = 1.9                                                          DNW00300
      BY = 48.2                                                         DNW00310
      AZ = 3.0                                                          DNW00320
      BZ = 40.2                                                         DNW00330
      CY = -1.4                                                         DNW00340
      CZ = -1.4                                                         DNW00350
      LY = W/2.                                                         DNW00360
      LZ = HB                                                           DNW00370
      CHK = (XD*KM2M)/HB                                                DNW00380
      IF( CHK .LT. 2.2) THEN                                            DNW00390
         X = 2.2*HB                                                     DNW00400
         SYOP = 0.071*X*SQRT(AY+(BY*(X/LY)**CY)-1.)                     DNW00410
C   3.915 = SQRT PORTION OF SZOP EQ. SOLVED USING X/LZ = 2.2            DNW00420
         SZOP = (.11*X**0.81)*3.915                                     DNW00430
                                                                        DNW00440
      ELSEIF( CHK .GT. 12.6) THEN                                       DNW00450
         X = 12.6*HB                                                    DNW00460
         SYOP = 0.071*X*SQRT(AY+(BY*(X/LY)**CY)-1.)                     DNW00470
C   3.915 = SQRT PORTION OF SZOP EQ. SOLVED USING X/LZ = 12.6           DNW00480
         SZOP = (.11*X**.81)*1.777                                      DNW00490
                                                                        DNW00500
      ELSE                                                              DNW00510
         SYOP = 0.071*XD*KM2M*SQRT(AY+(BY*(XD*KM2M/LY)**CY)-1.)         DNW00520
         SZOP = 0.11*((XD*KM2M)**0.81)*SQRT(AZ+(BZ*(XD*KM2M/LZ)**CZ)-1.)DNW00530
      ENDIF                                                             DNW00540
                                                                        DNW00550
      A = HE/HB                                                         DNW00560
      IF( A .GT. 3.0) THEN                                              DNW00570
         SIGY0 = 0.0                                                    DNW00580
         SIGZ0 = 0.0                                                    DNW00590
      ELSEIF( 1.2 .LT. A .AND. A .LE. 3.0) THEN                         DNW00600
         SIGY0 = 0.0                                                    DNW00610
         SIGZ0 = 0.5*(3.-A)*SZOP                                        DNW00620
      ELSEIF( 1.0 .LT. A .AND. A. LE. 1.2) THEN                         DNW00630
         SIGY0 = 0.5*(6.-5.*A)*SYOP                                     DNW00640
         SIGZ0 = 0.5*(3.-A)*SZOP                                        DNW00650
      ELSE                                                              DNW00660
         SIGY0 = SYOP                                                   DNW00670
         SIGZ0 = SZOP                                                   DNW00680
      ENDIF                                                             DNW00690
                                                                        DNW00700
      RETURN                                                            DNW00710
      END                                                               DNW00720
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCEMH00010
      SUBROUTINE EMISHT                                                 EMH00020
C                                                                       EMH00030
C PURPOSE:  PRINT EMISSIONS WITH HEIGHT TABLE                           EMH00040
C                                                                       EMH00050
C I/O:  NONE                                                            EMH00060
C                                                                       EMH00070
C CALLED BY:  READCF2                                                   EMH00080
C                                                                       EMH00090
C CALLS:  NONE                                                          EMH00100
C                                                                       EMH00110
C       MINERALS MANAGEMENT SERVICE                                     EMH00120
C       U.S. DEPARTMENT OF THE INTERIOR                                 EMH00130
C                                                                       EMH00140
C OCD             REVISION HISTORY:                                     EMH00150
C    DCD 880707   CREATED.                                              EMH00160
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCEMH00170
                                                                        EMH00180
      include 'params.cmn'
      INCLUDE 'store.cmn'                                               EMH00210
      INCLUDE 'src.cmn'                                                 EMH00220
      INCLUDE 'const.cmn'                                               EMH00230
                                                                        EMH00240
      IP = IPOL-2                                                       EMH00250
      DO 320 I=1,NPT                                                    EMH00260
         DO 290 J=1,20                                                  EMH00270
            HC=J*5.                                                     EMH00280
            IF (SOURCE(5,I).LE.HC) GO TO 300                            EMH00290
290      CONTINUE                                                       EMH00300
                                                                        EMH00310
C        POINT SOURCES WITH PHYSICAL HEIGHTS GT 100 METERS ARE LISTED   EMH00320
C        SEPARATELY.                                                    EMH00330
         WRITE (IO,1540) I,SOURCE(5,I),SOURCE(3,I)                      EMH00340
         GO TO 310                                                      EMH00350
                                                                        EMH00360
C        ADD EMISSION RATE INTO TABLE AND TOTAL.                        EMH00370
300      TABLE(1,J)=TABLE(1,J)+SOURCE(3,I)                              EMH00380
310      TABLE(1,21)=TABLE(1,21)+SOURCE(3,I)                            EMH00390
                                                                        EMH00400
320   CONTINUE                                                          EMH00410
                                                                        EMH00420
C        OUTPUT SOURCE-STRENGTH-HEIGHT TABLE                            EMH00430
C        THIS TABLE DISPLAYS THE TOTAL EMISSIONS FOR POINT              EMH00440
C        SOURCES AND THE CUMULATIVE FREQUENCY ACCORDING TO              EMH00450
C        HEIGHT CLASS                                                   EMH00460
      WRITE (IO,1550) TITLE(IP)                                         EMH00470
C        HEIGHT CLASS EMISSIONS ARE IN 1                                EMH00480
C        DETERMINE CUMULATIVE PERCENT IN 2                              EMH00490
      IH1=0                                                             EMH00500
      IH2=5                                                             EMH00510
      IM1=1                                                             EMH00520
      TABLE(2,1)=TABLE(1,1)/TABLE(1,21)                                 EMH00530
      WRITE (IO,1560) IH1,IH2,(TABLE(J,1),J=1,2)                        EMH00540
      DO 330 I=2,20                                                     EMH00550
         IH2=I*5                                                        EMH00560
         IH1=IH2-4                                                      EMH00570
         IM1=I-1                                                        EMH00580
         TABLE(2,I)=TABLE(1,I)/TABLE(1,21)+TABLE(2,IM1)                 EMH00590
         WRITE (IO,1560) IH1,IH2,(TABLE(J,I),J=1,2)                     EMH00600
330   CONTINUE                                                          EMH00610
      WRITE (IO,1570) TABLE(1,21)                                       EMH00620
                                                                        EMH00630
1540  FORMAT (1X,'HEIGHT ABOVE 100M FOR POINT SOURCE',I4,3X,' HEIGHT=',FEMH00640
     &  6.2,' (METERS)','   EMISSIONS=',F10.2,' (G/SEC)')               EMH00650
1550  FORMAT ( / ,4X,'TOTAL ',A4,' EMISSION AND CUMULATIVE FRACTION ACCOEMH00660
     &RDING TO HEIGHT'//1X,T12,'TOTAL POINT   CUMULATIVE '/1X,'HEIGHT(M)EMH00670
     & EMISSIONS(G/S)  FRACTION'/1X)                                    EMH00680
1560  FORMAT (1X,T2,I2,' -',I3,T11,F8.2,T26,F7.3,T41,F8.2,T56,F7.3)     EMH00690
1570  FORMAT ( / ,T2,'TOTAL',2X,F10.2)                                  EMH00700
                                                                        EMH00710
      RETURN                                                            EMH00720
      END                                                               EMH00730
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCFIL00010
      SUBROUTINE FILPOS                                                 FIL00020
C                                                                       FIL00030
C PURPOSE:  POSITION INPUT FILES AS REQUIRED                            FIL00040
C                                                                       FIL00050
C I/O:  NONE                                                            FIL00060
C                                                                       FIL00070
C CALLED BY:  MAIN                                                      FIL00080
C                                                                       FIL00090
C CALLS:  NONE                                                          FIL00100
C                                                                       FIL00110
C       MINERALS MANAGEMENT SERVICE                                     FIL00120
C       U.S. DEPARTMENT OF THE INTERIOR                                 FIL00130
C                                                                       FIL00140
C OCD             REVISION HISTORY:                                     FIL00150
C    DCD 880801   CREATED.                                              FIL00160
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCFIL00170
                                                                        FIL00180
      DIMENSION DUMR(24), HLH(2,24)                                     FIL00190
                                                                        FIL00200
      include 'params.cmn'
      INCLUDE 'count.cmn'                                               FIL00210
      INCLUDE 'opt.cmn'                                                 FIL00220
      INCLUDE 'store.cmn'                                               FIL00240
      character*1 junk
                                                                        FIL00250
C       IF INPUT LAND METEOROLOGY IS READ FROM A BINARY DISK FILE,
C       OR AN ASCII DISK FILE,
C       AND THE RUN DOES NOT BEGIN ON THE FIRST DAY, THEN RECORDS ARE   FIL00270
C       SKIPPED IN THE FOLLOWING DATA INPUT FILES:  LAND METEOROLOGY,   FIL00280
C       OVERWATER METEOROLOGY, AND HOURLY EMISSIONS.  IF INPUT LAND     FIL00290
C       METEOROLOGY IS READ FROM CARDS, THEN ALL OF THE ABOVE FILES     FIL00300
C       ARE ASSUMED TO START AT THE SAME TIME, AND NO RECORDS ARE       FIL00310
C       SKIPPED.                                                        FIL00320
C                                                                       FIL00330
                                                                        FIL00340
C        SKIP PREVIOUSLY USED HOURLY METEOROLOGY AND EMISSION RECORDS.  FIL00350
      DO 715 I=1,IDAY                                                   FIL00360
         DO 705 J = 1,24                                                FIL00370
            READ(13,1190) JUNK                                          FIL00380
705      CONTINUE                                                       FIL00390
         IF(IOPT(6).EQ.1) THEN                                          FIL00400
            DO 711 J = 1,24                                             FIL00410
               DO 710 K = 1,NPT                                         FIL00420
                  READ(15,1190) JUNK                                    FIL00430
710            CONTINUE                                                 FIL00440
711         CONTINUE                                                    FIL00450
         ENDIF                                                          FIL00460
         if (iopt(5).eq.0) then          ! Binary PCRAMMET file
           read (11) jyr,imo,day1,ikst,qu,qtemp,dumr,qtheta,hlh
         else if (iopt(5).eq.2) then     ! ASCII  PCRAMMET file
           do j = 1,24
           read (11,'(4i2,2f9.4,f6.1,i2,2f7.1)') jyr,imo,mday,
     &           mhour,qtheta(j),qu(j),qtemp(j),ikst(j),
     &           hlh(1,j),hlh(2,j)
           end do
           call julday (jyr,imo,mday,julian)
           day1=float(julian)
         end if
715   CONTINUE                                                          FIL00480
                                                                        FIL00490
1190  FORMAT(A1)                                                        FIL00500
                                                                        FIL00510
      RETURN                                                            FIL00520
      END                                                               FIL00530
c-----------------------------------------------------------------------
c subroutine: fopen
c
c purpose:  A file-open utility program
c
c Author:
c -------
c   Joseph C. Chang
c   EARTH TECH
c   196 Baker Avenue
c   Concord, MA 01742
c   Tel (508) 371-4256       Fax (508) 371-4280
c   E-mail chang@src.com
c
c   Developed for:
c   Minerals Management Service
c   U.S. Department of the Interior
c
c arguments passed:
c    variable   type    description
c      iunit    int     logical unit number of the file to be opened
c      cname    char    name of the file to be opened
c      cstatus  char    status of the file to be opened,
c                       'new', 'old', or 'unknown'
c      cform    char    form of the file to be opened,
c                       'formatted' or 'unformatted'
c
c calling routines: analysis
c
c-----------------------------------------------------------------------
c
      subroutine fopen (iunit,cname,cstatus,cform)
      character*(*) cname,cstatus,cform
      integer       iunit
      logical       lflag
      character*1   ians
c
c Check the validity of cstatus and cform
c
        if (cstatus.ne.'NEW' .and. cstatus.ne.'OLD' .and.
     1      cstatus.ne.'new' .and. cstatus.ne.'old' .and.
     2      cstatus.ne.'unknown' .and. cstatus.ne.'UNKNOWN') then
          print *,'Error in fopen: cstatus = ',cstatus,' not recognized'
          stop
        end if
c
        if (cform.ne.'FORMATTED' .and. cform.ne.'UNFORMATTED' .and.
     1      cform.ne.'formatted' .and. cform.ne.'unformatted') then
          print *,'Error in fopen: cform = ',cform,' not recognized'
          stop
        end if
c
        if (cstatus.eq.'NEW' .or. cstatus.eq.'new') then
c
c  Check whether the file already exists or not
          inquire (file=cname,exist=lflag)
          if (.not.lflag) then
            open (unit=iunit,file=cname,status=cstatus,form=cform)
          else
            print *,'File ',cname,' already exists, overwrite it ? (y/n,
     1 default=y)  '
            read 11,ians
            if (ians.eq.'N' .or. ians.eq.'n') stop
            open (unit=iunit,file=cname,status='unknown',form=cform)
          end if
c
        else if (cstatus.eq.'OLD' .or. cstatus.eq.'old') then
c
c  Check whether the file already exists or not
          inquire (file=cname,exist=lflag)
          if (lflag) then
            open (unit=iunit,file=cname,status=cstatus,form=cform)
          else
            print *,'File ',cname,' does not exist!'
            stop
          end if
c
        else if (cstatus.eq.'UNKNOWN' .or. cstatus.eq.'unknown') then
c
          open (unit=iunit,file=cname,status=cstatus,form=cform)
c
        end if
c
11      format(a1)
        return
        end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCGEO00010
      SUBROUTINE GEOM(XD,XREC,YREC,DL,DW,NTL,NTW,WLS,JSRC)              GEO00020
C                                                                       GEO00030
C PURPOSE:  DETERMINE THE DISTANCE FROM THE SOURCE TO THE SHORELINE     GEO00040
C           ALONG ANGR2S TO THE MODELED RECEPTOR                        GEO00050
C                                                                       GEO00060
C I/O:  XD      DISTANCE FROM SOURCE TO RECEPTOR (KM)                   GEO00070
C       XREC    X COORDINATE OF RECEPTOR                                GEO00080
C       YREC    Y COORDINATE OF RECEPTOR                                GEO00090
C       DL      DISTANCE TO LAND TO WATER TRANSITIONS                   GEO00100
C       DW      DISTANCE TO WATER TO LAND TRANSITIONS                   GEO00110
C       NTL     NUMBER OF LAND TO WATER TRANSITIONS                     GEO00120
C       NTW     NUMBER OF WATER TO LAND TRANSITIONS                     GEO00130
C       WLS     LAND OR WATER AT SOURCE                                 GEO00140
C       JSRC    SOURCE INDEX                                            GEO00150
C                                                                       GEO00160
C CALLED BY:  DIST                                                      GEO00170
C                                                                       GEO00180
C CALLS:  NONE                                                          GEO00190
C                                                                       GEO00200
C       MINERALS MANAGEMENT SERVICE                                     GEO00210
C       U.S. DEPARTMENT OF THE INTERIOR                                 GEO00220
C                                                                       GEO00230
C OCD             REVISION HISTORY:                                     GEO00240
C    DCD 880916   CREATED.                                              GEO00250
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCGEO00260
                                                                        GEO00270
      include 'params.cmn'
      INCLUDE 'met.cmn'                                                 GEO00280
      INCLUDE 'ptrrcp.cmn'                                              GEO00290
      INCLUDE 'src.cmn'                                                 GEO00310
      INCLUDE 'shr.cmn'                                                 GEO00320
      INCLUDE 'store.cmn'                                               GEO00330
      INCLUDE 'const.cmn'                                               GEO00340
                                                                        GEO00350
      REAL DL(10),DW(10)                                                GEO00360
      character*1 wls,rec
                                                                        GEO00370
C                                                                       GEO00380
C  IF NO SHORELINE ALONG PLUME PATH DETERMINE IF PLUME IS OVER LAND OR  GEO00390
C  WATER                                                                GEO00400
      IF(NTW.GT.0.OR.NTL.GT.0) GO TO 10                                 GEO00410
      REC=WLS                                                           GEO00420
      GO TO 150                                                         GEO00430
                                                                        GEO00440
C  DETERMINE IF PLUME-RECEPTOR INTERSECTION IS OVER LAND OR WATER       GEO00450
10    DELX0 = XREC - X0                                                 GEO00460
      DELY0 = Y0 - YREC                                                 GEO00470
      LX = INT(DELX0/DELX + 0.9999)                                     GEO00480
      LY = INT(DELY0/DELY + 0.9999)                                     GEO00490
C  CHECK IF INTERSECTION IS ON GRID                                     GEO00500
      IF(LX.LE.NX.AND.LX.GE.1.AND.LY.LE.NY.AND.LY.GE.1) GO TO 100       GEO00510
C  IF OFF THE, GRID ASSUME CONTINUITY FROM WHERE THE PLUME LEAVES       GEO00520
C  OR ENTERS THE GRID                                                   GEO00530
      IF(NTW.EQ.0) GO TO 50                                             GEO00540
      IF(NTL.EQ.0) GO TO 20                                             GEO00550
      IF(DL(NTL).GT.DW(NTW)) GO TO 50                                   GEO00560
20    IF(XD/CONTWO.LT.DW(NTW)) GO TO 30                                 GEO00570
      REC=LETL                                                          GEO00580
      GO TO 150                                                         GEO00590
30    REC=WLS                                                           GEO00600
      GO TO 150                                                         GEO00610
C                                                                       GEO00620
50    IF(XD/CONTWO.LT.DL(NTL)) GO TO 70                                 GEO00630
      REC=LETW                                                          GEO00640
      GO TO 150                                                         GEO00650
70    REC=WLS                                                           GEO00660
      GO TO 150                                                         GEO00670
C                                                                       GEO00680
100   REC=XYMAP(LY,LX)                                                  GEO00690
C                                                                       GEO00700
C     IF(ILH.EQ.NBB .AND. JSRC.EQ.1 .AND. REC.EQ.LETW) WRITE(IO,1000) K GEO00710
150   IF(REC.EQ.LETL) GO TO 350                                         GEO00720
C                                                                       GEO00730
C  LAND TO WATER                                                        GEO00740
      DO 200 I=1,NTL                                                    GEO00750
         IF(DL(NTL+1-I).LT.XD/CONTWO) GO TO 300                         GEO00760
200   CONTINUE                                                          GEO00770
      GO TO 900                                                         GEO00780
C                                                                       GEO00790
C  CHECK DISTANCE TO NEAREST UPWIND LAND                                GEO00800
C  IF LESS THAN USER SPECIFIED MINIMUM                                  GEO00810
C  TREAT RECEPTOR AS IF IT WERE ON LAND                                 GEO00820
C                                                                       GEO00830
300   DD=XD/CONTWO-DL(NTL+1-I)                                          GEO00840
      IF(DD.GT.WMIN) GO TO 900                                          GEO00850
C                                                                       GEO00860
C  WATER TO LAND                                                        GEO00870
350   DO 400 J=1,NTW                                                    GEO00880
         IF(DW(NTW+1-J).LT.XD/CONTWO) GO TO 500                         GEO00890
400   CONTINUE                                                          GEO00900
      IF(WLS.EQ.LETW) GO TO 900                                         GEO00910
      XLAND=0.0                                                         GEO00920
      RETURN                                                            GEO00930
C                                                                       GEO00940
C  DISTANCE TO CLOSEST UPWIND SHORELINE                                 GEO00950
500   XLAND=DW(NTW+1-J)*CONTWO                                          GEO00960
      RETURN                                                            GEO00970
900   XLAND=XD*10.                                                      GEO00980
                                                                        GEO00990
      RETURN                                                            GEO01000
                                                                        GEO01010
1000  FORMAT(/,1X,'CAUTION:  RECEPTOR NUMBER: ',I3,/,1X,                GEO01020
     &  'OVER WATER RECEPTOR INPUT TO OCD',/,1X,                        GEO01030
     &  'USER IS ADVISED TO VERIFY THAT THIS IS NOT AN INTENDED ',      GEO01040
     &  'OVER LAND RECEPTOR',//)                                        GEO01050
                                                                        GEO01060
      END                                                               GEO01070
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCHIF00010
      SUBROUTINE HIFIVE(NAVT,KMX)                                       HIF00020
C                                                                       HIF00030
C PURPOSE:  OUTPUT HIGH FIVE TABLE FOR 4 OR 5 AVERAGING PERIODS         HIF00040
C                                                                       HIF00050
C I/O:   NAVT, ADDITIONAL AVERAGING TIME FOR HIGH-5 TABLE               HIF00060
C        KMX, MAXIMUM CONCENTRATION INDEX                               HIF00070
C                                                                       HIF00080
C CALLED BY:  AVCON                                                     HIF00090
C                                                                       HIF00100
C CALLS:  NONE                                                          HIF00110
C                                                                       HIF00120
C       MINERALS MANAGEMENT SERVICE                                     HIF00130
C       U.S. DEPARTMENT OF THE INTERIOR                                 HIF00140
C                                                                       HIF00150
C OCD             REVISION HISTORY:                                     HIF00160
C    DCD 880906   CREATED.                                              HIF00170
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCHIF00180
                                                                        HIF00190
      include 'params.cmn'
      INCLUDE 'store.cmn'                                               HIF00220
      INCLUDE 'const.cmn'                                               HIF00230
                                                                        HIF00240
                                                                        HIF00270
      IP = IPOL - 2                                                     HIF00280
      STAR(1,KMX)=BLNK                                                  HIF00290
                                                                        HIF00300
      DO 1130 L=1,NAVT                                                  HIF00310
C        ASTERISKS DEPICT RECEPTORS WITH HIGHEST AND                    HIF00320
C         SECOND HIGHEST CONCENTRATIONS.                                HIF00330
         K1=1                                                           HIF00340
         K2=1                                                           HIF00350
         HI1=HMAXA(1,1,L)                                               HIF00360
         HI2=HMAXA(2,1,L)                                               HIF00370
         DO 1100 K=2,NRECEP                                             HIF00380
            IF (HMAXA(1,K,L).GT.HI1) THEN                               HIF00390
               HI1=HMAXA(1,K,L)                                         HIF00400
               K1=K                                                     HIF00410
            ENDIF                                                       HIF00420
            IF (HMAXA(2,K,L).GT.HI2) THEN                               HIF00430
               HI2=HMAXA(2,K,L)                                         HIF00440
               K2=K                                                     HIF00450
            ENDIF                                                       HIF00460
1100     CONTINUE                                                       HIF00470
         STAR(1,K1)=STR                                                 HIF00480
         STAR(2,K2)=STR                                                 HIF00490
         WRITE (IO,1760) NTIME(L),TITLE(IP),(I,I=1,5)                   HIF00500
         DMY=ATIME(L)                                                   HIF00510
         DO 1120 K=1,NRECEP                                             HIF00520
            DO 1110 J=1,5                                               HIF00530
               HMAXA(J,K,L)=HMAXA(J,K,L)/DMY                            HIF00540
1110        CONTINUE                                                    HIF00550
            WRITE (IO,1770) K,RREC(K),SREC(K),(STAR(J,K),HMAXA(J,K,L),  HIF00560
     &        NDAY(J,K,L),IHRARRAY(J,K,L),J=1,2),(HMAXA(J,K,L),         HIF00570
     &        NDAY(J,K,L),IHRARRAY(J,K,L),J=3,5)                        HIF00580
1120     CONTINUE                                                       HIF00590
                                                                        HIF00600
C        INITIALIZE ASTERISK STORAGE TO BLANKS.                         HIF00610
         STAR(1,K1)=BLNK                                                HIF00620
         STAR(2,K2)=BLNK                                                HIF00630
1130  CONTINUE                                                          HIF00640
                                                                        HIF00650
1760  FORMAT ( / ,T41,'FIVE HIGHEST ',I2,'-HOUR ',A4,' CONCENTRATIONS((EHIF00660
     &NDING ON JULIAN DAY, HOUR)'/1X,T55,'(MICROGRAMS/M**3)'//2X,'RECEPTHIF00670
     &OR ',T38,4(I1,20X),I1,/1X)                                        HIF00680
1770  FORMAT (' ',2X,I3,'(',F7.2,',',F7.2,')',2(1X,A1,6PF9.2,2X,'(',I3,'HIF00690
     &,',I2,')'),3(2X,6PF9.2,2X,'(',I3,',',I2,')'))                     HIF00700
                                                                        HIF00710
      RETURN                                                            HIF00720
      END                                                               HIF00730
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCHCN00010
      SUBROUTINE HRCNTR(IPOLU)                                          HCN00020
C                                                                       HCN00030
C PURPOSE:  OUTPUT HOURLY CONTRIBUTIONS                                 HCN00040
C                                                                       HCN00050
C I/O:  IPOLU, POLLUTANT LABEL                                          HCN00060
C                                                                       HCN00070
C CALLED BY: OUTHR                                                      HCN00080
C                                                                       HCN00090
C CALLS:  NONE                                                          HCN00100
C                                                                       HCN00110
C       MINERALS MANAGEMENT SERVICE                                     HCN00120
C       U.S. DEPARTMENT OF THE INTERIOR                                 HCN00130
C                                                                       HCN00140
C OCD             REVISION HISTORY:                                     HCN00150
C    DCD 881026   CREATED.                                              HCN00160
C    JCC 920819   UPDATED.                                              XXX00160
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCHCN00170
                                                                        HCN00180
      include 'params.cmn'
      INCLUDE 'store.cmn'                                               HCN00200
      INCLUDE 'const.cmn'                                               XXX00200
      character*4 ipolu
                                                                        HCN00210
C  WRITE HRLY SIGNIGICANT SOURCE CONTRIBUTIONS                          HCN00220
      WRITE (IO,350) pb,LINE1,LINE2,LINE3
      WRITE (IO,360) IPOLU,IDATE,LH
      IF (NSIGP.LE.10) THEN                                             HCN00230
C        PRINT FIRST PAGE OF OUTPUT AND TOTALS FOR 10 OR LESS           HCN00240
C        SIGNIFICANT SOURCES                                            HCN00250
         WRITE (IO,370)                                                 HCN00260
         WRITE (IO,380) (I,I=1,NSIGP)                                   HCN00270
         WRITE (IO,390)                                                 HCN00280
         WRITE (IO,380) (MPS(I),I=1,NSIGP)                              HCN00290
         WRITE (IO,400)                                                 HCN00300
         DO 30 K=1,NRECEP                                               HCN00310
            WRITE (IO,410) K,STAR(1,K),STAR(2,K),(PHSIGS(K,I),I=1,NSIGP)HCN00320
C        PRINT TOTALS                                                   HCN00330
            WRITE (IO,420) PHSIGS(K,26),PHCHI(K)                        HCN00340
30       CONTINUE                                                       HCN00350
         RETURN                                                         HCN00360
      ENDIF                                                             HCN00370
C        PRINT FIRST PAGE FOR MORE THAN 10 SIGNIFICANT SOURCES.         HCN00380
      WRITE (IO,370)                                                    HCN00390
      WRITE (IO,380) (I,I=1,10)                                         HCN00400
      WRITE (IO,430) (MPS(I),I=1,10)                                    HCN00410
      WRITE (IO,400)                                                    HCN00420
      DO 50 K=1,NRECEP                                                  HCN00430
         WRITE (IO,410) K,STAR(1,K),STAR(2,K),(PHSIGS(K,I),I=1,10)      HCN00440
50    CONTINUE                                                          HCN00450
      IF (NSIGP.LE.20) THEN                                             HCN00460
C        PRINT SECOND PAGE AND TOTALS FOR 11 TO 20 SIGNIFICANT SOURCES  HCN00470
         WRITE (IO,350) pb,LINE1,LINE2,LINE3                            HCN00480
         WRITE (IO,360) IPOLU,IDATE,LH                                  HCN00490
         WRITE (IO,370)                                                 HCN00500
         WRITE (IO,380) (I,I=11,NSIGP)                                  HCN00510
         WRITE (IO,390)                                                 HCN00520
         WRITE (IO,380) (MPS(I),I=11,NSIGP)                             HCN00530
         WRITE (IO,400)                                                 HCN00540
         DO 60 K=1,NRECEP                                               HCN00550
            WRITE (IO,410) K,STAR(1,K),STAR(2,K),                       HCN00560
     &                     (PHSIGS(K,I),I=11,NSIGP)                     HCN00570
            WRITE (IO,420) PHSIGS(K,26),PHCHI(K)                        HCN00580
60       CONTINUE                                                       HCN00590
         RETURN                                                         HCN00600
      ENDIF                                                             HCN00610
                                                                        HCN00620
C        WRITE SECOND PAGE FOR MORE THAN 20 SIGNIFICANT SOURCES.        HCN00630
      WRITE (IO,350) pb,LINE1,LINE2,LINE3                               HCN00640
      WRITE (IO,360) IPOLU,IDATE,LH                                     HCN00650
      WRITE (IO,370)                                                    HCN00660
      WRITE (IO,380) (I,I=11,20)                                        HCN00670
      WRITE (IO,430) (MPS(I),I=11,20)                                   HCN00680
      WRITE (IO,400)                                                    HCN00690
      DO 80 K=1,NRECEP                                                  HCN00700
         WRITE (IO,410) K,STAR(1,K),STAR(2,K),(PHSIGS(K,I),I=11,20)     HCN00710
80    CONTINUE                                                          HCN00720
      WRITE (IO,350) pb,LINE1,LINE2,LINE3                               HCN00730
      WRITE (IO,360) IPOLU,IDATE,LH                                     HCN00740
      WRITE (IO,370)                                                    HCN00750
C        WRITE LAST PAGE AND TOTALS FOR MORE THAN 20 SIGNIF. SOURCES.   HCN00760
      WRITE (IO,380) (I,I=21,NSIGP)                                     HCN00770
      WRITE (IO,390)                                                    HCN00780
      WRITE (IO,380) (MPS(I),I=21,NSIGP)                                HCN00790
      WRITE (IO,400)                                                    HCN00800
      DO 90 K=1,NRECEP                                                  HCN00810
         WRITE (IO,410) K,STAR(1,K),STAR(2,K),(PHSIGS(K,I),I=21,NSIGP)  HCN00820
         WRITE (IO,420) PHSIGS(K,26),PHCHI(K)                           HCN00830
90    CONTINUE                                                          HCN00840
      RETURN                                                            HCN00850
                                                                        HCN00860
350   FORMAT ( a1,A80/1X,A80/1X,A80)                                    HCN00870
360   FORMAT( /,T30,A4,' CONTRIBUTION (MICROGRAMS/M**3) FROM SIGNIFICANTHCN00880
     & POINT SOURCES ',5X,I2,'/',I4,' : HOUR ',I2//)                    HCN00890
370   FORMAT ( / ,T5,'RANK')                                            HCN00900
380   FORMAT (1X,'+',T12,10(I3,7X))                                     HCN00910
390   FORMAT (1X,'+',T113,'TOTAL     TOTAL'/1X,T113,'SIGNIF    ALL ',   HCN00920
     & 'POINT',/,1X,T113,'POINT     SOURCES'/1X,'SOURCE #')             HCN00930
400   FORMAT (1X,'RECEP #')                                             HCN00940
410   FORMAT (1X,I3,2A1,6P10F10.3)                                      HCN00950
420   FORMAT (1X,'+',T109,6P2F10.3)                                     HCN00960
430   FORMAT (1X,'SOURCE #',T12,10(I3,7X))                              HCN00970
                                                                        HCN00980
      END                                                               HCN00990
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCHRC00010
      SUBROUTINE HRCON(DAY1,LLAND,IHRRIS,IHRSET,NAVT,NAV5,NPER)         HRC00020
C                                                                       HRC00030
C PURPOSE:  COMPUTE HOURLY CONCENTRATIONS                               HRC00040
C                                                                       HRC00050
C I/O: DAY1, STARTING JULIAN DAY                                        HRC00060
C     LLAND, CONSTANT VALUES OF MONIN OBUKHOV LENGTH OVER LAND          HRC00070
C    IHRRIS, HOUR OF SUNRISE                                            HRC00080
C    IHRSET, HOUR OF SUNSET                                             HRC00090
C      NAVT, NUMBER OF AVERAGING TIMES                                  HRC00100
C      NAV5, ADDITIONAL AVERAGING TIME FOR HIGH-FIVE TABLE              HRC00110
C      NPER, NUMBER OF AVERAGING PERIODS                                HRC00120
C                                                                       HRC00130
C                                                                       HRC00140
C CALLED BY:  MAIN                                                      HRC00150
C                                                                       HRC00160
C CALLS:  PTR                                                           HRC00170
C         WRCONC                                                        HRC00180
C         RANK                                                          HRC00190
C         OUTHR                                                         HRC00200
C                                                                       HRC00210
C       MINERALS MANAGEMENT SERVICE                                     HRC00220
C       U.S. DEPARTMENT OF THE INTERIOR                                 HRC00230
C                                                                       HRC00240
C OCD             REVISION HISTORY:                                     HRC00250
C    DCD 880714   CREATED.                                              HRC00260
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCHRC00270
                                                                        HRC00280
      include 'params.cmn'
      INCLUDE 'met.cmn'                                                 HRC00310
      INCLUDE 'src.cmn'                                                 HRC00320
      INCLUDE 'opt.cmn'                                                 HRC00330
      INCLUDE 'store.cmn'                                               HRC00350
      INCLUDE 'count.cmn'                                               HRC00360
      INCLUDE 'const.cmn'                                               HRC00370
                                                                        HRC00380
      REAL LLAND(24)                                                    HRC00390
      INTEGER Z0CL
      real rmet(7)
                                                                        HRC00410
      LH=ILH                                                            HRC00420
      IF (LH.GT.24) THEN                                                HRC00430
         LH=MOD(ILH,24)                                                 HRC00440
         IF (LH.EQ.1) IDATE(2)=DAY1                                     HRC00450
      ENDIF                                                             HRC00460
                                                                        HRC00470
C        INITIALIZE SUMS FOR CONC AND PARTIAL CONC FOR HOURLY PERIODS.  HRC00480
      DO 930 K=1,NRECEP                                                 HRC00490
         PHCHI(K)=0.0                                                   HRC00500
         DO 920 I=1,26                                                  HRC00510
            PHSIGS(K,I)=0.0                                             HRC00520
920      CONTINUE                                                       HRC00530
930   CONTINUE                                                          HRC00540
                                                                        HRC00550
C        SET MET CONDITIONS FOR THIS HOUR                               HRC00560
      THETA=QTHETA(LH)                                                  HRC00570
      U=WU(LH)                                                          HRC00580
      HL=WHL(LH)                                                        HRC00590
      HLAND=QHL(LH)                                                     HRC00600
      TEMP=WTAIR(LH)                                                    HRC00610
      KST=IKST(LH)                                                      HRC00620
      TAMTS = QTEMP(LH) - WTAIR(LH)                                     HRC00630
      TRAD=THETA*DG2RAD                                                 HRC00640
      SINT=SIN(TRAD)                                                    HRC00650
      COST=COS(TRAD)                                                    HRC00660
C                                                                       HRC00670
C        ESTIMATE MONIN-OBUKHOV LENGTH (L) OVER LAND                    HRC00680
C                                                                       HRC00690
      IF(Z0L.GT.0.3) Z0CL = 4                                           HRC00700
      IF(Z0L.LE.0.3.AND.Z0L.GT.0.03) Z0CL = 3                           HRC00710
      IF(Z0L.LE.0.03.AND.Z0L.GT.0.003) Z0CL = 2                         HRC00720
      IF(Z0L.LE.0.003) Z0CL = 1                                         HRC00730
      ELLAND = LLAND((KST-1)*4 + Z0CL)                                  HRC00740
C                                                                       HRC00750
C   COMPUTE FRICTION VELOCITY (U*) OVER LAND                            HRC00760
      ZDL=HANE/ELLAND                                                   HRC00770
      IF(ZDL.LT.0.0) THEN                                               HRC00780
         PX=(1.0-15.0*ZDL)**0.25                                        HRC00790
         PSI=2.0*ALOG(0.5*(1.0+PX))+ALOG(0.5*(1.0+PX*PX))-2.0*ATAN(PX)  HRC00800
     &       +1.5707963                                                 HRC00810
      ELSE                                                              HRC00820
         PSI= -4.7*ZDL                                                  HRC00830
      ENDIF                                                             HRC00840
      USTARL = 0.4 * U/(ALOG(HANE/Z0L)-PSI)                             HRC00850
C                                                                       HRC00860
C      COMPUTE CHEMICAL TRANSFORMATION RATE FOR THIS HOUR, IF APPLICABLEHRC00870
C                                                                       HRC00880
      IF(IOPT(25).EQ.1) THEN                                            HRC00890
         RATE = 0.0                                                     HRC00900
         IF(LH.GT.IHRRIS.AND.LH.LT.IHRSET) RATE = DECAY(IMONTH)         HRC00910
      ENDIF                                                             HRC00920
C                                                                       HRC00930
C        IF OPTION 6 IS 1, READ HOURLY EMISSIONS.                       HRC00940
      IF (IOPT(6).EQ.1) THEN                                            HRC00950
C                                                                       HRC00960
C       READ SPECIAL FORMATTED FILE FOR HOURLY EMISSIONS                HRC00970
C       CHECK AGREEMENT BETWEEN YEAR, DAY, AND HOUR                     HRC00980
C                                                                       HRC00990
          DO 935 K = 1,NPT                                              HRC01000
             READ(15,*) LYR,LDAY,LHR,SOURCE(3,K),SOURCE(8,K),           HRC01010
     &                     SOURCE(6,K)                                  HRC01020
935       CONTINUE                                                      HRC01030
                                                                        HRC01040
C        CHECK DATE                                                     HRC01050
         IF (IDATE(1).NE.LYR.OR.IDATE(2).NE.LDAY.OR.LH.NE.LHR) THEN     HRC01060
            WRITE (IO,1390) IDATE(1),IDATE(2),LH,LYR,LDAY,LHR           HRC01070
            WRITE (ierr,1390) IDATE(1),IDATE(2),LH,LYR,LDAY,LHR
            STOP 'Error encountered.  See ERROR.OUT for more details.'
         ENDIF                                                          HRC01090
      ENDIF                                                             HRC01100
                                                                        HRC01110
C        CALCULATE POINT SOURCE CONTRIBUTIONS                           HRC01120
      CALL PTR(NPER)                                                    HRC01130
      IF (IOPT(22).EQ.1) THEN                                           HRC01140
C        WRITE HOURLY CONCENTRATIONS TO TAPE OR DISK                    HRC01150
C                                                                       HRC01160
C       WRITE CONCENTRATIONS IN A SEQUENCE COMPATIBLE WITH THE          HRC01170
C       'ANALYSIS' POSTPROCESSOR.                                       HRC01180
C                                                                       HRC01190
           rmet(1)=hl        ! overwater mixing height
           rmet(2)=theta     ! wind direction (OCD keeps track of
c                            ! only either overwater or overland wind
c                            ! direction depending on JOPT(1))
           rmet(3)=kwst(lh)  ! overwater stability category
           rmet(4)=u         ! overwater wind speed
           rmet(5)=qhl(lh)   ! overland mixing height
           rmet(6)=ikst(lh)  ! overland stability category
           rmet(7)=qu(lh)    ! overland wind speed
           call wrconc(rmet,phchi,nrecep)
        ENDIF                                                           HRC01250
C                                                                       HRC01260
C  CALCULATE AND STORE FOR HIGH-FIVE TABLE.                             HRC01270
C                                                                       HRC01280
      NHR=NHR+1                                                         HRC01290
C        IF OPTION 19 IS 1, DELETE COMPUTATIONS FOR AVG CONC.           HRC01300
C         FOR LENGTH OF RECORD AND HIGH-FIVE TABLE.                     HRC01310
      IF (IOPT(19).EQ.0) THEN                                           HRC01320
C        CUMULATE CONCENTRATIONS FOR AVG TIMES AND LENGTH OF RECORD.    HRC01330
         DO 970 K=1,NRECEP                                              HRC01340
            DO 960 L=1,NAVT                                             HRC01350
               CONC(K,L)=CONC(K,L)+PHCHI(K)                             HRC01360
960         CONTINUE                                                    HRC01370
            SUM(K)=SUM(K)+PHCHI(K)                                      HRC01380
970      CONTINUE                                                       HRC01390
                                                                        HRC01400
C          STORE DATE FOR WHICH CONCS. HAVE BEEN CALCULATED.            HRC01410
         JDAY=IDATE(2)                                                  HRC01420
C        SUBROUTINE RANK IS CALLED WHENEVER A COUNTER                   HRC01430
C        INDICATES THAT ENOUGH END-TO-END HOURLY CONCENTRATIONS         HRC01440
C        HAVE BEEN STORED OFF TO COMPLETE AN AVG TIME.                  HRC01450
C        NP3, NP8, NP24, NPX ARE USED AS COUNTERS FOR EACH              HRC01460
C        AVG TIME AND ARE ZEROED AFTER EACH CALL TO RANK.               HRC01470
         CALL RANK (1)                                                  HRC01480
         NP3=NP3+1                                                      HRC01490
         IF (NP3.EQ.3) THEN                                             HRC01500
            CALL RANK (2)                                               HRC01510
            NP3=0                                                       HRC01520
         ENDIF                                                          HRC01530
         NP8=NP8+1                                                      HRC01540
         IF (NP8.EQ.8) THEN                                             HRC01550
            CALL RANK (3)                                               HRC01560
            NP8=0                                                       HRC01570
         ENDIF                                                          HRC01580
         NP24=NP24+1                                                    HRC01590
         IF (NP24.EQ.24) THEN                                           HRC01600
            CALL RANK (4)                                               HRC01610
            NP24=0                                                      HRC01620
         ENDIF                                                          HRC01630
         IF (NAVT.NE.4) THEN                                            HRC01640
            NPX=NPX+1                                                   HRC01650
            IF (NPX.EQ.NAV5) THEN                                       HRC01660
               CALL RANK (5)                                            HRC01670
               NPX=0                                                    HRC01680
            ENDIF                                                       HRC01690
         ENDIF                                                          HRC01700
      ENDIF                                                             HRC01710
                                                                        HRC01720
      IF (IOPT(11).EQ.1.AND.IOPT(14).EQ.1) RETURN                       HRC01730
C        IF BOTH OPTIONS 11 AND 14 CALL FOR OUTPUT DELETIONS,           HRC01740
C         SKIP HOURLY PRINTOUT.                                         HRC01750
      CALL OUTHR                                                        HRC01760
                                                                        HRC01770
1390  FORMAT(' DATE BEING PROCESSED IS ',I2,I3,I2,/,
     &' DATE OF HOURLY EMISSION RECORD IS ',I2,I3,I2,/,
     &' THEY SHOULD BE CONSISTENT')
      RETURN                                                            HRC01820
      END                                                               HRC01830
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCHUM00010
      SUBROUTINE HUMID (JYR,JULDAY,JHR,WHUM)                            HUM00020
C                                                                       HUM00030
C PURPOSE:  CALCULATE HUMIDITY VARIABLE (BASED ON RELATIVE HUMIDITY,    HUM00040
C           WET BULB TEMPERATURE, OR DEW POINT)                         HUM00050
C                                                                       HUM00060
C I/O:   JYR, YEAR                                                      HUM00070
C       JULDAY, DAY                                                     HUM00080
C        JHR, HOUR                                                      HUM00090
C       WHUM, OVERWATER HUMIDITY (EXPRESSED AS RELATIVE HUMIDITY,       HUM00100
C             WET BULB TEMPERATURE, OR DEW POINT TEMPERATURE)           HUM00110
C                                                                       HUM00120
C CALLED BY: ADDMET                                                     HUM00130
C                                                                       HUM00140
C CALLS:  NONE                                                          HUM00150
C                                                                       HUM00160
C       MINERALS MANAGEMENT SERVICE                                     HUM00170
C       U.S. DEPARTMENT OF THE INTERIOR                                 HUM00180
C                                                                       HUM00190
C OCD             REVISION HISTORY:                                     HUM00200
C    DCD 880902   CREATED.                                              HUM00210
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCHUM00220
                                                                        HUM00230
      include 'params.cmn'
      INCLUDE 'store.cmn'                                               HUM00240
      INCLUDE 'opt.cmn'                                                 HUM00250
                                                                        HUM00260
      DATA A0/6.107799961/,A1/4.436518521E-1/,A2/1.428945805E-2/        HUM00270
      DATA A3/2.650648471E-4/,A4/3.031240396E-6/,A5/2.034080948E-8/     HUM00280
      DATA A6/6.136820929E-11/                                          HUM00290
                                                                        HUM00300
                                                                        HUM00310
      GO TO (5600,5700,5800),JOPT(4)                                    HUM00320
                                                                        HUM00330
5600  WRH(JHR) = WHUM                                                   HUM00340
      IF(WHUM.LE.0.0.OR.WHUM.GT.100.) THEN                              HUM00350
         WRITE(IO,5) JYR,JULDAY,JHR                                     HUM00360
         WRITE(IO,10) WHUM                                              HUM00370
         WRITE(ierr,5) JYR,JULDAY,JHR
         WRITE(ierr,10) WHUM
         STOP 'Error encountered.  See ERROR.OUT for more details.'
      ENDIF                                                             HUM00390
      RETURN                                                            HUM00400
                                                                        HUM00410
C                                                                       HUM00420
C     CONVERT WET BULB TEMP. TO RH; CHECK FOR RH > 100                  HUM00430
C                                                                       HUM00440
5700  IF(WHUM.LT.200.OR.WHUM.GT.330.) THEN                              HUM00450
         WRITE(IO,5) JYR,JULDAY,JHR                                     HUM00460
         WRITE(IO,15) WHUM                                              HUM00470
         WRITE(ierr,5) JYR,JULDAY,JHR
         WRITE(ierr,15) WHUM
         STOP 'Error encountered.  See ERROR.OUT for more details.'
      ENDIF                                                             HUM00490
      IF(WTAIR(JHR).LT.WHUM) THEN                                       HUM00500
         WRITE(IO,5) JYR,JULDAY,JHR                                     HUM00510
         WRITE(IO,20) WTAIR(JHR),WHUM                                   HUM00520
         WRITE(ierr,5) JYR,JULDAY,JHR
         WRITE(ierr,20) WTAIR(JHR),WHUM
         STOP 'Error encountered.  See ERROR.OUT for more details.'
      ENDIF                                                             HUM00540
                                                                        HUM00550
C     QSATTB IS SATURATION MIXING RATIO AT T=WET BULB TEMP.(T IN DEG C) HUM00560
C     QSATTA IS SATURATION MIXING RATIO AT T=AIR TEMP.                  HUM00570
                                                                        HUM00580
      TBDEGC = WHUM - 273.15                                            HUM00590
      TADEGC = WTAIR(JHR) - 273.15                                      HUM00600
      DT = TADEGC - TBDEGC                                              HUM00610
      ESATTB = A0+TBDEGC*(A1+TBDEGC*(A2+TBDEGC*(A3+TBDEGC*(A4+TBDEGC*   HUM00620
     & (A5+TBDEGC*A6)))))                                               HUM00630
      QSATTB = 0.622*ESATTB/(1000.-ESATTB)                              HUM00640
      XLAT = 593.-0.566*TADEGC                                          HUM00650
      Q = (QSATTB-0.24*DT/XLAT)/(1.+0.441*DT/XLAT)                      HUM00660
      ESATTA = A0+TADEGC*(A1+TADEGC*(A2+TADEGC*(A3+TADEGC*(A4+TADEGC*   HUM00670
     & (A5+TADEGC*A6)))))                                               HUM00680
      QSATTA = 0.622*ESATTA/(1000.-ESATTA)                              HUM00690
      WRH(JHR) = 100.*Q/QSATTA                                          HUM00700
      RETURN                                                            HUM00710
                                                                        HUM00720
C                                                                       HUM00730
C     CONVERT DEW POINT TEMPERATURE TO RH                               HUM00740
C                                                                       HUM00750
5800  IF(WHUM.LT.200.OR.WHUM.GT.330.) THEN                              HUM00760
         WRITE(IO,5) JYR,JULDAY,JHR                                     HUM00770
         WRITE(IO,25) WHUM                                              HUM00780
         WRITE(ierr,5) JYR,JULDAY,JHR
         WRITE(ierr,25) WHUM
         STOP 'Error encountered.  See ERROR.OUT for more details.'
      ENDIF                                                             HUM00800
      IF(WTAIR(JHR).LT.WHUM) THEN                                       HUM00810
         WRITE(IO,5) JYR,JULDAY,JHR                                     HUM00820
         WRITE(IO,30) WTAIR(JHR),WHUM                                   HUM00830
         WRITE(ierr,5) JYR,JULDAY,JHR
         WRITE(ierr,30) WTAIR(JHR),WHUM
         STOP 'Error encountered.  See ERROR.OUT for more details.'
      ENDIF                                                             HUM00850
      WRH(JHR)=100.*((1.8*WHUM-.18*WTAIR(JHR)-240.703)/(1.62*WTAIR(JHR) HUM00860
     &  - 240.703))**8                                                  HUM00870
                                                                        HUM00880
5     FORMAT(' YEAR: ',I4,' DAY: ',I4,' HOUR: ',I4)                     HUM00890
10    FORMAT(' INVALID OVERWATER RELATIVE HUMIDITY VALUE: ',F10.3)      HUM00900
15    FORMAT(' INVALID OVERWATER WET BULB TEMPERATURE: ',F10.3)         HUM00910
20    FORMAT(' WTAIR (= ',F10.3,') IS LESS THAN OVERWATER WET BULB',/,
     &' TEMP (=',F10.3,')')
25    FORMAT(' INVALID OVERWATER DEW POINT TEMPERATURE: ',F10.3)        HUM00940
30    FORMAT(' WTAIR (= ',F10.3,') IS LESS THAN OVERWATER DEW POINT',/,
     &' TEMP (=',F10.3,')')
      RETURN                                                            HUM00980
      END                                                               HUM00990
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCINI00010
      SUBROUTINE INIT                                                   INI00020
C                                                                       INI00030
C PURPOSE: INITIALIZE VARIABLES                                         INI00040
C                                                                       INI00050
C I/O:  NONE                                                            INI00060
C                                                                       INI00070
C CALLED BY:  MAIN                                                      INI00080
C                                                                       INI00090
C CALLS:  NONE                                                          INI00100
C                                                                       INI00110
C       MINERALS MANAGEMENT SERVICE                                     INI00120
C       U.S. DEPARTMENT OF THE INTERIOR                                 INI00130
C                                                                       INI00140
C OCD             REVISION HISTORY:                                     INI00150
C    DCD 880630   CREATED.                                              INI00160
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCINI00170
                                                                        INI00180
      include 'params.cmn'
      INCLUDE 'count.cmn'                                               INI00190
      INCLUDE 'store.cmn'                                               INI00230
      INCLUDE 'const.cmn'                                               INI00240
      INCLUDE 'linesrc.cmn'                                             INI00250
                                                                        INI00260
C  INITIALIZE NUMBER OF LINE SOURCE SEGMENTS                            INI00270
      NSEGS = 10                                                        INI00280
                                                                        INI00290
      NPT=0                                                             INI00300
      IDAY=0                                                            INI00310
      NRECEP=0                                                          INI00320
      NP=0                                                              INI00330
      NHR=0                                                             INI00340
      NP3=0                                                             INI00350
      NP8=0                                                             INI00360
      NP24=0                                                            INI00370
      NPX=0                                                             INI00380
      DO 10 I=1,21                                                      INI00390
         TABLE(1,I)=0.                                                  INI00400
         TABLE(2,I)=0.                                                  INI00410
10    CONTINUE                                                          INI00420
      DO 40 I=1,maxrec                                                  INI00430
         SUM(I)=0.                                                      INI00440
         DO 30 J=1,5                                                    INI00450
            CONC(I,J)=0.                                                INI00460
            DO 20 K=1,5                                                 INI00470
               HMAXA(J,I,K)=0.                                          INI00480
20          CONTINUE                                                    INI00490
30       CONTINUE                                                       INI00500
40    CONTINUE                                                          INI00510
                                                                        INI00520
C        I/O DEVICE INITIALIZATIONS                                     INI00530
      IN=1                                                              INI00540
      IO=2                                                              INI00550
      ierr=3
                                                                        INI00560
      pb = char(12)
c   PB is the page break character (=^L)
      RETURN                                                            INI00570
      END                                                               INI00580
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCIYZ00010
      SUBROUTINE IYIZ( IYWSB,IZWSB,IYLS,IZLS,IYWSA,IZWSA)               IYZ00020
C                                                                       IYZ00030
C PURPOSE:  COMPUTES IY AND IZ  AT STACK-TOP HEIGHT                     IYZ00040
C                                                                       IYZ00050
C I/O: IYWSB, OVERWATER HORIZONTAL TURBULENCE INTENSITY AT STACK TOP    IYZ00060
C      IZWSB, OVERWATER VERTICAL TURBULENCE INTENSITY AT STACK TOP      IYZ00070
C       IYLS, LAND HORIZONTAL TURBULENCE INTENSITY AT STACK TOP         IYZ00080
C       IZLS, LAND VERTICAL TURBULENCE INTENSITY AT STACK TOP           IYZ00090
C      IYWSA, STABILITY E OVERWATER HORIZONTAL TURBULENCE INTENSITY     IYZ00100
C             AT STACK TOP                                              IYZ00110
C      IZWSA, STABILITY E OVERWATER VERTICAL TURBULENCE INTENSITY       IYZ00120
C             AT STACK TOP                                              IYZ00130
C                                                                       IYZ00140
C CALLED BY:  PTR                                                       IYZ00150
C                                                                       IYZ00160
C CALLS:  NONE                                                          IYZ00170
C                                                                       IYZ00180
C       MINERALS MANAGEMENT SERVICE                                     IYZ00190
C       U.S. DEPARTMENT OF THE INTERIOR                                 IYZ00200
C                                                                       IYZ00210
C OCD             REVISION HISTORY:                                     IYZ00220
C    DCD 880908   CREATED.                                              IYZ00230
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCIYZ00240
                                                                        IYZ00250
      include 'params.cmn'
      INCLUDE 'hrdat.cmn'                                               IYZ00260
      INCLUDE 'met.cmn'                                                 IYZ00270
      INCLUDE 'src.cmn'                                                 IYZ00280
      INCLUDE 'store.cmn'                                               IYZ00290
                                                                        IYZ00300
      REAL IYLS,IZLS,IZWSB,IYWSB,IZWSA,IYWSA                            IYZ00310
                                                                        IYZ00320
C-- IY SECTION                                                          IYZ00330
      IF(IYW.LE.0.0) THEN                                               IYZ00340
        IF(EL.LT.0.0) THEN                                              IYZ00350
           FY = (4.9 - 0.5*HL/EL)**0.3333                               IYZ00360
        ELSE                                                            IYZ00370
           FY = 1.7                                                     IYZ00380
        ENDIF                                                           IYZ00390
        IYWSB= USTAR/UPL * FY                                           IYZ00400
        IYWSB= AMAX1(0.37/UPL,IYWSB)                                    IYZ00410
      ELSE                                                              IYZ00420
        IYWSB= IYW * U/UPL                                              IYZ00430
      ENDIF                                                             IYZ00440
                                                                        IYZ00450
C-- IZ SECTION                                                          IYZ00460
      IF(EL.LT.0.0) THEN                                                IYZ00470
         FZ1 = 1.3 * (1.0 - 3.0 * HWANE/EL)**0.3333                     IYZ00480
         FZ2 = 1.3 * (1.0 - 3.0 * ZST/EL)**0.3333                       IYZ00490
      ELSE                                                              IYZ00500
         FZ1 = 1.3                                                      IYZ00510
         FZ2 = 1.3                                                      IYZ00520
      ENDIF                                                             IYZ00530
C      OVERWATER PREDICTED SIGMA-W = 0.2 FOR NEUTRAL/UNSTABLE DT        IYZ00540
C        ORIGINAL FORMULATION FOR STABLE DT                             IYZ00550
      IF(IZW.LE.0.0) THEN                                               IYZ00560
         IF(WTDIF.LE.0.0) THEN                                          IYZ00570
            IZWSB = 0.2/UPL                                             IYZ00580
         ELSE                                                           IYZ00590
            IZWSB = USTAR/UPL * FZ2                                     IYZ00600
         ENDIF                                                          IYZ00610
      ELSE                                                              IYZ00620
         IZWSB= IZW * (U/UPL) * (FZ2/FZ1)                               IYZ00630
      ENDIF                                                             IYZ00640
                                                                        IYZ00650
C###    ASSIGN A CLASS G IZ IF VERY STABLE                              IYZ00660
      IF(KWIST.EQ.7) IZWSB= 0.02                                        IYZ00670
                                                                        IYZ00680
C                                                                       IYZ00690
C       IF AVAILABLE, COMPUTE IY AND IZ OVER LAND AT STACK-TOP HEIGHT   IYZ00700
C                                                                       IYZ00710
      IF(JOPT(5).EQ.1) THEN                                             IYZ00720
         SCALE = ALOG(HANE/Z0L)/ALOG(ZST/Z0L)                           IYZ00730
         IYLS = IYL * SCALE                                             IYZ00740
         IZLS = IZL * SCALE                                             IYZ00750
      ENDIF                                                             IYZ00760
C                                                                       IYZ00770
C-- FOR PLUMES ABOVE THE MIXING HEIGHT USE IZ FOR STABILITY E           IYZ00780
      IZWSA=0.03                                                        IYZ00790
      IYWSA=IYWSB                                                       IYZ00800
                                                                        IYZ00810
      RETURN                                                            IYZ00820
      END                                                               IYZ00830
c-----------------------------------------------------------------------
      subroutine julday (iyr, imo, iday, julian)
c-----------------------------------------------------------------------
c
c JULDAY        Version: 1.0            Level: 961126
c               Joseph C. Chang
c               EARTH TECH
c               196 Baker Avenue
c               Concord, MA 01742
c               Tel. (508)371-4256
c               Fax. (508)371-4280
c               E-mail chang@src.com, or jchang@earthtech.com
c
c PURPOSE:      Determine the Julian day.
c
c ARGUMENTS:
c   Input
c   -----
c   iyr    integer    year
c   imo    integer    month
c   iday   integer    day
c
c   Output
c   ------
c   julian integer    Julian day
c
c CALLING ROUTINES:     FILPOS, DAYLOOP
c
c EXTERNAL ROUTINES:    none
c
c-----------------------------------------------------------------------
        integer nodays(12)
        data nodays/31,28,31,30,31,30,31,31,30,31,30,31/
        julian=0
        if(imo.ne.1) goto 20
        julian=iday
        return
20      do 10 i=1,imo-1
        julian=julian+nodays(i)
10      continue
        julian=julian+iday
        if(mod(iyr,4).ne.0) return
        if(imo.eq.2) return
        julian=julian+1
        return
        end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCLNS00010
      SUBROUTINE LINESEG(X1,Y1)                                         LNS00020
C                                                                       LNS00030
C PURPOSE:  FIND THE (X,Y) MIDPOINT COORDINATES FOR EACH SEGMENT OF     LNS00040
C           LINE SOURCE                                                 LNS00050
C                                                                       LNS00060
C I/O:    X1, STARTING POINT OF EAST COORDINATE OF LINE SOURCE          LNS00070
C         Y1, STARTING POINT OF NORTH COORDINATE OF LINE SOURCE         LNS00080
C                                                                       LNS00090
C CALLED BY:  READCF2                                                   LNS00100
C                                                                       LNS00110
C CALLS: NONE                                                           LNS00120
C                                                                       LNS00130
C       MINERALS MANAGEMENT SERVICE                                     LNS00140
C       U.S. DEPARTMENT OF THE INTERIOR                                 LNS00150
C                                                                       LNS00160
C OCD             REVISION HISTORY:                                     LNS00170
C    DCD 890227   CREATED.                                              LNS00180
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCLNS00190
                                                                        LNS00200
      include 'params.cmn'
      INCLUDE 'linesrc.cmn'                                             LNS00210
      INCLUDE 'store.cmn'                                               LNS00220
      INCLUDE 'src.cmn'                                                 LNS00230
                                                                        LNS00240
C  CALCULATIONS ARE PERFORMED USING THE POINT-OF-DIVISION FORMULA (SEE  LNS00250
C  RIDDLE (1974) OR ANY OTHER STANDARD GEOMETRY REFERENCE)              LNS00260
                                                                        LNS00270
      REAL SEGX(10),SEGY(10)                                            LNS00280
                                                                        LNS00290
C  MAKE R=1/2 OF 1/SEGMENTS:  ALL ODD LOOP INDICATOR CALCULATIONS WILL  LNS00300
C  BE MIDPOINT VALUES. ALL EVEN LOOP INDICATOR CALCULATIONS WILL BE     LNS00310
C  SEGMENT VALUES.  USE IS AND IM AS COUNTERS TO FILL THE ARRAYS SEGX,  LNS00320
C  SEGY, MIDX, AND MIDY.  NOTE THAT REALI IS TWICE THE NUMBER OF        LNS00330
C  SEGMENTS MINUS ONE.                                                  LNS00340
                                                                        LNS00350
      X2 = XSTOP                                                        LNS00360
      Y2 = YSTOP                                                        LNS00370
                                                                        LNS00380
      R=(1.0/NSEGS)/2.0                                                 LNS00390
      RINC=R                                                            LNS00400
      REALI=NSEGS*2.0                                                   LNS00410
      ISEG=IFIX(REALI)                                                  LNS00420
      ISEGD2=ISEG/2                                                     LNS00430
      IS=1                                                              LNS00440
      IM=1                                                              LNS00450
                                                                        LNS00460
C  SET ALL ARRAY VALUES TO ZERO.                                        LNS00470
                                                                        LNS00480
      DO 500 J=1,10                                                     LNS00490
        SEGX(J)=0.0                                                     LNS00500
        SEGY(J)=0.0                                                     LNS00510
        MIDX(J)=0.0                                                     LNS00520
        MIDY(J)=0.0                                                     LNS00530
500   CONTINUE                                                          LNS00540
                                                                        LNS00550
      DO 1000 X=1,ISEG                                                  LNS00560
        IF(AMOD(X,2.0).EQ.0) THEN                                       LNS00570
          SEGX(IS)=X1+R*(X2-X1)                                         LNS00580
          SEGY(IS)=Y1+R*(Y2-Y1)                                         LNS00590
          IS=IS+1                                                       LNS00600
        ELSEIF(AMOD(X,2.0).NE.0) THEN                                   LNS00610
          MIDX(IM)=X1+R*(X2-X1)                                         LNS00620
          MIDY(IM)=Y1+R*(Y2-Y1)                                         LNS00630
          IM=IM+1                                                       LNS00640
        ELSE                                                            LNS00650
      ENDIF                                                             LNS00660
                                                                        LNS00670
        R=R+RINC                                                        LNS00680
                                                                        LNS00690
1000  CONTINUE                                                          LNS00700
                                                                        LNS00710
C    CALCULATE THE DISTANCE BETWEEN MIDPOINTS IN USER KM                LNS00720
      SEGL = SQRT((MIDX(2)-MIDX(1))**2 + (MIDY(2)-MIDY(1))**2)*CONTWO   LNS00730
                                                                        LNS00740
      WRITE(IO,100)                                                     LNS00750
      WRITE(IO,110) X1,Y1                                               LNS00760
      WRITE(IO,120) X2,Y2                                               LNS00770
      WRITE(IO,130) NSEGS                                               LNS00780
      WRITE(IO,140)                                                     LNS00790
      DO 1400 K=1,ISEGD2                                                LNS00800
         WRITE(IO,150) SEGX(K),SEGY(K),MIDX(K),MIDY(K)                  LNS00810
1400  CONTINUE                                                          LNS00820
      WRITE(IO,160)                                                     LNS00830
                                                                        LNS00840
100   FORMAT(//,16X,'LINE SOURCE CONFIGURATION')                        LNS00850
110   FORMAT(' STARTING COORDINATES (USER UNITS): ',2F10.3)             LNS00860
120   FORMAT(' ENDING COORDINATES (USER UNITS): ',2F10.3)               LNS00870
130   FORMAT(' NUMBER OF LINE SEGMENTS TO BE MODELED: ',I2)             LNS00880
140   FORMAT('   SEGMENT X  SEGMENT Y  MIDPOINT X  MIDPOINT Y')         LNS00890
150   FORMAT(1X,2(1X,F10.3),2(2X,F10.3))                                LNS00900
160   FORMAT(///)                                                       LNS00910
                                                                        LNS00920
      RETURN                                                            LNS00930
      END                                                               LNS00940
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCMAP00010
      SUBROUTINE MAP                                                    MAP00020
C                                                                       MAP00030
C PURPOSE: PRINTS A MAP OF THE LAND/WATER GRID INPUT                    MAP00040
C                                                                       MAP00050
C I/O:  NONE                                                            MAP00060
C                                                                       MAP00070
C CALLED BY:  READCF2                                                   MAP00080
C                                                                       MAP00090
C CALLS:  NONE                                                          MAP00100
C                                                                       MAP00110
C       MINERALS MANAGEMENT SERVICE                                     MAP00120
C       U.S. DEPARTMENT OF THE INTERIOR                                 MAP00130
C                                                                       MAP00140
C OCD             REVISION HISTORY:                                     MAP00150
C    DCD 880713   CREATED.                                              MAP00160
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCMAP00170
                                                                        MAP00180
C       MAP PRODUCES A PAGE-SIZE MAP THAT INCLUDES LOCATIONS            MAP00190
C       OF ALL POINT SOURCES, RECEPTORS, AND SHORELINES.  DUE TO THE    MAP00200
C       LIMITED RESOLUTION OF THE PRINTER, SOME FEATURES MAY BE         MAP00210
C       PRINTED ON TOP OF EACH OTHER.                                   MAP00220
                                                                        MAP00230
      include 'params.cmn'
      INCLUDE 'opt.cmn'                                                 MAP00240
      INCLUDE 'src.cmn'                                                 MAP00250
      INCLUDE 'shr.cmn'                                                 MAP00270
      INCLUDE 'store.cmn'                                               MAP00280
      INCLUDE 'const.cmn'                                               MAP00290
      INCLUDE 'linesrc.cmn'                                             MAP00300
                                                                        MAP00310
      character*1 LETS,KAST,plus                                        MAP00320
      character*1 MAPX(maxmap,maxmap)                                   MAP00330
      character*20 ifmt
      DATA LETS/'S'/,KAST/'*'/,PLUS/'+'/                                MAP00340
                                                                        MAP00350
c       Construct format statement
        write (ifmt,111) maxmap
111     format ('(6x,',i3,'a1)')
C       COMPUTE XMAX, XMIN, YMAX, YMIN                                  MAP00360
                                                                        MAP00370
      XMIN = X0                                                         MAP00380
      YMAX = Y0                                                         MAP00390
      XMAX = X0 + NX*DELX                                               MAP00400
      YMIN = Y0 - NY*DELY                                               MAP00410
C                                                                       MAP00420
C       INITIALIZE AND FILL IN LOCAL MAP ARRAY                          MAP00430
C                                                                       MAP00440
      DO 400 I = 1,maxmap                                               MAP00450
         DO 410 J = 1,maxmap                                            MAP00460
            MAPX(I,J) = BLNK                                            MAP00470
410      CONTINUE                                                       MAP00480
400   CONTINUE                                                          MAP00490
      MARX = (maxmap-NX)/2                                              MAP00500
      MARY = (maxmap-NY)/2                                              MAP00510
      DO 500 J = 1,NX                                                   MAP00520
         DO 510 I = 1,NY                                                MAP00530
            MAPX(I+MARY,J+MARX) = XYMAP(I,J)                            MAP00540
            IF(MAPX(I+MARY,J+MARX).EQ.LETW)                             MAP00550
     &           MAPX(I+MARY,J+MARX) = BLNK                             MAP00560
510      CONTINUE                                                       MAP00570
500   CONTINUE                                                          MAP00580
C                                                                       MAP00590
C       MARK CORNERS OF MAP                                             MAP00600
C                                                                       MAP00610
      MAPX(1+MARY,1+MARX) = PLUS                                        MAP00620
      MAPX(1+MARY,NX+MARX) = PLUS                                       MAP00630
      MAPX(NY+MARY,1+MARX) = PLUS                                       MAP00640
      MAPX(NY+MARY,NX+MARX) = PLUS                                      MAP00650
C                                                                       MAP00660
C       PRINT THIS MAP                                                  MAP00670
C                                                                       MAP00680
      WRITE(IO,10) pb,XMIN,XMAX,YMIN,YMAX,DELX,DELY                     MAP00690
      WRITE(IO,ifmt) ((MAPX(I,J),J=1,maxmap),I=1,maxmap)                MAP00700
C                                                                       MAP00710
C       MAP THE RECEPTOR POINTS NEXT                                    MAP00720
C                                                                       MAP00730
      XRANGE = DELX * NX                                                MAP00740
      YRANGE = DELY * NY                                                MAP00750
      DO 5000 I = 1,NRECEP                                              MAP00760
         X = RREC(I)                                                    MAP00770
         Y = SREC(I)                                                    MAP00780
         IF(X.LT.XMIN+0.001*DELX.OR.X.GT.XMAX-0.001*DELX.OR.            MAP00790
     &     Y.LT.YMIN+0.001*DELY.OR.Y.GT.YMAX-0.001*DELY) GO TO 5000     MAP00800
         IX = INT((X-XMIN)/XRANGE * NX + 1.0 + MARX)                    MAP00810
         IY = NY + 1 + MARY - INT((Y-YMIN)/YRANGE * NY + 1.0)           MAP00820
         MAPX(IY,IX) = KAST                                             MAP00830
5000  CONTINUE                                                          MAP00840
C                                                                       MAP00850
C       MAP THE SOURCES LAST                                            MAP00860
C                                                                       MAP00870
      IF(IOPT(20).EQ.2) THEN                                            MAP00880
         NCOUNT=NSEGS                                                   MAP00890
      ELSE                                                              MAP00900
         NCOUNT = NPT                                                   MAP00910
      ENDIF                                                             MAP00920
                                                                        MAP00930
      DO 6000 I = 1,NCOUNT                                              MAP00940
         IF(IOPT(20).EQ.2) THEN                                         MAP00950
            X = MIDX(I)                                                 MAP00960
            Y = MIDY(I)                                                 MAP00970
         ELSE                                                           MAP00980
            X = SOURCE(1,I)                                             MAP00990
            Y = SOURCE(2,I)                                             MAP01000
         ENDIF                                                          MAP01010
         IF(X.LT.XMIN+0.001*DELX.OR.X.GT.XMAX-0.001*DELX.OR.            MAP01020
     &      Y.LT.YMIN+0.001*DELY.OR.Y.GT.YMAX-0.001*DELY) GO TO 6000    MAP01030
         IX = INT((X-XMIN)/XRANGE * NX + 1.0 + MARX)                    MAP01040
         IY = NY + 1 + MARY - INT((Y-YMIN)/YRANGE * NY + 1.0)           MAP01050
         MAPX(IY,IX) = LETS                                             MAP01060
6000  CONTINUE                                                          MAP01070
C                                                                       MAP01080
C       PRINT THE MAP                                                   MAP01090
C                                                                       MAP01100
      WRITE(IO,30) pb,XMIN,XMAX,YMIN,YMAX,DELX,DELY                     MAP01110
      WRITE(IO,ifmt) ((MAPX(I,J),J=1,maxmap),I=1,maxmap)                MAP01120
                                                                        MAP01130
10    FORMAT( a1,15X,' MAP OF USER-SPECIFIED LAND/WATER ',              MAP01140
     &  'DISTRIBUTION;  L = LAND AREA, (BLANK) = WATER AREA',/,         MAP01150
     &   5X,'RANGE OF X: ',F9.3,' TO ',F9.3,';  RANGE OF ',             MAP01160
     &  'Y: ',F9.3,' TO ',F9.3,';  GRID (X,Y) LENGTHS = (',F9.3,        MAP01170
     &  ',',F9.3,') USER UNITS',/)                                      MAP01180
20    FORMAT(6X,120A1)                                                  MAP01190
30    FORMAT( a1,' MAP OF LAND/WATER, MODEL RECEPTORS (*), ',           MAP01200
     &  'AND POINT SOURCES (S); L = LAND , (BLANK) = WATER AREA; ',     MAP01210
     &  'SOME SYMBOLS MAY BE OVERWRITTEN',/,                            MAP01220
     &   1X,'RANGE OF X: ',F9.3,' TO ',F9.3,'; RANGE OF ',              MAP01230
     &  'Y: ',F9.3,' TO ',F9.3,'; GRID (X,Y) LENGTHS = (',F9.3,         MAP01240
     &  ',',F9.3,') USER UNITS',/)                                      MAP01250
                                                                        MAP01260
      RETURN                                                            MAP01270
      END                                                               MAP01280
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCMTR00010
      SUBROUTINE METRES(DELN,DELM,IFREQ,URES)                           MTR00020
C                                                                       MTR00030
C PURPOSE:  CALCULATE RESULTANT MET DATA SUMMARY FOR AVERAGING PERIOD   MTR00040
C                                                                       MTR00050
C I/O:  DELN, AVERAGE WIND COMPONENT IN NORTH DIRECTION                 MTR00060
C       DELM, AVERAGE WIND COMPONENT IN EAST DIRECTION                  MTR00070
C      IFREQ, STABILITY CLASS FREQUENCY IN NUMBER OF HOURS              MTR00080
C       URES, RESULTANT OVERWATER WIND SPEED                            MTR00090
C                                                                       MTR00100
C CALLED BY:  MAIN                                                      MTR00110
C                                                                       MTR00120
C CALLS:  NONE                                                          MTR00130
C                                                                       MTR00140
C       MINERALS MANAGEMENT SERVICE                                     MTR00150
C       U.S. DEPARTMENT OF THE INTERIOR                                 MTR00160
C                                                                       MTR00170
C OCD             REVISION HISTORY:                                     MTR00180
C    DCD 880713   CREATED.                                              MTR00190
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCMTR00200
                                                                        MTR00210
      include 'params.cmn'
      INCLUDE 'src.cmn'                                                 MTR00220
      INCLUDE 'opt.cmn'                                                 MTR00230
      INCLUDE 'store.cmn'                                               MTR00240
                                                                        MTR00260
      DIMENSION IFREQ(7)                                                MTR00270
                                                                        MTR00280
C        CALCULATE RESULTANT  WIND DIRECTION THETA                      MTR00290
      DELN=DELN/NAVG                                                    MTR00300
      DELM=DELM/NAVG                                                    MTR00310
      THETA=ANGARC(DELM,DELN)                                           MTR00320
C        CALCULATE AVERAGE AND RESULTANT SPEED AND PERSISTENCE.         MTR00330
      U=U/NAVG                                                          MTR00340
      TEMP=TEMP/NAVG                                                    MTR00350
      URES=SQRT(DELN*DELN+DELM*DELM)                                    MTR00360
      PERSIS=URES/U                                                     MTR00370
C        DETERMINE MODEL AND AVERAGE STABILITY                          MTR00380
      LSMAX=0                                                           MTR00390
      DO 810 I=1,7                                                      MTR00400
         LST=IFREQ(I)                                                   MTR00410
         IF (LST.GT.LSMAX) THEN                                         MTR00420
            LSMAX=LST                                                   MTR00430
            LSTAB=I                                                     MTR00440
         ENDIF                                                          MTR00450
810   CONTINUE                                                          MTR00460
      IP1=LSTAB+1                                                       MTR00470
      KST=LSTAB                                                         MTR00480
      DO 820 I=IP1,7                                                    MTR00490
         IF (LSMAX.EQ.IFREQ(I)) GO TO 830                               MTR00500
820   CONTINUE                                                          MTR00510
      GO TO 850                                                         MTR00520
C        IF TIE FOR MAX MODEL STABILITY CALCULATE AVERAGE STABILITY     MTR00530
830   KSUM=0                                                            MTR00540
      DO 840 J=1,7                                                      MTR00550
         KSUM=KSUM+IFREQ(J)*J                                           MTR00560
840   CONTINUE                                                          MTR00570
      KST=FLOAT(KSUM)/FLOAT(NAVG)+0.5                                   MTR00580
C        PRINT RESULTANT MET DATA SUMMARY FOR AVERAGING PERIOD.         MTR00590
850   WRITE (IO,1710)                                                   MTR00600
      WRITE (IO,1720) THETA,URES,U,TEMP,PERSIS,KST                      MTR00610
                                                                        MTR00620
1710  FORMAT ( / ,'RESULTANT MET CONDITIONS'/1X)                        MTR00630
1720  FORMAT (2X,'OVERWATER WIND DIRECTION = ',F6.1,10X,'RESULTANT ',   MTR00640
     X 'OVERWATER WIND SPEED = ',F6.2,10X,                              MTR00650
     X 'AVERAGE WIND SPEED = ',F7.2,/,2X,'AVERAGE SEA AIR TEMP = ',F7.2,MTR00660
     X ' DEG K',10X,'WIND PERSISTENCE = ',F6.3,10X,'MODEL STABILITY =', MTR00670
     X I2)                                                              MTR00680
                                                                        MTR00690
      RETURN                                                            MTR00700
      END                                                               MTR00710
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCMTS00010
      SUBROUTINE METSMRY(I,JHR,DELN,DELM,IFREQ,URES)                    MTS00020
C                                                                       MTS00030
C PURPOSE:  PRODUCE MET DATA SUMMARY FOR AVERAGING PERIOD               MTS00040
C                                                                       MTS00050
C I/O:  I, DAY LOOP PARAMETER                                           MTS00060
C     JHR, HOUR                                                         MTS00070
C    DELN, AVERAGE WIND COMPONENT IN NORTH DIRECTION                    MTS00080
C    DELM, AVERAGE WIND COMPONENT IN EAST DIRECTION                     MTS00090
C   IFREQ, STABILITY CLASS FREQUENCY IN NUMBER OF HOURS                 MTS00100
C    URES, RESULTANT OVERWATER WIND SPEED                               MTS00110
C                                                                       MTS00120
C CALLED BY:  AVGLOOP                                                   MTS00130
C                                                                       MTS00140
C CALLS:  NONE                                                          MTS00150
C                                                                       MTS00160
C       MINERALS MANAGEMENT SERVICE                                     MTS00170
C       U.S. DEPARTMENT OF THE INTERIOR                                 MTS00180
C                                                                       MTS00190
C OCD             REVISION HISTORY:                                     MTS00200
C    DCD 880713   CREATED.                                              MTS00210
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCMTS00220
                                                                        MTS00230
      include 'params.cmn'
      INCLUDE 'src.cmn'                                                 MTS00240
      INCLUDE 'opt.cmn'                                                 MTS00250
      INCLUDE 'store.cmn'                                               MTS00260
      INCLUDE 'const.cmn'                                               MTS00280
                                                                        MTS00290
      DIMENSION IFREQ(7)                                                MTS00300
                                                                        MTS00310
      IF (I.EQ.NB) WRITE (IO,1690) pb,IDATE                             MTS00320
      TRAD=QTHETA(JHR)*DG2RAD                                           MTS00330
C                                                                       MTS00340
C       WRITE AN EXPANDED LIST OF METEOROLOGICAL PARAMETERS             MTS00350
C                                                                       MTS00360
      WRITE (IO,1700) JHR,QTHETA(JHR),QU(JHR),WU(JHR),QHL(JHR),WHL(JHR),MTS00370
     & IKST(JHR),QTEMP(JHR),WTAIR(JHR),WTDIFF(JHR),WRH(JHR),WSH(JHR),   MTS00380
     & WIY(JHR),WIZ(JHR),QIY(JHR),QIZ(JHR),WUSTAR(JHR),WEL(JHR)         MTS00390
      SINT=SIN(TRAD)                                                    MTS00400
      COST=COS(TRAD)                                                    MTS00410
                                                                        MTS00420
C        CALCULATE WIND COMPONENTS                                      MTS00430
C                                                                       MTS00440
C       USE OVER-WATER WIND SPEED THROUGHOUT TO AVOID PROBLEMS WITH     MTS00450
C       CONSERVATION OF MASS.                                           MTS00460
C                                                                       MTS00470
      URES=WU(JHR)                                                      MTS00480
      UR=URES*SINT                                                      MTS00490
      VR=URES*COST                                                      MTS00500
      DELM=DELM+UR                                                      MTS00510
      DELN=DELN+VR                                                      MTS00520
C                                                                       MTS00530
C       USE OVER-WATER AIR TEMPERATURE                                  MTS00540
C                                                                       MTS00550
      TEMP=TEMP+WTAIR(JHR)                                              MTS00560
      U=U+URES                                                          MTS00570
      KST=IKST(JHR)                                                     MTS00580
      IFREQ(KST)=IFREQ(KST)+1                                           MTS00590
                                                                        MTS00600
1690  FORMAT ( a1,'INPUT MET DATA  ',I2,'/',I4/3X,                      MTS00610
     & '             LAND    SEA    LAND   SEA          LAND   SEA  ',  MTS00620
     & '   AIR-   SEA   WIND                                         '/,MTS00630
     &'                WIND   WIND   MIXING MIXING LAND   AIR    AIR  ',MTS00640
     & '  WATER   REL    DIR    OVER- OVER- OVER- OVER-              '/,MTS00650
     &'         WIND  SPEED   SPEED  HEIGHT HEIGHT STAB.  TEMP  TEMP  ',MTS00660
     & ' DELTA-T  HUM   SHEAR   WATER WATER LAND  LAND   USTAR     L '/,MTS00670
     &'   HOUR  DIR  (M/SEC) (M/SEC)  (M)    (M)   CLASS  (K)    (K)  ',MTS00680
     & '   (K)    ( )  (DEG/M)   IY    IZ    IY    IZ   (M/SEC)   (M)'/)MTS00690
1700  FORMAT (4X,I2,2X,F5.0,1X,F7.2,F7.2,3X,F5.0,2X,F5.0,1X,I3,3X,      MTS00700
     &  F6.1,F6.1,1X,F7.2,1X,F5.1,2X,F7.5,2X,4(F5.3,1X),1X,F5.2,        MTS00710
     &  2X,E8.2)                                                        MTS00720
                                                                        MTS00730
      RETURN                                                            MTS00740
      END                                                               MTS00750
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCMIX00010
      SUBROUTINE MIXHT( HLR,INTIBL,XD,XL,X2,XTIBL,SIGZW,CAP)            MIX00020
C                                                                       MIX00030
C PURPOSE:  CALCULATE MIXING HEIGHT FOR VERTICAL MIXING                 MIX00040
C                                                                       MIX00050
C I/O:  HLR, HOURLY MIXING HEIGHT VALUE                                 MIX00060
C    INTIBL, FLAG TO INDICATE IF PLUME IS (1) OR IS NOT (0) IN THE TIBL MIX00070
C        XD, DISTANCE FROM SOURCE TO RECEPTOR                           MIX00080
C        XL, DISTANCE FROM SOURCE TO WHERE PLUME ENTERS TIBL            MIX00090
C        X2, DISTANCE FROM LAND TO RECEPTOR                             MIX00100
C     XTIBL, DISTANCE FROM WHERE PLUME CROSSES SHORE TO WHERE PLUME     MIX00110
C              ENTERS TIBL                                              MIX00120
C     SIGZW, SIGMA-Z (WATER) AT RECEPTOR                                MIX00130
C       CAP, STABLE INTERNAL BOUNDARY LAYER CAP                         MIX00140
C                                                                       MIX00150
C CALLED BY:  CALC                                                      MIX00160
C                                                                       MIX00170
C CALLS:  NONE                                                          MIX00180
C                                                                       MIX00190
C       MINERALS MANAGEMENT SERVICE                                     MIX00200
C       U.S. DEPARTMENT OF THE INTERIOR                                 MIX00210
C                                                                       MIX00220
C OCD             REVISION HISTORY:                                     MIX00230
C    DCD 880914   CREATED.                                              MIX00240
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCMIX00250
                                                                        MIX00260
      INCLUDE 'met.cmn'                                                 MIX00270
      INCLUDE 'ptrrcp.cmn'                                              MIX00280
                                                                        MIX00290
C  FOR OVERLAND SOURCES, SET MIXING HEIGHT TO UNLIMITED                 MIX00300
      IF(XLAND.LE.0.0) THEN                                             MIX00310
         INTIBL = 1                                                     MIX00320
         HLR = 99999.                                                   MIX00330
         RETURN                                                         MIX00340
      ENDIF                                                             MIX00350
C  OVER WATER RECEPTORS                                                 MIX00360
C  UNLIMITED MIXING IF SOURCE HEIGHT > MARINE MIXING HEIGHT             MIX00370
C    OTHERWISE USE MARINE MIXING HEIGHT (HLR)                           MIX00380
                                                                        MIX00390
      INTIBL = 0                                                        MIX00400
      IF(XLAND.GT.XD) THEN                                              MIX00410
         IF(H.GE.HLR) HLR = 99999.                                      MIX00420
      ELSE                                                              MIX00430
                                                                        MIX00440
C  OVER LAND RECEPTORS                                                  MIX00450
C                                                                       MIX00460
C  PLUME NEVER ENTERS TIBL                                              MIX00470
        IF(XD.LT.XL) THEN                                               MIX00480
                                                                        MIX00490
C  UNLIMITED MIXING IF PLUME IS ABOVE MIXING HEIGHT OTHERWISE USE HLR   MIX00500
           IF(HA.GT.HLR) HLR = 99999.                                   MIX00510
                                                                        MIX00520
C                                                                       MIX00530
C  PLUME ENTERS TIBL                                                    MIX00540
C  XTIBL IN KM, X2 IN M                                                 MIX00550
C  CALCULATE TIBL HEIGHT AT X2 (HTX2)                                   MIX00560
                                                                        MIX00570
         ELSE                                                           MIX00580
            INTIBL = 1                                                  MIX00590
            IF(X2.LE.2000.) THEN                                        MIX00600
               HTX2=0.1*X2                                              MIX00610
            ELSE                                                        MIX00620
               HTX2=200.+0.03333*(X2-2000.)                             MIX00630
            ENDIF                                                       MIX00640
                                                                        MIX00650
c           IF(KST.LE.5) HTX2=AMIN1(HTX2,CAP)                           MIX00660
c      The mixing height is capped if KST is 5 or 6 (stable)            XXX00660
            IF(KST.GE.5) HTX2=AMIN1(HTX2,CAP)                           XXX00661
            HLR=AMAX1(HA-(XTIBL*1000.*ZER/X2)+2.*SIGZW,HTX2)            MIX00670
         ENDIF                                                          MIX00680
      ENDIF                                                             MIX00690
                                                                        MIX00700
      RETURN                                                            MIX00710
      END                                                               MIX00720
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCOTA00010
      SUBROUTINE OUTAVG                                                 OTA00020
C                                                                       OTA00030
C PURPOSE:  PROVIDES OUTPUT CONCENTRATIONS IN TWO FORMS IN              OTA00040
C        MICROGRAMS PER CUBIC METER FOR EACH AVERAGING PERIOD:          OTA00050
C           1) CONTRIBUTIONS FROM SIGNIFICANT SOURCES, AND              OTA00060
C           2) SUMMARIES.                                               OTA00070
C                                                                       OTA00080
C I/O:  NONE                                                            OTA00090
C                                                                       OTA00100
C CALLED BY:  MAIN                                                      OTA00110
C                                                                       OTA00120
C CALLS:  AVGCNTR                                                       OTA00130
C                                                                       OTA00140
C       MINERALS MANAGEMENT SERVICE                                     OTA00150
C       U.S. DEPARTMENT OF THE INTERIOR                                 OTA00160
C                                                                       OTA00170
C OCD             REVISION HISTORY:                                     OTA00180
C    DCD 881026   CREATED.                                              OTA00190
C    JCC 920819   UPDATED.                                              XXX00190
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCOTA00200
                                                                        OTA00210
      include 'params.cmn'
      INCLUDE 'opt.cmn'                                                 OTA00220
      INCLUDE 'store.cmn'                                               OTA00240
      INCLUDE 'const.cmn'                                               XXX00240
                                                                        OTA00250
      character*4 IPOLT(5),ipolu                                        OTA00260
      DATA IPOLT /'SO2 ','TSP ','NOX ','CO  ','    '/                   OTA00270
      IPOLU=IPOLT(IPOL-2)                                               OTA00280
                                                                        OTA00290
C         CONCENTRATION OUTPUT                                          OTA00300
C        IN MICROGRAMS PER CUBIC METER ARE PRINTED FOR THE              OTA00310
C        AVERAGING PERIOD. CONTRIBUTIONS AND/OR SUMMARY                 OTA00320
C        INFORMATION IS AVAILABLE.                                      OTA00330
C        AVERAGE CONCENTRATIONS OVER SPECIFIED TIME PERIOD              OTA00340
      DO 190 K=1,NRECEP                                                 OTA00350
         PCHI(K)=PCHI(K)/NAVG                                           OTA00360
         HSAV(K)=PCHI(K)                                                OTA00370
         DO 180 I=1,26                                                  OTA00380
            PSIGS(K,I)=PSIGS(K,I)/NAVG                                  OTA00390
180      CONTINUE                                                       OTA00400
190   CONTINUE                                                          OTA00410
                                                                        OTA00420
C   OUTPUT OF THE AVERAGED CONTRIBUTIONS.                               OTA00430
      IF (IOPT(17).EQ.0) CALL AVGCNTR(IPOLU)                            OTA00440
                                                                        OTA00450
C   WRITE AVERAGING-TIME SUMMARY.                                       OTA00460
                                                                        OTA00470
C        OPTION(18): SKIP OUTPUT OF THE AVERAGED SUMMARIES.             OTA00480
      IF (IOPT(18).EQ.1) RETURN                                         OTA00490
      WRITE (IO,350) pb,LINE1,LINE2,LINE3                               OTA00500
      WRITE (IO,530) NAVG,IPOLU,IDATE,NB                                OTA00510
      WRITE (IO,500)                                                    OTA00520
C        CALCULATE GRAND TOTALS AND RANK CONCENTRATIONS                 OTA00530
      DO 290 I=1,NRECEP                                                 OTA00540
         CMAX=-1.0                                                      OTA00550
         DO 280 K=1,NRECEP                                              OTA00560
            IF (HSAV(K).GT.CMAX) THEN                                   OTA00570
               CMAX=HSAV(K)                                             OTA00580
               LMAX=K                                                   OTA00590
            ENDIF                                                       OTA00600
280      CONTINUE                                                       OTA00610
         IRANK(LMAX)=I                                                  OTA00620
         HSAV(LMAX)=-1.0                                                OTA00630
290   CONTINUE                                                          OTA00640
      IF(JAR.NE.0) THEN                                                 OTA00650
         DO 300 K=1,JAR                                                 OTA00660
            WRITE (IO,510) K,STAR(1,K),STAR(2,K),RNAME(K),              OTA00670
     &      RREC(K),SREC(K),ZR(K),ELR(K),PSIGS(K,26),PCHI(K),IRANK(K)   OTA00680
300      CONTINUE                                                       OTA00690
      ENDIF                                                             OTA00700
      IF(JAR.NE.NRECEP) THEN                                            OTA00710
         DO 320 K=JAR+1,NRECEP                                          OTA00720
            WRITE (IO,515) K,STAR(1,K),STAR(2,K),RNAME(K),              OTA00730
     &      RREC(K),SREC(K),ZR(K),ELR(K),PSIGS(K,26),PCHI(K),IRANK(K)   OTA00740
320   CONTINUE                                                          OTA00750
      ENDIF                                                             OTA00760
      RETURN                                                            OTA00770
                                                                        OTA00780
350   FORMAT ( a1,A80/1X,A80/1X,A80)                                    OTA00790
500   FORMAT ( / ,T7,'RECEPTOR',T23,'EAST',T33,'NORTH',T43,'RECEPTOR HT'OTA00800
     &,T61,'RECEPTOR',T78,'TOTAL FROM',T93,'TOTAL FROM',T106,'CONCENTRATOTA00810
     &ION'/' ',T7,'NO. NAME',T22,'COORD',T33,'COORD',T44,'ABV GRD (M)',TOTA00820
     &59,'GRD-LVL ELEV',T77,'SIGNIF POINT',T93,'ALL SOURCES',T111,'RANK'OTA00830
     &/' ',T58,'(USER HT UNITS)',T80,'SOURCES'//)                       OTA00840
510   FORMAT (' ',I8,2A1,2X,A8,2F10.2,F12.1,F20.1,6P2F15.4,I15)         OTA00850
515   FORMAT (' ',I8,2A1,2X,A8,2F10.2,F12.1,F20.1,6P2F15.4,I15)         OTA00860
530   FORMAT ( / ,T25,I2,'-HOUR AVERAGE ',A4,' SUMMARY CONCENTRATION TABOTA00870
     &LE(MICROGRAMS/M**3)',5X,I2,'/',I3,'  START HOUR: ',I2//1X)        OTA00880
                                                                        OTA00890
      END                                                               OTA00900
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCOTH00010
      SUBROUTINE OUTHR                                                  OTH00020
C                                                                       OTH00030
C PURPOSE: THIS SUBROUTINE PROVIDES HOURLY OUTPUT CONCENTRATIONS IN     OTH00040
C          MICROGRAMS PER CUBIC METER FOR EACH HOUR IN TWO FORMS:       OTH00050
C           1) CONTRIBUTIONS FROM SIGNIFICANT SOURCES, AND              OTH00060
C           2) SUMMARIES.                                               OTH00070
C                                                                       OTH00080
C I/O:  NONE                                                            OTH00090
C                                                                       OTH00100
C CALLED BY: HRCON                                                      OTH00110
C                                                                       OTH00120
C CALLS:  NONE                                                          OTH00130
C                                                                       OTH00140
C       MINERALS MANAGEMENT SERVICE                                     OTH00150
C       U.S. DEPARTMENT OF THE INTERIOR                                 OTH00160
C                                                                       OTH00170
C OCD             REVISION HISTORY:                                     OTH00180
C    DCD 881026   CREATED.                                              OTH00190
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCOTH00200
                                                                        OTH00210
      include 'params.cmn'
      INCLUDE 'opt.cmn'                                                 OTH00220
      INCLUDE 'store.cmn'                                               OTH00240
                                                                        OTH00250
      character*4 IPOLT(5),ipolu                                        OTH00260
      DATA IPOLT /'SO2 ','TSP ','NOX ','CO  ','    '/                   OTH00270
      IPOLU=IPOLT(IPOL-2)                                               OTH00280
                                                                        OTH00290
C  WRITE HRLY SIGNIGICANT SOURCE CONTRIBUTIONS                          OTH00300
      if (iopt(11).eq.0) CALL HRCNTR(IPOLU)
                                                                        OTH00320
C  WRITE HOURLY SUMMARY TABLE.                                          OTH00330
      if (iopt(14).eq.1) return
      WRITE (IO,350) pb,LINE1,LINE2,LINE3
      WRITE (IO,360) IPOLU,IDATE,LH
      WRITE (IO,500)                                                    OTH00340
C        CALCULATE GRAND TOTALS AND RANK CONCENTRATIONS                 OTH00350
C        HSAV IS USED AS A DUMMY VARIABLE FOR THE REMAINDER OF THIS     OTH00360
C        SUBROUTINE. IT IS ZEROED AGAIN IN PTR BEFORE ITS NORMAL USE.   OTH00370
      DO 130 K=1,NRECEP                                                 OTH00380
         HSAV(K)=PHCHI(K)                                               OTH00390
130   CONTINUE                                                          OTH00400
C        DETERMINE RANKING ACCORDING TO CONCENTRATION                   OTH00410
      DO 150 I=1,NRECEP                                                 OTH00420
         CMAX=-1.0                                                      OTH00430
         DO 140 K=1,NRECEP                                              OTH00440
            IF (HSAV(K).GT.CMAX) THEN                                   OTH00450
               CMAX=HSAV(K)                                             OTH00460
               LMAX=K                                                   OTH00470
            ENDIF                                                       OTH00480
140      CONTINUE                                                       OTH00490
         IRANK(LMAX)=I                                                  OTH00500
         HSAV(LMAX)=-1.0                                                OTH00510
150   CONTINUE                                                          OTH00520
                                                                        OTH00530
C   WRITE INFORMATION FOR INTERNALLY-GENERATED RECEPTORS USING          OTH00540
C   SPECIAL FORMAT                                                      OTH00550
      IF(JAR.NE.0) THEN                                                 OTH00560
         DO 155 K=1,JAR                                                 OTH00570
            WRITE (IO,510) K,STAR(1,K),STAR(2,K),RNAME(K),              OTH00580
     &      RREC(K),SREC(K),ZR(K),ELR(K),PHSIGS(K,26),PHCHI(K),IRANK(K) OTH00590
155      CONTINUE                                                       OTH00600
      ENDIF                                                             OTH00610
      IF(JAR.NE.NRECEP) THEN                                            OTH00620
         DO 165 K=JAR+1,NRECEP                                          OTH00630
            WRITE (IO,515) K,STAR(1,K),STAR(2,K),RNAME(K),              OTH00640
     &      RREC(K),SREC(K),ZR(K),ELR(K),PHSIGS(K,26),PHCHI(K),IRANK(K) OTH00650
165      CONTINUE                                                       OTH00660
      ENDIF                                                             OTH00670
      RETURN                                                            OTH00680
                                                                        OTH00690
350   FORMAT ( a1,A80/1X,A80/1X,A80)
360   FORMAT( /,T30,A4,' SUMMARY CONCENTRATION TABLE (MICROGRAMS/M**3) '
     &,5X,I2,'/',I4,' : HOUR ',I2//)
500   FORMAT ( / ,T7,'RECEPTOR',T23,'EAST',T33,'NORTH',T43,'RECEPTOR HT'OTH00700
     &,T61,'RECEPTOR',T78,'TOTAL FROM',T93,'TOTAL FROM',T106,'CONCENTRATOTH00710
     &ION'/' ',T7,'NO. NAME',T22,'COORD',T33,'COORD',T44,'ABV GRD (M)',TOTH00720
     &59,'GRD-LVL ELEV',T77,'SIGNIF POINT',T93,'ALL SOURCES',T111,'RANK'OTH00730
     &/' ',T58,'(USER HT UNITS)',T80,'SOURCES'//)                       OTH00740
510   FORMAT (' ',I8,2A1,2X,A8,2F10.2,F12.1,F20.1,6P2F15.4,I15)         OTH00750
515   FORMAT (' ',I8,2A1,2X,A8,2F10.2,F12.1,F20.1,6P2F15.4,I15)         OTH00760
                                                                        OTH00770
      END                                                               OTH00780
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCPAR00010
      SUBROUTINE PARTIAL( J,P,F,HLP,HPRM,HBEL,HABV)                     PAR00020
C                                                                       PAR00030
C PURPOSE:  PARTIAL PLUME PENETRATION OF THE ELEVATED MARINE LAYER IS   PAR00040
C           CONSIDERED.                                                 PAR00050
C                                                                       PAR00060
C I/O:    J, SOURCE INDEX                                               PAR00070
C         P, FRACTION OF PLUME PENETRATION                              PAR00080
C         F, BUOYANCY FLUX                                              PAR00090
C      HPRM, STACK HEIGHT WHICH TAKES INTO ACCOUNT DOWNWASH             PAR00100
C       HLP, MIXING HEIGHT - HPRM                                       PAR00110
C      HBEL, EFFECTIVE PLUME HEIGHT BELOW THE MIXING LAYER              PAR00120
C      HABV, EFFECTIVE PLUME HEIGHT ABOVE THE MIXING LAYER              PAR00130
C                                                                       PAR00140
C CALLED BY: PTR                                                        PAR00150
C                                                                       PAR00160
C CALLS:  NONE                                                          PAR00170
C                                                                       PAR00180
C       MINERALS MANAGEMENT SERVICE                                     PAR00190
C       U.S. DEPARTMENT OF THE INTERIOR                                 PAR00200
C                                                                       PAR00210
C OCD             REVISION HISTORY:                                     PAR00220
C    DCD 880908   CREATED.                                              PAR00230
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCPAR00240
                                                                        PAR00250
      include 'params.cmn'
      INCLUDE 'src.cmn'                                                 PAR00260
      INCLUDE 'met.cmn'                                                 PAR00270
      INCLUDE 'store.cmn'                                               PAR00280
      INCLUDE 'const.cmn'                                               PAR00290
                                                                        PAR00300
C   IF THE PLUME RISE IS < 0.62*(MIX HT - STACK HT)                     PAR00310
C   THEN THE PLUME IS MODELED AS BEING COMPLETELY WITHIN THE MIXED LAYERPAR00320
C   OTHERWISE, BOUYANCY PLUME RISE IS RECOMPUTED ASSUMING AN ISOTHERMAL PAR00330
C   LAPSE RATE (DTHETADZ=0.01 C/M).  THE FRACTION OF THE PLUME WHICH    PAR00340
C   PENETRATES THE INVERSION IS THEN COMPUTED.  IF THE PLUME PARTIALLY  PAR00350
C   PENETRATES THEN TWO PLUMES WILL BE MODELED, ABOVE AND BELOW THE     PAR00360
C   MIXING HEIGHT.                                                      PAR00370
                                                                        PAR00380
                                                                        PAR00390
C   PENETRATION                                                         PAR00400
C   ASSUME ISOTHERMAL LAPSE RATE AND COMPUTE STABLE PLUME RISE          PAR00410
      S=GRAV/TEMP*0.01                                                  PAR00420
      DELHI=2.6*(F/(UPL*S))**0.3333                                     PAR00430
      UPLC=0.2746*(F*SQRT(S))**0.25                                     PAR00440
      IF(UPL.LT.UPLC) DELHI=4.0*F**0.25/(S**0.375)                      PAR00450
C                                                                       PAR00460
C   COMPUTE FRACTION OF PLUME PENETRATION                               PAR00470
      P=1.5-HLP/DELHI                                                   PAR00480
      IF(P.GE.1.) THEN                                                  PAR00490
C   COMPLETE PENETRATION                                                PAR00500
         H=DELHI+HPRM                                                   PAR00510
         HSAV(J)=H                                                      PAR00520
      ELSE                                                              PAR00530
         IF(P.LE.0.) THEN                                               PAR00540
C   NO PENETRATION                                                      PAR00550
            H=0.62*HLP+HPRM                                             PAR00560
            HSAV(J)=H                                                   PAR00570
C                                                                       PAR00580
C   PARTIAL PENETRATION                                                 PAR00590
C   COMPUTE EFFECTIVE PLUME HEIGHT OF BOTH PLUMES                       PAR00600
         ELSE                                                           PAR00610
            HBEL=HPRM+(0.62+0.38*P)*HLP                                 PAR00620
            HABV=HPRM+(1.+P)*HLP                                        PAR00630
         ENDIF                                                          PAR00640
      ENDIF                                                             PAR00650
                                                                        PAR00660
      RETURN                                                            PAR00670
      END                                                               PAR00680
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCPLM00010
      SUBROUTINE PLUME(J,ANGR2S,DL,DW,NTW,NTL,ISKIP,WLS,io,ierr)        PLM00020
C                                                                       PLM00030
C PURPOSE:  DETERMINES THE COORDINATES OF ALL TRANSITIONS FROM          PLM00040
C           WATER TO LAND AND LAND TO WATER ALONG THE PLUME PATH.       PLM00050
C                                                                       PLM00060
C I/O:  J       SOURCE INDEX                                            PLM00070
C       ANGR2S  ANGLE FROM SOURCE TO RECEPTOR OR WD                     PLM00080
C       WMIN    MINIMUM WIDTH FOR A WATER BODY OR LAND MASS TO BE       PLM00090
C               CONSIDERED SIGNIFICANT                                  PLM00100
C       DL      DISTANCE TO LAND TO WATER TRANSITION ALONG ANGR2S       PLM00110
C       DW      DISTANCE TO WATER TO LAND TRANSITION ALONG ANGR2S       PLM00120
C       NTL     NUMBER OF LAND TO WATER TRANSITIONS                     PLM00130
C       NTW     NUMBER OF WATER TO LAND TRANSITIONS                     PLM00140
C       ISKIP   FLAG TO SKIP TO NEXT SOURCE IF PLUME DOES NOT HIT GRID  PLM00150
C       WLS     LAND OR WATER AT SOURCE                                 PLM00160
c       IO      Unit number for output listing file
c       IERR    Unit number for error message file
C                                                                       PLM00170
C CALLED BY:  PTR                                                       PLM00180
C             DIST                                                      PLM00190
C                                                                       PLM00200
C CALLS:  NONE                                                          PLM00210
C                                                                       PLM00220
C       MINERALS MANAGEMENT SERVICE                                     PLM00230
C       U.S. DEPARTMENT OF THE INTERIOR                                 PLM00240
C                                                                       PLM00250
C OCD             REVISION HISTORY:                                     PLM00260
C    DCD 880919   CREATED.                                              PLM00270
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCPLM00280
                                                                        PLM00290
      include 'params.cmn'
      INCLUDE 'src.cmn'                                                 PLM00300
      INCLUDE 'shr.cmn'                                                 PLM00310
      INCLUDE 'const.cmn'                                               PLM00320
c     Declare the following variables with double precision to
c     provide sufficient precision.
      double precision xp,yp,delxs,delys,xdec,ydec,xinc,yinc
                                                                        PLM00330
      REAL XC(8),YC(8),DL(10),DW(10)                                    PLM00340
      character*1 wls,wl,wl0
      DATA MAXI/500/                                                    PLM00350
                                                                        PLM00360
      DO 4 I=1,10                                                       PLM00370
         DL(I)=0.                                                       PLM00380
         DW(I)=0.                                                       PLM00390
4     CONTINUE                                                          PLM00400
      ISKIP=0                                                           PLM00410
C                                                                       PLM00420
C   STEP 1: FOR OFF GRID SOURCES, DETERMINE IF PLUME WILL TRANSVERSE    PLM00430
C     THE GRID FOR A GIVEN WIND DIRECTION, IF NOT SKIP HOUR.            PLM00440
C                                                                       PLM00450
      XS = SOURCE(1,J)                                                  PLM00460
      YS = SOURCE(2,J)                                                  PLM00470
      DELXS = XS - X0                                                   PLM00480
      DELYS = Y0 - YS                                                   PLM00490
      IF(DELXS.GE.0..AND.DELXS.LT.0.0001) DELXS=0.0001*DELX             PLM00500
      IF(DELYS.GE.0..AND.DELYS.LT.0.0001) DELYS=0.0001*DELY             PLM00510
      LX = INT(DELXS/DELX + 0.9999)                                     PLM00520
      LY = INT(DELYS/DELY + 0.9999)                                     PLM00530
      IF(.NOT.(LX.GT.NX.OR.LX.LT.1.OR.LY.GT.NY.OR.LY.LT.1)) GO TO 99    PLM00540
                                                                        PLM00550
C  SOURCE IS NOT ON GRID. DETERMINE IF PLUME PATH INTERSECTS GRID       PLM00560
C                                                                       PLM00570
      DO 5 I=1,8                                                        PLM00580
         XC(I)=X0                                                       PLM00590
         YC(I)=Y0-NY*DELY                                               PLM00600
         IF(I.LE.2.OR.I.GE.7) XC(I)=X0+NX*DELX                          PLM00610
         IF(I.GE.5) YC(I)=Y0                                            PLM00620
5     CONTINUE                                                          PLM00630
C                                                                       PLM00640
C  DETERMINE LIMITS OF SECTOR OF INTERCEPTION                           PLM00650
C                                                                       PLM00660
      IF(XS.GT.XC(1)) THEN                                              PLM00670
         IF(YS.GT.Y0) THEN                                              PLM00680
            IZ=1                                                        PLM00690
            GO TO 90                                                    PLM00700
         ELSE IF(YS.LE.YC(1)) THEN                                      PLM00710
            IZ=3                                                        PLM00720
            GO TO 90                                                    PLM00730
         ELSE                                                           PLM00740
            IZ=2                                                        PLM00750
            GO TO 90                                                    PLM00760
         ENDIF                                                          PLM00770
      ENDIF                                                             PLM00780
C                                                                       PLM00790
      IF(XS.LE.X0) THEN                                                 PLM00800
         IF(YS.GT.Y0) THEN                                              PLM00810
            IZ=7                                                        PLM00820
            GO TO 90                                                    PLM00830
         ELSE IF(YS.LE.YC(1)) THEN                                      PLM00840
            IZ=5                                                        PLM00850
            GO TO 90                                                    PLM00860
         ELSE                                                           PLM00870
            IZ=6                                                        PLM00880
            GO TO 90                                                    PLM00890
         ENDIF                                                          PLM00900
      ENDIF                                                             PLM00910
C                                                                       PLM00920
      IF(YS.GE.Y0) THEN                                                 PLM00930
         IZ=8                                                           PLM00940
      ELSE                                                              PLM00950
         IZ=4                                                           PLM00960
      ENDIF                                                             PLM00970
                                                                        PLM00980
90    CONTINUE                                                          PLM00990
C                                                                       PLM01000
C  SECTOR IS DETERMINED BY TWO CORNERS OF GRID                          PLM01010
      XC1=XC(IZ)                                                        PLM01020
      YC1=YC(IZ)                                                        PLM01030
      IF(IZ.GT.3) THEN                                                  PLM01040
         XC2=XC(IZ-3)                                                   PLM01050
         YC2=YC(IZ-3)                                                   PLM01060
      ELSE                                                              PLM01070
         XC2=XC(IZ+5)                                                   PLM01080
         YC2=YC(IZ+5)                                                   PLM01090
      ENDIF                                                             PLM01100
C                                                                       PLM01110
C  COMPUTE SECTOR OF INTERCEPTION                                       PLM01120
      DX1=XC1-XS                                                        PLM01130
      DY1=YC1-YS                                                        PLM01140
      ANG1=180./PI*ATAN2(DX1,DY1)                                       PLM01150
      DX2=XC2-XS                                                        PLM01160
      DY2=YC2-YS                                                        PLM01170
      ANG2=180./PI*ATAN2(DX2,DY2)                                       PLM01180
      FV=ANGR2S-180.                                                    PLM01190
      IF(ANG1.LT.0.) ANG1=ANG1+360.                                     PLM01200
      IF(ANG2.LT.0.) ANG2=ANG2+360.                                     PLM01210
      IF(FV.LT.0.) FV=FV+360.                                           PLM01220
      IF(FV.LE.ANG2.AND.FV.GE.ANG1) GO TO 99                            PLM01230
      IF(IZ.EQ.4.AND.(FV.LE.ANG2.OR.FV.GE.ANG1)) GO TO 99               PLM01240
      ISKIP=1                                                           PLM01250
      RETURN                                                            PLM01260
99    CONTINUE                                                          PLM01270
C                                                                       PLM01280
C  STEP 2: PROCEED ALONG PLUME PATH FROM SOURCE AT                      PLM01290
C     INTERVALS OF 1 GRID LENGTH (THE SHORTER OF DELX AND DELY).        PLM01300
C     KEEP TRACK OF ALL TRANSITIONS FROM WATER TO LAND (MAX=10)         PLM01310
C     AND LAND TO WATER (MAX=10).                                       PLM01320
C                                                                       PLM01330
C  NOTE: THE LENGTH OF A OVERWATER OR OVERLAND PLUME TRAJECTORY MUST    PLM01340
C     BE GREATER THAN A USER SPECIFIED MINIMUM (WMIN) TO BE CONSIDERED  PLM01350
C     SIGNIFICANT.                                                      PLM01360
C                                                                       PLM01370
      NTW=0                                                             PLM01380
      NTL=0                                                             PLM01390
      NTRAN=0                                                           PLM01400
      MSW = 1                                                           PLM01410
      WL='0'                                                            PLM01420
      DINC = AMIN1(DELX,DELY)                                           PLM01430
      XINC = DINC*SIN(ANGR2S*PI/180.)                                   PLM01440
      YINC = DINC*COS(ANGR2S*PI/180.)                                   PLM01450
                                                                        PLM01460
C  CHECK IF SOURCE IS ON LAND, IF IT IS, CHECK UPWIND FOR SHORELINE     PLM01470
      INC=1                                                             PLM01480
      XP=XS                                                             PLM01490
      YP=YS                                                             PLM01500
      DELXS=XP-X0                                                       PLM01510
      DELYS=Y0-YP                                                       PLM01520
      LX=INT(DELXS/DELX+0.9999)                                         PLM01530
      LY=INT(DELYS/DELY+0.9999)                                         PLM01540
      IF(LX.GT.NX.OR.LX.LT.1.OR.LY.GT.NY.OR.LY.LT.1) GO TO 100          PLM01550
      WL=XYMAP(LY,LX)                                                   PLM01560
      WLS=WL                                                            PLM01570
      IF(WL.EQ.LETL) INC=-1                                             PLM01580
      MSW=0                                                             PLM01590
100   CONTINUE                                                          PLM01600
C                                                                       PLM01610
      DO 1000 I=1,MAXI                                                  PLM01620
         XP=XP-XINC*INC                                                 PLM01630
         YP=YP-YINC*INC                                                 PLM01640
         DELXS=XP-X0                                                    PLM01650
         DELYS=Y0-YP                                                    PLM01660
         LX=INT(DELXS/DELX+0.9999)                                      PLM01670
         LY=INT(DELYS/DELY+0.9999)                                      PLM01680
C                                                                       PLM01690
C  CHECK TO SEE IF ON GRID                                              PLM01700
         IF(LX.GT.NX.OR.LX.LT.1.OR.LY.GT.NY.OR.LY.LT.1) GO TO 900       PLM01710
         IF(MSW.EQ.1) WLS=XYMAP(LY,LX)                                  PLM01720
         MSW=0                                                          PLM01730
         WL0=WL                                                         PLM01740
         WL=XYMAP(LY,LX)                                                PLM01750
         IF(WL0.EQ.'0') GO TO 1000                                      PLM01760
         IF(WL.EQ.WL0) GO TO 1000                                       PLM01770
C  FOUND TRANSITION                                                     PLM01780
         NTRAN=NTRAN+1                                                  PLM01790
C  PINPOINT TRANSITION TO A TENTH OF A GRID LENGTH                      PLM01800
         XDEC=XINC/10.                                                  PLM01810
         YDEC=YINC/10.                                                  PLM01820
200      XP=XP+XDEC*INC                                                 PLM01830
         YP=YP+YDEC*INC                                                 PLM01840
         DELXS=XP-X0                                                    PLM01850
         DELYS=Y0-YP                                                    PLM01860
         LX=INT(DELXS/DELX+0.9999)                                      PLM01870
         LY=INT(DELYS/DELY+0.9999)                                      PLM01880
         if(lx.gt.nx.or.lx.lt.1.or.ly.gt.ny.or.ly.lt.1) then
            write (io,11)
            write (ierr,11)
11          format (' Error in subroutine PLUME:',/,
     &              ' This point should never be reached.  Contact',
     &              ' model developer for assistance.')
            stop 'Error encountered.  See ERROR.OUT for more details.'
         end if
         IF(XYMAP(LY,LX).EQ.WL) GO TO 200                               PLM01890
C                                                                       PLM01900
C  WATER TO LAND TRANSITION                                             PLM01910
         IF(WL.EQ.LETW.AND.INC.EQ.1) GO TO 300                          PLM01920
         IF(WL.EQ.LETL.AND.INC.EQ.-1) GO TO 300                         PLM01930
         NTW=NTW+1                                                      PLM01940
         DW(NTW)=SQRT((XP-XS)**2+(YP-YS)**2)*INC                        PLM01950
         IF(NTRAN.EQ.1 .or. ntw.le.0 .or. ntl.le.0) GO TO 1000          PLM01960
C                                                                       PLM01970
C  OVERWATER OR OVERLAND TRAJECTORY MUST BE LONGER THAN USER SPECIFIED  PLM01980
C   MINIMUM TO BE CONSIDERED                                            PLM01990
250      WDIST=ABS(DW(NTW)-DL(NTL))                                     PLM02000
         IF(WDIST.GE.WMIN) GO TO 400                                    PLM02010
         NTRAN=NTRAN-2                                                  PLM02020
         NTW=NTW-1                                                      PLM02030
         NTL=NTL-1                                                      PLM02040
         GO TO 1000                                                     PLM02050
C                                                                       PLM02060
C  LAND TO WATER TRANSITION                                             PLM02070
300      NTL=NTL+1                                                      PLM02080
         DL(NTL)=SQRT((XP-XS)**2+(YP-YS)**2)*INC                        PLM02090
         if (ntw.gt.0 .and. ntl.gt.0) GO TO 250                         PLM02100
C                                                                       PLM02110
400      IF(INC.NE.1) THEN                                              PLM02120
C  FOUND SHORELINE UPWIND OF LAND SOURCE                                PLM02130
C   NEXT PROCEED DOWNWIND ALONG PLUME PATH FROM SOURCE                  PLM02140
            XP=XS                                                       PLM02150
            YP=YS                                                       PLM02160
            WL=LETL                                                     PLM02170
            INC=1                                                       PLM02180
            NTL=0                                                       PLM02190
            GO TO 1000                                                  PLM02200
         ENDIF                                                          PLM02210
C                                                                       PLM02220
C  HIT EDGE OF GRID                                                     PLM02230
900      IF(INC.EQ.-1) GO TO 400                                        PLM02240
         IF(MSW.EQ.0) RETURN                                            PLM02250
1000  CONTINUE                                                          PLM02260
C                                                                       PLM02270
      RETURN                                                            PLM02280
      END                                                               PLM02290
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCPRI00010
      SUBROUTINE PRISE( A,DISTF,HPRM,F,IRISE,DHF,CC,EFFHT)              PRI00020
C                                                                       PRI00030
C PURPOSE:  PLUME RISE CALCULATION                                      PRI00040
C                                                                       PRI00050
C I/O:  A, RATIO A = EFF STACK HT/BLDG HT                               PRI00060
C   DISTF, DISTANCE TO FINAL RISE (M)                                   PRI00070
C    HPRM, STACK HEIGHT WHICH TAKES INTO ACCOUNT DOWNWASH               PRI00080
C       F, BUOYANCY FLUX                                                PRI00090
C   IRISE, RISE CODE                                                    PRI00100
C     DHF, FINAL RISE                                                   PRI00110
C      CC, DHF**3                                                       PRI00120
C   EFFHT, EFFECTIVE STACK HEIGHT                                       PRI00130
C                                                                       PRI00140
C CALLED BY:  PTR                                                       PRI00150
C                                                                       PRI00160
C CALLS:  NONE                                                          PRI00170
C                                                                       PRI00180
C       MINERALS MANAGEMENT SERVICE                                     PRI00190
C       U.S. DEPARTMENT OF THE INTERIOR                                 PRI00200
C                                                                       PRI00210
C OCD             REVISION HISTORY:                                     PRI00220
C    DCD 880907   CREATED.                                              PRI00230
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCPRI00240
                                                                        PRI00250
      include 'params.cmn'
      INCLUDE 'hrdat.cmn'                                               PRI00260
      INCLUDE 'opt.cmn'                                                 PRI00270
      INCLUDE 'met.cmn'                                                 PRI00280
      INCLUDE 'src.cmn'                                                 PRI00290
      INCLUDE 'store.cmn'                                               PRI00300
      INCLUDE 'ptrrcp.cmn'                                              PRI00310
      INCLUDE 'const.cmn'                                               PRI00320
                                                                        PRI00330
      ZST = THT + EP*CELM                                               PRI00340
      IF(ILAND.EQ.1) ZST = THT                                          PRI00350
      S = GRAV/TEMP * WDTHDZ                                            PRI00360
      IF(S.LT.0.0) S = 0.0                                              PRI00370
      SQRTS = SQRT(S)                                                   PRI00380
      ZDL = ZST/EL                                                      PRI00390
      IF(ZDL.LT.0.0) THEN                                               PRI00400
         X = (1.0 - 15.0*ZDL)**0.25                                     PRI00410
         PSIU = 2.0 * ALOG(0.5*(1.0+X)) + ALOG(0.5*(1.0+X*X)) -         PRI00420
     &          2.0 * ATAN(X) + 1.5707963                               PRI00430
      ELSE                                                              PRI00440
         PSIU = -4.7 * ZDL                                              PRI00450
      ENDIF                                                             PRI00460
      UPL = USTAR/0.4 * (ALOG(ZST/Z0W) - PSIU)                          PRI00470
      IF(UPL.LT.1.0) UPL = 1.0                                          PRI00480
C       TEMP- THE AMBIENT AIR TEMPERATURE FOR THIS HOUR                 PRI00490
      DELT=TS-TEMP                                                      PRI00500
      F=BUOY*DELT/TS                                                    PRI00510
      IF(F.LT.0.0) F = 0.0                                              PRI00520
C        CALCULATE  H PRIME WHICH TAKES INTO ACCOUNT STACK DOWNWASH     PRI00530
C        BRIGGS(1973) PAGE 4                                            PRI00540
      HPRM=ZST                                                          PRI00550
C        IF IOPT(2)=1, THEN NO STACK DOWNWASH COMPUTATION               PRI00560
      IF (IOPT(2).EQ.0) THEN                                            PRI00570
         DUM=VS/UPL                                                     PRI00580
         IF (DUM.LT.1.5) HPRM=HPRM+2.*D*(DUM-1.5)                       PRI00590
         IF (HPRM.LT.0.) HPRM=0.                                        PRI00600
      ENDIF                                                             PRI00610
C                                                                       PRI00620
C       COMPUTE MOMENTUM RISE AND EFFECTIVE STACK HEIGHT.  ANGLE OF     PRI00630
C       FROM VERTICAL IS USED IN COMPUTATION OF EFFECTIVE STACK HEIGHT. PRI00640
C       MOMENTUM RISE IS COMPUTED AT 2 BUILDING HEIGHTS DOWNWIND AS IN  PRI00650
C       THE ISC MODEL.                                                  PRI00660
C                                                                       PRI00670
      FM = TEMP/TS * VS*VS*D*D/4.0                                      PRI00680
      DELHM = 0.0                                                       PRI00690
      IF(VS.GT.0.0) THEN                                                PRI00700
         BETAM = 0.3333 + UPL/VS                                        PRI00710
         IF(S.LE.0.0) THEN                                              PRI00720
            DELHM = (6.0*FM*HB/(BETAM*BETAM*UPL*UPL))**0.3333           PRI00730
         ELSE                                                           PRI00740
            DELHM = (3.0 * FM * SIN(AMIN1((SQRTS * 2.0*HB/UPL),1.5708))/PRI00750
     &              (BETAM*BETAM * UPL * SQRTS))**0.3333                PRI00760
         ENDIF                                                          PRI00770
      ENDIF                                                             PRI00780
      EFFHT =  THT + DELHM*COMPON                                       PRI00790
      A = AMAX1(1.0,ABS(EFFHT/HB))                                      PRI00800
C                                                                       PRI00810
C       COMPUTE PLUME RISE DUE TO MOMENTUM OR BUOYANCY, MODIFIED BY     PRI00820
C       DOWNWASH EFFECTS.  HERE, COMPUTE FINAL RISE.  IF TRANSITIONAL   PRI00830
C       RISE IS NEEDED, IT IS CALCULATED WITHIN THE RECEPTOR LOOP.      PRI00840
C                                                                       PRI00850
C       NEUTRAL/UNSTABLE CONDITIONS:                                    PRI00860
C       DETERMINE WHETHER MOMENTUM OR BUOYANCY RISE DOMINATES           PRI00870
C                                                                       PRI00880
      IF(VS.LE.0.0 .OR. COMPON.LE.0.0) GO TO 1000                       PRI00890
      IF(F.LT.55.) THEN                                                 PRI00900
         DELTC=0.0297*TS*VS**0.3333*COMPON**1.3333/D**0.6667            PRI00910
      ELSE                                                              PRI00920
         DELTC=.00575*TS*VS**0.6667*COMPON**1.6667/D**0.3333            PRI00930
      ENDIF                                                             PRI00940
      IF(DELT.GE.DELTC) GO TO 1000                                      PRI00950
C                                                                       PRI00960
C       MOMENTUM RISE DOMINATES:                                        PRI00970
C       FINAL MOMENTUM RISE OCCURS CLOSE TO THE STACK: 3*D*VS/UPL*COMPONPRI00980
C                                                                       PRI00990
      DHF = 3.0 * D * VS/UPL * COMPON                                   PRI01000
C                                                                       PRI01010
C       IRISE CODES: 0 = MOMENTUM RISE, NEUTRAL FORMULA IS USED.        PRI01020
C                    1 = BUOYANCY RISE, NEUTRAL FORMULA IS USED.        PRI01030
C                    2 = MOMENTUM RISE, STABLE FORMULA IS USED.         PRI01040
C                    3 = BUOYANCY RISE, STABLE FORMULA IS USED.         PRI01050
C                                                                       PRI01060
      IRISE = 0                                                         PRI01070
      GO TO 1050                                                        PRI01080
C                                                                       PRI01090
C       BUOYANCY RISE DOMINATES:                                        PRI01100
C       UNSTABLE OR NEUTRAL RISE, DISTANCE TO FINAL RISE IN METERS      PRI01110
C                                                                       PRI01120
1000  IF(F.LT.55.0) THEN                                                PRI01130
         DISTF = 49.0 * F**0.625                                        PRI01140
      ELSE                                                              PRI01150
         DISTF = 119.0 * F**0.4                                         PRI01160
      ENDIF                                                             PRI01170
      DHF =  1.6/UPL * (F*DISTF*DISTF)**0.3333                          PRI01180
C                                                                       PRI01190
C       IF STACK POINTS DOWN, ADD THE NEGATIVE MOMENTUM RISE            PRI01200
      IF(COMPON.LT.0.0) DHF = DHF + 3.0 * D * VS/UPL * COMPON           PRI01210
      IRISE = 1                                                         PRI01220
C                                                                       PRI01230
C       SET UP COEFFICIENTS FOR CUBIC EQUATION:                         PRI01240
C       Z**3 + AC*Z**2 + BC*Z + CC = 0                                  PRI01250
1050  CC = -DHF * DHF * DHF                                             PRI01260
      IF(WDTHDZ.LE.0.0) RETURN                                          PRI01270
C                                                                       PRI01280
C       STABLE RISE, DISTANCE TO FINAL RISE IN METERS                   PRI01290
C                                                                       PRI01300
C       COMPUTE STABLE PLUME RISE; USE THE LESSER OF STABLE AND         PRI01310
C       NEUTRAL TOTAL PLUME RISE IF WDTHDZ IS POSITIVE.  FIRST, SAVE    PRI01320
C       COMPUTATIONS FROM NEUTRAL RISE CALCULATIONS                     PRI01330
      DHFN = DHF                                                        PRI01340
      CCN = CC                                                          PRI01350
      DISTFN = DISTF                                                    PRI01360
      IRISEN = IRISE                                                    PRI01370
C                                                                       PRI01380
      IF(VS.LE.0.0 .OR. COMPON.LE.0.0) GO TO 1100                       PRI01390
      DELTC = 0.01958 * VS*TEMP*SQRTS*COMPON*COMPON*COMPON              PRI01400
      IF(DELT.LE.DELTC) THEN                                            PRI01410
C                                                                       PRI01420
C       IN STABLE CONDITIONS, MOMENTUM RISE DOMINATES.                  PRI01430
         DHF = 1.5*(VS*VS*D*D*TEMP/(4.*TS*UPL))**.3333/S**0.1667*COMPON PRI01440
         IRISE = 2                                                      PRI01450
         DISTF = 2.07 * UPL/SQRTS                                       PRI01460
         GO TO 1150                                                     PRI01470
      ENDIF                                                             PRI01480
C                                                                       PRI01490
C       IN STABLE CONDITIONS, BUOYANCY RISE DOMINATES.                  PRI01500
1100  DHF  =  2.6 * (F/(UPL * S))**0.3333                               PRI01510
      DISTF = 2.07 * UPL/SQRTS                                          PRI01520
C                                                                       PRI01530
C       CHECK WIND SPEED TO SEE IF "CALM" CONDITIONS EXIST              PRI01540
      UPLC = 0.2746 * (F*SQRTS)**0.25                                   PRI01550
      IF(UPL.LT.UPLC) DHF = 4.0 * F**0.25/(S**0.375)                    PRI01560
C                                                                       PRI01570
C       IF STACK POINTS DOWN, ADD THE NEGATIVE MOMENTUM RISE            PRI01580
      IF(COMPON.LT.0.0) DHF = DHF +                                     PRI01590
     &      1.5*(VS*VS*D*D*TEMP/(4.*TS*UPL))**.3333/S**0.1667*COMPON    PRI01600
      IRISE = 3                                                         PRI01610
1150  CC = -DHF * DHF * DHF                                             PRI01620
C                                                                       PRI01630
C       CHOOSE THE LESSER OF STABLE AND NEUTRAL RISE                    PRI01640
      IF(CC.LE.CCN) THEN                                                PRI01650
         CC = CCN                                                       PRI01660
         DISTF = DISTFN                                                 PRI01670
         DHF = DHFN                                                     PRI01680
         IRISE = IRISEN                                                 PRI01690
      ENDIF                                                             PRI01700
                                                                        PRI01710
      RETURN                                                            PRI01720
      END                                                               PRI01730
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCPRO00010
      SUBROUTINE PROFILE (JHR,Z0W,EL,THSTAR,USTAR)                      PRO00020
C                                                                       PRO00030
C PURPOSE:  COMPUTE VALUES OF CRITICAL PROFILE VARIABLES: L AND USTAR   PRO00040
C                                                                       PRO00050
C I/O:  JHR, HOUR                                                       PRO00060
C       Z0W, OVERWATER ROUGHNESS LENGTH                                 PRO00070
C        EL, MONIN OBUKHOV LENGTH                                       PRO00080
C    THSTAR, DIMENSIONLESS TEMPERATURE (THETA STAR)                     PRO00090
C     USTAR, FRICTION VELOCITY                                          PRO00100
C                                                                       PRO00110
C CALLED BY:  ADDMET                                                    PRO00120
C                                                                       PRO00130
C CALLS:  NONE                                                          PRO00140
C                                                                       PRO00150
C       MINERALS MANAGEMENT SERVICE                                     PRO00160
C       U.S. DEPARTMENT OF THE INTERIOR                                 PRO00170
C                                                                       PRO00180
C OCD             REVISION HISTORY:                                     PRO00190
C    DCD 880902   CREATED.                                              PRO00200
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCPRO00210
                                                                        PRO00220
      include 'params.cmn'
      INCLUDE 'store.cmn'                                               PRO00230
      INCLUDE 'opt.cmn'                                                 PRO00240
                                                                        PRO00250
      DATA PMB/1000./                                                   PRO00260
      DATA A0/6.107799961/,A1/4.436518521E-1/,A2/1.428945805E-2/        PRO00270
      DATA A3/2.650648471E-4/,A4/3.031240396E-6/,A5/2.034080948E-8/     PRO00280
      DATA A6/6.136820929E-11/                                          PRO00290
                                                                        PRO00300
C       APPROXIMATE 10-M WIND SPEED AND (10-M POT. TEMP. MINUS WATER    PRO00310
C       SURFACE TEMPERATURE) FROM MEASUREMENTS AT GIVEN HEIGHTS         PRO00320
C       ASSUMING NEUTRAL STABILITY THEN COMPUTE L, Z0, U*, AND THETA*.  PRO00330
C       THEN ITERATE ONCE TO IMPROVE STABILITY DEPENDENCE.              PRO00340
                                                                        PRO00350
C  FUNCTION FOR ES (SATURATION MIXING RATIO)                            PRO00360
      ES(T) = A0+T*(A1+T*(A2+T*(A3+T*(A4+T*(A5+T*A6)))))                PRO00370
                                                                        PRO00380
      UZ = WU(JHR)                                                      PRO00390
      WTHA = WTAIR(JHR) + HWT*0.0098                                    PRO00400
      RH = WRH(JHR)                                                     PRO00410
      E = RH*ES(WTHA-273.15)/100.                                       PRO00420
      Q = 0.622*E/(PMB-E)                                               PRO00430
      WTHVA = WTHA*(1.+0.61*Q)                                          PRO00440
                                                                        PRO00450
C     ASSUME RH AT WATER SURFACE IS 100                                 PRO00460
C                                                                       PRO00470
      WTHS = WTAIR(JHR) - WTDIFF(JHR)                                   PRO00480
      E = ES(WTHS-273.15)                                               PRO00490
      QS = 0.622*E/(PMB-E)                                              PRO00500
      WTHVS = WTHS*(1.+0.61*QS)                                         PRO00510
      SCALEU = 11.51/(ALOG(HWANE) + 9.21)                               PRO00520
      SCALET =  11.51/(ALOG(HWT) + 9.21)                                PRO00530
      Z0 = 1.0E-4                                                       PRO00540
      DTHV = (WTHVA - WTHVS) * SCALET                                   PRO00550
      WTHV10 = WTHVS + DTHV                                             PRO00560
      U10 = UZ * SCALEU                                                 PRO00570
C                                                                       PRO00580
C     CALCULATE DRAG COEFFICIENT CUN WITH NEUTRAL CONDITION ASSUMPTION  PRO00590
C            -- GARRATT (1977)                                          PRO00600
C                                                                       PRO00610
      IT=0                                                              PRO00620
8500  CUN=7.5E-4+6.7E-5*U10                                             PRO00630
C                                                                       PRO00640
      EL=9999.                                                          PRO00650
      IF(ABS(DTHV).GT.1.0E-6) EL = WTHV10*CUN**1.5*U10*U10/(5.096E-3    PRO00660
     &    *DTHV)                                                        PRO00670
      IF(EL.LE.5.0.AND.EL.GE.-5.0) THEN                                 PRO00680
         IF(EL.LT.0.0) EL = -5.0                                        PRO00690
         IF(EL.GT.0.0) EL = 5.0                                         PRO00700
      ENDIF                                                             PRO00710
C                                                                       PRO00720
      ZDL = HWANE/EL                                                    PRO00730
      ZTL = HWT/EL                                                      PRO00740
      IF(ZDL.LT.0.0) THEN                                               PRO00750
C  WIND SPEED                                                           PRO00760
         X = (1.0 - 15.0*ZDL)**0.25                                     PRO00770
         PSIU = 2.0 * ALOG(0.5*(1.0+X)) + ALOG(0.5*(1.0+X*X)) -         PRO00780
     &     2.0*ATAN(X) + 1.5707963                                      PRO00790
C  POTENTIAL TEMPERATURE                                                PRO00800
         Y=0.74*SQRT(1.-9.*ZTL)                                         PRO00810
         PSIT=2.*ALOG((1+Y)/2.)                                         PRO00820
      ELSE                                                              PRO00830
         PSIU = -4.7*ZDL                                                PRO00840
         PSIT = -6.5*ZTL                                                PRO00850
      ENDIF                                                             PRO00860
                                                                        PRO00870
      Z0 = 0.000002 * U10**2.5                                          PRO00880
      USTAR = 0.4*UZ/(ALOG(HWANE/Z0) - PSIU)                            PRO00890
      THSTAR = 0.4*(WTHVA-WTHVS)/(0.74*ALOG(HWT/Z0) - PSIT)             PRO00900
      Z0W = Z0                                                          PRO00910
                                                                        PRO00920
C  DO ONE ITERATION ON L AND PROFILE EQUATIONS                          PRO00930
      IT=IT+1                                                           PRO00940
      IF(IT.GT.1) RETURN                                                PRO00950
      Z10L = 10./EL                                                     PRO00960
      IF(Z10L.LT.0.0) THEN                                              PRO00970
C  WIND SPEED                                                           PRO00980
         X = (1.0 - 15.0*Z10L)**0.25                                    PRO00990
         PSI10U = 2.0 * ALOG(0.5*(1.0+X)) + ALOG(0.5*(1.0+X*X)) -       PRO01000
     &     2.0*ATAN(X) + 1.5707963                                      PRO01010
C  POTENTIAL TEMPERATURE                                                PRO01020
         Y=0.74*SQRT(1.-9.*Z10L)                                        PRO01030
         PSI10T=2.*ALOG((1+Y)/2.)                                       PRO01040
      ELSE                                                              PRO01050
         PSI10U=-4.7*Z10L                                               PRO01060
         PSI10T=-6.5*Z10L                                               PRO01070
      ENDIF                                                             PRO01080
                                                                        PRO01090
      U10=USTAR/0.4*(ALOG(10./Z0)-PSI10U)                               PRO01100
      WTHV10=THSTAR/0.4*(0.74*ALOG(10./Z0)-PSI10T)+WTHVS                PRO01110
      GO TO 8500                                                        PRO01120
                                                                        PRO01130
      END                                                               PRO01140
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCPTR00010
      SUBROUTINE PTR( NPER)                                             PTR00020
C                                                                       PTR00030
C PURPOSE:  THIS ROUTINE CONTAINS BOTH SOURCE AND RECEPTOR LOOPS.       PTR00040
C           COMPUTES PLUME RISE, BUILDING DOWNWASH, AND PARTIAL PLUME   PTR00050
C           PENETRATION.                                                PTR00060
C                                                                       PTR00070
C I/O:  NPER, NUMBER OF AVERAGING PERIODS                               PTR00080
C                                                                       PTR00090
C CALLED BY: HRCON                                                      PTR00100
C                                                                       PTR00110
C CALLS:  PRISE                                                         PTR00120
C         PLUME                                                         PTR00130
C         CUBIC                                                         PTR00140
C         PARTIAL                                                       PTR00150
C         IYIZ                                                          PTR00160
C         RECEP                                                         PTR00170
C         DOWNWASH                                                      PTR00180
C                                                                       PTR00190
C       MINERALS MANAGEMENT SERVICE                                     PTR00200
C       U.S. DEPARTMENT OF THE INTERIOR                                 PTR00210
C                                                                       PTR00220
C OCD             REVISION HISTORY:                                     PTR00230
C    DCD 880907   CREATED.                                              PTR00240
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCPTR00250
                                                                        PTR00260
      include 'params.cmn'
      INCLUDE 'hrdat.cmn'                                               PTR00270
      INCLUDE 'opt.cmn'                                                 PTR00280
      INCLUDE 'met.cmn'                                                 PTR00300
      INCLUDE 'src.cmn'                                                 PTR00310
      INCLUDE 'store.cmn'                                               PTR00320
      INCLUDE 'shr.cmn'                                                 PTR00330
      INCLUDE 'ptrrcp.cmn'                                              PTR00340
      INCLUDE 'const.cmn'                                               PTR00350
      INCLUDE 'linesrc.cmn'                                             PTR00360
      character*1 wls
                                                                        PTR00370
      REAL IZWSB,IYWSB,IZWSA,IYWSA,DW(10),DL(10)                        PTR00380
C                                                                       PTR00390
      IHR=LH                                                            PTR00400
      KWIST=KWST(LH)                                                    PTR00410
      IYW=WIY(LH)                                                       PTR00420
      IZW=WIZ(LH)                                                       PTR00430
      IYL=QIY(LH)                                                       PTR00440
      IZL=QIZ(LH)                                                       PTR00450
      WDTHDZ=WDTDZ(LH)                                                  PTR00460
      EL=WEL(LH)                                                        PTR00470
      Z0W=WZ0(LH)                                                       PTR00480
      USTAR=WUSTAR(LH)                                                  PTR00490
      WTDIF=WTDIFF(LH)                                                  PTR00500
      WDIR = QTHETA(LH)                                                 PTR00510
C                                                                       PTR00520
C    PRINT PAGE HEADER IF IOPT(12,13,15,OR 16) IS ZERO                  PTR00530
      IF(IOPT(12).EQ.0.OR.IOPT(13).EQ.0.OR.IOPT(15).EQ.0.OR.            PTR00540
     &   IOPT(16).EQ.0) THEN                                            PTR00550
         IF(IOPT(20).EQ.2) THEN                                         PTR00560
            TIME = (FLOAT(LH)/FLOAT(NSEGS))*FLOAT(NPER)                 PTR00570
            WRITE(IO,101) pb,LINE1,LINE2,LINE3,IDATE,TIME               PTR00580
         ELSE                                                           PTR00590
            WRITE(IO,1) pb,LINE1,LINE2,LINE3,IDATE,LH                   PTR00600
         ENDIF                                                          PTR00610
      ENDIF                                                             PTR00620
C                                                                       PTR00630
C    PRINT METEOROLOGY FOR THIS HOUR IF IOPT(12) OR IOPT(15) IS ZERO.   PTR00640
      IF(IOPT(12).EQ.0.OR.IOPT(15).EQ.0) THEN                           PTR00650
         ITL = QTEMP(LH) + 0.5                                          PTR00660
         ITW = TEMP + 0.5                                               PTR00670
         WRITE(IO,2) HWANE                                              PTR00680
         WRITE(IO,3) WDIR,U,HL,KWIST,KST,ITW,ITL,WTDIFF(LH),WRH(LH),    PTR00690
     &               WSH(LH),IYW,IZW,IYL,IZL,WWDTHDZ,EL,ELLAND,Z0W,USTARPTR00700
      ENDIF                                                             PTR00710
      IF(IOPT(13).EQ.0.OR.IOPT(16).EQ.0) WRITE(IO,5)                    PTR00720
C                                                                       PTR00730
C    IN THE OCD MODEL, THE SOURCE LOOP IS OUTSIDE THE RECEPTOR LOOP     PTR00740
      DO 5000 J = 1,NPT                                                 PTR00750
      JS = J  ! Save source index and pass it via common block
C          RQ - EAST COORDINATE OF THE SOURCE                           PTR00760
         RQ=SOURCE(1,J)                                                 PTR00770
C          SQ - NORTH COORDINATE OF THE SOURCE                          PTR00780
         SQ=SOURCE(2,J)                                                 PTR00790
C          ELP - SOURCE GROUND LEVEL ELEVATION                          PTR00800
         EP=ELP(J)                                                      PTR00810
C         MODIFY WIND SPEED BY BULK AERODYNAMIC PROFILE EQN IN ORDER TO PTR00820
C        TAKE INTO ACCOUNT THE INCREASE OF WIND SPEED WITH HEIGHT.      PTR00830
C        ASSUME WIND MEASUREMENTS ARE REPRESENTATIVE FOR HEIGHT = HWANE.PTR00840
C         THT IS THE PHYSICAL STACK LENGTH                              PTR00850
         THT=SOURCE(5,J)                                                PTR00860
C        POINT SOURCE HEIGHT NOT ALLOWED TO BE LESS THAN 0.1 METER.     PTR00870
         IF (THT.LT.0.1) THT=0.1                                        PTR00880
         COMPON = COS(SOURCE(9,J)*DG2RAD)                               PTR00890
         VS=SOURCE(8,J)                                                 PTR00900
         BUOY=SOURCE(10,J)                                              PTR00910
         TS=SOURCE(6,J)                                                 PTR00920
         D = SOURCE(7,J)                                                PTR00930
         HB = SOURCE(4,J)                                               PTR00940
         WIDTHB = SOURCE(11,J)                                          PTR00950
C                                                                       PTR00960
C       BUILDING HEIGHT NOT ALLOWED TO BE LESS THAN 0.1 METER.          PTR00970
         IF(HB.LT.0.1) HB = 0.1                                         PTR00980
C                                                                       PTR00990
C   CALL SUBROUTINE TO FIND ALL SHORELINES ALONG PLUME PATH             PTR01000
         CALL PLUME(J,QTHETA(LH),DL,DW,NTW,NTL,ISKIP,WLS,io,ierr)       PTR01010
C                                                                       PTR01020
C   IF PLUME DOES NOT ENTER GRID (ISKIP=1) SKIP TO NEXT SOURCE          PTR01030
         IF(ISKIP.EQ.1) GO TO 5000                                      PTR01040
C                                                                       PTR01050
C       DETERMINE WHETHER SOURCE IS OVER LAND OR WATER                  PTR01060
         ILAND = 0                                                      PTR01070
         IF(WLS.EQ.LETL) ILAND = 1                                      PTR01080
C                                                                       PTR01090
C  CALCULATE PLUME RISE                                                 PTR01100
         CALL PRISE( A,DISTF,HPRM,F,IRISE,DHF,CC,EFFHT)                 PTR01110
C                                                                       PTR01120
C       BUILDING DOWNWASH                                               PTR01130
C   USE PETERSON'S FORMULA AT X = 2.2*HB                                PTR01140
        IF(WIDTHB .GT. 0.0.AND.HB.GT.0.0) THEN                          PTR01150
          XX = 2.2*HB/KM2M                                              PTR01160
          CALL DOWNWASH(XX,HB,WIDTHB,HE,SZ0,DUM)                        PTR01170
        ELSE                                                            PTR01180
          SZ0 = 0.0                                                     PTR01190
        ENDIF                                                           PTR01200
C   SQRT(2/PI) = .79788                                                 PTR01210
        RZ0 = SZ0/.79788                                                PTR01220
        AC = 3.3333 * RZ0                                               PTR01230
        BC = 8.3333 * RZ0 * RZ0                                         PTR01240
        SCOEFF=AC+BC+CC                                                 PTR01250
        DELH=0.0                                                        PTR01260
        IF(SCOEFF.NE.0.0) CALL CUBIC(AC,BC,CC,DELH)                     PTR01270
C                                                                       PTR01280
C       STORE PLUME HEIGHT AND ASSOCIATED VARIABLES FOR THIS SOURCE     PTR01290
        H = HPRM + DELH                                                 PTR01300
        HSAV(J)=H                                                       PTR01310
        DISTFK = DISTF/1000.                                            PTR01320
        DSAV(J)=DISTF                                                   PTR01330
C                                                                       PTR01340
C                  PARTIAL PLUME PENETRATION                            PTR01350
C   IN THIS SECTION PARTIAL PLUME PENETRATION OF THE ELEVATED MARINE    PTR01360
C    INVERSION IS CONSIDERED                                            PTR01370
C                                                                       PTR01380
        P=0.0                                                           PTR01390
        HLP=HL-HPRM                                                     PTR01400
C                                                                       PTR01410
C   SKIP PARTIAL PENETRATION CODE IF PLUME IS ABOVE MIXING DEPTH        PTR01420
C   OR IF THERE IS NO PENETRATION                                       PTR01430
                                                                        PTR01440
        IF(HLP.GT.0.0.AND.DELH.GE.0.62*HLP)                             PTR01450
     &       CALL PARTIAL(J,P,F,HLP,HPRM,HBEL,HABV)                     PTR01460
                                                                        PTR01470
C                                                                       PTR01480
C       COMPUTE IY AND IZ OVER WATER AND LAND AT STACK-TOP HEIGHT       PTR01490
        CALL IYIZ(IYWSB,IZWSB,IYLS,IZLS,IYWSA,IZWSA)                    PTR01500
        KSIG=IPSIGS(J)                                                  PTR01510
C                                                                       PTR01520
C      PRINT STACK-SPECIFIC VARIABLES HERE IF IOPT(13) = 0 OR           PTR01530
C      IOPT(16) = 0                                                     PTR01540
        IF(IOPT(13).EQ.0.OR.IOPT(16).EQ.0) THEN                         PTR01550
           WRITE(IO,4) J,SOURCE(3,J),VS,TS,ZST,F,UPL,A,IYWSB,IZWSB,S    PTR01560
           IF(IRISE.EQ.0) THEN                                          PTR01570
              WRITE(IO,10) DHF,DELH,DISTFK                              PTR01580
           ELSEIF(IRISE.EQ.1) THEN                                      PTR01590
              WRITE(IO,11) DHF,DELH,DISTFK                              PTR01600
           ELSEIF(IRISE.EQ.2) THEN                                      PTR01610
              WRITE(IO,12) DHF,DELH,DISTFK                              PTR01620
           ELSEIF(IRISE.EQ.3) THEN                                      PTR01630
              WRITE(IO,13) DHF,DELH,DISTFK                              PTR01640
           ENDIF                                                        PTR01650
           WRITE(IO,6)                                                  PTR01660
        ENDIF                                                           PTR01670
C                                                                       PTR01680
C       START RECEPTOR LOOP                                             PTR01690
        DO 4000 K = 1,NRECEP                                            PTR01700
           CALL RECEP( J,RQ,SQ,P,DISTFK,IRISE,F,HPRM,HBEL,HABV,IZWSA,   PTR01710
     &                 IYWSA,IZWSB,IYWSB,KSIG,WIDTHB,EFFHT,WDIR,NPER)   PTR01720
4000    CONTINUE                                                        PTR01730
C                                                                       PTR01740
C       END OF SOURCE LOOP                                              PTR01750
5000    CONTINUE                                                        PTR01760
C                                                                       PTR01770
1     FORMAT( a1,/,26X,A80,/,26X,A80,/,26X,A80,//,10X,'DATE: ',         PTR01780
     &  I2,'/',I3,' HOUR: ',I2,//)                                      PTR01790
101   FORMAT( a1,/,26X,A80,/,26X,A80,/,26X,A80,//,10X,'DATE: ',         PTR01800
     &  I2,'/',I3,' ENDING TIME INCREMENT (HOUR): ',F5.1,//)            PTR01810
2     FORMAT( / ,25X,80('-'),//,57X,'INPUT METEOROLOGY',//,             PTR01820
     &  1X,'<',40('-'),'OBSERVED',45('-'),'><',11('-'),'   CALCULATED', PTR01830
     &  10('-'),'>',/,90X,'OVER- ',/,8X,'OVER-',T61,'OVER-   OVER-   ', PTR01840
     &  'OVER-   OVER- WATER     OVER-   OVER-   OVER-   OVER-',/,8X,   PTR01850
     &  'WATER  OVER-    OVER-   OVER-    AIR',T61,'WATER   WATER   ',  PTR01860
     &  'LAND    LAND  VERT.     WATER   LAND    WATER   WATER',/,1X,   PTR01870
     &  'OVER-  WIND   WATER    WATER/  WATER/  MINUS       WIND    ',  PTR01880
     &  'HORIZ.  VERT.   HORIZ.  VERT. POT.      MONIN-  MONIN- SUR',   PTR01890
     &  'FACE FRICTION',/,1X,'WATER  SPEED, MIXING   LAND    LAND   ',  PTR01900
     &  'SEA SFC       DIR    TURB.   TURB.   TURB.   TURB. TEMP.    ', PTR01910
     &  'OBUKHOV OBUKHOV  ROUGH. VELOCITY',/,1X,'WIND   M/SEC  HEIGHT', PTR01920
     &  '  STAB.  AIR TEMP TEMP    RH  SHEAR   INTEN.  INTEN.  INTEN.', PTR01930
     &  '  INTEN. GRAD.     LENGTH  LENGTH  LENGTH   (U*)',/,1X,'DIR  ',PTR01940
     &  '(',F4.0,' M)   (M)    CLASS  (DEG K) (DEG K) ( ) (DEG/M)  ',   PTR01950
     &  '(IYW)   (IZW)   (IYL)   (IZL) (K/M)       (M)     (M)     (M)',PTR01960
     &  '   (M/SEC)',/)                                                 PTR01970
3     FORMAT(1X,F5.0,1X,F7.2,1X,F6.0,2X,I3,'/',I1,3X,I3,'/',I3,1X,      PTR01980
     &  F7.2,1X,F3.0,1X,F7.4,4(F7.3,1X),F8.3,1X,F7.2,1X,F7.2,1X,E7.2,   PTR01990
     &  1X,F7.4,//,' *** NOTE THAT -999 INDICATE MISSING OR',           PTR02000
     &  ' INVALID DATA',/,26X,80('-'))                                  PTR02010
4     FORMAT( / ,2X,'STACK #',I3,': EMISSIONS = ',F8.2,' G/SEC, ',      PTR02020
     &  'EXIT VELOCITY = ',F6.2,' M/SEC, STACK GAS TEMP = ',F7.2,       PTR02030
     &  ' DEG K,',1X,'STACK HT ABV SFC =',F8.2,' M',/,T32,              PTR02040
     &  '--------------CALCULATED VARIABLES LISTED BELOW------------',/ PTR02050
     &  ,3X,'BUOY. FLUX = ',F9.2,' M**4/S**3,  STACK-TOP WIND SPEED = ',PTR02060
     &  F9.4,' M/SEC,',3X,'RATIO A (EFF STACK HT/BLDG HT) = ',F6.2,/,   PTR02070
     &  3X,'IY AND IZ FOR THIS STACK = ',                               PTR02080
     &  2F10.4,3X,'STABILITY PARAMETER = ',E10.4,' SEC**-2',/)          PTR02090
5     FORMAT(//,5X,'*** KEY TO SOME VARIABLES LISTED BELOW:',/10X,      PTR02100
     &  'HORIZ. AND VERT. FACTORS ARE DEFINED SO THAT CHI = Q/',        PTR02110
     &  '(2*PI*U*SY*SZ) * HORIZ * VERT',/,10X,                          PTR02120
     &  'XTIBL: DISTANCE PLUME MUST TRAVEL FROM SHORELINE BEFORE ',     PTR02130
     &  'ENCOUNTERING THE THERMAL INTERNAL BOUNDARY LAYER (KM)',/,10X,  PTR02140
     &  'HT: HEIGHT OF THE SLOPING TIBL AT THE RECEPTOR',/,10X,         PTR02150
     &  'HL: HEIGHT OF MIXING LID AT RECEPTOR (M)',/,10X,               PTR02160
     &  'DF: DECAY FACTOR',/,10X,                                       PTR02170
     &  'TC: TERRAIN CORRECTION FACTOR',/,10X,'RF: REFLECTION',         PTR02180
     &  ' CORRECTION FACTOR FOR SLOPING TERRAIN',/                      PTR02190
     &  ,6X,'*** NOTE: IF -999 APPEAR IN ANY OF THE FIELDS BELOW',      PTR02200
     &  ' THE PLUME IS ENTIRELY OVERWATER',/,16X,'ALSO, ONLY RECEPTORS',PTR02210
     & ' PREDICTED TO HAVE SIGNIFICANT IMPACT ARE REPRESENTED BELOW',//)PTR02220
6     FORMAT(/,37X,'<---------(SIGMA-Y)------------>',T70,              PTR02230
     & '<--------(SIGMA-Z)------->',/,T64,'TOTAL',/,                    PTR02240
     &  23X,'TERRAIN PLUME',T38,'AMBIENT',T57,'SHEAR  SIG-Y  AMBIENT',  PTR02250
     &  T91,'SIG-Z',/,5X,'(RELATIVE TO',T25,'HEIGHT',T32,'HEIGHT',T40,  PTR02260
     &  'AT',T58,'AT',T65,'AT',T73,'AT',T92,'AT',/,6X,                  PTR02270
     &  'PLUME AXIS) DIST. ABOVE  ABOVE INTERFACE',T57,'INTF.  ',       PTR02280
     &  'INTF. INTERFACE',T91,'INTF.   HORIZ./',T111,'CHI',             PTR02290
     &  '    MISCELLANEOUS',/,19X,                                      PTR02300
     &  'TO   WATER  SFC/  AND AT',T52,'BLDG AND AT AND AT AND AT',T85, PTR02310
     &  'BLDG AND AT   VERT.',16X,'(SEE KEY TO ',/,1X,'REC   X      Y', PTR02320
     &  3X,'SHORE LEVEL  RECEP  RECEP  BUOY  WAKE RECEP  RECEP   RECEP',PTR02330
     &  '  BUOY  WAKE  RECEP   EXP.       (UG/',7X,'VARIABLES)',/,2X,   PTR02340
     &  '#  (KM)   (KM)  (KM)   (M)    (M)    (M)   (M)   (M)   (M)',   PTR02350
     &  '    (M)     (M)   (M)   (M)    (M)    FACTOR    ',             PTR02360
     &  '  M**3)',/)                                                    PTR02370
10    FORMAT(3X,'UNSTABLE/NEUTRAL MOMENTUM PLUME RISE FORMULAS USED:',  PTR02380
     &  /,10X,'FINAL PLUME RISE WITHOUT BUILDING DOWNWASH = ',F8.2,     PTR02390
     &  ' M;    FINAL PLUME RISE WITH BUILDING DOWNWASH = ',F8.2,' M',/,PTR02400
     &  10X,'DISTANCE TO FINAL RISE = ',F7.3,' KM',/)                   PTR02410
11    FORMAT(3X,'UNSTABLE/NEUTRAL BUOYANCY PLUME RISE FORMULAS USED:',  PTR02420
     &  /,10X,'FINAL PLUME RISE WITHOUT BUILDING DOWNWASH = ',F8.2,     PTR02430
     &  ' M;    FINAL PLUME RISE WITH BUILDING DOWNWASH = ',F8.2,' M',/,PTR02440
     &  10X,'DISTANCE TO FINAL RISE = ',F7.3,' KM',/)                   PTR02450
12    FORMAT(3X,'STABLE MOMENTUM PLUME RISE FORMULAS USED:',            PTR02460
     &  /,10X,'FINAL PLUME RISE WITHOUT BUILDING DOWNWASH = ',F8.2,     PTR02470
     &  ' M;    FINAL PLUME RISE WITH BUILDING DOWNWASH = ',F8.2,' M',/,PTR02480
     &  10X,'DISTANCE TO FINAL RISE = ',F7.3,' KM',/)                   PTR02490
13    FORMAT(3X,'STABLE BUOYANCY PLUME RISE FORMULAS USED:',            PTR02500
     &  /,10X,'FINAL PLUME RISE WITHOUT BUILDING DOWNWASH = ',F8.2,     PTR02510
     &  ' M;    FINAL PLUME RISE WITH BUILDING DOWNWASH = ',F8.2,' M',/,PTR02520
     &  10X,'DISTANCE TO FINAL RISE = ',F7.3,' KM',/)                   PTR02530
                                                                        PTR02600
      RETURN                                                            PTR02610
      END                                                               PTR02620
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCRNK00010
      SUBROUTINE RANK (L)                                               RNK00020
C                                                                       RNK00030
C PURPOSE:  ARRANGE CONCENTRATIONS OF VARIOUS AVERAGING TIME INTO       RNK00040
C           HIGH-FIVE TABLES FOR EACH RECEPTOR                          RNK00050
C                                                                       RNK00060
C I/O:  L, AVERAGING TIME                                               RNK00070
C                                                                       RNK00080
C CALLED BY:  MAIN                                                      RNK00090
C                                                                       RNK00100
C CALLS:  NONE                                                          RNK00110
C                                                                       RNK00120
C       MINERALS MANAGEMENT SERVICE                                     RNK00130
C       U.S. DEPARTMENT OF THE INTERIOR                                 RNK00140
C                                                                       RNK00150
C OCD             REVISION HISTORY:                                     RNK00160
C    DCD 880930   CREATED.                                              RNK00170
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCRNK00180
                                                                        RNK00190
      include 'params.cmn'
      INCLUDE 'store.cmn'                                               RNK00220
                                                                        RNK00230
C   VARIABLES OUTPUT:                                                   RNK00240
C      HMAXA(J,K,L)  CONCENTRATIONS ACCORDING TO                        RNK00250
C        J : RANK OF CONC. (1-5)                                        RNK00260
C        K : RECEPTOR NUMBER                                            RNK00270
C        L : AVG TIME                                                   RNK00280
C      NDAY(J,K,L) : ASSOCIATED DAY OF CONC.                            RNK00290
C      IHRARRAY(J,K,L) : ENDING HOUR OF CONC.                           RNK00300
                                                                        RNK00310
      DO 50 K=1,NRECEP                                                  RNK00320
         IF (CONC(K,L).LE.HMAXA(5,K,L)) GO TO 50                        RNK00330
         DO 10 J=1,5                                                    RNK00340
            IF (CONC(K,L).GT.HMAXA(J,K,L)) GO TO 20                     RNK00350
C           CONCENTRATION IS ONE OF THE TOP FIVE                        RNK00360
10       CONTINUE                                                       RNK00370
         WRITE (IO,70)                                                  RNK00380
         GO TO 50                                                       RNK00390
C                                                                       RNK00400
C        THE FOLLOWING DO-LOOP HAS THE EFFECT OF INSERTING A NEW        RNK00410
C        CONCENTRATION ENTRY INTO ITS PROPER POSITION WHILE SHIFTING    RNK00420
C        DOWN THE 'OLD' LOWER CONCENTRATIONS THUS ESTABLISHING THE      RNK00430
C        'HIGH-FIVE' CONCENTRATION TABLE.                               RNK00440
20       IF (J.NE.5) THEN                                               RNK00450
            DO 30 IJ=4,J,-1                                             RNK00460
               IJP1=IJ+1                                                RNK00470
               HMAXA(IJP1,K,L)=HMAXA(IJ,K,L)                            RNK00480
               NDAY(IJP1,K,L) = NDAY(IJ,K,L)                            RNK00490
               IHRARRAY(IJP1,K,L) = IHRARRAY(IJ,K,L)                    RNK00500
30          CONTINUE                                                    RNK00510
         ENDIF                                                          RNK00520
C                                                                       RNK00530
C           INSERT LATEST CONC, DAY AND ENDING HR INTO THE              RNK00540
C           PROPER RANK IN THE HIGH-FIVE TABLE                          RNK00550
         HMAXA(J,K,L)=CONC(K,L)                                         RNK00560
         NDAY(J,K,L) = JDAY                                             RNK00570
         IHRARRAY(J,K,L) = LH                                           RNK00580
50    CONTINUE                                                          RNK00590
                                                                        RNK00600
      DO 60 K=1,NRECEP                                                  RNK00610
         CONC(K,L)=0.                                                   RNK00620
60    CONTINUE                                                          RNK00630
                                                                        RNK00640
70    FORMAT (1X,'   ****ERROR IN FINDING THE MAX CONCENTRATION***')    RNK00650
                                                                        RNK00660
      RETURN                                                            RNK00670
      END                                                               RNK00680
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine readcf2(ihstrt,nper,nav5,day1a,hr1,navt,elow,
     &  alat,along,tzone)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c PURPOSE:  Read OCD Control File, validate model inputs, and open
c           all input and output files.
c           Input data groups 1 through 16 will be read by this
c           subroutine.  Input data group 17, sequential overland
c           meteorological data, will be read in subroutine AVGLOOP.
c
c I/O:  IHSTRT, starting hour
c         NPER, number of averaging periods
c         NAV5, additional averaging time for fifth averaging period
c        DAY1A, starting day
c          HR1, starting hour
c         NAVT, additional averaging time for high five table
c         ELOW, elevation of lowest ground-level source in inventory
c         ALAT, latitude
c        ALONG, longitude
c        TZONE, time zone (number of hours behind GMT)
c
c CALLED BY:  MAIN
c
c CALLS:  LINESEG, SRCRNK, and EMISHT
c
c       Minerals Management Service
c       U.S. Department of the Interior
c
c OCD             Revision History:
c    Joseph C. Chang, EARTH TECH
c    First created: 961105
c    Last revised:  961125
c
c This subroutine is based subroutines SETUP, EMIS, SRCE, SIGSRC,
c METCHK, POLAR, RCPPRO, SPECS, and SHORE in the original version of OCD.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include 'params.cmn'
      include 'version.cmn'
      include 'opt.cmn'
      include 'store.cmn'
      include 'src.cmn'
      include 'met.cmn'
      include 'const.cmn'
      include 'linesrc.cmn'
      include 'exps.cmn'
      include 'shr.cmn'
      include 'funcs.cmn'
c
      dimension imps(25) ! source number in order of significance
      character*12 pname(maxp)
      character*3 aname(36)
      dimension radil(maxring),elrdum(maxring),elrdum_c(maxcar)
c
      character*4 ctemp12(12),ctemp8(8),ctempshore(maxmap2)
      real source_tmp(11),receptor_tmp(5)
      character*12 cvdic(maxvar,maxgrp)
      integer ivleng(maxvar,maxgrp)
      integer ivtype(maxvar,maxgrp)
      logical lecho,lexist
c
      integer nallow,iallow(8)
c
      data aname /' 1,',' 2,',' 3,',' 4,',' 5,',' 6,',' 7,',' 8,',
     &' 9,','10,','11,','12,','13,','14,','15,','16,','17,','18,',
     &'19,','20,','21,','22,','23,','24,','25,','26,','27,','28,',
     &'29,','30,','31,','32,','33,','34,','35,','36,'/
c
      data lecho/.false./  ! Do not echo input
c
c --- Define data group dictionary
c
      data (cvdic(i,19),i=1,maxvar)/         ! Group 2
     &  'INORMAL',59*' '/
c
      data (cvdic(i,1),i=1,maxvar)/          ! Group 3
     &  'IDATE(1)','IDATE(2)','IHSTRT','NPER','NAVG','IPOL','NSIGP',
     &  'NAV5','CONTWO','CELM',50*' '/
c
      data (cvdic(i,2),i=1,maxvar)/          ! Group 4
     &  'IOPT(1)','IOPT(2)','IOPT(3)','IOPT(4)','IOPT(5)',
     &  'IOPT(6)','IOPT(7)','IOPT(8)','IOPT(9)','IOPT(10)',
     &  'IOPT(11)','IOPT(12)','IOPT(13)','IOPT(14)','IOPT(15)',
     &  'IOPT(16)','IOPT(17)','IOPT(18)','IOPT(19)','IOPT(20)',
     &  'IOPT(21)','IOPT(22)','IOPT(23)','IOPT(24)','IOPT(25)',
     &  35*' '/
c
      data (cvdic(i,3),i=1,maxvar)/          ! Group 5
     &  'HANE','Z0L','ZMIN','SLAT',56*' '/
c
      data (cvdic(i,4),i=1,maxvar)/          ! Group 6a
     &  'NPT',59*' '/
c
      data (cvdic(i,5),i=1,maxvar)/          ! Group 6b
     &  'SRCNAM','SRCPAR',58*' '/
c
      data (cvdic(i,6),i=1,maxvar)/          ! Group 6c
     &  'XSTOP','YSTOP',58*' '/
c
      data (cvdic(i,7),i=1,maxvar)/          ! Group 7
     &  'SRCNUM',59*' '/
c
      data (cvdic(i,8),i=1,maxvar)/          ! Group 8
     &  'ISFCD','ISFCYR','IMXD','IMXYR',56*' '/
c
      data (cvdic(i,9),i=1,maxvar)/          ! Group 9
     &  'NRING','CENTX','CENTY','RADII',56*' '/
c
      data (cvdic(i,10),i=1,maxvar)/         ! Group 10
     &  'ISECTOR','ELEV',58*' '/
c
      data (cvdic(i,11),i=1,maxvar)/         ! Group 11
     &  'X0_C','Y0_C','NX_C','NY_C','DELX_C','DELY_C',54*' '/
c
      data (cvdic(i,12),i=1,maxvar)/         ! Group 12
     &  'IROW','ELEV',58*' '/
c
      data (cvdic(i,13),i=1,maxvar)/         ! Group 13a
     &  'NDISC',59*' '/
c
      data (cvdic(i,14),i=1,maxvar)/         ! Group 13b
     &  'RCPNAM','RCPPAR',58*' '/
c
      data (cvdic(i,15),i=1,maxvar)/         ! Group 14
     &  'JOPT(1)','JOPT(2)','JOPT(3)','JOPT(4)','JOPT(5)',
     &  'JOPT(6)','JOPT(7)','JOPT(8)','JOPT(9)','HWANE','HWT',
     &  49*' '/
c
      data (cvdic(i,16),i=1,maxvar)/         ! Group 15
     &  'ALAT','ALONG','TZONE','DECAY',56*' '/
c
      data (cvdic(i,17),i=1,maxvar)/         ! Group 16a
     &  'X0','Y0','NX','NY','DELX','DELY','WMIN','AVGDIST',52*' '/
c
      data (cvdic(i,18),i=1,maxvar)/         ! Group 16b
     &  'LWFLAG',59*' '/
c
c --- Define variable lengths (i.e., scalar or vector)
c
      data (ivleng(i,19),i=1,maxvar)/1*1,59*0/
      data (ivleng(i,1),i=1,maxvar)/10*1,50*0/
      data (ivleng(i,2),i=1,maxvar)/25*1,35*0/
      data (ivleng(i,3),i=1,maxvar)/4*1,56*0/
      data (ivleng(i,4),i=1,maxvar)/1*1,59*0/
      data (ivleng(i,5),i=1,maxvar)/1*12,1*11,58*0/ ! 12 because character*12
      data (ivleng(i,6),i=1,maxvar)/2*1,58*0/
      data (ivleng(i,7),i=1,maxvar)/1*25,59*0/
      data (ivleng(i,8),i=1,maxvar)/4*1,56*0/
      data (ivleng(i,9),i=1,maxvar)/1*1,2*1,1*maxring,56*0/
      data (ivleng(i,10),i=1,maxvar)/1*1,1*maxring,58*0/
      data (ivleng(i,11),i=1,maxvar)/6*1,54*0/
      data (ivleng(i,12),i=1,maxvar)/1*1,1*maxcar,58*0/
      data (ivleng(i,13),i=1,maxvar)/1*1,59*0/
      data (ivleng(i,14),i=1,maxvar)/1*8,1*5,58*0/ ! 8 because character*8
      data (ivleng(i,15),i=1,maxvar)/11*1,49*0/
      data (ivleng(i,16),i=1,maxvar)/3*1,1*12,56*0/
      data (ivleng(i,17),i=1,maxvar)/8*1,52*0/
      data (ivleng(i,18),i=1,maxvar)/1*maxmap2,59*0/
c                           ! maxmap2 because character*maxmap2
c
c --- Define variable types:
c          0 = null
c          1 = real
c          2 = integer
c          3 = logical
c          4 = character
c
      data (ivtype(i,19),i=1,maxvar)/1*2,59*0/
      data (ivtype(i,1),i=1,maxvar)/8*2,2*1,50*0/
      data (ivtype(i,2),i=1,maxvar)/25*2,35*0/
      data (ivtype(i,3),i=1,maxvar)/4*1,56*0/
      data (ivtype(i,4),i=1,maxvar)/1*2,59*0/
      data (ivtype(i,5),i=1,maxvar)/1*4,1*1,58*0/
      data (ivtype(i,6),i=1,maxvar)/2*1,58*0/
      data (ivtype(i,7),i=1,maxvar)/1*2,59*0/
      data (ivtype(i,8),i=1,maxvar)/4*2,56*0/
      data (ivtype(i,9),i=1,maxvar)/1*2,3*1,56*0/
      data (ivtype(i,10),i=1,maxvar)/1*2,1*1,58*0/
      data (ivtype(i,11),i=1,maxvar)/2*1,2*2,2*1,54*0/
      data (ivtype(i,12),i=1,maxvar)/1*2,1*1,58*0/
      data (ivtype(i,13),i=1,maxvar)/1*2,59*0/
      data (ivtype(i,14),i=1,maxvar)/1*4,1*1,58*0/
      data (ivtype(i,15),i=1,maxvar)/9*2,2*1,49*0/
      data (ivtype(i,16),i=1,maxvar)/3*1,1*1,56*0/
      data (ivtype(i,17),i=1,maxvar)/2*1,2*2,4*1,52*0/
      data (ivtype(i,18),i=1,maxvar)/1*4,59*0/
c
c *** Open mandatory files
c
      call fopen (in,'input.dat','old','formatted')
      call fopen (io,'ocd.out','unknown','formatted')
      call fopen (13,'wmet.dat','old','formatted')
c
c     If the error message file, ERROR.OUT already exists, delete it
c     first before open it.  This is because the GUI will always read
c     the information in ERROR.OUT.  If the current run is fine
c     (i.e., no information is written to ERROR.OUT) but the previous
c     run had some problems (i.e., ERROR.OUT is not blank), then the
c     GUI will think there is still something wrong with the current run.
c
      inquire (file='error.out',exist=lexist)
      if (lexist) then
        call fopen (ierr,'error.out','old','formatted')
        close (unit=ierr,status='delete')
      end if
      call fopen (ierr,'error.out','new','formatted')
c
c ----------------------------------------
c *** Input group 1
c ----------------------------------------
c
c     Read title lines
c
      read (in,1180) line1,line2,line3
      write (io,1400) pb,model_version,line1,line2,line3
c
c ----------------------------------------
c *** Input group 2
c ----------------------------------------
c
c     Read flag for normal vs. test run
c
      call readin(cvdic(1,19),ivleng(1,19),ivtype(1,19),in,ierr,lecho,
     1 1,inormal,  ! Stop the run if anything is wrong (i.e., assuming
c                  ! a normal run).  This is because the value of
c                  ! INORMAL is not known yet.
     2 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum)
c
       if (inormal.ne.0 .and. inormal.ne.1) then
         write(6,1015) inormal
         write(ierr,1015) inormal
         stop 'Error encountered.  See ERROR.OUT for more details.'
c              ! This is an unconditional stop since INORMAL is the
c              ! very first control parameter for OCD
       end if
c
c ----------------------------------------
c *** Input group 3
c ----------------------------------------
c
c     Read control parameters and constants
c
      call readin(cvdic(1,1),ivleng(1,1),ivtype(1,1),in,ierr,lecho,
     1 inormal,
     2 idate(1),idate(2),ihstrt,nper,navg,ipol,nsigp,nav5,
     3 contwo,celm,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     7 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum)
c
      if (idate(1).lt.0 .or. idate(1).gt.99) then
         write(6,2001) idate(1)
         write(ierr,2001) idate(1)
         if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for m
     &ore details.'
c        Stop for normal run.
      endif
c
      if (idate(2).lt.1 .or. idate(2).gt.366) then
         write(6,2003) idate(2)
         write(ierr,2003) idate(2)
         if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for m
     &ore details.'
c        Stop for normal run.
      endif
c
      if (ihstrt.lt.1 .or. ihstrt.gt.24) then
         write(6,2005) ihstrt
         write(ierr,2005) ihstrt
         if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for m
     &ore details.'
c        Stop for normal run.
      endif
c
      if (ipol.lt.3 .or. ipol.gt.7) then
         write(6,2007) ipol
         write(ierr,2007) ipol
         if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for m
     &ore details.'
c        Stop for normal run.
      endif
c
c *** Assign IPOL to 7 so that a test run can still be completed even
c     when IPOL is out of range.
c
      if (ipol.lt.3 .or. ipol.gt.7) ipol=7
c
      if (nav5.lt.0 .or. nav5.gt.24) then
         write(6,2008) nav5
         write(ierr,2008) nav5
         if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for m
     &ore details.'
c        Stop for normal run.
      endif
c
      if (contwo.le.0.) then
         write(6,2009) contwo
         write(ierr,2009) contwo
         if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for m
     &ore details.'
c        Stop for normal run.
      endif
c
      if (celm.le.0.) then
         write(6,2011) celm
         write(ierr,2011) celm
         if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for m
     &ore details.'
c        Stop for normal run.
      endif
c
c    navg cannot exceed 24; input met arrays can hold up to 24 hours
c
      if(navg.lt.1) then
         write(6,1254) navg
         write(ierr,1254) navg
         if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for m
     &ore details.'
c        Stop for normal run.
      endif
c
      if(navg.gt.24) then
         write(6,1255) navg
         write(ierr,1255) navg
         if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for m
     &ore details.'
c        Stop for normal run.
      endif
c
      if (nsigp.gt.25) then
         write (6,1250) nsigp
         write (ierr,1250) nsigp
         if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for m
     &ore details.'
c        Stop for normal run.
      endif
c
      ip=ipol-2
c
c ----------------------------------------
c *** Input group 4
c ----------------------------------------
c
c     Read 25 main modeling switches are used by OCD
c
      call readin(cvdic(1,2),ivleng(1,2),ivtype(1,2),in,ierr,lecho,
     1 inormal,
     2 iopt(1),iopt(2),iopt(3),iopt(4),iopt(5),
     3 iopt(6),iopt(7),iopt(8),iopt(9),iopt(10),
     4 iopt(11),iopt(12),iopt(13),iopt(14),iopt(15),
     5 iopt(16),iopt(17),iopt(18),iopt(19),iopt(20),
     6 iopt(21),iopt(22),iopt(23),iopt(24),iopt(25),
     7 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     8 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     9 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum)
c
c *** Validate input
c
      nallow = 2    ! A total of two values are allowed
      iallow(1)=0   ! The value allowed
      iallow(2)=1   ! The value allowed
      call validate (iopt( 1),nallow,iallow,ierr,inormal,'IOPT(1)')
      call validate (iopt( 2),nallow,iallow,ierr,inormal,'IOPT(2)')
      call validate (iopt( 3),nallow,iallow,ierr,inormal,'IOPT(3)')
      call validate (iopt( 4),nallow,iallow,ierr,inormal,'IOPT(4)')
      call validate (iopt( 6),nallow,iallow,ierr,inormal,'IOPT(6)')
      call validate (iopt( 7),nallow,iallow,ierr,inormal,'IOPT(7)')
      call validate (iopt( 9),nallow,iallow,ierr,inormal,'IOPT(9)')
      call validate (iopt(10),nallow,iallow,ierr,inormal,'IOPT(10)')
      call validate (iopt(11),nallow,iallow,ierr,inormal,'IOPT(11)')
      call validate (iopt(12),nallow,iallow,ierr,inormal,'IOPT(12)')
      call validate (iopt(13),nallow,iallow,ierr,inormal,'IOPT(13)')
      call validate (iopt(14),nallow,iallow,ierr,inormal,'IOPT(14)')
      call validate (iopt(15),nallow,iallow,ierr,inormal,'IOPT(15)')
      call validate (iopt(16),nallow,iallow,ierr,inormal,'IOPT(16)')
      call validate (iopt(17),nallow,iallow,ierr,inormal,'IOPT(17)')
      call validate (iopt(18),nallow,iallow,ierr,inormal,'IOPT(18)')
      call validate (iopt(19),nallow,iallow,ierr,inormal,'IOPT(19)')
      call validate (iopt(21),nallow,iallow,ierr,inormal,'IOPT(21)')
      call validate (iopt(22),nallow,iallow,ierr,inormal,'IOPT(22)')
      call validate (iopt(23),nallow,iallow,ierr,inormal,'IOPT(23)')
      call validate (iopt(24),nallow,iallow,ierr,inormal,'IOPT(24)')
      call validate (iopt(25),nallow,iallow,ierr,inormal,'IOPT(25)')
c
      nallow = 3    ! A total of three values are allowed
      iallow(1)=0   ! The value allowed
      iallow(2)=1   ! The value allowed
      iallow(3)=2   ! The value allowed
      call validate (iopt( 5),nallow,iallow,ierr,inormal,'IOPT(5)')
      call validate (iopt(20),nallow,iallow,ierr,inormal,'IOPT(20)')
c
      nallow = 7    ! A total of seven values are allowed
      iallow(1)=0   ! The value allowed
      iallow(2)=1   ! The value allowed
      iallow(3)=2   ! The value allowed
      iallow(4)=3   ! The value allowed
      iallow(5)=4   ! The value allowed
      iallow(6)=5   ! The value allowed
      iallow(7)=6   ! The value allowed
      call validate (iopt( 8),nallow,iallow,ierr,inormal,'IOPT(8)')
c
      if (nsigp.le.0 .and. iopt(7).eq.1) then
         write(6,1257)
         write(ierr,1257)
         if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for m
     &ore details.'
c        Stop for normal run.
      endif
c
      if (nsigp.le.0 .and. iopt(11).eq.0) then
         write(6,1258)
         write(ierr,1258)
         iopt(11)=1
         print *,'Non-fatal error encountered.  See ERROR.OUT for more d
     &etails.'
      endif
c
      if (navg.eq.1 .and. iopt(11).eq.0 .and. iopt(17).eq.0) then
         write(6,1261)
         write(ierr,1261)
         iopt(17)=1
         print *,'Non-fatal error encountered.  See ERROR.OUT for more d
     &etails.'
      endif
c
      if (navg.eq.1 .and. iopt(14).eq.0 .and. iopt(18).eq.0) then
         write(6,1262)
         write(ierr,1262)
         iopt(18)=1
         print *,'Non-fatal error encountered.  See ERROR.OUT for more d
     &etails.'
      endif
c
c *** Open optional files
c
      if(iopt(6).eq.1)  call fopen (15,'emis.dat','old','formatted')
      if(iopt(5).eq.0)  call fopen (11,'lmet.dat','old','unformatted')
      if(iopt(5).eq.2)  call fopen (11,'lmet.dat','old','formatted')
      if(iopt(21).eq.1) call fopen (7,'extra.out','unknown','formatted')
      if(iopt(22).eq.1) call fopen (12,'conc.bin','unknown',
     &                                 'unformatted')
c
      write (io,1410) title(ip),nper,navg,ihstrt,idate(2),idate(1),
     &  contwo,nsigp
      day1a=idate(2)
      hr1=ihstrt
      if(iopt(21).eq.1) write(7,1100)
c
      if (iopt(25).ne.1) then
         write (io,1420)
      endif
c
      if (iopt(19).ne.1) then
         navt=5
         if(nav5.eq.1.or.nav5.eq.3.or.nav5.eq.8.or.nav5.eq.24.or.
     &      nav5.eq.0) navt=4
         ntime(5)=nav5
         atime(5)=nav5
         write (io,1440) navt
      endif
c
      if (iopt(1).ne.0) then
         write (io,1450) celm
         elhn=99999.
         elow=99999.
      endif
c
      write (io,1451) (i,iopt(i),i=1,4)
      write (io,1452) (i,iopt(i),i=5,8)
      write (io,1453) (i,iopt(i),i=9,19)
      write (io,1454) (i,iopt(i),i=20,25),inormal
c
c ----------------------------------------
c *** Input group 5
c ----------------------------------------
c
c     Read in surface roughness length over land, the minimum miss
c     distance, and the approximate latitude of the source region.
c
      call readin(cvdic(1,3),ivleng(1,3),ivtype(1,3),in,ierr,lecho,
     1 inormal,
     2 hane,z0l,zmin,slat,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     7 idum,idum,idum,idum,idum,idum,idum,idum)
c
      if(slat.gt.90.0 .or. slat.lt.-90.0) then
        write(6,1830) slat
        write(ierr,1830) slat
        if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for mo
     &re details.'
c       Stop for normal run.
      end if
c
      if(hane.le.0.) then
        write(6,2013) hane
        write(ierr,2013) hane
        if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for mo
     &re details.'
c       Stop for normal run.
      end if
c
      if(z0l.le.0.) then
        write(6,2015) z0l
        write(ierr,2015) z0l
        if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for mo
     &re details.'
c       Stop for normal run.
      end if
c
      if(zmin.lt.0.) then
        write(6,2017) zmin
        write(ierr,2017) zmin
        if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for mo
     &re details.'
c       Stop for normal run.
      end if
c
c   Compute coriolis parameter as f = 2 * omega * sin(latitude)
c   where omega is the angular velocity of the earth = 7.292e-5 1/s
c
      fcor=1.4584e-4*sin(dg2rad*slat)
c
      if (iopt(1).ne.1) then
         write (io,1480) hane,z0l
      else
         write (io,1490) hane,z0l,zmin,slat
      endif
c
c ----------------------------------------
c *** Input group 6a
c ----------------------------------------
c
c     Input and process emission data
c
      if(iopt(20) .eq. 0) then
         write (io,1500) pb
      else if( iopt(20) .eq. 1) then
         write (io,1510) pb
      else if( iopt(20) .eq. 2) then
         write (io,1520) pb
      endif
c
      call readin(cvdic(1,4),ivleng(1,4),ivtype(1,4),in,ierr,lecho,
     1 inormal,
     2 npt,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     7 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum)
c
c     Check for npt < or = 0
c
      if (npt.le.0) then
         write (6,1280) npt
         write (ierr,1280) npt
         if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for m
     &ore details.'
c        Stop for normal run.
      endif
c
c     For area sources, check to make sure that the number of circles
c     is not greater than 5.
c
      if( iopt(20) .eq. 1 .and. npt .gt. 5) then
         write(6,1605) npt
         write(ierr,1605) npt
         if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for m
     &ore details.'
c        Stop for normal run.
      endif
c
c     For line source, check to make sure that only one source is
c     allowed.
c
      if( iopt(20) .eq. 2 .and. npt .gt. 1) then
         write(6,1606) npt
         write(ierr,1606) npt
         if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for m
     &ore details.'
c        Stop for normal run.
      endif
c
c     Check for npt > maxp for point sources
c
      if ( iopt(20) .eq. 0 .and. npt .gt. maxp) then
         write (6,1275) maxp
         write (ierr,1275) maxp
         if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for m
     &ore details.'
c        Stop for normal run.
      endif
c
c ----------------------------------------
c *** Input group 6b
c ----------------------------------------
c
      do i=1,npt    !   S T A R T    O F    S O U R C E    L O O P
c
c --- Initialize ctemp12 before read
c
      do j=1,12
         ctemp12(j)(1:1)=' '
      enddo
      call readin(cvdic(1,5),ivleng(1,5),ivtype(1,5),in,ierr,lecho,
     1 inormal,
     2 ctemp12,source_tmp,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     7 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum)
c
c --- Transfer CHAR*4 name into CHAR*12 variable
c
      pname(i)='            '
      do j=1,12
         pname(i)(j:j)=ctemp12(j)(1:1)
      enddo
      source(1,i) =source_tmp(1)
      source(2,i) =source_tmp(2)
      source(3,i) =source_tmp(3)
      source(4,i) =source_tmp(4)
      source(5,i) =source_tmp(5)
      source(6,i) =source_tmp(6)
      source(7,i) =source_tmp(7)
      source(8,i) =source_tmp(8)
      source(9,i) =source_tmp(9)
      elp(i)      =source_tmp(10)
      source(11,i)=source_tmp(11)
c
      if(source(3,i).lt.0.) then
        write(6,2101) i,source(3,i)
        write(ierr,2101) i,source(3,i)
        if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for mo
     &re details.'
c       Stop for normal run.
      end if
c
      if(source(4,i).lt.0.) then
        write(6,2103) i,source(4,i)
        write(ierr,2103) i,source(4,i)
        if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for mo
     &re details.'
c       Stop for normal run.
      end if
c
      if(source(5,i).lt.0.1) then
        write(6,2105) i,source(5,i)
        write(ierr,2105) i,source(5,i)
        if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for mo
     &re details.'
c       Stop for normal run.
      end if
c
      if(source(6,i).lt.230.) then
        write(6,2107) i,source(6,i)
        write(ierr,2107) i,source(6,i)
        if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for mo
     &re details.'
c       Stop for normal run.
      end if
c
      if(source(7,i).le.0.) then
        write(6,2109) i,source(7,i)
        write(ierr,2109) i,source(7,i)
        if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for mo
     &re details.'
c       Stop for normal run.
      end if
c
      if(source(8,i).le.0.) then
        write(6,2111) i,source(8,i)
        write(ierr,2111) i,source(8,i)
        if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for mo
     &re details.'
c       Stop for normal run.
      end if
c
      if(source(9,i).lt.0. .or. source(9,i).gt.180.) then
        write(6,2113) i,source(9,i)
        write(ierr,2113) i,source(9,i)
        if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for mo
     &re details.'
c       Stop for normal run.
      end if
c
      if(source(10,i).lt.0.) then
        write(6,2115) i,source(10,i)
        write(ierr,2115) i,source(10,i)
        if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for mo
     &re details.'
c       Stop for normal run.
      end if
c
c     ELHN, elevation of lowest stack top in inventory, is determined
c     in user height units
c
      if (iopt(1).eq.1) then
         tom=source(5,i)/celm+elp(i)
         if (tom.lt.elhn) elhn=tom
c          ELOW, elevation of lowest source ground-level
c          in inventory, in user height units.
         if (elp(i).lt.elow) elow=elp(i)
c          calculate buoyancy factor
      end if
c
c     For point sources, D represents the inside stack-top diameter
c     For area sources, D represents the circle diameter for each area
c     source as represented by a circle
c
      d=source(7,i)
c
c     Following variable is Briggs' F without temperature factor.
c
      source(10,i)=2.45153*source(8,i)*d*d
c
c     2.45153 is gravity over four.
c
      ts=source(6,i)
      if (ts.le.293.) then
         hf=source(5,i)
      else
         f=source(10,i)*(ts-293.)/ts
         if(f.ge.55.) then
            hf=source(5,i)+38.71*f**0.6/3.
c
c     Only buoyancy plume rise is considered here.
c     Effects due to building downwash and momentum plume rise are
c     ignored for this source ranking procedure.
c
         else
            hf=source(5,i)+21.425*f**0.75/3.
         endif
      endif
c
c     HSAV, DSAV, AND PSAV are used for temporary storage (or as
c     dummies).
c
      hsav(i)=hf
c
c     Determine height index.
c
      do 210 ih=2,9
         if (hf.lt.(hc1(ih)-.01)) go to 220
210   continue
      ih=10
220   is=ih-1
      a=pxucof(2,is)
      b=pxuexp(2,is)
      dsav(i)=(a*hf**b)*source(3,i)/3.
c
c     An estimate of the potential impact of each source is
c     determined and stored in dsav. Max concentration is
c     determined by chi(max)=(a*h**b)*q/u where
c     a is the coefficent and b is the exponent, of
c     maximum chi*u/q values predetermined for b stability
c     and a specific effective height range.  Plume rise
c     is calculated for b stability and 3 m/sec wind speed.
c
      ipsigs(i)=0
c
c     List point source information.
c
      write (io,1515) i,pname(i),(source(k,i),k=1,9),elp(i),f,
     &                source(11,i)
c
      end do    !   E N D    O F    S O U R C E    L O O P
c
c ----------------------------------------
c *** Input group 6c
c ----------------------------------------
c
c     Read extra line source information
c
      if (iopt(20).eq.2) then
      call readin(cvdic(1,6),ivleng(1,6),ivtype(1,6),in,ierr,lecho,
     1 inormal,
     2 xstop,ystop,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     7 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum)
      end if
c
c *** Calculate line source mid points
c
      if(iopt(20).eq.2) call lineseg(source(1,1),source(2,1))
c
c *** Rank significant sources
c
      if (nsigp.gt.0) call srcrnk(imps)
c
c *** Emissions with height table
c
      if (iopt(9).eq.0) call emisht
c
c ----------------------------------------
c *** Input group 7
c ----------------------------------------
c
c     Significant source determination
c
      if (nsigp.gt.0) then
c
      write (io,1580)
c
      if (iopt(7).eq.1) then
c
      call readin(cvdic(1,7),ivleng(1,7),ivtype(1,7),in,ierr,lecho,
     1 inormal,
     2 mps,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     7 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum)
c
         write (io,1590) nsigp,(mps(i),i=1,nsigp)
c
         do 351 i=1,nsigp
            if (mps(i).le.0 .or. mps(i).gt.npt) then ! out-of-bound
              write (6,1300)
              write (ierr,1300)
              if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT 
     &for more details.'
c             Stop for normal run.
            end if
351      continue
c
      else
c
         do 380 i=1,nsigp
            mps(i)=imps(i)
380      continue
c
      end if
c
      write (io,1600) npt,nsigp,(mps(i),i=1,nsigp)
      if (iopt(6).eq.1) then
c        save average emission rate
         do 400 i=1,npt
            psav(i)=source(3,i)
400      continue
c        fill in significant point source array
      endif
410   do 420 i=1,nsigp
         j=mps(i)
         ipsigs(j)=i
420   continue
c
      end if
c
c ----------------------------------------
c *** Input group 8
c ----------------------------------------
c
c     Check binary or ASCII PCRAMMET data file
c
      if (iopt(5).eq.0 .or. iopt(5).eq.2) then
c
      call readin(cvdic(1,8),ivleng(1,8),ivtype(1,8),in,ierr,lecho,
     1 inormal,
     2 isfcd,isfcyr,imxd,imxyr,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     7 idum,idum,idum,idum,idum,idum,idum,idum)
c
      if (isfcd.lt.0 .or. isfcd.gt.99999) then
         write(6,2121) isfcd
         write(ierr,2121) isfcd
         if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for m
     &ore details.'
c        Stop for normal run.
      endif
c
      if (isfcyr.lt.0 .or. isfcyr.gt.99) then
         write(6,2123) isfcyr
         write(ierr,2123) isfcyr
         if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for m
     &ore details.'
c        Stop for normal run.
      endif
c
      if (imxd.lt.0 .or. imxd.gt.99999) then
         write(6,2125) imxd
         write(ierr,2125) imxd
         if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for m
     &ore details.'
c        Stop for normal run.
      endif
c
      if (imxyr.lt.0 .or. imxyr.gt.99) then
         write(6,2127) imxyr
         write(ierr,2127) imxyr
         if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for m
     &ore details.'
c        Stop for normal run.
      endif
c
c     Read ID record from preprocessed met disk file.
c
      if (iopt(5).eq.0) read (11) id,iyear,idm,iym
      if (iopt(5).eq.2) read (11,'(4(i6,1x))') id,iyear,idm,iym
      if (isfcd.ne.id.or.isfcyr.ne.iyear) then
         write (6,1310) isfcd,isfcyr,id,iyear
         write (ierr,1310) isfcd,isfcyr,id,iyear
         if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for m
     &ore details.'
c        Stop for normal run.
      endif
      if (imxd.ne.idm.or.imxyr.ne.iym) then
         write (6,1320) imxd,imxyr,idm,iym
         write (ierr,1320) imxd,imxyr,idm,iym
         if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for m
     &ore details.'
c        Stop for normal run.
      endif
      write (io,1610) isfcd,isfcyr,imxd,imxyr
c
      end if
c
c ----------------------------------------
c *** Input group 9
c ----------------------------------------
c
c     Generate polar coordinate receptors.
c
      write (io,1620) pb
c
      if (iopt(8).eq.1 .or. iopt(8).eq.3 .or.
     &    iopt(8).eq.4 .or. iopt(8).eq.6) then
c
c        NRING    -  Number of rings
c        RADIL(I),I= 1,NRING
c                 -  NRING radial distances (user units),
c                    each of which generates 36 receptors around point
c                    CENTX, CENTY on azimuths 10 to 360 degrees.
c        CENTX    -  East coordinate about which radials are centered.
c                    (user units)
c        CENTY    -  North coordinate about which radials are centered.
c                    (user units)
c
      call readin(cvdic(1,9),ivleng(1,9),ivtype(1,9),in,ierr,lecho,
     1 inormal,
     2 nring,centx,centy,radil,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     7 idum,idum,idum,idum,idum,idum,idum,idum)
c
c --- Update nrecep to include polar receptors
c
      nrecep=nrecep+36*nring
c
      if (nring.lt.1) then
         write (6,1327)
         write (ierr,1327)
         if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for m
     &ore details.'
c        Stop for normal run.
      endif
c
      if (nring.gt.maxring) then
         write (6,1328) maxring
         write (ierr,1328) maxring
         if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for m
     &ore details.'
c        Stop for normal run.
      endif
c
      if (nrecep.gt.maxrec) then
         write (6,1340) maxrec
         write (ierr,1340) maxrec
         if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for m
     &ore details.'
c        Stop for normal run.
      endif
c
      do 460 j=1,nring
         if (radil(j).le.0) then
            write (6,1329) radil(j)
            write (ierr,1329) radil(j)
            if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT fo
     &r more details.'
c           Stop for normal run.
         endif
460   continue
c
      write (io,1630) centx,centy,(radil(i),i=1,nring)
c
      do 480 i=1,36
c
c        Calculate the angle in radians
c
         radik=float(i)*0.1745329
c
c        0.1745329 is 2*pi/36
c
         sinrad=sin(radik)
         cosrad=cos(radik)
c
         do 470 j=1,nring
         k=nrecep-36*nring+(i+36*(j-1))     ! receptor counter
         rrec(k)=(radil(j)*sinrad)+centx    ! east-coordinate
         srec(k)=(radil(j)*cosrad)+centy    ! north-coordinate
         rname(k)(1:3)=aname(i)             ! receptor name
         write (rname(k)(4:8),'(f5.1)') radil(j)
         zr(k)=0.
         elr(k)=0.
         hter(k)=0.
470      continue
480   continue
c
c     Assign value to JAR
c
      jar = nrecep
c
      end if
c
c ----------------------------------------
c *** Input group 10  
c ----------------------------------------
c
c     Read polar coordinate elevations.
c
      if (iopt(1).eq.1 .and. (iopt(8).eq.1 .or. iopt(8).eq.3 .or.
     &                        iopt(8).eq.4 .or. iopt(8).eq.6)) then
c
c        ISECTOR  -  Azimuth indicator (1 to 36)
c        ELRDUM(I),I=1,NRING
c                 -  Receptor elevations for this azimuth
c                    for NRING distances (user height units).
c
      do i=1,36
c
      call readin(cvdic(1,10),ivleng(1,10),ivtype(1,10),in,ierr,lecho,
     1 inormal,
     2 isector,elrdum,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     7 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum)
c
         if (isector.ne.i) then
            write (6,1330) isector,i
            write (ierr,1330) isector,i
            if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT fo
     &r more details.'
c           Stop for normal run.
         endif
         do 500 j=1,nring
            k=nrecep-36*nring+(i+36*(j-1))     ! receptor counter
            elr(k)=elrdum(j)
500      continue
c
      end do
c
      end if
c
c ----------------------------------------
c *** Input group 11
c ----------------------------------------
c
c     Generate Cartesian coordinate receptors.
c
      if (iopt(8).eq.2 .or. iopt(8).eq.3 .or.
     &    iopt(8).eq.5 .or. iopt(8).eq.6) then
c
      call readin(cvdic(1,11),ivleng(1,11),ivtype(1,11),in,ierr,lecho,
     1 inormal,
     2 x0_c,y0_c,nx_c,ny_c,delx_c,dely_c,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     7 idum,idum,idum,idum,idum,idum)
c
      if (nx_c.lt.1 .or. nx_c.gt.maxcar) then
         write(6,2131) nx_c,maxcar
         write(ierr,2131) nx_c,maxcar
         if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for m
     &ore details.'
c        Stop for normal run.
      endif
c
      if (ny_c.lt.1 .or. ny_c.gt.maxcar) then
         write(6,2133) ny_c,maxcar
         write(ierr,2133) ny_c,maxcar
         if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for m
     &ore details.'
c        Stop for normal run.
      endif
c
      if (delx_c.lt.0.) then
         write(6,2135) delx_c
         write(ierr,2135) delx_c
         if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for m
     &ore details.'
c        Stop for normal run.
      endif
c
      if (dely_c.lt.0.) then
         write(6,2137) dely_c
         write(ierr,2137) dely_c
         if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for m
     &ore details.'
c        Stop for normal run.
      endif
c
c --- Update nrecep to include Cartesian receptor grid
c
      nrecep=nrecep+nx_c*ny_c
c
      if (nrecep.gt.maxrec) then
         write (6,1340) maxrec
         write (ierr,1340) maxrec
         if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for m
     &ore details.'
c        Stop for normal run.
      endif
c
c --- Create receptor network.  Note that (X0_C, Y0_C) refer to the
c     northwest corner of the receptor network.
c
      itot=0
      do i=1,ny_c
        ydum=y0_c-float(i-1)*dely_c
        do j=1,nx_c
          xdum=x0_c+float(j-1)*delx_c
          itot=itot+1
          k=nrecep-nx_c*ny_c+itot          ! receptor counter
          rrec(k)=xdum                     ! east-coordinate
          srec(k)=ydum                     ! north-coordinate
          write(rname(k),'(i3,'','',i3)')  ! receptor name
          zr(k)=0.
          elr(k)=0.
          hter(k)=0.
        end do
      end do
c
      end if
c
c ----------------------------------------
c *** Input group 12
c ----------------------------------------
c
c     Read Cartesian coordinate elevations.
c
      if (iopt(1).eq.1 .and. (iopt(8).eq.2 .or. iopt(8).eq.3 .or.
     &                        iopt(8).eq.5 .or. iopt(8).eq.6)) then
c
      itot=0
      do i=1,ny_c
      call readin(cvdic(1,12),ivleng(1,12),ivtype(1,12),in,ierr,lecho,
     1 inormal,
     2 irow,elrdum_c,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     7 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum)
c
        if (irow.ne.i) then
           write (6,1331) irow,i
           write (ierr,1331) irow,i
           if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for
     & more details.'
c          Stop for normal run.
        endif
c
        do j=1,nx_c
          itot=itot+1
          k=nrecep-nx_c*ny_c+itot          ! receptor counter
          elr(k)=elrdum_c(j)
        end do
c
      end do
      end if
c
c ----------------------------------------
c *** Input group 13a
c ----------------------------------------
c
c     Read and process discrete receptor information
c
      if (iopt(8).le.3) then
c
      call readin(cvdic(1,13),ivleng(1,13),ivtype(1,13),in,ierr,lecho,
     1 inormal,
     2 ndisc,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     7 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum)
c
c --- Update nrecep to include discrete receptors
c
      nrecep=nrecep+ndisc
c
      if (ndisc.lt.1) then
         write (6,1339)
         write (ierr,1339)
         if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for m
     &ore details.'
c        Stop for normal run.
      endif
c
      if (nrecep.gt.maxrec) then
         write (6,1340) maxrec
         write (ierr,1340) maxrec
         if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for m
     &ore details.'
c        Stop for normal run.
      endif
c
c ----------------------------------------
c *** Input group 13b
c ----------------------------------------
c
      do k=1,ndisc
c
        i=nrecep-ndisc+k     ! receptor counter
c
c --- Initialize ctemp8 before read
c
      do j=1,8
         ctemp8(j)(1:1)=' '
      enddo
c
      call readin(cvdic(1,14),ivleng(1,14),ivtype(1,14),in,ierr,lecho,
     1 inormal,
     2 ctemp8,receptor_tmp,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     7 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum)
c
c --- Transfer CHAR*4 name into CHAR*8 variable
c
      rname(i)='        '
      do j=1,8
         rname(i)(j:j)=ctemp8(j)(1:1)
      enddo
      rrec(i) =receptor_tmp(1)
      srec(i) =receptor_tmp(2)
      zr(i)   =receptor_tmp(3)
      elr(i)  =receptor_tmp(4)
      hter(i) =receptor_tmp(5)
c
      if(zr(i).lt.0.) then
        write(6,2201) i,zr(i)
        write(ierr,2201) i,zr(i)
        if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for mo
     &re details.'
c       Stop for normal run.
      end if
c
      end do
c
      end if
c
      if (iopt(1).eq.1) then
c
c       If terrain option is employed, determine if receptor elevations
c       require labeling with asterisks for additional remarks.
c
         do 560 j=1,nrecep
            if (elr(j).lt.elow) star(2,j)=str
560      continue
      endif
c
      if (nrecep.le.0) then
         write (6,1350) nrecep
         write (ierr,1350) nrecep
         if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for m
     &ore details.'
c        Stop for normal run.
      endif
c
c     print out table of receptors.
c
      write (io,1650)
c
c     Special format statement for the first jar receptors
c
      if(jar.ne.0) then
c
         do 585 k = 1,jar
            write (io,1655) k,star(1,k),star(2,k),rname(k),
     &        rrec(k),srec(k),zr(k),elr(k),hter(k)
585      continue
c
      endif
c
      if(jar.ne.nrecep) then
c
         do 590 k=jar+1,nrecep
            write (io,1660) k,star(1,k),star(2,k),rname(k),
     &      rrec(k),srec(k),zr(k),elr(k),hter(k)
590      continue
c
      endif
c
      if (iopt(1).eq.0) go to 820
      write (io,1670)
c
820   continue
c
c ----------------------------------------
c *** Input group 14
c ----------------------------------------
c
c     Read special options for additional met data
c
c     Initialize option settings for additional met data
c
      do 593 i = 1,9
         jopt(i) = 0
593   continue
      write(io,1799) pb
c
      call readin(cvdic(1,15),ivleng(1,15),ivtype(1,15),in,ierr,lecho,
     1 inormal,
     2 jopt(1),jopt(2),jopt(3),jopt(4),jopt(5),
     3 jopt(6),jopt(7),jopt(8),jopt(9),hwane,hwt,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     7 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     8 idum)
c
c *** Validate input
c
      nallow = 2    ! A total of two values are allowed
      iallow(1)=0   ! The value allowed
      iallow(2)=1   ! The value allowed
      call validate (jopt( 1),nallow,iallow,ierr,inormal,'JOPT(1)')
      call validate (jopt( 2),nallow,iallow,ierr,inormal,'JOPT(2)')
      call validate (jopt( 3),nallow,iallow,ierr,inormal,'JOPT(3)')
      call validate (jopt( 5),nallow,iallow,ierr,inormal,'JOPT(5)')
      call validate (jopt( 7),nallow,iallow,ierr,inormal,'JOPT(7)')
      call validate (jopt( 8),nallow,iallow,ierr,inormal,'JOPT(8)')
      call validate (jopt( 9),nallow,iallow,ierr,inormal,'JOPT(9)')
c
      nallow = 3    ! A total of three values are allowed
      iallow(1)=1   ! The value allowed
      iallow(2)=2   ! The value allowed
      iallow(3)=3   ! The value allowed
      call validate (jopt( 4),nallow,iallow,ierr,inormal,'JOPT(4)')
c
      nallow = 2    ! A total of two values are allowed
      iallow(1)=1   ! The value allowed
      iallow(2)=2   ! The value allowed
      call validate (jopt( 6),nallow,iallow,ierr,inormal,'JOPT(6)')
c
      if(hwane.le.0.) then
        write(6,2203) hwane
        write(ierr,2203) hwane
        if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for mo
     &re details.'
c       Stop for normal run.
      end if
c
      if(hwt.le.0.) then
        write(6,2205) hwt
        write(ierr,2205) hwt
        if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for mo
     &re details.'
c       Stop for normal run.
      end if
c
      write(io,1801) (jopt(i),i=1,5)
      write(io,1802) (jopt(i),i=6,9),hwane,hwt
c
c     Prepare for virtual distance calculations using P-G coefficients
c
      do 596 i = 1,6
         ps(i) = 1.0/ps(i)
         qs(i) = 1.0/qs(i)
596   continue
      do 597 i = 1,37
         asi(i) = 1.0/as(i)
         bsi(i) = 1.0/bs(i)
597   continue
      do 599 ist=1,6
         m = ia(ist)
         l = ia(ist+1)-2
         do 598 k = m,l
            sigzs(k) = as(k)*dst(k)**bs(k)
598      continue
         sigzs(l+1) = 99999.
599   continue
c
c ----------------------------------------
c *** Input group 15
c ----------------------------------------
c
c     Read chemical transformation rate specifications
c
      if (iopt(25).eq.1) then
c
      call readin(cvdic(1,16),ivleng(1,16),ivtype(1,16),in,ierr,lecho,
     1 inormal,
     2 alat,along,tzone,decay,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     7 idum,idum,idum,idum,idum,idum,idum,idum)
c
         if(alat .ne. slat) then
           write(6,1831) alat,slat
           write(ierr,1831) alat,slat
c           if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for
c     & more details.'
c          Stop for normal run.
         end if
c
         if(along.gt.180.0 .or. along.lt.-180.0) then
           write(6,1840) along
           write(ierr,1840) along
           if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for
     & more details.'
c          Stop for normal run.
         end if
c
         if(tzone.gt.12 .or. tzone.lt.-13.) then
           write(6,1850) tzone
           write(ierr,1850) tzone
           if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for
     & more details.'
c          Stop for normal run.
         end if
c
         do i=1,12
         if (decay(i).lt.0.) then
           write(6,2207) i,decay(i)
           write(ierr,2207) i,decay(i)
           if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for
     & more details.'
c          Stop for normal run.
         end if
         end do
c
         write(io,1860) alat,along,tzone
         write(io,1870) decay
c
      end if
c
c ----------------------------------------
c *** Input group 16a
c ----------------------------------------
c
c     Define shoreline geometry
c
      call readin(cvdic(1,17),ivleng(1,17),ivtype(1,17),in,ierr,lecho,
     1 inormal,
     2 x0,y0,nx,ny,delx,dely,wmin,avgdist,
     3 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     4 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     5 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     6 idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,idum,
     7 idum,idum,idum,idum)
c
      if (nx.lt.1 .or. nx.gt.maxmap) then
         write(6,2731) nx,maxmap
         write(ierr,2731) nx,maxmap
         if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for m
     &ore details.'
c        Stop for normal run.
      endif
c
      if (ny.lt.1 .or. ny.gt.maxmap) then
         write(6,2733) ny,maxmap
         write(ierr,2733) ny,maxmap
         if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for m
     &ore details.'
c        Stop for normal run.
      endif
c
      if (delx.lt.0.) then
         write(6,2735) delx
         write(ierr,2735) delx
         if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for m
     &ore details.'
c        Stop for normal run.
      endif
c
      if (dely.lt.0.) then
         write(6,2737) dely
         write(ierr,2737) dely
         if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for m
     &ore details.'
c        Stop for normal run.
      endif
c
      if (wmin.lt.0.) then
         write(6,2739) wmin
         write(ierr,2739) wmin
         if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for m
     &ore details.'
c        Stop for normal run.
      endif
c
      if (avgdist.lt.0.) then
         write(6,2741) avgdist
         write(ierr,2741) avgdist
         if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for m
     &ore details.'
c        Stop for normal run.
      endif
c
      delxk = delx*contwo
      delyk = dely*contwo
c
c   Check to make sure grid lengths meet the required grid
c   standardization criteria
c
c     if(avgdist*contwo .le. 2.0) then
c        if(delxk.lt.0.05 .or. delxk.gt.0.08) then
c           write(6,1000) delxk,avgdist
c           write(ierr,1000) delxk,avgdist
c           if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT fo
c     &r more details.'
c           Stop for normal run.
c        elseif(delyk.lt.0.03 .or. delyk.gt.0.06) then
c           write(6,1010) delyk,avgdist
c           write(ierr,1010) delyk,avgdist
c           if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT fo
c     &r more details.'
c           Stop for normal run.
c        endif
c     else
c        if(delxk.lt.0.2 .or. delxk.gt.0.4) then
c           write(6,1000) delxk,avgdist
c           write(ierr,1000) delxk,avgdist
c           if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT fo
c     &r more details.'
c           Stop for normal run.
c        elseif(delyk.lt.0.1 .or. delyk.gt.0.3) then
c           write(6,1010) delyk,avgdist
c           write(ierr,1010) delyk,avgdist
c           if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT fo
c     &r more details.'
c           Stop for normal run.
c        endif
c     endif
c
      write(io,1880) x0,y0,nx,ny,delx,dely,delxk,delyk,wmin,avgdist
c
c ----------------------------------------
c *** Input group 16b
c ----------------------------------------
c
c --- Initialize ctempshore before read
c
      do j=1,nx*ny
         ctempshore(j)(1:1)=' '
      enddo
      call readin2(cvdic(1,18),ivleng(1,18),ivtype(1,18),in,ierr,lecho,
     1 inormal,
     2 ctempshore)    ! Use special version of READIN to read in large
c                     ! character array
c
c --- Transfer CHAR*4 name into CHAR*1 variable
c
      do i=1,ny
         do j=1,nx
            k=j+(i-1)*nx
            xymap(i,j)=ctempshore(k)(1:1)
            if (xymap(i,j).ne.'W' .and. xymap(i,j).ne.'L') then
               write (6,1851)
               write (ierr,1851)
               if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT
     & for more details.'
c              Stop for normal run.
            end if
         end do
      enddo
c
c     Prepare comprenhesive map of entire area
c
      call map
c
c     Check for overwater receptors with elevations above water surface
c     then print warning and reset elevation to zero (water surface)
c
      if(iopt(1).eq.1) then
c
         do 700 l=1,nrecep
            if(elr(l).ne.0.) then
               delxr=rrec(l)-x0
               delyr=y0-srec(l)
               lx=int(delxr/delx+0.9999)
               ly=int(delyr/dely+0.9999)
               if(lx.ge.1 .and. ly.ge.1) then
                  if(lx.le.nx .or. ly.le.ny) then
                     if(xymap(ly,lx).eq.letw) then
                        write(io,27) l,elr(l)
                        elr(l)=0.
                     endif
                  endif
               endif
            endif
700      continue
c
      endif
c
c ----------------------------------------
c *** Input group 17 file positioning only, actual data are read
c                     in subroutine AVGLOOP)
c ----------------------------------------
c
      if (iopt(5).eq.1) then
        call readin3(in,ierr,lecho,inormal)
c                                    ! Use special version of READIN
c                                    ! to find 'INPUTGROUP17' header
      end if
c
c *** Format statements
c
c     Part I: Output listing
c     -----------------------
c
27    FORMAT(//,'   ***** WARNING -- ILLEGAL ELEVATION FOR OVERWATER',
     &         ' RECEPTOR NUMBER ',I3,', ELEVATION = ',F8.0,/,
     &         ' ---> ELEVATION IS RESET TO ZERO (WATER SURFACE)',//)
111   format ('(',i3,'a1)')
1100  FORMAT('DAY HR SR RC   X    Y    ZER    H    HA   XLAND  XSR   ',
     &'   XD   XTIBL    HT   SY    SZ    HLR   VIRTY  VIRTZ    XL',
     &'  T I    CHI',/,
     &'             (KM) (KM)   (M)   (M)  (M)    (KM)  (KM)    (KM)  ',
     &'(KM)    (M)  (M)    (M)   (M)    (KM)   (KM)   (KM)      ',
     &'(UG/M3)')
1180  FORMAT (A80/A80/A80)
1200  FORMAT (A4)
1215  FORMAT (A12)
1240  FORMAT (A8,5F10.3)
1400  FORMAT ( a1,20X,a,//,21X,A80,/,21X,A80,/,21X,A80)
1410  FORMAT( / ,T30,'GENERAL INPUT INFORMATION'//2X,'THIS RUN OF THE ',
     &'OCD MODEL IS FOR ','THE POLLUTANT ',A4,' FOR ',I5,1X,I3,'-HOUR ',
     &'PERIODS.'/2X,'CONCENTRATION ESTIMATES BEGIN ON HOUR-',I2,', JULIA
     &N DAY-',I3,', YEAR-19',I2,'.'/1X,' 1.0 USER LENGTH UNIT IN THE ',
     & 'HORIZONTAL = ',F14.7,' KILOMETERS.',/,1X,I3,
     &' SIGNIFICANT SOURCES ARE TO BE CONSIDERED.')
1420  FORMAT ( / ,' THIS RUN WILL NOT CONSIDER ANY POLLUTANT LOSS.')
1440  FORMAT (1X,' HIGH-FIVE SUMMARY CONCENTRATION TABLES ','WILL BE OUT
     &PUT FOR ',I3,' AVERAGING PERIODS.'/'  AVG TIMES ','OF 1,3,8, AND 2
     &4 HOURS ARE AUTOMATICALLY DISPLAYED.')
1450  FORMAT (' ',1X,'1.0 USER LENGTH UNIT IN THE VERTICAL = ',
     & F14.7,' METERS.')
1451  FORMAT ( / ,T5,'OPTION  ',T16,'OPTION LIST',T47,'OPTION SPECIFICAT
     &ION : 0= IGNORE OPTION',/,T70,'1= USE OPTION',/,
     &T25,'--TECHNICAL OPTIONS--',/,
     &1X,T7,I2,T16,'CONSIDER TERRAIN ADJUSTMENTS',T70,I1,/,
     &1X,T7,I2,T16,'DO NOT INCLUDE STACK DOWNWASH CALCULATIONS',T70,I1,/
     &1X,T7,I2,T16,'DO NOT INCLUDE GRADUAL PLUME RISE CALCULATIONS',
     &T70,I1,/,
     &1X,T7,I2,T16,'CALCULATE INITIAL PLUME SIZE DUE TO BUOYANCY',
     &T70,I1)
1452  FORMAT(
     &T25,'--INPUT OPTIONS--',/,
     &1X,T7,I2,T16,'SOURCE OF MET DATA',T70,I1,/,
     &         T16,'=0, MET DATA FROM SEPARATE BINARY PCRAMMET FILE',/,
     &         T16,'=1, ASCII MET DATA INCLUDED IN CONTROL FILE',/,
     &         T16,'=2, MET DATA FROM SEPARATE ASCII PCRAMMET FILE',/,
     &1X,T7,I2,T16,'READ HOURLY EMISSIONS',T70,I1,/,
     &1X,T7,I2,T16,'SPECIFY SIGNIFICANT SOURCES',T70,I1,/,
     &1X,T7,I2,T16,'RECEPTOR TYPES',T70,I1,/,
     &         T16,'=0, DISCRETE RECEPTORS ONLY',/,
     &         T16,'=1, DISCRETE AND POLAR RECEPTORS',/,
     &         T16,'=2, DISCRETE AND CARTESIAN RECEPTORS',/,
     &         T16,'=3, DISCRETE, POLAR AND CARTESIAN RECEPTORS',/,
     &         T16,'=4, POLAR RECEPTORS',/,
     &         T16,'=5, CARTESIAN RECEPTORS',/,
     &         T16,'=6, POLAR AND CARTESIAN RECEPTORS')
1453  FORMAT(
     &T25,'--PRINTED OUTPUT OPTIONS--',/,
     &1X,T7,I2,T16,'DELETE EMISSIONS WITH HEIGHT TABLE',T70,I1,/,
     &1X,T7,I2,T16,'DELETE MET DATA SUMMARY FOR AVG PERIOD',T70,I1,/,
     &1X,T7,I2,T16,'DELETE HOURLY CONTRIBUTIONS',T70,I1,/,
     &1X,T7,I2,T16,'DELETE MET DATA ON HOURLY CONTRIBUTIONS',T70,I1,/,
     &1X,T7,I2,T16,'DELETE PLUME RISE/TRANSPORT ON HRLY CONTRIBUTIONS',
     &T70,I1,/,
     &1X,T7,I2,T16,'DELETE HOURLY SUMMARY',T70,I1,/,
     &1X,T7,I2,T16,'DELETE MET DATA ON HRLY SUMMARY',T70,I1,/,
     &1X,T7,I2,T16,'DELETE PLUME RISE/TRANSPORT ON HRLY SUMMARY',
     &T70,I1,/,
     &1X,T7,I2,T16,'DELETE AVG-PERIOD CONTRIBUTIONS',T70,I1,/,
     &1X,T7,I2,T16,'DELETE AVERAGING PERIOD SUMMARY',T70,I1,/,
     &1X,T7,I2,T16,'DELETE AVG CONCENTRATIONS AND HI-5 TABLES',T70,I1)
1454  FORMAT(
     &T25,'--OTHER CONTROL AND OUTPUT OPTIONS--',/,
     &1X,T7,I2,T16,'SOURCE TYPE (0=POINT; 1=AREA; 2=LINE)',T70,I1,/,
     &1X,T7,I2,T16,'CREATE SUMMARY OUTPUT FILE CALLED EXTRA.OUT',
     &T70,I1,/,
     &1X,T7,I2,T16,'WRITE HOURLY CONC TO DISK',T70,I1,/,
     &1X,T7,I2,T16,
     &'CALCULATE ANNUAL IMPACT FROM NON-PERMANENT ACTIVITIES',T70,I1,/,
     &1X,T7,I2,T16,'LAND SOURCE (DO NOT MODIFY WIND SPEED)',T70,I1,/,
     &1X,T7,I2,T16,'SPECIFY POLLUTANT CHEMICAL TRANSFORMATION RATE',
     &T70,I1,/,
     &1X,T7,2X,T16,'PERFORM NORMAL RUN (=1) OR TEST RUN (=0)',T70,I1)
1480  FORMAT ( / ,'  LAND ANEMOMETER HEIGHT (METERS) = ',F10.2,/,3X,
     &'LAND SURFACE ROUGHNESS LENGTH (METERS) = ',F10.5,/)
1490  FORMAT ( / ,'  LAND ANEMOMETER HEIGHT (METERS) = ',F10.2,/,3X,
     & 'LAND SURFACE ROUGHNESS LENGTH (METERS) = ',F10.5,//,3X,
     & 'MINIMUM DISTANCE FOR PLUME ABOVE TERRAIN (METERS) = ',F5.1,/,
     & 3X,'LATITIDE OF SOURCE REGION (DEG) = ',F6.2)
1500  FORMAT ( a1,T55,'POINT SOURCE INFORMATION'//1X,T6,'SOURCE',T22,
     &'EAST    NORTH',T37,'EMISSION   BUILDING  STACK   STACK   STACK',
     &'    EXIT   STACK       GRD-LVL ',7X,'BUOY FLUX',4X,'BLDG',/,
     & 1X,T22,
     &'COORD   COORD',T37,'  RATE      HEIGHT   TOP HT  TEMP    DIAM ',
     &'  VELOCITY ANGLE        ELEV.  ',7X,'   (F)','       WIDTH',/,
     & 1X,T22,
     &' (USER UNITS)',T37,'(G/SEC)      (M)      (M)     (K)     (M) ',
     &'  (M/SEC)  (DEG         (USER',7X,'  M**4/S**3',6X,'(M)',/,1X,
     & T22,T87,'FROM VERT)',T101,'HT UNITS)',6X,'(CALCULATED)',/)
1510  FORMAT ( a1,T55,'AREA SOURCE INFORMATION'//1X,T6,'SOURCE',T22,
     &'EAST    NORTH',T37,'EMISSION   BUILDING  SOURCE  SOURCE  AREA  ',
     &'    EXIT   STACK       GRD-LVL ',7X,'BUOY FLUX',4X,'BLDG',/,
     & 1X,T22,
     &'COORD   COORD',T37,'  RATE      HEIGHT   HEIGHT  TEMP    DIAM ',
     &'  VELOCITY ANGLE        ELEV.  ',7X,'   (F)','       WIDTH',/,
     & 1X,T22,
     &' (USER UNITS)',T37,'(G/SEC)      (M)      (M)     (K)     (M) ',
     &'  (M/SEC)  (DEG         (USER',7X,'  M**4/S**3',6X,'(M)',/,1X,
     & T22,T87,'FROM VERT)',T101,'HT UNITS)',6X,'(CALCULATED)',/)
1515  FORMAT (1X,I3,1X,A12,1X,2F8.3,1P,E10.3,0P,F10.2,2F8.1,F7.1,1X,
     &  F8.1,F7.1,6X,F9.2,5X,F9.2,2X,F9.2)
1520  FORMAT ( a1,T55,'LINE SOURCE INFORMATION'//1X,T6,'SOURCE',T22,
     &'EAST    NORTH',T37,'EMISSION   BUILDING  SOURCE  SOURCE  SOURCE',
     &'    EXIT   STACK       GRD-LVL ',7X,'BUOY FLUX',4X,'BLDG',/,
     & 1X,T22,
     &'COORD   COORD',T37,'  RATE      HEIGHT   HEIGHT  TEMP    DIAM ',
     &'  VELOCITY ANGLE        ELEV.  ',7X,'   (F)','       WIDTH',/,
     & 1X,T22,
     &' (USER UNITS)',T37,'(G/SEC)      (M)      (M)     (K)     (M) ',
     &'  (M/SEC)  (DEG         (USER',7X,'  M**4/S**3',6X,'(M)',/,1X,
     & T22,T87,'FROM VERT)',T101,'HT UNITS)',6X,'(CALCULATED)',/)
1580  FORMAT ( / ,21X,'ADDITIONAL INFORMATION ON SOURCES:')
1590  FORMAT ( / ,'  USER SPECIFIED ',I3,' (NPT) SIGNIFICANT POINT ','SO
     &URCES AS LISTED BY POINT SOURCE NUMBER:'/2X,25I5)
1600  FORMAT ( / ,2X,'EMISSION INFORMATION FOR ',I4,' (NPT) POINT SOUR',
     &'CES HAS BEEN INPUT'/2X,I2,' SIGNIFICANT POINT SOURCES(NSIGP) ','A
     &RE TO BE',' USED FOR THIS RUN'/2X,'THE ORDER OF SIGNIFICANCE(IMPS)
     & FOR 25 OR LESS POINT SOURCES USED IN THIS RUN AS LISTED BY POINT
     &SOURCE NUMBER:'/2X,25I5)
1610  FORMAT (2X,'SURFACE MET DATA FROM STATION(ISFCD) ',I6,', YEAR IS(F
     &CYR) 19',I2/2X,'MIXING HEIGHT DATA FROM STATION(IMXD) ',I6,', YEAR
     &(IMXYR) 19',I2)
1620  FORMAT ( a1,T31,'RECEPTOR INFORMATION')
1630  FORMAT ( / ,' OCD INTERNALLY GENERATES 36 RECEPTORS ',  'ON A ',
     &'CIRCLE CORRESPONDING TO EACH NON-ZERO RADIAL DISTANCE FROM A ',
     &'CENTER POINT ',/,1X,T10,'COORDINATES ARE (USER UNITS): (',F8.3,',
     &',F8.3,')'/1X,T10,'RADIAL DISTANCE(S) USER SPECIFIED ',
     &'(USER UNITS): ',500(F11.3,'  '))
1650  FORMAT ( / ,' RECEPTOR    IDENTIFICATION  EAST     NORTH     RECEP
     &TOR HT     RECEPTOR GROUND LEVEL', /1X,T31,'COORD',
     &T40,'COORD  ABV LOCAL GRD LVL        ELEVATION',T90,
     & ' HTER'/,1X,T32,'(USER UNITS)        (METERS)         ',
     &'(USER HT UNITS)',T91,'(M)'/)
1655  FORMAT(1X,T3,I3,2A1,8X,A8,F12.3,F9.3,3X,F10.1,11X,F10.2,3X,
     &  F10.2,11X,F7.1)
1660  FORMAT(1X,T3,I3,2A1,8X,A8,F12.3,F9.3,3X,F10.1,11X,F10.2,11X,F6.1)
1670  FORMAT ( / ,T3,'* ONE ASTERISK INDICATES THAT THE ASSOCIATED ','RE
     &CEPTOR(S) HAVE A GROUND LEVEL ELEVATION LOWER ','THAN THE LOWEST S
     &OURCE BASE ELEVATION.'/' CAUTION SHOULD ','BE USED IN INTERPRETING
     & CONCENTRATIONS FOR THESE RECEPTORS.'/' **  TWO ASTERISKS ','INDIC
     &ATE THAT THE ASSOCIATED RECEPTOR(S) HAVE GROUND LEVEL ','ELEVATION
     &S ABOVE THE LOWEST STACK TOP.',/)
1799  FORMAT( a1)
1801  FORMAT(//,T21,'OPTION SETTINGS FOR INCLUSION OF ADDITIONAL',
     &  ' METEOROLOGY ARE LISTED BELOW:',//,
     &10X,'OPTION 1: OVERWATER WIND DIRECTION',T61,I2,/,
     &T21,'(1=PROVIDED, 0=NOT PROVIDED, OR DO NOT USE)',/,
     &10X,'OPTION 2: OVERWATER WIND SPEED',T61,I2,/,
     &T21,'(1=PROVIDED, 0=NOT PROVIDED, OR DO NOT USE)',/,
     &10X,'OPTION 3: OVERWATER VERT. POT. TEMP. GRAD. DATA',T61,I2,/,
     &T21,'(1=PROVIDED, 0=NOT PROVIDED, OR DO NOT USE)',/,
     &10X,'OPTION 4: OVERWATER HUMIDITY',T61,I2,/,
     &T21,'(1=RELATIVE HUMIDITY (%), 2=WET BULB TEMPERATURE (DEG K),',/,
     &T21,' 3=DEW POINT TEMPERATURE (DEG K))',/,
     &10X,'OPTION 5: OVERLAND TURBULENCE DATA',T61,I2,/,
     &T21,'(1=PROVIDED, 0=NOT PROVIDED, OR DO NOT USE)')
1802  FORMAT(
     &10X,'OPTION 6: WATER SURFACE TEMPERATURE',T61,I2,/,
     &T21,'(1=WATER SURFACE TEMP (DEG K),',/,
     &T21,' 2=AIR MINUS WATER TEMP (DEG K))',/,
     &10X,'OPTION 7: WIND DIRECTION SHEAR DATA',T61,I2,/,
     &T21,'(1=PROVIDED, 0=NOT PROVIDED, OR DO NOT USE)',/,
     &10X,'OPTION 8: OVERWATER TURBULENCE DATA (Y-COMPONENT)',T61,I2,/,
     &T21,'(1=PROVIDED, 0=NOT PROVIDED, OR DO NOT USE)',/,
     &10X,'OPTION 9: OVERWATER TURBULENCE DATA (Z-COMPONENT)',T61,I2,/,
     &T21,'(1=PROVIDED, 0=NOT PROVIDED, OR DO NOT USE)',//,
     &10X,'ANEMOMETER HEIGHT (ABOVE WATER LEVEL) FOR OVERWATER DATA = ',
     &F6.2,' METERS.',/,
     &10X,'AIR TEMPERATURE SENSOR HEIGHT (ABOVE WATER LEVEL) FOR OVERWAT
     &ER DATA = ',F6.2,' METERS.')
1860  FORMAT( / ,T31,'LATITUDE OF SITE = ',F8.3,' DEG (POSITIVE ',
     &  'NORTH OF EQUATOR)',/,T31,'LONGITUDE OF SITE = ',F8.3,
     &  ' DEG (POSITIVE WEST OF GREENWICH)',/,T31,'TIME ZONE, OR ',
     &  'NUMBER OF HOURS BEHIND GMT, IS ',F5.1,' (POSITIVE IF BEHIND ',
     &  'GMT)')
1870  FORMAT( / ,T31,'CLIMATOLOGICAL DAYTIME CHEMICAL TRANSFORMATION',
     &  ' RATES (%/HOUR) BY MONTH',//,
     &  T21,'  JAN     FEB     MAR     APR     MAY     JUN     JUL',
     &  '     AUG     SEP     OCT     NOV     DEC',//,T20,12F8.3,/)
1880    FORMAT(//////,20X,'LAND-WATER MAPPING:',/,20X,'COORDINATES OF ',
     &  'THE NORTHWEST CORNER OF THE MAP IN USER UNITS ARE (',F9.3,',',
     &  F9.3,')',/,20X,'# OF GRID RECTANGLES ALONG THE X-AXIS (I.E., ',
     &  'THE NUMBER OF GRID COLUMNS) = ',I2,/,20X,'# OF GRID RECTANGLE',
     &  'S ALONG THE Y-AXIS (I.E., THE NUMBER OF GRID ROWS) = ',I2,/,
     &  20X,'LENGTH OF THE (X,Y) SIDES OF A GRID RECTANGLE (USER ',
     &  'UNITS) = (',F9.3,',',F9.3,'), OR (',F9.3,',',F9.3,') KM.',/,
     &  20X,'MINIMUM SIGNIFICANT WIDTH OF LAND OR WATER BODY ALONG WIND'
     &  ,' DIRECTION (USER UNITS) = ',F9.3,/,
     &  20X,'AVERAGE DISTANCE BETWEEN SOURCE AND SHORELINE (USER UNITS)'
     &  ,' = ',F9.3,/)
c
c     Part II: Error messages
c     -----------------------
c
1000  FORMAT (/,' Error in OCD GROUP 16:',/
     &' DELX (KM) = ',F9.3,' IS INAPPROPRIATE FOR AVGDIST (KM) = ',f9.3,
     &'.')
1010  FORMAT (/,' Error in OCD GROUP 16:',/
     &' DELY (KM) = ',F9.3,' IS INAPPROPRIATE FOR AVGDIST (KM) = ',f9.3,
     &'.')
1015  FORMAT (/,' Error in OCD GROUP 2:',/
     &' INORMAL = ',I2,' SHOULD BE EITHER 0 or 1.')
1250  FORMAT (/,' Error in OCD Group 3:',/,
     &' NSIGP (THE NO. OF SIGNIFICANT POINT SOURCES) IS > 25.',/,
     &' THE USER TRIED TO INPUT ',I3,' SOURCES.')
1254  FORMAT (/,' Error in OCD Group 3:',/,
     &' NAVG (THE LENGTH OF AN AVERAGING PERIOD) WAS INPUT',
     &' AS ',I4,' HOURS;',/,
     &' IS NOT ALLOWED TO BE SMALLER THAN 1 HOUR.')
1255  FORMAT (/,' Error in OCD Group 3:',/,
     &' NAVG (THE LENGTH OF AN AVERAGING PERIOD) WAS INPUT',
     &' AS ',I4,' HOURS;',/,
     &' IS NOT ALLOWED TO EXCEED 24 HOURS.')
1257  FORMAT (/,' Error in OCD Group 4:',/,
     &' IOPT(7) CAN BE ONE, I.E., SIGNIFICANT SOURCES SPECIFIED BY THE U
     &SER,',/,
     &' ONLY IF NSIGP > 0.')
1258  FORMAT (/,' Warning in OCD Group 4:',/,
     &' IOPT(11) = 0, I.E., GENERATE PRINTOUT OF HOURLY CONTRIBUTIONS OF
     &',/,
     &' SIGNIFICANT SOURCES, IS MEANINGFUL ONLY IF NSIGP > 0.',/,
     &' IOPT(11) RESET TO 1.')
1261  FORMAT (/,' Warning in OCD Group 4:',/,
     &' WHEN NAVG = 1, BOTH IOPT(11) = 0 AND IOPT(17) = 0 GENERATE SIMIL
     &AR OUTPUT.',/,
     &' IOPT(17) RESET TO 1.')
1262  FORMAT (/,' Warning in OCD Group 4:',/,
     &' WHEN NAVG = 1, BOTH IOPT(14) = 0 AND IOPT(18) = 0 GENERATE SIMIL
     &AR OUTPUT.',/,
     &' IOPT(18) RESET TO 1.')
1275  FORMAT (/,' Error in OCD Group 6:',/,
     &' THE USER TRIED TO INPUT MORE THAN ',I5,' POINT SOURCES,',/,
     &' EXCEEDING CURRENT PROGRAM DIMENSIONS.')
1280  FORMAT (/,' Error in OCD Group 6:',/,
     &' NPT (THE NUMBER OF SOURCES) = ',I3,',I.E., EQUAL OR LESS',
     &' THAN ZERO.')
1300  FORMAT (/,' Error in OCD Group 7:',/,
     &' MPS, THE INPUT SIGNIFICANT SOURCE NUMBER,',
     &' WAS OUT OF BOUNDS.')
1310  FORMAT (/,' Error in OCD Group 8:',/,
     &' SURFACE DATA IDENTIFIERS READ INTO MODEL (STATION=',I6,
     &' ,YEAR=',I2,') DO NOT',/,
     &' AGREE WITH THE PREPROCESSOR OUTPUT FILE (STATION=',I6,
     &' ,YEAR=',I2,')')
1320  FORMAT (/,' Error in OCD Group 8:',/,
     &' MIXING HEIGHT IDENTIFIERS READ INTO MODEL (STATION=',I6,
     &' ,YEAR=',I2,') DO NOT',/,
     &' AGREE WITH THE PREPROCESSOR OUTPUT FILE (STATION=',I6,
     &' ,YEAR=',I2,')')
1327  FORMAT (/,' Error in OCD Group 9:',/,
     &' THE USER TRIED TO INPUT LESS THAN ONE RADIAL DISTANCE.')
1328  FORMAT (/,' Error in OCD Group 9:',/,
     &' THE USER TRIED TO INPUT MORE THAN ',I3,' RADIAL DISTANCES,',/,
     &' EXCEEDING CURRENT PROGRAM DIMENSIONS.')
1329  FORMAT (/,' Error in OCD Group 9:',/,
     &' THE USER TRIED TO INPUT NON-POSITIVE RADIAL DISTANCES = ',F13.4)
1330  FORMAT (/,' Error in OCD Group 10:',/,
     &' WRONG RECEPTOR ELEVATION CARD READ.',/,
     &' READ CARD FOR AZIMUTH ',I3,' SHOULD HAVE BEEN ',I3,'.')
1331  FORMAT (/,' Error in OCD Group 12:',/,
     &' WRONG RECEPTOR ELEVATION CARD READ.',/,
     &' READ CARD FOR ROW ',I3,' SHOULD HAVE BEEN ',I3,'.')
1339  FORMAT (/,' Error in OCD Group 13a:',/,
     &' THE USER TRIED TO INPUT LESS THAN ONE DISCRETE RECEPTOR.')
1340  FORMAT (/,' Error in OCD Group 9, 11, or 13:',/,
     &' THE USER TRIED TO INPUT MORE THAN ',i5,' RECEPTORS,',/,
     &' EXCEEDING CURRENT PROGRAM DIMENSIONS.')
1350  FORMAT (/,' Error in OCD Group 9, 11, or 13:',/,
     &' NO RECEPTORS HAVE BEEN CHOSEN.')
1605  FORMAT (/,' Error in OCD Group 6:',/,
     &' NPT = ',I3,',I.E., THE NUMBER OF AREA CIRCLES GREATER THAN 5.')
1606  FORMAT (/,' Error in OCD Group 6:',/,
     &' NPT = ',I3,',I.E., THE NUMBER OF LINE SOURCE GREATER THAN 1.')
1830  FORMAT (/,' Error in OCD Group 5:',/,
     &' INPUT LATITUDE VALUE OUT OF BOUNDS: ',F8.3,/,
     &' MUST BE BETWEEN -90 AND 90, INCLUSIVE.')
1831  FORMAT (/,' Warning in OCD Group 15:',/,
     &' INPUT LATITUDE VALUE (',F8.3,') IN GROUP 15 SHOULD BE THE',/,
     &' SAME AS INPUT LATITUDE VALUE (',F8.3,') IN GROUP 5.')
1840  FORMAT (/,' Error in OCD Group 15:',/,
     &' INPUT LONGITUDE VALUE OUT OF BOUNDS: ',F8.3,/,
     &' MUST BE BETWEEN -180 AND 180, INCLUSIVE.')
1850  FORMAT (/,' Error in OCD Group 15:',/,
     &' INPUT TIME ZONE VALUE OUT OF BOUNDS: ',F5.1,/,
     &' MUST BE BETWEEN -12 AND 11, INCLUSIVE.')
1851  FORMAT (/,' Error in OCD Group 16:',/,
     &' SHORELINE GEOMETRY INDICATOR MUST BE EITHER ''W'' OR ''L''.')
2001  FORMAT (/,' Error in OCD Group 3:',/,
     &' IDATE(1) (THE STARTING YEAR FOR THIS RUN) WAS INPUT',
     &' AS ',I4,/,
     &' MUST BE BETWEEN 0 AND 99, INCLUSIVE.')
2003  FORMAT (/,' Error in OCD Group 3:',/,
     &' IDATE(2) (THE STARTING JULIAN DAY FOR THIS RUN) WAS INPUT',
     &' AS ',I4,/,
     &' MUST BE BETWEEN 1 AND 366, INCLUSIVE.')
2005  FORMAT (/,' Error in OCD Group 3:',/,
     &' IHSTRT (THE STARTING HOUR FOR THIS RUN) WAS INPUT',
     &' AS ',I4,/,
     &' MUST BE BETWEEN 1 AND 24, INCLUSIVE.')
2007  FORMAT (/,' Error in OCD Group 3:',/,
     &' IPOL (THE POLLUTANT INDICATOR) WAS INPUT',
     &' AS ',I4,/,
     &' MUST BE BETWEEN 3 AND 7, INCLUSIVE.')
2008  FORMAT (/,' Error in OCD Group 3:',/,
     &' NAV5 (OPTIONAL FIFTH AVERAGING TIME) WAS INPUT',
     &' AS ',I4,/,
     &' MUST BE BETWEEN 0 AND 24, INCLUSIVE.')
2009  FORMAT (/,' Error in OCD Group 3:',/,
     &' CONTWO (CONVERSION FACTOR FOR HORIZONTAL USER UNITS) WAS INPUT',
     &' AS ',1P,E14.6,/,
     &' MUST BE POSITIVE.')
2011  FORMAT (/,' Error in OCD Group 3:',/,
     &' CELM (CONVERSION FACTOR FOR VERTICAL USER UNITS) WAS INPUT',
     &' AS ',1P,E14.6,/,
     &' MUST BE POSITIVE.')
2013  FORMAT (/,' Error in OCD Group 5:',/,
     &' HANE (OVERLAND ANEMOMETER HEIGHT) WAS INPUT',
     &' AS ',F12.5,' M',/,
     &' MUST BE POSITIVE.')
2015  FORMAT (/,' Error in OCD Group 5:',/,
     &' ZOL (OVERLAND SURFACE ROUGHNESS) WAS INPUT',
     &' AS ',F12.5,' M',/,
     &' MUST BE POSITIVE.')
2017  FORMAT (/,' Error in OCD Group 5:',/,
     &' ZMIN (MINIMUM MISS DISTANCE) WAS INPUT',
     &' AS ',F12.5,' M',/,
     &' MUST BE NON-NEGATIVE.')
2101  FORMAT (/,' Error in OCD Group 6b:',/,
     &' POLLUTANT EMISSION RATE FOR SOURCE ',I4,' WAS INPUT',
     &' AS ',F12.5,' G/S',/,
     &' MUST BE NON-NEGATIVE.')
2103  FORMAT (/,' Error in OCD Group 6b:',/,
     &' HEIGHT OF NEARBY BUILDING/OBSTACLE FOR SOURCE ',I4,' WAS INPUT',
     &' AS ',F12.5,' M',/,
     &' MUST BE NON-NEGATIVE.')
2105  FORMAT (/,' Error in OCD Group 6b:',/,
     &' STACK HEIGHT ABOVE GROUND FOR SOURCE ',I4,' WAS INPUT',
     &' AS ',F12.5,' M',/,
     &' MUST BE POSITIVE.1 M.')
2107  FORMAT (/,' Error in OCD Group 6b:',/,
     &' STACK EXIT TEMPERATURE FOR SOURCE ',I4,' WAS INPUT',
     &' AS ',F12.5,' K',/,
     &' APPEARS TOO LOW.')
2109  FORMAT (/,' Error in OCD Group 6b:',/,
     &' STACK DIAMETER FOR SOURCE ',I4,' WAS INPUT',
     &' AS ',F12.5,' M',/,
     &' MUST BE POSITIVE.')
2111  FORMAT (/,' Error in OCD Group 6b:',/,
     &' STACK EXIT VELOCITY FOR SOURCE ',I4,' WAS INPUT',
     &' AS ',F12.5,' M/S',/,
     &' MUST BE POSITIVE.')
2113  FORMAT (/,' Error in OCD Group 6b:',/,
     &' STACK VERTICAL DEVIATION ANGLE FOR SOURCE ',I4,' WAS INPUT',
     &' AS ',F12.5,' DEG',/,
     &' MUST BE BETWEEN 0 AND 180 DEG, INCLUSIVE.')
2115  FORMAT (/,' Error in OCD Group 6b:',/,
     &' NEARBY BUILDING WIDTH FOR SOURCE ',I4,' WAS INPUT',
     &' AS ',F12.5,' M',/,
     &' MUST BE NON-NEGATIVE.')
2121  FORMAT (/,' Error in OCD Group 8:',/,
     &' ISFCD (SURFACE STATION ID) WAS INPUT',
     &' AS ',I7,/,
     &' MUST BE BETWEEN 0 AND 99999, INCLUSIVE.')
2123  FORMAT (/,' Error in OCD Group 8:',/,
     &' ISFCYR (YEAR OF SURFACE DATA) WAS INPUT',
     &' AS ',I7,/,
     &' MUST BE BETWEEN 0 AND 99, INCLUSIVE.')
2125  FORMAT (/,' Error in OCD Group 8:',/,
     &' IMXD (UPPER AIR STATION ID) WAS INPUT',
     &' AS ',I7,/,
     &' MUST BE BETWEEN 0 AND 99999, INCLUSIVE.')
2127  FORMAT (/,' Error in OCD Group 8:',/,
     &' IMXYR (YEAR OF UPPER AIR DATA) WAS INPUT',
     &' AS ',I7,/,
     &' MUST BE BETWEEN 0 AND 99, INCLUSIVE.')
2131  FORMAT (/,' Error in OCD Group 11:',/,
     &' NX_C (NUMBER OF CARTESIAN RECEPTORS ALONG X-AXIS) WAS INPUT',
     &' AS ',I4,/,
     &' MUST BE BETWEEN 1 AND ',I3,', INCLUSIVE.')
2133  FORMAT (/,' Error in OCD Group 11:',/,
     &' NY_C (NUMBER OF CARTESIAN RECEPTORS ALONG Y-AXIS) WAS INPUT',
     &' AS ',I4,/,
     &' MUST BE BETWEEN 1 AND ',I3,', INCLUSIVE.')
2135  FORMAT (/,' Error in OCD Group 11:',/,
     &' DELX_C (DISTANCE BETWEEN SUCCESSIVE CARTESIAN RECEPTORS ALONG',
     &' X-AXIS) WAS INPUT',
     &' AS ',F12.5,/,
     &' MUST BE POSITIVE.')
2137  FORMAT (/,' Error in OCD Group 11:',/,
     &' DELY_C (DISTANCE BETWEEN SUCCESSIVE CARTESIAN RECEPTORS ALONG',
     &' Y-AXIS) WAS INPUT',
     &' AS ',F12.5,/,
     &' MUST BE POSITIVE.')
2201  FORMAT (/,' Error in OCD Group 13b:',/,
     &' RECEPTOR HEIGHT ABOVE GROUND FOR RECEPTOR ',I4,' WAS INPUT',
     &' AS ',F12.5,' M',/,
     &' MUST BE NON-NEGATIVE.')
2203  FORMAT (/,' Error in OCD Group 14:',/,
     &' HWANE (OVERWATER ANEMOMETER HEIGHT) WAS INPUT',
     &' AS ',F12.5,' M',/,
     &' MUST BE POSITIVE.')
2205  FORMAT (/,' Error in OCD Group 14:',/,
     &' HWT (OVERWATER AIR TEMPERATURE SENSOR HEIGHT) WAS INPUT',
     &' AS ',F12.5,' M',/,
     &' MUST BE POSITIVE.')
2207  FORMAT (/,' Error in OCD Group 15:',/,
     &' POLLUTANT DECAY RATE FOR THE ',I2,'TH MONTH WAS INPUT',
     &' AS ',F12.5,' %/HOUR',/,
     &' MUST BE NON-NEGATIVE.')
2731  FORMAT (/,' Error in OCD Group 16:',/,
     &' NX (NUMBER OF MAP GRIDS ALONG X-AXIS) WAS INPUT',
     &' AS ',I4,/,
     &' MUST BE BETWEEN 1 AND ',I3,', INCLUSIVE.')
2733  FORMAT (/,' Error in OCD Group 16:',/,
     &' NY (NUMBER OF MAP GRIDS ALONG Y-AXIS) WAS INPUT',
     &' AS ',I4,/,
     &' MUST BE BETWEEN 1 AND ',I3,', INCLUSIVE.')
2735  FORMAT (/,' Error in OCD Group 16:',/,
     &' DELX (DISTANCE BETWEEN SUCCESSIVE MAP GRIDS ALONG',
     &' X-AXIS) WAS INPUT',
     &' AS ',F12.5,/,
     &' MUST BE POSITIVE.')
2737  FORMAT (/,' Error in OCD Group 16:',/,
     &' DELY (DISTANCE BETWEEN SUCCESSIVE MAP GRIDS ALONG',
     &' Y-AXIS) WAS INPUT',
     &' AS ',F12.5,/,
     &' MUST BE POSITIVE.')
2739  FORMAT (/,' Error in OCD Group 16:',/,
     &' WMIN (MINIMUM ALONG-WIND WIDTH FOR A LAND OR WATER BODY TO BE',
     &' CONSIDERED SIGNIFICANT) WAS INPUT',
     &' AS ',F12.5,/,
     &' MUST BE POSITIVE.')
2741  FORMAT (/,' Error in OCD Group 16:',/,
     &' AVGDIST (AVERAGE DISTANCE FROM SOURCE TO SHORELINE) WAS INPUT',
     &' AS ',F12.5,/,
     &' MUST BE POSITIVE.')
c
c *** Stop execution if it is a test run.
c
      if (inormal.eq.0) then
         stop 'Test run completed, check ERROR.OUT to see if any errors 
     &have occured'
      else
         if (iopt(22).eq.1) then
            write (12) line1,line2,line3
            write (12) nrecep,(rrec(k),srec(k),k=1,nrecep)
         end if
         return
      end if
c
      end
c----------------------------------------------------------------------
      subroutine readin(cvdic,ivleng,ivtype,ioin,ioout,lecho,inormal,
     1 i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15,i16,i17,i18,
     2 i19,i20,i21,i22,i23,i24,i25,i26,i27,i28,i29,i30,i31,i32,i33,i34,
     3 i35,i36,i37,i38,i39,i40,i41,i42,i43,i44,i45,i46,i47,i48,i49,i50,
     4 i51,i52,i53,i54,i55,i56,i57,i58,i59,i60)
c----------------------------------------------------------------------
c
c --- CALPUFF    Version:  4.0     Level:  950122                READIN
c                J. Scire, SRC
c
c --- PURPOSE:  Read one input group of the free formatted control
c               file -- allows comments within the input file --
c               ignores all text except that within delimiters
c
c ---           NOTE:  All variables (real, integer, logical,
c                      or character) must be 4 bytes
c ---           NOTE:  Character*4 array uses only one character
c                      per word -- it must be dimensioned large
c                      enough to accommodate the number of characters
c                      in the variable field
c
c --- INPUTS:
c
c     CVDIC(maxvar) - character*12 array - Variable dictionary
c                                         containing up to "maxvar"
c                                         variable names
c    IVLENG(maxvar) - integer array      - Dimension of each variable
c                                         (dim. of scalars = 1)
c    IVTYPE(maxvar) - integer array      - Type of each variable
c                                           1 = real,
c                                           2 = integer,
c                                           3 = logical,
c                                           4 = character*4
c             IOIN - integer            - Fortran unit of control file
c                                         input
c            IOOUT - integer            - Fortran unit of list file
c                                         output
c            LECHO - logical            - Control variable determining
c                                         if input data are echoed to
c                                         list file (IOOUT)
c          INORMAL - integer            - =1, normal run;
c                                         =0, test run
c        Parameters: maxvar, MXCOL
c
c --- OUTPUT:
c
c      I1, I2, ... - integer arrays     - Variables being read
c                    (integer array locally, but can be a real,
c                     integer, logical, or character*4 array in
c                     the calling routine)
c
c --- READIN called by:  READCF
c --- READIN calls:      DEBLNK, ALTONU, SETVAR, ALLCAP
c----------------------------------------------------------------------
c
c --- Include parameter statements
      include 'params.cmn'
c
      integer*4 i1(*),i2(*),i3(*),i4(*),i5(*),i6(*),i7(*),i8(*),i9(*),
     1 i10(*),i11(*),i12(*),i13(*),i14(*),i15(*),i16(*),i17(*),i18(*),
     2 i19(*),i20(*),i21(*),i22(*),i23(*),i24(*),i25(*),i26(*),i27(*),
     3 i28(*),i29(*),i30(*),i31(*),i32(*),i33(*),i34(*),i35(*),i36(*),
     4 i37(*),i38(*),i39(*),i40(*),i41(*),i42(*),i43(*),i44(*),i45(*),
     5 i46(*),i47(*),i48(*),i49(*),i50(*),i51(*),i52(*),i53(*),i54(*),
     6 i55(*),i56(*),i57(*),i58(*),i59(*),i60(*)
      integer*4 ivleng(maxvar),jdex(maxvar),ivtype(maxvar)
c
      logical*4 lv
      logical lecho
c
      character*12 cvdic(maxvar),cvar,cblank
      character*4 cv(mxcol)
      character*1 cstor1(mxcol),cstor2(mxcol)
      character*1 cdelim,ceqls,ce,cn,cd,comma
c
      data cblank/'            '/
      data cdelim/'!'/,ceqls/'='/,ce/'E'/,cn/'N'/,cd/'D'/,comma/','/
c
      ilim2=99
      do 2 i=1,maxvar
      jdex(i)=1
2     continue
c
c --- begin loop over lines
c
c --- read a line of input
5     continue
      read(ioin,10)cstor1
10    format(150a1)
      if(lecho)write(ioout,7)cstor1
7     format(1x,150a1)
c
c --- check if this is a continuation line
      if(ilim2.gt.0)go to 16
c
c --- continuation line -- find the second delimiter
      do 12 i=1,mxcol
      if(cstor1(i).eq.cdelim)then
         ilim2=i
         go to 14
      endif
12    continue
14    continue
      il2=ilim2
      if(il2.eq.0)il2=mxcol
c
c --- remove blank characters from string within delimiters
      call deblnk(cstor1,1,il2,cstor2,nlim)
      icom=0
c
c --- convert lower case letters to upper case
      call allcap(cstor2,nlim)
      go to 55
16    continue
      ibs=1
c
c --- begin loop over delimiter pairs
17    continue
      if(ibs.ge.mxcol)go to 5
c
c --- find location of delimiters
      do 20 i=ibs,mxcol
      if(cstor1(i).eq.cdelim)then
         ilim1=i
         if(ilim1.eq.mxcol)go to 22
         ip1=ilim1+1
         do 18 j=ip1,mxcol
         if(cstor1(j).eq.cdelim)then
            ilim2=j
            go to 22
         endif
18       continue
c
c ---    second delimiter not on this line
         ilim2=0
         go to 22
      endif
20    continue
c
c --- no delimiters found -- skip line and read next line of text
      go to 5
22    continue
      ibs=ilim2+1
      if(ilim2.eq.0)ibs=mxcol+1
c
c --- remove blanks from string within delimiters
      il2=ilim2
      if(il2.eq.0)il2=mxcol
      call deblnk(cstor1,ilim1,il2,cstor2,nlim)
c
c --- convert lower case letters to upper case
      call allcap(cstor2,nlim)
c
c --- search for equals sign (cstor2(1) is delimiter; cstor2(2) is
c --- first letter of variable; cstor2(3) is earliest '=' can occur)
      do 30 i=3,14
      if(cstor2(i).eq.ceqls)then
         ieq=i
         go to 32
      endif
30    continue
c
c --- "END" within delimiters signifies the end of the read for
c --- this input group
      if(cstor2(2).eq.ce.and.cstor2(3).eq.cn.and.cstor2(4).eq.cd)return
      write(6,31)    (cstor2(n),n=1,nlim)
      write(ioout,31)(cstor2(n),n=1,nlim)
31    format(/1x,'ERROR IN SUBR. READIN -- Error in input data -- '/
     1 1x,'Variable too long (Equals sign not found in string) -- ',
     2 'CSTOR2 = ',200a1)
      if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for more
     & details.'
c     Stop for normal run.
c
c --- CVAR is character*12 variable name
32    continue
      cvar=cblank
      ieqm1=ieq-1
      do 40 i=2,ieqm1
      il=i-1
      cvar(il:il)=cstor2(i)
40    continue
c
c --- find the variable name in the variable dictionary
      do 50 i=1,maxvar
      if(cvar.eq.cvdic(i))then
         nvar=i
         go to 52
      endif
50    continue
      write(6,51)    cvar,(cvdic(n),n=1,maxvar)
      write(ioout,51)cvar,(cvdic(n),n=1,maxvar)
51    format(/1x,'ERROR IN SUBR. READIN -- Error in input data -- '/
     1 1x,'Variable not found in variable dictionary'/
     2 1x,'Variable: ',a12/
     3 1x,'Variable Dictionary: ',9(a12,1x)/
     4 10(22x,9(a12,1x)/))
      if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for more
     & details.'
c     Stop for normal run.
c
c --- search for comma
52    continue
      icom=ieq
c
c --- beginning of loop over values within delimiters
55    continue
      ivb=icom+1
c
c --- if reaches end of line, read next line
      if(ivb.gt.nlim)go to 5
      do 60 i=ivb,nlim
      if(cstor2(i).eq.comma)then
         icom=i
         go to 64
      endif
60    continue
c
c --- no comma found
      icom=0
      ive=nlim-1
c
c --- comma between last value and delimiter is allowed
      if(cstor2(ivb).eq.cdelim.and.cstor2(ive).eq.comma)go to 17
c
c --- if no comma & last non-blank character is not a delimiter,
c --- then the input is in error
      if(cstor2(nlim).eq.cdelim)go to 66
      write(6,63)    cstor1
      write(ioout,63)cstor1
63    format(/1x,'ERROR IN SUBR. READIN -- Error in input data -- '/
     1 1x,'If a string within delimiters covers more than one line, ',
     2 'the last character in the line must be a comma'/
     3 1x,'Input line: ',200a1)
      if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for more
     & details.'
c     Stop for normal run.
64    continue
c
c --- value of variable is contained in elements IVB to IVE of
c --- CSTOR2 array
      ive=icom-1
66    continue
      ncar=ive-ivb+1
      index=jdex(nvar)
      itype=ivtype(nvar)
c
c --- Check for invalid value of variable type
      if(itype.le.0.or.itype.ge.5)then
         write(6,68)    itype,nvar,ivtype(nvar),cvdic(nvar)
         write(ioout,68)itype,nvar,ivtype(nvar),cvdic(nvar)
68       format(/1x,'ERROR IN SUBR. READIN -- Error in input data -- '/
     1   1x,'Invalid value of variable type -- ITYPE must be 1, 2, 3, ',
     2   'or 4'/1x,'ITYPE = ',i10/1x,'NVAR = ',i10/1x,'IVTYPE(nvar) = ',
     3   i10/1x,'CVDIC(nvar) = ',a12)
         if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for m
     &ore details.'
c        Stop for normal run.
      endif
c
c --- Convert character string to numeric or logical value
c     (if ITYPE = 1,2, or 3 -- If 4, transfer characters to the
c     work array CV)
c
      call altonu(ioout,cstor2(ivb),ncar,itype,inormal,irep,rlno,
     &            ino,lv,cv)
c
c --- check that array bounds are not exceeded
      if(index+irep-1.gt.ivleng(nvar))go to 201
c
      go to (101,102,103,104,105,106,107,108,109,110,111,112,113,114,
     1 115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,
     2 131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,
     3 147,148,149,150,151,152,153,154,155,156,157,158,159,160),nvar
c
c --- code currently set up to handle up to 60 variables/source group
      write(6,71)    nvar,(cstor2(n),n=1,nlim)
      write(ioout,71)nvar,(cstor2(n),n=1,nlim)
71    format(/1x,'ERROR IN SUBR. READIN -- Current code ',
     1 'configuration allows up to 60 variables per source group'/
     2 1x,'No. variables (NVAR) = ',i10/
     3 1x,'Input data (CSTOR2)  = ',200a1)
      if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for more
     & details.'
c     Stop for normal run.
c
c --- transfer value into output variable
101   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i1(index),i1(index),
     1 i1(index),i1(index))
      go to 161
102   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i2(index),i2(index),
     1 i2(index),i2(index))
      go to 161
103   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i3(index),i3(index),
     1 i3(index),i3(index))
      go to 161
104   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i4(index),i4(index),
     1 i4(index),i4(index))
      go to 161
105   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i5(index),i5(index),
     1 i5(index),i5(index))
      go to 161
106   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i6(index),i6(index),
     1 i6(index),i6(index))
      go to 161
107   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i7(index),i7(index),
     1 i7(index),i7(index))
      go to 161
108   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i8(index),i8(index),
     1 i8(index),i8(index))
      go to 161
109   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i9(index),i9(index),
     1 i9(index),i9(index))
      go to 161
110   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i10(index),i10(index),
     1 i10(index),i10(index))
      go to 161
111   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i11(index),i11(index),
     1 i11(index),i11(index))
      go to 161
112   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i12(index),i12(index),
     1 i12(index),i12(index))
      go to 161
113   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i13(index),i13(index),
     1 i13(index),i13(index))
      go to 161
114   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i14(index),i14(index),
     1 i14(index),i14(index))
      go to 161
115   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i15(index),i15(index),
     1 i15(index),i15(index))
      go to 161
116   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i16(index),i16(index),
     1 i16(index),i16(index))
      go to 161
117   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i17(index),i17(index),
     1 i17(index),i17(index))
      go to 161
118   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i18(index),i18(index),
     1 i18(index),i18(index))
      go to 161
119   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i19(index),i19(index),
     1 i19(index),i19(index))
      go to 161
120   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i20(index),i20(index),
     1 i20(index),i20(index))
      go to 161
121   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i21(index),i21(index),
     1 i21(index),i21(index))
      go to 161
122   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i22(index),i22(index),
     1 i22(index),i22(index))
      go to 161
123   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i23(index),i23(index),
     1 i23(index),i23(index))
      go to 161
124   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i24(index),i24(index),
     1 i24(index),i24(index))
      go to 161
125   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i25(index),i25(index),
     1 i25(index),i25(index))
      go to 161
126   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i26(index),i26(index),
     1 i26(index),i26(index))
      go to 161
127   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i27(index),i27(index),
     1 i27(index),i27(index))
      go to 161
128   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i28(index),i28(index),
     1 i28(index),i28(index))
      go to 161
129   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i29(index),i29(index),
     1 i29(index),i29(index))
      go to 161
130   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i30(index),i30(index),
     1 i30(index),i30(index))
      go to 161
131   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i31(index),i31(index),
     1 i31(index),i31(index))
      go to 161
132   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i32(index),i32(index),
     1 i32(index),i32(index))
      go to 161
133   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i33(index),i33(index),
     1 i33(index),i33(index))
      go to 161
134   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i34(index),i34(index),
     1 i34(index),i34(index))
      go to 161
135   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i35(index),i35(index),
     1 i35(index),i35(index))
      go to 161
136   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i36(index),i36(index),
     1 i36(index),i36(index))
      go to 161
137   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i37(index),i37(index),
     1 i37(index),i37(index))
      go to 161
138   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i38(index),i38(index),
     1 i38(index),i38(index))
      go to 161
139   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i39(index),i39(index),
     1 i39(index),i39(index))
      go to 161
140   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i40(index),i40(index),
     1 i40(index),i40(index))
      go to 161
141   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i41(index),i41(index),
     1 i41(index),i41(index))
      go to 161
142   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i42(index),i42(index),
     1 i42(index),i42(index))
      go to 161
143   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i43(index),i43(index),
     1 i43(index),i43(index))
      go to 161
144   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i44(index),i44(index),
     1 i44(index),i44(index))
      go to 161
145   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i45(index),i45(index),
     1 i45(index),i45(index))
      go to 161
146   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i46(index),i46(index),
     1 i46(index),i46(index))
      go to 161
147   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i47(index),i47(index),
     1 i47(index),i47(index))
      go to 161
148   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i48(index),i48(index),
     1 i48(index),i48(index))
      go to 161
149   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i49(index),i49(index),
     1 i49(index),i49(index))
      go to 161
150   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i50(index),i50(index),
     1 i50(index),i50(index))
      go to 161
151   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i51(index),i51(index),
     1 i51(index),i51(index))
      go to 161
152   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i52(index),i52(index),
     1 i52(index),i52(index))
      go to 161
153   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i53(index),i53(index),
     1 i53(index),i53(index))
      go to 161
154   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i54(index),i54(index),
     1 i54(index),i54(index))
      go to 161
155   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i55(index),i55(index),
     1 i55(index),i55(index))
      go to 161
156   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i56(index),i56(index),
     1 i56(index),i56(index))
      go to 161
157   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i57(index),i57(index),
     1 i57(index),i57(index))
      go to 161
158   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i58(index),i58(index),
     1 i58(index),i58(index))
      go to 161
159   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i59(index),i59(index),
     1 i59(index),i59(index))
      go to 161
160   continue
      call setvar(itype,irep,rlno,ino,lv,cv,i60(index),i60(index),
     1 i60(index),i60(index))
c
161   continue
      jdex(nvar)=jdex(nvar)+irep
c
c --- continue reading values for this array until array is filled
c --- or delimiter is reached
      if(icom.ne.0.and.jdex(nvar).le.ivleng(nvar))go to 55
      go to 17
201   continue
      iatt=index+irep-1
      write(6,202)    cvdic(nvar),ivleng(nvar),iatt,cstor1
      write(ioout,202)cvdic(nvar),ivleng(nvar),iatt,cstor1
202   format(/1x,'ERROR IN SUBR. READIN -- Error in input data',
     1 1x,'Array bounds exceeded -- Variable: ',a12,3x,' Declared ',
     2 'dimension = ',i8/1x,'Input attempted to element ',i8/1x,
     3 'Input line: ',200a1)
      if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for more
     & details.'
c     Stop for normal run.
      end
c----------------------------------------------------------------------
      subroutine readin2(cvdic,ivleng,ivtype,ioin,ioout,lecho,
     &                   inormal,i1)
c----------------------------------------------------------------------
c
c     A speical version of READIN that handles only one character array,
c     I1, at a time.  The array I1 is explicitly declared as CHARACTER
c     in both the calling and called routines.  Note that READIN2 calls
c     SETVAR2, rather than SETVAR.
c
c     Joseph Chang, EARTH TECH, Level:  961111.
c
c --- CALPUFF    Version:  4.0     Level:  950122                READIN
c                J. Scire, SRC
c
c --- PURPOSE:  Read one input group of the free formatted control
c               file -- allows comments within the input file --
c               ignores all text except that within delimiters
c
c ---           NOTE:  All variables (real, integer, logical,
c                      or character) must be 4 bytes
c ---           NOTE:  Character*4 array uses only one character
c                      per word -- it must be dimensioned large
c                      enough to accommodate the number of characters
c                      in the variable field
c
c --- INPUTS:
c
c     CVDIC(maxvar) - character*12 array - Variable dictionary
c                                         containing up to "maxvar"
c                                         variable names
c    IVLENG(maxvar) - integer array      - Dimension of each variable
c                                         (dim. of scalars = 1)
c    IVTYPE(maxvar) - integer array      - Type of each variable
c                                           1 = real,
c                                           2 = integer,
c                                           3 = logical,
c                                           4 = character*4
c             IOIN - integer            - Fortran unit of control file
c                                         input
c            IOOUT - integer            - Fortran unit of list file
c                                         output
c            LECHO - logical            - Control variable determining
c                                         if input data are echoed to
c                                         list file (IOOUT)
c          INORMAL - integer            - =1, normal run;
c                                         =0, test run
c        Parameters: maxvar, MXCOL
c
c --- OUTPUT:
c
c      I1          - character array     - Variables being read
c
c --- READIN2 called by:  READCF
c --- READIN2 calls:      DEBLNK, ALTONU, SETVAR2, ALLCAP
c----------------------------------------------------------------------
c
c --- Include parameter statements
      include 'params.cmn'
c
      integer*4 ivleng,jdex,ivtype
      character*4 i1(ivleng)
c
      logical*4 lv
      logical lecho
c
      character*12 cvdic,cvar,cblank
      character*4 cv(mxcol)
      character*1 cstor1(mxcol),cstor2(mxcol)
      character*1 cdelim,ceqls,ce,cn,cd,comma
c
      data cblank/'            '/
      data cdelim/'!'/,ceqls/'='/,ce/'E'/,cn/'N'/,cd/'D'/,comma/','/
c
      ilim2=99
      jdex=1
c
c --- begin loop over lines
c
c --- read a line of input
5     continue
      read(ioin,10)cstor1
10    format(150a1)
      if(lecho)write(ioout,7)cstor1
7     format(1x,150a1)
c
c --- check if this is a continuation line
      if(ilim2.gt.0)go to 16
c
c --- continuation line -- find the second delimiter
      do 12 i=1,mxcol
      if(cstor1(i).eq.cdelim)then
         ilim2=i
         go to 14
      endif
12    continue
14    continue
      il2=ilim2
      if(il2.eq.0)il2=mxcol
c
c --- remove blank characters from string within delimiters
      call deblnk(cstor1,1,il2,cstor2,nlim)
      icom=0
c
c --- convert lower case letters to upper case
      call allcap(cstor2,nlim)
      go to 55
16    continue
      ibs=1
c
c --- begin loop over delimiter pairs
17    continue
      if(ibs.ge.mxcol)go to 5
c
c --- find location of delimiters
      do 20 i=ibs,mxcol
      if(cstor1(i).eq.cdelim)then
         ilim1=i
         if(ilim1.eq.mxcol)go to 22
         ip1=ilim1+1
         do 18 j=ip1,mxcol
         if(cstor1(j).eq.cdelim)then
            ilim2=j
            go to 22
         endif
18       continue
c
c ---    second delimiter not on this line
         ilim2=0
         go to 22
      endif
20    continue
c
c --- no delimiters found -- skip line and read next line of text
      go to 5
22    continue
      ibs=ilim2+1
      if(ilim2.eq.0)ibs=mxcol+1
c
c --- remove blanks from string within delimiters
      il2=ilim2
      if(il2.eq.0)il2=mxcol
      call deblnk(cstor1,ilim1,il2,cstor2,nlim)
c
c --- convert lower case letters to upper case
      call allcap(cstor2,nlim)
c
c --- search for equals sign (cstor2(1) is delimiter; cstor2(2) is
c --- first letter of variable; cstor2(3) is earliest '=' can occur)
      do 30 i=3,14
      if(cstor2(i).eq.ceqls)then
         ieq=i
         go to 32
      endif
30    continue
c
c --- "END" within delimiters signifies the end of the read for
c --- this input group
      if(cstor2(2).eq.ce.and.cstor2(3).eq.cn.and.cstor2(4).eq.cd)return
      write(6,31)    (cstor2(n),n=1,nlim)
      write(ioout,31)(cstor2(n),n=1,nlim)
31    format(/1x,'ERROR IN SUBR. READIN2 -- Error in input data -- '/
     1 1x,'Variable too long (Equals sign not found in string) -- ',
     2 'CSTOR2 = ',200a1)
      if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for more
     & details.'
c     Stop for normal run.
c
c --- CVAR is character*12 variable name
32    continue
      cvar=cblank
      ieqm1=ieq-1
      do 40 i=2,ieqm1
      il=i-1
      cvar(il:il)=cstor2(i)
40    continue
c
c --- find the variable name in the variable dictionary
      if(cvar.eq.cvdic)then
         nvar=1
         go to 52
      endif
50    continue
      write(6,51)    cvar,cvdic
      write(ioout,51)cvar,cvdic
51    format(/1x,'ERROR IN SUBR. READIN2 -- Error in input data -- '/
     1 1x,'Variable not found in variable dictionary'/
     2 1x,'Variable: ',a12/
     3 1x,'Variable Dictionary: ',9(a12,1x)/
     4 10(22x,9(a12,1x)/))
      if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for more
     & details.'
c     Stop for normal run.
c
c --- search for comma
52    continue
      icom=ieq
c
c --- beginning of loop over values within delimiters
55    continue
      ivb=icom+1
c
c --- if reaches end of line, read next line
      if(ivb.gt.nlim)go to 5
      do 60 i=ivb,nlim
      if(cstor2(i).eq.comma)then
         icom=i
         go to 64
      endif
60    continue
c
c --- no comma found
      icom=0
      ive=nlim-1
c
c --- comma between last value and delimiter is allowed
      if(cstor2(ivb).eq.cdelim.and.cstor2(ive).eq.comma)go to 17
c
c --- if no comma & last non-blank character is not a delimiter,
c --- then the input is in error
      if(cstor2(nlim).eq.cdelim)go to 66
      write(6,63)    cstor1
      write(ioout,63)cstor1
63    format(/1x,'ERROR IN SUBR. READIN2 -- Error in input data -- '/
     1 1x,'If a string within delimiters covers more than one line, ',
     2 'the last character in the line must be a comma'/
     3 1x,'Input line: ',200a1)
      if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for more
     & details.'
c     Stop for normal run.
64    continue
c
c --- value of variable is contained in elements IVB to IVE of
c --- CSTOR2 array
      ive=icom-1
66    continue
      ncar=ive-ivb+1
      index=jdex
      itype=ivtype
c
c --- Check for invalid value of variable type
      if(itype.le.0.or.itype.ge.5)then
         write(6,68)    itype,nvar,ivtype,cvdic
         write(ioout,68)itype,nvar,ivtype,cvdic
68       format(/1x,'ERROR IN SUBR. READIN2 -- Error in input data -- '/
     1   1x,'Invalid value of variable type -- ITYPE must be 1, 2, 3, ',
     2   'or 4'/1x,'ITYPE = ',i10/1x,'NVAR = ',i10/1x,'IVTYPE(nvar) = ',
     3   i10/1x,'CVDIC(nvar) = ',a12)
         if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for m
     &ore details.'
c        Stop for normal run.
      endif
c
c --- Convert character string to numeric or logical value
c     (if ITYPE = 1,2, or 3 -- If 4, transfer characters to the
c     work array CV)
c
      call altonu(ioout,cstor2(ivb),ncar,itype,inormal,irep,rlno,
     &            ino,lv,cv)
c
c --- check that array bounds are not exceeded
      if(index+irep-1.gt.ivleng)go to 201
c
      go to (101) nvar
c
c --- code currently set up to handle up to 60 variables/source group
      write(6,71)    nvar,(cstor2(n),n=1,nlim)
      write(ioout,71)nvar,(cstor2(n),n=1,nlim)
71    format(/1x,'ERROR IN SUBR. READIN2 -- Current code ',
     1 'configuration allows up to 60 variables per source group'/
     2 1x,'No. variables (NVAR) = ',i10/
     3 1x,'Input data (CSTOR2)  = ',200a1)
      if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for more
     & details.'
c     Stop for normal run.
c
c --- transfer value into output variable
101   continue
      call setvar2(irep,cv,i1(index))
c
161   continue
      jdex=jdex+irep
c
c --- continue reading values for this array until array is filled
c --- or delimiter is reached
      if(icom.ne.0.and.jdex.le.ivleng)go to 55
      go to 17
201   continue
      iatt=index+irep-1
      write(6,202)    cvdic,ivleng,iatt,cstor1
      write(ioout,202)cvdic,ivleng,iatt,cstor1
202   format(/1x,'ERROR IN SUBR. READIN2 -- Error in input data',
     1 1x,'Array bounds exceeded -- Variable: ',a12,3x,' Declared ',
     2 'dimension = ',i8/1x,'Input attempted to element ',i8/1x,
     3 'Input line: ',200a1)
      if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for more
     & details.'
c     Stop for normal run.
      end
c----------------------------------------------------------------------
      subroutine readin3(ioin,ioout,lecho,inormal)
c----------------------------------------------------------------------
c
c     A speical version of READIN that searches the header record for
c     the overland meteorological data embedded in the control file.
c     The header record should look like !INPUT GROUP 17!
c
c     Joseph Chang, EARTH TECH, Level:  961111.
c
c --- CALPUFF    Version:  4.0     Level:  950122                READIN
c                J. Scire, SRC
c
c --- INPUTS:
c
c             IOIN - integer            - Fortran unit of control file
c                                         input
c            IOOUT - integer            - Fortran unit of list file
c                                         output
c            LECHO - logical            - Control variable determining
c                                         if input data are echoed to
c                                         list file (IOOUT)
c          INORMAL - integer            - =1, normal run;
c                                         =0, test run
c        Parameters: maxvar, MXCOL
c
c --- OUTPUT:
c
c            None
c
c --- READIN3 called by:  READCF
c --- READIN3 calls:      DEBLNK, ALLCAP
c----------------------------------------------------------------------
c
c --- Include parameter statements
      include 'params.cmn'
c
      logical lecho
c
      character*12 cvdic
      character*1 cstor1(mxcol),cstor2(mxcol)
      character*1 cdelim
c
      data cvdic/'INPUTGROUP17'/  ! header record for overland meteorological
c                                 ! data embedded in the control file
      data cdelim/'!'/
c
      ilim2=99
c
c --- begin loop over lines
c
c --- read a line of input
5     continue
      read(ioin,10)cstor1
10    format(150a1)
      if(lecho)write(ioout,7)cstor1
7     format(1x,150a1)
c
c --- check if this is a continuation line
      if(ilim2.gt.0)go to 16
c
c --- continuation line -- find the second delimiter
      do 12 i=1,mxcol
      if(cstor1(i).eq.cdelim)then
         ilim2=i
         go to 14
      endif
12    continue
14    continue
      il2=ilim2
      if(il2.eq.0)il2=mxcol
c
c --- remove blank characters from string within delimiters
      call deblnk(cstor1,1,il2,cstor2,nlim)
c
c --- convert lower case letters to upper case
      call allcap(cstor2,nlim)
      go to 55
16    continue
      ibs=1
c
c --- begin loop over delimiter pairs
17    continue
      if(ibs.ge.mxcol)go to 5
c
c --- find location of delimiters
      do 20 i=ibs,mxcol
      if(cstor1(i).eq.cdelim)then
         ilim1=i
         if(ilim1.eq.mxcol)go to 22
         ip1=ilim1+1
         do 18 j=ip1,mxcol
         if(cstor1(j).eq.cdelim)then
            ilim2=j
            go to 22
         endif
18       continue
c
c ---    second delimiter not on this line
         ilim2=0
         go to 22
      endif
20    continue
c
c --- no delimiters found -- skip line and read next line of text
      go to 5
22    continue
      ibs=ilim2+1
      if(ilim2.eq.0)ibs=mxcol+1
c
c --- remove blanks from string within delimiters
      il2=ilim2
      if(il2.eq.0)il2=mxcol
      call deblnk(cstor1,ilim1,il2,cstor2,nlim)
c
c --- convert lower case letters to upper case
      call allcap(cstor2,nlim)
c
c --- verify that the character string equals cvdic, 'INPUTGROUP17'
c     (note that all blanks have been removed).
c
55    continue
      do i=1,12
        if (cstor2(i+1).ne.cvdic(i:i)) go to 100
      end do
      return
c
100   continue
c
      write(6,51)    (cstor1(i),i=ilim1,il2)
      write(ioout,51)(cstor1(i),i=ilim1,il2)
51    format(/1x,'ERROR IN SUBR. READIN3 -- Error in input data -- '/
     1 1x,'Variable not found in variable dictionary'/
     2 1x,'Variable Dictionary: INPUT GROUP 17'/
     3 1x,'Variable: ',40a1)
      if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for more
     & details.'
c     Stop for normal run.
      end
c----------------------------------------------------------------------
      subroutine altonu(ioout,alp,ncar,itype,inormal,irep,rlno,ino,
     &                  lv,cv)
c----------------------------------------------------------------------
c
c --- CALPUFF    Version: 4.0     Level:  950122                 ALTONU
c ---            J. Scire, SRC
c
c --- PURPOSE:  Convert a character string into a real, integer or
c               logical variable -- also compute the repetition factor
c               for the variable
c
c --- INPUTS:
c            IOOUT - integer           - Fortran unit of list file
c                                        output
c        ALP(ncar) - character*1 array - Characters to be converted
c             NCAR - integer           - Number of characters
c            ITYPE - integer           - Type of each variable
c                                           1 = real,
c                                           2 = integer,
c                                           3 = logical,
c                                           4 = character*4
c          INORMAL - integer            -   1 = normal run;
c                                           0 = test run
c
c       Parameter:   MXCOL
c
c --- OUTPUT:
c             IREP - integer           - Repetition factor for value
c             RLNO - real              - Real variable produced from
c                                        character string
c              INO - integer           - Integer variable produced from
c                                        character string
c               LV - logical*4         - Logical variable produced from
c                                        character string
c        CV(mxcol) - character*4       - Character*4 variable produced
c                                        from character string
c                                        (NOTE: Only 1 (NOT 4)
c                                        character(s) per word)
c
c --- ALTONU called by:  READIN
c --- ALTONU calls:      none
c----------------------------------------------------------------------
c
c --- Include parameter statements
      include 'params.cmn'
c
      real*8 rno,xmult,ten
      integer num2(mxcol)
      logical*4 lv
      character*4 cv(mxcol)
      character*1 alp(ncar),alpsv,ad(17),astar,adec
c
      data ad/'0','1','2','3','4','5','6','7','8','9','-',
c ---   num2 = 0   1   2   3   4   5   6   7   8   9  11
     1        '*','.','E','D','T','F'/
c ---   num2 = 12  13  14  15  16  17
      data astar/'*'/,adec/'.'/,ten/10.0d0/
c
c --- If dealing with a character*4 variable, transfer characters
c     into the work array CV (ONE character per 4-byte word)
      if(itype.eq.4)then
         do 5 i=1,ncar
         cv(i)(1:1)=alp(i)
5        continue
c
c ---    NOTE: Repetition factor refers to the number of
c              characters in the field, if ITYPE = 4
         irep=ncar
         return
      endif
c
c --- Convert character array elements into numeric codes
      do 30 i=1,ncar
      alpsv=alp(i)
      do 20 j=1,17
      if(alpsv.eq.ad(j))then
         num2(i)=j
         if(j.lt.11)num2(i)=j-1
         go to 30
      endif
20    continue
      write(6,21)    (alp(n),n=1,ncar)
      write(ioout,21)(alp(n),n=1,ncar)
21    format(/1x,'ERROR IN SUBR. ALTONU -- Unrecognizable character ',
     1 'in input -- Character string (ALP) = ',15a1)
      if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for more
     & details.'
c     Stop for normal run.
30    continue
c
c --- Locally classify variable type (1=real, 2=integer, 3=logical)
      do 40 i=1,ncar
      if(num2(i).le.12)go to 40
      if(num2(i).ge.16)then
c
c ---    logical variable ("T", "F")
         jtype=3
         go to 41
      else
c
c ---    real variable (".", "E", "D")
         jtype=1
         go to 41
      endif
40    continue
c
c --- integer variable
      jtype=2
41    continue
c
c --- determine if repetition factor "*" is used
      do 50 i=1,ncar
      if(alp(i).eq.astar)then
         istar=i
         go to 51
      endif
50    continue
      istar=0
51    continue
      if(istar.ne.0)go to 400
      irep=1
      go to (101,201,301),jtype
      write(6,55)    jtype,(alp(n),n=1,ncar)
      write(ioout,55)jtype,(alp(n),n=1,ncar)
55    format(/1x,'ERROR IN SUBR. ALTONU -- JTYPE must be 1, 2, or 3 ',
     1 '-- JTYPE = ',i3/3x,'Text string (ALP) = ',15a1)
      if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for more
     & details.'
c     Stop for normal run.
c
c --------------------------------------------------------------------
c --- REAL number w/o "*"
c --------------------------------------------------------------------
c --- Determine sign -- ISTAR is position of array containing "*"
c                       (ISTAR = 0 if no repetition factor)
101   continue
      if(num2(1+istar).eq.11)then
         isgn=-1
         istart=istar+2
      else
         isgn=1
         istart=istar+1
      endif
c
c --- Locate decimal point
      idec=0
      do 109 i=istart,ncar
      if(alp(i).eq.adec)then
         if(idec.eq.0)then
            idec=i
            go to 109
         endif
c
c ---    More than one decimal point found
         write(6,120)    (alp(n),n=1,ncar)
         write(ioout,120)(alp(n),n=1,ncar)
120      format(/1x,'ERROR IN SUBR. ALTONU -- Invalid real variable ',
     1   'entry'/5x,'Input text (ALP) = ',15a1)
         if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for m
     &ore details.'
c        Stop for normal run.
      endif
109   continue
c
c --- Search for E or D
      do 110 i=istart,ncar
      if(num2(i).eq.14.or.num2(i).eq.15)then
         istop=i-1
         go to 111
      endif
110   continue
      istop=ncar
111   continue
c
c --- Convert integer numerics to real number
      rno=0.0
      do 130 i=istart,istop
      if(i.eq.idec)go to 130
      if(num2(i).ge.10)then
         write(6,120)    (alp(n),n=1,ncar)
         write(ioout,120)(alp(n),n=1,ncar)
         if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for m
     &ore details.'
c        Stop for normal run.
      endif
      iexp=idec-i
      if(iexp.gt.0)iexp=iexp-1
      xmult=1.0
      if(iexp.ne.0)xmult=ten**iexp
      rno=rno+xmult*num2(i)
130   continue
c
c --- Account for minus sign (if present)
      rno=isgn*rno
      rlno=rno
c --- Also set integer variable in case of improper input
      if(rlno.lt.0.0)then
         ino=rlno-0.0001
      else
         ino=rlno+0.0001
      endif
      if(istop.eq.ncar)return
c
c --- Find exponent (istop+1 is position in array containing E or D)
      isgn=1
      istart=istop+2
      if(num2(istart).ne.11)go to 135
      isgn=-1
      istart=istart+1
135   continue
      if(istart.gt.ncar)then
         write(6,120)    (alp(n),n=1,ncar)
         write(ioout,120)(alp(n),n=1,ncar)
         if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for m
     &ore details.'
c        Stop for normal run.
      endif
      rexp=0.0
      do 140 i=istart,ncar
      if(num2(i).ge.10)then
         write(6,120)    (alp(n),n=1,ncar)
         write(ioout,120)(alp(n),n=1,ncar)
         if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for m
     &ore details.'
c        Stop for normal run.
      endif
      iexp=ncar-i
      xmult=1.0
      if(iexp.ne.0)xmult=ten**iexp
      rexp=rexp+xmult*num2(i)
140   continue
      xmult=1.0
      if(rexp.ne.0.0)xmult=ten**(isgn*rexp)
      rno=rno*xmult
      rlno=rno
c
c --- Also set integer variable in case of improper input
      if(rlno.lt.0.0)then
         ino=rlno-0.0001
      else
         ino=rlno+0.0001
      endif
      return
c
c --------------------------------------------------------------------
c --- INTEGER variables
c --------------------------------------------------------------------
201   continue
      if(num2(1+istar).ne.11)go to 228
      isgn=-1
      istart=istar+2
      go to 229
228   continue
      isgn=1
      istart=istar+1
229   continue
      ino=0
      do 230 i=istart,ncar
      if(num2(i).ge.10)go to 208
      iexp=ncar-i
      xmult=1.0
      if(iexp.ne.10)xmult=ten**iexp
      ino=ino+xmult*num2(i)+0.5
230   continue
      ino=isgn*ino
c
c --- Also set real variable in case of improper input
      rlno=ino
      return
208   continue
      write(6,220)    (alp(n),n=1,ncar)
      write(ioout,220)(alp(n),n=1,ncar)
220   format(/1x,'ERROR IN SUBR. ALTONU -- Invalid integer variable ',
     1 'entry'/5x,'Input text (ALP) = ',15a1)
      if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for more
     & details.'
c     Stop for normal run.
c
c --------------------------------------------------------------------
c --- LOGICAL variables
c --------------------------------------------------------------------
301   continue
      if(ncar-istar.ne.1)go to 308
      if(num2(istar+1).eq.16)then
c
c ---    Variable = T
         lv=.true.
         return
      else if(num2(istar+1).eq.17)then
c
c ---    Variable = F
         lv=.false.
         return
      endif
308   continue
      write(6,320)    (alp(n),n=1,ncar)
      write(ioout,320)(alp(n),n=1,ncar)
320   format(/1x,'ERROR IN SUBR. ALTONU -- Invalid logical variable ',
     1 'entry'/5x,'Input text (ALP) = ',15a1)
      if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for more
     & details.'
c     Stop for normal run.
c
c --- Determine repetition factor
400   continue
      irep=0
c
c --- ISTAR is the position of array containing "*"
      istrm1=istar-1
      do 430 i=1,istrm1
      if(num2(i).ge.10)go to 408
      iexp=istrm1-i
      xmult=1.0
      if(iexp.ne.0)xmult=ten**iexp
      irep=irep+xmult*num2(i)+0.5
430   continue
      go to(101,201,301),jtype
      write(6,55)    jtype,(alp(n),n=1,ncar)
      write(ioout,55)jtype,(alp(n),n=1,ncar)
      if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for more
     & details.'
c     Stop for normal run.
408   continue
      write(6,420)    (alp(n),n=1,ncar)
      write(ioout,420)(alp(n),n=1,ncar)
420   format(/1x,'ERROR IN SUBR. ALTONU -- Invalid repetition factor ',
     1 'entry'/5x,'Input text (ALP) = ',15a1)
      if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for more
     & details.'
c     Stop for normal run.
      end
c----------------------------------------------------------------------
      subroutine deblnk(cstor1,ilim1,il2,cstor2,nlim)
c----------------------------------------------------------------------
c
c --- CALPUFF    Version: 4.0     Level:  950122                 DEBLNK
c ---            J. Scire, SRC
c
c --- PURPOSE:  Remove all blank characters from the character string
c               within delimiters
c
c --- INPUTS:
c
c    CSTOR1(mxcol) - character*1 array - Input character string
c            ILIM1 - integer           - Array element at which search
c                                        for blanks begins
c              IL2 - integer           - Array element at which search
c                                        for blanks ends
c        Parameters: MXCOL
c
c --- OUTPUT:
c
c    CSTOR2(mxcol) - character*1 array - Output character string
c                                        (without blanks within text)
c             NLIM - integer           - Length of output string
c                                        (characters)
c
c --- DEBLNK called by:  READIN
c --- DEBLNK calls:      none
c----------------------------------------------------------------------
c
c --- Include parameter statements
      include 'params.cmn'
c
      character*1 cstor1(mxcol),cstor2(mxcol),cblnk,cplus
      data cblnk/' '/,cplus/'+'/
c
      ind=0
      do 10 i=ilim1,il2
      if(cstor1(i).eq.cblnk .or. cstor1(i).eq.cplus)go to 10
c         Also take out the + sign so that variable such as
c         1.23E+01 will be converted to 1.23E01.
c
c --- transfer non-blank character into output array
      ind=ind+1
      cstor2(ind)=cstor1(i)
10    continue
      nlim=ind
      if(ind.eq.mxcol)return
c
c --- pad rest of output array
      indp1=ind+1
      do 20 i=indp1,mxcol
      cstor2(i)=cblnk
20    continue
      return
      end
c----------------------------------------------------------------------
      subroutine setvar(itype,irep,xx,jj,ll,cv,xarr,jarr,larr,carr)
c----------------------------------------------------------------------
c
c --- CALPUFF    Version: 4.0     Level:  950122                 SETVAR
c ---            J. Scire, SRC
c
c --- PURPOSE:  Fill the output variable or array with the value read
c               from the input file
c
c --- INPUTS:
c
c            ITYPE - integer        - Variable type (1=real, 2=integer,
c                                     3=logical, 4=character*4)
c             IREP - integer        - Repetition factor
c                                     If ITYPE = 4, IREP refers to the
c                                     number of characters in the field)
c               XX - real           - Real value read from input
c                                     file (Used only if ITYPE=1)
c               JJ - integer        - Integer value read from input
c                                     file (Used only if ITYPE=2)
c               LL - logical*4      - Logical value read from input
c                                     file (Used only if ITYPE=3)
c        CV(mxcol) - character*4    - Character*4 values read from input
c                                     file (Used only if ITYPE=4)
c
c         PARAMETER:  MXCOL
c
c --- OUTPUT:
c
c          XARR(*) - real array     - Output real array (or scalar if
c                                     IREP=1) -- Used only if ITYPE=1
c          JARR(*) - integer array  - Output integer array (or scalar if
c                                     IREP=1) -- Used only if ITYPE=2
c          LARR(*) - logical array  - Output logical array (or scalar if
c                                     IREP=1) -- Used only if ITYPE=3
c          CARR(*) - character*4    - Output character*4 array (or
c                                     scalar if IREP=1) -- Used only if
c                                     ITYPE=4
c
c --- SETVAR called by:  READIN
c --- SETVAR calls:      none
c----------------------------------------------------------------------
c
c --- Include parameter statements
      include 'params.cmn'
c
      real xarr(*)
      integer jarr(*)
      logical*4 larr(*),ll
      character*4 carr(*),cv(mxcol)
c
      go to(10,20,30,40),itype
c
c --- real variable
10    continue
      do 15 i=1,irep
      xarr(i)=xx
15    continue
      return
c
c --- integer variable
20    continue
      do 25 i=1,irep
      jarr(i)=jj
25    continue
      return
c
c --- logical variable
30    continue
      do 35 i=1,irep
      larr(i)=ll
35    continue
      return
c
c --- character*4 variable string
40    continue
      do 45 i=1,irep
      carr(i)=cv(i)
45    continue
      return
      end
c----------------------------------------------------------------------
      subroutine setvar2(irep,cv,carr)
c----------------------------------------------------------------------
c
c     A speical version of SETVAR that handles only one character array
c     at a time.  The array is explicitly declared as CHARACTER in both
c     the calling and called routines.
c     Joseph Chang, EARTH TECH, Level:  961111.
c
c --- CALPUFF    Version: 4.0     Level:  950122                 SETVAR
c ---            J. Scire, SRC
c
c --- PURPOSE:  Fill the output variable or array with the value read
c               from the input file
c
c --- INPUTS:
c
c             IREP - integer        - Repetition factor
c                                     If ITYPE = 4, IREP refers to the
c                                     number of characters in the field)
c        CV(mxcol) - character*4    - Character*4 values read from input
c                                     file (Used only if ITYPE=4)
c
c         PARAMETER:  MXCOL
c
c --- OUTPUT:
c
c          CARR(*) - character*4    - Output character*4 array (or
c                                     scalar if IREP=1) -- Used only if
c                                     ITYPE=4
c
c --- SETVAR called by:  READIN
c --- SETVAR calls:      none
c----------------------------------------------------------------------
c
c --- Include parameter statements
      include 'params.cmn'
c
      character*4 carr(irep),cv(mxcol)
c
c --- character*4 variable string
40    continue
      do 45 i=1,irep
      carr(i)=cv(i)
45    continue
      return
      end
c----------------------------------------------------------------------
      subroutine allcap(cstor2,nlim)
c----------------------------------------------------------------------
c
c --- CALPUFF    Version: 4.0     Level:  950122                 ALLCAP
c ---            J. Scire, SRC
c
c --- PURPOSE:  Convert all lower case letters within a character
c               string to upper case
c
c --- INPUTS:
c
c    CSTOR2(mxcol) - character*1 array - Input character string
c             NLIM - integer           - Length of string (characters)
c        Parameters: MXCOL
c
c --- OUTPUT:
c
c    CSTOR2(mxcol) - character*1 array - Output character string with
c                                        lower case letters converted
c                                        to upper case
c
c --- ALLCAP called by:  READIN
c --- ALLCAP calls:      none
c----------------------------------------------------------------------
c
c --- Include parameter statements
      include 'params.cmn'
c
      character*1 cstor2(mxcol),cchar,clc(29),cuc(29)
c
      data clc/'i','n','x','a','e','o','u','b','c','d','f','g','h',
     1 'j','k','l','m','p','q','r','s','t','v','w','y','z','-','.',
     2 '*'/
      data cuc/'I','N','X','A','E','O','U','B','C','D','F','G','H',
     1 'J','K','L','M','P','Q','R','S','T','V','W','Y','Z','-','.',
     2 '*'/
c
      do 100 i=1,nlim
      cchar=cstor2(i)
c
      do 50 j=1,29
      if(cchar.eq.clc(j))then
         cstor2(i)=cuc(j)
         go to 52
      endif
50    continue
52    continue
100   continue
c
      return
      end
c
c----------------------------------------------------------------------
      subroutine validate (i,nallow,iallow,ierr,inormal,vname)
c----------------------------------------------------------------------
c
c PURPOSE:  To validate an integer modeling option
c
c Input:
c     I       integer   value of modeling option to be validated
c     NALLOW  integer   no. of values allowed for I
c     IALLOW  integer   the allowed values for I
c     IERR    integer   unit number for error message file
c     INORMAL integer   =1, normal run; =0, test run
c     VNAME   character name of modeling option
c 
c CALLED BY:  READCF2
c
c CALLS:  None
c
c       Minerals Management Service
c       U.S. Department of the Interior
c
c OCD             Revision History:
c    Joseph C. Chang, EARTH TECH
c    First created: 961125
c    Last revised:  961218
c----------------------------------------------------------------------
      integer i,nallow,iallow(nallow),ierr
      character *(*) vname
c
      do j=1,nallow
        if (i.eq.iallow(j)) return
      end do
c
c *** An illegal value has been specified
c
      write (6,11)
      write (ierr,11)
11    format(/,' Error in OCD GROUP 4 or 14:')
      write (6,21) vname
      write (ierr,21) vname
21    format(' An incorrect value for ',a,' has been specified')
      write (6,31) i
      write (ierr,31) i
31    format(' The spcified value = ',i3)
      write (6,41) (iallow(j),j=1,nallow)
      write (ierr,41) (iallow(j),j=1,nallow)
41    format(' The allowed values = ',8i3)
      if (inormal.eq.1) stop 'Error encountered.  See ERROR.OUT for more
     & details.'
c     Stop for normal run.
c
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCREC00010
      SUBROUTINE RECEP( J,RQ,SQ,P,DISTFK,IRISE,F,HPRM,HBEL,HABV,        REC00020
     &    IZWSA,IYWSA,IZWSB,IYWSB,KSIG,WIDTHB,EFFHT,WDIR,NPER)          REC00030
C                                                                       REC00040
C PURPOSE:  RECEPTOR LOOP CALCULATION OF CONCENTRATIONS FOR             REC00050
C           EACH RECEPTOR                                               REC00060
C                                                                       REC00070
C I/O:  J, SOURCE INDEX                                                 REC00080
C      RQ, EAST COORDINATE OF SOURCE                                    REC00090
C      SQ, NORTH COORDINATE OF SOURCE                                   REC00100
C       P, FRACTION OF PLUME PENETRATION                                REC00110
C  DISTFK, DISTANCE TO FINAL RISE (KM)                                  REC00120
C   IRISE, RISE CODE                                                    REC00130
C       F, BUOYANCY FLUX                                                REC00140
C    HPRM, STACK HEIGHT WHICH TAKES INTO ACCOUNT DOWNWASH               REC00150
C    HBEL, EFFECTIVE PLUME HEIGHT BELOW MIXING LAYER                    REC00160
C    HABV, EFFECTIVE PLUME HEIGHT ABOVE MIXING LAYER                    REC00170
C   IZWSA, STABILITY E OVER WATER VERTICAL TURBULENCE INTENSITY         REC00180
C   IYWSA, STABILITY E OVER WATER LATERAL TURBULENCE INTENSITY          REC00190
C   IZWSB, OVER WATER VERTICAL TURBULENCE INTENSITY AT STACK TOP        REC00200
C   IYWSB, OVER WATER LATERAL TURBULENCE INTENSITY AT STACK TOP         REC00210
C    KSIG, SIGNIFICANT POINT SOURCE CONSTANT                            REC00220
C  WIDTHB, BUILDING WIDTH                                               REC00230
C   EFFHT, EFFECTIVE HEIGHT OF PLUME                                    REC00240
C    WDIR, HOURLY WIND DIRECTION                                        REC00250
C    NPER, NUMBER OF AVERAGING PERIODS                                  REC00260
C                                                                       REC00270
C CALLED BY: PTR                                                        REC00280
C                                                                       REC00290
C CALLS:  DIST                                                          REC00300
C         CALC                                                          REC00310
C         CUBIC                                                         REC00320
C                                                                       REC00330
C       MINERALS MANAGEMENT SERVICE                                     REC00340
C       U.S. DEPARTMENT OF THE INTERIOR                                 REC00350
C                                                                       REC00360
C OCD             REVISION HISTORY:                                     REC00370
C    DCD 880909   CREATED.                                              REC00380
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCREC00390
                                                                        REC00400
      include 'params.cmn'
      INCLUDE 'hrdat.cmn'                                               REC00410
      INCLUDE 'opt.cmn'                                                 REC00420
      INCLUDE 'met.cmn'                                                 REC00440
      INCLUDE 'src.cmn'                                                 REC00450
      INCLUDE 'store.cmn'                                               REC00460
      INCLUDE 'shr.cmn'                                                 REC00470
      INCLUDE 'ptrrcp.cmn'                                              REC00480
      INCLUDE 'const.cmn'                                               REC00490
      INCLUDE 'linesrc.cmn'                                             REC00500
                                                                        REC00510
      REAL IZWSB,IYWSB,IZWSA,IYWSA                                      REC00520
                                                                        REC00530
      ISPLIT=0                                                          REC00540
      HTERRAIN = HTER(K)                                                REC00550
      ZER = ELR(K) * CELM                                               REC00560
      Z = ZR(K)                                                         REC00570
                                                                        REC00580
C   SET X,Y COORDINATE OF SOURCE TO MIDPOINT OF LINE SOURCE SEGMENT     REC00590
      IF(IOPT(20).EQ.2) THEN                                            REC00600
         RQ = MIDX(IHR)                                                 REC00610
         SQ = MIDY(IHR)                                                 REC00620
      ENDIF                                                             REC00630
                                                                        REC00640
C     DETERMINE DISTANCES                                               REC00650
      CALL DIST( J,RQ,SQ,XD)                                            REC00660
                                                                        REC00670
C     X IS THE UPWIND DISTANCE  OF THE SOURCE FROM THE RECEPTOR.        REC00680
C     IF X IS NEGATIVE, INDICATING THAT THE SOURCE IS DOWNWIND OF       REC00690
C     THE RECEPTOR, THE CALCULATION IS TERMINATED ASSUMING NO           REC00700
C     CONTRIBUTION FROM THAT SOURCE.                                    REC00710
      IF (X.LE.0.0) RETURN                                              REC00720
C                                                                       REC00730
C       DETERMINE CHEMICAL TRANSFORMATION RATE, IF ANY                  REC00740
      IF(IOPT(25).EQ.1) THEN                                            REC00750
         DECAYF = EXP(-RATE*X/(360.*UPL))                               REC00760
      ELSE                                                              REC00770
         DECAYF = 1.0                                                   REC00780
      ENDIF                                                             REC00790
      Q = SOURCE(3,J)                                                   REC00800
C  LINE SOURCE EMISSION RATE FOR EACH SEGMENT TAKING INTO               REC00810
C   ACCOUNT THE TIME INCREMENT (NPER)                                   REC00820
      IF(IOPT(20) .EQ. 2) Q = Q*FLOAT(NPER)/FLOAT(NSEGS)                REC00830
      H = HSAV(J)                                                       REC00840
C                                                                       REC00850
C   DO NOT USE GRADUAL PLUME RISE IF PLUME IS PARTIALLY OR COMPLETELY   REC00860
C   PENETRATING THE TOP OF THE MIXING LAYER                             REC00870
      IF(P.GT.0.) GOTO 2470                                             REC00880
C                                                                       REC00890
      IF ((IOPT(4).EQ.0.AND.IOPT(3).EQ.1).OR.X.GE.DISTFK) GO TO 2500    REC00900
C        CALCULATE GRADUAL PLUME RISE IF (1) THE USER SPECIFIES SO,     REC00910
C        OR (2) USER EMPLOYS CALCULATION OF INITIAL DISPERSION.....     REC00920
C        IN THIS CASE, USE OF FINAL EFFECTIVE HEIGHT IN THE CALCULATION REC00930
C        OF DISPERSION COEFFICENTS COULD LEAD TO MISLEADING VALUES SINCEREC00940
C          SIGMA-Y,-Z =  DELTA-H/3.5                                    REC00950
      IF(IRISE.EQ.0 .OR. IRISE.EQ.2) GOTO 2500                          REC00960
      CC = -4096000.*F*X*X/(UPL*UPL*UPL)                                REC00970
      DELH=0.                                                           REC00980
      SCOEFF=AC+BC+CC                                                   REC00990
      IF(SCOEFF.NE.0.0) CALL CUBIC(AC,BC,CC,DELH)                       REC01000
      H = HPRM + DELH                                                   REC01010
C                                                                       REC01020
C       IF TRANSITIONAL RISE NOT WANTED (BUT BUOYANCY-INDUCED DISPERSIONREC01030
C       WANTED), ASSIGN FINAL RISE                                      REC01040
      IF(IOPT(3).EQ.1) H = HSAV(J)                                      REC01050
      GOTO 2500                                                         REC01060
C                                                                       REC01070
C   START OF TWO PLUME LOOP FOR PARTIAL PLUME PENETRATION CASES         REC01080
2470  IF(P.LT.1.) THEN                                                  REC01090
         ISPLIT=ISPLIT+1                                                REC01100
         IF(ISPLIT.EQ.1) THEN                                           REC01110
            H=HBEL                                                      REC01120
         ELSEIF(ISPLIT.EQ.2) THEN                                       REC01130
            H=HABV                                                      REC01140
         ENDIF                                                          REC01150
      ENDIF                                                             REC01160
C                                                                       REC01170
C    CALCULATE THE CONTRIBUTION OF ONE PLUME TO ONE RECEPTOR            REC01180
2500  HA = H                                                            REC01190
C                                                                       REC01200
C   IF PLUME (HA) IS WITHIN MIXING LAYER (HL) USE STACK TOP IZ AND IY   REC01210
C   IF PLUME IS ABOVE MIXING LAYER USE STABILITY E                      REC01220
      IF(HA.GE.HL) THEN                                                 REC01230
         IZWS=IZWSA                                                     REC01240
         IYWS=IYWSA                                                     REC01250
      ELSE                                                              REC01260
         IZWS=IZWSB                                                     REC01270
         IYWS=IYWSB                                                     REC01280
      ENDIF                                                             REC01290
C                                                                       REC01300
C   IF IOPT(4)=1, CONSIDER BUOYANCY INDUCED DISPERSION OF PLUME DUE     REC01310
C    TO TURBULENCE DURING BUOYANT RISE.  INITIAL SIGMA IS PROPORTIONAL  REC01320
C    TO PLUME RISE WEIGHTED BY THE FRACTION OF TOTAL PLUME MASS IN EACH REC01330
C    PLUME                                                              REC01340
      SIGB = 0.0                                                        REC01350
      IF(ISPLIT.EQ.0) THEN                                              REC01360
         PMASS=1.                                                       REC01370
      ELSEIF(ISPLIT.EQ.1) THEN                                          REC01380
         PMASS=1.-P                                                     REC01390
      ELSEIF(ISPLIT.EQ.2) THEN                                          REC01400
         PMASS=P                                                        REC01410
      ENDIF                                                             REC01420
      IF (IOPT(4).EQ.1) SIGB = (HA-HPRM)/3.5*PMASS                      REC01430
C                                                                       REC01440
C    CALC RETURNS THE DISPERSION PARAMETERS, SY AND SZ (METERS)         REC01450
C     AND THE RELATIVE CONCENTRATION VALUES CHI/Q (SEC/M**3)            REC01460
      CALL CALC(XD,HTERRAIN,EFFHT,WIDTHB,WDIR)                          REC01470
C                                                                       REC01480
      PROD = RC * Q  * DECAYF                                           REC01490
C                                                                       REC01500
C   IF PLUME IS SPLIT DUE TO PARTIAL PENETRATION COMPUTE RC FOR BOTH    REC01510
C   PARTS AND THEN SUM                                                  REC01520
      IF(ISPLIT.EQ.1) THEN                                              REC01530
         PROD1=PROD                                                     REC01540
         GO TO 2470                                                     REC01550
      ENDIF                                                             REC01560
      IF(ISPLIT.EQ.2) PROD=PROD*P+PROD1*(1.-P)                          REC01570
C                                                                       REC01580
C        INCREMENT CONCENTRATION AT K-TH RECEPTOR(G/M**3)               REC01590
C          PCHI - SUM FOR THE AVERAGING TIME AT RECEPTOR K              REC01600
      PCHI(K)=PCHI(K)+PROD                                              REC01610
C          PHCHI - CONCENTRATION FOR THIS HOUR AT RECEPTOR K            REC01620
      PHCHI(K)=PHCHI(K)+PROD                                            REC01630
      IF (KSIG.EQ.0) RETURN                                             REC01640
C        STORE CONCENTRATIONS FROM SIGNIFICANT SOURCES.(G/M**3)         REC01650
      PSIGS(K,KSIG)=PSIGS(K,KSIG)+PROD                                  REC01660
      PHSIGS(K,KSIG)=PHSIGS(K,KSIG)+PROD                                REC01670
      PSIGS(K,26)=PSIGS(K,26)+PROD                                      REC01680
      PHSIGS(K,26)=PHSIGS(K,26)+PROD                                    REC01690
                                                                        REC01700
      RETURN                                                            REC01710
      END                                                               REC01720
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCSIG00010
      SUBROUTINE SIGMA(XY,XZ,IST,IY,IZ,SY,SZ,UPL,icheck)                SIG00020
C                                                                       SIG00030
C PURPOSE:  CALCULATE SIGMA-Y AND SIGMA-Z                               SIG00040
C           SIGMA-Y USING DRAXLER METHOD                                SIG00050
C           SIGMA-Z USING BRIGGS METHOD                                 SIG00060
C                                                                       SIG00070
C I/O:  XY, DOWNWIND DISTANCE FOR SIGMA-Y, in KM                        SIG00080
C       XZ, DOWNWIND DISTANCE FOR SIGMA-Z, in KM                        SIG00090
C      IST, STABILITY CLASS (USED FOR DOWNWIND BEHAVIOR OF SIGMA-Z)     SIG00100
C       IY, TURBULENCE INTENSITY COMPONENT IN Y DIRECTION               SIG00110
C       IZ, TURBULENCE INTENSITY COMPONENT IN Y DIRECTION               SIG00120
C       SY, SIGMA-Y DUE TO ATMOSPHERIC TURBULENCE                       SIG00130
C       SZ, SIGMA-Z DUE TO ATMOSPHERIC TURBULENCE                       SIG00140
C      UPL, STACK TOP WIND SPEED                                        SIG00150
c   ICHECK, Flag for checking IY, IZ, or both                           XXX00150
c           = 0, both IY and IZ                                         XXX00151
c           = 1, only IY                                                XXX00152
c           = 2, only IZ                                                XXX00153
c           This is to ensure that SIGMAY and SIGMAZ are not            XXX00154
c           calculated using missing data                               XXX00155
C                                                                       SIG00160
C CALLED BY:  CALC                                                      SIG00170
C             VIRT                                                      SIG00180
C                                                                       SIG00190
C CALLS:  NONE                                                          SIG00200
C                                                                       SIG00210
C       MINERALS MANAGEMENT SERVICE                                     SIG00220
C       U.S. DEPARTMENT OF THE INTERIOR                                 SIG00230
C                                                                       SIG00240
C OCD             REVISION HISTORY:                                     SIG00250
C    DCD 880930   CREATED.                                              SIG00260
C    JCC 920729   UPDATED.                                              XXX00260
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCSIG00270
                                                                        SIG00280
        include 'params.cmn'
        include 'store.cmn'
        INCLUDE 'const.cmn'                                             SIG00290
                                                                        SIG00300
        REAL IY,IZ                                                      SIG00310
c                                                                       XXX00310
c  Check ICHECK                                                         XXX00311
c                                                                       XXX00312
        if (icheck.ne.0 .and. icheck.ne.1 .and. icheck.ne.2) then       XXX00313
          write (io,11) icheck
          write (ierr,11) icheck
11        format(' Error in SIGMA: ICHECK = ',i2,' out of range')
          stop 'Error encountered.  See ERROR.OUT for more details.'
        end if                                                          XXX00316
c                                                                       XXX00317
c  Check IY and IZ.                                                     XXX00318
        if ((icheck.eq.0 .or. icheck.eq.1) .and. iy.le.0.) then         XXX00319
          write (io,12) iy
          write (ierr,12) iy
12        format(' Error in SIGMA: IY = ',1p,e10.3,' out of range')
          stop 'Error encountered.  See ERROR.OUT for more details.'
        end if                                                          XXX00322
c                                                                       XXX00323
        if ((icheck.eq.0 .or. icheck.eq.2) .and. iz.le.0.) then         XXX00324
          write (io,13) iz
          write (ierr,13) iz
13        format(' Error in SIGMA: IZ = ',1p,e10.3,' out of range')
          stop 'Error encountered.  See ERROR.OUT for more details.'
        end if                                                          XXX00327
                                                                        SIG00320
C       EXPRESS DOWNWIND DISTANCE IN METERS; COMPUTE SIGMA-Y  USING     SIG00330
C       DRAXLER METHOD                                                  SIG00340
        X = XY*KM2M                                                     SIG00350
                                                                        SIG00360
        IF(XY.LT.10.0) THEN                                             SIG00370
           TIME = X/UPL                                                 SIG00380
        ELSE                                                            SIG00390
           TIME = 10000./UPL                                            SIG00400
        ENDIF                                                           SIG00410
        FY = 1./(1. + 0.9*SQRT( TIME/1000.))                            SIG00420
        SY = IY * X * FY                                                SIG00430
                                                                        SIG00440
C       EXPRESS DOWNWIND DISTANCE IN METERS; COMPUTE SIGMA-Z USING      SIG00450
C       BRIGGS METHOD                                                   SIG00460
C                                                                       SIG00470
        X = XZ * KM2M                                                   SIG00480
        GO TO (1000,1000,2000,3000,4000,4000),IST                       SIG00490
C                                                                       SIG00500
C       STABILITY A OR B                                                SIG00510
C                                                                       SIG00520
1000    SZ = IZ * X                                                     SIG00530
        RETURN                                                          SIG00540
C                                                                       SIG00550
C       STABILITY C                                                     SIG00560
C                                                                       SIG00570
2000    SZ = IZ * X/SQRT(1.0 + 0.0002*X)                                SIG00580
        RETURN                                                          SIG00590
C                                                                       SIG00600
C       STABILITY D                                                     SIG00610
C                                                                       SIG00620
3000    SZ = IZ * X/SQRT(1.0 + 0.0015*X)                                SIG00630
        RETURN                                                          SIG00640
C                                                                       SIG00650
C       STABILITY E OR F                                                SIG00660
C                                                                       SIG00670
4000    SZ = IZ * X/(1.0 + 0.0003*X)                                    SIG00680
                                                                        SIG00690
        RETURN                                                          SIG00700
        END                                                             SIG00710
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCSTH00010
      SUBROUTINE SIGTHTA(SIGTHS,SIGTHL)                                 STH00020
C                                                                       STH00030
C PURPOSE: CALCULATE SIGMA-THETA AT SOURCE AND OVER LAND                STH00040
C                                                                       STH00050
C I/O:  SIGTHS, SIGMA-THETA AT SOURCE                                   STH00060
C       SIGTHL, SIGMA-THETA OVER LAND                                   STH00070
C                                                                       STH00080
C CALLED BY:  CALC                                                      STH00090
C                                                                       STH00100
C CALLS:  NONE                                                          STH00110
C                                                                       STH00120
C       MINERALS MANAGEMENT SERVICE                                     STH00130
C       U.S. DEPARTMENT OF THE INTERIOR                                 STH00140
C                                                                       STH00150
C OCD             REVISION HISTORY:                                     STH00160
C    DCD 881219   CREATED.                                              STH00170
C    JCC 920729   UPDATED.                                              XXX00170
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCSTH00180
                                                                        STH00190
      include 'params.cmn'
      INCLUDE 'met.cmn'                                                 STH00200
      INCLUDE 'ptrrcp.cmn'                                              STH00210
      INCLUDE 'store.cmn'                                               STH00220
                                                                        STH00230
C   BRIGGS(1973) DEFAULT RURAL IY VALUES                                STH00240
      REAL BRIGGS(7)/ 0.22,0.16,0.11,0.08,0.06,0.04,0.04/               STH00250
                                                                        STH00260
C   CALCULATE SIGMA-THETA AT THE SOURCE                                 STH00270
c                                                                       XXX00270
       sigths = atan( iyws)                                             XXX00271
c                                                                       XXX00272
c  There is no need to further check the values of IYWS and JOPT(8)     XXX00273
c  because:                                                             XXX00274
c  (1) In ADDMET.FOR, IYW (observed IY over water) is assumed missing   XXX00275
c      if JOPT(8)=0, or when IYW is out of range (regradless of the     XXX00276
c      value of JOPT(8)).  This means that 0. < IYW < 2. is true only   XXX00277
c      when JOPT(8)=1 and 0. < IYW < 2. initially.                      XXX00278
c  (2) In IYIZ.FOR, IYWSB and IYWSA are calculated using the observed   XXX00279
c      IYW value only if IYW > 0., and parameterized if IYW <= 0.       XXX00280
c  (3) In RECEP.FOR, either IYWSB or IYWSA is assigned to IYWS.         XXX00281
c                                                                       XXX00282
c                                                                       STH00330
C   CALCULATE SIGMA-THETA OVER LAND                                     STH00340
      IF( JOPT(5) .EQ. 1 .AND. IYLS .GT. 0.0) THEN                      STH00350
         SIGTHL = ATAN( IYLS)                                           STH00360
      ELSE                                                              STH00370
C   USE DEFAULT RURAL IY (BRIGGS)                                       STH00380
         SIGTHL = BRIGGS(KST)                                           STH00390
      ENDIF                                                             STH00400
                                                                        STH00410
      RETURN                                                            STH00420
      END                                                               STH00430
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCSRK00010
      SUBROUTINE SRCRNK(IMPS)                                           SRK00020
C                                                                       SRK00030
C PURPOSE:  RANK THE SIGNIFICANT SOURCES                                SRK00040
C                                                                       SRK00050
C I/O:    IMPS, SOURCE NUMBER IN ORDER OF SIGNIFICANCE                  SRK00060
C                                                                       SRK00070
C CALLED BY:  READCF2                                                   SRK00080
C                                                                       SRK00090
C CALLS:  NONE                                                          SRK00100
C                                                                       SRK00110
C       MINERALS MANAGEMENT SERVICE                                     SRK00120
C       U.S. DEPARTMENT OF THE INTERIOR                                 SRK00130
C                                                                       SRK00140
C OCD             REVISION HISTORY:                                     SRK00150
C    DCD 880707   CREATED.                                              SRK00160
C    JCC 920819   CREATED.                                              XXX00160
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCSRK00170
                                                                        SRK00180
      include 'params.cmn'
      INCLUDE 'store.cmn'                                               SRK00190
      INCLUDE 'src.cmn'                                                 SRK00210
      INCLUDE 'const.cmn'                                               SRK00220
                                                                        SRK00230
      DIMENSION IMPS(25)                                                SRK00240
                                                                        SRK00250
      IP = IPOL-2                                                       SRK00260
C                                                                       SRK00270
C    RANK NSIGP HIGHEST POINT SOURCES.                                  SRK00280
      IF (NPT.LT.NSIGP) NSIGP=NPT                                       SRK00290
      DO 260 I=1,NSIGP                                                  SRK00300
         SIGMAX=-1.0                                                    SRK00310
         DO 250 J=1,NPT                                                 SRK00320
            IF (DSAV(J).LE.SIGMAX) GO TO 250                            SRK00330
            SIGMAX=DSAV(J)                                              SRK00340
            LMAX=J                                                      SRK00350
250      CONTINUE                                                       SRK00360
C                                                                       SRK00370
C      IMPS IS THE SOURCE NUMBER IN ORDER OF SIGNIFICANCE.              SRK00380
         IMPS(I)=LMAX                                                   SRK00390
C                                                                       SRK00400
C      PSAV IS THE CALC. CONC. IN ORDER OF SIGNIFICANCE.                SRK00410
         PSAV(I)=SIGMAX                                                 SRK00420
         DSAV(LMAX)=-1.0                                                SRK00430
260   CONTINUE                                                          SRK00440
C                                                                       SRK00450
C    OUTPUT TABLE OF RANKED SOURCES.                                    SRK00460
      WRITE (IO,1520) TITLE(IP)                                         SRK00470
      DO 270 I=1,NSIGP                                                  SRK00480
         WRITE (IO,1530) I,PSAV(I),IMPS(I)                              SRK00490
270   CONTINUE                                                          SRK00500
1520  FORMAT ( / ,T3,'SIGNIFICANT ',A4,' POINT SOURCES'//1X,T8,'RANK',T2SRK00510
     &  2,'CHI-MAX',T33,'SOURCE NO.      (NOTE THAT THE VALUE OF CHI-MAXSRK00520
     & IS ONLY ESTIMATED, IT IS NOT BASED ON THE ACTUAL CALCULATION)'/1XXXX00520
     &,T17,'(MICROGRAMS/M**3)'/1X)                                      XXX00521
1530  FORMAT (1X,T9,I3,T18,6PF12.2,T35,I3)                              SRK00530
                                                                        SRK00540
      RETURN                                                            SRK00550
      END                                                               SRK00560
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCTER00010
      SUBROUTINE TERRAIN( HTERRAIN,PPC,HCRIT)                           TER00020
C                                                                       TER00030
C PURPOSE:  CALCULATE PLUME PATH CORRECTION FACTOR (PPC)                TER00040
C           AND CRITICAL DIVIDING STREAMLINE HEIGHT                     TER00050
C                                                                       TER00060
C I/O: HTERRAIN, MOUNTAIN/HILL ELEVATION IN THE VICINITY OF THE         TER00070
C                RECEPTOR                                               TER00080
C           PPC, PLUME PATH TERRAIN FACTOR                              TER00090
C         HCRIT, CRITICAL DIVIDING STREAMLINE HEIGHT                    TER00100
C                                                                       TER00110
C CALLED BY:  CALC                                                      TER00120
C                                                                       TER00130
C CALLS:  NONE                                                          TER00140
C                                                                       TER00150
C       MINERALS MANAGEMENT SERVICE                                     TER00160
C       U.S. DEPARTMENT OF THE INTERIOR                                 TER00170
C                                                                       TER00180
C OCD             REVISION HISTORY:                                     TER00190
C    DCD 880914   CREATED.                                              TER00200
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCTER00210
                                                                        TER00220
      include 'params.cmn'
      INCLUDE 'met.cmn'                                                 TER00230
      INCLUDE 'store.cmn'                                               TER00240
      INCLUDE 'ptrrcp.cmn'                                              TER00250
                                                                        TER00260
C                                                                       TER00270
C   PLUME PATH TERRAIN FACTOR (PPC)                                     TER00280
C   BASE PPC ON HCRIT IF BVN IS AVAILABLE OTHERWISE                     TER00290
C   FOR UNSTABLE CONDITIONS (BVN < 0) OR DTHDZ MISSING (BVN < 0)        TER00300
C    SET PPC=0 FOR OVERLAND CLASSES E - G                               TER00310
C    SET PPC=0.5 FOR OVERLAND CLASSES A - D                             TER00320
                                                                        TER00330
      IF( BVN.GT.0.) THEN                                               TER00340
         HCRIT = HTERRAIN - UPL/BVN                                     TER00350
         IF(HCRIT.LT.0.0) HCRIT = 0.0                                   TER00360
         IF( HA.LE.HCRIT) THEN                                          TER00370
            PPC = 0.0                                                   TER00380
         ELSE                                                           TER00390
            PPC = 0.5                                                   TER00400
         ENDIF                                                          TER00410
      ELSE                                                              TER00420
         HCRIT = 0.                                                     TER00430
         IF( KST .LE. 4) THEN                                           TER00440
            PPC = 0.5                                                   TER00450
         ELSE                                                           TER00460
            PPC = 0.0                                                   TER00470
         ENDIF                                                          TER00480
      ENDIF                                                             TER00490
                                                                        TER00500
      RETURN                                                            TER00510
      END                                                               TER00520
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCTBL00010
      SUBROUTINE TIBL(HT,XTIBL,X2,CAP)                                  TBL00020
C                                                                       TBL00030
C PURPOSE: COMPUTE TIBL HEIGHT (HT) AND THE POINT OF PLUME              TBL00040
C          ENTRY INTO THE TIBL (XTIBL)                                  TBL00050
C                                                                       TBL00060
C I/O:  HT    TIBL HEIGHT (M)                                           TBL00070
C    XTIBL    DISTANCE FROM WHERE PLUME CROSSES SHORE TO WHERE PLUME    TBL00080
C               ENTERS TIBL (KM)                                        TBL00090
C       X2    DISTANCE FROM LAND TO RECEPTOR (M)                        TBL00100
C      CAP    STABLE INTERNAL BOUNDARY CAP (M)                          TBL00110
C                                                                       TBL00120
C CALLED BY:  CALC                                                      TBL00130
C                                                                       TBL00140
C CALLS:  NONE                                                          TBL00150
C                                                                       TBL00160
C       MINERALS MANAGEMENT SERVICE                                     TBL00170
C       U.S. DEPARTMENT OF THE INTERIOR                                 TBL00180
C                                                                       TBL00190
C OCD             REVISION HISTORY:                                     TBL00200
C    DCD 880930   CREATED.                                              TBL00210
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCTBL00220
                                                                        TBL00230
        INCLUDE 'met.cmn'                                               TBL00240
        INCLUDE 'ptrrcp.cmn'                                            TBL00250
C                                                                       TBL00260
C   XLAND DISTANCE FROM SOURCE TO LAND (ALONG RECEPTOR)                 TBL00270
C   HP    PLUME HEIGHT                                                  TBL00280
C   HLR   MIXING DEPTH USED FOR VERTICAL MIXING                         TBL00290
C   KST   STABILITY CLASS                                               TBL00300
C   HA    PLUME HEIGHT ABOVE STACK BASE                                 TBL00310
C   ZER   RECEPTOR GROUND LEVEL ELEVATION (M)                           TBL00320
C   H     EFFECTIVE STACK HEIGHT (M)                                    TBL00330
                                                                        TBL00340
C   FOR OVERLAND SOURCES, SET TIBL HEIGHT TO UNLIMITED                  TBL00350
      IF(XLAND.LE.0.0) THEN                                             TBL00360
         XTIBL = 0.0                                                    TBL00370
         HT = 9999.                                                     TBL00380
         RETURN                                                         TBL00390
      ENDIF                                                             TBL00400
C                                                                       TBL00410
C    COMPUTE XTIBL                                                      TBL00420
C    OVERLAND STABILITY E OR F                                          TBL00430
      IF(KST.GE.5) THEN                                                 TBL00440
         XTIBL=H/(0.1+ZER/X2)                                           TBL00450
         IF(XTIBL.GT.2000.) XTIBL=(H-133.3333)/(0.0333+ZER/X2)          TBL00460
C                                                                       TBL00470
C          STABILITY A TO D                                             TBL00480
C       ZER > H                                                         TBL00490
      ELSE                                                              TBL00500
         IF(ZER.GT.H) THEN                                              TBL00510
            XTIBL=5.*H                                                  TBL00520
            IF(XTIBL.GT.2000.) XTIBL=30.*(0.5*H-133.3333)               TBL00530
C       ZER <= H                                                        TBL00540
         ELSE                                                           TBL00550
            XTIBL=H/(0.1+0.5*ZER/X2)                                    TBL00560
            IF(XTIBL.GT.2000.) XTIBL=(H-133.3333)/(0.03333+0.5*ZER/X2)  TBL00570
         ENDIF                                                          TBL00580
      ENDIF                                                             TBL00590
C                                                                       TBL00600
C   COMPUTE TIBL HEIGHT                                                 TBL00610
      IF( XTIBL.LE.2000.) THEN                                          TBL00620
         HT=0.1*XTIBL                                                   TBL00630
      ELSE                                                              TBL00640
         HT=200.+0.03333*(XTIBL-2000.)                                  TBL00650
      ENDIF                                                             TBL00660
      IF(KST.GE.5)  HT=AMIN1(HT,CAP)                                    TBL00670
C                                                                       TBL00680
C   CONVERT TO KM                                                       TBL00690
      XTIBL=XTIBL/1000.                                                 TBL00700
                                                                        TBL00710
      RETURN                                                            TBL00720
      END                                                               TBL00730
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCVDF00010
      SUBROUTINE VDF(HH,D,S,V)                                          VDF00020
C                                                                       VDF00030
C PURPOSE:  CALCULATES THE VERTICAL DISTRIBUTION:                       VDF00040
C           MULTIPLY RELECTED GAUSSIAN USING FOURIER SERIES             VDF00050
C                                                                       VDF00060
C I/O:  HH: RELEASE HEIGHT (M)                                          VDF00070
C        D: MIXING LID (M)                                              VDF00080
C        S: VERTICAL DISPERSION COEFFICIENT (SIGMA Z)                   VDF00090
C        V: CALCULATED DISTRIBUTION VALUE (1/M)                         VDF00100
C                                                                       VDF00110
C CALLED BY:                                                            VDF00120
C                                                                       VDF00130
C CALLS:  NONE                                                          VDF00140
C                                                                       VDF00150
C       MINERALS MANAGEMENT SERVICE                                     VDF00160
C       U.S. DEPARTMENT OF THE INTERIOR                                 VDF00170
C                                                                       VDF00180
C OCD             REVISION HISTORY:                                     VDF00190
C    DCD 880930   CREATED.                                              VDF00200
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCVDF00210
                                                                        VDF00220
      INCLUDE 'const.cmn'                                               VDF00230
                                                                        VDF00240
      IF(D.EQ.0.0) THEN                                                 VDF00250
         V = 0.0                                                        VDF00260
         RETURN                                                         VDF00270
      ENDIF                                                             VDF00280
                                                                        VDF00290
C  ACCURACY PARAMETERS                                                  VDF00300
      TMIN=0.0512                                                       VDF00310
      TMAX=9.21                                                         VDF00320
      V=1                                                               VDF00330
      T=(S/D)**2                                                        VDF00340
      H=HH/D                                                            VDF00350
                                                                        VDF00360
      IF(T .LT. .6) THEN                                                VDF00370
         ARG=2.*(1.-H)/T                                                VDF00380
         IF (ARG.LT.TMAX) THEN                                          VDF00390
            IF (ARG.LT.TMIN) V=V+1.-ARG                                 VDF00400
            IF (ARG.GE.TMIN) V=V+EXP(-ARG)                              VDF00410
            ARG=2.*(1.+H)/T                                             VDF00420
            IF(ARG.LT.TMAX) THEN                                        VDF00430
               V=V+EXP(-ARG)                                            VDF00440
               ARG=4.*(2-H)/T                                           VDF00450
               IF(ARG .LT. TMAX) THEN                                   VDF00460
                  V=V+EXP(-ARG)                                         VDF00470
                  ARG=4.*(2+H)/T                                        VDF00480
                  IF(ARG.LT.TMAX) V=V+EXP(-ARG)                         VDF00490
               ENDIF                                                    VDF00500
            ENDIF                                                       VDF00510
         ENDIF                                                          VDF00520
         ARG=-.5*H**2/T                                                 VDF00530
         IF (ARG.LT.-90.) V=0.                                          VDF00540
C  .797885=ROOT 2/PI                                                    VDF00550
         IF(ARG.GE.-90.) V=.797885*V*EXP(ARG)/S                         VDF00560
C  TO PREVENT UNDERFLOWS                                                VDF00570
         IF (V.LT.1E-30) V=0.                                           VDF00580
         RETURN                                                         VDF00590
      ENDIF                                                             VDF00600
                                                                        VDF00610
C  4.934802=PI**2/2                                                     VDF00620
      ARG=4.934802*T                                                    VDF00630
                                                                        VDF00640
      IF (ARG.LT.TMAX) THEN                                             VDF00650
         V=V+2.*EXP(-ARG)*COS(PI*H)                                     VDF00660
C  19.739209=2*PI**2                                                    VDF00670
         ARG=19.739209*T                                                VDF00680
         IF (ARG.LT.TMAX) V=V+2.*EXP(-ARG)*COS(2.*PI*H)                 VDF00690
      ENDIF                                                             VDF00700
                                                                        VDF00710
      V=V/D                                                             VDF00720
C  TO PREVENT UNDERFLOWS                                                VDF00730
      IF (V.LT.1E-30) V=0.                                              VDF00740
                                                                        VDF00750
      RETURN                                                            VDF00760
      END                                                               VDF00770
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCVER00010
      SUBROUTINE VERTICAL( HLR,HORIZ,VERT)                              VER00020
C                                                                       VER00030
C PURPOSE:  COMPUTE GAUSSIAN VERTICAL TERM AND THEN RELATIVE CONCENTRATIVER00040
C                                                                       VER00050
C I/O:   HLR, HOURLY MIXING HEIGHT                                      VER00060
C      HORIZ, GAUSSIAN HORIZONTAL TERM                                  VER00070
C       VERT, GAUSSIAN VERTICAL TERM                                    VER00080
C                                                                       VER00090
C CALLED BY:  CALC                                                      VER00100
C                                                                       VER00110
C CALLS:  VDF                                                           VER00120
C                                                                       VER00130
C       MINERALS MANAGEMENT SERVICE                                     VER00140
C       U.S. DEPARTMENT OF THE INTERIOR                                 VER00150
C                                                                       VER00160
C OCD             REVISION HISTORY:                                     VER00170
C    DCD 880916   CREATED.                                              VER00180
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCVER00190
                                                                        VER00200
      INCLUDE 'met.cmn'                                                 VER00210
      INCLUDE 'ptrrcp.cmn'                                              VER00220
                                                                        VER00230
C   TUPOS METHOD                                                        VER00240
C                                                                       VER00250
C   1) CALCULATION WITH REFLECTION USING TERRAIN HEIGHT AND TERRAIN     VER00260
C      CORRECTION FACTOR  (RC)                                          VER00270
C   2) CALCULATION WITH REFLECTION USING GAUSSIAN FORMULA WITH FLAT     VER00280
C      TERRAIN AT HH   (RC2)                                            VER00290
C    USE THE SMALLER OF RC AND RC2                                      VER00300
                                                                        VER00310
10000 IF (Z) 15000,20000,30000                                          VER00320
                                                                        VER00330
15000 RC = 0.0                                                          VER00340
      RETURN                                                            VER00350
                                                                        VER00360
C     SUBROUTINE VDF USES FOURIER SERIES FOR MULTIPLE REFLECTION        VER00370
20000 CALL VDF(H,HLR,SZ,VERT)                                           VER00380
      GO TO 40000                                                       VER00390
                                                                        VER00400
30000 CALL VDF(H-Z,HLR-Z,SZ,V1)                                         VER00410
      CALL VDF(H+Z,HLR+Z,SZ,V2)                                         VER00420
      VERT=(V1+V2)*0.5                                                  VER00430
                                                                        VER00440
40000 RC=HORIZ*VERT/UPL                                                 VER00450
                                                                        VER00460
      HEFF=AMIN1(HA,HLR-HA)                                             VER00470
      CHK=SZ/HEFF                                                       VER00480
      IF(CHK.LE.0.71) THEN                                              VER00490
         RAT=1.0                                                        VER00500
      ELSEIF( 0.71.LT.CHK.AND.0.85.GE.CHK) THEN                         VER00510
         RAT=2.01428-1.42857*CHK                                        VER00520
      ELSEIF( 0.85.LT.CHK.AND.0.93.GE.CHK) THEN                         VER00530
         RAT=2.925-2.5*CHK                                              VER00540
      ELSEIF( 0.93.LT.CHK.AND.0.97.GE.CHK) THEN                         VER00550
         RAT=5.25-5.0*CHK                                               VER00560
      ELSEIF( 0.97.LT.CHK.AND.1.0.GE.CHK) THEN                          VER00570
         RAT=13.3333-13.333*CHK                                         VER00580
      ELSE                                                              VER00590
         RAT=0.                                                         VER00600
      ENDIF                                                             VER00610
      ZZ=RAT*HEFF                                                       VER00620
      CALL VDF(HEFF-ZZ,HLR-ZZ,SZ,V1)                                    VER00630
      CALL VDF(HEFF+ZZ,HLR+ZZ,SZ,V2)                                    VER00640
      VERT2=(V1+V2)*0.5                                                 VER00650
      RC2 = HORIZ*VERT2/UPL                                             VER00660
                                                                        VER00670
      RC = AMIN1(RC,RC2)                                                VER00680
                                                                        VER00690
      IF(RC2.LT.RC) VERT=VERT2                                          VER00700
                                                                        VER00710
      RETURN                                                            VER00720
      END                                                               VER00730
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCVIR00010
      SUBROUTINE VIRT( DENOM,XL,XD,SIGZWA,SIGZA,                        VIR00020
     &                 SIGTHS,SIGTHL,SIGTHC,XSR,VIRTY,VIRTZ)            VIR00030
C                                                                       VIR00040
C PURPOSE:  CALCULATE VIRTUAL DISTANCES                                 VIR00050
C                                                                       VIR00060
C I/O:DENOM, DENOMINATOR USED IN VIRTUAL Z SOURCE TERM                  VIR00070
C        XD, DISTANCE FROM SOURCE TO RECEPTOR, in KM                    VIR00080
C        XL, DISTANCE FROM SOURCE TO WHERE PLUME CROSSES TIBL, in KM    VIR00090
c    SIGZWA, overwater sigma-z, class D for overwater classes A - D     VIR00100
C    SIGZA,  SIGMA-Z AT XD-XL+VIRTZ                                     VIR00110
C    SIGTHS, SOURCE SIGMA-THETA                                         VIR00120
C    SIGTHL, LAND SIGMA-THETA                                           VIR00130
C    SIGTHC, SOURCE SIGMA-THETA TIMES FY                                VIR00140
C    XSR,    DISTANCE FROM VIRTUAL SOURCE TO RECEPTOR, in METERS        VIR00150
C    VIRTY,  VIRTUAL Y DISTANCE, in KM                                  VIR00160
C    VIRTZ,  VIRTUAL Z DISTANCE, in KM                                  VIR00170
C                                                                       VIR00180
C CALLED BY:  CALC                                                      VIR00190
C                                                                       VIR00200
C CALLS:  SIGMA                                                         VIR00210
C                                                                       VIR00220
C       MINERALS MANAGEMENT SERVICE                                     VIR00230
C       U.S. DEPARTMENT OF THE INTERIOR                                 VIR00240
C                                                                       VIR00250
C OCD             REVISION HISTORY:                                     VIR00260
C    DCD 880915   CREATED.                                              VIR00270
C    JCC 930310   UPDATED.                                              XXX00280
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCVIR00280
                                                                        VIR00290
      include 'params.cmn'
      INCLUDE 'opt.cmn'                                                 VIR00300
      INCLUDE 'met.cmn'                                                 VIR00310
      INCLUDE 'funcs.cmn'                                               VIR00320
      INCLUDE 'ptrrcp.cmn'                                              VIR00330
      INCLUDE 'store.cmn'                                               VIR00340
      INCLUDE 'const.cmn'                                               VIR00350
      INCLUDE 'hrdat.cmn'                                               VIR00360
      INCLUDE 'linesrc.cmn'                                             VIR00370
                                                                        VIR00380
      DIMENSION C1(6)                                                   VIR00390
C   BRIGGS(1973) RURAL DEFAULT IZ VALUES                                VIR00400
      REAL BRIGGS(7)/ .20,.12,.08,.06,.03,.016,.016/                    VIR00410
      DATA C1/0.0,0.0,0.0002,0.0015,0.0003,0.0003/                      VIR00420
      REAL  izlstemp                                                    xxx00420
                                                                        VIR00430
C  SKIP VIRTUAL SOURCE CALCULATIONS IF THERE IS NO WATER BETWEEN        VIR00440
C  SOURCE AND RECEPTOR                                                  VIR00450
      IF(XLAND.LE.0.0.OR.XL.LE.0.0) GO TO 600                           VIR00460
C                                                                       VIR00470
C       USING TURBULENCE DATA OVER LAND; COMPUTE                        VIR00480
C       VIRTUAL DISTANCES                                               VIR00490
C                                                                       VIR00500
C  AREA SOURCE VIRTUAL SOURCE CALCULATION                               VIR00510
C  XD IS DISTANCE FROM AREA SOURCE CENTER TO RECEPTOR                   VIR00520
C  D IS THE CIRCLE DIAMETER REPRESENTING THE AREA SOURCE IN METERS      VIR00530
      IF(IOPT(20) .EQ. 1) THEN                                          VIR00540
          FRAC = (D/2.)/(SIGTHS*SQRT(2.))                               VIR00550
           b = 0.81*frac*frac+4000.*upl*frac                            XXX00550
           tt = (0.9*frac+sqrt(b))/(2000.*upl)                          XXX00551
           virty = 1000.*upl*tt*tt                                      XXX00552
C                                                                       VIR00580
C  LINE SOURCE VIRTUAL SOURCE CALCULATION                               VIR00590
C  XD IS DISTANCE FROM LINE SOURCE CENTER TO RECEPTOR                   VIR00600
C  SEGL IS THE DISTANCE BETWEEN LINE SOURCE MIDPOINTS                   VIR00610
      ELSEIF(IOPT(20) .EQ. 2 ) THEN                                     VIR00620
          FRAC = (SEGL*KM2M)/(SIGTHS*SQRT(2.))                          VIR00630
           b = 0.81*frac*frac+4000.*upl*frac                            XXX00630
           tt = (0.9*frac+sqrt(b))/(2000.*upl)                          XXX00631
           virty = 1000.*upl*tt*tt                                      XXX00632
      ELSE                                                              VIR00660
C   POINT SOURCE VIRTUAL SOURCE CALCULATION                             VIR00670
C   CALCULATE SIGTHC AT LAND INTERFACE FIRST USING DRAXLER FORMULATION  VIR00680
         IF(XL .LT. 10.0) THEN                                          VIR00690
            TIME = XL*KM2M/UPL                                          VIR00700
         ELSE                                                           VIR00710
            TIME = 10000./UPL                                           VIR00720
         ENDIF                                                          VIR00730
         FY = 1./(1.+0.9*SQRT(TIME/1000.))                              VIR00740
         SIGTHC = SIGTHS*FY                                             VIR00750
                                                                        VIR00760
         FRAC = SIGTHC*XL*KM2M/SIGTHL                                   VIR00770
C  VIRTY IS CALCULATED (BASED ON DRAXLER FORMULATION) IN METERS         VIR00780
C  First decide whether the full version of the Draxler formulation     XXX00780
c  (i.e. sigma-y is proportional to x**0.5) should be applied for       XXX00781
c  small to moderate downwind distances, or the simplified version of   XXX00782
c  the Draxler formulation (i.e. sigma-y is proportional to x) should   XXX00783
c  be applied for large downwind distances) according to the value      XXX00784
c  of FRAC                                                              XXX00785
c                                                                       XXX00786
          cutoff=10000./(1.+0.9*sqrt(10./upl))                          XXX00787
c                                                                       XXX00788
c  CUTOFF defines the cut-off point between the two regimes of Draxler  XXX00789
c  formulation.                                                         XXX00790
c                                                                       XXX00791
           if (frac.le.cutoff) then                                     XXX00792
               b = 0.81*frac*frac+4000.*upl*frac                        XXX00793
               tt = (0.9*frac+sqrt(b))/(2000.*upl)                      XXX00794
               virty = 1000.*upl*tt*tt                                  XXX00795
c  That is, SIGTHS*XL*KM2M*FY(XL) = SIGTHL*VIRTY*FY(VIRTY)              XXX00796
          else                                                          XXX00797
               virty = frac*(1.+0.9*sqrt(10./upl))                      XXX00798
          end if                                                        XXX00799
      ENDIF                                                             VIR00810
                                                                        VIR00820
C  CALCULATE DISTANCE FROM VIRTUAL SOURCE TO RECEPTOR (M)               VIR00830
C     THTA1 = ANGLE FROM WIND DIRECTION TO RECEPTOR                     VIR00840
      thta1 = acos_x (x/xd,'VIRT')                                      XXX00840
c  Note that thta1 must be between 0 and pi/2 since both x and xd       XXX00841
c  are greater than zero.                                               XXX00842
      DELTAX = XL*KM2M - VIRTY                                          VIR00910
c                                                                       XXX00910
c  Note that DELTAX > 0 if SIGTHL > SIGTHS                              XXX00911
c            DELTAX < 0 if SIGTHL < SIGTHS                              XXX00912
c                                                                       XXX00910
      XSR = SQRT((XD*KM2M)**2+DELTAX**2-2.*XD*KM2M*DELTAX*COS(THTA1))   VIR00920
c                                                                       XXX00920
c  Note that the above formula for XSR is valid regardless of the sign  XXX00921
c  of DELTAX.                                                           XXX00922
c  This is because although when DELTAX < 0, the above formula should   XXX00923
c  be written as                                                        XXX00924
c  XSR=SQRT((XD*KM2M)**2+DELTAX**2-2.*XD*KM2M*|DELTAX|*COS(PI-THTA1));  XXX00925
c  however, it is equal to                                              XXX00926
c  XSR=SQRT((XD*KM2M)**2+DELTAX**2-2.*XD*KM2M*DELTAX*COS(THTA1))        XXX00927
c  given |DELTAX| = -DELTAX and COS(PI-THTA1) = -COS(THTA1)             XXX00928
c                                                                       XXX00928
C  CONVERT VIRTY TO KM                                                  VIR00930
      VIRTY = VIRTY/KM2M                                                VIR00940
                                                                        VIR00950
C  RECALCULATE SIGTHC AT XSR USING LAND SIGMA THETA WITH DRAXLER        VIR00960
C  FORMULATION                                                          VIR00970
        time = amin1(xsr,10000.)/upl                                    XXX00970
        sigthc = sigthl/(1.+0.9*sqrt(time/1000.))                       XXX00971
c                                                                       VIR00990
C   FOR AREA SOURCE CLOSE TO SHORE                                      VIR01000
      IF(IOPT(20) .EQ. 1 .AND. XD .LT. 10.*(D/(KM2M*2.))) THEN          VIR01010
         VIRTZ = (D/2.)/KM2M                                            VIR01020
      ELSE                                                              VIR01030
c        IF(IZLS.LE.0.0) THEN                                           VIR01040
c           M = IA(KST)                                                 VIR01050
c           L = IA(KST+1) - 1                                           VIR01060
c           DO 560 KK = M,L                                             VIR01070
c              IF(SIGZS(KK).GT.SIGZWA) GO TO 570                        VIR01080
c560        CONTINUE                                                    VIR01090
c570        VIRTZ = (SIGZWA*ASI(KK))**BSI(KK)                           VIR01100
c        ELSE                                                           VIR01110
        IF(IZLS.LE.0.0)  THEN                                           XXX01110
          izlstemp = BRIGGS(KST)                                        XXX01111
        ELSE                                                            XXX01112
          izlstemp = IZLS                                               XXX01113
        ENDIF                                                           XXX01114
c           FRAC = SIGZWA/IZLS                                          VIR01120
            FRAC = SIGZWA/izlstemp                                      XXX01120
            FRAC2 = FRAC*FRAC                                           VIR01130
            C1S = C1(KST)                                               VIR01140
            IF(KST.LE.2) VIRTZ = FRAC/1000.                             VIR01150
            IF(KST.EQ.3.OR.KST.EQ.4) VIRTZ = (0.5*C1S*FRAC2 +           VIR01160
     &         0.5*FRAC*SQRT(C1S*C1S*FRAC2 + 4.0))/1000.                VIR01170
            IF(KST.GT.4) THEN                                           VIR01180
c              DENOM = IZLS - C1S*SIGZWA                                VIR01190
               DENOM = izlstemp - C1S*SIGZWA                            XXX01190
               VIRTZ = 0.0                                              VIR01200
C                                                                       VIR01210
C       IF DENOM IS LESS THAN ZERO, TREAT THE PLUME AS NOT GROWING WITH VIR01220
C       ADDITIONAL DISTANCE; THE VIRTUAL DISTANCE IS INFINITY.          VIR01230
               IF(DENOM.GT.0.0) VIRTZ = 0.001 * SIGZWA/DENOM            VIR01240
            ENDIF                                                       VIR01250
         ENDIF                                                          VIR01260
cjc  JC modifications of 1/24/03.  Should comment out the next statement.
C     ENDIF                                                             VIR01270
cjc  End of JC modifications.
C     ENDIF                                                             XXX01270
                                                                        VIR01280
C600  IF(IZLS.GT.0.0.AND.DENOM.LE.0.0.AND.KST.GT.4) THEN                VIR01290
600   IF(DENOM.LE.0.0.AND.KST.GT.4)  THEN                               XXX01290
         SIGZA = SIGZWA                                                 VIR01300
      ELSE                                                              VIR01310
C  Modify the following statement                                       XXX01310
C         IF(IZLS .LE. 0.0) IZLS = BRIGGS(KST)                          VIR01320
          dummy = izls                                                  XXX01320
          if(dummy .le. 0.0) dummy = briggs(kst)                        XXX01321
C                                                                       VIR01330
C   LINE OR AREA SOURCES WHERE OVERWATER RECEPTOR IS CLOSE TO SOURCE,   VIR01340
C   CALCULATE SIGMA-Z AT XD SINCE XL HAS BEEN MULT. BY TEN              VIR01350
         IF((XD.LT.XLAND) .AND.                                         VIR01360
     &     ((IOPT(20) .EQ. 1) .OR. (IOPT(20) .EQ. 2))) THEN             VIR01370
C   Modify the following statement                                      XXX01370
C           CALL SIGMA(XD,XD,KST,IYLS,IZLS,DUM,SIGZA,UPL,2)             VIR01380
            call sigma(xd,xd,kst,iyls,dummy,dum,sigza,upl,2)            XXX01380
         ELSE IF(XD-XL+VIRTZ.LE.0.0) THEN                               VIR01390
C   Modify the following statement                                      XXX01390
C           CALL SIGMA(XD,XD,KST,IYLS,IZLS,DUM,SIGZA,UPL,2)             VIR01400
            call sigma(xd,xd,kst,iyls,dummy,dum,sigza,upl,2)            XXX01400
         ELSE                                                           VIR01410
C   Modify the following statement                                      XXX01410
C            CALL SIGMA(XD,XD-XL+VIRTZ,KST,IYLS,IZLS,DUM,SIGZA,UPL,2)   VIR01420
             call sigma(xd,xd-xl+virtz,kst,iyls,dummy,dum,sigza,upl,2)  XXX01420
         ENDIF                                                          VIR01430
      ENDIF                                                             VIR01440
                                                                        VIR01450
      RETURN                                                            VIR01460
      END                                                               VIR01470
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCWRC00010
      SUBROUTINE WRCONC(RMET,CONC,NR)
C                                                                       WRC00030
C PURPOSE:  WRITES ONE CONCENTRATION FILE RECORD, WHICH HAS UP          WRC00040
C           TO MAXREC RECEPTOR CONCENTRATIONS.                          WRC00050
c           (MAXREC is defined in PARAMS.CMN.)
C                                                                       WRC00060
C I/O:  RMET, ARRAY OF 7 MET PARAMETERS FOR THIS HOUR                   WRC00070
c          rmet(1): overwater mixing height
c          rmet(2): wind direction (OCD keeps track of
c                   only either overwater or overland wind
c                   direction depending on JOPT(1))
c          rmet(3): overwater stability category
c          rmet(4): overwater wind speed
c          rmet(5): overland mixing height
c          rmet(6): overland stability category
c          rmet(7): overland wind speed
C       CONC, ARRAY OF SIZE NREC TO RECEIVE HOURLY CONCENTRATIONS       WRC00080
C         NR, # OF CONCENTRATIONS TO BE WRITTEN FOR EACH RECORD         WRC00090
C                                                                       WRC00100
C CALLED BY:  HRCON                                                     WRC00110
C                                                                       WRC00120
C CALLS:  NONE                                                          WRC00130
C                                                                       WRC00140
C       MINERALS MANAGEMENT SERVICE                                     WRC00150
C       U.S. DEPARTMENT OF THE INTERIOR                                 WRC00160
C                                                                       WRC00170
C OCD             REVISION HISTORY:                                     WRC00180
C    DCD 880910   CREATED.                                              WRC00190
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCWRC00200
                                                                        WRC00210
      DIMENSION CONC(nr),RMET(7)
                                                                        WRC00230
      WRITE(12) RMET,(CONC(I),I=1,NR)                                   WRC00240
                                                                        WRC00250
      RETURN                                                            WRC00260
      END                                                               WRC00270

