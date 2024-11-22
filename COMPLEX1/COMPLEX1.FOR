C***********************************************************************C1X00005
C                                                                       C1X00006
C                        COMPLEX I (DATED 92290)                        C1X00010
C                                                                       C1X00011
C       *** SEE COMPLEXI MODEL CHANGE BULLETINS MCB#2 & MCB#3 ***       C1X00012
C                                                                       C1X00013
C     ON THE SUPPORT CENTER FOR REGULATORY AIR MODELS BULLETIN BOARD    C1X00014
C                                                                       C1X00015
C                             919-541-5742                              C1X00016
C                                                                       C1X00017
C***********************************************************************C1X00018
C                 COMPLEX I - SCRAM VERSION (DATED 92290)               C1X00020
C
C***   COMPLEX I  - A POST SCREENING TERRAIN MODEL FORMULATED BY THE    C1X00070
C***   COMPLEX TERRAIN TEAM AT THE CHICAGO WORKSHOP ON AIR QUALITY      C1X00080
C***   MODELS (FEB 80)                                                  C1X00090
C                                                                       C1X00100
C->->->-> SECTION A -  GENERAL REMARKS.                                 C1X00110
C                                                                       C1X00120
C***********************************************************************C1X00130
C       NOTE: ON AUGUST 20, 1980 A SLIGHT MODIFICATION WAS MADE         C1X00140
C             TO THE SUBROUTINE PTR. THIS CHANGE BY TOM PIERCE.         C1X00150
C       NOTE: THE CARD INPUT FOR SOURCES DIFFERS SLIGHTLY FROM          C1X00160
C             PREVIOUS VERSIONS. ENDP SHOULD NOW BE INPUT TO            C1X00170
C             INDICATE THE LAST OF THE POINT SOURCES.                   C1X00180
C       NOTE: ON 10 DEC 1980 ADDITIONS AND CHANGES WERE                 C1X00190
C             MADE BY JOHN S. IRWIN OF THE ENVIRONMENTAL                C1X00200
C             OPERATIONS BRANCH.  THE CHANGES WERE MADE                 C1X00210
C             MAINLY BY COLLECTING THE TERRAIN                          C1X00220
C             TREATMENTS INTO ONE SUBROUTINE CMPLX.                     C1X00230
C             THIS NEW VERSION OF THE MODEL YIELDS                      C1X00240
C             DIFFERENT RESULTS FOR MODELED CONCENTRATIONS.             C1X00250
C       NOTE: IN  FEB 1986, TWO NEW OPTIONS WERE ADDED:                 C1X00260
C        IOPT(26) CALM PROCESSING                                       C1X00270
C        IOPT(27) VALLEY-TYPE SCREENING OPTION                          C1X00280
C       NOTE: IN OCT 1992, THE VALLEY SCREENING OPTION WAS MODIFIED     C1X00282
C             TO AUTOMATICALLY PRESET THE WIND PROFILE EXPONENT TO      C1X00284
C             ZERO AS IT IS IN THE VALLEY MODEL                         C1X00286
C***********************************************************************C1X00290
C                                                                       C1X00300
C***   COMPLEX I  IS A MULTIPLE POINT SOURCE CODE WITH TERRAIN          C1X00310
C   ADJUSTMENT.  HORIZONTAL PLUME SPREADING IS UNIFORM ACROSS A 22.5    C1X00320
C   DEGREE SECTOR.  TO CONFORM  TO THE TEAMS CONCEPT (SEE ABOVE) THE    C1X00330
C   FOLLOWING SHOULD BE ACCOMPLISHED WITH APPROPRIATE ENTRY OF INPUT:   C1X00340
C CARD 5:                                                               C1X00350
C    1) USE TERRAIN ADJUSTMENT IOPT(1)=1.                               C1X00360
C    2) USE BUOYANCY-INDUCED DISPERSION IOPT(4)=1.                      C1X00370
C    3) SET IOPT(25)=1.                                                 C1X00380
C CARD 6:                                                               C1X00390
C    4) USE THE FOLLOWING TERRAIN ADJUSTMENT VALUES:                    C1X00400
C       0.5,0.5,0.5,0.5,0.0,0.0                                         C1X00410
C    5) SET ZMIN=10.                                                    C1X00420
C                                                                       C1X00430
C***********************************************************************C1X00440
C                                                                       C1X00450
C***   IOPT(25) OPTIONS (IN BRIEF)                                      C1X00460
C                                                                       C1X00470
C   IF(IOPT(25)=0, COMPLEX I IS SIMILAR TO MPTER EXCEPT THAT            C1X00480
C   HORIZONTAL SPREADING IS UNIFORM ACROSS 22.5 DEGREES.                C1X00490
C                                                                       C1X00500
C   IF(IOPT(25)=1, PLUME HEIGHTS ARE NOT ALLOWED TO BE CLOSER           C1X00510
C   THAN ZMIN (IN METERS) TO RECEPTORS.                                 C1X00520
C                                                                       C1X00530
C   IF(IOPT(25)=2, ONE CALCULATION IS MADE WITH THE RECEPTOR AT         C1X00540
C   PLUME HEIGHT OVER LEVEL TERRAIN.  A SECOND CALCULATION IS MADE      C1X00550
C   WITH TERRAIN ALLOWING THE PLUME HEIGHT NO CLOSER THAN ZMIN          C1X00560
C   (SAME AS IOPT(25)=1).  THE MINIMUM OF THESE TWO CALCULATIONS        C1X00570
C   IS USED.                                                            C1X00580
C                                                                       C1X00590
C   IF IOPT(25)=3, CONCENTRATIONS ARE CALCULATED AS IF THERE IS         C1X00600
C   NO TERRAIN BUT THAT THE RECEPTOR IS AT THE SAME MEAN SEA            C1X00610
C   LEVEL HEIGHT AS WITH THE TERRAIN.                                   C1X00620
C                                                                       C1X00630
C   IF IOPT(25)=4, CONCENTRATIONS ARE CALCULATED THE                    C1X00640
C   SAME AS IF IOPT(25)=1 SO LONG AS CTER IS GT                         C1X00650
C   0.001.  IF CTER IS LT 0.001, WE COMPUTE                             C1X00660
C   AS IOPT(25)=1 FOR TERRAIN WHOSE MSL HEIGHT IS                       C1X00670
C   EQUAL TO OR BELOW THE MSL HEIGHT OF THE PLUME.                      C1X00680
C   FOR TERRAIN ABOVE THE PLUME, THE RECEPTOR IS AT                     C1X00690
C   ITS MSL HEIGHT (AS IF ON A POLE) AND THE PLUME                      C1X00700
C   IS ASSUMED TO GO BENEATH THE RECEPTOR.  THE                         C1X00710
C   GROUND IS "FAKED IN" AT A HEIGHT ZMIN BELOW                         C1X00720
C   THE PLUME.                                                          C1X00730
C                                                                       C1X00740
C   IF IOPT(25)=5, ONE CALCULATION IS MADE WITH THE RECEPTOR AT         C1X00750
C   PLUME HEIGHT OVER LEVEL TERRAIN.  A SECOND CALCULATION IS MADE      C1X00760
C   AS IF IOPT(25)=4.  THE MINIMUM OF THESE TWO CALCULATIONS            C1X00770
C   IS USED.                                                            C1X00780
C***   IOPT(26) - PROCESSING IS PERFORMED FOR CALM PERIODS IN A         C1X00790
C                 MANNER SIMILAR TO THE CALMPRO ROUTINE.                C1X00800
C    IF IOPT(26)=1, A CHECK FOR CALM CONDITIONS IS PERFORMED FOR        C1X00810
C    EACH GROUP OF AVERAGING PERIODS AND RESULTING CONCENTRATIONS       C1X00820
C    FOR THE HOUR ARE SET EQUAL TO ZERO.  AVERAGING PERIODS CONTAINING  C1X00830
C    1 OR MORE CALM HOURS ARE DIVIDED BY THE TOTAL NUMBER OF NON-CALM   C1X00840
C    HOURS IN THE PERIOD OR 75% OF THE AVERAGING PERIOD WHICHEVER IS    C1X00850
C    GREATER. (I.E. 3 HOUR AVERAGES ARE ALWAYS DIVIDED BY 3, 8 HOUR     C1X00860
C    AVERAGES BY 6 OR GREATER, 12 HOUR AVERAGES BY 9 OR GREATER, AND    C1X00870
C    24 HOUR AVERAGES BY 18 OR GREATER.)                                C1X00880
C       CALM PERIODS FOR THE HIGH FIVE TABLES ARE DENOTED BY A 'C' FLAG C1X00890
C    NEXT TO THE CONCENTRATION.  A COUNT OF THE TOTAL NUMBER OF CALMS   C1X00900
C    IN THE RUN IS ALSO PERFORMED AND LISTED IN THE FINAL AVERAGE       C1X00910
C    CONCENTRATION TABLE.                                               C1X00920
C***   IOPT(27) - PROCESSING IS PERFORMED IN  A SIMILAR MANNER          C1X00930
C                 TO THE SHORT-TERM SCREENING MODE OF THE VALLEY MODEL. C1X00940
C    IF IOPT(27)=1, COMPLEX I OPTIONS AND PARAMETERS ARE SET AS         C1X00950
C    NECESSARY TO PRODUCE RESULTS SIMILAR TO THE VALLEY MODEL           C1X00960
C    WHEN RUN AS A SCREENING MODEL.  FOR EACH WIND DIRECTION INPUT      C1X00970
C    BY THE USER, THE VALLEY SCREENING OPTION ESTIMATES THE             C1X00980
C    MAXIMUM 24-HOUR CONCENTRATION EXPECTED AT EACH USER                C1X00990
C    PROVIDED RECEPTOR.  THE MODEL ASSUMES THAT THE GIVEN               C1X01000
C    WIND DIRECTION EXISTS FOR ANY 6 HOURS OF A 24 HOUR PERIOD.         C1X01010
C    (THIS IS ACCOMPLISHED IN THE MODEL BY REDUCING THE USER            C1X01020
C    SUPPLIED ONE-HOUR EMISSION RATE TO 6/24 OF ITS ACTUAL              C1X01030
C    VALUE.)  ALL RUNS WITH THE VALLEY SCREENING OPTION                 C1X01040
C    ASSUME A WIND SPEED OF 2.5 M/SEC, A STABILITY OF 'F',              C1X01050
C    AND AN UNLIMITED MIXING HEIGHT (9999 METERS).                      C1X01060
C         THE VALLEY SCREENING OPTION INCLUDES AN ALGORITHM             C1X01070
C    FOR THE TREATMENT OF COMPLEX TERRAIN EFFECTS ON CONCENTRATION.     C1X01080
C    PLUME HEIGHT ABOVE STACK BASE IS ASSUMED TO REMAIN                 C1X01090
C    CONSTANT AFTER DISTANCE TO FINAL PLUME RISE.  IN EFFECT, AS        C1X01100
C    THE PLUME APPROACHES THE ELEVATED SURFACE,                         C1X01110
C    THE PLUME HEIGHT DECREASES.  IT IS FURTHER ASSUMED THAT            C1X01120
C    THE PLUME CENTERLINE COMES NO CLOSER THAN 10 METERS TO             C1X01130
C    THE ELEVATED TERRAIN.  IF THE TERRAIN EXTENDS ABOVE                C1X01140
C    THIS LEVEL, THE PLUME CENTERLINE IS                                C1X01150
C    ADJUSTED SO THAT IT REMAINS 10 METERS ABOVE THE GROUND.            C1X01160
C    ANY PLUME HEIGHT WHICH IS INITIALLY WITHIN 10 METERS OF            C1X01170
C    THE GROUND IS ASSUMED TO REMAIN AT ITS INITIAL HEIGHT              C1X01180
C    ABOVE GROUND, REGARDLESS OF DOWNWIND TERRAIN ELEVATIONS.           C1X01190
C         DEFLECTION OF THE PLUME BY TERRAIN IS SIMULATED               C1X01200
C    THROUGH THE ATTENUATION OF CONCENTRATION WITH HEIGHT               C1X01210
C    FOR TERRAIN ABOVE CENTERLINE HEIGHT.  A FACTOR IS                  C1X01220
C    APPLIED WHICH HAS A VALUE OF UNITY AT AND BELOW THE ELEVA-         C1X01230
C    TION OF THE PLUME CENTERLINE, BUT DECREASES LINEARLY WITH          C1X01240
C    INCREASING HEIGHT TO ZERO AT AND ABOVE 400                         C1X01250
C    METERS ABOVE THE UNDISTURBED PLUME CENTERLINE. THE                 C1X01260
C    ATTENUATION SCHEME IS NOT APPLIED TO PLUMES WITH                   C1X01270
C    HEIGHTS INITIALLY LESS THAN 10 METERS.                             C1X01280
C                                                                       C1X01290
C       EXECUTION OF COMPLEX I  IS LIMITED TO A MAXIMUM OF 250 POINT    C1X01300
C        SOURCES AND 180 RECEPTORS.  SIMULATION IS DONE HOUR-BY-HOUR    C1X01310
C        AND HOURLY METEOROLOGICAL DATA ARE REQUIRED AS INPUT.  LENGTH  C1X01320
C        OF SIMULATED TIME CAN VARY FROM 1 HOUR TO 1 YEAR.              C1X01330
C                                                                       C1X01340
C***   COMPLEX I  FABRICATOR:                                           C1X01350
C        D. BRUCE TURNER*                                               C1X01360
C        ENVIRONMENTAL OPERATIONS BRANCH                                C1X01370
C        METEOROLOGY AND ASSESSMENT DISISION, ESRL                      C1X01380
C        ENVIRONMENTAL PROTECTION AGENCY                                C1X01390
C                                                                       C1X01400
C        * ON ASSIGNMENT FROM NATIONAL OCEANIC AND ATMOSPHERIC ADMIN.,  C1X01410
C          DEPARTMENT OF COMMERCE.                                      C1X01420
C                                                                       C1X01430
C***    AS OF THIS DATE 80353, THIS PROGRAM IS NOT BEING SUPPORTED,     C1X01440
C        THAT IS, NO ONE HAS ASSUMED RESPONSIBILITY FOR MAINTENANCE.    C1X01450
C                                                                       C1X01460
C      ************************************************************     C1X01470
C      *            IMPORTANT MESSAGE NO. 1 !!!                   *     C1X01480
C      *       CALCULATIONS SUBMITTED TO SATISFY REGULATORY       *     C1X01490
C      *  REQUIREMENTS MAY REQUIRE CERTAIN PARAMETER VALUES       *     C1X01500
C      *  FOR WIND PROFILE POWER-LAW EXPONENTS AND TERRAIN        *     C1X01510
C      *  ADJUSTMENT FACTORS AND USE OF CERTAIN OPTIONS. CHECK    *     C1X01520
C      *  WITH THE APPROPRIATE EPA REGIONAL OFFICE TO INSURE THAT *     C1X01530
C      *  ACCEPTABLE PARAMETER VALUES ARE USED IN YOUR RUN.       *     C1X01540
C      ************************************************************     C1X01550
C                                                                       C1X01560
C***   TWO SYSTEMS OF LENGTH AND COORDINATES ARE USED IN COMPLEX I :    C1X01570
C                                                                       C1X01580
C             THE FIRST SYSTEM, USER UNITS, IS SELECTED BY THE USER AND C1X01590
C        NORMALLY USES THE COORDINATE SYSTEM OF THE EMISSION INVENTORY. C1X01600
C        ALL LOCATIONS INPUT BY THE USER (SUCH AS SOURCES AND RECEPTORS)C1X01610
C        ARE IN THIS SYSTEM. ALSO AS A CONVENIENCE TO THE USER, ALL     C1X01620
C        LOCATIONS ON OUTPUT ARE ALSO IN THIS SYSTEM.                   C1X01630
C                                                                       C1X01640
C             THE SECOND SYSTEM, X, Y, IS AN UPWIND, CROSSWIND          C1X01650
C        COORDINATE SYSTEM RELATIVE TO EACH RECEPTOR.  THE X-AXIS IS    C1X01660
C        DIRECTED UPWIND (SAME AS WIND DIRECTION FOR THE HOUR).  IN     C1X01670
C        ORDER TO DETERMINE DISPERSION PARAMETER VALUES AND EVALUATE    C1X01680
C        EQUATIONS FOR CONCENTRATION ESTIMATES, DISTANCES IN THIS       C1X01690
C        SYSTEM MUST BE IN KILOMETERS. THIS SYSTEM IS INTERNAL AND IS   C1X01700
C        NOT APPARENT TO THE USER.                                      C1X01710
C                                                                       C1X01720
C->->->-> SECTION B -  DATA INPUT LISTS.                                C1X01730
C                                                                       C1X01740
C    THE RUNSTREAM FOR COMPLEX I  IS NEARLY THE SAME AS THAT FOR MPTER. C1X01750
C THE ONLY EXCEPTIONS ARE THAT A VALUE IS ENTERED FOR IOPT(25) ON CARD  C1X01760
C 5, AND A VALUE FOR ZMIN IS ENTERED AS THE LAST PARAMETER ON CARD 6.   C1X01770
C                                                                       C1X01780
C***   CARD VARIABLES AND FORMAT.                                       C1X01790
C             THE REQUIRED AND OPTIONAL CARD TYPES USED AS INPUT TO     C1X01800
C        COMPLEX I  ARE DESCRIBED BELOW:                                C1X01810
C                                                                       C1X01820
C***   CARDS 1 - 3  ALPHANUMERIC DATA FOR TITLES. FORMAT(20A4)          C1X01830
C                                                                       C1X01840
C        LINE1 - 80 ALPHANUMERIC CHARACTERS.                            C1X01850
C        LINE2 - 80 ALPHANUMERIC CHARACTERS.                            C1X01860
C        LINE3 - 80 ALPHANUMERIC CHARACTERS.                            C1X01870
C                                                                       C1X01880
C***   CARD 4  CONTROL AND CONSTANTS.  FORMAT(FREE)                     C1X01890
C                                                                       C1X01900
C        IDATE(1) -  2-DIGIT YEAR FOR THIS RUN.                         C1X01910
C        IDATE(2) -  STARTING JULIAN DAY FOR THIS RUN.                  C1X01920
C        IHSTRT   -  STARTING HOUR FOR THIS RUN.                        C1X01930
C        NPER     -  NUMBER OF AVERAGING PERIODS TO BE RUN.             C1X01940
C                     IF THE VALLEY OPTION IS SELECTED (IOPT(27)=1),    C1X01950
C                     'NPER' IS THE NUMBER OF WIND DIRECTIONS TO        C1X01960
C                     BE PROCESSED (UP TO 360).                         C1X01970
C        NAVG     -  NUMBER OF HOURS IN AN AVERAGING PERIOD.            C1X01980
C                     IF THE VALLEY OPTION IS SELECTED (IOPT(27)=1),    C1X01990
C                     'NAVG' IS SET TO '1' BY THE MODEL.                C1X02000
C        IPOL     -  POLLUTANT INDICATOR; IS 3 FOR SO2, 4 FOR SUSPENDED C1X02010
C                     PARTICULATE.                                      C1X02020
C        NSIGP    -  NUMBER OF SOURCES FROM WHICH CONC. CONTRIBUTIONS   C1X02030
C                     ARE DESIRED (MAX = 25).                           C1X02040
C        NAV5     -  ADDITIONAL AVERAGING TIME FOR HIGH-FIVE TABLE;     C1X02050
C                     MOST LIKELY EQUAL TO 2, 4, 6, OR 12.              C1X02060
C        CONONE   -  MULTIPLIER TO CONVERT USER UNITS TO KILOMETERS.    C1X02070
C                       EXAMPLE MULTIPLIERS:                            C1X02080
C                         FEET TO KM    3.048E-04                       C1X02090
C                         MILES TO KM   1.609344                        C1X02100
C                         METERS TO KM  1.0E-03                         C1X02110
C        CELM     -  MULTIPLIER TO CONVERT USER HEIGHT UNITS TO METERS. C1X02120
C                       EXAMPLE MULTIPLIER:                             C1X02130
C                         FEET TO METERS  0.3048                        C1X02140
C        HAFL     -  POLLUTANT HALF-LIFE, SECONDS. AN ENTRY OF ZERO WILLC1X02150
C                     CAUSE SKIPPING OF POLLUTANT LOSS CALCULATIONS.    C1X02160
C                                                                       C1X02170
C      ************************************************************     C1X02180
C      *            IMPORTANT MESSAGE NO. 2 !!!                   *     C1X02190
C      *       CALCULATIONS SUBMITTED TO SATISFY REGULATORY       *     C1X02200
C      *  REQUIREMENTS MAY REQUIRE THAT CERTAIN OPTIONS BE USED!  *     C1X02210
C      *  CHECK WITH THE APPROPRIATE EPA REGIONAL OFFICE TO       *     C1X02220
C      *  INSURE THAT ACCEPTABLE OPTIONS ARE USED IN YOUR RUN.    *     C1X02230
C      ************************************************************     C1X02240
C                                                                       C1X02250
C        ******************************************************         C1X02260
C        *      THE USER IS REFERRED TO THE USERS GUIDE FOR   *         C1X02270
C        *  MORE DETAILED INFORMATION ON OPTIONS. ESPECIALLY  *         C1X02280
C        *  IMPORTANT IS AN UNDERSTANDING OF PRINTED OUTPUT   *         C1X02290
C        *  AND USE OF OPTIONS 9 THROUGH 19 TO DELETE UNNEEDED*         C1X02300
C        *  INFORMATION. COMPLEX I IS CAPABLE OF GENERATING A *         C1X02310
C        *  LARGE QUANTITY OF PRINTED INFORMATION UNLESS SOME *         C1X02320
C        *  OF THESE OPTIONS TO DELETE OUTPUT ARE USED        *         C1X02330
C        *  LIBERALLY.                                        *         C1X02340
C        ******************************************************         C1X02350
C                                                                       C1X02360
C                                                                       C1X02370
C***   CARD 5.  OPTIONS.  FORMAT(FREE)                                  C1X02380
C                                                                       C1X02390
C        1 = EMPLOY OPTION;  0 = DON'T USE OPTION.                      C1X02400
C                                                                       C1X02410
C       TECHNICAL OPTIONS:                                              C1X02420
C        IOPT(1)  -  USE TERRAIN ADJUSTMENTS.                           C1X02430
C        IOPT(2)  -  NO STACK DOWNWASH.                                 C1X02440
C        IOPT(3)  -  NO GRADUAL PLUME RISE.                             C1X02450
C        IOPT(4)  -  USE BUOYANCY INDUCED DISPERSION.                   C1X02460
C                                                                       C1X02470
C       INPUT OPTIONS:                                                  C1X02480
C        IOPT(5)  -  MET. DATA IS ON CARDS.                             C1X02490
C                     (IF IOPT(26)=1, OPTION SHOULD BE SET TO 0)        C1X02500
C                     (IF IOPT(27)=1, OPTION IS SET BY MODEL TO 1)      C1X02510
C        IOPT(6)  -  READ HOURLY EMISSIONS.                             C1X02520
C                     (IF IOPT(27)=1, OPTION IS SET BY MODEL TO 0)      C1X02530
C        IOPT(7)  -  SPECIFY SIGNIFICANT SOURCES.                       C1X02540
C        IOPT(8)  -  INPUT RADIAL DISTANCES AND GENERATE POLAR          C1X02550
C                     COORDINATE RECEPTORS.                             C1X02560
C                                                                       C1X02570
C       PRINTED OUTPUT OPTIONS:                                         C1X02580
C        IOPT(9)  -  DELETE EMISSIONS WITH HEIGHT TABLE.                C1X02590
C        IOPT(10) -  DELETE RESULTANT MET. DATA SUMMARY FOR AVG. PERIOD.C1X02600
C                     (IF IOPT(27)=1, OPTION IS SET BY MODEL TO 1)      C1X02610
C        IOPT(11) -  DELETE HOURLY CONTRIBUTIONS.                       C1X02620
C        IOPT(12) -  DELETE MET. DATA ON HOURLY CONTRIBUTIONS.          C1X02630
C                     (IF IOPT(27)=1, OPTION IS SET BY MODEL TO 1)      C1X02640
C        IOPT(13) -  DELETE FINAL PLUME HEIGHT AND DISTANCE TO FINAL    C1X02650
C                     RISE ON HOURLY CONTRIBUTIONS.                     C1X02660
C        IOPT(14) -  DELETE HOURLY SUMMARY.                             C1X02670
C        IOPT(15) -  DELETE MET. DATA ON HOURLY SUMMARY.                C1X02680
C                     (IF IOPT(27)=1, OPTION IS SET BY MODEL TO 1)      C1X02690
C        IOPT(16) -  DELETE FINAL PLUME HEIGHT AND DISTANCE TO FINAL    C1X02700
C                     RISE ON HOURLY SUMMARY.                           C1X02710
C        IOPT(17) -  DELETE AVERAGING-PERIOD CONTRIBUTIONS.             C1X02720
C                     (IF IOPT(27)=1, OPTION IS SET BY MODEL TO 1)      C1X02730
C        IOPT(18) -  DELETE AVERAGING-PERIOD SUMMARY.                   C1X02740
C                     (IF IOPT(27)=1, OPTION IS SET BY MODEL TO 1)      C1X02750
C        IOPT(19) -  DELETE AVERAGE CONCENTRATIONS AND HIGH-FIVE TABLE. C1X02760
C                                                                       C1X02770
C       OTHER CONTROL AND OUTPUT OPTIONS:                               C1X02780
C        IOPT(20) -  RUN IS PART OF A SEGMENTED LONG RUN.               C1X02790
C                     (IF IOPT(27)=1, OPTION IS SET BY MODEL TO 0)      C1X02800
C        IOPT(21) -  WRITE PARTIAL CONCENTRATIONS TO DISK OR TAPE.      C1X02810
C                     (IF IOPT(27)=1, OPTION IS SET BY MODEL TO 0)      C1X02820
C        IOPT(22) -  WRITE HOURLY CONCENTRATIONS TO DISK OR TAPE.       C1X02830
C                     (IF IOPT(27)=1, OPTION IS SET BY MODEL TO 0)      C1X02840
C        IOPT(23) -  WRITE AVERAGING-PERIOD CONCS TO DISK OR TAPE.      C1X02850
C                     (IF IOPT(27)=1, OPTION IS SET BY MODEL TO 0)      C1X02860
C        IOPT(24) -  PUNCH AVERAGING-PERIOD CONCENTRATIONS ON CARDS.    C1X02870
C                     (IF IOPT(27)=1, OPTION IS SET BY MODEL TO 0)      C1X02880
C                                                                       C1X02890
C        IOPT(25) - COMPLEX TERRAIN OPTION                              C1X02900
C                    (IF IOPT(27)=1, OPTION IS SET BY MODEL TO 0)       C1X02910
C                                                                       C1X02920
C        IOPT(26) - CALM PROCESSING OPTION                              C1X02930
C                    (IF IOPT(27)=1, OPTION IS SET BY MODEL TO 0)       C1X02940
C                                                                       C1X02950
C        IOPT(27) - VALLEY SCREENING OPTION                             C1X02960
C                                                                       C1X02970
C      ************************************************************     C1X02980
C      *            IMPORTANT MESSAGE NO. 3 !!!                   *     C1X02990
C      *       CALCULATIONS SUBMITTED TO SATISFY REGULATORY       *     C1X03000
C      *  REQUIREMENTS MAY REQUIRE THAT CERTAIN PARAMETER VALUES  *     C1X03010
C      *  BE USED FOR THE VARIABLES ON THIS CARD.  CHECK WITH THE *     C1X03020
C      *  APPROPRIATE EPA REGIONAL OFFICE TO INSURE THAT          *     C1X03030
C      *  ACCEPTABLE PARAMETER VALUES ARE USED IN YOUR RUN!       *     C1X03040
C      ************************************************************     C1X03050
C                                                                       C1X03060
C***   CARD 6.  WIND AND TERRAIN. FORMAT(FREE)                          C1X03070
C                                                                       C1X03080
C        HANE     -  ANEMOMETER HEIGHT (METERS)                         C1X03090
C        PL(I),I=1,6 -  WIND SPEED POWER LAW PROFILE EXPONENTS FOR EACH C1X03100
C***
C***     CHANGE 10/15/92:  ADD (IF ...) AFTER "STABILITY."  BY:  RFL
C***
C                        STABILITY.  (IF IOPT(27)=1, PL(6)=0.)          C1X03110
C***
C***
C        CONTER(I),I=1,6 -  TERRAIN ADJUSTMENT FACTORS FOR EACH         C1X03120
C                            STABILITY.                                 C1X03130
C                            (IF IOPT(27) = 1, CONTER(6) = 0. (DEFAULT))C1X03140
C        ZMIN - DISTANCE LIMIT FOR PLUME CENTERLINE FROM GROUND.        C1X03150
C                (IF IOPT(27) = 1, ZMIN = 10. (DEFAULT))                C1X03160
C                                                                       C1X03170
C***   CARD TYPE 7.  POINT SOURCE CARD.  FORMAT(3A4,8F8.2,F4.0)         C1X03180
C                                                                       C1X03190
C       (UP TO 250 POINT SOURCE CARDS ARE ALLOWED.)                     C1X03200
C        PNAME(I,NPT)I=1,3 -  12 CHARACTER SOURCE IDENTIFICATION.       C1X03210
C        SOURCE(1,NPT)  -  EAST COORDINATE OF POINT SOURCE (USER UNITS) C1X03220
C        SOURCE(2,NPT)  -  NORTH COORDINATE OF POINT SOURCE (USER UNITS)C1X03230
C        SOURCE(3,NPT)  -  SULFUR DIOXIDE EMISSION RATE (G/SEC).        C1X03240
C        SOURCE(4,NPT)  -  PARTICULATE EMISSION RATE (G/SEC).           C1X03250
C        SOURCE(5,NPT)  -  PHYSICAL STACK HEIGHT (METERS).              C1X03260
C        SOURCE(6,NPT)  -  STACK GAS TEMPERATURE (KELVIN).              C1X03270
C        SOURCE(7,NPT)  -  STACK INSIDE DIAMETER (METERS).              C1X03280
C        SOURCE(8,NPT)  -  STACK GAS EXIT VELOCITY (M/SEC).             C1X03290
C        ELP(NPT)       -  SOURCE GROUND-LEVEL ELEVATION (USER HT UNITS)C1X03300
C                                                                       C1X03310
C         CARD WITH 'ENDP' IN COLS 1-4 IS USED TO SIGNIFY THE           C1X03320
C          END OF THE POINT SOURCES.                                    C1X03330
C                                                                       C1X03340
C***   CARD TYPE 8.  SPECIFIED SIGNIFICANT SOURCES.  FORMAT(26I3)       C1X03350
C                                                                       C1X03360
C       (USED IF OPTION 7 = 1)                                          C1X03370
C        INPT           -  NUMBER OF USER SPECIFIED SIGNIFICANT SOURCES.C1X03380
C        MPS(I),I=1,NPT -  POINT SOURCE NUMBERS USER WANTS CONSIDERED   C1X03390
C                           SIGNIFICANT.                                C1X03400
C                                                                       C1X03410
C***   CARD TYPE 9. MET. DATA IDENTIFIERS.  FORMAT(FREE)                C1X03420
C                                                                       C1X03430
C       (USED IF OPTION 5 = 0)                                          C1X03440
C        ISFCD    -  SFC MET STATION IDENTIFIER    (5 DIGITS)           C1X03450
C        ISFCYR   -  YEAR OF SFC MET DATA          (2 DIGITS)           C1X03460
C        IMXD     -  UPPER-AIR STATION IDENTIFIER  (5 DIGITS)           C1X03470
C        IMXYR    -  YEAR OF MIXING HEIGHT DATA    (2 DIGITS)           C1X03480
C                                                                       C1X03490
C***   CARD TYPE 10.  POLAR COORDINATE RECEPTORS.  FORMAT(FREE)         C1X03500
C                                                                       C1X03510
C       (USED IF OPTION 8 = 1)                                          C1X03520
C        RADIL(I),I= 1,5 -  ONE TO FIVE RADIAL DISTANCES (REST OF FIVE  C1X03530
C                            ARE ZEROS) EACH OF WHICH GENERATES 36      C1X03540
C                            RECEPTORS AROUND POINT CENTX, CENTY ON     C1X03550
C                            AZIMUTHS 10 TO 360 DEGREES. (USER UNITS)   C1X03560
C                             (USER UNITS)                              C1X03570
C        CENTX    -  EAST COORDINATE ABOUT WHICH RADIALS ARE CENTERED.  C1X03580
C        CENTY    -  NORTH COORDINATE ABOUT WHICH RADIALS ARE CENTERED. C1X03590
C                          (USER UNITS)                                 C1X03600
C                                                                       C1X03610
C***   CARD TYPE 11.  POLAR COORDINATE RECEPTOR ELEVATIONS.             C1X03620
C                                                                       C1X03630
C       FORMAT(I2,8X,5F10.0).  (USED IF OPTIONS 1 AND 8 ARE BOTH 1)     C1X03640
C        IDUM     -  AZIMUTH INDICATOR (1 TO 36)                        C1X03650
C        ELRDUM(I),I=1,5  -  RECEPTOR ELEVATIONS FOR THIS AZIMUTH FOR   C1X03660
C                             UP TO FIVE DISTANCES (USER HEIGHT UNITS). C1X03670
C                                                                       C1X03680
C***   CARD TYPE 12.  RECEPTOR.  FORMAT(2A4,2F10.3,2F10.0)              C1X03690
C                                                                       C1X03700
C       (UP TO 180 RECEPTORS MAY BE GENERATED INCLUDING POLAR COORDINATEC1X03710
C        ONES IF OPTION 8 = 1.)                                         C1X03720
C        RNAME(I),I=1,2 -  8 DIGIT ALPHANUMERIC STATION IDENTIFICATION. C1X03730
C        RREC     -  EAST COORDINATE OF RECEPTOR (USER UNITS)           C1X03740
C        SREC     -  NORTH COORDINATE OF RECEPTOR (USER UNITS)          C1X03750
C        ZR       -  RECEPTOR HEIGHT ABOVE LOCAL GROUND-LEVEL (METERS)  C1X03760
C        ELR      -  RECEPTOR GROUND-LEVEL ELEVATION (USER HT UNITS)    C1X03770
C                                                                       C1X03780
C         CARD WITH 'ENDR' IN COLS 1-4 IS USED TO SIGNIFY THE END OF    C1X03790
C          THE RECEPTOR CARDS.                                          C1X03800
C                                                                       C1X03810
C***   CARD TYPE 13.  SEGMENTED RUN. FORMAT(FREE)                       C1X03820
C                                                                       C1X03830
C       (USED IF OPTION 20=1)                                           C1X03840
C        IDAY     -  NUMBER OF DAYS PREVIOUSLY PROCESSED.               C1X03850
C        LDRUN    -  LAST DAY TO BE PROCESSED IN THIS RUN.              C1X03860
C                                                                       C1X03870
C***   CARD TYPE 14A.  METEOROLOGY.  FORMAT(FREE)                       C1X03880
C                                                                       C1X03890
C       (ONE CARD FOR EACH HOUR OF THE SIMULATION.)                     C1X03900
C       (USED IF OPTION 5 = 1)                                          C1X03910
C       (USED IF OPTION 5 = 1 AND OPTION 27 = 0)                        C1X03920
C        JYR      -  YEAR OF MET DATA. (2 DIGITS)                       C1X03930
C        DAY1     -  JULIAN DAY OF MET DATA.                            C1X03940
C        JHR      -  HOUR OF MET DATA.                                  C1X03950
C        IKST     -  STABILITY CLASS FOR THIS HOUR.                     C1X03960
C        QU       -  WIND SPEED FOR THIS HOUR (M/SEC).                  C1X03970
C        QTEMP    -  AMBIENT AIR TEMPERATURE FOR THIS HOUR (KELVIN).    C1X03980
C        QTHETA   -  WIND DIRECTION FOR THIS HOUR (DEGREES AZIMUTH FROM C1X03990
C                     WHICH THE WIND BLOWS).                            C1X04000
C        QHL      -  MIXING HEIGHT FOR THIS HOUR (METERS).              C1X04010
C                                                                       C1X04020
C***    CARD TYPE 14B. METEOROLOGY.  FORMAT(F10.0,/,(16F5.0))           C1X04030
C                                                                       C1X04040
C        (USED IF OPTION 27 = 1)                                        C1X04050
C        (ONE CARD FOR TEMPERATURE AND UP TO 360 WIND DIRECTIONS        C1X04060
C         ON 23 CARDS)                                                  C1X04070
C         VTEMP     -  AMBIENT TEMPERATURE (K).                         C1X04080
C         VDIR      -  WIND DIRECTIONS (DEGREES AZIMUTH FROM            C1X04090
C                      WHICH THE WIND BLOWS).                           C1X04100
C                                                                       C1X04110
C->->->-> SECTION C -  COMMON, DIMENSION, AND DATA STATEMENTS.          C1X04120
C                                                                       C1X04130
C         /EXPOS/  BETWEEN MAIN PROGRAM AND BLOCK DATA                  C1X04140
      COMMON /EXPOS/ PXUCOF(6,9),PXUEXP(6,9),HC1(10)                    C1X04150
C         /MPOR/ BETWEEN MAIN, PTR, OUTHR,  RCP, AND RANK               C1X04160
C        IOPT HAS BEEN REDIMENSIONED TO 27 TO INCLUDE THE NEW CALM      C1X04170
C        PROCESSING AND VALLEY OPTIONS IN ALL SUBROUTINES AND MAIN.     C1X04180
      COMMON /MPOR/ IOPT(27),ZMIN                                       C1X04190
C         /MPO/ BETWEEN MAIN, PTR, AND OUTHR                            C1X04200
      COMMON /MPO/ NRECEP,NAVG,NB,LH,NPT,IDATE(2),RREC(180),SREC(180),ZRC1X04210
     1(180),ELR(180),PHCHI(180),PHSIGS(180,26),HSAV(250),DSAV(250),PCHI(C1X04220
     2180),PSIGS(180,26),IPOL                                           C1X04230
C         /MPR/  BETWEEN MAIN, PTR, AND RCP                             C1X04240
      COMMON /MPR/ UPL,Z,H,HL,X,Y,KST,DELH,SY,SZ,RC                     C1X04250
C         /MP/  BETWEEN MAIN PROGRAM AND PTR                            C1X04260
      COMMON /MP/ SOURCE(9,250),CONTWO,PSAV(250),IPSIGS(250),U,TEMP,SINTC1X04270
     1,COST,PL(6),ELP(250),ELHN,HANE,TLOS                               C1X04280
C         /MO/ AND /MOC/ BETWEEN MAIN PROGRAM AND OUTHR                 C1X04290
      COMMON /MO/ QTHETA(24),QU(24),IKST(24),QHL(24),QTEMP(24),MPS(25), C1X04300
     1            NSIGP,IO,IRANK(180)                                   C1X04310
      COMMON /MOC/LINE1(20),LINE2(20),LINE3(20),RNAME(2,180),STAR(5,180)C1X04320
C         /MR/  BETWEEN MAIN PROGRAM AND RANK                           C1X04330
      COMMON /MR/ HMAXA(5,180,5),NDAY(5,180,5),IHR(5,180,5),CONC(180,5),C1X04340
     1JDAY,NR                                                           C1X04350
C                                                                       C1X04360
C          NEW COMMON AREA FOR USE WITH NEW SUBROUTINE CMPLX            C1X04370
C          EP AND ER ARE DEFINED IN PTR                                 C1X04380
C                                                                       C1X04390
      COMMON /MSL/ ER,EP,CELM,CTER,ICLM,ICALMP,VDIR(360)                C1X04400
      CHARACTER*5 CRADIL                                                IBM
      CHARACTER*4 RNAME,PNAME,ANAME,ENDP,ENDR,DUM,TITLE,                IBM
     1            LINE1,LINE2,LINE3                                     IBM
      CHARACTER*1 BLNK,STAR,STR,C,CF
      DIMENSION PNAME(3,250), IFREQ(7), DUMR(24), HLH(2,24), IMPS(25), TC1X04410
     1ITLE(2), TABLE(2,21), CONTER(6), RADIL(5), ANAME(36)              C1X04420
      DIMENSION SUM(180),ELRDUM(5),NTIME(5)                             C1X04430
      DIMENSION IDUMR(24),CF(5),NC(24),RDAY(5,180)                      C1X04440
C                                                                       C1X04450
      DATA IFREQ /7*0/ ,BLNK /' '/                                      C1X04460
      DATA TITLE /'S02 ','PART'/                                        C1X04470
      DATA ENDP /'ENDP'/ ,ENDR /'ENDR'/                                 C1X04480
      DATA MAXP /250/ ,STR/'*'/                                         IBM04490
C        MAXP EQUALS SECOND DIMENSION OF THE ARRAY NAMED: SOURCE.       C1X04500
      DATA ANAME /' 10,',' 20,',' 30,',' 40,',' 50,',' 60,',' 70,',' 80,C1X04510
     1',' 90,','100,','110,','120,','130,','140,','150,','160,','170,','C1X04520
     2180,','190,','200,','210,','220,','230,','240,','250,','260,','270C1X04530
     3,','280,','290,','300,','310,','320,','330,','340,','350,','360,'/C1X04540
     4                                                                  C1X04550
C                                                                       C1X04560
      DATA NTIME /1,3,8,24,0/                                           C1X04570
      DATA ITMIN1 /9999/ ,IDIV8 /0/, IDIV24 /0/, ICALM /0/, IDIVX /0/   C1X04580
      DATA ICFL3 /0/, ICFL8 /0/, ICFL24 /0/, ICFLX /0/, C /'C'/,IDAY/0/ C1X04590
      DATA CONTER /6*0./                                                C1X04600
C                                                                       C1X04610
C     CALL WSTCLK
C**   PRINT HEADERS                                                     C1X04620
      WRITE (6,5)                                                       C1X04630
5     FORMAT('1',21X,'       COMPLEX I (DATED 92290)' / '1'   )         C1X04640
C                                                                       C1X04690
C->->->->SECTION D - FLOW DIAGRAM                                       C1X04700
C                                                                       C1X04710
C       RELATION OF SUBROUTINES IN COMPLEX I                            C1X04720
C                                                                       C1X04730
C                   COMPLEX I                                           C1X04740
C                     !                                                 C1X04750
C                     ! * READ INPUT DATA                               C1X04760
C             --------! - LOOP FOR CALENDAR DAYS                        C1X04770
C             !       !                                                 C1X04780
C             !  -----! - LOOP FOR AVERAGING TIME                       C1X04790
C             !  !    !                                                 C1X04800
C             !  !    ! * READ MET DATA                                 C1X04810
C             !  !    ! * ANGARC                                        C1X04820
C             !  !    !                                                 C1X04830
C             !  !  --! - LOOP ON HOURS                                 C1X04840
C             !  !  ! !                                                 C1X04850
C             !  !  ! ! * * * * * PTR                                   C1X04860
C             !  !  ! !            !                                    C1X04870
C             !  !  ! !        ----- - LOOP ON RECEPTORS                C1X04880
C             !  !  ! !       !    !                                    C1X04890
C             !  !  ! !       !  --- - LOOP ON SOURCES                  C1X04900
C             !  !  ! !       !  ! !                                    C1X04910
C             !  !  ! !       !  ! ! * * RCP                            C1X04920
C             !  !  ! !       !  ! !      !                             C1X04930
C             !  !  ! !       !  ! !      **PGZ                         C1X04940
C             !  !  ! !       !  ! !                                    C1X04950
C             !  !  ! !       !  --*                                    C1X04960
C             !  !  ! !       -----*                                    C1X04970
C             !  !  ! !                                                 C1X04980
C             !  !  ! ! * RANK                                          C1X04990
C             !  !  ! !                                                 C1X05000
C             !  !  ! ! * OUTHR                                         C1X05010
C             !  !  ! !                                                 C1X05020
C             !  !  --*                                                 C1X05030
C             !  !    !                                                 C1X05040
C             !  !    ! * OUTAVG (ENTRY POINT IN OUTHR)                 C1X05050
C             !  !    !                                                 C1X05060
C             !  -----*                                                 C1X05070
C             !       !                                                 C1X05080
C             --------*                                                 C1X05090
C                     !                                                 C1X05100
C                    EXIT                                               C1X05110
C                                                                       C1X05120
C->->->->SECTION E - RUN SET-UP AND READ FIRST 6 INPUT CARDS.           C1X05130
C                                                                       C1X05140
C        INITIALIZATIONS............                                    C1X05150
C        THE FOLLOWING 18 STATEMENTS MAY BE DELETED FOR USE ON          C1X05160
C        COMPUTERS THAT ZERO CORE LOCATIONS USED BY A PROBLEM           C1X05170
C        PRIOR TO EXECUTION.                                            C1X05180
      NRECEP=0                                                          C1X05190
      NP=0                                                              C1X05200
      NHR=0                                                             C1X05210
      NP3=0                                                             C1X05220
      NP8=0                                                             C1X05230
      NP24=0                                                            C1X05240
      NPX=0                                                             C1X05250
      DO 10 I=1,21                                                      C1X05260
      TABLE(1,I)=0.                                                     C1X05270
10    TABLE(2,I)=0.                                                     C1X05280
      DO 40 I=1,180                                                     C1X05290
      SUM(I)=0.                                                         C1X05300
      DO 30 J=1,5                                                       C1X05310
      CONC(I,J)=0.                                                      C1X05320
      DO 20 K=1,5                                                       C1X05330
20    HMAXA(J,I,K)=0.                                                   C1X05340
30    CONTINUE                                                          C1X05350
40    CONTINUE                                                          C1X05360
C        I/O DEVICE INITIALIZATIONS                                     C1X05370
      IN=5                                                              C1X05380
      IO=6                                                              C1X05390
C        UNIT 11 - DISK INPUT OF MET DATA--USED WHEN IOPT(5)=0.         C1X05400
C        UNIT 10 - DISK OUPUT OF PARTIAL CONCENTRATIONS                 C1X05410
C         AT EACH RECEPTOR--USED WHEN IOPT(21) = 1.                     C1X05420
C        UNIT 12 TAPE/DISK OUTPUT OF HRLY CONCENTRATIONS-IF IOPT(22)=1. C1X05430
C        UNIT 13 TAPE/DISK OUTPUT OF CONCENTRATIONS FOR AVERAGING PERIODC1X05440
C           USED IF IOPT(23) = 1.                                       C1X05450
C        UNIT 14 TAPE/DISK STORAGE FOR SUMMARY INFO, USED IF IOPT(20)=1.C1X05460
C        UNIT 15 - TAPE/DISK INPUT OF HOURLY POINT SOURCE EMISSIONS     C1X05470
C          -- USED IF IOPT(6) = 1.                                      C1X05480
C                                                                       C1X05490
C        READ CARDS 1-3  (SEE DESCRIPTION, SECTION B).                  C1X05500
C                                                                       C1X05510
      READ (IN,1180) LINE1,LINE2,LINE3                                  C1X05520
      WRITE (IO,1400) LINE1,LINE2,LINE3                                 C1X05530
C                                                                       C1X05540
C        READ CARD TYPE 4  (SEE DESCRIPTION, SECTION B).                C1X05550
C                                                                       C1X05560
C***    IF THE VALLEY SCREENING OPTION HAS BEEN SELECTED,               C1X05570
C***    'NPER' IS THE NUMBER OF WIND DIRECTIONS TO BE                   C1X05580
C***    PROCESSED.                                                      C1X05590
      READ (IN, *  ) IDATE(1),IDATE(2),IHSTRT,NPER,NAVG,IPOL,NSIGP,NAV5,C1X05600
     1CONONE,CELM,HAFL                                                  C1X05610
C        THE ABOVE FORMAT IS IBM'S FREE FIELD INPUT.                    IBM05620
C        VARIABLES MUST BE SEPARATED BY COMMAS.                         C1X05630
C        THIS IS SIMILAR TO IBM'S LIST DIRECTED IO.                     C1X05640
      IF (NSIGP.LE.25) GO TO 50                                         C1X05650
      WRITE (IO,1250) NSIGP                                             C1X05660
C     CALL WAUDIT
      STOP                                                              C1X05670
50    IP=IPOL-2                                                         C1X05680
      CONTWO=CONONE                                                     C1X05690
C                                                                       C1X05700
C         ************************************************              C1X05710
C         *     SEE IMPORTANT  MESSAGE                   *              C1X05720
C         *           NUMBER 2 ABOVE!                    *              C1X05730
C         ************************************************              C1X05740
C                                                                       C1X05750
C        READ CARD TYPE 5  (SEE DESCRIPTION, SECTION B).                C1X05760
C                                                                       C1X05770
      READ (IN, *  ) (IOPT(I),I=1,27)                                   C1X05780
C***    SEVERAL OPTIONS AND PARAMETERS ARE FIXED IF                     C1X05790
C***    THE VALLEY SCREENING OPTION IS SELECTED.                        C1X05800
      IF(IOPT(27) .EQ. 1) THEN                                          C1X05810
        IOPT(5)=1                                                       C1X05820
        IOPT(6)=0                                                       C1X05830
        IOPT(10)=1                                                      C1X05840
        IOPT(12)=1                                                      C1X05850
        IOPT(15)=1                                                      C1X05860
        IOPT(17)=1                                                      C1X05870
        IOPT(18)=1                                                      C1X05880
        IOPT(20)=0                                                      C1X05890
        IOPT(21)=0                                                      C1X05900
        IOPT(22)=0                                                      C1X05910
        IOPT(23)=0                                                      C1X05920
        IOPT(24)=0                                                      C1X05930
        IOPT(25)=0                                                      C1X05940
        IOPT(26)=0                                                      C1X05950
        NAV5=0                                                          C1X05960
        IHSTRT=1                                                        C1X05970
        NAVG=1                                                          C1X05980
      END IF                                                            C1X05990
      IF (IOPT(5) .EQ. 1 .AND. IOPT(26) .EQ. 1) THEN                    C1X06000
        WRITE(IO,1247)                                                  C1X06010
C       CALL WAUDIT
        STOP                                                            C1X06020
      ENDIF                                                             C1X06030
C           WRITE GENERAL INPUT INFORMATION                             C1X06040
C***    IF THE VALLEY OPTION IS SELECTED, CHANGE OUTPUT HEADINGS.       C1X06050
      IF(IOPT(27) .EQ. 1) THEN                                          C1X06060
        WRITE(IO,1409) TITLE(IP),NPER,CONTWO,NSIGP                      C1X06070
      ELSE                                                              C1X06080
      WRITE (IO,1410) TITLE(IP),NPER,NAVG,IHSTRT,IDATE(2),IDATE(1),CONTWC1X06090
     1O,NSIGP                                                           C1X06100
      END IF                                                            C1X06110
      DAY1A=IDATE(2)                                                    C1X06120
      HR1=IHSTRT                                                        C1X06130
      IF (HAFL.GT.0.0) GO TO 60                                         C1X06140
      TLOS=0.                                                           C1X06150
      WRITE (IO,1420)                                                   C1X06160
      GO TO 70                                                          C1X06170
60    WRITE (IO,1430) HAFL                                              C1X06180
      TLOS=693./HAFL                                                    C1X06190
70    IF (IOPT(19).EQ.1) GO TO 80                                       C1X06200
      NAVT=5                                                            C1X06210
      IF (NAV5.EQ.1.OR.NAV5.EQ.3.OR.NAV5.EQ.8.OR.NAV5.EQ.24.OR.NAV5.EQ.0C1X06220
     1) NAVT=4                                                          C1X06230
C***    ONLY 24-HOUR AVERAGE CONCENTRATIONS ARE DESIRED                 C1X06240
C***    FOR THE VALLEY OPTION.                                          C1X06250
      IF(IOPT(27) .EQ. 1) THEN                                          C1X06260
        NAVT=1                                                          C1X06270
        NTIME(1)=24                                                     C1X06280
      END IF                                                            C1X06290
      NTIME(5)=NAV5                                                     C1X06300
C***    IF THE VALLEY OPTION IS CHOSEN, CHANGE OUTPUT HEADINGS.         C1X06310
      IF(IOPT(27) .EQ. 1) THEN                                          C1X06320
        WRITE(IO,1439)                                                  C1X06330
      ELSE                                                              C1X06340
      WRITE (IO,1440) NAVT                                              C1X06350
      END IF                                                            C1X06360
80    IF (IOPT(1).EQ.0) GO TO 90                                        C1X06370
      WRITE (IO,1450) CELM                                              C1X06380
      ELHN=99999.                                                       C1X06390
      ELOW=99999.                                                       C1X06400
90    IF (NSIGP.GT.0) GO TO 100                                         C1X06410
      IOPT(11)=1                                                        C1X06420
      IOPT(17)=1                                                        C1X06430
100   WRITE (IO,1460) (I,IOPT(I),I=1,13)                                C1X06440
      WRITE (IO,1470) (I,IOPT(I),I=14,27)                               C1X06450
C                                                                       C1X06460
C        **************************************************             C1X06470
C        *    SEE IMPORTANT MESSAGE NO. 3 ABOVE           *             C1X06480
C        **************************************************             C1X06490
C                                                                       C1X06500
C        READ CARD TYPE 6 (SEE DESCRIPTION, SECTION B).                 C1X06510
C                                                                       C1X06520
      READ (IN, *  ) HANE,PL,CONTER,ZMIN                                C1X06530
C***    MINIMUM PLUME APPROACH AND NO TERRAIN CORRECTIONS               C1X06540
C***    ARE FIXED FOR THE VALLEY OPTION                                 C1X06550
      IF(IOPT(27) .EQ. 1) THEN                                          C1X06560
        ZMIN=10.                                                        C1X06570
C***
C***    CHANGE 10/15/92:  INSERT NEW LINE:  PL(6)=0.  BY:  RFL
C***
        PL(6)=0.                                                        C1X06575
C***
C***
        CONTER(6)=0.                                                    C1X06580
      END IF                                                            C1X06590
      IF (IOPT(1).EQ.1) GO TO 110                                       C1X06600
      WRITE (IO,1480) HANE,PL                                           C1X06610
      GO TO 140                                                         C1X06620
110   WRITE (IO,1490) HANE,PL,CONTER,ZMIN                               C1X06630
      IF(ZMIN.LT.0.) THEN                                               C1X06640
      WRITE(IO,112)                                                     C1X06650
112   FORMAT('0ZMIN IS NEGATIVE.')                                      C1X06660
C     CALL WAUDIT
      STOP                                                              C1X06670
      END IF                                                            C1X06680
      DO 120 I=1,6                                                      C1X06690
      IF (CONTER(I).LT.0..OR.CONTER(I).GT.1.) GO TO 130                 C1X06700
120   CONTINUE                                                          C1X06710
      GO TO 140                                                         C1X06720
130   WRITE (IO,1260)                                                   C1X06730
C     CALL WAUDIT
      STOP                                                              C1X06740
C                                                                       C1X06750
C        MUCH OF THE FOLLOWING PROGRAM SECTION IS BASED UPON            C1X06760
C        RAMQ IN THE RAM SYSTEM. THIS SECTION IS RESPONSIBLE            C1X06770
C        FOR MAKING THE NECESSARY DATA CONVERSIONS ON THE RAW           C1X06780
C        EMISSIONS DATA IN ORDER TO ESTABLISH A STANDARD                C1X06790
C        DATA BANK WHICH WILL BE ACCEPTABLE. A CONVERSION FACTOR        C1X06800
C        FROM USER UNITS TO KILOMETERS IS APPLIED WHEN NECESSARY.       C1X06810
C                                                                       C1X06820
C->->->->SECTION F - INPUT AND PROCESS EMISSION INFORMATION.            C1X06830
C                                                                       C1X06840
C***    IF THE VALLEY OPTION HAS BEEN SELECTED, OUTPUT A LIST           C1X06850
C***    OF ALL FIXED OPTIONS AND PARAMETERS.                            C1X06860
C140  WRITE (IO,1500)                                                   C1X06870
  140 IF(IOPT(27) .EQ. 1) WRITE(IO,1499)                                C1X06880
C***    IF THE VALLEY OPTION IS SELECTED, CHANGE OUTPUT                 C1X06890
C***    OF SOURCE INFORMATION.                                          C1X06900
      IF(IOPT(27) .EQ. 1) THEN                                          C1X06910
        WRITE(IO,1501)                                                  C1X06920
      ELSE                                                              C1X06930
        WRITE(IO,1500)                                                  C1X06940
      END IF                                                            C1X06950
      NPT=0                                                             C1X06960
C        BEGIN LOOP TO READ THE POINT SOURCE INFORMATION                C1X06970
150   NPT=NPT+1                                                         C1X06980
      IF (NPT.LE.MAXP) GO TO 160                                        C1X06990
      READ (IN,1200) DUM                                                C1X07000
      IF (DUM.EQ.ENDP) GO TO 230                                        C1X07010
      WRITE (IO,1270) MAXP                                              C1X07020
C     CALL WAUDIT
      STOP                                                              C1X07030
C                                                                       C1X07040
C        READ CARD TYPE 7  (SEE DESCRIPTION, SECTION B).                C1X07050
C                                                                       C1X07060
160   READ (IN,1210) (PNAME(I,NPT),I=1,3),(SOURCE(I,NPT),I=1,8),ELP(NPT)C1X07070
C        CARD WITH     'ENDP'   IN COL 1-10 IS USED TO SIGNIFY END OF   C1X07080
C        POINT SOURCES.                                                 C1X07090
      IF (PNAME(1,NPT).EQ.ENDP) GO TO 230                               C1X07100
C        ELHN, ELEVATION OF LOWEST STACK TOP IN INVENTORY, IS DETERMINEDC1X07110
C        IN USER HEIGHT UNITS                                           C1X07120
      IF (IOPT(1).EQ.0) GO TO 170                                       C1X07130
      TOM=SOURCE(5,NPT)/CELM+ELP(NPT)                                   C1X07140
      IF (TOM.LT.ELHN) ELHN=TOM                                         C1X07150
C        LOWPT, ELEVATION OF LOWEST SOURCE GROUND-LEVEL                 C1X07160
C         IN INVENTORY, IN USER HEIGHT UNITS.                           C1X07170
      IF (ELP(NPT).LT.ELOW) ELOW=ELP(NPT)                               C1X07180
C        CALCULATE BUOYANCY FACTOR                                      C1X07190
170   D=SOURCE(7,NPT)                                                   C1X07200
C         FOLLOWING VARIABLE IS BRIGGS' F WITHOUT TEMPERATURE FACTOR.   C1X07210
      SOURCE(9,NPT)=2.45153*SOURCE(8,NPT)*D*D                           C1X07220
C        2.45153 IS GRAVITY OVER FOUR.                                  C1X07230
      TS=SOURCE(6,NPT)                                                  C1X07240
      IF (TS.GT.293.) GO TO 180                                         C1X07250
      HF=SOURCE(5,NPT)                                                  C1X07260
      GO TO 200                                                         C1X07270
180   F=SOURCE(9,NPT)*(TS-293.)/TS                                      C1X07280
      IF (F.GE.55.) GO TO 190                                           C1X07290
C        ONLY BUOYANCY PLUME RISE IS CONSIDERED HERE.                   C1X07300
      HF=SOURCE(5,NPT)+21.425*F**0.75/3.                                C1X07310
      GO TO 200                                                         C1X07320
190   HF=SOURCE(5,NPT)+38.71*F**0.6/3.                                  C1X07330
C        HSAV, DSAV, AND PSAV ARE USED FOR TEMPORARY STORAGE            C1X07340
C        (OR AS DUMMIES) FOR THE NEXT 60 STATEMENTS.                    C1X07350
200   HSAV(NPT)=HF                                                      C1X07360
C        DETERMINE HEIGHT INDEX.                                        C1X07370
      DO 210 IH=2,9                                                     C1X07380
      IF (HF.LT.(HC1(IH)-.01)) GO TO 220                                C1X07390
210   CONTINUE                                                          C1X07400
      IH=10                                                             C1X07410
220   IS=IH-1                                                           C1X07420
      A=PXUCOF(2,IS)                                                    C1X07430
      B=PXUEXP(2,IS)                                                    C1X07440
      DSAV(NPT)=(A*HF**B)*SOURCE(IPOL,NPT)/3.                           C1X07450
C        AN ESTIMATE OF THE POTENTIAL IMPACT OF EACH SOURCE IS          C1X07460
C         DETERMINED AND STORED IN DSAV. MAX CONCENTRATION IS           C1X07470
C         DETERMINED BY CHI(MAX)=(A*H**B)*Q/U WHERE                     C1X07480
C         A IS THE COEFFICENT AND B IS THE EXPONENT, OF                 C1X07490
C         MAXIMUM CHI*U/Q VALUES PREDETERMINED FOR B STABILITY          C1X07500
C         AND A SPECIFIC EFFECTIVE HEIGHT RANGE. PLUME RISE             C1X07510
C         IS CALCULATED FOR B STABILITY AND 3 M/SEC WIND SPEED.         C1X07520
C                                                                       C1X07530
C        GO BACK AND READ DATA FOR ANOTHER POINT SOURCE.                C1X07540
      IPSIGS(NPT)=0                                                     C1X07550
C        LIST POINT SOURCE INFORMATION.                                 C1X07560
      IF (IOPT(27) .EQ. 1) THEN                                         C1X07570
      WRITE (IO,1509) NPT,(PNAME(J,NPT),J=1,3),(SOURCE(K,NPT),K=1,8),   C1X07580
     1   ELP(NPT),F                                                     C1X07590
      ELSE                                                              C1X07600
      WRITE (IO,1510) NPT,(PNAME(J,NPT),J=1,3),(SOURCE(K,NPT),K=1,8),DSAC1X07610
     1V(NPT),HSAV(NPT),ELP(NPT),F                                       C1X07620
      END IF                                                            C1X07630
      GO TO 150                                                         C1X07640
230   NPT=NPT-1                                                         C1X07650
C         CHECK FOR NPT < OR = 0                                        C1X07660
      IF (NPT.GT.0) GO TO 240                                           C1X07670
      WRITE (IO,1280) NPT                                               C1X07680
C     CALL WAUDIT
      STOP                                                              C1X07690
C                                                                       C1X07700
C->->->->SECTION G - RANK SIGNIFICANT SOURCES.                          C1X07710
C                                                                       C1X07720
240   IF (NSIGP.EQ.0) GO TO 280                                         C1X07730
C        RANK NSIGP HIGHEST POINT SOURCES.                              C1X07740
      IF (NPT.LT.NSIGP) NSIGP=NPT                                       C1X07750
      DO 260 I=1,NSIGP                                                  C1X07760
      SIGMAX=-1.0                                                       C1X07770
      DO 250 J=1,NPT                                                    C1X07780
      IF (DSAV(J).LE.SIGMAX) GO TO 250                                  C1X07790
      SIGMAX=DSAV(J)                                                    C1X07800
      LMAX=J                                                            C1X07810
250   CONTINUE                                                          C1X07820
C        IMPS IS THE SOURCE NUMBER IN ORDER OF SIGNIFICANCE.            C1X07830
      IMPS(I)=LMAX                                                      C1X07840
C        PSAV IS THE CALC. CONC. IN ORDER OF SIGNIFICANCE.              C1X07850
      PSAV(I)=SIGMAX                                                    C1X07860
260   DSAV(LMAX)=-1.0                                                   C1X07870
C        OUTPUT TABLE OF RANKED SOURCES.                                C1X07880
C***      BYPASS IF THE VALLEY OPTION IS SELECTED.                      C1X07890
      IF(IOPT(27) .EQ. 1) GO TO 280                                     C1X07900
      WRITE (IO,1520) TITLE(IP)                                         C1X07910
      DO 270 I=1,NSIGP                                                  C1X07920
      WRITE (IO,1530) I,PSAV(I),IMPS(I)                                 C1X07930
270   CONTINUE                                                          C1X07940
C                                                                       C1X07950
C->->->->SECTION H - EMISSIONS WITH HEIGHT TABLE.                       C1X07960
C                                                                       C1X07970
280   IF (IOPT(9).EQ.1) GO TO 340                                       C1X07980
      DO 320 I=1,NPT                                                    C1X07990
      DO 290 J=1,20                                                     C1X08000
      HC=J*5.                                                           C1X08010
      IF (SOURCE(5,I).LE.HC) GO TO 300                                  C1X08020
290   CONTINUE                                                          C1X08030
C        POINT SOURCES WITH PHYSICAL HEIGHTS GT 100 METERS ARE LISTED   C1X08040
C        SEPARATELY.                                                    C1X08050
      WRITE (IO,1540) I,SOURCE(5,I),SOURCE(IPOL,I)                      C1X08060
      GO TO 310                                                         C1X08070
C        ADD EMISSION RATE INTO TABLE AND TOTAL.                        C1X08080
300   TABLE(1,J)=TABLE(1,J)+SOURCE(IPOL,I)                              C1X08090
310   TABLE(1,21)=TABLE(1,21)+SOURCE(IPOL,I)                            C1X08100
320   CONTINUE                                                          C1X08110
C        OUTPUT SOURCE-STRENGTH-HEIGHT TABLE                            C1X08120
C        THIS TABLE DISPLAYS THE TOTAL EMISSIONS FOR POINT              C1X08130
C        SOURCES AND THE CUMULATIVE FREQUENCY ACCORDING TO              C1X08140
C        HEIGHT CLASS                                                   C1X08150
      WRITE (IO,1550) TITLE(IP)                                         C1X08160
C        HEIGHT CLASS EMISSIONS ARE IN 1                                C1X08170
C        DETERMINE CUMULATIVE PERCENT IN 2                              C1X08180
      IH1=0                                                             C1X08190
      IH2=5                                                             C1X08200
      IM1=1                                                             C1X08210
      TABLE(2,1)=TABLE(1,1)/TABLE(1,21)                                 C1X08220
      WRITE (IO,1560) IH1,IH2,(TABLE(J,1),J=1,2)                        C1X08230
      DO 330 I=2,20                                                     C1X08240
      IH2=I*5                                                           C1X08250
      IH1=IH2-4                                                         C1X08260
      IM1=I-1                                                           C1X08270
      TABLE(2,I)=TABLE(1,I)/TABLE(1,21)+TABLE(2,IM1)                    C1X08280
      WRITE (IO,1560) IH1,IH2,(TABLE(J,I),J=1,2)                        C1X08290
330   CONTINUE                                                          C1X08300
      WRITE (IO,1570) TABLE(1,21)                                       C1X08310
C                                                                       C1X08320
C->->->->SECTION I - EXECUTE FOR INPUT OF SIGNIFICANT SOURCE NUMBERS.   C1X08330
C                                                                       C1X08340
340   WRITE (IO,1580)                                                   C1X08350
      IF (IOPT(7).EQ.0) GO TO 370                                       C1X08360
C                                                                       C1X08370
C        READ CARD TYPE 8 (SEE DESCRIPTION, SECTION B).                 C1X08380
C                                                                       C1X08390
      READ (IN,1220) INPT,(MPS(I),I=1,INPT)                             C1X08400
      WRITE (IO,1590) INPT,(MPS(I),I=1,INPT)                            C1X08410
      IF (INPT.LE.NSIGP) GO TO 350                                      C1X08420
      WRITE (IO,1290) INPT,NSIGP                                        C1X08430
C     CALL WAUDIT
      STOP                                                              C1X08440
350   IF (INPT.EQ.0) GO TO 370                                          C1X08450
      IF (MPS(INPT).EQ.0) WRITE (IO,1300)                               C1X08460
      J=INPT+1                                                          C1X08470
      K=1                                                               C1X08480
C        ADD SIGNIFICANT SOURCES DETERMINED FROM RANKED SOURCE LIST     C1X08490
C        IF NSIGP GREATER THAN INPT.                                    C1X08500
      IF (J.GT.NSIGP) GO TO 390                                         C1X08510
      DO 360 I=J,NSIGP                                                  C1X08520
      MPS(I)=IMPS(K)                                                    C1X08530
360   K=K+1                                                             C1X08540
      GO TO 390                                                         C1X08550
370   DO 380 I=1,NSIGP                                                  C1X08560
380   MPS(I)=IMPS(I)                                                    C1X08570
390   WRITE (IO,1600) NPT,NSIGP,(MPS(I),I=1,NSIGP)                      C1X08580
      IF (IOPT(6).EQ.0) GO TO 410                                       C1X08590
C        SAVE AVERAGE EMISSION RATE                                     C1X08600
      DO 400 I=1,NPT                                                    C1X08610
400   PSAV(I)=SOURCE(IPOL,I)                                            C1X08620
C        FILL IN SIGNIFICANT POINT SOURCE ARRAY                         C1X08630
410   DO 420 I=1,NSIGP                                                  C1X08640
      J=MPS(I)                                                          C1X08650
420   IPSIGS(J)=I                                                       C1X08660
C                                                                       C1X08670
C->->->->SECTION J - CHECK MET DATA IF FROM FILE OF ONE YEAR'S DATA.    C1X08680
C                                                                       C1X08690
      IF (IOPT(5).EQ.1) GO TO 450                                       C1X08700
C                                                                       C1X08710
C        READ CARD TYPE 9 (SEE DESCRIPTION, SECTION B).                 C1X08720
C                                                                       C1X08730
      READ (IN, *  ) ISFCD,ISFCYR,IMXD,IMXYR                            C1X08740
C        READ ID RECORD FROM PREPROCESSED MET DISK OR TAPE FILE.        C1X08750
      READ (11) ID,IYEAR,IDM,IYM                                        C1X08760
      IF (ISFCD.EQ.ID.AND.ISFCYR.EQ.IYEAR) GO TO 430                    C1X08770
      WRITE (IO,1310) ISFCD,ISFCYR,ID,IYEAR                             C1X08780
C     CALL WAUDIT
      STOP                                                              C1X08790
430   IF (IMXD.EQ.IDM.AND.IMXYR.EQ.IYM) GO TO 440                       C1X08800
      WRITE (IO,1320) IMXD,IMXYR,IDM,IYM                                C1X08810
C     CALL WAUDIT
      STOP                                                              C1X08820
440   WRITE (IO,1610) ISFCD,ISFCYR,IMXD,IMXYR                           C1X08830
C                                                                       C1X08840
C->->->->SECTION K - GENERATE POLAR COORDINATE RECEPTORS.               C1X08850
C                                                                       C1X08860
450   NRECEP=0                                                          C1X08870
      WRITE (IO,1620)                                                   C1X08880
      IF (IOPT(8).NE.1) GO TO 520                                       C1X08890
C                                                                       C1X08900
C        READ CARD TYPE 10 (SEE DESCRIPTION, SECTION B).                C1X08910
C                                                                       C1X08920
      READ (IN, *  ) RADIL,CENTX,CENTY                                  C1X08930
      JA=0                                                              C1X08940
      DO 460 J=1,5                                                      C1X08950
      IF (RADIL(J).EQ.0) GO TO 460                                      C1X08960
      JA=JA+1                                                           C1X08970
460   CONTINUE                                                          C1X08980
      WRITE (IO,1630) CENTX,CENTY,RADIL                                 C1X08990
      DO 480 I=1,36                                                     C1X09000
C          CALCULATE THE ANGLE IN RADIANS                               C1X09010
      RADIK=FLOAT(I)*0.1745329                                          C1X09020
C        0.1745329 IS 2*PI/36                                           C1X09030
      SINRAD=SIN(RADIK)                                                 C1X09040
      COSRAD=COS(RADIK)                                                 C1X09050
      DO 470 J=1,JA                                                     C1X09060
      NRECEP=I+36*(J-1)                                                 C1X09070
      RREC(NRECEP)=(RADIL(J)*SINRAD)+CENTX                              C1X09080
C          CALCULATE THE EAST-COORDINATE                                C1X09090
      SREC(NRECEP)=(RADIL(J)*COSRAD)+CENTY                              C1X09100
C          CALCULATE THE NORTH-COORDINATE                               C1X09110
      RNAME(1,NRECEP)=ANAME(I)                                          C1X09120
C          ALPHANUMERIC INFORMATION WHICH INDICATES DEGREES AZIMUTH     C1X09130
C     ENCODE (4,1640,RNAME(2,NRECEP)) RADIL(J)                          IBM09140
C          ENCODE THE FLOATING POINT VARIABLE OF RADIAL DISTANCE        C1X09150
C          TO ALPHANUMERIC REPRESENTATION SO INFO CAN BE PRINTED        C1X09160
C                                                                       IBM
C  THE IBM3090 DOES NOT SUPPORT ENCODE SO WRITE THE RADIL DISTANCE      IBM
C  TO AN INTERNAL FILE TO CONVERT IT TO A CHARACTER VALUE               IBM
C                                                                       IBM
      ASSIGN 2531 TO NUMF                                               IBM
      IF (RADIL(J) .LT. 100.) ASSIGN 2532 TO NUMF                       IBM
      IF (RADIL(J) .LT. 0.1) ASSIGN 2533 TO NUMF                        IBM
      WRITE(CRADIL,NUMF) RADIL(J)                                       IBM
      RNAME(2,NRECEP)=CRADIL                                            IBM
C                                                                       IBM
      ZR(NRECEP)=0.                                                     C1X09170
      ELR(NRECEP)=0.                                                    C1X09180
470   CONTINUE                                                          C1X09190
480   CONTINUE                                                          C1X09200
      NRECEP=36*JA                                                      C1X09210
C                                                                       C1X09220
C->->->->SECTION L - READ POLAR COORDINATE ELEVATIONS.                  C1X09230
C                                                                       C1X09240
      IF (IOPT(1).EQ.0) GO TO 520                                       C1X09250
C                                                                       C1X09260
C        READ 36 CARDS, TYPE 11 (SEE DESCRIPTION, SECTION 8).           C1X09270
C                                                                       C1X09280
      DO 510 I=1,36                                                     C1X09290
      READ (IN,1230) IDUM,(ELRDUM(J),J=1,JA)                            C1X09300
      IF (IDUM.EQ.I) GO TO 490                                          C1X09310
      WRITE (IO,1330) I,IDUM                                            C1X09320
C     CALL WAUDIT
      STOP                                                              C1X09330
490   DO 500 J=1,JA                                                     C1X09340
      K=J-1                                                             C1X09350
      L=K*36+I                                                          C1X09360
500   ELR(L)=ELRDUM(J)                                                  C1X09370
510   CONTINUE                                                          C1X09380
C                                                                       C1X09390
C->->->->SECTION M - READ AND PROCESS RECEPTOR INFORMATION.             C1X09400
C                                                                       C1X09410
C          NOW READ CARD TYPE 12 IF NECESSARY. MUST HAVE A CARD WITH    C1X09420
C          'ENDR'IN COLS 1-4 TO INDICATE END OF RECEPTOR CARDS.         C1X09430
C          NO MORE THAN 180 RECEPTORS CAN BE INPUT TO COMPLEX I .       C1X09440
C        START LOOP TO ENTER RECEPTORS.                                 C1X09450
520   NRECEP=NRECEP+1                                                   C1X09460
      IF (NRECEP.LE.180) GO TO 540                                      C1X09470
      READ (IN,1200,END=530) DUM                                        C1X09480
      IF (DUM.EQ.ENDR) GO TO 550                                        C1X09490
530   WRITE (IO,1340)                                                   C1X09500
C     CALL WAUDIT
      STOP                                                              C1X09510
C                                                                       C1X09520
C        READ CARD TYPE 12 (SEE DESCRIPTION, SECTION B).                C1X09530
C                                                                       C1X09540
540   READ (IN,1240) (RNAME(J,NRECEP),J=1,2),RREC(NRECEP),SREC(NRECEP),ZC1X09550
     1R(NRECEP),ELR(NRECEP)                                             C1X09560
C        PLACE 'ENDR' IN COLS 1 TO 4 ON CARD FOLLOWING LAST RECEPTOR    C1X09570
C        TO END READING TYPE 12 CARDS.                                  C1X09580
      IF (RNAME(1,NRECEP).EQ.ENDR) GO TO 550                            C1X09590
      GO TO 520                                                         C1X09600
550   NRECEP=NRECEP-1                                                   C1X09610
      IF (NRECEP.GT.0) GO TO 580                                        C1X09620
      WRITE (IO,1350) NRECEP                                            C1X09630
C     CALL WAUDIT
      STOP                                                              C1X09640
C        PRINT OUT TABLE OF RECEPTORS. ***                              C1X09650
580   WRITE (IO,1650)                                                   C1X09660
      DO 590 K=1,NRECEP                                                 C1X09670
590   WRITE (IO,1660) K,STAR(1,K),STAR(2,K),(RNAME(J,K),J=1,2),RREC(K),SC1X09680
     1REC(K),ZR(K),ELR(K)                                               C1X09690
      IF (IOPT(1).EQ.0) GO TO 600                                       C1X09700
C                                                                       C1X09710
C->->->->SECTION N - POSITION FILES AS REQUIRED.                        C1X09720
C                                                                       C1X09730
600   IF (IOPT(20).EQ.0) GO TO 610                                      C1X09740
C                                                                       C1X09750
C        READ CARD TYPE 13 (SEE DESCRIPTION, SECTION B).                C1X09760
C                                                                       C1X09770
      READ (IN, *  ) IDAY,LDRUN                                         C1X09780
      WRITE (IO,1680) IDAY,LDRUN                                        C1X09790
      IF (IDAY.EQ.0) GO TO 610                                          C1X09800
C        READ INFO FOR HIGH-FIVE TABLE FROM LAST SEGMENT.               C1X09810
      READ (14) IDAYS,SUM,NHR,DAY1A,HR1,HMAXA,ICALM,NDAY,IHR            C1X09820
      REWIND 14                                                         C1X09830
      IF (IDAY.EQ.IDAYS) GO TO 610                                      C1X09840
      WRITE (IO,1360) IDAY,IDAYS                                        C1X09850
C     CALL WAUDIT
      STOP                                                              C1X09860
610   NP=IDAY*(24/NAVG)                                                 C1X09870
C        IF OPTION 21 = 1, WRITE INITIAL INFO TO UNIT 10                C1X09880
      IF (IOPT(21).EQ.1) WRITE (10) NPER,NAVG,LINE1,LINE2,LINE3         C1X09890
      IF (IOPT(22).EQ.0) GO TO 640                                      C1X09900
      IF (IDAY.LE.0) GO TO 630                                          C1X09910
C        SKIP REVIOUSLY GENERATED HOURLY RECORDS.                       C1X09920
      ISKIP=IDAY*24+2                                                   C1X09930
      DO 620 I=1,ISKIP                                                  C1X09940
620   READ (12)                                                         C1X09950
      GO TO 640                                                         C1X09960
C        WRITE LEAD TWO RECORDS ON HOURLY FILE.                         C1X09970
630   WRITE (12) NPER,NAVG,LINE1,LINE2,LINE3                            C1X09980
      WRITE (12) NRECEP,(RREC(I),I=1,NRECEP),(SREC(J),J=1,NRECEP)       C1X09990
640   IF (IOPT(23).EQ.0) GO TO 670                                      C1X10000
      IF (IDAY.LE.0) GO TO 660                                          C1X10010
C        SKIP PREVIOUSLY GENERATED AVERAGING-PERIOD FILE.               C1X10020
      ISKIP=NP+2                                                        C1X10030
      DO 650 I=1,ISKIP                                                  C1X10040
650   READ (13)                                                         C1X10050
      GO TO 670                                                         C1X10060
C        WRITE LEAD TWO RECORDS ON AVERAGING PERIOD FILE.               C1X10070
660   WRITE (13) NPER,NAVG,LINE1,LINE2,LINE3                            C1X10080
      WRITE (13) NRECEP,(RREC(I),I=1,NRECEP),(SREC(J),J=1,NRECEP)       C1X10090
670   IF (IOPT(6).EQ.0) GO TO 690                                       C1X10100
      IF (IDAY.LE.0) GO TO 690                                          C1X10110
      ISKIP=IDAY*24                                                     C1X10120
      DO 680 I=1,ISKIP                                                  C1X10130
680   READ (15)                                                         C1X10140
690   IDAY=IDATE(2)-1                                                   C1X10150
      IF (IDAY.LE.0.OR.IOPT(5).EQ.1) GO TO 710                          C1X10160
C        SKIP PREVIOUSLY USED HOURLY EMISSION RECORDS.                  C1X10170
      DO 700 I=1,IDAY                                                   C1X10180
700   READ (11)                                                         C1X10190
710   CONTINUE                                                          C1X10200
C                                                                       C1X10210
C->->->->SECTION O - START LOOPS FOR DAY AND AVG TIME; READ MET DATA.   C1X10220
C                                                                       C1X10230
C***    START LOOP FOR EACH WIND DIRECTION IF VALLEY OPTION             C1X10240
C***    IS SELECTED.  BEFORE LOOP, READ AND WRITE TEMP AND EACH WIND    C1X10250
C***    DIRECTION (CARD TYPE 14B).                                      C1X10260
      IF(IOPT(27) .EQ. 1) THEN                                          C1X10270
        READ(IN,1245) VTEMP,(VDIR(I),I=1,NPER)                          C1X10280
        WRITE(IO,1246) VTEMP,(VDIR(I),I=1,NPER)                         C1X10290
      END IF                                                            C1X10300
      OLDWD=-1.                                                         C1X10310
720   IDAY=IDAY+1                                                       C1X10320
      DAY=IDAY                                                          C1X10330
      NHRS=0                                                            C1X10340
      IF (IOPT(5).EQ.1) GO TO 760                                       C1X10350
C        IF OPTION 5 EQUALS ZERO, INPUT MET DATA OFF DISK (UNIT 11)     C1X10360
      READ (11) JYR,IMO,DAY1,IKST,QU,QTEMP,DUMR,QTHETA,HLH              C1X10370
C        SET HOURLY CALM FLAGS FOR THIS DAY                             C1X10380
      IF (IOPT(26) .EQ. 1) THEN                                         C1X10390
      NCM=0                                                             C1X10400
      DO 725 JT=1,24                                                    C1X10410
      NC(JT)=0                                                          C1X10420
      TSTWD=ABS(OLDWD-DUMR(JT))                                         C1X10430
      IF (QU(JT) .LT. 1.01 .AND. TSTWD .LT. 0.01) THEN                  C1X10440
        NC(JT)=1                                                        C1X10450
        NCM=1                                                           C1X10460
      ENDIF                                                             C1X10470
      OLDWD=DUMR(JT)                                                    C1X10480
      IDUMR(JT)=DUMR(JT)+0.5                                            C1X10490
  725 CONTINUE                                                          C1X10500
      IF (NCM .EQ. 1) WRITE(IO,1095) IDAY,(NC(I),I=1,24)                C1X10510
      ENDIF                                                             C1X10520
      IF (JYR.NE.IDATE(1)) GO TO 730                                    C1X10530
      IF (DAY1.EQ.DAY) GO TO 740                                        C1X10540
C        DATE ON MET TAPE DOES NOT MATCH INTERNAL DATE                  C1X10550
730   WRITE (IO,1370) JYR,IDATE(2),IDATE(1),IDAY                        C1X10560
C     CALL WAUDIT
      STOP                                                              C1X10570
C        MODIFY WIND VECTOR BY 180 DEGREES. SINCE FLOW VECTORS WERE     C1X10580
C        OUTPUT FROM RAMMET. THIS CONVERTS BACK TO WIND DIRECTIONS.     C1X10590
740   IDATE(2)=DAY1                                                     C1X10600
      DO 750 IQ=1,24                                                    C1X10610
      IF (IKST(IQ).EQ.7) IKST(IQ)=6                                     C1X10620
      QTHETA(IQ)=QTHETA(IQ)+180.                                        C1X10630
      IF (QTHETA(IQ).GT.360.) QTHETA(IQ)=QTHETA(IQ)-360.                C1X10640
C        RURAL MIXING HEIGHTS ARE USED FOR THIS APPLICATION             C1X10650
750   QHL(IQ)=HLH(1,IQ)                                                 C1X10660
760   NB=IHSTRT                                                         C1X10670
      NE=NB+NAVG-1                                                      C1X10680
      IF (NB.GT.0) GO TO 770                                            C1X10690
      WRITE (IO,1380) IHSTRT                                            C1X10700
C     CALL WAUDIT
      STOP                                                              C1X10710
C        START LOOP FOR AVERAGING PERIOD.                               C1X10720
770   U=0.0                                                             C1X10730
      TEMP=0.0                                                          C1X10740
      DELN=0.0                                                          C1X10750
      DELM=0.0                                                          C1X10760
C        INITIALIZE FOR THE NUMBER OF CALMS IN AN AVERAGING PERIOD      C1X10770
      ICALMP=0                                                          C1X10780
      DO 780 I=1,7                                                      C1X10790
780   IFREQ(I)=0.0                                                      C1X10800
      DO 800 I=NB,NE                                                    C1X10810
      JHR=I                                                             C1X10820
      DAY2=IDATE(2)                                                     C1X10830
      IF (IOPT(5).EQ.0) GO TO 790                                       C1X10840
C***    SET OTHER MET PARAMETERS IF THE VALLEY OPTION IS                C1X10850
C***    SELECTED.                                                       C1X10860
      IF(IOPT(27) .EQ. 1) THEN                                          C1X10870
        JHR=1                                                           C1X10880
        JYR=IDATE(1)                                                    C1X10890
        DAY1=1.                                                         C1X10900
        IKST(JHR)=6                                                     C1X10910
        QU(JHR)=2.5                                                     C1X10920
        QTEMP(JHR)=VTEMP                                                C1X10930
        QTHETA(JHR)=VDIR(NP+1)                                          C1X10940
        QHL(JHR)=9999.                                                  C1X10950
      ELSE                                                              C1X10960
C                                                                       C1X10970
C***    READ CARD TYPE 14A IF IOPT(5) =1 AND IOPT(27) =0. (HOURLY MET)  C1X10980
C         (SEE DESCRIPTION, SECTION B).                                 C1X10990
C                                                                       C1X11000
      READ (IN, *  ) JYR,DAY1,JHR,IKST(JHR),QU(JHR),QTEMP(JHR),QTHETA(JHC1X11010
     1R),QHL(JHR)                                                       C1X11020
      END IF                                                            C1X11030
      IF (I.NE.NB) GO TO 790                                            C1X11040
C        REDEFINE START HOURS AND DATES AT FIRST HOUR OF EACH           C1X11050
C         AVERAGING PERIOD IF READING HOURLY MET DATA.                  C1X11060
      IDATE(1)=JYR                                                      C1X11070
      IHSTRT=JHR                                                        C1X11080
      ISTDAY=DAY1                                                       C1X11090
      IDATE(2)=ISTDAY                                                   C1X11100
      DAY2=IDATE(2)                                                     C1X11110
790   IF (IKST(JHR).EQ.7) IKST(JHR)=6                                   C1X11120
      IF (IOPT(10).EQ.1) GO TO 800                                      C1X11130
C                                                                       C1X11140
C->->->->SECTION P - CALCULATE AND WRITE MET SUMMARY INFO.              C1X11150
C                                                                       C1X11160
      IF (I.EQ.NB) WRITE (IO,1690) IDATE                                C1X11170
      TRAD=QTHETA(JHR)*0.01745329                                       C1X11180
      WRITE (IO,1700) JHR,QTHETA(JHR),QU(JHR),QHL(JHR),QTEMP(JHR),IKST(JC1X11190
     1HR)                                                               C1X11200
      SINT=SIN(TRAD)                                                    C1X11210
      COST=COS(TRAD)                                                    C1X11220
C        CALCULATE WIND COMPONENTS                                      C1X11230
      URES=QU(JHR)                                                      C1X11240
      UR=URES*SINT                                                      C1X11250
      VR=URES*COST                                                      C1X11260
      DELM=DELM+UR                                                      C1X11270
      DELN=DELN+VR                                                      C1X11280
      TEMP=TEMP+QTEMP(JHR)                                              C1X11290
      U=U+URES                                                          C1X11300
      KST=IKST(JHR)                                                     C1X11310
      IFREQ(KST)=IFREQ(KST)+1                                           C1X11320
C        END LOOP TO READ ALL MET DATA FOR AVERAGING PERIOD.            C1X11330
800   CONTINUE                                                          C1X11340
      IF (IOPT(10).EQ.1) GO TO 860                                      C1X11350
C        CALCULATE RESULTANT  WIND DIRECTION THETA                      C1X11360
      DELN=DELN/NAVG                                                    C1X11370
      DELM=DELM/NAVG                                                    C1X11380
      THETA=ANGARC(DELM,DELN)                                           C1X11390
C        CALCULATE AVERAGE AND RESULTANT SPEED AND PERSISTENCE.         C1X11400
      U=U/NAVG                                                          C1X11410
      TEMP=TEMP/NAVG                                                    C1X11420
      URES=SQRT(DELN*DELN+DELM*DELM)                                    C1X11430
      PERSIS=URES/U                                                     C1X11440
C        DETERMINE MODAL AND AVERAGE STABILITY                          C1X11450
      LSMAX=0                                                           C1X11460
      DO 810 I=1,7                                                      C1X11470
      LST=IFREQ(I)                                                      C1X11480
      IF (LST.LE.LSMAX) GO TO 810                                       C1X11490
      LSMAX=LST                                                         C1X11500
      LSTAB=I                                                           C1X11510
810   CONTINUE                                                          C1X11520
      IP1=LSTAB+1                                                       C1X11530
      KST=LSTAB                                                         C1X11540
      DO 820 I=IP1,7                                                    C1X11550
      IF (LSMAX.EQ.IFREQ(I)) GO TO 830                                  C1X11560
820   CONTINUE                                                          C1X11570
      GO TO 850                                                         C1X11580
C        IF TIE FOR MAX MODAL STABILITY CALCULATE AVERAGE STABILITY     C1X11590
830   KSUM=0                                                            C1X11600
      DO 840 J=1,7                                                      C1X11610
840   KSUM=KSUM+IFREQ(J)*J                                              C1X11620
      KST=FLOAT(KSUM)/FLOAT(NAVG)+0.5                                   C1X11630
C        PRINT RESULTANT MET DATA SUMMARY FOR AVERAGING PERIOD.         C1X11640
850   WRITE (IO,1710)                                                   C1X11650
      WRITE (IO,1720) THETA,URES,U,TEMP,PERSIS,KST                      C1X11660
C        REDEFINE NB AND NE IN CASE NON-CONSECUTIVE DAYS ARE BEING RUN  C1X11670
860   IF (IOPT(5).EQ.0) GO TO 870                                       C1X11680
      NB=IHSTRT                                                         C1X11690
      NE=IHSTRT+NAVG-1                                                  C1X11700
C                                                                       C1X11710
C->->->->SECTION Q - INITIALIZE FOR HOURLY LOOP.                        C1X11720
C                                                                       C1X11730
C        INITIALIZE SUMS FOR CONC AND PARTIAL CONC FOR AVG PERIOD.      C1X11740
870   DO 890 K=1,NRECEP                                                 C1X11750
      PCHI(K)=0.0                                                       C1X11760
      DO 880 I=1,26                                                     C1X11770
880   PSIGS(K,I)=0.0                                                    C1X11780
890   CONTINUE                                                          C1X11790
C        IF SAVING PARTIAL CONCENTRATIONS, WRITE INITIAL RECEPTOR INFO. C1X11800
      IF (IOPT(21).EQ.0) GO TO 900                                      C1X11810
      WRITE (10) NRECEP,NPT,(RREC(I),I=1,NRECEP),(SREC(I),I=1,NRECEP)   C1X11820
C                                                                       C1X11830
C->->->->SECTION R - BEGIN HOURLY LOOP.                                 C1X11840
C                                                                       C1X11850
900   DO 1020 ILH=NB,NE                                                 C1X11860
      LH=ILH                                                            C1X11870
      IF (LH.LE.24) GO TO 910                                           C1X11880
      LH=MOD(ILH,24)                                                    C1X11890
      IF (LH.EQ.1) IDATE(2)=DAY1                                        C1X11900
C        INITIALIZE SUMS FOR CONC AND PARTIAL CONC FOR HOURLY PERIODS.  C1X11910
910   DO 930 K=1,NRECEP                                                 C1X11920
      PHCHI(K)=0.0                                                      C1X11930
      DO 920 I=1,26                                                     C1X11940
920   PHSIGS(K,I)=0.0                                                   C1X11950
930   CONTINUE                                                          C1X11960
C        SET MET CONDITIONS FOR THIS HOUR                               C1X11970
      THETA=QTHETA(LH)                                                  C1X11980
      U=QU(LH)                                                          C1X11990
      HL=QHL(LH)                                                        C1X12000
      TEMP=QTEMP(LH)                                                    C1X12010
      KST=IKST(LH)                                                      C1X12020
      TRAD=THETA*0.01745329                                             C1X12030
      SINT=SIN(TRAD)                                                    C1X12040
      COST=COS(TRAD)                                                    C1X12050
      CTER=CONTER(KST)                                                  C1X12060
C     CHECK FOR CALM PERIODS AND IF ONE OCCURS SET THE CONCENTRATIONS   C1X12070
C      EQUAL TO ZERO AT ALL RECEPTORS IN SUBROUTINE PTR AND INCREMENT A C1X12080
C     CALM COUNTER AND FLAGS FOR HOURLY AND AVERAGING PERIODS.          C1X12090
        ICLM=0                                                          C1X12100
      IF (IOPT(26) .EQ. 1 .AND. QU(LH) .LT. 1.009 .AND. ITMIN1 .EQ. IDUMC1X12110
     1R(LH)) THEN                                                       C1X12120
        ICLM=1                                                          C1X12130
        ICALM=ICALM+1                                                   C1X12140
      ICALMP=ICALMP+1                                                   C1X12150
      ENDIF                                                             C1X12160
C        IF OPTION 6 IS 1, READ HOURLY EMISSIONS.                       C1X12170
      IF (IOPT(6).EQ.0) GO TO 940                                       C1X12180
      IDCK=IDATE(1)*100000+IDATE(2)*100+LH                              C1X12190
      READ (15) IDATP,(SOURCE(IPOL,I),I=1,NPT)                          C1X12200
C        CHECK DATE                                                     C1X12210
      IF (IDCK.EQ.IDATP) GO TO 940                                      C1X12220
      WRITE (IO,1390) IDCK,IDATP                                        C1X12230
C     CALL WAUDIT
      STOP                                                              C1X12240
C        CALCULATE POINT SOURCE CONTRIBUTIONS                           C1X12250
940   CALL PTR                                                          C1X12260
      IF (IOPT(22).EQ.0) GO TO 950                                      C1X12270
C        WRITE HOURLY CONCENTRATIONS TO TAPE                            C1X12280
      WRITE (12) IDATE(2),LH,(PHCHI(I),I=1,NRECEP)                      C1X12290
C                                                                       C1X12300
C->->->->SECTION S - CALCULATE AND STORE FOR HIGH-FIVE TABLE.           C1X12310
C                                                                       C1X12320
950   NHR=NHR+1                                                         C1X12330
C        IF OPTION 19 IS 1, DELETE COMPUTATIONS FOR AVG CONC.           C1X12340
C         FOR LENGTH OF RECORD AND HIGH-FIVE TABLE.                     C1X12350
      IF ((IOPT(19).EQ.1 .AND. IOPT(26) .NE. 1)) GO TO 1010             C1X12360
C        CUMULATE CONCENTRATIONS FOR AVG TIMES AND LENGTH OF RECORD.    C1X12370
      DO 970 K=1,NRECEP                                                 C1X12380
      DO 960 L=1,NAVT                                                   C1X12390
960   CONC(K,L)=CONC(K,L)+PHCHI(K)                                      C1X12400
970   SUM(K)=SUM(K)+PHCHI(K)                                            C1X12410
C          STORE DATE FOR WHICH CONCS. HAVE BEEN CALCULATED.            C1X12420
      JDAY=IDATE(2)                                                     C1X12430
C***    IF THE VALLEY OPTION IS SELECTED, KEEP TRACK OF                 C1X12440
C***    THE WIND DIRECTION FOR EACH PERIOD FOR OUTPUT                   C1X12450
C***    TO THE NEAREST 10TH OF A DEGREE.                                C1X12460
      IF(IOPT(27) .EQ. 1) JDAY=THETA*10.                                C1X12470
C        SUBROUTINE RANK IS CALLED WHENEVER A COUNTER                   C1X12480
C        INDICATES THAT ENOUGH END TO END HOURLY CONCENTRATIONS         C1X12490
C        NP3, NP8, NP24, NPX ARE USED AS COUNTERS FOR EACH              C1X12500
C        AVG TIME AND ARE ZEROED AFTER EACH CALL TO RANK.               C1X12510
C                                                                       C1X12520
C       THE REMAINDER OF SECTION S AND SUBROUTINE RANK HAVE BEEN        C1X12530
C       MODIFIED TO ACCOUNT FOR CALM HOURS IF IOPT(26) HAS BEEN         C1X12540
C       SELECTED.                                                       C1X12550
      CALL RANK(1)                                                      C1X12560
C***    RANKINGS ARE DONE FOR ONLY ONE AVERAGING PERIOD IF              C1X12570
C***    THE VALLEY OPTION IS SELECTED.                                  C1X12580
      IF(IOPT(27) .EQ. 1) GO TO 1010                                    C1X12590
      NP3=NP3+1                                                         C1X12600
      IF (ICLM .EQ. 1) ICFL3=1                                          C1X12610
      IF (NP3 .NE. 3) GO TO 980                                         C1X12620
C        CONCENTRATION FOR 3 HOUR AVERAGING PERIOD                      C1X12630
      DO 972 LQ=1,NRECEP                                                C1X12640
  972 CONC(LQ,2)=CONC(LQ,2)/3.0                                         C1X12650
      LL2=2                                                             C1X12660
C        IF A CALM OCCURS, SET THE VARIABLE PASSED TO RANK (LL2) TO 22  C1X12670
      IF (ICFL3 .EQ. 1) LL2=22                                          C1X12680
      CALL RANK(LL2)                                                    C1X12690
      NP3=0                                                             C1X12700
      ICFL3=0                                                           C1X12710
  980 NP8=NP8+1                                                         C1X12720
      IDIV8=IDIV8+1                                                     C1X12730
C        IF A CALM HOUR IS ENCOUNTERED, SUBTRACT 1 FROM THE 8 HOUR      C1X12740
C        DIVISOR AND SET THE 8 HOUR CALM FLAG.                          C1X12750
      IF (ICLM .EQ. 1) THEN                                             C1X12760
           IDIV8=IDIV8-1                                                C1X12770
           ICFL8=1                                                      C1X12780
      ENDIF                                                             C1X12790
      IF (NP8 .NE. 8) GO TO 990                                         C1X12800
C        CALCULATE CONCENTRATIONS FOR 8 HOUR AVERAGING PERIODS          C1X12810
      IF (IDIV8 .LT. 6) IDIV8=6                                         C1X12820
       DIV8=IDIV8                                                       C1X12830
      DO 985 LQ=1,NRECEP                                                C1X12840
  985 CONC(LQ,3)=CONC(LQ,3)/DIV8                                        C1X12850
      LL3=3                                                             C1X12860
C        IF A CALM OCCURS, SET THE VARIABLE PASSED TO RANK (LL3) TO 33  C1X12870
      IF (ICFL8 .EQ. 1) LL3=33                                          C1X12880
      CALL RANK(LL3)                                                    C1X12890
      NP8=0                                                             C1X12900
      IDIV8=0                                                           C1X12910
      ICFL8=0                                                           C1X12920
  990 NP24=NP24+1                                                       C1X12930
      IDIV24=IDIV24+1                                                   C1X12940
C        IF A CALM HOUR IS ENCOUNTERED, SUBTRACT 1 FROM THE 24 HOUR     C1X12950
C        DIVISOR AND SET THE 24 HOUR CALM FLAG TO 1.                    C1X12960
      IF (ICLM .EQ. 1) THEN                                             C1X12970
           IDIV24=IDIV24-1                                              C1X12980
           ICFL24=1                                                     C1X12990
      ENDIF                                                             C1X13000
      IF (NP24 .NE. 24) GO TO 997                                       C1X13010
C        CALCULATE CONCENTRATIONS FOR 24 HOUR AVERAGING PERIODS         C1X13020
      IF (IDIV24 .LT. 18) IDIV24=18                                     C1X13030
      DIV24=IDIV24                                                      C1X13040
      DO 995 LQ=1,NRECEP                                                C1X13050
  995 CONC(LQ,4)=CONC(LQ,4)/DIV24                                       C1X13060
      LL4=4                                                             C1X13070
C        IF A CALM OCCURS,SET THE VARIABLE PASSED TO RANK (LL4) TO 44   C1X13080
      IF (ICFL24 .EQ. 1) LL4=44                                         C1X13090
      CALL RANK(LL4)                                                    C1X13100
      NP24=0                                                            C1X13110
      IDIV24=0                                                          C1X13120
      ICFL24=0                                                          C1X13130
C        SET ITMIN1 TO THE LAST WIND DIRECTION                          C1X13140
  997 ITMIN1=IDUMR(LH)                                                  C1X13150
1000  IF (NAVT.EQ.4) GO TO 1010                                         C1X13160
      NPX=NPX+1                                                         C1X13170
C        INCREMENT THE ARBITRARY AVERAGING PERIOD AND IF A CALM HOUR    C1X13180
C        OCCURS SUBTRACT 1 FROM THE DIVISOR AND SET THE CALM FLAG TO 1  C1X13190
      IDIVX=IDIVX+1                                                     C1X13200
      IF (ICLM .EQ. 1) THEN                                             C1X13210
         IDIVX=IDIVX-1                                                  C1X13220
         ICFLX=1                                                        C1X13230
      ENDIF                                                             C1X13240
      IF (NPX.NE.NAV5) GO TO 1010                                       C1X13250
C        ASSURE THAT THE DIVISOR OF THE ARBITRARY AVERAGING PERIOD IS   C1X13260
C        GREATER THAN OR EQUAL TO 75% OF THE AVERAGING PERIOD.          C1X13270
      NC5=NAV5*0.75+0.99                                                C1X13280
      IF (IDIVX .LT. NC5) IDIVX=NC5                                     C1X13290
      DIVX=IDIVX                                                        C1X13300
      DO 1005 LQ=1,NRECEP                                               C1X13310
 1005 CONC(LQ,5)=CONC(LQ,5)/DIVX                                        C1X13320
      LL5=5                                                             C1X13330
C        IF A CALM OCCURS, SET THE VARIABLE PASSED TO RANK (LL5) TO 55  C1X13340
      IF (ICFLX .EQ. 1) LL5=55                                          C1X13350
      CALL RANK (LL5)                                                   C1X13360
      NPX=0                                                             C1X13370
      IDIVX=0                                                           C1X13380
      ICFLX=0                                                           C1X13390
C                                                                       C1X13400
C->->->->SECTION T - END HOURLY, AVERAGING TIME, AND DAILY LOOPS.       C1X13410
C                                                                       C1X13420
1010  IF (IOPT(11).EQ.1.AND.IOPT(14).EQ.1) GO TO 1020                   C1X13430
C        IF BOTH OPTIONS 11 AND 14 CALL FOR OUTPUT DELETIONS,           C1X13440
C         SKIP HOURLY PRINTOUT.                                         C1X13450
C***    IF VALLEY OPTION IS CHOSEN, KEEP TRACK OF THE                   C1X13460
C***    PERIOD FOR OUTPUT IN OUTHR.                                     C1X13470
      IF(IOPT(27) .EQ. 1) LH=NP+1                                       C1X13480
      CALL OUTHR                                                        C1X13490
1020  CONTINUE                                                          C1X13500
C                                                                       C1X13510
C        END OF HOURLY LOOP                                             C1X13520
C                                                                       C1X13530
      IF (NE.GT.24) IDATE(2)=ISTDAY                                     C1X13540
C        OUTPUT FINAL RESULTS                                           C1X13550
      CALL OUTAVG                                                       C1X13560
      NP=NP+1                                                           C1X13570
      NHRS=NHRS+NAVG                                                    C1X13580
C        NEXT STATEMENT IS BRANCH FOR END OF RUN.                       C1X13590
      IF (NP.GE.NPER) GO TO 1050                                        C1X13600
      IF (NHRS.LT.24) GO TO 1030                                        C1X13610
      IF (IOPT(20).EQ.0) GO TO 720                                      C1X13620
C        NEXT STATEMENT CHECKS FOR END OF SEGMENTED RUN.                C1X13630
      IF (IDAY.GE.LDRUN) GO TO 1040                                     C1X13640
      GO TO 720                                                         C1X13650
C                                                                       C1X13660
C          END OF LOOP FOR CALENDAR DAYS                                C1X13670
C                                                                       C1X13680
1030  NB=NB+NAVG                                                        C1X13690
      NE=NE+NAVG                                                        C1X13700
      IF (NB.LE.24) GO TO 770                                           C1X13710
      NB=MOD(NB,24)                                                     C1X13720
      NE=NB+NAVG-1                                                      C1X13730
      GO TO 770                                                         C1X13740
C                                                                       C1X13750
C        END OF LOOP FOR AVERAGING PERIOD.                              C1X13760
C                                                                       C1X13770
C        IF SEGMENTED RUN, TEMPORARILY STORE                            C1X13780
C         HIGH-FIVE INFO ON UNIT 14 FILE.                               C1X13790
 1040 WRITE (14) IDAY,SUM,NHR,DAY1A,HR1,HMAXA,ICALM,NDAY,IHR            C1X13800
      WRITE (IO,1730) IDAY                                              C1X13810
      GO TO 1140                                                        C1X13820
1050  IF (IOPT(19).EQ.1) GO TO 1140                                     C1X13830
C                                                                       C1X13840
C->->->->SECTION U - WRITE AVERAGE CONC. AND HIGH-FIVE TABLES.          C1X13850
C                                                                       C1X13860
C        IF OPTION 19 = 0, WRITE AVERAGE CONCENTRATION.                 C1X13870
C         FOR LENGTH OF RECORD AND HIGH-FIVE TABLE.                     C1X13880
      DO 1060 J=1,NRECEP                                                C1X13890
      STAR(1,J)=BLNK                                                    C1X13900
      STAR(2,J)=BLNK                                                    C1X13910
1060  CONTINUE                                                          C1X13920
C***    IF THE VALLEY OPTION IS SELECTED, BRANCH                        C1X13930
C***    AROUND AVERAGES FOR ENTIRE PERIOD.                              C1X13940
      IF(IOPT(27) .EQ. 1) GO TO 1085                                    C1X13950
      WRITE (IO,1400) LINE1,LINE2,LINE3                                 C1X13960
      HR2=NE                                                            C1X13970
C        FOR IOPT(26) CALCULATE AND REPORT THE TOTAL NUMBER OF CALM     C1X13980
C        HOURS ENCOUNTERED FOR THE ENTIRE AVERAGING PERIOD (YEAR).      C1X13990
      IF (IOPT(26) .EQ. 1) THEN                                         C1X14000
      IF (NHR .LE. 24) THEN                                             C1X14010
         NHR=MAX((NHR-ICALM),INT(.75*NHR+.99))                          IBM14020
      ELSE                                                              C1X14030
         NHR=NHR-ICALM                                                  C1X14040
      ENDIF                                                             C1X14050
         WRITE(6,1061) ICALM                                            C1X14060
      ENDIF                                                             C1X14070
      SUM(1)=SUM(1)/NHR                                                 C1X14080
      HIMAX=SUM(1)                                                      C1X14090
      KMX=1                                                             C1X14100
C        INITIALIZE PERIODIC CONC TO BEGIN RANKING FOR PERIODIC MAX     C1X14110
      DO 1070 K=2,NRECEP                                                C1X14120
      SUM(K)=SUM(K)/NHR                                                 C1X14130
      IF (SUM(K).LE.HIMAX) GO TO 1070                                   C1X14140
      KMX=K                                                             C1X14150
      HIMAX=SUM(K)                                                      C1X14160
1070  CONTINUE                                                          C1X14170
      STAR(1,KMX)=STR                                                   C1X14180
C        FIND HIGHEST AVERAGE CONC. AMONG RECEPTORS.                    C1X14190
      WRITE (IO,1740) DAY1A,HR1,DAY2,HR2                                C1X14200
      DO 1080 K=1,NRECEP                                                C1X14210
1080  WRITE (IO,1750) K,(RNAME(J,K),J=1,2),RREC(K),SREC(K),ZR(K),ELR(K),C1X14220
     1STAR(1,K),SUM(K)                                                  C1X14230
      STAR(1,KMX)=BLNK                                                  C1X14240
 1085 CONTINUE                                                          C1X14250
C        LOOP TO WRITE HIGH-FIVE TABLE FOR 4 OR 5 AVG TIMES.            C1X14260
      DO 1130 L=1,NAVT                                                  C1X14270
C        ASTERISKS DEPICT RECEPTORS WITH HIGHEST AND                    C1X14280
C         SECOND HIGHEST CONCENTRATIONS.                                C1X14290
      K1=1                                                              C1X14300
      K2=1                                                              C1X14310
      HI1=HMAXA(1,1,L)                                                  C1X14320
      HI2=HMAXA(2,1,L)                                                  C1X14330
      DO 1100 K=2,NRECEP                                                C1X14340
      IF (HMAXA(1,K,L).LE.HI1) GO TO 1090                               C1X14350
      HI1=HMAXA(1,K,L)                                                  C1X14360
      K1=K                                                              C1X14370
1090  IF (HMAXA(2,K,L).LE.HI2) GO TO 1100                               C1X14380
      HI2=HMAXA(2,K,L)                                                  C1X14390
      K2=K                                                              C1X14400
1100  CONTINUE                                                          C1X14410
      STAR(1,K1)=STR                                                    C1X14420
      STAR(2,K2)=STR                                                    C1X14430
C        IF CALMS ARE NOT ENCOUNTERED OR THE OPTION IS NOT CHOSEN, THE  C1X14440
C        ORIGINAL TABLE TITLES ARE PRINTED, OTHERWISE A LINE IS ADDED   C1X14450
C        DESCRIBING HOW CALM HOURS ARE DENOTED.                         C1X14460
      IF ((IOPT(26) .EQ. 1 .AND. L .EQ. 1) .OR. (IOPT(26) .NE. 1)) THEN C1X14470
        IF(IOPT(27) .EQ. 1) THEN                                        C1X14480
          WRITE(IO,1759) TITLE(IP),(I,I=1,5)                            C1X14490
        ELSE                                                            C1X14500
      WRITE (IO,1760) NTIME(L),TITLE(IP),(I,I=1,5)                      C1X14510
        END IF                                                          C1X14520
      ELSE                                                              C1X14530
      WRITE(IO,1761) NTIME(L),TITLE(IP),(I,I=1,5)                       C1X14540
      ENDIF                                                             C1X14550
C        THE FOLLOWING LINE CONTAINING ATIME WAS DELETED BECAUSE THE    C1X14560
C        AVERAGE CONCENTRATIONS WERE CALCULATED IN SECTION S.           C1X14570
C     DUM=ATIME(L)                                                      C1X14580
      DO 1120 K=1,NRECEP                                                C1X14590
C***      IF VALLEY OPTION IS SELECTED, STORE WIND DIRECTION            C1X14600
C***      FOR OUTPUT.                                                   C1X14610
      IF(IOPT(27) .EQ. 1) THEN                                          C1X14620
       DO 1111 J=1,5                                                    C1X14630
        RDAY(J,K)=NDAY(J,K,L)/10.                                       C1X14640
 1111  CONTINUE                                                         C1X14650
      END IF                                                            C1X14660
C        IF A CALM HOUR OCCURS, LABEL THE PERIOD WITH A 'C' AND         C1X14670
C       RESTORE VALUE OF IHR                                            C1X14680
      IF(IOPT(26) .NE. 1) GO TO 1113                                    C1X14690
      DO 1112 J=1,5                                                     C1X14700
      CF(J)=BLNK                                                        C1X14710
      IF (IHR(J,K,L) .GT. 24) THEN                                      C1X14720
         IHR(J,K,L)=IHR(J,K,L)-100                                      C1X14730
         CF(J)=C                                                        C1X14740
      ENDIF                                                             C1X14750
 1112 CONTINUE                                                          C1X14760
 1113 IF (IOPT(27) .EQ. 1) THEN                                         C1X14770
      WRITE(IO,1769) K,RREC(K),SREC(K),(STAR(J,K),HMAXA(J,K,L),CF(J),   C1X14780
     1 RDAY(J,K),J=1,2),(HMAXA(J,K,L),CF(J),RDAY(J,K),                  C1X14790
     2J=3,5)                                                            C1X14800
      ELSE                                                              C1X14810
      WRITE(IO,1770) K,RREC(K),SREC(K),(STAR(J,K),HMAXA(J,K,L),CF(J),   C1X14820
     1NDAY(J,K,L),IHR(J,K,L),J=1,2),(HMAXA(J,K,L),CF(J),NDAY(J,K,L),    C1X14830
     2IHR(J,K,L),J=3,5)                                                 C1X14840
      END IF                                                            C1X14850
1120  CONTINUE                                                          C1X14860
C        INITIALIZE ASTERISK STORAGE TO BLANKS.                         C1X14870
      STAR(1,K1)=BLNK                                                   C1X14880
      STAR(2,K2)=BLNK                                                   C1X14890
1130  CONTINUE                                                          C1X14900
C                                                                       C1X14910
C->->->->SECTION V - CLOSE OUT FILES.                                   C1X14920
C                                                                       C1X14930
1140  IF (IOPT(21).EQ.0) GO TO 1150                                     C1X14940
      END FILE 10                                                       C1X14950
C     END FILE 10                                                       IBM14960
1150  IF (IOPT(22).EQ.0) GO TO 1160                                     C1X14970
      END FILE 12                                                       C1X14980
C     END FILE 12                                                       IBM14990
1160  IF (IOPT(23).EQ.0) GO TO 1170                                     C1X15000
      END FILE 13                                                       C1X15010
C     END FILE 13                                                       IBM15020
C1170  CALL WAUDIT
 1170 CONTINUE
      STOP                                                              C1X15030
C                                                                       C1X15040
C->->->->SECTION X - OUTLINE OF PROGRAM SECTIONS                        C1X15050
C                                                                       C1X15060
C        SECTION A - GENERAL REMARKS                                    C1X15070
C        SECTION B - DATA INPUT LISTS.                                  C1X15080
C        SECTION C - COMMON, DIMENSION, AND DATA STATEMENTS.            C1X15090
C        SECTION D - FLOW DIAGRAM.                                      C1X15100
C        SECTION E - RUN SET-UP AND READ FIRST 6 INPUT CARDS.           C1X15110
C        SECTION F - INPUT AND PROCESS EMISSION INFORMATION.            C1X15120
C        SECTION G - RANK SIGNIFICANT SOURCES.                          C1X15130
C        SECTION H - EMISSIONS WITH HEIGHT TABLE.                       C1X15140
C        SECTION I - EXECUTE FOR INPUT OF SIGNIFICANT SOURCE NUMBERS.   C1X15150
C        SECTION J - CHECK MET. DATA IF FROM FILE OF ONE YEARS'S DATA.  C1X15160
C        SECTION K - GENERATE POLAR COORDINATE RECEPTORS.               C1X15170
C        SECTION L - READ POLAR COORDINATE ELEVATIONS.                  C1X15180
C        SECTION M - READ AND PROCESS RECEPTOR INFORMATION.             C1X15190
C        SECTION N - POSITION FILES AS REQUIRED.                        C1X15200
C        SECTION O - START LOOPS FOR DAY AND AVERAGING TIME; READ       C1X15210
C                       MET. DATA.                                      C1X15220
C        SECTION P - CALCULATE AND WRITE MET. SUMMARY INFORMATION.      C1X15230
C        SECTION Q - INITIALIZE FOR HOURLY LOOP.                        C1X15240
C        SECTION R - BEGIN HOURLY LOOP.                                 C1X15250
C        SECTION S - CALCULATE AND STORE FOR HIGH-FIVE TABLE.           C1X15260
C        SECTION T - END HOURLY, AVERAGING TIME, AND DAILY LOOPS.       C1X15270
C        SECTION U - WRITE AVERAGE CONC. AND HIGH-FIVE TABLES.          C1X15280
C        SECTION V - CLOSE OUT FILES.                                   C1X15290
C        SECTION W - FORMAT STATEMENTS.                                 C1X15300
C        SECTION X - OUTLINE OF PROGRAM SECTIONS.                       C1X15310
C        SECTION Y - INPUT AND OUTPUT FILE DESCRIPTIONS.                C1X15320
C        SECTION Z - INDEX AND GLOSSARY.                                C1X15330
C                                                                       C1X15340
C                                                                       C1X15350
C->->->-> SECTION Y -  INPUT AND OUTPUT FILE DESCRIPTIONS.              C1X15360
C                                                                       C1X15370
C***     INPUT AND OUTPUT FILE DESCRIPTIONS.                            C1X15380
C                                                                       C1X15390
C***  INPUT FILE (UNIT 11) METEOROLOGICAL DATA (USED IF IOPT(5)=0)      C1X15400
C                                                                       C1X15410
C      RECORD 1                                                         C1X15420
C                                                                       C1X15430
C        ID          SFC STATION IDENTIFIER, 5 DIGITS                   C1X15440
C        IYEAR       YEAR OF SURFACE DATA, 2 DIGITS                     C1X15450
C        IDM         MIX HT STATION IDENTIFIER, 5 DIGITS                C1X15460
C        IYR         YEAR OF MIX HT DATA, 2 DIGITS                      C1X15470
C                                                                       C1X15480
C      RECORD TYPE 2 (ONE FOR EACH DAY OF YEAR)                         C1X15490
C                                                                       C1X15500
C        JYR         YEAR                                               C1X15510
C        IMO         MONTH                                              C1X15520
C        DAY1        JULIAN DAY                                         C1X15530
C        IKST(24)    STABILITY CLASS                                    C1X15540
C        QU(24)      WIND SPEED, METERS PER SECOND                      C1X15550
C        QTEMP(24)   AMBIENT AIR TEMPERATURE, KELVIN                    C1X15560
C        DUMR(24)    FLOW VECTOR TO 10 DEG, DEGREES AZIMUTH             C1X15570
C        QTHETA(24)  RANDOMIZED FLOW VECTOR, DEGREES AZIMUTH            C1X15580
C        HLH(2,24)   MIXING HEIGHT, METERS                              C1X15590
C                                                                       C1X15600
C***  INPUT FILE(UNIT 15) EMISSION DATA (USED IF IOPT(6)=1)             C1X15610
C                                                                       C1X15620
C      RECORD TYPE 1 (ONE FOR EACH HOUR OF SIMULATION)                  C1X15630
C                                                                       C1X15640
C        IDATP       DATE-TIME INDICATOR CONSISTING OF YEAR, JULIAN DAY,C1X15650
C                     AND HOUR: YYDDDHH.                                C1X15660
C        SOURCE(IPOL,I),I=1,NPT  EMISSION RATE FOR THE POLLUTANT IPOL   C1X15670
C                                 FOR EACH SOURCE, GRAMS PER SECOND.    C1X15680
C                                                                       C1X15690
C***  OUTPUT PUNCHED CARDS (UNIT 1) AVERAGE CONCENTRATIONS (PUNCHED IF  C1X15700
C                                    IOPT(24)=1)                        C1X15710
C                                                                       C1X15720
C      CARD TYPE 1 (ONE FOR EACH RECEPTOR FOR EACH AVERAGING TIME)      C1X15730
C                                                                       C1X15740
C        CC:1-4      WORD'CNTL' PUNCHED                                 C1X15750
C        CC:5        BLANK                                              C1X15760
C        CC:6-15     RREC  EAST COORDINATE OF RECEPTOR, USER UNITS      C1X15770
C        CC:16-25    SREC  NORTH COORDINATE OF RECEPTOR, USER UNITS     C1X15780
C        CC:26-35    GWU   CONCENTRATION FOR AVERAGING TIME, MICROG/M**3C1X15790
C        CC:36-55    BLANK                                              C1X15800
C        CC:56-59    K     RECEPTOR NUMBER                              C1X15810
C        CC:60-69    ZR    RECEPTOR HEIGHT ABOVE GROUND, METERS         C1X15820
C        CC:70-79    ELR  RECEPTOR GROUND-LEVEL ELEVATION, USER HT UNITSC1X15830
C                                                                       C1X15840
C***  OUTPUT FILE (UNIT 10) PARTIAL CONCENTRATIONS (USED IF IOPT(21)=1) C1X15850
C                                                                       C1X15860
C     RECORD TYPE 1                                                     C1X15870
C                                                                       C1X15880
C        NPER        NUMBER OF PERIODS                                  C1X15890
C        NAVG        NUMBER OF HOURS IN AVERAGING PERIOD.               C1X15900
C        LINE1(14)   80 ALPHANUMERIC CHARACTERS FOR TITLE.              C1X15910
C        LINE2(14)   80 ALPHANUMERIC CHARACTERS FOR TITLE.              C1X15920
C        LINE3(14)   80 ALPHANUMERIC CHARACTERS FOR TITLE.              C1X15930
C                                                                       C1X15940
C     RECORD TYPE 2 (FROM COMPLEX I ) (ONE FOR EACH AVERAGING PERIOD)   C1X15950
C                                                                       C1X15960
C        NRECEP      NUMBER OF RECEPTORS                                C1X15970
C        NPT         NUMBER OF SOURCES                                  C1X15980
C        RREC(I),I=1,NRECEP  EAST COORDINATE OF RECEPTOR, USER UNITS    C1X15990
C        SREC(I),I=1,NRECEP  NORTH COORDINATE OF RECEPTOR, USER UNITS   C1X16000
C                                                                       C1X16010
C      RECORD TYPE 3 (ONE FOR EACH RECEPTOR FOR EACH SIMULATED HOUR,    C1X16020
C                      FROM PTR)                                        C1X16030
C                                                                       C1X16040
C        IDATE       YEAR AND JULIAN DAY                                C1X16050
C        LH          HOUR                                               C1X16060
C        K           RECEPTOR NUMBER                                    C1X16070
C        PARTC(J),J=1,NPT  CONCENTRATION AT RECEPTOR K FROM SOURCE J,   C1X16080
C                           G/M**3.                                     C1X16090
C                                                                       C1X16100
C***  OUTPUT FILE (UNIT 12) HOURLY CONCENTRATIONS (USED IF IOPT(22)=1)  C1X16110
C                                                                       C1X16120
C     RECORD 1                                                          C1X16130
C                                                                       C1X16140
C        NPER        NUMBER OF PERIODS                                  C1X16150
C        NAVG        NUMBER OF HOURS IN AVERAGING PERIOD.               C1X16160
C        LINE1(14)   80 ALPHANUMERIC CHARACTERS FOR TITLE.              C1X16170
C        LINE2(14)   80 ALPHANUMERIC CHARACTERS FOR TITLE.              C1X16180
C        LINE3(14)   80 ALPHANUMERIC CHARACTERS FOR TITLE.              C1X16190
C                                                                       C1X16200
C      RECORD 2                                                         C1X16210
C                                                                       C1X16220
C        NRECEP      NUMBER OF RECEPTORS.                               C1X16230
C        RREC(I),I=1,NRECEP  EAST COORDINATE OF RECEPTOR, USER UNITS    C1X16240
C        SREC(I),I=1,NRECEP  NORTH COORDINATE OF RECEPTOR, USER UNITS   C1X16250
C                                                                       C1X16260
C      RECORD TYPE 3 (ONE FOR EACH SIMULATED HOUR)                      C1X16270
C                                                                       C1X16280
C        IDATE(2)    JULIAN DAY                                         C1X16290
C        LH          HOUR                                               C1X16300
C        PHCHI(I),I=1,NRECEP  HOURLY CONCENTRATION FOR EACH RECEPTOR,   C1X16310
C                              G/M**3.                                  C1X16320
C                                                                       C1X16330
C***  OUTPUT FILE (UNIT 13) AVERAGING-PERIOD CONCENTRATIONS (USED IF    C1X16340
C                            IOPT(23)=1)                                C1X16350
C                                                                       C1X16360
C      RECORD 1                                                         C1X16370
C                                                                       C1X16380
C        NPER        NUMBER OF PERIODS                                  C1X16390
C        NAVG        NUMBER OF HOURS IN AVERAGING PERIOD.               C1X16400
C        LINE1(14)   80 ALPHANUMERIC CHARACTERS FOR TITLE.              C1X16410
C        LINE2(14)   80 ALPHANUMERIC CHARACTERS FOR TITLE.              C1X16420
C        LINE3(14)   80 ALPHANUMERIC CHARACTERS FOR TITLE.              C1X16430
C                                                                       C1X16440
C      RECORD 2                                                         C1X16450
C                                                                       C1X16460
C        NRECEP      NUMBER OF RECEPTORS.                               C1X16470
C        RREC(I),I=1,NRECEP  EAST COORDINATE OF RECEPTOR, USER UNITS    C1X16480
C        SREC(I),I=1,NRECEP  NORTH COORDINATE OF RECEPTOR, USER UNITS   C1X16490
C                                                                       C1X16500
C      RECORD TYPE 3 (ONE FOR EACH SIMULATED AVERAGING PERIOD)          C1X16510
C                                                                       C1X16520
C        IDATE(2)    JULIAN DAY                                         C1X16530
C        NB          ENDING HOUR OF PERIOD                              C1X16540
C        PCHI(K),K=1,NRECEP  AVERAGING PERIOD CONCENTRATION FOR EACH    C1X16550
C                             RECEPTOR, G/M**3.                         C1X16560
C                                                                       C1X16570
C***  TEMPORARY FILE (UNIT 14) VALUES FOR HIGH-FIVE TABLES (USED IF     C1X16580
C                                IOPT(20)=1)                            C1X16590
C                                                                       C1X16600
C      ONLY RECORD                                                      C1X16610
C                                                                       C1X16620
C        NDAY(ON WRITE)    NUMBER OF DAYS PROCESSED                     C1X16630
C        IDAYS(ON READ     NUMBER OF DAYS PREVIOUSLY PROCESSED          C1X16640
C        SUM(180)          CUMULATION OF LONG-TERM CONCENTRATION,(G/M**3C1X16650
C        NHR               NUMBER OF HOURS PROCESSED                    C1X16660
C        DAY1A             JULIAN DAY OF START OF LENGTH OF RECORD.     C1X16670
C        HR1               START HOUR OF LENGTH OF RECORD               C1X16680
C        HMAXA(3,5,180,5)  HIGHEST FIVE CONCENTRATIONS (G/M**3), AND    C1X16690
C                           ASSOCIATED DAY AND HOUR, FOR EACH RECEPTOR, C1X16700
C                           FOR FIVE DIFFERENT AVERAGING TIMES.         C1X16710
C                                                                       C1X16720
C->->->->SECTION W - FORMAT STATEMENTS.                                 C1X16730
C                                                                       C1X16740
C        INPUT FORMATS                                                  C1X16750
C                                                                       C1X16760
1180  FORMAT (20A4/20A4/20A4)                                           C1X16770
1200  FORMAT (A4)                                                       C1X16780
1210  FORMAT (3A4,8F8.2,F4.0)                                           C1X16790
1220  FORMAT (26I3)                                                     C1X16800
1230  FORMAT (I2,8X,5F10.0)                                             C1X16810
1240  FORMAT (2A4,2F10.3,2F10.0)                                        C1X16820
1245  FORMAT(F10.0,/,(16F5.0))                                          C1X16830
1246  FORMAT('1',T20,'VALLEY METEOROLOGICAL INPUT DATA',///,            C1X16840
     +  ' PRESET BY MODEL:',/,T10,'MIXING HEIGHT (M) = 9999',           C1X16850
     +  /,T10,'STABILITY = 6',/,T10,'WIND SPEED (M/SEC) = 2.5',//,      C1X16860
     +  ' INPUT BY USER:',/,T10,'TEMPERATURE (K) = ',F6.1,              C1X16870
     +  /,T10,'WIND DIRECTIONS (DEG) = ',(T34,16F6.1))                  C1X16880
C                                                                       C1X16890
C        ERROR STATEMENT FORMATS                                        C1X16900
C                                                                       C1X16910
1247  FORMAT(' **ERROR** CALM PROCESSING OPTION NOT APPROPRIATE WITH METC1X16920
     1 DATA ON CARDS')                                                  C1X16930
1250  FORMAT (1X,' NSIGP (THE NO. OF SIGNF POINT SOURCES) WAS FOUND',' TC1X16940
     1O EXCEED THE LIMIT (25).  USER TRIED TO INPUT ',I3,' SOURCES'/'   C1X16950
     2 *********EXECUTION TERMINATED**********')                        C1X16960
1260  FORMAT (1H0,'CONTER VALUE IS OUTSIDE OF RANGE: ','ZERO TO ONE. EXEC1X16970
     1CUTION TERMINATED.')                                              C1X16980
1270  FORMAT (' USER TRIED TO INPUT MORE THAN ',I4,' POINT SOURCES. THISC1X16990
     1 GOES BEYOND THE CURRENT PROGRAM DIMENSIONS.')                    C1X17000
1280  FORMAT (1X,'NPT =  ',I3,'I.E., EQUAL OR LESS THAN ZERO'/' RUN TERMC1X17010
     1INATED----CHECK INPUT DATA')                                      C1X17020
1290  FORMAT (1H1,'***ERROR---USER TRIED TO SPECIFY ',I4,' SIGNIFICANT SC1X17030
     1OURCES, BUT IS ONLY ALLOWING ',I3,' TOTAL SIGNIFICANT SOURCES IN TC1X17040
     2HIS RUN.',/2X,'***RUN TERMINATED-CHECK INPUT DATA!***')           C1X17050
1300  FORMAT (' (MPS) THE INPUT SIGNIFICANT SOURCE NUMBER ','WAS FOUND TC1X17060
     1O EQUAL ZERO - USER CHECK INPUT DATA.')                           C1X17070
1310  FORMAT (' SURFACE DATA IDENTIFIERS READ INTO MODEL (STATION=',I5,'C1X17080
     1 ,YEAR=',I2,') DO NOT AGREE WITH THE PREPROCESSOR OUTPUT FILE',/1XC1X17090
     2,' (STATION=',I5,' ,YEAR=',I2)                                    C1X17100
1320  FORMAT (' MIXING HEIGHT IDENTIFIERS READ INTO MODEL (STATION=',I5,C1X17110
     1' ,YEAR=',I2,') DO NOT AGREE WITH THE PREPROCESSOR OUTPUT FILE',/1C1X17120
     2X,' (STATION=',I5,' ,YEAR=',I2)                                   C1X17130
1330  FORMAT (1H0,' WRONG RECEPTOR ELEVATION CARD READ.','READ CARD FOR C1X17140
     1AZIMUTH ',I3,' SHOULD HAVE BEEN ',I3,'.')                         C1X17150
1340  FORMAT (1X,'****USER EITHER TRIED TO INPUT MORE THAN 180 ','RECEPTC1X17160
     1ORS OR ENDREC WAS NOT PLACED AFTER THE LAST RECEPTOR ','CARD****'/C1X17170
     2'********EXECUTION TERMINATED*******')                            C1X17180
1350  FORMAT (1X,'NO RECEPTORS HAVE BEEN CHOSEN')                       C1X17190
1360  FORMAT (1H0,'***DAYS DO NOT MATCH, IDAY = ',I4,', IDAYS = ',I4)   C1X17200
1370  FORMAT (' DATE ON MET. TAPE, ',I2,I3,' ,DOES NOT MATCH INTERNAL DAC1X17210
     1TE, ',I2,I3)                                                      C1X17220
1380  FORMAT (' HOUR ',I3,' IS NOT PERMITTED. HOURS MUST BE DEFINED BETWC1X17230
     1EEN 1 AND 24')                                                    C1X17240
1390  FORMAT (' DATE BEING PROCESSED IS= ',I8/1X,'DATE OF HOURLY POINT EC1X17250
     1MISSION RECORD IS =',I8/1X,'***PLEASE CHECK EMISSION RECORDS***') C1X17260
C                                                                       C1X17270
C        OUTPUT FORMATS                                                 C1X17280
C                                                                       C1X17290
 1061 FORMAT(5X,T98,'# CALMS FOR PERIOD: ',I4)                          C1X17300
 1095 FORMAT(1X,'   CALM HOURS (=1) FOR DAY',I4,'  ',24I3)              C1X17310
1399  FORMAT ('1',T30,'COMPLEX I / VALLEY OPTION'/3(/1X,20A4))          C1X17320
1400  FORMAT  ('0',T30,'COMPLEX I (DATED 92290) '//1X,20A4/1X,20A4/1X,20C1X17330
     1A4)                                                               C1X17340
1409  FORMAT (1H0,T30,'GENERAL INPUT INFORMATION'//2X,'THIS RUN OF COMPLC1X17350
     1EX I/VALLEY OPTION IS FOR ','THE POLLUTANT ',A4,' FOR ',I3,1X,    C1X17360
     2'WIND DIRECTIONS.',                                               C1X17370
     3/1X,' A FACTOR OF ',F14.7,' HAS ',                                C1X17380
     4'BEEN SPECIFIED TO ','CONVERT USER LENGTH UNITS TO KILOMETERS.'/, C1X17390
     5 1X,I3,' SIGNIFICANT SOURCES ARE TO BE CONSIDERED.')              C1X17400
1410  FORMAT (1H0,T30,'GENERAL INPUT INFORMATION'//2X,'THIS RUN OF COMPLC1X17410
     1EX I -VERSION 2.0 IS FOR ','THE POLLUTANT ',A4,' FOR   ',I4,1X,I3,C1X17420
     2'-HOUR PERIODS.'/2X,'CONCENTRATION ESTIMATES BEGIN ON HOUR-',I2,',C1X17430
     3JULIAN DAY-',I3,', YEAR-19',I2,'.'/1X,' A FACTOR OF ',F14.7,' HAS C1X17440
     4BEEN SPECIFIED TO ','CONVERT USER LENGTH UNITS TO KILOMETERS.'/1X,C1X17450
     5 I3,' SIGNIFICANT SOURCES ARE TO BE CONSIDERED.')                 C1X17460
1420  FORMAT (1H ,'THIS RUN WILL NOT CONSIDER ANY POLLUTANT LOSS.')     C1X17470
1430  FORMAT (1H ,2X,'A HALF-LIFE OF ',F10.2,' (SECONDS) HAS BEEN ASSUMEC1X17480
     1D BY THE USER.')                                                  C1X17490
1439  FORMAT (1X,' HIGH-FIVE SUMMARY 24-HOUR AVERAGE CONCENTRATION ',   C1X17500
     + 'TABLES WILL BE OUTPUT FOR EACH RECEPTOR.')                      C1X17510
1440  FORMAT (1X,' HIGH-FIVE SUMMARY CONCENTRATION TABLES ','WILL BE OUTC1X17520
     1PUT FOR ',I3,' AVERAGING PERIODS.'/'  AVG TIMES ','OF 1,3,8, AND 2C1X17530
     24 HOURS ARE AUTOMATICALLY DISPLAYED.')                            C1X17540
1450  FORMAT (1H ,2X,'A FACTOR OF ',F14.7,' HAS BEEN SPECIFIED TO CONVERC1X17550
     1T USER HEIGHT UNITS TO METERS.')                                  C1X17560
1460  FORMAT (1H0,T3,'OPTION  ',T16,'OPTION LIST',T46,'OPTION SPECIFICATC1X17570
     1ION : 0= IGNORE OPTION'/1X,T68,' 1= USE OPTION'/T25,'TECHNICAL OPTC1X17580
     2IONS'/1X,T7,I2,T16,'TERRAIN ADJUSTMENTS',T70,I1/1X,T7,I2,T16,'DO NC1X17590
     3OT INCLUDE STACK DOWNWASH CALCULATIONS',T70,I1/1X,T7,I2,T16,'DO NOC1X17600
     4T INCLUDE GRADUAL PLUME RISE CALCULATIONS',T70,I1/1X,T7,I2,T16,'CAC1X17610
     5LCULATE INITIAL PLUME SIZE',T70,I1/1X,T25,'INPUT OPTIONS'/1X,T7,I2C1X17620
     6,T16,'READ MET DATA FROM CARDS',T70,I1/1X,T7,I2,T16,'READ HOURLY EC1X17630
     7MISSIONS',T70,I1/1X,T7,I2,T16,'SPECIFY SIGNIFICANT SOURCES',T70,I1C1X17640
     8/1X,T7,I2,T16,'READ RADIAL DISTANCES TO GENERATE RECEPTORS',T70,I1C1X17650
     9/T25,'PRINTED OUTPUT OPTIONS'/1X,T7,I2,T16,'DELETE EMISSIONS WITH C1X17660
     AHEIGHT TABLE',T70,I1/1X,T7,I2,T16,'DELETE MET DATA SUMMARY FOR AVGC1X17670
     B PERIOD',T70,I1/1X,T7,I2,T16,'DELETE HOURLY CONTRIBUTIONS',T70,I1/C1X17680
     C1X,T7,I2,T16,'DELETE MET DATA ON HOURLY CONTRIBUTIONS',T70,I1/1X,TC1X17690
     D7,I2,T16,'DELETE FINAL PLUME RISE CALC ON HRLY CONTRIBUTIONS',T70,C1X17700
     EI1)                                                               C1X17710
1470  FORMAT (1X,T7,I2,T16,'DELETE HOURLY SUMMARY',T70,I1/1X,T7,I2,T16,'C1X17720
     1DELETE MET DATA ON HRLY SUMMARY',T70,I1/1X,T7,I2,T16,'DELETE FINALC1X17730
     2 PLUME RISE CALC ON HRLY SUMMARY',T70,I1/1X,T7,I2,T16,'DELETE AVG-C1X17740
     3PERIOD CONTRIBUTIONS',T70,I1/1X,T7,I2,T16,'DELETE AVERAGING PERIODC1X17750
     4 SUMMARY',T70,I1/1X,T7,I2,T16,'DELETE AVG CONCENTRATIONS AND HI-5 C1X17760
     5TABLES',T70,I1/T25,'OTHER CONTROL AND OUTPUT OPTIONS'/1X,T7,I2,T16C1X17770
     6,'RUN IS PART OF A SEGMENTED RUN',T70,I1/1X,T7,I2,T16,'WRITE PARTIC1X17780
     7AL CONC TO DISK OR TAPE',T70,I1/1X,T7,I2,T16,'WRITE HOURLY CONC TOC1X17790
     8 DISK OR TAPE',T70,I1/1X,T7,I2,T16,'WRITE AVG-PERIOD CONC TO DISK C1X17800
     9OR TAPE',T70,I1/1X,T7,I2,T16,'PUNCH AVG-PERIOD CONC ONTO CARDS',T7C1X17810
     A0,I1,/,1X,T7,I2,T16,'COMPLEX TERRAIN OPTION',T70,I1,/,1X,T7,I2,T16C1X17820
     B,'CALM PROCESSING OPTION',T70,I1,/                                C1X17830
     C,1X,T7,I2,T16,'VALLEY SCREENING OPTION',T70,I1)                   C1X17840
1480  FORMAT (1H0,2X,'ANEMOMETER HEIGHT= ',F10.2/3X,'WIND PROFILE WITH 'C1X17850
     1,'HEIGHT EXPONENTS CORRESPONDING TO STABILITY ARE AS FOLLOWS:'/8X,C1X17860
     2'FOR STABILITY A: ',F4.2/12X,'STABILITY B: ',F4.2/12X,'STABILITY CC1X17870
     3: ',F4.2,/12X,'STABILITY D: ',F4.2,/12X,'STABILITY E: ',F4.2/12X,'C1X17880
     4STABILITY F: ',F4.2)                                              C1X17890
1490  FORMAT (1H0,'ANEMOMETER HEIGHT IS:',F10.2/1X,'EXPONENTS FOR POWER-C1X17900
     1 LAW WIND INCREASE WITH HEIGHT ARE:',F4.2,5(',',F4.2)/' TERRAIN ADC1X17910
     2JUSTMENTS ARE: ',F5.3,5(',',F5.3),' ZMIN IS ',F5.1,//)            C1X17920
1499  FORMAT(//,'1 BECAUSE THE VALLEY OPTION HAS BEEN SELECTED, THE ',  C1X17930
     +   'FOLLOWING',/,' OPTIONS AND PARAMETERS HAVE BEEN SET BY',      C1X17940
     +   ' THE MODEL, OVERRIDING VALUES',/,' PROVIDED BY THE USER:',    C1X17950
     +   //,T10,'IOPT(5), IOPT(10), IOPT(12), IOPT(15), IOPT(17),',     C1X17960
     +   ' IOPT(18) = 1',/,T10,'IOPT(6), IOPT(20) THRU IOPT(26) = 0'    C1X17970
     +   ,/,T10,'NAVG = 1            NAV5 = 0',                         C1X17980
     +    /,T10,'IHSTRT = 1          CONTER(6) = 0.'                    C1X17990
     +   ,/,T10,'ZMIN = 10.          IKST = 6',                         C1X18000
     +    /,T10,'QU = 2.5            QHL = 9999.')                      C1X18010
1500  FORMAT ('1',T40,'POINT SOURCE INFORMATION'//1X,T5,'SOURCE',T23,'EAC1X18020
     1ST',T31,'NORTH',T39,'SO2(G/SEC)  PART(G/SEC)  STACK  STACK   STACKC1X18030
     2   STACK',3X,'POTEN. IMPACT',2X,'EFF',3X,'GRD-LVL BUOY FLUX'/1X,T2C1X18040
     33,'COORD',T31,'COORD','   EMISSIONS   EMISSIONS    HT(M) TEMP(K) DC1X18050
     4IAM(M)','VEL(M/SEC)(MICRO G/M**3) HT(M)',3X,'ELEV',6X,'F'/1X,T24,'C1X18060
     5(USER UNITS)',T116,'USER HT M**4/S**3'/1X,T117,'UNITS'/)          C1X18070
1501  FORMAT ('1',T40,'POINT SOURCE INFORMATION'//1X,T5,'SOURCE',T23,'EAC1X18080
     1ST',T31,'NORTH',T39,'SO2(G/SEC)  PART(G/SEC)  STACK  STACK   STACKC1X18090
     2   STACK',6X,'GRD-LVL BUOY FLUX'/1X,T23                           C1X18100
     3 ,'COORD',T31,'COORD','   EMISSIONS   EMISSIONS    HT(M) TEMP(K) DC1X18110
     4IAM(M)',' VEL(M/SEC)',3X,'ELEV',6X,'F'/1X,T24,                    C1X18120
     5'(USER UNITS)',T98,'USER HT M**4/S**3'/1X,T99,'UNITS'/)           C1X18130
1509  FORMAT (1X,I3,1X,3A4,1X,2F9.2,2F12.2,4F8.1,F9.2,2X,F9.2)          C1X18140
1510  FORMAT (1X,I3,1X,3A4,1X,2F9.2,2F12.2,4F8.1,6P,F13.2,0P,F9.2,2F9.2)C1X18150
1520  FORMAT ('0',T3,'SIGNIFICANT ',A4,' POINT SOURCES'//1X,T8,'RANK',T2C1X18160
     12,'CHI-MAX',T33,'SOURCE NO.'/1X,T17,'(MICROGRAMS/M**3)'/1X)       C1X18170
1530  FORMAT (1X,T9,I3,T18,6P,F12.2,T35,I3)                             C1X18180
1540  FORMAT (1X,'HEIGHT ABOVE 100M FOR POINT SOURCE',I4,3X,' HEIGHT=',FC1X18190
     16.2,' (METERS)','   EMISSIONS=',F10.2,' (G/SEC)')                 C1X18200
1550  FORMAT ('0',4X,'TOTAL ',A4,' EMISSION AND CUMULATIVE FRACTION ACCOC1X18210
     1RDING TO HEIGHT'//1X,T12,'TOTAL POINT   CUMULATIVE '/1X,'HEIGHT(M)C1X18220
     2 EMISSIONS(G/S)  FRACTION'/1X)                                    C1X18230
1560  FORMAT (1X,T2,I2,' -',I3,T11,F8.2,T26,F7.3,T41,F8.2,T56,F7.3)     C1X18240
1570  FORMAT ('0',T2,'TOTAL',2X,F10.2)                                  C1X18250
1580  FORMAT (1H0,21X,'ADDITIONAL INFORMATION ON SOURCES.')             C1X18260
1590  FORMAT (1H0,'  USER SPECIFIED ',I3,' (NPT) SIGNIFICANT POINT ','SOC1X18270
     1URCES AS LISTED BY POINT SOURCE NUMBER:'/2X,25I5)                 C1X18280
1600  FORMAT ('0',2X,'EMISSION INFORMATION FOR ',I4,' (NPT) POINT SOUR',C1X18290
     1'CES HAS BEEN INPUT'/2X,I2,' SIGNIFICANT POINT SOURCES(NSIGP) ','AC1X18300
     2RE TO BE',' USED FOR THIS RUN'/2X,'THE ORDER OF SIGNIFICANCE(IMPS)C1X18310
     3 FOR 25 OR LESS POINT SOURCES USED IN THIS RUN AS LISTED BY POINT C1X18320
     4SOURCE NUMBER:'/2X,25I5)                                          C1X18330
1610  FORMAT (2X,'SURFACE MET DATA FROM STATION(ISFCD) ',I6,', YEAR(ISFCC1X18340
     1YR) 19',I2/2X,'MIXING HEIGHT DATA FROM STATION(IMXD) ',I6,', YEAR(C1X18350
     2IMXYR) 19',I2)                                                    C1X18360
1620  FORMAT (1H0,T21,'RECEPTOR INFORMATION')                           C1X18370
1630  FORMAT (1H0,' COMPLEX I  INTERNALLY GENERATES 36 RECEPTORS ',     C1X18380
     1            'ON A  CIRCLE CORRESPONDING TO EACH NON-ZERO ',       C1X18390
     2            'RADIAL DISTANCE FROM A ENTER POINT ',/1X,T10,        C1X18400
     3            'COORDINATES ARE (USER UNITS): (',F8.3,',',F8.3,')',  C1X18410
     4             5(F11.3,'  '))                                       C1X18420
1640  FORMAT (F4.1)                                                     C1X18430
1650  FORMAT ('0',' RECEPTOR    IDENTIFICATION  EAST     NORTH     RECEPC1X18440
     1TOR HT     RECEPTOR GROUND LEVEL'/1X,T30,'COORD',T39,'COORD  ABV LC1X18450
     2OCAL GRD LVL        ELEVATION'/1X,T31,'(USER UNITS)         (METERC1X18460
     3S)          (USER HT UNITS)'/1X)                                  C1X18470
1660  FORMAT (1X,T3,I3,2A1,8X,2A4,F13.3,F10.3,F10.1,F20.1)              C1X18480
1680  FORMAT (//1X,' THE NUMBER OF   DAYS  PREVIOUSLY COMPLETED EQUAL ',C1X18490
     1I3,' AND THE LAST   DAY  TO BE COMPLETED IN THIS RUN IS ',I3)     C1X18500
1690  FORMAT ('1INPUT MET DATA  ',I2,'/',I4/1X,T2,'HOUR   THETA    SPEEDC1X18510
     1   MIXING   TEMP     STABILITY'/1X,T9,'(DEG)    (M/S)  HEIGHT(M) (C1X18520
     2DEG-K)  CLASS'/1X)                                                C1X18530
1700  FORMAT (1X,T3,I2,4F9.2,6X,I1)                                     C1X18540
1710  FORMAT ('0','RESULTANT MET CONDITIONS'/1X)                        C1X18550
1720  FORMAT (2X,'WIND DIRECTION=',F7.2,T36,'RESULTANT WIND SPEED=',F7.2C1X18560
     1/2X,'AVERAGE WIND SPEED=',F7.2,T36,'AVERAGE TEMP=',F7.2/2X,'WIND PC1X18570
     2ERSISTENCE=',F6.3,T36,'MODAL STABILITY=',I2)                      C1X18580
1730  FORMAT (1H0,' THIS SEGMENT OF A SEGMENTED RUN HAS COMPLETED',I5,' C1X18590
     1(IDAY) DAYS.')                                                    C1X18600
1740  FORMAT ('0',T9,'      RECEPTORS'//1X,'RECEPTOR    IDENTIFICATION  C1X18610
     1EAST     NORTH     RECEPTOR HT     RECEPTOR GROUND LEVEL',T99,'AVGC1X18620
     2 CONC FOR PERIOD'/1X,T30,'COORD',T39,'COORD  ABV LOCAL GRD LVL    C1X18630
     3   ELEVATION',T94,'DAY',F4.0,'HR',F3.0,' TO DAY',F4.0,'HR',F3.0/1XC1X18640
     4,T31,'(USER UNITS)        (METERS)         (USER HT UNITS)',T100,'C1X18650
     5(MICROGRAMS/M**3)'/1X)                                            C1X18660
1750  FORMAT (1X,T3,I3,10X,2A4,5X,F8.2,2X,F8.2,F10.1,F20.1,T110,A1,6PF7.C1X18670
     12)                                                                C1X18680
1759  FORMAT (1H1,T33,'VALLEY: FIVE HIGHEST 24-HOUR ',A4,               C1X18690
     1 ' CONCENTRATIONS',                                               C1X18700
     2' (WIND DIRECTION)'/1X,T55,'(MICROGRAMS/M**3)'//2X,'RECEPTOR',    C1X18710
     3     T38,4(I1,20X),I1,/1X)                                        C1X18720
1760  FORMAT (1H0,T41,'FIVE HIGHEST ',I2,'-HOUR ',A4,' CONCENTRATIONS((EC1X18730
     1NDING ON JULIAN DAY, HOUR)'/1X,T55,'(MICROGRAMS/M**3)'//2X,'RECEPTC1X18740
     2OR ',T38,4(I1,20X),I1,/1X)                                        C1X18750
 1761 FORMAT (1H1,T29,'FIVE HIGHEST ',I2,'-HOUR ',A4,'CONCENTRATIONS((ENC1X18760
     1DING ON JULIAN DAY, HOUR)'/1X,T55,'(MICROGRAMS/M**3)'/1X,T36,     C1X18770
     2'C-FLAG IDENTIFIES CONCENTRATIONS AFFECTED BY CALM HOURS'//2X,    C1X18780
     3'RECEPTOR ',T38,4(I1,20X),I1,/1X)                                 C1X18790
 1769 FORMAT (1H ,2X,I3,'(',F7.2,',',F7.2,')',2(3X,A1,6PF9.2,A1,1X,'(', C1X18800
     10PF5.1,')'),3(4X,6PF9.2,A1,1X,'(',0PF5.1,')'))                    C1X18810
 1770 FORMAT (1H ,2X,I3,'(',F7.2,',',F7.2,')',2(1X,A1,6PF9.2,A1,1X,'(',IC1X18820
     13,',',I2,')'),3(2X,6PF9.2,A1,1X,'(',I3,',',I2,')'))               C1X18830
 2531 FORMAT(F5.1)                                                      IBM
 2532 FORMAT(F5.2)                                                      IBM
 2533 FORMAT(F5.3)                                                      IBM
C                                                                       C1X18840
      END                                                               C1X18850
C
      SUBROUTINE PTR                                                    C1X18870
C                                                                       C1X18880
C      THE PURPOSE OF THIS ROUTINE IS TO CALCULATE CONCENTRATIONS FROM  C1X18890
C      POINT SOURCES.                                                   C1X18900
C                                                                       C1X18910
C->->->->SECTION PTR.A - COMMON AND DIMENSION.                          C1X18920
C                                                                       C1X18930
      COMMON /MPOR/ IOPT(27),ZMIN                                       C1X18940
      COMMON /MPO/ NRECEP,NAVG,NB,LH,NPT,IDATE(2),RREC(180),SREC(180),ZRC1X18950
     1(180),ELR(180),PHCHI(180),PHSIGS(180,26),HSAV(250),DSAV(250),PCHI(C1X18960
     2180),PSIGS(180,26),IPOL                                           C1X18970
      COMMON /MPR/ UPL,Z,H,HL,X,Y,KST,DELH,SY,SZ,RC                     C1X18980
      COMMON /MP/ SOURCE(9,250),CONTWO,PSAV(250),IPSIGS(250),U,TEMP,SINTC1X18990
     1,COST,PL(6),ELP(250),ELHN,HANE,TLOS                               C1X19000
      COMMON /MSL/ ER,EP,CELM,CTER,ICLM,ICALMP,VDIR(360)                C1X19010
      DIMENSION UPH(250), HPR(250), FP(250), DH(250), PARTC(250)        C1X19020
C                                                                       C1X19030
C->->->->SECTION PTR.B - INITIALIZE AND START RECEPTOR LOOP.            C1X19040
C                                                                       C1X19050
C        ZERO EFFECTIVE STACK HEIGHT FOR EACH SOURCE                    C1X19060
C                                                                       C1X19070
C          NPT - THE NUMBER OF POINT SOURCES                            C1X19080
      CALL CMPLX                                                        C1X19090
      DO 10 J=1,NPT                                                     C1X19100
C        HSAV WILL BE USED TO STORE THE SOURCE PLUME HEIGHTS.           C1X19110
10    HSAV(J)=0.0                                                       C1X19120
C        LOOP ON RECEPTORS                                              C1X19130
C          NRECEP - THE NUMBER OF RECEPTORS                             C1X19140
      DO 180 K=1,NRECEP                                                 C1X19150
C        IF IOPT(1)=1, TERRAIN ADJUSTMENTS ARE MADE.                    C1X19160
      IF (IOPT(1).EQ.0) GO TO 20                                        C1X19170
C          ELR - RECEPTOR GROUND LEVEL ELEVATION  (USER HT. UNITS)      C1X19180
      ER=ELR(K)                                                         C1X19190
C          ELHN - LOWEST SOURCE STACK-TOP ELEVATION                     C1X19200
      IF(IOPT(25) .NE. 0 .OR. IOPT(27) .EQ. 1) GOTO20                   C1X19210
      IF (ER.LE.ELHN) GO TO 20                                          C1X19220
      PCHI(K)=99999.E+26                                                C1X19230
      PHCHI(K)=99999.E+26                                               C1X19240
      GO TO 180                                                         C1X19250
20    CONTINUE                                                          C1X19260
C          ZR - RECEPTOR HEIGHT ABOVE GROUND                            C1X19270
      Z=ZR(K)                                                           C1X19280
C                                                                       C1X19290
C->->->->SECTION PTR.C - START SOURCES LOOP, CALCULATE                  C1X19300
C                        UPWIND AND CROSSWIND DISTANCES.                C1X19310
C                                                                       C1X19320
      DO 170 J=1,NPT                                                    C1X19330
      PARTC(J)=0.0                                                      C1X19340
C        IF A CALM HOUR IS ENCOUNTERED, SET THE CONCENTRATIONS AT THE   C1X19350
C        RECEPTORS EQUAL TO ZERO.                                       C1X19360
      IF (ICLM .EQ. 1) THEN                                             C1X19370
         PROD=0.0                                                       C1X19380
         GO TO 150                                                      C1X19390
      ENDIF                                                             C1X19400
C          RQ - EAST COORDINATE OF THE SOURCE                           C1X19410
      RQ=SOURCE(1,J)                                                    C1X19420
C          SQ - NORTH COORDINATE OF THE SOURCE                          C1X19430
      SQ=SOURCE(2,J)                                                    C1X19440
C          ELP - SOURCE GROUND LEVEL ELEVATION   (USER HT. UNITS)       C1X19450
      EP=ELP(J)                                                         C1X19460
C        DETERMINE UPWIND DISTANCE                                      C1X19470
C        XDUM,YDUM IN USER UNITS. X,Y IN KM.                            C1X19480
C          RREC - EAST COORDINATE OF THE RECEPTOR                       C1X19490
      XDUM=RQ-RREC(K)                                                   C1X19500
C          SREC - NORTH COORDINATE OF THE RECEPTOR                      C1X19510
      YDUM=SQ-SREC(K)                                                   C1X19520
C        SINT AND COST ARE THE SIN AND COS OF THE WIND DIRECTION        C1X19530
C        CONTWO - MULTIPLIER CONSTANT TO CONVERT USER UNITS TO KM       C1X19540
      X=(YDUM*COST+XDUM*SINT)*CONTWO                                    C1X19550
C         X IS THE UPWIND DISTANCE  OF THE SOURCE FROM THE RECEPTOR.    C1X19560
C        IF X IS NEGATIVE, INDICATING THAT THE SOURCE IS DOWNWIND OF    C1X19570
C        THE RECEPTOR, THE CALCULATION IS TERMINATED ASSUMING NO        C1X19580
C        CONTRIBUTION FROM THAT SOURCE.                                 C1X19590
      IF (X.LE.0.0) GO TO 170                                           C1X19600
C                                                                       C1X19610
C        DETERMINE CROSSWIND DISTANCE                                   C1X19620
C                                                                       C1X19630
      Y=(YDUM*SINT-XDUM*COST)*CONTWO                                    C1X19640
      H=HSAV(J)                                                         C1X19650
C        SKIP PLUME RISE CALCULATION IF EFFECTIVE HT. HAS ALREADY BEEN  C1X19660
C           CALCULATED FOR THIS SOURCE                                  C1X19670
      IF (H.EQ.0.0) GO TO 30                                            C1X19680
      DELH=DH(J)                                                        C1X19690
C                                                                       C1X19700
C->->->->SECTION PTR.D - EXTRAPOLATE WIND SPEED TO STACK TOP            C1X19710
C                        CALCULATE PLUME RISE.                          C1X19720
C                                                                       C1X19730
      GO TO 110                                                         C1X19740
C         MODIFY WIND SPEED BY POWER LAW PROFILE IN ORDER TO TAKE INTO  C1X19750
C        ACCOUNT THE INCREASE OF WIND SPEED WITH HEIGHT.                C1X19760
C        ASSUME WIND MEASUREMENTS ARE REPRESENTATIVE FOR HEIGHT = HANE. C1X19770
C         THT IS THE PHYSICAL STACK HEIGHT                              C1X19780
30    THT=SOURCE(5,J)                                                   C1X19790
C        POINT SOURCE HEIGHT NOT ALLOWED TO BE LESS THAN 1 METER.       C1X19800
      IF (THT.LT.1.) THT=1.                                             C1X19810
C          U - WIND SPEED AT HEIGHT 'HANE'                              C1X19820
C          PL - POWER FOR THE WIND PROFILE                              C1X19830
C          UPL - WIND AT THE PHYSICAL STACK HEIGHT                      C1X19840
      UPL=U*(THT/HANE)**PL(KST)                                         C1X19850
C        WIND SPEED NOT ALLOWED TO BE LESS THAN 1 METER/SEC.            C1X19860
      IF (UPL.LT.1.) UPL=1.                                             C1X19870
C        STORE THE STACK TOP WIND FOR THE JTH SOURCE FOR THIS HOUR      C1X19880
      UPH(J)=UPL                                                        C1X19890
      VS=SOURCE(8,J)                                                    C1X19900
      BUOY=SOURCE(9,J)                                                  C1X19910
      TS=SOURCE(6,J)                                                    C1X19920
C        TEMP- THE AMBIENT AIR TEMPERATURE FOR THIS HOUR                C1X19930
      DELT=TS-TEMP                                                      C1X19940
      F=BUOY*DELT/TS                                                    C1X19950
C         IOPT(6) HOURLY EMISSION INPUT FROM TAPE/DISK?  0=NO, 1=YES.   C1X19960
      IF (IOPT(6).EQ.0) GO TO 40                                        C1X19970
C        MODIFY EXIT VELOCITY AND BUOYANCY BY RATIO OF HOURLY EMISSIONS C1X19980
C        TO AVERAGE EMISSIONS                                           C1X19990
      SCALE=SOURCE(IPOL,J)/PSAV(J)                                      C1X20000
      VS=VS*SCALE                                                       C1X20010
      F=F*SCALE                                                         C1X20020
40    D=SOURCE(7,J)                                                     C1X20030
C        CALCULATE  H PRIME WHICH TAKES INTO ACCOUNT STACK DOWNWASH     C1X20040
C        BRIGGS(1973) PAGE 4                                            C1X20050
      HPRM=THT                                                          C1X20060
C        IF IOPT(2)=1, THEN NO STACK DOWNWASH COMPUTATION               C1X20070
      IF (IOPT(2).EQ.1) GO TO 50                                        C1X20080
      DUM=VS/UPL                                                        C1X20090
      IF (DUM.LT.1.5) HPRM=THT+2.*D*(DUM-1.5)                           C1X20100
C        'HPRM' IS BRIGGS' H-PRIME                                      C1X20110
      IF (HPRM.LT.0.) HPRM=0.                                           C1X20120
50    CONTINUE                                                          C1X20130
C                                                                       C1X20140
C        CALCULATE PLUME RISE AND ADD H PRIME TO OBTAIN EFFECTIVE       C1X20150
C        STACK HEIGHT.                                                  C1X20160
C                                                                       C1X20170
C        PLUME RISE CALCULATION                                         C1X20180
      IF (KST.GT.4) GO TO 70                                            C1X20190
C        PLUME RISE FOR UNSTABLE CONDITIONS                             C1X20200
      IF (TS.LT.TEMP) GO TO 80                                          C1X20210
      IF (F.GE.55.) GO TO 60                                            C1X20220
C        DETERMINE DELTA-T FOR BUOYANCY-MOMENTUM CROSSOVER(F<55)        C1X20230
C        FOUND BY EQUATING BRIGGS(1969) EQ 5.2, P.59 WITH COMBINATION OFC1X20240
C        BRIGGS(1971) EQUATIONS 6 AND 7, PAGE 1031 FOR F<55.            C1X20250
      DTMB=0.0297*TS*VS**0.33333/D**0.66667                             C1X20260
      IF (DELT.LT.DTMB) GO TO 80                                        C1X20270
C        DISTANCE OF FINAL BUOYANT RISE(0.049 IS 14*3.5/1000)           C1X20280
C        BRIGGS(1971) EQN. 7,F<55, AND DIST TO FINAL RISE IS 3.5 XSTAR  C1X20290
C        DISTF IN KILOMETERS                                            C1X20300
      DISTF=0.049*F**0.625                                              C1X20310
C        COMBINATION OF BRIGGS(1971) EQNS. 6 AND 7, PAGE 1031 FOR F<55. C1X20320
      DELH=21.425*F**0.75/UPL                                           C1X20330
      GO TO 100                                                         C1X20340
C        DETERMINE DELTA-T FOR BUOYANCY-MOMENTUM CROSSOVER(F>55)        C1X20350
C        FOUND BY EQUATING BRIGGS(1969) EQ 5.2, P.59 WITH COMBINATION OFC1X20360
C         BRIGGS(1971) EQUATIONS 6 AND 7, PAGE 1031 FOR F>55.           C1X20370
60    DTMB=0.00575*TS*VS**0.66667/D**0.33333                            C1X20380
      IF (DELT.LT.DTMB) GO TO 80                                        C1X20390
C        DISTANCE OF FINAL BUOYANT RISE (0.119 IS 34*3.5/1000)          C1X20400
C        BRIGGS(1971) EQN. 7, F>55, AND DIST TO FINAL RISE IS 3.5 XSTAR.C1X20410
C        DISTF IN KILOMETERS                                            C1X20420
      DISTF=0.119*F**0.4                                                C1X20430
C        COMBINATION OF BRIGGS(1971) EQNS. 6 AND 7, PAGE 1031 FOR F>55. C1X20440
      DELH=38.71*F**0.6/UPL                                             C1X20450
      GO TO 100                                                         C1X20460
C        PLUME RISE FOR STABLE CONDITIONS.                              C1X20470
70    DTHDZ=0.02                                                        C1X20480
      IF (KST.GT.5) DTHDZ=0.035                                         C1X20490
      S=9.80616*DTHDZ/TEMP                                              C1X20500
      IF (TS.LT.TEMP) GO TO 90                                          C1X20510
C        DETERMINE DELTA-T FOR BUOYANCY-MOMENTUM CROSSOVER(STABLE)      C1X20520
C        FOUND BY EQUATING BRIGGS(1975) EQ 59, P. 96 FOR STABLE BUOYANCYC1X20530
C        RISE WITH BRIGGS(1969) EQ 4.28, P. 59 FOR STABLE MOMENTUM RISE.C1X20540
      DTMB=0.019582*TEMP*VS*SQRT(S)                                     C1X20550
      IF (DELT.LT.DTMB) GO TO 90                                        C1X20560
C        STABLE BUOYANT RISE FOR WIND CONDITIONS.(WIND NOT ALLOWED LOW  C1X20570
C        ENOUGH TO REQUIRE STABLE RISE IN CALM CONDITIONS.)             C1X20580
C        BRIGGS(1975) EQ 59, PAGE 96.                                   C1X20590
      DELH=2.6*(F/(UPL*S))**0.333333                                    C1X20600
C        COMBINATION OF BRIGGS(1975) EQ 48 AND EQ 59. NOTE DISTF IN KM. C1X20610
      DISTF=0.0020715*UPL/SQRT(S)                                       C1X20620
      GO TO 100                                                         C1X20630
C        UNSTABLE-NEUTRAL MOMENTUM RISE                                 C1X20640
C        BRIGGS(1969) EQN. 5.2, PAGE 59 NOTE: MOST ACCURATE WHEN VS/U>4;C1X20650
C        TENDS TO OVERESTIMATE RISE WHEN VS/U<4 (SEE BRIGGS(1975) P. 78,C1X20660
C         FIGURE 4.)                                                    C1X20670
80    DELH=3.*VS*D/UPL                                                  C1X20680
      DISTF=0.                                                          C1X20690
      GO TO 100                                                         C1X20700
C        STABLE MOMENTUM RISE                                           C1X20710
90    DHA=3.*VS*D/UPL                                                   C1X20720
C        BRIGGS(1969) EQUATION 4.28, PAGE 59                            C1X20730
      DELH=1.5*(VS*VS*D*D*TEMP/(4.*TS*UPL))**0.333333/S**0.166667       C1X20740
      IF (DHA.LT.DELH) DELH=DHA                                         C1X20750
      DISTF=0.                                                          C1X20760
C        STORE OFF PLUME HEIGHT(ETC.) FOR THIS SOURCE FOR USE WITH      C1X20770
C        OTHER RECEPTORS.                                               C1X20780
100   H=HPRM+DELH                                                       C1X20790
      HSAV(J)=H                                                         C1X20800
      DH(J)=DELH                                                        C1X20810
      DSAV(J)=DISTF                                                     C1X20820
      UPH(J)=UPL                                                        C1X20830
      HPR(J)=HPRM                                                       C1X20840
      FP(J)=F                                                           C1X20850
C        IF SOURCE-RECEPTOR DISTANCE IS GREATER OR EQUAL TO DISTANCE TO C1X20860
C        FINAL RISE, SKIP PLUME RISE CALCULATION AND USE FINAL RISE.    C1X20870
110   IF (X.GE.DSAV(J)) GO TO 120                                       C1X20880
      IF (IOPT(4).EQ.0.AND.IOPT(3).EQ.1) GO TO 120                      C1X20890
C        CALCULATE GRADUAL PLUME RISE IF (1) THE USER SPECIFIES SO,     C1X20900
C        OR (2) USER EMPLOYS CALCULATION OF INITIAL DISPERSION.....     C1X20910
C        IN THIS CASE, USE OF FINAL EFFECTIVE HEIGHT IN THE CALCULATION C1X20920
C        OF DISPERSION COEFFICENTS COULD LEAD TO MISLEADING VALUES SINCEC1X20930
C          SIGMA-Y,-Z =  DELTA-H/3.5                                    C1X20940
      DELH=160.*FP(J)**0.333333*X**0.666667/UPH(J)                      C1X20950
C        PLUME RISE FOR DISTANCE X(160 IS 1.6*1000**.67 BECAUSE X IN KM)C1X20960
      IF (DELH.GT.DH(J)) DELH=DH(J)                                     C1X20970
      IF (IOPT(3).EQ.1) GO TO 120                                       C1X20980
C        IF SPECIFYING CALCULATION OF INITIAL DISPERSION BUT ARE NOT    C1X20990
C        SPECIFYING CALCULATION OF GRADUAL PLUME RISE, THEN DO NOT      C1X21000
C        ADD THE NEW GRADUAL DELTA-H TO THE EFFECTIVE HEIGHT. OTHERWISE,C1X21010
C        CHECK THE GRADUAL RISE PLUME HEIGHT WITH FINAL EFFECTIVE HEIGHTC1X21020
C        AND SET THE PLUME HEIGHT TO THE SMALLER OF THE TWO VALUES.     C1X21030
      H=HPR(J)+DELH                                                     C1X21040
C        ADD PLUME RISE TO STACK HEIGHT FOR TOTAL EFFECTIVE STACK HT.   C1X21050
C        END PLUME RISE CALCULATION                                     C1X21060
120   UPL=UPH(J)                                                        C1X21070
C                                                                       C1X21080
C->->->->SECTION PTR.E - CALCULATE THE CONTRIBUTION OF                  C1X21090
C                        ONE SOURCE TO ONE RECEPTOR.                    C1X21100
C                                                                       C1X21110
      IF(KST.GT.4)GOTO130                                               C1X21120
      IF (H.LT.HL) GO TO 130                                            C1X21130
 125  PROD=0.                                                           C1X21140
      GO TO 150                                                         C1X21150
C        IF IOPT(1) = 1, TERRAIN ADJUSTMENTS ARE MADE                   C1X21160
130   IF(IOPT(25).GT.0) THEN                                            C1X21170
      CALL CMPXRC                                                       C1X21180
      GO TO 145                                                         C1X21190
      END IF                                                            C1X21200
      IF(IOPT(1).EQ.0) GO TO 140                                        C1X21210
      DUM = ER - EP                                                     C1X21220
      HE=H                                                              C1X21230
      H = H + CELM*(CTER*DUM - DUM)                                     C1X21240
C***    ATTENUATE CONCENTRATIONS LINEARLY OVER 400 METERS TO ZERO AT    C1X21250
C***    ELEVATED RECEPTORS IF THE VALLEY OPTION IS                      C1X21260
C***    SELECTED AND OTHER CONDITIONS ARE MET.                          C1X21270
      IF(IOPT(27) .EQ. 1) THEN                                          C1X21280
        ATTEN=1.0                                                       C1X21290
        IF(HE .LT. 10.0001) THEN                                        C1X21300
          H=HE                                                          C1X21310
          GO TO 136                                                     C1X21320
        END IF                                                          C1X21330
        IF(H .GT. 0.) GO TO 135                                         C1X21340
        IF(H .LE. -400.) GO TO 125                                      C1X21350
        ATTEN=400./(400.+H)                                             C1X21360
  135   IF(H .LT. 10.) H=10.                                            C1X21370
  136   CONTINUE                                                        C1X21380
      END IF                                                            C1X21390
C         RCP RETURNS THE DISPERSION PARAMETERS, SY AND SZ (METERS)     C1X21400
C        AND THE RELATIVE CONCENTRATION VALUES CHI/Q (SEC/M**3)         C1X21410
140   CALL RCP                                                          C1X21420
145   CONTINUE                                                          C1X21430
C        CALCULATE TRAVEL TIME IN KM-SEC/M TO INCLUDE DECAY RATE OF     C1X21440
C        POLLUTANT.                                                     C1X21450
      TT=X/UPL                                                          C1X21460
C        TLOS IN METERS/KM-SEC, SO TT*TLOS IS DIMENSIONLESS             C1X21470
C        INCLUDE THE POLLUTANT LOSS                                     C1X21480
C***   IF THE VALLEY OPTION IS CHOSEN, SIMULATE A 24-HOUR               C1X21490
C***   EMISSION RATE BY DIVIDING THE ONE-HOUR EMISSION RATE             C1X21500
C***   BY FOUR.  THIS ASSUMES A PERSISTENCE OF MET CONDITIONS           C1X21510
C***   FOR SIX HOURS. ALSO INCLUDE ATTENUATION FACTOR.                  C1X21520
      IF(IOPT(27) .EQ. 1) RC=.25*RC/ATTEN                               C1X21530
      PROD=RC*SOURCE(IPOL,J)/EXP(TT*TLOS)                               C1X21540
C        IF HAFL IS ZERO, TLOS WILL START AS ZERO AND                   C1X21550
C        RESULT IN NO COMPUTATIION OF POLLUTANT LOSS.                   C1X21560
C        INCREMENT CONCENTRATION AT K-TH RECEPTOR(G/M**3)               C1X21570
C          PCHI - SUM FOR THE AVERAGING TIME AT RECEPTOR K              C1X21580
150   PCHI(K)=PCHI(K)+PROD                                              C1X21590
C          PHCHI - CONCENTRATION FOR THIS HOUR AT RECEPTOR K            C1X21600
      PHCHI(K)=PHCHI(K)+PROD                                            C1X21610
      KSIG=IPSIGS(J)                                                    C1X21620
      IF (KSIG.EQ.0) GO TO 160                                          C1X21630
C        STORE CONCENTRATIONS FROM SIGNIFICANT SOURCES.(G/M**3)         C1X21640
      PSIGS(K,KSIG)=PSIGS(K,KSIG)+PROD                                  C1X21650
      PHSIGS(K,KSIG)=PHSIGS(K,KSIG)+PROD                                C1X21660
      PSIGS(K,26)=PSIGS(K,26)+PROD                                      C1X21670
      PHSIGS(K,26)=PHSIGS(K,26)+PROD                                    C1X21680
160   PARTC(J)=PROD                                                     C1X21690
C                                                                       C1X21700
C->->->->SECTION PTR.F - END SOURCE AND RECEPTOR LOOPS.                 C1X21710
C                                                                       C1X21720
170   CONTINUE                                                          C1X21730
C        END OF LOOP FOR SOURCES                                        C1X21740
C        WRITE PARTIAL CONCENTRATIONS ON DISK(G/M**3)  IF IOPT(21) = 1. C1X21750
      IF (IOPT(21).EQ.0) GO TO 180                                      C1X21760
C        USER PLEASE NOTE: PARTIAL CONC. IN G/M**3, NOT MICROGRAM/M**3  C1X21770
      WRITE (10) IDATE,LH,K,(PARTC(J),J=1,NPT)                          C1X21780
180   CONTINUE                                                          C1X21790
C        END OF LOOP FOR RECEPTORS                                      C1X21800
      RETURN                                                            C1X21810
C                                                                       C1X21820
C***   SECTIONS OF SUBROUTINE PTR.                                      C1X21830
C         SECTION PTR.A -  COMMON AND DIMENSION.                        C1X21840
C         SECTION PTR.B -  INITIALIZE AND START RECEPTOR LOOP.          C1X21850
C         SECTION PTR.C -  START SOURCES LOOP; CALCULATE UPWIND AND     C1X21860
C                           CROSSWIND DISTANCES.                        C1X21870
C         SECTION PTR.D -  EXTRAPOLATE WIND SPEED TO STACK TOP;         C1X21880
C                           CALCULATE PLUME RISE.                       C1X21890
C         SECTION PTR.E -  CALCULATE CONTRIBUTION FROM A SOURCE TO ONE  C1X21900
C                           RECEPTOR.                                   C1X21910
C         SECTION PTR.F -  END SOURCE AND RECEPTOR LOOPS.               C1X21920
C                                                                       C1X21930
      END                                                               C1X21940
C
      SUBROUTINE RCP                                                    C1X21960
C                                                                       C1X21970
C                                                                       C1X21980
C->->->->SECTION RCP.A - COMMON.                                        C1X21990
      COMMON /MPOR/ IOPT(27),ZMIN                                       C1X22000
      COMMON /MPR/ UPL,Z,H,HL,X,Y,KST,DELH,SY,SZ,RC                     C1X22010
C                                                                       C1X22020
C***   MODIFICATIONS:                                                   C1X22030
C        11/27/79 BY K.W.BALDRIDGE, C.S.C., CONVERTED CODE FROM FIELDATAC1X22040
C                 TO ASCII FORTRAN AND MADE CODE MORE STANDARD          C1X22050
C                                                                       C1X22060
C->->->->SECTION RCP.B - EXPLANATIONS AND COMPUTATIONS                  C1X22070
C                        COMMON TO ALL CONDITIONS.                      C1X22080
C                                                                       C1X22090
C  RCP DETERMINES RELATIVE CONCENTRATIONS, CHI/Q, FROM POINT SOURCES.   C1X22100
C         IT CALLS UPON  PGYZ TO OBTAIN STANDARD DEVIATIONS.            C1X22110
C        THE INPUT VARIABLES ARE....                                    C1X22120
C         UPL  WIND SPEED (M/SEC)                                       C1X22130
C         Z    RECEPTOR HEIGHT (M)                                      C1X22140
C         H    EFFECTIVE STACK HEIGHT (M)                               C1X22150
C         HL   MIXING HEIGHT- TOP OF NEUTRAL OR UNSTABLE LAYER(M).      C1X22160
C         X    DISTANCE RECEPTOR IS DOWNWIND OF SOURCE (KM)             C1X22170
C         Y    DISTANCE RECEPTOR IS CROSSWIND FROM SOURCE (KM)          C1X22180
C         KST  STABILITY CLASS                                          C1X22190
C         DELH PLUME RISE(METERS)                                       C1X22200
C        THE OUTPUT VARIABLES ARE....                                   C1X22210
C          SZ VERTICAL DISPERSION PARAMETER                             C1X22220
C         RC   RELATIVE CONCENTRATION (SEC/M**3) ,CHI/Q                 C1X22230
C        IO IS CONTROL CODE FOR WARNING OUTPUT.                         C1X22240
      IO=6                                                              C1X22250
C        THE FOLLOWING EQUATION IS SOLVED --                            C1X22260
C     RC=(1./(SQRT(2*PI)*UPL*W))                                        C1X22270
C           (EXP(-0.5*((Z-H)/SIGMA Z)**2) + EXP(-0.5*((Z+H)/SIGMA Z)**2)C1X22280
C             PLUS THE SUM OF THE FOLLOWING 4 TERMS K TIMES (N=1,K) --  C1X22290
C                  FOR NEUTRAL OR UNSTABLE CASES:                       C1X22300
C              TERM 1- EXP(-0.5*((Z-H-2NL)/SIGMA Z)**2)                 C1X22310
C              TERM 2- EXP(-0.5*((Z+H-2NL)/SIGMA Z)**2)                 C1X22320
C              TERM 3- EXP(-0.5*((Z-H+2NL)/SIGMA Z)**2)                 C1X22330
C              TERM 4- EXP(-0.5*((Z+H+2NL)/SIGMA Z)**2)                 C1X22340
C              WHERE W IS THE ARC LENGTH (METERS)OF A 22.5 DEGREE SECTORC1X22350
C NOTE THAT MIXING HEIGHT- THE TOP OF THE NEUTRAL OR UNSTABLE LAYER-    C1X22360
C HAS A VALUE ONLY FOR STABILITIES 1-4, THAT IS, MIXING HEIGHT,         C1X22370
C THE HEIGHT OF THE NEUTRAL OR UNSTABLE LAYER, DOES NOT EXIST FOR STABLEC1X22380
C LAYERS AT THE GROUND SURFACE- STABILITY 5 OR 6.                       C1X22390
C        THE ABOVE EQUATION IS SIMILAR TO EQUATION (5.8) P 36 IN        C1X22400
C         WORKBOOK OF ATMOSPHERIC DISPERSION ESTIMATES WITH THE ADDITIONC1X22410
C         OF THE EXPONENTIAL INVOLVING Y.                               C1X22420
C       IF STABLE, SKIP CONSIDERATION OF MIXING HEIGHT.                 C1X22430
      IF (KST.GE.5) GO TO 50                                            C1X22440
C        IF THE SOURCE IS ABOVE THE LID, SET RC = 0., AND RETURN.       C1X22450
      IF (H.GT.HL) GO TO 20                                             C1X22460
      IF (Z-HL) 50,50,40                                                C1X22470
20    IF (Z.LT.HL) GO TO 40                                             C1X22480
      WRITE (IO,470)                                                    C1X22490
40    RC=0.                                                             C1X22500
      RETURN                                                            C1X22510
C        IF X IS LESS THAN 1 METER, SET RC=0. AND RETURN.  THIS AVOIDS  C1X22520
C         PROBLEMS OF INCORRECT VALUES NEAR THE SOURCE.                 C1X22530
50    IF (X.LT.0.001) GO TO 40                                          C1X22540
      YPRIME=X*0.19891                                                  C1X22550
C   YPRIME IS CROSSWIND EXTENT OF A 11.25 DEGREE SECTOR IN KM.          C1X22560
      IF(ABS(Y).GT.YPRIME) GOTO40                                       C1X22570
      X=SQRT(X*X+Y*Y)                                                   C1X22580
      W=392.7*X                                                         C1X22590
C        CALL PGZ TO OBTAIN VALUE FOR SZ                                C1X22600
      CALL PGZ                                                          C1X22610
C         SZ = SIGMA Z, THE STANDARD DEVIATION OF CONCENTRATION IN THE  C1X22620
C         Z-DIRECTION (M)                                               C1X22630
C       IF IOPT(4)=1, CONSIDER BUOYANCY INDUCED DISPERSION OF PLUME DUE C1X22640
C         TO TURBULENCE DURING BUOYANT RISE.                            C1X22650
65    IF (IOPT(4).EQ.0) GO TO 100                                       C1X22660
      DUM=DELH/3.5                                                      C1X22670
      DUM=DUM*DUM                                                       C1X22680
      SZ=SQRT(SZ*SZ+DUM)                                                C1X22690
100   IF (KST.GT.4) GO TO 120                                           C1X22700
      IF (HL.LT.5000.) GO TO 200                                        C1X22710
C        IF STABLE CONDITION OR UNLIMITED MIXING HEIGHT,                C1X22720
C         USE EQUATION 3.2 IF Z = 0, OR EQ 3.1 FOR NON-ZERO Z.          C1X22730
C         (EQUATION NUMBERS REFER TO WORKBOOK OF ATMOSPHERIC DISPERSION C1X22740
C         ESTIMATES.)                                                   C1X22750
120   C2=2.*SZ*SZ                                                       C1X22760
      IF (Z) 40,130,150                                                 C1X22770
C       NOTE: AN ERRONEOUS NEGATIVE Z WILL RESULT IN ZERO CONCENTRATIONSC1X22780
C                                                                       C1X22790
C->->->->SECTION RCP.C - STABLE OR UNLIMITED MIXING, Z IS ZERO.         C1X22800
C                                                                       C1X22810
130   C3=H*H/C2                                                         C1X22820
      IF (C3.GE.50.) GO TO 40                                           C1X22830
      A2=1./EXP(C3)                                                     C1X22840
C        WADE EQUATION 3.2.                                             C1X22850
      RC=0.79788*A2/(UPL*SZ*W)                                          C1X22860
      RETURN                                                            C1X22870
C                                                                       C1X22880
C->->->->SECTION RCP.D - STABLE OR UNLIMITED MIXING, Z IS NON-ZERO.     C1X22890
C                                                                       C1X22900
150   A2=0.                                                             C1X22910
      A3=0.                                                             C1X22920
      CA=Z-H                                                            C1X22930
      CB=Z+H                                                            C1X22940
      C3=CA*CA/C2                                                       C1X22950
      C4=CB*CB/C2                                                       C1X22960
      IF (C3.GE.50.) GO TO 170                                          C1X22970
      A2=1./EXP(C3)                                                     C1X22980
170   IF (C4.GE.50.) GO TO 190                                          C1X22990
      A3=1./EXP(C4)                                                     C1X23000
C        WADE EQUATION 3.1.                                             C1X23010
190   RC=(A2+A3)/(2.5066*UPL*SZ*W)                                      C1X23020
      RETURN                                                            C1X23030
C                                                                       C1X23040
C->->->->SECTION RCP.E - UNSTABLE, ASSURED OF UNIFORM MIXING.           C1X23050
C                                                                       C1X23060
C        IF SIGMA-Z IS GREATER THAN 1.6 TIMES THE MIXING HEIGHT,        C1X23070
C         THE DISTRIBUTION BELOW THE MIXING HEIGHT IS UNIFORM WITH      C1X23080
C         HEIGHT REGARDLESS OF SOURCE HEIGHT OR RECEPTOR HEIGHT BECAUSE C1X23090
C         OF REPEATED EDDY REFLECTIONS FROM THE GROUND AND THE MIXING HTC1X23100
200   IF (SZ/HL.LE.1.6) GO TO 220                                       C1X23110
C        WADE EQUATION 3.5.                                             C1X23120
      RC=1./(UPL*HL*W)                                                  C1X23130
      RETURN                                                            C1X23140
C        INITIAL VALUE OF AN SET = 0.                                   C1X23150
C         AN - THE NUMBER OF TIMES THE SUMMATION TERM IS EVALUATED      C1X23160
C               AND ADDED IN.                                           C1X23170
220   AN=0.                                                             C1X23180
      IF (Z) 40,380,230                                                 C1X23190
C                                                                       C1X23200
C->->->->SECTION RCP.F - UNSTABLE, CALCULATE MULTIPLE EDDY              C1X23210
C                        REFLECTIONS, Z IS NON-ZERO.                    C1X23220
C                                                                       C1X23230
C       STATEMENTS 220-260 CALCULATE RC, THE RELATIVE CONCENTRATION,    C1X23240
C         USING THE EQUATION DISCUSSED ABOVE.  SEVERAL INTERMEDIATE     C1X23250
C         VARIABLES ARE USED TO AVOID REPEATING CALCULATIONS.           C1X23260
C         CHECKS ARE MADE TO BE SURE THAT THE ARGUMENT OF THE           C1X23270
C         EXPONENTIAL FUNCTION IS NEVER GREATER THAN 50 (OR LESS THAN   C1X23280
C         -50).                                                         C1X23290
C        CALCULATE MULTIPLE EDDY REFLECTIONS FOR RECEPTOR HEIGHT Z.     C1X23300
230   A1=1./(2.5066*UPL*SZ*W)                                           C1X23310
      C2=2.*SZ*SZ                                                       C1X23320
      A2=0.                                                             C1X23330
      A3=0.                                                             C1X23340
      CA=Z-H                                                            C1X23350
      CB=Z+H                                                            C1X23360
      C3=CA*CA/C2                                                       C1X23370
      C4=CB*CB/C2                                                       C1X23380
      IF (C3.GE.50.) GO TO 250                                          C1X23390
      A2=1./EXP(C3)                                                     C1X23400
250   IF (C4.GE.50.) GO TO 270                                          C1X23410
      A3=1./EXP(C4)                                                     C1X23420
270   SUM=0.                                                            C1X23430
      THL=2.*HL                                                         C1X23440
280   AN=AN+1.                                                          C1X23450
      A4=0.                                                             C1X23460
      A5=0.                                                             C1X23470
      A6=0.                                                             C1X23480
      A7=0.                                                             C1X23490
      C5=AN*THL                                                         C1X23500
      CC=CA-C5                                                          C1X23510
      CD=CB-C5                                                          C1X23520
      CE=CA+C5                                                          C1X23530
      CF=CB+C5                                                          C1X23540
      C6=CC*CC/C2                                                       C1X23550
      C7=CD*CD/C2                                                       C1X23560
      C8=CE*CE/C2                                                       C1X23570
      C9=CF*CF/C2                                                       C1X23580
      IF (C6.GE.50.) GO TO 300                                          C1X23590
      A4=1./EXP(C6)                                                     C1X23600
300   IF (C7.GE.50.) GO TO 320                                          C1X23610
      A5=1./EXP(C7)                                                     C1X23620
320   IF (C8.GE.50.) GO TO 340                                          C1X23630
      A6=1./EXP(C8)                                                     C1X23640
340   IF (C9.GE.50.) GO TO 360                                          C1X23650
      A7=1./EXP(C9)                                                     C1X23660
360   T=A4+A5+A6+A7                                                     C1X23670
      SUM=SUM+T                                                         C1X23680
      IF (T.GE.0.01) GO TO 280                                          C1X23690
      RC=A1*(A2+A3+SUM)                                                 C1X23700
      RETURN                                                            C1X23710
C                                                                       C1X23720
C->->->->SECTION RCP.G - UNSTABLE, CALCULATE MULTIPLE EDDY              C1X23730
C                        REFLECTIONS, Z IS ZERO.                        C1X23740
C                                                                       C1X23750
C        CALCULATE MULTIPLE EDDY REFLECTIONS FOR GROUND LEVEL RECEPTOR  C1X23760
C        HEIGHT.                                                        C1X23770
380   A1=1./(2.5066*UPL*SZ*W)                                           C1X23780
      A2=0.                                                             C1X23790
      C2=2.*SZ*SZ                                                       C1X23800
      C3=H*H/C2                                                         C1X23810
      IF (C3.GE.50.) GO TO 400                                          C1X23820
      A2=2./EXP(C3)                                                     C1X23830
400   SUM=0.                                                            C1X23840
      THL=2.*HL                                                         C1X23850
410   AN=AN+1.                                                          C1X23860
      A4=0.                                                             C1X23870
      A6=0.                                                             C1X23880
      C5=AN*THL                                                         C1X23890
      CC=H-C5                                                           C1X23900
      CE=H+C5                                                           C1X23910
      C6=CC*CC/C2                                                       C1X23920
      C8=CE*CE/C2                                                       C1X23930
      IF (C6.GE.50.) GO TO 430                                          C1X23940
      A4=2./EXP(C6)                                                     C1X23950
430   IF (C8.GE.50.) GO TO 450                                          C1X23960
      A6=2./EXP(C8)                                                     C1X23970
450   T=A4+A6                                                           C1X23980
      SUM=SUM+T                                                         C1X23990
      IF (T.GE.0.01) GO TO 410                                          C1X24000
      RC=A1*(A2+SUM)                                                    C1X24010
      RETURN                                                            C1X24020
C                                                                       C1X24030
C->->->->SECTION RCP.H - FORMAT                                         C1X24040
C                                                                       C1X24050
C***   SECTIONS OF SUBROUTINE RCP.                                      C1X24060
C         SECTION RCP.A -  COMMON.                                      C1X24070
C         SECTION RCP.B -  EXPLANATIONS AND COMPUTATIONS COMMON TO ALL  C1X24080
C                           CONDITIONS.                                 C1X24090
C         SECTION RCP.C -  STABLE OR UNLIMITED MIXING, Z IS ZERO.       C1X24100
C         SECTION RCP.D -  STABLE OR UNLIMITED MIXING, Z IS NON-ZERO.   C1X24110
C         SECTION RCP.E -  UNSTABLE, ASSURED OF UNIFORM MIXING.         C1X24120
C         SECTION RCP.F -  UNSTABLE, CALCULATE MULTIPLE EDDY            C1X24130
C                           REFLECTIONS; Z IS NON-ZERO.                 C1X24140
C         SECTION RCP.G -  UNSTABLE, CALCULATE MULTIPLE EDDY            C1X24150
C                           REFLECTIONS; Z IS ZERO.                     C1X24160
C         SECTION RCP.H -  FORMAT.                                      C1X24170
C                                                                       C1X24180
470   FORMAT (1HO,'BOTH H AND Z ARE ABOVE THE MIXING HEIGHT SO A RELIABLC1X24190
     1E COMPUTATION CAN NOT BE MADE.')                                  C1X24200
C                                                                       C1X24210
      END                                                               C1X24220
C
      SUBROUTINE CMPLX                                                  C1X24240
C                                                                       C1X24250
      COMMON /MPR/ UPL,Z,H,HL,X,Y,KST,DELH,SY,SZ,RC                     C1X24260
      COMMON /MPOR/ IOPT(27),ZMIN                                       C1X24270
      COMMON /MSL/ ER,EP,CELM,CTER,ICLM,ICALMP,VDIR(360)                C1X24280
C                                                                       C1X24290
C          THIS SUBROUTINE WAS CREATED 8 DEC 82 IN                      C1X24300
C          ORDER TO BETTER CONTROL THE TERRAIN                          C1X24310
C          TREATMENTS SPECIFIED BY IOPT(25)                             C1X24320
C                                                                       C1X24330
      ASSIGN 4500 TO K                                                  C1X24340
      IF(IOPT(25).EQ.1.AND.CTER.LT.0.001) ASSIGN 4000 TO K              C1X24350
      IF(IOPT(25).EQ.2) ASSIGN 3000 TO K                                C1X24360
      IF(IOPT(25).EQ.3) ASSIGN 2000 TO K                                C1X24370
      IF(IOPT(25).EQ.4) ASSIGN 1500 TO K                                C1X24380
      IF(IOPT(25).EQ.4.AND.CTER.LT.0.001) ASSIGN 1000 TO K              C1X24390
      IF(IOPT(25).EQ.5) ASSIGN 500 TO K                                 C1X24400
      RETURN                                                            C1X24410
C                                                                       C1X24420
C                                                                       C1X24430
      ENTRY CMPXRC                                                      C1X24440
C                                                                       C1X24450
C          STORE OFF VARIABLES                                          C1X24460
C                                                                       C1X24470
      ZA = Z                                                            C1X24480
      HA = H                                                            C1X24490
      HLA = HL                                                          C1X24500
C                                                                       C1X24510
C          COMPUTE THE MSL HEIGHT OF THE RECETOR INTAKE AND             C1X24520
C          THE FINAL EFFECTIVE PLUME HEIGHT (ASSUMING NO                C1X24530
C          TERRAIN EFFECTS).                                            C1X24540
C                                                                       C1X24550
      ZMSL = ER*CELM + ZA                                               C1X24560
      HEMSL = EP*CELM + HA                                              C1X24570
C                                                                       C1X24580
C          COMPUTE RECEPTOR HEIGHT ABOVE SOURCE (STACK)                 C1X24590
C          BASE ELEVATION IN METERS.                                    C1X24600
C                                                                       C1X24610
      DUM = (ER - EP)*CELM                                              C1X24620
      GO TO K                                                           C1X24630
C                                                                       C1X24640
C          IOPT(25) = 5 TERRAIN TREATMENT (PART ONE)                    C1X24650
C                                                                       C1X24660
  500 CONTINUE                                                          C1X24670
      IF(DUM.LT.0.0) THEN                                               C1X24680
C                                                                       C1X24690
C          IF DEPRESSED RECEPTOR, INCREASE EFF HT AND                   C1X24700
C          MIX HT                                                       C1X24710
C                                                                       C1X24720
      HL = HLA - DUM                                                    C1X24730
      H = HA - DUM                                                      C1X24740
      END IF                                                            C1X24750
C                                                                       C1X24760
C          PLACE INTAKE AT EFFECTIVE HT                                 C1X24770
C                                                                       C1X24780
      Z = H                                                             C1X24790
      CALL RCP                                                          C1X24800
C                                                                       C1X24810
C          STORE RELATIVE CONC. TEMPORARILY                             C1X24820
C                                                                       C1X24830
      RCA = RC                                                          C1X24840
C                                                                       C1X24850
C          RESTORE Z                                                    C1X24860
C                                                                       C1X24870
      Z = ZA                                                            C1X24880
C                                                                       C1X24890
C          GO ON TO IOPT(25) = 5 (PART TWO)                             C1X24900
C                                                                       C1X24910
      IF(CTER.GT.0.001) GO TO 1500                                      C1X24920
C                                                                       C1X24930
C                                                                       C1X24940
C          IOPT(25) = 4 TERRAIN TREATMENT                               C1X24950
C                                                                       C1X24960
 1000 CONTINUE                                                          C1X24970
C                                                                       C1X24980
C          SET MIX HGT SO NOT CONSIDERED WHEN CTER = 0.                 C1X24990
C                                                                       C1X25000
      HL = 5000.0                                                       C1X25010
      IF(DUM.LE.0.0) GO TO 1500                                         C1X25020
C                                                                       C1X25030
C          RECEPTOR ABOVE STACK BASE                                    C1X25040
C          SUBTRACT HGT DIFF FROM EFFECTIVE HEIGHT                      C1X25050
C                                                                       C1X25060
      H1 = HA - DUM                                                     C1X25070
C                                                                       C1X25080
C          CHOOSE MAX OF NEW EFF HT OR ZMIN                             C1X25090
C                                                                       C1X25100
      IF(H1.GT.ZMIN) THEN                                               C1X25110
      H = H1                                                            C1X25120
      GO TO 1600                                                        C1X25130
      END IF                                                            C1X25140
      H = ZMIN                                                          C1X25150
C                                                                       C1X25160
C          SET NEW Z TO AVOID LOCAL GRD LVL REFLECTION FROM             C1X25170
C          RECEPTORS ABOVE PLUME CENTERLINE                             C1X25180
C                                                                       C1X25190
      Z = ZMSL -(HEMSL - ZMIN)                                          C1X25200
      CALL RCP                                                          C1X25210
      IF(IOPT(25).EQ.5.AND.RC.GT.RCA)RC=RCA                             C1X25220
C                                                                       C1X25230
C          RESTORE ORIG. VALUES                                         C1X25240
C                                                                       C1X25250
      Z = ZA                                                            C1X25260
      H = HA                                                            C1X25270
      HL = HLA                                                          C1X25280
      RETURN                                                            C1X25290
 1500 CONTINUE                                                          C1X25300
C                                                                       C1X25310
C          CALC. TWO EFF HEIGHTS                                        C1X25320
C                                                                       C1X25330
      H1 = HA - DUM*(1.0 - CTER)                                        C1X25340
      H2 = HA*CTER                                                      C1X25350
C                                                                       C1X25360
C          CHOOSE LARGEST BETWEEN THE TWO CALC HTS.                     C1X25370
C                                                                       C1X25380
      IF(H1.LT.H2) H1 = H2                                              C1X25390
C                                                                       C1X25400
C          SET H TO LARGEST HT, CHOOSING BETWN H1 AND ZMIN              C1X25410
C                                                                       C1X25420
      IF(H1.LT.ZMIN) THEN                                               C1X25430
      H = ZMIN                                                          C1X25440
      ELSE                                                              C1X25450
      H = H1                                                            C1X25460
      END IF                                                            C1X25470
 1600 CALL RCP                                                          C1X25480
      IF(IOPT(25).EQ.5.AND.RC.GT.RCA) RC = RCA                          C1X25490
C                                                                       C1X25500
C          RESTORE ORIG. VALUES                                         C1X25510
C                                                                       C1X25520
      Z = ZA                                                            C1X25530
      H = HA                                                            C1X25540
      HL = HLA                                                          C1X25550
      RETURN                                                            C1X25560
C                                                                       C1X25570
C          IOPT(25) = 3 TERRAIN TREATMENT                               C1X25580
C                                                                       C1X25590
 2000 CONTINUE                                                          C1X25600
      IF(DUM.LT.0.0) THEN                                               C1X25610
C                                                                       C1X25620
C          IF DEPRESSED RECEPTOR, INCREASE EFF HT AND THE               C1X25630
C          MIX HGT                                                      C1X25640
C                                                                       C1X25650
      HL = HLA - DUM                                                    C1X25660
      H = HA - DUM                                                      C1X25670
      ELSE                                                              C1X25680
C                                                                       C1X25690
C          IF LEVEL OR ELEVATED RECEPTOR, CALC NEW INTAKE HT            C1X25700
C          AND MIX HT ABV STACK BASE                                    C1X25710
C                                                                       C1X25720
      HL = HLA + DUM                                                    C1X25730
      Z = ZA + DUM                                                      C1X25740
      END IF                                                            C1X25750
      CALL RCP                                                          C1X25760
C                                                                       C1X25770
C          RESTORE ORIG. VALUES                                         C1X25780
C                                                                       C1X25790
      Z = ZA                                                            C1X25800
      H = HA                                                            C1X25810
      HL = HLA                                                          C1X25820
      RETURN                                                            C1X25830
C                                                                       C1X25840
C          IOPT(25) = 2 TERRAIN TREATMENT (PART ONE)                    C1X25850
C                                                                       C1X25860
 3000 CONTINUE                                                          C1X25870
      IF(DUM.LT.0.0) THEN                                               C1X25880
C                                                                       C1X25890
C          IF DEPRESSED RECEPTOR, INCREASE EFF HT AND                   C1X25900
C          MIX HT                                                       C1X25910
C                                                                       C1X25920
      HL = HLA - DUM                                                    C1X25930
      H = HA - DUM                                                      C1X25940
      END IF                                                            C1X25950
C                                                                       C1X25960
C          PLACE INTAKE AT EFFECTIVE HT                                 C1X25970
C                                                                       C1X25980
      Z = H                                                             C1X25990
      CALL RCP                                                          C1X26000
C                                                                       C1X26010
C          STORE RELATIVE CONC. TEMPORARILY                             C1X26020
C                                                                       C1X26030
      RCA = RC                                                          C1X26040
C                                                                       C1X26050
C          RESTORE Z                                                    C1X26060
C                                                                       C1X26070
      Z = ZA                                                            C1X26080
C                                                                       C1X26090
C          GO ON TO IOPT(25) = 2 (PART TWO)                             C1X26100
C                                                                       C1X26110
      IF(CTER.GT.0.001) GO TO 4500                                      C1X26120
C                                                                       C1X26130
C          IOPT(25) = 1 TERRAIN TREATMENT                               C1X26140
C                                                                       C1X26150
 4000 CONTINUE                                                          C1X26160
      CORR = 1.0                                                        C1X26170
C                                                                       C1X26180
C          SET MX HT SO NOT CONSIDERED, WHEN CTER = 0.0                 C1X26190
C                                                                       C1X26200
      HL = 5000.0                                                       C1X26210
      IF(DUM.LT.0.0) GO TO 4500                                         C1X26220
C                                                                       C1X26230
C          IF DEPRESSED RECEPTOR, DETERMINE HEIGHT OF                   C1X26240
C          RECEPTOR INTAKE ABOVE PLUME CENTERLINE.                      C1X26250
C                                                                       C1X26260
      ZDUM = ZMSL - HEMSL                                               C1X26270
      IF(ZDUM.GE.400.0) THEN                                            C1X26280
C                                                                       C1X26290
C          IF MORE THAN 400 METERS ABOVE PLUME                          C1X26300
C          CENTERLINE, THEN CONC = 0.0                                  C1X26310
C                                                                       C1X26320
      CORR = 0.0                                                        C1X26330
      RC = 0.0                                                          C1X26340
      GO TO 5000                                                        C1X26350
      END IF                                                            C1X26360
      IF(ZMSL.LE.HEMSL) GO TO 4500                                      C1X26370
C                                                                       C1X26380
C          IF RECEPTOR INTAKE BETWEEN PLUME CENTERLINE                  C1X26390
C          AND 400 METERS ABOVE, CALC DROP-OFF FUNCTION.                C1X26400
C                                                                       C1X26410
      CORR = (400.0 - ZDUM)/400.0                                       C1X26420
      GO TO 4600                                                        C1X26430
 4500 CORR = 1.0                                                        C1X26440
C                                                                       C1X26450
C          CALC TWO EFF HEIGHTS                                         C1X26460
C                                                                       C1X26470
 4600 H1 = HA - DUM*(1.0 - CTER)                                        C1X26480
      H2 = HA*CTER                                                      C1X26490
C                                                                       C1X26500
C          CHOOSE LARGEST EFF. HT OF H1 AND H2.                         C1X26510
C                                                                       C1X26520
      IF(H1.LT.H2) H1 = H2                                              C1X26530
C                                                                       C1X26540
C          CHOOSE MAX OF NEW EFF. (H1) AND ZMIN                         C1X26550
C                                                                       C1X26560
      IF(H1.LT.ZMIN) THEN                                               C1X26570
      H = ZMIN                                                          C1X26580
      ELSE                                                              C1X26590
      H = H1                                                            C1X26600
      END IF                                                            C1X26610
      CALL RCP                                                          C1X26620
C                                                                       C1X26630
C          CORRECT (IF NECESSARY) FOR HEIGHT OF RECEPTOR INTAKE         C1X26640
C          RELATIVE TO THE PLUME CENTERLINE HEIGHT.                     C1X26650
C                                                                       C1X26660
      RC = RC*CORR                                                      C1X26670
C                                                                       C1X26680
C          IF IOPT(25) = 2, WE CHOOSE THE SMALLEST CONC BETWN           C1X26690
C          RC AND RCA                                                   C1X26700
C                                                                       C1X26710
 5000 IF(IOPT(25).EQ.2.AND.RC.GT.RCA) RC = RCA                          C1X26720
C                                                                       C1X26730
C          RESTORE ORIG. VALUES                                         C1X26740
C                                                                       C1X26750
      Z = ZA                                                            C1X26760
      H = HA                                                            C1X26770
      HL = HLA                                                          C1X26780
      RETURN                                                            C1X26790
      END                                                               C1X26800
C
      FUNCTION ANGARC (DELM,DELN)                                       C1X26820
C                                                                       C1X26830
C        DETERMINES APPROPRIATE ANGLE OF TAN(ANG) = DELM/DELN           C1X26840
C        WHICH IS REQUIRED FOR CALCULATION OF RESULTANT WIND DIRECTION. C1X26850
C        DELM IS THE AVERAGE WIND COMPONENT IN THE EAST DIRECTION.      C1X26860
C        DELN IS THE AVERAGE WIND COMPONENT IN THE NORTH DIRECTION.     C1X26870
C        NO COMMON REQUIREMENT, NO ARRAYS, USES LIBRARY FUNCTION ATAN   C1X26880
      IF (DELN) 10,40,80                                                C1X26890
10    IF (DELM) 20,30,20                                                C1X26900
20    ANGARC=57.29578*ATAN(DELM/DELN)+180.                              C1X26910
      RETURN                                                            C1X26920
30    ANGARC=180.                                                       C1X26930
      RETURN                                                            C1X26940
40    IF (DELM) 50,60,70                                                C1X26950
50    ANGARC=270.                                                       C1X26960
      RETURN                                                            C1X26970
60    ANGARC=0.                                                         C1X26980
C     ANGARC=0. INDICATES INDETERMINATE ANGLE                           C1X26990
      RETURN                                                            C1X27000
70    ANGARC=090.                                                       C1X27010
      RETURN                                                            C1X27020
80    IF (DELM) 90,100,110                                              C1X27030
90    ANGARC=57.29578*ATAN(DELM/DELN)+360.                              C1X27040
      RETURN                                                            C1X27050
100   ANGARC=360.                                                       C1X27060
      RETURN                                                            C1X27070
110   ANGARC=57.29578*ATAN(DELM/DELN)                                   C1X27080
      RETURN                                                            C1X27090
C                                                                       C1X27100
      END                                                               C1X27110
C
      SUBROUTINE OUTHR                                                  C1X27130
C                                                                       C1X27140
C        THIS SUBROUTINE PROVIDES OUTPUT CONCENTRATIONS IN              C1X27150
C        MICROGRAMS PER CUBIC METER FOR EACH HOUR IN TWO WAYS:          C1X27160
C           1) CONTRIBUTIONS FROM SIGNIFICANT SOURCES, AND              C1X27170
C           2) SUMMARIES.                                               C1X27180
C        BEYOND ENTRY POINT OUTAVG THE SUBROUTINE PROVIDES              C1X27190
C        CONCENTRATION OUTPUT FOR EACH AVERAGING PERIOD AGAIN           C1X27200
C        IN THE ABOVE MANNER.                                           C1X27210
C                                                                       C1X27220
C->->->->SECTION OUTHR.A - COMMON, DIMENSION, AND DATA.                 C1X27230
C                                                                       C1X27240
      COMMON /MPOR/ IOPT(27),ZMIN                                       C1X27250
      COMMON /MSL/ ER,EP,CELM,CTER,ICLM,ICALMP,VDIR(360)                C1X27260
      COMMON /MPO/ NRECEP,NAVG,NB,LH,NPT,IDATE(2),RREC(180),SREC(180),ZRC1X27270
     1(180),ELR(180),PHCHI(180),PHSIGS(180,26),HSAV(250),DSAV(250),PCHI(C1X27280
     2180),PSIGS(180,26),IPOL                                           C1X27290
      COMMON /MO/ QTHETA(24),QU(24),IKST(24),QHL(24),QTEMP(24),MPS(25), C1X27300
     1            NSIGP,IO,IRANK(180)                                   C1X27310
      COMMON /MOC/LINE1(20),LINE2(20),LINE3(20),RNAME(2,180),STAR(5,180)C1X27320
      CHARACTER*4 RNAME,IPOLT,IPOLU,LINE1,LINE2,LINE3
      CHARACTER*1 STAR
      DIMENSION IPOLT(2)                                                C1X27330
      DATA IPOLT /'SO2 ','PART'/                                        C1X27340
      IPOLU=IPOLT(1)                                                    C1X27350
      IF (IPOL.EQ.4) IPOLU=IPOLT(2)                                     C1X27360
C        OPTION(11): PRINT ONLY THE HOURLY SUMMARIES.                   C1X27370
      IF (IOPT(11).EQ.1) GO TO 100                                      C1X27380
C                                                                       C1X27390
C->->->->SECTION OUTHR.B - WRITE HOURLY CONTRIBUTION TITLE.             C1X27400
C                                                                       C1X27410
      WRITE (IO,350) LINE1,LINE2,LINE3                                  C1X27420
      IF(IOPT(27) .EQ. 1) THEN                                          C1X27430
        WRITE(IO,359) IPOLU,VDIR(LH)                                    C1X27440
      ELSE                                                              C1X27450
      WRITE (IO,360)IPOLU,IDATE,LH                                      C1X27460
      END IF                                                            C1X27470
C                                                                       C1X27480
C->->->->SECTION OUTHR.C - WRITE HOURLY MET DATA.                       C1X27490
C                                                                       C1X27500
      IF (IOPT(12).EQ.1) GO TO 10                                       C1X27510
      WRITE (IO,450)                                                    C1X27520
      WRITE (IO,460) LH,QTHETA(LH),QU(LH),QHL(LH),QTEMP(LH),IKST(LH)    C1X27530
C                                                                       C1X27540
C->->->->SECTION OUTHR.D - WRITE FINAL PLUME HEIGHT AND DISTANCE        C1X27550
C                          FINAL RISE.                                  C1X27560
C                                                                       C1X27570
10    IF (IOPT(13).EQ.1) GO TO 20                                       C1X27580
      WRITE (IO,470) (I,I=1,10)                                         C1X27590
C        HSAV ARE THE CALCULATED PLUME HEIGHTS FOR THIS HOUR            C1X27600
      WRITE (IO,480) (HSAV(I),I=1,NPT)                                  C1X27610
      WRITE (IO,490) (DSAV(I),I=1,NPT)                                  C1X27620
C                                                                       C1X27630
C->->->->SECTION OUTHR.E - WRITE HRLY SIGNIGICANT SOURCE CONTRIB.       C1X27640
C                                                                       C1X27650
20    IF (NSIGP.GT.10) GO TO 40                                         C1X27660
C        PRINT FIRST PAGE OF OUTPUT AND TOTALS FOR 10 OR LESS SIGNIF SOUC1X27670
      WRITE (IO,370)                                                    C1X27680
      WRITE (IO,380) (I,I=1,NSIGP)                                      C1X27690
      WRITE (IO,390)                                                    C1X27700
      WRITE (IO,380) (MPS(I),I=1,NSIGP)                                 C1X27710
      WRITE (IO,400)                                                    C1X27720
      DO 30 K=1,NRECEP                                                  C1X27730
      WRITE (IO,410) K,STAR(1,K),STAR(2,K),(PHSIGS(K,I),I=1,NSIGP)      C1X27740
C        PRINT TOTALS                                                   C1X27750
      WRITE (IO,420) PHSIGS(K,26),PHCHI(K)                              C1X27760
30    CONTINUE                                                          C1X27770
      GO TO 100                                                         C1X27780
C        PRINT FIRST PAGE FOR MORE THAN 10 SIGNIFICANT SOURCES.         C1X27790
40    WRITE (IO,370)                                                    C1X27800
      WRITE (IO,380) (I,I=1,10)                                         C1X27810
      WRITE (IO,430) (MPS(I),I=1,10)                                    C1X27820
      WRITE (IO,400)                                                    C1X27830
      DO 50 K=1,NRECEP                                                  C1X27840
50    WRITE (IO,410) K,STAR(1,K),STAR(2,K),(PHSIGS(K,I),I=1,10)         C1X27850
      IF (NSIGP.GT.20) GO TO 70                                         C1X27860
C        PRINT SECOND PAGE AND TOTALS FOR 11 TO 20 SIGNIFICANT SOURCES  C1X27870
      WRITE (IO,350) LINE1,LINE2,LINE3                                  C1X27880
      IF(IOPT(27) .EQ. 1) THEN                                          C1X27890
        WRITE(IO,359) IPOLU,VDIR(LH)                                    C1X27900
      ELSE                                                              C1X27910
      WRITE (IO,360)IPOLU,IDATE,LH                                      C1X27920
      END IF                                                            C1X27930
      WRITE (IO,370)                                                    C1X27940
      WRITE (IO,380) (I,I=11,NSIGP)                                     C1X27950
      WRITE (IO,390)                                                    C1X27960
      WRITE (IO,380) (MPS(I),I=11,NSIGP)                                C1X27970
      WRITE (IO,400)                                                    C1X27980
      DO 60 K=1,NRECEP                                                  C1X27990
      WRITE (IO,410) K,STAR(1,K),STAR(2,K),(PHSIGS(K,I),I=11,NSIGP)     C1X28000
60    WRITE (IO,420) PHSIGS(K,26),PHCHI(K)                              C1X28010
      GO TO 100                                                         C1X28020
C        WRITE SECOND PAGE FOR MORE THAN 20 SIGNIFICANT SOURCES.        C1X28030
70    WRITE (IO,350) LINE1,LINE2,LINE3                                  C1X28040
      IF(IOPT(27) .EQ. 1) THEN                                          C1X28050
        WRITE(IO,359) IPOLU,VDIR(LH)                                    C1X28060
      ELSE                                                              C1X28070
      WRITE (IO,360)IPOLU,IDATE,LH                                      C1X28080
      END IF                                                            C1X28090
      WRITE (IO,370)                                                    C1X28100
      WRITE (IO,380) (I,I=11,20)                                        C1X28110
      WRITE (IO,430) (MPS(I),I=11,20)                                   C1X28120
      WRITE (IO,400)                                                    C1X28130
      DO 80 K=1,NRECEP                                                  C1X28140
80    WRITE (IO,410) K,STAR(1,K),STAR(2,K),(PHSIGS(K,I),I=11,20)        C1X28150
      WRITE (IO,350) LINE1,LINE2,LINE3                                  C1X28160
      IF(IOPT(27) .EQ. 1) THEN                                          C1X28170
        WRITE(IO,359) IPOLU,VDIR(LH)                                    C1X28180
      ELSE                                                              C1X28190
      WRITE (IO,360)IPOLU,IDATE,LH                                      C1X28200
      END IF                                                            C1X28210
      WRITE (IO,370)                                                    C1X28220
C        WRITE LAST PAGE AND TOTALS FOR MORE THAN 20 SIGNIF. SOURCES.   C1X28230
      WRITE (IO,380) (I,I=21,NSIGP)                                     C1X28240
      WRITE (IO,390)                                                    C1X28250
      WRITE (IO,380) (MPS(I),I=21,NSIGP)                                C1X28260
      WRITE (IO,400)                                                    C1X28270
      DO 90 K=1,NRECEP                                                  C1X28280
      WRITE (IO,410) K,STAR(1,K),STAR(2,K),(PHSIGS(K,I),I=21,NSIGP)     C1X28290
90    WRITE (IO,420) PHSIGS(K,26),PHCHI(K)                              C1X28300
C        OPTION(14): SKIP OUTPUT OF THE HOURLY SUMMARIES.               C1X28310
100   IF (IOPT(14).EQ.1) GO TO 170                                      C1X28320
C                                                                       C1X28330
C->->->->SECTION OUTHR.F - WRITE HOURLY SUMMARY TITLE.                  C1X28340
C                                                                       C1X28350
      WRITE (IO,350) LINE1,LINE2,LINE3                                  C1X28360
      IF(IOPT(27) .EQ. 1) THEN                                          C1X28370
        WRITE(IO,439) IPOLU,VDIR(LH)                                    C1X28380
      ELSE                                                              C1X28390
      WRITE (IO,440)IPOLU,IDATE,LH                                      C1X28400
      END IF                                                            C1X28410
C                                                                       C1X28420
C->->->->SECTION OUTHR.G - WRITE HOURLY MET DATA.                       C1X28430
C                                                                       C1X28440
      IF (IOPT(15).EQ.1) GO TO 110                                      C1X28450
      WRITE (IO,450)                                                    C1X28460
      WRITE (IO,460) LH,QTHETA(LH),QU(LH),QHL(LH),QTEMP(LH),IKST(LH)    C1X28470
C                                                                       C1X28480
C->->->->SECTION OUTHR.H - WRITE FINAL PLUME HEIGHT AND                 C1X28490
C        DISTANCE TO FINAL RISE.                                        C1X28500
C                                                                       C1X28510
110   IF (IOPT(16).EQ.1) GO TO 120                                      C1X28520
      WRITE (IO,470) (I,I=1,10)                                         C1X28530
C        HSAV ARE THE CALCULATED PLUME HEIGHTS FOR THIS HOUR            C1X28540
      WRITE (IO,480) (HSAV(I),I=1,NPT)                                  C1X28550
      WRITE (IO,490) (DSAV(I),I=1,NPT)                                  C1X28560
C                                                                       C1X28570
C->->->->SECTION OUTHR.I - WRITE HOURLY SUMMARY TABLE.                  C1X28580
C                                                                       C1X28590
120   WRITE (IO,500)                                                    C1X28600
C        CALCULATE GRAND TOTALS AND RANK CONCENTRATIONS                 C1X28610
      DO 130 K=1,NRECEP                                                 C1X28620
C        HSAV IS USED AS A DUMMY VARIABLE FOR THE REMAINDER OF THIS     C1X28630
C        SUBROUTINE. IT IS ZEROED AGAIN IN PTR BEFORE ITS NORMAL USE.   C1X28640
130   HSAV(K)=PHCHI(K)                                                  C1X28650
C        DETERMINE RANKING ACCORDING TO CONCENTRATION                   C1X28660
      DO 150 I=1,NRECEP                                                 C1X28670
      CMAX=-1.0                                                         C1X28680
      DO 140 K=1,NRECEP                                                 C1X28690
      IF (HSAV(K).LE.CMAX) GO TO 140                                    C1X28700
      CMAX=HSAV(K)                                                      C1X28710
      LMAX=K                                                            C1X28720
140   CONTINUE                                                          C1X28730
      IRANK(LMAX)=I                                                     C1X28740
      HSAV(LMAX)=-1.0                                                   C1X28750
150   CONTINUE                                                          C1X28760
      DO 160 K=1,NRECEP                                                 C1X28770
      WRITE (IO,510) K,STAR(1,K),STAR(2,K),(RNAME(J,K),J=1,2),RREC(K),SRC1X28780
     1EC(K),ZR(K),ELR(K),PHSIGS(K,26),PHCHI(K),IRANK(K)                 C1X28790
160   CONTINUE                                                          C1X28800
170   RETURN                                                            C1X28810
C                                                                       C1X28820
C->->->->SECTION OUTHR.J - ENTRY POINT FOR AVERAGING TIME               C1X28830
C                                                                       C1X28830
      ENTRY OUTAVG                                                      C1X28850
C            AT THIS ENTRY POINT, CONCENTRATION OUTPUT                  C1X28860
C        IN MICROGRAMS PER CUBIC METER ARE PRINTED FOR THE              C1X28870
C        AVERAGING PERIOD. CONTRIBUTIONS AND/OR SUMMARY                 C1X28880
C        INFORMATION IS AVAILABLE.                                      C1X28890
C        AVERAGE CONCENTRATIONS OVER SPECIFIED TIME PERIOD              C1X28900
      NAVGP=MAX(INT(.75*NAVG+.99),(NAVG-ICALMP))                        IBM28910
      DO 190 K=1,NRECEP                                                 C1X28920
      PCHI(K)=PCHI(K)/NAVGP                                             C1X28930
      HSAV(K)=PCHI(K)                                                   C1X28940
      DO 180 I=1,26                                                     C1X28950
180   PSIGS(K,I)=PSIGS(K,I)/NAVGP                                       C1X28960
190   CONTINUE                                                          C1X28970
C        OPTION(17): SKIP OUTPUT OF THE AVERAGED CONTRIBUTIONS.         C1X28980
      IF (IOPT(17).EQ.1) GO TO 270                                      C1X28990
C->->->->SECTION OUTHR.K - WRITE AVERAGING-TIME SIGNIFICANT             C1X29000
C                  SOURCE CONTRIBUTIONS.                                C1X29010
      WRITE (IO,350) LINE1,LINE2,LINE3                                  C1X29020
      WRITE (IO,520) NAVG,IPOLU,IDATE,NB                                C1X29030
      IF (NSIGP.GT.10) GO TO 210                                        C1X29040
C        PRINT FIRST PAGE OF OUTPUT AND TOTALS FOR 10 OR LESS SIGNIF-   C1X29050
C        ICANT SOURCES                                                  C1X29060
      WRITE (IO,380) (I,I=1,NSIGP)                                      C1X29070
      WRITE (IO,390)                                                    C1X29080
      WRITE (IO,380) (MPS(I),I=1,NSIGP)                                 C1X29090
      WRITE (IO,400)                                                    C1X29100
      DO 200 K=1,NRECEP                                                 C1X29110
      WRITE (IO,410) K,STAR(1,K),STAR(2,K),(PSIGS(K,I),I=1,NSIGP)       C1X29120
C        PRINT TOTALS                                                   C1X29130
      WRITE (IO,420) PSIGS(K,26),PCHI(K)                                C1X29140
200   CONTINUE                                                          C1X29150
      GO TO 270                                                         C1X29160
C        PRINT FIRST PAGE FOR MORE THAN 10 SIGNIF SOURCES               C1X29170
210   WRITE (IO,380) (I,I=1,10)                                         C1X29180
      WRITE (IO,430) (MPS(I),I=1,10)                                    C1X29190
      WRITE (IO,400)                                                    C1X29200
      DO 220 K=1,NRECEP                                                 C1X29210
220   WRITE (IO,410) K,STAR(1,K),STAR(2,K),(PSIGS(K,I),I=1,10)          C1X29220
      IF (NSIGP.GT.20) GO TO 240                                        C1X29230
C        PRINT SECOND PAGE AND TOTALS FOR 11 TO 20 SIGNIF SOURCES       C1X29240
      WRITE (IO,350) LINE1,LINE2,LINE3                                  C1X29250
      WRITE (IO,520) NAVG,IPOLU,IDATE,NB                                C1X29260
      WRITE (IO,380) (I,I=11,NSIGP)                                     C1X29270
      WRITE (IO,390)                                                    C1X29280
      WRITE (IO,380) (MPS(I),I=11,NSIGP)                                C1X29290
      WRITE (IO,400)                                                    C1X29300
      DO 230 K=1,NRECEP                                                 C1X29310
      WRITE (IO,410) K,STAR(1,K),STAR(2,K),(PSIGS(K,I),I=11,NSIGP)      C1X29320
230   WRITE (IO,420) PSIGS(K,26),PCHI(K)                                C1X29330
      GO TO 270                                                         C1X29340
C        WRITE SECOND PAGE FOR MORE THAN 20 SIGNIF SOURCES              C1X29350
240   WRITE (IO,350) LINE1,LINE2,LINE3                                  C1X29360
      WRITE (IO,520) NAVG,IPOLU,IDATE,NB                                C1X29370
      WRITE (IO,380) (I,I=11,20)                                        C1X29380
      WRITE (IO,430) (MPS(I),I=11,20)                                   C1X29390
      WRITE (IO,400)                                                    C1X29400
      DO 250 K=1,NRECEP                                                 C1X29410
250   WRITE (IO,410) K,STAR(1,K),STAR(2,K),(PSIGS(K,I),I=11,20)         C1X29420
      WRITE (IO,350) LINE1,LINE2,LINE3                                  C1X29430
      WRITE (IO,520) NAVG,IPOLU,IDATE,NB                                C1X29440
C        WRITE LAST PAGE AND TOTALS FOR MORE THAN 20 SIGNIF SOURCES     C1X29450
      WRITE (IO,380) (I,I=21,NSIGP)                                     C1X29460
      WRITE (IO,390)                                                    C1X29470
      WRITE (IO,380) (MPS(I),I=21,NSIGP)                                C1X29480
      WRITE (IO,400)                                                    C1X29490
      DO 260 K=1,NRECEP                                                 C1X29500
      WRITE (IO,410) K,STAR(1,K),STAR(2,K),(PSIGS(K,I),I=21,NSIGP)      C1X29510
260   WRITE (IO,420) PSIGS(K,26),PCHI(K)                                C1X29520
C                                                                       C1X29530
C->->->->SECTION OUTHR.L - WRITE AVERAGING-TIME SUMMARY.                C1X29540
C                                                                       C1X29550
C        OPTION(18): SKIP OUTPUT OF THE AVERAGED SUMMARIES.             C1X29560
270   IF (IOPT(18).EQ.1) GO TO 310                                      C1X29570
      WRITE (IO,350) LINE1,LINE2,LINE3                                  C1X29580
      WRITE (IO,530) NAVG,IPOLU,IDATE,NB                                C1X29590
      WRITE (IO,500)                                                    C1X29600
C        CALCULATE GRAND TOTALS AND RANK CONCENTRATIONS                 C1X29610
      DO 290 I=1,NRECEP                                                 C1X29620
      CMAX=-1.0                                                         C1X29630
      DO 280 K=1,NRECEP                                                 C1X29640
      IF (HSAV(K).LE.CMAX) GO TO 280                                    C1X29650
      CMAX=HSAV(K)                                                      C1X29660
      LMAX=K                                                            C1X29670
280   CONTINUE                                                          C1X29680
      IRANK(LMAX)=I                                                     C1X29690
      HSAV(LMAX)=-1.0                                                   C1X29700
290   CONTINUE                                                          C1X29710
      DO 300 K=1,NRECEP                                                 C1X29720
      WRITE (IO,510) K,STAR(1,K),STAR(2,K),(RNAME(J,K),J=1,2),RREC(K),SRC1X29730
     1EC(K),ZR(K),ELR(K),PSIGS(K,26),PCHI(K),IRANK(K)                   C1X29740
300   CONTINUE                                                          C1X29750
310   IF (IOPT(24).EQ.0) GO TO 330                                      C1X29760
C        PUNCH CONCENTRATIONS FOR CONTOURING(MICROGRAMS/CUBIC METER)    C1X29770
C        RECEPTOR COORDINATES IN USER UNITS.                            C1X29780
      DO 320 K=1,NRECEP                                                 C1X29790
      GWU=PCHI(K)*1.0E+06                                               C1X29800
      WRITE (IO,540) RREC(K),SREC(K),GWU,K,ZR(K),ELR(K)                 C1X29810
      WRITE (1,540) RREC(K),SREC(K),GWU,K,ZR(K),ELR(K)                  C1X29820
320   CONTINUE                                                          C1X29830
330   IF (IOPT(23).EQ.0) GO TO 340                                      C1X29840
C        WRITE PERIODIC CONC. TO DISK/TAPE - FOR LONG-TERM APPLICATION  C1X29850
C        FOR EACH RUN, THIS WRITE STATEMENT WILL GENERATE               C1X29860
C        'NPER' RECORDS.                                                C1X29870
      WRITE (13) IDATE(2),NB,(PCHI(K),K=1,NRECEP)                       C1X29880
340   RETURN                                                            C1X29890
C                                                                       C1X29900
C->->->->SECTION OUTHR.M - FORMATS.                                     C1X29910
C                                                                       C1X29920
C***   SECTIONS OF SUBROUTINE OUTHR.                                    C1X29930
C         SECTION OUTHR.A -  COMMON, DIMENSION, AND DATA.               C1X29940
C         SECTION OUTHR.B -  WRITE HOURLY CONTRIBUTION TITLE.           C1X29950
C         SECTION OUTHR.C -  WRITE HOURLY MET. DATA.                    C1X29960
C         SECTION OUTHR.D -  WRITE FINAL PLUME HEIGHT AND DISTANCE TO   C1X29970
C                             FINAL RISE.                               C1X29980
C         SECTION OUTHR.E -  WRITE HOURLY SIGNIFICANT SOURCE CONTRIB.   C1X29990
C         SECTION OUTHR.F -  WRITE HOURLY SUMMARY TITLE.                C1X30000
C         SECTION OUTHR.G -  WRITE HOURLY MET. DATA.                    C1X30010
C         SECTION OUTHR.H -  WRITE FINAL PLUME HEIGHT AND DISTANCE TO   C1X30020
C                             FINAL RISE.                               C1X30030
C         SECTION OUTHR.I -  WRITE HOURLY SUMMARY TABLE.                C1X30040
C         SECTION OUTHR.J -  ENTRY POINT FOR AVERAGING TIME.            C1X30050
C         SECTION OUTHR.K -  WRITE AVERAGING-TIME SIGNIFICANT SOURCE    C1X30060
C                             CONTRIBUTIONS.                            C1X30070
C         SECTION OUTHR.L -  WRITE AVERAGING-TIME SUMMARY.              C1X30080
C         SECTION OUTHR.M -  FORMATS.                                   C1X30090
C                                                                       C1X30100
350   FORMAT ('1',20A4/1X,20A4/1X,20A4)                                 C1X30110
 359  FORMAT('0',T5,'VALLEY:',T14,A4,' 24-HR AVERAGE CONTRIBUTION '     C1X30120
     1   ,'(MICROGRAMS/M**3) '                                          C1X30130
     2,'FROM SIGNIFICANT POINT SOURCES ',5X,'(WIND DIRECTION (DEG) = ', C1X30140
     3     F5.1,')')                                                    C1X30150
360   FORMAT('0',T30,A4,' CONTRIBUTION(MICROGRAMS/M**3) FROM SIGNIFICANTC1X30160
     1 POINT SOURCES ',5X,I2,'/',I4,' : HOUR ',I2//)                    C1X30170
370   FORMAT (1H0,T5,'RANK')                                            C1X30180
380   FORMAT ('+',T12,10(I3,7X))                                        C1X30190
390   FORMAT ('+',T113,'TOTAL     TOTAL'/1X,T113,'SIGNIF    ALL POINT'/1C1X30200
     1X,T113,'POINT     SOURCES'/1X,'SOURCE #')                         C1X30210
400   FORMAT (1X,'RECEP #')                                             C1X30220
410   FORMAT (1X,I3,2A1,6P,10F10.3)                                     C1X30230
420   FORMAT ('+',T109,6P,2F10.3)                                       C1X30240
430   FORMAT (1X,'SOURCE #',T12,10(I3,7X))                              C1X30250
439   FORMAT('0',T9,'VALLEY:',T19,A4,' 24-HR AVERAGE SUMMARY ',         C1X30260
     1 'CONCENTRATION TABLE',                                           C1X30270
     2' (MICROGRAMS/M**3) ',5X,'(WIND DIRECTION (DEG) = ',F5.1,')')     C1X30280
440   FORMAT('0',T25,A4,' SUMMARY CONCENTRATION TABLE(MICROGRAMS/M**3) 'C1X30290
     1,5X,I2,'/',I4,' : HOUR ',I2/1X)                                   C1X30300
450   FORMAT (1X,T2,'HOUR   THETA    SPEED   MIXING   TEMP   STABILITY'/C1X30310
     11X,T9,'(DEG)    (M/S) HEIGHT(M)   (K)     CLASS'/1X)              C1X30320
460   FORMAT (1X,T3,I2,4F9.2,6X,I1//)                                   C1X30330
470   FORMAT (13X,10I11)                                                C1X30340
480   FORMAT (' FINAL HT (M) ',10F11.2)                                 C1X30350
490   FORMAT (' DIST FIN HT (KM)',10F11.3)                              C1X30360
500   FORMAT ('0',T7,'RECEPTOR',T23,'EAST',T33,'NORTH',T43,'RECEPTOR HT'C1X30370
     1,T61,'RECEPTOR',T78,'TOTAL FROM',T93,'TOTAL FROM',T106,'CONCENTRATC1X30380
     2ION'/' ',T7,'NO. NAME',T22,'COORD',T33,'COORD',T44,'ABV GRD (M)',TC1X30390
     359,'GRD-LVL ELEV',T77,'SIGNIF POINT',T93,'ALL SOURCES',T111,'RANK'C1X30400
     4/' ',T58,'(USER HT UNITS)',T80,'SOURCES'//)                       C1X30410
510   FORMAT (1H ,I8,2A1,2X,2A4,2F10.2,F12.1,F20.1,6P,2F15.4,I15)       C1X30420
520   FORMAT ('0',T22,I2,'-HOUR AVERAGE ',A4,' CONTRIBUTION(MICROGRAMS/MC1X30430
     1**3) FROM SIGNIFICANT POINT SOURCES',5X,I2,'/',I3,'  START HOUR: 'C1X30440
     2,I2//1X,T5,'RANK')                                                C1X30450
530   FORMAT ('0',T25,I2,'-HOUR AVERAGE ',A4,' SUMMARY CONCENTRATION TABC1X30460
     1LE(MICROGRAMS/M**3)',5X,I2,'/',I3,'  START HOUR: ',I2//1X)        C1X30470
540   FORMAT ('CNTL',1X,3F10.3,20X,I4,2F10.1)                           C1X30480
C                                                                       C1X30490
      END                                                               C1X30500
C
      SUBROUTINE PGZ                                                    C1X30520
C                                                                       C1X30530
C        VERTICAL DISPERSION PARAMETER VALUE, SZ, DETERMINED BY         C1X30540
C        SZ = A * X**B      WHERE A AND B ARE FUNCTIONS OF              C1X30550
C        BOTH STABILITY AND RANGE OF X.                                 C1X30560
      COMMON /MPR/ UPL,Z,H,HL,X,Y,KST,DELH,SY,SZ,RC                     C1X30570
      DIMENSION XA(7), XB(2), XD(5), XE(8), XF(9), AA(8), BA(8), AB(3), C1X30580
     1BB(3), AD(6), BD(6), AE(9), BE(9), AF(10), BF(10)                 C1X30590
      DATA XA /.5,.4,.3,.25,.2,.15,.1/                                  C1X30600
      DATA XB /.4,.2/                                                   C1X30610
      DATA XD /30.,10.,3.,1.,.3/                                        C1X30620
      DATA XE /40.,20.,10.,4.,2.,1.,.3,.1/                              C1X30630
      DATA XF /60.,30.,15.,7.,3.,2.,1.,.7,.2/                           C1X30640
      DATA AA /453.85,346.75,258.89,217.41,179.52,170.22,158.08,122.8/  C1X30650
      DATA BA /2.1166,1.7283,1.4094,1.2644,1.1262,1.0932,1.0542,.9447/  C1X30660
      DATA AB /109.30,98.483,90.673/                                    C1X30670
      DATA BB /1.0971,0.98332,0.93198/                                  C1X30680
      DATA AD /44.053,36.650,33.504,32.093,32.093,34.459/               C1X30690
      DATA BD /0.51179,0.56589,0.60486,0.64403,0.81066,0.86974/         C1X30700
      DATA AE /47.618,35.420,26.970,24.703,22.534,21.628,21.628,23.331,2C1X30710
     14.26/                                                             C1X30720
      DATA BE /0.29592,0.37615,0.46713,0.50527,0.57154,0.63077,0.75660,0C1X30730
     1.81956,0.8366/                                                    C1X30740
      DATA AF /34.219,27.074,22.651,17.836,16.187,14.823,13.953,13.953,1C1X30750
     14.457,15.209/                                                     C1X30760
      DATA BF /0.21716,0.27436,0.32681,0.41507,0.46490,0.54503,0.63227,0C1X30770
     1.68465,0.78407,0.81558/                                           C1X30780
      GO TO (10,40,70,80,110,140), KST                                  C1X30790
C        STABILITY A                                                    C1X30800
10    IF (X.GT.3.11) GO TO 170                                          C1X30810
      DO 20 ID=1,7                                                      C1X30820
      IF (X.GE.XA(ID)) GO TO 30                                         C1X30830
20    CONTINUE                                                          C1X30840
      ID=8                                                              C1X30850
30    SZ=AA(ID)*X**BA(ID)                                               C1X30860
      RETURN                                                            C1X30870
C        STABILITY B                                                    C1X30880
40    IF (X.GT.35.) GO TO 170                                           C1X30890
      DO 50 ID=1,2                                                      C1X30900
      IF (X.GE.XB(ID)) GO TO 60                                         C1X30910
50    CONTINUE                                                          C1X30920
      ID=3                                                              C1X30930
60    SZ=AB(ID)*X**BB(ID)                                               C1X30940
      GO TO 180                                                         C1X30950
C        STABILITY C                                                    C1X30960
70    SZ=61.141*X**0.91465                                              C1X30970
      GO TO 180                                                         C1X30980
C        STABILITY D                                                    C1X30990
80    DO 90 ID=1,5                                                      C1X31000
      IF (X.GE.XD(ID)) GO TO 100                                        C1X31010
90    CONTINUE                                                          C1X31020
      ID=6                                                              C1X31030
100   SZ=AD(ID)*X**BD(ID)                                               C1X31040
      GO TO 180                                                         C1X31050
C        STABILITY E                                                    C1X31060
110   DO 120 ID=1,8                                                     C1X31070
      IF (X.GE.XE(ID)) GO TO 130                                        C1X31080
120   CONTINUE                                                          C1X31090
      ID=9                                                              C1X31100
130   SZ=AE(ID)*X**BE(ID)                                               C1X31110
      GO TO 180                                                         C1X31120
C        STABILITY F                                                    C1X31130
140   DO 150 ID=1,9                                                     C1X31140
      IF (X.GE.XF(ID)) GO TO 160                                        C1X31150
150   CONTINUE                                                          C1X31160
      ID=10                                                             C1X31170
160   SZ=AF(ID)*X**BF(ID)                                               C1X31180
      GO TO 180                                                         C1X31190
170   SZ=5000.                                                          C1X31200
      RETURN                                                            C1X31210
180   IF (SZ.GT.5000.) SZ=5000.                                         C1X31220
      RETURN                                                            C1X31230
C                                                                       C1X31240
      END                                                               C1X31250
C
      SUBROUTINE RANK (L)                                               C1X31270
C                                                                       C1X31280
C        CALLED BY COMPLEX II TO ARRANGE CONCENTRATIONS OF VARIOUS AVG  C1X31290
C        TIMES INTO HIGH-FIVE TABLES...THAT IS, ARRAYS STORING          C1X31300
C        THE HIGHEST FIVE CONCENTRATIONS FOR EACH RECEPTOR FOR          C1X31310
C        EACH AVG TIME.                                                 C1X31320
C        VARIABLES OUTPUT:                                              C1X31330
C             HMAXA(J,K,L)  CONCENTRATIONS ACCORDING TO                 C1X31340
C                   J : RANK OF CONC. (1-5)                             C1X31350
C                   K : RECEPTOR NUMBER                                 C1X31360
C                   L : AVG TIME                                        C1X31370
C             NDAY(J,K,L) : ASSOCIATED DAY OF CONC.                     C1X31380
C             IHR(J,K,L) : ENDING HOUR OF CONC.                         C1X31390
      COMMON/MR/HMAXA(5,180,5),NDAY(5,180,5),IHR(5,180,5),CONC(180,5),  C1X31400
     1          JDAY,NR                                                 C1X31410
      COMMON /MPO/ NRECEP,NAVG,NB,LH,NPT,IDATE(2),RREC(180),SREC(180),ZRC1X31420
     1(180),ELR(180),PHCHI(180),PHSIGS(180,26),HSAV(250),DSAV(250),PCHI(C1X31430
     2180),PSIGS(180,26),IPOL                                           C1X31440
      COMMON /MPOR/ IOPT(27),ZMIN                                       C1X31450
      IO=6                                                              C1X31460
C       RESET THE VARIABLE 'L' FOR CALM HOURS PASSED THROUGH FROM THE   C1X31470
C       CALL STATEMENT IN THE MAIN PROGRAM.                             C1X31480
C                                                                       C1X31490
C        IF IOPT(19) WAS SELECTED, RETURN TO THE MAIN PROGRAM           C1X31500
      IF (IOPT(19) .EQ. 1) RETURN                                       C1X31510
      LL=0                                                              C1X31520
      IF (L .GT. 5) LL=1                                                C1X31530
      IF (L .EQ. 22) L=2                                                C1X31540
      IF (L .EQ. 33) L=3                                                C1X31550
      IF (L .EQ. 44) L=4                                                C1X31560
      IF (L .EQ. 55) L=5                                                C1X31570
      DO 50 K=1,NRECEP                                                  C1X31580
      IF (CONC(K,L).LE.HMAXA(5,K,L)) GO TO 50                           C1X31590
      DO 10 J=1,5                                                       C1X31600
      IF (CONC(K,L).GT.HMAXA(J,K,L)) GO TO 20                           C1X31610
C           CONCENTRATION IS ONE OF THE TOP FIVE                        C1X31620
10    CONTINUE                                                          C1X31630
      WRITE (IO,70)                                                     C1X31640
      GO TO 50                                                          C1X31650
C        THE FOLLOWING DO-LOOP HAS THE EFFECT OF INSERTING A NEW        C1X31660
C        CONCENTRATION ENTRY INTO ITS PROPER POSITION WHILE SHIFTING    C1X31670
C        DOWN THE 'OLD' LOWER CONCENTRATIONS THUS ESTABLISHING THE      C1X31680
C        'HIGH-FIVE' CONCENTRATION TABLE.                               C1X31690
20    IF (J.EQ.5) GO TO 40                                              C1X31700
      DO 30 IJ=4,J,-1                                                   C1X31710
      IJP1=IJ+1                                                         C1X31720
      HMAXA(IJP1,K,L)=HMAXA(IJ,K,L)                                     C1X31730
      NDAY(IJP1,K,L) = NDAY(IJ,K,L)                                     C1X31740
30    IHR(IJP1,K,L) = IHR(IJ,K,L)                                       C1X31750
C           INSERT LATEST CONC, DAY AND ENDING HR INTO THE              C1X31760
C           PROPER RANK IN THE HIGH-FIVE TABLE                          C1X31770
40    HMAXA(J,K,L)=CONC(K,L)                                            C1X31780
      NDAY(J,K,L) = JDAY                                                C1X31790
      IHR(J,K,L) = LH                                                   C1X31800
C        IF A CALM HOUR IS ENCOUNTERED, ADD 100 TO THE HOUR             C1X31810
      IF (LL .EQ. 1 .AND. L .NE. 1) IHR(J,K,L)=IHR(J,K,L)+100.          C1X31820
50    CONTINUE                                                          C1X31830
      DO 60 K=1,NRECEP                                                  C1X31840
      CONC(K,L)=0.                                                      C1X31850
60    CONTINUE                                                          C1X31860
      RETURN                                                            C1X31870
C                                                                       C1X31880
70    FORMAT (1X,'   ****ERROR IN FINDING THE MAX CONCENTRATION***')    C1X31890
C                                                                       C1X31900
      END                                                               C1X31910
C
      BLOCK DATA                                                        C1X31930
C                                                                       C1X31940
C***COEFFICIENTS GENERATED WITH RURAL SIGMAS USING PGYZ                 C1X31950
C***RELATIVE CONC. NORMALIZED FOR WIND SPEED FROM PT SOURCE, CHI*U/Q, = C1X31960
C***     PXUCOF(KST,IH)*H**PXUEXP(KST,IH)                               C1X31970
C***   IH=1 FOR H LESS THAN 20 METERS.                                  C1X31980
C***   IH=2 FOR H FROM 20 TO 30 METERS.                                 C1X31990
C***   IH=3 FOR H FROM 30 TO 50 METERS.                                 C1X32000
C***   IH=4 FOR H FROM 50 TO 70 METERS.                                 C1X32010
C***   IH=5 FOR H FROM 70 TO 100 METERS.                                C1X32020
C***   IH=6 FOR H FROM 100 TO 200 METERS.                               C1X32030
C***   IH=7 FOR H FROM 200 TO 300 METERS.                               C1X32040
C***   IH=8 FOR H FROM 300 TO 500 METERS.                               C1X32050
C***   IH=9 FOR H GREATER THAN 500 METERS.                              C1X32060
      COMMON /EXPOS/ PXUCOF(6,9),PXUEXP(6,9),HC1(10)                    C1X32070
      DATA PXUCOF /.10401E+00,.12133E+00,.14273E+00,.15351E+00,.18855E+0C1X32080
     10,.18668E+00,.77533E-01,.11728E+00,.14120E+00,.18239E+00,.20458E+0C1X32090
     20,.34326E+00,.67228E-01,.10013E+00,.13963E+00,.19162E+00,.38998E+0C1X32100
     30,.76271E+00,.40484E-01,.75308E-01,.13784E+00,.54357E+00,.72550E+0C1X32110
     40,.22936E+01,.28539E-01,.66936E-01,.13615E+00,.52790E+00,.12908E+0C1X32120
     51,.56943E+01,.14792E-01,.65799E-01,.13315E+00,.74832E+00,.28818E+0C1X32130
     61,.40940E+03,.12403E-01,.64321E-01,.12927E+00,.10826E+01,.77020E+0C1X32140
     72,.23011E+05,.12340E-01,.62874E-01,.12546E+00,.15580E+01,.68810E+0C1X32150
     83,.46522E+06,.12245E-01,.60615E-01,.11952E+00,.22517E+01,.42842E+0C1X32160
     93,.00000E+00/                                                     C1X32170
      DATA PXUEXP /-.19460E+01,-.19774E+01,-.20086E+01,-.20742E+01,-.218C1X32180
     122E+01,-.22176E+01,-.18479E+01,-.19661E+01,-.20050E+01,-.21317E+01C1X32190
     2,-.22094E+01,-.24209E+01,-.18060E+01,-.19196E+01,-.20017E+01,-.214C1X32200
     362E+01,-.23991E+01,-.26556E+01,-.16763E+01,-.18468E+01,-.19984E+01C1X32210
     4,-.24128E+01,-.25578E+01,-.29371E+01,-.15940E+01,-.18191E+01,-.199C1X32220
     555E+01,-.24059E+01,-.26934E+01,-.31511E+01,-.14513E+01,-.18153E+01C1X32230
     6,-.19907E+01,-.24817E+01,-.28678E+01,-.40795E+01,-.14181E+01,-.181C1X32240
     711E+01,-.19851E+01,-.25514E+01,-.34879E+01,-.48399E+01,-.14172E+01C1X32250
     8,-.18071E+01,-.19799E+01,-.26152E+01,-.38719E+01,-.53670E+01,-.141C1X32260
     960E+01,-.18012E+01,-.19721E+01,-.26744E+01,-.37956E+01,-.17020E+02C1X32270
     A/                                                                 C1X32280
      DATA HC1 /10.,20.,30.,50.,70.,100.,200.,300.,500.,1000./          C1X32290
C                                                                       C1X32300
      END                                                               C1X32310
C                                                                       IBM
      BLOCK DATA ONE                                                    IBM
C                                                                       IBM
      COMMON /MO/ QTHETA(24),QU(24),IKST(24),QHL(24),QTEMP(24),MPS(25), XXX
     1            NSIGP,IO,IRANK(180)                                   XXX
      COMMON /MOC/LINE1(20),LINE2(20),LINE3(20),RNAME(2,180),STAR(5,180)XXX
C                                                                       IBM
      CHARACTER*1 STAR
      CHARACTER*4 LINE1,LINE2,LINE3,RNAME
      DATA STAR /900*' '/                                               IBM
C                                                                       IBM
        END                                                             IBM
