      PROGRAM PLUVUE                                                    PLV00010
C     SAVE                                                              PLV00020
C **********************************************************************PLV00030
C                      PLUVUE II VERSION 13222   Level 1.0              PLV00040
C                                                                       PLV00050
C            ***    SEE PLUVUE2 MODEL CHANGE BULLETIN     ***           PLV00060
C                                                                       PLV00070
C    ON THE SUPPORT CENTER FOR REGULATORY AIR MODELS BULLETIN BOARD     PLV00080
C                                                                       PLV00090
C               http://www.epa.gov/ttn/scram/comments.htm               PLV00100
C                                                                       PLV00110
C **********************************************************************PLV00120
C                                                                       PLV00130
C                                                                       PLV00140
C                             PLUVUE II                                 PLV00150
C                                                                       PLV00160
C                       PLUME VISIBILITY MODEL                          PLV00170
C                                                                       PLV00180
C     THIS MODEL WAS DEVELOPED FOR :                                    PLV00190
C                                                                       PLV00200
C                 U. S. ENVIRONMENTAL PROTECTION AGENCY                 PLV00210
C                   RESEARCH TRIANGLE PARK, NC 27111                    PLV00220
C                                                                       PLV00230
C     BY                                                                PLV00240
C                                                                       PLV00250
C                          DOUGLAS A. LATIMER                           PLV00260
C                         ROBERT W. BERGSTROM                           PLV00270
C                           CLARK D. JOHNSON                            PLV00280
C                              HENRY HOGO                               PLV00290
C                          CHRISTIAN SEIGNEUR                           PLV00300
C                                                                       PLV00310
C                      SYSTEMS APPLICATIONS, INC.                       PLV00320
C                        101 LUCAS VALLEY ROAD                          PLV00330
C                         SAN RAFAEL, CA 94903                          PLV00340
C                                                                       PLV00350
C                            (415) 472-4011                             PLV00360
C                                                                       PLV00370
C IT IS RECOMMENDED THAT THE USER OF THIS MODEL REFER TO EPA'S          PLV00380
C SERIES OF TECHNICAL GUIDANCE DOCUMENTS ON VISIBILITY, INCLUDING       PLV00390
C THE "WORKBOOK FOR ESTIMATING VISIBILITY IMPAIRMENT" AND THE "USERS    PLV00400
C MANUAL FOR THE PLUME VISIBILITY MODEL (PLUVUE II)", AND THE           PLV00410
C FOLLOWING DOCUMENTS:                                                  PLV00420
C                                                                       PLV00430
C      (1) U. S. ENVIRONMENTAL PROTECTION AGENCY (OCTOBER,1979),        PLV00440
C          "PROTECTING VISIBILITY:  AN EPA REPORT TO CONGRESS",         PLV00450
C          EPA-450/5-79-008.                                            PLV00460
C                                                                       PLV00470
C      (2) LATIMER, D. A. ET AL. (SEPTEMBER,1978), "DEVELOPMENT         PLV00480
C          OF MATHEMATICAL MODELS FOR THE PREDICTION OF ANTHROPOGEN     PLV00490
C          VISIBILITY IMPAIRMENT",EPA-450/3-78-110A,B,C, SYSTEMS        PLV00500
C          APPLICATIONS, INCORPORATED, SAN RAFAEL, CALIFORNIA.          PLV00510
C                                                                       PLV00520
C                                                                       PLV00530
C **********************************************************************PLV00540
C
C                    PLUVUE II (VERSION 13222) 
C
C     Modified: August 10, 2013
C
C     Modified by:  Peter Eckhoff
C                   US EPA
C
C     The primary goal of this modification was to enable this model to
C     run on Windows 64-bit operating systems. Additional enhancements
C     are noted in the README file that accompanies these revisions 
C     along with modifications to the operating instructions.
C
C     The input and output files have been hard coded to read the files: 
C       PLUVUE2.INP
C       PLUVUE2.OUT  <-- will be overwritten upon next execution
C
C     If you have any questions, please direct them through our scram
C     web site page found in the main title section above.
C
C **********************************************************************PLV00541
C                                                                       PLV00550
C                    PLUVUE II (VERSION 96170)                          PLV00560
C                    AN AIR QUALITY DISPERSION MODEL IN                 PLV00570
C                    SECTION 2.  NON-GUIDELINE MODELS                   PLV00580
C                                                                       PLV00590
C                                                                       PLV00600
C                                                                       PLV00610
C                                                                       PLV00620
C **********************************************************************PLV00630
C                                                                       PLV00640
C  Note: The implementation of the Faugeras parameters was disabled in
C        Version 92243 and remains disabled in the current version.
C
C    THIS VERSION OF PLUVUE II CALCULATES FAUGERAS MODEL PARAMETERS AS  PLV00650
C    DEVELOPED BY:                                                      PLV00660
C                       JOHN COLLINS                                    PLV00670
C                       ERT INC                                         PLV00680
C                                                                       PLV00690
C    THE REVISIONS WERE MADE ON 8/11/83 TO THE ORIGINAL PLUVUE CODE.    PLV00700
C                                                                       PLV00710
C    NOTE: CHANGES WERE INCORPORATED INTO PLUVUE II BY:                 PLV00720
C                                                                       PLV00730
C                       ED CARR                                         PLV00740
C                       H. HOGO                                         PLV00750
C                                                                       PLV00760
C                       SYSTEMS APPLICATIONS, INC                       PLV00770
C                                                                       PLV00780
C                       FEBRUARY 1988                                   PLV00790
C                                                                       PLV00800
C **********************************************************************PLV00810
C                                                                       PLV00820
C    PROGRAM MODIFICATION BY RICHARD LOOKER SAI - OCTOBER 1991          PLV00830
C    THE COMPUTATION OF THE MULTIPLE SCATTERING CONTRIBUTION TO THE     PLV00840
C    SPECTRAL RADIANCE WAS CORRECTED IN THE MODULE PLMOBJ.  A PORTION OFPLV00850
C    THE TOTAL MULTIPLE SCATTERING INTEGRAL WAS MISSING FROM THE CODE.  PLV00860
C                                                                       PLV00870
C *******************************************************************   PLV00880
C                                                                       PLV00890
C   COSMETIC CHANGES WERE MADE TO PROGRAM STRUCTURE BY G E MOORE OF     PLV00900
C   SIGMA RESEARCH CORPORATION AS OF JUNE 1992.  UNUSED VARIABLES AND   PLV00910
C   PRINT STATEMENTS WERE ELIMINATED TO REDUCE THE PROGRAM SIZE.        PLV00920
C   ADDITIONAL COMMENTS,BORDERS, AND LOOP INDENTATIONS WERE ADDED FOR   PLV00930
C   CODE LEGIBILITY. SOME PROGRAMMING KLUGES (SUCH AS gwl'S OPENING OF  PLV00940
C   UNIT 10) THAT DO NOT IMPACT THE NUMERICS WERE RECTIFIED.            PLV00950
C                                                                       PLV00960
C   SUBSTANTIATIVE CHANGES WERE MADE TO THE CODE IN THE FOLLOWING AREAS PLV00970
C                                                                       PLV00980
C   1) SZPAS ALGORITHM WAS REPLACED WITH THE ISCST EQUIVALENT FOR       PLV00990
C      CONSISTENCY AND ACCURACY (THE ORIGINAL WAS IN ERROR FOR STABILITYPLV01000
C      CLASS A) - NOTE THE SYPAS ALGORITHM WAS ALSO REPLACED            PLV01010
C                                                                       PLV01020
C   2) TAUP3 IS NOT ALLOWED TO GO NEGATIVE DUE TO RO < RP               PLV01030
C                                                                       PLV01040
C   3) THE ABILITY TO CHANGE STABILITY CLASS WITH DOWNWIND DISTANCE     PLV01050
C      WAS DISABLED BY SETTING NXSTAB TO NX2+1 AND INEW TO I.           PLV01060
C      NXSTAB AND INEW ARE NO LONGER READ IN.                           PLV01070
C                                                                       PLV01080
C   4) THIS VERSION RUNS WITH THE PLUVUE II MODELING SYSTEM CALLED      PLV01090
C      RUNPLUVU.  THE INPUT FILENAME IS ENTERED INTERACTIVELY VIA THE   PLV01100
C      RUNPLUVU SYSTEM.  IT IS PASSED TO PLUVUE II VIA A SCRATCH FILE   PLV01110
C      (SEE SUBROUTINE OPENA).  THE OUTPUT FILE IS CALLED SCRATCH3      PLV01120
C      WHICH IS RENAMED IN RUNPLUVU TO A SELECTED FILENAME.             PLV01130
C                                                                       PLV01140
C   5) CHANGED SURFACE ANEMOMETER HEIGHT FROM 7 M TO 10 M FOR           PLV01150
C      WIND SPEED CALCULATION                                           PLV01160
C                                                                       PLV01170
C   THE ABOVE CHANGES WERE IMPLEMENTED AND TESTED BY G MOORE            PLV01180
C   AND D DICRISTOFARO OF SIGMA RESEARCH CORPORATION.                   PLV01190
C                                                                       PLV01210
C********************************************************************** PLV01220
C
C   Additional changes in the code were made in December 1994 and May 1996 by
C   
C               L. Willard Richards and Marcelo E. Korc
C                       Sonoma Technology, Inc.
C                    5510 Skylane Blvd., Suite 101
C                        Santa Rosa, CA  95403
C                        voice: (707) 527-9372
C                         fax: (707) 527-9398
C                     E-mail: will@sonomatech.com
C   
C   These changes:
C     -   Corrected an error in line BAC00620 of BACOBJ.
C     -   Provided realistic reflectances of 0.1 and 0.9
C         instead of 0.0 and 1.0 for black and white terrain
C         backgrounds.
C     -   Added the calculation of the contrast of a plume against
C         the white, gray, or black backgrounds assuming the eye is
C         accommodated to the radiance of the horizon sky.
C     -   Added diffuse skylight to the illumination of the
C         reference white and the terrain backgrounds in
C         subroutine BACOBJ (See comments after line BAC00600).
C     -   Corrected errors related to the calculation of the
C         fraction of the primary particles that are carbon.
C     -   Avoided errors in some Lahey FORTRAN compilers by adding
C         1 min to all times exactly on the hour.
C    
C   ****************************************************************
c   ****************************************************************
c
c Further changes made June 1996 by John Vimont, National Park Service
c  These include:
c  1) changing the 1 minute add-on listed above to an inital
c     check at the beginning of the program and stopping the program if
c     the compiler is having this problem.
c  2) Inserting a check on the zenith angle and preventing it from
c     exceeding ZMAX (currently set at 88 degrees).  If zenith is
c     greater than ZMAX then the program loops on time until the zenith
c     angle is greater than ZMAX. (It adds one minute at a time in the
c     morning and subracts one minute at a time in the evening.)  It
c     does not allow this to go beyond one hour.  A warning message
c     is printed if this occurs.
c     (Note: ZMAX is specified in a data statement at beginning of
c     the main program)
c  3) A message is printed in the output if non-horizontal sky views
c     are specified and the angle BETA is less than 5.  The program
c     does not calculate non-horizontal vies for BETA less than 5 and
c     this can lead to some confusion by users.
c  4) Let the direct solar flux on an object, in subroutine BACOBJ,
c     be represented by FD0 rather than FDH.
c **********************************************************************
C                                                                       PLV01230
C*** ADD TYPE STATEMENTS                                                PLV01240
c     CHARACTER*5 CTIME                                                 PLV01245
      REAL FA(256),FC1(256),FC2(256),FB(256),FF(256)                    PLV01250
      REAL FFF(256), FCC(256)                                           PLV01260
      DOUBLE PRECISION FDWK1(256), FDWK2(256)                           PLV01270
      DOUBLE PRECISION DCWK(256,2)                                      PLV01280
C*** END TYPE STATEMENT ADDITIONS                                       PLV01290
      COMMON/BCKGND/ ELEV,RVAMB,ACCAMB,AMBNO2,RH,ROVA,ROVC,ROVS,ROVP,   PLV01300
     1SIGA,SIGC,SIGS,SIGP,HPBL,IREAD,CORAMB,AMBNO3,AMBSO4,INTYP         PLV01310
     2,DENA,DENC,DENP,DENS,ROVCAR,SIGCAR,DENCAR,AMBCAR                  PLV01320
      DIMENSION SPECR(39),SPECB(39),SPECO(39),SPECP(39)                 PLV01330
      COMMON/MIESCT/ROG,SIGMA,NLAMB,LAMB(20),JX,IT,TT(200),DUM(20),     PLV01340
     1PDUM(20,50),DUMA(20),DUMG(20)                                     PLV01350
      COMMON / OPTDEP / TAT0IZ(39),TATHIZ(39),TAT0HZ(39),X1(39),        PLV01360
     1X2(39),TAUTDI(39),TAUT0D(39),XHDI(39),XH0D(39)                    PLV01370
      COMMON/ MISC/ ABSNO2(39),SOLAR(39),RAD,FORPIN,OMZ(39),OMH(39)     PLV01380
     1,NTHETA,RFR,RFI                                                   PLV01390
      COMMON / RAIN / ITEST,IASD                                        PLV01400
      COMMON / ISAACS / IRE                                             PLV01410
      COMMON/INDREF/RFRSO4,RFISO4,RFRCOR,RFICOR,RFRPRM,RFIPRM           PLV01420
     1,RFRCAR,RFICAR                                                    PLV01430
      COMMON/RADPRP/BTASO4(39),BTACOR(39),BTAPRM(39),BTAAER(39),        PLV01440
     1PAER(39,27),PPRIM(39,27),PSO4(39,27),PCOR(39,27),BTABAC(39)       PLV01450
     2,PCAR(39,27),ABSCAR(39),BTACAR(39),ABSSO4(39),ABSCOR(39),         PLV01460
     3 ABSPRM(39),GSO4(39),GCOR(39),GPRM(39),GCAR(39),GAER(39)          PLV01470
     4,GBAC(39),PBAC(39,27)                                             PLV01480
      COMMON/COLOR/YCAP,VAL,X,Y,YCAPD,VALD,XD,YD,DELUV,DELAB,           PLV01490
     1XBAR(39),YBAR(39),ZBAR(39),PI,CONT1,CONT2,CONT3,BRATIO            PLV01500
      COMMON /SOL/ EFFDEC, HRANGL                                       PLV01510
      COMMON /MAX/ PLUMEC                                               PLV01520
C                                                                       PLV01530
      DIMENSION        AA(4),DIST(16),                                  PLV01540
     1XNO2I(16,6),QNO2I(6),XPARTP(16,6),XPARTS(16,6)                    PLV01550
      DIMENSION ROBJ(7),REFL(3)                                         PLV01560
      DIMENSION XQ(6),CHIQIY(6),RSO2(17)                                PLV01570
      DIMENSION TER(16),OBSPLU(16),AZMUTH(16),AALPHA(16),ABETA(16)      PLV01580
     1,ROBJT(16),ROBJCT(24)                                             PLV01590
      DIMENSION PLT1(16,4),PLT2(16,4),PLT3(16,4),PLT4(16,4),PLT33(16,4),PLV01600
     1PLOT1(16,4),PLOT2(16,4),PLOT3(16,4),PLOT4(16,4),PLOT33(16,4)      PLV01610
      DIMENSION RSO2R(17,6,17),RNOXR(17,6,17),QSO2TR(6,16),QNOXTR(6,16),PLV01620
     1 QSO4TR(6,16),QNO3TR(6,16),QPARTT(6,16),RATIOT(6,16),PHIKKR(24),  PLV01630
     1 QJ(24),RNO2X(17,6,17)                                            PLV01640
       CHARACTER*4 PLANT(6),ALT(6)                                      PLV01650
       CHARACTER*2 STABLE(7)                                            PLV01660
      REAL INTSKY                                                       PLV01665
C                                                                       PLV01670
C  IF NUMBER OF DOWNWIND DISTANCES > 16, INCREASE ABOVE DIMENSIONS.     PLV01680
C                                                                       PLV01690
      DIMENSION P(7)                                                    PLV01700
      DATA P/0.10,0.15,0.20,0.25,0.30,0.30,0.30/                        PLV01710
      DATA AA/30.,45.,60.,90./                                          PLV01720
      DATA ALT /'H+2S','H+1S','H','H-1S','H-2S',' 0'/                   PLV01730
      DATA STABLE/' A',' B',' C',' D',' E',' F',' G'/                   PLV01740
      DATA NX1 /1/                                                      PLV01750
      DATA ROBJ/0.02,0.05,0.10,0.20,0.50,0.80,1.0/                      PLV01760
C     Code modification to provide a realistic reflectance for white and
C     black backgrounds.
C     Will Richards and Marcelo Korc, STI, December 28, 1994
      DATA REFL/0.9, 0.3, 0.1/                                          PLV01770
c add variables for later use in zenith angle check jcv NPS 6/18/96
	data zmax/88.0/
	logical timflg/.false./
c***
	INTSKY = -99.                     
c*** John Vimont (JCV) National Park Service (NPS)6/18/96
c Various versions of the LAHEY FORTRAN compiler have had bugs which
c   cause problems in FUNCTION CLOCK.  Will do a test call here to
c   deterimine if the compiled version of this code produces the
c   bogus result.  (Ultimately it dramatically affects the zenith
c   angle calculation.) The value of clck should be 558 after this test.
	i = -2
	time = 600.
	clck = clok(time,i)
	if (clck.ne.558.) then
	    print *,'The compiler used to compile this program'
	    print *,'  is yielding spurious results in function clock.'
	    print *,'  Contact the compiler developer for a fix.'
	    stop
	endif
c*** End JCV modification
C                                                                       PLV01780
C  CALL SUBROUTINE TO OPEN FILES                                        PLV01790
C                                                                       PLV01800
      CALL OPENA                                                        PLV01810
C                                                                       PLV01820
C  PRINT OUT HEADERS                                                    PLV01830
C                                                                       PLV01840
      WRITE(6,120)                                                      PLV01850
120   FORMAT(1H1,T23,'       PLUVUE II (VERSION 13222)',/,              PLV01860
     1    T23,'    AN AIR QUALITY DISPERSION MODEL IN',/,               PLV01870
     2    T23,'        THE OTHER MODELS SECTION       ',/,              PLV01880
     3    T23,'EPA SUPPORT CENTER FOR REGULATORY AIR MODELS',/)         PLV01890
      PI=3.14159                                                        PLV01900
      RAD=PI/180.                                                       PLV01910
C                                                                       PLV01920
C **********************************************************************PLV01930
C                                                                       PLV01940
C READ IN DATA (METEOROLOGICAL, PLANT EMISSIONS, AND AMBIENT AIR QUALITYPLV01950
C                                                                       PLV01960
C **********************************************************************PLV01970
C                                                                       PLV01980
99    CONTINUE                                                          PLV01990
      READ(5,1) (PLANT(J),J=1,6)                                        PLV02000
1     FORMAT (6A4)                                                      PLV02010
C                                                                       PLV02020
C READ IN U(MPH), STABILITY INDEX, LAPSE RATE(DEG F/1000 FT).  I IS     PLV02030
C STABILITY INDEX.  FOR PASQUILL-GIFFORD STABILITY CLASSES,             PLV02040
C I=1 FOR "A", 2 FOR "B", 3 FOR "C", ETC.                               PLV02050
C                                                                       PLV02060
      READ (5,2) U,I,ALAPSE                                             PLV02070
2     FORMAT (F5.1,I5,F5.2,F5.1)                                        PLV02080
      IS=I                                                              PLV02090
C                                                                       PLV02100
C   IUSFC=0 FOR WIND SPEED ALOFT, NON-ZERO FOR SURFACE.                 PLV02110
C   INEW=NEW STABILITY INDEX. NXSTAB IS DOWN WIND DISTANCE INDEX WHERE  PLV02120
C   NEW STABILITY STARTS.  SET NXSTAB TO NX2+1 AND INEW=I FOR NO        PLV02130
C   STABILITY CHANGE.                                                   PLV02140
C                                                                       PLV02150
c*** Modification by D.DiCristofaro, Sigma Research                     PLV02160
c*** Eliminate stability change with distance option by                 PLV02170
c*** not reading INEW AND NXSTAB                                        PLV02180
c*** Set INEW TO I AND NXSTAB TO NX2 (AFTER NX2 HAS BEEN INPUT)         PLV02190
                                                                        PLV02200
c     READ(5,2001)IUSFC,INEW,NXSTAB                                     PLV02210
      READ(5,2001)IUSFC                                                 PLV02220
c*** End of Modification                                                PLV02230
C                                                                       PLV02240
C  READ INITIAL PLUME HORIZONTAL AND VERTICAL DIMENSIONS                PLV02250
C  FOR A NON-POINT SOURCE.                                              PLV02260
C                                                                       PLV02270
      READ(5,9)YINITL,ZINITL                                            PLV02280
C                                                                       PLV02290
C   READ IN PLANETARY BOUNDARY HEIGHT (MIXING DEPTH) IN METERS.         PLV02300
C   THIS IS THE LIMIT ON VERTICAL MIXING OF EMISSIONS.  IF SET TO ZERO, PLV02310
C   THERE IS NO LIMIT.                                                  PLV02320
C                                                                       PLV02330
      READ(5,7) HPBLM                                                   PLV02340
C                                                                       PLV02350
C  SET PBL HEIGHT TO 2 KM FOR BACKGROUND POLLUTANTS FOR OPTICS          PLV02360
C  CALCULATIONS ONLY.                                                   PLV02370
C                                                                       PLV02380
      HPBL=2.                                                           PLV02390
C                                                                       PLV02400
C   READ IN RELATIVE HUMIDITY IN PERCENT                                PLV02410
C                                                                       PLV02420
      READ(5,8) RH                                                      PLV02430
C                                                                       PLV02440
C   READ INDICATOR FOR TYPE OF STABILITY SCHEME.  IDIS=0 FOR PASQUILL-  PLV02450
C  GIFFORD, 1 FOR TVA.                                                  PLV02460
C                                                                       PLV02470
      READ(5,801) IDIS                                                  PLV02480
C                                                                       PLV02490
C    FLAGS FOR OPTICS ANALYSIS ROUTINES.  IFLG1=1 FOR HORIZONTAL SIGHT  PLV02500
C    PATH VIEWS.  IFLG2=1 FOR NON-HORIZONTAL SIGHT PATHS.  IFLG3=1 FOR  PLV02510
C    BACKGROUND OBJECT VIEWS.  IFLG4=1 FOR SIGHT PATHS ALONG THE PLUME  PLV02520
C    CENTERLINE. NT1= STARTING INDEX FOR SCATTERING ANGLE ARRAY FOR     PLV02530
C    RADIATIVE TRANSFER AND SCATTERING CALCULATIONS.  NT2= ENDING INDEX PLV02540
C    FOR SCATTERRING ANGLE CALCULATIONS.  NORMALLY, NT1 IS SET TO 1     PLV02550
C    AND NT2 IS SET TO 7 FOR THE 7 GENERIC SCATTERING ANGLES AND        PLV02560
C    THESE VALUES SHOULD BE USED WHENEVER A OBSERVER-BASED RUN (NC1=1)  PLV02570
C    IS BEING MADE.  FOR A GENERIC CASE RUN, NT1 AND NT2 CAN BE SET     PLV02580
C    TO LIMIT CALCULATIONS FOR LESS THAN THE 7 ANGLES. EXAMPLE:  FOR    PLV02590
C    A GENERIC RUN FOR CALCULATIONS AT 90 DEGREES ONLY, NT1=3 AND       PLV02600
C    NT2=4.  NX2= ENDING INDEX FOR ARRAY OF DOWNWIND DISTANCES OF       PLV02610
C    POINTS FOR OPTICS CALCULATIONS.  NZF=1 FOR CALCULATIONS AT         PLV02620
C    PLUME CENTERLINE ONLY, 2 FOR OPTICS CALCULATIONS AT PLUME          PLV02630
C    CENTERLINE AND AT GROUND LEVEL.                                    PLV02640
C                                                                       PLV02650
C*** ADD THREE VARIABLES TO READ STATEMENT 00138: NX3,NX4,NX5           PLV02660
C*** AND CHANGE FORMAT STATEMENT 00139 FROM (10I2) TO (11I2)            PLV02670
      READ(5,2001) IFLG1,IFLG2,IFLG3,IFLG4,NX2,NT1,NT2,NZF,NX3,NX4,NX5  PLV02680
2001  FORMAT(11I2)                                                      PLV02690
                                                                        PLV02700
c*** Modification by D.DiCristofaro, Sigma to eliminate                 PLV02710
c    option of changing stability class with downwind distance          PLV02720
      NXSTAB=NX2+1                                                      PLV02730
c*** End of Modification                                                PLV02740
C                                                                       PLV02750
C  SWITCH FOR TURNING ON PRINT OUT OF TABLE FOR INITIAL PLUME DILUTION. PLV02760
C  IDILU=0 FOR NO TABLE, 1 FOR TABLE.                                   PLV02770
C                                                                       PLV02780
C*** ADD VARIABLE I1HFAU,I1DFAU,I2FAU,I3FAU,I4FAU  TO READ STATEMENT 200PLV02790
C*** I1HFAU NUMBER OF HUNDREDS OF POINTS FOR FOURIER ANALYSIS           PLV02800
C*** I1DFAU NUMBER OF DIGITS   OF POINTS FOR FOURIER ANALYSIS           PLV02810
C*** I1FAU NUMBER OF POINTS FOR FOURIER ANALYSIS=100*I1HFAU+I1DFAU      PLV02820
C*** I2FAU STEPPING INTERVAL FOR PRINTOUT OF SPATIAL DATA               PLV02830
C*** I3FAU PRINTOUT OPTIONS -1 INDIVIDUAL CHANNEL PRINTOUTS             PLV02840
C*** I3FAU                  +1 COMBINED PRINTOUT                        PLV02850
C*** I3FAU                   0 BOTH THE ABOVE                           PLV02860
C*** I4FAU FORTRAN CHANNEL NUMBER FOR PRINTED OUTPUT                    PLV02870
      READ(5,2001)IDILU,I1HFAU,I1DFAU,I2FAU,I3FAU,I4FAU                 PLV02880
      I1FAU= 100*I1HFAU + I1DFAU                                        PLV02890
C                                                                       PLV02900
C  READ TEST INDEX FOR BSIZE OPTIMIZATION                               PLV02910
C                                                                       PLV02920
      ITEST = 0                                                         PLV02930
      IASD = 0                                                          PLV02940
      IRE=1                                                             PLV02950
C                                                                       PLV02960
C   READ IN DOWNWIND DISTANCES FOR OPTICS CALULATIONS (KILOMETERS).     PLV02970
C   IT IS RECOMMENDED THAT THE FIRST 4 DOWNWIND DISTANCES BE SET TO     PLV02980
C   1., 2., 5., AND 10. KM.                                             PLV02990
C                                                                       PLV03000
      READ(5,9)(DIST(I),I=1,NX2)                                        PLV03010
9     FORMAT(8F10.0)                                                    PLV03020
C                                                                       PLV03030
C   CONVERT DOWNWIND DISTANCES FROM KILOMETERS TO METERS                PLV03040
C                                                                       PLV03050
      DO 32 I=1,NX2                                                     PLV03060
32    DIST(I)=DIST(I)*1000.                                             PLV03070
C                                                                       PLV03080
C    READ IN SO2, NOX, AND PARTICULATE EMISSION RATES IN TONS/DAY       PLV03090
C    FOR ALL STACKS COMBINED.  (SHORT TONS = 2000 LB, NOT METRIC TONS)  PLV03100
C                                                                       PLV03110
      READ (5,3) QSO2,QNOX,QPART                                        PLV03120
C                                                                       PLV03130
3     FORMAT (3F10.2)                                                   PLV03140
C                                                                       PLV03150
C    READ IN FLUE GAS FLOW RATE PER STACK (CU FT PER MIN), FLUE GAS     PLV03160
C    EXIT TEMPERATURE (DEG F), FLUE GAS OXYGEN CONCENTRATION (MOLE-     PLV03170
C    PERCENT), FLUE GAS EXIT VELOCITY (M/S)                             PLV03180
C                                                                       PLV03190
      READ (5,4) FLOW,FGTEMP,FGO2,WMAX                                  PLV03200
4     FORMAT (3F10.1,F10.2)                                             PLV03210
C                                                                       PLV03220
C    READ IN NUMBER OF STACKS AND STACK HEIGHT.                         PLV03230
C                                                                       PLV03240
      READ (5,5) UNITS,HSTACK                                           PLV03250
5     FORMAT (2F5.1)                                                    PLV03260
6     FORMAT (F10.0)                                                    PLV03270
C                                                                       PLV03280
C    READ IN AMBIENT AIR TEMPERATURE AT STACK HEIGHT.                   PLV03290
C                                                                       PLV03300
      READ (5,7) TAMB                                                   PLV03310
7     FORMAT (F10.1)                                                    PLV03320
C                                                                       PLV03330
C    READ IN AMBIENT BACKGROUND POLLUTANT CONCENTRATION IN PPM          PLV03340
C    OF NOX, NO2, O3, AND SO2.                                          PLV03350
C                                                                       PLV03360
      READ(5,8) AMBNOX,AMBNO2,O3AMB,AMBSO2                              PLV03370
 8    FORMAT(5F10.3)                                                    PLV03380
C                                                                       PLV03390
C********************************************************************** PLV03400
C    READ IN AEROSOL SIZE DISTRIBUTION DATA.  ROVA=MASS MEAN RADIUS     PLV03410
C    FOR BACKGROUND ACCUMULATION MODE (.1 TO 1 MICROMETER).  ROVC =     PLV03420
C    MASS MEAN RADIUS FOR BACKGROUND COARSE MODE (> 1 MICROMETER).      PLV03430
C    ROVS = MASS MEAN RADIUS FOR PLUME SECONDARY AEROSOL.  ROVP =       PLV03440
C    MASS MEAN RADIUS FOR PLUME PRIMARY AEROSOL.  SIGA = GEOMETRIC      PLV03450
C    STANDARD DEVIATION OF RADIUS OF BACKGROUND ACCUMULATION MODE.      PLV03460
C    SIGC = GEOMETRIC STANDARD DEVIATION OF BACKGROUND COARSE MODE      PLV03470
C    AEROSOL.  SIGS = GEOMETRIC STANDARD DEVIATION OF RADIUS OF         PLV03480
C    PLUME SECONDARY AEROSOL.  SIGP GEOMETRIC STANDARD DEVIATION OF     PLV03490
C    RADIUS OF PLUME PRIMARY PARTICULATE.  DENA=DENSITY (G/CM**3) OF    PLV03500
C    BACKGROUND ACCUMULATION MODE AEROSOL.  DENC = DENSITY OF BACKGROUNDPLV03510
C    COARSE MODE AEROSOL.  DENS = DENSITY OF PLUME SECONDARY AEROSOL    PLV03520
C    AND DENP = DENSITY OF PLUME PRIMARY AEROSOL.                       PLV03530
C********************************************************************** PLV03540
C                                                                       PLV03550
      READ(5,8) ROVA,ROVC,ROVS,ROVP                                     PLV03560
      READ(5,8) SIGA,SIGC,SIGS,SIGP                                     PLV03570
      READ(5,8) DENA,DENC,DENS,DENP                                     PLV03580
C                                                                       PLV03590
C********************************************************************** PLV03600
C     ROVCAR = MASS MEDIAN RADIUS OF CARBON PARTICULATE.                PLV03610
C     SIGCAR = GEOMETRIC STD DEVIATION OF CARBON PARTICULATE            PLV03620
C     DENCAR = DENSITY OF PLUME CARBON PARTICULATE EMISSIONS.           PLV03630
C     RFR = REAL PART OF INDEX OF REFRACTION FOR AEROSOL SCATTERING.    PLV03640
C     RFI = IMAGINARY PART OF INDEX OF REFRACTION FOR  AEROSOL          PLV03650
C     ABSORPTION.                                                       PLV03660
C     FRACTC = FRACTION OF PARTICULATE MASS EMISSION DUE TO CARBON.     PLV03670
C     AMBCAR = BACKGROUND ATMOSPHERIC  ELEMENTAL CARBON (UG/M3).        PLV03680
C     RFRSO4 = REAL PART OF INDEX OF REFRACTION FOR ACCUMULATION MODE   PLV03690
C     AEROSOL.                                                          PLV03700
C     RFISO4 = IMAGINARY PART OF INDEX OF REFRACTION FOR ACCUMULATION   PLV03710
C         MODE AEROSOL.                                                 PLV03720
C     RFRCAR = REAL PART OF INDEX OF REFRACTION FOR CARBON AEROSOL.     PLV03730
C              (CARBON AEROSOL WITHIN PLUME).                           PLV03740
C     RFICAR = IMAGINARY PART OF INDEX OF REFRACTION FOR CARBON         PLV03750
C              AEROSOL (FOR ABSORPTION WITHIN PLUME.)                   PLV03760
C   RFRCOR, RFICOR = REAL AND IMAGINARY PARTS OF INDEX OF               PLV03770
C             REFRACTION FOR BACKGROUND SKY COARSE MODE                 PLV03780
C             AEROSOL.                                                  PLV03790
C   RFRPRM, RFIPRM = REAL AND IMAGINARY PARTS OF INDEX OF               PLV03800
C             REFRACTION FOR PLUME PRIMARY AEROSOL                      PLV03810
C********************************************************************** PLV03820
C                                                                       PLV03830
      READ(5,8)ROVCAR,SIGCAR,DENCAR,FRACTC,AMBCAR                       PLV03840
      READ(5,8)RFRSO4,RFISO4,RFRCOR,RFICOR                              PLV03850
      READ(5,8)RFRPRM,RFIPRM,RFRCAR,RFICAR                              PLV03860
C                                                                       PLV03870
C    READ IN AMBIENT COARSE MODE AEROSOL CONCENTRATION  (BACKGROUND,)   PLV03880
C    IN UG/M.                                                           PLV03890
C                                                                       PLV03900
      READ(5,8) CORAMB                                                  PLV03910
C                                                                       PLV03920
C********************************************************************** PLV03930
C    INTYP = PARAMETER TO DETERMINE INPUT DATA.  IF INTYP=1,            PLV03940
C    BACKGROUND SO4 AND NO3 CONCENTRATIONS ARE INPUT AND MODEL          PLV03950
C    COMPUTES BACKGROUND VISUAL RANGE.  IF INTYP .NE. ONE,              PLV03960
C    BACKGROUND VISUAL RANGE IS INPUT AND MODEL COMPUTES BACKGROUND     PLV03970
C    ACCUMULATION MODE CONCENTRATION.                                   PLV03980
C********************************************************************** PLV03990
C                                                                       PLV04000
      READ(5,801) INTYP                                                 PLV04010
 801  FORMAT(I5)                                                        PLV04020
      IF(INTYP.EQ.1) GO TO 802                                          PLV04030
C                                                                       PLV04040
C    READ IN BACKGROUND VISUAL RANGE.                                   PLV04050
C                                                                       PLV04060
      READ(5,8) RVAMB                                                   PLV04070
      GO TO 803                                                         PLV04080
C                                                                       PLV04090
C    READ IN BACKGROUND SULFATE AND  NITRATE CONCENTRATIONS. (UG/M**3)  PLV04100
C                                                                       PLV04110
 802  READ(5,8) AMBSO4,AMBNO3                                           PLV04120
 803  CONTINUE                                                          PLV04130
C                                                                       PLV04140
C********************************************************************** PLV04150
C    READ IN DEPOSITION VELOCITIES. VDSO2 = DEPOSTION VEL FOR SO2.      PLV04160
C    VDNOX = DEPOSITION VELOCITY FOR NOX.  VDCOR = DEP. VEL. FOR        PLV04170
C    COARSE MODE PARTICULATE. VDSUB =DEPOSITION VELOCITY FOR            PLV04180
C    SUB-MICRON PARTICULATE.  UNITS ARE CM/S.                           PLV04190
C********************************************************************** PLV04200
C                                                                       PLV04210
      READ(5,806) VDSO2,VDNOX,VDCOR,VDSUB                               PLV04220
 806  FORMAT(16F5.2)                                                    PLV04230
C                                                                       PLV04240
C   READ IN FLAG FOR SO2-TO-SO4 CONVERSION RATE (IN ADDITION TO OH      PLV04250
C   CHEMISTRY) TO BE CONSTANT WITH DOWNWIND DISTANCE (ICON=0) OR        PLV04260
C   TO CHANGE WITH DISTANCE FROM THE SOURCE (ICON=1).                   PLV04270
C                                                                       PLV04280
      READ(5,801)ICON                                                   PLV04290
C                                                                       PLV04300
C   READ IN SO2-TO-SO4 CONVERSION RATE (%/HR) TO BE ADDED TO RATE       PLV04310
C   CALCULATED FROM OH CHEMISRY.                                        PLV04320
C                                                                       PLV04330
      READ(5,46)RSO2C                                                   PLV04340
46    FORMAT(8F10.7)                                                    PLV04350
      IF(ICON.EQ.1)GO TO 826                                            PLV04360
      DO 79 NX=1,NX2                                                    PLV04370
79    RSO2(NX)=RSO2C                                                    PLV04380
      GO TO 807                                                         PLV04390
C                                                                       PLV04400
C  READ IN SO2-TO-SO4 CONVERSION RATE TO BE ADDED TO RATE CALCULATED BY PLV04410
C  OH MODEL, WITH VALUES CORRESPONDING TO EACH OF THE ANALYSIS POINTS   PLV04420
C  DOWNWIND FROM THE SOURCE.                                            PLV04430
C                                                                       PLV04440
826   READ(5,46)(RSO2(NX),NX=1,NX2)                                     PLV04450
 807  CONTINUE                                                          PLV04460
C                                                                       PLV04470
C********************************************************************** PLV04480
C  NC1 AND NC2 CONTROL OUTPUT                                           PLV04490
C  NC1=1,NC2=1 FOR STANDARD TABLES ONLY (NOT SITE SPECIFIC)             PLV04500
C  NC1=2,NC2=2 FOR SITE SPECIFIC TABLES ONLY                            PLV04510
C  NC1=1, NC2=2 FOR BOTH TABLES-- SITE SPECIFIC AND STANDARD            PLV04520
C********************************************************************** PLV04530
C                                                                       PLV04540
      READ(5,808)NC1,NC2                                                PLV04550
      IF(NC1.EQ.2)GO TO 825                                             PLV04560
C                                                                       PLV04570
C   READ IN INDICES FOR CONTROLLING PLUME-BASED DATA SAVED FOR PLOTTING.PLV04580
C                                                                       PLV04590
      READ(5,2001)NPP,NAP,NTP,NZP,IO1P,IPP                              PLV04600
825   IF(NC2.EQ.1)GO TO 816                                             PLV04610
C                                                                       PLV04620
C    FOR RUNS WITH SPECIFIC CASE CALCULATIONS, INCREASE NUMBER OF       PLV04630
C    SCATTERING ANGLES FOR ALL THE SPECIFIC VIEWER LINE-OF-SIGHT        PLV04640
C    GEOMETRIES.                                                        PLV04650
C                                                                       PLV04660
      NTHETA=7+NX2                                                      PLV04670
C                                                                       PLV04680
C    READ IN OBSERVER POSITION:  UTM X-COORDINATE (KM), UTM Y-COORDINATEPLV04690
C    (KM), ELEVATION (FT,MSL) FOR OBSERVER-BASED CALCULATIONS.          PLV04700
C                                                                       PLV04710
      READ(5,4)XOBS,YOBS,ZOBS                                           PLV04720
C                                                                       PLV04730
C    READ IN SOURCE POSITION:  UTM X-COORDINATE, Y-COORDINATE, ELEVATIONPLV04740
C    FOR ALL RUNS.                                                      PLV04750
C                                                                       PLV04760
816   READ(5,4)XSTACK,YSTACK,ZSTACK                                     PLV04770
C                                                                       PLV04780
C********************************************************************** PLV04790
C    READ UTM GRID ZONE NUMBER, MONTH, DAY, TIME (24 HOUR MILITARY),    PLV04800
C    TIME ZONE NUMBER, AND YEAR.                                        PLV04810
C   IZONE IS THE UNIVERSAL TRANSVERSE MERCATOR GRID ZONE NUMBER         PLV04820
C   READ FROM A USGS MAP                                                PLV04830
C   TZONE IS THE TIME ZONE NUMBER, COUNTING WEST FROM GREENWICH,        PLV04840
C   ADD ONE FOR DAYLIGHT SAVINGS TIME.                                  PLV04850
C********************************************************************** PLV04860
C                                                                       PLV04870
      READ(5,808)IZONE,IMO,IDAY, TIME,TZONE,IYEAR                       PLV04880
  808 FORMAT(3I5,2F5.0,I5)                                              PLV04890
c delete this check since we have initial check at start of program
C  IF TIME IS EXACTLY ON THE HOUR, ADD ONE MINUTE TO AVOID COMPILATION  PLV04891
C  ERRORS.                                                              PLV04892
C    Will Richards and Marcelo Korc, STI, December, 14, 1994            PLV04893
c      WRITE(CTIME,899) TIME                                             PLV04894
c  899 FORMAT(F5.0)                                                      PLV04895
c      IF (CTIME(3:4).EQ.'00') TIME=TIME+1                               PLV04896
C                                                                       PLV04900
C    SKIP OVER READ STATEMENTS IF RUN IS FOR A PLUME-BASED CASE ONLY.   PLV04910
C                                                                       PLV04920
      IF(NC2.EQ.1)GO TO 818                                             PLV04930
C                                                                       PLV04940
C********************************************************************** PLV04950
C    READ IN ELEVATION OF TERRAIN AT EACH DOWNWIND POINT (FT MSL). IF   PLV04960
C    TER(1)=0, MODEL SETS UP FLAT TERRAIN FOR ALL POINTS.  THESE CHANGESPLV04970
C    ARE USED ONLY IN THE CALCULATION OF THE ELEVATION ANGLE BETA OF THEPLV04980
C    SPECIFIC LINES OF SIGHT USED FOR THE OBSERVER-BASED CALCULATIONS.  PLV04990
C    THE GAUSSIAN DISPERSION CALCULATIONS USE FLAT TERRAIN.             PLV05000
C********************************************************************** PLV05010
C                                                                       PLV05020
      READ(5,809)(TER(I),I=1,NX2)                                       PLV05030
      IF(TER(1).NE.0.) GO TO 8080                                       PLV05040
      DO 8079 I=1,NX2                                                   PLV05050
      TER(I)=ZSTACK                                                     PLV05060
 8079 CONTINUE                                                          PLV05070
 8080 CONTINUE                                                          PLV05080
C                                                                       PLV05090
C********************************************************************** PLV05100
C    READ IN BACKGROUND OBJECT DISTANCES FROM OBSERVER THROUGH PLUME    PLV05110
C    TO BACKGROUND TERRAIN FOR LINE-OF-SIGHT AZIMUTHS OF 15 DEG.,       PLV05120
C    30 DEG., 45 DEG., ... , 360 DEG.  THE DISTANCE FOR EACH LINE-      PLV05130
C    OF-SIGHT AZIMUTH ACTUALLY USED IS INTERPOLATED FROM THESE VALUES.  PLV05140
C    IF ROBJCT(I) = 0, THE OBJECT DISTANCE IS SET EQUAL TO THE PLUME-   PLV05150
C    OBSERVER DISTANCE ALONG THE LINE-OF-SIGHT TO THE ITH DOWNWIND POINTPLV05160
C********************************************************************** PLV05170
C                                                                       PLV05180
      READ(5,809)(ROBJCT(NAZ),NAZ=1,24)                                 PLV05190
  809 FORMAT(8F10.1)                                                    PLV05200
C                                                                       PLV05210
C    READ IN WIND DIRECTION IN DEGREES FROM NORTH (FROM WHICH WIND IS   PLV05220
C    BLOWING).                                                          PLV05230
C                                                                       PLV05240
      READ(5,7)WIND                                                     PLV05250
818   WRITE (6,10)                                                      PLV05260
10    FORMAT (1H0)                                                      PLV05270
      WRITE  (6,11) (PLANT(J),J=1,6)                                    PLV05280
11    FORMAT (30H VISUAL IMPACT ASSESSMENT FOR ,6A4//)                  PLV05290
      WRITE (6,12) ZSTACK                                               PLV05300
12    FORMAT (5X,21HEMISSIONS SOURCE DATA//10X,20HELEVATION OF SITE = , PLV05310
     1 F10.0,10H  FEET MSL)                                             PLV05320
C                                                                       PLV05330
C CONVERT ELEVATION FROM FEET TO METERS                                 PLV05340
C                                                                       PLV05350
      ELEV = ZSTACK/3.281                                               PLV05360
      WRITE (6,13) ELEV                                                 PLV05370
 13   FORMAT (30X,F10.0,12H  METERS MSL,/)                              PLV05380
      WRITE (6,14) UNITS                                                PLV05390
 14   FORMAT (10X,15HNO. OF UNITS = ,F6.0,/)                            PLV05400
      WRITE (6,15) HSTACK                                               PLV05410
 15   FORMAT (10X,15HSTACK HEIGHT = ,F5.0,6H  FEET)                     PLV05420
C                                                                       PLV05430
C CONVERT STACK HEIGHT FROM FEET TO METERS                              PLV05440
C                                                                       PLV05450
      HSTACK = HSTACK/3.281                                             PLV05460
      WRITE (6,16) HSTACK                                               PLV05470
 16   FORMAT (25X,F5.0,8H  METERS,/)                                    PLV05480
      WRITE (6,17) FLOW                                                 PLV05490
 17   FORMAT(10X,21HFLUE GAS FLOW RATE = ,F10.0,11H  CU FT/MIN)         PLV05500
C                                                                       PLV05510
C CONVERT FLUE GAS FLOW RATE FROM CU FT/MIN TO CU METERS/SEC            PLV05520
C                                                                       PLV05530
      FLOW = FLOW/(3.281**3.*60.)                                       PLV05540
      WRITE (6,18) FLOW                                                 PLV05550
 18   FORMAT (31X,F10.2,10H  CU M/SEC,/)                                PLV05560
      WRITE (6,19) FGTEMP                                               PLV05570
 19   FORMAT (10X,23HFLUE GAS TEMPERATURE = ,F10.0,3H  F)               PLV05580
C                                                                       PLV05590
C CONVERT FLUE GAS TEMPERATURE FROM F TO K                              PLV05600
C                                                                       PLV05610
      FGTEMP = (FGTEMP+459.67)/1.8                                      PLV05620
      WRITE (6,20) FGTEMP                                               PLV05630
 20   FORMAT(33X,F10.0,3H  K,/)                                         PLV05640
      WRITE (6,21) FGO2                                                 PLV05650
 21   FORMAT(10X,26HFLUE GAS OXYGEN CONTENT = ,F10.1,13H  MOL PERCENT,/)PLV05660
C                                                                       PLV05670
C CONVERT FGO2 IN M0L PERCENT TO QO2 IN PPM BY VOLUME                   PLV05680
C                                                                       PLV05690
      QO2 = FGO2*1.E4                                                   PLV05700
      WRITE (6,22) QSO2                                                 PLV05710
22    FORMAT (10X,28HSO2 EMISSION RATE (TOTAL) = ,F10.2,                PLV05720
     1       10H  TONS/DAY)                                             PLV05730
C                                                                       PLV05740
C CONVERSION FACTOR TONS/DAY TO GRAMS PER SEC                           PLV05750
C                                                                       PLV05760
      C1 = 2000.*453.6/(24.*3600.)                                      PLV05770
      QSO2 = QSO2*C1                                                    PLV05780
      WRITE (6,23) QSO2                                                 PLV05790
 23   FORMAT (38X,1PE12.3,7H  G/SEC,/)                                  PLV05800
      WRITE (6,24) QNOX                                                 PLV05810
24    FORMAT (10X,35HNOX EMISSION RATE (TOTAL,AS NO2) = ,F10.2,         PLV05820
     1      10H  TONS/DAY)                                              PLV05830
      QNOX = QNOX*C1                                                    PLV05840
      WRITE (6,231) QNOX                                                PLV05850
 231  FORMAT(45X,1PE12.3,7H  G/SEC,/)                                   PLV05860
      WRITE (6,25) QPART                                                PLV05870
25    FORMAT (10X,36HPARTICULATE EMISSION RATE (TOTAL) = ,F10.2,        PLV05880
     1      10H  TONS/DAY)                                              PLV05890
      QPART = QPART*C1                                                  PLV05900
      WRITE(6,231) QPART                                                PLV05910
      WRITE(6,10)                                                       PLV05920
      WRITE (6,26) U                                                    PLV05930
26    FORMAT (///5X,43HMETEOROLOGICAL AND AMBIENT AIR QUALITY DATA//10X,PLV05940
     1           12HWINDSPEED = ,F5.1,10H  MILES/HR)                    PLV05950
C                                                                       PLV05960
C CONVERT WIND SPEED FROM MILES/HR TO M/SEC                             PLV05970
C                                                                       PLV05980
      U = U*0.447                                                       PLV05990
      WRITE (6,27) U                                                    PLV06000
      WRITE(6,271)                                                      PLV06010
      IF(IDIS.EQ.9) WRITE(6,281)                                        PLV06020
 281  FORMAT(10X,45HDISPERSION COEFFICIENTS ARE USER-INPUT VALUES)      PLV06030
      I=IS                                                              PLV06040
      IF(IDIS.EQ.1) WRITE(6,282) I                                      PLV06050
 282  FORMAT(10X,19HTVA STABILITY INDEX,I2)                             PLV06060
      IF(IDIS.EQ.0) WRITE(6,283) STABLE(I)                              PLV06070
 283  FORMAT(10X,42HPASQUILL-GIFFORD-TURNER STABILITY CATEGORY,A2)      PLV06080
 27   FORMAT(1H ,21X,F5.1,7H  M/SEC)                                    PLV06090
      WRITE(6,271)                                                      PLV06100
 271  FORMAT(5X)                                                        PLV06110
28    FORMAT (10X,18HSTABILITY INDEX = ,I2)                             PLV06120
      WRITE (6,29) ALAPSE                                               PLV06130
 29   FORMAT(10X,13HLAPSE RATE = ,F5.2,11H  F/1000 FT)                  PLV06140
C                                                                       PLV06150
C CONVERT LAPSE RATE FROM F/1000 FT TO K/M                              PLV06160
C                                                                       PLV06170
      ALAPSE = ALAPSE*3.281/(1000.*1.8)                                 PLV06180
      WRITE (6,30) ALAPSE                                               PLV06190
 30   FORMAT (23X,1PE12.3,5H  K/M,/)                                    PLV06200
C                                                                       PLV06210
C CONVERT TO POTENTIAL TEMPERATURE BY ADDING DRY ADIABATIC LAPSE RATE   PLV06220
C                                                                       PLV06230
      ALAPSE = ALAPSE + 9.8E-3                                          PLV06240
C                                                                       PLV06250
C    TO PREVENT POSSIBLE DIVIDE BY ZERO IN CALCULATION OF STABILITY     PLV06260
C    PARAMETER S IN PLUME RISE,CHECK IF ALAPSE = 0.                     PLV06270
C                                                                       PLV06280
      IF(ALAPSE.EQ.0.)ALAPSE=.1E-30                                     PLV06290
      WRITE (6,31) ALAPSE                                               PLV06300
31    FORMAT (10X,35HPOTENTIAL TEMPERATURE LAPSE RATE = ,1PE12.3,       PLV06310
     1         5H  K/M,/)                                               PLV06320
      WRITE (6,33) TAMB                                                 PLV06330
 33   FORMAT(10X,22HAMBIENT TEMPERATURE = ,F5.1,3H  F)                  PLV06340
C                                                                       PLV06350
C CONVERT AMBIENT TEMPERATURE FROM F TO K                               PLV06360
C                                                                       PLV06370
      TAMB = (TAMB + 459.67)/1.8                                        PLV06380
      WRITE (6,34) TAMB                                                 PLV06390
 34   FORMAT(32X,F5.1,3H  K,/)                                          PLV06400
      WRITE(6,341) RH                                                   PLV06410
      RH=RH/100.                                                        PLV06420
 341  FORMAT(10X,20HRELATIVE HUMIDITY = ,F5.1,3H  %,/)                  PLV06430
      WRITE(6,342) HPBLM                                                PLV06440
 342  FORMAT(10X,15HMIXING DEPTH = ,F10.1,3H  M,/)                      PLV06450
C                                                                       PLV06460
C  CALCULATION OF AMBIENT PRESSURE IN ATMOSPHERES FROM ELEVATION IN M MSPLV06470
C                                                                       PLV06480
      PAMB = EXP(-1.15E-4*ELEV)                                         PLV06490
C                                                                       PLV06500
C  CALCULATE WATER CONCENTRATION IN PPM                                 PLV06510
C                                                                       PLV06520
      WATER =(6030.*RH/PAMB)*EXP(4884.*(1./273.-1./TAMB))               PLV06530
      WRITE (6,40) PAMB                                                 PLV06540
      WRITE(6,271)                                                      PLV06550
40    FORMAT (10X,19HAMBIENT PRESSURE = ,F5.2,5H  ATM)                  PLV06560
      WRITE (6,35) AMBNOX                                               PLV06570
      WRITE(6,271)                                                      PLV06580
35    FORMAT (10X,31HBACKGROUND NOX CONCENTRATION = ,F10.3,5H  PPM)     PLV06590
C                                                                       PLV06600
C     CONVERT UTM COORDINATES TO LAT, LONG.                             PLV06610
C     XSTACK = UTM EAST COORD., YSTACK= UTM NORTH COORD.                PLV06620
C  IZONE= UTM ZONE NUMBER                                               PLV06630
C     ALON=LONGITUDE IN DEGREES, ALAT= LATITUDE IN DEGREES              PLV06640
C                                                                       PLV06650
      CALL MAPUTG(XSTACK,YSTACK,IZONE,ALON,ALAT)                        PLV06660
C                                                                       PLV06670
C********************************************************************** PLV06680
C CHANGE SIGN OF LONGITUDE TO MATCH THE POSITIVE DIRECTION              PLV06690
C ASSUMED IN SOLARZ, WHERE POSITIVE INCREASING IS FROM EAST             PLV06700
C TO WEST STARTING FROM THE GREENWICH MERIDIAN. MAPUTG HAS              PLV06710
C LONGITUDE INCREASING POSITIVE FROM WEST TO EAST STARTING              PLV06720
C FROM THE GRENWICH MERIDIAN, AND IT RETURNS NEGATIVE                   PLV06730
C VALUES OF LONGITUDE FOR THE U.S.                                      PLV06740
C********************************************************************** PLV06750
C                                                                       PLV06760
      ALON = -1.*ALON                                                   PLV06770
c*** jcv NPS 6/18/96
c Insert a check for zenith angle greater than zmax (initially set
c   at 88 degrees).  If greater than 88 modify time until zenith
c   is less than 88.  Also print warning message about time change.
c   warning is printed in section where zenith angle is printed.

	oldtime = time
	icnt = 0
22201 continue
C                                                                       PLV06780
C     CALCULATE SOLAR ZENITH ANGLE.                                     PLV06790
C                                                                       PLV06800
      TIME1=CLOK(TIME,-2)                                               PLV06810
      CALL SOLARZ(ALAT,ALON,TZONE,IYEAR,IMO,IDAY,TIME1,SUNEL,5)         PLV06820
      SINAL=COS(EFFDEC)*SIN(HRANGL)/SIN((90.-SUNEL)*RAD)                PLV06830
      CALL SOLARZ(ALAT,ALON,TZONE,IYEAR,IMO,IDAY, TIME,SUNELE,5)        PLV06840
      ZENITH=90.-SUNELE                                                 PLV06850

	if (zenith.gt.zmax) then
	  timflg = .true.
	  icnt = icnt + 1
	  if (icnt.gt.60) then
	    print *,'Time specified is more than 1 hour from sun rise/set'
	    stop
	  endif
	  if (time.le.1200.) then
	    time = time + 1.
	  else
	    time = time - 1.
	  endif
	  go to 22201
	endif

c*** End of JCV modification
C                                                                       PLV06860
C    CALCULATE SOLAR AZIMUTH ANGLE FOR LATE AM                          PLV06870
C                                                                       PLV06880
      SINA=COS(EFFDEC)*SIN(HRANGL )/SIN(ZENITH*RAD)                     PLV06890
      SUNAZ=180.-ASIN(SINA)/RAD                                         PLV06900
C                                                                       PLV06910
C     SUN AZIMUTH FOR EARLY AM                                          PLV06920
C                                                                       PLV06930
      IF(TIME .LT. 1030. .AND. SINA .GT. SINAL)SUNAZ=ASIN(SINA)/RAD     PLV06940
C                                                                       PLV06950
C     SUN AZIMUTH FOR EARLY PM                                          PLV06960
C                                                                       PLV06970
      IF(TIME .GT. 1030. .AND. SINA .GT. SINAL)SUNAZ=180.+ASIN(SINA)/RADPLV06980
C                                                                       PLV06990
C     SUN AZIMUTH FOR LATE PM                                           PLV07000
C                                                                       PLV07010
      IF(TIME .GT. 1430. .AND. SINA .LT. SINAL)SUNAZ=360.-ASIN(SINA)/RADPLV07020
C                                                                       PLV07030
C    CALCULATE DYNAMIC EQUILIBRIUM CONSTANT FOR NO2 - NO                PLV07040
C    BRANCH IF ZENITH > 90 DEGREES AND SET CONSTANT TO ZERO.            PLV07050
C                                                                       PLV07060
      IF(ZENITH.GE.90.) GO TO 351                                       PLV07070
      PHIKK=(1.E-2/0.44)*EXP(-0.38/COS(PI/180.*ZENITH))                 PLV07080
      GO TO 352                                                         PLV07090
 351  PHIKK=0.0                                                         PLV07100
 352  CONTINUE                                                          PLV07110
      SUM=AMBNOX+AMBNO2+O3AMB+PHIKK                                     PLV07120
      XNO2=0.5*(SUM-SQRT(SUM*SUM-4.*AMBNOX*(O3AMB+AMBNO2)))             PLV07130
      IF(XNO2.GT.AMBNOX) XNO2=AMBNOX                                    PLV07140
      DIF=XNO2-AMBNO2                                                   PLV07150
      AMBNO2=XNO2                                                       PLV07160
      IF(DIF.GT.0.0005) GO TO 352                                       PLV07170
      WRITE (6,36) AMBNO2                                               PLV07180
      WRITE(6,271)                                                      PLV07190
36    FORMAT (10X,31HBACKGROUND NO2 CONCENTRATION = ,F10.3,5H  PPM)     PLV07200
      WRITE (6,37) O3AMB                                                PLV07210
      WRITE(6,271)                                                      PLV07220
37    FORMAT (10X,33HBACKGROUND OZONE CONCENTRATION = ,F10.3,5H  PPM)   PLV07230
      WRITE(6,370) AMBSO2                                               PLV07240
 370  FORMAT(10X,31HBACKGROUND SO2 CONCENTRATION = ,F10.3,5H  PPM,/)    PLV07250
      ELEV=ELEV/1000.                                                   PLV07260
  811 CONTINUE                                                          PLV07270
      IF(IUSFC.EQ.0)GO TO 817                                           PLV07280
C                                                                       PLV07290
C   Modified by D. DiCristofaro, 9/30/92                                PLV07300
C   Change anemometer height from 7 m to 10 m                           PLV07310
                                                                        PLV07320
C     CORRECT 10 METER WIND TO STACK HEIGHT                             PLV07330
C                                                                       PLV07340
      USFC=U                                                            PLV07350
      IF(IDIS.EQ.1)GO TO 821                                            PLV07360
C                                                                       PLV07370
C     CORRECTION IF PASQUILL-GIFFORD STABILITY CLASS                    PLV07380
C                                                                       PLV07390
C     U=USFC*(HSTACK/7.)**P(I)                                          PLV07400
      U=USFC*(HSTACK/10.)**P(I)                                         PLV07410
      GO TO 817                                                         PLV07420
821   CONTINUE                                                          PLV07430
C                                                                       PLV07440
C     CORRECTION IF TVA STABILTY CLASS                                  PLV07450
C                                                                       PLV07460
      P(1)=0.25                                                         PLV07470
      DO 822 II=2,6                                                     PLV07480
822   P(II)=0.30                                                        PLV07490
C     U=USFC*(HSTACK/7.)**P(I)                                          PLV07500
      U=USFC*(HSTACK/10.)**P(I)                                         PLV07510
C                                                                       PLV07520
C     PLUME RISE CALCULATION (FROM CRSTER MANUAL (BRIGGS))              PLV07530
C     BOUYANCY FLUX                                                     PLV07540
C                                                                       PLV07550
  817 F=(9.8*FLOW/PI)*(FGTEMP-TAMB)/FGTEMP                              PLV07560
C                                                                       PLV07570
C     PLUME RISE FOR STABLE CONDITIONS                                  PLV07580
C     STABILITY PARAMETER S                                             PLV07590
C                                                                       PLV07600
      S=(9.8/TAMB)*ALAPSE                                               PLV07610
      SMIN=(9.8/TAMB)*9.8E-4                                            PLV07620
      IF(S.LT.SMIN)GO TO 812                                            PLV07630
      XFINAL=PI*U/SQRT(S)                                               PLV07640
      RISE =2.6*(F/(U*S))**0.333334                                     PLV07650
      RISE2=5.*(F**0.25)*(S**(-3./8.))                                  PLV07660
      IF(RISE2.LT.RISE )RISE=RISE2                                      PLV07670
      GO TO 815                                                         PLV07680
  812 IF(F.GE.55.)GO TO 813                                             PLV07690
      XSTR=14.*F**(5./8.)                                               PLV07700
      GO TO 814                                                         PLV07710
  813 XSTR=34.*F**(2./5.)                                               PLV07720
814   XFINAL=3.5*XSTR                                                   PLV07730
      IF(XFINAL.LE.0.0)XFINAL=0.0                                       PLV07740
      RISE=1.6*(F**0.333334)*((3.5*XSTR)**.666667)/U                    PLV07750
  815 H=HSTACK+RISE                                                     PLV07760
C                                                                       PLV07770
C   CORRECT 7 METER WIND SPEED TO FINAL PLUME HEIGHT                    PLV07780
C                                                                       PLV07790
      IF(IUSFC.EQ.0)GO TO 823                                           PLV07800
C     U=USFC*(H/7.)**P(I)                                               PLV07810
      U=USFC*(H/10.)**P(I)                                              PLV07820
823   CONTINUE                                                          PLV07830
C                                                                       PLV07840
C    SKIP OVER SIT SPECIFIC GEOMETRY IF RUN IS FOR PLUME-BASED          PLV07850
C    CALCULATIONS ONLY.                                                 PLV07860
C                                                                       PLV07870
      IF(NC2.EQ.1)GO TO 820                                             PLV07880
C                                                                       PLV07890
C    CONVERT WIND TO AZIMUTH OF PLUME TRAGECTORY IN RADIANS.            PLV07900
C                                                                       PLV07910
      WIND=(WIND-180.)*RAD                                              PLV07920
C                                                                       PLV07930
C    CALCULATE OBSERVER AZIMUTH ANGLE, ELEVATION ANGLE (ABETA),         PLV07940
C    AND HORIZONTAL ANGLE BETWEEN LINE-OF-SITE AND PLUME CENTERLINE     PLV07950
C    FOR EACH DOWNWIND POINT (AALPHA).                                  PLV07960
C                                                                       PLV07970
      DO 810 I=1,NX2                                                    PLV07980
C                                                                       PLV07990
C********************************************************************** PLV08000
C    XPLUME = X COORDINATE OF DOWNWIND POINT IN KILOMETERS.             PLV08010
C    YPLUME = Y COORDINATE OF DOWNWIND POINT IN KILOMETERS.             PLV08020
C    ZPLUME = ELEVATION OF PLUME PARCEL (FT, MSL)                       PLV08030
C    OBSPLU(I) = DISTANCE IN KM FROM OBSERVER TO PLUME AT               PLV08040
C    DOWNWIND POINT I.                                                  PLV08050
C********************************************************************** PLV08060
C                                                                       PLV08070
      XPLUME= XSTACK+DIST(I)/1000. *SIN(WIND)                           PLV08080
      YPLUME= YSTACK+DIST(I)/1000. *COS(WIND)                           PLV08090
      ZPLUME=H*3.281+TER(I)                                             PLV08100
      OBSPLU(I)=((XPLUME-XOBS)**2+(YPLUME-YOBS)**2+((ZPLUME-ZOBS)/3.281/PLV08110
     1 1000.)**2)**0.5                                                  PLV08120
C                                                                       PLV08130
C    CALCULATE DELTAX, DELTAY AND DELTAZ BETWEEN PLUME PARCEL AND       PLV08140
C    AND OBSERVER POSITION.                                             PLV08150
C                                                                       PLV08160
      DELTAX=XPLUME-XOBS                                                PLV08170
      DELTAY=YPLUME-YOBS                                                PLV08180
      DELTAZ=(ZPLUME-ZOBS)/3281.                                        PLV08190
C                                                                       PLV08200
C    CALCULATE AZIMUTH OF OBSERVER SIGHTLINE.                           PLV08210
C                                                                       PLV08220
      AZMUTH(I)=ATAN2(DELTAX,DELTAY)/RAD                                PLV08230
      IF(AZMUTH(I).LT.0.)AZMUTH(I)=AZMUTH(I)+360.                       PLV08240
      AALPHA(I)=ACOS(COS(WIND)*COS(AZMUTH(I)*RAD)+SIN(WIND)*SIN(        PLV08250
     1 AZMUTH(I)*RAD))/RAD                                              PLV08260
      IF (AALPHA(I).GT.90.)AALPHA(I)=180.-AALPHA(I)                     PLV08270
C                                                                       PLV08280
C    CALCULATION OF SCATTERING ANGLE BETWEEN DIRECT SOLAR RAY AND       PLV08290
C    OBSERVER LINE-OF-SIGHT FOR EACH DOWNWIND POINT.                    PLV08300
C                                                                       PLV08310
      X1A=(DELTAX*SIN(SUNAZ*RAD)+DELTAY*COS(SUNAZ*RAD))*SIN(ZENITH      PLV08320
     1 *RAD)+DELTAZ*COS(ZENITH*RAD)                                     PLV08330
      X2A=OBSPLU(I)                                                     PLV08340
      TT(I+7)=ACOS(X1A/X2A)/RAD                                         PLV08350
C                                                                       PLV08360
C    CALCULATION OF ANGLE OF OBSERVER LINE-OF-SIGHT ABOVE HORIZON.      PLV08370
C                                                                       PLV08380
      ABETA(I)=ASIN(DELTAZ/OBSPLU(I))/RAD                               PLV08390
C                                                                       PLV08400
C   INTERPOLATION OF DISTANCE TO BACKGROUND TERRAIN FOR THE AZIMUTH     PLV08410
C   OF THE SPECIFIC LINE OF SIGHT FOR EACH DOWNWIND DISTANCE.  IF       PLV08420
C   ROBJCT(I)=0., SET ROBJT(I)=OBSPLU(I).                               PLV08430
C                                                                       PLV08440
      IF(ROBJCT(I).EQ.0.)GO TO 824                                      PLV08450
      IF(AZMUTH(I).LT.15.)GO TO 805                                     PLV08460
      DO 804 IAZZ=1,23                                                  PLV08470
      IAZ=IAZZ+1                                                        PLV08480
      AZU=FLOAT(IAZZ)*15.                                               PLV08490
      AZUM=FLOAT(IAZ)*15.                                               PLV08500
      IF(AZMUTH(I).GE.AZU.AND.AZMUTH(I).LT.AZUM)GO TO 819               PLV08510
804   CONTINUE                                                          PLV08520
C                                                                       PLV08530
C   SPECIAL CALCULATION FOR AZIMUTH .LT. 15 DEGREES.                    PLV08540
C                                                                       PLV08550
805   ROBJT(I)=ROBJCT(24)+(ROBJCT(1)-ROBJCT(24))*(AZMUTH(I)/15.)        PLV08560
      GO TO 810                                                         PLV08570
819   ROBJT(I)=ROBJCT(IAZZ)+(ROBJCT(IAZ)-ROBJCT(IAZZ))*((AZMUTH(I)-AZU) PLV08580
     1 /15.)                                                            PLV08590
      GO TO 810                                                         PLV08600
824   ROBJT(I)=OBSPLU(I)                                                PLV08610
  810 CONTINUE                                                          PLV08620
  820 CONTINUE                                                          PLV08630
C                                                                       PLV08640
C **********************************************************************PLV08650
C                                                                       PLV08660
C    CALCULATE BACKGROUND RADIATION CHARACTERISTICS :  SCATTERING,      PLV08670
C    ABSORPTION, AND EXTINCTION COEFFICIENTS AND OPTICAL DEPTHS.        PLV08680
C    ******CALL INRAD*******                                            PLV08690
C                                                                       PLV08700
C **********************************************************************PLV08710
C                                                                       PLV08720
      CALL INRAD(ZENITH)                                                PLV08730
228   CONTINUE                                                          PLV08740
      WRITE(6,371) CORAMB                                               PLV08750
 371  FORMAT(///,10X,39HBACKGROUND COARSE MODE CONCENTRATION = ,F10.1,  PLV08760
     1'  UG/M3',/)                                                      PLV08770
      WRITE(6,372) AMBSO4                                               PLV08780
 372  FORMAT(10X,35HBACKGROUND SULFATE CONCENTRATION = ,F10.1,7H  UG/M3,PLV08790
     1/)                                                                PLV08800
      WRITE(6,373) AMBNO3                                               PLV08810
 373  FORMAT(10X,35HBACKGROUND NITRATE CONCENTRATION = ,F10.1,7H  UG/M3,PLV08820
     1/)                                                                PLV08830
      WRITE(6,38) RVAMB                                                 PLV08840
 38   FORMAT(10X,26HBACKGROUND VISUAL RANGE = ,F10.1,12H  KILOMETERS,/) PLV08850
      WRITE(6,374) VDSO2                                                PLV08860
 374  FORMAT(10X,26HSO2 DEPOSITION VELOCITY = ,F10.2,8H  CM/SEC,/)      PLV08870
      WRITE(6,375) VDNOX                                                PLV08880
 375  FORMAT(10X,26HNOX DEPOSITION VELOCITY = ,F10.2,8H  CM/SEC,/)      PLV08890
      WRITE(6,376) VDCOR                                                PLV08900
  376 FORMAT(10X,41HCOARSE PARTICULATE DEPOSITION VELOCITY = ,F10.2,    PLV08910
     1'  CM/SEC',/)                                                     PLV08920
      WRITE(6,377) VDSUB                                                PLV08930
 377  FORMAT(10X,44HSUBMICRON PARTICULATE DEPOSITION VELOCITY = ,F10.2, PLV08940
     18H  CM/SEC,/)                                                     PLV08950
      WRITE(6,271)                                                      PLV08960
      WRITE(6,310)                                                      PLV08970
310   FORMAT(39X,18HAEROSOL STATISTICS)                                 PLV08980
      WRITE(6,271)                                                      PLV08990
      WRITE(6,312)                                                      PLV09000
312   FORMAT(36X,10HBACKGROUND,32X,5HPLUME)                             PLV09010
      WRITE(6,271)                                                      PLV09020
313   FORMAT(25X,12HACCUMULATION,11X,6HCOARSE,11X,12HACCUMULATION,11X,  PLV09030
     1'COARSE',11X,12HCARBONACEOUS)                                     PLV09040
      WRITE(6,313)                                                      PLV09050
      WRITE(6,314)                                                      PLV09060
314   FORMAT(10X,11HMASS MEDIAN,8X,4HMODE,16X,4HMODE,16X,4HMODE,16X,    PLV09070
     14HMODE,16X,8HAEROSOLS)                                            PLV09080
      WRITE(6,315)                                                      PLV09090
315   FORMAT(10X,6HRADIUS)                                              PLV09100
      WRITE(6,316)ROVA,ROVC,ROVS,ROVP,ROVCAR                            PLV09110
316   FORMAT(10X,11HMICROMETERS,5X,F10.3,4(10X,F10.3))                  PLV09120
      WRITE(6,271)                                                      PLV09130
      WRITE(6,317)                                                      PLV09140
317   FORMAT(10X,9HGEOMETRIC)                                           PLV09150
      WRITE(6,318)                                                      PLV09160
318   FORMAT(10X,8HSTANDARD)                                            PLV09170
      WRITE(6,319)SIGA,SIGC,SIGS,SIGP,SIGCAR                            PLV09180
319   FORMAT(10X,9HDEVIATION,7X,F10.3,4(10X,F10.3))                     PLV09190
      WRITE(6,271)                                                      PLV09200
      WRITE(6,320)                                                      PLV09210
320   FORMAT(10X,8HPARTICLE)                                            PLV09220
      WRITE(6,321)                                                      PLV09230
321   FORMAT(10X,7HDENSITY)                                             PLV09240
      WRITE(6,322)DENA,DENC,DENS,DENP,DENCAR                            PLV09250
322   FORMAT(10X,9HG/(CM**3),7X,F10.3,4(10X,F10.3))                     PLV09260
      WRITE(6,10015) FRACTC                                             PLV09270
      WRITE(6,10016) AMBCAR                                             PLV09280
10015 FORMAT(/1X,'CARBONACEOUS FRACTION OF PARTICLATE MASS EMISSIONS = 'PLV09290
     2 ,F10.3,/)                                                        PLV09300
10016 FORMAT(1X,42HBACKGROUND ATMOSPHERIC ELEMENTAL CARBON = ,F10.3,    PLV09310
     28H UG/M**3,/)                                                     PLV09320
      IF(NC2.EQ.1) GO TO 387                                            PLV09330
      WRITE(6,10)                                                       PLV09340
      WRITE(6,378)                                                      PLV09350
  378 FORMAT(///5X,  'GEOMETRY OF USER-SPECIFIED PLUME-OBSERVER-SUN ORIEPLV09360
     1NTATION',//)                                                      PLV09370
      WIND=WIND/RAD+180.                                                PLV09380
      WRITE(6,379)WIND                                                  PLV09390
  379 FORMAT(10X,26HWIND DIRECTION (DEGREES) =,F5.1/)                   PLV09400
387   WRITE(6,271)                                                      PLV09410
c***jcv NPS 6/18/96
	if (timflg) then
	  print 22210
	  print *,' WARNING:  The time has been reset from ',oldtime
	  print *,'     to ',time,'to avoid zenith angles greater than',
     +        zmax
	  print 22210
	  write (6,22210)
	  write (6,22211)oldtime
	  write (6,22212)time,zmax
	  write (6,22210)
	endif
22210 format (  '*******************************************************
     +*******************')
22211 format (' WARNING:  The time has been reset from ',f6.0)
22212 format ('     to ',f6.0,' to avoid zenith angles greater than',
     +        f6.2)
c***End of JCV modification
      WRITE(6,380) TIME,IMO,IDAY                                        PLV09420
  380 FORMAT(10X,18HSIMULATION IS FOR ,F5.0,10H HOURS ON ,I2,1H/,I2,/)  PLV09430
      WRITE(6,381)ZENITH                                                PLV09440
  381 FORMAT(10X,30HSOLAR ZENITH ANGLE (DEGREES) =,F5.1,/)              PLV09450
      WRITE(6,382)SUNAZ                                                 PLV09460
  382 FORMAT(10X,31HSOLAR AZIMUTH ANGLE (DEGREES) =,F8.1,///)           PLV09470
C                                                                       PLV09480
C    SKIP OVER SPECIFIC CASE DATA IF RUN IS FOR A PLUME-BASED           PLV09490
C    CALCULATION ONLY.                                                  PLV09500
C                                                                       PLV09510
      IF(NC2.EQ.1) GO TO 386                                            PLV09520
      WRITE(6,383)                                                      PLV09530
  383 FORMAT(10X,  'GEOMETRIES FOR LINES-OF-SIGHT THROUGH PLUME PARCELS PLV09540
     1AT GIVEN DOWNWIND DISTANCES (X)',//,4X,6HX (KM),3X, 7HAZIMUTH,    PLV09550
     2 8X,2HRP,5X,5HALPHA,6X,4HBETA,5X,5HTHETA)                         PLV09560
      DO 385 NX=1,NX2                                                   PLV09570
      XPLUME=DIST(NX)/1000.                                             PLV09580
      WRITE(6,384)XPLUME,AZMUTH(NX),OBSPLU(NX),AALPHA(NX),ABETA(NX),    PLV09590
     1 TT(NX+7)                                                         PLV09600
  384 FORMAT(6F10.1)                                                    PLV09610
  385 CONTINUE                                                          PLV09620
  386 CONTINUE                                                          PLV09630
C                                                                       PLV09640
C*****                                                                  PLV09650
C***  WRITE OUT THE HEADER PAGE                                         PLV09660
C*****                                                                  PLV09670
      WRITE(6,10000)                                                    PLV09680
10000 FORMAT(1H1,30X,21HBACKGROUND CONDITIONS,//)                       PLV09690
      WRITE(6,10200)                                                    PLV09700
10200 FORMAT(1H ,4X,17HACCUMULATION MODE,24X,20HCOARSE PARTICLE MODE,23XPLV09710
     1,21HPRIMARY PARTICLE MODE)                                        PLV09720
      WRITE(6,10300)                                                    PLV09730
10300 FORMAT(1H ,4X,11HMASS RADIUS,4X,5HSIGMA,3X,13HBSCAT.55/MASS,8X,   PLV09740
     1 11HMASS RADIUS,4X,5HSIGMA,3X,13HBSCAT.55/MASS,8X,11HMASS RADIUS, PLV09750
     2 4X,5HSIGMA,3X,13HBSCAT.55/MASS)                                  PLV09760
      WRITE(6,10400) ROVA,SIGA,BTASO4(19),ROVC,SIGC,BTACOR(19),ROVP,    PLV09770
     1SIGP,BTAPRM(19)                                                   PLV09780
10400 FORMAT(1H ,3E13.4,6X,3E13.4,5X,3E13.4)                            PLV09790
10600 FORMAT(1H ,3X,5HNOX =,E10.4,7H  NO2 =,E10.4,7H  NO3 =,E10.4, '  SOPLV09800
     12 =',E10.4,7H  SO4 =,E10.4,6H  O3 =,E10.4,5HACC =,E10.4,5HCOR =,  PLV09810
     1E10.4)                                                            PLV09820
      WRITE(6,10010)                                                    PLV09830
      WRITE(6,10011) RFRSO4,RFISO4                                      PLV09840
      WRITE(6,10012) RFRCOR,RFICOR                                      PLV09850
      WRITE(6,10013) RFRPRM,RFIPRM                                      PLV09860
      WRITE(6,10014) RFRCAR,RFICAR                                      PLV09870
10010 FORMAT(///,50X,18HREFRACTION INDEXES,//)                          PLV09880
10011 FORMAT(5X,24HACCUMULATION MODE     = ,E13.4,5H + I ,E13.4,/)      PLV09890
10012 FORMAT(5X,24HCOARSE MODE           = ,E13.4,5H + I ,E13.4,/)      PLV09900
10013 FORMAT(5X,24HPRIMARY AEROSOLS      = ,E13.4,5H + I ,E13.4,/)      PLV09910
10014 FORMAT(5X,24HCARBONACEOUS AEROSOLS = ,E13.4,5H + I ,E13.4,/)      PLV09920
      WRITE(6,271)                                                      PLV09930
      WRITE(6,10700)                                                    PLV09940
10700 FORMAT(1H ,30X,40HCOEFFICIENTS AT 0.55 MICROMETERS , 1./KM)       PLV09950
      BTARAY = 0.01162*EXP(-ELEV/9.8)                                   PLV09960
C                                                                       PLV09970
C     BTARAY = RAYLEIGH SCATTERING COEFFICIENT.                         PLV09980
C                                                                       PLV09990
      ABNO2T = ABSNO2(19)*AMBNO2                                        PLV10000
C                                                                       PLV10010
C********************************************************************** PLV10020
C    ABNO2T = ABSORPTION COEFFICIENT FOR NO2 AT .55 MICROMETERS         PLV10030
C    BTAAER(19) = BACKGROUND AEROSOL EXTINCTION COEFFICIENT AT          PLV10040
C    .55 MICROMETERS.                                                   PLV10050
C    BTABAC(19) = BACKGROUND EXTINCTION COEFFICIENT AT .55              PLV10060
C    MICROMETERS.  THIS SUMS THE RAYLEIGH AND AEROSOL SCATTERING        PLV10070
C    AND NO2 ABSORPTION.                                                PLV10080
C********************************************************************** PLV10090
C                                                                       PLV10100
      WRITE(6,10800 ) BTARAY,BTAAER(19),ABNO2T,BTABAC(19)               PLV10110
10800 FORMAT(1H ,6X,8HBTARAY =,E10.4,3X,8HBTAAER =,E10.4,3X,8HABSNO2 =, PLV10120
     1 E10.4,3X,8HBTABAC =,E10.4,//)                                    PLV10130
C                                                                       PLV10140
C    CALCULATE PERFECT DIFFUSE REFLECTOR PROPERTIES.                    PLV10150
C                                                                       PLV10160
      CALL PERDIF(SPECR,ZENITH)                                         PLV10170
40025 FORMAT(/,5X,17HELEVATION (KM) = ,F5.1,5X,20HVISUAL RANGE (KM) = , PLV10180
     1F5.0,5X,15HACCUMULATION = ,F5.1,5X,17HNO2 CONC (PPM) = ,F5.2,/)   PLV10190
      IF(NC1.EQ.2)GO TO 44121                                           PLV10200
      DO 40100 I=1,6                                                    PLV10210
C                                                                       PLV10220
C    LOOP FOR 6 PLUME-BASED SCATTERING ANGLES                           PLV10230
C                                                                       PLV10240
         ITHETA =I+1                                                    PLV10250
         THETA=TT(ITHETA)                                               PLV10260
         IFLAGP=0                                                       PLV10270
C                                                                       PLV10280
C    LOOP FOR 6 PLUME-BASED SIGHTLINE ELEVATION ANGLES                  PLV10290
C                                                                       PLV10300
         DO 40100 IBETA=1,7                                             PLV10310
            BETA=FLOAT(IBETA-1)*15.                                     PLV10320
C                                                                       PLV10330
C    SPECR = RAYLEIGH ATMOSPHERE INTENSITIES                            PLV10340
C    SPECB = BACKGROUND ATMOSPHERE INTENSITIES.                         PLV10350
C    CALCULATE RAYLEIGH ATMOSPHERE INTENSITIES FOR 39 WAVELENGTHS       PLV10360
C                                                                       PLV10370
            CALL RAYREF (ZENITH,BETA,THETA, SPECR)                      PLV10380
C                                                                       PLV10390
C    CALCULATE BACKGROUND ATMOSPHERE INTENSITIES FOR 39 WAVELENGTHS     PLV10400
C                                                                       PLV10410
            CALL BACCLN(ZENITH, BETA, THETA,ITHETA, SPECB)              PLV10420
C                                                                       PLV10430
C    CALCULATE VISUAL PERCEPTIBILITY PARAMETERS                         PLV10440
C                                                                       PLV10450
            CALL CHROMA(SPECB,SPECR)                                    PLV10460
            IF(BETA.EQ.0.) GO TO 40031                                  PLV10470
C                                                                       PLV10480
C    CORRECT BACKGROUND ATMOSPHERE OPTICAL DEPTH FOR NON-VERTICAL       PLV10490
C    VIEWING ANGLE.                                                     PLV10500
C                                                                       PLV10510
            TAU=TAT0IZ(19)/SIN(BETA*RAD)                                PLV10520
            GO TO 40032                                                 PLV10530
C                                                                       PLV10540
C    OPTICAL DEPTH FOR HORIZONTAL VIEW.                                 PLV10550
C                                                                       PLV10560
40031       TAU=TAUT0D(19)+TAUTDI(19)                                   PLV10570
40032       CONTINUE                                                    PLV10580
            IF(IFLAGP.EQ.0) GO TO 40040                                 PLV10590
            GO TO 40050                                                 PLV10600
40040       CONTINUE                                                    PLV10610
            IFLAGP = 1                                                  PLV10620
40050       CONTINUE                                                    PLV10630
40100 CONTINUE                                                          PLV10640
C                                                                       PLV10650
C------LOOP FOR 6 PLUME-BASED SCATTERING ANGLES                         PLV10660
C                                                                       PLV10670
      IFLAGQ=1                                                          PLV10680
      DO 40200 I=1,6                                                    PLV10690
         IFLAGQ=-1*IFLAGQ                                               PLV10700
C                                                                       PLV10710
C    ASSIGN SCATTERING ANGLE                                            PLV10720
C                                                                       PLV10730
         ITHETA=I+1                                                     PLV10740
         THETA=TT(ITHETA)                                               PLV10750
C                                                                       PLV10760
C    CALCULATE BACKGROUND ATMOSPHERE INTENSITIES WITHOUT PLUME          PLV10770
C                                                                       PLV10780
         CALL BACCLN(ZENITH, 0., THETA,ITHETA, SPECB)                   PLV10790
C                                                                       PLV10800
C    LOOP FOR DIFFERENT SHADES OF BACKGROUND OBJECTS.                   PLV10810
C                                                                       PLV10820
         DO 40200 K=1,3                                                 PLV10830
C                                                                       PLV10840
C    K = 1 FOR WHITE OBJECTS, 2 FOR GRAY, 3 FOR BLACK                   PLV10850
C                                                                       PLV10860
            XLUMIN=REFL(K)/(2.*PI)                                      PLV10870
            DO 40200 J=1,7                                              PLV10880
C                                                                       PLV10890
C    LOOP FOR 7 PLUME-BASED BACKGROUND OBJECT DISTANCES AS FUNCTION     PLV10900
C    OF AMBIENT VISUAL RANGE.                                           PLV10910
C                                                                       PLV10920
               RO=ROBJ(J)*RVAMB                                         PLV10930
C                                                                       PLV10940
C    CALCULATE INTENSITIES FOR BACKGROUND OBJECT WITHOUT PLUME.         PLV10950
C                                                                       PLV10960
               CALL BACOBJ(ZENITH,BETA,THETA,ITHETA,RO,SPECB,XLUMIN)    PLV10970
C                                                                       PLV10980
C   CALCULATE DIFFERENCE IN COLOR OF OBJECTS VIEWED THROUGH             PLV10990
C   THE BACKGROUND ATMOSPHERE COMPARED WITH THAT VIEWED                 PLV11000
C   THROUGH A RAYLEIGH ATMOSPHERE.                                      PLV11010
C                                                                       PLV11020
               CALL CHROMA(SPECB,SPECR)                                 PLV11030
40200 CONTINUE                                                          PLV11040
44120 CONTINUE                                                          PLV11050
44121 CONTINUE                                                          PLV11060
C                                                                       PLV11070
C   CONVERSION FACTOR FROM G/S TO MICRO-G/SEC AT REFERENCE CONDITIONS (1PLV11080
C   ATMOSPHERE) .  FOR ONE STACK INSTEAD OF TOTAL STACK EMISSIONS.      PLV11090
C                                                                       PLV11100
      C2 = ((1.E6/UNITS)/PAMB)*(FGTEMP/298.)                            PLV11110
C                                                                       PLV11120
C CONVERT G/S TO PPM-CU M/S FOR GASES AND TO MICRO-G/CU M - CU M/S FOR  PLV11130
C    PARTICULATES.                                                      PLV11140
      QSO2  = QSO2*C2* 3.821E-4                                         PLV11150
      QNOX  = QNOX*C2* 5.315E-4                                         PLV11160
      QPART = QPART*C2                                                  PLV11170
      QO2   = FGO2*1.E4*FLOW                                            PLV11180
      PI = 3.14159                                                      PLV11190
      IF(IDILU .EQ.0)GO TO 45                                           PLV11200
      WRITE(6,10)                                                       PLV11210
      WRITE (6,41) (PLANT(J),J=1,6)                                     PLV11220
41    FORMAT (35X, 'INITIAL PLUME RISE AND DILUTION AND NITROGEN DIOXIDEPLV11230
     1 FORMATION'//45X,6A4//2X,4HTIME,5X,1HX,5X,7HDELTA H,4X,1HU,7X,1HW,PLV11240
     17X,1HV,4X,5HSIGMA,4X,4HTEMP,5X,2HO2,4X,12HNO2-NO RATIO,5X,3HNOX,  PLV11250
     16X,2HNO,5X,4HNO2T,4X,3HSO2,3X,11HPARTICULATE/2X,5H(SEC),3X,3H(M), PLV11260
     15X,3H(M),5X,5H(M/S),3X,5H(M/S),3X,5H(M/S),3X,3H(M),5X,3H(K),4X,   PLV11270
     15HMOL P,3X,5HEQUIL,3X,6HACTUAL,2X,5H(PPM),4X,5H(PPM),2X,5H(PPM),  PLV11280
     13X,5H(PPM),3X,5HUG/M3/)                                           PLV11290
42    FORMAT (F7.0,2F8.1,3F8.2,3F8.1,1P2E8.1,0P4F8.3,1PE10.2)           PLV11300
C                                                                       PLV11310
C     CALCULATE THE INTEGER CLOSEST TO THE NO.OF SECONDS AFTER EMISSION PLV11320
C     THAT PLUME IS TRANSPORTED 1 KM                                    PLV11330
C                                                                       PLV11340
45    NSECF=1000./U                                                     PLV11350
C                                                                       PLV11360
4300  CONTINUE                                                          PLV11370
C                                                                       PLV11380
C------DO LOOP LAGRANGIAN FRAME OF REFERENCE (EVERY 10 SECONDS)         PLV11390
C                                                                       PLV11400
      NSECF1=NSECF+1                                                    PLV11410
      DO 50 NSEC = 1,NSECF1,10                                          PLV11420
         T = NSEC-1                                                     PLV11430
         IF(T.EQ.0.0) GO TO 43                                          PLV11440
C                                                                       PLV11450
C    CALCULATION OF PARCEL VELOCITY, POSITION,DILUTION FACTOR           PLV11460
C    POWER LAW RISE OF PLUME TO EFFECTIVE STACK HEIGHT.                 PLV11470
C    CHANGE FOR F = 0.0                                                 PLV11480
C                                                                       PLV11490
         IF(XFINAL.EQ.0.0)GO TO 92                                      PLV11500
         DELH=RISE*(U*T/XFINAL)**.66667                                 PLV11510
         GO TO 93                                                       PLV11520
92       DELH=0.0                                                       PLV11530
93       CONTINUE                                                       PLV11540
C                                                                       PLV11550
C    CORRECT DELH IF U*T > XFINAL                                       PLV11560
C                                                                       PLV11570
         IF(DELH.GT.RISE) DELH=RISE                                     PLV11580
C                                                                       PLV11590
C    CALCULATE VERTICAL VELOCITY VIA POWER LAW                          PLV11600
C                                                                       PLV11610
         W=.66667*RISE*FLOAT(NSECF)**(-0.66667)*FLOAT(NSEC)**(-.33333)  PLV11620
C                                                                       PLV11630
C    W =WMAX AT STACK, W = 0 AT XFINAL                                  PLV11640
C                                                                       PLV11650
         IF(NSEC.GT.NSECF) W=0.                                         PLV11660
         IF (W.GT.WMAX) W=WMAX                                          PLV11670
C                                                                       PLV11680
C    VELOCITY VECTOR AT NSEC.                                           PLV11690
C                                                                       PLV11700
         V = SQRT(U**2.+W**2.)                                          PLV11710
         DENOM = DELH**2*V*(1.57/2.15)/2.15                             PLV11720
         IF (DENOM.LT.FLOW) DENOM=FLOW                                  PLV11730
         GO TO 44                                                       PLV11740
C                                                                       PLV11750
C     PROPERTIES AT TOP OF STACK                                        PLV11760
C                                                                       PLV11770
43       DELH = 0.0                                                     PLV11780
         W = WMAX                                                       PLV11790
         DENOM = FLOW                                                   PLV11800
         V = W                                                          PLV11810
C                                                                       PLV11820
C     CALCULATION OF PLUME TEMPERATURE                                  PLV11830
C                                                                       PLV11840
44       TEMP = TAMB*DENOM/(DENOM-FLOW*(1.-(TAMB/FGTEMP)))              PLV11850
C                                                                       PLV11860
C     DOWNWIND DISTANCE FROM STACK IN METERS                            PLV11870
C                                                                       PLV11880
         XM = U*T                                                       PLV11890
C                                                                       PLV11900
C     HORIZONTAL OR VERTICAL DISPERSION COEFFICIENT = 1/2.15 OF PLUME   PLV11910
C     RADIUS.  RADIUS = 1/2 OF PLUME RISE.                              PLV11920
C                                                                       PLV11930
         SIG = (DELH/2.)/2.15                                           PLV11940
C                                                                       PLV11950
C     O2 CONCENTRATION IN PPM IN PLUME IS NEEDED TO                     PLV11960
C     CALCULATE NO2 FORMATION                                           PLV11970
C                                                                       PLV11980
         XO2 = 209460. + (QO2 - FLOW*209460.)*TEMP/(DENOM*FGTEMP)       PLV11990
C                                                                       PLV12000
C     O2 IN MOL PERCENT                                                 PLV12010
C                                                                       PLV12020
         PO2 = XO2*1.E-4                                                PLV12030
C                                                                       PLV12040
C     CALCULATION OF EQUILIBRIUM NO2/NO RATIO                           PLV12050
C                                                                       PLV12060
         C3 = 16.786 + (8.841 - 16.786)*(TEMP-298.)/(500.-298.)         PLV12070
         RATIOE = SQRT(10.**C3*4.09E-11*XO2)                            PLV12080
C                                                                       PLV12090
C     ACTUAL NO2/NO RATIO.                                              PLV12100
C     INITIALLY = EQUILIBRIUM. NEVER GREATER THAN EQUIL                 PLV12110
C                                                                       PLV12120
         IF(T.EQ.0.0) RATIOA= 8.03E-12*EXP(526.4/FGTEMP)*QNOX/FLOW      PLV12130
     1                     *XO2*HSTACK/W                                PLV12140
         IF(RATIOA.GT.RATIOE) RATIOA = RATIOE                           PLV12150
C                                                                       PLV12160
C     POLLUTANT CONCENTRATIONS.                                         PLV12170
C     NO2 RESULTING FROM TERMOLECULAR REACTION ONL                      PLV12180
C                                                                       PLV12190
         XNOX = QNOX*TEMP/(DENOM*FGTEMP)                                PLV12200
         XSO2 = QSO2*TEMP/(DENOM*FGTEMP)                                PLV12210
         XPART = QPART*TEMP/(DENOM*FGTEMP)                              PLV12220
         XNO = XNOX/(1.+RATIOA)                                         PLV12230
         XNO2T = XNO*RATIOA                                             PLV12240
         IF(IDILU .EQ. 0)GO TO 50                                       PLV12250
         WRITE (6,42) T,XM,DELH,U,W,V,SIG,TEMP,PO2,RATIOE,RATIOA,       PLV12260
     1   XNOX,XNO,XNO2T,XSO2,XPART                                      PLV12270
C                                                                       PLV12280
C     CALCULATION OF CHANGE IN NO2/NO RATIO VIA                         PLV12290
C     TERMOLECULAR REACTION IN 10                                       PLV12300
C                                                                       PLV12310
         RATIOA=RATIOA+4.015E-12*EXP(1046./(1.987*TEMP))*XNO*XO2*10.    PLV12320
50    CONTINUE                                                          PLV12330
C                                                                       PLV12340
C     EMISSION RATES PER STACK ARE CONVERTED BACK TO TOTALS.            PLV12350
C                                                                       PLV12360
      QSO2 = QSO2*UNITS                                                 PLV12370
      QNOX = QNOX*UNITS                                                 PLV12380
      QPART = QPART*UNITS                                               PLV12390
C                                                                       PLV12400
C********************************************************************** PLV12410
C     CALCULATION OF VIRTUAL POINT SOURCE DISTANCE OFFSET               PLV12420
C     SO THAT CALCULATED SIGMA-X AND SIGMA-Y MATCH INITIAL              PLV12430
C     PLUME DILUTION RESULTS AT  1 KILOMETER.                           PLV12440
C                                                                       PLV12450
C     IS = STABILITY CLASS                                              PLV12460
C                                                                       PLV12470
C********************************************************************** PLV12480
C                                                                       PLV12490
      I=IS                                                              PLV12500
      IF(IDIS.EQ.9)GO TO 476                                            PLV12510
      DILUTE=((RISE*RISE/4.)/2.15)/2.15                                 PLV12520
      XM=XFINAL                                                         PLV12530
      SIGYXZ=0.                                                         PLV12540
470   CONTINUE                                                          PLV12550
      IF(IDIS.EQ.1) GO TO 471                                           PLV12560
C                                                                       PLV12570
C-----Note GEM subsituted  SY,SZ function calls for ISC routines        PLV12580
      CALL SIGY(XM,I,SSY)                                               PLV12590
      CALL SIGZ(XM,I,SSZ)                                               PLV12600
      SY = SSY                                                          PLV12610
      SZ = SSZ                                                          PLV12620
      GO TO 472                                                         PLV12630
471   SY=SYTVA(I,XM)                                                    PLV12640
      SZ=SZTVA(I,XM)                                                    PLV12650
472   SIGYZI=SIGYXZ                                                     PLV12660
      SIGYXZ=SY*SZ                                                      PLV12670
      IF(DILUTE.LE.SIGYXZ.AND.SIGYZI.EQ.0.)GO TO 476                    PLV12680
      IF(DILUTE.LE.SIGYXZ)GO TO 475                                     PLV12690
      XM=XM+1.E4                                                        PLV12700
      GO TO 470                                                         PLV12710
475   XVIRTL=XM-XFINAL-1.E4+1.E4*(DILUTE-SIGYZI)/(SIGYXZ-SIGYZI)        PLV12720
      GO TO 477                                                         PLV12730
476   XVIRTL=0.                                                         PLV12740
477   CONTINUE                                                          PLV12750
C                                                                       PLV12760
C     CALCULATE PHOTOLYSIS RATE CONSTANTS FOR EACH OF 24 HOURLY         PLV12770
C     PERIODS IN DAY.                                                   PLV12780
C     CONVERT STACK UTM COORDINATES TO LATITUDE AND LONGITUDE.          PLV12790
C                                                                       PLV12800
      CALL MAPUTG(XSTACK,YSTACK,IZONE,ALON,ALAT)                        PLV12810
      DO 49 I=1,24                                                      PLV12820
C                                                                       PLV12830
C    TIMER = HOURS OF DAY (MILITARY CLOCK)                              PLV12840
C                                                                       PLV12850
         XTIMER=FLOAT(I)*100.                                           PLV12860
C                                                                       PLV12870
C    CALCULATE SUN ELEVATION ANGLE                                      PLV12880
C                                                                       PLV12890
         CALL SOLARZ(ALAT,ALON,TZONE,IYEAR,IMO,IDAY,XTIMER,SUNELE,5)    PLV12900
         AZEN=90.-SUNELE                                                PLV12910
         IF(AZEN.GT.90.)AZEN=90.                                        PLV12920
         PHIKKR(I)=0.0                                                  PLV12930
         QJ(I)=0.0                                                      PLV12940
         IF(AZEN.EQ.90.)GO TO 48                                        PLV12950
C                                                                       PLV12960
C    DYNAMIC EQUILIBRIUM CONSTANT (NO2 AND NO)                          PLV12970
C                                                                       PLV12980
         PHIKKR(I)=(1.E-2/0.44)*EXP(-0.38/COS(AZEN*RAD))                PLV12990
C                                                                       PLV13000
C    OZONE PHOTOLYSIS RATE CONSTANT                                     PLV13010
C                                                                       PLV13020
         QJ(I)=2.23E-5*60.*(COS(AZEN*RAD))**2.74                        PLV13030
48       CONTINUE                                                       PLV13040
49    CONTINUE                                                          PLV13050
C                                                                       PLV13060
C **********************************************************************PLV13070
C                                                                       PLV13080
C     DO LOOP FOR POLLUTANT CONCENTRATIONS AND VISUAL EFFECTS AT        PLV13090
C     16 DOWNWIND DISTANCES                                             PLV13100
C                                                                       PLV13110
C **********************************************************************PLV13120
C                                                                       PLV13130
      NZFLAG=0                                                          PLV13140
C                                                                       PLV13150
C     INITIALIZE EFFECTIVE EMISSION RATES FOR SO2, NOX, SO4, NO3, AND   PLV13160
C     PRIMARY PARTICULATE, AND CONVERSION RATES FOR SO2, NOX, AND NO2.  PLV13170
C                                                                       PLV13180
      DO 495 NZ=1,6                                                     PLV13190
         DO 495 NT=NX1,NX2                                              PLV13200
            QSO2TR(NZ,NT)=QSO2                                          PLV13210
            QNOXTR(NZ,NT)=QNOX                                          PLV13220
            QSO4TR(NZ,NT)=0.                                            PLV13230
            QNO3TR(NZ,NT)=0.                                            PLV13240
            QPARTT(NZ,NT)=QPART                                         PLV13250
            RSO2R(1,NZ,NT)=RSO2(1)                                      PLV13260
            RNOXR(1,NZ,NT)=0.                                           PLV13270
            RNO2X(1,NZ,NT)=0.                                           PLV13280
C                                                                       PLV13290
C    SET NO2/NO RATIO TO INITIAL VALUE CALCULATED EARLIER. SET TO ZERO  PLV13300
C    AT SURFACE.                                                        PLV13310
C                                                                       PLV13320
            RATIOT(NZ,NT)=RATIOA                                        PLV13330
            IF(NZ.EQ.6)RATIOT(NZ,NT)=0.                                 PLV13340
495   CONTINUE                                                          PLV13350
      CHIQIY(6)=0.                                                      PLV13360
      QPART0=QPART                                                      PLV13370
C                                                                       PLV13380
C    INITIALIZE VALUE FOR DOWNWIND DISTANCE OF PREVIOUS POINT.          PLV13390
C                                                                       PLV13400
      XM=0.                                                             PLV13410
C                                                                       PLV13420
C    INITIALIZE TOTAL DEPOSITION FOR SO2, NOX, PRIMARY PARTICULATE,     PLV13430
C    SULFATE AND NITRATE.                                               PLV13440
C                                                                       PLV13450
      TDSO2=0.                                                          PLV13460
      TDNOX=0.                                                          PLV13470
      TDPART=0.                                                         PLV13480
      TDSO4=0.                                                          PLV13490
      TDNO3=0.                                                          PLV13500
C                                                                       PLV13510
C    *****LOOP ON DOWNWIND DISTANCE *******                             PLV13520
C                                                                       PLV13530
      DO 1000 NX = NX1,NX2                                              PLV13540
C                                                                       PLV13550
C    XM0 = DOWNWIND DISTANCE OF PREVIOUS POINT.                         PLV13560
C                                                                       PLV13570
          XM0=XM                                                        PLV13580
C                                                                       PLV13590
C    XM = DOWNWIND DISTANCE (METERS)                                    PLV13600
C                                                                       PLV13610
          XM=DIST(NX)                                                   PLV13620
C                                                                       PLV13630
C    XKM = DOWNWIND DISTANCE (KILOMETERS)                               PLV13640
C                                                                       PLV13650
          XKM=XM/1000.                                                  PLV13660
C                                                                       PLV13670
C   ADD IN VIRTUAL POINT SOURCE OFFSET TO ACCOUNT FOR INITIAL DILUTION  PLV13680
C   DURING PLUME RISE.                                                  PLV13690
C                                                                       PLV13700
          XADD=XM+XVIRTL                                                PLV13710
C                                                                       PLV13720
C    IS = STABILITY                                                     PLV13730
C                                                                       PLV13740
          I = IS                                                        PLV13750
C                                                                       PLV13760
C   NXSTAB = DOWNWIND DISTANCE INDEX FOR DISTANCE WHERE STABILITY CHANGEPLV13770
C                                                                       PLV13780
          IF(NX.GT.NXSTAB)GO TO 135                                     PLV13790
          IF(IDIS.NE.9) GO TO 510                                       PLV13800
C                                                                       PLV13810
C    IDIS = 9 FOR READING IN SPECIAL SIGMA VALUES FOR EACH DOWNWIND     PLV13820
C    DISTANCE                                                           PLV13830
C                                                                       PLV13840
          READ(5,5) SY,SZ                                               PLV13850
          GO TO 136                                                     PLV13860
 510      CONTINUE                                                      PLV13870
          IF(IDIS.EQ.1) GO TO 511                                       PLV13880
C                                                                       PLV13890
C    PASQUILL-GIFFORD SIGMAS (I=1 FOR "A", 2 FOR "B", 3 FOR "C", ETC)   PLV13900
C                                                                       PLV13910
C-----Note substituted ISC sigma routines for the SY,SZ function routinePLV13920
          CALL SIGY(XADD,I,SSY)                                         PLV13930
          CALL SIGZ(XADD,I,SSZ)                                         PLV13940
          SY = SSY + (YINITL/2.)/PI                                     PLV13950
          SZ = SSZ + (ZINITL/2.)/PI                                     PLV13960
          GO TO 512                                                     PLV13970
C                                                                       PLV13980
C    TVA SIGMAS                                                         PLV13990
C                                                                       PLV14000
 511      SY=SYTVA(I,XADD)+(YINITL/2.)/PI                               PLV14010
          SZ=SZTVA(I,XADD)+(ZINITL/2.)/PI                               PLV14020
 512      CONTINUE                                                      PLV14030
C                                                                       PLV14040
C    IF UPWIND OF STABILITY CHANGE, JUMP AROUND                         PLV14050
C                                                                       PLV14060
          IF(NX.LT.NXSTAB)GO TO 136                                     PLV14070
C                                                                       PLV14080
C********************************************************************** PLV14090
C   NXSTAB=DOWNWIND DISTANCE INDEX WHERE STABILITY CHANGES              PLV14100
C    NXSTAB MUST BE GREATER THAN 1                                      PLV14110
C   I=STABILITY INDEX FOR FIRST PART OF PLUME.                          PLV14120
C   INEW=NEW STABILITY INDEX                                            PLV14130
C   SYNEW=SIGMA-Y FOR NEW STABILITY.                                    PLV14140
C   SZNEW=SIGMA-Z FOR NEW STABILITY.                                    PLV14150
C   CODE FOR NEW STABILITY.  FIRST DETERMINE VIRTUAL DOWNWIND           PLV14160
C   DISTANCE FOR NEW STABILITY TO AVOID LARGE PLUME DIMENSIONAL         PLV14170
C   DISCONTINUITY AT STABILITY INTERFACE.                               PLV14180
C********************************************************************** PLV14190
C                                                                       PLV14200
100      CONTINUE                                                       PLV14210
         IF(IDIS.EQ.1)GO TO 101                                         PLV14220
         DISTA=5000.                                                    PLV14230
C                                                                       PLV14240
C-----Note substituted ISC sigy routine for old SY function call        PLV14250
C-----(despite the fact that this portion of the code has been          PLV14260
C-----disabled)                                                         PLV14270
         CALL SIGY(DISTA,INEW,SSY)                                      PLV14280
         SYNEW=SSY                                                      PLV14290
         DELSIG=SYNEW-SY                                                PLV14300
C                                                                       PLV14310
C********************************************************************** PLV14320
C    FIND VIRTUAL DISTANCE FOR NEW STABILITY WHERE DIFFERENCE IN        PLV14330
C    SIGMA-Y FOR INITIAL STABILITY AND SIGMA-Y AT NEW STABILITY         PLV14340
C    STOPS DECREASING AND STARTS INCREASING, WITH A DISTANCE            PLV14350
C    INTERVAL OF 1 KM. ASSUME SYNEW IS FIRST LESS THAN SY, THEN AT SOME PLV14360
C    DISTANCE GOES THROUGH ZERO AND STARTS INCREASING.                  PLV14370
C********************************************************************** PLV14380
C                                                                       PLV14390
140      DISTB=DISTA                                                    PLV14400
         DISTA=DISTA+5000.                                              PLV14410
         CALL SIGY(DISTA,INEW,SSY)                                      PLV14420
         SYNEW = SSY                                                    PLV14430
         DELSG=SYNEW-SY                                                 PLV14440
         IF(ABS(DELSG).GT.ABS(DELSIG))GO TO 141                         PLV14450
         DELSIG=DELSG                                                   PLV14460
         GO TO 140                                                      PLV14470
141      CONTINUE                                                       PLV14480
C                                                                       PLV14490
C    VIRTUAL DISTANCE FOR NEW STABILITY FOR SIGMA-Y IS BETWEEN          PLV14500
C    DISTB AND DISTA.                                                   PLV14510
C   CALCULATE DISTANCE OFFSET FOR FUTURE SY DETERMINATIONS.             PLV14520
C                                                                       PLV14530
         DDISTY=DISTB+5000.*((0.-DELSIG)/(DELSG-DELSIG))-DIST(NX)       PLV14540
C                                                                       PLV14550
C    SAME PROCEDURE FOR SIGMA-Z                                         PLV14560
C                                                                       PLV14570
         DISTA=5000.                                                    PLV14580
C                                                                       PLV14590
C-----Note substituted ISC sigz routine for old SZ function call        PLV14600
C-----(despite the fact that this portion of the code has been          PLV14610
C-----disabled)                                                         PLV14620
         CALL SIGZ(DISTA,INEW,SSZ)                                      PLV14630
         SZNEW = SSZ                                                    PLV14640
         DELSIG=SZNEW-SZ                                                PLV14650
142      DISTB=DISTA                                                    PLV14660
         DISTA=DISTA+5000.                                              PLV14670
         CALL SIGZ(DISTA,INEW,SSZ)                                      PLV14680
         SZNEW = SSZ                                                    PLV14690
         DELSG=SZNEW-SZ                                                 PLV14700
         IF(ABS(DELSG).GT.ABS(DELSIG))GO TO 143                         PLV14710
         DELSIG=DELSG                                                   PLV14720
         GO TO 142                                                      PLV14730
143      CONTINUE                                                       PLV14740
         DDISTZ=DISTB+5000.*((0.-DELSIG)/(DELSG-DELSIG))-DIST(NX)       PLV14750
         SY=SYNEW                                                       PLV14760
         SZ=SZNEW                                                       PLV14770
         GO TO 136                                                      PLV14780
C                                                                       PLV14790
C   SAME PROCEDURE FOR TVA SIGMA-Y                                      PLV14800
C                                                                       PLV14810
101      DISTA=5000.                                                    PLV14820
         SYNEW=SYTVA(INEW,DISTA)                                        PLV14830
         DELSIG=SYNEW-SY                                                PLV14840
144      DISTB=DISTA                                                    PLV14850
         DISTA=DISTA+5000.                                              PLV14860
         SYNEW=SYTVA(INEW,DISTA)                                        PLV14870
         DELSG=SYNEW-SY                                                 PLV14880
         IF(ABS(DELSG).GT.ABS(DELSIG))GO TO 145                         PLV14890
         DELSIG=DELSG                                                   PLV14900
         GO TO 144                                                      PLV14910
145      CONTINUE                                                       PLV14920
         DDISTY=DISTB+5000.*((0.-DELSIG)/(DELSG-DELSIG))-DIST(NX)       PLV14930
C                                                                       PLV14940
C    SAME PROCEDURE FOR TVA SIGMA-Z                                     PLV14950
C                                                                       PLV14960
         DISTA=5000.                                                    PLV14970
         SZNEW=SZTVA(INEW,DISTA)                                        PLV14980
         DELSIG=SZNEW-SZ                                                PLV14990
146      DISTB=DISTA                                                    PLV15000
         DISTA=DISTA+5000.                                              PLV15010
         SZNEW=SZTVA(INEW,DISTA)                                        PLV15020
         DELSG=SZNEW-SZ                                                 PLV15030
         IF(ABS(DELSG).GT.ABS(DELSIG))GO TO 147                         PLV15040
         DELSIG=DELSG                                                   PLV15050
         GO TO 146                                                      PLV15060
147      CONTINUE                                                       PLV15070
         DDISTZ=DISTB+5000.*((0.-DELSIG)/(DELSG-DELSIG))-DIST(NX)       PLV15080
135      IF(NX.LE.NXSTAB)GO TO 136                                      PLV15090
C                                                                       PLV15100
C********************************************************************** PLV15110
C    CODE  FOR CALCULATING SIGMA-Y AND SIGMA-Z DOWNWIND                 PLV15120
C    OF STABILITY CHANGE.                                               PLV15130
C    FIRST ADD DISTANCE OFFSET FOR NEW STABILITY BEFORE                 PLV15140
C    CALCULATING SY AND SZ                                              PLV15150
C********************************************************************** PLV15160
C                                                                       PLV15170
         XMY=DIST(NX)+DDISTY                                            PLV15180
         XMZ=DIST(NX)+DDISTZ                                            PLV15190
         IF(IDIS.EQ.1)GO TO 122                                         PLV15200
C                                                                       PLV15210
C-----Note substituted ISC sigy,z routine for old SY,SZ function call   PLV15220
C-----(despite the fact that this portion of the code has been          PLV15230
C-----disabled)                                                         PLV15240
         CALL SIGY(XMY,INEW,SSY)                                        PLV15250
         CALL SIGZ(XMZ,INEW,SSZ)                                        PLV15260
         SY = SSY                                                       PLV15270
         SZ = SSZ                                                       PLV15280
         GO TO 136                                                      PLV15290
122      SY=SYTVA(INEW,XMY)                                             PLV15300
         SZ=SZTVA(INEW,XMZ)                                             PLV15310
136      CONTINUE                                                       PLV15320
         WRITE (6,10)                                                   PLV15330
         WRITE (6,51) (PLANT(J),J=1,6)                                  PLV15340
51    FORMAT (25X,50HCONCENTRATIONS OF AEROSOL AND GASES CONTRIBUTED BY,PLV15350
     1//35X,6A4/)                                                       PLV15360
         WRITE (6,52) XKM                                               PLV15370
52       FORMAT(25H DOWNWIND DISTANCE (KM) =, F7.1)                     PLV15380
         WRITE (6,53) H                                                 PLV15390
53       FORMAT(19H PLUME ALTITUDE (M) ,5X,1H=,F7.0)                    PLV15400
         WRITE (6,54) SY                                                PLV15410
54       FORMAT(12H SIGMA Y (M),12X,1H=,F7.0)                           PLV15420
         WRITE (6,55) SZ                                                PLV15430
55       FORMAT(12H SIGMA Z (M),12X,1H=,F7.0)                           PLV15440
         WRITE(6,56) RSO2R(NX,3,NX)                                     PLV15450
 56      FORMAT(27H SO2-SO4 CONVERSION RATE=  ,F10.4,11H PERCENT/HR)    PLV15460
         WRITE(6,57) RNOXR(NX,3,NX)                                     PLV15470
 57      FORMAT(27H NOX-NO3 CONVERSION RATE=  ,F10.4,11H PERCENT/HR)    PLV15480
         WRITE(6,271)                                                   PLV15490
         IFLAGP=0                                                       PLV15500
C                                                                       PLV15510
C     CALCULATE DECREASE OF SO2 AND NOX FLUX AND INCREASE IN            PLV15520
C     PARTICULATE FLUX  DUE TO SULFATE AND NITRATE FORMATION.           PLV15530
C     ALSO, CALCULATE THE DECREASE IN FLUX DUE TO SURFACE DEPOSITION.   PLV15540
C                                                                       PLV15550
         NTT=0                                                          PLV15560
C      TIME IN HOURS AND DECIMAL FRACTION OF HOUR                       PLV15570
         TIMEHR=AINT(TIME/100.)+AMOD(TIME,100.)/60.                     PLV15580
C                                                                       PLV15590
C    CHECK IF FIRST DOWNWIND POINT                                      PLV15600
C                                                                       PLV15610
         IF(NX.EQ.NX1)GO TO 68                                          PLV15620
C                                                                       PLV15630
C    TIME IN HOURS FOR TRANSPORT FROM PREVIOUS (NX-1) DOWNWIND POINT    PLV15640
C                                                                       PLV15650
         DTIME=((DIST(NX)-DIST(NX-1))/U)/3600.                          PLV15660
         GO TO 69                                                       PLV15670
68       DTIME=0.                                                       PLV15680
C                                                                       PLV15690
C    TIME AT PREVIOUS DOWNWIND POINT                                    PLV15700
C                                                                       PLV15710
69       XTIMER=TIMEHR-DTIME                                            PLV15720
C                                                                       PLV15730
C    NTT STOPS SURFACE DEPOSITION AT NIGHT                              PLV15740
C                                                                       PLV15750
         IF(XTIMER.LT.7..OR.XTIMER.GT.18.)NTT=1                         PLV15760
C                                                                       PLV15770
C    CALCULATION OF DEPOSITION                                          PLV15780
C                                                                       PLV15790
         YGRND=CHIQIY(6)*1000.                                          PLV15800
         DEPOT=YGRND*(XM-XM0)                                           PLV15810
C                                                                       PLV15820
C   SUPPRESS SURFACE DEPOSITION AT NIGHT (ASSUME STABLE STRATIFICATION) PLV15830
C    DEPOSITION OF PRIMARY PARTICULATE                                  PLV15840
C                                                                       PLV15850
         DEPPAR=QPART*VDCOR*DEPOT*FLOAT(1-NTT)/100.                     PLV15860
C                                                                       PLV15870
C   ADJUST EFFECTIVE PARTICULATE EMISSION RATE FOR DEPOSITION           PLV15880
C                                                                       PLV15890
         QPART=QPART-DEPPAR                                             PLV15900
C                                                                       PLV15910
C    LOOP ON DOWNWIND DISTANCES FROM PRESENT POSITION (DIST(NX)) TO     PLV15920
C    FINAL POINT (DIST(NX2)).                                           PLV15930
C                                                                       PLV15940
         DO 570 NT=NX,NX2                                               PLV15950
C                                                                       PLV15960
C    CALCULATE DEPOSITION FOR SO2, NOX, SO4, AND NO3                    PLV15970
C                                                                       PLV15980
            DEPSO2=QSO2TR(6,NT)*VDSO2*DEPOT*FLOAT(1-NTT)/100.           PLV15990
            DEPNOX=QNOXTR(6,NT)*VDNOX*DEPOT*FLOAT(1-NTT)/100.           PLV16000
            DEPSO4=QSO4TR(6,NT)*VDSUB*DEPOT*FLOAT(1-NTT)/100.           PLV16010
            DEPNO3=QNO3TR(6,NT)*VDNOX*DEPOT*FLOAT(1-NTT)/100.           PLV16020
C                                                                       PLV16030
C    LOOP ON ALTITUDE                                                   PLV16040
C                                                                       PLV16050
            DO 568 NZ=1,6                                               PLV16060
              FORSO2=((QSO2TR(NZ,NT)*RSO2R(NX,NZ,NT)/3.6E5)/U)*(XM-XM0) PLV16070
              FORNOX=((QNOXTR(NZ,NT)*RNOXR(NX,NZ,NT)/3.6E5)/U)*(XM-XM0) PLV16080
     1        *RNO2X(NX,NZ,NT)                                          PLV16090
C                                                                       PLV16100
C    ADJUST EFFECTIVE SO2 AND NOX EMISSION RATES FOR CONVERSION TO      PLV16110
C    SECONDARY SPECIES AND FOR SURFACE DEPOSITION AFTER TRANSPORT       PLV16120
C    FROM DIST(NX-1) TO DIST(NX).                                       PLV16130
C                                                                       PLV16140
               QSO2TR(NZ,NT)=QSO2TR(NZ,NT)-FORSO2-DEPSO2                PLV16150
               QNOXTR(NZ,NT)=QNOXTR(NZ,NT)-FORNOX-DEPNOX                PLV16160
C                                                                       PLV16170
C    ADJUST EFFECTIVE SULFATE EMISSION RATE, INCLUDING SULFATE          PLV16180
C    FORMATION AND SURFACE DEPOSITION.  (ADJUST MASS FLUX)              PLV16190
C                                                                       PLV16200
               QSO4TR(NZ,NT)=QSO4TR(NZ,NT)                              PLV16210
     1         +1.5/3.821E-4*FORSO2-DEPSO4                              PLV16220
C                                                                       PLV16230
C  ADJUST NO3 MASS FLUX                                                 PLV16240
C                                                                       PLV16250
               QNO3TR(NZ,NT)=QNO3TR(NZ,NT)+FORNOX-DEPNO3                PLV16260
C                                                                       PLV16270
C     ADJUST PARTICULATE MASS FLUX FOR SULFATE FORMATION                PLV16280
C                                                                       PLV16290
               QPARTT(NZ,NT)=QPART+QSO4TR(NZ,NT)                        PLV16300
568          CONTINUE                                                   PLV16310
             IF(NT.NE.NX)GO TO 569                                      PLV16320
C                                                                       PLV16330
C    CALCULATE TOTAL DEPOSITION TO DIST(NX) FOR SO2, NOX, PARTICULATE,  PLV16340
C    SO4, AND NO3.                                                      PLV16350
C                                                                       PLV16360
             TDSO2=TDSO2+DEPSO2                                         PLV16370
             TDNOX=TDNOX+DEPNOX                                         PLV16380
             TDPART=TDPART+DEPPAR                                       PLV16390
             TDSO4=TDSO4+DEPSO4                                         PLV16400
             TDNO3=TDNO3+DEPNO3                                         PLV16410
C                                                                       PLV16420
C    CALCULATE RATIO OF TOTAL DEPOSITION TO EMISSION RATE               PLV16430
C                                                                       PLV16440
             FDSO2=TDSO2/QSO2                                           PLV16450
             FDNOX=TDNOX/QNOX                                           PLV16460
             FDPART=TDPART/QPART0                                       PLV16470
             FDSO4=((TDSO4/QSO2)/1.5)*3.821E-4                          PLV16480
             FDNO3=((TDNO3/QNOX)/1.35)*5.315E-4                         PLV16490
569          CONTINUE                                                   PLV16500
570       CONTINUE                                                      PLV16510
C                                                                       PLV16520
C    DO LOOP FOR 6 ALTITUDES:  2*SIGMA-Z+H, SIGMA-Z+H, H,               PLV16530
C    H -SIGMA-Z, H -2*SIGMA-Z, SURFACE                                  PLV16540
C                                                                       PLV16550
         DO 60 NZ = 1,6                                                 PLV16560
            RNZ = NZ                                                    PLV16570
            IF(NZ.EQ.6) GO TO 58                                        PLV16580
            Z = H + (3.-RNZ)*SZ                                         PLV16590
            IF(Z.LT.0.) Z=0.                                            PLV16600
            GO TO 59                                                    PLV16610
58          Z = 0.0                                                     PLV16620
59          CONTINUE                                                    PLV16630
C                                                                       PLV16640
C    GAUSSIAN DIFFUSION CALCULATION                                     PLV16650
C                                                                       PLV16660
            ARG1 = -0.5*(H+Z)*(H+Z)/SZ/SZ                               PLV16670
            ARG2 = -0.5*(H-Z)*(H-Z)/SZ/SZ                               PLV16680
C                                                                       PLV16690
C     PREVENTION OF EXPONENTIAL UNDERFLOW                               PLV16700
C-----Sigma - GEM - make sure I don't get an underflow!                 PLV16710
C                                                                       PLV16720
            IF(ARG1.LT.(-60.)) ARG1=-60.                                PLV16730
            IF(ARG2.LT.(-60.)) ARG2=-60.                                PLV16740
            CHIQ=((TAMB/FGTEMP)/(2.*PI*SY*SZ*U))*(EXP(ARG1)+EXP(ARG2))  PLV16750
            IF(CHIQ.LT.2.E-26)CHIQ=2.E-26                               PLV16760
C                                                                       PLV16770
C    CHECK IF EQUILIBRIUM PLUME CENTERLINE HEIGHT IS ABOVE THE TOP      PLV16780
C    OF THE MIXED LAYER.                                                PLV16790
C                                                                       PLV16800
            IF(H.GT.HPBLM) GO TO 594                                    PLV16810
C                                                                       PLV16820
C    VIRTUAL SOURCE TO ACCOUNT FOR REFLECTION OFF CAPPING LAYER.        PLV16830
C                                                                       PLV16840
            HPRIME=2.*HPBLM-H                                           PLV16850
C                                                                       PLV16860
C    IF Z IS GTR THAN TOP OF MIXED LAYER, CHANGE Z TO TOP OF MIXED      PLV16870
C    LAYER.                                                             PLV16880
C                                                                       PLV16890
            IF(Z.GT.HPBLM) Z=HPBLM                                      PLV16900
C                                                                       PLV16910
C    REVISED CALCULATION FOR PLUME BELOW INVERSION.                     PLV16920
C                                                                       PLV16930
            ARG1 = -0.5*(H+Z)*(H+Z)/SZ/SZ                               PLV16940
            ARG2 = -0.5*(H-Z)*(H-Z)/SZ/SZ                               PLV16950
            ARG3=-0.5*(HPRIME-Z)*(HPRIME-Z)/SZ/SZ                       PLV16960
            ARG4=-0.5*(HPRIME+Z)*(HPRIME+Z)/SZ/SZ                       PLV16970
C                                                                       PLV16980
C-----Sigma - GEM - make sure I don't get an underflow!                 PLV16990
C                                                                       PLV17000
            IF(ARG1.LT. -60. ) ARG3=-60.                                PLV17010
            IF(ARG2.LT. -60. ) ARG4=-60.                                PLV17020
            IF(ARG3.LT. -60. ) ARG3=-60.                                PLV17030
            IF(ARG4.LT. -60. ) ARG4=-60.                                PLV17040
            CHIQ=(TAMB/FGTEMP)/(2.*PI*SY*SZ*U)*(EXP(ARG1)+EXP(ARG2)+    PLV17050
     1      EXP(ARG3) + EXP(ARG4))                                      PLV17060
            IF(CHIQ.LT.2.E-26)CHIQ=2.E-26                               PLV17070
C                                                                       PLV17080
C    FORMULA FOR UNIFORMLY MIXED CHI/Q.  CONCENTRATION CONSTANT IN Z    PLV17090
C    FROM SURFACE TO INVERSION.                                         PLV17100
C                                                                       PLV17110
            CHIQ1=(TAMB/FGTEMP)/(SQRT(2.*PI)*SY*HPBLM*U)                PLV17120
C                                                                       PLV17130
C    BRANCH IF Z = PLUME CENTERLINE HEIGHT.                             PLV17140
C                                                                       PLV17150
            IF(NZ.EQ.3) GO TO 592                                       PLV17160
            IF(NZFLAG.EQ.1) CHIQ=CHIQ1                                  PLV17170
            GO TO 594                                                   PLV17180
C                                                                       PLV17190
C     IF GAUSSIAN PLUME CENTERLINE CHI/Q < CHI/Q FOR UNIFORMLY MIXED    PLV17200
C     CASE, SET FLAG TO USE CHI/Q FOR UNIFORMLY MIXED CASE.             PLV17210
C                                                                       PLV17220
 592        IF(CHIQ.LT.CHIQ1) NZFLAG=1                                  PLV17230
            IF(NZFLAG.EQ.1) CHIQ=CHIQ1                                  PLV17240
 594        CONTINUE                                                    PLV17250
C                                                                       PLV17260
C    BRANCH FOR FIRST DOWNWIND DISTANCE                                 PLV17270
C                                                                       PLV17280
            IF(NX.EQ.1) GO TO 596                                       PLV17290
C                                                                       PLV17300
C    MAXIMUM CHI/Q CANNOT EXCEED CHI/Q FOR CENTERLINE AT PREVIOUS       PLV17310
C    DOWNWIND POINT                                                     PLV17320
C                                                                       PLV17330
            XQMAX=XQ(3)                                                 PLV17340
            IF(CHIQ.GT.XQMAX) CHIQ=XQMAX                                PLV17350
 596        CONTINUE                                                    PLV17360
C                                                                       PLV17370
C    SAVE CHI/Q VALUES FOR ALL 6 LEVELS.                                PLV17380
C                                                                       PLV17390
            XQ(NZ)=CHIQ                                                 PLV17400
            CHIQIY(NZ)=CHIQ*SQRT(2.*PI)*SY                              PLV17410
            CHIQIY(NZ)=CHIQIY(NZ)/1000.                                 PLV17420
C                                                                       PLV17430
C********************************************************************** PLV17440
C    OH MODEL FOR SULFATE AND NITRATE FORMATION.  CALCULATE CONVERSION  PLV17450
C    FOR PLUME PARCEL AT PRESENT POSITION AS IT IS ADVECTED TO POINTS   PLV17460
C    FARTHER DOWNWIND WHERE CONCENTRATIONS ARE CALCULATED.              PLV17470
C  CALCULATE PLUME PARCEL CONCENTRATIONS CORRESPONDING TO THE PLUME     PLV17480
C  PARCEL AT DOWNWIND DISTANCE NX WHICH  GETS TRANSPORTED TO            PLV17490
C  DOWNWIND DISTANCE I. SO4 AND NO3 FORMATION RATES ARE CALCULATED FOR  PLV17500
C  THE TIME CORRESPONDING TO PARCEL DISTANCE I.                         PLV17510
C********************************************************************** PLV17520
C                                                                       PLV17530
C    TIME IN HOURS AND DECIMAL FRACTIONS                                PLV17540
C                                                                       PLV17550
            TIMEHR=AINT(TIME/100.)+AMOD(TIME,100.)/60.                  PLV17560
            DO 5960 I=NX,NX2                                            PLV17570
C                                                                       PLV17580
C    TIME FOR TRANSPORT FROM PRESENT POSITION TO POINT DOWNWIND (DIST(I)PLV17590
C                                                                       PLV17600
               DTIME=((DIST(I)-DIST(NX))/U)/3600.                       PLV17610
C                                                                       PLV17620
C    CALCULATE TIME OF DAY PARCEL WOULD BE AT PRESENT POINT (DIST(NX))  PLV17630
C    IN ORDER TO BE TRANSPORTED  TO EACH POINT FARTHER DOWNWIND FOR     PLV17640
C    TIME OF OPTICS CALCULATIONS.                                       PLV17650
C                                                                       PLV17660
               XTIMER=TIMEHR-DTIME                                      PLV17670
5961           IF(XTIMER.GT.0.)GO TO 5962                               PLV17680
C                                                                       PLV17690
C    CORRECT FOR NEGATIIVE CLOCK TIME                                   PLV17700
C                                                                       PLV17710
               XTIMER=XTIMER+24.                                        PLV17720
               GO TO 5961                                               PLV17730
5962           CONTINUE                                                 PLV17740
C                                                                       PLV17750
C    IT1 = HOUR OF DAY FOR PARCEL AT DIST(NX)                           PLV17760
C                                                                       PLV17770
               IT1=INT(XTIMER)                                          PLV17780
               IF(IT1.EQ.0)IT1=24                                       PLV17790
               IT2=IT1+1                                                PLV17800
               IF(IT2.EQ.25)IT2=1                                       PLV17810
C                                                                       PLV17820
C    RTIME = DECIMAL FRACTION OF TIME OF DAY (TIME BEYOND IT1)          PLV17830
C                                                                       PLV17840
               RTIME=AMOD(XTIMER,1.)                                    PLV17850
C                                                                       PLV17860
C********************************************************************** PLV17870
C    LINEAR INTERPOLATION OF NO2 - NO DYNAMIC EQUILIBRIUM CONSTANT      PLV17880
C    AND INTERPOLATION OF PHOTOLYSIS RATE CONSTANT FOR TIME OF DAY      PLV17890
C    WHEN PLUME PARCEL WOULD BE AT DIST(NX) AND BE TRANSPORTED TO       PLV17900
C    TO DIST(NX+1), DIST(NX+2), ETC.                                    PLV17910
C********************************************************************** PLV17920
C                                                                       PLV17930
               PHIKKA=PHIKKR(IT1)+RTIME*(PHIKKR(IT2)-PHIKKR(IT1))       PLV17940
               QJA=QJ(IT1)+RTIME*(QJ(IT2)-QJ(IT1))                      PLV17950
C                                                                       PLV17960
C    CONCENTRATION OF SO2                                               PLV17970
C                                                                       PLV17980
               XSO2R=QSO2TR(NZ,I)*CHIQ                                  PLV17990
C                                                                       PLV18000
C    CONCENTRATION OF NOX                                               PLV18010
C                                                                       PLV18020
               XNOXR=QNOXTR(NZ,I)*CHIQ                                  PLV18030
C                                                                       PLV18040
C    CONCENTRATION OF NO2                                               PLV18050
C                                                                       PLV18060
               XNO2TR=XNOXR*RATIOT(NZ,I)/(1.+RATIOT(NZ,I))              PLV18070
               SUMR=XNOXR+AMBNOX+O3AMB+XNO2TR+AMBNO2+PHIKKA             PLV18080
               XNO2R=0.5*(SUMR-SQRT(ABS(SUMR*SUMR-4.*(XNOXR+AMBNOX)*    PLV18090
     1         (O3AMB+XNO2TR+AMBNO2))))                                 PLV18100
C                                                                       PLV18110
C    NOX PLUME CONCENTRATION WITH AMBIENT BACKGROUND NOX ADDED.         PLV18120
C                                                                       PLV18130
               TOTR=XNOXR+AMBNOX                                        PLV18140
               IF(XNO2R.GT.TOTR)XNO2R=TOTR                              PLV18150
               IF(NX.EQ.NX2)GO TO 39                                    PLV18160
               RATIOT(NZ,I)=RATIOT(NZ,I)+4.015E-12*EXP(1046./           PLV18170
     1         (1.987*TAMB))*(TOTR-XNO2R)*209460.*(DIST(NX+1)           PLV18180
     2         -DIST(NX))/U                                             PLV18190
39             CONTINUE                                                 PLV18200
C                                                                       PLV18210
C    OZONE CALCULATION                                                  PLV18220
C                                                                       PLV18230
               XO3R=O3AMB-(XNO2R-XNO2TR-AMBNO2)                         PLV18240
C                                                                       PLV18250
C    OH RADICAL CONCENTRATION                                           PLV18260
C                                                                       PLV18270
               XOHR=2.*QJA*3.4E5*WATER*XO3R/((4.45E10+3.4E5*WATER)*     PLV18280
     1         (2000.*XSO2R+14.E3*XNO2R))                               PLV18290
C                                                                       PLV18300
C    MAXIMUM OH RADICAL CONCENTRATION                                   PLV18310
C                                                                       PLV18320
               XOHRMX=4.87E-7*QJA/1.32E-3                               PLV18330
               IF(XOHR.GT.XOHRMX)XOHR=XOHRMX                            PLV18340
C                                                                       PLV18350
C    SO2, NOX, AND NO2  CONVERSION RATES                                PLV18360
C                                                                       PLV18370
               RSO2R(NX+1,NZ,I+1)=2000.*XOHR*60.*100.+RSO2(NX+1)        PLV18380
               RNOXR(NX+1,NZ,I+1)=14.E3*XOHR*60.*100.                   PLV18390
               RNO2X(NX+1,NZ,I+1)=XNO2R/TOTR                            PLV18400
C                                                                       PLV18410
C     BRANCH IF PLUME PARCEL NOT AT FINAL POSITION (I=NX)               PLV18420
C                                                                       PLV18430
               IF(I.NE.NX)GO TO 5959                                    PLV18440
C                                                                       PLV18450
C     SAVE SO2 CONCENTRATION AND PARTICULATE CONCENTRATION.             PLV18460
C                                                                       PLV18470
               XSO2=XSO2R                                               PLV18480
               XPART=QPART*CHIQ                                         PLV18490
C                                                                       PLV18500
C    CALCULATE  MOLE RATIO OF SULFATE TO INITIAL SO2 (%).               PLV18510
C                                                                       PLV18520
               RATIOS= QSO4TR(NZ,I)/1.5 * 3.821E-4/QSO2*100.            PLV18530
C                                                                       PLV18540
C    SAVE OZONE, NOX, AND NO2 CONCENTRATIONS.                           PLV18550
C                                                                       PLV18560
               XO3=XO3R                                                 PLV18570
               XNOX=XNOXR                                               PLV18580
               XNO2=XNO2R                                               PLV18590
C                                                                       PLV18600
C    SAVE EFFECTIVE EMISSION RATE OF NO2 AND SO4 FOR LEVEL NZ           PLV18610
C                                                                       PLV18620
               QNO3=QNO3TR(NZ,I)                                        PLV18630
               QSO4=QSO4TR(NZ,I)                                        PLV18640
C                                                                       PLV18650
C   PERCENT NOX CONVERTED TO NO3 (NITRIC ACID VAPOR)                    PLV18660
C                                                                       PLV18670
               RNO3=(QNO3/QNOX)*100.                                    PLV18680
C                                                                       PLV18690
C    TOTAL NOX CONCENTRATION                                            PLV18700
C                                                                       PLV18710
               TOT=TOTR                                                 PLV18720
C                                                                       PLV18730
C    CALCULATE NO3 CONCENTRATION                                        PLV18740
C                                                                       PLV18750
               XNO3=QNO3*CHIQ                                           PLV18760
C                                                                       PLV18770
C    CALCULATE EFFECTIVE NO2 EMISSION RATE                              PLV18780
C                                                                       PLV18790
               QNO2I(NZ)=XNO2R/CHIQ                                     PLV18800
               XNO2I(NX,NZ)=XNO2R                                       PLV18810
C                                                                       PLV18820
C   CALCULATE MOLE RATIO OF HNO3 TO INITIAL NOX(%).                     PLV18830
C                                                                       PLV18840
               RATION=(QNO3TR(NZ,I)/QNOX)*100.                          PLV18850
5959           CONTINUE                                                 PLV18860
5960        CONTINUE                                                    PLV18870
                                                                        PLV18880
            IF(IFLAGP.EQ.1) GO TO 5616                                  PLV18890
C                                                                       PLV18900
C   WRITE OUT TITLES FOR PRINT FILE                                     PLV18910
C                                                                       PLV18920
            WRITE(6,5611)                                               PLV18930
 5611       FORMAT(9H ALTITUDE,9X,3HNOX,7X,3HNO2,7X,4HNO3-,2X,          PLV18940
     1      8HNO2/NTOT,2X,9HNO3-/NTOT,5X,3HSO2,7X,4HSO4=,2X,            PLV18950
     2      9HSO4=/STOT,5X,2HO3,5X,7HPRIMARY,1X,9HBSP-TOTAL,            PLV18960
     3      2X,9HBSPSN/BSP)                                             PLV18970
            WRITE(6,5612)                                               PLV18980
5612        FORMAT(16X,5H(PPM),5X,5H(PPM),5X,5H(PPM),2(2X,8H(MOLE %)),  PLV18990
     1      5X,5H(PPM),3X,7H(UG/M3),2X,8H(MOLE %),5X,5H(PPM),3X,        PLV19000
     2      7H(UG/M3),1X,10H(10-4 M-1),3X,3H(%),/)                      PLV19010
 5616       CONTINUE                                                    PLV19020
C                                                                       PLV19030
C   NO2 PLUME INCREMENT                                                 PLV19040
C                                                                       PLV19050
            XNO2IN=XNO2-AMBNO2                                          PLV19060
            QNO2I(NZ)=XNO2IN/CHIQ                                       PLV19070
            XNO3=QNO3*CHIQ                                              PLV19080
C                                                                       PLV19090
C  TOTAL NO3 (PLUME + AMBIENT)                                          PLV19100
C                                                                       PLV19110
            XNO3T=XNO3+AMBNO3                                           PLV19120
C                                                                       PLV19130
C  RATIO OF PLUME INCREMENT NO2 TO INITIAL NOX (%).                     PLV19140
C                                                                       PLV19150
            RNO2=(100.*(XNO2-AMBNO2)/CHIQ)/QNOX                         PLV19160
C                                                                       PLV19170
C  IF (PLUME NO2 - AMBIENT NO2) IS SMALL, RATIO SET TO ZERO.            PLV19180
C                                                                       PLV19190
            IF(XNO2IN.LT.0.0005)  RNO2 = 0.                             PLV19200
C                                                                       PLV19210
C  MOLE RATIO OF TOTAL NO2 AND NO3 TO TOTAL NOX (PLUME + AMBIENT).      PLV19220
C                                                                       PLV19230
            RNO2T=(XNO2/(TOT+XNO3T))*100.                               PLV19240
            RNO3T=(XNO3T/(TOT+XNO3T))*100.                              PLV19250
C                                                                       PLV19260
C  SET NO2/NTOT AND NO3- RATIOS TO ZERO IF NTOT IS LESS                 PLV19270
C  THAN 1.E-10 PPM TO AVOID MEANINGLESS NUMBERS.                        PLV19280
C                                                                       PLV19290
            IF(TOT.GE.1.E-10) GO TO 777                                 PLV19300
            RNO2T = 0.                                                  PLV19310
            RNO3T = 0.                                                  PLV19320
            RATION = 0.                                                 PLV19330
  777       CONTINUE                                                    PLV19340
C                                                                       PLV19350
C  TOTAL SO2 CONCENTRATION IN PLUME (PLUME + AMBIENT)                   PLV19360
C                                                                       PLV19370
            XSO2T=XSO2+AMBSO2                                           PLV19380
C                                                                       PLV19390
C  SULFATE PLUME INCREMENT                                              PLV19400
C                                                                       PLV19410
            XSO4=QSO4*CHIQ                                              PLV19420
C                                                                       PLV19430
C  SAVE PARTICULATE AND SULFATE PLUME INCREMENTS FOR ALONG PLUME        PLV19440
C  OPTICAL ANALYSIS.                                                    PLV19450
C                                                                       PLV19460
            XPARTP(NX,NZ)=XPART                                         PLV19470
            XPARTS(NX,NZ)=XSO4                                          PLV19480
C                                                                       PLV19490
C  TOTAL SULFATE CONCENTRATION.                                         PLV19500
C                                                                       PLV19510
            XSO4T=XSO4+AMBSO4                                           PLV19520
C                                                                       PLV19530
C  RATIO OF TOTAL SO4 COMPONENT TO TOTAL SULFUR COMPONENT.              PLV19540
C                                                                       PLV19550
            RSO4T=(XSO4T*3.821E-4/1.5)/(XSO2T+XSO4T*3.821E-4/1.5)       PLV19560
C                                                                       PLV19570
C  CONVERT TO PERCENT                                                   PLV19580
C                                                                       PLV19590
            RSO4T=RSO4T*100.                                            PLV19600
C                                                                       PLV19610
C  PLUME INCREMENT OF OZONE (THIS IS LESS THAN ZERO FOR NOX PLUMES).    PLV19620
C                                                                       PLV19630
            XO3IN=XO3-O3AMB                                             PLV19640
C                                                                       PLV19650
C  TOTAL AEROSOL CONCENTRATION (PLUME PLUS BACKGROUND)                  PLV19660
C                                                                       PLV19670
            XPT = XPART + CORAMB + AMBSO4                               PLV19680
C                                                                       PLV19690
C  EXTINCTION COEFFICIENT FOR PLUME PRIMARY PARTICULATE AND             PLV19700
C  SULFATE AND CONVERT FROM 1/KM TO 1/METERS                            PLV19710
C                                                                       PLV19720
            BSP=10.*(BTAPRM(19)*XPART+BTASO4(19)*XSO4)                  PLV19730
C                                                                       PLV19740
C  ADD IN BACKGROUND AEROSOL EXTINCTION                                 PLV19750
C                                                                       PLV19760
            BSPT=BSP+10.*BTAAER(19)                                     PLV19770
C                                                                       PLV19780
C  RATIO OF SULFATE EXTINCTION COEFFICIENT IN PLUME TO TOTAL            PLV19790
C  PLUME AEROSOL EXTINCTION COEFFICIENT (%).                            PLV19800
C                                                                       PLV19810
            RSEC=1000.*BTASO4(19)*XSO4/BSP                              PLV19820
C                                                                       PLV19830
C  RATIO OF PLUME SULFATE EXTINCTION COEFFICIENT TO TOTAL               PLV19840
C  AEROSOL EXTINCTION COEFFICIENT (%).                                  PLV19850
C                                                                       PLV19860
            RSECT=1000.*BTASO4(19)*XSO4T/BSPT                           PLV19870
            WRITE(6,5613) ALT(NZ)                                       PLV19880
            WRITE(6,5614) XNOX,XNO2IN,XNO3,RNO2,RATION,XSO2,XSO4,       PLV19890
     1      RATIOS,XO3IN,XPART,BSP,RSEC                                 PLV19900
 5613       FORMAT(2X,A4)                                               PLV19910
 5614       FORMAT(11H INCREMENT:,12F10.3)                              PLV19920
            WRITE(6,5615) TOT,XNO2,XNO3T,RNO2T,RNO3T,XSO2T,XSO4T,       PLV19930
     1      RSO4T,XO3,XPT,BSPT,RSECT                                    PLV19940
            IFLAGP=1                                                    PLV19950
 5615       FORMAT(11H TOTAL AMB:,12F10.3,/)                            PLV19960
C                                                                       PLV19970
C  BRANCH AROUND IF NOT AT PLUME CENTERLINE HEIGHT.                     PLV19980
C                                                                       PLV19990
            IF(NZ.NE.3) GO TO 60                                        PLV20000
C                                                                       PLV20010
C  SAVE PLUME CENTERLINE CONCENTRATIONS, INCLUDING AMBIENT              PLV20020
C  BACKGROUND CONTRIBUTION.                                             PLV20030
C                                                                       PLV20040
            XNO2C=XNO2                                                  PLV20050
            XSO4C=XSO4T                                                 PLV20060
            XPRMC=XPT                                                   PLV20070
 60      CONTINUE                                                       PLV20080
         WRITE(6,5622)                                                  PLV20090
 5622    FORMAT(////, ' CUMULATIVE SURFACE DEPOSITION (MOLE FRACTION'   PLV20100
     1   'OF INITIAL FLUX',//)                                          PLV20110
         WRITE(6,5617) FDSO2                                            PLV20120
 5617    FORMAT(1H ,16X,4HSO2:,F10.4)                                   PLV20130
         WRITE(6,5618) FDNOX                                            PLV20140
 5618    FORMAT(1H ,16X,4HNOX:,F10.4)                                   PLV20150
         WRITE(6,5619) FDPART                                           PLV20160
 5619    FORMAT(21H PRIMARY PARTICULATE:,F10.4)                         PLV20170
         WRITE(6,5620) FDSO4                                            PLV20180
 5620    FORMAT(1H ,16X,4HSO4:,F10.4)                                   PLV20190
         WRITE(6,5621) FDNO3                                            PLV20200
 5621    FORMAT(1H ,16X,4HNO3:,F10.4)                                   PLV20210
C                                                                       PLV20220
C    CALCULATION OF NO2 AND SO4 FLUXES TAKING INTO ACCOUNT THE NON-     PLV20230
C    UNIFORM MOLE RATIOS ACROSS THE PLUME.                              PLV20240
C                                                                       PLV20250
         QNO2I(3) = 0.39*QNO2I(3) + 0.485*QNO2I(2) + 0.125*QNO2I(1)     PLV20260
         QNO2I(2) = 0.39*QNO2I(2) + 0.61*QNO2I(1)                       PLV20270
         QNO2I(4)  = 0.39*QNO2I(4) + 0.61*QNO2I(5)                      PLV20280
         QSO4=0.39*QSO4TR(3,NX)+0.485*QSO4TR(2,NX)+0.125*QSO4TR(1,NX)   PLV20290
C                                                                       PLV20300
C  SKIP OVER HORIZONTAL SIGHT PATHS WITH SKY BACKGROUND OPTICS          PLV20310
C  CALCULATION IF IFLG1 = 0 .                                           PLV20320
C                                                                       PLV20330
         IF(IFLG1.EQ.0) GO TO 7001                                      PLV20340
C                                                                       PLV20350
C **********************************************************************PLV20360
C                                                                       PLV20370
C VISUAL EFFECTS (HORIZONTAL SIGHT PATHS) ******************************PLV20380
C                                                                       PLV20390
C **********************************************************************PLV20400
C                                                                       PLV20410
C  LOOP FOR PLUME-BASED OR OBSERVER-BASED CALCULATIONS OR BOTH          PLV20420
C                                                                       PLV20430
      DO 7000 NC=NC1,NC2                                                PLV20440
      NFLAG=1                                                           PLV20450
C                                                                       PLV20460
C  ALTITUDE LOOP, NZF=1 FOR PLUME CENTERLINE ONLY, NZF=2 FOR            PLV20470
C  CENTERLINE AND SURFACE.                                              PLV20480
C                                                                       PLV20490
      DO 7000 NZ1 = 1,NZF                                               PLV20500
      NZ=NZ1*3                                                          PLV20510
      IF(NZ.EQ.3) Z=H                                                   PLV20520
      IF(NZ.EQ.6) Z=0.                                                  PLV20530
C                                                                       PLV20540
C  BRANCH FOR OBSERVER-SPECIFIC CALCULATION                             PLV20550
C                                                                       PLV20560
      IF(NC.EQ.2)GO TO 6100                                             PLV20570
C                                                                       PLV20580
C  INITIALIZE AND INCREMENT SCATTERING ANGLE INDEX                      PLV20590
C                                                                       PLV20600
      NT=NT1-1                                                          PLV20610
 6990 NT=NT+1                                                           PLV20620
      ITHETA=NT+1                                                       PLV20630
C                                                                       PLV20640
C  ASSIGN SCATTERING ANGLE                                              PLV20650
C                                                                       PLV20660
      THETA=TT(ITHETA)                                                  PLV20670
      NFLAG=-1*NFLAG                                                    PLV20680
C                                                                       PLV20690
C  SKIP WRITING TITLE EVERY SECOND TIME THROUGH                         PLV20700
C                                                                       PLV20710
      IF(NFLAG.GT.0) GO TO 65                                           PLV20720
 6100 CONTINUE                                                          PLV20730
      WRITE(6,10)                                                       PLV20740
      WRITE(6,61)(PLANT(J),J=1,6)                                       PLV20750
 61   FORMAT (20X,41HVISUAL EFFECTS FOR HORIZONTAL SIGHT PATHS,/20X,6A4,PLV20760
     1/)                                                                PLV20770
C                                                                       PLV20780
C  XKM = DOWNWIND DISTANCE IN KILOMETERS                                PLV20790
C                                                                       PLV20800
      WRITE(6,52) XKM                                                   PLV20810
C                                                                       PLV20820
C  H = HEIGHT OF PLUME CENTERLINE.                                      PLV20830
C                                                                       PLV20840
      WRITE(6,53) H                                                     PLV20850
C                                                                       PLV20860
C  BRANCH IF DOING PLUME-BASED CALCULATION                              PLV20870
C                                                                       PLV20880
      IF(NC.EQ.1)GO TO 6400                                             PLV20890
      WRITE(6,611)OBSPLU(NX)                                            PLV20900
  611 FORMAT(31H PLUME-OBSERVER DISTANCE (KM) =,F7.1)                   PLV20910
      WRITE(6,612)AZMUTH(NX)                                            PLV20920
  612 FORMAT(27H AZIMUTH OF LINE-OF-SIGHT =,F7.1)                       PLV20930
      WRITE(6,613)ABETA(NX)                                             PLV20940
  613 FORMAT(35H ELEVATION ANGLE OF LINE-OF-SIGHT =,F7.1)               PLV20950
      WRITE(6,614)ZENITH,TIME,IMO,IDAY                                  PLV20960
  614 FORMAT(21H SOLAR ZENITH ANGLE =,F7.1,2X,4H AT ,F5.0,4H ON ,       PLV20970
     1  I2,1H/,I2)                                                      PLV20980
C                                                                       PLV20990
C  ASSIGN SCATTERING ANGLE                                              PLV21000
C                                                                       PLV21010
      THETA=TT(NX+7)                                                    PLV21020
C                                                                       PLV21030
C   ASSIGN INDEX FOR PHASE FUNCTIONS FOR CALL TO PLMCLN                 PLV21040
C                                                                       PLV21050
      ITHETA=7+NX                                                       PLV21060
C                                                                       PLV21070
C  ASSIGN HORIZONTAL ANGLE BETWEEN OBSERVER LINE-OF-SIGHT AND PLUME     PLV21080
C  CENTERLINE.                                                          PLV21090
C                                                                       PLV21100
      ALPHA=AALPHA(NX)                                                  PLV21110
C                                                                       PLV21120
C  ASSIGN OBSERVER-PLUME DISTANCE ALONG LINE-OF-SIGHT                   PLV21130
C                                                                       PLV21140
      RP=OBSPLU(NX)                                                     PLV21150
C                                                                       PLV21160
C  ADJUST OPTICAL THICKNESS FOR LINE-OF-SIGHT NOT PERPENDICULAR TO PLUMEPLV21170
C                                                                       PLV21180
      YINT=CHIQIY(NZ)/SIN(ALPHA*RAD)                                    PLV21190
 6400 CONTINUE                                                          PLV21200
      IF(NZ1.EQ.1) WRITE(6,62)                                          PLV21210
      IF(NZ1.EQ.2) WRITE(6,63)                                          PLV21220
 62   FORMAT(35H SIGHT PATH IS THROUGH PLUME CENTER,/)                  PLV21230
 63   FORMAT(30H SIGHT PATH IS AT GROUND LEVEL,/)                       PLV21240
      WRITE(6,64)                                                       PLV21250
 64   FORMAT(6H THETA,1X,5HALPHA,2X,6HRP/RV0,4X,2HRV,2X,8H%REDUCED,4X,  PLV21260
     14HYCAP,7X,1HL,7X,1HX,7X,1HY,8H DELYCAP,4X,4HDELL,2X,6HC(550),2X,  PLV21270
     26HBRATIO,4X,4HDELX,4X,4HDELY,2X,6HE(LUV),2X,6HE(LAB))             PLV21280
 65   CONTINUE                                                          PLV21290
      WRITE(6,66) THETA                                                 PLV21300
C                                                                       PLV21310
C  BRANCH FOR OBSERVER-BASED CALCULATION                                PLV21320
C                                                                       PLV21330
      IF(NC.EQ.2)GO TO 6500                                             PLV21340
 66   FORMAT(/,F6.0)                                                    PLV21350
C                                                                       PLV21360
C  INITIALIZE AND INCREMENT INDEX FOR HORIZONTAL ANGLE BETWEEN          PLV21370
C OBSERVER LINE-OF-SIGHT AND PLUME CENTERLINE (PLUME-BASED CALCULATIONS)PLV21380
C                                                                       PLV21390
      NA=0                                                              PLV21400
 6995 NA=NA+1                                                           PLV21410
      IF(NA.LT.5)GO TO 70                                               PLV21420
      ALPHA=AA(4)                                                       PLV21430
      GO TO 73                                                          PLV21440
70    ALPHA=AA(NA)                                                      PLV21450
C                                                                       PLV21460
C  CHI/Q AT PLUME CENTERLINE * EQUIVALENT DISTANCE TO MATCH CHI/Q       PLV21470
C  INTEGAL IN Y-DIRECTION FOR GAUSSIAN DISTRIBUTION.                    PLV21480
C                                                                       PLV21490
73    YINT=CHIQIY(NZ)/SIN(ALPHA*RAD)                                    PLV21500
      NP=0                                                              PLV21510
C                                                                       PLV21520
C  INITIALIZE AND INCREMENT OBSERVER-PLUME DISTANCE INDEX               PLV21530
C                                                                       PLV21540
 6999 NP=NP+1                                                           PLV21550
C                                                                       PLV21560
C  RP = DISTANCE TO PLUME FROM OBSERVER ALONG LINE-OF-SIGHT             PLV21570
C                                                                       PLV21580
      RP=ROBJ(NP)*RVAMB                                                 PLV21590
C                                                                       PLV21600
C   SPECIAL CASE FOR NA=5 FOR OBSERVER POSITIONED AT 1/2 OF A 22.5      PLV21610
C   DEGREE SECTOR FROM THE PLUME CENTERLINE.IF THIS DISTANCE IS <       PLV21620
C   5 KM, IT IS SET TO 5 KM.                                            PLV21630
C                                                                       PLV21640
      IF(NA.LT.5)GO TO 6500                                             PLV21650
      RP=AMAX1((DIST(NX)/1000.)*TAN(22.5/2.*RAD),5.)                    PLV21660
 6500 CONTINUE                                                          PLV21670
      PLMDIS=RP                                                         PLV21680
C                                                                       PLV21690
C   CALCULATE RATIO OF PERPENDICULAR PLUME-OBSERVER DISTANCE TO SIGMA Y.PLV21700
C                                                                       PLV21710
      RPR= RP/SY*1000.*SIN(ALPHA*RAD)                                   PLV21720
      YINTR=1.0                                                         PLV21730
C                                                                       PLV21740
C  BRANCH IF OBSERVER TO PLUME DISTANCE IS GREATER THAN 2.17*SIGMA-Y    PLV21750
C                                                                       PLV21760
      IF(RPR.GT.2.17) GO TO 6601                                        PLV21770
C                                                                       PLV21780
C  REDEFINING RP IF OBSERVER IS WITHIN THE PLUME SO THAT RP IS DISTANCE PLV21790
C  TO PLUME CENTROID.  ALSO, CALCULATE THE FRACTION OF TOTAL PLUME      PLV21800
C  OPTICAL THICKNESS WITHIN LINE OF SIGHT.                              PLV21810
C                                                                       PLV21820
      RPHALF=0.18*(RPR+0.25)+0.153*(RPR+0.75)+0.167*(RPR+1.5)           PLV21830
      IF(RPR.GT.0.5)GO TO 6501                                          PLV21840
      YINTR1=(RPR/0.5)*0.18                                             PLV21850
      YINTR=YINTR1+0.50                                                 PLV21860
      RP=((RPR*YINTR1/2.+RPHALF)/YINTR)*(SY/1000.)/SIN(ALPHA*RAD)       PLV21870
      GO TO 6601                                                        PLV21880
6501  IF(RPR.GT.1.0)GO TO 6502                                          PLV21890
      YINTR1=(RPR-0.5)*0.153/0.5                                        PLV21900
      YINTR=YINTR1+0.68                                                 PLV21910
      RP=(((RPR-0.5)*YINTR1/2.+(RPR-0.25)*0.18+RPHALF)/YINTR)*(SY/1000.)PLV21920
     1 /SIN(ALPHA*RAD)                                                  PLV21930
      GO TO 6601                                                        PLV21940
6502  YINTR1=(RPR-1.0)*0.142                                            PLV21950
      YINTR=YINTR1+0.833                                                PLV21960
      RP=(((RPR-1.)*YINTR1/2.+(RPR-0.75)*0.153+(RPR-0.25)*0.18+RPHALF)  PLV21970
     1 /YINTR)*(SY/1000.)/SIN(ALPHA*RAD)                                PLV21980
 6601 CONTINUE                                                          PLV21990
      YINT1=YINT*YINTR                                                  PLV22000
C                                                                       PLV22010
C  CALCULATION OF MASS INTEGRAL ALONG LINE-OF-SIGHT WITHIN PLUME FOR    PLV22020
C  PRIMARY PARTICULATE, SULFATE, AND NO2                                PLV22030
C                                                                       PLV22040
      PLUMEP=YINT1*QPART*(1.-FRACTC)                                    PLV22050
      PLUMES=YINT1*QSO4                                                 PLV22060
      PLUMEN= YINT1*QNO2I(NZ)                                           PLV22070
      PLUMEC= YINT1*QPART*FRACTC                                        PLV22080
C                                                                       PLV22090
C  CALCULATION OF OPTICAL THICKNESS OF PLUME DUE TO SULFATE, NO2,       PLV22100
C  AND PRIMARY PARTICULATE.                                             PLV22110
C                                                                       PLV22120
      TAPSO4 = BTASO4(19)*PLUMES                                        PLV22130
      TAPNO2 = ABSNO2(19)*PLUMEN                                        PLV22140
      TAUPRM = BTAPRM(19)*PLUMEP+BTACAR(19)*PLUMEC                      PLV22150
C     WRITE(6,6520)PLUMEN,TAPSO4,TAUPRM                                 PLV22160
C6520 FORMAT(/,1X,8HPLUMEN =,E10.4,1X,8HTAPSO4 =,E10.4,                 PLV22170
C    21X,8HTAUPRM =,E10.4)                                              PLV22180
C                                                                       PLV22190
C  CALCULATE BACKGROUND SKY INTENSITIES (SPECB) AND PLUME INTENSITIES   PLV22200
C  (SPECP).   ********CALL PLMCLN*********                              PLV22210
C                                                                       PLV22220
      CALL PLMCLN(ZENITH,0.,THETA,ITHETA,PLUMEP,PLUMES,PLUMEN,          PLV22230
     1PLUMEC,SPECB,SPECP,RP,THICK)                                      PLV22240
C     PRSU=(SPECP(4)+SPECP(5))/(SPECB(4)+SPECB(5))                      PLV22250
C     PRSB=SPECP(9)/SPECB(9)                                            PLV22260
C     PRSG=SPECP(19)/SPECB(19)                                          PLV22270
C     PRSR=SPECP(27)/SPECB(27)                                          PLV22280
C     DO 966 I=1,39                                                     PLV22290
C     XP(I)=SPECP(I)                                                    PLV22300
C     XB(I)=SPECB(I)                                                    PLV22310
C     XR(I)=XP(I)/XB(I)                                                 PLV22320
C 966 CONTINUE                                                          PLV22330
C     WRITE (6,94)(XP(I),I=1,39)                                        PLV22340
C     WRITE (6,94)(XB(I),I=1,39)                                        PLV22350
C     WRITE (6,94)(XR(I),I=1,39)                                        PLV22360
C  94 FORMAT(//5(7(1X,E13.4)/),4(1X,E13.4))                             PLV22370
C                                                                       PLV22380
C   CALL ROUTINE TO CALCULATE COLORATION PARAMETERS                     PLV22390
C                                                                       PLV22400
      IF(NZ1.EQ.1) INTSKY=SPECB(19)                                     PLV22410
      CALL CHROMA(SPECP,SPECB)                                          PLV22420
 6608 CONTINUE                                                          PLV22430
C                                                                       PLV22440
C  NEXT CALCULATE REDUCTION IN VISUAL RANGE THROUGH PLUME DUE TO AEROSOLPLV22450
C                                                                       PLV22460
      CORNO2=ALOG(CONT2+1.)                                             PLV22470
      THICK = TAPSO4 + TAUPRM + TAPNO2 + CORNO2                         PLV22480
      SYALPH = (SY/1000.)/SIN(ALPHA*RAD)                                PLV22490
C                                                                       PLV22500
C  ADD IN OPTICAL DEPTH OF BACKGROUND AIR IN FRONT OF AND WITHIN        PLV22510
C  PLUME.                                                               PLV22520
C                                                                       PLV22530
      TAUPLU = BTABAC(19)*(PLMDIS + 2.17*SYALPH) + THICK                PLV22540
C                                                                       PLV22550
C  BRANCH IF PLUME AEROSOL OPTICAL DEPTH = ZERO                         PLV22560
C                                                                       PLV22570
      IF(THICK.EQ.0.) GO TO 6602                                        PLV22580
C                                                                       PLV22590
C  BRANCH IF OPTICAL DEPTH OF AEROSOL IN FRONT OF PLUME AND OF PLUME    PLV22600
C  AEROSOL IS NOT SUFFICIENT TO REDUCE VISUAL RANGE TO PREVENT SEEING   PLV22610
C  BEYOND PLUME.                                                        PLV22620
C                                                                       PLV22630
      IF(TAUPLU .LT. 3.912) GO TO 6602                                  PLV22640
      TAUHAF = BTABAC(19)*PLMDIS + ((YINTR-0.5)/YINTR)*THICK            PLV22650
      RESIDP = 3.912 - TAUHAF                                           PLV22660
      RESIDA = RESIDP                                                   PLV22670
      IF(RESIDP .LT. 0.) RESIDA = -1.*RESIDP                            PLV22680
      DELT1 = 0.18*THICK/YINTR + 0.5*SYALPH*BTABAC(19)                  PLV22690
      DELT2 = 0.333*THICK/YINTR + SYALPH*BTABAC(19)                     PLV22700
      DELT3 = 0.5*THICK/YINTR + 2.17*SYALPH*BTABAC(19)                  PLV22710
      RV = PLMDIS +((RESIDP/RESIDA)*(1. + 1.17*(RESIDA - DELT2)/        PLV22720
     1 (DELT3 - DELT2)))*SYALPH                                         PLV22730
      IF(RESIDA .LT. DELT2) RV = PLMDIS + (RESIDP/RESIDA)*(1. + (RESIDA-PLV22740
     1 DELT1)/(DELT2 - DELT1))*0.5*SYALPH                               PLV22750
      IF(RESIDA .LT. DELT1) RV = PLMDIS + 0.5*SYALPH*RESIDP/DELT1       PLV22760
      GO TO 6606                                                        PLV22770
 6602 CONTINUE                                                          PLV22780
C                                                                       PLV22790
C  CALCULATION OF DISTANCE OBSERVER WOULD SEE BEYOND PLUME              PLV22800
C                                                                       PLV22810
      BKPLM = (3.912 - TAUPLU)/BTABAC(19)                               PLV22820
C                                                                       PLV22830
C  VISUAL RANGE = DISTANCE TO PLUME + WIDTH OF PLUME + DISTANCE         PLV22840
C  BEYOND PLUME.                                                        PLV22850
C                                                                       PLV22860
      RV = PLMDIS + 2.17*SYALPH + BKPLM                                 PLV22870
 6606 CONTINUE                                                          PLV22880
C                                                                       PLV22890
C  MINIMUM VISUAL RANGE LOOKING THROUGH PLUME CENTERLINE WITH           PLV22900
C  BACKGROUND EXTINCTION AND WITH PLUME SO4, NO2, AND PRIMARY           PLV22910
C  PARTICULATE EXTINCTION.                                              PLV22920
C                                                                       PLV22930
       RVMIN=3.912/(BTABAC(19) + BTASO4(19)*XSO4C + ABSNO2(19)*XNO2C    PLV22940
     1 + BTAPRM(19)*XPRMC*(1.-FRACTC) + XPRMC*FRACTC* BTACAR(19))       PLV22950
      IF(RV .LT. RVMIN) RV= RVMIN                                       PLV22960
      IF(RV.GT.RVAMB)RV=RVAMB                                           PLV22970
C                                                                       PLV22980
C  REDUCTION IN VISUAL RANGE LOOKING THROUGH THE PLUME                  PLV22990
C                                                                       PLV23000
      REDRV=100.*(1.-RV/RVAMB)                                          PLV23010
      IF(NC.EQ.1)GO TO 6610                                             PLV23020
C                                                                       PLV23030
C  SAVE KEY VISUAL IMPACT PARAMETERS FOR PLOTTING LATER (OBSERVER-      PLV23040
C  BASED CASE)                                                          PLV23050
C                                                                       PLV23060
      PLT1(NX,1)=REDRV                                                  PLV23070
      PLT2(NX,1)=BRATIO                                                 PLV23080
      PLT3(NX,1)=CONT2                                                  PLV23090
      PLT4(NX,1)=DELAB                                                  PLV23100
      GO TO 6607                                                        PLV23110
C                                                                       PLV23120
C  SELECT THE PARTICULAR CASE FOR PLOTTING FOR THE PLUME-BASED          PLV23130
C  CALCULATION.                                                         PLV23140
C  NP IS PLUME-OBSERVER DISTANCE INDEX, NA IS ALPHA INDEX,              PLV23150
C  NT IS SCATTERING ANGLE INDEX, AND NZ IS ALTITUDE INDEX.              PLV23160
C                                                                       PLV23170
6610  IF(NP.NE.NPP)GO TO 6607                                           PLV23180
      IF(NA.NE.NAP)GO TO 6607                                           PLV23190
      IF(NT.NE.NTP)GO TO 6607                                           PLV23200
      IF(NZ.NE.NZP)GO TO 6607                                           PLV23210
C                                                                       PLV23220
C  SAVE THE KEY VISUAL IMPACT VALUES FOR THE DESIRED RP, THETA, ALPHA,  PLV23230
C  AND NZ FOR THE PLUME-BASED  PLOTS.                                   PLV23240
C                                                                       PLV23250
      PLOT1(NX,1)=REDRV                                                 PLV23260
      PLOT2(NX,1)=BRATIO                                                PLV23270
      PLOT3(NX,1)=CONT2                                                 PLV23280
      PLOT4(NX,1)=DELAB                                                 PLV23290
 6607 CONTINUE                                                          PLV23300
      IF(NC.EQ.1) GO TO 6609                                            PLV23310
C                                                                       PLV23320
C  WRITE OUT RESULTS FOR OBSERVER-BASED CASE CALCULATIONS               PLV23330
C                                                                       PLV23340
      RPLMOB=OBSPLU(NX)/RVAMB                                           PLV23350
      WRITE(6,67) ALPHA,RPLMOB,RV,REDRV,YCAP,VAL,X,Y,YCAPD,VALD,CONT2,  PLV23360
     1BRATIO,XD,YD,DELUV,DELAB                                          PLV23370
C*** CALL FAUGERAS SUBROUTINE                                           PLV23380
      IF(NZ1.NE.1) GOTO 7772                                            PLV23390
      IF(NX.EQ.NX3 .OR. NX.EQ.NX4 .OR. NX.EQ.NX5) GOTO 7771             PLV23400
      GOTO 7772                                                         PLV23410
7771  CONTINUE                                                          PLV23420
      Z0=(ZOBS-ZSTACK)/3.281                                            PLV23430
      FRP = 1000. * OBSPLU(NX)                                          PLV23440
      CALL FAU42(FRP,H,Z0,ALPHA,HPBLM,SY,SZ                             PLV23450
     *         ,YCAP,X,Y,YCAPD,XD,YD                                    PLV23460
     *         ,FA,FC1,FC2,FB,FF,DCWK                                   PLV23470
     *         ,FFF, FCC, FDWK1, FDWK2                                  PLV23480
     *         ,I1FAU,I2FAU,I3FAU,I4FAU,IER)                            PLV23490
7772  CONTINUE                                                          PLV23500
C*** END CALL TO FAUGERAS SUBROUTINE                                    PLV23510
      GO TO 7000                                                        PLV23520
 6609 CONTINUE                                                          PLV23530
      RROBJT=ROBJ(NP)                                                   PLV23540
      IF(NA.EQ.5)RROBJT=RP/RVAMB                                        PLV23550
C                                                                       PLV23560
C  WRITE OUT RESULTS FOR PLUME-BASED CASE CALCULATIONS                  PLV23570
C                                                                       PLV23580
      IF(NA.EQ.5)WRITE(6,88)                                            PLV23590
88    FORMAT(1H0,4X,   'OBSERVER POSITION AT 1/2 OF A 22.5 DEGREE WIND DPLV23600
     1IRECTION SECTOR FROM THE PLUME CENTERLINE AT THE GIVEN DISTANCE FRPLV23610
     2OM THE SOURCE',/)                                                 PLV23620
      WRITE(6,67) ALPHA,RROBJT,RV,REDRV,YCAP,VAL,X,Y,YCAPD,VALD,        PLV23630
     1CONT2,BRATIO,XD,YD,DELUV,DELAB                                    PLV23640
67    FORMAT(4X,F8.0,F8.2,F8.1,3F8.2,2F8.4,2F8.2,6F8.4)                 PLV23650
C*** CALL FAUGERAS SUBROUTINE                                           PLV23660
      IF(NZ1.NE.1) GOTO 7774                                            PLV23670
      IF(NX.EQ.NX3 .OR. NX.EQ.NX4 .OR. NX.EQ.NX5) GOTO 7773             PLV23680
      GOTO 7774                                                         PLV23690
7773  CONTINUE                                                          PLV23700
      Z0=(ZOBS-ZSTACK)/3.281                                            PLV23710
      FRP = 1000. * OBSPLU(NX)                                          PLV23720
      CALL FAU42(FRP,H,Z0,ALPHA,HPBLM,SY,SZ                             PLV23730
     *         ,YCAP,X,Y,YCAPD,XD,YD                                    PLV23740
     *         ,FA,FC1,FC2,FB,FF,DCWK                                   PLV23750
     *         ,FFF, FCC, FDWK1, FDWK2                                  PLV23760
     *         ,I1FAU,I2FAU,I3FAU,I4FAU,IER)                            PLV23770
7774  CONTINUE                                                          PLV23780
C*** END CALL TO FAUGERAS SUBROUTINE                                    PLV23790
C                                                                       PLV23800
C  TEST IF DISTANCE, ALPHA, AND THETA LOOPS ARE COMPLETED.              PLV23810
C                                                                       PLV23820
      IF(NP.LT.6 .AND. NA .LT. 5)GO TO 6999                             PLV23830
      IF(NA.LT.5)GO TO 6995                                             PLV23840
      IF(NT.LT.(NT2-1))GO TO 6990                                       PLV23850
 7000 CONTINUE                                                          PLV23860
 7001 CONTINUE                                                          PLV23870
      CHIQIZ=(TAMB/FGTEMP)/(SQRT(2.*PI)*SY*U)                           PLV23880
      CHIQIZ=CHIQIZ/1000.                                               PLV23890
C                                                                       PLV23900
C  BRANCH AROUND NON-HORIZONTAL SIGHT PATHS CALCULATIONS IF IFLG2       PLV23910
C  WAS SET TO ZERO.                                                     PLV23920
C                                                                       PLV23930
      IF(IFLG2.EQ.0) GO TO 8001                                         PLV23940
C                                                                       PLV23950
C **********************************************************************PLV23960
C                                                                       PLV23970
C  **** NON-HORIZONTAL SIGHT PATHS CALCULATIONS ************************PLV23980
C                                                                       PLV23990
C **********************************************************************PLV24000
C                                                                       PLV24010
C LOOP ON TYPES OF CALCULATIONS (PLUME-BASED OR OBSERVER-BASED OR BOTH) PLV24020
C                                                                       PLV24030
      DO 8000 NC=NC1,NC2                                                PLV24040
C                                                                       PLV24050
C  IF SPECIFIC CASE BETA LESS THAN 5 DEGREES, SKIP CALCULATION.         PLV24060
C                                                                       PLV24070
c*** jcv NPS 6/18/96
c  put in warning message to avoid confusion about why non-horizontal
c  views not printed under some conditions
	if(nc.eq.2.and.abeta(nx).lt.5..and.iflg2.eq.1) then
	  write(6,'(/)')
	  write(6,22210)
	  write(6,22220)
22220   format ('Non-horizontal views not calculated for beta less than'
     +        ,' 5 degrees')
	  write(6,22210)
	  write(6,'(/)')
	endif
c*** End of jcv modification
      IF(NC.EQ.2.AND.ABETA(NX).LT.5.)GO TO 8001                         PLV24080
C                                                                       PLV24090
C  BRANCH FOR SPECIFIC CASE CALCULATION                                 PLV24100
C                                                                       PLV24110
 7110 IF(NC.EQ.2)GO TO 7100                                             PLV24120
      NFLAG=1                                                           PLV24130
C                                                                       PLV24140
C  INITIALIZE AND INCREMENT SCATTERING ANGLE INDEX                      PLV24150
C                                                                       PLV24160
      NT=NT1-1                                                          PLV24170
 7996 NT=NT+1                                                           PLV24180
      ITHETA=NT+1                                                       PLV24190
C                                                                       PLV24200
C  ASSIGN SCATTERING ANGLE                                              PLV24210
C                                                                       PLV24220
      THETA=TT(ITHETA)                                                  PLV24230
      NFLAG=-1*NFLAG                                                    PLV24240
      IF(NFLAG.GT.0) GO TO 75                                           PLV24250
 7100 CONTINUE                                                          PLV24260
      WRITE(6,10)                                                       PLV24270
      WRITE(6,71)(PLANT(J),J=1,6)                                       PLV24280
 71   FORMAT(20X,  'VISUAL EFFECTS FOR NON-HORIZONTAL CLEAR SKY VIEWS THPLV24290
     1ROUGH PLUME CENTER',//,20X,6A4,//)                                PLV24300
C                                                                       PLV24310
C  XKM = DOWNWIND DISTANCE IN KM                                        PLV24320
C  H = PLUME CENTERLINE HEIGHT                                          PLV24330
C  BRANCH IF DOING PLUME-BASED CALCULATION                              PLV24340
C                                                                       PLV24350
      WRITE(6,52) XKM                                                   PLV24360
      WRITE(6,53) H                                                     PLV24370
      IF(NC.EQ.1)GO TO 7200                                             PLV24380
C                                                                       PLV24390
C  WRITE OUT DISTANCE FROM OBSERVER TO PLUME ALONG LINE-OF-SIGHT.       PLV24400
C                                                                       PLV24410
      WRITE(6,611)OBSPLU(NX)                                            PLV24420
C                                                                       PLV24430
C  WRITE OUT AZIMUTH OF OBSERVER LINE-OF-SIGHT TO PLUME                 PLV24440
C                                                                       PLV24450
      WRITE(6,612)AZMUTH(NX)                                            PLV24460
C                                                                       PLV24470
C  WRITE OUT ELEVATION ANGLE OF OBSERVER LINE-OF-SIGHT.                 PLV24480
C                                                                       PLV24490
      WRITE(6,613)ABETA(NX)                                             PLV24500
C                                                                       PLV24510
C  WRITE OUT SOLAR ZENITH ANGLE, TIME OF DAY, MONTH, DAY OF MONTH       PLV24520
C                                                                       PLV24530
      WRITE(6,614)ZENITH,TIME,IMO,IDAY                                  PLV24540
C                                                                       PLV24550
C  ASSIGN SCATTERING ANGLE AND INDEX FOR SCATTERING PHASE FUNCTIONS.    PLV24560
C                                                                       PLV24570
      THETA=TT(NX+7)                                                    PLV24580
      ITHETA=7+NX                                                       PLV24590
C                                                                       PLV24600
C  ASSIGN HORIZONTAL ANGLE BETWEEN PLUME CENTERLINE AND LINE-OF-SIGHT   PLV24610
C                                                                       PLV24620
      ALPHA=AALPHA(NX)                                                  PLV24630
C                                                                       PLV24640
C  ASSIGN ELEVATION ANGLE FOR LINE OF SIGHT TO POINT ON PLUME CENTERLINEPLV24650
C                                                                       PLV24660
      BETA=ABETA(NX)                                                    PLV24670
C                                                                       PLV24680
C  ASSIGN DISTANCE TO PLUME CENTERLINE ALONG LINE-OF-SIGHT.             PLV24690
C                                                                       PLV24700
      RP=OBSPLU(NX)                                                     PLV24710
 7200 CONTINUE                                                          PLV24720
      WRITE(6,72)                                                       PLV24730
 72   FORMAT(/,6H THETA,3X,5HALPHA,4X,4HBETA,6X,2HRP,                   PLV24740
     14X,4HYCAP,7X,1HL,7X,1HX,7X,1HY,8H DELYCAP,4X,4HDELL,2X,6HC(550),  PLV24750
     2 2X,6HBRATIO,4X,4HDELX,4X,4HDELY,2X,6HE(LUV),2X,6HE(LAB))         PLV24760
 75   CONTINUE                                                          PLV24770
      WRITE(6,66) THETA                                                 PLV24780
C                                                                       PLV24790
C  BRANCH FOR OBSERVER-BASED CASE CALCULATIONS.                         PLV24800
C                                                                       PLV24810
      IF(NC.EQ.2)GO TO 7300                                             PLV24820
C                                                                       PLV24830
C  INITIALIZE AND INCREMENT FOR LOOP ON ALPHA ANGLE                     PLV24840
C                                                                       PLV24850
      NA=0                                                              PLV24860
 7997 NA=NA+1                                                           PLV24870
      ALPHA=AA(NA)                                                      PLV24880
C                                                                       PLV24890
C  INITIALIZE AND INCREMENT FOR LOOP ON OBSERVER LINE-OF-SIGHT          PLV24900
C  ELEVATION ANGLE.                                                     PLV24910
C                                                                       PLV24920
        NB=0                                                            PLV24930
 7999 NB=NB+1                                                           PLV24940
      BETA=15.*FLOAT(NB)                                                PLV24950
C                                                                       PLV24960
C  CALCULATION OF OBSERVER LINE-OF-SIGHT TO PLUME DISTANCE.             PLV24970
C                                                                       PLV24980
      DELZ=H*1.E-3                                                      PLV24990
      DELXY=DELZ*COS(BETA*RAD)/SIN(BETA*RAD)                            PLV25000
      DELX=DELXY*COS(ALPHA*RAD)                                         PLV25010
      DELY=DELXY*SIN(ALPHA*RAD)                                         PLV25020
      RP=SQRT(DELZ*DELZ+DELY*DELY+DELX*DELX)                            PLV25030
 7300 CONTINUE                                                          PLV25040
      CY=CHIQIY(3)*COS(BETA*RAD)                                        PLV25050
      CZ=CHIQIY(3)*SIN(BETA*RAD)                                        PLV25060
      YZINT=SQRT(CY*CY+CZ*CZ)/SIN(ALPHA*RAD)                            PLV25070
      PLUMEP=YZINT*QPART*(1.-FRACTC)                                    PLV25080
      PLUMEC=YZINT*QPART*FRACTC                                         PLV25090
                                                                        PLV25100
      PLUMES=YZINT*(QSO4+QNO3)                                          PLV25110
      PLUMEN=YZINT*QNO2I(3)                                             PLV25120
C                                                                       PLV25130
C  CALL ROUTINE TO CALCULATE SKY INTENSITIES FOR PLUME AND SKY          PLV25140
C  WITHOUT PLUME.                                                       PLV25150
C                                                                       PLV25160
      CALL PLMCLN(ZENITH,BETA,THETA,ITHETA,PLUMEP,PLUMES,PLUMEN,        PLV25170
     1 PLUMEC,SPECB,SPECP,RP,THICK)                                     PLV25180
C                                                                       PLV25190
C  CALL ROUTINE TO CALCULATE COLORATION PARAMETERS.                     PLV25200
C                                                                       PLV25210
      CALL CHROMA(SPECP,SPECB)                                          PLV25220
      WRITE(6,78) ALPHA,BETA,RP,YCAP,VAL,X,Y,YCAPD,VALD,CONT2,          PLV25230
     1BRATIO,XD,YD,DELUV,DELAB                                          PLV25240
 78   FORMAT(5X,2F8.0,3F8.2,2F8.4,2F8.2,6F8.4)                          PLV25250
C                                                                       PLV25260
C  CHECK IF END OF LOOPS FOR PLUME-BASED CALCULATION.                   PLV25270
C                                                                       PLV25280
      IF(NC.EQ.2)GO TO 8000                                             PLV25290
      IF(NB.LT.6)GO TO 7999                                             PLV25300
      IF(NA.LT.4)GO TO 7997                                             PLV25310
      IF(NT.LT.(NT2-1))GO TO 7996                                       PLV25320
 8000 CONTINUE                                                          PLV25330
 8001 CONTINUE                                                          PLV25340
      IF(IFLG3.EQ.0) GO TO 9001                                         PLV25350
C                                                                       PLV25360
C **********************************************************************PLV25370
C                                                                       PLV25380
C  ***** WHITE, GRAY, AND BLACK OBJECT VIEWS ***************************PLV25390
C                                                                       PLV25400
C **********************************************************************PLV25410
C                                                                       PLV25420
C  OPTICAL EFFECTS ON VIEW OF OBJECT BEHIND PLUME.                      PLV25430
C  CALCULATION OF PLUME PARTICULATE, SULFATE, AND NO2 INTEGRAL          PLV25440
C  ACROSS PLUME.                                                        PLV25450
C                                                                       PLV25460
      PLUMEP=CHIQIY(3)*QPART*(1.-FRACTC)                                PLV25470
      PLUMES=CHIQIY(3)*(QSO4+QNO3)                                      PLV25480
      PLUMEN=CHIQIY(3)*QNO2I(3)                                         PLV25490
      PLUMEC=CHIQIY(3)*QPART*FRACTC                                     PLV25500
C                                                                       PLV25510
C  LOOP FOR PLUME-BASED, OBSERVER-BASED, OR BOTH TYPES OF CALCULATIONS  PLV25520
C                                                                       PLV25530
      DO 9000 NC=NC1,NC2                                                PLV25540
C                                                                       PLV25550
C  BRANCH IF DOING THE PLUME-BASED CALCULATION                          PLV25560
C                                                                       PLV25570
      IF(NC.EQ.1) GO TO 8002                                            PLV25580
C                                                                       PLV25590
C  ADJUST FOR OBSERVER-BASED ALPHA FOR SPECIFIC POINT                   PLV25600
C                                                                       PLV25610
      F3=SIN(AALPHA(NX)*RAD)                                            PLV25620
      PLUMEP=PLUMEP/F3                                                  PLV25630
      PLUMES=PLUMES/F3                                                  PLV25640
      PLUMEN=PLUMEN/F3                                                  PLV25650
      PLUMEC=PLUMEC/F3                                                  PLV25660
 8002 CONTINUE                                                          PLV25670
C                                                                       PLV25680
C  BRANCH IF DOING THE OBSERVER-BASED CALCULATION.                      PLV25690
C                                                                       PLV25700
      IF(NC.EQ.2)GO TO 8100                                             PLV25710
C                                                                       PLV25720
C  INITIALIZE AND INCREMENT SCATTERING ANGLE INDEX FOR PLUME-BASED      PLV25730
C  CASE CALCULATION.                                                    PLV25740
C                                                                       PLV25750
      NT=NT1-1                                                          PLV25760
 8995 NT=NT+1                                                           PLV25770
      ITHETA=NT+1                                                       PLV25780
      THETA=TT(ITHETA)                                                  PLV25790
 8100 CONTINUE                                                          PLV25800
      WRITE(6,10)                                                       PLV25810
      IF(NC.EQ.1)WRITE(6,81)(PLANT(J),J=1,6)                            PLV25820
      IF(NC.EQ.2)WRITE(6,87)(PLANT(J),J=1,6)                            PLV25830
 81   FORMAT(10X,41HPLUME VISUAL EFFECTS FOR HORIZONTAL VIEWS,/,10X,    PLV25840
     160HPERPENDICULAR TO THE PLUME OF WHITE, GRAY, AND BLACK OBJECTS,/,PLV25850
     210X,56HFOR VARIOUS OBSERVER-PLUME AND OBSERVER-OBJECT DISTANCES,  PLV25860
     3//10X,6A4,//)                                                     PLV25870
 87   FORMAT(10X,41HPLUME VISUAL EFFECTS FOR HORIZONTAL VIEWS,/,10X,    PLV25880
     146HOF THE PLUME OF WHITE, GRAY, AND BLACK OBJECTS,/,              PLV25890
     210X,57HFOR SPECIFIC OBSERVER-PLUME AND OBSERVER-OBJECT DISTANCES, PLV25900
     3//10X,6A4,//)                                                     PLV25910
      WRITE(6,52) XKM                                                   PLV25920
C                                                                       PLV25930
C  BRANCH IF DOING PLUME-BASED CALCULATION                              PLV25940
C                                                                       PLV25950
      IF(NC.EQ.1)GO TO 84                                               PLV25960
      WRITE(6,611)OBSPLU(NX)                                            PLV25970
      WRITE(6,612)AZMUTH(NX)                                            PLV25980
      WRITE(6,613)ABETA(NX)                                             PLV25990
      WRITE(6,614)ZENITH,TIME,IMO,IDAY                                  PLV26000
C                                                                       PLV26010
C  ASSIGN SPECIFIC SCATTERING ANGLE AND INDEX FOR SCATTERING ANGLE PHASEPLV26020
C   FUNCTIONS.                                                          PLV26030
C                                                                       PLV26040
      THETA=TT(NX+7)                                                    PLV26050
      ITHETA=7+NX                                                       PLV26060
   84 WRITE(6,82)THETA                                                  PLV26070
 82   FORMAT(9H THETA = ,F5.0)                                          PLV26080
      WRITE(6,83)                                                       PLV26090
 83   FORMAT(/,1X,7HREFLECT,2X,6HRP/RV0,2X,6HRO/RV0,4X,4HYCAP,7X,1HL,7X,PLV26100
     11HX,7X,1HY,8H DELYCAP,4X,4HDELL,2X,6HC(550),2X,6HBRATIO,4X,4HDELX,PLV26110
     24X,4HDELY,2X,6HE(LUV),2X,6HE(LAB),/)                              PLV26120
C                                                                       PLV26130
C  LOOP ON REFLECTIVITY OF OBJECT (K=1 FOR WHITE, K=2 FOR GRAY, K=3 FOR PLV26140
C  BLACK)                                                               PLV26150
C                                                                       PLV26160
      DO 8999 K=1,3                                                     PLV26170
      XLUMIN=REFL(K)/(2.*PI)                                            PLV26180
C                                                                       PLV26190
C  BRANCH IF OBSERVER-BASED CASE                                        PLV26200
C                                                                       PLV26210
      IF(NC.EQ.2)GO TO 86                                               PLV26220
C                                                                       PLV26230
C  INITIALIZE AND INCREMENT INDEX FOR OBSERVER TO PLUME DISTANCE        PLV26240
C  ALONG LINE-OF-SIGHT.                                                 PLV26250
C                                                                       PLV26260
      IP=0                                                              PLV26270
 8996 IP=IP+1                                                           PLV26280
      NRO=7-IP                                                          PLV26290
C                                                                       PLV26300
C  INITIALIZE AND INCREMENT INDEX FOR DISTANCE FROM OBSERVER TO         PLV26310
C  BACKGROUND OBJECT ALONG LINE-OF-SIGHT.                               PLV26320
C                                                                       PLV26330
      IO1=0                                                             PLV26340
 8997 IO1=IO1+1                                                         PLV26350
      RP=ROBJ(IP)*RVAMB                                                 PLV26360
      IPIO1=IP+IO1-1                                                    PLV26370
      RO=ROBJ(IPIO1)*RVAMB                                              PLV26380
      RPR=(RP/SY)*1000.                                                 PLV26390
      ROR=((RO-RP)/SY)*1000.                                            PLV26400
      IF(RPR.GT.2.17.AND.ROR.GT.2.17)GO TO 8991                         PLV26410
C                                                                       PLV26420
C   IF OBSERVER OR OBJECT BACKGROUND IS WITHIN THE PLUME, CHANGE THE    PLV26430
C   OBSERVER-TO-PLUME DISTANCE TO BE THE DISTANCE TO THE CENTROID OF    PLV26440
C   THE AREA UNDER THE GAUSSIAN CURVE FOR THE PLUME BETWEEN THE         PLV26450
C   OBSERVER AND THE BACKGROUND OBJECT.                                 PLV26460
C                                                                       PLV26470
      CALL PLMIN(RPR,ROR,SY,RP,YINTR)                                   PLV26480
      PLUMP1=PLUMEP*YINTR                                               PLV26490
      PLUMS1=PLUMES*YINTR                                               PLV26500
      PLUMN1=PLUMEN*YINTR                                               PLV26510
      PLUMC1=PLUMEC*YINTR                                               PLV26520
C                                                                       PLV26530
C  CALL ROUTINE TO CALCULATE INTENSITIES FOR OBJECT W/O PLUME           PLV26540
C  (SPECO) AND WITH PLUME (SPECP).                                      PLV26550
C                                                                       PLV26560
      CALL PLMOBJ(ZENITH,BETA,THETA,ITHETA,PLUMP1,PLUMS1,PLUMN1,        PLV26570
     1PLUMC1,XLUMIN,RO,RP,SPECO,SPECP)                                  PLV26580
      GO TO 89                                                          PLV26590
8991  CALL PLMOBJ(ZENITH,BETA,THETA,ITHETA,PLUMEP,PLUMES,PLUMEN,        PLV26600
     1PLUMEC,XLUMIN,RO,RP,SPECO,SPECP)                                  PLV26610
C                                                                       PLV26620
C  CALL ROUTINE TO CALCULATE COLORATION PARAMETERS.                     PLV26630
                                                                        PLV26640
89    CONTINUE                                                          PLV26650
      CALL CHROMA(SPECP,SPECO)                                          PLV26660
      IPIO1=IP+IO1-1                                                    PLV26670
      WRITE(6,85) REFL(K),ROBJ(IP),ROBJ(IPIO1),YCAP,VAL,X,Y,YCAPD,      PLV26680
     1VALD,CONT2,BRATIO,XD,YD,DELUV,DELAB                               PLV26690
 85   FORMAT(F8.1,4F8.2,2F8.4,2F8.2,6F8.4)                              PLV26700
C                                                                       PLV26710
C  SPECIFY OBJECT DISTANCE, PLUME-OBSERVER DISTANCE, AND SCATTERING ANGLPLV26720
C  FOR SAVING RESULTS TO BE PLOTTED.                                    PLV26730
C                                                                       PLV26740
      IF(IO1.NE.IO1P)GO TO 8990                                         PLV26750
      IF(IP.NE.IPP)GO TO 8990                                           PLV26760
      IF(NT.NE.NTP)GO TO 8990                                           PLV26770
C                                                                       PLV26780
C  VISUAL RANGE REDUCTION IS DEFINED FOR CLEAR SKY ONLY.                PLV26790
C                                                                       PLV26800
      PLOT1(NX,K+1)=0.                                                  PLV26810
      PLOT2(NX,K+1)=BRATIO                                              PLV26820
      PLOT3(NX,K+1)=CONT2                                               PLV26830
      IF(INTSKY .GT. 0.) PLOT33(NX,K+1)=CONT2*SPECO(19)/INTSKY          PLV26835
      IF(INTSKY .LE. 0.) PLOT33(NX,K+1)= -99.                           PLV26838
      PLOT4(NX,K+1)=DELAB                                               PLV26840
C                                                                       PLV26850
C  CHECK IF END OF LOOP ON BACKGROUND OBJECT DISTANCE INDEX.            PLV26860
C                                                                       PLV26870
8990  IF(IO1.LT.NRO)GO TO 8997                                          PLV26880
C                                                                       PLV26890
C  CHECK IF END OF LOOP ON OBSERVER-PLUME DISTANCE INDEX.               PLV26900
C                                                                       PLV26910
      IF(IP.LT.6)GO TO 8996                                             PLV26920
       GO TO 8999                                                       PLV26930
C                                                                       PLV26940
C  CONTINUE WITH OBSERVER-BASED CALCULATION.  ASSIGN SCATTERING ANGLE.  PLV26950
C                                                                       PLV26960
   86 ITHETA=7+NX                                                       PLV26970
      THETA=TT(ITHETA)                                                  PLV26980
      RP=OBSPLU(NX)                                                     PLV26990
C                                                                       PLV27000
C  JUMP OUT OF CALCULATION IF OBJECT IS BETWEEN CENTER                  PLV27010
C  OF PLUME AND OBSERVER.                                               PLV27020
C                                                                       PLV27030
      IF(OBSPLU(NX).GT.ROBJT(NX))WRITE(6,90)                            PLV27040
90    FORMAT(10X,  'BACKGROUND OBJECT IS BETWEEN OBSERVER AND CENTER OF PLV27050
     1PLUME AND CALCULATION IS STOPPED.')                               PLV27060
      IF(OBSPLU(NX).GT.ROBJT(NX))GO TO 9000                             PLV27070
C                                                                       PLV27080
C  THE FOLLOWING TWO LINES WERE CORRECTED BY DCD (SIGMA RESEARCH)       PLV27090
C  (4/8/93) BASED ON DISCUSSIONS WITH EPA AND NPS                       PLV27100
C  RPR AND ROR SHOULD BE PERPENDICULAR DISTANCES; THEREFORE,            PLV27110
C  RPR AND ROR ARE MULTIPLIED BY (SIN(AALPHA(NX)*RAD)                   PLV27120
C                                                                       PLV27130
      RPR=(RP/SY)*1000.*(SIN(AALPHA(NX)*RAD))                           PLV27140
      ROR=((ROBJT(NX)-RP)/SY)*1000.*(SIN(AALPHA(NX)*RAD))               PLV27150
      IF(RPR.GT.2.17.AND.ROR.GT.2.17)GO TO 8992                         PLV27160
C                                                                       PLV27170
C   IF OBSERVER OR OBJECT IS WITHIN THE PLUME, CHANGE THE OBSERVER-TO-  PLV27180
C   PLUME DISTANCE TO BE THE DISTANCE TO THE CENTROID OF THE AREA UNDER PLV27190
C   THE GAUSSIAN CURVE FOR THE PLUME BETWEEN THE OBSERVER AND THE BACK- PLV27200
C   GROUND OBJECT.                                                      PLV27210
C                                                                       PLV27220
      CALL PLMIN(RPR,ROR,SY,RP,YINTR)                                   PLV27230
C                                                                       PLV27240
C   CORRECT FOR VIEWS NOT PERPENDICULAR TO THE PLUME CENTERLINE         PLV27250
C                                                                       PLV27260
      RP=RP/SIN(AALPHA(NX)*RAD)                                         PLV27270
      PLUMP1=PLUMEP*YINTR                                               PLV27280
      PLUMS1=PLUMES*YINTR                                               PLV27290
      PLUMN1=PLUMEN*YINTR                                               PLV27300
      PLUMC1=PLUMEC*YINTR                                               PLV27310
C                                                                       PLV27320
C  CALL ROUTINE TO CALCULATE INTENSITIES WITH AND WITHOUT PLUME.        PLV27330
C                                                                       PLV27340
      CALL PLMOBJ(ZENITH,ABETA(NX),THETA,ITHETA,PLUMP1,PLUMS1,          PLV27350
     1 PLUMN1,PLUMC1,XLUMIN,ROBJT(NX),RP,SPECO,SPECP)                   PLV27360
      GO TO 91                                                          PLV27370
8992  CALL PLMOBJ(ZENITH,ABETA(NX) ,THETA,ITHETA,PLUMEP,PLUMES,         PLV27380
     1 PLUMEN,PLUMEC,XLUMIN,ROBJT(NX),RP,SPECO,SPECP)                   PLV27390
C                                                                       PLV27400
C  CALL ROUTINE TO CALCULATE COLORATION PARAMETERS.                     PLV27410
C                                                                       PLV27420
91    CONTINUE                                                          PLV27430
      CALL CHROMA(SPECP,SPECO)                                          PLV27440
C                                                                       PLV27450
C  SAVE RESULTS FOR PLOTTING.                                           PLV27460
C                                                                       PLV27470
      PLT1(NX,K+1)=0.                                                   PLV27480
      PLT2(NX,K+1)=BRATIO                                               PLV27490
      PLT3(NX,K+1)=CONT2                                                PLV27500
      IF(INTSKY .GT. 0.) PLT33(NX,K+1)=CONT2*SPECO(19)/INTSKY           PLV27505
      IF(INTSKY .LE. 0.) PLT33(NX,K+1)= -99.                            PLV27508
      PLT4(NX,K+1)=DELAB                                                PLV27510
 8998 CONTINUE                                                          PLV27520
      RPLMOB=OBSPLU(NX)/RVAMB                                           PLV27530
      RROBJT=ROBJT(NX)/RVAMB                                            PLV27540
      WRITE(6,85)REFL(K),RPLMOB,RROBJT,YCAP,VAL,X,Y,YCAPD,VALD,         PLV27550
     1 CONT2,BRATIO,XD,YD,DELUV,DELAB                                   PLV27560
 8999 CONTINUE                                                          PLV27570
C                                                                       PLV27580
C  CHECK IF LOOP ON SCATTERING ANGLE FOR PLUME-BASED CASE IS COMPLETED  PLV27590
C                                                                       PLV27600
      IF(NC.EQ.1.AND.NT.LT.(NT2-1))GO TO 8995                           PLV27610
 9000 CONTINUE                                                          PLV27620
 9001 CONTINUE                                                          PLV27630
C                                                                       PLV27640
C  SKIP VISUAL EFFECTS ALONG PLUME IF IFLG4=0                           PLV27650
C                                                                       PLV27660
      IF(IFLG4.EQ.0)GO TO 1301                                          PLV27670
C                                                                       PLV27680
C **********************************************************************PLV27690
C                                                                       PLV27700
C** VISUAL EFFECTS ALONG PLUME *****************************************PLV27710
C                                                                       PLV27720
C **********************************************************************PLV27730
C                                                                       PLV27740
C LOOP ON TYPE OF CALCULATION: PLUME-BASED, OBSERVER-BASED, OR BOTH.    PLV27750
C                                                                       PLV27760
      DO 1300 NC=NC1,NC2                                                PLV27770
C                                                                       PLV27780
C  LOOP FOR CALC AT PLUME CL AND AT GROUND.                             PLV27790
C                                                                       PLV27800
      DO 1300 NZ1=1,NZF                                                 PLV27810
      NZ=NZ1*3                                                          PLV27820
      IF(NZ.EQ.3)Z=H                                                    PLV27830
      IF(NZ.EQ.6)Z=0.                                                   PLV27840
C                                                                       PLV27850
C  BRANCH IF DOING OBSERVER-BASED CALCULATION.                          PLV27860
C                                                                       PLV27870
      IF(NC.EQ.2)GO TO 1002                                             PLV27880
C                                                                       PLV27890
C  INITIALIZE AND INCREMENT LOOP FOR SCATTERING ANGLE                   PLV27900
C                                                                       PLV27910
      NT=NT1-1                                                          PLV27920
1001  NT=NT+1                                                           PLV27930
      ITHETA=NT+1                                                       PLV27940
      THETA=TT(ITHETA)                                                  PLV27950
1002  CONTINUE                                                          PLV27960
      WRITE(6,10)                                                       PLV27970
      WRITE(6,1010)(PLANT(J),J=1,6)                                     PLV27980
1010  FORMAT(10X,45HVISUAL EFFECTS FOR LINES OF SIGHT ALONG PLUME//,10X,PLV27990
     1 6A4//)                                                           PLV28000
      WRITE(6,52)XKM                                                    PLV28010
C                                                                       PLV28020
C  BRANCH FOR PLUME-BASED CALCULATION.                                  PLV28030
C                                                                       PLV28040
      IF(NC.EQ.1)GO TO 1005                                             PLV28050
      WRITE(6,611)OBSPLU(NX)                                            PLV28060
      WRITE(6,612)AZMUTH(NX)                                            PLV28070
      WRITE(6,613)ABETA(NX)                                             PLV28080
      WRITE(6,614)ZENITH,TIME,IMO,IDAY                                  PLV28090
C                                                                       PLV28100
C  ASSIGN PLUME-BASED CASE SCATTERING ANGLE AND SCATTERING PHASE FUNCTIOPLV28110
C  INDEX.                                                               PLV28120
C                                                                       PLV28130
      THETA=TT(NX+7)                                                    PLV28140
      ITHETA=7+NX                                                       PLV28150
1005  CONTINUE                                                          PLV28160
      WRITE(6,1020)                                                     PLV28170
1020  FORMAT(6H THETA,1X,6HLENGTH,1X,6HRP/RV0,4X,2HRV,2X,8H%REDUCED,4X, PLV28180
     1 4HYCAP,7X,1HL,7X,1HX,7X,1HY,8H DELYCAP,4X,4HDELL,2X,6HC(550),2X, PLV28190
     2 6HBRATIO,4X,4HDELX,4X,4HDELY,2X,6HE(LUV),2X,6HE(LAB))            PLV28200
1021  CONTINUE                                                          PLV28210
      WRITE(6,66)THETA                                                  PLV28220
C                                                                       PLV28230
C  BRANCH IF DOING PLUME-BASED CASE CALCULATION                         PLV28240
C                                                                       PLV28250
      IF(NC.EQ.1)GO TO 1008                                             PLV28260
C                                                                       PLV28270
C  OBLIQUE ANGLE CALCULATIONS FOR SPECIFIC CASE                         PLV28280
C  CONCENTRATIONS FOR PRIMARY PARTICULATE, SO4, AND NO2                 PLV28290
C                                                                       PLV28300
      CPAVE=XPARTP(NX,NZ)*(1.-FRACTC)                                   PLV28310
      CSAVE=XPARTS(NX,NZ)                                               PLV28320
      CNAVE=XNO2I(NX,NZ)                                                PLV28330
      CARAVE=XPARTP(NX,NZ)*FRACTC                                       PLV28340
C                                                                       PLV28350
C  DISTANCE WITHIN PLUME                                                PLV28360
C                                                                       PLV28370
       XALONG =((SY/1000.)*SQRT(PI*2.))/(SIN(AALPHA(NX)*RAD))           PLV28380
      XLONG2=XALONG/2.                                                  PLV28390
C                                                                       PLV28400
C  CHECK IF OBSERVER IN PLUME, IF YES , MODIFY XALONG                   PLV28410
C                                                                       PLV28420
      IF(OBSPLU(NX).LT.XLONG2)XALONG=OBSPLU(NX)+XLONG2                  PLV28430
C                                                                       PLV28440
C  CALCULATIONS FOR CLEAR SKY AND THREE BACKGROUND REFLECTANCES.        PLV28450
C                                                                       PLV28460
      ALONG=OBSPLU(NX)-XLONG2                                           PLV28470
      IF(ALONG.LT.0.0)ALONG=0.0                                         PLV28480
      DO 1009 K=1,4                                                     PLV28490
C                                                                       PLV28500
C  K=1 FOR CLEAR SKY, K=2 FOR WHITE OBJECTS, K=3 FOR GRAY OBJECTS,      PLV28510
C  K=4 FOR BLACK OBJECTS.                                               PLV28520
C  BRANCH IF K > 1                                                      PLV28530
C                                                                       PLV28540
      IF(K.GT.1)GO TO 1025                                              PLV28550
C                                                                       PLV28560
C  BACKGROUND CLEAR SKY CASE                                            PLV28570
C  CALL ROUTINE TO DEFINE BACKGROUND SKY INTENSITY WITHOUT PLUME        PLV28580
C                                                                       PLV28590
      CALL BACCLN(ZENITH,0.,THETA,ITHETA,SPECB)                         PLV28600
      GO TO 1026                                                        PLV28610
1025  CONTINUE                                                          PLV28620
C                                                                       PLV28630
C  CALCULATION FOR BACKGROUND OBJECT.                                   PLV28640
C                                                                       PLV28650
C  DISTANCE CALULATIONS ARE THE SAME FOR K = 2, 3, 4.                   PLV28660
C                                                                       PLV28670
      IF(ROBJT(NX).LE.0.0)GO TO 1009                                    PLV28680
      XLUMIN=REFL(K-1)/(2.*PI)                                          PLV28690
C                                                                       PLV28700
C  DISTANCE CALCULATIONS DONE ONCE FOR K=2 / SKIP AROUND FOR K =3,4     PLV28710
C                                                                       PLV28720
      IF(K.GT.2)GO TO 95                                                PLV28730
C                                                                       PLV28740
C  DISTANCE FROM OBJECT TO BACK SIDE OF PLUME                           PLV28750
C                                                                       PLV28760
      RO=ROBJT(NX)-(OBSPLU(NX)+XLONG2)                                  PLV28770
C                                                                       PLV28780
C   RESET RO IF OBJECT IS WITHIN BACK SIDE OF PLUME                     PLV28790
C                                                                       PLV28800
      IF((ROBJT(NX)-OBSPLU(NX)).LT.XLONG2)RO=0.                         PLV28810
C                                                                       PLV28820
C   XALONG IS THE DISTANCE THAT THE LINE OF SIGHT IS WITHIN PLUME.      PLV28830
C   RECALCULATE  XALONG IF OBJECT IS WITHIN BACK SIDE OF PLUME AND      PLV28840
C   OBSERVER IS OUTSIDE PLUME.                                          PLV28850
C                                                                       PLV28860
      IF((ROBJT(NX)-OBSPLU(NX)).LT.XLONG2)XALONG=XLONG2+(ROBJT(NX)-     PLV28870
     1 OBSPLU(NX))                                                      PLV28880
C                                                                       PLV28890
C  ADJUSTMENT IF BOTH OBJECT AND OBSERVER ARE IN FRONT OF PLUME.        PLV28900
C                                                                       PLV28910
      IF(OBSPLU(NX).LT.XLONG2)GO TO 97                                  PLV28920
      IF(ROBJT(NX).LT.(OBSPLU(NX)-XLONG2))GO TO 96                      PLV28930
      GO TO 97                                                          PLV28940
96    XALONG=0.                                                         PLV28950
      RO=0.                                                             PLV28960
      ALONG=ROBJT(NX)                                                   PLV28970
97    CONTINUE                                                          PLV28980
C                                                                       PLV28990
C  ADJUSTMENT IF BOTH OBSERVER AND BACKGROUND OBJECT ARE WITHIN FRONT   PLV29000
C  SIDE OF PLUME                                                        PLV29010
C                                                                       PLV29020
      IF(ROBJT(NX).GT.OBSPLU(NX))GO TO 98                               PLV29030
      IF(OBSPLU(NX).GT.XLONG2)GO TO 98                                  PLV29040
      XALONG=ROBJT(NX)                                                  PLV29050
      ALONG=0.                                                          PLV29060
      RO=0.                                                             PLV29070
98    CONTINUE                                                          PLV29080
C                                                                       PLV29090
C   ADJUSTMENT IF OBSERVER IS OUTSIDE PLUME AND BACKGROUND OBJECT IS    PLV29100
C   WITHIN FRONT SIDE OF PLUME.                                         PLV29110
C                                                                       PLV29120
      IF(ROBJT(NX).GT.OBSPLU(NX))GO TO 104                              PLV29130
      IF(ROBJT(NX).LT.(OBSPLU(NX)-XLONG2))GO TO 104                     PLV29140
      IF((OBSPLU(NX)-XLONG2).LE.0.)GO TO 104                            PLV29150
      RO=0.                                                             PLV29160
      XALONG=ROBJT(NX)-(OBSPLU(NX)-XLONG2)                              PLV29170
      ALONG=OBSPLU(NX)-XLONG2                                           PLV29180
104   CONTINUE                                                          PLV29190
C                                                                       PLV29200
C  ADJUSTMENT FOR OBSERVER WITHIN FRONT SIDE OF PLUME AND OBJECT        PLV29210
C  BEHIND PLUME.                                                        PLV29220
C                                                                       PLV29230
      IF(OBSPLU(NX).GT.XLONG2)GO TO 102                                 PLV29240
      IF(ROBJT(NX).LT.(OBSPLU(NX)+XLONG2))GO TO 102                     PLV29250
      XALONG=XLONG2+OBSPLU(NX)                                          PLV29260
      RO=ROBJT(NX)-(OBSPLU(NX)+XLONG2)                                  PLV29270
      ALONG=0.                                                          PLV29280
102   CONTINUE                                                          PLV29290
C                                                                       PLV29300
C  ADJUSTMENT FOR OBSERVER WITHIN FRONT SIDE OF PLUME AND OBJECT        PLV29310
C  WITHIN BACK SIDE OF PLUME.                                           PLV29320
C                                                                       PLV29330
      IF(OBSPLU(NX).GT.XLONG2)GO TO 103                                 PLV29340
      IF(ROBJT(NX).LT.OBSPLU(NX))GO TO 103                              PLV29350
      IF(ROBJT(NX).GT.(OBSPLU(NX)+XLONG2))GO TO 103                     PLV29360
      RO=0.                                                             PLV29370
      ALONG=0.                                                          PLV29380
      XALONG=ROBJT(NX)                                                  PLV29390
103   CONTINUE                                                          PLV29400
95    CONTINUE                                                          PLV29410
C                                                                       PLV29420
C  CALL ROUTINE TO CALCULATE INTENSITY FOR BACKGROUND OBJECT            PLV29430
C  AT BACK SIDE OF PLUME.                                               PLV29440
C                                                                       PLV29450
      CALL BACOBJ(ZENITH,0.,THETA,ITHETA,RO,SPECB,XLUMIN)               PLV29460
1026  CONTINUE                                                          PLV29470
C                                                                       PLV29480
C  STORE BACKGROUND INTENSITIES AT BACK SIDE OF PLUME IN SPECP ARRAY.   PLV29490
C                                                                       PLV29500
      DO 1027 I=1,39                                                    PLV29510
      SPECP(I)=SPECB(I)                                                 PLV29520
1027  CONTINUE                                                          PLV29530
C                                                                       PLV29540
C     INTENSITY FOR PLUME SEGMENT(LINE OF SIGHT FOR SPECIFIC CASE, NOT  PLV29550
C     NECESSARILY ON PLUME CENTERLINE.                                  PLV29560
C                                                                       PLV29570
      CALL PLMAX(ZENITH,BETA,THETA,ITHETA,CPAVE,CSAVE,CNAVE,CARAVE,     PLV29580
     1 XALONG,SPECP,SPECB)                                              PLV29590
C                                                                       PLV29600
C  INTENSITY CHANGE FOR BACKGROUND AIR BETWEEN PLUME AND OBSERVER       PLV29610
C                                                                       PLV29620
      CALL PLMAX(ZENITH,BETA,THETA,ITHETA,0.,0.,0.,0.,ALONG,SPECP,SPECB)PLV29630
C                                                                       PLV29640
C  CALL ROUTINE TO CALCULATE COLORATION PARAMETERS                      PLV29650
      CALL CHROMA(SPECP,SPECB)                                          PLV29660
C                                                                       PLV29670
C  OPTICAL DEPTH BETWEEN OBSERVER AND PLUME                             PLV29680
C                                                                       PLV29690
      TAUTOT=BTABAC(19)*ALONG                                           PLV29700
      TAU1=TAUTOT                                                       PLV29710
C                                                                       PLV29720
C  OPTICAL DEPTH DUE TO PLUME PRIMARY PARTICULATE AND SO4               PLV29730
C                                                                       PLV29740
      TAUTOT=TAUTOT+XALONG*(CPAVE*BTAPRM(19)+CSAVE*BTASO4(19)+          PLV29750
     1 BTABAC(19)+CNAVE*ABSNO2(19)+CARAVE*BTACAR(19))+ALOG(CONT2+1.)    PLV29760
C                                                                       PLV29770
C  CHECK IF OPTICAL DEPTH .GT. VISUAL RANGE LIMIT                       PLV29780
C                                                                       PLV29790
      IF(TAUTOT.GT.3.912)GO TO 1006                                     PLV29800
C                                                                       PLV29810
C  REDUCTION IN VISUAL RANGE                                            PLV29820
C                                                                       PLV29830
      DELRV=(3.912-TAUTOT)/BTABAC(19)                                   PLV29840
      RV=XALONG+ALONG+DELRV                                             PLV29850
      GO TO 1007                                                        PLV29860
1006  DELRV=(3.912-TAU)/(TAUTOT-TAU1)                                   PLV29870
      RV=ALONG+DELRV                                                    PLV29880
1007  REDRV=((RVAMB-RV)/RVAMB)*100.                                     PLV29890
      RR=OBSPLU(NX)/RVAMB                                               PLV29900
      IF(K.EQ.1)WRITE(6,47)                                             PLV29910
47    FORMAT(5X,19HFOR SKY BACKGROUND:)                                 PLV29920
      IF(K.EQ.2)WRITE(6,74)                                             PLV29930
74    FORMAT(5X,21HFOR WHITE BACKGROUND:)                               PLV29940
      IF(K.EQ.3)WRITE(6,76)                                             PLV29950
76    FORMAT(5X,20HFOR GRAY BACKGROUND:)                                PLV29960
      IF(K.EQ.4)WRITE(6,77)                                             PLV29970
77    FORMAT(5X,21HFOR BLACK BACKGROUND:)                               PLV29980
      WRITE(6,67)XALONG,RR,RV,REDRV,YCAP,VAL,X,Y,YCAPD,VALD,CONT2,      PLV29990
     1 BRATIO,XD,YD,DELUV,DELAB                                         PLV30000
1009  CONTINUE                                                          PLV30010
C                                                                       PLV30020
C END OF CALCULATIONS FOR NC=2                                          PLV30030
C                                                                       PLV30040
      GO TO 1300                                                        PLV30050
1008  CONTINUE                                                          PLV30060
C                                                                       PLV30070
C  PLUME-BASED CALCULATIONS FOR PLUME CENTERLINE.                       PLV30080
      NXEND=NX-1                                                        PLV30090
C  IF NX=1, SKIP OVER CALCULATION                                       PLV30100
C                                                                       PLV30110
      IF(NXEND.EQ.0)GO TO 1300                                          PLV30120
C                                                                       PLV30130
C  LOOP ON NUMBER OF PLUME SEGMENTS                                     PLV30140
C  LOOP ON PLUME SIGMENTS THAT CAN BE IN OBSERVER'S LINE-OF-SIGHT       PLV30150
C                                                                       PLV30160
      DO 1200 NXIN=1,NXEND                                              PLV30170
      NXX1=NX-NXIN                                                      PLV30180
C                                                                       PLV30190
C  CALL ROUTINE TO CALCULATE INTENSITIES FOR BACKGROUND SKY             PLV30200
C  WITHOUT PLUME.                                                       PLV30210
C                                                                       PLV30220
      CALL BACCLN(ZENITH,0.,THETA,ITHETA,SPECB)                         PLV30230
      DO 1022 I=1,39                                                    PLV30240
1022  SPECP(I)=SPECB(I)                                                 PLV30250
C                                                                       PLV30260
C  LOOP ON PLUME SEGMENTS BETWEEN DISTANCE NX AND FIRST POINT.          PLV30270
C                                                                       PLV30280
      DO 1100 NXX=NXX1,NXEND                                            PLV30290
C                                                                       PLV30300
C  USE MEANS OF END POINTS OF SEGMENTS TO DEFINE CONCENTRATIONS         PLV30310
C                                                                       PLV30320
      CPAVE=(XPARTP(NXX,NZ)+XPARTP(NXX+1,NZ))/2.*(1.-FRACTC)            PLV30330
      CSAVE=(XPARTS(NXX,NZ)+XPARTS(NXX+1,NZ))/2.                        PLV30340
      CNAVE=(XNO2I(NXX,NZ)+XNO2I(NXX+1,NZ))/2.                          PLV30350
      CARAVE=(XPARTP(NXX,NZ)+XPARTP(NXX+1,NZ))/2.*FRACTC                PLV30360
C                                                                       PLV30370
C  DISTANCE WITHIN PLUME IN KILOMETERS.                                 PLV30380
C                                                                       PLV30390
      XALONG=(DIST(NXX+1)-DIST(NXX))/1000.                              PLV30400
C                                                                       PLV30410
C  CHANGE INTENSITY FOR EACH SEGMENT OF PLUME (1 POINT TO THE NEXT).    PLV30420
C                                                                       PLV30430
      CALL PLMAX(ZENITH,BETA,THETA,ITHETA,CPAVE,CSAVE,CNAVE,CARAVE,     PLV30440
     1 XALONG,SPECP,SPECB)                                              PLV30450
1100  CONTINUE                                                          PLV30460
C                                                                       PLV30470
C  NESTED LOOP FOR VARIOUS OBSERVER-TO-PLUME-SEGMENT DISTANCES.         PLV30480
C                                                                       PLV30490
      DO 1200 NROBJ=1,7                                                 PLV30500
      NR1=NROBJ-1                                                       PLV30510
      XALONG=0.                                                         PLV30520
C                                                                       PLV30530
C  ROBJ NOT DEFINED FOR INDEX .LT. 1, THEREFORE NO INPUT BY AIRLIGHT    PLV30540
C  BETWEEN PLUME AND OBSERVER.                                          PLV30550
C                                                                       PLV30560
      IF(NROBJ.EQ.1)GO TO 1115                                          PLV30570
      IF(NROBJ.EQ.2)GO TO 1111                                          PLV30580
      GO TO 1112                                                        PLV30590
C                                                                       PLV30600
C  DISTANCE OF LINE-OF-SIGHT FROM OBSERVER TO PLUME SEGMENT.            PLV30610
C                                                                       PLV30620
1111  XALONG=ROBJ(NR1)*RVAMB                                            PLV30630
      GO TO 1110                                                        PLV30640
1112  CONTINUE                                                          PLV30650
      XALONG=(ROBJ(NR1)-ROBJ(NR1-1))*RVAMB                              PLV30660
1110  CALL PLMAX(ZENITH,BETA,THETA,ITHETA,0.,0.,0.,0.,XALONG,SPECP      PLV30670
     1,SPECB)                                                           PLV30680
1115  CONTINUE                                                          PLV30690
C                                                                       PLV30700
C  CALCULATE COLORATION PARAMETERS                                      PLV30710
      CALL CHROMA(SPECP,SPECB)                                          PLV30720
      IF(NROBJ.EQ.1)GO TO 1116                                          PLV30730
      GO TO 1117                                                        PLV30740
1116  TAUTOT=0.                                                         PLV30750
      GO TO 1118                                                        PLV30760
1117  CONTINUE                                                          PLV30770
C                                                                       PLV30780
C  OPTICAL DEPTH DUE TO AMBIENT BACKGROUND AIR                          PLV30790
C                                                                       PLV30800
      TAUTOT=BTABAC(19)*RVAMB*ROBJ(NR1)                                 PLV30810
1118  NXX=0                                                             PLV30820
C                                                                       PLV30830
C  NESTED LOOP ON DISTANCE WITHIN PLUME TO DETERMINE VISUAL RANGE.      PLV30840
C                                                                       PLV30850
1120  NXX=NXX+1                                                         PLV30860
      TAUT1=TAUTOT                                                      PLV30870
C                                                                       PLV30880
C  DISTANCE WITHIN PLUME SEGMENT                                        PLV30890
C                                                                       PLV30900
      NXDIFF=NX-NXX                                                     PLV30910
      XALONG=(DIST(NXDIFF+1)-DIST(NXDIFF))/1000.                        PLV30920
C                                                                       PLV30930
C  ADD OPTICAL DEPTH FOR PRIMARY PARTICULATE                            PLV30940
C                                                                       PLV30950
      TAUTOT=TAUTOT+XALONG*BTAPRM(19)*(XPARTP(NXDIFF+1,NZ)+XPARTP(NXDI  PLV30960
     1FF,NZ))*(1.-FRACTC)/2. + BTACAR(19)*FRACTC*(XPARTP(NXDIFF+1,NZ)   PLV30970
     2 + XPARTP(NXDIFF,NZ))*XALONG/2.                                   PLV30980
C                                                                       PLV30990
C  ADD OPTICAL DEPTH FOR SO4                                            PLV31000
C                                                                       PLV31010
      TAUTOT=TAUTOT+XALONG*BTASO4(19)*(XPARTS(NXDIFF+1,NZ)+XPARTS(NXDI  PLV31020
     1FF,NZ))/2.                                                        PLV31030
C                                                                       PLV31040
C  ADD OPTICAL DEPTH FOR NO2 ABSORPTION.                                PLV31050
C                                                                       PLV31060
      TAUTOT=TAUTOT+XALONG*ABSNO2(19)*(XNO2I(NXDIFF+1,NZ)+XNO2I(NXDIFF, PLV31070
     1NZ))/2.+ALOG(CONT2+1.)                                            PLV31080
C                                                                       PLV31090
C  ADD OPTICAL DEPTH FOR BACKGROUND AIR IN PLUME                        PLV31100
C                                                                       PLV31110
      TAUTOT=TAUTOT+XALONG*BTABAC(19)                                   PLV31120
C                                                                       PLV31130
C  STOP CALCULATION IF VISUAL RANGE EXCEEDED                            PLV31140
C                                                                       PLV31150
      IF(TAUTOT.GT.3.912)GO TO 1160                                     PLV31160
      IF(NXX.LT.NXIN)GO TO 1120                                         PLV31170
C                                                                       PLV31180
C   DETERMINE DISTANCE FROM BACKSIDE OF PLUME TO VISUAL RANGE LIMIT     PLV31190
C                                                                       PLV31200
      DELRV=(3.912-TAUTOT)/BTABAC(19)                                   PLV31210
C                                                                       PLV31220
C  CALCULATE VISUAL RANGE                                               PLV31230
C                                                                       PLV31240
      IF(NROBJ.GT.1)GO TO 1130                                          PLV31250
      RV=(DIST(NX)-DIST(NXX1))/1000.+DELRV                              PLV31260
      GO TO 1170                                                        PLV31270
1130  CONTINUE                                                          PLV31280
      RV=ROBJ(NR1)*RVAMB+(DIST(NX)-DIST(NXX1))/1000.+DELRV              PLV31290
      GO TO 1170                                                        PLV31300
1160  CONTINUE                                                          PLV31310
C                                                                       PLV31320
C  INTERPOLATE INTO PLUME TO GET VISUAL RANGE                           PLV31330
C                                                                       PLV31340
      DELRV= (3.912-TAUT1)/(TAUTOT-TAUT1) *XALONG                       PLV31350
      IF(NROBJ.GT.1)GO TO 1165                                          PLV31360
      RV=(DIST(NX)-DIST(NX+1-NXX))/1000.+DELRV                          PLV31370
      GO TO 1170                                                        PLV31380
1165  CONTINUE                                                          PLV31390
      RV=ROBJ(NR1)*RVAMB+(DIST(NX)-DIST(NX+1-NXX))/1000.+DELRV          PLV31400
1170  CONTINUE                                                          PLV31410
C                                                                       PLV31420
C  REDUCTION IN VISUAL RANGE                                            PLV31430
C                                                                       PLV31440
      REDRV=((RVAMB-RV)/RVAMB)*100.                                     PLV31450
C                                                                       PLV31460
C  DISTANCE WITHIN PLUME                                                PLV31470
C                                                                       PLV31480
      ALONG=(DIST(NX)-DIST(NXX1))/1000.                                 PLV31490
      IF(NROBJ.GT.1)GO TO 1180                                          PLV31500
      RR=0.                                                             PLV31510
      GO TO 1190                                                        PLV31520
1180  RR=ROBJ(NR1)                                                      PLV31530
1190  WRITE(6,67)ALONG,RR,RV,REDRV,YCAP,VAL,X,Y,YCAPD,VALD,CONT2        PLV31540
     1,BRATIO,XD,YD,DELUV,DELAB                                         PLV31550
1200  CONTINUE                                                          PLV31560
C                                                                       PLV31570
C  CHECK IF AT END OF LOOP ON INDEX FOR SCATTERING ANGLES FOR           PLV31580
C  THE PLUME-BASED CASE CALCULATIONS.                                   PLV31590
C                                                                       PLV31600
      IF(NT.LT.(NT2-1))GO TO 1001                                       PLV31610
1300  CONTINUE                                                          PLV31620
1301  CONTINUE                                                          PLV31630
 1000 CONTINUE                                                          PLV31640
C                                                                       PLV31650
C  PRINT THE SO4 AND NO3 FORMATION RATES FOR EACH PLUME PARCEL AT X,Z,T.PLV31660
C                                                                       PLV31670
      DO 30020 NT=1,NX2                                                 PLV31680
      WRITE(6,10)                                                       PLV31690
      XKM=DIST(NT)/1000.                                                PLV31700
      WRITE(6,30010)XKM                                                 PLV31710
30010 FORMAT(47H HISTORY OF PLUME PARCEL AT DOWNWIND DISTANCE =,F6.1,   PLV31720
     1 3H KM,//)                                                        PLV31730
      WRITE(6,30011)                                                    PLV31740
30011 FORMAT(4X,6HPARCEL,5X,5HLOCAL,5X,2X,                              PLV31750
     1 34HSO2-TO-SO4= CONVERSION RATE (%/HR),5X,2X,                     PLV31760
     1 34HNOX-TO-HNO3 CONVERSION RATE (%/HR))                           PLV31770
      WRITE(6,30012) (ALT(NZ),NZ=1,6),(ALT(NZ),NZ=1,6)                  PLV31780
30012 FORMAT(6X,3HAGE,7X,4HTIME,/,6X,4H(HR),10X,5X,6(2X,A4),5X,6(2X,A4),PLV31790
     1 /)                                                               PLV31800
      DO 30020 NX=1,NT                                                  PLV31810
      DTIME=((DIST(NT)-DIST(NX))/U)/3600.                               PLV31820
      XTIMER=TIMEHR-DTIME                                               PLV31830
30013 IF(XTIMER.GT.0.)GO TO 30014                                       PLV31840
      XTIMER=XTIMER+24.                                                 PLV31850
      GO TO 30013                                                       PLV31860
30014 CONTINUE                                                          PLV31870
      ITIMER=INT(XTIMER)*100+INT(AMOD(XTIMER,1.)*60.)                   PLV31880
      PLMAGE=(DIST(NX)/U)/3600.                                         PLV31890
      WRITE(6,30015) PLMAGE,ITIMER,(RSO2R(NX,NZ,NT),NZ=1,6),            PLV31900
     1 (RNOXR(NX,NZ,NT),NZ=1,6)                                         PLV31910
30015 FORMAT(5X,F5.1,6X,I4,2(5X,6F6.2))                                 PLV31920
30020 CONTINUE                                                          PLV31930
      DO 50018 NX=1,16                                                  PLV31940
50018 DIST(NX)=DIST(NX)/1000.                                           PLV31950
      WRITE(6,50001)                                                    PLV31960
50001 FORMAT(1H1,55X,22HPLOT FILE VERIFICATION)                         PLV31970
      IF(NC2.NE.2)GO TO 30025                                           PLV31980
C                                                                       PLV31990
C  WRITE OUT THE VISUAL IMPACT PARAMETERS FOR PLOTTING OF THE           PLV32000
C  RESULTS OF OBSERVER-BASED CALCULATIONS. PLT1 IS PERCENT REDUCTION    PLV32010
C  IN VISUAL RANGE, FOR THE 16 DOWNWIND POINTS.NN=1 FOR CLEAR           PLV32020
C  SKY BACKGROUND. NN=2 FOR WHITE BACKGROUND.  NN=3 FOR GRAY            PLV32030
C  BACKGROUND.  NN=4 FOR BLACK BACKGROUND.  PLT1 IS SET TO              PLV32040
C  ZERO FOR NN=2,3,4. PLT2 = BLUE-RED RATIO.  PLT3 = PLUME              PLV32050
C  CONTRAST AT 0.55 MICROMETER.  PLT4 = DELTA E(LAB).                   PLV32060
C                                                                       PLV32070
      WRITE(7)((PLT1(NX,NN),NX=1,16),NN=1,4)                            PLV32080
      WRITE(7)((PLT2(NX,NN),NX=1,16),NN=1,4)                            PLV32090
      WRITE(7)((PLT3(NX,NN),NX=1,16),NN=1,4)                            PLV32100
      WRITE(7)((PLT4(NX,NN),NX=1,16),NN=1,4)                            PLV32110
      WRITE(6,50002)                                                    PLV32120
50002 FORMAT(1H0,56X,19HOBSERVER-BASED DATA)                            PLV32130
      WRITE(6,50003)                                                    PLV32140
50003 FORMAT(1H0,5X,14HSKY BACKGROUND)                                  PLV32150
      WRITE(6,50019)                                                    PLV32152
50019 FORMAT(6X,'For ground level if calculations done for both plume', PLV32154
     1   ' centerline and ground level.')                               PLV32156
      WRITE(6,50004)(NN,NN=NX1,NX2)                                     PLV32160
50004 FORMAT(1H0,9X,2HNX,7X,16(5X,I2))                                  PLV32170
      WRITE(6,50005)(DIST(NX),NX=NX1,NX2)                               PLV32180
50005 FORMAT(1H0,3X,13HDISTANCE (KM),2X,16(3X,F4.0))                    PLV32190
      WRITE(6,50006)                                                    PLV32200
50006 FORMAT(1H0,1X,19HREDUCTION OF VISUAL)                             PLV32210
      WRITE(6,50007)(PLT1(NX,1),NX=NX1,NX2)                             PLV32220
50007 FORMAT(1H ,5X,9HRANGE (%),5X,16(1X,F6.3))                         PLV32230
      WRITE(6,50008)                                                    PLV32240
50008 FORMAT(1H0,2X,14HBLUE-RED RATIO)                                  PLV32250
      WRITE(6,50009)(PLT2(NX,1),NX=NX1,NX2)                             PLV32260
50009 FORMAT(1H ,19X,16(1X,F6.3))                                       PLV32270
      WRITE(6,50010)                                                    PLV32280
50010 FORMAT(1H0,2X,17HPLUME CONTRAST AT)                               PLV32290
      WRITE(6,50011)(PLT3(NX,1),NX=NX1,NX2)                             PLV32300
50011 FORMAT(1H ,4X,12H0.55 MICRONS,3X,16(1X,F6.3))                     PLV32310
      WRITE(6,50012)                                                    PLV32320
50012 FORMAT(1H0,20HPLUME PERCEPTIBILITY)                               PLV32330
      WRITE(6,50013)(PLT4(NX,1),NX=1,NX2)                               PLV32340
50013 FORMAT(1H ,2X,15HDELTA E(L*A*B*),2X,16(1X,F6.3))                  PLV32350
      WRITE(6,50014)                                                    PLV32360
50014 FORMAT(/,1H0,3X,16HWHITE BACKGROUND)                              PLV32370
      WRITE(6,50004)(NN,NN=NX1,NX2)                                     PLV32380
      WRITE(6,50005)(DIST(NX),NX=NX1,NX2)                               PLV32390
      WRITE(6,50006)                                                    PLV32400
      WRITE(6,50007)(PLT1(NX,2),NX=NX1,NX2)                             PLV32410
      WRITE(6,50008)                                                    PLV32420
      WRITE(6,50009)(PLT2(NX,2),NX=NX1,NX2)                             PLV32430
      WRITE(6,50010)                                                    PLV32440
      WRITE(6,50111)                                                    PLV32442
      WRITE(6,50211)                                                    PLV32444
      WRITE(6,50311)(PLT3(NX,2),NX=NX1,NX2)                             PLV32450
50111 FORMAT(1H ,4X,12H0.55 MICRONS)                                    PLV32451
50211 FORMAT(1H ,1X,18HEYE ACCOMODATED TO)                              PLV32452
50311 FORMAT(1H ,6X,10HBACKGROUND,3X,16(1X,F6.3))                       PLV32453
50411 FORMAT(1H ,6X,3HSKY,10X,16(1X,F6.3))                              PLV32454                    
      WRITE(6,50211)                                                    PLV32455
      WRITE(6,50411)(PLT33(NX,2),NX=NX1,NX2)                            PLV32456
      WRITE(6,50012)                                                    PLV32460
      WRITE(6,50013)(PLT4(NX,2),NX=NX1,NX2)                             PLV32470
      WRITE(6,50015)                                                    PLV32480
50015 FORMAT(/,1H0,4X,15HGRAY BACKGROUND)                               PLV32490
      WRITE(6,50004)(NN,NN=NX1,NX2)                                     PLV32500
      WRITE(6,50005)(DIST(NX),NX=NX1,NX2)                               PLV32510
      WRITE(6,50006)                                                    PLV32520
      WRITE(6,50007)(PLT1(NX,3),NX=NX1,NX2)                             PLV32530
      WRITE(6,50008)                                                    PLV32540
      WRITE(6,50009)(PLT2(NX,3),NX=NX1,NX2)                             PLV32550
      WRITE(6,50010)                                                    PLV32560
      WRITE(6,50111)                                                    PLV32562
      WRITE(6,50211)                                                    PLV32564
      WRITE(6,50311)(PLT3(NX,3),NX=NX1,NX2)                             PLV32570
      WRITE(6,50211)                                                    PLV32572
      WRITE(6,50411)(PLT33(NX,3),NX=NX1,NX2)                            PLV32574
      WRITE(6,50012)                                                    PLV32580
      WRITE(6,50013)(PLT4(NX,3),NX=NX1,NX2)                             PLV32590
      WRITE(6,50016)                                                    PLV32600
50016 FORMAT(1H1,3X,16HBLACK BACKGROUND)                                PLV32610
      WRITE(6,50004)(NN,NN=NX1,NX2)                                     PLV32620
      WRITE(6,50005)(DIST(NX),NX=NX1,NX2)                               PLV32630
      WRITE(6,50006)                                                    PLV32640
      WRITE(6,50007)(PLT1(NX,4),NX=NX1,NX2)                             PLV32650
      WRITE(6,50008)                                                    PLV32660
      WRITE(6,50009)(PLT2(NX,4),NX=NX1,NX2)                             PLV32670
      WRITE(6,50010)                                                    PLV32680
      WRITE(6,50111)                                                    PLV32682
      WRITE(6,50211)                                                    PLV32684
      WRITE(6,50311)(PLT3(NX,4),NX=NX1,NX2)                             PLV32690
      WRITE(6,50211)                                                    PLV32692
      WRITE(6,50411)(PLT33(NX,4),NX=NX1,NX2)                            PLV32694
      WRITE(6,50012)                                                    PLV32700
      WRITE(6,50013)(PLT4(NX,4),NX=NX1,NX2)                             PLV32710
      IF(NC1.NE.1)GO TO 30030                                           PLV32720
C                                                                       PLV32730
C  VISUAL IMPACT PARAMETERS FOR PLOTTING OF THE RESULTS OF              PLV32740
C  THE PLUME-BASED CALCULATIONS WITH THE DESIRED SCATTERING ANGLE       PLV32750
C  DISTANCES, AND PLUME-OBSERVER GEOMETRY AS SPECIFIED IN THE           PLV32760
C  PLUME-BASED CALCULATIONS FOR CLEAR SKY AND WHITE, GRAY, AND          PLV32770
C  BLACK BACKGROUNDS.                                                   PLV32780
C                                                                       PLV32790
30025 CONTINUE                                                          PLV32800
      WRITE(8)((PLOT1(NX,NN),NX=1,16),NN=1,4)                           PLV32810
      WRITE(8)((PLOT2(NX,NN),NX=1,16),NN=1,4)                           PLV32820
      WRITE(8)((PLOT3(NX,NN),NX=1,16),NN=1,4)                           PLV32830
      WRITE(8)((PLOT4(NX,NN),NX=1,16),NN=1,4)                           PLV32840
      IF(NC2.NE.1)WRITE(6,50001)                                        PLV32850
      WRITE(6,50017)                                                    PLV32860
50017 FORMAT(1H0,57X,16HPLUME-BASED DATA)                               PLV32870
      WRITE(6,50003)                                                    PLV32880
      WRITE(6,50004)(NN,NN=NX1,NX2)                                     PLV32890
      WRITE(6,50005)(DIST(NX),NX=NX1,NX2)                               PLV32900
      WRITE(6,50006)                                                    PLV32910
      WRITE(6,50007)(PLOT1(NX,1),NX=NX1,NX2)                            PLV32920
      WRITE(6,50008)                                                    PLV32930
      WRITE(6,50009)(PLOT2(NX,1),NX=NX1,NX2)                            PLV32940
      WRITE(6,50010)                                                    PLV32950
      WRITE(6,50011)(PLOT3(NX,1),NX=NX1,NX2)                            PLV32960
      WRITE(6,50012)                                                    PLV32970
      WRITE(6,50013)(PLOT4(NX,1),NX=NX1,NX2)                            PLV32980
      WRITE(6,50014)                                                    PLV32990
      WRITE(6,50004)(NN,NN=NX1,NX2)                                     PLV33000
      WRITE(6,50005)(DIST(NX),NX=NX1,NX2)                               PLV33010
      WRITE(6,50006)                                                    PLV33020
      WRITE(6,50007)(PLOT1(NX,2),NX=NX1,NX2)                            PLV33030
      WRITE(6,50008)                                                    PLV33040
      WRITE(6,50009)(PLOT2(NX,2),NX=NX1,NX2)                            PLV33050
      WRITE(6,50010)                                                    PLV33060
      WRITE(6,50111)                                                    PLV33062
      WRITE(6,50211)                                                    PLV33064
      WRITE(6,50311)(PLOT3(NX,2),NX=NX1,NX2)                            PLV33070
      WRITE(6,50211)                                                    PLV33072
      WRITE(6,50411)(PLOT33(NX,2),NX=NX1,NX2)                           PLV33074
      WRITE(6,50012)                                                    PLV33080
      WRITE(6,50013)(PLOT4(NX,2),NX=NX1,NX2)                            PLV33090
      WRITE(6,50015)                                                    PLV33100
      WRITE(6,50004)(NN,NN=NX1,NX2)                                     PLV33110
      WRITE(6,50005)(DIST(NX),NX=NX1,NX2)                               PLV33120
      WRITE(6,50006)                                                    PLV33130
      WRITE(6,50007)(PLOT1(NX,3),NX=NX1,NX2)                            PLV33140
      WRITE(6,50008)                                                    PLV33150
      WRITE(6,50009)(PLOT2(NX,3),NX=NX1,NX2)                            PLV33160
      WRITE(6,50010)                                                    PLV33170
      WRITE(6,50111)                                                    PLV33172
      WRITE(6,50211)                                                    PLV33174
      WRITE(6,50311)(PLOT3(NX,3),NX=NX1,NX2)                            PLV33180
      WRITE(6,50211)                                                    PLV33182
      WRITE(6,50411)(PLOT33(NX,3),NX=NX1,NX2)                           PLV33184
      WRITE(6,50012)                                                    PLV33190
      WRITE(6,50013)(PLOT4(NX,3),NX=NX1,NX2)                            PLV33200
      WRITE(6,50016)                                                    PLV33210
      WRITE(6,50004)(NN,NN=NX1,NX2)                                     PLV33220
      WRITE(6,50005)(DIST(NX),NX=NX1,NX2)                               PLV33230
      WRITE(6,50006)                                                    PLV33240
      WRITE(6,50007)(PLOT1(NX,4),NX=NX1,NX2)                            PLV33250
      WRITE(6,50008)                                                    PLV33260
      WRITE(6,50009)(PLOT2(NX,4),NX=NX1,NX2)                            PLV33270
      WRITE(6,50010)                                                    PLV33280
      WRITE(6,50111)                                                    PLV33282
      WRITE(6,50211)                                                    PLV33284
      WRITE(6,50311)(PLOT3(NX,4),NX=NX1,NX2)                            PLV33290
      WRITE(6,50211)                                                    PLV33292
      WRITE(6,50411)(PLOT33(NX,4),NX=NX1,NX2)                           PLV33294
      WRITE(6,50012)                                                    PLV33300
      WRITE(6,50013)(PLOT4(NX,4),NX=NX1,NX2)                            PLV33310
30030 CONTINUE                                                          PLV33320
      ENDFILE 7                                                         PLV33330
      ENDFILE 8                                                         PLV33340
      STOP                                                              PLV33350
      END                                                               PLV33360
