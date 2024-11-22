CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCSDM00010
C                                                                       SDM00020
C              SDMMET - SHORELINE DISPERSION MODEL PREPROCESSOR         SDM00030
C                         --VERSION 1.2--                               SDM00040
C                                                                       SDM00050
C   SDMMET PROGRAM ABSTRACT:                                            SDM00060
C      CREATES THE TOWER DATA INPUT FILE FOR USE IN THE SDM MODEL.      SDM00070
C      USE MONIN-OBUKHOV SIMILARITY THEORY TO CALCULATE THE SURFACE     SDM00080
C      SENSIBLE HEAT FLUX.                                              SDM00090
C                                                                       SDM00100
C   INPUT (TWO FILES):                                                  SDM00110
C                                                                       SDM00120
C   FILE SDMMET.INP                                                     SDM00130
C   CARD1 (FREE FORMAT)                                                 SDM00140
C     Z0     SURFACE ROUGHNESS LENGTH (M)                               SDM00150
C     Z1     FIRST TOWER LEVEL HEIGHT (M)                               SDM00160
C            THE 10-M HEIGHT IS RECOMMENDED FOR USE                     SDM00170
C     Z2     SECOND TOWER LEVEL HEIGHT (M)                              SDM00180
C            THE USE OF 60-M MEASUREMENTS IS RECOMMENDED                SDM00190
C     ZOW    OVERWATER TEMPERATURE HEIGHT (M)                           SDM00200
C                                                                       SDM00210
C   CARD2 (FREE FORMAT)                                                 SDM00220
C     ORD(1) POSITION OF FIRST TOWER LEVEL TEMPERATURE                  SDM00230
C     ORD(2) POSITION OF FIRST TOWER LEVEL WIND SPEED                   SDM00240
C     ORD(3) POSITION OF SECOND TOWER LEVEL TEMPERATURE                 SDM00250
C     ORD(4) POSITION OF TIBL WINDSPEED                                 SDM00260
C     ORD(5) POSITION OF STACK HEIGHT WIND SPEED                        SDM00270
C     ORD(6) POSITION OF OVERWATER TEMPERATURE                          SDM00280
C     ORD(7) POSITION OF WATER TEMPERATURE                              SDM00290
C                                                                       SDM00300
C       POSITION REFERS TO TOWER INPUT DATA (TOWER.MET)                 SDM00310
C                                                                       SDM00320
C   CARD3 (FORMAT(A80))                                                 SDM00330
C     FMT    FORMAT OF TOWER DATA (TOWER.MET) USING FORTRAN FORMAT      SDM00340
C            EDITING CHARACTERS ENCLOSED IN PARENTHESIS                 SDM00350
C                                                                       SDM00360
C                                                                       SDM00370
C   FILE TOWER.MET                                                      SDM00380
C   TOWER DATA INPUT (FORMAT AND POSITION AS DESCRIBED IN SDMMET.INP)   SDM00390
C                                                                       SDM00400
C     JUL    JULIAN DAY                                                 SDM00410
C     HR     HOUR                                                       SDM00420
C     TEMP1  TEMPERATURE AT THE FIRST MEASURING HEIGHT (K)              SDM00430
C     U1     WIND SPEED AT THE FIRST MEASURING HEIGHT (M/S)             SDM00440
C     TEMP2  TEMPERATURE AT THE SECOND MEASURING HEIGHT (M)             SDM00450
C     UL     WIND SPEED WITHIN THE TIBL (M/S)                           SDM00460
C            VALUE AT 60 M IS RECOMMENDED                               SDM00470
C     USTACK STACK HEIGHT WIND SPEED (M/S)                              SDM00480
C            IF NO MEASUREMENT AT STACK HEIGHT, THE CLOSEST HEIGHT      SDM00490
C            ON THE TOWER IS RECOMMENDED                                SDM00500
C     TEMPOW OVERWATER TEMPERATURE (K)                                  SDM00510
C            OVERLAND TEMPERATURE AT 10 M IS RECOMMENDED IF NO          SDM00520
C            OVERWATER MEASUREMENTS ARE AVAILABLE                       SDM00530
C     TEMPWT WATER TEMPERATURE (K)                                      SDM00540
C                                                                       SDM00550
C   OUTPUT (BINARY OR FREE-FORMATTED ASCII FILE NAMED TOWER.BIN)        SDM00560
C                                                                       SDM00570
C     JUL    JULIAN DAY                                                 SDM00580
C     HR     HOUR                                                       SDM00590
C     UL     WIND SPEED WITHIN THE TIBL (M/S)                           SDM00600
C     USTACK STACK HEIGHT WIND SPEED (M/S)                              SDM00610
C     PTMOL  MEAN POTENTIAL TEMPERATURE OVER LAND (K)                   SDM00620
C     PTMOW  MEAN POTENTIAL TEMPERATURE OVER WATER (K)                  SDM00630
C     DTHDZ  OVER-WATER POTENTIAL TEMPERATURE GRADIENT (K/M)            SDM00640
C     H0     SURFACE SENSIBLE HEAT FLUX (W/M^2)                         SDM00650
C                                                                       SDM00660
C                                                                       SDM00670
C      U.S. ENVIRONMENTAL PROTECTION AGENCY                             SDM00680
C      RESEARCH TRIANGLE PARK, NC                                       SDM00690
C                                                                       SDM00700
C SDMMET 1.2  REVISION HISTORY                                          SDM00710
C                                                                       SDM00720
C    CREATED    8/18/88 JEFF WINGET                                     SDM00730
C    REVISED   10/10/90 DON DICRISTOFARO                                SDM00740
C              ADD MISSING MINUS SIGN TO H0 CALCULATION                 SDM00750
C              CONVERT TEMPERATURES TO POTENTIAL TEMPERATURES           SDM00760
C              SDMMET.INP INPUT CARDS ONE AND TWO CHANGED TO FREE FORMATSDM00770
C              RICHARDSON NUMBER CALCULATION BASED ON POTENTIAL TEMP.   SDM00780
C              ASCII/BINARY OPTION FOR OUTPUT                           SDM00790
C                                                                       SDM00800
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCSDM00810
C                                                                       SDM00820
      DIMENSION VAR(7)                                                  SDM00830
      INTEGER ORD(7),HR                                                 SDM00840
C                                                                       SDM00850
      CHARACTER*80 FMT                                                  SDM00860
C                                                                       SDM00870
C     MONIN-OBUKHOV SIMILARITY FUNCTIONS FOR NORMALIZED VELOCITY        SDM00880
C     AND NORMALIZED POTENTIAL TEMPERATURE                              SDM00890
      PSIM(X)=2*ALOG(1+X/2.)+ALOG((1+X*X)/2.)-2*ATAN(X)+PI/2            SDM00900
      PSIH(X)=2*ALOG((1+X*X)/2)                                         SDM00910
C                                                                       SDM00920
      OPEN(3,FILE='SDMMET.INP',STATUS='OLD')                            SDM00930
      OPEN(4,FILE='TOWER.MET',STATUS='OLD')                             SDM00940
C                                                                       SDM00950
C   Output option in binary or ascii                                    SDM00960
C   DCD 10/15/90                                                        SDM00970
C                                                                       SDM00980
      WRITE(*,*) 'Form of output data: '                                SDM00990
      WRITE(*,*) '  1 = ASCII '                                         SDM01000
      WRITE(*,*) '  2 = BINARY'                                         SDM01010
      WRITE(*,*) 'Enter 1 or 2: '                                       SDM01020
      READ(*,*) IOPT                                                    SDM01030
      IF(IOPT .EQ. 1) THEN                                              SDM01040
         OPEN(9,FILE='TOWER.ASC',STATUS='UNKNOWN')                      SDM01050
      ELSE                                                              SDM01060
         OPEN(10,FILE='TOWER.BIN',FORM='UNFORMATTED',STATUS='UNKNOWN')  SDM01070
      ENDIF                                                             SDM01080
C                                                                       SDM01090
      PI=3.1415927                                                      SDM01100
      CK=.4                                                             SDM01110
      CSUBP=1020.                                                       SDM01120
      G=9.8                                                             SDM01130
      RHO=1.188                                                         SDM01140
C   Adiabatic lapse rate GAMMA added                                    SDM01150
C   DCD 10/10/90                                                        SDM01160
      GAMMA = 0.0098                                                    SDM01170
C                                                                       SDM01180
C  SDMMET.INP inputs for cards one and two changed to free format       SDM01190
c  DCD  10/10/90                                                        SDM01200
                                                                        SDM01210
      READ (3,*) Z0,Z1,Z2,ZOW                                           SDM01220
      READ (3,*) (ORD(K),K=1,7)                                         SDM01230
      READ (3,10020) FMT                                                SDM01240
C                                                                       SDM01250
10020 FORMAT (A80)                                                      SDM01260
C                                                                       SDM01270
      MAX = 0                                                           SDM01280
      DO 10030 I=1,7                                                    SDM01290
         IF (ORD(I).GT.MAX) MAX=ORD(I)                                  SDM01300
10030 CONTINUE                                                          SDM01310
C                                                                       SDM01320
C  Process hourly meteorological data                                   SDM01330
C                                                                       SDM01340
C                                                                       SDM01350
20    READ (4,FMT,END=99) JUL,HR,(VAR(K),K=1,MAX)                       SDM01360
C                                                                       SDM01370
C   Convert temperatures to potential temperatures                      SDM01380
C   DCD 10/10/90                                                        SDM01390
      TEMP1=VAR(ORD(1))                                                 SDM01400
      TH1 = TEMP1 + GAMMA*Z1                                            SDM01410
      U1=VAR(ORD(2))                                                    SDM01420
      TEMP2=VAR(ORD(3))                                                 SDM01430
      TH2 = TEMP2 + GAMMA*Z2                                            SDM01440
      U2=VAR(ORD(4))                                                    SDM01450
      USTACK=VAR(ORD(5))                                                SDM01460
      TEMPOW=VAR(ORD(6))                                                SDM01470
      THOW = TEMPOW + GAMMA*ZOW                                         SDM01480
      TEMPWT=VAR(ORD(7))                                                SDM01490
      THWT = TEMPWT                                                     SDM01500
C                                                                       SDM01510
C  Convert DTHDZ from dT/dz to dTH/dz                                   SDM01520
C  DCD 10/10/90                                                         SDM01530
      DTHDZ = (TEMPOW-TEMPWT)/ZOW + GAMMA                               SDM01540
C                                                                       SDM01550
C  Convert mean temperatures over land and water to                     SDM01560
C  mean potential temperatures                                          SDM01570
C  DCD 10/10/90                                                         SDM01580
      PTMOL = (TH1 + TH2)/2                                             SDM01590
      PTMOW = (THOW + THWT)/2                                           SDM01600
C  Calculate mean temperature over land for Richardson                  SDM01610
C  Number calculation                                                   SDM01620
C  DCD  10/10/90                                                        SDM01630
      TMOL = (TEMP1 + TEMP2)/2                                          SDM01640
C                                                                       SDM01650
C  Use potential temperatures to calculate Richardson Number            SDM01660
C  DCD  10/10/90                                                        SDM01670
C                                                                       SDM01680
      IF( TH2 .LT. TH1) THEN                                            SDM01690
C  Use Gradient Richardson Number if wind shear > 0                     SDM01700
C  DCD 10/10/90                                                         SDM01710
         IF(USTACK .NE. U1) THEN                                        SDM01720
            RI = (G/TMOL)*((TH2-TH1)/(Z2-Z1))/                          SDM01730
     >           (((USTACK-U1)/(Z2-Z1))**2)                             SDM01740
         ELSE                                                           SDM01750
C   Use Bulk Richardson Number if wind shear = 0                        SDM01760
            RI  = (G/TMOL)*((TH2-TH1)/(Z2-Z1))/((U1/Z1)**2)             SDM01770
         ENDIF                                                          SDM01780
         ZR=SQRT(Z1*Z2)                                                 SDM01790
         CL=ZR/RI                                                       SDM01800
         BETA0=(1-16*Z0/CL)**.25                                        SDM01810
         BETA1=(1-16*Z1/CL)**.25                                        SDM01820
         BETA2=(1-16*Z2/CL)**.25                                        SDM01830
         USTAR=CK*U1/(ALOG(Z1/Z0)-PSIM(BETA1)+PSIM(BETA0))              SDM01840
C  TSTAR calculation based on potential temperature                     SDM01850
C  DCD  10/15/90                                                        SDM01860
         TSTAR=CK*(TH2-TH1)/(ALOG(Z2/Z1)-PSIH(BETA2)+                   SDM01870
     &         PSIH(BETA1))                                             SDM01880
C   HO corrected by DCD 9/7/90                                          SDM01890
C   missing minus sign added                                            SDM01900
         HO=-RHO*CSUBP*USTAR*TSTAR                                      SDM01910
                                                                        SDM01920
      ELSE                                                              SDM01930
         HO=0.                                                          SDM01940
      ENDIF                                                             SDM01950
                                                                        SDM01960
C   CHECK VALUES OF HO                                                  SDM01970
      IF(HO .GT. 400.) WRITE(*,30) JUL,HR,HO                            SDM01980
30    FORMAT('CAUTION:  HO > 400 W/M^2 ON DAY ',I3,' HOUR ',I2,         SDM01990
     >       'HO = ',F10.3,'  CHECK TEMPERATURES.')                     SDM02000
      IF(IOPT .EQ. 2) THEN                                              SDM02010
         WRITE (10) JUL,HR,U2,USTACK,PTMOL,PTMOW,DTHDZ,HO               SDM02020
      ELSE                                                              SDM02030
         WRITE(9,29) JUL,HR,U2,USTACK,PTMOL,PTMOW,DTHDZ,HO              SDM02040
29       FORMAT(2I5,6F10.3)                                             SDM02050
      ENDIF                                                             SDM02060
      GO TO 20                                                          SDM02070
C                                                                       SDM02080
99    END                                                               SDM02090
             