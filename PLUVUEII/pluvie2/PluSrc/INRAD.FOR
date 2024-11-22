CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCINR00010
      SUBROUTINE INRAD(ZENITH)                                          INR00020
C                                                                       INR00030
C  INRAD.FOR                                                            INR00040
C                                                                       INR00050
C  CALCULATE OPTICAL PROPERTIES OF BACKGROUND ATMOSPHERE AND FOUR       INR00060
C  AEROSOL MODES                                                        INR00070
C                                                                       INR00080
C  PLUVUE II                                                            INR00090
C  Version 96142   Level 1.0                                            INR00100
C                                                                       INR00110
C  U.S. Environmental Protection Agency                                 INR00120
C  Office of Air Quality Planning and Standards                         INR00130
C  Research Triangle Park, North Carolina                               INR00140
C                                                                       INR00150
C  Modifications:                                                       INR00160
C                                                                       INR00170
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCINR00180
      SAVE                                                              INR00190
C                                                                       INR00200
C                                                                       INR00210
      REAL LAMB                                                         INR00220
      COMMON/BCKGND/ ELEV,RVAMB,ACCAMB,AMBNO2,RH,ROVA,ROVC,ROVS,ROVP,   INR00230
     1SIGA,SIGC,SIGS,SIGP,HPBL,IREAD,CORAMB,AMBNO3,AMBSO4,INTYP         INR00240
     2,DENA,DENC,DENP,DENS,ROVCAR,SIGCAR,DENCAR,AMBCAR                  INR00250
      COMMON/MIESCT/ROG,SIGMA,NLAMB,LAMB(20),JX,IT,TT(200),DUM(20),     INR00260
     1PDUM(20,50),DUMA(20),DUMG(20)                                     INR00270
      COMMON/RADPRP/BTASO4(39),BTACOR(39),BTAPRM(39),BTAAER(39),        INR00280
     1PAER(39,27),PPRIM(39,27),PSO4(39,27),PCOR(39,27),BTABAC(39)       INR00290
     2,PCAR(39,27),ABSCAR(39),BTACAR(39),ABSSO4(39),ABSCOR(39),         INR00300
     3 ABSPRM(39),GSO4(39),GCOR(39),GPRM(39),GCAR(39),GAER(39)          INR00310
     4,GBAC(39),PBAC(39,27)                                             INR00320
      COMMON / OPTDEP / TAT0IZ(39),TATHIZ(39),TAT0HZ(39),X1(39),        INR00330
     1X2(39),TAUTDI(39),TAUT0D(39),XHDI(39),XH0D(39)                    INR00340
      COMMON/ MISC/ ABSNO2(39),SOLAR(39),RAD,FORPIN,OMZ(39),OMH(39)     INR00350
     1,NTHETA,RFR,RFI                                                   INR00360
      COMMON/INDREF/RFRSO4,RFISO4,RFRCOR,RFICOR,RFRPRM,RFIPRM           INR00370
     1,RFRCAR,RFICAR                                                    INR00380
      COMMON/DIFFI/FLUXUP(39),DIFIDN(9,21,41),DIFIUP(9,21,41),FLUXDW(39)INR00390
      COMMON/DIFRAY/DIFRDN(9,21,41),DIFRUP(9,21,41)                     INR00400
      DIMENSION YMU(21),XAZ(21),SPECBI(39),SPECBU(39)                   INR00410
      DIMENSION DUMP(41),D(2)                                           INR00420
      DIMENSION COEF(57),W(57)                                          INR00430
      DIMENSION WINK(9),REL(9)                                          INR00440
      DIMENSION OSO4(39),OCOR(39),OCAR(39),OAER(39)                     INR00450
      DIMENSION SPECRU(39),SPECRI(39)                                   INR00460
      DATA WINK / 0. , 0.03 , 0.05 , 0.1 , 0.18 , 0.3 , 0.5,0.75 , 1. / INR00470
      DATA D / 0.,0. /                                                  INR00480
      DATA YMU/1.0,0.95,0.9,0.85,0.8,0.75,0.7,0.65,0.6,0.55,0.5,        INR00490
     20.45,0.40,0.35,0.30,0.25,0.20,0.15,0.10,0.05,0.0/                 INR00500
      DATA XAZ/0.,9.,18.,27.,36.,45.,54.,63.,72.,81.,90.,99.,108.,117., INR00510
     2126.,135.,144.,153.,162.,171.,180./                               INR00520
      IREAD = 0                                                         INR00530
      TWOPI = 2.*3.141596                                               INR00540
      FORPI = 2.*TWOPI                                                  INR00550
      FORPIN = 1./FORPI                                                 INR00560
      SQRTPI = SQRT(3.141596)                                           INR00570
      HR = 9.8                                                          INR00580
      HA=3.5                                                            INR00590
      CONR = SQRT(2.*6356.*HR)                                          INR00600
      CONA = SQRT(2.*6356.*HA)                                          INR00610
      PARMA = CONA*SQRTPI*0.5                                           INR00620
      PARMR = CONR*SQRTPI*0.5                                           INR00630
      DX = SQRT(2.*6356.*HPBL)                                          INR00640
      DR = DX/CONR                                                      INR00650
      DA = DX/CONA                                                      INR00660
C***  COMPUTE THE BACKGROUND RADIATIVE PROPERTIES                       INR00670
C*****                                                                  INR00680
C*****                                                                  INR00690
C*** GENERATE THE BSCAT TO MASS RATIOS AND THE PHASE FUNCTIONS          INR00700
C*****                                                                  INR00710
C*****                                                                  INR00720
C***  ACCUMULATION MODE                                                 INR00730
C*****                                                                  INR00740
      ROG = ROVA                                                        INR00750
      SIGMA = SIGA                                                      INR00760
      RFR = RFRSO4                                                      INR00770
      RFI = RFISO4                                                      INR00780
      DO 5 I = 1,39                                                     INR00790
    5 DUMP(I) = 0.36 + 0.01*FLOAT(I)                                    INR00800
      IF(IREAD.EQ.1) GO TO 210                                          INR00810
      CALL BSIZE                                                        INR00820
      GO TO 250                                                         INR00830
  210 READ(5,1100) (    DUM(I),I=1,NLAMB)                               INR00840
      DO 220 I = 1,NLAMB                                                INR00850
  220 READ(5,1200) (PDUM(I,J),J=1,NTHETA)                               INR00860
  250 CONTINUE                                                          INR00870
 1100 FORMAT(9F7.4)                                                     INR00880
 1200 FORMAT(7E11.4)                                                    INR00890
      CALL SPLNA (NLAMB,LAMB,DUM,2,D,COEF,W)                            INR00900
      DO 12 I = 1,39                                                    INR00910
      X = DUMP(I)                                                       INR00920
      DO 13 J = 2,NLAMB                                                 INR00930
13    IF(X.LE.LAMB(J)) GO TO 14                                         INR00940
   14 Q = LAMB(J) - LAMB(J-1)                                           INR00950
      Z = (X - LAMB(J-1))/Q                                             INR00960
   12 BTASO4(I) =((Z*COEF(3*J-3)+COEF(3*J-4))*Z+COEF(3*J-5))*Z+DUM(J-1) INR00970
      DO 10 JT = 1,NTHETA                                               INR00980
      DO 15 I = 1,NLAMB                                                 INR00990
   15 DUM(I) = PDUM(I,JT)                                               INR01000
      CALL SPLNA (NLAMB,LAMB,DUM,2,D,COEF,W)                            INR01010
      DO 17 I = 1,39                                                    INR01020
      X = DUMP(I)                                                       INR01030
      DO 18 J = 2,NLAMB                                                 INR01040
18    IF(X.LE.LAMB(J)) GO TO 19                                         INR01050
   19 Q = LAMB(J) - LAMB(J-1)                                           INR01060
      Z = (X - LAMB(J-1))/Q                                             INR01070
   17 PSO4(I,JT) =((Z*COEF(3*J-3)+COEF(3*J-4))*Z+COEF(3*J-5))*Z+DUM(J-1)INR01080
   10 CONTINUE                                                          INR01090
      DO 50 I=1,NLAMB                                                   INR01100
50    DUM(I)=DUMA(I)                                                    INR01110
      CALL SPLNA(NLAMB,LAMB,DUM,2,D,COEF,W)                             INR01120
      DO 53 I=1,39                                                      INR01130
         X=DUMP(I)                                                      INR01140
         DO 52 J=2,NLAMB                                                INR01150
52          IF(X.LE.LAMB(J))GO TO 51                                    INR01160
51       Q=LAMB(J)-LAMB(J-1)                                            INR01170
         Z=(X-LAMB(J-1))/Q                                              INR01180
53       ABSSO4(I)=((Z*COEF(3*J-3)+COEF(3*J-4))*Z+COEF(3*J-5))*Z+       INR01190
     1 DUM(J-1)                                                         INR01200
      DO 150 I=1,NLAMB                                                  INR01210
  150 DUM(I)=DUMG(I)                                                    INR01220
      CALL SPLNA(NLAMB,LAMB,DUM,2,D,COEF,W)                             INR01230
      DO 151 I=1,39                                                     INR01240
      X=DUMP(I)                                                         INR01250
      DO 152 J=2,NLAMB                                                  INR01260
  152 IF(X.LE.LAMB(J)) GO TO 153                                        INR01270
  153 Q=LAMB(J)-LAMB(J-1)                                               INR01280
      Z=(X-LAMB(J-1))/Q                                                 INR01290
  151 GSO4(I)=((Z*COEF(3*J-3)+COEF(3*J-4))*Z                            INR01300
     2+COEF(3*J-5))*Z+DUM(J-1)                                          INR01310
C*****                                                                  INR01320
C*****                                                                  INR01330
C*****                                                                  INR01340
C***  COARSE PARTICLE MODE                                              INR01350
C*****                                                                  INR01360
      ROG = ROVC                                                        INR01370
      SIGMA = SIGC                                                      INR01380
      RFR=RFRCOR                                                        INR01390
      RFI=RFICOR                                                        INR01400
      IF(IREAD.EQ.1) GO TO 310                                          INR01410
      CALL BSIZE                                                        INR01420
      GO TO 350                                                         INR01430
  310 READ(5,1100) (    DUM(I),I=1,NLAMB)                               INR01440
      DO 320 I = 1,NLAMB                                                INR01450
  320 READ(5,1200) (PDUM(I,J),J=1,NTHETA)                               INR01460
  350 CONTINUE                                                          INR01470
      CALL SPLNA (NLAMB,LAMB,DUM,2,D,COEF,W)                            INR01480
      DO 22 I = 1,39                                                    INR01490
      X = DUMP(I)                                                       INR01500
      DO 23 J = 2,NLAMB                                                 INR01510
23    IF(X.LE.LAMB(J)) GO TO 24                                         INR01520
   24 Q = LAMB(J) - LAMB(J-1)                                           INR01530
      Z = (X - LAMB(J-1))/Q                                             INR01540
   22 BTACOR(I) =((Z*COEF(3*J-3)+COEF(3*J-4))*Z+COEF(3*J-5))*Z+DUM(J-1) INR01550
      DO 20 JT = 1,NTHETA                                               INR01560
      DO 25 I = 1,NLAMB                                                 INR01570
25    DUM(I) = PDUM(I,JT)                                               INR01580
      CALL SPLNA (NLAMB,LAMB,DUM,2,D,COEF,W)                            INR01590
      DO 27 I = 1,39                                                    INR01600
      X = DUMP(I)                                                       INR01610
      DO 28 J = 2,NLAMB                                                 INR01620
28    IF(X.LE.LAMB(J)) GO TO 29                                         INR01630
   29 Q = LAMB(J) - LAMB(J-1)                                           INR01640
      Z = (X - LAMB(J-1))/Q                                             INR01650
   27 PCOR(I,JT) =((Z*COEF(3*J-3)+COEF(3*J-4))*Z+COEF(3*J-5))*Z+DUM(J-1)INR01660
   20 CONTINUE                                                          INR01670
      DO 60 I=1,NLAMB                                                   INR01680
60    DUM(I)=DUMA(I)                                                    INR01690
      CALL SPLNA(NLAMB,LAMB,DUM,2,D,COEF,W)                             INR01700
      DO 63 I=1,39                                                      INR01710
         X=DUMP(I)                                                      INR01720
         DO 62 J=2,NLAMB                                                INR01730
62       IF(X.LE.LAMB(J))GO TO 61                                       INR01740
61       Q=LAMB(J)-LAMB(J-1)                                            INR01750
         Z=(X-LAMB(J-1))/Q                                              INR01760
63    ABSCOR(I)=((Z*COEF(3*J-3)+COEF(3*J-4))*Z+COEF(3*J-5))*Z+DUM(J-1)  INR01770
      DO 160 I=1,NLAMB                                                  INR01780
 160  DUM(I)=DUMG(I)                                                    INR01790
      CALL SPLNA(NLAMB,LAMB,DUM,2,D,COEF,W)                             INR01800
      DO 161 I=1,39                                                     INR01810
      X=DUMP(I)                                                         INR01820
      DO 162 J=2,NLAMB                                                  INR01830
  162 IF(X.LE.LAMB(J)) GO TO 163                                        INR01840
  163 Q=LAMB(J)-LAMB(J-1)                                               INR01850
      Z=(X-LAMB(J-1))/Q                                                 INR01860
  161 GCOR(I)=((Z*COEF(3*J-3)+COEF(3*J-4))*Z                            INR01870
     2+COEF(3*J-5))*Z+DUM(J-1)                                          INR01880
C*****                                                                  INR01890
C*** COMPUTE THE PRIMARY PARTICULATE PROPERTIES                         INR01900
C*****                                                                  INR01910
      ROG = ROVP                                                        INR01920
      SIGMA = SIGP                                                      INR01930
      RFR=RFRPRM                                                        INR01940
      RFI=RFIPRM                                                        INR01950
      IF(IREAD.EQ.1) GO TO 410                                          INR01960
      CALL BSIZE                                                        INR01970
      GO TO 450                                                         INR01980
  410 READ(5,1100) (DUM(I),I=1,NLAMB)                                   INR01990
      DO 420 I = 1,NLAMB                                                INR02000
  420 READ(5,1200) (PDUM(I,J),J=1,NTHETA)                               INR02010
  450 CONTINUE                                                          INR02020
      CALL SPLNA (NLAMB,LAMB,DUM,2,D,COEF,W)                            INR02030
      DO 32 I = 1,39                                                    INR02040
      X = DUMP(I)                                                       INR02050
      DO 33 J = 2,NLAMB                                                 INR02060
   33 IF(X.LE.LAMB(J)) GO TO 34                                         INR02070
   34 Q = LAMB(J) - LAMB(J-1)                                           INR02080
      Z = (X - LAMB(J-1))/Q                                             INR02090
   32 BTAPRM(I) =((Z*COEF(3*J-3)+COEF(3*J-4))*Z+COEF(3*J-5))*Z+DUM(J-1) INR02100
      DO 30 JT = 1,NTHETA                                               INR02110
      DO 35 I = 1,NLAMB                                                 INR02120
   35 DUM(I) = PDUM(I,JT)                                               INR02130
      CALL SPLNA (NLAMB,LAMB,DUM,2,D,COEF,W)                            INR02140
      DO 37 I = 1,39                                                    INR02150
      X = DUMP(I)                                                       INR02160
      DO 38 J = 2,NLAMB                                                 INR02170
   38 IF(X.LE.LAMB(J)) GO TO 39                                         INR02180
   39 Q = LAMB(J) - LAMB(J-1)                                           INR02190
      Z = (X - LAMB(J-1))/Q                                             INR02200
   37 PPRIM(I,JT)=((Z*COEF(3*J-3)+COEF(3*J-4))*Z+COEF(3*J-5))*Z+DUM(J-1)INR02210
   30 CONTINUE                                                          INR02220
      DO 70 I=1,NLAMB                                                   INR02230
70       DUM(I)=DUMA(I)                                                 INR02240
      CALL SPLNA(NLAMB,LAMB,DUM,2,D,COEF,W)                             INR02250
      DO 73 I=1,39                                                      INR02260
         X=DUMP(I)                                                      INR02270
         DO 72 J=2,NLAMB                                                INR02280
72       IF(X.LE.LAMB(J))GO TO 71                                       INR02290
71       Q=LAMB(J)-LAMB(J-1)                                            INR02300
         Z=(X-LAMB(J-1))/Q                                              INR02310
73       ABSPRM(I)=((Z*COEF(3*J-3)+COEF(3*J-4))*Z+COEF(3*J-5))*Z+       INR02320
     1   DUM(J-1)                                                       INR02330
      DO 170 I=1,NLAMB                                                  INR02340
  170 DUM(I)=DUMG(I)                                                    INR02350
      CALL SPLNA(NLAMB,LAMB,DUM,2,D,COEF,W)                             INR02360
      DO 171 I=1,39                                                     INR02370
      X=DUMP(I)                                                         INR02380
      DO 172 J=2,NLAMB                                                  INR02390
  172 IF(X.LE.LAMB(J)) GO TO 173                                        INR02400
  173 Q=LAMB(J)-LAMB(J-1)                                               INR02410
      Z=(X-LAMB(J-1))/Q                                                 INR02420
  171 GPRM(I)=((Z*COEF(3*J-3)+COEF(3*J-4))*Z                            INR02430
     2+COEF(3*J-5))*Z+DUM(J-1)                                          INR02440
C                                                                       INR02450
C     COMPUTE THE CARBON AEROSOL PROPERTIES.  FIRST                     INR02460
C     DO THE SCATTERING COEFFICIENT, THEN PHASE FUNCTION FOR            INR02470
C     CARBON SCATTERING, THEN THE ABSORPTION                            INR02480
C     COEFFICIENT FOR CARBON.                                           INR02490
C                                                                       INR02500
      ROG=ROVCAR                                                        INR02510
      SIGMA=SIGCAR                                                      INR02520
      RFR = RFRCAR                                                      INR02530
      RFI = RFICAR                                                      INR02540
      CALL BSIZE                                                        INR02550
      CALL SPLNA(NLAMB,LAMB, DUM, 2,D, COEF,W)                          INR02560
      DO 612 I=1,39                                                     INR02570
         X=DUMP(I)                                                      INR02580
         DO 613 J=2,NLAMB                                               INR02590
613         IF(X.LE.LAMB(J))GO TO 614                                   INR02600
614      Q=LAMB(J)-LAMB(J-1)                                            INR02610
         Z=(X-LAMB(J-1))/Q                                              INR02620
612      BTACAR(I)=((Z*COEF(3*J-3)+COEF(3*J-4))*Z+COEF(3*J-5))*Z+DUM(J-1INR02630
     1   )                                                              INR02640
      DO 610 JT=1,NTHETA                                                INR02650
         DO 615 I=1,NLAMB                                               INR02660
615         DUM(I)=PDUM(I,JT)                                           INR02670
         CALL SPLNA(NLAMB,LAMB,DUM,2,D,COEF,W)                          INR02680
         DO 617 I=1,39                                                  INR02690
            X=DUMP(I)                                                   INR02700
            DO 618 J=2,NLAMB                                            INR02710
618            IF(X.LE.LAMB(J))GO TO 619                                INR02720
619         Q=LAMB(J)-LAMB(J-1)                                         INR02730
            Z=(X-LAMB(J-1))/Q                                           INR02740
617         PCAR(I,JT)=((Z*COEF(3*J-3)+COEF(3*J-4))*Z+COEF(3*J-5))*Z+DUMINR02750
     1      (J-1)                                                       INR02760
610         CONTINUE                                                    INR02770
C                                                                       INR02780
C     NEXT DO ABSORPTION DUE TO CARBON                                  INR02790
C     DUMA= ABS/VTOT, DUMA F(WAVELENGTH ONLY)                           INR02800
C                                                                       INR02810
      CALL SPLNA(NLAMB,LAMB,DUMA,2,D,COEF,W)                            INR02820
      DO 620 I=1,39                                                     INR02830
         X=DUMP(I)                                                      INR02840
         DO 625 J=2,NLAMB                                               INR02850
625         IF(X.LE.LAMB(J))GO TO 624                                   INR02860
624      Q=LAMB(J)-LAMB(J-1)                                            INR02870
         Z=(X-LAMB(J-1))/Q                                              INR02880
620      ABSCAR(I)=((Z*COEF(3*J-3)+COEF(3*J-4))*Z+COEF(3*J-5))*Z+DUMA(J-INR02890
     2   1)                                                             INR02900
      DO 180 I=1,NLAMB                                                  INR02910
  180 DUM(I)=DUMG(I)                                                    INR02920
      CALL SPLNA(NLAMB,LAMB,DUM,2,D,COEF,W)                             INR02930
      DO 181 I=1,39                                                     INR02940
      X=DUMP(I)                                                         INR02950
      DO 182 J=2,NLAMB                                                  INR02960
  182 IF(X.LE.LAMB(J)) GO TO 183                                        INR02970
  183 Q=LAMB(J)-LAMB(J-1)                                               INR02980
      Z=(X-LAMB(J-1))/Q                                                 INR02990
  181 GCAR(I)=((Z*COEF(3*J-3)+COEF(3*J-4))*Z                            INR03000
     2+COEF(3*J-5))*Z+DUM(J-1)                                          INR03010
C*****                                                                  INR03020
C*** ADJUST FOR THE RELATIVE HUMIDITY                                   INR03030
C*****                                                                  INR03040
      DO 41 I = 1,9                                                     INR03050
   41 REL(I) = 0.1*FLOAT(I-1)                                           INR03060
      CALL SPLNA(9,REL,WINK,2,D,COEF,W)                                 INR03070
      DO 42 I = 2,9                                                     INR03080
   42 IF(RH.LE.REL(I)) GO TO 43                                         INR03090
   43 Z = (RH-REL(I-1))*10.                                             INR03100
      FH = ((Z*COEF(3*I-3)+COEF(3*I-4))*Z+COEF(3*I-5))*Z+WINK(I-1)      INR03110
      FACT =  1. + (0.85*FH)/(1.-RH) /DENA                              INR03120
      DO 40 I = 1,39                                                    INR03130
   40 BTASO4(I) = BTASO4(I)*FACT                                        INR03140
C*****                                                                  INR03150
C***  DETERMINE THE TYPE OF INPUT AND COMPUTE THE BACKGROUND RADIATIVE  INR03160
C***  PROPERTIES.  INTYP = 1 MEANS THE NO3,SO4,COARSE MODE LEVEELS ARE  INR03170
C***  SPECIFIED IN UGM/M3.  INTYPE = 2 MEANS THE VISUAL RANGE AND       INR03180
C***  THE ACCUMULATION MODE ARE SPECIFIED IN KM-1 AND UGM/M3            INR03190
C*****                                                                  INR03200
      DENSO4=DENA                                                       INR03210
      WAVE = 0.55                                                       INR03220
      WV2 = (1./WAVE)**2                                                INR03230
      WV4 = WV2*WV2                                                     INR03240
      TAR0IZ = 0.008569*WV4*(1. +0.0113*WV2+0.00013*WV4)*EXP(-ELEV/HR)  INR03250
      RAY = TAR0IZ/HR                                                   INR03260
      IF(INTYP.EQ.2) GO TO 500                                          INR03270
      IF(INTYP.NE.1) RETURN                                             INR03280
      DO 460 I = 1,39                                                   INR03290
      X8 = BTASO4(I)*(AMBSO4/DENS + AMBNO3/DENS)                        INR03300
      X9 = BTACOR(I)*CORAMB/DENC                                        INR03310
      X10 = BTACAR(I)*AMBCAR/DENCAR                                     INR03320
      BTAAER(I) = (X8+X9+X10)*1.E-03                                    INR03330
      IF(BTAAER(I).NE.0.) GO TO 458                                     INR03340
      X8=0.                                                             INR03350
      X9=0.                                                             INR03360
      X10=0.                                                            INR03370
      GO TO 459                                                         INR03380
 458  CONTINUE                                                          INR03390
 459  CONTINUE                                                          INR03400
      OSO4(I)=(BTASO4(I)-ABSSO4(I))/BTASO4(I)                           INR03410
      OCOR(I)=(BTACOR(I)-ABSCOR(I))/BTACOR(I)                           INR03420
      OCAR(I)=(BTACAR(I)-ABSCAR(I))/BTACAR(I)                           INR03430
      SIG8=OSO4(I)*X8                                                   INR03440
      SIG9=OCOR(I)*X9                                                   INR03450
      SIG10=OCAR(I)*X10                                                 INR03460
      SIGAR=(X8+X9+X10)*1.E-03                                          INR03470
      OAER(I)=SIGAR/BTAAER(I)                                           INR03480
      GAER(I) = (GSO4(I)*SIG8+GCOR(I)*SIG9+GCAR(I)*SIG10)*1.E-03        INR03490
     2 /SIGAR                                                           INR03500
      DO 460 J = 1,NTHETA                                               INR03510
 460  PAER(I,J)=(PSO4(I,J)*SIG8+PCOR(I,J)*SIG9+PCAR(I,J)*SIG10)*1.E-03  INR03520
     1 /SIGAR                                                           INR03530
      SIGKOS = RAY + BTAAER(19) + ABSNO2(19)*AMBNO2                     INR03540
      RVAMB = -ALOG(.02)/SIGKOS                                         INR03550
      GO TO 600                                                         INR03560
  500 CONTINUE                                                          INR03570
C*****                                                                  INR03580
C***  DETERMINE THE MASS OF THE ACCUMULATION MODE                       INR03590
C*****                                                                  INR03600
      SIGKOS = -ALOG(.02)/RVAMB                                         INR03610
      SIGINT = BTACOR(19)*CORAMB*1.E-03/DENC + RAY + ABSNO2(19)*AMBNO2  INR03620
     1 + BTACAR(19)*AMBCAR*1.E-03/DENCAR                                INR03630
      IF(SIGKOS-SIGINT) 100,100,120                                     INR03640
  100 AMBSO4 = 0.                                                       INR03650
      CORAMB = (SIGKOS-RAY-ABSNO2(19)*AMBNO2-AMBCAR*BTACAR(19)          INR03660
     1 *1.E-03/DENCAR)/(BTACOR(19)*1.E-03/DENC)                         INR03670
      IF(CORAMB.LT.0.) PRINT 2000                                       INR03680
 2000 FORMAT(1H ,44HTROUBLE WITH THE CONCENTRATION SPECIFICATION)       INR03690
      GO TO 1150                                                        INR03700
  120 AMBSO4 = (SIGKOS - SIGINT)/(BTASO4(19)*1.E-03/DENS)               INR03710
 1150 CONTINUE                                                          INR03720
      DO 1160 I = 1,39                                                  INR03730
      X8 = BTASO4(I)*(AMBSO4/DENS+AMBNO3/DENS)                          INR03740
      X9 = BTACOR(I)*CORAMB/DENC                                        INR03750
      X10 = BTACAR(I)*AMBCAR/DENCAR                                     INR03760
      BTAAER(I) =(X8 + X9 + X10)*1.E-03                                 INR03770
      OSO4(I)=(BTASO4(I)-ABSSO4(I))/BTASO4(I)                           INR03780
      OCOR(I)=(BTACOR(I)-ABSCOR(I))/BTACOR(I)                           INR03790
      OCAR(I)=(BTACAR(I)-ABSCAR(I))/BTACAR(I)                           INR03800
      SIG8=OSO4(I)*X8                                                   INR03810
      SIG9=OCOR(I)*X9                                                   INR03820
      SIG10=OCAR(I)*X10                                                 INR03830
      SIGAR=(SIG8+SIG9+SIG10)*1.E-03                                    INR03840
      OAER(I)=SIGAR/BTAAER(I)                                           INR03850
      DO 2160 J = 1,NTHETA                                              INR03860
 2160 PAER(I,J) = (PSO4(I,J)*SIG8+PCOR(I,J)*SIG9+PCAR(I,J)*SIG10)*1.E-03INR03870
     1 /SIGAR                                                           INR03880
      GAER(I) = (GSO4(I)*SIG8+GCOR(I)*SIG9+GCAR(I)*SIG10)*1.E-03        INR03890
     2 /SIGAR                                                           INR03900
 2345 FORMAT(2X,E13.4)                                                  INR03910
 1160 CONTINUE                                                          INR03920
      AMBNO3 = 0.                                                       INR03930
  600 CONTINUE                                                          INR03940
C*****                                                                  INR03950
C***  COMPUTE THE VERTICAL AND HORIZONTAL OPTICAL DEPTHS                INR03960
C*****                                                                  INR03970
      DO 200 I = 1,39                                                   INR03980
      WAVE = 0.36 + FLOAT(I)*0.01                                       INR03990
      WV2 = WAVE**(-2)                                                  INR04000
      WV4 = WV2*WV2                                                     INR04010
      TAR0IZ = 0.008569*WV4*(1. + 0.0113*WV2 + 0.00013*WV4)*EXP(-ELEV/  INR04020
     1HR)                                                               INR04030
      TAA0IZ = BTAAER(I)*HA                                             INR04040
      TAT0IZ(I) = TAR0IZ + TAA0IZ                                       INR04050
      TARHIZ = TAR0IZ*EXP(-HPBL/HR)                                     INR04060
      TAR0HZ = TAR0IZ - TARHIZ                                          INR04070
      TAAHIZ = TAA0IZ*EXP(-HPBL/HA)                                     INR04080
      TATHIZ(I) = TARHIZ + TAAHIZ                                       INR04090
      TAT0HZ(I) = TAT0IZ(I) - TATHIZ(I)                                 INR04100
      BETA0R = TAR0IZ/HR                                                INR04110
      BTABAC(I) = BTAAER(I) + BETA0R + ABSNO2(I)*AMBNO2                 INR04120
      TAA0HZ=TAA0IZ-TAAHIZ                                              INR04130
      X1(I)=TARHIZ/(TAAHIZ*OAER(I)+TARHIZ)                              INR04140
      X2(I)=TAR0HZ/(TAA0HZ*OAER(I)+TAR0HZ)                              INR04150
C     COMPUTE THE HORIZONTAL PATH DEPTH                                 INR04160
      TAUR0I = BETA0R*PARMR                                             INR04170
      TAUA0I = BTAAER(I)*PARMA                                          INR04180
      TAUR0D = BETA0R*ERF(DR)*PARMR                                     INR04190
      TAUA0D = BTAAER(I)*ERF(DA)*PARMA                                  INR04200
      TAURDI = TAUR0I - TAUR0D                                          INR04210
      TAUADI = TAUA0I - TAUA0D                                          INR04220
      TAUT0D(I) = TAUR0D + TAUA0D                                       INR04230
      TAUTDI(I) = TAURDI + TAUADI                                       INR04240
      XHDI(I) = TAURDI/(TAURDI + TAUADI*OAER(I))                        INR04250
      XH0D(I) = TAUR0D/(TAUR0D + TAUA0D*OAER(I))                        INR04260
      GBAC(I) = GAER(I)*TAA0IZ/TAT0IZ(I)                                INR04270
      DO 510 J=1,NTHETA                                                 INR04280
      PRAY = 0.75*(1.+COS(RAD*TT(J))**2)                                INR04290
      PBAC(I,J)=(PAER(I,J)*TAA0IZ+PRAY*TAR0IZ)/TAT0IZ(I)                INR04300
  510 CONTINUE                                                          INR04310
 2346 FORMAT(2X,E13.4)                                                  INR04320
C                                                                       INR04330
C     *************************************                             INR04340
C                                                                       INR04350
C***  ADJUST THE OPTICAL DEPTH FOR NO2                                  INR04360
      TAA0HZ = TAA0IZ - TAAHIZ                                          INR04370
      TAT0IZ(I) = TAR0IZ + TAA0IZ + ABSNO2(I)*AMBNO2*HPBL               INR04380
      TAT0HZ(I) = TAT0IZ(I) - TATHIZ(I)                                 INR04390
      TAUT0D(I) = TAUR0D + TAUA0D + ABSNO2(I)*AMBNO2*DX                 INR04400
      OMZ(I) = (TAR0HZ + TAA0HZ*OAER(I))/TAT0HZ(I)                      INR04410
            IF(OMZ(I).GT.0.999) OMZ(I) = 0.999                          INR04420
      OMH(I) = (TAUR0D + TAUA0D*OAER(I))/TAUT0D(I)                      INR04430
C*****                                                                  INR04440
C***  CHANGE THE SCATTERING COEFICIENTS FROM BSCAT/VOL TO BSCAT/MASS    INR04450
C***                                                                    INR04460
      BTASO4(I) = BTASO4(I)*1.E-03/DENSO4                               INR04470
      BTAPRM(I) = BTAPRM(I)*1.E-03/DENP                                 INR04480
      BTACOR(I) = BTACOR(I)*1.E-03/DENC                                 INR04490
C                                                                       INR04500
C     CHANGE COEFFICIENTS FOR CARBON FROM BSCAT/VOL TO BSCAT/MASS.      INR04510
C     CHANGE ABSORPTION COEFFICIENT FOR CARBON TO ABS/MASS.             INR04520
C     CHANGE ABSORPTION COEFFICIENTS FOR SULFATE, COARSE                INR04530
C     MODE AEROSOL, AND PRIMARY PARTICULATE FROM ABS/VOL                INR04540
C     TO ABS/MASS.                                                      INR04550
C                                                                       INR04560
      BTACAR(I)=BTACAR(I)*1.E-03/DENCAR                                 INR04570
      ABSCAR(I)=ABSCAR(I)*1.E-03/DENCAR                                 INR04580
      ABSSO4(I)=ABSSO4(I)*1.E-03/DENSO4                                 INR04590
      ABSCOR(I)=ABSCOR(I)*1.E-03/DENC                                   INR04600
      ABSPRM(I)=ABSPRM(I)*1.E-03/DENP                                   INR04610
C                                                                       INR04620
C   *********************************************                       INR04630
C                                                                       INR04640
  200 CONTINUE                                                          INR04650
  958 FORMAT(/,3X,'TEST BEFORE CALL BACCLI AND RAYREI',/)               INR04660
      PI=3.14159                                                        INR04670
      RAD=PI/180.                                                       INR04680
C                                                                       INR04690
C     COMPUTE DOWNWARD AND UPWARD INTENSITIES                           INR04700
C                                                                       INR04710
      U0=COS(ZENITH*RAD)                                                INR04720
      DO 55 J=1,21                                                      INR04730
      U=YMU(J)                                                          INR04740
      DO 55 K=1,21                                                      INR04750
      DD=XAZ(K)                                                         INR04760
      CALL BACCLI(U,U0,DD,SPECBI,SPECBU)                                INR04770
      CALL RAYREI(U,U0,DD,SPECRI,SPECRU)                                INR04780
      DO 55 I=1,9                                                       INR04790
      DIFIUP(I,J,K)=SPECBU(I)                                           INR04800
      DIFIDN(I,J,K)=SPECBI(I)                                           INR04810
      DIFRUP(I,J,K)=SPECRU(I)                                           INR04820
      DIFRDN(I,J,K)=SPECRI(I)                                           INR04830
   55 CONTINUE                                                          INR04840
      RETURN                                                            INR04850
      END                                                               INR04860
