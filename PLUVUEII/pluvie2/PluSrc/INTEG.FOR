CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCITG00010
      SUBROUTINE INTEG(DF,BETA,IPF,GI)                                  ITG00020
C                                                                       ITG00030
C  INTEG.FOR                                                            ITG00040
C                                                                       ITG00050
C  CALCULATES THE DIFFUSE FIELD INTEGRAL                                ITG00060
C                                                                       ITG00070
C  PLUVUE II                                                            ITG00080
C  Version 96142   Level 1.0                                            ITG00090
C                                                                       ITG00100
C  U.S. Environmental Protection Agency                                 ITG00110
C  Office of Air Quality Planning and Standards                         ITG00120
C  Research Triangle Park, North Carolina                               ITG00130
C                                                                       ITG00140
C  Modifications:                                                       ITG00150
C                                                                       ITG00160
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCITG00170
      SAVE                                                              ITG00180
C                                                                       ITG00190
      COMMON/RADPRP/BTASO4(39),BTACOR(39),BTAPRM(39),BTAAER(39),        ITG00200
     1PAER(39,27),PPRIM(39,27),PSO4(39,27),PCOR(39,27),BTABAC(39)       ITG00210
     2,PCAR(39,27),ABSCAR(39),BTACAR(39),ABSSO4(39),ABSCOR(39),         ITG00220
     3 ABSPRM(39),GSO4(39),GCOR(39),GPRM(39),GCAR(39),GAER(39)          ITG00230
     4,GBAC(39),PBAC(39,27)                                             ITG00240
      COMMON/ MISC/ ABSNO2(39),SOLAR(39),RAD,FORPIN,OMZ(39),OMH(39)     ITG00250
     1,NTHETA,RFR,RFI                                                   ITG00260
      COMMON/MIESCT/ROG,SIGMA,NLAMB,LAMB(20),JX,IT,TT(200),DUM(20),     ITG00270
     1PDUM(20,50),DUMA(20),DUMG(20)                                     ITG00280
      COMMON/DIFFI/FLUXUP(39),DIFIDN(9,21,41),DIFIUP(9,21,41),FLUXDW(39)ITG00290
      COMMON/DIFRAY/DIFRDN(9,21,41),DIFRUP(9,21,41)                     ITG00300
      COMMON/PLM/PPLM(39,27)                                            ITG00310
      DIMENSION YMU(21),XAZI(41),ERP(9),                                ITG00320
     2SUMT(39),GI(39),SPB(9),SPP(9)                                     ITG00330
     3,DFIDN(9,21,41),DFIUP(9,21,41)                                    ITG00340
     4,GII(9),PBT(9),PPT(9)                                             ITG00350
      DATA YMU/1.0,0.95,0.9,0.85,0.8,0.75,0.7,0.65,0.6,0.55,0.5,        ITG00360
     20.45,0.40,0.35,0.30,0.25,0.20,0.15,0.10,0.05,0.0/                 ITG00370
      DATA XAZI/0.,9.,18.,27.,36.,45.,54.,63.,72.,81.,90.,              ITG00380
     299.,108.,117.,126.,135.,144.,153.,162.,171.,180.,                 ITG00390
     3189.,198.,207.,216.,225.,234.,243.,252.,261.,270.,                ITG00400
     4279.,288.,297.,306.,315.,324.,333.,342.,351.,360./                ITG00410
      PI=3.14159                                                        ITG00420
      RAD=PI/180.                                                       ITG00430
      UR=SIN(BETA*RAD)                                                  ITG00440
      IF(IPF.EQ.2)GO TO 172                                             ITG00450
      IEV=0                                                             ITG00460
      DO 171 I=1,9                                                      ITG00470
      IF(IPF.EQ.1)GO TO 304                                             ITG00480
      PBT(I)=0.1*PBAC(I,1)                                              ITG00490
      GO TO 305                                                         ITG00500
  304 PPT(I)=0.1*PPLM(I,1)                                              ITG00510
  305 CONTINUE                                                          ITG00520
      SPB(I)=0.                                                         ITG00530
      SPP(I)=0.                                                         ITG00540
      DO 171 J=1,21                                                     ITG00550
      DO 171 K=1,41                                                     ITG00560
      DFIDN(I,J,K)=1.0                                                  ITG00570
      DFIUP(I,J,K)=1.0                                                  ITG00580
  171 CONTINUE                                                          ITG00590
      GO TO 302                                                         ITG00600
  172 CONTINUE                                                          ITG00610
  301 CONTINUE                                                          ITG00620
      IF(IPF.EQ.2) GO TO 70                                             ITG00630
      DO 71 I=1,9                                                       ITG00640
      DO 71 J=1,21                                                      ITG00650
      DO 71 K=1,21                                                      ITG00660
      DFIDN(I,J,K)=DIFIDN(I,J,K)                                        ITG00670
      DFIUP(I,J,K)=DIFIUP(I,J,K)                                        ITG00680
   71 CONTINUE                                                          ITG00690
      GO TO 72                                                          ITG00700
   70 DO 73 I=1,9                                                       ITG00710
      DO 73 J=1,21                                                      ITG00720
      DO 73 K=1,21                                                      ITG00730
      DFIDN(I,J,K)=DIFRDN(I,J,K)                                        ITG00740
      DFIUP(I,J,K)=DIFRUP(I,J,K)                                        ITG00750
   73 CONTINUE                                                          ITG00760
   72 CONTINUE                                                          ITG00770
      DO 91 I=1,9                                                       ITG00780
      DO 91 J=1,21                                                      ITG00790
      DO 91 K=22,41                                                     ITG00800
      DFIDN(I,J,K)=DFIDN(I,J,42-K)                                      ITG00810
      DFIUP(I,J,K)=DFIUP(I,J,42-K)                                      ITG00820
   91 CONTINUE                                                          ITG00830
  302 CONTINUE                                                          ITG00840
C                                                                       ITG00850
C     COMPUTE THE DIFFUSE RADIATION INTEGRAL                            ITG00860
C                                                                       ITG00870
      DO 30 II=1,9                                                      ITG00880
      I=5*(II-2)+4                                                      ITG00890
      IF(II.EQ.1) I=1                                                   ITG00900
      JJ=1                                                              ITG00910
      SUMT(II)=0.                                                       ITG00920
      DO 16 J=1,21                                                      ITG00930
      U=YMU(J)                                                          ITG00940
      SUM=0.                                                            ITG00950
      KK=1                                                              ITG00960
      DO 13 K=1,41                                                      ITG00970
      D=XAZI(K)                                                         ITG00980
      DI=(DF-D)*RAD                                                     ITG00990
      IF(U.EQ.1.)GO TO 81                                               ITG01000
      IF(UR.EQ.1.)GO TO 81                                              ITG01010
      CTHET=U*UR+SQRT(1.-U**2)*SQRT(1.-UR**2)*COS(DI)                   ITG01020
      GO TO 82                                                          ITG01030
   81 CTHET=U*UR                                                        ITG01040
   82 ACTHET=ABS(CTHET)                                                 ITG01050
      IF(ACTHET.GE.1.)GO TO 83                                          ITG01060
      IF(ACTHET.EQ.0.)GO TO 183                                         ITG01070
      THET=ACOS(CTHET)/RAD                                              ITG01080
      GO TO 84                                                          ITG01090
   83 THET=0.                                                           ITG01100
      GO TO 84                                                          ITG01110
  183 THET=90.                                                          ITG01120
   84 CONTINUE                                                          ITG01130
      IF(THET.LT.180.) GO TO 44                                         ITG01140
      THET=360.-THET                                                    ITG01150
   44 DO 40 ITHETA=2,7                                                  ITG01160
   40 IF(THET.LE.TT(ITHETA))GO TO 41                                    ITG01170
   41 Z=(THET-TT(ITHETA-1))/(TT(ITHETA)-TT(ITHETA-1))                   ITG01180
      IF(IPF.EQ.1)GO TO 42                                              ITG01190
      IF(IPF.EQ.2)GO TO 45                                              ITG01200
      PTHETA=10.**(ALOG10(PBAC(I,ITHETA))*Z+ALOG10(PBAC(I,ITHETA-1))    ITG01210
     2*(1.-Z))                                                          ITG01220
      IF(IEV.EQ.1)GO TO 140                                             ITG01230
      IF(PTHETA.LE.PBT(II))GO TO 141                                    ITG01240
      SPB(II)=SPB(II)+PTHETA*FLOAT(KK)*FLOAT(JJ)                        ITG01250
  141 CONTINUE                                                          ITG01260
      GO TO 143                                                         ITG01270
  140 IF(PTHETA.LE.PBT(II))GO TO 143                                    ITG01280
      PTHETA=PTHETA*(1.0-ERP(II)/SPB(II))                               ITG01290
  143 CONTINUE                                                          ITG01300
      GO TO 43                                                          ITG01310
   42 PTHETA=10.**(ALOG10(PPLM(I,ITHETA))*Z+ALOG10(PPLM(I,ITHETA-1))    ITG01320
     2*(1.-Z))                                                          ITG01330
      IF(IEV.EQ.1)GO TO 144                                             ITG01340
      IF(PTHETA.LE.PPT(II))GO TO 145                                    ITG01350
      SPP(II)=SPP(II)+PTHETA*FLOAT(KK)*FLOAT(JJ)                        ITG01360
  145 CONTINUE                                                          ITG01370
      GO TO 147                                                         ITG01380
  144 IF(PTHETA.LE.PPT(II))GO TO 147                                    ITG01390
      PTHETA=PTHETA*(1.-ERP(II)/SPP(II))                                ITG01400
  147 CONTINUE                                                          ITG01410
      GO TO 43                                                          ITG01420
   45 PTHETA=0.75*(1.+COS(RAD*THET)**2)                                 ITG01430
   43 CONTINUE                                                          ITG01440
      IF(KK.GT.1)GO TO 11                                               ITG01450
      SUM=DFIDN(II,J,K)*PTHETA + SUM                                    ITG01460
      KK=4                                                              ITG01470
      GO TO 13                                                          ITG01480
   11 IF(KK.GT.2)GO TO 12                                               ITG01490
      SUM=2.*DFIDN(II,J,K)*PTHETA + SUM                                 ITG01500
      KK=4                                                              ITG01510
      GO TO 13                                                          ITG01520
   12 SUM=4.*DFIDN(II,J,K)*PTHETA + SUM                                 ITG01530
      KK=2                                                              ITG01540
      IF(K.EQ.40) KK=1                                                  ITG01550
   13 CONTINUE                                                          ITG01560
      SUM=SUM/3.*9.*RAD                                                 ITG01570
      IF(JJ.GT.1) GO TO 14                                              ITG01580
      SUMT(II)=SUM + SUMT(II)                                           ITG01590
      JJ=4                                                              ITG01600
      GO TO 16                                                          ITG01610
   14 IF(JJ.GT.2) GO TO 15                                              ITG01620
      SUMT(II)=2.*SUM + SUMT(II)                                        ITG01630
      JJ=4                                                              ITG01640
      GO TO 16                                                          ITG01650
   15 SUMT(II)=4.*SUM + SUMT(II)                                        ITG01660
      JJ=2                                                              ITG01670
      IF(J.EQ.20) JJ=1                                                  ITG01680
   16 CONTINUE                                                          ITG01690
      JJ=1                                                              ITG01700
      DO 26 J=1,21                                                      ITG01710
      U=-YMU(J)                                                         ITG01720
      SUM=0.                                                            ITG01730
      KK=1                                                              ITG01740
      DO 23 K=1,41                                                      ITG01750
      D=XAZI(K)                                                         ITG01760
      DI=(DF-D)*RAD                                                     ITG01770
      IF(U.EQ.1.)GO TO 85                                               ITG01780
      IF(UR.EQ.1.)GO TO 85                                              ITG01790
      CTHET=U*UR+SQRT(1.-U**2)*SQRT(1.-UR**2)*COS(DI)                   ITG01800
      GO TO 86                                                          ITG01810
   85 CTHET=U*UR                                                        ITG01820
   86 ACTHET=ABS(CTHET)                                                 ITG01830
      IF(ACTHET.GE.1.)GO TO 87                                          ITG01840
      IF(ACTHET.EQ.0.)GO TO 187                                         ITG01850
      THET=ACOS(CTHET)/RAD                                              ITG01860
      GO TO 88                                                          ITG01870
   87 THET=0.                                                           ITG01880
      GO TO 88                                                          ITG01890
  187 THET=90.                                                          ITG01900
   88 CONTINUE                                                          ITG01910
      IF(THET.LT.180.) GO TO 54                                         ITG01920
      THET=360.-THET                                                    ITG01930
   54 DO 50 ITHETA=2,7                                                  ITG01940
   50 IF(THET.LE.TT(ITHETA))GO TO 51                                    ITG01950
   51 Z=(THET-TT(ITHETA-1))/(TT(ITHETA)-TT(ITHETA-1))                   ITG01960
      IF(IPF.EQ.1)GO TO 52                                              ITG01970
      IF(IPF.EQ.2)GO TO 55                                              ITG01980
      PTHETA=10.**(ALOG10(PBAC(I,ITHETA))*Z+ALOG10(PBAC(I,ITHETA-1))    ITG01990
     2*(1.-Z))                                                          ITG02000
      IF(IEV.EQ.1)GO TO 240                                             ITG02010
      IF(PTHETA.LE.PBT(II))GO TO 241                                    ITG02020
      SPB(II)=SPB(II)+PTHETA*FLOAT(KK)*FLOAT(JJ)                        ITG02030
  241 CONTINUE                                                          ITG02040
      GO TO 243                                                         ITG02050
  240 IF(PTHETA.LE.PBT(II))GO TO 243                                    ITG02060
      PTHETA=PTHETA*(1.0-ERP(II)/SPB(II))                               ITG02070
  243 CONTINUE                                                          ITG02080
      GO TO 53                                                          ITG02090
   52 PTHETA=10.**(ALOG10(PPLM(I,ITHETA))*Z+ALOG10(PPLM(I,ITHETA-1))    ITG02100
     2*(1.-Z))                                                          ITG02110
      IF(IEV.EQ.1)GO TO 244                                             ITG02120
      IF(PTHETA.LE.PPT(II))GO TO 245                                    ITG02130
      SPP(II)=SPP(II)+PTHETA*FLOAT(KK)*FLOAT(JJ)                        ITG02140
  245 CONTINUE                                                          ITG02150
      GO TO 247                                                         ITG02160
  244 IF(PTHETA.LE.PPT(II))GO TO 247                                    ITG02170
      PTHETA=PTHETA*(1.-ERP(II)/SPP(II))                                ITG02180
  247 CONTINUE                                                          ITG02190
      GO TO 53                                                          ITG02200
   55 PTHETA=0.75*(1.+COS(RAD*THET)**2)                                 ITG02210
   53 CONTINUE                                                          ITG02220
      IF(KK.GT.1)GO TO 21                                               ITG02230
      SUM=DFIUP(II,J,K)*PTHETA + SUM                                    ITG02240
      KK=4                                                              ITG02250
      GO TO 23                                                          ITG02260
   21 IF(KK.GT.2)GO TO 22                                               ITG02270
      SUM=2.*DFIUP(II,J,K)*PTHETA + SUM                                 ITG02280
      KK=4                                                              ITG02290
      GO TO 23                                                          ITG02300
   22 SUM=4.*DFIUP(II,J,K)*PTHETA + SUM                                 ITG02310
      KK=2                                                              ITG02320
      IF(K.EQ.40) KK=1                                                  ITG02330
   23 CONTINUE                                                          ITG02340
      SUM=SUM/3.*9.*RAD                                                 ITG02350
      IF(JJ.GT.1) GO TO 24                                              ITG02360
      SUMT(II)=SUM + SUMT(II)                                           ITG02370
      JJ=4                                                              ITG02380
      GO TO 26                                                          ITG02390
   24 IF(JJ.GT.2) GO TO 25                                              ITG02400
      SUMT(II)=2.*SUM + SUMT(II)                                        ITG02410
      JJ=4                                                              ITG02420
      GO TO 26                                                          ITG02430
   25 SUMT(II)=4.*SUM + SUMT(II)                                        ITG02440
      JJ=2                                                              ITG02450
      IF(J.EQ.20) JJ=1                                                  ITG02460
   26 CONTINUE                                                          ITG02470
      SUMT(II)=SUMT(II)/3.*0.05                                         ITG02480
      GII(II) = OMZ(I)*SUMT(II)/(4.*PI)                                 ITG02490
      ERP(II)=(SUMT(II) - 4.0*PI)/(RAD*0.05)                            ITG02500
   30 CONTINUE                                                          ITG02510
      IF(IEV.EQ.1)GO TO 300                                             ITG02520
      IEV=1                                                             ITG02530
      GO TO 301                                                         ITG02540
C                                                                       ITG02550
C      INTERPOLATE FOR THE DIFFUSE FIELD INTEGRAL                       ITG02560
C      AT 39 WAVELENGTHS                                                ITG02570
C                                                                       ITG02580
  300 CONTINUE                                                          ITG02590
      DO 80 I=1,39                                                      ITG02600
      J=(I+5)/5                                                         ITG02610
      Z=FLOAT((I+5)-5*J)*0.2                                            ITG02620
      GI(I)=Z*(GII(J+1)-GII(J))+GII(J)                                  ITG02630
   80 CONTINUE                                                          ITG02640
      RETURN                                                            ITG02650
      END                                                               ITG02660
