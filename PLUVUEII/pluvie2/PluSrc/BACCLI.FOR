CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCBCI00010
      SUBROUTINE BACCLI(U,U0,DD,SPECBI,SPECBU)                          BCI00020
C                                                                       BCI00030
C  BACCLI.FOR                                                           BCI00040
C                                                                       BCI00050
C  CALCULATE SPECTRAL RADIANCE OF CLEAR SKY OF GIVEN BACKGROUND         BCI00060
C  ATMOSPHERE WITH NO2 AND FINE AND COARSE AEROSOL.                     BCI00070
C                                                                       BCI00080
C  PLUVUE II                                                            BCI00090
C  Version 96142   Level 1.0                                            BCI00100
C                                                                       BCI00110
C  U.S. Environmental Protection Agency                                 BCI00120
C  Office of Air Quality Planning and Standards                         BCI00130
C  Research Triangle Park, North Carolina                               BCI00140
C                                                                       BCI00150
C  Modifications:                                                       BCI00160
C                                                                       BCI00170
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCBCI00180
C                                                                       BCI00190
      SAVE                                                              BCI00200
C                                                                       BCI00210
C                                                                       BCI00220
      COMMON/RADPRP/BTASO4(39),BTACOR(39),BTAPRM(39),BTAAER(39),        BCI00230
     1PAER(39,27),PPRIM(39,27),PSO4(39,27),PCOR(39,27),BTABAC(39)       BCI00240
     2,PCAR(39,27),ABSCAR(39),BTACAR(39),ABSSO4(39),ABSCOR(39),         BCI00250
     3 ABSPRM(39),GSO4(39),GCOR(39),GPRM(39),GCAR(39),GAER(39)          BCI00260
     4,GBAC(39),PBAC(39,27)                                             BCI00270
      COMMON/ OPTDEP / TAT0IZ(39),TATHIZ(39),TAT0HZ(39),X1(39),         BCI00280
     1X2(39),TAUTDI(39),TAUT0D(39),XHDI(39),XH0D(39)                    BCI00290
      COMMON/ MISC/ ABSNO2(39),SOLAR(39),RAD,FORPIN,OMZ(39),OMH(39)     BCI00300
     1,NTHETA,RFR,RFI                                                   BCI00310
      COMMON / ISAACS / IRE                                             BCI00320
      COMMON/DIFFI/FLUXUP(39),DIFIDN(9,21,41),DIFIUP(9,21,41),FLUXDW(39)BCI00330
      DIMENSION SPECBI(39),SPECBU(39)                                   BCI00340
      REAL K                                                            BCI00350
      DATA PI/3.14159265358/                                            BCI00360
C                                                                       BCI00370
      DTR(X)=PI/180.*X                                                  BCI00380
      RTD(X)=180./PI*X                                                  BCI00390
C                                                                       BCI00400
      A1=0.3                                                            BCI00410
      IF(U.LT.0.0262) U=0.0262                                          BCI00420
      DO 50 II=1,9                                                      BCI00430
      I=5*(II-2)+4                                                      BCI00440
      IF(II.EQ.1) I=1                                                   BCI00450
      T0=TAT0IZ(I)                                                      BCI00460
      W0=OMZ(I)                                                         BCI00470
      F=SOLAR(I)/PI                                                     BCI00480
      G=GBAC(I)                                                         BCI00490
      IF(IRE.GT.1) GO TO 543                                            BCI00500
  543 CONTINUE                                                          BCI00510
      CALL HGSCT(U,U0,DD,G,W0,B0,B1,B2,P1,P2)                           BCI00520
C                                                                       BCI00530
C     DEFINE CONSTANTS                                                  BCI00540
C                                                                       BCI00550
      G1=2.*(1.-W0*(1.-B1))                                             BCI00560
      G2=2.*W0*B1                                                       BCI00570
      K=G1**2-G2**2                                                     BCI00580
      IF(K.LE.0.)GO TO 1                                                BCI00590
      K=SQRT(G1*G1-G2*G2)                                               BCI00600
      GO TO 2                                                           BCI00610
    1 K=0.                                                              BCI00620
   2  C=PI*F*W0*(B0/U0-G2*(1.-B0)-G1*B0)*U0**2/(1.-K**2*U0**2)          BCI00630
      E1=(C*(G1+1./U0)-PI*F*W0*B0)*(1./(G1-K)-A1/G2)                    BCI00640
      E2=-C+PI*F*U0*A1+A1/G2*(C*(G1+1./U0)-PI*F*W0*B0)                  BCI00650
      E3=(G1+K)*(-1./(G1-K)+A1/G2)                                      BCI00660
      E4=1.-A1/G2*(G1+K)                                                BCI00670
      B=(E1*EXP(K*T0)+E2*EXP(-T0/U0))/(E3*EXP(K*T0)+E4*EXP(-K*T0))      BCI00680
      A=(B*(G1+K)+(C*(G1+1./U0)-PI*F*W0*B0))/(K-G1)                     BCI00690
      FLUXUP(II) = A*EXP(K*T0) + B*EXP(-K*T0) + C*EXP(-T0/U0)           BCI00700
      SPECBU(II)=FLUXUP(II)/PI                                          BCI00710
C                                                                       BCI00720
C     DOWNWARD INTENSITY                                                BCI00730
C                                                                       BCI00740
      T=T0                                                              BCI00750
      W1=W0/PI*A*(1.-U*K)/(1.+U*K)*(B2+(1.-B2)*(G1-K)/G2)               BCI00760
      W2=W0/PI*B*(1.+U*K)/(1.-U*K)*(B2+(1.-B2)*(G1+K)/G2)               BCI00770
      Y=C*(G1+1./U0)-PI*F*W0*B0                                         BCI00780
      W3=(U0+U)/(U0-U)*(W0/PI*B2*C+W0/PI*(1.-B2)*Y/G2+F/4.*P2)          BCI00790
      E1=-(W1/(1.-U*K)+W2/(1.+U*K)+W3/(1.+U/U0))                        BCI00800
      SPECBI(II)=E1*EXP(-T/U)+W1*EXP(K*T)/(1.-U*K)+W2*EXP(-K*T)/(1.+U*K)BCI00810
     1 +W3*EXP(-T/U0)/(1.+U/U0)                                         BCI00820
   50 CONTINUE                                                          BCI00830
C                                                                       BCI00831
C     DOWNWARD FLUX(FLUXDW) FOR 39 WAVELENGHTS                          BCI00832
C     Will Richards and Marcelo Korc, STI, December 14, 1994            BCI00833
C                                                                       BCI00834
      DO 60 I=1,39                                                      BCI00840
      T0=TAT0IZ(I)                                                      BCI00850
      W0=OMZ(I)                                                         BCI00860
      F=SOLAR(I)/PI                                                     BCI00870
      G=GBAC(I)                                                         BCI00880
      IF(IRE.GT.1) GO TO 544                                            BCI00890
  544 CONTINUE                                                          BCI00900
      CALL HGSCT(U,U0,DD,G,W0,B0,B1,B2,P1,P2)                           BCI00910
C                                                                       BCI00920
C     DEFINE CONSTANTS                                                  BCI00930
C                                                                       BCI00940
      G1=2.*(1.-W0*(1.-B1))                                             BCI00950
      G2=2.*W0*B1                                                       BCI00960
      K=G1**2-G2**2                                                     BCI00970
      IF(K.LE.0.)GO TO 4                                                BCI00980
      K=SQRT(G1*G1-G2*G2)                                               BCI00990
      GO TO 3                                                           BCI01000
   4  K=0.                                                              BCI01010
   3  C=PI*F*W0*(B0/U0-G2*(1.-B0)-G1*B0)*U0**2/(1.-K**2*U0**2)          BCI01020
      E1=(C*(G1+1./U0)-PI*F*W0*B0)*(1./(G1-K)-A1/G2)                    BCI01030
      E2=-C+PI*F*U0*A1+A1/G2*(C*(G1+1./U0)-PI*F*W0*B0)                  BCI01040
      E3=(G1+K)*(-1./(G1-K)+A1/G2)                                      BCI01050
      E4=1.-A1/G2*(G1+K)                                                BCI01060
      B=(E1*EXP(K*T0)+E2*EXP(-T0/U0))/(E3*EXP(K*T0)+E4*EXP(-K*T0))      BCI01070
      A=(B*(G1+K)+(C*(G1+1./U0)-PI*F*W0*B0))/(K-G1)                     BCI01080
      FLUXDW(I) = (A*EXP(K*T0) + B*EXP(-K*T0) + C*EXP(-T0/U0))/A1       BCI01090
 60   CONTINUE                                                          BCI01100
      GO TO 24                                                          BCI01110
 24   RETURN                                                            BCI01120
      END                                                               BCI01130
