CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCBCN00010
      SUBROUTINE BACCLN(ZENITH,BETA,THETA,ITHETA,SPECB)                 BCN00020
C                                                                       BCN00030
C   BACCLN.FOR                                                          BCN00040
C                                                                       BCN00050
C   CALCULATE SPECTRAL RADIANCE OF CLEAR SKY OF GIVEN BACKGROUND        BCN00060
C   ATMOSPHERE WITH NO2 AND FINE AND COARSE AEROSOL.                    BCN00070
C                                                                       BCN00080
C  PLUVUE II                                                            BCN00090
C  Version 92243   Level 1.0                                            BCN00100
C                                                                       BCN00110
C  U.S. Environmental Protection Agency                                 BCN00120
C  Office of Air Quality Planning and Standards                         BCN00130
C  Research Triangle Park, North Carolina                               BCN00140
C                                                                       BCN00150
C  Modifications:                                                       BCN00160
C                                                                       BCN00170
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCBCN00180
      SAVE                                                              BCN00190
C                                                                       BCN00200
      COMMON/RADPRP/BTASO4(39),BTACOR(39),BTAPRM(39),BTAAER(39),        BCN00210
     1PAER(39,27),PPRIM(39,27),PSO4(39,27),PCOR(39,27),BTABAC(39)       BCN00220
     2,PCAR(39,27),ABSCAR(39),BTACAR(39),ABSSO4(39),ABSCOR(39),         BCN00230
     3 ABSPRM(39),GSO4(39),GCOR(39),GPRM(39),GCAR(39),GAER(39)          BCN00240
     4,GBAC(39),PBAC(39,27)                                             BCN00250
      COMMON/ OPTDEP / TAT0IZ(39),TATHIZ(39),TAT0HZ(39),X1(39),         BCN00260
     1X2(39),TAUTDI(39),TAUT0D(39),XHDI(39),XH0D(39)                    BCN00270
      COMMON/ MISC/ ABSNO2(39),SOLAR(39),RAD,FORPIN,OMZ(39),OMH(39)     BCN00280
     1,NTHETA,RFR,RFI                                                   BCN00290
      DIMENSION SPECB(39),GI(39)                                        BCN00300
      XMU0 = COS(RAD*ZENITH)                                            BCN00310
      XMU = SIN(RAD*BETA)                                               BCN00320
      PRAY = 0.75*(1.+COS(RAD*THETA)**2)                                BCN00330
      IF(BETA.EQ.0.) GO TO 100                                          BCN00340
      IPF = 0                                                           BCN00350
      XMUR=SIN(BETA*RAD)                                                BCN00360
      XMU0=COS(ZENITH*RAD)                                              BCN00370
      COSTTA=COS(THETA*RAD)                                             BCN00380
      IF(BETA.EQ.90.)GO TO 30                                           BCN00390
      CDR=(COSTTA-XMUR*XMU0)/(SQRT(1.-XMU0**2)*                         BCN00400
     2SQRT(1.-XMUR**2))                                                 BCN00410
      GO TO 31                                                          BCN00420
   30 IF(THETA.NE.ZENITH)GO TO 60                                       BCN00430
      DF=0.                                                             BCN00440
      GO TO 32                                                          BCN00450
   31 CONTINUE                                                          BCN00460
      ACDR=ABS(CDR)                                                     BCN00470
C***  SRC modification  8/6/92                                          BCN00480
C-----Note near the scattering angle/azimuth crossover (around 45deg)   BCN00490
C-----there is some angular excess - we now allow for 2%                BCN00500
      IF(ACDR.GT.1.02) GO TO 60                                         BCN00510
      IF(ACDR.GT.1.0)CDR = CDR/ACDR                                     BCN00520
      DF=ACOS(CDR)/RAD                                                  BCN00530
   32 CONTINUE                                                          BCN00540
      CALL INTEG(DF,BETA,IPF,GI)                                        BCN00550
      DO 50 I = 1,39                                                    BCN00560
      FD0 = SOLAR(I)*EXP(-TAT0IZ(I)/XMU0)                               BCN00570
      FDH = SOLAR(I)*EXP(-TATHIZ(I)/XMU0)                               BCN00580
      FDAV = (FD0 + FDH)*0.5                                            BCN00590
      PTOTDI = X1(I)*PRAY + (1.-X1(I))*PAER(I,ITHETA)                   BCN00600
      PTOT0D = X2(I)*PRAY + (1. - X2(I))*PAER(I,ITHETA)                 BCN00610
      PTOT0D = PTOT0D*OMZ(I)                                            BCN00620
      SKY = FORPIN*PTOTDI*(1.-EXP(-TATHIZ(I)/XMU))*SOLAR(I)             BCN00630
      SURF = SKY*EXP(-TAT0HZ(I)/XMU) + FORPIN*PTOT0D*(1.-EXP(-TAT0HZ(I  BCN00640
     1)/XMU))*FDAV                                                      BCN00650
      SPECB(I) = SURF + GI(I)*(1.-EXP(-TAT0IZ(I)/XMU))                  BCN00660
   50 CONTINUE                                                          BCN00670
      RETURN                                                            BCN00680
  100 CONTINUE                                                          BCN00690
C*****                                                                  BCN00700
C***  DO THE HORIZONTAL CASE                                            BCN00710
C*****                                                                  BCN00720
      IPF=0                                                             BCN00730
      XMUR=SIN(BETA*RAD)                                                BCN00740
      XMU0=COS(ZENITH*RAD)                                              BCN00750
      COSTTA=COS(THETA*RAD)                                             BCN00760
      CDR=(COSTTA-XMUR*XMU0)/(SQRT(1.-XMU0**2)*                         BCN00770
     2SQRT(1.-XMUR**2))                                                 BCN00780
      ACDR=ABS(CDR)                                                     BCN00790
C***  SRC modification 8/26/92                                          BCN00800
C-----Note near the scattering angle/azimuth crossover (around 45deg)   BCN00810
C-----there is some angular excess - we now allow for 2%                BCN00820
      IF(ACDR.GT.1.02) GO TO 60                                         BCN00830
      IF(ACDR.GT.1.0)CDR = CDR/ACDR                                     BCN00840
      DF=ACOS(CDR)/RAD                                                  BCN00850
      CALL INTEG(DF,BETA,IPF,GI)                                        BCN00860
      DO 75 I = 1,39                                                    BCN00870
      FD0 = SOLAR(I)*EXP(-TAT0IZ(I)/XMU0)                               BCN00880
      FDH = SOLAR(I)*EXP(-TATHIZ(I)/XMU0)                               BCN00890
      FDAV = (FD0 + FDH)*0.5                                            BCN00900
      PTOTDI = PRAY*XHDI(I) + (1.-XHDI(I))*PAER(I,ITHETA)               BCN00910
      PTOT0D = XH0D(I)*PRAY + (1.-XH0D(I))*PAER(I,ITHETA)               BCN00920
      PTOT0D = PTOT0D*OMH(I)                                            BCN00930
      SKYH = FORPIN*PTOTDI*(1.-EXP(-TAUTDI(I)))*SOLAR(I)                BCN00940
      SURFH = SKYH*EXP(-TAUT0D(I)) + FORPIN*PTOT0D*(1.-EXP(-TAUT0D(I))) BCN00950
     1*FDAV                                                             BCN00960
      SPECB(I) = SURFH + GI(I)*(1.-EXP(-TAUT0D(I)-TAUTDI(I)))           BCN00970
   75 CONTINUE                                                          BCN00980
      GO TO 63                                                          BCN00990
   60 DO 61 I=1,39                                                      BCN01000
      SPECB(I)=1.                                                       BCN01010
   61 CONTINUE                                                          BCN01020
   63 RETURN                                                            BCN01030
      END                                                               BCN01040
C                                                                       BCN01050
