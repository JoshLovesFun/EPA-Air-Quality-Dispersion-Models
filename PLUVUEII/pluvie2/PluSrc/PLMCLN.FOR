CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCPLC00010
      SUBROUTINE PLMCLN(ZENITH,BETA,THETA,ITHETA,PLUMEP,PLUMES,         PLC00020
     1PLUMEN,PLUMEC,SPECB,SPECP,RP,THICK)                               PLC00030
C                                                                       PLC00040
C  PLMCLN.FOR                                                           PLC00050
C                                                                       PLC00060
C  CALCULATE SPECTRAL RADIANCES OF THE CLEAR SKY VIEWED WITH LINES      PLC00070
C  OF SIGHT THROUGH THE PLUME AND BACKGROUND ATMOSPHERE.                PLC00080
C                                                                       PLC00090
C  PLUVUE II                                                            PLC00100
C  Version 92243   Level 1.0                                            PLC00110
C                                                                       PLC00120
C  U.S. Environmental Protection Agency                                 PLC00130
C  Office of Air Quality Planning and Standards                         PLC00140
C  Research Triangle Park, North Carolina                               PLC00150
C                                                                       PLC00160
C  Modifications:                                                       PLC00170
C                                                                       PLC00180
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCPLC00190
      SAVE                                                              PLC00200
      COMMON/RADPRP/BTASO4(39),BTACOR(39),BTAPRM(39),BTAAER(39),        PLC00210
     1PAER(39,27),PPRIM(39,27),PSO4(39,27),PCOR(39,27),BTABAC(39)       PLC00220
     2,PCAR(39,27),ABSCAR(39),BTACAR(39),ABSSO4(39),ABSCOR(39),         PLC00230
     3 ABSPRM(39),GSO4(39),GCOR(39),GPRM(39),GCAR(39),GAER(39)          PLC00240
     4,GBAC(39),PBAC(39,27)                                             PLC00250
      COMMON/ OPTDEP / TAT0IZ(39),TATHIZ(39),TAT0HZ(39),X1(39),         PLC00260
     1X2(39),TAUTDI(39),TAUT0D(39),XHDI(39),XH0D(39)                    PLC00270
      COMMON/ MISC/ ABSNO2(39),SOLAR(39),RAD,FORPIN,OMZ(39),OMH(39)     PLC00280
     1,NTHETA,RFR,RFI                                                   PLC00290
      COMMON/PLM/PPLM(39,27)                                            PLC00300
      DIMENSION SPECB(39),SPECP(39),GI(39)                              PLC00310
      DIMENSION OMEGIP(39),TPI2(39),DIFI(39),SPECPI(39)                 PLC00320
      DIMENSION TPI1(39)                                                PLC00330
      XMU0 = COS(RAD*ZENITH)                                            PLC00340
      XMU = SIN(RAD*BETA)                                               PLC00350
      PRAY = 0.75*(1.+COS(RAD*THETA)**2)                                PLC00360
      IPF = 0                                                           PLC00370
      XMUR=SIN(BETA*RAD)                                                PLC00380
      XMU0=COS(ZENITH*RAD)                                              PLC00390
      COSTTA=COS(THETA*RAD)                                             PLC00400
      IF(BETA.EQ.90)GO TO 40                                            PLC00410
      CDR=(COSTTA-XMUR*XMU0)/(SQRT(1.-XMU0**2)*                         PLC00420
     2SQRT(1.-XMUR**2))                                                 PLC00430
      GO TO 41                                                          PLC00440
   40 IF(THETA.NE.ZENITH)GO TO 60                                       PLC00450
      DF=0.                                                             PLC00460
      GO TO 42                                                          PLC00470
   41 CONTINUE                                                          PLC00480
      ACDR=ABS(CDR)                                                     PLC00490
C***  SRC modification  8/6/92                                          PLC00500
C-----Note near the scattering angle/azimuth crossover (around 45deg)   PLC00510
C-----there is some angular excess - we now allow for 2%                PLC00520
      IF(ACDR.GT.1.02) GO TO 60                                         PLC00530
      IF(ACDR.GT.1.0)CDR = CDR/ACDR                                     PLC00540
      DF=ACOS(CDR)/RAD                                                  PLC00550
   42 CONTINUE                                                          PLC00560
      CALL INTEG(DF,BETA,IPF,GI)                                        PLC00570
      IF(BETA.EQ.0.) GO TO 100                                          PLC00580
      DO 50 I = 1,39                                                    PLC00590
      FD0 = SOLAR(I)*EXP(-TAT0IZ(I)/XMU0)                               PLC00600
      FDH = SOLAR(I)*EXP(-TATHIZ(I)/XMU0)                               PLC00610
      FDAV = (FD0 + FDH)*0.5                                            PLC00620
      PTOTDI = X1(I)*PRAY + (1.-X1(I))*PAER(I,ITHETA)                   PLC00630
      PTOT0D = X2(I)*PRAY + (1. - X2(I))*PAER(I,ITHETA)                 PLC00640
      PTOT0D = PTOT0D*OMZ(I)                                            PLC00650
      SKY = FORPIN*PTOTDI*(1.-EXP(-TATHIZ(I)/XMU))*SOLAR(I)             PLC00660
      SURF = SKY*EXP(-TAT0HZ(I)/XMU) + FORPIN*PTOT0D*(1.-EXP(-TAT0HZ(I  PLC00670
     1)/XMU))*FDAV                                                      PLC00680
      SPECB(I) = SURF + GI(I)*(1.-EXP(-TAT0IZ(I)/XMUR))                 PLC00690
   50 CONTINUE                                                          PLC00700
      GO TO 200                                                         PLC00710
  100 CONTINUE                                                          PLC00720
C*****                                                                  PLC00730
C***  DO THE HORIZONTAL CASE                                            PLC00740
C*****                                                                  PLC00750
      DO 75 I = 1,39                                                    PLC00760
      FD0 = SOLAR(I)*EXP(-TAT0IZ(I)/XMU0)                               PLC00770
      FDH = SOLAR(I)*EXP(-TATHIZ(I)/XMU0)                               PLC00780
      FDAV = (FD0 + FDH)*0.5                                            PLC00790
      PTOTDI = PRAY*XHDI(I) + (1.-XHDI(I))*PAER(I,ITHETA)               PLC00800
      PTOT0D = XH0D(I)*PRAY + (1.-XH0D(I))*PAER(I,ITHETA)               PLC00810
      PTOT0D = PTOT0D*OMH(I)                                            PLC00820
      SKYH = FORPIN*PTOTDI*(1.-EXP(-TAUTDI(I)))*SOLAR(I)                PLC00830
      SURFH = SKYH*EXP(-TAUT0D(I)) + FORPIN*PTOT0D*(1.-EXP(-TAUT0D(I))) PLC00840
     1*FDAV                                                             PLC00850
      SPECB(I) = SURFH + GI(I)*(1.-EXP(-TAUT0D(I)-TAUTDI(I)))           PLC00860
   75 CONTINUE                                                          PLC00870
  200 CONTINUE                                                          PLC00880
      DO 250 I = 1,39                                                   PLC00890
C*****                                                                  PLC00900
C***  COMPUTE THE PLUME TRANSMISSION                                    PLC00910
C*****                                                                  PLC00920
      FD0 = SOLAR(I)*EXP(-TAT0IZ(I)/XMU0)                               PLC00930
      FDH = SOLAR(I)*EXP(-TATHIZ(I)/XMU0)                               PLC00940
      FDAV = (FD0+FDH)*.5                                               PLC00950
      TAUP1 = BTABAC(I)*RP                                              PLC00960
      TAPSO4 = BTASO4(I)*PLUMES                                         PLC00970
      TAPNO2 = ABSNO2(I)*PLUMEN                                         PLC00980
      TAPRIM = BTAPRM(I)*PLUMEP + PLUMEC*BTACAR(I)                      PLC00990
      TAUP2   = TAPSO4 + TAPNO2 + TAPRIM                                PLC01000
      IF(I.EQ.19) THICK=TAUP2                                           PLC01010
      XD2 = TAPSO4/(TAPSO4 + TAPRIM)                                    PLC01020
      OMEGAP = 1. - (TAPNO2+PLUMEC*ABSCAR(I)+PLUMES*ABSSO4(I)           PLC01030
     1 +PLUMEP*ABSPRM(I))/TAUP2                                         PLC01040
      OMEGIP(I) = OMEGAP                                                PLC01050
      XD3 = PLUMEC*BTACAR(I)/(PLUMEC*BTACAR(I)+PLUMEP*BTAPRM(I))        PLC01060
      PTOTPL = XD2*PSO4(I,ITHETA) + (1.-XD2)*PPRIM(I,ITHETA)*(1.-XD3)   PLC01070
     1 + (1.-XD2)*XD3*PCAR(I,ITHETA)                                    PLC01080
      TP1 = EXP(-TAUP1)                                                 PLC01090
      TPI1(I)=TP1                                                       PLC01100
      TP2 = EXP(-TAUP2)                                                 PLC01110
      TPI2(I) = TP2                                                     PLC01120
      PTOT0D = XH0D(I)*PRAY + (1.-XH0D(I))*PAER(I,ITHETA)               PLC01130
      PTOT0D = PTOT0D*OMH(I)                                            PLC01140
      DIFI(I) = GI(I)*(1.-TP1)                                          PLC01150
      SPECBI=(SPECB(I)-FORPIN*(1.-TP1)*PTOT0D*FDAV-DIFI(I))/TP1         PLC01160
      PLI=SPECBI*TP2+OMEGAP*FORPIN*PTOTPL*FDAV*(1.-TP2)                 PLC01170
      SPECPI(I)=PLI*TP1+FORPIN*PTOT0D*(1.-TP1)*FDAV+DIFI(I)             PLC01180
      DO 252 J=1,7                                                      PLC01190
      PPLM(I,J) = XD2*PSO4(I,J) + (1.-XD2)*PPRIM(I,J)*(1.-XD3)          PLC01200
     1 + (1.-XD2)*XD3*PCAR(I,J)                                         PLC01210
  252 CONTINUE                                                          PLC01220
  250 CONTINUE                                                          PLC01230
      IPF = 1                                                           PLC01240
      XMUR=SIN(BETA*RAD)                                                PLC01250
      XMU0=COS(ZENITH*RAD)                                              PLC01260
      COSTTA=COS(THETA*RAD)                                             PLC01270
      IF(BETA.EQ.90)GO TO 30                                            PLC01280
      CDR=(COSTTA-XMUR*XMU0)/(SQRT(1.-XMU0**2)*                         PLC01290
     2SQRT(1.-XMUR**2))                                                 PLC01300
      GO TO 31                                                          PLC01310
   30 IF(THETA.NE.ZENITH)GO TO 60                                       PLC01320
      DF=0.                                                             PLC01330
      GO TO 32                                                          PLC01340
   31 CONTINUE                                                          PLC01350
      ACDR=ABS(CDR)                                                     PLC01360
C***  SRC modification   8/6/92                                         PLC01370
C-----Note near the scattering angle/azimuth crossover (around 45deg)   PLC01380
C-----there is some angular excess - we now allow for 2%                PLC01390
      IF(ACDR.GT.1.02) GO TO 60                                         PLC01400
      IF(ACDR.GT.1.0)CDR = CDR/ACDR                                     PLC01410
      DF=ACOS(CDR)/RAD                                                  PLC01420
   32 CONTINUE                                                          PLC01430
      CALL INTEG(DF,BETA,IPF,GI)                                        PLC01440
      DO 254 I=1,39                                                     PLC01450
      SPECP(I) = SPECPI(I) + GI(I)*(1.-TPI2(I))*OMEGIP(I)*TPI1(I)       PLC01460
  254 CONTINUE                                                          PLC01470
      GO TO 63                                                          PLC01480
   60 DO 61 I=1,39                                                      PLC01490
      SPECP(I)=1.                                                       PLC01500
      SPECB(I)=1.                                                       PLC01510
   61 CONTINUE                                                          PLC01520
      WRITE(6,62)                                                       PLC01530
  62  FORMAT(/,' THIS THETA ANGLE IS IMPOSSIBLE ',/)                    PLC01540
  63  continue                                                          PLC01550
      RETURN                                                            PLC01560
      END                                                               PLC01570
C                                                                       PLC01580
