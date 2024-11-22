CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCPMX00010
      SUBROUTINE PLMAX(ZENITH,BETA,THETA,ITHETA,CPAVE,CPSAVE,CNAVE,     PMX00020
     1 CARAVE,XALONG,SPECP,SPECB)                                       PMX00030
C                                                                       PMX00040
C  PLMAX.FOR                                                            PMX00050
C                                                                       PMX00060
C  CALCULATE THE CHANGE IN SPECTRAL RADIANCE ALONG A SEGMENT OF THE     PMX00070
C  LINE OF SIGHT WITH SPECIFIED AVERAGE PLUME CONCENTRATIONS.           PMX00080
C                                                                       PMX00090
C                                                                       PMX00100
C  PLUVUE II                                                            PMX00110
C  Version 96142   Level 1.0                                            PMX00120
C                                                                       PMX00130
C  U.S. Environmental Protection Agency                                 PMX00140
C  Office of Air Quality Planning and Standards                         PMX00150
C  Research Triangle Park, North Carolina                               PMX00160
C                                                                       PMX00170
C  Modifications:                                                       PMX00180
C                                                                       PMX00190
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCPMX00200
      SAVE                                                              PMX00210
      COMMON/RADPRP/BTASO4(39),BTACOR(39),BTAPRM(39),BTAAER(39),        PMX00220
     1PAER(39,27),PPRIM(39,27),PSO4(39,27),PCOR(39,27),BTABAC(39)       PMX00230
     2,PCAR(39,27),ABSCAR(39),BTACAR(39),ABSSO4(39),ABSCOR(39),         PMX00240
     3 ABSPRM(39),GSO4(39),GCOR(39),GPRM(39),GCAR(39),GAER(39)          PMX00250
     4,GBAC(39),PBAC(39,27)                                             PMX00260
      COMMON/OPTDEP/TAT0IZ(39),TATHIZ(39),TAT0HZ(39),X1(39),            PMX00270
     1X2(39),TAUTDI(39),TAUT0D(39),XHDI(39),XH0D(39)                    PMX00280
      COMMON/ MISC/ ABSNO2(39),SOLAR(39),RAD,FORPIN,OMZ(39),OMH(39)     PMX00290
     1,NTHETA,RFR,RFI                                                   PMX00300
      COMMON/PLM/PPLM(39,27)                                            PMX00310
      COMMON /MAX/ PLUMEC                                               PMX00320
      DIMENSION SPECP(39),SPECB(39)                                     PMX00330
      DIMENSION GB(39),GP(39),X1I(39),TPTOTI(39),OMEGIP(39),TP1I(39)    PMX00340
      DIMENSION FDAVI(39),GI(39)                                        PMX00350
      XMU0=COS(RAD*ZENITH)                                              PMX00360
      XMUR=SIN(BETA*RAD)                                                PMX00370
      XMU0=COS(ZENITH*RAD)                                              PMX00380
      COSTTA=COS(THETA*RAD)                                             PMX00390
      IF(BETA.EQ.90.)GO TO 30                                           PMX00400
      CDR=(COSTTA-XMUR*XMU0)/(SQRT(1.-XMU0**2)*                         PMX00410
     2SQRT(1.-XMUR**2))                                                 PMX00420
      GO TO 31                                                          PMX00430
   30 IF(THETA.NE.ZENITH)GO TO 660                                      PMX00440
      DF=0.                                                             PMX00450
      GO TO 32                                                          PMX00460
   31 CONTINUE                                                          PMX00470
      ACDR=ABS(CDR)                                                     PMX00480
C***  SRC modification   8/6/92                                         PMX00490
C-----Note near the scattering angle/azimuth crossover (around 45deg)   PMX00500
C-----there is some angular excess - we now allow for 2%                PMX00510
      IF(ACDR.GT.1.02) GO TO 660                                        PMX00520
      IF(ACDR.GT.1.0)CDR = CDR/ACDR                                     PMX00530
      DF=ACOS(CDR)/RAD                                                  PMX00540
   32 CONTINUE                                                          PMX00550
      PRAY=0.75*(1.+COS(RAD*THETA)**2)                                  PMX00560
      DO 50 I=1,39                                                      PMX00570
         FD0=SOLAR(I)*EXP(-TAT0IZ(I)/XMU0)                              PMX00580
         FDH=SOLAR(I)*EXP(-TATHIZ(I)/XMU0)                              PMX00590
         FDAV=(FD0+FDH)*0.5                                             PMX00600
         PTOT0D=XH0D(I)*PRAY+(1.-XH0D(I))*PAER(I,ITHETA)                PMX00610
         PTOT0D=PTOT0D*OMH(I)                                           PMX00620
C******                                                                 PMX00630
C***   COMPUTE THE PLUME TRANSMISSION                                   PMX00640
C*****                                                                  PMX00650
         TAUP1=BTABAC(I)*XALONG                                         PMX00660
         TAPSO4=BTASO4(I)*CPSAVE*XALONG                                 PMX00670
         TAPNO2=ABSNO2(I)*CNAVE*XALONG                                  PMX00680
         TAPRIM=BTAPRM(I)*CPAVE*XALONG + (BTACAR(I) + ABSCAR(I))*CARAVE*PMX00690
     1   XALONG                                                         PMX00700
         TAUP2=TAPSO4+TAPNO2+TAPRIM                                     PMX00710
         TAPTOT=TAUP1+TAUP2                                             PMX00720
         IF((TAPSO4+TAPRIM).EQ.0.0)GO TO 55                             PMX00730
         XD2=TAPSO4/(TAPSO4+TAPRIM)                                     PMX00740
         XD3=(1.-XD2)*PLUMEC*BTACAR(I)/TAPRIM                           PMX00750
         GO TO 60                                                       PMX00760
55       XD2=0.0                                                        PMX00770
60       IF(TAUP2.EQ.0.0)GO TO 65                                       PMX00780
         OMEGAP=1.-(TAPNO2+ABSCAR(I)*CARAVE*XALONG)/TAUP2               PMX00790
         GO TO 70                                                       PMX00800
65       OMEGAP=1.                                                      PMX00810
70       CONTINUE                                                       PMX00820
         TP1=EXP(-TAUP1)                                                PMX00830
         TPTOT=EXP(-TAPTOT)                                             PMX00840
         PTOT0D=XH0D(I)*PRAY+(1.-XH0D(I))*PAER(I,ITHETA)                PMX00850
         PTOT0D=PTOT0D*OMH(I)                                           PMX00860
         IF(TAPTOT.EQ.0.0)GO TO 75                                      PMX00870
         X11=TAUP2/TAPTOT                                               PMX00880
         GO TO 80                                                       PMX00890
75       X11=0.0                                                        PMX00900
80       CONTINUE                                                       PMX00910
      X1I(I) = X11                                                      PMX00920
      TPTOTI(I) = TPTOT                                                 PMX00930
      OMEGIP(I) = OMEGAP                                                PMX00940
      TP1I(I) = TP1                                                     PMX00950
      FDAVI(I) = FDAV                                                   PMX00960
      DO 252 J=1,7                                                      PMX00970
      PPLM(I,J) = XD2*PSO4(I,J) + (1.-XD2)*PPRIM(I,J)                   PMX00980
     2 + XD3*PCAR(I,J)                                                  PMX00990
      PBAC(I,J) = XH0D(I)*PRAY + (1.-XH0D(I))*PAER(I,J)                 PMX01000
  252 CONTINUE                                                          PMX01010
   50 CONTINUE                                                          PMX01020
      DF = 0.                                                           PMX01030
      IPF = 0                                                           PMX01040
      CALL INTEG(DF,BETA,IPF,GI)                                        PMX01050
      DO 253 I=1,39                                                     PMX01060
  253 GB(I) = GI(I)                                                     PMX01070
      IPF = 1                                                           PMX01080
      CALL INTEG(DF,BETA,IPF,GI)                                        PMX01090
      DO 254 I=1,39                                                     PMX01100
  254 GP(I) = GI(I)                                                     PMX01110
      DO 255 I=1,39                                                     PMX01120
      SPECP(I)=SPECP(I)*TPTOTI(I)+X1I(I)*OMEGIP(I)*FORPIN*              PMX01130
     1PPLM(I,ITHETA)*FDAVI(I)*(1.-TPTOTI(I))+(1.-X1I(I))*FORPIN         PMX01140
     2*PBAC(I,ITHETA)*FDAVI(I)*(1.-TPTOTI(I))                           PMX01150
      DP = GB(I)*(1.-X1I(I))*(1.-TPTOTI(I)) + GP(I)*X1I(I)              PMX01160
     1 *OMEGIP(I)*(1.-TPTOTI(I))                                        PMX01170
         SPECP(I)=SPECP(I)+DP                                           PMX01180
      SPECB(I)=SPECB(I)*TP1I(I)+FORPIN*PBAC(I,ITHETA)*FDAVI(I)          PMX01190
     1*(1.-TP1I(I)) + GB(I)*(1.-TP1I(I))                                PMX01200
255   CONTINUE                                                          PMX01210
  660 DO 61 I=1,39                                                      PMX01220
      SPECB(I)=1.                                                       PMX01230
   61 CONTINUE                                                          PMX01240
      RETURN                                                            PMX01250
      END                                                               PMX01260
