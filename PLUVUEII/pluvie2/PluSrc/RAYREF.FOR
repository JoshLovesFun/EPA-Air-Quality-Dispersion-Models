CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCRAY00010
      SUBROUTINE RAYREF(ZENITH,BETA,THETA,SPECR)                        RAY00020
C                                                                       RAY00030
C  RAYREF.FOR                                                           RAY00040
C                                                                       RAY00050
C  CALCULATE SPECTRAL RADIANCE OF CLEAR SKY IN A RAYLEIGH ATMOSPHERE.   RAY00060
C                                                                       RAY00070
C  PLUVUE II                                                            RAY00080
C  Version 92243   Level 1.0                                            RAY00090
C                                                                       RAY00100
C  U.S. Environmental Protection Agency                                 RAY00110
C  Office of Air Quality Planning and Standards                         RAY00120
C  Research Triangle Park, North Carolina                               RAY00130
C                                                                       RAY00140
C  Modifications:                                                       RAY00150
C                                                                       RAY00160
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCRAY00170
      SAVE                                                              RAY00180
C                                                                       RAY00190
      COMMON/BCKGND/ ELEV,RVAMB,ACCAMB,AMBNO2,RH,ROVA,ROVC,ROVS,ROVP,   RAY00200
     1SIGA,SIGC,SIGS,SIGP,HPBL,IREAD,CORAMB,AMBNO3,AMBSO4,INTYP         RAY00210
     2,DENA,DENC,DENP,DENS,ROVCAR,SIGCAR,DENCAR,AMBCAR                  RAY00220
      COMMON / MISC/ ABSNO2(39),SOLAR(39),RAD,FORPIN,OMZ(39),OMH(39)    RAY00230
     1,NTHETA,RFR,RFI                                                   RAY00240
      DIMENSION SPECR(39)                                               RAY00250
      DIMENSION GI(39)                                                  RAY00260
      XMU0 = COS(RAD*ZENITH)                                            RAY00270
      XMU = SIN(RAD*BETA)                                               RAY00280
      PRAY = 0.75*(1.+COS(RAD*THETA)**2)                                RAY00290
      HR = 9.8                                                          RAY00300
      CONR = SQRT(2.*6356.*HR)                                          RAY00310
      SQRTPI = SQRT(3.1415962)                                          RAY00320
      PARMR = CONR*SQRTPI*0.5                                           RAY00330
      DX = SQRT(2.*6356.*HPBL)                                          RAY00340
      DRR = DX/CONR                                                     RAY00350
      IPF=2                                                             RAY00360
      XMUR=SIN(BETA*RAD)                                                RAY00370
      XMU0=COS(ZENITH*RAD)                                              RAY00380
      COSTTA=COS(THETA*RAD)                                             RAY00390
      IF(BETA.EQ.90.)GO TO 30                                           RAY00400
      CDR=(COSTTA-XMUR*XMU0)/(SQRT(1.-XMU0**2)*                         RAY00410
     2SQRT(1.-XMUR**2))                                                 RAY00420
      GO TO 31                                                          RAY00430
   30 IF(THETA.NE.ZENITH)GO TO 60                                       RAY00440
      DF=0.                                                             RAY00450
      GO TO 32                                                          RAY00460
   31 CONTINUE                                                          RAY00470
      ACDR=ABS(CDR)                                                     RAY00480
C***  SRC modification   8/6/92                                         RAY00490
C-----Note near the scattering angle/azimuth crossover (around 45deg)   RAY00500
C-----there is some angular excess - we now allow for 2%                RAY00510
      IF(ACDR.GT.1.02) GO TO 60                                         RAY00520
      IF(ACDR.GT.1.0)CDR = CDR/ACDR                                     RAY00530
      DF=ACOS(CDR)/RAD                                                  RAY00540
   32 CONTINUE                                                          RAY00550
      CALL INTEG(DF,BETA,IPF,GI)                                        RAY00560
      DO 50 I = 1,39                                                    RAY00570
      WAVE = 0.36 + FLOAT(I)*0.01                                       RAY00580
      WV2 = WAVE**(-2.)                                                 RAY00590
      WV4 = WV2*WV2                                                     RAY00600
      TAR0IZ = 0.008569*WV4*(1. + 0.0113*WV2 + 0.00013*WV4)*EXP(-ELEV/  RAY00610
     1HR)                                                               RAY00620
      TARHIZ = TAR0IZ*EXP(-HPBL/HR)                                     RAY00630
      TAR0HZ = TAR0IZ - TARHIZ                                          RAY00640
      FD0 = SOLAR(I)*EXP(-TAR0IZ/XMU0)                                  RAY00650
      FDH = SOLAR(I)*EXP(-TARHIZ/XMU0)                                  RAY00660
      FDAV = (FD0 + FDH)*0.5                                            RAY00670
      IF(BETA.EQ.0.) GO TO 100                                          RAY00680
      SKY = FORPIN*PRAY*(1.-EXP(-TARHIZ/XMU))*SOLAR(I)                  RAY00690
      SURF = SKY*EXP(-TAR0HZ/XMU) + FORPIN*PRAY*(1.-EXP(-TAR0HZ         RAY00700
     1/XMU0))*FDAV                                                      RAY00710
      SPECR(I) = SURF + GI(I)                                           RAY00720
      GO TO 50                                                          RAY00730
  100 CONTINUE                                                          RAY00740
      BETA0R = TAR0IZ/HR                                                RAY00750
      TAUR0I = BETA0R*PARMR                                             RAY00760
      TAUR0D = BETA0R*ERF(DRR)*PARMR                                    RAY00770
      TAURDI = TAUR0I - TAUR0D                                          RAY00780
      SKYH = FORPIN*PRAY*(1.-EXP(-TAURDI))*SOLAR(I)                     RAY00790
      SURFH = SKYH*EXP(-TAUR0D) + FORPIN*PRAY*(1.-EXP(-TAUR0D))         RAY00800
     1*FDAV                                                             RAY00810
      SPECR(I) = SURFH + GI(I)                                          RAY00820
   50 CONTINUE                                                          RAY00830
      GO TO 63                                                          RAY00840
   60 DO 61 I=1,39                                                      RAY00850
      SPECR(I)=1.                                                       RAY00860
   61 CONTINUE                                                          RAY00870
  63  RETURN                                                            RAY00880
      END                                                               RAY00890
