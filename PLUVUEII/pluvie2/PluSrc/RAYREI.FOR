CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCREI00010
      SUBROUTINE RAYREI(U,U0,DD,SPECRI,SPECRU)                          REI00020
C                                                                       REI00030
C  RAYREI.FOR                                                           REI00040
C                                                                       REI00050
C  CALCULATE SPECTRAL RADIANCE OF CLEAR SKY IN A RAYLEIGH ATMOSPHERE.   REI00060
C                                                                       REI00070
C  PLUVUE II                                                            REI00080
C  Version 92243   Level 1.0                                            REI00090
C                                                                       REI00100
C  U.S. Environmental Protection Agency                                 REI00110
C  Office of Air Quality Planning and Standards                         REI00120
C  Research Triangle Park, North Carolina                               REI00130
C                                                                       REI00140
C  Modifications:                                                       REI00150
C                                                                       REI00160
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCREI00170
      SAVE                                                              REI00180
C                                                                       REI00190
      COMMON/BCKGND/ ELEV,RVAMB,ACCAMB,AMBNO2,RH,ROVA,ROVC,ROVS,ROVP,   REI00200
     1SIGA,SIGC,SIGS,SIGP,HPBL,IREAD,CORAMB,AMBNO3,AMBSO4,INTYP         REI00210
     2,DENA,DENC,DENP,DENS,ROVCAR,SIGCAR,DENCAR,AMBCAR                  REI00220
      COMMON / MISC/ ABSNO2(39),SOLAR(39),RAD,FORPIN,OMZ(39),OMH(39)    REI00230
     1,NTHETA,RFR,RFI                                                   REI00240
      DIMENSION SPECRI(39),SPECRU(39),FLUXUP(39)                        REI00250
C      REAL I1,K  !I1 commented out below                               REI00260
      REAL K                                                            REI00260
      DATA PI/3.14159265358/                                            REI00270
C                                                                       REI00280
      DTR(X)=PI/180.*X                                                  REI00290
      RTD(X)=180./PI*X                                                  REI00300
C                                                                       REI00310
      IF(U.LT.0.0262) U=0.0262                                          REI00320
      A1=0.3                                                            REI00330
      W0=1.                                                             REI00340
      DO 50 II=1,9                                                      REI00350
      I=5*(II-2)+4                                                      REI00360
      IF(II.EQ.1)I=1                                                    REI00370
      WAVE = 0.36 + FLOAT(I)*0.01                                       REI00380
      WV2 = WAVE**(-2.)                                                 REI00390
      WV4 = WV2*WV2                                                     REI00400
      T0 = 0.008569*WV4*(1. + 0.0113*WV2 + 0.00013*WV4)*EXP(-ELEV/      REI00410
     19.8)                                                              REI00420
      F=SOLAR(I)/PI                                                     REI00430
C                                                                       REI00440
      CALL RAYSCT(U,U0,DD,C1,C2,S,S1,P1,P2,W0,B0,B1,B2)                 REI00450
C                                                                       REI00460
C     DEFINE CONSTANTS                                                  REI00470
C                                                                       REI00480
      G1=2.*(1.-W0*(1.-B1))                                             REI00490
      G2=2.*W0*B1                                                       REI00500
      K=G1**2-G2**2                                                     REI00510
      IF(K.LE.0) K=0.                                                   REI00520
      K=SQRT(G1*G1-G2*G2)                                               REI00530
      C=PI*F*W0*(B0/U0-G2*(1.-B0)-G1*B0)*U0**2/(1.-K**2*U0**2)          REI00540
      E1=(C*(G1+1./U0)-PI*F*W0*B0)*(1./(G1-K)-A1/G2)                    REI00550
      E2=-C+PI*F*U0*A1+A1/G2*(C*(G1+1./U0)-PI*F*W0*B0)                  REI00560
      E3=(G1+K)*(-1./(G1-K)+A1/G2)                                      REI00570
      E4=1.-A1/G2*(G1+K)                                                REI00580
      B=(E1*EXP(K*T0)+E2*EXP(-T0/U0))/(E3*EXP(K*T0)+E4*EXP(-K*T0))      REI00590
      A=(B*(G1+K)+(C*(G1+1./U0)-PI*F*W0*B0))/(K-G1)                     REI00600
C                                                                       REI00610
C     COMPUTE INTENSITIES                                               REI00620
C                                                                       REI00630
C                                                                       REI00640
C     UPWARD INTENSITY                                                  REI00650
C     In this modified version of ISAAC's TWOSTR program the term I1    REI00660
C     seems to be a trivial holdover from testing and has been          REI00670
C     commented out by SRC - GEM                                        REI00680
C                                                                       REI00690
C      U1=A*(W0/PI)*(1.-B2+B2/G2*(G1-K))                                REI00700
C      U2=B*(W0/PI)*(1.-B2+B2/G2*(G1+K))                                REI00710
C      U3=C*(W0/PI)*(1.-B2)+W0*B2/(PI*G2)*C*(G1+1./U0-PI*F*W0*B0)+F/4.*PREI00720
C                                                                       REI00730
C      F5=A*EXP(K*T0)+B*EXP(-K*T0)+C*EXP(-T0/U0)                        REI00740
C                                                                       REI00750
C      D2=F5/A-U1*EXP(K*T0)/(1.-U*K)                                    REI00760
C      D3=-U2*EXP(-K/T0)/(1.+U*K)-U3*EXP(-T0/U)/(1.+U/U0)               REI00770
C      D1=EXP(-T0/U)*(D2+D3)                                            REI00780
C      T=0.                                                             REI00790
C      I1=D1*EXP(T/U)+U1*EXP(K*T)/(1.-U*K)+U2*EXP(-K*T)/(1.+U*K)        REI00800
C     1 +U3*EXP(-T/U0)/(1.+U/U0)                                        REI00810
C                                                                       REI00820
      FLUXUP(II)=A*EXP(K*T0) + B*EXP(-K*T0) + C*EXP(-T0/U0)             REI00830
      SPECRU(II) = FLUXUP(II) / PI                                      REI00840
C                                                                       REI00850
C     DOWNWARD INTENSITY                                                REI00860
C                                                                       REI00870
      T=T0                                                              REI00880
      W1=W0/PI*A*(1.-U*K)/(1.+U*K)*(B2+(1.-B2)*(G1-K)/G2)               REI00890
      W2=W0/PI*B*(1.+U*K)/(1.-U*K)*(B2+(1.-B2)*(G1+K)/G2)               REI00900
      Y=C*(G1+1./U0)-PI*F*W0*B0                                         REI00910
      W3=(U0+U)/(U0-U)*(W0/PI*B2*C+W0/PI*(1.-B2)*Y/G2+F/4.*P2)          REI00920
      E1=-(W1/(1.-U*K)+W2/(1.+U*K)+W3/(1.+U/U0))                        REI00930
      SPECRI(II)=E1*EXP(-T/U)+W1*EXP(K*T)/(1.-U*K)+W2*EXP(-K*T)/(1.+U*K)REI00940
     1 +W3*EXP(-T/U0)/(1.+U/U0)                                         REI00950
   50 CONTINUE                                                          REI00960
      GO TO 51                                                          REI00970
  51  RETURN                                                            REI00980
      END                                                               REI00990
