CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCPER00010
      SUBROUTINE PERDIF(SPECR,ZENITH)                                   PER00020
C                                                                       PER00030
C  PERDIF                                                               PER00040
C                                                                       PER00050
C  CALCULATE PERFECT DIFFUSE REFLECTOR PROPERTIES                       PER00060
C                                                                       PER00070
C      Arguments                                                        PER00080
C       input: zenith (constant) =                                      PER00090
C       output: specr (array (39 elements) = spectral radiance          PER00100
C                                                                       PER00110
C  PLUVUE II                                                            PER00120
C  Version 92243   Level 1.0                                            PER00130
C                                                                       PER00140
C  U.S. Environmental Protection Agency                                 PER00150
C  Office of Air Quality Planning and Standards                         PER00160
C  Research Triangle Park, North Carolina                               PER00170
C                                                                       PER00180
C  Modifications:                                                       PER00190
C                                                                       PER00200
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCPER00210
      SAVE                                                              PER00220
      COMMON/REF/XCAP0,YCAP0,ZCAP0,U0,V0,PROP                           PER00230
      COMMON/COLOR/YCAP,VAL,X,Y,YCAPD,VALD,XD,YD,DELUV,DELAB,           PER00240
     1XBAR(39),YBAR(39),ZBAR(39),PI,CONT1,CONT2,CONT3,BRATIO            PER00250
      DIMENSION SPECR(39)                                               PER00260
      PI = 3.1415962                                                    PER00270
      XLUMIN=1./(2.*PI)                                                 PER00280
      CALL BACOBJ(ZENITH,0.,90.,4,0.,SPECR,XLUMIN)                      PER00290
      XCAP0=0.                                                          PER00300
      YCAP0=0.                                                          PER00310
      ZCAP0=0.                                                          PER00320
      DO 10 I=1,39                                                      PER00330
      XCAP0=XCAP0+SPECR(I)*XBAR(I)                                      PER00340
      YCAP0=YCAP0+SPECR(I)*YBAR(I)                                      PER00350
      ZCAP0=ZCAP0+SPECR(I)*ZBAR(I)                                      PER00360
 10   CONTINUE                                                          PER00370
      PROP=100./YCAP0                                                   PER00380
      XCAP0=XCAP0*PROP                                                  PER00390
      YCAP0=100.                                                        PER00400
      ZCAP0=ZCAP0*PROP                                                  PER00410
      D0=XCAP0+1500.+3.*ZCAP0                                           PER00420
      U0=4.*XCAP0/D0                                                    PER00430
      V0=900./D0                                                        PER00440
      RETURN                                                            PER00450
      END                                                               PER00460
