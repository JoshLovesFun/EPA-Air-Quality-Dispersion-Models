CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCHR00010
      SUBROUTINE CHROMA(SPECB,SPECR)                                    CHR00020
C                                                                       CHR00030
C  CHROMA.FOR                                                           CHR00040
C                                                                       CHR00050
C  CALCULATES VARIOUS COLORATION PARAMETERS SUCH AS CHROMA-             CHR00060
C  TICITY COORDINATES,LUMINANCE,VALUE,CONTRAST,BLUE-RED RATIO,          CHR00070
C  AND DELTA E.                                                         CHR00080
C                                                                       CHR00090
C                                                                       CHR00100
C  PLUVUE II                                                            CHR00110
C  Version 92243   Level 1.0                                            CHR00120
C                                                                       CHR00130
C  U.S. Environmental Protection Agency                                 CHR00140
C  Office of Air Quality Planning and Standards                         CHR00150
C  Research Triangle Park, North Carolina                               CHR00160
C                                                                       CHR00170
C  Modifications:                                                       CHR00180
C                                                                       CHR00190
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCHR00200
C                                                                       CHR00210
      SAVE                                                              CHR00220
C*****                                                                  CHR00230
C*****                                                                  CHR00240
      COMMON/COLOR/YCAP,VAL,X,Y,YCAPD,VALD,XD,YD,DELUV,DELAB,           CHR00250
     1XBAR(39),YBAR(39),ZBAR(39),PI,CONT1,CONT2,CONT3,BRATIO            CHR00260
      COMMON/REF/XCAP0,YCAP0,ZCAP0,U0,V0,PROP                           CHR00270
      DIMENSION SPECB(39),SPECR(39)                                     CHR00280
      XCAP=0.                                                           CHR00290
      YCAP=0.                                                           CHR00300
      ZCAP=0.                                                           CHR00310
      XCAPR=0.                                                          CHR00320
      YCAPR=0.                                                          CHR00330
      ZCAPR=0.                                                          CHR00340
      DO 10 I=1,39                                                      CHR00350
      XCAP=XCAP+SPECB(I)*XBAR(I)                                        CHR00360
      YCAP=YCAP+SPECB(I)*YBAR(I)                                        CHR00370
      ZCAP=ZCAP+SPECB(I)*ZBAR(I)                                        CHR00380
      XCAPR=XCAPR+SPECR(I)*XBAR(I)                                      CHR00390
      YCAPR=YCAPR+SPECR(I)*YBAR(I)                                      CHR00400
      ZCAPR=ZCAPR+SPECR(I)*ZBAR(I)                                      CHR00410
 10   CONTINUE                                                          CHR00420
      XCAP=XCAP*PROP                                                    CHR00430
      YCAP=YCAP*PROP                                                    CHR00440
      ZCAP=ZCAP*PROP                                                    CHR00450
      TXYZ=XCAP+YCAP+ZCAP                                               CHR00460
      XCAPR=XCAPR*PROP                                                  CHR00470
      YCAPR=YCAPR*PROP                                                  CHR00480
      ZCAPR=ZCAPR*PROP                                                  CHR00490
      TXYZR=XCAPR+YCAPR+ZCAPR                                           CHR00500
      X=XCAP/TXYZ                                                       CHR00510
      Y=YCAP/TXYZ                                                       CHR00520
      XR=XCAPR/TXYZR                                                    CHR00530
      YR=YCAPR/TXYZR                                                    CHR00540
      XD=X-XR                                                           CHR00550
      YD=Y-YR                                                           CHR00560
      YCAPD=YCAP-YCAPR                                                  CHR00570
      D=XCAP+15.*YCAP+3.*ZCAP                                           CHR00580
      DR=XCAPR+15.*YCAPR+3.*ZCAPR                                       CHR00590
      U=4.*XCAP/D                                                       CHR00600
      V=9.*YCAP/D                                                       CHR00610
      UR=4.*XCAPR/DR                                                    CHR00620
      VR=9.*YCAPR/DR                                                    CHR00630
      CONT1=(SPECB(4)-SPECR(4))/SPECR(4)                                CHR00640
      CONT2=(SPECB(19)-SPECR(19))/SPECR(19)                             CHR00650
      CONT3=(SPECB(34)-SPECR(34))/SPECR(34)                             CHR00660
      BRATIO=(SPECB(4)/SPECR(4))/(SPECB(34)/SPECR(34))                  CHR00670
      VAL=116.*(YCAP/YCAP0)**.333-16.                                   CHR00680
      VALR=116.*(YCAPR/YCAP0)**.333-16.                                 CHR00690
      USTAR=13.*VAL*(U-U0)                                              CHR00700
      USTARR=13.*VALR*(UR-U0)                                           CHR00710
      VSTAR=13.*VAL*(V-V0)                                              CHR00720
      VSTARR=13.*VALR*(VR-V0)                                           CHR00730
      ASTAR=500.*((XCAP/XCAP0)**.333-(YCAP/YCAP0)**.333)                CHR00740
      ASTARR=500.*((XCAPR/XCAP0)**.333-(YCAPR/YCAP0)**.333)             CHR00750
      BSTAR=200.*((YCAP/YCAP0)**.333-(ZCAP/ZCAP0)**.333)                CHR00760
      BSTARR=200.*((YCAPR/YCAP0)**.333-(ZCAPR/ZCAP0)**.333)             CHR00770
      VALD=VAL-VALR                                                     CHR00780
      USTARD=USTAR-USTARR                                               CHR00790
      VSTARD=VSTAR-VSTARR                                               CHR00800
      ASTARD=ASTAR-ASTARR                                               CHR00810
      BSTARD=BSTAR-BSTARR                                               CHR00820
      DELUV=SQRT(VALD*VALD+USTARD*USTARD+VSTARD*VSTARD)                 CHR00830
      DELAB=SQRT(VALD*VALD+ASTARD*ASTARD+BSTARD*BSTARD)                 CHR00840
                                                                        CHR00850
      RETURN                                                            CHR00860
      END                                                               CHR00870
