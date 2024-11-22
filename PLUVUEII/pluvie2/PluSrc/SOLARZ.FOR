CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCSOL00010
      SUBROUTINE SOLARZ(SLA,SLO,TZ,IY,IM,ID,TIME,D,NV)                  SOL00020
C                                                                       SOL00030
C  SOLARZ.FOR                                                           SOL00040
C                                                                       SOL00050
C  CALCULATE SOLAR ZENITH ANGLE                                         SOL00060
C                                                                       SOL00070
C  PLUVUE II                                                            SOL00080
C  Version 92243   Level 1.0                                            SOL00090
C                                                                       SOL00100
C  U.S. Environmental Protection Agency                                 SOL00110
C  Office of Air Quality Planning and Standards                         SOL00120
C  Research Triangle Park, North Carolina                               SOL00130
C                                                                       SOL00140
C  Modifications:                                                       SOL00150
C                                                                       SOL00160
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCSOL00170
      SAVE                                                              SOL00180
C***                                                                    SOL00190
C***     SLA...  LATITUDE (DEG)  SOUTH = MINUS                          SOL00200
C***     SLO...  LONGITUDE (DEG)  EAST = MINUS                          SOL00210
C***     TZ...  TIME ZONE                                               SOL00220
C***            ALSO INCLUDES FRACTION IF LOCAL TIME IS NOT             SOL00230
C***             STANDARD MERIDIAN TIME.  E.G. POONA, INDIA 5.5         SOL00240
C***     IY..  YEAR                                                     SOL00250
C***     IM..  MONTH                                                    SOL00260
C***     ID..  DAY                                                      SOL00270
C***     TIME.. LOCAL STANDARD TIME IN HOURS AND MINUTES.               SOL00280
C***            1 30 PM = 1330  ** STANDARD TIME **                     SOL00290
C***     D..  RETURNED VALUE                                            SOL00300
C***     NV..  VALUE TO BE RETURNED, SELECTED AS FOLLOWS....            SOL00310
C***           1...  DECLINATION (DEG.)                                 SOL00320
C***           2...  EQUATION OF TIME ADJUSTMENT (HRS.)                 SOL00330
C***           3...  TRUE SOLAR TIME (HRS.)                             SOL00340
C***           4...  HOUR ANGLE (DEG.)                                  SOL00350
C***           5...  SOLAR ELEVATION (DEG.)                             SOL00360
C***           6...  OPTICAL AIRMASS                                    SOL00370
C***     0 ( NV ( 7.  OTHERWISE, D = 9999.                              SOL00380
C***                                                                    SOL00390
      COMMON /SOL/ EFFDEC, HRANGL                                       SOL00400
      DIMENSION MD(11)                                                  SOL00410
      DOUBLE PRECISION RAD,SDEC                                         SOL00420
      DATA MD/31,29,31,30,31,30,2*31,30,31,30/                          SOL00430
      DATA A,B,C,SIGA/0.15,3.885,1.253,279.9348/                        SOL00440
      RAD=572957.75913E-4                                               SOL00450
      SDEC=39784.988432E-5                                              SOL00460
      RE=1.                                                             SOL00470
      IF (SLO.LT.0.) RE=-1.                                             SOL00480
      KZ=TZ                                                             SOL00490
      TC=(TZ-FLOAT(KZ))*RE                                              SOL00500
      TZZ=FLOAT(KZ)*RE                                                  SOL00510
      SLB=SLA/RAD                                                       SOL00520
      K=ID                                                              SOL00530
      TIMH=TIME/100.                                                    SOL00540
      I=TIMH                                                            SOL00550
      TIMLOC=(TIMH-FLOAT(I))/0.6+FLOAT(I)+TC                            SOL00560
      IMC=IM-1                                                          SOL00570
      IF (IMC.LT.1) GO TO 10                                            SOL00580
      DO 5 I=1,IMC                                                      SOL00590
    5 K=K+MD(I)                                                         SOL00600
   10 LEAP=1                                                            SOL00610
      NL=MOD(IY,4)                                                      SOL00620
      IF (NL.LT.1) LEAP=2                                               SOL00630
      SMER=TZZ*15.                                                      SOL00640
      TK=((SMER-SLO)*4.)/60.                                            SOL00650
      KR=1                                                              SOL00660
      IF (K.GE.61.AND.LEAP.LT.2) KR=2                                   SOL00670
      DAD=(TIMLOC+TZZ)/24.                                              SOL00680
      DAD=DAD+FLOAT(K)-FLOAT(KR)                                        SOL00690
      DF=DAD*360./365.242                                               SOL00700
      DE=DF/RAD                                                         SOL00710
      DESIN=SIN(DE)                                                     SOL00720
      DECOS=COS(DE)                                                     SOL00730
      DESIN2=SIN(DE*2.)                                                 SOL00740
      DECOS2=COS(DE*2.)                                                 SOL00750
      SIG=SIGA+DF+1.914827*DESIN-0.079525*DECOS+0.019938*DESIN2-0.00162*SOL00760
     1DECOS2                                                            SOL00770
      SIG=SIG/RAD                                                       SOL00780
      DECSIN=SDEC*SIN(SIG)                                              SOL00790
      EFFDEC=ASIN(DECSIN)                                               SOL00800
      IF (NV.NE.1) GO TO 15                                             SOL00810
      D=EFFDEC*RAD                                                      SOL00820
      RETURN                                                            SOL00830
   15 EQT=0.12357*DESIN-0.004289*DECOS+0.153809*DESIN2+0.060783*DECOS2  SOL00840
      IF (NV.NE.2) GO TO 20                                             SOL00850
      D=EQT                                                             SOL00860
      RETURN                                                            SOL00870
   20 TST=TK+TIMLOC-EQT                                                 SOL00880
      IF (NV.NE.3) GO TO 25                                             SOL00890
      D=TST                                                             SOL00900
      IF (D.LT.0.) D=D+24.                                              SOL00910
      IF (D.GE.24.) D=D-24.                                             SOL00920
      RETURN                                                            SOL00930
   25 HRANGL=ABS(TST-12.)*15.                                           SOL00940
      IF (NV.NE.4) GO TO 30                                             SOL00950
      D=HRANGL                                                          SOL00960
      RETURN                                                            SOL00970
   30 HRANGL=HRANGL/RAD                                                 SOL00980
      SOLSIN=DECSIN*SIN(SLB)+COS(EFFDEC)*COS(SLB)*COS(HRANGL)           SOL00990
      SOLEL=ASIN(SOLSIN)*RAD                                            SOL01000
      IF (NV.NE.5) GO TO 35                                             SOL01010
      D=SOLEL                                                           SOL01020
      RETURN                                                            SOL01030
   35 IF (NV.NE.6) GO TO 40                                             SOL01040
      IF (SOLEL.LE.0.) GO TO 40                                         SOL01050
      TK=SOLEL+B                                                        SOL01060
      E=1./TK**C                                                        SOL01070
      D=1./(A*E+SOLSIN)                                                 SOL01080
      RETURN                                                            SOL01090
   40 D=9999.                                                           SOL01100
      RETURN                                                            SOL01110
      END                                                               SOL01120
