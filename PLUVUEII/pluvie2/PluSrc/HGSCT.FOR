CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCHGS00010
      SUBROUTINE HGSCT(U,U0,D1,G,W0,B0,B1,B2,P1,P2)                     HGS00020
C                                                                       HGS00030
C   HGSCT.FOR                                                           HGS00040
C                                                                       HGS00050
C  Calculate Henyey-Greenstein scattering functions                     HGS00060
C                                                                       HGS00070
C  PLUVUE II                                                            HGS00080
C  Version 92243   Level 1.0                                            HGS00090
C                                                                       HGS00100
C  U.S. Environmental Protection Agency                                 HGS00110
C  Office of Air Quality Planning and Standards                         HGS00120
C  Research Triangle Park, North Carolina                               HGS00130
C                                                                       HGS00140
C  Modifications:                                                       HGS00150
C                                                                       HGS00160
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCHGS00170
      SAVE                                                              HGS00180
      COMMON / ISAACS / IRE                                             HGS00190
      DIMENSION A9(10,5),U8(9),U9(2),B7(2)                              HGS00200
      DATA A9/.5,.5,.5,.5,.5,.5,.5,.5,.5,.5,                            HGS00210
     2.0,.13979292889,-.12019000482,-.46017123188,-.406828796532,       HGS00220
     3-.3001541656,-.553474414782,-.626794663911,-.84678101,            HGS00230
     4-.406823676662,.0,-1.5989873995,-.2724219928,1.18747390274,       HGS00240
     5.49409050834,-.35928947292,.37397957743,.18057740986,             HGS00250
     6.50718036245,.01406832224,.0,3.5184116963,.6385960123,            HGS00260
     7-2.081230849,-1.0144699491,.1589475781,-.74761782865,             HGS00270
     8-.37416958202,-.374040109,.1055607702,.0,-2.5592172257,           HGS00280
     9-.7459840146,.85392041407,.4272082373,.00049606046,               HGS00290
     1.42711266606,.32038683614,.2136746594,-.2128054157/               HGS00300
      DATA U8/0.0,.1,.2,.3,.4,.5,.6,.8,1.0/                             HGS00310
      DATA PI/3.14159265358/                                            HGS00320
C                                                                       HGS00330
      DTR(X)=PI/180.*X                                                  HGS00340
      RTD(X)=180./PI*X                                                  HGS00350
C                                                                       HGS00360
C     BACKSCATTERING INTERPOLATION                                      HGS00370
C                                                                       HGS00380
      U9(1)=U                                                           HGS00390
      U9(2)=U0                                                          HGS00400
           IF(W0.EQ.1.0) W0 = 0.99999                                   HGS00410
      DO 50 IR=1,2                                                      HGS00420
      IF(U9(IR).LT. .1) GO TO 10                                        HGS00430
      IF(U9(IR).LT. .2) GO TO 12                                        HGS00440
      IF(U9(IR).LT. .3) GO TO 14                                        HGS00450
      IF(U9(IR).LT. .4) GO TO 16                                        HGS00460
      IF(U9(IR).LT. .5) GO TO 18                                        HGS00470
      IF(U9(IR).LT. .6) GO TO 20                                        HGS00480
      IF(U9(IR).LT. .8) GO TO 22                                        HGS00490
      IF(U9(IR).LE.1.0) GO TO 24                                        HGS00500
  10  IQ=1                                                              HGS00510
      GO TO 25                                                          HGS00520
  12  IQ=2                                                              HGS00530
      GO TO 25                                                          HGS00540
  14  IQ=3                                                              HGS00550
      GO TO 25                                                          HGS00560
  16  IQ=4                                                              HGS00570
      GO TO 25                                                          HGS00580
  18  IQ=5                                                              HGS00590
      GO TO 25                                                          HGS00600
  20  IQ=6                                                              HGS00610
      GO TO 25                                                          HGS00620
  22  IQ=7                                                              HGS00630
      GO TO 25                                                          HGS00640
  24  IQ=8                                                              HGS00650
C                                                                       HGS00660
  25  Y5=0.0                                                            HGS00670
      Y6=0.0                                                            HGS00680
      Y7=0.0                                                            HGS00690
      IQP1=IQ+1                                                         HGS00700
C                                                                       HGS00710
      DO 30 J=1,5                                                       HGS00720
      JM1=J-1                                                           HGS00730
      Y9=A9(IQ,J)*G**JM1                                                HGS00740
      Y8=A9(IQP1,J)*G*JM1                                               HGS00750
      Y3=A9(10,J)*G**JM1                                                HGS00760
      Y7=Y7+Y9                                                          HGS00770
      Y6=Y6+Y8                                                          HGS00780
      Y5=Y5+Y3                                                          HGS00790
  30  CONTINUE                                                          HGS00800
      B7(IR)=Y7+(Y6-Y7)*(U9(IR)-U8(IQ))/(U8(IQP1)-U8(IQ))               HGS00810
  50  CONTINUE                                                          HGS00820
C                                                                       HGS00830
      B1=Y5                                                             HGS00840
      B2=B7(1)                                                          HGS00850
      B0=B7(2)                                                          HGS00860
C                                                                       HGS00870
      D=DTR(D1)                                                         HGS00880
      C1=-U*U0+SQRT(1.-U*U)*SQRT(1.-U0*U0)*COS(D)                       HGS00890
      C2=U*U0+SQRT(1.-U*U)*SQRT(1.-U0*U0)*COS(D)                        HGS00900
      P1=W0*(1.-G*G)/(1.+G*G-2.*G*C1)**1.5                              HGS00910
      P2=W0*(1.-G*G)/(1.+G*G-2.*G*C2)**1.5                              HGS00920
      IRE=IRE+1                                                         HGS00930
      RETURN                                                            HGS00940
      END                                                               HGS00950
