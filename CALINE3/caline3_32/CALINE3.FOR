C                                                                       CLN00010
C***********************************************************************CLN00011
C                                                                       CLN00012
C                         CALINE3 (DATED 12317)                         CLN00020
C                                                                       CLN00021
C            *** SEE CALINE3 MODEL CHANGE BULLETIN MCB#2 ***            CLN00022
C                                                                       CLN00023
C    ON THE SUPPORT CENTER FOR REGULATORY AIR MODELS BULLETIN BOARD     CLN00024
C                                                                       CLN00025
C                        http://www.epa.gov/ttn/scram                   CLN00026
C                                                                       CLN00030
C***********************************************************************CLN00040
C   November 13, 2012 CALINE3 was recompiled using Intel Visual Fortran
C                     No changes were made to the source code other
C                       than revising the creation date above and 
C                       with a Format statement below
C***********************************************************************
C                                                                       CLN00060
C          DATA SET TMENV051   AT LEVEL 011 AS OF 03/04/80              CLN00070
C                                                                       CLN00080
C **********  PROGRAM CALINE3  *****  (RENAMED TMENV051)  ******        CLN00090
C                                                                       CLN00100
C    SEPTEMBER, 1979 VERSION                                            CLN00110
C    DEVELOPER: PAUL BENSON (916) 739-2459                              CLN00120
C                            ATSS 497-2459                              CLN00130
C                                                                       CLN00140
C       AGENCY: CALTRANS                                                CLN00150
C                TRANSPORTATION LABORATORY                              CLN00160
C                5900 FOLSOM BLVD.                                      CLN00170
C                SACRAMENTO, CALIFORNIA  95819                          CLN00180
C                                                                       CLN00190
C   FORTRAN                                                             CLN00200
C   CONVERSION: EDWARD C. WONG                                          CLN00210
C                                                                       CLN00220
C                                                                       CLN00230
C *****  PROGRAM DESCRIPTION  *****                                     CLN00240
C                                                                       CLN00250
C    CALINE3 IS THE 3RD GENERATION GAUSSIAN LINE SOURCE AIR             CLN00260
C    QUALITY MODEL DEVELOPED BY CALTRANS.  ITS PURPOSE IS TO            CLN00270
C    PREDICT POLLUTANT CONCENTRATIONS NEAR TRANSPORTATION               CLN00280
C    FACILITIES (MICROSCALE REGION).  THE PRESENT VERSION               CLN00290
C    CAN PROCESS UP TO 20 LINKS AND 20 RECEPTORS FOR AN                 CLN00300
C    UNLIMITED NUMBER OF METEOROLOGICAL CONDITIONS.                     CLN00310
C                                                                       CLN00320
C                                                                       CLN00330
C                                                                       CLN00340
C                                                                       CLN00350
      REAL MOWT,NE,LIM,KZ,LB,LBRG(20),RCP(5,20),LNK(5,20),              CLN00360
     *    INC,MIXH                                                      XXX00370
      INTEGER PGCT,CLAS                                                 CLN00380
      DOUBLE PRECISION JOB(5),RUN(5)                                    XXX00390
C     ! FACTOR MICRO-GRAMS/METER**3 TO PPM                              CLN00400
      DOUBLE PRECISION HYP,SIDE,FAC2,PD,A,B,L,D,                        CLN00410
     *    XPRI,YPRI,APRI,BPRI,LPRI,DPRI,XD,YD,D1,D2,                    CLN00420
     *    LL(20),INTG(6)                                                CLN00430
      CHARACTER*4 FMT,FMT1,FMT2,FMT3,FMT4,VFM,VFM1,VFM2                 XXX00435
      CHARACTER*2 AG,DP,FL,BR,TYP                                       XXX00436
      CHARACTER*1 STB,COD,DASH,LINE                                     XXX00437
C                                                                       CLN00440
C *****  DIMENSION ARRAYS  *****                                        CLN00450
C                                                                       CLN00460
      DIMENSION C(20,20),XR(20),YR(20),ZR(20),XL1(20),XL2(20),YL1(20),  CLN00470
     *    YL2(20),VPHL(20),EFL(20),HL(20),WL(20),AZ(6),AY1(6),          CLN00480
     *    AY2(6),Y(6),WT(5),FMT(6),FMT1(16),FMT2(4),FMT3(6),FMT4(4),    XXX00490
     *    VFM(20),VFM1(20),VFM2(20),STB(6),COD(20),LINE(102),TYP(20)    XXX00491
C                                                                       CLN00500
C ***  STATEMENTS FOR VARIABLE OUTPUT (NL>1)  ***                       CLN00510
C                                                                       CLN00520
      DATA FMT  /'(1H+','    ',',70X',',7HC','O/LI','NK) '/             XXX00530
      DATA FMT1 /'(29X','   ,','1H*,','08X,',' 15H','COOR',             XXX00540
     *     'DINA','TES ','(M),','08X,','9H* ','+ AM','B *,','01X,',     XXX00550
     *     '5H(P','PM))'/                                               XXX00560
      DATA FMT2 /'(1H+',',69X',',01A','1)  '/                           XXX00570
      DATA FMT3 /'(29X','   ,','1H*,','25X,','5H(P','PM))'/             XXX00580
      DATA FMT4 /'(1H+',',29X',', 57','A1) '/                           XXX00590
      DATA VFM /',70X',',72X',',74X',',77X',',79X',                     XXX00600
     *                   ',82X',',84X',',87X',',89X',',92X',            XXX00610
     *                   ',53X',',55X',',58X',',60X',',63X',            XXX00620
     *                   ',65X',',68X',',70X',',73X',',75X'/            XXX00630
      DATA VFM1 /'01X,','04X,','06X,','09X,','11X,',                    XXX00640
     *                   '14X,','16X,','19X,','21X,','24X,',            XXX00650
     *                   '25X,','27X,','30X,','32X,','35X,',            XXX00660
     *                   '37X,','40X,','42X,','45X,','47X,'/            XXX00670
      DATA VFM2 /',01A',',12A',',17A',',22A',',27A',                    XXX00680
     *                   ',32A',',37A',',42A',',47A',',52A',            XXX00690
     *                   ', 57',', 62',', 67',', 72',', 77',            XXX00700
     *                   ', 82',', 87',', 92',', 97',',102'/            XXX00710
C                                                                       CLN00720
C *****  DATA INPUT  *****                                              CLN00730
C                                                                       CLN00740
      DATA AZ/1112.,556.,353.,219.,124.,56./                            CLN00750
      DATA AY1/0.46,0.29,0.18,0.11,0.087,0.057/                         CLN00760
      DATA AY2/1831.,1155.,717.,438.,346.,227./                         CLN00770
      DATA WT/0.25,0.75,1.,0.75,0.25/                                   CLN00780
C                                                                       CLN00790
      DATA AG,DP,FL,BR/'AG','DP','FL','BR'/                             XXX00800
      DATA STB/'A','B','C','D','E','F'/                                 XXX00810
      DATA COD/'A','B','C','D','E','F','G','H','I','J',                 XXX00820
     *         'K','L','M','N','O','P','Q','R','S','T'/                 XXX00830
      DATA DASH/'-'/                                                    XXX00840
C                                                                       CLN00850
C                                                                       CLN00860
C *****  END DATA STATEMENTS  *****                                     CLN00870
C                                                                       CLN00880
C     CALL WSTCLK
C                                                                       CLN00890
C *****  WRITE HEADERS  *****                                           CLN00900
C                                                                       CLN00910
      WRITE(6,100)                                                      CLN00920
C                                                                       CLN00930
C                                                                       CLN00940
C *****  INITIALIZATION OF CONSTANTS AND COUNTERS  *****                CLN00950
C                                                                       CLN00960
      PGCT=1                                                            CLN00970
      PI=3.1415926                                                      CLN00980
      RAD=PI/180.                                                       CLN00990
      DEG=180./PI                                                       CLN01000
      MOWT=28.                                                          CLN01010
C     ! MOLECULAR WEIGHT OF CO                                          CLN01020
      FPPM=0.0245/MOWT                                                  CLN01030
C                                                                       CLN01040
      DREF=ALOG(10000.)                                                 CLN01050
C *****  DATA READ FOR CURRENT JOB AND INITIAL COMPUTATIONS  *****      CLN01060
C                                                                       CLN01070
   10 READ (5,120,END=9999) JOB,ATIM,Z0,VS,VD,NR,SCAL                   CLN01080
C     ATIM = AVERAGING TIME (MINUTES)                                   CLN01090
C       Z0 = ROUGHNESS (CM)                                             CLN01100
C       VS = SETTLING VELOCITY (CM/SEC)                                 CLN01110
C       VD = DEPOSITION VELOCITY (CM/SEC)                               CLN01120
C       NR = NUMBER OF RECEPTORS                                        CLN01130
C                                                                       CLN01140
C *** VS AND VD FOR OUTPUT                                              CLN01150
C                                                                       CLN01160
        VS1=VS                                                          CLN01170
        VD1=VD                                                          CLN01180
C                                                                       CLN01190
C *** CONVERT CM/S TO M/S                                               CLN01200
C                                                                       CLN01210
      VS=VS/100.                                                        CLN01220
      VD=VD/100.                                                        CLN01230
      V1=VD-VS/2.                                                       CLN01240
C                                                                       CLN01250
C ***  RECEPTOR LOCATIONS  ***                                          CLN01260
C                                                                       CLN01270
      DO 1000 I=1,NR                                                    CLN01280
      READ (5,130) (RCP(J,I),J=1,5),XR(I),YR(I),ZR(I)                   CLN01290
C XR,YR,ZR = RECEPTOR COORDINATES                                       CLN01300
      XR(I)=SCAL*XR(I)                                                  CLN01310
      YR(I)=SCAL*YR(I)                                                  CLN01320
      ZR(I)=SCAL*ZR(I)                                                  CLN01330
 1000 CONTINUE                                                          CLN01340
C                                                                       CLN01350
C ***  LINK & MET CONDITIONS  ***                                       CLN01360
C                                                                       CLN01370
      READ (5,150) RUN,NL,NM                                            CLN01380
C       NL = NUMBER OF LINKS                                            CLN01390
C       NM = NUMBER OF MET CONDITIONS                                   CLN01400
      DO 1050 I=1,NL                                                    CLN01410
      READ (5,160) (LNK(J,I),J=1,5),TYP(I),XL1(I),YL1(I),XL2(I),YL2(I), CLN01420
     *    VPHL(I),EFL(I),HL(I),WL(I)                                    CLN01430
C      TYP = HIGHWAY TYPE                                               CLN01440
C              AG: AT-GRADE                                             CLN01450
C              DP: DEPRESSED (CUT)                                      CLN01460
C              FL: FILL                                                 CLN01470
C              BR: BRIDGE                                               CLN01480
C XL1,ETC. = LINK ENDPOINT COORDINATES                                  CLN01490
C     VPHL = TRAFFIC VOLUME (VEHICLES/HR)                               CLN01500
C       HL = SOURCE HEIGHT (M)                                          CLN01510
C       WL = MIXING ZONE WIDTH (M)                                      CLN01520
      XL1(I)=SCAL*XL1(I)                                                CLN01530
      XL2(I)=SCAL*XL2(I)                                                CLN01540
      YL1(I)=SCAL*YL1(I)                                                CLN01550
      YL2(I)=SCAL*YL2(I)                                                CLN01560
      HL(I)=SCAL*HL(I)                                                  CLN01570
      WL(I)=SCAL*WL(I)                                                  CLN01580
      LL(I)=SQRT((XL1(I)-XL2(I))**2+(YL1(I)-YL2(I))**2)                 CLN01590
C     ! LINK LENGTH                                                     CLN01600
      IF (LL(I).GE.WL(I)) GO TO 1025                                    CLN01610
      WRITE (6,170)                                                     CLN01620
C     CALL WAUDIT
      STOP                                                              CLN01630
 1025 IF (ABS(HL(I)).LE.10.) GO TO 1050                                 CLN01640
      WRITE (6,180)                                                     CLN01650
C     CALL WAUDIT
      STOP                                                              CLN01660
 1050 CONTINUE                                                          CLN01670
C                                                                       CLN01680
C *****  LOOP TO GENERATE LINE  *****                                   CLN01690
C                                                                       CLN01700
      ILINE=2+5*NL                                                      CLN01710
      DO 5 K=1,ILINE                                                    CLN01720
    5 LINE(K)=DASH                                                      CLN01730
C                                                                       CLN01740
C *****  MET LOOP  *****                                                CLN01750
C                                                                       CLN01760
      DO 9000 IM=1,NM                                                   CLN01770
      READ (5,190) U,BRG,CLAS,MIXH,AMB                                  CLN01780
C        U = WIND SPEED (M/S)                                           CLN01790
C      BRG = WIND DIRECTION (DEGREES)                                   CLN01800
C     CLAS = STABILITY CLASS (A-F)                                      CLN01810
C     MIXH = MIXING HEIGHT (M)                                          CLN01820
C      AMB = AMBIENT CONCENTRATION (PPM)                                CLN01830
C                                                                       CLN01840
      BRG1=BRG                                                          CLN01850
C     ! WIND ANGLE FOR OUTPUT                                           CLN01860
C                                                                       CLN01870
      BRG=BRG+180.                                                      CLN01880
      IF (BRG.GE.360.) BRG=BRG-360.                                     CLN01890
C     ! CONVERSION TO VECTOR ORIENTATION                                CLN01900
C                                                                       CLN01910
C ***  VIRTUAL DISPLACEMENT VECTORS                                     CLN01920
C                                                                       CLN01930
      XVEC=COS(RAD*(450.-BRG))                                          CLN01940
      YVEC=SIN(RAD*(450.-BRG))                                          CLN01950
C                                                                       CLN01960
C *****  CORRECTIONS FOR AVERAGING TIME AND SURFACE ROUGHNESS           CLN01970
C                                                                       CLN01980
      AFAC=(ATIM/3.0)**.2                                               CLN01990
      SY1=ALOG(AY1(CLAS)*((Z0/3.)**.2)*AFAC)                            CLN02000
C     ! ALOG(SIGMA Y) AT 1 M                                            CLN02010
      SY10=ALOG(AY2(CLAS)*((Z0/3.)**.07)*AFAC)                          CLN02020
C     ! ALOG(SIGMA Y) AT 10 KM                                          CLN02030
      PY1=EXP(SY1)                                                      CLN02040
      PY2=(SY10-SY1)/DREF                                               CLN02050
      SZ10=ALOG(AZ(CLAS)*((Z0/10.)**.07)*AFAC)                          CLN02060
C     ! ALOG(SIGMA Z) AT 10 KM                                          CLN02070
C                                                                       CLN02080
C *** ZERO CONCENTRATION MATRIX                                         CLN02090
C                                                                       CLN02100
      DO 720 I=1,NL                                                     CLN02110
          DO 720 J=1,NR                                                 CLN02120
      C(I,J)=0.                                                         CLN02130
  720 CONTINUE                                                          CLN02140
C                                                                       CLN02150
C                                                                       CLN02160
C *****  LINK LOOP  *****                                               CLN02170
C                                                                       CLN02180
      DO 8000 IL=1,NL                                                   CLN02190
      VPH=VPHL(IL)                                                      CLN02200
      EF=EFL(IL)                                                        CLN02210
      IF (TYP(IL).EQ.DP .OR.                                            CLN02220
     *    TYP(IL).EQ.FL) GO TO 870                                      CLN02230
      H=HL(IL)                                                          CLN02240
      GO TO 880                                                         CLN02250
  870 H=0.                                                              CLN02260
  880 W=WL(IL)                                                          CLN02270
C                                                                       CLN02280
C *****  LINK ROUTINE  *****                                            CLN02290
C **************************                                            CLN02300
C                                                                       CLN02310
      W2=W/2.                                                           CLN02320
      Q1=0.1726*VPH*EF                                                  CLN02330
C     ! LINEAL SOURCE STRENGTH PARALLEL TO HIGHWAY IN MICRO-GRAMS/      CLN02340
C                                                   (METER*SEC)         CLN02350
      XD=XL2(IL)-XL1(IL)                                                CLN02360
      YD=YL2(IL)-YL1(IL)                                                CLN02370
      ABSXD=DABS(XD)                                                    CLN02380
      IF(ABSXD.GT.LL(IL))LL(IL)=ABSXD                                   CLN02390
      LB=DEG*(DACOS(DABS(XD)/LL(IL)))                                   XXX02400
C     ! LINK BEARING                                                    CLN02410
      IF (XD.GT.0. .AND.                                                CLN02420
     *    YD.GE.0.) LB=90.-LB                                           CLN02430
      IF (XD.GE.0. .AND.                                                CLN02440
     *    YD.LT.0.) LB=90.+LB                                           CLN02450
      IF (XD.LT.0. .AND.                                                CLN02460
     *    YD.LE.0.) LB=270.-LB                                          CLN02470
      IF (XD.LE.0. .AND.                                                CLN02480
     *    YD.GT.0.) LB=270.+LB                                          CLN02490
      LBRG(IL)=LB                                                       CLN02500
C     ! LINK BEARING MATRIX FOR OUTPUT                                  CLN02510
      PHI=ABS(BRG-LB)                                                   CLN02520
C     ! WIND ANGLE WITH RESPECT TO LINK                                 CLN02530
      IF (PHI.LE.90.) GO TO 7600                                        CLN02540
      IF (PHI.GE.270.) GO TO 5000                                       CLN02550
      PHI=ABS(PHI-180.)                                                 CLN02560
      GO TO 7600                                                        CLN02570
 5000 PHI=ABS(PHI-360.)                                                 CLN02580
C     ! SET ELEMENT GROWTH BASE                                         CLN02590
 7600 IF (PHI.LT.20.) GO TO 7630                                        CLN02600
      IF (PHI.LT.50.) GO TO 7620                                        CLN02610
      IF (PHI.LT.70.) GO TO 7610                                        CLN02620
      BASE=4.                                                           CLN02630
      GO TO 7650                                                        CLN02640
 7610 BASE=2.                                                           CLN02650
      GO TO 7650                                                        CLN02660
 7620 BASE=1.5                                                          CLN02670
      GO TO 7650                                                        CLN02680
 7630 BASE=1.1                                                          CLN02690
 7650 PHI=RAD*(PHI)                                                     CLN02700
C     ! CONVERSION OF PHI FROM DEGREES TO RADIANS                       CLN02710
      IF (PHI.GT.1.5706) PHI=1.5706                                     CLN02720
      IF (PHI.LT.0.00017) PHI=0.00017                                   CLN02730
C                                                                       CLN02740
C *****  DEPRESSED SECTION  *****                                       CLN02750
C                                                                       CLN02760
      IF (HL(IL).LT.-1.5) GO TO 7700                                    CLN02770
      DSTR=1.                                                           CLN02780
      HDS=1.                                                            CLN02790
      GO TO 7800                                                        CLN02800
 7700 HDS=HL(IL)                                                        CLN02810
      DSTR=0.72*ABS(HDS)**0.83                                          CLN02820
C     ! RESIDENCE TIME FACTOR                                           CLN02830
C                                                                       CLN02840
C *****  SIGMA Z POWER CURVE  *****                                     CLN02850
C                                                                       CLN02860
 7800 TR=DSTR*W2/U                                                      CLN02870
C     ! RESIDENCE TIME                                                  CLN02880
      SGZ1=ALOG((1.8+0.11*TR)*(ATIM/30.)**0.2)                          CLN02890
C     ! ALOG(SIGMA Z) AT W2                                             CLN02900
      PZ2=(SZ10-SGZ1)/(DREF-ALOG(W2))                                   CLN02910
      PZ1=EXP((SZ10+SGZ1-PZ2*(DREF+ALOG(W2)))/2.)                       CLN02920
C                                                                       CLN02930
C *****  END OF LINK ROUTINE  *****                                     CLN02940
C                                                                       CLN02950
C                                                                       CLN02960
C *****  RECEPTOR LOOP  *****                                           CLN02970
C                                                                       CLN02980
      DO 6000 IR=1,NR                                                   CLN02990
      A=(XR(IR)-XL1(IL))**2+(YR(IR)-YL1(IL))**2                         CLN03000
      B=(XR(IR)-XL2(IL))**2+(YR(IR)-YL2(IL))**2                         CLN03010
      L=(B-A-LL(IL)**2)/(2.*LL(IL))                                     CLN03020
C     ! OFFSET LENGTH                                                   CLN03030
      IF (A.GT.L**2) D=DSQRT(A-L**2)                                    CLN03040
      IF (A.LE.L**2) D=0.                                               CLN03050
C     ! RECEPTOR DISTANCE                                               CLN03060
      UWL=LL(IL)+L                                                      CLN03070
C     ! UPWIND LENGTH                                                   CLN03080
      DWL=L                                                             CLN03090
C     ! DOWNWIND LENGTH                                                 CLN03100
      IF(D.EQ.0.D0)DVIR=1.D0                                            CLN03110
      IF(D.NE.0.D0)DVIR=D                                               CLN03120
      XPRI=XR(IR)+DVIR*XVEC                                             CLN03130
      YPRI=YR(IR)+DVIR*YVEC                                             CLN03140
      APRI=(XPRI-XL1(IL))**2+(YPRI-YL1(IL))**2                          CLN03150
      BPRI=(XPRI-XL2(IL))**2+(YPRI-YL2(IL))**2                          CLN03160
      LPRI=(BPRI-APRI-LL(IL)**2)/(2.*LL(IL))                            CLN03170
      IF (APRI.GT.LPRI**2) DPRI=DSQRT(APRI-LPRI**2)                     CLN03180
      IF (APRI.LE.LPRI**2) DPRI=0.                                      CLN03190
      IF (DPRI.LT.D) D=-D                                               CLN03200
      IF (LPRI-L) 5725,5735,5735                                        CLN03210
 5725 TEMP=UWL                                                          CLN03220
      UWL=-DWL                                                          CLN03230
      DWL=-TEMP                                                         CLN03240
 5735 IF (TYP(IL).EQ.AG .OR.                                            CLN03250
     *    TYP(IL).EQ.BR) GO TO 5750                                     CLN03260
C                                                                       CLN03270
      D1=W2+2.*ABS(HL(IL))                                              CLN03280
      D2=W2                                                             CLN03290
C     ! SINGLE PRECISION TO DOUBLE PRECISION FOR LOGICAL 'IF'           CLN03300
      IF (DABS(D).GE.D1) GO TO 5750                                     CLN03310
C     ! 2:1 SLOPE ASSUMED                                               CLN03320
      IF (DABS(D).LE.D2) Z=ZR(IR)-HL(IL)                                CLN03330
      IF (DABS(D).GT.D2)                                                CLN03340
     *    Z=ZR(IR)-HL(IL)*(1.-(DABS(D)-W2)/(2.*ABS(HL(IL))))            CLN03350
      GO TO 3050                                                        CLN03360
 5750 Z=ZR(IR)                                                          CLN03370
C                                                                       CLN03380
C                                                                       CLN03390
C *****  CALINE3 ROUTINE  *****                                         CLN03400
C *****************************                                         CLN03410
C                                                                       CLN03420
C                                                                       CLN03430
 3050 SGN=1.                                                            CLN03440
C ***  DETERMINES DIRECTION ALONG LINK                                  CLN03450
C      +1 --> UPWIND ELEMENTS;  -1 --> DOWNWIND ELEMENTS                CLN03460
C                                                                       CLN03470
 3060 NE=0.                                                             CLN03480
      STP=1.                                                            CLN03490
      FINI=1.                                                           CLN03500
C ***  ELEMENT NUMBER, STEP FACTOR AND LOOP END INITIALIZATION          CLN03510
C                                                                       CLN03520
      IF (SGN.EQ.1. .AND.                                               CLN03530
     *    UWL.LE.0. .AND.                                               CLN03540
     *    DWL.LT.0.) SGN=-1.                                            CLN03550
 3080 IF (SGN.EQ.-1. .AND.                                              CLN03560
     *    UWL.GT.0. .AND.                                               CLN03570
     *    DWL.GE.0.) GO TO 6000                                         CLN03580
C                                                                       CLN03590
C *****  ELEMENT LOOP  *****                                            CLN03600
C                                                                       CLN03610
      ED1=0.                                                            CLN03620
      ED2=SGN*W                                                         CLN03630
C     ! INITIALIZATION OF ELEMENT LIMITS                                CLN03640
 3110 IF (SGN.EQ.-1.) GO TO 3160                                        CLN03650
      IF (ED1.LE.DWL .AND. ED2.LE.DWL) GO TO 3770                       CLN03660
      IF (ED1.GT.DWL .AND. ED2.LT.UWL) GO TO 3250                       CLN03670
      IF (ED1.LE.DWL) ED1=DWL                                           CLN03680
      IF (ED2.LT.UWL) GO TO 3250                                        CLN03690
      ED2=UWL                                                           CLN03700
      SGN=-1.                                                           CLN03710
      NE=-1.                                                            CLN03720
      GO TO 3250                                                        CLN03730
 3160 IF (ED1.GE.UWL .AND. ED2.GE.UWL) GO TO 3770                       CLN03740
      IF (ED1.LT.UWL .AND. ED2.GT.DWL) GO TO 3250                       CLN03750
      IF (ED1.GE.UWL) ED1=UWL                                           CLN03760
      IF (ED2.GT.DWL) GO TO 3250                                        CLN03770
      ED2=DWL                                                           CLN03780
      FINI=0.                                                           CLN03790
 3250 EL2=ABS(ED2-ED1)/2.                                               CLN03800
C     ! ELEMENT HALF-DISTANCE                                           CLN03810
      ECLD=(ED1+ED2)/2.                                                 CLN03820
C     ! ELEMENT CENTERLINE DISTANCE                                     CLN03830
      ELL2=W2/COS(PHI)+(EL2-W2*TAN(PHI))*SIN(PHI)                       CLN03840
C     ! EQUIVALENT LINE HALF-LENGTH                                     CLN03850
      IF (PHI.GE.ATAN(W2/EL2)) CSL2=W2/SIN(PHI)                         CLN03860
      IF (PHI.LT.ATAN(W2/EL2)) CSL2=EL2/COS(PHI)                        CLN03870
C     ! CENTRAL SUB-ELEMENT HALF-LENGTH                                 CLN03880
      EM2=ABS((EL2-W2/TAN(PHI))*SIN(PHI))                               CLN03890
C     ! CENTRAL SUB-ELEMENT HALF-WIDTH                                  CLN03900
      EN2=(ELL2-EM2)/2.                                                 CLN03910
C     ! PERIPHERAL SUB-ELEMENT WIDTH                                    CLN03920
C                                                                       CLN03930
C *****  RECEPTOR DISTANCE LOOP  *****                                  CLN03940
C                                                                       CLN03950
      QE=Q1*CSL2/W2                                                     CLN03960
C     ! CENTRAL SUB-ELEMENT LINEAL SOURCE STRENGTH                      CLN03970
      FET=(ECLD+D*TAN(PHI))*COS(PHI)                                    CLN03980
C     ! ELEMENT FETCH                                                   CLN03990
      HYP=ECLD**2+D**2                                                  CLN04000
      SIDE=FET**2                                                       CLN04010
      IF (SIDE.GT.HYP) YE=0.                                            CLN04020
      IF (SIDE.LE.HYP) YE=DSQRT(HYP-SIDE)                               CLN04030
C     ! Y DISTANCE FROM ELEMENT CENTER TO RECEPTOR                      CLN04040
C                                                                       CLN04050
C *****  DETERMINE SIGMA Y AND SIGMA Z  *****                           CLN04060
C                                                                       CLN04070
      IF (FET.LE.-CSL2) GO TO 3830                                      CLN04080
C     ! ELEMENT DOES NOT CONTRIBUTE                                     CLN04090
      IF (FET.GE.CSL2) GO TO 3320                                       CLN04100
C **  RECEPTOR WITHIN ELEMENT  **                                       CLN04110
      QE=QE*(FET+CSL2)/(2.*CSL2)                                        CLN04120
      FET=(CSL2+FET)/2.                                                 CLN04130
 3320 SGZ=PZ1*FET**PZ2                                                  CLN04140
      KZ=SGZ**2*U/(2.*FET)                                              CLN04150
C     ! VERTICAL DIFFUSIVITY ESTIMATE                                   CLN04160
      SGY=PY1*FET**PY2                                                  CLN04170
      FAC1=0.399/(SGZ*U)                                                CLN04180
C     ! SOURCE STRENGTH - WIND SPEED FACTOR                             CLN04190
C                                                                       CLN04200
C *****  ADJUSTMENT FOR ELEMENT END EFFECT  *****                       CLN04210
C           (POLYNOMIAL APPROXIMATION)                                  CLN04220
C                                                                       CLN04230
      Y(1)=YE+ELL2                                                      CLN04240
      Y(2)=Y(1)-EN2                                                     CLN04250
      Y(3)=Y(2)-EN2                                                     CLN04260
      Y(4)=Y(3)-2*EM2                                                   CLN04270
      Y(5)=Y(4)-EN2                                                     CLN04280
      Y(6)=Y(5)-EN2                                                     CLN04290
      DO 3480 I=1,6                                                     CLN04300
C ***  SUB-ELEMENT SOURCE STRENGTH LOOP                                 CLN04310
C                                                                       CLN04320
      LIM=ABS(Y(I)/SGY)                                                 CLN04330
      T=1./(1.+0.23164*LIM)                                             CLN04340
      ARG=LIM**2/(-2.)                                                  CLN04350
      IF (LIM.GT.5.) INTG(I)=0.                                         CLN04360
      IF (LIM.LE.5.) INTG(I)=0.3989*EXP(ARG)*(0.3194*T-0.3566*T**2+     CLN04370
     *    1.7815*T**3-1.8213*T**4+1.3303*T**5)                          CLN04380
 3480 CONTINUE                                                          CLN04390
      FAC2=0.                                                           CLN04400
      DO 3530 I=1,5                                                     CLN04410
      IF ((SIGN(1.,Y(I))).EQ.(SIGN(1.,Y(I+1))))                         CLN04420
     *    PD=DABS(INTG(I+1)-INTG(I))                                    CLN04430
      IF ((SIGN(1.,Y(I))).NE.(SIGN(1.,Y(I+1))))                         CLN04440
     *    PD=1.-INTG(I)-INTG(I+1)                                       CLN04450
C ***  NORMAL PROBABILITY DENSITY FUNCTION                              CLN04460
C                                                                       CLN04470
      FAC2=FAC2+PD*QE*WT(I)                                             CLN04480
 3530 CONTINUE                                                          CLN04490
C                                                                       CLN04500
      FACT=FAC1*FAC2                                                    CLN04510
C                                                                       CLN04520
C *****  DEPRESSED SECTION  *****                                       CLN04530
C                                                                       CLN04540
      IF (HDS.LT.-1.5 .AND.                                             CLN04550
     *    DABS(D).LT.(W2-3.*HDS)) GO TO 3560                            CLN04560
      GO TO 3580                                                        CLN04570
 3560 IF (DABS(D).LE.W2) FACT=FACT*DSTR                                 CLN04580
      IF (DABS(D).GT.W2) FACT=FACT*(DSTR-(DSTR-1.)*(DABS(D)-W2)/        CLN04590
     *     (-3.*HDS))                                                   CLN04600
C     ! ADJUST FOR DEPRESSED SECTION WIND SPEED                         CLN04610
C                                                                       CLN04620
C *****  DEPOSITION CORRECTION  *****                                   CLN04630
C                                                                       CLN04640
 3580 FAC3=0.                                                           CLN04650
      IF (V1.EQ.0.) GO TO 3670                                          CLN04660
      ARG=V1*SGZ/(KZ*SQRT(2.))+(Z+H)/(SGZ*SQRT(2.))                     CLN04670
      IF (ARG.GT.5.) GO TO 3770                                         CLN04680
      T=1./(1.+0.47047*ARG)                                             CLN04690
      EFRC=(.3480242*T-.0958798*T**2+.7478556*T**3)*EXP(-1.*ARG**2)     CLN04700
      FAC3=(SQRT(2.*PI)*V1*SGZ*EXP(V1*(Z+H)/KZ+.5*(V1*SGZ/KZ)**2)       CLN04710
     *    *EFRC)/KZ                                                     CLN04720
      IF (FAC3.GT.2.) FAC3=2.                                           CLN04730
                                                                        CLN04740
C                                                                       CLN04750
C *****  SETTLING CORRECTION  *****                                     CLN04760
C                                                                       CLN04770
 3670 IF (VS.EQ.0.) GO TO 3710                                          CLN04780
      FAC4=EXP(-VS*(Z-H)/(2.*KZ)-(VS*SGZ/KZ)**2/8.)                     CLN04790
      FACT=FACT*FAC4                                                    CLN04800
C                                                                       CLN04810
C *****  INCREMENTAL CONCENTRATION  *****                               CLN04820
C                                                                       CLN04830
 3710 FAC5=0.                                                           CLN04840
      CNT=0.                                                            CLN04850
 3720 EXLS=0.                                                           CLN04860
 3730 ARG1=-0.5*((Z+H+2.*CNT*MIXH)/SGZ)**2                              CLN04870
      IF (ARG1.LT.-44.) EXP1=0.                                         CLN04880
      IF (ARG1.GE.-44.) EXP1=EXP(ARG1)                                  CLN04890
      ARG2=-0.5*((Z-H+2.*CNT*MIXH)/SGZ)**2                              CLN04900
      IF (ARG2.LT.-44.) EXP2=0.                                         CLN04910
      IF (ARG2.GE.-44.) EXP2=EXP(ARG2)                                  CLN04920
      FAC5=FAC5+EXP1+EXP2                                               CLN04930
      IF (MIXH.GE.1000.) GO TO 3760                                     CLN04940
C     BYPASS MIXING HEIGHT CALCULATION                                  CLN04950
      IF ((EXP1+EXP2+EXLS).EQ.0. .AND. CNT.LE.0.) GO TO 3760            CLN04960
 3740 IF (CNT.GT.0.) GO TO 3750                                         CLN04970
      CNT=ABS(CNT)+1.                                                   CLN04980
      GO TO 3720                                                        CLN04990
 3750 CNT=-1.*CNT                                                       CLN05000
      EXLS=EXP1+EXP2                                                    CLN05010
      GO TO 3730                                                        CLN05020
C                                                                       CLN05030
 3760 INC=FACT*(FAC5-FAC3)                                              CLN05040
C     ! INCREMENTAL CONCENTRATION FROM ELEMENT                          CLN05050
C                                                                       CLN05060
      C(IL,IR)=C(IL,IR)+INC                                             CLN05070
C     ! SUMMATION OF CONCENTRATIONS                                     CLN05080
C                                                                       CLN05090
 3770 IF (FINI.EQ.0.) GO TO 6000                                        CLN05100
      NE=NE+1.                                                          CLN05110
      STP=BASE**NE                                                      CLN05120
C     ! STEP FACTOR                                                     CLN05130
C                                                                       CLN05140
      IF (NE.EQ.0.) GO TO 3080                                          CLN05150
      ED1=ED2                                                           CLN05160
      ED2=ED2+SGN*STP*W                                                 CLN05170
C     ! INCREMENT TO NEXT ELEMENT                                       CLN05180
      GO TO 3110                                                        CLN05190
 3830 IF (SGN.EQ.1.) GO TO 3770                                         CLN05200
C                                                                       CLN05210
C *****  END OF CALINE3 ROUTINE  *****                                  CLN05220
C                                                                       CLN05230
C                                                                       CLN05240
C                                                                       CLN05250
C *****  END LOOPS  *****                                               CLN05260
C                                                                       CLN05270
 6000 CONTINUE                                                          CLN05280
 8000     CONTINUE                                                      CLN05290
C                                                                       CLN05300
      DO 1020 I=1,NL                                                    CLN05310
      DO 1010 J=1,NR                                                    CLN05320
      C(I,J)=C(I,J)*FPPM                                                CLN05330
C     ! CONCENTRATION OF CO FROM MICROGRAMS/M**3 TO PPM                 CLN05340
 1010 CONTINUE                                                          CLN05350
 1020     CONTINUE                                                      CLN05360
C                                                                       CLN05370
C                                                                       CLN05380
C                                                                       CLN05390
C *****  OUTPUT  *****                                                  CLN05400
C                                                                       CLN05410
C ***  CALINE3, ETC. HEADING  ***                                       CLN05420
      WRITE (6,200) PGCT                                                CLN05430
C                                                                       CLN05440
C **  JOB & RUN  **                                                     CLN05450
      WRITE (6,210) JOB,RUN                                             CLN05460
C                                                                       CLN05470
C **  SITE VARIABLES  **                                                CLN05480
      WRITE (6,220)                                                     CLN05490
      WRITE (6,230) U,CLAS,STB(CLAS),VS1,ATIM,MIXH                      CLN05500
      WRITE (6,240) BRG1,Z0,VD1,AMB                                     CLN05510
C                                                                       CLN05520
C **  LINK VARIABLES  **                                                CLN05530
      WRITE (6,250)                                                     CLN05540
      WRITE (6,260)                                                     CLN05550
      WRITE (6,270)                                                     CLN05560
      WRITE (6,280)                                                     CLN05570
      DO 1200 I=1,NL                                                    CLN05580
      WRITE (6,290) COD(I),(LNK(J,I),J=1,5),XL1(I),YL1(I),XL2(I),YL2(I),CLN05590
     *    LL(I),LBRG(I),TYP(I),VPHL(I),EFL(I),HL(I),WL(I)               CLN05600
 1200 CONTINUE                                                          CLN05610
C                                                                       CLN05620
C ***  RECEPTOR LOCATIONS AND MODEL RESULTS  ***                        CLN05630
C                    NL<2                                               CLN05640
      IF (NL.GT.1) GO TO 1350                                           CLN05650
      WRITE (6,300)                                                     CLN05660
      WRITE (6,310)                                                     CLN05670
      WRITE (6,320)                                                     CLN05680
      WRITE (6,330)                                                     CLN05690
      DO 1300 I=1,NR                                                    CLN05700
      C(1,I)=C(1,I)+AMB                                                 CLN05710
C     ADDITION OF AMBIENT                                               CLN05720
      WRITE (6,340) I,(RCP(J,I),J=1,5),XR(I),YR(I),ZR(I),C(1,I)         CLN05730
 1300 CONTINUE                                                          CLN05740
C                                                                       CLN05750
      GO TO 1680                                                        CLN05760
C                                                                       CLN05770
C ***  RECEPTOR LOCATIONS AND MODEL RESULTS  ***                        CLN05780
C                    NL>10                                              CLN05790
 1350 IF(NL.LE.10) GO TO 1430                                           CLN05800
      PGCT=PGCT+1                                                       CLN05810
      WRITE (6,200) PGCT                                                CLN05820
      WRITE (6,210) JOB,RUN                                             CLN05830
      WRITE (6,220)                                                     CLN05840
      WRITE (6,230) U,CLAS,STB(CLAS),VS1,ATIM,MIXH                      CLN05850
      WRITE (6,240) BRG1,Z0,VD1,AMB                                     CLN05860
      WRITE (6,300)                                                     CLN05870
      WRITE (6,450)                                                     CLN05880
      WRITE (6,460)                                                     CLN05890
      WRITE (6,470)                                                     CLN05900
      WRITE (6,480)                                                     CLN05910
      DO 1370 I=1,NR                                                    CLN05920
      CSUM=0.                                                           CLN05930
      DO 1360 J=1,NL                                                    CLN05940
      C(J,I)=10.*C(J,I)+.5                                              CLN05950
      K=C(J,I)                                                          CLN05960
      C(J,I)=K/10.                                                      CLN05970
      CSUM=CSUM+C(J,I)                                                  CLN05980
 1360 CONTINUE                                                          CLN05990
      CSUM=CSUM+AMB                                                     CLN06000
      WRITE (6,490) I,(RCP(J,I),J=1,5),XR(I),YR(I),ZR(I),CSUM           CLN06010
 1370 CONTINUE                                                          CLN06020
      PGCT=PGCT+1                                                       CLN06030
      WRITE (6,200) PGCT                                                CLN06040
      WRITE (6,210) JOB,RUN                                             CLN06050
      WRITE (6,220)                                                     CLN06060
      WRITE (6,230) U,CLAS,STB(CLAS),VS1,ATIM,MIXH                      CLN06070
      WRITE (6,240) BRG1,Z0,VD1,AMB                                     CLN06080
      WRITE (6,305)                                                     CLN06090
      WRITE (6,500)                                                     CLN06100
      FMT(3)=VFM(NL)                                                    CLN06110
      FMT3(4)=VFM1(NL)                                                  CLN06120
      FMT4(3)=VFM2(NL)                                                  CLN06130
      WRITE (6,FMT)                                                     CLN06140
      WRITE (6,FMT3)                                                    CLN06150
      WRITE (6,510) (COD(I),I=1,NL)                                     CLN06160
      WRITE (6,520)                                                     CLN06170
      WRITE (6,FMT4) (LINE(I),I=1,ILINE)                                CLN06180
      DO 1400 I=1,NR                                                    CLN06190
      WRITE (6,530) I,(RCP(J,I),J=1,5),(C(J,I),J=1,NL)                  CLN06200
 1400 CONTINUE                                                          CLN06210
C                                                                       CLN06220
      GO TO 1680                                                        CLN06230
C                                                                       CLN06240
C ***  RECEPTOR LOCATIONS AND MODEL RESULTS  ***                        CLN06250
C                  1<NL<11                                              CLN06260
 1430 FMT(3)=VFM(NL)                                                    CLN06270
      FMT1(14)=VFM1(NL)                                                 CLN06280
      FMT2(3)=VFM2(NL)                                                  CLN06290
      WRITE (6,300)                                                     CLN06300
      WRITE (6,390)                                                     CLN06310
      WRITE (6,FMT)                                                     CLN06320
      WRITE (6,FMT1)                                                    CLN06330
      WRITE (6,400) (COD(I),I=1,NL)                                     CLN06340
      WRITE (6,420)                                                     CLN06350
      WRITE (6,FMT2) (LINE(I),I=1,ILINE)                                CLN06360
C                                                                       CLN06370
C **  MODEL RESULTS  **                                                 CLN06380
C                                                                       CLN06390
      DO 1670 I=1,NR                                                    XXX06400
      CSUM=0.                                                           CLN06410
      DO 1600 J=1,NL                                                    CLN06420
      C(J,I)=10.*C(J,I)+.5                                              CLN06430
      K=C(J,I)                                                          CLN06440
      C(J,I)=K/10.                                                      CLN06450
      CSUM=CSUM+C(J,I)                                                  CLN06460
 1600 CONTINUE                                                          CLN06470
      CSUM=CSUM+AMB                                                     CLN06480
C     ADDITION OF AMBIENT                                               CLN06490
      WRITE (6,440) I,(RCP(J,I),J=1,5),XR(I),YR(I),ZR(I),CSUM,          CLN06500
     *    (C(J,I),J=1,NL)                                               CLN06510
 1670 CONTINUE                                                          XXX06515
 1680 CONTINUE                                                          XXX06520
      PGCT=PGCT+1                                                       CLN06530
 9000 CONTINUE                                                          CLN06540
      GO TO 10                                                          CLN06550
C                                                                       CLN06560
C                                                                       CLN06570
C *****  FORMAT STATEMENTS  *****                                       CLN06580
C NOTE: FORMAT STATEMENT NUMBERS RANGE FROM 100 TO 999                  CLN06590
C                                                                       CLN06600
C **  HEADER FORMATS **                                                 CLN06610
C                                                                       CLN06620
100   FORMAT(1H1,21X,'CALINE3              (DATED 12317)')              CLN06630
C                                                                       CLN06680
C **  INPUT FORMATS  **                                                 CLN06690
C                                                                       CLN06700
  120 FORMAT (5A8,2F4.0,2F5.0,I2,F10.0)                                 CLN06710
C NOTE: CC1-40=JOB;CC41-44=ATIM;CC45-48=Z0;CC49-53=VS;CC54-58=VD;       CLN06720
C       CC59-60=NR;CC61-70=SCAL                                         CLN06730
  130 FORMAT (5A4,3F10.0)                                               CLN06740
C NOTE: CC1-20=RCP(I);CC21-30=XR(I),CC31-40=YR(I);CC41-50=ZR(I)         CLN06750
C                                                                       CLN06760
  150 FORMAT (5A8,2I3)                                                  CLN06770
C NOTE: CC1-40=RUN;CC41-43=NL;CC44-46=NM                                CLN06780
C                                                                       CLN06790
  160 FORMAT (5A4,A2,4F7.0,F8.0,3F4.0)                                  CLN06800
C NOTE: CC1-20=LNK;CC21-22=TYP;CC23-29=XL1(I);CC30-36=YL1(I);           CLN06810
C       CC37-43=XL2(I);CC44-50=YL2(I);CC51-58=VPHL(I);CC59-62=EFL(I);   CLN06820
C       CC63-66=HL(I);CC67-70=WL(I)                                     CLN06830
C                                                                       CLN06840
  170 FORMAT (1X,//,22HPROGRAM RUN TERMINATED,///,                      CLN06850
     * 61H * * LINK LENGTH MUST BE GREATER THAN OR EQUAL TO LINK WIDTH.)CLN06860
C                                                                       CLN06870
  180 FORMAT (1X,//,22HPROGRAM RUN TERMINATED,///,                      CLN06880
     * 46H * * SOURCE MUST BE WITHIN 10 METERS OF DATUM.)               CLN06890
C                                                                       CLN06900
  190 FORMAT (F3.0,F4.0,I1,F6.0,F4.0)                                   CLN06910
C NOTE: CC1-3=U;CC4-7=BRG;CC8=CLAS;CC9-14=MIXH;CC15-18=AMB              CLN06920
C                                                                       CLN06930
C                                                                       CLN06940
C **  OUTPUT FORMAT STATEMENTS  **                                      CLN06950
C                                                                       CLN06960
  200 FORMAT (1H0,28X,                                                  CLN06970
     *    48HCALINE3: CALIFORNIA LINE SOURCE DISPERSION MODEL,          CLN06980
     *    26H - SEPTEMBER, 1979 VERSION,21X,5HPAGE ,I2//)               CLN06990
C                                                                       CLN07000
  210 FORMAT (6X,5HJOB: ,5A8,13X,5HRUN: ,5A8///)                        CLN07010
C                                                                       CLN07020
  220 FORMAT (8X,18HI.  SITE VARIABLES//)                               CLN07030
  230 FORMAT (7X,4HU = ,F4.1,4H M/S,12X,9HCLAS =   ,I1,3H  (,A1,1H),    CLN07040
     *    8X,5HVS = ,F5.1,5H CM/S,7X,7HATIM = ,F4.0,8H MINUTES,         CLN07050
     *    19X,7HMIXH = ,F6.0,2H M)                                      CLN07060
  240 FORMAT (5X,6HBRG = ,F4.0,8H DEGREES,10X,5HZ0 = ,F4.0,3H CM,9X,    CLN07070
     *    5HVD = ,F5.1,5H CM/S,8X,6HAMB = ,F4.1,4H PPM///)              CLN07080
C                                                                       CLN07090
  250 FORMAT (7X,19HII.  LINK VARIABLES//)                              CLN07100
  260 FORMAT (8X,16HLINK DESCRIPTION,5X,                                CLN07110
     *    34H*      LINK COORDINATES (M)      *,1X,11HLINK LENGTH,      CLN07120
     *    2X,8HLINK BRG,3X,4HTYPE,2X,3HVPH,5X,2HEF,5X,1HH,              CLN07130
     *    4X,1HW)                                                       CLN07140
  270 FORMAT (29X,34H*   X1      Y1      X2      Y2   *,5X,3H(M),       CLN07150
     *    7X,5H(DEG),17X,6H(G/MI),2X,3H(M),2X,3H(M))                    CLN07160
  280 FORMAT (4X,25(1H-),1H*,32(1H-),1H*,55(1H-))                       CLN07170
C                                                                       CLN07180
  290 FORMAT (4X,A1,2H. ,5A4,2X,1H*,4(1X,F6.0,1X),1H*,4X,F6.0,          CLN07190
     *    6X,F4.0,6X,A2,2X,F6.0,1X,F5.1,1X,F5.1,2X,F4.1)                CLN07200
C                                                                       CLN07210
C **  NL<2  **                                                          CLN07220
C                                                                       CLN07230
  300 FORMAT (//,6X,42HIII.  RECEPTOR LOCATIONS AND MODEL RESULTS//)    CLN07240
  305 FORMAT (//,7X,41HIV.  MODEL RESULTS (RECEPTOR-LINK MATRIX)//)     CLN07250
  310 FORMAT (29X,37H*        COORDINATES (M)        *  CO)             CLN07260
  320 FORMAT (8X,8HRECEPTOR,13X,                                        CLN07270
     *    39H*      X        Y        Z      * (PPM))                   CLN07280
  330 FORMAT (4X,25(1H-),1H*,31(1H-),1H*,7(1H-))                        CLN07290
C                                                                       CLN07300
C **  MODEL RESULTS  **                                                 CLN07310
  340 FORMAT (4X,I2,2H. ,5A4,1X,1H*,4X,F6.0,3X,F6.0,3X,F6.1,            CLN07320
     *    3X,1H*,F5.1)                                                  CLN07330
C                                                                       CLN07340
C **  NL>1  **                                                          CLN07350
C                                                                       CLN07360
  390 FORMAT (29X,1H*,31X,7H* TOTAL,1X,1H*)                             CLN07370
  400 FORMAT (8X,8HRECEPTOR,13X,                                        CLN07380
     *    41H*      X        Y        Z      * (PPM) *,10(3X,A1,1X))    CLN07390
  420 FORMAT (4X,25(1H-),1H*,31(1H-),1H*,7(1H-),1H*)                    CLN07400
  440 FORMAT (4X,I2,2H. ,5A4,1X,1H*,4X,F6.0,3X,F6.0,3X,F6.1,            CLN07410
     *    3X,1H*,F5.1,2X,1H*,10(1X,F4.1))                               CLN07420
  450 FORMAT (29X,1H*,31X,8H*  TOTAL)                                   CLN07430
  460 FORMAT (29X,24H*        COORDINATES (M),8X,8H*  + AMB)            CLN07440
  470 FORMAT (8X,8HRECEPTOR,13X,26H*      X        Y        Z,          CLN07450
     *    6X,8H*  (PPM))                                                CLN07460
  480 FORMAT (4X,25(1H-),1H*,31(1H-),1H*,8(1H-))                        CLN07470
  490 FORMAT (4X,I2,2H. ,5A4,1X,1H*,4X,F6.0,3X,F6.0,3X,F6.1,            CLN07480
     *    3X,1H*,F6.1)                                                  CLN07490
  500 FORMAT (29X,1H*)                                                  CLN07500
  510 FORMAT (8X,8HRECEPTOR,13X,1H*,20(3X,A1,1X))                       CLN07510
  520 FORMAT (4X,25(1H-),1H*)                                           CLN07520
  530 FORMAT (4X,I2,2H. ,5A4,1X,1H*,20(1X,F4.1))                        CLN07530
C9999 CALL WAUDIT
 9999 CONTINUE                                                          IBM
      STOP                                                              CLN07540
      END                                                               CLN07550
