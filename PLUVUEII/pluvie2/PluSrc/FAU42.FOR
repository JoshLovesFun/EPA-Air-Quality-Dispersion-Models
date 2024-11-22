CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCFAU00010
      SUBROUTINE FAU42(RP,H,Z0,ALPHA,HPBLM,SY,SZ                        FAU00020
     *    ,YCAP,X,Y,DYCAP,DX,DY                                         FAU00030
     *    ,A,C1,C2,B,F,DCWK                                             FAU00040
     *    ,FF,CC,DWK1,DWK2                                              FAU00050
     *    ,N,NSTEP,IOPT,IOUT,IER)                                       FAU00060
C                                                                       FAU00070
C  FAU42.FOR                                                            FAU00080
C                                                                       FAU00090
C FAU42.FR IS CALLED BY PLUVUE.                                         FAU00100
C FAU42.FR CALCULATES PERCEPTIBILTY, DELTA F, ANALAGOUS TO DELTA E      FAU00110
C BUT BASSED ON A MODEL OF VISION WHICH INCLUDES SPATIAL INFORMATION.   FAU00120
C X Y Z TRISIMULUS DATA IS RECIEVED FROM PLUVUE FOR A VIEW THROUGH THE  FAU00130
C PLUME CENTERLINE.  THESE ARE CONVERTED TO FAUGERAS CHANNELS A C1 C2.  FAU00140
C PLUME DENSITY IS INTEGRATED ALONG SEVERAL LINES OF SIGHT CORRES-      FAU00150
C PONDING TO A VERTICAL SCAN OF THE PLUME, THUS FORMING A HIGH          FAU00160
C RESOLUTION IMAGE OF THE PLUME.  A FOURIER TRANSFORM IS APPLIED TO     FAU00170
C OBTAIN THE IMAGE OF THE PLUME IN THE FREQUNCY DOMAIN.                 FAU00180
C THE MODULATION TRANSFER FUNCTIONS  OF THE HUMAN VISUAL SYSTEM         FAU00190
C ARE APPLIED TO EACH CHANNEL A C1 C2. AN INVERSE FOURIER               FAU00200
C TRANSFORM CONVERTS EACH CHANNEL BACK TO THE SPATIAL DOMAIN WHERE      FAU00210
C DELATA F VALUES ARE CALCULATED.                                       FAU00220
C                                                                       FAU00230
C ARGUMENTS:                                                            FAU00240
C              RP - DISTANCE FROM OBSERVER TO PLUME CENTERLINE          FAU00250
C                   IN METERS                                           FAU00260
C               H - HEIGHT OF PLUME ABOVE GROUND AT BASE OF STACK       FAU00270
C                   IN METERS                                           FAU00280
C              Z0 - HEIGHT OF OBSEVER ABOVE GROUND AT BASE OF STACK     FAU00290
C                   IN METERS                                           FAU00300
C           ALPHA - AZIMUTH ANGLE OF INTERSECTION OF LINE OF SIGHT      FAU00310
C              HM - HEIGHT OF TOP OF MIXING LAYER ABOVE GROUND          FAU00320
C                   IN METERS                                           FAU00330
C              SY - SIGMA Y                                             FAU00340
C              SZ - SIGMA Z                                             FAU00350
C                   WITH PLUME CENETER LINE 0 < ALPHA <= 90             FAU00360
C            YCAP - Y TRISTIMULUS OF BACKGROUND SKY                     FAU00370
C               X - X CHROMATICITY OF BACKGROUND SKY                    FAU00380
C               Y - Y CHROMATICITY OF BACKGROUND SKY                    FAU00390
C           YCAPD - DELTA YCAP BACKGROUND MINUS PLUME                   FAU00400
C              XD - DELTA X    BACKGROUND MINUS PLUME                   FAU00410
C              YD - DELTA Y    BACKGROUND MINUS PLUME                   FAU00420
C               A - WORKSPACE FOR GENERATING    A VALUES       LEN=N    FAU00430
C              C1 - WORKSPACE FOR GENERATING   C1 VALUES       LEN=N    FAU00440
C              C2 - WORKSPACE FOR GENERATING   C2 VALUES       LEN=N    FAU00450
C               B - WORKSPACE FOR GENERATING BETA VALUES     LEN=N      FAU00460
C               F - WORKSPACE FOR GENERATING DELTA F VALUES  LEN=N      FAU00470
C            DCWK - DOUBLE PRECISION COMPLEX WORKSPACE                  FAU00480
C                   FOR FOURIER ANALYSIS LEN = N                        FAU00490
C              FF - REAL WORKSPACE FOR HHHMTF LENGTH N                  FAU00500
C              CC - REAL WORKSPACE FOR HHHMTF LENGTH N                  FAU00510
C            DWK1 - DOUBLE PRECISION WORKSPACE FOR HHHMTF LENGTH N      FAU00520
C            DWK2 - DOUBLE PRECISION WORKSPACE FOR HHHMTF LENGTH N      FAU00530
C               N - NUMBER OF VALUES TO GENERATE FOR FOURIER TRANSFORM  FAU00540
C           NSTEP - STEPPING INTERVAL FOR PRINTOUT OF N DATA POINTS     FAU00550
C           IOPT - PRINTOUT CONTROL OPTION:                             FAU00560
C                  -1 = SINGLE CHANNEL PRINTOUTS ONLY                   FAU00570
C                   0 = SINGLE AND COMBINED PRINTOUTS                   FAU00580
C                  +1 = COMBINED PRINTOUTS ONLY                         FAU00590
C           IOUT - FORTRAN CHANNEL NUMBER FOR PRINTED OUTPUT            FAU00600
C            IER - ERROR CODE: 0 = OK                                   FAU00610
C                                                                       FAU00620
C SUBROUTINES REQUIRED: FAUOUT, FAOUT2, FILTR2, HHHMTF, FFTCC,          FAU00630
C                       ICSEVU, ICSCCU, INT1, INT2, ERFC                FAU00640
C                                                                       FAU00650
C          (REV 10/31/83)                                               FAU00660
C                                                                       FAU00670
C          WRITTEN BY : JOHN COLLINS       8/11/83                      FAU00680
C                       ENVIRONMENTAL RESEARCH AND TECHNOLOGY, INC      FAU00690
C                       2625 TOWNSGATE ROAD                             FAU00700
C                       WESTLAKE VILLAGE, CA 91360                      FAU00710
C                                                                       FAU00720
C  PLUVUE II                                                            FAU00730
C  Version 96142   Level 1.0                                            FAU00740
C                                                                       FAU00750
C  U.S. Environmental Protection Agency                                 FAU00760
C  Office of Air Quality Planning and Standards                         FAU00770
C  Research Triangle Park, North Carolina                               FAU00780
C                                                                       FAU00790
C  Modifications:                                                       FAU00800
C                                                                       FAU00810
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCFAU00820
C                                                                       FAU00830
      SAVE                                                              FAU00840
      CHARACTER*2 ITA,ITC1,ITC2,ITF                                     FAU00845
      REAL L,LR,M,MR                                                    FAU00850
      REAL FAUM1(3,3)                                                   FAU00860
      REAL A(N) ,C1(N) ,C2(N),B(N),F(N),FF(N),CC(N)                     FAU00870
      DOUBLE PRECISION DWK1(N),DWK2(N)                                  FAU00880
C     COMPLEX*16 DCWK(N)                                                FAU00890
C                                                                       FAU00900
C  COMPLEX*16 IS AN EXTENSION OF THE ANSI STANDARD                      FAU00910
C                                                                       FAU00920
      DOUBLE PRECISION DCWK(256,2)                                      FAU00930
C                                                                       FAU00940
      DATA ITA,ITC1,ITC2,ITF/'A ','C1','C2','F '/                       FAU00950
      DATA F1,F2,F3,F4,F5/ 13.831, 8.339, .4294, 64. ,10. /             FAU00960
      DATA FAUM1/ .0945,   1.0222,   -.0980,                            FAU00970
     *           -.5589,   1.5527,   -.002376,                          FAU00980
     *           -.002504, -.008474,  .8402/                            FAU00990
C                                                                       FAU01000
      IER      = 0                                                      FAU01010
      TEST     = 1.E-20                                                 FAU01020
      PI       = 3.14159                                                FAU01030
      ONE      = 1.                                                     FAU01040
      TWOPI    = PI * 2.                                                FAU01050
      R2D      = 360. / TWOPI                                           FAU01060
      D2R      = TWOPI / 360.                                           FAU01070
      BWIDTH   = 10.                                                    FAU01080
C                                                                       FAU01090
C                                                                       FAU01100
C CALCULATE PLUME DATA FROM SKY AND DELTA DATA                          FAU01110
      YCAPR= YCAP+DYCAP                                                 FAU01120
         XR=   X +  DX                                                  FAU01130
         YR=   Y +  DY                                                  FAU01140
C                                                                       FAU01150
C CALCULATE TRISTIMULUS VALUES FROM CHROMATICITIES                      FAU01160
      TXYZ=YCAP/Y                                                       FAU01170
      XCAP=X*TXYZ                                                       FAU01180
      ZCAP=TXYZ-(XCAP+YCAP)                                             FAU01190
C                                                                       FAU01200
      TXYZR=YCAPR/YR                                                    FAU01210
      XCAPR=XR*TXYZR                                                    FAU01220
      ZCAPR=TXYZR-(XCAPR+YCAPR)                                         FAU01230
C                                                                       FAU01240
C CALCULATE FAUGERAS CONE RESPONSE                                      FAU01250
      L = FAUM1(1,1)*XCAP  +FAUM1(2,1)*YCAP  +FAUM1(3,1)*ZCAP           FAU01260
      M = FAUM1(1,2)*XCAP  +FAUM1(2,2)*YCAP  +FAUM1(3,2)*ZCAP           FAU01270
      S = FAUM1(1,3)*XCAP  +FAUM1(2,3)*YCAP  +FAUM1(3,3)*ZCAP           FAU01280
C                                                                       FAU01290
      LR= FAUM1(1,1)*XCAPR +FAUM1(2,1)*YCAPR +FAUM1(3,1)*ZCAPR          FAU01300
      MR= FAUM1(1,2)*XCAPR +FAUM1(2,2)*YCAPR +FAUM1(3,2)*ZCAPR          FAU01310
      SR= FAUM1(1,3)*XCAPR +FAUM1(2,3)*YCAPR +FAUM1(3,3)*ZCAPR          FAU01320
C                                                                       FAU01330
C CALULATE DELTA L M S                                                  FAU01340
      DL=LR-L                                                           FAU01350
      DM=MR-M                                                           FAU01360
      DS=SR-S                                                           FAU01370
C                                                                       FAU01380
C RATIO OF DELTA L M S  TO BACKGROUND L M S                             FAU01390
      RL=  DL/L                                                         FAU01400
      RM=  DM/M                                                         FAU01410
      RS=  DS/S                                                         FAU01420
C                                                                       FAU01430
C ESTABLISH N POINT ARRAY OF FAUGERAS VAULES FOR SCAN ACROSS PLUME      FAU01440
C   STEP 1 - SET UP GEOMETRY                                            FAU01450
      BETA1=ASIN( (H-Z0)/RP )                                           FAU01460
      RP1=RP*COS(BETA1)                                                 FAU01470
      SINA = SIN(ALPHA*D2R)                                             FAU01480
      D = RP1 * SINA                                                    FAU01490
C                                                                       FAU01500
C   STEP 2 - ESTABLISH INTEGRAL OF PLUME DENSITY ALONG LINE OF          FAU01510
C   SIGHT THROUGH THE PLUME CENTER                                      FAU01520
C     CASE A: PLUME ABOVE CAPPING LAYER                                 FAU01530
      IF(H.GE.HPBLM) CALL INT1(D,H,Z0,      SY,SZ,SINA,BETA1,VMID)      FAU01540
C     CASE B: PLUME BELOW CAPPING LAYER                                 FAU01550
      IF(H.LT.HPBLM) CALL INT2(D,H,Z0,HPBLM,SY,SZ,SINA,BETA1,VMID)      FAU01560
C                                                                       FAU01570
C   STEP 3 - GENERATE N POINTS                                          FAU01580
      BETA0 = 0.                                                        FAU01590
      BETA1 = BETA1 * R2D                                               FAU01600
      IF(BETA1 .GT. 5.) BETA0 = BETA1 - 5.                              FAU01610
      DBETA=BWIDTH/FLOAT(N)                                             FAU01620
      DO 10 I=1,N                                                       FAU01630
C       B(I) = BETA0 + DBETA*FLOAT(I)  *** OLD LINE ***                 FAU01640
        B(I) = BETA0 + DBETA*FLOAT(I-1)                                 FAU01650
       BETA = B(I) * D2R                                                FAU01660
        IF(H.GE.HPBLM) CALL INT1(D,H,Z0,      SY,SZ,SINA,BETA,V)        FAU01670
        IF(H.LT.HPBLM) CALL INT2(D,H,Z0,HPBLM,SY,SZ,SINA,BETA,V)        FAU01680
        V=V/VMID                                                        FAU01690
        QL=ALOG10(ONE+V*RL)                                             FAU01700
        QM=ALOG10(ONE+V*RM)                                             FAU01710
        QS=ALOG10(ONE+V*RS)                                             FAU01720
         A(I) =     F1*QL + F2*QM + F3*QS                               FAU01730
        C1(I) = F4*(   QL -    QM )                                     FAU01740
        C2(I) = F5*(   QL -            QS )                             FAU01750
10      CONTINUE                                                        FAU01760
C                                                                       FAU01770
C ADJUST FIRST POINT TO REFLECT PERIODICITY                             FAU01780
       A(1)=(  A(1) +  A(N) ) / 2.                                      FAU01790
      C1(1)=( C1(1) + C1(N) ) / 2.                                      FAU01800
      C2(1)=( C2(1) + C2(N) ) / 2.                                      FAU01810
C                                                                       FAU01820
C       CALCULATE DELTA F                                               FAU01830
      DO 30 I=1,N                                                       FAU01840
        IF( ABS(  A(I) ) .LT. TEST )  A(I)=TEST                         FAU01850
        IF( ABS( C1(I) ) .LT. TEST ) C1(I)=TEST                         FAU01860
        IF( ABS( C2(I) ) .LT. TEST ) C2(I)=TEST                         FAU01870
30      F(I)=SQRT( A(I)*A(I) + C1(I)*C1(I) + C2(I)*C2(I) )              FAU01880
C                                                                       FAU01890
C OUTPUT INFORMATION ON RAW FAUGERAS CHANNELS                           FAU01900
      IF( IOPT.GT.0 ) GOTO 31                                           FAU01910
      WRITE(IOUT,503)                                                   FAU01920
      WRITE(IOUT,504)ITA                                                FAU01930
        CALL FAUOUT(B,A ,N,NSTEP,IOUT)                                  FAU01940
      WRITE(IOUT,503)                                                   FAU01950
      WRITE(IOUT,504)ITC1                                               FAU01960
        CALL FAUOUT(B,C1,N,NSTEP,IOUT)                                  FAU01970
      WRITE(IOUT,503)                                                   FAU01980
      WRITE(IOUT,504)ITC2                                               FAU01990
        CALL FAUOUT(B,C2,N,NSTEP,IOUT)                                  FAU02000
      WRITE(IOUT,503)                                                   FAU02010
      WRITE(IOUT,504)ITF                                                FAU02020
        CALL FAUOUT(B,F ,N,NSTEP,IOUT)                                  FAU02030
31    IF( IOPT.LT.0 ) GOTO 32                                           FAU02040
      WRITE(IOUT,503)                                                   FAU02050
      WRITE(IOUT,508)                                                   FAU02060
        CALL FAOUT2(B,A,C1,C2,F,N,NSTEP,IOUT)                           FAU02070
32    CONTINUE                                                          FAU02080
C                                                                       FAU02090
C APPLY SPATIAL FILTERING ( FT MTF INVFT ) TO EACH CHANNEL              FAU02100
      NCH=1                                                             FAU02110
      CALL FILTR2 (A ,N,BWIDTH,NCH,FF,CC,DWK1,DWK2,DCWK,IOUT,IER)       FAU02120
      IF(IER.NE.0) GOTO 992                                             FAU02130
      NCH=2                                                             FAU02140
      CALL FILTR2 (C1,N,BWIDTH,NCH,FF,CC,DWK1,DWK2,DCWK,IOUT,IER)       FAU02150
      IF(IER.NE.0) GOTO 992                                             FAU02160
      NCH=3                                                             FAU02170
      CALL FILTR2 (C2,N,BWIDTH,NCH,FF,CC,DWK1,DWK2,DCWK,IOUT,IER)       FAU02180
      IF(IER.NE.0) GOTO 992                                             FAU02190
C                                                                       FAU02200
C CALCULATE DELTA F FOR FILTERED RESPONSE                               FAU02210
      DO 100 I=1,N                                                      FAU02220
100    F(I)= SQRT( A(I)*A(I) + C1(I)*C1(I) + C2(I)*C2(I) )              FAU02230
C                                                                       FAU02240
C OUTPUT INFORMATION ON FILTERED FAUGERAS CHANNELS                      FAU02250
      IF( IOPT.GT.0 ) GOTO 101                                          FAU02260
      WRITE(IOUT,505)                                                   FAU02270
      WRITE(IOUT,506)ITA                                                FAU02280
        CALL FAUOUT(B,A ,N,NSTEP,IOUT)                                  FAU02290
      WRITE(IOUT,505)                                                   FAU02300
      WRITE(IOUT,506)ITC1                                               FAU02310
        CALL FAUOUT(B,C1,N,NSTEP,IOUT)                                  FAU02320
      WRITE(IOUT,505)                                                   FAU02330
      WRITE(IOUT,506)ITC2                                               FAU02340
        CALL FAUOUT(B,C2,N,NSTEP,IOUT)                                  FAU02350
      WRITE(IOUT,505)                                                   FAU02360
      WRITE(IOUT,506)ITF                                                FAU02370
        CALL FAUOUT(B,F ,N,NSTEP,IOUT)                                  FAU02380
101    IF( IOPT.LT.0 ) GOTO 102                                         FAU02390
      WRITE(IOUT,505)                                                   FAU02400
      WRITE(IOUT,508)                                                   FAU02410
        CALL FAOUT2(B,A,C1,C2,F,N,NSTEP,IOUT)                           FAU02420
102    CONTINUE                                                         FAU02430
C                                                                       FAU02440
      GOTO 999                                                          FAU02450
992   IER=3                                                             FAU02460
      WRITE(IOUT,507)                                                   FAU02470
999   RETURN                                                            FAU02480
503   FORMAT(1H1,                                                       FAU02490
     1       20X,51HVISUAL EFFECTS FOR ARRAY OF SIGHTPATHS USING       /FAU02500
     2       20X,51HFAUGERAS MODEL OF HUMAN VISUAL SYSTEM (UNFILTERED) /FAU02510
     3       20X,51HFOR GEOMETRY SEE LAST PAGE OF REGULAR PLUVUE OUTPUT/FAU02520
     4       / /)                                                       FAU02530
504   FORMAT(1X,13H  ELEV. ANGLE,11H     DELTA ,A2/)                    FAU02540
505   FORMAT(1H1,                                                       FAU02550
     1       20X,51HVISUAL EFFECTS FOR ARRAY OF SIGHTPATHS USING       /FAU02560
     2       20X,51HFAUGERAS MODEL OF HUMAN VISUAL SYSTEM (FILTERED)   /FAU02570
     3       20X,51HFOR GEOMETRY SEE LAST PAGE OF REGULAR PLUVUE OUTPUT/FAU02580
     4        / / )                                                     FAU02590
506   FORMAT(1X,13H  ELEV. ANGLE,11H     DELTA ,A2/)                    FAU02600
507   FORMAT(1X,42HERROR FROM FAU3.  ERROR RETURNED BY FILTER)          FAU02610
508   FORMAT(1X,13H  ELEV. ANGLE,13H     DELTA A ,                      FAU02620
     *          13H     DELTA C1,13H     DELTA C2,                      FAU02630
     *          13H     DELTA F /)                                      FAU02640
      END                                                               FAU02650
