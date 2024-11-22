CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCSZC00010
      SUBROUTINE SZCOEF(XKM,KST,A,B,XMIN,XMAX)                          SZC00020
C                                                                       SZC00030
C  SZCOEF.FOR                                                           SZC00040
C                                                                       SZC00050
C  DETERMINES COEFFICIENTS AND RANGES FOR RURAL SIGMA-Z                 SZC00060
C                                                                       SZC00070
C                 SZCOEF Module of ISC2 Model                           SZC00080
C                                                                       SZC00090
C        PROGRAMMER: Roger Brode, Jeff Wang                             SZC00100
C                                                                       SZC00110
C        DATE:    March 2, 1992                                         SZC00120
C                                                                       SZC00130
C        INPUTS:  KST     Stability Category                            SZC00140
C                 XKM     Downwind Distance (km)                        SZC00150
C                                                                       SZC00160
C        OUTPUTS: Coefficients A and B and Distance Range XMIN and XMAX SZC00170
C                                                                       SZC00180
C  PLUVUE II                                                            SZC00190
C  Version 92243   Level 1.0                                            SZC00200
C                                                                       SZC00210
C  U.S. Environmental Protection Agency                                 SZC00220
C  Office of Air Quality Planning and Standards                         SZC00230
C  Research Triangle Park, North Carolina                               SZC00240
C                                                                       SZC00250
C  Modifications:                                                       SZC00260
C                                                                       SZC00270
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCSZC00280
      IF (KST .EQ. 4) THEN                                              SZC00290
         IF (XKM .LE. .30) THEN                                         SZC00300
            A = 34.459                                                  SZC00310
            B = 0.86974                                                 SZC00320
            XMIN = 0.                                                   SZC00330
            XMAX = 0.30                                                 SZC00340
         ELSE IF (XKM .LE. 1.0) THEN                                    SZC00350
            A = 32.093                                                  SZC00360
            B = 0.81066                                                 SZC00370
            XMIN = 0.30                                                 SZC00380
            XMAX = 1.                                                   SZC00390
         ELSE IF (XKM .LE. 3.0) THEN                                    SZC00400
            A = 32.093                                                  SZC00410
            B = 0.64403                                                 SZC00420
            XMIN = 1.                                                   SZC00430
            XMAX = 3.                                                   SZC00440
         ELSE IF (XKM .LE. 10.) THEN                                    SZC00450
            A = 33.504                                                  SZC00460
            B = 0.60486                                                 SZC00470
            XMIN = 3.                                                   SZC00480
            XMAX = 10.                                                  SZC00490
         ELSE IF (XKM .LE. 30.) THEN                                    SZC00500
            A = 36.650                                                  SZC00510
            B = 0.56589                                                 SZC00520
            XMIN = 10.                                                  SZC00530
            XMAX = 30.                                                  SZC00540
         ELSE                                                           SZC00550
            A = 44.053                                                  SZC00560
            B = 0.51179                                                 SZC00570
            XMIN = 30.                                                  SZC00580
            XMAX = 100.                                                 SZC00590
         END IF                                                         SZC00600
C                                                                       SZC00610
      ELSE IF (KST .EQ. 5) THEN                                         SZC00620
         IF (XKM .LE. .10) THEN                                         SZC00630
            A = 24.26                                                   SZC00640
            B = 0.83660                                                 SZC00650
            XMIN = 0.                                                   SZC00660
            XMAX = .10                                                  SZC00670
         ELSE IF (XKM .LE. .30) THEN                                    SZC00680
            A = 23.331                                                  SZC00690
            B = 0.81956                                                 SZC00700
            XMIN = 0.10                                                 SZC00710
            XMAX = 0.30                                                 SZC00720
         ELSE IF (XKM .LE. 1.0) THEN                                    SZC00730
            A = 21.628                                                  SZC00740
            B = 0.75660                                                 SZC00750
            XMIN = 0.30                                                 SZC00760
            XMAX = 1.                                                   SZC00770
         ELSE IF (XKM .LE. 2.0) THEN                                    SZC00780
            A = 21.628                                                  SZC00790
            B = 0.63077                                                 SZC00800
            XMIN = 1.                                                   SZC00810
            XMAX = 2.                                                   SZC00820
         ELSE IF (XKM .LE. 4.0) THEN                                    SZC00830
            A = 22.534                                                  SZC00840
            B = 0.57154                                                 SZC00850
            XMIN = 2.                                                   SZC00860
            XMAX = 4.                                                   SZC00870
         ELSE IF (XKM .LE. 10.) THEN                                    SZC00880
            A = 24.703                                                  SZC00890
            B = 0.50527                                                 SZC00900
            XMIN = 4.                                                   SZC00910
            XMAX = 10.                                                  SZC00920
         ELSE IF (XKM .LE. 20.) THEN                                    SZC00930
            A = 26.97                                                   SZC00940
            B = 0.46713                                                 SZC00950
            XMIN = 10.                                                  SZC00960
            XMAX = 20.                                                  SZC00970
         ELSE IF (XKM .LE. 40.) THEN                                    SZC00980
            A = 35.42                                                   SZC00990
            B = 0.37615                                                 SZC01000
            XMIN = 20.                                                  SZC01010
            XMAX = 40.                                                  SZC01020
         ELSE                                                           SZC01030
            A = 47.618                                                  SZC01040
            B = 0.29592                                                 SZC01050
            XMIN = 40.                                                  SZC01060
            XMAX = 100.                                                 SZC01070
         END IF                                                         SZC01080
C                                                                       SZC01090
      ELSE IF (KST .EQ. 6) THEN                                         SZC01100
         IF (XKM .LE. .20) THEN                                         SZC01110
            A = 15.209                                                  SZC01120
            B = 0.81558                                                 SZC01130
            XMIN = 0.                                                   SZC01140
            XMAX = 0.20                                                 SZC01150
         ELSE IF (XKM .LE. .70) THEN                                    SZC01160
            A = 14.457                                                  SZC01170
            B = 0.78407                                                 SZC01180
            XMIN = 0.20                                                 SZC01190
            XMAX = 0.70                                                 SZC01200
         ELSE IF (XKM .LE. 1.0) THEN                                    SZC01210
            A = 13.953                                                  SZC01220
            B = 0.68465                                                 SZC01230
            XMIN = 0.7                                                  SZC01240
            XMAX = 1.                                                   SZC01250
         ELSE IF (XKM .LE. 2.0) THEN                                    SZC01260
            A = 13.953                                                  SZC01270
            B = 0.63227                                                 SZC01280
            XMIN = 1.                                                   SZC01290
            XMAX = 2.                                                   SZC01300
         ELSE IF (XKM .LE. 3.0) THEN                                    SZC01310
            A = 14.823                                                  SZC01320
            B = 0.54503                                                 SZC01330
            XMIN = 2.                                                   SZC01340
            XMAX = 3.                                                   SZC01350
         ELSE IF (XKM .LE. 7.0) THEN                                    SZC01360
            A = 16.187                                                  SZC01370
            B = 0.46490                                                 SZC01380
            XMIN = 3.                                                   SZC01390
            XMAX = 7.                                                   SZC01400
         ELSE IF (XKM .LE. 15.) THEN                                    SZC01410
            A = 17.836                                                  SZC01420
            B = 0.41507                                                 SZC01430
            XMIN = 7.                                                   SZC01440
            XMAX = 15.                                                  SZC01450
         ELSE IF (XKM .LE. 30.) THEN                                    SZC01460
            A = 22.651                                                  SZC01470
            B = 0.32681                                                 SZC01480
            XMIN = 15.                                                  SZC01490
            XMAX = 30.                                                  SZC01500
         ELSE IF (XKM .LE. 60.) THEN                                    SZC01510
            A = 27.074                                                  SZC01520
            B = 0.27436                                                 SZC01530
            XMIN = 30.                                                  SZC01540
            XMAX = 60.                                                  SZC01550
         ELSE                                                           SZC01560
            A = 34.219                                                  SZC01570
            B = 0.21716                                                 SZC01580
            XMIN = 60.                                                  SZC01590
            XMAX = 100.                                                 SZC01600
         END IF                                                         SZC01610
C                                                                       SZC01620
      ELSE IF (KST .EQ. 1) THEN                                         SZC01630
         IF (XKM .LE. 0.10) THEN                                        SZC01640
            A = 122.8                                                   SZC01650
            B = 0.94470                                                 SZC01660
            XMIN = 0.                                                   SZC01670
            XMAX = 0.1                                                  SZC01680
         ELSE IF (XKM .LE. 0.15) THEN                                   SZC01690
            A = 158.080                                                 SZC01700
            B = 1.05420                                                 SZC01710
            XMIN = 0.1                                                  SZC01720
            XMAX = 0.15                                                 SZC01730
         ELSE IF (XKM .LE. 0.20) THEN                                   SZC01740
            A = 170.22                                                  SZC01750
            B = 1.09320                                                 SZC01760
            XMIN = 0.15                                                 SZC01770
            XMAX = 0.20                                                 SZC01780
         ELSE IF (XKM .LE. 0.25) THEN                                   SZC01790
            A = 179.52                                                  SZC01800
            B = 1.12620                                                 SZC01810
            XMIN = 0.20                                                 SZC01820
            XMAX = 0.25                                                 SZC01830
         ELSE IF (XKM .LE. 0.30) THEN                                   SZC01840
            A = 217.41                                                  SZC01850
            B = 1.2644                                                  SZC01860
            XMIN = 0.25                                                 SZC01870
            XMAX = 0.30                                                 SZC01880
         ELSE IF (XKM .LE. 0.40) THEN                                   SZC01890
            A = 258.89                                                  SZC01900
            B = 1.4094                                                  SZC01910
            XMIN = 0.30                                                 SZC01920
            XMAX = 0.40                                                 SZC01930
         ELSE IF (XKM .LE. 0.50) THEN                                   SZC01940
            A = 346.75                                                  SZC01950
            B = 1.72830                                                 SZC01960
            XMIN = 0.40                                                 SZC01970
            XMAX = 0.50                                                 SZC01980
         ELSE                                                           SZC01990
            A = 453.85                                                  SZC02000
            B = 2.11660                                                 SZC02010
            XMIN = 0.50                                                 SZC02020
            XMAX = 100.                                                 SZC02030
         END IF                                                         SZC02040
C                                                                       SZC02050
      ELSE IF (KST .EQ. 2) THEN                                         SZC02060
         IF (XKM .LE. 0.20) THEN                                        SZC02070
            A = 90.673                                                  SZC02080
            B = 0.93198                                                 SZC02090
            XMIN = 0.                                                   SZC02100
            XMAX = 0.20                                                 SZC02110
         ELSE IF (XKM .LE. 0.40) THEN                                   SZC02120
            A = 98.483                                                  SZC02130
            B = 0.98332                                                 SZC02140
            XMIN = 0.20                                                 SZC02150
            XMAX = 0.40                                                 SZC02160
         ELSE                                                           SZC02170
            A = 109.3                                                   SZC02180
            B = 1.0971                                                  SZC02190
            XMIN = 0.40                                                 SZC02200
            XMAX = 100.                                                 SZC02210
         END IF                                                         SZC02220
C                                                                       SZC02230
      ELSE IF (KST .EQ. 3) THEN                                         SZC02240
            A = 61.141                                                  SZC02250
            B = 0.91465                                                 SZC02260
            XMIN = 0.                                                   SZC02270
            XMAX = 100.                                                 SZC02280
      END IF                                                            SZC02290
C                                                                       SZC02300
      RETURN                                                            SZC02310
      END                                                               SZC02320
