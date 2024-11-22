CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCIT100010
      SUBROUTINE INT1(D,H,Z0,SY,SZ,SINA,BETA,V)                         IT100020
C                                                                       IT100030
C  INT1.FOR                                                             IT100040
C                                                                       IT100050
C  INT1.FR IS CALLED BY FAU3 CALLED BY PLUVUE.                          IT100060
C  INT1.FR EVALUATES THE INTEGRAL OF MASS CONCENTRATION ALONG A LINE    IT100070
C  OF SIGHT INTERSECTING THE GAUSSIAN PLUME, INCLUDING SINGLE           IT100080
C  REFLECTON FROM THE GROUND. SCALING FACTORS INCLUDING 2,PI,SIGMA Y,   IT100090
C  SIGMA Z, AND CHI/Q, WHICH DO NOT CHANGE ARE NOT INCLUDED IN THE      IT100100
C  EVALUATED VALUE SINCE THE RESULT IS ONLY USED PROPORTIONALY BY FAU3. IT100110
C                                                                       IT100120
C ARGUMENTS:                                                            IT100130
C         D - PERPENDICULAR DISTANCE FROM OBSERVER TO PLUME CENTER      IT100140
C         H - HEIGHT OF PLUME CENTERLINE ABOVE GROUND IN METERS         IT100150
C        Z0 - HEIGHT OF OBSERVER         ABOVE GROUND IN METERS         IT100160
C        SY - SIGMA Y OF PLUME                                          IT100170
C        SZ - SIGMA Z OF PLUME                                          IT100180
C      SINA - SIN OF ALPHA THE                                          IT100190
C             AZIMUTH ANGLE OF INTERSECTION OF LINE OF SIGHT WITH       IT100200
C             PLUME CENTERLINE.   0 < ALPHA =< 90                       IT100210
C      BETA - ELVATION ANGLE OF LINE OF SITE IN RADIANS                 IT100220
C         V - RETURNED VALUE OF INTEGRAL ALONG LINE OF SIGHT            IT100230
C                                                                       IT100240
C SUBROUTINES REQUIRED: ERFC                                            IT100250
C                                                                       IT100260
C                           (REV 8/ 7/83)                               IT100270
C                                                                       IT100280
C          WRITTEN BY : JOHN COLLINS       8/ 7/83                      IT100290
C                       ENVIRONMENTAL RESEARCH AND TECHNOLOGY, INC      IT100300
C                       2625 TOWNSGATE ROAD                             IT100310
C                       WESTLAKE VILLAGE, CA 91360                      IT100320
C                                                                       IT100330
C  PLUVUE II                                                            IT100340
C  Version 92243   Level 1.0                                            IT100350
C                                                                       IT100360
C  U.S. Environmental Protection Agency                                 IT100370
C  Office of Air Quality Planning and Standards                         IT100380
C  Research Triangle Park, North Carolina                               IT100390
C                                                                       IT100400
C  Modifications:                                                       IT100410
C                                                                       IT100420
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCIT100430
C                                                                       IT100440
      SAVE                                                              IT100450
      REAL K1,K2,K3,K4                                                  IT100460
      EXTERNAL ERFC                                                     IT100470
C                                                                       IT100480
C CALCULATE GAMMA THE ANGLE OF ELVATION OF THE LINE OF SIGHT            IT100490
C IN A PLANE PERPENDICULAR TO THE PLUME.                                IT100500
      GAMMA=ATAN( TAN(BETA)/SINA )                                      IT100510
      SING = SIN( GAMMA )                                               IT100520
      COSG = COS( GAMMA )                                               IT100530
                                                                        IT100540
C SET H0= HEIGHT OF PLUME ABOVE OBSEVER                                 IT100550
      H0=H-Z0                                                           IT100560
C                                                                       IT100570
C EVALUATE INTEGRAL FOR PLUME                                           IT100580
      P1=COSG/SY                                                        IT100590
      P2=   D/SY                                                        IT100600
      P3=SING/SZ                                                        IT100610
      P4= -H0/SZ                                                        IT100620
C                                                                       IT100630
      K1=SQRT( ( P1*P1 + P3*P3 ) / 2. )                                 IT100640
      K2=      ( P3*P4 - P1*P2 )                                        IT100650
      K3=      ( P2*P2 + P4*P4 ) / 2.                                   IT100660
      K4= K2/(K1*2.)                                                    IT100670
C                                                                       IT100680
      V1= ( EXP(K4*K4 - K3) * ERFC(K4) ) / K1                           IT100690
C                                                                       IT100700
C SET H0= HEIGHT OF REFLECTED PLUME ABOVE OBSEVER                       IT100710
      H0=  -H-Z0                                                        IT100720
C                                                                       IT100730
C EVALUATE INTEGRAL FOR REFLECTED PLUME                                 IT100740
      P4= -H0/SZ                                                        IT100750
      K2=      ( P3*P4 - P1*P2 )                                        IT100760
      K3=      ( P2*P2 + P4*P4 ) / 2.                                   IT100770
      K4= K2/(K1*2.)                                                    IT100780
C                                                                       IT100790
      V2= ( EXP(K4*K4 - K3) * ERFC(K4) ) / K1                           IT100800
C                                                                       IT100810
      V= V1 + V2                                                        IT100820
*                                                                       IT100830
      RETURN                                                            IT100840
      END                                                               IT100850
