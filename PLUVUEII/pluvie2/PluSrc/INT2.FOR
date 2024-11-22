CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCIT200010
      SUBROUTINE INT2(D,H,Z0,HPBLM,SY,SZ,SINA,BETA,V)                   IT200020
C                                                                       IT200030
C  INT2.FOR                                                             IT200040
C                                                                       IT200050
C  INT2.FR IS CALLED BY FAU42 CALLED BY PLUVUE.                         IT200060
C  INT2.FR EVALUATES THE INTEGRAL OF MASS CONCENTRATION ALONG A LINE    IT200070
C  OF SIGHT INTERSECTING THE GAUSSIAN PLUME, INCLUDING SINGLE           IT200080
C  REFLECTON FROM THE GROUND. SCALING FACTORS INCLUDING 2,PI,SIGMA Y,   IT200090
C  SIGMA Z, AND CHI/Q, WHICH DO NOT CHANGE ARE NOT INCLUDED IN THE      IT200100
C  EVALUATED VALUE SINCE THE RESULT IS ONLY USED PROPORTIONALY BY FAU3. IT200110
C  ARGUMENTS:                                                           IT200120
C         D - PERPENDICULAR DISTANCE FROM OBSERVER TO PLUME CENTER      IT200130
C         H - HEIGHT OF PLUME CENTERLINE ABOVE GROUND IN METERS         IT200140
C        Z0 - HEIGHT OF OBSERVER         ABOVE GROUND IN METERS         IT200150
C     HPBLM - SIGMA Y OF PLUME                                          IT200160
C        SY - SIGMA Y OF PLUME                                          IT200170
C        SZ - SIGMA Z OF PLUME                                          IT200180
C      SINA - SIN OF ALPHA THE                                          IT200190
C             AZIMUTH ANGLE OF INTERSECTION OF LINE OF SIGHT WITH       IT200200
C             PLUME CENTERLINE.   0 < ALPHA =< 90                       IT200210
C      BETA - ELVATION ANGLE OF LINE OF SITE IN RADIANS                 IT200220
C         V - RETURNED VALUE OF INTEGRAL ALONG LINE OF SIGHT            IT200230
C                                                                       IT200240
C  SUBROUTINES REQUIRED: ERFC                                           IT200250
C                                                                       IT200260
C                       (REV 8/ 7/83)                                   IT200270
C                                                                       IT200280
C          WRITTEN BY : JOHN COLLINS       8/22/83                      IT200290
C                       ENVIRONMENTAL RESEARCH AND TECHNOLOGY, INC      IT200300
C                       2625 TOWNSGATE ROAD                             IT200310
C                       WESTLAKE VILLAGE, CA 91360                      IT200320
C                                                                       IT200330
C  PLUVUE II                                                            IT200340
C  Version 92243   Level 1.0                                            IT200350
C                                                                       IT200360
C  U.S. Environmental Protection Agency                                 IT200370
C  Office of Air Quality Planning and Standards                         IT200380
C  Research Triangle Park, North Carolina                               IT200390
C                                                                       IT200400
C  Modifications:                                                       IT200410
C                                                                       IT200420
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCIT200430
      SAVE                                                              IT200440
      REAL K1,K2,K3,K4,K5,L1                                            IT200450
      EXTERNAL ERFC                                                     IT200460
C                                                                       IT200470
C CALCULATE GAMMA THE ANGLE OF ELVATION OF THE LINE OF SIGHT            IT200480
C IN A PLANE PERPENDICULAR TO THE PLUME.                                IT200490
      GAMMA=ATAN( TAN(BETA)/SINA )                                      IT200500
      SING = SIN( GAMMA )                                               IT200510
C*ERT* CHECK AND CORRECT FOR ZERO ELEVATION ANGLE                       IT200520
      IF(SING.LT.1.0E-30) SING = 1.0E-30                                IT200530
C*ERT                                                                   IT200540
      COSG = COS( GAMMA )                                               IT200550
C                                                                       IT200560
C CALCULATE DISTANCE TO INTERSECTION OF LINE OF SIGHT WITH CAPPING LAYERIT200570
      L1=HPBLM/SING                                                     IT200580
C                                                                       IT200590
C SET H0= HEIGHT OF PLUME ABOVE OBSEVER                                 IT200600
      H0=H-Z0                                                           IT200610
C                                                                       IT200620
C EVALUATE INTEGRAL FOR PLUME                                           IT200630
      P1=COSG/SY                                                        IT200640
      P2=   D/SY                                                        IT200650
      P3=SING/SZ                                                        IT200660
      P4= -H0/SZ                                                        IT200670
C                                                                       IT200680
      K1=SQRT( ( P1*P1 + P3*P3 ) / 2. )                                 IT200690
      K2=      ( P3*P4 - P1*P2 )                                        IT200700
      K3=      ( P2*P2 + P4*P4 ) / 2.                                   IT200710
      K4= K2/(K1*2.)                                                    IT200720
      K5= K1*(L1+K4/K1)                                                 IT200730
C                                                                       IT200740
      V1= (  EXP(K4*K4 - K3) * ( ERFC(K4)-ERFC(K5) )  ) / K1            IT200750
C                                                                       IT200760
C                                                                       IT200770
C SET H0= HEIGHT OF PLUME REFLECTED FROM GROUND                         IT200780
      H0=  -H-Z0                                                        IT200790
C                                                                       IT200800
C EVALUATE INTEGRAL FOR REFLECTED PLUME                                 IT200810
      P4= -H0/SZ                                                        IT200820
      K2=      ( P3*P4 - P1*P2 )                                        IT200830
      K3=      ( P2*P2 + P4*P4 ) / 2.                                   IT200840
      K4= K2/(K1*2.)                                                    IT200850
      K5= K1*(L1+K4/K1)                                                 IT200860
C                                                                       IT200870
      V2= (  EXP(K4*K4 - K3) * ( ERFC(K4)-ERFC(K5) )  ) / K1            IT200880
C                                                                       IT200890
C                                                                       IT200900
C SET H0= HEIGHT OF PLUME REFLECTED FROM CAPPING LAYER                  IT200910
      H0= (2*HPBLM - H) -Z0                                             IT200920
C                                                                       IT200930
C EVALUATE INTEGRAL FOR REFLECTED PLUME                                 IT200940
      P4= -H0/SZ                                                        IT200950
      K2=      ( P3*P4 - P1*P2 )                                        IT200960
      K3=      ( P2*P2 + P4*P4 ) / 2.                                   IT200970
      K4= K2/(K1*2.)                                                    IT200980
      K5= K1*(L1+K4/K1)                                                 IT200990
C                                                                       IT201000
      V3= (  EXP(K4*K4 - K3) * ( ERFC(K4)-ERFC(K5) )  ) / K1            IT201010
C                                                                       IT201020
C                                                                       IT201030
C SET H0= HEIGHT OF PLUME REFLECTED FROM CAPPING LAYER REFLECTED        IT201040
C         AGAIN FROM GROUND                                             IT201050
      H0= -(2*HPBLM - H) -Z0                                            IT201060
C                                                                       IT201070
C EVALUATE INTEGRAL FOR REFLECTED PLUME                                 IT201080
      P4= -H0/SZ                                                        IT201090
      K2=      ( P3*P4 - P1*P2 )                                        IT201100
      K3=      ( P2*P2 + P4*P4 ) / 2.                                   IT201110
      K4= K2/(K1*2.)                                                    IT201120
      K5= K1*(L1+K4/K1)                                                 IT201130
C                                                                       IT201140
      V4= (  EXP(K4*K4 - K3) * ( ERFC(K4)-ERFC(K5) )  ) / K1            IT201150
C                                                                       IT201160
C SUM UP CONTRIBUTIONS OF REAL AND VIRTUAL SOURCES                      IT201170
      V= V1 + V2 + V3 + V4                                              IT201180
C                                                                       IT201190
      RETURN                                                            IT201200
      END                                                               IT201210
