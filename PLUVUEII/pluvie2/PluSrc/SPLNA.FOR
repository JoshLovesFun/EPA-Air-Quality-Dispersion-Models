CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCSPN00010
      SUBROUTINE SPLNA (N,X,Y,J,D,C,W)                                  SPN00020
C                                                                       SPN00030
C  SPLNA.FOR                                                            SPN00040
C                                                                       SPN00050
C  OVER THE INTERVAL X(I) TO X(I+1), THE INTERPOLATING POLYNOMIAL       SPN00060
C                  Y=Y(I)+A(I)*Z+B(I)*Z**2+E(I)*Z**3                    SPN00070
C  WHERE Z=(X-X(I))/(X(I+1)-X(I))                                       SPN00080
C  IS USED. THE COEFFICIENTS A(I),B(I) AND E(I) ARE COMPUTED            SPN00090
C  BY SPLNA AND STORED IN LOCATIONS C(3*I-2),C(3*I-1)                   SPN00100
C  AND C(3*I) RESPECTIVELY.                                             SPN00110
C  WHILE WORKING IN THE ITH INTERVAL,THE VARIABLE Q WILL                SPN00120
C  REPRESENT Q=X(I+1) - X(I), AND Y(I) WILL REPRESENT Y(I+1)-Y(I)       SPN00130
C                                                                       SPN00140
C  PLUVUE II                                                            SPN00150
C  Version 92243   Level 1.0                                            SPN00160
C                                                                       SPN00170
C  U.S. Environmental Protection Agency                                 SPN00180
C  Office of Air Quality Planning and Standards                         SPN00190
C  Research Triangle Park, North Carolina                               SPN00200
C                                                                       SPN00210
C  Modifications:                                                       SPN00220
C                                                                       SPN00230
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCSPN00240
      SAVE                                                              SPN00250
      DIMENSION X(10), Y(10), D(2), C(30), W(30)                        SPN00260
C                                                                       SPN00270
      Q=X(2)-X(1)                                                       SPN00280
      YI=Y(2)-Y(1)                                                      SPN00290
      IF (J.EQ.2) GO TO 5                                               SPN00300
C     ------------------------------------------------------------------SPN00310
C             IF THE FIRST DERIVATIVE AT THE END POINTS IS GIVEN,       SPN00320
C             A(1) IS KNOWN, AND THE SECOND EQUATION BECOMES            SPN00330
C             MERELY B(1)+E(1)=YI - Q*D(1).                             SPN00340
C     ------------------------------------------------------------------SPN00350
      C(1)=Q*D(1)                                                       SPN00360
      C(2)=1.0                                                          SPN00370
      W(2)=YI-C(1)                                                      SPN00380
      GO TO 10                                                          SPN00390
C     ------------------------------------------------------------------SPN00400
C             IF THE SECOND DERIVATIVE AT THE END POINTS IS GIVEN       SPN00410
C             B(1) IS KNOWN, THE SECOND EQUATION BECOMES                SPN00420
C             A(1)+E(1)=YI-0.5*Q*Q*D(1). DURING THE SOLUTION OF         SPN00430
C             THE 3N-4 EQUATIONS,A1 WILL BE KEPT IN CELL C(2)           SPN00440
C             INSTEAD OF C(1) TO RETAIN THE TRIDIAGONAL FORM OF THE     SPN00450
C             COEFFICIENT MATRIX.                                       SPN00460
C     ------------------------------------------------------------------SPN00470
    5 C(2)=0.0                                                          SPN00480
      W(2)=0.5*Q*Q*D(1)                                                 SPN00490
   10 M=N-2                                                             SPN00500
      IF (M.LE.0) GO TO 20                                              SPN00510
C     ------------------------------------------------------------------SPN00520
C             UPPER TRIANGULARIZATION OF THE TRIDIAGONAL SYSTEM OF      SPN00530
C             EQUATIONS FOR THE COEFFICIENT MATRIX FOLLOWS--            SPN00540
C     ------------------------------------------------------------------SPN00550
      DO 15 I=1,M                                                       SPN00560
      AI=Q                                                              SPN00570
      Q=X(I+2)-X(I+1)                                                   SPN00580
      H=AI/Q                                                            SPN00590
      C(3*I)=-H/(2.0-C(3*I-1))                                          SPN00600
      W(3*I)=(-YI-W(3*I-1))/(2.0-C(3*I-1))                              SPN00610
      C(3*I+1)=-H*H/(H-C(3*I))                                          SPN00620
      W(3*I+1)=(YI-W(3*I))/(H-C(3*I))                                   SPN00630
      YI=Y(I+2)-Y(I+1)                                                  SPN00640
      C(3*I+2)=1.0/(1.0-C(3*I+1))                                       SPN00650
   15 W(3*I+2)=(YI-W(3*I+1))/(1.0-C(3*I+1))                             SPN00660
C     ------------------------------------------------------------------SPN00670
C             E(N-1) IS DETERMINED DIRECTLY FROM THE LAST EQUATION      SPN00680
C             OBTAINED ABOVE, AND THE FIRST OR SECOND DERIVATIVE        SPN00690
C             VALUE GIVEN AT THE END POINT.                             SPN00700
C     ------------------------------------------------------------------SPN00710
   20 IF (J.EQ.1) GO TO 25                                              SPN00720
      C(3*N-3)=(Q*Q*D(2)/2.0-W(3*N-4))/(3.0-C(3*N-4))                   SPN00730
      GO TO 30                                                          SPN00740
   25 C(3*N-3)=(Q*D(2)-YI-W(3*N-4))/(2.0-C(3*N-4))                      SPN00750
   30 M=3*N-6                                                           SPN00760
      IF (M.LE.0) GO TO 40                                              SPN00770
C     ------------------------------------------------------------------SPN00780
C             BACK SOLUTION FOR ALL COEFFICENTS EXCEPT                  SPN00790
C             A(1) AND B(1) FOLLOWS--                                   SPN00800
C     ------------------------------------------------------------------SPN00810
      DO 35 II=1,M                                                      SPN00820
      I=M-II+3                                                          SPN00830
   35 C(I)=W(I)-C(I)*C(I+1)                                             SPN00840
   40 IF (J.EQ.1) GO TO 45                                              SPN00850
C     ------------------------------------------------------------------SPN00860
C             IF THE SECOND DERIVATIVE IS GIVEN AT THE END POINTS,      SPN00870
C             A(1) CAN NOW BE COMPUTED FROM THE KNOWN VALUES OF         SPN00880
C             B(1) AND E(1). THEN A(1) AND B(1) ARE PUT INTO THEIR      SPN00890
C             PROPER PLACES IN THE C ARRAY.                             SPN00900
C     ------------------------------------------------------------------SPN00910
      C(1)=Y(2)-Y(1)-W(2)-C(3)                                          SPN00920
      C(2)=W(2)                                                         SPN00930
      RETURN                                                            SPN00940
   45 C(2)=W(2)-C(3)                                                    SPN00950
      RETURN                                                            SPN00960
      END                                                               SPN00970
