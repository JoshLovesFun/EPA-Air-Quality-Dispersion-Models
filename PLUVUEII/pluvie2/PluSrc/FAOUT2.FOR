CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCFAO00010
      SUBROUTINE FAOUT2(B,A,C1,C2,F,N,ISTEP,IOUT)                       FAO00020
C                                                                       FAO00030
C  FAOUT2                                                               FAO00040
C                                                                       FAO00050
C FAUOUT.FR IS CALLED BY FAU2.FR IN TURN CALLED BY PLUVUE.              FAO00060
C FAUOUT.FR PRINTS OUT A CONDENSED VERSION OF THE INTERPOLATED          FAO00070
C FAUGERAS SPATIAL DATA VS ELEVATION ANGLE AND PLOTS                    FAO00080
C A CORRESPONDING GRAPH.                                                FAO00090
C THE PROGRAM IS INTENDED TO BE GENERAL.  HEADERS MUST BE WRITTEN       FAO00100
C BY THE CALLING PROGRAM.                                               FAO00110
C                                                                       FAO00120
C ARGUMENTS:                                                            FAO00130
C         B - REAL ARRAY OF ELEVATION ANGLES IN DEGREES                 FAO00140
C         A - REAL ARRAY OF FAUGERAS CHANNEL DATA                       FAO00150
C        C1 - REAL ARRAY OF FAUGERAS CHANNEL DATA                       FAO00160
C        C2 - REAL ARRAY OF FAUGERAS CHANNEL DATA                       FAO00170
C         F - REAL ARRAY OF FAUGERAS CHANNEL DATA                       FAO00180
C     ISTEP - INTERVAL AT WHICH TO PRINT OUTPUT FROM AMONG N POINTS     FAO00190
C      IOUT - FORTRAN PRINTED OUTPUT CHANNEL NUMBER                     FAO00200
C                                                                       FAO00210
C         (REV 7/21/83)                                                 FAO00220
C                                                                       FAO00230
C          WRITTEN BY : JOHN COLLINS       7/13/83                      FAO00240
C                       ENVIRONMENTAL RESEARCH AND TECHNOLOGY, INC      FAO00250
C                       2625 TOWNSGATE ROAD                             FAO00260
C                       WESTLAKE VILLAGE, CA 91360                      FAO00270
C                                                                       FAO00280
C  PLUVUE II                                                            FAO00290
C  Version 96142   Level 1.0                                            FAO00300
C                                                                       FAO00310
C  U.S. Environmental Protection Agency                                 FAO00320
C  Office of Air Quality Planning and Standards                         FAO00330
C  Research Triangle Park, North Carolina                               FAO00340
C                                                                       FAO00350
C  Modifications:                                                       FAO00360
C                                                                       FAO00370
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCFAO00380
C                                                                       FAO00390
      SAVE                                                              FAO00400
      CHARACTER*1 IPLT(61),IBLNK,IPLUS,ISYM                             FAO00410
      REAL B(N),A(N),C1(N),C2(N),F(N)                                   FAO00420
C                                                                       FAO00430
      DATA IBLNK,IPLUS,ISYM/' ','+','X'/                                FAO00440
C                                                                       FAO00450
C FIND RANGE OF F                                                       FAO00460
      FMAX=F(1)                                                         FAO00470
      FMIN=F(1)                                                         FAO00480
      DO 10 I=2,N                                                       FAO00490
        IF(F(I).GT.FMAX) FMAX=F(I)                                      FAO00500
10      IF(F(I).LT.FMIN) FMIN=F(I)                                      FAO00510
      IF(FMIN.GT.0.) FMIN=0.                                            FAO00520
      IF(FMAX.LT.0.) FMAX=0.                                            FAO00530
      FRANGE=FMAX-FMIN                                                  FAO00540
      FFACT= 60. / FRANGE                                               FAO00550
C                                                                       FAO00560
C FILL IPLT WITH BLANKS                                                 FAO00570
      DO 20 I=1,61                                                      FAO00580
20      IPLT(I)=IBLNK                                                   FAO00590
C                                                                       FAO00600
C CALCULATE INDEX OF F=0 AND PLACE "+" THERE                            FAO00610
      F0=0.                                                             FAO00620
      INDXF0= 1+INT( FFACT*(F0-FMIN) +0.5 )                             FAO00630
      IPLT(INDXF0)=IPLUS                                                FAO00640
C                                                                       FAO00650
      DO 100 I=1,N,ISTEP                                                FAO00660
        INDX=1+INT( FFACT*(F(I)-FMIN) + 0.5 )                           FAO00670
        IPLT(INDX)=ISYM                                                 FAO00680
          WRITE(IOUT,501)B(I),A(I),C1(I),C2(I),F(I),(IPLT(J),J=1,61)    FAO00690
        IPLT(INDX )=IBLNK                                               FAO00700
        IPLT(INDXF0)=IPLUS                                              FAO00710
100     CONTINUE                                                        FAO00720
      RETURN                                                            FAO00730
501   FORMAT(1X,5G13.5,1X,61A1)                                         FAO00740
      END                                                               FAO00750
