CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCFAT00010
      SUBROUTINE FAUOUT(B,X,N,ISTEP,IOUT)                               FAT00020
C                                                                       FAT00030
C  FAUOUT.FOR                                                           FAT00040
C                                                                       FAT00050
C FAUOUT.FR IS CALLED BY FAU2.FR IN TURN CALLED BY PLUVUE.              FAT00060
C FAUOUT.FR PRINTS OUT A CONDENSED VERSION OF THE INTERPOLATED          FAT00070
C FAUGERAS SPATIAL DATA VS ELEVATION ANGLE AND PLOTS                    FAT00080
C A CORRESPONDING GRAPH.                                                FAT00090
C THE PROGRAM IS INTENDED TO BE GENERAL.  HEADERS MUST BE WRITTEN       FAT00100
C BY THE CALLING PROGRAM.                                               FAT00110
C                                                                       FAT00120
C ARGUMENTS:                                                            FAT00130
C         B - REAL ARRAY OF ELEVATION ANGLES IN DEGREES                 FAT00140
C         X - REAL ARRAY OF FAUGERAS CHANNEL DATA                       FAT00150
C         N - NUMBER OF POINTS IN THE ARRAYS FOR OUTPUT                 FAT00160
C     ISTEP - INTERVAL AT WHICH TO STEP THROUGH PRINTOUT                FAT00170
C      IOUT - FORTRAN PRINTED OUTPUT CHANNEL NUMBER                     FAT00180
C                                                                       FAT00190
C         (REV 7/21/83)                                                 FAT00200
C                                                                       FAT00210
C          WRITTEN BY : JOHN COLLINS       7/13/83                      FAT00220
C                       ENVIRONMENTAL RESEARCH AND TECHNOLOGY, INC      FAT00230
C                       2625 TOWNSGATE ROAD                             FAT00240
C                       WESTLAKE VILLAGE, CA 91360                      FAT00250
C                                                                       FAT00260
C  PLUVUE II                                                            FAT00270
C  Version 96142   Level 1.0                                            FAT00280
C                                                                       FAT00290
C  U.S. Environmental Protection Agency                                 FAT00300
C  Office of Air Quality Planning and Standards                         FAT00310
C  Research Triangle Park, North Carolina                               FAT00320
C                                                                       FAT00330
C  Modifications:                                                       FAT00340
C                                                                       FAT00350
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCFAT00360
      SAVE                                                              FAT00370
      CHARACTER*1 IPLT(61),IBLNK,IPLUS,ISYM                             FAT00380
      REAL B(N),X(N)                                                    FAT00390
C                                                                       FAT00400
      DATA IBLNK,IPLUS,ISYM/' ','+','X'/                                FAT00410
C                                                                       FAT00420
C FIND RANGE OF X                                                       FAT00430
      XMAX=X(1)                                                         FAT00440
      XMIN=X(1)                                                         FAT00450
      DO 10 I=2,N                                                       FAT00460
        IF(X(I).GT.XMAX) XMAX=X(I)                                      FAT00470
10      IF(X(I).LT.XMIN) XMIN=X(I)                                      FAT00480
      IF(XMIN.GT.0.) XMIN=0.                                            FAT00490
      IF(XMAX.LT.0.) XMAX=0.                                            FAT00500
      XRANGE=XMAX-XMIN                                                  FAT00510
      XFACT= 60. / XRANGE                                               FAT00520
C                                                                       FAT00530
C FILL IPLT WITH BLANKS                                                 FAT00540
      DO 20 I=1,61                                                      FAT00550
20      IPLT(I)=IBLNK                                                   FAT00560
C                                                                       FAT00570
C CALCULATE INDEX OF X=0 AND PLACE "+" THERE                            FAT00580
      X0=0.                                                             FAT00590
      INDX0= 1+INT( XFACT*(X0-XMIN) +0.5 )                              FAT00600
      IPLT(INDX0)=IPLUS                                                 FAT00610
C                                                                       FAT00620
      DO 100 I=1,N,ISTEP                                                FAT00630
        INDX=1+INT( XFACT*(X(I)-XMIN) + 0.5 )                           FAT00640
        IPLT(INDX)=ISYM                                                 FAT00650
          WRITE(IOUT,501)B(I),X(I),(IPLT(J),J=1,61)                     FAT00660
        IPLT(INDX )=IBLNK                                               FAT00670
        IPLT(INDX0)=IPLUS                                               FAT00680
100     CONTINUE                                                        FAT00690
      RETURN                                                            FAT00700
501   FORMAT(1X,2G13.5,1X,61A1)                                         FAT00710
      END                                                               FAT00720
