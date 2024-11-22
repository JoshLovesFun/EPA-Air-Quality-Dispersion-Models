CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCFIL00010
      SUBROUTINE FILTR2(A,N,WIDTH,NCH,FF,CC,DWK1,DWK2,DCWK,IOUT,IER)    FIL00020
C                                                                       FIL00030
C  FILTR2.FOR                                                           FIL00040
C                                                                       FIL00050
C  FILTER.FR IS CALLED BY FAU42.FR IN TURN CALLED BY PLUVUE.            FIL00060
C  FILTER PERFORMS SPATIAL FILTERING BY PERFORMING A FOURIER TRANSFORM, FIL00070
C  MULTIPLYING THE FREQUNCY SPECTRUM BY A MODULATION TRAMSFER FUNCTION, FIL00080
C  AND PERFORMING THE INVERSE TRANSFORM.  THE MODULATION TRANSFER       FIL00090
C  FUNCTION IS SPECIFIED BY THE FAUGERAS MODEL OF THE HUMAN VISUAL      FIL00100
C  SYSTEM FOR THREE CHANNELS AND IS DIFFERENT FOR EACH CHANNEL.         FIL00110
C                                                                       FIL00120
C ARGUMENTS:                                                            FIL00130
C         A - REAL ARRAY OF SPATIAL DATA                                FIL00140
C         N - NUMBER OF POINTS IN ARRAY A                               FIL00150
C     WIDTH - SPATIAL EXTENT OF THE ARRAY DATA IN DEGREES               FIL00160
C       NCH - SPECIFIC CHANNEL OF THE FAUGERAS MODEL USED FOR           FIL00170
C             CALCULATING THE MODULATION TRANSFER FUNCTION:             FIL00180
C             NCH CHANNEL COMMENT                                       FIL00190
C              1     A    ACHROMATIC                                    FIL00200
C              2     C1   RED/GREEN                                     FIL00210
C              3     C2   RED/BLUE                                      FIL00220
C        FF - REAL    WORK SPACE, LENGTH N                              FIL00230
C        CC - REAL    WORK SPACE, LENGTH N                              FIL00240
C      DWK1 - DOUBLE PRECISION WORK SPACE LENGTH N                      FIL00250
C      DWK2 - DOUBLE PRECISION WORK SPACE LENGTH N                      FIL00260
C      DCWK - DOUBLE PRECISION COMPLEX WORK SPACE, LENGTH N             FIL00270
C      IOUT - FORTRAN CHANNEL NUMBER FOR PRINTED OUTPUT                 FIL00280
C       IER - ERROR CODE. 0 = OK                                        FIL00290
C                                                                       FIL00300
C SUBROUTINES REQUIRED: FFTCC,HHHMTF,ICSEVU,ICSCCU                      FIL00310
C                                                                       FIL00320
C          (REV 10/28/83)                                               FIL00330
C                                                                       FIL00340
C          WRITTEN BY : JOHN COLLINS      10/28/83                      FIL00350
C                       ENVIRONMENTAL RESEARCH AND TECHNOLOGY, INC      FIL00360
C                       2625 TOWNSGATE ROAD                             FIL00370
C                       WESTLAKE VILLAGE, CA 91360                      FIL00380
C                                                                       FIL00390
C  PLUVUE II                                                            FIL00400
C  Version 92243   Level 1.0                                            FIL00410
C                                                                       FIL00420
C  U.S. Environmental Protection Agency                                 FIL00430
C  Office of Air Quality Planning and Standards                         FIL00440
C  Research Triangle Park, North Carolina                               FIL00450
C                                                                       FIL00460
C  Modifications:                                                       FIL00470
C                                                                       FIL00480
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCFIL00490
      SAVE                                                              FIL00500
      REAL A(N),FF(N),CC(N)                                             FIL00510
      DOUBLE PRECISION DN,DZERO,DWK1(N),DWK2(N)                         FIL00520
C     COMPLEX*16 DCWK(N)                                                FIL00530
C                                                                       FIL00540
C  COMPLEX*16 IS AN EXTENSION OF THE ANSI STANDARD                      FIL00550
C                                                                       FIL00560
      DOUBLE PRECISION DCWK(256,2)                                      FIL00570
      DATA DZERO/0.0D00/                                                FIL00580
C                                                                       FIL00590
      IER=0                                                             FIL00600
      DN= DBLE(N)                                                       FIL00610
C                                                                       FIL00620
C PREPARE TO PERFORM FOURIER TRANSFORM.                                 FIL00630
C IMAGINARY PART IS ZERO; CONJUGATION (NORMALY REQUIRED) IS UNECESSARY  FIL00640
      DO 10 I=1,N                                                       FIL00650
C       DCWK(I)=DCMPLX( DBLE( A(I) ),DZERO )                            FIL00660
        DCWK(I,1)= DBLE( A(I) )                                         FIL00670
        DCWK(I,2)= DZERO                                                FIL00680
10    CONTINUE                                                          FIL00690
C                                                                       FIL00700
C PERFORM FORWARD TRANSFORM (CONJGATION REQUIRED) AND SCALE OUTPUT      FIL00710
      CALL FFTCC(DCWK,N)                                                FIL00720
      DO 20 I=1,N                                                       FIL00730
C       DCWK(I)= DCWK(I) / DN                                           FIL00740
        DCWK(I,1)= DCWK(I,1) / DN                                       FIL00750
        DCWK(I,2)= DCWK(I,2) / DN                                       FIL00760
20     CONTINUE                                                         FIL00770
C OBTAIN MODULATION TRANSFER FUNCTION                                   FIL00780
      IFLAT=1                                                           FIL00790
      IF (NCH.EQ.1) IFLAT = 0                                           FIL00800
      GPEAK=3.                                                          FIL00810
      IF(NCH.EQ.3) GPEAK=1.                                             FIL00820
      CALL HHHMTF(GPEAK,N,WIDTH,FF,CC,DWK1,DWK2,IOUT,IFLAT,IER)         FIL00830
      IF(IER.NE.0) GOTO 990                                             FIL00840
      DO 21 I = 1,N                                                     FIL00850
C21   A(I) = SNGL(DREAL(DCWK(I)))                                       FIL00860
21    A(I) = SNGL(DCWK(I,1))                                            FIL00870
C PRINT OUT INTERMEDIATE DATA                                           FIL00880
      NSTEP = 1                                                         FIL00890
      IOUT = 6                                                          FIL00900
      WRITE(IOUT,502)NCH                                                FIL00910
      CALL FAUOUT(FF,CC,N,NSTEP,IOUT)                                   FIL00920
C                                                                       FIL00930
C APPLY MTF                                                             FIL00940
      DO 30 I=1,N                                                       FIL00950
C30     DCWK(I)=DCWK(I) * CC(I)                                         FIL00960
        DCWK(I,1)=DCWK(I,1) * CC(I)                                     FIL00970
        DCWK(I,2)=DCWK(I,2) * CC(I)                                     FIL00980
  30    CONTINUE                                                        FIL00990
C                                                                       FIL01000
C                                                                       FIL01010
C PERFORM INVERSE TRANSFORM (NO CONJGATION REQUIRED)                    FIL01020
C                                                                       FIL01030
      DO 101 I = 1,N                                                    FIL01040
C     DCWK(I) = DCONJG(DCWK(I))                                         FIL01050
      DCWK(I,2) = -DCWK(I,2)                                            FIL01060
101   CONTINUE                                                          FIL01070
C                                                                       FIL01080
      CALL FFTCC(DCWK,N)                                                FIL01090
      DO 102 I = 1,N                                                    FIL01100
C     DCWK(I) = DCONJG(DCWK(I))                                         FIL01110
      DCWK(I,2) = -DCWK(I,2)                                            FIL01120
102   CONTINUE                                                          FIL01130
C                                                                       FIL01140
C RETURN FILTERED VALUES IN A                                           FIL01150
      DO 40 I=1,N                                                       FIL01160
C40     A(I)=SNGL( DREAL( DCWK(I) ) )                                   FIL01170
40      A(I)=SNGL(  DCWK(I,1) )                                         FIL01180
      GOTO 999                                                          FIL01190
C                                                                       FIL01200
990   WRITE(IOUT,501)IER                                                FIL01210
      IER=1                                                             FIL01220
999   RETURN                                                            FIL01230
501   FORMAT(1X,'ERROR FROM FILTR2. ERROR RETURNED BY HHHMTF=',I3)      FIL01240
502   FORMAT(1X,'PRINTING MTF FOR CHANNEL',I2/)                         FIL01250
      END                                                               FIL01260
