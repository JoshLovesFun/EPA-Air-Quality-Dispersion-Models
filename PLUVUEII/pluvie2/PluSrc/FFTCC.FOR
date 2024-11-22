CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCFFT00010
        SUBROUTINE FFTCC(DCWK,N)                                        FFT00020
C                                                                       FFT00030
C  FFTCC.FOR                                                            FFT00040
C                                                                       FFT00050
C  THIS SUBROUTINE DOES BOTH THE FORWARD AND THE INVERSE                FFT00060
C  FOURIER TRANSFORM.                                                   FFT00070
C  THE ALGORITHM IS NOT A FAST FOURIER TRANSFORM.                       FFT00080
C                                                                       FFT00090
C  PLUVUE II                                                            FFT00100
C  Version 92243   Level 1.0                                            FFT00110
C                                                                       FFT00120
C  U.S. Environmental Protection Agency                                 FFT00130
C  Office of Air Quality Planning and Standards                         FFT00140
C  Research Triangle Park, North Carolina                               FFT00150
C                                                                       FFT00160
C  Modifications:                                                       FFT00170
C                                                                       FFT00180
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCFFT00190
        SAVE                                                            FFT00200
        DOUBLE PRECISION RE(256), IM(256),TRE(256),TIM(256)             FFT00210
C       COMPLEX*16 DCWK(N)                                              FFT00220
C                                                                       FFT00230
C  COMPLEX*16 IS AN EXTENSION OF THE ANSI STANDARD                      FFT00240
C                                                                       FFT00250
        DOUBLE PRECISION DCWK(256,2)                                    FFT00260
        PI2=6.2831853                                                   FFT00270
        SUM = 0                                                         FFT00280
        SUMI = 0                                                        FFT00290
        DO 10 I = 1,N                                                   FFT00300
        RE(I)=DCWK(I,1)                                                 FFT00310
        IM(I)=DCWK(I,2)                                                 FFT00320
10      CONTINUE                                                        FFT00330
C                                                                       FFT00340
        DO 5 M = 1, N                                                   FFT00350
        DO 6 I = 1, N                                                   FFT00360
        J=M-1                                                           FFT00370
        K=I-1                                                           FFT00380
C                                                                       FFT00390
        T= -PI2*K*J/N                                                   FFT00400
        CF=COS(T)                                                       FFT00410
        SF=SIN(T)                                                       FFT00420
        SUM = SUM+(RE(K+1)*CF)-(IM(K+1)*SF)                             FFT00430
        SUMI=SUMI+(RE(K+1)*SF)+(IM(K+1)*CF)                             FFT00440
6       CONTINUE                                                        FFT00450
        TRE(J+1)=SUM                                                    FFT00460
        TIM(J+1)=SUMI                                                   FFT00470
        SUM = 0                                                         FFT00480
        SUMI = 0                                                        FFT00490
5       CONTINUE                                                        FFT00500
C                                                                       FFT00510
C       COMBINE REAL AND IMAGINARY PARTS INTO ONE NUMBER                FFT00520
C                                                                       FFT00530
        DO 11 I = 1,N                                                   FFT00540
        DCWK(I,1) = DBLE(TRE(I))                                        FFT00550
        DCWK(I,2) = DBLE(TIM(I))                                        FFT00560
11      CONTINUE                                                        FFT00570
        RETURN                                                          FFT00580
        END                                                             FFT00590
