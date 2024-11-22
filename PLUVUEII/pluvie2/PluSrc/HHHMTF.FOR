CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCMTF00010
      SUBROUTINE HHHMTF(GPEAK,N,WIDTH,FF,CC,DWK1,DWK2,IOUT,IFLAT,IER)   MTF00020
C                                                                       MTF00030
C  HHHMTF.FOR                                                           MTF00040
C                                                                       MTF00050
C  THIS SUBROUTINE CALCULATES THE MTF OF THE HUMAN VISUAL               MTF00060
C  SYSTEM BASED ON HOWELL AND HESS AND THEN OPTIONALLY SETS             MTF00070
C  THE MTF FOR FREQS BELOW FPEAK TO 1.0                                 MTF00080
C  SUBROUTINES REQUIRED: SPLINE ROUTINES                                MTF00090
C                                                                       MTF00100
C  PLUVUE II                                                            MTF00110
C  Version 92243   Level 1.0                                            MTF00120
C                                                                       MTF00130
C  U.S. Environmental Protection Agency                                 MTF00140
C  Office of Air Quality Planning and Standards                         MTF00150
C  Research Triangle Park, North Carolina                               MTF00160
C                                                                       MTF00170
C  Modifications:                                                       MTF00180
C                                                                       MTF00190
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCMTF00200
      SAVE                                                              MTF00210
C                                                                       MTF00220
      REAL F(10),C(10),FF(N),CC(N)                                      MTF00230
C     DOUBLE PRECISION DCOEF(9,3),GL(10),CL(10),DWK1(N),DWK2(N)         MTF00240
      DOUBLE PRECISION DWK1(N),DWK2(N)                                  MTF00250
      DOUBLE PRECISION GL(10),CL(10),S(10),RHO(10),TAU(10)              MTF00260
      DIMENSION INDEX(10)                                               MTF00270
C                                                                       MTF00280
      DATA F/ .1 ,.2 ,.5  ,1.  ,3.  ,5.  ,10. ,20.,30.,40. /            MTF00290
      DATA C/ 47.,90.,150.,185.,208.,180.,105.,35.,10.,3.5 /            MTF00300
      DATA MDIM,M,FPEAK,CPEAK,ONE / 9,10,3.,208.,1. /                   MTF00310
C                                                                       MTF00320
C CALCULATE SHIFTED LOOK UP TABLE                                       MTF00330
      FSHIFT=ALOG10(GPEAK/FPEAK)                                        MTF00340
      CSHIFT=ALOG10(CPEAK)                                              MTF00350
      DO 10 I=1,10                                                      MTF00360
        GL(I)=DBLE( ALOG10(F(I)) + FSHIFT )                             MTF00370
        CL(I)=DBLE( ALOG10(C(I)) - CSHIFT )                             MTF00380
10      CONTINUE                                                        MTF00390
C                                                                       MTF00400
C CALCULATE ARRAY OF FREQUENCIES TO LOOK UP                             MTF00410
      N2=N/2                                                            MTF00420
      IF( (FLOAT(N)/2. - FLOAT(N2)) .GT. 0.1 ) GOTO 990                 MTF00430
      DELF=1/WIDTH                                                      MTF00440
      FF(1)=0.                                                          MTF00450
      DWK1(1)=1                                                         MTF00460
      NNN=N2+1                                                          MTF00470
      DO 20 I=2,NNN                                                     MTF00480
        FF(I)= DELF * FLOAT(I-1)                                        MTF00490
        FF(N+2-I)= FF(I)                                                MTF00500
        DWK1(I)=DBLE( ALOG10(FF(I)) )                                   MTF00510
        DWK1(N+2-I)= DWK1(I)                                            MTF00520
20      CONTINUE                                                        MTF00530
C                                                                       MTF00540
C CALCULATE AND EVALUATE CUBIC SPLINE                                   MTF00550
C     CALL ICSCCU(GL,CL,M,DCOEF,MDIM)                                   MTF00560
      CALL ICSCCU(GL,CL,M,S,MDIM,RHO,TAU,INDEX,IER)                     MTF00570
      IF (IER.NE.0) GOTO 991                                            MTF00580
      CALL ICSEVU(GL,CL,M,S,DWK1,DWK2,N,INDEX,IER)                      MTF00590
C                                                                       MTF00600
C CHECK FOR ERROR CONDITIONS                                            MTF00610
      IF ( IER.EQ.0 ) GOTO 110                                          MTF00620
      IF (IER.EQ.33 .OR. IER.EQ.34 ) GOTO 100                           MTF00630
      GOTO 992                                                          MTF00640
100   CONTINUE                                                          MTF00650
      IER=0                                                             MTF00660
      GLMIN=GL(1)                                                       MTF00670
      GLMAX=GL(10)                                                      MTF00680
      CLMIN=CL(1)                                                       MTF00690
      CLMAX=CL(10)                                                      MTF00700
      SLOPE1=(CL(2)-CLMIN)/(GL(2)-GLMIN)                                MTF00710
      SLOPE2=(CL(9)-CLMAX)/(GL(9)-GLMAX)                                MTF00720
      DO 30 I=2,NNN                                                     MTF00730
        IF      (DWK1(I).GE.GLMIN ) GOTO 101                            MTF00740
        DWK2(I)=(DWK1(I)  - GLMIN ) *SLOPE1 + CLMIN                     MTF00750
        GOTO 30                                                         MTF00760
101     IF      (DWK1(I).LE.GLMAX ) GOTO 30                             MTF00770
        DWK2(I)=(DWK1(I)  - GLMAX ) *SLOPE2 + CLMAX                     MTF00780
30        CONTINUE                                                      MTF00790
C                                                                       MTF00800
C CALCULATE NON-LOG VALUES FOR RETURN TO CALLING PROGRAM                MTF00810
110   CONTINUE                                                          MTF00820
      CC(1)=1.                                                          MTF00830
      DO 40 I=2,NNN                                                     MTF00840
        CC(I)=10.0**(SNGL(DWK2(I)))                                     MTF00850
        CC(N+2-I)=CC(I)                                                 MTF00860
40      CONTINUE                                                        MTF00870
C                                                                       MTF00880
      IF( IFLAT.EQ.0 ) GOTO 999                                         MTF00890
      DO 50 I=2,NNN                                                     MTF00900
        IF(CC(I).GE.ONE) GOTO 999                                       MTF00910
        CC(I)=ONE                                                       MTF00920
        CC(N+2-I) = ONE                                                 MTF00930
50      CONTINUE                                                        MTF00940
C                                                                       MTF00950
      GOTO 999                                                          MTF00960
990   WRITE(IOUT,507)N                                                  MTF00970
      GOTO 999                                                          MTF00980
991   WRITE(IOUT,508)IER                                                MTF00990
      GOTO 999                                                          MTF01000
992   WRITE(IOUT,509)IER                                                MTF01010
C                                                                       MTF01020
999   RETURN                                                            MTF01030
507   FORMAT(1X,'ERROR FROM HHMTF: N IS NOT EVEN   N= ',I4)             MTF01040
508   FORMAT(1X,'ERROR FROM HHMTF RETURNED BY ICSCCU= ',I4)             MTF01050
509   FORMAT(1X,'ERROR FROM HHMTF RETURNED BY ICSEVU= ',I4)             MTF01060
      END                                                               MTF01070
