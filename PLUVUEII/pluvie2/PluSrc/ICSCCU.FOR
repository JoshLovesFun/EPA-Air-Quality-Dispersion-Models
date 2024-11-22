CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCICS00010
        SUBROUTINE ICSCCU(GL,CL,M,S,MDIM,RHO,TAU,INDEX,IER)             ICS00020
C                                                                       ICS00030
C  ICSCCU.FOR                                                           ICS00040
C                                                                       ICS00050
C  THIS SUBROUTINE CALCULATES THE COEFFIECIENTS OF THE                  ICS00060
C  CUBIC INTERPOLATORY SPLINE FIT USING THE POINTS                      ICS00070
C  GIVEN  IN THE DATA STATEMENT IN SUBROUTINE HHHMTF.                   ICS00080
C                                                                       ICS00090
C  PLUVUE II                                                            ICS00100
C  Version 92243   Level 1.0                                            ICS00110
C                                                                       ICS00120
C  U.S. Environmental Protection Agency                                 ICS00130
C  Office of Air Quality Planning and Standards                         ICS00140
C  Research Triangle Park, North Carolina                               ICS00150
C                                                                       ICS00160
C  Modifications:                                                       ICS00170
C                                                                       ICS00180
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCICS00190
C                                                                       ICS00200
        SAVE                                                            ICS00210
        DOUBLE PRECISION GL(10),CL(10),S(10),RHO(10),TAU(10)            ICS00220
        DIMENSION INDEX(10)                                             ICS00230
C                                                                       ICS00240
        MDIM = M-1                                                      ICS00250
        DO 1 I = 1,M                                                    ICS00260
        INDEX(I)= I                                                     ICS00270
1       CONTINUE                                                        ICS00280
        DO 3 I = 1,MDIM                                                 ICS00290
        IP1 = I+1                                                       ICS00300
        DO 2 J=IP1,M                                                    ICS00310
        II=INDEX(I)                                                     ICS00320
        IJ=INDEX(J)                                                     ICS00330
        IF(GL(II) .LE. GL(IJ)) GO TO 2                                  ICS00340
        ITEMP=INDEX(I)                                                  ICS00350
        INDEX(I)= INDEX(J)                                              ICS00360
        INDEX(J) = ITEMP                                                ICS00370
2       CONTINUE                                                        ICS00380
3       CONTINUE                                                        ICS00390
        NM2= M-2                                                        ICS00400
C                                                                       ICS00410
        RHO(2) = 0.0                                                    ICS00420
        TAU(2) = 0.0                                                    ICS00430
        DO 4 I = 2,MDIM                                                 ICS00440
        IIM1 = INDEX(I-1)                                               ICS00450
        II = INDEX(I)                                                   ICS00460
        IIP1 = INDEX(I+1)                                               ICS00470
        HIM1 = GL(II)-GL(IIM1)                                          ICS00480
        HI = GL(IIP1)-GL(II)                                            ICS00490
        TEMP = (HIM1/HI)*(RHO(I)+2.0)+2.0                               ICS00500
        RHO(I+1) = -1.0/TEMP                                            ICS00510
        D = 6.0*((CL(IIP1)-CL(II))/HI-(CL(II)-CL(IIM1))/HIM1)/HI        ICS00520
        TAU(I+1)=(D-HIM1*TAU(I)/HI)/TEMP                                ICS00530
4       CONTINUE                                                        ICS00540
C                                                                       ICS00550
        S(1)= 0.0                                                       ICS00560
        S(M)= 0.0                                                       ICS00570
        DO 5 I = 1,NM2                                                  ICS00580
        IB=M-I                                                          ICS00590
        S(IB)=RHO(IB+1)*S(IB+1)+TAU(IB+1)                               ICS00600
5       CONTINUE                                                        ICS00610
        IER=0                                                           ICS00620
        DO 92 I = 1,M                                                   ICS00630
92      CONTINUE                                                        ICS00640
        RETURN                                                          ICS00650
        END                                                             ICS00660
