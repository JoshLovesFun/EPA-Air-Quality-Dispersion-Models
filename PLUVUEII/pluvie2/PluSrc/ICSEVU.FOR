CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCIVU00010
        SUBROUTINE ICSEVU(GL,CL,M,S,DWK1,DWK2,N,INDEX,IER)              IVU00020
C                                                                       IVU00030
C  THIS SUBROUTINE EVALUATES THE TRANSFORMED VALUES FROM                IVU00040
C  SUBROUTINE FFTCC USING THE COEFFICIENTS CALCULATED IN                IVU00050
C  SUBROUTINE ICSCCU.                                                   IVU00060
C                                                                       IVU00070
C  PLUVUE II                                                            IVU00080
C  Version 92243   Level 1.0                                            IVU00090
C                                                                       IVU00100
C  U.S. Environmental Protection Agency                                 IVU00110
C  Office of Air Quality Planning and Standards                         IVU00120
C  Research Triangle Park, North Carolina                               IVU00130
C                                                                       IVU00140
C  Modifications:                                                       IVU00150
C                                                                       IVU00160
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCIVU00170
C                                                                       IVU00180
        SAVE                                                            IVU00190
        DOUBLE PRECISION GL(10),CL(10),S(10),DWK1(N),DWK2(N)            IVU00200
        DIMENSION INDEX(10)                                             IVU00210
        IER=0                                                           IVU00220
C                                                                       IVU00230
        DO 59 J = 1,N                                                   IVU00240
        I1 = INDEX(1)                                                   IVU00250
        IF(DWK1(J) .GE. GL(I1)) GO TO 1                                 IVU00260
        I2= INDEX(2)                                                    IVU00270
        H1 = GL(I2)-GL(I1)                                              IVU00280
C                                                                       IVU00290
       DWK2(J)=CL(I1)+(DWK1(J)-GL(I1))*((CL(I2)-CL(I1))/H1-H1*S(2)/6.0) IVU00300
C                                                                       IVU00310
        GO TO 59                                                        IVU00320
C                                                                       IVU00330
1       IN=INDEX(M)                                                     IVU00340
        IF(DWK1(J) .LE. GL(IN)) GO TO 2                                 IVU00350
        INM1=INDEX(M-1)                                                 IVU00360
        HNM1=GL(IN)-GL(INM1)                                            IVU00370
        DWK2(J)=CL(IN)+(DWK1(J)-GL(IN))*((CL(IN)-CL(INM1))/HNM1+HNM1*   IVU00380
     &  S(M-1)/6.0)                                                     IVU00390
        GO TO 59                                                        IVU00400
C                                                                       IVU00410
2       DO 3 I=2,M                                                      IVU00420
        II=INDEX(I)                                                     IVU00430
        IF(DWK1(J).LE.GL(II)) GO TO 4                                   IVU00440
3       CONTINUE                                                        IVU00450
4       L=I-1                                                           IVU00460
        IL=INDEX(L)                                                     IVU00470
        ILP1=INDEX(L+1)                                                 IVU00480
        A = GL(ILP1)-DWK1(J)                                            IVU00490
        B = DWK1(J)-GL(IL)                                              IVU00500
        HL=GL(ILP1)-GL(IL)                                              IVU00510
        DWK2(J) = A*S(L)*(A**2/HL-HL)/6.0+B*S(L+1)*(B**2/HL-HL)         IVU00520
     &  /6.0 + (A*CL(IL)+B*CL(ILP1))/HL                                 IVU00530
59      CONTINUE                                                        IVU00540
        RETURN                                                          IVU00550
        END                                                             IVU00560
