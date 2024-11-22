CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCERF00010
      FUNCTION ERF(XX)                                                  ERF00020
C                                                                       ERF00030
C  ERF.FOR                                                              ERF00040
C                                                                       ERF00050
C  ERF=2/SQRT(PI)*INTEGRAL OF EXP(-T*T) FROM 0 TO X.                    ERF00060
C  USING AN APPROXIMATION DUE TO HASTINGS  GOOD TO SEVEN SI             ERF00070
C  USING AN APPROXIMATION DUE TO HASTINGS. ABSOLUTE ERROR ABOUT 3E-7    ERF00080
C                                                                       ERF00090
C  PLUVUE II                                                            ERF00100
C  Version 92243   Level 1.0                                            ERF00110
C                                                                       ERF00120
C  U.S. Environmental Protection Agency                                 ERF00130
C  Office of Air Quality Planning and Standards                         ERF00140
C  Research Triangle Park, North Carolina                               ERF00150
C                                                                       ERF00160
C  Modifications:                                                       ERF00170
C                                                                       ERF00180
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCERF00190
C                                                                       ERF00200
      SAVE                                                              ERF00210
C                                                                       ERF00220
      DIMENSION A(6)                                                    ERF00230
      DATA A/.0000430638,.0002765672,.0001520143,.0092705272,.0422820123ERF00240
     1,.0705230784 /                                                    ERF00250
      X=ABS(XX)                                                         ERF00260
      T=A(1)*X                                                          ERF00270
      DO 10 I=2,6                                                       ERF00280
      T=(T+A(I))*X                                                      ERF00290
   10 CONTINUE                                                          ERF00300
      T=1./(T+1.)                                                       ERF00310
      ERF=1.-T**16                                                      ERF00320
      IF(XX.LT.0.)ERF=-ERF                                              ERF00330
      RETURN                                                            ERF00340
      END                                                               ERF00350
