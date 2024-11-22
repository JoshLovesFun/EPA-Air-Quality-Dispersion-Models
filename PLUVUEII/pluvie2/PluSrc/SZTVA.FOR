CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCSZT00010
      FUNCTION SZTVA(I,X)                                               SZT00020
C                                                                       SZT00030
C  SZTVA.FOR                                                            SZT00040
C                                                                       SZT00050
C  TVA VERTICAL DISPERSION COEFFICIENT (SIGMA-Z)                        SZT00060
C                                                                       SZT00070
C  PLUVUE II                                                            SZT00080
C  Version 92243   Level 1.0                                            SZT00090
C                                                                       SZT00100
C  U.S. Environmental Protection Agency                                 SZT00110
C  Office of Air Quality Planning and Standards                         SZT00120
C  Research Triangle Park, North Carolina                               SZT00130
C                                                                       SZT00140
C  Modifications:                                                       SZT00150
C                                                                       SZT00160
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCSZT00170
      SAVE                                                              SZT00180
      REAL A(6),B(6),C(6),D(6),LOGX                                     SZT00190
      DATA A/-.04057,-.02174,-.01092,.002771,.006483,.009586/           SZT00200
      DATA B/.4847,.2555,.1266,-.03534,-.08176,-.1183/                  SZT00210
      DATA C/-1.149,-.4799,-.1457,.369,.5107,.6285/                     SZT00220
      DATA D/2.027,1.382,1.09,.5842,.4429,.3116/                        SZT00230
      LOGX=ALOG10(X)                                                    SZT00240
      SZTVA=10.**(A(I)*LOGX*LOGX*LOGX+B(I)*LOGX*LOGX+C(I)*LOGX+D(I))    SZT00250
      RETURN                                                            SZT00260
      END                                                               SZT00270
