CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCSYT00010
      FUNCTION SYTVA(I,X)                                               SYT00020
C                                                                       SYT00030
C  SYTVA                                                                SYT00040
C                                                                       SYT00050
C  TVA HORIZONTAL DISPERSION COEFFICIENT (SIGMA Y)                      SYT00060
C                                                                       SYT00070
C  PLUVUE II                                                            SYT00080
C  Version 92243   Level 1.0                                            SYT00090
C                                                                       SYT00100
C  U.S. Environmental Protection Agency                                 SYT00110
C  Office of Air Quality Planning and Standards                         SYT00120
C  Research Triangle Park, North Carolina                               SYT00130
C                                                                       SYT00140
C  Modifications:                                                       SYT00150
C                                                                       SYT00160
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCSYT00170
      SAVE                                                              SYT00180
      REAL A(6),B(6),C(6),D(6),LOGX                                     SYT00190
      DATA A/-.04054,-.04092,-.02536,-.02398,-.01837,-.01766/           SYT00200
      DATA B/.4871,.4745,.3092,.2857,.2165,.2022/                       SYT00210
      DATA C/-1.16,-1.108,-.5988,-.5402,-.3099,-.2681/                  SYT00220
      DATA D/2.057,1.993,1.497,1.459,1.217,1.174/                       SYT00230
      LOGX=ALOG10(X)                                                    SYT00240
      SYTVA=10.**(A(I)*LOGX*LOGX*LOGX+B(I)*LOGX*LOGX+C(I)*LOGX+D(I))    SYT00250
      RETURN                                                            SYT00260
      END                                                               SYT00270
