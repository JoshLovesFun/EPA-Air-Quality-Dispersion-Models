CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCERC00010
       FUNCTION ERFC(X)                                                 ERC00020
C                                                                       ERC00030
C  ERFC.FOR                                                             ERC00040
C                                                                       ERC00050
C  THIS SUBROUTINE APPROXIMATES ONE MINUS THE ERROR FUNCTION BY MEANS   ERC00060
C  OF A THREE TERM POLYNOMIAL.  THE ERROR IN THE APPROXIMATION          ERC00070
C  IS LESS THAN OR EQUAL TO 2.5E-05                                     ERC00080
C                                                                       ERC00090
C  PLUVUE II                                                            ERC00100
C  Version 92243   Level 1.0                                            ERC00110
C                                                                       ERC00120
C  U.S. Environmental Protection Agency                                 ERC00130
C  Office of Air Quality Planning and Standards                         ERC00140
C  Research Triangle Park, North Carolina                               ERC00150
C                                                                       ERC00160
C  Modifications:                                                       ERC00170
C                                                                       ERC00180
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCERC00190
C                                                                       ERC00200
       SAVE                                                             ERC00210
       DATA ONE,P/1.0,.47047/                                           ERC00220
       DATA A1,A2,A3/.34802,-.0958798,.7478556/                         ERC00230
C                                                                       ERC00240
       S=SIGN(ONE,X)                                                    ERC00250
       X=S*X                                                            ERC00260
C                                                                       ERC00270
       T1=ONE/(ONE+P*X)                                                 ERC00280
       T2=T1*T1                                                         ERC00290
       T3=T2*T1                                                         ERC00300
C                                                                       ERC00310
       ERFC=(A1*T1+A2*T2+A3*T3)*EXP(-X*X)                               ERC00320
       ERFC= ONE-S + S*ERFC                                             ERC00330
       X=S*X                                                            ERC00340
C                                                                       ERC00350
       RETURN                                                           ERC00360
       END                                                              ERC00370
