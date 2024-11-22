CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCSCT00010
      SUBROUTINE RAYSCT(U,U0,D1,C1,C2,S,S1,P1,P2,W0,B0,B1,B2)           SCT00020
C                                                                       SCT00030
C  RAYSCT.FOR                                                           SCT00040
C                                                                       SCT00050
C  COMPUTE RAYLEIGH SCATTERING                                          SCT00060
C                                                                       SCT00070
C  PLUVUE II                                                            SCT00080
C  Version 92243   Level 1.0                                            SCT00090
C                                                                       SCT00100
C  U.S. Environmental Protection Agency                                 SCT00110
C  Office of Air Quality Planning and Standards                         SCT00120
C  Research Triangle Park, North Carolina                               SCT00130
C                                                                       SCT00140
C  Modifications:                                                       SCT00150
C                                                                       SCT00160
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCSCT00170
      SAVE                                                              SCT00180
      DATA PI/3.141592653/                                              SCT00190
C                                                                       SCT00200
      DTR(X)=PI/180.*X                                                  SCT00210
      RTD(X)=180./PI*X                                                  SCT00220
C                                                                       SCT00230
      D=DTR(D1)                                                         SCT00240
C                                                                       SCT00250
      C1=-U*U0+SQRT(1.-U*U)*SQRT(1.-U0*U0)*COS(D)                       SCT00260
      C2=U*U0+SQRT(1.-U*U)*SQRT(1.-U0*U0)*COS(D)                        SCT00270
      S=ACOS(-C2)                                                       SCT00280
      S1=RTD(S)                                                         SCT00290
      P1=.75*(1.+C1*C1)                                                 SCT00300
      P2=.75*(1.+C2*C2)                                                 SCT00310
      W0=.9999                                                          SCT00320
      B2=.500005                                                        SCT00330
      B1=.500005                                                        SCT00340
      B0=.500005                                                        SCT00350
      RETURN                                                            SCT00360
      END                                                               SCT00370
