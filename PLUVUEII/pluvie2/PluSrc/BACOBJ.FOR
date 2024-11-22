CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCBAC00010
      SUBROUTINE BACOBJ(ZENITH,BETA,THETA,ITHETA,RO,SPECO,XLUMIN)       BAC00020
C                                                                       BAC00030
C  BACOBJ.FOR                                                           BAC00040
C                                                                       BAC00050
C  CALCULATE SPECTRAL RADIANCE OF VIEWING BACKGROUNDS OF SPECIFIED      BAC00060
C  REFLECTANCE AS VIEWED THROUGH BACKGROUND ATMOSPHERE.                 BAC00070
C                                                                       BAC00080
C  PLUVUE II                                                            BAC00090
C  Version 96150   Level 1.0                                            BAC00100
C                                                                       BAC00110
C  U.S. Environmental Protection Agency                                 BAC00120
C  Office of Air Quality Planning and Standards                         BAC00130
C  Research Triangle Park, North Carolina                               BAC00140
C                                                                       BAC00150
C  Modifications:                                                       BAC00160
C                                                                       BAC00170
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCBAC00180
      SAVE                                                              BAC00190
C                                                                       BAC00200
      COMMON/RADPRP/BTASO4(39),BTACOR(39),BTAPRM(39),BTAAER(39),        BAC00210
     1PAER(39,27),PPRIM(39,27),PSO4(39,27),PCOR(39,27),BTABAC(39)       BAC00220
     2,PCAR(39,27),ABSCAR(39),BTACAR(39),ABSSO4(39),ABSCOR(39),         BAC00230
     3 ABSPRM(39),GSO4(39),GCOR(39),GPRM(39),GCAR(39),GAER(39)          BAC00240
     4,GBAC(39),PBAC(39,27)                                             BAC00250
      COMMON/ OPTDEP / TAT0IZ(39),TATHIZ(39),TAT0HZ(39),X1(39),         BAC00260
     1X2(39),TAUTDI(39),TAUT0D(39),XHDI(39),XH0D(39)                    BAC00270
      COMMON/DIFFI/FLUXUP(39),DIFIDN(9,21,41),DIFIUP(9,21,41),FLUXDW(39)BAC00275
      COMMON/ MISC/ ABSNO2(39),SOLAR(39),RAD,FORPIN,OMZ(39),OMH(39)     BAC00280
     1,NTHETA,RFR,RFI                                                   BAC00290
      DIMENSION SPECO(39),GI(39)                                        BAC00300
C     XMU0 = COS(RAD*ZENITH)       C added 28 May 1996; see line 350    BAC00310
      PRAY = 0.75*(1.+COS(RAD*THETA)**2)                                BAC00320
      IPF = 0                                                           BAC00330
      XMUR=SIN(BETA*RAD)                                                BAC00340
      XMU0=COS(ZENITH*RAD)                                              BAC00350
      COSTTA=COS(THETA*RAD)                                             BAC00360
      IF(BETA.EQ.90.)GO TO 30                                           BAC00370
      CDR=(COSTTA-XMUR*XMU0)/(SQRT(1.-XMU0**2)*                         BAC00380
     2SQRT(1.-XMUR**2))                                                 BAC00390
      GO TO 31                                                          BAC00400
   30 IF(THETA.NE.ZENITH)GO TO 60                                       BAC00410
      DF=0.                                                             BAC00420
      GO TO 32                                                          BAC00430
   31 CONTINUE                                                          BAC00440
      ACDR=ABS(CDR)                                                     BAC00450
C***  SRC modification  8/6/92                                          BAC00460
C-----Note near the scattering angle/azimuth crossover (around 45deg)   BAC00470
C-----there is some angular excess - we now allow for 2%                BAC00480
      IF(ACDR.GT.1.02) GO TO 60                                         BAC00490
      IF(ACDR.GT.1.0)CDR = CDR/ACDR                                     BAC00500
      DF=ACOS(CDR)/RAD                                                  BAC00510
   32 CONTINUE                                                          BAC00520
      CALL INTEG(DF,BETA,IPF,GI)                                        BAC00530
      DO 50 I = 1,39                                                    BAC00540
      FD0 = SOLAR(I)*EXP(-TAT0IZ(I)/XMU0)                               BAC00550
      FDH = SOLAR(I)*EXP(-TATHIZ(I)/XMU0)                               BAC00560
      FDAV = (FD0 + FDH)*0.5                                            BAC00570
      PTOT0D = XH0D(I)*PRAY + (1.-XH0D(I))*PAER(I,ITHETA)               BAC00580
      PTOT0D = PTOT0D*OMH(I)                                            BAC00590
      TRO = EXP(-BTABAC(I)*RO)                                          BAC00600
C     STI Modification, 12/22/94 and 5/28/96
C     Will Richards and Marcelo Korc
C     FLUXDW(I), is the direct solar plus diffuse skylight flux falling on
C     a horizontal surface at the surface of the Earth.  Of this 
C     XMU0*FD0 is due to the direct solar flux.  Thus, the diffuse flux
C     on a horizontal surface is FLUXDW(I) -XMU0*FD0.  Previous versions
C     of PLUVUE I and II illuminated the target only with the direct solar
C     ray, FDH, calculated at the top of the haze layer assuming that the
C     background target is normal to the solar rays.  This modification
C     adds the diffuse illumination to the direct solar flux at the bottom
c     of the haze layer, assuming that the diffuse illumination
C     of a horizontal surface approximately represents the diffuse 
C     illumination of the background object by skylight and light reflected
C     from the surface of the Earth. 
      SPECO(I) = XLUMIN*(FD0 + FLUXDW(I) - XMU0*FD0)*TRO +              BAC00610
     1  FORPIN*PTOT0D*(1.-TRO)*FDAV  + GI(I)*(1.-TRO)                   BAC00620
   50 CONTINUE                                                          BAC00630
      GO TO 63                                                          BAC00640
   60 DO 61 I=1,39                                                      BAC00650
      SPECO(I)=1.                                                       BAC00660
   61 CONTINUE                                                          BAC00670
  63  RETURN                                                            BAC00680
      END                                                               BAC00690
