CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCPLO00010
      SUBROUTINE PLMOBJ(ZENITH,BETA,THETA,ITHETA,PLUMEP,                PLO00020
C                                                                       PLO00030
C  PLMOBJ.FOR                                                           PLO00040
C                                                                       PLO00050
C  CALCULATE SPECTRAL RADIANCE OF A VIEWED OBJECT OF GIVEN REFLECTANCE  PLO00060
C  FOR LINES OF SIGHT THROUGH PLUME AND BACKGROUND ATMOSPHERE.          PLO00070
C                                                                       PLO00080
C  Richard Looker --  SAI -- October 20 1991                            PLO00090
C  modified the code pertaining to the multiple scattering contribution PLO00100
C  to the spectral radiance through the plume to achieve agreement with PLO00110
C  the theoretical development in the PLUVUE II manual                  PLO00120
C                                                                       PLO00130
C  PLUVUE II                                                            PLO00140
C  Version 92243   Level 1.0                                            PLO00150
C                                                                       PLO00160
C  U.S. Environmental Protection Agency                                 PLO00170
C  Office of Air Quality Planning and Standards                         PLO00180
C  Research Triangle Park, North Carolina                               PLO00190
C                                                                       PLO00200
C  Modifications:                                                       PLO00210
C                                                                       PLO00220
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCPLO00230
     1PLUMES,PLUMEN,PLUMEC,XLUMIN,RO,RP,SPECO,SPECP)                    PLO00240
      SAVE                                                              PLO00250
      COMMON/RADPRP/BTASO4(39),BTACOR(39),BTAPRM(39),BTAAER(39),        PLO00260
     1PAER(39,27),PPRIM(39,27),PSO4(39,27),PCOR(39,27),BTABAC(39)       PLO00270
     2,PCAR(39,27),ABSCAR(39),BTACAR(39),ABSSO4(39),ABSCOR(39),         PLO00280
     3 ABSPRM(39),GSO4(39),GCOR(39),GPRM(39),GCAR(39),GAER(39)          PLO00290
     4,GBAC(39),PBAC(39,27)                                             PLO00300
      COMMON/ OPTDEP / TAT0IZ(39),TATHIZ(39),TAT0HZ(39),X1(39),         PLO00310
     1X2(39),TAUTDI(39),TAUT0D(39),XHDI(39),XH0D(39)                    PLO00320
      COMMON/ MISC/ ABSNO2(39),SOLAR(39),RAD,FORPIN,OMZ(39),OMH(39)     PLO00330
     1,NTHETA,RFR,RFI                                                   PLO00340
      COMMON/PLM/PPLM(39,27)                                            PLO00350
      DIMENSION SPECP(39),SPECO(39),GI(39)                              PLO00360
      DIMENSION OMEGIP(39),TPI2(39)                                     PLO00370
      DIMENSION TPI3(39)                                                PLO00380
      DIMENSION TPI1(39)                                                PLO00390
c **********************************************************************PLO00400
      XMU0 = COS(RAD*ZENITH)                                            PLO00410
      PRAY = 0.75*(1.+COS(RAD*THETA)**2)                                PLO00420
      IPF = 0                                                           PLO00430
      XMUR=SIN(BETA*RAD)                                                PLO00440
      XMU0=COS(ZENITH*RAD)                                              PLO00450
      COSTTA=COS(THETA*RAD)                                             PLO00460
      IF(BETA.EQ.90)GO TO 40                                            PLO00470
      CDR=(COSTTA-XMUR*XMU0)/(SQRT(1.-XMU0**2)*                         PLO00480
     2SQRT(1.-XMUR**2))                                                 PLO00490
      GO TO 41                                                          PLO00500
   40 IF(THETA.NE.ZENITH)GO TO 60                                       PLO00510
      DF=0.                                                             PLO00520
      GO TO 42                                                          PLO00530
   41 CONTINUE                                                          PLO00540
      ACDR=ABS(CDR)                                                     PLO00550
C***  SRC modification   8/6/92                                         PLO00560
C-----Note near the scattering angle/azimuth crossover (around 45deg)   PLO00570
C-----there is some angular excess - we now allow for 2%                PLO00580
      IF(ACDR.GT.1.02) GO TO 60                                         PLO00590
      IF(ACDR.GT.1.0)CDR = CDR/ACDR                                     PLO00600
      DF=ACOS(CDR)/RAD                                                  PLO00610
   42 CONTINUE                                                          PLO00620
      CALL INTEG(DF,BETA,IPF,GI)                                        PLO00630
C                                                                       PLO00640
C-----Now loop over all spectral bands                                  PLO00650
C                                                                       PLO00660
      DO 50 I = 1,39                                                    PLO00670
         FD0 = SOLAR(I)*EXP(-TAT0IZ(I)/XMU0)                            PLO00680
         FDH = SOLAR(I)*EXP(-TATHIZ(I)/XMU0)                            PLO00690
         FDAV = (FD0 + FDH)*0.5                                         PLO00700
         PTOT0D = XH0D(I)*PRAY + (1.-XH0D(I))*PAER(I,ITHETA)            PLO00710
         PTOT0D = PTOT0D*OMH(I)                                         PLO00720
         TRO = EXP(-BTABAC(I)*RO)                                       PLO00730
         SPECO(I) = XLUMIN*FDAV*TRO + FORPIN*PTOT0D*(1.-TRO)*FDAV       PLO00740
         SPECO(I) = SPECO(I) + GI(I)*(1.-TRO)                           PLO00750
C*****                                                                  PLO00760
C***  COMPUTE THE PLUME TRANSMISSION                                    PLO00770
C*****                                                                  PLO00780
         TAUP1 = BTABAC(I)*RP                                           PLO00790
         TAPSO4 = BTASO4(I)*PLUMES                                      PLO00800
         TAPNO2 = ABSNO2(I)*PLUMEN                                      PLO00810
         TAPRIM = BTAPRM(I)*PLUMEP                                      PLO00820
         TAPRIM = TAPRIM + (BTACAR(I) + ABSCAR(I)) * PLUMEC             PLO00830
         TAUP2   = TAPSO4 + TAPNO2 + TAPRIM                             PLO00840
C                                                                       PLO00850
C------A test is made to determine if the plume lies behind the object  PLO00860
C------(RO < RP).  If such is the case the computation is stopped       PLO00870
C------since the assumed geometry is violated and a message is printed  PLO00880
C                                                                       PLO00890
         IF(RO.LT.RP)THEN                                               PLO00900
            WRITE(6,944)                                                PLO00910
944         FORMAT(1X,'YOU HAVE PLACED THE PLUME BEHIND THE ',          PLO00920
     1      'BACKGROUND - STOPPED PROCESSING')                          PLO00930
            STOP                                                        PLO00940
         ENDIF                                                          PLO00950
         TAUP3 = BTABAC(I)*(RO-RP)                                      PLO00960
         XD2 = TAPSO4/(TAPSO4 + TAPRIM)                                 PLO00970
         XD3 = PLUMEC*BTACAR(I)/(PLUMEC*BTACAR(I)+PLUMEP*BTAPRM(I))     PLO00980
         OMEGAP = 1. -(TAPNO2+PLUMEC*ABSCAR(I)+PLUMES*ABSSO4(I)         PLO00990
     1   +PLUMEP*ABSPRM(I))/TAUP2                                       PLO01000
         OMEGIP(I) = OMEGAP                                             PLO01010
         PTOTPL = XD2*PSO4(I,ITHETA) + (1.-XD2)*PPRIM(I,ITHETA)         PLO01020
     1   *(1.-XD3)+(1.-XD2)*XD3*PCAR(I,ITHETA)                          PLO01030
         TP1 = EXP(-TAUP1)                                              PLO01040
         TPI1(I)=TP1                                                    PLO01050
         TP2 = EXP(-TAUP2)                                              PLO01060
         TPI2(I) = TP2                                                  PLO01070
         TP3 = EXP(-TAUP3)                                              PLO01080
         TPI3(I)=TP3                                                    PLO01090
         PTOT0D = XH0D(I)*PRAY + (1.-XH0D(I))*PAER(I,ITHETA)            PLO01100
         PTOT0D = PTOT0D*OMH(I)                                         PLO01110
         PL2 = XLUMIN*FDAV*TP3 + FORPIN*PTOT0D*FDAV*(1.-TP3)            PLO01120
         PL1 =      PL2*TP2 + OMEGAP*FORPIN*PTOTPL*FDAV*(1.-TP2)        PLO01130
         SPECP(I) = PL1*TP1 + FORPIN*PTOT0D*(1.-TP1)*FDAV               PLO01140
c                                                                       PLO01150
c now add the multiple scattering contributions from eqns.              PLO01160
c 53 and 55 from the PLUVUE II manual - note we use previous call to    PLO01170
c INTEG for the multiple scattering integral GI(i)                      PLO01180
c                                                                       PLO01190
         SPECP(I)= SPECP(I) +  GI(I)*(1.-TPI1(I)) +                     PLO01200
     1   GI(I)* (1.-TPI3(I)) * TPI1(I) * TPI2(I)                        PLO01210
c                                                                       PLO01220
c ********************************                                      PLO01230
c                                                                       PLO01240
         DO 252 J=1,7                                                   PLO01250
            PPLM(I,J) = XD2*PSO4(I,J) + (1.-XD2)*PPRIM(I,J)*(1.-XD3)    PLO01260
     1      + (1.-XD2)*XD3*PCAR(I,J)                                    PLO01270
  252    CONTINUE                                                       PLO01280
   50 CONTINUE                                                          PLO01290
      IPF = 1                                                           PLO01300
      XMUR=SIN(BETA*RAD)                                                PLO01310
      XMU0=COS(ZENITH*RAD)                                              PLO01320
      COSTTA=COS(THETA*RAD)                                             PLO01330
      IF(BETA.EQ.90)GO TO 30                                            PLO01340
      CDR=(COSTTA-XMUR*XMU0)/(SQRT(1.-XMU0**2)*                         PLO01350
     2SQRT(1.-XMUR**2))                                                 PLO01360
      GO TO 31                                                          PLO01370
   30 IF(THETA.NE.ZENITH)GO TO 60                                       PLO01380
      DF=0.                                                             PLO01390
      GO TO 32                                                          PLO01400
   31 CONTINUE                                                          PLO01410
      ACDR=ABS(CDR)                                                     PLO01420
C***  SRC modification                                                  PLO01430
C-----Note near the scattering angle/azimuth crossover (around 45deg)   PLO01440
C-----there is some angular excess - we now allow for 2%                PLO01450
      IF(ACDR.GT.1.02) GO TO 60                                         PLO01460
      IF(ACDR.GT.1.0)CDR = CDR/ACDR                                     PLO01470
      DF=ACOS(CDR)/RAD                                                  PLO01480
   32 CONTINUE                                                          PLO01490
c                                                                       PLO01500
c here we will add the multiple scattering contribution from the        PLO01510
c integral across the optical depth of the plume - from eqn. 54 in      PLO01520
c the PV2 manual - note we call INTEG with slightly different           PLO01530
c arguments IPF=1 here                                                  PLO01540
c                                                                       PLO01550
      CALL INTEG(DF,BETA,IPF,GI)                                        PLO01560
      DO 254 I=1,39                                                     PLO01570
          SPECP(I) = SPECP(I) + GI(I)*(1.-TPI2(I))*TPI1(I)*OMEGIP(I)    PLO01580
  254 CONTINUE                                                          PLO01590
      GO TO 63                                                          PLO01600
   60 DO 61 I=1,39                                                      PLO01610
         SPECP(I)=1.                                                    PLO01620
         SPECO(I)=1.                                                    PLO01630
   61 CONTINUE                                                          PLO01640
      WRITE(6,62)                                                       PLO01650
  62  FORMAT(/,' THIS THETA ANGLE IS IMPOSSIBLE ',/)                    PLO01660
  63  continue                                                          PLO01670
      RETURN                                                            PLO01680
      END                                                               PLO01690
C                                                                       PLO01700
