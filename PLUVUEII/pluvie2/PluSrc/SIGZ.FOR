CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCSGZ00010
      SUBROUTINE SIGZ(XARG,KST,SZ)                                      SGZ00020
C                                                                       SGZ00030
C  SIGZ.FOR                                                             SGZ00040
C                                                                       SGZ00050
C  CALCULATES SIGMA-Z VALUES FROM DISPERSION CURVES                     SGZ00060
C                                                                       SGZ00070
C                 SIGZ Module of ISC2 Model                             SGZ00080
C                                                                       SGZ00090
C        PROGRAMMER: Roger Brode, Jeff Wang                             SGZ00100
C                    Adapted for use by GEM SRC July 1992               SGZ00110
C                                                                       SGZ00120
C        NOTE: Removed urban option and set PGT class G = F             SGZ00130
C                                                                       SGZ00140
C        DATE:    March 2, 1992  revised July 1992                      SGZ00150
C                                                                       SGZ00160
C        INPUTS:  Downwind Distance (XARG in M)                         SGZ00170
C                 Stability Class (KST)                                 SGZ00180
C                                                                       SGZ00190
C        OUTPUTS: Vertical Dispersion Coefficient, SZ (M)               SGZ00200
C                                                                       SGZ00210
C  PLUVUE II                                                            SGZ00220
C  Version 92243   Level 1.0                                            SGZ00230
C                                                                       SGZ00240
C  U.S. Environmental Protection Agency                                 SGZ00250
C  Office of Air Quality Planning and Standards                         SGZ00260
C  Research Triangle Park, North Carolina                               SGZ00270
C                                                                       SGZ00280
C  Modifications:                                                       SGZ00290
C                                                                       SGZ00300
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCSGZ00310
C                                                                       SGZ00320
C     Convert Distance to km                                            SGZ00330
      XKM = XARG * 0.001                                                SGZ00340
                                                                        SGZ00350
C     Determine Sigma-z Based on RURAL/URBAN, Stability Class, and DistaSGZ00360
C     Stability Classes are Checked in the Order 4, 5, 6, 1, 2, 3       SGZ00370
C     For Optimization, Since Neutral and Stable are Generally the Most SGZ00380
C     Frequent Classes.                                                 SGZ00390
                                                                        SGZ00400
C        Retrieve Coefficients, A and B                     ---   CALL SSGZ00410
      IPGT = KST                                                        SGZ00420
      IF(KST.EQ.7)IPGT = 6                                              SGZ00430
      CALL SZCOEF(XKM,IPGT,A,B,XMIN,XMAX)                               SGZ00440
      SZ = A*XKM**B                                                     SGZ00450
C                                                                       SGZ00460
      RETURN                                                            SGZ00470
      END                                                               SGZ00480
