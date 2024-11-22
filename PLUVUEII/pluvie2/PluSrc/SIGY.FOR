CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCSGY00010
      SUBROUTINE SIGY(XARG,KST,SY)                                      SGY00020
C                                                                       SGY00030
C  SIGY.FOR                                                             SGY00040
C                                                                       SGY00050
C  CALCULATES SIGMA-Y VALUES FROM DISPERSION CURVES                     SGY00060
C                                                                       SGY00070
C                 SIGY Module of ISC2 Model                             SGY00080
C                                                                       SGY00090
C        PROGRAMMER: Roger Brode, Jeff Wang                             SGY00100
C                                                                       SGY00110
C        NOTE: Removed urban option set class g to d                    SGY00120
C                                                                       SGY00130
C        DATE:    March 2, 1992                                         SGY00140
C                                                                       SGY00150
C        INPUTS:  Downwind Distance (XARG in M)                         SGY00160
C                 Stability Class (KST)                                 SGY00170
C                                                                       SGY00180
C        OUTPUTS: Lateral Dispersion Coefficient, SY  (M)               SGY00190
C                                                                       SGY00200
C  PLUVUE II                                                            SGY00210
C  Version 92243   Level 1.0                                            SGY00220
C                                                                       SGY00230
C  U.S. Environmental Protection Agency                                 SGY00240
C  Office of Air Quality Planning and Standards                         SGY00250
C  Research Triangle Park, North Carolina                               SGY00260
C                                                                       SGY00270
C  Modifications:                                                       SGY00280
C                                                                       SGY00290
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCSGY00300
C     Degree to radian conversion                                       SGY00310
      DTORAD = 0.01745                                                  SGY00320
      IPGT = KST                                                        SGY00330
      IF(KST.EQ.7)IPGT = 4                                              SGY00340
                                                                        SGY00350
C                                                                       SGY00360
C     Convert Distance to km                                            SGY00370
      XKM = XARG * 0.001                                                SGY00380
C                                                                       SGY00390
C     Determine Sigma-y Based on RURAL/URBAN, Stability Class, and DistaSGY00400
C     Stability Classes are Checked in the Order 4, 5, 6, 1, 2, 3       SGY00410
C     For Optimization, Since Neutral and Stable are Generally the Most SGY00420
C     Frequent Classes.                                                 SGY00430
                                                                        SGY00440
      IF (IPGT .EQ. 4) THEN                                             SGY00450
         TH = (8.3330 - 0.72382*ALOG(XKM)) * DTORAD                     SGY00460
      ELSE IF (IPGT .EQ. 5) THEN                                        SGY00470
         TH = (6.25 - 0.54287*ALOG(XKM)) * DTORAD                       SGY00480
      ELSE IF (IPGT .EQ. 6) THEN                                        SGY00490
         TH = (4.1667 - 0.36191*ALOG(XKM)) * DTORAD                     SGY00500
      ELSE IF (IPGT .EQ. 1) THEN                                        SGY00510
         TH = (24.1667 - 2.5334*ALOG(XKM)) * DTORAD                     SGY00520
      ELSE IF (IPGT .EQ. 2) THEN                                        SGY00530
         TH = (18.333 - 1.8096*ALOG(XKM)) * DTORAD                      SGY00540
      ELSE IF (IPGT .EQ. 3) THEN                                        SGY00550
         TH = (12.5 - 1.0857*ALOG(XKM)) * DTORAD                        SGY00560
      ENDIF                                                             SGY00570
C                                                                       SGY00580
C        NOTE THAT 465.11628 = 1000. (m/km) / 2.15                      SGY00590
C                                                                       SGY00600
      SY = 465.11628 * XKM * TAN(TH)                                    SGY00610
      RETURN                                                            SGY00620
      END                                                               SGY00630
