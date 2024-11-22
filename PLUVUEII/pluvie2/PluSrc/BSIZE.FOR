CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCBSI00010
      SUBROUTINE BSIZE                                                  BSI00020
C                                                                       BSI00030
C BSIZE.FOR                                                             BSI00040
C                                                                       BSI00050
C     This subroutine can be used to replace BSIZE in PLUVUE II and     BSI00060
C     and thereby avoid repeating the calculation of the phase          BSI00070
C     (scattering) functions for aerosol particles from Mie theory      BSI00080
C     each time PLUVUE II is executed.  The PLUVUE subroutines ALGN     BSI00090
C     and DAMIE called by the PLUVUE version of BSIZE are not used.     BSI00100
C                                                                       BSI00110
C     This subroutine operates by reading a data file of phase          BSI00120
C     functions previously calculated from the Mie equations,           BSI00130
C     selecting one of the phase functions based on the values entered  BSI00140
C     for the geometric mean radius and sigma of the aerosol size       BSI00150
C     distribution and the index of refraction, then interpolating to   BSI00160
C     obtain values of the phase function at the sattering angles       BSI00170
C     needed for the present case.  Negative values of the geometric    BSI00180
C     mean radius serve as a code to select phase functions for         BSI00190
C     particle size distributions that are not log-normally             BSI00200
C     distributed.                                                      BSI00210
C                                                                       BSI00220
C     This routine generates the output documenting the data files      BSI00230
C     used.  The data file is read from the beginning each time this    BSI00240
C     subroutine is called.                                             BSI00250
C                                                                       BSI00260
C     PLUVUE I version written by L. W. Richards and J. A. McDonald     BSI00270
C          Sonoma Technology Inc., Santa Rosa, California 95401         BSI00280
C                         For West Associates                           BSI00290
C     Revised for PLUVUE II by L. W. Richards and R. G. Hammarstrand    BSI00300
C                            October 1987                               BSI00310
C                                                                       BSI00320
C     Program version : 03    Modification number : 01                  BSI00330
C     Modification dates :  18 November 1987                            BSI00340
C                                                                       BSI00350
C  PLUVUE II                                                            BSI00360
C  Version 92243   Level 1.0                                            BSI00370
C                                                                       BSI00380
C  U.S. Environmental Protection Agency                                 BSI00390
C  Office of Air Quality Planning and Standards                         BSI00400
C  Research Triangle Park, North Carolina                               BSI00410
C                                                                       BSI00420
C  Modifications:                                                       BSI00430
C                                                                       BSI00440
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCBSI00450
      SAVE                                                              BSI00460
C                                                                       BSI00470
C+---------------------------------------------------------------------+BSI00480
C+                                                                     +BSI00490
C+                  SPECIFICATION STATEMENTS                           +BSI00500
C+                                                                     +BSI00510
C+---------------------------------------------------------------------+BSI00520
C                                                                       BSI00530
      COMMON/MIESCT/ROG,SIGMA,NLAMB,LAMB(20),JX,IT,TT(200),DUM(20),     BSI00540
     1PDUM(20,50),DUMA(20),DUMG(20)                                     BSI00550
      COMMON/ MISC/ ABSNO2(39),SOLAR(39),RAD,FORPIN,OMZ(39),OMH(39)     BSI00560
     1 ,NTHETA,RFR,RFI                                                  BSI00570
      REAL LAMB                                                         BSI00580
      DIMENSION TTI(91),ADUM(91), SDSCPT(20), DUMS(20)                  BSI00590
      LUDUM = 9                                                         BSI00600
C                                                                       BSI00610
C+---------------------------------------------------------------------+BSI00620
C+                                                                     +BSI00630
C+                     PROGRAM STATEMENTS                              +BSI00640
C+                                                                     +BSI00650
C+---------------------------------------------------------------------+BSI00660
C                                                                       BSI00670
C                                                                       BSI00680
C  Select the Mie calculation results for the aerosol size distribution BSI00690
C  and index of refraction indicated by the values of ROG, SIGMA, RFR,  BSI00700
C  and RFI by searching until a match is found.                         BSI00710
C                                                                       BSI00720
C  Log-normal size distribution with Rgv = ROG and std. dev. = SIGMA    BSI00730
C  Index of refraction = RFR - i*RFI                                    BSI00740
C  Number of wave lengths = ILAMB                                       BSI00750
C  Number of angles in input data array = ITHETA                        BSI00760
C                                                                       BSI00770
 99   READ (LUDUM,265,END=92) ROGI,SIGMAI,RFRI,RFII,ILAMB,ITHETA        BSI00780
 265  FORMAT (3F10.4,F10.6,2I10)                                        BSI00790
      IF ( ROGI .NE. ROG    ) GO TO 139                                 BSI00800
      IF ( SIGMAI .NE. SIGMA ) GO TO 139                                BSI00810
      IF (RFRI .NE. RFR) GOTO 139                                       BSI00820
      IF (RFII .NE. RFI) GOTO 139                                       BSI00830
      GO TO 300                                                         BSI00840
 139  CONTINUE                                                          BSI00850
      READ (LUDUM,201,END=94)                                           BSI00860
 201  FORMAT( 99 (/))                                                   BSI00870
      GO TO 99                                                          BSI00880
C                                                                       BSI00890
C  Read and write the size distribution description.                    BSI00900
C                                                                       BSI00910
 300  CONTINUE                                                          BSI00920
      READ (LUDUM,905,END=94) (SDSCPT(K),K=1,20)                        BSI00930
 905  FORMAT(20A4)                                                      BSI00940
      WRITE ( 6,910) ROG,SIGMA,RFR,RFI                                  BSI00950
 910  FORMAT('  ROG =',F8.4,8X,'SIGMA =',F8.4,8X,                       BSI00960
     1  'REFRACTIVE INDEX =',F7.4,' +',F10.6)                           BSI00970
      WRITE (6,905) SDSCPT                                              BSI00980
C                                                                       BSI00990
C  Generate the array of angles corresponding to the angles in the data BSI01000
C   file.                                                               BSI01010
C                                                                       BSI01020
      DO 310 I=1,91                                                     BSI01030
 310  TTI(I)= 2.0 *(I-1)                                                BSI01040
C                                                                       BSI01050
C  Read wavelength XLAM, cross sections per volume for extinction       BSI01060
C   DUM(IL), scattering DUMS(IL), which is not used by PLUVUE, and      BSI01070
C   absorption DUMA(IL), and the average cosine of the scattered        BSI01080
C   radiation DUMG(IL).                                                 BSI01090
C                                                                       BSI01100
      DO 40 IL = 1,NLAMB                                                BSI01110
      READ (LUDUM, 80, END=94) XLAM,DUM(IL),DUMS(IL),DUMA(IL),DUMG(IL)  BSI01120
  80  FORMAT (5 F10.5)                                                  BSI01130
C                                                                       BSI01140
C  Read data for phase function and interpolate to evaluate phase       BSI01150
C  function values PDUM(IL,I) used by PLUVUE.                           BSI01160
C                                                                       BSI01170
      READ (LUDUM, 88, END=94) (ADUM(I),I=1,ITHETA)                     BSI01180
  88  FORMAT (F8.3,9(/,10F8.3))                                         BSI01190
      DO 36 I = 1,NTHETA                                                BSI01200
      K= INT( TT(I)/2.) + 1                                             BSI01210
      IF ( K .EQ. 91 ) GO TO 35                                         BSI01220
      PDUM(IL,I) = (ADUM(K+1)-ADUM(K)) * (TT(I)-TTI(K))/2.0 + ADUM(K)   BSI01230
      GO TO 36                                                          BSI01240
  35  PDUM(IL,I) = ADUM(91)                                             BSI01250
  36  CONTINUE                                                          BSI01260
  40  CONTINUE                                                          BSI01270
C                                                                       BSI01280
C      close(LUDUM)                                                     BSI01290
C                                                                       BSI01300
C-----Replaced the close with a rewind to avoid opening problems        BSI01310
C                                                                       BSI01320
      rewind(LUDUM)                                                     BSI01330
      RETURN                                                            BSI01340
C                                                                       BSI01350
C  Error message if end of file when trying to read ROG, SIGMA, RFR,    BSI01360
C   and RFI.                                                            BSI01370
C                                                                       BSI01380
  92  WRITE(6,93)                                                       BSI01390
  93  FORMAT(49H REQUESTED SIZE DISTRIBUTION NOT IN MIE DATA FILE )     BSI01400
      GO TO 96                                                          BSI01410
C                                                                       BSI01420
C  Error message if end of file when trying to read other part of Mie   BSI01430
C   data.                                                               BSI01440
C                                                                       BSI01450
  94  WRITE (6,95)                                                      BSI01460
  95  FORMAT(41H END OF FILE ENCOUNTERED READING MIE DATA  ,/,          BSI01470
     1  ' THE FORMAT OF THE MIE DATA FILE IS PROBABLY INCORRECT')       BSI01480
c*** Modification by D.DiCristofaro, Sigma Research                     BSI01490
c*** Output the requested size distribution if not found                BSI01500
c 96  WRITE (6,97) ROGI, SIGMAI, RFRI, RFII                             BSI01510
  96  WRITE (6,97) ROG, SIGMA, RFR, RFI                                 BSI01520
c*** End of modification                                                BSI01530
  97  FORMAT (' REQUESTED SIZE DISTRIBUTION: ROG =',F10.4,              BSI01540
     1 '   SIGMA =',F10.4,/,' REQUESTED INDEX OF REFRACTION =',         BSI01550
     2 F10.4,' -',F10.6,'*i')                                           BSI01560
      STOP                                                              BSI01570
      END                                                               BSI01580
