      program MIETBL
C
C **********************************************************************PLV00030
C                          MIETBL (Dated 13220)                         PLV00040
C                                                                       PLV00050
C            ***    SEE PLUVUE2 MODEL CHANGE BULLETIN     ***           PLV00060
C                                                                       PLV00070
C    ON THE SUPPORT CENTER FOR REGULATORY AIR MODELS BULLETIN BOARD     PLV00080
C                                                                       PLV00090
C                http://www.epa.gov/ttn/scram/comments.htm              PLV00100
C                                                                       PLV00110
C **********************************************************************PLV00120
C     Purpose:
C
C     This program prepares particle size distribution data for input
C     into PLUVUE2.
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C+                                                                     +
C+    FUNCTIONAL DESCRIPTION :                                         +
C+                                                                     +
C+    Program to calculate the for log-normal aerosol size distri-     +
C+    butions the cross sections per volume for extinction (DUM),      +
C+    scattering (DUMS) (not used by PLUVUE), and absorption (DUMA),   +
C+    and also the average cosine of the scattered radiation (DUMG),   +
C+    and the scattering phase function (PDUM) for nine wavelengths    +
C+    from 0.35 um to 0.75 um in 0.05 um increments.  For each wave-   +
C+    length, PDUM is tabulated at 91 angles from 0 to 180 degrees in  +
C+    increments of 2 degrees.  An output file is created in the       +
C+    format expected by the version of BSIZE which looks up Mie       +
C+    calculation results rather than recalculating them.  The Mie     +
C+    data files for PLUVUE I and PLUVUE II have the same format.      +
C+                                                                     +
C+    The program calls for terminal input of the geometric mean       +
C+    radius and the geometric standard deviation (which equals unity  +
C+    rather than zero for monodisperse aerosol).  Terminal input of   +
C+    the real (RFR) and the imaginary (RFI) components of the index   +
C+    of refraction (RFR - i*RFI) are also required.  After the        +
C+    completion of the calculations the program asks for the next     +
C+    input.  Entry of a negative radius and any standard deviation    +
C+    will cause an exit from the program.  Execution times on a       +
C+    personal computer can be many hours, so terminal output shows    +
C+    the progress of the calculation.                                 +
C+                                                                     +
C+    Statements with line numbers are the same as in the PLUVUE I     +
C+    subroutine BSIZE.  The code to generate the log-normal histogram +
C+    has been altered to allow changing the number of particle sizes  +
C+    by changing the dimensions of XNT, ST, VT, R and DR and by       +
C+    changing the value of the variable points in card number 230     +
C+                                                                     +
C+    SUBPROGRAMS CALLED : DAMIE, ALGN, OPENIT                         +
C+                                                                     +
C+    LAHEY FORTRAN COMPILER LIBRARY CALLS:                            +
C+      UNDER0  (Underflows set equal to zero)                         +
C+      SYSTEM  (To use DOS commands)                                  +
C+                                                                     +
C+    WRITTEN BY : L. Willard Richards and J. A. McDonald              +
C+    Sonoma Technology Inc. Santa Rosa, California 95401              +
C+                    For West Associates.                             +
C+    REVISED BY : L. W. Richards and R. G. Hammarstrand               +
C+    to generate the additional data required by PLUVUE II            +
C+          for the NPS and the EPA, October 1987.                     +
C+                                                                     +
C+    PROGRAM VERSION : 02  , MODIFICATION NUMBER : 01                 +
C+    MODIFICATION DATES : 19 November 1987                            +
C+                                                                     +
C+    REVISED BY : D. DiCristofaro, Sigma Research Corporation         +
C+    to output outfile name to scratch file                           +
C+    PROGRAM VERSION : 02  , MODIFICATION NUMBER : 02                 +
C+    MODIFICATION DATE : 22 July 1992                                 +
C+                                                                     +
C+    REVISED BY : P. ECKHOFF, U.S. ENVIRONMENTAL PROTECTION AGENCY
c+    to revise method of input from Command Line interaction to 
C+    reading input data from a text file.  Lahey library calls are 
C+    no longer needed and were commented out.  Also this program was 
C+    recompiled as 32-bit and 64-bit executables.  
C+    Assigned creation date: 13220
C+    MODIFICATION DATE :  8 August 2013 
C+
C+---------------------------------------------------------------------+
C+                                                                     +
C+                  SPECIFICATION STATEMENTS                           +
C+                                                                     +
C+---------------------------------------------------------------------+
C+                                                                     +
C+    The 91 in the dimensions refers to the number of angles for      +
c+    which the MIE calculations are done, and must be .GE. NTHETA.    +
C+    The variable IT must equal this dimension, and transfers its     +
c+    value to DAMIE.                                                  +
C+                                                                     +
c+    The variable LL must equal the dimension of ACAP, and transfers  +
c+    this value to DAMIE.                                             +
C+                                                                     +
C+    LUOUT = logical unit to write data to a disk file.               +
C+    LUTT  = terminal logical unit for input of radius and sigma and  +
C+            for screen display showing the progress of the calc.     +
C+                                                                     +
C+---------------------------------------------------------------------+
C
C     TYPE DECLARATIONS
C
      REAL LAMB(20)
      COMPLEX ACAP(1600)
      LOGICAL UNDFLO
C
C     DIMENSION DECLARATIONS
C
      DIMENSION TT(91),DUM(20),PDUM(9,91),DUMA(20),DUMG(20),DUMS(20)
      DIMENSION ELTRMX(4,91,2),PIE(3,91),TAU(3,91),CSTHT(91),SI2THT(91)
      DIMENSION TT1(91)
      DIMENSION S1(91),S2(91)
      DIMENSION XNT(101),ST(101),VT(101),R(101),DR(101)
C
C     DATA DECLARATIONS
C
      DATA LAMB / 0.35,0.40,0.45,0.50,0.55,0.60,0.65,0.70,0.75,11*0.0/
      DATA NLAMB / 9 /
      DATA NTHETA / 91 /
      DATA UNDFLO /.TRUE./, LUOUT /2/, LUTT /6/
C
Cpae  Lahey construct no longer needed:
Cpae  CALL  UNDER0(UNDFLO)   ! underflow values set to zero
C
      PI = 3.14159267
      TWOPI = 2.*PI
      IT=91                                                             rP  770
      LL = 1600                                                          P  910
C
C+---------------------------------------------------------------------+
C+                                                                     +
C+                     PROGRAM STATEMENTS                              +
C+                                                                     +
C+---------------------------------------------------------------------+
      H = 180./FLOAT(NTHETA-1)
      DO 2 I = 1,NTHETA
      TT(I) = H*FLOAT(I-1)
  2   CONTINUE
C
C  Open output file and prompt user for filename.
C
      call openit
C
   3   read(LUin,*, END = 99999) ROG, SIGMA,  RFR, RFI
C  3   WRITE (LUTT,4)
C  4   FORMAT ('  Enter geometric mean radius (by volume) ROG',
C     1   /,   '  and geometric standard deviation SIGMA.',
C     2   /,   '  Separate the numbers by a blank space.',
C     3   /,   '  (A negative ROG terminates execution.): ',//)
C
C      write (LUTT, 5)
C 5    format (' Enter m and k for the index of refraction = m-i*k.',
C     1     /, ' Separate the numbers by a blank space: ',//)
C      read (*,*)
C
C  The value of POINTS controls the number of points in the histogram,
C  which is 2*POINTS + 1.  This value may be changed by the user, but
C  cannot be made larger than 50 without increasing some dimensions.
C
   50 POINTS = 50.                                                      RP  230
      K = POINTS                                                        RP  240
      ITWKP1 = 2*K + 1                                                   P  250
C
C  Terminal output confirms the input data.
C
C      WRITE (LUTT,6) ROG, SIGMA, RFR, RFI
C   6  FORMAT(1X,' ROG =',F8.4,4X,'SIGMA =',F8.4,4X,'m =',F8.4,
C     1    4X,'k =',F10.6)
      WRITE (LUOUT,266) ROG,SIGMA,RFR,RFI,NLAMB,NTHETA,ITWKP1
 266  FORMAT (3F10.4,F10.6,2I10,/
     1 31H LOG-NORMAL SIZE DISTRIBUTION (,I3,18H POINT HISTOGRAM)  )
      KP1 = K + 1                                                        P  260
      KP2 = K + 2                                                        P  270
      ALSIGM = ALOG(SIGMA)                                               P  280
      ALSGM2 = ALSIGM**2                                                 P  290
      SIGM2 = ALSGM2                                                     P  300
      SQRT2 = SQRT(2.)                                                   P  310
      RSM = EXP(ALOG(ROG)-ALSGM2)                                        P  320
      RNM = EXP(ALOG(ROG)-3.*ALSGM2)                                     P  330
      TA = 2.*ALSGM2                                                     P  340
      CN = 1./(SQRT(TWOPI)*ALSIGM)                                       P  350
      CS = PI*CN*RNM**2*EXP(TA)                                          P  360
      CV = (4./3.)*PI*CN*RNM**3*EXP(4.5*ALSGM2)                          P  370
      F1 = ALOG(30.*SQRT2*ALSIGM)                                        P  380
      F1 = F1/POINTS                                                    RP  390
      F1 = EXP(-F1)                                                      P  400
      AA = ALOG(1./F1)                                                   P  410
      F2 = 1./F1                                                         P  420
      V1 = 1.                                                            P  430
      V2 = 1.                                                            P  440
      DO 20 I = 1,K                                                      P  450
      V1 = V1*F1                                                         P  460
      R(I) = RSM*V1                                                      P  470
      ST(I) = ALGN(ALSGM2,RSM,R(I))*CS                                   P  480
      VT(I) = ALGN(ALSGM2,ROG,R(I))*CV                                   P  490
      XNT(I) = ALGN(ALSGM2,RNM,R(I))*CN                                  P  500
   20 DR(I) = AA                                                         P  510
      R(KP1) = RSM                                                       P  520
      ST(KP1) = ALGN(SIGM2, RSM,RSM)*CS                                  P  530
      VT(KP1) = ALGN(SIGM2,ROG,RSM)*CV                                   P  540
      XNT(KP1) = ALGN(SIGM2,RNM,RSM)*CN                                  P  550
      DR(KP1) = AA                                                       P  560
      DO 21 I = KP2,ITWKP1                                               P  570
      V2 = V2*F2                                                         P  580
      R(I) = RSM*V2                                                      P  590
      ST(I) = ALGN(SIGM2,RSM,R(I))*CS                                    P  600
      VT(I) = ALGN(SIGM2,ROG,R(I))*CV                                    P  610
      DR(I) = AA                                                         P  620
   21 XNT(I) = ALGN(SIGM2,RNM,R(I))*CN                                   P  630
C     DO THE INTEGRATION OVER THE DISTRIBUTIONS                          P  640
      XNTOT = 0.                                                         P  650
      STOT = 0.                                                          P  660
      VTOT = 0.                                                          P  670
      DO 30 I = 1,ITWKP1                                                 P  680
      VTOT = VTOT + VT(I)*DR(I)                                          P  690
      STOT = STOT + ST(I)*DR(I)                                          P  700
      XNTOT = XNTOT + XNT(I)*DR(I)                                       P  710
   30 CONTINUE                                                           P  720
 1000 FORMAT(1H ,10X,10E10.4)                                            P  730
  500 CONTINUE                                                           P  740
      NT=NTHETA                                                          P  750
      JX=NTHETA                                                          P  760
      DO 570 J=1,NTHETA                                                  P  780
      IF(TT(J).LE.90.) GO TO 560                                         P  790
      TT1(J)=180.-TT(J)                                                  P  800
      GO TO 570                                                          P  810
 560  CONTINUE                                                           P  820
      TT1(J)=TT(J)                                                       P  830
 570  CONTINUE                                                           P  840
      DO 40 IL = 1,NLAMB                                                 P  850
      XLAM = LAMB(IL)                                                    P  860
      WAVE = TWOPI/XLAM                                                  P  870
      EXT = 0.                                                           P  880
      SCAT = 0.                                                          P  890
      ABS = 0.                                                           P  900
      AVCOS = 0.0
      DO 31 J = 1,NT                                                     P  920
      S1(J) = 0.                                                         P  930
   31 S2(J) = 0.                                                         P  940
      DO 35 I = 1,ITWKP1                                                 P  950
      AL = WAVE*R(I)                                                     P  960
C
C  Terminal output indicates the progress of the calculation.
C
      WRITE (LUTT,32) XLAM, I
  32  FORMAT ('  Wavelength =',F5.2,'    Size no.',I4)
      CALL DAMIE(AL,RFR,RFI,TT1,JX,QEXT,QSCAT,QRPRD,ELTRMX,PIE,TAU,      P  970
     1 CSTHT,SI2THT,ACAP,IT,LL)                                          P  980
      EXT = QEXT*ST(I)*DR(I) + EXT                                       P  990
      SCAT = QSCAT*ST(I)*DR(I) +  SCAT                                   P 1000
      ABS = (QEXT-QSCAT)*ST(I)*DR(I) + ABS                               P 1010
      AVCOS = QRPRD * ST(I) * DR(I) + AVCOS
      DO 35 J=1,JX                                                       P 1020
      IF(TT(J).NE.TT1(J)) GO TO 365                                      P 1030
      S1(J) = S1(J) + ELTRMX(1,J,1)*XNT(I)*DR(I)                         P 1040
   33 S2(J) = S2(J) + ELTRMX(2,J,1)*XNT(I)*DR(I)                         P 1050
      GO TO 35                                                           P 1060
 365  CONTINUE                                                           P 1070
      JJ=J                                                               P 1080
      S1(JJ) = S1(JJ) + ELTRMX(1,J,2)*XNT(I)*DR(I)                       P 1090
   34 S2(JJ) = S2(JJ) + ELTRMX(2,J,2)*XNT(I)*DR(I)                       P 1100
   35 CONTINUE                                                           P 1110
C
      CS = XLAM**2/(PI*SCAT)                                             P 1130
      DUM(IL) = EXT/VTOT                                                 P 1140
      DUMS(IL) = SCAT/VTOT
      DUMA(IL) = ABS/VTOT
      DUMG(IL) = AVCOS/SCAT
C
C  Display results of the calculation at the terminal.
C
      WRITE (LUTT,78)XLAM,DUM(IL),DUMS(IL),DUMA(IL),DUMG(IL)
  78  FORMAT(/,
     1     '  Wave-         Cross sections for              Average',
     2  /, ' length  Extinction  Scattering  Absorption      cosine',
     3  /, 1x,F6.2,4F12.5,//)
C
C  Write results of the calculation to disk
C
      WRITE (LUOUT, 80) XLAM, DUM(IL),DUMS(IL),DUMA(IL),DUMG(IL)
  80  FORMAT (5F10.5)
      DO 36 I = 1,NT                                                     P 1150
      PDUM(IL,I) = 0.5*(S1(I)+S2(I))*CS                                  P 1160
   36 CONTINUE                                                           P 1170
      WRITE (LUOUT, 88) (PDUM(IL,I),I=1,NT)
  88  FORMAT (F8.3/(10F8.3))
   40 CONTINUE                                                           P 1180
      GO TO 3
99999 Continue
      Close (LUin)
      Close (LUout)    
      ENDPROGRAM                                                         P 1200
C
      FUNCTION ALGN(X,Y,Z)                                               Q   10
      TWOX = 2.*X                                                        Q   20
      U = (ALOG(Y/Z))**2                                                 Q   30
      ALGN = EXP(-U/TWOX)                                                Q   40
      RETURN                                                             Q   50
      ENDFUNCTION                                                        Q   60
C
      SUBROUTINE DAMIE (X,RFR,RFI,THETD,JX,QEXT,QSCAT,CTBRQS,ELTRMX,     W   10
     1 PI,TAU,CSTHT,SI2THT,ACAP,IT,LL)                                   W   20
C      LARGE VERSION                                                     W   30
C      SUBROUTINE FOR COMPUTING THE PARAMETERS OF THE ELECTROMAGNETIC    W   40
C      RADIATION SCATTERED BY A SPHERE. THIS SUBROUTINE CARRIES OUT ALL  W   50
C      COMPUTATIONS IN DOUBLE PRECISION ARITHMETIC.                      W   60
C      THIS SUBROUTINE COMPUTES THE CAPITAL A FUNCTION BY MAKING USE OF  W   70
C      DOWNWARD RECURRENCE RELATIONSHIP.                                 W   80
C      X! SIZE PARAMETER OF THE SPHERE,( 2 * PI * RADIUS OF THE SPHERE)  W   90
C      WAVELENGTH OF THE INCIDENT RADIATION).                            W  100
C      RF! REFRACTIVE INDEX OF THE MATERIAL OF THE SPHERE.  COMPLEX      W  110
C      QUANTITY..FORM! (RFR - I * RFI )                                  W  120
C      THETD(J)! ANGLE IN DEGREES BETWEEN THE DIRECTIONS OF THE INCIDENT W  130
C      AND THE SCATTERED RADIATION.  THETD(J) IS< OR= 90.0               W  140
C      IF THETD(J) SHOULD HAPPEN TO BE GREATER THAN 90.0, ENTER WITH     W  150
C      SUPPLEMENTARY VALUE                                               W  160
CSEE COMMENTS BELOW ON ELTRMX.                                           W  170
C      JX! TOTAL NUMBER OF THETD FOR WHICH THE COMPUTATIONS ARE          W  180
C      REQUIRED.  JX SHOULD NOT EXCEED IT UNLESS THE DIMENSIONS          W  190
C      STATEMENTS ARE APPROPRIATEDLY MODIFIED.                           W  200
C      MAIN PROGRAM SHOULD ALSO HAVE REAL*8 THETD(IT ),ELTRMX(4,IT ,2).  W  210
C      THE DEFINITIONS FOR THE FOLLOWING SYMBOLS CAN BE FOUND IN         W  220
       REAL   X,RX,RFR,RFI,QEXT,QSCAT,T(5),TA(4),TB(2),TC(2)             W  230
       REAL   TD(2),TE(2),CTBRQS                                         W  240
       REAL   ELTRMX(4,IT ,2),PI(3,IT ),TAU(3,IT ),CSTHT(IT ),SI2THT(IT) W  250
     1 ,THETD(IT)                                                        W  260
       COMPLEX    RF,RRF,RRFX,WM1,FNA,FNB,TC1,TC2,WFN(2)                 W  270
       COMPLEX    FNAP,FNBP,ACAP(1600)                                   W  280
C      IN THE ORIGINAL PROGRAM THE DIMENSION OF ACAP WAS 7000.           W  290
C      FOR CONSERVING SPACE THIS SHOULD BE NOT MUCH HIGHER THAN          W  300
C      THE VALUE, N=1.1*(NREAL**2 + NIMAG**2)**.5 * X + 1                W  310
C      TA(1)! REAL PART OF WFN(1).. TA(2)! IMAGINARY PART OF WFN(1)..    W  320
C      TA(3)! REAL PART OF WFN(2)..TA(4)! IMAGINARY PART OF WFN(2).      W  330
C      TB(1)! REAL PART OF FNA.. TB(2)! IMAGINARTY PART OF FNA..         W  340
C      TC(1)! REAL PART OF FNB.. TC(2)! IMAGINARY PART OF FNB..          W  350
C      TD(1)! REAL PART OF FNAP..TD(2) IMAGINARY PART OF FNAP.           W  360
C      TE(1)! REAL PART OF FNBP.. TE(2)! IMAGINARY PART OF FNBP.         W  370
C     FNAP ? FNBP ARE THE PRECEDING VALUES OF FNA ? FNB RESPECTIVELY.    W  380
C**** PRIME VERSION WORKS ONLY IN SINGLE PRECISION WITH THE              W  390
C     COMPLEX ARITHMETIC.                                                W  400
       EQUIVALENCE (WFN(1),TA(1)),(FNA,TB(1)),(FNB,TC(1))                W  410
       EQUIVALENCE(FNAP,TD(1)),(FNBP,TE(1))                              W  420
      DATA PII /3.1415926535897932/                                      W  430
      LUOUT=7                                                            W  440
C                                                                        W  450
C     FORMAT STATEMENTS FOLLOW                                           W  460
 5     FORMAT(10X,' THE VALUE OF THE SCATTERING ANGLE IS GREATER THAN    W  470
     1 90.0 DEGREES. IT IS ',E15.4)                                      W  480
 6     FORMAT(//10X,' PLEASE READ COMMENTS. '//)                         W  490
 7     FORMAT(//10X,'THE VALUE OF THE ARGUMENT JX IS GREATER THAN IT ')  W  500
 8     FORMAT(//10X,'THE UPPER LIMIT FOR ACAP IS NOT ENOUGH. SUGGEST GET W  510
     1 DETAILED OUPUT AND MODIFY SUBROUTINE'//)                          W  520
C                                                                        W  530
C     EXECUTABLE STATEMENTS FOLLOW                                       W  540
       IF(JX.LE. IT) GO TO 20                                            W  550
       WRITE (LUOUT,7)                                                   W  560
       WRITE (LUOUT,6)                                                   W  570
       CALL EXIT                                                         W  580
 20    RF =  CMPLX(RFR,-RFI)                                             W  590
       RRF = 1.0E0/RF                                                    W  600
       RX = 1.0E0/X                                                      W  610
       RRFX = RRF * RX                                                   W  620
       T(1) = (X**2)*(RFR**2 + RFI**2)                                   W  630
       T(1) =  SQRT(T(1))                                                W  640
       NMX1 = 1.10E0 * T(1)                                              W  650
C      WRITE (LUOUT,199) X,RFR,RFI,LL,NMX1                               W  660
C 199 FORMAT(1H , X= ,E14.6,2X, RFR= ,E14.6,2X, RFI= ,E14.6,2X, LL= ,I10 W  670
C    1,2X,*NMX1                             *,I10)                       W  680
       IF( NMX1 .LE. LL-1 ) GO TO 21                                     W  690
       WRITE (LUOUT,8)                                                   W  700
       STOP                                                              W  710
 21    NMX2 = T(1)                                                       W  720
       IF( NMX1 .GT. 150) GO TO 22                                       W  730
       NMX1 = 150                                                        W  740
       NMX2 = 135                                                        W  750
 22    ACAP(NMX1 + 1 ) = (0.0E0,0.0E0)                                   W  760
       DO 23 N = 1,NMX1                                                  W  770
       NN = NMX1 - N + 1                                                 W  780
       ACAP(NN)=FLOAT(NN+1)*RRFX-1.0E0/(FLOAT(NN+1)*RRFX+ACAP(NN+1))     W  790
 23    CONTINUE                                                          W  800
       DO 30 J = 1,JX                                                    W  810
       IF ( THETD(J) .LT. 0.0E0 ) THETD(J) =  ABS(THETD(J))              W  820
       IF ( THETD(J) .GT. 0.0E0 ) GO TO 24                               W  830
       CSTHT(J) = 1.0E0                                                  W  840
       SI2THT(J) = 0.0E0                                                 W  850
       GO TO 30                                                          W  860
 24    IF ( THETD(J) .GE. 90.0E0) GO TO 25                               W  870
       T(1) = ( PII * THETD(J))/180.0E0                                  W  880
       CSTHT(J) =  COS(T(1))                                             W  890
       SI2THT(J) = 1.0E0 - CSTHT(J)**2                                   W  900
       GO TO 30                                                          W  910
 25    IF ( THETD(J) .GT. 90.0E0) GO TO 28                               W  920
       CSTHT(J) = 0.0E0                                                  W  930
       SI2THT(J) = 1.0E0                                                 W  940
       GO TO 30                                                          W  950
 28    WRITE (LUOUT,5) THETD(J)                                          W  960
       WRITE (LUOUT,6)                                                   W  970
       CALL EXIT                                                         W  980
 30    CONTINUE                                                          W  990
       DO 35 J = 1,JX                                                    W 1000
       PI(1,J) = 0.0E0                                                   W 1010
       PI(2,J) = 1.0E0                                                   W 1020
       TAU(1,J) = 0.0E0                                                  W 1030
       TAU(2,J) = CSTHT(J)                                               W 1040
 35    CONTINUE                                                          W 1050
       T(1) =  COS(X)                                                    W 1060
       T(2) =  SIN(X)                                                    W 1070
       WM1 =  CMPLX( T(1),-T(2))                                         W 1080
       WFN(1) =  CMPLX(T(2),T(1))                                        W 1090
       WFN(2) = RX * WFN(1) - WM1                                        W 1100
       TC1 = ACAP(1) * RRF + RX                                          W 1110
       TC2 = ACAP(1) * RF + RX                                           W 1120
       FNA = (TC1*TA(3) - TA(1))/(TC1*WFN(2) - WFN(1))                   W 1130
       FNB = ( TC2*TA(3) - TA(1))/(TC2  * WFN(2) - WFN(1))               W 1140
       FNAP = FNA                                                        W 1150
       FNBP = FNB                                                        W 1160
       T(1) = 1.50E0                                                     W 1170
C      FROM HERE TO THE STATMENT NUMBER 90, ELTRMX(I,J,K) HAS            W 1180
C      FOLLOWING MEANING!                                                W 1190
C      ELTRMX(1,J,K)! REAL PART OF THE FIRST COMPLEX AMPLITUDE.          W 1200
C      ELTRMX(2,J,K)! IMAGINARY PART OF THE FIRST COMPLEX AMPLITUDE.     W 1210
C      ELTRMX(3,J,K)! REAL PART OF THE SECOND COMPLEX AMPLITUDE.         W 1220
C      ELTRMX(4,J,K)! IMAGINARY PART OF THE SECOND COMPLEX AMPLITUDE.    W 1230
C      K = 1 ! FOR THETD(J) AND K = 2 ! FOR 180.0 - THETD(J)             W 1240
C      DEFINITION OF THE COMPLEX AMPLITUDE! VAN DE HULST,P.125.          W 1250
       TB(1) = T(1) * TB(1)                                              W 1260
       TB(2) = T(1) * TB(2)                                              W 1270
       TC(1) = T(1) * TC(1)                                              W 1280
       TC(2) = T(1) * TC(2)                                              W 1290
       DO 60 J = 1,JX                                                    W 1300
       ELTRMX(1,J,1) = TB(1) * PI(2,J) + TC(1) * TAU(2,J)                W 1310
       ELTRMX(2,J,1) = TB(2) * PI(2,J) + TC(2) * TAU(2,J)                W 1320
       ELTRMX(3,J,1) = TC(1) * PI(2,J) + TB(1) * TAU(2,J)                W 1330
       ELTRMX(4,J,1) = TC(2) * PI(2,J) + TB(2) * TAU(2,J)                W 1340
       ELTRMX(1,J,2) = TB(1) * PI(2,J) - TC(1) * TAU(2,J)                W 1350
       ELTRMX(2,J,2) = TB(2) * PI(2,J) - TC(2) * TAU(2,J)                W 1360
       ELTRMX(3,J,2) = TC(1) * PI(2,J) - TB(1) * TAU(2,J)                W 1370
       ELTRMX(4,J,2) = TC(2) * PI(2,J) - TB(2) * TAU(2,J)                W 1380
 60    CONTINUE                                                          W 1390
       QEXT = 2.0E0 * ( TB(1) + TC(1))                                   W 1400
C     WRITE (LUOUT,999) TB(1),TC(1)                                      W 1410
       QSCAT =(TB(1)**2 + TB(2)**2 + TC(1)**2 + TC(2)**2)/0.75E0         W 1420
       CTBRQS = 0.0E0                                                    W 1430
       N = 2                                                             W 1440
 65    T(1) = 2*N - 1                                                    W 1450
       T(2) = N - 1                                                      W 1460
       T(3) = 2 * N + 1                                                  W 1470
       DO 70 J = 1,JX                                                    W 1480
       PI(3,J) = (T(1)*PI(2,J)*CSTHT(J)-FLOAT(N)*PI(1,J))/T(2)           W 1490
       TAU(3,J) = CSTHT(J)*(PI(3,J)-PI(1,J))-T(1)*SI2THT(J)*PI(2,J)+     W 1500
     1 TAU(1,J)                                                          W 1510
 70    CONTINUE                                                          W 1520
       WM1 = WFN(1)                                                      W 1530
       WFN(1) = WFN(2)                                                   W 1540
       WFN(2) = T(1)*RX*WFN(1) - WM1                                     W 1550
       TC1 = ACAP(N)*RRF + FLOAT(N)*RX                                   W 1560
       TC2 = ACAP(N)*RF + FLOAT(N)*RX                                    W 1570
       FNA = (TC1*TA(3)-TA(1))/(TC1*WFN(2) - WFN(1))                     W 1580
      TC2 = TC2                                                          W 1590
      TA(3) = TA(3)                                                      W 1600
      TA(1) = TA(1)                                                      W 1610
      TC1 = TC1                                                          W 1620
      WFN(2) = WFN(2)                                                    W 1630
      WFN(1) = WFN(1)                                                    W 1640
       FNB = (TC2*TA(3)-TA(1))/(TC2*WFN(2) - WFN(1))                     W 1650
       T(5) = N                                                          W 1660
       T(4) = T(1)/(T(5)*T(2))                                           W 1670
       T(2) = (T(2)*(T(5) + 1.0E0))/T(5)                                 W 1680
       CTBRQS = CTBRQS + T(2)*(TD(1)*TB(1) + TD(2)*TB(2) + TE(1)*TC(1) + W 1690
     1 TE(2)*TC(2))  + T(4)*(TD(1)*TE(1) + TD(2)*TE(2))                  W 1700
       QEXT = QEXT + T(3)*(TB(1)+TC(1))                                  W 1710
       T(4) = TB(1)**2 + TB(2)**2 + TC(1)**2 + TC(2)**2                  W 1720
       QSCAT = QSCAT + T(3) *T(4)                                        W 1730
       FN = N
C  FN ADDED TO PREVENT INTEGER OVERFLOW IN FOLLOWING STATEMENT
       T(2) = FN*(FN+1.)                                                RW 1740
       T(1) = T(3)/T(2)                                                  W 1750
C     WRITE (LUOUT,999) T(1),QEXT                                        W 1760
      K = (N/2)*2                                                        W 1770
       DO 80 J = 1,JX                                                    W 1780
       ELTRMX(1,J,1)=ELTRMX(1,J,1)+T(1)*(TB(1)*PI(3,J)+TC(1)*TAU(3,J))   W 1790
       ELTRMX(2,J,1)=ELTRMX(2,J,1)+T(1)*(TB(2)*PI(3,J)+TC(2)*TAU(3,J))   W 1800
       ELTRMX(3,J,1)=ELTRMX(3,J,1)+T(1)*(TC(1)*PI(3,J)+TB(1)*TAU(3,J))   W 1810
       ELTRMX(4,J,1)=ELTRMX(4,J,1)+T(1)*(TC(2)*PI(3,J)+TB(2)*TAU(3,J))   W 1820
       IF ( K .EQ. N) GO TO 75                                           W 1830
      ELTRMX(1,J,2)=ELTRMX(1,J,2) +T(1)*(TB(1)*PI(3,J)-TC(1)*TAU(3,J))   W 1840
       ELTRMX(2,J,2)=ELTRMX(2,J,2)+T(1)*(TB(2)*PI(3,J)-TC(2)*TAU(3,J))   W 1850
       ELTRMX(3,J,2)=ELTRMX(3,J,2)+T(1)*(TC(1)*PI(3,J)-TB(1)*TAU(3,J))   W 1860
       ELTRMX(4,J,2)=ELTRMX(4,J,2)+T(1)*(TC(2)*PI(3,J)-TB(2)*TAU(3,J))   W 1870
       GO TO 80                                                          W 1880
 75    ELTRMX(1,J,2)=ELTRMX(1,J,2)+T(1)*(-TB(1)*PI(3,J)+TC(1)*TAU(3,J))  W 1890
       ELTRMX(2,J,2)=ELTRMX(2,J,2)+T(1)*(-TB(2)*PI(3,J)+TC(2)*TAU(3,J))  W 1900
       ELTRMX(3,J,2)=ELTRMX(3,J,2)+T(1)*(-TC(1)*PI(3,J)+TB(1)*TAU(3,J))  W 1910
       ELTRMX(4,J,2)=ELTRMX(4,J,2)+T(1)*(-TC(2)*PI(3,J)+TB(2)*TAU(3,J))  W 1920
 80    CONTINUE                                                          W 1930
       IF( T(4) .LT.  1.0E-6 ) GO TO 100                                 W 1940
       N = N + 1                                                         W 1950
       DO 90 J = 1,JX                                                    W 1960
       PI(1,J) = PI(2,J)                                                 W 1970
       PI(2,J) = PI(3,J)                                                 W 1980
       TAU(1,J) = TAU(2,J)                                               W 1990
       TAU(2,J) = TAU(3,J)                                               W 2000
 90    CONTINUE                                                          W 2010
       FNAP = FNA                                                        W 2020
       FNBP = FNB                                                        W 2030
       IF ( N .LE. NMX2) GO TO 65                                        W 2040
       WRITE (LUOUT,8)                                                   W 2050
       CALL EXIT                                                         W 2060
 100   DO 120 J = 1,JX                                                   W 2070
       DO 120 K = 1,2                                                    W 2080
       DO  115  I= 1,4                                                   W 2090
       T(I) = ELTRMX(I,J,K)                                              W 2100
 115   CONTINUE                                                          W 2110
       ELTRMX(2,J,K) = T(1)**2 + T(2)**2                                 W 2120
       ELTRMX(1,J,K) = T(3)**2 + T(4)**2                                 W 2130
       ELTRMX(3,J,K) = T(1)*T(3) + T(2)*T(4)                             W 2140
       ELTRMX(4,J,K) = T(2)*T(3) - T(4)*T(1)                             W 2150
 120   CONTINUE                                                          W 2160
       T(1) = 2.0E0 * RX**2                                              W 2170
       QEXT = QEXT * T(1)                                                W 2180
C     WRITE (LUOUT,999) T(1),QEXT                                        W 2190
C999    FORMAT(1H ,3E20.8)                                               W 2200
       QSCAT = QSCAT * T(1)                                              W 2210
       CTBRQS = 2.E0 * CTBRQS * T(1)                                     W 2220
C      THE DETAIL ABOUT THIS SUBROUTINE CAN BE FOUND IN THE FOLLOWING    W 2230
C      REPORT!                               SUBROUTINES FOR COMPUTING T W 2240
C      ELECTROMAGNETIC RADIATION SCATTERED BY A SPHERE                   W 2250
C      I B M SCIENTIFIC CENTER, PALO ALTO , CALIFORNIA.                  W 2260
C      REPORT NO. 320 - 3236 .. MAY 1968 ..                              W 2270
99999  RETURN                                                            W 2280
       ENDSUBROUTINE                                                     W 2290


      SUBROUTINE OPENIT
C
C Opens an input ASCII file that was manually created by the user.  If
C the file does not exist, an error message is created.  An output file
C is also created or an old file is over written.  The filenames are 
C hard coded as MieVal.inp and Mietbl.out.
C
      integer*4 luin, luout, ioval
C
C      data  luin/8/, luout/9/
C
      open(unit=luin, file='MieVal.inp', iostat=ioval)
      open(unit=luout, file='Mietbl.out')
c
      if (ioval .eq. 1002) then
          write (luout, *) "File not found."
          stop
      endif
      if (ioval .gt. 0) then
          write (luout, *) "Error code: ",ioval
          stop
      endif
C 1001 write(luout,*) "Error occurred in opening MIEVAL.inp"
C        stop
      ENDSUBROUTINE
