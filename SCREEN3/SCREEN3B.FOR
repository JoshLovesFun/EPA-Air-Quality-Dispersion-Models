      SUBROUTINE WSADJ
C***********************************************************************
C                 WSADJ Module of SCREEN2 Short Term Model
C
C        PURPOSE: Adjusts Wind Speed from Anemometer Height to Stack Height
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        INPUTS:  Arrays of Source Parameters
C                 Meteorological Variables for One Hour
C                 Wind Speed Profile Exponents (Default or User-defined)
C
C        OUTPUTS: Stack Top Wind Speed, US
C
C        CALLED FROM:   PCALC
C                       VCALC
C                       ACALC
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN.INC'

C     Variable Initializations
CISC2      MODNAM = 'WSADJ'

C     Adjust Wind Speed -- Assume Wind Speed Constant Below 10 meters
      IF (HS .GE. 10.0) THEN
         US = UREF * (HS/ZREF)**P
      ELSE IF (ZREF .GT. 10.0) THEN
         US = UREF * (10.0/ZREF)**P
      ELSE
         US = UREF
      END IF

C     Do Not Allow Stack Height Wind Speed < 1.0 m/s
      IF (US .LT. 1.0) THEN
         US = 1.0
      END IF

      RETURN
      END

      SUBROUTINE DISTF
C***********************************************************************
C                 DISTF Module of SCREEN2 Short Term Model
C
C        PURPOSE: Calculates Distance to Final Plume Rise
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Arrays of Source Parameters
C                 Buoyancy and Momentum Fluxes
C                 Meteorological Variables for One Hour
C                 Wind Speed Adjusted to Stack Height
C
C        OUTPUTS: Distance to Final Plume Rise, XF (m), and Distance
C                 to Final Buoyant Rise (XFB) and Final Momentum Rise (XFM)
C
C        CALLED FROM:   PCALC
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN.INC'

C     Variable Initializations
CISC2      MODNAM = 'DISTF'

      IF (UNSTAB .OR. NEUTRL) THEN
         IF (FB .GE. 55.) THEN
            XFB = 119. * FB**0.4
         ELSE IF (FB .GT. 0.) THEN
            XFB = 49. * FB**0.625
         ELSE
            XFB = 4.*DS*(VS+3.*US)*(VS+3.*US)/(VS*US)
         END IF
         XFM = 4.*DS*(VS+3.*US)*(VS+3.*US)/(VS*US)
         XF = AMAX1(XFB,XFM)
      ELSE IF (STABLE) THEN
         XFB = 2.0715*US/RTOFS
         XFM = 0.5*PI*US/RTOFS
         XF = AMAX1(XFB,XFM)
      END IF

      RETURN
      END

      SUBROUTINE WAKFLG
C***********************************************************************
C                 WAKFLG Module of SCREEN2 Short Term Model
C
C        PURPOSE: To Set Wake Flags for Building Downwash Algorithms
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Building Dimensions
C                 Source Parameters
C                 Meteorological Variables for One Hour
C
C        OUTPUTS: Logical Flags for Wake Switches, WAKE and WAKESS;
C                 And Building Types, TALL, SQUAT, and SSQUAT;
C                 And Value of ZLB
C
C        CALLED FROM:   PCALC
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN.INC'

C     Variable Initializations
CISC2      MODNAM = 'WAKFLG'

C     Set Initial Wake Switches Based on Building Dimensions
      IF (DSBH.EQ.0.0 .OR. DSBW.EQ.0.0 .OR.
     &    HS .GT. (DSBH + 1.5*AMIN1(DSBH,DSBW))) THEN
         WAKE   = .FALSE.
         WAKESS = .FALSE.
      ELSE IF (HS .GT. (DSBH + 0.5*AMIN1(DSBH,DSBW))) THEN
         WAKE   = .TRUE.
         WAKESS = .FALSE.
      ELSE
         WAKE   = .TRUE.
         WAKESS = .TRUE.
      END IF

C     Set Final Wake Switches Based on Plume Height
      IF (WAKE) THEN
         X2BH = DSBH + DSBH
C        Calculate Gradual Momentum Rise at X2BH            ---   CALL DHPMOM
         CALL DHPMOM(X2BH)
         HEMWAK = HS + DHPM
         IF (WAKESS) THEN
            IF (HEMWAK .LE. (DSBH + 2.0*AMIN1(DSBH,DSBW))) THEN
               WAKE   = .TRUE.
            ELSE
               WAKE   = .FALSE.
               WAKESS = .FALSE.
            END IF
         ELSE
            IF (HEMWAK .LE. (DSBH + 1.5*AMIN1(DSBH,DSBW))) THEN
               WAKE = .TRUE.
            ELSE
               WAKE = .FALSE.
            END IF
         END IF
      ELSE
         HEMWAK = 0.0
      END IF

C     Set Value of ZLB And Set Logical Flags for Building Type
      IF (WAKE) THEN
         ZLB = AMIN1(DSBH,DSBW)
         IF (DSBW .LT. DSBH) THEN
C           Tall Building
            TALL  = .TRUE.
            SQUAT = .FALSE.
            SSQUAT= .FALSE.
         ELSE IF (DSBW .LE. 5.*DSBH) THEN
C           Squat Building
            TALL  = .FALSE.
            SQUAT = .TRUE.
            SSQUAT= .FALSE.
         ELSE
C           Super-Squat Building
            TALL  = .FALSE.
            SQUAT = .FALSE.
            SSQUAT= .TRUE.
         END IF
      ELSE
         ZLB = 0.0
      END IF

      RETURN
      END

      SUBROUTINE DELH
C***********************************************************************
C                 DELH Module of SCREEN2 Model
C
C        PURPOSE: To Calculate Final Plume Rise
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Source Parameters
C                 Meteorological Variables
C                 Buoyancy and Momentum Fluxes
C
C        OUTPUTS: Final Plume Rise, DHF (m)
C
C        CALLED FROM:   PHEFF
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN.INC'

C     Variable Initializations
CISC2      MODNAM = 'DELH'

C     Calculate Delta-T
      DELT = TS - TA

      IF (UNSTAB .OR. NEUTRL) THEN
         IF (FB .GE. 55.) THEN
            DTCRIT = 0.00575*TS*((VS*VS)/DS)**0.333333
         ELSE   
            DTCRIT = 0.0297*TS*(VS/(DS*DS))**0.333333
         END IF
         IF (DELT .GE. DTCRIT) THEN
            BUOYNT = .TRUE. 
         ELSE   
            BUOYNT = .FALSE.
         END IF
         IF (BUOYNT) THEN   
            IF (FB .GE. 55.) THEN
               DHF = 38.71*(FB**0.6)/US
            ELSE
               DHF = 21.425*(FB**0.75)/US
            END IF
         ELSE   
            DHF = 3.*DS*VS/US
         END IF

      ELSE IF (STABLE) THEN
         DTCRIT = 0.019582*VS*TA*RTOFS
         IF (DELT .GE. DTCRIT) THEN
            BUOYNT = .TRUE. 
         ELSE   
            BUOYNT = .FALSE.
         END IF
         IF (BUOYNT) THEN   
            DHF = 2.6*(FB/(US*S))**0.333333
C           Compare to Final Plume Rise for Calm Winds, DHCLM
            DHCLM = 4.*FB**0.25/S**0.375
            IF (DHCLM .LT. DHF) DHF = DHCLM
         ELSE   
            DHF = 1.5*(FM/(US*RTOFS))**0.333333
C           Compare to Maximum Momentum Rise for UNSTABLE/NEUTRAL, DHCHK
            DHCHK = 3.*DS*VS/US
            IF (DHCHK .LT. DHF) DHF = DHCHK
         END IF
      END IF

      RETURN
      END

      FUNCTION HSPRIM(US,VS,HS,DS)
C***********************************************************************
C                 HSPRIM Module of the ISC Model - Version 2
C
C        PURPOSE: Calculates Stack Height Adjusted for Stack
C                 Tip Downwash (HS')
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Arrays of Source Parameters
C                 Wind Speed Adjusted to Stack Height
C
C        OUTPUTS: Adjusted Stack Height (m)
C
C        CALLED FROM:   PHEFF
C***********************************************************************

C     Variable Declarations
CISC2      CHARACTER MODNAM*6
C     Variable Initializations
CISC2      MODNAM = 'HSPRIM'

C     Calculate Adjusted Stack Height (Eqn. 1-7)

      IF (VS .LT. 1.5*US) THEN
         HSPRIM = HS - 2.*DS*(1.5-VS/US)
      ELSE
         HSPRIM = HS
      END IF

      IF (HSPRIM .LT. 0.0)  HSPRIM = 0.0

      RETURN
      END

      SUBROUTINE DHPHS(XARG)
C***********************************************************************
C                 DHPHS Module of SCREEN2 Model
C
C        PURPOSE: Calculates Distance-dependent Plume Rise for
C                 Huber-Snyder Downwash Algorithm and for BID
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Arrays of Source Parameters
C                 Buoyancy and Momentum Fluxes
C                 Meteorological Variables for One Hour
C                 Wind Speed Adjusted to Stack Height
C                 Downwind Distance
C
C        OUTPUTS: Distance-dependent Plume Rise, DHP (m)
C
C        CALLED FROM:   PHEFF
C                       BID
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN.INC'

C     Variable Initializations
CISC2      MODNAM = 'DHPHS'

C     Calculate Distance-dependent Buoyant Plume Rise (Eqn. 1-22)
      XP = AMIN1(XARG,XFB)
      IF (XP .LT. 1.0)  XP = 1.0
      IF (FB .LT. 1.0E-10)  FB = 1.0E-10
      DHPB = 1.60 * (FB*XP*XP)**0.333333 / US

C     Calculate Dist-dependent Momentum Plume Rise          ---   CALL DHPMOM
      CALL DHPMOM(XARG)

C     Select Maximum of Buoyant or Momentum Rise for Gradual Rise
      DHP = AMAX1(DHPB, DHPM)
C     Compare to Final Rise and Select Smaller Value for Gradual Rise
      DHP = AMIN1(DHP, DHF)

      RETURN
      END

      SUBROUTINE DHPMOM(XARG)
C***********************************************************************
C                 DHPMOM Module of SCREEN2 Model
C
C        PURPOSE: Calculates Distance-dependent Momentum Plume Rise
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Arrays of Source Parameters
C                 Buoyancy and Momentum Fluxes
C                 Meteorological Variables for One Hour
C                 Wind Speed Adjusted to Stack Height
C                 Downwind Distance
C
C        OUTPUTS: Distance-dependent Momentum Plume Rise, DHPM (m)
C
C        CALLED FROM:   WAKFLG
C                       DHPHS
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN.INC'

C     Variable Initializations
CISC2      MODNAM = 'DHPMOM'

C     Calculate BETAJ Parameter (Entrainment Coefficient)
      BETAJ = 0.333333 + US/VS

      IF (UNSTAB .OR. NEUTRL) THEN
         XP = AMIN1(XARG,XFM)
         DHPM = (3.*FM*XP/(BETAJ*BETAJ*US*US))**0.333333
      ELSE IF (STABLE) THEN
         XP = AMIN1(XARG,XFM)
         DHPM = 3.*FM*SIN(RTOFS*XP/US) / (BETAJ*BETAJ*US*RTOFS)
C        Set Lower Limit for DHPM to Avoid Negative Arg for Cube Root
         DHPM = AMAX1(1.0E-10, DHPM)
         DHPM = DHPM ** 0.333333
      END IF

C     Do Not Let Gradual Rise Exceed Final Momentum Rise
      DHPM = AMIN1(DHPM, 3.*DS*VS/US)

      RETURN
      END

      SUBROUTINE DHPSS(XARG)
C***********************************************************************
C                 DHPSS Module of SCREEN2 Model
C
C        PURPOSE: Calculates Distance-dependent Plume Rise for
C                 Schulman-Scire Downwash Algorithm
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To Change TOL from 1.0E-5 to 1.0E-4
C
C        INPUTS:  Arrays of Source Parameters
C                 Buoyancy and Momentum Fluxes
C                 Meteorological Variables for One Hour
C                 Wind Speed Adjusted to Stack Height
C                 Downwind Distance
C                 Wake Plume Height, HEMWAK
C
C        OUTPUTS: Distance-dependent BLP Plume Rise, DHP (m)
C
C        CALLED FROM:   PHEFF
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN.INC'

C     Variable Initializations
CISC2      MODNAM = 'DHPSS'

C     Determine BLP Line Source Parameters, ZLY and RINIT

      X3LB = 3.*ZLB
      IF (HEMWAK .GT. 1.2*DSBH) THEN
         ZLY = 0.0
C        Calculate Building Enhanced Sigma-z at X3LB        ---   CALL SZENH
         CALL SZENH(X3LB)
      ELSE
C        Calculate Building Enhanced Sigma-y at X3LB        ---   CALL SYENH
         CALL SYENH(X3LB)
C        Calculate Building Enhanced Sigma-z at X3LB        ---   CALL SZENH
         CALL SZENH(X3LB)
         IF (SY .GE. SZ) THEN
C           Note That SRT2PI = SQRT(2.*PI)
            ZLY = SRT2PI*(SY-SZ)
         ELSE
            ZLY = 0.0
         END IF
      END IF
C     Calculate Initial Radius of Plume, RINIT = SQRT(2.)*SZ
      RINIT = 1.414214 * SZ

C     Determine Coefficients A, B and C of Cubic Equation

      A = 3.*ZLY/(PI*BETA) + 3.*RINIT/BETA
      B = 6.*RINIT*ZLY/(PI*BETA*BETA) + 3.*RINIT*RINIT/(BETA*BETA)
C     Compute Coefficient C for Buoyant Rise (CB)           ---   CALL BLPCB
      CALL BLPCB(XARG)
C     Compute Coefficient C for Momentum Rise (CM)          ---   CALL BLPCM
      CALL BLPCM(XARG)

C     Solve Cubic Equation With Buoyant Rise (CB) and Momentum Rise (CM)
C     and Select the Larger of the Two as the Gradual Plume Rise, DHP.
C     Set TOLerance Limit to 1.0E-4, and Initial Guess to Cube Root of C.
      TOL = 1.0E-4

C     First Check For Non-zero CB, To Avoid Zero-divide in CUBIC
      IF (CB .LT. -1.0E-5) THEN
         ZINIT = ABS(CB) ** 0.333333
      ELSE
         CB = -1.0E-5
         ZINIT = 0.01
      END IF
C     Solve Cubic Equation for Buoyant Rise, ZB             ---   CALL CUBIC
      CALL CUBIC(A,B,CB,ZINIT,TOL,ZB)

C     First Check For Non-zero CM, To Avoid Zero-divide in CUBIC
      IF (CM .LT. -1.0E-5) THEN
         ZINIT = ABS(CM) ** 0.333333
      ELSE
         CM = -1.0E-5
         ZINIT = 0.01
      END IF
C     Solve Cubic Equation for Momentum Rise, ZM            ---   CALL CUBIC
      CALL CUBIC(A,B,CM,ZINIT,TOL,ZM)

      DHP = AMAX1(ZB,ZM)

      RETURN
      END

      SUBROUTINE BLPCB(XARG)
C***********************************************************************
C                 BLPCB Module of SCREEN2 Model
C
C        PURPOSE: Calculates C Coefficient for BLP Buoyant Rise Used in
C                 Schulman-Scire Downwash Algorithm
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Arrays of Source Parameters
C                 Buoyancy and Momentum Fluxes
C                 Meteorological Variables for One Hour
C                 Wind Speed Adjusted to Stack Height
C                 Downwind Distance
C
C        OUTPUTS: Coefficient CB
C
C        CALLED FROM:   DHPSS
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN.INC'

C     Variable Initializations
CISC2      MODNAM = 'BLPCB'

C     Compute Coefficient for Buoyant BLP Rise

      IF (UNSTAB .OR. NEUTRL) THEN
         XP = AMIN1(XARG,XFB)
         CB = -3.*FB*XP*XP/(2.*BETA*BETA*US*US*US)
      ELSE IF (STABLE) THEN
         XP = AMIN1(XARG,XFB)
         CBS = 6.*FB/(BETA*BETA*US*S)
C        Compare Stable Term to Neutral Term
         CBN = 3.*FB*XP*XP/(2.*BETA*BETA*US*US*US)
C        Select Minimum of Stable and Neutral Term
         CB = -1.*AMIN1(CBS,CBN)
      END IF

      RETURN
      END

      SUBROUTINE BLPCM(XARG)
C***********************************************************************
C                 BLPCM Module of SCREEN2 Model
C
C        PURPOSE: Calculates C Coefficient for BLP Momentum Rise Used in
C                 Schulman-Scire Downwash Algorithm
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Arrays of Source Parameters
C                 Buoyancy and Momentum Fluxes
C                 Meteorological Variables for One Hour
C                 Wind Speed Adjusted to Stack Height
C                 Downwind Distance
C
C        OUTPUTS: Coefficient CM
C
C        CALLED FROM:   DHPSS
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN.INC'

C     Variable Initializations
CISC2      MODNAM = 'BLPCM'

C     Calculate BETAJ Parameter (Entrainment Coefficient)
      BETAJ = 0.333333 + US/VS

C     Compute Coefficient for Momentum BLP Rise

      IF (UNSTAB .OR. NEUTRL) THEN
         XP = AMIN1(XARG,XFM)
         CM = -3.*FM*XP/(BETAJ*BETAJ*US*US)
      ELSE IF (STABLE) THEN
         XP = AMIN1(XARG,XFM)
         CMS = 3.*FM*SIN(RTOFS*XP/US)/(BETAJ*BETAJ*US*RTOFS)
C        Compare Stable Term to Neutral Term
         XFMN = 4.*DS*(VS+3.*US)*(VS+3.*US)/(VS*US)
         XP = AMIN1(XARG,XFMN)
         CMN = 3.*FM*XP/(BETAJ*BETAJ*US*US)
C        Select Minimum of Stable and Neutral Term
         CM = -1.*AMIN1(CMS,CMN)
      END IF

      RETURN
      END

      SUBROUTINE CUBIC(A,B,C,ZINIT,TOL,ZITER)
C***********************************************************************
C                 CUBIC Module of SCREEN2 Model
C
C        PURPOSE: Solves Cubic Equation Using Newton's Method
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Coefficients (A, B and C) of Cubic Equation
c                 Initial Guess for Variable
C                 Tolerance Level for Iteration
C
C        OUTPUTS: Solution to Cubic Equation;
C                    Z**3 + A*Z**2 + B*Z + C = 0
C
C        CALLED FROM:   DHPSS
C                       XVZ
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN.INC'
      REAL Z(25)

C     Variable Initializations
CISC2      MODNAM = 'CUBIC'

C     Assign Initial Guess to Z(1)
      Z(1) = ZINIT

C     Begin Iterative LOOP (24 iterations)
      DO 20 N = 1, 24
C        Calculate Cubic Function and First Derivative With Current Guess
         FZ = Z(N)*Z(N)*Z(N) + A*Z(N)*Z(N) + B*Z(N) + C
         FP = 3.*Z(N)*Z(N) + 2.*A*Z(N) + B
C        Calculate New Guess
         Z(N+1) = Z(N) - FZ/FP
C        Check successive iterations for specified tolerance level
         IF (ABS(Z(N+1) - Z(N)) .LE. TOL) THEN
            ZITER = Z(N+1)
C           Exit Loop
            GO TO 999
         END IF
 20   CONTINUE
C     End Iterative LOOP

C     If No Convergence In Loop, Then Use Average of Last Two Estimates,
C     and Write Information Message
CISC2      WRITE(DUMMY,'(I8)') KURDAT
CISC2      CALL ERRHDL(PATH,MODNAM,'I','400',DUMMY)
      WRITE(IOUT,*) 'NO CONVERGENCE IN SUBROUTINE CUBIC'
      ZITER = 0.5 * (Z(24) + Z(25))

 999  RETURN
      END

      SUBROUTINE SIGY(XARG)
C***********************************************************************
C                 SIGY Module of SCREEN2 Model
C
C        PURPOSE: Calculates Sigma-y Values From Dispersion Curves
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Downwind Distance
C                 Stability Class
C                 Rural or Urban Dispersion Option
C
C        OUTPUTS: Lateral Dispersion Coefficient, SY
C
C        CALLED FROM:   PDIS
C                       VDIS
C                       ADIS
C                       SYENH
C                       DHPSS
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN.INC'

C     Variable Initializations
CISC2      MODNAM = 'SIGY'

C     Convert Distance to km
      XKM = XARG * 0.001

C     Determine Sigma-y Based on RURAL/URBAN, Stability Class, and Distance.
C     Stability Classes are Checked in the Order 4, 5, 6, 1, 2, 3
C     For Optimization, Since Neutral and Stable are Generally the Most
C     Frequent Classes.

      IF (RURAL) THEN
         IF (KST .EQ. 4) THEN
            TH = (8.3330 - 0.72382*ALOG(XKM)) * DTORAD
         ELSE IF (KST .EQ. 5) THEN
            TH = (6.25 - 0.54287*ALOG(XKM)) * DTORAD
         ELSE IF (KST .EQ. 6) THEN
            TH = (4.1667 - 0.36191*ALOG(XKM)) * DTORAD
         ELSE IF (KST .EQ. 1) THEN
            TH = (24.1667 - 2.5334*ALOG(XKM)) * DTORAD
         ELSE IF (KST .EQ. 2) THEN
            TH = (18.333 - 1.8096*ALOG(XKM)) * DTORAD
         ELSE IF (KST .EQ. 3) THEN
            TH = (12.5 - 1.0857*ALOG(XKM)) * DTORAD
         END IF
C
C        NOTE THAT 465.11628 = 1000. (m/km) / 2.15
C
         SY = 465.11628 * XKM * TAN(TH)
      ELSE IF (URBAN) THEN
         IF (KST .EQ. 4) THEN
            SY = 160.*XKM/SQRT(1.+0.4*XKM)
         ELSE IF (KST .GE. 5) THEN
            SY = 110.*XKM/SQRT(1.+0.4*XKM)
         ELSE IF (KST .LE. 2) THEN
            SY = 320.*XKM/SQRT(1.+0.4*XKM)
         ELSE IF (KST .EQ. 3) THEN
            SY = 220.*XKM/SQRT(1.+0.4*XKM)
         END IF
      END IF

      RETURN
      END

      SUBROUTINE SIGZ(XARG)
C***********************************************************************
C                 SIGZ Module of SCREEN2 Model
C
C        PURPOSE: Calculates Sigma-z Values From Dispersion Curves
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Downwind Distance
C                 Stability Class
C                 Rural or Urban Dispersion Option
C
C        OUTPUTS: Vertical Dispersion Coefficient, SZ
C
C        CALLED FROM:   PDIS
C                       VDIS
C                       ADIS
C                       SZENH
C                       DHPSS
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN.INC'

C     Variable Initializations
CISC2      MODNAM = 'SIGZ'

C     Convert Distance to km
      XKM = XARG * 0.001

C     Determine Sigma-z Based on RURAL/URBAN, Stability Class, and Distance.
C     Stability Classes are Checked in the Order 4, 5, 6, 1, 2, 3
C     For Optimization, Since Neutral and Stable are Generally the Most
C     Frequent Classes.

      IF (RURAL) THEN
C        Retrieve Coefficients, A and B                     ---   CALL SZCOEF
         CALL SZCOEF(XKM,A,B,XMIN,XMAX)
         SZ = A*XKM**B
      ELSE IF (URBAN) THEN
         IF (KST .EQ. 4) THEN
            SZ = 140.*XKM/SQRT(1.+0.3*XKM)
         ELSE IF (KST .GE. 5) THEN
            SZ = 80.*XKM/SQRT(1.+1.5*XKM)
         ELSE IF (KST .LE. 2) THEN
            SZ = 240.*XKM*SQRT(1.+XKM)
         ELSE IF (KST .EQ. 3) THEN
            SZ = 200.*XKM
         END IF
      END IF

      RETURN
      END

      SUBROUTINE SZCOEF(XKM,A,B,XMIN,XMAX)
C***********************************************************************
C                 SZCOEF Module of SCREEN2 Model
C
C        PURPOSE: Determines Coefficients and Ranges for Rural Sigma-z
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  KST     Stability Category
C                 XKM     Downwind Distance (km)
C
C        OUTPUTS: Coefficients A and B and Distance Range XMIN and XMAX
C
C        CALLED FROM:   SIGZ
C                       XVZ
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN.INC'

C     Variable Initializations
CISC2      MODNAM = 'SZCOEF'

      IF (KST .EQ. 4) THEN
         IF (XKM .LE. .30) THEN
            A = 34.459
            B = 0.86974
            XMIN = 0.
            XMAX = 0.30
         ELSE IF (XKM .LE. 1.0) THEN
            A = 32.093
            B = 0.81066
            XMIN = 0.30
            XMAX = 1.
         ELSE IF (XKM .LE. 3.0) THEN
            A = 32.093
            B = 0.64403
            XMIN = 1.
            XMAX = 3.
         ELSE IF (XKM .LE. 10.) THEN
            A = 33.504
            B = 0.60486
            XMIN = 3.
            XMAX = 10.
         ELSE IF (XKM .LE. 30.) THEN
            A = 36.650
            B = 0.56589
            XMIN = 10.
            XMAX = 30.
         ELSE 
            A = 44.053
            B = 0.51179
            XMIN = 30.
            XMAX = 100.
         END IF

      ELSE IF (KST .EQ. 5) THEN
         IF (XKM .LE. .10) THEN
            A = 24.26
            B = 0.83660
            XMIN = 0.
            XMAX = .10
         ELSE IF (XKM .LE. .30) THEN
            A = 23.331
            B = 0.81956
            XMIN = 0.10
            XMAX = 0.30
         ELSE IF (XKM .LE. 1.0) THEN
            A = 21.628
            B = 0.75660
            XMIN = 0.30
            XMAX = 1.
         ELSE IF (XKM .LE. 2.0) THEN
            A = 21.628
            B = 0.63077
            XMIN = 1.
            XMAX = 2.
         ELSE IF (XKM .LE. 4.0) THEN
            A = 22.534
            B = 0.57154
            XMIN = 2.
            XMAX = 4.
         ELSE IF (XKM .LE. 10.) THEN
            A = 24.703
            B = 0.50527
            XMIN = 4.
            XMAX = 10.
         ELSE IF (XKM .LE. 20.) THEN
            A = 26.97
            B = 0.46713
            XMIN = 10.
            XMAX = 20.
         ELSE IF (XKM .LE. 40.) THEN
            A = 35.42
            B = 0.37615
            XMIN = 20.
            XMAX = 40.
         ELSE 
            A = 47.618
            B = 0.29592
            XMIN = 40.
            XMAX = 100.
         END IF

      ELSE IF (KST .EQ. 6) THEN
         IF (XKM .LE. .20) THEN
            A = 15.209
            B = 0.81558
            XMIN = 0.
            XMAX = 0.20
         ELSE IF (XKM .LE. .70) THEN
            A = 14.457
            B = 0.78407
            XMIN = 0.20
            XMAX = 0.70
         ELSE IF (XKM .LE. 1.0) THEN
            A = 13.953
            B = 0.68465
            XMIN = 0.7
            XMAX = 1.
         ELSE IF (XKM .LE. 2.0) THEN
            A = 13.953
            B = 0.63227
            XMIN = 1.
            XMAX = 2.
         ELSE IF (XKM .LE. 3.0) THEN
            A = 14.823
            B = 0.54503
            XMIN = 2.
            XMAX = 3.
         ELSE IF (XKM .LE. 7.0) THEN
            A = 16.187
            B = 0.46490
            XMIN = 3.
            XMAX = 7.
         ELSE IF (XKM .LE. 15.) THEN
            A = 17.836
            B = 0.41507
            XMIN = 7.
            XMAX = 15.
         ELSE IF (XKM .LE. 30.) THEN
            A = 22.651
            B = 0.32681
            XMIN = 15.
            XMAX = 30.
         ELSE IF (XKM .LE. 60.) THEN
            A = 27.074
            B = 0.27436
            XMIN = 30.
            XMAX = 60.
         ELSE 
            A = 34.219
            B = 0.21716
            XMIN = 60.
            XMAX = 100.
         END IF

      ELSE IF (KST .EQ. 1) THEN
         IF (XKM .LE. 0.10) THEN
            A = 122.8
            B = 0.94470
            XMIN = 0.
            XMAX = 0.1
         ELSE IF (XKM .LE. 0.15) THEN
            A = 158.080
            B = 1.05420
            XMIN = 0.1
            XMAX = 0.15
         ELSE IF (XKM .LE. 0.20) THEN
            A = 170.22
            B = 1.09320
            XMIN = 0.15
            XMAX = 0.20
         ELSE IF (XKM .LE. 0.25) THEN
            A = 179.52
            B = 1.12620
            XMIN = 0.20
            XMAX = 0.25
         ELSE IF (XKM .LE. 0.30) THEN
            A = 217.41
            B = 1.2644
            XMIN = 0.25
            XMAX = 0.30
         ELSE IF (XKM .LE. 0.40) THEN
            A = 258.89
            B = 1.4094
            XMIN = 0.30
            XMAX = 0.40
         ELSE IF (XKM .LE. 0.50) THEN
            A = 346.75
            B = 1.72830
            XMIN = 0.40
            XMAX = 0.50
         ELSE 
            A = 453.85
            B = 2.11660
            XMIN = 0.50
            XMAX = 100.
         END IF

      ELSE IF (KST .EQ. 2) THEN
         IF (XKM .LE. 0.20) THEN
            A = 90.673
            B = 0.93198
            XMIN = 0.
            XMAX = 0.20
         ELSE IF (XKM .LE. 0.40) THEN
            A = 98.483
            B = 0.98332
            XMIN = 0.20
            XMAX = 0.40
         ELSE 
            A = 109.3
            B = 1.0971
            XMIN = 0.40
            XMAX = 100.
         END IF

      ELSE IF (KST .EQ. 3) THEN
            A = 61.141
            B = 0.91465
            XMIN = 0.
            XMAX = 100.
      END IF

      RETURN
      END

      SUBROUTINE SYENH(XARG)
C***********************************************************************
C                 SYENH Module of SCREEN2 Model
C
C        PURPOSE: Calculates Building Enhanced Sigma-y Values (>= Curves)
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Downwind Distance
C                 Building Dimensions
C                 Stability Class
C
C        OUTPUTS: Lateral Dispersion Coefficient, Sigma-y
C
C        CALLED FROM:  PDIS
C                      DHPSS
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN.INC'

C     Variable Initializations
CISC2      MODNAM = 'SYENH'

      IF (TALL) THEN
         IF (XARG .LT. 10.*ZLB) THEN
C           Calculate Building Enhanced Sigma-y (Eqn. 1-45)
            SY1 = 0.35*ZLB + 0.067*(XARG - 3.*ZLB)
C           Calculate Sigma-y from Dispersion Curves, SY   ---   CALL SIGY
            CALL SIGY(XARG)
            SY = AMAX1(SY1,SY)
         ELSE
C           Calculate Building Enhanced Sigma-y at 10*ZLB
            SYINIT = 0.85*ZLB
CC**********************************************************************
CC         The Following Commented Line Removes Rounding From Estimate
CC         of SYINIT at 10*ZLB.  Rounding in Original ISC Model Causes
CC         Slight Discontinuity at 10*ZLB.
CC
CC            SYINIT = 0.819*ZLB
CC**********************************************************************
C           Calculate Lateral Virtual Distance              ---   CALL XVY
            CALL XVY
            XY = AMAX1(0.0, (XY-10.*ZLB))
C           Calculate Sigma-y from Curves for X+XY          ---   CALL SIGY
            CALL SIGY(XARG+XY)
         END IF

      ELSE IF (SQUAT) THEN
         IF (XARG .LT. 10.*ZLB) THEN
C           Calculate Buidling Enhanced Sigma-y (Eqn. A-41)
            SY1 = 0.35*DSBW + 0.067*(XARG - 3.*DSBH)
C           Calculate Sigma-y from Dispersion Curves, SY   ---   CALL SIGY
            CALL SIGY(XARG)
            SY = AMAX1(SY1,SY)
         ELSE
C           Calculate Building Enhanced Sigma-y at 10*ZLB
            SYINIT = 0.35*DSBW + 0.5*DSBH
CC**********************************************************************
CC         The Following Commented Line Removes Rounding From Estimate
CC         of SYINIT at 10*ZLB.  Rounding in Original ISC Model Causes
CC         Slight Discontinuity at 10*ZLB.
CC
CC            SYINIT = 0.35*DSBW + 0.469*DSBH
CC**********************************************************************
C           Calculate Lateral Virtual Distance              ---   CALL XVY
            CALL XVY
            XY = AMAX1(0.0, (XY-10.*ZLB))
C           Calculate Sigma-y from Curves for X+XY          ---   CALL SIGY
            CALL SIGY(XARG+XY)
         END IF

      ELSE IF (SSQUAT) THEN
         IF (XARG .LT. 10.*ZLB) THEN
C           Calculate Building Enhanced Sigma-y
            IF (WAKLOW) THEN
C              Use Eqn. 1-44 for "Lower Bound" Estimate
               SY1 = 1.75*ZLB + 0.067*(XARG - 3.*ZLB)
            ELSE
C              Use Eqn. 1-43 for "Upper Bound" Estimate
               SY1 = 0.35*ZLB + 0.067*(XARG - 3.*ZLB)
            END IF
C           Calculate Sigma-y from Dispersion Curves, SY    ---   CALL SIGY
            CALL SIGY(XARG)
            SY = AMAX1(SY1,SY)
         ELSE
C           Calculate Building Enhanced Sigma-y at 10*ZLB
            IF (WAKLOW) THEN
               SYINIT = 2.25*ZLB
CC**********************************************************************
CC         The Following Commented Line Removes Rounding From Estimate
CC         of SYINIT at 10*ZLB.  Rounding in Original ISC Model Causes
CC         Slight Discontinuity at 10*ZLB.
CC
CC               SYINIT = 2.219*ZLB
CC**********************************************************************
            ELSE
               SYINIT = 0.85*ZLB
CC**********************************************************************
CC         The Following Commented Line Removes Rounding From Estimate
CC         of SYINIT at 10*ZLB.  Rounding in Original ISC Model Causes
CC         Slight Discontinuity at 10*ZLB.
CC
CC               SYINIT = 0.819*ZLB
CC**********************************************************************
            END IF
C           Calculate Lateral Virtual Distance              ---   CALL XVY
            CALL XVY
            XY = AMAX1(0.0, (XY-10.*ZLB))
C           Calculate Sigma-y from Curves for X+XY          ---   CALL SIGY
            CALL SIGY(XARG+XY)
         END IF
      END IF

      RETURN
      END

      SUBROUTINE SZENH(XARG)
C***********************************************************************
C                 SZENH Module of SCREEN2 Model
C
C        PURPOSE: Calculates Building Enhanced Sigma-z Values
C                 and Compares to Sigma-z From Dispersion Curves
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Downwind Distance
C                 Stability Class
C                 Rural or Urban Dispersion Option
C                 Wake Plume Height, HEMWAK
C
C        OUTPUTS: Vertical Dispersion Coefficient, Sigma-z
C
C        CALLED FROM:   PDIS
C                       DHPSS
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN.INC'

C     Variable Initializations
CISC2      MODNAM = 'SZENH'

C     Calculate Decay Coefficient, DA                       ---   CALL SZDCAY
      CALL SZDCAY

C     Calculate Building Enhanced Sigma-z at 10*ZLB (Eqn. 1-40)
      SZINIT = 1.2 * ZLB * DA
CC**********************************************************************
CC         The Following Commented Line Removes Rounding From Estimate
CC         of SZINIT at 10*ZLB.  Rounding in Original ISC Model Causes
CC         Slight Discontinuity at 10*ZLB.
CC
CC      SZINIT = 1.169 * ZLB * DA
CC**********************************************************************
C     Calculate Vertical Virtual Distance, XZ               ---   CALL XVZ
      CALL XVZ(XARG-10.*ZLB)
      XZ = AMAX1(0.0, (XZ - 10.*ZLB))

      IF (XARG .LT. 10.*ZLB) THEN
C        Calculate Building Enhanced Sigma-z (Eqn. 1-37 & 1-38)
         SZ1 = (0.7*ZLB + 0.067*(XARG - 3.*ZLB)) * DA
C        Calculate Sigma-z from Curves, SZ2                 ---   CALL SIGZ
         CALL  SIGZ(XARG)
         SZ = AMAX1(SZ1,SZ)
      ELSE
C        Calculate Sigma-z from Curves using X+XZ           ---   CALL SIGZ
         CALL SIGZ(XARG+XZ)
      END IF

      RETURN
      END

      SUBROUTINE XVY
C***********************************************************************
C                 XVY Module of SCREEN2 Model
C
C        PURPOSE: Calculates Lateral Virtual Distances
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Initial Dispersion, SYINIT
C                 Stability Class
C                 Rural or Urban Dispersion Option
C
C        OUTPUTS: Lateral Virtual Distance, XY (m)
C
C        CALLED FROM:   VDIS
C                       SYENH
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN.INC'
      REAL A(6),SP(6),SQ(6)

C     Variable Initializations
      DATA A/0.32,0.32,0.22,0.16,0.11,0.11/,
     &     B/0.0004/,
     &     SP/.004781486,.006474168,.009684292,.014649868,.019584802,
     &        0.029481132/,
     &     SQ/1.1235955,1.1086475,1.0905125,1.0881393,1.0857763,
     &        1.0881393/
CISC2      MODNAM = 'XVY'

      IF (RURAL) THEN
         XY = (SYINIT*SP(KST))**SQ(KST) * 1000.
      ELSE IF (URBAN) THEN
         A2 = A(KST) * A(KST)
         SY2 = SYINIT * SYINIT
         XY = (B*SY2 + SQRT(B*B*SY2*SY2 + 4.*A2*SY2)) / (2.*A2)
      END IF

      RETURN
      END

      SUBROUTINE XVZ(XARG)
C***********************************************************************
C                 XVZ Module of SCREEN2 Model
C
C        PURPOSE: Calculates Vertical Virtual Distances
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To Change TOL from 1.0E-5 to 1.0E-4
C
C        INPUTS:  Initial Dispersion, SZINIT
C                 Downwind Distance
C                 Stability Class
C                 Rural or Urban Dispersion Option
C
C        OUTPUTS: Vertical Virtual Distance, XZ (m)
C
C        CALLED FROM:   VDIS
C                       SZENH
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN.INC'
      REAL AA(6), BB(6), XXZ(6)

C     Variable Initializations
      DATA AA/0.24,0.24,0.2,0.14,0.08,0.08/
      DATA BB/.001,.001,.0,.0003,.0015,.0015/
CISC2      MODNAM = 'XVZ'

      IF (SZINIT .LE. 0.01) THEN
         XZ = 0.

      ELSE IF (RURAL) THEN
C        Solve Iteratively
C        Convert Distance to km
         XKM = XARG * 0.001
C        Initial Guess of 10 m
         XXZ(1) = 0.01
         DO 10 N = 1, 5
C           Retrieve Coef. AZ & BZ, Range XMIN & XMAX    ---   CALL SZCOEF
            CALL SZCOEF((XXZ(N)+XKM),AZ,BZ,XMIN,XMAX)
            XXZ(N+1) = (SZINIT/AZ) ** (1./BZ)
C           Check for X+XZ falling within Range of Coefficients
            IF((XXZ(N+1)+XKM).GE.XMIN .AND. (XXZ(N+1)+XKM).LE.XMAX)THEN
               XZ = XXZ(N+1) * 1000.
C              EXIT LOOP
               GO TO 999
            END IF
 10      CONTINUE
C        If No Convergence in Loop, Use Smaller of Last Two Estimates,
C        Consistent With Original ISC Model - Version 2
         XZ = AMIN1(XXZ(5),XXZ(6)) * 1000.

      ELSE IF (URBAN) THEN
         IF (KST .GE. 4) THEN
            A2  = AA(KST) * AA(KST)
            B   = BB(KST)
            SZ2 = SZINIT * SZINIT
            XZ  = (B*SZ2 + SQRT(B*B*SZ2*SZ2 + 4.*A2*SZ2)) / (2.*A2)
         ELSE IF (KST .LE. 2) THEN
C           Set Initial Guess and Tolerance Limit for Cubic Equation
            XZERO = 4. * SZINIT
            TOL = 1.0E-4
C           Set Cubic Coefficients, ACOEF, BCOEF, and CCOEF
            ACOEF = 1./BB(KST)
            BCOEF = 0.0
            CCOEF = -1. * SZINIT*SZINIT/(AA(KST)*AA(KST) * BB(KST))
C           Solve Cubic Equation                          ---   CALL CUBIC
            CALL CUBIC(ACOEF,BCOEF,CCOEF,XZERO,TOL,XZ)
         ELSE IF (KST .EQ. 3) THEN
            XZ = SZINIT/AA(KST)
         END IF
      END IF

 999  RETURN
      END

      SUBROUTINE SZDCAY
C***********************************************************************
C                 SZDCAY Module of SCREEN2 Model
C
C        PURPOSE: Calculates Linear Decay Coefficient for Sigma-z
C                 Used in Schulman-Scire Building Downwash
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Plume Height (Due to Momentum at X2BH, HEMWAK)
C                 Building Dimensions
C                 Wake Flags
C
C        OUTPUTS: Decay Coefficient
C
C        CALLED FROM:   DHPSS
C                       SZENH
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN.INC'

C     Variable Initializations
CISC2      MODNAM = 'SZDCAY'

      IF (WAKESS) THEN
         IF (HEMWAK .LE. DSBH) THEN
            DA = 1.
         ELSE IF (HEMWAK .LE. DSBH+2.*ZLB) THEN
            DA = (DSBH - HEMWAK)/(2.*ZLB) + 1.
         ELSE
            DA = 0.
         END IF
      ELSE
         DA = 1.
      END IF

      RETURN
      END

      SUBROUTINE BID
C***********************************************************************
C                 BID Module of ISC2 Model
C
C        PURPOSE: Applies Bouyancy-Induced Dispersion to
C                 Sigma-y and Sigma-z
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C        MODIFIED BY D. Strimaitis, SRC (add SBID to commons)
C
C        DATE:    February 15, 1993
C
C        INPUTS:  Sigma-y
C                 Sigma-z
C                 Downwind Distance
C                 Buoyancy and Momentum Fluxes
C                 Source Parameter Arrays
C
C        OUTPUTS: Sigma-y and Sigma-z Adjusted for BID
C
C        CALLED FROM:   PDIS
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN.INC'

C     Variable Initializations
CISC2      MODNAM = 'BID'

C     Calculate Gradual Rise, If Needed
      IF ((.NOT. WAKE) .AND. (.NOT. GRDRIS)) THEN
         IF (X .LT. XF) THEN
C           Calculate Gradual Rise, DHP                     ---   CALL DHPHS
            CALL DHPHS(X)
         ELSE
            DHP = DHF
         END IF
      END IF

C     Calculate The Coefficients
      SBID = DHP/3.5
      SBIDSQ = SBID*SBID
C     Apply BID to Sigma-y and Sigma-z
      SY = SQRT(SY*SY + SBIDSQ)
      SZ = SQRT(SZ*SZ + SBIDSQ)

      RETURN
      END

      SUBROUTINE PHEFF
C***********************************************************************
C                 PHEFF Module of the SCREEN2 Model
C
C        PURPOSE: Calculates Effective Plume Height for POINT Sources (m)
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Arrays of Source Parameters
C                 Logical Wake Flags
C                 Meteorological Variables for One Hour
C                 Wind Speed Adjusted to Stack Height
C                 Downwind Distance
C                 Terrain Elevation of Receptor
C
C        OUTPUTS: Effective Plume Height (HE)
C
C        CALLED FROM:   PCALC
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN.INC'

C     Variable Initializations
CISC2      MODNAM = 'PHEFF'

C     Calculate Plume Height Without Terrain Adjustment
      IF ((.NOT. WAKE) .AND. (.NOT. GRDRIS)) THEN
C        Calculate Final Rise for First Receptor Only
         IF (FSTREC) THEN
            FSTREC = .FALSE.
C           Calculate Final Rise, DHF                       ---   CALL DELH
            CALL DELH
            HSP = HSPRIM(US,VS,HS,DS)
         END IF
         IF (NOSTD) THEN
            HE = HS + DHF
         ELSE
            HE = HSP + DHF
         END IF
      ELSE IF ((WAKE .AND. (.NOT. WAKESS)) .OR.
     &         ((.NOT. WAKE) .AND. GRDRIS)) THEN
C        Calculate Final Rise for First Receptor Only
         IF (FSTREC) THEN
            FSTREC = .FALSE.
C           Calculate Final Rise, DHF                       ---   CALL DELH
            CALL DELH
            HSP = HSPRIM(US,VS,HS,DS)
         END IF
         IF (X .LT. XF) THEN
C           Calculate Gradual Rise, DHP                     ---   CALL DHPHS
            CALL DHPHS(X)
         ELSE
            DHP = DHF
         END IF
         IF (NOSTD) THEN
            HE = HS + DHP
         ELSE
            HE = HSP + DHP
         END IF
      ELSE IF (WAKE .AND. WAKESS) THEN
C        Calculate Final Rise for First Receptor Only
         IF (FSTREC) THEN
            FSTREC = .FALSE.
C           Calculate Final Rise (at X=XF), DHF             ---   CALL DHPSS
            CALL DHPSS(XF)
            DHF = DHP
         END IF
         IF (X .LT. XF) THEN
C           Calculate Gradual Rise, DHP                     ---   CALL DHPSS
            CALL DHPSS(X)
         ELSE
            DHP = DHF
         END IF
         HE = HS + DHP
      END IF

C     Adjust Plume Height for Elevated Terrain, Save Flat Terrain Value (HEFLAT)
C     For Later Comparison With Mixing Height
      IF (FLAT) THEN
         HEFLAT = HE
      ELSE IF (ELEV) THEN
         HEFLAT = HE
C        Calculate Terrain Hgt Above Plant Grade (Chopped-off at Release Height)
CISC2         HTER = AMIN1( HS, (ZELEV - ZS))
         HE = HE - HTER
      END IF

C     Don't Allow Effective Plume Height to be < 0.0
      HE = AMAX1( 0.0, HE)

      RETURN
      END

      SUBROUTINE VHEFF
C***********************************************************************
C                 VHEFF Module of the SCREEN2 Model
C
C        PURPOSE: Calculates Effective Plume Height for VOLUME Sources (m)
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Arrays of Source Parameters
C                 Logical Wake Flags
C                 Meteorological Variables for One Hour
C                 Wind Speed Adjusted to Stack Height
C                 Downwind Distance
C                 Terrain Elevation of Receptor
C
C        OUTPUTS: Effective Plume Height (HE)
C
C        CALLED FROM:   VCALC
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN.INC'

C     Variable Initializations
CISC2      MODNAM = 'VHEFF'

C     Calculate Terrain Height Above Plant Grade (Chopped-off at Release Height)
      IF (FLAT) THEN
         HTER = 0.0
CISC2      ELSE IF (ELEV) THEN
CISC2         HTER = AMIN1( HS, (ZELEV - ZS))
      END IF

C     Calculate Effective Plume Height (No Rise) Adjusted for Terrain Height
      HE = HS - HTER

C     Save Plume Height for Flat Terrain for Later Comparison to Mixing Height
      HEFLAT = HS

      RETURN
      END

      SUBROUTINE PDIS
C***********************************************************************
C                 PDIS Module of the ISC Short Term Model - Version 2
C
C        PURPOSE: Calculates Dispersion Parameters for POINT Sources
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C        MODIFIED BY D. Strimaitis, SRC (initialize SBID to 0.0)
C
C        DATE:    February 15, 1993
C
C        INPUTS:  Arrays of Source Parameters
C                 Logical Wake Flags
C                 Wake Plume Height, HEMWAK
C                 Meteorological Variables for One Hour
C                 Downwind Distance
C
C        OUTPUTS: Lateral and Vertical Dispersion Coefficients, SY and SZ
C
C        CALLED FROM:   PCALC
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN.INC'

C     Variable Initializations
CISC2      MODNAM = 'PDIS'

      IF (.NOT. WAKE) THEN
C        Calculate Sigma-y from Curves                   ---   CALL SIGY
         CALL SIGY(X)
C        Calculate Sigma-z from Curves                   ---   CALL SIGZ
         CALL SIGZ(X)
         IF (.NOT. NOBID) THEN
C           Apply BID                                    ---   CALL BID
            CALL BID
         ELSE
            SBID = 0.0
         END IF
         XY = 0.0
         XZ = 0.0
      ELSE IF (WAKE) THEN
         IF (HEMWAK .GT. 1.2*DSBH) THEN
C           Calculate Sigma-y from Curves                ---   CALL SIGY
            CALL SIGY(X)
            XY = 0.0
         ELSE
C           Calculate Building Enhanced Sigma-y          ---   CALL SYENH
            CALL SYENH(X)
         END IF
C        Calculate Building Enhanced Sigma-z             ---   CALL SZENH
         CALL SZENH(X)
         IF ((.NOT. NOBID) .AND. (.NOT. WAKESS)) THEN
C           Apply BID                                    ---   CALL BID
            CALL BID
         ELSE
            SBID = 0.0
         END IF
      END IF

      IF (SZ .GT. 5000. .AND. NPD .EQ. 0)  SZ = 5000.

      RETURN
      END

      SUBROUTINE VDIS
C***********************************************************************
C                 VDIS Module of the SCREEN2 Model
C
C        PURPOSE: Calculates Dispersion Parameters for VOLUME Sources
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Arrays of Source Parameters
C                 Meteorological Variables for One Hour
C                 Downwind Distance
C
C        OUTPUTS: Lateral and Vertical Dispersion Coefficients
C
C        CALLED FROM:   VCALC
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN.INC'

C     Variable Initializations
CISC2      MODNAM = 'VDIS'

C     Calculate Lateral Virtual Distance                 ---   CALL XVY
      CALL XVY
C     Calculate Sigma-y from Curves for X+XY             ---   CALL SIGY
      CALL SIGY(X+XY)
C     Calculate Vertical Virtual Distance                ---   CALL XVZ
      CALL XVZ(X)
C     Calculate Sigma-z from Curves for X+XZ             ---   CALL SIGZ
      CALL SIGZ(X+XZ)

      IF (SZ .GT. 5000. .AND. NPD .EQ. 0)  SZ = 5000.

      RETURN
      END

      SUBROUTINE PCHI
C***********************************************************************
C                 PCHI Module of SCREEN2 Model
C
C        PURPOSE: Calculates Hourly Concentration for POINT Sources
C                 Using Gaussian Plume Equation
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Downwind Distance
C                 Crosswind Distance
C                 Plume Height
C                 Stack Top Wind Speed
C                 Lateral Dispersion Parameter
C                 Vertical Dispersion Parameter
C                 Stability Class
C                 Mixing Height
C                 Receptor Height Above Ground
C                 Emission Rate and Units Scaling Factor
C                 Source Parameter Arrays
C
C        OUTPUTS: HRVAL, Concentration for Particular
C                 Source/Receptor Combination
C
C        CALLED FROM:   PCALC
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN.INC'

C     Variable Initializations
CISC2      MODNAM = 'PCHI'

      IF ((UNSTAB .OR. NEUTRL) .AND. HEFLAT.GT.ZI) THEN
C        Plume Is Above Mixing Height, ZI
         HRVAL  = 0.0
      ELSE
         YTERM = -0.5*(Y*Y)/(SY*SY)
         IF (YTERM .GT. EXPLIM) THEN
C           Calculate the Vertical Term, V                  ---   CALL VERT
            CALL VERT
CISC2C           Calculate the Decay Term, D                     ---   CALL DECAY
CISC2            CALL DECAY
            D = 1.0
            VTERM = (D*V)/(TWOPI*US*SY*SZ)
C           Check for Possible Underflow Condition
            IF (VTERM.GT.0.0 .AND. (LOG(VTERM)+YTERM).GT.EXPLIM) THEN
               HRVAL = QTK * VTERM * EXP(YTERM)
            ELSE
               HRVAL = 0.0
            END IF
         ELSE
            HRVAL = 0.0
         END IF
      END IF

      RETURN
      END

      SUBROUTINE PDEP
C***********************************************************************
C                 PDEP Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Calculates Hourly Deposition for POINT Sources
C                 Using Gaussian Plume Equation
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C        MODIFIED BY D. Strimaitis, SRC (for DEPOSITION)
C
C        DATE:    February 15, 1993
C
C        INPUTS:  Downwind Distance
C                 Crosswind Distance
C                 Plume Height
C                 Stack Top Wind Speed
C                 Lateral Dispersion Parameter
C                 Vertical Dispersion Parameter
C                 Stability Class
C                 Mixing Height
C                 Receptor Height Above Ground
C                 Emission Rate and Units Scaling Factor
C                 Source Parameter Arrays
C
C        OUTPUTS: Deposition for Particular Source/Receptor Combination
C
C        CALLED FROM:   PCALC
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN.INC'

C     Variable Initializations
CISC2      MODNAM = 'PDEP'

      IF ((UNSTAB .OR. NEUTRL) .AND. HEFLAT.GT.ZI) THEN
C        Plume Is Above Mixing Height, ZI
         HRVAL = 0.0
      ELSE
         YTERM = -0.5*(Y*Y)/(SY*SY)
         IF (YTERM .GT. EXPLIM) THEN
C           Calculate the Deposition Vertical Term, V       ---   CALL VERTSR
C           Uniform approach to depostion and removal allows identical
C           treatment for either concentration or deposition.
C           Note that V contains corrections to the vertical profile of
C           concentrations and the depletion factor for the emission rate.
            CALL VERTSR
CISC2C           Calculate the Decay Term, D                     ---   CALL DECAY
CISC2            CALL DECAY
            D = 1.0
            VTERM = (D*V)/(TWOPI*SY*SZ*US)
C           Check for Possible Underflow Condition
            IF (VTERM.GT.0.0 .AND. (LOG(VTERM)+YTERM).GT.EXPLIM) THEN
               HRVAL = QTK * VTERM * EXP(YTERM)
            ELSE
               HRVAL = 0.0
            END IF
         ELSE
            HRVAL = 0.0
         END IF
      END IF

      RETURN
      END

      SUBROUTINE ACHI(X1,RCZ)
C***********************************************************************
C                 ACHI Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Calculates Hourly Concentration for AREA Sources
C                 Using Numerical Integration Algorithm
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To incorporate numerical integration
C                    algorithm for AREA source - 7/7/93
C
C        INPUTS:  Downwind Distance
C                 Crosswind Distance
C                 Plume Height
C                 Stack Top Wind Speed
C                 Lateral Dispersion Parameter
C                 Vertical Dispersion Parameter
C                 Stability Class
C                 Mixing Height
C                 Receptor Height Above Ground
C                 Emission Rate and Units Scaling Factor
C                 Source Parameter Arrays
C
C        OUTPUTS: Concentration for A Unit Of Source/Receptor Combination
C
C        CALLED FROM:   PLUMEF
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN.INC'

C     Variable Initializations
CISC2      MODNAM = 'ACHI'

      RCZ = 0.0
      IF (X1 .GE. 0.001) THEN
C        Calculate the Vertical Term, V                     ---   CALL VERT
         CALL VERT
CISC2C        Calculate the Decay Term, D                        ---   CALL DECAY
CISC2         CALL DECAY
         D = 1.0
         RCZ = (D*V)/(SRT2PI*SZ)
      END IF

      RETURN
      END

      SUBROUTINE ADEP(X1,RCZ)
C***********************************************************************
C                 ADEP Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Calculates Hourly Deposition for AREA Sources
C                 Using Numerical Integration Algorithm
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        MODIFIED by Yicheng Zhuang, SRC to combine version 93188 with
C                 version 93046 - 9/28/93
C
C        DATE:    September 28, 1993
C
C        MODIFIED:   To incorporate numerical integration
C                    algorithm for AREA source - 7/7/93
C
C        MODIFIED BY D. Strimaitis, SRC (for DEPOSITION) - 2/15/93
C
C        INPUTS:  Downwind Distance
C                 Crosswind Distance
C                 Plume Height
C                 Stack Top Wind Speed
C                 Lateral Dispersion Parameter
C                 Vertical Dispersion Parameter
C                 Stability Class
C                 Mixing Height
C                 Receptor Height Above Ground
C                 Emission Rate and Units Scaling Factor
C                 Source Parameter Arrays
C
C        OUTPUTS: Deposition for A Unit Of Source /Receptor Combination
C
C        CALLED FROM:   PLUMEF
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN.INC'

C     Variable Initializations
CISC2      MODNAM = 'ADEP'

      RCZ = 0.0
      IF (X1 .GE. 0.001) THEN
C        Calculate the Deposition Vertical Term, V          ---   CALL VERTSR
C        Uniform approach to depostion and removal allows identical
C        treatment for either concentration or deposition.
C        Note that V contains corrections to the vertical profile of
C        concentrations and the depletion factor for the emission rate.
         CALL VERTSR
CISC2C        Calculate the Decay Term, D                        ---   CALL DECAY
CISC2         CALL DECAY
         D = 1.0
         RCZ = (D*V)/(SRT2PI*SZ)
      END IF

      RETURN
      END

      SUBROUTINE VERT
C***********************************************************************
C                 VERT Module of SCREEN2 Model
C
C        PURPOSE: Calculates Vertical Term for Use in Gaussian Plume Equation
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Plume Height
C                 Vertical Dispersion Parameter
C                 Stability Class
C                 Mixing Height
C                 Receptor Height Above Ground
C
C        OUTPUTS: Vertical Term, V
C
C        CALLED FROM:   PCHI, VCHI, ACHI
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN.INC'

C     Variable Initializations
CISC2      MODNAM = 'VERT'
      V = 0.0

      IF (NPD .GT. 0) THEN
C        Determine Vertical Term With Settling & Removal    --- CALL VERTSR
         CALL VERTSR
      ELSE IF (ZFLAG .EQ. 0.0) THEN
C        Vertical Term for Case With No Flagpole Receptor
         IF (STABLE .OR. ZI.GE.10000.) THEN
            A1 = (-0.5/(SZ*SZ)) * HE * HE
            IF (A1 .GT. EXPLIM)  V = 2.*EXP(A1)
         ELSE IF ((SZ/ZI) .GE. 1.6) THEN
            V  = SRT2PI*(SZ/ZI)
         ELSE
            A1 = (-0.5/(SZ*SZ)) * HE * HE
            IF (A1 .GT. EXPLIM)  V = EXP(A1)
            SUM = 0.0
            DO 100 I = 1, 100
               T  = 0.0
               TWOIZI = 2.*I*ZI
               A2 = (-0.5/(SZ*SZ)) * (TWOIZI-HE) * (TWOIZI-HE)
               A3 = (-0.5/(SZ*SZ)) * (TWOIZI+HE) * (TWOIZI+HE)
               IF (A2 .GT. EXPLIM)  T = EXP(A2)
               IF (A3 .GT. EXPLIM)  T = T + EXP(A3)
               SUM = SUM + T
               IF (ABS(T) .LE. 5.0E-9) THEN
C                 Exit Loop
                  GO TO 200
               END IF
 100        CONTINUE
C           Calculate Total Vert. Term - (2.*) was Removed for Optimization
 200        V  = 2.*(V + SUM)
         END IF
      ELSE
C        Vertical Term for Case of ZFLAG .NE. 0.0
         IF (STABLE .OR. ZI .GE. 10000.) THEN
            A1 = (-0.5/(SZ*SZ)) * (ZFLAG-HE) * (ZFLAG-HE)
            A2 = (-0.5/(SZ*SZ)) * (ZFLAG+HE) * (ZFLAG+HE)
            IF (A1 .GT. EXPLIM)  V = V + EXP(A1)
            IF (A2 .GT. EXPLIM)  V = V + EXP(A2)
         ELSE IF (SZ/ZI .GE. 1.6) THEN
            V  = SRT2PI*(SZ/ZI)
         ELSE
            A1 = (-0.5/(SZ*SZ)) * (ZFLAG-HE) * (ZFLAG-HE)
            A2 = (-0.5/(SZ*SZ)) * (ZFLAG+HE) * (ZFLAG+HE)
            IF (A1 .GT. EXPLIM)  V = V + EXP(A1)
            IF (A2 .GT. EXPLIM)  V = V + EXP(A2)
            SUM = 0.0
            DO 300 I = 1, 100
               T  = 0.0
               TWOIZI = 2.*I*ZI
               A3 = (-0.5/(SZ*SZ)) * (ZFLAG-(TWOIZI-HE)) *
     &                               (ZFLAG-(TWOIZI-HE))
               A4 = (-0.5/(SZ*SZ)) * (ZFLAG+(TWOIZI-HE)) *
     &                               (ZFLAG+(TWOIZI-HE))
               A5 = (-0.5/(SZ*SZ)) * (ZFLAG-(TWOIZI+HE)) *
     &                               (ZFLAG-(TWOIZI+HE))
               A6 = (-0.5/(SZ*SZ)) * (ZFLAG+(TWOIZI+HE)) *
     &                               (ZFLAG+(TWOIZI+HE))
               IF (A3 .GT. EXPLIM)  T = T + EXP(A3)
               IF (A4 .GT. EXPLIM)  T = T + EXP(A4)
               IF (A5 .GT. EXPLIM)  T = T + EXP(A5)
               IF (A6 .GT. EXPLIM)  T = T + EXP(A6)
               SUM = SUM + T
               IF (ABS(T) .LE. 1.0E-8) THEN
C                 Exit Loop
                  GO TO 400
               END IF
 300        CONTINUE
 400        V  = V + SUM
         END IF
      END IF

      RETURN
      END

      SUBROUTINE VERTSR
C***********************************************************************
C                 VERTSR Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Calculates Vertical Term with Gravitational Settling and
C                 Surface Removal for Use in Gaussian Plume Equation
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C        MODIFIED BY D. Strimaitis, SRC (for DEPOSITION)
C
C        DATE:    February 15, 1993
C
C        INPUTS:  Plume Height
C                 Vertical Dispersion Parameter
C                 Stability Class
C                 Mixing Height
C                 Receptor Height Above Ground
C                 Source Parameter Arrays (Settling and Removal Variables)
C
C        OUTPUTS: Vertical Term, V
C
C        CALLED FROM:   VERT, PDEP, ADEP
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN.INC'

C     Variable Initializations
CISC2      MODNAM = 'VERTSR'
      V = 0.0
      A0 = -0.5/(SZ*SZ)

      DO 1000 J = 1, NPD
         VJ = 0.0
C        Calculate Plume Tilt Due to Settling, HV
         HV = (X/US) * VGRAV(J)
C        Calculate Settled Plume Height, HESETL
         HESETL = HE - HV
C        Restrict settled height to be positive, so that the plume
C        does not settle below the surface -- this is the limit of
C        the tilted plume technique.
         HESETL = AMAX1(0.0,HESETL)

         IF (ZFLAG .EQ. 0.0) THEN
C           Vertical Term for Case With No Flagpole Receptor
            IF (STABLE .OR. ZI .GE. 10000.) THEN
               A1 = A0 * HESETL * HESETL
               IF (A1 .GT. EXPLIM)  VJ = VJ + 2.*EXP(A1)
            ELSE IF (SZ/ZI .GE. 1.6) THEN
               VJ = VJ + SRT2PI*(SZ/ZI)
            ELSE
               A1 = A0 * HESETL * HESETL
               IF (A1 .GT. EXPLIM)  VJ = VJ + 2.*EXP(A1)
               SUM = 0.0
               DO 100 I = 1, 500
                  T  = 0.0
                  TWOIZI = 2.*I*ZI
                  A2 = A0* (TWOIZI-HESETL)*(TWOIZI-HESETL)
                  A3 = A0* (TWOIZI+HESETL)*(TWOIZI+HESETL)
                  IF (A2 .GT. EXPLIM)
     &                   T = T + 2. * EXP(A2)
                  IF (A3 .GT. EXPLIM)
     &                   T = T + 2. * EXP(A3)
                  SUM = SUM + T
                  IF (ABS(T) .LE. 1.0E-8) THEN
C                    Exit Loop
                     GO TO 200
                  END IF
 100           CONTINUE
 200           VJ = VJ + SUM
            END IF
         ELSE
C           Vertical Term for Case of ZFLAG .NE. 0.0
            IF (STABLE .OR. ZI .GE. 10000.) THEN
               A1 = A0 * (ZFLAG-HESETL) * (ZFLAG-HESETL)
               A2 = A0 * (ZFLAG+HESETL) * (ZFLAG+HESETL)
               IF (A1 .GT. EXPLIM)  VJ = VJ + EXP(A1)
               IF (A2 .GT. EXPLIM)  VJ = VJ + EXP(A2)
            ELSE IF (SZ/ZI .GE. 1.6) THEN
               VJ = VJ + SRT2PI*(SZ/ZI)
            ELSE
               A1 = A0 * (ZFLAG-HESETL) * (ZFLAG-HESETL)
               A2 = A0 * (ZFLAG+HESETL) * (ZFLAG+HESETL)
               IF (A1 .GT. EXPLIM)  VJ = VJ + EXP(A1)
               IF (A2 .GT. EXPLIM)  VJ = VJ + EXP(A2)
               SUM = 0.0
               DO 300 I = 1, 500
                  T = 0.0
                  TWOIZI = 2.*I*ZI
                  A3 = A0 * (ZFLAG-(TWOIZI-HESETL)) *
     &                      (ZFLAG-(TWOIZI-HESETL))
                  A4 = A0 * (ZFLAG+(TWOIZI-HESETL)) *
     &                      (ZFLAG+(TWOIZI-HESETL))
                  A5 = A0 * (ZFLAG-(TWOIZI+HESETL)) *
     &                      (ZFLAG-(TWOIZI+HESETL))
                  A6 = A0 * (ZFLAG+(TWOIZI+HESETL)) *
     &                      (ZFLAG+(TWOIZI+HESETL))
                  IF (A3 .GT. EXPLIM)  T = T + EXP(A3)
                  IF (A4 .GT. EXPLIM)  T = T + EXP(A4)
                  IF (A5 .GT. EXPLIM)  T = T + EXP(A5)
                  IF (A6 .GT. EXPLIM)  T = T + EXP(A6)
                  SUM = SUM + T
                  IF (ABS(T) .LE. 1.0E-8) THEN
C                    Exit Loop
                     GO TO 400
                  END IF
 300           CONTINUE
 400           VJ  = VJ + SUM
            END IF
         END IF
C        Adjust Jth contribution by mass fraction, source depletion, and
C        profile correction factors, and add to total.
C        Also, multiply by deposition velocity if deposition is needed.
         IF (DEPOS) THEN
            V = V + VDEP(J) * PHI(J) * QCOR(J) * PCORZD(J) * VJ
         ELSE
            V = V + PHI(J) * QCOR(J) * PCORZR(J) * VJ
         ENDIF
 1000 CONTINUE

      RETURN
      END

      SUBROUTINE ERFAB
C***********************************************************************
C                 ERFAB Module of SCREEN2 Model
C
C        PURPOSE: Calculates Error Function Term for Area Sources
C                 for Use in Gaussian Plume Equation
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Crosswind Distance, Y
C                 Lateral Dispersion Parameter, SY
C                 Effective Radius of Area Source, XRAD
C
C        OUTPUTS: Error Function Term, E = ERF(A) + ERF(B)
C
C        CALLED FROM:   ACHI
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN.INC'
      DOUBLE PRECISION AS, BS, ERFX

C     Variable Initializations
CISC2      MODNAM = 'ERFAB'

C     Calculate A and B Arguments of Error Function Term (1.414214 = SQRT(2.))
      AS = (XRAD + Y)/(1.414214*SY)
      BS = (XRAD - Y)/(1.414214*SY)

C     Calculate E Term for Area Source:  E = ERF(AS) + ERF(BS)
C     Using Call to FUNCTION ERFX
      E = ERFX(AS) + ERFX(BS)

      RETURN
      END

      FUNCTION ERFX(ARG)
C***********************************************************************
C                 ERFX Module of SCREEN2 Model
C
C        PURPOSE: Calculates Error Function, Using Method Documented
C                 on Page 187 of "Approximations for Digital Computers"
C                 by Cecil Hastings, Princeton University Press, 1955
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Error Function Argument, ARG
C
C        OUTPUTS: Error Function Value, ERFX
C
C        CALLED FROM:   ERFAB
C***********************************************************************

C     Variable Declarations
CISC2      CHARACTER MODNAM*6
      DOUBLE PRECISION ARG, X, ERFX
C     Variable Initializations
CISC2      MODNAM = 'ERFX'

      IF (ARG .GT. 4.0) THEN
         ERFX = 1.0
      ELSE IF (ARG .LT. -4.0) THEN
         ERFX = -1.0
      ELSE IF (ABS(ARG) .LT. 1.0E-10) THEN
         ERFX = 0.0
      ELSE
         X = ABS(ARG)
         ERFX = 1. - 1./(1.+X*(0.705230784E-1+X*(0.422820123E-1+X*
     &           (0.92705272E-2+X*(0.1520143E-3+X*(0.2765672E-3+X*
     &            0.430638E-4))))))**16.
         IF (ARG .LT. 0.0)  ERFX = -ERFX
      END IF

      RETURN
      END

C***  Begin new code for area source numerical integration algorithm - 7/7/93

      SUBROUTINE AVERTS
C***********************************************************************
C                 AVERTS Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Calculates coordinates of vertices for Wind
C                 Direction Coordinate system for AREA sources
C                 for the integrated line source algorithm.
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    July 7, 1993
C
C        INPUTS:  Source Parameters for Specific Source
C
C        OUTPUTS: Array of Vertex Coordinates for Specific Source
C
C        CALLED FROM:   ACALC
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN.INC'

C     Variable Initializations
CISC2      MODNAM = 'AVERTS'

      DO 1670 NSP = 1, NVERT+1
C***********************************************************************
C        SPA(i,1/2) = x/y co-ords of vertex in WIND direction
C        co-ord system; x' is up/down wind; y' is crosswind
C***********************************************************************
CISC2         SPA(NSP,1) = -(AXVERT(ISRC,NSP)*WDSIN + AYVERT(ISRC,NSP)*WDCOS)
CISC2         SPA(NSP,2) =   AXVERT(ISRC,NSP)*WDCOS - AYVERT(ISRC,NSP)*WDSIN
         SPA(NSP,1) = -(AXVERT(NSP)*WDSIN + AYVERT(NSP)*WDCOS)
         SPA(NSP,2) =   AXVERT(NSP)*WDCOS - AYVERT(NSP)*WDSIN
1670  CONTINUE

      RETURN
      END

      SUBROUTINE AREAIN
C***********************************************************************
C                 AREAIN Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Calculates Hourly Concentration for AREA Sources
C                 Using Numerical Integration Algorithm
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C                    Adapted From Codes By Richard Strelitz, CSC
C
C        DATE:    July 7, 1993
C
C        INPUTS:  Source Parameters for Specific Source
C                 Arrays of Receptor Locations
C                 Meteorological Variables for One Hour
C
C        OUTPUTS: Concentration for Particular Source/Receptor Combination
C
C        CALLED FROM:   ACALC
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN.INC'
      LOGICAL QGO
      REAL VAL, DVAL

C     Variable Initializations
CISC2      MODNAM = 'AREAIN'

C     INITIALIZE VARIABLES FOR INTEGRATION PROCEDURE.
CISC2      RP1 = -(XR*WDSIN+YR*WDCOS)/1000.0
CISC2      RP2 =  (XR*WDCOS-YR*WDSIN)/1000.0
      RP1 = -(X*WDSIN+Y*WDCOS)/1000.0
      RP2 =  (X*WDCOS-Y*WDSIN)/1000.0
      UCRIT = 0.00101
      VAL = 0.0
      KSIDE = 0
      do 1658 ncp = 1, NVERT
         ua = RP1-SPA(ncp,1)
         ub = RP1-SPA(ncp+1,1)
         va = RP2-SPA(ncp,2)
         vb = RP2-SPA(ncp+1,2)
         IF (ua .ge. ucrit) THEN
            kside = kside + 1
            uvert(kside) = ua
            vvert(kside) = va
         END IF
         IF ((ua .ge. ucrit .AND. ub .lt. ucrit) .OR.
     1       (ua .lt. ucrit .AND. ub .ge. ucrit)) THEN
            kside = kside+1
            uvert(kside) = ucrit
            vvert(kside) = va+(ucrit-ua)*(vb-va)/(ub-ua)
         END IF
1658  CONTINUE

      QGO = .FALSE.
      IF (kside .ge. 2) THEN
         QGO = .TRUE.
         vnmin=  4.0
         vnmax= -4.0
         do 1659 ncp = 1,kside
            ua = uvert(ncp)
            va = vvert(ncp)
            call pwidth(ua,va,vnorm,wa)
            vNVERT(ncp) = vnorm
            wvert(ncp) = wa
            vnmax = amax1(vnorm,vnmax)
            vnmin = amin1(vnorm,vnmin)
1659     CONTINUE
         IF (vnmin .ge. 4.0 .or. vnmax .le. -4.0) QGO = .FALSE.
      END IF

C     Integrate Between Vertices u(1),u(2) THEN u(2),u(3); etc.
      IF (QGO) THEN
C        MAKE 1st Point Same as Last
         ksp = kside+1
         uvert(ksp) = uvert(1)
         vvert(ksp) = vvert(1)
         vNVERT(ksp) = vNVERT(1)
         wvert(ksp) = wvert(1)
         nsegs = 0
         LSEG = .FALSE.
         do 1660 ks = 1,kside
            QGO = .TRUE.
            ivert = ks
            ua = uvert(ks)
            ub = uvert(ks+1)
            dval = 0.0
            IF (abs(ua-ub) .le. 0.0001) QGO = .FALSE.
            IF (QGO) call pside(ua,ub,dval)
            val = val+dval
1660     CONTINUE
         IF (nsegs .gt. 0) THEN
            LSEG = .TRUE.
            call pside2(dval)
            val = val+dval
         END IF
      END IF

      HRVAL = ABS(VAL)*QTK/US

CISC2      IF (DEBUG) THEN
CISC2C        Print Out Debugging Information                    ---   CALL DEBOUT
CISC2         CALL DEBOUT
CISC2      END IF

      RETURN
      END

      SUBROUTINE QROMB(A1,B1,SS1)
C***********************************************************************
C                 QROMB Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Performs Romberg Integration of Function Using
C                 Polynomial Extrapolation for h=0 With h1(i)=h1(i-1)/4
C                 Modifed To Use Variable Order Extrapolation
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C                    Adapted From Codes By Richard Strelitz, CSC
C
C        DATE:    July 7, 1993
C
C        MODIFIED by Roger Brode, PES, Inc. to change lower limit on
C                    J from 3 to 4, and correct lower threshold check
C                    for SS1. - 7/29/94
C
C        INPUTS:  Left Maximum Value of the Integral
C                 Right Maximum Limit of the Integral
C
C        OUTPUTS: Concentration for Particular Source/Receptor Combination
C
C        CALLED FROM:   PSIDE, PSIDE2
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN.INC'
      REAL S1(21), H1(21)
C     Variable Initializations
CISC2      MODNAM = 'QROMB'

      H1(1) = 1
      CALL TRAPZD(A1,B1,S1(1),1)
      SS1 = S1(1)

      DO 11 J = 2, JMAX1
         H1(J) = 0.25*H1(J-1)
         CALL TRAPZD(A1,B1,S1(J),J)
         KP = MIN(J,K1)-1
         CALL POLINT(H1(J-KP),S1(J-KP),KP+1,SS1,DSS1)
C***********************************************************************
C        Check The Convergence Criteria:
C        EPS is tolerance level for convergence of the integral,
C          initially set = 1.0E-4 in a PARAMETER statement in MAIN1.INC;
C        EPS2 is lower threshold for the integral, initially set = 1.0E-10
C          in a PARAMETER statement in MAIN1.INC;
C        J is number of halving intervals and must be at least 4 for
C          convergence criteria to be met (i.e., minimum of 9 data points).
C          Maximum number of intervals is set by JMAX1 (=10).
C***********************************************************************
         IF ((ABS(DSS1) .LE. EPS*ABS(SS1) .OR. ABS(SS1) .LE. EPS2)
     &          .AND. J .GE. 4) GO TO 999
  11  CONTINUE

 999  RETURN
      END

      SUBROUTINE POLINT(XA,YA,N1,Y1,DY1)
C***********************************************************************
C                 POLINT Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Computes Y(X) as Interpolation of XA, YA
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C                    Adapted From Codes By Richard Strelitz, CSC
C
C        DATE:    July 7, 1993
C
C        INPUTS:  The Edge Pairs
C                 The Dimension of The Edge Pairs
C
C        OUTPUTS: Interpolation of XA and YA
C
C        CALLED FROM:   QROMB
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN.INC'
      DIMENSION XA(N1),YA(N1),C1(JMAX1),D1(JMAX1)

C     Variable Initializations
CISC2      MODNAM = 'POLINT'

      ns = n1
      y1 = ya(ns)
      dIFt = abs(xa(n1))
C     Set Up Interpolation/Divided Differences
      do 11 i = 1,n1
         c1(i) = ya(i)
         d1(i) = ya(i)
  11  CONTINUE

C     Compute Table Entries
      ns = ns-1
      do 13 m1 = 1,n1-1
         do 12 i = 1,n1-m1
            ho = xa(i)
            hp = xa(i+m1)
            w = c1(i+1)-d1(i)
            den = w/(ho-hp)
            d1(i) = hp*den
            c1(i) = ho*den
  12     CONTINUE
         IF (2*ns .lt. n1-m1) THEN
            dy1 = c1(ns+1)
         else
            dy1 = d1(ns)
            ns = ns-1
         END IF
         y1 = y1+dy1
  13  CONTINUE

      RETURN
      END

      SUBROUTINE TRAPZD(XTMIN,XTMAX,VAL,N)
C***********************************************************************
C                 TRAPZD Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Standard Trapezoidal Integration Routine for 2 Dimensional
C                 integrals. It Integrates the function plumef(x)*
C                 (erf(y2(x))-erf(y1(x)), where Y2 And Y1 Are Determined from
C                 Geometric Terms Computed in ACALC And Found In PLUMEF
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C                    Adapted From Codes By Richard Strelitz, CSC
C
C        DATE:    July 7, 1993
C
C        INPUTS:  Lower Limit For The Integration
C                 Upper Limit For The Integration
C
C        OUTPUTS: The Result Produced By The Integration
C
C        CALLED FROM:   QROMB
C***********************************************************************

C     Variable Declarations
      real del, sum, sval
      INCLUDE 'MAIN.INC'

C     Variable Initializations
CISC2      MODNAM = 'TRAPZD'

      IF (n .EQ. 1) THEN
         call plumef(xtmax,sum1)
         call plumef(xtmin,sum2)
         sum = sum1+sum2
         del = xtmax-xtmin
         sval = 0.0
         neval2 = 1
      else
         del = (xtmax-xtmin)/neval2
         x1 = xtmin+del*0.5
         sum = 0.0
         do 2 i = 1,neval2
            call plumef(x1,sumc)
            sum = sum+sumc
            x1 = x1+del
2        CONTINUE
         neval2 = neval2*2
      END IF

      val = 0.5*(sval+del*sum)
      sval = val

      RETURN
      END
 	
      SUBROUTINE PLUMEF(X1,POUT)
C***********************************************************************
C                 PLUMEF Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Driving Program for Plume Calculations
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C                    Adapted From Codes By Richard Strelitz, CSC
C
C        DATE:    July 7, 1993
C
C        MODIFIED BY D. Strimaitis and Yicheng Zhuang, SRC (for DEPOSITION)
C
C        MODIFIED BY R. Brode, PES, Inc. to move calculation of dispersion
C                    coefficients to a new ADIS subroutine - 7/21/94
C
C        DATE:    September 28, 1993
C
C        INPUTS:  Downwind Distance (in km !)
C                 Source Parameter Arrays
C
C        OUTPUTS: Concentration for Particular Source/Receptor Combination
C                 For A Certain Downwind Distance
C
C        CALLED FROM:   TRAPZD
C***********************************************************************
C     Variable Declarations
      INCLUDE 'MAIN.INC'

C     Variable Initializations
CISC2      MODNAM = 'PLUMEF'
      IOUNIT = IDBG

C     Scale x1 from kilometers to meters, and place in variable XARG
      XARG = X1*1000.0

C     Note:  XZ and SBID area initialized to 0.0 in ACALC for call to DEPCOR

C     MODIFIED to NOT compute v, val for cases with val=1.0 uses LSEG,
C     a logical variable set in PSIDE2, AREAIN to establish case
      IF (LSEG) THEN
         SZ = 1.0
         SY = 1.0
         IF (RURAL .AND. X1 .LE. 0.0005) THEN
            CALL SZCOEF(X1, AFAC, BFAC, X1MAX, X1MIN)
            SZ = AFAC
         ELSE
C           Calculate Sigma-y from curves
            CALL SIGY(XARG)
C           Calculate Sigma-z from curves
            CALL SIGZ(XARG)
         END IF
         VAL = 1.0
      ELSE
         CALL XWIDTH(X1,VT)
         CALL PWIDTH(X1,VT,VN,VAL)
      END IF

C     Determine deposition correction factors   ---   CALL DEPCOR
      IF (LDEP) THEN
C     Loop over particle sizes
        DO 150 I=1,NPD
        IF (DPLETE) THEN
        CALL DEPCOR( VDEP(I),VGRAV(I),ZRDEP,ZFLAG,XARG,
     &    XZ,HE,ZI,US,RURAL,URBAN,KST,SZ,SBID,
     &    DEBUG,IOUNIT,QCOR(I),PCORZR(I),
     &    PCORZD(I))
        ELSE
          QCOR(I) = 1.
          PCORZR(I) = 1.
          PCORZD(I) = 1.
        ENDIF
150     CONTINUE
      ENDIF

C     Get Concentration or Deposition
      IF (CONC) THEN
         CALL ACHI(X1,RCZ)
      ELSE IF (DEPOS) THEN
         CALL ADEP(X1,RCZ)
      END IF

C     Now compute the function
      POUT = VAL*RCZ*1000.0

      RETURN
      END

      SUBROUTINE PSIDE(U1,U2,DVAL)
C***********************************************************************
C                 PSIDE Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: INTEGRATES SIDE K of POLYGON
C                 int f(u)*CNF(v(u)/sig(u))=f(u)*vn(u) from u1 to u2
C                           CNF = cumulative normal distribution
C                 Computes W(1),W(2)--normalized plume width at   u1    u2
C                 Checks for w(i) outside of -4.0,4.0 with i+, i-
C                 L=-4.0  U=4.0  = bounds for testing
C                 Integrates according to case encountered:
C                 situation     CASE    iplus    iminus  integral limits
C                 L<w1,w2<U      1        0        0         u1,u2
C                 w1,w2<L        2        0       1+2      don't compute
C                 w1,w2>U        3       1+2       0         u1,u2
C                 w1<L<w2<U      4        0        1         u-,u2
C                 w2<L<w1<U      5        0        2         u1,u-
C                 L<w1<U<w2      6        2        0       u1,u+  u+,u2
C                 L<w2<U<w1      7        1        0       u1,u+  u+,u2
C                 w1<L<U<w2      8        2        1       u-,u+  u+,u2
C                 w2<L<U<w1      9        1        2       u1,u+  u+,u-
C
C                 u+ = value of u such that w(u)=U=4.0
C                 u- =     "                w(u)=L=-4.0
C                 u+,u- computed with Brent's Algorithm
C
C                 IF uplus >0, part of side is outside plume
C                 but is integrated anyway, unless there is
C                 a corresponding part on another side that will
C                 cause cancellation.  This is determined in
C                 PSIDE2;
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C                    Adapted From Codes By Richard Strelitz, CSC
C
C        DATE:    July 7, 1993
C
C        MODIFIED by Roger Brode, PES, Inc. to correct lower integration
C                    limit for Case 4, and to remove extraneous calls
C                    to XWIDTH and PWIDTH after calls to ZBRENT. - 7/29/94
C
C        INPUTS:  End Points of The Segments
C
C        OUTPUTS: Integral Value (if any) for Segment
C
C        CALLED FROM:   AREAIN
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN.INC'
      real u(2), v1(2), vn(2), w(2)

C     Variable Initializations
CISC2      MODNAM = 'PSIDE'

C     NSEG = number of segments; set to 0 in AREAIN
C     for each source/rcvr/time step
      dval = 0.0
      do 2 i =  1,2
         ks = ivert + i-1
         u(i) = uvert(ks)
         v1(i) = vvert(ks)
         vn(i) = vNVERT(ks)
         w(i) = wvert(ks)
2     CONTINUE

      iminus = 0
      iplus = 0
      uminus = -1.
      uplus =  -1.0
      do 3 i = 1,2
         IF (vn(i) .lt. -4.0) iminus = i + iminus
         IF (vn(i) .gt.  4.0) iplus  = i + iplus
3     CONTINUE

      ua = u(1)
      ub = u(2)
      IF (iplus.EQ.1 .or. iplus.EQ.2) THEN
         call zbrent(1,u(1),u(2),0.0001,uplus)
crwb         call xwidth(uplus,vtemp)
crwb         call pwidth(uplus,vtemp,vnt,wtemp)
      END IF
      IF (iminus.EQ.1 .or. iminus.EQ.2) THEN
         call zbrent(-1,u(1),u(2),0.0001,uminus)
crwb         call xwidth(uminus,vtemp)
crwb         call pwidth(uminus,vtemp,vnt,wtemp)
      END IF

c     CASE DEPENDs on iplus, iminus
      IF (iplus .EQ. 0) THEN
         IF (iminus .EQ. 0) THEN
c                                             iplus  iminus  case
c                                               0     0       1
            call qromb(u1,u2,dval)
         else IF(iminus .EQ. 3) THEN
c                                               0     3       2
            dval = 0
         else IF(iminus .EQ. 1) THEN
c                                               0     1       4
            call qromb(uminus,u2,dval)
         else
c                                               0     2       5
            call qromb(u1,uminus,dval)
c              changed from u1,uminus
         END IF
      else IF(iplus .EQ. 1) THEN
         nsegs = nsegs+1
         uasegs(nsegs) = u1
         ubsegs(nsegs) = uplus
         IF (iminus .EQ. 0) THEN
c                                               1     0       7
            call qromb(uplus,u2,dval)
         else
c                                               1     2       9
            call qromb(uplus,uminus,dval)
         END IF
      else IF(iplus .EQ. 2) THEN
         nsegs = nsegs+1
         uasegs(nsegs) = uplus
         ubsegs(nsegs) = u2
         IF (iminus .EQ. 0) THEN
c                                               2     0       6
            call qromb(u1,uplus,dval)
         else
c                                               2     1       8
            call qromb(uminus,uplus,dval)
         END IF
      else
c                                               3     0       3
         nsegs = nsegs+1
         uasegs(nsegs) = u1
         ubsegs(nsegs) = u2
      END IF

      RETURN
      END

      SUBROUTINE XWIDTH(U,XOUT)
C***********************************************************************
C                 XWIDTH Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Given Any Y Coordinate of A Vertex of an Area
C                 Source, Calculate the X Coordinate
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C                    Adapted From Codes By Richard Strelitz, CSC
C
C        DATE:    July 7, 1993
C
C        INPUTS:  The Y Coordinate
C
C        OUTPUTS: The X Coordinate Value
C
C        CALLED FROM:   ZBRENT
C                       PSIDE
C                       PLUMEF
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN.INC'

C     Variable Initializations
CISC2      MODNAM = 'XWIDTH'

      U1 = UVERT(IVERT)
      U2 = UVERT(IVERT+1)
      V1 = VVERT(IVERT)
      V2 = VVERT(IVERT+1)
      XOUT = V1+(U-U1)*(V2-V1)/(U2-U1)

      RETURN
      END

      SUBROUTINE PWIDTH(X1,V1,VN,WIDTH)
C***********************************************************************
C                 PWIDTH Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Calculates The Effective Area of The Plume for A
C                 Certain Downwind Distance
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C                    Adapted From Codes By Richard Strelitz, CSC
C
C        DATE:    July 7, 1993
C
C        MODIFIED BY R. Brode, PES, Inc. to move calculation of dispersion
C                    coefficients to a new ADIS subroutine - 7/21/94
C
C        MODIFIED BY R. Brode, PES, Inc. to correct table of GA values
C                    and extend GA to 79 values - 7/29/94
C
C        INPUTS:  Downwind Distance
C                 Crosswind Distance
C                 Lateral Dispersion Parameter
C                 Vertical Dispersion Parameter
C                 Receptor Height Above Ground
C                 Source Parameter Arrays
C
C        OUTPUTS: The Effective Width
C
C        CALLED FROM:   ZBRENT
C                       PSIDE
C                       PLUMEF
C                       AREAIN
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN.INC'
      REAL GA(79)

C     Variable Initializations
c     GA ARE VALUES OF THE CUMULATIVE NORMAL DISTRIBUTION IN
C     INCREMENTS OF 0.1 S.
      DATA GA/0.0,.0001,.0001,.0002,.0002,.0003,.0005,.0007,.0010,.0013,
     1.0019,.0026,.0035,.0047,.0062,.0082,.0107,.0139,.0179,.0227,.0287,
     2.0359,.0446,.0548,.0668,.0808,.0968,.1151,.1357,.1587,.1841,.2119,
     3.2420,.2742,.3085,.3445,.3821,.4207,.4602,.5000,.5398,.5793,.6179,
     4.6555,.6915,.7258,.7580,.7881,.8159,.8413,.8643,.8849,.9032,.9192,
     5.9332,.9452,.9554,.9641,.9713,.9773,.9821,.9861,.9893,.9918,.9938,
     6.9953,.9965,.9974,.9981,.9987,.9990,.9993,.9995,.9997,.9998,.9998,
     7.9999,.9999,1.000/
CISC2      MODNAM = 'PWIDTH'

      IF (X1 .EQ. 0.0) THEN
         SZ = 1.0
         SY = 1.0
         VN = V1*1000.0
         WIDTH = VN
C        Exit Routine
         GO TO 999
      END IF

      XARG = X1*1000.0
      SZ = 1.0
      SY = 1.0
      IF (RURAL .AND. X1 .LE. 0.0005) THEN
         CALL SZCOEF( X1, AFAC, BFAC, X1MAX, X1MIN )
         SZ = AFAC
      ELSE
C        Calculate Sigma-y from curves
         CALL SIGY(XARG)
C        Calculate Sigma-z from curves
         CALL SIGZ(XARG)
      END IF
      SY = MAX(SY,0.0001)
      SZ = MAX(SZ,0.0001)

      VN = 1000.0*V1/SY
      TEMP = 10*VN + 40
      ITEMP = INT(TEMP)
      WIDTH = 0.0

      IF (ITEMP. GT. 78) THEN
         WIDTH = 1.0000
      ELSE
         IF (ITEMP .GE. 1) THEN
            WIDTH = GA(ITEMP)+(TEMP-FLOAT(ITEMP))*
     1              (GA(ITEMP+1)-GA(ITEMP))
         END IF
      END IF

 999  RETURN
      END

      SUBROUTINE ZBRENT(IFD,X1,X2,TOL,OUTVAL)
C***********************************************************************
C                 ZBRENT Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: To Divide The Segments According to The Plume Split
C                 And Edge Effects
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C                    Adapted From Codes By Richard Strelitz, CSC
C
C        DATE:    July 7, 1993
C
C        INPUTS:  Downwind Distance
C                 Crosswind Distance
C                 Plume Height
C                 Lateral Dispersion Parameter
C                 Vertical Dispersion Parameter
C                 Source Parameter Arrays
C
C        OUTPUTS: The Effective Integration Segments
C
C        CALLED FROM:   PSIDE
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN.INC'

C     Variable Initializations
CISC2      MODNAM = 'ZBRENT'

      a1 = x1
      b1 = x2
      call xwidth(a1,v1)
      call pwidth(a1,v1,vn,w1)
      fa = vn-ifd*4.0
      call xwidth(b1,v1)
      call pwidth(b1,v1,vn,w1)
      fb = vn-ifd*4.0
      IF (fb*fa .LE. 0.0) THEN
         fc = fb
         do 11 iter = 1, itmax
            IF (fb*fc .gt. 0.0) THEN
               c1 = a1
               fc = fa
               d1 = b1-a1
               e1 = d1
            END IF
            IF (abs(fc) .lt. abs(fb)) THEN
               a1 = b1
               b1 = c1
               c1 = a1
               fa = fb
               fb = fc
               fc = fa
            END IF
            tol1 = 2.0*eps*abs(b1)+0.5*tol
            xm = 0.5*(c1-b1)
            IF (abs(xm).le.tol1  .or. fb .EQ. 0.0) THEN
               outval = b1
               RETURN
            END IF
            IF (abs(e1).ge.tol1 .AND. abs(fa).gt.abs(fb)) THEN
               s1 = fb/fa
               IF (a1 .EQ. c1)THEN
                  p1 = 2.0*xm*s1
                  q1 = 1.0-s1
               else
                  q1 = fa/fc
                  r1 = fb/fc
                  p1 = s1*(2.0*xm*q1*(q1-r1)-(b1-a1)*(r1-1.0))
                  q1 = (q1-1.0)*(r1-1.0)*(s1-1.0)
               END IF
               IF(p1. gt. 0.0) q1 = -q1
               p1 = abs(p1)
               IF (2.0*p1.lt.min(3.0*xm*q1-
     &             abs(tol1*q1),abs(e1-q1))) THEN
                  e1 = d1
                  d1 = p1/q1
               else
                  d1 = xm
                  e1 = d1
               END IF
            else
               d1 = xm
               e1 = d1
            END IF
            a1 = b1
            fa = fb
            IF (abs(d1).gt. tol1)THEN
               b1 = b1+d1
            else
               b1 = b1+sign(tol1,xm)
            END IF
            call xwidth(b1,v1)
            call pwidth(b1,v1,vn,w1)
            fb = vn-ifd*4.0
  11     CONTINUE
         outval = b1
      END IF

      RETURN
      END
	
      SUBROUTINE PSIDE2(DVAL)
C***********************************************************************
C                 PSIDE2 Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Integrates Over Segments For Which ABS(VN) > VNTEST
C                 Eliminates Overlap of Segments And Useless Integration
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C                    Adapted From Codes By Richard Strelitz, CSC
C
C        DATE:    July 7, 1993
C
c        INPUTS:   Number of The Original Segments
c                  End Points Array of The Segments
C
C        OUTPUT:   The Correction of The Results From PSIDE
C
C        CALLED FROM:   AREAIN
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN.INC'
      real ulist(nvmax2), useg(nvmax,2)
      integer usign(nvmax), ufac, usegf(nvmax)
      LOGICAL Ltest1,Ltest2

C     Variable Initializations
CISC2      MODNAM = 'PSIDE2'

      j = 1
      do 1 i = 1, nsegs
         ulist(j) = uasegs(i)
         j = j+1
         ulist(j) = ubsegs(i)
         j = j+1
1     CONTINUE
      npts = 2*nsegs
      call hpsort(npts,ulist,nvmax2)
      do 10 i = 1, nsegs
         usign(i) = 1
         IF (uasegs(i) .gt. ubsegs(i)) THEN
            usign(i) = -1
            temp = uasegs(i)
            uasegs(i) = ubsegs(i)
            ubsegs(i) = temp
         END IF
         IF(uasegs(i) .EQ. ubsegs(i)) usign(i) = 0
10    CONTINUE
      iseg = 0

      do 2 i = 2,npts
         u1 = ulist(i-1)
         u2 = ulist(i)
         ufac = 0
c*****
c           compare segment [u1,u2] against each ua,ub
c*****
         IF (u1.ne.u2) THEN
            do 3 j = 1, nsegs
               IF (u1.ge.uasegs(j) .AND. u2 .le. ubsegs(j)) THEN
                  ufac = ufac + usign(j)
               END IF
3           CONTINUE
c*****
c              make table of segments and factors
c*****
            IF (ufac.ne.0) THEN
               iseg = iseg+1
               useg(iseg,1) = u1
               useg(iseg,2) = u2
               usegf(iseg) = ufac
            END IF
         END IF
2     CONTINUE
c*****
c            CONSOLIDATE SEGMENTS IF iseg>1
c*****
      nsegs = iseg
      IF (nsegs .gt. 1) THEN
         do 4 iseg = 2, nsegs
            Ltest1 = useg(iseg,1) .EQ. useg(iseg-1,2)
            Ltest2 = usegf(iseg)*usegf(iseg-1) .gt. 0
            IF (Ltest1 .AND. Ltest2) THEN
               usegf(iseg-1) = 0
               useg(iseg,1) = useg(iseg-1,1)
            END IF
4        CONTINUE
      END IF
      dval = 0.0
      IF (nsegs .gt. 0) THEN
         do 5 iseg = 1, nsegs
            IF (usegf(iseg) .ne. 0) THEN
               uav = useg(iseg,1)
               ubv = useg(iseg,2)
               ufac = usegf(iseg)
               LSEG = .TRUE.
               call qromb(uav,ubv,tmpval)
               dval = dval + ufac*tmpval
            END IF
5        CONTINUE
      END IF

      RETURN
      END

      SUBROUTINE HPSORT(NVAR,UVAR,IDIM)
C***********************************************************************
C                 HPSORT Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: A General Program For Heap Sort of An Array
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C                    Adapted From Codes By Richard Strelitz, CSC
C
C        DATE:    July 7, 1993
C
C        INPUTS:  The Array To Be Sorted
C
C        OUTPUTS: The Array Sorted
C
C        CALLED FROM:   PSIDE2
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN.INC'
      DIMENSION UVAR(IDIM)

C     Variable Initializations
CISC2      MODNAM = 'HPSORT'

      ILMID = NVAR/2 + 1
      IR = NVAR
10    CONTINUE
      IF (ilmid.gt.1) THEN
         ilmid = ilmid-1
         ru = uvar(ilmid)
      else
         ru = uvar(ir)
         uvar(ir) = uvar(1)
         ir = ir-1
         IF (ir .EQ. 1)THEN
            uvar(1) = ru
            RETURN
         END IF
      END IF
      i = ilmid
      j = ilmid+ilmid
      DO WHILE (j. le. ir)
         IF (j. lt. ir) THEN
            IF (uvar(j).lt.uvar(j+1) ) j = j+1
         END IF
         IF (ru.lt.uvar(j)) THEN
            uvar(i) = uvar(j)
            i = j
            j = 2*j
         else
            j = ir+1
         END IF
      END DO
      uvar(i) = ru
      go to 10

      RETURN
      END

C***  End new code for area source numerical integration algorithm - 7/7/93

      SUBROUTINE CAVITY
C
C        COMPUTES CONCENTRATIONS IN THE NEAR WAKE OF THE 
C        BUILDING IF THE PLUME IS TRAPPED IN THE CAVITY.  BASED ON   
C        THE METHODOLOGY DESCRIBED IN VOLUME 10 - 2ND EDITION.
C        CALCULATIONS ARE MADE FOR TWO ORIENTATIONS - FIRST WITH
C        HL ALONGWIND, THEN WITH HW ALONGWIND.
C
C        INPUTS:
C           Q      - EMISSION RATE (G/S)
C           FB     - BUOYANCY FLUX (M**4/S**3)
C           FM     - MOMENTUM FLUX (M**4/S**2)
C           TS     - STACK GAS TEMPERATURE (K)
C           VS     - STACK GAS VELOCITY (M/S)
C           DS     - STACK INSIDE DIAMETER (M)
C           HS     - STACK HEIGHT (M)
C           HB     - BUILDING HEIGHT (M)
C           HW     - BLDG CROSS-WIND WIDTH (MAX HORIZ DIMENSION) (M)
C           HL     - BLDG ALONG-WIND LENGTH (MIN HORIZ DIMENSION) (M)
C           TA     - AMBIENT AIR TEMPERATURE (K)
C
C        OUTPUTS:
C           CAVCHI - CAVITY CONCENTRATION (UG/M**3)
C           UD     - DILUTION WIND SPEED, 0.5*UC (M/S)
C           UC10M  - WIND SPEED AT 10 M FOR CAVITY ENTRAPMENT (M/S)
C           UCSTK  - WIND SPEED AT STK HT FOR CAVITY ENTRAPMENT (M/S)
C           HC     - CAVITY HEIGHT (M)
C           XR     - CAVITY LENGTH (M)
C           ALW    - ALONGWIND DIMENSION (M)
C
C        ROUTINE CALLED:
C           UCRIT  - COMPUTES CRITICAL WIND SPEED FOR ENTRAPMENT 
C
      INCLUDE 'MAIN.INC'
      REAL ALW(3), UD(2), UC10M(2), UCSTK(2), HC(2)
      X2HB  = 2.*HB
      ALW(1) = HL
      ALW(2) = HW
      ALW(3) = HL
C
C        COMPUTE THE CAVITY HEIGHT  
C
      DO 10 I = 1,2
         IF (ALW(I).GT.5.*HB) THEN
            HC(I) = HB
         ELSE
            HC(I) = HB*(1.+1.6*EXP(-1.3*ALW(I)/HB))
         END IF
C
C        COMPUTE THE MINIMUM (I.E., CRITICAL) WIND SPEED
C        NEEDED TO TRAP THE PLUME IN THE CAVITY.
C
         HCAV = HC(I)
         CALL UCRIT(HCAV,X2HB,UC,HEM)
C
C        EXTRAPOLATE THE CRITICAL WIND SPEED DOWN TO 10 M TO SEE
C        IF PLUME IS ENTRAINED INTO THE CAVITY.
C        USE 0.5*UC (<=10.0 M/S) FOR DILUTION CALCULATION.
C
         UC10M(I) = UC * (ZREF/AMAX1(10.,HS))**0.20
         UD(I) = AMIN1(10.0,UC*0.5)
         UD(I) = AMAX1(1.0,UD(I))
         UCSTK(I) = UC
C
C        COMPUTE THE CAVITY CONCENTRATION IF THE PLUME IS TRAPPED
C        IN THE CAVITY AND COMPUTE THE LENGTH OF THE CAVITY.
C
         IF (HEM.GT.HC(I) .OR. UC10M(I).GT.20.) THEN
            CAVCHI(I) = 0.
            UC10M(I)  = 99.99
            UCSTK(I)  = 99.99
            UD(I)     = 99.99
            IF (ALW(I)/HB .LT. 2.) THEN
               A = -2. + 3.7/(ALW(I)/HB)**0.33333
               B = -0.15 + 0.305/(ALW(I)/HB)**0.33333
               XR(I) = A*ALW(I+1)/(1. + B*ALW(I+1)/HB)
            ELSE
               XR(I) = 1.75*ALW(I+1)/(1. + 0.25*ALW(I+1)/HB)
            END IF
         ELSE
            CAVCHI(I) = Q/(1.5*ALW(I+1)*HB*UD(I))*1.0E06
            IF (ALW(I)/HB .LT. 2.) THEN
               A = -2. + 3.7/(ALW(I)/HB)**0.33333
               B = -0.15 + 0.305/(ALW(I)/HB)**0.33333
               XR(I) = A*ALW(I+1)/(1. + B*ALW(I+1)/HB)
            ELSE
               XR(I) = 1.75*ALW(I+1)/(1. + 0.25*ALW(I+1)/HB)
            END IF
         END IF
10    CONTINUE
C
C        PRINT OUT SUMMARY TABLES
C
      WRITE(IPRT,600) (CAVCHI(I),I=1,2)
      WRITE(IOUT,600) (CAVCHI(I),I=1,2)
600   FORMAT(/,1X,' *** CAVITY CALCULATION - 1 ***',6X,
     &            ' *** CAVITY CALCULATION - 2 ***',/,
     &            '   CONC (UG/M**3)     = ',G12.4,4X,
     &            'CONC (UG/M**3)     = ',G12.4)
      IF (ABS(HANE-10.0) .LT. .1) THEN
         WRITE(IPRT,610) (UC10M(I),I=1,2)
         WRITE(IOUT,610) (UC10M(I),I=1,2)
       ELSE
         WRITE(IPRT,612) (UC10M(I),I=1,2)
         WRITE(IOUT,612) (UC10M(I),I=1,2)
      END IF
610   FORMAT(3X,2('CRIT WS @10M (M/S) = ',F8.2,8X))
612   FORMAT(3X,2('CRIT WS @HANE(M/S) = ',F8.2,8X))
      WRITE(IPRT,620) (UCSTK(I),I=1,2)
      WRITE(IOUT,620) (UCSTK(I),I=1,2)
620   FORMAT(3X,2('CRIT WS @ HS (M/S) = ',F8.2,8X))
      WRITE(IPRT,630) (UD(I),I=1,2)
      WRITE(IOUT,630) (UD(I),I=1,2)
630   FORMAT(3X,2('DILUTION WS (M/S)  = ',F8.2,8X))
      WRITE(IPRT,640) (HC(I),I=1,2)
      WRITE(IOUT,640) (HC(I),I=1,2)
640   FORMAT(3X,2('CAVITY HT (M)      = ',F8.2,8X))
      WRITE(IPRT,650) (XR(I),I=1,2)
      WRITE(IOUT,650) (XR(I),I=1,2)
650   FORMAT(3X,2('CAVITY LENGTH (M)  = ',F8.2,8X))
      WRITE(IPRT,660) (ALW(I),I=1,2)
      WRITE(IOUT,660) (ALW(I),I=1,2)
660   FORMAT(3X,2('ALONGWIND DIM (M)  = ',F8.2,8X))
      IF (UC10M(1) .EQ. 99.99 .OR. UC10M(2) .EQ. 99.99) THEN
         WRITE(IPRT,670)
         WRITE(IOUT,670)
670      FORMAT(/1X,'CAVITY CONC NOT CALCULATED FOR CRIT WS > 20.0 M/S',
     &          '.  CONC SET = 0.0')
      END IF
C
      RETURN
      END

      SUBROUTINE UCRIT(HC,X2HB,UC,HEM)
C
C        SUBROUTINE TO COMPUTE THE CRITICAL WIND SPEED NEEDED TO KEEP
C        THE PLUME WITHIN THE BUILDING CAVITY AND THE STACK HT 
C        MODIFIED FOR STACK TIP DOWNWASH.
C
C        INPUT VARIABLES:
C           HS    - STK HT (M)
C           DS    - STK DIAM (M)
C           VS    - STK GAS EXIT VELOCITY (M/S)
C           HC    - CAVITY HT (M)
C           FB    - BUOYANCY FLUX (M**4/S**3)
C           FM    - MOMENTUM FLUX (M**4/S**2)
C           X2HB  - 2*BUILDING HT (M)
C           ZREF  - ANEMOMETER HEIGHT
C     
C        OUTPUTS:
C           UC    - CRITICAL WIND SPEED (M/S)
C           HEM   - TRANSITIONAL MOMENTUM PLUME HEIGHT FOR UC (M)
C
C        ROUTINE USED:
C           HSPRM - COMPUTED STACK HT DUE TO DOWNWASH
C           HM    - COMPUTES EFFECTIVE PLUME HT FOR MOMENTUM RISE
C
C        SUBROUTINE ITERATES TO FIND THE CRITICAL WIND SPEED.
C        UP TO 20 ITERATIONS ARE ALLOWED FOR CONVERGENCE.  THE
C        CONVERGENCE CRITERIA IS FOR THE EFFECTIVE HT TO BE WITHIN
C        1 PERCENT AND THE CRITICAL WIND SPEED TO BE WITHIN 0.1 M/S.
C
C        BEGIN BY SETTING THE INITIAL WIND SPEED AT ANEMOMETER HEIGHT
C        TO 1 M/S AND THE NEXT WIND SPEED AT 20 M/S. THESE TWO WIND
C        SPEEDS SHOULD SERVE AS A BOUND FOR THE CRITICAL WIND SPEED.
C        IF NOT THE ROUTINE RETURNS TO CAVITY.
C
      INCLUDE 'MAIN.INC'

      U0TEN = 1.
      U1TEN = 20.
      U0 = U0TEN*(AMAX1(10.,HS)/ZREF)**0.20
      U1 = U1TEN*(AMAX1(10.,HS)/ZREF)**0.20
      UMIN = U0
      UMAX = U1
      NEUTRL = .TRUE.
      UNSTAB = .FALSE.
      STABLE = .FALSE.
      US = U0
      CALL DISTF
      CALL DHPMOM(X2HB)
      HE0 = HSPRIM(US,VS,HS,DS) + DHPM
      IF (HE0.LT.HC) THEN
         UC  = U0
         HEM = HE0
         RETURN
      END IF
      US = U1
      CALL DISTF
      CALL DHPMOM(X2HB)
      HE1 = HSPRIM(US,VS,HS,DS) + DHPM
      IF (HE1.GT.HC) THEN
         UC  = U1
         HEM = HE1
         RETURN
      END IF
C
C        BEGIN ITERATION TO FIND CRITICAL WIND SPEED
C
      DO 10 I = 1,20
         IF (HE0.LT.HC) THEN
            IF (HE1.LT.HC) THEN
               UNEW = AMIN1(U1,U0) - ABS(U1-U0) * 0.5
            ELSE
               UNEW = (U0+U1) * 0.5
            END IF
         ELSE
            IF (HE1.LT.HC) THEN
               UNEW = (U1+U0) * 0.5
            ELSE
               UNEW = AMAX1(U0,U1) + ABS(U1-U0)*0.5
            END IF
         END IF
         IF (UNEW.LT.UMIN .AND. U1.LT.UMIN) THEN
            WRITE(IPRT,*) 'ITERATION ERROR, UNEW AND U1 < UMIN'
            STOP
         END IF
         IF (UNEW.GT.UMAX .AND. U1.GT.UMAX) THEN
            WRITE(IPRT,*) 'ITERATION ERROR, UNEW AND U1 > UMAX'
            STOP
         END IF
         U0 = U1
         U1 = UNEW
         HE0 = HE1
         US = U1
         CALL DISTF
         CALL DHPMOM(X2HB)
         HE1 = HSPRIM(US,VS,HS,DS) + DHPM
         IF (ABS((HE0-HE1)/(0.5*(HE0+HE1))).LT.0.01 .AND.
     &       ABS(U0-U1).LT.0.1)   THEN
            UC = (U1+U0)*0.5
            HEM = HC
            RETURN
         END IF
10    CONTINUE
C
      WRITE(IPRT,*) 'CONVERGENCE NOT OBTAINED FOR UCRIT'

      RETURN
      END

      SUBROUTINE FUMI(CHI,XMAX)
C        
C        DETERMINES THE MAGNITUDE AND THE DISTANCE OF THE 
C        MAXIMUM FUMIGATION CONCENTRATION FOR RURAL SITES.
C
C        INPUTS:
C           Q     - EMISSION RATE (G/S)
C           HS    - STACK HEIGHT (M)
C           VS    - STACK GAS EXIT VELOCITY (M/S)
C           DS    - STACK DIAMETER (M)
C           TS    - STACK GAS TEMPERATURE (K)
C           TA    - AMBIENT AIR TEMPERATURE (K)
C
C        OUTPUTS:
C           CHI   - FUMIGATION CONCENTRATION (UG/M**3)
C           XMAX  - DISTANCE TO CONCENTRATION (M)
C
C        ROUTINES CALLED:
C           DELH   - COMPUTES FINAL PLUME RISE
C           HSPRM  - COMPUTES HS DUE TO STACK TIP DOWNWASH
C           FUMMAX - COMPUTES DISTANCE TO MAX FUMIGATION CONC.
C
      INCLUDE 'MAIN.INC'
C
C        COMPUTE EFFECTIVE PLUME HEIGHT
C
      KST = 6
      US  = 2.5
      DTDZ = 0.035
      IF (DTDZ .GT. 0.0 .AND. TA .NE. 0.0) THEN
         S = G*DTDZ/TA
         RTOFS = SQRT(S)
      ELSE
         S = 1.0E-10
         RTOFS = 1.0E-10
      END IF
C     Reset Terrain Height to 0.0
      HTER = 0.0
      WAKE   = .FALSE.
      WAKESS = .FALSE.
      NOBID  = .FALSE.
      NOSTD  = .FALSE.
      GRDRIS = .FALSE.
      STABLE = .TRUE.
      NEUTRL = .FALSE.
      UNSTAB = .FALSE.
      FSTREC = .TRUE.
      CALL DISTF
      CALL PHEFF
C
C        DETERMINE DISTANCE TO MAXIMUM CONCENTRATION
C
      CALL FUMMAX(XMAX)
C
C        COMPUTE CONCENTRATION USING TURNER'S WADE, EQN. 5.2. 
C        SET CHI = 0.0 IF XMAX < 2000.
C
      CHI = Q/(2.50663*US*(SY+HE/8.)*(HE+2.*SZ))*1.0E06
      IF (XMAX .LT. 2000.) CHI = 0.0
      IF (XMAX .GE. 100000.) XMAX = 99999.99
C
C        PRINT OUT RESULTS
C
      WRITE(IPRT,700) CHI,XMAX
      WRITE(IOUT,700) CHI,XMAX
700   FORMAT(/,1X,' *** INVERSION BREAK-UP FUMIGATION CALC. ***',/
     &           ,'   CONC (UG/M**3)   = ',G12.4,/
     &           ,'   DIST TO MAX (M)  =',F9.2)
C
      IF (XMAX .LT. 2000.) THEN
         WRITE(IPRT,710)
         WRITE(IOUT,710)
710      FORMAT(/,1X,'DIST TO MAX IS < 2000. M.  CONC SET = 0.0')
      END IF
C
      RETURN
      END

      SUBROUTINE FUMMAX(XMAX)
C
C        DETERMINES THE DISTANCE OF THE MAX FUMIGATION CONC.
C        IT ALSO RETURNS THE VALUES OF SY AND SZ AT THIS DISTANCE.
C
C        INPUTS:
C           HS   - STACK HEIGHT (M)
C           HE   - EFFECTIVE PLUME HEIGHT (M)
C           DH   - PLUME RISE (M) FOR USE IN BID
C       
C        OUTPUTS:
C           XMAX - DISTANCE TO MAX FUMIGATION CONC (M)
C           SY   - SIGMA-Y AT XMAX (M)
C           SZ   - SIGMA-Z AT XMAX (M)
C
C        ROUTINES CALLED:
C           SIGY - DETERMINES SIGMA-Y
C           SIGZ - DETERMINES SIGMA-Z
C
      INCLUDE 'MAIN.INC'
      KST = 6
      U   = 2.5
C        INITIAL GUESS IS 5000 M, ITERATE TO FIND XMAX.
      X0 = 5000.
      DO 10 I = 1,20
         X = X0
         CALL PDIS
C               0.1511 = RHO*CP*DTDZ/R
         X1 = U*0.1511*(HE+2.*SZ-HS)*0.5*(HS+HE+2.*SZ)
C
C        CHECK FOR CONVERGENCE TO WITHIN 10 M.
C
         IF (ABS(X1-X0) .LT. 10.) THEN
            XMAX = X1
            X = XMAX
            CALL PDIS
            RETURN 
         ELSE
            X0 = X1
         END IF
C
C        PREVENT XMAX FROM GETTING TOO SMALL FOR NEAR ZERO PLUME RISE
C
         IF (X1 .LT. 100.) THEN
            XMAX = 100.
            RETURN
         END IF
10    CONTINUE
C
C        USE AVG. OF LAST TWO IF NO CONVERGENCE AFTER 20 ITERATIONS.
C
      XMAX = (X1 + X0) * 0.5
      X = XMAX
      CALL PDIS
      RETURN
      END

      SUBROUTINE FUMS(CHI,XMAXS)
C     
C        DETERMINE MAX GROUND LEVEL SHORELINE FUMIGATION CONCENTRATION
C        FOR RURAL SITES WITHIN 3000 M OF A LARGE BODY OF WATER.
C        ASSUMES A PARABOLIC TIBL OF THE FORM H=A*X**0.5, WHERE
C        A IS CONSERVATIVELY ASSIGNED A VALUE OF 6.0.
C       
C        INPUTS:
C           Q   - EMISSION RATE (G/S)
C           HS  - STACK HT (M)
C           VS  - EXIT VELOCITY (M/S)
C           DS  - STACK DIAMETER (M)
C           TS  - STACK TEMPERATURE (K)
C           TA  - AMBIENT TEMPERATURE (K)
C           XS  - MIN DISTANCE TO SHORELINE (M)
C
C        OUTPUTS:
C           CHI   - MAX FUMIGATION CONCENTRATION (UG/M**3)
C           XMAXS - DISTANCE TO MAX CONCENTRATION (M)
C
C        ROUTINES USED:
C           HSPRM - CALCULATES HS' DUE TO STACK DOWNWASH
C           DELH  - CALCULATES FINAL PLUME RISE
C           SIGY  - CALCULATES HORIZONTAL DISPERSION PARAMETER
C           SIGZ  - CALCULATES VERTICAL DISPERSION PARAMETER
C
      INCLUDE 'MAIN.INC'
C
C        MAKE INITIAL GUESS BASED ON PLUME CENTERLINE HEIGHT
C
      KST = 6
      US  = 2.5
      DTDZ = 0.035
      IF (DTDZ .GT. 0.0 .AND. TA .NE. 0.0) THEN
         S = G*DTDZ/TA
         RTOFS = SQRT(S)
      ELSE
         S = 1.0E-10
         RTOFS = 1.0E-10
      END IF
C     Reset Terrain Height to 0.0
      HTER = 0.0
      WAKE   = .FALSE.
      WAKESS = .FALSE.
      NOBID  = .FALSE.
      NOSTD  = .FALSE.
      GRDRIS = .FALSE.
      STABLE = .TRUE.
      NEUTRL = .FALSE.
      UNSTAB = .FALSE.
      FSTREC = .TRUE.
      CALL DISTF
      CALL PHEFF
      X0 = (HE/6.)**2.-XS
      IF (X0 .LE. 0.0) THEN
         CHI = 0.0
         XMAXS = 0.0
         WRITE(IPRT,100) XS
         WRITE(IOUT,100) XS
100      FORMAT(/,1X,'***',/,1X,' PLUME HEIGHT IS BELOW TIBL HEIGHT',
     &          /,1X,' FOR DISTANCE TO SHORELINE OF ',F8.2,' M.',
     &          /,1X,' NO SHORELINE FUMIGATION CALCULATION MADE.',
     &          /,1X,'***',/)
         RETURN
      END IF
      DO 10 I = 1,20
C
C        CALCULATE SIGMA-Z WITH B.I.D.
C
         X = X0
         CALL PDIS
         X1 = ((HE+2.*SZ)/6.)**2.-XS
C
C        CHECK FOR CONVERGENCE TO WITHIN 10 M.
C
         IF (ABS(X1-X0) .LT. 10.) THEN
            XMAXS = X1
            X = XMAXS
            CALL PDIS
            GO TO 20
         ELSE
            X0 = X1
         END IF
10    CONTINUE
C
C        USE AVG. OF LAST TWO IF NO CONVERGENCE AFTER 20 ITERATIONS.
C
      XMAXS = (X1 + X0) * 0.5
      X = XMAXS
      CALL PDIS
C
C        CALCULATE CONCENTRATION USING TURNER'S WADE, EQN. 5.2.
C        SET CHI = 0.0 FOR XMAXS < 200. M.
C
20    CHI = Q/(2.50663*US*(SY+HE/8.)*(HE+2.*SZ))*1.0E06
      IF (XMAXS .LT. 200.) CHI = 0.0
C
C        PRINT OUT RESULTS
C
      WRITE(IPRT,800) CHI,XMAXS,XS
      WRITE(IOUT,800) CHI,XMAXS,XS
800   FORMAT(/,1X,' *** SHORELINE FUMIGATION CALC. ***',/
     &           ,'   CONC (UG/M**3)   = ',G12.4,/
     &           ,'   DIST TO MAX (M)  = ',F8.2,/
     &           ,'   DIST TO SHORE (M)= ',F8.2)
C
      IF (XMAXS .LT. 200.) THEN
         WRITE(IPRT,810)
         WRITE(IOUT,810) 
810      FORMAT(/1X,'DIST TO MAX IS < 200. M.  CONC SET = 0.0')
      END IF
C
      RETURN
      END

      SUBROUTINE VALLEY
C
C        VALLEY PERFORMS COMPLEX TERRAIN SCREENING FOR ELEVATED TERRAIN 
C        ABOVE STACK HEIGHT.  FOR TERRAIN ABOVE PLUME HEIGHT THE VALLEY
C        SCREENING TECHNIQUE IS USED ASSUMING F (RURAL) OR E (URBAN)
C        STABILITY, US = 2.5 M/S, AND HE = 10.0 M.  FOR TERRAIN BETWEEN
C        STACK HEIGHT AND PLUME HEIGHT, THE MAXIMUM OF THE VALLEY
C        SCREEN AND SIMPLE TERRAIN MODELING (WITH TERRAIN CHOPPED OFF
C        AT STACK HEIGHT) IS USED AS THE CONTROLLING VALUE.
C
C        INPUTS:
C           Q    - EMISSION RATE (G/S)
C           HS   - STACK HT (M)
C           DS   - STACK DIAMETER (M)
C           VS   - EXIT VELOCITY (M/S)
C           TS   - STACK TEMPERATURE (K)
C           TA   - AMBIENT TEMPERATURE (K)
C           ZR   - RECEPTOR HEIGHT ABOVE GROUND (M)
C           IOPT - URBAN/RURAL OPTION (U=URBAN, R=RURAL)
C           HTER - TERRAIN HEIGHT ABOVE STACK BASE (M)
C
C        OUTPUTS:
C           CHICNT- MAXIMUM 24-HR CONCENTRATION AT DISTANCE X (UG/M**3)
C
C        ROUTINES USED:
C           HSPRM  - CALCULATES HS' DUE TO STACK TIP DOWNWASH
C           DHHS   - CALCULATES PARTIAL PLUME RISE
C           SIGY   - CALCULATES HORIZONTAL DISPERSION PARAMETER
C           SIGZ   - CALCULATES VERTICAL DISPERSION PARAMETER
C           USERX  - CALCULATES MAX CONC AT DIST X FOR SIMPLE TERRAIN
C
      INCLUDE 'MAIN.INC'

C
C        SAVE RECEPTOR HEIGHT AND BUILDING DIMENSIONS, AND SET 
C        EQUAL TO ZERO FOR VALLEY CALCULATIONS.
C
      ZRSAV  = ZR
      HBSAV  = HB
      HLSAV  = HL
      HWSAV  = HW
      HWPSAV = HWP
      ZR  = 0.0
      HB  = 0.0
      HL  = 0.0
      HW  = 0.0
      HWP = 0.0
C
      WRITE(IOUT,102) RUNDAT, RUNTIM
102   FORMAT(70X,A8/70X,A8)

      IF (POINT) THEN
         WRITE(IOUT,103) VERSN, TITLE, Q, HS, DS, VS, TS, TA, ZR, KPRT
103      FORMAT(' ',1X,'***  SCREEN3 MODEL RUN  ***',
     &            /,2X,'*** VERSION DATED ',A5,' ***',//,1X,A79,//,
     &          1X,'COMPLEX TERRAIN INPUTS:',/,
     &          1X,'   SOURCE TYPE            =        POINT',/,
     &          1X,'   EMISSION RATE (G/S)    = ',G16.6,/,
     &          1X,'   STACK HT (M)           = ',F12.4,/,
     &          1X,'   STACK DIAMETER (M)     = ',F12.4,/,
     &          1X,'   STACK VELOCITY (M/S)   = ',F12.4,/,
     &          1X,'   STACK GAS TEMP (K)     = ',F12.4,/,
     &          1X,'   AMBIENT AIR TEMP (K)   = ',F12.4,/,
     &          1X,'   RECEPTOR HEIGHT (M)    = ',F12.4,/,
     &          1X,'   URBAN/RURAL OPTION     = ',7X,A5,/)
      IF (ICI .EQ. 1) THEN
         WRITE(IOUT, 105)
        ELSE
         WRITE(IOUT, 106)
      END IF
      IF (HANE .EQ. 10.0) THEN
         WRITE(IOUT, 107) HANE
        ELSE
         WRITE(IOUT, 108) HANE
      END IF

105   FORMAT(' THE NON-REGULATORY BUT CONSERVATIVE BRODE 2 MIXING'
     +       ' HEIGHT OPTION WAS SELECTED.')
106   FORMAT(' THE REGULATORY (DEFAULT) MIXING HEIGHT OPTION WAS',
     +       ' SELECTED.')
107   FORMAT(' THE REGULATORY (DEFAULT) ANEMOMETER HEIGHT OF',F5.1,
     +       ' METERS WAS ENTERED.'/)
108   FORMAT(' A NON-REGULATORY ANEMOMETER HEIGHT (HANE) OF',F6.1,
     +       ' METERS WAS ENTERED.'/)


      ELSE IF (FLARE) THEN
         WRITE(IOUT,220) VERSN, TITLE, Q, HSTK, H, ZR, KPRT, HS
220      FORMAT(' ',1X,'***  SCREEN3 MODEL RUN  ***',
     &            /,2X,'*** VERSION DATED ',A5,' ***',//,1X,A79,//,
     &          1X,'COMPLEX TERRAIN INPUTS:',/,
     &          1X,'   SOURCE TYPE            =        FLARE',/,
     &          1X,'   EMISSION RATE (G/S)    = ',G16.6,/,
     &          1X,'   FLARE STACK HEIGHT (M) = ',F12.4,/,
     &          1X,'   TOT HEAT RLS (CAL/S)   = ',G16.6,/,
     &          1X,'   RECEPTOR HEIGHT (M)    = ',F12.4,/,
     &          1X,'   URBAN/RURAL OPTION     = ',7X,A5,/,
     &          1X,'   EFF RELEASE HEIGHT (M) = ',F12.4,/)
      IF (ICI .EQ. 1) THEN
         WRITE(IOUT, 105)
        ELSE
         WRITE(IOUT, 106)
      END IF
      IF (HANE .EQ. 10.0) THEN
         WRITE(IOUT, 107) HANE
        ELSE
         WRITE(IOUT, 108) HANE
      END IF


      ELSE
         RETURN
      END IF

      IF (VS .LT. 1.0E-05) VS = 1.0E-05
      IF (DS .LT. 1.0E-05) DS = 1.0E-05
C
C        CALCULATE BUOYANCY AND MOMENTUM FLUX AND WRITE TO FILE.
C
      IF (TA .GT. TS) THEN
         TS = TA
         FB = 0.0
         FM = TA*VS*VS*DS*DS/(4.*TS)
         WRITE(IPRT,*) 'TA > TS!!!  BUOY. FLUX SET = 0.0'
         WRITE(IOUT,*) 'TA > TS!!!  BUOY. FLUX SET = 0.0'
      ELSE
         FB = G*VS*DS*DS*(TS-TA)/(4.*TS)
         FM = TA*VS*VS*DS*DS/(4.*TS)
      END IF
      WRITE(IOUT,110) FB,FM
110   FORMAT(/,1X,'BUOY. FLUX =',F9.3,' M**4/S**3;  MOM. FLUX =',
     &       F9.3,' M**4/S**2.',/)
C
C        CALCULATE FINAL STABLE RISE (F or E, 2.5 m/s) AND DISTANCE
C        TO FINAL RISE FOR REFERENCE
C
      US = 2.5
      IF (RURAL) THEN
         KST = 6
         DTDZ = 0.035
      ELSE IF (URBAN) THEN
         KST = 5
         DTDZ = 0.02
      END IF
      IF (DTDZ .GT. 0.0 .AND. TA .NE. 0.0) THEN
         S = G*DTDZ/TA
         RTOFS = SQRT(S)
      ELSE
         S = 1.0E-10
         RTOFS = 1.0E-10
      END IF
      WAKE   = .FALSE.
      WAKESS = .FALSE.
      NOBID  = .FALSE.
      NOSTD  = .FALSE.
      GRDRIS = .FALSE.
      STABLE = .TRUE.
      UNSTAB = .FALSE.
      NEUTRL = .FALSE.
      FSTREC = .TRUE.
C     Determine Distance to Final Plume Rise
      CALL DISTF
      X = XF
C     Determine Final Plume Height
      CALL PHEFF
      HEF = HE
      WRITE(IPRT,250) HEF,XF
      WRITE(IOUT,250) HEF,XF
250   FORMAT(/,1X,'FINAL STABLE PLUME HEIGHT (M) = ',F6.1,
     &       /,1X,'DISTANCE TO FINAL RISE (M)    = ',F6.1,/)
      WRITE(IPRT,260) 
260   FORMAT(1X,'MAXIMUM CONCENTRATIONS ARE EXPECTED TO OCCUR DUE TO',
     &   ' PLUME IMPACTION.'/1X,'THEREFORE ENTER MINIMUM DISTANCES AND',
     &   ' TERRAIN HEIGHTS FOR WHICH',/,1X,'IMPACTION IS LIKELY, ',
     &   'TAKING INTO ACCOUNT TERRAIN CLOSER THAN',/,1X,'THE DISTANCE',
     &   ' TO FINAL RISE.',/,1X,'FOR TERRAIN BELOW PLUME HEIGHT, SIMPLE'
     &   ,' TERRAIN AND VALLEY 24-HR',/,1X,'CALCULATIONS ARE BOTH MADE',
     &   ' AND THE MAXIMUM SELECTED.')
C
C        INITIALIZE, THEN ENTER TERRAIN HEIGHT AND DISTANCE
C
      N = 0
1     CHIVAL = 0.0
      CHISIM = 0.0
      CHICNT = 0.0
      HEV = 0.0
      HES = 0.0
      KSTS = 0
C     Inputs Have Been Modified to Read Terrain Height and Distance Together
      WRITE(IPRT,*) ' '
      WRITE(IPRT,*) 'ENTER TERRAIN HEIGHT ABOVE STACK BASE (M), '
      WRITE(IPRT,*) 'AND DISTANCE TO TERRAIN (M) (ZEROES TO EXIT):'
      READ(IRD,*,ERR=1) HTER, X
      WRITE(IDAT,*) HTER, X
      IF (HTER .EQ. 0. .OR. X .EQ. 0.) GO TO 99
      IF (HTER .LE. HS) THEN
         WRITE(IPRT,*) ' TERRAIN HEIGHT IS NOT ABOVE STACK HEIGHT.'
         GO TO 1
      END IF
      IF (X .GT. 100000.) THEN
         WRITE(IPRT,*) 'DISTANCE IS > 100 KM!  TRY AGAIN.'
         GO TO 1
      END IF
99    IF (X .GE. 1. .AND. HTER .GE. 1.) THEN
         N = N + 1
C
C        CALCULATE PLUME HEIGHT USING VALLEY SCREEN CONDITIONS
C        WITH GRADUAL RISE (GRDRIS = .TRUE.)
C
         US = 2.5
         IF (RURAL) THEN
            KST = 6
            DTDZ = 0.035
         ELSE IF (URBAN) THEN
            KST = 5
            DTDZ = 0.02
         END IF
         IF (DTDZ .GT. 0.0 .AND. TA .NE. 0.0) THEN
            S = G*DTDZ/TA
            RTOFS = SQRT(S)
         ELSE
            S = 1.0E-10
            RTOFS = 1.0E-10
         END IF
         WAKE   = .FALSE.
         WAKESS = .FALSE.
         NOBID  = .FALSE.
         NOSTD  = .FALSE.
         STABLE = .TRUE.
         UNSTAB = .FALSE.
         NEUTRL = .FALSE.
         FSTREC = .TRUE.
         GRDRIS = .TRUE.
         CALL DISTF
         CALL PHEFF
         HEC = HE
         IF (HE .LE. HTER) THEN
C
C        VALLEY 24-HR SCREENING CALCULATION
C
            HEV = 10.0
            CALL PDIS
C
C        CALCULATE SECTOR-AVERAGED CONCENTRATION
C
            CHIVAL = 2.032*Q/(SZ*US*X)*EXP(-0.5*(HEV/SZ)**2.)*1.0E06
C     
C        MULTIPLY 1-HR VALLEY CHI BY 0.25 TO OBTAIN 24-HR CHI
C  
            CHIVAL = CHIVAL * 0.25
         ELSE
            HEV = AMAX1(10.0,HE-HTER)
            CALL PDIS
C
C        CALCULATE SECTOR-AVERAGED CONCENTRATION
C
            CHIVAL = 2.032*Q/(SZ*US*X)*EXP(-0.5*(HEV/SZ)**2.)*1.0E06
C     
C        MULTIPLY 1-HR VALLEY CHI BY 0.25 TO OBTAIN 24-HR CHI
C  
            CHIVAL = CHIVAL * 0.25
C
C        COMPARE WITH SIMPLE TERRAIN CALCULATION USING TERRAIN CHOPPED
C        OFF AT STACK HEIGHT.
C
            KMAX = 6
            DO 20 I=1,KMAX
               IST(I) = I
20          CONTINUE
            GRDRIS = .FALSE.
            ELEV = .TRUE.
            FLAT = .FALSE.
            WSINP = .FALSE.
            UINP = 1.0
            HTFULL = HTER
            HTER = HS
C
            CALL USERX

C           Save Max. Values from USERX
            CHISIM  = CHIMAX
            UREFOUT = UMAX
            USOUT   = USMAX
            HES     = HEMAX
            ZIOUT   = ZIMAX
            KSTS    = KSTMAX
            SYOUT   = SYMAX
            SZOUT   = SZMAX
            IFLG    = IFGMAX

C
C        MULTIPLY SIMPLE TERRAIN CHI BY 0.40 TO OBTAIN 24-HR VALUE
C
            CHISIM = CHISIM * 0.40
            HTER = HTFULL
C 
         END IF
C
C        SELECT MAXIMUM OF VALLEY SCREEN AND SIMPLE TERRAIN CALCULATION
C        AS CONTROLLING VALUE.
C
         CHICNT = AMAX1(CHIVAL,CHISIM)
         IF (CHICNT .GT. CMAXCT) THEN
            CMAXCT = CHICNT
            XMAXCT = X
            TMAXCT = HTER
         END IF
         IF (N .EQ. 1) THEN
           IF (HANE .EQ. 10.0) THEN
              WRITE(IPRT,300)
              WRITE(IOUT,300)
            ELSE
              WRITE(IPRT,301)
              WRITE(IOUT,301)
           END IF
         END IF
         IF (N .EQ. 6 .OR. N .EQ. 11 .OR. N .EQ. 16) THEN
           IF (HANE .EQ. 10.0) THEN
              WRITE(IPRT,300)
            ELSE
              WRITE(IPRT,301)
           END IF
         END IF
300      FORMAT(26X,'*VALLEY 24-HR CALCS*',3X,'**SIMPLE TERRAIN 24-HR',
     &      ' CALCS**',/,1X,' TERR',9X,'MAX 24-HR',14X,
     &      'PLUME HT',13X,'PLUME HT',/,3X,'HT',4X,'DIST',2X,
     &      '  CONC',8X,'CONC',4X,'ABOVE STK',4X,'CONC',4X,'ABOVE STK'
     &      ,4X,'U10M',1X,'USTK',/,2X,'(M)',4X,'(M)',3X,'(UG/M**3)',3X,
     &      '(UG/M**3)',2X,'BASE (M)',2X,'(UG/M**3)',2X,'HGT (M)',
     &      2X,'SC',3X,'(M/S)',/,1X,'-----',1X,'-------',1X,
     &      '----------',2X,'----------',2X,'------',3X,
     &      '----------',2X,'------',2X,'--',1X,'----',
     &      1X,'----')
301      FORMAT(26X,'*VALLEY 24-HR CALCS*',3X,'**SIMPLE TERRAIN 24-HR',
     &      ' CALCS**',/,1X,' TERR',9X,'MAX 24-HR',14X,
     &      'PLUME HT',13X,'PLUME HT',/,3X,'HT',4X,'DIST',2X,
     &      '  CONC',8X,'CONC',4X,'ABOVE STK',4X,'CONC',4X,'ABOVE STK'
     &      ,3X,'UHANE',1X,'USTK',/,2X,'(M)',4X,'(M)',3X,'(UG/M**3)',3X,
     &      '(UG/M**3)',2X,'BASE (M)',2X,'(UG/M**3)',2X,'HGT (M)',
     &      2X,'SC',3X,'(M/S)',/,1X,'-----',1X,'-------',1X,
     &      '----------',2X,'----------',2X,'------',3X,
     &      '----------',2X,'------',2X,'--',1X,'----',
     &      1X,'----')

         WRITE(IPRT,310) HTER,X,CHICNT,CHIVAL,HEC,CHISIM,HES,KSTS,
     &                   UREFOUT,USOUT
         WRITE(IOUT,310) HTER,X,CHICNT,CHIVAL,HEC,CHISIM,HES,KSTS,
     &                   UREFOUT,USOUT
310      FORMAT(1X,F5.0,1X,F7.0,1X,2(G11.4,2X),F6.1,3X,G11.4,2X,F6.1,3X,
     &          I1,1X,F4.1,1X,F4.1)

         ELEV = .FALSE.
         FLAT = .TRUE.
         UREFOUT = 0.0
         USOUT   = 0.0

         GO TO 1
C
      ELSE
         WRITE(IPRT,*) ' '
3        WRITE(IPRT,*) 'COMPLEX TERRAIN CALCULATIONS DONE.'
         WRITE(IPRT,*) 'CONTINUE WITH SIMPLE TERRAIN CALCULATIONS?',
     &                 '  ENTER Y OR N:'
         READ(IRD,400) QUERY
400      FORMAT(A1)
         IF (QUERY .EQ. 'Y' .OR. QUERY .EQ. 'y') THEN
            WRITE(IDAT,400) QUERY
            STP = .FALSE.
            HTER = 0.0
            ZR  = ZRSAV
            HB  = HBSAV
            HL  = HLSAV
            HW  = HWSAV
            HWP = HWPSAV
         ELSE IF (QUERY .EQ. 'N' .OR. QUERY .EQ. 'n') THEN
            WRITE(IDAT,400) QUERY
            STP = .TRUE.
            WRITE(IPRT,*) ' '
         ELSE 
            GO TO 3
         END IF

      END IF

      RETURN
      END

      SUBROUTINE PRTOUT(IOUT)
C						  
C      SUBROUTINE TO PRINT FORTRAN FORMATTED OUTPUT FILE	  
C      ON PRINTER WITH CARRIAGE CONTROLS OBSERVED.	  
C
      CHARACTER*1 A(80)
      CHARACTER*1 FFEED
      FFEED = CHAR(12)
      REWIND IOUT
      OPEN(1,FILE='PRN')
1     READ(IOUT,100,END=99) A
      WRITE(1,100) A
100   FORMAT(80A1)
      GO TO 1

99    WRITE(1,101) FFEED
101   FORMAT(1X,A1)

      RETURN
      END

      BLOCK DATA RURALA

      INCLUDE 'MAIN.INC'

      DATA TBLASP /1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0,2.5,
     &             3.0,3.5,4.0,4.5,5.0,6.0,7.0,8.0,9.0,10.0/

      DATA TBLDIS /0.,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,
     &             0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,
     &             0.65,0.7,0.75,0.8,0.85,0.9,0.95,1.0,1.1,1.2,1.3,
     &             1.4,1.5,1.6,1.7,1.8,1.9,2.0,2.5,3.0,3.5,4.0,4.5,
     &             5.0,6.0,7.0,8.0,9.0,10.,11.,12.,13.,14.,15.,20./
c     &             25.,30.,35.,40.,45.,50.,60.,70.,80.,90.,100.,150.,
c     &             200.,300.,400.,500./

      DATA (MXRURA(I,1),I=1,MAXCOL) /
     &45,39,34,28,24,20, 5, 5, 5, 5, 5, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXRURA(I,2),I=1,MAXCOL) /
     &45,39,33,28,24,20, 5, 5, 5, 5, 5, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXRURA(I,3),I=1,MAXCOL) /
     &45,39,33,28,24, 6, 6, 6, 6, 6, 6, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXRURA(I,4),I=1,MAXCOL) /
     &45,39,33,28,24, 6, 6, 6, 6, 6, 4, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0/
      DATA (MXRURA(I,5),I=1,MAXCOL) /
     &45,39,33,28,24, 6, 6, 6, 6, 6, 5, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0/
      DATA (MXRURA(I,6),I=1,MAXCOL) /
     &45,39,33,28,23, 6, 6, 6, 6, 6, 4, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXRURA(I,7),I=1,MAXCOL) /
     &45,39,33,30,23, 6, 6, 6, 6, 6, 4, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXRURA(I,8),I=1,MAXCOL) /
     &45,38,33,31,23, 6, 6, 6, 6, 6, 3, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXRURA(I,9),I=1,MAXCOL) /
     &45,38,32,32,22, 6, 6, 6, 6, 6, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXRURA(I,10),I=1,MAXCOL) /
     &45,38,32,33,22, 6, 6, 6, 6, 6, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXRURA(I,11),I=1,MAXCOL) /
     &45,38,32,35,22, 6, 6, 6, 7, 4, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXRURA(I,12),I=1,MAXCOL) /
     &45,38,31,27, 7, 7, 7, 7, 6, 3, 2, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXRURA(I,13),I=1,MAXCOL) /
     &45,37,34,25, 7, 8, 8, 8, 3, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXRURA(I,14),I=1,MAXCOL) /
     &45,37,39, 8, 8, 8, 9, 4, 2, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXRURA(I,15),I=1,MAXCOL) /
     &45,36,30, 9, 9, 9, 7, 3, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXRURA(I,16),I=1,MAXCOL) /
     &44,36,29, 9,10,11, 3, 2, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXRURA(I,17),I=1,MAXCOL) /
     &44,40,10,10,11, 7, 3, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXRURA(I,18),I=1,MAXCOL) /
     &44,36,11,12,13, 2, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXRURA(I,19),I=1,MAXCOL) /
     &42,35,12,13,14, 2, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXRURA(I,20),I=1,MAXCOL) /
     &42,34,13,15, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0/
      DATA (MXRURA(I,21),I=1,MAXCOL) /
     &43,13,15,17, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXRURA(I,22),I=1,MAXCOL) /
     &45,15,17, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXRURA(I,23),I=1,MAXCOL) /
     &44,17,19, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXRURA(I,24),I=1,MAXCOL) /
     &44,19,22, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXRURA(I,25),I=1,MAXCOL) /
     &44,21,25, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXRURA(I,26),I=1,MAXCOL) /
     &20,23, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXRURA(I,27),I=1,MAXCOL) /
     &22,26, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXRURA(I,28),I=1,MAXCOL) /
     &24,30, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXRURA(I,29),I=1,MAXCOL) /
     &27,33, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXRURA(I,30),I=1,MAXCOL) /
     &33,41,24,24,29,25,24,24,24,24,24,21,16,12, 9, 5, 4, 1, 1, 0, 1, 0/
      DATA (MXRURA(I,31),I=1,MAXCOL) /
     &40,43,34,37,35,33,31,30,28,26,25,18,12, 6, 1, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXRURA(I,32),I=1,MAXCOL) /
     &41,42,40,37,34,32,30,28,26,24,22,14, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXRURA(I,33),I=1,MAXCOL) /
     &45,42,39,36,33,31,29,26,24,22,20,10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXRURA(I,34),I=1,MAXCOL) /
     &45,42,39,36,32,30,27,25,19,19,17, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXRURA(I,35),I=1,MAXCOL) /
     &45,42,38,35,32,28,25,22,20,16,13, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXRURA(I,36),I=1,MAXCOL) /
     &45,41,38,35,30,27,23,20,17,13, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXRURA(I,37),I=1,MAXCOL) /
     &45,41,37,34,29,25,23,18,14, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXRURA(I,38),I=1,MAXCOL) /
     &45,41,37,33,28,24,20,14, 6, 4, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0/
      DATA (MXRURA(I,39),I=1,MAXCOL) /
     &45,41,36,32,26,22,17,11, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0/
      DATA (MXRURA(I,40),I=1,MAXCOL) /
     &45,39,31,26,19, 7, 0, 0, 0, 1, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXRURA(I,41),I=1,MAXCOL) /
     &45,35,29,17, 2, 0, 0, 0, 0, 2, 0, 1, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0/
      DATA (MXRURA(I,42),I=1,MAXCOL) /
     &45,36,24,10, 2, 0, 0, 5, 4, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0/
      DATA (MXRURA(I,43),I=1,MAXCOL) /
     &45,34,16, 1, 0, 0, 0, 2, 1, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0/
      DATA (MXRURA(I,44),I=1,MAXCOL) /
     &45,25, 2, 6, 0, 4, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXRURA(I,45),I=1,MAXCOL) /
     &43, 3, 4, 0, 5, 3, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXRURA(I,46),I=1,MAXCOL) /
     &45, 3, 0, 2, 0, 0, 0, 0, 7, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXRURA(I,47),I=1,MAXCOL) /
     &41,13, 0, 4, 0, 0, 5, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXRURA(I,48),I=1,MAXCOL) /
     &45, 5,11, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 2, 0, 0, 0, 0, 0, 0/
      DATA (MXRURA(I,49),I=1,MAXCOL) /
     &37,23,17, 1, 0, 1, 1, 2, 0, 0, 0, 0, 0, 0, 2, 1, 0, 0, 0, 0, 0, 0/
      DATA (MXRURA(I,50),I=1,MAXCOL) /
     &27, 0, 1, 2, 0, 1, 1, 0, 1, 3, 2, 1, 1, 3, 2, 1, 0, 0, 1, 0, 1, 0/
      DATA (MXRURA(I,51),I=1,MAXCOL) /
     &37, 1, 3, 0, 2, 1, 1, 3, 3, 2, 0, 0, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXRURA(I,52),I=1,MAXCOL) /
     &40, 9, 0, 3, 0, 2, 5, 7, 1, 0, 0, 2, 4, 2, 0, 0, 0, 0, 0, 1, 0, 0/
      DATA (MXRURA(I,53),I=1,MAXCOL) /
     &35, 3, 3, 0, 9, 6, 7, 0, 0, 1, 0, 4, 3, 0, 0, 0, 0, 1, 0, 0, 1, 0/
      DATA (MXRURA(I,54),I=1,MAXCOL) /
     &32, 3, 0,10, 9, 7, 0, 0, 0, 0, 0, 6, 3, 0, 0, 0, 0, 1, 0, 0, 0, 0/
      DATA (MXRURA(I,55),I=1,MAXCOL) /
     & 0, 0,15,11, 7, 0, 0, 0, 0, 2, 3, 5, 2, 0, 0, 1, 0, 0, 0, 0, 0, 1/
      DATA (MXRURA(I,56),I=1,MAXCOL) /
     &17, 6, 0, 0, 3, 6, 8,10,10, 8, 7, 0, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0/

      END

      BLOCK DATA RURALB

      INCLUDE 'MAIN.INC'

      DATA (MXRURB(I,1),I=1,MAXCOL) /                                           
     &45,40,36,33,29,26,23,20,18,15,12, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURB(I,2),I=1,MAXCOL) /                                           
     &45,40,36,33,29,26,23,20,17,14,12, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURB(I,3),I=1,MAXCOL) /                                           
     &45,40,36,32,29,26,23,20,17,14,12, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURB(I,4),I=1,MAXCOL) /                                           
     &45,40,36,32,29,26,22,20,17,14,12, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURB(I,5),I=1,MAXCOL) /                                           
     &45,40,36,32,29,25,22,20,16,13,13, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURB(I,6),I=1,MAXCOL) /                                           
     &45,40,36,32,29,25,22,19,16,13,13, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURB(I,7),I=1,MAXCOL) /                                           
     &45,40,37,32,29,25,22,19,16,13,13, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURB(I,8),I=1,MAXCOL) /                                           
     &45,40,36,32,28,25,22,19,16,13,13, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURB(I,9),I=1,MAXCOL) /                                           
     &45,40,36,32,28,25,22,19,15,13,14, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURB(I,10),I=1,MAXCOL) /                                          
     &45,40,36,32,28,25,22,19,15,13,14, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURB(I,11),I=1,MAXCOL) /                                          
     &45,40,36,32,28,25,21,18,15,13,14, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURB(I,12),I=1,MAXCOL) /                                          
     &45,40,36,32,28,24,21,17,14,15,15, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURB(I,13),I=1,MAXCOL) /                                          
     &45,40,35,31,27,23,20,16,15,16,10, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURB(I,14),I=1,MAXCOL) /                                          
     &45,40,35,31,27,22,19,15,16,15, 8, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURB(I,15),I=1,MAXCOL) /                                          
     &45,40,35,30,26,22,17,17,18,10, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURB(I,16),I=1,MAXCOL) /                                          
     &45,40,35,30,25,21,17,18,19, 7, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURB(I,17),I=1,MAXCOL) /                                          
     &45,39,34,29,24,20,18,19,10, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4/        
      DATA (MXRURB(I,18),I=1,MAXCOL) /                                          
     &45,39,34,29,24,18,19,20, 7, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 0/        
      DATA (MXRURB(I,19),I=1,MAXCOL) /                                          
     &45,39,33,28,22,19,21,10, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 0, 3/        
      DATA (MXRURB(I,20),I=1,MAXCOL) /                                          
     &45,39,33,27,22,20,22, 7, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 3, 3/        
      DATA (MXRURB(I,21),I=1,MAXCOL) /                                          
     &45,39,33,26,20,22,23, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 4, 0, 3, 3/        
      DATA (MXRURB(I,22),I=1,MAXCOL) /                                          
     &45,38,32,25,21,23,10, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 4, 3, 3, 0/        
      DATA (MXRURB(I,23),I=1,MAXCOL) /                                          
     &45,38,32,24,23,25, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 4, 3, 3, 0, 2/        
      DATA (MXRURB(I,24),I=1,MAXCOL) /                                          
     &45,38,31,23,24,26, 1, 0, 0, 0, 0, 0, 0, 0, 6, 5, 5, 4, 3, 0, 2, 2/        
      DATA (MXRURB(I,25),I=1,MAXCOL) /                                          
     &44,37,30,24,26, 9, 0, 0, 0, 0, 0, 0, 0, 0, 6, 5, 4, 0, 3, 0, 2, 2/        
      DATA (MXRURB(I,26),I=1,MAXCOL) /                                          
     &44,37,29,25,27, 2, 0, 0, 0, 0, 0, 0, 8, 6, 5, 5, 4, 3, 0, 2, 0, 0/        
      DATA (MXRURB(I,27),I=1,MAXCOL) /                                          
     &44,37,28,26,29, 1, 0, 0, 0, 0, 0, 0, 7, 6, 5, 4, 4, 3, 0, 2, 2, 0/        
      DATA (MXRURB(I,28),I=1,MAXCOL) /                                          
     &44,36,27,28,30, 0, 0, 0, 0, 0, 0, 9, 7, 5, 5, 4, 0, 3, 0, 2, 0, 0/        
      DATA (MXRURB(I,29),I=1,MAXCOL) /                                          
     &44,36,27,29, 0, 0, 0, 0, 0, 0, 0, 8, 6, 5, 4, 4, 3, 0, 2, 2, 0, 0/        
      DATA (MXRURB(I,30),I=1,MAXCOL) /                                          
     &44,34,30,33,35,24,24,24,25,24,24,21,17,14,11, 9, 6, 1, 0, 0, 0, 0/        
      DATA (MXRURB(I,31),I=1,MAXCOL) /                                          
     &43,33,33,36,36,33,32,30,28,27,25,19,14,10, 6, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURB(I,32),I=1,MAXCOL) /                                          
     &43,40,40,37,35,33,31,29,27,25,24,17,11, 5, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURB(I,33),I=1,MAXCOL) /                                          
     &45,42,40,37,35,32,29,27,26,24,22,14, 6, 0, 0, 1, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURB(I,34),I=1,MAXCOL) /                                          
     &45,42,39,36,34,32,28,26,24,22,20,11, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURB(I,35),I=1,MAXCOL) /                                          
     &45,42,39,36,34,30,28,25,23,20,18, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURB(I,36),I=1,MAXCOL) /                                          
     &45,42,38,35,32,29,26,24,21,19,16, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURB(I,37),I=1,MAXCOL) /                                          
     &45,41,38,34,31,28,25,22,19,16,13, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURB(I,38),I=1,MAXCOL) /                                          
     &45,41,38,34,31,28,24,20,17,13, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURB(I,39),I=1,MAXCOL) /                                          
     &45,41,37,33,30,25,22,19,15,11, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURB(I,40),I=1,MAXCOL) /                                          
     &45,40,35,30,25,19,11, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURB(I,41),I=1,MAXCOL) /                                          
     &45,39,32,25,17, 4, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURB(I,42),I=1,MAXCOL) /                                          
     &45,37,28,15, 2, 3, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURB(I,43),I=1,MAXCOL) /                                          
     &45,35,22, 0, 1, 2, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURB(I,44),I=1,MAXCOL) /                                          
     &43,32,15, 1, 2, 1, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURB(I,45),I=1,MAXCOL) /                                          
     &45,31, 6, 0, 0, 2, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURB(I,46),I=1,MAXCOL) /                                          
     &45,24, 3, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURB(I,47),I=1,MAXCOL) /                                          
     &42,13, 0, 0, 0, 2, 1, 0, 2, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURB(I,48),I=1,MAXCOL) /                                          
     &45,16, 0, 3, 1, 2, 2, 1, 0, 1, 2, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0/        
      DATA (MXRURB(I,49),I=1,MAXCOL) /                                          
     &41, 6, 1, 0, 3, 1, 0, 1, 2, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURB(I,50),I=1,MAXCOL) /                                          
     &45, 5, 2, 0, 0, 0, 2, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1/        
      DATA (MXRURB(I,51),I=1,MAXCOL) /                                          
     &45, 2, 1, 0, 1, 0, 0, 2, 0, 0, 2, 2, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0/
      DATA (MXRURB(I,52),I=1,MAXCOL) /                                          
     &45, 1, 3, 2, 1, 1, 4, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0/        
      DATA (MXRURB(I,53),I=1,MAXCOL) /                                          
     &43,11, 2, 0, 3, 4, 0, 1, 0, 1, 2, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1/        
      DATA (MXRURB(I,54),I=1,MAXCOL) /                                          
     &29, 0, 0, 9, 5, 0, 0, 0, 2, 4, 3, 0, 0, 2, 1, 1, 0, 0, 0, 1, 1, 0/        
      DATA (MXRURB(I,55),I=1,MAXCOL) /                                          
     &40, 0,10, 6, 0, 0, 0, 3, 4, 3, 1, 0, 0, 3, 2, 0, 0, 0, 0, 0, 1, 1/        
      DATA (MXRURB(I,56),I=1,MAXCOL) /                                          
     &31, 2, 7,11, 9, 6, 0, 0, 0, 0, 2, 6, 2, 0, 0, 1, 0, 0, 0, 0, 2, 1/

      END

      BLOCK DATA RURALC

      INCLUDE 'MAIN.INC'

      DATA (MXRURC(I,1),I=1,MAXCOL) /                                           
     &45,41,38,35,32,29,27,25,23,21,19, 2, 2, 2, 2, 2, 1, 1, 0, 0, 0, 0/        
      DATA (MXRURC(I,2),I=1,MAXCOL) /                                           
     &45,39,38,35,32,29,27,25,23,21,19, 2, 2, 2, 2, 2, 1, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,3),I=1,MAXCOL) /                                           
     &45,37,38,35,32,29,27,25,23,21,19, 2, 2, 2, 2, 2, 1, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,4),I=1,MAXCOL) /                                           
     &45,41,38,35,32,29,27,25,23,21,19, 2, 2, 2, 2, 2, 1, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,5),I=1,MAXCOL) /                                           
     &45,41,38,35,32,29,27,25,22,20,19, 2, 2, 2, 2, 2, 1, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,6),I=1,MAXCOL) /                                           
     &45,41,38,35,32,29,27,25,22,20,19, 2, 2, 2, 2, 1, 1, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,7),I=1,MAXCOL) /                                           
     &45,41,38,35,32,29,27,24,22,20,19, 2, 2, 2, 2, 1, 1, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,8),I=1,MAXCOL) /                                           
     &45,41,38,35,32,29,27,24,22,20,18, 2, 2, 2, 2, 1, 1, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,9),I=1,MAXCOL) /                                           
     &45,41,38,35,32,29,27,24,22,20,18, 2, 2, 2, 2, 1, 1, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,10),I=1,MAXCOL) /                                          
     &45,41,38,35,32,29,27,24,22,20,18, 2, 2, 2, 2, 1, 1, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,11),I=1,MAXCOL) /                                          
     &45,41,38,34,32,29,27,24,22,20,18, 2, 2, 2, 2, 1, 1, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,12),I=1,MAXCOL) /                                          
     &45,41,38,34,31,29,26,24,22,19,18, 2, 2, 3, 2, 1, 1, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,13),I=1,MAXCOL) /                                          
     &45,41,37,34,31,28,26,23,21,19,17, 3, 3, 3, 1, 1, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,14),I=1,MAXCOL) /                                          
     &45,41,37,34,31,28,26,23,21,18, 3, 3, 3, 2, 1, 1, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,15),I=1,MAXCOL) /                                          
     &45,41,37,34,31,28,25,22,20,19, 3, 3, 3, 2, 1, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,16),I=1,MAXCOL) /                                          
     &45,41,37,34,31,28,25,22,20, 3, 3, 3, 4, 1, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,17),I=1,MAXCOL) /                                          
     &45,41,37,33,30,27,24,21,24, 3, 3, 4, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,18),I=1,MAXCOL) /                                          
     &45,41,37,33,30,27,24,21,19, 3, 3, 4, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,19),I=1,MAXCOL) /                                          
     &45,41,37,33,30,27,23,27, 3, 4, 4, 5, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,20),I=1,MAXCOL) /                                          
     &45,41,37,33,30,26,23, 4, 4, 4, 4, 5, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,21),I=1,MAXCOL) /                                          
     &45,41,37,33,29,26,26, 4, 4, 4, 5, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,22),I=1,MAXCOL) /                                          
     &45,40,36,32,29,25,22, 4, 4, 5, 5, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,23),I=1,MAXCOL) /                                          
     &45,40,36,32,29,25,22, 4, 5, 5, 6, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,24),I=1,MAXCOL) /                                          
     &45,40,36,32,28,33, 5, 5, 5, 6, 6, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,25),I=1,MAXCOL) /                                          
     &45,40,36,32,28,25, 5, 5, 6, 7, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,26),I=1,MAXCOL) /                                          
     &45,40,36,31,27,24, 5, 6, 7, 7, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,27),I=1,MAXCOL) /                                          
     &45,40,35,31,35,23, 6, 7, 7, 9,10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,28),I=1,MAXCOL) /                                          
     &45,40,35,31,27, 6, 7, 7, 9,10,11, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,29),I=1,MAXCOL) /                                          
     &45,40,35,30,27, 6, 7, 8,10,12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,30),I=1,MAXCOL) /                                          
     &45,40,34,31,26,24,24,24,24,24,24,21,18,15,12,10, 8, 6, 0, 0, 0, 0/        
      DATA (MXRURC(I,31),I=1,MAXCOL) /                                          
     &45,39,34,33,33,33,32,30,29,27,26,20,16,13,10, 7, 0, 0, 0, 0, 0, 1/        
      DATA (MXRURC(I,32),I=1,MAXCOL) /                                          
     &44,40,40,37,35,33,31,30,28,26,25,18,14,10, 6, 1, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,33),I=1,MAXCOL) /                                          
     &44,42,40,37,35,33,31,29,27,25,24,18,12, 7, 1, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,34),I=1,MAXCOL) /                                          
     &45,42,39,37,34,32,30,29,27,24,23,16,10, 2, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,35),I=1,MAXCOL) /                                          
     &45,42,39,36,34,32,30,28,25,23,22,14, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,36),I=1,MAXCOL) /                                          
     &45,42,39,36,34,31,28,27,26,22,21,12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,37),I=1,MAXCOL) /                                          
     &45,42,39,36,33,30,28,26,24,21,19, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,38),I=1,MAXCOL) /                                          
     &45,42,39,36,32,30,27,25,22,20,18, 4, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,39),I=1,MAXCOL) /                                          
     &45,42,38,34,32,29,27,23,21,19,16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,40),I=1,MAXCOL) /                                          
     &45,41,37,33,29,25,22,18,14, 8, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,41),I=1,MAXCOL) /                                          
     &45,40,36,31,26,21,15, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,42),I=1,MAXCOL) /                                          
     &45,39,34,28,21,13, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,43),I=1,MAXCOL) /                                          
     &45,37,31,24,13, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,44),I=1,MAXCOL) /                                          
     &44,37,29,18, 2, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,45),I=1,MAXCOL) /                                          
     &45,36,26,12, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,46),I=1,MAXCOL) /                                          
     &45,34,16, 3, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,47),I=1,MAXCOL) /                                          
     &45,29, 8, 3, 0, 0, 2, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXRURC(I,48),I=1,MAXCOL) /                                          
     &43,23, 0, 3, 2, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,49),I=1,MAXCOL) /                                          
     &45,16, 0, 4, 0, 2, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,50),I=1,MAXCOL) /                                          
     &45, 0, 1, 0, 0, 0, 3, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,51),I=1,MAXCOL) /                                          
     &44, 0, 3, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,52),I=1,MAXCOL) /                                          
     &45, 3, 0, 2, 1, 0, 2, 0, 0, 0, 0, 1, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURC(I,53),I=1,MAXCOL) /                                          
     &40, 5, 2, 0, 0, 2, 0, 1, 0, 2, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0/        
      DATA (MXRURC(I,54),I=1,MAXCOL) /                                          
     &45, 0, 0, 0, 2, 0, 1, 0, 1, 0, 1, 0, 2, 0, 0, 0, 0, 1, 0, 0, 0, 0/        
      DATA (MXRURC(I,55),I=1,MAXCOL) /                                          
     &40, 0, 3, 2, 0, 3, 0, 0, 0, 2, 2, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1/        
      DATA (MXRURC(I,56),I=1,MAXCOL) /                                          
     &43, 8, 0, 0, 7, 5, 0, 1, 0, 2, 3, 0, 1, 2, 3, 2, 0, 0, 0, 1, 0, 0/

      END

      BLOCK DATA RURALD

      INCLUDE 'MAIN.INC'

      DATA (MXRURD(I,1),I=1,MAXCOL) /                                           
     &45,42,39,36,33,31,29,27,25,24,22,16,12, 8, 5, 5, 5, 1, 0, 0, 0, 0/        
      DATA (MXRURD(I,2),I=1,MAXCOL) /                                           
     &45,42,39,36,33,31,29,27,25,24,22,16,11, 8, 5, 5, 4, 1, 0, 0, 0, 0/        
      DATA (MXRURD(I,3),I=1,MAXCOL) /                                           
     &45,42,39,36,33,31,29,27,25,24,22,16,11, 8, 5, 5, 4, 1, 0, 0, 0, 0/        
      DATA (MXRURD(I,4),I=1,MAXCOL) /                                           
     &45,42,39,36,33,31,29,27,25,24,22,16,11, 8, 4, 5, 4, 1, 0, 0, 0, 0/        
      DATA (MXRURD(I,5),I=1,MAXCOL) /                                           
     &45,42,39,36,33,31,29,27,25,23,22,16,11, 8, 4, 5, 3, 1, 0, 0, 0, 0/        
      DATA (MXRURD(I,6),I=1,MAXCOL) /                                           
     &45,42,39,36,33,31,29,27,25,23,22,16,11, 7, 4, 5, 3, 1, 0, 0, 0, 0/        
      DATA (MXRURD(I,7),I=1,MAXCOL) /                                           
     &44,42,39,36,33,31,29,27,25,23,22,16,11, 7, 5, 5, 3, 1, 0, 0, 0, 0/        
      DATA (MXRURD(I,8),I=1,MAXCOL) /                                           
     &43,42,39,36,33,31,29,27,25,23,22,16,11, 7, 5, 5, 3, 1, 0, 0, 0, 0/        
      DATA (MXRURD(I,9),I=1,MAXCOL) /                                           
     &42,42,39,36,33,31,29,27,25,23,22,16,11, 7, 5, 6, 3, 1, 0, 0, 0, 0/        
      DATA (MXRURD(I,10),I=1,MAXCOL) /                                          
     &45,42,39,36,33,31,29,27,25,23,22,15,11, 7, 5, 6, 3, 1, 0, 0, 0, 0/        
      DATA (MXRURD(I,11),I=1,MAXCOL) /                                          
     &45,42,39,36,33,31,29,27,25,23,22,15,11, 7, 5, 6, 2, 1, 0, 0, 0, 0/        
      DATA (MXRURD(I,12),I=1,MAXCOL) /                                          
     &45,42,39,36,33,31,28,27,25,23,21,15,10, 6, 6, 4, 2, 0, 0, 0, 0, 0/        
      DATA (MXRURD(I,13),I=1,MAXCOL) /                                          
     &45,42,38,36,33,31,28,26,24,23,21,15,10, 5, 6, 3, 1, 0, 0, 0, 0, 0/        
      DATA (MXRURD(I,14),I=1,MAXCOL) /                                          
     &45,42,38,36,33,30,28,26,24,23,21,14, 9, 6, 7, 2, 1, 0, 0, 0, 0, 0/        
      DATA (MXRURD(I,15),I=1,MAXCOL) /                                          
     &45,42,38,35,33,30,28,26,24,22,21,14, 9, 6, 6, 1, 1, 0, 0, 0, 0, 0/        
      DATA (MXRURD(I,16),I=1,MAXCOL) /                                          
     &45,41,38,35,33,30,28,26,24,22,20,13, 8, 7, 4, 1, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURD(I,17),I=1,MAXCOL) /                                          
     &45,41,38,35,33,30,28,26,24,22,20,13, 7, 8, 2, 1, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURD(I,18),I=1,MAXCOL) /                                          
     &45,41,38,35,32,30,27,25,23,22,20,13, 7, 8, 1, 1, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURD(I,19),I=1,MAXCOL) /                                          
     &45,41,38,35,32,30,27,25,23,21,20,12, 7, 9, 1, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURD(I,20),I=1,MAXCOL) /                                          
     &45,41,38,35,32,30,27,25,23,21,19,12, 8, 4, 1, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURD(I,21),I=1,MAXCOL) /                                          
     &45,41,38,35,32,29,27,25,23,21,19,11, 8, 3, 1, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURD(I,22),I=1,MAXCOL) /                                          
     &45,41,38,35,32,29,27,25,22,21,19,11, 9, 2, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURD(I,23),I=1,MAXCOL) /                                          
     &45,41,38,35,32,29,27,24,22,20,18,10,10, 1, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURD(I,24),I=1,MAXCOL) /                                          
     &45,41,38,35,32,29,26,24,22,20,18, 9,11, 1, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURD(I,25),I=1,MAXCOL) /                                          
     &45,41,38,35,31,29,26,24,22,20,18, 9,11, 1, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURD(I,26),I=1,MAXCOL) /                                          
     &45,41,38,34,31,29,26,24,21,19,17, 9,12, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURD(I,27),I=1,MAXCOL) /                                          
     &45,41,38,34,31,28,26,23,21,19,17,10, 1, 0, 0, 0, 0, 0, 0, 0, 0, 2/        
      DATA (MXRURD(I,28),I=1,MAXCOL) /                                          
     &45,41,38,34,31,28,26,23,21,19,17,11, 1, 0, 0, 0, 0, 0, 3, 0, 2, 2/        
      DATA (MXRURD(I,29),I=1,MAXCOL) /                                          
     &45,41,37,34,31,28,25,23,21,18,16,11, 0, 0, 0, 0, 0, 0, 3, 0, 2, 2/        
      DATA (MXRURD(I,30),I=1,MAXCOL) /                                          
     &45,41,37,34,31,28,25,24,24,24,24,21,18,15,13,11,10, 8, 2, 0, 0, 0/        
      DATA (MXRURD(I,31),I=1,MAXCOL) /                                          
     &45,41,37,34,33,34,32,30,29,27,26,21,17,14,12, 9, 5, 0, 0, 0, 0, 0/        
      DATA (MXRURD(I,32),I=1,MAXCOL) /                                          
     &45,41,39,37,35,33,32,30,28,27,26,20,16,13, 9, 1, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURD(I,33),I=1,MAXCOL) /                                          
     &45,42,40,37,35,33,31,30,28,26,25,19,15,11, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURD(I,34),I=1,MAXCOL) /                                          
     &45,42,40,37,35,33,31,29,27,26,24,18,13, 8, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURD(I,35),I=1,MAXCOL) /                                          
     &45,42,39,37,35,33,31,29,27,25,24,17,12, 0, 1, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURD(I,36),I=1,MAXCOL) /                                          
     &45,42,39,37,34,32,30,28,26,25,23,16,10, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURD(I,37),I=1,MAXCOL) /                                          
     &45,42,39,36,34,32,30,28,26,24,23,15, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURD(I,38),I=1,MAXCOL) /                                          
     &45,42,39,36,34,32,29,27,25,23,22,14, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURD(I,39),I=1,MAXCOL) /                                          
     &45,42,39,36,34,31,29,27,25,23,20,12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURD(I,40),I=1,MAXCOL) /                                          
     &45,41,38,35,32,29,27,24,22,20,16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURD(I,41),I=1,MAXCOL) /                                          
     &45,41,37,34,30,27,24,21,17,13, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURD(I,42),I=1,MAXCOL) /                                          
     &45,41,36,32,28,26,21,16,10, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURD(I,43),I=1,MAXCOL) /                                          
     &45,40,35,31,26,21,15, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURD(I,44),I=1,MAXCOL) /                                          
     &45,40,34,28,23,17, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURD(I,45),I=1,MAXCOL) /                                          
     &45,39,33,26,19,11, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURD(I,46),I=1,MAXCOL) /                                          
     &45,38,29,21, 9, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURD(I,47),I=1,MAXCOL) /                                          
     &45,36,27,11, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXRURD(I,48),I=1,MAXCOL) /                                          
     &45,34,22, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURD(I,49),I=1,MAXCOL) /                                          
     &45,32,14, 3, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURD(I,50),I=1,MAXCOL) /                                          
     &45,28, 1, 3, 2, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURD(I,51),I=1,MAXCOL) /                                          
     &44,27, 2, 4, 0, 1, 0, 1, 0, 2, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0/        
      DATA (MXRURD(I,52),I=1,MAXCOL) /                                          
     &44,23, 4, 0, 0, 2, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURD(I,53),I=1,MAXCOL) /                                          
     &45,20, 1, 0, 3, 0, 2, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0/        
      DATA (MXRURD(I,54),I=1,MAXCOL) /                                          
     &45, 0, 1, 3, 0, 2, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0/        
      DATA (MXRURD(I,55),I=1,MAXCOL) /                                          
     &45, 0, 2, 0, 2, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0/
      DATA (MXRURD(I,56),I=1,MAXCOL) /                                          
     &41, 7, 0, 0, 4, 0, 0, 2, 1, 0, 0, 0, 1, 2, 0, 0, 1, 2, 0, 0, 0, 0/        

      END

      BLOCK DATA RURALE

      INCLUDE 'MAIN.INC'

      DATA (MXRURE(I,1),I=1,MAXCOL) /                                           
     &45,42,39,36,34,32,30,28,26,25,23,18,14,10, 8, 6, 4, 3, 3, 1, 1, 0/        
      DATA (MXRURE(I,2),I=1,MAXCOL) /                                           
     &45,42,39,36,34,32,30,28,26,25,23,18,14,10, 8, 6, 4, 3, 3, 1, 1, 0/        
      DATA (MXRURE(I,3),I=1,MAXCOL) /                                           
     &45,42,39,36,34,32,30,28,26,25,23,18,13,10, 8, 6, 4, 3, 3, 1, 1, 0/        
      DATA (MXRURE(I,4),I=1,MAXCOL) /                                           
     &45,42,39,36,34,32,30,28,26,25,23,18,13,10, 8, 6, 4, 3, 3, 1, 1, 0/        
      DATA (MXRURE(I,5),I=1,MAXCOL) /                                           
     &45,42,39,36,34,32,30,28,26,25,23,18,13,10, 8, 6, 3, 3, 3, 1, 1, 0/        
      DATA (MXRURE(I,6),I=1,MAXCOL) /                                           
     &45,42,39,36,34,32,30,28,26,25,23,17,13,10, 8, 5, 3, 3, 3, 1, 0, 0/        
      DATA (MXRURE(I,7),I=1,MAXCOL) /                                           
     &44,42,39,36,34,32,30,28,26,25,23,17,13,10, 8, 5, 3, 3, 3, 1, 0, 0/        
      DATA (MXRURE(I,8),I=1,MAXCOL) /                                           
     &43,42,39,36,34,32,30,28,26,25,23,17,13,10, 7, 5, 3, 3, 3, 1, 0, 0/        
      DATA (MXRURE(I,9),I=1,MAXCOL) /                                           
     &42,42,39,36,34,32,30,28,26,25,23,17,13,10, 7, 5, 3, 3, 2, 1, 0, 0/        
      DATA (MXRURE(I,10),I=1,MAXCOL) /                                          
     &42,42,39,36,34,32,30,28,26,25,23,17,13,10, 7, 5, 3, 3, 2, 1, 0, 0/        
      DATA (MXRURE(I,11),I=1,MAXCOL) /                                          
     &45,42,39,36,34,32,30,28,26,25,23,17,13,10, 7, 5, 3, 3, 2, 1, 0, 0/        
      DATA (MXRURE(I,12),I=1,MAXCOL) /                                          
     &45,42,39,36,34,32,30,28,26,24,23,17,13, 9, 7, 4, 4, 3, 1, 1, 0, 0/        
      DATA (MXRURE(I,13),I=1,MAXCOL) /                                          
     &45,42,39,36,34,31,29,28,26,24,23,17,12, 9, 6, 4, 4, 4, 1, 0, 0, 0/        
      DATA (MXRURE(I,14),I=1,MAXCOL) /                                          
     &45,42,39,36,34,31,29,27,26,24,23,17,12, 9, 6, 4, 4, 3, 1, 0, 0, 0/        
      DATA (MXRURE(I,15),I=1,MAXCOL) /                                          
     &45,42,39,36,34,31,29,27,26,24,22,16,12, 8, 5, 4, 4, 2, 1, 0, 0, 0/        
      DATA (MXRURE(I,16),I=1,MAXCOL) /                                          
     &45,42,39,36,33,31,29,27,25,24,22,16,12, 8, 5, 4, 4, 1, 0, 0, 0, 0/        
      DATA (MXRURE(I,17),I=1,MAXCOL) /                                          
     &45,42,39,36,33,31,29,27,25,24,22,16,11, 8, 4, 4, 4, 1, 0, 0, 0, 0/        
      DATA (MXRURE(I,18),I=1,MAXCOL) /                                          
     &45,42,39,36,33,31,29,27,25,23,22,16,11, 7, 5, 4, 5, 1, 0, 0, 0, 0/        
      DATA (MXRURE(I,19),I=1,MAXCOL) /                                          
     &45,42,39,36,33,31,29,27,25,23,22,15,11, 6, 5, 5, 4, 1, 0, 0, 0, 0/        
      DATA (MXRURE(I,20),I=1,MAXCOL) /                                          
     &45,42,39,36,33,31,29,27,25,23,22,15,10, 6, 5, 5, 2, 0, 0, 0, 0, 0/        
      DATA (MXRURE(I,21),I=1,MAXCOL) /                                          
     &45,42,39,36,33,31,29,27,25,23,21,15,10, 5, 5, 5, 1, 0, 0, 0, 0, 0/        
      DATA (MXRURE(I,22),I=1,MAXCOL) /                                          
     &45,42,39,36,33,31,28,26,25,23,21,15, 9, 5, 5, 6, 1, 0, 0, 0, 0, 0/        
      DATA (MXRURE(I,23),I=1,MAXCOL) /                                          
     &45,42,38,36,33,31,28,26,24,23,21,14, 9, 6, 6, 3, 1, 0, 0, 0, 0, 0/        
      DATA (MXRURE(I,24),I=1,MAXCOL) /                                          
     &45,42,38,35,33,30,28,26,24,22,21,14, 8, 5, 6, 2, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURE(I,25),I=1,MAXCOL) /                                          
     &45,42,38,35,33,30,28,26,24,22,21,14, 8, 6, 6, 1, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURE(I,26),I=1,MAXCOL) /                                          
     &45,42,38,35,33,30,28,26,24,22,20,13, 7, 6, 7, 1, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURE(I,27),I=1,MAXCOL) /                                          
     &45,42,38,35,33,30,28,26,24,22,20,13, 7, 6, 2, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURE(I,28),I=1,MAXCOL) /                                          
     &45,42,38,35,33,30,28,26,24,22,20,13, 6, 7, 1, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURE(I,29),I=1,MAXCOL) /                                          
     &45,42,38,35,32,30,28,25,23,22,20,12, 7, 7, 1, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURE(I,30),I=1,MAXCOL) /                                          
     &45,42,38,35,32,30,27,25,24,24,24,21,18,15,13,12,10, 8, 0, 0, 0, 0/        
      DATA (MXRURE(I,31),I=1,MAXCOL) /                                          
     &45,41,38,35,33,33,32,30,29,27,26,21,17,15,12,10, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURE(I,32),I=1,MAXCOL) /                                          
     &45,41,39,37,35,33,32,30,29,27,26,21,17,14,11, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURE(I,33),I=1,MAXCOL) /                                          
     &45,42,40,37,35,33,31,30,28,27,25,20,15,12, 5, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURE(I,34),I=1,MAXCOL) /                                          
     &45,42,40,37,35,33,31,29,28,26,25,19,15,11, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURE(I,35),I=1,MAXCOL) /                                          
     &45,42,40,37,35,33,31,29,28,26,25,19,14, 8, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURE(I,36),I=1,MAXCOL) /                                          
     &45,42,39,37,35,33,31,29,27,26,24,18,13, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURE(I,37),I=1,MAXCOL) /                                          
     &45,42,39,37,35,32,30,29,27,25,24,17,12, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURE(I,38),I=1,MAXCOL) /                                          
     &45,42,39,37,34,32,30,28,26,25,23,16,10, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURE(I,39),I=1,MAXCOL) /                                          
     &45,42,39,36,34,32,30,28,26,24,23,16, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURE(I,40),I=1,MAXCOL) /                                          
     &45,42,39,36,33,31,28,26,24,22,20,11, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURE(I,41),I=1,MAXCOL) /                                          
     &45,41,38,35,32,29,27,24,22,19,17, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURE(I,42),I=1,MAXCOL) /                                          
     &45,41,38,34,31,28,25,22,19,15,13, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURE(I,43),I=1,MAXCOL) /                                          
     &45,41,37,33,30,25,23,19,15,11, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURE(I,44),I=1,MAXCOL) /                                          
     &45,41,36,32,28,24,20,16,11, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURE(I,45),I=1,MAXCOL) /                                          
     &45,40,36,31,27,22,16,11, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURE(I,46),I=1,MAXCOL) /                                          
     &45,40,34,29,22,16, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURE(I,47),I=1,MAXCOL) /                                          
     &45,38,32,24,16, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURE(I,48),I=1,MAXCOL) /                                          
     &45,38,30,20, 8, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURE(I,49),I=1,MAXCOL) /                                          
     &44,36,26,14, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURE(I,50),I=1,MAXCOL) /                                          
     &45,36,23, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURE(I,51),I=1,MAXCOL) /                                          
     &45,33,17, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXRURE(I,52),I=1,MAXCOL) /                                          
     &45,30,11, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURE(I,53),I=1,MAXCOL) /                                          
     &44,31, 1, 0, 0, 2, 1, 2, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0/        
      DATA (MXRURE(I,54),I=1,MAXCOL) /                                          
     &45,29, 1, 0, 2, 0, 1, 0, 2, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0/
      DATA (MXRURE(I,55),I=1,MAXCOL) /                                          
     &45,26, 2, 2, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0/        
      DATA (MXRURE(I,56),I=1,MAXCOL) /                                          
     &43, 2, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 0/        

      END

      BLOCK DATA RURALF

      INCLUDE 'MAIN.INC'

      DATA (MXRURF(I,1),I=1,MAXCOL) /                                           
     &45,42,39,37,35,32,31,29,27,26,25,19,15,12,10, 8, 7, 5, 3, 2, 2, 2/        
      DATA (MXRURF(I,2),I=1,MAXCOL) /                                           
     &45,42,39,37,35,32,31,29,27,26,25,19,15,12,10, 8, 7, 5, 3, 2, 2, 2/        
      DATA (MXRURF(I,3),I=1,MAXCOL) /                                           
     &44,42,39,37,34,32,31,29,27,26,24,19,15,12,10, 8, 7, 5, 3, 2, 2, 2/        
      DATA (MXRURF(I,4),I=1,MAXCOL) /                                           
     &44,42,39,37,34,32,31,29,27,26,24,19,15,12,10, 8, 7, 4, 3, 2, 2, 1/        
      DATA (MXRURF(I,5),I=1,MAXCOL) /                                           
     &43,42,39,37,34,32,31,29,27,26,24,19,15,12,10, 8, 7, 4, 3, 2, 2, 1/        
      DATA (MXRURF(I,6),I=1,MAXCOL) /                                           
     &45,42,39,37,34,32,31,29,27,26,24,19,15,12,10, 8, 7, 4, 2, 2, 2, 1/        
      DATA (MXRURF(I,7),I=1,MAXCOL) /                                           
     &45,42,39,37,34,32,31,29,27,26,24,19,15,12,10, 8, 7, 7, 2, 2, 2, 1/        
      DATA (MXRURF(I,8),I=1,MAXCOL) /                                           
     &45,42,39,37,34,32,31,29,27,26,24,19,15,12,10, 8, 7, 4, 2, 2, 2, 1/        
      DATA (MXRURF(I,9),I=1,MAXCOL) /                                           
     &45,42,39,37,34,32,30,29,27,26,24,19,15,12,10, 8, 7, 4, 2, 2, 2, 1/        
      DATA (MXRURF(I,10),I=1,MAXCOL) /                                          
     &45,42,39,37,34,32,30,29,27,26,24,19,15,12,10, 8, 7, 4, 2, 2, 2, 1/        
      DATA (MXRURF(I,11),I=1,MAXCOL) /                                          
     &45,42,39,37,34,32,30,29,27,26,24,19,15,12,10, 8, 6, 4, 2, 2, 2, 1/        
      DATA (MXRURF(I,12),I=1,MAXCOL) /                                          
     &45,42,39,37,34,32,30,29,27,26,24,19,15,12,10, 8, 6, 4, 2, 2, 2, 1/        
      DATA (MXRURF(I,13),I=1,MAXCOL) /                                          
     &45,42,39,37,34,32,30,29,27,26,24,19,15,12, 9, 8, 6, 3, 2, 2, 1, 0/        
      DATA (MXRURF(I,14),I=1,MAXCOL) /                                          
     &45,42,39,37,34,32,30,29,27,25,24,19,15,12, 9, 7, 6, 3, 2, 3, 1, 0/        
      DATA (MXRURF(I,15),I=1,MAXCOL) /                                          
     &45,42,39,37,34,32,30,28,27,25,24,18,14,11, 9, 7, 5, 3, 3, 2, 1, 0/        
      DATA (MXRURF(I,16),I=1,MAXCOL) /                                          
     &45,42,39,37,34,32,30,28,27,25,24,18,14,11, 9, 7, 5, 2, 3, 1, 0, 0/        
      DATA (MXRURF(I,17),I=1,MAXCOL) /                                          
     &45,42,39,37,34,32,30,28,27,25,21,18,14,11, 9, 7, 5, 2, 3, 1, 0, 0/        
      DATA (MXRURF(I,18),I=1,MAXCOL) /                                          
     &45,42,39,36,34,32,30,28,27,25,24,18,14,11, 8, 6, 5, 3, 3, 1, 0, 0/        
      DATA (MXRURF(I,19),I=1,MAXCOL) /                                          
     &45,42,39,36,34,32,30,28,27,25,24,18,14,11, 8, 6, 4, 3, 2, 0, 0, 0/        
      DATA (MXRURF(I,20),I=1,MAXCOL) /                                          
     &45,42,39,36,34,32,30,28,25,25,23,18,14,10, 8, 6, 4, 3, 1, 0, 0, 0/        
      DATA (MXRURF(I,21),I=1,MAXCOL) /                                          
     &45,42,39,36,34,32,30,28,26,25,23,18,13,10,12, 5, 3, 4, 1, 0, 0, 0/        
      DATA (MXRURF(I,22),I=1,MAXCOL) /                                          
     &45,42,39,36,34,32,30,25,26,25,23,17,13,10, 7, 5, 3, 4, 1, 0, 0, 0/        
      DATA (MXRURF(I,23),I=1,MAXCOL) /                                          
     &45,42,39,36,34,32,30,28,26,25,23,17,13,10, 7, 5, 3, 4, 0, 0, 0, 0/        
      DATA (MXRURF(I,24),I=1,MAXCOL) /                                          
     &45,42,39,36,34,32,27,28,26,24,23,17,13, 9, 7, 4, 3, 3, 0, 0, 0, 0/        
      DATA (MXRURF(I,25),I=1,MAXCOL) /                                          
     &45,42,39,36,34,32,30,28,26,24,23,17,13, 9, 7, 4, 4, 1, 0, 0, 0, 0/        
      DATA (MXRURF(I,26),I=1,MAXCOL) /                                          
     &45,42,39,36,34,32,29,28,26,24,23,17,12, 9, 6, 3, 4, 1, 0, 0, 0, 2/        
      DATA (MXRURF(I,27),I=1,MAXCOL) /                                          
     &45,42,39,36,34,31,29,28,26,24,23,17,12, 9, 6, 3, 4, 1, 0, 0, 2, 2/        
      DATA (MXRURF(I,28),I=1,MAXCOL) /                                          
     &45,42,39,36,34,31,29,27,26,24,23,16,12, 9, 5, 4, 5, 0, 0, 3, 2, 2/        
      DATA (MXRURF(I,29),I=1,MAXCOL) /                                          
     &45,42,39,36,32,31,29,27,26,24,23,16,12, 8, 5, 4, 5, 0, 0, 3, 2, 1/        
      DATA (MXRURF(I,30),I=1,MAXCOL) /                                          
     &45,42,39,36,33,31,29,27,25,24,24,21,18,15,13,12,10, 8, 0, 0, 0, 0/        
      DATA (MXRURF(I,31),I=1,MAXCOL) /                                          
     &45,42,39,36,37,33,32,30,29,27,26,21,18,15,13,11, 9, 0, 0, 0, 0, 0/        
      DATA (MXRURF(I,32),I=1,MAXCOL) /                                          
     &45,42,39,37,35,33,32,30,29,27,26,21,17,14,12,10, 6, 0, 0, 0, 0, 0/        
      DATA (MXRURF(I,33),I=1,MAXCOL) /                                          
     &45,42,40,37,35,33,32,30,28,27,26,21,17,14,10, 7, 3, 0, 0, 0, 0, 0/        
      DATA (MXRURF(I,34),I=1,MAXCOL) /                                          
     &45,42,40,37,35,33,31,30,28,27,26,20,16,13, 9, 6, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURF(I,35),I=1,MAXCOL) /                                          
     &45,42,40,37,35,33,31,30,28,27,25,20,16,12, 8, 4, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURF(I,36),I=1,MAXCOL) /                                          
     &45,42,40,37,35,33,31,29,28,26,25,19,15,12, 7, 2, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURF(I,37),I=1,MAXCOL) /                                          
     &45,42,40,37,35,33,31,29,28,26,25,18,15,11, 6, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURF(I,38),I=1,MAXCOL) /                                          
     &45,42,39,37,35,33,31,29,27,26,25,19,14,10, 5, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURF(I,39),I=1,MAXCOL) /                                          
     &45,42,39,37,35,33,31,29,27,26,24,18,14, 9, 3, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURF(I,40),I=1,MAXCOL) /                                          
     &45,42,39,36,34,32,30,28,25,24,23,16,11, 3, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURF(I,41),I=1,MAXCOL) /                                          
     &45,42,39,36,33,31,29,27,25,23,21,14, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURF(I,42),I=1,MAXCOL) /                                          
     &45,42,38,36,33,30,28,26,24,22,20,11, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURF(I,43),I=1,MAXCOL) /                                          
     &45,41,38,35,32,29,27,25,22,20,18, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURF(I,44),I=1,MAXCOL) /                                          
     &45,41,38,35,31,29,26,23,21,18,16, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURF(I,45),I=1,MAXCOL) /                                          
     &45,41,37,34,31,28,25,22,19,17,14, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURF(I,46),I=1,MAXCOL) /                                          
     &45,41,37,33,29,26,22,19,15,11, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURF(I,47),I=1,MAXCOL) /                                          
     &45,40,36,32,27,23,19,14, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURF(I,48),I=1,MAXCOL) /                                          
     &45,40,35,30,25,20,14, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURF(I,49),I=1,MAXCOL) /                                          
     &45,37,34,28,22,16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURF(I,50),I=1,MAXCOL) /                                          
     &45,39,33,26,18, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURF(I,51),I=1,MAXCOL) /                                          
     &45,38,31,23,14, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXRURF(I,52),I=1,MAXCOL) /                                          
     &45,37,30,21, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURF(I,53),I=1,MAXCOL) /                                          
     &44,37,28,17, 2, 2, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURF(I,54),I=1,MAXCOL) /                                          
     &45,35,25,12, 3, 2, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURF(I,55),I=1,MAXCOL) /                                          
     &45,35,22, 3, 2, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXRURF(I,56),I=1,MAXCOL) /                                          
     &45,31, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0/        

      END

      BLOCK DATA URBANA

      INCLUDE 'MAIN.INC'

      DATA (MXURBA(I,1),I=1,MAXCOL) /                                           
     &45,38,31,30, 6, 6, 6, 6, 6, 4, 3, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBA(I,2),I=1,MAXCOL) /                                           
     &45,38,31,32, 6, 6, 6, 6, 6, 4, 2, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBA(I,3),I=1,MAXCOL) /                                           
     &45,38,31,33, 6, 6, 6, 6, 5, 3, 2, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBA(I,4),I=1,MAXCOL) /                                           
     &45,38,31,34, 6, 6, 6, 6, 5, 3, 2, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBA(I,5),I=1,MAXCOL) /                                           
     &45,38,30,35, 6, 6, 6, 6, 5, 3, 2, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBA(I,6),I=1,MAXCOL) /                                           
     &45,38,30,25, 6, 6, 6, 6, 4, 3, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBA(I,7),I=1,MAXCOL) /                                           
     &45,37,30,25, 6, 6, 6, 6, 4, 3, 3, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0/        
      DATA (MXURBA(I,8),I=1,MAXCOL) /                                           
     &45,37,30,25, 6, 7, 7, 7, 4, 2, 3, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0/        
      DATA (MXURBA(I,9),I=1,MAXCOL) /                                           
     &45,37,30,24, 7, 7, 7, 7, 4, 2, 2, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0/        
      DATA (MXURBA(I,10),I=1,MAXCOL) /                                          
     &45,37,30, 7, 7, 7, 7, 5, 3, 2, 2, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0/        
      DATA (MXURBA(I,11),I=1,MAXCOL) /                                          
     &45,37,31, 7, 7, 7, 7, 5, 3, 2, 2, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBA(I,12),I=1,MAXCOL) /                                          
     &44,36,36, 7, 7, 8, 8, 4, 2, 2, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBA(I,13),I=1,MAXCOL) /                                          
     &44,35,29, 8, 8, 8, 4, 2, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBA(I,14),I=1,MAXCOL) /                                          
     &44,34, 8, 9, 9, 8, 3, 2, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBA(I,15),I=1,MAXCOL) /                                          
     &44,38, 9,10,10, 4, 2, 2, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBA(I,16),I=1,MAXCOL) /                                          
     &43,43,10,11, 9, 3, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBA(I,17),I=1,MAXCOL) /                                          
     &43,10,11,12, 4, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBA(I,18),I=1,MAXCOL) /                                          
     &40,11,12,14, 3, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBA(I,19),I=1,MAXCOL) /                                          
     &41,13,14, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1/        
      DATA (MXURBA(I,20),I=1,MAXCOL) /                                          
     &45,14,16, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1/        
      DATA (MXURBA(I,21),I=1,MAXCOL) /                                          
     &44,16,18, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBA(I,22),I=1,MAXCOL) /                                          
     &15,18, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0/        
      DATA (MXURBA(I,23),I=1,MAXCOL) /                                          
     &17,20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0/        
      DATA (MXURBA(I,24),I=1,MAXCOL) /                                          
     &19,22, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0/        
      DATA (MXURBA(I,25),I=1,MAXCOL) /                                          
     &21,25, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0/        
      DATA (MXURBA(I,26),I=1,MAXCOL) /                                          
     &23, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0/        
      DATA (MXURBA(I,27),I=1,MAXCOL) /                                          
     &26, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0/        
      DATA (MXURBA(I,28),I=1,MAXCOL) /                                          
     &29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0/        
      DATA (MXURBA(I,29),I=1,MAXCOL) /                                          
     &32, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0/        
      DATA (MXURBA(I,30),I=1,MAXCOL) /                                          
     &39,24,24,34,29,26,24,24,24,24,24,21,16,13, 9, 6, 3, 1, 0, 0, 0, 0/        
      DATA (MXURBA(I,31),I=1,MAXCOL) /                                          
     &45,43,34,38,34,33,32,30,28,26,25,18,12, 5, 1, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXURBA(I,32),I=1,MAXCOL) /                                          
     &45,43,40,37,35,32,30,28,26,24,22,14, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXURBA(I,33),I=1,MAXCOL) /                                          
     &45,42,39,36,34,31,29,26,24,22,19, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBA(I,34),I=1,MAXCOL) /                                          
     &45,42,39,36,32,29,28,24,22,19,16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBA(I,35),I=1,MAXCOL) /                                          
     &45,42,38,35,31,28,25,22,19,16,12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBA(I,36),I=1,MAXCOL) /                                          
     &45,41,38,33,30,27,23,20,16,12, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBA(I,37),I=1,MAXCOL) /                                          
     &45,41,38,34,29,25,21,17,12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1/        
      DATA (MXURBA(I,38),I=1,MAXCOL) /                                          
     &45,41,36,33,28,23,19,12, 5, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBA(I,39),I=1,MAXCOL) /                                          
     &45,40,36,32,26,21,14, 5, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0/        
      DATA (MXURBA(I,40),I=1,MAXCOL) /                                          
     &45,39,30,25,15, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBA(I,41),I=1,MAXCOL) /                                          
     &45,36,25,11, 2, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBA(I,42),I=1,MAXCOL) /                                          
     &45,33,14, 0, 3, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXURBA(I,43),I=1,MAXCOL) /                                          
     &45,34,11, 3, 2, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0/        
      DATA (MXURBA(I,44),I=1,MAXCOL) /                                          
     &45,20, 5, 1, 2, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXURBA(I,45),I=1,MAXCOL) /                                          
     &45,23, 0, 5, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBA(I,46),I=1,MAXCOL) /                                          
     &40,10, 0, 0, 2, 2, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBA(I,47),I=1,MAXCOL) /                                          
     &45, 0, 5, 0, 0, 1, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBA(I,48),I=1,MAXCOL) /                                          
     &41, 3, 0, 0, 2, 2, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBA(I,49),I=1,MAXCOL) /                                          
     &38,16, 1, 4, 1, 1, 3, 1, 0, 0, 0, 0, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBA(I,50),I=1,MAXCOL) /                                          
     &35, 0, 1, 0, 3, 3, 1, 0, 0, 0, 0, 4, 3, 1, 0, 0, 0, 1, 1, 0, 1, 0/        
      DATA (MXURBA(I,51),I=1,MAXCOL) /                                          
     &32, 0,10, 5, 7, 0, 0, 1, 0, 1, 2, 5, 2, 0, 0, 0, 0, 0, 0, 0, 1, 0/        
      DATA (MXURBA(I,52),I=1,MAXCOL) /                                          
     &45,17,12, 7, 0, 1, 0, 0, 2, 4, 5, 4, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0/        
      DATA (MXURBA(I,53),I=1,MAXCOL) /                                          
     &19,14, 8, 0, 2, 0, 1, 3, 5, 6, 8, 3, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1/        
      DATA (MXURBA(I,54),I=1,MAXCOL) /                                          
     &21,10, 0, 2, 0, 2, 4, 6, 8, 8, 7, 2, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0/        
      DATA (MXURBA(I,55),I=1,MAXCOL) /                                          
     &18, 5, 1, 0, 2, 5, 7, 9, 8, 7, 6, 1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0/        
      DATA (MXURBA(I,56),I=1,MAXCOL) /                                          
     & 2, 7,12,15,17,11,10, 6, 3, 0, 0, 0, 1, 0, 2, 3, 0, 0, 1, 1, 0, 0/        

      END

      BLOCK DATA URBANC

      INCLUDE 'MAIN.INC'

      DATA (MXURBC(I,1),I=1,MAXCOL) /                                           
     &45,40,36,32,28,25,21,18,15,13,14, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBC(I,2),I=1,MAXCOL) /                                           
     &45,40,36,32,28,24,21,18,14,14,14, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBC(I,3),I=1,MAXCOL) /                                           
     &45,40,36,32,28,24,21,18,13,14,14, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBC(I,4),I=1,MAXCOL) /                                           
     &45,40,36,32,28,24,21,18,13,14,14, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBC(I,5),I=1,MAXCOL) /                                           
     &45,40,36,32,28,24,21,17,14,14,14, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBC(I,6),I=1,MAXCOL) /                                           
     &45,40,36,31,28,24,20,17,14,15,13, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBC(I,7),I=1,MAXCOL) /                                           
     &45,40,36,31,28,24,20,17,14,15,12, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBC(I,8),I=1,MAXCOL) /                                           
     &45,40,35,31,27,24,20,16,14,15,12, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBC(I,9),I=1,MAXCOL) /                                           
     &45,40,35,31,27,23,20,16,15,15,12, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBC(I,10),I=1,MAXCOL) /                                          
     &45,40,35,31,27,23,19,15,15,16,10, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBC(I,11),I=1,MAXCOL) /                                          
     &45,40,35,31,27,23,19,15,15,16,10, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBC(I,12),I=1,MAXCOL) /                                          
     &45,40,35,31,26,22,18,16,16,13, 6, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBC(I,13),I=1,MAXCOL) /                                          
     &45,40,35,30,26,21,16,17,16, 8, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBC(I,14),I=1,MAXCOL) /                                          
     &45,39,34,29,24,19,17,18,13, 5, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0/        
      DATA (MXURBC(I,15),I=1,MAXCOL) /                                          
     &45,39,34,29,23,18,18,19, 7, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBC(I,16),I=1,MAXCOL) /                                          
     &45,39,34,28,22,18,20,11, 2, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBC(I,17),I=1,MAXCOL) /                                          
     &45,39,33,27,20,20,21, 7, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5/        
      DATA (MXURBC(I,18),I=1,MAXCOL) /                                          
     &45,38,32,26,20,21,14, 4, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 4/        
      DATA (MXURBC(I,19),I=1,MAXCOL) /                                          
     &45,38,31,24,21,23, 7, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 4, 4/        
      DATA (MXURBC(I,20),I=1,MAXCOL) /                                          
     &45,38,31,22,22,24, 3, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 6, 5, 4, 4, 3/        
      DATA (MXURBC(I,21),I=1,MAXCOL) /                                          
     &45,37,30,22,24,14, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 0, 0, 4, 0, 3/        
      DATA (MXURBC(I,22),I=1,MAXCOL) /                                          
     &45,37,29,23,26, 5, 0, 1, 0, 0, 0, 0, 0, 0, 0, 7, 6, 5, 4, 0, 3, 0/        
      DATA (MXURBC(I,23),I=1,MAXCOL) /                                          
     &45,36,27,25,27, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 0, 4, 0, 3, 0/        
      DATA (MXURBC(I,24),I=1,MAXCOL) /                                          
     &45,36,25,26,29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 6, 5, 4, 0, 3, 0, 0/        
      DATA (MXURBC(I,25),I=1,MAXCOL) /                                          
     &45,35,25,28, 1, 0, 0, 0, 0, 0, 0, 0, 0, 8, 7, 6, 5, 4, 3, 0, 0, 0/        
      DATA (MXURBC(I,26),I=1,MAXCOL) /                                          
     &45,35,27,30, 0, 0, 0, 0, 0, 0, 0, 0, 9, 7, 6, 5, 0, 0, 3, 0, 0, 0/        
      DATA (MXURBC(I,27),I=1,MAXCOL) /                                          
     &44,32,28,31, 0, 0, 0, 0, 0, 0, 0, 0, 8, 7, 6, 5, 4, 1, 0, 0, 0, 0/        
      DATA (MXURBC(I,28),I=1,MAXCOL) /                                          
     &44,32,30,33, 0, 0, 0, 0, 0, 0, 0,10, 8, 6, 5, 0, 4, 3, 0, 0, 0, 0/        
      DATA (MXURBC(I,29),I=1,MAXCOL) /                                          
     &43,30,31, 0, 0, 0, 0, 0, 0, 0, 0, 9, 7, 6, 5, 4, 0, 3, 0, 2, 0, 0/        
      DATA (MXURBC(I,30),I=1,MAXCOL) /                                          
     &42,31,35,38,33,24,24,24,26,24,25,21,17,14,11, 7, 6, 1, 0, 0, 0, 0/        
      DATA (MXURBC(I,31),I=1,MAXCOL) /                                          
     &41,35,39,33,34,33,32,30,28,27,25,19,14,10, 5, 1, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBC(I,32),I=1,MAXCOL) /                                          
     &40,40,40,37,35,33,31,29,27,25,24,17,10, 2, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBC(I,33),I=1,MAXCOL) /                                          
     &45,42,40,37,34,32,29,28,26,24,22,14, 5, 0, 1, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBC(I,34),I=1,MAXCOL) /                                          
     &45,42,39,36,34,31,28,26,24,22,20,10, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBC(I,35),I=1,MAXCOL) /                                          
     &45,42,39,36,33,30,27,25,19,20,18, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBC(I,36),I=1,MAXCOL) /                                          
     &45,42,38,35,33,29,26,23,21,18,15, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBC(I,37),I=1,MAXCOL) /                                          
     &45,41,38,34,31,28,25,22,18,15,12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBC(I,38),I=1,MAXCOL) /                                          
     &45,41,38,34,30,27,24,20,15,12, 7, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBC(I,39),I=1,MAXCOL) /                                          
     &45,41,37,33,29,25,21,18,13, 7, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBC(I,40),I=1,MAXCOL) /                                          
     &45,40,34,29,23,15, 6, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBC(I,41),I=1,MAXCOL) /                                          
     &45,38,33,22,12, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1/        
      DATA (MXURBC(I,42),I=1,MAXCOL) /                                          
     &45,37,27,15, 3, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBC(I,43),I=1,MAXCOL) /                                          
     &45,35,20, 5, 3, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBC(I,44),I=1,MAXCOL) /                                          
     &45,30,10, 2, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBC(I,45),I=1,MAXCOL) /                                          
     &44,31, 0, 2, 3, 0, 2, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBC(I,46),I=1,MAXCOL) /                                          
     &45,19, 0, 3, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBC(I,47),I=1,MAXCOL) /                                          
     &45,12, 5, 1, 1, 1, 0, 0, 3, 1, 1, 2, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0/        
      DATA (MXURBC(I,48),I=1,MAXCOL) /                                          
     &41, 0, 2, 2, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0/        
      DATA (MXURBC(I,49),I=1,MAXCOL) /                                          
     &43, 5, 2, 1, 3, 0, 0, 1, 0, 1, 1, 2, 0, 0, 0, 2, 0, 1, 0, 0, 0, 0/        
      DATA (MXURBC(I,50),I=1,MAXCOL) /                                          
     &45, 7, 0, 3, 0, 2, 0, 0, 1, 0, 0, 0, 0, 0, 2, 2, 1, 0, 0, 0, 0, 0/        
      DATA (MXURBC(I,51),I=1,MAXCOL) /                                          
     &45, 0, 1, 1, 4, 0, 2, 0, 0, 2, 3, 0, 0, 3, 3, 2, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBC(I,52),I=1,MAXCOL) /                                          
     &40, 0,10, 5, 0, 1, 0, 2, 3, 3, 2, 0, 2, 4, 2, 0, 0, 0, 0, 0, 1, 2/        
      DATA (MXURBC(I,53),I=1,MAXCOL) /                                          
     &38,11, 6, 0, 0, 0, 3, 5, 3, 0, 0, 1, 4, 3, 2, 0, 0, 0, 0, 0, 1, 0/        
      DATA (MXURBC(I,54),I=1,MAXCOL) /                                          
     &36, 7, 1, 0, 1, 4, 6, 3, 1, 0, 0, 2, 5, 3, 1, 0, 0, 0, 1, 0, 0, 2/        
      DATA (MXURBC(I,55),I=1,MAXCOL) /                                          
     &33, 0, 1, 1,10, 7, 6, 1, 0, 1, 0, 4, 4, 2, 0, 0, 0, 1, 0, 0, 1, 0/        
      DATA (MXURBC(I,56),I=1,MAXCOL) /                                          
     &20,15, 9, 1, 0, 0, 2, 3, 5, 6, 8, 4, 0, 0, 0, 0, 0, 1, 2, 2, 0, 0/        

      END

      BLOCK DATA URBAND

      INCLUDE 'MAIN.INC'

      DATA (MXURBD(I,1),I=1,MAXCOL) /                                           
     &45,41,37,34,31,28,25,23,21,18,16, 5, 5, 2, 1, 1, 1, 0, 0, 0, 0, 0/        
      DATA (MXURBD(I,2),I=1,MAXCOL) /                                           
     &44,41,37,34,31,28,25,23,21,18,16, 5, 5, 2, 1, 1, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBD(I,3),I=1,MAXCOL) /                                           
     &42,41,37,34,31,28,25,23,20,18,16, 5, 5, 2, 1, 1, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBD(I,4),I=1,MAXCOL) /                                           
     &45,41,37,34,31,28,25,23,20,18,16, 5, 5, 2, 1, 1, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBD(I,5),I=1,MAXCOL) /                                           
     &45,41,37,34,31,28,25,23,20,18,15, 5, 5, 2, 1, 1, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBD(I,6),I=1,MAXCOL) /                                           
     &45,41,37,34,31,28,25,22,20,17,15, 5, 5, 2, 1, 1, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBD(I,7),I=1,MAXCOL) /                                           
     &45,41,37,34,30,28,25,22,20,17,15, 5, 5, 2, 1, 1, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBD(I,8),I=1,MAXCOL) /                                           
     &45,41,37,34,30,27,25,22,20,17,17, 5, 5, 1, 1, 1, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBD(I,9),I=1,MAXCOL) /                                           
     &45,41,37,34,30,27,25,22,19,17,19, 5, 4, 1, 1, 1, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBD(I,10),I=1,MAXCOL) /                                          
     &45,41,37,34,30,27,25,22,19,17,21, 5, 4, 1, 1, 1, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBD(I,11),I=1,MAXCOL) /                                          
     &45,41,37,33,30,27,24,22,19,17, 5, 6, 4, 1, 1, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBD(I,12),I=1,MAXCOL) /                                          
     &45,41,37,33,30,27,24,21,18,21, 6, 6, 3, 1, 1, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBD(I,13),I=1,MAXCOL) /                                          
     &45,41,37,33,30,26,23,20,18, 6, 6, 7, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBD(I,14),I=1,MAXCOL) /                                          
     &45,41,36,33,29,26,23,19, 6, 6, 7, 7, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBD(I,15),I=1,MAXCOL) /                                          
     &45,40,36,32,29,25,22,22, 7, 7, 7, 5, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBD(I,16),I=1,MAXCOL) /                                          
     &45,40,36,32,28,25,21, 7, 7, 7, 8, 4, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBD(I,17),I=1,MAXCOL) /                                          
     &45,40,36,32,28,24,24, 7, 8, 8, 8, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBD(I,18),I=1,MAXCOL) /                                          
     &45,40,36,31,27,23, 8, 8, 8, 9, 9, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBD(I,19),I=1,MAXCOL) /                                          
     &45,40,35,31,27,24, 8, 9, 9, 9,10, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBD(I,20),I=1,MAXCOL) /                                          
     &45,40,35,31,26,31, 9, 9,10,10,11, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBD(I,21),I=1,MAXCOL) /                                          
     &45,40,35,30,25, 9,10,10,11,11, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1/        
      DATA (MXURBD(I,22),I=1,MAXCOL) /                                          
     &45,40,34,30,28,10,10,11,12,13, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBD(I,23),I=1,MAXCOL) /                                          
     &45,39,34,29,34,10,11,12,13, 7, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBD(I,24),I=1,MAXCOL) /                                          
     &45,39,34,28,10,11,12,13,15, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBD(I,25),I=1,MAXCOL) /                                          
     &45,39,33,29,11,12,13,15, 7, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBD(I,26),I=1,MAXCOL) /                                          
     &45,39,32,35,12,13,15,17, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBD(I,27),I=1,MAXCOL) /                                          
     &45,38,32,12,13,15,16, 8, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBD(I,28),I=1,MAXCOL) /                                          
     &45,38,32,13,14,16,18, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBD(I,29),I=1,MAXCOL) /                                          
     &45,38,33,14,16,18,21, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0/        
      DATA (MXURBD(I,30),I=1,MAXCOL) /                                          
     &44,37,31,25,25,24,24,24,24,24,27,21,17,15,12,10, 7, 5, 0, 0, 0, 0/        
      DATA (MXURBD(I,31),I=1,MAXCOL) /                                          
     &44,36,33,33,33,33,32,30,28,27,26,20,16,12, 9, 6, 1, 0, 0, 0, 0, 0/        
      DATA (MXURBD(I,32),I=1,MAXCOL) /                                          
     &43,41,40,37,35,33,31,29,27,26,25,18,13, 9, 3, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBD(I,33),I=1,MAXCOL) /                                          
     &44,42,40,37,35,33,30,28,26,25,23,16,10, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBD(I,34),I=1,MAXCOL) /                                          
     &45,42,39,37,34,32,30,26,25,24,22,14, 6, 1, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBD(I,35),I=1,MAXCOL) /                                          
     &45,42,39,36,34,31,29,27,24,22,20,12, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBD(I,36),I=1,MAXCOL) /                                          
     &45,42,39,36,33,31,28,25,23,21,19, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBD(I,37),I=1,MAXCOL) /                                          
     &45,42,39,36,33,30,27,24,22,19,17, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBD(I,38),I=1,MAXCOL) /                                          
     &45,42,38,34,32,29,25,23,20,18,14, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBD(I,39),I=1,MAXCOL) /                                          
     &45,41,38,35,31,28,24,22,19,16,12, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBD(I,40),I=1,MAXCOL) /                                          
     &45,41,36,32,27,22,17,12, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBD(I,41),I=1,MAXCOL) /                                          
     &45,39,32,28,21,14, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBD(I,42),I=1,MAXCOL) /                                          
     &45,38,31,23,13, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBD(I,43),I=1,MAXCOL) /                                          
     &45,37,27,15, 3, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBD(I,44),I=1,MAXCOL) /                                          
     &45,35,23, 5, 2, 0, 3, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXURBD(I,45),I=1,MAXCOL) /                                          
     &44,33,17, 3, 2, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBD(I,46),I=1,MAXCOL) /                                          
     &45,30, 0, 0, 2, 0, 2, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXURBD(I,47),I=1,MAXCOL) /                                          
     &45,22, 6, 0, 5, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBD(I,48),I=1,MAXCOL) /                                          
     &43,14, 7, 0, 0, 2, 0, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBD(I,49),I=1,MAXCOL) /                                          
     &45, 6, 1, 4, 0, 0, 3, 1, 0, 0, 2, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0/        
      DATA (MXURBD(I,50),I=1,MAXCOL) /                                          
     &44, 0, 1, 0, 3, 0, 0, 0, 2, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 0/        
      DATA (MXURBD(I,51),I=1,MAXCOL) /                                          
     &41, 0, 1, 3, 0, 0, 0, 1, 0, 1, 1, 0, 2, 0, 0, 0, 0, 2, 0, 0, 0, 0/        
      DATA (MXURBD(I,52),I=1,MAXCOL) /                                          
     &45, 2, 2, 0, 0, 1, 0, 0, 2, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0/        
      DATA (MXURBD(I,53),I=1,MAXCOL) /                                          
     &45, 3, 7, 0, 2, 0, 2, 2, 0, 0, 1, 3, 0, 0, 0, 1, 3, 1, 0, 0, 0, 0/        
      DATA (MXURBD(I,54),I=1,MAXCOL) /                                          
     &44, 9, 4, 2, 0, 3, 5, 0, 1, 0, 0, 2, 0, 0, 1, 3, 2, 0, 0, 0, 0, 0/        
      DATA (MXURBD(I,55),I=1,MAXCOL) /                                          
     &43, 5, 1, 0, 8, 5, 0, 1, 0, 1, 2, 1, 0, 0, 2, 3, 2, 0, 0, 0, 0, 0/        
      DATA (MXURBD(I,56),I=1,MAXCOL) /                                          
     &36, 9, 3, 0, 1, 7, 6, 7, 3, 1, 0, 2, 4, 4, 2, 1, 0, 0, 0, 0, 0, 0/        

      END

      BLOCK DATA URBANF

      INCLUDE 'MAIN.INC'

      DATA (MXURBF(I,1),I=1,MAXCOL) /                                           
     &45,41,38,35,33,30,28,26,24,22,20,14, 8, 7, 3, 1, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBF(I,2),I=1,MAXCOL) /                                           
     &45,41,38,35,33,30,28,26,24,22,20,14, 8, 7, 3, 1, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBF(I,3),I=1,MAXCOL) /                                           
     &45,41,38,35,33,30,28,26,24,22,20,13, 8, 7, 3, 1, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBF(I,4),I=1,MAXCOL) /                                           
     &45,41,38,35,33,30,28,26,24,22,20,13, 8, 7, 2, 1, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBF(I,5),I=1,MAXCOL) /                                           
     &45,41,38,35,33,30,28,26,24,22,20,13, 7, 7, 2, 1, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBF(I,6),I=1,MAXCOL) /                                           
     &45,41,38,35,33,30,28,26,23,22,20,13, 7, 7, 2, 1, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBF(I,7),I=1,MAXCOL) /                                           
     &45,41,38,35,32,30,28,25,23,22,20,13, 7, 7, 2, 1, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBF(I,8),I=1,MAXCOL) /                                           
     &45,41,38,35,32,30,28,25,23,22,20,13, 7, 7, 2, 1, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBF(I,9),I=1,MAXCOL) /                                           
     &45,41,38,35,32,30,28,25,23,21,20,13, 7, 6, 1, 1, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBF(I,10),I=1,MAXCOL) /                                          
     &45,41,38,35,32,30,28,25,23,21,20,13, 7, 6, 1, 1, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBF(I,11),I=1,MAXCOL) /                                          
     &45,41,38,35,32,30,27,25,23,21,20,13, 7, 5, 1, 1, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBF(I,12),I=1,MAXCOL) /                                          
     &45,41,38,35,32,30,27,25,23,21,19,12, 8, 4, 1, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBF(I,13),I=1,MAXCOL) /                                          
     &45,41,38,35,32,29,27,25,23,21,19,11, 9, 2, 1, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBF(I,14),I=1,MAXCOL) /                                          
     &45,41,38,35,32,29,27,24,22,20,18,10, 9, 1, 1, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBF(I,15),I=1,MAXCOL) /                                          
     &45,41,38,35,32,29,26,24,22,20,18, 9, 7, 1, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBF(I,16),I=1,MAXCOL) /                                          
     &45,41,38,34,31,29,26,24,21,19,17, 9, 5, 1, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBF(I,17),I=1,MAXCOL) /                                          
     &45,41,38,34,31,28,26,23,21,19,17,10, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBF(I,18),I=1,MAXCOL) /                                          
     &45,41,37,34,31,28,26,23,21,19,16,11, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBF(I,19),I=1,MAXCOL) /                                          
     &45,41,37,34,31,28,25,23,20,18,16,12, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBF(I,20),I=1,MAXCOL) /                                          
     &45,41,37,34,31,28,25,22,20,17,15,13, 1, 0, 0, 0, 0, 0, 0, 0, 0, 3/        
      DATA (MXURBF(I,21),I=1,MAXCOL) /                                          
     &45,41,37,34,31,27,24,22,19,17,14,13, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBF(I,22),I=1,MAXCOL) /                                          
     &45,41,37,33,30,27,24,21,19,16,14, 5, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0/        
      DATA (MXURBF(I,23),I=1,MAXCOL) /                                          
     &45,41,37,33,30,27,24,21,18,16,13, 3, 0, 0, 0, 0, 0, 0, 0, 3, 3, 0/        
      DATA (MXURBF(I,24),I=1,MAXCOL) /                                          
     &45,41,37,33,30,27,23,21,18,15,12, 2, 0, 0, 0, 0, 0, 0, 0, 3, 0, 2/        
      DATA (MXURBF(I,25),I=1,MAXCOL) /                                          
     &45,41,37,33,30,26,23,20,17,14,13, 1, 0, 0, 0, 0, 0, 4, 3, 3, 0, 2/        
      DATA (MXURBF(I,26),I=1,MAXCOL) /                                          
     &45,41,37,33,29,26,22,19,17,13,14, 1, 0, 0, 0, 0, 0, 4, 3, 3, 2, 2/        
      DATA (MXURBF(I,27),I=1,MAXCOL) /                                          
     &45,41,36,33,29,26,22,19,16,14,15, 0, 0, 0, 0, 5, 0, 0, 3, 0, 2, 2/        
      DATA (MXURBF(I,28),I=1,MAXCOL) /                                          
     &45,40,36,32,29,25,22,18,15,15,16, 0, 0, 0, 6, 5, 4, 3, 3, 2, 2, 0/        
      DATA (MXURBF(I,29),I=1,MAXCOL) /                                          
     &45,40,36,32,29,25,21,18,15,16,17, 0, 0, 7, 0, 5, 4, 3, 0, 2, 2, 0/        
      DATA (MXURBF(I,30),I=1,MAXCOL) /                                          
     &45,40,36,32,28,24,24,24,24,24,24,21,18,15,13,11, 9, 7, 2, 0, 0, 0/        
      DATA (MXURBF(I,31),I=1,MAXCOL) /                                          
     &45,40,36,33,33,34,32,30,29,27,26,20,17,14,11, 8, 6, 0, 0, 0, 0, 0/        
      DATA (MXURBF(I,32),I=1,MAXCOL) /                                          
     &45,40,39,37,35,33,32,30,28,27,25,20,15,12, 8, 3, 1, 0, 0, 0, 0, 0/        
      DATA (MXURBF(I,33),I=1,MAXCOL) /                                          
     &45,42,40,37,35,33,31,29,28,26,24,18,13, 9, 3, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBF(I,34),I=1,MAXCOL) /                                          
     &45,42,40,37,35,33,31,29,27,26,23,17,11, 5, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBF(I,35),I=1,MAXCOL) /                                          
     &45,42,39,37,34,32,30,28,27,24,23,15, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBF(I,36),I=1,MAXCOL) /                                          
     &45,42,39,36,34,32,30,28,25,24,22,14, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBF(I,37),I=1,MAXCOL) /                                          
     &45,42,39,36,34,31,29,27,24,23,21,11, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBF(I,38),I=1,MAXCOL) /                                          
     &45,42,39,36,33,31,29,26,23,22,20, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBF(I,39),I=1,MAXCOL) /                                          
     &45,42,39,36,33,31,28,25,19,21,18, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBF(I,40),I=1,MAXCOL) /                                          
     &45,41,38,34,30,27,24,21,17,13, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBF(I,41),I=1,MAXCOL) /                                          
     &45,41,36,32,27,23,18,13, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBF(I,42),I=1,MAXCOL) /                                          
     &45,40,35,29,24,18, 8, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBF(I,43),I=1,MAXCOL) /                                          
     &45,39,32,26,18, 6, 2, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBF(I,44),I=1,MAXCOL) /                                          
     &45,38,31,22,11, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBF(I,45),I=1,MAXCOL) /                                          
     &45,37,28,15, 2, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBF(I,46),I=1,MAXCOL) /                                          
     &45,34,21, 3, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MXURBF(I,47),I=1,MAXCOL) /                                          
     &44,32, 6, 2, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBF(I,48),I=1,MAXCOL) /                                          
     &45,27, 0, 0, 4, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBF(I,49),I=1,MAXCOL) /                                          
     &43,21, 2, 1, 3, 0, 0, 1, 0, 0, 2, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0/        
      DATA (MXURBF(I,50),I=1,MAXCOL) /                                          
     &44, 7, 1, 2, 0, 2, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/        
      DATA (MXURBF(I,51),I=1,MAXCOL) /                                          
     &45, 0, 1, 0, 0, 0, 3, 0, 0, 0, 2, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0/        
      DATA (MXURBF(I,52),I=1,MAXCOL) /                                          
     &45, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0/
      DATA (MXURBF(I,53),I=1,MAXCOL) /                                          
     &45, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0/        
      DATA (MXURBF(I,54),I=1,MAXCOL) /                                          
     &45, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 1, 0, 1, 0/        
      DATA (MXURBF(I,55),I=1,MAXCOL) /                                          
     &42, 8, 1, 5, 0, 0, 1, 1, 0, 1, 0, 0, 0, 2, 1, 0, 0, 1, 2, 0, 1, 0/        
      DATA (MXURBF(I,56),I=1,MAXCOL) /                                          
     &45, 1, 7, 4, 0, 0, 2, 4, 4, 0, 0, 3, 1, 0, 0, 0, 2, 2, 0, 0, 0, 0/

      END

      SUBROUTINE VDP

c --- ISC2ST     Version:  1.0     Level:  930215                   VDP
c                J. Scire, SRC
c
c --- MODIFIED   March 9, 1994
c                Changed procedure for estimating the deposition layer
c                resistance.
c                D.T. Bailey, USEPA
c
c --- PURPOSE:  Compute particle deposition velocities for each size
c               category of a size distribution.
c
c --- INPUTS:
c     Common block /METVAR/ variables:
c               Z0M - real       - Surface roughness length (m)
c             USTAR - real       - Friction velocity (m/s)
c                EL - real       - Monin-Obukhov length (m)
c     Common block /CALCS3/ variables:
c               NPD - integer    - Number of particle size categories
c             PDIAM - real array - Mean diameter (microns) of each
c                                  particle size category
c               PHI - real array - Mass fraction in each size category
c             PDENS - real       - Particle density (g/cm**3)
c                SC - real array - Schmidt number
c             VGRAV - real array - Gravitational settling velocity (m/s)
c             TSTOP - real array - Stopping time (s)
c     Common block /SOURC4/ variables:
c            VAIRMS - real       - Viscosity of air (m**2/s)
c             ZRDEP - real       - Reference height (m)
c            VDPHOR - real       - Phoretic effects term (m/s)
c
c --- OUTPUT:
c     Common block /CALCS3/ variables:
c              VDEP - real array - Deposition velocity (m/s) for each
c                                  particle size category
c
c --- VDP called by:  PCALC, VCALC, ACALC
c --- VDP calls:      none
c----------------------------------------------------------------------
c
      INCLUDE 'MAIN.INC'
c
      real rd(npdmax)
c
CXXX      IDBG=iounit
cxxx      ZRDEP = ZREF
c
c ***
      if(DEBUG)then
         write(IDBG,*)
         write(IDBG,*)'SUBR. VDP -- Inputs'
         write(IDBG,*)'USTAR (m/s)     = ',ustar
         write(IDBG,*)'MONIN-EL (m)    = ',el
         write(IDBG,*)'Z0M (m)         = ',z0m
         write(IDBG,*)'VDPHOR (m/s)    = ',vdphor
         write(IDBG,*)'NPD             = ',npd
         write(IDBG,*)'PDIAM (um)      = ',(pdiam(n),n=1,npd)
         write(IDBG,*)'FRACT           = ',(phi(n),n=1,npd)
         write(IDBG,*)'PDENS (g/cm**3) = ',(pdens(n),n=1,npd)
         write(IDBG,*)'SC              = ',(sc(n),n=1,npd)
         write(IDBG,*)'VGRAV (m/s)     = ',(vgrav(n),n=1,npd)
         write(IDBG,*)'TSTOP (s)       = ',(tstop(n),n=1,npd)
         write(IDBG,*)'VAIRMS (m**2/s) = ',vairms
         write(IDBG,*)'ZRDEP (m)       = ',zrdep
         write(IDBG,*)'VDPHOR (m/s)    = ',vdphor
      endif
c ***
c
c --- Use minimum value of USTAR to avoid numerical problems
c --- when USTAR near zero
      ustarr=AMAX1(ustar,1.e-9)
c
c --- Minimum absolute value of Monin-Obukhov length is 1.0 m
      if(el.GE.0.0)then
c ---    stable
         ell=AMAX1(el,1.0)
      else
c ---    unstable
         ell=AMIN1(el,-1.0)
      endif
c
c --- Compute stability-dependent psi function (heat)
      elabs=ABS(ell)
      if(ell.LT.0.0.and.elabs.LT.9.e9)then
c ---    Unstable
         psih=2.*ALOG(0.5*(1.+SQRT(1.-16.*zrdep/ell)))
      else if(elabs.GE.9.e9)then
c ---    Neutral
         psih=0.0
      else
c ---    Stable
         psih=-5.*zrdep/ell
      endif
c
c --- Calculate atmospheric resistance (s/m)
c --- VK is the von Karman constant, set as parameter in MAIN1.INC
      ra=(ALOG(zrdep/z0m)-psih)/(vk*ustarr)
c
      t1=ustarr*ustarr/vairms
c
c ***
      if(DEBUG)then
         write(IDBG,*)
         write(IDBG,*)'USTARR (m/s)    = ',ustarr
         write(IDBG,*)'ELL (m)         = ',ell
         write(IDBG,*)'PSIH            = ',psih
      endif
c ***

c --- LOOP OVER SIZE INTERVALS
      do 10 i=1,npd

C        compute the Stokes Number   St = (Vg/g) * (Ustar^2/Nu)
         st=tstop(i)*t1

c ---    Compute inertial impaction term
         xinert = 10 ** (-3/St)                                           DTB94068
c
c ---    Adjust (raise) the Schmidt Number to the 2/3rd's power.
         Schmidt = sc(i) ** (-.667)                                       DTB94068

c ---    Compute the deposition layer resistance (s/m)
         rd(i)=1.0 / (ustarr * (Schmidt + xinert))                        DTB94068
c
c ---    Deposition velocity for this current interval
         vdep(i)=1.0/(ra+rd(i)+ra*rd(i)*vgrav(i))+vgrav(i)+vdphor
10    continue
c ***
      if(DEBUG)then
         write(IDBG,*)
         write(IDBG,*)'RA (s/m)    = ',ra
         write(IDBG,*)'RD (s/m)    = ',(rd(n),n=1,npd)
         write(IDBG,*)'VDEP (m/s)  = ',(vdep(n),n=1,npd)
      endif
c ***
c
      return
      end

      SUBROUTINE SIGZD(XARG,sz)
c-----------------------------------------------------------------------------
C
C                 SIGZD Module of ISC2 Model
C
C        PURPOSE: Calculates Sigma-z Values From Dispersion Curves
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C        MODIFIED BY D. Strimaitis, SRC:
C
C !!!  Special form for deposition section  !!!
C  --  MAIN commons are replaced with deposition common, and computed
C  --  sigma-z is returned through the argument list.
C
C        DATE:    February 15, 1993
C
C        INPUTS:  Downwind Distance
C                 Stability Class
C                 Rural or Urban Dispersion Option
C
C        OUTPUTS: Vertical Dispersion Coefficient, SZ
C
C        CALLED FROM:   F2INT
C
C
C ----   ADAPTED FROM:
C
C                 SIGZ Module of ISC2 Model
C
C        PURPOSE: Calculates Sigma-z Values From Dispersion Curves
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C***********************************************************************

C     Variable Declarations
c     INCLUDE 'MAIN1.INC'
c     INCLUDE 'MAIN2.INC'
c     INCLUDE 'MAIN3.INC'
      INCLUDE 'DEPVAR.INC'

C     Variable Initializations
c     MODNAM = 'SIGZD'

C     Convert Distance to km
      XKM = XARG * 0.001

C     Determine Sigma-z Based on RURAL/URBAN, Stability Class, and Distance.
C     Stability Classes are Checked in the Order 4, 5, 6, 1, 2, 3
C     For Optimization, Since Neutral and Stable are Generally the Most
C     Frequent Classes.

      IF (RURAL) THEN
C        Retrieve Coefficients, A and B                     ---   CALL SZCOEF
         CALL SZCOEF(XKM,A,B,XMIN,XMAX)
         SZ = A*XKM**B
      ELSE IF (URBAN) THEN
         IF (KST .EQ. 4) THEN
            SZ = 140.*XKM/SQRT(1.+0.3*XKM)
         ELSE IF (KST .GE. 5) THEN
            SZ = 80.*XKM/SQRT(1.+1.5*XKM)
         ELSE IF (KST .LE. 2) THEN
            SZ = 240.*XKM*SQRT(1.+XKM)
         ELSE IF (KST .EQ. 3) THEN
            SZ = 200.*XKM
         END IF
      END IF

      RETURN
      END

      SUBROUTINE OBUKHOV
C************************************************************************
C                OBUKHOV Module of ISC Model - Long Term
C
C        PURPOSE: Calculates Monin-Obukhov Length
C
C        PROGRAMMER: DESMOND BAILEY
C
C        DATE:       March 4, 1994
C
C        INPUTS:
C                    Z0M    Surface Roughness Length
C                    KST    P-G Stability Class
C
C        OUTPUTS:    EL     M-O Length (m)
C
C        CALLED FROM:    METSET
C*************************************************************************

C     Variable Declarations
C      INCLUDE 'MAIN1LT.INC'
      INCLUDE 'MAIN.INC'

c      PARAMETER (G = 9.80655, VK = 0.4, CP = 1004., PRESMB = 1013.,
c     &           ELMIN = 2., ANEM = 10.)
C      PARAMETER (PRESMB = 1013., ELMIN = 2.)

C      COMMON UREF, TA, KST, USTAR, EL, Z0M, RHO

      DIMENSION A(6), B(6)

      DATA A /-0.0875, -0.03849, -0.00807, 0.0,  0.00807, 0.03849/
      DATA B /-0.1029, -0.17140, -0.30490, 0.0, -0.3049, -0.1714/

      PRESMB = 1013.
      ELMIN = 2.
      ANEM = ZREF

      Z0 = Z0M
      IF( Z0 .LT. 0.001 ) Z0 = 0.001

       IF(KST .EQ .4) THEN
          EL = 9000.0
       ELSE

         EL = 1.0/(A(KST)*Z0**B(KST))

       END IF

      RETURN
      END


      SUBROUTINE U_STAR
c --- compute nighttime u* using the Weil - Brower method; or
c --- compute daytime u* using the Holtslag - van Ulden method.

      INCLUDE 'MAIN.INC'


c --- compute air density in kg/m**3 -- rho = p/(r * t) --

      PRESMB = 1013.

c     RHO = P/R*T
c     R   = 287 m**2/(deg k * sec**2)
c     1/R = (100. kg/(m*sec**2) per mb)/(287 m**2/(deg k * sec**2))
c     1/R = 0.3484321

      RHO  =  0.3484321 * PRESMB / TA

      IF (KST .GT. 4) THEN

            CALL USLNITE
      ELSE
            CALL USLDAY

      ENDIF

      RETURN
      END

C     ********************************************************************
      SUBROUTINE USLDAY

c     Purpose:   Calculates u* for neutral and unstable conditions
C                using Wang and Chen's technique
c
c     Reference: Wang and Chen, 1980:  Estimations of heat and momentum
c                fluxes near the ground.  Proc. 2nd Joint Conf. on
c                Applications of Air Poll. Meteorol., AMS, 764-769.
c
c     Arguments passed:     none

      INCLUDE 'MAIN.INC'

      ANEM = ZREF
c
c --- Calculate neutral friction velocity
C *** Beware of possible devide by zero  ***
      USTARN = VK*UREF / ALOG (ANEM/Z0M)
c
      IF ( KST .EQ. 4) THEN
         USTAR = USTARN
      else

C     Calculate friction velocity for unstable conditions.

         CALL WC

      endif
      return
      end

C     ********************************************************************

      SUBROUTINE WC
c
c     Purpose: Implementation of Wang and Chen's technique to
c              parameterize ustar under convective conditions


      INCLUDE 'MAIN.INC'

      DIMENSION SHFF(6)
      DATA SHFF/260., 210., 130., 0.0, -10., -20./

      SHF = SHFF(KST)

      ANEM = ZREF

c   Arguments passed:
c       ustar  real      calculated friction velocity
c       UREF   real      measured wind speed, m/s
c       anem   real      anemometer height, m
c       Z0M    real      roughness length, m
c       VK     real      von karman constant
c       g      real      gravitational acceleration, m/sec**2
c       shf    real      sensible heat flux, w/m**2
c       cp     real      specific heat at constant pressure, j/k-kg
c       rho    real      density of air, kg/m**3
c       TA     real      air temperature, k
c

        ratio  =  Z0M/anem
        ratln  =  ALOG(ratio)
        d1     =  0.107

        IF (ratio .LE. 0.01) d1 = 0.128 + 0.005*ratln

        d2     =  1.95+32.6*(ratio)**0.45
        x1     =  shf/(rho*cp)
        y1     =  VK * g * anem / TA
        z1     =  (-ratln/(VK*UREF))**3
        ustar  =  VK*UREF*(1.+d1*alog(1.+d2*x1*y1*z1))/(-ratln)

        return
        end

C     *******************************************************************

      SUBROUTINE USLNITE
c
c     Purpose: This routine calculates ustar for the stable cases (l > 0)
c              using the Weil-Brower technique (1983)
c
c     Arguments passed:

c       anem   real      anemometer height
c       elmin  real      the lower limit on L when stable
c
c
      INCLUDE 'MAIN.INC'

c      real ROOTS(3)
c
c --- Assign constants: von karman constant, specific heat, and grav.
c
C     Assume clear sky conditions

      FRCC = 0.0

c --- const = maximum product of ustar and thetastar
c --- bbeta is used for profile relatationships in stable conditions
c
      const = 0.05
      bbeta = 4.7

      ANEM = ZREF
c
      cdn    =  VK/ALOG(anem/Z0M)
      ths1   =  0.09*(1.-0.5*frcc**2)
      ths2   =  (TA*cdn*UREF**2)/(4.0*bbeta*anem*g)
      thstar =  AMIN1(ths1,ths2)
      unot   =  SQRT((bbeta*anem*g*thstar)/(TA))
c
c --- Since thstar is taken as the smaller of ths1 and ths2,
c --- (2.*unot/(sqrt(cdn)*UREF))**2 can only be as big as 1.0
c
      dum    = 1.-(2.*unot/(SQRT(cdn)*UREF))**2
c
c --- Prevent round-off error
c
      IF (dum.lt.0.) dum = 0.0

      USTAR  =  (cdn*UREF/2.)*(1.+SQRT(dum))
c
c --- Special attention required for the high ustar cases
c
C     if(USTAR*thstar .GT. const) then
C         aa = -cdn*UREF
C         b  = 0.0
C         c  = beta*anem*g*const*cdn/TA
C
C         call cubic(aa,b,c, ROOTS, nroots)
C         call pickus(ROOTS, nroots,USTAR,cdn,UREF)
C
C         thstar = const/USTAR
C     endif
C
C     if(EL .GE. elmin) goto 100
C      EL   = elmin
C     USTAR = SQRT(ELMIN*VK*g*thstar/TA)
c
100   continue

      return
      end

      SUBROUTINE FINDMX(XNORM)
C=======================================================================
C                FINDMX Module of the SCREEN2 Dispersion Model
C
C   Purpose:   Find maximum wind direction for area source calculations.
C
C   Input:     Normalized downwind distance.
C
C   Output:    Maximum and minimum wind directions through which the model
C              will search for the maximum concentration or deposition.
C
C   Assumptions:  This routine finds the maximum and minimum wind directions
C              to search through from look-up tables as a function of
C              stability class, aspect ratio of the area source (length/width),
C              and a normalized downwind distance.  The normalized distances
C              are measured from the center of the area source and are
C              normalized relative to one-half the maximum dimension of the
C              area.
C
C   Programmer:  Roger Brode
C                Pacific Environmental Services
C
C   Revision history:
C                <none>
C
C   Reference(s):
C
C-----------------------------------------------------------------------
C
C---- Variable declarations
C
      INCLUDE 'MAIN.INC'
C
C---- Data initializations
C

C     Find array indexes for aspect ratio (NDXCOL) and distance (NDXROW)
      IF (FSTCAL) THEN
C        Only need to determine aspect ratio index on first call
         NDXCOL = -1
         I  =  1
         DO WHILE( NDXCOL .LT. 0  .AND.  I .LE. MAXCOL )
            IF ( ASPECT .LT. TBLASP(I) ) THEN
               NDXCOL = I - 1
            END IF
            I = I + 1
         END DO
         FSTCAL = .FALSE.
         IF (NDXCOL .EQ. -1) NDXCOL = MAXCOL
      END IF

      NDXROW = -1
      J = 1
      DO WHILE( NDXROW .LT. 0  .AND.  J .LE. MAXROW )
         IF ( XNORM .LT. TBLDIS(J) ) THEN
            NDXROW = J - 1
         END IF
         J = J + 1
      END DO
      IF (NDXROW .EQ. -1) NDXROW = MAXROW

C     NOTE: An index less than 0 indicates that values exceed the max.

      IF (NDXCOL .GT. 0 .AND. NDXCOL .LT. MAXCOL .AND.
     &    NDXROW .GT. 0 .AND. NDXROW .LT. MAXROW) THEN

         IF (RURAL) THEN
            IF (KST .EQ. 6) THEN
               MINDIR = MIN0( MXRURF(NDXCOL,NDXROW),
     &                        MXRURF(NDXCOL+1,NDXROW),
     &                        MXRURF(NDXCOL,NDXROW+1),
     &                        MXRURF(NDXCOL+1,NDXROW+1) )
               MAXDIR = MAX0( MXRURF(NDXCOL,NDXROW),
     &                        MXRURF(NDXCOL+1,NDXROW),
     &                        MXRURF(NDXCOL,NDXROW+1),
     &                        MXRURF(NDXCOL+1,NDXROW+1) )
            ELSE IF (KST .EQ. 5) THEN
               MINDIR = MIN0( MXRURE(NDXCOL,NDXROW),
     &                        MXRURE(NDXCOL+1,NDXROW),
     &                        MXRURE(NDXCOL,NDXROW+1),
     &                        MXRURE(NDXCOL+1,NDXROW+1) )
               MAXDIR = MAX0( MXRURE(NDXCOL,NDXROW),
     &                        MXRURE(NDXCOL+1,NDXROW),
     &                        MXRURE(NDXCOL,NDXROW+1),
     &                        MXRURE(NDXCOL+1,NDXROW+1) )
            ELSE IF (KST .EQ. 4) THEN
               MINDIR = MIN0( MXRURD(NDXCOL,NDXROW),
     &                        MXRURD(NDXCOL+1,NDXROW),
     &                        MXRURD(NDXCOL,NDXROW+1),
     &                        MXRURD(NDXCOL+1,NDXROW+1) )
               MAXDIR = MAX0( MXRURD(NDXCOL,NDXROW),
     &                        MXRURD(NDXCOL+1,NDXROW),
     &                        MXRURD(NDXCOL,NDXROW+1),
     &                        MXRURD(NDXCOL+1,NDXROW+1) )
            ELSE IF (KST .EQ. 3) THEN
               MINDIR = MIN0( MXRURC(NDXCOL,NDXROW),
     &                        MXRURC(NDXCOL+1,NDXROW),
     &                        MXRURC(NDXCOL,NDXROW+1),
     &                        MXRURC(NDXCOL+1,NDXROW+1) )
               MAXDIR = MAX0( MXRURC(NDXCOL,NDXROW),
     &                        MXRURC(NDXCOL+1,NDXROW),
     &                        MXRURC(NDXCOL,NDXROW+1),
     &                        MXRURC(NDXCOL+1,NDXROW+1) )
            ELSE IF (KST .EQ. 2) THEN
               MINDIR = MIN0( MXRURB(NDXCOL,NDXROW),
     &                        MXRURB(NDXCOL+1,NDXROW),
     &                        MXRURB(NDXCOL,NDXROW+1),
     &                        MXRURB(NDXCOL+1,NDXROW+1) )
               MAXDIR = MAX0( MXRURB(NDXCOL,NDXROW),
     &                        MXRURB(NDXCOL+1,NDXROW),
     &                        MXRURB(NDXCOL,NDXROW+1),
     &                        MXRURB(NDXCOL+1,NDXROW+1) )
            ELSE IF (KST .EQ. 1) THEN
               MINDIR = MIN0( MXRURA(NDXCOL,NDXROW),
     &                        MXRURA(NDXCOL+1,NDXROW),
     &                        MXRURA(NDXCOL,NDXROW+1),
     &                        MXRURA(NDXCOL+1,NDXROW+1) )
               MAXDIR = MAX0( MXRURA(NDXCOL,NDXROW),
     &                        MXRURA(NDXCOL+1,NDXROW),
     &                        MXRURA(NDXCOL,NDXROW+1),
     &                        MXRURA(NDXCOL+1,NDXROW+1) )
            END IF
         ELSE IF (URBAN) THEN
            IF (KST .EQ. 6 .OR. KST .EQ. 5) THEN
               MINDIR = MIN0( MXURBF(NDXCOL,NDXROW),
     &                        MXURBF(NDXCOL+1,NDXROW),
     &                        MXURBF(NDXCOL,NDXROW+1),
     &                        MXURBF(NDXCOL+1,NDXROW+1) )
               MAXDIR = MAX0( MXURBF(NDXCOL,NDXROW),
     &                        MXURBF(NDXCOL+1,NDXROW),
     &                        MXURBF(NDXCOL,NDXROW+1),
     &                        MXURBF(NDXCOL+1,NDXROW+1) )
            ELSE IF (KST .EQ. 4) THEN
               MINDIR = MIN0( MXURBD(NDXCOL,NDXROW),
     &                        MXURBD(NDXCOL+1,NDXROW),
     &                        MXURBD(NDXCOL,NDXROW+1),
     &                        MXURBD(NDXCOL+1,NDXROW+1) )
               MAXDIR = MAX0( MXURBD(NDXCOL,NDXROW),
     &                        MXURBD(NDXCOL+1,NDXROW),
     &                        MXURBD(NDXCOL,NDXROW+1),
     &                        MXURBD(NDXCOL+1,NDXROW+1) )
            ELSE IF (KST .EQ. 3) THEN
               MINDIR = MIN0( MXURBC(NDXCOL,NDXROW),
     &                        MXURBC(NDXCOL+1,NDXROW),
     &                        MXURBC(NDXCOL,NDXROW+1),
     &                        MXURBC(NDXCOL+1,NDXROW+1) )
               MAXDIR = MAX0( MXURBC(NDXCOL,NDXROW),
     &                        MXURBC(NDXCOL+1,NDXROW),
     &                        MXURBC(NDXCOL,NDXROW+1),
     &                        MXURBC(NDXCOL+1,NDXROW+1) )
            ELSE IF (KST .EQ. 1 .OR. KST .EQ. 2) THEN
               MINDIR = MIN0( MXURBA(NDXCOL,NDXROW),
     &                        MXURBA(NDXCOL+1,NDXROW),
     &                        MXURBA(NDXCOL,NDXROW+1),
     &                        MXURBA(NDXCOL+1,NDXROW+1) )
               MAXDIR = MAX0( MXURBA(NDXCOL,NDXROW),
     &                        MXURBA(NDXCOL+1,NDXROW),
     &                        MXURBA(NDXCOL,NDXROW+1),
     &                        MXURBA(NDXCOL+1,NDXROW+1) )
            END IF
         END IF

      ELSE IF (NDXCOL .GT. 0 .AND. NDXCOL .LT. MAXCOL) THEN

         IF (RURAL) THEN
            IF (KST .EQ. 6) THEN
               MINDIR = MIN0( MXRURF(NDXCOL,NDXROW),
     &                        MXRURF(NDXCOL+1,NDXROW) )
               MAXDIR = MAX0( MXRURF(NDXCOL,NDXROW),
     &                        MXRURF(NDXCOL+1,NDXROW) )
            ELSE IF (KST .EQ. 5) THEN
               MINDIR = MIN0( MXRURE(NDXCOL,NDXROW),
     &                        MXRURE(NDXCOL+1,NDXROW) )
               MAXDIR = MAX0( MXRURE(NDXCOL,NDXROW),
     &                        MXRURE(NDXCOL+1,NDXROW) )
            ELSE IF (KST .EQ. 4) THEN
               MINDIR = MIN0( MXRURD(NDXCOL,NDXROW),
     &                        MXRURD(NDXCOL+1,NDXROW) )
               MAXDIR = MAX0( MXRURD(NDXCOL,NDXROW),
     &                        MXRURD(NDXCOL+1,NDXROW) )
            ELSE IF (KST .EQ. 3) THEN
               MINDIR = MIN0( MXRURC(NDXCOL,NDXROW),
     &                        MXRURC(NDXCOL+1,NDXROW) )
               MAXDIR = MAX0( MXRURC(NDXCOL,NDXROW),
     &                        MXRURC(NDXCOL+1,NDXROW) )
            ELSE IF (KST .EQ. 2) THEN
               MINDIR = MIN0( MXRURB(NDXCOL,NDXROW),
     &                        MXRURB(NDXCOL+1,NDXROW) )
               MAXDIR = MAX0( MXRURB(NDXCOL,NDXROW),
     &                        MXRURB(NDXCOL+1,NDXROW) )
            ELSE IF (KST .EQ. 1) THEN
               MINDIR = MIN0( MXRURA(NDXCOL,NDXROW),
     &                        MXRURA(NDXCOL+1,NDXROW) )
               MAXDIR = MAX0( MXRURA(NDXCOL,NDXROW),
     &                        MXRURA(NDXCOL+1,NDXROW) )
            END IF
         ELSE IF (URBAN) THEN
            IF (KST .EQ. 6 .OR. KST .EQ. 5) THEN
               MINDIR = MIN0( MXURBF(NDXCOL,NDXROW),
     &                        MXURBF(NDXCOL+1,NDXROW) )
               MAXDIR = MAX0( MXURBF(NDXCOL,NDXROW),
     &                        MXURBF(NDXCOL+1,NDXROW) )
            ELSE IF (KST .EQ. 4) THEN
               MINDIR = MIN0( MXURBD(NDXCOL,NDXROW),
     &                        MXURBD(NDXCOL+1,NDXROW) )
               MAXDIR = MAX0( MXURBD(NDXCOL,NDXROW),
     &                        MXURBD(NDXCOL+1,NDXROW) )
            ELSE IF (KST .EQ. 3) THEN
               MINDIR = MIN0( MXURBC(NDXCOL,NDXROW),
     &                        MXURBC(NDXCOL+1,NDXROW) )
               MAXDIR = MAX0( MXURBC(NDXCOL,NDXROW),
     &                        MXURBC(NDXCOL+1,NDXROW) )
            ELSE IF (KST .EQ. 1 .OR. KST .EQ. 2) THEN
               MINDIR = MIN0( MXURBA(NDXCOL,NDXROW),
     &                        MXURBA(NDXCOL+1,NDXROW) )
               MAXDIR = MAX0( MXURBA(NDXCOL,NDXROW),
     &                        MXURBA(NDXCOL+1,NDXROW) )
            END IF
         END IF

      ELSE IF (NDXROW .GT. 0 .AND. NDXROW .LT. MAXROW) THEN

         IF (RURAL) THEN
            IF (KST .EQ. 6) THEN
               MINDIR = MIN0( MXRURF(NDXCOL,NDXROW),
     &                        MXRURF(NDXCOL,NDXROW+1) )
               MAXDIR = MAX0( MXRURF(NDXCOL,NDXROW),
     &                        MXRURF(NDXCOL,NDXROW+1) )
            ELSE IF (KST .EQ. 5) THEN
               MINDIR = MIN0( MXRURE(NDXCOL,NDXROW),
     &                        MXRURE(NDXCOL,NDXROW+1) )
               MAXDIR = MAX0( MXRURE(NDXCOL,NDXROW),
     &                        MXRURE(NDXCOL,NDXROW+1) )
            ELSE IF (KST .EQ. 4) THEN
               MINDIR = MIN0( MXRURD(NDXCOL,NDXROW),
     &                        MXRURD(NDXCOL,NDXROW+1) )
               MAXDIR = MAX0( MXRURD(NDXCOL,NDXROW),
     &                        MXRURD(NDXCOL,NDXROW+1) )
            ELSE IF (KST .EQ. 3) THEN
               MINDIR = MIN0( MXRURC(NDXCOL,NDXROW),
     &                        MXRURC(NDXCOL,NDXROW+1) )
               MAXDIR = MAX0( MXRURC(NDXCOL,NDXROW),
     &                        MXRURC(NDXCOL,NDXROW+1) )
            ELSE IF (KST .EQ. 2) THEN
               MINDIR = MIN0( MXRURB(NDXCOL,NDXROW),
     &                        MXRURB(NDXCOL,NDXROW+1) )
               MAXDIR = MAX0( MXRURB(NDXCOL,NDXROW),
     &                        MXRURB(NDXCOL,NDXROW+1) )
            ELSE IF (KST .EQ. 1) THEN
               MINDIR = MIN0( MXRURA(NDXCOL,NDXROW),
     &                        MXRURA(NDXCOL,NDXROW+1) )
               MAXDIR = MAX0( MXRURA(NDXCOL,NDXROW),
     &                        MXRURA(NDXCOL,NDXROW+1) )
            END IF
         ELSE IF (URBAN) THEN
            IF (KST .EQ. 6 .OR. KST .EQ. 5) THEN
               MINDIR = MIN0( MXURBF(NDXCOL,NDXROW),
     &                        MXURBF(NDXCOL,NDXROW+1) )
               MAXDIR = MAX0( MXURBF(NDXCOL,NDXROW),
     &                        MXURBF(NDXCOL,NDXROW+1) )
            ELSE IF (KST .EQ. 4) THEN
               MINDIR = MIN0( MXURBD(NDXCOL,NDXROW),
     &                        MXURBD(NDXCOL,NDXROW+1) )
               MAXDIR = MAX0( MXURBD(NDXCOL,NDXROW),
     &                        MXURBD(NDXCOL,NDXROW+1) )
            ELSE IF (KST .EQ. 3) THEN
               MINDIR = MIN0( MXURBC(NDXCOL,NDXROW),
     &                        MXURBC(NDXCOL,NDXROW+1) )
               MAXDIR = MAX0( MXURBC(NDXCOL,NDXROW),
     &                        MXURBC(NDXCOL,NDXROW+1) )
            ELSE IF (KST .EQ. 1 .OR. KST .EQ. 2) THEN
               MINDIR = MIN0( MXURBA(NDXCOL,NDXROW),
     &                        MXURBA(NDXCOL,NDXROW+1) )
               MAXDIR = MAX0( MXURBA(NDXCOL,NDXROW),
     &                        MXURBA(NDXCOL,NDXROW+1) )
            END IF
         END IF

      ELSE

         IF (RURAL) THEN
            IF (KST .EQ. 6) THEN
               MINDIR = MXRURF(NDXCOL,NDXROW)
               MAXDIR = MXRURF(NDXCOL,NDXROW)
            ELSE IF (KST .EQ. 5) THEN
               MINDIR = MXRURE(NDXCOL,NDXROW)
               MAXDIR = MXRURE(NDXCOL,NDXROW)
            ELSE IF (KST .EQ. 4) THEN
               MINDIR = MXRURD(NDXCOL,NDXROW)
               MAXDIR = MXRURD(NDXCOL,NDXROW)
            ELSE IF (KST .EQ. 3) THEN
               MINDIR = MXRURC(NDXCOL,NDXROW)
               MAXDIR = MXRURC(NDXCOL,NDXROW)
            ELSE IF (KST .EQ. 2) THEN
               MINDIR = MXRURB(NDXCOL,NDXROW)
               MAXDIR = MXRURB(NDXCOL,NDXROW)
            ELSE IF (KST .EQ. 1) THEN
               MINDIR = MXRURA(NDXCOL,NDXROW)
               MAXDIR = MXRURA(NDXCOL,NDXROW)
            END IF
         ELSE IF (URBAN) THEN
            IF (KST .EQ. 6 .OR. KST .EQ. 5) THEN
               MINDIR = MXURBF(NDXCOL,NDXROW)
               MAXDIR = MXURBF(NDXCOL,NDXROW)
            ELSE IF (KST .EQ. 4) THEN
               MINDIR = MXURBD(NDXCOL,NDXROW)
               MAXDIR = MXURBD(NDXCOL,NDXROW)
            ELSE IF (KST .EQ. 3) THEN
               MINDIR = MXURBC(NDXCOL,NDXROW)
               MAXDIR = MXURBC(NDXCOL,NDXROW)
            ELSE IF (KST .EQ. 1 .OR. KST .EQ. 2) THEN
               MINDIR = MXURBA(NDXCOL,NDXROW)
               MAXDIR = MXURBA(NDXCOL,NDXROW)
            END IF
         END IF

      END IF

      RETURN
      END

c-----------------------------------------------------------------------
      subroutine depcor(vdi,vsi,zdi,zri,xri,xvi,hi,hmixi,ui,
     &                  rurali,urbani,ksti,sgzi,sgz0i,debugi,iouniti,
     &                  qcor,pcor,pxrzd)
c-----------------------------------------------------------------------
c
c --- ISCST2    Version: 1.0            Level: 930215           DEPCOR
c               D. Strimaitis, SRC
c
c PURPOSE:     DEPCOR returns correction factors for the emission rate
c              and the vertical distribution of plume material to
c              account for deposition between the source and the current
c              receptor.
c
c ARGUMENTS:
c    PASSED:  vdi,vsi   total deposition and gravitational settling  [r]
c                       velocities (m/s)
c             zdi       height for evaluating deposition (m)         [r]
c             zri       receptor height (m)                          [r]
c             xri       receptor distance (m)                        [r]
c             xvi       virtual source distance (m)                  [r]
c             hi        plume height (m)                             [r]
c             hmixi     mixing height (m)                            [r]
c             ui        wind speed (m/s)                             [r]
c             rurali    logical for rural dispersion curves          [l]
c             urbani    logical for urban dispersion curves          [l]
c             ksti      stability class indicator                    [i]
c             sgzi      sigma-z at current receptor (m)              [r]
c             sgz0i     initial sigma-z (e.g. for BID) (m)           [r]
c             debugi    logical controlling DEBUG output             [l]
c             iouniti   unit number for DEBUG output                 [i]
c  RETURNED:  qcor      ratio of depleted emission rate to original  [r]
c             pcorzr    profile correction factor at receptor height [r]
c             pcorzd    profile correction factor at deposition ht   [r]
c
c CALLING ROUTINES:   PCALC, VCALC, ACALC
c
c EXTERNAL ROUTINES:  RESIST, PROFD, PROFD1, PROFD2, DEPLETE
c-----------------------------------------------------------------------
c  NOTE:  all inputs ending with "i" are passed to subsequent routines
c         through common /DEPVAR/.

      include 'DEPVAR.INC'
      logical rurali,urbani,debugi
c  Arrays RUR and URB contain coefficients derived from Briggs sigma-z
c  coefficients, for use in Horst's resistance and profile functions.
      real rur(3,6),urb(3,6)
      data rur/3.989,0.,0.,
     2         6.649,0.,0.,
     3         9.974,0.03125,0.,
     4         13.298,0.4167,0.,
     5         26.596,0.6667,0.005,
     6         49.868,2.344,0.03296/
      data urb/3.325,0.,0.,
     2         3.325,0.,0.,
     3         3.989,0.,0.,
     4         5.699,0.01531,0.,
     5         9.974,0.2344,0.,
     6         9.974,0.2344,0./

c  Initialize deposition factors to 1, and return if edge of plume is
c  well above the ground at the receptor (h > 5 sigz)
      pxrzd=1.0
      pcor=1.0
      qcor=1.0
      if(hi .GT. 5.*sgzi) return

c  Set constants
      rtpiby2=1.2533141
      rt2=1.4142136
      rtpi=1.7724539

c  Assign input variables to working variables
      vd=vdi
      vs=vsi
      zd=zdi
      zr=zri
      xr=xri
      xv=xvi
      h=hi
      hmix=hmixi
      onebyu=1./ui
      rural=rurali
      urban=urbani
      kst=ksti
      sgz=sgzi
      sgz0=sgz0i
      debug=debugi
      iounit=iouniti

c  Obtain coefficients for resistance and profile functions
c             [a,b,c]p coefficients for profile functions
c             [a,b,c]r coefficients for resistance functions
      if(rural) then
         ar=onebyu*rur(1,kst)
         br=onebyu*rur(2,kst)
         cr=onebyu*rur(3,kst)
         ap=ar
         bp=br/rtpiby2
         cp=cr*rtpiby2
      else
         ar=onebyu*urb(1,kst)
         br=onebyu*urb(2,kst)
         cr=onebyu*urb(3,kst)
         ap=ar
         bp=br/rtpiby2
      endif

c  Flush other variables in DEPVAR common
      igrav=0

c  Obtain profile factor at height zd for current receptor (x=xr).
c  First, check importance of gravitational settling velocity by
c  computing vs*RESIST at the minimum of z=hmix or z=3*sgz.
c  Note: profile function for URBAN class A & B is of a different
c  form than all other classes, which is contained in PROFD2.
      zcheck=3.*sgz
      if(hmix .GT. h) zcheck=AMIN1(hmix,zcheck)
      if(vs*RESIST(zcheck) .GT. 0.1) then
         igrav=1
c  --  gravitational settling is "large", use numerical integration.
         call PROFD(pxrzd)
c     elseif(urban .AND. kst .LT. 3) then
         igrav=0
c  --  gravitational settling is "small", use analytic function as
c  --  approximation for URBAN class A & B.
         call PROFD2(pxrzd)
      else
         igrav=0
c  --  gravitational settling is "small", use analytic function for all
c  --  other classes.
         call PROFD1(pxrzd)
      endif

c  Now compute factor at receptor height zr.
      if(zr .LE. zd) then
         pcor=pxrzd
      elseif(igrav .EQ. 0) then
         pcor=pxrzd*(1.+(vd-vs)*RESIST(zr))
      else
         pcor=pxrzd*(1.+((vd-vs)/vs)*(1.-EXP(-vs*RESIST(zr))))
      endif

c  Compute ratio of depleted source strength to initial source strength.
      call DEPLETE(qcor)

      if(debug) then
         write(iounit,*) '  DEPCOR Module:'
         write(iounit,*) '     igrav,pxrzd = ',igrav,pxrzd
         write(iounit,*) '       pcor,qcor = ',pcor,qcor
      endif

      return
      end
c-----------------------------------------------------------------------
      function resist(z)
c-----------------------------------------------------------------------
c
c --- ISCST2    Version: 1.0            Level: 930215           RESIST
c               D. Strimaitis, SRC
c
c PURPOSE:     Function RESIST provides the resistance factor for a
c              particular height above the surface (z), relative to a
c              reference height (zd). Based on Horst (1983).
c
c ARGUMENTS:
c    PASSED:  z         height above the surface                     [r]
c
c  RETURNED:  resist    resistance factor (via /DEPVAR/)             [r]
c
c CALLING ROUTINES:   DEPCOR, FINT
c
c EXTERNAL ROUTINES:  ROOT
c-----------------------------------------------------------------------


c  AR, BR, and CR are the coefficients of the 3 F(z) forms given for
c  the various forms of the Briggs representation of sigma-z (rural &
c  urban)

c  Common DEPVAR contains AR, BR, CR, and zd
      include 'DEPVAR.INC'

      if(z .GT. zd) then
c  Special Case:  URBAN/Stability Class=A,B
c  Resistance function requires the root of a implicit expression.
c  Because sigma-z functions are the same for URBAN/A,B the equation to
c  solve for x(z) is ax(1+bx)^.5=z*(pi/2)^.5, where a=.24, b=.001
         if(urban .AND. kst .LT. 3) then
c --       cz=SQRT(pi/2) * z/a = 5.222142 * z, where a=.24
           cz=5.222142*z
           call ROOT(cz,xz)
           argz=cz/xz
c  Approximate functional dependence on zd using binomial expansion
c  --      c=2*b*SQRT(pi/2)/a = 0.0104443
c  --      8./(c*zd)=765.96804/zd
           argzd=765.96804/zd
           resist=AR*ALOG((argz-1.)*(argzd+1.)/(argz+1.))
         else
           resist=AR*ALOG(z/zd) + BR*(z-zd) + CR*(z*z-zd*zd)
         endif
      else
         resist=0.0
      endif

      return
      end
c-----------------------------------------------------------------------
      subroutine root(c,x)
c-----------------------------------------------------------------------
c
c --- ISCST2    Version: 1.0            Level: 930215           ROOT
c               D. Strimaitis, SRC
c
c PURPOSE:     Program solves an expression of the form:
c                  x*(1+b*x)**.5=c
c              using a simple iteration on:
c                  x=c/(1+b*x0)**.5
c
c              ! WARNING !     This is a special solver for current
c                              application.....it may not converge for
c                              other applications.
c
c ARGUMENTS:
c    PASSED:  c         constant for RHS of equation to solve        [r]
c
c  RETURNED:  x         root of equation                             [r]
c
c CALLING ROUTINES:   RESIST
c
c EXTERNAL ROUTINES:  none
c-----------------------------------------------------------------------

      data b/.001/,onebyb/1000./,twob/.002/
      data eby2/.005/,oneby3/.3333333/
c  "e" is a fractional error criterion for convergence, so eby2=e/2

c  First guess
      twobc=twob*c
      if(twobc .LT. 6.) then
         x0=(SQRT(1.+twobc)-1.)*onebyb
      else
         x0=(c*c*onebyb)**oneby3
      endif
10    x=c/SQRT(1.+b*x0)
      errby2=ABS(x-x0)/(x+x0)
      if(errby2 .LE. eby2) goto 100
      x0=x
      goto 10

100   continue
      return
      end
c-----------------------------------------------------------------------
      subroutine profd(pxzd)
c-----------------------------------------------------------------------
c
c --- ISCST2    Version: 1.0            Level: 930930           PROFD
c               D. Strimaitis, SRC
c
c PURPOSE:     Subroutine PROFD provides the base profile factor at
c              height zd for the given deposition and settling velocities,
c              and sigma-z.  Here, the settling velocity and diffusion
c              resistance are not "small" so that a numerical integration
c              is used to obtain Ip:
c                    P(x,zd) = 1. / (1.+ Ip*(vd-vs)/vs) .... "pxzd"
c              Based on Horst (1983).
c
c ARGUMENTS:
c    PASSED:            (see /DEPVAR/)
c
c  RETURNED:  pxzd      profile factor at height zd                  [r]
c
c CALLING ROUTINES:   DEPCOR, F2INT
c
c EXTERNAL ROUTINES:  QATR, FINT
c-----------------------------------------------------------------------

c  Set up call to integration routine QATR(xl,xu,eps,ndim,fct,y,ier,num,aux)
c  Declare parameter to fix the size of the aux array
      parameter(ndim=16)
      real aux(ndim)
      external FINT
      INTEGER*2 NUM
      include 'DEPVAR.INC'

c  Return a value of 1.0 for pxzd if the sigma-z is less than 2*zd,
c  since the integrals assume that zd is less than the plume spread.
      pxzd=1.0
      if(sgz .LE. 2.*zd) return

c  Evaluate integral Ip:
c  Upper limit of integral reset from infinity to MIN(5*sigma-z,hmix)
      eps=.10
      top=AMIN1(5.*sgz,hmix)
      call QATR(zd,top,eps,ndim,FINT,value,ier,num,aux)
      if(ier .EQ. 2) then
         write(*,*) 'WARNING from PROFD -  integration failed to'
         write(*,*) 'converge to fractional error of ',eps
      endif
      pxzd=1./(1.+value*(vd-vs)/vs)

      return
      end
c-----------------------------------------------------------------------
      function fint(z)
c-----------------------------------------------------------------------
c
c --- ISCST2    Version: 1.0            Level: 930215           FINT
c               D. Strimaitis, SRC
c
c PURPOSE:     Function is the integrand of integral over height to
c              calculate the profile parameter P(x,zd).  The resistance
c              value is returned from the function RESIST.  Common
c              /DEPVAR/ is used to pass data that are constant during
c              the integration, so QATR (the integrator) only needs to
c              pass values of height (z).
c               -VCOUP calculates the vertical coupling factor:
c                       (1/(sgz*SQRT(2pi)))*EXP...
c                 (this includes multiple reflections!)
c
c ARGUMENTS:
c    PASSED:  z         height above surface                         [r]
c
c  RETURNED:  fint      value of integrand                           [r]
c
c CALLING ROUTINES:   QATR
c
c EXTERNAL ROUTINES:  RESIST, VCOUP
c-----------------------------------------------------------------------
      include 'DEPVAR.INC'

      arg=vs*RESIST(z)
      fint=(1.-EXP(-arg))*VCOUP(z,0.,sgz,hmix)

      return
      end
c-----------------------------------------------------------------------
      subroutine profd1(pxzd)
c-----------------------------------------------------------------------
c
c --- ISCST2    Version: 1.0            Level: 930930           PROFD1
c               D. Strimaitis, SRC
c
c PURPOSE:     Subroutine PROFD1 provides the base profile factor at
c              height zd for the given deposition and settling
c              velocities, and sigma-z.  Here, the settling velocity
c              and diffusion resistance are "small" so that the analytic
c              results are used.
c              Based on Horst (1983).
c
c ARGUMENTS:
c    PASSED:            (see /DEPVAR/)
c
c  RETURNED:  pxzd      profile factor at height zd                  [r]
c
c CALLING ROUTINES:   DEPCOR, F2INT
c
c EXTERNAL ROUTINES:  none
c-----------------------------------------------------------------------


c  AP, BP, and CP are the coefficients of the 3 F(z) forms given for
c  the various forms of the Briggs representation of sigma-z (rural &
c  urban)

c  Approximate the results for a mixing lid by "clamping" the
c  calculation at the limit of a well-mixed plume in the vertical.
c       .7071=SQRT(1/2)
c       .6267=SQRT(pi/8)
c       .5157=SQRT( [SQRT(2/pi)]/3 )
c       .8932=SQRT( SQRT(2/pi) )

      include 'DEPVAR.INC'

c  Return a value of 1.0 for pxzd if the sigma-z is less than 2*zd,
c  since the integrals assume that zd is less than the plume spread.
      pxzd=1.0
      if(sgz .LE. 2.*zd) return

      if(hmix .GT. h) then
         za=AMIN1(sgz,.7071*hmix)
         if(BP .GT. 0.) zb=AMIN1(sgz,.6267*hmix)
         if(CP .GT. 0.) zc=AMIN1(sgz,.5157*hmix)
      else
         za=sgz
         zb=sgz
         zc=sgz
      endif

      pxzd=1./(1.+(vd-vs)*(AP*(ALOG(rt2*za/zd) -1.) + BP*(zb-rtpiby2*zd)
     1         + CP*(zc*zc-zd*zd)))

      return
      end
c-----------------------------------------------------------------------
      subroutine profd2(pxzd)
c-----------------------------------------------------------------------
c
c --- ISCST2    Version: 1.0            Level: 930930           PROFD2
c               D. Strimaitis, SRC
c
c PURPOSE:     Subroutine PROFD2 provides the base profile factor at
c              height zd for the given deposition and settling
c              velocities, and sigma-z.  Here, the settling velocity
c              and diffusion resistance are "small" so that the analytic
c              results are used.
c              --------------- URBAN Class A & B !!! ---------------------
c              Based on Horst (1983).
c
c ARGUMENTS:
c    PASSED:            (see /DEPVAR/)
c
c  RETURNED:  pxzd      profile factor at height zd                  [r]
c
c CALLING ROUTINES:   DEPCOR, F2INT
c
c EXTERNAL ROUTINES:  none
c-----------------------------------------------------------------------

c  Approximate the results for a mixing lid by switching to the
c  calculation for the limit of a well-mixed plume in the vertical
c  when sigmaz = .7071 H, where  .7071=SQRT(1/2).

      include 'DEPVAR.INC'

c  Return a value of 1.0 for pxzd if the sigma-z is less than 2*zd,
c  since the integrals assume that zd is less than the plume spread.
      pxzd=1.0
      if(sgz .LE. 2.*zd) return

c  AP is the coefficient (SQRT(2/pi) / aU)
c     ck = 2*b*SQRT(pi/2)/a = 0.0104443
      ck=0.0104443

      za=sgz
      if(hmix .GT. h) za=AMIN1(sgz,.7071*hmix)
      sgz1=za
      if(za .LT. 300.) then
         sgz1=za*(1.-za*.0006)**2
      else
         sgz1=za*(1.-300.*.0006)**2
      endif
      sgz2=sgz1
      if(sgz1 .GT. 1000.) sgz2=SQRT(1000.*sgz1)
      approx=-1.+ALOG(rt2*sgz1/zd)+ALOG(1.+ck*zd/8.)-
     &       ck*rt2*sgz2/(8.*rtpi)
      pxzd=1./(1.+(vd-vs)*AP*approx)

      return
      end
c-----------------------------------------------------------------------
      subroutine deplete(qcor)
c-----------------------------------------------------------------------
c
c --- ISCST2    Version: 1.0            Level: 930930           DEPLETE
c               D. Strimaitis, SRC
c
c PURPOSE:     Subroutine DEPLETE provides the value of the integral of
c              the product of the vertical distribution function and the
c              profile correction factor over the travel of the plume
c              from the source to the receptor.  Because the integrating
c              subroutine may be used within this integration, the
c              routine QATR has been duplicated as QATR2.
c              Based on Horst (1983).
c
c ARGUMENTS:
c    PASSED:            (see /DEPVAR/)
c
c  RETURNED:  qcor      ratio of depleted emission rate to original  [r]
c
c CALLING ROUTINES:   DEPCOR
c
c EXTERNAL ROUTINES:  QATR2, F2INT
c-----------------------------------------------------------------------

c  Set up call to QATR2(xl,xu,eps,ndim2,fct,y,ier,num,aux2)
c  Declare parameter to fix the size of the aux2 array
      parameter(ndim2=16)
      real aux2(ndim2)
      external F2INT
      INTEGER*2 NUM
      include 'DEPVAR.INC'

      IOUNIT = 13
      iout = 9

c  Evaluate integral:
      eps=.05
c  Do not let integral try to evaluate sigma-z at x=0! -- start at 1m
      call QATR2(1.,xr,eps,ndim2,F2INT,value,ier,num,aux2)
      if(ier .EQ. 2) then
         write(*,*) 'WARNING from DEPLETE -  integration failed to'
         write(*,*) 'converge to fractional error of ',eps
         write(iout,*) 'WARNING from DEPLETE -  integration failed to'
         write(iout,*) 'converge to fractional error of ',eps
      endif
      qcor=EXP(-vd*value)

      if(debug) then
         write(iounit,*) '  DEPLETE: eps, QATR2 iterations = ',eps,num
      endif

      return
      end
c-----------------------------------------------------------------------
      function f2int(x)
c-----------------------------------------------------------------------
c
c --- ISCST2    Version: 1.0            Level: 930215           F2INT
c               D. Strimaitis, SRC
c
c PURPOSE:     Function is the integrand of integral over the travel
c              distance to obtain the fraction of material removed from
c              the plume. Common /DEPVAR/ is used to pass data that are
c              constant during the integration, so QATR (the integrator)
c              only needs to pass values of distance.
c
c ARGUMENTS:
c    PASSED:  z         height above surface                         [r]
c
c  RETURNED:  fint      value of integrand                           [r]
c
c CALLING ROUTINES:   QATR2
c
c EXTERNAL ROUTINES:  SIGZD, PROFD, PROFD1, PROD2, VCOUP
c-----------------------------------------------------------------------
      include 'DEPVAR.INC'

c  Account for any virtual source distance
      xxv=x+xv

c  Recalculate sigma-z; note that sgz had been for the receptor
c  location -- once the integration in distance begins, this value is
c  not needed, so we replace it with that for the current "x".
      call SIGZD(xxv,sgz)

c  Account for any initial sigma-z (e.g BID)
      sgz=SQRT(sgz**2+sgz0**2)

c  Adjust plume height for gravitational settling
      hh=AMAX1(0.,h-vs*x*onebyu)

c  -VCOUP calculates the vertical coupling factor:
c    (1/(sgz*SQRT(2pi)))*EXP...  (this includes multiple reflections!)
c  -PROFD1 or PROFD2 calculates the profile correction factor if
c  gravitational settling is weak (analytic representations are used);
c  -PROFD calculates the profile correction factor if gravitational
c  settling is strong (numerical integration is used).
      f2int=onebyu*VCOUP(zd,hh,sgz,hmix)
      if(f2int .GT. 0.0) then
         if(igrav .EQ. 0) then
            if(urban .AND. kst .LT. 3) then
               call PROFD2(pxzd)
            else
               call PROFD1(pxzd)
            endif
         else
            call PROFD(pxzd)
         endif
         f2int=f2int*pxzd
      endif

      return
      end
c----------------------------------------------------------------------
      FUNCTION VCOUP(ZR,ZS,SZ,HLID)
c----------------------------------------------------------------------
c
c --- ISCST2     Version: 1.0       Level: 930215                 VCOUP
c                R. Yamartino, SRC
c
c     Adapted from --
c --- CALPUFF    Version: 1.0       Level: 900228                 VCOUP
c                R. Yamartino, SRC
c
c --- PURPOSE:  Computes the vertical coupling coefficient for a source
c               at height ZS, to a receptor at height ZR given a plume
c               with sigma z of SZ and including reflections from the
c               ground and lid at height HLID.
c
c
c --- INPUTS:
c
c                ZR - real    - Z-coordinate of receptor (m)
c                ZS - real    - Z-coordinate of source (m)
c                SZ - real    - Z-sigma at receptor (m)
c              HLID - real    - Mixing depth at receptor (m)
c              Note that these input values must have same units.
c
c
c --- OUTPUTS:
c
c             VCOUP - real    - Vertical coupling coefficient (1/m)
c
c --- VCOUP called by:  FINT, F2INT
c --- VCOUP calls:      none
c----------------------------------------------------------------------
c
c --- All heights have same units.      1/26/89
c
      data small/1.0e-10/,srttpi/2.5066283/,pi/3.1415926/
c
      vcoup = 0.0
c
c !!! Guard against hlid LE plume height:  reset to 10 sigma-z
      if(hlid .LE. zs) hlid=zs+10.*sz
          if((sz/hlid).gt.0.63) go to 15
c !!! Also, make sure that receptors above a real lid have zero conc.
      if(zr .GT. hlid) return
c
c --- Sum the reflection terms
      sz1 = sz + small
      sz2 = sz*sz + small
      x = -0.5*(zr-zs)**2/sz2
      if(x.lt.-20.0) go to 20
      expz = exp(x)
      x = -0.5*(zr+zs)**2/sz2
      if(x.gt.-20.0) expz = expz + exp(x)
c
          do 10 j = -1 , +1 , 2
            zrefl = 2.0*float(j)*hlid
            x = -0.5*(zr+zs+zrefl)**2/sz2
            if(x.gt.-20.0) expz = expz + exp(x)
            x = -0.5*(zr-zs+zrefl)**2/sz2
            if(x.gt.-20.0) expz = expz + exp(x)
 10       continue
c
      vcoup = expz/(srttpi*sz1)
      go to 20
c
c --- Near uniform mixing using approximation of R. Yamartino
c     (JAPCA 27, 5, MAY 1977)
 15   szsb = -0.5*(pi*sz/hlid)**2
      expz = 1.0
      if(szsb.gt.-20.0) then
         beta = exp(szsb)
         beta2 = beta*beta
         expz = (1.0-beta2)*(1.0+beta2+2.0*beta*cos(pi*zs/hlid)*
     *                                       cos(pi*zr/hlid))
      endif
      vcoup = expz/hlid
c
 20   return
      end
c-----------------------------------------------------------------------
      subroutine qatr(xl,xu,eps,ndim,fct,y,ier,i,aux)
c-----------------------------------------------------------------------
c
c --- ISCST2    Version: 1.0            Level: 930930           QATR
c
c PURPOSE:      Integration routine adapted from the IBM SSP program
c               DQATR.  Modified for single precision.
c
c ARGUMENTS:
c    PASSED:    xl,xu   lower and upper limits of integration        [r]
c               eps     fractional error used to define convergence  [r]
c               ndim    dimension of array aux (parameter)           [p]
c               fct     external function (integrand)
c               aux     working array, passed to allow variable dim. [r]
c  RETURNED:    y       value of integral                            [r]
c               ier     status flag at termination                   [i]
c               i       number of subdivision steps                  [i]
c
c CALLING ROUTINES:     PROFD
c
c EXTERNAL ROUTINES:    none
c-----------------------------------------------------------------------

c  NOTES: status flags denote the following --
c               ier=0   value of integral converged to within eps
c               ier=1   value of integral is diverging
c               ier=2   value of integral did not converge to within
c                       eps before ndim limit was reached

c  NDIM Note:  The aux(ndim) array keeps track of the average value of
c              the integrand for each of the steps in subdividing the
c              interval.  For example, when i=4 in the "do 7 i=2,ndim"
c              loop, aux(4) contains the mean value as obtained from
c              the trapezoidal rule, while aux(1 through 3) contain
c              a set of current Romberg extrapolations.  At each new
c              value of i, the interval is subdivided again, and the
c              integrand is evaluated at jj=2**(i-2) new points.
c              Therefore, at i=5, there will be jj=8 new points added
c              to the 9 points already used in the interval.  When i=17
c              there will be jj=32,768 new points added to the 32,769
c              already used.  This is the maximum number of new points
c              that are allowed as jj is an INTEGER*2 variable, with
c              a maximum value of 2**15.  Therefore, i should not exceed
c              17, and probably should be no larger than 16.  This means
c              that NDIM should be set at 16.  Larger values of NDIM
c              could be accepted if the INTEGER*2 variables were changed
c              to INTEGER*4, but for most applications, 30000 to 60000
c              points ought to be sufficient for evaluating an integral.

      EXTERNAL fct
      dimension aux(ndim)
      integer*2 i,ii,ji,j,jj
      half=0.5

c  Preparations for Romberg loop
      aux(1)=half*(fct(xl)+fct(xu))
      h=xu-xl
      y=h*aux(1)
      if(ndim .LE. 1) then
         ier=2
         return
      elseif(h .EQ. 0.) then
         ier=0
         return
      endif

      hh=h
      delt2=0.
      p=1.
      jj=1

c  Initialize flag for integer*2 limit: jj cannot exceed 32,000
c  This limit should not be reached if NDIM .LE. 16
      lstop=0

      do 7 i=2,ndim
         y=aux(1)
         delt1=delt2
         hd=hh
         hh=half*hh
         p=half*p
         x=xl+hh
         sm=0.

c  Integer*2 limit: jj cannot exceed 32,000
         if(lstop .EQ. 1) then
            write(6,1010)
1010        format(2X,'ERROR FROM QATR - VARIABLE jj EXCEEDED 32,000')
            stop
         endif
         if(jj .GT. 16000) lstop=1

         do 3 j=1,jj
            sm=sm+fct(x)
            x=x+hd
3        continue

c  A new approximation to the integral is computed by means
c  of the trapezoidal rule
         aux(i)=half*aux(i-1)+p*sm

c  Start of Rombergs extrapolation method

         q=1.
         ji=i-1
         do 4 j=1,ji
            ii=i-j
            q=q+q
            q=q+q
            aux(ii)=aux(ii+1)+(aux(ii+1)-aux(ii))/(q-1.)
4        continue

c  End of Romberg step

         delt2=ABS(y-aux(1))
         if(i .GE. 3) then
c  Modification for cases in which function = 0 over interval
            if(y .EQ. 0.) then
               ier=0
               return
            elseif(delt2/y .LE. eps) then
               ier=0
               y=h*aux(1)
               return
c           elseif(delt2 .GE. delt1)then
c              ier=1
c              y=h*y
c              return
            endif
         endif
7     jj=jj+jj
      ier=2
      y=h*aux(1)

      return
      end
c-----------------------------------------------------------------------
      subroutine qatr2(xl,xu,eps,ndim,fct,y,ier,i,aux)
c-----------------------------------------------------------------------
c
c --- ISCST2    Version: 1.0            Level: 930930           QATR2
c
c PURPOSE:      Integration routine adapted from the IBM SSP program
c               DQATR.  Modified for single precision.  This is a COPY
c               of QATR for use in double integrations.
c
c ARGUMENTS:
c    PASSED:    xl,xu   lower and upper limits of integration        [r]
c               eps     fractional error used to define convergence  [r]
c               ndim    dimension of array aux (parameter)           [p]
c               fct     external function (integrand)
c               aux     working array, passed to allow variable dim. [r]
c  RETURNED:    y       value of integral                            [r]
c               ier     status flag at terminatio                    [i]
c               i       number of subdivision steps                  [i]
c
c CALLING ROUTINES:     DEPLETE
c
c EXTERNAL ROUTINES:    none
c-----------------------------------------------------------------------

c  NOTES: status flags denote the following --
c               ier=0   value of integral converged to within eps
c               ier=1   value of integral is diverging
c               ier=2   value of integral did not converge to within
c                       eps before ndim limit was reached

c  NDIM Note:  The aux(ndim) array keeps track of the average value of
c              the integrand for each of the steps in subdividing the
c              interval.  For example, when i=4 in the "do 7 i=2,ndim"
c              loop, aux(4) contains the mean value as obtained from
c              the trapezoidal rule, while aux(1 through 3) contain
c              a set of current Romberg extrapolations.  At each new
c              value of i, the interval is subdivided again, and the
c              integrand is evaluated at jj=2**(i-2) new points.
c              Therefore, at i=5, there will be jj=8 new points added
c              to the 9 points already used in the interval.  When i=17
c              there will be jj=32,768 new points added to the 32,769
c              already used.  This is the maximum number of new points
c              that are allowed as jj is an INTEGER*2 variable, with
c              a maximum value of 2**15.  Therefore, i should not exceed
c              17, and probably should be no larger than 16.  This means
c              that NDIM should be set at 16.  Larger values of NDIM
c              could be accepted if the INTEGER*2 variables were changed
c              to INTEGER*4, but for most applications, 30000 to 60000
c              points ought to be sufficient for evaluating an integral.

      EXTERNAL fct
      dimension aux(ndim)
      integer*2 i,ii,ji,j,jj
      half=0.5

c  Preparations for Romberg loop
      aux(1)=half*(fct(xl)+fct(xu))
      h=xu-xl
      y=h*aux(1)
      if(ndim .LE. 1) then
         ier=2
         return
      elseif(h .EQ. 0.) then
         ier=0
         return
      endif

      hh=h
      delt2=0.
      p=1.
      jj=1

c  Initialize flag for integer*2 limit: jj cannot exceed 32,000
c  This limit should not be reached if NDIM .LE. 16
      lstop=0

      do 7 i=2,ndim
         y=aux(1)
         delt1=delt2
         hd=hh
         hh=half*hh
         p=half*p
         x=xl+hh
         sm=0.

c  Integer*2 limit: jj cannot exceed 32,000
         if(lstop .EQ. 1) then
            write(6,1010)
1010        format(2X,'ERROR FROM QATR2- VARIABLE jj EXCEEDED 32,000')
            stop
         endif
         if(jj .GT. 16000) lstop=1

         do 3 j=1,jj
            sm=sm+fct(x)
            x=x+hd
3        continue

c  A new approximation to the integral is computed by means
c  of the trapezoidal rule
         aux(i)=half*aux(i-1)+p*sm

c  Start of Rombergs extrapolation method

         q=1.
         ji=i-1
         do 4 j=1,ji
            ii=i-j
            q=q+q
            q=q+q
            aux(ii)=aux(ii+1)+(aux(ii+1)-aux(ii))/(q-1.)
4        continue

c  End of Romberg step

         delt2=ABS(y-aux(1))
         if(i .GE. 3) then
c  Modification for cases in which function = 0 over interval
            if(y .EQ. 0.) then
               ier=0
               return
            elseif(delt2/y .LE. eps) then
               ier=0
               y=h*aux(1)
               return
c           elseif(delt2 .GE. delt1)then
c              ier=1
c              y=h*y
c              return
            endif
         endif
7     jj=jj+jj
      ier=2
      y=h*aux(1)

      return
      end
