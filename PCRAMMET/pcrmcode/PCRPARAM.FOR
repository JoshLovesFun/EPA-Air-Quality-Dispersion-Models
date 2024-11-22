      SUBROUTINE STABIT(IDAY,IHR)
C***********************************************************************
C*    STABIT      Module of PCRAMMET Meteorological Pre-processor
C*
C*    PURPOSE: This Routine Calculates Hourly Stability Classes
C*
C*    PROGRAMMER: Jayant Hardikar
C*                PES Inc.
C*                Adapted From MPRM
C*
C*    DATE:       February 23, 1995
C*
C*    INPUTS:     Wind speed, ciling height, cloud cover,
C*                sunris/sunset/solar elevation angle
C*
C*    OUTPUTS:    Pasquill-Gifford Stability Category
C*
C*    CALLED FROM: MAIN program
C***********************************************************************
C*
C*    Variable Declarations
      INCLUDE 'PCRAM.INC'
      REAL ANGL(3)
      INTEGER LSTAB(12,7)
      DATA ANGL /60.0, 35.0, 15.0/
      DATA LSTAB /7,7,7,6,6,6,5,5,5,5,4,4,
     &            6,6,6,5,5,5,4,4,4,4,4,4,
     &            4,4,4,4,4,4,4,4,4,4,4,4,
     &            3,3,3,4,4,4,4,4,4,4,4,4,
     &            2,2,2,3,3,3,3,3,3,4,4,4,
     &            1,2,2,2,2,2,2,3,3,3,3,4,
     &            1,1,1,1,1,2,2,2,2,3,3,3/

C*    INITIALIZE STABILITY BEFORE IT IS CALCULATED
      KST(IHR)  = 9
      ISPD      = 0
      IRADX     = 0

C*    Determine the wind speed index based on the wind speed (in knots)
      ISPD = KSPEED(IHR)
      IF( ISPD .LT. 1 )  ISPD = 1
      IF( ISPD .GT. 12 ) ISPD = 12

      ICLHT = ICEIL(IHR)
      ISKY  = ICOVER(IHR)

      IF( ISKY .LE. 10  .AND.  KSPEED(IHR) .LT. 90 )THEN

C*       DETERMINE RADIATION INDEX.
         IF (ISKY .EQ. 10 .AND. ICLHT .LT. 70) THEN

C*          LOW/MIDDLE OVERCAST CLOUD LAYER: NEUTRAL CONDITIONS
            IRADX = 3

         ELSE IF (IHR .GT. SUNRIS .AND. IHR .LT. SUNSET) THEN

C*          Daytime conditions
            DO 220 I=1,3
               IF (SOLANG(IHR) .GT. ANGL(I)) GO TO 230
220         CONTINUE
            I=4
230         ICN=5-I
            IF (ISKY .GT. 5) GO TO 240
            IRADX=ICN+3
            GO TO 280
240         IRADX=ICN-1
            IF (ICLHT .LT. 70) GO TO 250
            IF (ICLHT .LT. 160) GO TO 260
            IF (ISKY .EQ. 10) GO TO 270
            IRADX=ICN
            GO TO 270
250         IRADX=ICN-2
            GO TO 270
260         IF (ISKY .EQ. 10) IRADX=IRADX-1
270         IF (IRADX .LT. 1) IRADX=1
            IRADX=IRADX+3

         ELSE
C*          Nighttime conditions
            IRADX=2
            IF (ISKY .LE. 4) IRADX=1

         END IF
C
C*       Determine stability.
280      KST(IHR) = LSTAB(ISPD,IRADX)

C*       Do not allow stability to vary by more than one category hour-to-hour
         IF (IDAY.EQ.1.AND.IHR.EQ.1) LST=KST(IHR)
         IF ((KST(IHR)-LST).GT.1) KST(IHR)=LST+1
         IF ((LST-KST(IHR)).GT.1) KST(IHR)=LST-1
         LST=KST(IHR)

      ENDIF

      IF (KST(IHR) .LT. 1) THEN
1042     CALL ERRHDL('E','1042',1)
      END IF

      RETURN
      END


      SUBROUTINE MIXHIT(IHR)
C***********************************************************************
C*    MIXHIT      Module of PCRAMMET Meteorological Pre-processor
C*
C*    PURPOSE: This Routine Calculates Hourly Mixing Heights
C*             By Interpolating Between the Twice Daily Values
C*
C*    PROGRAMMER: Jayant Hardikar
C*                PES Inc.
C*
C*    DATE:       February 23, 1995
C*
C*    INPUTS:     Twice-daily mixing heights, sunrise and sunset,
C*                P-G category
C*
C*    OUTPUTS:    Mixing height for the hour
C*
C*    CALLED FROM: MAIN program
C***********************************************************************
C*
C*    Variable Declarations
      INCLUDE 'PCRAM.INC'

C*    Fill Local Variables With Common Block Data, Change Integer
C*    to Real ...

      XHR=IHR

C*    ...Previous Day - AM and PM
      XMNM1 = AMMIX(1)
      XAFM1 = PMMIX(1)

C*    ...Current Day  - AM and PM
      XMN = AMMIX(2)
      XAF = PMMIX(2)

C*    ...Next Day     - AM and PM
      XMNP1 = AMMIX(3)
      XAFP1 = PMMIX(3)

C*    CALCULATE MIXING HEIGHT
      IF ( IHR.GT.14  .AND.  XHR.LE.SUNSET ) GO TO 300
      IND=2
      IF (XHR.LE.SUNSET) GO TO 310
      IF (KST(IHR).EQ.4) GO TO 290
      HLH(2,IHR) = XAF + (XMNP1-XAF) * ((XHR-SUNSET)/(24.0-SUNSET))
      IND=1
290   HLH(IND,IHR) = XAF + (XAFP1-XAF) * ((XHR-SUNSET)/(38.0-SUNSET))
      IF (IND.EQ.2) HLH(1,IHR)=HLH(2,IHR)
      GO TO 360

300   HLH(1,IHR) = XAF
      HLH(2,IHR) = XAF
      GO TO 360

310   IF (XHR.GT.SUNRIS) GO TO 330
      KSTSP=KST(IHR)
      IF (KST(IHR).EQ.4) GO TO 320
      HLH(2,IHR)=XMN
      IND=1

320   HLH(IND,IHR) = XAFM1 + (XAF-XAFM1) *
     &               ((24.0-SUNSET+XHR) / (24.0-SUNSET+14.0))
      IF (IND.EQ.2) HLH(1,IHR)=HLH(2,IHR)
      GO TO 360

330   IF (KSTSP.EQ.4) GO TO 350
      HLH(2,IHR) = XMN + (XAF-XMN) * ((XHR-SUNRIS)/(14.0-SUNRIS))
      HLH(1,IHR) = XAF * (XHR-SUNRIS)/(14.0-SUNRIS)
      GO TO 360

350   HLH(1,IHR) = XAFM1 + (XAF-XAFM1) *
     &             ((24.0-SUNSET+XHR)/(24.0-SUNSET+14.0))
      HLH(2,IHR) = HLH(1,IHR)

360   IF( HLH(1,IHR) .LT. 10.0 )THEN
         WRITE( IDIAG,550 ) HLH(1,IHR), ISYR(1),ISMO(1),ISDY(1),IHR
         NOWARN = .FALSE.
      ENDIF

      IF( HLH(2,IHR) .LT. 10.0 )THEN
         WRITE( IDIAG,555 ) HLH(2,IHR), ISYR(1),ISMO(1),ISDY(1),IHR
         NOWARN = .FALSE.
      ENDIF

      RETURN

  550 FORMAT( ' Rural Mixing Height < 10 m (', F5.1,
     &        ') for (yy/mm/dd/hh) ', 4(i2.2:,'/') )
  555 FORMAT( ' Urban Mixing Height < 10 m (', F5.1,
     &        ') for (yy/mm/dd/hh) ', 4(i2.2:,'/') )

      END


      SUBROUTINE USTARL(IHR)
C***********************************************************************
C*    USTARL      Module of PCRAMMET Meteorological Pre-processor
C*
C*    PURPOSE: This Routine Calculates Surface Friction Velocity
C*             And Monin Obukhov Length for he Current Hour
C*
C*    PROGRAMMER: Jayant Hardikar
C*                PES Inc.
C*                Adapted from DEPMET
C*
C*    DATE:       February 23, 1995
C*
C*    INPUTS:     Hourly temperature, wind speed, cloud cover,
C*                roughness length (measurement and applied site),
C*                sunrise and sunset
C*
C*    OUTPUTS:    Friction velocity and Obukhov length at the
C*                application site
C*
C*    CALLED FROM: MAIN program
C***********************************************************************
C*
C*    Variable Declarations
      INCLUDE 'PCRAM.INC'


C*    Trap ISCST2 "calms" and return u*=L=0.0
      IF(CALM(IHR)) THEN
         USTAR(IHR)  = 0.0
         XMONIN(IHR) = 0.0

      ELSEIF( TEMP(IHR) .LT. 900.0   .AND.  WSPEED(IHR) .LT. 90.0 .AND.
     &        FCOVER(IHR) .LE. 1.0 )THEN
C*       Sufficient data to make computations

C*       Adjust anemometer height for the displacement height at the
C        site of the measurements and the application site
C        NOTE: ZDMEAS is the displacement height,
C              which is inactive (defaulted to 0.0) at this time
         ANEMDM = ANEMHT-ZDMEAS

C*       Compute incomming solar radiation flux (W/m**2) at the
C        measurement site
         CALL RADSOL(IHR)

C*       Estimate net radiation at the measurement site
         CALL NETRAD(IHR)

C*       Estimate the sensible heat flux (SHF) in W/m**2 at the
C        measurement site
         CALL HEAT(IHR)
c
C*       Make sure that net radiation and surface heat flux are
C*       positive during the main part of the day.
         IF(FLOAT(IHR) .GT. SUNRIS+2.0  .AND.
     &      FLOAT(IHR) .LT. SUNSET-2.0) THEN
            IF(RNET(IHR) .LE. 0.) RNET(IHR) = 0.001
            IF(SHF(IHR)  .LE. 0.) SHF(IHR)  = 0.001
         ENDIF

C*       Compute nighttime u* and L using the Weil - Brower method; or
C*       compute daytime u* and L using the Holtslag - van Ulden method.
         IF(FLOAT(IHR) .GT .SUNSET+1.0  .OR.
     2      FLOAT(IHR) .LT. SUNRIS      .OR. RNET(IHR) .LE. 0.) THEN
            CALL USLNIT( IHR,ANEMDM,Z0MEAS,USTARM,XMOLM )

         ELSE
            IF(SHF(IHR) .GT. 0.0) THEN
               CALL USLDAY( IHR,ANEMDM,Z0MEAS,USTARM,XMOLM )
            ELSE
               CALL USLNIT( IHR,ANEMDM,Z0MEAS,USTARM,XMOLM )
            END IF
         END IF

C*       Compute the friction velocity (USTARA) and Monin-Obukhov
C        length (XMOLA) at the application site
         CALL USTAR2( ANEMDM,WSPEED(IHR), USTARM,XMOLM,
     &                Z0APPL,USTARA,XMOLA,ELMINM )

C*       Save these values for writing to the output file
         USTAR(IHR)  = USTARA
         XMONIN(IHR) = XMOLA

C*       Make sure that |L| < 999.0 to comply with format of output
         IF(XMONIN(IHR) .LT. -999.0) THEN
            XMONIN(IHR) = -999.0
         ELSEIF(XMONIN(IHR) .GT. 9999.0) THEN
            XMONIN(IHR) = 9999.0
         ENDIF

      ELSE
C*       Not a calm hour but there is insufficient data to make the
C*       computations - set USTAR and XMONIN to missing
         USTAR(IHR)  = -9.0
         XMONIN(IHR) = -999.0

      ENDIF

      RETURN
      END


      SUBROUTINE RADSOL(IHR)
C======================================================================
C     Purpose:  To compute the hourly total incoming solar radiation
C
C     Input:    Solar elevation, cloud cover
C
C     Output:   Insolation
C
C     Called by: USTARL
C-----------------------------------------------------------------------
      INTEGER IHR
      REAL AVGANG
      INCLUDE 'PCRAM.INC'
C
C---- First, compute the average of solar elevation angles at the
C     beginning and end of the hour.  Check for the 24th hour.
C     Note that the angles are in DEGREES - convert to radians
C
      IF (IHR.LE.23) THEN
         AVGANG = (SOLANG(IHR)+SOLANG(IHR+1))/2.0
      ELSE
         AVGANG = (SOLANG(24)+SOLANG(1))/2.0
      ENDIF

      AVGANG = AVGANG / CONST
C
C---- AVGANG can be no less than 10 degrees to avoid problems at low
C     latitudes
C     141.91 W/M**2 = incoming radiation for a 10 degree elevation angle
C     10 degrees = 0.174533 radians
C
      IF ( (AVGANG .LT. 0.174533)  .AND.  (AVGANG .GT. 0.0) )THEN
         RSOL(IHR) = 141.91 * AVGANG * 5.729578

      ELSEIF( AVGANG .LT. 0.0 )THEN
         RSOL(IHR) = 0.0

      ELSE
         RSOL(IHR)=  990.0*SIN(AVGANG) - 30.0

      ENDIF
C
C---- Finally, include a correction for the cloud cover fraction (CCVR).
C     Note that if cloud cover is missing, then CCVR is set to 0.0
C
      IF( FCOVER(IHR) .GT. 0.0 )THEN
         RSOL(IHR) = RSOL(IHR) * (1.0 - 0.75*(FCOVER(IHR)**3.4))
      ENDIF

      RETURN
      END


      SUBROUTINE NETRAD(IHR)
C====================================================================**
C     Purpose: To compute the net radiation (RN) from solar elevation
C              angle, incoming solar radiation, albedo, and temperature.
C              The equation for the net radiation is:
C
C     Input:   Insolation, solar elevation, albedo, temperature,
C              cloud cover, anthropogenic heat flux
C
C     Output:  Net radiation
C
C     Called from: USTARL
C-----------------------------------------------------------------------
      INTEGER IHR
      REAL C1,STEFB
      INCLUDE 'pcram.inc'
C
C---- Constants used in the computation
C
      C1 = 5.31E-13
      C2 = 60.0
      C3 = 1.12
      STEFB = 5.67E-08
C
C---- Adjust the albedo for the hour of the day; sun's elevation is
C     expressed in degrees in PCRAMMET
      ANGLE = SOLANG(IHR)
      B1 = 1.0 - ALBEDO
      IF( SOLANG(IHR) .LE. 0.0 )THEN
         ALBADJ = 1.0
      ELSE
         ALBADJ = ALBEDO + B1 * EXP(-0.1 * ANGLE + (-0.5 * B1 * B1) )
      ENDIF


C---- Compute the net radiation from solar insolation, albedo adjusted
C     for the hour of day, surface temperature and cloud fraction.
C
      RNET(IHR) = ( (1.0-ALBADJ)*RSOL(IHR) + C1*(TEMP(IHR)**6) -
     &               STEFB*(TEMP(IHR)**4)  + C2*FCOVER(IHR) ) / C3

C---- Add the anthropogenioc heat flux
      RNET(IHR) = RNET(IHR) + QF

      RETURN
      END


      SUBROUTINE HEAT(IHR)
C======================================================================
C     Purpose:  To calculate the heat flux, SHF, from the net radiation
C               and Bowen ratio.  The equation for the heat flux is:
C               (CONSTANT*NET RADIATION) / (1+(1/BOWEN RATIO))
C
C     Input:    Net radiation, Bowen ratio, fraction of net radiation
C               absorbed at the ground (CSUBG)
C
C     Output:   Sensible heat flux
C
C     Called from: USTARL
C-----------------------------------------------------------------------
      INTEGER IHR
      INCLUDE 'PCRAM.INC'

C---- Calculate the HEAT FLUX
C
      SHF(IHR) = ( (1.0-CSUBG) * RNET(IHR) ) / ( 1.0 + (1.0/BOWEN) )

      RETURN
      END


      SUBROUTINE USLDAY( IHR,ANEMZD,Z0M,USTARM,XMOLM )
C=======================================================================
C          USLDAY Module of PCRAMMET Meteorological Preprocessor
C
C   Purpose:  This subroutine will set up the iteration needed to
C             calculate the friction velocity during unstable
C             atmospheric cases (SHF > 0.0.) at the measurement site
C
C   Input:
C        IHR       Hour of day
C        ANEMZD    The anemometer height (adjusted for the
C                     displacement height, which is set to 0.0)
C        Z0M       Roughness length at the measurement site
C        SHF       Sensible heat flux
C        WSPEED    Wind speed
C        RHO       Density
C
C   Output:
C        USTARM    Friction velocity at the measurement site
C        XMOLM     Monin-Obukhov length at the measurement site

C
C   Programmed by:   J. Paumier, PES, Inc.               28 April 1995
C      (based on source code for AERMET, the meteorological preprocessor
C       for AERMOD and METPRO, the preprocessor for CTDMPLUS)
C
C   Called from:  USTARL
C-----------------------------------------------------------------------
C
C---- Local variables
C
      INTEGER IHR, ITER
      REAL    PSIZL, PSIZOL, EPS, VONK, LASTL
      REAL    ANEMZD, MU, MU0, CP, PI, GRAV
      REAL    Z0M, USTARM, XMOLM
C
C---- Include common blocks & declarations
C
      INCLUDE 'PCRAM.INC'
C
C---- Define constants.
C       CP   = specific volume of dry air at constant pressure
C       PI   = 3.14159
C       G    = gravitational acceleration (m/sec*sec)
C       VONK = von Karman constant
C       EPS  = convergence criterion (1% here) for Monin-Obukov length
C
      DATA CP/1004.0/, PI/3.14159/, GRAV/9.80655/, VONK/0.4/
      DATA EPS/0.01/
C
C.......................................................................
C---- Make first guess for iteration initialize PSIZL and PSIZOL to zero.
C     Initialize other parameters:
C        ITER = iteration number
C        LASTL =  previous value of the Monin-Obukhov length
C
      LASTL = 0.0
      PSIZL = 0.0
      PSIZOL = 0.0
      ITER = 1
C
C---- Calm conditions for convective boundary layer -
C     checked in the calling routine SUBR.USTARL

C---- Set up iteration loop over Monin-Obukhov length (XMONIN) and
C     friction velocity (USTAR).  NOTE: The first guess for the
C     iteration is with PSIZL and PSIZOL set equal to zero.
C     The next guess is made by reevaluating PSIZL and PSIZOL.

  100 USTARM = VONK * WSPEED(IHR) /
     &           ( ALOG( ANEMZD / Z0M ) - PSIZL + PSIZOL)
      XMOLM = -RHO(IHR) * CP * TEMP(IHR) * (USTARM**3) /
     &              ( VONK * GRAV * SHF(IHR) )

C---- Check loop and stop loop only if within 1% (EPS) of previous value.

      IF( ITER .GT. 1  .AND.
     &    (ABS(XMOLM-LASTL) .LT. ABS(EPS*XMOLM )) ) THEN
         GO TO 200
      ENDIF
C
C---- Save computed M-O length and calculate new MU, MU0,
C     PSIZL and PSIZOL values.
C
      LASTL=XMOLM

      MU = (1.0 - 16.0 * ( ANEMZD / XMOLM ) ) **0.25
      MU0 = (1.0 - 16.0  * ( Z0MEAS / XMOLM ) ) **0.25

      PSIZL = 2.0 * ALOG( (1.0 + MU) / 2.0) +
     &              ALOG( (1.0 + MU * MU) / 2.0) -
     &              2.0 * ATAN(MU) + PI/2.0

      PSIZOL = 2.0 * ALOG( (1.0 + MU0) / 2.0) +
     &               ALOG( (1.0 + MU0 * MU0) / 2.0 ) -
     &               2.0 * ATAN(MU0) + PI/2.0

      ITER = ITER+1
      GO TO 100

200   CONTINUE

      RETURN
      END
C

      SUBROUTINE USLNIT( IHR,ANEMZD,Z0M,USTARM,XMOLM )
C=====================================================================**
C          USLNIT Module of the PCRAMMET Meteorological Preprocessor
C
C   Purpose:  To calculate the friction velocity and surface heat flux
C             for the SBL at the measurement site.  THETA-STAR is first
C             estimated as a temperature scale for profiling.
C             A check is used to see if
C             if the solution to USTAR will be either real or complex.
C             If the solution is real, the computation for USTAR follows
C             Venkatram's solution.  If the solution is complex, a
C             linear interpolation, used by van Ulden and Holtslag, is
C             applied to find the solution.  To find SHF, USTAR is
C             back-substituted  to find the heat flux.  For strong winds
C             there is a limit for hflux (-64 W/M**2).
C
C             These computations are for the measurement site
C
C   Input:
C        IHR       Hour of day
C        ANEMZD    Anemometer height (adjusted for the
C                  displacement height, which is 0.0)
C        WSPEED    Wind speed
C        Z0M       Roughness length at the measurement site
C        FCOVER    Fractional cloud cover
C        TEMP      Temperature
C        RHO       Density
C        ELMINM    Minimum Obukhov length for stable conditions
C
C   Output:
C        USTARM    Friction velocity
C        XMOLM     Obukhov length
C
C   Called from: USTARL
C-----------------------------------------------------------------------

      INTEGER  IHR
      REAL     THSTAR,AA,BB,CC,CDN,UNOT,VONK,GRAV
      REAL     CP, CHEK, UCR, USTCR, BETAM, HLIM, XLIMIT
      REAL     Z0M, USTARM, XMOLM

      INCLUDE 'PCRAM.INC'

C     VONK = von Karman's constant
C     GRAV = Acceleration due to gravity

      DATA VONK/0.4/,  GRAV/9.80655/

C---- Here, BETAM is a constant used for profile relationships in the
C     SBL, CP is the specific heat capacity of dry air
      CP = 1004.0
      BETAM = 4.7

C---- Calm wind conditions for stable boundary layer.
C     already checked in the calling routine SUBR.USTARL

C---- Equations for CDN, THSTAR, UNOT

      CDN = VONK /( ALOG(ANEMZD/Z0M) )
      THSTAR = 0.09 * (1.0 - 0.5*( FCOVER(IHR)**2) )
      UNOT = SQRT( (BETAM * ANEMHT * GRAV * THSTAR)/TEMP(IHR) )

C---- Check to see if USTAR has a real or complex solution by
C     checking the square root part (CHEK) of the USTAR equation.

      CHEK = ( ( 2.0 * UNOT) / (SQRT( CDN ) * WSPEED(IHR) ) )**2

C---- If there is a complex part to USTAR, a subroutine is called that
C     uses VAN ULDEN AND HOLTSLAG linear interpolation method to
C     compute USTAR.  This interpolation is used when the variable
C     CHEK is greater than 1.0.

C---- If the solution to USTAR is real, go ahead with computation
C     of USTAR, SHF and XMONIN.  The solution is real if "CHEK" is
C     less than or equal to 1.0.  For the case of strong winds,
C     the product USTAR*THSTAR, and thus SHF, may become
C     unrealistically large.  To avoid this, a limit of -64 W/m**2
C     is set for heat flux (SHF)

      IF( CHEK.LE.1.0 )THEN
         USTARM = (CDN * WSPEED(IHR) / 2.0) * (1.0 + SQRT(1.0-CHEK))
      ELSE
         UCR = (2.0 * UNOT) / SQRT(CDN)
         USTCR = CDN * UCR / 2.0
         USTARM = USTCR * WSPEED(IHR) / UCR
      ENDIF

C---- Compare the heat flux (SHF) to the limiting value; if it is too
C     large, then recompute the friction velocity.

      HLIM = -64.0
      XLIMIT = -HLIM / (RHO(IHR)*CP)
      IF( (USTARM * THSTAR) .GT. XLIMIT )THEN
         AA = -CDN * WSPEED(IHR)
         BB = 0.0
         CC = BETAM * ANEMHT * GRAV * XLIMIT * CDN / TEMP(IHR)
         CALL CUBIC ( AA, BB, CC, USTARM )
         THSTAR = XLIMIT / USTARM
      ENDIF
C
C---- Calculate the M-O length and friction velocity;
C     check the value of the Monin-Obukov length against the user-
C     specified minimum, setting XMONIN to the minimum and recomputing
C     USTAR if necessary; compute the heat flux.
C
      XMOLM   = TEMP(IHR) * USTARM * USTARM / ( VONK * GRAV * THSTAR )
      IF( XMOLM .LT. ELMINM )THEN
         XMOLM = ELMINM
         USTARM = SQRT(XMOLM * VONK * GRAV * THSTAR / TEMP(IHR) )
      ENDIF

      SHF(IHR) = -THSTAR * RHO(IHR) * CP * USTARM

      RETURN
      END
C

      SUBROUTINE CUBIC( A,B,C,Z )
C=====================================================================**
C
C   Purpose:  To solve a cubic equation to get the proper solution for
C             the friction velocity (UST), where the cubic equation in
C             this case looks similar to: Z**3 + A*Z**2 + B*Z + C = 0.
C
C   Input:
C        A         Real      Coefficient of UST**2
C        B         Real      Coefficient of UST**1
C        C         Real      Coefficient of UST**0
C
C   Output:
C        Z         Real      New friction velocity
C
C   Called from: USLNIT
C-----------------------------------------------------------------------
C
C---- Solve for one root of the cubic equation.
C
       DOUBLE PRECISION CM,SGN,A3,AP,BP,AP3,TROOT,BP2,APP,BSV
       DOUBLE PRECISION ALPHA,TR,BPP,ONE
       REAL A,B,C,Z
       DATA ONE/1.0/

       A3=A/3.
       AP=B-A*A3
       BP=2.*A3**3-A3*B+C
       AP3=AP/3.
       BP2=BP/2.
       TROOT=BP2*BP2+AP3*AP3*AP3
       IF(TROOT.LE.0.0) GO TO 50
       TR=DSQRT(TROOT)
       APP=(-BP2+TR)**0.333333
       BSV=-BP2-TR
       IF(BSV.EQ.0.0)GO TO 45
       SGN=DSIGN(ONE,BSV)
       BPP=SGN*(DABS(BSV))**0.333333
       Z=APP+BPP-A3
       RETURN

45     CONTINUE
       Z=APP-A3
       RETURN
C
50     CM=2.*DSQRT(-AP3)
       ALPHA=DACOS(BP/(AP3*CM))/3.
       Z=CM*DCOS(ALPHA)-A3
       RETURN
       END


      SUBROUTINE USTAR2(ZREF,UREF, US1,OBL1, Z02,US2,OBL2,ELMINM)
C=====================================================================**
C
C     Purpose:  To compute the friction velocity and Monin-Obukhov length
C               at the application site using equations from Atmospheric
C               Environment Tech Note, 1979 P 191-194
C
C               An iterative process is used and the solution is found
C               when the two consecutive values of the friction velocity
C               are within 1% of each other
C
C      Input:   Wind speed, anemometer height, friction velocity and
C               Obukhov length for the meadurement site, minimum Obukhov
C               length for stable conditions, roughness length at the
C               application site
C
C      Output:  Friction velocity and Obukhov length at the application
C               site
C
C      Called from: USTARL
C-----------------------------------------------------------------------
C
      DATA VONKAR/0.4/

      ITER = 0
      USOLD = US1
      OBL = OBL1
10    CONTINUE

C     Check the number of interations; use the last guess if the
C     number exceeds 100
      ITER = ITER + 1
      IF( ITER .GT. 100) THEN
         US2 = USNEW
         OBL2 = OBL
         RETURN
      ENDIF

C     Compute the stability correction for similarity laws
      IF( OBL .GE. 0.0 )THEN
         PSI = 4.7*ZREF/OBL

      ELSE
         GM1 = (1.0 - 16.0*ZREF/OBL)**0.25
         GM2 = (1.0 - 16.0*Z02/OBL)**0.25
         TA  = (GM2*GM2+1)*(GM2+1)*(GM2+1)
         TB  = (GM1*GM1+1)*(GM1+1)*(GM1+1)
         T2  = TA/TB
         ARG2 = ALOG(T2)
         ARG3 = 2.0 * (ATAN(GM1) - ATAN(GM2) )
         PSI  = ARG2 + ARG3

      END IF

      USNEW  = SQRT( (VONKAR * UREF * US1 ) /( ALOG(ZREF/Z02) + PSI ) )

C     Iterate on friction velocity to account for new roughness length
      IF( ABS( (USNEW-USOLD)/USNEW) .LT. 0.01 )THEN
         US2 = USNEW
         OBL2 = OBL
         GO TO 100

      ELSE
         OBL = OBL*(USNEW*USNEW*USNEW)/(USOLD*USOLD*USOLD)
         IF( OBL .GT. 0.0  .AND.  OBL .LT. ELMINM )THEN
            OBL = ELMINM
         ELSEIF( OBL .LT. 0.0  .AND.  OBL .GT. -1.0 )THEN
            OBL = -1.0
         ENDIF
         USOLD = USNEW
         GO TO 10
      END IF

100   RETURN
      END
