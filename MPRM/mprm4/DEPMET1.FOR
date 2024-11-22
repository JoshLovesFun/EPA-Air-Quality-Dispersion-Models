      SUBROUTINE DEPMET( TEST )
C=======================================================================
C
C   Purpose:
C     This subroutine computes the surface friction velocity (USTAR),
C     heat flux (HFLUX), for the convective boundary layer and
C     stable boundary layer for each hour of each day using solar
C     elevation angle, surface temperature, temperature profiles,
C     bowen ratio, cloud cover fraction, wind speed and wind direction.
C
C     For the convective boundary layer (CBL), the incoming solar
C     radiation is computed from the solar elevation angle and corrected
C     for cloud cover.  From this and the Bowen ratio representative of
C     surrounding environment, the heat flux is determined (Holtslag and
C     van Ulden).  Next, an iterative procedure is used to obtain USTAR
C     and the Monin-Obukhov length (MOL).
C
C     For the stable boundary layer (SBL), a THETA-STAR is calculated
C     from cloud cover (van Ulden and Holtslag), where THETA-STAR
C     is the temperature scale used for profiling.  The routine
C     determines whether the solution for USTAR will be real- or
C     complex-valued.  If it is a real solution, the Venkatram solution
C     is used.  A linear interpolation developed by van Ulden and
C     Holtslag is used for complex-valued solutions (a critical USTAR
C     is defined by the variable CHEK).  The friction velocity is then
C     found and is substituted into the heat flux formula to find the
C     surface heat flux (HFLUX).
C        
C     Throughout the routines, computations are made based on the
C     availability of the meteorological data.  A flag (ITEST) is used
C     for this purpose:
C        ITEST=0 : no data missing
C        ITEST=1 : surface variable(s) missing
C
C   Adapted from: AERMET, the meteorological preprocessor for AERMOD
C
C-----------------------------------------------------------------------

      INTEGER IHR,ITEST(24),IW,JULIAN

      REAL    B1, ANGLE, G, CP

      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'UA1.INC'
      INCLUDE 'UA2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'WORK1.INC'

C.......................................................................

      PATH = 'MP'
      LOC  = 'DEPMET'

C---- Initalize the arrays for this day's processing

      DO 5 IW=1,24
         HFLUX(IW)  = -999.0
         MOL(IW)    = -99999.0
         USTAR(IW)  = -9.0
         BOWEN(IW)  = -9.0
         ALBEDO(IW) = -9.0
         XLAI(IW)   = -9.0
         ZO(IW)     = -9.0
         WDIRN(IW)  = -9.0
         AMBT(IW)   = -999.0
         WSPEED(IW) = -9.0
    5 CONTINUE

C---- If NWS and OS both missing, write a warning message and return.
C     IWORK1(21) = # of hours during the day for which there are
C                  NWS surface data (both nonmissing and missing)
C     IWORK1(22) = # of hours during the day for which there are
C                  onsite data (both nonmissing and missing)

      IF( (IWORK1(21) .EQ. 0) .AND. (IWORK1(22) .EQ. 0) )THEN
         MESS=BLNK40
         ECODE='W71'
         WRITE(MESS,10) MPYR,MPJDY
10       FORMAT(' NO DATA TO COMPUTE PARAMETERS ON ',I2.2,I3.3 )
         CALL ERROR(MPJDY,PATH,ECODE,LOC,MESS)
         TEST = 1
         RETURN
      ENDIF

C---- Initialize constants
C     CP = specific heat of air at constant pressure
C     G  = acceleration due to gravity (m/(sec*sec))

      CP = 1004.0
      G  = 9.8

C---- START the hour loop

      DO 100 IHR=1,24
         ITEST(IHR) = 0

C------- Call routine that searches for data in the merged-data file
C        for the meteorological processing to follow.  Also, assign
C        surface characteristics for the hour.             ---- CALL SUBST
 
         CALL SUBST( IHR,ITEST(IHR) )

C------- If ITEST=1 (missing sfc data), go to the next hour.
C        ITEST is reset to 3 if this is a "daytime" hour

         IF( ITEST(IHR) .EQ. 1 )THEN
            MESS=BLNK40
            ECODE='W71'
            WRITE(MESS,55) MPYR,MPJDY,IHR
   55       FORMAT(' NO SFC VARS CALCULATED ON ',I2.2,I3.3,'/',I2.2)
            CALL ERROR(MPJDY,PATH,ECODE,LOC,MESS)
            GOTO 100
         ENDIF

C------- Calculating albedo: if the solar elevation angle is less than
C        zero (at night), then albedo equals one, else albedo is
C        determined from the solar angle and noon-time albedo for the
C        wind sector
       
         B1 = 1.0 - ALBEDO(IHR)
         IF( SOLANG(IHR) .LE. 0.0 )THEN
            ALBEDO(IHR) = 1.0
         ELSE
            ALBEDO(IHR) = ALBEDO(IHR) + B1 *
     &                    EXP(-0.1 * SOLANG(IHR) + (-0.5 * B1 * B1) )
         ENDIF
C----- If the albedo is not within bounds, write a warning message.

       IF( (ALBEDO(IHR) .GT. 1.00)  .OR.  (ALBEDO(IHR) .LT. 0.0) )THEN
          MESS  = BLNK40
          ECODE = 'W77'
          WRITE(MESS,60)ALBEDO(IHR), MPYR,MPJDY,IHR
   60     FORMAT(' ALBEDO SUSPECT     (',F6.2,') ON ',
     &             I2.2,I3.3,'/',I2.2)
          CALL ERROR(MPJDY,PATH,ECODE,LOC,MESS)
          IF( (IHR .GT. TSR)  .AND.  (IHR .LT. TSS) )THEN
             ITEST(IHR)=3
          ENDIF
          GOTO 100
       ENDIF

C---- If incoming solar (short-wave) radiation is missing,
C     then compute the incoming solar radiation from solar
C     elevation angle and fractional cloud cover.          ---- CALL INCRAD
 
      IF( QR(IHR) .EQ. SFQA(7,2) )THEN
         CALL INCRAD(IHR)
      ENDIF

C---- If the incoming radiation is unrealistic, write a warning message.

      IF( QR(IHR) .NE. SFQA(7,2) )THEN
         IF( (QR(IHR) .LT. SFQA(7,3) )  .OR.
     &       (QR(IHR) .GT. SFQA(7,4) ) )THEN
            MESS  = BLNK40
            ECODE = 'W76'
            WRITE( MESS,65 ) QR(IHR),MPYR,MPJDY,IHR
   65       FORMAT(' INSOLATION SUSPECT (',F6.0,') ON ',
     &               I2.2,I3.3,'/',I2.2)
            CALL ERROR(MPJDY,PATH,ECODE,LOC,MESS)
         ENDIF
      ENDIF

C---- If the net radiation is missing, compute the net radiation
C        from temperature, albedo, fractional cloud cover,
C        and incoming solar radiation.                     ---- CALL NETRAD

      IF( RN(IHR) .EQ. SFQA(8,2) )THEN
         CALL NETRAD(IHR)
      ENDIF

C---- If the net radiation is not within the QA bounds, then
C     write a warning message.

      IF( (RN(IHR) .LT. SFQA(8,3) ) .OR.
     &    (RN(IHR) .GT. SFQA(8,4) ) )THEN
         MESS  = BLNK40
         ECODE = 'W76'
         WRITE( MESS,70 ) RN(IHR),MPYR,MPJDY,IHR
   70    FORMAT(' NET RAD''N SUSPECT  (',F6.0,') ON ',
     &            I2.2,I3.3,'/',I2.2)
         CALL ERROR(MPJDY,PATH,ECODE,LOC,MESS)
      ENDIF

C---- If the net radiation is less than zero during the day (and not
C     just after sunrise or just before sunset), then write a warning.

      IF( (IHR .GT. TSR+2)  .AND.
     &    (IHR .LT. TSS-2)  .AND.
     &    (RN(IHR) .LE. 0.0) )THEN

         MESS  = BLNK40
         ECODE = 'W76'
         WRITE( MESS,75 ) MPYR,MPJDY,IHR
   75    FORMAT(' NET RAD''N < 0 DURING DAY ON ',I2.2,I3.3,'/',I2.2)
         CALL ERROR(MPJDY,PATH,ECODE,LOC,MESS)
      ENDIF

C---- Calculate density for given hour.                    ---- CALL RHOCAL

      CALL RHOCAL( P(IHR),AMBT(IHR),RHO(IHR) )
      IF( (RHO(IHR) .LT. 0.9)  .OR.  (RHO(IHR) .GT. 1.5) )THEN
         MESS  = BLNK40
         ECODE = 'W79'
         WRITE(MESS,80) RHO(IHR), MPYR,MPJDY,IHR
   80    FORMAT(' DENSITY SUSPECT  (',F7.2,') ON ',I2.2,I3.3,'/',I2.2)
         CALL ERROR(MPJDY,PATH,ECODE,LOC,MESS)
         IF( (IHR .GT. TSR)  .AND.  (IHR .LT. TSS) )THEN
            ITEST(IHR) = 3
         ENDIF
         GOTO 100
      ENDIF

C------- Determine if a stable boundary layer (UCALST) or convective
C        boundary layer (UCALCO) subroutine is needed.  Compute the
C        friction velocity (USTAR), Monin-Obukhov length (L), and heat
C        flux (HFLUX) for the SBL by using Holtslag-van Ulden method.
C        The Venkatram method is used for CBL calculations.
C           RN is net radiation
C           TSS & TSR are the time of sunset and sunrise, resp.
C        These computations are for the measurement site
C
C------- The "HEAT" subroutine computes heat flux for the cbl.

         IF( RN(IHR).LE.0.0  .OR.  IHR.GT.TSS+1.0  .OR.
     &                             IHR.LT.TSR     )THEN
            CALL UCALST(IHR, USTARM, XMOLM)
         ELSE
            CALL HEAT(IHR)
            CALL UCALCO(IHR, USTARM, XMOLM)
         ENDIF

C------- Next, estimate the friction velocity (USTARA) and Monin-Obukhov
C        length (XMOLA) at the application/receptor site.
C
C        At the application/receptor site, the direction from which the
C        wind is blowing is the same as the flow vector for the
C        measurement site.  Therefore, the surface characteristics used
C        to estimate u* and L at the application site are based on the
C        flow vector for the hour.  We also get the leaf area index at
C        the application site (for gas deposition) from this call to SFCCH.
C
         FLOVEC = WDIRN(IHR) + 180.0
         IF( FLOVEC .GT. 360.0 )THEN
            FLOVEC = FLOVEC - 360.0
         ENDIF

         CALL SFCCH( IHR,FLOVEC,TEST )

         CALL USTAR2( ZREF(IHR),WSPEED(IHR), USTARM,XMOLM,
     &                Z0APPL(IHR),USTARA,XMOLA,OBUMIN )

C*       Save these values for writing to the output file
         USTAR(IHR)  = USTARA
         MOL(IHR) = XMOLA


C------- If the friction velocity is unrealistic, write a warning message

         IF( (USTAR(IHR) .LT. SFQA(2,3) )  .OR.
     &       (USTAR(IHR) .GT. SFQA(2,4) ) )THEN
            MESS  = BLNK40
            ECODE = 'W80'
            WRITE( MESS,95 ) USTAR(IHR), MPYR,MPJDY,IHR
   95       FORMAT(' u* SUSPECT (',F8.3,') ON ',I2.2,I3.3,'/',I2.2)
            CALL ERROR(MPJDY,PATH,ECODE,LOC,MESS)
         ENDIF

C------- Check the value of the Monin-Obkuhov length to make sure it is
C        not outside the range of the format field.  Note, too, that a
C        minimum (absolute) value is checked in SUBR.USTAR2
C 
         IF( MOL(IHR) .GT. 9998.0) THEN
            MOL(IHR) = 9998.0
         ENDIF

         IF( (MOL(IHR) .LT. -999.0)  .AND.
     &       (MOL(IHR) .NE. -99999.0) ) THEN
            MOL(IHR) = -998.0
         ENDIF

C------- Continue to next hour's surface data; end of hour loop

  100 CONTINUE

      RETURN
      END


      SUBROUTINE INCRAD(IHR)
C=====================================================================**
C
C     Purpose:  To compute the total incoming solar radiation from
C               fractional cloud cover and solar elevation angle.
C
C   ARGUMENTS PASSED:  IHR    INTEGER    HOUR OF DAY
C
C-----------------------------------------------------------------------
      INTEGER IHR
      REAL    AVGANG
      INCLUDE 'MP1.INC'

C
C---- First, compute solar elevation angle at the the midpoint of the
C     hour.  Check for the 24th hour, using the angle at hour 1 to
C     compute the average angle.
C     SOLANG() is defined in degrees.
C
      IF (IHR.LE.23) THEN
         AVGANG = (SOLANG(IHR)+SOLANG(IHR+1))/2.0
      ELSE
         AVGANG = (SOLANG(24)+SOLANG(1))/2.0
      ENDIF

      AVGANG = AVGANG / RAD2DG
C
C---- Limit AVGANG to 10 degrees to avoid negative values at low
C     solar elevation angles

C     141.91 W/sq.meter = incoming radiation for an elevation angle
C                         of 10 degrees
C     10 degrees = 0.174533 radians
C
      IF ( (AVGANG .LT. 0.174533)  .AND.  (AVGANG .GT. 0.0) )THEN
         QR(IHR) = 141.91 * AVGANG * 5.729578

      ELSEIF( AVGANG .LT. 0.0 )THEN
         QR(IHR) = 0.0

      ELSE
         QR(IHR)=  990.0*SIN(AVGANG) - 30.0

      ENDIF
C
C---- Finally, include a correction for the cloud cover fraction (CCVR).
C     Note that if cloud cover is missing, then no adjustment for cloud
C     cover is made, i.e., the insolation is the clear sky estimate.
C
      IF(CCVR(IHR).GT.0.0) THEN
         QR(IHR) = QR(IHR) * (1.0 - 0.75*((CCVR(IHR)/10.0)**3.4))
      ENDIF

      RETURN
      END


      SUBROUTINE NETRAD(IHR)
C=====================================================================**
C
C     Purpose: To compute the net radiation (RN) from solar elevation
C              angle, incoming solar radiation, albedo, and temperature.
C              The equation for the net radiation is:
C
C              (1-ALBEDO)*(INCOMING SOLAR RADIATION) +
C              CONSTANT1 * (TEMP**6)-(STEFAN-BOLTZMANN'S CONSTANT) *
C              (TEMP**4) + CONSTANT2 * (CLOUD COVER FRACTION/CONSTANT3).
C
C   ARGUMENTS PASSED:  IHR       INTEGER    HOUR OF DAY
C
C-----------------------------------------------------------------------
      INTEGER IHR
      REAL C1,STEFB
      INCLUDE 'MP1.INC'
C
C---- Constants used in the computation
C
      C1 = 5.31E-13
      C2 = 60.0
      C3 = 1.12
      STEFB = 5.67E-08
C
C---- Compute the net radiation from solar insolation, albedo, surface
C     temperature and cloud fraction.
C
      RNET = ((1.0-ALBEDO(IHR))*QR(IHR)+C1*(AMBT(IHR)**6)-STEFB*
     $          (AMBT(IHR)**4) + C2*(CCVR(IHR)/10.0))/C3

C     Add the anthropogenic heat flux.
      RN(IHR) = RNET + ANTHRO(IHR)

      RETURN
      END


      SUBROUTINE HEAT(IHR)
C=====================================================================**
C
C     Purpose: To calculate the heat flux, HFLUX, from the net radiation
C     and Bowen ratio.  The equation for the heat flux is:
C            (CONSTANT*NET RADIATION) / (1+(1/BOWEN RATIO))
C
C     ARGUMENTS PASSED:  IHR     INTEGER   HOUR OF DAY
C
C-----------------------------------------------------------------------
      INTEGER IHR
      INCLUDE 'MP1.INC'

C
C---- Calculate the HEAT FLUX
C
      HFLUX(IHR) = ( (1.0-CSUBG(IHR)) * RN(IHR) ) /
     &               (1.0+(1.0/BOWEN(IHR) ) )

      RETURN
      END


      SUBROUTINE RHOCAL(P1,T1,DEN)
C======================================================================
C
C  PURPOSE: To calculate the density from the ideal gas law equation.
C           The equation for density is:
C                DENSITY = PRESSURE/(GAS CONSTANT*TEMPERATURE)
C
C  ARGUMENTS PASSED:    P1      REAL     PRESSURE
C                       T1      REAL     TEMPERATURE
C  ARGUMENTS RETURNED:  DEN     REAL     DENSITY (kg/cu. meter)
C
C-----------------------------------------------------------------------
      REAL P1,T1,RD,DEN
C
C---- Compute the density, DEN, from pressure(P1) and temperature(T1)
C
      RD = 287.04
      DEN = P1*100.0/(RD*T1)

      RETURN
      END



      SUBROUTINE UCALCO(IHR, USTARM, XMOLM)
C=======================================================================
C                    UCALCO Module of MPRM
C
C   Purpose:
C   To calculate the friction velocity an Monin-Obukhov length for the
C   measurement site during unstable atmospheric conditions when
C   HFLUX > 0.0.  An iterative approach using similarity theory is used.
C   The heat flux, HFLUX, has already been calculated in SUBR.HEAT.
C 
C   Arguments passed:
C        IHR    Integer    Hour of day
C
C   Programmed by:
C      Jeff Vukovich, Computer Sciences Corp.         December 15, 1992
C      (for AERMET 1.00; based on METPRO)
C
C-----------------------------------------------------------------------
C
C---- Local variables
C
      INTEGER IHR, ITER
      REAL    PSIZL, PSIZOL, EPS, VONK, LASTL
      REAL    MU, MU0, CP, PI, G
C
C---- Include common blocks & declarations
C
      INCLUDE 'MP1.INC'
C
C---- Define constants.
C       CP   = specific volume of dry air at constant pressure
C       PI   = 3.14159
C       G    = gravitational acceleration (m/sec*sec)
C       VONK = von Karman constant
C       EPS  = convergence criterion (1% here) for Monin-Obukov length
C
      DATA CP/1004.0/, PI/3.14159/, G/9.80655/, EPS/0.01/, VONK/0.4/
C
C.......................................................................
C
C---- Make first guess for iteration initialize PSIZL and PSIZOL to zero.
C     Initialize other parameters:
C        ITER = iteration number
C        LASTL =  previous value of the Monin-Obukhov length
C     DCOEFF is set in MASTER.INC and was set to 0.0 on 12/9/94 -- jop
C 
      PSIZL = 0.0
      PSIZOL = 0.0
      ITER = 1
      LASTL = 0.0
C
C---- Calm conditions for convective boundary layer.
C
      IF( WSPEED(IHR) .EQ. 0.0 ) THEN
         uspeed = 1.0
      ELSE
         uspeed = wspeed(ihr)
      ENDIF


C---- Set up iteration loop over Monin-Obukhov length (MOL) and
C     friction velocity (USTAR).  NOTE: The first guess for the
C     iteration is with PSIZL and PSIZOL set equal to zero.
C     The next guess is made by reevaluating PSIZL and PSIZOL.

  100 USTARM = VONK * USPEED /
     &             ( ALOG( ZREF(IHR) / ZO(IHR) ) -
     &             PSIZL + PSIZOL)
      XMOLM  = -RHO(IHR) * CP * AMBT(IHR) * (USTARM**3) /
     &            ( VONK * G * HFLUX(IHR) )

C---- Check loop and stop if within 1% (EPS) of previous value.

      IF( ITER .NE. 1 )THEN
        IF( ABS(XMOLM-LASTL) .LT. ABS(EPS*XMOLM) ) GO TO 200
      ENDIF
C      
C---- Calculate new MU, MU0, PSIZL and PSIZOL values.
C      
      MU = (1.0 - 16.0 * (ZREF(IHR) / XMOLM) ) ** 0.25
      MU0 = (1.0 - 16.0  * ( ZO(IHR) / XMOLM) ) **0.25

      PSIZL = 2.0 * ALOG( (1.0 + MU) / 2.0) +
     &              ALOG( (1.0 + MU * MU) / 2.0) -
     &              2.0 * ATAN(MU) + PI/2.0

      PSIZOL = 2.0 * ALOG( (1.0 + MU0) / 2.0) +
     &               ALOG( (1.0 + MU0 * MU0) / 2.0 ) -
     &               2.0 * ATAN(MU0) + PI/2.0

      LASTL = XMOLM

      ITER = ITER+1
Cjop  IF(MOD(ITER,100).EQ.0)WRITE(6,*)'# OF ITERATIONS IS NOW ', ITER
      GO TO 100

200   CONTINUE

      RETURN
      END


      SUBROUTINE UCALST(IHR, USTARM, XMOLM)
C=====================================================================**
C
C   Purpose:  To calculate the friction velocity and surface heat flux
C             for the SBL.  A THETA-STAR is first estimated as a
C             temperature scale for profiling.  A check is used to see if
C             if the solution to USTAR will be either real or complex.
C             If the solution is real, the computation for USTAR follows
C             Venkatram's solution.  If the solution is complex, a
C             linear interpolation, used by van Ulden and Holtslag, is
C             applied to find the solution.  To find HFLUX, USTAR is
C             back-substituted  to find the heat flux.  For strong winds
C             there is a limit for hflux (-64 W/M**2).
C
C   Arguments passed in:
C        IHR       Integer   Hour of day
C                                                                       
C
C   Original programming:  Jeff Vukovich              December 1992
C                          Computer Sciences Corp.
C
C-----------------------------------------------------------------------

      INTEGER  IHR
      REAL     THSTAR,AA,BB,CC,CDN,UNOT,VONK,GRAV
      REAL     CP, CHEK, UCR, USTCR, BETAM, HLIM, XLIMIT
      INCLUDE 'MP1.INC'

C     VONK = von Karman's constant
C     GRAV = Acceleration due to gravity

      PARAMETER (HLIM = -64.0)
      DATA VONK/0.4/,  GRAV/9.80655/

C---- Here, BETAM is a constant used for profile relationships in the
C     SBL, CP is the specific heat capacity of dry air
      CP = 1004.0
      BETAM = 4.7

C---- Calm wind conditions for stable boundary layer.
      IF( WSPEED(IHR) .EQ. 0.0 )THEN
         uspeed = 1.0
      ELSE
         uspeed = wspeed(ihr)
      ENDIF

C---- Equations for CDN, THSTAR, UNOT

      CDN = VONK / ( ALOG(ZREF(IHR)/ZO(IHR)) )
      THSTAR = 0.09 * (1.0 - 0.5*( (CCVR(IHR)/10.0)**2) )
      UNOT = SQRT( (BETAM * ZREF(IHR) * GRAV * THSTAR)/AMBT(IHR) )

C---- Check to see if USTAR has a real or complex solution by
C     checking the square root part (CHEK) of the USTAR equation.

      CHEK = ( ( 2.0 * UNOT) / (SQRT( CDN ) * USPEED ) )**2

C---- If there is a complex part to USTAR, a subroutine is called that
C     uses VAN ULDEN AND HOLTSLAG linear interpolation method to
C     compute USTAR.  This interpolation is used when the variable
C     CHEK is greater than 1.0.


      IF( CHEK.LE.1.0 )THEN
C------- The solution for USTAR is real, go ahead with computation
C        of USTAR and L.
         USTARM = (CDN * USPEED / 2.0) * (1.0 + SQRT(1.0-CHEK))

      ELSE
         UCR = (2.0 * UNOT) / SQRT(CDN)
         USTCR = CDN * UCR / 2.0
         USTARM = USTCR * USPEED / UCR
         THSTAR = THSTAR * USPEED / UCR

      ENDIF

C     For strong winds, the product USTAR*THSTAR, may be unrealistically
C     large.  To avoid this situation, the heat flux (HLIM) is limited to
C     a minimum of -64 Watts/meter**2.
C     Note: XLIMIT is the kinematic heat flux

      XLIMIT = -HLIM / (RHO(IHR)*CP)
      IF( (USTARM * THSTAR) .GT. XLIMIT )THEN
         AA = -CDN * USPEED
         BB = 0.0
         CC = BETAM *ZREF(IHR) * GRAV * XLIMIT * CDN / AMBT(IHR)
         CALL CUBIC ( AA, BB, CC, USTARM )
         THSTAR = XLIMIT / USTARM
      ENDIF
C
C---- Calculate the M-O length and check it against the minimum entered
C     by the user
C
      XMOLM   = AMBT(IHR) * USTARM * USTARM / ( VONK * GRAV * THSTAR )

      IF( XMOLM .LT. OBUMIN(IHR) )THEN
         XMOLM  = OBUMIN(IHR)
         USTARM = SQRT(XMOLM * VONK * GRAV * THSTAR / AMBT(IHR) )
      ENDIF

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
C   Arguments passed in:
C        A         Real      Coefficient of UST**2
C        B         Real      Coefficient of UST**1
C        C         Real      Coefficient of UST**0
C
C   Arguments returned:
C        Z         Real      New friction velocity
C
C-----------------------------------------------------------------------
C
C---- Solve for one root of the cubic equation.
C
       DOUBLE PRECISION CM,SGN,A3,AP,BP,AP3,TROOT,BP2,APP,BSV
       DOUBLE PRECISION ALPHA,SIGN,TR,BPP,ONE
       REAL A,B,C,Z
       DATA ONE/1.0/
  
       A3 = A/3.0
       AP = B - A*A3
       BP = 2.0 * A3**3 - A3*B + C
       AP3 = AP/3.0
       BP2 = BP/2.0
       TROOT = BP2*BP2 + AP3*AP3*AP3
       IF( TROOT.LE.0.0 ) GO TO 50
       TR = DSQRT(TROOT)
       APP = (-BP2 + TR)**0.333333
       BSV = -BP2 - TR
       IF( BSV.EQ.0.0 )GO TO 45
       SGN = DSIGN(ONE,BSV)
       BPP = SGN*(DABS(BSV))**0.333333
       Z = APP + BPP - A3
       RETURN

45     CONTINUE
       Z = APP - A3
       RETURN
C
50     CM = 2.0*DSQRT(-AP3)
       ALPHA = DACOS(BP/(AP3*CM))/3.0
       Z = CM*DCOS(ALPHA) - A3
       RETURN
       END


      SUBROUTINE USTAR2(ZREFU,UREF, US1,OBL1, Z02,US2,OBL2,ELMINM)
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
C     Arguments:
C        Input
C           ZREFU   Reference height for wind speed
C           UREF    Reference height wind speed
C           US1     Friction velocity at the measurement site
C           OBL1    Obukhov length at the measurement site
C           Z02     Roughness length at the application site
C           ELMINM  Minimum Obukhov length for stable conditions
C
C        Output
C           US2     Friction velocity at the application site
C           OBL2    Obukhov length for the application site
C     
C-----------------------------------------------------------------------
C
      DATA VONKAR/0.4/

      ITER = 0
      USOLD = US1
      OBL = OBL1

C---- If the wind speed is calm (= 0.0), set it to 1.0 m/s (as was done
C     in UCALCO and UCALST)
      IF( UREF .EQ. 0.0 )THEN
         UREF = 1.0
      ENDIF

10    CONTINUE
Cdbg  print *,' US2:', ihr, obl,z02,usold

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
         PSI = 4.7*ZREFU/OBL

      ELSE
         GM1 = (1.0 - 16.0*ZREFU/OBL)**0.25
         GM2 = (1.0 - 16.0*Z02/OBL)**0.25
         TA  = (GM2*GM2+1)*(GM2+1)*(GM2+1)
         TB  = (GM1*GM1+1)*(GM1+1)*(GM1+1)
         T2  = TA/TB
         ARG2 = ALOG(T2)
         ARG3 = 2.0 * (ATAN(GM1) - ATAN(GM2) )
         PSI  = ARG2 + ARG3

      END IF

      USNEW  = SQRT( (VONKAR * UREF * US1 ) /( ALOG(ZREFU/Z02) + PSI ))

C     Iterate on friction velocity to account for new roughness length
      IF( ABS( (USNEW-USOLD)/USNEW) .LT. 0.01 )THEN
         US2 = USNEW
         OBL2 = OBL
         GO TO 100

      ELSE
         OBL = OBL*(USNEW*USNEW*USNEW)/(USOLD*USOLD*USOLD)
         USOLD = USNEW
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
