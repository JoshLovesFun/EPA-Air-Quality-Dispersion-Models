      SUBROUTINE SUBST ( IH,TEST )
C===================================================================**
C
C   Purpose:  To position onsite and/or National Weather Service (NWS)
C      data into the appropriate arrays for use in the computations
C      that follow.  The reference height for winds, ZREF, and
C      temperature, ZTREF, are determined from the data.
C
C   Arguments passed:     IH       Integer    Hour
C   Arguments returned:   TEST     Integer    Status check on data
C                                                0 = valid data
C                                                1 = some data missing
C
C
C   Programmer: Jim Paumier, PES, Inc.                28 April 1995
C
C   Adapted from: AERMET, the meteorological preprocessor for AERMOD
C
C---------------------------------------------------------------------
C---- Declaration of variables and INCLUDE statements
C
      INTEGER IZTMP, IZREF, IH, TEST, PCOUNT
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'UA1.INC'
      INCLUDE 'UA2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'MP1.INC'
      INCLUDE 'WORK1.INC'
C---------------------------------------------------------------------
C
C---- Initialize variables
C
      DATA    PCOUNT /0/
      PATH = 'MP'
      LOC = ' SUBST'

C     PCOUNT   Number of times the default value for station pressure is
C              used; if it exceeds 500, then MPRM stops writing the
C              warning message

C                              TEMPERATURE
C     ================================================================
C     Use TMPLVL to determine if on-site temperature should be used
C     in the computations
C     ================================================================
C
      IF ( TMPLVL .LE. 0 ) THEN
C
C------- Retrieve the NWS temperature and convert to kelvins (recall
C          that it was multiplied by 10)
C

         IF ( SFOBS(IH,46) .NE. SFQA(46,2) ) THEN
            AMBT(IH)  = FLOAT (SFOBS(IH,46)) / 10.0 + 273.15
            ZTREF(IH) = TMPHGT
         ELSE
C
C---------- NWS temperature is missing, set the 'no data' flag
C
            TEST  = 1
            MESS  = BLNK40
            ECODE = '   '
            WRITE(MESS,105) MPYR,MPJDY,IH
105         FORMAT(' NWS TEMP. MISSING ON ', I2.2,I3.3,'/',I2.2)
            CALL ERROR ( MPJDY,PATH,ECODE,LOC,MESS )
         ENDIF
C
      ELSEIF( TMPLVL .GT. 0 )THEN

         IF( OSVOBS(IH,TMPLVL,7) .NE. SFQA(21,2)  .AND.
     &       OSHT(TMPLVL) .NE. SFQA(15,2) ) THEN
C---------- The temperature and height at this level aren't missing;
C           convert T to kelvins
            AMBT(IH)  = OSVOBS(IH,TMPLVL,7) + 273.15
            ZTREF(IH) = OSHT(TMPLVL)

         ELSE                                                           JOP 7/22/96
C--------- The on-site temperature is missing; set the flag to 1.        |
           TEST = 1                                                      |
           MESS  = BLNK40                                                |
           ECODE = 'W78'                                                 |
           WRITE ( MESS,109 ) MPYR,MPJDY,IH                              |
109        FORMAT (' ON-SITE TEMP MISSING  ON ',I2.2,I3.3,'/',I2.2)      |
           CALL ERROR ( MPJDY,PATH,ECODE,LOC,MESS )                     JOP 7/22/96

         END IF
C

      ELSE
          PRINT *, '  SUBST: SHOULD NOT GET HERE FOR TMPLVL'
          
      ENDIF

C
C                                WINDS
C     ================================================================
C     Use KEYLVL to determine if on-site winds should be used
C     in the computations
C     ================================================================

      IF( KEYLVL .LE. 0 )THEN

C------- Retrieve the NWS wind speed; recall it was multiplied by 10
C          (INSTHT(1) is the user-specified anemometer height)
C
         IF ( SFOBS(IH,51) .NE. SFQA(51,2) ) THEN
            WSPEED(IH) = FLOAT (SFOBS(IH,51)) / 10.0
            ZREF(IH)   = ANEHGT
C
         ELSE
C
C---------- NWS wind speed missing, set the 'no data' flag
C
            TEST  = 1
            MESS  = BLNK40
            ECODE = '   '
            WRITE ( MESS, 110 ) MPYR,MPJDY,HR
110         FORMAT (' NWS WIND SPD MISSING ON ',I2.2,I3.3,'/',I2.2)
            CALL ERROR ( MPJDY,PATH,ECODE,LOC,MESS )

         ENDIF
C
C------- Retrieve NWS wind direction (recall it was multiplied by 10)
C
         IF ( ( SFOBS(IH,50) .NE. SFQA(50,2) ) ) THEN
            WDIRN(IH)  = FLOAT (SFOBS(IH,50)) * 10.0
C
C---------- Randomize the NWS wind direction and be sure the
C           direction remains < 360
C
            IF( WDIRN(IH) .GT. 0.0 ) THEN
               WDIRN(IH) = WDIRN(IH) + IRND(IH,MPJDY) - 4.0
               IF (WDIRN(IH) .GT. 360.0) THEN
                  WDIRN(IH) = WDIRN(IH) - 360.0
               ELSE IF (WDIRN(IH) .LT. 0.0) THEN
                  WDIRN(IH) = WDIRN(IH) + 360.0
               END IF
            ENDIF
C
C---------- Get the surface characteristics (roughness length,
C             albedo, and Bowen ratio) corresponding to the
C             wind direction
C
            CALL SFCCH ( IH,WDIRN(IH),TEST )
C
         ELSE
C
C---------- NWS wind direction missing, set the 'no data' flag
C
            TEST  = 1
            MESS  = BLNK40
            ECODE = 'W78'
            WRITE ( MESS,115 ) MPYR,MPJDY,IH
115         FORMAT (' NWS WIND DIR MISSING ON ',I2.2,I3.3,'/',I2.2)
            CALL ERROR ( MPJDY,PATH,ECODE,LOC,MESS )

         ENDIF
C
      ELSEIF( KEYLVL .GT. 0 )THEN

C------- Use the on-site data if it is not missing at KEYLVL;
C        the arrays were initialized to missing, so if the
C        observations are missing, nothing has to be done
         IF ( OSVOBS(IH,KEYLVL,9) .NE. SFQA(23,2)  .AND.
     &        OSVOBS(IH,KEYLVL,8) .NE. SFQA(22,2)  .AND.
     &        OSHT(KEYLVL) .NE. SFQA(15,2) ) THEN
C
C---------- The winds at this level are not missing
            WSPEED(IH)  = OSVOBS(IH,KEYLVL,9)
            WDIRN(IH)   = OSVOBS(IH,KEYLVL,8)
            ZREF(IH)    = OSHT(KEYLVL)

C---------- Get the site surface characteristics corresponding to the
C           wind direction
            CALL SFCCH( IH,WDIRN(IH),TEST )

         ELSE
C--------- One of the required on-site wind variables is missing;
C          set the flag to 1.
           TEST = 1
           MESS  = BLNK40
           ECODE = 'W78'
           WRITE ( MESS,116 ) MPYR,MPJDY,IH
116        FORMAT (' ON-SITE WINDS MISSING  ON ',I2.2,I3.3,'/',I2.2)
           CALL ERROR ( MPJDY,PATH,ECODE,LOC,MESS )

         ENDIF
C
      ELSE
          PRINT *, '  SUBST: SHOULD NOT GET HERE FOR KEYLVL'
                
      ENDIF
C
C---- Retrieve other pertinent information -
C       P = surface pressure - use NWS if onsite is missing   (#17)
C       CCVR = total cloud cover - use NWS only
C       QR   = insolation - use onsite data                   ( #8)
C       RN   = net radiation - use onsite data                ( #7)
C       PPTAMT = liquid preciptiation amount                  ( #6)
C       IPPTYP = precipitation type (liquid takes precedence) (#44)
C

C---- Station pressure - pressure is used to calculate density;
C                        rather than not calculate the boundary
C                        layer parameters for the hour because of
C                        a missing pressure, a default value is used.
      IF ( OSSOBS(IH,17) .NE. FLOAT(SFQA(32,2)) ) THEN
C        Use on-site station pressure
         P(IH) = OSSOBS(IH,17)

      ELSE IF ( SFOBS(IH,32) .NE. SFQA(32,2) ) THEN
C        Use NWS station pressure
         P(IH) = FLOAT (SFOBS(IH,32) ) / 10.0

      ELSE
C        Use a default pressure of 1013.25
         P(IH) = 1013.25
         IF( PCOUNT .LE. 500 )THEN
            PCOUNT = PCOUNT + 1
            MESS  = BLNK40
            ECODE = 'W78'
            WRITE ( MESS,65 ) MPYR,MPJDY,IH
65          FORMAT (' NO PRESSURE; USING 1013.25 ON ',
     &                  I2.2,I3.3,'/',I2.2)
            CALL ERROR ( MPJDY, PATH, ECODE, LOC, MESS )

         ELSEIF( PCOUNT .EQ. 501 )THEN
            PCOUNT = PCOUNT + 1
            MESS  = BLNK40
            ECODE = 'W82'
            WRITE ( MESS,66 )
66          FORMAT ( ' MISSING STN PRESSURE COUNT EXCEEDS 500')
            CALL ERROR ( MPJDY, PATH, ECODE, LOC, MESS )
         ENDIF
      ENDIF
C
C---- Use NWS cloud cover;
      IF ( IWORK1(21) .GT. 0 ) THEN
C          Split the cloud cover concatenated variable TOTAL//OPAQUE
           ITOTAL = SFOBS(IH,34) / 100
           IOPAQ  = SFOBS(IH,34) - ITOTAL*100
C          Check opaque cloud cover first and use it if it's not missing
           IF( IOPAQ .NE. SFQA(34,2) ) THEN
              CCVR(IH) = IOPAQ
           ELSE
              CCVR(IH) = ITOTAL
           ENDIF

      ELSE
         CCVR(IH) = OSTSKY(2)
      ENDIF
C
C---- Insolation and net radiation
      QR(IH) = OSSOBS(IH,7)
      RN(IH) = OSSOBS(IH,8)

C---- Precipitation, as needed for ISCSTWET or ISCGASW model options
      IF( MDSTAT .EQ. 16  .OR.  MDSTAT .EQ. 18 )THEN

C------- Get liquid precip amount from on-site (first choice) or
C        NWS (second choice) and the precipitation type (from NWS).
C        Recall that NWS precipitation amount was multiplied by 1000
C
         IF ( OSSOBS(IH,6) .NE. FLOAT(SFQA(6,2)) ) THEN
            PRECIP(IH) = OSSOBS(IH,6)

         ELSE IF ( SFOBS(IH,52) .NE. SFQA(52,2) ) THEN
            PRECIP(IH) = FLOAT (SFOBS(IH,52) ) / 1000.0

         ELSE
            MESS  = BLNK40
            ECODE = 'W78'
            WRITE ( MESS,70 ) MPYR, MPJDY, IH
70          FORMAT ( ' NO SITE/NWS PRECIP ON ',I2.2,I3.3,'/',I2.2 )
            CALL ERROR ( MPJDY, PATH, ECODE, LOC, MESS )
         ENDIF

C        IPPT1 is the weather phenomenon producing liquid precip;
C        IPPT2 is the weather phenomenon producing frozen precip
         IPPT1 = SFOBS(IH,44)/100.0
         IPPT2 = SFOBS(IH,44) - IPPT1*100

C        Convert the MPRM/3280 codes to match the codes output from PCRAMMET
         IF( IPPT1 .GT. 0  .AND. IPPT1 .LT. 99 ) THEN
            IF( IPPT1 .GT. 30 )THEN
                IPPTYP(IH) = IPPT1 - 20
            ELSE
                IPPTYP(IH) = IPPT1 - 19
            ENDIF

         ELSEIF( IPPT2 .GT. 0  .AND. IPPT2 .LT. 99 ) THEN
            IF( IPPT2 .GE. 90 )THEN
                IPPTYP(IH) = IPPT2 - 53
            ELSEIF( IPPT2 .EQ. 64 )THEN
                IPPTYP(IH) = 41
            ELSEIF( IPPT2 .GE. 50 )THEN
                IPPTYP(IH) = IPPT2 - 22
            ELSE
                IPPTYP(IH) = IPPT2 - 21
            ENDIF

         ELSE
            IPPTYP(IH) = 0
         ENDIF
C     Endif for ISCSTWET model selection
      ENDIF
C
C---- Make sure there are valid (nonmissing) data to make the
C       flux computations
C
      IF ( CCVR(IH) .EQ. OSTSKY(2)  .AND.
     &       RN(IH)   .EQ. SFQA(8,2) )  THEN
         TEST  = 1
         MESS  = BLNK40
         ECODE = 'W74'
         WRITE ( MESS, 75 ) MPYR,MPJDY,IH
75       FORMAT(' CCVR & NET RAD''N MISSING ON ',I2.2,I3.3,'/',I2.2)
         CALL ERROR ( MPJDY,PATH,ECODE,LOC,MESS )
      ENDIF
C
      RETURN
      END


      SUBROUTINE SFCCH( HR,DIRECT,KTEST)
C=====================================================================**
C
C   Purpose:  To determine the wind sector and the direction-dependent
C             site characteristics from the direction passed to the
C             subroutine (DIRECT) for the hour (HR) being processed.
C
C     ARGUMENTS PASSED: HR       INTEGER    Hour
C                       DIRECT   REAL       Value to use in the search -
C                                           can be wind direction or
C                                           flow vector
C                       KTEST    INTEGER    Status check on data
C
C   Modifications:
C        J. Paumier, PES                                   May 15, 1996
C        Added leaf area index 
C
C-----------------------------------------------------------------------
        REAL    DIRECT
        INTEGER ISTAT,JTEST,KTEST,HR,NN,NSEC
        INCLUDE 'MAIN1.INC'
        INCLUDE 'MAIN2.INC'
        INCLUDE 'MP1.INC'
        INCLUDE 'OS1.INC'
        INCLUDE 'OS2.INC'
        INCLUDE 'WORK1.INC'
C
        PATH = 'MP'
        LOC = ' SFCCH'
C
C------ Find wind sector for the given hour: the lower bound is
C       INCLUDED in the sector and the upper bound is EXCLUDED
C       from the sector.  Therefore, we must trap on a sector
C       that with an upper bound of 360.0.
C
        JTEST = 1
        NN = 0
   30   NN = NN + 1
        IF( OSWDS(NN,1).LT.OSWDS(NN,2) )THEN
           IF(DIRECT .GE. OSWDS(NN,1)  .AND.
     &        DIRECT .LT. OSWDS(NN,2)  )THEN
              NSEC=NN
              JTEST=2
              GO TO 50
           ENDIF
C
           IF( OSWDS(NN,2).EQ.360.0 )THEN
              IF(DIRECT .GE. OSWDS(NN,1)   .AND.
     &           DIRECT .LE. OSWDS(NN,2) )  THEN
                 NSEC=NN
                 JTEST=2
                 GO TO 50
              ENDIF
           ENDIF

        ELSE
           IF(DIRECT .LT. OSWDS(NN,1)  .AND.
     &        DIRECT .LT. OSWDS(NN,2)  .OR.
     &        DIRECT .GE. OSWDS(NN,1)  .AND.
     &        DIRECT .GT. OSWDS(NN,2)  )THEN
              NSEC=NN
              JTEST=2
              GO TO 50
           ENDIF

        ENDIF

        IF( NN .LT. OSNWDS) GOTO 30
C
C------ If the program reaches this point, there is an error in the range of
C       wind sectors.
C
        MESS=BLNK40
        ECODE='E72'
        WRITE(MESS,2000) DIRECT, MPYR,MPJDY,HR
 2000   FORMAT(' NO SECTOR FOUND FOR ',F5.1, ' ON ',I2.2,I3.3,'/',I2.2)
        CALL ERROR(MPJDY,PATH,ECODE,LOC,MESS)
        KTEST=1
        RETURN
C
C------ Assign albedo, bowen ratio, and surface roughness length
C       based on the month and wind sector.

C       NOTE: Albedo (ALBEDO), Bowen ratio (BOWEN), roughness length at
C             the measurement site (ZO), Obukhov length (OBUMIN),
C             fraction of heat absorbed by the ground (CSUBG) and
C             anthropogenic heat flux (ANTHRO) are derived from the
C             wind direction (upwind sector).  However, Z0APPL and XLAI
C             are reported for the DOWNWIND sector in the output file.
C             A second call to SUBR.SFCCH (this routine) is made later
C             (after the initial computations are made) using the flow
C             vector to get these values.

   50   ALBEDO(HR) = OSSFC(MPCMO,NSEC,1)
        BOWEN(HR)  = OSSFC(MPCMO,NSEC,2)
        ZO(HR)     = OSSFC(MPCMO,NSEC,3)
        Z0APPL(HR) = OSSFC(MPCMO,NSEC,4)
        OBUMIN(HR) = OSSFC(MPCMO,NSEC,5)
        CSUBG(HR)  = OSSFC(MPCMO,NSEC,6)
        ANTHRO(HR) = OSSFC(MPCMO,NSEC,7)
        XLAI(HR)   = OSSFC(MPCMO,NSEC,8)

        RETURN
        END
