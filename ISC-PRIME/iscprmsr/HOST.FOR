      SUBROUTINE PCALC
C***********************************************************************
C                 PCALC Module of ISC2 Short Term Model - ISCST2
c ----------------------------------------------------------------------
c ---    ISC-PRIME     Version 1.0    Level 970812              Modified
c ---        D. Strimaitis, J. Scire
c ---        Earth Tech, Inc.
c            Prepared for EPRI under contract WO3527-01
c ----------------------------------------------------------------------
C
C        PURPOSE: Calculates concentration or deposition values
C                 for POINT sources
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        MODIFIED BY D. Strimaitis, SRC (for Wet & Dry DEPOSITION)
C        MODIFIED BY D. Strimaitis, SRC (for COMPLEX I -Intermediate
C                                        Terrain Processing)
C
C        DATE:    December 15, 1993
C
C        MODIFIED:   To add call for new source type of OPENPIT.
C                    R. W. Brode, PES - 9/30/94
C
C        MODIFIED BY D. Strimaitis, SRC (for Dry DEPOSITION)
C        (DATE:    February 15, 1993)
C
C        INPUTS:  Source Parameters for Specific Source
C                 Arrays of Receptor Locations
C                 Meteorological Variables for One Hour
C
C        OUTPUTS: 1-hr CONC or DEPOS Values for Each Receptor for
C                 Particular Source
C
C        CALLED FROM:   CALC
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Local Variables
      logical ldbhr
c ***
      data nwrit/0/
c ***

C     Variable Initializations
      MODNAM = 'PCALC'

C     Set the Source Variables for This Source              ---   CALL SETSRC
      CALL SETSRC

C     Apply Variable Emission Rate and Unit Factors         ---   CALL EMFACT
      CALL EMFACT(QS)

C     Set Particle Deposition Variables for this Source
      IF (LDPART .OR. LWPART) THEN
C        Calculate Deposition Velocities for this Source    ---   CALL VDP
         CALL VDP
      END IF

C     Calculate Scavenging Ratios for this Source           ---   CALL SCAVRAT
      IF (LWPART .OR. LWGAS) CALL SCAVRAT

      IF ((QTK.NE.0.0) .AND. (STABLE .OR. (HS.LE.ZI) .OR.
     &                        DEPOS .OR. WDEP)) THEN
C        Calculate Buoyancy and Momentum Fluxes
         IF (TS .LT. TA)  TS = TA
         FB = (0.25/TS)*(VS*DS*DS)*G*(TS-TA)
         FM = (0.25/TS)*(VS*DS*DS)*VS*TA
C        Adjust Wind Speed to Stack Height                  ---   CALL WSADJ
         CALL WSADJ
C        Calculate Distance to Final Rise                   ---   CALL DISTF
         CALL DISTF
C        Set Wake and Building Type Switches                ---   CALL WAKFLG
         CALL WAKFLG
C        Initialize FSTREC Logical Switch for First Receptor of Loop
         FSTREC = .TRUE.
C        Initialize FSTREC Logical Switch for First CMP1 Receptor of Loop
         FSTCMP = .TRUE.
         IF (LDPART .OR. LWPART) THEN
C           Calculate Min Sigma-z for Settled Plume @ Surface --- CALL SETSZMN
            CALL SETSZMN
         END IF
c
c --- PRIME ---------------------------------------------------------
c ---    Setup computations for numerical plume rise algorithm
c ---    and building wake analysis
         if(WAKE) then
c ---       Store selected data in new variables for future reference
            hstack=hs
            ustack=us

c ---       Define vertical profiles of meteorological parameters
c                                                           ---   CALL NUMMET
            ldbhr=.false.
            call NUMMET(uref,zref,p,ta,dtdz,ldbhr)

c ---       Compute wind speed at top of building           ---   CALL WSADJ
            hs=dsbh
            call WSADJ
            ubldg=us
            hs=hstack
            us=ustack

c ---       Refresh /WAKEDAT/ variables                     ---   CALL WAKE_INI
            ldbhr=DEBUG
            call WAKE_INI(ldbhr,kst,rural,dsbh,dsbw,dsbl,
     &                    xadj,yadj,ubldg,ustack)
         endif
c ------------------------------------------------------------

C        Begin Receptor LOOP

         DO 2000 IREC = 1, NUMREC
C           Calculate Down and Crosswind Distances          ---   CALL XYDIST
            CALL XYDIST

c --- PRIME ---------------------------------------------------------
            if(WAKE) then
c ---          Calculate where receptor is relative to near-wake cavity
c              and building (IPOSITN=1 for within bldg; 2=within
c              near-wake, 3=within far wake; 4=outside)
c ---          Note:  xbrec is downwind dist. of receptor from upwind
c              bldg face; ybrec is crosswind dist. of receptor from
c              center of upwind bldg. face                  ---  CALL POSITION
               xbrec=x-xadj
               ybrec=y-yadj
               call POSITION(xbrec,ybrec,zflag,ipositn)
            else
               ipositn=4
            endif
c
            if(ipositn.EQ.4 .AND. ABS(y).GT.1.191754*x) then
C              Receptor is at least 50 deg. off the plume centerline
c ---          and is not within a building wake
               DO ITYP = 1, NUMTYP
                  HRVAL(ITYP) = 0.0
               END DO
            elseif(ipositn.NE.2 .AND. distr.LT.0.99) then
C              Receptor Too Close to Source for Calculation
c ---          and is not within a building near-wake (cavity)
               DO ITYP = 1, NUMTYP
                  HRVAL(ITYP) = 0.0
               END DO
c -------------------------------------------------------------------

            ELSE

CRWB           Modifications to Integrate COMPLEX1 Algorithms.
CRWB           Get Plume Heights and Check for INTERMEDIATE TERRAIN Regime
               IF (NOCMPL) THEN
                  CALL PHEFF(X,DHP,HEFLAT)
                  CALL STERAD(HEFLAT,ZELEV,HE)
                  SIMPLE = .TRUE.
                  INTERM = .FALSE.
                  COMPLX = .FALSE.
C                 Set HECOMP = HEFLAT for later check versus ZI
                  HECOMP = HEFLAT
               ELSE IF (NOSMPL) THEN
C                 Note: Use radial distance, DISTR, for COMPLEX1 plume height.
                  CALL PHEFFC(DISTR,DHPCMP,HECOMP)
                  CALL CTERAD(HECOMP,ZELEV,HECMP1,CORR)
                  COMPLX = .TRUE.
                  INTERM = .FALSE.
                  SIMPLE = .FALSE.
C                 Set HEFLAT = HECOMP for later check versus ZI
                  HEFLAT = HECOMP
               ELSE
                  CALL PHEFF(X,DHP,HEFLAT)
                  CALL STERAD(HEFLAT,ZELEV,HE)
                  IF (ELEV) THEN
                     CALL PHEFFC(DISTR,DHPCMP,HECOMP)
                     CALL CTERAD(HECOMP,ZELEV,HECMP1,CORR)
C                    Set the Simple/Intermediate/Complex Terrain Flags
                     CALL ITSET
                  ELSE
                     SIMPLE = .TRUE.
                     HECOMP = HEFLAT
                  END IF
               END IF

               IF (STABLE .OR. (HEFLAT.LE.ZI) .OR. (HECOMP.LE.ZI) .OR.
     &             DEPOS  .OR.  WDEP) THEN

                  IF (SIMPLE .OR. INTERM) THEN
C                    Determine Simple Terrain Sigmas        ---   CALL PDIS
                     CALL PDIS(X,SY,SZ,XY,XZ,SBID)
                  END IF
                  IF (COMPLX .OR. INTERM) THEN
C                    Determine Complex Terrain Sigmas       ---   CALL PDISC
                     CALL PDISC(DISTR,SZCMP1,XZCMP1,SBCMP1)
                  END IF


C                 Determine Deposition Correction Factors for Gases
                  IF (LWGAS) THEN
C                    Initialize wet source depletion factor to unity.
                     WQCORG = 1.
                     WQCORGC = 1.
                     IF (WDPLETE) THEN
C                       Determine source depletion factor
C                       from wet removal (GASES)
                        IF (SIMPLE .OR. INTERM) THEN
C                          Simple Terrain Model
                           WQCORG = EXP(-GSCVRT*X/US)
                        ENDIF
                        IF (COMPLX .OR. INTERM) THEN
C                          Complex Terrain Model - use radial distance
                           WQCORGC = EXP(-GSCVRT*DISTR/US)
                        ENDIF
                     ENDIF
                  ENDIF

C                 Apply Intermediate Terrain Logic
                  IF (SIMPLE) THEN
C                    Simple Terrain Model Only              ---   CALL PSIMPL
                     CALL PSIMPL
                  ELSE IF (COMPLX) THEN
C                    Complex Terrain Model Only             ---   CALL PCOMPL
                     CALL PCOMPL
                  ELSE IF (INTERM) THEN
C                    Initialize simple and complex terrain holding variables
                     SIMCON = 0.0
                     COMCON = 0.0
                     DO ITYP = 1, NUMTYP
                        SIMPL(ITYP) = 0.
                        COMPL(ITYP) = 0.
                     END DO
C                    Determine Which Model Predicts the Larger Conc.
C                    Save Simple Terrain Conc.           ---   CALL PSIMPL
                     CALL PSIMPL
                     DO ITYP = 1, NUMTYP
                        SIMPL(ITYP) = HRVAL(ITYP)
                     END DO
C                    Save Complex Terrain Conc.          ---   CALL PCOMPL
                     CALL PCOMPL
                     DO ITYP = 1, NUMTYP
                        COMPL(ITYP) = HRVAL(ITYP)
                     END DO
C                    Report Result for Model that Produces the Larger
C                    Concentration
                     IF (SIMCON .GE. COMCON) THEN
                        DO ITYP = 1, NUMTYP
                           HRVAL(ITYP) = SIMPL(ITYP)
                        END DO
                     ELSE
                        DO ITYP = 1, NUMTYP
                           HRVAL(ITYP) = COMPL(ITYP)
                        END DO
                     END IF
                  END IF

C                 Sum HRVAL to AVEVAL and ANNVAL Arrays  ---   CALL SUMVAL
                  CALL SUMVAL

               ELSE
                  DO ITYP = 1, NUMTYP
                     HRVAL(ITYP) = 0.0
                  END DO
               END IF

C              Write DEBUG Information related to Terrain and Removal
               IF (DEBUG) THEN
                  WRITE(IOUNIT,*)
                  WRITE(IOUNIT,*) 'HOUR, RECEPTOR : ',IHOUR,IREC
                  WRITE(IOUNIT,*) 'PCALC: HRVAL(final) = ',HRVAL
                 IF (LDPART .OR. LWPART) THEN
                  WRITE(IOUNIT,*) 'PCALC: Particle Removal --------'
                  WRITE(IOUNIT,*) 'WQCOR  = ',(WQCOR(I),I=1,NPD)
                  WRITE(IOUNIT,*) 'WQCORC = ',(WQCORC(I),I=1,NPD)
                  WRITE(IOUNIT,*) 'DQCOR  = ',(DQCOR(I),I=1,NPD)
                  WRITE(IOUNIT,*) 'DQCORC = ',(DQCORC(I),I=1,NPD)
                  WRITE(IOUNIT,*) 'PCORZD = ',(PCORZD(I),I=1,NPD)
                  WRITE(IOUNIT,*) 'PCORZDC= ',(PCORZDC(I),I=1,NPD)
                  WRITE(IOUNIT,*) 'PCORZR = ',(PCORZR(I),I=1,NPD)
                  WRITE(IOUNIT,*) 'PCORZRC= ',(PCORZRC(I),I=1,NPD)
                  WRITE(IOUNIT,*) 'SZCOR  = ',(SZCOR(I),I=1,NPD)
                  WRITE(IOUNIT,*) 'SZCORC = ',(SZCORC(I),I=1,NPD)
                 ENDIF
                  WRITE(IOUNIT,*) 'PCALC: Gas Removal -------------'
                  WRITE(IOUNIT,*) 'WQCORG, WQCORGC = ',WQCORG,WQCORGC
                  WRITE(IOUNIT,*) 'PCALC: Concentration -----------'
                  WRITE(IOUNIT,*) 'SIMPL, COMPL    = ',SIMPL,COMPL
               END IF

            END IF
 2000    CONTINUE
C        End Receptor LOOP
      END IF

      RETURN
      END

      SUBROUTINE SETSRC
C***********************************************************************
C                 SETSRC Module of ISC2 Short Term Model - ISCST2
c ----------------------------------------------------------------------
c ---    ISC-PRIME     Version 1.0    Level 970812              Modified
c ---        D. Strimaitis
c ---        Earth Tech, Inc.
c            Prepared for EPRI under contract WO3527-01
c ----------------------------------------------------------------------
C
C        PURPOSE: Sets the Source Parameters for a Particular Source
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        MODIFIED BY: D. Strimaitis, SRC (for Wet & Dry DEPOSITION)
C
C        DATE:  November 8,1993
C
C        MODIFIED by Yicheng Zhuang, SRC to combine version 93188 with
C                 version 93046 - 9/28/93
C
C        MODIFIED:   To incorporate inputs for numerical integration
C                    algorithm for AREA source - 7/7/93
C
C        MODIFIED BY D. Strimaitis, SRC (for DEPOSITION) - 2/15/93
C
C        MODIFIED BY Jayant Hardikar,PES (for handling OPENPIT
C                    Source - 7/19/94 , also modified AREA Source
C                    for Consistency with OPENPIT Source)
C
C        MODIFIED BY Roger Brode, PES (modified data structure for
C                    AXVERT and AYVERT for consistency with other
C                    2-D source arrays) - 8/15/95
C
C        INPUTS:  Source Parameters Arrays
C                 Source Index
C
C        OUTPUTS: Source Parameters for a Particular Source
C
C        CALLED FROM:   PCALC
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'SETSRC'

C     Assign The Values From Array Elements To Variables
      IF (SRCTYP(ISRC) .EQ. 'POINT') THEN
         XS = AXS(ISRC)
         YS = AYS(ISRC)
         ZS = AZS(ISRC)
         QS = AQS(ISRC)
         HS = AHS(ISRC)

         DS = ADS(ISRC)
         VS = AVS(ISRC)
         TS = ATS(ISRC)

C        Check for Negative Stack Temperature, Used to Indicate Constant TS-TA
         IF (TS .LT. 0.0) THEN
            TS = TA + ABS(TS)
         END IF

         IF (IFVSEC .LE. NSEC) THEN
            DSBH = ADSBH(IFVSEC,ISRC)
            DSBW = ADSBW(IFVSEC,ISRC)

c --- PRIME ---------------------------------
            dsbl = adsbl(IFVSEC,ISRC)
            xadj = adsxadj(IFVSEC,ISRC)
            yadj = adsyadj(IFVSEC,ISRC)
c -------------------------------------------

            IF (IDSWAK(IFVSEC,ISRC) .EQ. 0) THEN
               WAKLOW = .FALSE.
            ELSE IF (IDSWAK(IFVSEC,ISRC) .EQ. 1) THEN
               WAKLOW = .TRUE.
            END IF
         END IF

      ELSE IF (SRCTYP(ISRC) .EQ. 'VOLUME') THEN
         XS = AXS(ISRC)
         YS = AYS(ISRC)
         ZS = AZS(ISRC)
         QS = AQS(ISRC)
         HS = AHS(ISRC)

         SYINIT = ASYINI(ISRC)
         SZINIT = ASZINI(ISRC)

      ELSE IF (SRCTYP(ISRC) .EQ. 'AREA') THEN
         XS = AXS(ISRC)
         YS = AYS(ISRC)
         ZS = AZS(ISRC)
         QS = AQS(ISRC)
         HS = AHS(ISRC)

         XINIT = AXINIT(ISRC)
         YINIT = AYINIT(ISRC)
         ANGLE = AANGLE(ISRC)

         SZINIT = ASZINI(ISRC)

         DO 15 IVERT = 1, NVERT+1
            XVERT(IVERT) = AXVERT(IVERT,ISRC)
            YVERT(IVERT) = AYVERT(IVERT,ISRC)
15       CONTINUE


      ELSE IF (SRCTYP(ISRC) .EQ. 'OPENPIT') THEN
         XS = AXS(ISRC)
         YS = AYS(ISRC)
         ZS = AZS(ISRC)
         QS = AQS(ISRC)
C        Set Emission Height of Effective Area, HS = 0.0
         HS = 0.0
C        Set Height of Emissions Above Base of Pit, EMIHGT
         EMIHGT = AHS(ISRC)

         XINIT = AXINIT(ISRC)
         YINIT = AYINIT(ISRC)
         ANGLE = AANGLE(ISRC)
         PALPHA = AALPHA(ISRC)
         PDEFF  = APDEFF(ISRC)
         SZINIT = ASZINI(ISRC)
         PITLEN = AMAX1(XINIT,YINIT)
         PITWID = AMIN1(XINIT,YINIT)


         DO 20 IVERT = 1, NVERT+1
            XVERT(IVERT) = AXVERT(IVERT,ISRC)
            YVERT(IVERT) = AYVERT(IVERT,ISRC)
20       CONTINUE


      END IF

      NPD = INPD(ISRC)
      IF (NPD .GT. 0) THEN
         DO 25 J = 1, NPD
            PDIAM(J) = APDIAM(J,ISRC)
            PHI(J) = APHI(J,ISRC)
            PDENS(J) = APDENS(J,ISRC)
            VGRAV(J) = AVGRAV(J,ISRC)
            TSTOP(J) = ATSTOP(J,ISRC)
            SC(J) = ASC(J,ISRC)
            PSCAV(J,1) = APSLIQ(J,ISRC)
            PSCAV(J,2) = APSICE(J,ISRC)
 25      CONTINUE
      END IF

C     Transfer Gas Wet Scavenging Coeff. (1:liquid, 2:frozen)
      GSCAV(1) = AGSCAV(1,ISRC)
      GSCAV(2) = AGSCAV(2,ISRC)

      RETURN
      END

      SUBROUTINE PDIS(XARG,SYOUT,SZOUT,XYOUT,XZOUT,SBOUT)
C***********************************************************************
C                 PDIS Module of the ISC Short Term Model - Version 2
c ----------------------------------------------------------------------
c ---    ISC-PRIME     Version 1.0    Level 970812              Modified
c ---        D. Strimaitis
c ---        Earth Tech, Inc.
c            Prepared for EPRI under contract WO3527-01
c ----------------------------------------------------------------------
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
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'PDIS'

      IF (.NOT. WAKE) THEN
C        Calculate Sigma-y from Curves                   ---   CALL SIGY
         CALL SIGY(XARG,SYARG)
C        Calculate Sigma-z from Curves                   ---   CALL SIGZ
         CALL SIGZ(XARG,SZARG)
         IF (.NOT. NOBID) THEN
C           Apply BID                                    ---   CALL BID
            CALL BID(DHP,SYARG,SZARG,SYOUT,SZOUT,SBOUT)
         ELSE
            SBOUT = 0.0
            SYOUT = SYARG
            SZOUT = SZARG
         END IF
         XYOUT = 0.0
         XZOUT = 0.0
      ELSE IF (WAKE) THEN

c --- PRIME -----------------------------------------------------
c ---    Calculate sigmas from WAKE information          ---   CALL WAKE_XSIG
         call WAKE_XSIG(xarg,dhpout,nobid,szout,syout,
     &                  szcav,sycav)
c
c !!!         IF (HEMWAK .GT. 1.2*DSBH) THEN
c !!!C           Calculate Sigma-y from Curves                ---   CALL SIGY
c !!!c ***
c !!!            kstsav=kst
c !!!            if(kst.ge.5)kst=4
c !!!            CALL SIGY(XARG,SYARG)
c !!!            kst=kstsav
c !!!c ***
c !!!            XYOUT = 0.0
c !!!         ELSE
c !!!C           Calculate Building Enhanced Sigma-y          ---   CALL SYENH
c !!!            CALL SYENH(XARG,SYARG,XYOUT)
c !!!         END IF
c !!!C        Calculate Building Enhanced Sigma-z             ---   CALL SZENH
c !!!         CALL SZENH(XARG,SZARG,XZOUT)
c !!!c
c !!!c ---    Set SY = SZ, if SY is now < SZ
c !!!         if(syarg.lt.szarg)then
c !!!            syarg=szarg
c !!!c
c !!!c ---       Calculate lateral virtual distance              ---   CALL XVY
c !!!            syinit=syarg
c !!!            CALL XVY(XYOUT)
c !!!         END IF
c !!!c
c !!!c ***    Eliminate exclusion of BID with Schulman-Scire scheme
c !!!c ***    IF ((.NOT. NOBID) .AND. (.NOT. WAKESS)) THEN
c !!!         IF ((.NOT. NOBID)) THEN
c !!!C           Apply BID                                    ---   CALL BID
c !!!            CALL BID(DHP,SYARG,SZARG,SYOUT,SZOUT,SBOUT)
c !!!         ELSE
c !!!            SBOUT = 0.0
c !!!            SYOUT = SYARG
c !!!            SZOUT = SZARG
c !!!         END IF
c --------------------------------------------------------------------

      END IF

      IF (SZOUT .GT. 5000. .AND. NPD .EQ. 0)  SZOUT = 5000.

      RETURN
      END

      SUBROUTINE PHEFF(XARG,DHPOUT,HEOUT)
C***********************************************************************
C                 PHEFF Module of the ISC Short Term Model - Version 2
c ----------------------------------------------------------------------
c ---    ISC-PRIME     Version 1.0    Level 970812              Modified
c ---        J. Scire, D. Strimaitis
c ---        Earth Tech, Inc.
c            Prepared for EPRI under contract WO3527-01
c ----------------------------------------------------------------------
C
C        PURPOSE: Calculates Effective Plume Height for POINT Sources (m)
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To remove terrain adjustment to separate subroutine,
C                    and to use calling arguments
C                    R.W. Brode, PES, Inc. - 9/30/94
C
C        INPUTS:  Arrays of Source Parameters
C                 Logical Wake Flags
C                 Meteorological Variables for One Hour
C                 Wind Speed Adjusted to Stack Height
C                 Downwind Distance
C                 Terrain Elevation of Receptor
C
C        OUTPUTS: Plume Height (HEOUT) without Terrain Adjustment
C
C        CALLED FROM:   PCALC
C***********************************************************************

c --- Include PRIME plume rise parameters
      include 'params.pri'

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

c --- Dimension work arrays for PRIME numerical plume rise
      real xtr(mxntr),ytr(mxntr),ztr(mxntr),rtr(mxntr)
      logical ldbhr

C     Variable Initializations
      MODNAM = 'PHEFF'

C     Calculate Plume Height Without Terrain Adjustment
      IF ((.NOT. WAKE) .AND. (.NOT. GRDRIS)) THEN
C        Calculate Final Rise for First Receptor Only
         IF (FSTREC) THEN
            FSTREC = .FALSE.
            HSP = HSPRIM(US,VS,HS,DS)
C           Calculate Final Rise, DHF                       ---   CALL DELH
            CALL DELH(DHF)
         END IF
         IF (NOSTD) THEN
            HEOUT = HS + DHF
         ELSE
            HEOUT = HSP + DHF
         END IF
         IF (.NOT. NOBID) THEN
C           Calculate Gradual Plume Rise for Use in BID Calculation
            IF (XARG .LT. XF) THEN
C              Calculate Gradual Rise, DHPOUT               ---   CALL DHPHS
               CALL DHPHS(XARG,DHF,DHPOUT)
            ELSE
               DHPOUT = DHF
            END IF
         ELSE
            DHPOUT = DHF
         END IF

c --- PRIME ---------------------------------------------------
c *** ELSE IF (WAKE .AND. WAKESS) THEN
C        Calculate Final Rise for First Receptor Only
c ***    IF (FSTREC) THEN
c ***       FSTREC = .FALSE.
C           Calculate Final Rise (at X=XF), DHF             ---   CALL DHPSS
c ***       CALL DHPSS(XF,DHPOUT)
c ***       DHF    = DHPOUT
c ***    END IF
c ***    IF (XARG .LT. XF) THEN
C           Calculate Gradual Rise, DHP                     ---   CALL DHPSS
c ***       CALL DHPSS(XARG,DHPOUT)
c ***    ELSE
c ***       DHPOUT = DHF
c ***    END IF
c ***    HEOUT = HS + DHPOUT
c *** ELSE
CRWB     if ((WAKE .AND. (.NOT. WAKESS)) .OR.
CRWB        ((.NOT. WAKE) .AND. GRDRIS)) then
      elseif((.NOT. WAKE) .AND. GRDRIS) then
c ---    Keep standard gradual rise code if no bldg downwash
c -------------------------------------------------------------

C        Calculate Final Rise for First Receptor Only
         IF (FSTREC) THEN
            FSTREC = .FALSE.
            HSP = HSPRIM(US,VS,HS,DS)
C           Calculate Final Rise, DHF                       ---   CALL DELH
            CALL DELH(DHF)
         END IF
         IF (XARG .LT. XF) THEN
C           Calculate Gradual Rise, DHP                     ---   CALL DHPHS
            CALL DHPHS(XARG,DHF,DHPOUT)
         ELSE
            DHPOUT = DHF
         END IF
         IF (NOSTD) THEN
            HEOUT = HS + DHPOUT
         ELSE
            HEOUT = HSP + DHPOUT
         END IF
c *** END IF
c
c --- PRIME ---------------------------------------------------
      else
c ---    Calculate final rise & array of transitional rise values
c ---    for first receptor only
         if (FSTREC) then
            FSTREC = .FALSE.
c ---------------------- Drop tip downwash
c ---       Account for stack-tip effects, if option is requested
c           if(nostd)then
               hseff=hs
c           else
c              HSP = HSPRIM(US,VS,HS,DS)
c              hseff=hsp
c           endif
c ---------------------- Drop tip downwash
c ---       Compute stack radius from diameter
            reff=0.5*ds
c ---       Calculate transitional & final plume rise       ---   CALL NUMRISE
            ldbhr=DEBUG
c           ldbhr=.FALSE.
            call NUMRISE(ldbhr,hseff,reff,ts,vs,
     &                   mxntr,xtr,ytr,ztr,rtr)
c ---       ZTR is effective plume ht. - compute final rise
            DHF    = ztr(mxntr) - hseff
c ---       Report selected data to file for debug          ---   CALL WAKE_DBG
            if(DEBUG) call WAKE_DBG(iounit,mxntr,xtr,ytr,ztr,rtr,
     &                              nobid,hseff)
         endif
c
c ---    Determine the plume rise for current receptor
         IF (XARG .LT. xtr(mxntr)) THEN
c ---       Interpolate in rise table to get gradual rise   ---   CALL NUMGRAD
            call NUMGRAD(xarg,xtr,ztr,mxntr,zeff)
            dhpout = zeff - hseff
         ELSE
            DHPOUT = ztr(mxntr) - hseff
         END IF
c ---    HEOUT is the effective stack height
         HEOUT = dhpout + hseff
c -------------------------------------------------------------

      endif

      RETURN
      END

      SUBROUTINE PSIMPL
C***********************************************************************
C               PSIMPL Module of ISC2 Short Term Model - ISCST2
c ----------------------------------------------------------------------
c ---    ISC-PRIME     Version 1.0    Level 970812              Modified
c ---        D. Strimaitis
c ---        Earth Tech, Inc.
c            Prepared for EPRI under contract WO3527-01
c ----------------------------------------------------------------------
C
C        PURPOSE: Calculates Hourly Concentration or Deposition
C                 value for POINT Sources
C                 Using Gaussian Plume Equation for Simple Terrain
C
C                 (Replaces PCHI and PDEP)
C
C           NOTE: Particle settling is treated as a "tilted plume"
C                 until the centerline reaches the surface.  Thereafter
C                 the centroid height of the plume continues to be
C                 modified by gravity.  This process is simulated by
C                 altering the sigma-z for each particle-size.  Hence,
C                 sigma-z is now a function of particle-size.
C
C        PROGRAMMER: D. Strimaitis, SRC
C
C        DATE:    December 15, 1993
C
C        MODIFIED:   To call PDEP for call to SUB. DEPCOR; to use
C                    modified SUB. VERT.  R.W. Brode, PES, Inc. - 9/30/94
C
C        INPUTS:
C
C        OUTPUTS: HRVAL, Concentration or Deposition for Particular
C                 Source/Receptor Combination
C
C        CALLED FROM:   PCALC
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      LOGICAL WDONLY

c --- Declare local PRIME arrays for "3-source" data
      real q2(3),y2(3),sy2(3),z2(3),h2(3),sz2(3),qc2(3)

C     Variable Initializations
      MODNAM = 'PSIMPL'
      WDONLY = .FALSE.

      IF ((UNSTAB .OR. NEUTRL) .AND. HEFLAT.GT.ZI) THEN
C        Plume Is Above Mixing Height, ZI
         IF (DEPOS .OR. WDEP) THEN
C           Set WDONLY flag for Wet Deposition Only
            WDONLY = .TRUE.
         ELSE
            DO ITYP = 1, NUMTYP
               HRVAL(ITYP) = 0.0
            END DO
            if(DEBUG) then
c ---          Print out debug information                  ---   CALL DEBOUT
               call DEBOUT
            endif
            return
         END IF
      END IF

c --- PRIME ---------------------------------------------------
c --- When there is a building wake, consider treatment of mass in
c --- cavity as additional sources, or as only source
      qtksav=qtk
c --- Place selected plume data into transfer arrays (first element)
      q2(1)=qtk
      y2(1)=y
      sy2(1)=sy
      z2(1)=zflag
      h2(1)=he
      sz2(1)=sz
      n1=1
      n2=1
      if(WAKE) then
c ---    Define cavity source                              ---   CALL CAV_SRC
         call CAV_SRC(x,y,zflag,fqcav,qc2,h2,y2,z2,sz2,sy2,n1,n2)
         if(fqcav.GT.0.0) then
c ---       Set source strengths
            q2(1)=qtk*(1.0-fqcav)
            q2(2)=qtk*fqcav*qc2(2)
            q2(3)=qtk*fqcav*qc2(3)
         endif
      endif

c --- Initialize output array values to zero, because contributions
c --- due to more than one source are summed here (or do-1000 loop may
c --- not execute if neither source contributes)
      do ityp = 1, numtyp
         hrval(ityp) = 0.0
      enddo
      simcon=0.0

c --- Loop over 3 possible sources
      do 1000 is=n1,n2

c --- Transfer data for current source
      qtk=q2(is)
      y=y2(is)
      sy=sy2(is)
      zflag=z2(is)
      he=h2(is)
      sz=sz2(is)
c ----------------------------------------------------------------------

      YTERM = -0.5*(Y*Y)/(SY*SY)
      IF (YTERM .GT. EXPLIM) THEN

C        Determine Deposition Correction Factors for Particles
         IF (LDPART .OR. LWPART) THEN
            CALL PDEP (WDONLY)
         ENDIF

         IF (NPD .EQ. 0) THEN
            DO ITYP = 1, NUMTYP
               V(ITYP) = 0.
            END DO
            VSIMP = 0.0
            ITYP = 0
C           Calculate the Vertical Term, V, for gases
            IF (CONC) THEN
               ITYP = ITYP + 1
               IF (WDONLY) THEN
C                 Plume is above mixing height so set CONC = 0.0
                  V(ITYP) = 0.0
               ELSE
C                 Calculate Concentration Form of V         ---   CALL VERT
                  A0 = -0.5/(SZ*SZ)
                  CALL VERT(HE,SZ,A0,ZFLAG,V(ITYP))
               END IF
            ELSE IF (INTERM) THEN
               IF (WDONLY) THEN
C                 Plume is above mixing height so set CONC = 0.0
                  VSIMP = 0.0
               ELSE
C                 Calculate Concentration Form of V         ---   CALL VERT
                  A0 = -0.5/(SZ*SZ)
                  CALL VERT(HE,SZ,A0,ZFLAG,VSIMP)
               END IF
            ENDIF
            IF (DEPOS) THEN
               ITYP = ITYP + 1
C              Calculate Wet Flux Form of V
C              Vertical Term is Integral of EXP terms Over All z
               V(ITYP) = SRT2PI*SZ
C              Apply Scavenging Ratio
               V(ITYP) = V(ITYP) * GSCVRT
            END IF
            IF (DDEP) THEN
               ITYP = ITYP + 1
            END IF
            IF (WDEP) THEN
               ITYP = ITYP + 1
C              Calculate Wet Flux Form of V
C              Vertical Term is Integral of EXP terms Over All z
               V(ITYP) = SRT2PI*SZ
C              Apply Scavenging Ratio
               V(ITYP) = V(ITYP) * GSCVRT
            END IF
C           Allow for Depletion of Gases Due to Wet Scavenging
            IF (LWGAS) THEN
               DO ITYP = 1, NUMTYP
                  V(ITYP) = V(ITYP) * WQCORG
               END DO
               IF (.NOT.CONC .AND. INTERM) THEN
                  VSIMP = VSIMP * WQCORG
               END IF
            END IF

C           Include SZ in the denomenator of V
            DO ITYP = 1, NUMTYP
               V(ITYP) = V(ITYP)/SZ
            END DO
            IF (.NOT.CONC .AND. INTERM) THEN
               VSIMP = VSIMP/SZ
            END IF

         ELSE
C           Calculate the Vertical Term, V for particles
            DO ITYP = 1, NUMTYP
               V(ITYP) = 0.
            END DO
            VSIMP = 0.
            DO J = 1, NPD
               ITYP = 0
C              Settling may alter SZ for the Jth particle plume
               SZADJ = SZ*SZCOR(J)
               A0 = -0.5/(SZADJ*SZADJ)
C              Calculate Plume Tilt Due to Settling, HV
               HV = (X/US) * VGRAV(J)
C              Calculate Settled Plume Height, HESETL
               HESETL = HE - HV
C              Restrict settled height to be positive, so that the plume
C              does not settle below the surface -- this is the limit of
C              the tilted plume technique.
               HESETL = AMAX1(0.0,HESETL)
C              Adjust Jth contribution by mass fraction and source
C              depletion
               ADJ = PHI(J) * DQCOR(J) * WQCOR(J)
               IF (CONC) THEN
C                 Concentration
                  ITYP = ITYP + 1
                  IF (WDONLY) THEN
C                    Plume is above mixing height so set CONC = 0.0
                     V(ITYP) = 0.0
                  ELSE
C                    For Concentration Complete Vertical Term is Needed for
C                    Each Particle Size Calculated at ZFLAG ---   CALL VERT
                     CALL VERT(HESETL,SZADJ,A0,ZFLAG,VJ)
                     V(ITYP) = V(ITYP) + ADJ*PCORZR(J)*VJ/SZADJ
                  END IF
               END IF
               IF (DEPOS .OR. DDEP) THEN
                  IF (WDONLY) THEN
C                    Plume is above mixing height so set DDEP = 0.0
                     DRYFLUX = 0.0
                  ELSE
C                    For Dry Deposition Complete Vertical Term is Needed for
C                    Each Particle Size Calculated at ZRDEP ---   CALL VERT
                     CALL VERT(HESETL,SZADJ,A0,ZRDEP,VJ)
C                    Calculate Dry Flux VJ/SZ
                     DRYFLUX = ADJ*PCORZD(J)*VDEP(J)*VJ/SZADJ
                  END IF
               END IF
               IF (DEPOS .OR. WDEP) THEN
C                 Calculate Wet Flux VJ/SZ --
C                 For Wet Flux, Vertical Term is Integral of EXP terms
C                 Over All z, so VJ/SZ=SQRT(2PI)
                  WETFLUX = ADJ*PSCVRT(J)*SRT2PI
               ENDIF
               IF (DEPOS) THEN
C                 Wet & Dry fluxes of particles are summed
                  ITYP = ITYP + 1
                  V(ITYP) = V(ITYP) + DRYFLUX + WETFLUX
               END IF
               IF (DDEP) THEN
C                 Dry flux of particles
                  ITYP = ITYP + 1
                  V(ITYP) = V(ITYP) + DRYFLUX
               END IF
               IF (WDEP) THEN
C                 Wet flux of particles
                  ITYP = ITYP + 1
                  V(ITYP) = V(ITYP) + WETFLUX
               ENDIF
               IF (.NOT.CONC .AND. INTERM) THEN
                  IF (WDONLY) THEN
C                    Plume is above mixing height so set CONC = 0.0
                     VSIMP = 0.0
                  ELSE
C                    For Concentration Complete Vertical Term is Needed for
C                    Each Particle Size Calculated at ZFLAG ---   CALL VERT
                     CALL VERT(HESETL,SZADJ,A0,ZFLAG,VJ)
C                    Calculate Concentration for Intermediate Terrain Check
                     VSIMP = VSIMP + ADJ*PCORZR(J)*VJ/SZADJ
                  END IF
               END IF
            ENDDO
         END IF

C        Calculate the Decay Term, D                     ---   CALL DECAY
         CALL DECAY (X)

         DO ITYP = 1, NUMTYP
C           Complete VTERM (SZ already in denomenator of V)
            VTERM = (D*V(ITYP))/(TWOPI*US*SY)

C           Check for Possible Underflow Condition
            IF (VTERM.GT.0.0 .AND. (LOG(VTERM)+YTERM).GT.EXPLIM) THEN
               HRVAL(ITYP) = hrval(ityp) +
     &                       QTK * EMIFAC(ITYP) * VTERM * EXP(YTERM)
            END IF
         END DO

         IF (.NOT.CONC .AND. INTERM) THEN
C           Calculate Concentration for Simple Terrain
C           Complete VTERM (SZ already in denomenator of V)
            VTERM = (D*VSIMP)/(TWOPI*US*SY)

C           Check for Possible Underflow Condition
            IF (VTERM.GT.0.0 .AND. (LOG(VTERM)+YTERM).GT.EXPLIM) THEN
               SIMCON = simcon +
     &                  QTK * EMICON * VTERM * EXP(YTERM)
            END IF
         ELSE IF (CONC .AND. INTERM) THEN
            SIMCON = simcon + HRVAL(1)
         END IF

      END IF

      IF (DEBUG) THEN
C        Print Out Debugging Information                    ---   CALL DEBOUT
         CALL DEBOUT
      END IF

c --- PRIME ---------------------------------------------------
 1000 CONTINUE

c --- Restore original plume data
      qtk=qtksav
      y=y2(1)
      sy=sy2(1)
      zflag=z2(1)
      he=h2(1)
      sz=sz2(1)
c -------------------------------------------------------------

 999  RETURN
      END

      SUBROUTINE DEBOUT
C***********************************************************************
C                 DEBOUT Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Outputs Debugging Information: Sigmas, Plume Heights,
C                 etc., for Each Calculation
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
C        OUTPUTS: Debug Outputs
C
C        CALLED FROM:   PSIMPL, AREAIN
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'DEBOUT'

c !!! Define ITYP to be 1 to print out first output type
      ityp=1
c !!!

      WRITE(IOUNIT,119) JDAY, IHOUR, KURDAT, ISRC, IREC
 119  FORMAT(/1X,'JDAY= ',I3,'  IHOUR= ',I5,'  KURDAT= ',I10,
     &       '  ISRC= ',I3,'  IREC= ',I3)

      IF (SRCTYP(ISRC) .EQ. 'POINT') THEN
         WRITE(IOUNIT,209) QS, HS, TS, VS, DS, DSBH, DSBW, US
 209     FORMAT(1X,'QS= ',F8.2,' HS= ',F8.2,' TS= ',F8.2,' VS= ',
     &       F8.2,' DS= ',F8.2,' DSBH= ',F8.2,' DSBW= ',F8.2,
     &       ' US= ',F8.5)
         WRITE(IOUNIT,219) FB, FM
 219     FORMAT(1X,' FB= ',F11.5,' FM= ',F11.5)
      ELSE IF (SRCTYP(ISRC) .EQ. 'VOLUME') THEN
         WRITE(IOUNIT,229) QS, HS, SYINIT, SZINIT, US
 229     FORMAT(1X,'QS= ',F8.2,'  HS= ',F8.2,'  SYINIT= ',F8.2,
     &          '  SZINIT= ',F8.2,'  US= ',F8.5)
      ELSE IF (SRCTYP(ISRC) .EQ. 'AREA') THEN

         WRITE(IOUNIT,239) QS, HS, XINIT, US, E, SZINIT
 239     FORMAT(1X,'QS= ',F8.2,'  HS= ',F8.2,'  XINIT= ',F8.2,
     &          '  US= ',F8.5,'  E= ',G14.8,
     &          '  SZINIT= ',F8.2)

      END IF

      WRITE(IOUNIT,319) X, Y, XY, XZ, SY, SZ
 319  FORMAT(1X,'X= ',F12.4,'  Y= ',F12.4,'  XY= ',F11.5,'  XZ= ',
     &       F11.5,'  SY= ',F11.5,'  SZ= ',F12.5)
      IF (CONC) THEN
         WRITE(IOUNIT,409) HE, HEMWAK, HEFLAT, KST, TA, ZI, V(ITYP), D
 409     FORMAT(1X,'HE= ',F11.5,'  HEMWAK= ',F11.5,'  HEFLAT= ',F11.5,
     &       '  KST= ',I2,'  TA= ',F6.1,'  ZI= ',F8.2,'  V= ',E12.6,
     &       '  D= ',E12.6)
      ELSE
         WRITE(IOUNIT,419) HE, HEMWAK, HEFLAT, KST, TA, ZI, V(ITYP), D
 419     FORMAT(1X,'HE= ',F11.5,'  HEMWAK= ',F11.5,'  HEFLAT= ',F11.5,
     &       '  KST= ',I2,'  TA= ',F6.1,'  ZI= ',F8.2,'  V= ',E12.6,
     &       '  D= ',E12.6)
      END IF
      WRITE(IOUNIT,519) ZLB, RINIT, ZLY, DA, WAKE, WAKESS
 519  FORMAT(1X,'ZLB=',F11.5,'  RINIT= ',F9.4,'  ZLY= ',
     &       F9.4,'  DA= ',F8.6,'  WAKE= ',L3,'  WAKESS=',L3)
      WRITE(IOUNIT,619) QTK, XF, XFB, XFM, DHF, DHP
 619  FORMAT(1X,'QTK= ',E12.5,'  XF= ',F9.2,
     &       '  XFB= ',F9.2,'  XFM= ',F9.2,'  DHF= ',F9.2,
     &       '  DHP= ',F9.2)

      WRITE(IOUNIT,719) HRVAL(ITYP)
 719  FORMAT(1X,'*** HRVAL= ',G16.8,' ***')

      RETURN
      END

      SUBROUTINE WAKFLG
C***********************************************************************
C                 WAKFLG Module of ISC2 Short Term Model - ISCST2
c ----------------------------------------------------------------------
c ---    ISC-PRIME     Version 1.0    Level 970812              Modified
c ---        D. Strimaitis
c ---        Earth Tech, Inc.
c            Prepared for EPRI under contract WO3527-01
c ----------------------------------------------------------------------
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
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'WAKFLG'

C     Set Initial Wake Switches Based on Building Dimensions

c --- PRIME ----------------------------------------------------
c --- Fix error in ISC to conform to GEP policy
c *** IF (DSBH.EQ.0.0 .OR. DSBW.EQ.0.0 .OR.
c ***&    HS .GT. (DSBH + 1.5*AMIN1(DSBH,DSBW))) THEN
      IF (DSBH.EQ.0.0 .OR. DSBW.EQ.0.0 .OR.
     &    HS .GE. (DSBH + 1.5*AMIN1(DSBH,DSBW))) THEN
c -------------------------------------------------------------
         WAKE   = .FALSE.
         WAKESS = .FALSE.
      ELSE IF (HS .GT. (DSBH + 0.5*AMIN1(DSBH,DSBW))) THEN
         WAKE   = .TRUE.
         WAKESS = .FALSE.
      ELSE
         WAKE   = .TRUE.
         WAKESS = .TRUE.
      END IF

c --- PRIME ----------------------------------------------------
c --- Drop section for old DW parameters .................
cC     Set Final Wake Switches Based on Plume Height
c      IF (WAKE) THEN
c         X2BH = DSBH + DSBH
cC        Calculate Gradual Momentum Rise at X2BH            ---   CALL DHPMOM
c         CALL DHPMOM(X2BH)
c         HEMWAK = HS + DHPM
c         IF (WAKESS) THEN
c            IF (HEMWAK .LE. (DSBH + 2.0*AMIN1(DSBH,DSBW))) THEN
c               WAKE   = .TRUE.
c            ELSE
c               WAKE   = .FALSE.
c               WAKESS = .FALSE.
c            END IF
c         ELSE
c            IF (HEMWAK .LE. (DSBH + 1.5*AMIN1(DSBH,DSBW))) THEN
c               WAKE = .TRUE.
c            ELSE
c               WAKE = .FALSE.
c            END IF
c         END IF
c      ELSE
c         HEMWAK = 0.0
c      END IF
c
cC     Set Value of ZLB And Set Logical Flags for Building Type
c      IF (WAKE) THEN
c         ZLB = AMIN1(DSBH,DSBW)
c         IF (DSBW .LT. DSBH) THEN
cC           Tall Building
c            TALL  = .TRUE.
c            SQUAT = .FALSE.
c            SSQUAT= .FALSE.
c         ELSE IF (DSBW .LE. 5.*DSBH) THEN
cC           Squat Building
c            TALL  = .FALSE.
c            SQUAT = .TRUE.
c            SSQUAT= .FALSE.
c         ELSE
cC           Super-Squat Building
c            TALL  = .FALSE.
c            SQUAT = .FALSE.
c            SSQUAT= .TRUE.
c         END IF
c      ELSE
c         ZLB = 0.0
c      END IF
c -------------------------------------------------------------

      RETURN
      END

      SUBROUTINE XVY(sigy,XYOUT)
c !!! SUBROUTINE XVY(XYOUT)
C***********************************************************************
C                 XVY Module of ISC2 Model
c ----------------------------------------------------------------------
c ---    ISC-PRIME     Version 1.0    Level 970812              Modified
c ---        D. Strimaitis
c ---        Earth Tech, Inc.
c            Prepared for EPRI under contract WO3527-01
c ----------------------------------------------------------------------
C
C        PURPOSE: Calculates Lateral Virtual Distances
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To use calling argument for output
C                    R. W. Brode, PES, Inc. - 9/30/94
C
C        INPUTS:  Initial Dispersion, SYINIT
C                 Stability Class
C                 Rural or Urban Dispersion Option
C
C        OUTPUTS: Lateral Virtual Distance, XYOUT (m)
C
C        CALLED FROM:   VDIS
C                       SYENH
c                       WAKE_DFSN
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      REAL A(6),SP(6),SQ(6)

C     Variable Initializations
      DATA A/0.32,0.32,0.22,0.16,0.11,0.11/,
     &     B/0.0004/,
     &     SP/.004781486,.006474168,.009684292,.014649868,.019584802,
     &        0.029481132/,
     &     SQ/1.1235955,1.1086475,1.0905125,1.0881393,1.0857763,
     &        1.0881393/
      MODNAM = 'XVY'

c --- PRIME -------------------------------------------------------
c --- Initial sigma is provided as an argument
c --- The sigma in MAIN1 common is NOT USED
c -----------------------------------------------------------------

      IF (RURAL) THEN
         XYOUT = (sigy  *SP(KST))**SQ(KST) * 1000.
c !!!    XYOUT = (SYINIT*SP(KST))**SQ(KST) * 1000.
      ELSE IF (URBAN) THEN
         A2 = A(KST) * A(KST)
         SY2 = sigy   * sigy
c !!!    SY2 = SYINIT * SYINIT
         XYOUT = (B*SY2 + SQRT(B*B*SY2*SY2 + 4.*A2*SY2)) / (2.*A2)
      END IF

      RETURN
      END

      SUBROUTINE XVZ(sigz,XARG,XZOUT)
c !!! SUBROUTINE XVZ(XARG,XZOUT)
C***********************************************************************
C                 XVZ Module of ISC2 Model
c ----------------------------------------------------------------------
c ---    ISC-PRIME     Version 1.0    Level 970812              Modified
c ---        D. Strimaitis
c ---        Earth Tech, Inc.
c            Prepared for EPRI under contract WO3527-01
c ----------------------------------------------------------------------
C
C        PURPOSE: Calculates Vertical Virtual Distances
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To use calling argument for output
C                    R. W. Brode, PES, Inc. - 9/30/94
C
C        MODIFIED:   To Change TOL from 1.0E-5 to 1.0E-4 - 9/29/92
C
C        INPUTS:  Initial Dispersion, SZINIT
C                 Downwind Distance
C                 Stability Class
C                 Rural or Urban Dispersion Option
C
C        OUTPUTS: Vertical Virtual Distance, XZOUT (m)
C
C        CALLED FROM:   VDIS
C                       SZENH
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      REAL AA(6), BB(6), XXZ(6)

C     Variable Initializations
      DATA AA/0.24,0.24,0.2,0.14,0.08,0.08/
      DATA BB/.001,.001,.0,.0003,.0015,.0015/
      MODNAM = 'XVZ'

c --- PRIME -------------------------------------------------------
c --- Initial sigma is provided as an argument
c --- The sigma in MAIN1 common is NOT USED
c -----------------------------------------------------------------

      IF (sigz   .LE. 0.01) THEN
c !!! IF (SZINIT .LE. 0.01) THEN
         XZOUT = 0.

      ELSE IF (RURAL) THEN
C        Solve Iteratively
C        Convert Distance to km
         XKM = XARG * 0.001
C        Initial Guess of 10 m
         XXZ(1) = 0.01
         DO 10 N = 1, 5
C           Retrieve Coef. AZ & BZ, Range XMIN & XMAX    ---   CALL SZCOEF
            CALL SZCOEF((XXZ(N)+XKM),AZ,BZ,XMIN,XMAX)
            XXZ(N+1) = (sigz  /AZ) ** (1./BZ)
c !!!       XXZ(N+1) = (SZINIT/AZ) ** (1./BZ)
C           Check for X+XZ falling within Range of Coefficients
            IF((XXZ(N+1)+XKM).GE.XMIN .AND. (XXZ(N+1)+XKM).LE.XMAX) THEN
               XZOUT = XXZ(N+1) * 1000.
C              EXIT LOOP
               GO TO 999
            END IF
 10      CONTINUE
C        If No Convergence in Loop, Use Smaller of Last Two Estimates,
C        Consistent With Original ISC Model - Version 2
         XZOUT = AMIN1(XXZ(5),XXZ(6)) * 1000.

      ELSE IF (URBAN) THEN
         IF (KST .GE. 4) THEN
            A2  = AA(KST) * AA(KST)
            B   = BB(KST)
            SZ2 = sigz   * sigz
c !!!       SZ2 = SZINIT * SZINIT
            XZOUT  = (B*SZ2 + SQRT(B*B*SZ2*SZ2 + 4.*A2*SZ2)) / (2.*A2)
         ELSE IF (KST .LE. 2) THEN
C           Set Initial Guess and Tolerance Limit for Cubic Equation
            XZERO = 4. * sigz
c !!!       XZERO = 4. * SZINIT
            TOL = 1.0E-4
C           Set Cubic Coefficients, ACOEF, BCOEF, and CCOEF
            ACOEF = 1./BB(KST)
            BCOEF = 0.0
            CCOEF = -1. * sigz  *sigz  /(AA(KST)*AA(KST) * BB(KST))
c !!!       CCOEF = -1. * SZINIT*SZINIT/(AA(KST)*AA(KST) * BB(KST))
C           Solve Cubic Equation                          ---   CALL CUBIC
            CALL CUBIC(ACOEF,BCOEF,CCOEF,XZERO,TOL,XZOUT)
         ELSE IF (KST .EQ. 3) THEN
            XZOUT = sigz  /AA(KST)
c !!!       XZOUT = SZINIT/AA(KST)
         END IF
      END IF

 999  RETURN
      END

      SUBROUTINE VDIS(XARG,SYOUT,SZOUT,XYOUT,XZOUT)
C***********************************************************************
C                 VDIS Module of the ISC Short Term Model - Version 2
c ----------------------------------------------------------------------
c ---    ISC-PRIME     Version 1.0    Level 990120              Modified
c ---        D. Strimaitis
c ---        Earth Tech, Inc.
c            Prepared for EPRI under contract WO3527-01
c ----------------------------------------------------------------------
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
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'VDIS'

c --- PRIME --------------------------------------------------------
c --- Initial sigma  for virtual distance is provided as an argument
c --- in call to XVY and XVZ
c --- Modification 990120 -- Fix typo in XVY call:
c     CALL XVY(szinit,XYOUT) is changed to CALL XVY(syinit,XYOUT)
c ------------------------------------------------------------------

C     Calculate Lateral Virtual Distance                 ---   CALL XVY
      CALL XVY(syinit,XYOUT)
c !!! CALL XVY(XYOUT)
C     Calculate Sigma-y from Curves for X+XY             ---   CALL SIGY
      CALL SIGY(XARG+XYOUT,SYOUT)
C     Calculate Vertical Virtual Distance                ---   CALL XVZ
      CALL XVZ(szinit,XARG,XZOUT)
c !!! CALL XVZ(XARG,XZOUT)
C     Calculate Sigma-z from Curves for X+XZ             ---   CALL SIGZ
      CALL SIGZ(XARG+XZOUT,SZOUT)

      IF (SZOUT .GT. 5000. .AND. NPD .EQ. 0)  SZOUT = 5000.

      RETURN
      END

      SUBROUTINE SOCARD
C***********************************************************************
C                 SOCARD Module of ISC2 Model
c ----------------------------------------------------------------------
c ---    ISC-PRIME     Version 1.0    Level 970812              Modified
c ---        V. Tino
c ---        Earth Tech, Inc.
c            Prepared for EPRI under contract WO3527-01
c ----------------------------------------------------------------------
C
C        PURPOSE: To process SOurce Pathway card images
C
C        PROGRAMMER:  Roger Brode, Jeff Wang
C        MODIFIED BY  D. Strimaitis, SRC (for WET DEPOSITION)
C
C        DATE:    November  8, 1993
C
C        MODIFIED BY  D. Strimaitis, SRC (for DRY DEPOSITION)
C        (DATE:    February 15, 1993)
C
C        INPUTS:  Pathway (SO) and Keyword
C
C        OUTPUTS: Source Arrays
C                 Sourcer Setup Status Switches
C
C        CALLED FROM:   SETUP
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'SOCARD'

      IF (KEYWRD .EQ. 'STARTING') THEN
C        Initialize Counters and Set Status Switch
         ISRC = 0
         IGRP = 0
         NUMSRC = 0
         NUMGRP = 0
         ISSTAT(1) = ISSTAT(1) + 1
         IF (ISSTAT(1) .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
            GO TO 999
         END IF
C        Flush The Working Area
         DO 20 I = 1, NSRC
            DO 10 J = 1, 7
               IWRK2(I,J) = 0
  10        CONTINUE
  20     CONTINUE
      ELSE IF (KEYWRD .EQ. 'LOCATION') THEN
C        Set Status Switch
         ISSTAT(2) = ISSTAT(2) + 1
C        Check for SRCGROUP Card Out Of Order
         IF (ISSTAT(19) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
         END IF
C        Process Source Location                            ---   CALL SOLOCA
         CALL SOLOCA
      ELSE IF (KEYWRD .EQ. 'SRCPARAM') THEN
C        Set Status Switch
         ISSTAT(3) = ISSTAT(3) + 1
C        Check for SRCGROUP Card Out Of Order
         IF (ISSTAT(19) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
         END IF
C        Process Source Parameters                          ---   CALL SOPARM
         CALL SOPARM

c --- PRIME ---------------------------------
      ELSE IF (KEYWRD .EQ. 'BUILDHGT' .OR.
     &         KEYWRD .EQ. 'BUILDWID' .OR.
c prm&         KEYWRD .EQ. 'LOWBOUND') THEN
     &         KEYWRD .EQ. 'LOWBOUND' .OR.
     &         KEYWRD .EQ. 'BUILDLEN' .OR.
     &         KEYWRD .EQ. 'XBADJ   ' .OR.
     &         KEYWRD .EQ. 'YBADJ   ') THEN
c -------------------------------------------

C        Check for SRCGROUP Card Out Of Order
         IF (ISSTAT(19) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
         END IF
C        Set Status Switch
         IF (KEYWRD .EQ. 'BUILDHGT') THEN
            ISSTAT(4) = ISSTAT(4) + 1
         ELSE IF (KEYWRD .EQ. 'BUILDWID') THEN
            ISSTAT(5) = ISSTAT(5) + 1

c --- PRIME -----------------------------------
         ELSE IF (KEYWRD .EQ. 'BUILDLEN') THEN
            ISSTAT(21) = ISSTAT(21) + 1
         ELSE IF (KEYWRD .EQ. 'XBADJ   ') THEN
            ISSTAT(22) = ISSTAT(22) + 1
         ELSE IF (KEYWRD .EQ. 'YBADJ   ') THEN
            ISSTAT(23) = ISSTAT(23) + 1
c ---------------------------------------------

         ELSE IF (KEYWRD .EQ. 'LOWBOUND') THEN
            ISSTAT(6) = ISSTAT(6) + 1
            IF (DFAULT) THEN
C              WRITE Warning Message and Ignore Inputs
               CALL ERRHDL(PATH,MODNAM,'W','206',KEYWRD)
               GO TO 999
            END IF
         END IF
C        Process Direction-specific Building Dimensions     ---   CALL DSBLDG
         CALL DSBLDG
      ELSE IF (KEYWRD .EQ. 'EMISFACT') THEN
C        Set Status Switch
         ISSTAT(7) = ISSTAT(7) + 1
C        Check for SRCGROUP Card Out Of Order
         IF (ISSTAT(19) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
         END IF
C        Process Variable Emission Rate Factors             ---   CALL EMVARY
         CALL EMVARY
      ELSE IF (KEYWRD .EQ. 'EMISUNIT') THEN
C        Set Status Switch
         ISSTAT(8) = ISSTAT(8) + 1
C        Check for SRCGROUP Card Out Of Order
         IF (ISSTAT(19) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
         END IF
         IF (ISSTAT(8) .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE IF (NUMTYP .EQ. 1) THEN
C           Process Emission Rate Unit Conversion Factors   ---   CALL EMUNIT
            CALL EMUNIT
         ELSE
C           WRITE Error Message: EMISUNIT Keyword with more than 1 output type
            CALL ERRHDL(PATH,MODNAM,'E','157',' ')
         END IF
      ELSE IF (KEYWRD .EQ. 'PARTDIAM' .OR. KEYWRD .EQ. 'MASSFRAX' .OR.
     &         KEYWRD .EQ. 'PARTDENS' .OR. KEYWRD .EQ. 'PARTSLIQ' .OR.
     &         KEYWRD .EQ. 'PARTSICE') THEN
C        Set Status Switch
         IF (KEYWRD .EQ. 'PARTDIAM') THEN
            ISSTAT(9) = ISSTAT(9) + 1
         ELSE IF (KEYWRD .EQ. 'MASSFRAX') THEN
            ISSTAT(10) = ISSTAT(10) + 1
         ELSE IF (KEYWRD .EQ. 'PARTDENS') THEN
            ISSTAT(11) = ISSTAT(11) + 1
         ELSE IF (KEYWRD .EQ. 'PARTSLIQ') THEN
            ISSTAT(12) = ISSTAT(12) + 1
         ELSE IF (KEYWRD .EQ. 'PARTSICE') THEN
            ISSTAT(13) = ISSTAT(13) + 1
         END IF
C        Check for SRCGROUP Card Out Of Order
         IF (ISSTAT(19) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
         END IF
C        Process Particle Deposition Parameters             ---   CALL PARTDEP
         CALL PARTDEP
      ELSE IF (KEYWRD .EQ. 'GAS-SCAV') THEN
C        Set Status Switch
         ISSTAT(14) = ISSTAT(14) + 1
C        Check for SRCGROUP Card Out Of Order
         IF (ISSTAT(19) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
         END IF
C        Process Wet Deposition Parameters for gases        ---   CALL GASDEP
         CALL GASDEP

C*----   ISCSTM Modification: allow for hourly emissions - jah 11/3/94
      ELSE IF (KEYWRD .EQ. 'HOUREMIS') THEN
C*       Set Status Switch
         ISSTAT(16) = ISSTAT(16) + 1
C        Check for SRCGROUP Card Out Of Order
         IF (ISSTAT(19) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
         END IF
C*       Process Hourly Emissions                           ---   CALL HREMIS
         CALL HREMIS
C*#

      ELSE IF (KEYWRD .EQ. 'SRCGROUP') THEN
C        Set Status Switch
         ISSTAT(19) = ISSTAT(19) + 1
C        Process Source Groups                              ---   CALL SOGRP
         CALL SOGRP
      ELSE IF (KEYWRD .EQ. 'ELEVUNIT') THEN
C        Set Status Switch
         ISSTAT(15) = ISSTAT(15) + 1
C        Check for SRCGROUP Card Out Of Order
         IF (ISSTAT(19) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
         END IF
         IF (ISSTAT(15) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE IF (NUMSRC .GT. 0) THEN
C           Write Error Message: ELEVUNIT must be first card after STARTING
            CALL ERRHDL(PATH,MODNAM,'E','152','  SO')
         ELSE IF (ICSTAT(10) .NE. 0) THEN
C           Write Error Message: Use of obsolescent CO ELEVUNIT card with
C           SO ELEVUNIT card
            CALL ERRHDL(PATH,MODNAM,'E','153',' SO Path')
         ELSE
C           Process Elevation Units for Source Elevations   ---   CALL SOELUN
            CALL SOELUN
         END IF
      ELSE IF (KEYWRD .EQ. 'CONCUNIT') THEN
C        Set Status Switch
         ISSTAT(17) = ISSTAT(17) + 1
C        Check for SRCGROUP Card Out Of Order
         IF (ISSTAT(19) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
         END IF
         IF (ISSTAT(17) .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE IF (ISSTAT(8) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','158',KEYWRD)
         ELSE
C           Process Emission Rate Unit Conversion Factors   ---   CALL COUNIT
            CALL COUNIT
         END IF
      ELSE IF (KEYWRD .EQ. 'DEPOUNIT') THEN
C        Set Status Switch
         ISSTAT(18) = ISSTAT(18) + 1
C        Check for SRCGROUP Card Out Of Order
         IF (ISSTAT(19) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
         END IF
         IF (ISSTAT(18) .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE IF (ISSTAT(8) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','158',KEYWRD)
         ELSE
C           Process Emission Rate Unit Conversion Factors   ---   CALL DPUNIT
            CALL DPUNIT
         END IF
      ELSE IF (KEYWRD .EQ. 'FINISHED') THEN
C        Set Status Switch
         ISSTAT(20) = ISSTAT(20) + 1
         IF (ISSTAT(20) .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         END IF

C        Check for Missing Mandatory Keywords
         IF (ISSTAT(1) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','STARTING')
         END IF
         IF (ISSTAT(2) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','LOCATION')
         END IF
         IF (ISSTAT(3) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','SRCPARAM')
         END IF
         IF (ISSTAT(19) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','SRCGROUP')
         END IF
         IF (ISSTAT(3) .LT. ISSTAT(2)) THEN
C           Must Be Missing a SRCPARAM Card for One or More Sources
            CALL ERRHDL(PATH,MODNAM,'E','130','SRCPARAM')
         END IF

C        Check to Insure That SRCGROUP Was The Last Functional Keyword
         IF (PKEYWD .NE. 'SRCGROUP') THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
         END IF

         IF (NUMSRC .EQ. 0) THEN
C           WRITE Error Message:  No Sources Input
            CALL ERRHDL(PATH,MODNAM,'E','248','NUMSRC=0')
         ELSE
C           Quality Assure Source Parameter Inputs          ---   CALL SRCQA
            CALL SRCQA
C           Calculate settling velocity and related time-invariant
C           deposition data                                 ---   CALL VDP1
            IF (LDPART) then
               CALL VDP1
            END IF
         END IF

      ELSE
C        Write Error Message: Invalid Keyword for This Pathway
         CALL ERRHDL(PATH,MODNAM,'E','110',KEYWRD)
      END IF

 999  RETURN
      END

      SUBROUTINE SRCQA
C***********************************************************************
C                 SRCQA Module of ISC2 Model
c ----------------------------------------------------------------------
c ---    ISC-PRIME     Version 1.0    Level 970812              Modified
c ---        V. Tino
c ---        Earth Tech, Inc.
c            Prepared for EPRI under contract WO3527-01
c ----------------------------------------------------------------------
C
C        PURPOSE: Quality Assure Source Parameter Inputs
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C        MODIFIED BY D. Strimaitis, SRC (for WET & DRY DEPOSITION)
C
C        DATE:    November 8, 1993
C
C        MODIFIED BY D. Strimaitis, SRC (for DEPOSITION)
C        (DATE:    February 15, 1993)
C
C        INPUTS:  Source Parameters
C                 Source Parameters Array Limits, IWRK2(NSRC,9)
C
C        OUTPUTS: Source Parameter Error Messages
C
C        CALLED FROM:   SOCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'SRCQA'

C     Begin Source LOOP
      DO 40 I = 1, NUMSRC

C        Check Source Array Limits for Too Few Values;
C        (Too Many Checked In DSFILL and EFFILL)
         IF (IWRK2(I,1).NE.0 .AND. IWRK2(I,1).LT.NSEC) THEN
C           WRITE Error Message:  Not Enough BUILDHGTs
            CALL ERRHDL(PATH,MODNAM,'E','236',SRCID(I))
         END IF
         IF (IWRK2(I,2).NE.0 .AND. IWRK2(I,2).LT.NSEC) THEN
C           WRITE Error Message:  Not Enough BUILDWIDs
            CALL ERRHDL(PATH,MODNAM,'E','237',SRCID(I))
         END IF
         IF (IWRK2(I,3).NE.0 .AND. IWRK2(I,3).LT.NSEC) THEN
C           WRITE Error Message:  Not Enough LOWBOUNDs
            CALL ERRHDL(PATH,MODNAM,'E','238',SRCID(I))
         END IF

c --- PRIME -------------------------------------------------
         IF (IWRK2(I,10).NE.0 .AND. IWRK2(I,10).LT.NSEC) THEN
C           WRITE Error Message:  Not Enough BUILDLENs
            CALL ERRHDL(PATH,MODNAM,'E','241',SRCID(I))
         END IF
         IF (IWRK2(I,11).NE.0 .AND. IWRK2(I,11).LT.NSEC) THEN
C           WRITE Error Message:  Not Enough XBADJs
            CALL ERRHDL(PATH,MODNAM,'E','246',SRCID(I))
         END IF
         IF (IWRK2(I,12).NE.0 .AND. IWRK2(I,12).LT.NSEC) THEN
C           WRITE Error Message:  Not Enough YBADJs
            CALL ERRHDL(PATH,MODNAM,'E','247',SRCID(I))
         END IF
c -----------------------------------------------------------

         IF (QFLAG(I) .NE. ' ') THEN
            IF (QFLAG(I).EQ.'SEASON' .AND. IWRK2(I,4).LT.4) THEN
C              WRITE Error Message: Not Enough QFACTs
               CALL ERRHDL(PATH,MODNAM,'E','239',SRCID(I))
            ELSE IF (QFLAG(I).EQ.'MONTH' .AND. IWRK2(I,4).LT.12) THEN
C              WRITE Error Message: Not Enough QFACTs
               CALL ERRHDL(PATH,MODNAM,'E','239',SRCID(I))
            ELSE IF(QFLAG(I).EQ.'HROFDY' .AND. IWRK2(I,4).LT.24) THEN
C              WRITE Error Message: Not Enough QFACTs
               CALL ERRHDL(PATH,MODNAM,'E','239',SRCID(I))
            ELSE IF (QFLAG(I).EQ.'STAR' .AND. IWRK2(I,4).LT.36) THEN
C              WRITE Error Message: Not Enough QFACTs
               CALL ERRHDL(PATH,MODNAM,'E','239',SRCID(I))
            ELSE IF(QFLAG(I).EQ.'SEASHR' .AND. IWRK2(I,4).LT.96) THEN
C              WRITE Error Message: Not Enough QFACTs
               CALL ERRHDL(PATH,MODNAM,'E','239',SRCID(I))
            END IF
         END IF

C        Check Settling and Removal Parameters
         IF (IWRK2(I,5).NE.0 .OR. IWRK2(I,6).NE.0 .OR.
     &       IWRK2(I,7).NE.0 .OR. IWRK2(I,8).NE.0 .OR.
     &                            IWRK2(I,9).NE.0) THEN
C           Set Number of Particle Diameter Categories for This Source
            INPD(I) = IWRK2(I,5)
C           Check for Consistent Number of Categories for All Parameters
            IF (IWRK2(I,5).NE.IWRK2(I,6) .OR.
     &          IWRK2(I,5).NE.IWRK2(I,7)) THEN
C              WRITE Error Message: PartDiam Categories Don't Match
               CALL ERRHDL(PATH,MODNAM,'E','240',SRCID(I))
            ELSE IF (DEPOS .OR. WDEP .OR. WDPLETE) THEN
               IF (IWRK2(I,5).NE.IWRK2(I,8) .OR.
     &             IWRK2(I,5).NE.IWRK2(I,9)) THEN
C                 WRITE Error Message: PartDiam Categories Don't Match
                  CALL ERRHDL(PATH,MODNAM,'E','240',SRCID(I))
               END IF
            END IF
C           Check for Mass Fraction Summing to 1.0 (+/- 2%)
            ATOT = 0.0
            N = INPD(I)
            IF (N .LE. NPDMAX) THEN
               DO 30 J = 1, N
                  ATOT = ATOT + APHI(J,I)
 30            CONTINUE
               IF (ATOT .LT. 0.98 .OR. ATOT .GT. 1.02) THEN
C                 WRITE Error Message: Mass Fractions Don't Sum to 1.0
                  CALL ERRHDL(PATH,MODNAM,'W','330',SRCID(I))
               END IF
               DO 35 J = 1, N
                  IF ((DEPOS.OR.WDEP.OR.WDPLETE) .AND.
     &                      (APSLIQ(J,I).EQ.0.0)) THEN
C                    WRITE Warning Message: Missing or Invalid Scavenging Coef.
                     CALL ERRHDL(PATH,MODNAM,'W','243',SRCID(I))
                  END IF
 35            CONTINUE
            ELSE
C              WRITE Error Message:  Too Many Settling/Removal Categories
               CALL ERRHDL(PATH,MODNAM,'E','244',SRCID(I))
            END IF

C        Check for OPENPIT source type with no particle categories
         ELSE IF (SRCTYP(I) .EQ. 'OPENPIT') THEN
C           WRITE Error Message: Open Pit source with no particle categories
            CALL ERRHDL(PATH,MODNAM,'E','323',SRCID(I))
         END IF

C        Screen for Conflicts with the use of Particles
         IF (INPD(I) .EQ. 0) THEN
C           Check for NPD=0 with the DDEP option (DDEP is active only for
C           particles at this time, not for gases!)
            IF (DEPOS .OR. DDEP .OR. DDPLETE) THEN
C              WRITE Error Message for Lack of Settling/Removal Categories
               CALL ERRHDL(PATH,MODNAM,'W','242',SRCID(I))
            END IF
            IF (DEPOS .OR. WDEP .OR. WDPLETE) THEN
               IF (AGSCAV(1,I) .LE. 0.0) THEN
C                 WRITE Error Message:  Missing or Invalid Scavenging Coef.
                  CALL ERRHDL(PATH,MODNAM,'W','243',SRCID(I))
               END IF
            END IF
         ELSE
C           Check for NPD .NE. 0 and Gas Scavenging Coefficient .NE. 0 for
C           this source (A source may be either particles, or gas, but
C           not both !)
            IF((AGSCAV(1,I) .GT. 0.0) .OR. (AGSCAV(2,I) .GT. 0.0)) THEN
C              WRITE Error Message:  Too Many Settling/Removal Categories
               CALL ERRHDL(PATH,MODNAM,'E','244',SRCID(I))
            END IF
         END IF

 40   CONTINUE
C     End Source LOOP

      RETURN
      END

      SUBROUTINE DSFILL(ISDX)
C***********************************************************************
C                 DSFILL Module of ISC2 Model
c ----------------------------------------------------------------------
c ---    ISC-PRIME     Version 1.0    Level 970812              Modified
c ---        V. Tino
c ---        Earth Tech, Inc.
c            Prepared for EPRI under contract WO3527-01
c ----------------------------------------------------------------------
C
C        PURPOSE: Fill Direction-specific Building Dimension Arrays
C
C        PROGRAMMER:  Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Direction Specific Building Directions
C
C        CALLED FROM:   DSBLDG
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'DSFILL'

      IF (KEYWRD .EQ. 'BUILDHGT') THEN
         ISET = IWRK2(ISDX,1)
         DO 200 K = 4, IFC
C           Change Fields To Numbers
            CALL STONUM(FIELD(K),40,FNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 200
            END IF
            DO 100 J = 1, IMIT
               ISET = ISET + 1
C              Assign The Field
               IF (ISET .LE. NSEC) THEN
                  ADSBH(ISET,ISDX) = FNUM
                  IF (FNUM .LT. 0.0) THEN
C                    WRITE Error Message:  Negative Value for ADSBH
                     CALL ERRHDL(PATH,MODNAM,'E','209',KEYWRD)
                  END IF
               ELSE
C                 WRITE Error Message    ! Too Many Sectors Input
                  CALL ERRHDL(PATH,MODNAM,'E','234',KEYWRD)
               END IF
 100        CONTINUE
 200     CONTINUE
         IWRK2(ISDX,1) = ISET
      ELSE IF (KEYWRD .EQ. 'BUILDWID') THEN
         ISET = IWRK2(ISDX,2)
         DO 400 K = 4, IFC
C           Change Fields To Numbers
            CALL STONUM(FIELD(K),40,FNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 400
            END IF
            DO 300 J = 1, IMIT
               ISET = ISET + 1
C              Assign The Field
               IF (ISET .LE. NSEC) THEN
                  ADSBW(ISET,ISDX) = FNUM
                  IF (FNUM .LT. 0.0) THEN
C                    WRITE Error Message:  Negative Value for ADSBW
                     CALL ERRHDL(PATH,MODNAM,'E','209',KEYWRD)
                  END IF
               ELSE
C                 WRITE Error Message    ! Too Many Sectors Input
                  CALL ERRHDL(PATH,MODNAM,'E','234',KEYWRD)
               END IF
 300        CONTINUE
 400     CONTINUE
         IWRK2(ISDX,2) = ISET
      ELSE IF (KEYWRD .EQ. 'LOWBOUND') THEN
         ISET = IWRK2(ISDX,3)
         DO 600 K = 4, IFC
C           Change Fields To Numbers
            CALL STONUM(FIELD(K),40,FNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 600
            END IF
            DO 500 J = 1, IMIT
               ISET = ISET + 1
C              Assign The Field
               IF (ISET .LE. NSEC) THEN
                  IDSWAK(ISET,ISDX) = INT(FNUM)
                  IF (INT(FNUM) .NE. 0 .AND. INT(FNUM) .NE. 1) THEN
C                    WRITE Error Message:  Invalid Parameter for IDSWAK
                     CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  END IF
               ELSE
C                 WRITE Error Message    ! Too Many Sectors Input
                  CALL ERRHDL(PATH,MODNAM,'E','234',KEYWRD)
               END IF
 500        CONTINUE
 600     CONTINUE
         IWRK2(ISDX,3) = ISET

c --- PRIME --------------------------------------------
c --- Fill building length information
      ELSE IF (KEYWRD .EQ. 'BUILDLEN') THEN
         ISET = IWRK2(ISDX,10)
         DO 800 K = 4, IFC
C           Change Fields To Numbers
            CALL STONUM(FIELD(K),40,FNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 800
            END IF
            DO 700 J = 1, IMIT
               ISET = ISET + 1
C              Assign The Field
               IF (ISET .LE. NSEC) THEN
                  ADSBL(ISET,ISDX) = FNUM
                  IF (FNUM .LT. 0.0) THEN
C                    WRITE Error Message:  Negative value for ADSBL
                     CALL ERRHDL(PATH,MODNAM,'E','209',KEYWRD)
                  END IF
               ELSE
C                 WRITE Error Message    ! Too Many Sectors Input
                  CALL ERRHDL(PATH,MODNAM,'E','234',KEYWRD)
               END IF
 700        CONTINUE
 800     CONTINUE
         IWRK2(ISDX,10) = ISET

c --- Fill building XBADJ information
      ELSE IF (KEYWRD .EQ. 'XBADJ   ') THEN
         ISET = IWRK2(ISDX,11)
         DO 900 K = 4, IFC
C           Change Fields To Numbers
            CALL STONUM(FIELD(K),40,FNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 900
            END IF
            DO 1000 J = 1, IMIT
               ISET = ISET + 1
C              Assign The Field
               IF (ISET .LE. NSEC) THEN
                  ADSXADJ(ISET,ISDX) = FNUM
               ELSE
C                 WRITE Error Message    ! Too Many Sectors Input
                  CALL ERRHDL(PATH,MODNAM,'E','234',KEYWRD)
               END IF
1000        CONTINUE
 900     CONTINUE
         IWRK2(ISDX,11) = ISET

c --- Fill building YBADJ information
      ELSE IF (KEYWRD .EQ. 'YBADJ   ') THEN
         ISET = IWRK2(ISDX,12)
         DO 1200 K = 4, IFC
C           Change Fields To Numbers
            CALL STONUM(FIELD(K),40,FNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 1200
            END IF
            DO 1100 J = 1, IMIT
               ISET = ISET + 1
C              Assign The Field
               IF (ISET .LE. NSEC) THEN
                  ADSYADJ(ISET,ISDX) = FNUM
               ELSE
C                 WRITE Error Message    ! Too Many Sectors Input
                  CALL ERRHDL(PATH,MODNAM,'E','234',KEYWRD)
               END IF
1100        CONTINUE
1200     CONTINUE
         IWRK2(ISDX,12) = ISET
c --------------------------------------------------------

      END IF

 999  RETURN
      END

      SUBROUTINE PRTSRC
C***********************************************************************
C                 PRTSRC Module of ISC2 Short Term Model - ISCST2
c ----------------------------------------------------------------------
c ---    ISC-PRIME     Version 1.0    Level 970812              Modified
c ---        V. Tino
c ---        Earth Tech, Inc.
c            Prepared for EPRI under contract WO3527-01
c ----------------------------------------------------------------------
C
C        PURPOSE: Print Out The Input Source Data Summary
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        MODIFIED BY D. Strimaitis, SRC (for Wet & Dry DEPOSITION)
C
C        DATE:    November 8, 1993
C
C        MODIFIED by YICHENG ZHUANG, SRC to combine version 93188 with
C                 version 93046 - 9/28/93
C
C        MODIFIED BY D. Strimaitis, SRC (for DEPOSITION) - 2/25/93
C
C*       MODIFIED BY PES (for OPENPIT Source) - 7/22/94
C
C        INPUTS:  Model Options and Keyword Summarys
C
C        OUTPUTS: Printed Model Outputs
C
C        CALLED FROM:   INPSUM
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      CHARACTER BLDING*3, IQUN*12
      CHARACTER ATHRUF(6)*1, SEASON(4)*6

C     Variable Initializations
      DATA ATHRUF / 'A','B','C','D','E','F' /
      DATA SEASON /'WINTER','SPRING','SUMMER',' FALL '/
      MODNAM = 'PRTSRC'

      IF (ISSTAT(8) .EQ. 0) THEN
C        Write Default Emission Rate Units
         IQUN = ' (GRAMS/SEC)'
      ELSE
         IQUN = '(USER UNITS)'
      END IF

C     Write Out The Point Source Data, If Any
      INDC = 0
      DO 600 I = 1, NUMSRC
         IF (SRCTYP(I) .EQ. 'POINT') THEN
            INDC = INDC + 1
            BLDING = 'NO'
            DO 500 J = 1, NSEC

c --- PRIME -------------------------------------------------------
C prm          IF (ADSBH(J,I).NE.0.0 .AND. ADSBW(J,I).NE.0.0) THEN
               IF (ADSBH(J,I).NE.0.0 .AND. ADSBW(J,I).NE.0.0 .AND.
     &             ADSBL(J,I).NE.0.0 .AND. ADSXADJ(J,I).NE.0.0 .AND.
     &             ADSYADJ(J,I).NE.0.0) THEN
c -----------------------------------------------------------------

                  BLDING = 'YES'
               END IF
 500        CONTINUE
            IF (MOD(INDC-1,40) .EQ. 0) THEN
               CALL HEADER
               WRITE(IOUNIT,9046) IQUN
            END IF
            WRITE(IOUNIT,9047) SRCID(I),INPD(I),AQS(I),
     &              AXS(I),AYS(I),AZS(I),AHS(I),ATS(I),AVS(I),ADS(I),
     &              BLDING,QFLAG(I)
         END IF
 600  CONTINUE

C     Write Out The Volume Source Data, If Any
      INDC = 0
      DO 610 I = 1, NUMSRC
         IF (SRCTYP(I) .EQ. 'VOLUME') THEN
            INDC = INDC + 1
            IF (MOD(INDC-1,40) .EQ. 0) THEN
               CALL HEADER
               WRITE(IOUNIT,9074) IQUN
            END IF
            WRITE(IOUNIT,9075) SRCID(I),INPD(I),AQS(I),
     &              AXS(I),AYS(I),AZS(I),AHS(I),ASYINI(I),ASZINI(I),
     &              QFLAG(I)
         END IF
 610  CONTINUE

C     Write Out The Area Source Data, If Any
      INDC = 0
      DO 620 I = 1, NUMSRC
         IF (SRCTYP(I) .EQ. 'AREA') THEN
            INDC = INDC + 1
            IF (MOD(INDC-1,40) .EQ. 0) THEN
               CALL HEADER
               WRITE(IOUNIT,9076) IQUN
            END IF
            WRITE(IOUNIT,9077) SRCID(I),INPD(I),AQS(I),
     &              AXS(I),AYS(I),AZS(I),AHS(I),AXINIT(I),AYINIT(I),
     &              AANGLE(I), ASZINI(I), QFLAG(I)
C*----
         END IF

 620  CONTINUE

C*    Write Out The OpenPit Source Data, If Any
      INDC = 0
      DO 625 I = 1, NUMSRC
         IF (SRCTYP(I) .EQ. 'OPENPIT') THEN
            INDC = INDC + 1
            IF (MOD(INDC-1,40) .EQ. 0) THEN
               CALL HEADER
               WRITE(IOUNIT,9078) IQUN
            END IF
            WRITE(IOUNIT,9079) SRCID(I),INPD(I),AQS(I),
     &              AXS(I),AYS(I),AZS(I),AHS(I),AXINIT(I),AYINIT(I),
     &              AANGLE(I), AVOLUM(I), QFLAG(I)
         END IF
 625  CONTINUE

C     Print The Source Group IDs with Source IDs
      ICNT = 12
      DO 200 J = 1, NUMGRP
         INGRP = 0
         DO 210 K = 1, NUMSRC
            IF (IGROUP(K,J) .EQ. 1) THEN
               INGRP = INGRP + 1
               WORKID(INGRP) = SRCID(K)
            END IF
  210    CONTINUE
C        Determine Number of Lines @ 12/Line
         NL = 1 + INT((INGRP-1)/12)
         ICNT = ICNT + 2*NL
         IF (J .EQ. 1 .OR. ICNT .GT. 55) THEN
            CALL HEADER
            WRITE(IOUNIT,9058)
            IF (J .NE. 1) ICNT = 12 + 2*NL
         END IF
         DO 202 K = 1, NL
            IF (K .EQ. 1 .AND. K .EQ. NL) THEN
               WRITE(IOUNIT,9068) GRPID(J), (WORKID(I),I=1,INGRP)
            ELSE IF (K .EQ. 1 .AND. K .NE. NL) THEN
               WRITE(IOUNIT,9068) GRPID(J), (WORKID(I),I=1,12*K)
            ELSE IF (K .EQ. NL) THEN
               WRITE(IOUNIT,9067) (WORKID(I),I=1+12*(K-1),INGRP)
            ELSE
               WRITE(IOUNIT,9067) (WORKID(I),I=1+12*(K-1),12*K)
            END IF
  202    CONTINUE
  200 CONTINUE

      INDC = 0
C     Print Out Wet or Dry Deposition Information.
      DO 720 I = 1, NUMSRC
         NPD = INPD(I)
         IF (NPD .NE. 0) THEN
            INDC = INDC + 1
            IF (MOD(INDC-1,3) .EQ. 0) THEN
               CALL HEADER
               WRITE(IOUNIT,9049)
            END IF
            WRITE(IOUNIT,9050) SRCID(I), SRCTYP(I)
            WRITE(IOUNIT,9051) (APHI(J,I),J=1,NPD)
            WRITE(IOUNIT,9052) (APDIAM(J,I),J=1,NPD)
            WRITE(IOUNIT,9053) (APDENS(J,I),J=1,NPD)
            IF (LWPART) THEN
               WRITE(IOUNIT,9054) (APSLIQ(J,I),J=1,NPD)
               WRITE(IOUNIT,9055) (APSICE(J,I),J=1,NPD)
            END IF
         ELSE IF (LWGAS) THEN
            INDC = INDC + 1
            IF (MOD(INDC-1,3) .EQ. 0) THEN
               CALL HEADER
               WRITE(IOUNIT,9049)
            END IF
            WRITE(IOUNIT,9050) SRCID(I), SRCTYP(I)
            WRITE(IOUNIT,9054) AGSCAV(1,I)
            WRITE(IOUNIT,9055) AGSCAV(2,I)

         END IF
 720  CONTINUE

      INDC = 0
C     Write Out Direction Specific Bldg. Dimensions, If Present
      DO 630 I = 1, NUMSRC
         BLDING = 'NO'
         DO 650 J = 1, NSEC

c --- PRIME ---------------------------------------------------
c prm       IF (ADSBH(J,I).NE.0.0 .AND. ADSBW(J,I).NE.0.0) THEN
c prm - ONLY BLDG DIMENSIONS MUST BE NONZERO HERE!
            IF (ADSBH(J,I).NE.0.0 .AND. ADSBW(J,I).NE.0.0 .AND.
     &          ADSBL(J,I).NE.0.0) THEN
c -------------------------------------------------------------

               BLDING = 'YES'
            END IF
 650     CONTINUE
         IF (BLDING .EQ. 'YES') THEN
            INDC = INDC + 1
C           Print Out Direction Specific Bldg. Dimensions
            IF (MOD(INDC-1,4) .EQ. 0) THEN
               CALL HEADER
               WRITE(IOUNIT,9064)
            END IF

c --- PRIME ----------------------------------------------------------
            WRITE(IOUNIT,9062) SRCID(I),
C prm&           (J,ABS(ADSBH(J,I)),ADSBW(J,I),IDSWAK(J,I), J=1,NSEC)
     &           (J,ABS(ADSBH(J,I)),ADSBW(J,I),ADSBL(J,I),ADSXADJ(J,I),
     &            ADSYADJ(J,I),IDSWAK(J,I),
     &            J=1,NSEC)
c --------------------------------------------------------------------
         END IF
 630  CONTINUE

C     Print Source Emission Rate Scalars.
      INDC = 0
      DO 1050 I = 1, NUMSRC
         IF (QFLAG(I) .EQ. 'SEASON') THEN
            INDC = INDC + 1
            IF (MOD(INDC-1,6) .EQ. 0) THEN
               CALL HEADER
               WRITE(IOUNIT,9002)
               WRITE(IOUNIT,9004) (SEASON(I1),I1=1,4)
            END IF
            WRITE(IOUNIT,9005) SRCID(I),SRCTYP(I)
            WRITE(IOUNIT,9006) (QFACT(I1,I),I1=1,4)
         END IF
 1050 CONTINUE

      INDC = 0
      DO 1060 I = 1, NUMSRC
         IF (QFLAG(I) .EQ. 'MONTH') THEN
            INDC = INDC + 1
            IF (MOD(INDC-1,6) .EQ. 0) THEN
               CALL HEADER
               WRITE(IOUNIT,9007)
               WRITE(IOUNIT,9008)
               WRITE(IOUNIT,9013)
            END IF
            WRITE(IOUNIT,9009) SRCID(I),SRCTYP(I)
            WRITE(IOUNIT,9010) (QFACT(I1,I),I1=1,12)
         END IF
 1060 CONTINUE

      INDC = 0
      DO 1070 I = 1, NUMSRC
         IF (QFLAG(I) .EQ. 'HROFDY') THEN
            INDC = INDC + 1
            IF (MOD(INDC-1,5) .EQ. 0) THEN
               CALL HEADER
               WRITE(IOUNIT,9011)
               WRITE(IOUNIT,9012)
               WRITE(IOUNIT,9013)
            END IF
            WRITE(IOUNIT,9009) SRCID(I),SRCTYP(I)
            WRITE(IOUNIT,9014) (I1,QFACT(I1,I),I1=1,24)
         END IF
 1070 CONTINUE

      INDC = 0
      DO 1080 I = 1, NUMSRC
         IF (QFLAG(I) .EQ. 'STAR') THEN
            INDC = INDC + 1
            IF (MOD(INDC-1,3) .EQ. 0) THEN
               CALL HEADER
               WRITE(IOUNIT,9015)
               WRITE(IOUNIT,9013)
            END IF
            WRITE(IOUNIT,9009) SRCID(I),SRCTYP(I)
            WRITE(IOUNIT,9025) (J, J=1,6)
            DO 760  I1 = 1,6
               IFR = (I1-1)*6 + 1
               ITO = IFR + 5
               WRITE(IOUNIT,9024) ATHRUF(I1),
     &               (QFACT(I2,I),I2=IFR,ITO)
 760        CONTINUE
         END IF
 1080 CONTINUE

      INDC = 0
      DO 1090 I = 1, NUMSRC
         IF (QFLAG(I) .EQ. 'SEASHR') THEN
            INDC = INDC + 1
C            IF (MOD(INDC-1,1) .EQ. 0) THEN
               CALL HEADER
               WRITE(IOUNIT,9018)
               WRITE(IOUNIT,9012)
               WRITE(IOUNIT,9013)
C            END IF
            WRITE(IOUNIT,9009) SRCID(I),SRCTYP(I)
            DO 790  I1 = 1, 4
               IFR = (I1-1)*24
               WRITE(IOUNIT,9019) SEASON(I1)
               WRITE(IOUNIT,9014) (I2,QFACT(I2+IFR,I),I2=1,24)
 790        CONTINUE
         END IF
 1090 CONTINUE

 9002 FORMAT(39X,'* SOURCE EMISSION RATE SCALARS WHICH VARY SEASONALLY',
     &       ' *'//)
 9003 FORMAT(56X,'* FOR ALL SOURCES *'//)
 9004 FORMAT(40X,4(A6,9X)/20X,40('- ')/)
 9005 FORMAT(/10X,' SOURCE ID = ',A8,' ;  SOURCE TYPE = ',A8,' :')
 9006 FORMAT(38X,4(E10.5,5X))
 9007 FORMAT(41X,'* SOURCE EMISSION RATE SCALARS WHICH VARY MONTHLY *',
     &       //)
 9008 FORMAT(7X,'JANUARY  FEBRUARY   MARCH     APRIL      MAY       ',
     &  'JUNE      JULY     AUGUST   SEPTEMBER  OCTOBER  NOVEMBER  ',
     &  'DECEMBER'/)
 9009 FORMAT(/' SOURCE ID = ',A8,' ;  SOURCE TYPE = ',A8,' :')
 9010 FORMAT(5X,12E10.4)
 9011 FORMAT(28X,'* SOURCE EMISSION RATE SCALARS WHICH VARY FOR EACH',
     &       ' HOUR OF THE DAY *'//)
 9012 FORMAT(5X,6('HOUR    SCALAR',6X))
 9013 FORMAT(1X,65('- ')/)
 9014 FORMAT(4(5X,6(I3,3X,E10.5,4X)/))
 9015 FORMAT(20X,'* SOURCE EMISSION RATE SCALARS WHICH VARY WITH',
     &       ' STABILITY AND WIND SPEED (STAR) *'//)
 9017 FORMAT(19X,A1,5X,6(5X,E10.5))
 9018 FORMAT(22X,'* SOURCE EMISSION RATE SCALARS WHICH VARY',
     &       ' SEASONALLY AND DIURNALLY (SEASHR) *'//)
 9019 FORMAT(59X,'SEASON = ',A6)
 9024 FORMAT(6X,'STABILITY CATEGORY ',A1,6(1X,E12.5))
 9025 FORMAT(/26X,6('   WIND SPEED')/26X,6('   CATEGORY',I2))
 9046 FORMAT(//50X,'*** POINT SOURCE DATA ***'///14X,
     & 'NUMBER EMISSION RATE',20X,'BASE     STACK   STACK',4X,
     & 'STACK     STACK    BUILDING EMISSION RATE',/4X,
     & 'SOURCE',5X,'PART. ',A12,5X,'X',8X,'Y',6X,'ELEV.    ',
     & 'HEIGHT  TEMP.   EXIT VEL. DIAMETER',3X,'EXISTS   SCALAR VARY',
     & /4X,'  ID       CATS.              ',
     & 1X,2('(METERS) (METERS) '),'(DEG.K) ',' (M/SEC) ',1X,'(METERS)',
     & 16X,'BY'/61(' -')/)
 9047 FORMAT(3X,A8,2X,I5,3X,E11.5,2F10.1,F8.1,4F9.2,
     &       6X,A3,6X,A6)
 9049 FORMAT(48X,'*** SOURCE PARTICULATE/GAS DATA ***'//)
 9050 FORMAT(//10X,'*** SOURCE ID = ',A8,'; SOURCE TYPE = ',A8,' ***')
 9051 FORMAT(/10X,'MASS FRACTION ='/2(10X,10(F9.5,', ')/))
 9052 FORMAT(/10X,'PARTICLE DIAMETER (MICRONS) ='/2(10X,10(F9.5,', ')
     &       /))
 9053 FORMAT(/10X,'PARTICLE DENSITY (G/CM**3) ='/2(10X,10(F9.5,', '
     &       )/))
 9054 FORMAT(/10X,'SCAV COEF [LIQ] 1/(S-MM/HR)='/2(10X,10(E9.2,', '
     &       )/))
 9055 FORMAT(/10X,'SCAV COEF [ICE] 1/(S-MM/HR)='/2(10X,10(E9.2,', '
     &       )/))
 9058 FORMAT(//43X,'*** SOURCE IDs DEFINING SOURCE GROUPS ***'//
     &       1X,'GROUP ID',49X,'SOURCE IDs'/)
 9068 FORMAT(//2X,A8,1X,12(1X,A8,','))
 9067 FORMAT(/11X,12(1X,A8,','))

c --- PRIME --------------------------------------------------
 9062 FORMAT(/' SOURCE ID: ',A8,
     &    /,2('  IFV  BH     BW     BL    XADJ   YADJ    WAK')
     &    ,/,18(2(2X,I3,5(F6.1,','),i3)/))
c prm&       /,6('  IFV   BH     BW  WAK'),/,
c prm&       6(6(2X,I3,F6.1,',',F6.1,',',I2,1X)/)/)
c ------------------------------------------------------------

 9064 FORMAT(42X,'*** DIRECTION SPECIFIC BUILDING DIMENSIONS ***'/)
 9074 FORMAT(//50X,'*** VOLUME SOURCE DATA ***'//14X,
     & 'NUMBER EMISSION RATE',20X,'BASE    RELEASE    INIT.',4X,
     & 'INIT.   EMISSION RATE',/4X,
     & 'SOURCE',5X,'PART. ',A12,5X,'X',8X,'Y',6X,'ELEV.   ',
     & 'HEIGHT      SY       SZ      SCALAR VARY',
     & /4X,'  ID       CATS.              ',
     & 1X,3('(METERS) (METERS) '),5X,'  BY'/61(' -')/)
 9075 FORMAT(3X,A8,2X,I5,3X,E11.5,2F10.1,F8.1,F9.2,1X,F8.2,1X,
     &       F8.2,3X,A6)
 9076 FORMAT(//50X,'*** AREA SOURCE DATA ***'//14X,
     & 'NUMBER EMISSION RATE',2X,'COORD (SW CORNER)',2X,
     & 'BASE     RELEASE  X-DIM     Y-DIM    ORIENT.',4X,
     & 'INIT.',2X,
     & 'EMISSION RATE',
     & /4X,'SOURCE',5X,'PART. ',A11,7X,'X',8X,'Y',6X,'ELEV.    ',
     & 'HEIGHT  OF AREA   OF AREA   OF AREA     SZ     SCALAR VARY',
     & /4X,'  ID       CATS.   /METER**2)  ',
     & 1X,2('(METERS) (METERS) '),2('(METERS)',2X),' (DEG.)  (METERS)',
     & 6X,'BY'/63(' -')/)
 9077 FORMAT(3X,A8,2X,I5,3X,E11.5,2F10.1,F8.1,F9.2,3(1X,F9.2),1X,F8.2,
     &       5X,A6)
 9078 FORMAT(//50X,'*** OPENPIT SOURCE DATA ***'//14X,
     & 'NUMBER EMISSION RATE',2X,'COORD (SW CORNER)',2X,
     & 'BASE     RELEASE  X-DIM     Y-DIM    ORIENT.',4X,
     & 'VOLUME',3X,'EMISSION RATE',
     & /4X,'SOURCE',5X,'PART. ',A11,7X,'X',8X,'Y',6X,'ELEV.    ',
     & 'HEIGHT  OF PIT    OF PIT    OF PIT     OF PIT    SCALAR VARY',
     & /4X,'  ID       CATS.   /METER**2)  ',
     & 1X,2('(METERS) (METERS) '),2('(METERS)',2X),' (DEG.) ',3X,
     & '(M**3)        BY'
     & /63(' -')/)
 9079 FORMAT(3X,A8,2X,I5,3X,E11.5,2F10.1,F8.1,F9.2,3(1X,F9.2),
     &       3X,E10.5,3X,A6)

      RETURN
      END


      SUBROUTINE VARINI
C***********************************************************************
C                 VARINI Module of ISC2 Short Term Model - ISCST2
c ----------------------------------------------------------------------
c ---    ISC-PRIME     Version 1.0    Level 970812              Modified
c ---        V. Tino
c ---        Earth Tech, Inc.
c            Prepared for EPRI under contract WO3527-01
c ----------------------------------------------------------------------
C
C        PURPOSE: To Initialize Variables for Setup
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C        MODIFIED BY D. Strimaitis, SRC (for Wet & Dry DEPOSITION, INT.
C                                        TERRAIN, and GRIDDED TERRAIN
C                                        Processing)
C
C        DATE:    December 15, 1993
C
C        MODIFIED BY D. Strimaitis, SRC (for DEPOSITION)
C        (DATE:    February 15, 1993)
C
C        MODIFIED:  To Include TOXXFILE Option - 9/29/92
C
C        INPUTS:  None
C
C        OUTPUTS: Initialized Variables
C
C        CALLED FROM:   MAIN
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'VARINI'

      IPNUM  = 0
      IPPNUM = 0
      IERRST = 0
      IWARN  = 0
      NDUMP  = 0

C     Calculate Sector Width in Radians, DELTHP, Minimum Plume Height,
C     and Terrain Adjustment Factors for COMPLEX1
      DELTHP = 2.*PI/16.
      ZMIN = 10.0
      TCF(1) = 0.5
      TCF(2) = 0.5
      TCF(3) = 0.5
      TCF(4) = 0.5
      TCF(5) = 0.0
      TCF(6) = 0.0

C     Initialize the Logical Control Variables
      FATAL  = .FALSE.
      ISTART = .FALSE.
      IFINIS = .TRUE.
      ERRLST = .FALSE.
      DFAULT = .FALSE.
      CONC   = .FALSE.
      DEPOS  = .FALSE.
C     Add logicals to output just wet or just deposition fluxes
      DDEP   = .FALSE.
      WDEP   = .FALSE.
      RURAL  = .FALSE.
      URBAN  = .FALSE.
      GRDRIS = .FALSE.
      NOSTD  = .FALSE.
      NOBID  = .FALSE.
      NOCALM = .FALSE.
      MSGPRO = .FALSE.
      CLMPRO = .TRUE.
      PERIOD = .FALSE.
      ANNUAL = .FALSE.
      MONTH  = .FALSE.
      FLAT   = .TRUE.
      ELEV   = .FALSE.
      FLGPOL = .FALSE.
      RUN    = .FALSE.
      EVENTS = .FALSE.
      RSTSAV = .FALSE.
      RSTINP = .FALSE.
      MULTYR = .FALSE.
      DAYTAB = .FALSE.
      MXFILE = .FALSE.
      PPFILE = .FALSE.
      PLFILE = .FALSE.
C     Add TXFILE Variable for the TOXXFILE Option, 9/29/92
      TXFILE = .FALSE.
      ANPOST = .FALSE.
      ANPLOT = .FALSE.
      RECERR = .FALSE.
      RUNERR = .FALSE.
      NEWDAY = .TRUE.
      ENDMON = .FALSE.
      USERP  = .FALSE.
      USERDT = .FALSE.
      CALCS  = .FALSE.
      DEBUG  = .FALSE.
      WAKLOW = .FALSE.
      WAKE   = .FALSE.
      WAKESS = .FALSE.
      TALL   = .FALSE.
      SQUAT  = .FALSE.
      SSQUAT = .FALSE.
      ECHO   = .TRUE.
C     Add logicals to identify use wet and dry removal information
      LDPART  = .FALSE.
      LWPART  = .FALSE.
      LWGAS   = .FALSE.
C     Add logicals to control use of Wet & Dry plume depletion
      DDPLETE = .FALSE.
      WDPLETE = .FALSE.
C     Add logicals to control use of Simple and Complex Terrain Models
      NOSMPL = .FALSE.
      NOCMPL = .FALSE.
C     Add logical to control use of Gridded Terrain Processing
      LTGRID = .FALSE.

C*----   ISCSTM Modification: allow for NOCHKD option - jah 11/2/94
      NOCHKD = .FALSE.
C*----
C*#

C     Initialize Decay Coefficient to 0.0 (Urban SO2 Default Set in POLLUT)
      DECOEF = 0.0

C     Initialize the Source Arrays
      ISRC = 0
      DO 10 I = 1, NSRC
         DO 4 J = 1, NWET
            AGSCAV(J,I) = 0.0
 4       CONTINUE
         DO 5 J = 1, NSEC
            ADSBH(J,I)  = 0.0
            ADSBW(J,I)  = 0.0

c --- PRIME ----------------------
            ADSBL(J,I)  = 0.0
            ADSXADJ(J,I)  = 0.0
            ADSYADJ(J,I)  = 0.0
c --------------------------------

            IDSWAK(J,I) = 0
 5       CONTINUE
         DO 6 J = 1, NQF
            QFACT(J,I) = 0.0
 6       CONTINUE
         QFLAG(I) = ' '
         DO 7 J = 1, NGRP
            IGROUP(I,J) = 0
 7       CONTINUE
         INPD(I) = 0
         DO 8 J = 1, NPDMAX
            APDIAM(J,I) = 0.0
            APHI(J,I)   = 0.0
            APDENS(J,I) = 0.0
            APSLIQ(J,I) = 0.0
            APSICE(J,I) = 0.0
 8       CONTINUE
         DO 9 J = 1, NVMAX
            AXVERT(J,I) = 0.0
            AYVERT(J,I) = 0.0
 9       CONTINUE
 10   CONTINUE

C     Counters for the Receptor Groups
      IREC = 0
      ISTA = .FALSE.
      IEND = .FALSE.
      IRXR = 0
      IRYR = 0
      IRZE = 0
      IRZF = 0
      IBND =  36
      IBELEV = 36
      NEWID = .TRUE.
C     Initialize ITAB, NXTOX, NYTOX Variables for the TOXXFILE Option, 9/29/92
      ITAB  = -9
      NXTOX = 0
      NYTOX = 0

C     Initialize Variables Associated with the Meteorology Data
      ISJDAY = 0
      IEJDAY = 366
      ISDATE = 0
      IEDATE = 99999999
      ISYR   = 0
      ISMN   = 0
      ISDY   = 0
      IEYR   = 99
      IEMN   = 99
      IEDY   = 99
      IPDATE = 0
      IPHOUR = 0
      NDAY   = 0
      INCRST = 1
      SFX = 0.0
      SFY = 0.0
      UAX = 0.0
      UAY = 0.0
      ROTANG = 0.0
      AFVLST = 360.
      AFV24  = 360.
      KSTMSG = 9
C     Set Threshold Wind Speed Value For Defining Calm Winds
      UMIN = 0.0

C     Specify Default Met Data Filename and Format.
C     Include u-star,L,z0,zd,ipcode, and prate in the format even
C     though these variables are used (read) only if DEPOSITION is
C     simulated.  The order of met variables is:
C       yr,mo,day,hr,rfvec,ws,tempk,stab,rmix,umix,ustar,el,z0,
C       zd,ipcode,prate

      METINP = ' '
cjop  METFRM = '(4I2,2F9.4,F6.1,I2,2F7.1,f9.4,f10.1,f8.4,f5.1,i4,f7.2)'
      METFRM = '(4I2,2F9.4,F6.1,I2,2F7.1,f9.4,f10.1,f8.4,i4,f7.2)'

C     Initialize the Results Arrays
      DO 95 M = 1, NTYP
         HRVAL(M) = 0.0
         DO 90 L = 1, NAVE
            NUMHRS(L) = 0
            NUMCLM(L) = 0
            NUMMSG(L) = 0
            DO 80 K = 1, NGRP
               DO 60 J = 1, NREC
                  AVEVAL(J,K,L,M) = 0.0
                  DO 50 I = 1, NVAL
                     HIVALU(J,I,K,L,M) = 0.0
                     NHIDAT(J,I,K,L,M) = 0
                     HCLMSG(J,I,K,L,M) = ' '
 50               CONTINUE
 60            CONTINUE
               DO 70 J = 1, NMAX
                  RMXVAL(J,K,L,M) = 0.0
                  MXDATE(J,K,L,M) = 0
                  MXLOCA(J,K,L,M) = 0
                  MCLMSG(J,K,L,M) = ' '
 70            CONTINUE
 80         CONTINUE
 90      CONTINUE
 95   CONTINUE
      IANHRS = 0
      IANCLM = 0
      IANMSG = 0
      DO 125 K = 1, NTYP
         DO 120 J = 1, NGRP
            DO 100 I = 1, NREC
               ANNVAL(I,J,K) = 0.0
 100        CONTINUE
            DO 110 I = 1, NVAL
               AMXVAL(I,J,K) = 0.0
               IMXLOC(I,J,K) = 0
 110        CONTINUE
 120     CONTINUE
 125  CONTINUE

C     Initialize the Outputs
      WRITE(TITLE1,130)
      WRITE(TITLE2,130)
 130  FORMAT(68(' '))
      IPAGE  = 0
      NHIVAL = 0
      NMXVAL = 0
      THRFRM = '(1X,I3,1X,A8,1X,I8,2(1X,F13.5),2(1X,F7.2),1X,F13.5)'
      PSTFRM = '(3(1X,F13.5),1X,F8.2,2X,A6,2X,A8,2X,I8,2X,A8)'
      PLTFRM = '(3(1X,F13.5),1X,F8.2,3X,A5,2X,A8,2X,A4,6X,A8)'
      DO 150 J = 1, NAVE
         INHI(J) = 0
         DO 140 I = 1, NVAL
            NHIAVE(I,J) = 0
 140     CONTINUE
         MAXAVE(J) = 0
         DO 145 I = 1, NGRP
            MAXFLE(I,J) = 0
            IPSTFL(I,J) = 0
            DO 144 K = 1, NVAL
               IPLTFL(K,I,J) = 0
 144        CONTINUE
 145     CONTINUE
 150  CONTINUE
      DO 160 I = 1, NGRP
         IANPST(I) = 0
         IANFRM(I) = 0
         IANPLT(I) = 0
 160  CONTINUE

C     Initialize the Number of Error/Warning/Informational Messages, and
c     The Number of Fatal Errors.
      IERROR = 0
      NFATAL = 0

      RETURN
      END

      SUBROUTINE CHKREC
C***********************************************************************
C                 CHKREC Module of ISC2 Short Term Model - ISCST2
c ----------------------------------------------------------------------
c ---    ISC-PRIME     Version 1.0    Level 970812              Modified
c ---        D. Strimaitis
c ---        Earth Tech, Inc.
c            Prepared for EPRI under contract WO3527-01
c ----------------------------------------------------------------------
C
C        PURPOSE: Print Out The Input Met Data Summary and Source Groups
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To account for new area source algorithm, which
C                    allows for receptors located within the area - 7/7/93
C
C        MODIFIED:   To account for OpenPit Source - PES - 7/22/94
C
C        INPUTS:  Source and Receptor Inputs
C
C        OUTPUTS: Listing of Receptors Too Close To Sources
C
C        CALLED FROM:   INPSUM
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      REAL XVM(5), YVM(5)

C     Variable Initializations
      MODNAM = 'CHKREC'
      INC = 0

C     Begin Source LOOP
      DO 200 ISRC = 1, NUMSRC

C        Set Effective Source Radius Based on Source Type
         IF (SRCTYP(ISRC) .EQ. 'POINT') THEN
            XRAD = 0.0
         ELSE IF (SRCTYP(ISRC) .EQ. 'VOLUME') THEN
            XRAD = 2.15 * ASYINI(ISRC)
         ELSE IF (SRCTYP(ISRC) .EQ. 'AREA') THEN
C           Skip to End of Source LOOP for AREA Sources - No Restrictions on
C           Receptor Placement for New Algorithm
            GO TO 200
         ELSE IF (SRCTYP(ISRC) .EQ. 'OPENPIT') THEN
C*           Skip to End of Source LOOP for OPENPIT Sources - No Restrictions on
C*            GO TO 200
            XRAD = -1.0
            XVM(1) = AXVERT(1,ISRC) * 1000.
            XVM(2) = AXVERT(2,ISRC) * 1000.
            XVM(3) = AXVERT(3,ISRC) * 1000.
            XVM(4) = AXVERT(4,ISRC) * 1000.
            XVM(5) = AXVERT(5,ISRC) * 1000.
            YVM(1) = AYVERT(1,ISRC) * 1000.
            YVM(2) = AYVERT(2,ISRC) * 1000.
            YVM(3) = AYVERT(3,ISRC) * 1000.
            YVM(4) = AYVERT(4,ISRC) * 1000.
            YVM(5) = AYVERT(5,ISRC) * 1000.
         END IF

C        Begin Receptor LOOP
         DO 100 IREC = 1, NUMREC

C           Calculate DIST From Edge of Source to Receptor
            X = AXR(IREC) - AXS(ISRC)
            Y = AYR(IREC) - AYS(ISRC)
            DIST = SQRT (X*X + Y*Y) - XRAD

            IF (DIST .LT. 0.99) THEN
C              Receptor Is Too Close To Source
               INC = INC + 1
               IF (MOD((INC-1), 40) .EQ. 0) THEN
                  CALL HEADER
                  WRITE(IOUNIT,9002)
               END IF
               WRITE(IOUNIT,9003) SRCID(ISRC), AXR(IREC),
     &                            AYR(IREC), DIST

c --- PRIME --------------------------------------------------------
c --- Drop check for receptors in cavity because PRIME includes
c --- module for concentrations in the cavity
c
c            ELSE IF (SRCTYP(ISRC) .EQ. 'POINT') THEN
cC              Check For Receptors Less Than 3*ZLB For POINT Sources
c               ANG = ATAN2(X,Y) * RTODEG
c               IF (ANG .LT. 0.0) ANG = ANG + 360.0
c               ISEC = INT(ANG*0.10 + 0.4999)
c               IF (ISEC .EQ. 0) ISEC = 36
c               IF (ISEC .LE. NSEC) THEN
c                  DSBH = ADSBH(ISEC,ISRC)
c                  DSBW = ADSBW(ISEC,ISRC)
c                  XMIN = 3.*AMIN1(DSBH,DSBW)
c                  IF (DIST .LT. XMIN) THEN
cC                    Receptor Is Too Close To Source
c                     INC = INC + 1
c                     IF (MOD((INC-1), 40) .EQ. 0) THEN
c                        CALL HEADER
c                        WRITE(IOUNIT,9002)
c                     END IF
c                     WRITE(IOUNIT,9003) SRCID(ISRC), AXR(IREC),
c     &                                  AYR(IREC), DIST
c                  END IF
c               END IF
c ------------------------------------------------------------------

            ELSE IF (SRCTYP(ISRC) .EQ. 'OPENPIT') THEN
C              Check for receptors within boundary of an open pit source
               XR = AXR(IREC)
               YR = AYR(IREC)
               CALL PNPOLY(XR,YR,XVM,YVM,5,INOUT)
               IF (INOUT .GT. 0) THEN
C                 Receptor is within boundary
                  INC = INC + 1
                  IF (MOD((INC-1), 40) .EQ. 0) THEN
                     CALL HEADER
                     WRITE(IOUNIT,9002)
                  END IF
                  WRITE(IOUNIT,9004) SRCID(ISRC), AXR(IREC),
     &                               AYR(IREC)
               END IF
            END IF

 100     CONTINUE
C        End Receptor LOOP

 200  CONTINUE
C     End Source LOOP

 9002 FORMAT(22X,'* SOURCE-RECEPTOR COMBINATIONS FOR WHICH ',
     & 'CALCULATIONS MAY NOT BE PERFORMED *'/27X,'LESS THAN 1.0 METER',
c --- PRIME ---------------------------------------------------------
c prm& ' OR 3*ZLB IN DISTANCE, OR WITHIN OPEN PIT SOURCE',//
     & ' OR WITHIN OPEN PIT SOURCE',//
c -------------------------------------------------------------------
     & /31X,'SOURCE',9X,'- - RECEPTOR LOCATION - -',9X,'DISTANCE',
     & /31X,'  ID  ',9X,'XR (METERS)   YR (METERS)',9X,'(METERS)',
     & /30X,30('- ')/)
 9003 FORMAT(31X,A8,5X,F13.1,1X,F13.1,7X,F10.2)
 9004 FORMAT(31X,A8,5X,F13.1,1X,F13.1,7X,'   OPENPIT')

      RETURN
      END





c --- PRIME ---------------------------------------------------------
c
c --- The remaining routines in this file are not called by ISC-PRIME
c
c -------------------------------------------------------------------

      SUBROUTINE DHPSS(XARG,DHPOUT)
C***********************************************************************
C                 DHPSS Module of ISC2 Model
C
C        PURPOSE: Calculates Distance-dependent Plume Rise for
C                 Schulman-Scire Downwash Algorithm
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To use calling argument for output
C                    R. W. Brode, PES, Inc. - 9/30/94
C
C        MODIFIED:   To Change TOL from 1.0E-5 to 1.0E-4 - 9/29/92
C
C        INPUTS:  Arrays of Source Parameters
C                 Buoyancy and Momentum Fluxes
C                 Meteorological Variables for One Hour
C                 Wind Speed Adjusted to Stack Height
C                 Downwind Distance
C                 Wake Plume Height, HEMWAK
C
C        OUTPUTS: Distance-dependent BLP Plume Rise, DHPOUT (m)
C
C        CALLED FROM:   PHEFF
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'DHPSS'

C     Determine BLP Line Source Parameters, ZLY and RINIT

      X3LB = 3.*ZLB
      IF (HEMWAK .GT. 1.2*DSBH) THEN
         ZLY = 0.0
C        Calculate Building Enhanced Sigma-z at X3LB        ---   CALL SZENH
         CALL SZENH(X3LB,SZ3LB,XZ3LB)
      ELSE
C        Calculate Building Enhanced Sigma-y at X3LB        ---   CALL SYENH
         CALL SYENH(X3LB,SY3LB,XY3LB)
C        Calculate Building Enhanced Sigma-z at X3LB        ---   CALL SZENH
         CALL SZENH(X3LB,SZ3LB,XZ3LB)
         IF (SY3LB .GE. SZ3LB) THEN
C           Note That SRT2PI = SQRT(2.*PI)
            ZLY = SRT2PI*(SY3LB-SZ3LB)
         ELSE
            ZLY = 0.0
         END IF
      END IF
C     Calculate Initial Radius of Plume, RINIT = SQRT(2.)*SZ
      RINIT = 1.414214 * SZ3LB

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

      DHPOUT = AMAX1(ZB,ZM)

      RETURN
      END

      SUBROUTINE BLPCB(XARG)
C***********************************************************************
C                 BLPCB Module of ISC2 Model
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
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'BLPCB'

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
C                 BLPCM Module of ISC2 Model
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
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'BLPCM'

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

      SUBROUTINE SYENH(XARG,SYOUT,XYOUT)
C***********************************************************************
C                 SYENH Module of ISC2 Model
C
C        PURPOSE: Calculates Building Enhanced Sigma-y Values (>= Curves)
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To use calling argument for output
C                    R. W. Brode, PES, Inc. - 9/30/94
C
C        INPUTS:  Downwind Distance
C                 Building Dimensions
C                 Stability Class
C
C        OUTPUTS: Lateral Dispersion Coefficient, Sigma-y (SYOUT)
C                 Lateral Virtual Distance (XYOUT)
C
C        CALLED FROM:  PDIS
C                      DHPSS
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'SYENH'

      IF (TALL) THEN
         IF (XARG .LT. 10.*ZLB) THEN
C           Calculate Building Enhanced Sigma-y (Eqn. 1-45)
            SY1 = 0.35*ZLB + 0.067*(XARG - 3.*ZLB)
C           Calculate Sigma-y from Dispersion Curves, SY   ---   CALL SIGY
            CALL SIGY(XARG,SYOUT)
            SYOUT = AMAX1(SY1,SYOUT)
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
            CALL XVY(syinit,XYOUT)
c !!!       CALL XVY(XYOUT)
            XYOUT = AMAX1(0.0, (XYOUT-10.*ZLB))
C           Calculate Sigma-y from Curves for X+XY          ---   CALL SIGY
            CALL SIGY(XARG+XYOUT,SYOUT)
         END IF

      ELSE IF (SQUAT) THEN
         IF (XARG .LT. 10.*ZLB) THEN
C           Calculate Buidling Enhanced Sigma-y (Eqn. A-41)
            SY1 = 0.35*DSBW + 0.067*(XARG - 3.*DSBH)
C           Calculate Sigma-y from Dispersion Curves, SY   ---   CALL SIGY
            CALL SIGY(XARG,SYOUT)
            SYOUT = AMAX1(SY1,SYOUT)
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
            CALL XVY(syinit,XYOUT)
c !!!       CALL XVY(XYOUT)
            XYOUT = AMAX1(0.0, (XYOUT-10.*ZLB))
C           Calculate Sigma-y from Curves for X+XY          ---   CALL SIGY
            CALL SIGY(XARG+XYOUT,SYOUT)
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
            CALL SIGY(XARG,SYOUT)
            SYOUT = AMAX1(SY1,SYOUT)
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
            CALL XVY(syinit,XYOUT)
c !!!       CALL XVY(XYOUT)
            XYOUT = AMAX1(0.0, (XYOUT-10.*ZLB))
C           Calculate Sigma-y from Curves for X+XY          ---   CALL SIGY
            CALL SIGY(XARG+XYOUT,SYOUT)
         END IF
      END IF

      RETURN
      END

      SUBROUTINE SZENH(XARG,SZOUT,XZOUT)
C***********************************************************************
C                 SZENH Module of ISC2 Model
C
C        PURPOSE: Calculates Building Enhanced Sigma-z Values
C                 and Compares to Sigma-z From Dispersion Curves
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To use calling argument for output
C                    R. W. Brode, PES, Inc. - 9/30/94
C
C        INPUTS:  Downwind Distance
C                 Stability Class
C                 Rural or Urban Dispersion Option
C                 Wake Plume Height, HEMWAK
C
C        OUTPUTS: Vertical Dispersion Coefficient, Sigma-z (SZOUT)
C                 Vertical Virtual Distance (XZOUT)
C
C        CALLED FROM:   PDIS
C                       DHPSS
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'SZENH'

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
      CALL XVZ(szinit,XARG-10.*ZLB,XZOUT)
c !!! CALL XVZ(XARG-10.*ZLB,XZOUT)
      XZOUT = AMAX1(0.0, (XZOUT - 10.*ZLB))

      IF (XARG .LT. 10.*ZLB) THEN
C        Calculate Building Enhanced Sigma-z (Eqn. 1-37 & 1-38)
         SZ1 = (0.7*ZLB + 0.067*(XARG - 3.*ZLB)) * DA
C        Calculate Sigma-z from Curves, SZ2                 ---   CALL SIGZ
         CALL  SIGZ(XARG,SZOUT)
         SZOUT = AMAX1(SZ1,SZOUT)
      ELSE
C        Calculate Sigma-z from Curves using X+XZ           ---   CALL SIGZ
         CALL SIGZ(XARG+XZOUT,SZOUT)
      END IF

      RETURN
      END

