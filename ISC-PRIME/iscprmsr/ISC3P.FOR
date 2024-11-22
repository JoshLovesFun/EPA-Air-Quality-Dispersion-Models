      PROGRAM ISC3P
C***********************************************************************
c ---            ISC3 with PRIME Building Downwash  -  ISC3P
c ---                       (Version 04269)
C***********************************************************************
c
c ---   Modified on: August 26, 2004
c
c ---   Modified by: Peter Eckhoff 
c                    U.S. Environmental Protection Agency
c                    4930 Old Page Road
c                    D243-01
c                    Research Triangle Park, NC 27709
c
c       Modifications: Lahey specific I/O routines were commented out
c                      Default input and output units were changed 
c                        from 5 & 6 to 7 & 8, respectively.
c                      Recompiled with Compaq Visual Fortran V6.6c
c
c********************************************************************
c
c ---   Adapted from: 
c                    ISC3 Short Term Model - ISCST3
c                         (Version Dated 96113)
c
c ---   Prepared for EPRI under contract WO3527-01
c
c ---   by:
c                    David G. Strimaitis
c                    Joseph S. Scire
c                    Lloyd L. Schulman
c
c                    Earth Tech, Inc.
c                    196 Baker Avenue
c                    Concord, MA 01742
c
c                    August 12, 1997
c 
c ---   Technical references:
c
c       Schulman, L.L., D.G. Strimaitis, and J.S. Scire, 1998.
C         Development and Evaluation of the PRIME Plume Rise and 
c         Building Downwash Model. Submitted to Atmospheric   
c         Environment, 32.
C
C       Schulman, L.L., D.G. Strimaitis, and J.S. Scire, 1998.
C         Development and Evaluation of the PRIME Plume Rise and 
c         Building Downwash Model. Paper 4B.1 in Proceedings of 
c         Tenth Joint Conference on the Applications of Air Pollution
c         Meteorology, Phoenix, AZ, Amer. Meteor. Soc., Boston, MA  
c 
c       Schulman, L.L. and J.S. Scire, 1996. The Development of the 
c         Plume Rise Model Enhancements (PRIME): The EPRI Plume Rise
c         and Downwash Modeling Project. Paper 6.1 in Proceedings of the
c         Ninth Joint Conference on the Applications of Air Pollution
c         Meteorology, Atlanta, GA, Amer. Meteor. Soc., Boston, MA  
c           
c       Schulman, L.L. and J.S. Scire, 1996. Development of the EPRI
c         Plume Rise Model Enhancements (PRIME), Paper 96-TA24A.01 in   
c         89th AWMA Annual Meeting, Nashville, TN
c
c       Scire, J.S., L.L. Schulman and D.G. Strimaitis, 1995.
C         Observations of Plume Descent Downwind of Buildings, Paper 
c         95-WP75B.01 in 88th AWMA Annual Meeting, San Antonio, TX
c
c  
c ---   The development of PRIME and its implementation in ISCST
c ---   were funded by a consortium of members of:
c                    EPRI
c                    3412 Hillview Avenue
c                    P.O. Box 10412
c                    Palo Alto, CA 94303-0813
c
C***********************************************************************
C
C               *** SEE ISCST3 MODEL CHANGE BULLETIN MCB#2 ***
C
C       ON THE SUPPORT CENTER FOR REGULATORY AIR MODELS BULLETIN BOARD
C
C                               919-541-5742
C
C       This version includes a correction to SUB. DEPCOR in DEPFLUX.FOR,
C       modifications to SETUP.FOR and OUTPUT.FOR for compatibility with
C       the ISCEV3 (EVENT) model, and a modification to SUB. HRLOOP for a
C       potential problem with the use of the STARTEND keyword with non-
C       sequential meteorological data sets.
C
C       MODIFIED BY:    Roger W. Brode
C                       PES, Inc.
C                       April 22, 1996
C
C
C       MODIFIED FROM:    (Version Dated 95250)
C
C       This version of ISCST3 contains a revised AREA SOURCE algorithm
C       and revised DEPOSITION algorithm.  These Supplement C revisions had
C       been implemented independently, but are now merged in this version.
C       This version also includes a wet deposition algorithm, an open
C       pit source algorithm, and incorporates the COMPLEX1 algorithms for
C       receptor elevations above the release height.
C
C        PROGRAMMED BY: Roger W. Brode, Jayant A. Hardikar,
C                       and James O. Paumier
C                       Pacific Environmental Services, Inc.
C                       5001 S. Miami Blvd., Suite 300
C                       P.O. Box 12077
C                       Research Triangle Park, North Carolina  27709
C
C                       Deposition portions by:
C
C                       Yicheng Zhuang and David G. Strimaitis
C                       Sigma Research Corperation
C                       196 Baker Ave
C                       Concord, MA 01742
C
C        DATE:    September 7, 1995
C
C***********************************************************************
C
C        Version History of ISCST3
C
C          Date     Version    Alias      Developed From
C        --------   -------   -------    ---------------------
C        09-07-95   95250     ISCST3     ISCSTDFT (dated 94340)
C        12-06-94   94340     ISCSTDFT   DEPST    (dated 94067)
C                                        AREA-ST  (dated 93188), and
C                                        ISCST2   (dated 93109)
C        03-08-94   94067     DEPST      DEPST    (dated 94006)
C        01-06-94   94006     DEPST      ISCST2D  (dated 93046)
C        02-15-93   93046     ISCST2D    ISCST2   (dated 92273)
C        09-29-92   92273     ISCST2     ISCST2   (dated 92062)
C        03-02-92   92062     ISCST2
C
C        ISCSTDFT was formerly known as ISC-COMPDEP.
C
C***********************************************************************
C
C                 MAIN Module of the ISC3 Short Term Model - ISCST3
C
C        PURPOSE: Controls Overall Flow and Processing of ISCST3 Model
C
C        PROGRAMMED BY: Roger W. Brode
C                       Pacific Environmental Services, Inc.
C                       5001 S. Miami Blvd., Suite 300
C                       P.O. Box 12077
C                       Research Triangle Park, North Carolina  27709
C
C        DATE: September 7, 1995
C
C        INPUTS:  Command Line Options
C
C        OUTPUTS: Model Results
C
C        The ISC3 models have been developed for the U.S. Environmental
C        Protection Agency under Contract No. 68D30032.  The PES Project
C        Manager is Roger W. Brode.  For instructions on running the model,
C        refer to the User's Guide for the Industrial Source Complex (ISC3)
C        Dispersion Models, Volume I - User Instructions (EPA-454/B-95-003a).
C
C***********************************************************************
C
C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations                              ---   CALL VARINI
      CALL VARINI
      MODNAM = 'MAIN'

C     Open the Temporary File for Error Messages Generated from the Program
      OPEN(UNIT=IERUNT,FILE='ERRMSG.TMP',STATUS='UNKNOWN')

C     Close and Delete The Error Message Temporary Files To Avoid Re-reading
C     Messages if File Already Exists and READ Precedes WRITE.
      CLOSE(IERUNT,STATUS='DELETE')

C     Re-Open the Temporary File for Error Messages Generated from the Program
      OPEN(UNIT=IERUNT,FILE='ERRMSG.TMP',STATUS='UNKNOWN')

C     OPEN The Temporary File to Store Events for EVENT File;
C     Also Used to Store High Values for Summary Tables
      OPEN(UNIT=ITEVUT,FILE='EVENT.TMP',STATUS='UNKNOWN')
C     Initialize the Event Counter
      IEVENT = 0

C     Retrieve Input and Output File Names From Command Line,
C     ---   CALL GETCOM
c      CALL GETCOM (' ISC3P ',INPFIL,OUTFIL)

C     Open Input and Output Files                           ---   CALL FILOPN
      CALL FILOPN

C     Process The Model Setup Information                   ---   CALL SETUP
      CALL SETUP

C     Determine Number of Setup Messages by Message Type    ---   CALL TERRST
      CALL TERRST

c --- Set up common for PRIME numerical rise algorithm      ---   CALL NUMPR1
      CALL NUMPR1

c --- Set up common for PRIME building cavity model         ---   CALL PRIME1
      CALL PRIME1

      IF (.NOT.RUN .OR. FATAL .OR. IWRN .GT. 0) THEN
C        Write Out Summary Of Setup Error/Message Stats     ---   CALL SUMTBL
         WRITE(IOUNIT,9111)
 9111    FORMAT(//2X,'*** Message Summary For ISC3P Model Setup ***'/)
         CALL SUMTBL
      END IF

      IF (FATAL) THEN
         WRITE(IOUNIT,9112)
 9112    FORMAT(/4X,'**************************************',
     &          /4X,'*** SETUP Finishes UN-successfully ***',
     &          /4X,'**************************************'/)
      ELSE
         WRITE(IOUNIT,9113)
 9113    FORMAT(/1X,'***********************************',
     &          /1X,'*** SETUP Finishes Successfully ***',
     &          /1X,'***********************************'/)
      END IF

C     Print Summary of the Input Data                       ---   CALL INPSUM
      CALL INPSUM

      IF (.NOT.FATAL .AND. RUN) THEN
C        No Fatal Errors in Setup and RUN Option Selected

C        Reinitialize Results Arrays With Zeroes            ---   CALL RESINI
         CALL RESINI

         IF (RSTINP) THEN
C           Initialize Results Arrays from Re-start File    ---   CALL RSINIT
            CALL RSINIT
         END IF

C        Process The Hourly Meteorological Data             ---   CALL HRLOOP
         CALL HRLOOP

         IF ((PERIOD.OR.ANNUAL) .AND. (.NOT. RUNERR)) THEN
C           PERIOD Average Selected and No Runtime/Meteorology Errors
            IF (CONC) THEN
C              Calculate Period Average Concentrations      ---   CALL PERAVE
               CALL PERAVE
            END IF
            IF ((DEPOS.OR.DDEP.OR.WDEP) .AND. ANNUAL) THEN
C              Calculate Annual Average Deposition Rates    ---   CALL ANNAVE
               CALL ANNAVE
            END IF
            DO ITYP = 1, NUMTYP
C              Select Highest PERIOD Values by Source Group ---   CALL HIPER
               CALL HIPER
            END DO
            IF (ANPOST) THEN
C              Write PERIOD Results to Post File            ---   CALL PSTANN
               CALL PSTANN
            END IF
            IF (ANPLOT) THEN
C              Write PERIOD Results to Plot File            ---   CALL PLTANN
               CALL PLTANN
            END IF
         END IF

         IF (PLFILE .AND. (.NOT. RUNERR)) THEN
C           Write Short Term High Values to Plot File       ---   CALL PLOTFL
            CALL PLOTFL
         END IF

         IF (.NOT. RUNERR) THEN
C           Print Out Model Results                         ---   CALL OUTPUT
            CALL OUTPUT
         END IF

      END IF

      CALL HEADER
      WRITE(IOUNIT,9114)
 9114 FORMAT(/1X,'*** Message Summary : ISC3P Model Execution ***'/)
C     Determine Number of Errors/Messages by Message Type   ---   CALL TERRST
      CALL TERRST
C     Write Summary of Message Stats for Model Execution    ---   CALL SUMTBL
      CALL SUMTBL

      IF (FATAL .OR. RUNERR) THEN
         WRITE(IOUNIT,9115)
 9115    FORMAT(/4X,'***************************************',
     &          /4X,'*** ISC3P  Finishes UN-successfully ***',
     &          /4X,'***************************************'/)
      ELSE
         WRITE(IOUNIT,9116)
 9116    FORMAT(/4X,'************************************',
     &          /4X,'*** ISC3P  Finishes Successfully ***',
     &          /4X,'************************************'/)
      END IF

      IF (ERRLST) THEN
C        OPEN and Write Out Permanent Error Message File    ---   CALL MSGWRT
         OPEN(UNIT=IERWRT,FILE=MSGFIL,STATUS='UNKNOWN',
     &        FORM='FORMATTED')
         CALL MSGWRT
         CLOSE(IERWRT)
      END IF

C     Close and Delete The Error Message And EVENT Temporary Files
      CLOSE(IERUNT,STATUS='DELETE')
      CLOSE(ITEVUT,STATUS='DELETE')

      STOP
      END

      SUBROUTINE HRLOOP
C***********************************************************************
C                 HRLOOP Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Controls Main Calculation Loop Through
C                 Hourly Meteorological Data
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  Minor change to logic of IF block to correct
C                   potential problem with STARTEND keyword for
C                   non-sequential meteorological data sets.
C                   R.W. Brode, PES, Inc. - 4/22/96
C
C        MODIFIED:  To Include TOXXFILE Option - 9/29/92
C
C        INPUTS:  Source, Receptor and Setup Options
C
C        OUTPUTS: Update Hourly Results
C
C        CALLED FROM:   MAIN
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'HRLOOP'
      EOF = .FALSE.

C     Begin Hourly LOOP
      DO WHILE (KURDAT.LT.IEDATE .AND. .NOT.EOF)
C        Retrieve One Hour of Meteorology                   ---   CALL METEXT
         CALL METEXT
         
C*----   ISCSTM Modification: allow for hourly emissions - jah 11/3/94                           
C*        Process Hourly Emissions from File
C*        Begin Source Loop
          DO 5 ISRC = 1, NUMSRC
            IF (QFLAG(ISRC) .EQ. 'HOURLY') THEN
C*             Retrieve Source Parameters for This Hour     ---   CALL HRQEXT
               CALL HRQEXT(ISRC)
            ENDIF
    5    CONTINUE
C*       End Source Loop
C*----
C*----   ISCSTM Modification: allow for NOCHKD option - jah 11/2/94                           
C*       Check for IHOUR = 1 and Write Update to the Screen For PC Version
         IF (IHOUR .EQ. 1 .AND. .NOT.NOCHKD) THEN
C*          Write Out Update to the Screen by Julian Day
            WRITE(*,909) JDAY
 909        FORMAT('+','Now Processing Data For Day No. ',I4)
         ELSE IF (NOCHKD) THEN
C*          Write Out Update to the Screen by Hour
            WRITE(*,910) KURDAT
 910        FORMAT('+','Now Processing Data For     ',I8)
         END IF
C*----
C*#

         IF (KURDAT.GT.ISDATE .AND. KURDAT.LE.IEDATE .AND.
     &       IPROC(JDAY).EQ.1 .AND.
     &               .NOT.EOF .AND. .NOT.RUNERR) THEN

            IF (CLMHR .AND. CLMPRO) THEN
C              Check for Calm Hr & Processing and Increment Counters
               DO 10 IAVE = 1, NUMAVE
                  NUMHRS(IAVE) = NUMHRS(IAVE) + 1
                  NUMCLM(IAVE) = NUMCLM(IAVE) + 1
 10            CONTINUE
               IF (PERIOD .OR. ANNUAL) THEN
                  IANHRS = IANHRS + 1
                  IANCLM = IANCLM + 1
               END IF
            ELSE IF (MSGHR .AND. MSGPRO) THEN
C              Check for Missing Hour & Processing and Increment Counters
               DO 20 IAVE = 1, NUMAVE
                  NUMHRS(IAVE) = NUMHRS(IAVE) + 1
                  NUMMSG(IAVE) = NUMMSG(IAVE) + 1
 20            CONTINUE
               IF (PERIOD .OR. ANNUAL) THEN
                  IANHRS = IANHRS + 1
                  IANMSG = IANMSG + 1
               END IF
            ELSE IF (ZI .LE. 0) THEN
C              Write Out The Informational Message & Increment Counters
               WRITE(DUMMY,'(I8)') KURDAT
               CALL ERRHDL(PATH,MODNAM,'I','470',DUMMY)
               DO 30 IAVE = 1, NUMAVE
                  NUMHRS(IAVE) = NUMHRS(IAVE) + 1
 30            CONTINUE
               IF (PERIOD .OR. ANNUAL) THEN
                  IANHRS = IANHRS + 1
               END IF
            ELSE
C              Set CALCS Flag, Increment Counters & Calculate HRVAL
               CALCS = .TRUE.
               DO 40 IAVE = 1, NUMAVE
                  NUMHRS(IAVE) = NUMHRS(IAVE) + 1
 40            CONTINUE
               IF (PERIOD .OR. ANNUAL) THEN
                  IANHRS = IANHRS + 1
               END IF

C              Time/Date Marker for DEBUG Output
               IF (DEBUG) THEN
                  WRITE(IOUNIT,*)
                  WRITE(IOUNIT,*) '--------------------------------',
     &                            '--------------------'
                  WRITE(IOUNIT,*) '---  JDAY, IHOUR =  ',JDAY,IHOUR
                  WRITE(IOUNIT,*) '--------------------------------',
     &                            '--------------------'
               END IF

C              Calculate CONC or DEPOS Values               ---   CALL CALC
               CALL CALC
            END IF

C           Beging Averaging Period LOOP
            DO 100 IAVE = 1, NUMAVE
C              Check for End of Averaging Period
               IF (MOD(IHOUR,KAVE(IAVE)).EQ.0 .OR.
     &            (KAVE(IAVE).EQ.720 .AND. ENDMON)) THEN
                  IF (CONC) THEN
C                    Calculate Applicable Averages          ---   CALL AVER
                     CALL AVER
                  END IF
C                 Update High Value Arrays                  ---   CALL HIVALS
                  CALL HIVALS
                  IF (DAYTAB .AND. IDYTAB(IAVE).EQ.1) THEN
                     DO ITYP = 1, NUMTYP
C                       Print Out Daily Value Tables        ---   CALL PRTDAY
                        CALL PRTDAY
                     END DO
                  END IF
                  IF (MXFILE) THEN
C                    Write Max Values (>Thresh) to File     ---   CALL MAXFIL
                     CALL MAXFIL
                  END IF
                  IF (PPFILE) THEN
C                    Write Values to Postprocessor File     ---   CALL POSTFL
                     CALL POSTFL
                  END IF
                  IF (TXFILE) THEN
C                    Write Values to TOXXFILE File (9/29/92) ---  CALL TOXXFL
                     CALL TOXXFL
                  END IF
C                 Flush Block Average Values in AVEVAL Array for This IAVE
                  DO 70 ITYP = 1, NUMTYP
                     DO 60 IGRP = 1, NUMGRP
                        DO 50 IREC = 1, NUMREC
                           AVEVAL(IREC,IGRP,IAVE,ITYP) = 0.0
 50                     CONTINUE
 60                  CONTINUE
 70               CONTINUE
               END IF
 100        CONTINUE
C           End Averaging Period LOOP

            IF (RSTSAV .AND. IHOUR.EQ.24) THEN
               NDAYS = NDAYS + 1
               IF (NDAYS .EQ. INCRST) THEN
C                 Save Results to File for Later Re-start   ---   CALL RSDUMP
                  CALL RSDUMP
                  NDAYS = 0
               END IF
            END IF

C           Flush HRVAL Variable
            DO ITYP = 1, NUMTYP
               HRVAL(ITYP) = 0.0
            END DO
         END IF
C        Reset CALCS and ENDMON Flags
         CALCS  = .FALSE.
         ENDMON = .FALSE.

      END DO
C     End Hourly LOOP

C     Check for TOXXFILE Option, Fill Buffer and Dump to File - 9/29/92
      IF (TXFILE) THEN
         IDUM = 0
         RDUM = 0.0
         DO 300 IAVE = 1, NUMAVE
            IF (ITOXFL(IAVE) .EQ. 1) THEN
C              Fill Rest of Buffer With Zeroes and Write to TOXXFILE
               DO 200 I = IPAIR+1, NPAIR
                 IDCONC(IAVE,I) = IDUM
                 TXCONC(IAVE,I) = RDUM
 200           CONTINUE
               WRITE(ITXUNT(IAVE)) (IDCONC(IAVE,I),I=1,NPAIR)
               WRITE(ITXUNT(IAVE)) (TXCONC(IAVE,I),I=1,NPAIR)
               CLOSE(ITXUNT(IAVE))
            END IF
 300     CONTINUE
      END IF

C     Write Out Update to the Screen for PC Version
      WRITE(*,919)
 919  FORMAT('+','Now Processing Output Options       ')

      RETURN
      END

      SUBROUTINE JULIAN(IYR,IMN,IDY,JDY)
C***********************************************************************
C                 JULIAN Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE:    CONVERT YR/MN/DY DATE TO JULIAN DAY (1-366),
C                    INCLUDES TEST FOR 100 AND 400 YEAR CORRECTIONS TO
C                    HANDLE 4 DIGIT YEARS BEYOND 2099 AND BEFORE 1901
C                    (WILL WORK WITH 2 DIGIT YR FOR PERIOD 1901-2099)
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:     YEAR,  IYR (2 OR 4 DIGIT)
C                    MONTH, IMN
C                    DAY,   IDY
C
C        OUTPUT:     JULIAN DAY,  JDY (1-366)
C
C        CALLED FROM:   DAYRNG
C
C        ERROR HANDLING:   Checks for Invalid Month or Day
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      INTEGER NDAY(12), IDYMAX(12)

C     Variable Initializations
      DATA NDAY/0,31,59,90,120,151,181,212,243,273,304,334/
      DATA IDYMAX/31,29,31,30,31,30,31,31,30,31,30,31/
      MODNAM = 'JULIAN'
      JDY = 0

C     Check for 2-digit Year Input and WRITE Warning Message
      IF (IYR .LT. 100) THEN
C        WRITE Warning Message  ! Routine Will Work for Years 1901-2099
         CALL ERRHDL(PATH,MODNAM,'W','360',KEYWRD)
      END IF

C     Check for Invalid Month or Day
      IF (IMN.LT.1 .OR. IMN.GT.12) THEN
C        WRITE Error Message    ! Invalid Month
         CALL ERRHDL(PATH,MODNAM,'E','203','MONTH')
         GO TO 999
      ELSE IF (IDY .GT. IDYMAX(IMN)) THEN
C        WRITE Error Message    ! Invalid Day
         CALL ERRHDL(PATH,MODNAM,'E','203','DAY')
         GO TO 999
      END IF

C     Determine JULIAN Day Number; For Non-Leap Year First
      IF ((MOD(IYR,4) .NE. 0) .OR.
     &    (MOD(IYR,100) .EQ. 0 .AND. MOD(IYR,400) .NE. 0)) THEN
C        Not a Leap Year
         IF (IMN.NE.2 .OR. (IMN.EQ.2 .AND. IDY.LE.28)) THEN
            JDY = IDY + NDAY(IMN)
         ELSE
C           WRITE Error Message    ! Invalid Date; 2/29 in a Non-Leap Year
            CALL ERRHDL(PATH,MODNAM,'E','370',' ')
         END IF
      ELSE
C        Leap Year
         JDY = IDY + NDAY(IMN)
         IF (IMN .GT. 2)  JDY = JDY + 1
      END IF

 999  CONTINUE

      RETURN
      END

      SUBROUTINE GREGOR(IYR,IMN,JDY,IDY)
C***********************************************************************
C                 GREGOR Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE:    CONVERT JULIAN DAY (1-366) TO DAY OF MONTH,
C                    INCLUDES TEST FOR 100 AND 400 YEAR CORRECTIONS TO
C                    HANDLE 4 DIGIT YEARS BEYOND 2099 AND BEFORE 1901
C                    (WILL WORK WITH 2 DIGIT YR FOR PERIOD 1901-2099)
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:     YEAR,       IYR (2 OR 4 DIGIT)
C                    MONTH,      IMN
C                    JULIAN DAY, JDY (1-366)
C
C        OUTPUT:     DAY OF MONTH, IDY
C
C        CALLED FROM:   METEXT
C
C        ERROR HANDLING:   Checks for Invalid Month or Day
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      INTEGER NDAY(12)

C     Variable Initializations
      DATA NDAY/0,31,59,90,120,151,181,212,243,273,304,334/
      MODNAM = 'GREGOR'

C     Check for Invalid Month or Julian Day
      IF (IMN.LT.1 .OR. IMN.GT.12) THEN
C        WRITE Error Message    ! Invalid Month
         CALL ERRHDL(PATH,MODNAM,'E','203','MONTH')
         GO TO 999
      ELSE IF (JDY.LT.1 .OR. JDY.GT.366) THEN
C        WRITE Error Message    ! Invalid Julian Day
         CALL ERRHDL(PATH,MODNAM,'E','203','Juli Day')
         GO TO 999
      END IF

C     Determine Day-of-Month Number; For Non-Leap Year First
      IF ((MOD(IYR,4) .NE. 0) .OR.
     &    (MOD(IYR,100).EQ.0 .AND. MOD(IYR,400).NE.0)) THEN
C        Not a Leap Year
         IDY = JDY - NDAY(IMN)
      ELSE
C        Leap Year
         IDY = JDY - NDAY(IMN)
         IF (IMN .GT. 2)  IDY = IDY - 1
      END IF

 999  CONTINUE

      RETURN
      END
      
      SUBROUTINE HRQEXT (IS)
C***********************************************************************
C*                  HRQEXT Module of AERMOD
C* 
C*         PURPOSE: To Assign Hourly Source Parameters
C* 
C*         PROGRAMMER:  Jayant Hardikar, Roger Brode
C* 
C*         DATE:    September 15, 1993
C* 
C*         INPUTS:  Variable QFLAG and Current Source Number Being Processed
C* 
C*         OUTPUTS: Source Arrays
C*          
C*         MODIFIED:  REMOVED THE 'POINT' SOURCE CONDITION, SO IT APPLIES 
C*                    TO ALL SOURCE TYPES, EXCEPT SAVING THE TEMP & VEL
C* 
C*         CALLED FROM:  HRLOOP
C************************************************************************
C*
C*    Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER RDFRM*20

      CHARACTER*8 HRSOID

C*    Variable Initializations
      MODNAM = 'HRQEXT'
C*
C*    READ Record to Buffers, A80 and 80A1
C*    Length of ISTRG is Set in PARAMETER Statement in MAIN1.INC
C     Setup READ format and ECHO format for runstream record,
C     based on the ISTRG PARAMETER (set in MAIN1.INC)
      WRITE(RDFRM,9100) ISTRG, ISTRG
 9100 FORMAT('(A',I3.3,',T1,',I3.3,'A1)')
      READ (IHREMI,RDFRM,ERR=99,END=999) RUNST1, (RUNST(I), I=1, ISTRG)
C*
C*    Convert Lower Case to Upper Case Letters              ---   CALL LWRUPR
      CALL LWRUPR
C*      
C*    Define Fields on Card                                 ---   CALL DEFINE
      CALL DEFINE
C*
C*    Get the Contents of the Fields                        ---   CALL GETFLD
      CALL GETFLD
C*
C*    Check for number of fields - error if less than 7.
      IF (IFC .LT. 7) THEN
         CALL ERRHDL(PATH,MODNAM,'E','201','HOUREMIS')
         GO TO 999
      END IF
C*         
C*    Assign the Feilds to Local Varables and Check The Numerical Field
C*
      CALL STONUM(FIELD(3), 40, HYEAR, IMIT)
      IHYEAR = HYEAR
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      END IF

      CALL STONUM(FIELD(4), 40, HMON, IMIT)
      IHMON = HMON
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      END IF

      CALL STONUM(FIELD(5), 40, HDAY, IMIT)
      IHDAY = HDAY
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      END IF

      CALL STONUM(FIELD(6), 40, HHOUR, IMIT)
      IHHOUR = HHOUR
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      END IF

      HRSOID = FIELD(7)

      IF (IFC .GE. 8) THEN
         CALL STONUM(FIELD(8), 40, HRQS, IMIT)
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         END IF
      ELSE
C*       Emission rate is missing - set to zero
         HRQS = 0.0
      END IF

      IF (IFC.EQ.10) THEN
C*       Also Assign Exit Temperature and Exit Velocity
         CALL STONUM(FIELD(9), 40, HRTS, IMIT)
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         END IF

         CALL STONUM(FIELD(10), 40, HRVS, IMIT)
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         END IF
      ELSE
C*       Some missing parameters - assign zeros to all
         HRTS = 0.0
         HRVS = 0.0
      ENDIF

C*    Check for Date and Time Consistency ; If Failed - Abort Program
      KURHRQ = IHYEAR*1000000 + IHMON*10000 + IHDAY*100 + IHHOUR
      IF (KURDAT .NE. KURHRQ ) THEN
C*       WRITE Error Message - Date mismatch
         WRITE(DUMMY,'(I8)') KURDAT
         CALL ERRHDL(PATH,MODNAM,'E','455',DUMMY)
         RUNERR = .TRUE.
      END IF


C*    Check for Source ID Consistency ; If Failed - Abort Program
      IF ( HRSOID .NE. SRCID(IS) ) THEN
         WRITE(DUMMY,'(A8)') SRCID(IS)
         CALL ERRHDL(PATH,MODNAM,'E','342',SRCID(IS))
         RUNERR = .TRUE.
      ENDIF

C*    Assign the Hourly Emission Parameters to the Stack Variables
      AQS(IS) = HRQS

      IF (SRCTYP(IS) .EQ. 'POINT') THEN
         ATS(IS) = HRTS
         AVS(IS) = HRVS
      ENDIF


C*    Perform QA Error Checking on Source Parameters
C*

      IF (ATS(IS) .EQ. 0.0) THEN
C*       Set Temperature to Small Negative Value for Ambient Releases
         ATS(IS) = -1.0E-5
      ELSE IF (ATS(IS) .GT. 2000.0) THEN
C*       WRITE Informational Message:  Exit Temp. > 2000K
         CALL ERRHDL(PATH,MODNAM,'I','320','HRTS')
      END IF

      IF (SRCTYP(IS) .EQ. 'POINT') THEN
         IF (AVS(IS) .LT. 0.0) THEN
C*          WRITE Informational Message:  Negative or Zero Exit Velocity
            CALL ERRHDL(PATH,MODNAM,'I','325','HRVS')
C*          Set to Small Value to Avoid Zero-divide and Underflow
            AVS(IS) = 1.0E-5
         ELSE IF (AVS(IS) .LT. 1.0E-5) THEN
C*          Set to Small Value to Avoid Zero-divide and Underflow
            AVS(IS) = 1.0E-5
         ELSE IF (AVS(IS) .GT. 50.0) THEN
C*          WRITE Informational Message:  Exit Velocity > 50.0 m/s
            CALL ERRHDL(PATH,MODNAM,'I','320','HRVS')
         END IF
      ENDIF

      GO TO 999

C*    Write Error Message for Error Reading Hourly Emissions File
 99   CALL ERRHDL(PATH,MODNAM,'E','510','HOUREMIS')
      RUNERR = .TRUE.

999   RETURN
      END


      SUBROUTINE ERRHDL(PATHWY,MODNAM,INERTP,INERCD,INPMSG)
C***********************************************************************
C                 ERRHDL Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: A General Error Handling Procedure
C
C        PROGRAMMER: Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Error Code, Occur Locations
C
C        OUTPUTS: Error Message, Error Statistics..etc.
C
C        CALLED FROM:  (This Is An Utility Programm)
C***********************************************************************
C
C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER ERRMG1*50, PATHWY*2, INERTP*1, INERCD*3, ICODE*3,
     &          INPMSG*(*)
      LOGICAL FIND

C     Variable Initializations
      IERROR = IERROR + 1
      FIND = .FALSE.
      I = 1

C     Check for Occurrence of 'E' Error Type, and Set FATAL Switch
      IF (INERTP .EQ. 'E') THEN
         FATAL = .TRUE.
         NFATAL = NFATAL + 1
         IF (NFATAL .EQ. 999) THEN
C           Number Of Fatal Errors Has Reached Limit of 999
            ERRMG1 = 'Number of Fatal Errors Has Reached Limit of 999'
            MODNAM = 'ERRHDL'
            ICODE = '999'
            INPMSG = ' '
            WRITE(IERUNT,1111) PATHWY,INERTP,ICODE,ILINE,MODNAM,ERRMG1,
     &                         INPMSG
            GO TO 999
         ELSE IF (NFATAL .GT. 999) THEN
C           Skip Any More Error WRITEs
            GO TO 999
         END IF
      END IF

C     Go To Match The Error Massage
      DO WHILE (.NOT.FIND .AND. I.LE.IERRN)
         IF (INERCD .EQ. ERRCOD(I)) THEN
            IERRDX = I
            ERRMG1 = ERRMSG(I)
            FIND = .TRUE.
         END IF
         I = I + 1
      END DO

      IF (.NOT. FIND) THEN
         WRITE(ERRMG1,1001)
 1001    FORMAT('SYSTEM ERROR: MESSAGE NOT FOUND FOR THIS NUMBER!')
      END IF

C     Write Out The Error Message
      WRITE(IERUNT,1111) PATHWY,INERTP,INERCD,ILINE,MODNAM,ERRMG1,INPMSG
 1111 FORMAT(A2,1X,A1,A3,I6,1X,A6,':',A50,1X,A8)

 999  RETURN
      END

      SUBROUTINE TERRST
C***********************************************************************
C                 TERRST Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: To Determine Total Error/Message Statistics
C
C        PROGRAMMER:  Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Error Message Temporary File
C
C        OUTPUTS: Total Number of Messages by Message Type
C
C        CALLED FROM:  This is A Utility Program
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER ERRTP*1, ERRCD*3, ERRMG1*50, ERRMG2*8, INPFLD*3

C     Variable Initialization
      MODNAM = 'TERRST'
      IFTL = 0
      IWRN = 0
      INFO = 0
      ICLM = 0
      IMSG = 0
      EOF = .FALSE.

C     Rewind the Temporary Error/Message File
      REWIND IERUNT

      DO WHILE (.NOT. EOF)
         READ(IERUNT,1116,END=99,ERR=9999) PATH,ERRTP,ERRCD,IERRLN,
     &                                     MODNAM,ERRMG1,ERRMG2

C        Sort Error Group And Find The Index
         INPFLD = ERRCD
         CALL STONUM(INPFLD,3,FNUM,IMIT)

         IF (ERRTP .EQ. 'E') THEN
            IFTL = IFTL + 1
         ELSE IF (ERRTP .EQ. 'W') THEN
            IWRN = IWRN + 1
         ELSE IF (ERRTP .EQ. 'I') THEN
            INFO = INFO + 1
            IF (INT(FNUM) .EQ. 440) THEN
C              Message for Calm Hour, Increment Calm Counter
               ICLM = ICLM + 1
            END IF
            IF (INT(FNUM) .EQ. 460) THEN
C              Message for Calm Hour, Increment Calm Counter
               IMSG = IMSG + 1
            END IF
         END IF

         GO TO 11
 99      EOF = .TRUE.
 11      CONTINUE
      END DO

 1116 FORMAT(A2,1X,A1,A3,I6,1X,A6,1X,A50,1X,A8)

C     Use BACKSPACE To Reposition Temporary Error Message File Ahead of EOF;
C     This Is Needed in Order To Allow For Additional Message Writes
      BACKSPACE IERUNT

      GO TO 1000

C     WRITE Error Message: Error Reading Temp Error Message File
 9999 CALL ERRHDL(PATH,MODNAM,'E','510','ERRORMSG')

 1000 RETURN
      END

      SUBROUTINE SUMTBL
C***********************************************************************
C                 SUMTBL Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: To Print Out The Error Summary Table
C
C        PROGRAMMER:  Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Error Message Temporary File
C
C        OUTPUTS: Summary Of Errors
C
C        CALLED FROM:  This is A Utility Program
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER ERRTP*1, ERRCD*3, ERRMG1*50, ERRMG2*8

C     Variable Initialization
      MODNAM = 'SUMTBL'

C     Write Out The Total Error Statistics
      WRITE(IOUNIT,*) ' --------- Summary of Total Messages --------'
      WRITE(IOUNIT,*) ' '
      WRITE(IOUNIT,*) 'A Total of ',IFTL,' Fatal Error Message(s)'
      WRITE(IOUNIT,*) 'A Total of ',IWRN,' Warning Message(s)'
      WRITE(IOUNIT,*) 'A Total of ',INFO,' Informational Message(s)'
      IF (ICLM .GT. 0) THEN
         WRITE(IOUNIT,*) ' '
         WRITE(IOUNIT,*) 'A Total of ',ICLM,' Calm Hours Identified'
      END IF
      IF (IMSG .GT. 0) THEN
         WRITE(IOUNIT,*) ' '
         WRITE(IOUNIT,*) 'A Total of ',IMSG,' Missing Hours Identified'
      END IF
      WRITE(IOUNIT,*) ' '

C     Write Out All The Fatal Error Messages
      WRITE(IOUNIT,*) ' '
      WRITE(IOUNIT,*) '   ******** FATAL ERROR MESSAGES ******** '
      REWIND IERUNT
      EOF = .FALSE.
      J = 0
      DO WHILE (.NOT. EOF)
         READ(IERUNT,1116,END=99,ERR=9999) PATH,ERRTP,ERRCD,IERRLN,
     &                                     MODNAM,ERRMG1,ERRMG2
         IF (ERRTP .EQ. 'E') THEN
            J = J + 1
            WRITE(IOUNIT,1117) PATH,ERRTP,ERRCD,IERRLN,MODNAM,
     &                         ERRMG1,ERRMG2
         END IF
         GO TO 11
 99      EOF = .TRUE.
 11      CONTINUE
      END DO

C     If No Fatal Error Messages, Then Write 'NONE'
      IF (J .EQ. 0) THEN
         WRITE(IOUNIT,*) '              ***  NONE  ***         '
         WRITE(IOUNIT,*) ' '
      END IF

C     Write Out All The Warning Messages
      WRITE(IOUNIT,*) ' '
      WRITE(IOUNIT,*) '   ********   WARNING MESSAGES   ******** '
      REWIND IERUNT
      EOF = .FALSE.
      J = 0
      DO WHILE (.NOT. EOF)
         READ(IERUNT,1116,END=999,ERR=9999) PATH,ERRTP,ERRCD,IERRLN,
     &                                      MODNAM,ERRMG1,ERRMG2
         IF (ERRTP .EQ. 'W') THEN
            J = J + 1
            WRITE(IOUNIT,1117) PATH,ERRTP,ERRCD,IERRLN,MODNAM,
     &                         ERRMG1,ERRMG2
         END IF
         GO TO 111
 999     EOF = .TRUE.
 111     CONTINUE
      END DO

C     If No Warning Messages, Then Write 'NONE'
      IF (J .EQ. 0) THEN
         WRITE(IOUNIT,*) '              ***  NONE  ***        '
         WRITE(IOUNIT,*) ' '
      END IF

 1116 FORMAT(A2,1X,A1,A3,I6,1X,A6,1X,A50,1X,A8)
 1117 FORMAT(1X,A2,1X,A1,A3,I6,1X,A6,':',A50,1X,A8)

C     Use BACKSPACE To Reposition Temporary Error Message File Ahead of EOF;
C     This Is Needed in Order To Allow For Additional Message Writes
      BACKSPACE IERUNT

      GO TO 1000

C     WRITE Error Message: Error Reading Temp Error Message File
 9999 CALL ERRHDL(PATH,MODNAM,'E','510','ERRORMSG')

 1000 RETURN
      END

      SUBROUTINE MSGWRT
C***********************************************************************
C                 MSGWRT Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: To Print Out The Error Summary Table
C
C        PROGRAMMER: Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Error Message File
C
C        OUTPUTS: The Error Message File
C
C        CALLED FROM:  This is A Utility Program
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER ERRTP*1, ERRCD*3, ERRMG1*50, ERRMG2*8

C     Variable Initialization
      MODNAM = 'MSGWRT'

C     Write Out The Header Of The Message File
      WRITE(IERWRT,*) ' '
      WRITE(IERWRT,*) '     *********** Error Message List ********* '
      WRITE(IERWRT,*) ' '
      WRITE(IERWRT,*) '   PW     --- Pathway                           '
      WRITE(IERWRT,*) '   Code   --- Error Type + Error Code           '
      WRITE(IERWRT,*) '   L#     --- The Line Number Where Error Occurs'
      WRITE(IERWRT,*) '   ModNam --- Module Name In Which Error Occurs '
      WRITE(IERWRT,*) '   Hints  --- Hints For The Possible Solution   '
      WRITE(IERWRT,*) '     ***************************************** '
      WRITE(IERWRT,*) ' '
      WRITE(IERWRT,1114)
      WRITE(IERWRT,1115)
 1114 FORMAT('PW CODE  L#  MODNAM  ',18X,'ERROR MESSAGES',20X,'HINTS')
 1115 FORMAT('-- ---- ---- ------  ',50('-'),' --------')
      WRITE(IERWRT,*) ' '
      REWIND IERUNT
      EOF = .FALSE.

      DO WHILE (.NOT. EOF)
         READ(IERUNT,1116,END=99,ERR=999) PATH,ERRTP,ERRCD,IERRLN,
     &                                    MODNAM,ERRMG1,ERRMG2
         WRITE(IERWRT,1117) PATH,ERRTP,ERRCD,IERRLN,
     &                      MODNAM,ERRMG1,ERRMG2
         GO TO 11
 99      EOF = .TRUE.
 11      CONTINUE
      END DO

 1116 FORMAT(A2,1X,A1,A3,I6,1X,A6,1X,A50,1X,A8)
 1117 FORMAT(A2,1X,A1,A3,I6,1X,A6,':',A50,1X,A8)

      GO TO 1000

C     WRITE Error Message: Error Reading Temp Error Message File
 999  CALL ERRHDL(PATH,MODNAM,'E','510','ERRORMSG')

 1000 RETURN
      END

C----------------------------------------------------------------------
C     Courtesy: Jay Sandhu
C               email: jsandhu@esri.com
C
C
C Please cite David H. Douglas, COLLECTED ALGORITHMS, Cambridge MA:
C Harvard Laboratory for Computer Graphics, 1974
C
C This is my reinvention buster.
C 1974 1974 1974 1974 1974 1974 1974 1974 1974 1974 1974 1974
C
C>>>PNPY
C     .................................................................
C
C        SUBROUTINE PNPOLY
C
C        PURPOSE
C           TO DETERMINE WHETHER A POINT IS INSIDE A POLYGON
C
C        USAGE
C           CALL PNPOLY (PX, PY, X, Y, N, INOUT )
C
C        DESCRIPTION OF THE PARAMETERS
C           PX      - X-COORDINATE OF POINT IN QUESTION.
C           PY      - Y-COORDINATE OF POINT IN QUESTION.
C           X       - N LONG VECTOR CONTAINING X-COORDINATES OF
C                     VERTICES OF POLYGON.
C           Y       - N LONG VECTOR CONTAINING Y-COORDINATES OF
C                     VERTICES OF POLYGON.
C           N       - NUMBER OF VERTICES IN THE POLYGON.
C           INOUT   - THE SIGNAL RETURNED:
C                     -1 IF THE POINT IS OUTSIDE OF THE POLYGON,
C                      0 IF THE POINT IS ON AN EDGE OR AT A VERTEX,
C                      1 IF THE POINT IS INSIDE OF THE POLYGON.
C
C        REMARKS
C           THE VERTICES MAY BE LISTED IN CLOCKWISE OR ANTICLOCKWISE
C           ORDER.  FOR THIS SUBROUTINE A POINT IS CONSIDERED INSIDE
C           THE POLYGON IF IT IS LOCATED IN THE ENCLOSED AREA DEFINED
C           BY THE LINE FORMING THE POLYGON.
C           THE INPUT POLYGON MAY BE A COMPOUND POLYGON CONSISTING
C           OF SEVERAL SEPARATE SUBPOLYGONS. IF SO, THE FIRST VERTEX
C           OF EACH SUBPOLYGON MUST BE REPEATED, AND WHEN CALCULATING
C           N, THESE FIRST VERTICES MUST BE COUNTED TWICE.
C           INOUT IS THE ONLY PARAMETER WHOSE VALUE IS CHANGED.
C           PNPOLY CAN HANDLE ANY NUMBER OF VERTICES IN THE POLYGON.
C           WRITTEN BY RANDOLPH FRANKLIN, UNIVERSITY OF OTTAWA, 6/72.
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           NONE
C
C        METHOD
C           A VERTICAL SEMI-INFINITE LINE IS DRAWN UP FROM THE POINT
C           IN QUESTION. IF IT CROSSES THE POLYGON AN ODD NUMBER OF
C           TIMES, THE POINT IS INSIDE THE POLYGON.
C
C     .................................................................
C
      SUBROUTINE PNPOLY (PX,PY,X,Y,N,INOUT)

      REAL X(N),Y(N)
      LOGICAL IX,IY,JX,JY,EOR

C     EXCLUSIVE OR FUNCTION.
      EOR(IX,IY)=(IX.OR.IY).AND..NOT.(IX.AND.IY)

      INOUT=-1
      DO 4 I=1,N
         XI=X(I)-PX
         YI=Y(I)-PY
C        CHECK WHETHER THE POINT IN QUESTION IS AT THIS VERTEX.
         IF (XI.EQ.0.0.AND.YI.EQ.0.0) GO TO 2
C        J IS NEXT VERTEX NUMBER OF POLYGON.
         J=1+MOD(I,N)
         XJ=X(J)-PX
         YJ=Y(J)-PY
C        IS THIS LINE OF 0 LENGTH ?
         IF (XI.EQ.XJ.AND.YI.EQ.YJ) GO TO 4
         IX=XI.GE.0.0
         IY=YI.GE.0.0
         JX=XJ.GE.0.0
         JY=YJ.GE.0.0
C        CHECK WHETHER (PX,PY) IS ON VERTICAL SIDE OF POLYGON.
         IF (XI.EQ.0.0.AND.XJ.EQ.0.0.AND.EOR(IY,JY)) GO TO 2
C        CHECK WHETHER (PX,PY) IS ON HORIZONTAL SIDE OF POLYGON.
         IF (YI.EQ.0.0.AND.YJ.EQ.0.0.AND.EOR(IX,JX)) GO TO 2
C        CHECK WHETHER BOTH ENDS OF THIS SIDE ARE COMPLETELY 1) TO RIGHT
C        OF, 2) TO LEFT OF, OR 3) BELOW (PX,PY).
         IF (.NOT.((IY.OR.JY).AND.EOR(IX,JX))) GO TO 4
C        DOES THIS SIDE OBVIOUSLY CROSS LINE RISING VERTICALLY FROM (PX,PY
         IF (.NOT.(IY.AND.JY.AND.EOR(IX,JX))) GO TO 1
         INOUT=-INOUT
         GO TO 4

1        IF ((YI*XJ-XI*YJ)/(XJ-XI)) 4,2,3
2        INOUT=0
         RETURN
3        INOUT=-INOUT
4     CONTINUE

      RETURN
      END

      BLOCK DATA INIT
C***********************************************************************
C
C     BLOCK DATA SUBPROGRAM OF THE ISC - Version 2 MODEL
c ----------------------------------------------------------------------
c ---    ISC-PRIME     Version 1.0    Level 980310              Modified
c ---        V. Tino
c ---        Earth Tech, Inc.
c ---        Prepared for EPRI under contract WO3527-01
c ----------------------------------------------------------------------
C
C     PURPOSE: Initialize Data in COMMON Blocks
C
C     MODIFIED:  To Include Terrain Grid pathway - 12/15/93
C
C     MODIFIED:  To Include WET DEPOSITION Arrays - 11/8/93
C
C     MODIFIED:  To Include New Area Source Arrays - 7/7/93
C
C     MODIFIED:  For revised DRY DEPOSITION code - 2/15/93
C
C     MODIFIED:  To Include TOXXFILE Option - 9/29/92
C
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN3.INC'


C***********************************************************************
C     Initialize Model Version Number, VERSN (Year, Julian Day), as a
C     Character Variable
C***********************************************************************

      DATA VERSN /'04269'/


C***********************************************************************
C     Input/Output File Units and Input/Output File Names
C***********************************************************************

      DATA INUNIT/ 7/, IOUNIT/ 8/, MFUNIT/19/, IERUNT/10/,
     &     IERWRT/11/, IDPUNT/12/, IDPUN2/14/, IRSUNT/15/,     
     &     IEVUNT/17/, ITEVUT/18/, IZUNIT/13/,
     &     IHREMI/16/
C*#
      DATA INPFIL/' '/,OUTFIL/' '/


C***********************************************************************
C     Initialize Keyword Array
C***********************************************************************

      DATA (KEYWD(I),I=1,IKN) /'STARTING','FINISHED','TITLEONE',
     &   'TITLETWO','MODELOPT','AVERTIME','POLLUTID','HALFLIFE',
     &   'DCAYCOEF','TERRHGTS','ELEVUNIT','FLAGPOLE','RUNORNOT',
     &   'EVENTFIL','SAVEFILE','INITFILE','MULTYEAR','ERRORFIL',
     &   'LOCATION','SRCPARAM','BUILDHGT','BUILDWID','LOWBOUND',

c --- PRIME ------------------------------
     &   'BUILDLEN','XBADJ   ','YBADJ   ',
c ----------------------------------------

     &   'EMISFACT','EMISUNIT','PARTDIAM','MASSFRAX','PARTDENS',
     &   'PARTSLIQ','PARTSICE','GAS-SCAV','CONCUNIT','DEPOUNIT',
     &   'HOUREMIS',
     &   'SRCGROUP','GRIDCART','GRIDPOLR','DISCCART','DISCPOLR',
     &   'BOUNDARY','BOUNDELV','INPUTFIL','ANEMHGHT','SURFDATA',
     &   'UAIRDATA','STARTEND','DAYRANGE','WDROTATE','WINDPROF',
     &   'DTHETADZ','WINDCATS','RECTABLE','MAXTABLE','DAYTABLE',
     &   'MAXIFILE','POSTFILE','PLOTFILE','TOXXFILE'/


C***********************************************************************
C     Initialize Miscellaneous Variables
C***********************************************************************

      DATA IPROC /366*1/, KAVE /NAVE*0/, EXPLIM /-50.0/
      DATA UCAT /1.54, 3.09, 5.14, 8.23, 10.8/
      DATA MODOPS /17*'      '/


C***********************************************************************
C     Initialize Default Wind Profile Exponents and DTHETADZ
C***********************************************************************

C     STAB. CLASS  A    B     C     D      E      F
C                 ***  ***   ***   ***    ***    ***
      DATA DTURB  /0.,  0.,   0.,   0.,   0.02, 0.035/,
     &     DTRUR  /0.,  0.,   0.,   0.,   0.02, 0.035/,
     &     PURB  /0.15, 0.15, 0.20, 0.25, 0.30, 0.30/,
     &     PRUR  /0.07, 0.07, 0.10, 0.15, 0.35, 0.55/


C***********************************************************************
C     Initialize Receptor Arrays
C***********************************************************************

      DATA AXR/NREC*0.0/, AYR/NREC*0.0/, AZELEV/NREC*0.0/,
     &     AZFLAG/NREC*0.0/


C***********************************************************************
C     Initialize Source Arrays  (Multi-dimensional Arrays Initialized in
C                                SUBROUTINE VARINI)
C***********************************************************************

      DATA AXS/NSRC*0.0/, AYS/NSRC*0.0/, AZS/NSRC*0.0/, AQS/NSRC*0.0/,
     &     AHS/NSRC*0.0/, ADS/NSRC*0.0/, AVS/NSRC*0.0/, ATS/NSRC*0.0/,
     &     ASYINI/NSRC*0.0/, ASZINI/NSRC*0.0/, AXINIT/NSRC*0.0/,
     &     AYINIT/NSRC*0.0/, AANGLE/NSRC*0.0/,
     &     SOPCRD/NSRC*'N'/


C***********************************************************************
C     Initialize Setup Status Arrays
C***********************************************************************

c --- PRIME -------------------------------------------------------
c prm DATA ICSTAT/20*0/, ISSTAT/20*0/, IRSTAT/20*0/, IMSTAT/20*0/,
      DATA ICSTAT/20*0/, ISSTAT/23*0/, IRSTAT/20*0/, IMSTAT/20*0/,
     &     IOSTAT/20*0/, ITSTAT/20*0/
c -----------------------------------------------------------------


C***********************************************************************
C     Initialize Error Code and Message Arrays
C***********************************************************************

      DATA ERRCOD(1)/'100'/,
     &  ERRMSG(1)/'Invalid Pathway Specified. The Troubled Pathway is'/
      DATA ERRCOD(2)/'105'/,
     &  ERRMSG(2)/'Invalid Keyword Specified. The Troubled Keyword is'/
      DATA ERRCOD(3)/'110'/,
     &  ERRMSG(3)/'Keyword is Not Valid for This Pathway.  Keyword is'/
      DATA ERRCOD(4)/'115'/,
     &  ERRMSG(4)/'STARTING or FINISHED Out of Sequence:  Pathway =  '/
      DATA ERRCOD(5)/'120'/,
     &  ERRMSG(5)/'Pathway is Out of Sequence:  Pathway =            '/
      DATA ERRCOD(6)/'125'/,
     &  ERRMSG(6)/'Missing FINISHED-Runstream File Incomplete: ISTAT='/
      DATA ERRCOD(7)/'130'/,
     &  ERRMSG(7)/'Missing Mandatory Keyword.  The Missing Keyword is'/
      DATA ERRCOD(8)/'135'/,
     &  ERRMSG(8)/'Duplicate Nonrepeatable Keyword Specified:Keyword='/
      DATA ERRCOD(9)/'140'/,
     &  ERRMSG(9)/'Invalid Order of Keyword.  The Troubled Keyword is'/
      DATA ERRCOD(10)/'145'/,
     &  ERRMSG(10)/'Conflicting Options: MULTYEAR and Re-Start Option '/
      DATA ERRCOD(11)/'150'/,
     &  ERRMSG(11)/'Conflicting Options: MULTYEAR for Wrong Pollutant '/
      DATA ERRCOD(12)/'155'/,
     &  ERRMSG(12)/'Conflicting Decay Keyword. Inputs Ignored for     '/
      DATA ERRCOD(13)/'160'/,
     &  ERRMSG(13)/'Duplicate ORIG Secondary Keyword for GRIDPOLR:    '/
      DATA ERRCOD(14)/'170'/,
     &  ERRMSG(14)/'Invalid Secondary Keyword for Receptor Grid:      '/
      DATA ERRCOD(15)/'175'/,
     &  ERRMSG(15)/'Missing Secondary Keyword END for Receptor Grid:  '/
      DATA ERRCOD(16)/'180'/,
     &  ERRMSG(16)/'Conflicting Secondary Keyword for Receptor Grid:  '/
      DATA ERRCOD(17)/'185'/,
     &  ERRMSG(17)/'Missing Receptor Keywords. No Receptors Specified.'/
      DATA ERRCOD(18)/'190'/,
     &  ERRMSG(18)/'No Keywords for OU Path and No PERIOD/ANNUAL Aves.'/
      DATA ERRCOD(19)/'195'/,
     &  ERRMSG(19)/'Incompatible Option Used With SAVEFILE or INITFILE'/
      DATA ERRCOD(20)/'200'/,
     &  ERRMSG(20)/'Missing Parameter(s). No Options Specified For    '/
      DATA ERRCOD(21)/'201'/,
     &  ERRMSG(21)/'Not Enough Parameters Specified For the Keyword of'/
      DATA ERRCOD(22)/'202'/,
     &  ERRMSG(22)/'Too Many Parameters Specified For the Keyword of  '/
      DATA ERRCOD(23)/'203'/,
     &  ERRMSG(23)/'Invalid Parameter Specified.  Troubled Parameter: '/
      DATA ERRCOD(24)/'204'/,
     &  ERRMSG(24)/'Option Parameters Conflict.  Forced by Default to '/
      DATA ERRCOD(25)/'205'/,
     &  ERRMSG(25)/'No Option Parameter Setting.  Forced by Default to'/
      DATA ERRCOD(26)/'206'/,
     &  ERRMSG(26)/'Regulatory DFAULT Overrides Non-DFAULT Option For '/
      DATA ERRCOD(27)/'207'/,
     &  ERRMSG(27)/'No Parameters Specified. Default Values Will Used.'/
      DATA ERRCOD(28)/'208'/,
     &  ERRMSG(28)/'Illegal Numerical Field Encountered in            '/
      DATA ERRCOD(29)/'209'/,
     &  ERRMSG(29)/'Negative Value Appears For Non-negative Variable. '/
      DATA ERRCOD(30)/'210'/,
     &  ERRMSG(30)/'Number of Short Term Averages Exceeds Max:  NAVE= '/
      DATA ERRCOD(31)/'211'/,
     &  ERRMSG(31)/'Duplicate Averaging Period Specified for Keyword  '/
      DATA ERRCOD(32)/'212'/,
     &  ERRMSG(32)/'END Encountered Without (X,Y) Points Properly Set '/
      DATA ERRCOD(33)/'213'/,
     &  ERRMSG(33)/'ELEV Input Inconsistent With Option: Input Ignored'/
      DATA ERRCOD(34)/'214'/,
     &  ERRMSG(34)/'ELEV Input Inconsistent With Option: Defaults Used'/
      DATA ERRCOD(35)/'215'/,
     &  ERRMSG(35)/'FLAG Input Inconsistent With Option: Input Ignored'/
      DATA ERRCOD(36)/'216'/,
     &  ERRMSG(36)/'FLAG Input Inconsistent With Option: Defaults Used'/
      DATA ERRCOD(37)/'217'/,
     &  ERRMSG(37)/'More Than One Delimiter In A Field for Keyword    '/
      DATA ERRCOD(38)/'218'/,
     &  ERRMSG(38)/'Number of (X,Y) Points Not Match With Number Of   '/
      DATA ERRCOD(39)/'219'/,
     &  ERRMSG(39)/'Number Of Receptors Specified Exceeds Max:  NREC= '/
      DATA ERRCOD(40)/'220'/,
     &  ERRMSG(40)/'Missing Origin (Use Default = 0,0) In GRIDPOLR    '/
      DATA ERRCOD(41)/'221'/,
     &  ERRMSG(41)/'Missing Distance Setting In Polar Network         '/
      DATA ERRCOD(42)/'222'/,
     &  ERRMSG(42)/'Missing Degree Or Dist Setting In Polar Network   '/
      DATA ERRCOD(43)/'223'/,
     &  ERRMSG(43)/'Missing Distance or Degree Field in               '/
      DATA ERRCOD(44)/'224'/,
     &  ERRMSG(44)/'Number of Receptor Networks Exceeds Max:  NNET=   '/
      DATA ERRCOD(45)/'225'/,
     &  ERRMSG(45)/'Number of X-Coords Specified Exceeds Max:  IXM=   '/
      DATA ERRCOD(46)/'226'/,
     &  ERRMSG(46)/'Number of Y-Coords Specified Exceeds Max:  IYM=   '/
      DATA ERRCOD(47)/'227'/,
     &  ERRMSG(47)/'No Receptors Were Defined on the RE Pathway.      '/
      DATA ERRCOD(48)/'228'/,
     &  ERRMSG(48)/'Default(s) Used for Missing Parameters on Keyword '/
      DATA ERRCOD(49)/'229'/,
     &  ERRMSG(49)/'Too Many Parameters - Inputs Ignored on Keyword   '/
      DATA ERRCOD(50)/'230'/,
     &  ERRMSG(50)/'Not Enough Numerical Values Specified for         '/
      DATA ERRCOD(51)/'231'/,
     &  ERRMSG(51)/'Too Many Numerical Values Specified for           '/
      DATA ERRCOD(52)/'232'/,
     &  ERRMSG(52)/'Number Of Specified Sources Exceeds Maximum: NSRC='/
      DATA ERRCOD(53)/'233'/,
     &  ERRMSG(53)/'Building Dimensions Specified for Non-POINT Source'/
      DATA ERRCOD(54)/'234'/,
     &  ERRMSG(54)/'Too Many Sectors Input for                        '/
      DATA ERRCOD(55)/'235'/,
     &  ERRMSG(55)/'Number of Source Groups Exceeds Maximum:  NGRP=   '/
      DATA ERRCOD(56)/'236'/,
     &  ERRMSG(56)/'Not Enough BUILDHGTs Specified for SourceID       '/
      DATA ERRCOD(57)/'237'/,
     &  ERRMSG(57)/'Not Enough BUILDWIDs Specified for SourceID       '/
      DATA ERRCOD(58)/'238'/,
     &  ERRMSG(58)/'Not Enough LOWBOUNDs Specified for SourceID       '/
      DATA ERRCOD(59)/'239'/,
     &  ERRMSG(59)/'Not Enough QFACTs Specified for SourceID          '/
      DATA ERRCOD(60)/'240'/,
     &  ERRMSG(60)/'Inconsistent Number of Settling/Removal Cats for  '/
      DATA ERRCOD(61)/'242'/,
     &  ERRMSG(61)/'No Particle Size Categories Specified for SRCID   '/
      DATA ERRCOD(62)/'244'/,
     &  ERRMSG(62)/'Too Many Particle Categories Specified for        '/
      DATA ERRCOD(63)/'245'/,
     &  ERRMSG(63)/'No. of Settling/Removal Cats Exceeds Max:  NPDMAX='/
      DATA ERRCOD(64)/'248'/,
     &  ERRMSG(64)/'No Sources Were Defined on the SO Pathway.        '/
      DATA ERRCOD(65)/'250'/,
     &  ERRMSG(65)/'Duplicate XPNT/DIST or YPNT/DIR Specified for GRID'/
      DATA ERRCOD(66)/'252'/,
     &  ERRMSG(66)/'Duplicate Receptor Network ID Specified.  NETID = '/
      DATA ERRCOD(67)/'255'/,
     &  ERRMSG(67)/'Boundary Receptor Distances Not Defined Yet for   '/
      DATA ERRCOD(68)/'260'/,
     &  ERRMSG(68)/'Number of Emission Factors Exceeds Max:      NQF= '/
      DATA ERRCOD(69)/'270'/,
     &  ERRMSG(69)/'Number of High Values Specified Exceeds Max: NVAL='/
      DATA ERRCOD(70)/'280'/,
     &  ERRMSG(70)/'Number of Max Values Specified Exceeds Max:  NMAX='/
C     Message '290' specified below as array element 120.
      DATA ERRCOD(71)/'300'/,
     &  ERRMSG(71)/'Specified SRCID Has Not Been Defined Yet: KEYWORD='/
      DATA ERRCOD(72)/'310'/,
     &  ERRMSG(72)/'Attempt to Define Duplicate LOCATION Card for SRC:'/
      DATA ERRCOD(73)/'315'/,
     &  ERRMSG(73)/'Attempt to Define Duplicate SRCPARAM Card for SRC:'/
      DATA ERRCOD(74)/'320'/,
     &  ERRMSG(74)/'Source Parameter May Be Out-of-Range for Parameter'/
      DATA ERRCOD(75)/'325'/,
     &  ERRMSG(75)/'Negative Exit Velocity (Set=1.0E-5) for SRCID:    '/
      DATA ERRCOD(76)/'330'/,
     &  ERRMSG(76)/'Mass Fraction Parameters Do Not Sum to 1. for Src '/
      DATA ERRCOD(77)/'332'/,
     &  ERRMSG(77)/'Mass Fraction Parameter Out-of-Range for Source   '/
      DATA ERRCOD(78)/'334'/,
     &  ERRMSG(78)/'Particle Density Out-of-Range for Source          '/
      DATA ERRCOD(79)/'340'/,
     &  ERRMSG(79)/'Possible Error In ANHT of ANEMHGHT. The Value is  '/
      DATA ERRCOD(80)/'350'/,
     &  ERRMSG(80)/'Julian Day Out Of Range at                        '/
      DATA ERRCOD(81)/'360'/,
     &  ERRMSG(81)/'2-Digit Year Specified: Valid for Range 1901-2099 '/
      DATA ERRCOD(82)/'370'/,
     &  ERRMSG(82)/'Invalid Date: 2/29 In a Non-leap Year.            '/
      DATA ERRCOD(83)/'380'/,
     &  ERRMSG(83)/'This Input Variable is Out-of-Range:              '/
      DATA ERRCOD(84)/'385'/,
     &  ERRMSG(84)/'Averaging Period .NE. 1-Hr for TOXXFILE Option    '/
      DATA ERRCOD(85)/'400'/,
     &  ERRMSG(85)/'No Convergence Reached in SUB. CUBIC.  KURDAT=    '/
      DATA ERRCOD(86)/'410'/,
     &  ERRMSG(86)/'Flow Vector Out-of-Range.   KURDAT=               '/
      DATA ERRCOD(87)/'420'/,
     &  ERRMSG(87)/'Wind Speed Out-of-Range.   KURDAT=                '/
      DATA ERRCOD(88)/'430'/,
     &  ERRMSG(88)/'Ambient Temperature Data Out-of-Range.  KURDAT=   '/
      DATA ERRCOD(89)/'440'/,
     &  ERRMSG(89)/'Calm Hour Identified in Meteorology Data File at  '/
      DATA ERRCOD(90)/'450'/,
     &  ERRMSG(90)/'Error in Meteor. File - Record Out of Sequence at '/
      DATA ERRCOD(91)/'460'/,
     &  ERRMSG(91)/'Missing Hour Identified in Meteor. Data File at   '/
      DATA ERRCOD(92)/'470'/,
     &  ERRMSG(92)/'Mixing Height Value is < or = 0.0.   KURDAT=      '/
      DATA ERRCOD(93)/'500'/,
     &  ERRMSG(93)/'Fatal Error Occurs Opening the Data File of       '/
      DATA ERRCOD(94)/'510'/,
     &  ERRMSG(94)/'Fatal Error Occurs During Reading of the File of  '/
      DATA ERRCOD(95)/'520'/,
     &  ERRMSG(95)/'Fatal Error Occurs During Writing to the File of  '/
      DATA ERRCOD(96)/'530'/,
     &  ERRMSG(96)/'Error Occurs Reading Met Station or Year:File Says'/
      DATA ERRCOD(97)/'540'/,
     &  ERRMSG(97)/'No RECTABLE/MAXTABLE/DAYTABLE for Average Period  '/
      DATA ERRCOD(98)/'550'/,
     &  ERRMSG(98)/'File Unit/Name Conflict for the Output Option:    '/
      DATA ERRCOD(99)/'560'/,
     &  ERRMSG(99)/'User Specified File Unit < 20 for OU Keyword:     '/
      DATA ERRCOD(100)/'565'/,
     & ERRMSG(100)/'Possible Conflict With Dynamically Allocated FUNIT'/
      DATA ERRCOD(101)/'570'/,
     & ERRMSG(101)/'Problem Reading Temporary Event File for Event:   '/
      DATA ERRCOD(102)/'580'/,
     & ERRMSG(102)/'End of File Reached Trying to Read the File of    '/
      DATA ERRCOD(103)/'305'/,
     & ERRMSG(103)/'Terrain Grid Does Not Cover Modeling Area, Change:'/
      DATA ERRCOD(104)/'144'/,
     & ERRMSG(104)/'Conflicting Options: NOSMPL with FLAT Terrain     '/
      DATA ERRCOD(105)/'151'/,
     & ERRMSG(105)/'CO ELEVUNIT card obsolescent: use RE ELEVUNIT card'/
      DATA ERRCOD(106)/'152'/,
     & ERRMSG(106)/'ELEVUNIT card must be first for this Pathway:     '/
      DATA ERRCOD(107)/'153'/,
     & ERRMSG(107)/'Cannot use CO ELEVUNIT card with ELEVUNIT card for'/
      DATA ERRCOD(108)/'391'/,
     & ERRMSG(108)/'Aspect ratio (L/W) of area source greater than 10 '/
      DATA ERRCOD(109)/'392'/,
     & ERRMSG(109)/'Aspect ratio (L/W) of open pit is greater than 10 '/
      DATA ERRCOD(110)/'393'/,
     & ERRMSG(110)/'Terrain Grid Value Differs >50% From Source Elev. '/
      DATA ERRCOD(111)/'394'/,
     & ERRMSG(111)/'Terrain Grid Value Differs >50% From Receptor Elev'/
      DATA ERRCOD(112)/'322'/,
     & ERRMSG(112)/'Release Height Exceeds Effective Depth for OPENPIT'/
      DATA ERRCOD(113)/'243'/,
     & ERRMSG(113)/'Scav. Coef. may be out-of-range for SRCID         '/
      DATA ERRCOD(114)/'143'/,
     & ERRMSG(114)/'Conflicting Options: UNFORM with Dry or Wet Depos.'/
      DATA ERRCOD(115)/'435'/,
     & ERRMSG(115)/'Surface Roughness Length Out-of-Range.  KURDAT=   '/


      DATA ERRCOD(116)/'342'/,
     &  ERRMSG(116)/'Src ID Mismatch in Hourly Emissions File for ID ='/
      DATA ERRCOD(117)/'344'/,
     &  ERRMSG(117)/'Hourly Emission Rate is Zero for KURDAT ='/
C*----
C*#

      DATA ERRCOD(118)/'157'/,
     & ERRMSG(118)/'EMISUNIT Keyword Used With More Than 1 Output Type'/
      DATA ERRCOD(119)/'158'/,
     & ERRMSG(119)/'EMISUNIT Keyword Used With the Following Keyword: '/
      DATA ERRCOD(120)/'290'/,
     & ERRMSG(120)/'Number of Output Types Specified Exceeds Max:NTYP='/
      DATA ERRCOD(121)/'295'/,
     & ERRMSG(121)/'PERIOD and ANNUAL averages are both selected for  '/
      DATA ERRCOD(122)/'455'/,
     & ERRMSG(122)/'Date/time Mismatch: Hourly Emission File. KURDAT ='/
      DATA ERRCOD(123)/'323'/,
     & ERRMSG(123)/'No Particle Categories Specified for OPENPIT Src. '/

c --- PRIME -----------------------------------------------------------
      DATA ERRCOD(130)/'241'/,
     &  ERRMSG(130)/'Not Enough BUILDLENs Specified for SourceID      '/
      DATA ERRCOD(131)/'246'/,
     &  ERRMSG(131)/'Not Enough XBADJs Specified for SourceID         '/
      DATA ERRCOD(132)/'247'/,
     &  ERRMSG(132)/'Not Enough YBADJs Specified for SourceID         '/
c ---------------------------------------------------------------------

      END
      SUBROUTINE METEXT
C***********************************************************************
C                METEXT Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Controls Extraction and Quality Assurance of
C                 One Hour of Meteorological Data
C
C        PROGRAMMER: ROGER BRODE, JEFF WANG
C        MODIFIED BY D. Strimaitis, SRC (for Wet & Dry DEPOSITION)
C
C        DATE:    November 8, 1993
C
C        MODIFIED BY D. Strimaitis, SRC (for Dry DEPOSITION)
C        (DATE:    February 15, 1993)
C
C        MODIFIED:   To avoid potential math error due to negative
C                    ambient temperatures in calculating the square
C                    root of the stability parameter, RTOFS - 4/19/93
C
C        MODIFIED:
C        7/27/94     J. Paumier, PES, Inc.
C                    The variables for displacement height, ZDM and
C                    AZDM(), were removed from the input to and output
C                    from ISC-COMPDEP.  The following format statements
C                    also were affected: 9009, 9026, 9032, 9033
C
C*       7/27/94     J. Hardikar, PES, Inc.
C*                   Added code to calculate reference wind speed at 10m
C*                   to be used for OPENPIT source algorithms
C
C        INPUTS:  Meteorology File Specifications
C
C        OUTPUTS: Meteorological Variables for One Hour
C
C        CALLED FROM:   HRLOOP
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
C     Declare Arrays for Use With Unformatted Input Met Files
      INTEGER IKST(24), NDAY(12)
      REAL AUREF(24), ATA(24), AAFV(24), AAFVR(24), AZI(2,24)
      COMMON /UNFMET/ IKST, AUREF, ATA, AAFV, AAFVR, AZI

C     Variable Initializations
      DATA NDAY/31,59,90,120,151,181,212,243,273,304,334,365/
      MODNAM = 'METEXT'
      PATH   = 'MX'

C     Save Value of Last YR/MN/DY/HR and Previous Hour
      IPDATE = KURDAT
      IPHOUR = IHOUR

C     READ Meteorology Data Based on Format --
C     When DRY deposition is modeled, U-star, L, and z0 (surface
C     roughness length) are read in addition to the standard RAMMET
C     data.  These must be provided at the end of each hourly record
C     for the FORMATTED ASCII, CARD, and FREE options.
C
C     When WET deposition is modeled, ipcode (precip.
C     code) and prate (precip. rate in mm/hr) must also be added to
C     each hourly record.
C     The format statement allows for all additional data:

 9009 FORMAT(4I2,2F9.4,F6.1,I2,2F7.1,2F8.4,F9.4,F10.1,F8.4,I4,F7.2)
cjop  FORMAT(4I2,2F9.4,F6.1,I2,2F7.1,2F8.4,F9.4,F10.1,F8.4,F5.1,I4,F7.2)

C     Initialize USTAR, EL, Z0M, IPCODE, AND PRATE to ZERO for hour
      USTAR=0.0
      EL=0.0
      Z0M=0.0
cjop  ZDM=0.0
      IPCODE=0
      PRATE=0.0

      IF (IMONTH .EQ. 12 .AND. IDAY .EQ. 31 .AND. IHOUR .EQ. 24) THEN
C        End of year has been reached - check for presence of header
C        record at beginning of next year for multi-year data files.
         IF (METFRM .NE. 'UNFORM') THEN
C           Multi-year data files applies only to ASCII files.
            READ(MFUNIT,*,END=1000,ERR=998,IOSTAT=IOERRN) JSSI, JSYI,
     &                                                    JUSI, JUYI
            IF (JSSI .NE. IDSURF .AND. JUSI .NE. IDUAIR) THEN
C              Station IDs don't match runstream input, assume that header
C              record is missing.  Backspace met file and continue processing.
               BACKSPACE MFUNIT
            END IF

            GO TO 1001

C           Error reading 'header record' - assume that header record is
C           missing.  Backspace met file and continue processing.
 998        BACKSPACE MFUNIT

         END IF
      END IF

1001  CONTINUE

      IF (LWPART .OR. LWGAS) THEN
C        WET Deposition -- Read Met. for Both Wet & Dry Deposition
         IF (METFRM .EQ. 'FREE') THEN
C           Read Hourly Records from ASCII File Using FREE Format -
C           Yr, Mn, Dy, Hr, AFV, UREF, TA, KST, ZIRUR, ZIURB, USTAR,
C           EL, Z0M, IPCODE, PRATE
            READ(MFUNIT,*,END=1000,ERR=99,IOSTAT=IOERRN) IYEAR, IMONTH,
     &           IDAY, IHOUR, AFV, UREF, TA, KST, ZIRUR, ZIURB, USTAR,
     &           EL, Z0M, IPCODE, PRATE
         ELSE IF (METFRM .EQ. 'CARD') THEN
C           Read Hourly Records from ASCII File Using CARD Format -
C           Yr, Mn, Dy, Hr, AFV, UREF, TA, KST, ZIRUR, ZIURB, P, DTDZ,
C           USTAR, EL, Z0M, IPCODE, PRATE
            READ(MFUNIT,9009,END=1000,ERR=99,IOSTAT=IOERRN) IYEAR,
     &           IMONTH, IDAY, IHOUR, AFV, UREF, TA, KST, ZIRUR, ZIURB,
     &           P, DTDZ, USTAR, EL, Z0M, IPCODE, PRATE
         ELSE
C           Read Hourly Records from Formatted ASCII File Using METFRM
            READ(MFUNIT,METFRM,END=1000,ERR=99,IOSTAT=IOERRN) IYEAR,
     &           IMONTH, IDAY, IHOUR, AFV, UREF, TA, KST, ZIRUR, ZIURB,
     &           USTAR, EL, Z0M, IPCODE, PRATE
         ENDIF
      ELSE IF (LDPART) THEN
C        Just DRY Deposition
         IF (METFRM .EQ. 'FREE') THEN
C           Read Hourly Records from ASCII File Using FREE Format -
C           Yr, Mn, Dy, Hr, AFV, UREF, TA, KST, ZIRUR, ZIURB, USTAR,
C           EL, Z0M
            READ(MFUNIT,*,END=1000,ERR=99,IOSTAT=IOERRN) IYEAR, IMONTH,
     &           IDAY, IHOUR, AFV, UREF, TA, KST, ZIRUR, ZIURB, USTAR,
     &           EL, Z0M
         ELSE IF (METFRM .EQ. 'CARD') THEN
C           Read Hourly Records from ASCII File Using CARD Format -
C           Yr, Mn, Dy, Hr, AFV, UREF, TA, KST, ZIRUR, ZIURB, P, DTDZ,
C           USTAR, EL, Z0M
            READ(MFUNIT,9009,END=1000,ERR=99,IOSTAT=IOERRN) IYEAR,
     &           IMONTH, IDAY, IHOUR, AFV, UREF, TA, KST, ZIRUR, ZIURB,
     &           P, DTDZ, USTAR, EL, Z0M
         ELSE
C           Read Hourly Records from Formatted ASCII File Using METFRM
            READ(MFUNIT,METFRM,END=1000,ERR=99,IOSTAT=IOERRN) IYEAR,
     &           IMONTH, IDAY, IHOUR, AFV, UREF, TA, KST, ZIRUR, ZIURB,
     &           USTAR, EL, Z0M
         ENDIF
      ELSE
C        No Deposition Met Needed
         IF (METFRM .EQ. 'UNFORM') THEN
C           Read 24-hour Records from RAMMET-type UNFORMatted File
            IF (NEWDAY) THEN
               READ(MFUNIT,END=1000,ERR=99,IOSTAT=IOERRN) IYEAR, IMONTH,
     &              DAY, IKST, AUREF, ATA, AAFV, AAFVR, AZI
               NEWDAY = .FALSE.
               IHOUR = 0
               JDAY = INT(DAY)
C              Determine Day of Month Number, IDAY     ---   CALL GREGOR
               CALL GREGOR(IYEAR,IMONTH,JDAY,IDAY)
            END IF
            IHOUR = IHOUR + 1
C           Specify Flow Vector for Previous Hour, AFVM1
            IF (IHOUR .EQ. 1) THEN
               AFVM1 = AFV24
            ELSE IF (IHOUR .LT. 24) THEN
               AFVM1 = AAFV(IHOUR-1)
            ELSE IF (IHOUR .EQ. 24) THEN
               AFVM1  = AAFV(IHOUR-1)
               AFV24  = AAFV(IHOUR)
               NEWDAY = .TRUE.
            END IF
            IF (AAFV(IHOUR) .EQ. AFVM1 .AND. AUREF(IHOUR) .EQ. 1.0) THEN
C              Set Wind Speed to 0.0 for Calm Hour
               AUREF(IHOUR) = 0.0
            END IF
C           Set Variables for the Current Hour
            AFV   = AAFVR(IHOUR)
            UREF  = AUREF(IHOUR)
            TA    = ATA(IHOUR)
            KST   = IKST(IHOUR)
            ZIRUR = AZI(1,IHOUR)
            ZIURB = AZI(2,IHOUR)
         ELSE IF (METFRM .EQ. 'FREE') THEN
C           Read Hourly Records from ASCII File Using FREE Format -
C           Yr, Mn, Dy, Hr, AFV, UREF, TA, KST, ZIRUR, ZIURB
            READ(MFUNIT,*,END=1000,ERR=99,IOSTAT=IOERRN) IYEAR, IMONTH,
     &           IDAY, IHOUR, AFV, UREF, TA, KST, ZIRUR, ZIURB
         ELSE IF (METFRM .EQ. 'CARD') THEN
C           Read Hourly Records from ASCII File Using CARD Format -
C           Yr, Mn, Dy, Hr, AFV, UREF, TA, KST, ZIRUR, ZIURB, P, DTDZ
            READ(MFUNIT,9009,END=1000,ERR=99,IOSTAT=IOERRN) IYEAR,
     &           IMONTH, IDAY, IHOUR, AFV, UREF, TA, KST, ZIRUR, ZIURB,
     &           P, DTDZ
         ELSE
C           Read Hourly Records from Formatted ASCII File Using METFRM
            READ(MFUNIT,METFRM,END=1000,ERR=99,IOSTAT=IOERRN) IYEAR,
     &           IMONTH, IDAY, IHOUR, AFV, UREF, TA, KST, ZIRUR, ZIURB
         END IF
      ENDIF

C     Retrieve Appropriate Mixing Height
      IF (RURAL) THEN
         ZI = ZIRUR
      ELSE IF (URBAN) THEN
         ZI = ZIURB
      END IF

C     Determine The Current Julian Day and Calculate Current Gregorian Date
C     First Convert Year to 4-Digit Value
      IF (IYEAR .GE. 50 .AND. IYEAR .LE. 99) THEN
         IYR = 1900 + IYEAR
      ELSE IF (IYEAR .LT. 50) THEN
         IYR = 2000 + IYEAR
      END IF
      IF (METFRM .NE. 'UNFORM') THEN
C        Determine Julian Day (Day of Year) Number, JDAY    ---   CALL JULIAN
         CALL JULIAN(IYR,IMONTH,IDAY,JDAY)
      END IF

C     Calculate Integer Variable for Current Date/Hour, KURDAT
      KURDAT = IYEAR*1000000 + IMONTH*10000 + IDAY*100 + IHOUR

      IF (MONTH .AND. IHOUR .EQ. 24) THEN
C        Check for the End of the Month
         IF (IMONTH .EQ. 1 .OR. (MOD(IYR,4) .NE. 0) .OR.
     &      (MOD(IYR,100) .EQ. 0 .AND. MOD(IYR,400) .NE. 0)) THEN
C           Not a Leap Year OR Month = January
            IF (JDAY .EQ. NDAY(IMONTH)) THEN
               ENDMON = .TRUE.
            END IF
         ELSE
C           Leap Year AND Month > January
            IF (JDAY .EQ. NDAY(IMONTH)+1) THEN
               ENDMON = .TRUE.
            END IF
         END IF
      END IF

      ILINE = ILINE + 1

      IF (ILINE .EQ. 1) THEN
C        Write Out Sample of the Meteorology Data
C        (Up to the First 24 Hours)                         ---   CALL METDAT
         CALL METDAT
      END IF

C     Check Data for Calms, Missing, Out-of-Range Values    ---   CALL METCHK
      CALL METCHK

C     Apply ROTANG Adjustment to Flow Vector
      IF (ROTANG .NE. 0.0) THEN
         AFV = AFV - ROTANG
         IF (AFV .LE. 0.0) THEN
            AFV = AFV + 360.
         END IF
      END IF

      IF ((.NOT.CLMHR .OR. .NOT.CLMPRO) .AND.
     &    (.NOT.MSGHR .OR. .NOT.MSGPRO)) THEN
C        Convert Flow Vector in Degrees to Wind Direction in Radians
         WDRAD = (AFV + 180.0) * DTORAD
C        Determine Nearest Ten Degree Sector
         IFVSEC = INT (AFV*0.10 + 0.4999)
         IF (IFVSEC .EQ. 0) IFVSEC = 36
C        Determine SIN and COS of WDRAD for Later Use in SUB. XYDIST
         WDSIN = SIN(WDRAD)
         WDCOS = COS(WDRAD)
      END IF

C     Set Stability Category Logical Flags and Adjust for KST > 6
      UNSTAB = .FALSE.
      NEUTRL = .FALSE.
      STABLE = .FALSE.
      IF (KST .GE. 1 .AND. KST .NE. KSTMSG) THEN
         IF (KST .GT. 6) KST = 6
         IF (KST .LT. 4) THEN
            UNSTAB = .TRUE.
         ELSE IF (KST .EQ. 4) THEN
            NEUTRL = .TRUE.
         ELSE IF (KST .GT. 4) THEN
            STABLE = .TRUE.
         END IF
      ELSE
         KST = KSTMSG
         MSGHR = .TRUE.
      END IF

C*****  Modified by EMI   2/15/96
c   Compressed file output changes
      ubar = uref
      wd2 = afv
      if( metfrm .eq. 'UNFORM') wd2 = aafv( ihour)
      wd4 = afv
      ist = kst
      hm = zi
      ihr = ihour
c***** End of Modification

C     Set Appropriate Wind Speed Category Index
      IF (UREF .LE. UCAT(1)) THEN
         IUCAT = 1
      ELSE IF (UREF .LE. UCAT(2)) THEN
         IUCAT = 2
      ELSE IF (UREF .LE. UCAT(3)) THEN
         IUCAT = 3
      ELSE IF (UREF .LE. UCAT(4)) THEN
         IUCAT = 4
      ELSE IF (UREF .LE. UCAT(5)) THEN
         IUCAT = 5
      ELSE
         IUCAT = 6
      END IF

C     Select Appropriate Power Law Exponent
      IF (KST .NE. KSTMSG .AND. METFRM .NE. 'CARD') THEN
         IF (USERP) THEN
            P = PUSER(KST,IUCAT)
         ELSE IF (URBAN) THEN
            P = PURB(KST)
         ELSE IF (RURAL) THEN
            P = PRUR(KST)
         END IF
      END IF

C*    Scale the Wind Speed from Anemometer Height to 10 meter

      UREF10 = UREF * (10.0/ZREF)**P

C*    Do Not Allow 10m Wind Speed < 1.0 m/s
      IF (UREF10 .LT. 1.0) THEN
         UREF10 = 1.0
      END IF

C     Select Appropriate delta theta/delta z
      IF (KST .NE. KSTMSG .AND. METFRM .NE. 'CARD') THEN
         IF (USERDT) THEN
            DTDZ = DTUSER(KST,IUCAT)
         ELSE IF (URBAN) THEN
            DTDZ = DTURB(KST)
         ELSE IF (RURAL) THEN
            DTDZ = DTRUR(KST)
         END IF
      END IF
      IF (DTDZ .GT. 0.0 .AND. TA .GT. 0.0) THEN
         S = G*DTDZ/TA
         RTOFS = SQRT(S)
      ELSE
         S = 1.0E-10
         RTOFS = 1.0E-10
      END IF

      IF (MSGHR) THEN
         IF (.NOT. MSGPRO) THEN
C           Set Flag for Runtime Met. Error to Prevent Further Calculations
            RUNERR = .TRUE.
C           WRITE Error Message:  Missing Meteorological Data
            WRITE(DUMMY,'(I8)') KURDAT
            CALL ERRHDL(PATH,MODNAM,'E','460',DUMMY)
         ELSE
C           WRITE Informational Message:  Missing Meteorological Data
            WRITE(DUMMY,'(I8)') KURDAT
            CALL ERRHDL(PATH,MODNAM,'I','460',DUMMY)
         END IF
      END IF

      GO TO 999

C     WRITE Error Message:  Error Reading Met Data File
 99   CALL ERRHDL(PATH,MODNAM,'E','510',' MET-INP ')
      RUNERR = .TRUE.
      GO TO 999

 1000 EOF = .TRUE.

 999  RETURN
      END

      SUBROUTINE METCHK
C***********************************************************************
C                 METCHK Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Performs Various Checks and Quality Assurance of
C                 One Hour of Meteorological Data
C
C        PROGRAMMER: JEFF WANG, ROGER BRODE
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Meteorological Variables for One Hour
C
C        OUTPUTS: Meteorological Data Error and Status Switches
C
C        CALLED FROM:   METEXT
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'METCHK'
      CLMHR  = .FALSE.
      MSGHR  = .FALSE.

C*----   ISCSTM Modification: allow for NOCHKD option - jah 11/2/94
      IF (.NOT. NOCHKD) THEN
C*       Check date for record out of sequence on the surface
C*       scaling file - NOCHKD=.TRUE. means no date check   ---   CALL CHKDAT
         CALL CHKDAT
      END IF
C*----
C*#
C     Check Data for Calm Winds                             ---   CALL CHKCLM
      CALL CHKCLM
C     Check Data for Missing Data Indicators                ---   CALL CHKMSG
      CALL CHKMSG
C     Check Data for Out-of-Range Values                    ---   CALL METQA
      CALL METQA

      RETURN
      END

      SUBROUTINE CHKDAT
C***********************************************************************
C                 CHKDAT Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Checks Meteorological Data for Record Out of Sequence
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Date Variable
C
C        OUTPUTS: Date Error Messages
C
C        CALLED FROM:   METCHK
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'CHKDAT'

C     Check for Record Out of Sequence
      IF (METFRM.NE.'UNFORM' .AND. IPDATE.NE.0) THEN
         IF (KURDAT .LE. IPDATE) THEN
C           WRITE Error Message - Record Out of Sequence
            WRITE(DUMMY,'(I8)') KURDAT
            CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
            RUNERR = .TRUE.
         ELSE IF (IHOUR.NE.1 .AND. (KURDAT-IPDATE).NE.1) THEN
C           WRITE Error Message - Record Out of Sequence
            WRITE(DUMMY,'(I8)') KURDAT
            CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
            RUNERR = .TRUE.
         ELSE IF (IHOUR.EQ.1 .AND. IPHOUR.NE.24) THEN
C           WRITE Error Message - Record Out of Sequence
            WRITE(DUMMY,'(I8)') KURDAT
            CALL ERRHDL(PATH,MODNAM,'E','450',DUMMY)
            RUNERR = .TRUE.
         END IF
      END IF

      RETURN
      END

      SUBROUTINE CHKCLM
C***********************************************************************
C                 CHKCLM Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Checks One Hour Meteorological Data for Calm Winds
C
C        PROGRAMMER: ROGER BRODE, JEFF WANG
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Meteorological Variables for One Hour
C
C        OUTPUTS: Calm Hour Flag, CLMHR, and Message
C
C        CALLED FROM:   METCHK
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'CHKCLM'

C     Check Data for Calm Winds (<= Threshold Value, UMIN)
C     The Threshold Value is Initially Set = 0.0
      IF (UREF .GE. 0.0 .AND. UREF .LE. UMIN) THEN
         CLMHR = .TRUE.
C        WRITE Informational Message: Calm Hour
         WRITE(DUMMY,'(I8)') KURDAT
         CALL ERRHDL(PATH,MODNAM,'I','440',DUMMY)
         IF (.NOT. CLMPRO) THEN
            UREF = 1.0
         END IF
      END IF

      RETURN
      END

      SUBROUTINE CHKMSG
C***********************************************************************
C                 CHKMSG Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Checks One Hour Meteorological Data for Missing Data
C
C        PROGRAMMER: JEFF WANG
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  To Change Temperature Range Check - 9/29/92
C
C        INPUTS:  Meteorological Variables for One Hour
C
C        OUTPUTS: Meteorological Data Error and Status Switches
C
C        CALLED FROM:   METCHK
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'CHKMSG'

C     Check Data for Missing Data Indicators
      IF (UREF .GE. 90.0 .OR. UREF .LE. -9.0) THEN
         MSGHR = .TRUE.
      ELSE IF (AFV .GT. 900. .OR. AFV .LE. -90.) THEN
         MSGHR = .TRUE.
      ELSE IF (KST .EQ. KSTMSG) THEN
         MSGHR = .TRUE.
      ELSE IF (TA .GT. 900. .OR. TA .LE. 0.) THEN
         MSGHR = .TRUE.
      ELSE IF (ZI .GT. 90000. .OR. ZI .LE. -90.) THEN
         MSGHR = .TRUE.
      END IF

      RETURN
      END

      SUBROUTINE METQA
C***********************************************************************
C                 METQA Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Performs Quality Assurance Checks for
C                 One Hour of Meteorological Data
C
C        PROGRAMMER: JEFF WANG, ROGER BRODE
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  To check for errors reading surface variables for
C                   new deposition algorithms.  R. Brode, PES, 12/6/94
C
C        MODIFIED:  To Change Temperature Range Check Lower Limit To
C                   230 K - 9/29/92
C
C        INPUTS:  Meteorological Variables for One Hour
C
C        OUTPUTS: Meteorological Data Error and Status Switches
C
C        CALLED FROM:   METCHK
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'METQA'

C     Check Data for Out-of-Range Values:

C     Wind Direction Check:
      IF (AFV.LT.0.0 .OR. AFV.GT.360.0) THEN
C        WRITE Warning Message: Invalid Flow Vector; and Set MSGHR Flag
         WRITE(DUMMY,'(I8)') KURDAT
         CALL ERRHDL(PATH,MODNAM,'W','410',DUMMY)
         MSGHR = .TRUE.
      END IF

C     Wind Speed Range Check
      IF (UREF .LT. 0.0) THEN
C        WRITE Warning Message: Invalid Wind Speed; and Set MSGHR Flag
         WRITE(DUMMY,'(I8)') KURDAT
         CALL ERRHDL(PATH,MODNAM,'W','420',DUMMY)
         MSGHR = .TRUE.
      END IF
      IF (UREF .GT. 30.0) THEN
C        WRITE Warning Message: Wind Speed Over 30m/s
         WRITE(DUMMY,'(I8)') KURDAT
         CALL ERRHDL(PATH,MODNAM,'W','420',DUMMY)
      END IF

C     Ambient Temperature Check
      IF (TA.LT.230.0 .OR. TA.GT.320.0) THEN
C        WRITE Warning Message: Ambient Temperature May be Out-of-Range
         WRITE(DUMMY,'(I8)') KURDAT
         CALL ERRHDL(PATH,MODNAM,'W','430',DUMMY)
      END IF

C     Check for missing (i.e., zero) USTAR, EL (Monin-Obukhov Length),
C     and Z0M (surface roughness length) when deposition algorithms are used.
      IF (LWPART .OR. LDPART .OR. LWGAS) THEN
         IF (USTAR.EQ.0.0 .AND. EL.EQ.0.0 .AND. Z0M.EQ.0.0) THEN
C           WRITE Error Message:  Error reading meteorology file for deposition
            CALL ERRHDL(PATH,MODNAM,'E','510','DEP-MET')
C           Set Z0M to 1.0E-5 to avoid divide-by-zero error
            Z0M = 1.0E-5
            RUNERR = .TRUE.
         ELSE IF (Z0M .LT. 1.0E-5) THEN
C           WRITE Warning Message:  Surface roughness length out-of-range
            WRITE(DUMMY,'(I8)') KURDAT
            CALL ERRHDL(PATH,MODNAM,'W','435',DUMMY)
C           Set to 1.0E-5 to avoid divide-by-zero error
            Z0M = 1.0E-5
         END IF
      END IF

      RETURN
      END

      SUBROUTINE METDAT
C***********************************************************************
C                 METDAT Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Print Out The Summary Of The Meteorology Data
C
C        PROGRAMMER: JEFF WANG
C        MODIFIED BY D. Strimaitis, SRC (for Wet & Dry DEPOSITION)
C
C        DATE:    November 8, 1993
C
C        MODIFIED BY D. Strimaitis, SRC (for Dry DEPOSITION)
C        (DATE:    February 15, 1993)
C
C        INPUTS:  Meteorology Input Data
C
C        OUTPUTS: Printed Model Outputs
C
C        CALLED FROM:   METEXT
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
C     Declare Arrays for Use With Unformatted Input Met Files
      INTEGER IKST(24)
      INTEGER IMNTH(24), INDY(24), INHR(24)
      REAL AUREF(24), ATA(24), AAFV(24), AAFVR(24), AZI(2,24)
      REAL APROF(24), ADTDZ(24)
C     Add LOCAL arrays for USTAR, EL, Z0M, IPCODE, PRATE
      REAL AUSTAR(24), AEL(24), AZ0M(24), APRATE(24)
      INTEGER IAPCODE(24)
      COMMON /UNFMET/ IKST, AUREF, ATA, AAFV, AAFVR, AZI

C     Variable Initializations
      MODNAM = 'METDAT'
      DO 10 IHR=1,24
         AUSTAR(IHR) = 0.
         AEL(IHR) = 0.
         AZ0M(IHR) = 0.
cjop     AZDM(IHR) = 0.
         IAPCODE(IHR) = 0
         APRATE(IHR) = 0.
10    CONTINUE

      IF (METFRM .EQ. 'UNFORM') THEN
         INUM = 24

      ELSE IF (METFRM .NE. 'CARD') THEN
C        READ In First 24 Hours of Data
         DO 100 I = 1, 24
            IMNTH(I) = IMONTH
            INDY(I)  = IDAY
            INHR(I)  = IHOUR
            AAFVR(I) = AFV
            AUREF(I) = UREF
            ATA(I)   = TA
            IKST(I)  = KST
            AZI(1,I) = ZIRUR
            AZI(2,I) = ZIURB
            IF (LWPART .OR. LWGAS) THEN
               AUSTAR(I) = USTAR
               AEL(I)    = EL
               AZ0M(I)   = Z0M
cjop           AZDM(I)   = ZDM
               APRATE(I) = PRATE
               IAPCODE(I)= IPCODE
            ELSE IF (LDPART) THEN
               AUSTAR(I) = USTAR
               AEL(I)    = EL
               AZ0M(I)   = Z0M
cjop           AZDM(I)   = ZDM
            ENDIF
            IF (METFRM .EQ. 'FREE') THEN
C              Read Hourly Records from ASCII File Using FREE Format
C              Yr, Mn, Dy, Hr, AFV, UREF, TA, KST, ZIRUR, ZIURB
C              (USTAR, EL, Z0M), (IPCODE, PRATE)
               IF (LWPART .OR. LWGAS) THEN
                  READ(MFUNIT,*,END=999,ERR=99,IOSTAT=IOERRN) IYEAR,
     &                 IMONTH,IDAY,IHOUR,AFV,UREF,TA,KST,ZIRUR,ZIURB,
     &                 USTAR,EL,Z0M,IPCODE,PRATE
               ELSE IF (LDPART) THEN
                  READ(MFUNIT,*,END=999,ERR=99,IOSTAT=IOERRN) IYEAR,
     &                 IMONTH,IDAY,IHOUR,AFV,UREF,TA,KST,ZIRUR,ZIURB,
     &                 USTAR,EL,Z0M
               ELSE
                  READ(MFUNIT,*,END=999,ERR=99,IOSTAT=IOERRN) IYEAR,
     &                 IMONTH,IDAY,IHOUR,AFV,UREF,TA,KST,ZIRUR,ZIURB
               ENDIF
            ELSE
C              Read Hourly Records from Formatted ASCII File Using METFRM
               IF (LWPART .OR. LWGAS) THEN
                  READ(MFUNIT,METFRM,END=999,ERR=99,IOSTAT=IOERRN)IYEAR,
     &                 IMONTH,IDAY,IHOUR,AFV,UREF,TA,KST,ZIRUR,ZIURB,
     &                 USTAR,EL,Z0M,IPCODE,PRATE
               ELSE IF (LDPART) THEN
                  READ(MFUNIT,METFRM,END=999,ERR=99,IOSTAT=IOERRN)IYEAR,
     &                 IMONTH,IDAY,IHOUR,AFV,UREF,TA,KST,ZIRUR,ZIURB,
     &                 USTAR,EL,Z0M
               ELSE
                  READ(MFUNIT,METFRM,END=999,ERR=99,IOSTAT=IOERRN)IYEAR,
     &                 IMONTH,IDAY,IHOUR,AFV,UREF,TA,KST,ZIRUR,ZIURB
               ENDIF
            END IF
 100     CONTINUE
C        Save Number of Records (Up to 24);  REWIND Met File, Skip First
C        Two Records, and Reset Variables to First Hour
 999     INUM = AMIN0(I,24)
         REWIND MFUNIT
         READ(MFUNIT,'(I2)') IDUM
         IF (METFRM .EQ. 'FREE') THEN
C           Read Hourly Records from ASCII File Using FREE Format
C           Yr, Mn, Dy, Hr, AFV, UREF, TA, KST, ZIRUR, ZIURB
C           (USTAR, EL, Z0M), (IPCODE, PRATE)
            IF (LWPART .OR. LWGAS) THEN
               READ(MFUNIT,*,END=999,ERR=99,IOSTAT=IOERRN) IYEAR,
     &              IMONTH,IDAY,IHOUR,AFV,UREF,TA,KST,ZIRUR,ZIURB,
     &              USTAR,EL,Z0M,IPCODE,PRATE
            ELSE IF (LDPART) THEN
               READ(MFUNIT,*,END=999,ERR=99,IOSTAT=IOERRN) IYEAR,
     &              IMONTH,IDAY,IHOUR,AFV,UREF,TA,KST,ZIRUR,ZIURB,
     &              USTAR,EL,Z0M
            ELSE
               READ(MFUNIT,*,END=999,ERR=99,IOSTAT=IOERRN) IYEAR,
     &             IMONTH,IDAY,IHOUR,AFV,UREF,TA,KST,ZIRUR,ZIURB
            ENDIF
         ELSE
C           Read Hourly Records from Formatted ASCII File Using METFRM
            IF (LWPART .OR. LWGAS) THEN
               READ(MFUNIT,METFRM,END=999,ERR=99,IOSTAT=IOERRN)IYEAR,
     &              IMONTH,IDAY,IHOUR,AFV,UREF,TA,KST,ZIRUR,ZIURB,
     &              USTAR,EL,Z0M,IPCODE,PRATE
            ELSE IF (LDPART) THEN
               READ(MFUNIT,METFRM,END=999,ERR=99,IOSTAT=IOERRN)IYEAR,
     &              IMONTH,IDAY,IHOUR,AFV,UREF,TA,KST,ZIRUR,ZIURB,
     &              USTAR,EL,Z0M
            ELSE
               READ(MFUNIT,METFRM,END=999,ERR=99,IOSTAT=IOERRN)IYEAR,
     &              IMONTH,IDAY,IHOUR,AFV,UREF,TA,KST,ZIRUR,ZIURB
            ENDIF
         END IF

      ELSE IF (METFRM .EQ. 'CARD') THEN
C        READ In First 24 Hours of Data
         DO 200 I = 1, 24
            IMNTH(I) = IMONTH
            INDY(I)  = IDAY
            INHR(I)  = IHOUR
            AAFVR(I) = AFV
            AUREF(I) = UREF
            ATA(I)   = TA
            IKST(I)  = KST
            AZI(1,I) = ZIRUR
            AZI(2,I) = ZIURB
            APROF(I) = P
            ADTDZ(I) = DTDZ
            IF (LWPART .OR. LWGAS) THEN
               AUSTAR(I) = USTAR
               AEL(I)    = EL
               AZ0M(I)   = Z0M
cjop           AZDM(I)   = ZDM
               APRATE(I) = PRATE
               IAPCODE(I)= IPCODE
            ELSE IF (LDPART) THEN
               AUSTAR(I) = USTAR
               AEL(I)    = EL
               AZ0M(I)   = Z0M
cjop           AZDM(I)   = ZDM
            ENDIF
C           Read Hourly Records from ASCII File Using CARD Format
C           Yr, Mn, Dy, Hr, AFV, UREF, TA, KST, ZIRUR, ZIURB, P, DTDZ
C           (USTAR, EL, Z0M), (IPCODE, PRATE)
cjop        FORMAT(4I2,2F9.4,F6.1,I2,2F7.1,2F8.4,F9.4,F10.1,F8.4,f5.1,
 9009       FORMAT(4I2,2F9.4,F6.1,I2,2F7.1,2F8.4,F9.4,F10.1,F8.4,
     &             I4,F7.2)
            IF (LWPART .OR. LWGAS) THEN
               READ(MFUNIT,9009,END=1000,ERR=99,IOSTAT=IOERRN) IYEAR,
     &              IMONTH,IDAY,IHOUR,AFV,UREF,TA,KST,ZIRUR,ZIURB,P,
     &              DTDZ,USTAR,EL,Z0M,IPCODE,PRATE
            ELSE IF (LDPART) THEN
               READ(MFUNIT,9009,END=1000,ERR=99,IOSTAT=IOERRN) IYEAR,
     &              IMONTH,IDAY,IHOUR,AFV,UREF,TA,KST,ZIRUR,ZIURB,P,
     &              DTDZ,USTAR,EL,Z0M
            ELSE
               READ(MFUNIT,9009,END=1000,ERR=99,IOSTAT=IOERRN) IYEAR,
     &              IMONTH,IDAY,IHOUR,AFV,UREF,TA,KST,ZIRUR,ZIURB,P,
     &              DTDZ
            ENDIF
 200     CONTINUE
 1000    INUM = AMIN0(I,24)
C        Save Number of Records (Up to 24);  REWIND Met File, Skip First
C        Two Records, and Reset Variables to First Hour
         REWIND MFUNIT
         READ(MFUNIT,'(I2)') IDUM
         IF (LWPART .OR. LWGAS) THEN
            READ(MFUNIT,9009,END=1000,ERR=99,IOSTAT=IOERRN) IYEAR,
     &           IMONTH,IDAY,IHOUR,AFV,UREF,TA,KST,ZIRUR,ZIURB,P,
     &           DTDZ,USTAR,EL,Z0M,IPCODE,PRATE
         ELSE IF (LDPART) THEN
            READ(MFUNIT,9009,END=1000,ERR=99,IOSTAT=IOERRN) IYEAR,
     &           IMONTH,IDAY,IHOUR,AFV,UREF,TA,KST,ZIRUR,ZIURB,P,
     &           DTDZ,USTAR,EL,Z0M
         ELSE
            READ(MFUNIT,9009,END=1000,ERR=99,IOSTAT=IOERRN) IYEAR,
     &           IMONTH,IDAY,IHOUR,AFV,UREF,TA,KST,ZIRUR,ZIURB,P,
     &           DTDZ
         ENDIF

      END IF

C     WRITE Out Header Information
      CALL HEADER
      WRITE(IOUNIT,9011) INUM
      WRITE(IOUNIT,9016) METINP, METFRM
      WRITE(IOUNIT,9020) IDSURF, IDUAIR, SFNAME, UANAME,
     &                   ISYEAR, IUYEAR
      IF (METFRM .NE. 'CARD') THEN
         WRITE(IOUNIT,9025)
      ELSE
         WRITE(IOUNIT,9026)
      END IF

C     WRITE Out First INUM Records (Up to 24)
      DO 300 I = 1, INUM
         IF (METFRM .EQ. 'UNFORM') THEN
            WRITE(IOUNIT,9032) IYEAR, IMONTH, IDAY, I, AAFVR(I),
     &            AUREF(I), ATA(I), IKST(I), AZI(1,I), AZI(2,I),
     &            AUSTAR(I), AEL(I), AZ0M(I), IAPCODE(I),
     &            APRATE(I)
         ELSE IF (METFRM .NE. 'CARD') THEN
            WRITE(IOUNIT,9032) IYEAR, IMNTH(I), INDY(I), INHR(I),
     &            AAFVR(I),AUREF(I),ATA(I), IKST(I), AZI(1,I), AZI(2,I),
     &            AUSTAR(I), AEL(I), AZ0M(I), IAPCODE(I),
     &            APRATE(I)
         ELSE
            WRITE(IOUNIT,9033) IYEAR, IMNTH(I), INDY(I), INHR(I),
     &            AAFVR(I),AUREF(I),ATA(I), IKST(I), AZI(1,I), AZI(2,I),
     &            APROF(I), ADTDZ(I), AUSTAR(I), AEL(I), AZ0M(I),
     &            IAPCODE(I), APRATE(I)
         END IF
 300  CONTINUE

C     Write Out Explanatory Message for Stability Class
      WRITE(IOUNIT,9050)

      GO TO 9999

 9011 FORMAT(/22X,'*** THE FIRST ',I3,' HOURS OF ',
     &       'METEOROLOGICAL DATA ***'/)
 9016 FORMAT(12X,'FILE: ',A40,' FORMAT: ',A60)
 9020 FORMAT(12X,'SURFACE STATION NO.: ',I6,20X,
     &       'UPPER AIR STATION NO.: ',I6/27X,'NAME: ',A40,3X,
     &       'NAME: ',A40/27X,'YEAR: ',I6,37X,'YEAR: ',I6)
 9025 FORMAT(/38X,'FLOW',4X,'SPEED',3X,'TEMP',5X,'STAB',4X,
     &       'MIXING HEIGHT (M)',4X,'USTAR',2X,'M-O LENGTH',3X,'Z-0',
     &       1X,'IPCODE',1X,'PRATE',
     &       /11X,'YEAR',2X,'MONTH',2X,'DAY',2X,'HOUR',4X,'VECTOR',
     &       3X,'(M/S)',4X,'(K)',5X,'CLASS',4X,'RURAL',4X,'URBAN',
     &       6X,'(M/S)',5X,'(M)',7X,'(M)',7X, '(mm/HR)',
     &       /75('- ')/)
 9026 FORMAT(/38X,'FLOW',4X,'SPEED',3X,'TEMP',5X,'STAB',4X,
     &       'MIXING HEIGHT (M)',4X,'WIND',4X,'VERT TEMP',
     &       5X,'USTAR',2X,'M-O LENGTH',3X,'Z-0',
     &       3X,'IPCODE',1X,'PRATE',
     &       /11X,'YEAR',2X,'MONTH',2X,'DAY',2X,'HOUR',4X,'VECTOR',
     &       3X,'(M/S)',4X,'(K)',5X,'CLASS',4X,'RURAL',4X,'URBAN',
     &       6X,'PROF',4X,'GRAD (K/M)',4X,'(M/S)',5X,'(M)',7X,'(M)',
     &           7X, '(mm/HR)',
     &       /75('- ')/)
 9032 FORMAT(12X,4(I2,4X),1X,F6.1,2X,F6.2,2X,F6.1,6X,I1,5X,2(F7.1,2X),
     &       F9.4,F10.1,F8.4,I4,F7.2)
 9033 FORMAT(12X,4(I2,4X),1X,F6.1,2X,F6.2,2X,F6.1,6X,I1,5X,2(F7.1,2X),
     &       2(F8.4,2X),F9.4,F10.1,F8.4,I4,F7.2)

cjop  FORMAT(12X,4(I2,4X),1X,F6.1,2X,F6.2,2X,F6.1,6X,I1,5X,2(F7.1,2X),
cjop &       F9.4,F10.1,F8.4,F5.1,I4,F7.2)
cjop  FORMAT(12X,4(I2,4X),1X,F6.1,2X,F6.2,2X,F6.1,6X,I1,5X,2(F7.1,2X),
cjop &       2(F8.4,2X),F9.4,F10.1,F8.4,F5.1,I4,F7.2)

 9050 FORMAT(///' *** NOTES:  STABILITY CLASS 1=A, 2=B, 3=C, 4=D, 5=E',
     &       ' AND 6=F.',
     &         /'             FLOW VECTOR IS DIRECTION TOWARD WHICH ',
     &       'WIND IS BLOWING.')

C     WRITE Error Message:  Error Reading Met Data Input File
 99   CALL ERRHDL(PATH,MODNAM,'E','510',' MET-INP ')
      RUNERR = .TRUE.

 9999 RETURN
      END
      SUBROUTINE OUCARD
C***********************************************************************
C                 OUCARD Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: To process OUtput Pathway card images
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To add subroutine call for TOXXFILE option - 9/29/92
C
C        INPUTS:  Pathway (OU) and Keyword
C
C        OUTPUTS: Output Option Switches
C                 Output Setup Status Switches
C
C        CALLED FROM:   SETUP
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'OUCARD'

      IF (KEYWRD .EQ. 'STARTING') THEN
C        Set Status Switch
         IOSTAT(1) = IOSTAT(1) + 1
      ELSE IF (KEYWRD .EQ. 'RECTABLE') THEN
C        Process High Value Output Option                   ---   CALL OUHIGH
         CALL OUHIGH
C        Set Status Switch
         IOSTAT(2) = IOSTAT(2) + 1
      ELSE IF (KEYWRD .EQ. 'MAXTABLE') THEN
C        Process Maximum 50 Table Option                    ---   CALL OUMXVL
         CALL OUMXVL
C        Set Status Switch
         IOSTAT(3) = IOSTAT(3) + 1
      ELSE IF (KEYWRD .EQ. 'DAYTABLE') THEN
C        Process Daily Value Table Option                   ---   CALL OUDALY
         CALL OUDALY
C        Set Status Switch
         IOSTAT(4) = IOSTAT(4) + 1
      ELSE IF (KEYWRD .EQ. 'MAXIFILE') THEN
C        Process Maximum Value (Threshold) File Option      ---   CALL OUMXFL
         CALL OUMXFL
C        Set Status Switch
         IOSTAT(5) = IOSTAT(5) + 1
      ELSE IF (KEYWRD .EQ. 'POSTFILE') THEN
C        Process Postprocessing File Output Option          ---   CALL OUPOST
         CALL OUPOST
C        Set Status Switch
         IOSTAT(6) = IOSTAT(6) + 1
      ELSE IF (KEYWRD .EQ. 'PLOTFILE') THEN
C        Process Plotting File Output Option                ---   CALL OUPLOT
         CALL OUPLOT
C        Set Status Switch
         IOSTAT(7) = IOSTAT(7) + 1
      ELSE IF (KEYWRD .EQ. 'TOXXFILE') THEN
C        Process TOXXFILE Output Option                     ---   CALL OUTOXX
         CALL OUTOXX
C        Set Status Switch
         IOSTAT(8) = IOSTAT(8) + 1
      ELSE IF (KEYWRD .EQ. 'FINISHED') THEN
C        Set Status Switch
         IOSTAT(20) = IOSTAT(20) + 1
C        Check The Consistency of The Output Options
         CALL OUTQA
      ELSE
C        Write Error Message: Invalid Keyword for This Pathway
         CALL ERRHDL(PATH,MODNAM,'E','110',KEYWRD)
      END IF

      RETURN
      END

      SUBROUTINE OUTQA
C***********************************************************************
C                 OUTQA Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: To process OUtput Pathway card images QA Check
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  To Include TOXXFILE Option - 9/29/92
C
C        INPUTS:  Pathway (OU) and Keyword
C
C        OUTPUTS: Output Messages
C
C        CALLED FROM: OUCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      LOGICAL OUTOPT
      CHARACTER KEYMSG*8, MSG1*3

C     Variable Initializations
      MODNAM = 'OUTQA'
      MSG1   = '-HR'
      OUTOPT = .FALSE.

C     Check If Missing Mandatory Keyword
      IF (IOSTAT(1) .EQ. 0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','130','STARTING')
      END IF

C     Check For Lack of Any Output Option Cards
      DO 5 I = 2, 8
         IF (IOSTAT(I) .GT. 0) THEN
            OUTOPT = .TRUE.
         END IF
 5    CONTINUE
      IF (.NOT.OUTOPT .AND. .NOT.PERIOD .AND. .NOT.ANNUAL) THEN
C        WRITE Error Message - No Output Keywords and No PERIOD Averages
         CALL ERRHDL(PATH,MODNAM,'E','190','  ')
      END IF

      DO 10 IAVE = 1, NUMAVE
         IDCST1 = 0
         DO 20 IVAL = 1, NVAL
            IF (NHIAVE(IVAL,IAVE) .EQ. 1) THEN
               IDCST1 = 1
            END IF
 20      CONTINUE
         IF (IDCST1.EQ.0 .AND. MAXAVE(IAVE).EQ.0 .AND.
     &                         IDYTAB(IAVE).EQ.0) THEN
            WRITE(KEYMSG,'(I2.2,A3)') KAVE(IAVE), MSG1
            CALL ERRHDL(PATH,MODNAM,'W','540',KEYMSG)
         END IF
 10   CONTINUE

C     Check for DAYTABLE Option With SAVEFILE or INITFILE Options
      IF (DAYTAB .AND. (RSTSAV .OR. RSTINP)) THEN
C        WRITE Warning Message: DAYTABLE Results Overwritten on Re-start
         CALL ERRHDL(PATH,MODNAM,'W','195','DAYTABLE')
      END IF
C     Check for TOXXFILE Option With SAVEFILE or INITFILE Options
      IF (TXFILE .AND. (RSTSAV .OR. RSTINP)) THEN
C        WRITE Error Message: Incompatible Options
         CALL ERRHDL(PATH,MODNAM,'E','195','TOXXFILE')
      END IF

      RETURN
      END

      SUBROUTINE OUHIGH
C***********************************************************************
C                 OUHIGH Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: To process High Value By Receptor Table
C                 Output Selections
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Parameters
C
C        OUTPUTS: Output Option Switches
C
C        CALLED FROM:   OUCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      CHARACTER LPRD*8, HPRD*8, NCHR1(10)*8, NCHR2(10)*4
      LOGICAL FOUND, RMARK
      DIMENSION HIGHST(NVAL), ILOCH(NAVE)

C     Variable Initializations
      DATA (NCHR1(I),I=1,10) /'FIRST','SECOND','THIRD','FOURTH',
     &                        'FIFTH','SIXTH','SEVENTH','EIGHTH',
     &                        'NINTH','TENTH'/
      DATA (NCHR2(I),I=1,10) /'1ST','2ND','3RD','4TH','5TH','6TH',
     &                        '7TH','8TH','9TH','10TH'/
      MODNAM = 'OUHIGH'
      FOUND  = .FALSE.

      DO 145 I = 1, NVAL
         HIGHST(I) = 0
 145  CONTINUE

      DO 150 I = 1, NAVE
         ILOCH(I) = 0
 150  CONTINUE

C     Check If Enough Fields
      IF (IFC .EQ. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .EQ. 3) THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 13) THEN
C        Error Message: Too Many Fields
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Retrieve Averaging Period
      IF (FIELD(3) .EQ. 'ALLAVE') THEN
C        Go For All Averaging Periods
         DO 40 I = 1, NUMAVE
            INHI(I) = 1
            ILOCH(I) = 1
 40      CONTINUE
         FOUND = .TRUE.
      ELSE IF (FIELD(3) .EQ. 'MONTH' .AND. MONTH) THEN
C        Set Value of IPRDT = 720 for MONTHly Averages
         IPRDT = 720
C        Search The Period to find out the Location
         DO 35 I = 1, NUMAVE
            IF (IPRDT .EQ. KAVE(I)) THEN
               FOUND = .TRUE.
               INHI(I) = 1
               ILOCH(I) = 1
            END IF
 35      CONTINUE
      ELSE
         CALL FSPLIT(PATH,KEYWRD,FIELD(3),40,'-',RMARK,LPRD,HPRD)
C        Single Time Period
         IF (HPRD .EQ. LPRD) THEN
            CALL STONUM(HPRD,8,FNUM,IMIT)
            IF (IMIT .NE. 1) THEN
C              Write Error Message:Invalid Numerical Field
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 115
            END IF
            IPRDT1 = INT(FNUM)
C           Search The Period to find out the Location
            DO 45 I = 1, NUMAVE
               IF (IPRDT1 .EQ. KAVE(I)) THEN
                  FOUND = .TRUE.
                  INHI(I) = 1
                  ILOCH(I) = 1
               END IF
 45         CONTINUE
         ELSE
C           Find The Lower Boundary
            CALL STONUM(LPRD,8,FNUM,IMIT)
            IF (IMIT .NE. 1) THEN
C              Write Error Message:Invalid Numerical Field
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 114
            END IF
            IPRDT1 = INT(FNUM)
C           Find The Upper Boundary
 114        CALL STONUM(HPRD,8,FNUM,IMIT)
            IF (IMIT .NE. 1) THEN
C              Write Error Message:Invalid Numerical Field
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 115
            END IF
            IPRDT2 = INT(FNUM)
C           Search The Period to find out the Location
            DO 50 I = 1, NUMAVE
               IF (KAVE(I).GE.IPRDT1 .AND.
     &             KAVE(I).LE.IPRDT2) THEN
                  FOUND = .TRUE.
                  INHI(I) = 1
                  ILOCH(I) = 1
               END IF
 50         CONTINUE
C           Multi Time Period
         END IF
      END IF

 115  CONTINUE

C     Check Averaging Period Against KAVE Array,
      IF (.NOT. FOUND) THEN
C        Error Message:E203 AVEPER Not Match With Pre-Defined One
         CALL ERRHDL(PATH,MODNAM,'E','203','AVEPER')
         GO TO 999
      END IF

C     Check for Previous NHIGHEST Card for This Averaging Period

C     Begin LOOP Through Fields
      DO 20 I = 4, IFC
C        Retrieve The High Value
         CALL FSPLIT(PATH,KEYWRD,FIELD(I),40,'-',RMARK,LPRD,HPRD)
C        Fit To The Status Array
         ISPRD = 0
         IEPRD = 0
         DO 30 J = 1, 10
            IF (LPRD.EQ.NCHR1(J) .OR.
     &          LPRD.EQ.NCHR2(J)) ISPRD = J
            IF (HPRD.EQ.NCHR1(J) .OR.
     &          HPRD.EQ.NCHR2(J)) IEPRD = J
 30      CONTINUE
         IF (ISPRD.EQ.0 .OR. IEPRD.EQ.0) THEN
C           Write Error Message:Illegal Parameter Field
            CALL ERRHDL(PATH,MODNAM,'E','203','HIVALU')
            GO TO 20
         END IF
         IF (ISPRD.GT.NVAL .OR. IEPRD.GT.NVAL) THEN
C           Write Error Message: High Value Requested Exceeds NVAL
            WRITE(DUMMY,'(I8)') NVAL
            CALL ERRHDL(PATH,MODNAM,'E','270',DUMMY)
            GO TO 20
         END IF
         DO 41 J = ISPRD,IEPRD
            HIGHST(J) = 1
 41      CONTINUE
C     End LOOP Through Fields
 20   CONTINUE

C     Set Array Switch to Indicate Which High Values to Report
C     And Set the Maximum Number of High Values, NHIVAL
      DO 125 I = 1, NUMAVE
         DO 120 J = 1, NVAL
            IF (HIGHST(J).EQ.1 .AND. ILOCH(I).EQ.1) THEN
               NHIAVE(J,I) = 1
               IF (J .GT. NHIVAL) THEN
                  NHIVAL = J
               END IF
            END IF
 120     CONTINUE
 125  CONTINUE

 999  RETURN
      END

      SUBROUTINE OUMXVL
C***********************************************************************
C                 OUMXVL Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: To Process Maximum (Overall) Value Table
C                 Output Selections
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Parameters
C
C        OUTPUTS: Output Option Switches
C
C        CALLED FROM:   OUCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      LOGICAL FOUND

C     Variable Initializations
      MODNAM = 'OUMXVL'
      FOUND = .FALSE.

C     Check for Appropriate Number of Fields
      IF (IFC .EQ. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .EQ. 3) THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 4) THEN
C        Error Message: Too Many Fields
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Retrieve Averaging Period
      IF (FIELD(3) .EQ. 'ALLAVE') THEN
C        Go For All Averaging Periods
         DO 40 I = 1, NUMAVE
            MAXAVE(I) = 1
 40      CONTINUE
         FOUND = .TRUE.
      ELSE
         IF (FIELD(3) .EQ. 'MONTH' .AND. MONTH) THEN
C           Set Value of IPRDT = 720 for MONTHly Averages
            IPRDT = 720
         ELSE
            CALL STONUM(FIELD(3),40,FNUM,IMIT)
            IF (IMIT .NE. 1) THEN
C              Write Error Message:Invalid Numerical Field
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 999
            END IF
            IPRDT = INT(FNUM)
         END IF
C        Check Averaging Period Against KAVE Array
         J = 1
         DO WHILE (.NOT.FOUND .AND. J.LE.NUMAVE)
            IF (IPRDT .EQ. KAVE(J)) THEN
               FOUND = .TRUE.
               INDAVE = J
               MAXAVE(J) = 1
            END IF
            J = J + 1
         END DO
      END IF
      IF (.NOT. FOUND) THEN
C        Error Message: E203 AVEPER Not Match With Pre-Defined One
         CALL ERRHDL(PATH,MODNAM,'E','203','AVEPER')
         GO TO 999
      END IF

C     Set Number of Maximum Values to Sort
      CALL STONUM(FIELD(4),40,FNUM,IMIT)
      IF (IMIT .NE. 1) THEN
C        Write Error Message:Invalid Numerical Field
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF
      INUM = INT(FNUM)
      IF (INUM .GT. NMAX) THEN
C        WRITE Error Message:  Maximum Value Selected Exceeds NMAX
         WRITE(DUMMY,'(I8)') NMAX
         CALL ERRHDL(PATH,MODNAM,'E','280',DUMMY)
         GO TO 999
      END IF

      IF (FIELD(3) .EQ. 'ALLAVE') THEN
C        Go For All Averaging Periods
         DO 50 I = 1, NUMAVE
            IMXVAL(I) = INUM
 50      CONTINUE
      ELSE
         IMXVAL(INDAVE) = INUM
      END IF

      IF (INUM .GT. NMXVAL) THEN
         NMXVAL = INUM
      END IF

 999  RETURN
      END

      SUBROUTINE OUDALY
C***********************************************************************
C                 OUDALY Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: To Process Daily Concurrent Value Table
C                 Output Selections
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Parameters
C
C        OUTPUTS: Output Option Switches
C
C        CALLED FROM:   OUCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      LOGICAL FOUND

C     Variable Initializations
      MODNAM = 'OUDALY'

C     Check for Appropriate Number of Fields
      IF (IFC .EQ. 2) THEN
C        Error Message: No AvePer And High Value
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. NUMAVE+2) THEN
C        Error Message: Too Many Fields
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Retrieve Averaging Period(s)
      IF (FIELD(3) .EQ. 'ALLAVE') THEN
C        Go For All Averaging Periods
         DO 40 I = 1, NUMAVE
            IDYTAB(I) = 1
 40      CONTINUE
C        Set Logical Switch Indicating That Daily Value Tables Are Generated
         DAYTAB = .TRUE.
      ELSE
         DO 100 I = 3, IFC
            IF (FIELD(I) .EQ. 'MONTH' .AND. MONTH) THEN
C              Set Value of IPRDT = 720 for MONTHly Averages
               IPRDT = 720
            ELSE
               FOUND = .FALSE.
               CALL STONUM(FIELD(I),40,FNUM,IMIT)
               IF (IMIT .NE. 1) THEN
C                 Write Error Message:Invalid Numerical Field
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  GO TO 999
               END IF
               IPRDT = INT(FNUM)
            END IF
C           Check Averaging Period Against KAVE Array
            J = 1
            DO WHILE (.NOT.FOUND .AND. J.LE.NUMAVE)
               IF (IPRDT .EQ. KAVE(J)) THEN
                  FOUND = .TRUE.
                  IDYTAB(J) = 1
               END IF
               J = J + 1
            END DO
            IF (.NOT. FOUND) THEN
C              Error Message:E203 KAVE Not Match With Pre-Defined One
               CALL ERRHDL(PATH,MODNAM,'E','203','AVEPER')
               GO TO 999
            END IF
 100     CONTINUE
C        Set Logical Switch Indicating That Daily Value Tables Are Generated
         DAYTAB = .TRUE.
      END IF

 999  RETURN
      END

      SUBROUTINE OUMXFL
C***********************************************************************
C                 OUMXFL Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: To Process Threshold Value Output Selections
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  To Change File Length Limit To 40 - 9/29/92
C        MODIFIED BY D. Strimaitis, SRC (for Wet & Dry DEPOSITION) - 11/8/93
C        MODIFIED:  To skip writing of header records if FATAL error
C                   has been encountered.  R.W. Brode, PES, Inc. - 6/20/95
C
C        INPUTS:  Input Runstream Parameters
C
C        OUTPUTS: Output Option Switches
C
C        CALLED FROM:   OUCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      CHARACTER INPGRP*8, BUF80*80
      LOGICAL FOUND

C     Variable Initializations
      MODNAM = 'OUMXFL'

C     Check If Enough Field
      IF (IFC .EQ. 2) THEN
C        Error Message: No Fields
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 6) THEN
C        Error Message: Not Enough Fields
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 7) THEN
C        Error Message: Too Many Fields
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Retrieve Averaging Period
      IF (FIELD(3) .EQ. 'MONTH' .AND. MONTH) THEN
C        Set Value of IPRDT = 720 for MONTHly Averages
         IPRDT = 720
      ELSE
         CALL STONUM(FIELD(3),40,FNUM,IMIT)
         IF (IMIT .NE. 1) THEN
C           Write Error Message:Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
         IPRDT = INT(FNUM)
      END IF

C     Check Averaging Period Against KAVE Array
      FOUND = .FALSE.
      J = 1
      DO WHILE (.NOT.FOUND .AND. J.LE.NUMAVE)
         IF (IPRDT .EQ. KAVE(J)) THEN
            FOUND = .TRUE.
            INDAVE = J
         END IF
         J = J + 1
      END DO
      IF (.NOT. FOUND) THEN
C        Error Message:E203 AVEPER Not Match With Pre-Defined One
         CALL ERRHDL(PATH,MODNAM,'E','203','AVEPER')
         GO TO 999
      END IF

C     Retrieve Source Group ID
      INPGRP = FIELD(4)
C     Check Source Group ID
      FOUND = .FALSE.
      J = 1
      DO WHILE (.NOT.FOUND .AND. J.LE.NUMGRP)
         IF (INPGRP .EQ. GRPID(J)) THEN
            FOUND = .TRUE.
            INDGRP = J
         END IF
         J = J + 1
      END DO
      IF (.NOT. FOUND) THEN
C        Error Message:E203 GRPID Not Match With Pre-Defined One
         CALL ERRHDL(PATH,MODNAM,'E','203','GRPID')
         GO TO 999
      END IF

C     Set Switch and Check for Previous MAXIFILE Card
C     for This Averaging Period & Group ID
      MAXFLE(INDGRP,INDAVE) = MAXFLE(INDGRP,INDAVE) + 1
      IF (MAXFLE(INDGRP,INDAVE) .GT. 1) THEN
C        WRITE Error Message
         CALL ERRHDL(PATH,MODNAM,'E','211',KEYWRD)
         GO TO 999
      END IF

C     Retrieve Threshold
      CALL STONUM(FIELD(5),40,FNUM,IMIT)
C     Check for Valid Threshold Value
      IF (IMIT .NE. 1) THEN
C        Write Error Message:Invalid Numerical Field
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF
      THRESH(INDGRP,INDAVE) = FNUM

      IF ((LOCE(6)-LOCB(6)) .LE. 39) THEN
C        Retrieve Filename as Character Substring to Maintain Original Case
C        Also Check for Filename Larger Than 40 Characters
         THRFIL(INDGRP,INDAVE) = RUNST1(LOCB(6):LOCE(6))
      ELSE
C        WRITE Error Message:  THRFIL Field is Too Long
         CALL ERRHDL(PATH,MODNAM,'E','203',' FILNAM ')
      END IF

C     Retrieve File Unit If Input, or Assign File Unit and OPEN File
      IF (IFC .EQ. 7) THEN
         CALL STONUM(FIELD(7),40,FNUM,IMIT)
C        Check for Valid Threshold Value
         IF (IMIT .NE. 1) THEN
C           Write Error Message:Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
C        Check for Conflict With System Files
         IF (FNUM .LT. 20.) THEN
C           WRITE Error Message:  Invalid File Unit Specified
            CALL ERRHDL(PATH,MODNAM,'E','560',KEYWRD)
            GO TO 999
         ELSE IF (FNUM .GT. 100) THEN
C           WRITE Warning Message:  Suspect File Unit Specified
C           Unit May Conflict With Dynamically Allocated File Units
            CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
            IMXUNT(INDGRP,INDAVE) = INT(FNUM)
         ELSE
            IMXUNT(INDGRP,INDAVE) = INT(FNUM)
         END IF
      ELSE
C        Dynamically Allocate File Unit (100's)
         IMXUNT(INDGRP,INDAVE) = 100 + INDGRP*10 + INDAVE
         IF (INDGRP .GE. 10 .OR. INDAVE .GE. 10) THEN
C           WRITE Warning Message: Dynamic FUnit Allocation May Have Conflict
            CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
         END IF
      END IF

C     Check for Earlier Use of This Filename and File Unit
      FOUND = .FALSE.
      DO 200 J = 1, NUMAVE
         DO 100 I = 1, NUMGRP
            IF (I .NE. INDGRP .OR. J .NE. INDAVE) THEN
               IF (THRFIL(INDGRP,INDAVE) .EQ. THRFIL(I,J) .AND.
     &             IMXUNT(INDGRP,INDAVE) .EQ. IMXUNT(I,J)) THEN
                  FOUND = .TRUE.
               ELSE IF (THRFIL(INDGRP,INDAVE) .EQ. THRFIL(I,J) .AND.
     &                  IMXUNT(INDGRP,INDAVE) .NE. IMXUNT(I,J)) THEN
C                 Write Error Message: Conflicting Inputs
                  CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
                  GO TO 999
               ELSE IF (THRFIL(INDGRP,INDAVE) .NE. THRFIL(I,J) .AND.
     &                  IMXUNT(INDGRP,INDAVE) .EQ. IMXUNT(I,J)) THEN
C                 Write Error Message: Conflicting Inputs
                  CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
                  GO TO 999
               END IF
            END IF
 100     CONTINUE
 200  CONTINUE

      IF (.NOT. FOUND) THEN
C        First Time File is Identified - OPEN File
         OPEN(IMXUNT(INDGRP,INDAVE),ERR=99,FILE=THRFIL(INDGRP,INDAVE),
     &        IOSTAT=IOERRN,STATUS='UNKNOWN')
         IF (RSTINP) THEN
C           Results Arrays Are To Be Initialized From Re-start File.
C           Read Start Date From File and Rewind.
            DUMMY = 'INITFILE'
            READ(IRSUNT,ERR=919,END=919) ISDATE
            REWIND IRSUNT
C           Now Position MAXIFILE To End of File, But Not Past ISDATE.
            DUMMY = 'MAXIFILE'
            EOF = .FALSE.
            DO WHILE (.NOT. EOF)
               READ(IMXUNT(INDGRP,INDAVE),'(A80)',ERR=919,END=199) BUF80
               IF (BUF80(1:1) .NE. '*') THEN
C                 Record Is Not Part of Header - Read Date For This Record
                  READ(BUF80(15:22),'(I8)',ERR=919) IDAT
               END IF
               IF (IDAT .GT. ISDATE) THEN
C                 Date of MAXIFILE Event Is Greater Than Start Date
C                 From Save File.  Treat As End of File to Exit Loop.
                  GO TO 199
               END IF
               GO TO 11
 199           EOF = .TRUE.
 11            CONTINUE
            END DO
            EOF = .FALSE.
            BACKSPACE IMXUNT(INDGRP,INDAVE)
C           Skip Header Records
            GO TO 999
         END IF
      ELSE IF (FOUND .AND. RSTINP) THEN
C        This Run is a Re-start From an Earlier Run
C        Skip Header Records
         GO TO 999
      END IF

C     Write Header to File.  Skip Header if FATAL.
      IF (RUN .AND. .NOT.FATAL) THEN
         WRITE(IMXUNT(INDGRP,INDAVE),9005) VERSN, TITLE1
 9005    FORMAT('* ISC3P  (',A5,'): ',A68)
         WRITE(IMXUNT(INDGRP,INDAVE),9007) (MODOPS(I),I=1,17)
 9007    FORMAT('* MODELING OPTIONS USED:',/'* ',17(1X,A6))
         WRITE(IMXUNT(INDGRP,INDAVE),9010) CHRAVE(INDAVE),
     &               THRESH(INDGRP,INDAVE), GRPID(INDGRP), THRFRM
 9010    FORMAT('*',9X,'MAXI-FILE FOR ',A5,' VALUES ',
     &          '>= A THRESHOLD OF ',G12.4,
     &         /'*',9X,'FOR SOURCE GROUP: ',A8,
     &         /'*',9X,'FORMAT: ',A60)
         WRITE(IMXUNT(INDGRP,INDAVE),9020) CHIDEP(1,1), CHIDEP(2,1),
     &                                     CHIDEP(3,1)
 9020    FORMAT('*AVE',3X,'GRP',5X,'DATE',11X,'X',13X,'Y',8X,'ELEV',
     &          4X,'FLAG',3X,3A4,
     &          /'*___',2(1X,'________'),2(2X,'____________'),
     &          2(2X,'______'),'  ____________')
      END IF

      GO TO 999

C     WRITE Error Message for Error Opening File
 99   WRITE(DUMMY,'(5HMAXFL,I3.3)') IMXUNT(INDGRP,INDAVE)
      CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

      GO TO 999

C     WRITE Error Message for Error Reading File
 919  CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)

C     Set Logical Switch Indicating That Maximum Value File(s) Are Generated
 999  MXFILE = .TRUE.

      RETURN
      END

      SUBROUTINE OUPOST
C***********************************************************************
C                 OUPOST Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: To Process Post-processor File Output Selections
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  To Change File Length Limit To 40 - 9/29/92
C        MODIFIED BY D. Strimaitis, SRC (for Wet & Dry DEPOSITION) - 11/8/93
C        MODIFIED:  To skip writing of header records if FATAL error
C                   has been encountered.  R.W. Brode, PES, Inc. - 6/20/95
C        MODIFIED:  To change buffer length for PLOT format to 132, and
C                   change read statement to allow for multiple output
C                   types (CONC/DEPOS/etc.).   R.W. Brode, PES, Inc., 6/20/95
C
C        INPUTS:  Input Runstream Parameters
C
C        OUTPUTS: Output Option Switches
C
C        CALLED FROM:   OUCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      CHARACTER INPGRP*8, BUF132*132, HDRFRM*256
      LOGICAL FOUND

C     Variable Initializations
      MODNAM = 'OUPOST'

C     Create Header Format for Columns
      WRITE(HDRFRM,9020) NUMTYP, NUMTYP+2

C     Check If Enough Fields
      IF (IFC .EQ. 2) THEN
C        Error Message: No Fields
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 6) THEN
C        Error Message: Not Enough Fields
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 7) THEN
C        Error Message: Too Many Fields
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Retrieve Averaging Period
      IF (FIELD(3) .EQ. 'PERIOD' .AND. PERIOD) THEN
C        Post File is for PERIOD Averages                   ---   CALL PERPST
         CALL PERPST
C        Exit to End
         GO TO 999
      ELSE IF (FIELD(3) .EQ. 'ANNUAL' .AND. ANNUAL) THEN
C        Post File is for PERIOD Averages                   ---   CALL PERPST
         CALL PERPST
C        Exit to End
         GO TO 999
      ELSE IF (FIELD(3).EQ.'PERIOD' .OR. FIELD(3).EQ.'ANNUAL') THEN
C        Period Post File Selected But No PERIOD Averages Calculated
C        WRITE Error Message: Invalid Averaging Period Selected for POSTFILE
         CALL ERRHDL(PATH,MODNAM,'E','203',' AVEPER ')
         GO TO 999
      ELSE IF (FIELD(3) .EQ. 'MONTH' .AND. MONTH) THEN
C        Set Value of IPRDT = 720 for MONTHly Averages
         IPRDT = 720
      ELSE
         CALL STONUM(FIELD(3),40,FNUM,IMIT)
         IF (IMIT .NE. 1) THEN
C           Write Error Message:Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
         IPRDT = INT(FNUM)
      END IF

C     Check Averaging Period Against KAVE Array
      FOUND = .FALSE.
      J = 1
      DO WHILE (.NOT.FOUND .AND. J.LE.NUMAVE)
         IF (IPRDT .EQ. KAVE(J)) THEN
            FOUND = .TRUE.
            INDAVE = J
         END IF
         J = J + 1
      END DO
      IF (.NOT. FOUND) THEN
C        Error Message:E203 AVEPER Not Match With Pre-Defined One
         CALL ERRHDL(PATH,MODNAM,'E','203','AVEPER')
         GO TO 999
      END IF

C     Retrieve Source Group ID
      INPGRP = FIELD(4)
C     Check Source Group ID
      FOUND = .FALSE.
      J = 1
      DO WHILE (.NOT.FOUND .AND. J.LE.NUMGRP)
         IF (INPGRP .EQ. GRPID(J)) THEN
            FOUND = .TRUE.
            INDGRP = J
         END IF
         J = J + 1
      END DO
      IF (.NOT. FOUND) THEN
C        Error Message:E203 GRPID Not Match With Pre-Defined One
         CALL ERRHDL(PATH,MODNAM,'E','203','GRPID')
         GO TO 999
      END IF

C     Retrieve Format Secondary Keyword
      IF (FIELD(5) .EQ. 'UNFORM') THEN
         IPSFRM(INDGRP,INDAVE) = 0
      ELSE IF (FIELD(5) .EQ. 'PLOT') THEN
         IPSFRM(INDGRP,INDAVE) = 1
c***** Modified  EMI  2/15/96
c  Add option for compressed (BLP format) binary file
c  Set average time to one hour and source groups to all
      else if (field(5) .eq. 'COMPR') then
          iprdt = 1
          indgrp = 1
          numgrp = 1
          indave = 1
          ipsfrm(indgrp,indave) = 2
          lcompr = .TRUE.
          do 30 i = 1,numsrc
             igroup(i,igrp) = 1
30        continue
c***** End of Modification
      ELSE
C        Error Message: Invalid Format Specified for POSTFILE
         CALL ERRHDL(PATH,MODNAM,'E','203','FORMAT')
         GO TO 999
      END IF

C     Set Switch and Check for Previous POSTFILE Card
C     for This Averaging Period & Group ID
      IPSTFL(INDGRP,INDAVE) = IPSTFL(INDGRP,INDAVE) + 1
      IF (IPSTFL(INDGRP,INDAVE) .GT. 1) THEN
C        WRITE Error Message
         CALL ERRHDL(PATH,MODNAM,'E','211',KEYWRD)
         GO TO 999
      END IF

      IF ((LOCE(6)-LOCB(6)) .LE. 39) THEN
C        Retrieve Filename as Character Substring to Maintain Original Case
C        Also Check for Filename Larger Than 40 Characters
         PSTFIL(INDGRP,INDAVE) = RUNST1(LOCB(6):LOCE(6))
      ELSE
C        WRITE Error Message:  PSTFIL Field is Too Long
         CALL ERRHDL(PATH,MODNAM,'E','203',' FILNAM ')
      END IF

C     Retrieve File Unit If Input, or Assign File Unit and OPEN File
      IF (IFC .EQ. 7) THEN
         CALL STONUM(FIELD(7),40,FNUM,IMIT)
C        Check for Valid Threshold Value
         IF (IMIT .NE. 1) THEN
C           Write Error Message: Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
C        Check for Conflict With System Files
         IF (FNUM .LT. 20.) THEN
C           WRITE Error Message: Invalid File Unit Specified
            CALL ERRHDL(PATH,MODNAM,'E','560',KEYWRD)
            GO TO 999
         ELSE IF (FNUM .GT. 100) THEN
C           WRITE Warning Message:  Suspect File Unit Specified
C           Unit May Conflict With Dynamically Allocated File Units
            CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
            IPSUNT(INDGRP,INDAVE) = INT(FNUM)
         ELSE
            IPSUNT(INDGRP,INDAVE) = INT(FNUM)
         END IF
      ELSE
C        Dynamically Allocate File Unit (200's)
         IPSUNT(INDGRP,INDAVE) = 200 + INDGRP*10 + INDAVE
         IF (INDGRP .GE. 10 .OR. INDAVE .GE. 10) THEN
C           WRITE Warning Message: Dynamic FUnit Allocation May Have Conflict
            CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
         END IF
      END IF

C     Check for Earlier Use of This Filename and File Unit
      FOUND = .FALSE.
      DO 200 J = 1, NUMAVE
         DO 100 I = 1, NUMGRP
            IF (I .NE. INDGRP .OR. J .NE. INDAVE) THEN
               IF (PSTFIL(INDGRP,INDAVE) .EQ. PSTFIL(I,J) .AND.
     &             IPSUNT(INDGRP,INDAVE) .EQ. IPSUNT(I,J)) THEN
                  FOUND = .TRUE.
               ELSE IF (PSTFIL(INDGRP,INDAVE) .EQ. PSTFIL(I,J) .AND.
     &                  IPSUNT(INDGRP,INDAVE) .NE. IPSUNT(I,J)) THEN
C                 Write Error Message: Conflicting Inputs
                  CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
                  GO TO 999
               ELSE IF (PSTFIL(INDGRP,INDAVE) .NE. PSTFIL(I,J) .AND.
     &                  IPSUNT(INDGRP,INDAVE) .EQ. IPSUNT(I,J)) THEN
C                 Write Error Message: Conflicting Inputs
                  CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
                  GO TO 999
               END IF
            END IF
 100     CONTINUE
 200  CONTINUE

C     Check Against POSTFILEs for PERIOD Averages
      DO 300 I = 1, NUMGRP
         IF (PSTFIL(INDGRP,INDAVE) .EQ. ANNPST(I) .AND.
     &       IPSUNT(INDGRP,INDAVE) .EQ. IAPUNT(I)) THEN
           FOUND = .TRUE.
         ELSE IF (PSTFIL(INDGRP,INDAVE) .EQ. ANNPST(I) .AND.
     &            IPSUNT(INDGRP,INDAVE) .NE. IAPUNT(I)) THEN
C          Write Error Message: Conflicting Inputs
           CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
           GO TO 999
         ELSE IF (PSTFIL(INDGRP,INDAVE) .NE. ANNPST(I) .AND.
     &            IPSUNT(INDGRP,INDAVE) .EQ. IAPUNT(I)) THEN
C          Write Error Message: Conflicting Inputs
           CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
           GO TO 999
         END IF
 300  CONTINUE

      IF (.NOT. FOUND) THEN
C        First Time File is Identified - OPEN File
         IF (FIELD(5) .EQ. 'UNFORM') THEN
           OPEN(IPSUNT(INDGRP,INDAVE),ERR=99,FILE=PSTFIL(INDGRP,INDAVE),
     &          IOSTAT=IOERRN,FORM='UNFORMATTED',STATUS='UNKNOWN')
            IF (RSTINP) THEN
C              Results Arrays Are To Be Initialized From Re-start File.
C              Read Start Date From File and Rewind.
               DUMMY = 'INITFILE'
               READ(IRSUNT,ERR=919,END=919) ISDATE
               REWIND IRSUNT
C              Now Position POSTFILE To End of File, But Not Past ISDATE.
               DUMMY = 'POSTFILE'
               EOF = .FALSE.
               DO WHILE (.NOT. EOF)
                  READ(IPSUNT(INDGRP,INDAVE),ERR=919,END=199) IDAT
                  IF (IDAT .GT. ISDATE) THEN
C                    Date of POSTFILE Record Is Greater Than Start Date
C                    From Save File.  Treat As End of File to Exit Loop.
                     GO TO 199
                  END IF
                  GO TO 11
 199              EOF = .TRUE.
 11               CONTINUE
               END DO
               EOF = .FALSE.
               BACKSPACE IPSUNT(INDGRP,INDAVE)
C              Skip Header Records
               GO TO 999
            END IF
         ELSE IF (FIELD(5) .EQ. 'PLOT') THEN
           OPEN(IPSUNT(INDGRP,INDAVE),ERR=99,FILE=PSTFIL(INDGRP,INDAVE),
     &          IOSTAT=IOERRN,FORM='FORMATTED',STATUS='UNKNOWN')
            IF (RSTINP) THEN
C              Results Arrays Are To Be Initialized From Re-start File.
C              Read Start Date From File and Rewind.
               DUMMY = 'INITFILE'
               READ(IRSUNT,ERR=919,END=919) ISDATE
               REWIND IRSUNT
C              Now Position POSTFILE To End of File, But Not Past ISDATE.
               DUMMY = 'POSTFILE'
               EOF = .FALSE.
               DO WHILE (.NOT. EOF)
                  READ(IPSUNT(INDGRP,INDAVE),'(A132)',
     &                                        ERR=919,END=299) BUF132
                  IF (BUF132(1:1) .NE. '*') THEN
C                    Record Is Not Part of Header - Read Date For This Record
C                    First calculate start & end of date string based on NUMTYP
                     ISTR = 72 + 14*(NUMTYP-1)
                     ISTP = ISTR + 7
                     READ(BUF132(ISTR:ISTP),'(I8)',ERR=919) IDAT
                  END IF
                  IF (IDAT .GT. ISDATE) THEN
C                    Date of POSTFILE Record Is Greater Than Start Date
C                    From Save File.  Treat As End of File to Exit Loop.
                     GO TO 299
                  END IF
                  GO TO 21
 299              EOF = .TRUE.
 21               CONTINUE
               END DO
               EOF = .FALSE.
               BACKSPACE IPSUNT(INDGRP,INDAVE)
C              Skip Header Records
               GO TO 999
            END IF
         END IF
      ELSE IF (FOUND .AND. RSTINP) THEN
C        This Run is a Re-start From an Earlier Run
C        Skip Header Records
         GO TO 999
      END IF

C     Write Header to File for Formatted Plot Files.  Skip Header if FATAL.
      IF (RUN .AND. .NOT.FATAL .AND. FIELD(5) .EQ. 'PLOT') THEN
         WRITE(IPSUNT(INDGRP,INDAVE),9005) VERSN, TITLE1
 9005    FORMAT('* ISC3P  (',A5,'): ',A68)
         WRITE(IPSUNT(INDGRP,INDAVE),9007) (MODOPS(I),I=1,17)
 9007    FORMAT('* MODELING OPTIONS USED:',/'* ',17(1X,A6))
         WRITE(IPSUNT(INDGRP,INDAVE),9010) CHRAVE(INDAVE),GRPID(INDGRP),
     &                                     NUMREC, PSTFRM
 9010    FORMAT('*',9X,'POST/PLOT FILE OF CONCURRENT ',A5,' VALUES',
     &         ' FOR SOURCE GROUP: ',A8,
     &         /'*',9X,'FOR A TOTAL OF ',I5,' RECEPTORS.',
     &         /'*',9X,'FORMAT: ',A60)
         WRITE(IPSUNT(INDGRP,INDAVE),HDRFRM) (CHIDEP(1,ITYP),
     &                   CHIDEP(2,ITYP), CHIDEP(3,ITYP), ITYP=1,NUMTYP)
 9020 FORMAT('(''*'',8X,''X'',13X,''Y'',4X,',I1,'(2X,3A4),3X,''ZELEV'',
     &  5X,''AVE'',5X,''GRP'',7X,''DATE'',5X,''NET ID'',/,''*'',2X,',
     &  I1,'(''___________'',3X),''______  ______  ________  ________'',
     &  ''  ________'')')
      END IF

      GO TO 999

C     WRITE Error Message for Error Opening File
 99   WRITE(DUMMY,'(5HPSTFL,I3.3)') IPSUNT(INDGRP,INDAVE)
      CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

      GO TO 999

C     WRITE Error Message for Error Reading File
 919  CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)

c*****  Modified by EMI   2/15/96
 999  if (lcompr) then
         open(ipsunt(indgrp,indave),err=99,file=pstfil(indgrp,indave),
     &        iostat=ioerrn,form='unformatted',status='unknown')
c   write title to output file in BLP format for postprocessing ease
         if( isyear .lt. 2000) then
            iyr = isyear - 1900
         else
            iyr = isyear - 2000
         endif
         rema = mod(iyr,4)
         if( rema .ne. 0.000) iproc(366) = 0
         call outitl( title1,numrec,numsrc,0,iyr,iproc,lcompr,
     &      ipsunt(indgrp,indave),emifac)
c   write receptor information to binary file
         itap = ipsunt(indgrp,indave)
         call outrecp( itap)
      endif

C     Set Logical Switch Indicating That Postprocessor File(s) Are Generated
c 999  PPFILE = .TRUE.
      ppfile = .true.
c***** End of Modification

      RETURN
      END

      SUBROUTINE OUPLOT
C***********************************************************************
C                 OUPLOT Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: To Process Plot File Output Selections
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  To Change File Length Limit To 40 - 9/29/92
C
C        INPUTS:  Input Runstream Parameters
C
C        OUTPUTS: Output Option Switches
C
C        CALLED FROM:   OUCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      CHARACTER NCHR1(10)*8, NCHR2(10)*4, INPGRP*8
      LOGICAL FOUND

C     Variable Initializations
      DATA (NCHR1(I),I=1,10) /'FIRST','SECOND','THIRD','FOURTH',
     &                        'FIFTH','SIXTH','SEVENTH','EIGHTH',
     &                        'NINTH','TENTH'/
      DATA (NCHR2(I),I=1,10) /'1ST','2ND','3RD','4TH','5TH','6TH',
     &                        '7TH','8TH','9TH','10TH'/
      MODNAM = 'OUPLOT'

C     Check If Enough Field
      IF (IFC .EQ. 2) THEN
C        Error Message: No Fields
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 5) THEN
C        Error Message: Not Enough Fields
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 7) THEN
C        Error Message: Too Many Fields
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Retrieve Averaging Period
      IF (FIELD(3) .EQ. 'PERIOD' .AND. PERIOD) THEN
C        Plot File is for PERIOD Averages                   ---   CALL PERPLT
         CALL PERPLT
C        Exit to End
         GO TO 999
      ELSE IF (FIELD(3) .EQ. 'ANNUAL' .AND. ANNUAL) THEN
C        Plot File is for PERIOD Averages                   ---   CALL PERPLT
         CALL PERPLT
C        Exit to End
         GO TO 999
      ELSE IF (FIELD(3).EQ.'PERIOD' .OR. FIELD(3).EQ.'ANNUAL') THEN
C        Period Plot File Selected But No PERIOD Averages Calculated
C        WRITE Error Message: Invalid Averaging Period Selected for PLOTFILE
         CALL ERRHDL(PATH,MODNAM,'E','203',' AVEPER ')
         GO TO 999
      ELSE IF (FIELD(3) .EQ. 'MONTH' .AND. MONTH) THEN
C        Set Value of IPRDT = 720 for MONTHly Averages
         IPRDT = 720
      ELSE
         CALL STONUM(FIELD(3),40,FNUM,IMIT)
         IF (IMIT .NE. 1) THEN
C           Write Error Message:Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
         IPRDT = INT(FNUM)
      END IF

C     Check Short Term Averaging Period Against KAVE Array
      FOUND = .FALSE.
      J = 1
      DO WHILE (.NOT.FOUND .AND. J.LE.NUMAVE)
         IF (IPRDT .EQ. KAVE(J)) THEN
            FOUND = .TRUE.
            INDAVE = J
         END IF
         J = J + 1
      END DO
      IF (.NOT. FOUND) THEN
C        Error Message:E203 AVEPER Not Match With Pre-Defined One
         CALL ERRHDL(PATH,MODNAM,'E','203','AVEPER')
         GO TO 999
      END IF

C     Retrieve Source Group ID
      INPGRP = FIELD(4)
C     Check Source Group ID
      FOUND = .FALSE.
      J = 1
      DO WHILE (.NOT.FOUND .AND. J.LE.NUMGRP)
         IF (INPGRP .EQ. GRPID(J)) THEN
            FOUND = .TRUE.
            INDGRP = J
         END IF
         J = J + 1
      END DO
      IF (.NOT. FOUND) THEN
C        Error Message:E203 GRPID Not Match With Pre-Defined One
         CALL ERRHDL(PATH,MODNAM,'E','203','GRPID')
         GO TO 999
      END IF

C     Retrieve High Value
      FOUND = .FALSE.
      DO 50 I = 1, 10
         IF (FIELD(5).EQ.NCHR1(I) .OR. FIELD(5).EQ.NCHR2(I)) THEN
            FOUND = .TRUE.
            INDVAL = I
         END IF
 50   CONTINUE
      IF (.NOT. FOUND) THEN
C        Error Message:E203 INDVAL Not Match With Options
         CALL ERRHDL(PATH,MODNAM,'E','203','HIVALU')
         GO TO 999
      END IF

C     Check High Value Specified Against Previous Options
      IF (NHIAVE(INDVAL,INDAVE) .NE. 1) THEN
C        WRITE Error Message
         CALL ERRHDL(PATH,MODNAM,'E','203','HIVALU')
         GO TO 999
      END IF

C     Set Switch and Check for Previous PLOTFILE Card
C     for This Averaging Period & Group ID
      IPLTFL(INDVAL,INDGRP,INDAVE) = IPLTFL(INDVAL,INDGRP,INDAVE) + 1
      IF (IPLTFL(INDVAL,INDGRP,INDAVE) .GT. 1) THEN
C        WRITE Error Message
         CALL ERRHDL(PATH,MODNAM,'E','211',KEYWRD)
         GO TO 999
      END IF

      IF ((LOCE(6)-LOCB(6)) .LE. 39) THEN
C        Retrieve Filename as Character Substring to Maintain Original Case
C        Also Check for Filename Larger Than 40 Characters
         PLTFIL(INDVAL,INDGRP,INDAVE) = RUNST1(LOCB(6):LOCE(6))
      ELSE
C        WRITE Error Message:  PLTFIL Field is Too Long
         CALL ERRHDL(PATH,MODNAM,'E','203',' FILNAM ')
      END IF

C     Retrieve File Unit If Input, or Assign File Unit and OPEN File
      IF (IFC .EQ. 7) THEN
         CALL STONUM(FIELD(7),40,FNUM,IMIT)
C        Check for Valid Threshold Value
         IF (IMIT .NE. 1) THEN
C           Write Error Message: Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
C        Check for Conflict With System Files
         IF (FNUM .LT. 20.) THEN
C           WRITE Error Message: Invalid File Unit Specified
            CALL ERRHDL(PATH,MODNAM,'E','560',KEYWRD)
            GO TO 999
         ELSE IF (FNUM .GT. 100) THEN
C           WRITE Warning Message:  Suspect File Unit Specified
C           Unit May Conflict With Dynamically Allocated File Units
            CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
            IPLUNT(INDVAL,INDGRP,INDAVE) = INT(FNUM)
         ELSE
            IPLUNT(INDVAL,INDGRP,INDAVE) = INT(FNUM)
         END IF
      ELSE
C        Dynamically Allocate File Unit (> 400)
         IPLUNT(INDVAL,INDGRP,INDAVE) = (INDVAL+3)*100+INDGRP*10+INDAVE
         IF (INDGRP .GE. 10 .OR. INDAVE .GE. 10) THEN
C           WRITE Warning Message: Dynamic FUnit Allocation May Have Conflict
            CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
         END IF
      END IF

C     Check for Earlier Use of This Filename and File Unit
      FOUND = .FALSE.
      DO 300 K = 1, NUMAVE
        DO 200 J = 1, NUMGRP
          DO 100 I = 1, NHIVAL
            IF (I.NE.INDVAL .OR. J.NE.INDGRP .OR. K.NE.INDAVE) THEN
              IF (PLTFIL(INDVAL,INDGRP,INDAVE) .EQ. PLTFIL(I,J,K) .AND.
     &            IPLUNT(INDVAL,INDGRP,INDAVE) .EQ. IPLUNT(I,J,K)) THEN
                FOUND = .TRUE.
              ELSEIF (PLTFIL(INDVAL,INDGRP,INDAVE).EQ.PLTFIL(I,J,K).AND.
     &                IPLUNT(INDVAL,INDGRP,INDAVE).NE.IPLUNT(I,J,K))THEN
C               Write Error Message: Conflicting Inputs
                CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
                GO TO 999
              ELSEIF (PLTFIL(INDVAL,INDGRP,INDAVE).NE.PLTFIL(I,J,K).AND.
     &                IPLUNT(INDVAL,INDGRP,INDAVE).EQ.IPLUNT(I,J,K))THEN
C               Write Error Message: Conflicting Inputs
                CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
                GO TO 999
              END IF
            END IF
 100      CONTINUE
 200    CONTINUE
 300  CONTINUE

C     Check Against PLOTFILEs for PERIOD Averages
      DO 400 I = 1, NUMGRP
         IF (PLTFIL(INDVAL,INDGRP,INDAVE) .EQ. ANNPLT(I) .AND.
     &       IPLUNT(INDVAL,INDGRP,INDAVE) .EQ. IPPUNT(I)) THEN
           FOUND = .TRUE.
         ELSE IF (PLTFIL(INDVAL,INDGRP,INDAVE) .EQ. ANNPLT(I) .AND.
     &            IPLUNT(INDVAL,INDGRP,INDAVE) .NE. IPPUNT(I)) THEN
C          Write Error Message: Conflicting Inputs
           CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
           GO TO 999
         ELSE IF (PLTFIL(INDVAL,INDGRP,INDAVE) .NE. ANNPLT(I) .AND.
     &            IPLUNT(INDVAL,INDGRP,INDAVE) .EQ. IPPUNT(I)) THEN
C          Write Error Message: Conflicting Inputs
           CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
           GO TO 999
         END IF
 400  CONTINUE

      IF (.NOT. FOUND) THEN
C        First Time File is Identified - OPEN File
         OPEN(IPLUNT(INDVAL,INDGRP,INDAVE),ERR=99,
     &        FILE=PLTFIL(INDVAL,INDGRP,INDAVE),
     &        IOSTAT=IOERRN,STATUS='UNKNOWN')
      END IF

C     Set Logical Switch Indicating That Plot File(s) Are Generated
      PLFILE = .TRUE.

      GO TO 999

C     WRITE Error Message for Error Opening File
 99   WRITE(DUMMY,'(5HPLTFL,I3.3)') IPLUNT(INDVAL,INDGRP,INDAVE)
      CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

 999  RETURN
      END

      SUBROUTINE PERPST
C***********************************************************************
C                 PERPST Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: To Process Postprocessor File Output Selection for PERIOD
C                 Averages
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  To Change File Length Limit To 40 - 9/29/92
C
C        INPUTS:  Input Runstream Parameters
C
C        OUTPUTS: Output Option Switches
C
C        CALLED FROM:   OUPLOT
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      CHARACTER INPGRP*8
      LOGICAL FOUND

C     Variable Initializations
      MODNAM = 'PERPST'

C     Check If Too Many Fields
      IF (IFC .GT. 7) THEN
C        Error Message: Too Many Fields
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Retrieve Source Group ID
      INPGRP = FIELD(4)
C     Check Source Group ID
      FOUND = .FALSE.
      J = 1
      DO WHILE (.NOT.FOUND .AND. J.LE.NUMGRP)
         IF (INPGRP .EQ. GRPID(J)) THEN
            FOUND = .TRUE.
            INDGRP = J
         END IF
         J = J + 1
      END DO
      IF (.NOT. FOUND) THEN
C        Error Message: E203 GRPID Not Match With Pre-Defined One
         CALL ERRHDL(PATH,MODNAM,'E','203','GRPID')
         GO TO 999
      END IF

C     Set Switch and Check for Previous POSTFILE Card
C     for This Averaging Period & Group ID
      IANPST(INDGRP) = IANPST(INDGRP) + 1
      IF (IANPST(INDGRP) .GT. 1) THEN
C        WRITE Error Message
         CALL ERRHDL(PATH,MODNAM,'E','211',KEYWRD)
         GO TO 999
      END IF

C     Retrieve Format Secondary Keyword
      IF (FIELD(5) .EQ. 'UNFORM') THEN
         IANFRM(INDGRP) = 0
      ELSE IF (FIELD(5) .EQ. 'PLOT') THEN
         IANFRM(INDGRP) = 1
      ELSE
C        Error Message: Invalid Format Specified for POSTFILE
         CALL ERRHDL(PATH,MODNAM,'E','203','FORMAT')
         GO TO 999
      END IF

      IF ((LOCE(6)-LOCB(6)) .LE. 39) THEN
C        Retrieve Filename as Character Substring to Maintain Original Case
C        Also Check for Filename Larger Than 40 Characters
         ANNPST(INDGRP) = RUNST1(LOCB(6):LOCE(6))
      ELSE
C        WRITE Error Message:  ANNPST Field is Too Long
         CALL ERRHDL(PATH,MODNAM,'E','203',' FILNAM ')
      END IF

C     Retrieve File Unit If Input, or Assign File Unit and OPEN File
      IF (IFC .EQ. 7) THEN
         CALL STONUM(FIELD(7),40,FNUM,IMIT)
C        Check for Valid Threshold Value
         IF (IMIT .NE. 1) THEN
C           Write Error Message: Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
C        Check for Conflict With System Files
         IF (FNUM .LT. 20.) THEN
C           WRITE Error Message: Invalid File Unit Specified
            CALL ERRHDL(PATH,MODNAM,'E','560',KEYWRD)
            GO TO 999
         ELSE IF (FNUM .GT. 100) THEN
C           WRITE Warning Message:  Suspect File Unit Specified
C           Unit May Conflict With Dynamically Allocated File Units
            CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
            IAPUNT(INDGRP) = INT(FNUM)
         ELSE
            IAPUNT(INDGRP) = INT(FNUM)
         END IF
      ELSE
C        Dynamically Allocate File Unit (300's)
         IAPUNT(INDGRP) = 300 + INDGRP*10 - 5
         IF (INDGRP .GE. 10) THEN
C           WRITE Warning Message: Dynamic FUnit Allocation May Have Conflict
            CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
         END IF
      END IF

C     Check for Earlier Use of This Filename and File Unit
      FOUND = .FALSE.
      DO 100 I = 1, NUMGRP
         IF (I .NE. INDGRP) THEN
            IF (ANNPST(INDGRP) .EQ. ANNPST(I) .AND.
     &          IAPUNT(INDGRP) .EQ. IAPUNT(I)) THEN
              FOUND = .TRUE.
            ELSE IF (ANNPST(INDGRP) .EQ. ANNPST(I) .AND.
     &               IAPUNT(INDGRP) .NE. IAPUNT(I)) THEN
C             Write Error Message: Conflicting Inputs
              CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
              GO TO 999
            ELSE IF (ANNPST(INDGRP) .NE. ANNPST(I) .AND.
     &               IAPUNT(INDGRP) .EQ. IAPUNT(I)) THEN
C             Write Error Message: Conflicting Inputs
              CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
              GO TO 999
            END IF
         END IF
 100  CONTINUE

C     Check Against POSTFILEs for Short Term Averages
      DO 300 J = 1, NUMAVE
         DO 200 I = 1, NUMGRP
            IF (ANNPST(INDGRP) .EQ. PSTFIL(I,J) .AND.
     &          IAPUNT(INDGRP) .EQ. IPSUNT(I,J)) THEN
               FOUND = .TRUE.
            ELSE IF (ANNPST(INDGRP) .EQ. PSTFIL(I,J) .AND.
     &               IAPUNT(INDGRP) .NE. IPSUNT(I,J)) THEN
C              Write Error Message: Conflicting Inputs
               CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
               GO TO 999
            ELSE IF (ANNPST(INDGRP) .NE. PSTFIL(I,J) .AND.
     &               IAPUNT(INDGRP) .EQ. IPSUNT(I,J)) THEN
C              Write Error Message: Conflicting Inputs
               CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
               GO TO 999
            END IF
 200     CONTINUE
 300  CONTINUE

      IF (.NOT. FOUND) THEN
C        First Time File is Identified - OPEN File
         IF (FIELD(5) .EQ. 'UNFORM') THEN
            OPEN(IAPUNT(INDGRP),ERR=99,FILE=ANNPST(INDGRP),
     &           IOSTAT=IOERRN,FORM='UNFORMATTED',STATUS='UNKNOWN')
         ELSE IF (FIELD(5) .EQ. 'PLOT') THEN
            OPEN(IAPUNT(INDGRP),ERR=99,FILE=ANNPST(INDGRP),
     &           IOSTAT=IOERRN,FORM='FORMATTED',STATUS='UNKNOWN')
         END IF
      END IF

C     Set Logical Switch Indicating That Post File(s) Are Generated
      ANPOST = .TRUE.

      GO TO 999

C     WRITE Error Message for Error Opening File
 99   WRITE(DUMMY,'(5HPSTFL,I3.3)') IAPUNT(INDGRP)
      CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

 999  RETURN
      END

      SUBROUTINE PERPLT
C***********************************************************************
C                 PERPLT Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: To Process Plot File Output Selection for PERIOD
C                 Averages
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  To Change File Length Limit To 40 - 9/29/92
C
C        INPUTS:  Input Runstream Parameters
C
C        OUTPUTS: Output Option Switches
C
C        CALLED FROM:   OUPLOT
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      CHARACTER INPGRP*8
      LOGICAL FOUND

C     Variable Initializations
      MODNAM = 'PERPLT'

C     Check If Too Many Fields
      IF (IFC .GT. 6) THEN
C        Error Message: Too Many Fields
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Retrieve Source Group ID
      INPGRP = FIELD(4)
C     Check Source Group ID
      FOUND = .FALSE.
      J = 1
      DO WHILE (.NOT.FOUND .AND. J.LE.NUMGRP)
         IF (INPGRP .EQ. GRPID(J)) THEN
            FOUND = .TRUE.
            INDGRP = J
         END IF
         J = J + 1
      END DO
      IF (.NOT. FOUND) THEN
C        Error Message: E203 GRPID Not Match With Pre-Defined One
         CALL ERRHDL(PATH,MODNAM,'E','203','GRPID')
         GO TO 999
      END IF

C     Set Switch and Check for Previous PLOTFILE Card
C     for This Averaging Period & Group ID
      IANPLT(INDGRP) = IANPLT(INDGRP) + 1
      IF (IANPLT(INDGRP) .GT. 1) THEN
C        WRITE Error Message
         CALL ERRHDL(PATH,MODNAM,'E','211',KEYWRD)
         GO TO 999
      END IF

      IF ((LOCE(6)-LOCB(6)) .LE. 39) THEN
C        Retrieve Filename as Character Substring to Maintain Original Case
C        Also Check for Filename Larger Than 40 Characters
         ANNPLT(INDGRP) = RUNST1(LOCB(5):LOCE(5))
      ELSE
C        WRITE Error Message:  ANNPLT Field is Too Long
         CALL ERRHDL(PATH,MODNAM,'E','203',' FILNAM ')
      END IF

C     Retrieve File Unit If Input, or Assign File Unit and OPEN File
      IF (IFC .EQ. 6) THEN
         CALL STONUM(FIELD(6),40,FNUM,IMIT)
C        Check for Valid Threshold Value
         IF (IMIT .NE. 1) THEN
C           Write Error Message: Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
C        Check for Conflict With System Files
         IF (FNUM .LT. 20.) THEN
C           WRITE Error Message: Invalid File Unit Specified
            CALL ERRHDL(PATH,MODNAM,'E','560',KEYWRD)
            GO TO 999
         ELSE IF (FNUM .GT. 100) THEN
C           WRITE Warning Message:  Suspect File Unit Specified
C           Unit May Conflict With Dynamically Allocated File Units
            CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
            IPPUNT(INDGRP) = INT(FNUM)
         ELSE
            IPPUNT(INDGRP) = INT(FNUM)
         END IF
      ELSE
C        Dynamically Allocate File Unit (300's)
         IPPUNT(INDGRP) = 300 + INDGRP*10
         IF (INDGRP .GE. 10) THEN
C           WRITE Warning Message: Dynamic FUnit Allocation May Have Conflict
            CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
         END IF
      END IF

C     Check for Earlier Use of This Filename and File Unit
      FOUND = .FALSE.
      DO 100 I = 1, NUMGRP
         IF (I .NE. INDGRP) THEN
            IF (ANNPLT(INDGRP) .EQ. ANNPLT(I) .AND.
     &          IPPUNT(INDGRP) .EQ. IPPUNT(I)) THEN
              FOUND = .TRUE.
            ELSE IF (ANNPLT(INDGRP) .EQ. ANNPLT(I) .AND.
     &               IPPUNT(INDGRP) .NE. IPPUNT(I)) THEN
C             Write Error Message: Conflicting Inputs
              CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
              GO TO 999
            ELSE IF (ANNPLT(INDGRP) .NE. ANNPLT(I) .AND.
     &               IPPUNT(INDGRP) .EQ. IPPUNT(I)) THEN
C             Write Error Message: Conflicting Inputs
              CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
              GO TO 999
            END IF
         END IF
 100  CONTINUE

C     Check Against PLOTFILEs for Short Term Averages
      DO 400 K = 1, NUMAVE
         DO 300 J = 1, NUMGRP
            DO 200 I = 1, NHIVAL
               IF (ANNPLT(INDGRP) .EQ. PLTFIL(I,J,K) .AND.
     &             IPPUNT(INDGRP) .EQ. IPLUNT(I,J,K)) THEN
                  FOUND = .TRUE.
               ELSE IF (ANNPLT(INDGRP) .EQ. PLTFIL(I,J,K) .AND.
     &                  IPPUNT(INDGRP) .NE. IPLUNT(I,J,K)) THEN
C                 Write Error Message: Conflicting Inputs
                  CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
                  GO TO 999
               ELSE IF (ANNPLT(INDGRP) .NE. PLTFIL(I,J,K) .AND.
     &                  IPPUNT(INDGRP) .EQ. IPLUNT(I,J,K)) THEN
C                 Write Error Message: Conflicting Inputs
                  CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
                  GO TO 999
               END IF
 200        CONTINUE
 300     CONTINUE
 400  CONTINUE

      IF (.NOT. FOUND) THEN
C        First Time File is Identified - OPEN File
         OPEN(IPPUNT(INDGRP),ERR=99,FILE=ANNPLT(INDGRP),
     &        IOSTAT=IOERRN,STATUS='UNKNOWN')
      END IF

C     Set Logical Switch Indicating That Plot File(s) Are Generated
      ANPLOT = .TRUE.

      GO TO 999

C     WRITE Error Message for Error Opening File
 99   WRITE(DUMMY,'(5HPLTFL,I3.3)') IPPUNT(INDGRP)
      CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

 999  RETURN
      END

      SUBROUTINE OUTOXX
C***********************************************************************
C                 OUTOXX Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: To Process TOXXFILE Output Selections
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    September 29, 1992
C
C        INPUTS:  Input Runstream Parameters
C
C        OUTPUTS: Output Option Switches
C
C        CALLED FROM:   OUCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      CHARACTER BUF12*12
      LOGICAL FOUND

C     Variable Initializations
      MODNAM = 'OUTOXX'
      BUF12  = '            '
      IDUM = 0
      RDUM = 0.
      NIDUM = 3
      NRDUM = 9

C     Check If Enough Fields
      IF (IFC .EQ. 2) THEN
C        Error Message: No Fields
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 5) THEN
C        Error Message: Not Enough Fields
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 6) THEN
C        Error Message: Too Many Fields
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Retrieve Averaging Period
      IF (FIELD(3) .EQ. 'MONTH' .AND. MONTH) THEN
C        Set Value of IPRDT = 720 for MONTHly Averages
         IPRDT = 720
      ELSE
         CALL STONUM(FIELD(3),40,FNUM,IMIT)
         IF (IMIT .NE. 1) THEN
C           Write Error Message:Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
         IPRDT = INT(FNUM)
      END IF

C     Check Averaging Period Against KAVE Array
      FOUND = .FALSE.
      J = 1
      DO WHILE (.NOT.FOUND .AND. J.LE.NUMAVE)
         IF (IPRDT .EQ. KAVE(J)) THEN
            FOUND = .TRUE.
            INDAVE = J
         END IF
         J = J + 1
      END DO
      IF (.NOT. FOUND) THEN
C        Error Message:E203 AVEPER Not Match With Pre-Defined One
         CALL ERRHDL(PATH,MODNAM,'E','203','AVEPER')
         GO TO 999
      END IF

C     Check for Averaging Period Other Than 1-HOUR, and Write Warning
      IF (IPRDT .NE. 1) THEN
         WRITE(DUMMY,'(2X,I4,2X)') IPRDT
         CALL ERRHDL(PATH,MODNAM,'W','385',DUMMY)
      END IF

C     Set Switch and Check for Previous TOXXFILE Card
C     for This Averaging Period
      ITOXFL(INDAVE) = ITOXFL(INDAVE) + 1
      IF (ITOXFL(INDAVE) .GT. 1) THEN
C        WRITE Error Message
         CALL ERRHDL(PATH,MODNAM,'E','211',KEYWRD)
         GO TO 999
      END IF

C     Retrieve Threshold
      CALL STONUM(FIELD(4),40,FNUM,IMIT)
C     Check for Valid Threshold Value
      IF (IMIT .NE. 1) THEN
C        Write Error Message:Invalid Numerical Field
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF
      TOXTHR(INDAVE) = FNUM

      IF ((LOCE(5)-LOCB(5)) .LE. 39) THEN
C        Retrieve Filename as Character Substring to Maintain Original Case
C        Also Check for Filename Larger Than 40 Characters
         TOXFIL(INDAVE) = RUNST1(LOCB(5):LOCE(5))
      ELSE
C        WRITE Error Message:  TOXFIL Field is Too Long
         CALL ERRHDL(PATH,MODNAM,'E','203',' FILNAM ')
      END IF

C     Retrieve File Unit If Input, or Assign File Unit and OPEN File
      IF (IFC .EQ. 6) THEN
         CALL STONUM(FIELD(6),40,FNUM,IMIT)
C        Check for Valid Threshold Value
         IF (IMIT .NE. 1) THEN
C           Write Error Message:Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
C        Check for Conflict With System Files
         IF (FNUM .LT. 20.) THEN
C           WRITE Error Message:  Invalid File Unit Specified
            CALL ERRHDL(PATH,MODNAM,'E','560',KEYWRD)
            GO TO 999
         ELSE IF (FNUM .GT. 100) THEN
C           WRITE Warning Message:  Suspect File Unit Specified
C           Unit May Conflict With Dynamically Allocated File Units
            CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
            ITXUNT(INDAVE) = INT(FNUM)
         ELSE
            ITXUNT(INDAVE) = INT(FNUM)
         END IF
      ELSE
C        Dynamically Allocate File Unit (300's)
         ITXUNT(INDAVE) = 300 + INDAVE
         IF (INDAVE .GE. 5) THEN
C           WRITE Warning Message: Dynamic FUnit Allocation May Have Conflict
            CALL ERRHDL(PATH,MODNAM,'W','565',KEYWRD)
         END IF
      END IF

C     Check for Earlier Use of This Filename and File Unit
      FOUND = .FALSE.
      DO 100 I = 1, NUMAVE
         IF (I .NE. INDAVE) THEN
            IF (TOXFIL(INDAVE) .EQ. TOXFIL(I) .AND.
     &          ITXUNT(INDAVE) .EQ. ITXUNT(I)) THEN
               FOUND = .TRUE.
            ELSE IF (TOXFIL(INDAVE) .EQ. TOXFIL(I) .AND.
     &               ITXUNT(INDAVE) .NE. ITXUNT(I)) THEN
C              Write Error Message: Conflicting Inputs
               CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
               GO TO 999
            ELSE IF (TOXFIL(INDAVE) .NE. TOXFIL(I) .AND.
     &               ITXUNT(INDAVE) .EQ. ITXUNT(I)) THEN
C              Write Error Message: Conflicting Inputs
               CALL ERRHDL(PATH,MODNAM,'E','550',KEYWRD)
               GO TO 999
            END IF
         END IF
 100  CONTINUE

      IF (.NOT. FOUND) THEN
C        First Time File is Identified - OPEN File
         OPEN(ITXUNT(INDAVE),ERR=99,FILE=TOXFIL(INDAVE),
     &        FORM='UNFORMATTED',IOSTAT=IOERRN,STATUS='UNKNOWN')
      END IF

C     Write Header to File
      IF (RUN) THEN
         NUMPER = NHOURS/IPRDT
C        Write Header Records (BUF12 is used to fill out 80-character title)
         WRITE(ITXUNT(INDAVE)) TITLE1, BUF12
         WRITE(ITXUNT(INDAVE)) ISYEAR, NUMGRP, NUMREC, NUMPER, ITAB,
     &                         NXTOX, NYTOX, (IDUM,I=1,NIDUM)
         WRITE(ITXUNT(INDAVE)) TOXTHR(INDAVE), (RDUM,I=1,NRDUM)
      END IF

      GO TO 999

C     WRITE Error Message for Error Opening File
 99   WRITE(DUMMY,'(5HTOXFL,I3.3)') ITXUNT(INDAVE)
      CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

      GO TO 999

C     WRITE Error Message for Error Reading File
 919  CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)

C     Set Logical Switch Indicating That Maximum Value File(s) Are Generated
 999  TXFILE = .TRUE.

      RETURN
      END
c-----------------------------------------------------------------------
      subroutine depcor(vdi,vsi,zdi,zri,xri,xvi,hi,hmixi,ui,
     &                  xsrci,ysrci,xreci,yreci,
     &                  rurali,urbani,ksti,sgzi,sgz0i,szmni,
     &                  erin,epin,lterri,debugi,iouniti,
     &                  srctypi,ltgridi,kurdati,
     &                  qcor,pcor,pxrzd,szcor)
c-----------------------------------------------------------------------
c
c --- ISCST2    Version: 1.0            Level: 931215           DEPCOR
c               D. Strimaitis, SRC
c
c MODIFIED:    Remove comment erroneously placed on a valid ELSEIF line.
c              R. W. Brode, PES, Inc. - 4/19/96
c
c MODIFIED:    Includes elevation data (MSL) needed to simulate
c              COMPLEX I terrain treatment, and new settling treatment.
c
c MODIFIED:    Uses proper distance-dependent plume rise and sigmas in
c              the integration in QATR2.  Also modified to use terrain
c              adjustment during the integration for simple terrain.
c              R. W. Brode, PES, Inc. - 9/30/94
c
c MODIFIED by R. Brode, PES, added initialization for urban 'cp' - 8/2/94
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
c             zri       receptor height above sfc (m)                [r]
c             xri       receptor distance (m)                        [r]
c             xvi       virtual source distance (m)                  [r]
c             hi        plume height (m)                             [r]
c             hmixi     mixing height (m)                            [r]
c             ui        wind speed (m/s)                             [r]
c             xsrci     source position in x  (m)                    [r]
c             ysrci     source position in y  (m)                    [r]
c             xreci     receptor position in x  (m)                  [r]
c             yreci     receptor position in y  (m)                  [r]
c             rurali    logical for rural dispersion curves          [l]
c             urbani    logical for urban dispersion curves          [l]
c             ksti      stability class indicator                    [i]
c             sgzi      sigma-z at current receptor (m)              [r]
c             sgz0i     initial sigma-z (e.g. for BID) (m)           [r]
c             szmni     minimum sigma-z for settling   (m)           [r]
c             erin      elevation (MSL) at receptor    (m)           [r]
c             epin      elevation (MSL) at source      (m)           [r]
c             lterri    logical controlling terrain adjustments      [l]
c             debugi    logical controlling DEBUG output             [l]
c             iouniti   unit number for DEBUG output                 [i]
c             ltgridi   logical indicating gridded terrain data      [l]
c  RETURNED:  qcor      ratio of depleted emission rate to original  [r]
c             pcorzr    profile correction factor at receptor height [r]
c             pcorzd    profile correction factor at deposition ht   [r]
c             szcor     sigma-z correction factor (settling)         [r]
c
c CALLING ROUTINES:   PCALC, VCALC, ACALC
c
c EXTERNAL ROUTINES:  SZSETL, RESIST, PROFD, PROFD1, PROFD2,
c                     DEPLETE
c-----------------------------------------------------------------------
c  NOTE:  all inputs ending with "i" are passed to subsequent routines
c         through common /DEPVAR/.

      include 'DEPVAR.INC'
      logical rurali,urbani,lterri,debugi,ltgridi
      character*8 srctypi
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
CRWB      Note: These are initialized in SUBs. PDEP & PDEPC in CALC1.FOR
CRWB      pxrzd=1.0
CRWB      pcor=1.0
CRWB      qcor=1.0
CRWB      szcor=1.0
CRWB      Since 'hi' is plume height without terrain adjustment, this does
CRWB      not apply.
CRWB      if(hi .GT. 5.*sgzi) return

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
      xsrc=xsrci
      ysrc=ysrci
      xrec=xreci
      yrec=yreci
      rural=rurali
      urban=urbani
      kst=ksti
      sgz=sgzi
      sgz0=sgz0i
      szmn=szmni
      er=erin
      ep=epin
      lterr=lterri
      debug=debugi
      iounit=iouniti
      ltgrid=ltgridi
      srctyp=srctypi
      kurdat=kurdati

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
         cp=0.0
      endif

c  Flush other variables in DEPVAR common
      igrav=0


c  Set the distance at which the plume centerline reaches the surface
c  -- the touchdown distance due to gravitational settling --
c  and calculate the sigma-z at that point (sztd)
      xtd=h/(vs*onebyu)

C     Obtain Sigma-z for This Value of X = XTD
      IF (SRCTYP .EQ. 'POINT' .AND. .NOT.LTERR) THEN
C        Determine Simple Terrain Sigmas
         CALL PDIS(XTD,SGY,SZTD,XGY,XGZ,SBID)
      ELSE IF (SRCTYP .EQ. 'POINT' .AND. LTERR) THEN
C        Determine Complex Terrain Sigmas
         CALL PDISC(XTD,SZTD,XZCMP1,SBCMP1)
      ELSE IF (SRCTYP .EQ. 'VOLUME') THEN
         CALL VDIS(XTD,SGY,SZTD,XY,XZ)
      ELSE IF (SRCTYP .EQ. 'AREA') THEN
C        Calculate dispersion coefficients, SY and SZ
         CALL ADIS(XTD,SGY,SZTD,XY,XZ)
      ELSE IF (SRCTYP .EQ. 'OPENPIT') THEN
C        Calculate dispersion coefficients, SY and SZ
         CALL ADIS(XTD,SGY,SZTD,XY,XZ)
      END IF


c  Calculate the correction factor for sigma-z at this receptor
c  if x > xtd, to simulate the effect of settling on a surface-based
c  plume.
      if(x .GT. xtd) then
         call SZSETL(x,szgrav)
         szcor=szgrav/sgz
         sgz=szgrav
      endif


c  Obtain profile factor at height zd for current receptor (x=xr).
c  First, check importance of gravitational settling velocity by
c  computing vs*RESIST at the minimum of z=hmix or z=3*sgz.
c  Note: profile function for URBAN class A & B is of a different
c  form than all other classes, and is contained in PROFD2.
      zcheck=3.*sgz
      if(hmix .GT. h) zcheck=AMIN1(hmix,zcheck)
      if(vs*RESIST(zcheck) .GT. 0.1) then
         igrav=1
c  --  gravitational settling is "large", use numerical integration.
         call PROFD(pxrzd)
      elseif(urban .AND. kst .LT. 3) then
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
         write(iounit,*) '-------------------------------------------'
         write(iounit,*) '  DEPCOR Module:'
         write(iounit,*) '    x,y (source) = ',xsrc,ysrc
         write(iounit,*) '    x,y (recept) = ',xrec,yrec
         write(iounit,*) '   LTERR, LTGRID = ',lterr,ltgrid
         write(iounit,*) '  ZTERR(src), ep = ',ZTERR(0.),ep
         write(iounit,*) '  ZTERR(rec), ep = ',ZTERR(xr),er
         write(iounit,*) '  ZTERR(1/2)     = ',ZTERR(xr/2.)
         write(iounit,*) '     igrav,pxrzd = ',igrav,pxrzd
         write(iounit,*) '       pcor,qcor = ',pcor,qcor
         write(iounit,*) '-------------------------------------------'
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
c MODIFIED:    To set ndim = 12 instead of 16 for faster convergence.
c              R. W. Brode, PES, Inc. - 09/30/94
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
      parameter(ndim=12)
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
crwb      Comment out warning messge
crwb      if(ier .EQ. 2) then
crwb         write(*,*) 'WARNING from PROFD -  integration failed to'
crwb         write(*,*) 'converge to fractional error of ',eps
crwb      endif
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
      a0 = -0.5/(sgz*sgz)
      call vert(0.,sgz,a0,z,vcoup)
      fint=(1.-EXP(-arg))*VCOUP/(SGZ*2.5066283)

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
      if(sgz .LT. 2.*zd) return

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
c MODIFIED:    To prevent potential underflow and overflow conditions
c              in calculation of qcor.
c              R. W. Brode, PES, Inc. - 11/08/94
c
c MODIFIED:    To set ndim2 = 12 instead of 16 for faster convergence.
c              This corresponds to 2,049 values in the integral, and
c              gives best overall peformance based on sensitivity tests.
c              R. W. Brode, PES, Inc. - 09/30/94
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

c     Set up call to QATR2(xl,xu,eps,ndim2,fct,y,ier,num,aux2)
c     Declare parameter to fix the size of the aux2 array
      parameter(ndim2=12)
      real aux2(ndim2)
      external F2INT
      INTEGER*2 NUM
      include 'DEPVAR.INC'

c     Evaluate integral:
      eps=.05
c     Do not let integral try to evaluate sigma-z at x=0! -- start at 1m
      call QATR2(1.,xr,eps,ndim2,F2INT,value,ier,num,aux2)

crwb     comment out warning message
crwb     if(ier .EQ. 2) then
crwb         write(*,*) 'WARNING from DEPLETE -  integration failed to'
crwb         write(*,*) 'converge to fractional error of ',eps
crwb         write(iounit,*) 'WARNING from DEPLETE -  integration failed to'
crwb         write(iounit,*) 'converge to fractional error of ',eps
crwb         write(iounit,*) 'on ',kurdat,' at: ',xrec,yrec
crwb      endif

      if (vd*value .gt. 50.0) then
c        Potential underflow, limit product to 50.0
         value = 50.0/vd
      else if (vd*value .lt. -50.0) then
c        Potential overflow, limit product to 50.0
         value = -50.0/vd
      end if

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
c --- ISCST2    Version: 1.0            Level: 931215           F2INT
c               D. Strimaitis, SRC
c
c MODIFIED:    Call to ZTERR bypassed for AREA and OPENPIT sources,
c              which assume flat terrain.
c              R. W. Brode, PES, Inc. - 4/14/95
c
c MODIFIED:    Includes use of proper distance-dependent plume rise
c              and sigmas.  Also includes terrain adjustment for
c              simple terrain cases, and uses modified SUB. VERT in
c              place of FUNCTION VCOUP.
c              R. W. Brode, PES, Inc. - 9/30/94
c
c MODIFIED:    Includes call to new plume height adjustment subr.
c              to simulate COMPLEX I terrain treatment, and also
c              includes new settling treatment.
c
c PURPOSE:     Function is the integrand of integral over the travel
c              distance to obtain the fraction of material removed from
c              the plume. Common /DEPVAR/ is used to pass data that are
c              constant during the integration, so QATR (the integrator)
c              only needs to pass values of distance.
c
c ARGUMENTS:
c    PASSED:  x         distance from source                         [r]
c
c  RETURNED:  fint      value of integrand                           [r]
c
c CALLING ROUTINES:   QATR2
c
c EXTERNAL ROUTINES:  SZSETL, PROFD, PROFD1, PROD2, VERT,
c                     STERAD, CTERAD, ZTERR
c-----------------------------------------------------------------------
      include 'DEPVAR.INC'

c     Fetch terrain elevation (m MSL) for this location
crwb  Modified to bypass call to ZTERR for AREA and OPENPIT sources
      IF (SRCTYP .EQ. 'AREA' .OR. SRCTYP .EQ. 'OPENPIT') THEN
         terr = ep
      ELSE
         terr=ZTERR(x)
      END IF

C     Obtain Plume Height and Sigmas for This Value of X
      IF (SRCTYP .EQ. 'POINT' .AND. .NOT.LTERR) THEN
         CALL PHEFF(X,DHP,HEFLAT)
         CALL STERAD(HEFLAT,TERR,HX)
c        If x > xtd, simulate the effect of settling on a surface-based
c        plume by calculating a modified sigma-z.
         if(x .GT. xtd) then
            call SZSETL(x,sgz)
         else
c           Compute sigma-z in the usual way.
C           Determine Simple Terrain Sigmas
            CALL PDIS(X,SGY,SGZ,XGY,XGZ,SBID)
         end if
         COR400 = 1.0
      ELSE IF (SRCTYP .EQ. 'POINT' .AND. LTERR) THEN
         CALL PHEFFC(X,DHPCMP,HECOMP)
         CALL CTERAD(HECOMP,TERR,HX,COR400)
c        If x > xtd, simulate the effect of settling on a surface-based
c        plume by calculating a modified sigma-z.
         if(x .GT. xtd) then
            call SZSETL(x,sgz)
         else
c           Compute sigma-z in the usual way.
C           Determine Complex Terrain Sigmas
            CALL PDISC(X,SGZ,XZCMP1,SBCMP1)
         end if
      ELSE IF (SRCTYP .EQ. 'VOLUME') THEN
         CALL VHEFF(TERR,HEFLAT,HX)
c        If x > xtd, simulate the effect of settling on a surface-based
c        plume by calculating a modified sigma-z.
         if(x .GT. xtd) then
            call SZSETL(x,sgz)
         else
c           Compute sigma-z in the usual way.
C           Determine Simple Terrain Sigmas
C           Determine Dispersion Parameters
            CALL VDIS(X,SGY,SGZ,XY,XZ)
         end if
         COR400 = 1.0
      ELSE IF (SRCTYP .EQ. 'AREA') THEN
         HX = H
c        If x > xtd, simulate the effect of settling on a surface-based
c        plume by calculating a modified sigma-z.
         if(x .GT. xtd) then
            call SZSETL(x,sgz)
         else
c           Compute sigma-z in the usual way.
C           Determine Simple Terrain Sigmas
C           Calculate dispersion coefficients, SY and SZ
            CALL ADIS(X,SGY,SGZ,XY,XZ)
         end if
         COR400 = 1.0
      ELSE IF (SRCTYP .EQ. 'OPENPIT') THEN
         HX = H
c        If x > xtd, simulate the effect of settling on a surface-based
c        plume by calculating a modified sigma-z.
         if(x .GT. xtd) then
            call SZSETL(x,sgz)
         else
c           Compute sigma-z in the usual way.
C           Determine Simple Terrain Sigmas
C           Calculate dispersion coefficients, SY and SZ
            CALL ADIS(X,SGY,SGZ,XY,XZ)
         end if
         COR400 = 1.0
      END IF

c  Adjust plume height for gravitational settling
      hh=AMAX1(0.,hx-vs*x*onebyu)


c  -VCOUP calculates the vertical coupling factor:
c    (1/(sgz*SQRT(2pi)))*EXP...  (this includes multiple reflections!)
c  -PROFD1 or PROFD2 calculates the profile correction factor if
c  gravitational settling is weak (analytic representations are used);
c  -PROFD calculates the profile correction factor if gravitational
c  settling is strong (numerical integration is used).
c  -Apply "cor400" to F2INT to simulate correction that is applied
c  by COMPLEX I.
      if (cor400 .eq. 0.0) then
         f2int = 0.0
      else
         a0 = -0.5/(sgz*sgz)
         call vert(hh,sgz,a0,zd,vcoup)
         f2int=cor400*onebyu*VCOUP/(SGZ*2.5066283)
      end if
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
c MODIFIED:     To use new convergence criteria, including a lower
c               threshold in the value of the integral (1.0E-10), and
c               to check for "delta-x" < 1.0 meters (delta-x = hh).
c               R. W. Brode, PES, Inc. - 9/30/94
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
crwb        add lower threshold convergence test
            elseif(h*aux(1) .LT. 1.0e-10) then
               ier=0
               y=h*aux(1)
               return
            elseif(delt2/y .LE. eps) then
               ier=0
               y=h*aux(1)
               return
crwb        add lower limit on "delta-x"
            elseif(hh .LT. 1.0) then
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
      function zterr(x)
c-----------------------------------------------------------------------
c
c --- ISCST2    Version: 1.0            Level: 931215           ZTERR
c               D. Strimaitis, SRC
c
c PURPOSE:     Function computes the elevation (m MSL) at the position
c              "x", which is the distance downwind of the source, by
c              interpolating within field of gridded terrain elevations.
c
c ARGUMENTS:
c    PASSED:  x      distance from source to interpolation point (m) [r]
c
c  RETURNED:  zterr  value interpolated at x (m MSL)                 [r]
c
c CALLING  ROUTINES:   F2INT
c
c EXTERNAL ROUTINES:  none
c-----------------------------------------------------------------------
      include 'DEPVAR.INC'

      fract=x/xr

      if(LTGRID) then
c ---    Interpolate within gridded terrain ---

c ---    Convert the distance from source to point into (xp,yp) location
         xp=xsrc+(xrec-xsrc)*fract
         yp=ysrc+(yrec-ysrc)*fract

c ---    Set inverse of the size of a grid-cell
         di=1./sizem

c     - ll  denotes lower left corner of a grid-cell
c     - llm denotes lower left corner of grid-cell (1,1) -- this is the
c           lower left corner of the master terrain grid
c
c  Full development of the algorithm to obtain value at point xp,yp
c -- array index of lower left corner of cell that contains point
c     ixll=(xp-xllm)*di+1
c     iyll=(yp-yllm)*di+1
c -- position of lower left value
c     xll=xllm+sizem*(ixll-1)
c     yll=yllm+sizem*(iyll-1)
c -- fractional position of point within cell wrt lower left corner
c     tt=(xp-xll)*di
c     uu=(yp-yll)*di
c -- interpolated value
c     zi=(1.-tt)*(1.-uu)*zarray(ixll,iyll)
c    1     +tt*(1.-uu)*zarray(ixll+1,iyll)
c    2     +tt*uu*zarray(ixll+1,iyll+1)
c    3     +uu*(1.-tt)*zarray(ixll,iyll+1)

c ---    Compact representation:
         xpos=(xp-xllm)*di
         ixll=INT(xpos)+1
         tt=xpos-(ixll-1)
         onemt=1.-tt
         ixllp1=ixll+1
         ypos=(yp-yllm)*di
         iyll=INT(ypos)+1
         uu=ypos-(iyll-1)
         onemu=1.-uu
         iyllp1=iyll+1
         zterr=onemt*onemu*FLOAT(izarray(ixll,iyll))
     1         +tt*onemu*FLOAT(izarray(ixllp1,iyll))
     2         +tt*uu*FLOAT(izarray(ixllp1,iyllp1))
     3         +uu*onemt*FLOAT(izarray(ixll,iyllp1))

      else
c ---    Interpolate height between source and receptor

c ---    Height difference from source to receptor
         delz=er-ep

         zterr=ep+delz*fract

      endif

      return
      end

c-----------------------------------------------------------------------
      subroutine szsetl(x,sz)
c-----------------------------------------------------------------------
c
c --- ISCST2    Version: 1.0            Level: 931215           SZSETL
c               D. Strimaitis, SRC
c
c PURPOSE:     SZSETL determines the value of sigma-z at a particular
c              distance after the point at which gravitational settling
c              causes the centerline of the plume to reach the ground.
c              A default minimum of 2*zd, where zd is the near-surface
c              height at which the deposition flux is evaluated, is
c              returned if sigma-z would otherwise become LE zero.
c
c ARGUMENTS:
c    PASSED:  x         distance from source   (m)                   [r]
c             /DEPVAR/  --
c             kst       stability class                              [i]
c             zd        reference height for deposition flux (m)     [r]
c             vs        settling vel.          (m/s)                 [r]
c             onebyu    1/wind speed           (s/m)                 [r]
c             urban     logical for URBAN/RURAL dispersion params    [l]
c             xtd       distance from source at which centerline     [r]
c                       reaches the ground     (m)
c             sztd      value of sigma-z @ xtd (m)                   [r]
c             szmn      minimum allowed for "settling" sigma-z       [r]
c
c
c  RETURNED:  sz        revised value of sigma-z at x  (m)           [r]
c
c CALLING ROUTINES:     DEPCOR, F2INT
c
c EXTERNAL ROUTINES:    SZFORM1, SZFORM2, SZFORM3
c-----------------------------------------------------------------------
      include 'DEPVAR.INC'

      real car(6),cau(6),cbr(6),cbu(6)

c --- Coefficients for the Briggs Rural and Urban dispersion curves
      data car/.2,.12,.08,.06,.03,.016/
      data cbr/0.,0.,.0002,.0015,.0003,.0003/
      data cau/.24,.24,.2,.14,.08,.08/
      data cbu/.001,.001,0.,.0003,.0015,.0015/

      c=rtpiby2*vs*onebyu

c --- Urban section
      if(URBAN) then
         a=cau(kst)
         b=cbu(kst)
         if(kst .EQ. 3) then
c ---       Stability Class C
            call SZFORM1(a,b,c,xtd,sztd,x,sz)
         elseif(kst .GT. 3) then
c ---       Stability Classes D,E,F
            call SZFORM2(a,b,c,xtd,sztd,szmn,x,sz)
         elseif(kst .LT. 3) then
c ---       Stability Classes A,B
            call SZFORM4(c,xtd,sgz,x,sz)
         endif

c --- Rural section
      else
         a=car(kst)
         b=cbr(kst)
         if(kst .LT. 3) then
c ---       Stability Classes A,B
            call SZFORM1(a,b,c,xtd,sztd,x,sz)
         elseif(kst .GT. 4) then
c ---       Stability Classes E,F
            call SZFORM3(a,b,c,xtd,sztd,x,sz)
         else
c ---       Stability Classes C,D
            call SZFORM2(a,b,c,xtd,sztd,szmn,x,sz)
         endif
      endif

c --- Set minimum accepted value at 2*zd
      sz=AMAX1(sz,2.*zd)

      return
      end

c-----------------------------------------------------------------------
      subroutine szform1(a,b,c,x0,sig0,x,sz)
c-----------------------------------------------------------------------
c
c --- ISCST2    Version: 1.0            Level: 931215           SZFORM1
c               D. Strimaitis, SRC
c
c PURPOSE:     SZFORM1 determines the value of sigma-z at a particular
c              distance after the point at which gravitational settling
c              causes the centerline of the plume to reach the ground,
c              using the Briggs dispersion function of the form:
c
c              sz = a*x  (RURAL stabilities A,B ; URBAN stability C)
c
c
c ARGUMENTS:
c    PASSED:  a,b       coefficients in sz equation                  [r]
c             c         gravitational settling slope SQRT(pi/2)Vs/U  [r]
c             x0        distance from source at which centerline     [r]
c                       reaches the ground     (m)
c             sig0      value of sigma-z at x0 (m)                   [r]
c             x         distance from source   (m)                   [r]
c
c  RETURNED:  sz        revised value of sigma-z at x  (m)           [r]
c
c CALLING ROUTINES:   SZSETL
c
c EXTERNAL ROUTINES:  none
c-----------------------------------------------------------------------

      sz=sig0+(a-c)*(x-x0)
      sz=AMAX1(0.0,sz)

      return
      end

c-----------------------------------------------------------------------
      subroutine szform2(a,b,c,x0,sig0,szmn,x,sz)
c-----------------------------------------------------------------------
c
c --- ISCST2    Version: 1.0            Level: 931215           SZFORM2
c               D. Strimaitis, SRC
c
c PURPOSE:     SZFORM2 determines the value of sigma-z at a particular
c              distance after the point at which gravitational settling
c              causes the centerline of the plume to reach the ground,
c              using the Briggs dispersion function of the form:
c
c              sz = a*x/SQRT(1+bx)  (RURAL stabilities C,D
c                                    URBAN stabilities D,E,F)
c
c
c ARGUMENTS:
c    PASSED:  a,b       coefficients in sz equation                  [r]
c             c         gravitational settling slope SQRT(pi/2)Vs/U  [r]
c             x0        distance from source at which centerline     [r]
c                       reaches the ground     (m)
c             sig0      value of sigma-z at x0 (m)                   [r]
c             szmn      minimum allowed for "settling" sigma-z       [r]
c             x         distance from source   (m)                   [r]
c
c  RETURNED:  sz        revised value of sigma-z at x  (m)           [r]
c
c CALLING ROUTINES:   SZSETL
c
c EXTERNAL ROUTINES:  none
c-----------------------------------------------------------------------

c --- This algorithm provides an estimate of sigma-z, not the exact
c --- solution.  It uses the results for the limits sz >> a/b and
c --- sz << a/b, and patches these together at sz=2a/b.  Therefore, the
c --- equations used must differentiate between these two regimes.

      data feps/.01/, aeps/.1/, small/1.0e-10/, itermx/300/
c     -- feps    convergence criterion for fractional error
c     -- aeps    convergence criterion for absolute error
c     -- small   tolerance in detecting no-growth situation
c     -- itermx  max number of iterations allowed for inverting solution

      rt2=SQRT(2.)
      abyb=a/b
      abyk=a/c
      twoabyb=2.*abyb
      asqbybk=abyb*abyk
      rtabyk=SQRT(abyk)
      rt4kbya=2./rtabyk


c --- Does modified sigma change, or does d/dx(sigma)-c=0 ?
      if(sig0 .LE. twoabyb) then
         test=1.-2.*sig0/asqbybk
         if(ABS(test) .LT. small) then
c ---       Sigma does not change
            sz=sig0
            return
         endif
      else
         test=1.+sig0/twoabyb-rtabyk
         if(ABS(test) .LT. small) then
c ---       Sigma does not change
            sz=sig0
            return
         endif
      endif

c --- Set matching constants for the Large and Small sz forms, at x0:
      delsig=0.5*asqbybk*((1.-rt4kbya)*ALOG(ABS(1.-rt4kbya))
     &                +(1.+rt4kbya)*ALOG(1.+rt4kbya))
      if(sig0 .LT. twoabyb) then
         fac=1.+sig0/twoabyb
         sigs=abyb*rtabyk*ALOG(ABS((fac-rtabyk)/(fac+rtabyk)))
         sigl=sigs+delsig
      elseif(sig0 .GE. twoabyb) then
         sigl=0.5*asqbybk*ALOG(ABS(1.-2.*sig0/asqbybk))
         sigs=sigl-delsig
      endif

c     write(*,*) 'sigl, sigs   =  ',sigl,sigs

c --- Iterate to find sz for current value of x
      sz=szmn
      if(szmn .GT. 0.) sz=sig0
      szlast=sz
      icount=0

1     if(sz .GE. twoabyb) then
c ---    (Large sz form)
         if(sz .GE. asqbybk) then
c ---       (LOG form of iteration)
            sz=sig0+sigl-c*(x-x0)
     &         -0.5*asqbybk*ALOG(ABS(1.-2.*sz/asqbybk))
         else
c ---       (EXP form of iteration)
            expterm=EXP(-2.*(sz-sig0-sigl+c*(x-x0))/asqbybk)
            test=2.*sz/asqbybk
            if(test .LT. 1.) then
               sz=0.5*asqbybk*(1.-expterm)
            else
               sz=0.5*asqbybk*(1.+expterm)
            endif
         endif
      else
c ---    (Small sz form)
         fac=1.+sz/twoabyb
         if(fac .GE. rt2*rtabyk) then
c ---       (LOG form of iteration)
            sz=sig0+sigs-c*(x-x0)
     &         -abyb*rtabyk*ALOG(ABS(fac-rtabyk)/(fac+rtabyk))
         else
c ---       (EXP form of iteration)
            expterm=EXP(-(sz-sig0-sigs+c*(x-x0))/(abyb*rtabyk))
            test=fac-rtabyk
            if(test .LE. 1.) then
               sz=twoabyb*(rtabyk*(1.-expterm)/(1.+expterm)-1.)
            else
               sz=twoabyb*(rtabyk*(1.+expterm)/(1.-expterm)-1.)
            endif
         endif
      endif

      icount=icount+1
      if(icount .LE. itermx) then
         err=ABS(sz-szlast)
         if(szlast .NE. 0.0) then
            ferr=ABS(err/szlast)
         else
            ferr=ABS(err/sz)
         endif
         if(ferr .GT. feps .AND. err .GT. aeps) then
            szlast=sz
            goto 1
         endif
      else
crwb         Comment out warning message
crwb         write(*,*)
crwb         write(*,*) 'SZFORM2 FAILED -- FATAL'
crwb         write(*,*) 'iterations, sz   =  ',icount,sz
crwb         write(*,*) 'a, b, c          =  ',a,b,c
crwb         write(*,*) 'xtd, sztd        =  ',x0,sig0
      endif

c     write(*,*) 'iterations, sz     =  ',icount,sz

      sz=AMAX1(0.0,sz)

      return
      end

c-----------------------------------------------------------------------
      subroutine szform3(a,b,c,x0,sig0,x,sz)
c-----------------------------------------------------------------------
c
c --- ISCST2    Version: 1.0            Level: 931215           SZFORM3
c               D. Strimaitis, SRC
c
c PURPOSE:     SZFORM3 determines the value of sigma-z at a particular
c              distance after the point at which gravitational settling
c              causes the centerline of the plume to reach the ground,
c              using the Briggs dispersion function of the form:
c
c              sz = a*x/(1+bx)  (RURAL stabilities E,F)
c
c
c ARGUMENTS:
c    PASSED:  a,b       coefficients in sz equation                  [r]
c             c         gravitational settling slope SQRT(pi/2)Vs/U  [r]
c             x0        distance from source at which centerline     [r]
c                       reaches the ground     (m)
c             sig0      value of sigma-z at x0 (m)                   [r]
c             x         distance from source   (m)                   [r]
c
c  RETURNED:  sz        revised value of sigma-z at x  (m)           [r]
c
c CALLING ROUTINES:   SZSETL
c
c EXTERNAL ROUTINES:  none
c-----------------------------------------------------------------------

      rtak=SQRT(a*c)
      ambs0=a-b*sig0
      gamx=((ambs0-rtak)/(ambs0+rtak))*EXP(-2.*b*rtak*(x-x0)/a)
      sz=(a-rtak*(1.+gamx)/(1.-gamx))/b
      sz=AMAX1(0.0,sz)

      return
      end

c-----------------------------------------------------------------------
      subroutine szform4(c,x0,sgz,x,sz)
c-----------------------------------------------------------------------
c
c --- ISCST2    Version: 1.0            Level: 931215           SZFORM4
c               D. Strimaitis, SRC
c
c PURPOSE:     SZFORM4 determines the value of sigma-z at a particular
c              distance after the point at which gravitational settling
c              causes the centerline of the plume to reach the ground,
c              using the Briggs dispersion function of the form:
c
c              sz = a*x*SQRT(1+bx)  (URBAN stabilities A,B)
c
c    NOTE:     This is an interim treatment that merely subtracts the
c              "fall" due to gravitational settling from the sigma-z
c              at the receptor computed in the absence of settling.
c
c ARGUMENTS:
c    PASSED:  c         gravitational settling slope SQRT(pi/2)Vs/U  [r]
c             x0        distance from source at which centerline     [r]
c                       reaches the ground     (m)
c             sgz       current value of sigma-z at x (m)            [r]
c             x         distance from source   (m)                   [r]
c
c  RETURNED:  sz        revised value of sigma-z at x  (m)           [r]
c
c CALLING ROUTINES:   SZSETL
c
c EXTERNAL ROUTINES:  none
c-----------------------------------------------------------------------
      sz=sgz-(x-x0)*c
      sz=AMAX1(0.0,sz)

      return
      end
c-------------------------------------------------------------------------
      subroutine otput(icode,chis,rcompr,itap)
c-------------------------------------------------------------------------
c     SUBROUTINE OUTPUT(ICODE,CHIS,NREC,RCOMPR)                         BLP16300
C                    SUBROUTINE OUTPUT (VERSION 82102), PART OF BLP.    BLP16310
C      BLP   VERSION 4.1  LEVEL 820412     OUTPUT                       BLP16320
C      modified for use in ISCST2, 050692 RCM
C                                                                       BLP16360
C     THIS SUBROUTINE OUTPUTS ALL CHI ARRAYS TO TAPE (OR DISK)          BLP16370
C                                                                       BLP16380
C     ICODE IDENTIFIES THE CHI ARRAY TO FOLLOW:                         BLP16390
C                                                                       BLP16400
C     ICODE = 9999 IMPLIES THE CHI ARRAY IS THE TOTAL                   BLP16540
C     CONCENTRATION SUMMED OVER ALL THE POINT AND LINE SOURCES AT       BLP16550
C     EACH RECEPTOR                                                     BLP16560
c
c     CALLED BY : POSTFL
c     CALLS     : COMPRS
c-------------------------------------------------------------------------
      include 'MAIN1.INC'
      REAL CHIS(NREC)                                                   BLP16330
      LOGICAL RCOMPR                                                    BLP16340
ccccc COMMON/METD/ZMEAS,WS,WD,ISTAB,TDEGK,DPBL,THETA,S,P,IYR,JDAY,IHOUR BLP16350
      idayhr = jday*100 + ihr
C     ROUND THE DPBL (NEAREST METER)
      idpbl = hm + 0.5
      idpbl = MIN0(idpbl,9999)
      icd = icode
      imet2=ist*10000+idpbl                                             BLP16640
      IF(RCOMPR)GO TO 10                                                BLP16650
      WRITE(ITAP)IDAYHR,ICD,IMET2,ubar,wd2,wd4,CHIS
      RETURN                                                            BLP16670
10    CONTINUE                                                          BLP16680
      CALL COMPRS(IDAYHR,ICD,IMET2,CHIS,itap)
      RETURN                                                            BLP16700
      END                                                               BLP16710

c-------------------------------------------------------------------------
      subroutine out(idayhr,icd,imet2,ubar,wd2,wd4,ii,chiout,itap)
c-------------------------------------------------------------------------
C                    SUBROUTINE OUT (VERSION 82102), PART OF BLP.       BLP22210
C      BLP   VERSION 4.1  LEVEL 820412     OUT                          BLP22220
C      modified for use in ISCST2, 050692 RCM
c
c      CALLED BY : COMPRS
c-------------------------------------------------------------------------
      REAL CHIOUT(II)                                                   BLP22230
      WRITE(ITAP)IDAYHR,ICD,IMET2,ubar,wd2,wd4,CHIOUT
      RETURN                                                            BLP22250
      END                                                               BLP22260

c-------------------------------------------------------------------------
      subroutine comprs(idayhr,icd,imet2,chis,itap)
c-------------------------------------------------------------------------
C                    SUBROUTINE COMPRS (VERSION 82102), PART OF BLP.    BLP21870
C      BLP   VERSION 4.1  LEVEL 820412     COMPRS                       BLP21880
C      modified for use in ISCST2, 050692 RCM
cvrt   Modified to use the same parameter NREC to define the
cvrt    maximum number of receptors to use.
C                                                                       BLP21900
C     ARRAY COMPRESSION TECHNIQUE USES NEGATIVE NUMBERS TO FLAG ZEROES  BLP21910
C     FOR EXAMPLE, CHIS=12.5, 12.2, 0.0, 0.0, 0.0, 10.1, 0.0, 15.1,     BLP21920
C     16.7, 0.0, 0.0, 0.0, 0.0, 0.0 IS STORED AS:                       BLP21930
C     CHIOUT=12.5, 12.2, -3., 10.1, -1., 15.1, 16.7, -5.                BLP21940
C     WHERE -3 REPLACES THREE ZEROES, -1 REPLACES ONE ZERO, ETC.        BLP21950
C                                                                       BLP21960
c     CALLED BY : OTPUT
c     CALLS     : OUT
c
c-------------------------------------------------------------------------

      include 'MAIN1.INC'
cvrt -- I don't know why this was left in when NREC was added
cvrt      parameter(maxrec=999)
cvrt      REAL CHIS(NREC),CHIOUT(maxrec)
      REAL CHIS(NREC),CHIOUT(NREC)
      NZERO=0                                                           BLP21970
      II=0                                                              BLP21980
      DO 100 I=1,NumREC                                                   BLP21990
      IF(CHIS(I).NE.0.0)GO TO 50                                        BLP22000
      NZERO=NZERO+1                                                     BLP22010
      GO TO 100                                                         BLP22020
50    CONTINUE                                                          BLP22030
      IF(NZERO.EQ.0)GO TO 70                                            BLP22040
      II=II+1                                                           BLP22050
      CHIOUT(II)=-NZERO                                                 BLP22060
      NZERO=0                                                           BLP22070
70    CONTINUE                                                          BLP22080
      II=II+1                                                           BLP22090
      CHIOUT(II)=CHIS(I)                                                BLP22100
100   CONTINUE                                                          BLP22110
      IF(NZERO.EQ.0)GO TO 105                                           BLP22120
      II=II+1                                                           BLP22130
      CHIOUT(II)=-NZERO                                                 BLP22140
105   CONTINUE                                                          BLP22150
      WRITE(ITAP)II                                                     BLP22160
      call out(idayhr,icd,imet2,ubar,wd2,wd4,ii,chiout,itap)
      RETURN                                                            BLP22180
      END                                                               BLP22190
c-------------------------------------------------------------------------
      SUBROUTINE OUTITL(TITLE,nmREC,NPTS,NLINES,IYR,IDAYS,
     1 rcompr,itap,tk)
c-------------------------------------------------------------------------
cccc  SUBROUTINE OUTITL(TITLE,nmREC,NPTS,NLINES,IPCL,IPCP,IYR,IDAYS,    BLP08320
c    1 RCOMPR)                                                          BLP08330
C                    SUBROUTINE OUTITL (VERSION 82102), PART OF BLP.    BLP08340
C      BLP   VERSION 4.1  LEVEL 820412     OUTITL                       BLP08350
C      modified for use in ISCST2, 050692 RCM
c*****
c **** Modified header of output to include RCOMPR logical to indicate
c **** whether or not the output file is compressed, thus removing the
c **** restriction of having no more than 999 receptors in a run.
c **** EMI 931118
c*****
c     REAL TITLE(20)                                                    BLP08360
C                                                                       BLP08400
C     THIS SUBROUTINE WRITES THE TITLE CARD AND OTHER RUN               BLP08410
C     INFORMATION TO RECORD #1 OF THE OUTPUT FILE (ITAP)                BLP08420
C                                                                       BLP08430
c **** This is No Longer True **************************************
C *    THOUSANDS PLACE OF NNREC IS CODED TO INDICATE IF ARRAY      *     BLP08440
C *    COMPRESSION OPTION IS USED                                  *     BLP08450
C *    IF NNREC > 1000, OUTPUT ARRAYS ARE COMPRESSED               *    BLP08460
C *    IF NNREC < 1000, OUTPUT ARRAYS ARE NOT COMPRESSED           *    BLP08470
c ******************************************************************
c     CALLED BY : OUPOST
c
c-------------------------------------------------------------------------
      include 'MAIN1.INC'

      character*4 title(17),titleout(20)
      INTEGER IPCL(mxlinp1),IPCP(mxpntp1)
      DIMENSION IDAYS(366)                                              BLP08380
      LOGICAL RCOMPR                                                    BLP08390
      data ipcl/mxlinp1*0/,ipcp/mxpntp1*0/
      do 10 i=1,17
      titleout(i)=title(i)
10    continue
      titleout(20)='    '
      NNREC=NmREC                                                        BLP08480
c**** The following convention is no longer used -- EMI 931118
c**** IF(RCOMPR)NNREC=NNREC+1000                                        BLP08490
c**** WRITE(ITAP)titleout,NNREC,NPTS,NLINES,IPCL,IPCP,IYR,IDAYS,tk
      WRITE(ITAP)titleout,rcompr,NNREC,NPTS,NLINES,IPCL,IPCP,IYR,
     &           IDAYS,tk
      RETURN                                                            BLP08510
      END                                                               BLP08520
C
c-------------------------------------------------------------------------
      subroutine outrecp(itap)
c-------------------------------------------------------------------------
c --- Modified by E. Insley   2/22/96
c --- Removed the ORIGIN from the binary output file header record and .
c --- added the TCOR,XOR,YOR variables for use in POSTBLP.
c ---
cccc  subroutine outrecp(nxpnts,nypnts,gridx,gridy,iflag1,nxwypt,xdis,
cccc +           ydis,iflag2,itap)
C      modified for use in ISCST2, 050692 RCM
c
c *** This subroutine writes out receptor data to the hourly binary
c *** blp-formatted file. Polar coordinates are converted to rectangular
c *** coordinates.
c
c    CALLED BY : OUPOST
c-------------------------------------------------------------------------
c include files......
      include 'MAIN1.INC'
c---  Define variables for writing to binary data file which will cause
c---  no rotation or translation of the receptor coordinates in POSTBLP
      tcor = 90.
      xor = 0.0
      yor = 0.0
c***  write(itap)xorig(1),yorig(1),(axr(i),i=1,numrec),
      write(itap) tcor,xor,yor,(axr(i),i=1,numrec),
     1           (ayr(i),i=1,numrec)
      return
      end
      SUBROUTINE PERAVE
C***********************************************************************
C                 PERAVE Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Calculates PERIOD Averages
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   Removed 75 percent limit on calculation of the
C                    denominator, SNUM - 4/19/93
C
C        INPUTS:  Array of Period Sums and Counters
C
C        OUTPUTS: Array of Period Averages
C
C        CALLED FROM:   MAIN
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'PERAVE'

C     Calculate Denominator Considering Calms and Missing
      SNUM = IANHRS - IANCLM - IANMSG

C     Calculate Period Average Concentrations for Each Source Group and Receptor

C     Begin Source Group LOOP
      DO 200 IGRP = 1, NUMGRP
C        Begin Receptor LOOP
         DO 100 IREC = 1, NUMREC

            ANNVAL(IREC,IGRP,1) = (1./SNUM) * ANNVAL(IREC,IGRP,1)

 100     CONTINUE
C        End Receptor LOOP
 200  CONTINUE
C     End Source Group LOOP

      RETURN
      END

      SUBROUTINE ANNAVE
C***********************************************************************
C                 ANNAVE Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Calculates ANNUAL Averages for Deposition Rates
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Array of Period Sums and Counters
C
C        OUTPUTS: Array of Period Averages
C
C        CALLED FROM:   MAIN
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'ANNAVE'

C     First Convert Year to 4-Digit Value
      IF (IYEAR .GE. 50 .AND. IYEAR .LE. 99) THEN
         IYR = 1900 + IYEAR
      ELSE IF (IYEAR .LT. 50) THEN
         IYR = 2000 + IYEAR
      END IF

C     Calculate Denominator Considering Calms and Missing
      IF (IANHRS .LE. 8784) THEN
C        Set demoninator for single year or portion of a year
         IF ((MOD(IYR,4) .NE. 0) .OR.
     &       (MOD(IYR,100).EQ.0 .AND. MOD(IYR,400).NE.0)) THEN
C           Not a Leap Year
            IF (IANHRS .LT. 8760) THEN
               SNUMYR = IANHRS/8760.
            ELSE
               SNUMYR = 1.0
            END IF
         ELSE
C           Leap Year
            IF (IANHRS .LT. 8764) THEN
               SNUMYR = IANHRS/8784.
            ELSE
               SNUMYR = 1.0
            END IF
         END IF
      ELSE
C        Set denominator for multiple (whole) years
         SNUMYR = FLOAT( NINT(IANHRS/8760.) )
      END IF

C     Calculate Period Averages for Each Source Group and Receptor

C     Begin LOOP Over Output Types
      DO 300 ITYP = 1, NUMTYP
         IF (CONC .AND. ITYP .EQ. 1) THEN
C           Skip ITYP = 1
            GO TO 300
         ELSE
C           Begin Source Group LOOP
            DO 200 IGRP = 1, NUMGRP
C              Begin Receptor LOOP
               DO 100 IREC = 1, NUMREC

                  ANNVAL(IREC,IGRP,ITYP) = (1./SNUMYR) *
     &                                      ANNVAL(IREC,IGRP,ITYP)

 100           CONTINUE
C              End Receptor LOOP
 200        CONTINUE
C           End Source Group LOOP
         END IF
 300  CONTINUE
C     End LOOP Over Output Types

      RETURN
      END

      SUBROUTINE HIPER
C***********************************************************************
C                 HIPER Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Selects Highest PERIOD Average Values
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Array of Period Averages
C
C        OUTPUTS: Array of Highest Period Averages By Source Group
C
C        CALLED FROM:   MAIN
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'HIPER'

C     Begin Source Group LOOP
      DO 500 IGRP = 1, NUMGRP
C        Begin Receptor LOOP
         DO 400 IREC = 1, NUMREC
            IF (NVAL .GT. 1) THEN
               IF (ANNVAL(IREC,IGRP,ITYP) .GT.
     &             AMXVAL(NVAL,IGRP,ITYP)) THEN
                  DO 100 J = NVAL-1, 1, -1
                     IF (ANNVAL(IREC,IGRP,ITYP) .LE.
     &                      AMXVAL(J,IGRP,ITYP))THEN
                        AMXVAL(J+1,IGRP,ITYP) = ANNVAL(IREC,IGRP,ITYP)
                        IMXLOC(J+1,IGRP,ITYP) = IREC
C                       Exit Block
                        GO TO 200
                     ELSE
                        AMXVAL(J+1,IGRP,ITYP) = AMXVAL(J,IGRP,ITYP)
                        IMXLOC(J+1,IGRP,ITYP) = IMXLOC(J,IGRP,ITYP)
                        IF (J .EQ. 1) THEN
                           AMXVAL(1,IGRP,ITYP) = ANNVAL(IREC,IGRP,ITYP)
                           IMXLOC(1,IGRP,ITYP) = IREC
                        END IF
                     END IF
 100              CONTINUE
               END IF
            ELSE IF (NVAL .EQ. 1) THEN
               IF (ANNVAL(IREC,IGRP,ITYP) .GT. AMXVAL(1,IGRP,ITYP)) THEN
                  AMXVAL(1,IGRP,ITYP) = ANNVAL(IREC,IGRP,ITYP)
                  IMXLOC(1,IGRP,ITYP) = IREC
               END IF
            END IF
 200        CONTINUE
 400     CONTINUE
C        End Receptor LOOP
 500  CONTINUE
C     End Source Group LOOP

      IF (MULTYR) THEN
C        Dump Results Arrays to SAVFIL                      ---   CALL RSDUMP
         CALL RSDUMP
      END IF

      RETURN
      END

      SUBROUTINE PSTANN
C***********************************************************************
C                 PSTANN Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process Postprocessor Files for PERIOD Results
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED BY D. Strimaitis, SRC (for Wet & Dry DEPOSITION) - 11/8/93
C
C        INPUTS:  Array of High Values
C
C        OUTPUTS: File of High Values for Postprocessing
C
C        CALLED FROM:   MAIN
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER PERCHR*6, HDRFRM*256

C     Variable Initializations
      MODNAM = 'PSTANN'
      IF (PERIOD) THEN
         PERCHR = 'PERIOD'
      ELSE IF (ANNUAL) THEN
         PERCHR = 'ANNUAL'
      END IF

C     Create Header Format for Columns
      WRITE(HDRFRM,9020) NUMTYP, NUMTYP+2

C     Begin Source Group LOOP
      DO 500 IGRP = 1, NUMGRP
C        Check for Selection of PERIOD POSTFILE for This Group
         IF (IANPST(IGRP) .EQ. 1) THEN
            IF (IANFRM(IGRP) .EQ. 0) THEN
C              WRITE Results to Unformatted POSTFILE
               WRITE(IAPUNT(IGRP),ERR=99) KURDAT, IANHRS,
     &            GRPID(IGRP), ((ANNVAL(IREC,IGRP,ITYP),IREC=1,NUMREC),
     &                           ITYP=1,NUMTYP)
            ELSE
C              WRITE Results to Formatted Plot File
C              Write Header Information
               WRITE(IAPUNT(IGRP),9005) VERSN, TITLE1
               WRITE(IAPUNT(IGRP),9007) (MODOPS(I),I=1,17)
               WRITE(IAPUNT(IGRP),9010) PERCHR,GRPID(IGRP),NUMREC,PSTFRM
              WRITE(IAPUNT(IGRP),HDRFRM) (CHIDEP(1,ITYP),CHIDEP(2,ITYP),
     &                                   CHIDEP(3,ITYP),ITYP=1,NUMTYP)
C              Begin Receptor LOOP
               DO 300 IREC = 1, NUMREC
                  WRITE(IAPUNT(IGRP),PSTFRM,ERR=99)
     &               AXR(IREC), AYR(IREC), (ANNVAL(IREC,IGRP,ITYP),
     &                                      ITYP=1,NUMTYP),
     &               AZELEV(IREC), PERCHR, GRPID(IGRP), IANHRS,
     &               NETID(IREC)
 300           CONTINUE
C              End Receptor LOOP
            END IF
         END IF
 500  CONTINUE
C     End Source Group LOOP

      GO TO 999

C     WRITE Error Message for Problem Writing to Postprocessor File
 99   WRITE(DUMMY,'(5HPSTFL,I3.3)') IAPUNT(IGRP)
      CALL ERRHDL(PATH,MODNAM,'E','520',DUMMY)

 9005 FORMAT('* ISC3P  (',A5,'): ',A68)
 9007 FORMAT('* MODELING OPTIONS USED:',/'* ',17(1X,A6))
 9010 FORMAT('*',9X,'POST/PLOT FILE OF ',A6,' VALUES FOR ',
     &       'SOURCE GROUP: ',A8,
     &      /'*',9X,'FOR A TOTAL OF ', I5,' RECEPTORS.',
     &      /'*',9X,'FORMAT: ',A60)
 9020 FORMAT('(''*'',8X,''X'',13X,''Y'',4X,',I1,'(2X,3A4),3X,''ZELEV'',
     &  5X,''AVE'',5X,''GRP'',6X,''NUM HRS'',3X,''NET ID'',/,''*'',2X,'
     & ,I1,'(''___________'',3X),''______  ______  ________  ________'',
     &  ''  ________'')')

 999  RETURN
      END

      SUBROUTINE PLTANN
C***********************************************************************
C                 PLTANN Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process Files To Plot Annual (i.e. PERIOD) Results
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED BY D. Strimaitis, SRC (for Wet & Dry DEPOSITION) - 11/8/93
C
C        INPUTS:  Array of High Values
C
C        OUTPUTS: File of High Values for Plotting
C
C        CALLED FROM:   MAIN
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER PERCHR*6, HDRFRM*256

C     Variable Initializations
      MODNAM = 'PLTANN'
      IF (PERIOD) THEN
         PERCHR = 'PERIOD'
      ELSE IF (ANNUAL) THEN
         PERCHR = 'ANNUAL'
      END IF

C     Create Header Format for Columns Based on Number of Output Types
      WRITE(HDRFRM,9020) NUMTYP, NUMTYP+2

C     Begin Source Group LOOP
      DO 500 IGRP = 1, NUMGRP
C        Check for Selection of PERIOD PLOTFILE for This Group
         IF (IANPLT(IGRP) .EQ. 1) THEN
C           Write Header Information
            WRITE(IPPUNT(IGRP),9005) VERSN, TITLE1
            WRITE(IPPUNT(IGRP),9007) (MODOPS(I),I=1,17)
            WRITE(IPPUNT(IGRP),9010) PERCHR, GRPID(IGRP), NUMREC, PSTFRM
            WRITE(IPPUNT(IGRP),HDRFRM) (CHIDEP(1,ITYP),CHIDEP(2,ITYP),
     &                                CHIDEP(3,ITYP),ITYP=1,NUMTYP)
C           Begin Receptor LOOP
            DO 300 IREC = 1, NUMREC
               WRITE(IPPUNT(IGRP),PSTFRM,ERR=99)
     &            AXR(IREC), AYR(IREC), (ANNVAL(IREC,IGRP,ITYP),
     &            ITYP=1,NUMTYP),AZELEV(IREC),PERCHR,GRPID(IGRP),IANHRS,
     &            NETID(IREC)
 300        CONTINUE
C           End Receptor LOOP
         END IF
 500  CONTINUE
C     End Source Group LOOP

      GO TO 999

C     WRITE Error Message for Problem Writing to Plot File
 99   WRITE(DUMMY,'(5HPLTFL,I3.3)') IPPUNT(IGRP)
      CALL ERRHDL(PATH,MODNAM,'E','520',DUMMY)

 9005 FORMAT('* ISC3P  (',A5,'): ',A68)
 9007 FORMAT('* MODELING OPTIONS USED:',/'* ',17(1X,A6))
 9010 FORMAT('*',9X,'PLOT FILE OF ',A6,' VALUES FOR ',
     &       'SOURCE GROUP: ',A8,
     &      /'*',9X,'FOR A TOTAL OF ', I5,' RECEPTORS.',
     &      /'*',9X,'FORMAT: ',A60)
 9020 FORMAT('(''*'',8X,''X'',13X,''Y'',4X,',I1,'(2X,3A4),3X,''ZELEV'',
     &  5X,''AVE'',5X,''GRP'',6X,''NUM HRS'',3X,''NET ID'',/,''*'',2X,'
     & ,I1,'(''___________'',3X),''______  ______  ________  ________'',
     &  ''  ________'')')
C 9020 FORMAT(23H('*',8X,'X',13X,'Y',4X,,I1,23H(2X,3A4),3X,'ZELEV',5X,
C     &,36H'AVE',5X,'GRP',6X,'NUM HRS',/'*',2X,,I1,19H('___________',3X),
C     &     ,37H'______  ______  ________  ________'))

 999  RETURN
      END

      SUBROUTINE PLOTFL
C***********************************************************************
C                 PLOTFL Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process Files To Plot
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED BY D. Strimaitis, SRC (for Wet & Dry DEPOSITION) - 11/8/93
C
C        INPUTS:  Array of High Values
C
C        OUTPUTS: File of High Values for Plotting
C
C        CALLED FROM:   MAIN
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER NCHR2(10)*4, HDRFRM*256

C     Variable Initializations
      DATA (NCHR2(I),I=1,10) /'1ST','2ND','3RD','4TH','5TH','6TH',
     &                        '7TH','8TH','9TH','10TH'/
      MODNAM = 'PLOTFL'

C     Create Header Format for Columns
      WRITE(HDRFRM,9020) NUMTYP, NUMTYP+2

C     Begin Averaging Period LOOP
      DO 1000 IAVE = 1, NUMAVE
C        Begin Source Group LOOP
         DO 500 IGRP = 1, NUMGRP
C           Begin High Value LOOP
            DO 400 IVAL = 1, NHIVAL
C              Decide if we should go through the processing
               IF (IPLTFL(IVAL,IGRP,IAVE) .EQ. 1) THEN
C                 Write Header Information
                  WRITE(IPLUNT(IVAL,IGRP,IAVE),9005) VERSN, TITLE1
                  WRITE(IPLUNT(IVAL,IGRP,IAVE),9007) (MODOPS(I),I=1,17)
                  WRITE(IPLUNT(IVAL,IGRP,IAVE),9010) NCHR2(IVAL),
     &                    CHRAVE(IAVE), GRPID(IGRP), NUMREC, PLTFRM
                  WRITE(IPLUNT(IVAL,IGRP,IAVE),HDRFRM) (CHIDEP(1,ITYP),
     &                      CHIDEP(2,ITYP),CHIDEP(3,ITYP),ITYP=1,NUMTYP)
C                 Begin Receptor LOOP
                  DO 300 IREC = 1, NUMREC
                     WRITE(IPLUNT(IVAL,IGRP,IAVE),PLTFRM,ERR=99)
     &                AXR(IREC), AYR(IREC), (HIVALU(IREC,IVAL,IGRP,IAVE,
     &                ITYP),ITYP=1,NUMTYP),
     &                AZELEV(IREC),CHRAVE(IAVE),GRPID(IGRP),NCHR2(IVAL),
     &                NETID(IREC)
 300              CONTINUE
C                 End Receptor LOOP
               END IF
 400        CONTINUE
 500     CONTINUE
C        End Source Group LOOP
 1000 CONTINUE
C     End Averaging Period LOOP

      GO TO 999

C     WRITE Error Message for Problem Writing to Plot File
 99   WRITE(DUMMY,'(5HPLTFL,I3.3)') IPLUNT(IVAL,IGRP,IAVE)
      CALL ERRHDL(PATH,MODNAM,'E','520',DUMMY)

 9005 FORMAT('* ISC3P  (',A5,'): ',A68)
 9007 FORMAT('* MODELING OPTIONS USED:',/'* ',17(1X,A6))
 9010 FORMAT('*',9X,'PLOT FILE OF  HIGH ',A4,' HIGH ',A5,
     &       ' VALUES FOR SOURCE GROUP: ',A8,
     &      /'*',9X,'FOR A TOTAL OF ',I5,' RECEPTORS.',
     &      /'*',9X,'FORMAT: ',A60)
 9020 FORMAT('(''*'',8X,''X'',13X,''Y'',4X,',I1,'(2X,3A4),3X,''ZELEV'',
     &  5X,''AVE'',5X,''GRP'',7X,''HIVAL'',4X,''NET ID'',/,''*'',2X,',
     &  I1,'(''___________'',3X),''______  ______  ________  ________'',
     &  ''  ________'')')

 999  RETURN
      END

      SUBROUTINE OUTPUT
C***********************************************************************
C                 OUTPUT Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Controls Output of Printed Model Results
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Arrays of Source Parameters
C                 Arrays of Receptor Locations
C                 Arrays of Model Results
C
C        OUTPUTS: Printed Model Outputs
C
C        CALLED FROM:   MAIN
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'OUTPUT'
      PATH = 'OU'

      IF (PERIOD .OR. ANNUAL) THEN
         DO ITYP = 1, NUMTYP
C           Print Out Summary of Period Averages            ---   CALL PRTANN
            CALL PRTANN
         END DO
      END IF
      IF (NHIVAL .GT. 0) THEN
         DO ITYP = 1, NUMTYP
C           Print Out Summary of High Values by Receptor    ---   CALL PRTNHI
            CALL PRTNHI
         END DO
      END IF
      IF (NMXVAL .GT. 0) THEN
         DO ITYP = 1, NUMTYP
C           Print Out Summary of Overall Maximum Values     ---   CALL PRTMAX
            CALL PRTMAX
         END DO
      END IF

      IF (PERIOD .OR. ANNUAL .OR. NHIVAL .GT. 0) THEN
         DO ITYP = 1, NUMTYP
C           Generate The Summary Result                     ---   CALL PRTSUM
            CALL PRTSUM
         END DO
      END IF

C     Generate The EVENT Input File                         ---   CALL EVEFIL
      IF (EVENTS) THEN
         CALL EVEFIL
      END IF

      RETURN
      END

      SUBROUTINE PRTANN
C***********************************************************************
C                 PRTANN Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Print Out The Annual Average Data
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To adjust format statement 9082 for BOUNDARY receptors
C                    to better accommodate UTM coordinates - 9/29/92
C
C        INPUTS:  Arrays of Source Parameters
C                 Arrays of Receptor Locations
C                 Arrays of Model Results
C
C        OUTPUTS: Printed Model Outputs
C
C        CALLED FROM:   OUTPUT
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER CHRVAL*6, BUF132*132

C     Variable Initializations
      MODNAM = 'PRTANN'
      IF (PERIOD) THEN
         CHRVAL = 'PERIOD'
      ELSE IF (ANNUAL) THEN
         CHRVAL = 'ANNUAL'
      END IF

      DO 1000 IGRP = 1, NUMGRP

C        Fill Work Array With SRCIDs For This Group
         INDGRP = 0
         DO 210 ISRC = 1, NUMSRC
            IF (IGROUP(ISRC,IGRP) .EQ. 1) THEN
               INDGRP = INDGRP + 1
               WORKID(INDGRP) = SRCID(ISRC)
            END IF
 210     CONTINUE
C        Check for More Than 31 Sources Per Group
         IF (INDGRP .GT. 31) THEN
            WORKID(31) = ' . . . '
            INDGRP = 31
         END IF

C        Print Receptor Network Coordinates:
C        Set Number of Columns Per Page, NCPP
         NCPP = 9
C        Set Number of Rows Per Page, NRPP
         NRPP = 40
C        Begin LOOP Through Networks
         DO 50 I = 1, INNET
C           Calculate Number of Pages Per X-Group, NPPX, & Per Y-Group, NPPY
            NPPX = 1 + INT((NUMXPT(I)-1)/NCPP)
            NPPY = 1 + INT((NUMYPT(I)-1)/NRPP)
            DO 40 NX = 1, NPPX
               DO 30 NY = 1, NPPY
                  CALL HEADER
                  WRITE(IOUNIT,9032) CHRVAL, IANHRS,
     &     (CHIDEP(II,ITYP),II=1,6),GRPID(IGRP),(WORKID(K),K = 1,INDGRP)
                  WRITE(IOUNIT,9037) NTID(I), NTTYP(I)
C                 Print The Values By Source Group
                  WRITE(IOUNIT,9011) CHIDEP(3,ITYP), POLLUT,OUTLBL(ITYP)
                  IF (NX .EQ. NPPX) THEN
                     IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                        WRITE(IOUNIT,9016)
                        WRITE(IOUNIT,9017) (XCOORD(J,I),J=1+NCPP*(NX-1),
     &                                                    NUMXPT(I))
                     ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                        WRITE(IOUNIT,9018)
                        WRITE(IOUNIT,9019) (XCOORD(J,I),J=1+NCPP*(NX-1),
     &                                                    NUMXPT(I))
                     END IF
                  ELSE
                     IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                        WRITE(IOUNIT,9016)
                        WRITE(IOUNIT,9017) (XCOORD(J,I),J=1+NCPP*(NX-1),
     &                                                    NCPP*NX)
                     ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                        WRITE(IOUNIT,9018)
                        WRITE(IOUNIT,9019) (XCOORD(J,I),J=1+NCPP*(NX-1),
     &                                                    NCPP*NX)
                     END IF
                  END IF
                  WRITE(IOUNIT,9010)
                  IF (NY .EQ. NPPY) THEN
                     DO 20 K = 1+NRPP*(NY-1), NUMYPT(I)
                        IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                           INDZ = NETEND(I) - K*NUMXPT(I) + 1
                           YCOVAL = YCOORD(NUMYPT(I)-K+1,I)
                        ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                           INDZ = NETSTA(I) + (K-1)*NUMXPT(I)
                           YCOVAL = YCOORD(K,I)
                        END IF
                        IF (NX .EQ. NPPX) THEN
                           WRITE(IOUNIT,9013) YCOVAL,
     &            (ANNVAL(INDZ+J-1,IGRP,ITYP),J=1+NCPP*(NX-1),NUMXPT(I))
                        ELSE
                           WRITE(IOUNIT,9013) YCOVAL,
     &              (ANNVAL(INDZ+J-1,IGRP,ITYP),J=1+NCPP*(NX-1),NCPP*NX)
                        END IF
 20                  CONTINUE
                  ELSE
                     DO 25 K = 1+NRPP*(NY-1), NRPP*NY
                        IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                           INDZ = NETEND(I) - K*NUMXPT(I) + 1
                           YCOVAL = YCOORD(NUMYPT(I)-K+1,I)
                        ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                           INDZ = NETSTA(I) + (K-1)*NUMXPT(I)
                           YCOVAL = YCOORD(K,I)
                        END IF
                        IF (NX .EQ. NPPX) THEN
                           WRITE(IOUNIT,9013) YCOVAL,
     &            (ANNVAL(INDZ+J-1,IGRP,ITYP),J=1+NCPP*(NX-1),NUMXPT(I))
                        ELSE
                           WRITE(IOUNIT,9013) YCOVAL,
     &              (ANNVAL(INDZ+J-1,IGRP,ITYP),J=1+NCPP*(NX-1),NCPP*NX)
                        END IF
 25                  CONTINUE
                  END IF
 30            CONTINUE
 40         CONTINUE
 50      CONTINUE
C        End LOOP Through Networks

         IF (IRSTAT(4) .NE. 0) THEN
C           Print Out The Coord. & Concentrations For Discrete Cart Receptors
            INDC = 0
            DO 1030 IREC = 1, NUMREC
               IF (RECTYP(IREC) .EQ. 'DC') THEN
                  INDC = INDC + 1
                  IF (MOD(INDC-1,80) .EQ. 0) THEN
                     CALL HEADER
                     WRITE(IOUNIT,9032) CHRVAL,IANHRS,(CHIDEP(II,ITYP),
     &                 II=1,6),GRPID(IGRP),(WORKID(K),K = 1,INDGRP)
                     WRITE(IOUNIT,9043)
                     WRITE(IOUNIT,9011) CHIDEP(3,ITYP), POLLUT,
     &                                  OUTLBL(ITYP)
                     WRITE(IOUNIT,9048) CHIDEP(3,ITYP), CHIDEP(3,ITYP)
                  END IF
                  IF (MOD(INDC,2) .NE. 0) THEN
                     WRITE(BUF132(1:60),9045) AXR(IREC), AYR(IREC),
     &                     ANNVAL(IREC,IGRP,ITYP)
                  ELSE
                     WRITE(BUF132(61:120),9045) AXR(IREC), AYR(IREC),
     &                     ANNVAL(IREC,IGRP,ITYP)
                     WRITE(IOUNIT,9090) BUF132
                     WRITE(BUF132,9095)
                  END IF
               END IF
 1030       CONTINUE
            IF (MOD(INDC,2) .NE. 0) THEN
               WRITE(IOUNIT,9090) BUF132
               WRITE(BUF132,9095)
            END IF
         END IF

         IF (IRSTAT(5) .NE. 0) THEN
C           Print Out The Coord. & Concentrations For Discrete Polar Receptors
            INDC = 0
            DO 1040 IREC = 1, NUMREC
               IF (RECTYP(IREC) .EQ. 'DP') THEN
                  INDC = INDC + 1
                  XRMS = AXR(IREC) - AXS(IREF(IREC))
                  YRMS = AYR(IREC) - AYS(IREF(IREC))
                  DIST = SQRT(XRMS*XRMS + YRMS*YRMS)
                  DIR  = ATAN2(XRMS, YRMS) * RTODEG
                  IF (DIR .LE. 0.0) DIR = DIR + 360.
                  IF (MOD(INDC-1,80) .EQ. 0) THEN
                     CALL HEADER
                     WRITE(IOUNIT,9032) CHRVAL,IANHRS,(CHIDEP(II,ITYP),
     &                 II=1,6),GRPID(IGRP),(WORKID(K),K = 1,INDGRP)
                     WRITE(IOUNIT,9044)
                     WRITE(IOUNIT,9011) CHIDEP(3,ITYP), POLLUT,
     &                                  OUTLBL(ITYP)
                     WRITE(IOUNIT,9049) CHIDEP(3,ITYP), CHIDEP(3,ITYP)
                  END IF
                  IF (MOD(INDC,2) .NE. 0) THEN
                     WRITE(BUF132(1:65),9047) SRCID(IREF(IREC)), DIST,
     &                                       DIR, ANNVAL(IREC,IGRP,ITYP)
                  ELSE
                     WRITE(BUF132(66:130),9047) SRCID(IREF(IREC)), DIST,
     &                                       DIR, ANNVAL(IREC,IGRP,ITYP)
                     WRITE(IOUNIT,9090) BUF132
                     WRITE(BUF132,9095)
                  END IF
               END IF
 1040       CONTINUE
            IF (MOD(INDC,2) .NE. 0) THEN
               WRITE(IOUNIT,9090) BUF132
               WRITE(BUF132,9095)
            END IF
         END IF

C        Write Out The Boundary Receptors For The Sources
         IF (IRSTAT(6) .NE. 0) THEN
            INDC = 0
            IREC = 1
            DO WHILE (IREC .LE. NUMREC)
               IF (RECTYP(IREC) .EQ. 'BD') THEN
                  INDC = INDC + 1
                  ISRF = IREF(IREC)
                  IF (MOD(INDC-1,3) .EQ. 0) THEN
                     CALL HEADER
                     WRITE(IOUNIT,9032) CHRVAL,IANHRS,(CHIDEP(II,ITYP),
     &               II=1,6),GRPID(IGRP),(WORKID(K),K = 1,INDGRP)
                     WRITE(IOUNIT,9011) CHIDEP(3,ITYP), POLLUT,
     &                                  OUTLBL(ITYP)
                  END IF
                  WRITE(IOUNIT,9082) SRCID(ISRF), SRCTYP(ISRF),
     &                AXS(ISRF), AYS(ISRF), AZS(ISRF), CHIDEP(3,ITYP),
     &                CHIDEP(3,ITYP), CHIDEP(3,ITYP), (J, AXR(IREC+J-1),
     &                AYR(IREC+J-1), ANNVAL(IREC+J-1,IGRP,ITYP),J=1,36)
                  IREC = IREC + 36
               ELSE
                  IREC = IREC + 1
               END IF
            END DO
         END IF

 1000 CONTINUE

 9011 FORMAT(/40X,'** ',A4,' OF ',A8,' IN ',A40,' **'/)
 9010 FORMAT(66(' -')/)
 9013 FORMAT(2X,F10.2,1X,'|',1X,9(F13.5))
 9016 FORMAT(3X,' Y-COORD  |',48X,'X-COORD (METERS)')
 9017 FORMAT(3X,' (METERS) |',1X,9(1X,F12.2,:))
 9018 FORMAT(3X,'DIRECTION |',48X,'DISTANCE (METERS)')
 9019 FORMAT(3X,'(DEGREES) |',1X,9(1X,F12.2,:))
 9032 FORMAT(30X,'*** THE ',A6,' (',I6,' HRS) ',6A4,
     &       'VALUES FOR SOURCE GROUP:',1X,A8,' ***',
     &       /34X,'INCLUDING SOURCE(S):      ',7(A8,', ',:),
     &       /10X,12(A8,', ',:)/10X,12(A8,', ',:))
 9037 FORMAT(/35X,'*** NETWORK ID: ',A8,' ;  NETWORK TYPE: ',
     &       A8,' ***')
 9043 FORMAT(/45X,'*** DISCRETE CARTESIAN RECEPTOR POINTS ***')
 9044 FORMAT(/47X,'*** DISCRETE POLAR RECEPTOR POINTS ***')
 9045 FORMAT(6X,2(F12.2,2X),F13.5)
 9047 FORMAT(4X,A8,': ',2(F12.2,2X),F13.5)
 9048 FORMAT(6X,' X-COORD (M)   Y-COORD (M)        ',A4,
     &      22X,' X-COORD (M)   Y-COORD (M)        ',A4,/65(' -'))
 9049 FORMAT(5X,'ORIGIN',59X,'ORIGIN',
     &      /5X,' SRCID       DIST (M)     DIR (DEG)        ',A4,
     &      18X,' SRCID       DIST (M)     DIR (DEG)        ',A4,
     &      /65(' -'))
 9082 FORMAT(' BOUNDARY RECEPTOR NETWORK OF SOURCE ID: ',A8,/,5X,
     &       ' OF SOURCE TYPE: ',A8,'; WITH ORIGIN AT (',2(F10.2,', '),
     &       F10.2,')'/3(' (SEC.)  X-COORD    Y-COORD       ',A4,6X),/,
     &       12(3(1X,I4,2X,F9.1,',',F10.1,',',F13.5,' ',2X),/),/)
 9090 FORMAT(A132)
 9095 FORMAT(132(' '))

      RETURN
      END

      SUBROUTINE PRTNHI
C***********************************************************************
C                 PRTNHI Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Print Out The Specified Highest Value
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To store high short term values in global arrays
C                    rather than local arrays for later summary table
C                    output.
C                    R.W. Brode, PES, Inc. - August 15, 1995.
C
C        MODIFIED:   To add one more decimal place to receptor elevations
C                    and flagpole heights for the temporary event file.
C                    R.W. Brode, PES, Inc. - November 15, 1995.
C
C        INPUTS:  Arrays of Model Results
C
C        OUTPUTS: Printed Model Outputs for Short Term Values
C
C        CALLED FROM:   MAIN
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER NAMEEV*8
      DIMENSION IWHP(NVAL)

C     Variable Initialization
      MODNAM = 'PRTNHI'

C     Write Out the 'EV STARTING' Card to the Temp-EVent File for
C     First Output Type Only (i.e., ITYP = 1)
      IF (ITYP .EQ. 1) THEN
         WRITE(ITEVUT,1900)
      END IF

      DO 2000 IAVE = 1, NUMAVE
C        Decide if Print The Period
         IHST = 0
         DO 199 IVAL = 1, NVAL
            IF (NHIAVE(IVAL,IAVE) .EQ. 1) THEN
               IHST = IHST + 1
               IWHP(IHST) = IVAL
            END IF
 199     CONTINUE
         IF (IHST .EQ. 0) THEN
C           No High Values for This IAVE; Skip to Next Averaging Period
            GO TO 2000
         END IF
C        Print The Data
         DO 299 IVAL = 1, NVAL
            IF (NHIAVE(IVAL,IAVE) .EQ. 1) THEN
C              Print Out High Value By Receptor Table       ---   CALL SPRTHT
               CALL SPRTHT(IVAL)
            END IF
 299     CONTINUE
C        Print Out The Temporary File
         DO 1000 IGRP = 1, NUMGRP
C           Print Out the High Values
            DO 500 IREC = 1, NUMREC
C               Get The Maximum in Nth Highest
                DO 450 K = 1, IHST
                   IF (HIVALU(IREC,IWHP(K),IGRP,IAVE,ITYP) .GT.
     &                  HMAX(K,IGRP,IAVE,ITYP)) THEN
          HMAX(K,IGRP,IAVE,ITYP)   = HIVALU(IREC,IWHP(K),IGRP,IAVE,ITYP)
          HMDATE(K,IGRP,IAVE,ITYP) = NHIDAT(IREC,IWHP(K),IGRP,IAVE,ITYP)
          HMCLM(K,IGRP,IAVE,ITYP)  = HCLMSG(IREC,IWHP(K),IGRP,IAVE,ITYP)
          HMLOC(K,IGRP,IAVE,ITYP)  = IREC
                   END IF
 450           CONTINUE
 500        CONTINUE
C
C           Output The Max-Upto-IHST to the TempEVent File for the
C           First Output Type Only (i.e., ITYP = 1)
            IF (ITYP .EQ. 1) THEN
               DO 1093 K = 1, IHST
                  IT1 = MOD(IWHP(K),10)
                  IF (HMLOC(K,IGRP,IAVE,ITYP) .EQ. 0) THEN
                     XR2 = 0.0
                     YR2 = 0.0
                     ZE2 = 0.0
                     ZF2 = 0.0
                  ELSE
                     XR2 = AXR(HMLOC(K,IGRP,IAVE,ITYP))
                     YR2 = AYR(HMLOC(K,IGRP,IAVE,ITYP))
                     ZE2 = AZELEV(HMLOC(K,IGRP,IAVE,ITYP))
                     ZF2 = AZFLAG(HMLOC(K,IGRP,IAVE,ITYP))
                  END IF
                  IF (KAVE(IAVE) .LE. 24) THEN
                     WRITE(NAMEEV,'(A1,I1,A1,I2.2,I3.3)')
     &                       'H',IT1,'H',KAVE(IAVE),IGRP
                  ELSE
C                    KAVE > 24 Means MONTH Average; Write Out as 72 (=720/10)
                     KWRT = KAVE(IAVE)/10
                     WRITE(NAMEEV,'(A1,I1,A1,I2.2,I3.3)')
     &                       'H',IT1,'H',KWRT,IGRP
                  END IF
                  WRITE(ITEVUT,1901) NAMEEV, KAVE(IAVE),
     &                  GRPID(IGRP), HMDATE(K,IGRP,IAVE,ITYP),
     &                  HMAX(K,IGRP,IAVE,ITYP), HMCLM(K,IGRP,IAVE,ITYP),
     &                  HMLOC(K,IGRP,IAVE,ITYP)
                  WRITE(ITEVUT,1902) NAMEEV, XR2, YR2, ZE2, ZF2
 1093          CONTINUE
            END IF

 1000    CONTINUE

 2000 CONTINUE

C     Write Out the 'EV FINISHED' Card to the Temp-EVent File for
C     First Output Type Only (i.e., ITYP = 1)
      IF (ITYP .EQ. 1) THEN
         WRITE(ITEVUT,1909)
      END IF

 1900 FORMAT('EV STARTING')
 1901 FORMAT(3X,'EVENTPER',1X,A8,1X,I3,2X,A8,3X,I8,3X,F14.5,1X,A1,1X,I5)
 1902 FORMAT(3X,'EVENTLOC',1X,A8,1X,'XR= ',F14.5,' YR= ',F14.5,
     &       2(1X,F10.4))
 1909 FORMAT('EV FINISHED')

      RETURN
      END

      SUBROUTINE SPRTHT(IHNUM)
C***********************************************************************
C                 SPRTHT Module of ISC Short Term Model
C
C        PURPOSE: Print Out The Highest Result Values by Receptor Net
C
C        PROGRAMMER: ROGER BRODE, JEFF WANG
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To adjust format statement 9082 for BOUNDARY receptors
C                    to better accommodate UTM coordinates - 9/29/92
C
C        INPUTS:  Arrays of Source Parameters
C                 Arrays of Receptor Locations
C                 Arrays of Model Results
C
C        OUTPUTS: Printed Model Outputs
C
C        CALLED FROM:   LTOUT
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER BUF132*132, CHRVAL(10)*4

C     Variable Initializations
      DATA (CHRVAL(I),I=1,10)/' 1ST',' 2ND',' 3RD',' 4TH',' 5TH',
     &                        ' 6TH',' 7TH',' 8TH',' 9TH','10TH'/
      MODNAM = 'SPRTHT'

      DO 1001 IGRP = 1, NUMGRP

C        Fill Work Array With SRCIDs For This Group
         INDGRP = 0
         DO 210 ISRC = 1, NUMSRC
            IF (IGROUP(ISRC,IGRP) .EQ. 1) THEN
               INDGRP = INDGRP + 1
               WORKID(INDGRP) = SRCID(ISRC)
            END IF
 210     CONTINUE
C        Check for More Than 31 Sources Per Group
         IF (INDGRP .GT. 31) THEN
            WORKID(31) = ' . . . '
            INDGRP = 31
         END IF

C        Print Receptor Network Coordinates:
C        Set Number of Columns Per Page, NCPP
         NCPP = 5
C        Set Number of Rows Per Page, NRPP
         NRPP = 40
C        Begin LOOP Through Networks
         DO 50 I = 1, INNET
C           Calculate Number of Pages Per X-Group, NPPX, & Per Y-Group, NPPY
            NPPX = 1 + INT((NUMXPT(I)-1)/NCPP)
            NPPY = 1 + INT((NUMYPT(I)-1)/NRPP)
            DO 40 NX = 1, NPPX
               DO 30 NY = 1, NPPY
                  CALL HEADER
                  WRITE(IOUNIT,9032) CHRVAL(IHNUM),CHRAVE(IAVE),
     &       (CHIDEP(II,ITYP),II=1,6),GRPID(IGRP),(WORKID(J),J=1,INDGRP)
                  WRITE(IOUNIT,9037) NTID(I), NTTYP(I)
C                 Print The Values By Source Group
                  WRITE(IOUNIT,9011) CHIDEP(3,ITYP), POLLUT,
     &                               OUTLBL(ITYP)
                  IF (NX .EQ. NPPX) THEN
                     IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                        WRITE(IOUNIT,9016)
                        WRITE(IOUNIT,9017) (XCOORD(J,I),J=1+NCPP*(NX-1),
     &                                                    NUMXPT(I))
                     ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                        WRITE(IOUNIT,9018)
                        WRITE(IOUNIT,9019) (XCOORD(J,I),J=1+NCPP*(NX-1),
     &                                                    NUMXPT(I))
                     END IF
                  ELSE
                     IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                        WRITE(IOUNIT,9016)
                        WRITE(IOUNIT,9017) (XCOORD(J,I),J=1+NCPP*(NX-1),
     &                                                    NCPP*NX)
                     ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                        WRITE(IOUNIT,9018)
                        WRITE(IOUNIT,9019) (XCOORD(J,I),J=1+NCPP*(NX-1),
     &                                                    NCPP*NX)
                     END IF
                  END IF
                  WRITE(IOUNIT,9010)
                  IF (NY .EQ. NPPY) THEN
                     DO 20 K = 1+NRPP*(NY-1), NUMYPT(I)
                        IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                           INDZ = NETEND(I) - K*NUMXPT(I) + 1
                           YCOVAL = YCOORD(NUMYPT(I)-K+1,I)
                        ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                           INDZ = NETSTA(I) + (K-1)*NUMXPT(I)
                           YCOVAL = YCOORD(K,I)
                        END IF
                        IF (NX .EQ. NPPX) THEN
                           WRITE(IOUNIT,9013) YCOVAL,
     &                       (HIVALU(INDZ+J-1,IHNUM,IGRP,IAVE,ITYP),
     &                        HCLMSG(INDZ+J-1,IHNUM,IGRP,IAVE,ITYP),
     &                        NHIDAT(INDZ+J-1,IHNUM,IGRP,IAVE,ITYP),
     &                        J=1+NCPP*(NX-1),NUMXPT(I))
                        ELSE
                           WRITE(IOUNIT,9013) YCOVAL,
     &                       (HIVALU(INDZ+J-1,IHNUM,IGRP,IAVE,ITYP),
     &                        HCLMSG(INDZ+J-1,IHNUM,IGRP,IAVE,ITYP),
     &                        NHIDAT(INDZ+J-1,IHNUM,IGRP,IAVE,ITYP),
     &                        J=1+NCPP*(NX-1),NCPP*NX)
                        END IF
 20                  CONTINUE
                  ELSE
                     DO 25 K = 1+NRPP*(NY-1), NRPP*NY
                        IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                           INDZ = NETEND(I) - K*NUMXPT(I) + 1
                           YCOVAL = YCOORD(NUMYPT(I)-K+1,I)
                        ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                           INDZ = NETSTA(I) + (K-1)*NUMXPT(I)
                           YCOVAL = YCOORD(K,I)
                        END IF
                        IF (NX .EQ. NPPX) THEN
                           WRITE(IOUNIT,9013) YCOVAL,
     &                       (HIVALU(INDZ+J-1,IHNUM,IGRP,IAVE,ITYP),
     &                        HCLMSG(INDZ+J-1,IHNUM,IGRP,IAVE,ITYP),
     &                        NHIDAT(INDZ+J-1,IHNUM,IGRP,IAVE,ITYP),
     &                        J=1+NCPP*(NX-1),NUMXPT(I))
                        ELSE
                           WRITE(IOUNIT,9013) YCOVAL,
     &                       (HIVALU(INDZ+J-1,IHNUM,IGRP,IAVE,ITYP),
     &                        HCLMSG(INDZ+J-1,IHNUM,IGRP,IAVE,ITYP),
     &                        NHIDAT(INDZ+J-1,IHNUM,IGRP,IAVE,ITYP),
     &                        J=1+NCPP*(NX-1),NCPP*NX)
                        END IF
 25                  CONTINUE
                  END IF
 30            CONTINUE
 40         CONTINUE
 50      CONTINUE
C        End LOOP Through Networks

         IF (IRSTAT(4) .NE. 0) THEN
C           Print Out The Coord. & Concentrations For Discrete Cart Receptors
            INDC = 0
            DO 1030 IREC = 1, NUMREC
               IF (RECTYP(IREC) .EQ. 'DC') THEN
                  INDC = INDC + 1
                  IF (MOD(INDC-1,80) .EQ. 0) THEN
                     CALL HEADER
                     WRITE(IOUNIT,9032) CHRVAL(IHNUM), CHRAVE(IAVE),
     &       (CHIDEP(II,ITYP),II=1,6),GRPID(IGRP),(WORKID(J),J=1,INDGRP)
                     WRITE(IOUNIT,9043)
                     WRITE(IOUNIT,9011) CHIDEP(3,ITYP), POLLUT,
     &                                  OUTLBL(ITYP)
                     WRITE(IOUNIT,9048) CHIDEP(3,ITYP), CHIDEP(3,ITYP)
                  END IF
                  IF (MOD(INDC,2) .NE. 0) THEN
                     WRITE(BUF132(1:65),9045) AXR(IREC), AYR(IREC),
     &                     HIVALU(IREC,IHNUM,IGRP,IAVE,ITYP),
     &                     HCLMSG(IREC,IHNUM,IGRP,IAVE,ITYP),
     &                     NHIDAT(IREC,IHNUM,IGRP,IAVE,ITYP)
                  ELSE
                     WRITE(BUF132(66:130),9045) AXR(IREC), AYR(IREC),
     &                     HIVALU(IREC,IHNUM,IGRP,IAVE,ITYP),
     &                     HCLMSG(IREC,IHNUM,IGRP,IAVE,ITYP),
     &                     NHIDAT(IREC,IHNUM,IGRP,IAVE,ITYP)
                     WRITE(IOUNIT,9090) BUF132
                     WRITE(BUF132,9095)
                  END IF
               END IF
 1030       CONTINUE
            IF (MOD(INDC,2) .NE. 0) THEN
               WRITE(IOUNIT,9090) BUF132
               WRITE(BUF132,9095)
            END IF
         END IF

         IF (IRSTAT(5) .NE. 0) THEN
C           Print Out The Coord. & Concentrations For Discrete Polar Receptors
            INDC = 0
            DO 1040 IREC = 1, NUMREC
               IF (RECTYP(IREC) .EQ. 'DP') THEN
                  INDC = INDC + 1
                  XRMS = AXR(IREC) - AXS(IREF(IREC))
                  YRMS = AYR(IREC) - AYS(IREF(IREC))
                  DIST = SQRT(XRMS*XRMS + YRMS*YRMS)
                  DIR  = ATAN2(XRMS, YRMS) * RTODEG
                  IF (DIR .LE. 0.0) DIR = DIR + 360.
                  IF (MOD(INDC-1,80) .EQ. 0) THEN
                     CALL HEADER
                     WRITE(IOUNIT,9032) CHRVAL(IHNUM), CHRAVE(IAVE),
     &       (CHIDEP(II,ITYP),II=1,6),GRPID(IGRP),(WORKID(J),J=1,INDGRP)
                     WRITE(IOUNIT,9044)
                     WRITE(IOUNIT,9011) CHIDEP(3,ITYP), POLLUT,
     &                                  OUTLBL(ITYP)
                     WRITE(IOUNIT,9049) CHIDEP(3,ITYP), CHIDEP(3,ITYP)
                  END IF
                  IF (MOD(INDC,2) .NE. 0) THEN
                     WRITE(BUF132(1:66),9047) SRCID(IREF(IREC)), DIST,
     &                       DIR, HIVALU(IREC,IHNUM,IGRP,IAVE,ITYP),
     &                            HCLMSG(IREC,IHNUM,IGRP,IAVE,ITYP),
     &                            NHIDAT(IREC,IHNUM,IGRP,IAVE,ITYP)
                  ELSE
                     WRITE(BUF132(67:132),9047) SRCID(IREF(IREC)), DIST,
     &                       DIR, HIVALU(IREC,IHNUM,IGRP,IAVE,ITYP),
     &                            HCLMSG(IREC,IHNUM,IGRP,IAVE,ITYP),
     &                            NHIDAT(IREC,IHNUM,IGRP,IAVE,ITYP)
                     WRITE(IOUNIT,9090) BUF132
                     WRITE(BUF132,9095)
                  END IF
               END IF
 1040       CONTINUE
            IF (MOD(INDC,2) .NE. 0) THEN
               WRITE(IOUNIT,9090) BUF132
               WRITE(BUF132,9095)
            END IF
         END IF

C        Write Out The Boundary Receptors For The Sources
         IF (IRSTAT(6) .NE. 0) THEN
            INDC = 0
            IREC = 1
            DO WHILE (IREC .LE. NUMREC)
               IF (RECTYP(IREC) .EQ. 'BD') THEN
                  INDC = INDC + 1
                  ISRF = IREF(IREC)
                  IF (MOD(INDC-1,2) .EQ. 0) THEN
                     CALL HEADER
                     WRITE(IOUNIT,9032) CHRVAL(IHNUM), CHRAVE(IAVE),
     &       (CHIDEP(II,ITYP),II=1,6),GRPID(IGRP),(WORKID(J),J=1,INDGRP)
                     WRITE(IOUNIT,9011) CHIDEP(3,ITYP), POLLUT,
     &                                  OUTLBL(ITYP)
                  END IF
                  WRITE(IOUNIT,9082) SRCID(ISRF),SRCTYP(ISRF),AXS(ISRF),
     &             AYS(ISRF), AZS(ISRF), CHIDEP(3,ITYP), CHIDEP(3,ITYP),
     &                (J,AXR(IREC+J-1),AYR(IREC+J-1),
     &                HIVALU(IREC+J-1,IHNUM,IGRP,IAVE,ITYP),
     &                HCLMSG(IREC+J-1,IHNUM,IGRP,IAVE,ITYP),
     &                NHIDAT(IREC+J-1,IHNUM,IGRP,IAVE,ITYP),J=1,36)
                  IREC = IREC + 36
               ELSE
                  IREC = IREC + 1
               END IF
            END DO
         END IF

 1001 CONTINUE

 9011 FORMAT(/40X,'** ',A4,' OF ',A8,' IN ',A40,' **'/)
 9010 FORMAT(66(' -')/)
 9016 FORMAT(1X,' Y-COORD  |',50X,'X-COORD (METERS)')
 9017 FORMAT(1X,' (METERS) |',3X,F13.2,4(11X,F13.2,:))
 9018 FORMAT(1X,'DIRECTION |',50X,'DISTANCE (METERS)')
 9019 FORMAT(1X,'(DEGREES) |',3X,F13.2,4(11X,F13.2,:))
 9013 FORMAT(1X,F9.1,1X,'|',5(F13.5,A1,'(',I8,')',:))
 9032 FORMAT(30X,'*** THE  ',A4,' HIGHEST ',A5,1X,6A4,
     &       'VALUES FOR SOURCE GROUP:',2X,A8,' ***',
     &       /34X,'INCLUDING SOURCE(S):      ',7(A8,', ',:),
     &       /10X,12(A8,', ':)/10X,12(A8,', ':))
 9037 FORMAT(/35X,'*** NETWORK ID: ',A8,' ;  NETWORK TYPE: ',
     &       A8,' ***')
 9043 FORMAT(/45X,'*** DISCRETE CARTESIAN RECEPTOR POINTS ***')
 9044 FORMAT(/47X,'*** DISCRETE POLAR RECEPTOR POINTS ***')
 9045 FORMAT(6X,2(F11.2,2X),F13.5,A1,1X,'(',I8,')')
 9047 FORMAT(4X,A8,': ',2(F11.2,2X),F13.5,A1,1X,'(',I8,')')
 9048 FORMAT(6X,'X-COORD (M)  Y-COORD (M)        ',A4,5X,'(YYMMDDHH)',
     &      14X,'X-COORD (M)  Y-COORD (M)        ',A4,5X,'(YYMMDDHH)',
     &      /66(' -'))
 9049 FORMAT(5X,'ORIGIN',60X,'ORIGIN',
     &      /5X,' SRCID      DIST (M)    DIR (DEG)        ',A4,
     &       5X,'(YYMMDDHH)',
     &       6X,' SRCID      DIST (M)    DIR (DEG)        ',A4,
     &       5X,'(YYMMDDHH)',
     &      /66(' -'))
 9082 FORMAT(' BOUNDARY RECEPTOR NETWORK OF SOURCE ID: ',A8,/,5X,
     &       ' OF SOURCE TYPE: ',A8,'; WITH ORIGIN AT (',2(F10.2,', '),
     &       F10.2,')'/2(' (SEC.)    X-COORD     Y-COORD       ',A4,
     &       '    (YYMMDDHH)',7X),/,
     &       18(2(1X,I4,3X,F10.2,', ',F10.2,',',F13.5,A1,'(',I8,')',
     &       7X),/),/)
 9090 FORMAT(A132)
 9095 FORMAT(132(' '))

      RETURN
      END

      SUBROUTINE PRTMAX
C***********************************************************************
C                 PRTMAX Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Print Out The Overall Maximum Value Tables
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Arrays of Source Parameters
C                 Arrays of Receptor Locations
C                 Arrays of Model Results
C
C        OUTPUTS: Printed Model Outputs
C
C        CALLED FROM:   OUTPUT
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER NID1*8, NID2*8, NTY1*2, NTY2*2

C     Variable Initializations
      MODNAM = 'PRTMAX'

      DO 2000 IAVE = 1, NUMAVE
C        Check Array to See IF Maximum Values Are Needed For This AVEPER
         IF (MAXAVE(IAVE) .NE. 1) GO TO 2000

         DO 1000 IGRP = 1, NUMGRP
            INDGRP = 0

C           Assign The Group ID
            DO 210 ISRC = 1, NUMSRC
               IF (IGROUP(ISRC,IGRP) .EQ. 1) THEN
                  INDGRP = INDGRP + 1
                  WORKID(INDGRP) = SRCID(ISRC)
               END IF
 210        CONTINUE
C           Check for More Than 31 Sources Per Group
            IF (INDGRP .GT. 31) THEN
               WORKID(31) = ' . . . '
               INDGRP = 31
            END IF

            IF (IMXVAL(IAVE) .GE. 2) THEN
C              Determine Number of Pages @ 80 Per Page, NPG
               NPG = 1 + INT((IMXVAL(IAVE)-1)/80)
               DO 800 L = 1, NPG
C                 Determine Number of Rows for This Page, NROWS
                  IF (L .EQ. NPG) THEN
                     NROWS = (IMXVAL(IAVE)-80*(L-1))/2
                  ELSE
                     NROWS = 40
                  END IF
C                 Write Out Header Information for This Page
                  CALL HEADER
                  WRITE(IOUNIT,9032) IMXVAL(IAVE), CHRAVE(IAVE),
     &              (CHIDEP(II,ITYP),II=1,6), GRPID(IGRP), (WORKID(K),
     &                                              K = 1,INDGRP)
                  WRITE(IOUNIT,9011) CHIDEP(3,ITYP), POLLUT,
     &                               OUTLBL(ITYP)
                  WRITE(IOUNIT,1) CHIDEP(3,ITYP), CHIDEP(3,ITYP)
C                 Set Start Row of Loop for This Page, JSTRT
                  JSTRT = 1 + 80*(L-1)
                  DO 500 J = JSTRT, JSTRT+NROWS-1
                     J1 = J + NROWS
                     IF (L.EQ.NPG .AND. MOD(IMXVAL(IAVE),2).NE.0) THEN
                        J1 = J1 + 1
                     END IF
                     KMAX1 = MXLOCA(J,IGRP,IAVE,ITYP)
                     KMAX2 = MXLOCA(J1,IGRP,IAVE,ITYP)
                     IF (KMAX1 .EQ. 0) THEN
                        XR1 = 0.
                        YR1 = 0.
                        NID1 = ' '
                        NTY1 = ' '
                     ELSE
                        XR1 = AXR(KMAX1)
                        YR1 = AYR(KMAX1)
                        NID1 = NETID(KMAX1)
                        NTY1 = RECTYP(KMAX1)
                     END IF
                     IF (KMAX2 .EQ. 0) THEN
                        XR2 = 0.
                        YR2 = 0.
                        NID2 = ' '
                        NTY2 = ' '
                     ELSE
                        XR2 = AXR(KMAX2)
                        YR2 = AYR(KMAX2)
                        NID2 = NETID(KMAX2)
                        NTY2 = RECTYP(KMAX2)
                     END IF
                     WRITE(IOUNIT,2) J, RMXVAL(J,IGRP,IAVE,ITYP),
     &               MCLMSG(J,IGRP,IAVE,ITYP), MXDATE(J,IGRP,IAVE,ITYP),
     &                XR1, YR1, NTY1, J1,
     &             RMXVAL(J1,IGRP,IAVE,ITYP), MCLMSG(J1,IGRP,IAVE,ITYP),
     &                 MXDATE(J1,IGRP,IAVE,ITYP), XR2, YR2, NTY2
 500              CONTINUE
 800           CONTINUE
               IF (MOD(IMXVAL(IAVE),2) .NE. 0) THEN
C                 Odd Number of Max Values - Print Out Last Value
                  J = INT(IMXVAL(IAVE)/2) + 1 + 40*(NPG-1)
                  KMAX1 = MXLOCA(J,IGRP,IAVE,ITYP)
                  XR1 = AXR(KMAX1)
                  YR1 = AYR(KMAX1)
                  NTY1 = RECTYP(KMAX1)
                  WRITE(IOUNIT,3) J, RMXVAL(J,IGRP,IAVE,ITYP),
     &               MCLMSG(J,IGRP,IAVE,ITYP), MXDATE(J,IGRP,IAVE,ITYP),
     &                  XR1, YR1, NTY1
               END IF
            ELSE
               J = 1
               KMAX1 = MXLOCA(J,IGRP,IAVE,ITYP)
               XR1 = AXR(KMAX1)
               YR1 = AYR(KMAX1)
               NTY1 = RECTYP(KMAX1)
               CALL HEADER
               WRITE(IOUNIT,9032) IMXVAL(IAVE), CHRAVE(IAVE),
     &           (CHIDEP(II,ITYP),II=1,6), GRPID(IGRP), (WORKID(K),
     &                                              K = 1,INDGRP)
               WRITE(IOUNIT,9011) CHIDEP(3,ITYP), POLLUT, OUTLBL(ITYP)
               WRITE(IOUNIT,1) CHIDEP(3,ITYP), CHIDEP(3,ITYP)
               WRITE(IOUNIT,3) J, RMXVAL(J,IGRP,IAVE,ITYP),
     &               MCLMSG(J,IGRP,IAVE,ITYP), MXDATE(J,IGRP,IAVE,ITYP),
     &               XR1, YR1, NTY1
            END IF

C           WRITE Out Explanation of Receptor Types
            WRITE(IOUNIT,9050)

 1000    CONTINUE
 2000 CONTINUE

 1    FORMAT(1X,'RANK',7X,A4,4X,'(YYMMDDHH) AT',7X,
     &           'RECEPTOR (XR,YR) OF TYPE ',3X,
     &           'RANK',7X,A4,4X,'(YYMMDDHH) AT',7X,
     &           'RECEPTOR (XR,YR) OF TYPE ',
     &           /66(' -'))
 2    FORMAT(1X,I3,'.',1X,F13.5,A1,'(',I8,') AT',1X,
     &          '( ',F10.2,', ',F10.2,')  ',A2,5X,
     &          I3,'.',1X,F13.5,A1,'(',I8,') AT',1X,
     &          '( ',F10.2,', ',F10.2,')  ',A2)
 3    FORMAT(1X,I3,'.',1X,F13.5,A1,'(',I8,') AT',1X,
     &          '( ',F10.2,', ',F10.2,')  ',A2)
 9011 FORMAT(/40X,'** ',A4,' OF ',A8,' IN ',A40,' **'/)
 9032 FORMAT(32X,'*** THE MAXIMUM ',I4,2X,A5,1X,6A4,
     &       'VALUES FOR SOURCE GROUP:',2X,A8,' ***'
     &       /36X,'INCLUDING SOURCE(S):    ',
     &       7(A8,', ',:),/10x,12(A8,', ',:)/10x,12(A8,', ',:))
 9050 FORMAT(/1X,' *** RECEPTOR TYPES:  GC = GRIDCART',
     &                            /23X,'GP = GRIDPOLR',
     &                            /23X,'DC = DISCCART',
     &                            /23X,'DP = DISCPOLR',
     &                            /23X,'BD = BOUNDARY')

      RETURN
      END

      SUBROUTINE PRTSUM
C***********************************************************************
C                 PRTSUM Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Print Out the Result Summary Tables
C
C        PROGRAMMER: Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To use arrays for high short term values, rather
C                    than reading from temporary event file.
C                    R.W. Brode, PES, Inc. - August 15, 1995.
C
C        INPUTS:  EVENT.TMP File Which Contains Maximum Values
C
C        OUTPUTS: Result Summary Table By Average Period
C
C        CALLED FROM:   MAIN
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      INTEGER IWHP(NVAL)
      CHARACTER RANK(10)*4

C     Variable Initializations
      DATA (RANK(I),I=1,10) /' 1ST',' 2ND',' 3RD',' 4TH',' 5TH',
     &                       ' 6TH',' 7TH',' 8TH',' 9TH','10TH'/
      MODNAM = 'PRTSUM'

C     Print Maximum PERIOD Averages, If Appropriate
      IF (PERIOD .OR. ANNUAL) THEN
C        Calculate Number of Groups Per Page, NGPP
         NGPP = INT(40/(NVAL+1))
         DO 200 IGRP = 1, NUMGRP
            IF (MOD(IGRP-1, NGPP) .EQ. 0) THEN
               CALL HEADER
               WRITE(IOUNIT,9021) IANHRS
               WRITE(IOUNIT,9011) CHIDEP(3,ITYP), POLLUT, OUTLBL(ITYP)
               WRITE(IOUNIT,9022) CHIDEP(1,ITYP), CHIDEP(2,ITYP),
     &                            CHIDEP(3,ITYP)
            END IF
            DO 100 IVAL = 1, NVAL
               INDMX = IMXLOC(IVAL,IGRP,ITYP)
               IF (IVAL .EQ. 1 .AND. INDMX .NE. 0) THEN
                  WRITE(IOUNIT,1012) GRPID(IGRP), RANK(IVAL),
     &                  AMXVAL(IVAL,IGRP,ITYP), AXR(INDMX), AYR(INDMX),
     &                  AZELEV(INDMX), AZFLAG(INDMX), RECTYP(INDMX),
     &                  NETID(INDMX)
               ELSE IF (IVAL .EQ. 1 .AND. INDMX .EQ. 0) THEN
                  AXR1 = 0.0
                  AYR1 = 0.0
                  AZELV1 = 0.0
                  AZFLG1 = 0.0
                  WRITE(IOUNIT,1014) GRPID(IGRP), RANK(IVAL),
     &                AMXVAL(IVAL,IGRP,ITYP), AXR1, AYR1, AZELV1, AZFLG1
               ELSE IF (INDMX .EQ. 0) THEN
                  AXR1 = 0.0
                  AYR1 = 0.0
                  AZELV1 = 0.0
                  AZFLG1 = 0.0
                  WRITE(IOUNIT,1015) RANK(IVAL),
     &                AMXVAL(IVAL,IGRP,ITYP), AXR1, AYR1, AZELV1, AZFLG1
               ELSE
                  WRITE(IOUNIT,1013) RANK(IVAL),
     &                  AMXVAL(IVAL,IGRP,ITYP), AXR(INDMX), AYR(INDMX),
     &                  AZELEV(INDMX), AZFLAG(INDMX), RECTYP(INDMX),
     &                  NETID(INDMX)
               END IF
 100        CONTINUE
 200     CONTINUE
C        WRITE Out Explanation of Receptor Types
         WRITE(IOUNIT,9050)
      END IF

C     Begin LOOP Through Averaging Periods
      DO 500 IAVE = 1, NUMAVE
         IHST = 0
         DO IVAL = 1, NVAL
            IF (NHIAVE(IVAL,IAVE) .EQ. 1) THEN
               IHST = IHST + 1
               IWHP(IHST) = IVAL
            END IF
         END DO
         IF (IHST .EQ. 0) THEN
C           No High Values for This IAVE; Skip to Next Averaging Period
            GO TO 500
         END IF
C        Calculate Number of Groups Per Page, NGPP
         NGPP = INT(40/(IHST+1))

C        Begin Source Group LOOP
         DO 400 IGRP = 1, NUMGRP
C           Begin LOOP Through High Values
            DO 300 I = 1, IHST
               INDLOC = HMLOC(I,IGRP,IAVE,ITYP)
               IF (I .EQ. 1) THEN
                  IF (MOD(IGRP-1,NGPP) .EQ. 0) THEN
                     CALL HEADER
                     WRITE(IOUNIT,9031) CHRAVE(IAVE)
                     WRITE(IOUNIT,9011) CHIDEP(3,ITYP),POLLUT,
     &                                  OUTLBL(ITYP)
                     WRITE(IOUNIT,9032) CHIDEP(1,ITYP),
     &                                  CHIDEP(2,ITYP),CHIDEP(3,ITYP)
                  END IF
                  WRITE(IOUNIT,*) ' '
                  IF (INDLOC .EQ. 0) THEN
                     XR2 = 0.0
                     YR2 = 0.0
                     ZE2 = 0.0
                     ZF2 = 0.0
                     WRITE(IOUNIT,1004) GRPID(IGRP), RANK(IWHP(I)),
     &                  HMAX(I,IGRP,IAVE,ITYP),
     &                  HMCLM(I,IGRP,IAVE,ITYP),
     &                  HMDATE(I,IGRP,IAVE,ITYP),
     &                  XR2, YR2, ZE2, ZF2
                  ELSE
                     XR2 = AXR(INDLOC)
                     YR2 = AYR(INDLOC)
                     ZE2 = AZELEV(INDLOC)
                     ZF2 = AZFLAG(INDLOC)
                     WRITE(IOUNIT,1002) GRPID(IGRP), RANK(IWHP(I)),
     &                  HMAX(I,IGRP,IAVE,ITYP),
     &                  HMCLM(I,IGRP,IAVE,ITYP),
     &                  HMDATE(I,IGRP,IAVE,ITYP),
     &                  XR2, YR2, ZE2, ZF2, RECTYP(INDLOC),NETID(INDLOC)
                  END IF
               ELSE
                  IF (INDLOC .EQ. 0) THEN
                     XR2 = 0.0
                     YR2 = 0.0
                     ZE2 = 0.0
                     ZF2 = 0.0
                     WRITE(IOUNIT,1005) RANK(IWHP(I)),
     &                  HMAX(I,IGRP,IAVE,ITYP),
     &                  HMCLM(I,IGRP,IAVE,ITYP),
     &                  HMDATE(I,IGRP,IAVE,ITYP),
     &                  XR2, YR2, ZE2, ZF2
                  ELSE
                     XR2 = AXR(INDLOC)
                     YR2 = AYR(INDLOC)
                     ZE2 = AZELEV(INDLOC)
                     ZF2 = AZFLAG(INDLOC)
                     WRITE(IOUNIT,1003) RANK(IWHP(I)),
     &                  HMAX(I,IGRP,IAVE,ITYP),
     &                  HMCLM(I,IGRP,IAVE,ITYP),
     &                  HMDATE(I,IGRP,IAVE,ITYP),
     &                  XR2, YR2, ZE2, ZF2, RECTYP(INDLOC),NETID(INDLOC)
                  END IF
               END IF
 300        CONTINUE
 400     CONTINUE

C        WRITE Out Explanation of Receptor Types
         WRITE(IOUNIT,9050)

 500  CONTINUE

 1001 FORMAT(A80)
 1002 FORMAT(1X,A8,' HIGH ',A4,' HIGH VALUE IS',F14.5,A1,' ON ',
     &       I8,': AT ','(',2(F11.2,', '),F9.2,', ',F9.2,')',
     &       2X,A2,3X,A8)
 1003 FORMAT(9X,' HIGH ',A4,' HIGH VALUE IS',F14.5,A1,' ON ',
     &       I8,': AT ','(',2(F11.2,', '),F9.2,', ',F9.2,')',
     &       2X,A2,3X,A8)
 1004 FORMAT(1X,A8,' HIGH ',A4,' HIGH VALUE IS',F14.5,A1,' ON ',
     &       I8,': AT ','(',2(F11.2,', '),F9.2,', ',F9.2,')')
 1005 FORMAT(9X,' HIGH ',A4,' HIGH VALUE IS',F14.5,A1,' ON ',
     &       I8,': AT ','(',2(F11.2,', '),F9.2,', ',F9.2,')')
 1012 FORMAT(/1X,A8,A4,' HIGHEST VALUE IS',F14.5,' AT ',
     &       '(',2(F11.2,', '),F9.2,', ',F9.2,')',2X,A2,3X,A8)
 1013 FORMAT(9X,A4,' HIGHEST VALUE IS',F14.5,' AT ',
     &       '(',2(F11.2,', '),F9.2,', ',F9.2,')',2X,A2,3X,A8)
 1014 FORMAT(/1X,A8,A4,' HIGHEST VALUE IS',F14.5,' AT ',
     &       '(',2(F11.2,', '),F9.2,', ',F9.2,')')
 1015 FORMAT(9X,A4,' HIGHEST VALUE IS',F14.5,' AT ',
     &       '(',2(F11.2,', '),F9.2,', ',F9.2,')')
 9011 FORMAT(/40X,'** ',A4,' OF ',A8,' IN ',A40,' **'/)
 9021 FORMAT(44X,'*** THE SUMMARY OF MAXIMUM PERIOD (',I6,
     &       ' HRS) RESULTS ***'/)
 9022 FORMAT(103X,'NETWORK',/1X,'GROUP ID',22X,3A4,
     &       16X,'RECEPTOR  (XR, YR, ZELEV, ZFLAG)',3X,'OF TYPE',
     &       2X,'GRID-ID',/60(' -'))
 9031 FORMAT(48X,'*** THE SUMMARY OF HIGHEST ',A5,' RESULTS ***'/)
 9032 FORMAT(54X,'DATE',62X,'NETWORK',/1X,'GROUP ID',25X,3A4,5X,
     &       '(YYMMDDHH)',13X,'RECEPTOR  (XR, YR, ZELEV, ZFLAG)',
     &       5X,'OF TYPE',2X,'GRID-ID',/65(' -'))
 9050 FORMAT(//1X,' *** RECEPTOR TYPES:  GC = GRIDCART',
     &                             /23X,'GP = GRIDPOLR',
     &                             /23X,'DC = DISCCART',
     &                             /23X,'DP = DISCPOLR',
     &                             /23X,'BD = BOUNDARY')

 1000 RETURN
      END

      SUBROUTINE EVEFIL
C***********************************************************************
C                 EVEFIL Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Generate EVENT Input File
C
C        PROGRAMMER: Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To allow for changes in the ISTRG PARAMETER, currently
C                    set to 132.  Also moved the code to insert a blank line
C                    after each pathway to SUB. SETUP.
C                    R.W. Brode, PES, Inc. - November 15, 1995.
C
C        INPUTS:  EVENT.TMP File Which Contains Maximum 10 Values
C
C        OUTPUTS: EVENT Input Runstream Image File
C
C        CALLED FROM: MAIN
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER EVFRM*20, EVFRM1*20, EVFRM2*20, EVFRM3*20
      LOGICAL HITIN

C     Variable Initializations
      MODNAM = 'EVEFIL'
      HITIN  = .FALSE.
      EOF    = .FALSE.

C     Setup WRITE format for runstream record,
C     based on the ISTRG PARAMETER (set in MAIN1.INC)
      WRITE(EVFRM,9300) ISTRG
 9300 FORMAT('(A',I3.3,')')
      WRITE(EVFRM1,9301) ISTRG
 9301 FORMAT('(1X,A',I3.3,')')
      WRITE(EVFRM2,9302) ISTRG
 9302 FORMAT('(2X,A',I3.3,')')
      WRITE(EVFRM3,9303) ISTRG
 9303 FORMAT('(3X,A',I3.3,')')

C     Rewind Temporary Event File
      REWIND ITEVUT

C     Read Records From The Temporary Event File
      DO WHILE (.NOT. EOF)
         IF (.NOT. HITIN) THEN
C           Not in the Event Pathway - Echo Input to EVENT File
            READ(ITEVUT,EVFRM,END=999) RUNST1
            IF (RUNST1(1:11) .EQ. 'EV STARTING') THEN
C              Event Pathway Starts - Set Logical Switch
               HITIN = .TRUE.
               IF (LOCB(1) .EQ. 1) THEN
                  WRITE(IEVUNT,EVFRM) RUNST1
               ELSE IF (LOCB(1) .EQ. 2) THEN
                  WRITE(IEVUNT,EVFRM1) RUNST1
               ELSE IF (LOCB(1) .EQ. 3) THEN
                  WRITE(IEVUNT,EVFRM2) RUNST1
               ELSE IF (LOCB(1) .EQ. 4) THEN
                  WRITE(IEVUNT,EVFRM3) RUNST1
               END IF
            ELSE
               WRITE(IEVUNT,EVFRM) RUNST1
            END IF
         ELSE
            READ(ITEVUT,EVFRM,END=999) RUNST1
            IF (RUNST1(1:11) .EQ. 'EV FINISHED') THEN
               IF (MXFILE) THEN
C                 Add Events From Max Value (>Thresh) Files ---   CALL MXEVNT
                  CALL MXEVNT
               END IF
               IF (LOCB(1) .EQ. 1) THEN
                  WRITE(IEVUNT,EVFRM) RUNST1
               ELSE IF (LOCB(1) .EQ. 2) THEN
                  WRITE(IEVUNT,EVFRM1) RUNST1
               ELSE IF (LOCB(1) .EQ. 3) THEN
                  WRITE(IEVUNT,EVFRM2) RUNST1
               ELSE IF (LOCB(1) .EQ. 4) THEN
                  WRITE(IEVUNT,EVFRM3) RUNST1
               END IF
               HITIN = .FALSE.
            END IF
            IF (HITIN .AND. RUNST1(1:11).EQ.'   EVENTPER') THEN
               READ(RUNST1(22:),'(I3)') IAVEP
               READ(RUNST1(38:),'(F14.5)',ERR=99) CONC1
            END IF

            GO TO 100

C           Write Out Warning Message:  Error Reading CONC From TmpEvent File
 99         CALL ERRHDL(PATH,MODNAM,'W','570',RUNST1(13:20))
C           Set CONC1 To Large Value for Event File
            CONC1 = 1.0E9

 100        CONTINUE
            IF (HITIN. AND. IAVEP.NE.720 .AND. CONC1.NE.0.0) THEN
C              Write Out EVENTPER & EVENTLOC Cards, Allowing for Column Shift
               IF (RUNST1(1:11) .EQ. '   EVENTPER') THEN
                  IF (LOCB(1) .EQ. 1) THEN
                     WRITE(IEVUNT,1061) RUNST1(1:63)
                  ELSE IF (LOCB(1) .EQ. 2) THEN
                     WRITE(IEVUNT,1062) RUNST1(1:63)
                  ELSE IF (LOCB(1) .EQ. 3) THEN
                     WRITE(IEVUNT,1063) RUNST1(1:63)
                  ELSE IF (LOCB(1) .EQ. 4) THEN
                     WRITE(IEVUNT,1064) RUNST1(1:63)
                  END IF
               ELSE
                  IF (LOCB(1) .EQ. 1) THEN
                     WRITE(IEVUNT,EVFRM) RUNST1
                  ELSE IF (LOCB(1) .EQ. 2) THEN
                     WRITE(IEVUNT,EVFRM1) RUNST1
                  ELSE IF (LOCB(1) .EQ. 3) THEN
                     WRITE(IEVUNT,EVFRM2) RUNST1
                  ELSE IF (LOCB(1) .EQ. 4) THEN
                     WRITE(IEVUNT,EVFRM3) RUNST1
                  END IF
               END IF
            END IF
         END IF

         GO TO 11

 999     EOF = .TRUE.
 11      CONTINUE
      END DO

C     Write OU Pathway Images to EVENT File, Allowing For Column Shift
      IF (LOCB(1) .EQ. 1) THEN
         WRITE(IEVUNT,1011) EVPARM
      ELSE IF (LOCB(1) .EQ. 2) THEN
         WRITE(IEVUNT,1012) EVPARM
      ELSE IF (LOCB(1) .EQ. 3) THEN
         WRITE(IEVUNT,1013) EVPARM
      ELSE IF (LOCB(1) .EQ. 4) THEN
         WRITE(IEVUNT,1014) EVPARM
      END IF

      CLOSE(UNIT=IEVUNT)

C 1001 FORMAT(A80)
C 1002 FORMAT(1X,A80)
C 1003 FORMAT(2X,A80)
C 1004 FORMAT(3X,A80)
 1061 FORMAT(A63)
 1062 FORMAT(1X,A63)
 1063 FORMAT(2X,A63)
 1064 FORMAT(3X,A63)
 1011 FORMAT(/'OU STARTING',
     &       /'   EVENTOUT  ',A6,
     &       /'OU FINISHED')
 1012 FORMAT(/' OU STARTING',
     &       /'    EVENTOUT  ',A6,
     &       /' OU FINISHED')
 1013 FORMAT(/'  OU STARTING',
     &       /'     EVENTOUT  ',A6,
     &       /'  OU FINISHED')
 1014 FORMAT(/'   OU STARTING',
     &       /'      EVENTOUT  ',A6,
     &       /'   OU FINISHED')

      RETURN
      END

      SUBROUTINE MXEVNT
C***********************************************************************
C                 MXEVNT Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Generate EVENT File Inputs From
C                 Maximum Value (>Threshold) Files
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To add one more decimal place to receptor elevations
C                    and flagpole heights for the event file.
C                    R.W. Brode, PES, Inc. - November 15, 1995.
C
C        INPUTS:  Maximum Value Files
C
C        OUTPUTS: Events for EVENT Input Runstream File
C
C        CALLED FROM: EVEFIL
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER NAMEEV*8, GID*8, BUFIN*80

C     Variable Initializations
      MODNAM = 'MXEVNT'

C     Begin Averaging Period LOOP
      DO 500 IAVE = 1, NUMAVE
C        Initialize Event Counter for This IAVE
         NUMEVE = 0
C        Begin Source Group LOOP
         DO 400 IGRP = 1, NUMGRP
            IF (MAXFLE(IGRP,IAVE) .EQ. 1) THEN
C              Maximum Value File Exists for This Group and AvePer
C              Rewind File
               REWIND IMXUNT(IGRP,IAVE)
               EOF = .FALSE.

C              Loop Through Threshold File and Write Out Events to EVENT File
               DO WHILE (.NOT. EOF)
                  READ(IMXUNT(IGRP,IAVE),100,ERR=99,END=999) BUFIN
 100              FORMAT(A80)
C                 Skip Record if Part of Header, '*' in Column 1
                  IF (BUFIN(1:1) .EQ. '*') GO TO 11
                  READ(BUFIN,THRFRM,ERR=99) IAVEP,
     &                 GID, KDATE, XR2, YR2, ZE2, ZF2, CONC1
                  IF (IAVEP.NE.720 .AND. IAVEP.EQ.KAVE(IAVE) .AND.
     &                                     GID.EQ.GRPID(IGRP)) THEN
C                    Increment Event Counter and Generate Event Name
                     NUMEVE = NUMEVE + 1
                     IF (NUMEVE .GT. 9999) THEN
C                       Number of Events Exceeds Limit of Field, Reset to 1
                        NUMEVE = 1
                     END IF
                     WRITE(NAMEEV,'(2HTH,I2.2,I4.4)') IAVEP, NUMEVE
C                    Write EVENTPER & EVENTLOC Cards, Allowing for Col. Shift
                     IF (LOCB(1) .EQ. 1) THEN
                        WRITE(IEVUNT,1901) NAMEEV,IAVEP,GID,KDATE,CONC1
                        WRITE(IEVUNT,1911) NAMEEV, XR2, YR2, ZE2, ZF2
                     ELSE IF (LOCB(1) .EQ. 2) THEN
                        WRITE(IEVUNT,1902) NAMEEV,IAVEP,GID,KDATE,CONC1
                        WRITE(IEVUNT,1912) NAMEEV, XR2, YR2, ZE2, ZF2
                     ELSE IF (LOCB(1) .EQ. 3) THEN
                        WRITE(IEVUNT,1903) NAMEEV,IAVEP,GID,KDATE,CONC1
                        WRITE(IEVUNT,1913) NAMEEV, XR2, YR2, ZE2, ZF2
                     ELSE IF (LOCB(1) .EQ. 4) THEN
                        WRITE(IEVUNT,1904) NAMEEV,IAVEP,GID,KDATE,CONC1
                        WRITE(IEVUNT,1914) NAMEEV, XR2, YR2, ZE2, ZF2
                     END IF
                     GO TO 11
                  ELSE
                     GO TO 11
                  END IF

 999              EOF = .TRUE.
 11               CONTINUE
               END DO

            END IF
 400     CONTINUE
C        End Source Group LOOP
 500  CONTINUE
C     End Averaging Period LOOP

      GO TO 1000

C     WRITE Error Message for Error Reading Threshold File
 99   WRITE(DUMMY,'(5HMAXFL,I3.3)') IMXUNT(IGRP,IAVE)
      CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)

 1901 FORMAT(3X,'EVENTPER',1X,A8,1X,I3,2X,A8,3X,I8,3X,F14.5)
 1902 FORMAT(4X,'EVENTPER',1X,A8,1X,I3,2X,A8,3X,I8,3X,F14.5)
 1903 FORMAT(5X,'EVENTPER',1X,A8,1X,I3,2X,A8,3X,I8,3X,F14.5)
 1904 FORMAT(6X,'EVENTPER',1X,A8,1X,I3,2X,A8,3X,I8,3X,F14.5)
 1911 FORMAT(3X,'EVENTLOC',1X,A8,1X,'XR= ',F14.5,' YR= ',F14.5,
     &       2(1X,F10.4))
 1912 FORMAT(4X,'EVENTLOC',1X,A8,1X,'XR= ',F14.5,' YR= ',F14.5,
     &       2(1X,F10.4))
 1913 FORMAT(5X,'EVENTLOC',1X,A8,1X,'XR= ',F14.5,' YR= ',F14.5,
     &       2(1X,F10.4))
 1914 FORMAT(6X,'EVENTLOC',1X,A8,1X,'XR= ',F14.5,' YR= ',F14.5,
     &       2(1X,F10.4))

 1000 RETURN
      END

      SUBROUTINE SOELUN
C***********************************************************************
C                 SOELUN Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process Elevation Units Option for Sources
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    November 22, 1994
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Source Elevation Units Switch
C
C        ERROR HANDLING:   Checks for Invalid Parameters;
C                          Checks for No Parameters;
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   SOCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'SOELUN'

      IF (IFC .EQ. 3) THEN
         IF (FIELD(3) .EQ. 'METERS') THEN
            SOELEV = 'METERS'
         ELSE IF (FIELD(3) .EQ. 'FEET') THEN
            SOELEV = 'FEET'
         ELSE
C           WRITE Error Message  ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203','SO_ELEV')
         END IF
      ELSE IF (IFC .GT. 3) THEN
C        WRITE Error Message     ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Error Message     ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200','ElevUnit')
      END IF

 999  RETURN
      END

      SUBROUTINE SOLOCA
C***********************************************************************
C                 SOLOCA Module of ISC2 Model
C
C        PURPOSE: Processes Source Location Card
C
C        PROGRAMMER:  Jeff Wang, Roger Brode
C
C*       MODIFIED BY: Jayant Hardikar (PES) 7/19/94 to incorporate
C*                    new "PIT" source type.
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Source Type and Location
C
C        CALLED FROM:   SOCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER SOID*8
      LOGICAL FIND

C     Variable Initializations
      FIND = .FALSE.
      MODNAM = 'SOLOCA'

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 6) THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 7) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Read In The Data Fields and Assign to Arrays
C     First Check for Previous Occurrence of This SRCID
      SOID = FIELD(3)
      CALL SINDEX(SRCID,NSRC,SOID,INDEXS,FIND)

      IF (.NOT. FIND) THEN
         ISRC = ISRC + 1
         IF (ISRC .LE. NSRC) THEN
            SRCID(ISRC)  = FIELD(3)
            SRCTYP(ISRC) = FIELD(4)

            IF (SRCTYP(ISRC) .EQ. 'OPENPIT'  .OR.
     &          SRCTYP(ISRC) .EQ. 'OPEN_PIT' .OR.
     &          SRCTYP(ISRC) .EQ. 'OPEN-PIT') THEN
                   SRCTYP(ISRC) = 'OPENPIT'
            ENDIF
            
            
C*          IF (SRCTYP(ISRC).EQ.'POINT' .OR. SRCTYP(ISRC).EQ.
C*   &             'VOLUME' .OR. SRCTYP(ISRC).EQ.'AREA') THEN
            IF (SRCTYP(ISRC).EQ.'POINT' .OR. SRCTYP(ISRC).EQ.
     &             'VOLUME' .OR. SRCTYP(ISRC).EQ.'AREA' .OR.
     &              SRCTYP(ISRC) .EQ. 'OPENPIT') THEN

               CALL STONUM(FIELD(5), 40, AXS(ISRC), IMIT)
C              Check The Numerical Field
               IF (IMIT .NE. 1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               END IF
               CALL STONUM(FIELD(6), 40, AYS(ISRC), IMIT)
C              Check The Numerical Field
               IF (IMIT .NE. 1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               END IF
               IF (IFC .EQ. 7) THEN
C                 Retrieve Source Elevation From Inputs
                  CALL STONUM(FIELD(7), 40, AZS(ISRC), IMIT)
C                 Check The Numerical Field
                  IF (IMIT .NE. 1) THEN
                     CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  END IF
C                 Check for units conversion from feet to meters
                  IF (SOELEV .EQ. 'FEET') THEN
                     AZS(ISRC) = AZS(ISRC) * 0.3048
                  END IF
               ELSE
C                 No Source Elevation Field - Default to 0.0
                  AZS(ISRC) = 0.0
                  IF (ELEV) THEN
C                    Write Warning Message for No Source Elevation with ELEV
                     CALL ERRHDL(PATH,MODNAM,'W','205','ZS = 0.0')
                  END IF
               END IF
            ELSE
C              Error Message: Invalid Source Type
               CALL ERRHDL(PATH,MODNAM,'E','203','SRCTYP')
               GO TO 999
            END IF
            ISET = ISRC
            NUMSRC = NUMSRC + 1
         ELSE
C           WRITE Error Message    ! Number of Sources Exceeds NSRC Parameter
            WRITE(DUMMY,'(I8)') NSRC
            CALL ERRHDL(PATH,MODNAM,'E','232',DUMMY)
            GO TO 999
         END IF
      ELSE
C        WRITE Error Message    ! Source Location Has Already Been Identified
         CALL ERRHDL(PATH,MODNAM,'E','310',SOID)
      END IF

 999  RETURN
      END

      SUBROUTINE SOPARM
C***********************************************************************
C                 SOPARM Module of ISC2 Model
C
C        PURPOSE: Processes Source parameter Card
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To allow for additional parameters on area source
C                    parameter cards for new algorithm - 7/7/93
C
C*       MODIFIED BY: Jayant Hardikar (PES) 7/19/94 to incorporate
C*                    new "PIT" source type.
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Source Parameters
C
C        CALLED FROM:   SOCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      LOGICAL FIND
      REAL TEMP(IFMAX)

C     Variable Initializations
      FIND = .FALSE.
      MODNAM = 'SOPARM'

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .EQ. 3) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      END IF

C     Search For The Source ID Index
      CALL SINDEX(SRCID,NSRC,FIELD(3),ISDX,FIND)

      IF (FIND) THEN
C        Check for Previous SRCPARAM Card for This Source
         IF (SOPCRD(ISDX) .EQ. 'Y') THEN
C           WRITE Error Message: Duplicate SRCPARAM Card
            CALL ERRHDL(PATH,MODNAM,'E','315',SRCID(ISDX))
            GO TO 999
         ELSE
            SOPCRD(ISDX) = 'Y'
         END IF
C        Assign The Parameter Arrays
         DO 50 I = 4, IFC
            CALL STONUM(FIELD(I),40,TEMP(I-3),IMIT)
C           Check The Numerical Field
            IF (IMIT .NE. 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 999
            END IF
 50      CONTINUE
         IF (SRCTYP(ISDX) .EQ. 'POINT') THEN
            IF (IFC .EQ. 3) THEN
C              Error Message: No Parameters
               CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
               GO TO 999
            ELSE IF (IFC .LT. 8) THEN
C              Error Message: Not Enough Parameters
               CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
               GO TO 999
            ELSE IF (IFC .GT. 8) THEN
C              Error Message: Too Many Parameters
               CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
               GO TO 999
            END IF
            CALL PPARM(ISDX,TEMP)
         ELSE IF (SRCTYP(ISDX) .EQ. 'VOLUME') THEN
            IF (IFC .EQ. 3) THEN
C              Error Message: No Parameters
               CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
               GO TO 999
            ELSE IF (IFC .LT. 7) THEN
C              Error Message: Not Enough Parameters
               CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
               GO TO 999
            ELSE IF (IFC .GT. 7) THEN
C              Error Message: Too Many Parameters
               CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
               GO TO 999
            END IF
            CALL VPARM(ISDX,TEMP)
         ELSE IF (SRCTYP(ISDX) .EQ. 'AREA') THEN
            IF (IFC .EQ. 3) THEN
C              Error Message: No Parameters
               CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
               GO TO 999
            ELSE IF (IFC .LT. 6) THEN
C              Error Message: Not Enough Parameters
               CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
               GO TO 999
            ELSE IF (IFC .GT. 9) THEN
C              Error Message: Too Many Parameters
               CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
               GO TO 999
            END IF
            CALL APARM(ISDX,TEMP)
C*       Get Source Parameters for the OPENPIT source
         ELSE IF (SRCTYP(ISDX) .EQ. 'OPENPIT') THEN
            IF (IFC .EQ. 3) THEN
C              Error Message: No Parameters
               CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
               GO TO 999
            ELSE IF (IFC .LT. 8) THEN
C              Error Message: Not Enough Parameters
               CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
               GO TO 999
            ELSE IF (IFC .GT. 9) THEN
C              Error Message: Too Many Parameters
               CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
               GO TO 999
            END IF
            CALL OPARM(ISDX,TEMP)
            
         END IF
      ELSE
C        WRITE Error Message    ! Source Location Has Not Been Identified Yet
         CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
      END IF

 999  RETURN
      END

      SUBROUTINE PPARM(ISDX,TEMP)
C***********************************************************************
C                 PPARM Module of ISC2 Model
C
C        PURPOSE: Processes Source Parameters for POINT Sources
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Source Parameter Card
C
C        CALLED FROM:   SOPARM
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      REAL TEMP(IFMAX)

C     Variable Initializations
      MODNAM = 'PPARM'

      AQS(ISDX) = TEMP(1)
      AHS(ISDX) = TEMP(2)
      ATS(ISDX) = TEMP(3)
      AVS(ISDX) = TEMP(4)
      ADS(ISDX) = TEMP(5)

C     Perform QA Error Checking on Source Parameters

      IF (AQS(ISDX) .EQ. 0.0) THEN
C        WRITE Warning Message:  Emission Rate Equals 0.0
         CALL ERRHDL(PATH,MODNAM,'W','320',' QS ')
      END IF

      IF (AHS(ISDX) .LT. 0.0) THEN
C        WRITE Error Message:  Negative Release Height
         CALL ERRHDL(PATH,MODNAM,'E','209',' HS ')
      ELSE IF (AHS(ISDX) .GT. 600.0) THEN
C        WRITE Warning Message:  Large Release Height (> 600M)
         CALL ERRHDL(PATH,MODNAM,'W','320',' HS ')
      END IF

      IF (ATS(ISDX) .EQ. 0.0) THEN
C        Set Temperature to Small Negative Value for Ambient Releases
         ATS(ISDX) = -1.0E-5
      ELSE IF (ATS(ISDX) .GT. 2000.0) THEN
C        WRITE Warning Message:  Exit Temp. > 2000K
         CALL ERRHDL(PATH,MODNAM,'W','320',' TS ')
      END IF

      IF (AVS(ISDX) .LT. 0.0) THEN
C        WRITE Warning Message:  Negative or Zero Exit Velocity
         CALL ERRHDL(PATH,MODNAM,'W','325',' VS ')
C        Set to Small Value to Avoid Zero-divide and Underflow
         AVS(ISDX) = 1.0E-5
      ELSE IF (AVS(ISDX) .LT. 1.0E-5) THEN
C        Set to Small Value to Avoid Zero-divide and Underflow
         AVS(ISDX) = 1.0E-5
      ELSE IF (AVS(ISDX) .GT. 50.0) THEN
C        WRITE Warning Message:  Exit Velocity > 50.0 m/s
         CALL ERRHDL(PATH,MODNAM,'W','320',' VS ')
      END IF

      IF (ADS(ISDX) .LT. 0.0) THEN
C        WRITE Warning Message:  Negative Stack Diameter
         CALL ERRHDL(PATH,MODNAM,'E','209',' DS ')
      ELSE IF (ADS(ISDX) .LT. 1.0E-5) THEN
C        Set to Small Value to Avoid Zero-divide and Underflow
         ADS(ISDX) = 1.0E-5
      ELSE IF (ADS(ISDX) .GT. 20.0) THEN
C        WRITE Warning Message:  Large Stack Diameter (> 20m)
         CALL ERRHDL(PATH,MODNAM,'W','320',' DS ')
      END IF

      RETURN
      END

      SUBROUTINE VPARM(ISDX,TEMP)
C***********************************************************************
C                 VPARM Module of ISC2 Model
C
C        PURPOSE: Processes Source Parameters for VOLUME Sources
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Source Parameter Card
C
C        CALLED FROM:   SOPARM
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      REAL TEMP(IFMAX)

C     Variable Initializations
      MODNAM = 'VPARM'

      AQS(ISDX) = TEMP(1)
      AHS(ISDX) = TEMP(2)
      ASYINI(ISDX) = TEMP(3)
      ASZINI(ISDX) = TEMP(4)

C     Perform QA Error Checking on Source Parameters

      IF (AQS(ISDX) .EQ. 0.0) THEN
C        WRITE Warning Message:  Emission Rate Equals 0.0
         CALL ERRHDL(PATH,MODNAM,'W','320',' QS ')
      END IF

      IF (AHS(ISDX) .LT. 0.0) THEN
C        WRITE Error Message:  Negative Release Height
         CALL ERRHDL(PATH,MODNAM,'E','209',' HS ')
      ELSE IF (AHS(ISDX) .GT. 100.0) THEN
C        WRITE Warning Message:  Large Release Height (> 100M)
         CALL ERRHDL(PATH,MODNAM,'W','320',' HS')
      END IF

      IF (ASYINI(ISDX) .LT. 0.0) THEN
C        WRITE Warning Message:  Negative Initial Lateral Parameter
         CALL ERRHDL(PATH,MODNAM,'E','209',' SYINIT ')
      ELSE IF (ASYINI(ISDX) .LT. 1.0E-5) THEN
C        WRITE Warning Message:  Small Initial Lateral Parameter
         CALL ERRHDL(PATH,MODNAM,'W','320',' SYINIT ')
C        Set to Small Value to Avoid Zero-divide and Underflow
         ASYINI(ISDX) = 1.0E-5
      ELSE IF (ASYINI(ISDX) .GT. 200.0) THEN
C        WRITE Warning Message:  Large Initial Lateral Parameter (> 200m)
         CALL ERRHDL(PATH,MODNAM,'W','320',' SYINIT ')
      END IF

      IF (ASZINI(ISDX) .LT. 0.0) THEN
C        WRITE Warning Message:  Negative Initial Vertical Parameter
         CALL ERRHDL(PATH,MODNAM,'E','209',' SZINIT ')
      ELSE IF (ASZINI(ISDX) .LT. 1.0E-5) THEN
C        WRITE Warning Message:  Small Initial Lateral Parameter
         CALL ERRHDL(PATH,MODNAM,'W','320',' SZINIT ')
C        Set to Small Value to Avoid Zero-divide and Underflow
         ASZINI(ISDX) = 1.0E-5
      ELSE IF (ASZINI(ISDX) .GT. 200.0) THEN
C        WRITE Warning Message:  Large Initial Vertical Parameter (> 200m)
         CALL ERRHDL(PATH,MODNAM,'W','320',' SZINIT ')
      END IF

      RETURN
      END

      SUBROUTINE APARM(ISDX,TEMP)
C***********************************************************************
C                 APARM Module of ISC2 Model
C
C        PURPOSE: Processes Source Parameters for AREA Sources
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To allow for additional parameters on area source
C                    parameter cards for new algorithm - 7/7/93
C
C        MODIFIED:   Corrected IF-BLOCK for error checking - 7/21/94
C
C        MODIFIED BY Roger Brode, PES (modified data structure for
C                    AXVERT and AYVERT for consistency with other
C                    2-D source arrays) - 8/15/95
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Source Parameter Card
C
C        CALLED FROM:   SOPARM
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      REAL TEMP(IFMAX)

C     Variable Initializations
      MODNAM = 'APARM'

      AQS(ISDX) = TEMP(1)
      AHS(ISDX) = TEMP(2)
      IF (IFC .EQ. 6) THEN
         AXINIT(ISDX) = TEMP(3)
         AYINIT(ISDX) = AXINIT(ISDX)
         AANGLE(ISDX) = 0.
         ASZINI(ISDX) = 0.
      ELSE IF (IFC .EQ. 7) THEN
         AXINIT(ISDX) = TEMP(3)
         AYINIT(ISDX) = TEMP(4)
         AANGLE(ISDX) = 0.
         ASZINI(ISDX) = 0.
      ELSE IF (IFC .EQ. 8) THEN
         AXINIT(ISDX) = TEMP(3)
         AYINIT(ISDX) = TEMP(4)
         AANGLE(ISDX) = TEMP(5)
         ASZINI(ISDX) = 0.

C*----   ISCSTM Modification: allow for initial sigma-Z - jah 11/2/94                        
      ELSE IF (IFC .EQ. 9) THEN
         AXINIT(ISDX) = TEMP(3)
         AYINIT(ISDX) = TEMP(4)
         AANGLE(ISDX) = TEMP(5)
         ASZINI(ISDX) = TEMP(6)
C*#
      END IF

C     Perform QA Error Checking on Source Parameters

      IF (AQS(ISDX) .EQ. 0.0) THEN
C        WRITE Warning Message:  Emission Rate Equals 0.0
         CALL ERRHDL(PATH,MODNAM,'W','320',' QS ')
      END IF

      IF (AHS(ISDX) .LT. 0.0) THEN
C        WRITE Error Message:  Negative Release Height
         CALL ERRHDL(PATH,MODNAM,'E','209',' HS ')
      ELSE IF (AHS(ISDX) .GT. 100.0) THEN
C        WRITE Warning Message:  Large Release Height (> 100M)
         CALL ERRHDL(PATH,MODNAM,'W','320',' HS ')
      END IF

      IF (AXINIT(ISDX) .LT. 0.0) THEN
C        WRITE Error Message:  Negative Area Size
         CALL ERRHDL(PATH,MODNAM,'E','209',' XINIT ')
      ENDIF
      IF (AYINIT(ISDX) .LT. 0.0) THEN
C        WRITE Error Message:  Negative Area Size
         CALL ERRHDL(PATH,MODNAM,'E','209',' YINIT ')
      ENDIF
      IF (AXINIT(ISDX) .LT. 1.0E-5) THEN
C        WRITE Warning Message:  Small Source Area
         CALL ERRHDL(PATH,MODNAM,'W','320',' XINIT ')
C        Set to Small Value to Avoid Zero-divide and Underflow
         AXINIT(ISDX) = 1.0E-5
      ENDIF
      IF (AYINIT(ISDX) .LT. 1.0E-5) THEN
C        WRITE Warning Message:  Small Source Area
         CALL ERRHDL(PATH,MODNAM,'W','320',' YINIT ')
C        Set to Small Value to Avoid Zero-divide and Underflow
         AYINIT(ISDX) = 1.0E-5
      ENDIF
      IF (AXINIT(ISDX) .GT. 2000.0) THEN
C        WRITE Warning Message:  Large Source Area (> 2000m)
         CALL ERRHDL(PATH,MODNAM,'W','320',' XINIT ')
      ENDIF
      IF (AYINIT(ISDX) .GT. 2000.0) THEN
C        WRITE Warning Message:  Large Source Area (> 2000m)
         CALL ERRHDL(PATH,MODNAM,'W','320',' YINIT ')
      ENDIF
      IF (ABS(AANGLE(ISDX)) .GT. 180. ) THEN
C        WRITE Warning Message:  Rotation Angle Larger Than 180 Degrees
         CALL ERRHDL(PATH,MODNAM,'W','320',' ANGLE ')
      ENDIF
      
C*----   ISCSTM Modification: allow for initial sigma-Z - jah 11/2/94                        
      IF (ASZINI(ISDX) .LT. 0.0) THEN
C*       WRITE Warning Message:  Negative Initial Vertical Parameter
         CALL ERRHDL(PATH,MODNAM,'E','209',' SZINIT ')
      ELSE IF (ASZINI(ISDX) .LT. 1.0E-5) THEN
C*       Set to Small Value to Avoid Zero-divide and Underflow
         ASZINI(ISDX) = 1.0E-5
      ELSE IF (ASZINI(ISDX) .GT. 200.0) THEN
C*       WRITE Warning Message:  Large Initial Vertical Parameter (> 200m)
         CALL ERRHDL(PATH,MODNAM,'W','320',' SZINIT ')
      END IF
C*----

C     Check for aspect ratio (length/width) > 10
      IF (AYINIT(ISDX)/AXINIT(ISDX) .GT. 10.0 .OR.
     &    AXINIT(ISDX)/AYINIT(ISDX) .GT. 10.0) THEN
C        WRITE Warning Message: Aspect ratio > 10 for area source
         CALL ERRHDL(PATH,MODNAM,'W','391',SRCID(ISDX))
      END IF

C     Set Number of Vertices (4 for Rectangular Source)
      NVERT = 4

C     Set Coordinates of Vertices for Rectangular Area (in Kilometers).
C     Vertices Start with the "Southwest" Corner and Are Defined
C     Clockwise.  The First Vertex is Repeated as the Last Vertex.

      AXVERT(1,ISDX) = AXS(ISDX)/1000.
      AYVERT(1,ISDX) = AYS(ISDX)/1000.

      AXVERT(2,ISDX) = AXVERT(1,ISDX) +
     &                (AYINIT(ISDX)*SIN(AANGLE(ISDX)*DTORAD))/1000.
      AYVERT(2,ISDX) = AYVERT(1,ISDX) +
     &                (AYINIT(ISDX)*COS(AANGLE(ISDX)*DTORAD))/1000.

      AXVERT(3,ISDX) = AXVERT(2,ISDX) +
     &                (AXINIT(ISDX)*COS(AANGLE(ISDX)*DTORAD))/1000.
      AYVERT(3,ISDX) = AYVERT(2,ISDX) -
     &                (AXINIT(ISDX)*SIN(AANGLE(ISDX)*DTORAD))/1000.

      AXVERT(4,ISDX) = AXVERT(3,ISDX) -
     &                (AYINIT(ISDX)*SIN(AANGLE(ISDX)*DTORAD))/1000.
      AYVERT(4,ISDX) = AYVERT(3,ISDX) -
     &                (AYINIT(ISDX)*COS(AANGLE(ISDX)*DTORAD))/1000.

      AXVERT(5,ISDX) = AXS(ISDX)/1000.
      AYVERT(5,ISDX) = AYS(ISDX)/1000.

      RETURN
      END

      SUBROUTINE OPARM(ISDX,TEMP)
C***********************************************************************
C                 OPARM Module of ISC2 Model
C
C        PURPOSE: Processes Source Parameters for OPENPIT Sources
C
C        PROGRAMMER: Jayant Hardikar, Roger Brode
C                    (based on APARM - Jeff Wang/Roger Brode)
C
C        DATE:       July 19, 1994
C
C        MODIFIED BY Roger Brode, PES (modified data structure for
C                    AXVERT and AYVERT for consistency with other
C                    2-D source arrays) - 8/15/95
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Source Parameter Card
C
C        CALLED FROM:   SOPARM
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      REAL TEMP(IFMAX)

C     Variable Initializations
      MODNAM = 'OPARM'

      AQS(ISDX) = TEMP(1)
      AHS(ISDX) = TEMP(2)
      AXINIT(ISDX) = TEMP(3)
      AYINIT(ISDX) = TEMP(4)
      AVOLUM(ISDX) = TEMP(5)
      AANGLE(ISDX) = 0.      
      IF (IFC .EQ. 9) THEN
         AANGLE(ISDX) = TEMP(6)
      END IF

C     Perform QA Error Checking on Source Parameters

      IF (AQS(ISDX) .EQ. 0.0) THEN
C        WRITE Warning Message:  Emission Rate Equals 0.0
         CALL ERRHDL(PATH,MODNAM,'W','320',' QS ')
      END IF

      IF (AHS(ISDX) .LT. 0.0) THEN
C        WRITE Error Message:  Negative Release Height
         CALL ERRHDL(PATH,MODNAM,'E','209',' HS ')
      ELSE IF (AHS(ISDX) .GT. 200.0) THEN
C        WRITE Warning Message:  Large Release Height (> 200M)
         CALL ERRHDL(PATH,MODNAM,'W','320',' HS ')
      END IF

      IF (AXINIT(ISDX) .LT. 0.0) THEN
C        WRITE Error Message:  Negative Area Size
         CALL ERRHDL(PATH,MODNAM,'E','209',' XINIT ')
      ENDIF
      IF (AYINIT(ISDX) .LT. 0.0) THEN
C        WRITE Error Message:  Negative Area Size
         CALL ERRHDL(PATH,MODNAM,'E','209',' YINIT ')
      ENDIF
      IF (AXINIT(ISDX) .LT. 1.0E-5) THEN
C        WRITE Warning Message:  Small Source Area
         CALL ERRHDL(PATH,MODNAM,'W','320',' XINIT ')
C        Set to Small Value to Avoid Zero-divide and Underflow
         AXINIT(ISDX) = 1.0E-5
      ENDIF
      IF (AYINIT(ISDX) .LT. 1.0E-5) THEN
C        WRITE Warning Message:  Small Source Area
         CALL ERRHDL(PATH,MODNAM,'W','320',' YINIT ')
C        Set to Small Value to Avoid Zero-divide and Underflow
         AYINIT(ISDX) = 1.0E-5
      ENDIF
      IF (AXINIT(ISDX) .GT. 2000.0) THEN
C        WRITE Warning Message:  Large Source Area (> 2000m)
         CALL ERRHDL(PATH,MODNAM,'W','320',' XINIT ')
      ENDIF
      IF (AYINIT(ISDX) .GT. 2000.0) THEN
C        WRITE Warning Message:  Large Source Area (> 2000m)
         CALL ERRHDL(PATH,MODNAM,'W','320',' YINIT ')
      ENDIF
      IF (ABS(AANGLE(ISDX)) .GT. 180. ) THEN
C        WRITE Warning Message:  Rotation Angle Larger Than 180 Degrees
         CALL ERRHDL(PATH,MODNAM,'W','320',' ANGLE ')
      ENDIF
      IF (AVOLUM(ISDX) .LE. 0.0) THEN
C        WRITE Error Message: Open-Pit Volume is less than
C        or equal to zero
         CALL ERRHDL(PATH,MODNAM,'E','209',' AVOLUM ')
      ENDIF

C     Check for aspect ratio (length/width) > 10
      IF (AYINIT(ISDX)/AXINIT(ISDX) .GT. 10.0 .OR.
     &    AXINIT(ISDX)/AYINIT(ISDX) .GT. 10.0) THEN
C        WRITE Warning Message: Aspect ratio > 10 for pit source
         CALL ERRHDL(PATH,MODNAM,'W','392',SRCID(ISDX))
      END IF

C     Check for Release Height > Effective Depth
      EFFDEP = AVOLUM(ISDX)/(AXINIT(ISDX)*AYINIT(ISDX))
      IF (AHS(ISDX) .GT. EFFDEP) THEN
C        WRITE Error Message: Release Height is greater than Effective Depth
         CALL ERRHDL(PATH,MODNAM,'E','322',SRCID(ISDX))
      END IF

C     Set Number of Vertices (4 for Rectangular Source)
      NVERT = 4

C     Set Coordinates of Vertices for Rectangular Area (in Kilometers).
C     Vertices Start with the "Southwest" Corner and Are Defined
C     Clockwise.  The First Vertex is Repeated as the Last Vertex.

      AXVERT(1,ISDX) = AXS(ISDX)/1000.
      AYVERT(1,ISDX) = AYS(ISDX)/1000.

      AXVERT(2,ISDX) = AXVERT(1,ISDX) +
     &                (AYINIT(ISDX)*SIN(AANGLE(ISDX)*DTORAD))/1000.
      AYVERT(2,ISDX) = AYVERT(1,ISDX) +
     &                (AYINIT(ISDX)*COS(AANGLE(ISDX)*DTORAD))/1000.

      AXVERT(3,ISDX) = AXVERT(2,ISDX) +
     &                (AXINIT(ISDX)*COS(AANGLE(ISDX)*DTORAD))/1000.
      AYVERT(3,ISDX) = AYVERT(2,ISDX) -
     &                (AXINIT(ISDX)*SIN(AANGLE(ISDX)*DTORAD))/1000.

      AXVERT(4,ISDX) = AXVERT(3,ISDX) -
     &                (AYINIT(ISDX)*SIN(AANGLE(ISDX)*DTORAD))/1000.
      AYVERT(4,ISDX) = AYVERT(3,ISDX) -
     &                (AYINIT(ISDX)*COS(AANGLE(ISDX)*DTORAD))/1000.

      AXVERT(5,ISDX) = AXS(ISDX)/1000.
      AYVERT(5,ISDX) = AYS(ISDX)/1000.

C*    Determine the angle of long pit dimension with North
      IF (AYINIT(ISDX) .GE. AXINIT(ISDX)) THEN
         AALPHA(ISDX) = AANGLE(ISDX)
      ELSE IF (AXINIT(ISDX) .GT. AYINIT(ISDX)) THEN
         AALPHA(ISDX) = AANGLE(ISDX) + 90.0
      ENDIF
      
C*    Calculate the effective pit depth
      APDEFF(ISDX) = AVOLUM(ISDX) / (AXINIT(ISDX) * AYINIT(ISDX))

C*    Calculate Initial Sigma-Z
      ASZINI(ISDX) = APDEFF(ISDX) / 4.3      

      RETURN
      END


      SUBROUTINE DSBLDG
C***********************************************************************
C                 DSBLDG Module of ISC2 Model
C
C        PURPOSE: Processes Direction-specific Building Directions
C
C        PROGRAMMER:  Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Direction Specific Building Directions
C
C        CALLED FROM:   SOCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER LID*8, HID*8, LID1*8, LID2*8, HID1*8, HID2*8, SOID*40
      LOGICAL FIND, INGRP, RMARK

C     Variable Initializations
      FIND = .FALSE.
      INGRP =  .FALSE.
      MODNAM = 'DSBLDG'

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .EQ. 3) THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      END IF

C     Get The Source ID(s)
      SOID = FIELD(3)
      CALL FSPLIT(PATH,KEYWRD,SOID,40,'-',RMARK,LID,HID)

C     Verify The Effective Srcid
      IF (LID .EQ. HID) THEN
C        Search For The Index
         CALL SINDEX(SRCID,NSRC,SOID,ISDX,FIND)
         IF (FIND) THEN
            IF (SRCTYP(ISDX) .EQ. 'POINT') THEN
C              Fill Array
               CALL DSFILL(ISDX)
            ELSE
C              WRITE Warning Message: Building Inputs for Non-POINT Source
               CALL ERRHDL(PATH,MODNAM,'W','233',SRCID(ISDX))
            END IF
         ELSE
C           WRITE Error Message     ! Source Location Has Not Been Identified
            CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
         END IF
      ELSE
C        First Check Range for Upper Value < Lower Value
         CALL SETIDG(LID,LID1,IL,LID2)
         CALL SETIDG(HID,HID1,IH,HID2)
         IF ((HID1.LT.LID1) .OR. (IH.LT.IL) .OR. (HID2.LT.LID2)) THEN
C           WRITE Error Message:  Invalid Range,  Upper < Lower
            CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
            GO TO 999
         END IF
         DO 20 I = 1, NUMSRC
C           See Whether It's In The Group
            CALL ASNGRP(SRCID(I),LID,HID,INGRP)
            IF (INGRP .AND. SRCTYP(I).EQ.'POINT') THEN
               ISDX = I
C              Fill DS Array
               CALL DSFILL(ISDX)
            END IF
 20      CONTINUE
      END IF

 999  RETURN
      END

      SUBROUTINE EMVARY
C***********************************************************************
C                 EMVARY Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Processes Variable Emission Rate Factors
C
C        PROGRAMMER:  Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Variable Emmission Rate Factors
C
C        CALLED FROM:   SOCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER LID*8, HID*8, LID1*8, LID2*8, HID1*8, HID2*8, SOID*40
      LOGICAL FIND, INGRP, RMARK

C     Variable Initializations
      FIND = .FALSE.
      INGRP = .FALSE.
      MODNAM = 'EMVARY'

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .EQ. 3) THEN
C        Error Message: No Numerical Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 5) THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      END IF

C     Get The Source ID(s)
      SOID = FIELD(3)
      CALL FSPLIT(PATH,KEYWRD,SOID,40,'-',RMARK,LID,HID)

C     Verify The Effective Srcid
      IF (LID .EQ. HID) THEN
C        Search For The Index
         CALL SINDEX(SRCID,NSRC,SOID,ISDX,FIND)
         IF (FIND) THEN
            QFLAG(ISDX) = FIELD(4)
            IF (QFLAG(ISDX) .EQ. 'SEASON') THEN
               IQMAX = 4
            ELSE IF (QFLAG(ISDX) .EQ. 'MONTH') THEN
               IQMAX = 12
            ELSE IF (QFLAG(ISDX) .EQ. 'HROFDY') THEN
               IQMAX = 24
            ELSE IF (QFLAG(ISDX) .EQ. 'STAR') THEN
               IQMAX = 36
            ELSE IF (QFLAG(ISDX) .EQ. 'SEASHR') THEN
               IQMAX = 96
            ELSE
C              WRITE Error Message    ! Invalid QFLAG Field Entered
               CALL ERRHDL(PATH,MODNAM,'E','203','QFLAG')
            END IF
            IF (IQMAX .LE. NQF) THEN
               CALL EFFILL(ISDX,IQMAX)
            ELSE
C              WRITE Error Message     ! NQF Parameter Not Large Enough
               WRITE(DUMMY,'(I8)') NQF
               CALL ERRHDL(PATH,MODNAM,'E','260',DUMMY)
            END IF
         ELSE
C           WRITE Error Message     ! Source Location Has Not Been Identified
            CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
         END IF
      ELSE
C        First Check Range for Upper Value < Lower Value
         CALL SETIDG(LID,LID1,IL,LID2)
         CALL SETIDG(HID,HID1,IH,HID2)
         IF ((HID1.LT.LID1) .OR. (IH.LT.IL) .OR. (HID2.LT.LID2)) THEN
C           WRITE Error Message:  Invalid Range,  Upper < Lower
            CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
            GO TO 999
         END IF
         DO 20 I = 1, NUMSRC
C           See Whether It's In The Group
            CALL ASNGRP(SRCID(I),LID,HID,INGRP)
            IF (INGRP) THEN
               ISDX = I
               QFLAG(ISDX) = FIELD(4)
               IF (QFLAG(ISDX) .EQ. 'SEASON') THEN
                  IQMAX = 4
               ELSE IF (QFLAG(ISDX) .EQ. 'MONTH') THEN
                  IQMAX = 12
               ELSE IF (QFLAG(ISDX) .EQ. 'HROFDY') THEN
                  IQMAX = 24
               ELSE IF (QFLAG(ISDX) .EQ. 'STAR') THEN
                  IQMAX = 36
               ELSE IF (QFLAG(ISDX) .EQ. 'SEASHR') THEN
                  IQMAX = 96
               ELSE
C                 WRITE Error Message    ! Invalid QFLAG Field Entered
                  CALL ERRHDL(PATH,MODNAM,'E','203','QFLAG')
               END IF
               IF (IQMAX .LE. NQF) THEN
                  CALL EFFILL(ISDX,IQMAX)
               ELSE
C                 WRITE Error Message    ! NQF Parameter Not Large Enough
                  WRITE(DUMMY,'(I8)') NQF
                  CALL ERRHDL(PATH,MODNAM,'E','260',DUMMY)
               END IF
            END IF
 20      CONTINUE
      END IF

 999  RETURN
      END

      SUBROUTINE EFFILL(ISDX,IQMAX)
C***********************************************************************
C                 EFFILL Module of ISC2 Model
C
C        PURPOSE: Fill Variable Emission Rate Array
C
C        PROGRAMMER:  Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Direction Specific Building Directions
C
C        CALLED FROM:   EMVARY
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'EFFILL'

      ISET = IWRK2(ISDX,4)

      DO 200 K = 5, IFC
C        Change Fields To Numbers
         CALL STONUM(FIELD(K),40,FNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT.EQ.-1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 200
         END IF
         DO 100 J = 1, IMIT
            ISET = ISET + 1
C           Assign The Field
            IF (ISET .LE. IQMAX) THEN
               QFACT(ISET,ISDX) = FNUM
               IF (FNUM .LT. 0.0) THEN
C                 WRITE Error Message:  Negative Value for QFACT
                  CALL ERRHDL(PATH,MODNAM,'E','209',KEYWRD)
               END IF
            ELSE
C              WRITE Error Message    ! Too Many QFACT Values Input
               CALL ERRHDL(PATH,MODNAM,'E','231','QFACT')
            END IF
 100     CONTINUE
 200  CONTINUE

      IWRK2(ISDX,4) = ISET

      RETURN
      END

      SUBROUTINE EMUNIT
C***********************************************************************
C                 EMUNIT Module of ISC2 Model
C
C        PURPOSE: Processes Emission Rate Unit Conversion Factors
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Emission Rate Unit Conversion Factors
C
C        CALLED FROM:   SOCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'EMUNIT'

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 5) THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 5) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Fetch Each Field
      CALL STONUM(FIELD(3),40,FNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF

      EMIFAC(1) = FNUM
      EMILBL(1) = FIELD(4)
      OUTLBL(1) = FIELD(5)

 999  RETURN
      END

      SUBROUTINE COUNIT
C***********************************************************************
C                 COUNIT Module of ISC2 Model
C
C        PURPOSE: Processes Emission Rate Unit Conversion Factors
C                 for CONCentration Values
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Emission Rate Unit Conversion Factors
C
C        CALLED FROM:   SOCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'COUNIT'

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 5) THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 5) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Fetch Each Field
      CALL STONUM(FIELD(3),40,FNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF

      EMIFAC(1) = FNUM
      EMILBL(1) = FIELD(4)
      OUTLBL(1) = FIELD(5)

 999  RETURN
      END

      SUBROUTINE DPUNIT
C***********************************************************************
C                 DPUNIT Module of ISC2 Model
C
C        PURPOSE: Processes Emission Rate Unit Conversion Factors
C                 for Deposition Values
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Emission Rate Unit Conversion Factors
C
C        CALLED FROM:   SOCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'DPUNIT'

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 5) THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 5) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Fetch Each Field
      CALL STONUM(FIELD(3),40,FNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF

      IF (.NOT. CONC) THEN
         DO I = 1, NTYP
            EMIFAC(I) = FNUM
            EMILBL(I) = FIELD(4)
            OUTLBL(I) = FIELD(5)
         END DO
      ELSE
         DO I = 2, NTYP
            EMIFAC(I) = FNUM
            EMILBL(I) = FIELD(4)
            OUTLBL(I) = FIELD(5)
         END DO
      END IF

 999  RETURN
      END

      SUBROUTINE PARTDEP
C***********************************************************************
C                 PARTDEP Module of ISC2 Model
C
C        ADAPTED from  DRYDEP Module of ISC2 Model
C        PROGRAMMER:  Jeff Wang, Roger Brode
C
C        PURPOSE: Processes Inputs for Wet & Dry PARTicle DEPosition
C
C        DRYDEP ADAPTED BY: D. Strimaitis, SRC (for Wet & Dry Deposition)
C        DATE:    November 8, 1993
C
C        DRYDEP MODIFIED BY: D. Strimaitis, SRC (for Dry Deposition)
C        (DATE:    February 15, 1993)
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Input For Setting and Removal Variables
C
C        CALLED FROM:   SOCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'PARTDE'

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .EQ. 3) THEN
C        Error Message: No Numerical Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      END IF

C     Process The Appropriate Settling & Removal Parameter
      IF (KEYWRD .EQ. 'PARTDIAM') THEN
C        Process Particle Diameter Categories (PDIAM)       ---   CALL INPPDM
         CALL INPPDM
      ELSE IF (KEYWRD .EQ. 'MASSFRAX') THEN
C        Process Mass Fractions (PHI)                       ---   CALL INPPHI
         CALL INPPHI
      ELSE IF (KEYWRD .EQ. 'PARTDENS') THEN
C        Process Particle Density (PDENS)                   ---   CALL INPPDN
         CALL INPPDN
      ELSE IF (KEYWRD .EQ. 'PARTSLIQ') THEN
C        Process Wet(liquid) Scavenging Coefficient (PSLIQ) ---   CALL INPLSC
         CALL INPLSC
C        Set logical LWPART to indicate processing of wet deposition
C        of particles
         LWPART=.TRUE.
      ELSE IF (KEYWRD .EQ. 'PARTSICE') THEN
C        Process Wet(frozen) Scavenging Coefficient (PSICE) ---   CALL INPFSC
         CALL INPFSC
C        Set logical LWPART to indicate processing of wet deposition
C        of particles
         LWPART=.TRUE.
      END IF
C     Set logical LDPART to indicate processing of dry particle deposition
      LDPART=.TRUE.

 999  RETURN
      END

      SUBROUTINE INPPDM
C***********************************************************************
C                 INPPDM Module of ISC2 Model
C
C        PURPOSE: Processes Particle Diameter Categories
C
C        PROGRAMMER: D. Strimaitis, SRC
C
C        ADAPTED FROM "INPVSN"
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    February 15, 1993
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Particle Diameter Categories
C
C        CALLED FROM:   PARTDEP
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER LID*8, HID*8, LID1*8, LID2*8, HID1*8, HID2*8, SOID*40
      LOGICAL FIND, INGRP, RMARK

C     Variable Initializations
      FIND   = .FALSE.
      INGRP  = .FALSE.
      MODNAM = 'INPPDM'

C     Get The Source ID(s)
      SOID = FIELD(3)
      CALL FSPLIT(PATH,KEYWRD,SOID,40,'-',RMARK,LID,HID)

      IF (LID .EQ. HID) THEN
C        Search For The Index
         CALL SINDEX(SRCID,NSRC,SOID,ISDX,FIND)
         IF (FIND) THEN
            ISET = IWRK2(ISDX,5)
            DO 36 K = 4, IFC
C              Change It To Numbers
               CALL STONUM(FIELD(K),40,FNUM,IMIT)
C              Check The Numerical Field
               IF (IMIT .EQ. -1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  GO TO 36
               END IF
               DO 35 J = 1, IMIT
                  ISET = ISET + 1
                  IF (ISET .LE. NPDMAX) THEN
C                    Assign The Field
                     APDIAM(ISET,ISDX) = FNUM
                  ELSE
C                    WRITE Error Message: Too Many PartDiam Categories
                     WRITE(DUMMY,'(I8)') NPDMAX
                     CALL ERRHDL(PATH,MODNAM,'E','245',DUMMY)
                  END IF
 35            CONTINUE
 36         CONTINUE
            IWRK2(ISDX,5) = ISET
         ELSE
C           WRITE Error Message     ! Source Location Has Not Been Identified
            CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
         END IF
      ELSE
C        First Check Range for Upper Value < Lower Value
         CALL SETIDG(LID,LID1,IL,LID2)
         CALL SETIDG(HID,HID1,IH,HID2)
         IF ((HID1.LT.LID1) .OR. (IH.LT.IL) .OR. (HID2.LT.LID2)) THEN
C           WRITE Error Message:  Invalid Range,  Upper < Lower
            CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
            GO TO 999
         END IF
         DO 26 I = 1, NUMSRC
C           See Whether It's In The Group
            CALL ASNGRP(SRCID(I),LID,HID,INGRP)
            IF (INGRP) THEN
               IING = I
               ISET = IWRK2(IING,5)
               DO 25 K = 4, IFC
C                 Get Numbers
                  CALL STONUM(FIELD(K),40,FNUM,IMIT)
C                 Check The Numerical Field
                  IF (IMIT .EQ. -1) THEN
                     CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                     GO TO 26
                  END IF
                  DO 20 J = 1, IMIT
                     ISET = ISET + 1
                     IF (ISET .LE. NPDMAX) THEN
                        APDIAM(ISET,I) = FNUM
                     ELSE
C                       WRITE Error Message: Too Many PartDiam Categories
                        WRITE(DUMMY,'(I8)') NPDMAX
                        CALL ERRHDL(PATH,MODNAM,'E','245',DUMMY)
                     END IF
 20               CONTINUE
 25            CONTINUE
               IWRK2(IING,5) = ISET
            END IF
 26      CONTINUE
      END IF

 999  RETURN
      END

      SUBROUTINE INPPHI
C***********************************************************************
C                 INPPHI Module of ISC2 Model
C
C        PURPOSE: Processes Mass Fraction (PHI) Input Values
C
C        PROGRAMMER:  Jeff Wang, Roger Brode
C        MODIFIED BY: D. Strimaitis, SRC
C
C        DATE:    February 15, 1993
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Mass Fraction Input Values
C
C        CALLED FROM:   PARTDEP
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER LID*8, HID*8, LID1*8, LID2*8, HID1*8, HID2*8, SOID*40
      LOGICAL FIND, INGRP, RMARK

C     Variable Initializations
      FIND = .FALSE.
      INGRP =  .FALSE.
      MODNAM = 'INPPHI'

C     Get The Source ID(s)
      SOID = FIELD(3)
      CALL FSPLIT(PATH,KEYWRD,SOID,40,'-',RMARK,LID,HID)

      IF (LID .EQ. HID) THEN
C        Search For The Index
         CALL SINDEX(SRCID,NSRC,SOID,ISDX,FIND)
         IF (FIND) THEN
            ISET = IWRK2(ISDX,6)
            DO 36 K = 4, IFC
C              Change It To Numbers
               CALL STONUM(FIELD(K),40,FNUM,IMIT)
C              Check The Numerical Field
               IF (IMIT .EQ. -1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  GO TO 36
               END IF
               IF (FNUM .LT. 0.0 .OR. FNUM .GT. 1.0) THEN
C                 WRITE Error Message: Mass Fraction Out-of-Range
                  CALL ERRHDL(PATH,MODNAM,'E','332',SRCID(ISDX))
               END IF
               DO 35 J = 1, IMIT
                  ISET = ISET + 1
                  IF (ISET .LE. NPDMAX) THEN
C                    Assign The Field
                     APHI(ISET,ISDX) = FNUM
                  ELSE
C                    WRITE Error Message: Too Many PartDiam Categories
                     WRITE(DUMMY,'(I8)') NPDMAX
                     CALL ERRHDL(PATH,MODNAM,'E','245',DUMMY)
                  END IF
 35            CONTINUE
 36         CONTINUE
            IWRK2(ISDX,6) = ISET
         ELSE
C           WRITE Error Message     ! Source Location Has Not Been Identified
            CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
         END IF
      ELSE
C        First Check Range for Upper Value < Lower Value
         CALL SETIDG(LID,LID1,IL,LID2)
         CALL SETIDG(HID,HID1,IH,HID2)
         IF ((HID1.LT.LID1) .OR. (IH.LT.IL) .OR. (HID2.LT.LID2)) THEN
C           WRITE Error Message:  Invalid Range,  Upper < Lower
            CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
            GO TO 999
         END IF
         DO 26 I = 1, NUMSRC
C           See Whether It's In The Group
            CALL ASNGRP(SRCID(I),LID,HID,INGRP)
            IF (INGRP) THEN
               IING = I
               ISET = IWRK2(IING,6)
               DO 25 K = 4, IFC
C                 Get Numbers
                  CALL STONUM(FIELD(K),40,FNUM,IMIT)
C                 Check The Numerical Field
                  IF (IMIT .EQ. -1) THEN
                     CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                     GO TO 26
                  END IF
                  IF (FNUM .LT. 0.0 .OR. FNUM .GT. 1.0) THEN
C                    WRITE Error Message: Mass Fraction Out-of-Range
                     CALL ERRHDL(PATH,MODNAM,'E','332',SRCID(ISDX))
                  END IF
                  DO 20 J = 1, IMIT
                     ISET = ISET + 1
                     IF (ISET .LE. NPDMAX) THEN
                        APHI(ISET,I) = FNUM
                     ELSE
C                       WRITE Error Message: Too Many PartDiam Categories
                        WRITE(DUMMY,'(I8)') NPDMAX
                        CALL ERRHDL(PATH,MODNAM,'E','245',DUMMY)
                     END IF
 20               CONTINUE
 25            CONTINUE
               IWRK2(IING,6) = ISET
            END IF
 26      CONTINUE
      END IF

 999  RETURN
      END

      SUBROUTINE INPPDN
C***********************************************************************
C                 INPPDN Module of ISC2 Model
C
C        PURPOSE: Processes Particle Density Input Values
C
C        PROGRAMMER:  D. Strimaitis, SRC
C
C        ADAPTED FROM "INPGAM"
C        PROGRAMMER:  Jeff Wang, Roger Brode
C
C        DATE:    February 15, 1993
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Particle Density Input Values
C
C        CALLED FROM:   PARTDEP
C***********************************************************************
C
C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER LID*8, HID*8, LID1*8, LID2*8, HID1*8, HID2*8, SOID*40
      LOGICAL FIND, INGRP, RMARK

C     Variable Initializations
      FIND = .FALSE.
      INGRP =  .FALSE.
      MODNAM = 'INPPDN'

C     Get The Source ID(s)
      SOID = FIELD(3)
      CALL FSPLIT(PATH,KEYWRD,SOID,40,'-',RMARK,LID,HID)

      IF (LID .EQ. HID) THEN
C        Search For The Index
         CALL SINDEX(SRCID,NSRC,SOID,ISDX,FIND)
         IF (FIND) THEN
            ISET = IWRK2(ISDX,7)
            DO 36 K = 4, IFC
C              Change It To Numbers
               CALL STONUM(FIELD(K),40,FNUM,IMIT)
C              Check The Numerical Field
               IF (IMIT .EQ. -1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  GO TO 36
               END IF
               IF (FNUM .LE. 0.0) THEN
C                 WRITE Error Message: Particle Density Out-of-Range
                  CALL ERRHDL(PATH,MODNAM,'E','334',SRCID(ISDX))
               END IF
               DO 35 J = 1, IMIT
                  ISET = ISET + 1
                  IF (ISET .LE. NPDMAX) THEN
C                    Assign The Field
                     APDENS(ISET,ISDX) = FNUM
                  ELSE
C                    WRITE Error Message: Too Many PartDiam Categories
                     WRITE(DUMMY,'(I8)') NPDMAX
                     CALL ERRHDL(PATH,MODNAM,'E','245',DUMMY)
                  END IF
 35            CONTINUE
 36         CONTINUE
            IWRK2(ISDX,7) = ISET
         ELSE
C           WRITE Error Message     ! Source Location Has Not Been Identified
            CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
         END IF
      ELSE
C        First Check Range for Upper Value < Lower Value
         CALL SETIDG(LID,LID1,IL,LID2)
         CALL SETIDG(HID,HID1,IH,HID2)
         IF ((HID1.LT.LID1) .OR. (IH.LT.IL) .OR. (HID2.LT.LID2)) THEN
C           WRITE Error Message:  Invalid Range,  Upper < Lower
            CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
            GO TO 999
         END IF
         DO 26 I = 1, NUMSRC
C           See Whether It's In The Group
            CALL ASNGRP(SRCID(I),LID,HID,INGRP)
            IF (INGRP) THEN
               IING = I
               ISET = IWRK2(IING,7)
               DO 25 K = 4, IFC
C                 Get Numbers
                  CALL STONUM(FIELD(K),40,FNUM,IMIT)
C                 Check The Numerical Field
                  IF (IMIT .NE. 1) THEN
                     CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                     GO TO 26
                  END IF
                  IF (FNUM .LE. 0.0) THEN
C                    WRITE Error Message: Particle Density Out-of-Range
                     CALL ERRHDL(PATH,MODNAM,'E','334',SRCID(ISDX))
                  END IF
                  DO 20 J = 1, IMIT
                     ISET = ISET + 1
                     IF (ISET .LE. NPDMAX) THEN
                        APDENS(ISET,I) = FNUM
                     ELSE
C                       WRITE Error Message: Too Many PartDiam Categories
                        WRITE(DUMMY,'(I8)') NPDMAX
                        CALL ERRHDL(PATH,MODNAM,'E','245',DUMMY)
                     END IF
 20               CONTINUE
 25            CONTINUE
               IWRK2(IING,7) = ISET
            END IF
 26      CONTINUE
      END IF

 999  RETURN
      END

      SUBROUTINE INPLSC
C***********************************************************************
C                 INPLSC Module of ISC2 Model
C
C        PURPOSE: Processes Wet Scavenging Coefficients for Particles
C                 -- Liquid Precipitation --
C
C        PROGRAMMER: D. Strimaitis, SRC
C
C        ADAPTED FROM "INPVSN"
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    November 8, 1993
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Wet Scavenging Coefficient for each Particle Category
C
C        CALLED FROM:   PARTDEP
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER LID*8, HID*8, LID1*8, LID2*8, HID1*8, HID2*8, SOID*40
      LOGICAL FIND, INGRP, RMARK

C     Variable Initializations
      FIND   = .FALSE.
      INGRP  = .FALSE.
      MODNAM = 'INPLSC'

C     Get The Source ID(s)
      SOID = FIELD(3)
      CALL FSPLIT(PATH,KEYWRD,SOID,40,'-',RMARK,LID,HID)

      IF (LID .EQ. HID) THEN
C        Search For The Index
         CALL SINDEX(SRCID,NSRC,SOID,ISDX,FIND)
         IF (FIND) THEN
            ISET = IWRK2(ISDX,8)
            DO 36 K = 4, IFC
C              Change It To Numbers
               CALL STONUM(FIELD(K),40,FNUM,IMIT)
C              Check The Numerical Field
               IF (IMIT .EQ. -1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  GO TO 36
               END IF
               DO 35 J = 1, IMIT
                  ISET = ISET + 1
                  IF (ISET .LE. NPDMAX) THEN
C                    Assign The Field
                     APSLIQ(ISET,ISDX) = FNUM
                  ELSE
C                    WRITE Error Message: Too Many PartDiam Categories
                     WRITE(DUMMY,'(I8)') NPDMAX
                     CALL ERRHDL(PATH,MODNAM,'E','245',DUMMY)
                  END IF
 35            CONTINUE
 36         CONTINUE
            IWRK2(ISDX,8) = ISET
         ELSE
C           WRITE Error Message     ! Source Location Has Not Been Identified
            CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
         END IF
      ELSE
C        First Check Range for Upper Value < Lower Value
         CALL SETIDG(LID,LID1,IL,LID2)
         CALL SETIDG(HID,HID1,IH,HID2)
         IF ((HID1.LT.LID1) .OR. (IH.LT.IL) .OR. (HID2.LT.LID2)) THEN
C           WRITE Error Message:  Invalid Range,  Upper < Lower
            CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
            GO TO 999
         END IF
         DO 26 I = 1, NUMSRC
C           See Whether It's In The Group
            CALL ASNGRP(SRCID(I),LID,HID,INGRP)
            IF (INGRP) THEN
               IING = I
               ISET = IWRK2(IING,8)
               DO 25 K = 4, IFC
C                 Get Numbers
                  CALL STONUM(FIELD(K),40,FNUM,IMIT)
C                 Check The Numerical Field
                  IF (IMIT .EQ. -1) THEN
                     CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                     GO TO 26
                  END IF
                  DO 20 J = 1, IMIT
                     ISET = ISET + 1
                     IF (ISET .LE. NPDMAX) THEN
                        APSLIQ(ISET,I) = FNUM
                     ELSE
C                       WRITE Error Message: Too Many PartDiam Categories
                        WRITE(DUMMY,'(I8)') NPDMAX
                        CALL ERRHDL(PATH,MODNAM,'E','245',DUMMY)
                     END IF
 20               CONTINUE
 25            CONTINUE
               IWRK2(IING,8) = ISET
            END IF
 26      CONTINUE
      END IF

 999  RETURN
      END

      SUBROUTINE INPFSC
C***********************************************************************
C                 INPFSC Module of ISC2 Model
C
C        PURPOSE: Processes Wet Scavenging Coefficients for Particles
C                 -- Frozen Precipitation --
C
C        PROGRAMMER: D. Strimaitis, SRC
C
C        ADAPTED FROM "INPVSN"
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    November 8, 1993
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Wet Scavenging Coefficient for each Particle Category
C
C        CALLED FROM:   PARTDEP
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER LID*8, HID*8, LID1*8, LID2*8, HID1*8, HID2*8, SOID*40
      LOGICAL FIND, INGRP, RMARK

C     Variable Initializations
      FIND   = .FALSE.
      INGRP  = .FALSE.
      MODNAM = 'INPFSC'

C     Get The Source ID(s)
      SOID = FIELD(3)
      CALL FSPLIT(PATH,KEYWRD,SOID,40,'-',RMARK,LID,HID)

      IF (LID .EQ. HID) THEN
C        Search For The Index
         CALL SINDEX(SRCID,NSRC,SOID,ISDX,FIND)
         IF (FIND) THEN
            ISET = IWRK2(ISDX,9)
            DO 36 K = 4, IFC
C              Change It To Numbers
               CALL STONUM(FIELD(K),40,FNUM,IMIT)
C              Check The Numerical Field
               IF (IMIT .EQ. -1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  GO TO 36
               END IF
               DO 35 J = 1, IMIT
                  ISET = ISET + 1
                  IF (ISET .LE. NPDMAX) THEN
C                    Assign The Field
                     APSICE(ISET,ISDX) = FNUM
                  ELSE
C                    WRITE Error Message: Too Many PartDiam Categories
                     WRITE(DUMMY,'(I8)') NPDMAX
                     CALL ERRHDL(PATH,MODNAM,'E','245',DUMMY)
                  END IF
 35            CONTINUE
 36         CONTINUE
            IWRK2(ISDX,9) = ISET
         ELSE
C           WRITE Error Message     ! Source Location Has Not Been Identified
            CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
         END IF
      ELSE
C        First Check Range for Upper Value < Lower Value
         CALL SETIDG(LID,LID1,IL,LID2)
         CALL SETIDG(HID,HID1,IH,HID2)
         IF ((HID1.LT.LID1) .OR. (IH.LT.IL) .OR. (HID2.LT.LID2)) THEN
C           WRITE Error Message:  Invalid Range,  Upper < Lower
            CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
            GO TO 999
         END IF
         DO 26 I = 1, NUMSRC
C           See Whether It's In The Group
            CALL ASNGRP(SRCID(I),LID,HID,INGRP)
            IF (INGRP) THEN
               IING = I
               ISET = IWRK2(IING,9)
               DO 25 K = 4, IFC
C                 Get Numbers
                  CALL STONUM(FIELD(K),40,FNUM,IMIT)
C                 Check The Numerical Field
                  IF (IMIT .EQ. -1) THEN
                     CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                     GO TO 26
                  END IF
                  DO 20 J = 1, IMIT
                     ISET = ISET + 1
                     IF (ISET .LE. NPDMAX) THEN
                        APSICE(ISET,I) = FNUM
                     ELSE
C                       WRITE Error Message: Too Many PartDiam Categories
                        WRITE(DUMMY,'(I8)') NPDMAX
                        CALL ERRHDL(PATH,MODNAM,'E','245',DUMMY)
                     END IF
 20               CONTINUE
 25            CONTINUE
               IWRK2(IING,9) = ISET
            END IF
 26      CONTINUE
      END IF

 999  RETURN
      END

      SUBROUTINE GASDEP
C***********************************************************************
C                 GASDEP Module of ISC2 Model
C
C        PROGRAMMER: D. Strimaitis, SRC
C
C        PURPOSE: Processes Inputs for Wet & Dry GAS DEPosition
C
C        DATE:    November 8, 1993
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Input For Gaseous Removal Variables
C
C        CALLED FROM:   SOCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'GASDEP'

C     Process Keyword
      IF (KEYWRD .EQ. 'GAS-SCAV') THEN
C        Process Wet Scavending Coefficients (AGSCAV)       ---   CALL INPGSC
         CALL INPGSC
      END IF
C     Set logical LWGAS to indicate processing of Wet removal of gases
      LWGAS=.TRUE.

 999  RETURN
      END

      SUBROUTINE INPGSC
C***********************************************************************
C                 INPGSC Module of ISC2 Model
C
C        PURPOSE: Processes Scavenging Parameters for Gases
C
C        PROGRAMMER: D. Strimaitis, SRC
C
C        DATE:    November 8, 1993
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Wet Scavenging Coefficients for Gases
C
C        CALLED FROM:   GASDEP
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER LID*8, HID*8, LID1*8, LID2*8, HID1*8, HID2*8, SOID*40
      LOGICAL FIND, INGRP, RMARK

C     Variable Initializations
      FIND   = .FALSE.
      INGRP  = .FALSE.
      MODNAM = 'INPGSC'

C     Check the Number of Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 5) THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 5) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Get The Source ID(s)
      SOID = FIELD(3)
      CALL FSPLIT(PATH,KEYWRD,SOID,40,'-',RMARK,LID,HID)

      IF (LID .EQ. HID) THEN
C        Search For The Index
         CALL SINDEX(SRCID,NSRC,SOID,ISDX,FIND)
         IF (FIND) THEN
C           Read Secondary Parameter for Either LIQ (liquid) or
C           ICE (frozen) Precipitation, and assign index accordingly
            IF (FIELD(4) .EQ. 'LIQ') THEN
               ipindex=1
            ELSE IF (FIELD(4) .EQ. 'ICE') THEN
               ipindex=2
            ELSE
C              Error Message: Invalid Precipitation Type
               CALL ERRHDL(PATH,MODNAM,'E','203','PRECIP')
               GO TO 999
            END IF
C           Read Scavenging Coef.
C           Change it to Numbers
            CALL STONUM(FIELD(5),40,FNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 999
            END IF
C           Assign The Field
            AGSCAV(IPINDEX,ISDX) = FNUM
         ELSE
C           WRITE Error Message     ! Source Location Has Not Been Identified
            CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
         END IF
      ELSE
C        First Check Range for Upper Value < Lower Value
         CALL SETIDG(LID,LID1,IL,LID2)
         CALL SETIDG(HID,HID1,IH,HID2)
         IF ((HID1.LT.LID1) .OR. (IH.LT.IL) .OR. (HID2.LT.LID2)) THEN
C           WRITE Error Message:  Invalid Range,  Upper < Lower
            CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
            GO TO 999
         END IF
         DO 26 I = 1, NUMSRC
C           See Whether It's In The Group
            CALL ASNGRP(SRCID(I),LID,HID,INGRP)
            IF (INGRP) THEN
               IING = I
C              Read Secondary Parameter for Either LIQ (liquid) or
C              ICE (frozen) Precipitation, and assign index accordingly
               IF (FIELD(4) .EQ. 'LIQ') THEN
                  ipindex=1
               ELSE IF (FIELD(4) .EQ. 'ICE') THEN
                  ipindex=2
               ELSE
C                 Error Message: Invalid Precipitation Type
                  CALL ERRHDL(PATH,MODNAM,'E','203','PRECIP')
                  GO TO 999
               END IF
C              Read Scavenging Coef.
C              Change it to Numbers
               CALL STONUM(FIELD(5),40,FNUM,IMIT)
C              Check The Numerical Field
               IF (IMIT .EQ. -1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  GO TO 999
               END IF
C              Assign The Field
               AGSCAV(IPINDEX,IING) = FNUM
            END IF
 26      CONTINUE
      END IF

 999  RETURN
      END

      SUBROUTINE SOGRP
C***********************************************************************
C                 SOGRP Module of ISC2 Model
C
C        PURPOSE: Processes Source Group Inputs for Pass One
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Source Group Input For Pass One
C
C        CALLED FROM: SOCARD
C***********************************************************************
C

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER*8 LOWID, HIGID, LID1, LID2, HID1, HID2, TEMPID
      LOGICAL CONT, INGRP, RMARK

C     Variable Initializations
      CONT   = .FALSE.
      MODNAM = 'SOGRP'

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LE. 3 .AND. FIELD(3) .NE. 'ALL') THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      END IF

C     READ in the Group ID and Check for Continuation Card
      TEMPID = FIELD(3)
      DO 20 I = 1, NUMGRP
         IF (TEMPID .EQ. GRPID(I)) THEN
            CONT = .TRUE.
         END IF
 20   CONTINUE

C     Increment Counters and Assign Group ID If Not a Continuation Card
      IF (.NOT. CONT) THEN
         IGRP = IGRP + 1
         IF (IGRP .GT. NGRP) THEN
C           WRITE Error Message    ! Too Many Source Groups Specified
            WRITE(DUMMY,'(I8)') NGRP
            CALL ERRHDL(PATH,MODNAM,'E','235',DUMMY)
C           Exit to END
            GO TO 999
         END IF
         NUMGRP = NUMGRP + 1
         GRPID(IGRP) = TEMPID
      END IF

C     Set Up The Source Group Array
      IF (GRPID(IGRP) .EQ. 'ALL' .AND. .NOT.CONT) THEN
         DO 30 I = 1, NUMSRC
            IGROUP(I,IGRP) = 1
 30      CONTINUE
      ELSE
C        Loop Through Fields
         DO 50 I = 4, IFC
            CALL FSPLIT(PATH,KEYWRD,FIELD(I),40,'-',RMARK,LOWID,HIGID)
C           First Check Range for Upper Value < Lower Value
            CALL SETIDG(LOWID,LID1,IL,LID2)
            CALL SETIDG(HIGID,HID1,IH,HID2)
            IF ((HID1.LT.LID1) .OR. (IH.LT.IL) .OR. (HID2.LT.LID2)) THEN
C              WRITE Error Message:  Invalid Range,  Upper < Lower
               CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
               GO TO 50
            END IF
            DO 40 K = 1, NUMSRC
               CALL ASNGRP(SRCID(K),LOWID,HIGID,INGRP)
               IF (INGRP) THEN
                  IGROUP(K,IGRP) = 1
               END IF
 40         CONTINUE
 50      CONTINUE
      END IF

 999  RETURN
      END

      SUBROUTINE ASNGRP(INID,LOWID,HIGID,INGRP)
C***********************************************************************
C                 ASNGRP Module of ISC2 Model
C
C        PURPOSE: Find Whether A Source ID is In The Specific Group
C
C        PROGRAMMER: Roger Brode, Jeff Wang, Kevin Stroupe
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Field Parameters
C
C        OUTPUTS: Indicator for Source ID in The Group
C
C        CALLED FROM: (This is An Utility Program)
C***********************************************************************
C
C     Variable Declarations
      CHARACTER LOWID*8, HIGID*8, INID*8, IID1*8, LID1*8, HID1*8,
     &          PATH*2, MODNAM*6, IID2*8, LID2*8, HID2*8
      INTEGER IN, IL, IH
      LOGICAL INGRP

C     Variable Initializations
      MODNAM = 'ASNGRP'
      PATH = 'SO'
      INGRP = .FALSE.

C     Extract The Character Field And Numerical Field
      CALL SETIDG(INID,IID1,IN,IID2)
      CALL SETIDG(LOWID,LID1,IL,LID2)
      CALL SETIDG(HIGID,HID1,IH,HID2)

C     Do Comparisons of Character and Numeric Fields, All Must Satisfy Ranges
      IF ((IID1.GE.LID1 .AND. IID1.LE.HID1) .AND.
     &        (IN.GE.IL .AND. IN.LE.IH) .AND.
     &    (IID2.GE.LID2 .AND. IID2.LE.HID2)) THEN
         INGRP = .TRUE.
      END IF

      RETURN
      END

      SUBROUTINE SETIDG(INID,IDCHR1,IDNUM,IDCHR2)
C***********************************************************************
C                 SETIDG Module of ISC2 Model
C
C        PURPOSE: Find A Source ID's Character Part and
C                 Numerical Part
C
C        PROGRAMMER: Jeff Wang, Roger Brode, Kevin Stroupe
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Field Parameters
C
C        OUTPUTS: An Initial Character String, a Number, and
C                 a Second Character String
C
C        CALLED FROM: (This is An Utility Program)
C***********************************************************************
C
C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER INID*8, IDCHR1*8, IDCHR2*8, CHKI, NUMID*40, ZERO, BLNK
      LOGICAL HIT

C     Variable Initializations
      MODNAM = 'SETIDG'
      I = 8
      NUMID  = ' '
      IDCHR1 = ' '
      IDCHR2 = ' '
      IDNUM  = 0
      HIT    = .FALSE.
      ZERO   = '0'
      BLNK   = ' '

C     Find The Length of the Input Field, II (<= 8)
      DO WHILE (.NOT.HIT .AND. I.GE.1)
         CHKI = INID(I:I)
         IF (CHKI .NE. ' ') THEN
            II = I
            HIT = .TRUE.
         END IF
         I = I - 1
      END DO

C     Divide the Input Id into 3 parts (char1, int, and char2)
      I = 1
      ISTR = I
      CHKI = INID(I:I)
C     Get first character part
      DO WHILE (CHKI .LT. '0' .OR. CHKI .GT. '9')
         IDCHR1 = INID(ISTR:I)
         I = I + 1
         IF (I .GT. II) THEN
            GO TO 20
         ELSE
            CHKI = INID(I:I)
         END IF
      END DO

C     Get integer part
      ISTR = I
      DO WHILE (CHKI .GE. '0' .AND. CHKI .LE. '9')
         NUMID = INID(ISTR:I)
         I = I + 1
         IF (I .GT. II) THEN
            GO TO 20
         ELSE
            CHKI = INID(I:I)
         END IF
      END DO

C     Get second character part
      ISTR = I
      DO WHILE (I .LE. II)
         IDCHR2 = INID(ISTR:I)
         I = I + 1
         IF (I .GT. II) THEN
            GO TO 20
         ELSE
            CHKI = INID(I:I)
         END IF
      END DO

 20   CONTINUE

C     Convert Numeric Part to Integer Variable
      CALL STONUM(NUMID,40,FNUM,IMIT)
      IDNUM = INT(FNUM)

 991  FORMAT(A40)
 992  FORMAT(A8)

      RETURN
      END

c----------------------------------------------------------------------
      subroutine vdp1
c----------------------------------------------------------------------
c
c --- ISC2ST     Version:  1.0     Level:  930215                  VDP1
c                J. Scire, SRC
c
c --- PURPOSE:  Setup routine for PARTICLE dry deposition.
c               Completes particle common block /SOURC4/.  Performs
c               initialization and time-invariant calculations.
c
c --- INPUTS:
c     Common block /SOURC4/ variables:
c              INPD - integer    - Number of particle size categories
c            APDIAM - real array - Mean diameter (microns) of each
c                                  particle size category
c              APHI - real array - Mass fraction in each size category
c            APDENS - real       - Particle density (g/cm**3)
c
c --- OUTPUT:
c     Common block /SOURC4/ variables:
c               ASC - real array - Schmidt number
c            AVGRAV - real array - Gravitational settling velocity (m/s)
c            ATSTOP - real array - Stopping time (s)
c            VAIRMS - real       - Viscosity of air (m**2/s)
c             ZRDEP - real       - Reference height (m) for Deposition
c            VDPHOR - real       - Phoretic effects term (m/s)
c
c --- VDP1 called by:  SOCARD
c --- VDP1 calls:      none
c----------------------------------------------------------------------
c
      INCLUDE 'MAIN1.INC'

      data a1/1.257/,a2/0.4/,a3/0.55/,xmfp/6.5e-6/
      data vcon/1.81e-4/,xk/1.38e-16/
      data vair/0.15/,gcgs/981./,rhoair/1.2e-3/,tair/293.15/
c
      io6=iounit
c ***
      if(DEBUG)then
         write(io6,*)
         write(io6,*)'SUBR. VDP1 -- INPUTS'
         write(io6,*)
         do 5 i=1,numsrc
         write(io6,*)'SOURCE          = ',i
         write(io6,*)'INPD            = ',inpd(i)
         write(io6,*)'APDIAM (um)     = ',(apdiam(n,i),n=1,inpd(i))
         write(io6,*)'APDIAM (um)     = ',(apdiam(n,i),n=1,inpd(i))
         write(io6,*)'APHI            = ',(aphi(n,i),n=1,inpd(i))
         write(io6,*)'APDENS(g/cm**3) = ',(apdens(n,i),n=1,inpd(i))
         write(io6,*)
5        continue
      endif
c ***
c
c --- Convert viscosity of air (at 20 deg C) from cm**2/s to m**2/s
      vairms=1.e-4*vair

CRWB  Reference Height is now a function of Z0M, which may change hourly.
CRWB  It is now calculated in SUB. VDP of CALC1.FOR.
CRWBc
CRWBc --- Set reference height for aerodynamic resistance calculation
CRWB      zrdep=1.0
c
c --- Define phoretic effects term (m/s)
      vdphor=0.0001
c
c --  LOOP over sources
      do 25 j=1,numsrc
c
c --- LOOP over "INPD" size intervals if non-zero
         if(inpd(j) .LE. 0) goto 25
         do 20 i=1,inpd(j)
c
c ---       Slip correction factor
            diamcm=1.e-4*apdiam(i,j)
            scf=1.+2.0*xmfp*(a1+a2*exp(-a3*diamcm/xmfp))/diamcm
c
c ---       Stokes friction coefficient
            sfc=3.*pi*vcon*diamcm/scf
c
c ---       Diffusivity (cm**2/s)
            diff=xk*tair/sfc
c ***
            if(DEBUG)then
               write(io6,*)'i = ',i,' diamcm = ',diamcm,' scf = ',scf,
     1         ' sfc = ',sfc,' diff = ',diff
            endif
c ***
c
c ---       Schmidt number
c ---       (vair = viscosity of air at 20 deg. c = 0.15 cm**2/s)
            asc(i,j)=vair/diff
c
c ---       Gravitational settling velocity (m/s)
c ---       (rhoair is approx. density of air -- 1.2e-3 g/cm**3)
            avgrav(i,j)=0.01*(apdens(i,j)-rhoair)*gcgs*diamcm**2
     1                     *scf/(18.*vcon)
c
c ---       Stopping times
            atstop(i,j)=avgrav(i,j)/(0.01*gcgs)
20       continue
25    continue
c ***
      if(DEBUG)then
         write(io6,*)
         write(io6,*)'SUBR. VDP1 -- Outputs'
         write(io6,*)
         do 30 i=1,numsrc
         write(io6,*)'SOURCE          = ',i
         write(io6,*)'ASC             = ',(asc(n,i),n=1,inpd(i))
         write(io6,*)'AVGRAV (m/s)    = ',(avgrav(n,i),n=1,inpd(i))
         write(io6,*)'ATSTOP (s)      = ',(atstop(n,i),n=1,inpd(i))
         write(io6,*)'VAIRMS (m**2/s) = ',vairms
C         write(io6,*)'ZRDEP (m)       = ',zrdep
         write(io6,*)'VDPHOR (m/s)    = ',vdphor
         write(io6,*)
30       continue
      endif
c ***
c
      return
      end

      SUBROUTINE HREMIS
C***********************************************************************
C                 HREMIS Module of AERMOD
C
C        PURPOSE: To process Hourly Emissions Data 
C
C        PROGRAMMER: Jayant Hardikar, Roger Brode
C  
C        DATE:    September 15, 1993
C
C        INPUTS:  Pathway (SO) and Keyword (HOURLY)
C
C        OUTPUTS: Source QFLAG Array
C
C        CALLED FROM:   SOCARD
C***********************************************************************
   
C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

      LOGICAL FOPEN, INGRP
      LOGICAL RMARK

      CHARACTER*8 LOWID, HIGID, LID1, LID2, HID1, HID2, TEMPID

C     Variable Initializations
      MODNAM = 'HREMIS'

      FOPEN  = .FALSE.

      IF (IFC .GE. 4) THEN
C        Retrieve Hourly Emissions Data Filename as Character Substring to
C        Maintain Case
         HRFILE = RUNST1(LOCB(3):LOCE(3))

C        Open Hourly Emissions Data File If Not Already Open
         INQUIRE (FILE=HRFILE,OPENED=FOPEN)
         
         IF (.NOT. FOPEN) THEN
C           Open Hourly Emissions Data File If Not Already Open
            INQUIRE (UNIT=IHREMI,OPENED=FOPEN)
            IF (.NOT. FOPEN) THEN
               OPEN (UNIT=IHREMI,ERR=998,FILE=HRFILE,IOSTAT=IOERRN,
     &               STATUS='OLD')
            ELSE
C              Hourly Emissions File is Already Opened With Different Filename
               CALL ERRHDL(PATH,MODNAM,'E','500',KEYWRD)
               GO TO 999
            ENDIF
         ENDIF

      ELSE
C        WRITE Error Message         ! Not Enough Parameters Specified
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      END IF

      TEMPID = FIELD(4)

C     Set Up The Source Group Array
      IF (TEMPID .EQ. 'ALL') THEN
         DO 30 I = 1, NUMSRC
            QFLAG(I) = 'HOURLY'
 30      CONTINUE
      ELSE
C        Loop Through Fields
         DO 50 I = 4, IFC
            CALL FSPLIT(PATH,KEYWRD,FIELD(I),40,'-',RMARK,LOWID,HIGID)
C           First Check Range for Upper Value < Lower Value
            CALL SETIDG(LOWID,LID1,IL,LID2)
            CALL SETIDG(HIGID,HID1,IH,HID2)
            IF ((HID1.LT.LID1) .OR. (IH.LT.IL) .OR. (HID2.LT.LID2)) THEN
C              WRITE Error Message:  Invalid Range,  Upper < Lower
               CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
               GO TO 50
            END IF
            DO 40 K = 1, NUMSRC
               CALL ASNGRP(SRCID(K),LOWID,HIGID,INGRP)
               IF (INGRP) THEN
                  QFLAG(K) = 'HOURLY'                  
               END IF
 40         CONTINUE
 50      CONTINUE
      END IF

      GO TO 999

C     Process Error Messages
998   CALL ERRHDL(PATH,MODNAM,'E','500',KEYWRD)

999   RETURN
      END 
      SUBROUTINE CALC
C***********************************************************************
C                 CALC Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Controls Flow and Processing of CALCulation Modules
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To add call for new source type of OPENPIT.
C                    R. W. Brode, PES - 9/30/94
C
C        INPUTS:  Arrays of Source Parameters
C                 Arrays of Receptor Locations
C                 Meteorological Variables for One Hour
C
C        OUTPUTS: Array of 1-hr CONC or DEPOS Values for Each Source/Receptor
C
C        CALLED FROM:   HRLOOP
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'CALC'
      PATH = 'CN'

C     Begin Source LOOP
      DO 20 ISRC = 1, NUMSRC
         IF (SRCTYP(ISRC) .EQ. 'POINT') THEN
C           Calculate Point Source Values                ---   CALL PCALC
            CALL PCALC
         ELSE IF (SRCTYP(ISRC) .EQ. 'VOLUME') THEN
C           Calculate Volume Source Values               ---   CALL VCALC
            CALL VCALC
         ELSE IF (SRCTYP(ISRC) .EQ. 'AREA') THEN
C           Calculate Area Source Values                 ---   CALL ACALC
            CALL ACALC
         ELSE IF (SRCTYP(ISRC) .EQ. 'OPENPIT') THEN
C           Calculate OpenPit Source Values              ---   CALL OCALC
            CALL OCALC            
         END IF
 20   CONTINUE
C     End Source LOOP

      RETURN
      END

      SUBROUTINE ITSET
C***********************************************************************
C                 ITSET Module of the ISC Short Term Model - Version 2
C
C        PURPOSE:    To set intermediate terrain variables, based on
C                    complex terrain plume height and terrain height.
C
C        PROGRAMMER: Roger W. Brode, PES, Inc.
C
C        DATE:       September 30, 1994
C
C        INPUTS:     HECOMP = Complex Terrain Plume Height, without
C                             Terrain Adjustment Factors (through COMMON)
C
C        OUTPUTS:    SIMPLE, COMPLX, INTERM = Intermediate terrain
C                    logical control variables (through COMMON)
C
C        CALLED FROM:   PCALC
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'ITSET'
      SIMPLE = .FALSE.
      INTERM = .FALSE.
      COMPLX = .FALSE.

      IF (HS .GE. HTER) THEN
         SIMPLE = .TRUE.
      ELSE IF (HECOMP .GT. HTER) THEN
         INTERM = .TRUE.
      ELSE
         COMPLX = .TRUE.
      END IF

C     Write Special DEBUG values for IT results
      if(DEBUG) then
         write(iounit,*)
         write(iounit,*) 'ITSET --- IT RESULTS, HOUR :',IHOUR
         IF (SIMPLE) THEN
            write(iounit,*) 'ITFLAG = SIMPLE'
         ELSE IF (INTERM) THEN
            write(iounit,*) 'ITFLAG = INTERM'
         ELSE IF (COMPLX) THEN
            write(iounit,*) 'ITFLAG = COMPLX'
         END IF
         write(iounit,*) 'HECOMP         = ',HECOMP
         write(iounit,*) 'HS, ZS, ZELEV  = ',HS,ZS,ZELEV
      endif

      RETURN
      END

      SUBROUTINE XYDIST
C***********************************************************************
C                 XYDIST Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Sets Receptor Variables and Calculates Downwind (X)
C                 and Crosswind (Y) Distances,
C                 and Radial Distance from Source to Receptor (DISTR)
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Source Location
C                 Arrays of Receptor Locations
C                 SIN and COS of Wind Direction FROM Which Wind
C                 is Blowing, WDSIN and WDCOS
C
C        OUTPUTS: Values of X, Y, and DISTR (m)
C
C        CALLED FROM:   PCALC
C                       VCALC
C                       ACALC
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'XYDIST'

C     Set Receptor Coordinates, Terrain Elevation and Flagpole Heights
      XR = AXR(IREC)
      YR = AYR(IREC)
      ZELEV = AZELEV(IREC)
      ZFLAG = AZFLAG(IREC)

C     Calculate Downwind (X) and Crosswind (Y) Distances
      X = -((XR-XS)*WDSIN + (YR-YS)*WDCOS)
      Y =   (XR-XS)*WDCOS - (YR-YS)*WDSIN

C     Calculate Source-Receptor (Radial) Distance, DISTR
      DISTR = SQRT (X*X + Y*Y)

      RETURN
      END

      SUBROUTINE VCALC
C***********************************************************************
C                 VCALC Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Calculates concentration or deposition values
C                 for VOLUME sources
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        MODIFIED BY D. Strimaitis, SRC (for Wet & Dry DEPOSITION)
C
C        DATE:    December 15, 1993
C
C        MODIFIED BY D. Strimaitis, SRC (for Dry DEPOSITION)
C        (DATE:    February 15, 1993)
C
C        MODIFIED BY R. Brode, PES, to initialize SBID - 7/15/94
C
C        MODIFIED BY R. Brode, PES, to set WAKE flag to .FALSE. to
C                 avoid problem in call to PSIMPL - 8/16/01
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

C     Variable Initializations
      MODNAM = 'VCALC'

C     Set WAKE Flag to False to Avoid Problem in call to PSIMPL
      WAKE = .FALSE.

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
C        Adjust Wind Speed to Release Height                ---   CALL WSADJ
         CALL WSADJ
C        Calculate Effective Radius
         XRAD = 2.15*SYINIT
         IF (LDPART .OR. LWPART) THEN
C           Calculate Min Sigma-z for Settled Plume @ Surface --- CALL SETSZMN
            CALL SETSZMN
         END IF
C        Initialize SBID to 0.0 for call to DEPCOR
         SBID = 0.0

C        Begin Receptor LOOP
         DO 20 IREC = 1, NUMREC
C           Calculate Down and Crosswind Distances          ---   CALL XYDIST
            CALL XYDIST
            IF (ABS(Y) .GT. 1.191754*X) THEN
C              Receptor is at least 50 deg. off the plume centerline
               DO ITYP = 1, NUMTYP
                  HRVAL(ITYP) = 0.0
               END DO
            ELSE IF (DISTR .LT. (XRAD+0.99)) THEN
C              Receptor Too Close to Source for Calculation
               DO ITYP = 1, NUMTYP
                  HRVAL(ITYP) = 0.0
               END DO
            ELSE IF ((X-XRAD) .LT. 0.0) THEN
C              Receptor Upwind of Downwind Edge
               DO ITYP = 1, NUMTYP
                  HRVAL(ITYP) = 0.0
               END DO
            ELSE
C              Determine Effective Plume Height             ---   CALL VHEFF
               CALL VHEFF(ZELEV,HEFLAT,HE)
C              Determine Dispersion Parameters              ---   CALL VDIS
               CALL VDIS(X,SY,SZ,XY,XZ)
               IF (LWGAS) THEN
C                 Initialize wet source depletion factor to unity.
                  WQCORG = 1.
                  IF (WDPLETE) THEN
C                    Determine source depletion factor
C                    from wet removal (GASES)
                     WQCORG=EXP(-GSCVRT*X/US)
                  ENDIF
               ENDIF
C              Calculate Conc. or Depos. for Virtual Point Source
C              Using a Simple Terrain Model                 ---   CALL PSIMPL
               CALL PSIMPL

C              Sum HRVAL to AVEVAL and ANNVAL Arrays     ---   CALL SUMVAL
               CALL SUMVAL

            END IF
 20      CONTINUE
C        End Receptor LOOP
      END IF

      RETURN
      END


      SUBROUTINE ACALC
C***********************************************************************
C                 ACALC Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Calculates concentration or deposition values
C                 for AREA sources utilizing an integrated line source.
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
C        MODIFIED:   To incorporate numerical integration algorithm
C                    for AREA source - 7/7/93
C
C        MODIFIED BY D. Strimaitis, SRC (for DEPOSITION) - 2/15/93
C
C        MODIFIED BY R. Brode, PES, to initialize XZ, XY, and SBID - 7/15/94
C
C*       MODIFIED BY J. Hardikar, PES, to make consistent with the new
C*                   OPENPIT Source Methodology - 7/20/94
C
C
C        INPUTS:  Source Parameters for Specific Source
C                 Arrays of Receptor Locations
C                 Meteorological Variables for One Hour
C
C        OUTPUTS: Array of 1-hr CONC or DEPOS Values for Each Source/Receptor
C
C        CALLED FROM:   CALC
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      
      REAL XSPA(NVMAX),YSPA(NVMAX)
      
C     Variable Initializations
      MODNAM = 'ACALC'

C     Set the Source Variables for This Source              ---   CALL SETSRC
      CALL SETSRC

C     Apply Variable Emission Rate and Unit Factors         ---   CALL EMFACT
      CALL EMFACT(QS)

C     Set Particel Deposition Variables for this Source
      IF (LDPART .OR. LWPART) THEN
C        Calculate Deposition Velocities for this Source    ---   CALL VDP
         CALL VDP
      END IF

C     Calculate Scavenging Ratios for this Source           ---   CALL SCAVRAT
      IF (LWPART .OR. LWGAS) CALL SCAVRAT

      IF ((QTK.NE.0.0) .AND. (STABLE .OR. (HS.LE.ZI))) THEN
C        Determine Coordinates of Vertices for AREA Source
C        in Wind Direction Coordinate System                ---   CALL AVERTS
         CALL AVERTS(XVERT,YVERT,XSPA,YSPA,NVERT+1)

C*       Store Coordinates of the Area in COMMON Variables
         DO 40 IVERT = 1,NVERT+1
            SPA(IVERT,1) = XSPA(IVERT)
            SPA(IVERT,2) = YSPA(IVERT)
40       CONTINUE
         
C        Adjust Wind Speed to Release Height                ---   CALL WSADJ
         CALL WSADJ
         IF (LDPART .OR. LWPART) THEN
C           Calculate Min Sigma-z for Settled Plume @ Surface --- CALL SETSZMN
            CALL SETSZMN
         END IF
C*       Initialize XY and XZ to 0.0 (XZ is used in
C*       call to DEPCOR from PLUMEF)
         XY = 0.0
         XZ = 0.0
         
C        Initialize SBID to 0.0 (for call to DEPCOR from PLUMEF)
         SBID = 0.0

C        Begin Receptor LOOP
         DO 660 IREC = 1, NUMREC
C           Calculate Down and Crosswind Distances          ---   CALL XYDIST
            CALL XYDIST
            HE = HS
            HEFLAT = HE
            IF (STABLE .OR. (HEFLAT.LE.ZI) .OR. DEPOS .OR. WDEP) THEN
               DO ITYP = 1, NUMTYP
C                 Calculate Area Source Integral            ---   CALL AREAIN
                  CALL AREAIN
               END DO
            ELSE
C              Plume Is Above Mixing Height, ZI, and No Wet Deposition
               DO ITYP = 1, NUMTYP
                  HRVAL(ITYP) = 0.0
               END DO
            END IF

C           Sum HRVAL to AVEVAL and ANNVAL Arrays        ---   CALL SUMVAL
            CALL SUMVAL

 660     CONTINUE
C        End Receptor LOOP
      END IF

      RETURN
      END


      SUBROUTINE OCALC
C***********************************************************************
C                 OCALC Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Calculates concentration or deposition values
C                 for OPENPIT sources
C
C        PROGRAMMER: Jayant Hardikar, Roger Brode
C        ADAPTED FROM:  SUBROUTINE ACALC
C
C        DATE:    July 19, 1994
C
C        MODIFIED:   To skip calculations if QPTOT = 0.0, avoiding
C                    zero divide error in SUB. AMFRAC.
C                    R. W. Brode, PES Inc., - 4/14/95
C
C        INPUTS:  Source Parameters for Specific Source
C                 Arrays of Receptor Locations
C                 Meteorological Variables for One Hour
C
C        OUTPUTS: Array of 1-hr CONC or DEPOS Values for Each Source/Receptor
C
C        CALLED FROM:   CALC
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      REAL XVM(5), YVM(5)

C     Variable Initializations
      MODNAM = 'OCALC'

C     Set the Source Variables for This Source              ---   CALL SETSRC
      CALL SETSRC

C*    Initialize the Total Adjusted Emission Rate from
C*    All Particles
      QPTOT = 0.0          

C*    Loop over Particle Size Categories
      DO 20 ICAT = 1,NPD
C*       Calculate the Escape Fraction for Each Category    ---   CALL ESCAPE      
         CALL ESCAPE(ICAT)

C*       Adjust the Emission Rate for Each Category         ---   CALL ADJEMI
         CALL ADJEMI(ICAT,QPTOT)

C*    End Loop Over Particle Size Categories
20    CONTINUE

C*    Skip Calculations if QPTOT = 0.0
      IF (QPTOT .EQ. 0.0)  GO TO 999

C*    Adjust the Mass Fractions for All the Particle 
C*    Size Categories                                       ---   CALL AMFRAC
      CALL AMFRAC(QPTOT)
      
C*    Determine the AlongWind Length of the OPENPIT Source  ---   CALL LWIND
      CALL LWIND

C*    Calculate the Relative Depth of the OPENPIT Source    ---   CALL PDEPTH            
      CALL PDEPTH
      
C*    Calculate the Fractional Size of the 
C*    Effective Pit Area                                    ---   CALL PTFRAC
      CALL PTFRAC


C*    WRITE DEBUG INFORMATION
      IF (DEBUG) THEN
         WRITE (IOUNIT,*)
         WRITE (IOUNIT,*)
         WRITE (IOUNIT,*) 'DETAIL INFORMATION ON THE OPENPIT SOURCE:'
         WRITE (IOUNIT,*)
         WRITE (IOUNIT,*)   
      ENDIF
     
C*    Determine the Coordinates of the Effective Pit Area
C*    in Wind Direction Coordinate System                   ---   CALL PITEFF
      CALL PITEFF

C*    Calculate the Emission Rate for the Effective
C*    Pit Area                                              ---   CALL PITEMI
      CALL PITEMI(QPTOT)

C*    WRITE DEBUG INFORMATION
      IF (DEBUG) THEN
         WRITE (IOUNIT,*) 'OPENPIT PARTICLE CHARACTERISTICS:'
         WRITE (IOUNIT,*) '-------------------------------'
         WRITE (IOUNIT,*) 
         WRITE (IOUNIT,8000) (EFRAC(II),II = 1, NPD)
8000     FORMAT (1X,'ESCAPE FRACTIONS= ',10(F8.3,2X))
         WRITE (IOUNIT,8200) (QPART(II),II = 1, NPD)
8200     FORMAT (1X,'ADJUSTED EMISSION RATES= ',10(F8.3,2X))
         WRITE (IOUNIT,8400) (PHI(II),II = 1, NPD)
8400     FORMAT (1X,'ADJUSTED MASS FRACTIONS= ',10(F8.3,2X))
         WRITE (IOUNIT,*) 'EMISSION RATE OF EFFECTIVE PIT= ',QEFF
         WRITE (IOUNIT,*) 
      ENDIF                  
      
                  
C     Set Particel Deposition Variables for this Source
      IF (LDPART .OR. LWPART) THEN
C        Calculate Deposition Velocities for this Source    ---   CALL VDP
         CALL VDP
      END IF

C     Calculate Scavenging Ratios for this Source           ---   CALL SCAVRAT
      IF (LWPART .OR. LWGAS) CALL SCAVRAT

C     Apply Variable Emission Rate and Unit Factors         ---   CALL EMFACT
      CALL EMFACT(QEFF)

      IF ((QTK.NE.0.0) .AND. (STABLE .OR. (HS.LE.ZI))) THEN
C        Adjust Wind Speed to Release Height                ---   CALL WSADJ
         CALL WSADJ
         IF (LDPART .OR. LWPART) THEN
C           Calculate Min Sigma-z for Settled Plume @ Surface --- CALL SETSZMN
            CALL SETSZMN
         END IF
C        Initialize XY and XZ to 0.0 (XZ is used in call to DEPCOR from PLUMEF)
         XY = 0.0
         XZ = 0.0
C        Initialize SBID to 0.0 (for call to DEPCOR from PLUMEF)
         SBID = 0.0
C        Begin Receptor LOOP
         DO 660 IREC = 1, NUMREC
C           Check for receptor located inside boundary of open pit source
            XVM(1) = XVERT(1) * 1000.
            XVM(2) = XVERT(2) * 1000.
            XVM(3) = XVERT(3) * 1000.
            XVM(4) = XVERT(4) * 1000.
            XVM(5) = XVERT(5) * 1000.
            YVM(1) = YVERT(1) * 1000.
            YVM(2) = YVERT(2) * 1000.
            YVM(3) = YVERT(3) * 1000.
            YVM(4) = YVERT(4) * 1000.
            YVM(5) = YVERT(5) * 1000.
            XR = AXR(IREC)
            YR = AYR(IREC)
            CALL PNPOLY(XR,YR,XVM,YVM,5,INOUT)
            IF (INOUT .GT. 0) THEN
C              Receptor is within boundary - skip to next receptor
               GO TO 660
            END IF

C           Calculate Down and Crosswind Distances          ---   CALL XYDIST
            CALL XYDIST
            HE = HS
            HEFLAT = HE
            IF (STABLE .OR. (HEFLAT.LE.ZI) .OR. DEPOS .OR. WDEP) THEN
               DO ITYP = 1, NUMTYP
C                 Calculate Area Source Integral            ---   CALL AREAIN
                  CALL AREAIN
               END DO
            ELSE
C              Plume Is Above Mixing Height, ZI, and No Wet Deposition
               DO ITYP = 1, NUMTYP
                  HRVAL(ITYP) = 0.0
               END DO
            END IF

C           Sum HRVAL to AVEVAL and ANNVAL Arrays        ---   CALL SUMVAL
            CALL SUMVAL

 660     CONTINUE
C        End Receptor LOOP
      END IF

 999  RETURN
      END


       SUBROUTINE PHEFFC(XARG,DHPOUT,HEOUT)
C***********************************************************************
C                 PHEFFC Module of the ISC Short Term Model - Version 2
C
C        PURPOSE: Calculates Effective Plume Height for POINT Sources (m)
C                 in Complex Terrain
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    September 30, 1994
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

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'PHEFFC'

      IF (FSTCMP) THEN
         FSTCMP = .FALSE.
C        This is the First Call for PHEFFC - Calculate HSP and DHFCMP
         HSP = HSPRIM(US,VS,HS,DS)
         CALL DELH(DHFCMP)
      END IF

      IF (XARG .LT. XF) THEN
C        Distance is less than distance to final rise - Calculate gradual rise
         CALL DHPHS(XARG,DHFCMP,DHPOUT)
      ELSE
C        Set gradual rise = final rise for XARG > XF
         DHPOUT = DHFCMP
      END IF

C     Check for stack-tip downwash option
      IF (NOSTD) THEN
         HEOUT = HS + DHPOUT
      ELSE
         HEOUT = HSP + DHPOUT
      END IF

      RETURN
      END

      SUBROUTINE STERAD(HEARG,ZARG,HEOUT)
C***********************************************************************
C                 STERAD Module of the ISC Short Term Model - Version 2
C
C        PURPOSE: Adjusts Effective Plume Height for Simple Terrain Effects
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    September 30, 1994
C
C        INPUTS:  HEARG = Flat terrain plume height
C                 ZARG  = Elevation of terrain
C
C        OUTPUTS: HEOUT = Effective plume height with terrain adjustment
C
C        CALLED FROM:   PCALC
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'STERAD'

C     Adjust Plume Height for Elevated Terrain, Save Flat Terrain Value (HEFLAT)
C     For Later Comparison With Mixing Height
      IF (FLAT) THEN
         HEOUT  = HEARG
      ELSE IF (ELEV) THEN
C        Calculate Terrain Hgt Above Plant Grade (Chopped-off at Release Height)
         HTERCHOP = AMIN1( HS, (ZARG - ZS))
         HEOUT = HEARG - HTERCHOP
      END IF

C     Don't Allow Effective Plume Height to be < 0.0
      HEOUT = AMAX1( 0.0, HEOUT)

      RETURN
      END

      SUBROUTINE CTERAD(HEARG,ZARG,HEOUT,COUT)
C***********************************************************************
C                 CTERAD Module of the ISC Short Term Model - Version 2
C
C        PURPOSE: Adjusts Effective Plume Height for Complex Terrain Effects
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    September 30, 1994
C
C        INPUTS:  HEARG = Flat terrain plume height
C                 ZARG  = Elevation of terrain
C
C        OUTPUTS: HEOUT = Effective plume height with terrain adjustment
C                 COUT  = Attenuation correction factor
C
C        CALLED FROM:   PCALC
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'CTERAD'

C     Calculate Terrain Hgt Above Plant Grade
      HTER = ZARG - ZS

C     Calculate COMPLEX1 Plume Height
      HEOUT = AMAX1( (HEARG*TCF(KST)),
     &               (HEARG-(1.0-TCF(KST))*HTER) )
      HEOUT = AMAX1( HEOUT, ZMIN )

C     Calculate the Attentuation Correction Factor, COUT
      IF ( (UNSTAB.OR.NEUTRL) .OR. (HEARG.GE.(HTER+ZFLAG)) ) THEN
         COUT = 1.0
      ELSE IF ((HTER+ZFLAG-HEARG) .GE. 400.) THEN
         COUT = 0.0
      ELSE
         COUT = (400. - (HTER+ZFLAG-HEARG))/400.
      END IF

      RETURN
      END

      SUBROUTINE VHEFF(ZARG,HEFOUT,HEOUT)
C***********************************************************************
C                 VHEFF Module of the ISC Short Term Model - Version 2
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
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'VHEFF'

C     Calculate Terrain Height Above Plant Grade (Chopped-off at Release Height)
      IF (FLAT) THEN
         HTERCHOP = 0.0
      ELSE IF (ELEV) THEN
         HTERCHOP = AMIN1( HS, (ZARG - ZS))
      END IF

C     Calculate Effective Plume Height (No Rise) Adjusted for Terrain Height
      HEOUT = HS - HTERCHOP

C     Save Plume Height for Flat Terrain for Later Comparison to Mixing Height
      HEFOUT = HS

      RETURN
      END

      SUBROUTINE PDISC(XARG,SZOUT,XZOUT,SBOUT)
C***********************************************************************
C                 PDISC Module of the ISC Short Term Model - Version 2
C
C        PURPOSE: Calculates Dispersion Parameters for POINT Sources
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    March 2, 1992
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
      MODNAM = 'PDISC'

C     Calculate Sigma-z from Curves Using Radial Distance   ---   CALL SIGZ
      CALL SIGZ(XARG,SZARG)
      SYARG = 0.0

      IF (.NOT. NOBID) THEN
C        Apply BID                                          ---   CALL BID
         CALL BID(DHPCMP,SYARG,SZARG,SYOUT,SZOUT,SBOUT)
      ELSE
         SBOUT = 0.0
         SZOUT = SZARG
         SYOUT = 0.0
      END IF
      XZOUT = 0.0

      IF (SZOUT .GT. 5000. .AND. NPD .EQ. 0)  SZOUT = 5000.

      RETURN
      END
          

      SUBROUTINE ADIS(XARG,SYOUT,SZOUT,XYOUT,XZOUT)
C***********************************************************************
C                 ADIS Module of the ISC Short Term Model - Version 2
C
C        PURPOSE: Calculates Dispersion Parameters for AREA Sources
C
C        PROGRAMMER: Roger Brode, PES, Inc.
C
C        DATE:    July 21, 1994
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
      MODNAM = 'ADIS'

      X1 = XARG * 0.001
      SZOUT = 1.0
      SYOUT = 1.0
      IF (RURAL .AND. X1 .LE. 0.0005) THEN
         CALL SZCOEF(X1, AFAC, BFAC, X1MAX, X1MIN)
         SZOUT = AFAC
      ELSE
C        Calculate Sigma-y from Curves for X                ---   CALL SIGY
         CALL SIGY(XARG,SYOUT)
C        Calculate Sigma-z from Curves for X                ---   CALL SIGZ
         CALL SIGZ(XARG,SZOUT)
      END IF
      SYOUT = MAX(SYOUT,0.0001)
      SZOUT = MAX(SZOUT,0.0001)
      XYOUT = 0.0
      XZOUT = 0.0

C     Add Initial Dispersion for OPENPIT Sources
      IF (SZINIT .GT. 0.0) THEN
         SZOUT = SQRT (SZOUT*SZOUT + SZINIT*SZINIT)
      END IF

      IF (SZOUT .GT. 5000. .AND. NPD .EQ. 0)  SZOUT = 5000.

      RETURN
      END


      SUBROUTINE PCOMPL
C***********************************************************************
C               PCOMPL Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Calculates Hourly Concentration or Deposition
C                 value for POINT Sources
C                 Using Gaussian Plume Equation for Complex Terrain
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
C        MODIFIED:   To use fully integrated COMPLEX1 algorithms rather
C                    than calls to CMP1.  R.W. Brode, PES, Inc. - 9/30/94
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

C     Variable Initializations
      MODNAM = 'PCOMPL'
      WDONLY = .FALSE.

      IF ((UNSTAB .OR. NEUTRL) .AND. HECOMP.GT.ZI) THEN
C        Plume Is Above Mixing Height, ZI
         IF (DEPOS .OR. WDEP) THEN
C           Set WDONLY flag for Wet Deposition Only
            WDONLY = .TRUE.
         ELSE
            DO ITYP = 1, NUMTYP
               HRVAL(ITYP) = 0.0
            END DO
            GO TO 1000
         END IF
      END IF

      IF (ABS(Y).LE.X*0.19891 .AND. CORR.GT.0.0) THEN
C        Receptor is inside of sector and Plume is < 400m Below Receptor
         IF (LDPART .OR. LWPART) THEN
            CALL PDEPC (WDONLY)
         ENDIF

         IF (NPD .EQ. 0) THEN
            DO ITYP = 1, NUMTYP
               V(ITYP) = 0.
            END DO
            VCOMP = 0.0
            ITYP = 0
C           Calculate the Vertical Term, V, for gases
            IF (CONC) THEN
               ITYP = ITYP + 1
               IF (WDONLY) THEN
C                 Plume is above mixing height so set CONC = 0.0
                  V(ITYP) = 0.0
               ELSE
C                 Calculate Concentration Form of V         ---   CALL VERT
                  A0 = -0.5/(SZCMP1*SZCMP1)
                  CALL VERT(HECMP1,SZCMP1,A0,ZFLAG,V(ITYP))
               END IF
            ELSE IF (INTERM) THEN
               IF (WDONLY) THEN
C                 Plume is above mixing height so set CONC = 0.0
                  VCOMP = 0.0
               ELSE
C                 Calculate Concentration Form of V         ---   CALL VERT
                  A0 = -0.5/(SZCMP1*SZCMP1)
                  CALL VERT(HECMP1,SZCMP1,A0,ZFLAG,VCOMP)
               END IF
            ENDIF
            IF (DEPOS) THEN
               ITYP = ITYP + 1
C              Calculate Wet Flux Form of V
C              Vertical Term is Integral of EXP terms Over All z
               V(ITYP) = SRT2PI*SZCMP1
C              Apply Scavenging Ratio
               V(ITYP) = V(ITYP) * GSCVRT
            ENDIF
            IF (DDEP) THEN
               ITYP = ITYP + 1
            END IF
            IF (WDEP) THEN
               ITYP = ITYP + 1
C              Calculate Wet Flux Form of V
C              Vertical Term is Integral of EXP terms Over All z
               V(ITYP) = SRT2PI*SZCMP1
C              Apply Scavenging Ratio
               V(ITYP) = V(ITYP) * GSCVRT
            ENDIF
C           Allow for Depletion of Gases Due to Wet Scavenging
            IF (LWGAS) THEN
               DO ITYP = 1, NUMTYP
                  V(ITYP) = V(ITYP) * WQCORGC
               END DO
               IF (.NOT.CONC .AND. INTERM) THEN
                  VCOMP = VCOMP * WQCORG
               END IF
            END IF

C           Apply CORR Attenuation Factor and
C           Include SZ in the denomenator of V
            DO ITYP = 1, NUMTYP
               V(ITYP) = CORR * V(ITYP)/SZCMP1
            END DO
            IF (.NOT.CONC .AND. INTERM) THEN
               VCOMP = CORR * VCOMP/SZCMP1
            END IF

         ELSE
C           Calculate the Vertical Term, V for particles
            DO ITYP = 1, NUMTYP
               V(ITYP) = 0.
            END DO
            VCOMP = 0.
            DO J = 1, NPD
               ITYP = 0
C              Settling may alter SZ for the Jth particle plume
               SZADJ = SZCMP1*SZCORC(J)
               A0 = -0.5/(SZADJ*SZADJ)
C              Calculate Plume Tilt Due to Settling, HV
               HV = (X/US) * VGRAV(J)
C              Calculate Settled Plume Height, HESETL
               HESETL = HECOMP - HV
C              Restrict settled height to be positive, so that the plume
C              does not settle below the surface -- this is the limit of
C              the tilted plume technique.
               HESETL = AMAX1(0.0,HESETL)
C              Calculate Adjusted Plume Height and Attenuation Factor
C              for This Particle Category
               CALL CTERAD(HESETL,ZELEV,HECMP1,CORRJ)
C              Adjust Jth contribution by mass fraction and source
C              depletion
               ADJ = PHI(J) * DQCORC(J) * WQCORC(J)
               IF (CONC) THEN
C                 Concentration
                  ITYP = ITYP + 1
                  IF (WDONLY) THEN
C                    Plume is above mixing height so set CONC = 0.0
                     V(ITYP) = 0.0
                  ELSE
C                    For Concentration, Complete Vertical Term is Needed for
C                    Each Particle Size Calculated at ZFLAG ---   CALL VERT
                     CALL VERT(HECMP1,SZADJ,A0,ZFLAG,VJ)
                     V(ITYP) = V(ITYP) + CORRJ*ADJ*PCORZRC(J)*VJ/SZADJ
                  END IF
               END IF
               IF (DEPOS .OR. DDEP) THEN
                  IF (WDONLY) THEN
C                    Plume is above mixing height so set DRYFLUX = 0.0
                     DRYFLUX = 0.0
                  ELSE
C                    For Dry Deposition, Complete Vertical Term is Needed for
C                    Each Particle Size Calculated at ZRDEP ---   CALL VERT
                     CALL VERT(HECMP1,SZADJ,A0,ZRDEP,VJ)
C                    Calculate Dry Flux VJ/SZ
                     DRYFLUX = CORRJ*ADJ*PCORZDC(J)*VDEP(J)*VJ/SZADJ
                  END IF
               END IF
               IF (DEPOS .OR. WDEP) THEN
C                 Calculate Wet Flux VJ/SZ --
C                 For Wet Flux, Vertical Term is Integral of EXP terms
C                 Over All z, so VJ/SZ=SQRT(2PI)
                  WETFLUX = CORRJ*ADJ*PSCVRT(J)*SRT2PI
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
                     VCOMP = 0.0
                  ELSE
C                    For Concentration, Complete Vertical Term is Needed for
C                    Each Particle Size Calculated at ZFLAG ---   CALL VERT
                     CALL VERT(HECMP1,SZADJ,A0,ZFLAG,VJ)
C                    Calculate Concentration for Intermediate Terrain Check
                     VCOMP = VCOMP + CORRJ*ADJ*PCORZRC(J)*VJ/SZADJ
                  END IF
               END IF
            ENDDO
         END IF

C        Calculate the Decay Term, D                        ---   CALL DECAY
         CALL DECAY (X)

         DO ITYP = 1, NUMTYP
C           Calculate HRVAL for Sector Average in Complex Terrain
            HRVAL(ITYP) = (QTK*EMIFAC(ITYP)*D*V(ITYP)) /
     &                    (SRT2PI*DISTR*DELTHP*US)
         END DO

         IF (.NOT.CONC .AND. INTERM) THEN
C           Calculate Concentration for Sector Average in Complex Terrain
            COMCON = (QTK*EMICON*D*VCOMP) /
     &               (SRT2PI*DISTR*DELTHP*US)
         ELSE IF (CONC .AND. INTERM) THEN
            COMCON = HRVAL(1)
         END IF

      ELSE
C        Receptor is outside of sector or Plume is > 400m Below Receptor
         DO ITYP = 1, NUMTYP
            HRVAL(ITYP) = 0.0
         END DO
         COMCON = 0.0
      END IF

 1000 CONTINUE

      IF (DEBUG) THEN
         WCMP1 = DELTHP * DISTR
         WRITE(IOUNIT,*) 'PCOMPL ----------------------------------'
         WRITE(IOUNIT,*) 'Hour, Receptor     =',IHOUR,IREC
         WRITE(IOUNIT,*) '  '
         WRITE(IOUNIT,*) 'QTK, D             =',QTK,D
         WRITE(IOUNIT,*) 'CORRJ, WCMP1, US   =',CORRJ,WCMP1,US
         WRITE(IOUNIT,*) 'PCOMPL ----------------------------------'
      END IF

      IF (DEBUG) THEN
C        Print Out Debugging Information                    ---   CALL DEBOUT
CRWB         CALL DEBOUT
      END IF

      RETURN
      END


      SUBROUTINE ASIMPL(X1,RCZ)
C***********************************************************************
C               ASIMPL Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Calculates Hourly Concentration or Deposition
C                 value for AREA Sources Using Numerical
C                 Integration Algorithm for Simple Terrain
C
C                 (Replaces ACHI and ADEP)
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
C        INPUTS:
C
C        OUTPUTS: Concentration or Deposition for A Unit Of
C                 Source/Receptor Combination
C
C        CALLED FROM:   PLUMEF
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      LOGICAL SCONC, SDEPOS, SDDEP, SWDEP, WDONLY

C     Variable Initializations
      MODNAM = 'ASIMPL'
      WDONLY = .FALSE.

C     Convert distance X1 from km to meters
      XARG = X1 * 1000.

C     Assign output type logicals to local variables
      SCONC  = CONC
      SDEPOS = DEPOS
      SDDEP  = DDEP
      SWDEP  = WDEP
C     Determine appropriate output type for this ITYP, set others to .FALSE.
      IF (ITYP .EQ. 1) THEN
         IF (CONC) THEN
            SDEPOS = .FALSE.
            SDDEP  = .FALSE.
            SWDEP  = .FALSE.
         ELSE IF (DEPOS) THEN
            SCONC  = .FALSE.
            SDDEP  = .FALSE.
            SWDEP  = .FALSE.
         ELSE IF (DDEP) THEN
            SCONC  = .FALSE.
            SDEPOS = .FALSE.
            SWDEP  = .FALSE.
         ELSE IF (WDEP) THEN
            SCONC  = .FALSE.
            SDEPOS = .FALSE.
            SDDEP  = .FALSE.
         END IF
      ELSE IF (ITYP .EQ. 2) THEN
         IF (CONC) THEN
            IF (DEPOS) THEN
               SCONC  = .FALSE.
               SDDEP  = .FALSE.
               SWDEP  = .FALSE.
            ELSE IF (DDEP) THEN
               SCONC  = .FALSE.
               SDEPOS = .FALSE.
               SWDEP  = .FALSE.
            ELSE IF (WDEP) THEN
               SCONC  = .FALSE.
               SDEPOS = .FALSE.
               SDDEP  = .FALSE.
            END IF
         ELSE IF (DEPOS) THEN
            IF (DDEP) THEN
               SCONC  = .FALSE.
               SDEPOS = .FALSE.
               SWDEP  = .FALSE.
            ELSE IF (WDEP) THEN
               SCONC  = .FALSE.
               SDEPOS = .FALSE.
               SDDEP  = .FALSE.
            END IF
         ELSE IF (DDEP) THEN
            IF (WDEP) THEN
               SCONC  = .FALSE.
               SDEPOS = .FALSE.
               SDDEP  = .FALSE.
            END IF
         END IF
      ELSE IF (ITYP .EQ. 3) THEN
         IF (CONC) THEN
            IF (DDEP) THEN
               SCONC  = .FALSE.
               SDEPOS = .FALSE.
               SWDEP  = .FALSE.
            ELSE IF (WDEP) THEN
               SCONC  = .FALSE.
               SDEPOS = .FALSE.
               SDDEP  = .FALSE.
            END IF
         ELSE
            IF (WDEP) THEN
               SCONC  = .FALSE.
               SDEPOS = .FALSE.
               SDDEP  = .FALSE.
            END IF
         END IF
      ELSE IF (ITYP .EQ. 4) THEN
         IF (WDEP) THEN
            SCONC  = .FALSE.
            SDEPOS = .FALSE.
            SDDEP  = .FALSE.
         END IF
      END IF

      IF ((UNSTAB .OR. NEUTRL) .AND. HEFLAT.GT.ZI) THEN
C        Plume is above mixing height, ZI
         IF (DEPOS .OR. WDEP) THEN
C           Set WDONLY flag for Wet Deposition Only
            WDONLY = .TRUE.
         ELSE
            V(ITYP) = 0.0
            RCZ     = 0.0
            GO TO 1000
         END IF
      END IF

      RCZ = 0.0
      IF (XARG .GE. 1.0) THEN
         IF (NPD .EQ. 0) THEN
            V(ITYP) = 0.
C           Calculate the Vertical Term, V, for gases
            IF (SCONC) THEN
               IF (WDONLY) THEN
C                 Plume is above mixing height so set CONC = 0.0
                  V(ITYP) = 0.0
               ELSE
C                 Calculate Concentration Form of V         ---   CALL VERT
                  A0 = -0.5/(SZ*SZ)
                  CALL VERT(HE,SZ,A0,ZFLAG,V(ITYP))
               END IF
            ELSE IF (SDEPOS .OR. SWDEP) THEN
C              Calculate Wet Flux Form of V
C              Vertical Term is Integral of EXP terms Over All z
               V(ITYP) = SRT2PI*SZ
C              Apply Scavenging Ratio
               V(ITYP) = V(ITYP) * GSCVRT
            ENDIF
C           Allow for Depletion of Gases Due to Wet Scavenging
            IF (LWGAS) THEN
               V(ITYP) = V(ITYP) * WQCORG
            END IF

C           Include SZ in the denomenator of V
            V(ITYP) = V(ITYP)/SZ

         ELSE
C           Calculate the Vertical Term, V for particles
            V(ITYP) = 0.0
            DO J = 1, NPD
C              Settling may alter SZ for the Jth particle plume
               SZADJ = SZ*SZCOR(J)
               A0 = -0.5/(SZADJ*SZADJ)
C              Calculate Plume Tilt Due to Settling, HV
               HV = (XARG/US) * VGRAV(J)
C              Calculate Settled Plume Height, HESETL
               HESETL = HE - HV
C              Restrict settled height to be positive, so that the plume
C              does not settle below the surface -- this is the limit of
C              the tilted plume technique.
               HESETL = AMAX1(0.0,HESETL)
C              Adjust Jth contribution by mass fraction and source
C              depletion
               ADJ = PHI(J) * DQCOR(J) * WQCOR(J)
               IF (SCONC) THEN
C                 Concentration
                  IF (WDONLY) THEN
C                    Plume is above mixing height so set CONC = 0.0
                     V(ITYP) = 0.0
                  ELSE
C                    For Concentration, Complete Vertical Term is Needed for
C                    Each Particle Size Calulated at ZFLAG     ---   CALL VERT
                     CALL VERT(HESETL,SZADJ,A0,ZFLAG,VJ)
                     V(ITYP) = V(ITYP) + ADJ*PCORZR(J)*VJ/SZADJ
                  END IF
               ELSE IF (SDEPOS) THEN
                  IF (WDONLY) THEN
C                    Plume is above mixing height so set DRYFLUX = 0.0
                     DRYFLUX = 0.0
                  ELSE
C                    For Dry Deposition, Complete Vertical Term is Needed for
C                    Each Particle Size Calulated at ZRDEP     ---   CALL VERT
                     CALL VERT(HESETL,SZADJ,A0,ZRDEP,VJ)
C                    Calculate Dry Flux VJ/SZ
                     DRYFLUX = ADJ*PCORZD(J)*VDEP(J)*VJ/SZADJ
                  END IF
C                 Calculate Wet Flux VJ/SZ --
C                 For Wet Flux, Vertical Term is Integral of EXP terms
C                 Over All z, so VJ/SZ=SQRT(2PI)
                  WETFLUX = ADJ*PSCVRT(J)*SRT2PI
C                 Wet & Dry fluxes of particles are summed
                  V(ITYP) = V(ITYP) + DRYFLUX + WETFLUX
               ELSE IF (SDDEP) THEN
                  IF (WDONLY) THEN
C                    Plume is above mixing height so set DDEP = 0.0
                     V(ITYP) = 0.0
                  ELSE
C                    For Dry Deposition, Complete Vertical Term is Needed for
C                    Each Particle Size Calulated at ZRDEP     ---   CALL VERT
                     CALL VERT(HESETL,SZADJ,A0,ZRDEP,VJ)
C                    Calculate Dry Flux VJ/SZ
                     DRYFLUX = ADJ*PCORZD(J)*VDEP(J)*VJ/SZADJ
C                    Dry flux of particles
                     V(ITYP) = V(ITYP) + DRYFLUX
                  END IF
               ELSE IF (SWDEP) THEN
C                 Calculate Wet Flux VJ/SZ --
C                 For Wet Flux, Vertical Term is Integral of EXP terms
C                 Over All z, so VJ/SZ=SQRT(2PI)
                  WETFLUX = ADJ*PSCVRT(J)*SRT2PI
C                 Wet flux of particles
                  V(ITYP) = V(ITYP) + WETFLUX
               ENDIF
            ENDDO
         END IF

C        Calculate the Decay Term, D                        ---   CALL DECAY
         CALL DECAY (XARG)

C        Complete TERM (SZ already in denomenator of V)
         RCZ = (D*V(ITYP))/(SRT2PI)

      END IF

 1000 CONTINUE

      RETURN
      END


c----------------------------------------------------------------------
      subroutine vdp
c----------------------------------------------------------------------
c
c --- ISC2ST     Version:  1.0     Level:  930215                   VDP
c                J. Scire, SRC
c
c --- MODIFIED   May 26, 1995
c                Modified atmospheric resistance term, ra, based on
c                D. Byun and R. Dennis, Atmos. Environ., Vol. 29, No. 1
c                R. W. Brode, PES, Inc.
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
      INCLUDE 'MAIN1.INC'
c
      real rd(npdmax)
c
      io6=iounit

C     Calculate Deposition Reference Height, ZRDEP.
      ZRDEP = AMAX1( 1.0, 20.*Z0M)
c
c ***
      if(DEBUG)then
         write(io6,*)
         write(io6,*)'SUBR. VDP -- Inputs'
         write(io6,*)'USTAR (m/s)     = ',ustar
         write(io6,*)'MONIN-EL (m)    = ',el
         write(io6,*)'Z0M (m)         = ',z0m
         write(io6,*)'VDPHOR (m/s)    = ',vdphor
         write(io6,*)'NPD             = ',npd
         write(io6,*)'PDIAM (um)      = ',(pdiam(n),n=1,npd)
         write(io6,*)'FRACT           = ',(phi(n),n=1,npd)
         write(io6,*)'PDENS (g/cm**3) = ',(pdens(n),n=1,npd)
         write(io6,*)'SC              = ',(sc(n),n=1,npd)
         write(io6,*)'VGRAV (m/s)     = ',(vgrav(n),n=1,npd)
         write(io6,*)'TSTOP (s)       = ',(tstop(n),n=1,npd)
         write(io6,*)'VAIRMS (m**2/s) = ',vairms
         write(io6,*)'ZRDEP (m)       = ',zrdep
         write(io6,*)'VDPHOR (m/s)    = ',vdphor
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
c --- Calculate atmospheric resistance (s/m)
      elabs=ABS(ell)
      if (ell .gt. 0.0) then
c ---    Stable
c ---    VK is the von Karman constant, set as parameter in MAIN1.INC
         psih = 4.7*zrdep/ell
         ra = (1.0/(vk*ustarr)) * (ALOG(zrdep/z0m) + psih)

      else
c ---    Unstable
         a1 = 16.*zrdep/elabs
         b1 = 16.*z0m/elabs
         ra = (1.0/(vk*ustarr)) * (1.0*ALOG(
     &        ((2.+a1)-2.*SQRT(1.+a1)) * ((2.+b1)+2.*SQRT(1.+b1)) /
     &        (a1*b1) ))
      endif
c
      t1=ustarr*ustarr/vairms
c
c ***
      if(DEBUG)then
         write(io6,*)
         write(io6,*)'USTARR (m/s)    = ',ustarr
         write(io6,*)'ELL (m)         = ',ell
         write(io6,*)'PSIH            = ',psih
      endif
c ***
c
c --- LOOP OVER SIZE INTERVALS
      do 10 i=1,npd
c
         st=tstop(i)*t1
c
c ---    Compute inertial impaction term
         xinert=10**(-3./st)
c
c ---    Adjust (raise) the Schmidt Number to the 2/3rd's power.
         Schmidt = sc(i) ** (-.667)                                       DTB94068
c
c ---    Compute the deposition layer resistance (s/m)
         rd(i)=1.0 / (ustarr * (Schmidt + xinert))                        DTB94068
c
c ---    Deposition velocity for this current interval
         vdep(i)=1.0/(ra+rd(i)+ra*rd(i)*vgrav(i))+vgrav(i)+vdphor
10    continue
c ***
      if(DEBUG)then
         write(io6,*)
         write(io6,*)'RA (s/m)    = ',ra
         write(io6,*)'RD (s/m)    = ',(rd(n),n=1,npd)
         write(io6,*)'VDEP (m/s)  = ',(vdep(n),n=1,npd)
      endif
c ***
c
      return
      end

c-----------------------------------------------------------------------
      subroutine setszmn
c-----------------------------------------------------------------------
c
c --- ISCST2    Version: 1.0            Level: 931215           SETSZMN
c               D. Strimaitis, SRC
c
c PURPOSE:     SETSZMN determines the value of sigma-z at which the rate
c              of growth in sigma-z equals the rate at which the settling
c              velocity acts to reduce the height of the center-of-mass.
c              A default minimum of 2*zd, where zd is the near-surface
c              height at which the deposition flux is evaluated, is
c              returned if there is no balance-point.
c
c ARGUMENTS:  (MAIN1.INC)
c    PASSED:  kst       stability class (A=1, F=6)                   [i]
c             zrdep     reference height for deposition flux  (m)    [r]
c             vs        settling velocity  (m/s)                     [r]
c             us        plume advection wind speed (m/s)             [r]
c             urban     logical for URBAN/RURAL dispersion params    [l]
c             npd       number of particle size categories           [i]
c
c  RETURNED:  szmin     Minimum value of sigma-z (m)                 [r]
c
c CALLING ROUTINES:   PCALC, VCALC, ACALC
c
c EXTERNAL ROUTINES:  GCUBIC
c-----------------------------------------------------------------------
      include 'MAIN1.INC'

      real root(3),car(6),cau(6),cbr(6),cbu(6)

      data car/.2,.12,.08,.06,.03,.016/
      data cbr/0.,0.,.0002,.0015,.0003,.0003/
      data cau/.24,.24,.2,.14,.08,.08/
      data cbu/.001,.001,0.,.0003,.0015,.0015/


c --- Loop over particle sizes
      do i=1,npd
         xmin=0.0
         szmin(i)=2.*zrdep
         c=rtpiby2*vgrav(i)/us

c ---    Urban section
         if(URBAN) then
            a=cau(kst)
            b=cbu(kst)
            if(kst .GE. 4) then
               if(a .GT. 20.*c) then
                  szmin(i)=a*a/(2.*b*c)
               elseif(a .GT. c) then
c ---             Solve cubic for y=bx, then report x      ---  call GCUBIC
                  aby2csq=(a/(2.*c))**2
                  a1=(3.-aby2csq)
                  a2=(3.-4.*aby2csq)
                  a3=(1.-4.*aby2csq)
                  call GCUBIC(a1,a2,a3,root)
c ---             There should be ONE real root
                  if(root(2) .NE. 0. .OR. root(3) .NE. 0.) then
                     write(*,*) 'SETSZMN: Potential error!!! '
                     write(*,*) 'More than 1 root ----'
                     write(*,*) 'xb= ',(root(j),j=1,3)
                  endif
                  xmin=root(1)/b
                  szmin(i)=a*xmin/SQRT(1.+b*xmin)
               endif
            endif

c ---    Rural section
         else
            a=car(kst)
            b=cbr(kst)
            if(kst .EQ. 3 .OR. kst .EQ. 4) then
               if(a .GT. 20.*c) then
                  szmin(i)=a*a/(2.*b*c)
               elseif(a .GT. c) then
c ---             Solve cubic for y=bx, then report x      ---  call GCUBIC
                  aby2csq=(a/(2.*c))**2
                  a1=(3.-aby2csq)
                  a2=(3.-4.*aby2csq)
                  a3=(1.-4.*aby2csq)
                  call GCUBIC(a1,a2,a3,root)
c ---             There should be ONE real root
                  if(root(2) .NE. 0. .OR. root(3) .NE. 0.) then
                     write(*,*) 'Potential error!!! More than 1 root'
                     write(*,*) 'xb= ',(root(j),j=1,3)
                  endif
                  xmin=root(1)/b
                  szmin(i)=a*xmin/SQRT(1.+b*xmin)
               endif
            elseif(kst .GT. 4) then
               if(a .GT. c) then
                  xmin=(SQRT(a/c)-1.)/b
                  szmin(i)=a*xmin/(1+b*xmin)
               endif
            endif
         endif

      enddo

      return
      end

c-----------------------------------------------------------------------
      subroutine gcubic(a1,a2,a3,root)
c-----------------------------------------------------------------------
c
c --- ISCST2    Version: 1.0            Level: 931215           GCUBIC
c               D. Strimaitis, SRC
c
c PURPOSE:     Program solves the general cubic equation of the form:
c                  0 = x**3 + (a1)x**2 + (a2)x + (a3)
c              for the real roots
c              (Numerical Recipes, Press et al., 1986)
c
c ARGUMENTS:
c    PASSED:  a1,a2,a3  constants for terms as described above       [r]
c
c  RETURNED:  root      root(s) of equation                          [r]
c
c CALLING ROUTINES:   (utility routine)
c
c EXTERNAL ROUTINES:  none
c-----------------------------------------------------------------------

      real root(3)

      data twopi/6.2831853/,fourpi/12.566371/

      third=1./3.
      a1sq=a1*a1
      a1cube=a1*a1sq
      a1by3=a1*third

      q=(a1sq-3.*a2)/9.
      r=(2.*a1cube-9.*a1*a2+27.*a3)/54.

      qcube=q*q*q
      rsq=r*r

      if(qcube .GE. rsq) then
c ---    THREE real roots
         sqrtq2=SQRT(q)*2.
         theta=ACOS(r/SQRT(qcube))
         root(1)=-sqrtq2*COS(theta/3.)-a1by3
         root(2)=-sqrtq2*COS((theta+twopi)/3.)-a1by3
         root(3)=-sqrtq2*COS((theta+fourpi)/3.)-a1by3
      else
c ---    ONE real root
         arg=(SQRT(rsq-qcube)+ABS(r))**third
         root(1)=-SIGN(1.0,r)*(arg+q/arg)-a1by3
         root(2)=0.
         root(3)=0.
      endif


      return
      end

c----------------------------------------------------------------------
      subroutine scavrat
c----------------------------------------------------------------------
c
c --- ISCST2     Version: 1.0       Level: 931108               SCAVRAT
c                D. Strimaitis, SRC
c
c --- PURPOSE:  Compute the wet SCAVenging RATio for particles, as a
c               function of particle size, and for gases
c
c --- INPUTS:
c     Common block /METVAR/ variables:
c            IPCODE - integer    - Precip. code (00-45)
c             PRATE - real       - Precip. rate (mm/hr)
c                TA - real       - Ambient Temperature (deg K)
c     Common block /CALCS3/ variables:
c               NPD - integer    - Number of particle size categories
c             PSCAV - real array - Particle scavenging coefs. for liquid
c                                  (1) and frozen (2) precip. for each
c                                  size category (1/[s-mm/hr])
c             GSCAV - real array - Gas scavenging coefs. for liquid (1)
c                                  and frozen (2) precip. (1/[s-mm/hr])
c
c --- OUTPUT:
c     Common block /CALCS3/ variables:
c            PSCVRT - real array - Scavenging ratio for particles (1/s)
c            GSCVRT - real       - Scavenging ratio for gases (1/s)
c
c --- SCAVRAT called by:  PCALC, VCALC, ACALC
c --- SCAVRAT calls:      none
c----------------------------------------------------------------------
c
c --- Include common blocks
      include 'MAIN1.INC'

      data imiss/9999/

      if(DEBUG)then
         write(iounit,*)
         write(iounit,*)'SUBR. SCAVRAT -- Inputs'
         write(iounit,*)'IPCODE               = ',ipcode
         write(iounit,*)'PRATE (mm/hr)        = ',prate
         write(iounit,*)'TA (deg K)           = ',ta
         write(iounit,*)'NPD                  = ',npd
         write(iounit,*)'PSCAV(1) 1/(s-mm/hr) = ',(pscav(n,1),n=1,npd)
         write(iounit,*)'PSCAV(2) 1/(s-mm/hr) = ',(pscav(n,2),n=1,npd)
         write(iounit,*)'GSCAV(1) 1/(s-mm/hr) = ',gscav(1)
         write(iounit,*)'GSCAV(2) 1/(s-mm/hr) = ',gscav(2)
         write(iounit,*)' (1 = Liquid ; 2 = Frozen )'
         write(iounit,*)
      endif

c --- If no precipitation, no wet removal
      if(prate .EQ. 0.) then
         do i=1,npd
            pscvrt(i)=0.0
         enddo
         gscvrt=0.0
      else
c ---    Determine if precip. is liquid (ILQ=1) or frozen (ILQ=2)
         if(ipcode .EQ. imiss .OR. ipcode .EQ. 0) then
c ---       Precip. code is unavailable due to missing data or no
c           precip. at time of obs. at surface station, therefore,
c           determine precip. type based on the air temperature
c ---       Assume liquid precip. if temp. > freezing, otherwise,
c           assume frozen precip.
            if(ta .GT. 273.15) then
               ilq=1
            else
               ilq=2
            endif
         else if(ipcode .LE. 18) then
c ---       Liquid precipitation type
            ilq=1
         else
c ---       Frozen precipitation type
            ilq=2
         endif
c ---    Determine the scavenging ratios
         do i=1,npd
            pscvrt(i)=pscav(i,ilq)*prate
         enddo
         gscvrt=gscav(ilq)*prate
      endif

      if(DEBUG)then
         write(iounit,*)'SUBR. SCAVRAT -- Results'
         write(iounit,*)'GSCVRT (1/s)= ',gscvrt
         write(iounit,*)'PSCVRT (1/s)= ',(pscvrt(n),n=1,npd)
         write(iounit,*)
      endif

      return
      end


      SUBROUTINE PDEP (LWDONLY)
C***********************************************************************
C               PDEP Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Calculates Simple Terrain Deposition Adjustment
C                 Factors from DEPCOR
C
C        PROGRAMMER: R. W. Brode, PES, Inc.
C
C        DATE:    September 30, 1994
C
C        MODIFIED:   To add logical argument for Wet Deposition Only,
C                    to skip call to DEPCOR when plume is above ZI.
C                    R.W. Brode, PES, 7/17/95
C
C        INPUTS:     LWDONLY, logical specifying whether Wet Deposition
C                    Only is to be calculated for plume above ZI
C
C        OUTPUTS:
C
C
C        CALLED FROM:   PSIMPL
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      LOGICAL LTERR, LWDONLY

C     Variable Initializations
      MODNAM = 'PDEP'

C     Set LTERR to FALSE to signal simple terrain call to DEPCOR.
      LTERR = .FALSE.

C     Loop over particle sizes
      DO 150 I=1,NPD
         DQCOR(I)  = 1.0
         PCORZR(I) = 1.0
         PCORZD(I) = 1.0
         SZCOR(I)  = 1.0
C        Initialize wet & dry source depletion factors,
C        profile correction factors, and settles sigma-z
C        factors to unity. - Done in DEPCOR
         IF (DDPLETE .AND. .NOT.LWDONLY) THEN
C           Determine factors for depletion - note that
C           plume ht adjustment for terrain is signalled
C           by a local logical - LTERR
C           Simple Terrain Model          ---   CALL DEPCOR
            CALL DEPCOR( VDEP(I),VGRAV(I),ZRDEP,ZFLAG,
     &        X,XZ,HEFLAT,ZI,US,XS,YS,XR,YR,
     &        RURAL,URBAN,KST,SZ,SBID,
     &        SZMIN(I),ZELEV,ZS,LTERR,DEBUG,IOUNIT,
     &        SRCTYP(ISRC),LTGRID,KURDAT,
     &        DQCOR(I),PCORZR(I),PCORZD(I),SZCOR(I))
         END IF
         IF (WDPLETE) THEN
C           Determine source depletion factor
C           from wet removal
C           Simple Terrain Model
            WQCOR(I) = EXP(-PSCVRT(I)*X/US)
         ELSE
            WQCOR(I) = 1.
         ENDIF
150   CONTINUE

      RETURN
      END


      SUBROUTINE PDEPC (LWDONLY)
C***********************************************************************
C               PDEPC Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Calculates Complex Terrain Deposition Adjustment
C                 Factors from DEPCOR
C
C        PROGRAMMER: R. W. Brode, PES, Inc.
C
C        DATE:    September 30, 1994
C
C        MODIFIED:   To add logical argument for Wet Deposition Only,
C                    to skip call to DEPCOR when plume is above ZI.
C                    R.W. Brode, PES, 7/17/95
C
C        INPUTS:     LWDONLY, logical specifying whether Wet Deposition
C                    Only is to be calculated for plume above ZI
C
C        OUTPUTS:
C
C
C        CALLED FROM:   PCOMPL
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      LOGICAL LTERR, LWDONLY

C     Variable Initializations
      MODNAM = 'PDEPC'

C     Set LTERR to TRUE to signal complex terrain call to DEPCOR.
      LTERR = .TRUE.

C     Loop over particle sizes
      DO 150 I=1,NPD
         DQCORC(I)  = 1.0
         PCORZRC(I) = 1.0
         PCORZDC(I) = 1.0
         SZCORC(I)  = 1.0
C        Initialize wet & dry source depletion factors,
C        profile correction factors, and settles sigma-z
C        factors to unity. - Done in DEPCOR
         IF (DDPLETE .AND. .NOT.LWDONLY) THEN
C           Determine factors for depletion - note that
C           plume ht adjustment for terrain is signalled
C           by a local logical - LTERR
C           Complex Terrain Model         ---   CALL DEPCOR
            CALL DEPCOR( VDEP(I),VGRAV(I),ZRDEP,ZFLAG,
     &        DISTR,XZCMP1,HECOMP,ZI,US,XS,YS,XR,YR,
     &        RURAL,URBAN,KST,SZCMP1,SBCMP1,
     &        SZMIN(I),ZELEV,ZS,LTERR,DEBUG,IOUNIT,
     &        SRCTYP(ISRC),LTGRID,KURDAT,
     &        DQCORC(I),PCORZRC(I),PCORZDC(I),
     &        SZCORC(I))
         END IF
         IF (WDPLETE) THEN
C           Determine source depletion factor
C           from wet removal
C           Complex Terrain Model - use radial distance
            WQCORC(I) = EXP(-PSCVRT(I)*DISTR/US)
         ELSE
            WQCORC(I) = 1.
         ENDIF
150   CONTINUE

      RETURN
      END
      SUBROUTINE EMFACT (QARG)
C***********************************************************************
C                 EMFACT Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Applies Variable Emission Rate and
C                 Unit Conversion Factors
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C        MODIFIED  : for handling OpenPit Source Type - PES, 7/26/94
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Arrays of Source Parameters
C                 Date and Hour
C                 Meteorological Variables for One Hour
C                 Variable Emission Rate Flags and Factors
C                 Unit Conversion Rate Factors
C
C        OUTPUTS: Adjusted Emission Rate, QTK
C
C        CALLED FROM:   PCALC
C                       VCALC
C                       ACALC
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'EMFACT'

C     Apply Emission Unit Factor (EMIFAC) and Variable Emission Rate
C     Factor, Based on Value of QFLAG
      IF (QFLAG(ISRC) .EQ. ' ') THEN
         QTK = QARG

C*----   ISCSTM Modification: To handle hourly emissions - jah 11/4/94
      ELSE IF (QFLAG(ISRC) .EQ. 'HOURLY') THEN
         QTK = QARG
C*----
C*#

      ELSE IF (QFLAG(ISRC) .EQ. 'MONTH') THEN
         QTK = QARG * QFACT(IMONTH,ISRC)
      ELSE IF (QFLAG(ISRC) .EQ. 'HROFDY') THEN
         QTK = QARG * QFACT(IHOUR,ISRC)
      ELSE IF (QFLAG(ISRC) .EQ. 'STAR') THEN
         QTK = QARG * QFACT((IUCAT+(KST-1)*NWSCAT),ISRC)
      ELSE IF (QFLAG(ISRC) .EQ. 'SEASON' .OR.
     &         QFLAG(ISRC) .EQ. 'SEASHR') THEN
C        Determine SEASON
         IF (IMONTH.LE.2 .OR. IMONTH.EQ.12) THEN
            ISEAS = 1
         ELSE IF (IMONTH.GE.3 .AND. IMONTH.LE.5) THEN
            ISEAS = 2
         ELSE IF (IMONTH.GE.6 .AND. IMONTH.LE.8) THEN
            ISEAS = 3
         ELSE IF (IMONTH.GE.9 .AND. IMONTH.LE.11) THEN
            ISEAS = 4
         END IF
         IF (QFLAG(ISRC) .EQ. 'SEASON') THEN
            QTK = QARG * QFACT(ISEAS,ISRC)
         ELSE IF (QFLAG(ISRC). EQ. 'SEASHR') THEN
            QTK = QARG * QFACT((IHOUR+(ISEAS-1)*24),ISRC)
         END IF
      END IF

      RETURN
      END

      SUBROUTINE WSADJ
C***********************************************************************
C                 WSADJ Module of ISC2 Short Term Model - ISCST2
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
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'WSADJ'

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
C                 DISTF Module of ISC2 Short Term Model - ISCST2
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
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'DISTF'

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

      SUBROUTINE DECAY (XARG)
C***********************************************************************
C                 DECAY Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Calculates Decay Term for Use in Gaussian Plume Equation
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Downwind Distance, XARG (m)
C                 Stack Top Wind Speed, US (m/s)
C                 Decay Coefficient, DECOEFF (1/s)
C
C        OUTPUTS: Decay Term, D
C
C        CALLED FROM:   CHI
C                       DEP
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'DECAY'

      D = 1.0

      IF (DECOEF .GT. 0.0) THEN
         D = EXP (AMAX1 (EXPLIM, -DECOEF*XARG/US))
      END IF

      RETURN
      END

      SUBROUTINE VERT(HEARG,SZARG,A0,ZARG,VOUT)
C***********************************************************************
C                 VERT Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Calculates Vertical Term for Use in Gaussian Plume Equation
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        MODIFIED BY R.W. Brode, PES, Inc. to use calling arguments - 9/30/94
C
C        MODIFIED BY D. Strimaitis, SRC (for Wet REMOVAL of Gases)
C
C        DATE:    November 8, 1993
C
C
C        INPUTS:  Plume Height
C                 Vertical Dispersion Parameter
C                 Stability Class
C                 Mixing Height
C                 Receptor Height Above Ground
C
C        OUTPUTS: Vertical Term, VOUT
C
C        CALLED FROM:   PSIMPL, PCOMPL, ASIMPL
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'VERT'

      VOUT = 0.0

      IF (ZARG .EQ. 0.0) THEN
C        Vertical Term for Case With No Flagpole Receptor
         IF (STABLE .OR. ZI.GE.10000.) THEN
            A1 = A0 * HEARG * HEARG
            IF (A1 .GT. EXPLIM)  VOUT = 2.*EXP(A1)
         ELSE IF ((SZARG/ZI) .GE. 1.6) THEN
            VOUT  = SRT2PI*(SZARG/ZI)
         ELSE
            A1 = A0 * HEARG * HEARG
            IF (A1 .GT. EXPLIM)  VOUT = EXP(A1)
            SUM = 0.0
            DO 100 I = 1, 100
               T  = 0.0
               TWOIZI = 2.*I*ZI
               A2 = A0 * (TWOIZI-HEARG) * (TWOIZI-HEARG)
               A3 = A0 * (TWOIZI+HEARG) * (TWOIZI+HEARG)
               IF (A2 .GT. EXPLIM)  T = EXP(A2)
               IF (A3 .GT. EXPLIM)  T = T + EXP(A3)
               SUM = SUM + T
               IF (ABS(T) .LE. 5.0E-9) THEN
C                 Exit Loop
                  GO TO 200
               END IF
 100        CONTINUE
C           Calculate Total Vert. Term - (2.*) was Removed for Optimization
 200        VOUT  = 2.*(VOUT + SUM)
         END IF
      ELSE
C        Vertical Term for Case of ZARG .NE. 0.0
         IF (STABLE .OR. ZI .GE. 10000.) THEN
            A1 = A0 * (ZARG-HEARG) * (ZARG-HEARG)
            A2 = A0 * (ZARG+HEARG) * (ZARG+HEARG)
            IF (A1 .GT. EXPLIM)  VOUT = EXP(A1)
            IF (A2 .GT. EXPLIM)  VOUT = VOUT + EXP(A2)
         ELSE IF (SZARG/ZI .GE. 1.6) THEN
            VOUT  = SRT2PI*(SZARG/ZI)
         ELSE
            A1 = A0 * (ZARG-HEARG) * (ZARG-HEARG)
            A2 = A0 * (ZARG+HEARG) * (ZARG+HEARG)
            IF (A1 .GT. EXPLIM)  VOUT = EXP(A1)
            IF (A2 .GT. EXPLIM)  VOUT = VOUT + EXP(A2)
            SUM = 0.0
            DO 300 I = 1, 100
               T  = 0.0
               TWOIZI = 2.*I*ZI
               A3 = A0 * (ZARG-(TWOIZI-HEARG)) * (ZARG-(TWOIZI-HEARG))
               A4 = A0 * (ZARG+(TWOIZI-HEARG)) * (ZARG+(TWOIZI-HEARG))
               A5 = A0 * (ZARG-(TWOIZI+HEARG)) * (ZARG-(TWOIZI+HEARG))
               A6 = A0 * (ZARG+(TWOIZI+HEARG)) * (ZARG+(TWOIZI+HEARG))
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
 400        VOUT  = VOUT + SUM
         END IF
      END IF

      RETURN
      END

      SUBROUTINE SUMVAL
C***********************************************************************
C                 SUMVAL Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Sums HRVAL to AVEVAL and ANNVAL Arrays
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  HRVAL - Hourly Value for (IREC,ISRC) Combination
C                 Averaging Period Options
C                 Source Groupings
C
C        OUTPUTS: Updated Sums of AVEVAL and ANNVAL Arrays
C
C        CALLED FROM:   PCALC
C                       VCALC
C                       ACALC
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'SUMVAL'

C     Begin LOOP Over Output Types
      DO 1500 ITYP = 1, NUMTYP
         IF (HRVAL(ITYP) .NE. 0.0) THEN
C           Begin Source Group LOOP
            DO 1000 IGRP = 1, NUMGRP
C              Check for Source Belonging to Group
               IF (IGROUP(ISRC,IGRP) .EQ. 1) THEN
C                 Begin Averaging Period LOOP
                  DO 500 IAVE = 1, NUMAVE
                     AVEVAL(IREC,IGRP,IAVE,ITYP) = HRVAL(ITYP) +
     &                                       AVEVAL(IREC,IGRP,IAVE,ITYP)
 500              CONTINUE
C                 End Averaging Period LOOP
                  IF (PERIOD .OR. ANNUAL) THEN
                     ANNVAL(IREC,IGRP,ITYP) = HRVAL(ITYP) +
     &                                        ANNVAL(IREC,IGRP,ITYP)
                  END IF
               END IF
 1000       CONTINUE
C           End Source Group LOOP
         END IF
 1500 CONTINUE
C     End LOOP Over Output Types

      RETURN
      END
      SUBROUTINE AVER
C***********************************************************************
C                 AVER Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Calculates Short Term (<=24 hr) Average Concentrations
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Averaging Time Option Switches
C                 Updated Array of Cumulative Values, AVEVAL
C
C        OUTPUTS: Updated Array of Averages, AVEVAL
C
C        CALLED FROM: HRLOOP
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'AVER'

      IF (KAVE(IAVE) .NE. 1) THEN
C        Calculate Denominator Considering Calms and Missing,
C        Skipping Averaging if Averaging Period is 1-Hour
         SNUM = AMAX0((NUMHRS(IAVE)-NUMCLM(IAVE)-NUMMSG(IAVE)),
     &                 NINT(NUMHRS(IAVE)*0.75+0.4))
C        Begin Source Group LOOP
         DO 700 IGRP = 1, NUMGRP
C           Begin Receptor LOOP
            DO 600 IREC = 1, NUMREC
               AVEVAL(IREC,IGRP,IAVE,1) = (1./SNUM)*
     &                                  AVEVAL(IREC,IGRP,IAVE,1)
 600        CONTINUE
C           End Receptor LOOP
 700     CONTINUE
C        End Source Group LOOP
      END IF

      RETURN
      END

      SUBROUTINE HIVALS
C***********************************************************************
C                 HIVALS Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Updates High Value Tables
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  High Value Option Switches
C                 Array of CONC or DEPOS Averages
C
C        OUTPUTS: Updated High Value Arrays
C
C        CALLED FROM:   MAIN
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'HIVALS'

C     Check for High/Max Value Options - Skip Update If KAVE=1,
C     And No CALCS Were Made for the Current Hour
      IF (CALCS .OR. KAVE(IAVE).NE.1) THEN
         IF (INHI(IAVE) .EQ. 1) THEN
            DO ITYP = 1, NUMTYP
C              Update High Values for Each Receptor            ---   CALL NHIGH
               CALL NHIGH
            END DO
         END IF
         IF (MAXAVE(IAVE) .EQ. 1) THEN
            DO ITYP = 1, NUMTYP
C              Update Maximum Value Table for KAVE             ---   CALL MAXVAL
               CALL MAXVAL
            END DO
         END IF
      END IF
C     Reset Counters for This Averaging Period
      NUMHRS(IAVE) = 0
      NUMCLM(IAVE) = 0
      NUMMSG(IAVE) = 0

      RETURN
      END

      SUBROUTINE NHIGH
C***********************************************************************
C                 NHIGH Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Update Highest Value by Receptor Arrays
C                 NVAL = 6 Assigned in PARAMETER Statement
C                 Note: For duplicate values, the earlier occurrence keeps its
C                       rank within the array
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  High Value Options
C                 Array of CONC or DEPOS Averages
C                 Averaging Period
C
C        OUTPUTS: Updated Highest Value Array
C                 Updated Highest Date Array
C
C        CALLED FROM:   HIVALS
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'NHIGH'

C     Begin Source Group LOOP
      DO 2000 IGRP = 1, NUMGRP
C        Begin Receptor LOOP
         DO 1000 IREC = 1, NUMREC
            IF (NHIVAL .GT. 1) THEN
               IF (AVEVAL(IREC,IGRP,IAVE,ITYP) .GT.
     &                    HIVALU(IREC,NHIVAL,IGRP,IAVE,ITYP)) THEN
                  DO 100 J = NHIVAL-1, 1, -1
                     IF (AVEVAL(IREC,IGRP,IAVE,ITYP) .LE.
     &                          HIVALU(IREC,J,IGRP,IAVE,ITYP)) THEN
                        HIVALU(IREC,J+1,IGRP,IAVE,ITYP) =
     &                      AVEVAL(IREC,IGRP,IAVE,ITYP)
                        IF (NUMCLM(IAVE).EQ.0 .AND.
     &                      NUMMSG(IAVE).EQ.0) THEN
                           HCLMSG(IREC,J+1,IGRP,IAVE,ITYP) = ' '
                        ELSE
C                          Set Indicator Of Calm and Msg    ---   CALL HSETFG
                           CALL HSETFG(0,J)
                        END IF
                        NHIDAT(IREC,J+1,IGRP,IAVE,ITYP) = KURDAT
C                       Exit Block
                        GO TO 200
                     ELSE
                        HIVALU(IREC,J+1,IGRP,IAVE,ITYP) =
     &                    HIVALU(IREC,J,IGRP,IAVE,ITYP)
                        HCLMSG(IREC,J+1,IGRP,IAVE,ITYP) =
     &                    HCLMSG(IREC,J,IGRP,IAVE,ITYP)
                        NHIDAT(IREC,J+1,IGRP,IAVE,ITYP) =
     &                    NHIDAT(IREC,J,IGRP,IAVE,ITYP)
                        IF (J .EQ. 1) THEN
                           HIVALU(IREC,1,IGRP,IAVE,ITYP) =
     &                       AVEVAL(IREC,IGRP,IAVE,ITYP)
                           IF (NUMCLM(IAVE).EQ.0 .AND.
     &                         NUMMSG(IAVE).EQ.0) THEN
                              HCLMSG(IREC,1,IGRP,IAVE,ITYP) = ' '
                           ELSE
C                             Set Indicator Of Calm and Msg ---   CALL HSETFG
                              CALL HSETFG(1,1)
                           END IF
                           NHIDAT(IREC,1,IGRP,IAVE,ITYP) = KURDAT
                        END IF
                     END IF
 100              CONTINUE
               END IF
            ELSE IF (NHIVAL .EQ. 1) THEN
               IF (AVEVAL(IREC,IGRP,IAVE,ITYP) .GT.
     &                    HIVALU(IREC,1,IGRP,IAVE,ITYP)) THEN
             HIVALU(IREC,1,IGRP,IAVE,ITYP) = AVEVAL(IREC,IGRP,IAVE,ITYP)
                  IF (NUMCLM(IAVE).EQ.0 .AND.
     &                NUMMSG(IAVE).EQ.0) THEN
                     HCLMSG(IREC,1,IGRP,IAVE,ITYP) = ' '
                  ELSE
C                    Set Indicator Of Calm and Missing      ---   CALL HSETFG
                     CALL HSETFG(1,1)
                  END IF
                  NHIDAT(IREC,1,IGRP,IAVE,ITYP) = KURDAT
               END IF
            END IF
 200        CONTINUE
C        End Receptor LOOP
 1000    CONTINUE
C     End Source Group LOOP
 2000 CONTINUE

      RETURN
      END

      SUBROUTINE HSETFG(INDT,J)
C***********************************************************************
C                 HSETFG Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Set Calm and Missing Flag Of the Result
C
C        PROGRAMMER: Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To correct error in order of indices for array
C                    HCLMSG on first assignment to 'b' - 9/29/92
C
C        INPUTS:  High Value Options
C                 Array of CONC or DEPOS Averages
C                 Averaging Period
C
C        OUTPUTS: Updated Highest Value Flag Array
C
C        CALLED FROM:   NHIGH
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'HSETFG'

      IF (INDT .EQ. 0) THEN
C        Set Indicator Of Calm and Missing
         IF (NUMCLM(IAVE).NE.0 .AND.
     &       NUMMSG(IAVE).EQ.0) THEN
             HCLMSG(IREC,J+1,IGRP,IAVE,ITYP) = 'c'
         ELSE IF (NUMCLM(IAVE).EQ.0 .AND.
     &            NUMMSG(IAVE).NE.0) THEN
             HCLMSG(IREC,J+1,IGRP,IAVE,ITYP) = 'm'
         ELSE IF (NUMCLM(IAVE).NE.0 .AND.
     &            NUMMSG(IAVE).NE.0) THEN
             HCLMSG(IREC,J+1,IGRP,IAVE,ITYP) = 'b'
         END IF
      ELSE IF (INDT .EQ. 1) THEN
C        Set Indicator Of Calm and Missing
         IF (NUMCLM(IAVE).NE.0 .AND.
     &       NUMMSG(IAVE).EQ.0) THEN
             HCLMSG(IREC,1,IGRP,IAVE,ITYP) = 'c'
         ELSE IF (NUMCLM(IAVE).EQ.0 .AND.
     &            NUMMSG(IAVE).NE.0) THEN
             HCLMSG(IREC,1,IGRP,IAVE,ITYP) = 'm'
         ELSE IF (NUMCLM(IAVE).NE.0 .AND.
     &            NUMMSG(IAVE).NE.0) THEN
             HCLMSG(IREC,1,IGRP,IAVE,ITYP) = 'b'
         END IF
      END IF

      RETURN
      END

      SUBROUTINE MAXVAL
C***********************************************************************
C                 MAXVAL Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Update Overall Maximum Value Arrays
C                 NMAX = 50 Assigned in PARAMETER Statement
C                 Note: For duplicate values, the earlier occurrence keeps
C                       its rank within the array
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Maximum Value Table Options
C                 Array of CONC or DEPOS Averages
C                 Averaging Period
C
C        OUTPUTS: Updated Maximum Value Array
C                 Updated Maximum Date Array
C                 Updated Maximum Receptor Array
C
C        CALLED FROM:   HIVALS
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'MAXVAL'

C     Begin Source Group LOOP
      DO 2000 IGRP = 1, NUMGRP
C        Begin Receptor LOOP
         DO 1000 IREC = 1, NUMREC
            IF (NMXVAL .GT. 1) THEN
               IF (AVEVAL(IREC,IGRP,IAVE,ITYP) .GT.
     &                       RMXVAL(NMXVAL,IGRP,IAVE,ITYP)) THEN
                  DO 100 J = NMXVAL-1, 1, -1
                     IF(AVEVAL(IREC,IGRP,IAVE,ITYP) .LE.
     &                     RMXVAL(J,IGRP,IAVE,ITYP)) THEN
                RMXVAL(J+1,IGRP,IAVE,ITYP) = AVEVAL(IREC,IGRP,IAVE,ITYP)
                        IF (NUMCLM(IAVE).EQ.0 .AND.
     &                      NUMMSG(IAVE).EQ.0) THEN
                           MCLMSG(J+1,IGRP,IAVE,ITYP) = ' '
                        ELSE
C                          Set Indicator Of Calm and Msg    ---   CALL MSETFG
                           CALL MSETFG(0,J)
                        END IF
                        MXDATE(J+1,IGRP,IAVE,ITYP) = KURDAT
                        MXLOCA(J+1,IGRP,IAVE,ITYP) = IREC
C                       Exit Block
                        GO TO 200
                     ELSE
                   RMXVAL(J+1,IGRP,IAVE,ITYP) = RMXVAL(J,IGRP,IAVE,ITYP)
                   MXDATE(J+1,IGRP,IAVE,ITYP) = MXDATE(J,IGRP,IAVE,ITYP)
                   MCLMSG(J+1,IGRP,IAVE,ITYP) = MCLMSG(J,IGRP,IAVE,ITYP)
                   MXLOCA(J+1,IGRP,IAVE,ITYP) = MXLOCA(J,IGRP,IAVE,ITYP)
                        IF (J .EQ. 1) THEN
                  RMXVAL(1,IGRP,IAVE,ITYP) = AVEVAL(IREC,IGRP,IAVE,ITYP)
                           IF (NUMCLM(IAVE).EQ.0 .AND.
     &                         NUMMSG(IAVE).EQ.0) THEN
                              MCLMSG(1,IGRP,IAVE,ITYP) = ' '
                           ELSE
C                             Set Indicator Of Calm and Msg ---   CALL MSETFG
                              CALL MSETFG(1,1)
                           END IF
                           MXDATE(1,IGRP,IAVE,ITYP) = KURDAT
                           MXLOCA(1,IGRP,IAVE,ITYP) = IREC
                        END IF
                     END IF
 100              CONTINUE
               END IF
            ELSE IF (NMXVAL .EQ. 1) THEN
               IF (AVEVAL(IREC,IGRP,IAVE,ITYP) .GT.
     &                RMXVAL(1,IGRP,IAVE,ITYP)) THEN
                  RMXVAL(1,IGRP,IAVE,ITYP) = AVEVAL(IREC,IGRP,IAVE,ITYP)
                  IF (NUMCLM(IAVE).EQ.0 .AND.
     &                NUMMSG(IAVE).EQ.0) THEN
                     MCLMSG(1,IGRP,IAVE,ITYP) = ' '
                  ELSE
C                    Set Indicator Of Calm and Missing      ---   CALL MSETFG
                     CALL MSETFG(1,1)
                  END IF
                  MXDATE(1,IGRP,IAVE,ITYP) = KURDAT
                  MXLOCA(1,IGRP,IAVE,ITYP) = IREC
               END IF
            END IF
 200        CONTINUE
 1000    CONTINUE
C        End Receptor LOOP
 2000 CONTINUE
C     End Source Group LOOP

      RETURN
      END

      SUBROUTINE MSETFG(INDT,J)
C***********************************************************************
C                 MSETFG Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Set Calm and Missing Flag Of the Max Result
C
C        PROGRAMMER: Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Maximum Value Table Options
C                 Array of CONC or DEPOS Averages
C                 Averaging Period
C
C        OUTPUTS: Updated Maximum Value Flag Array
C
C        CALLED FROM:   MAXVAL
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'MSETFG'

      IF (INDT .EQ. 0) THEN
C        Set Indicator Of Calm and Missing
         IF (NUMCLM(IAVE).NE.0 .AND.
     &       NUMMSG(IAVE).EQ.0) THEN
             MCLMSG(J+1,IGRP,IAVE,ITYP) = 'c'
         ELSE IF (NUMCLM(IAVE).EQ.0 .AND.
     &            NUMMSG(IAVE).NE.0) THEN
             MCLMSG(J+1,IGRP,IAVE,ITYP) = 'm'
         ELSE IF (NUMCLM(IAVE).NE.0 .AND.
     &            NUMMSG(IAVE).NE.0) THEN
             MCLMSG(J+1,IGRP,IAVE,ITYP) = 'b'
         END IF
      ELSE IF (INDT .EQ. 1) THEN
C        Set Indicator Of Calm and Missing
         IF (NUMCLM(IAVE).NE.0 .AND.
     &       NUMMSG(IAVE).EQ.0) THEN
             MCLMSG(1,IGRP,IAVE,ITYP) = 'c'
         ELSE IF (NUMCLM(IAVE).EQ.0 .AND.
     &            NUMMSG(IAVE).NE.0) THEN
             MCLMSG(1,IGRP,IAVE,ITYP) = 'm'
         ELSE IF (NUMCLM(IAVE).NE.0 .AND.
     &            NUMMSG(IAVE).NE.0) THEN
             MCLMSG(1,IGRP,IAVE,ITYP) = 'b'
         END IF
      END IF

      RETURN
      END
      SUBROUTINE MAXFIL
C***********************************************************************
C                 MAXFIL Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Update Maximum Value File (>Threshold)
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Maximum File Options
C                 Array of CONC or DEPOS Averages
C                 Averaging Period
C
C        OUTPUTS: Updated Maximum Value File
C
C        CALLED FROM:   HRLOOP
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'MAXFIL'

C     Check for High/Max Value Options - Skip Update If KAVE=1,
C     And No CALCS Were Made for the Current Hour
      IF (CALCS .OR. KAVE(IAVE).NE.1) THEN
C        Begin Source Group LOOP
         DO 1000 IGRP = 1, NUMGRP
C           Check for MAXIFILE Option for This IGRP,IAVE Combination
            IF (MAXFLE(IGRP,IAVE) .EQ. 1) THEN
C              Begin Receptor LOOP
               DO 750 IREC = 1, NUMREC
C                 For the Values Over Threshold
                  IF (AVEVAL(IREC,IGRP,IAVE,1) .GE.
     &                     THRESH(IGRP,IAVE)) THEN
                     WRITE(IMXUNT(IGRP,IAVE),THRFRM,ERR=99) KAVE(IAVE),
     &                  GRPID(IGRP), KURDAT, AXR(IREC), AYR(IREC),
     &                  AZELEV(IREC), AZFLAG(IREC),
     &                  AVEVAL(IREC,IGRP,IAVE,1)
                     IF (RSTSAV) THEN
C                       Saving Intermediate Results to File for Later Re-start
C                       Close MAXIFILE and Reposition to End
                        CLOSE (IMXUNT(IGRP,IAVE))
                        OPEN(IMXUNT(IGRP,IAVE),FILE=THRFIL(IGRP,IAVE))
                        EOF = .FALSE.
                        DO WHILE (.NOT. EOF)
                           READ(IMXUNT(IGRP,IAVE),'(A8)',END=199) DUMMY
                           GO TO 11
 199                       EOF = .TRUE.
 11                        CONTINUE
                        END DO
                        EOF = .FALSE.
                        BACKSPACE IMXUNT(IGRP,IAVE)
                     END IF
                  END IF
  750          CONTINUE
C              End Receptor LOOP
            END IF
 1000    CONTINUE
C        End Source Group LOOP
      END IF

      GO TO 999

C     WRITE Error Message for Problem Writing to Maximum Value File
 99   WRITE(DUMMY,'(5HMAXFL,I3.3)') IMXUNT(IGRP,IAVE)
      CALL ERRHDL(PATH,MODNAM,'E','520',DUMMY)
      RUNERR = .TRUE.

 999  RETURN
      END

      SUBROUTINE POSTFL
C***********************************************************************
C                 POSTFL Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Write Concurrent Values to File for Postprocessing
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Postprocessing File Options
C                 Array of CONC or DEPOS Averages
C
C        OUTPUTS: Postprocessor Files
C
C        CALLED FROM:   HRLOOP
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'POSTFL'

C     Begin Source Group LOOP
      DO 1000 IGRP = 1, NUMGRP
C        Check for POSTFILE Option for This IGRP,IAVE Combination
         IF (IPSTFL(IGRP,IAVE) .EQ. 1) THEN
            IF (IPSFRM(IGRP,IAVE) .EQ. 0) THEN
C              WRITE Results to Unformatted POSTFILE
               WRITE(IPSUNT(IGRP,IAVE),ERR=99) KURDAT, KAVE(IAVE),
     &            GRPID(IGRP), ((AVEVAL(IREC,IGRP,IAVE,ITYP),
     &                           IREC=1,NUMREC),ITYP=1,NUMTYP)
               IF (RSTSAV) THEN
C                 Saving Intermediate Results to File for Later Re-start
C                 Close POSTFILE and Reposition to End
                  CLOSE (IPSUNT(IGRP,IAVE))
                  OPEN(IPSUNT(IGRP,IAVE),FILE=PSTFIL(IGRP,IAVE),
     &                 FORM='UNFORMATTED')
                  EOF = .FALSE.
                  DO WHILE (.NOT. EOF)
                     READ(IPSUNT(IGRP,IAVE),END=199) IDUM
                     GO TO 11
 199                 EOF = .TRUE.
 11                  CONTINUE
                  END DO
                  EOF = .FALSE.
                  BACKSPACE IPSUNT(IGRP,IAVE)
               END IF
c*****   Modified by EMI   2/15/96
c           ELSE
            else if (ipsfrm(igrp,iave) .eq. 1) then
c*****   End of Modification
C              WRITE Results to Formatted Plot File
C              Begin Receptor LOOP
               DO 300 IREC = 1, NUMREC
                  WRITE(IPSUNT(IGRP,IAVE),PSTFRM,ERR=99)
     &               AXR(IREC), AYR(IREC), (AVEVAL(IREC,IGRP,IAVE,ITYP),
     &                                      ITYP=1,NUMTYP),
     &               AZELEV(IREC), CHRAVE(IAVE), GRPID(IGRP), KURDAT,
     &               NETID(IREC)
 300           CONTINUE
C              End Receptor LOOP
               IF (RSTSAV) THEN
C                 Saving Intermediate Results to File for Later Re-start
C                 Close POSTFILE and Reposition to End
                  CLOSE (IPSUNT(IGRP,IAVE))
                  OPEN(IPSUNT(IGRP,IAVE),FILE=PSTFIL(IGRP,IAVE),
     &                 FORM='FORMATTED')
                  EOF = .FALSE.
                  DO WHILE (.NOT. EOF)
                     READ(IPSUNT(IGRP,IAVE),'(A8)',END=299) DUMMY
                     GO TO 21
 299                 EOF = .TRUE.
 21                  CONTINUE
                  END DO
                  EOF = .FALSE.
                  BACKSPACE IPSUNT(IGRP,IAVE)
               END IF
c*****  Modified by EMI   2/15/96
c    Output to compressed file
            else if( ipsfrm( igrp,iave) .eq. 2) then
               call otput( 9999,aveval(1,igrp,iave,1),lcompr,
     &                     ipsunt( igrp,iave))
c*****  End of Modification
            END IF
         END IF
 1000 CONTINUE
C     End Source Group LOOP

      GO TO 999

C     WRITE Error Message for Problem Writing to Postprocessor File
 99   WRITE(DUMMY,'(5HPSTFL,I3.3)') IPSUNT(IGRP,IAVE)
      CALL ERRHDL(PATH,MODNAM,'E','520',DUMMY)
      RUNERR = .TRUE.

 999  RETURN
      END

      SUBROUTINE TOXXFL
C***********************************************************************
C                 TOXXFL Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Update TOXXFILE Buffers, and Write Out if Full
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    September 29, 1992
C
C        INPUTS:  TOXXFILE Options
C                 Array of CONC or DEPOS Averages
C                 Averaging Period
C
C        OUTPUTS: Updated TOXXFILE Buffers and File
C
C        CALLED FROM:   HRLOOP
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'TOXXFL'

C     Check for TOXXFILE Option - Skip Update If KAVE=1,
C     And No CALCS Were Made for the Current Hour
      IF (ITOXFL(IAVE).EQ.1 .AND. (CALCS .OR. KAVE(IAVE).NE.1)) THEN
C        Convert TOXXFILE Threshold to User Units
         CUTOFF = TOXTHR(IAVE) * EMIFAC(1)

C        Begin Receptor LOOP
         DO 1000 IREC = 1, NUMREC

C           Begin Source Group LOOP
            DO 500 IGRP = 1, NUMGRP

C              For the Values Over Threshold (in user units), Fill Buffers
               IF (AVEVAL(IREC,IGRP,IAVE,1) .GE. CUTOFF) THEN
                  DO 300 IG = 1, NUMGRP
C                    Loop Through Groups and Write Values to Buffer
                     IPAIR = IPAIR + 1
                     ICODE = 100000*ILINE + 1000*IG + IREC
                     IDCONC(IAVE,IPAIR) = ICODE
C                    Convert CONC Values Back to Units of g/s
                     TXCONC(IAVE,IPAIR)=AVEVAL(IREC,IG,IAVE,1)/EMIFAC(1)
                     IF (IPAIR .EQ. NPAIR) THEN
C                       Write Out Full Buffers and Reset Counter
                        WRITE(ITXUNT(IAVE)) (IDCONC(IAVE,I),I=1,NPAIR)
                        WRITE(ITXUNT(IAVE)) (TXCONC(IAVE,I),I=1,NPAIR)
                        IPAIR = 0
                     END IF
 300              CONTINUE
C                 Exit Source Group LOOP
                  GO TO 1000
               END IF

  500       CONTINUE
C           End Source Group LOOP

 1000    CONTINUE
C        End Receptor LOOP
      END IF

      GO TO 999

C     WRITE Error Message for Problem Writing to TOXXFILE
 99   WRITE(DUMMY,'(5HTOXFL,I3.3)') ITXUNT(IAVE)
      CALL ERRHDL(PATH,MODNAM,'E','520',DUMMY)
      RUNERR = .TRUE.

 999  RETURN
      END

      SUBROUTINE PRTDAY
C***********************************************************************
C                 PRTDAY Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Write Concurrent Values to Printed Output File
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To adjust format statement 9082 for BOUNDARY receptors
C                    to better accommodate UTM coordinates - 9/29/92
C
C        INPUTS:  Postprocessing File Options
C                 Array of CONC or DEPOS Averages
C
C        OUTPUTS: Postprocessor Files
C
C        CALLED FROM:   HRLOOP
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER BUF132*132

C     Variable Initializations
      MODNAM = 'PRTDAY'

C     Begin Source Group LOOP
      DO 1000 IGRP = 1, NUMGRP

C        Fill Work Array With SRCIDs For This Group
         INDGRP = 0
         DO 210 ISRC = 1, NUMSRC
            IF (IGROUP(ISRC,IGRP) .EQ. 1) THEN
               INDGRP = INDGRP + 1
               WORKID(INDGRP) = SRCID(ISRC)
            END IF
 210     CONTINUE
C        Check for More Than 31 Sources Per Group
         IF (INDGRP .GT. 31) THEN
            WORKID(31) = ' . . . '
            INDGRP = 31
         END IF

C        Print Results for Receptor Networks
C        Set Number of Columns Per Page, NCPP
         NCPP = 9
C        Set Number of Rows Per Page, NRPP
         NRPP = 40
C        Begin LOOP Through Networks
         DO 50 I = 1, INNET
C           Calculate Number of Pages Per X-Group, NPPX, & Per Y-Group, NPPY
            NPPX = 1 + INT((NUMXPT(I)-1)/NCPP)
            NPPY = 1 + INT((NUMYPT(I)-1)/NRPP)
            DO 40 NX = 1, NPPX
               DO 30 NY = 1, NPPY
                  CALL HEADER
                  WRITE(IOUNIT,9032) CHRAVE(IAVE), (CHIDEP(II,ITYP),
     &                                              II=1,6),
     &                  IHOUR,JDAY,GRPID(IGRP),(WORKID(K),K = 1,INDGRP)
                  WRITE(IOUNIT,9037) NTID(I), NTTYP(I)
C                 Print The Value By Groups
                  WRITE(IOUNIT,9011) CHIDEP(3,ITYP), POLLUT,OUTLBL(ITYP)
                  IF (NX .EQ. NPPX) THEN
                     IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                        WRITE(IOUNIT,9016)
                        WRITE(IOUNIT,9017) (XCOORD(J,I),J=1+NCPP*(NX-1),
     &                                                    NUMXPT(I))
                     ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                        WRITE(IOUNIT,9018)
                        WRITE(IOUNIT,9019) (XCOORD(J,I),J=1+NCPP*(NX-1),
     &                                                    NUMXPT(I))
                     END IF
                  ELSE
                     IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                        WRITE(IOUNIT,9016)
                        WRITE(IOUNIT,9017) (XCOORD(J,I),J=1+NCPP*(NX-1),
     &                                                    NCPP*NX)
                     ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                        WRITE(IOUNIT,9018)
                        WRITE(IOUNIT,9019) (XCOORD(J,I),J=1+NCPP*(NX-1),
     &                                                    NCPP*NX)
                     END IF
                  END IF
                  WRITE(IOUNIT,9010)
                  IF (NY .EQ. NPPY) THEN
                     DO 20 K = 1+NRPP*(NY-1), NUMYPT(I)
                        IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                           INDZ = NETEND(I) - K*NUMXPT(I) + 1
                           YCOVAL = YCOORD(NUMYPT(I)-K+1,I)
                        ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                           INDZ = NETSTA(I) + (K-1)*NUMXPT(I)
                           YCOVAL = YCOORD(K,I)
                        END IF
                        IF (NX .EQ. NPPX) THEN
                           WRITE(IOUNIT,9013) YCOVAL,
     &                 (AVEVAL(INDZ+J-1,IGRP,IAVE,ITYP),J=1+NCPP*(NX-1),
     &                                                  NUMXPT(I))
                        ELSE
                           WRITE(IOUNIT,9013) YCOVAL,
     &                 (AVEVAL(INDZ+J-1,IGRP,IAVE,ITYP),J=1+NCPP*(NX-1),
     &                                                  NCPP*NX)
                        END IF
 20                  CONTINUE
                  ELSE
                     DO 25 K = 1+NRPP*(NY-1), NRPP*NY
                        IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                           INDZ = NETEND(I) - K*NUMXPT(I) + 1
                           YCOVAL = YCOORD(NUMYPT(I)-K+1,I)
                        ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                           INDZ = NETSTA(I) + (K-1)*NUMXPT(I)
                           YCOVAL = YCOORD(K,I)
                        END IF
                        IF (NX .EQ. NPPX) THEN
                           WRITE(IOUNIT,9013) YCOVAL,
     &                 (AVEVAL(INDZ+J-1,IGRP,IAVE,ITYP),J=1+NCPP*(NX-1),
     &                                                  NUMXPT(I))
                        ELSE
                           WRITE(IOUNIT,9013) YCOVAL,
     &                 (AVEVAL(INDZ+J-1,IGRP,IAVE,ITYP),J=1+NCPP*(NX-1),
     &                                                  NCPP*NX)
                        END IF
 25                  CONTINUE
                  END IF
 30            CONTINUE
 40         CONTINUE
 50      CONTINUE
C        End LOOP Through Networks

         IF (IRSTAT(4) .NE. 0) THEN
C           Print Out The Coord. & Concentrations For Discrete Cart Receptors
            INDC = 0
            DO 1030 IREC = 1, NUMREC
               IF (RECTYP(IREC) .EQ. 'DC') THEN
                  INDC = INDC + 1
                  IF (MOD(INDC-1,80) .EQ. 0) THEN
                     CALL HEADER
                     WRITE(IOUNIT,9032) CHRAVE(IAVE),(CHIDEP(II,ITYP),
     &             II=1,6),IHOUR,JDAY,GRPID(IGRP),(WORKID(K),K=1,INDGRP)
                     WRITE(IOUNIT,9043)
                     WRITE(IOUNIT,9011) CHIDEP(3,ITYP), POLLUT,
     &                                  OUTLBL(ITYP)
                     WRITE(IOUNIT,9048) CHIDEP(3,ITYP), CHIDEP(3,ITYP)
                  END IF
                  IF (MOD(INDC,2) .NE. 0) THEN
                     WRITE(BUF132(1:60),9045) AXR(IREC),AYR(IREC),
     &                     AVEVAL(IREC,IGRP,IAVE,ITYP)
                  ELSE
                     WRITE(BUF132(61:120),9045) AXR(IREC),
     &                     AYR(IREC), AVEVAL(IREC,IGRP,IAVE,ITYP)
                     WRITE(IOUNIT,9090) BUF132
                     WRITE(BUF132,9095)
                  END IF
               END IF
 1030       CONTINUE
            IF (MOD(INDC,2) .NE. 0) THEN
               WRITE(IOUNIT,9090) BUF132
               WRITE(BUF132,9095)
            END IF
         END IF

         IF (IRSTAT(5) .NE. 0) THEN
C           Print Out The Coord. & Concentrations For Discrete Polar Receptors
            INDC = 0
            DO 1040 IREC = 1, NUMREC
               IF (RECTYP(IREC) .EQ. 'DP') THEN
                  INDC = INDC + 1
                  XRMS = AXR(IREC) - AXS(IREF(IREC))
                  YRMS = AYR(IREC) - AYS(IREF(IREC))
                  DIST = SQRT(XRMS*XRMS + YRMS*YRMS)
                  DIR  = ATAN2(XRMS, YRMS) * RTODEG
                  IF (DIR .LE. 0.0) DIR = DIR + 360.
                  IF (MOD(INDC-1,80) .EQ. 0) THEN
                     CALL HEADER
                     WRITE(IOUNIT,9032) CHRAVE(IAVE), (CHIDEP(II,ITYP),
     &            II=1,6),IHOUR,JDAY, GRPID(IGRP),(WORKID(K),K=1,INDGRP)
                     WRITE(IOUNIT,9044)
                     WRITE(IOUNIT,9011) CHIDEP(3,ITYP), POLLUT,
     &                                  OUTLBL(ITYP)
                     WRITE(IOUNIT,9049) CHIDEP(3,ITYP), CHIDEP(3,ITYP)
                  END IF
                  IF (MOD(INDC,2) .NE. 0) THEN
                     WRITE(BUF132(1:65),9047) SRCID(IREF(IREC)),
     &                          DIST, DIR, AVEVAL(IREC,IGRP,IAVE,ITYP)
                  ELSE
                     WRITE(BUF132(66:130),9047) SRCID(IREF(IREC)),
     &                          DIST, DIR, AVEVAL(IREC,IGRP,IAVE,ITYP)
                     WRITE(IOUNIT,9090) BUF132
                     WRITE(BUF132,9095)
                  END IF
               END IF
 1040       CONTINUE
            IF (MOD(INDC,2) .NE. 0) THEN
               WRITE(IOUNIT,9090) BUF132
               WRITE(BUF132,9095)
            END IF
         END IF

C        Write Out The Boundary Receptors For The Sources
         IF (IRSTAT(6) .NE. 0) THEN
            INDC = 0
            IREC = 1
            DO WHILE (IREC .LE. NUMREC)
               IF (RECTYP(IREC) .EQ. 'BD') THEN
                  INDC = INDC + 1
                  ISRF = IREF(IREC)
                  IF (MOD(INDC-1,3) .EQ. 0) THEN
                     CALL HEADER
                     WRITE(IOUNIT,9032) CHRAVE(IAVE), (CHIDEP(II,ITYP),
     &            II=1,6),IHOUR,JDAY, GRPID(IGRP),(WORKID(K),K=1,INDGRP)
                     WRITE(IOUNIT,9011) CHIDEP(3,ITYP), POLLUT,
     &                                  OUTLBL(ITYP)
                  END IF
                  WRITE(IOUNIT,9082) SRCID(ISRF), SRCTYP(ISRF),
     &      AXS(ISRF),AYS(ISRF),AZS(ISRF),CHIDEP(3,ITYP),CHIDEP(3,ITYP),
     &                CHIDEP(3,ITYP), (J, AXR(IREC+J-1), AYR(IREC+J-1),
     &                AVEVAL(IREC+J-1,IGRP,IAVE,ITYP),J=1,36)
                  IREC = IREC + 36
               ELSE
                  IREC = IREC + 1
               END IF
            END DO
         END IF

 1000 CONTINUE
C     End Source Group LOOP

 9011 FORMAT(/40X,'** ',A4,' OF ',A8,' IN ',A40,' **'/)
 9010 FORMAT(66(' -')/)
 9013 FORMAT(2X,F10.2,1X,'|',1X,9(F13.5))
 9016 FORMAT(3X,' Y-COORD  |',48X,'X-COORD (METERS)')
 9017 FORMAT(3X,' (METERS) |',1X,9(1X,F12.2,:))
 9018 FORMAT(3X,'DIRECTION |',48X,'DISTANCE (METERS)')
 9019 FORMAT(3X,'(DEGREES) |',1X,9(1X,F12.2,:))
 9032 FORMAT(30X,'*** CONCURRENT ',A5,1X,6A4,'VALUES',
     &       ' ENDING WITH HOUR ',I2,' FOR DAY ',I3,' ***'
     &       /34X,'FOR SOURCE GROUP:',1X,A8,
     &       /34X,'INCLUDING SOURCE(S):      ',7(A8,', ',:),
     &       /10X,12(A8,', ',:)/10X,12(A8,', ',:))
 9037 FORMAT(/35X,'*** NETWORK ID: ',A8,' ;  NETWORK TYPE: ',
     &       A8,' ***')
 9043 FORMAT(/45X,'*** DISCRETE CARTESIAN RECEPTOR POINTS ***')
 9044 FORMAT(/47X,'*** DISCRETE POLAR RECEPTOR POINTS ***')
 9045 FORMAT(6X,2(F12.2,2X),F13.5)
 9047 FORMAT(4X,A8,': ',2(F12.2,2X),F13.5)
 9048 FORMAT(6X,' X-COORD (M)   Y-COORD (M)        ',A4,
     &      22X,' X-COORD (M)   Y-COORD (M)        ',A4,/65(' -'))
 9049 FORMAT(5X,'ORIGIN',59X,'ORIGIN',
     &      /5X,' SRCID       DIST (M)     DIR (DEG)        ',A4,
     &      18X,' SRCID       DIST (M)     DIR (DEG)        ',A4,
     &      /65(' -'))
 9082 FORMAT(' BOUNDARY RECEPTOR NETWORK OF SOURCE ID: ',A8,/,5X,
     &       ' OF SOURCE TYPE: ',A8,'; WITH ORIGIN AT (',2(F10.2,', '),
     &       F10.2,')'/3(' (SEC.)  X-COORD    Y-COORD       ',A4,6X),/,
     &       12(3(1X,I4,2X,F9.1,',',F10.1,',',F13.5,' ',2X),/),/)
 9090 FORMAT(A132)
 9095 FORMAT(132(' '))

 999  RETURN
      END

      SUBROUTINE RSDUMP
C***********************************************************************
C                 RSDUMP Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: To Save Intermediate Results Arrays for Later Restart
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Current Date Variable
C                 Array Limits
C                 Results Arrays
C
C        OUTPUTS: Unformatted File of Intermediate Results
C
C        CALLED FROM:   HRLOOP
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'RSDUMP'
      NDUMP = NDUMP + 1

C     Check for Monthly Averages and Only Dump at End of Month
      IF (MONTH .AND. .NOT.ENDMON)  GO TO 1000

      IF (SAVFIL .EQ. SAVFL2 .OR. MOD(NDUMP,2) .NE. 0) THEN
         OPEN(UNIT=IDPUNT,ERR=99,FILE=SAVFIL,FORM='UNFORMATTED',
     &        IOSTAT=IOERRN,STATUS='UNKNOWN')
         WRITE(IDPUNT) KURDAT
         WRITE(IDPUNT) NHIVAL, NMXVAL, NUMREC, NUMGRP, NUMAVE, NUMTYP

         IF (NHIVAL .GT. 0) THEN
           WRITE(IDPUNT) (((((HIVALU(I,J,K,L,M),I=1,NUMREC),J=1,NHIVAL),
     &                       K=1,NUMGRP),L=1,NUMAVE),M=1,NUMTYP)
           WRITE(IDPUNT) (((((NHIDAT(I,J,K,L,M),I=1,NUMREC),J=1,NHIVAL),
     &                       K=1,NUMGRP),L=1,NUMAVE),M=1,NUMTYP)
           WRITE(IDPUNT) (((((HCLMSG(I,J,K,L,M),I=1,NUMREC),J=1,NHIVAL),
     &                       K=1,NUMGRP),L=1,NUMAVE),M=1,NUMTYP)
         END IF

         IF (NMXVAL .GT. 0) THEN
            WRITE(IDPUNT) ((((RMXVAL(I,J,K,L),I=1,NMXVAL),J=1,NUMGRP),
     &                       K=1,NUMAVE),L=1,NUMTYP)
            WRITE(IDPUNT) ((((MXDATE(I,J,K,L),I=1,NMXVAL),J=1,NUMGRP),
     &                       K=1,NUMAVE),L=1,NUMTYP)
            WRITE(IDPUNT) ((((MXLOCA(I,J,K,L),I=1,NMXVAL),J=1,NUMGRP),
     &                       K=1,NUMAVE),L=1,NUMTYP)
            WRITE(IDPUNT) ((((MCLMSG(I,J,K,L),I=1,NMXVAL),J=1,NUMGRP),
     &                       K=1,NUMAVE),L=1,NUMTYP)
         END IF

         IF (PERIOD .OR. ANNUAL) THEN
            WRITE(IDPUNT) IANHRS, IANCLM, IANMSG
            WRITE(IDPUNT) (((ANNVAL(I,J,K),I=1,NUMREC),J=1,NUMGRP),
     &                       K=1,NUMTYP)
            IF (MULTYR) THEN
               WRITE(IDPUNT) (((AMXVAL(I,J,K),I=1,NVAL),J=1,NUMGRP),
     &                          K=1,NUMTYP)
               WRITE(IDPUNT) (((IMXLOC(I,J,K),I=1,NVAL),J=1,NUMGRP),
     &                          K=1,NUMTYP)
            END IF
         END IF

         CLOSE (IDPUNT)

      ELSE
         OPEN(UNIT=IDPUN2,ERR=99,FILE=SAVFL2,FORM='UNFORMATTED',
     &        IOSTAT=IOERRN,STATUS='UNKNOWN')
         WRITE(IDPUN2) KURDAT
         WRITE(IDPUN2) NHIVAL, NMXVAL, NUMREC, NUMGRP, NUMAVE, NUMTYP

         IF (NHIVAL .GT. 0) THEN
           WRITE(IDPUN2) (((((HIVALU(I,J,K,L,M),I=1,NUMREC),J=1,NHIVAL),
     &                       K=1,NUMGRP),L=1,NUMAVE),M=1,NUMTYP)
           WRITE(IDPUN2) (((((NHIDAT(I,J,K,L,M),I=1,NUMREC),J=1,NHIVAL),
     &                       K=1,NUMGRP),L=1,NUMAVE),M=1,NUMTYP)
           WRITE(IDPUN2) (((((HCLMSG(I,J,K,L,M),I=1,NUMREC),J=1,NHIVAL),
     &                       K=1,NUMGRP),L=1,NUMAVE),M=1,NUMTYP)
         END IF

         IF (NMXVAL .GT. 0) THEN
            WRITE(IDPUN2) ((((RMXVAL(I,J,K,L),I=1,NMXVAL),J=1,NUMGRP),
     &                       K=1,NUMAVE),L=1,NUMTYP)
            WRITE(IDPUN2) ((((MXDATE(I,J,K,L),I=1,NMXVAL),J=1,NUMGRP),
     &                       K=1,NUMAVE),L=1,NUMTYP)
            WRITE(IDPUN2) ((((MXLOCA(I,J,K,L),I=1,NMXVAL),J=1,NUMGRP),
     &                       K=1,NUMAVE),L=1,NUMTYP)
            WRITE(IDPUN2) ((((MCLMSG(I,J,K,L),I=1,NMXVAL),J=1,NUMGRP),
     &                       K=1,NUMAVE),L=1,NUMTYP)
         END IF

         IF (PERIOD .OR. ANNUAL) THEN
            WRITE(IDPUN2) IANHRS, IANCLM, IANMSG
            WRITE(IDPUN2) (((ANNVAL(I,J,K),I=1,NUMREC),J=1,NUMGRP),
     &                       K=1,NUMTYP)
            IF (MULTYR) THEN
               WRITE(IDPUN2) (((AMXVAL(I,J,K),I=1,NVAL),J=1,NUMGRP),
     &                          K=1,NUMTYP)
               WRITE(IDPUN2) (((IMXLOC(I,J,K),I=1,NVAL),J=1,NUMGRP),
     &                          K=1,NUMTYP)
            END IF
         END IF

         CLOSE (IDPUN2)

      END IF

      GO TO 1000

 99   CALL ERRHDL(PATH,MODNAM,'E','500','SAVEFILE')

 1000 RETURN
      END
      SUBROUTINE AVERTS(XVIN,YVIN,XWD,YWD,NUMV)
C***********************************************************************
C*                AVERTS Module of ISC2 Short Term Model - ISCST2
C*
C*       PURPOSE: Calculates coordinates of vertices for Wind
C*                Direction Coordinate system for AREA and OPENPIT
C*                sources.
C*
C*       PROGRAMMER: Jeff Wang, Roger Brode
C*       MODIFIED:   Jayant Hardikar, Roger Brode (for OPENPIT sources)
C*
C*       DATE:      July 7, 1993
C*
C*       INPUTS:  Source Coordinates for Specific Source
C*                Number of vertices + 1
C*
C*       OUTPUTS: Array of Vertex Coordinates for Specific Source
C*
C*       CALLED FROM:   ACALC, PITEFF
C***********************************************************************

C*    Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

      REAL XVIN(NVMAX),YVIN(NVMAX)
      REAL XWD(NVMAX),YWD(NVMAX)

C*    Variable Initializations
      MODNAM = 'AVERTS'
      

      DO 1670 NSP = 1, NUMV
         XWD(NSP) = -(XVIN(NSP)*WDSIN + YVIN(NSP)*WDCOS)
         YWD(NSP) =   XVIN(NSP)*WDCOS - YVIN(NSP)*WDSIN
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
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      LOGICAL QGO
      REAL VAL, DVAL

C     Variable Initializations
      MODNAM = 'AREAIN'

C     INITIALIZE VARIABLES FOR INTEGRATION PROCEDURE.
      RP1 = -(XR*WDSIN+YR*WDCOS)/1000.0
      RP2 =  (XR*WDCOS-YR*WDSIN)/1000.0
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

      HRVAL(ITYP) = ABS(VAL)*QTK*EMIFAC(ITYP)/US

      IF (DEBUG) THEN
C        Print Out Debugging Information                    ---   CALL DEBOUT
         CALL DEBOUT
      END IF

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
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      REAL S1(21), H1(21)
C     Variable Initializations
      MODNAM = 'QROMB'

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
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      DIMENSION XA(N1),YA(N1),C1(JMAX1),D1(JMAX1)

C     Variable Initializations
      MODNAM = 'POLINT'

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
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'TRAPZD'

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
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
C     Declare Local Variables
      LOGICAL LTERR

C     Variable Initializations
      MODNAM = 'PLUMEF'

C     Set LTERR to FALSE to signal simple terrain call to DEPCOR.
      LTERR = .FALSE.

C     Scale x1 from kilometers to meters, and place in variable XARG
      XARG = X1*1000.0

C     MODIFIED to NOT compute vn, val for cases with val=1.0, uses LSEG,
C     a logical variable set in PSIDE2, AREAIN to establish case
      IF (LSEG) THEN
C        Calculate dispersion coefficients, SY and SZ       ---   CALL ADIS
         CALL ADIS(XARG,SY,SZ,XY,XZ)
         VAL = 1.0
      ELSE
         CALL XWIDTH(X1,VT)
         CALL PWIDTH(X1,VT,VN,VAL)
      END IF

C     Determine deposition correction factors
      IF (LDPART .OR. LWPART) THEN
C        Loop over particle sizes
         DO 150 I=1,NPD
C           Initialize wet & dry source depletion factors, profile
C           correction factors, and sigma-z settling correction
C           factors to unity.
            WQCOR(I) = 1.
            DQCOR(I) = 1.
            PCORZR(I) = 1.
            PCORZD(I) = 1.
            SZCOR(I) = 1.
            IF (DDPLETE .AND. (STABLE .OR. (HEFLAT.LE.ZI)) ) THEN
C              Determine factors for depletion
C              from dry removal                 ---   CALL DEPCOR
               CALL DEPCOR( VDEP(I),VGRAV(I),ZRDEP,ZFLAG,
     &           XARG,XZ,HE,ZI,US,XS,YS,XR,YR,
     &           RURAL,URBAN,KST,SZ,SBID,
     &           SZMIN(I),ZELEV,ZS,LTERR,DEBUG,IOUNIT,
     &           SRCTYP(ISRC),LTGRID,KURDAT,
     &           DQCOR(I),PCORZR(I),PCORZD(I),SZCOR(I))
            END IF
            IF (WDPLETE) THEN
C              Determine source depletion factor
C              from wet removal
               WQCOR(I) = EXP(-PSCVRT(I)*XARG/US)
            ENDIF
150      CONTINUE
      ENDIF
      IF (LWGAS) THEN
C        Initialize wet source depletion factor to unity.
         WQCORG = 1.
         IF (WDPLETE) THEN
C           Determine source depletion factor
C           from wet removal (GASES)
            WQCORG = EXP(-GSCVRT*XARG/US)
         ENDIF
      ENDIF

C     Get Concentration or Deposition
      CALL ASIMPL(X1,RCZ)

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
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      real u(2), v1(2), vn(2), w(2)

C     Variable Initializations
      MODNAM = 'PSIDE'

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
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'XWIDTH'

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
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
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
      MODNAM = 'PWIDTH'

      IF (X1 .EQ. 0.0) THEN
         SZ = 1.0
         SY = 1.0
         VN = V1*1000.0
         WIDTH = VN
C        Exit Routine
         GO TO 999
      END IF

      XARG = X1*1000.0
C     Calculate dispersion coefficients, SY and SZ          ---   CALL ADIS
      CALL ADIS(XARG,SY,SZ,XY,XZ)

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
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'ZBRENT'

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
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      real ulist(nvmax2), useg(nvmax,2)
      integer usign(nvmax), ufac, usegf(nvmax)
      LOGICAL Ltest1,Ltest2

C     Variable Initializations
      MODNAM = 'PSIDE2'

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
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      DIMENSION UVAR(IDIM)

C     Variable Initializations
      MODNAM = 'HPSORT'

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



C***  Subroutines for OPENPIT Source algorithms - 7/19/94


      SUBROUTINE ESCAPE(ICAT)
C***********************************************************************
C*                ESCAPE Module of ISC2 Short Term Model - ISCST2
C*
C*       PURPOSE: To Calculate Escape Fractions for a Particle
C*                Size Category
C*
C*       PROGRAMMER: Jayant Hardikar, Roger Brode
C*
C*       DATE:    July 20, 1994
C*
C*       INPUTS:  Index for Particle Size Category Being Processed
C*                Gravitational Settling Velocity for Current 
C*                     Particle Size Category & Current Source
C*                10-meter Wind Speed for the Current Hour
C*                Constant ALPHA (= 0.029)
C*                
C*
C*       OUTPUTS: The Escape Fraction for the current Size Category
C*
C*       CALLED FROM:   OCALC
C***********************************************************************

C*    Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
C*    Variable Initializations
      MODNAM = 'ESCAPE'

      EFRAC(ICAT) = 1.0/(1.0 + VGRAV(ICAT) / (ALPHA * UREF10) )
      
      RETURN
      END



      SUBROUTINE ADJEMI(ICAT,QPTOT)
C***********************************************************************
C*                ADJEMI Module of ISC2 Short Term Model - ISCST2
C*
C*       PURPOSE: To Adjust Emission Rate for Current Particle
C*                Size Category Being Processed
C*
C*       PROGRAMMER: Jayant Hardikar, Roger Brode
C*
C*       DATE:    July 20, 1994
C*
C*       INPUTS:  Index for Particle Size Category Being Processed
C*                Escape Fraction for the Current Size Category
C*                Mass Fraction of the Current Size Category
C*                Total Emission Rate Per Unit Area
C*                
C*
C*       OUTPUTS: Adjusted Emission Rate for the Current Size Category
C*                Cumulative Adjusted Emission Rate Over All Categories
C*
C*       CALLED FROM:   OCALC
C***********************************************************************

C*    Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
C*    Variable Initializations
      MODNAM = 'ADJEMI'

      QPART(ICAT) = EFRAC(ICAT) * PHI(ICAT) * QS
      QPTOT = QPTOT + QPART(ICAT)
          
      RETURN
      END


      SUBROUTINE AMFRAC(QPTOT)
C***********************************************************************
C*                AMFRAC Module of ISC2 Short Term Model - ISCST2
C*
C*       PURPOSE: To Adjust the Mass Fractions for each Particle
C*                Size Category
C*
C*       PROGRAMMER: Jayant Hardikar, Roger Brode
C*
C*       DATE:    July 20, 1994
C*
C*       INPUTS:  Array of Adjusted Emission Rates
C*                Cumulative Adjusted Emission Rate Over All Categories
C*
C*       OUTPUTS: Array of Adjusted Mass Fractions
C*                
C*
C*       CALLED FROM:   OCALC
C***********************************************************************

C*    Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
C*    Variable Initializations
      MODNAM = 'AMFRAC'

      DO 20 ICAT = 1,NPD
         PHI(ICAT) = QPART(ICAT)/QPTOT
20    CONTINUE         
          
      RETURN
      END


      SUBROUTINE LWIND
C***********************************************************************
C*                LWIND Module of ISC2 Short Term Model - ISCST2
C*
C*       PURPOSE: To Calculate the Along-Wind Length of the OPENPIT
C*
C*       PROGRAMMER: Jayant Hardikar, Roger Brode
C*
C*       DATE:    July 20, 1994
C*
C*       INPUTS:  Wind Flow Vector for the Current Hour
C*                Angle of the Long OPENPIT dimension from the North
C*                Length of the OPENPIT
C*                Width of the OPENPIT
C*
C*       OUTPUTS: Along-Wind Length of the OPENPIT
C*                
C*
C*       CALLED FROM:   OCALC
C***********************************************************************

C*    Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
C*    Variable Initializations
      MODNAM = 'LWIND'
      

C*    Determine the Wind Direction Angle Relative to the Long
C*    Axis of the OpenPit
      CALL CTHETA(AFV,PALPHA,THETA)
      
C*    Determine the Along-Wind Length of the OPENPIT
      PITL = PITLEN * (1 - THETA/90.) + PITWID * (THETA / 90.)
      
      RETURN
      END


      SUBROUTINE PDEPTH
C***********************************************************************
C*                PDEPTH Module of ISC2 Short Term Model - ISCST2
C*
C*       PURPOSE: To Calculate the Relative Depth of the OPENPIT Source
C*
C*       PROGRAMMER: Jayant Hardikar, Roger Brode
C*
C*       DATE:    July 20, 1994
C*
C*       INPUTS:  Effective Depth of the OPENPIT
C*                Release Height Above 
C*                Along Wind Length of the OPENPIT
C*                
C*       OUTPUTS: Relative Depth of the OPENPIT
C*                
C*       CALLED FROM:   OCALC
C***********************************************************************

C*    Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
C*    Variable Initializations
      MODNAM = 'PDEPTH'

      PDREL = (PDEFF-EMIHGT) / PITL

      RETURN
      END
      
            

      SUBROUTINE PTFRAC 
C***********************************************************************
C*                PTFRAC Module of ISC2 Short Term Model - ISCST2
C*
C*       PURPOSE: To Calculate the Fractional Size of the Effective
C*                Area of the OPENPIT Source
C*
C*       PROGRAMMER: Jayant Hardikar, Roger Brode
C*
C*       DATE:    July 20, 1994
C*
C*       INPUTS:  Relative Pit Depth
C*
C*       OUTPUTS: Fractional Size of the Effective Area of the OPENPIT
C*                
C*
C*       CALLED FROM:   OCALC
C***********************************************************************

C*    Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
C*    Variable Initializations
      MODNAM = 'PTFRAC'
      
      IF (PDREL .GE. 0.2) THEN
         PITFRA = 0.08
      ELSE
         PITFRA = SQRT (1.0 - 1.7*(PDREL**(0.333333)) )
      ENDIF
      
      RETURN
      END
      

      SUBROUTINE PITEFF
C***********************************************************************
C*                PITEFF Module of ISC2 Short Term Model - ISCST2
C*
C*       PURPOSE: To Determine the Coordinates of the OPENPIT Source
C*                in Wind Direction Coordinate System
C*
C*       PROGRAMMER: Jayant Hardikar, Roger Brode
C*
C*       DATE:    July 20, 1994
C*
C*       INPUTS:  
C*
C*       OUTPUTS: Coordinates of the OPENPIT Source in Wind 
C*                Direction Coordinate System
C*
C*       CALLED FROM:   OCALC
C***********************************************************************

C*    Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      
      REAL XTEMP(NVMAX),YTEMP(NVMAX), XSPA(NVMAX), YSPA(NVMAX)
      REAL EPSLON
      
C*    Variable Initializations
      MODNAM = 'PITEFF'
      EPSLON = 0.00001
      
C*    Get Vertices of Actual Pit in WD-Coordinate System    ---   CALL AVERTS
      
      CALL AVERTS(XVERT,YVERT,XSPA,YSPA,NVERT+1)
              
C*    Find the Upwind Vertex of the Pit (one with minimum X)
      SPAMIN = 1.0E+20
      IUPWND = 0
      DO 20 IVERT = 1,NVERT
         IF (XSPA(IVERT) .LT. SPAMIN) THEN
            IUPWND = IVERT
            SPAMIN = XSPA(IVERT)-EPSLON
         ENDIF
20    CONTINUE         

C*    If DEBUG Requested, Write Out Pit Info            
      IF (DEBUG) THEN
         WRITE (IOUNIT,*) 'ACTUAL PIT COORDINATES:'
         WRITE (IOUNIT,*) '----------------'
         WRITE (IOUNIT,*) 'SYSTEM   X1       Y1       X2       Y2',
     &                    '       X3       Y3       X4       Y4'
         WRITE (IOUNIT,*) '-------- -------- -------- -------- ',
     &                    '-------- -------- -------- -------- ',
     &                    '--------'
         WRITE (IOUNIT,8000) (XVERT(II),YVERT(II),II=1,NVERT)
         WRITE (IOUNIT,8100) (XSPA (II),YSPA (II),II=1,NVERT)         
         WRITE (IOUNIT,*)
         WRITE (IOUNIT,*) ' UPWIND VERTEX OF THE PIT= ', IUPWND
         WRITE (IOUNIT,*) ' WIND DIRECTION W.R.T. PIT LONG AXIS= ',THETA
         WRITE (IOUNIT,*) ' ALONGWIND LENGTH OF THE PIT= ',PITL
         WRITE (IOUNIT,*) ' RELATIVE DEPTH OF THE PIT= ',PDREL
         WRITE (IOUNIT,*)
8000     FORMAT (1X,'User     ',8(f8.3,1x))
8100     FORMAT (1X,'Wind-Dir ',8(f8.3,1x))
      ENDIF
      
C*    Determine the Angle of the Effective Pit Relative
C*    to North
      EFFANG = ANGLE + (90.*(IUPWND - 1))
      
C*    Determine Length and Width Dimensions of the
C*    Effective Pit Area
      EFFWID = PITFRA**(1.0 - (COS(THETA*DTORAD))**2)*PITWID
      EFFLEN = PITFRA**((COS(THETA*DTORAD))**2)*PITLEN

C*    Calculate the Coordinates of the Vertices of the 
C*    Effective Pit Area
C*    Set Coordinates of Vertices for Rectangular Area (in Kilometers).
C*    Vertices Start with the "Southwest" Corner and Are Defined
C*    Clockwise.  The First Vertex is Repeated as the Last Vertex.


C*    First determine proper 'x-dim' and 'y-dim' for effective area,
C*    taking into account angle of orientation and relation to actual pit.

      IF (XINIT .LE. YINIT .AND. (IUPWND.EQ.1 .OR. IUPWND.EQ.3)) THEN
         XEFF = EFFWID
         YEFF = EFFLEN
      ELSE IF (XINIT.LE.YINIT .AND. (IUPWND.EQ.2 .OR. IUPWND.EQ.4)) THEN
         XEFF = EFFLEN
         YEFF = EFFWID
      ELSE IF (XINIT.GT.YINIT .AND. (IUPWND.EQ.1 .OR. IUPWND.EQ.3)) THEN
         XEFF = EFFLEN
         YEFF = EFFWID
      ELSE IF (XINIT.GT.YINIT .AND. (IUPWND.EQ.2 .OR. IUPWND.EQ.4)) THEN
         XEFF = EFFWID
         YEFF = EFFLEN
      END IF

      XTEMP(1) = XVERT(IUPWND)
      YTEMP(1) = YVERT(IUPWND)

      XTEMP(2) = XTEMP(1) +
     &                (YEFF*SIN(EFFANG*DTORAD)) / 1000.
      YTEMP(2) = YTEMP(1) +
     &                (YEFF*COS(EFFANG*DTORAD)) / 1000.

      XTEMP(3) = XTEMP(2) +
     &                (XEFF*COS(EFFANG*DTORAD)) / 1000.
      YTEMP(3) = YTEMP(2) -
     &                (XEFF*SIN(EFFANG*DTORAD)) / 1000.

      XTEMP(4) = XTEMP(3) -
     &                (YEFF*SIN(EFFANG*DTORAD)) / 1000.
      YTEMP(4) = YTEMP(3) -
     &                (YEFF*COS(EFFANG*DTORAD)) / 1000.

      XTEMP(5) = XVERT(IUPWND)
      YTEMP(5) = YVERT(IUPWND)


C*    Calculate Coordinates of the Effective Pit Area in
C*    Wind Direction Coordinate System                      ---   CALL AVERTS
      CALL AVERTS(XTEMP,YTEMP,XSPA,YSPA,NVERT+1)

C*    If DEBUG Requested, Write Out Pit Info            
      IF (DEBUG) THEN
         WRITE (IOUNIT,*) 'EFFECTIVE PIT COORDINATES:'
         WRITE (IOUNIT,*) '----------------'
         WRITE (IOUNIT,*) 'SYSTEM   X1       Y1       X2       Y2',
     &                    '       X3       Y3       X4       Y4'
         WRITE (IOUNIT,*) '-------- -------- -------- -------- ',
     &                    '-------- -------- -------- -------- ',
     &                    '--------'
       WRITE (IOUNIT,8000) (XTEMP(II),YTEMP(II),II=1,NVERT)
         WRITE (IOUNIT,8100) (XSPA (II),YSPA (II),II=1,NVERT)         
         WRITE (IOUNIT,*)
         WRITE (IOUNIT,*) 'EFFECTIVE PIT LENGTH = ', EFFLEN
         WRITE (IOUNIT,*) 'EFFECTIVE PIT WIDTH  = ', EFFWID
         WRITE (IOUNIT,*) 'EFFECTIVE PIT ORIENTATION RELATIVE',
     &                    ' TO NORTH= ', EFFANG
         WRITE (IOUNIT,*) 'FRACTIONAL SIZE OF THE EFFECTIVE PIT AREA= ',
     &                     PITFRA
      ENDIF
      
C*    Store Coordinates of the Effective Area in COMMON Variables
      DO 40 IVERT = 1,NVERT+1
         SPA(IVERT,1) = XSPA(IVERT)
         SPA(IVERT,2) = YSPA(IVERT)
40    CONTINUE
         
         
      RETURN
      END


      SUBROUTINE PITEMI(QPTOT)
C***********************************************************************
C*                PITEMI Module of ISC2 Short Term Model - ISCST2
C*
C*       PURPOSE: To Determine the Emission Rate for the Effective 
C*                Pit Area
C*
C*       PROGRAMMER: Jayant Hardikar, Roger Brode
C*
C*       DATE:    July 20, 1994
C*
C*       INPUTS:  Fractional Area of the Pit
C*                Total Adjusted Emission Rate
C*
C*       OUTPUTS: Emission Rate for the Effective Area of the Current
C*                OPENPIT Source
C*                
C*
C*       CALLED FROM:   OCALC
C***********************************************************************

C*    Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
C*    Variable Initializations
      MODNAM = 'PITEMI'
      
      QEFF = QPTOT / PITFRA
      
      RETURN
      END


      SUBROUTINE CTHETA(AFVIN,ALFIN,THOUT)
C***********************************************************************
C*                CTHETA Module of ISC2 Short Term Model - ISCST2
C*
C*       PURPOSE: To Determine the Wind Direction Angle Relative to 
C*                the Pit Long Axis
C*
C*       PROGRAMMER: Jayant Hardikar, Roger Brode
C*
C*       DATE:    July 26, 1994
C*
C*       INPUTS:  Flow Vector 
C*                Angle of Pit Long Axis from North
C*
C*       OUTPUTS: THETA - Wind Direction Angle Relative to 
C*                the Pit Long Axis
C*                
C*
C*       CALLED FROM:   LWIND
C***********************************************************************

      if (abs(AFVIN-ALFIN) .le. 90.) then
         THOUT = abs(AFVIN-ALFIN)
      else if (AFVIN .gt. ALFIN) then
         theta = AFVIN - ALFIN
         if (theta .gt. 90.) then
            theta = AFVIN-180. - ALFIN
         endif
         if (theta .gt. 90.) then
            theta = AFVIN-360. - ALFIN
         endif
         if (theta .gt. 90.) then
            theta = AFVIN-540. - ALFIN
         endif
         THOUT = abs(theta)
      else if (AFVIN .lt. ALFIN) then
         theta = AFVIN - ALFIN
         if (theta .lt. -90.) then
            theta = AFVIN + 180. - ALFIN
         endif
         if (theta .lt. -90.) then
            theta = AFVIN + 360. - ALFIN
         endif
         if (theta .lt. -90.) then
            theta = AFVIN + 540. - ALFIN
         endif
         THOUT = abs(theta)
      endif
      RETURN
      end
      
      SUBROUTINE SIGY(XARG,SYOUT)
C***********************************************************************
C                 SIGY Module of ISC2 Model
C
C        PURPOSE: Calculates Sigma-y Values From Dispersion Curves
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
C
C        OUTPUTS: Lateral Dispersion Coefficient, SYOUT
C
C        CALLED FROM:   PDIS
C                       VDIS
C                       ADIS
C                       SYENH
C                       DHPSS
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'SIGY'

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
         SYOUT = 465.11628 * XKM * TAN(TH)
      ELSE IF (URBAN) THEN
         IF (KST .EQ. 4) THEN
            SYOUT = 160.*XKM/SQRT(1.+0.4*XKM)
         ELSE IF (KST .GE. 5) THEN
            SYOUT = 110.*XKM/SQRT(1.+0.4*XKM)
         ELSE IF (KST .LE. 2) THEN
            SYOUT = 320.*XKM/SQRT(1.+0.4*XKM)
         ELSE IF (KST .EQ. 3) THEN
            SYOUT = 220.*XKM/SQRT(1.+0.4*XKM)
         END IF
      END IF

      RETURN
      END

      SUBROUTINE SIGZ(XARG,SZOUT)
C***********************************************************************
C                 SIGZ Module of ISC2 Model
C
C        PURPOSE: Calculates Sigma-z Values From Dispersion Curves
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
C
C        OUTPUTS: Vertical Dispersion Coefficient, SZOUT
C
C        CALLED FROM:   PDIS
C                       VDIS
C                       ADIS
C                       SZENH
C                       DHPSS
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'SIGZ'

C     Convert Distance to km
      XKM = XARG * 0.001

C     Determine Sigma-z Based on RURAL/URBAN, Stability Class, and Distance.
C     Stability Classes are Checked in the Order 4, 5, 6, 1, 2, 3
C     For Optimization, Since Neutral and Stable are Generally the Most
C     Frequent Classes.

      IF (RURAL) THEN
C        Retrieve Coefficients, A and B                     ---   CALL SZCOEF
         CALL SZCOEF(XKM,A,B,XMIN,XMAX)
         SZOUT = A*XKM**B
      ELSE IF (URBAN) THEN
         IF (KST .EQ. 4) THEN
            SZOUT = 140.*XKM/SQRT(1.+0.3*XKM)
         ELSE IF (KST .GE. 5) THEN
            SZOUT = 80.*XKM/SQRT(1.+1.5*XKM)
         ELSE IF (KST .LE. 2) THEN
            SZOUT = 240.*XKM*SQRT(1.+XKM)
         ELSE IF (KST .EQ. 3) THEN
            SZOUT = 200.*XKM
         END IF
      END IF

      RETURN
      END

      SUBROUTINE SZCOEF(XKM,A,B,XMIN,XMAX)
C***********************************************************************
C                 SZCOEF Module of ISC2 Model
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
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'SZCOEF'

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


      SUBROUTINE SZDCAY
C***********************************************************************
C                 SZDCAY Module of ISC2 Model
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
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'SZDCAY'

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

      SUBROUTINE BID(DHPARG,SYARG,SZARG,SYOUT,SZOUT,SBOUT)
C***********************************************************************
C                 BID Module of ISC2 Model
C
C        PURPOSE: Applies Bouyancy-Induced Dispersion to
C                 Sigma-y and Sigma-z
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        MODIFIED:   To use calling arguments
C                    R. W. Brode, PES, Inc. - 9/30/94
C
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
C        OUTPUTS: Sigma-y and Sigma-z Adjusted for BID (SYOUT and SZOUT)
C
C        CALLED FROM:   PDIS
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'BID'

C     Calculate The Coefficients
      SBOUT = DHPARG/3.5
      SBIDSQ = SBOUT*SBOUT
C     Apply BID to Sigma-y and Sigma-z
      SYOUT = SQRT(SYARG*SYARG + SBIDSQ)
      SZOUT = SQRT(SZARG*SZARG + SBIDSQ)

      RETURN
      END
      SUBROUTINE COCARD
C***********************************************************************
C                 COCARD Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: To process COntrol Pathway card images
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To add error check for use of NOSMPL option with
C                    FLAT terrain.
C                    R. W. Brode, PES - 9/30/94
C
C        MODIFIED:   To add DDEP and WDEP parameters to CONC/DEPOS options
C                    to allow just the wet or just the dry deposition flux
C                    to be reported.  DEPOS now reports the sum of wet and
C                    dry fluxes.  Also, a new option parameter is provided
C                    to force the Intermeiate Terrain procedure to ignore
C                    either the simple terrain model or the complex terrain
C                    model (NOSMPL/NOCMPL).
C                    D. Strimaitis, SRC - 11/8/93
C
C        MODIFIED:   To add DEPLETE parameter for plume depletion option
C                    and to allow flagpole receptors with DEPOS option.
C                    D. Strimaitis, SRC - 2/15/93
C
C        INPUTS:  Pathway (CO) and Keyword
C
C        OUTPUTS: Processing Option Switches
C                 Option Setup Status Switches
C
C        CALLED FROM:   SETUP
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'COCARD'

      IF (KEYWRD .EQ. 'STARTING') THEN
C        Set Status Switch
         ISTART = .TRUE.
         ICSTAT(1) = ICSTAT(1) + 1
         IF (ICSTAT(1) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'W','135',KEYWRD)
         END IF
      ELSE IF (KEYWRD .EQ. 'TITLEONE') THEN
C        Set Status Switch
         ICSTAT(2) = ICSTAT(2) + 1
         IF (ICSTAT(2) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'W','135',KEYWRD)
         ELSE
C           Process Titles                                  ---   CALL TITLES
            CALL TITLES
         END IF
      ELSE IF (KEYWRD .EQ. 'TITLETWO') THEN
C        Set Status Switch
         ICSTAT(3) = ICSTAT(3) + 1
         IF (ICSTAT(3) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'W','135',KEYWRD)
         ELSE
C           Process Titles                                  ---   CALL TITLES
            CALL TITLES
         END IF
      ELSE IF (KEYWRD .EQ. 'MODELOPT') THEN
C        Set Status Switch
         ICSTAT(4) = ICSTAT(4) + 1
         IF (ICSTAT(4) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Modeling Options                        ---   CALL MODOPT
            CALL MODOPT
         END IF
      ELSE IF (KEYWRD .EQ. 'AVERTIME') THEN
C        Set Status Switch
         ICSTAT(5) = ICSTAT(5) + 1
         IF (ICSTAT(5) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Averaging Time Options                  ---   CALL AVETIM
            CALL AVETIM
         END IF
      ELSE IF (KEYWRD .EQ. 'POLLUTID') THEN
C        Set Status Switch
         ICSTAT(6) = ICSTAT(6) + 1
         IF (ICSTAT(6) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE IF (ICSTAT(4) .NE. 1) THEN
C           WRITE Error Message: Keyword Out of Order (Must Follow MODELOPT)
            CALL ERRHDL(PATH,MODNAM,'E','140',KEYWRD)
         ELSE
C           Process Pollutant ID Option                     ---   CALL POLLID
            CALL POLLID
         END IF
      ELSE IF (KEYWRD .EQ. 'HALFLIFE' .OR.
     &         KEYWRD .EQ. 'DCAYCOEF') THEN
         IF (KEYWRD .EQ. 'HALFLIFE') THEN
C           Check for Previous DCAYCOEF Keyword in Runstream File
            IF (ICSTAT(8) .NE. 0) THEN
               CALL ERRHDL(PATH,MODNAM,'W','155',KEYWRD)
               GO TO 999
            ELSE
C              Set Status Switch and Check for Duplicate Keyword
               ICSTAT(7) = ICSTAT(7) + 1
               IF (ICSTAT(7) .NE. 1) THEN
C                 WRITE Error Message: Repeat Non-repeatable Keyword
                  CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
                  GO TO 999
               END IF
            END IF
         ELSE IF (KEYWRD .EQ. 'DCAYCOEF') THEN
C           Check for Previous HALFLIFE Keyword in Runstream File
            IF (ICSTAT(7) .NE. 0) THEN
               CALL ERRHDL(PATH,MODNAM,'W','155',KEYWRD)
               GO TO 999
            ELSE
C              Set Status Switch and Check for Duplicate Keyword
               ICSTAT(8) = ICSTAT(8) + 1
               IF (ICSTAT(8) .NE. 1) THEN
C                 WRITE Error Message: Repeat Non-repeatable Keyword
                  CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
                  GO TO 999
               END IF
            END IF
         END IF
C        Check for Keyword Out of Order
         IF (ICSTAT(4) .NE. 1) THEN
C           WRITE Error Message: Keyword Out of Order (Must Follow MODELOPT)
            CALL ERRHDL(PATH,MODNAM,'E','140',KEYWRD)
         ELSE IF (ICSTAT(6) .NE. 1) THEN
C           WRITE Error Message: Keyword Out of Order (Must Follow POLLUTID)
            CALL ERRHDL(PATH,MODNAM,'E','140',KEYWRD)
         END IF
C        Process Exponential Decay Option                   ---   CALL EDECAY
         CALL EDECAY
      ELSE IF (KEYWRD .EQ. 'TERRHGTS') THEN
C        Set Status Switch
         ICSTAT(9) = ICSTAT(9) + 1
         IF (ICSTAT(9) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Terrain Heights Option                  ---   CALL TERRHT
            CALL TERRHT
         END IF
      ELSE IF (KEYWRD .EQ. 'ELEVUNIT') THEN
C        Set Status Switch
         ICSTAT(10) = ICSTAT(10) + 1
         IF (ICSTAT(10) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Elevation Units Option                  ---   CALL ELUNIT
            CALL ELUNIT
C           WRITE Warning Message: CO ELEVUNIT is obsolete, should place
C           ELEVUNIT on SO, RE and/or TG pathway
            CALL ERRHDL(PATH,MODNAM,'W','151','instead ')
         END IF
      ELSE IF (KEYWRD .EQ. 'FLAGPOLE') THEN
C        Set Status Switch
         ICSTAT(11) = ICSTAT(11) + 1
         IF (ICSTAT(11) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Flagpole Receptor Height Option         ---   CALL FLAGDF
            CALL FLAGDF
         END IF
      ELSE IF (KEYWRD .EQ. 'RUNORNOT') THEN
C        Set Status Switch
         ICSTAT(12) = ICSTAT(12) + 1
         IF (ICSTAT(12) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Option to Run Model or Not              ---   CALL RUNNOT
            CALL RUNNOT
         END IF
      ELSE IF (KEYWRD .EQ. 'EVENTFIL') THEN
C        Set Status Switch
         ICSTAT(13) = ICSTAT(13) + 1
         IF (ICSTAT(13) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process EVENT File Option                       ---   CALL EVNTFL
            CALL EVNTFL
         END IF
      ELSE IF (KEYWRD .EQ. 'SAVEFILE') THEN
C        Set Status Switch
         ICSTAT(14) = ICSTAT(14) + 1
         IF (ICSTAT(14) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Model Re-start Save File Option         ---   CALL SAVEFL
            CALL SAVEFL
         END IF
      ELSE IF (KEYWRD .EQ. 'INITFILE') THEN
C        Set Status Switch
         ICSTAT(15) = ICSTAT(15) + 1
         IF (ICSTAT(15) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Re-start Initialization File Option     ---   CALL INITFL
            CALL INITFL
         END IF
      ELSE IF (KEYWRD .EQ. 'MULTYEAR') THEN
C        Set Status Switch
         ICSTAT(16) = ICSTAT(16) + 1
         IF (ICSTAT(16) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Multiple-Year Run Option                ---   CALL MYEAR
            CALL MYEAR
         END IF
      ELSE IF (KEYWRD .EQ. 'ERRORFIL') THEN
C        Set Status Switch
         ICSTAT(17) = ICSTAT(17) + 1
         IF (ICSTAT(17) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Error File Option                       ---   CALL ERRFIL
            CALL ERRFIL
         END IF
      ELSE IF (KEYWRD .EQ. 'FINISHED') THEN
C        Set Status Switch
         IFINIS = .TRUE.
C        Set Status Switch
         ICSTAT(20) = ICSTAT(20) + 1
         IF (ICSTAT(20) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
            GO TO 999
         END IF

C        Check for Missing Mandatory Keywords
         IF (ICSTAT(1) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','STARTING')
         END IF
         IF (ICSTAT(2) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','TITLEONE')
         END IF
         IF (ICSTAT(4) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','MODELOPT')
         END IF
         IF (ICSTAT(5) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','AVERTIME')
         END IF
         IF (ICSTAT(6) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','POLLUTID')
         END IF
         IF (ICSTAT(12) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','RUNORNOT')
         END IF

C        Check for Conflicting Options
         IF (NOSMPL .AND. FLAT) THEN
C           WRITE Error Message for Use of NOSMPL with FLAT terrain
            CALL ERRHDL(PATH,MODNAM,'E','144','       ')
         END IF

C *** Elevated terrain option is allowed in ISCOMDEP -- remove check
C        IF (DEPOS .AND. ELEV) THEN
C           WRITE Warning Message for Use of ELEV Terrain with DEPOS,
C           Reset to FLAT Terrain
C           CALL ERRHDL(PATH,MODNAM,'W','213',' DEPOS ')
c           ELEV = .FALSE.
C           FLAT = .TRUE.
C        END IF
C *** Flagpole option is OK with new deposition algorithm --remove check
C       IF (DEPOS .AND. FLGPOL) THEN
C           WRITE Warning Message for Use of FLGPOL Option with DEPOS,
C           Reset FLGPOL Option to FALSE
C           CALL ERRHDL(PATH,MODNAM,'W','215',' DEPOS ')
C           FLGPOL = .FALSE.
C        END IF

C        OPEN Restart Save and Initialization Files
         IF (RSTSAV) THEN
            DUMMY = 'SAVEFILE'
            OPEN(UNIT=IDPUNT,ERR=99,FILE=SAVFIL,FORM='UNFORMATTED',
     &           IOSTAT=IOERRN,STATUS='UNKNOWN')
            IF (SAVFL2 .NE. SAVFIL) THEN
               OPEN(UNIT=IDPUN2,ERR=99,FILE=SAVFL2,FORM='UNFORMATTED',
     &              IOSTAT=IOERRN,STATUS='UNKNOWN')
            END IF
         END IF
         IF (RSTINP) THEN
            DUMMY = 'INITFILE'
            OPEN(UNIT=IRSUNT,ERR=99,FILE=INIFIL,FORM='UNFORMATTED',
     &           IOSTAT=IOERRN,STATUS='OLD')
         END IF

C        Generate MODOPS Character Array to Summarize Modeling Options
         IF (CONC) THEN
            MODOPS(1) = 'CONC '
         END IF
         IF (DEPOS) THEN
            MODOPS(2) = 'DEPOS'
         END IF
         IF (DDEP) THEN
            MODOPS(3) = 'DDEP'
         END IF
         IF (WDEP) THEN
            MODOPS(4) = 'WDEP'
         END IF
         IF (RURAL) THEN
            MODOPS(5) = 'RURAL'
         ELSE
            MODOPS(5) = 'URBAN'
         END IF
         IF (FLAT) THEN
            MODOPS(6) = 'FLAT'
         ELSE
            MODOPS(6) = 'ELEV'
         END IF
         IF (FLGPOL) MODOPS(7) = 'FLGPOL'
         IF (DFAULT) MODOPS(8) = 'DFAULT'
         IF (GRDRIS) MODOPS(9) = 'GRDRIS'
         IF (NOSTD)  MODOPS(10) = 'NOSTD'
         IF (NOBID)  MODOPS(11) = 'NOBID'
         IF (NOCALM) MODOPS(12) = 'NOCALM'
         IF (MSGPRO) MODOPS(13) = 'MSGPRO'
         IF (MULTYR) MODOPS(14) = 'MULTYR'
         IF (DDPLETE) MODOPS(15) = 'DRYDPL'
         IF (WDPLETE) MODOPS(16) = 'WETDPL'
         IF (NOSMPL) MODOPS(17) = 'NOSMPL'
         IF (NOCMPL) MODOPS(17) = 'NOCMPL'

         GO TO 1000

C        WRITE Error Message for Error Opening File
 99      CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)
         IF (DUMMY .EQ. 'SAVEFILE') THEN
C           Reset Logical Flag for SAVEFILE Option Due to Error Opening File
            RSTSAV = .FALSE.
         ELSE IF (DUMMY .EQ. 'INITFILE') THEN
C           Reset Logical Flag for INITFILE Option Due to Error Opening File
            RSTINP = .FALSE.
         END IF

 1000    CONTINUE

      ELSE
C        Write Error Message: Invalid Keyword for This Pathway
         CALL ERRHDL(PATH,MODNAM,'E','110',KEYWRD)
      END IF

 999  RETURN
      END

      SUBROUTINE TITLES
C***********************************************************************
C                 TITLES Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process Title Information From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Title Strings for Model Outputs
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'TITLES'

      IF (KEYWRD .EQ. 'TITLEONE') THEN
         TITLE1 = RUNST1(LOCE(2)+2:80)
         IF (RUNST1(LOCE(2)+2:80) .EQ. ' ') THEN
C           Write Error Message: Missing Parameter Title
            CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         END IF
      ELSE IF (KEYWRD .EQ. 'TITLETWO') THEN
         TITLE2 = RUNST1(LOCE(2)+2:80)
         IF (RUNST1(LOCE(2)+2:80) .EQ. ' ') THEN
C           Write Warning Message
            CALL ERRHDL(PATH,MODNAM,'W','200',KEYWRD)
         END IF
      END IF

 999  RETURN
      END

      SUBROUTINE MODOPT
C***********************************************************************
C                 MODOPT Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process Modeling Options From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To allow for calculating CONC/DEPOS/DDEP/WDEP in
C                    a single model run.
C                    R. W. Brode, PES - 4/17/95
C
C        MODIFIED:   To add DDEP and WDEP parameters to CONC/DEPOS options
C                    to allow just the wet or just the dry deposition flux
C                    to be reported.  DEPOS now reports the sum of wet and
C                    dry fluxes.  Also, a new option parameter is provided
C                    to force the Intermeiate Terrain procedure to ignore
C                    either the simple terrain model or the complex terrain
C                    model (NOSMPL/NOCMPL).
C                    D. Strimaitis, SRC - 11/8/93
C
C        MODIFIED:   To add DEPLETE parameter for plume depletion option
C                    D. Strimaitis, SRC - 2/15/93
C
C        MODIFIED:   To Output Warning Message '206' For Overriding
C                    Non-DEFAULT Option - 9/29/92
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Modeling Option Logical Switch Settings
C
C        ERROR HANDLING:   Checks for Too Few or Too Many Option Keywords;
C                          Checks for Invalid Option Keywords;
C                          Checks for Conflicting or Missing Option Keywords
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      CHARACTER KOPT*8

C     Variable Initializations - Initialize All Logical Switches to FALSE
      MODNAM = 'MODOPT'

C     Check for Too Few or Too Many Parameters
      IF (IFC .LT. 3) THEN
C        WRITE Error Message     ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      ELSE IF (IFC .LT. 4) THEN
C        WRITE Warning Message   ! Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'W','201',KEYWRD)
      ELSE IF (IFC .GT. 11) THEN
C        WRITE Warning Message   ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'W','202',KEYWRD)
      END IF

C     First Check for Presence of DFAULT Switch
      DO 10 I = 3, IFC
         KOPT = FIELD(I)
         IF (KOPT .EQ. 'DFAULT' .OR. KOPT .EQ. 'DEFAULT') THEN
            DFAULT = .TRUE.
         END IF
 10   CONTINUE

      NUMTYP = 0
C     Loop Through Fields Again Setting All Swithes
      DO 20 I = 3, IFC
         KOPT = FIELD(I)
         IF (KOPT .EQ. 'DFAULT') THEN
            DFAULT = .TRUE.
         ELSE IF (KOPT .EQ. 'CONC') THEN
            IF (.NOT. CONC) THEN
               CONC   = .TRUE.
               NUMTYP = NUMTYP + 1
            END IF
c           EMIFAC = 1.0E06
c           EMILBL = 'GRAMS/SEC'
c           OUTLBL = 'MICROGRAMS/M**3'
c           CHIDEP(1) = 'AVER'
c           CHIDEP(2) = 'AGE '
c           CHIDEP(3) = 'CONC'
c           CHIDEP(4) = 'ENTR'
c           CHIDEP(5) = 'ATIO'
c           CHIDEP(6) = 'N   '
         ELSE IF (KOPT .EQ. 'DEPOS') THEN
            IF (.NOT. DEPOS) THEN
               DEPOS  = .TRUE.
               NUMTYP = NUMTYP + 1
            END IF
c           EMIFAC = 3600.
c           EMILBL = 'GRAMS/SEC'
c           OUTLBL = 'GRAMS/M**2'
c           CHIDEP(1) = '  TO'
c           CHIDEP(2) = 'TAL '
c           CHIDEP(3) = 'DEPO'
c           CHIDEP(4) = 'SITI'
c           CHIDEP(5) = 'ON  '
c           CHIDEP(6) = '    '
         ELSE IF (KOPT .EQ. 'DDEP') THEN
            IF (.NOT. DDEP) THEN
               DDEP   = .TRUE.
               NUMTYP = NUMTYP + 1
            END IF
c           EMIFAC = 3600.
c           EMILBL = 'GRAMS/SEC'
c           OUTLBL = 'GRAMS/M**2'
c           CHIDEP(1) = '    '
c           CHIDEP(2) = 'DRY '
c           CHIDEP(3) = 'DEPO'
c           CHIDEP(4) = 'SITI'
c           CHIDEP(5) = 'ON  '
c           CHIDEP(6) = '    '
         ELSE IF (KOPT .EQ. 'WDEP') THEN
            IF (.NOT. WDEP) THEN
               WDEP   = .TRUE.
               NUMTYP = NUMTYP + 1
            END IF
c           EMIFAC = 3600.
c           EMILBL = 'GRAMS/SEC'
c           OUTLBL = 'GRAMS/M**2'
c           CHIDEP(1) = '    '
c           CHIDEP(2) = 'WET '
c           CHIDEP(3) = 'DEPO'
c           CHIDEP(4) = 'SITI'
c           CHIDEP(5) = 'ON  '
c           CHIDEP(6) = '    '
         ELSE IF (KOPT .EQ. 'RURAL') THEN
            RURAL = .TRUE.
         ELSE IF (KOPT .EQ. 'URBAN') THEN
            URBAN = .TRUE.
         ELSE IF (KOPT .EQ. 'DRYDPLT') THEN
            DDPLETE = .TRUE.
         ELSE IF (KOPT .EQ. 'WETDPLT') THEN
            WDPLETE = .TRUE.
         ELSE IF (KOPT .EQ. 'NOSMPL') THEN
            NOSMPL = .TRUE.
         ELSE IF (KOPT .EQ. 'NOCMPL') THEN
            NOCMPL = .TRUE.
         ELSE IF (KOPT .EQ. 'GRDRIS') THEN
            IF (.NOT. DFAULT) THEN
               GRDRIS = .TRUE.
            ELSE
C              WRITE Warning Message     ! Non-DEFAULT Option Overridden
               CALL ERRHDL(PATH,MODNAM,'W','206',KOPT)
            END IF
         ELSE IF (KOPT .EQ. 'NOSTD') THEN
            IF (.NOT. DFAULT) THEN
               NOSTD = .TRUE.
            ELSE
C              WRITE Warning Message     ! Non-DEFAULT Option Overridden
               CALL ERRHDL(PATH,MODNAM,'W','206',KOPT)
            END IF
         ELSE IF (KOPT .EQ. 'NOBID') THEN
            IF (.NOT. DFAULT) THEN
               NOBID = .TRUE.
            ELSE
C              WRITE Warning Message     ! Non-DEFAULT Option Overridden
               CALL ERRHDL(PATH,MODNAM,'W','206',KOPT)
            END IF
         ELSE IF (KOPT .EQ. 'NOCALM') THEN
            IF (.NOT. DFAULT) THEN
               NOCALM = .TRUE.
               CLMPRO = .FALSE.
            ELSE
C              WRITE Warning Message     ! Non-DEFAULT Option Overridden
               CALL ERRHDL(PATH,MODNAM,'W','206',KOPT)
            END IF
         ELSE IF (KOPT .EQ. 'MSGPRO') THEN
            IF (.NOT. DFAULT) THEN
               MSGPRO = .TRUE.
            ELSE
C              WRITE Warning Message     ! Non-DEFAULT Option Overridden
               CALL ERRHDL(PATH,MODNAM,'W','206',KOPT)
            END IF
         ELSE
C           WRITE Error Message     ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203',KOPT)
         END IF
 20   CONTINUE

C     Setup Label Array for Concentration and Depositions
      IF (NUMTYP .GT. NTYP) THEN
C        WRITE Error Message: Number of output types exceeds maximum
         WRITE(DUMMY,'(I4)') NTYP
         CALL ERRHDL(PATH,MODNAM,'E','290',DUMMY)
      ELSE IF (NUMTYP .EQ. 0) THEN
C        WRITE Warning Message: No Output Types Selected, Assume CONC Only
         CALL ERRHDL(PATH,MODNAM,'W','205','CONC')
         NUMTYP = 1
         ITYP   = 1
         CONC   = .TRUE.
         CHIDEP(1,ITYP) = 'AVER'
         CHIDEP(2,ITYP) = 'AGE '
         CHIDEP(3,ITYP) = 'CONC'
         CHIDEP(4,ITYP) = 'ENTR'
         CHIDEP(5,ITYP) = 'ATIO'
         CHIDEP(6,ITYP) = 'N   '
         EMIFAC(ITYP) = 1.0E06
         EMILBL(ITYP) = 'GRAMS/SEC'
         OUTLBL(ITYP) = 'MICROGRAMS/M**3'
      ELSE IF (CONC) THEN
         ITYP = 1
         CHIDEP(1,ITYP) = 'AVER'
         CHIDEP(2,ITYP) = 'AGE '
         CHIDEP(3,ITYP) = 'CONC'
         CHIDEP(4,ITYP) = 'ENTR'
         CHIDEP(5,ITYP) = 'ATIO'
         CHIDEP(6,ITYP) = 'N   '
         EMIFAC(ITYP) = 1.0E06
         EMILBL(ITYP) = 'GRAMS/SEC'
         OUTLBL(ITYP) = 'MICROGRAMS/M**3'
         IF (DEPOS) THEN
            ITYP = 2
            CHIDEP(1,ITYP) = '  TO'
            CHIDEP(2,ITYP) = 'TAL '
            CHIDEP(3,ITYP) = 'DEPO'
            CHIDEP(4,ITYP) = 'SITI'
            CHIDEP(5,ITYP) = 'ON  '
            CHIDEP(6,ITYP) = '    '
            EMIFAC(ITYP) = 3600.
            EMILBL(ITYP) = 'GRAMS/SEC'
            OUTLBL(ITYP) = 'GRAMS/M**2'
            IF (DDEP) THEN
               ITYP = 3
               CHIDEP(1,ITYP) = '    '
               CHIDEP(2,ITYP) = 'DRY '
               CHIDEP(3,ITYP) = 'DEPO'
               CHIDEP(4,ITYP) = 'SITI'
               CHIDEP(5,ITYP) = 'ON  '
               CHIDEP(6,ITYP) = '    '
               EMIFAC(ITYP) = 3600.
               EMILBL(ITYP) = 'GRAMS/SEC'
               OUTLBL(ITYP) = 'GRAMS/M**2'
               IF (WDEP) THEN
                  ITYP = 4
                  CHIDEP(1,ITYP) = '    '
                  CHIDEP(2,ITYP) = 'WET '
                  CHIDEP(3,ITYP) = 'DEPO'
                  CHIDEP(4,ITYP) = 'SITI'
                  CHIDEP(5,ITYP) = 'ON  '
                  CHIDEP(6,ITYP) = '    '
                  EMIFAC(ITYP) = 3600.
                  EMILBL(ITYP) = 'GRAMS/SEC'
                  OUTLBL(ITYP) = 'GRAMS/M**2'
               END IF
            ELSE IF (WDEP) THEN
               ITYP = 3
               CHIDEP(1,ITYP) = '    '
               CHIDEP(2,ITYP) = 'WET '
               CHIDEP(3,ITYP) = 'DEPO'
               CHIDEP(4,ITYP) = 'SITI'
               CHIDEP(5,ITYP) = 'ON  '
               CHIDEP(6,ITYP) = '    '
               EMIFAC(ITYP) = 3600.
               EMILBL(ITYP) = 'GRAMS/SEC'
               OUTLBL(ITYP) = 'GRAMS/M**2'
            END IF
         ELSE IF (DDEP) THEN
            ITYP = 2
            CHIDEP(1,ITYP) = '    '
            CHIDEP(2,ITYP) = 'DRY '
            CHIDEP(3,ITYP) = 'DEPO'
            CHIDEP(4,ITYP) = 'SITI'
            CHIDEP(5,ITYP) = 'ON  '
            CHIDEP(6,ITYP) = '    '
            EMIFAC(ITYP) = 3600.
            EMILBL(ITYP) = 'GRAMS/SEC'
            OUTLBL(ITYP) = 'GRAMS/M**2'
            IF (WDEP) THEN
               ITYP = 3
               CHIDEP(1,ITYP) = '    '
               CHIDEP(2,ITYP) = 'WET '
               CHIDEP(3,ITYP) = 'DEPO'
               CHIDEP(4,ITYP) = 'SITI'
               CHIDEP(5,ITYP) = 'ON  '
               CHIDEP(6,ITYP) = '    '
               EMIFAC(ITYP) = 3600.
               EMILBL(ITYP) = 'GRAMS/SEC'
               OUTLBL(ITYP) = 'GRAMS/M**2'
            END IF
         ELSE IF (WDEP) THEN
            ITYP = 2
            CHIDEP(1,ITYP) = '    '
            CHIDEP(2,ITYP) = 'WET '
            CHIDEP(3,ITYP) = 'DEPO'
            CHIDEP(4,ITYP) = 'SITI'
            CHIDEP(5,ITYP) = 'ON  '
            CHIDEP(6,ITYP) = '    '
            EMIFAC(ITYP) = 3600.
            EMILBL(ITYP) = 'GRAMS/SEC'
            OUTLBL(ITYP) = 'GRAMS/M**2'
         END IF
      ELSE IF (DEPOS) THEN
         ITYP = 1
         CHIDEP(1,ITYP) = '  TO'
         CHIDEP(2,ITYP) = 'TAL '
         CHIDEP(3,ITYP) = 'DEPO'
         CHIDEP(4,ITYP) = 'SITI'
         CHIDEP(5,ITYP) = 'ON  '
         CHIDEP(6,ITYP) = '    '
         EMIFAC(ITYP) = 3600.
         EMILBL(ITYP) = 'GRAMS/SEC'
         OUTLBL(ITYP) = 'GRAMS/M**2'
         IF (DDEP) THEN
            ITYP = 2
            CHIDEP(1,ITYP) = '    '
            CHIDEP(2,ITYP) = 'DRY '
            CHIDEP(3,ITYP) = 'DEPO'
            CHIDEP(4,ITYP) = 'SITI'
            CHIDEP(5,ITYP) = 'ON  '
            CHIDEP(6,ITYP) = '    '
            EMIFAC(ITYP) = 3600.
            EMILBL(ITYP) = 'GRAMS/SEC'
            OUTLBL(ITYP) = 'GRAMS/M**2'
            IF (WDEP) THEN
               ITYP = 3
               CHIDEP(1,ITYP) = '    '
               CHIDEP(2,ITYP) = 'WET '
               CHIDEP(3,ITYP) = 'DEPO'
               CHIDEP(4,ITYP) = 'SITI'
               CHIDEP(5,ITYP) = 'ON  '
               CHIDEP(6,ITYP) = '    '
               EMIFAC(ITYP) = 3600.
               EMILBL(ITYP) = 'GRAMS/SEC'
               OUTLBL(ITYP) = 'GRAMS/M**2'
            END IF
         ELSE IF (WDEP) THEN
            ITYP = 2
            CHIDEP(1,ITYP) = '    '
            CHIDEP(2,ITYP) = 'WET '
            CHIDEP(3,ITYP) = 'DEPO'
            CHIDEP(4,ITYP) = 'SITI'
            CHIDEP(5,ITYP) = 'ON  '
            CHIDEP(6,ITYP) = '    '
            EMIFAC(ITYP) = 3600.
            EMILBL(ITYP) = 'GRAMS/SEC'
            OUTLBL(ITYP) = 'GRAMS/M**2'
         END IF
      ELSE IF (DDEP) THEN
         ITYP = 1
         CHIDEP(1,ITYP) = '    '
         CHIDEP(2,ITYP) = 'DRY '
         CHIDEP(3,ITYP) = 'DEPO'
         CHIDEP(4,ITYP) = 'SITI'
         CHIDEP(5,ITYP) = 'ON  '
         CHIDEP(6,ITYP) = '    '
         EMIFAC(ITYP) = 3600.
         EMILBL(ITYP) = 'GRAMS/SEC'
         OUTLBL(ITYP) = 'GRAMS/M**2'
         IF (WDEP) THEN
            ITYP = 2
            CHIDEP(1,ITYP) = '    '
            CHIDEP(2,ITYP) = 'WET '
            CHIDEP(3,ITYP) = 'DEPO'
            CHIDEP(4,ITYP) = 'SITI'
            CHIDEP(5,ITYP) = 'ON  '
            CHIDEP(6,ITYP) = '    '
            EMIFAC(ITYP) = 3600.
            EMILBL(ITYP) = 'GRAMS/SEC'
            OUTLBL(ITYP) = 'GRAMS/M**2'
         END IF
      ELSE IF (WDEP) THEN
         ITYP = 1
         CHIDEP(1,ITYP) = '    '
         CHIDEP(2,ITYP) = 'WET '
         CHIDEP(3,ITYP) = 'DEPO'
         CHIDEP(4,ITYP) = 'SITI'
         CHIDEP(5,ITYP) = 'ON  '
         CHIDEP(6,ITYP) = '    '
         EMIFAC(ITYP) = 3600.
         EMILBL(ITYP) = 'GRAMS/SEC'
         OUTLBL(ITYP) = 'GRAMS/M**2'
      END IF

      EMICON = 1.0E06

C     Modify PLTFRM and PSTFRM if needed for more than one output type
      IF (NUMTYP .GT. 1) THEN
         WRITE(PLTFRM,1009) NUMTYP+2
 1009    FORMAT('(',I1,'(1X,F13.5),1X,F8.2,3X,A5,2X,A8,2X,A4,6X,A8)')
         WRITE(PSTFRM,1019) NUMTYP+2
 1019    FORMAT('(',I1,'(1X,F13.5),1X,F8.2,2X,A6,2X,A8,2X,I8,2X,A8)')
      END IF

C     Check for Conflicting Options
      IF (RURAL .AND. URBAN) THEN
C        WRITE Warning Message:  Using RURAL Setting
         CALL ERRHDL(PATH,MODNAM,'W','204','RURAL')
         URBAN = .FALSE.
      ELSE IF (.NOT.RURAL .AND. .NOT.URBAN) THEN
C        WRITE Warning Message   ! Use RURAL as Default
         CALL ERRHDL(PATH,MODNAM,'W','205','RURAL')
         RURAL = .TRUE.
      END IF
      IF (NOSMPL .AND. NOCMPL) THEN
C        WRITE ERROR Message:  Using NOCMPL Setting
         CALL ERRHDL(PATH,MODNAM,'E','204','NOCMPL')
         NOSMPL = .FALSE.
      END IF

 999  RETURN
      END

      SUBROUTINE AVETIM
C***********************************************************************
C                 AVETIM Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process Averaging Time Options From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Averaging Period Array and PERIOD Logical Switch
C
C        ERROR HANDLING:   Checks for Too Many Short Term Averages (>4);
C                          Checks for Invalid Averaging Periods, MOD(24,X) NE 0;
C                          Checks for Duplicate Short Term Averaging Periods
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      CHARACTER*8 KOPT

C     Variable Initializations
      MODNAM = 'AVETIM'

C     Check for No Parameters
      IF (IFC .LT. 3) THEN
C        WRITE Error Message     ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      END IF

C     First Check for Presence of PERIOD or ANNUAL Switch
      DO 10 I = 3, IFC
         KOPT = FIELD(I)
         IF (KOPT .EQ. 'PERIOD') THEN
            PERIOD = .TRUE.
         ELSE IF (KOPT .EQ. 'ANNUAL') THEN
            ANNUAL = .TRUE.
         END IF
 10   CONTINUE

C     Check for Both PERIOD and ANNUAL
      IF (PERIOD .AND. ANNUAL) THEN
         CALL ERRHDL(PATH,MODNAM,'E','295',KEYWRD)
      END IF

C     Check for Too Many Averaging Periods
      IF (PERIOD .OR. ANNUAL) THEN
         IF (IFC .GT. NAVE+3) THEN
C           WRITE Error Message: Too Many Period Or Time Fields
            CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
          END IF
      ELSE
         IF (IFC .GT. NAVE+2) THEN
C           WRITE Error Message: Too Many Period Or Time Fields
            CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         END IF
      END IF

C     Loop Through Fields Again, Filling KAVE Array for Short Term Averages
      J = 0
      DO 20 I = 3, IFC
         KOPT = FIELD(I)
         IF (KOPT .NE. 'PERIOD' .AND. KOPT .NE. 'ANNUAL') THEN
            IF (KOPT .NE. 'MONTH') THEN
               CALL STONUM(KOPT,8,AVENUM,IDUM)
               IF (IDUM .EQ. -1) THEN
C                 Write Error Message:Invalid Numerical Field
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               END IF
            END IF
C           Check for Valid Averaging Period
            IF (KOPT.EQ.'MONTH' .OR. (MOD(24,INT(AVENUM)).EQ.0 .AND.
     &                                IDUM.EQ.1)) THEN
               J = J + 1
               IF (J .LE. NAVE) THEN
                  IF (KOPT .EQ. 'MONTH') THEN
                     KAVE(J) = 720
                     MONTH = .TRUE.
                     CHRAVE(J) = 'MONTH'
                  ELSE
                     KAVE(J) = AVENUM
                     WRITE(CHRAVE(J),'(I2,3H-HR)') KAVE(J)
                  END IF
                  NUMAVE = J
C                 Check for Duplicate Averaging Periods
                  DO 15 K = J-1, 1, -1
                     IF (KAVE(J) .EQ. KAVE(K)) THEN
C                       WRITE Error Message    ! Duplicate Averaging Period
                        CALL ERRHDL(PATH,MODNAM,'E','211',KEYWRD)
                     END IF
 15               CONTINUE
               ELSE
C                 WRITE Error Message   ! Too Many Short Term Averaging Periods
                  WRITE(DUMMY,'(I8)') NAVE
                  CALL ERRHDL(PATH,MODNAM,'E','210',DUMMY)
               END IF
            ELSE
C              WRITE Error Message      ! Invalid Averaging Period
               CALL ERRHDL(PATH,MODNAM,'E','203','AVEPER')
            END IF
         END IF
 20   CONTINUE

 999  RETURN
      END

      SUBROUTINE POLLID
C***********************************************************************
C                 POLLID Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Processes Pollutant Identification Option
C
C        PROGRAMMER: Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Pollutant Identification Option
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'POLLID'

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 3) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

      POLLUT = FIELD(3)

C     Check for Urban Regulatory Default for SO2
      IF (DFAULT .AND. URBAN .AND. POLLUT.EQ.'SO2') THEN
C        Set Default Decay Coefficient Corresponding to 4-Hour Half Life
         DECOEF = 4.81E-5
      END IF

 999  RETURN
      END

      SUBROUTINE EDECAY
C***********************************************************************
C                 EDECAY Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Processes Exponential Decay Options
C
C        PROGRAMMER: Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Exponental Decay Options
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'EDECAY'

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 3) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Start To Get Decay Coef.
      CALL STONUM(FIELD(3),40,FNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF

      IF (KEYWRD .EQ. 'HALFLIFE') THEN
         HAFLIF = FNUM
C        Calculate Decay Coef. by Halflife
         DECOEF = 0.693/HAFLIF
      ELSE IF (KEYWRD .EQ. 'DCAYCOEF') THEN
         DECOEF = FNUM
      END IF

C     Check for Urban Regulatory Default for SO2
      IF (DFAULT .AND. URBAN .AND. POLLUT.EQ.'SO2') THEN
         IF (DECOEF .NE. 4.81E-5) THEN
C           WRITE Warning Message: Attempt to Override Regulatory Default
            CALL ERRHDL(PATH,MODNAM,'W','206',KEYWRD)
         END IF
         DECOEF = 4.81E-5
      ELSE IF (DFAULT) THEN
         IF (DECOEF .NE. 0.0) THEN
C           WRITE Warning Message: Attempt to Override Regulatory Default
            CALL ERRHDL(PATH,MODNAM,'W','206',KEYWRD)
         END IF
         DECOEF = 0.0
      END IF

 999  RETURN
      END

      SUBROUTINE ELUNIT
C***********************************************************************
C                 ELUNIT Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process Terrain Elevation Units Option
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Terrain Elevation Units Switch
C
C        ERROR HANDLING:   Checks for Invalid Parameters;
C                          Checks for No Parameters;
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'ELUNIT'

      IF (IFC .EQ. 3) THEN
         IF (FIELD(3) .EQ. 'METERS') THEN
            ELTYPE = 'METERS'
         ELSE IF (FIELD(3) .EQ. 'FEET') THEN
            ELTYPE = 'FEET'
         ELSE
C           WRITE Error Message  ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203','ELTYPE')
         END IF
      ELSE IF (IFC .GT. 3) THEN
C        WRITE Error Message     ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Error Message     ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200','ElevUnit')
      END IF

 999  RETURN
      END

      SUBROUTINE TERRHT
C***********************************************************************
C                 TERRHT Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process Terrain Height Option From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Terrain Height Option Logical Switch
C
C        ERROR HANDLING:   Checks for Invalid Parameters;
C                          Checks for No Parameters;
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'TERRHT'

      IF (IFC .EQ. 3) THEN
         IF (FIELD(3) .EQ. 'FLAT') THEN
            FLAT = .TRUE.
            ELEV = .FALSE.
         ELSE IF (FIELD(3) .EQ. 'ELEV') THEN
            ELEV = .TRUE.
            FLAT = .FALSE.
         ELSE
C           WRITE Error Message  ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203',KEYWRD)
         END IF
      ELSE IF (IFC .GT. 3) THEN
C        WRITE Error Message     ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Error Message     ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      END IF

 999  RETURN
      END

      SUBROUTINE RUNNOT
C***********************************************************************
C                 RUNNOT Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process Option To RUN Or NOT From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Model RUN Logical Switch
C
C        ERROR HANDLING:   Checks for Invalid Parameters;
C                          Checks for No Parameters;
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'RUNNOT'

      IF (IFC .EQ. 3) THEN
         IF (FIELD(3) .EQ. 'RUN') THEN
            RUN = .TRUE.
         ELSE IF (FIELD(3) .EQ. 'NOT') THEN
            RUN = .FALSE.
         ELSE
C           WRITE Error Message  ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203',KEYWRD)
         END IF
      ELSE IF (IFC .GT. 3) THEN
C        WRITE Error Message     ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Error Message     ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      END IF

 999  RETURN
      END

      SUBROUTINE FLAGDF
C***********************************************************************
C                 FLAGDF Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process Default Flagpole Receptor Height Option
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Default Flagpole Receptor Heights
C
C        ERROR HANDLING:   Checks for Invalid Parameters;
C                          Checks for No Parameters;
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'FLAGDF'
      FLGPOL = .TRUE.

      IF (IFC .EQ. 3) THEN
         CALL STONUM(FIELD(3),40,ZFLG,IDUM)
         IF (IDUM .EQ. -1) THEN
C           Write Error Message:Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         END IF
         IF (ZFLG .GE. 0.0 .AND. IDUM .EQ. 1) THEN
            DO 10 I = 1, NREC
               AZFLAG(I) = ZFLG
 10         CONTINUE
         ELSE IF (ZFLG .LT. 0.0) THEN
C            WRITE Error Message: Invalid Data. Positive Value Turns Negative
             CALL ERRHDL(PATH,MODNAM,'E','209','ZFLAG')
         ELSE IF (IDUM .NE. 1) THEN
C            WRITE Error Message: Field Number Not Meet Requirement
             CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         ELSE
C            WRITE Error Message: Invalid Parameter
             CALL ERRHDL(PATH,MODNAM,'E','203',KEYWRD)
         END IF
      ELSE IF (IFC .GT. 3) THEN
C        WRITE Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'W','205','ZFLAG=0.')
      END IF

 999  RETURN
      END

      SUBROUTINE EVNTFL
C***********************************************************************
C                 EVNTFL Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process EVENT File Option
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: EVENT File Logical Switch and EVENT Filename
C
C        ERROR HANDLING:   Checks for No Parametes;
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'EVNTFL'

      IF (IFC .EQ. 3) THEN
         EVENTS = .TRUE.
         EVFILE = RUNST1(LOCB(3):LOCE(3))
         EVPARM = 'DETAIL'
      ELSE IF (IFC .EQ. 4) THEN
         EVENTS = .TRUE.
         EVFILE = RUNST1(LOCB(3):LOCE(3))
         EVPARM = FIELD(4)
      ELSE IF (IFC .GT. 4) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Warning Message         ! No Parameters - Use Default Name
         CALL ERRHDL(PATH,MODNAM,'W','207',KEYWRD)
         EVENTS = .TRUE.
         EVFILE = 'EVENTS.INP'
         EVPARM = 'DETAIL'
      END IF

C     Check for Invalid EVPARM
      IF (EVPARM .NE. 'SOCONT' .AND. EVPARM .NE. 'DETAIL') THEN
C        WRITE Warning Message         ! Invalid Parameter - Use Default
         CALL ERRHDL(PATH,MODNAM,'W','203','EVPARM')
      END IF

C     Open The EVENT Input File
      OPEN(UNIT=IEVUNT,FILE=EVFILE,STATUS='UNKNOWN',
     &     FORM='FORMATTED')

 999  RETURN
      END

      SUBROUTINE SAVEFL
C***********************************************************************
C                 SAVEFL Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process RESTART File Save Option
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: RSTSAV File Logical Switch and RESTART Filename
C
C        ERROR HANDLING:   Checks for No Parametes (uses default name);
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   COCARD
C***********************************************************************
C
C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'SAVEFL'

      IF (MULTYR) THEN
C        WRITE Error Message:  Conflicting Options RE-START and MULTYEAR
         CALL ERRHDL(PATH,MODNAM,'E','145',KEYWRD)
      ELSE IF (IFC .EQ. 3) THEN
         RSTSAV = .TRUE.
         SAVFIL = RUNST1(LOCB(3):LOCE(3))
         SAVFL2 = SAVFIL
         INCRST = 1
      ELSE IF (IFC .EQ. 4) THEN
         RSTSAV = .TRUE.
         SAVFIL = RUNST1(LOCB(3):LOCE(3))
         SAVFL2 = SAVFIL
         CALL STONUM(FIELD(4),40,FNUM,IDUM)
         INCRST = INT(FNUM)
         IF (IDUM .EQ. -1) THEN
C           Write Error Message:Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         END IF
      ELSE IF (IFC .EQ. 5) THEN
         RSTSAV = .TRUE.
         SAVFIL = RUNST1(LOCB(3):LOCE(3))
         CALL STONUM(FIELD(4),40,FNUM,IDUM)
         INCRST = INT(FNUM)
         IF (IDUM .EQ. -1) THEN
C           Write Error Message:Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         END IF
         SAVFL2 = RUNST1(LOCB(5):LOCE(5))
      ELSE IF (IFC .GT. 5) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Warning Message          ! No Parameters - Use Default Name
         CALL ERRHDL(PATH,MODNAM,'W','207',KEYWRD)
         RSTSAV = .TRUE.
         SAVFIL = 'SAVE.FIL'
         SAVFL2 = SAVFIL
         INCRST = 1
      END IF

 999  RETURN
      END

      SUBROUTINE INITFL
C***********************************************************************
C                 INITFL Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process RESTART Initialization Input File Option
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To change default filename to SAVE.FIL to match
C                    default name for SAVEFILE card.
C                    R.W. Brode, PES, Inc. - 6/20/95
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: RSTINP Logical Switch and Re-start Input Filename
C
C        ERROR HANDLING:   Checks for No Parametes (uses default name);
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   COCARD
C***********************************************************************
C
C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'INITFL'

      IF (MULTYR) THEN
C        WRITE Error Message:  Conflicting Options RE-START and MULTYEAR
         CALL ERRHDL(PATH,MODNAM,'E','145',KEYWRD)
      ELSE IF (IFC .EQ. 3) THEN
         RSTINP = .TRUE.
         INIFIL = RUNST1(LOCB(3):LOCE(3))
      ELSE IF (IFC .GT. 3) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Warning Message          ! No Parameters - Use Default Name
         CALL ERRHDL(PATH,MODNAM,'W','207',KEYWRD)
         RSTINP = .TRUE.
         INIFIL = 'SAVE.FIL'
      END IF

 999  RETURN
      END

      SUBROUTINE ERRFIL
C***********************************************************************
C                 ERRFIL Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process Error Message File Option
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Error Message File Logical Switch and ERRMSG Filename
C
C        ERROR HANDLING:   Checks for No Parametes (uses default name);
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   COCARD
C***********************************************************************
C
C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'ERRFIL'

      IF (IFC .EQ. 3) THEN
         ERRLST = .TRUE.
         MSGFIL = RUNST1(LOCB(3):LOCE(3))
      ELSE IF (IFC .EQ. 4) THEN
C*----   ISCSTM Modification: allow for NOCHKD option - jah 11/2/94
         IF (FIELD(4) .EQ. 'DEBUG') THEN
C*          DEBUG Option Selected
            ERRLST = .TRUE.
            MSGFIL = RUNST1(LOCB(3):LOCE(3))
            DEBUG  = .TRUE.

         ELSE IF (FIELD(4) .EQ. 'NOCHKD') THEN
C*          NOCHKD Option Selected - Bypass Checking of Date Sequence
            ERRLST = .TRUE.
            MSGFIL = RUNST1(LOCB(3):LOCE(3))
            NOCHKD = .TRUE.
         END IF

      ELSE IF (IFC .EQ. 5) THEN
         IF (FIELD(4) .EQ. 'DEBUG' .OR. FIELD(5) .EQ. 'DEBUG') THEN
C*          DEBUG Option Selected
            ERRLST = .TRUE.
            MSGFIL = RUNST1(LOCB(3):LOCE(3))
            DEBUG  = .TRUE.
         END IF
         IF (FIELD(4) .EQ. 'NOCHKD' .OR. FIELD(5) .EQ. 'NOCHKD') THEN
C*          NOCHKD Option Selected - Bypass Checking of Date Sequence
            ERRLST = .TRUE.
            MSGFIL = RUNST1(LOCB(3):LOCE(3))
            NOCHKD = .TRUE.
         END IF

      ELSE IF (IFC .GT. 5) THEN
C*----

C*       WRITE Error Message                      ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C*       WRITE Warning Message              ! No Parameters - Use Default Name
         CALL ERRHDL(PATH,MODNAM,'W','207',KEYWRD)
         ERRLST = .TRUE.
         MSGFIL = 'ERRORS.LST'
      END IF
C*#

 999  RETURN
      END

      SUBROUTINE MYEAR
C***********************************************************************
C                 MYEAR Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process RESTART File Save Option
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: RSTSAV File Logical Switch and RESTART Filename
C
C        ERROR HANDLING:   Checks for No Parametes (uses default name);
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   COCARD
C***********************************************************************
C
C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'MYEAR'

      IF (RSTSAV .OR. RSTINP) THEN
C        WRITE Error Message:  Conflicting Options RE-START and MULTYEAR
         CALL ERRHDL(PATH,MODNAM,'E','145',KEYWRD)
      ELSE IF (POLLUT .NE. 'PM10' .AND. POLLUT .NE. 'PM-10' .AND.
     &         POLLUT .NE. 'LEAD' .AND. POLLUT .NE. 'OTHER') THEN
C        WRITE Error Message:  Conflicting Options MULTYEAR For Wrong POLLUT
         CALL ERRHDL(PATH,MODNAM,'E','150',KEYWRD)
      ELSE IF (IFC .EQ. 3) THEN
         MULTYR = .TRUE.
         RSTSAV = .TRUE.
C        Use Character Substring to Retrieve Filenames to Maintain Case
         SAVFIL = RUNST1(LOCB(3):LOCE(3))
         SAVFL2 = SAVFIL
C        Value of INCRST is Set to 365 or 366 in SUB. MECARD
      ELSE IF (IFC .EQ. 4) THEN
         MULTYR = .TRUE.
         RSTSAV = .TRUE.
C        Use Character Substring to Retrieve Filenames to Maintain Case
         SAVFIL = RUNST1(LOCB(3):LOCE(3))
         SAVFL2 = SAVFIL
         RSTINP = .TRUE.
         INIFIL = RUNST1(LOCB(4):LOCE(4))
C        Value of INCRST is Set to 365 or 366 in SUB. MECARD
      ELSE IF (IFC .GT. 4) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Warning Message          ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      END IF

 999  RETURN
      END
      SUBROUTINE TGCARD
C***********************************************************************
C                 TGCARD Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: To process Terrain Grid Pathway Card Images
C
C        PROGRAMMER: D. Strimaitis, SRC
C
C        DATE:    December 15, 1993
C
C        INPUTS:  Pathway (TG) and Keyword
C
C        OUTPUTS: Terrain Grid Filename
C                 Origin to use with Terrain Grid (shift in UTM coord.,
C                   must be same as that used for sources/receptors,
C                   but this is not checked!)
C                 Gridded Terrain Data
C
C        CALLED FROM:   SETUP
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'TGCARD'

      IF (KEYWRD .EQ. 'STARTING') THEN
C        Set Status Switch
         ITSTAT(1) = ITSTAT(1) + 1
         IF (ITSTAT(1) .NE. 1) THEN
C           WRITE Error Message: Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         END IF
      ELSE IF (KEYWRD .EQ. 'INPUTFIL') THEN
C        Set Status Switch
         ITSTAT(2) = ITSTAT(2) + 1
         IF (ITSTAT(2) .NE. 1) THEN
C           WRITE Error Message: Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Gridded Terrain File Information         ---   CALL TERFIL
            CALL TERFIL
         END IF
      ELSE IF (KEYWRD .EQ. 'LOCATION') THEN
C        Set Status Switch
         ITSTAT(3) = ITSTAT(3) + 1
         IF (ITSTAT(3) .NE. 1) THEN
C           WRITE Error Message: Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Location (origin) Information           ---   CALL TERLOC
            CALL TERLOC
         END IF
      ELSE IF (KEYWRD .EQ. 'ELEVUNIT') THEN
C        Set Status Switch
         ITSTAT(4) = ITSTAT(4) + 1
         IF (ICSTAT(10) .NE. 0) THEN
C           Write Error Message: Use of obsolescent CO ELEVUNIT card with
C           TG ELEVUNIT card
            CALL ERRHDL(PATH,MODNAM,'E','153',' TG Path')
         ELSE IF (ITSTAT(4) .NE. 1) THEN
C           WRITE Error Message: Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Elevation Units for Source Elevations   ---   CALL TGELUN
            CALL TGELUN
         END IF
      ELSE IF (KEYWRD .EQ. 'FINISHED') THEN
C        Set Status Switch
         ITSTAT(20) = ITSTAT(20) + 1
         IF (ITSTAT(20) .NE. 1) THEN
C           WRITE Error Message: Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
            GO TO 999
         END IF

C        Open Terrain Data File (Free-Format ASCII) and Process Data
         IF (ITSTAT(2) .NE. 0) THEN
            OPEN(UNIT=IZUNIT,ERR=99,FILE=TERINP,IOSTAT=IOERRN,
     &           STATUS='OLD')
            GOTO 100
C           Write Out Error Message for File OPEN Error
 99         CALL ERRHDL(PATH,MODNAM,'E','500',' TER-INP')
            GOTO 999
C                                                           ---   CALL TGDATA
100         CALL TGDATA(TGX0,TGY0,IZUNIT,TGELEV,IERRTG,
     &                  GRDXLL,GRDXUR,GRDYLL,GRDYUR,XYINT)
            IF (IERRTG .NE. 0) THEN
C              Write Out Error Message for File Grid Error
               CALL ERRHDL(PATH,MODNAM,'E','510',' TER-INP')
            ELSE
               LTGRID=.TRUE.
            END IF

C           Check to make sure that terrain grid covers all source/receptor
C           locations and check consistency of elevations   ---   CALL TGQA
            IF (LTGRID) THEN
               CALL TGQA
            END IF

         END IF

      ELSE
C        Write Error Message: Invalid Keyword for This Pathway
         CALL ERRHDL(PATH,MODNAM,'E','110',KEYWRD)
      END IF

 999  RETURN
      END

      SUBROUTINE TERFIL
C***********************************************************************
C                 TERFIL Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process Meteorology Input File Options
C                 From Runstream Input Image
C
C        PROGRAMMER: D. Strimaitis, SRC
C
C        DATE:    December 15, 1993
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Gridded Terrain Data Filename
C
C        ERROR HANDLING:   Checks for No Parameters;
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   TGCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'TERFIL'

      IF (IFC .EQ. 3) THEN
C        Retrieve Data Filename as Character Substring to Maintain Case
         TERINP = RUNST1(LOCB(3):LOCE(3))
      ELSE IF (IFC .GT. 3) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Warning Message         ! No Parameters Specified
         CALL ERRHDL(PATH,MODNAM,'W','200',KEYWRD)
      END IF

      RETURN
      END

      SUBROUTINE TERLOC
C***********************************************************************
C                 TERLOC Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process Location to be used as Origin of Grid Coord.
C                 From Runstream Input Image
C
C        PROGRAMMER: D. Strimaitis, SRC
C
C        DATE:    December 15, 1993
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Origin of Cartesian System, TGX0,TGY0 (UTM m)
C
C        ERROR HANDLING:   Checks for No Parameters;
C                          Checks for No Units (uses default of m);
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   TGCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'TERLOC'

      IF (IFC .EQ. 4 .OR. IFC .EQ. 5) THEN
         CALL STONUM(FIELD(3),40,TGX0,IDUM1)
C        Check The Numerical Field
         IF (IDUM1.EQ.-1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
         CALL STONUM(FIELD(4),40,TGY0,IDUM2)
C        Check The Numerical Field
         IF (IDUM2.EQ.-1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
         IF (IFC .EQ. 5) THEN
            IF (FIELD(5) .EQ. 'FEET') THEN
               TGX0 = 0.3048 * TGX0
               TGY0 = 0.3048 * TGY0
            ELSE IF (FIELD(5) .EQ. 'KM') THEN
               TGX0 = 1000. * TGX0
               TGY0 = 1000. * TGY0
            ELSE IF (FIELD(5) .NE. 'METERS') THEN
C              WRITE Warning Message - Invalid TGUNIT Parameter
               CALL ERRHDL(PATH,MODNAM,'W','203','TGUNIT')
            END IF
         ELSE IF (IDUM1*IDUM2 .NE. 1) THEN
C           WRITE Error Message - Invalid Numeric Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         END IF
      ELSE IF (IFC .GT. 5) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Error Message           ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      END IF

 999  RETURN
      END

      SUBROUTINE TGELUN
C***********************************************************************
C                 TGELUN Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process Elevation Units Option for Terrain Grid
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    November 22, 1994
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Terrain Grid Elevation Units Switch
C
C        ERROR HANDLING:   Checks for Invalid Parameters;
C                          Checks for No Parameters;
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   TGCARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'TGELUN'

      IF (IFC .EQ. 3) THEN
         IF (FIELD(3) .EQ. 'METERS') THEN
            TGELEV = 'METERS'
         ELSE IF (FIELD(3) .EQ. 'FEET') THEN
            TGELEV = 'FEET'
         ELSE
C           WRITE Error Message  ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203','TG_ELEV')
         END IF
      ELSE IF (IFC .GT. 3) THEN
C        WRITE Error Message     ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Error Message     ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200','ElevUnit')
      END IF

 999  RETURN
      END

c-----------------------------------------------------------------------
      subroutine tgdata(tgx0,tgy0,io,tgelev,ierr,grdxll,grdxur,grdyll,
     &                  grdyur,xyint)
c-----------------------------------------------------------------------
c
c --- ISCST2    Version: 1.0            Level: 931215           TGDATA
c               D. Strimaitis, SRC
c
c PURPOSE:     Subroutine reads terrain elevation data (m MSL) from
c              file, and places it in array for use by ZTERR.
c
c MODIFIED:    To elevation units from feet to meters if
c              TGELEV = 'FEET'.  Roger W. Brode, PES, Inc. - 11/22/94
c
c ARGUMENTS:
c    PASSED:  tgx0      x-UTM offset of modeling coord. system (m)   [r]
c             tgy0      y-UTM offset of modeling coord. system (m)   [r]
c             io        FORTRAN unit number for gridded terrain data [i]
c             tgelev    TG elevation units option ('FEET', 'METERS') [c]
c
c  RETURNED:  ierr      error condition indicator (no error = 0)     [i]
c             grdxllm   x-coord. of lower-left corner of grid  (m)   [r]
c             grdxurm   x-coord. of upper-right corner of grid (m)   [r]
c             grdyllm   y-coord. of lower-left corner of grid  (m)   [r]
c             grdyurm   y-coord. of upper-right corner of grid (m)   [r]
c             xyint     spacing between points in grid (m)           [r]
c
c To /TGRID/
c             xllm      x-coord. of lower-left corner of grid  (m)   [r]
c             xurm      x-coord. of upper-right corner of grid (m)   [r]
c             yllm      y-coord. of lower-left corner of grid  (m)   [r]
c             yurm      y-coord. of upper-right corner of grid (m)   [r]
c             sizem     spacing between points in grid (m)           [r]
c             izarray   array of terrain data  (whole m MSL)         [i]
c
c     (NOTE:  The coordinates of the lower-left and upper-right corners
c             of the grid are returned as arguments so that they can be
c             placed in MAIN1.INC for QA checks against source and
c             receptor locations.)
c
c
c CALLING ROUTINES:   SETUP
c
c EXTERNAL ROUTINES:  none
c-----------------------------------------------------------------------
      include 'DEPVAR.INC'
      character tgelev*6

c --- Initialize error flag
      ierr=0

c --- Read header
      read(io,*) ntx,nty,xllm,yllm,xurm,yurm,sizem

c --- Check number of points against the allocated limits
      if(ntx .GT. mxtx .OR. nty .GT. mxty) then
         write(*,*) 'TGDATA:  Fatal Error -- too many points!'
         write(*,*) 'ntx,nty     =',ntx,nty
         write(*,*) 'mxtx,mxty   =',mxtx,mxty
         write(*,*) 'Increase the parameters in DEPVAR.INC'
         ierr=1

      else

c ---    Reset coordinates of corners of grid to align with origin used
c ---    to specify souce/receptor locations.
         xllm=xllm-tgx0
         yllm=yllm-tgy0
         xurm=xurm-tgx0
         yurm=yurm-tgy0

c ---    Assign corners to variables passed back to calling subroutine
         grdxll=xllm
         grdyll=yllm
         grdxur=xurm
         grdyur=yurm
c ---    Assign interval size to variable passed back to calling subroutine
         xyint = sizem

c ---    Read data into array
         do jy=1,nty
            read(io,*) (izarray(ix,jy),ix=1,ntx)
         enddo

      endif

c --- Check for units conversion from feet to meters - R. Brode 11/22/94
      if (tgelev .eq. 'FEET') THEN
         do jy=1,nty
            do ix=1,ntx
               izarray(ix,jy) = izarray(ix,jy) * 0.3048
            enddo
         enddo
      endif

      return
      end

c-----------------------------------------------------------------------
      subroutine tgqa
c-----------------------------------------------------------------------
c
c --- ISCST2    Version: 1.0            Level: 931215           TGQA
c               D. Strimaitis, SRC
c
c PURPOSE:     Subroutine checks source/receptor locations against
c              the corners of the terrain grid to assure that all lie
c              within the grid.
c
c MODIFIED:    To compare interpolated elevations from grid file against
c              source elevations and receptor elevations.
c              Roger W. Brode, PES, Inc. - 11/29/94
c
c CALLING ROUTINES:   SETUP
c
c EXTERNAL ROUTINES:  none
c-----------------------------------------------------------------------

c --- Variable Declarations
      include 'MAIN1.INC'
      include 'MAIN2.INC'
      real params(4), xval(4), yval(4)

c --- Define a test logical
      logical lfail
      data lfail/.FALSE./

c --- Variable Initializations
      modnam = 'TGQA'

c --- Loop over sources to find max/min x and y coordinates
c --- (Does NOT treat Area Sources !!)
      xsmin=axs(1)
      ysmin=ays(1)
      xsmax=axs(1)
      ysmax=ays(1)
      do i=2,numsrc
         if(axs(i) .GT. xsmax) then
            xsmax=axs(i)
         elseif(axs(i) .LT. xsmin) then
            xsmin=axs(i)
         endif
         if(ays(i) .GT. ysmax) then
            ysmax=ays(i)
         elseif(ays(i) .LT. ysmin) then
            ysmin=ays(i)
         endif
      enddo

c --- Loop over receptors to find max/min x and y coordinates
      xrmin=axr(1)
      yrmin=ayr(1)
      xrmax=axr(1)
      yrmax=ayr(1)
      do i=2,numrec
         if(axr(i) .GT. xrmax) then
            xrmax=axr(i)
         elseif(axr(i) .LT. xrmin) then
            xrmin=axr(i)
         endif
         if(ayr(i) .GT. yrmax) then
            yrmax=ayr(i)
         elseif(ayr(i) .LT. yrmin) then
            yrmin=ayr(i)
         endif
      enddo

c --- Test max/min against corners of terrain grid
      xlltest=AMIN1(xsmin,xrmin)
      ylltest=AMIN1(ysmin,yrmin)
      xurtest=AMAX1(xsmax,xrmax)
      yurtest=AMAX1(ysmax,yrmax)
      if(xlltest .LT. grdxll) lfail=.TRUE.
      if(ylltest .LT. grdyll) lfail=.TRUE.
      if(xurtest .GT. grdxur) lfail=.TRUE.
      if(yurtest .GT. grdyur) lfail=.TRUE.

      if(LFAIL) then
C        Write Error Message: Invalid Keyword for This Pathway
         call ERRHDL(PATH,MODNAM,'E','305','GRID')
         write(iounit,*) 'Lower Left of Source Range   : ',xsmin,ysmin
         write(iounit,*) 'Upper Right of Source Range  : ',xsmax,ysmax
         write(iounit,*) 'Lower Left of Receptor Range : ',xrmin,yrmin
         write(iounit,*) 'Upper Right of Receptor Range: ',xrmax,yrmax
         write(iounit,*) 'Lower Left of Terrain Grid   : ',grdxll,grdyll
         write(iounit,*) 'Upper Right of Terrain Grid  : ',grdxur,grdyur
      endif

c     Loop through sources to compare source elevations to terrain grid
      do i = 1, numsrc
c        Set indices for 4-corners to interpolate elevation to source location
         indx1 = INT((axs(i) - grdxll)/xyint)
         indx2 = indx1 + 1
         indy1 = INT((ays(i) - grdyll)/xyint)
         indy2 = indy1 + 1
c        Extract elevations from Terrain Grid for the 4 corners
         call TGEXT(indx1,indx2,indy1,indy2,params)
         xval(1) = indx1
         xval(2) = indx2
         xval(3) = indx1
         xval(4) = indx2
         yval(1) = indy1
         yval(2) = indy1
         yval(3) = indy2
         yval(4) = indy2

c        Interpolate to obtain elevation at source location
         call INTERP(params,xval,yval,axs(i),ays(i),zint)

c        Compare interpolated height from terrain grid to source elevation
         if (azs(i) .lt. 0.5*zint .or. azs(i) .gt. 1.5*zint) then
            call ERRHDL(path,modnam,'W','393',srcid(i))
         end if

      enddo

c     Loop through receptors to compare receptor elevations to terrain grid
      do i = 1, numrec
c        Set indices for 4-corners to interpolate elevation to receptor location
         indx1 = INT((axr(i) - grdxll)/xyint)
         indx2 = indx1 + 1
         indy1 = INT((ayr(i) - grdyll)/xyint)
         indy2 = indy1 + 1
c        Extract elevations from Terrain Grid for the 4 corners
         call TGEXT(indx1,indx2,indy1,indy2,params)
         xval(1) = indx1
         xval(2) = indx2
         xval(3) = indx1
         xval(4) = indx2
         yval(1) = indy1
         yval(2) = indy1
         yval(3) = indy2
         yval(4) = indy2

c        Interpolate to obtain elevation at receptor location
         call INTERP(params,xval,yval,axr(i),ayr(i),zint)

c        Compare interpolated height from terrain grid to receptor elevation
         if (azelev(i) .lt. 0.5*zint .or. azelev(i) .gt. 1.5*zint) then
            write(dummy,'(3hRE#,i5.5)') i
            call ERRHDL(path,modnam,'W','394',dummy)
         end if

      enddo

      return
      end

      subroutine tgext(ix1,ix2,iy1,iy2,params)
C------------------------------------------------------------
C     ROUTINE: tgext
C     
C     PURPOSE: Extract terrain elevations from terrain grid array
C     for four points
C     
C     ARGUMENTS PASSED/RETURNED:
C     
C     PASSED:  
C     ix1       i4  First x-coordinate
C     ix2       i4  Second x-coordinate
C     iy1       i4  First y-coordinate
C     iy2       i4  Second y-coordinate
C     
C     RETURNED: 
C     params   r4  Array of grid values at (x1,y1), (x1,y2), (x2,y1) & (x2,y2)
C     
C     I/O:           NONE
C     
C     COMMON BLOCKS: DEPVAR.INC
C     
C     
C     EXTERNAL ROUTINES: NONE
C     
C------------------------------------------------------------

      include 'DEPVAR.INC'
      real params(*)

      params(1) = izarray(ix1,iy1)
      params(2) = izarray(ix2,iy1)
      params(3) = izarray(ix1,iy2)
      params(4) = izarray(ix2,iy2)

      return
      end


      SUBROUTINE INTERP(PARAMS,X1,Y1,XVALUE,YVALUE,VALUE)
C------------------------------------------------------------
C     ROUTINE: INTERP
C     
C     PURPOSE: LINEARLY INTERPOLATES BETWEEN FOUR VALUES AT FOUR POINTS
C     ON A PLANE TO GET A VALUE AT ONE POINT
C     
C     ARGUMENTS PASSED/RETURNED:
C     
C     PASSED:  
C     PARAMS   R4  KNOWN VALUES AT THE FOUR VERTICES OF A RECTANGLE
C     X1       R4  X COORDINATE LOCATION OF THE FOUR POINTS
C     Y1       R4  Y COORDINATE LOCATION OF THE FOUR POINTS
C     XVALUE   R4  X COORDINATE AT WHERE THE VALUE IS DESIRED
C     YVALUE   R4  Y COORDINATE AT WHERE THE VALUE IS DESIRED
C     
C     RETURNED: 
C     VALUE    R4  VALUE AT THE POINT DESIRED
C     
C     I/O:           NONE
C     
C     COMMON BLOCKS: NONE
C     
C     
C     EXTERNAL ROUTINES: NONE
C     
C------------------------------------------------------------
      DIMENSION PARAMS(4), X1(4), Y1(4)
C     
      XVALUM = XVALUE
      IF(XVALUM .GT. X1(2)) XVALUM = X1(2)
      IF(XVALUM .LT. X1(1)) XVALUM = X1(1)
      YVALUM = YVALUE
      IF(YVALUM .GT. Y1(3)) YVALUM = Y1(3)
      IF(YVALUM .LT. Y1(1)) YVALUM = Y1(1)
C     
      IF (X1(2) - X1(1) .LE. 0.) THEN
         X2MX = 1.0
         XMX1 = 0.0
      ELSE
         XBLKI = 1./(X1(2) - X1(1))
         X2MX  = (X1(2) - XVALUM)*XBLKI
         XMX1  = (XVALUM - X1(1))*XBLKI
      ENDIF
C     
      PINT1 = PARAMS(1)*X2MX + PARAMS(2)*XMX1
      PINT2 = PARAMS(3)*X2MX + PARAMS(4)*XMX1
C     
      IF (Y1(3) - Y1(1) .LE. 0.) THEN
         Y2MY = 1.0
         YMY1 = 0.0
      ELSE
         YBLKI = 1./(Y1(3) - Y1(1))
         Y2MY  = (Y1(3) - YVALUM)*YBLKI
         YMY1  = (YVALUM - Y1(1))*YBLKI
      ENDIF
C     
      VALUE = PINT1*Y2MY + PINT2*YMY1
C     
      RETURN
      END
      SUBROUTINE RECARD
C***********************************************************************
C                 RECARD Module of ISC2 Model
C
C        PURPOSE: To process REceptor Pathway card images
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To remove some restrictions on the order of
C                    the BOUNDELV keyword - 9/29/92
C
C        INPUTS:  Pathway (RE) and Keyword
C
C        OUTPUTS: Receptor Arrays
C                 Receptor Setup Status Switches
C
C        CALLED FROM:   SETUP
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'RECARD'

      IF (KEYWRD .EQ. 'STARTING') THEN
C        Initialize Counters and Set Status Switch
         IREC = 0
         INNET = 0
         NUMREC = 0
         IRXR = 0
         IRYR = 0
         IRZE = 0
         IRZF = 0
         IBND = 0
         IBELEV = 0
         PXSOID = ' '
         PESOID = ' '
         ISTA = .FALSE.
         IRSTAT(1) = IRSTAT(1) + 1
         IF (IRSTAT(1) .NE. 1) THEN
C           Error Message: Repeat Starting In Same Pathway
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         END IF
C        Flush the Working Arrays
         DO 10 I = 1, NREC
            ZETMP1(I) = 0.0
            ZETMP2(I) = 0.0
            ZFTMP1(I) = 0.0
            ZFTMP2(I) = 0.0
 10      CONTINUE
      ELSE IF (KEYWRD .EQ. 'GRIDCART') THEN
C        Set Status Switch
         IRSTAT(2) = IRSTAT(2) + 1
C        Process Cartesian Grid Receptor Network            ---   CALL RECART
         CALL RECART
      ELSE IF (KEYWRD .EQ. 'GRIDPOLR') THEN
C        Set Status Switch
         IRSTAT(3) = IRSTAT(3) + 1
C        Process Polar Receptor Network                     ---   CALL REPOLR
         CALL REPOLR
      ELSE IF (KEYWRD .EQ. 'DISCCART') THEN
C        Set Status Switch
         IRSTAT(4) = IRSTAT(4) + 1
C        Process Discrete Cartesian Receptor Locations      ---   CALL DISCAR
         CALL DISCAR
      ELSE IF (KEYWRD .EQ. 'DISCPOLR') THEN
C        Set Status Switch
         IRSTAT(5) = IRSTAT(5) + 1
C        Process Discrete Polar Receptor Locations          ---   CALL DISPOL
         CALL DISPOL
      ELSE IF (KEYWRD .EQ. 'BOUNDARY' .OR.
     &         KEYWRD .EQ. 'BOUNDELV') THEN
C        Set Status Switch
         IF (KEYWRD .EQ. 'BOUNDARY') THEN
            IRSTAT(6) = IRSTAT(6) + 1
         ELSE IF (KEYWRD .EQ. 'BOUNDELV') THEN
            IRSTAT(7) = IRSTAT(7) + 1
         END IF
C        Process Plant Boundary Receptor Locations          ---   CALL BOUNDR
         CALL BOUNDR
      ELSE IF (KEYWRD .EQ. 'ELEVUNIT') THEN
C        Set Status Switch
         IRSTAT(8) = IRSTAT(8) + 1
         IF (IRSTAT(8) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE IF (IRSTAT(2) .GT. 0 .OR. IRSTAT(3) .GT. 0 .OR.
     &            IRSTAT(4) .GT. 0 .OR. IRSTAT(5) .GT. 0 .OR.
     &            IRSTAT(6) .GT. 0 .OR. IRSTAT(7) .GT. 0) THEN
C           Write Error Message: ELEVUNIT must be first card after STARTING
            CALL ERRHDL(PATH,MODNAM,'E','152','  RE')
         ELSE IF (ICSTAT(10) .NE. 0) THEN
C           Write Error Message: Use of obsolescent CO ELEVUNIT card with
C           RE ELEVUNIT card
            CALL ERRHDL(PATH,MODNAM,'E','153',' RE Path')
         ELSE
C           Process Elevation Units for Source Elevations   ---   CALL REELUN
            CALL REELUN
         END IF
      ELSE IF (KEYWRD .EQ. 'FINISHED') THEN
C        Set Status Switch
         IRSTAT(20) = IRSTAT(20) + 1
         IF (IRSTAT(20) .NE. 1) THEN
C           Error Message: Repeat Finished In Same Pathway
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
            GO TO 999
         END IF
C        Write Out The Error Message: Mandatory Keyword Missing
         IF (IRSTAT(1) .EQ. 0)THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','STARTING')
         END IF

         IF (IRSTAT(2).EQ.0 .AND. IRSTAT(3).EQ.0 .AND.
     &       IRSTAT(4).EQ.0 .AND. IRSTAT(5).EQ.0 .AND.
     &       IRSTAT(6).EQ.0) THEN
C           WRITE Error Message:  No Receptor Keywords
            CALL ERRHDL(PATH,MODNAM,'E','185',' ')
         END IF

         IF (ISTA) THEN
C           WRITE Error Message:  Missing END Keyword for a Grid Network
            CALL ERRHDL(PATH,MODNAM,'E','175',PNETID)
         END IF

         IF (IRSTAT(6) .GT. 0) THEN
C           Check for Correct Number of Distances for Boundary Receptor
            IF (IBND .LT. 36) THEN
C              Error Message: No. Of Dist Not Enough
               CALL ERRHDL(PATH,MODNAM,'E','230','BOUNDARY')
            END IF
         END IF

         IF (ELEV .AND. IRSTAT(7) .GT. 0) THEN
C           Check for Correct Number of Elevations for Boundary Receptor
            IF (IBELEV .LT. 36) THEN
C              Error Message: No. Of Elev Not Enough
               CALL ERRHDL(PATH,MODNAM,'E','230','BOUNDELV')
            END IF
         END IF

C        Set Total Number of Receptors for This Run, NUMREC
         NUMREC = IRXR
         IF (NUMREC .EQ. 0) THEN
C           WRITE Error Message:  No Receptors Defined
            CALL ERRHDL(PATH,MODNAM,'E','227','NUMREC=0')
         END IF

C        Reinitialize ZELEV and ZFLAG arrays if needed
         IF (FLAT) THEN
            DO 100 IREC = 1, NUMREC
               AZELEV(IREC) = 0.0
 100        CONTINUE
         END IF
         IF (.NOT. FLGPOL) THEN
            DO 200 IREC = 1, NUMREC
               AZFLAG(IREC) = 0.0
 200        CONTINUE
         END IF

      ELSE
C        Write Error Message:  Invalid Keyword for This Pathway
         CALL ERRHDL(PATH,MODNAM,'E','110',KEYWRD)
      END IF

 999  RETURN
      END

      SUBROUTINE REELUN
C***********************************************************************
C                 REELUN Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process Elevation Units Option for Receptors
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    November 22, 1994
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Receptor Elevation Units Switch
C
C        ERROR HANDLING:   Checks for Invalid Parameters;
C                          Checks for No Parameters;
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   RECARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'REELUN'

      IF (IFC .EQ. 3) THEN
         IF (FIELD(3) .EQ. 'METERS') THEN
            REELEV = 'METERS'
         ELSE IF (FIELD(3) .EQ. 'FEET') THEN
            REELEV = 'FEET'
         ELSE
C           WRITE Error Message  ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203','RE_ELEV')
         END IF
      ELSE IF (IFC .GT. 3) THEN
C        WRITE Error Message     ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Error Message     ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200','ElevUnit')
      END IF

 999  RETURN
      END

      SUBROUTINE RECART
C***********************************************************************
C                 RECART Module of ISC2 Model
C
C        PURPOSE: Processes Cartesian Grid Receptor Network Inputs
C
C        PROGRAMMER:  Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To Fix Error Checking - Compare NETIDT With
C                    Full Secondary Keywords - 9/29/92
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Cartesian Grid Receptor Network Inputs
C
C        CALLED FROM:   RECARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'RECART'

C     READ in the Netid and Nettype
      IF (IFC .LT. 3) THEN
C        Write Error Message: Missing Data Field
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      END IF
      NETIDT = FIELD(3)
      IF (.NOT.NEWID .AND. (NETIDT.EQ.'    ' .OR.
     &    NETIDT.EQ.'XYINC' .OR. NETIDT.EQ.'XPNTS' .OR.
     &    NETIDT.EQ.'YPNTS' .OR. NETIDT.EQ.'ELEV' .OR.
     &    NETIDT.EQ.'FLAG'  .OR. NETIDT.EQ.'END')) THEN
         NETIDT = PNETID
         KTYPE = FIELD(3)
      ELSE IF (.NOT.NEWID .AND. NETIDT.EQ.PNETID) THEN
         KTYPE = FIELD(4)
      ELSE IF (NEWID .AND. NETIDT.NE.' ') THEN
         NEWID = .FALSE.
         KTYPE = FIELD(4)
C        The Keyword Counter
         INNET = INNET + 1
         IF (INNET .GT. NNET) THEN
C           WRITE Error Message:  Too Many Networks
            WRITE(DUMMY,'(I8)') NNET
            CALL ERRHDL(PATH,MODNAM,'E','224',DUMMY)
            RECERR = .TRUE.
            GO TO 999
         END IF
         INCSET = 0
         IXYSET = 0
         IEVSET = 0
         IFGSET = 0
      ELSE
C        Error Message: Invalid Secondary Keyword
         CALL ERRHDL(PATH,MODNAM,'E','170',PNETID)
         RECERR = .TRUE.
         GO TO 999
      END IF

C     Start to Set Up the Network
      IF (KTYPE .EQ. 'STA') THEN
C        Initialize Logical Control Variables
         ISTA = .TRUE.
         IEND = .FALSE.
         NEWID = .FALSE.
         RECERR = .FALSE.
C        Set Counters of Calculation Field
         ICOUNT = 0
         JCOUNT = 0
         IZE = 0
         IZF = 0
         IDC1 = IRXR
C        Check for Previous Grid Network With Same ID
         DO 100 I = 1, INNET-1
            IF (FIELD(3) .EQ. NTID(I)) THEN
C              WRITE Warning Message:  Duplicate Network ID
               CALL ERRHDL(PATH,MODNAM,'W','252',NTID(I))
            END IF
 100     CONTINUE
      ELSE IF (KTYPE .EQ. 'XYINC') THEN
C        Error Message:Conflict Secondary Keyword
         IF (IXYSET .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','180',NETIDT)
         END IF
C        Set the Uniform Spacing Receptor Network           ---   CALL GENCAR
         CALL GENCAR
         INCSET = INCSET + 1
      ELSE IF (KTYPE.EQ.'XPNTS' .OR. KTYPE.EQ.'YPNTS') THEN
C        Error Message:Conflict Secondary Keyword
         IF (INCSET .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','180',NETIDT)
         END IF
C        Set the Non-uniform Spacing Receptor Network       ---   CALL XYPNTS
         CALL XYPNTS
         IXYSET = IXYSET + 1
      ELSE IF (KTYPE .EQ. 'ELEV') THEN
C        Read in and set the Terrain Elevation              ---   CALL TERHGT
         CALL TERHGT
         IEVSET = IEVSET + 1
      ELSE IF (KTYPE .EQ. 'FLAG') THEN
C        Read in and set the Flagpole Receptor              ---   CALL FLGHGT
         CALL FLGHGT
         IFGSET = IFGSET + 1
      ELSE IF (KTYPE .EQ. 'END') THEN
         IEND = .TRUE.
C        Get The Final Results
         IF (.NOT. ISTA) THEN
C           Write Error: MISSING STA OF THE BLOCK DATA
            CALL ERRHDL(PATH,MODNAM,'E','200','STA')
         ELSE IF (.NOT. RECERR) THEN
            CALL SETCAR
         END IF
         ISTA = .FALSE.
         NEWID = .TRUE.
C        Check If The Secondary Parameter Has Been Specified
         IF (IXYSET.EQ.0 .AND. INCSET.EQ.0) THEN
C           WRITE Error Message: Missing (X,Y) Point Setting
            CALL ERRHDL(PATH,MODNAM,'E','212',NETIDT)
         END IF

C        Warning: Elevated Terrain Inputs Inconsistent With Options
         IF (ELEV .AND. IEVSET.EQ.0) THEN
            CALL ERRHDL(PATH,MODNAM,'W','214',NETIDT)
            IRZE = IRXR
         ELSE IF (FLAT .AND. IEVSET.NE.0) THEN
            CALL ERRHDL(PATH,MODNAM,'W','213',NETIDT)
            IRZE = IRXR
         ELSE IF (FLAT .AND. IEVSET.EQ.0) THEN
            IRZE = IRXR
         END IF

C        Warning: Flagpole Receptor Inputs Inconsistent With Options
         IF (FLGPOL .AND. IFGSET.EQ.0) THEN
            CALL ERRHDL(PATH,MODNAM,'W','216',NETIDT)
            IRZF = IRXR
         ELSE IF (.NOT.FLGPOL .AND. IFGSET.NE.0) THEN
            CALL ERRHDL(PATH,MODNAM,'W','215',NETIDT)
            IRZF = IRXR
         ELSE IF (.NOT.FLGPOL .AND. IFGSET.EQ.0) THEN
            IRZF = IRXR
         END IF

C        Check If The Number of Elev & Flag Is Match
         IF (ELEV .AND. IEVSET.NE.0) THEN
            IF (ICOUNT*JCOUNT .NE. IZE) THEN
C              Write Out The Error Message: No. Of ELEV not match
               CALL ERRHDL(PATH,MODNAM,'E','218','ELEV')
            END IF
         END IF
         IF (FLGPOL .AND. IFGSET.NE.0) THEN
            IF (ICOUNT*JCOUNT .NE. IZF) THEN
C              Write Out The Error Message: No. Of FLAG not match
               CALL ERRHDL(PATH,MODNAM,'E','218','FLAG')
            END IF
         END IF

      ELSE
C        Error Message: Invalid Secondary Keyword
         CALL ERRHDL(PATH,MODNAM,'E','170',NETIDT)
         RECERR = .TRUE.
         GO TO 999

      END IF

      PNETID = NETIDT

 999  RETURN
      END

      SUBROUTINE GENCAR
C***********************************************************************
C                 GENCAR Module of ISC2 Model
C
C        PURPOSE: Generates Cartesian Grid Receptor Network With
C                 Uniform Spacing
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Cartesian Grid Receptor Network With Uniform
C                 Spacing
C
C        CALLED FROM:   RECART
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      DIMENSION TEMPP(6)
      LOGICAL ERROR

C     Variable Initializations
      MODNAM = 'GENCAR'
      ERROR = .FALSE.

C     Check for Location of Secondary Keyword, XYINC
      DO 41 I = 1, IFC
         IF (FIELD(I) .EQ. 'XYINC') THEN
            ISC = I + 1
         END IF
 41   CONTINUE

C     Determine Whether There Are Enough Parameter Fields
      IF (IFC .EQ. ISC-1) THEN
C        Error Message: Missing Parameter
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         RECERR = .TRUE.
         GO TO 999
      ELSE IF (IFC .GT. ISC+5) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KTYPE)
         RECERR = .TRUE.
         GO TO 999
      ELSE IF (IFC .LT. ISC+5) THEN
C        Error Message: Too Few Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KTYPE)
         RECERR = .TRUE.
         GO TO 999
      END IF

C     Input The Numerical Values
      DO 21 K = 1,6
         CALL STONUM(FIELD(ISC + K-1),40,TEMPP(K),MITL)
C        Check The Numerical Field
         IF (MITL .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ERROR = .TRUE.
            RECERR = .TRUE.
         END IF
 21   CONTINUE

      IF (ERROR) THEN
         ERROR = .FALSE.
         GO TO 999
      END IF

C     Assign Values to Appropriate Variables for Generated Network
      XINT   = TEMPP(1)
      ICOUNT = INT(TEMPP(2))
      XDELTA = TEMPP(3)
      YINT   = TEMPP(4)
      JCOUNT = INT(TEMPP(5))
      YDELTA = TEMPP(6)

C     Assign Them to the Coordinate Arrays
      IF (ICOUNT .LE. IXM) THEN
         DO 30 I = 1, ICOUNT
            XCOORD(I,INNET) = XINT + XDELTA*FLOAT(I-1)
 30      CONTINUE
      ELSE
C        WRITE Error Message:  Too Many X-Coordinates for This Network
         WRITE(DUMMY,'(I8)') IXM
         CALL ERRHDL(PATH,MODNAM,'E','225',DUMMY)
         RECERR = .TRUE.
      END IF
      IF (JCOUNT .LE. IYM) THEN
         DO 40 J = 1, JCOUNT
            YCOORD(J,INNET) = YINT + YDELTA*FLOAT(J-1)
 40      CONTINUE
      ELSE
C        WRITE Error Message:  Too Many Y-Coordinates for This Network
         WRITE(DUMMY,'(I8)') IYM
         CALL ERRHDL(PATH,MODNAM,'E','226',DUMMY)
         RECERR = .TRUE.
      END IF

 999  RETURN
      END

      SUBROUTINE XYPNTS
C***********************************************************************
C                 XYPNTS Module of ISC2 Model
C
C        PURPOSE: Processes Cartesian Grid x,y Input Value
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To Fix Error Checking - Change Limit for DO 15
C                    To 'JSET -1' - 9/29/92
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Cartesian Grid x,y Input Value
C
C        CALLED FROM:   RECART
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'XYPNTS'

      IF (KTYPE .EQ. 'XPNTS') THEN
C        Check for Location of Secondary Keyword, XPNTS
         DO 41 I = 1, IFC
            IF (FIELD(I) .EQ. 'XPNTS') THEN
               ISC = I + 1
            END IF
 41      CONTINUE

C        Determine Whether There Are Enough Parameter Fields
         IF (IFC .EQ. ISC-1) THEN
C           Error Message: Missing Parameter
            CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
            RECERR = .TRUE.
            GO TO 999
         END IF

         ISET = ICOUNT
         DO 20 I = ISC, IFC
            CALL STONUM(FIELD(I),40,FNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               RECERR = .TRUE.
            END IF
            ISET = ISET + 1
            IF (ISET .LE. IXM) THEN
               XCOORD(ISET,INNET) = FNUM
               DO 10 J = 1, ISET-1
                  IF (FNUM .EQ. XCOORD(J,INNET)) THEN
C                    WRITE Warning Message:  X-Coord Specified More Than Once
                     CALL ERRHDL(PATH,MODNAM,'W','250',NETIDT)
                  END IF
 10            CONTINUE
            ELSE
C              WRITE Error Message:  Too Many X-Coordinates for This Network
               WRITE(DUMMY,'(I8)') IXM
               CALL ERRHDL(PATH,MODNAM,'E','225',DUMMY)
               RECERR = .TRUE.
            END IF
 20      CONTINUE
         ICOUNT = ISET

      ELSE IF (KTYPE .EQ. 'YPNTS') THEN
C        Check for Location of Secondary Keyword, YPNTS
         DO 42 I = 1, IFC
            IF (FIELD(I) .EQ. 'YPNTS') THEN
               ISC = I + 1
            END IF
 42      CONTINUE

C        Determine Whether There Are Enough Parameter Fields
         IF (IFC .EQ. ISC-1) THEN
C           Error Message: Missing Parameter
            CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
            RECERR = .TRUE.
            GO TO 999
         END IF

         JSET = JCOUNT

         DO 25 I = ISC, IFC
            CALL STONUM(FIELD(I),40,FNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               RECERR = .TRUE.
            END IF
            JSET = JSET + 1
            IF (JSET .LE. IYM) THEN
               YCOORD(JSET,INNET) = FNUM
               DO 15 J = 1, JSET-1
                  IF (FNUM .EQ. YCOORD(J,INNET)) THEN
C                    WRITE Warning Message:  Y-Coord Specified More Than Once
                     CALL ERRHDL(PATH,MODNAM,'W','250',NETIDT)
                  END IF
 15            CONTINUE
            ELSE
C              WRITE Error Message:  Too Many Y-Coordinates for This Network
               WRITE(DUMMY,'(I8)') IYM
               CALL ERRHDL(PATH,MODNAM,'E','226',DUMMY)
               RECERR = .TRUE.
            END IF
 25      CONTINUE
         JCOUNT = JSET
      END IF

 999  RETURN
      END

      SUBROUTINE SETCAR
C***********************************************************************
C                 SETCAR Module of ISC2 Model
C
C        PURPOSE: Setup the Final Cartesian Grid Receptor Network Inputs
C
C        PROGRAMMER:  Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  To Include TOXXFILE Option - 9/29/92
C
C        INPUTS:  The GRIDCART Sub-pathway Input Parameters
C
C        OUTPUTS: Cartesian Grid Receptor Network Inputs
C
C        CALLED FROM:   RECART
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'SETCAR'

      IF (ICOUNT.NE.0 .AND. JCOUNT.NE.0) THEN
C        Setup The Coordinate Of The Receptors
         NETSTA(INNET) = IRXR + 1
         ISET = IRXR
         JSET = IRYR
         DO 25 J = 1, JCOUNT
            DO 20 I = 1, ICOUNT
               ISET = ISET + 1
               JSET = JSET + 1
               IF (ISET .GT. NREC) THEN
C                 Error Msg: Maximum Number Of Receptor Exceeded
                  WRITE(DUMMY,'(I8)') NREC
                  CALL ERRHDL(PATH,MODNAM,'E','219',DUMMY)
                  GO TO 999
               END IF
               IF (ICOUNT .GT. IXM) THEN
C                 WRITE Error Message:  Too Many X-Coordinates for This Network
                  WRITE(DUMMY,'(I8)') IXM
                  CALL ERRHDL(PATH,MODNAM,'E','225',DUMMY)
                  GO TO 999
               END IF
               IF (JCOUNT .GT. IYM) THEN
C                 WRITE Error Message:  Too Many Y-Coordinates for This Network
                  WRITE(DUMMY,'(I8)') IYM
                  CALL ERRHDL(PATH,MODNAM,'E','226',DUMMY)
                  GO TO 999
               END IF
               AXR(ISET) = XCOORD(I,INNET)
               AYR(JSET) = YCOORD(J,INNET)
 20         CONTINUE
 25      CONTINUE
         IRXR = ISET
         IRYR = JSET
         NETEND(INNET) = IRXR
         NUMXPT(INNET) = ICOUNT
         NUMYPT(INNET) = JCOUNT
         NTID(INNET)   = NETIDT
         NTTYP(INNET)  = 'GRIDCART'
C        Define ITAB, NXTOX, NYTOX Variables for TOXXFILE Option, 9/29/92
         IF (ITAB .LT. 0) THEN
C           First Receptor Network Defined - Set Variables
            ITAB  = 2
            NXTOX = ICOUNT
            NYTOX = JCOUNT
         ELSE
C           Previous Receptors Have Been Defined - Reset ITAB = 0
            ITAB = 0
         END IF
      END IF

C     Setup The AZELEV Array
      CALL SBYVAL(ZETMP1,ZETMP2,IZE)
      ISET = IRZE
      DO 30 I = 1, IZE
         ISET = ISET + 1
         IF (ISET .GT. NREC) THEN
C           Error Msg: Maximum Number Of Receptor Exceeded
            WRITE(DUMMY,'(I8)') NREC
            CALL ERRHDL(PATH,MODNAM,'E','219',DUMMY)
            GO TO 999
         END IF
         AZELEV(ISET) = ZETMP2(I)
 30   CONTINUE
      IRZE = ISET

C     Setup The AZFLAG Aarry
      CALL SBYVAL(ZFTMP1,ZFTMP2,IZF)
      ISET = IRZF
      DO 35 I = 1, IZF
         ISET = ISET + 1
         IF (ISET .GT. NREC) THEN
C           Error Msg: Maximum Number Of Receptor Exceeded
            WRITE(DUMMY,'(I8)') NREC
            CALL ERRHDL(PATH,MODNAM,'E','219',DUMMY)
            GO TO 999
         END IF
         AZFLAG(ISET) = ZFTMP2(I)
 35   CONTINUE
      IRZF = ISET

      DO 40 I = IDC1+1, IRXR
         NETID(I) = NETIDT
         RECTYP(I) = 'GC'
 40   CONTINUE

 999  RETURN
      END

      SUBROUTINE REPOLR
C***********************************************************************
C                 REPOLR Module of ISC2 Model
C
C        PURPOSE: Processes Polar Grid Receptor Network Inputs
C
C        PROGRAMMER:  Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Polar Receptor Network Inputs
C
C        CALLED FROM:   RECARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'REPOLR'

      IF (IFC .LT. 3) THEN
C        Write Error Message: Missing Data Field
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      END IF

C     READ in the Netid and Nettype
      NETIDT = FIELD(3)
      IF (.NOT.NEWID .AND. (NETIDT.EQ.'    ' .OR.
     &    NETIDT.EQ.'ORIG' .OR. NETIDT.EQ.'DIST' .OR.
     &    NETIDT.EQ.'DDIR' .OR. NETIDT.EQ.'ELEV' .OR.
     &    NETIDT.EQ.'FLAG' .OR. NETIDT.EQ.'GDIR' .OR.
     &    NETIDT.EQ.'END')) THEN
         NETIDT = PNETID
         KTYPE = FIELD(3)
      ELSE IF (.NOT.NEWID .AND. NETIDT.EQ.PNETID) THEN
         KTYPE = FIELD(4)
      ELSE IF (NEWID .AND. NETIDT.NE.'    ') THEN
         NEWID = .FALSE.
         KTYPE = FIELD(4)
C        The Keyword Counter
         INNET = INNET + 1
         IF (INNET .GT. NNET) THEN
C           WRITE Error Message:  Too Many Networks
            WRITE(DUMMY,'(I8)') NNET
            CALL ERRHDL(PATH,MODNAM,'E','224',DUMMY)
            RECERR = .TRUE.
            GO TO 999
         END IF
         IORSET = 0
         IXRSET = 0
         IDRSET = 0
         IGRSET = 0
         IEVSET = 0
         IFGSET = 0
      ELSE
C        Error Message: Invalid Secondary Keyword
         CALL ERRHDL(PATH,MODNAM,'E','170',PNETID)
         RECERR = .TRUE.
         GO TO 999
      END IF

C     Start to Set Up the Network
      IF (KTYPE .EQ. 'STA') THEN
         ISTA = .TRUE.
         IEND = .FALSE.
         NEWID = .FALSE.
         RECERR = .FALSE.
         ICOUNT = 0
         JCOUNT = 0
         IZE = 0
         IZF = 0
         IDC1 = IRXR
C        Check for Previous Grid Network With Same ID
         DO 100 I = 1, INNET-1
            IF (FIELD(3) .EQ. NTID(I)) THEN
C              WRITE Warning Message:  Duplicate Network ID
               CALL ERRHDL(PATH,MODNAM,'W','252',NTID(I))
            END IF
 100     CONTINUE
      ELSE IF (KTYPE .EQ. 'ORIG') THEN
C        Error Message: Conflict Secondary Keyword
         IF (IORSET .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','160',NETIDT)
         END IF
C        Read In XINT, YINT                                 ---   CALL POLORG
         CALL POLORG
         IORSET = IORSET + 1
      ELSE IF (KTYPE .EQ. 'DIST') THEN
C        Read in the Distance Set                           ---   CALL POLDST
         CALL POLDST
         IXRSET = IXRSET + 1
      ELSE IF (KTYPE .EQ. 'GDIR') THEN
C        Error Message: Conflict Secondary Keyword
         IF (IDRSET .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','180',NETIDT)
         END IF
C        Set the Uniform Spacing Receptor Network           ---   CALL GENPOL
         CALL GENPOL
         IGRSET = IGRSET + 1
      ELSE IF (KTYPE .EQ. 'DDIR') THEN
C        Error Message: Conflict Secondary Keyword
         IF (IGRSET .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','180',NETIDT)
         END IF
C        Set the Non-uniform Spacing Receptor Network       ---   CALL RADRNG
         CALL RADRNG
         IDRSET = IDRSET + 1
      ELSE IF (KTYPE .EQ. 'ELEV') THEN
C        Read in and set the Terrain Elevation              ---   CALL TERHGT
         CALL TERHGT
         IEVSET = IEVSET + 1
      ELSE IF (KTYPE .EQ. 'FLAG') THEN
C        Read in and set the Flagpole Receptor              ---   CALL FLGHGT
         CALL FLGHGT
         IFGSET = IFGSET + 1
      ELSE IF (KTYPE .EQ. 'END') THEN
         IEND = .TRUE.
C        Get the Final Result
         IF (.NOT. ISTA) THEN
C           Write Error: MISSING STA OF THE BLOCK DATA
            CALL ERRHDL(PATH,MODNAM,'E','200','STA')
         ELSE IF (.NOT. RECERR) THEN
            CALL SETPOL
         END IF
         ISTA = .FALSE.
         NEWID = .TRUE.
C        Check If The Secondary Parameter Has Been Specified
C        Warning Message: Missing (Xin,Yin) Point Setting
         IF (IORSET .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'W','220',NETIDT)
            XINT = 0.0
            YINT = 0.0
         END IF
C        Error Message: Missing Distance Point Setting
         IF (IXRSET .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','221',NETIDT)
         END IF
C        Error Message: Missing Degree Or Rad Setting
         IF (IGRSET.EQ.0 .AND. IDRSET.EQ.0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','222',NETIDT)
         END IF

C        Warning: Elevated Terrain Inputs Inconsistent With Options
         IF (ELEV .AND. IEVSET.EQ.0) THEN
            CALL ERRHDL(PATH,MODNAM,'W','214',NETIDT)
            IRZE = IRXR
         ELSE IF (FLAT .AND. IEVSET.NE.0) THEN
            CALL ERRHDL(PATH,MODNAM,'W','213',NETIDT)
            IRZE = IRXR
         ELSE IF (FLAT .AND. IEVSET.EQ.0) THEN
            IRZE = IRXR
         END IF

C        Warning: Flagpole Receptor Inputs Inconsistent With Options
         IF (FLGPOL .AND. IFGSET.EQ.0) THEN
            CALL ERRHDL(PATH,MODNAM,'W','216',NETIDT)
            IRZF = IRXR
         ELSE IF (.NOT.FLGPOL .AND. IFGSET.NE.0) THEN
            CALL ERRHDL(PATH,MODNAM,'W','215',NETIDT)
            IRZF = IRXR
         ELSE IF (.NOT.FLGPOL .AND. IFGSET.EQ.0) THEN
            IRZF = IRXR
         END IF

C        Check If The Number of Elev & Flag Is Match
         IF (ELEV .AND. IEVSET.NE.0) THEN
            IF (ICOUNT*JCOUNT .NE. IZE) THEN
C              Write Out The Error Message: No. Of ELEV not match
               CALL ERRHDL(PATH,MODNAM,'E','218','ELEV')
            END IF
         END IF
         IF (FLGPOL .AND. IFGSET.NE.0) THEN
            IF (ICOUNT*JCOUNT .NE. IZF) THEN
C              Write Out The Error Message: No. Of FLAG not match
               CALL ERRHDL(PATH,MODNAM,'E','218','FLAG')
            END IF
         END IF

      ELSE
C        Error Message: Invalid Secondary Keyword
         CALL ERRHDL(PATH,MODNAM,'E','170',NETIDT)
         RECERR = .TRUE.
         GO TO 999

      END IF

      PNETID = NETIDT

 999  RETURN
      END

      SUBROUTINE POLORG
C***********************************************************************
C                 POLORG Module of ISC2 Model
C
C        PURPOSE: Input The Original of The Polar Network
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Polar Network Origin  Coordinates
C
C        CALLED FROM:   REPOLR
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER SOID*8
      LOGICAL FIND

C     Variable Initializations
      MODNAM = 'POLORG'
      FIND = .FALSE.

C     Check for the Location of the Secondary Keyword, ORIG
      DO 41 I = 1, IFC
         IF (FIELD(I) .EQ. 'ORIG') THEN
            ISC = I + 1
         END IF
 41   CONTINUE

C     Determine Whether There Are Enough Parameter Fields
      IF (IFC .EQ. ISC-1) THEN
C        Error Message: Missing Parameter
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         RECERR = .TRUE.
         GO TO 999
      ELSE IF (IFC .GT. ISC+1) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KTYPE)
         RECERR = .TRUE.
         GO TO 999
      END IF

      IF (IFC .EQ. ISC) THEN
C        Identify Origin Associated With a Source ID
         SOID = FIELD(ISC)
         CALL SINDEX(SRCID,NSRC,SOID,ISDX,FIND)
         IF (.NOT. FIND) THEN
C           Error Message: Source ID Does Not Match Existing Sources
            CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
            RECERR = .TRUE.
         ELSE
            XINT = AXS(ISDX)
            YINT = AYS(ISDX)
         END IF

      ELSE
C        Input Numerical Values, XINT and YINT
         CALL STONUM(FIELD(ISC),40,XINT,IMUT)
C        Check The Numerical Field
         IF (IMUT .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            RECERR = .TRUE.
         END IF

         CALL STONUM(FIELD(ISC + 1),40,YINT,IMUT)
C        Check The Numerical Field
         IF (IMUT .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            RECERR = .TRUE.
         END IF
      END IF

 999  RETURN
      END

      SUBROUTINE POLDST
C***********************************************************************
C                 POLDST Module of ISC2 Model
C
C        PURPOSE: Gets Distances for the Polar Network
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Polar Network Distance Input Value
C
C        CALLED FROM:   REPOLR
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'POLDST'

C     Skip the Unrelated Fields
      DO 41 I = 1, IFC
         IF (FIELD(I) .EQ. 'DIST') THEN
            ISC = I + 1
         END IF
 41   CONTINUE

C     Determine Whether There Are Enough Parameter Fields
      IF (IFC .EQ. ISC-1) THEN
C        Error Message: Missing Parameter
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         RECERR = .TRUE.
         GO TO 999
      END IF

      ISET = ICOUNT

      DO 20 I = ISC, IFC
         CALL STONUM(FIELD(I),40,FNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            RECERR = .TRUE.
         END IF
         ISET = ISET + 1
         IF (ISET .LE. IXM) THEN
C           Store Distance to XCOORD Array and Check for Previous Occurrence
            XCOORD(ISET,INNET) = FNUM
            DO 10 J = 1, ISET-1
               IF (FNUM .EQ. XCOORD(J,INNET)) THEN
C                 WRITE Warning Message:  Distance Specified More Than Once
                  CALL ERRHDL(PATH,MODNAM,'W','250',NETIDT)
               END IF
 10         CONTINUE
         ELSE
C           WRITE Error Message:  Too Many X-Coordinates for This Network
            WRITE(DUMMY,'(I8)') IXM
            CALL ERRHDL(PATH,MODNAM,'E','225',DUMMY)
            RECERR = .TRUE.
         END IF
 20   CONTINUE

      ICOUNT = ISET

 999  RETURN
      END

      SUBROUTINE GENPOL
C***********************************************************************
C                 GENPOL Module of ISC2 Model
C
C        PURPOSE: Generates Polar Receptor Network With
C                 Uniform Spacing
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Polar Receptor Network With Uniform Direction Spacing
C
C        CALLED FROM:   REPOLR
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      DIMENSION TEMPP(3)
      LOGICAL ERROR

C     Variable Initializations
      MODNAM = 'GENPOL'
      ERROR = .FALSE.

C     Check for the Location of the Secondary Keyword, GDIR
      DO 41 I = 1, IFC
         IF (FIELD(I) .EQ. 'GDIR') THEN
            ISC = I + 1
         END IF
 41   CONTINUE

C     Determine Whether There Are Enough Parameter Fields
      IF (IFC .EQ. ISC-1) THEN
C        Error Message: Missing Parameter
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         RECERR = .TRUE.
         GO TO 999
      ELSE IF (IFC .LT. ISC+2) THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KTYPE)
         RECERR = .TRUE.
         GO TO 999
      ELSE IF (IFC .GT. ISC+2) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KTYPE)
         RECERR = .TRUE.
         GO TO 999
      END IF

C     Input Numerical Values
      DO 21 K = 1, 3
         CALL STONUM(FIELD(ISC + K-1),40,TEMPP(K),MITL)
C        Check The Numerical Field
         IF (MITL .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            RECERR = .TRUE.
            ERROR = .TRUE.
         END IF
 21   CONTINUE

      IF (ERROR) THEN
         ERROR = .FALSE.
         GO TO 999
      END IF

      JCOUNT = INT(TEMPP(1))
      DIRINI = TEMPP(2)
      DIRINC = TEMPP(3)

C     Assign Them to the Coordinate Arrays
      IF (JCOUNT .LE. IYM) THEN
         DO 20 J = 1, JCOUNT
            YCOORD(J,INNET) = (DIRINI + DIRINC*FLOAT(J-1))
            IF (YCOORD(J,INNET) .GT. 360.) THEN
               YCOORD(J,INNET) = YCOORD(J,INNET) - 360.
            ELSE IF (YCOORD(J,INNET) .LE. 0.) THEN
               YCOORD(J,INNET) = YCOORD(J,INNET) + 360.
            END IF
 20      CONTINUE
      ELSE
C        WRITE Error Message:  Too Many Y-Coordinates for This Network
         WRITE(DUMMY,'(I8)') IYM
         CALL ERRHDL(PATH,MODNAM,'E','226',DUMMY)
         RECERR = .TRUE.
      END IF

 999  RETURN
      END

      SUBROUTINE RADRNG
C***********************************************************************
C                 RADRNG Module of ISC2 Model
C
C        PURPOSE: Processes Non-Uniform Polar Network Value
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Polar Network Directions in Non-Uniform Spacing
C
C        CALLED FROM:   REPOLR
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'RADRNG'

C     Skip the non-useful Fields
      DO 41 I = 1, IFC
         IF (FIELD(I) .EQ. 'DDIR') THEN
            ISC = I + 1
         END IF
 41   CONTINUE

C     Determine Whether There Are Enough Parameter Fields
      IF (IFC .EQ. ISC-1) THEN
C        Error Message: Missing Parameter
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         RECERR = .TRUE.
         GO TO 999
      END IF

      ISET = JCOUNT

      DO 20 I = ISC, IFC
         CALL STONUM(FIELD(I),40,FNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            RECERR = .TRUE.
         END IF
         ISET = ISET + 1
         IF (ISET .LE. IYM) THEN
C           Store Direction to YCOORD Array, Adjust to 0-360 Range if Needed,
C           and Check for Previous Occurrence
            YCOORD(ISET,INNET) = FNUM
            IF (YCOORD(ISET,INNET) .GT. 360.) THEN
               YCOORD(ISET,INNET) = YCOORD(ISET,INNET) - 360.
            ELSE IF (YCOORD(ISET,INNET) .LE. 0.) THEN
               YCOORD(ISET,INNET) = YCOORD(ISET,INNET) + 360.
            END IF
            DO 10 J = 1, ISET-1
               IF (FNUM .EQ. YCOORD(J,INNET)) THEN
C                 WRITE Warning Message:  Direction Specified More Than Once
                  CALL ERRHDL(PATH,MODNAM,'W','250',NETIDT)
               END IF
 10         CONTINUE
         ELSE
C           WRITE Error Message:  Too Many Y-Coordinates for This Network
            WRITE(DUMMY,'(I8)') IYM
            CALL ERRHDL(PATH,MODNAM,'E','226',DUMMY)
            RECERR = .TRUE.
         END IF
 20   CONTINUE

      JCOUNT = ISET

 999  RETURN
      END

      SUBROUTINE SETPOL
C***********************************************************************
C                 SETPOL Module of ISC2 Model
C
C        PURPOSE: Setup the Final Polar Receptor Network Inputs
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  To Include TOXXFILE Option - 9/29/92
C
C        INPUTS:  The GRIDPOLR Sub-pathway Input Parameters
C
C        OUTPUTS: Polar Receptor Network Arrays
C
C        CALLED FROM:   REPOLR
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'SETPOL'

      IF (ICOUNT.NE.0 .AND. JCOUNT.NE.0) THEN
C        Setup The Coordinate Of The Receptors
         NETSTA(INNET) = IRXR + 1
         ISET = IRXR
         JSET = IRYR
         DO 25 J = 1, JCOUNT
            DO 20 I = 1, ICOUNT
               ISET = ISET + 1
               JSET = JSET + 1
               IF (ISET .GT. NREC) THEN
C                 Error Msg: Maximum Number Of Receptor Exceeded
                  WRITE(DUMMY,'(I8)') NREC
                  CALL ERRHDL(PATH,MODNAM,'E','219',DUMMY)
                  GO TO 999
               END IF
               IF (ICOUNT .GT. IXM) THEN
C                 WRITE Error Message:  Too Many X-Coordinates for This Network
                  WRITE(DUMMY,'(I8)') IXM
                  CALL ERRHDL(PATH,MODNAM,'E','225',DUMMY)
                  GO TO 999
               END IF
               IF (JCOUNT .GT. IYM) THEN
C                 WRITE Error Message:  Too Many Y-Coordinates for This Network
                  WRITE(DUMMY,'(I8)') IYM
                  CALL ERRHDL(PATH,MODNAM,'E','226',DUMMY)
                  GO TO 999
               END IF
               YTEMP = YCOORD(J,INNET) * DTORAD
               AXR(ISET) = XINT + XCOORD(I,INNET)*SIN(YTEMP)
               AYR(JSET) = YINT + XCOORD(I,INNET)*COS(YTEMP)
 20         CONTINUE
 25      CONTINUE
         IRXR = ISET
         IRYR = JSET
         XORIG(INNET)  = XINT
         YORIG(INNET)  = YINT
         NETEND(INNET) = IRXR
         NUMXPT(INNET) = ICOUNT
         NUMYPT(INNET) = JCOUNT
         NTID(INNET)   = NETIDT
         NTTYP(INNET)  = 'GRIDPOLR'
C        Define ITAB, NXTOX, NYTOX Variables for TOXXFILE Option, 9/29/92
         IF (ITAB .LT. 0) THEN
C           First Receptor Network Defined - Set Variables
            ITAB  = 1
            NXTOX = ICOUNT
            NYTOX = JCOUNT
         ELSE
C           Previous Receptors Have Been Defined - Reset ITAB = 0
            ITAB = 0
         END IF
      END IF

C     Setup The AZELEV Array
      CALL SBYVAL(ZETMP1,ZETMP2,IZE)
      ISET = IRZE
      DO 30 I = 1, IZE
         ISET = ISET + 1
         IF (ISET .GT. NREC) THEN
C           Error Msg: Maximum Number Of Receptor Exceeded
            WRITE(DUMMY,'(I8)') NREC
            CALL ERRHDL(PATH,MODNAM,'E','219',DUMMY)
            GO TO 999
         END IF
         AZELEV(ISET) = ZETMP2(I)
 30   CONTINUE
      IRZE = ISET

C     Setup The AZFLAG Array
      CALL SBYVAL(ZFTMP1,ZFTMP2,IZF)
      ISET = IRZF
      DO 35 I = 1, IZF
         ISET = ISET + 1
         IF (ISET .GT. NREC) THEN
C           Error Msg: Maximum Number Of Receptor Exceeded
            WRITE(DUMMY,'(I8)') NREC
            CALL ERRHDL(PATH,MODNAM,'E','219',DUMMY)
            GO TO 999
         END IF
         AZFLAG(ISET) = ZFTMP2(I)
 35   CONTINUE
      IRZF = ISET

      DO 40 I = IDC1+1, IRXR
         NETID(I) = NETIDT
         RECTYP(I) = 'GP'
 40   CONTINUE

 999  RETURN
      END

      SUBROUTINE TERHGT
C***********************************************************************
C                 TERHGT Module of ISC2 Model
C
C        PURPOSE: Processes Elevated Terrain Inputs for Receptor Network
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Elevated Terrain Input for a Receptor Network
C
C        CALLED FROM:   RECART
C                       REPOLR
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'TERHGT'
      IZE1 = IZE + 1

C     Check for the Location of the Secondary Keyword, ELEV
      DO 10 I = 1, IFC
         IF (FIELD(I) .EQ. 'ELEV') THEN
            ISC = I + 1
         END IF
 10   CONTINUE

C     Determine Whether There Are Enough Parameter Fields
      IF (IFC .EQ. ISC-1) THEN
C        Error Message: Missing Parameter
         CALL ERRHDL(PATH,MODNAM,'E','223',KTYPE)
         RECERR = .TRUE.
         GO TO 999
      ELSE IF (IFC .EQ. ISC) THEN
C        Error Message: Missing Numerical Field
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         RECERR = .TRUE.
         GO TO 999
      END IF

      CALL STONUM(FIELD(ISC),40,FNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .EQ. -1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         RECERR = .TRUE.
      END IF
      ROW = FNUM

      ISET = IZE

      DO 25 I = ISC+1, IFC
         CALL STONUM(FIELD(I),40,FNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            RECERR = .TRUE.
         END IF
         DO 20 J = 1, IMIT
            ISET = ISET + 1
            ZETMP1(ISET) = ROW
            ZETMP2(ISET) = FNUM
 20      CONTINUE
 25   CONTINUE

      IZE = ISET

      IF (ELTYPE .EQ. 'FEET' .OR. REELEV .EQ. 'FEET') THEN
C        Convert ELEV to Metric System
         DO 50 I = IZE1, IZE
            ZETMP2(I) = 0.3048*ZETMP2(I)
 50      CONTINUE
      END IF

 999  RETURN
      END

      SUBROUTINE FLGHGT
C***********************************************************************
C                 FLGHGT Module of ISC2 Model
C
C        PURPOSE: Processes Flagpole Receptor Heights for Receptor Network
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Flagpole Receptor Heights for a Receptor Network
C
C        CALLED FROM:   RECART
C                       REPOLR
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'FLGHGT'

C     Check for the Location of the Secondary Keyword, FLAG
      DO 10 I = 1, IFC
         IF (FIELD(I) .EQ. 'FLAG') THEN
            ISC = I + 1
         END IF
 10   CONTINUE

C     Determine Whether There Are Enough Parameter Fields
      IF (IFC .EQ. ISC-1) THEN
C        Error Message: Missing Parameter
         CALL ERRHDL(PATH,MODNAM,'E','223',KTYPE)
         RECERR = .TRUE.
         GO TO 999
      ELSE IF (IFC .EQ. ISC) THEN
C        Error Message: Missing Numerical Field
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         RECERR = .TRUE.
         GO TO 999
      END IF

      CALL STONUM(FIELD(ISC),40,FNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .EQ. -1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         RECERR = .TRUE.
      END IF
      ROW = FNUM

      ISET = IZF

      DO 25 I = ISC+1, IFC
         CALL STONUM(FIELD(I),40,FNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            RECERR = .TRUE.
         END IF
         DO 20 J = 1, IMIT
            ISET = ISET + 1
            ZFTMP1(ISET) = ROW
            ZFTMP2(ISET) = FNUM
 20      CONTINUE
 25   CONTINUE

      IZF = ISET

 999  RETURN
      END

      SUBROUTINE DISCAR
C***********************************************************************
C                 DISCAR Module of ISC2 Model
C
C        PURPOSE: Processes Discrete Cartesian Receptor Location Inputs
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  To Include TOXXFILE Option - 9/29/92
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Discrete Cartesian Receptor Location Inputs
C
C        CALLED FROM:   RECARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'DISCAR'
      I1 = IRXR
      I2 = IRYR
      I3 = IRZE
      I4 = IRZF

C     Determine Whether There Are Too Few Or Too Many Parameter Fields
      IF (IFC .LT. 4) THEN
C        WRITE Error Message: Missing Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 6) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      ELSE IF (ELEV .AND. FLGPOL .AND. IFC.LT.6) THEN
C        WRITE Warning Message: Default(s) Used for Missing Parameter(s)
         CALL ERRHDL(PATH,MODNAM,'W','228',KEYWRD)
      ELSE IF ((ELEV .OR. FLGPOL) .AND. IFC.LT.5) THEN
C        WRITE Warning Message: Default(s) Used for Missing Parameter(s)
         CALL ERRHDL(PATH,MODNAM,'W','228',KEYWRD)
      ELSE IF (ELEV .AND. .NOT.FLGPOL .AND. IFC .GT. 5) THEN
C        WRITE Warning Message: Parameter Ignored, ZFLAG
         CALL ERRHDL(PATH,MODNAM,'W','229',KEYWRD)
      ELSE IF (FLGPOL .AND. .NOT.ELEV .AND. IFC .GT. 5) THEN
C        WRITE Warning Message: Parameter Ignored, ZELEV
         CALL ERRHDL(PATH,MODNAM,'W','229',KEYWRD)
      ELSE IF (.NOT.ELEV .AND. .NOT.FLGPOL .AND. IFC .GT. 4) THEN
C        WRITE Warning Message: Parameters Ignored, ZELEV & ZFLAG
         CALL ERRHDL(PATH,MODNAM,'W','229',KEYWRD)
      END IF

C     Check Whether The Maximum Number of Receptors is Exceeded
      IF (I1.EQ.NREC .OR. I2.EQ.NREC .OR. I3.EQ.NREC .OR.
     &                                    I4.EQ.NREC) THEN
C        Error Msg: Maximum Number Of Receptors Exceeded
         WRITE(DUMMY,'(I8)') NREC
         CALL ERRHDL(PATH,MODNAM,'E','219',DUMMY)
         GO TO 999
      END IF

C     READ XCOORD,YCOORD,ELEV,FLAG And Assign Them to Different Arrays

      CALL STONUM(FIELD(3),40,FNUM,INUM)
C     Check The Numerical Field
      IF (INUM .EQ. -1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      ELSE
         AXR(I1 + 1) = FNUM
      END IF

      CALL STONUM(FIELD(4),40,FNUM,INUM)
C     Check The Numerical Field
      IF (INUM .EQ. -1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      ELSE
         AYR(I2 + 1) = FNUM
      END IF

      IF (ELEV .AND. FLGPOL) THEN
         IF (IFC .GE. 5) THEN
            CALL STONUM(FIELD(5),40,FNUM,INUM)
C           Check The Numerical Field
            IF (INUM .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               AZELEV(I3 + 1) = FNUM
            END IF
         END IF
         IF (IFC .EQ. 6) THEN
            CALL STONUM(FIELD(6),40,FNUM,INUM)
C           Check The Numerical Field
            IF (INUM .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               AZFLAG(I4 + 1) = FNUM
            END IF
         END IF
      ELSE IF (ELEV .AND. .NOT.FLGPOL) THEN
         IF (IFC .GE. 5) THEN
            CALL STONUM(FIELD(5),40,FNUM,INUM)
C           Check The Numerical Field
            IF (INUM .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               AZELEV(I3 + 1) = FNUM
            END IF
         END IF
      ELSE IF (FLGPOL .AND. .NOT.ELEV) THEN
         IF (IFC .EQ. 5) THEN
            CALL STONUM(FIELD(5),40,FNUM,INUM)
C           Check The Numerical Field
            IF (INUM .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               AZFLAG(I4 + 1) = FNUM
            END IF
         ELSE IF (IFC .EQ. 6) THEN
            CALL STONUM(FIELD(6),40,FNUM,INUM)
C           Check The Numerical Field
            IF (INUM .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               AZFLAG(I4 + 1) = FNUM
            END IF
         END IF
      END IF

      IF (ELTYPE .EQ. 'FEET' .OR. REELEV .EQ. 'FEET') THEN
C        Convert ELEV to Metric system
         AZELEV(I3 + 1) = 0.3048*AZELEV(I3 + 1)
      END IF

      IRXR = I1 + 1
      IRYR = I2 + 1
      IRZE = I3 + 1
      IRZF = I4 + 1
      NETID(IRXR) = '   NA   '
      RECTYP(IRXR) = 'DC'
C     Reset ITAB Variable for TOXXFILE Option, 9/29/92
      ITAB = 0

 999  RETURN
      END

      SUBROUTINE DISPOL
C***********************************************************************
C                 DISPOL Module of ISC2 Model
C
C        PURPOSE: Processes Discrete Polar Receptor Location Inputs
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  To Include TOXXFILE Option - 9/29/92
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Discrete Polar Receptor Location Inputs
C
C        CALLED FROM:   RECARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER SOID*8
      LOGICAL FIND

C     Variable Initializations
      MODNAM = 'DISPOL'
      I1 = IRXR
      I2 = IRYR
      I3 = IRZE
      I4 = IRZF

C     Determine Whether There Are Too Few Or Too Many Parameter Fields
      IF (IFC .LT. 5) THEN
C        WRITE Error Message: Missing Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 7) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      ELSE IF (ELEV .AND. FLGPOL .AND. IFC.LT.7) THEN
C        WRITE Warning Message: Default(s) Used for Missing Parameter(s)
         CALL ERRHDL(PATH,MODNAM,'W','228',KEYWRD)
      ELSE IF ((ELEV .OR. FLGPOL) .AND. IFC.LT.6) THEN
C        WRITE Warning Message: Default(s) Used for Missing Parameter(s)
         CALL ERRHDL(PATH,MODNAM,'W','228',KEYWRD)
      ELSE IF (ELEV .AND. .NOT.FLGPOL .AND. IFC .GT. 6) THEN
C        WRITE Warning Message: Parameter Ignored, ZFLAG
         CALL ERRHDL(PATH,MODNAM,'W','229',' ZFLAG ')
      ELSE IF (FLGPOL .AND. .NOT.ELEV .AND. IFC .GT. 6) THEN
C        WRITE Error Message: Parameter Ignored, ZELEV
         CALL ERRHDL(PATH,MODNAM,'W','229',KEYWRD)
      ELSE IF (.NOT.ELEV .AND. .NOT.FLGPOL .AND. IFC .GT. 5) THEN
C        WRITE Warning Message: Parameters Ignored, ZELEV & ZFLAG
         CALL ERRHDL(PATH,MODNAM,'W','229',KEYWRD)
      END IF

C     Check Whether The Maximum Number of Receptors is Exceeded
      IF (I1.EQ.NREC .OR. I2.EQ.NREC .OR. I3.EQ.NREC .OR.
     &                                    I4.EQ.NREC) THEN
C        Error Msg: Maximum Number Of Receptors Exceeded
         WRITE(DUMMY,'(I8)') NREC
         CALL ERRHDL(PATH,MODNAM,'E','219',DUMMY)
         GO TO 999
      END IF

C     READ SRCID,RANGE,DIRECT,ELEV,FLAG

      SOID = FIELD(3)

      CALL STONUM(FIELD(4),40,RANGE,IMIT)
C     Check The Numerical Field
      IF (IMIT .EQ. -1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      END IF

      CALL STONUM(FIELD(5),40,DIRECT,IMIT)
C     Check The Numerical Field
      IF (IMIT .EQ. -1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
      ELSE IF (DIRECT .GT. 360.) THEN
         DIRECT = DIRECT - 360.
      ELSE IF (DIRECT .LE. 0.) THEN
         DIRECT = DIRECT + 360.
      END IF

      IF (ELEV .AND. FLGPOL) THEN
         IF (IFC .GE. 6) THEN
            CALL STONUM(FIELD(6),40,FNUM,INUM)
C           Check The Numerical Field
            IF (INUM .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               AZELEV(I3 + 1) = FNUM
            END IF
         END IF
         IF (IFC .EQ. 7) THEN
            CALL STONUM(FIELD(7),40,FNUM,INUM)
C           Check The Numerical Field
            IF (INUM .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               AZFLAG(I4 + 1) = FNUM
            END IF
         END IF
      ELSE IF (ELEV .AND. .NOT.FLGPOL) THEN
         IF (IFC .GE. 6) THEN
            CALL STONUM(FIELD(6),40,FNUM,INUM)
C           Check The Numerical Field
            IF (INUM .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               AZELEV(I3 + 1) = FNUM
            END IF
         END IF
      ELSE IF (FLGPOL .AND. .NOT.ELEV) THEN
         IF (IFC .EQ. 6) THEN
            CALL STONUM(FIELD(6),40,FNUM,INUM)
C           Check The Numerical Field
            IF (INUM .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               AZFLAG(I4 + 1) = FNUM
            END IF
         ELSE IF (IFC .EQ. 7) THEN
            CALL STONUM(FIELD(7),40,FNUM,INUM)
C           Check The Numerical Field
            IF (INUM .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE
               AZFLAG(I4 + 1) = FNUM
            END IF
         END IF
      END IF

C     Assign Them to Different Arrays,
C     Retrieve The Origin From Source Coordinates

      CALL SINDEX(SRCID,NSRC,SOID,ISDX,FIND)
      IF (.NOT. FIND) THEN
C        Error Message: Source ID Not Match
         CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
      ELSE
         AXR(I1 + 1) = AXS(ISDX) + RANGE*SIN(DIRECT*DTORAD)
         AYR(I2 + 1) = AYS(ISDX) + RANGE*COS(DIRECT*DTORAD)
         IF (IFC.GE.6 .AND. (ELTYPE.EQ.'FEET' .OR.
     &                       REELEV.EQ.'FEET')) THEN
C           Convert ELEV to Metric system
            AZELEV(I3 + 1) = 0.3048*AZELEV(I3 + 1)
         END IF
         IRXR = I1 + 1
         IRYR = I2 + 1
         IRZE = I3 + 1
         IRZF = I4 + 1
C        Reset ITAB Variable for TOXXFILE Option, 9/29/92
         ITAB = 0
      END IF

      NETID(IRXR)  = '   NA   '
      RECTYP(IRXR) = 'DP'
      IREF(IRXR)   = ISDX

 999  RETURN
      END

      SUBROUTINE BOUNDR
C***********************************************************************
C                 BOUNDR Module of ISC2 Model
C
C        PURPOSE: Processes Plant Boundary Receptor Location Inputs
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To Include TOXXFILE Option - 9/29/92
C                    To Correct Index Counter for BOUNDELV, and
C                    To Include Conversion of Elevations From
C                    Feet to Meters - 9/29/92
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Plant Boundary Receptor Location Inputs
C
C        CALLED FROM:   RECARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      CHARACTER SOID*8
      LOGICAL FIND

C     Variable Initializations
      MODNAM = 'BOUNDR'

C     Determine Whether There Are Enough Parameter Fields
      IF (IFC .EQ. 2) THEN
C        Error Message: Missing Parameter
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .EQ. 3) THEN
C        Error Message: Missing Numerical Field
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      END IF

      SOID = FIELD(3)

      CALL SINDEX(SRCID,NSRC,SOID,ISDX,FIND)

      IF (.NOT. FIND) THEN
C        Error Message: Source ID Not Match
         CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
         GO TO 999
      ELSE
         IF (KEYWRD .EQ. 'BOUNDARY') THEN
            ISET = IRXR
            JSET = IRYR
C           Update The Counter
            IF (SOID.NE.PXSOID .AND. PXSOID.NE.' ') THEN
C              Check If The Previous Boundary Points Number Right
               IF (IBND .LT. 36) THEN
C                 Error Message: No. Of Dist Not Enough
                  CALL ERRHDL(PATH,MODNAM,'E','230',PXSOID)
               END IF
C              Reset The Counter
               IBND = 0
            END IF
            DO 25 I = 4, IFC
               CALL STONUM(FIELD(I),40,FNUM,IMIT)
C              Check The Numerical Field
               IF (IMIT .EQ. -1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               END IF
               DO 20 J = 1, IMIT
                  ISET = ISET + 1
                  JSET = JSET + 1
                  IF (ISET .GT. NREC) THEN
C                    Error Msg: Maximum Number Of Receptors Exceeded
                     WRITE(DUMMY,'(I8)') NREC
                     CALL ERRHDL(PATH,MODNAM,'E','219',DUMMY)
                     GO TO 999
                  END IF
                  IBND = IBND + 1
C                 Check If The Count Exceeds 36
                  IF (IBND .GT. 36) THEN
C                    Error Message: No. of Dist Over 36
                     CALL ERRHDL(PATH,MODNAM,'E','231',SOID)
                     GO TO 999
                  END IF
                  DEG  =  10.*IBND*DTORAD
                  AXR(ISET) = AXS(ISDX) + FNUM*SIN(DEG)
                  AYR(JSET) = AYS(ISDX) + FNUM*COS(DEG)
                  RECTYP(ISET) = 'BD'
                  NETID(ISET) = '   NA   '
                  IREF(ISET) = ISDX
 20            CONTINUE
 25         CONTINUE

            IRXR = ISET
            IRYR = JSET
            IRZE = ISET
            IRZF = ISET
            PXSOID = SOID
C           Reset ITAB Variable for TOXXFILE Option, 9/29/92
            ITAB = 0

         ELSE IF (KEYWRD .EQ. 'BOUNDELV') THEN

            IF (FLAT) THEN
C              WRITE Warning Message:  Terrain Elevations Ignored
               CALL ERRHDL(PATH,MODNAM,'W','213',KEYWRD)
               GO TO 999
            END IF
            IF (SOID .NE. PESOID) THEN
C              First BOUNDELV Card for This Source,
C              Check for Previous BOUNDARY Card and Set Index
               FIND = .FALSE.
               DO 100 IREC = 1, NREC
                  IF (RECTYP(IREC) .EQ. 'BD' .AND.
     &                  IREF(IREC) .EQ. ISDX) THEN
C                    Set Index Counter for Elevations and Exit Loop
                     FIND = .TRUE.
                     IRZE = IREC - 1
                     GO TO 200
                  END IF
 100           CONTINUE
               IF (.NOT. FIND) THEN
C                 WRITE Error Message:  Boundary Locations Not Defined
                  CALL ERRHDL(PATH,MODNAM,'E','255',SOID)
                  GO TO 999
               END IF
            END IF

 200        CONTINUE

            ISET = IRZE
            IF (SOID.NE.PESOID .AND. PESOID.NE.' ') THEN
C              Check If The Previous Boundary Points Number Right
               IF (IBELEV .LT. 36) THEN
C                 Error Message: No. Of Elev Not Enough
                  CALL ERRHDL(PATH,MODNAM,'E','230',PESOID)
               END IF
C              Reset The Counter
               IBELEV = 0
            END IF
            DO 35 I = 4, IFC
               CALL STONUM(FIELD(I),40,FNUM,IMIT)
C              Check The Numerical Field
               IF (IMIT .EQ. -1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               END IF
               IF (ELTYPE .EQ. 'FEET' .OR. REELEV .EQ. 'FEET') THEN
C                 Convert ELEV to Metric system
                  FNUM = 0.3048 * FNUM
               END IF
               DO 30 J = 1, IMIT
                  ISET = ISET + 1
                  IF (ISET .GT. NREC) THEN
C                    Error Msg: Maximum Number Of Receptors Exceeded
                     WRITE(DUMMY,'(I8)') NREC
                     CALL ERRHDL(PATH,MODNAM,'E','219',DUMMY)
                     GO TO 999
                  END IF
                  IBELEV = IBELEV + 1
C                 Check If The Count Exceeds 36
                  IF (IBELEV .GT. 36) THEN
C                    Error Message: Elev Over 36
                     CALL ERRHDL(PATH,MODNAM,'E','231',SOID)
                     GO TO 300
                  END IF
                  AZELEV(ISET) = FNUM
 30            CONTINUE
 35         CONTINUE

 300        CONTINUE

            IRZE = ISET
            IRZF = ISET
            PESOID = SOID
         END IF
      END IF

 999  RETURN
      END

      SUBROUTINE SBYVAL(ARRIN1,ARRIN2,INX)
C***********************************************************************
C                 SBYVAL Module of ISC2 Model
C
C        PURPOSE: Sort Array By Its 'Index Value'
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  ARRIN1: 'Index Array',  ARRIN2: 'Value Array'
C                 INX: Number of Values to Sort
C
C        OUTPUTS: Sorted Array
C
C        CALLED FROM: (This Is A Utility Program)
C***********************************************************************
C
C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
C     Declare Input Arrays as Assumed-Size Arrays (Currently Dimensioned NREC
C     in Calling Routines)
      DIMENSION ARRIN1(*), ARRIN2(*)

C     Variable Initialization
      MODNAM = 'SBYVAL'
      JC = 1

      DO WHILE (JC .LE. INX)
C        Find out The First Minimum In the Array
         MIN = ARRIN1(JC)
         IMIN = JC
         DO 20 I = JC, INX
            IF (ARRIN1(I) .LT. MIN) THEN
               IMIN = I
               MIN = ARRIN1(I)
            END IF
  20     CONTINUE
C        Swap The Selected Array Elements
         TEMP1 = ARRIN1(JC)
         TEMP2 = ARRIN2(JC)
         ARRIN1(JC) = ARRIN1(IMIN)
         ARRIN2(JC) = ARRIN2(IMIN)
         ARRIN1(IMIN) = TEMP1
         ARRIN2(IMIN) = TEMP2
C        Increment The Counter
         JC = JC + 1
      END DO

      RETURN
      END
      SUBROUTINE GETCOM (MODEL,INPFIL,OUTFIL)
C***********************************************************************
C     
C        ADAPTED FROM PCCODE Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Controls Retrieving Input and Output File Names From
C                 the Command Line for PCs
C
C        PROGRAMMER: Roger Brode
C        
C        DATE:    March 2, 1992
C
C        MODIFIED:   Jayant Hardikar, PES, Inc.
C                    - Length of command line for Lahey version changed
C                      from 80 to 120 characters - 4/19/93
C                    - Adapted for DEPMET/PMERGE - 7/29/94
C
C        INPUTS:  Command Line
C
C        OUTPUTS: Input Runstream File Name
C                 Output Print File Name
C
C        CALLED FROM:   MAIN
C***********************************************************************
C
C     Variable Declarations
      CHARACTER*40 INPFIL,OUTFIL
      CHARACTER*8 MODEL
$IF DEFINED (MICRO)
C     Declare 2-Byte Integer for Field Number of Command Line Argument
      INTEGER*2 IARG
$ELSEIF DEFINED (LAHEY)
C     Declare the COMLIN Variable to Hold Contents of Command Line for Lahey
      CHARACTER*120 COMLIN
      INTEGER*2 LOCB(120),LOCE(120)
      LOGICAL INFLD
$ENDIF

$IF DEFINED (MICRO)
C************************************************************MICRO START
C     Use Microsoft Functions NARGS and GETARG To Retrieve
C     Contents of Command Line
      IFCNT = NARGS()
C     IFCNT Is The Number Of Arguments on Command Line Including Program
      IF (IFCNT .NE. 3) THEN
C        Error on Command Line.  Write Error Message and STOP
         WRITE(*,660) MODEL
         STOP
      ELSE
C        Retrieve First Argument as Input File Name
         IARG = 1
         CALL GETARG(IARG,INPFIL,ISTAT)
C        Retrieve Second Argument as Output File Name
         IARG = 2
         CALL GETARG(IARG,OUTFIL,ISTAT)
      END IF
C************************************************************MICRO STOP

$ELSEIF DEFINED (LAHEY)
C************************************************************LAHEY START
C     Use Lahey Function GETCL To Retrieve Contents of Command Line.
C     Retrieve Input and Output File Names From the COMLIN Variable.
      CALL GETCL(COMLIN)
      INFLD = .FALSE.
      IFCNT = 0
      DO 100 I = 1, 120
         IF (.NOT.INFLD .AND. COMLIN(I:I) .NE. ' ') THEN
            INFLD = .TRUE.
            IFCNT = IFCNT + 1
            LOCB(IFCNT) = I
         ELSE IF (INFLD .AND. COMLIN(I:I) .EQ. ' ') THEN
            INFLD = .FALSE.
            LOCE(IFCNT) = I - 1
         END IF
 100  CONTINUE
      IF (IFCNT .NE. 2) THEN
C        Error on Command Line.  Write Error Message and STOP
         WRITE(*,660) MODEL
         STOP
      END IF
      INPFIL = COMLIN(LOCB(1):LOCE(1))
      OUTFIL = COMLIN(LOCB(2):LOCE(2))
C************************************************************LAHEY STOP

$ENDIF

  660 FORMAT (' COMMAND LINE ERROR: ',A8,' input_file output_file')

      RETURN
      END


      SUBROUTINE DATIME ( DCALL, TCALL )
C***********************************************************************
C                 DATIME Module
C
C        PURPOSE: Obtain the system date and time
C
C        PROGRAMMER: Jim Paumier, PES, Inc.
C
C        DATE:    April 15, 1994
C
C        INPUTS:  none
C
C        OUTPUTS: Date and time in character format
C
C        CALLED FROM:  RUNTIME
C***********************************************************************
C
C     Variable Declarations
$IF DEFINED (MICRO)
      INTEGER*2 IPTHR, IPTMIN, IPTSEC, IPTHUN, IPTYR, IPTMON, IPTDAY
$ENDIF
      CHARACTER DCALL*8, TCALL*11

      DCALL = ' '
      TCALL = ' '

C     The Following Statements Support Either the Microsoft or
C        the Lahey Fortran Compilers for the PC Versions of the Code.
C        For Porting Model to Other Systems, Change Date & Time Function
C        Calls

$IF DEFINED (MICRO)
         CALL GETDAT(IPTYR, IPTMON, IPTDAY)
         CALL GETTIM(IPTHR, IPTMIN, IPTSEC, IPTHUN)
$ELSEIF DEFINED (LAHEY)
         CALL DATE(DCALL)
         CALL TIME(TCALL)
$ENDIF


$IF DEFINED (MICRO)
C        Convert Year to Two Digits
         IPTYR = IPTYR - 100 * INT(IPTYR/100)
C        Write Date and Time to Character Variables, DCALL & TCALL
         WRITE(DCALL, '(2(I2.2,1H/),I2.2)' ) IPTMON, IPTDAY, IPTYR
         WRITE(TCALL, '(2(I2.2,1H:),I2.2, 1H.,I2.2)' ) IPTHR, IPTMIN,
     &                                                IPTSEC, IPTHUN
$ENDIF

      RETURN
      END

      SUBROUTINE FILOPN
C***********************************************************************
C                 FILOPN Module
C
C        PURPOSE: Obtain the system date and time
C
C        PROGRAMMER: Roger Brode, PES, Inc.
C
C        DATE:    December 6, 1994
C
C        INPUTS:  Input filename, INPFIL
C                 Output filename, OUTFIL
C
C        OUTPUTS: Openned files
C
C        CALLED FROM:  HEADER
C
C        ERROR HANDLING:   Checks errors openning files
C***********************************************************************
C
C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     OPEN Input Runstream File, Unit INUNIT=5
      DUMMY = 'RUN-STRM'
C      OPEN (UNIT=INUNIT,FILE=INPUT,ERR=99,STATUS='OLD')

C     OPEN Print Output File, Unit IOUNIT=6
      DUMMY = 'OUTPUT'
$IF DEFINED (MICRO)
C      OPEN (UNIT=IOUNIT,FILE=OUTPUT,ERR=99,STATUS='UNKNOWN')
$ELSEIF DEFINED (LAHEY)
C      OPEN (UNIT=IOUNIT,FILE=OUTPUT,CARRIAGE CONTROL='FORTRAN',
C     &      ERR=99,STATUS='UNKNOWN')
$ENDIF

C     Write Out Update to the Screen
      WRITE(*,909)
 909  FORMAT('+','Now Processing SETUP Information')

      GO TO 1000

C     WRITE Error Message:  Error Opening File
 99   CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

C     Check for Error Opening Runstream File and STOP
      IF (DUMMY .EQ. 'RUN-STRM') THEN
         WRITE(*,919)
 919     FORMAT('+','Error Opening Runstream Input File!  Aborting.')
         STOP
      END IF

 1000 CONTINUE

      RETURN
      END

      SUBROUTINE HEADER
C***********************************************************************
C                 HEADER Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Control Page Feed and Header Information for
C                 Printed File Output
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    September 28, 1993
C
C        MODIFIED:   Replace DEPLETE parameter for plume depletion option
C                    with DDPLETE and WDPLETE in the list of model options
C                    for Wet & Dry depletion.
C                    D. Strimaitis, SRC - 11/8/93
C
C        MODIFIED:   Header modified for draft version of model with new
C                    area source and deposition algorithms - 9/28/93
C
C        MODIFIED:   To add DEPLETE parameter for plume depletion option
C                    to the list of model options
C                    D. Strimaitis, SRC - 2/15/93
C        INPUTS:  Page Number from COMMON
C
C        OUTPUTS: Page Feed and Header
C
C        CALLED FROM:  (This Is An Utility Program)
C***********************************************************************
C
C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
$IF DEFINED (MICRO)
      CHARACTER*1 FFEED
$ENDIF
      CHARACTER RUNDAT*8, RUNTIM*8, DCALL*8, TCALL*11
      COMMON /DATTIM/ RUNDAT, RUNTIM

C     Variable Initializations
      MODNAM = 'HEADER'

C     Increment Page Number Counter
      IPAGE = IPAGE + 1

C     Retrieve Date and Time Variables for First Call
      IF (IPAGE .EQ. 1) THEN
         RUNDAT = ' '
         RUNTIM = ' '

C        Get Date and Time using system-specific functions  ---   CALL DATIME
         CALL DATIME (DCALL, TCALL)

C        Store to variables in COMMON, and trim hundreths of seconds from TCALL
         RUNDAT = DCALL
         WRITE(RUNTIM,'(A8)') TCALL(1:8)
      END IF

C     Write Header to Printed Output File
$IF DEFINED (MICRO)
C     Assign ASCII Form Feed Character to Variable FFEED
      FFEED = CHAR(12)
      WRITE(IOUNIT,9028) FFEED, VERSN, TITLE1, RUNDAT
$ELSE
      WRITE(IOUNIT,9028) VERSN, TITLE1, RUNDAT
$ENDIF
      WRITE(IOUNIT,9029) TITLE2, RUNTIM
      WRITE(IOUNIT,9030) IPAGE
      WRITE(IOUNIT,9040) (MODOPS(I),I=1,17)

$IF DEFINED (MICRO)
C     Write an ASCII Form Feed Character (as Variable FFEED) for
C     Carriage Control With The Microsoft Version.
 9028 FORMAT(A1,'  *** ISC3P  - VERSION ',A5,' ***',4X,'*** ',A68,
     &        ' ***',8X,A8)
$ELSE
 9028 FORMAT('1',' *** ISC3P  - VERSION ',A5,' ***',4X,'*** ',A68,
     &       ' ***',8X,A8)
$ENDIF
 9029 FORMAT(36X,'*** ',A68,' ***',8X,A8)
 9030 FORMAT(120X,'PAGE ',I3)
 9040 FORMAT(1X,'**MODELOPTs:',17(1X,A6)/)

      RETURN
      END
      SUBROUTINE MECARD
C***********************************************************************
C                 MECARD Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: To process MEteorology Pathway Card Images
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  To Include TOXXFILE Option - 9/29/92
C
C        INPUTS:  Pathway (ME) and Keyword
C
C        OUTPUTS: Meteorology Option Switches
C                 Meteorology Setup Status Switches
C
C        CALLED FROM:   SETUP
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'MECARD'

      IF (KEYWRD .EQ. 'STARTING') THEN
C        Set Status Switch
         IMSTAT(1) = IMSTAT(1) + 1
         IF (IMSTAT(1) .NE. 1) THEN
C           WRITE Error Message: Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         END IF
      ELSE IF (KEYWRD .EQ. 'INPUTFIL') THEN
C        Set Status Switch
         IMSTAT(2) = IMSTAT(2) + 1
         IF (IMSTAT(2) .NE. 1) THEN
C           WRITE Error Message: Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Meteorology File Information            ---   CALL METFIL
            CALL METFIL
         END IF
      ELSE IF (KEYWRD .EQ. 'ANEMHGHT') THEN
C        Set Status Switch
         IMSTAT(3) = IMSTAT(3) + 1
         IF (IMSTAT(3) .NE. 1) THEN
C           WRITE Error Message: Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Anemometer Height Information           ---   CALL ANEMHT
            CALL ANEMHT
         END IF
      ELSE IF (KEYWRD .EQ. 'SURFDATA') THEN
C        Set Status Switch
         IMSTAT(4) = IMSTAT(4) + 1
         IF (IMSTAT(4) .NE. 1) THEN
C           WRITE Error Message: Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Surface Data Information                ---   CALL SFDATA
            CALL SFDATA
         END IF
      ELSE IF (KEYWRD .EQ. 'UAIRDATA') THEN
C        Set Status Switch
         IMSTAT(5) = IMSTAT(5) + 1
         IF (IMSTAT(5) .NE. 1) THEN
C           WRITE Error Message: Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Upper Air Data Information              ---   CALL UADATA
            CALL UADATA
         END IF
      ELSE IF (KEYWRD .EQ. 'STARTEND') THEN
C        Set Status Switch
         IMSTAT(6) = IMSTAT(6) + 1
         IF (IMSTAT(6) .NE. 1) THEN
C           WRITE Error Message: Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Start and End Dates for Reading         ---   CALL STAEND
            CALL STAEND
         END IF
      ELSE IF (KEYWRD .EQ. 'DAYRANGE') THEN
C        Set Status Switch
         IMSTAT(7) = IMSTAT(7) + 1
C        Check for First Occurrence of DAYRANGE Card, and
C        Reinitialize IPROC Array
         IF (IMSTAT(7) .EQ. 1) THEN
            DO 10 I = 1, 366
               IPROC(I) = 0
 10         CONTINUE
         END IF
C        Process Days and Day Ranges for Processing         ---   CALL DAYRNG
         CALL DAYRNG
      ELSE IF (KEYWRD .EQ. 'WDROTATE') THEN
C        Set Status Switch
         IMSTAT(8) = IMSTAT(8) + 1
         IF (IMSTAT(8) .NE. 1) THEN
C           WRITE Error Message: Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Wind Direction Correction Option        ---   CALL WDROTA
            CALL WDROTA
         END IF
      ELSE IF (KEYWRD .EQ. 'WINDPROF') THEN
C        Set Status Switch
         IMSTAT(9) = IMSTAT(9) + 1
         IF (DFAULT) THEN
C           WRITE Warning Message and Ignore Inputs
            CALL ERRHDL(PATH,MODNAM,'W','206',KEYWRD)
         ELSE
C           Process Wind Speed Profile Exponents            ---   CALL WSPROF
            CALL WSPROF
C           Set Logical Flag Indicating User-specified P-Exponents
            USERP = .TRUE.
         END IF
      ELSE IF (KEYWRD .EQ. 'DTHETADZ') THEN
C        Set Status Switch
         IMSTAT(10) = IMSTAT(10) + 1
         IF (DFAULT) THEN
C           WRITE Warning Message and Ignore Inputs
            CALL ERRHDL(PATH,MODNAM,'W','206',KEYWRD)
         ELSE
C           Process Vertical Pot. Temperature Gradients     ---   CALL DTHETA
            CALL DTHETA
C           Set Logical Flag Indicating User-specified DThetaDZ
            USERDT = .TRUE.
         END IF
      ELSE IF (KEYWRD .EQ. 'WINDCATS') THEN
C        Set Status Switch
         IMSTAT(11) = IMSTAT(11) + 1
         IF (IMSTAT(11) .NE. 1) THEN
C           WRITE Error Message: Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Wind Speed Categories                   ---   CALL WSCATS
            CALL WSCATS
         END IF
      ELSE IF (KEYWRD .EQ. 'FINISHED') THEN
C        Set Status Switch
         IMSTAT(20) = IMSTAT(20) + 1
         IF (IMSTAT(20) .NE. 1) THEN
C           WRITE Error Message: Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
            GO TO 999
         END IF
C        Write Error Messages for Missing Mandatory Keyword(s)
         IF (IMSTAT(1) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','STARTING')
         END IF
         IF (IMSTAT(2) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','INPUTFIL')
         END IF
         IF (IMSTAT(3) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','ANEMHGHT')
         END IF
         IF (IMSTAT(4) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','SURFDATA')
         END IF
         IF (IMSTAT(5) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','UAIRDATA')
         END IF

C        OPEN Met Data File                                 ---   CALL MEOPEN
         IF (IMSTAT(2) .NE. 0) THEN
            CALL MEOPEN
         END IF

         IF (MULTYR) THEN
C           Set the Increment for Saving Results, INCRST, Based on
C           ISYEAR, Surface Data Year, from SURFDATA Keyword
            IF ((MOD(ISYEAR,4) .NE. 0) .OR.
     &          (MOD(ISYEAR,100).EQ.0 .AND. MOD(ISYEAR,400).NE.0)) THEN
C              Not a Leap Year
               INCRST = 365
            ELSE
C              Leap Year
               INCRST = 366
            END IF
         END IF

C        Determine Number of Hours to be Processed, NHOURS, For Use
C        With the TOXXFILE Option - 9/29/92
         IF ((MOD(ISYEAR,4) .NE. 0) .OR.
     &       (MOD(ISYEAR,100).EQ.0 .AND. MOD(ISYEAR,400).NE.0)) THEN
C           Not a Leap Year
            ND = 365
         ELSE
C           Leap Year
            ND = 366
         END IF
         NDYS = 0
         DO 100 I = 1, ND
            IF (IPROC(I) .EQ. 1) THEN
               NDYS = NDYS + 1
            END IF
 100     CONTINUE
         NHOURS = NDYS * 24

      ELSE
C        Write Error Message: Invalid Keyword for This Pathway
         CALL ERRHDL(PATH,MODNAM,'E','110',KEYWRD)
      END IF

 999  RETURN
      END

      SUBROUTINE METFIL
C***********************************************************************
C                 METFIL Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process Meteorology Input File Options
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Meteorological Data Filename and Format
C
C        ERROR HANDLING:   Checks for No Parameters;
C                          Checks for No Format (uses default);
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   MECARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'METFIL'

      IF (IFC .EQ. 3) THEN
C        Retrieve Met Data Filename as Character Substring to Maintain Case
         METINP = RUNST1(LOCB(3):LOCE(3))
C        Use Default Met Data Format: Initialized in SUBROUTINE VARINI to
CC        METFRM = '(4I2,2F9.4,F6.1,I2,2F7.1)'
      ELSE IF (IFC .EQ. 4) THEN
C        Retrieve Met Data Filename as Character Substring to Maintain Case
         METINP = RUNST1(LOCB(3):LOCE(3))
C        Check for Format String > 60 (Limit for METFRM Variable)
         IF ((LOCE(4)-LOCB(4)) .LE. 59) THEN
            IF ((LOCE(4)-LOCB(4)) .GT. 39) THEN
C              Retrieve Met Format as Char. Substring to Bypass Field Limit
               METFRM = RUNST1(LOCB(4):LOCE(4))
            ELSE
C              Retrieve Met Format From FIELD(4)
               METFRM = FIELD(4)
            END IF
         ELSE
C           WRITE Error Message:  METFRM Field is Too Long
            CALL ERRHDL(PATH,MODNAM,'E','203',' METFRM ')
         END IF
C        Check for Use of CARD Format With DFAULT Option
         IF (DFAULT .AND. METFRM .EQ. 'CARD') THEN
C           WRITE Error Message:  DFAULT With Non-DEFAULT Option (Due to
C           Hourly WINDPROF and DTHETADZ on CARD Format
            CALL ERRHDL(PATH,MODNAM,'E','206','CARD-MET')
         END IF
C        Check for Use of UNFORM format with Dry or Wet Deposition
         IF ((LWPART.OR.LDPART.OR.LWGAS) .AND. METFRM.EQ.'UNFORM') THEN
C           WRITE Error Message:  Can't use UNFORM with Dry or Wet Deposition
            CALL ERRHDL(PATH,MODNAM,'E','143',' ')
         END IF
      ELSE IF (IFC .GT. 4) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Warning Message         ! No Parameters Specified
         CALL ERRHDL(PATH,MODNAM,'W','200',KEYWRD)
      END IF

      RETURN
      END

      SUBROUTINE ANEMHT
C***********************************************************************
C                 ANEMHT Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process Anemometer Height Options
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Anemometer Height, ZREF (m)
C
C        ERROR HANDLING:   Checks for No Parameters;
C                          Checks for No Units (uses default of m);
C                          Checks for Invalid or Suspicious Values of ZREF;
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   MECARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'ANEMHT'

      IF (IFC .EQ. 3 .OR. IFC .EQ. 4) THEN
         CALL STONUM(FIELD(3),40,ANHT,IDUM)
C        Check The Numerical Field
         IF (IDUM.EQ.-1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 999
         END IF
         IF (IFC .EQ. 4 .AND. FIELD(4) .EQ. 'FEET') THEN
            ANHT = 0.3048 * ANHT
         ELSE IF (IFC .EQ. 4 .AND. FIELD(4) .NE. 'METERS') THEN
C           WRITE Warning Message - Invalid ZRUNIT Parameter
            CALL ERRHDL(PATH,MODNAM,'W','203','ZRUNIT')
         END IF
         IF (ANHT .GT. 100.0 .AND. IDUM .EQ. 1) THEN
C           WRITE Warning Message - Possible Error In ANHT
            WRITE(DUMMY,'(F8.3)') ANHT
            CALL ERRHDL(PATH,MODNAM,'W','340',DUMMY)
            ZREF = ANHT
         ELSE IF (ANHT .GE. 2.0 .AND. IDUM .EQ. 1) THEN
            ZREF = ANHT
         ELSE IF (ANHT .GT. 0.0 .AND. IDUM .EQ. 1) THEN
C           WRITE Warning Message - Possible Error In ANHT
            CALL ERRHDL(PATH,MODNAM,'W','340',KEYWRD)
            ZREF = ANHT
         ELSE IF (ANHT .LE. 0.0 .AND. IDUM .EQ. 1) THEN
C           WRITE Error Message - Invalid Anemometer Height
            CALL ERRHDL(PATH,MODNAM,'E','203','Anem Hgt')
         ELSE IF (IDUM .NE. 1) THEN
C           WRITE Error Message - Invalid Numeric Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         END IF
      ELSE IF (IFC .GT. 4) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Error Message           ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      END IF

 999  RETURN
      END

      SUBROUTINE SFDATA
C***********************************************************************
C                 SFDATA Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process Meteorology Surface Data Station Options
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Meteorological Surface Data Station Identification
C
C        ERROR HANDLING:   Checks for Too Few Parameters;
C                          Checks for Invalid Numeric Fields;
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   MECARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'SFDATA'

      IF (IFC .EQ. 2) THEN
C        WRITE Error Message           ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 4) THEN
C        WRITE Error Message           ! Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 7) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

      CALL STONUM(FIELD(3),40,FNUM,IDUM)
C     Check The Numerical Field
      IF (IDUM .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 199
      END IF
      IDSURF = INT(FNUM)

 199  CALL STONUM(FIELD(4),40,FNUM,IDUM)
C     Check The Numerical Field
      IF (IDUM .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 299
      END IF
      ISYEAR = INT(FNUM)

 299  IF (IFC .GE. 5) THEN
C        Retrieve Surface Data Station Name (Optional)
         SFNAME = FIELD(5)
      ELSE
         SFNAME = 'UNKNOWN'
      END IF

      IF (IFC .EQ. 7) THEN
C        Retrieve Coordinates for Surface Data Location (Optional)
         CALL STONUM(FIELD(6),40,SFX,IDUM)
         IF (IDUM .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         END IF
         CALL STONUM(FIELD(7),40,SFY,IDUM)
         IF (IDUM .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         END IF
      END IF

 999  RETURN
      END

      SUBROUTINE UADATA
C***********************************************************************
C                 UADATA Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process Meteorology Upper Air Data Station Options
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Meteorological Upper Air Data Station Identification
C
C        ERROR HANDLING:   Checks for Too Few Parameters;
C                          Checks for Invalid Numeric Fields;
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   MECARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'UADATA'

      IF (IFC .EQ. 2) THEN
C        WRITE Error Message           ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 4) THEN
C        WRITE Error Message           ! Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 7) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

      CALL STONUM(FIELD(3),40,FNUM,IDUM)
C     Check The Numerical Field
      IF (IDUM .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 199
      END IF
      IDUAIR = INT(FNUM)

 199  CALL STONUM(FIELD(4),40,FNUM,IDUM)
C     Check The Numerical Field
      IF (IDUM .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 299
      END IF
      IUYEAR = INT(FNUM)

 299  IF (IFC .GE. 5) THEN
C        Retrieve Surface Data Station Name (Optional)
         UANAME = FIELD(5)
      ELSE
         UANAME = 'UNKNOWN'
      END IF

      IF (IFC .EQ. 7) THEN
C        Retrieve Coordinates for Surface Data Location (Optional)
         CALL STONUM(FIELD(6),40,UAX,IDUM)
         IF (IDUM .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         END IF
         CALL STONUM(FIELD(7),40,UAY,IDUM)
         IF (IDUM .NE. 1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         END IF
      END IF

 999  RETURN
      END

      SUBROUTINE STAEND
C***********************************************************************
C                 STAEND Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process Start and End Dates for Meteorology File
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Start and End Dates to Read from Meteorological File
C
C        ERROR HANDLING:   Checks for Too Few Parameters;
C                          Checks for Invalid Numeric Fields;
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   MECARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'STAEND'

      IF (IFC .EQ. 8) THEN
C        Process for YR, MD, DY
         CALL STONUM(FIELD(3),40,FNUM,IDUM1)
C        Check The Numerical Field
         IF (IDUM1 .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 198
         END IF
         ISYR = INT(FNUM)
 198     CALL STONUM(FIELD(4),40,FNUM,IDUM2)
C        Check The Numerical Field
         IF (IDUM2 .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 298
         END IF
         ISMN = INT(FNUM)
 298     CALL STONUM(FIELD(5),40,FNUM,IDUM3)
C        Check The Numerical Field
         IF (IDUM3 .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 398
         END IF
         ISDY = INT(FNUM)
 398     CALL STONUM(FIELD(6),40,FNUM,IDUM4)
C        Check The Numerical Field
         IF (IDUM4 .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 498
         END IF
         IEYR = INT(FNUM)
 498     CALL STONUM(FIELD(7),40,FNUM,IDUM5)
C        Check The Numerical Field
         IF (IDUM5 .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 598
         END IF
         IEMN = INT(FNUM)
 598     CALL STONUM(FIELD(8),40,FNUM,IDUM6)
C        Check The Numerical Field
         IF (IDUM6 .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 698
         END IF
         IEDY = INT(FNUM)
 698     IF (IDUM1 .NE. 1 .OR. IDUM2 .NE. 1 .OR. IDUM3 .NE. 1 .OR.
     &       IDUM4 .NE. 1 .OR. IDUM5 .NE. 1 .OR. IDUM6 .NE. 1) THEN
C           WRITE Error Message    ! Invalid Numeric Parameter
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         ELSE
C           Convert ISYR and IEYR to Four Digits
            IF (ISYR .GE. 50 .AND. ISYR .LE. 99) THEN
               ISYR = 1900 + ISYR
            ELSE IF (ISYR .LT. 50) THEN
               ISYR = 2000 + ISYR
            END IF
            IF (IEYR .GE. 50 .AND. IEYR .LE. 99) THEN
               IEYR = 1900 + IEYR
            ELSE IF (IEYR .LT. 50) THEN
               IEYR = 2000 + IEYR
            END IF
C           Calculate JULIAN Day for Start and End Dates
            CALL JULIAN (ISYR,ISMN,ISDY,ISJDAY)
            CALL JULIAN (IEYR,IEMN,IEDY,IEJDAY)
C           Convert Years Back to Two Digits
            ISYR = ISYR - 100*INT(ISYR/100)
            IEYR = IEYR - 100*INT(IEYR/100)
C           Use 0 for Start Hour and 24 for End Hour
            ISHR = 0
            IEHR = 24
            ISDATE = ISYR*1000000 + ISMN*10000 + ISDY*100 + ISHR
            IEDATE = IEYR*1000000 + IEMN*10000 + IEDY*100 + IEHR
         END IF
      ELSE IF (IFC .EQ. 10) THEN
C        Process for YR, MD, DY, HR
         CALL STONUM(FIELD(3),40,FNUM,IDUM1)
C        Check The Numerical Field
         IF (IDUM1 .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 199
         END IF
         ISYR = INT(FNUM)
 199     CALL STONUM(FIELD(4),40,FNUM,IDUM2)
C        Check The Numerical Field
         IF (IDUM2 .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 299
         END IF
         ISMN = INT(FNUM)
 299     CALL STONUM(FIELD(5),40,FNUM,IDUM3)
C        Check The Numerical Field
         IF (IDUM3 .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 399
         END IF
         ISDY = INT(FNUM)
 399     CALL STONUM(FIELD(6),40,FNUM,IDUM4)
C        Check The Numerical Field
         IF (IDUM4 .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 499
         END IF
         ISHR = INT(FNUM)
 499     CALL STONUM(FIELD(7),40,FNUM,IDUM5)
C        Check The Numerical Field
         IF (IDUM5 .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 599
         END IF
         IEYR = INT(FNUM)
 599     CALL STONUM(FIELD(8),40,FNUM,IDUM6)
C        Check The Numerical Field
         IF (IDUM6 .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 699
         END IF
         IEMN = INT(FNUM)
 699     CALL STONUM(FIELD(9),40,FNUM,IDUM7)
C        Check The Numerical Field
         IF (IDUM7 .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 799
         END IF
         IEDY = INT(FNUM)
 799     CALL STONUM(FIELD(10),40,FNUM,IDUM8)
C        Check The Numerical Field
         IF (IDUM8 .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            GO TO 899
         END IF
         IEHR = INT(FNUM)
 899     IF (IDUM1 .NE. 1 .OR. IDUM2 .NE. 1 .OR. IDUM3 .NE. 1 .OR.
     &       IDUM4 .NE. 1 .OR. IDUM5 .NE. 1 .OR. IDUM6 .NE. 1 .OR.
     &       IDUM7 .NE. 1 .OR. IDUM8 .NE. 1) THEN
C           WRITE Error Message    ! Invalid Numeric Parameter
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         ELSE
C           Convert ISYR and IEYR to Four Digits
            IF (ISYR .GE. 50 .AND. ISYR .LE. 99) THEN
               ISYR = 1900 + ISYR
            ELSE IF (ISYR .LT. 50) THEN
               ISYR = 2000 + ISYR
            END IF
            IF (IEYR .GE. 50 .AND. IEYR .LE. 99) THEN
               IEYR = 1900 + IEYR
            ELSE IF (IEYR .LT. 50) THEN
               IEYR = 2000 + IEYR
            END IF
C           Calculate JULIAN Day for Start and End Dates
            CALL JULIAN (ISYR,ISMN,ISDY,ISJDAY)
            CALL JULIAN (IEYR,IEMN,IEDY,IEJDAY)
C           Convert Years Back to Two Digits
            ISYR = ISYR - 100*INT(ISYR/100)
            IEYR = IEYR - 100*INT(IEYR/100)
            ISDATE = ISYR*1000000 + ISMN*10000 + ISDY*100 + ISHR
            IEDATE = IEYR*1000000 + IEMN*10000 + IEDY*100 + IEHR
            IF (ISHR .NE. 0) THEN
C              Adjust Start Hour to One Hour Earlier
               ISDATE = ISDATE - 1
            END IF
         END IF
      ELSE IF (IFC .GT. 8) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Error Message           ! Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
      END IF

 999  RETURN
      END

      SUBROUTINE DAYRNG
C***********************************************************************
C                 DAYRNG Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Process the Selection of Days and Ranges of Days
C                 for Processing from the Meteorology File
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Array of Dates to Process from Meteorological File
C
C        ERROR HANDLING:   Checks for Too Few Parameters;
C                          Checks for Invalid Numeric Fields;
C                          Checks for Improper Combinations of Fields;
C                          Checks for Dates Out of Range
C
C        CALLED FROM:   MECARD
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      CHARACTER BEGRNG*8, ENDRNG*8, CMN1*8, CDY1*8, CMN2*8, CDY2*8
      CHARACTER BLNK08*8
      LOGICAL WRONG, RMARK, GMARK

C     Variable Initializations
      MODNAM = 'DAYRNG'
      DATA BLNK08/'        '/

      IF (IFC .LT. 3) THEN
C        WRITE Error Message           ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      ELSE
         DO 40 I = 3, IFC
C           First Check For Range Marker (-) And Gregorian Day Marker (/)
C           Initialize Character Fields
            BEGRNG = BLNK08
            ENDRNG = BLNK08
            CMN1 = BLNK08
            CDY1 = BLNK08
            CMN2 = BLNK08
            CDY2 = BLNK08
            CALL FSPLIT(PATH,KEYWRD,FIELD(I),40,'-',RMARK,BEGRNG,ENDRNG)
            CALL FSPLIT(PATH,KEYWRD,BEGRNG,8,'/',GMARK,CMN1,CDY1)
            IF (RMARK .AND. GMARK) THEN
               CALL FSPLIT(PATH,KEYWRD,ENDRNG,8,'/',GMARK,CMN2,CDY2)
            END IF

            IF (.NOT.RMARK .AND. .NOT.GMARK) THEN
C              Field Must Be a Single Julian Day
               CALL STONUM(BEGRNG,8,ZDAY,IDUM)
C              Check The Numerical Field
               IF (IDUM .EQ. -1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  GO TO 40
               END IF
               JDAY = INT(ZDAY)
               IF (JDAY.GE.1 .AND. JDAY.LE.366 .AND. IDUM.EQ.1) THEN
                  IPROC(JDAY) = 1
               ELSE
C                 WRITE Error Message    ! Invalid Julian Day
                  CALL ERRHDL(PATH,MODNAM,'E','203','Juli Day')
               END IF
               IF (JDAY.LT.ISJDAY .OR. JDAY.GT.IEJDAY) THEN
C                 WRITE Warning Message  ! Julian Day Out-of-Range
                  WRITE(DUMMY,'(I8)') JDAY
                  CALL ERRHDL(PATH,MODNAM,'W','350',DUMMY)
               END IF

            ELSE IF (RMARK .AND. .NOT.GMARK) THEN
C              Field Must Be a Julian Day Range - Extract Beg & End
               CALL STONUM(BEGRNG,8,BZDAY,IDUM)
C              Check The Numerical Field
               IF (IDUM .EQ. -1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  WRONG = .TRUE.
               END IF
               CALL STONUM(ENDRNG,8,EZDAY,IDUM)
C              Check The Numerical Field
               IF (IDUM .EQ. -1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  WRONG = .TRUE.
               END IF
               IF (WRONG) GO TO 40
               JDAYB = INT(BZDAY)
               JDAYE = INT(EZDAY)
               IF ((JDAYB .LE. JDAYE) .AND. (JDAYB .GE. 1) .AND.
     &             (JDAYE .LE. 366)) THEN
                  DO 20 K = JDAYB, JDAYE
                     IPROC(K) = 1
 20               CONTINUE
               ELSE
C                 WRITE Error Message    ! Invalid Julian Day Range
                  CALL ERRHDL(PATH,MODNAM,'E','203','Juli Day')
               END IF
               IF (JDAYB.LT.ISJDAY .OR. JDAYE.GT.IEJDAY) THEN
C                 WRITE Warning Message  ! Julian Day Out-of-Range
                  WRITE(DUMMY,'(I3,1H-,I3)') JDAYB, JDAYE
                  CALL ERRHDL(PATH,MODNAM,'W','350',DUMMY)
               END IF

            ELSE IF (.NOT.RMARK .AND. GMARK) THEN
C               Field Must Be a Single Month/Day
               CALL STONUM(CMN1,8,ZMON,IDUM)
C              Check The Numerical Field
               IF (IDUM .EQ. -1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  WRONG = .TRUE.
               END IF
               CALL STONUM(CDY1,8,ZDAY,IDUM)
C              Check The Numerical Field
               IF (IDUM .EQ. -1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  WRONG = .TRUE.
               END IF
               IF (WRONG) GO TO 40
               IMN = INT(ZMON)
               IDY = INT(ZDAY)
               CALL JULIAN(ISYEAR,IMN,IDY,JDAY)
               IF (JDAY .GE. 1 .AND. JDAY .LE. 366) THEN
                  IPROC(JDAY) = 1
               ELSE
C                 WRITE Error Message    ! Invalid Julian Day
                  CALL ERRHDL(PATH,MODNAM,'E','203','Juli Day')
               END IF
               IF (JDAY.LT.ISJDAY .OR. JDAY.GT.IEJDAY) THEN
C                 WRITE Warning Message  ! Julian Day Out-of-Range
                  WRITE(DUMMY,'(I8)') JDAY
                  CALL ERRHDL(PATH,MODNAM,'W','350',DUMMY)
               END IF

            ELSE IF (RMARK .AND. GMARK) THEN
C              Field Must Be a Greg. Date Range (MN/DY-MN/DY)
               CALL STONUM(CMN1,8,ZMN1,IDUM)
C              Check The Numerical Field
               IF (IDUM .EQ. -1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  WRONG = .TRUE.
               END IF
               CALL STONUM(CDY1,8,ZDY1,IDUM)
C              Check The Numerical Field
               IF (IDUM .EQ. -1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  WRONG = .TRUE.
               END IF
               IF (WRONG) GO TO 41
               IMN1 = INT(ZMN1)
               IDY1 = INT(ZDY1)
 41            CALL STONUM(CMN2,8,ZMN2,IDUM)
C              Check The Numerical Field
               IF (IDUM .EQ. -1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  WRONG = .TRUE.
               END IF
               CALL STONUM(CDY2,8,ZDY2,IDUM)
C              Check The Numerical Field
               IF (IDUM .EQ. -1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  WRONG = .TRUE.
               END IF
               IF (WRONG) GO TO 40
               IMN2 = INT(ZMN2)
               IDY2 = INT(ZDY2)
               CALL JULIAN(ISYEAR,IMN1,IDY1,JDAYB)
               CALL JULIAN(ISYEAR,IMN2,IDY2,JDAYE)
               IF ((JDAYB .LE. JDAYE) .AND. (JDAYB .GE. 1) .AND.
     &             (JDAYE .LE. 366)) THEN
                  DO 30 K = JDAYB, JDAYE
                     IPROC(K) = 1
 30               CONTINUE
               ELSE
C                 WRITE Error Message    ! Invalid Julian Day
                  CALL ERRHDL(PATH,MODNAM,'E','203','Juli Day')
               END IF
               IF (JDAYB.LT.ISJDAY .OR. JDAYE.GT.IEJDAY) THEN
C                 WRITE Warning Message  ! Julian Day Out-of-Range
                  WRITE(DUMMY,'(I3,1H-,I3)') JDAYB, JDAYE
                  CALL ERRHDL(PATH,MODNAM,'W','350',DUMMY)
               END IF

            ELSE
C               WRITE Error Message    ! Invalid Field
                CALL ERRHDL(PATH,MODNAM,'E','203','DAYRANGE')
            END IF

 40      CONTINUE
      END IF

      RETURN
      END

      SUBROUTINE WDROTA
C***********************************************************************
C                 WDROTA Module of ISC2 Short Term Model - ISCST2
C
C     PURPOSE:    PROCESSES INPUT FOR ROTATING WIND DIRECTION DATA
C
C     PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C     INPUTS:     Input Runstream Image Parameters
C
C     OUTPUT:     Wind Direction Rotation Angle
C
C     CALLED FROM:   MECARD
C
C     ERROR HANDLING:   Checks for No Parameters;
C                       Checks for Too Many Parameters;
C                       Checks for Invalid Numeric Field
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      ROTANG = 0.0
      MODNAM = 'WDROTA'

      IF (IFC .EQ. 3) THEN
         CALL STONUM(FIELD(3),40,ROTANG,IDUM)
         IF (IDUM .NE. 1) THEN
C            WRITE Error Message  ! Invalid Numeric Field Encountered
             CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         ELSE IF (ABS(ROTANG) .GT. 180.0) THEN
C            WRITE Error Message       ! ROTANG Out of Range
             CALL ERRHDL(PATH,MODNAM,'E','380','ROTANG')
         END IF
      ELSE IF (IFC .GT. 3) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Error Message           ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      END IF

 999  RETURN
      END

      SUBROUTINE WSCATS
C***********************************************************************
C                 WSCATS Module of ISC2 Short Term Model - ISCST2
C
C     PURPOSE:    PROCESSES INPUT FOR WIND SPEED CATEGORIES
C
C     PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C     INPUTS:     Input Runstream Image Parameters
C
C     OUTPUT:     Array of Wind Speed Category Limits (5)
C
C     CALLED FROM:   MECARD
C
C     ERROR HANDLING:   Checks for No Parameters;
C                       Checks for Too Many Parameters;
C                       Checks for Invalid Numeric Fields;
C                       Checks for Wind Speed Category Decreasing
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'WSCATS'

      IF (IFC .EQ. 7) THEN
C        Fill UCAT Array
         DO 100 I = 3, IFC
            CALL STONUM(FIELD(I),40,ZNUM,IDUM)
            IF (IDUM .NE. 1) THEN
C              WRITE Error Message  ! Invalid Numeric Field Encountered
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE IF (ZNUM .LT. 1.0 .OR. ZNUM .GT. 20.0) THEN
C               WRITE Error Message       ! UCAT Out of Range
                CALL ERRHDL(PATH,MODNAM,'E','380','UCAT')
            ELSE
               IWS = I - 2
               UCAT(IWS) = ZNUM
               IF (IWS.GT.1 .AND. UCAT(IWS).LE.UCAT(IWS-1)) THEN
C                 WRITE Error Message    ! Invalid UCAT Value, LE Previous
                  CALL ERRHDL(PATH,MODNAM,'E','203','UCAT')
               END IF
            END IF
 100     CONTINUE
      ELSE IF (IFC .GT. 7) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Error Message           ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      END IF

      RETURN
      END

      SUBROUTINE WSPROF
C***********************************************************************
C                 WSPROF Module of ISC2 Short Term Model - ISCST2
C
C     PURPOSE:    PROCESSES INPUT FOR WIND SPEED PROFILE EXPONENTS
C
C     PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C     INPUTS:     Input Runstream Image Parameters
C
C     OUTPUT:     Array of Wind Speed Profile Exponents for Each Stability
C                 and Wind Speed Category
C
C     CALLED FROM:   MECARD
C
C     ERROR HANDLING:   Checks for No Parameters;
C                       Checks for Too Many Parameters;
C                       Checks for Invalid Numeric Fields;
C                       Checks for Invalid Stability Class Indicator
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'WSPROF'

      IF (IFC .GE. 4 .AND. IFC .LE. 9) THEN
         IWS = 0
C        Determine Stability Category Index from FIELD(3); Accepts Either
C        Character Inputs (A-F) or Numeric Inputs (1-6).
         IF (FIELD(3) .EQ. 'A' .OR. FIELD(3) .EQ. '1') THEN
            IST = 1
         ELSE IF (FIELD(3) .EQ. 'B' .OR. FIELD(3) .EQ. '2') THEN
            IST = 2
         ELSE IF (FIELD(3) .EQ. 'C' .OR. FIELD(3) .EQ. '3') THEN
            IST = 3
         ELSE IF (FIELD(3) .EQ. 'D' .OR. FIELD(3) .EQ. '4') THEN
            IST = 4
         ELSE IF (FIELD(3) .EQ. 'E' .OR. FIELD(3) .EQ. '5') THEN
            IST = 5
         ELSE IF (FIELD(3) .EQ. 'F' .OR. FIELD(3) .EQ. '6') THEN
            IST = 6
         ELSE
C           WRITE Error Message           ! Invalid Stability Class Indicator
            CALL ERRHDL(PATH,MODNAM,'E','203','INDKST')
C           Exit to END
            GO TO 999
         END IF

C        Fill PUSER Array
         DO 100 I = 4, IFC
            CALL STONUM(FIELD(I),40,ZNUM,IDUM)
C           Check The Numerical Field
            IF (IDUM .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 100
            END IF
            IF (ZNUM .LT. 0.0 .OR. ZNUM .GT. 1.0) THEN
C              WRITE Error Message          ! PUSER Out of Range
               CALL ERRHDL(PATH,MODNAM,'E','380','PUSER')
            ELSE
               DO 90 J = 1,IDUM
                  IWS = IWS + 1
                  IF (IWS .LE. NWSCAT) THEN
                     PUSER(IST,IWS) = ZNUM
                  ELSE
C                    WRITE Error Message    ! Too Many Values
                     CALL ERRHDL(PATH,MODNAM,'E','231',KEYWRD)
                  END IF
 90            CONTINUE
            END IF
 100     CONTINUE

      ELSE IF (IFC .GT. 9) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Error Message           ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      END IF

 999  CONTINUE

      RETURN
      END

      SUBROUTINE DTHETA
C***********************************************************************
C                 DTHETA Module of ISC2 Short Term Model - ISCST2
C
C     PURPOSE:    PROCESSES INPUT FOR VERTICAL POTENTIAL TEMPERATURE
C                 GRADIENTS
C
C     PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C     INPUTS:     Input Runstream Image Parameters
C
C     OUTPUT:     Array of Vertical Potential Temperature Gradients for
C                 Each Stability and Wind Speed Category
C
C     CALLED FROM:   MECARD
C
C     ERROR HANDLING:   Checks for No Parameters;
C                       Checks for Too Many Parameters;
C                       Checks for Invalid Numeric Fields;
C                       Checks for Invalid Stability Class Indicator
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'DTHETA'

      IF (IFC .GE. 4 .AND. IFC .LE. 9) THEN
         IWS = 0

C        Determine Stability Category Index from FIELD(3); Accepts Either
C        Character Inputs (A-F) or Numeric Inputs (1-6).
         IF (FIELD(3) .EQ. 'A' .OR. FIELD(3) .EQ. '1') THEN
            IST = 1
         ELSE IF (FIELD(3) .EQ. 'B' .OR. FIELD(3) .EQ. '2') THEN
            IST = 2
         ELSE IF (FIELD(3) .EQ. 'C' .OR. FIELD(3) .EQ. '3') THEN
            IST = 3
         ELSE IF (FIELD(3) .EQ. 'D' .OR. FIELD(3) .EQ. '4') THEN
            IST = 4
         ELSE IF (FIELD(3) .EQ. 'E' .OR. FIELD(3) .EQ. '5') THEN
            IST = 5
         ELSE IF (FIELD(3) .EQ. 'F' .OR. FIELD(3) .EQ. '6') THEN
            IST = 6
         ELSE
C           WRITE Error Message           ! Invalid Stability Class Indicator
            CALL ERRHDL(PATH,MODNAM,'E','203','INDKST')
C           Exit to END
            GO TO 1999
         END IF

C        Fill DTUSER Array
         DO 100 I = 4, IFC
            CALL STONUM(FIELD(I),40,ZNUM,IDUM)
C           Check The Numerical Field
            IF (IDUM .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 100
            END IF
            IF (ZNUM .LT. 0.0 .OR. ZNUM .GT. 0.2) THEN
C               WRITE Error Message       ! DTUSER Out of Range
                CALL ERRHDL(PATH,MODNAM,'E','380','DTUSER')
            ELSE
               DO 90 J = 1,IDUM
                  IWS = IWS + 1
                  IF (IWS .LE. NWSCAT) THEN
                     DTUSER(IST,IWS) = ZNUM
                  ELSE
C                    WRITE Error Message    ! Too Many Values
                     CALL ERRHDL(PATH,MODNAM,'E','231',KEYWRD)
                  END IF
 90            CONTINUE
            END IF
 100     CONTINUE

      ELSE IF (IFC .GT. 9) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Error Message           ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      END IF
 1999 CONTINUE

      RETURN
      END

      SUBROUTINE MEOPEN
C***********************************************************************
C                 MEOPEN Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Open The Input file for Hourly Meteorological Data,
C                 And Check Header Record
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Meteorology File Specifications
C
C        OUTPUTS: File OPEN Error Status
C
C        CALLED FROM:   SETUP
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'MEOPEN'

C     File Unit Initialized in BLOCK DATA INIT
C     File Format Set By Keyword "INPUTFIL" on "ME" pathway
C     OPEN Met Data File --- Either Unformatted or Formatted
C     READ In the Station Numbers and Years for Comparison to SETUP File

      IF (METFRM .EQ. 'UNFORM') THEN
         OPEN(UNIT=MFUNIT,ERR=999,FILE=METINP,FORM='UNFORMATTED',
     &        IOSTAT=IOERRN,STATUS='OLD')
         READ(MFUNIT,ERR=99,IOSTAT=IOERRN) ISSI, ISYI, IUSI, IUYI
      ELSE
         OPEN(UNIT=MFUNIT,ERR=999,FILE=METINP,FORM='FORMATTED',
     &        IOSTAT=IOERRN,STATUS='OLD')
         READ(MFUNIT,*,ERR=99,IOSTAT=IOERRN) ISSI, ISYI, IUSI, IUYI
      END IF

C     Convert 2-Digit Year to 4-Digit Value
      IF (ISYI .LE. 50) THEN
         ISYI = 2000 + ISYI
      ELSE IF (ISYI .LE. 99) THEN
         ISYI = 1900 + ISYI
      END IF
      IF (IUYI .LE. 50) THEN
         IUYI = 2000 + IUYI
      ELSE IF (IUYI .LE. 99) THEN
         IUYI = 1900 + IUYI
      END IF

C     Check Station IDs and Data Year for Errors
      IF (ISSI .NE. IDSURF) THEN
         WRITE(DUMMY,'(I8)') ISSI
         CALL ERRHDL(PATH,MODNAM,'E','530',DUMMY)
      END IF
      IF (ISYI .NE. ISYEAR) THEN
         WRITE(DUMMY,'(I8)') ISYI
         CALL ERRHDL(PATH,MODNAM,'E','530',DUMMY)
      END IF
      IF (IUSI .NE. IDUAIR) THEN
         WRITE(DUMMY,'(I8)') IUSI
         CALL ERRHDL(PATH,MODNAM,'E','530',DUMMY)
      END IF
      IF (IUYI .NE. IUYEAR) THEN
         WRITE(DUMMY,'(I8)') IUYI
         CALL ERRHDL(PATH,MODNAM,'E','530',DUMMY)
      END IF

      GO TO 1000

C     Write Out Error Message for File READ Error
 99   CALL ERRHDL(PATH,MODNAM,'E','510',' MET-INP')

      GO TO 1000

C     Write Out Error Message for File OPEN Error
 999  CALL ERRHDL(PATH,MODNAM,'E','500',' MET-INP')

 1000 RETURN
      END
      SUBROUTINE DELH(DHFOUT)
C***********************************************************************
C                 DELH Module of ISC2 Model
C
C        PURPOSE: To Calculate Final Plume Rise
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To use calling argument
C                    R. W. Brode, PES, Inc. - 9/30/94
C
C        INPUTS:  Source Parameters
C                 Meteorological Variables
C                 Buoyancy and Momentum Fluxes
C
C        OUTPUTS: Final Plume Rise, DHFOUT (m)
C
C        CALLED FROM:   PHEFF
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'DELH'

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
               DHFOUT = 38.71*(FB**0.6)/US
            ELSE
               DHFOUT = 21.425*(FB**0.75)/US
            END IF
         ELSE   
            DHFOUT = 3.*DS*VS/US
         END IF

      ELSE IF (STABLE) THEN
         DTCRIT = 0.019582*VS*TA*RTOFS
         IF (DELT .GE. DTCRIT) THEN
            BUOYNT = .TRUE. 
         ELSE   
            BUOYNT = .FALSE.
         END IF
         IF (BUOYNT) THEN   
            DHFOUT = 2.6*(FB/(US*S))**0.333333
C           Compare to Final Plume Rise for Calm Winds, DHCLM
            DHCLM = 4.*FB**0.25/S**0.375
            IF (DHCLM .LT. DHFOUT) DHFOUT = DHCLM
         ELSE   
            DHFOUT = 1.5*(FM/(US*RTOFS))**0.333333
C           Compare to Maximum Momentum Rise for UNSTABLE/NEUTRAL, DHCHK
            DHCHK = 3.*DS*VS/US
            IF (DHCHK .LT. DHFOUT) DHFOUT = DHCHK
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
      CHARACTER MODNAM*6
C     Variable Initializations
      MODNAM = 'HSPRIM'

C     Calculate Adjusted Stack Height (Eqn. 1-7)

      IF (VS .LT. 1.5*US) THEN
         HSPRIM = HS - 2.*DS*(1.5-VS/US)
      ELSE
         HSPRIM = HS
      END IF

      IF (HSPRIM .LT. 0.0)  HSPRIM = 0.0

      RETURN
      END

      SUBROUTINE DHPHS(XARG,DHFARG,DHPOUT)
C***********************************************************************
C                 DHPHS Module of ISC2 Model
C
C        PURPOSE: Calculates Distance-dependent Plume Rise for
C                 Huber-Snyder Downwash Algorithm and for BID
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To use calling arguments
C                    R. W. Brode, PES, Inc. - 9/30/94
C
C        INPUTS:  Arrays of Source Parameters
C                 Buoyancy and Momentum Fluxes
C                 Meteorological Variables for One Hour
C                 Wind Speed Adjusted to Stack Height
C                 Downwind Distance
C
C        OUTPUTS: Distance-dependent Plume Rise, DHPOUT (m)
C
C        CALLED FROM:   PHEFF
C                       BID
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'DHPHS'

C     Calculate Distance-dependent Buoyant Plume Rise (Eqn. 1-22)
      XP = AMIN1(XARG,XFB)
      IF (XP .LT. 1.0)  XP = 1.0
      IF (FB .LT. 1.0E-10)  FB = 1.0E-10
      DHPB = 1.60 * (FB*XP*XP)**0.333333 / US

C     Calculate Dist-dependent Momentum Plume Rise          ---   CALL DHPMOM
      CALL DHPMOM(XARG)

C     Select Maximum of Buoyant or Momentum Rise for Gradual Rise
      DHPOUT = AMAX1(DHPB, DHPM)
C     Compare to Final Rise and Select Smaller Value for Gradual Rise
      DHPOUT = AMIN1(DHPOUT, DHFARG)

      RETURN
      END

      SUBROUTINE DHPMOM(XARG)
C***********************************************************************
C                 DHPMOM Module of ISC2 Model
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
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'

C     Variable Initializations
      MODNAM = 'DHPMOM'

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

      SUBROUTINE CUBIC(A,B,C,ZINIT,TOL,ZITER)
C***********************************************************************
C                 CUBIC Module of ISC2 Model
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
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'MAIN3.INC'
      REAL Z(25)

C     Variable Initializations
      MODNAM = 'CUBIC'

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
      WRITE(DUMMY,'(I8)') KURDAT
      CALL ERRHDL(PATH,MODNAM,'I','400',DUMMY)
      ZITER = 0.5 * (Z(24) + Z(25))

 999  RETURN
      END
      SUBROUTINE INPSUM
C***********************************************************************
C                 INPSUM Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Print Out The Input Data Summary
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Arrays of Source Parameters
C                 Arrays of Receptor Locations
C                 Arrays of Model Results
C
C        OUTPUTS: Printed Model Outputs
C
C        CALLED FROM:   MAIN
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'INPSUM'

C     Print Out The Model Options
      CALL PRTOPT

C     Print Out The Input Source Data
      CALL PRTSRC

C     Print Out The Input Receptor Coordinates.
      CALL PRTREC

C     Check For Receptors Too Close To Sources (< 1m or < 3Lb)
      CALL CHKREC

C     Print Out The Input Met Data Summary
      CALL PRTMET

      RETURN
      END

      SUBROUTINE PRTOPT
C***********************************************************************
C                 PRTOPT Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Print Out The Model Options and Keyword Summary
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To Remove Summary of Keywords Table
C                    Roger Brode, PES, Inc.,  - 11/08/94
C
C        MODIFIED:   To add pathway 'TG' to process input file of Gridded
C                    Terrain data.
C                    D. Strimaitis, SRC - 11/8/93
C
C        MODIFIED:   To add DDEP and WDEP parameters to CONC/DEPOS options
C                    to allow just the wet or just the dry deposition flux
C                    to be reported.  DEPOS now reports the sum of wet and
C                    dry fluxes.  Expand keywords to include input of wet
C                    scavenging coefficients (SO path).  Add override of
C                    Intermediate Terrain so that results are for only the
C                    simple terrain or the complex terrain model.
C                    D. Strimaitis, SRC - 11/8/93
C
C        MODIFIED:  To Include TOXXFILE Option - 9/29/92
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

C     Variable Initializations
      MODNAM = 'PRTOPT'

C     Summarize The Model Options
      CALL HEADER
      WRITE(IOUNIT,9041)
      IF (NOSMPL) THEN
         WRITE(IOUNIT,*) '**Complex Terrain Model is Selected'
      ELSE IF (NOCMPL) THEN
         WRITE(IOUNIT,*) '**Simple Terrain Model is Selected'
      ELSE
         WRITE(IOUNIT,*) '**Intermediate Terrain Processing is Selected'
      ENDIF

      WRITE(IOUNIT,9099)
      IF (CONC) THEN
         WRITE(IOUNIT,*) '**Model Is Setup For Calculation of ',
     &        'Average CONCentration Values.'
      END IF
      IF (DEPOS) THEN
         WRITE(IOUNIT,*) '**Model Is Setup For Calculation of ',
     &        'Total DEPOSition Values.'
      END IF
      IF (DDEP) THEN
         WRITE(IOUNIT,*) '**Model Is Setup For Calculation of ',
     &        'Dry DEPosition Values.'
      END IF
      IF (WDEP) THEN
         WRITE(IOUNIT,*) '**Model Is Setup For Calculation of ',
     &        'Wet DEPosition Values.'
      END IF

      WRITE(IOUNIT,9099)
      WRITE(IOUNIT,*) '  --  SCAVENGING/DEPOSITION LOGIC --'
      IF (DDPLETE) THEN
         WRITE(IOUNIT,*) '**Model Uses DRY DEPLETION.  DDPLETE = ',
     &                    DDPLETE
      ELSE
         WRITE(IOUNIT,*) '**Model Uses NO DRY DEPLETION.  DDPLETE = ',
     &                    DDPLETE
      END IF
      IF (WDPLETE) THEN
         WRITE(IOUNIT,*) '**Model Uses WET DEPLETION.  WDPLETE = ',
     &                    WDPLETE
      ELSE
         WRITE(IOUNIT,*) '**Model Uses NO WET DEPLETION.  WDPLETE = ',
     &                    WDPLETE
      END IF
      IF (LWGAS .OR. LWPART) THEN
         WRITE(IOUNIT,*) '**SCAVENGING Data Provided.  LWGAS,LWPART = ',
     &                    LWGAS,LWPART
      ELSE
         WRITE(IOUNIT,*) '**NO WET SCAVENGING Data Provided. '
      END IF
      IF (LTGRID) THEN
         WRITE(IOUNIT,*) '**Model Uses GRIDDED TERRAIN Data for ',
     &                   'Depletion Calculations'
      ELSE
         WRITE(IOUNIT,*) '**Model Does NOT Use GRIDDED TERRAIN Data ',
     &                   'for Depletion Calculations'
      END IF

      WRITE(IOUNIT,9099)
      IF (RURAL) THEN
         WRITE(IOUNIT,*) '**Model Uses RURAL Dispersion.'
      ELSE IF (URBAN) THEN
         WRITE(IOUNIT,*) '**Model Uses URBAN Dispersion.'
      END IF

      WRITE(IOUNIT,9099)
      IF (DFAULT) THEN
         WRITE(IOUNIT,*) '**Model Uses Regulatory DEFAULT Options:'
         WRITE(IOUNIT,*) '           1. Final Plume Rise.'
         WRITE(IOUNIT,*) '           2. Stack-tip Downwash.'
         WRITE(IOUNIT,*) '           3. Buoyancy-induced ',
     &           'Dispersion.'
         WRITE(IOUNIT,*) '           4. Use Calms Processing ',
     &           'Routine.'
         WRITE(IOUNIT,*) '           5. Not Use Missing Data ',
     &           'Processing Routine.'
         WRITE(IOUNIT,*) '           6. Default Wind Profile Exponents.'
         WRITE(IOUNIT,*) '           7. Default Vertical Potential',
     &           ' Temperature Gradients.'
         WRITE(IOUNIT,*) '           8. "Upper Bound" Values ',
     &           'for Supersquat Buildings.'
         IF (URBAN .AND. POLLUT .EQ. 'SO2') THEN
            WRITE(IOUNIT,*) '           9. Half-life of 4 hrs for',
     &              ' URBAN SO2.'
         ELSE IF (URBAN .AND. POLLUT .NE. 'SO2') THEN
            WRITE(IOUNIT,*) '           9. No Exponential Decay for',
     &              ' URBAN/Non-SO2'
         ELSE
            WRITE(IOUNIT,*) '           9. No Exponential Decay for',
     &              ' RURAL Mode'
         END IF
      ELSE
         WRITE(IOUNIT,*) '**Model Uses User-Specified Options:'
         IF (GRDRIS) THEN
            WRITE(IOUNIT,*) '           1. Gradual Plume Rise.'
         ELSE
            WRITE(IOUNIT,*) '           1. Final Plume Rise.'
         END IF
         IF (NOSTD) THEN
            WRITE(IOUNIT,*) '           2. Not Use Stack-tip ',
     &           'Downwash.'
         ELSE
            WRITE(IOUNIT,*) '           2. Stack-tip Downwash.'
         END IF
         IF (NOBID) THEN
            WRITE(IOUNIT,*) '           3. Not Use Buoyancy-induced ',
     &           'Dispersion.'
         ELSE
            WRITE(IOUNIT,*) '           3. Buoyancy-induced ',
     &           'Dispersion.'
         END IF
         IF (NOCALM) THEN
            WRITE(IOUNIT,*) '           4. Not Use Calms Processing ',
     &           'Routine.'
         ELSE
            WRITE(IOUNIT,*) '           4. Calms Processing ',
     &           'Routine.'
         END IF
         IF (MSGPRO) THEN
            WRITE(IOUNIT,*) '           5. Missing Data Processing ',
     &           'Routine.'
         ELSE
            WRITE(IOUNIT,*) '           5. Not Use Missing Data ',
     &           'Processing Routine.'
         END IF
         IF (USERP) THEN
            WRITE(IOUNIT,*) '           6. User-Specified Wind Profile',
     &           ' Exponents.'
         ELSE
            WRITE(IOUNIT,*) '           6. Default Wind Profile',
     &           ' Exponents.'
         END IF
         IF (USERDT) THEN
            WRITE(IOUNIT,*) '           7. User-Specified Vertical ',
     &           'Potential Temperature Gradients.'
         ELSE
            WRITE(IOUNIT,*) '           7. Default Vertical Potential',
     &           ' Temperature Gradients.'
         END IF
      END IF
      
C*----   ISCSTM Modification: allow for NOCHKD option - jah 11/2/94                           
      IF (NOCHKD) THEN
         WRITE(IOUNIT,*) '        NOCHKD - Suppresses checking',
     &                   ' of date sequence in meteorology files'
      END IF
C*#
      WRITE(IOUNIT,9099)
      IF (FLAT) THEN
         WRITE(IOUNIT,*) '**Model Assumes Receptors on FLAT Terrain.'
      ELSE IF (ELEV) THEN
         WRITE(IOUNIT,*) '**Model Accepts Receptors on ELEV Terrain.'
      END IF

      WRITE(IOUNIT,9099)
      IF (FLGPOL) THEN
         WRITE(IOUNIT,*) '**Model Accepts FLAGPOLE Receptor Heights.'
      ELSE
         WRITE(IOUNIT,*) '**Model Assumes No FLAGPOLE Receptor Heights.'
      END IF

C     Model Sources And Receptors Summary
      WRITE(IOUNIT,9099)
      IF (PERIOD) THEN
         IF (NUMAVE .GT. 0) THEN
            WRITE(IOUNIT,9042) NUMAVE, (CHRAVE(I),I=1,NUMAVE)
            WRITE(IOUNIT,9043)
         ELSE
            WRITE(IOUNIT,9045)
         END IF
      ELSE IF (ANNUAL) THEN
         IF (NUMAVE .GT. 0) THEN
            WRITE(IOUNIT,9042) NUMAVE, (CHRAVE(I),I=1,NUMAVE)
            WRITE(IOUNIT,9143)
         ELSE
            WRITE(IOUNIT,9145)
         END IF
      ELSE
         WRITE(IOUNIT,9042) NUMAVE, (CHRAVE(I),I=1,NUMAVE)
      END IF

C     Write Out Numbers of Sources, Groups, and Receptors for This Run
      WRITE(IOUNIT,9099)
      WRITE(IOUNIT,9044) NUMSRC, NUMGRP, NUMREC

C     Write Out Pollutant Type
      WRITE(IOUNIT,9099)
      WRITE(IOUNIT,9048) POLLUT

C     Model Run OR Not Options
      WRITE(IOUNIT,9099)
      IF (RUN) THEN
         WRITE(IOUNIT,*) '**Model Set To Continue RUNning After the ',
     &         'Setup Testing.'
      ELSE
         WRITE(IOUNIT,*) '**Model Will NOT Run After the ',
     &         'Setup Testing.'
      END IF

C     Model Output Options Setting Summary
      WRITE(IOUNIT,9099)
      WRITE(IOUNIT,9070)
      IF (PERIOD) THEN
C        PERIOD Averages by Receptor Are Output
         WRITE(IOUNIT,9071)
      ELSE IF (ANNUAL) THEN
C        ANNUAL Averages by Receptor Are Output
         WRITE(IOUNIT,9171)
      END IF
      IF (IOSTAT(2) .GT. 0) THEN
C        RECTABLE Keyword Used
         WRITE(IOUNIT,9072)
      END IF
      IF (IOSTAT(3) .GT. 0) THEN
C        MAXTABLE Keyword Used
         WRITE(IOUNIT,9073)
      END IF
      IF (IOSTAT(4) .GT. 0) THEN
C        DAYTABLE Keyword Used
         WRITE(IOUNIT,9074)
      END IF
      IF (IOSTAT(5) .GT. 0) THEN
C        MAXIFILE Keyword Used
         WRITE(IOUNIT,9075)
      END IF
      IF (IOSTAT(6) .GT. 0) THEN
C        POSTFILE Keyword Used
         WRITE(IOUNIT,9076)
      END IF
      IF (IOSTAT(7) .GT. 0) THEN
C        PLOTFILE Keyword Used
         WRITE(IOUNIT,9077)
      END IF
      IF (IOSTAT(8) .GT. 0) THEN
C        TOXXFILE Keyword Used
         WRITE(IOUNIT,9078)
      END IF

C     Write Explanatory Note About Calm and Missing Flags
      IF (CLMPRO .OR. MSGPRO) THEN
         WRITE(IOUNIT,9099)
         WRITE(IOUNIT,9079) CHIDEP(3,1)
      END IF

C     Model Misc. Information
      WRITE(IOUNIT,9099)
      WRITE(IOUNIT,9050) ZREF, DECOEF, ROTANG
      WRITE(IOUNIT,9055) EMILBL(1), EMIFAC(1), OUTLBL(1)

C     Model I/O Setting Summary
      WRITE(IOUNIT,9099)
      IF (INPFIL .NE. ' ' .OR. OUTFIL .NE. ' ') THEN
         WRITE(IOUNIT,9080) INPFIL, OUTFIL
      END IF
      IF (EVENTS) WRITE(IOUNIT,*) '**File Created for Event Model:  ',
     &                             EVFILE
      IF (MULTYR) THEN
         WRITE(IOUNIT,*) '**This Run is Part of a Multi-year Run.'
         WRITE(IOUNIT,*) '  NOTE:  PERIOD Results Are for Current ',
     &                   'Period Only.'
         WRITE(IOUNIT,*) '         Short Term Results Are Cumulative',
     &                   ' Across All Years Processed.'
      END IF
      IF (RSTSAV) WRITE(IOUNIT,*) '**File for Saving Result Arrays: ',
     &                             SAVFIL
      IF (RSTINP) WRITE(IOUNIT,*) '**File for Initializing Result',
     &                            ' Arrays: ',INIFIL
      IF (ERRLST) WRITE(IOUNIT,*) '**Detailed Error/Message File:   ',
     &                             MSGFIL

 9041 FORMAT(44X,'***     MODEL SETUP OPTIONS SUMMARY       ***'/
     &       63(' -')/)
 9042 FORMAT(1X,'**Model Calculates ',I2,' Short Term Average(s)',
     &       ' of:  ',8(A5,2X,:))
 9043 FORMAT(1X,'    and Calculates PERIOD Averages')
 9045 FORMAT(1X,'**Model Calculates PERIOD Averages Only')
 9143 FORMAT(1X,'    and Calculates ANNUAL Averages')
 9145 FORMAT(1X,'**Model Calculates ANNUAL Averages Only')
 9044 FORMAT(1X,'**This Run Includes: ',I4,' Source(s);   ',I4,
     &       ' Source Group(s); and  ',I5,' Receptor(s)')
 9048 FORMAT(1X,'**The Model Assumes A Pollutant Type of:  ',A8)
 9050 FORMAT(1X,'**Misc. Inputs:  Anem. Hgt. (m) = ',F8.2,
     &       ' ;    Decay Coef. = ',G12.4,' ;    Rot. Angle = ',F7.1)
 9055 FORMAT(18X,'Emission Units = ',A40,' ;  Emission Rate Unit ',
     &       'Factor = ',G13.5,
     &      /18X,'Output Units   = ',A40)
 9070 FORMAT(1X,'**Output Options Selected:')
 9071 FORMAT(10X,'Model Outputs Tables of PERIOD Averages by Receptor')
 9171 FORMAT(10X,'Model Outputs Tables of ANNUAL Averages by Receptor')
 9072 FORMAT(10X,'Model Outputs Tables of Highest Short Term Values by',
     &       ' Receptor (RECTABLE Keyword)')
 9073 FORMAT(10X,'Model Outputs Tables of Overall Maximum Short Term',
     &       ' Values (MAXTABLE Keyword)')
 9074 FORMAT(10X,'Model Outputs Tables of Concurrent Short Term Values',
     &       ' by Receptor for Each Day Processed (DAYTABLE Keyword)')
 9075 FORMAT(10X,'Model Outputs External File(s) of Threshold',
     &       ' Violations (MAXIFILE Keyword)')
 9076 FORMAT(10X,'Model Outputs External File(s) of Concurrent Values',
     &       ' for Postprocessing (POSTFILE Keyword)')
 9077 FORMAT(10X,'Model Outputs External File(s) of High Values for',
     &       ' Plotting (PLOTFILE Keyword)')
 9078 FORMAT(10X,'Model Outputs External File(s) of Values for Input',
     &       ' to TOXX Model (TOXXFILE Keyword)')
 9079 FORMAT(1X,'**NOTE:  The Following Flags May Appear Following ',
     &       A4,' Values:  c for Calm Hours',
     &               /65X,'m for Missing Hours',
     &               /65X,'b for Both Calm and Missing Hours')
 9080 FORMAT(1X,'**Input Runstream File: ',A40,';  **Output Print ',
     &       'File: ',A40)
 9099 FORMAT(1X,' ')

      RETURN
      END

      SUBROUTINE PRTREC
C***********************************************************************
C                 PRTREC Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Print Out The Receptor Network Values
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To Adjust Format Statement 9082 for Boundary
C                    Receptors - 9/29/92
C
C        INPUTS:  Arrays of Source Parameters
C                 Arrays of Receptor Locations
C                 Arrays of Model Results
C
C        OUTPUTS: Printed Model Outputs
C
C        CALLED FROM:   INPSUM
C***********************************************************************

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      CHARACTER BUF132*132

C     Variable Initializations
      MODNAM = 'PRTREC'

      DO 100 I = 1, INNET
         CALL HEADER
         WRITE(IOUNIT,9034)
         WRITE(IOUNIT,9037) NTID(I), NTTYP(I)
         IF (NTTYP(I) .EQ. 'GRIDCART') THEN
            WRITE(IOUNIT,9038)
         ELSE
            WRITE(IOUNIT,9036) XORIG(I), YORIG(I)
            WRITE(IOUNIT,9039)
         END IF
         WRITE(IOUNIT,9040) (XCOORD(J,I),J=1,NUMXPT(I))
         IF (NTTYP(I) .EQ. 'GRIDCART') THEN
            WRITE(IOUNIT,9041)
         ELSE
            WRITE(IOUNIT,9042)
         END IF
         WRITE(IOUNIT,9040) (YCOORD(J,I),J=1,NUMYPT(I))
         IF (ELEV) THEN
C           Print Terrain Heights for Network
C           Set Number of Columns Per Page, NCPP
            NCPP = 9
C           Set Number of Rows Per Page, NRPP
            NRPP = 40
C           Begin LOOP Through Networks
C           Calculate Number of Pages Per X-Group, NPPX, & Per Y-Group, NPPY
            NPPX = 1 + INT((NUMXPT(I)-1)/NCPP)
            NPPY = 1 + INT((NUMYPT(I)-1)/NRPP)
            DO 40 NX = 1, NPPX
               DO 30 NY = 1, NPPY
                  CALL HEADER
                  WRITE(IOUNIT,9037) NTID(I), NTTYP(I)
                  WRITE(IOUNIT,9011)
                  IF (NX .EQ. NPPX) THEN
                     IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                        WRITE(IOUNIT,9016)
                        WRITE(IOUNIT,9017) (XCOORD(J,I),J=1+NCPP*(NX-1),
     &                                                    NUMXPT(I))
                     ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                        WRITE(IOUNIT,9018)
                        WRITE(IOUNIT,9019) (XCOORD(J,I),J=1+NCPP*(NX-1),
     &                                                    NUMXPT(I))
                     END IF
                  ELSE
                     IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                        WRITE(IOUNIT,9016)
                        WRITE(IOUNIT,9017) (XCOORD(J,I),J=1+NCPP*(NX-1),
     &                                                    NCPP*NX)
                     ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                        WRITE(IOUNIT,9018)
                        WRITE(IOUNIT,9019) (XCOORD(J,I),J=1+NCPP*(NX-1),
     &                                                    NCPP*NX)
                     END IF
                  END IF
                  WRITE(IOUNIT,9010)
                  IF (NY .EQ. NPPY) THEN
                     DO 20 K = 1+NRPP*(NY-1), NUMYPT(I)
                        IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                           INDZ = NETEND(I) - K*NUMXPT(I) + 1
                           YCOVAL = YCOORD(NUMYPT(I)-K+1,I)
                        ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                           INDZ = NETSTA(I) + (K-1)*NUMXPT(I)
                           YCOVAL = YCOORD(K,I)
                        END IF
                        IF (NX .EQ. NPPX) THEN
                           WRITE(IOUNIT,9013) YCOVAL,
     &                   (AZELEV(INDZ+J-1),J=1+NCPP*(NX-1),NUMXPT(I))
                        ELSE
                           WRITE(IOUNIT,9013) YCOVAL,
     &                   (AZELEV(INDZ+J-1),J=1+NCPP*(NX-1),NCPP*NX)
                        END IF
 20                  CONTINUE
                  ELSE
                     DO 25 K = 1+NRPP*(NY-1), NRPP*NY
                        IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                           INDZ = NETEND(I) - K*NUMXPT(I) + 1
                           YCOVAL = YCOORD(NUMYPT(I)-K+1,I)
                        ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                           INDZ = NETSTA(I) + (K-1)*NUMXPT(I)
                           YCOVAL = YCOORD(K,I)
                        END IF
                        IF (NX .EQ. NPPX) THEN
                           WRITE(IOUNIT,9013) YCOVAL,
     &                   (AZELEV(INDZ+J-1),J=1+NCPP*(NX-1),NUMXPT(I))
                        ELSE
                           WRITE(IOUNIT,9013) YCOVAL,
     &                   (AZELEV(INDZ+J-1),J=1+NCPP*(NX-1),NCPP*NX)
                        END IF
 25                  CONTINUE
                  END IF
 30            CONTINUE
 40         CONTINUE
         END IF
         IF (FLGPOL) THEN
C           Print The Receptor Heights Above Ground for This Network
C           Set Number of Columns Per Page, NCPP
            NCPP = 9
C           Set Number of Rows Per Page, NRPP
            NRPP = 40
C           Begin LOOP Through Networks
C           Calculate Number of Pages Per X-Group, NPPX, & Per Y-Group, NPPY
            NPPX = 1 + INT((NUMXPT(I)-1)/NCPP)
            NPPY = 1 + INT((NUMYPT(I)-1)/NRPP)
            DO 80 NX = 1, NPPX
               DO 70 NY = 1, NPPY
                  CALL HEADER
                  WRITE(IOUNIT,9037) NTID(I), NTTYP(I)
                  WRITE(IOUNIT,9035)
                  IF (NX .EQ. NPPX) THEN
                     IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                        WRITE(IOUNIT,9016)
                        WRITE(IOUNIT,9017) (XCOORD(J,I),J=1+NCPP*(NX-1),
     &                                                    NUMXPT(I))
                     ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                        WRITE(IOUNIT,9018)
                        WRITE(IOUNIT,9019) (XCOORD(J,I),J=1+NCPP*(NX-1),
     &                                                    NUMXPT(I))
                     END IF
                  ELSE
                     IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                        WRITE(IOUNIT,9016)
                        WRITE(IOUNIT,9017) (XCOORD(J,I),J=1+NCPP*(NX-1),
     &                                                    NCPP*NX)
                     ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                        WRITE(IOUNIT,9018)
                        WRITE(IOUNIT,9019) (XCOORD(J,I),J=1+NCPP*(NX-1),
     &                                                    NCPP*NX)
                     END IF
                  END IF
                  WRITE(IOUNIT,9010)
                  IF (NY .EQ. NPPY) THEN
                     DO 50 K = 1+NRPP*(NY-1), NUMYPT(I)
                        IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                           INDZ = NETEND(I) - K*NUMXPT(I) + 1
                           YCOVAL = YCOORD(NUMYPT(I)-K+1,I)
                        ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                           INDZ = NETSTA(I) + (K-1)*NUMXPT(I)
                           YCOVAL = YCOORD(K,I)
                        END IF
                        IF (NX .EQ. NPPX) THEN
                           WRITE(IOUNIT,9013) YCOVAL,
     &                   (AZFLAG(INDZ+J-1),J=1+NCPP*(NX-1),NUMXPT(I))
                        ELSE
                           WRITE(IOUNIT,9013) YCOVAL,
     &                   (AZFLAG(INDZ+J-1),J=1+NCPP*(NX-1),NCPP*NX)
                        END IF
 50                  CONTINUE
                  ELSE
                     DO 60 K = 1+NRPP*(NY-1), NRPP*NY
                        IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                           INDZ = NETEND(I) - K*NUMXPT(I) + 1
                           YCOVAL = YCOORD(NUMYPT(I)-K+1,I)
                        ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                           INDZ = NETSTA(I) + (K-1)*NUMXPT(I)
                           YCOVAL = YCOORD(K,I)
                        END IF
                        IF (NX .EQ. NPPX) THEN
                           WRITE(IOUNIT,9013) YCOVAL,
     &                   (AZFLAG(INDZ+J-1),J=1+NCPP*(NX-1),NUMXPT(I))
                        ELSE
                           WRITE(IOUNIT,9013) YCOVAL,
     &                   (AZFLAG(INDZ+J-1),J=1+NCPP*(NX-1),NCPP*NX)
                        END IF
 60                  CONTINUE
                  END IF
 70            CONTINUE
 80         CONTINUE
         END IF
 100  CONTINUE

      IF (IRSTAT(4) .NE. 0) THEN
C        Print Out The Coordinates, Height & Flags For Discrete Cart Receptors
         INDC = 0
         DO 1030 I = 1, NUMREC
            IF (RECTYP(I) .EQ. 'DC') THEN
               INDC = INDC + 1
               IF (MOD(INDC-1,90) .EQ. 0) THEN
                  CALL HEADER
                  WRITE(IOUNIT,9043)
               END IF
               IF (MOD(INDC,2) .NE. 0) THEN
                  WRITE(BUF132(1:55),9045) AXR(I),AYR(I),AZELEV(I),
     &                                     AZFLAG(I)
               ELSE
                  WRITE(BUF132(56:110),9045) AXR(I),AYR(I),AZELEV(I),
     &                                       AZFLAG(I)
                  WRITE(IOUNIT,9090) BUF132
                  WRITE(BUF132,9095)
               END IF
            END IF
 1030    CONTINUE
         IF (MOD(INDC,2) .NE. 0) THEN
            WRITE(IOUNIT,9090) BUF132
            WRITE(BUF132,9095)
         END IF
      END IF

      IF (IRSTAT(5) .NE. 0) THEN
C        Print Out The Coordinates, Height & Flags For Discrete Polar Receptors
         INDC = 0
         DO 1040 I = 1, NUMREC
            IF (RECTYP(I) .EQ. 'DP') THEN
               INDC = INDC + 1
               XRMS = AXR(I) - AXS(IREF(I))
               YRMS = AYR(I) - AYS(IREF(I))
               RANGE  = SQRT(XRMS*XRMS + YRMS*YRMS)
               RADIAL = ATAN2(XRMS, YRMS) * RTODEG
               IF(RADIAL .LE. 0.0) RADIAL = RADIAL + 360.
               IF (MOD(INDC-1,90) .EQ. 0) THEN
                  CALL HEADER
                  WRITE(IOUNIT,9044)
               END IF
               IF (MOD(INDC,2) .NE. 0) THEN
                  WRITE(BUF132(1:65),9047) SRCID(IREF(I)),RANGE,RADIAL,
     &                                     AZELEV(I),AZFLAG(I)
               ELSE
                  WRITE(BUF132(66:130),9047) SRCID(IREF(I)),RANGE,
     &                                       RADIAL,AZELEV(I),AZFLAG(I)
                  WRITE(IOUNIT,9090) BUF132
                  WRITE(BUF132,9095)
               END IF
            END IF
 1040    CONTINUE
         IF (MOD(INDC,2) .NE. 0) THEN
            WRITE(IOUNIT,9090) BUF132
            WRITE(BUF132,9095)
         END IF
      END IF

      IF (IRSTAT(6) .NE. 0) THEN
C        Write Out The Boundary Receptors For The Sources
         INDC = 0
         I = 1
         DO WHILE (I .LE. NUMREC)
            IF (RECTYP(I) .EQ. 'BD') THEN
               INDC = INDC + 1
               ISRF = IREF(I)
               IF (MOD(INDC-1,3) .EQ. 0) THEN
                  CALL HEADER
                  WRITE(IOUNIT,9084)
               END IF
               WRITE(IOUNIT,9082) SRCID(ISRF),SRCTYP(ISRF),
     &             AXS(ISRF),AYS(ISRF),AZS(ISRF),
     &             (J,AXR(I+J-1),AYR(I+J-1),AZELEV(I+J-1),
     &              AZFLAG(I+J-1),J=1,36)
               I = I + 36
            ELSE
               I = I + 1
            END IF
         END DO
      END IF

 9011 FORMAT(/48X,'* ELEVATION HEIGHTS IN METERS *'/)
 9035 FORMAT(/44X,'* RECEPTOR FLAGPOLE HEIGHTS IN METERS *'/)
 9034 FORMAT(/40X,'*** GRIDDED RECEPTOR NETWORK SUMMARY ***')
 9037 FORMAT(/34X,'*** NETWORK ID: ',A8,' ;  NETWORK TYPE: ',
     &       A8,' ***')
 9038 FORMAT(/42X,'*** X-COORDINATES OF GRID ***'/
     &       52X,'(METERS)'/)
 9039 FORMAT(/42X,'*** DISTANCE RANGES OF NETWORK ***'/
     &       52X,'(METERS)'/)
 9036 FORMAT(/42X,'*** ORIGIN FOR POLAR NETWORK ***'/,
     &      32X,'X-ORIG =',F10.2,' ;   Y-ORIG = ',F10.2,'  (METERS)')
 9040 FORMAT(100(5X,10(F10.1,',')/))
 9041 FORMAT(/42X,'*** Y-COORDINATES OF GRID *** ',
     &       /52X,'(METERS)'/)
 9042 FORMAT(/42X,'*** DIRECTION RADIALS OF NETWORK *** ',
     &       /52X,'(DEGREES)'/)
 9010 FORMAT(66(' -')/)
 9013 FORMAT(2X,F10.2,1X,'|',1X,9(1X,F12.2,:))
 9016 FORMAT(3X,' Y-COORD  |',48X,'X-COORD (METERS)')
 9017 FORMAT(3X,' (METERS) |',1X,9(1X,F12.2,:))
 9018 FORMAT(3X,'DIRECTION |',48X,'DISTANCE (METERS)')
 9019 FORMAT(3X,'(DEGREES) |',1X,9(1X,F12.2,:))
 9043 FORMAT(/45X,'*** DISCRETE CARTESIAN RECEPTORS ***',
     &       /45X,'  (X-COORD, Y-COORD, ZELEV, ZFLAG)',
     &       /45X,'              (METERS)'/)
 9044 FORMAT(/45X,' *** DISCRETE POLAR RECEPTORS ***',
     &       /45X,' ORIGIN: (DIST, DIR, ZELEV, ZFLAG)',
     &       /45X,' SRCID: (METERS,DEG,METERS,METERS)'/)
 9045 FORMAT(4X,' (',3(F9.1,', '),F9.1,'); ')
 9047 FORMAT(4X,A8,': (',3(F9.1,', '),F9.1,'); ')
 9082 FORMAT(' BOUNDARY RECEPTORS FOR SOURCE ID: ',A8,/,5X,
     &       ' OF SOURCE TYPE: ',A8,'; WITH ORIGIN AT (',2(F10.2,', '),
     &     F10.2,')'/3(' SEC.    XCOORD      YCOORD   ZELEV  ZFLAG',3X),
     &       /,12(3(I4,1X,F10.2,', ',F10.2,',',F7.2,',',F6.1,3X),/),/)
 9084 FORMAT(/50X,'*** BOUNDARY RECEPTOR LOCATIONS ***',
     &    /47X,'(DISCRETE RECEPTORS AT 10 DEGREE SECTORS)'//)
 9090 FORMAT(A132)
 9095 FORMAT(132(' '))

      RETURN
      END

      SUBROUTINE PRTMET
C***********************************************************************
C                 PRTMET Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Print Out The Input Met Data Summary and Source Groups
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
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
      CHARACTER ATHRUF(6)*1

C     Variable Initializations
      DATA ATHRUF / 'A','B','C','D','E','F' /
      MODNAM = 'PRTMET'

C     Start New Page and Print The Titles
      CALL HEADER

C     Print The Meteorology Data Date Array.
      WRITE(IOUNIT,9037) (IPROC(I),I = 1, 366)

      IF (ISDATE .NE. 0 .OR. IEDATE .NE. 99999999) THEN
C        Write Out User-specified Start and End Dates
         WRITE(IOUNIT,9038) ISYR, ISMN, ISDY, ISHR,
     &                      IEYR, IEMN, IEDY, IEHR
      END IF

      WRITE(IOUNIT,9039)

C     Print Upper Bound Of First 5 Wind Speed Categories.
      WRITE(IOUNIT,9001) (UCAT(I),I=1,5)

C     Print Wind Profile Exponents
      IF (DFAULT .OR. .NOT.USERP) THEN
         IF (URBAN) THEN
            DO 111 I = 1, 6
               DO 110 J = 1, 6
                  PUSER(I,J) = PURB(I)
  110          CONTINUE
  111       CONTINUE
         ELSE IF (RURAL) THEN
            DO 222 I = 1, 6
               DO 220 J = 1, 6
                  PUSER(I,J) = PRUR(I)
  220          CONTINUE
  222       CONTINUE
         END IF
      END IF
      WRITE(IOUNIT,9059)
      WRITE(IOUNIT,9016) (K,K=1,6)
      DO 490  I = 1, 6
         WRITE(IOUNIT,9017) ATHRUF(I), (PUSER(I,J),J=1,6)
  490 CONTINUE

C     Print Vertical Potential Temperature Gradients
      IF (DFAULT .OR. .NOT.USERDT) THEN
         IF (URBAN) THEN
            DO 333 I = 1, 6
               DO 330 J = 1, 6
                  DTUSER(I,J) = DTURB(I)
  330          CONTINUE
  333       CONTINUE
         ELSE IF (RURAL) THEN
            DO 444 I = 1, 6
               DO 440 J = 1, 6
                  DTUSER(I,J) = DTRUR(I)
  440          CONTINUE
  444       CONTINUE
         END IF
      END IF
      WRITE(IOUNIT,9060)
      WRITE(IOUNIT,9016) (K, K=1,6)
      DO 520  I = 1, 6
         WRITE(IOUNIT,9017) ATHRUF(I), (DTUSER(I,J),J=1,6)
  520 CONTINUE

 9001 FORMAT(//34X,'*** UPPER BOUND OF FIRST THROUGH FIFTH WIND SPEED',
     &       ' CATEGORIES ***'/60X,'(METERS/SEC)'//46X,5(F7.2,','))
 9016 FORMAT(16X,'STABILITY',29X,'WIND SPEED CATEGORY'/16X,'CATEGORY',
     &       9X,6(I1,14X))
 9017 FORMAT(19X,A1,5X,6(5X,E10.5))
 9037 FORMAT(/44X,'*** METEOROLOGICAL DAYS SELECTED FOR PROCESSING ***'
     &       /63X,'(1=YES; 0=NO)'//8(11X,5(10I2,2X)/))
 9038 FORMAT(/23X,'METEOROLOGICAL DATA PROCESSED BETWEEN START DATE: ',
     &       4I3,/59X,'AND END DATE: ',4I3)
 9039 FORMAT(/16X,'NOTE:  METEOROLOGICAL DATA ACTUALLY PROCESSED WILL',
     &       ' ALSO DEPEND ON WHAT IS INCLUDED IN THE DATA FILE.'/)
 9059 FORMAT(//51X,'*** WIND PROFILE EXPONENTS ***'//)
 9060 FORMAT(//42X,'*** VERTICAL POTENTIAL TEMPERATURE GRADIENTS ***'/
     &       53X,'(DEGREES KELVIN PER METER)'//)

      RETURN
      END

      SUBROUTINE RSINIT
C***********************************************************************
C                 RSINIT Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: To Initialize Results Variables for Restart
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
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
      MODNAM = 'RSINIT'

      READ(IRSUNT,ERR=99,END=999) ISDATE
      READ(IRSUNT,ERR=99,END=999) NHIVAL, NMXVAL, NUMREC, NUMGRP, NUMAVE,
     &                            NUMTYP

      IF (NHIVAL .GT. 0) THEN
         READ(IRSUNT,ERR=99,END=999) (((((HIVALU(I,J,K,L,M),I=1,NUMREC),
     &                   J=1,NHIVAL),K=1,NUMGRP),L=1,NUMAVE),M=1,NUMTYP)
         READ(IRSUNT,ERR=99,END=999) (((((NHIDAT(I,J,K,L,M),I=1,NUMREC),
     &                   J=1,NHIVAL),K=1,NUMGRP),L=1,NUMAVE),M=1,NUMTYP)
         READ(IRSUNT,ERR=99,END=999) (((((HCLMSG(I,J,K,L,M),I=1,NUMREC),
     &                   J=1,NHIVAL),K=1,NUMGRP),L=1,NUMAVE),M=1,NUMTYP)
      END IF

      IF (NMXVAL .GT. 0) THEN
         READ(IRSUNT,ERR=99,END=999) ((((RMXVAL(I,J,K,L),I=1,NMXVAL),
     &                               J=1,NUMGRP),K=1,NUMAVE),L=1,NUMTYP)
         READ(IRSUNT,ERR=99,END=999) ((((MXDATE(I,J,K,L),I=1,NMXVAL),
     &                               J=1,NUMGRP),K=1,NUMAVE),L=1,NUMTYP)
         READ(IRSUNT,ERR=99,END=999) ((((MXLOCA(I,J,K,L),I=1,NMXVAL),
     &                               J=1,NUMGRP),K=1,NUMAVE),L=1,NUMTYP)
         READ(IRSUNT,ERR=99,END=999) ((((MCLMSG(I,J,K,L),I=1,NMXVAL),
     &                               J=1,NUMGRP),K=1,NUMAVE),L=1,NUMTYP)
      END IF

      IF (PERIOD .OR. ANNUAL) THEN
         READ(IRSUNT,ERR=99,END=999) IANHRS, IANCLM, IANMSG
         READ(IRSUNT,ERR=99,END=999) (((ANNVAL(I,J,K),I=1,NUMREC),
     &                                    J=1,NUMGRP),K=1,NUMTYP)
      END IF

      IF (MULTYR .AND. (PERIOD .OR. ANNUAL)) THEN
C        Reinitialize the ANNVAL Array and Annual Counters
         DO 300 K = 1, NUMTYP
            DO 200 J = 1, NUMGRP
               DO 100 I = 1, NUMREC
                  ANNVAL(I,J,K) = 0.0
 100           CONTINUE
 200        CONTINUE
 300     CONTINUE
         IANHRS = 0
         IANCLM = 0
         IANMSG = 0
C        Read the Maximum Annual Values
         READ(IRSUNT,ERR=99,END=999) (((AMXVAL(I,J,K),I=1,NVAL),
     &                                    J=1,NUMGRP),K=1,NUMTYP)
         READ(IRSUNT,ERR=99,END=999) (((IMXLOC(I,J,K),I=1,NVAL),
     &                                    J=1,NUMGRP),K=1,NUMTYP)
      END IF

      GO TO 1000

C     WRITE Error Message:  Error Reading INITFILE
 99   DUMMY = 'INITFILE'
      CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)
      GO TO 1000

C     WRITE Error Message:  End of File Reached for INITFILE
 999  DUMMY = 'INITFILE'
      CALL ERRHDL(PATH,MODNAM,'E','580',DUMMY)

 1000 RETURN
      END

      SUBROUTINE RESINI
C***********************************************************************
C                 RESINI Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: To Initialize Results Variables With Zeroes
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
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
      MODNAM = 'RESINI'

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
                     HMAX(I,K,L,M)   = 0.0
                     HMDATE(I,K,L,M) = 0
                     HMLOC(I,K,L,M)  = 0
                     HMCLM(I,K,L,M)  = ' '
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

      RETURN
      END
      SUBROUTINE SETUP
C***********************************************************************
C                 SETUP Module of ISC2 Short Term Model - ISCST2
C
C        PURPOSE: Controls Processing of Run SETUP Information
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C        MODIFIED BY D. Strimaitis, SRC (for GRIDDED TERRAIN Processing)
C
C        MODIFIED:   Moved the code to insert a blank line in temporary event
C                    file after each pathway from SUB EVEFIL.
C                    R.W. Brode, PES, Inc. - November 15, 1995.
C
C        MODIFIED:  Default format for METFRM modified to eliminate the
C                   variable ZDM on input.
C                   BY:  J. Paumier, PES              DATE: 27 July 1994
C
C        DATE:    December 15, 1993
C
C        INPUTS:  Input Runstream File
C
C        OUTPUTS: Processing Option Switches
C                 Arrays of Source Parameters
C                 Arrays of Receptor Locations
C                 Meteorological Data Specifications
C                 Terrain Grid Data Specifications
C                 Output Options
C
C        CALLED FROM:   MAIN
C***********************************************************************
C
C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      LOGICAL NOPATH, NOKEY
      CHARACTER RDFRM*20, ECFRM*20, EVFRM*20

C     Variable Initializations
      MODNAM = 'SETUP'
      EOF = .FALSE.
      ILINE = 0

C     Setup READ format and ECHO format for runstream record,
C     based on the ISTRG PARAMETER (set in MAIN1.INC)
      WRITE(RDFRM,9100) ISTRG, ISTRG
 9100 FORMAT('(A',I3.3,',T1,',I3.3,'A1)')
      WRITE(ECFRM,9250) ISTRG
 9250 FORMAT('(1X,A',I3.3,')')
      WRITE(EVFRM,9300) ISTRG
 9300 FORMAT('(A',I3.3,')')

C     LOOP Through Input Runstream Records
      DO WHILE (.NOT. EOF)

C        Increment the Line Counter
         ILINE = ILINE + 1

C        READ Record to Buffers, as A80 and 80A1 for ISTRG = 80.
C        Length of ISTRG is Set in PARAMETER Statement in MAIN1.INC
         READ (INUNIT,RDFRM,END=999) RUNST1, (RUNST(I), I = 1, ISTRG)

C        Convert Lower Case to Upper Case Letters           ---   CALL LWRUPR
         CALL LWRUPR

C        Define Fields on Card                              ---   CALL DEFINE
         CALL DEFINE

C        Get the Contents of the Fields                     ---   CALL GETFLD
         CALL GETFLD

         IF (ECHO .AND.
     &            (FIELD(1).EQ.'OU' .AND. FIELD(2).EQ.'FINISHED')) THEN
C           Echo Last Input Card to Output File (Use Character Substring to
C           Avoid Echoing ^Z Which May Appear at "End of File" for Some
C           Editors).  Also, Allow for Shift in the Input Runstream File of
C           Up to 3 Columns.
            IF (LOCB(1) .EQ. 1) THEN
               WRITE(IOUNIT,9200) RUNST1(1:11)
 9200          FORMAT(' ',A11)
            ELSE IF (LOCB(1) .EQ. 2) THEN
               WRITE(IOUNIT,9210) RUNST1(1:12)
 9210          FORMAT(' ',A12)
            ELSE IF (LOCB(1) .EQ. 3) THEN
               WRITE(IOUNIT,9220) RUNST1(1:13)
 9220          FORMAT(' ',A13)
            ELSE IF (LOCB(1) .EQ. 4) THEN
               WRITE(IOUNIT,9230) RUNST1(1:14)
 9230          FORMAT(' ',A14)
            END IF
         ELSE IF (ECHO) THEN
C           Echo Full Input Card to Output File
            WRITE(IOUNIT,ECFRM) RUNST1
         END IF

C        If Blank Line, Then CYCLE to Next Card
         IF (BLINE) GO TO 11

C        Check for 'NO ECHO' In First Two Fields
         IF (FIELD(1) .EQ. 'NO' .AND. FIELD(2) .EQ. 'ECHO') THEN
            ECHO = .FALSE.
            GO TO 11
         END IF

C        Extract Pathway ID From Field 1                    ---   CALL EXPATH
         CALL EXPATH(FIELD(1),NOPATH)

C        For Invalid Pathway and Comment Lines Skip to Next Record
         IF (NOPATH .OR. PATH .EQ. '**') GO TO 11

C        Extract Keyword From Field 2                       ---   CALL EXKEY
         CALL EXKEY(FIELD(2),NOKEY)

C        When Keyword Is Wrong, Save Keyword and Skip To The Next Record
         IF (NOKEY) THEN
            PKEYWD = KEYWRD
            GO TO 11
         END IF

C        Check for Proper Order of Setup Cards              ---   CALL SETORD
         CALL SETORD

C        Process Input Card Based on Pathway
         IF (PATH .EQ. 'CO') THEN
C           Process COntrol Pathway Cards                   ---   CALL COCARD
            CALL COCARD
C           Echo Runstream Image to Temporary Event File (Except ELEVUNIT,
C                EVENTFIL, SAVEFILE, INITFILE & MULTYEAR)
            IF (KEYWRD.NE.'ELEVUNIT' .AND. KEYWRD.NE.'EVENTFIL' .AND.
     &          KEYWRD.NE.'SAVEFILE' .AND. KEYWRD.NE.'INITFILE' .AND.
     &          KEYWRD.NE.'MULTYEAR') THEN
               WRITE(ITEVUT,EVFRM) RUNST1
            END IF
            IF (KEYWRD .EQ. 'FINISHED') THEN
               WRITE(ITEVUT,*) '  '
            END IF
         ELSE IF (PATH .EQ. 'SO') THEN
C           Process SOurce Pathway Cards                    ---   CALL SOCARD
            CALL SOCARD
C           Echo Runstream Image to Temporary Event File
            WRITE(ITEVUT,EVFRM) RUNST1
            IF (KEYWRD .EQ. 'FINISHED') THEN
               WRITE(ITEVUT,*) '  '
            END IF
         ELSE IF (PATH .EQ. 'RE') THEN
C           Process REceptor Pathway Cards                  ---   CALL RECARD
            CALL RECARD
         ELSE IF (PATH .EQ. 'ME') THEN
C           Process MEteorology Pathway Cards               ---   CALL MECARD
            CALL MECARD
C           Echo Runstream Image to Temporary Event File (Except STARTEND
C           & DAYRANGE)
            IF (KEYWRD.NE.'STARTEND' .AND.
     &          KEYWRD.NE.'DAYRANGE') THEN
                WRITE(ITEVUT,EVFRM) RUNST1
            END IF
            IF (KEYWRD .EQ. 'FINISHED') THEN
               WRITE(ITEVUT,*) '  '
            END IF
         ELSE IF (PATH .EQ. 'TG') THEN
C           Process Terrain Grid Pathway Cards              ---   CALL TGCARD
            CALL TGCARD
C           Echo Runstream Image to Temporary Event File
            WRITE(ITEVUT,EVFRM) RUNST1
            IF (KEYWRD .EQ. 'FINISHED') THEN
               WRITE(ITEVUT,*) '  '
            END IF
         ELSE IF (PATH .EQ. 'OU') THEN
C           Process OUtput Pathway Cards                    ---   CALL OUCARD
            CALL OUCARD
         END IF

C        Store the Current Keyword as the Previous Keyword
         PKEYWD = KEYWRD

C        Check for 'OU FINISHED' Card.  Exit DO WHILE Loop By Branching
C        to Statement 999 in Order to Avoid Reading a ^Z "End of File"
C        Marker That May Be Present For Some Editors.
         IF (PATH .EQ. 'OU' .AND. KEYWRD .EQ. 'FINISHED') THEN
            GO TO 999
         END IF

         GO TO 11
 999     EOF = .TRUE.
 11      CONTINUE
      END DO

C     Reinitialize Line Number Counter to Count Meteorology Data
      ILINE = 0

C     Check That All Pathways Were Finished
      IF (ICSTAT(20).NE.1 .OR. ISSTAT(20).NE.1 .OR. IRSTAT(20).NE.1 .OR.
     &    IMSTAT(20).NE.1 .OR. IOSTAT(20).NE.1) THEN
C        Runstream File Incomplete, Save I?STAT to IFSTAT and Write Message
         IFSTAT = ICSTAT(20)*10000 + ISSTAT(20)*1000 + IRSTAT(20)*100 +
     &            IMSTAT(20)*10 + IOSTAT(20)
         WRITE(DUMMY,'(I5.5)') IFSTAT
         CALL ERRHDL(PATH,MODNAM,'E','125',DUMMY)
      END IF

      RETURN
      END

      SUBROUTINE LWRUPR
C***********************************************************************
C                 LWRUPR Module of ISC2 Model
C
C        PURPOSE: Transfer All Characters From Lower Case To
C                 Upper Case (Using INDEX Intrinsic Function)
C                 Note that the CHAR*80 RUNST1 Variable Includes
C                 the Original Case for Echoing and for Later Use
C                 To Retrieve Filenames.
C
C        PROGRAMMER: Roger Brode, Kevin Stroupe
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Card Image (80 Character Array)
C                 Number of Characters in String, PARAMETER ISTRG
C
C        OUTPUTS: Input Runstream Card Image (Array) in Uppercase
C
C        CALLED FROM:   SETUP
C***********************************************************************
C
C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      CHARACTER UPCASE*26
      CHARACTER LWCASE*26

C     Variable Initializations
      DATA UPCASE/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      DATA LWCASE/'abcdefghijklmnopqrstuvwxyz'/
      MODNAM = 'LWRUPR'

      DO 20 I = 1, ISTRG
         IF (RUNST(I) .NE. ' ') THEN
            INDCHK = INDEX(LWCASE,RUNST(I))
            IF (INDCHK .NE. 0) THEN
               RUNST(I) = UPCASE(INDCHK:INDCHK)
            END IF
         END IF
 20   CONTINUE

      RETURN
      END

      SUBROUTINE DEFINE
C***********************************************************************
C                 DEFINE Module of ISC2 Model
C
C        PURPOSE: Defines Location of Fields on Runstream Input Image
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Card Image
C
C        OUTPUTS: Number of Fields on Card, IFC
C                 Beginning and Ending Columns of Fields, LOCB and LOCE
C
C        CALLED FROM:   SETUP
C***********************************************************************
C

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'DEFINE'

C     Initialize the Blank Line and In-field Status Indicators
      BLINE = .TRUE.
      INFLD = .FALSE.

      IF (ILINE .EQ. 1) THEN
C        Define the Starting Column for the Input File In Case File Is Shifted.
C        Allow for Shift of Up to 3 Columns
         LOCB(1) = 0
         IF (RUNST(1) .NE. ' ') THEN
            LOCB(1) = 1
         ELSE IF (RUNST(2) .NE. ' ') THEN
            LOCB(1) = 2
         ELSE IF (RUNST(3) .NE. ' ') THEN
            LOCB(1) = 3
         ELSE IF (RUNST(4) .NE. ' ') THEN
            LOCB(1) = 4
         ELSE
            LOCB(1) = 1
         END IF
         LOCE(1) = LOCB(1) + 1
         LOCB(2) = LOCB(1) + 3
         LOCE(2) = LOCB(1) + 10
      END IF

      IFC = 2

C     Loop Through the Pathway and Keyword Fields To Check for Blank Line
      DO 15 I = LOCB(1), LOCE(2)+1
         IF (RUNST(I) .NE. ' ') BLINE = .FALSE.
 15   CONTINUE

C     Loop through the Data Fields
      DO 20 I = LOCB(1)+12, ISTRG

         IF (.NOT.INFLD .AND. RUNST(I).NE.' ') THEN
C           Location is the Beginning of a Field
C           Set Mark of not Blank Line
            BLINE = .FALSE.
C           Set Mark of in a Field
            INFLD = .TRUE.
C           Increment the Field Counter
            IFC = IFC + 1
C           Record the Location of Beginning of the Field
            LOCB(IFC) = I
         ELSE IF (INFLD .AND. RUNST(I).EQ.' ') THEN
C           Location is the End of a Field
C           Set Mark of Not In a field
            INFLD = .FALSE.
C           Record the Location of Ending of the Field
            LOCE(IFC) = I - 1
         END IF

C        Check for End of Input String
C        (Length of ISTRG is Set as a PARAMETER in MAIN1.INC)
         IF (INFLD .AND. I.EQ.ISTRG) THEN
            LOCE(IFC) = ISTRG
         END IF

 20   CONTINUE

      RETURN
      END

      SUBROUTINE GETFLD
C***********************************************************************
C                 GETFLD Module of ISC2 Model
C
C        PURPOSE: Gets Contents of Fields on Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Card Image
C
C        OUTPUTS: Contents of Fields on Card
C
C        CALLED FROM:   SETUP
C***********************************************************************
C
C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'GETFLD'

      DO 25 I = 1, IFC
         IF (LOCE(I)-LOCB(I) .LE. 39) THEN
C           Field Satisfies Limit of 40 Characters
            WRITE(FIELD(I),9004) (RUNST(J),J=LOCB(I),LOCE(I))
         ELSE
C           Field Exceeds 40 Character Limit (May Be Valid for Met Format)
C           Truncate Field at 40 Characters
            WRITE(FIELD(I),9004) (RUNST(J),J=LOCB(I),LOCB(I)+39)
         END IF
 25   CONTINUE

 9004 FORMAT(40(A1:))

      RETURN
      END

      SUBROUTINE EXPATH(INPFLD,NOPATH)
C***********************************************************************
C                 EXPATH Module of ISC2 Model
C
C        PURPOSE: Extracts and Verifies Pathway ID from
C                 Runstream Input Card Image
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Card Image
C
C        OUTPUTS: The Extracted Pathway ID
C
C        CALLED FROM:   SETUP
C***********************************************************************
C
C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      CHARACTER INPFLD*2, PATHWY(IPN)*2
      LOGICAL NOPATH

C     Variable Initializations
      DATA (PATHWY(I),I = 1, IPN) /'CO','SO','RE','ME','TG','OU','**'/
      NOPATH = .TRUE.
      MODNAM = 'EXPATH'

C     Begin The Processing
      IF (INPFLD .NE. '  ') THEN
C        Check the Read-in Pathway
         PATH = INPFLD
         DO 100 I = 1, IPN
C           In Case of Match Set NOPATH to FALSE and Set Path Number, IPNUM
            IF (INPFLD .EQ. PATHWY(I)) THEN
               NOPATH = .FALSE.
               IPNUM = I
C              Exit to END
               GO TO 999
            END IF
 100     CONTINUE
C        In Case Of Invalid Pathway ID, Write Out Error Meassage
         IF (NOPATH) THEN
C           WRITE Error Message    ! Invalid Pathway ID
            CALL ERRHDL(PPATH,MODNAM,'E','100',PATH)
C           Reset the Pathway to the Previous Valid Pathway
            PATH = PPATH
         END IF
      ELSE
C        In Case of Blank Field Set Pathway to Previous Pathway
         NOPATH = .FALSE.
         PATH  = PPATH
         IPNUM = IPPNUM
      END IF

 999  RETURN
      END

      SUBROUTINE EXKEY(INPFLD,NOKEY)
C***********************************************************************
C                 EXKEY Module of ISC2 Model
C
C        PURPOSE: Extracts and Verifies Keyword from
C                 Runstream Input Card Image
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Card Image
C
C        OUTPUTS: The Extracted Keyword
C
C        CALLED FROM:   SETUP
C***********************************************************************
C

C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      CHARACTER INPFLD*8
      LOGICAL NOKEY

C     Variable Initializations
      NOKEY  = .TRUE.
      MODNAM = 'EXKEY'

C     Begin The Processing
      IF (INPFLD .NE. '        ') THEN
C        Check the Read-in Keyword
         KEYWRD = INPFLD
         DO 100 I = 1, IKN
C           In Case of Match Set NOKEY to FALSE
            IF (INPFLD .EQ. KEYWD(I)) THEN
               NOKEY = .FALSE.
C              Exit to END
               GO TO 999
            END IF
 100     CONTINUE
C        When Illegal Keyword Output Error Message
         IF (NOKEY) THEN
C           WRITE Error Message    ! Invalid Keyword
            CALL ERRHDL(PATH,MODNAM,'E','105',KEYWRD)
         END IF
      ELSE
C        In Case of Blank Field, Keyword Is Set to Previous Keyword
         NOKEY  = .FALSE.
         KEYWRD = PKEYWD
      END IF

 999  RETURN
      END

      SUBROUTINE SETORD
C***********************************************************************
C                 SETORD Module of ISC2 Model
C
C        PURPOSE: To Check Run Stream Setup Images for Proper
C                 Order
C
C        MODIFIED:   To allow for skipping of TG pathway if no terrain
C                    grid is used.  Roger Brode, PES, Inc. - 11/7/94
C
C        INPUTS:  Input Runstream Card Image
C
C        OUTPUTS: Status Settings and Error Messages
C
C        CALLED FROM:   SETUP
C***********************************************************************
C
C     Variable Declarations
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'

C     Variable Initializations
      MODNAM = 'SETORD'

      IF (KEYWRD .EQ. 'STARTING') THEN
         IF (ISTART .OR. .NOT.IFINIS) THEN
C           WRITE Error Message: Starting Out of Order
            CALL ERRHDL(PPATH,MODNAM,'E','115',PATH)
         ELSE IF (IPNUM .NE. IPPNUM+1) THEN
            IF (PATH.EQ.'OU' .AND. PPATH.EQ.'ME') THEN
C              TG Pathway has been omitted - Assume no TG file and no error
               LTGRID = .FALSE.
            ELSE
C              WRITE Error Message: Pathway Out of Order
               CALL ERRHDL(PPATH,MODNAM,'E','120',PATH)
            END IF
         END IF
C        Set Starting Indicator
         ISTART = .TRUE.
C        Set Finished Indicator
         IFINIS = .FALSE.
      ELSE IF (KEYWRD .EQ. 'FINISHED') THEN
         IF (IFINIS .OR. .NOT.ISTART) THEN
C           WRITE Error Message: Finished Out of Order
            CALL ERRHDL(PPATH,MODNAM,'E','115',PATH)
         ELSE IF (ISTART .AND. PATH.NE.PPATH) THEN
C           WRITE Warning Message: Pathway Out of Order
            CALL ERRHDL(PPATH,MODNAM,'E','120',PATH)
         END IF
C        Reset Starting Indicator
         ISTART = .FALSE.
C        Set Finished Indicator
         IFINIS = .TRUE.
      ELSE IF (.NOT.ISTART .OR. IFINIS) THEN
C        WRITE Error Message: Starting or Finished Out of Order
         CALL ERRHDL(PPATH,MODNAM,'E','115',PATH)
      ELSE IF (ISTART .AND. PATH.NE.PPATH) THEN
C        WRITE Warning Message: Pathway Out of Order
         CALL ERRHDL(PPATH,MODNAM,'E','120',PATH)
      END IF

C     Save Current Path and Path Number as Previous Path and Number
      PPATH = PATH
      IPPNUM = IPNUM

      RETURN
      END

      SUBROUTINE STONUM(STRVAR,LENGTH,FNUM,IMUTI)
C***********************************************************************
C                 STONUM Module of ISC2 Model
C
C        PURPOSE: Gets Number From A String Variable
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input String Variable
C                 Length of Character String
C
C        OUTPUTS: Numbers
C
C        CALLED FROM: (This Is A Utility Program)
C***********************************************************************
C
C     Variable Declarations
      CHARACTER STRVAR*(*), CHK, MODNAM*6, NUMS*10
      REAL FNUM, CNUM
      LOGICAL MEND, IN, NMARK, PMARK, DMARK, MMARK, EMARK

C     Variable Initialization
      MODNAM = 'STONUM'
      NUMS = '0123456789'
      I = 1
      MEND = .FALSE.
      IN = .FALSE.
      NMARK = .FALSE.
      PMARK = .FALSE.
      DMARK = .FALSE.
      MMARK = .FALSE.
      EMARK = .FALSE.
      CNUM  = 0.0
      IMUTI = 1
      FDEC  = 1.

C     Beginning the Processing
      DO WHILE (.NOT.MEND .AND. I.LE.LENGTH)
         CHK = STRVAR(I:I)
         IF (CHK .NE. ' ') THEN
            IN = .TRUE.
            IF (CHK.GE.'0' .AND. CHK.LE.'9') THEN
C              CHK is a Number, Assign a Value
               IF (.NOT. DMARK) THEN
                  CNUM = CNUM*10.+FLOAT(INDEX(NUMS,CHK)-1)
               ELSE
                  FDEC = FDEC/10.
                  FDC1 = FDEC*FLOAT(INDEX(NUMS,CHK)-1)
                  CNUM = CNUM+FDC1
               END IF
            ELSE
C              Handle The E-Type Real Number
               IF (.NOT.EMARK .AND. CHK.EQ.'E') THEN
                  EMARK = .TRUE.
                  IF (.NOT.NMARK) THEN
                     HEAD = CNUM
                  ELSE
                     HEAD = -CNUM
                  END IF
                  DMARK = .FALSE.
                  NMARK = .FALSE.
                  CNUM = 0.0
               ELSE IF (.NOT.PMARK .AND. CHK.EQ.'+') THEN
C                 Set Positive Indicator
                  PMARK = .TRUE.
               ELSE IF (.NOT.NMARK .AND. CHK.EQ.'-') THEN
C                 Set Negative Indicator
                  NMARK = .TRUE.
               ELSE IF (.NOT.DMARK .AND. CHK.EQ.'.') THEN
C                 Set Decimal Indicator
                  DMARK = .TRUE.
               ELSE IF (.NOT.MMARK .AND. CHK.EQ.'*' .AND.
     &                  .NOT.NMARK) THEN
C                 Set Repeat Number
                  MMARK = .TRUE.
                  IMUTI = INT(CNUM)
                  CNUM = 0.0
               ELSE
C                 Error Occurs, Set Switch and Exit Out Of The Subroutine
                  GO TO 9999
               END IF
            END IF
         ELSE IF (IN .AND. CHK.EQ.' ') THEN
            MEND = .TRUE.
         END IF
         I = I + 1
      END DO

      FNUM = CNUM

C     In Case Of Negative Field, Value Set to Negative
      IF (NMARK) THEN
         FNUM = -FNUM
      END IF

C     In Case of E-Format, Check for Exponents Out of Range
      IF (EMARK .AND. ABS(FNUM) .LE. 30.) THEN
         FNUM = HEAD*10**(FNUM)
      ELSE IF (EMARK .AND. ABS(FNUM) .GT. 30.) THEN
         IF (FNUM .LT. 0.0) THEN
            FNUM = 0.0
         ELSE IF (FNUM .GT. 0.0) THEN
            FNUM = HEAD * 10**30.
         END IF
         GO TO 9999
      END IF

      GO TO 1000

C     Set Error Switch for Illegal Numerical Field (WRITE Message and Handle
C     Error in Calling Routine)
 9999 IMUTI = -1

 1000 RETURN
      END

      SUBROUTINE SINDEX(ARRIN,IDIM,ELEM,INDEXS,FIND)
C***********************************************************************
C                 SINDEX Module of ISC2 Model
C
C        PURPOSE: Search The Index of An Input Array Element
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Character Element
C
C        OUTPUTS: Index Of This Element in An Array
C
C        CALLED FROM:  (This Is An Utility Programm)
C***********************************************************************
C
C     Variable Declarations
      CHARACTER*8 ARRIN(IDIM), ELEM
      CHARACTER MODNAM*6
      LOGICAL FIND

C     Variable Initializations
      MODNAM = 'SINDEX'
      FIND = .FALSE.
      I = 1
      INDEXS = 0

      DO WHILE (.NOT.FIND .AND. I.LE.IDIM)
         IF (ELEM .EQ. ARRIN(I)) THEN
            FIND = .TRUE.
            INDEXS = I
         END IF
         I = I + 1
      END DO

      RETURN
      END

      SUBROUTINE FSPLIT(PATHIN,KEYIN,INPFLD,LENGTH,DELIM,LFLAG,
     &                  BEGFLD,ENDFLD)
C***********************************************************************
C                 FSPLIT Module of ISC2 Model
C
C        PURPOSE: SPLIT A FIELD, BASED ON AN INPUT DELIMITER
C                 CHARACTER.  SETS A LOGICAL FLAG AND RETURNS
C                 BEGINNING AND ENDING PARTS OF FIELD.
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Pathway for Calling Routine
C                 Keyword for Calling Routine
C                 Input Field Variable
C                 Length of Input Character Field
C                 Delimiter Character
C
C        OUTPUTS: Logical Flag to Indicate Presence of Delimiter
C                 Beginning Part of Field (.LE. 8 Character)
C                 Ending Part of Field (.LE. 8 Character)
C
C        CALLED FROM: (This Is A Utility Program)
C***********************************************************************

C     Variable Declarations
      CHARACTER CHK, INPFLD*(*), DELIM, BEGFLD*8, ENDFLD*8, MODNAM*6,
     &          PATHIN*2, KEYIN*8
      LOGICAL LFLAG, MEND, IN

C     Variable Initialization
      MODNAM = 'FSPLIT'
      I = LENGTH
      IDELM = LENGTH
      BEGFLD = ' '
      ENDFLD = ' '
      MEND  = .FALSE.
      IN    = .FALSE.
      LFLAG = .FALSE.

C     Begin the Processing
      DO WHILE (.NOT.MEND .AND. I.GE.1)
         CHK = INPFLD(I:I)
         IF (CHK .NE. ' ') THEN
            IN = .TRUE.
C           Check for the Group Delimiter
            IF (.NOT.LFLAG .AND. CHK.EQ.DELIM) THEN
               LFLAG = .TRUE.
               IDELM = I
               ENDFLD = INPFLD(I+1:LENGTH)
               IF (I .EQ. 1) THEN
C                 Write Error Message for Invalid Range Parameter
                  CALL ERRHDL(PATHIN,MODNAM,'E','203',KEYIN)
                  GO TO 999
               END IF
            ELSE IF (LFLAG .AND. CHK.EQ.DELIM) THEN
C              WRITE Error Message  ! More Than One Delimiter in a Field
               CALL ERRHDL(PATHIN,MODNAM,'E','217',KEYIN)
            END IF
         ELSE IF (IN .AND. CHK.EQ.' ') THEN
            MEND = .TRUE.
            IF (LFLAG) THEN
               BEGFLD = INPFLD(1:IDELM-1)
            ELSE
               BEGFLD = INPFLD
            END IF
         END IF
         I = I - 1
      END DO

      IF (.NOT. MEND) THEN
         IF (LFLAG) THEN
            BEGFLD = INPFLD(1:IDELM-1)
         ELSE
            BEGFLD = INPFLD
         END IF
      END IF

C     In Case Of No Delimiter, Set ENDFLD = BEGFLD
      IF (.NOT. LFLAG) THEN
         ENDFLD = BEGFLD
      END IF

 999  RETURN
      END
