      SUBROUTINE LWRUPR (RUNST1)
C***********************************************************************
C*                LWRUPR Module of ISC2 Model
C*
C*       PURPOSE: Transfer All Characters From Lower Case To
C*                Upper Case (Using INDEX IntrinsiC*Function)
C*                Note that the CHAR*80 RUNST1 Variable Includes
C*                the Original Case for Echoing and for Later Use
C*                To Retrieve Filenames.
C*
C*       PROGRAMMER: Roger Brode, Kevin Stroupe
C*
C*       DATE:    March 2, 1992
C*
C*       MODIFIED:   Jayant Hardikar, PES, Inc.
C*                   February 13, 1995
C*                   Adapted for PCRAMMET - Made Generic
C*
C*       INPUTS:  Input Runstream Card Image (80 Character Array)
C*                Number of Characters in String, PARAMETER ISTRG
C*
C*       OUTPUTS: Input Runstream Card Image (Array) in Uppercase
C*
C*       CALLED FROM:   READIN
C***********************************************************************
C*
C*    Variable Declarations
      PARAMETER (ISTRG = 80)
      CHARACTER UPCASE*26
      CHARACTER LWCASE*26
      CHARACTER*1 RUNST(ISTRG)
      CHARACTER   RUNST1*80

C*    Variable Initializations
      DATA UPCASE/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      DATA LWCASE/'abcdefghijklmnopqrstuvwxyz'/

      DO 10 I = 1, ISTRG
         READ (RUNST1(I:I),'(A1)') RUNST(I)
10    CONTINUE

      DO 20 I = 1, ISTRG
         IF (RUNST(I) .NE. ' ') THEN
            INDCHK = INDEX(LWCASE,RUNST(I))
            IF (INDCHK .NE. 0) THEN
               RUNST(I) = UPCASE(INDCHK:INDCHK)
            END IF
         END IF
 20   CONTINUE

      DO 30 I = 1, ISTRG
         WRITE (RUNST1(I:I),'(A1)') RUNST(I)
30    CONTINUE


      RETURN
      END


      SUBROUTINE JULIAN(IYR,IMO,IDY,JDAY,ITYPE)
C***********************************************************************
C*    JULIAN Utility Module
C*
C*    PURPOSE:    Converts dates between Julian and mo/day format
C*
C*    INPUTS:     Year, month, day   OR  Julian day and year
C*                ITYPE: 0 - CONVERT IMO/IDY TO JDAY
C*                       1 - CONVERT JDAY TO IMO/IDY
C*
C*    OUTPUTS:    Julian day and year   OR  year, month, day
C*
C*    CALLED FROM: Various subprograms
C***********************************************************************

C*    Variable Declarations
      dimension iofset(13,2)

      data iofset/
     $ 0,31,59,90,120,151,181,212,243,273,304,334,365,
     $ 0,31,60,91,121,152,182,213,244,274,305,335,366/
C
C**** SEE IF WE ARE IN A LEAP YEAR
C
      if( mod(iyr,4).eq.0 ) then
         ileap = 2
      else
         ileap = 1
      endif
C
C**** SEE WHICH DIRECTION WE ARE GOING
C
      if(itype.eq.0) then
         jday = iofset(imo,ileap) + idy
      else
         do 10 j=2,13
            if(jday.le.iofset(j,ileap)) then
               imo = j-1
               idy = jday - iofset(imo,ileap)
               go to 999
            endif
   10    continue
      endif
  999 continue
      return
      end


      SUBROUTINE HR0024(IYR,JULDAY,IHR)
C***********************************************************************
C*    HR0024 Utility Module
C*
C*    PURPOSE:    To convert a date/time group from hour=0 to hour=24
C*                of the previous day
C*
C*    INPUTS:     Julian day and hour
C*
C*    OUTPUTS:    Julian day and hour
C*
C*    CALLED FROM: FIL144, READPP
C***********************************************************************

C*    Variable Declarations
C
      INTEGER IYR, JULDAY, IHR

C*    IYR    = YEAR
C*    JULDAY = JULIAN DAY
C*    IHR    = IHR

C*    Set hour 00 to hour 24
      IHR = 24

      IF(JULDAY .EQ. 1) THEN
C*       The following code will be processed only for January 1
         IYR = IYR - 1

C*       Account for Y2K
         IF( IYR .LT. 0 )THEN
            IYR = 99
         ENDIF

         IF( MOD(IYR,4).EQ.0 )THEN
            JULDAY = 366
         ELSE
            JULDAY = 365
         ENDIF
      ELSE
         JULDAY = JULDAY - 1
      ENDIF
C
      RETURN
      END


      SUBROUTINE SUN(JULDAY)
C***********************************************************************
C*    SUN Module of PCRAMMET Meteorological Pre-processor
C*
C*    PURPOSE: This routine calculates the sunrise and sunset times
C*             of the day from the date, latitude, longitude, and time
C*             zone
C*
C*    PROGRAMMER: Jayant Hardikar
C*                PES Inc.
C*                Adapted From MPRM
C*
C*    DATE:       February 23, 1995
C*
C*    INPUTS:     Latitude, longitude, Julian day
C*
C*    OUTPUTS:    Sunrise, sunset, solar elevation for each hour of day
C*
C*    CALLED FROM: MAIN program
C***********************************************************************
C*
C*    Variable Declarations
      INCLUDE 'PCRAM.INC'

C*    ZONE 05 = EASTERN
C*    ZONE 06 = CENTRAL
C*    ZONE 07 = MOUNTAIN
C*    ZONE 08 = PACIFIC
C*
      DUM    = XLON/15.0 - ITZONE
      TEMPZ  = 15.0*ITZONE - XLON
      SINLAT = SIN(XLAT/CONST )
      COSLAT = COS(XLAT/CONST )

C---- Determine the angular(radians) fraction of a year for this date;
C     the constant 0.0172028=360./(365.242*57.29578)
      DAYNO  = (JULDAY-1.0)*0.0172028
      TDAYNO = 2.0*DAYNO
      SIND   = SIN(DAYNO)
      COSD   = COS(DAYNO)
      SINTD  = SIN(TDAYNO)
      COSTD  = COS(TDAYNO)
C
C---- Account for ellipticity of earth's orbit.

      SIGMA = 279.9348 + (DAYNO*CONST) + 1.914827*SIND -
     &        0.079525*COSD + 0.019938*SINTD - 0.00162*COSTD

C---- Find the sine of the solar declination;
C     the constant 0.39785=sin(0.4091720193=23.44383/57.29578)

      DSIN = 0.39785*SIN(SIGMA/CONST)
      DCOS = SQRT(1.0-DSIN*DSIN)

C---- Determine time(hrs) of meridian passage

      AMM  = 12.0 + 0.12357*SIND - 0.004289*COSD + 0.153809*SINTD +
     &                                             0.060783*COSTD
      HCOS = (-SINLAT*DSIN)/(COSLAT*DCOS)

C---- Determine solar hour angle of sunrise-sunset.
      H2 = (ATAN2(SQRT(1.0-HCOS*HCOS),HCOS)/15.0)*CONST

C---- Time of sunrise(SUNRIS) and time of sunset(SUNSET) are expressed
C     in local standard time since the zone correction has already
C     been made; otherwise they would be in Greenwich mean time.
      SUNRIS = AMM - H2 + DUM
      SUNSET = AMM + H2 + DUM


C*    Calculate the solar elevation for all (daytime) hours
      DO 100 IHOUR = 1,24

         IF( IHOUR .LE. SUNRIS ) THEN
            SOLANG(IHOUR) = 0.0
         ELSEIF( IHOUR .GE. SUNSET )THEN
            SOLANG(IHOUR) = 0.0
         ELSE
C----       Determine the angle of elevation and solar hour angle(radians)
            HI    = (15.0*(IHOUR-AMM)+TEMPZ)/CONST
            ALFSN = SINLAT*DSIN + DCOS*COSLAT*COS(HI)

C----       Determine solar elevation angle (in degrees).
            ALF   = ATAN2(ALFSN,SQRT(1.0-ALFSN*ALFSN))*CONST
            SOLANG(IHOUR) = ALF
         ENDIF

  100 CONTINUE

      RETURN
      END


      SUBROUTINE PARSER (RUNSTG,FIELDS,IFCC)
C***********************************************************************
C*    PARSER Module of PCRAMMET Meteorological Pre-processor
C*
C*    PURPOSE: A generic routine for parsing a line of space-delimited
C*             text into individual character variables
C*
C*    PROGRAMMER: Jayant Hardikar
C*
C*    DATE:    February 9, 1995
C*
C*    INPUTS:  Input character string (space delimited)
C*             Number of Characters in String
C*
C*    OUTPUTS: Array of parsed character Variables
C*
C*    CALLED FROM: SAMSON
C***********************************************************************
C
C     Variable Declarations
      PARAMETER (IFMAX = 27)
      PARAMETER (ISTRG = 256)

      CHARACTER*256  RUNSTG
      CHARACTER        FIELDS(IFMAX)* 40

      CHARACTER RUNST(ISTRG)*1
      CHARACTER FIELD(IFMAX)*40

      LOGICAL INFLD

      COMMON /FIELDS/ LOCB(IFMAX), LOCE(IFMAX), IFC
      COMMON /FLDCHR/ FIELD
      COMMON /LOGIN1/ INFLD
      COMMON /RUNSTR/ RUNST


      DO 20 I = 1,ISTRG
         READ (RUNSTG(I:I),'(A1)') RUNST(I)
20    CONTINUE


C*    Convert Lower Case to Upper Case Letters        ---   CALL LWRUPR
      CALL LWRUPR (RUNSTG)

C     Define Fields on Card                           ---   CALL DEFINE
      CALL DEFINE

C     Get the Contents of the Fields as Characters    ---   CALL GETFLD
      CALL GETFLD


      DO 10 I = 1,IFMAX
         FIELDS (I) = FIELD(I)
10    CONTINUE

      IFCC = IFC


      RETURN
      END


      SUBROUTINE DEFINE
C***********************************************************************
C     DEFINE Module of PCRAMMET Meteorological Pre-processor
C
C     PURPOSE: Defines Location of Fields on Runstream Input Image
C
C     PROGRAMMER: Jeff Wang, Roger Brode
C
C     DATE:       March 2, 1992
C
C     MODIFIED:   Jayant Hardikar - 2/9/95 : Made it generic
C
C     INPUTS:     One line of variables separated by spaces
C
C     OUTPUTS:    Number of Fields on Card, IFC
C                 Beginning and Ending Columns of Fields, LOCB and LOCE
C
C     CALLED FROM: PARSER
C***********************************************************************
C
C     Variable Declarations
      PARAMETER (IFMAX = 27)
      PARAMETER (ISTRG = 256)
      CHARACTER*1 RUNST
      CHARACTER*40 FIELD
      LOGICAL INFLD
      COMMON /FIELDS/ LOCB(IFMAX), LOCE(IFMAX), IFC
      COMMON /FLDCHR/ FIELD(IFMAX)
      COMMON /LOGIN1/ INFLD
      COMMON /RUNSTR/ RUNST(ISTRG)

C     Initialize the Blank Line and In-field Status Indicators
      INFLD = .FALSE.

C     Define the Starting Column for the Input Line In Case File Is Shifted.
C     Allow for Shift of Up to 3 Columns

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

      IFC = 0

C     Loop through the Data Fields
      DO 20 I = LOCB(1), ISTRG

         IF (.NOT.INFLD .AND. RUNST(I).NE.' ') THEN
C           Location is the Beginning of a Field
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
         IF (INFLD .AND. I.EQ.ISTRG) THEN
            LOCE(IFC) = ISTRG
         END IF

 20   CONTINUE

      RETURN
      END

      SUBROUTINE GETFLD
C***********************************************************************
C     GETFLD Module of PCRAMMET Meteorological Pre-processor
C
C     PURPOSE: Gets Contents of Fields on Runstream Input Image
C
C     PROGRAMMER: Roger Brode, Jeff Wang
C
C     DATE:    March 2, 1992
C
C     MODIFIED:   Jayant Hardikar - 2/9/95 : Made it generic
C
C     INPUTS:     Fields on a line
C
C     OUTPUTS: Contents of Fields on Card
C
C     CALLED FROM:   PARSER
C***********************************************************************
C
C     Variable Declarations
      PARAMETER (IFMAX = 27)
      PARAMETER (ISTRG = 256)
      CHARACTER*1 RUNST
      CHARACTER*40 FIELD
      LOGICAL INFLD

      COMMON /FIELDS/ LOCB(IFMAX), LOCE(IFMAX), IFC
      COMMON /FLDCHR/ FIELD(IFMAX)
      COMMON /LOGIN1/ INFLD
      COMMON /RUNSTR/ RUNST(ISTRG)


      DO 25 I = 1, IFC
         IF (LOCE(I)-LOCB(I) .LE. 39) THEN
C           Field Satisfies Limit of 40 Characters
            WRITE(FIELD(I),9004) (RUNST(J),J=LOCB(I),LOCE(I))
         ELSE
            WRITE(FIELD(I),9004) (RUNST(J),J=LOCB(I),LOCB(I)+39)
         END IF
 25   CONTINUE

 9004 FORMAT(40(A1:))

      RETURN
      END

      SUBROUTINE STONUM(STRVAR,LENGTH,FNUM,IMUTI)
C***********************************************************************
C     STONUM Module of PCRAMMET Meteorological Pre-processor
C
C     PURPOSE: Gets Number From A String Variable
C
C     PROGRAMMER: Jeff Wang, Roger Brode
C
C     DATE:    March 2, 1992
C
C     INPUTS:  Input String Variable Length of Character String
C
C     OUTPUTS: Numbers
C
C     CALLED FROM: (This Is A Utility Program)
C***********************************************************************
C
C     Variable Declarations
      CHARACTER STRVAR*(*), CHK, NUMS*10
      REAL FNUM, CNUM
      LOGICAL MEND, IN, NMARK, PMARK, DMARK, MMARK, EMARK

C     Variable Initialization
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


      SUBROUTINE ERRHDL(INERTP,INERCD,IWRITE)
C***********************************************************************
C     ERRHDL   Module of PCRAMMET Meteorological Pre-processor
C
C     PURPOSE: A General Error Handling Procedure
C
C     PROGRAMMED BY: PES, Inc.
C
C     DATE:
C
C     INPUTS:  Error Type (INERTP), Error Code (INERCD)
C              Whether to print the month & day (IWRITE=1)
C              or not (IWRITE=0)
C
C     OUTPUTS: Error Message
C
C     CALLED FROM: Various subprograms
C***********************************************************************
C
C     Variable Declarations
      INCLUDE 'PCRAM.INC'
      CHARACTER ERRMG1*72, INERTP*1, INERCD*4, SAVCD*4
      LOGICAL FIND

C     Variable Initializations
      FIND = .FALSE.
      I = 1

C     Match The Error Message
      DO WHILE (.NOT.FIND .AND. I.LE.MAXERR)
         IF (INERCD .EQ. ERRCOD(I)) THEN
            IERRDX = I
            ERRMG1 = ERRMSG(I)
            FIND = .TRUE.
         END IF
         I = I + 1
      END DO

      IF (.NOT. FIND) THEN
         INERTP = 'E'
         SAVCD  = INERCD
         INERCD = '----'
         WRITE(ERRMG1,1001) SAVCD
 1001    FORMAT('SYSTEM ERROR: MESSAGE NOT FOUND FOR NUMBER ',A4)
      END IF

C     Write Out The Error Message
      IF( IWRITE .EQ. 1 )THEN
         WRITE(IDIAG,1111) INERTP,INERCD,ERRMG1,ISMO(1),ISDY(1)
      ELSEIF( IWRITE .EQ. 0 )THEN
         WRITE(IDIAG,1111) INERTP,INERCD,ERRMG1
      ENDIF
 1111 FORMAT(A1,A4,1X,': ',A72: ,/,7X,' on (month,day) ', i3, ',',i3)

      IF (INERTP .EQ. 'E') THEN
         WRITE (*,*) '            ****** FATAL ERROR ******        '
         WRITE (*,*) '               PROGRAM TERMINATED            '
         WRITE (*,*) '    See the Output File PCRAM.LOG for Details'
         STOP
      ENDIF


 999  RETURN
      END
