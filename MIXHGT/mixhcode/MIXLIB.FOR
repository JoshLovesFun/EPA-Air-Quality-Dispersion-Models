      SUBROUTINE INITUA( MAXLEV, IUAPR, IUAHT, IUATT, IUATD, IUAWD,
     &                   IUAWS, UAPR, UAHT, UATT, UATD, UAWD, UAWS )
C=======================================================================
C     Purpose:  Initialize the upper air arrays
C
C-----------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER  MAXLEV, ILEV
      INTEGER  IUAPR(MAXLEV), IUAHT(MAXLEV), IUATT(MAXLEV),
     &         IUATD(MAXLEV), IUAWD(MAXLEV), IUAWS(MAXLEV)
      REAL      UAPR(MAXLEV),  UAHT(MAXLEV),  UATT(MAXLEV),
     &          UATD(MAXLEV),  UAWD(MAXLEV),  UAWS(MAXLEV)


      DO 1 ILEV = 1, MAXLEV

         IUAPR(ILEV) = -9999
         IUAHT(ILEV) = -9999
         IUATT(ILEV) = -9999
         IUATD(ILEV) = -9999
         IUAWD(ILEV) = -9999
         IUAWS(ILEV) = -9999

         UAPR(ILEV)  = -9999.0
         UAHT(ILEV)  = -9999.0
         UATT(ILEV)  = -9999.0
         UATD(ILEV)  = -9999.0
         UAWD(ILEV)  = -9999.0
         UAWS(ILEV)  = -9999.0

    1 CONTINUE

      RETURN
      END


      SUBROUTINE INITSF( SFP, SFT )
C=======================================================================
C     Purpose:  Initialize the surface data
C               ISFP = station pressure
C               ISFT = station temperature
C
C-----------------------------------------------------------------------
      IMPLICIT NONE

      REAL   SFP,SFT

      SFP = -9999.
      SFT = -9999.

      RETURN
      END


      SUBROUTINE CHROND( IYR,IJD,IYDY )
C=======================================================================
C     Purpose:  Converts dates between 1900 and 1999 to a common
C               sequential day.  The zero point is January 1, 1900,
C               i.e., 1 is January 1,1900.
C
C     Argument list:
C          Input:   IYR  - year (2 digits, e.g., 94 = 1994)
C                   IJD  - Julian day (3 digits)
C          Output:  IYDY - date converted to sequential day since 1/1/1900
C
C
C     Called by:     UAEXT,GETSDG,GETMIX,SFEXT,GETSFC,MERGE
C
C     Calls to:      ERROR
C
C     Programmed by: Pacific Environmental Services, Inc. (PES)
C                    Research Triangle Park, NC
C
C-----------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER NDAY,IYDY,IYR,IJD,ICNT,ILEAP

C-----------------------------------------------------------------------
C *** Check for invalid data

      IF( IYR.LT.0 .OR. IYR.GE.100 )THEN
         WRITE(  6, 3500 ) IYR, IJD
         WRITE( 35, 3500 ) IYR, IJD
 3500    FORMAT( ' Invalid year passed to subr.CHROND;',
     &           ' (year, julian day): ',i8,i8)
         IYDY = -9999
         RETURN
      END IF

C *** Test for leap year and determine the number of days in the year
      ILEAP = MOD(IYR,4)
      IF( ILEAP .EQ. 0  .OR.  IYR .EQ. 0) THEN
         NDAY = 366
      ELSE
         NDAY = 365
      END IF

C *** Check for a valid julian day

      IF( IJD .LT. 1  .OR. IJD .GT. NDAY )THEN
         WRITE(  6, 3510 ) IYR, IJD
         WRITE( 35, 3500 ) IYR, IJD
 3510    FORMAT( ' Invalid Julian day passed to subr.CHROND:',
     &           ' (year, julian day): ',i8,i8)
         IYDY = -9999
         RETURN
      END IF

C *** Compute the chronological day

      ICNT = (IYR+3)/4
      IYDY = 366*ICNT + 365*(IYR-ICNT) + IJD


      RETURN
      END


      SUBROUTINE ICHRND( IYDY,IYR,IJD )
C=====================================================================**
C  Purpose:  Converts a common sequential day to a date between 1900 and
C            1999.  The zero point (day 1) is the first day of 1900.
C
C  Argument list:
C          Input:   IYDY - common sequential day
C          Output:  IYR  - year (2 digits)
C                   IJD  - julian day (3 digits)
C
C  CALLED BY:     MERGE
C
C  CALLS TO:      ERROR
C
C-----------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER IYDY,IYR,IJD
      INTEGER IYDYO, IYDYN, NDAY

C      IYDYO   = Old sequential day
C      IYDYN   = New sequential day
C-----------------------------------------------------------------------
C *** Check for invalid data

      IF ((IYDY.LT.1) .OR. (IYDY.GT.36525)) THEN
c        WRITE( 35, 3500 ) iydy
         PRINT *, ' Invalid chronological day year passed to',
     &            ' subr.ICHROND: ',iydy
         IYR = -99
         IJD = -99
         RETURN
      ENDIF

C *** Convert common sequential day to 2-digit year and julian day

      IYDYO = IYDY
      IYR = 0
    1 IF(MOD(IYR,4) .EQ. 0) THEN
         NDAY = 366
      ELSE
         NDAY = 365
      ENDIF

      IYDYN = IYDYO - NDAY

C *** When the chronological day becomes negative, the year and julian
C      day have been determined

      IF(IYDYN .GT. 0) THEN
         IYR = IYR + 1
         IYDYO = IYDYN
         GO TO 1
      ELSE IF(IYDYN .LE. 0) THEN
         IJD = IYDYO
      ENDIF

C *** Check for valid year and julian day

      IF(IYR .GT. 99) THEN
         GO TO 999
      ELSE
         IF(MOD(IYR,4) .EQ. 0) THEN
            NDAY = 366
         ELSE
            NDAY = 365
         ENDIF

         IF((IJD .LT. 0) .OR. (IJD.GT.NDAY)) THEN
            GO TO 999
         ENDIF
      ENDIF

      RETURN

  999 CONTINUE
      PRINT *, ' Invalid year result from subr.ICHROND: '
      PRINT *, ' year = ', iyr, ' ,  julian day = ',ijd
      IYR = -99
      IJD = -99
      RETURN

      END


      SUBROUTINE GREG( YEAR,JDAY,MNTH,MDAY )
C=====================================================================**
C     Purpose:  To compute the Gregorian month and day given the year
C               and Julian day
C
C     Called by:  Several routines that needs to convert Julian to
C                 Gregorian day
C
C     Calls to:      -NONE-
C
C-----------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER YEAR,JDAY,MNTH,MDAY,L,J

C      YEAR = CALENDAR YEAR
C      JDAY = DAY OF YEAR -- 1, 365, OR 366
C      MNTH = MONTH OF YEAR -- 1, 12
C      MDAY = DAY OF MONTH -- 1, 31

C-----------------------------------------------------------------------
      L=365

      IF(MOD(YEAR,4)   .EQ. 0) L=366
      IF(MOD(YEAR,100) .EQ. 0) L=365
      IF(MOD(YEAR,400) .EQ. 0) L=366

      J    = MOD( JDAY+305,L )
      J    = MOD( J,153 ) / 61 + (J/153)*2 + J
      MNTH = MOD( J/31+2,12 ) + 1
      MDAY = MOD( J,31 ) + 1

      RETURN
      END


      FUNCTION JULIAN( YR1,MO1,DY1 )
C=====================================================================**
C     Purpose:  To convert Gregorian month and day to Julian day for the
C               given year.
C
C     Limitations:  Works only for years 1901 - 1999; inclusion of a test
C                   for 100 and 400 year corrections could extend this
C                   routine to those countries using our current calendar.
C                   This would require the year to be expressed fully
C                   (e.g. 1936 not 36).  The current calendar came into
C                   official use in the world in 1582, in Great Britain
C                   (and associated colonies in 1752).
C
C     Called by:  Several routines that needs to convert Gregorian to
C                 Julian day
C
C     Calls to:      -NONE-
C
C=======================================================================
      IMPLICIT NONE

      INTEGER YR1,MO1,DY1,JULIAN, IWORK1, IWORK2, IWORK3

C        YR1        LAST 2-DIGITS CALENDAR YEAR
C        MO1        MONTH OF YEAR (1-12)
C        DY1        DAY OF MONTH (1-31)
C-----------------------------------------------------------------------

      IF( YR1 .LE. 0  .OR.  MO1 .LE. 0  .OR.  DY1 .LE. 0 ) THEN
         JULIAN = 0
      ELSE

         IWORK1 = MOD( (MO1+9),12 )
         IWORK2 = (IWORK1*153+2)/5 + DY1 + 58

         IF( MOD(YR1,4).EQ.0 ) THEN
            IWORK3 = 366
            IWORK2 = IWORK2 + 1
         ELSE
            IWORK3 = 365
         END IF

         JULIAN = 1 + MOD( IWORK2,IWORK3 )

      END IF

      RETURN
      END


      SUBROUTINE READUA( IWBAN, YY, MONTH, DD, HH, NLEV, UAPR,
     &           UAHT, UATT, UATD, UAWD, UAWS, IOST13, ILEVEL, IFLAG )
C=======================================================================
C     Purpose:  To read one upper air sounding and converting the
C               integer values to real values
C
C     Modified:
C        10/98  Pacific Environmental Services
C               Added 'IF' to skip the sort routine if there are no data
C               levels in the sounding
C
C-----------------------------------------------------------------------
C       IMPLICIT NONE

       INCLUDE   'mixhts.inc'

       INTEGER   IOST13, NLEV, IFLAG
       INTEGER   IUAPR(MAXLEV), IUAHT(MAXLEV), IUATT(MAXLEV),
     &           IUATD(MAXLEV), IUAWD(MAXLEV), IUAWS(MAXLEV)

       INTEGER   HH, DD, YY, IND, IPRESS, IHGT, ITEMP, IDEWPT, IWD, IWS,
     &           IWBAN, NLVLS, I, IERR, NEWI

       REAL      P(MAXLEV), Z(MAXLEV) ,TT(MAXLEV), DIR(MAXLEV),
     &           WSP(MAXLEV), DEW(MAXLEV), XMISS
       REAL      UAPR(MAXLEV), UAHT(MAXLEV), UATT(MAXLEV),
     &           UATD(MAXLEV), UAWD(MAXLEV), UAWS(MAXLEV)


       CHARACTER*3 MM, MON(12)

       CHARACTER*80 Card

       DATA MON / 'JAN','FEB','MAR','APR','MAY','JUN','JUL',
     &            'AUG','SEP','OCT','NOV','DEC' /

C      MISS is the missing data indicator used by the data retrieval
C      program
       DATA MISS/32767/
       DATA XMISS/-999./


C-----------------------------------------------------------------------

C---- Initialize scalars and arrays
      IOST13  = 0
      IFLAG   = 0
      CALL INITUA( MAXLEV, IUAPR, IUAHT, IUATT, IUATD, IUAWD, IUAWS,
     &                      UAPR,  UAHT,  UATT,  UATD,  UAWD,  UAWS )


      icount = 0
   5  CONTINUE
      READ (13,105,IOSTAT=IOST13,END=900,ERR=900) HH, DD, MM, YY
 105  FORMAT (12X,I2,I7,6X,A3,4X,I4)
      icount = icount + 1

      MONTH = -99

      DO 10 I=1,12
      IF (MM .EQ. MON(I)) THEN
         MONTH = I
      ENDIF
  10  CONTINUE

C---- Convert year to a 2-digit year
      IF( YY .LT. 2000 )THEN
         YY = YY - 1900
      ELSE
         YY = YY - 2000
      ENDIF


C---- Read line 2.  This record contains the WBAN station ID & latitude
C     and longitude
      READ (13,106, IOSTAT=IOST13,END=900,ERR=900) IWBAN
  106 FORMAT( 9X, I5 )


C---- Read the number of levels in this particular sounding from line 3
      READ (13,107, IOSTAT=IOST13, END=900, ERR=900) NLVLS
      NLVLS = NLVLS - 4
C      write(*,*) 'Nlvls: ',Nlvls
  107 FORMAT (32X,I3)


C---- Read line 4 to get the wind speed units

      READ (13,108,IOSTAT=IOST13,END=900, ERR=900) CARD
  108 FORMAT (A80)


C---- Read data parameters and sort needed variables into arrays; the
C     input is read as integer and is stored in the real-valued arrays

      zlow = 999999
      DO 30 ILEVEL=1,NLVLS
         READ (13, 1000, IOSTAT=IOST13, END=920, ERR=920) IND, IPRESS,
     &         IHGT, ITEMP, IDEWPT, IWD, IWS
 1000    FORMAT (6X,I1,3X,I4,2X,I5,2X,I5,2X,I5,2X,I5,2X,I5)
         P(ilevel)   = IPRESS
         Z(ilevel)   = IHGT
         TT(ilevel)  = ITEMP
         DEW(ilevel) = IDEWPT
         DIR(ilevel) = IWD
         WSP(ilevel) = IWS

         IF (IWD .NE. MISS  .AND.  ITEMP .NE. MISS) THEN
            IF (IHGT .LT. ZLOW) THEN
               ZLOW = IHGT
            ENDIF
         ENDIF
 30   CONTINUE


C---- Sort initial array from high pressure to low pressure

      IF( NLVLS .GT. 0 )THEN
C        There is at least one level in the current sounding - sort data;
C        as the code stands on 10/9/98, the remainder of this subroutine
C        and the program executes without problem if there are no data
         CALL SORT6 (P,Z,TT,DEW,DIR,WSP,NLVLS,IERR)

         IF (IERR .LT. 0) THEN
            WRITE(  35, 3510 ) YY,MONTH,DD,HH
            WRITE(   6, 3510 ) YY,MONTH,DD,HH
 3500       FORMAT( ' Error in sort routine on',
     &               I2.2,2('-',I2.2), ', hour ',i2.2)
            IOST13 = -99
            RETURN
         ENDIF
      ENDIF


C---- Loop through sounding levels and store results into new arrays
C     Both pressure, height, and wind direction are correct orders of
C     magnitude; temperature, dew point, and wind speed have been
C     multiplied by 10

      DO 40 I=1,NLVLS

         P(I) = NINT( P(I) )

         Z(I) = NINT( Z(I)-Zlow )

         TT(I)  = NINT( TT(I) )
         IF( ABS(TT(I) - MISS) .GT. 0.01 ) THEN
            TT(I) = TT(I)/10.0
         ELSE
            TT(I) = XMISS
         END IF

         DIR(I)    = NINT( DIR(I) )
         IF( ABS(DIR(I) - MISS) .LT. 0.01 ) THEN
            DIR(I) = XMISS
         END IF

         DEW(I) = NINT( DEW(I) )
         IF ( ABS(DEW(I) - MISS) .GT. 0.01 ) THEN
            DEW(I) = DEW(I)/10.0
         ELSE
            DEW(I) = XMISS
         END IF

         WSP(I)    = NINT( WSP(I) )
         IF ( ABS(WSP(I) - MISS) .GT. 0.01 ) THEN
            IF (CARD(48:49) .EQ. 'kt') THEN
C              Conversion of knots to meters/second (0.5148)
               WSP(I) = (WSP(I)/10.0)*0.5148
            ELSEIF(CARD(48:49) .EQ. 'ms') THEN
C              Must be meters/second
               WSP(I) = WSP(I)/10.0
            ELSE
               WRITE( 35, 3510 ) YY,MONTH,DD,HH
               WRITE(  6, 3510 ) YY,MONTH,DD,HH
 3510          FORMAT( ' Unable to determine units for wind speed on',
     &               I2.2,2('-',I2.2), ', hour ',i2.2)
            END IF
         ELSE
            WSP(I)    = XMISS
         ENDIF

  40  CONTINUE

      HCOUNT = 0


C---- Loop through heights to determine number of negative values,
C     then reassign loop interval for this new number of levels
C     and store data in new arrays

      DO 35 I=1,NLVLS
         IF (Z(I).LT.0) THEN
            HCOUNT = HCOUNT + 1
         ELSE
            NEWI = I - HCOUNT

            UAPR(NEWI) = P(I)
            UAHT(NEWI) = Z(I)
            UATT(NEWI) = TT(I)
            UATD(NEWI) = DEW(I)
            UAWD(NEWI) = DIR(I)
            UAWS(NEWI) = WSP(I)

         ENDIF
  35  CONTINUE


C---- Calculate the number of levels (per sounding) at or
C     above ground

      NLEV = NLVLS - HCOUNT


      RETURN

C     EOF or error reading headers
900   IFLAG = 1
      RETURN

C     EOF or error reading levels
920   IFLAG = 2
      RETURN

      END


      SUBROUTINE SORT6(Y,U,V,W,X,Z,N,IERR)
C=====================================================================**
C     Purpose:  To sort the upper air data by pressure
C
C     Called by:  READUA
C
C     Calls to:   -NONE-
C
C=======================================================================

      PARAMETER (NMAX=150)

C     Shell sort of n values in y-array from largest to smallest.

      INTEGER I,J,J1,GAP,NMG,IERR
      REAL TY,TU,TV,TW,TX,TZ
      REAL Y(NMAX),U(NMAX),V(NMAX),W(NMAX),X(NMAX),Z(NMAX)

      IF(N.GT.NMAX) THEN
      IERR = -4
      GO TO 999
      END IF

      IF(N.GE.1) GO TO 10
      IERR = -5
      GO TO 999

C     One-element arrays are sorted by definition.
   10 IF(N.EQ.1) GO TO 999

      GAP = N
   20 GAP = GAP/2
      NMG = N - GAP
      DO 40 J1 = 1,NMG
      I = J1 + GAP

C     LOOP J = J1,1,-GAP

      J = J1
   30 IF(Y(J).GE.Y(I)) GO TO 40

C     Swap out-of-order pairs
C       Set IERR to +2 to indicate reordering was needed to sort array

      IERR = +2

      TY = Y(I)
      TU = U(I)
      TV = V(I)
      TW = W(I)
      TX = X(I)
      TZ = Z(I)

      Y(I) = Y(J)
      U(I) = U(J)
      V(I) = V(J)
      W(I) = W(J)
      X(I) = X(J)
      Z(I) = Z(J)

      Y(J) = TY
      U(J) = TU
      V(J) = TV
      W(J) = TW
      X(J) = TX
      Z(J) = TZ

C     Keep old pointer for next time through

      I = J
      J = J - GAP
      IF(J.GE.1) GO TO 30
   40 CONTINUE
      IF(GAP.GT.1) GO TO 20
  999 RETURN
      END


      SUBROUTINE READSF(SFCTYP, YY, MM, DD, HR, TT, PRESR, IFLAG )
C=====================================================================**
C     Purpose:  To read one hour of surface observations
C
C     Called by:  Main program
C
C     Calls to:   -NONE-
C
C=======================================================================
      INTEGER YY, MM, DD, HR, IDSTN, IPRESR, IWDR
      REAL    TT, SPD, PRESR, WDR
      CHARACTER*1 ASOSFLG

      CHARACTER*6 SFCTYP

      DATA XMISS2/999.9/
      DATA XMISS3/-9999.0/
      DATA XMISS4/9999./

      DATA  MISS2/9999/

C     NMISS denotes missing values for temperature and pressure in
C           output file for sure.  It has been used as well for
C           wind direction and wind speeds.

      IFLAG = 0

      IF( SFCTYP .EQ. 'HUSWO') THEN
         READ (22,100,END=900,ERR=910) IDSTN, ASOSFLG, YY, MM, DD, HR,
     &                                   TT, IPRESR, IWDR, SPD
 100     FORMAT (I5,A1,I4,3I2, T33,F5.0, T50,I4, T56,I3, T60,F4.1)


C------- Dry bulb temperature: degrees Fahrenheit, convert to degrees Celsius
         IF ( TT .LT. XMISS2 ) THEN
            TT = (TT-32.0)/1.8
         ELSE
            TT = XMISS3
         ENDIF

C------- Station pressure: hundredths of an inch, convert to (approx.) millibars
         IF ( IPRESR .NE. MISS2 ) THEN
            PRESR = (IPRESR/100.0) * 33.8639
         ELSE
            PRESR = XMISS3
         ENDIF

C------- HUSWO utilizes a 4-digit year; convert to a 2-digit value
         IF( YY .LT. 2000 )THEN
            YY =  YY - 1900
         ELSEIF( YY .GE. 2000 )THEN
            YY = YY - 2000
         END IF

      ELSEIF( SFCTYP .EQ. 'SAMSON' )THEN
         READ (22,105,END=900,ERR=910) YY, MM, DD, HR, TT,
     &                                     IPRESR, IWDR, SPD
 105     FORMAT ( 4(1X,I2), T56,F5.0, T72,I4, T77,I3, T81,F5.0)

         IF ( TT .LT. XMISS4 ) THEN
         ELSE
            TT = XMISS3
         ENDIF

         IF ( IPRESR .LT. MISS2 ) THEN
            PRESR = FLOAT(IPRESR)
         ELSE
            PRESR = XMISS3
         ENDIF

      ENDIF

      RETURN

  900 IFLAG = -1
      RETURN

  910 IFLAG =  1
      RETURN
      END


      SUBROUTINE INTRP2(NTEMPS,TEMPS,Z,SFCT,VALUE,FLAG)
C=======================================================================
C     Purpose:  To linearly interpolates a parameter to
C       a specified level.  Missing data are allowed in the arrays.

C       To perform the interpolation for the holzworth estimate of the
C       mixing heights, the temps and z array functions are inter-
C       changed
C-----------------------------------------------------------------------

      INCLUDE 'mixhts.inc'
      REAL    TEMPS(MAXLEV),Z(MAXLEV),SFCT,VALUE
      INTEGER NTEMPS,FLAG,TOP,BOTTOM
      REAL    MISS,MSSCHK
      DATA    MISS/-9999./, MSSCHK/-9000.0/

C---  COMPARE THE SPECIFIED INTERPOLATION TEMPERATURE (SFCT) TO  THE
C      LOWEST HEIGHT IN THE TEMPERATURE ARRAY (TEMPS(1)).
C      IF SFCT < TEMPS(1), SET THE FLAG TO 1 AND RETURN,
C      OTHERWISE CONTINUE.

      IF (SFCT .LT. TEMPS(1)) THEN
        FLAG  = 1
        VALUE = MISS
        RETURN
      END IF

C---  LOCATE THE LEVEL OF NON-MISSING HEIGHT AND TEMPERATURE DATA ABOVE
C       THE INTERPOLATION HEIGHT, BEGINNING WITH THE BOTTOM LEVEL.

      DO 200 I=2,NTEMPS
        IF ((TEMPS(I).GE.SFCT) .AND. (TEMPS(I).GT.MSSCHK) .AND.
     &      (Z(I).GT.MSSCHK)) THEN

C---  ALL NECESSARY DATA PRESENT ABOVE THE INTERPOLATION LEVEL;
C      LOCATE THE LEVEL OF NON-MISSING HEIGHT AND TEMPERATURE DATA
C      BELOW THE INTERPOLATION HEIGHT.

            DO 210 II=I-1,1,-1
              IF ((TEMPS(II).GT.MSSCHK) .AND. (Z(II).GT.MSSCHK)) THEN

C---  ALL REQUIRED DATA ARE AVAILABLE - INTERPOLATE

                TOP=I
                BOTTOM=II
                TOTDIF = TEMPS(TOP) - TEMPS(BOTTOM)
                PRTDIF = SFCT - TEMPS(BOTTOM)
                FRAC   = PRTDIF/TOTDIF
                PRMDIF = Z(TOP) - Z(BOTTOM)
                VALUE  = Z(BOTTOM) + FRAC*PRMDIF
                FLAG   = 0
                RETURN
              ELSE

C--- THE LOWER ARRAY HEIGHT OR TEMPERATURE IS MISSING

                CONTINUE
              ENDIF

210         CONTINUE

C----- NO LEVEL OF NON-MISSING DATA WAS LOCATED BELOW THE INTERPOLATION
C      HEIGHT

            VALUE = MISS
            FLAG = 3
            RETURN
        ELSE

C--- THE ARRAY HEIGHT IS MISSING OR IS BELOW THE INTERPOLATION HEIGHT

          CONTINUE
        ENDIF

200   CONTINUE

C-----  NO LEVEL OF NON-MISSING DATA WAS LOCATED ABOVE THE
C       INTERPOLATION HEIGHT

      VALUE = MISS
      FLAG = 2

      RETURN

      END


      SUBROUTINE LWRUPR(ISTRG,RUNST)
C=====================================================================**
C
C     PURPOSE: Convert all characters from lower case to
C              upper case (using INDEX intrinsic function).
C              Note that the CHAR*80 RUNST1 Variable Includes
C              the Original Case for Echoing and for Later Use
C              To Retrieve Filenames.
C
C***********************************************************************

C     Variable Declarations
      CHARACTER UPCASE*26
      CHARACTER LWCASE*26
      CHARACTER*(*) RUNST

C     Variable Initializations
      DATA UPCASE/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      DATA LWCASE/'abcdefghijklmnopqrstuvwxyz'/

      DO 20 I = 1, ISTRG
         IF (RUNST(I:I) .NE. ' ') THEN
            INDCHK = INDEX(LWCASE,RUNST(I:I))
            IF (INDCHK .NE. 0) THEN
               RUNST(I:I) = UPCASE(INDCHK:INDCHK)
            END IF
         END IF
 20   CONTINUE

      RETURN
      END

      SUBROUTINE DEFINE(RUNSTR,MAXLEN,IFC)
C***********************************************************************
C
C     PURPOSE:    Defines the number of fields on a record
C
C     INPUTS:     One line of variables separated by spaces
C
C     OUTPUTS:    Number of Fields on Card, IFC
C
C     CALLED FROM: main program
C***********************************************************************
C
C     Variable Declarations
      IMPLICIT   NONE
      INTEGER    IFMAX
      PARAMETER (IFMAX = 27)

      INTEGER    IFC, MAXLEN, I
      CHARACTER  RUNSTR*(*)
      LOGICAL    INFLD

C     Initialize
      INFLD = .FALSE.
      IFC = 0

C     Loop through the Data Fields
      DO 20 I = 1, MAXLEN

         IF (.NOT.INFLD .AND. RUNSTR(I:I).NE.' ') THEN
C           Location is the Beginning of a Field
C           Set Mark of in a Field
            INFLD = .TRUE.
C           Increment the Field Counter
            IFC = IFC + 1
         ELSE IF (INFLD .AND. RUNSTR(I:I).EQ.' ') THEN
C           Location is the End of a Field
C           Set Mark of Not In a field
            INFLD = .FALSE.
         END IF

 20   CONTINUE

      RETURN
      END
