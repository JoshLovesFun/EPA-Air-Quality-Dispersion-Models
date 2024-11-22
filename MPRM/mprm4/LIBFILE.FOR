C     FILE: LIBFILE.FOR


c  ************  December 1999 Y2K Compliance Changes  ****************
c  *                                                                  *
c  *   A Y2K subroutine has been added.  The following subroutines    *
c  *   have been revised to include calls to the Y2K routine:         *
C  *                                                                  *
c  *   CHROND, GMTLST, GREG, HR0024, ICHRND, JULIAN (IN LIBFILE.FOR)  *
C  *                                                                  *
C  *   EXCARD (IN SETUP2.FOR)                                         *
c  *                                                                  *
c  *   These changes are the work of Desmond Bailey                   *
c  ********************************************************************

      SUBROUTINE Y2K ( PATH, IYR, I2YR, I4YR, CENTURY)

C     VERSION DATE: DECEMBER 1999

      INTEGER  IYR, I2YR, I4YR, CENTURY
      CHARACTER*2 PATH

c     IYR       Integer year (2 or 4 digits)
c     I2YR      Two-digit integer year (01-99)
c     I4YR      Four-digit integer year (1901-2099)
c     CENTURY   Two-digit century (20 or 21)

      IF (IYR .LT. 0) THEN                            !  Invalid year
         MESS = BLNK40
         WRITE(MESS,1000) IYR
         CALL ERROR(0,PATH,'E24',' Y2K  ',MESS)
         I2YR = -99
         I4YR = -9999
         CENTURY = -9
         RETURN
      ELSE IF (IYR .LE. 99) THEN                      !  2-digit year
         IF (IYR .GE. 50) THEN                        !  20th Century
            I4YR = IYR + 1900
            CENTURY = 20
         ELSE                                         !  21st Century
            I4YR = IYR + 2000
            CENTURY = 21
         END IF
         I2YR = IYR

      ELSE IF (IYR.GE.1901 .AND. IYR.LE.2099) THEN    !  4-digit year
         I4YR = IYR
         CENTURY = (IYR/100) + 1
         I2YR = I4YR - ((CENTURY-1)*100)
      ELSE                                            !  Invalid year
         MESS = BLNK40
         WRITE(MESS,1000) IYR
         CALL ERROR(0,PATH,'E24','  Y2K ',MESS)
         I2YR = -99
         I4YR = -9999
         CENTURY = -9
         RETURN
      END IF

      RETURN
 1000 FORMAT(' INPUT YEAR (',I4,') NOT IN RANGE OF 1901-2099')
      END


C     SUBROUTINE CHROND ( PATH, IYR, IJDY, ICDY )

C     PURPOSE:
C     THIS SUBROUTINE CONVERTS DATES BETWEEN 1900 AND 1999 TO A
C     COMMON SEQUENTIAL DAY.  THE ZERO POINT IS JANUARY 1, 1900
C     I.E., 1 IS JANUARY 1, 1900.

C     ARGUMENT LIST:

C          INPUT:    IYR   -    Year (2 or 4 digits)
C                    IJDY  -    Julian day (3 digits)
C          OUTPUT:   ICDY  -    Chronological day

C     CALLED BY:     UAEXT, GETSDG, GETMIX, SFEXT, GETSFC, MERGE
C                    EXCARD, OSNEXT, RD144D, RDSAMS, RDHUSW, ZIEXT

C     CALLS TO:      ERROR
C                    Y2K


C     LOCAL:         I2YR  -    2-digit year
C                    I4YR  -    4-digit year

C     VERSION DATE:  December 1999

C=======================================================================
      SUBROUTINE CHROND ( PATH, IYR, IJDY, ICDY )

      INTEGER  IYR, IJDY, ICDY, NDAY, ICNT, TEST
      INTEGER  I2YR, I4YR, CENTURY

c     Convert 2-digit year to 4-digit year as necessary
      CALL Y2K (PATH, IYR, I2YR, I4YR, CENTURY)

c     Check for invalid year

      IF ( I4YR.LT.1901 .OR. I4YR.GE.2099 ) THEN
         MESS = BLNK40
         WRITE(MESS,1000) I4YR
         CALL ERROR(0,PATH,'E24','CHROND',MESS)
         ICDY = -9999
         RETURN
      END IF

      IF ( IJDY.LE.0 .OR. IJDY.GE.367 ) THEN
         MESS = BLNK40
         WRITE(MESS,1010) IJDY
         CALL ERROR(0,PATH,'E24','CHROND',MESS)
         ICDY = -9999
         RETURN
      END IF

c     Test for leap year
      NDAY = 365
      IF(MOD(I4YR,4) .EQ.   0)NDAY = 366
      IF(MOD(I4YR,4) .EQ. 100)NDAY = 365
      IF(MOD(I4YR,4) .EQ. 400)NDAY = 366

c     Check for a valid Julian day
      IF ( IJDY .GT. NDAY ) THEN
         MESS = BLNK40
         WRITE(MESS,1030) IJDY
         CALL ERROR(0,PATH,'E24','CHROND',MESS)
         ICDY = -9999
         RETURN
      END IF

c     Compute the chronological day

      ICNT = (I4YR - 1900 +3)/4
      ICDY = 366*(ICNT) + 365*((I4YR-1900) -ICNT) + IJDY
      ICDY = ICDY - 1   !  Necessary since 1900 is not a leap year


      RETURN

 1000 FORMAT(' INPUT YEAR (',I4,') NOT IN RANGE OF 1901-2099')
 1010 FORMAT(' JULIAN DAY (',I4,') NOT IN RANGE OF 1-366')
 1030 FORMAT(' INVALID JULIAN DAY FOR A NON LEAP YEAR  ',I4)
      END


C     SUBROUTINE GMTLST ( YEAR,MONTH,DAY,HOUR,ZONE )

C     PURPOSE:
C     COMPUTES LOCAL STANDARD TIME FROM GREENWICH MEAN TIME
C     (LST FROM GMT).

C     CALLS TO:     GREG, JULIAN, Y2K

C     VERSION DATE: DECEMBER 1999

C=======================================================================
      SUBROUTINE GMTLST( JYR,JMO,JDA,JHR,JZONE )

      INTEGER JULIAN, JYR, JMO, JDA, JZONE, JULHR, JULDA, NDAY
      INTEGER JHR, JULHRC, JDATE
      INTEGER I2YR, I4YR, CENTURY

C  JYR        = INPUT/OUTPUT YEAR (2 OR 4 DIGITS)
C  JMO        = INPUT/OUTPUT MONTH
C  JDA        = INPUT/OUTPUT DAY
C  JHR        = INPUT/OUTPUT HOUR
C  JZONE      = INPUT TIME ZONES FROM GREENWICH
C               Number of hours to subtract (east longitude) or
C               to add (east lingitude) to convert GMT to LST.
C               - ==> WEST LONGITUDE
C               + ==> EAST LONGITUDE
C  JDATE      = TEMPORARY JULIAN DATE IN COMPUTATION
C  JULDA      = JULIAN DATE
C  JULHR      = JULIAN HOUR FROM JULIAN DATE
C  JULHRC     = JULIAN HOUR CORRECTED FROM GMT TO LST
C  NDAY       = NUMBER OF DAYS IN THE YEAR
C
C-----------------------------------------------------------------------

c     Convert 2-digit year to 4-digit year as necessary
      CALL Y2K(PATH, JYR, I4YR, I2YR, CENTURY)

C     COMPUTE JULIAN DAY
      JULDA = JULIAN(JYR,JMO,JDA)

C     COMPUTE JULIAN HOUR
      JULHR = (JULDA-1)*24. + JHR

C     SUBTRACT THE TIME ZONE FACTOR TO GET CORRECTED JULIAN HOUR
      JULHRC = JULHR + JZONE

C     DETERMINE THE JULIAN DAY FROM THE CORRECTED JULIAN HOUR
      JDATE = JULHRC/24 + 1

C     DETERMINE THE NUMBER OF DAYS IN THE YEAR
      NDAY = 365
      IF(MOD(I4YR,4).EQ.  0) NDAY = 366
      IF(MOD(I4YR,100).EQ.0) NDAY = 365
      IF(MOD(I4YR,400).EQ.0) NDAY = 366

C     COMPUTE THE YEAR, MONTH, DAY AND HOUR FOR THE CORRECTED
C     JULIAN HOUR
      IF(JULHRC .LT. 0)THEN
         JYR   = JYR - 1
         I2YR  = I2YR + 1
         I4YR  = I4YR + 1
         JMO   = 12
         JDA   = 31
         JHR   = 24 + JHR - JZONE
      ELSE IF(JDATE .GT. NDAY)THEN
         JDATE = JDATE - NDAY
         JYR   = JYR + 1
         I2YR  = I2YR + 1
         I4YR  = I4YR + 1
         CALL GREG(JYR,JDATE,JMO,JDA)
         JHR   = MOD(JULHRC,24)
      ELSE
         CALL GREG(JYR,JDATE,JMO,JDA)
         JHR   = MOD(JULHRC,24)
      ENDIF

      RETURN
      END


C     SUBROUTINE GREG (YEAR, JULIAN DAY, MONTH, DAY)

C     PURPOSE:
C     THIS ROUTINE COMPUTES THE GREGORIAN MONTH AND DAY GIVEN THE YEAR
C     AND JULIAN DAY
C
C     CALLED BY:     ANY ROUTINE THAT NEEDS TO CONVERT JULIAN TO
C                    GREGORIAN DAY
C
C     CALLS TO:      Y2K
C
C     VERSION DATE:  DECEMBER 1999
C
C=======================================================================
      SUBROUTINE GREG(IYR, JDAY, MNTH, MDAY )

      INTEGER*4 IYR, JDAY, MNTH, MDAY, L, J
      INTEGER I2YR, I4YR, CENTURY

C     YEAR = 2 OR 4-DIGIT CALENDAR YEAR
C     JDAY = JULIAN DAY OF YEAR -- 1, 365, OR 366
C     MNTH = MONTH OF YEAR -- 1, 12
C     MDAY = DAY OF MONTH -- 1, 31

C-----------------------------------------------------------------------

c     Convert 2-digit year to 4-digit year as necessary
      CALL Y2K (PATH, IYR, I2YR, I4YR, CENTURY)

      L = 365
      IF(MOD(I4YR,4)   .EQ. 0) L = 366
      IF(MOD(I4YR,100) .EQ. 0) L = 365
      IF(MOD(I4YR,400) .EQ. 0) L = 366

      J = MOD(JDAY+305,L)
      J = MOD(J,153)/61+(J/153)*2+J
      MNTH = MOD(J/31+2,12)+1
      MDAY = MOD(J,31)+1

      RETURN
      END


C  SUBROUTINE HR0024 (IYR, JDAY, IHR)
C
C  PURPOSE
C     TO CONVERT A DATE/TIME GROUP FROM HOUR=0 TO HOUR=24 OF THE
C     PREVIOUS DAY
C
C  CALLED BY:     GETSDG, MERGE
C
C  CALLS TO:      -NONE-
C  VERSION DATE:  DECEMBER 1999
C
C=======================================================================
      SUBROUTINE HR0024(IYR, JDAY, IHR)
C
      INTEGER IYR, JDAY, IHR
      INTEGER I2YR, I4YR, CENTURY
C
C     IYR    = 2-DIGIT YEAR
C     JDAY   = JULIAN DAY
C     IHR    = IHR
C
C-----------------------------------------------------------------------
C     Determine the 4-digit year
      CALL Y2K(PATH, IYR, I2YR, I4YR, CENTURY)

      IHR = 24
      IF(JDAY .EQ. 1) THEN
         I4YR = I4YR - 1
C        Recompute the 2-digit year for return to calling program
         CALL Y2K(PATH, I4YR, I2YR, I4YR, CENTURY)
         IYR = I2YR

C        Test for leap year
         JDAY = 365
         IF(MOD(I4YR,4) .EQ.   0)JDAY = 366
         IF(MOD(I4YR,4) .EQ. 100)JDAY = 365
         IF(MOD(I4YR,4) .EQ. 400)JDAY = 366

      ELSE
         JDAY = JDAY - 1
      ENDIF

      RETURN
      END



C     SUBROUTINE ICHRND(PATH, ICDY, I2YR, IJDY)
C
C     PURPOSE
C     THIS SUBROUTINE CONVERTS A COMMON SEQUENTIAL DAY TO A DATE
C     BETWEEN 1900 AND 2099.  THE ZERO POINT IS JANUARY 1, 1990;
C     I.E., DAY 1 IS THE FIRST DAY OF 1900.
C
C     ARGUMENT LIST:
C          INPUT:   ICDY  - COMMON SEQUENTIAL DAY
C          OUTPUT:  I2YR  - YEAR (2 DIGITS)
C                   IJDY  - JULIAN DAY (3 DIGITS)
C
C     CALLED BY:     MERGE
C
C     CALLS TO:      Y2K
C
C     VERSION DATE:  DECEMBER 1999
C
C=======================================================================
      SUBROUTINE ICHRND(PATH, ICDY, I2YR, IJDY)

      INTEGER ICDY,IYR,IJDY
      INTEGER I2YR, I4YR, CENTURY

C     IOLD  = OLD SEQUENTIAL DAY
C     INEW  = NEW SEQUENTIAL DAY
C-----------------------------------------------------------------------
C     CHECK FOR INVALID DATA

      IF ((ICDY.LT.1) .OR. (ICDY.GT.73050)) THEN
         MESS = BLNK40
         WRITE(MESS,1000) ICDY
         CALL ERROR(ICDY,PATH,'E24','ICHRND',MESS)
         IYR = -99
         IJDY = -99
         RETURN
      ENDIF

C     CONVERT COMMON SEQUENTIAL DAY TO 4-DIGIT YEAR AND JULIAN DAY

      IOLD = ICDY
      I4YR = 1900

    1 NDAY = 365
      IF(MOD(I4YR,4)   .EQ. 0) NDAY = 366
      IF(MOD(I4YR,100) .EQ. 0) NDAY = 365
      IF(MOD(I4YR,400) .EQ. 0) NDAY = 366

      INEW = IOLD - NDAY

C     WHEN THE CHRONOLOGICAL DAY BECOMES NEGATIVE, THE YEAR AND JULIAN
C     DAY HAVE BEEN DETERMINED

      IF(INEW .GT. 0) THEN
         I4YR = I4YR + 1
         IOLD = INEW
         GO TO 1
      ELSE IF(INEW .LE. 0) THEN
         IJDY = IOLD
      ENDIF

C     CHECK FOR VALID YEAR AND JULIAN DAY

      IF(I4YR .GT. 2099) THEN
         GO TO 999
      END IF

      IF((IJDY .LT. 0) .OR. (IJDY.GT.NDAY)) THEN
         GO TO 999
      ENDIF

c     Call Y2K to get the 2-digit year for pass back
      CALL Y2K (PATH, I4YR, I2YR, I4YR, CENTURY)

      RETURN

  999 MESS = BLNK40
      WRITE(MESS,2000)
      CALL ERROR(ICDY,PATH,'E24','ICHRND',MESS)
      IYR  = -99
      I2YR = -99
      I4YR = -99
      IJDY = -99
      RETURN

 1000 FORMAT(' CHRONOLOGICAL DAY NOT IN RANGE (1-73050) ',I7)
 2000 FORMAT(' YEAR/JULIAN DAY NOT VALID; SET TO -99' )

      END


C     SUBROUTINE JULIAN ( YEAR,MONTH,DAY )

C     PURPOSE
C     ROUTINE CONVERTS GREGORIAN MONTH AND DAY
C     TO JULIAN DAY, FOR GIVEN YEAR.
C
C     LIMITATIONS:  ROUTINE WORKS IN THOSE COUNTRIES USING
C                   OUR CURRENT CALENDAR.
C
C     CALLED BY:    ANY ROUTINE THAT NEEDS TO CONVERT GREGORIAN
C                   TO JULIAN DAY
C
C     CALLS TO:     Y2K
C
C     VERSION DATE:  DECEMBER 1999
C
C=======================================================================

      FUNCTION JULIAN( IYR,MO1,DY1 )

C     LOCAL VARIABLES

      INTEGER IYR,MO1,DY1,JULIAN
      INTEGER I2YR, I4YR, CENTURY

C        IYR        2 OR 4-DIGIT CALENDAR YEAR
C        MO1        MONTH OF YEAR (1-12)
C        DY1        DAY OF MONTH (1-31)
C-----------------------------------------------------------------------


c     Convert 2-digit year to 4-digit year as necessary
      CALL Y2K (PATH, IYR, I2YR, I4YR, CENTURY)

      IF( I4YR.LE.0 .OR. MO1.LE.0 .OR. DY1.LE.0 ) THEN
         JULIAN = 0
      ELSE

         IWORK1 = MOD( (MO1+9),12 )
         IWORK2 = (IWORK1*153+2)/5 + DY1 + 58

         NDAY = 365
         IF(MOD(I4YR,4).EQ.  0) NDAY = 366
         IF(MOD(I4YR,100).EQ.0) NDAY = 365
         IF(MOD(I4YR,400).EQ.0) NDAY = 366

         IF( NDAY.EQ.366 ) THEN
            IWORK2 = IWORK2 + 1
         END IF

         JULIAN = 1 + MOD( IWORK2,NDAY )

      END IF

      RETURN
      END




C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C  SUBROUTINE ATAPE2 (ALTERNATE RETURN, INPUT DEVICE, ARRAY FOR DATA,
C                     ARRAY SIZE, STATUS, SYSTEM I/O STATUS)
C
C  PURPOSE
C    THIS ROUTINE READS  BLOCKS FROM A TAPE. IT IS
C     CAPABLE OF READING FIXED OR VARIABLE BLOCK RECORDS.  TO READ
C     VARIABLE BLOCK TAPES, ASSIGN THE TAPE AND SPECIFY THE MAXIMUM
C     NUMBER OF CHARACTERS PER BLOCK IN THE MOUNT STATEMENT.
C
C  CALLED BY:    GETSDG, GETMIX, GETSFC
C
C  CALLS TO:     EHANDL
C
C  VERSION DATE: 12 JULY 1988
C
C=======================================================================
      SUBROUTINE ATAPE2( *,IDEVRD,BUFF,NCHAR,ISTAT,IOST )
C
      INTEGER      ISTAT, IOST, IDEVRD, NCHAR
      CHARACTER*1  BUFF(NCHAR)
C
C  BUFF      OUTPUT: ARRAY FOR DATA READ FROM TAPE
C  IDEVRD    UNIT NUMBER ASSIGNED TO TAPE FOR READ
C  IOST      OUTPUT: ERROR STATUS MESSAGE RETURNED FROM TAPE READ
C  ISTAT     OUTPUT: STATUS OF READ - GOOD(0), BAD(1), EOF(-1)
C
C-----------------------------------------------------------------------
      CALL EHANDL( 212 )
  100 READ(IDEVRD,1000,END=101,ERR=102,IOSTAT=IOST) BUFF
 1000 FORMAT(100(80A1))
C
      ISTAT = 0
      RETURN 1
C
C--- THE END OF THE FILE HAS BEEN ENCOUNTERED
C
  101 ISTAT = -1
      RETURN 1
C
C---  AN ERROR WAS ENCOUNTERED READING THE TAPE
C
  102 ISTAT = 1
      RETURN 1
C
C---
      END
C
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C  SUBROUTINE BANNER
C
C  PURPOSE
C     TO WRITE THE BANNER HEADER TO THE DEVICE SPECIFIED BY THE INPUT
C     ARGUMENT
C
C  CALLED BY:     SUMRY1, SUMRY2, AUDIT, METPROCESSOR (STAGE 3)
C
C  CALLS TO:      DATER
C  VERSION DATE:  16 MARCH 1988
C
C=======================================================================
      SUBROUTINE BANNER( LUN )
C
      INTEGER    LUN
      CHARACTER  DMY*18, HMS*8

      INCLUDE 'MAIN1.INC'

C     LUN          LOGICAL UNIT NUMBER OF FILE TO WRITE BANNER TO
C     DMY, HMS     DATE AND TIME RETURNED FROM SYSTEM CLOCK
C     IVDATE       Version date 'YYJJJ' as defined in MASTER.INC        DTBJUN93

C  CALL THE SYSTEM DATE AND TIME

      CALL DATER ( DMY,HMS )

C  WRITE THE BANNER

      WRITE( LUN,5000 ) IVDATE                                          DTB94251

      WRITE( LUN,5010 )
      WRITE( LUN,5020 ) DMY, HMS

      RETURN

 5000 FORMAT('1',/9X,'METEOROLOGICAL PROCESSOR FOR REGULATORY MODELS',
     &  ' [MPRM (dated ', I5, ')]' )                                    DTBJUN93

 5010 FORMAT(1X)
 5020 FORMAT(1X, T25, A18, 5X, A8)                                      DTB99349

      END
C
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C  SUBROUTINE ERROR ( NUMBER,PATH,ERROR CODE,LOCATION,MESSAGE )
C
C  PURPOSE:
C    WE USE THIS ROUTINE TO WRITE OUR ERROR MESSAGES OUT TO THE ERROR
C    MESSAGE LOG FILE.
C
C    THIS MAKES THE ERROR MESSAGES ALL FIT A STANDARD FORMAT, WHICH THEN
C    MAKES IT POSSIBLE TO REREAD THEM AND GENERATE GENERAL SUMMARY
C    REPORTS.
C
C  CALLED BY:    MOST SUBROUTINES
C
C  CALLS TO:     -NONE-
C
C  VERSION DATE: 01 SEPTEMBER 1987
C
C=======================================================================
         SUBROUTINE ERROR( NUMBER,CPATH,CODE,CLOC,CMESS )

C        LOCAL VARIABLES
C
        INTEGER   DEVICE,NUMBER
        CHARACTER CPATH*2,CODE*3,CLOC*6,CMESS*40
C
        INCLUDE 'MAIN1.INC'
        INCLUDE 'MAIN2.INC'
C
C-----------------------------------------------------------------------
C        CHECK TO SEE IF ERROR REPORT FILE IS AVAILABLE, IF NOT
C        USE DEVIO, ELSE USE DEV60
C
        IF( STATUS(1,5).EQ.2 ) THEN
        DEVICE = DEV60
        ELSE
        DEVICE = DEVIO
        END IF
C
        WRITE(DEVICE,1000) NUMBER,CPATH,CODE,CLOC,CMESS
1000    FORMAT(I10,1X,A2,1X,A3,1X,A6,':',A40)
C
        RETURN
        END
C
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C  SUBROUTINE FLBUFF
C
C  PURPOSE
C    THIS SUBROUTINE IS USED TO 'BLANK OUT' THE BUFFERS THAT ARE
C    USED IN READING DATA FROM MAGNETIC TAPE.  THERE ARE TWO ENTRY
C    POINTS, ONE FOR THE 8000 CHARACTER BUFFER AND THE OTHER FOR THE
C    4000 CHARACTER BUFFER.  THERE ARE SEVERAL OTHER ENTRY POINTS
C    THAT ARE USED TO DEFINE VARIABLES AS MISSING OR ZERO.
C
C     MOST VARIABLES ARE DEFINED IN THE 'INCLUDE' FILES
C
C  CALLED BY:     ANY SUBROUTINE THAT NEEDS TO CLEAR AN ARRAY
C
C  CALLS TO:      -NONE-
C
C  VERSION DATE:  05 JANUARY 1988
C
C=======================================================================
C
      SUBROUTINE FLBUFF
C
      INTEGER MH, J, I
C
      INCLUDE 'UA1.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'UA2.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'WORK1.INC'
C
C-----------------------------------------------------------------------
      ENTRY FLBUF1
      DO 10 I = 1,NCHB1
       BUFFR1(I) = ' '
   10 CONTINUE
      RETURN
C
C-----------------------------------------------------------------------
      ENTRY FLBUF2
      DO 20 I = 1,NCHB2
       BUFFR2(I) = ' '
   20 CONTINUE
      RETURN
C
C-----------------------------------------------------------------------
      ENTRY FLSDG(MH)
C MH = MAXIMUM NUMBER OF HOURS TO FLUSH, BEGINNING WITH 1
C
      DO 30 K = 1,MH
         UAYR(K)  = -9
         UAMO(K)  = -9
         UADAY(K) = -9
         UAHR(K)  = -9
         DO 35 I = 1,UAML
            DO 40 J =1,UAMV
               UAOBS(K,I,J) = UAQA(J,2)
   40       CONTINUE
   35    CONTINUE
   30 CONTINUE
      RETURN
C
C-----------------------------------------------------------------------
      ENTRY FLZI
      MIXC1 = UAQA(11,2)
      MIXP1 = UAQA(11,2)
      MIXN1 = UAQA(11,2)
      MIXC2 = UAQA(12,2)
      MIXP2 = UAQA(12,2)
      MIXN2 = UAQA(12,2)
      RETURN
C
C-----------------------------------------------------------------------
      ENTRY FLSFC(MH)
C MH = MAXIMUM NUMBER OF HOURS TO FLUSH, BEGINNING WITH 1
C      NOTE: Variable # 52 is for precipitation
C
      DO 50 K = 1,MH
         SFYR(K)  = SFQA(56,2)
         SFMO(K)  = SFQA(57,2)
         SFDAY(K) = SFQA(58,2)
         SFHR(K)  = SFQA(59,2)
         DO 55 I = 30,52
            SFOBS(K,I) = SFQA(I,2)
   55    CONTINUE
   50 CONTINUE
C
      RETURN
C
C-----------------------------------------------------------------------
      ENTRY FLPPT(MH)
C MH = MAXIMUM NUMBER OF HOURS TO FLUSH, BEGINNING WITH 1
C
C      NOTE: Variable # 52 is for precipitation in SFQA and VNAMES
C
       DO 57 I = 1,MH
          PPTAMT(I) = -9
   57  CONTINUE

      RETURN
C-----------------------------------------------------------------------
      ENTRY FLOS(MH)
C MH = MAXIMUM NUMBER OF HOURS TO FLUSH, BEGINNING WITH 1
C
      DO 60 K = 1,MH
         OSYR(K)  = SFQA(56,2)
         OSMO(K)  = SFQA(57,2)
         OSDAY(K) = SFQA(58,2)
         OSHR(K)  = SFQA(59,2)
         OSMN(K)  = SFQA(60,2)
         DO 65 J = 1,OSNL
            DO 70 I = 15,29
               OSVOBS(K,J,I-14) = FLOAT(SFQA(I,2))
   70       CONTINUE
   65    CONTINUE
C
         DO 75 I = 1,14
            OSSOBS(K,I) = FLOAT(SFQA(I,2))
   75    CONTINUE
         DO 80 I =30,52
            OSSOBS(K,I-15) = SFQA(I,2)
   80    CONTINUE
C
   60 CONTINUE
C
      RETURN
C
C-----------------------------------------------------------------------
      ENTRY FLWRK2
      DO 100 I = 1,AD2
         DO 110 J = 1,AD3
            WORK2(I,J) = -9999.0
  110    CONTINUE
  100 CONTINUE
      RETURN
C
C-----------------------------------------------------------------------
      ENTRY FLWRK1
      DO 120 I = 1,AD1
         WORK1(I) = -9999.0
  120 CONTINUE
      RETURN
C-----------------------------------------------------------------------
      ENTRY FLIWK2
      DO 130 I = 1,AD2
         DO 140 J = 1,AD3
            IWORK2(I,J) = -9999
  140    CONTINUE
  130 CONTINUE
      RETURN
C
C-----------------------------------------------------------------------
      ENTRY FLIWK1
      DO 150 I = 1,AD1
         IWORK1(I) = -9999
  150 CONTINUE
      RETURN
C-----------------------------------------------------------------------
      END
C
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C  SUBROUTINE FLHEAD
C
C  PURPOSE
C    THIS ROUTINE PROCESSES THE HEADER RECORDS FROM
C     THE 3 INPUT FILES IN MERGE FOR UNFORMATTED OUTPUT FILE
C     IN ENTRY POINT MRHDR.  PROCESSING OF HEADERS BY ALL
C     OTHER PROGRAMS IS DONE IN ENTRY POINT OTHHDR.
C
C  CALLED BY:     ANY SUBROUTINE THAT WRITES FILE HEADER RECORDS
C
C  CALLS TO:      ERROR
C
C  VERSION DATE:  30 SEPT 1992
C
C=======================================================================
      SUBROUTINE FLHEAD
C
      INTEGER IDEV(4),IL,OUDEV,NUM,IOST
      INTEGER IDEV1,IDEV2,IDEV3,IDEV4
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'
C
C---  INPUT DEVICES USED
C  IDEV(2)     = INPUT FILE (FOR MERGE,  RAWINSONDE DATA)
C  IDEV(3)     = FOR MERGE, SURFACE OBS DATA
C  IDEV(4)     = FOR MERGE, ONSITE DATA
C
C---  OUTPUT DEVICES USED
C  OUDEV     = OUTPUT FILE (UNFORMATTED FOR MERGED DATA)
C  IDEV(1)   = SUMMARY FILE
C
C
C  PATH      = 2-CHARACTER IDENTIFIER FOR THE PATH
C  LOC       = 6-CHARACTER IDENTIFIER OF PROGRAM CALLING THIS ROUTINE
C  IDEV(2)   = INPUT LOGICAL UNIT NUMBER (UPPER AIR FOR MERGE)
C  IDEV(3)   = INPUT LOGICAL UNIT NUMBER OF SF DATA - FOR MERGE ONLY
C  IDEV(4)   = INPUT LOGICAL UNIT NUMBER OF OS DATA - FOR MERGE ONLY
C  OUDEV     = OUTPUT DEVICE LOGICAL UNIT NUMBER
C  NUM       = PATH NUMBER IN THE ARRAY "STATUS(NUM,-)"
C  KOUNT     = HEADER COUNTER
C  IWORK1( ) = HEADER COUNTER FOR THE INPUT FILES TO MERGE
C
C
C--- FORMAT STATEMENTS USED BY BOTH ENTRY POINTS
C
  200 FORMAT(A3,A80)
  217 FORMAT(10X,I5,' HEADERS PROCESSED FROM INPUT FILES')
  400 FORMAT(' HDR ERROR, DEV',I3,', HDR',I4,', IOST=',I6)
  401 FORMAT(' HEADER E-O-F ON INPUT DEV ',I3)
C=======================================================================
C
      ENTRY MRHDR(PATH,LOC,NUM,IDEV2,IDEV3,IDEV4,OUDEV,IDEV1)
C
      IDEV(2) = IDEV2
      IDEV(3) = IDEV3
      IDEV(4) = IDEV4
      KOUNT   = 1
      REWIND OUDEV
C
C  READ THE FIRST 3 CHARACTERS OF EACH RECORD IN OUDEV AND TEST THE
C  FIRST CHARACTER TO READ TO THE END OF VALID HEADER RECORDS
    1 BUF03 = '   '
      READ(OUDEV,END=110,ERR=120,IOSTAT=IOST)BUF03
      IF(BUF03(1:1) .EQ. '*') THEN
C ***  HEADERS ARE PRESENT IN THE OUTPUT FILE
C
       KOUNT = KOUNT + 1
       GO TO 1
C
      ELSE IF(BUF03 .EQ. '   ') THEN
C ***  NO HEADERS IN THE OUTPUT FILE
C
       REWIND OUDEV
       KOUNT = 1
C
C  PROCESS DESCRIPTIVE HEADERS FROM EACH INPUT FILE AND WRITE TO
C  OUDEV.  READ FIRST 3 CHARACTERS TO IDENTIFY VALID HEADER RECORDS,
C  THEN BACKSPACE AND READ FULL RECORD.
       DO 130 IL=2,4
        IWORK1(950) = 0
        IF(STATUS(IL,5) .GE. 2) THEN
         REWIND IDEV(IL)
  131    IWORK1(950) = IWORK1(950) + 1
         BUF03 = '   '
         READ(IDEV(IL),200,END=1301,ERR=1302,IOSTAT=IOST) BUF03
         IF(BUF03(1:1) .EQ. '*') THEN
          BACKSPACE IDEV(IL)
          READ(IDEV(IL),200,END=1301,ERR=1302,IOSTAT=IOST) BUF03,
     *                                                     BUF80(1)
          WRITE(OUDEV) BUF03,BUF80(1)
          KOUNT = KOUNT + 1
          GO TO 131
         ELSE
          BACKSPACE IDEV(IL)
          GO TO 130
         ENDIF
        ELSE
          GO TO 130
        ENDIF
C
C- PROCESSING CONTINUES HERE FOR ERRORS AND END-OF-FILES
 1301   MESS = BLNK40
        WRITE(MESS,401) IDEV(IL)
        CALL ERROR(0,PATH,'W22',LOC,MESS)
        STATUS(IL,5) = 0
        GO TO 130
 1302   MESS = BLNK40
        WRITE(MESS,400) IDEV(IL), IWORK1(950), IOST
        CALL ERROR(0,PATH,'E22',LOC,MESS)
        NUM = -1
        RETURN
C
  130  CONTINUE
      ENDIF
C
      WRITE(IDEV1,217) KOUNT
       PRINT 217,KOUNT
      RETURN
C
  120 MESS = BLNK40
      WRITE(MESS,400) OUDEV,KOUNT
      CALL ERROR(0,PATH,'E23',LOC,MESS)
      NUM = -1
      RETURN
C
C-----------------------------------------------------------------------
C-    OUTPUT FILE HEADERS PROCESSED;
C-    READ DESCRIPTIVE HEADERS FROM EACH INPUT FILE TO POSITION DATA
C-     FIRST THE RAWINSONDE DATA
C
  110  IF(BACK40) BACKSPACE OUDEV
       DO 135 IL=2,4
       IWORK1(950) = 0
       IF(STATUS(IL,5) .GE. 2) THEN
        REWIND IDEV(IL)
  136   IWORK1(950) = IWORK1(950) + 1
        BUF03 = '   '
        READ(IDEV(IL),200,END=1311,ERR=1312,IOSTAT=IOST) BUF03
        IF(BUF03(1:1) .EQ. '*') THEN
         GO TO 136
        ELSE
         BACKSPACE IDEV(IL)
         GO TO 135
        ENDIF
       ELSE
         GO TO 135
       ENDIF
C
C- PROCESSING CONTINUES HERE FOR ERRORS AND END-OF-FILES
 1311  MESS = BLNK40
       WRITE(MESS,401) IDEV(IL)
       CALL ERROR(0,PATH,'W22',LOC,MESS)
       STATUS(IL,5) = 0
       GO TO 135
 1312  MESS = BLNK40
       WRITE(MESS,400) IDEV(IL),IWORK1(950), IOST
       CALL ERROR(0,PATH,'E22',LOC,MESS)
       NUM = -1
C
  135 CONTINUE
C
      RETURN
C
C=======================================================================
C- THIS ENTRY POINT PROCESSES HEADERS FROM ALL OTHER ROUTINES
C   IN THE SAME MANNER AS ABOVE EXCEPT THERE IS ONLY ONE INPUT FILE
C   TO THE ROUTINE
C
      ENTRY OTHHDR(PATH,LOC,NUM,IDEV2,OUDEV,IDEV1)
      KOUNT =  1
      REWIND OUDEV
      REWIND IDEV2
    2 BUF03 = '   '
      READ(OUDEV,200,END=210,ERR=215,IOSTAT=IOST) BUF03
C
      IF(BUF03(1:1) .EQ. '*') THEN
       KOUNT = KOUNT + 1
       GO TO 2
C
      ELSE IF(BUF03 .EQ. '   ') THEN
       REWIND OUDEV
    3  BUF03 = '   '
       READ(IDEV2,200,END=220,ERR=225,IOSTAT=IOST) BUF03
       IF(BUF03(1:1) .EQ. '*') THEN
        BACKSPACE IDEV2
        READ(IDEV2,200,END=220,ERR=225,IOSTAT=IOST) BUF03,BUF80(1)
        WRITE(OUDEV,200) BUF03,BUF80(1)
        KOUNT = KOUNT  + 1
        GO TO 3
       ELSE
        BACKSPACE IDEV2
        RETURN
       ENDIF
      ENDIF
C
C *** OUTPUT FILE HEADERS PROCESSED: READ, BUT DO NOT WRITE, INPUT
C     FILE HEADERS
C
  210   IF(BACK40) BACKSPACE OUDEV
        REWIND IDEV2
        KOUNT = 1
    4   BUF03 = '   '
        READ(IDEV2,200,END=220,ERR=225,IOSTAT=IOST) BUF03
        IF(BUF03(1:1) .EQ. '*') THEN
         KOUNT = KOUNT  + 1
         GO TO 4
        ELSE
         BACKSPACE IDEV2
        ENDIF
      RETURN
C
  215 MESS = BLNK40
      WRITE(MESS,400) OUDEV,KOUNT
      CALL ERROR(0,PATH,'E23',LOC,MESS)
      NUM = -1
      RETURN
C
  220 MESS = BLNK40
      WRITE(MESS,401) OUDEV
      CALL ERROR(0,PATH,'W22',LOC,MESS)
      STATUS(NUM,5) = 0
      RETURN
C
  225 MESS = BLNK40
      WRITE(MESS,400) IDEV2,KOUNT
      CALL ERROR(0,PATH,'E22',LOC,MESS)
      NUM = -1
      RETURN
C
C----------------------------------------------------------------------
      END


C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=

C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C  SUBROUTINE P2MSUB
C
C  PURPOSE
C    TO CONVERT DATA TO STANDARD METEOROLOGICAL UNITS
C    THERE ARE SEVERAL ENTRY POINTS TO MAKE THESE COMPUTATIONS
C
C  CALLED BY: SFLEVS
C
C  VERSION DATE: 8 AUGUST 1987
C
C=======================================================================
       SUBROUTINE P2MSUB
C
       REAL      MILBAR,FEET,FAHR1,FAHR,INCHES,MBARS,KNOTS,MTRSEC
       REAL      KNTS, MPM, ABSENT, FAHRN, CENTG, AVG, AN, RH, VRH
       REAL      T, TD, ES, E, B
       INTEGER   IABSNT, IFEET, IMETER, IMILE
       PARAMETER (MILBAR=33.8639,FAHR=32.,FAHR1=5./9.,FEET=.3048)
       PARAMETER (MPM=1609.0, IABSNT=-9999, ABSENT=-9999.0)
       PARAMETER (KNTS=.514791)
C
C  ABSENT   = MISSING VALUE INDICATOR
C  AN       = VALUE INDICATING IF NONMISSING RH VALUE
C  AVG      = VALUE USED TO DETERMINE IF RH IS IN FRACTION OR %
C  B        = COMPUTATIONAL VARIABLE USED IN CALCULATION OF DEW POINT
C  CENTG    = TEMPERATURE IN DEGREES CENTIGRADE
C  E        = VAPOR PRESSURE
C  ES       = SATURATED VAPOR PRESSURE
C  FAHR     = FREEZING POINT FOR FAHRENHEIT SCALE
C  FAHR1    = CONVERSION FACTOR FOR DEGREES F TO C
C  FAHRN    = TEMPERATURE IN DEGREES FAHRENHEIT
C  FEET     = CONVERSION FOR FEET TO METERS
C  IFEET    = DISTANCE IN FEET
C  IMETER   = DISTANCE IN METERS
C  IMILE    = DISTANCE IN MILES
C  INCHES   = PRESSURE IN INCHES
C  IPCT     = VALUE IN PERCENT
C  KNOTS    = SPEED IN KNOTS
C  KNTS     = CONVERSION FACTOR FOR KNOTS TO METERS/SEC
C  LA1      = INTEGER VALUE TO BE CONVERTED TO PERCENT
C  MBARS    = PRESSURE IN MILLIBARS
C  MILBAR   = CONVERSION FACTOR FROM INCHES TO MILLIBARS
C  MPM      = CONVERSION FACTOR FOR MILES TO METERS (1609 METERS/MI)
C  MTRSEC   = SPEED IN METERS PER SECOND
C  RH       = RELATIVE HUMIDITY
C  T        = TEMPERATURE IN DEGREES C
C  TD       = DEW POINT(DEG C)
C  VRH      = CONVERSION FACTOR TO PUT RH IN PERCENT
C
C=======================================================================
       ENTRY P2MMBR(INCHES,MBARS)
C
C     THIS SUBROUTINE CONVERTS FROM INCHES TO MILLIBARS.
C
           IF(INCHES.LT.0.0) THEN
            MBARS=ABSENT
            GO TO 100
           END IF
           MBARS=INCHES*MILBAR
100    RETURN
C
C=======================================================================
       ENTRY P2MCEN(FAHRN,CENTG)
C
C     THIS ENTRY CONVERTS FAHRENHEIT TO CELSIUS
C
           IF(FAHRN.LT.-200.0) THEN
            CENTG=ABSENT
            GO TO 200
           END IF
           CENTG=FAHR1*(FAHRN-FAHR)
C
200    RETURN
C
C=======================================================================
       ENTRY P2MMTR(IFEET,IMETER)
C
C     THIS ENTRY CONVERTS FEET TO METERS
C
           IF(IFEET.LT.0) THEN
            IMETER=-9999
            GO TO 300
           END IF
           IMETER=IFEET*FEET
           IF(IMETER .LT. 0) THEN
           ENDIF
C
300    RETURN
C
C=======================================================================
       ENTRY P2MMSC(KNOTS,MTRSEC)
C
C     THIS ENTRY CONVERTS KNOTS TO METERS PER SEC
C
           IF(KNOTS.LT.0.0) THEN
            MTRSEC=ABSENT
            GO TO 400
           END IF
           MTRSEC=KNOTS*KNTS
C
400        RETURN
C
C=======================================================================
       ENTRY P2MDP (T,RH,TD)
C*
C     THIS ENTRY CALCULATES DEWPOINT FROM TEMPERATURE AND
C      RELATIVE HUMIDITY
C*
C     TEMPERATURE IN DEGREES C AND RH MAY
C      BE EXPRESSED AS EITHER A FRACTION OR A PERCENT
C*
       AVG=0.
       AN=0.
       IF(RH.LT.0.0) GO TO 41
       AVG=AVG+RH
       AN=AN+1.
41     IF(AN.GT.0.) AVG=AVG/AN
C
C*  AVG IS USED TO DETERMINE WHETHER RH IS PERCENT OR FRACTION
       IF(AVG.GT.1.) THEN
        VRH=0.01
       ELSE
        VRH=1.
       END IF
       IF(T.LT.-200.0 .OR. RH.LT.0.0) THEN
        TD=ABSENT
        GO TO 42
       ELSE
        ES=6.1078*EXP(17.2964*T/(T+237.3))
        E=VRH*RH*ES
        B=ALOG(E/6.1078)
        TD=237.3*B/(17.2964-B)
       END IF
42    CONTINUE
      RETURN
C
C=======================================================================
      ENTRY P2MMM(IMILE,IMETER)
C
C    THIS ROUTINE CONVERTS MILES TO METERS
C
       IF(IMILE.LT.0) THEN
         IMETER=ABSENT
       ELSE
         IMETER=IMILE*MPM
       ENDIF
C
      RETURN
      END
C
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C  SUBROUTINE SCNGEN
C
C  PURPOSE
C    THIS SUBROUTINE GENERATES THE SCAN REPORTS OF TAPES AND FILES
C
C  CALLED BY:     GETSDG, GETMIX, GETSFC
C
C  CALLS TO:      -NONE-
C
C  VERSION DATE:  30 SEPTEMBER 1987
C
C-----------------------------------------------------------------------
      SUBROUTINE SCNGEN( KOUNT,FLAG,LOC1,IYR,IMO,IDY,IHR,N1,ST1 )
C
      INTEGER    IYR,IMO,IDY,IHR,N1,NN,JULIAN,IOST
      CHARACTER  LOC1*8,LOC2*8,ST1*79
      LOGICAL    FLAG
C
      INCLUDE 'WORK1.INC'
C-----------------------------------------------------------------------
C
C     KOUNT      A COUNTER
C     FLAG       FLAG INDICATING IF THIS IS THE FIRST ACCESS TO THIS
C                 SUBROUTINE ON THIS PATHWAY
C     LOC1       CURRENT RECORD STATION ID
C     LOC2       PREVIOUS RECORD STATION ID
C     IYR,IMO    PREVIOUS RECORD YEAR, MONTH, DAY AND HOUR
C      IDY,IHR
C     N1         STARTING LOCATION IN IWORK ARRAY TO STORE DATA -
C                 VARIES BY PATH
C     ST1        STRING RETURNED TO CALLING PROGRAM WITH SCAN DATA
C
C-----------------------------------------------------------------------
C- THE FIRST PART OF THE IF- BLOCK IS EXECUTED ONLY FOR THE FIRST CALL
C    TO THIS ROUTINE BY EACH DATA TYPE
C
      IF( FLAG ) THEN
        LOC2 = LOC1
        IWORK1(N1)   = IYR
        IWORK1(N1+1) = IMO
        IWORK1(N1+2) = IDY
        IWORK1(N1+3) = IHR
        IWORK1(N1+4) = JULIAN(IYR,IMO,IDY)
        IWORK1(N1+11) = 0
        IWORK1(N1+12) = 0
        IWORK1(N1+13) = 0
        IWORK1(N1+14) = 0
        FLAG = .FALSE.
      ELSE
C
C- THIS PART OF THE IF- BLOCK IS EXECUTED FOR EVERY SUBSEQUENT CALL.
C    IF THE STATION OR YEAR CHANGES, THEN A REPORT IS WRITTEN;
C    OTHERWISE THE MOST 'RECENT' STATION/DATE INFORMATION IS RETAINED
C
       IF( LOC2.NE.LOC1 .OR. IWORK1(N1).NE.IYR ) THEN
        KOUNT = KOUNT + 1
         WRITE(ST1,98,ERR=900,IOSTAT=IOST)LOC2,(IWORK1(NN),NN=N1,N1+4),
     *        (IWORK1(NN),NN=N1+11,N1+14)
        LOC2 = LOC1
        IWORK1(N1)   = IYR
        IWORK1(N1+1) = IMO
        IWORK1(N1+2) = IDY
        IWORK1(N1+3) = IHR
        IWORK1(N1+4) = JULIAN(IYR,IMO,IDY)
        IWORK1(N1+11) = 0
        IWORK1(N1+12) = 0
        IWORK1(N1+13) = 0
        IWORK1(N1+14) = 0
        FLAG = .TRUE.
       ELSE
        IWORK1(N1+11) = IMO
        IWORK1(N1+12) = IDY
        IWORK1(N1+13) = IHR
        IWORK1(N1+14) = JULIAN(IYR,IMO,IDY)
        FLAG = .FALSE.
       ENDIF
      END IF
      RETURN
C
C
  900 CONTINUE
      WRITE(ST1,99) IOST
      RETURN
C
   98 FORMAT(10X,A8,4(1X,I2),1X,I4,4X,3(1X,I2),1X,I4)
   99 FORMAT(' ERROR WRITING THE SCAN REPORT - I/O STATUS = ',I6)
C
      END

C
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C  SUBROUTINE REALCK(WHERE,NUM,KEY,MISS,LOWER,UPPER,VALUE,NAME,QAFLG)
C
C   PURPOSE:
C       ROUTINE PERFORMS RANGE CHECK ON REAL VALUED VARIABLES.
C
C   CALLED BY:     UAQASS
C                  OSQA
C
C   CALLS TO:      ERROR
C
C   VERSION DATE:  18 APRIL 1988
C-----------------------------------------------------------------------
      SUBROUTINE REALCK(WHERE,NUM,KEY,MISS,LOWER,UPPER,VALUE,NAME,QAFLG)
C
C   LOCAL VARIABLES
C
        INTEGER       WHERE,NUM,KEY,QAFLG
        REAL          MISS,LOWER,UPPER,VALUE
        CHARACTER     NAME*4
        CHARACTER     E1*1
C
        INCLUDE 'MAIN1.INC'
        INCLUDE 'MAIN2.INC'
        INCLUDE 'WORK1.INC'
C-----------------------------------------------------------------------
C        WHERE      PATHWAY NUMBER
C        NUM        PASSED NUMBER TO ROUTINE, MAY OR MAY NOT BE OF USE
C                   ORIGINALLY WAS TO HELP IDENTIFY RECORD HAVING PROBS.
C        KEY        TYPE OF RANGE CHECK 1 EXCLUDE BOUNDS
C                                       2 INCLUDE BOUNDS
C        LOWER      REAL VALUED LOWER BOUND
C        UPPER      REAL VALUED UPPER BOUND
C        MISS       REAL VALUED MISSING VALUE KEY
C        VALUE      VALUE BEING TESTED
C        NAME       NAME OF VALUE BEING RANGE CHECKED
C        QAFLG      AS AN INPUT ARGUMENT, DEFINES THE TYPE OF MESSAGE
C                       5 = 'Q'
C                       6 = 'I'
C                   AS AN OUTPUT ARGUMENT, DEFINES THE STATUS OF QA
C                   RETURNED TO CALLING PROGRAM
C                       0 = QA PASSED
C                       1 = MISSING DATA
C                       2 = LOWER BOUND VIOLATION
C                       3 = UPPER BOUND VIOLATION
C
C-----------------------------------------------------------------------
C   INITIALIZE VALUES
C
        PATH  = PATHWD(WHERE)
        LOC   = 'REALCK'
C
        IF(QAFLG .EQ. 5) THEN
         E1 = 'Q'
        ELSE IF(QAFLG .EQ.6) THEN
         E1 = 'I'
        ENDIF
        QAFLG = 0
C
        IF( VALUE.EQ.MISS ) THEN
C ***    VALUE TO QA IS MISSING
         QAFLG = 1
         RETURN
        END IF
C
        IF( KEY.LE.1 ) THEN
C ***    VALUES AT THE BOUNDARIES ARE CONSIDERED VIOLATIONS
C
         IF( VALUE.GE.UPPER ) THEN
          QAFLG = 3
          MESS = BLNK40
          WRITE( MESS,1000 ) UPPER,NAME,VALUE
1000      FORMAT(1X,'UB: ',F10.2,1X,A4,': ',F10.2)
          BUF02 = '  '
          WRITE(BUF02,500) WHERE*10 + 15 + QAFLG
          ECODE = E1//BUF02
          CALL ERROR( NUM,PATH,ECODE,LOC,MESS )
          RETURN
         END IF
C
         IF( VALUE.LE.LOWER ) THEN
          QAFLG = 2
          MESS = BLNK40
          WRITE( MESS,2000 ) LOWER,NAME,VALUE
2000      FORMAT(1X,'LB: ',F10.2,1X,A4,': ',F10.2)
          BUF02 = '  '
          WRITE(BUF02,500) WHERE*10 + 15 + QAFLG
          ECODE = E1//BUF02
          CALL ERROR( NUM,PATH,ECODE,LOC,MESS )
          RETURN
         END IF
C
        ELSE
C ***    VALUES AT THE BOUNDARIES ARE NOT CONSIDERED VIOLATIONS
C
         IF( VALUE.GT.UPPER ) THEN
          QAFLG = 3
          MESS = BLNK40
          WRITE( MESS,1000 ) UPPER,NAME,VALUE
          BUF02 = '  '
          WRITE(BUF02,500) WHERE*10 + 15 + QAFLG
          ECODE = E1//BUF02
          CALL ERROR( NUM,PATH,ECODE,LOC,MESS )
          RETURN
         END IF
C
         IF( VALUE.LT.LOWER ) THEN
          QAFLG = 2
          MESS = BLNK40
          WRITE( MESS,2000 ) LOWER,NAME,VALUE
          BUF02 = '  '
          WRITE(BUF02,500) WHERE*10 + 15 + QAFLG
          ECODE = E1//BUF02
          CALL ERROR( NUM,PATH,ECODE,LOC,MESS )
          RETURN
         END IF
C
        END IF
C
  500   FORMAT(I2)
        END
C
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C  SUBROUTINE INTECK(WHERE,NUM,KEY,MISS,LOWER,UPPER,VALUE,NAME,QAFLG)
C
C   PURPOSE:
C      ROUTINE PERFORMS RANGE CHECK ON INTEGER VALUED VARIABLES.
C
C   CALLED BY:     UAQASS
C                  SFQASS
C                  OSQA
C
C   CALLS TO:      ERROR
C
C   VERSION DATE:  18 APRIL 1988
C
C-----------------------------------------------------------------------
      SUBROUTINE INTECK(WHERE,NUM,KEY,MISS,LOWER,UPPER,VALUE,NAME,QAFLG)
C
C   LOCAL VARIABLES
C
        INTEGER       WHERE,NUM,KEY,MISS,LOWER,UPPER,VALUE,QAFLG
        CHARACTER     NAME*4
        CHARACTER     E1*1
C
        INCLUDE 'MAIN1.INC'
        INCLUDE 'MAIN2.INC'
        INCLUDE 'WORK1.INC'
C-----------------------------------------------------------------------
C        WHERE      PATHWAY NUMBER
C        NUM        PASSED NUMBER TO ROUTINE, MAY OR MAY NOT BE OF USE
C                   ORIGINALLY WAS TO HELP IDENTIFY RECORD HAVING PROBS.
C        KEY        TYPE OF RANGE CHECK 1 EXCLUDE BOUNDS
C                                       2 INCLUDE BOUNDS
C        LOWER      INTEGER VALUED LOWER BOUND
C        UPPER      INTEGER VALUED UPPER BOUND
C        MISS       INTEGER VALUED MISSING VALUE KEY
C        VALUE      INTEGER VALUE BEING TESTED
C        NAME       NAME OF VALUE BEING RANGE CHECKED
C        QAFLG      AS AN INPUT ARGUMENT, DEFINES THE TYPE OF MESSAGE
C                       5 = 'Q'
C                       6 = 'I'
C                   AS AN OUTPUT ARGUMENT, DEFINES THE STATUS OF QA
C                   RETURNED TO CALLING PROGRAM
C                       0 = QA PASSED
C                       1 = MISSING DATA
C                       2 = LOWER BOUND VIOLATION
C                       3 = UPPER BOUND VIOLATION
C
C-----------------------------------------------------------------------
C   INITIALIZE VALUES
        PATH  = PATHWD(WHERE)
        LOC   = 'INTECK'
C
        IF(QAFLG .EQ. 5) THEN
         E1 = 'Q'
        ELSE IF(QAFLG .EQ.6) THEN
         E1 = 'I'
        ENDIF
        QAFLG = 0
C
        IF( VALUE.EQ.MISS ) THEN
C ***    VALUE TO QA IS MISSING
         QAFLG = 1
         RETURN
        END IF
C
        IF( KEY.LE.1 ) THEN
C ***    VALUES AT THE BOUNDARIES ARE CONSIDERED VIOLATIONS
C
         IF( VALUE.GE.UPPER ) THEN
          QAFLG = 3
          MESS = BLNK40
          WRITE( MESS,1000 ) UPPER,NAME,VALUE
1000      FORMAT(1X,'UB: ',I10,1X,A4,': ',I10)
          BUF02 = '  '
          WRITE(BUF02,500) WHERE*10 + 15 + QAFLG
          ECODE = E1//BUF02
          CALL ERROR( NUM,PATH,ECODE,LOC,MESS )
          RETURN
         END IF
C
         IF( VALUE.LE.LOWER ) THEN
          QAFLG = 2
          MESS = BLNK40
          WRITE( MESS,2000 ) LOWER,NAME,VALUE
2000      FORMAT(1X,'LB: ',I10,1X,A4,': ',I10)
          BUF02 = '  '
          WRITE(BUF02,500) WHERE*10 + 15 + QAFLG
          ECODE = E1//BUF02
          CALL ERROR( NUM,PATH,ECODE,LOC,MESS )
          RETURN
         END IF
C
        ELSE
C ***    VALUES AT THE BOUNDARIES ARE NOT CONSIDERED VIOLATIONS
C
         IF( VALUE.GT.UPPER ) THEN
          QAFLG = 3
          MESS = BLNK40
          WRITE( MESS,1000 ) UPPER,NAME,VALUE
          BUF02 = '  '
          WRITE(BUF02,500) WHERE*10 + 15 + QAFLG
          ECODE = E1//BUF02
          CALL ERROR( NUM,PATH,ECODE,LOC,MESS )
          RETURN
         END IF
C
         IF( VALUE.LT.LOWER ) THEN
          QAFLG = 2
          MESS = BLNK40
          WRITE( MESS,2000 ) LOWER,NAME,VALUE
          BUF02 = '  '
          WRITE(BUF02,500) WHERE*10 + 15 + QAFLG
          ECODE = E1//BUF02
          CALL ERROR( NUM,PATH,ECODE,LOC,MESS )
          RETURN
         END IF
C
        END IF
C
  500   FORMAT(I2)
        END
C
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C   SUBROUTINE LATLON( KOUNT,KEY,CDATA,RDATA,ISTAT )
C
C   PURPOSE:
C      ROUTINE CONVERTS A*8 CHARACTER VALUE (CDATA) TO ITS REAL-VALUE
C      EQUIVALENT (RDATA).
C
C        KEY =   1 INDICATES WE ARE WORKING ON THE LATITUDE.
C                2 INDICATES WE ARE WORKING ON THE LONGITUDE.
C        ISTAT = 1 INDICATES ERRORS,
C                2 CONVERSION APPEARS OK.
C
C   CALLED BY:
C
C   CALLS TO:      ERROR
C
C   VERSION DATE:  14 JULY 1988
C
C=======================================================================
        SUBROUTINE LATLON( KOUNT,KEY,CDATA,RDATA,ISTAT )
C
C        LOCAL VARIABLES
C
        INTEGER   KEY,ISTAT
        REAL      RDATA
        CHARACTER CDATA*8
C
        INCLUDE 'MAIN1.INC'
        INCLUDE 'MAIN2.INC'
        INCLUDE 'WORK1.INC'
C
C-----------------------------------------------------------------------
C    INITIALIZE VALUES
C
        PATH = PATHWD(IRD1)
        LOC  = 'LATLON'
        ISTAT = 0
C
C        1.  TEST KEY
C
        IF( KEY.LT.1 .OR. KEY.GT.2 ) THEN
        ECODE = 'E06'
        MESS = BLNK40
        WRITE( MESS,900 ) KEY
 900    FORMAT(1X,'ERROR ARGUMENT  KEY, IT IS: ',I3)
        CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
        ISTAT = 1
        RETURN
        END IF
C
        IF( KEY.EQ.1 ) THEN
C
C        LATITUDE ANALYSIS
C
        IF( CDATA(8:8).EQ.'N') THEN
        IWORK1(1) = 8
        IWORK1(2) = 0
        IWORK1(9) = +1
        ELSE IF( CDATA(8:8).EQ.'S') THEN
        IWORK1(1) = 0
        IWORK1(2) = 8
        IWORK1(9) = -1
        ELSE
        IWORK1(1) = 0
        IWORK1(2) = 0
        IWORK1(9) = 0
        END IF
C
        IF( IWORK1(1).EQ.0 .AND. IWORK1(2).EQ.0 ) THEN
C
C        ERROR CONDITION
C
        ECODE = 'E06'
        MESS = BLNK40
        WRITE( MESS,1000 ) CDATA
1000    FORMAT(1X,'NO N OR S IN LATITUDE VALUE: ',A8)
        CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
        ISTAT = 1
        END IF
C
C        LONGITUDE HAS BEEN FOUND.
C
C        2.  READ FOR THE NUMERIC VALUE
C
        BUF08(1) = BLNK08
        BUF08(1)(2:8) = CDATA( 1:7 )
        READ( BUF08(1),2000,IOSTAT=IRD4 ) XRD1
2000    FORMAT( F8.0 )
C
C        CHECK READ STATUS
C
        IF( IRD4.NE.0 ) THEN
        ECODE = 'E03'
        MESS = BLNK40
        WRITE( MESS,3000 )
3000    FORMAT(1X,'IOSTAT= ',I6,' READING LATITUDE VALUE')
        CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
        ISTAT = 1
        END IF
C
         RDATA = IWORK1(9)*XRD1
C
C         3.  CHECK THAT VALUE IS REASONABLE
C
        IF( RDATA.LT.-90.0 .OR. RDATA.GT.90.0 ) THEN
        ECODE = 'E06'
        MESS = BLNK40
        WRITE( MESS,4000 ) CDATA
4000    FORMAT(1X,'LATITUDE OUT OF RANGE: ',A8)
        CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
        ISTAT = 1
        END IF
C
        IF( ISTAT.EQ.1 ) THEN
        RETURN
        ELSE
        ISTAT = 2
        RETURN
        END IF
C
        END IF
C
C        4.  LONGITUDE ANALYSIS
C
        IF( CDATA(8:8).EQ.'W') THEN
        IWORK1(1) = 8
        IWORK1(2) = 0
        IWORK1(9) = +1
        ELSE IF( CDATA(8:8).EQ.'E') THEN
        IWORK1(1) = 0
        IWORK1(2) = 8
        IWORK1(9) = -1
        ELSE
        IWORK1(1) = 0
        IWORK1(2) = 0
        IWORK1(9) = 0
        END IF
C
        IF( IWORK1(1).EQ.0 .AND. IWORK1(2).EQ.0 ) THEN
C
C        ERROR CONDITION
C
        ECODE = 'E06'
        MESS = BLNK40
        WRITE( MESS,5000 ) CDATA
5000    FORMAT(1X,'NO W OR E IN LONGITUDE VALUE: ',A8)
        CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
        ISTAT = 1
        END IF
C
C        5.  READ FOR THE NUMERIC VALUE
C
        BUF08(1) = BLNK08
        BUF08(1)(2:8) = CDATA( 1:7 )
        READ( BUF08(1),2000,IOSTAT=IRD4 ) XRD1
C
C        CHECK READ STATUS
C
        IF( IRD4.NE.0 ) THEN
        ECODE = 'E03'
        MESS = BLNK40
        WRITE( MESS,6000 )
6000    FORMAT(1X,'IOSTAT= ',I6,' READING LONGITUDE VALUE')
        CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
        ISTAT = 1
        END IF
C
         RDATA = IWORK1(9)*XRD1
C
C         6.  CHECK THAT VALUE IS REASONABLE
C
        IF( RDATA.LT.-180.0 .OR. RDATA.GT.180.0 ) THEN
        ECODE = 'E06'
        MESS = BLNK40
        WRITE( MESS,7000 ) CDATA
7000    FORMAT(1X,'LONGITUDE OUT OF RANGE: ',A8)
        CALL ERROR( KOUNT,PATH,ECODE,LOC,MESS )
        ISTAT = 1
        END IF
C
        IF( ISTAT.EQ.1 ) THEN
        CONTINUE
        ELSE
        ISTAT = 2
        END IF
C
        RETURN
        END
C
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C   SUBROUTINE OSFILL( IPATH,NUMBER,DEVNUM,RCSTAT )
C
C   PURPOSE:
C      THIS ROUTINE FETCHES FROM THE SPECIFIED DEVICE (DEVNUM) ONE
C      OS OBSERVATION.  THE OS DATA ARE STORED ('BUFFERED') INTO THREE
C      FILES WITHIN THE WORK1 COMMON BLOCK.  THE SCALAR DATA VALUES
C      ARE STORED WITHIN IWORK1(1400+) AND WORK1(1400+) AND THE
C      VECTOR VALUES ARE STORED WITHIN WORK2(100++,+).  NOTE, THE DATE
C      AND TIME DATA, WHICH ARE INTEGER VARIABLES ARE STORED IN
C      IWORK1(1400+).  THE + IN THE ABOVE DESCRIPTION IS THE
C      VARIABLE'S INDEX WITHIN THE VNAME ARRAY, AND ++ IS THE
C      TOWER LEVEL INDEX.
C
C   CALLED BY:
C
C   CALLS TO:      ERROR
C
C   VERSION DATE:  14 JULY 1988
C
C=======================================================================
      SUBROUTINE OSFILL( IPATH,NUMBER,DEVNUM,RCSTAT )
C
C    LOCAL VARIABLES
C
      INTEGER  IPATH,NUMBER,DEVNUM,RCSTAT,NERR
      REAL     RMISS
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'WORK1.INC'
C
C
C     IPATH   PATHWAY (LIKELY 4=OS OR 5=MR)
C     NUMBER  RECORD NUMBER OF FILE
C     DEVNUM  DEVICE NUMBER FROM WHICH WE ARE TO READ OS DATA
C     RCSTAT  RECORD STATUS   0 = INITIAL VALUE
C                             1 = FILLED BUFFER BUT HAD READ ERRORS
C                             2 = WE HAVE EXCEEDED THE ALLOWABLE
C                                  NUMBER OF READ ERRORS (MAXERR)
C                             3 = END OF FILE ENCOUNTERED
C                             4 = END OF FILE ENCOUNTERED SOONER THAN
C                                  EXPECTED.
C     NERR    NUMBER OF READ ERRORS ENCOUNTERED THUS FAR
C
C-----------------------------------------------------------------------
C         INITIALIZE VALUES
C
        PATH = PATHWD(IPATH)
        LOC  = 'OSFILL'
        RCSTAT = 0
C
C        SET ALL VALUES IN 'BUFFER' TO MISSING FLAG VALUES
C
C        FIRST THE SCALAR VARIABLES (sky cover is in a special variable)
C
        DO 10 I=1,52
        IF( I.EQ.34 ) THEN
           RMISS = FLOAT( OSTSKY(2) )
        ELSE
           RMISS = FLOAT( SFQA(I,2) )
        END IF
        WORK1(1400+I) = RMISS
10      CONTINUE
C
C        THEN THE VECTOR VARIABLES
C
        DO 30 I=15,29
           DO 20 J=1,OSNL
              WORK2(100+J,I-14) = FLOAT(SFQA(I,2))
   20      CONTINUE
   30   CONTINUE
C
C        FINALLY, THE INTEGER (date/time) VARIABLES
C
        DO 40 I=56,60
        IWORK1(1400+I) = SFQA(I,2)
40      CONTINUE
C
C        1.  ATTEMPT READ FOR DATA
C
C        1A. FIRST PROCESS RECORD
C
        IF( OSDNUM(1).EQ.OSTIME) THEN
        READ( DEVNUM,OSFRMT(1),IOSTAT=IRD5,END=300 )
     1              ( IWORK1(1400+OSDVAR(1,J,1)),J=1,OSTIME )
C
C
        ELSE
        READ( DEVNUM,OSFRMT(1),IOSTAT=IRD5,END=300 )
     1          ( IWORK1(1400+OSDVAR(1,J,1)),J=1,OSTIME ),
     2          ( WORK1(J),J=OSTIME+1,OSDNUM(1) )
C
        DO 45 J=OSTIME+1,OSDNUM(1)
C
        IF( OSDVAR(1,J,2).LE.0 ) THEN
C       SCALAR VARIABLE
        WORK1(1400+OSDVAR(1,J,1)) = WORK1(J)
        ELSE
C       VECTOR VARIABLE
        WORK2(100+OSDVAR(1,J,2),OSDVAR(1,J,1)-14) = WORK1(J)
        END IF
C
45      CONTINUE
C
        END IF
C
        NUMBER = NUMBER + 1
C
C        CHECK READ STATUS
C
        IF( IRD5.NE.0 ) THEN
C
        RCSTAT = 1
        NERR = NERR + 1
        MESS = BLNK40
        ECODE = 'W52'
        I = 1
        WRITE( MESS,1000 ) IRD5,NUMBER
1000    FORMAT(1X,'IOSTAT= ',I8,', READING RECORD ',I3)
        CALL ERROR( NUMBER,PATH,ECODE,LOC,MESS )
C
        IF( NERR.GT.MAXERR ) THEN
        GO TO 100
        END IF
C
        END IF
C
C        LOOP ON REST OF RECORDS
C
        DO 60 I=2,OSDCRD
C
        READ( DEVNUM,OSFRMT(I),IOSTAT=IRD5,END=200 )
     1              ( WORK1(J),J=1,OSDNUM(I) )
C
C        CHECK READ STATUS
C
        NUMBER = NUMBER + 1
        IF( IRD5.NE.0 ) THEN
C
        RCSTAT = 1
        NERR = NERR + 1
        MESS = BLNK40
        ECODE = 'W52'
        WRITE( MESS,1000 ) IRD5,NUMBER
        CALL ERROR( NUMBER,PATH,ECODE,LOC,MESS )
C
        IF( NERR.GT.MAXERR ) THEN
        GO TO 100
        END IF
C
        END IF
C
        DO 50 J=1,OSDNUM(I)
C
        IF( OSDVAR(I,J,2) .LE. 0 ) THEN
C       SCALAR VARIABLE
        WORK1(1400+OSDVAR(I,J,1)) = WORK1(J)
        ELSE
C       VECTOR VARIABLE
        WORK2(100+OSDVAR(I,J,2),OSDVAR(I,J,1)-14) = WORK1(J)
        END IF
C
50      CONTINUE
C
60      CONTINUE
C
C        APPARENTLY WE MADE IT!
C
        RETURN
C
100     CONTINUE
C
C        EXCEEDED ALLOWABLE LIMIT OF READ ERRORS
C
        RCSTAT = 2
        MESS = BLNK40
        ECODE = 'E52'
        WRITE( MESS,2000 )
2000    FORMAT(1X,'EXCEEDED LIMIT ON READ ERRORS')
        CALL ERROR( NUMBER,PATH,ECODE,LOC,MESS )
        RETURN
C
200     CONTINUE
C
C        FOUND EOF TOO SOON
C
        RCSTAT = 3
        MESS = BLNK40
        ECODE = 'E56'
        WRITE( MESS,3000 )
3000    FORMAT(1X,'HIT EOF TOO SOON')
        CALL ERROR( NUMBER,PATH,ECODE,LOC,MESS )
        RETURN
C
C        FOUND EOF ON DATA FILE
C
300     CONTINUE
        RCSTAT = 4
        MESS = BLNK40
        ECODE = 'I59'
        WRITE( MESS,4000 )
4000    FORMAT(1X,'FOUND EOF FILE ')
        CALL ERROR( NUMBER,PATH,ECODE,LOC,MESS )
        RETURN
        END
C
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C   SUBROUTINE OSREAD( PATHID,NUMBER,HOUR,TEST )
C
C   PURPOSE:
C      ROUTINE READS OS DATA STORED IN MERGE OUTPUT FILE.
C
C   CALLED BY:
C
C   CALLS TO:      ERROR
C
C   VERSION DATE:  14 JULY 1988
C
C=======================================================================
        SUBROUTINE OSREAD( PATHID,NUMBER,HOUR,TEST )
C
C     LOCAL VARIABLES
C
        INTEGER  PATHID,NUMBER,IFLAG,HOUR,TEST
        REAL     RMISS
        INCLUDE 'MAIN1.INC'
        INCLUDE 'MAIN2.INC'
        INCLUDE 'SF1.INC'
        INCLUDE 'SF2.INC'
        INCLUDE 'OS1.INC'
        INCLUDE 'OS2.INC'
        INCLUDE 'WORK1.INC'
C
        DATA IFLAG/0/
C
C    PATHID  PATHWAY NUMBER IN METPROCESSOR SYSTEM
C             5 = MR
C             6 = MP
C    NUMBER  COUNTER FOR OBSERVATION BEING PROCESSED TO
C             OUTPUT FILE
C    IFLAG   INITIALLY = 0, IF ERRORS OCCUR IN READING THE MERGE
C             FILE, RESET TO -1.  IF IFLAG = -1, NO MORE ATTEMPTS ARE
C             MADE TO READ MERGE FILE.
C    HOUR    POSITION WITHIN OS-SCALAR AND OS-VECTOR MASTER DATA
C             ARRAYS THAT DATA BELONG.
C    TEST    STATUS OF PROCESS
C             1 = ERROR HAS OCCURRED
C             2 = ALL OK
C             3 = EOF FOUND
C
C-----------------------------------------------------------------------
C   INITIALIZE VALUES
C
        PATH = PATHWD(PATHID)
        LOC  = 'OSREAD'
        TEST = 0
C
C        CHECK IFLAG
C
        IF( IFLAG.EQ.-1 ) THEN
        RETURN
        END IF
C
C        SET ALL VALUES IN 'BUFFER' TO MISSING FLAG VALUES
C
C        FIRST THE SCALAR VARIABLES
C
        DO 10 I=1,52
        IF( I.EQ.34 ) THEN
           RMISS = FLOAT( OSTSKY(2) )
        ELSE
           RMISS = FLOAT( SFQA(I,2) )
        END IF
        WORK1(1400+I) = RMISS
10      CONTINUE
C
C        THEN THE VECTOR VARIABLES
C
        DO 30 I=15,29
C
        DO 20 J=1,OSNL
C
        WORK2(100+J,I-14) = FLOAT(SFQA(I,2))
20      CONTINUE
30      CONTINUE
C
C        FINALLY, THE INTEGER (date/time) VARIABLES
C
        DO 40 I=56,60
            IWORK1(1400+I) = SFQA(I,2)
40      CONTINUE
C
C        1.  READ FIRST RECORD
C
        IF( OSDNUM(1).EQ.OSTIME) THEN
        READ( DEV40,IOSTAT=IRD5,END=100 )
     1              ( IWORK1(1400+OSDVAR(1,J,1)),J=1,OSTIME )
        ELSE
        READ( DEV40,IOSTAT=IRD5,END=100 )
     1          ( IWORK1(1400+OSDVAR(1,J,1)),J=1,OSTIME ),
     2          ( WORK1(J),J=OSTIME+1,OSDNUM(1) )
C
        END IF
C
C        CHECK READ STATUS
C
        IF( IRD5.NE.0 ) THEN
C
        MESS = BLNK40
        ECODE = ' 3P'
        WRITE( MESS,1000 ) IRD5
1000    FORMAT(1X,'IOSTAT=',I8,', READING MR DATA RECORD')
        CALL ERROR( NUMBER,PATH,ECODE,LOC,MESS )
        TEST = 1
C
C        RESET IFLAG
C
        IFLAG = -1
        RETURN
C
        ELSE IF( OSDNUM(1) .GT. OSTIME ) THEN
C
        DO 50 J=OSTIME+1,OSDNUM(1)
C
        IF( OSDVAR(1,J,2) .LE. 0 ) THEN
C       SCALAR VARIABLE
        WORK1(1400+OSDVAR(1,J,1)) = WORK1(J)
        ELSE
C       VECTOR VARIABLE
        WORK2(100+OSDVAR(1,J,2),OSDVAR(1,J,1)-14) = WORK1(J)
        END IF
C
50      CONTINUE
C
        END IF
C
C       DEFINE HOUR
C
        HOUR = IWORK1(1400+59)
C
C       TEST HOUR
C
        IF( HOUR.EQ.SFQA(59,2) ) THEN
        MESS = BLNK40
        ECODE = 'E71'
        WRITE( MESS,2000 )
2000    FORMAT(1X,'HOUR IS DEFINED AS MISSING')
        CALL ERROR( NUMBER,PATH,ECODE,LOC,MESS )
        TEST = 1
        IFLAG = -1
        RETURN
        END IF
C
        IF( HOUR.LT.1 .OR. HOUR.GT.24 ) THEN
        MESS = BLNK40
        ECODE = 'E71'
        WRITE( MESS,2500 )
2500    FORMAT(1X,'OS DATA NOT ON 1 - 24 HOUR CLOCK')
        CALL ERROR( NUMBER,PATH,ECODE,LOC,MESS )
        TEST = 1
        IFLAG = -1
        RETURN
        END IF
C
        TEST = 2
C
C        LOOP ON REST RECORDS
C
        DO 70 I=2,OSDCRD
C
        READ( DEV40,IOSTAT=IRD5,END=100 )
     1              ( WORK1(J),J=1,OSDNUM(I) )
C
C        CHECK READ STATUS
C
        IF( IRD5.NE.0 ) THEN
C
        MESS = BLNK40
        ECODE = ' 3P'
        WRITE( MESS,1000 ) IRD5
        CALL ERROR( NUMBER,PATH,ECODE,LOC,MESS )
        TEST = 1
C
        IFLAG = -1
        RETURN
        ELSE
        DO 60 J=1,OSDNUM(I)
C
        IF( OSDVAR(I,J,2) .LE. 0 ) THEN
C       SCALAR VARIABLE
        WORK1(1400+OSDVAR(I,J,1)) = WORK1(J)
        ELSE
C       VECTOR VARIABLE
        WORK2(100+OSDVAR(I,J,2),OSDVAR(I,J,1) - 14) = WORK1(J)
        END IF
C
60      CONTINUE
C
        END IF
C
70      CONTINUE
C
        RETURN
100     TEST = 3
        RETURN
        END
C
C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C   SUBROUTINE OSPRNT( NUMBER )
C
C   PURPOSE:
C       ROUTINE PRINTS DATA STORED IN 1ST HOUR POSITION OF OSSOBS
C       AND OSVOBS TO REPORT FILE.
C
C   CALLED BY:
C
C   CALLS TO:      ERROR
C
C   VERSION DATE:  14 JULY1988
C
C=======================================================================
        SUBROUTINE OSPRNT( NUMBER )
C
C     LOCAL VARIABLES
C
        INTEGER NUMBER,IFLAG
C
        INCLUDE 'MAIN1.INC'
        INCLUDE 'MAIN2.INC'
        INCLUDE 'SF1.INC'
        INCLUDE 'SF2.INC'
        INCLUDE 'OS1.INC'
        INCLUDE 'OS2.INC'
        INCLUDE 'WORK1.INC'
C
        DATA IFLAG/0/
C
C   NUMBER  COUNTER FOR OBSERVATION BEING PROCESSED TO
C            OUTPUT PRINT FILE
C   IFLAG   INITIALLY = 0, IF ERRORS OCCUR IN WRITING TO OUTPUT
C            FILE, RESET TO -1.  IF IFLAG = -1, NO MORE ATTEMPTS
C            ARE MADE TO WRITE TO OUTPUT FILE.
C
C-----------------------------------------------------------------------
C        INITIALIZE VALUES
C
        PATH = PATHWD(4)
        LOC  = 'OSWRTE'
C
C        CHECK IFLAG
C
        IF( IFLAG.EQ.-1 ) THEN
        RETURN
        END IF
C
C        2.  WRITE FIRST RECORD
C
        IF( OSDNUM(1).EQ.OSTIME) THEN
        WRITE( IRD4,OSFRMT(1),IOSTAT=IRD5 )
     1              ( IWORK1(1400+OSDVAR(1,J,1)),J=1,OSTIME )
        ELSE
C
        DO 10 J=OSTIME+1,OSDNUM(1)
C
        IF( OSDVAR(1,J,2) .LE. 0 ) THEN
C       SCALAR VARIABLE
        WORK1(J) = WORK1(1400+OSDVAR(1,J,1))
        ELSE
C       VECTOR VARIABLE
        WORK1(J) = WORK2(100+OSDVAR(1,J,2),OSDVAR(1,J,1)-14)
        END IF
C
10      CONTINUE
C
        WRITE( IRD4,OSFRMT(1),IOSTAT=IRD5 )
     1          ( IWORK1(1400+OSDVAR(1,J,1)),J=1,OSTIME ),
     2          ( WORK1(J),J=OSTIME+1,OSDNUM(1) )
        END IF
C
C        CHECK WRITE STATUS
C
        IF( IRD5.NE.0 ) THEN
C
        MESS = BLNK40
        ECODE = ' 3P'
        WRITE( MESS,1000 ) IRD5
1000    FORMAT(1X,'IOSTAT=',I8,', WRITING OS DATA RECORD')
        CALL ERROR( NUMBER,PATH,ECODE,LOC,MESS )
C
C        RESET IFLAG
C
        IFLAG = -1
        RETURN
C
        END IF
C
C        LOOP ON REST OF RECORDS
C
        DO 50 I=2,OSDCRD
C
        DO 20 J=1,OSDNUM(I)
C
        IF( OSDVAR(I,J,2) .LE. 0 ) THEN
C       SCALAR VARIABLE
        WORK1(J) = WORK1(1400+OSDVAR(I,J,1))
        ELSE
C       VECTOR VARIABLE
        WORK1(J) = WORK2(100+OSDVAR(I,J,2),OSDVAR(I,J,1)-14)
        END IF
C
20      CONTINUE
C
        WRITE( IRD4,OSFRMT(I),IOSTAT=IRD5 )
     1              ( WORK1(J),J=1,OSDNUM(I) )
C
C        CHECK WRITE STATUS
C
        IF( IRD5.NE.0 ) THEN
C
        MESS = BLNK40
        ECODE = ' 3P'
        WRITE( MESS,1000 ) IRD5
        CALL ERROR( NUMBER,PATH,ECODE,LOC,MESS )
C
        IFLAG = -1
        RETURN
        END IF
C
50      CONTINUE
C
C        APPARENTLY WE MADE IT!
C
        RETURN
        END
C
        SUBROUTINE OSSWAP( HOUR )
C---------------------------------------------------------------------**
C   SUBROUTINE OSSWAP(HOUR)
C
C   Purpose:
C      Transfer buffered OS data (from call to OSFILL or OSREAD) to
C      OS-scalar and OS-vector master data arrays.
C      Data are swapped into first hour of these arrays.
C
C   Called by:     OSFILL, OSREAD
C
C   Calls to:      -NONE-
C
C   Version date:  14 July 1988
C
C   Modified:      10 February 1995  (J. Paumier, PES)
C                  To account for the addition of 4 variables to the
C                  hourly surface observations
C-----------------------------------------------------------------------
C     Data declarations

      INTEGER  I,J,HOUR

      INCLUDE 'SF1.INC'
      INCLUDE 'SF2.INC'
      INCLUDE 'OS1.INC'
      INCLUDE 'OS2.INC'
      INCLUDE 'WORK1.INC'

C     HOUR    position in the array to occupy

C     Swap buffer in for processing

      OSDAY(HOUR) = IWORK1(1400+56)
      OSMO(HOUR)  = IWORK1(1400+57)
      OSYR(HOUR)  = IWORK1(1400+58)
      OSHR(HOUR)  = IWORK1(1400+59)
      OSMN(HOUR)  = IWORK1(1400+60)

      DO 50 I=1,14
         OSSOBS(HOUR,I) = WORK1(1400+I)
   50 CONTINUE

C     Subtract 15 to account for the 15 onsite array variables
C     between the onsite scalars (1-14) and surface obs (30-52)

      DO 60 I=30,52
         OSSOBS(HOUR,I-15) = WORK1(1400+I)
   60 CONTINUE

      DO 80 I=15,29
         DO 70 J=1,OSNL
            OSVOBS(HOUR,J,I-14) = WORK2(100+J,I-14)
   70    CONTINUE
   80 CONTINUE

      RETURN
      END



C=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=
C   SUBROUTINE SUMRY2 (STAGE)
C
C   PURPOSE
C      THIS ROUTINE CREATES A SUMMARY TABLE OF ALL INFORMATIONAL,
C      WARNING, ERROR AND QA MESSAGES GENERATED BY THE MPRM
C      PROCESSOR.  IN ADDITION IT REITERATES THE WARNING AND
C      ERROR MESSAGES.
C
C   CALLED BY:     FINISH, MAIN PROGRAM OF STAGE 3
C
C   CALLS TO:      BANNER
C
C   VERSION DATE:  1 JULY 1988

C   MODIFIED:      24 JANUARY 1996  (D. BAILEY)
C                  REMOVED PRINTOUT OF BANNER ABOVE SUMMARY TABLE
C=======================================================================
      SUBROUTINE SUMRY2( STAGE )
C
      LOGICAL       LVAR
      INTEGER       SUMTBL(6,0:4,10), SEVER,IOST60,IP,ICODE1,ICODE2,
     &              NCNT, PSTAT(6),ITOT1(8),ITOT2(6,0:4),TOTAL,SUM,
     &              STAGE
      CHARACTER*1   ETYPE(0:4)
      CHARACTER*48  CVAR1
C
      INCLUDE 'MAIN1.INC'
      INCLUDE 'MAIN2.INC'
      INCLUDE 'WORK1.INC'
C
C
      DATA SUMTBL/300*0/, NCNT/0/, ITOT1/8*0/, ITOT2/30*0/, TOTAL/0/
      DATA ETYPE /'T','Q','I','W','E'/
C
C     SUMTBL        TABLE OF MESSAGE COUNTS
C     SEVER         MESSAGE TYPE (0:4) CORRESPONDING TO ETYPE
C     PSTAT         PATHWAY STATUSES
C     ITOT1,ITOT2   COLUMN AND ROW TOTALS
C     TOTAL         TOTAL NUMBER OF MESSAGES
C     STAGE         PROCESSING STAGE - SUBROUTINE CALLING ARGUMENT
C                   = 1 FOR STAGES 1 AND 2; = 3 FOR STAGE 3
C     ETYPE         MESSAGE CATEGORIES
C
C-----------------------------------------------------------------------
C ***   DETERMINE WHETHER TO WRITE TO A REPORT FILE OR DEFAULT OUTPUT.
C       DETERMINE IF AN ERROR/MESSAGE FILE AND TEMPORARY FILE ARE OPEN.
C       PLACE THE PATHWAY STATUSES IN AN ARRAY. PSTAT(6) IS A DUMMY
C       STATUS FOR STAGE 3 PROCESSING
C
      IF( STATUS(1,3) .EQ. 2 ) THEN
       IRD5 = DEV50
      ELSE
       IRD5 = DEVIO
      END IF
C
      IF( STATUS(1,5) .EQ.2 ) THEN
       IRD3 = DEV60
      ELSE
       IRD3 = DEVIO
      END IF
C
      INQUIRE (UNIT=DEV70,OPENED=LVAR,NAME=CVAR1)
C
      PSTAT(1) = JBSTAT
      PSTAT(2) = UASTAT
      PSTAT(3) = SFSTAT
      PSTAT(4) = OSSTAT
      PSTAT(5) = MRSTAT
      PSTAT(6) = 0
C
C *** IF THIS IS STAGE 1 PROCESSING, WRITE ANY SCAN REPORTS TO THE
C      MESSAGE FILE BEFORE PROCEEDING
C
       IF(STAGE .EQ. 1 .AND. LVAR) THEN
        REWIND DEV70
  101   BUF80(1) = BLNK80
        READ(DEV70,6000,END=200,ERR=120,IOSTAT=IOST70) BUF80(1)
        IF( INDEX(BUF80(1),'$UASCAN$') .NE. 0) THEN
         WRITE(IRD3,6002)
C
C ***    SCAN INFORMATION FOR SOUNDINGS TO FOLLOW
  110    BUF80(1) = BLNK80
         READ(DEV70,6000,END=130,ERR=120,IOSTAT=IOST70) BUF80(1)
         IF( INDEX(BUF80(1),'$UASCAN$') .NE. 0) THEN
C
C ***     SCAN INFORMATION COMPLETED
          GO TO 200
         ELSE
C
C ***     WRITE SCAN INFORMATION
          WRITE(IRD3,6000) BUF80(1)
          GO TO 110
         ENDIF
        ELSE
         GO TO 101
        ENDIF
C
  120   WRITE(IRD3,6050) IOST70
        GO TO 200
  130   WRITE(IRD3,6060)
C
C *** MIXING HEIGHTS
  200   REWIND DEV70
  201   BUF80(1) = BLNK80
        READ(DEV70,6000,END=300,ERR=220,IOSTAT=IOST70) BUF80(1)
        IF( INDEX(BUF80(1),'$ZISCAN$') .NE. 0) THEN
         WRITE(IRD3,6002)
C
C ***    SCAN INFORMATION FOR MIXING HEIGHTS TO FOLLOW
  210    BUF80(1) = BLNK80
         READ(DEV70,6000,END=230,ERR=220,IOSTAT=IOST70) BUF80(1)
         IF( INDEX(BUF80(1),'$ZISCAN$') .NE. 0) THEN
C
C ***     SCAN INFORMATION COMPLETED
          GO TO 300
         ELSE
C
C ***     WRITE SCAN INFORMATION
          WRITE(IRD3,6000) BUF80(1)
          GO TO 210
         ENDIF
        ELSE
         GO TO 201
        ENDIF
C
  220   WRITE(IRD3,6050) IOST70
        GO TO 300
  230   WRITE(IRD3,6060)
C
C *** SURFACE OBSERVATIONS
  300   REWIND DEV70
  301   BUF80(1) = BLNK80
        READ(DEV70,6000,END=400,ERR=320,IOSTAT=IOST70) BUF80(1)
        IF( INDEX(BUF80(1),'$SFSCAN$') .NE. 0) THEN
         WRITE(IRD3,6002)
C
C ***    SCAN INFORMATION FOR SURFACE OBS TO FOLLOW
  310    BUF80(1) = BLNK80
         READ(DEV70,6000,END=330,ERR=320,IOSTAT=IOST70) BUF80(1)
         IF( INDEX(BUF80(1),'$SFSCAN$') .NE. 0) THEN
C
C ***     SCAN INFORMATION COMPLETED
          GO TO 400
         ELSE
C
C ***     WRITE SCAN INFORMATION
          WRITE(IRD3,6000) BUF80(1)
          GO TO 310
         ENDIF
        ELSE
         GO TO 301
        ENDIF
C
  320   WRITE(IRD3,6050) IOST70
        GO TO 400
  330   WRITE(IRD3,6060)
       ENDIF
C
C------------------------------------------------------------------
C *** BEGIN WRITING THE TABLE WITH THE BANNER AND DATE/TIME

C  400 CALL BANNER(IRD5)                                                DTB96024
  400 CONTINUE                                                          DTB96024

C      IF( STAGE .EQ. 1 )THEN
C       WRITE( IRD5,5001 )
C      ELSE IF( STAGE .EQ. 3 ) THEN
C       WRITE( IRD5,5002 )
C      END IF

C     *** INDICATE THE TERMINATION STATUS OF THE JOB RUN
C
C      IF( STATUS(1,4) .EQ. 0 ) THEN                                    DTB96024
C       IF(JBSTAT.LT.0 .OR. UASTAT.LT.0 .OR. SFSTAT.LT.0                DTB96024
C     &     .OR. OSSTAT.LT.0) THEN                                      DTB96024
C       WRITE(IRD5,5110)                                                DTB96024
C       ELSE                                                            DTB96024
C       WRITE(IRD5,5120)                                                DTB96024
C       END IF                                                          DTB96024
C
      IF(STATUS(1,4) .GT. 0) THEN                                       DTB96024
      WRITE(IRD5,5100)
      END IF
C
      WRITE(IRD5,5005)
C
C--------------------------------------------------------------------
C *** CHECK FOR THE EXISTANCE OF AN ERROR/MESSAGE FILE (DEV60)
C     AND TEMPORARY FILE - SKIP THIS LOGIC IF EITHER IS MISSING;
C     REWIND THE MESSAGE FILE AND BACKSPACE ON THE TEMPORARY FILE;
C
      IF( (STATUS(1,5).EQ.2) .AND. LVAR ) THEN
       REWIND DEV60
       IF(BACK40) BACKSPACE DEV70
C
   10  BUF80(1) = BLNK80
       NCNT = NCNT + 1
       READ(IRD3,6000,END=6100,ERR=6200,IOSTAT=IOST60) BUF80(1)
C
       DO 20 IP = 1,6
        IF( BUF80(1)(12:13) .EQ. PATHWD(IP) ) THEN
         IF( BUF80(1)(15:15) .EQ. 'E' ) THEN
          SEVER = 4
          WRITE(DEV70,6000) BUF80(1)
         ELSE IF(BUF80(1)(15:15) .EQ. 'W') THEN
          SEVER = 3
          WRITE(DEV70,6000) BUF80(1)
         ELSE IF(BUF80(1)(15:15) .EQ. 'I') THEN
          SEVER = 2
         ELSE IF(BUF80(1)(15:15) .EQ. 'Q') THEN
          SEVER = 1
         ELSE IF(BUF80(1)(15:15) .EQ. 'T') THEN
          SEVER = 0
         ELSE
          GO TO 20
         ENDIF
C
         READ(BUF80(1)(16:17),6001,ERR=6300,IOSTAT=IOST60) ICODE1
         ICODE2  = ICODE1/10 + 1
         SUMTBL(IP,SEVER,ICODE2) = SUMTBL(IP,SEVER,ICODE2) + 1
        ENDIF
   20  CONTINUE
C
       GO TO 10
C
C *** CONTINUE HERE WHEN THE END OF THE ERROR FILE IS REACHED
C
 6100  CONTINUE
C
C *** ACCUMULATE THE TOTALS HORIZONTALLY AND VERTICALLY
C
      DO 25 ICODE2 = 1,8
       DO 26 SEVER = 0,4
        DO 27 IP = 1,6
         ITOT1(ICODE2) = ITOT1(ICODE2) + SUMTBL(IP,SEVER,ICODE2)
         ITOT2(IP,SEVER) = ITOT2(IP,SEVER) + SUMTBL(IP,SEVER,ICODE2)
   27   CONTINUE
   26  CONTINUE
   25 CONTINUE
C
      DO 28 ICODE2 = 1,8
       TOTAL = TOTAL + ITOT1(ICODE2)
   28 CONTINUE
C
      DO 35 IP = 1,8
       IWORK1(IP+100) = (IP - 1)*10
       IWORK1(IP+120) = IP*10 - 1
   35 CONTINUE
C
      WRITE(IRD5,5010) (IWORK1(I+100),IWORK1(I+120),I=1,8)
      WRITE(IRD5,5020)
      DO 40 IP = 1,6
        SUM = ITOT2(IP,0) + ITOT2(IP,1) + ITOT2(IP,2) +
     &        ITOT2(IP,3) + ITOT2(IP,4)
        IF(SUM .EQ. 0) GO TO 40
        WRITE(IRD5,5030) PATHWD(IP)
        DO 50 SEVER = 4,0,-1
C
C ***    THERE ARE NO TRACE ERRORS FOR PATHS 1 - 5
         IF((SEVER .EQ. 0) .AND. (IP.NE. 6)) GO TO 50
C
C ***    IF THIS IS THE QA CODE, DON'T PRINT FOR PATHS: JB, MR, MP
         IF(SEVER .EQ. 1) THEN
          IF( (IP.LT.2) .OR. (IP.GT.4) ) THEN
           GO TO 50
          ELSE
C
C ***      DID WE QA DATA? IF NOT, GO TO 50
           IF( (PSTAT(IP) .LT. 2) .OR. (PSTAT(IP).EQ.4) )THEN
            GO TO 50
           ENDIF
          ENDIF
         ENDIF
         WRITE(IRD5,5040) ETYPE(SEVER),
     &        (SUMTBL(IP,SEVER,ICODE2),ICODE2=1,8),ITOT2(IP,SEVER)
   50   CONTINUE
   40 CONTINUE
      WRITE(IRD5,5020)
      WRITE(IRD5,5042) (ITOT1(ICODE2),ICODE2=1,8),TOTAL
C
C *** REWIND DEV70 AND REITERATE THE WARNING MESSAGES BY PATH
C
      ICODE1 = 0
      WRITE(IRD5,5050)
      DO 60 IP = 1,6
       REWIND DEV70
       NCNT = 0
  65   BUF80(1) = BLNK80
       NCNT = NCNT + 1
       READ(DEV70,6000,END=60,ERR=6400,IOSTAT=IOST60) BUF80(1)
       IF(BUF80(1)(12:13) .EQ. PATHWD(IP) .AND. (BUF80(1)(15:15)
     &     .EQ. 'W') ) THEN
        WRITE(IRD5,6000) BUF80(1)
        ICODE1 = 1
      ENDIF
       GO TO 65
  60  CONTINUE
C
      IF(ICODE1 .EQ.0) THEN
       WRITE(IRD5,6010)
      ENDIF
C
C
C *** REWIND DEV70 AND REITERATE THE ERROR MESSAGES BY PATH
C
      ICODE1 = 0
      WRITE(IRD5,5055)
      DO 70 IP = 1,6
       REWIND DEV70
       NCNT = 0
  75   BUF80(1) = BLNK80
       NCNT = NCNT + 1
       READ(DEV70,6000,END=70,ERR=6400,IOSTAT=IOST60) BUF80(1)
       IF(BUF80(1)(12:13) .EQ. PATHWD(IP) .AND. (BUF80(1)(15:15)
     &     .EQ. 'E') ) THEN
        WRITE(IRD5,6000) BUF80(1)
        ICODE1 = 1
       ENDIF
       GO TO 75
  70  CONTINUE

      IF(ICODE1 .EQ. 0) THEN
       WRITE(IRD5,6010)
      ENDIF
C
      IF(STATUS(1,3) .EQ. 2) THEN
       WRITE(DEVIO,5060) DISK50
      ENDIF
C
      ELSE
C
C     THERE IS NO ERROR/MESSAGE FILE AND/OR TEMPORARY FILE
C
      WRITE(IRD5,5070)
      END IF
C
      RETURN
C
C-----------------------------------------------------------------------
C *** PROCESSING CONTINUE HERE IN CASE AN ERROR OCCURS IN A READ STMT
C
 6200 CONTINUE
      WRITE(IRD5,6020) NCNT, IRD3, IOST60
      RETURN
C
 6300 CONTINUE
      WRITE(IRD5,6030) NCNT, IOST60
      RETURN
C
 6400 CONTINUE
      WRITE(IRD5,6040) NCNT, DEV70, IOST60
      RETURN
C
C-----------------------------------------------------------------------
C *** FORMAT STATEMENTS
C
C 5001 FORMAT(16X,'STAGE 1 EXTRACTION AND QA OF METEOROLOGICAL DATA')   DTB95025
C 5002 FORMAT(16X,'STAGE 3 PROCESSING OF MERGED METEOROLOGICAL DATA')   DTB95025
 5005 FORMAT(//22X,'**** MPRM MESSAGE SUMMARY TABLE ****')
 5010 FORMAT(/8X,8(I2,'-',I2,3X),'TOTAL')
 5020 FORMAT(7X,70('-'))
 5030 FORMAT(/4X,A2)
 5040 FORMAT(6X,A1,8(I5,3X),I5)
 5042 FORMAT(7X,8(I5,3X),I5)
 5050 FORMAT(/8X,'****   WARNING MESSAGES ****')
 5055 FORMAT(/8X,'****    ERROR MESSAGES  ****')
 5060 FORMAT(5X,'**** THE GENERAL REPORT GENERATED BY',/5X,
     &          '     THIS RUN IS STORED IN FILE:'/10X,A48//)
 5070 FORMAT(18X,' NO SUMMARY TABLE IS AVAILABLE BECAUSE THERE IS',/,
     &       18X,' NO ERROR/MESSAGE AND/OR TEMPORARY FILE(S) OPEN')
 5100 FORMAT(//14X,56('*'),
     & /14X,'***   THIS RUN IS ONLY A CHECK OF THE INPUT IMAGES   ***',
     & /14X,'********************************************************')
 5110 FORMAT(//14X,56('*'),
     & /14X,'***             ABNORMAL JOB TERMINATION             ***',
     & /14X,'********************************************************')
 5120 FORMAT(//14X,56('*'),
     & /14X,'***             JOB TERMINATED NORMALLY              ***',
     & /14X,'********************************************************')
 6000 FORMAT(A80)
 6001 FORMAT(I2)
 6002 FORMAT(' ')
 6010 FORMAT(/15X,'---  NONE  ---'/)
 6020 FORMAT(5X,'ERROR READING RECORD',I5,' ON UNIT',I3,' WITH IOSTAT',
     &     I8,/5X,'NO FURTHER PROCESSING')
 6030 FORMAT(5X,'ERROR READING 2-DIGIT CODE FROM BUF80 AT RECORD',I5,
     &     ' WITH IOSTAT',I8,/5X,'NO FURTHER PROCESSING')
 6040 FORMAT(5X,'ERROR READING INFORMATIONAL MESSAGE AT RECORD',I5,
     &     ' ON UNIT',I3,' WITH IOSTAT',I8,/5X,'NO FURTHER PROCESSING')
 6050 FORMAT(' ERROR READING SCAN REPORT ON DEV70, I/O STATUS=',I8)
 6060 FORMAT(' END-OF-FILE ON DEV70, SCAN REPORT INCOMPLETE')
C
C---
      END


      SUBROUTINE STONUM(STRVAR,LENGTH,FNUM,IMUTI)
C***********************************************************************
C                 STONUM Module of ISC2 Model
C                     (borrowed for MPRM)
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
      FDEC  = 1.0

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
      IF (EMARK .AND. ABS(FNUM) .LE. 30.0) THEN
         FNUM = HEAD*10**(FNUM)
      ELSE IF (EMARK .AND. ABS(FNUM) .GT. 30.0) THEN
         IF (FNUM .LT. 0.0) THEN
            FNUM = 0.0
         ELSE IF (FNUM .GT. 0.0) THEN
            FNUM = HEAD * 10**30.0
         END IF
         GO TO 9999
      END IF

      GO TO 1000

C     Set Error Switch for Illegal Numerical Field (WRITE Message and Handle
C     Error in Calling Routine)
 9999 IMUTI = -1

 1000 RETURN
      END
