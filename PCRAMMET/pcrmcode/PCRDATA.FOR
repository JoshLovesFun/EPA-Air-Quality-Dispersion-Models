      SUBROUTINE READSF (NREAD)
C***********************************************************************
C*    READSF  :   A Subroutine for the PCRAMMET Meteorological
C*                Preprocessor for EPA Dispersion Models
C*
C*    PURPOSE:    Reads 24 hours of Surface Data from either CD144 or
C*                SAMSON format.
C*
C*    PROGRAMMER: Jayant Hardikar, Jim Paumier
C*                PES Inc.
C*
C*    DATE:       August 15, 1995
C*
C*    INPUTS:     File Types and Unit Numbers
C*
C*
C*    OUTPUTS:    Character array of hourly weather data
C*
C*    CALLED FROM: MAIN program
C***********************************************************************
C*
C*    Variable Declarations
      INCLUDE 'PCRAM.INC'

      IF (SFCTYP .EQ. 'CD144' ) THEN
C*       DATA TYPE IS CD144
         CALL CD144(INSFC, ACD144, EOFSFC, LWD, NREAD)

      ELSEIF (SFCTYP .EQ. 'SCRAM' ) THEN
C*       DATA TYPE IS SCRAM
         CALL SCRAM(INSFC, ACD144, EOFSFC, LWD, NREAD)

      ELSEIF (SFCTYP .EQ. 'SAMSON') THEN
C*       DATA TYPE IS SAMSON
         CALL SAMSON (NVARS,IDVAR,JSAMSN,MAXSAM,IDSFC,CITY,STATE,
     &           ITZONE,XLAT,XLON,ISELEV,INSFC,EOFSFC,NREAD,
     &           WETFLG,DRYFLG,PPTFLG)

      ELSEIF( SFCTYP .EQ. 'HUSWO' )THEN

C*       Data type is HUSWO (with assumed ENGLISH units)
         CALL HUSWO (NVARS,IFC,IDVAR,JHUSWO,MAXHUS,INSFC,EOFSFC,
     &                  NREAD,WETFLG,DRYFLG,PPTFLG)

      ENDIF

      RETURN
      END


      SUBROUTINE SAMSON (NVARS,IDVAR,JVALUE,MAXVAR,IWBAN,CITY,STATE,
     &                   ITZONE,XLAT,XLON,ISELEV,IUNIT,EOFSFC,NREAD,
     &                   WETFLG,DRYFLG,PPTFLG)
C***********************************************************************
C*    SAMSON Module of PCRAMMET Meteorological Pre-processor
C*
C*    PURPOSE:    Reads met data from a SAMSON met file
C*                in 24 hour blocks.  Returns a character array of up
C*                to 21 variables for 24 hours
C*
C*    PROGRAMMER: Jayant Hardikar, Jim Paumier
C*                PES Inc.
C*
C*    DATE:       August 15, 1995
C*
C*    INPUTS:     SAMSON met data file
C*
C*    OUTPUTS:    Arrays of Various Met Parameters for 24 hours
C*
C*    Modifications:
C*       7/31/98 - Added code to retrieve relative humidity and global
C*                 radiation from CD if dry or wet deposition selected
C*
C*    CALLED FROM: READSF
C***********************************************************************
C*
C*    Variable Declarations
C*    (ILINE is Used to Check If This Routine is Being Called
C*     for the Very First Time or Not)
      SAVE          ILINE

      CHARACTER*40  FIELD(26)          ! dimensioned MAXSAM
      CHARACTER*256 ALINE
      CHARACTER     CITY*22, STATE*2, NS*1, EW*1
      INTEGER       LATDEG,LATMIN,IWBAN,ITZONE
      INTEGER       LONDEG,LONMIN,ISELEV
      INTEGER       IDVAR(MAXVAR-5)
      CHARACTER*9   JVALUE(MAXVAR,24)
      CHARACTER*9   JTEMP(26)          ! dimensioned MAXSAM
      CHARACTER*2   VFMT(21)           ! dimensioned MAXSAM-5
      CHARACTER*180 PFMT
      REAL          XLAT,XLON
      LOGICAL       GOTCLD, GOTDBT, GOTWD, GOTWS, GOTCHT, GOTPWX, GOTPPT
      LOGICAL       EOFSFC, PPTFLG, WETFLG, DRYFLG, GOTGRAD, GOTRELH

      DATA ILINE /0/
      DATA VFMT /'A4','A4','A7','A7','A7','A2','A2','A5','A5','A3',
     &           'A4','A3','A5','A6','A6','A9','A4','A6','A4','A3',
     &           'A6'/


C*    First time reading the file, read the file headers
      IF (ILINE .EQ. 0) THEN
C*       READ STATION HEADER RECORD
         READ (IUNIT,6000,ERR=1035) IWBAN,CITY,STATE,ITZONE,NS,LATDEG,
     &                              LATMIN,EW,LONDEG,LONMIN,ISELEV

C*       CONVERT DEG/MIN TO DECIMAL DEGREES
C*       North and West = positive, South and East = negative
         XLAT = LATDEG + LATMIN/60.0
         IF(NS.EQ. 'S') XLAT=XLAT*(-1.0)
         XLON=LONDEG+LONMIN/60.0
         IF(EW.EQ. 'E') XLON=XLON*(-1.0)

C*       Convert the time zone so POSITIVE represents west longitudes
         ITZONE = -ITZONE

C*       READ EXTRACTED-VARIABLES HEADER RECORD
         READ (IUNIT,'(A)') ALINE

C*       PARSE THE LINE TO DETERMINE THE INDIVIDUAL VARIABLES
         CALL PARSER (ALINE,FIELD,IFC)

C*       DECREMENT THE COUNTER FOR NUMBER OF VARIABLES TO EXCLUDE
C*       YR,MO,DY,HR,I FIELDS
         NVARS = IFC-5

C*       DETERMINE THE FORMAT AND THE VARIABLES TO READ
         WRITE (PFMT(1:19),'(A)') "(4(1X,A2),1X,A1,1X,"
         IPOS = 19

         DO 200 IVAR = 1,NVARS

C*          GET THE VARIABLE ID
            CALL STONUM(FIELD(IVAR+5),40,FNUM,IMIT)
            IF (IMIT .NE. 1) THEN
               CALL ERRHDL('E','1036',1)

            ELSE
               IDVAR(IVAR) = INT(FNUM)
            END IF

            IPOS = IPOS+1
            WRITE (PFMT(IPOS:IPOS+1),'(A2)') VFMT(IDVAR(IVAR))
            IPOS = IPOS +2
            IF (IVAR .NE. NVARS) THEN
               WRITE (PFMT(IPOS:IPOS+3),'(A4)') ",1X,"
               IPOS = IPOS +3
            ELSE
              WRITE (PFMT(IPOS:IPOS),'(A1)') ")"
            ENDIF

200      CONTINUE

C        SET FLAG FOR FIRST-TIME READ
         ILINE = ILINE + 1

C        Check the ID numbers of the variables to be sure there
C        is sufficient data to perform the calculations
C        NVARS = # of variables retrieved from SAMSON CD
C        IDVAR = ID number

         DO 250 IV = 1,NVARS
            IF( IDVAR(IV) .EQ. 7 )THEN
               GOTCLD = .TRUE.
            ELSEIF( IDVAR(IV) .EQ. 8 )THEN
               GOTDBT = .TRUE.
            ELSEIF( IDVAR(IV) .EQ. 12 )THEN
               GOTWD = .TRUE.
            ELSEIF( IDVAR(IV) .EQ. 13 )THEN
               GOTWS = .TRUE.
            ELSEIF( IDVAR(IV) .EQ. 15 )THEN
               GOTCHT = .TRUE.
            ELSEIF( WETFLG  .AND.  .NOT. PPTFLG )THEN
               IF( IDVAR(IV) .EQ. 16 )THEN
                  GOTPWX = .TRUE.
               ELSEIF( IDVAR(IV) .EQ. 21 )THEN
                  GOTPPT = .TRUE.
               ENDIF
            ENDIF

C           Additional fields are required for deposition processing
C           3 = global radition; 10 = relative humidity
            IF( DRYFLG .OR. WETFLG )THEN
               IF( IDVAR(IV) .EQ. 3 )THEN
                  GOTGRAD = .TRUE.
               ELSEIF( IDVAR(IV) .EQ. 10 )THEN
                  GOTRELH = .TRUE.
               ENDIF
            ENDIF

  250    CONTINUE

C        Now check the logical variables to determine if there is
C        sufficient data to continue processing
         IF( .NOT. GOTCLD  .OR.  .NOT. GOTDBT  .OR.  .NOT. GOTCHT
     &       .OR.  .NOT. GOTWD   .OR.  .NOT. GOTWS) THEN
            CALL ERRHDL( 'E', '1120', 0 )
         ENDIF

         IF( WETFLG  .AND.  .NOT. PPTFLG )THEN
            IF( .NOT. GOTPWX  .OR.  .NOT. GOTPPT )THEN
               CALL ERRHDL( 'E', '1125', 0 )
            ENDIF
         ENDIF

         IF( WETFLG .OR. DRYFLG )THEN
            IF( .NOT. GOTGRAD  .OR.  .NOT. GOTRELH )THEN
               CALL ERRHDL( 'E', '1126', 0 )
            ENDIF
         ENDIF

C     Endif first time reading the file
      ENDIF

6000  FORMAT (T2,I5,T8,A22,T31,A2,T34,I3,T39,A1,T40,I2,T43,I2,
     &        T47,A1,T48,I3,T52,I2,T56,I4)

C*    For all calls to this routine, read data
C*    NREAD keeps track of the number of data records read

      NREAD = 0

C*    LOOP HOURS
      DO 700 IHR = 1,24

C*       READ DATA
         READ (IUNIT,PFMT,END=999,ERR=1037) (JTEMP(IV),IV=1,NVARS+5)
         NREAD = NREAD + 1

C*       ASSIGN THE DATA TO THE APPROPRIATE VARIABLE INDICES
         DO 300 II = 1,5
            JVALUE(II,IHR) = JTEMP(II)
300      CONTINUE

         DO 600 IALL = 6,MAXVAR
            DO 500 IV = 1,NVARS
               IF (IALL-5 .EQ. IDVAR(IV)) THEN
                  JVALUE(IALL,IHR) = JTEMP(IV+5)
                  GO TO 600
               ELSE
                  IF (IV .EQ. NVARS) THEN
                     JVALUE(IALL,IHR) = '   '
                  ENDIF
               ENDIF
500         CONTINUE
600      CONTINUE

700   CONTINUE
      GO TO 1000

C*    Error Handling
1035  CALL ERRHDL('E','1035',1)
1037  CALL ERRHDL('E','1037',1)
C     WRITE (*,*) 'ERROR GETTING VARIABLE ID', FIELD(IVAR+5)


999   EOFSFC = .TRUE.
1000  RETURN
      END


      SUBROUTINE HUSWO (NVARS,IFC,IDVAR,JVALUE,MAXVAR,IUNIT,EOFSFC,
     &                  NREAD,WETFLG,DRYFLG,PPTFLG)
C***********************************************************************
C*    HUSWO Module of PCRAMMET Meteorological Pre-processor
C*
C*    PURPOSE:    Reads met data from a HUSWO met file in 24-hour
C*                blocks.  Returns a character array of up to 20
C*                variables for 24 hours
C*
C*    PROGRAMMER: PES Inc.
C*
C*    DATE:       May 1998
C*
C*    INPUTS:     HUSWO met data file (data retrieved from CD)
C*
C*    OUTPUTS:    Arrays of Various Met Parameters for 24 hours
C*
C*    Modifications:
C*       7/31/98 - Added code to retrieve relative humidity and global
C*                 radiation from CD if dry or wet deposition selected
C*
C*
C*    CALLED FROM: READSF
C***********************************************************************

C*    Variable Declarations
C*    (ILINE is Used to Check If This Routine is Being Called
C*     for the Very First Time or Not)

      SAVE          ILINE

      CHARACTER*40  FIELD(24)          ! dimensioned MAXHUS
      CHARACTER*256 ALINE
      INTEGER       IDVAR(MAXVAR)
      CHARACTER*9   JVALUE(MAXVAR,24)
c     CHARACTER*9   JTEMP(24)          ! dimensioned MAXHUS
      CHARACTER*2   VFMT(24)           ! dimensioned MAXHUS
      CHARACTER*120  PFMT
      LOGICAL       GOTCLD, GOTDBT, GOTWD, GOTWS, GOTCHT, GOTPWX, GOTPPT
      LOGICAL       EOFSFC, PPTFLG, WETFLG, DRYFLG, GOTGRAD, GOTRELH

      DATA ILINE /0/
      DATA VFMT /'A5','A1','A4','A2','A2','A2','A4','A4','A2','A2',
     &           'A5','A6','A3','A4','A4','A4','A6','A5','A8','A5',
     &           'A5','A5','A4','A4'/
      DATA PFMT/' '/


C*    First time reading the file, read the file header
      IF (ILINE .EQ. 0) THEN
C*       READ EXTRACTED-VARIABLES HEADER RECORD
         READ (IUNIT,'(A)') ALINE

C*       PARSE THE LINE TO DETERMINE THE INDIVIDUAL VARIABLES
         CALL PARSER (ALINE,FIELD,IFC)

C*       DECREMENT THE COUNTER OF THE NUMBER OF FIELDS READ TO DETERMINE
C8       THE NUMBER OF WEATHER VARIABLES READ
C*       (STATION AND YEAR FIELDS ARE EXCLUDED)
         NVARS = IFC-2

C*       DETERMINE THE FORMAT AND THE VARIABLES TO READ
C        Format of the first 6 variables, which are always in the file;
C        Variables are: station, ASOS flag, 4-digit year, month, day, hour
         WRITE (PFMT(1:17),'(A)') "(A5,A1,A4,3A2,1X,"
         IPOS = 17

         DO 200 IVAR = 3,IFC

C*          GET THE VARIABLE ID FROM THE FIELD NUMBERS READ ON THE 1ST RECORD
            CALL STONUM(FIELD(IVAR),40,FNUM,IMIT)
            IF (IMIT .NE. 1) THEN
               CALL ERRHDL('E','1036',1)
            ELSE
               IDVAR(IVAR) = INT(FNUM)
            END IF

C           Set the pointer to the position just past the last comma
C           and write the format of the variable to the format statement
C           that will be used to read the data; since there are 4
C           unnumbered fields, the correct position in the VFMT array is
C           4 past the field number being processed
            IPOS = IPOS+1
            WRITE (PFMT(IPOS:IPOS+1),'(A2)') VFMT(IDVAR(IVAR)+4)
            IPOS = IPOS +2
            IF (IVAR .NE. IFC) THEN
               IF( IDVAR(IVAR) .NE. 5 )THEN
                  WRITE (PFMT(IPOS:IPOS+3),'(A4)') ",1X,"
                  IPOS = IPOS +3
               ELSE
                  WRITE(PFMT(IPOS:IPOS),'(A1)')  ","
               ENDIF
            ELSE
              WRITE (PFMT(IPOS:IPOS),'(A1)') ")"
            ENDIF

200      CONTINUE


C*       SET FLAG FOR FIRST-TIME READ
         ILINE = ILINE + 1

C*       Check the ID numbers of the variables to be sure there
C*       is sufficient data to perform the calculations

         DO 250 IV = 1,IFC
            IF( IDVAR(IV) .EQ. 5 .OR. IDVAR(IV) .EQ. 16 )THEN
C              Either total/opaque cloud cover is in the file and/or the
C              ASOS cloud layers are in the file
               GOTCLD = .TRUE.
            ELSEIF( IDVAR(IV) .EQ. 7 )THEN
C              Dry bulb temperature
               GOTDBT = .TRUE.
            ELSEIF( IDVAR(IV) .EQ. 11 )THEN
C              Wind direction
               GOTWD = .TRUE.
            ELSEIF( IDVAR(IV) .EQ. 12 )THEN
C              Wind speed
               GOTWS = .TRUE.
            ELSEIF( IDVAR(IV) .EQ. 14 )THEN
C              Ceiling height
               GOTCHT = .TRUE.
            ELSEIF( WETFLG  .AND.  .NOT. PPTFLG )THEN
               IF( IDVAR(IV) .EQ. 15 )THEN
C                 Present weather
                  GOTPWX = .TRUE.
               ELSEIF( IDVAR(IV) .EQ. 19 )THEN
C                 Precipitation
                  GOTPPT = .TRUE.
               ENDIF
            ENDIF

C           Additional fields are required for deposition processing
C           3 = global radition; 9 = relative humidity
            IF( DRYFLG .OR. WETFLG )THEN
               IF( IDVAR(IV) .EQ. 3 )THEN
                  GOTGRAD = .TRUE.
               ELSEIF( IDVAR(IV) .EQ. 9 )THEN
                  GOTRELH = .TRUE.
               ENDIF
            ENDIF

  250    CONTINUE

         IF( .NOT. GOTCLD  .OR.  .NOT. GOTDBT  .OR.  .NOT. GOTCHT
     &       .OR.  .NOT. GOTWD   .OR.  .NOT. GOTWS) THEN
            CALL ERRHDL( 'E', '1140', 0 )
         ENDIF
         IF( WETFLG  .AND.  .NOT. PPTFLG )THEN
            IF( .NOT. GOTPWX  .OR.  .NOT. GOTPPT )THEN
               CALL ERRHDL( 'E', '1145', 0 )
            ENDIF
         ENDIF

         IF( WETFLG .OR. DRYFLG )THEN
            IF( .NOT. GOTGRAD  .OR.  .NOT. GOTRELH )THEN
               CALL ERRHDL( 'E', '1126', 0 )
            ENDIF
         ENDIF

C     Endif first time reading the file
      ENDIF

C*    For all calls to this routine, read data
C*    NREAD keeps track of the number of data records read

      NREAD = 0

C*    LOOP HOURS
      DO 700 IHR = 1,24

C*       READ DATA; NOTE THAT THERE ARE 4 'UNNAMED' FIELDS ON THE 1ST
C        RECORD - ASOS FLAG, MONTH, DAY, AND HOUR - IN ADDITION TO THE
C        'IFC' NUMBERED FIELDS
         READ (IUNIT,PFMT,END=999,ERR=1045) (JVALUE(IV,IHR),IV=1,IFC+4)
         NREAD = NREAD + 1

700   CONTINUE

      GO TO 1000

C*    Error Handling (program stops in the error handler)

1045  CALL ERRHDL('E','1045',1)


999   EOFSFC = .TRUE.
1000  RETURN
      END


      SUBROUTINE CD144 (IUNIT,ADATA,EOFSFC,LWD,NREAD)
C***********************************************************************
C*    CD144 Module of PCRAMMET Meteorological Pre-processor
C*
C*    PURPOSE:    Reads met data from a CD144 met file
C*                in 24 hour blocks.  Returns a character array of
C*                12 variables for 24 hours
C*
C*    PROGRAMMER: Jayant Hardikar, Jim Paumier
C*                PES Inc.
C*
C*    DATE:       August 15, 1995
C*
C*    INPUTS:     CD144 met data file
C*
C*    OUTPUTS:    Arrays of Various Met Parameters for 24 hours
C*
C*    CALLED FROM: READSF
C***********************************************************************
C*
C*    Variable Declarations
C*    (ISF is Used to Check If This Routine is Being Called
C*    for the Very First Time or Not)
      SAVE ISF
      INTEGER IUNIT
      CHARACTER*5 ADATA(12,24)
      LOGICAL EOFSFC

      DATA ISF /0/

C*    Skip the Very First Hour-00 (Belongs to Previous Day)
C*    However, keep the station ID and wind direction for calms processing
      ISF = ISF + 1
      IF (ISF .EQ. 1) THEN
         READ (IUNIT,410,ERR=1038) ADATA(1,1),LWD
      ENDIF
  410 FORMAT( A5,T39,I2 )

C     1 = Station ID
C     2 = Year
C     3 = Month
C     4 = Day
C     5 = Hour
C     6 = Ceiling ht
C     7 = Present weather
C     8 = Wind direction
C     9 = Wind speed
C     10 = Station pressure
C     11 = Dry bulb temperature
C     12 = Opaque sky cover

      NREAD = 0

C*    Begin Processing With Hour 01
      DO 10 IHR = 1,24
         READ (IUNIT,420,END=999) (ADATA (I,IHR), I=1,12)
         NREAD = NREAD + 1
   10 CONTINUE
  420 FORMAT (A5,4A2,A3,8X,A5,9X,2A2,A4,A3,T79,A1)

      GO TO 1000

1038  CALL ERRHDL('E','1038',1)

999   EOFSFC = .TRUE.
      IF( IHR .EQ. 24 )THEN
C*       Duplicate last hour's data for hr=24
         DO 20 IVBL = 1,12
            ADATA(IVBL,24) = ADATA(IVBL,23)
   20    CONTINUE
         ADATA(5,24) = '24'
         NREAD = NREAD + 1
      ENDIF

1000  RETURN
      END


      SUBROUTINE SCRAM (IUNIT,ADATA,EOFSFC,LWD,NREAD)
C***********************************************************************
C*    SCRAM Module of PCRAMMET Meteorological Pre-processor
C*
C*    PURPOSE:    Reads met data from a SCRAM met file
C*                in 24 hour blocks.  Returns a character array of
C*                12 variables for 24 hours (with station pressure
C*                and present weather as missing)
C*
C*    PROGRAMMER: Jayant Hardikar, Jim Paumier
C*                PES Inc.
C*
C*    DATE:       June 30, 1995
C*
C*    INPUTS:     SCRAM met data file
C*
C*    OUTPUTS:    Arrays of Weather Varaibles for 24 hours
C*
C*    CALLED FROM: READSF
C***********************************************************************
C*
C*    Variable Declarations
C*    (ISF is Used to Check If This Routine is Being Called
C*    for the Very First Time or Not)
      SAVE ISF
      INTEGER IUNIT
      CHARACTER*5 ADATA(12,24)
      CHARACTER  SPEED*3, TOTAL*2, OPAQ*2
      LOGICAL EOFSFC

      DATA ISF /0/

C*    Skip the Very First Hour-00 (Belongs to Previous Day)
C*    However, keep the station ID and wind direction for calms processing
      ISF = ISF + 1
      IF (ISF .EQ. 1) THEN
         READ (IUNIT,410,ERR=1038) ADATA(1,1),LWD
      ENDIF
  410 FORMAT( A5,T17,I2 )

C     1 = Station ID
C     2 = Year
C     3 = Month
C     4 = Day
C     5 = Hour
C     6 = Ceiling ht
C     7 = Present weather (not available)
C     8 = Wind direction
C     9 = Wind speed
C     10 = Station pressure (not available)
C     11 = Dry bulb temperature
C     12 = Opaque sky cover

      NREAD = 0

C*    Begin Processing With Hour 01
      DO 10 IHR = 1,24
         READ (IUNIT,420,END=999,ERR=1038) ADATA(1,IHR),ADATA(2,IHR),
     &         ADATA(3,IHR),ADATA(4,IHR),ADATA(5,IHR),ADATA(6,IHR),
     &         ADATA(8,IHR),SPEED,ADATA(11,IHR),TOTAL,OPAQ
         NREAD = NREAD + 1
C------- Set present weather (vbl 7) and stn pressure (vbl 10) to missing
         ADATA(7,IHR)  = '     '
         ADATA(10,IHR) = '    '

C------- The SCRAM data are processed through the CD-144 routines;
C        therefore, the data first must be formatted after CD-144.
C        The program MET144 was used as a guide to developing the
C        conversion.

C------- Ceiling height, temperature and wind direction do not need
C        converting

C------- Wind Speed
         IF( SPEED .EQ. '   ' )THEN
            ADATA(9,IHR) = '     '

         ELSE
            READ( SPEED,'(I3)') IWS
            IF( IWS .LE. 99 )THEN
               WRITE( ADATA(9,IHR),'(A2)' ) SPEED(2:3)

            ELSE
               ADATA(9,IHR)(2:2) = SPEED(3:3)
               IF( IWS .LT. 200 )THEN
                  ADATA(9,IHR)(1:1) = 'R'

               ELSEIF( IWS .LT. 190 )THEN
                  ADATA(9,IHR)(1:1) = 'Q'

               ELSEIF( IWS .LT. 180 )THEN
                  ADATA(9,IHR)(1:1) = 'P'

               ELSEIF( IWS .LT. 170 )THEN
                  ADATA(9,IHR)(1:1) = 'O'

               ELSEIF( IWS .LT. 160 )THEN
                  ADATA(9,IHR)(1:1) = 'N'

               ELSEIF( IWS .LT. 150 )THEN
                  ADATA(9,IHR)(1:1) = 'M'

               ELSEIF( IWS .LT. 140 )THEN
                  ADATA(9,IHR)(1:1) = 'L'

               ELSEIF( IWS .LT. 130 )THEN
                  ADATA(9,IHR)(1:1) = 'K'

               ELSEIF( IWS .LT. 120 )THEN
                  ADATA(9,IHR)(1:1) = 'J'

               ELSEIF( IWS .LT. 110 )THEN
                  ADATA(9,IHR)(1:1) = '}'

               ENDIF
            ENDIF
         ENDIF

C----    Opaque cloud cover
         IF( OPAQ .NE. '  ') THEN
            IF( OPAQ .EQ. '10' )THEN
               ADATA(12,IHR) = '-'
            ELSE
               WRITE( ADATA(12,IHR),'(A1)' ) OPAQ(2:2)
            ENDIF

         ELSE
            IF( TOTAL .NE. '  ' )THEN
               IF( TOTAL .EQ. '10' )THEN
                  ADATA(12,IHR) = '-'
               ELSE
                  WRITE( ADATA(12,IHR),'(A1)' ) TOTAL(2:2)
               ENDIF

            ELSE
               IF( ADATA(6,IHR) .EQ. '   ' )THEN
                  ADATA(12,IHR) = ' '

               ELSEIF( ADATA(6,IHR) .EQ. '---' )THEN
                  ADATA(12,IHR) = '0'

               ELSEIF( ADATA(6,IHR) .LT. '070' )THEN
                  ADATA(12,IHR) = '7'

               ELSE
                  ADATA(12,IHR) = '0'

               ENDIF
            ENDIF
         ENDIF

   10 CONTINUE

  420 FORMAT (A5,4A2,A3,A2,A3,A3,a2,A2)

      GO TO 1000

1038  CALL ERRHDL('E','1038',1)

999   EOFSFC = .TRUE.
      IF( IHR .EQ. 24 )THEN
C*       Duplicate last hour's data for hr=24
         DO 20 IVBL = 1,12
            ADATA(IVBL,24) = ADATA(IVBL,23)
   20    CONTINUE
         ADATA(5,24) = '24'
         NREAD = NREAD + 1
      ENDIF

1000  RETURN
      END


      SUBROUTINE FILMET (NREAD)
C***********************************************************************
C*    FILMET Module of PCRAMMET Meteorological Pre-processor
C*
C*    PURPOSE:    Fills in the Appropriate Weather Variable Arrays,
C*                Translating Character to Numeric
C*
C*    PROGRAMMER: Jayant Hardikar, Jim Paumier
C*                PES Inc.
C*
C*    DATE:       August 15, 1995
C*
C*    INPUTS:     Surface and Precip Raw Data
C*                NREAD = # of hourly sfc obs read for the day
C*
C*    OUTPUTS:    24 Hour Arrays of Numeric Weather Data
C*
C*    CALLED FROM: MAIN program
C***********************************************************************
C*
C*    Variable Declarations
      INCLUDE 'PCRAM.INC'
      CHARACTER*1  ASOSYN
      LOGICAL      ASOSNOW

      DATA ASOSNOW/.FALSE./

      DO 100 IH = 1,24

C*       Fill in Surface Variables
         IF (SFCTYP .EQ. 'SAMSON') THEN
C*          From SAMSON ...
            CALL FILSAM(IH)

         ELSEIF( SFCTYP .EQ. 'HUSWO' )THEN
C*          From HUSWO ...
            CALL FILHUS(IH, NREAD, ASOSYN, ASOS1, ASOS2, ASOS3)

C*          Since HUSWO will be the first to have ASOS obs. in a data
C           base, ASOS cloud reports need to be converted to the standard
C           format if the ASOS flag (ASOSYN) is equal to 'A'
            IF (ASOSYN .EQ. 'A' )THEN

C              Let the user know that ASOS data are now being processed
               IF( .NOT. ASOSNOW ) THEN
                  ASOSNOW = .TRUE.
                  WRITE( IDIAG,520 ) ISYR(IH),ISMO(IH),ISDY(IH),IH
               ENDIF
 
               CALL DOCLDS(ISYR(IH),ISMO(IH),ISDY(IH),IH, IDIAG,
     &                     ASOS1,ASOS2,ASOS3, ICC, ICHT)
               ICOVER(IH) = ICC
               IF( ICEIL(IH) .EQ. 9999 )THEN
                  ICEIL(IH)  = ICHT
               ENDIF
            ENDIF

         ELSEIF (SFCTYP .EQ. 'CD144'  .OR.  SFCTYP .EQ. 'SCRAM') THEN
C*          Or From CD144 or SCRAM
            CALL FIL144(IH)

         ENDIF


C*       Check the wind direction:
         IF( IDIR(IH) .NE. 0  .AND. IDIR(IH) .NE. 999 )THEN
C*          It is not a calm hour and not missing - save the direction
            LWD = IDIR(IH)
         ELSEIF( IDIR(IH) .EQ.  0 )THEN
C*          The direction is zero (calm hour), set the direction to the
C           previous valid wind direction
            IDIR(IH) = LWD
         ENDIF

C*       Fill in Hourly Precipitation Variable If Needed
         IF (WETFLG) THEN

C*          If the Surface Data is SAMSON
            IF (SFCTYP .EQ. 'SAMSON' .OR. SFCTYP .EQ. 'HUSWO' )THEN

C*             SAMSON/HUSWO Precip (in PRECIP(IHR)) May have been Read
C*             with surface variables

C*             If Supplementing with TD3240 Data (PPTFLG = .T.), Then
C*             Check to See if SAMSON Precip Was Missing.  If So, '
C*             Substitute With TD3240 If Non-Zero, Else Use SAMSON/HUSWO
               IF (PPTFLG) THEN
                  IF (PRECIP(IH) .GT. 9999.0) THEN
                     IF (P3240(IH) .GE. 0.0 )THEN
                        PRECIP(IH) = P3240(IH)
                        WRITE( IDIAG,500 ) ISYR(IH),ISMO(IH),ISDY(IH),IH
                        NOWARN = .FALSE.
                     ENDIF
                  ENDIF
               ENDIF

C*          Otherwise, if it is CD144 or SCRAM
            ELSE IF (SFCTYP .EQ. 'CD144' .OR. SFCTYP .EQ. 'SCRAM') THEN

C*             Get Precip Variable From 3240 Data
               PRECIP(IH) = P3240(IH)
            ENDIF

         ENDIF


100   CONTINUE

      RETURN

  500 FORMAT( ' FILMET: TD-3240 precip substituted for SAMSON/HUSWO',
     &        ' on (yy/mm/dd/hh): ', 4(i2.2:,'/') )
C 510 FORMAT( ' FILMET: All precip data missing for (yy/mm/dd/hh): ',
C    &         4(i2.2:,'/'),'; set to 0.0' )
  520 FORMAT( /,' ATTENTION: ASOS-derived observations begin',
     &        ' on (yy/mm/dd/hh): ', 3(i2.2,'/'),i2.2, / )
      END


      SUBROUTINE FILSAM(IHR)
C***********************************************************************
C*    FILSAM Module of PCRAMMET Meteorological Pre-processor
C*
C*    PURPOSE:    Translates the Weather Variables from Character to
C*                Numeric for SAMSON data
C*
C*    PROGRAMMER: Jayant Hardikar
C*                PES Inc.
C*
C*    DATE:       February 14, 1995
C*
C*    INPUTS:     Surface Data from SAMSON
C*
C*    OUTPUTS:    24 Hour Arrays of Numeric Weather Data
C*
C*    Modifications:
C*       7/31/98 - Added code to process relative humidity and global
C*                 radiation from CD if dry or wet deposition selected
C*
C*    CALLED FROM: FILMET
C***********************************************************************
C*
C*    Variable Declarations
      INCLUDE 'PCRAM.INC'
      CHARACTER*6 WFMT(MAXSAM)
      CHARACTER*9 ATEMP


      DATA WFMT /'(I2)','(I2)','(I2)','(I2)','(I1)',
     &           '(I4)','(I4)','(A7)','(A7)','(A7)','(I2)','(I2)',
     &           '(F5.0)','(F5.0)','(I3)','(I4)','(I3)','(F5.0)',
     &           '(F6.0)',
     &           '(I6)','(A9)','(I4)','(F6.0)','(I4)','(I3)','(I6)'/

C*    Obtain Date and Time Info
      READ (JSAMSN(1,IHR),WFMT(1)) ISYR(IHR)
      READ (JSAMSN(2,IHR),WFMT(2)) ISMO(IHR)
      READ (JSAMSN(3,IHR),WFMT(3)) ISDY(IHR)
      READ (JSAMSN(4,IHR),WFMT(4)) ISHR(IHR)


C*    Loop Over All the SAMSON Variables
      DO 600 IALL = 6,MAXSAM
         DO 300 IV = 1,NVARS

C*          If the Current Variable is Present, Then Process It
C*            As with CD-144 data, if the last hour of the last day
C*            of the year is missing, then substitute with data from
C*            hour 23

            IF (IALL-5 .NE. IDVAR(IV)) THEN
               IF (IV .EQ. NVARS) GO TO 600

            ELSE
C*------------ Opaque Sky Cover (tenths)
               IF (IDVAR(IV) .EQ. 7) THEN
                  READ (JSAMSN(IALL,IHR),WFMT(7+5)) ICOVER(IHR)
                  IF( ICOVER(IHR) .EQ. 99 ) THEN
                     IF( IHR .EQ. 24  .AND. ISMO(IHR) .EQ. 12  .AND.
     &                   ISDY(IHR).EQ.31 )THEN
                         ICOVER(IHR)  =  ICOVER(IHR-1)
                     ELSE
                         WRITE( IDIAG,540 ) ISYR(IHR), ISMO(IHR),
     &                                      ISDY(IHR), ISHR(IHR)
                         NOWARN = .FALSE.
                     ENDIF
                  ENDIF
               ENDIF

C*------------ Ceiling Height (meters)
               IF (IDVAR(IV) .EQ. 15) THEN
                  READ (JSAMSN(IALL,IHR),WFMT(15+5)) ICEIL(IHR)
                  IF( ICEIL(IHR) .GT. 999990 )THEN
                     IF( IHR .EQ. 24  .AND. ISMO(IHR).EQ.12  .AND.
     &                   ISDY(IHR).EQ.31 )THEN
                         ICEIL(IHR)  =  ICEIL(IHR-1)
                     ELSE
                        ICEIL(IHR) = 77777
                        WRITE( IDIAG,550 ) ISYR(IHR), ISMO(IHR),
     &                                     ISDY(IHR), ISHR(IHR)
                        NOWARN = .FALSE.
                     ENDIF
                  ENDIF
               ENDIF

C*------------ Pressure (millibars)
               IF (IDVAR(IV) .EQ. 11) THEN
                  READ (JSAMSN(IALL,IHR),WFMT(11+5)) IPRESS
                  PRESS(IHR) = IPRESS
                  IF( PRESS(IHR) .GT. 9990.0 )THEN
                     IF( IHR .EQ. 24  .AND. ISMO(IHR).EQ.12  .AND.
     &                   ISDY(IHR).EQ.31 )THEN
                         PRESS(IHR) =  PRESS(IHR-1)
                     ELSE
                        WRITE( IDIAG,520 ) ISYR(IHR), ISMO(IHR),
     &                                     ISDY(IHR), ISHR(IHR)
                        NOWARN = .FALSE.
                     ENDIF
                  ENDIF
               ENDIF

C*------------ Wind Direction (degrees)
               IF (IDVAR(IV) .EQ. 12) THEN
                  READ (JSAMSN(IALL,IHR),WFMT(12+5)) IDIR(IHR)
                  IF( IDIR(IHR) .EQ. 999 ) THEN
                     IF( IHR .EQ. 24  .AND. ISMO(IHR).EQ.12  .AND.
     &                   ISDY(IHR).EQ.31 )THEN
                         IDIR(IHR) =  IDIR(IHR-1)
                     ELSE
                        WRITE( IDIAG,500 ) ISYR(IHR), ISMO(IHR),
     &                                     ISDY(IHR), ISHR(IHR)
                        NOWARN = .FALSE.
                     ENDIF
                  ENDIF
               ENDIF

C*------------ Wind Speed (meters/second)
C*             (wind speed has two possible missing codes:
C*                         99 and 9999 - use 99 as the missing code)
               IF (IDVAR(IV) .EQ. 13) THEN
                  READ (JSAMSN(IALL,IHR),WFMT(13+5)) WSPEED(IHR)
                  IF( WSPEED(IHR) .GE. 99.0 ) THEN
                     IF( IHR .EQ. 24  .AND. ISMO(IHR).EQ.12  .AND.
     &                   ISDY(IHR).EQ.31 )THEN
                         WSPEED(IHR) =  WSPEED(IHR-1)
                     ELSE
                        WSPEED(IHR) = 99.0
                        WRITE( IDIAG,510 ) ISYR(IHR), ISMO(IHR),
     &                                     ISDY(IHR), ISHR(IHR)
                        NOWARN = .FALSE.
                     ENDIF
                  ENDIF
               ENDIF

C*------------ Temperature (degrees Celsius)
               IF (IDVAR(IV) .EQ. 8) THEN
                  READ (JSAMSN(IALL,IHR),WFMT(8+5)) TEMP(IHR)
                  IF( TEMP(IHR) .GT. 9990. )THEN
                     IF( IHR .EQ. 24  .AND. ISMO(IHR).EQ.12  .AND.
     &                   ISDY(IHR).EQ.31 )THEN
                         TEMP(IHR) =  TEMP(IHR-1)
                     ELSE
                        TEMP(IHR) = 999.9
                        WRITE( IDIAG,530 ) ISYR(IHR), ISMO(IHR),
     &                                     ISDY(IHR), ISHR(IHR)
                        NOWARN = .FALSE.
                     ENDIF
                  ENDIF
               ENDIF

C*------------ Present Weather Code
               IF (IDVAR(IV) .EQ. 16) THEN
                  READ (JSAMSN(IALL,IHR),WFMT(16+5)) ATEMP
                  DO 200 JJ = 1, 9
                     READ (ATEMP(JJ:JJ),'(I1)') IWXSAM(JJ,IHR)
200               CONTINUE
               ENDIF

C*             Hourly Precip (inches and hundredths)
C*             Convert precip to millimeters here rather than in S.UNITS
               IF (IDVAR(IV) .EQ. 21) THEN
                  READ (JSAMSN(IALL,IHR),WFMT(21+5)) IPCP
                  PRECIP(IHR) = IPCP
                  IF( PRECIP(IHR) .GT. 99990.0 ) THEN
                     IF( IHR .EQ. 24  .AND. ISMO(IHR).EQ.12  .AND.
     &                   ISDY(IHR).EQ.31 )THEN
                         PRECIP(IHR) =  PRECIP(IHR-1)
                     ELSE
                        WRITE( IDIAG,560 ) ISYR(IHR), ISMO(IHR),
     &                                     ISDY(IHR), ISHR(IHR)
                        NOWARN = .FALSE.
                     ENDIF
                  ENDIF
                  IF( PRECIP(IHR) .LE. 99990.0 ) THEN
                     PRECIP(IHR) = ( PRECIP(IHR) /100.0 ) * 25.4
                  ENDIF
               ENDIF

C              For dry and wet deposition, both relative humidity and 
C              GLOBAL radiation are required in the output file

               IF( DRYFLG  .OR.  WETFLG )THEN
C                 First, the relative humidity, variable 10 in SAMSON
                  IF( IDVAR(IV) .EQ. 10 )THEN
                     READ (JSAMSN(IALL,IHR),WFMT(10+5)) IRHUM(IHR)
                     IF( IRHUM(IHR) .GE. 990 ) THEN
                        IF( IHR .EQ. 24  .AND. ISMO(IHR).EQ.12  .AND.
     &                      ISDY(IHR).EQ.31 )THEN
                            IRHUM(IHR) =  IRHUM(IHR-1)
                        ELSE
                           IRHUM(IHR) = -99
                           WRITE( IDIAG,570 ) ISYR(IHR), ISMO(IHR),
     &                                        ISDY(IHR), ISHR(IHR)
                           NOWARN = .FALSE.
                        ENDIF
                     ENDIF
                  ENDIF
    
C                 Next, the global radiation, variable 3 in SAMSON
C                 Note that it is a 7-character field, but only the
C                   first 4 characters (all integers) are needed; the
C                   last 3 characters are source and uncertainty flags

                  IF( IDVAR(IV) .EQ. 3 )THEN
c                    READ (JSAMSN(IALL,IHR)(1:4),WFMT(3+5)) IGRAD(IHR)
                     READ (JSAMSN(IALL,IHR)(1:4),'(I4)') IGRAD(IHR)
                     IF( IGRAD(IHR) .GE. 9990 ) THEN
                        IF( IHR .EQ. 24  .AND. ISMO(IHR).EQ.12  .AND.
     &                      ISDY(IHR).EQ.31 )THEN
                            IGRAD(IHR) =  IGRAD(IHR-1)
                        ELSE
                           IGRAD(IHR) = 9999
                           WRITE( IDIAG,580 ) ISYR(IHR), ISMO(IHR),
     &                                        ISDY(IHR), ISHR(IHR)
                           NOWARN = .FALSE.
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF

C*       End Loops Over Variables
300      CONTINUE
600   CONTINUE

      RETURN

  500 FORMAT( ' FILSAM: Wind direction missing for (yy/mm/dd/hh) ',
     &         4(i2.2:,'/') )
  510 FORMAT( ' FILSAM: Wind speed missing for (yy/mm/dd/hh) ',
     &         4(i2.2:,'/') )
  520 FORMAT( ' FILSAM: Station pressure missing for (yy/mm/dd/hh) ',
     &         4(i2.2:,'/') )
  530 FORMAT( ' FILSAM: Temperature missing for (yy/mm/dd/hh) ',
     &         4(i2.2:,'/') )
  540 FORMAT( ' FILSAM: Cloud cover missing for (yy/mm/dd/hh) ',
     &         4(i2.2:,'/') )
  550 FORMAT( ' FILSAM: Ceiling height missing for (yy/mm/dd/hh) ',
     &         4(i2.2:,'/') )
  560 FORMAT( ' FILSAM: Precipitation missing for (yy/mm/dd/hh) ',
     &         4(i2.2:,'/') )
  570 FORMAT( ' FILSAM: Dry/Wet dep: SAMSON rel. humidity missing for',
     &        ' (yy/mm/dd/hh) ', 4(i2.2:,'/') )
  580 FORMAT( ' FILSAM: Dry/Wet dep: SAMSON global rad''n missing for',
     &        ' (yy/mm/dd/hh) ', 4(i2.2:,'/') )

      END

      SUBROUTINE FILHUS(IHR, NREAD, ASOSFL,ASOS1,ASOS2,ASOS3)
C***********************************************************************
C*    FILSAM Module of PCRAMMET Meteorological Pre-processor
C*
C*    PURPOSE:    Translates the Weather Variables from Character to
C*                Numeric for HUSWO data
C*
C*    PROGRAMMER: PES Inc.
C*
C*    DATE:       May 1998
C*
C*    INPUTS:     Surface Data from HUSWO
C*
C*    OUTPUTS:    24 Hour Arrays of Numeric Weather Data
C*
C*    Modifications:
C*       7/31/98 - Added code to process relative humidity and global
C*                 radiation from CD if dry or wet deposition selected
C*
C*    CALLED FROM: FILMET
C***********************************************************************
C*
C*    Variable Declarations
      INCLUDE 'PCRAM.INC'
      INTEGER     ASOS1, ASOS2, ASOS3
      CHARACTER*8 ATEMP
      CHARACTER*6 WFMT(MAXHUS)
      CHARACTER*1 ASOSFL

      DATA WFMT /'(I5)','(A1)','(I4)','(I2)',  '(I2)',  '(I2)','(I4)',
     &           '(I4)','(I2)','(I2)','(F5.0)','(F6.0)','(I3)','(I4)',
     &           '(I4)','(F4.0)','(F6.0)','(I5)','(A8)','(I5)',
     &           '(I5)','(I5)','(I4)','(I4)'/

C*    Initialize the ASOS flag to a blank (indicating NO ASOS data) and
C     the ASOS variables
      ASOSFL = ' '
      ASOS1 = 99999
      ASOS2 = 99999
      ASOS3 = 99999

C*    Obtain Station ID, ASOS Flag, Date and Time Info

      READ (JHUSWO(1,IHR),WFMT(1)) IDSFC          !STNID
      READ (JHUSWO(2,IHR),WFMT(2)) ASOSFL
      READ (JHUSWO(3,IHR),WFMT(3)) ISYR(IHR)
      READ (JHUSWO(4,IHR),WFMT(4)) ISMO(IHR)
      READ (JHUSWO(5,IHR),WFMT(5)) ISDY(IHR)
      READ (JHUSWO(6,IHR),WFMT(6)) ISHR(IHR)

C*    HUSWO format uses 4-digit year; convert to 2-digit year
      IF( ISYR(IHR) .GE. 1900  .AND.  ISYR(IHR) .LT. 2000 )THEN
         ISYR(IHR) = ISYR(IHR) - 1900
      ELSEIF( ISYR(IHR) .GE. 2000 )THEN
         ISYR(IHR) = ISYR(IHR) - 2000
      ENDIF


C*    Loop Over All the HUSWO variables
      DO 300 IV = 1,IFC
C*       If the Current Variable is Present, Then Process It

C*------ Opaque Sky Cover (tenths)
         IF (IDVAR(IV) .EQ. 6) THEN
            READ (JHUSWO(IV+4,IHR),WFMT(IDVAR(IV)+4)) ICOVER(IHR)
            IF( ICOVER(IHR) .EQ. 99 ) THEN
               IF( ASOSFL .EQ. ' ' )THEN
                  IF( IHR .EQ. 24  .AND. ISMO(IHR).EQ.12  .AND.
     &                ISDY(IHR).EQ.31 )THEN
                      ICOVER(IHR)  =  ICOVER(IHR-1)
                  ELSE
                      WRITE( IDIAG,540 ) ISYR(IHR), ISMO(IHR),
     &                                   ISDY(IHR), ISHR(IHR)
                      NOWARN = .FALSE.
                  ENDIF
               ENDIF
            ENDIF
         ENDIF

C*------ ASOS Cloud Cover and Ceiling (tenths and hundreds of feet)
C        IF field 16 is present, then fields 17 and 18 are also present

         IF (IDVAR(IV) .EQ. 16) THEN
            READ (JHUSWO(IV+4,IHR),WFMT(IDVAR(IV)+4)) ASOS1
            READ (JHUSWO(IV+5,IHR),WFMT(IDVAR(IV)+5)) ASOS2
            READ (JHUSWO(IV+6,IHR),WFMT(IDVAR(IV)+6)) ASOS3

            IF( ASOSFL .EQ. 'A'  )THEN
C*             Save the ASOS data for use as last hour of year
               SAVAS1 = ASOS1
               SAVAS2 = ASOS2
               SAVAS3 = ASOS3
               IF( ASOS1 .EQ. 99999 )THEN
                  IF( IHR .EQ. 24  .AND. ISMO(IHR).EQ.12  .AND.
     &                ISDY(IHR).EQ.31 )THEN
                      ASOS1  =  SAVAS1
                      ASOS2  =  SAVAS2
                      ASOS3  =  SAVAS3
                  ELSE
C                     The next message has been commented out since the
C                     subroutine DOCLDS, called from FILMET, will
C                     issue an identical message
c                     WRITE( IDIAG,545 ) ISYR(IHR), ISMO(IHR),
c    &                                   ISDY(IHR), ISHR(IHR)
                      NOWARN = .FALSE.
                  ENDIF
               ENDIF
            ENDIF
         ENDIF

C*------ Ceiling Height (feet)
         IF (IDVAR(IV) .EQ. 14) THEN
            READ (JHUSWO(IV+4,IHR),WFMT(IDVAR(IV)+4)) ICEIL(IHR)
            IF( ICEIL(IHR) .GT. 99990 )THEN
               IF( IHR .EQ. 24  .AND. ISMO(IHR).EQ.12  .AND.
     &             ISDY(IHR).EQ.31 )THEN
                   ICEIL(IHR)  =  ICEIL(IHR-1)
               ELSE
                  ICEIL(IHR) = 77777
                  WRITE( IDIAG,550 ) ISYR(IHR), ISMO(IHR),
     &                               ISDY(IHR), ISHR(IHR)
                  NOWARN = .FALSE.
               ENDIF
            ENDIF
         ENDIF

C*------ Pressure (hundredths of inches)
         IF (IDVAR(IV) .EQ. 10) THEN
            READ (JHUSWO(IV+4,IHR),WFMT(IDVAR(IV)+4)) IPRESS
            PRESS(IHR) = IPRESS
            IF( PRESS(IHR) .GT. 9990.0 )THEN
               IF( IHR .EQ. 24  .AND. ISMO(IHR).EQ.12  .AND.
     &             ISDY(IHR).EQ.31 )THEN
                   PRESS(IHR) =  PRESS(IHR-1)
               ELSE
                  WRITE( IDIAG,520 ) ISYR(IHR), ISMO(IHR),
     &                               ISDY(IHR), ISHR(IHR)
                  NOWARN = .FALSE.
               ENDIF
            ENDIF
         ENDIF

C*------ Wind Direction (degrees)
         IF (IDVAR(IV) .EQ. 11) THEN
            READ (JHUSWO(IV+4,IHR),WFMT(IDVAR(IV)+4)) IDIR(IHR)
            IF( IDIR(IHR) .EQ. 999 ) THEN
               IF( IHR .EQ. 24  .AND. ISMO(IHR).EQ.12  .AND.
     &             ISDY(IHR).EQ.31 )THEN
                   IDIR(IHR) =  IDIR(IHR-1)
               ELSE
                  WRITE( IDIAG,500 ) ISYR(IHR), ISMO(IHR),
     &                               ISDY(IHR), ISHR(IHR)
                  NOWARN = .FALSE.
               ENDIF
            ENDIF
         ENDIF

C*------ Wind Speed (miles per hour)
         IF (IDVAR(IV) .EQ. 12) THEN
            READ (JHUSWO(IV+4,IHR),WFMT(IDVAR(IV)+4)) WSPEED(IHR)
            IF( WSPEED(IHR) .GT. 99.0 ) THEN
               IF( IHR .EQ. 24  .AND. ISMO(IHR).EQ.12  .AND.
     &             ISDY(IHR).EQ.31 )THEN
                   WSPEED(IHR) =  WSPEED(IHR-1)
               ELSE
                  WSPEED(IHR) = 99.0
                  WRITE( IDIAG,510 ) ISYR(IHR), ISMO(IHR),
     &                               ISDY(IHR), ISHR(IHR)
                  NOWARN = .FALSE.
               ENDIF
            ENDIF
         ENDIF

C*------ Temperature (degrees Fahrenheit)
         IF (IDVAR(IV) .EQ. 7) THEN
            READ (JHUSWO(IV+4,IHR),WFMT(IDVAR(IV)+4)) TEMP(IHR)
            IF( TEMP(IHR) .GT. 990. )THEN
               IF( IHR .EQ. 24  .AND. ISMO(IHR).EQ.12  .AND.
     &             ISDY(IHR).EQ.31 )THEN
                   TEMP(IHR) =  TEMP(IHR-1)
               ELSE
                  TEMP(IHR) = 999.9
                  WRITE( IDIAG,530 ) ISYR(IHR), ISMO(IHR),
     &                               ISDY(IHR), ISHR(IHR)
                  NOWARN = .FALSE.
               ENDIF
            ENDIF
         ENDIF

C*------ Present Weather Code
         IF (IDVAR(IV) .EQ. 15) THEN
            READ (JHUSWO(IV+4,IHR),WFMT(IDVAR(IV)+4)) ATEMP
            DO 200 JJ = 1, 4
               READ (ATEMP(2*JJ-1:2*JJ),'(I2)') IWXHUS(JJ,IHR)
200         CONTINUE
         ENDIF

C*       Hourly Precip (inches and hundredths)
C*       Convert precip to millimeters here rather than in S.UNITS
         IF (IDVAR(IV) .EQ. 19) THEN
            READ (JHUSWO(IV+4,IHR),WFMT(IDVAR(IV)+4)) IPCP
            IF( IPCP .GT. 990.0 ) THEN
               IF( IHR .EQ. 24  .AND. ISMO(IHR).EQ.12  .AND.
     &             ISDY(IHR).EQ.31 )THEN
                   PRECIP(IHR) =  PRECIP(IHR-1)
               ELSE
                  WRITE( IDIAG,560 ) ISYR(IHR), ISMO(IHR),
     &                               ISDY(IHR), ISHR(IHR)
                  NOWARN = .FALSE.
               ENDIF
            ENDIF 

            IF( IPCP .LE. 990.0 ) THEN
               PRECIP(IHR) = ( IPCP /100.0 ) * 25.4
            ENDIF
         ENDIF

C        For dry and wet deposition, both relative humidity and 
C        GLOBAL radiation are required in the output file

         IF( DRYFLG  .OR.  WETFLG )THEN
C           First, the relative humidity, variable 10 in SAMSON
            IF( IDVAR(IV) .EQ. 9 )THEN
              READ (JHUSWO(IV+4,IHR),WFMT(IDVAR(IV)+4)) IRHUM(IHR)
               IF( IRHUM(IHR) .GE. 990 ) THEN
                  IF( IHR .EQ. 24  .AND. ISMO(IHR).EQ.12  .AND.
     &                ISDY(IHR).EQ.31 )THEN
                      IRHUM(IHR) =  IRHUM(IHR-1)
                  ELSE
                     IRHUM(IHR) = -99
                     WRITE( IDIAG,570 ) ISYR(IHR), ISMO(IHR),
     &                                  ISDY(IHR), ISHR(IHR)
                     NOWARN = .FALSE.
                  ENDIF
               ENDIF
            ENDIF
    
C           Next, the global radiation, variable 3 in SAMSON

            IF( IDVAR(IV) .EQ. 3 )THEN
              READ (JHUSWO(IV+4,IHR),WFMT(IDVAR(IV)+4)) IGRAD(IHR)
               IF( IGRAD(IHR) .GE. 9990 ) THEN
                  IF( IHR .EQ. 24  .AND. ISMO(IHR).EQ.12  .AND.
     &                ISDY(IHR).EQ.31 )THEN
                      IGRAD(IHR) =  IGRAD(IHR-1)
                  ELSE
                     IGRAD(IHR) = 9999
                     WRITE( IDIAG,580 ) ISYR(IHR), ISMO(IHR),
     &                                  ISDY(IHR), ISHR(IHR)
                     NOWARN = .FALSE.
                  ENDIF
               ENDIF
            ENDIF
         ENDIF

C*    End Loops Over Variables
300   CONTINUE

      RETURN

  500 FORMAT( ' FILHUS: Wind direction missing for (yy/mm/dd/hh) ',
     &         4(i2.2:,'/') )
  510 FORMAT( ' FILHUS: Wind speed missing for (yy/mm/dd/hh) ',
     &         4(i2.2:,'/') )
  520 FORMAT( ' FILHUS: Station pressure missing for (yy/mm/dd/hh) ',
     &         4(i2.2:,'/') )
  530 FORMAT( ' FILHUS: Temperature missing for (yy/mm/dd/hh) ',
     &         4(i2.2:,'/') )
  540 FORMAT( ' FILHUS: Cloud cover missing for (yy/mm/dd/hh) ',
     &         4(i2.2:,'/') )
  545 FORMAT( ' FILHUS: ASOS cloud data missing for (yy/mm/dd/hh) ',
     &         4(i2.2:,'/') )
  550 FORMAT( ' FILHUS: Ceiling height missing for (yy/mm/dd/hh) ',
     &         4(i2.2:,'/') )
  560 FORMAT( ' FILHUS: Precipitation missing for (yy/mm/dd/hh) ',
     &         4(i2.2:,'/') )
  570 FORMAT( ' FILSHUS: Dry/Wet dep: HUSWO rel. humidity missing for',
     &        ' (yy/mm/dd/hh) ', 4(i2.2:,'/') )
  580 FORMAT( ' FILSHUS: Dry/Wet dep: HUSWO global rad''n missing for',
     &        ' (yy/mm/dd/hh) ', 4(i2.2:,'/') )

      END


      SUBROUTINE FIL144(IHR)
C***********************************************************************
C*    FIL144 Module of PCRAMMET Meteorological Pre-processor
C*
C*    PURPOSE:    Translates the Character Weather Variables to
C*                Numeric for CD-144 or SCRAM
C*
C*    PROGRAMMER: Jayant Hardikar
C*                PES Inc.
C*
C*    DATE:       February 14, 1995
C*
C*    INPUTS:     Surface Data from CD144
C*
C*    OUTPUTS:    1 Hour of Numeric Weather Data
C*
C*    CALLED FROM: FILMET
C***********************************************************************
C*
C*    Variable Declarations
      INCLUDE 'PCRAM.INC'
      CHARACTER*5 CCC,DDD
      INTEGER     PCOUNT

      DATA PCOUNT /0/

C     1 = Station ID
C     2 = Year
C     3 = Month
C     4 = Day
C     5 = Hour
C     6 = Ceiling ht
C     7 = Present weather (missing in SCRAM)
C     8 = Wind direction
C     9 = Wind speed
C     10 = Station pressure (missing in SCRAM)
C     11 = Dry bulb temperature
C     12 = Opaque sky cover

C*    Obtain Station ID, Date and Time Info
      READ (ACD144(1,IHR),'(I5)') IDSFC
      READ (ACD144(2,IHR),'(I2)') IY
      READ (ACD144(3,IHR),'(I2)') IM
      READ (ACD144(4,IHR),'(I2)') ID
      READ (ACD144(5,IHR),'(I2)') IH

C*    Change Hour Zero to Hour 24 of Previous Day
      IF (IH .EQ. 0) THEN
         CALL JULIAN (IY,IM,ID,JD,0)
         CALL HR0024(IY,JD,IH)
         CALL JULIAN (IY,IM,ID,JD,1)
      ENDIF

C*    Save Date/Time Info In Appropriate Arrays
      ISYR(IHR) = IY
      ISMO(IHR) = IM
      ISDY(IHR) = ID
      ISHR(IHR) = IH

C*--- Decode Ceiling Height - Standard RAMMET Procedures
      CCC = ACD144(6,IHR)
      DDD = ACD144(12,IHR)
      CALL CCCODE(IDIAG,CCC,DDD,ICOVER(IHR),ICEIL(IHR),NOWARN)

C*    Save Other Variables in Appropriate Arrays

C*--- Present weather: only the 5 fields, in cols. 25-29 for CD-144,
C                      for liquid and frozen precipitation;
C                      for SCRAM data, these fields are blanks

      DO 100 J=1,5
         READ(ACD144(7,IHR)(J:J), '(I1)' ) IPREC(J,IHR)
  100 CONTINUE

C*    Missing value codes correspond to the codes used in SAMSON data

C*--- Wind direction
      IF( ACD144(8,IHR) .NE. '  ' )THEN
         READ (ACD144(8,IHR),'(I2)')   IDIR(IHR)
      ELSE
         WRITE( IDIAG, 500 ) IY, IM, ID, IH
         IDIR(IHR) = 999
         NOWARN = .FALSE.
      ENDIF

C*--- Wind speed
      IF( ACD144(9,IHR) .NE. '  ' )THEN
         READ (ACD144(9,IHR),'(f2.0)') WSPEED(IHR)
      ELSE
         WRITE( IDIAG, 510 ) IY, IM, ID, IH
         WSPEED(IHR) = 99.0
         NOWARN = .FALSE.
      ENDIF

C*--- Station pressure: needed only for dry or wet deposition
      IF( DRYFLG  .OR.  WETFLG )THEN
         IF( ACD144(10,IHR) .NE. '    ' )THEN
            READ (ACD144(10,IHR),'(f4.2)')PRESS(IHR)
         ELSE
C*          In SUBR.UNITS, the station pressure will be set to 1000mb
C           when the pressure is missing; the message is written only
C           for the first 500 occurrences of missing pressure
            PCOUNT = PCOUNT + 1
            IF( PCOUNT .LE. 500 )THEN
               WRITE( IDIAG, 520 ) IY, IM, ID, IH
            ENDIF
            PRESS(IHR) = 9999.0
            NOWARN = .FALSE.
         ENDIF
      ENDIF

C*--- Temperature
      IF( ACD144(11,IHR) .NE. '   ' )THEN
         READ (ACD144(11,IHR),'(F3.0)')  TEMP(IHR)
      ELSE
         WRITE( IDIAG, 530 ) IY, IM, ID, IH
         TEMP(IHR) = 999.9
         NOWARN = .FALSE.
      ENDIF

      RETURN

  500 FORMAT( ' FIL144: Wind direction missing for (yy/mm/dd/hh) ',
     &         4(i2.2:,'/') )
  510 FORMAT( ' FIL144: Wind speed missing for (yy/mm/dd/hh) ',
     &         4(i2.2:,'/') )
  520 FORMAT( ' FIL144: Station pressure missing for (yy/mm/dd/hh) ',
     &         3(i2.2:,'/'),i2.2, ', using 1000 mb')
  530 FORMAT( ' FIL144: Temperature missing for (yy/mm/dd/hh) ',
     &         4(i2.2:,'/') )

      END


      SUBROUTINE CCCODE(IDIAG,CCC,DDD,ISKY,IROOF,NOWARN)
C***********************************************************************
C*    CCCODE Module of PCRAMMET Meteorological Pre-processor
C*
C*    PURPOSE:    Decodes the Ceiling Height and Cloud Cover from the
C*                CD144 or SCRAM Data
C*
C*    PROGRAMMER: Jayant Hardikar
C*                PES Inc.
C*
C*    DATE:       February 23, 1995
C*
C*    INPUTS:     Surface Data from CD144 or SCRAM
C*
C*    OUTPUTS:    1 Hour Numeric Value for Ceiling and Cloud Cover
C*
C*    CALLED FROM: FIL144
C***********************************************************************
C*
C*    Variable Declarations
      CHARACTER*1 DIG(11), CTMP(3), CC
      CHARACTER*5 CCC,DDD
      INTEGER IDG(3)
      LOGICAL NOWARN
      DATA DIG /'0','1','2','3','4','5','6','7','8','9','-'/

C*    Decode Cloud Cover - if it is missing (i.e., blank) or any invalid
C*    character, set to 10/10
      READ (DDD(1:1),'(A1)') CC

80    DO 90 JK=1,11
         IF (CC .EQ. DIG(JK)) GO TO 100
90    CONTINUE
      JK=11
      WRITE (IDIAG,530) CC
530   FORMAT (' CCCODE: The character (',A1,') is not allowable.',
     &        ' Cloud cover defaults to 10.')
      NOWARN = .FALSE.
100   ISKY=JK-1


C*    Decode Ceiling Height
      IF( CCC(1:1) .EQ. '-'  .OR.  CCC(1:3) .EQ. '   ' )THEN
         IDG(1)=9
         IDG(2)=9
         IDG(3)=8

      ELSE
         DO 20 JJ= 1,3
            READ (CCC(JJ:JJ),'(A1)') CTMP(JJ)
20       CONTINUE

110      DO 140 JI=1,3
            DO 120 JK=1,10
               IF (CTMP(JI).EQ.DIG(JK)) GO TO 130
120         CONTINUE
130         IDG(JI)=JK-1
140      CONTINUE
      ENDIF

C*    Compute ceiling height in hundreds of feet.
150   IROOF = IDG(1)*100 + IDG(2)*10 + IDG(3)

      RETURN
      END


      SUBROUTINE DOCLDS(IY,IM,ID,IH,IDIAG,ASOS1,ASOS2,ASOS3,ICCVR,ICEIL)
C***********************************************************************
C*    DOCLDS Module of PCRAMMET Meteorological Pre-processor
C*
C*    PURPOSE:    Decodes the ASOS Cloud Data
C*
C*    PROGRAMMER: PES Inc.
C*
C*    DATE:       May 1998
C*
C*    INPUTS:     3 levels of ASOS cloud data of the form XXYYY
C*                where XX = cloud coverage, YYY = ceiling in hundreds
C*                of feet
C*
C*    OUTPUTS:    1 Hour Numeric Value for Ceiling and Cloud Cover
C*
C*    CALLED FROM: FILMET
C***********************************************************************
C*
C*    Variable Declarations
      INTEGER  ACC1,ACC2,ACC3,ACHT1,ACHT2,ACHT3,ASOS1,ASOS2,ASOS3
      INTEGER  MAXCLD, ICCVR, ICEIL

      MAXCLD = 99
      ICCVR  = 99
      ICEIL  = 77777


      IF( ASOS1 .NE. 99999 )THEN
         ACC1  = INT(ASOS1/1000.0)
         ACHT1 = ASOS1 - ACC1*1000
         MAXCLD = ACC1
         IF( MAXCLD .GT. 02 )THEN
            ICEIL = ACHT1
         ENDIF
      ENDIF

      IF( ASOS2 .NE. 99999 )THEN
         ACC2  = INT(ASOS2/1000.0)
         ACHT2 = ASOS2 - ACC2*1000
         IF( ACC2 .NE. 99  .AND.  ACC2 .GT. MAXCLD )THEN
            MAXCLD = ACC2
            IF( MAXCLD .GT. 02 .AND. ICEIL .NE. 77777 )THEN
               ICEIL = ACHT2
            ENDIF
         ENDIF

      ENDIF

      IF( ASOS3 .NE. 99999 )THEN
         ACC3  = INT(ASOS3/1000.0)
         ACHT3 = ASOS3 - ACC3*1000
         IF( ACC3 .NE. 99  .AND.  ACC3 .GT. MAXCLD )THEN
            MAXCLD = ACC3
            IF( MAXCLD .GT. 02 .AND. ICEIL .NE. 77777 )THEN
               ICEIL = ACHT3
            ENDIF
         ENDIF
      ENDIF


C---- Convert the ASOS sky condition codes to  fractional cloud cover
C     (tenths).  Note that the VALID codes are 0, 2, 4, 6, 7, 9, and 99.
C     The remaining codes (1, 3, 5, 8, and 10) are not valid according to
C     the HUSWO documentation, but are allowed to pass through as 1/10,
C     3/10, etc. (per conversation with Dennis Atkinson, USEPA, 10/30/98).
C     These codes _may_ be augmented by human observers or errors, but 
C     there is no way to determine this.

      IF( MAXCLD .EQ. 0 )THEN
C*       A valid ASOS Sky Condition Value
         ICCVR = 0
      ELSEIF( MAXCLD .EQ. 01 )THEN
C*       Not a valid ASOS Sky Condition Value
         ICCVR = 1
      ELSEIF( MAXCLD .EQ. 02 )THEN
C*       A valid ASOS Sky Condition Value
         ICCVR = 3
      ELSEIF( MAXCLD .EQ. 03 )THEN
C*       Not a valid ASOS Sky Condition Value
         ICCVR = 3
      ELSEIF( MAXCLD .EQ. 04 )THEN
C*       A valid ASOS Sky Condition Value
         ICCVR = 7
      ELSEIF( MAXCLD .EQ. 05 )THEN
C*       Not a valid ASOS Sky Condition Value
         ICCVR = 5
      ELSEIF( MAXCLD .EQ. 06 )THEN
C*       A valid ASOS Sky Condition Value
         ICCVR = 10
      ELSEIF( MAXCLD .EQ. 07 )THEN
C*       A valid ASOS Sky Condition Value
         ICCVR = 10
      ELSEIF( MAXCLD .EQ. 08 )THEN
C*       Not a valid ASOS Sky Condition Value
         ICCVR = 8
      ELSEIF( MAXCLD .EQ. 09 )THEN
C*       A valid ASOS Sky Condition Value
         ICCVR = 99
         WRITE( IDIAG, 500 ) IY, IM, ID, IH
      ELSEIF( MAXCLD .EQ. 10 )THEN
C*       A valid ASOS Sky Condition Value
         ICCVR = 10
      ELSEIF( MAXCLD .EQ. 99 )THEN
         ICCVR = 99
         WRITE( IDIAG, 510 ) IY, IM, ID, IH

      ENDIF

500   FORMAT( ' DOCLDS: ASOS cloud cover unknown for (yy/mm/dd/hh) ',
     &         4(i2.2:,'/') )
510   FORMAT( ' DOCLDS: ASOS cloud data missing for (yy/mm/dd/hh) ',
     &         4(i2.2:,'/') )


      RETURN
      END

      SUBROUTINE PCODES(IDIAG,IDATE,IPREC,IPCODE,NOWARN,SFTYPE)
C***********************************************************************
C*    PCODES Module of PCRAMMET Meteorological Pre-processor
C*
C*    PURPOSE:    Convert CD144 precipitation type data to two-digit
C*                precipitation codes; SCRAM has blanks (zeroes) for
C*                these fields
C*
C*    PROGRAMMER: Jayant Hardikar
C*                PES Inc.
C*                Adapted From DEPMET Version: 1.0 Level: 931108
C*                                           : J. Chang, SRC
C*
C*    DATE:       February 23, 1995
C*
C*    INPUTS:
C*            IDATE - integer    - Date for this hour YYJJJHH
C*            IPREC - int. array - Precipitation TYPE data
C*
C*    OUTPUTS:
C*           IPCODE - integer    - Two-digit precipitation CODE
C*
C*    REFERENCE: Press W.H. et al., 1986: Numerical Recipes, Cambridge
C*               Univ. Press, 818pp.
C*
C*    CALLED FROM: MAIN program
C***********************************************************************

C*    VARIABLE DECLARATIONS
      INTEGER      IDATE,IPREC(5),IPCODE
      CHARACTER*6  SFTYPE
      LOGICAL      NOWARN

      KT = IPREC(1) + IPREC(2) + IPREC(3) + IPREC(4) + IPREC(5)
      IF(KT.EQ.0) THEN
        IPCODE = 0
        GO TO 99
      ELSE
        ILIQ = 0
        IFROZ = 0
        DO 5 I = 5,1,-1
          IP = IPREC(I)
          IF(IP.EQ.0) GO TO 5
C*---     IPCODE IS THE PRECIPITATION CODE (0-45)
          IPCODE = (I-1) * 9 + IP
          IF(IPCODE.LT.19)THEN
            ILIQ = 1
          ELSE
            IFROZ = 1
          ENDIF
5       CONTINUE
      ENDIF

C*--- CHECK FOR MORE THAN ONE TYPE OF PRECIPITATION REPORTED FOR THE SAME HOUR
      IF(ILIQ.EQ.1 .AND. IFROZ.EQ.1)THEN
         WRITE(IDIAG,1000) IDATE,SFTYPE,IPREC,IPCODE
1000     FORMAT(' Warning -- more than one type of precipitation ',
     1          'reported on (YYDDDHH):(',I7,')',/,15X,
     2          A6,' TYPE = ',5I1,3X,'precip. code used = ',I2)
         NOWARN = .FALSE.
      ENDIF

99    RETURN
      END


      SUBROUTINE SAMWX(IWETH,IP5)
C***********************************************************************
C*    SAMWX Module of PCRAMMET Meteorological Pre-processor
C*
C*    PURPOSE:    Converts SAMSON present weather codes into
C*                CD144 weather observations (columns 25-29)
C*                as best as possible
C*
C*    PROGRAMMER: Jayant Hardikar
C*                PES Inc.
C*                Adapted From DEPMET Version: 1.0 Level: 931108
C*                                           : J. Chang, SRC
C*
C*    DATE:       February 23, 1995
C*
C*    INPUTS:
C*       IWETH - int. array - Precipitation TYPE data
C*
C*    OUTPUTS:
C*       IP5    - integer    - Two-digit precipitation CODE
C*
C*    CALLED FROM: MAIN program
C***********************************************************************

C*    Variable Declarations
C*    IPREC    = COLUMNS 25-29 OF NOAA WEATHER CODE(IN CHAR FORM)
C*    IWETH    = SAMSON PRESENT WEATHER CODE ARRAY
      INTEGER IWETH(9),IP5(5)

C*    CLASSIFY LIQUID PRECIPITATION
      IF (IWETH(2) .EQ. 0) IP5(1) = 1
      IF (IWETH(2) .EQ. 1) IP5(1) = 2
      IF (IWETH(2) .EQ. 2) IP5(1) = 3
      IF (IWETH(2) .EQ. 3) IP5(1) = 4
      IF (IWETH(2) .EQ. 4) IP5(1) = 5
      IF (IWETH(2) .EQ. 5) IP5(1) = 6
      IF (IWETH(2) .EQ. 6) IP5(1) = 7
      IF (IWETH(2) .EQ. 7) IP5(1) = 8
      IF (IWETH(2) .EQ. 8) IP5(1) = 9
      IF (IWETH(2) .EQ. 9) IP5(1) = 0

      IF (IWETH(3) .EQ. 0) IP5(2) = 1
      IF (IWETH(3) .EQ. 1) IP5(2) = 2
      IF (IWETH(3) .EQ. 3) IP5(2) = 4
      IF (IWETH(3) .EQ. 4) IP5(2) = 5
      IF (IWETH(3) .EQ. 5) IP5(2) = 6
      IF (IWETH(3) .EQ. 6) IP5(2) = 7
      IF (IWETH(3) .EQ. 7) IP5(2) = 8
      IF (IWETH(3) .EQ. 8) IP5(2) = 9
      IF (IWETH(3) .EQ. 9) IP5(2) = 0

C*    CLASSIFY SNOW
      IF (IWETH(4) .EQ. 0) IP5(3) = 1
      IF (IWETH(4) .EQ. 1) IP5(3) = 2
      IF (IWETH(4) .EQ. 2) IP5(3) = 3
      IF (IWETH(4) .EQ. 3) IP5(3) = 4
      IF (IWETH(4) .EQ. 4) IP5(3) = 5
      IF (IWETH(4) .EQ. 5) IP5(3) = 6
      IF (IWETH(4) .EQ. 6) IP5(3) = 7
      IF (IWETH(4) .EQ. 7) IP5(3) = 8
      IF (IWETH(4) .EQ. 8) IP5(3) = 9
      IF (IWETH(4) .EQ. 9) IP5(3) = 0

      IF (IWETH(5) .EQ. 0) IP5(4) = 1
      IF (IWETH(5) .EQ. 1) IP5(4) = 2
      IF (IWETH(5) .EQ. 2) IP5(4) = 3
      IF (IWETH(5) .EQ. 3) IP5(4) = 4
      IF (IWETH(5) .EQ. 4) IP5(4) = 5
      IF (IWETH(5) .EQ. 5) IP5(4) = 6
      IF (IWETH(5) .EQ. 9) IP5(4) = 0

      IF (IWETH(6) .EQ. 0) IP5(5) = 1
      IF (IWETH(6) .EQ. 1) IP5(5) = 2
      IF (IWETH(6) .EQ. 2) IP5(5) = 3
      IF (IWETH(6) .EQ. 4) IP5(5) = 5
      IF (IWETH(6) .EQ. 9) IP5(5) = 0

      RETURN
      END


      SUBROUTINE HUSWX(IWETH,IP5)
C***********************************************************************
C*    HUSWX Module of PCRAMMET Meteorological Pre-processor
C*
C*    PURPOSE:    Converts SAMSON present weather codes into
C*                CD144 weather observations (columns 25-29)
C*                as best as possible
C*
C*    PROGRAMMER: PES Inc.
C*
C*    DATE:       May 1998
C*
C*    INPUTS:
C*       IWETH - integer array - Precipitation TYPE data
C*
C*    OUTPUTS:
C*       IP5   - integer array - Two-digit precipitation CODE
C*
C*    CALLED FROM: MAIN program
C***********************************************************************

C*    Variable Declarations
C*    IPREC    = COLUMNS 25-29 OF NOAA WEATHER CODE(IN CHAR FORM)
C*    IWETH    = HUSWO PRESENT WEATHER ARRAY
      INTEGER     IWETH(4),IP5(5)


      IP5(1) = 0
      IP5(2) = 0
      IP5(3) = 0
      IP5(4) = 0
      IP5(5) = 0

C     If the entire present weather filed is filled with 9's, or possibly
C     00009999, then there was no weather; make a quick check and return
C     if this condition exists
      IF( (IWETH(1) .eq. 99  .or.  IWETH(1) .eq. 00) .and.
     &    (IWETH(2) .eq. 99  .or.  IWETH(2) .eq. 00) .and.
     &    (IWETH(3) .eq. 99  .or.  IWETH(3) .eq. 00) .and.
     &    (IWETH(4) .eq. 99  .or.  IWETH(4) .eq. 00) ) THEN

         RETURN

      ELSE
C*       Only 4 weather groups are in the HUSWO data; to use subr.PCODES
C        to determine the precip codes, the 5th element in the IP5 array
C        is retained and set to zero

         IP5(5) = 0

         DO I=1,4
C*          CLASSIFY LIQUID PRECIPITATION
            IF (IWETH(i) .EQ. 20) IP5(i) = 1
            IF (IWETH(i) .EQ. 21) IP5(i) = 2
            IF (IWETH(i) .EQ. 22) IP5(i) = 3
            IF (IWETH(i) .EQ. 23) IP5(i) = 4
            IF (IWETH(i) .EQ. 24) IP5(i) = 5
            IF (IWETH(i) .EQ. 25) IP5(i) = 6
            IF (IWETH(i) .EQ. 26) IP5(i) = 7
            IF (IWETH(i) .EQ. 27) IP5(i) = 8
            IF (IWETH(i) .EQ. 28) IP5(i) = 9
            IF (IWETH(i) .EQ. 29) IP5(i) = 0

            IF (IWETH(i) .EQ. 30) IP5(i) = 1
            IF (IWETH(i) .EQ. 31) IP5(i) = 2
            IF (IWETH(i) .EQ. 33) IP5(i) = 4
            IF (IWETH(i) .EQ. 34) IP5(i) = 5
            IF (IWETH(i) .EQ. 35) IP5(i) = 6
            IF (IWETH(i) .EQ. 36) IP5(i) = 7
            IF (IWETH(i) .EQ. 37) IP5(i) = 8
            IF (IWETH(i) .EQ. 38) IP5(i) = 9
            IF (IWETH(i) .EQ. 39) IP5(i) = 0

C*          CLASSIFY FROZEN PRECIPITATION
            IF (IWETH(i) .EQ. 40) IP5(i) = 1
            IF (IWETH(i) .EQ. 41) IP5(i) = 2
            IF (IWETH(i) .EQ. 42) IP5(i) = 3
            IF (IWETH(i) .EQ. 43) IP5(i) = 4
            IF (IWETH(i) .EQ. 44) IP5(i) = 5
            IF (IWETH(i) .EQ. 45) IP5(i) = 6
            IF (IWETH(i) .EQ. 46) IP5(i) = 7
            IF (IWETH(i) .EQ. 47) IP5(i) = 8
            IF (IWETH(i) .EQ. 48) IP5(i) = 9
            IF (IWETH(i) .EQ. 49) IP5(i) = 0

            IF (IWETH(i) .EQ. 50) IP5(i) = 1
            IF (IWETH(i) .EQ. 51) IP5(i) = 2
            IF (IWETH(i) .EQ. 52) IP5(i) = 3
            IF (IWETH(i) .EQ. 53) IP5(i) = 4
            IF (IWETH(i) .EQ. 54) IP5(i) = 5
            IF (IWETH(i) .EQ. 55) IP5(i) = 6
            IF (IWETH(i) .EQ. 59) IP5(i) = 0

            IF (IWETH(i) .EQ. 60) IP5(i) = 1
            IF (IWETH(i) .EQ. 61) IP5(i) = 2
            IF (IWETH(i) .EQ. 62) IP5(i) = 3
            IF (IWETH(i) .EQ. 64) IP5(i) = 5
            IF (IWETH(i) .EQ. 69) IP5(i) = 0

            IF (IWETH(i) .EQ. 90) IP5(i) = 1
            IF (IWETH(i) .EQ. 91) IP5(i) = 2
            IF (IWETH(i) .EQ. 92) IP5(i) = 3
            IF (IWETH(i) .EQ. 99) IP5(i) = 0
         ENDDO
      ENDIF
      RETURN
      END


      SUBROUTINE READMX
C***********************************************************************
C*    READMIX Module of PCRAMMET Meteorological Pre-processor
C*
C*    PURPOSE:    Reads Mixing Height Data - AM and PM
C*
C*    PROGRAMMER: Jayant Hardikar
C*                PES Inc.
C*
C*    DATE:       February 14, 1995
C*
C*    INPUTS:     SCRAM Mixing Height File
C*
C*    OUTPUTS:    Morning and Afternoon Mixing Heights
C*
C*    CALLED FROM: MAIN program
C***********************************************************************

C*    Variable Declarations
C*    (IMX is Used to Check If This Routine is Being Called
C*    for the Very First Time or Not)
      SAVE IMX
      INCLUDE 'PCRAM.INC'
      CHARACTER*35 DUMMY

      DATA IMX/0/

C*    For The Very First Day Read Previous Days And Next
C*    Days Mixing Heights As Well
      IMX = IMX + 1
      IF (IMX .EQ. 1) THEN

C*       Look To See If a Header Is Present
         READ(INMIX,'(A35)') DUMMY

C*       If It Is, Then Skip It and Read Another Line, Otherwise,
C*       Start Reading from This Line.
         IF (DUMMY(8:11)  .EQ. '    '   .AND.
     &       DUMMY(12:12) .EQ. ' '      .AND.
     &       DUMMY(31:35) .EQ. '     ' ) THEN

            READ (INMIX,440,ERR=1039) IDMIX(1),IYRMIX(1),IMOMIX(1),
     &                           IDYMIX(1),AMMIX(1),PMMIX(1)
440         FORMAT (I5,3I2,1X,F5.0,13X,F5.0)
         ELSE
            READ (DUMMY,440,ERR=1039) IDMIX(1),IYRMIX(1),IMOMIX(1),
     &                           IDYMIX(1),AMMIX(1),PMMIX(1)

         ENDIF

C*       Read Current Day and Next Day
         READ (INMIX,440,ERR=1039) IDMIX(2),IYRMIX(2),IMOMIX(2),
     &                           IDYMIX(2),AMMIX(2),PMMIX(2)
         READ (INMIX,440,ERR=1039) IDMIX(3),IYRMIX(3),IMOMIX(3),
     &                           IDYMIX(3),AMMIX(3),PMMIX(3)

      ELSE

C*       Make this Previous Day = Last Current Day
C*       Make this Current  Day = Last Next Day
         AMMIX(1) = AMMIX(2)
         PMMIX(1) = PMMIX(2)
         IDMIX(1) = IDMIX(2)
         IYRMIX(1) = IYRMIX(2)
         IMOMIX(1) = IMOMIX(2)
         IDYMIX(1) = IDYMIX(2)
         AMMIX(2) = AMMIX(3)
         PMMIX(2) = PMMIX(3)
         IDMIX(2) = IDMIX(3)
         IYRMIX(2) = IYRMIX(3)
         IMOMIX(2) = IMOMIX(3)
         IDYMIX(2) = IDYMIX(3)

C*       Read the Next Day
         READ (INMIX,440,ERR=1039) IDMIX(3),IYRMIX(3),IMOMIX(3),
     &                        IDYMIX(3),AMMIX(3),PMMIX(3)

      ENDIF
      GO TO 999

C*    Error Handling
1039  CALL ERRHDL('E','1039',0)

999   RETURN
      END


      SUBROUTINE READPP
C***********************************************************************
C*    READPPT Module of PCRAMMET Meteorological Pre-processor
C*
C*    PURPOSE:    Reads Hourly Preciptitation Data from the
C*                TD3240 File
C*
C*    PROGRAMMER: Jayant Hardikar
C*                PES Inc.
C*
C*    DATE:       February 14, 1995
C*
C*    INPUTS:     TD3240 Precip
C*
C*    OUTPUTS:    24 Hours of Precip Data
C*
C*    CALLED FROM: MAIN program
C***********************************************************************

C*    Variable Declarations
      INCLUDE 'PCRAM.INC'

C*    If PPTFLG Is TRUE, Read From TD3240
      IF (PPTFLG) THEN

C*       Loop Over 24 Hours for This Day
         DO 100 IHR = 1,24

C*          Determine The Period For Which Data Is Needed
C*          Using Surface Data
            IF (SFCTYP .EQ. 'CD144') THEN

               READ (ACD144(2,IHR),'(I5)') IY
               READ (ACD144(3,IHR),'(I5)') IM
               READ (ACD144(4,IHR),'(I5)') ID
               READ (ACD144(5,IHR),'(I5)') IH

               IF (IH .EQ. 0) THEN
                  CALL JULIAN (IY,IM,ID,JD,0)
                  CALL HR0024(IY,JD,IH)
                  CALL JULIAN (IY,IM,ID,JD,1)
               ENDIF

            ELSE IF (SFCTYP .EQ. 'SAMSON') THEN
               READ (JSAMSN(1,IHR),'(I2)') IY
               READ (JSAMSN(2,IHR),'(I2)') IM
               READ (JSAMSN(3,IHR),'(I2)') ID
               READ (JSAMSN(4,IHR),'(I2)') IH

            ENDIF

C*          Code Date Variable
            CALL JULIAN (IY,IM,ID,JD,0)
            NDATE = IY*100000+JD*100+IH

C*          Get an Hour of Precip Data
            CALL UNCDP(INPPT,NDATE,24,1,PMM,ICODE,IDIAG)
            P3240(IHR) = PMM
            IY3240(IHR) = IY
            IM3240(IHR) = IM
            ID3240(IHR) = ID

100      CONTINUE
      ENDIF
      RETURN
      END


      SUBROUTINE CHKCLM(IHR)
C***********************************************************************
C*    CHKCLM Module of PCRAMMET Meteorological Pre-processor
C*
C*    PURPOSE:    This routine checks if the current hour is calm
C*
C*    PROGRAMMER: Jayant Hardikar
C*                PES Inc.
C*
C*    DATE:       February 23, 1995
C*
C*    INPUTS:     Wind speed
C*
C*    OUTPUTS:    Logical (T or F) indicating calm conditions
C*
C*    CALLED FROM: MAIN program
C***********************************************************************

C*    Variable Declarations
      INCLUDE 'PCRAM.INC'

C*    If The Wind Speed Is Zero, Then
      IF (WSPEED(IHR) .EQ. 0.0) THEN
         CALM(IHR) = .TRUE.
      ELSE
         CALM(IHR) = .FALSE.
      ENDIF

      RETURN
      END


      SUBROUTINE CHKDAT(IHR)
C***********************************************************************
C*    CHKDAT      Module of PCRAMMET Meteorological Pre-processor
C*
C*    PURPOSE:    Checks the Date and Time Stamps Between All Surface
C*                Mixing Height (and Precip Data)
C*
C*    PROGRAMMER: Jayant Hardikar
C*                PES Inc.
C*
C*    DATE:       February 16, 1995
C*
C*    INPUTS:     Various Date amd Time Stamps
C*
C*    OUTPUTS:    none - program terminates on error
C*
C*    CALLED FROM: MAIN program
C***********************************************************************
C*
C*    Variable Declarations
      INCLUDE 'PCRAM.INC'

C*    Check To See If The Surface Data Time Stamp Matches
C*    The Upper Air Data Time Stamp
      IF (ISYR(IHR) .NE. IYRMIX(2) .OR.
     &    ISMO(IHR) .NE. IMOMIX(2) .OR.
     &    ISDY(IHR) .NE. IDYMIX(2)) THEN
          GO TO 1040
      ENDIF

C*    If Using Precip Data...
C*    A check on dates for precipitation is not necessary because
C*    the year, month, day and hour come from the CD144 or SAMSON data
C*    which is then provided to the routines that fetch the precip for
C*    the requested hour.

      GO TO 999

C*    Error Handling
1040  CALL ERRHDL('E','1040',1)
1041  CALL ERRHDL('E','1041',1)

999   RETURN
      END


      SUBROUTINE FLOVEC(IHR,JDAY)
C***********************************************************************
C*    FLOVEC Module of PCRAMMET Meteorological Pre-processor
C*
C*    PURPOSE: This Routine Converts The Meteorological Wind Direction
C*             To Flow Vector and Also Calculates Random Flow Vector
C*
C*    PROGRAMMER: Jayant Hardikar
C*                PES Inc.
C*
C*    DATE:       February 23, 1995
C*
C*    INPUTS:     Wind direction, array of random numbers
C*
C*    OUTPUTS:    Flow vector for the hour
C*
C*    CALLED FROM: MAIN program
C***********************************************************************

C*    Variable Declarations
      INCLUDE 'PCRAM.INC'
      INTEGER KRAND
      COMMON /BDATA/ IRAND(24,366)

C*    Calculate flow vector and random flow vector only if the
C*    wind direciton is missing.

C*    Only winds from CD144 format need to be multiplied by 10;
C*    the SAMSON data are reported to the nearest 10 degrees, but
C*    in whole degrees.

      IF( IDIR(IHR) .LT. 900 )THEN

         IF( SFCTYP .EQ. 'CD144'  .OR.  SFCTYP .EQ. 'SCRAM' )THEN
            XDIR = IDIR(IHR)*10.0
         ELSEIF( SFCTYP .EQ. 'SAMSON'  .OR.  SFCTYP .EQ. 'HUSWO' )THEN
            XDIR = IDIR(IHR)
         ENDIF

         IF (XDIR.GT.180.0) THEN
            FV=XDIR-180.0
         ELSE
            FV=XDIR+180.0
         ENDIF
         AFV(IHR)=FV

C*       IHR AND JDAY, THE LOOP CONTROL VARIABLES, ARE BEING USED AS
C*       INDICES INTO THE RANDOM NUMBER ARRAY KRAND(24,366).
C*       IRAND IS SINGLE DIGIT FROM 0 TO 9.

         KRAND = IRAND(IHR,JDAY)
         FVR(IHR) = FV + KRAND - 4.0
         IF (FVR(IHR).GT.360.0) FVR(IHR)=FVR(IHR)-360.0

      ELSE
         FVR(IHR) = 999.0

      ENDIF

      RETURN
      END


      SUBROUTINE UNITS(IHR)
C***********************************************************************
C*    UNITS Module of PCRAMMET Meteorological Pre-processor
C*
C*    PURPOSE:    This Routine Converts All Met Variables to Appropriate
C*                Units; it also converts any missing values to a
C*                nonmissing value for selected variables
C*
C*    PROGRAMMER: Jayant Hardikar
C*                PES Inc.
C*
C*    DATE:       April 28, 1995
C*
C*    INPUTS:     Various Parameters
C*
C*    OUTPUTS:    Same parameters converted to required units
C*
C*    CALLED FROM: MAIN program
C***********************************************************************

C*    Variable Declarations
      INCLUDE 'PCRAM.INC'

      IF( SFCTYP .EQ. 'CD144'  .OR.  SFCTYP .EQ. 'SCRAM')THEN

C*       CONVERT TEMP FROM FAHRENHEIT TO KELVIN
         IF( TEMP(IHR) .LT. 9000.0 )THEN
            TEMP(IHR) = 0.5556 * (TEMP(IHR)-32.0) + 273.15
         ENDIF

C*       CONVERT WIND SPEED FROM KNOTS TO METERS/SEC,
C*       but save the wind speed in knots (integer) for use in
C*       computing stability category
         KSPEED(IHR) = WSPEED(IHR) + 0.01
         IF( KSPEED(IHR) .LT. 98 )THEN
            WSPEED(IHR) = WSPEED(IHR)*0.51444
C*          IF Wind Speed < 1.0 m/s, Wind Speed Is Set To 1 m/s
            IF (WSPEED(IHR) .LT. 1.0) WSPEED(IHR)=1.0
         ENDIF

C*       Convert cloud cover from tenths to fraction
C*       Note that a missing cloud cover is assumed to be 10/10
C        as performed in SUBR.CCCODE
         FCOVER(IHR) = 0.1 * FLOAT(ICOVER(IHR))

C*       Convert surface pressure from inches hg to mb
C*       (33.864 mb = 1 inch hg)
         IF( PRESS(IHR) .LT. 9000.0 )THEN
            PRESS(IHR) = PRESS(IHR) * 33.864
         ELSE
            PRESS(IHR) = 1000.0
         ENDIF

      ELSEIF( SFCTYP .EQ. 'SAMSON' )THEN

C*       Ceiling height must be converted back to hundreds of feet;
C*       77777 = unlimited, 88888 = ciroform, 999999 = missing, 
C*       but the range of valid values is 0 - 30450 meters (99900 feet)
         IF( ICEIL(IHR) .EQ. 77777  .OR.  ICEIL(IHR) .GT. 30450 )THEN
C           Unlimited ceiling
            ICEIL(IHR) = 998
         ELSE
            ICEIL(IHR) = NINT( ( ICEIL(IHR) / 0.3048 ) / 100.0 )
         ENDIF

C*       Convert temp from celsius to Kelvin
C*       For the SAMSON results to be consistent with the CD144 results,
C*       temperature is converted from celsius to fahrenheit then back
C*       to celsius
         IF( TEMP(IHR) .LT. 9000.0 )THEN
            TFAHR     = NINT(TEMP(IHR)/0.5556 + 32.0)
            TEMP(IHR) = 0.5556*(TFAHR-32.0) + 273.15
         ENDIF

C*       Wind speed (WSPEED) is already in meters/sec, but we need knots
C*       for stability class (KSPEED);
C*       Convert the KSPEED back to m/s so the SAMSON results are
C*       consistent with the CD-144 results
         IF( WSPEED(IHR) .LT. 99.0 )THEN
            KSPEED(IHR) = NINT(WSPEED(IHR)/0.51444)
            WSPEED(IHR) = KSPEED(IHR) * 0.51444

C*          IF Wind Speed < 1.0 m/s, Wind Speed Is Set To 1 m/s
            IF (WSPEED(IHR) .LT. 1.0) WSPEED(IHR)=1.0
         ENDIF

C*       Convert cloud cover from tenths to fraction
         IF( ICOVER(IHR) .LT. 90 )THEN
            FCOVER(IHR) = 0.1*FLOAT(ICOVER(IHR))
         ELSE
            FCOVER(IHR) = 1.0
         ENDIF

C*       Surface pressure is already in millibars, but check for missing
C        and set it to 1000 mb
         IF( PRESS(IHR) .GT. 9000.0 )THEN
            PRESS(IHR) = 1000.0
         ENDIF

C*       Precipitation - already converted to millimeters (in S.FILSAM),
C*       but check for missing
         IF( PRECIP(IHR) .GT. 9999.0 )THEN
            PRECIP(IHR) = 0.0
         ENDIF

      ELSEIF( SFCTYP .EQ. 'HUSWO' )THEN

C*       Ceiling height must be converted frpm feet to hundreds of feet
C*       77777 = unlimited, 88888 = ciroform, 99999 = missing, 
C*       but the valid range of values is 00000 - 99900 feet
         IF( ICEIL(IHR) .EQ. 77777  .OR.  ICEIL(IHR) .GT. 99900 )THEN
C           Unlimited ceiling
            ICEIL(IHR) = 998
         ELSE
            ICEIL(IHR) = NINT( ( ICEIL(IHR) ) / 100.0 )
         ENDIF

C*       Convert temp from Fahrenheit to Kelvin
         IF( TEMP(IHR) .LT. 900.0 )THEN
            TEMP(IHR) = 0.5556*(TEMP(IHR)-32.0) + 273.15
         ENDIF

C*       Wind speed is in miles per hour, but we need knots for
C*       stability class and meters/second for all other calculations
         IF( WSPEED(IHR) .LT. 99.0 )THEN
            KSPEED(IHR) = NINT(WSPEED(IHR)/1.15)
            WSPEED(IHR) = WSPEED(IHR) / 2.237

C*          IF Wind Speed < 1.0 m/s, Wind Speed Is Set To 1 m/s
            IF (WSPEED(IHR) .LT. 1.0) WSPEED(IHR)=1.0
         ELSE
            KSPEED(IHR) = 99
         ENDIF

C*       Convert cloud cover from tenths to fraction
         IF( ICOVER(IHR) .LT. 99 )THEN
            FCOVER(IHR) = 0.1*FLOAT(ICOVER(IHR))
         ELSE
            FCOVER(IHR) = 1.0
         ENDIF

C*       Surface pressure is hundredths of inches
         IF( PRESS(IHR) .LT. 9000.0 )THEN
            PRESS(IHR) = (PRESS(IHR)/100.0) * 33.8639
         ENDIF

C*       Precipitation - already converted to millimeters (in S.FILSAM),
C*       but check for missing
         IF( PRECIP(IHR) .GT. 9999.0 )THEN
            PRECIP(IHR) = 0.0
         ENDIF

      ENDIF

C*    Check the surface pressure - if less than 500 mb or greater than
C*    9000.0, then the pressure could be missing, so set it to 1000 mb
C     (A similar check and substitution may already have been performed)
      IF( PRESS(IHR) .LT. 500.0  .OR.  PRESS(IHR) .GT. 9000.0 )THEN
          PRESS(IHR) = 1000.0
      ENDIF

C*    Compute air density in kg/m**3 -- rho = p/(r * t) --
C*    r=287 m**2/(deg k * sec**2)
C*    0.3484321=(100. kg/(m*sec**2) per mb)/(287 m**2/(deg k * sec**2))
      IF(TEMP(IHR) .LT. 900.0 )THEN
         RHO(IHR) = 0.3484321 * PRESS(IHR) / TEMP(IHR)
      ELSE
         RHO(IHR) = -1.0
      ENDIF

      RETURN
      END


c ------------------------------------------------------------------------------
      subroutine uncdp(io,ndate,maxap,k,pmm,icode,idiag)
c ------------------------------------------------------------------------------

c
c*******sample call *********************************
c            NDATE = IYEAR*100000+JULIAN*100+IHOUR  *
c            CALL UNCDP(INPPT,NDATE,24,1,PMM,ICODE) *
c****************************************************
c
c --- PMERGE   Version: 1.2       Level: 901130                    UNCDP
c ---          J. Scire, SRC
C*
C*
C*    Modified: Jayant A. Hardikar, PES
C*              Made it relatively generic
C*              2/15/95
C*
c
c --- Determine the precipitation rate for a given date/hour by:
c        (a) reading a character variable storing a previously read
c            record
c        (b) reading a precipitation record from a TD-3240 file
c        (c) resolving an accumulation period
c        (d) resolving a missing data period
c
c --- INPUTS:
c                IO - integer    - Fortran unit no. of precip. input
c                                  file
c             NDATE - integer    - Coded date/time field (YYJJJHH) of
c                                  current hour
c             MAXAP - integer    - Maximum allowed length (hrs) of an
c                                  accumulation period
c                 K - integer    - Array index of precip. arrays
c                                  (K = station no.)
c --- OUTPUT:
c               PMM - real       - Precipitation rate (mm/hr)
c                                  (missing value indicator = 9999.)
c             ICODE - integer    - Data status code:
c                                  1 = valid hourly value,
c                                  2 = valid accumulation period,
c                                  3 = missing data flag (labeled missing),
c                                  4 = missing due to excessive length
c                                      of accumulation period,
c                                  5 = missing data before first valid
c                                      record in file
c                                  6 = missing data after last valid
c                                      record in file
c
c --- UNCDP called by:  READPP
c --- UNCDP calls:      PREAD
c
c --- IFLAG -- Flag indicating precip. data status:
c              IFLAG = -99 if this is the first pass for this station
c              IFLAG =   0 if date/hr of first precip. record has not
c                          been reached yet (data is assumed missing
c                          up to date/hr of first valid record)
c              IFLAG =  +1 if current date/hr > date/hr of first record
c                          (precip. rate is assumed = 0.0 between time
c                          of valid records)
c ------------------------------------------------------------------------------
      parameter(maxps=1)
c
      real pmmsav(maxps)
      integer icodsv(maxps),ibdat(maxps),iedat(maxps),iflag(maxps)
      integer iprev(maxps)
      character*42 cdat(maxps)
c
      data icodsv/maxps*0/,ibdat/maxps*0/,iedat/maxps*0/
      data iflag/maxps*-99/,cdat/maxps*' '/
      data iprev/maxps*0/

c     Set the unit log file unit number, io6, to idiag
      io6=idiag
c
c --- determine if current date/hr is within range previously stored
c --- pmmsav array
10    continue
      if(ndate.lt.ibdat(k))then
c
c ---    Date/hr between valid records -- precip. rate = 0.0
         pmm=0.0
         icode=1
         return
      else if(ndate.le.iedat(k))then
c
c ---    Current date/hr is within period of validity of pmmsav
         pmm=pmmsav(k)
         icode=icodsv(k)
         return
      endif
c
c --- Current date/hr is after end of period of validity of pmmsav
25    continue
      call pread(io,ndate,maxap,iflag(k),cdat(k),iprev(k),
     1           icodsv(k),ibdat(k),iedat(k),pmmsav(k),idiag)
      go to 10
      end
c ------------------------------------------------------------------------------
      subroutine pread(io,ndate,maxap,iflag,cdat,iprev,
     1 icode,ibdat,iedat,pmmsav,idiag)
c ------------------------------------------------------------------------------
c
c --- PMERGE   Version: 1.2       Level: 921022                    PREAD
c ---          J. Scire, SRC
c
c --- Read a precipitation record -- if necessary, read a second record
c --- to resolve a missing data or accumulation period
c
c --- INPUTS:
c                IO - integer    - Fortran unit no. of precip. input
c                                  file
c             NDATE - integer    - Coded date/time field (YYJJJHH) of
c                                  current hour
c             MAXAP - integer    - Maximum allowed length (hrs) of an
c                                  accumulation period
c             IFLAG - integer    - Flag indicating precip. data status:
c                                  IFLAG = -99 if this is the first pass
c                                              for this station
c                                  IFLAG =   0 if date/hr of first
c                                              precip. record has not
c                                              been reached yet (data is
c                                              assumed missing up to
c                                              date/hr of first valid
c                                              record)
c                                  IFLAG =  +1 if current date/hr >
c                                              date/hr of first record
c                                              (precip. rate is assumed
c                                              = 0.0 between time of
c                                              valid records)
c              CDAT - char.*42   - A character string to store a TD-3240
c                                  data record (an input only if IFLAG = 0)
c             IPREV - integer    - Coded date/time field (YYJJJHH) of
c                                  previously read TD-3240 record
c
c --- OUTPUT:
c             ICODE - integer    - Data status code:
c                                  1 = valid hourly value,
c                                  2 = valid accumulation period,
c                                  3 = missing data flag (labeled missing)
c                                  4 = missing due to excessive length
c                                      of accumulation period
c                                  5 = missing data before first valid
c                                      record in file
c                                  6 = missing data after last valid
c                                      record in file
c             IBDAT - integer    - Beginning date/time of data (YYJJJHH)
c             IEDAT - integer    - Ending date/time of data (YYJJJHH)
c            PMMSAV - real       - Precipitation rate (mm/hr)
c                                  (missing value indicator = 9999.)
c              CDAT - char.*42   - A character string storing a TD-3240
c                                  data record (an output only if
c                                  IFLAG = -99)
c             IPREV - integer    - Updated coded date/time field of
c                                  last TD-3240 record read
c
c --- PREAD called by:  UNCDP
c --- PREAD calls:   JULIAN
c                    INDECR
c ------------------------------------------------------------------------------
c
      character*42 cdat
      character*1 cflag,cflag2
      cflag=' '
      cflag2=' '
      io6=idiag

      if(iflag.le.0)then
c
c ---    If first time through for this station, read TD-3240 record &
c ---    store in character string (CDAT)
         if(iflag.eq.-99)then
            read(io,18)cdat
18          format(a42)
            iflag=0
c
c ---       extract station id, date/hr
            read(cdat,20)idsta,iyr,imo,iday,ihr
20          format(3x,i6,10x,i2,i2,i4,3x,i2,2x,i6,a1)
            call julian(iyr,imo,iday,ijul,0)
            idate=iyr*100000+ijul*100+ihr
            iprev=idate
c
c
c ---       all data up to time of first record in file is considered
c ---       as zero
            icode=5
            ibdat=0
c ---       subtract one hour from yr/Julian day/hr
            call indecr(iyr,ijul,ihr,-1,1,24,idiag )
            iedat=iyr*100000+ijul*100+ihr
            pmmsav=0.
            return
         endif
c
c ---    extract data from a previously read character string
         read(cdat,20,end=995)idsta,iyr,imo,iday,ihr,ihinch,cflag
         iflag=1
         call julian(iyr,imo,iday,ijul,0)
         idate=iyr*100000+ijul*100+ihr
c

      else
c
c ---    read a new record
         read(io,20,end=995)idsta,iyr,imo,iday,ihr,ihinch,cflag
         call julian(iyr,imo,iday,ijul,0)
         idate=iyr*100000+ijul*100+ihr
c
c
c ---    check if date/hr of record is out of order
         if(idate.gt.iprev)then
            iprev=idate
         else
            go to 1040
         endif
      endif
c
      if(cflag.eq.' ' .or. cflag.eq.'E'  .or. cflag .eq. 'e')then
c
c ---    valid hourly data value -- convert to mm/hr
         icode=1
         ibdat=idate
         iedat=idate
         pmmsav=0.254*float(ihinch)
         return
      else if(cflag.eq.'A' .or.  cflag .eq. 'a')then
c
c ---    beginning of accumulation period -- read next record
c ---    with ending date/time of accumulation period & accum. amount
         read(io,20,end=996)jdsta,jyr,jmo,jday,jhr,jhinch,cflag2
         call julian(jyr,jmo,jday,jjul,0)
         jdate=jyr*100000+jjul*100+jhr
         if(jdate.gt.iprev)then
            iprev=jdate
         else
            go to 1050
         endif
c
         if(cflag2.ne.'A' .and. cflag2 .ne. 'a')then
c
c ---       ERROR -- unpaired accumulation period
            go to 1000
         else
c
c ---       paired accumulation records -- resolve accumulation
c ---       period precip. rate
            call deltt(iyr,ijul,ihr,jyr,jjul,jhr,idelt)
            nhrs=idelt+1
c
c ---       if length of the accumulation period exceeds max.
c ---       allowed, consider data as missing
            if(nhrs.gt.maxap)then
               icode=4
               ibdat=idate
               iedat=jdate
               pmmsav=9999.
               return
            endif
c
c ---       valid accumulation period -- resolve & save results
            icode=2
            ibdat=idate
            iedat=jdate
            pmmsav=0.254*float(jhinch)/float(nhrs)
            return
         endif
c
      else if(cflag.eq.'M' .or. cflag.eq.'D' .or.
     &        cflag.eq.'m' .or. cflag.eq.'d')then
c ---    beginning of missing or deleted data period -- read next record
c ---    with ending date/time of missing or deleted data period
         read(io,20,end=998)jdsta,jyr,jmo,jday,jhr,jhinch,cflag2
         call julian(jyr,jmo,jday,jjul,0)
         jdate=jyr*100000+jjul*100+jhr
         if(jdate.gt.iprev)then
            iprev=jdate
         else
            go to 1050
         endif
c
c **     if(cflag2.ne.'M' .and. cflag2 .ne. 'm')then
         if(cflag2.ne.cflag)then
c
c ---       ERROR -- unpaired missing or deleted data records
            go to 1030
         else
c
c ---       paired missing data records
            icode=3
            ibdat=idate
            iedat=jdate
            pmmsav=9999.
            return
         endif
      endif
c
c --- invalid precipitation flag encountered
      write(io6,990)idsta,ndate,io,iyr,imo,iday,ihr,cflag,cflag2
990   format(/1x,'Error in subr. PREAD -- invalid ',
     1 'precipitation flag encountered'//1x,'ID = ',i6,3x,
     2 //1x,'Requested date/hr (YYJJJHH) = ',i7,3x,'io = ',i5
     3 //1x,'Yr: ',i2,2x,'Month: ',i2,2x,'Day: ',i2,
     4 2x,'Hr: ',i2//1x,'CFLAG = ',a1,3x,'CFLAG2 = ',a1)
      CALL ERRHDL('E','0000',2)
c
c --- end of file encountered
995   continue
      icode=6
      ibdat=0
      iedat=9999999
      pmmsav=0.
      return
c
c --- end of file encountered -- unpaired accumulation period
996   continue
      write(io6,997)idsta,ndate,io,iyr,imo,iday,ihr,cflag
997   format(/1x,'Error in subr. PREAD -- unpaired ',
     1 'accumulation period'//1x,'ID = ',i6,3x,
     2 //1x,'Requested date/hr (YYJJJHH) = ',i7,3x,'io = ',i5
     3 //1x,'1st record: ',2x,'Yr: ',i2,2x,'Month: ',i2,2x,
     4 'Day: ',i2,2x,'Hr: ',i2,2x,' Flag: ',a1
     5 //1x,'2nd record: ',2x,'END OF FILE REACHED')
      CALL ERRHDL('E','0000',2)
c
c --- end of file encountered -- unpaired missing or deleted period
998   continue
      write(io6,999)idsta,ndate,io,iyr,imo,iday,ihr,cflag
999   format(/1x,'Error in subr. PREAD -- unpaired ',
     1 'missing or deleted data period'//1x,'ID = ',i6,3x,
     2 //1x,'Requested date/hr (YYJJJHH) = ',i7,3x,'io = ',i5
     3 //1x,'1st record: ',2x,'Yr: ',i2,2x,'Month: ',i2,2x,
     4 'Day: ',i2,2x,'Hr: ',i2,2x,' Flag: ',a1
     5 //1x,'2nd record: ',2x,'END OF FILE REACHED')
      CALL ERRHDL('E','0000',2)
c
c --- write error message -- unpaired accumulation period
1000  continue
      write(io6,1002)idsta,ndate,io,iyr,imo,iday,ihr,cflag,
     1 jyr,jmo,jday,jhr,cflag2
1002  format(/1x,'Error in subr. PREAD -- unpaired ',
     1 'accumulation period'//1x,'ID = ',i6,3x,
     2 //1x,'Requested date/hr (YYJJJHH) = ',i7,3x,'io = ',i5
     3 //1x,'1st record: ',2x,'Yr: ',i2,2x,'Month: ',i2,2x,
     4 'Day: ',i2,2x,'Hr: ',i2,2x,' Flag: ',a1
     5 //1x,'2nd record: ',2x,'Yr: ',i2,2x,'Month: ',i2,2x,
     6 'Day: ',i2,2x,'Hr: ',i2,2x,' Flag: ',a1)
      CALL ERRHDL('E','0000',2)
c
c --- write error message -- unpaired missing data period
1030  continue
      write(io6,1032)idsta,ndate,io,iyr,imo,iday,ihr,cflag,
     1 jyr,jmo,jday,jhr,cflag2
1032  format(/1x,'Error in subr. PREAD -- unpaired ',
     1 'missing or deleted data period'//1x,3x,
     2 //1x,'Requested date/hr (YYJJJHH) = ',i7,3x,'io = ',i5
     3 //1x,'1st record: ',2x,'Yr: ',i2,2x,'Month: ',i2,2x,
     4 'Day: ',i2,2x,'Hr: ',i2,2x,' Flag: ',a1
     5 //1x,'2nd record: ',2x,'Yr: ',i2,2x,'Month: ',i2,2x,
     6 'Day: ',i2,2x,'Hr: ',i2,2x,' Flag: ',a1)
      CALL ERRHDL('E','0000',2)
c
c --- write error message -- invalid date/hr ( <= previous value)
1040  continue
      write(io6,1042)idsta,ndate,io,iyr,imo,iday,ihr,cflag,
     1 idate,iprev
1042  format(/1x,'Error in subr. PREAD -- invalid date/hr ( <= ',
     1 'previous value)'//1x,'ID = ',i6,3x,
     2 //1x,'Requested date/hr (YYJJJHH) = ',i7,3x,'io = ',i5
     3 //1x,'Yr: ',i2,2x,'Month: ',i2,2x,
     4 'Day: ',i2,2x,'Hr: ',i2,2x,' Flag: ',a1
     5 //1x,'Date/hr (YYJJJHH) = ',i10,'  (Current record)'
     6 //1x,'Date/hr (YYJJJHH) = ',i10,' (Previous record)')
      CALL ERRHDL('E','0000',2)
c
1050  continue
      write(io6,1042)jdsta,ndate,io,jyr,jmo,jday,jhr,cflag,
     1 jdate,iprev
      CALL ERRHDL('E','0000',2)

      end


c ------------------------------------------------------------------------------
      subroutine indecr(iyr,ijul,ihr,idelt,ihrmin,ihrmax,idiag)
c ------------------------------------------------------------------------------
c
c --- PMERGE   Version: 1.2       Level: 901130                    INDECR
c ---          J. Scire, SRC
c
c --- Increment or decrement a date/time by "IDELT" hours
c --- (-24 <= IDELT <= 24)
c --- Allows specification of 0-23 or 1-24 hour clock
c
c --- INPUTS:
c               IYR - integer    - Input Year
c              IJUL - integer    - Input Julian day
c               IHR - integer    - Input hour (ihrmin <= IHR <= ihrmax)
c             IDELT - integer    - Change in time (hours) -- must be
c                                  between -24 to +24, inclusive
c            IHRMIN - integer    - Minimum hour (i.e., either  0 or  1)
c            IHRMAX - integer    - Maximum hour (i.e., either 23 or 24)
c
c --- OUTPUT:
c               IYR - integer    - Year after change of "IDELT" hours
c              IJUL - integer    - Julian day after change of "IDELT" hours
c               IHR - integer    - Hour after change of "IDELT" hours
c
c --- INDECR called by:  PREAD
c --- INDECR calls:      none
c ------------------------------------------------------------------------------
      io6=idiag
c
      if(iabs(idelt).gt.24)then
         write(io6,10)'IDELT',iyr,ijul,ihr,idelt,ihrmin,ihrmax
10       format(/1x,'ERROR in subr. INDECR -- invalid "',a,'" -- ',
     1   ' iyr,ijul,ihr,idelt,ihrmin,ihrmax = ',6i10)
         stop
      endif
      if(ihr.lt.ihrmin.or.ihr.gt.ihrmax)then
         write(io6,10)'IHR',iyr,ijul,ihr,idelt,ihrmin,ihrmax
         stop
      endif
c
      if(idelt.lt.0)then
c ---    idelt is negative
         ihr=ihr+idelt
         if(ihr.lt.ihrmin)then
            ihr=ihr+24
            ijul=ijul-1
            if(ijul.lt.1)then
               iyr=iyr-1
               if(mod(iyr,4).eq.0)then
                  ijul=366
               else
                  ijul=365
               endif
            endif
         endif
      else
c ---    idelt is positive or zero
         ihr=ihr+idelt
         if(ihr.gt.ihrmax)then
            ihr=ihr-24
            ijul=ijul+1
            if(mod(iyr,4).eq.0)then
               ndays=366
            else
               ndays=365
            endif
            if(ijul.gt.ndays)then
               ijul=1
               iyr=iyr+1
            endif
         endif
      endif
c
      return
      end


c-----------------------------------------------------------------------
      subroutine deltt(j1yr,j1jul,j1hr,j2yr,j2jul,j2hr,jleng)
c-----------------------------------------------------------------------
c
c --- PMERGE   Version: 1.2       Level: 901130                    DELTT
c ---          J. Scire, SRC
c
c --- Compute the difference (in hours) between two dates & times
c ---    (time #2 - time #1)
c
c --- INPUTS:
c              J1YR - integer    - Year of date/time #1
c             J1JUL - integer    - Julian day of date/time #1
c              J1HR - integer    - Hour of date/time #1
c              J2YR - integer    - Year of date/time #2
c             J2JUL - integer    - Julian day of date/time #2
c              J2HR - integer    - Hour of date/time #2
c
c --- OUTPUT:
c             JLENG - integer    - Difference (#2 - #1) in hours
c
c --- DELTT called by:  PREAD
c --- DELTT calls:      none
c-----------------------------------------------------------------------
c
      jmin=min0(j1yr,j2yr)
c
c --- find the number of hours between Jan. 1 of the "base" year and
c --- the first date/hour
      if(j1yr.eq.jmin)then
         j1=0
      else
         j1=0
         j1yrm1=j1yr-1
         do 10 i=jmin,j1yrm1
         if(mod(i,4).eq.0)then
            j1=j1+8784
         else
            j1=j1+8760
         endif
10       continue
      endif
      j1=j1+(j1jul-1)*24+j1hr
c
c --- find the number of hours between Jan. 1 of the "base" year and
c --- the second date/hour
      if(j2yr.eq.jmin)then
         j2=0
      else
         j2=0
         j2yrm1=j2yr-1
         do 20 i=jmin,j2yrm1
         if(mod(i,4).eq.0)then
            j2=j2+8784
         else
            j2=j2+8760
         endif
20       continue
      endif
      j2=j2+(j2jul-1)*24+j2hr
c
c --- compute the time difference (in hours)
      jleng=j2-j1
c
      return
      end


      SUBROUTINE VAR2FIX(INPPT)
C*--------------------------------------------------------------------
C*    PURPOSE:    THIS ROUTINE WILL READ IN A VARIABLE LENGTH TD3240
C*                FORMAT HOURLY PRECIPITATION FILE AND WRITE OUT A FIXED
C*                LENGTH PRECIPITATION SCRATCH FILE.
C*
C*    WRITTEN BY: JAYANT A. HARDIKAR
C*                PES, INC
C*
C*    DATE:       2/23/95
C*
C*    INPUT:      Variable-format TD-3240 file
C*
C*    OUTPUT:     Fixed-format temporary file
C*
C*    CALLED BY:  FILES
C----------------------------------------------------------------------

      CHARACTER*27 TEXT1
      CHARACTER*12 TEXT2(100)
      INTEGER NUMTXT

      DO 500 I = 1,999999

         READ (INPPT,6666,END=999) TEXT1,NUMTXT,(TEXT2(J),J=1,NUMTXT)
6666     FORMAT(A27,I3,100(A12))

         DO 400 II = 1,NUMTXT
            WRITE (INPPT+50,7777) TEXT1,TEXT2(II)
7777        FORMAT(A27,3X,A12)
400      CONTINUE

C*    Note: the unit number is changed when returned to the calling routine

500   CONTINUE

999   RETURN
      END