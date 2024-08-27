      MODULE ProcCtrlFile
C***********************************************************************
C     SETUP Procedure Adapted from AERMOD for AERSURFACE
C
C     PURPOSE: Controls Processing of Run SETUP Information
C
C     INPUTS:  Input Runstream File
C
C     OUTPUTS: Processing Option Switches
C              Output Options
C
C***********************************************************************

C     Variable Declarations    
      USE StartVars, only: ModNam
      USE UserParams, only: WndSectors,StartDir,SeasonMnths,
     &    StatnOrdr,numSectors,VaryAP,Airport,
     &    Moisture,MoistStr,Snow,Arid,TemporalRes,AssignMnth,
     &    Zo_Method, Anem_Height, IBL_Factor, ZoRAD
      
      USE FileUnits, only: CtrlUnt,OutUnt
      USE ErrorHandling, only: ERRHDL,EOF
      
      IMPLICIT NONE
      SAVE
      
      integer (kind=4), parameter :: IFMAX=40         ! Max Number of Fields Per Runstream Record
      integer (kind=4), parameter :: IKN=26           ! Number of Keywords
      integer (kind=4), parameter :: ISTRG=512        ! Length of Runstream Image Record
      integer (kind=4), parameter :: ILEN_FLD=200     ! Max Length of Field in Runstream Record
      

C***********************************************************************
C     This is The Global Variable Definition Block for Runstream Data
C***********************************************************************

      integer (kind=4), parameter    :: PathLen = 2       ! Length of PATH variable
      integer (kind=4), parameter    :: KeywdLen = 9      ! Length of KEYWD variable

      logical                    :: BLINE             ! Blank line
      logical                    :: INFLD             ! In-field Status Indicator
      logical                    :: ECHO              ! Echo input to output file
                                  
      character (len=Pathlen)    :: PATHWY(3)         ! Path
      character (len=PathLen)    :: PATH              ! Current path
      character (len=PathLen)    :: PPATH             ! Previous path
      character (len=KeywdLen)   :: KEYWRD            ! Current keyword
      character (len=KeywdLen)   :: PKEYWD            ! Previous keyword
      character (len=KeywdLen)   :: KEYWD(IKN)        ! Keyword array
      character (len=5)          :: KTYPE             ! Keyword type
      character (len=1)          :: RUNST(ISTRG)      ! Runstream record
      
      integer (kind=4)           :: ILINE             ! File record or line number
                                 
      character (len=ILEN_FLD)   :: FIELD(IFMAX)      ! Values read from a record in control file 
      character (len=ISTRG)      :: RUNST1            ! Runstream records as 1-char array
                                 
      integer (kind=4)           :: LOCB(IFMAX)       ! Beginning location (position)
      integer (kind=4)           :: LOCE(IFMAX)       ! Ending location (position)
      integer (kind=4)           :: IFC               ! Number of fields found on a record?
      integer (kind=4)           :: IPNUM             ! Current path number
      integer (kind=4)           :: IPPNUM            ! Previous path number
      integer (kind=4)           :: IMIT              ! error flag for type conversions
      
      
      logical                    :: ISTART            ! 'STARTING' keyword found for a path
      logical                    :: IFINIS            ! 'FINISHED' keyword found for a path
      
      logical                    :: RUNERR            ! error encountered during processing after setup
      
      character (len=KeywdLen)   :: DUMMY
      
      integer (kind=4), private  :: I


      data (KEYWD(I),I=1,IKN)   /'STARTING ','FINISHED ','TITLEONE ',
     &   'TITLETWO ','OPTIONS  ','DEBUGOPT ','DATAFILE ','DATAFILB ',
     &   'CENTERXY ','CENTERLL ','ANEM_HGT ','ZORADIUS ',
     &   'CLIMATE  ','FREQ_SECT','SECTOR   ','SEASON   ',
     &   'PROJPRMS ','RUNORNOT ','SFCCHAR  ','EFFRAD   ','NLCDGRID ',
     &   'MPRVGRID ','CNPYGRID ','NLCDTIFF ',
     &   'MPRVTIFF ','CNPYTIFF '/
     

C***********************************************************************
C     This is The Global Variable Definition Block for COntrol Pathway
C***********************************************************************

      CHARACTER (LEN=ILEN_FLD) :: TITLE1, TITLE2  ! Titles
      LOGICAL                  :: EffRadDbg, GridDbg, TiffDbg ! debug options
      LOGICAL                  :: RUN  ! run or not

      integer, parameter         :: CONUM = 17        ! Number of CO path keywords
      integer, parameter         :: OUNUM = 10        ! Number of OU path keywords
      integer (kind=4)           :: COSTAT(CONUM)     ! Array of counts for CO path keywords found
      integer (kind=4)           :: OUSTAT(OUNUM)     ! Array of counts for OU path keywords found
      character (len=1)          :: COMAND(CONUM)     ! Array of mandatory flags for CO path keywords
      character (len=1)          :: OUMAND(OUNUM)     ! Array of mandatory flags for OU path keywords
      character (len=9)          :: COWRDS(CONUM)     ! Array of ordered CO path keywords
      character (len=9)          :: OUWRDS(OUNUM)     ! Array of ordered OU path keywords
      
C***********************************************************************
C     Keyword and Status Arrays
C***********************************************************************

      data COSTAT/CONUM*0/, OUSTAT/OUNUM*0/
      
      data COWRDS(1)  /'STARTING'/,   COMAND(1)  /'T'/
      data COWRDS(2)  /'TITLEONE'/,   COMAND(2)  /'T'/
      data COWRDS(3)  /'TITLETWO'/,   COMAND(3)  /'F'/
      data COWRDS(4)  /'OPTIONS'/,    COMAND(4)  /'F'/
      data COWRDS(5)  /'DEBUGOPT'/,   COMAND(5)  /'F'/
      data COWRDS(6)  /'CENTERXY'/,   COMAND(6)  /'F'/  !conditional
      data COWRDS(7)  /'CENTERLL'/,   COMAND(7)  /'F'/  !conditional
      data COWRDS(8)  /'DATAFILE'/,   COMAND(8)  /'T'/
      data COWRDS(9)  /'ANEM_HGT'/,   COMAND(9)  /'F'/  !conditional
      data COWRDS(10) /'ZORADIUS'/,   COMAND(10) /'F'/  !conditional
      data COWRDS(11) /'CLIMATE'/,    COMAND(11) /'F'/
      data COWRDS(12) /'FREQ_SECT'/,  COMAND(12) /'T'/
      data COWRDS(13) /'SECTOR'/,     COMAND(13) /'F'/  !conditional
      data COWRDS(14) /'SEASON'/,     COMAND(14) /'F'/  !conditional
      data COWRDS(15) /'PROJPRMS'/,   COMAND(15) /'F'/  !RWB - not in use
      data COWRDS(16) /'RUNORNOT'/,   COMAND(16) /'T'/
      data COWRDS(17) /'FINISHED'/,   COMAND(17) /'T'/
      
      data OUWRDS(1)  /'STARTING'/,   OUMAND(1)  /'T'/
      data OUWRDS(2)  /'SFCCHAR'/,    OUMAND(2)  /'F'/
      data OUWRDS(3)  /'EFFRAD'/,     OUMAND(3)  /'F'/
      data OUWRDS(4)  /'NLCDGRID'/,   OUMAND(4)  /'F'/
      data OUWRDS(5)  /'MPRVGRID'/,   OUMAND(5)  /'F'/
      data OUWRDS(6)  /'CNPYGRID'/,   OUMAND(6)  /'F'/
      data OUWRDS(7)  /'NLCDTIFF'/,   OUMAND(7)  /'F'/
      data OUWRDS(8)  /'MPRVTIFF'/,   OUMAND(8)  /'F'/
      data OUWRDS(9)  /'CNPYTIFF'/,   OUMAND(9)  /'F'/
      data OUWRDS(10) /'FINISHED'/,   OUMAND(10) /'T'/
      
C     Logical flags for met site type (PRIMARY or SECONDARY)
      logical           :: PrimaryFlg    ! Primary site
      logical           :: SecondaryFlg  ! Secondary site
      
C     Logical flags for surface roughness (Zo) method
      logical           :: ZoEffFlg      ! IBL effective roughness
      logical           :: ZoRadFlg      ! User-defined radius
      
C     Logical flag for anemometer height
      logical           :: AnemHgtFlg    ! anemometer height 


C     Control variables required to process land cover file
      
      character (len=3) :: fileType ! NLCD file type, tif or bin
      
      integer (kind=4)  :: NLCDYear  ! 1992, 2001, 2006
      integer (kind=4)  :: ImpYear   ! 2001, 2006, 2011
      integer (kind=4)  :: CanYear   ! 2001, 2011 (not available in 2006)
      integer (kind=4)  :: NumSeas = 4 ! number of seasons
      
      logical           :: gotLCD    ! got land cover data file
      logical           :: GotImp    ! got impervious data file
      logical           :: GotCan    ! got canopy data file
      logical           :: UseImp    ! use impervious file
      logical           :: UseCan    ! use canopy file
      
C     Flag to indicate error processing FREQ_SECT keyword for 
C     FREQ_SECT/SECTOR keyword dependency.
      logical               :: FreqSectErr
      
C     Flag to indicate error processing SECTOR keyword for 
C     for post processing SECTOR keywords (IDs, directions, AP/NONAP)
      logical               :: SectorErr
      
C     Flag to indicate if default sector definitions (start/end)
C     should be used.  Set when processing first sector in SECTOR.
      logical               :: DfltSec

      contains

      SUBROUTINE SETUP
C***********************************************************************
C     Process control file
C
C***********************************************************************
      
      IMPLICIT NONE
      
      CHARACTER (len=20) ::  RDFRM

      INTEGER            :: I, IFSTAT
      LOGICAL            :: NOPATH, NOKEY
      
      ModNam = 'SETUP'  ! Module/Procedure name
      
C     Variable Initializations
      PATH  = '  '
      PPATH = '  '
      EOF = .FALSE.
      IFINIS = .TRUE.
      ECHO  = .TRUE. 

C     Initialize line counters: ILINE
      ILINE  = 0

C     Initialize PATHWY array
      PATHWY(1) = 'CO'
      PATHWY(2) = 'OU'
      PATHWY(3) = '**'
      
C     Initialize user input variables that have default values
      TITLE1 = ""                   ! Title 1
      TITLE2 = ""                   ! Title 2
      StatnOrdr = "PRIMARY"         ! Station Order (PRIMARY or SECONDARY)
      PrimaryFlg = .false.          ! Primary station/site flag
      SecondaryFlg = .false.        ! Secondary station/site flag
      Zo_Method = "ZORAD"           ! Roughness Method (ZORAD or ZOEFF)
      ZoEffFlg = .false.            ! ZOEFF method flag
      ZoRadFlg = .false.            ! ZORAD method flag
      Anem_Height = -9.0D0          ! Anemometer height
      AnemHgtFlg = .false.          ! Anemometer height flag
      IBL_Factor = 6.0D0            ! IBL factor for computing roughness
      fileType = 'tif'              ! NLCD file type (geotiff, default)
      ZoRad = 1000.0D0              ! Radius to compute roughness (ZORAD)
      NLCDYear = 0                  ! NLCD version
      ImpYear  = 0                  ! Impervious data version
      CanYear  = 0                  ! Canopy data version
      GotLCD  = .false.             ! got land cover file
      GotCan  = .false.             ! got canopy file
      GotImp  = .false.             ! got impervious file
      UseCan  = .false.             ! use canopy file
      UseImp  = .false.             ! use impervious file
      FreqSectErr = .false.         ! error processing FREQ_SECT keyword
      SectorErr = .false.           ! error processing SECTOR keyword
      DfltSec = .false.             ! use default sector definitions
      NumSectors = 1                ! number of roughness sectors
      WndSectors%sec_id(:) = 0      ! sector ids
      WndSectors%sec_start(:) = 0   ! sector start direction
      WndSectors%sec_end(:) = 0     ! sector end direction
      WndSectors%sec_ap(:) = .true. ! sector airport flag
      StartDir(1) = 0.0D0           ! single sector, start at 0 degrees
      VaryAP = .false.              ! vary airport association by individual sector
      Airport = .true.              ! site is an airport
      Moisture = 'A'                ! moisture (A=Average, W=Wet, D=Dry)
      MoistStr = 'Average'          ! moisture label for output
      Snow = .false.                ! no continuous snow cover
      Arid = .false.                ! non-arid conditions
      TemporalRes = 'M'             ! resolution of sfc vals (A=Annual, M=Monthly, S=Season)
      AssignMnth = .false.          ! reassign months to seasons, use default
      
      EffRadDbg = .false.           ! create effective radius debug file
      GridDbg   = .false.           ! create grid files for import to GIS
      TiffDbg   = .false.           ! create tiff debug files

        
C     default month/season assignments - assume winter without continuous snow
      SeasonMnths(1)=1     ! Jan - winter
      SeasonMnths(2)=1     ! Feb - winter
      SeasonMnths(3)=3     ! Mar - spring
      SeasonMnths(4)=3     ! Apr - spring
      SeasonMnths(5)=3     ! May - spring
      SeasonMnths(6)=4     ! Jun - summer
      SeasonMnths(7)=4     ! Jul - summer
      SeasonMnths(8)=4     ! Aug - summer
      SeasonMnths(9)=5     ! Sep - fall
      SeasonMnths(10)=5    ! Oct - fall
      SeasonMnths(11)=5    ! Nov - fall
      SeasonMnths(12)=1    ! Dec - winter

C     Setup READ format and ECHO format for runstream record,
C     based on the ISTRG PARAMETER (set in mod_ProcCtrlFile)
      WRITE(RDFRM,9100) ISTRG, ISTRG
 9100 FORMAT('(A',I4.4,',T1,',I4.4,'A1)')

C     LOOP Through Input Runstream Records
      DO WHILE (.NOT. EOF)

C        Increment the Line Counter
         ILINE = ILINE + 1

C        READ Record to Buffers, as A'num' and 'num'A1, where 'num' = ISTRG
C        Length of ISTRG is Set in PARAMETER Statement in mod_ControlParams
         READ (CtrlUnt,RDFRM,END=999) RUNST1, (RUNST(I), I = 1, ISTRG)

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
               WRITE(OutUnt,9200) RUNST1(1:11)
 9200          FORMAT(A11)
            ELSE IF (LOCB(1) .EQ. 2) THEN
               WRITE(OutUnt,9210) RUNST1(1:12)
 9210          FORMAT(A12)
            ELSE IF (LOCB(1) .EQ. 3) THEN
               WRITE(OutUnt,9220) RUNST1(1:13)
 9220          FORMAT(A13)
            ELSE IF (LOCB(1) .EQ. 4) THEN
               WRITE(OutUnt,9230) RUNST1(1:14)
 9230          FORMAT(A14)
            END IF
         ELSE IF (ECHO) THEN
C           Echo Full Input Card to Output File
            WRITE(OutUnt,'(a)') RUNST1(1:LEN_TRIM(RUNST1))
         END IF

C        If Blank Line, Then CYCLE to Next Card
         IF (BLINE) GO TO 11

C        Check for 'NO ECHO' In First Two Fields
         IF (FIELD(1) .EQ. 'NO' .AND. FIELD(2) .EQ. 'ECHO') THEN
            ECHO = .FALSE.
            GO TO 11
         END IF

C        Extract Pathway ID From Field 1                    ---   CALL EXPATH
         CALL EXPATH(FIELD(1),PATHWY,6,NOPATH)

C        For Invalid Pathway and Comment Lines Skip to Next Record
         IF (NOPATH) THEN
C           WRITE Error Message    ! Invalid Pathway ID
            CALL ERRHDL(PPATH,ModNam,'E','100',PATH,ILINE)
            PATH = PPATH
            GO TO 11
         ELSE IF (PATH .EQ. '**') THEN
            GO TO 11
         END IF

C        Extract Keyword From Field 2                       ---   CALL EXKEY
         CALL EXKEY(FIELD(2),NOKEY)

         IF (NOKEY) THEN
C           WRITE Error Message    ! Invalid Keyword
            CALL ERRHDL(PATH,ModNam,'E','105',KEYWRD,ILINE)
            PKEYWD = KEYWRD
            GO TO 11
         END IF

C        Check for Proper Order of Setup Cards              ---   CALL SETORD
         CALL SETORD

C        Process Input Card Based on Pathway

         IF (PATH .EQ. 'CO') THEN
C           Process COntrol Pathway Cards                   ---   CALL COCARD
            CALL COCARD

         ELSE IF (PATH .EQ. 'OU') THEN
C           Process OUtput Pathway Cards                    ---   CALL OUCARD
            CALL OUCARD
         END IF

C        Store the Current Keyword as the Previous Keyword
         PKEYWD = KEYWRD(1:LEN_TRIM(KEYWRD))

C        Check for 'OU FINISHED' Card.  Exit DO WHILE Loop By Branching
C        to Statement 999 in Order to Avoid Reading a ^Z "End of File"
C        Marker That May Be Present For Some Editors.
         IF (PATH .EQ. 'OU' .AND. 
     &       KEYWRD(1:LEN_TRIM(KEYWRD)) .EQ. 'FINISHED') THEN
            GO TO 999
         END IF

         GO TO 11
 999     EOF = .TRUE.
 11      CONTINUE
      END DO

C     Reinitialize Line Number Counter
      ILINE = 0

C     Check That All Pathways Were Finished
      IF (COSTAT(CONUM).NE.1 .OR. OUSTAT(OUNUM).NE.1) THEN
C        Runstream File Incomplete, Save I?STAT to IFSTAT and Write Message
         IFSTAT = COSTAT(CONUM)*10000 + OUSTAT(OUNUM)
         WRITE(DUMMY,'(I5.5)') IFSTAT
         CALL ERRHDL(PATH,ModNam,'E','125',DUMMY,ILINE)
      END IF

C     ------------------------------------------------------------------
C     Now that runstream has been read, perform postprocessing
C     and validation on sector definitions if no error was encountered 
C     in FREQ_SECT.

      IF (.not. FreqSectErr .and. .not. SectorErr) CALL POSTSECT
      
C     ------------------------------------------------------------------

      RETURN
      END SUBROUTINE SETUP

      SUBROUTINE LWRUPR
C***********************************************************************
C        LWRUPR Procedure Adapted from AERMOD for AERSURFACE
C
C        PURPOSE: Transfer All Characters From Lower Case To
C                 Upper Case (Using INDEX Intrinsic Function)
C                 Note that the CHAR*ISTRG RUNST1 Variable Includes
C                 the Original Case for Echoing and for Later Use
C                 To Retrieve Filenames.
C
C        ORIGINAL PROGRAMMER: Roger Brode, Kevin Stroupe
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Card Image (ISTRG Character Array)
C                 Number of Characters in String, PARAMETER ISTRG
C
C        OUTPUTS: Input Runstream Card Image (Array) in Uppercase
C
C***********************************************************************
      
      IMPLICIT NONE
      
      INTEGER (KIND=4) :: I, INDCHK
      CHARACTER (LEN=26) UPCASE
      CHARACTER (LEN=26) LWCASE

C     Variable Initializations
      DATA UPCASE/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      DATA LWCASE/'abcdefghijklmnopqrstuvwxyz'/

      DO I = 1, ISTRG
         IF (RUNST(I) .NE. ' ') THEN
            INDCHK = INDEX(LWCASE,RUNST(I))
            IF (INDCHK .NE. 0) THEN
               RUNST(I) = UPCASE(INDCHK:INDCHK)
            END IF
         END IF
      END DO

      RETURN
      END SUBROUTINE LWRUPR
      
      SUBROUTINE DEFINE
C***********************************************************************
C        DEFINE Procedure Adapted from AERMOD for AERSURFACE
C
C        PURPOSE: Defines Location of Fields on Runstream Input Image
C
C        ORIGINAL PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C*       Revision History:
C*
C*       MODIFIED: October 19, 2009
C*                
C*                 Modified to recognize double quotes (") as 
C*                 field delimiters to allow for filenames with
C*                 embedded spaces.
C
C        INPUTS:   Input Runstream Card Image
C
C        OUTPUTS:  Number of Fields on Card, IFC
C                  Beginning and Ending Columns of Fields, LOCB and LOCE
C
C***********************************************************************
C
    
      IMPLICIT NONE
          
      LOGICAL INQUOTE

      INTEGER (KIND=4) :: I

C     Initialize the Blank Line and In-field Status Indicators
      BLINE = .TRUE.
      INFLD = .FALSE.
      INQUOTE = .FALSE.

      IF (ILINE .EQ. 1) THEN
C        Define the Starting Column for the Input File In Case File Is Shifted.
C        Allow for Shift of Up to 3 Columns
         LOCB(1) = 1
         LOCE(1) = LOCB(1) + 1
         LOCB(2) = LOCB(1) + 3
         LOCE(2) = LOCB(1) + 11  ! CRT, change to add 11 (not 10) to allow for 9-char field
      END IF

      IFC = 1

C     Loop Through the Pathway and Keyword Fields To Check for Blank Line
      DO I = LOCB(1), ISTRG
         IF (RUNST(I) .NE. ' ') THEN
            BLINE = .FALSE.
            EXIT
         END IF     
      END DO

C     Loop through the Data Fields
      DO I = LOCB(1)+3, ISTRG
         IF (.NOT.INFLD .AND. RUNST(I).EQ.'"') THEN
C           Location is the Beginning of a Field using "'s
C           Set Mark of not Blank Line
            BLINE = .FALSE.
C           Set Mark of in a Field
            INFLD = .TRUE.
C           Set Mark of in a Quote Field
            INQUOTE = .TRUE.
C           Increment the Field Counter
            IFC = IFC + 1
C           Record the Location of Beginning of the Field
            LOCB(IFC) = I + 1
         ELSE IF (.NOT.INFLD .AND. RUNST(I).NE.' ') THEN
C           Location is the Beginning of a Field
C           Set Mark of not Blank Line
            BLINE = .FALSE.
C           Set Mark of in a Field
            INFLD = .TRUE.
C           Increment the Field Counter
            IFC = IFC + 1
C           Record the Location of Beginning of the Field
            LOCB(IFC) = I
         ELSE IF (INQUOTE .AND. RUNST(I).EQ.'"') THEN
C           Location is the End of a Field
C           Set Mark of Not In a field
            INFLD = .FALSE.
C           Set Mark of Not in a Quote Field
            INQUOTE = .FALSE.
C           Record the Location of Ending of the Field
            LOCE(IFC) = I - 1
         ELSE IF (.NOT.INQUOTE .AND. INFLD .AND. RUNST(I).EQ.' ') THEN
C           Location is the End of a Field
C           Set Mark of Not In a field
            INFLD = .FALSE.
C           Record the Location of Ending of the Field
            LOCE(IFC) = I - 1
         END IF

C        Check for End of Input String
C        (Length of ISTRG is Set as a PARAMETER in mod_ProcCtrlFile)
         IF (INFLD .AND. I.EQ.ISTRG) THEN
            LOCE(IFC) = ISTRG
         END IF
      END DO
      
      RETURN
      END SUBROUTINE DEFINE

      SUBROUTINE GETFLD
C***********************************************************************
C        GETFLD Adapted from AERMOD for AERSURFACE
C
C        PURPOSE: Gets Contents of Fields on Runstream Input Image
C
C        ORIGINAL PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Card Image
C
C        OUTPUTS: Contents of Fields on Card
C
C***********************************************************************
C
C     Variable Declarations
      
      IMPLICIT NONE
      
      CHARACTER (len=20)        :: WRTFRM
      INTEGER (KIND=4)          :: I, J

C --  Initialize FIELD array
      FIELD(:) = ''

C     Setup WRITE format for internal write to FIELD
C     based on the ILEN_FLD PARAMETER
      WRITE(WRTFRM,9004) ILEN_FLD
 9004 FORMAT('(',I4.4,'(A1:))')

      DO I = 1, IFC
         IF (LOCE(I)-LOCB(I) .LE. (ILEN_FLD - 1) ) THEN
C           Field Satisfies Limit of ILEN_FLD Characters (set in mod_ProcCtrlFile)
            WRITE(FIELD(I),WRTFRM) (RUNST(J),J=LOCB(I),LOCE(I))

         ELSE
C           Field Exceeds ILEN_FLD Character Limit
C           Truncate Field at ILEN_FLD Characters
            WRITE(FIELD(I),WRTFRM) (RUNST(J),J=LOCB(I),
     &                                         LOCB(I)+ILEN_FLD-1)
         END IF
      END DO

      RETURN
      END SUBROUTINE GETFLD

      SUBROUTINE EXPATH(INPFLD,PATHWY,IPN,NOPATH)
C***********************************************************************
C        EXPATH Procedure Adapted from AERMOD for AERSURFACE
C
C        PURPOSE: Extracts and Verifies Pathway ID from
C                 Runstream Input Card Image
C
C        ORIGINAL PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Card Image
C
C        OUTPUTS: The Extracted Pathway ID
C
C***********************************************************************
C
C     Variable Declarations
      
      IMPLICIT NONE
      
      CHARACTER (LEN=*), INTENT(IN)               :: INPFLD
      CHARACTER (LEN=*), INTENT(IN), DIMENSION(:) :: PATHWY
      INTEGER, INTENT(IN)                         :: IPN
      LOGICAL, INTENT(OUT)                        :: NOPATH
      
      INTEGER :: I

C     Variable Initializations
      NOPATH = .TRUE.

C     Begin The Processing
      IF (len(adjustl(trim(INPFLD))) .gt. 0) THEN
C        Check the Read-in Pathway
         PATH = INPFLD
         DO I = 1, IPN
C           In Case of Match Set NOPATH to FALSE and Set Path Number, IPNUM
            IF (INPFLD .EQ. PATHWY(I)) THEN
               NOPATH = .FALSE.
               IPNUM = I
C              Exit to END
               GO TO 999
            END IF
         END DO
      ELSE
C        In Case of Blank Field Set Pathway to Previous Pathway
         NOPATH = .FALSE.
         PATH   = PPATH
         IPNUM  = IPPNUM
      END IF

 999  RETURN
      END SUBROUTINE EXPATH

      SUBROUTINE EXKEY(INPFLD,NOKEY)
C***********************************************************************
C        EXKEY Procedure Adapted from AERMOD for AERSURFACE
C
C        PURPOSE: Extracts and Verifies Keyword from
C                 Runstream Input Card Image
C
C        ORIGINAL PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Card Image
C
C        OUTPUTS: The Extracted Keyword
C
C***********************************************************************
C

C     Variable Declarations
      
      IMPLICIT NONE
      
      CHARACTER (len=*), INTENT(IN)   :: INPFLD
      LOGICAL,           INTENT(OUT)  :: NOKEY

      INTEGER                :: I

C     Variable Initializations
      NOKEY  = .TRUE.

C     Begin The Processing
      IF (len(adjustl(trim(INPFLD))) .gt. 0) THEN
C        Check the Read-in Keyword
         KEYWRD = INPFLD
         DO I = 1, IKN
C           In Case of Match Set NOKEY to FALSE
            IF (INPFLD .EQ. KEYWD(I)) THEN
               NOKEY = .FALSE.
C              Exit to END
               GO TO 999
            END IF
         END DO
      ELSE
C        In Case of Blank Field, Keyword Is Set to Previous Keyword
         NOKEY  = .FALSE.
         KEYWRD = PKEYWD
      END IF

 999  RETURN
      END SUBROUTINE EXKEY

      SUBROUTINE SETORD
C***********************************************************************
C        SETORD Procedure Adapted from AERMOD for AERSURFACE
C
C        PURPOSE: To Check Run Stream Setup Images for Proper
C                 Order
C
C        INPUTS:  Input Runstream Card Image
C
C        OUTPUTS: Status Settings and Error Messages
C
C***********************************************************************
C
C     Variable Declarations    
      IMPLICIT NONE
      
      ModNam = 'SETORD'  ! Module/Procedure name

      IF (KEYWRD .EQ. 'STARTING') THEN
         IF (ISTART .OR. .NOT.IFINIS) THEN
C           WRITE Error Message: Starting Out of Order
            CALL ERRHDL(PPATH,ModNam,'E','115',PATH,ILINE)
         ELSE IF (IPNUM .NE. IPPNUM+1) THEN
C           WRITE Error Message: Pathway Out of Order
            CALL ERRHDL(PPATH,ModNam,'E','120',PATH,ILINE)
         END IF
C        Set Starting Indicator
         ISTART = .TRUE.
C        Set Finished Indicator
         IFINIS = .FALSE.
      ELSE IF (KEYWRD .EQ. 'FINISHED') THEN
         IF (IFINIS .OR. .NOT.ISTART) THEN
C           WRITE Error Message: Finished Out of Order
            CALL ERRHDL(PPATH,ModNam,'E','115',PATH,ILINE)
         ELSE IF (ISTART .AND. PATH.NE.PPATH) THEN
C           WRITE Warning Message: Pathway Out of Order
            CALL ERRHDL(PPATH,ModNam,'E','120',PATH,ILINE)
         END IF
C        Reset Starting Indicator
         ISTART = .FALSE.
C        Set Finished Indicator
         IFINIS = .TRUE.
      ELSE IF (.NOT.ISTART .OR. IFINIS) THEN
C        WRITE Error Message: Starting or Finished Out of Order
         CALL ERRHDL(PPATH,ModNam,'E','115',PATH,ILINE)
      ELSE IF (ISTART .AND. PATH.NE.PPATH) THEN
C        WRITE Warning Message: Pathway Out of Order
         CALL ERRHDL(PPATH,ModNam,'E','120',PATH,ILINE)
      END IF

C     Save Current Path and Path Number as Previous Path and Number
      PPATH  = PATH
      IPPNUM = IPNUM

      RETURN
      END SUBROUTINE SETORD


      SUBROUTINE POSTSECT
C***********************************************************************
C        POSTSECT Procedure for AERSURFACE
C
C        PURPOSE: Post setup processing for sector definitions after 
C                 runstream has been read.

C                 1) If no occurrences of the SECTOR keyword, verify
C                    the conditions were valid.
C
C                 2) Validate # of occurrences of SECTOR keyword based 
C                    on FREQ_SECT attributes (numSectors, VARYAP) and 
C                    default sector flag. The occurrences of the SECTOR 
C                    keyword must equal numSectors from FREQ_SECT if 
C                    NumSectors is 2-7 OR 9-11 OR Default sectors is 
C                    false OR the VARYAP flag is true OR VARYAP is false
C                    and there is at at least one occurrence.
C
C                 3) If using default wind sectors, define start/end
C                    directions:
C                    8 sectors are centered on 0, 45, 90, etc. degrees.
C                    12 sectors are centered on 15, 45, 75, etc. degrees.
C                    16 sectors are centered on 0, 22.5, 45, etc. degrees.
C
C                    Assign start directions to StartDir() array
C
C                 4) If user defined sectors:
C                    a) Sort sectors as they may be entered out
C                       of order by sector ID.
C                    b) Check for consectutive sector IDs.
C                    d) Check start/end directions for each adjacent 
C                       pair of sectors for overlap and gaps.
C                    e) For 2..12 sectors, if not using default
C                       definitions, check to ensure all sectors span
C                       at least 30-degrees. For 16 sectors, all sectors
C                       must span 22.5 degrees.
C                    f) Assign start directions to StartDir() array
C
C***********************************************************************
C
C     Variable Declarations
      USE UserParams, only: numSectors, VaryAP, SectKyWds, 
     &                      WndSectors, StartDir
     
      IMPLICIT NONE
      
      
      INTEGER                    :: i,j,k         ! counters
      DOUBLE PRECISION           :: secWidth      ! sector width in degrees
      DOUBLE PRECISION           :: secStart      ! start direction of first sector
      
      INTEGER                    :: tmpid         ! tmp sector id
      DOUBLE PRECISION           :: tmpstart      ! tmp sector start direction
      DOUBLE PRECISION           :: tmpend        ! tmp sector end direction
      LOGICAL                    :: tmpap         ! tmp airport flag
      
      DOUBLE PRECISION            :: degSum        ! sum of degrees across sectors
      
      DOUBLE PRECISION, PARAMETER :: eps = 0.001   ! epsilon for comparison      
      
      CHARACTER (LEN=9)          :: TmpKwd        ! temp keyword
      
      ModNam = 'POSTSECT'  ! Module/Procedure name

C     Variable Initializations
      TmpKwd = 'SECTOR'
      secStart = 0.0D0
C ----------------------------------------------------------------------
C     If no SECTOR keywords were processed, check conditions to make
C     sure this is valid and set flag for default sectors.  A zero count
C     of sector keywords is only valid when number of sectors on 
C     FREQ_SECT is 1, 8, 12, or 16 and VARYAP is false.
C ----------------------------------------------------------------------
      SELECT CASE(numSectors)
         CASE(1,8,12,16)
            IF (.not. VARYAP .and. SectKyWds == 0) THEN
               DfltSec = .true.
            END IF
      END SELECT    
      
C ----------------------------------------------------------------------
C     Check # of occurrences of SECTOR keyword based on FREQ_SECT 
C     attributes (numSectors, VARYAP) and default sector flag. The 
C     occurrences of the SECTOR keyword must equal numSectors from 
C     FREQ_SECT if NumSectors is 2-7 OR 9-11 OR Default sectors is false 
C     OR the VARYAP flag is true OR VARYAP is false and there is at
C     at least one occurrence.
C ----------------------------------------------------------------------
      IF (((numSectors >= 2 .and. numSectors <= 7) .or.
     &     (numSectors >= 9 .and. numSectors <= 11) .or.
     &     (VARYAP) .or.
     &     (.not. DfltSec) .or.
     &     (.not. VARYAP .and. SectKyWds > 0)) .and.
     &      SectKyWds /= numSectors) THEN

         CALL ERRHDL(PPATH,ModNam,'E','208',TmpKwd,ILINE)
         RETURN
      END IF

C ----------------------------------------------------------------------
C     Fill Automated Wind Sectors when using defaults, DfltSec = .true.
C ----------------------------------------------------------------------    

      IF (DfltSec) THEN
      
         ! Compute sector width based on number of sectors specified
         ! Must convert arguments to floating point REAL
         secWidth = 360.0D0/real(numSectors, kind=8)
         
         SELECT CASE(numSectors)
         
            ! 1 and 12 sectors, start direction = 0 degrees
            CASE(1,12)  
               secStart = 0.0D0           
            
!           8 and 16 sectors, first sector centered on North (0 degrees)
            CASE(8,16)
               secStart = 360.0D0 - (secWidth/2.0D0)
            
         END SELECT
         
         ! Assign sectors
         DO i=1,numSectors
            WndSectors%sec_id(i) = i
            WndSectors%sec_start(i) = 
     &         secStart + (secWidth*real(i-1,8))
            WndSectors%sec_end(i) = WndSectors%sec_start(i) + 
     &                              secWidth
            IF (WndSectors%sec_start(i) .gt. 360.0D0) THEN
               WndSectors%sec_start(i) = 
     &         WndSectors%sec_start(i) - 360.0D0
            END IF
            IF (WndSectors%sec_end(i) .gt. 360.0) THEN
               WndSectors%sec_end(i) = WndSectors%sec_end(i) - 360.0D0
            END IF
         END DO
         
         ! set start direction from WndSectors array
         DO i=1,NumSectors
            StartDir(i) = WndSectors%sec_start(i)
         END DO 
   
!      END IF

C ----------------------------------------------------------------------
C     For User Defined Sectors
C ----------------------------------------------------------------------
      ELSE
         
         ! Sort Wind Sectors by ID
         DO i=1,numSectors
            DO j=i,numSectors
               IF (wndsectors%sec_id(j) .lt. wndsectors%sec_id(i)) THEN
                  tmpid = wndsectors%sec_id(j)
                  tmpstart = wndsectors%sec_start(j)
                  tmpend = wndsectors%sec_end(j)
                  tmpap = wndsectors%sec_ap(j)
                  DO k=j,i+1,-1
                     wndsectors%sec_id(k) = wndsectors%sec_id(k-1)
                     wndsectors%sec_start(k) = wndsectors%sec_start(k-1)
                     wndsectors%sec_end(k) = wndsectors%sec_end(k-1)
                     wndsectors%sec_ap(k) = wndsectors%sec_ap(k-1)
                  END DO
                  wndsectors%sec_id(i) = tmpid
                  wndsectors%sec_start(i) = tmpstart
                  wndsectors%sec_end(i) = tmpend
                  wndsectors%sec_ap(i) = tmpap
               END IF
            END DO ! j
         END DO ! i
         
C ----------------------------------------------------------------------
C     Validate Sectors: check for consecutive IDs
C                       check start/end directions (overlap and gaps)
C                       check for full 360-degree coverage
C                       1..12 sectors, check for 30-degree minimum span
C                       16 sectors, check for 22.5 degree span
C ----------------------------------------------------------------------

      
         ! initialize total degrees across sectors to zero
         degSum = 0.0
   
         ! loop over sector definitions
         DO i=1,numSectors
         
            ! cannot check the first sector
            IF (i .gt. 1) THEN
                     
               ! Check for consecutive sector IDs (1..number of sectors)
               IF (wndsectors%sec_id(i)-
     &             wndsectors%sec_id(i-1) .ne. 1) THEN
                  CALL ERRHDL(PPATH,ModNam,'E','266',TmpKwd,ILINE)
                  RETURN
               END IF
               
               ! Check for a overlap and gap in the previous end 
               ! and current start directions
               IF (abs(wndsectors%sec_start(i)-
     &                wndsectors%sec_end(i-1)) .gt. eps) THEN
   
                  CALL ERRHDL(PPATH,ModNam,'E','267',TmpKwd,ILINE)
                  RETURN
               END IF
               
            END IF
            
            ! compute sector width
            secWidth = wndsectors%sec_end(i) - wndsectors%sec_start(i)
            
            IF (secWidth .lt. 0.0) THEN
               secWidth = secWidth + 360.0
            END IF
            
            
            ! Check for a minimum 30-degree sector width for 2..12 sectors
            IF (numSectors .ge. 2 .and. numSectors .le. 12) THEN
               IF (secWidth .lt. 30.0) THEN
                  CALL ERRHDL(PPATH,ModNam,'E','268',TmpKwd,ILINE)
                  RETURN
               END IF
            ELSEIF (numSectors == 16) THEN
               IF (secWidth .lt. 22.5) THEN
                  CALL ERRHDL(PPATH,ModNam,'E','271',TmpKwd,ILINE)
                  RETURN
               END IF
            END IF
               
            ! increment cumulative degree sum
            degSum = degSum + secWidth
         
         END DO
   
         ! Check total coverage (sum should equal 360.0
         IF (abs(degSum - 360.0) .gt. eps) THEN
            CALL ERRHDL(PPATH,ModNam,'E','269',TmpKwd,ILINE)
            RETURN
         END IF
       
C ----------------------------------------------------------------------
C        Populate StartDir() Array - may be temporary if code references
C           for StartDir() are replaced with WndSectors%sec_start().
C        Sort StartDir() (this should be temporary code)
C        Sort WndSectors array to match (this should be temporary code)
C ----------------------------------------------------------------------
   
         DO i=1,NumSectors
            StartDir(i) = WndSectors%sec_start(i)
         END DO
        
C ---    Sort the sector boundaries into ascending order from smallest to
C        largest angle so the sector allocation
         IF( NumSectors > 1 )THEN
            CALL STRAIGHTSORT(NumSectors, 
     &                        StartDir)
         ENDIF
         
C ---    Sort WndSectors to match StartDir()
         DO i=1,NumSectors
            DO j=1,NumSectors
               
               IF (abs(StartDir(i)-
     &             WndSectors%sec_start(j)) .lt. eps) THEN
                  ! set temp variables for current i
                  tmpstart = WndSectors%sec_start(i)
                  tmpend = WndSectors%sec_end(i)
                  tmpap = WndSectors%sec_ap(i)
                  
                  ! new assignments
                  WndSectors%sec_start(i) = WndSectors%sec_start(j)
                  WndSectors%sec_end(i) = WndSectors%sec_end(j)
                  WndSectors%sec_ap(i) = WndSectors%sec_ap(j)
                  
                  ! reassign from tmp so they are not lost
                  WndSectors%sec_start(j) = tmpstart
                  WndSectors%sec_end(j) = tmpend
                  WndSectors%sec_ap(j) = tmpap
                  
                  cycle
                  
               END IF
               
            END DO  ! i loop
          END DO  ! j loop

      END IF

      RETURN

      END SUBROUTINE POSTSECT
      

      SUBROUTINE STONUM(STRVAR,LENGTH,FNUM,IMUTI)
C***********************************************************************
C        STONUM Adapted from AERMOD for AERSURFACE
C
C        (This Is A Utility Program)
C
C        PURPOSE: Gets Number From A String Variable
C
C        ORIGINAL PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input String Variable
C                 Length of Character String
C
C        OUTPUTS: Numbers
C
C***********************************************************************
C
C     Variable Declarations

      IMPLICIT NONE

      CHARACTER (LEN=*), INTENT(IN)    :: STRVAR
      INTEGER,           INTENT(IN)    :: LENGTH
      INTEGER,           INTENT(INOUT) :: IMUTI
      DOUBLE PRECISION,  INTENT(OUT)   :: FNUM
      
      CHARACTER (LEN=1)                :: CHK
        
      CHARACTER (LEN=10)               :: NUMS
      INTEGER                          :: I
      DOUBLE PRECISION                 :: FDEC, FDC1, HEAD
      DOUBLE PRECISION                 :: CNUM
      LOGICAL                          :: MEND, IN, NMARK, PMARK
      LOGICAL                          :: DMARK, MMARK, EMARK

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
      HEAD  = 0.0
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
                  CNUM = CNUM*10.+DBLE(INDEX(NUMS,CHK)-1)
               ELSE
                  FDEC = FDEC/10.
                  FDC1 = FDEC*DBLE(INDEX(NUMS,CHK)-1)
                  CNUM = CNUM+FDC1
               END IF
            ELSE
C              Handle The E-Type Real Number
               IF (DMARK .AND. .NOT.EMARK .AND. CHK.EQ.'E') THEN
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
                  IMUTI = NINT(CNUM)
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
            FNUM = HEAD * 10.**30.
         END IF
         GO TO 9999
      END IF

      GO TO 1000

C     Set Error Switch for Illegal Numerical Field (WRITE Message and Handle
C     Error in Calling Routine)
 9999 IMUTI = -1

 1000 RETURN
      END SUBROUTINE STONUM

      SUBROUTINE STODBL(STRVAR,LEN,FNUM,IMUTI)
C***********************************************************************
C        Subroutine STODBL Adapted from AERMOD for AERSURFACE
C
C        PURPOSE: Gets Double Precision of Real Number
C                 From A Character String 
C
C        ORIGINAL PROGRAMMER: Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To Change Exponent Limit for Out-of-range
C                    Inputs - 9/29/92
C
C        INPUTS:  Input String Variable
C                 Length of Character String
C
C        OUTPUTS: Double Precision Real Numbers
C
C***********************************************************************
C
C     Variable Declarations

      IMPLICIT NONE

      CHARACTER (LEN=*), INTENT(IN)    :: STRVAR
      INTEGER,           INTENT(IN)    :: LEN
      INTEGER,           INTENT(INOUT) :: IMUTI
      DOUBLE PRECISION,  INTENT(OUT)   :: FNUM
     
      CHARACTER (LEN=1)                :: CHK
      CHARACTER (LEN=10)               :: NUMS
      INTEGER                          :: I
      DOUBLE PRECISION                 :: FDEC, FDC1, HEAD
      DOUBLE PRECISION                 :: CNUM
      LOGICAL                          :: MEND, IN, NMARK, PMARK
      LOGICAL                          :: DMARK, MMARK, EMARK

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
      CNUM = 0.0D0
      HEAD = 0.0D0
      FDEC = 1.0D0
      IMUTI = 1

C     Beginning the Processing
      DO WHILE (.NOT.MEND .AND. I.LE.LEN)
         CHK = STRVAR(I:I)
         IF (CHK .NE. ' ') THEN
            IN = .TRUE.
            IF (CHK.GE.'0' .AND. CHK.LE.'9') THEN
C              CHK is a Number, Assign a Value
               IF (.NOT. DMARK) THEN
                  CNUM = CNUM*10.0D0+DBLE(INDEX(NUMS,CHK)-1)
               ELSE
                  FDEC = FDEC/10.0D0
                  FDC1 = FDEC*DBLE(INDEX(NUMS,CHK)-1)
                  CNUM = CNUM+FDC1
               END IF
            ELSE
C              Handle The E-Type (or D-Type) Real Number
               IF ((DMARK .AND. .NOT.EMARK .AND. CHK.EQ.'E') .OR.
     &             (DMARK .AND. .NOT.EMARK .AND. CHK.EQ.'D')) THEN
                  EMARK = .TRUE.
                  IF (.NOT.NMARK) THEN
                     HEAD = CNUM
                  ELSE
                     HEAD = -CNUM
                  END IF
                  DMARK = .FALSE.
                  NMARK = .FALSE.
                  CNUM = 0.0D0
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
     &            .NOT.NMARK) THEN
C                 Set Repeat Indicator
                  MMARK = .TRUE.
                  IMUTI = IDNINT(CNUM)
                  CNUM = 0.0D0
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

C     In Case Of Negative Field, Value set to Negative
      IF (NMARK) THEN
         FNUM = -FNUM
      END IF

C     In Case of *E* Format, Check for Exponents Out of Range
      IF (EMARK .AND. DABS(FNUM) .LE. 30.0D0) THEN
         FNUM = HEAD*10**(FNUM)
      ELSE IF (EMARK .AND. DABS(FNUM) .GT. 30.0D0) THEN
         IF (FNUM .LT. 0.0D0) THEN
            FNUM = 0.0D0
         ELSE IF (FNUM .GT. 0.0D0) THEN
            FNUM = HEAD * 10.0D0**30.
         END IF

         GO TO 9999
      END IF

      GO TO 1000

C     Set Error Switch for Illegal Numerical Field (WRITE Message and Handle
C     Error in Calling Routine)
 9999 IMUTI = -1

 1000 RETURN
      END SUBROUTINE STODBL
      

      SUBROUTINE STRAIGHTSORT(NSect, DblArray)
C***********************************************************************
C     SUBROUTINE STRAIGHTSORT
C 
C     Purpose: To sort an array of wind directions into ascending order
C              by straight insertion (not efficient, but is fine for
C              small N)
C
C     Assumptions: If 360 is enterd as a starting direction, it will
C                  be converted to to 0
C
C     I/O:  NSect = Number of wind direction sectors entered by the user
C           DblArray() = Array of wind sectors
C
C     Called By: main program
C
C     Calls To:  <none>
C
C***********************************************************************

      
      use Constants, only: Eps5
      
      IMPLICIT NONE

      INTEGER (kind=4), INTENT(IN)     :: NSect
      DOUBLE PRECISION, INTENT(INOUT)  :: DblArray(NSect)

      INTEGER (kind=4)                 :: i, j
      DOUBLE PRECISION                 :: TempVal

      DO j = 1,NSect
         IF( abs(DblArray(j) - 360.0D0) < Eps5 )THEN
             DblArray(j) = 0.0D0
            EXIT
         ENDIF
      END DO

      DO j = 2, NSect
         TempVal = DblArray(j)
         DO i = j-1, 1, -1
            IF (DblArray(i) .LE. TempVal ) GO TO 100
            DblArray(i+1) = DblArray(i)
         ENDDO
         i = 0
  100    DblArray(i+1) = TempVal
      ENDDO

      RETURN
      END SUBROUTINE STRAIGHTSORT
      
      SUBROUTINE COCARD
C***********************************************************************
C        COCARD Procedure Adapted from AERMOD for AERSURFACE
C
C        PURPOSE: To process COntrol Pathway card images
C
C        ORIGINAL PROGRAMMERS: Roger Brode, Jeff Wang
C
C        OUTPUTS: Processing Option Switches
C                 Option Setup Status Switches
C
C***********************************************************************
C     Variable Declarations
      USE FileUnits, only: NGPath
      USE ErrorHandling, only: ERRHDL
      USE UserParams, only: SectKyWds, NumSectors, TemporalRes,
     &      CenterUTME, CenterUTMN, CenterUTMZone,
     &      CenterLatIn, CenterLonIn, Datum, 
     &      SnowChr, Snow, AridChr, Arid,
     &      SeasonMnths, WinNoSnMnths, WinWSnMnths, SprgMnths,
     &      SumrMnths, AutmMnths
     
      USE Geographic, only: UTMGEO, NGRIDS, NADCON
      
      IMPLICIT NONE
      
c      CHARACTER (len=ModLen), parameter   ::  ModNam = 'COCARD'
      
      CHARACTER*3 TmpMnth

      INTEGER (KIND=4)       :: I,J        ! loop counter     
      INTEGER (KIND=4)       :: NDX = 0      ! Keyword status array index
      INTEGER (KIND=4), SAVE :: PRVNDX = 0   ! Array index for previous keyword
      
C --- Assign logical variable for first call to SUB_COCARD to avoid reinitializing
C     SnowChr, AridChr, Snow and Arid variable for each CO record!
      LOGICAL       ::  L_FirstCall = .TRUE.
      LOGICAL, SAVE ::  GOTCNTRXY  = .FALSE.  ! Flag for CENTERXX
      LOGICAL, SAVE ::  GOTCNTRLL  = .FALSE.  ! Flag for CENTERLL
      
      DOUBLE PRECISION :: tmpLat,tmpLon  ! temp lat/lon for alaska check 
      INTEGER (KIND=4) :: tmpZone        ! temp utm zone - dummy variable
      INTEGER (KIND=4) :: tmpSphere      ! tmp spheroid for call to UTMGEO
      
      integer (kind=4)   :: Key     ! key for datum shift
      logical            :: NADShft ! Flag to indicate if NAD shift is necessary
      logical            :: NODATA     ! flag for existence of NAD Grid files
      double precision   :: XPTIN, YPTIN, XPTOUT, YPTOUT  ! for NADCON
      double precision   :: DLOS, DLAS, DLOM, DLAM  ! for NADCON
      
      ModNam = 'COCARD'      

C --- Initialize logical flags     
      NODATA = .false.    
      NADShft = .false.

C --- Initialize default Snow Cover and Arid variables since CLIMATE keyword is optional:
C     Check for first call to sub_COCARD to avoid reinitializing SnowChr, Snow, AridChr and Arid
C     variable with each call!
      IF( L_FirstCall )THEN
         SnowChr = 'N'
         Snow    = .false.
         AridChr = 'N'
         Arid    = .false.
         L_FirstCall = .false.
      ENDIF

C     Get status array index
      DO I=1,SIZE(COWRDS)
         IF (KEYWRD .EQ. COWRDS(I)) THEN
            NDX = I
            EXIT
         END IF
      END DO

C     Set increment status array for keyword
      COSTAT(NDX) = COSTAT(NDX) + 1
      
      SELECT CASE(KEYWRD)
         CASE('STARTING')
            ISTART = .TRUE.
            IF (COSTAT(NDX) .NE. 1) THEN
C              WRITE Error Message: Repeat Non-repeatable Keyword
               CALL ERRHDL(PATH,ModNam,'E','135',KEYWRD,ILINE)
            END IF
         
         CASE('TITLEONE')
            IF (COSTAT(NDX) .NE. 1) THEN
C              WRITE Error Message: Repeat Non-repeatable Keyword
               CALL ERRHDL(PATH,ModNam,'E','135',KEYWRD,ILINE)
            ELSE
C              Process Titles                                  ---   CALL TITLES
               CALL TITLES
            END IF
         
         CASE('TITLETWO')
            IF (COSTAT(NDX) .NE. 1) THEN
C              WRITE Error Message: Repeat Non-repeatable Keyword
               CALL ERRHDL(PATH,ModNam,'E','135',KEYWRD,ILINE)
            ELSE
C              Process Titles                                  ---   CALL TITLES
               CALL TITLES
            END IF

         CASE('OPTIONS')
            IF (COSTAT(NDX) .NE. 1) THEN
C              WRITE Error Message: Repeat Non-repeatable Keyword
               CALL ERRHDL(PATH,ModNam,'E','135',KEYWRD,ILINE)
            ELSE
               CALL OPTIONS
            END IF

         CASE('DEBUGOPT')
            IF (COSTAT(NDX) .NE. 1) THEN
C              WRITE Error Message: Repeat Non-repeatable Keyword
               CALL ERRHDL(PATH,ModNam,'E','135',KEYWRD,ILINE)
            ELSE
               CALL DEBUGOPT
            END IF

         CASE('DATAFILE')
            CALL DATAFILE

         CASE('CENTERXY')
            GOTCNTRXY = .TRUE.
            IF (COSTAT(NDX) .NE. 1) THEN
C              WRITE Error Message: Repeat Non-repeatable Keyword
               CALL ERRHDL(PATH,ModNam,'E','135',KEYWRD,ILINE)
               GO TO 999
            ELSE
C              Get coordinates and datum
               CALL CENTERLL
            END IF

         CASE('CENTERLL')
            GOTCNTRLL = .TRUE.
            IF (COSTAT(NDX) .NE. 1) THEN
C              WRITE Error Message: Repeat Non-repeatable Keyword
               CALL ERRHDL(PATH,ModNam,'E','135',KEYWRD,ILINE)
               GO TO 999
            ELSE
C              Get coordinates and datum
               CALL CENTERLL
            END IF

         CASE('ANEM_HGT')
            IF (COSTAT(NDX) .NE. 1) THEN
C              WRITE Error Message: Repeat Non-repeatable Keyword
               CALL ERRHDL(PATH,ModNam,'E','135',KEYWRD,ILINE)
               GO TO 999
            ELSE
C              Get anemometer height entered by user - required if
C              ZOEFF entered on OPTIONS keyword
               AnemHgtFlg = .true.
               CALL ANEM_HGT
            END IF
            
         CASE('ZORADIUS')
            IF (COSTAT(NDX) .NE. 1) THEN
C              WRITE Error Message: Repeat Non-repeatable Keyword
               CALL ERRHDL(PATH,ModNam,'E','135',KEYWRD,ILINE)
            ELSE
               CALL ZORADIUS
            END IF            

         CASE('CLIMATE')
            IF (COSTAT(NDX) .NE. 1) THEN
C              WRITE Error Message: Repeat Non-repeatable Keyword
               CALL ERRHDL(PATH,ModNam,'E','135',KEYWRD,ILINE)
               GO TO 999
            ELSE
               CALL CLIMATE
            END IF

         CASE('FREQ_SECT')
CCRT        FIND INDEX FOR KEYWORD
            DO I=1, SIZE(COWRDS)
               IF (COWRDS(I) == "SECTOR") EXIT
            END DO

            IF (COSTAT(I) .GT. 1) THEN
               CALL ERRHDL(PATH,ModNam,'E','140',KEYWRD,ILINE)
               FreqSectErr = .true.
            END IF 
            
            IF (COSTAT(NDX) .NE. 1) THEN
C              WRITE Error Message: Repeat Non-repeatable Keyword
               CALL ERRHDL(PATH,ModNam,'E','135',KEYWRD,ILINE)
               FreqSectErr = .true.
            ELSE
               CALL FREQSECT
            END IF

         CASE('SECTOR')
CCRT        FIND INDEX FOR KEYWORD
            DO I=1, SIZE(COWRDS)
               IF (COWRDS(I) == "FREQ_SECT") EXIT
            END DO

C           IF FREQ_SECT not processed 
            IF (COSTAT(I) .NE. 1) THEN
               CALL ERRHDL(PATH,ModNam,'E','140',KEYWRD,ILINE)            

C           If there was an error processing the FREQ_SECT keyword, 
C           print error that SECTOR could not be processed
            ELSEIF (FreqSectErr) THEN
               CALL ERRHDL(PATH,ModNam,'E','140',KEYWRD,ILINE)
            
C           process SECTOR                          
            ELSE
            
C              Set/reset the number of sectors based on repeated SECTOR keyword 
               SectKyWds = COSTAT(NDX)
            
C              Number of sectors cannot exceed number on FREQ_SECT
               IF (SectKyWds .GT. NumSectors) THEN
                  CALL ERRHDL(PATH,ModNam,'E','205',KEYWRD,ILINE)
               ELSEIF (.not. SectorErr) THEN
                  CALL SECTOR
               END IF
               
            END IF
 
         CASE('SEASON')
            ! only applicable temporal resolution (frequency) is Annual or Monthly
            IF (TemporalRes .EQ. 'S' .OR. COSTAT(NDX) .GT. 5) THEN
               CALL ERRHDL(PATH,ModNam,'E','210',KEYWRD,ILINE)
            ELSE
               CALL SEASON
            END IF

         CASE('PROJPRMS')
C           RESERVED
C           WRITE Error Message: Keyword currently not implemented
            CALL ERRHDL(PATH,ModNam,'E','105',KEYWRD,ILINE)

         CASE('RUNORNOT')
            IF (COSTAT(NDX) .NE. 1) THEN
C              WRITE Error Message: Repeat Non-repeatable Keyword
               CALL ERRHDL(PATH,ModNam,'E','135',KEYWRD,ILINE)
            ELSE
C              Process Option to Run Model or Not              ---   CALL RUNNOT
               CALL RUNNOT
            END IF
            
C ---    ***************************************************************
C                   Post-processing and input validation/QA
C ---    ***************************************************************

         CASE( 'FINISHED')
C           Set Status Switch
            IFINIS = .TRUE.
C        ---------------------------------------------------------------
C ---       Assign Months-to-Seasons strings for output

            IF (Snow) THEN
               WinNoSnMnths = ''
               WinWSnMnths  = ''
            ELSE
               WinNoSnMnths = ''
               WinWSnMnths  = ''
            ENDIF
            SprgMnths = ''
            SumrMnths = ''
            AutmMnths = ''    
            
C ---       Loop through five "seasons"
            DO I = 1, 5 
            
C ---        Loop through months to assign to "seasons"
             DO J = 1, 12
            
               IF( Snow )THEN
                  IF( SeasonMnths(J) .eq. I )THEN
                     IF( I .EQ. 1 )THEN
                        WRITE(TmpMnth,'(I3)') J
                        WinNoSnMnths = 
     &                  WinNoSnMnths(1:LEN_TRIM(WinNoSnMnths))//TmpMnth
                     ELSEIF( I .EQ. 2 )THEN
                        WRITE(TmpMnth,'(I3)') J
                        WinWSnMnths = 
     &                  WinWSnMnths(1:LEN_TRIM(WinWSnMnths))//TmpMnth
                     ELSEIF( I .EQ. 3 )THEN
                        WRITE(TmpMnth,'(I3)') J
                        SprgMnths = 
     &                  SprgMnths(1:LEN_TRIM(SprgMnths))//TmpMnth
                     ELSEIF( I .EQ. 4 )THEN
                        WRITE(TmpMnth,'(I3)') J
                        SumrMnths = 
     &                  SumrMnths(1:LEN_TRIM(SumrMnths))//TmpMnth
                     ELSEIF( I .EQ. 5 )THEN
                        WRITE(TmpMnth,'(I3)') J
                        AutmMnths = 
     &                  AutmMnths(1:LEN_TRIM(AutmMnths))//TmpMnth
                     ENDIF
                  ENDIF
               ELSE
                  IF( SeasonMnths(J) .eq. I )THEN
                     IF( I .EQ. 1 )THEN
                        WRITE(TmpMnth,'(I3)') J
                        WinNoSnMnths = 
     &                  WinNoSnMnths(1:LEN_TRIM(WinNoSnMnths))//TmpMnth
                     ELSEIF( I .EQ. 3 )THEN
                        WRITE(TmpMnth,'(I3)') J
                        SprgMnths = 
     &                  SprgMnths(1:LEN_TRIM(SprgMnths))//TmpMnth
                     ELSEIF( I .EQ. 4 )THEN
                        WRITE(TmpMnth,'(I3)') J
                        SumrMnths = 
     &                  SumrMnths(1:LEN_TRIM(SumrMnths))//TmpMnth
                     ELSEIF( I .EQ. 5 )THEN
                        WRITE(TmpMnth,'(I3)') J
                        AutmMnths = 
     &                  AutmMnths(1:LEN_TRIM(AutmMnths))//TmpMnth
                     ENDIF
                  ENDIF
                ENDIF
            
             ENDDO

            ENDDO

C ----------------------------------------------------------------------
C           WRITE Error Message if repeated: Repeat Non-repeatable 
C           Keyword
C ----------------------------------------------------------------------

            IF (COSTAT(NDX) .NE. 1) THEN
               CALL ERRHDL(PATH,ModNam,'E','135',KEYWRD,ILINE)
               GO TO 999
            END IF

C ----------------------------------------------------------------------
C           Check for Missing Mandatory Keywords
C ----------------------------------------------------------------------

            DO I=1,CONUM
               IF (COMAND(I) .EQ. 'T' .AND. COSTAT(I) .EQ. 0) THEN
                  CALL ERRHDL(PATH,ModNam,'E','130',COWRDS(I),ILINE)
               END IF
            END DO

C ----------------------------------------------------------------------           
C           Only one of PRIMARY and SECONDARY options allowed
C ----------------------------------------------------------------------
            
            IF (PrimaryFlg .AND. SecondaryFlg) THEN
                 CALL ERRHDL(PATH,ModNam,'E','200','OPTIONS',ILINE)
            END IF


C ----------------------------------------------------------------------
C           Only one of ZORAD and ZOEFF options allowed
C ----------------------------------------------------------------------

            IF (ZoEffFlg .AND. ZoRadFlg) THEN
                 CALL ERRHDL(PATH,ModNam,'E','201','OPTIONS',ILINE)
            END IF
            
C ----------------------------------------------------------------------
C          If OPTION ZORAD and DEBUGOPT  EFFRAD, issue warning
C          Effective radius debug option only applies to ZOEFF option
C          and will not be generated
C ----------------------------------------------------------------------

            IF (ZoRadFlg .AND. EffRadDbg) THEN
                 CALL ERRHDL(PATH,ModNam,'W','209','DBGOPT',ILINE)
            END IF

C ----------------------------------------------------------------------
C           Only one of ANEM_HGT and ZORADIUS needed based on Zo_Method.
C           ZOEFF requires ANEM_HGT, ZORAD is optional for ZORADIUS. 
C           Default of 1 km is used if ZORADIUS is not provided.
C           If both are provided, ignore the one not needed.
C ----------------------------------------------------------------------

            IF (ZoEffFlg .AND. .NOT. AnemHgtFlg) THEN
                CALL ERRHDL(PATH,ModNam,'E','202','ANEM_HGT',ILINE)
            END IF

C ----------------------------------------------------------------------            
C           Special check for CENTERXY AND CENTERLL, one or the other 
C           is required
C ----------------------------------------------------------------------

            IF (.NOT. GOTCNTRXY .AND. .NOT. GOTCNTRLL) THEN
               CALL ERRHDL(PATH,ModNam,'E','130',
     &                     'CENTERXY or CENTERLL',ILINE)
C           Special check, cannot have both CENTERXY AND CENTERLL
            ELSE IF (GOTCNTRXY .AND. GOTCNTRLL) THEN
                CALL ERRHDL(PATH,ModNam,'E','170',
     &                      'CENTERXY or CENTERLL',ILINE)
            END IF   
            
C ----------------------------------------------------------------------            
C           Special check for Alaska coordinates; If user entered UTMs,
C           convert to Lat/Lon.  This will be used to generate warnings/
C           errors specific to Alaska 
C ----------------------------------------------------------------------
 
            IF (GOTCNTRXY) THEN
             
              ! Determine spheroid for call to UTMGEO
              ! NAD27 = 0 (Clark 1866); NAD83 and WGS-84 = 4 (GRS-80)
              SELECT CASE(Datum)
                 CASE("NAD27")
                    tmpSphere = 0
                 CASE("NAD83")
                    tmpSphere = 4
              END SELECT
       
              ! call UTMGEO to convert to lat/lon         
              CALL UTMGEO (333, CenterUTMZone,tmpZone,CenterUTME, 
     &                          CenterUTMN,tmpLon,tmpLat,tmpSphere)

              ! lat/lon are returned in seconds
              tmpLat = tmpLat/3600.0D0
              tmpLon = tmpLon/3600.0D0
              
            ELSEIF (GOTCNTRLL) THEN
              tmpLat = CenterLatIn
              tmpLon = CenterLonIn         
            END IF   
            
C-----------------------------------------------------------------------
C           Determine if NAD shift is needed.
C-----------------------------------------------------------------------
      ! Call NADCON 2.1 code to compute DATUM shifts and convert
      ! geographic coordinates between NAD27 and NAD83, and vice versa.
      
      ! NADCON(XPT,YPT,XPT2,YPT2,DLOS,DLAS,DLOM,DLAM,KEY)
      ! XPT, YPT: NAD27 coordinates input to NADCON when converting from 
      !   NAD27 to NAD83. However, when converting from NAD83 to NAD27,
      !   these coordinates are both input and output values.  Use to
      !   input NAD83 values and returned NAD27 values converted from
      !   NAD83 (due to an interative guess/check process in NADCON). 
      !   Input as postive decimal degrees.
      ! XPT2, YPT2: NAD83 coordinates returned from NADCON when converting
      !   from NAD27 to NAD83.  
      ! DLOS, DLAS: Lon/Lat shift in seconds (returned)
      ! DLOM, DLOS: Lon/Lat shift in meters (returned)
      ! KEY: +1 indicates a transformation of NAD 27 to NAD 83 datums
      !      -1 indicates a psuedo-transformation of NAD 83 to NAD 27 
      !      datums
          
C    ------------------------------------------------------------           
C    WARNING: Use temp variables for input and output coordinates.   
C             NADCONwill reverse the signs of longitudinal values 
C             under certain conditions.
C    ------------------------------------------------------------
     
       ! Set the KEY parameter needed by NADCON which indicates the
       !  direction of the conversion. 
       ! KEY = +1 indicates a transformation of NAD 27 to 
       !       NAD 83 datums
       ! KEY = -1 indicates a psuedo-transformation of NAD 83 to 
       !       NAD 27 datums
      
            ! NLCD File datum
            SELECT CASE(Datum)
               CASE("NAD27")
                  Key = +1
                  CALL NGRIDS (NODATA,.true.,0,NGPath)
                  IF( NODATA )THEN
                     CALL ERRHDL(PATH,ModNam,'E','206',
     &                    'CENTERXY or CENTERLL',ILINE)
                     RETURN
                  ENDIF

                  XPTIN =  tmpLon
                  YPTIN =  tmpLat
            
                  CALL NADCON(XPTIN,YPTIN,XPTOUT,YPTOUT,DLOS,DLAS,
     &                  DLOM,DLAM,KEY)
                  ! set converted center coordinates
                  tmpLon =   XPTOUT
                  tmpLat =   YPTOUT

            END SELECT
            
         CASE DEFAULT
C           Write Error Message: Invalid Keyword for This Pathway
            CALL ERRHDL(PATH,ModNam,'E','110',KEYWRD,ILINE)
      
      END SELECT

C     Set previous index to current index
      PRVNDX = NDX

 999  RETURN
      END SUBROUTINE COCARD

      SUBROUTINE TITLES
C***********************************************************************
C        TITLES Procedure Adapted from AERMOD for AERSURFACE
C
C        PURPOSE: Process Title Information From Runstream Input Image
C
C        ORIGINAL PROGRAMMERS: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Title Strings for Model Outputs
C***********************************************************************

C     Variable Declarations
      USE ErrorHandling, only: ERRHDL
      
      IMPLICIT NONE
      ModNam = 'TITLES'

      IF (KEYWRD .EQ. 'TITLEONE') THEN
         TITLE1 = RUNST1(LOCE(2)+2:MIN(LEN_TRIM(RUNST1),
     &                                         (LOCE(2)+2+ILEN_FLD-1)))
         IF (TITLE1 .EQ. ' ') THEN
C*          Write Error Message: Missing Parameter Title
            CALL ERRHDL(PATH,ModNam,'E','230',KEYWRD,ILINE)
         END IF

      ELSE IF (KEYWRD .EQ. 'TITLETWO') THEN
         TITLE2 = RUNST1(LOCE(2)+2:MIN(LEN_TRIM(RUNST1),
     &                                         (LOCE(2)+2+ILEN_FLD-1)))
         IF (LEN_TRIM(TITLE2) .EQ. 0) THEN
C*          Write Warning Message
            CALL ERRHDL(PATH,ModNam,'W','235',KEYWRD,ILINE)
         END IF

      END IF

      RETURN
      END SUBROUTINE TITLES

      SUBROUTINE OPTIONS
C***********************************************************************
C
C        OPTS Procedure Adapted from AERMOD for AERSURFACE
C
C        PURPOSE: OPTIONS From Runstream Input Image
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Modeling Option Logical Switch Settings
C
C        ERROR HANDLING:   Checks for Too Few or Too Many Option Keywords;
C                          Checks for Invalid Option Keywords;
C                          Checks for Conflicting or Missing Option Keywords
C***********************************************************************

C     Variable Declarations
      USE UserParams, only: StatnOrdr, Zo_Method
      USE ErrorHandling, only: ERRHDL
      
      IMPLICIT NONE
      
c      CHARACTER (len=ModLen), parameter   ::  ModNam = 'OPTIONS'
      INTEGER (kind=4)                    ::  i  ! loop counter

      ModNam = 'OPTIONS'
      
C     Set check number of params
      IF (IFC .LT. 3) THEN
C        WRITE Error Message     ! No Parameters
         CALL ERRHDL(PATH,ModNam,'E','150',KEYWRD,ILINE)
      ELSEIF (IFC .GT. 4) THEN
C        WRITE Warning Message   ! Too Many Parameters
         CALL ERRHDL(PATH,ModNam,'W','155',KEYWRD,ILINE)
      ENDIF
      
C     Set options
      IF (IFC .GT. 2) THEN
      
         DO i=3,IFC

         SELECT CASE (FIELD(i))
         
                CASE ('PRIMARY')

                    StatnOrdr = 'PRIMARY'
                    PrimaryFlg = .true.

                CASE ('SECONDARY')

                    StatnOrdr = 'SECONDARY'
                    SecondaryFlg = .true.
         
                CASE ('ZOEFF')

                    Zo_Method = 'ZOEFF'
                    ZoEffFlg = .true.

                CASE ('ZORAD')

                    Zo_Method = 'ZORAD'
                    ZoRadFlg = .true.

                CASE DEFAULT
                    CALL ERRHDL(PATH,ModNam,'E','105',FIELD(i),ILINE)
         
            END SELECT
            
        END DO
        
C       
         
      END IF

      END SUBROUTINE OPTIONS
      
      
      SUBROUTINE DEBUGOPT
C***********************************************************************
C        OPTS Procedure Adapted from AERMOD for AERSURFACE
C
C        PURPOSE: OPTIONS From Runstream Input Image
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Modeling Option Logical Switch Settings
C
C        ERROR HANDLING:   Checks for Too Few or Too Many Option Keywords;
C                          Checks for Invalid Option Keywords;
C                          Checks for Conflicting or Missing Option Keywords
C***********************************************************************

C     Variable Declarations
      USE ErrorHandling, only: ERRHDL
      
      IMPLICIT NONE
c      CHARACTER (len=ModLen), parameter   ::  ModNam = 'DEBUGOPT'
      INTEGER (kind=4)                    ::  i  ! counter

      ModNam = 'DEBUGOPT'
      
C     Set check number of params
      IF (IFC .LT. 3) THEN
C        WRITE Error Message     ! No Parameters
         CALL ERRHDL(PATH,ModNam,'E','150',KEYWRD,ILINE)
      ELSEIF (IFC .GT. 5) THEN
C        WRITE Warning Message   ! Too Many Parameters
         CALL ERRHDL(PATH,ModNam,'W','155',KEYWRD,ILINE)
      ENDIF
      
C     Get debug option
      IF (IFC .GT. 2) THEN
      
         DO i=3,IFC
         
            SELECT CASE (FIELD(i))
            
               CASE ('EFFRAD')
                  EffRadDbg = .true.
                  
               CASE ('GRID')
                  GridDbg = .true.
                  
               CASE ('TIFF')
                  TiffDbg = .true.
                  
               CASE ('ALL')
                  EffRadDbg = .true.
                  GridDbg = .true.
                  TiffDbg = .true.
   
               CASE DEFAULT
                  CALL ERRHDL(PATH,ModNam,'E','105',KEYWRD,ILINE)
            
            END SELECT
         
         END DO
         
      END IF
      
      RETURN
      END SUBROUTINE DEBUGOPT
      
      
      SUBROUTINE DATAFILE
C***********************************************************************
C        DATAFILE Procedure Adapted from AERMOD for AERSURFACE
C
C        PURPOSE: Process File Options NLCD, impervious, and canopy data
C                 from Runstream Input Image associated with the primary
C                 location (generally the met tower).
C                 
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: File Logical Switches and Filenames
C
C        ERROR HANDLING:   Checks for No Parametes;
C                          Checks for Too Many Parameters
C
C***********************************************************************

C     Variable Declarations
      USE Constants, only: FilNmLen
      USE UserParams, only: LCFile, ImpFile, CanFile
      USE ErrorHandling, only: ERRHDL

      IMPLICIT NONE
      
c      CHARACTER (len=ModLen), parameter  ::  ModNam = 'DATAFILE' ! Module/Procedure name
      CHARACTER (LEN=FilNmLen)           ::  DFILE   ! File name retrieved

      ModNam = 'DATAFILE'
      
C     Retrieve Included Filename as Character Substring to Maintain Case
      IF ((LOCE(4)-LOCB(4)) .LE. (ILEN_FLD - 1) ) THEN
C           Retrieve Filename as Character Substring to Maintain Original Case
C           Also Check for Filename Larger Than ILEN_FLD Characters
            DFILE = RUNST1(LOCB(4):LOCE(4))

C           Assign file name to file variable, set year of data
C           NLCDYear.
            SELECT CASE(FIELD(3))
C              Assign full Data Type based on FIELD(3) 
               CASE("NLCD1992")
                  LCFile   = DFILE
                  NLCDYear = 1992
                  GotLCD   = .true.
               CASE("NLCD2001")
                  LCFile   = DFILE
                  NLCDYear = 2001
                  GotLCD   = .true.
               CASE("NLCD2006")
                  LCFile   = DFILE
                  NLCDYear = 2006
                  GotLCD   = .true.
               CASE("NLCD2011")
                  LCFile   = DFILE
                  NLCDYear = 2011
                  GotLCD   = .true.
               CASE("NLCD2016")
                  LCFile   = DFILE
                  NLCDYear = 2016
                  GotLCD   = .true.
               CASE("MPRV2001")
                  ImpFile  = DFILE
                  ImpYear  = 2001
                  GotImp   = .true.
               CASE("MPRV2006")
                  ImpFile  = DFILE
                  ImpYear  = 2006
                  GotImp   = .true.
               CASE("MPRV2011")
                  ImpFile  = DFILE
                  ImpYear  = 2011
                  GotImp   = .true.
               CASE("MPRV2016")
                  ImpFile  = DFILE
                  ImpYear  = 2016
                  GotImp   = .true.
               CASE("CNPY2001")
                  CanFile  = DFILE
                  CanYear  = 2001
                  GotCan   = .true.
               CASE("CNPY2006")
                  CanFile  = DFILE
                  CanYear  = 2006
                  GotCan   = .true.
               CASE("CNPY2011")
                  CanFile  = DFILE
                  CanYear  = 2011
                  GotCan   = .true.
               CASE("CNPY2016")
                  CanFile  = DFILE
                  CanYear  = 2016
                  GotCan   = .true.
               CASE DEFAULT
                  CALL ERRHDL(PATH,ModNam,'E','110',KEYWRD,ILINE)
            END SELECT

      ELSE
C        WRITE Error Message:  Filename is Too Long
         CALL ERRHDL(PATH,ModNam,'E','240',KEYWRD,ILINE)
         RETURN
      END IF

      RETURN
      END SUBROUTINE DATAFILE


      SUBROUTINE CENTERLL
C***********************************************************************
C        CENTERLL Procedure for AERSURFACE
C
C        PURPOSE: Process Lat/Lon, UTM Coordinates
C                 From Runstream Input Image
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Study site coordinates and reference datum
C
C        ERROR HANDLING:   Checks for Too Few Parametes;
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   SICARD
C***********************************************************************

C     Variable Declarations
      USE UserParams, only: CoordType, CenterUTME, CenterUTMN,
     &    CenterUTMZone, Datum, CenterLatIn, CenterLonIn
      USE ErrorHandling, only: ERRHDL

      IMPLICIT NONE  

c      CHARACTER (len=ModLen), parameter  ::  ModNam = 'CENTERLL'
      
      DOUBLE PRECISION    :: TMPZONE  ! UTM zone returned as real number (temporary)

      ModNam = 'CENTERLL'
      
C     Initialize error flag
      IMIT = 1      
      
c     Initialize temp utm zone (only used if utm coords entered)
      TMPZONE = 0
          
C     Set coordinate type, check number of params, get inputs

C     UTM
      IF (KEYWRD .EQ. 'CENTERXY') THEN
         CoordType = 'UTM'
         IF (IFC .LT. 6) THEN
C           WRITE Error Message     ! No Parameters
            CALL ERRHDL(PATH,ModNam,'E','150',KEYWRD,ILINE)
         ELSEIF (IFC .GT. 6) THEN
C           WRITE Warning Message   ! Too Many Parameters
            CALL ERRHDL(PATH,ModNam,'W','155',KEYWRD,ILINE)
         ENDIF

C        Get easting coordinate
         CALL STODBL(FIELD(3), ILEN_FLD, CenterUTME, IMIT)
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,ModNam,'E','245','CENTERXY',ILINE)
         END IF   
      
C        Get northing coordinate
         CALL STODBL(FIELD(4), ILEN_FLD, CenterUTMN, IMIT)
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,ModNam,'E','245','CENTERXY',ILINE)
         END IF 
      
C        Get UTM zone
         CALL STONUM(FIELD(5), ILEN_FLD, TMPZONE, IMIT)
         
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,ModNam,'E','245','CENTERXY',ILINE)

C        Convert to integer
         ELSE
            CenterUTMZone = NINT(TMPZONE)
         END IF    
      
C        Get Datum
         IF (FIELD(6) .NE. 'NAD83' .AND. FIELD(6) .NE. 'NAD27') THEN
            CALL ERRHDL(PATH,ModNam,'E','250','CENTERXY',ILINE) 
         ELSE
            Datum = trim(FIELD(6))
         END IF

C     Lat/Lon
      ELSEIF (KEYWRD .EQ. 'CENTERLL') THEN
      
         CoordType = 'LATLON'
         IF (IFC .LT. 5) THEN
C           WRITE Error Message     ! No Parameters
            CALL ERRHDL(PATH,ModNam,'E','150',KEYWRD,ILINE)
         ELSEIF (IFC .GT. 5) THEN
C           WRITE Warning Message   ! Too Many Parameters
            CALL ERRHDL(PATH,ModNam,'W','155',KEYWRD,ILINE)
         ENDIF

C        Get latitude
         CALL STODBL(FIELD(3), ILEN_FLD, CenterLatIn, IMIT)
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,ModNam,'E','245','CENTERLL',ILINE)
         END IF   
      
C        Get longitude
         CALL STODBL(FIELD(4), ILEN_FLD, CenterLonIn, IMIT)
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,ModNam,'E','245','CENTERLL',ILINE)
         END IF   
      
C        Get Datum
         IF (FIELD(5) .NE. 'NAD83' .AND. FIELD(5) .NE. 'NAD27') THEN
            CALL ERRHDL(PATH,ModNam,'E','250','CENTERLL',ILINE) 
         ELSE
            Datum = trim(FIELD(5))
         END IF
         
      END IF
      
      RETURN
      END SUBROUTINE CENTERLL
          
      
      SUBROUTINE ANEM_HGT
C***********************************************************************
C        ANEM_HGT Procedure for AERSURFACE
C
C        PURPOSE: Process Anemometer Height to Compute IBL Reference Hgt
C                 From Runstream Input Image
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Anemometer Height
C
C        ERROR HANDLING:   Checks for Too Few Parametes;
C                          Checks for Too Many Parameters
C
C***********************************************************************

C     Variable Declarations
      USE UserParams, only: Anem_Height, IBL_Factor
      USE Constants, only: AnemHt_Max, AnemHt_Min, 
     &                     IBLFact_Max, IBLFact_Min

      USE ErrorHandling, only: ERRHDL

      IMPLICIT NONE  

c      CHARACTER (len=ModLen), parameter  ::  ModNam = 'ANEM_HGT'      

C     Anemometer height, maximum height value, minimum height value      
      CHARACTER (len=10)                 :: AnemHtStr,  
     &                                      AnemMaxStr, AnemMinStr   
C     IBL Factor, maximum, minimum value      
      CHARACTER (len=10)                 :: IBLFactStr,  
     &                                      IBLMaxStr, IBLMinStr   

      ModNam = 'ANEM_HGT'
     
C     Initialize error flag
      IMIT = 1      
          
C     Set check number of params
      IF (IFC .LT. 3) THEN
C        WRITE Error Message     ! No Parameters
         CALL ERRHDL(PATH,ModNam,'E','150',KEYWRD,ILINE)
         return
      ELSEIF (IFC .GT. 4) THEN
C        WRITE Warning Message   ! Too Many Parameters
         CALL ERRHDL(PATH,ModNam,'W','155',KEYWRD,ILINE)
      ENDIF
      
C     Anemometer height is required, Get anemometer height
      CALL STODBL(FIELD(3), ILEN_FLD, Anem_Height, IMIT)
      IF (IMIT == 1) THEN
         
         IF (Anem_Height < AnemHt_Min .OR. 
     &       Anem_Height > AnemHt_Max) THEN
      
            WRITE(AnemHtStr,'(f10.2)') Anem_Height
            WRITE(AnemMaxStr,'(f10.1)') AnemHt_Max
            WRITE(AnemMinStr,'(f10.1)') AnemHt_Min
     
            CALL ERRHDL(PATH,ModNam,'E','247',trim(adjustl(AnemMinStr))
     &          //" - "//trim(adjustl(AnemMaxStr))//" m",ILINE)
         END IF

         IF (IFC .EQ. 4) THEN
C           Optional IBL Factor specified
            CALL STODBL(FIELD(4), ILEN_FLD, IBL_Factor, IMIT)
            IF (IMIT == 1) THEN

               IF (IBL_Factor < IBLFact_Min .OR. 
     &             IBL_Factor > IBLFact_Max) THEN
            
                  WRITE(IBLFactStr,'(f10.2)') IBL_Factor
                  WRITE(IBLMaxStr,'(f10.1)') IBLFact_Max
                  WRITE(IBLMinStr,'(f10.1)') IBLFact_Min
     
                  CALL ERRHDL(PATH,ModNam,'E','248',
     &                       trim(adjustl(IBLMinStr))
     &                //" - "//trim(adjustl(IBLMaxStr)),ILINE)
               END IF

           ELSE
               CALL ERRHDL(PATH,ModNam,'E','245','IBL_Factor',ILINE)
           ENDIF

         ELSE
C           Assign "default" value of 6 for IBL_Factor
            IBL_Factor = 6.0

         ENDIF

      ELSEIF (IMIT /= 1) THEN
         CALL ERRHDL(PATH,ModNam,'E','245','ANEM_HGT',ILINE)

      END IF 

   
      RETURN

C     WRITE Error Message:  Error Writing to File
C 250  WRITE(*,251) trim(OutFile)
C 251  FORMAT(/,' Error Writing to Output File: ',a,//,
C     &         ' Processing Aborted!')
C      STOP

C     WRITE Error Message:  Error Opening File
C 300  WRITE(*,301) trim(LogFile)
C 301  FORMAT(/,' Error Opening Log File: ',a,//,
C     &         ' Processing Aborted!')
C      STOP

C     WRITE Error Message:  Error Writing to File
C 350  WRITE(*,351) trim(LogFile)
C 351  FORMAT(/,' Error Writing to Log File: ',a,//,
C     &         ' Processing Aborted!')
C      STOP
      
      END SUBROUTINE ANEM_HGT
      
      SUBROUTINE ZORADIUS
C***********************************************************************
C        ZORADIUS Procedure for AERSURFACE
C
C        PURPOSE: Process ZORADIUS keyword. Radius within which surface 
C                 roughness is computed when ZORAD is specified as an 
C                 OPTION.
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: ZoRad
C
C        ERROR HANDLING:   Checks for Too Few Parametes;
C                          Checks for Too Many Parameters
C
C***********************************************************************

C     Variable Declarations
      USE UserParams, only: ZoRad
      USE Constants, only: ZoRad_Max, ZoRad_Min
      USE ErrorHandling, only: ERRHDL

      IMPLICIT NONE  

C     Radius, maximum radius value, minimum radius value      
      CHARACTER (len=10)                 :: ZoRadStr,  
     &                                      ZoRadMaxStr, ZoRadMinStr   

      ModNam = 'ZORADIUS'
      
C     Initialize error flag
      IMIT = 1      
          
C     Set check number of params
      IF (IFC .LT. 3) THEN
C        WRITE Error Message     ! No Parameters
         CALL ERRHDL(PATH,ModNam,'E','150',KEYWRD,ILINE)
         return
      ELSEIF (IFC .GT. 3) THEN
C        WRITE Warning Message   ! Too Many Parameters
         CALL ERRHDL(PATH,ModNam,'W','155',KEYWRD,ILINE)
      ENDIF
      
C     Radius is required, Get radius
      CALL STODBL(FIELD(3), ILEN_FLD, ZoRad, IMIT)
      IF (IMIT == 1) THEN
         
         IF (ZoRad < ZoRad_Min .OR. 
     &       ZoRad > ZoRad_Max) THEN
      
            WRITE(ZoRadStr,'(f10.2)') ZoRad
            WRITE(ZoRadMaxStr,'(f10.1)') ZoRad_Max
            WRITE(ZoRadMaxStr,'(f10.1)') ZoRad_Min
     
            CALL ERRHDL(PATH,ModNam,'E','247',trim(adjustl(ZoRadMinStr))
     &          //" - "//trim(adjustl(ZoRadMaxStr))//" m",ILINE)
     
         ELSE
            ZoRad = ZoRad*1000.0D0

         END IF

      ELSEIF (IMIT /= 1) THEN
         CALL ERRHDL(PATH,ModNam,'E','245','ZORADIUS',ILINE)

      END IF 

      RETURN

C     WRITE Error Message:  Error Writing to File
c 250  WRITE(*,251) trim(OutFile)
c 251  FORMAT(/,' Error Writing to Output File: ',a,//,
c     &         ' Processing Aborted!')
C      STOP

C     WRITE Error Message:  Error Opening File
c 300  WRITE(*,301) trim(LogFile)
c 301  FORMAT(/,' Error Opening Log File: ',a,//,
c     &         ' Processing Aborted!')
C      STOP

C     WRITE Error Message:  Error Writing to File
C 350  WRITE(*,351) trim(LogFile)
C 351  FORMAT(/,' Error Writing to Log File: ',a,//,
C     &         ' Processing Aborted!')
C      STOP

      RETURN
      
      END SUBROUTINE ZORADIUS
      
      
      SUBROUTINE CLIMATE
C***********************************************************************
C        CLIMATE Procedure for AERSURFACE
C
C        PURPOSE: Process CLIMATE Keword
C                 From Runstream Input Image
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Climate moisture (Average, Dry, Wet)
C                 Winter snow cover (period of continuous cover)
C                 Arid conditions (Arid or non-arid)
C
C        ERROR HANDLING:   Checks for Too Few Parametes;
C                          Checks for Too Many Parameters
C                          Checks for inconsistent keywords on record
C
C***********************************************************************

C     Variable Declarations
      USE UserParams, only: Moisture, MoistStr, Snow, SnowChr,
     &    Arid, AridChr,
     &    SeasonMnths, WinNoSnMnths, WinWSnMnths, SprgMnths,
     &    SumrMnths, AutmMnths
      USE ErrorHandling, only: ERRHDL

      IMPLICIT NONE  

C     Local variables
c      CHARACTER (len=ModLen), parameter  ::  ModNam = 'CLIMATE'

      LOGICAL                  :: ARIDFLG, SnowFlg   ! Arid and Snow condition flags

      ModNam = 'CLIMATE'
      
C     Initialize arid conditions keyword flag      
      ARIDFLG = .false.
      SnowFlg = .false.

C     Check for number of parameters
      IF (IFC .LT. 3) THEN
C        WRITE Error Message     ! No Parameters
         CALL ERRHDL(PATH,ModNam,'E','150',KEYWRD,ILINE)
         RETURN
      ELSEIF (IFC .GT. 5) THEN
C        WRITE Warning Message   ! Too Many Parameters
         CALL ERRHDL(PATH,ModNam,'E','155',KEYWRD,ILINE)
         RETURN
      ENDIF

C     Surface moisture
      IF (IFC .GT. 2) THEN
      
C        Evaluate and set variables         
         SELECT CASE (FIELD(3)(1:LEN_TRIM(FIELD(3))))
         
C           Moisture
            CASE ('AVERAGE','AVG')
               Moisture = 'A'        ! global
               MoistStr = 'Average'  ! global
            CASE ('WET')
               Moisture = 'W'        ! global
               MoistStr = 'Wet'      ! global
            CASE ('DRY')
               Moisture = 'D'        ! global
               MoistStr = 'Dry'      ! global
               
            CASE DEFAULT
               CALL ERRHDL(PATH,ModNam,'E','105',KEYWRD,ILINE)
               
         END SELECT
         
      END IF

C     Get Snow Cover
      IF (IFC .GT. 3) THEN
      
         SELECT CASE (FIELD(4)(1:LEN_TRIM(FIELD(4))))

C           Snow
            CASE ('SNOW')
               SnowFlg  = .TRUE.
               Snow     = .TRUE.  ! global 
               SnowChr = "Y"
            CASE ('NOSNOW')
               SnowFlg  = .TRUE.
               Snow     = .FALSE. ! global 
               SnowChr = "N"
               
            CASE DEFAULT
               CALL ERRHDL(PATH,ModNam,'E','105',KEYWRD,ILINE)
               
         END SELECT
         
      END IF
      
      
      IF (IFC .GT. 4) THEN
      
         SELECT CASE (FIELD(5)(1:LEN_TRIM(FIELD(5))))

C           Arid
            CASE ('ARID')
               ARIDFLG  = .TRUE.  ! local flag 
               Arid     = .TRUE.  ! global 
               AridChr = "Y"
            CASE ('NONARID')
               ARIDFLG  = .TRUE.  ! local flag 
               Arid     = .FALSE. ! global
               AridChr = "N"
               
            CASE DEFAULT
               CALL ERRHDL(PATH,ModNam,'E','105',KEYWRD,ILINE)
               
         END SELECT

      END IF
      
C --- Test for conflicting user entries
C     SNOW vs. ARID  (NONARID is the default)
C     Inconsistency should only occur if user entered Arid Condition flag
      IF (ARIDFLG .AND. Arid .AND. Snow) THEN
         CALL ERRHDL(PATH,ModNam,'E','255',KEYWRD,ILINE)
      END IF
      
      
C --- adjust season/month assignments (temporary placement)
C --- special case for winter, continous snow or not
      IF (Snow .AND. SnowFlg) THEN
         SeasonMnths(1) = 2
         SeasonMnths(2) = 2
         SeasonMnths(12)= 2
      ELSEIF (Snow .AND. .NOT. SnowFlg) THEN
         SeasonMnths(1) = 1
         SeasonMnths(2) = 1
         SeasonMnths(12)= 1
      ELSE
         SeasonMnths(1) = 1
         SeasonMnths(2) = 1
         SeasonMnths(12)= 1
      ENDIF

C ----set default assignment strings for output
      IF (Snow .AND. SnowFlg) THEN
         WinNoSnMnths = '0'
         WinWSnMnths = '12 1 2'
      ELSE
         WinNoSnMnths = '12 1 2'
         WinWSnMnths = '0'
      ENDIF
      SprgMnths = '3 4 5'
      SumrMnths = '6 7 8'
      AutmMnths = '9 10 11'
      
      RETURN
      END SUBROUTINE CLIMATE
      

      SUBROUTINE FREQSECT
C***********************************************************************
C        FREQSECT Procedure for AERSURFACE
C
C        PURPOSE: Process FREQ_SECT keyword to extract temporal resolution
C                 of surface values, number of wind sectors, and if all 
C                 sectors are to be processed as an airport
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Temporal Resolution, number of sectors, airport flag
C
C        ERROR HANDLING:   Checks for Too Few Parametes;
C                          Checks for Too Many Parameters
C
C***********************************************************************

C     Variable Declarations
      USE UserParams, only: VaryAP, Airport, TemporalRes, TempResStr,
     &    NumSectors, WndSectors
      USE ErrorHandling, only: ERRHDL

      IMPLICIT NONE 
      
      DOUBLE PRECISION         :: tmpSecs ! temp number of sectors as a real number
      INTEGER (KIND=4)         :: i ! counter
      
      ModNam = 'FREQSECT'
     
C     Initialize error flag
      IMIT = 1      

C     Set check number of params
C     Conditional based on VARYAP option

      IF (IFC .LT. 5) THEN
C        WRITE Error Message     ! No Parameters
         CALL ERRHDL(PATH,ModNam,'E','150',KEYWRD,ILINE)
         FreqSectErr = .true.
      ELSEIF (IFC .GT. 5) THEN
C        WRITE Warning Message   ! Too Many Parameters
         CALL ERRHDL(PATH,ModNam,'W','155',KEYWRD,ILINE) 
         FreqSectErr = .true.
      ENDIF

C     Get temporal resolution
      IF (IFC .GT. 2) THEN
      
         SELECT CASE (FIELD(3)(1:LEN_TRIM(FIELD(3))))
     
            CASE ('ANNUAL')
               TemporalRes = 'A'        ! global
               TempResStr  = 'ANNUAL'   ! global
            CASE ('SEASONAL')
               TemporalRes = 'S'        ! global
               TempResStr  = 'SEASONAL' ! global
            CASE ('MONTHLY')
               TemporalRes = 'M'        ! global
               TempResStr  = 'MONTHLY'  ! global
           
            CASE DEFAULT
               CALL ERRHDL(PATH,ModNam,'E','105',FIELD(3),ILINE)
               FreqSectErr = .true.
         END SELECT
   
      END IF
       
C     Get number of sectors
      tmpSecs = 0
      
      IF (IFC .GT. 3) THEN
         CALL STONUM(FIELD(4), ILEN_FLD, tmpSecs, IMIT)
     
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,ModNam,'E','245','FREQ_SECT',ILINE)
            FreqSectErr = .true.
         ELSE
C           Convert number of sectors to an integer
            NumSectors = nint(tmpSecs)
            
C           Check validity of number of sectors
C           Allowable: 1...12, and 16
            IF (NumSectors .LT. 1 .OR. 
     &         (NumSectors .GT. 12 .AND. NumSectors .LT. 16) .OR.
     &         (NumSectors .GT. 16)) THEN
               CALL ERRHDL(PATH,ModNam,'E','260','FREQ_SECT',ILINE)
               FreqSectErr = .true.
            END IF
         END IF 
      END IF   

C     Get airport association flag
      IF (IFC .GT. 4) THEN
         SELECT CASE (FIELD(5)(1:LEN_TRIM(FIELD(5))))
     
         
            CASE ('AP')
C              Site is at airport
               Airport = .true.
               VaryAP  = .false.
               
            CASE ('NONAP')
C              Site is not at an airport
               Airport = .false.
               VaryAP  = .false.
         
            CASE ('VARYAP')
C              Site is at airport
               Airport = .false.
               VaryAP  = .true.
       
            CASE DEFAULT
               CALL ERRHDL(PATH,ModNam,'E','105',FIELD(5),ILINE)
               FreqSectErr = .true.
         END SELECT
         
      END IF

C     If VARYAP is false set airport flag for individual
C     sectors based on site wide airport flag.
      IF (.not.VARYAP) THEN
         DO i=1,NumSectors
            WndSectors%sec_ap(i) = Airport
         END DO 
      END IF
      
      RETURN
      
      END SUBROUTINE FREQSECT


      SUBROUTINE SECTOR
C***********************************************************************
C        SECTOR Procedure for AERSURFACE
C
C        PURPOSE: Process SECTOR keyword to extract start directions
C                 for each sector defined and fill sector array.
C                 Sets sector airport flag if varying sectors independent
C                 of one another.
C
C                 Procedure assumes the FREQ_SECT process has been
C                 processed to extract the number of sectors and
C                 whether the site-wide airport flag was set or 
C                 if the airport flag should vary by sector.
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Sector start direction, 
C                 Sector airport flag
C
C        ERROR HANDLING:   Checks for Too Few Parametes;
C                          Checks for Too Many Parameters
C
C***********************************************************************

C     Variable Declarations
      USE UserParams, only: VaryAP, NumSectors, WndSectors
      USE ErrorHandling, only: ERRHDL

      IMPLICIT NONE

      DOUBLE PRECISION         :: tmpNum  ! temp real number
      INTEGER(KIND=4)          :: tmpSec  ! temp sector
      
      ! Count how many times the SECTOR keyword has been processed with
      ! procedure; represents the current sector processing
      INTEGER(KIND=4),save     :: thisSec = 0    

      ModNam = 'SECTOR'

C     Initialize to first sector
      thisSec = thisSec+1
      
C     Initialize error flags and temp sector
      SectorErr = .false.
      IMIT = 1         
      tmpSec = 1 

C     Must always be at least 3 fields
      IF (IFC < 3) THEN
         CALL ERRHDL(PATH,ModNam,'E','150',KEYWRD,ILINE)
         SectorErr = .true.
         RETURN
      ELSE

         CALL STODBL(FIELD(3), ILEN_FLD, tmpNum, IMIT)

C        determine if id is a valid number
         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,ModNam,'E','245','SECTOR',ILINE)
            SectorErr = .true.
            RETURN
         ELSEIF (nint(tmpNum) <= 0 .or. 
     &           nint(tmpNum) > NumSectors) THEN
            CALL ERRHDL(PATH,ModNam,'E','265','SECTOR',ILINE)
            SectorErr = .true.
            RETURN
         ELSE
            tmpSec = nint(tmpNum)
            WndSectors%sec_id(thisSec) = tmpSec
         END IF
      END IF
      
C     If the first sector, check for default sector conditions 
C     and set flag accordingly. Defaults only apply for 1, 8, 12, 
C     and 16 sectors. If VARYAP flag is true, the only valid number
C     of fields is 4 and 6.  Default sectors will have only 4, and 4
C     must be AP/NONAP. If VARYAP is false, the only valid number of 
C     fields is 3 and 5 and they must be numeric.  Default sectors 
C     will have only 3 fields.
      IF (thisSec == 1) THEN
         SELECT CASE(NumSectors)
            ! default sector defs only apply to 1, 8 12, 16 sectors
            CASE(1,8,12,16)
               SELECT CASE(VARYAP)
                  CASE(.true.)      
                    ! VARYAP true and 4 fields and 4 must be AP/NONAP
                    IF (IFC == 4) THEN
                      IF((FIELD(4)(1:LEN_TRIM(FIELD(4)))).eq."AP" .or. 
     &                  (FIELD(4)(1:LEN_TRIM(FIELD(4)))).eq."NONAP")THEN
                           DfltSec = .true.
                      ELSE
                        CALL ERRHDL(PATH,ModNam,'E','160',KEYWRD,ILINE)
                        SectorErr = .true.
                        RETURN
                      END IF
                    ELSEIF (IFC == 6) THEN
                      CALL STODBL(FIELD(4), ILEN_FLD, tmpNum, IMIT)
                      IF (IMIT .NE. 1) THEN
                        CALL ERRHDL(PATH,ModNam,'E','245','SECTOR',
     &                              ILINE)
                        SectorErr = .true.
                        RETURN
                      END IF
                      CALL STODBL(FIELD(5), ILEN_FLD, tmpNum, IMIT)
                      IF (IMIT .NE. 1) THEN
                        CALL ERRHDL(PATH,ModNam,'E','245','SECTOR',
     &                              ILINE)
                        SectorErr = .true.
                        RETURN
                      END IF
                      IF((FIELD(6)(1:LEN_TRIM(FIELD(6)))).ne."AP".and.
     &                  (FIELD(6)(1:LEN_TRIM(FIELD(6)))).ne."NONAP")THEN
                        CALL ERRHDL(PATH,ModNam,'E','160',KEYWRD,ILINE)
                        SectorErr = .true.
                        RETURN
                      END IF
                      DfltSec = .false.
                    ELSEIF (IFC < 6) THEN
                        CALL ERRHDL(PATH,ModNam,'E','150',KEYWRD,ILINE)
                        SectorErr = .true.
                        RETURN
                    ELSEIF (IFC > 6) THEN
                        CALL ERRHDL(PATH,ModNam,'E','155','SECTOR',
     &                              ILINE)
                        SectorErr = .true.
                        RETURN
                    END IF
                  CASE(.false.)
C ---                VaryAP not specified
                     IF (IFC < 5) THEN
                        CALL ERRHDL(PATH,ModNam,'E','150',KEYWRD,ILINE)
                        SectorErr = .true.
                        RETURN
                     ELSE IF (IFC < 5) THEN
                        CALL ERRHDL(PATH,ModNam,'E','155',KEYWRD,ILINE)
                        SectorErr = .true.
                        RETURN
                     ELSE
                        CALL STODBL(FIELD(4), ILEN_FLD, tmpNum, IMIT)
                        IF (IMIT .NE. 1) THEN
                           CALL ERRHDL(PATH,ModNam,'E','245','SECTOR',
     &                                 ILINE)
                           SectorErr = .true.
                           RETURN
                        END IF
                        CALL STODBL(FIELD(5), ILEN_FLD, tmpNum, IMIT)
                        IF (IMIT .NE. 1) THEN
                           CALL ERRHDL(PATH,ModNam,'E','245','SECTOR',
     &                                 ILINE)
                           SectorErr = .true.
                           RETURN
                        END IF
                        DfltSec = .false.
                     END IF
               END SELECT
            CASE DEFAULT
               DfltSec = .false.
         END SELECT
      END IF
      
C     Validate field entries, all sector records
      SELECT CASE(NumSectors)
         CASE(1,8,12,16)
            IF (VARYAP .and. DfltSec) THEN
               IF (IFC < 4) THEN
C                 Too few params
                  CALL ERRHDL(PATH,ModNam,'E','150',KEYWRD,ILINE)
                  SectorErr = .true.
                  RETURN
               ELSEIF (IFC > 4) THEN
C                 Too many params
                  CALL ERRHDL(PATH,ModNam,'E','155',KEYWRD,ILINE)
                  SectorErr = .true.
                  RETURN
               END IF
C              Field 4 must be airport flag               
               SELECT CASE(FIELD(4)(1:LEN_TRIM(FIELD(4))))
                  CASE ('AP')
C                    sector treated as an airport
                     WndSectors%sec_ap(thisSec) = .true.
                  CASE ('NONAP')
C                    sector not treated as an airport
                     WndSectors%sec_ap(thisSec) = .false.
                  CASE DEFAULT
                     CALL ERRHDL(PATH,ModNam,'E','105',FIELD(4),ILINE)
                     SectorErr = .true.
                     RETURN
               END SELECT
               
            END IF

      END SELECT ! NumSectors

C     Applies to all sectors if not using default sectors        
      IF (.not. DfltSec) THEN
         IF (IFC < 5) THEN
C           Too few params
            CALL ERRHDL(PATH,ModNam,'E','150',KEYWRD,ILINE)
            SectorErr = .true.
            RETURN
         END IF
      
C        check field 4, must be numeric, 0.0-360.0
         CALL STODBL(FIELD(4), ILEN_FLD, tmpNum, IMIT)

         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,ModNam,'E','245','SECTOR',ILINE)
            SectorErr = .true.
         ELSEIF (tmpNum .LT. 0.0 .OR. tmpNum .GT. 360.0) THEN
            CALL ERRHDL(PATH,ModNam,'E','270','SECTOR',ILINE)
            SectorErr = .true.
         ELSE
            WndSectors%sec_start(thisSec) = tmpNum
         ENDIF
      
C        check field 5, must be numeric, 0.0-360.0
         CALL STONUM(FIELD(5), ILEN_FLD, tmpNum, IMIT)

         IF (IMIT .NE. 1) THEN
            CALL ERRHDL(PATH,ModNam,'E','245','SECTOR',ILINE)
            SectorErr = .true.
         ELSEIF (tmpNum .LT. 0.0 .OR. tmpNum .GT. 360.0) THEN
            CALL ERRHDL(PATH,ModNam,'E','270','SECTOR',ILINE)
            SectorErr = .true.
         ELSE
            WndSectors%sec_end(thisSec) = tmpNum
         ENDIF               

         IF (SectorErr) RETURN

      END IF

C     Applies to all sectors if not using sector defaults and
C     vary airport flag specified             
      IF (VARYAP .and. .not. DfltSec) THEN
      
         IF (IFC < 6) THEN
C           Too few params
            CALL ERRHDL(PATH,ModNam,'E','150',KEYWRD,ILINE)
            SectorErr = .true.
            RETURN
         ELSEIF (IFC > 6) THEN
C           Too many params
            CALL ERRHDL(PATH,ModNam,'E','155',KEYWRD,ILINE)
            SectorErr = .true.
            RETURN
         END IF
   
C        Field 6 must be airport flag               
         SELECT CASE(FIELD(6)(1:LEN_TRIM(FIELD(6))))
            CASE ('AP')
C              sector treated as an airport
               WndSectors%sec_ap(thisSec) = .true.
            CASE ('NONAP')
C              sector not treated as an airport
               WndSectors%sec_ap(thisSec) = .false.
            CASE DEFAULT
               CALL ERRHDL(PATH,ModNam,'E','105',FIELD(6),ILINE)
               SectorErr = .true.
               RETURN
         END SELECT
         
      ELSEIF (.not. VARYAP .and. .not. DfltSec) THEN
         IF (IFC > 5) THEN
            CALL ERRHDL(PATH,ModNam,'E','155',KEYWRD,ILINE)
            SectorErr = .true.
            RETURN
         END IF
         
      END IF
      
      RETURN
      END SUBROUTINE SECTOR


      SUBROUTINE SEASON
C***********************************************************************
C        SEASON Procedure for AERSURFACE
C
C        PURPOSE: Process SEASON keyword to extract month-to-season
C                 assignments.  Only valid when temporal resolution 
C                 (or frequency) of surface values is Annual or Monthly.
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Month-to-Season assignment 
C
C        ERROR HANDLING:   Checks for Too Few Parametes;
C                          Checks for Too Many Parameters
C
C***********************************************************************

C     Variable Declarations
      USE UserParams, only: SeasonMnths, Snow, AssignMnth
      USE ErrorHandling, only: ERRHDL

      IMPLICIT NONE

      INTEGER (KIND=4)         :: i       ! counter
      DOUBLE PRECISION         :: tmpNum  ! temp real number
      INTEGER (KIND=4)         :: tmpMo   ! temp month reference index
      
      ! tmpSsnMo: temporary month/season assignments (indices = month, store season)
      ! Save values for subsequent calls to check for overlapping/duplicate assignments
      ! Initialize with zero to represent no reassignments
      INTEGER (KIND=4), save   :: tmpSsnMo(12) = 0  
      
C     ssnNdx: season index
C             1 = winter without snow
C             2 = winter with continuous snow at least one month
C             3 = spring
C             4 = summer
C             5 = autumn

      INTEGER (KIND=4)         :: ssnNdx   
      
      ModNam = 'SEASON'
            
C     Initialize ssnNdx
      ssnNdx = 0
      
C     Initialize tmpMo
      tmpMo = 0
      
C     Initialize error flag
      IMIT = 1     

C     Set check number of params
      IF (IFC .LT. 4) THEN  
C        WRITE Error Message     ! Not enough Parameters
         CALL ERRHDL(PATH,ModNam,'E','150',KEYWRD,ILINE)
         RETURN
      ELSEIF(IFC .GT. 15) THEN
         CALL ERRHDL(PATH,ModNam,'E','155',KEYWRD,ILINE)
         RETURN
      ENDIF

C --- Set AssignMnth logical
      AssignMnth = .TRUE.

C     Get season index
      IF (IFC .GT. 2) THEN
      
         SELECT CASE (FIELD(3)(1:LEN_TRIM(FIELD(3))))
         
            CASE ('WINTERNS')
               ssnNdx = 1
            CASE ('WINTERWS')
               ! winter with snow is only valid if SNOW specified
               ! on climate keyword
               IF (.NOT. Snow) THEN
                  CALL ERRHDL(PATH,ModNam,'E','275',FIELD(3),ILINE)
               END IF
               ! go ahead and set season so processing will complete
               ssnNdx = 2
            CASE ('SPRING')
               ssnNdx = 3
            CASE ('SUMMER')
               ssnNdx = 4
            CASE ('AUTUMN')
               ssnNdx = 5
               
            CASE DEFAULT
               CALL ERRHDL(PATH,ModNam,'E','105',FIELD(3),ILINE)
               
         END SELECT   

      END IF

C     Extract months from fields, loop over remaining fields      
      IF (IFC .GT. 3 .AND. IFC .LE. 15) THEN
      
         DO I=4,IFC
            IMIT = 1
            !  get number
            CALL STONUM(FIELD(I), ILEN_FLD, tmpNum, IMIT)
            IF (IMIT .NE. 1) THEN
               CALL ERRHDL(PATH,ModNam,'E','245',KEYWRD,ILINE)
            ELSE
               tmpMo = NINT(tmpNum)
            END IF   

            ! if not a valid month, write error message
            IF (tmpMo .LT. 0 .OR. tmpMo .GT. 12) THEN
               CALL ERRHDL(PATH,ModNam,'E','280',KEYWRD,ILINE)
            ELSEIF (tmpMo .GE. 1 .AND. tmpMo .LE. 12) THEN
               ! assign month/season if not previously assigned, else error           
               IF (tmpSsnMo(tmpMo) .EQ. 0) THEN
                   tmpSsnMo(tmpMo) = ssnNdx
                  SeasonMnths(tmpMo) = ssnNdx
               ELSE
                  CALL ERRHDL(PATH,ModNam,'E','285',FIELD(I),ILINE)
               END IF
               
            END IF
       
         END DO
      
      END IF
      
      RETURN
      END SUBROUTINE SEASON


      SUBROUTINE RUNNOT
C***********************************************************************
C        RUNNOT Procedure for AERSURFACE
C
C        PURPOSE: Process Option To RUN Or NOT From Runstream Input Image
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Model RUN Logical Switch
C
C        ERROR HANDLING:   Checks for Invalid Parameters;
C                          Checks for No Parameters;
C                          Checks for Too Many Parameters
C
C***********************************************************************

C     Variable Declarations
      USE ErrorHandling, only: ERRHDL
      
      IMPLICIT NONE

      ModNam = 'RUNNOT'

      IF (IFC .EQ. 3) THEN
         IF (FIELD(3) .EQ. 'RUN') THEN
            RUN = .TRUE.
         ELSE IF (FIELD(3) .EQ. 'NOT') THEN
            RUN = .FALSE.
         ELSE
C           WRITE Error Message  ! Invalid Parameter
            CALL ERRHDL(PATH,ModNam,'E','105',KEYWRD,ILINE)
         END IF
      ELSE IF (IFC .GT. 3) THEN
C        WRITE Error Message     ! Too Many Parameters
         CALL ERRHDL(PATH,ModNam,'W','155',KEYWRD,ILINE)
      ELSE
C        WRITE Error Message     ! No Parameters
         CALL ERRHDL(PATH,ModNam,'E','150',KEYWRD,ILINE)
      END IF

      RETURN
      END SUBROUTINE RUNNOT
      
      SUBROUTINE OUCARD
C***********************************************************************
C        OUCARD Procedure Adapted from AERMOD for AERSURFACE
C
C        INPUTS:  Pathway (OU) and Keyword
C
C        OUTPUTS: Output Option Switches
C                 Output Setup Status Switches
C
C***********************************************************************

C     Variable Declarations
      USE ErrorHandling, only: ERRHDL
      
      IMPLICIT NONE
          
      INTEGER (KIND=4)             :: I              ! loop counter  
      INTEGER (KIND=4)             :: NDX = 0        ! Keyword status array index

      ModNam = 'OUCARD'
      
C     Get status array index
      DO I=1,SIZE(OUWRDS)
         IF (KEYWRD .EQ. OUWRDS(I)) THEN
            NDX = I
            EXIT
         END IF
      END DO

C     Set increment status array for keyword
      OUSTAT(NDX) = OUSTAT(NDX) + 1
      
      SELECT CASE(KEYWRD(1:LEN_TRIM(KEYWRD)))
      
         CASE('STARTING')
            ISTART = .TRUE.
            IF (OUSTAT(NDX) .NE. 1) THEN
C              WRITE Error Message: Repeat Non-repeatable Keyword
               CALL ERRHDL(PATH,ModNam,'E','135',KEYWRD,ILINE)
            END IF
         
         CASE('SFCCHAR')
            IF (OUSTAT(NDX) .NE. 1) THEN
C              WRITE Error Message: Repeat Non-repeatable Keyword
               CALL ERRHDL(PATH,ModNam,'E','135',KEYWRD,ILINE)
            ELSE
               CALL OUTPTFIL
            END IF
         
         CASE('EFFRAD')
            IF (OUSTAT(NDX) .NE. 1) THEN
C              WRITE Error Message: Repeat Non-repeatable Keyword
               CALL ERRHDL(PATH,ModNam,'E','135',KEYWRD,ILINE)
            ELSE
               CALL OUTPTFIL
            END IF
         
         CASE('NLCDGRID')
            IF (OUSTAT(NDX) .NE. 1) THEN
C              WRITE Error Message: Repeat Non-repeatable Keyword
               CALL ERRHDL(PATH,ModNam,'E','135',KEYWRD,ILINE)
            ELSE
               CALL OUTPTFIL
            END IF
         
         CASE('MPRVGRID')
            IF (OUSTAT(NDX) .NE. 1) THEN
C              WRITE Error Message: Repeat Non-repeatable Keyword
               CALL ERRHDL(PATH,ModNam,'E','135',KEYWRD,ILINE)
            ELSE
C              Get file name
               CALL OUTPTFIL
            END IF
         
         CASE('CNPYGRID')
            IF (OUSTAT(NDX) .NE. 1) THEN
C              WRITE Error Message: Repeat Non-repeatable Keyword
               CALL ERRHDL(PATH,ModNam,'E','135',KEYWRD,ILINE)
            ELSE
C              Get file name
               CALL OUTPTFIL
            END IF
            
         
         CASE('NLCDTIFF')
            IF (OUSTAT(NDX) .NE. 1) THEN
C              WRITE Error Message: Repeat Non-repeatable Keyword
               CALL ERRHDL(PATH,ModNam,'E','135',KEYWRD,ILINE)
            ELSE
               CALL OUTPTFIL
            END IF
         
         CASE('MPRVTIFF')
            IF (OUSTAT(NDX) .NE. 1) THEN
C              WRITE Error Message: Repeat Non-repeatable Keyword
               CALL ERRHDL(PATH,ModNam,'E','135',KEYWRD,ILINE)
            ELSE
C              Get file name
               CALL OUTPTFIL
            END IF
         
         CASE('CNPYTIFF')
            IF (OUSTAT(NDX) .NE. 1) THEN
C              WRITE Error Message: Repeat Non-repeatable Keyword
               CALL ERRHDL(PATH,ModNam,'E','135',KEYWRD,ILINE)
            ELSE
C              Get file name
               CALL OUTPTFIL
            END IF
         CASE('FINISHED')
            IFINIS = .TRUE.
            IF (OUSTAT(NDX) .NE. 1) THEN
C              WRITE Error Message: Repeat Non-repeatable Keyword
               CALL ERRHDL(PATH,ModNam,'E','135',KEYWRD,ILINE)
               GO TO 999
            END IF

C           Check for Missing Mandatory Keywords
            DO I=1,OUNUM
               IF (OUMAND(I) .EQ. 'T' .AND. OUSTAT(I) .EQ. 0) THEN
                  CALL ERRHDL(PATH,ModNam,'E','130',OUWRDS(I),ILINE)
               END IF
            END DO

            GO TO 1000

 1000       CONTINUE

         CASE DEFAULT
C           Write Error Message: Invalid Keyword for This Pathway
            CALL ERRHDL(PATH,ModNam,'E','110',KEYWRD,ILINE)
      
      END SELECT

 999  RETURN
      END SUBROUTINE OUCARD


      SUBROUTINE OUTPTFIL
C***********************************************************************
C        OUTPTFIL Procedure for AERSURFACE
C
C        PURPOSE: Process Output File Names
C                 From Runstream Input Image
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Filenames
C
C        ERROR HANDLING:   Checks for No Parametes;
C                          Checks for Too Many Parameters
C
C***********************************************************************

C     Variable Declarations
      USE Constants, only: FilNmLen
      USE FileUnits, only: SfcFile, EffRadFile, 
     &                     LCGridFile, 
     &                     ImpGridFile,
     &                     CanGridFile,
     &                     TIFLCDbgFile,
     &                     TIFImpDbgFile,
     &                     TIFCanDbgFile
      USE ErrorHandling, only: ERRHDL

      IMPLICIT NONE
      
      CHARACTER (LEN=FilNmLen)           ::  DFILE   ! File name retrieved

      ModNam = 'OUTPTFIL'
  
C     Check for too few parameters (filename missing)
      IF (IFC .LT. 3) THEN
C        WRITE Error Message     ! Not enough Parameters
         CALL ERRHDL(PATH,ModNam,'E','150',KEYWRD,ILINE)
         RETURN
      END IF
  
      
C     Retrieve Included Filename as Character Substring to Maintain Case
      IF ((LOCE(3)-LOCB(3)) .LE. (ILEN_FLD - 1) ) THEN
C           Retrieve Filename as Character Substring to Maintain Original Case
C           Also Check for Filename Larger Than ILEN_FLD Characters
            DFILE = RUNST1(LOCB(3):LOCE(3))

C           Assign file name to file variable, set year of data
            SELECT CASE(KEYWRD(1:LEN_TRIM(KEYWRD)))
            
               CASE('SFCCHAR')
                  SfcFile = DFILE(1:LEN_TRIM(DFILE))
               
               CASE('EFFRAD')
                  EffRadFile = DFILE(1:LEN_TRIM(DFILE))

                  ! Issue warning if EffRadDbg is not true from CO DEBUGOPT
                  IF (.not. EffRadDbg) THEN
                     CALL ERRHDL(PATH,ModNam,'W','305',KEYWRD,ILINE)
                  END IF
               
               CASE('NLCDGRID')
                  LCGridFile = DFILE(1:LEN_TRIM(DFILE))
                  
                  ! Issue warning if GridDbg is not true from CO DEBUGOPT
                  IF (.not. GridDbg) THEN
                     CALL ERRHDL(PATH,ModNam,'W','310',KEYWRD,ILINE)
                  END IF
               
               CASE('MPRVGRID')
                  ImpGridFile = DFILE(1:LEN_TRIM(DFILE))
                  
                  ! Issue warning if GridDbg is not true from CO DEBUGOPT
                  IF (.not. GridDbg) THEN
                     CALL ERRHDL(PATH,ModNam,'W','310',KEYWRD,ILINE)
                  END IF
               
               CASE('CNPYGRID')
                  CanGridFile = DFILE(1:LEN_TRIM(DFILE))
                  
                  ! Issue warning if GridDbg is not true from CO DEBUGOPT
                  IF (.not. GridDbg) THEN
                     CALL ERRHDL(PATH,ModNam,'W','310',KEYWRD,ILINE)
                  END IF
               
               CASE('NLCDTIFF')
                  TIFLCDbgFile = DFILE(1:LEN_TRIM(DFILE))
                  
                  ! Issue warning if TiffDbg is not true from CO DEBUGOPT
                  IF (.not. TiffDbg) THEN
                     CALL ERRHDL(PATH,ModNam,'W','315',KEYWRD,ILINE)
                  END IF
               
               CASE('MPRVTIFF')
                  TIFImpDbgFile = DFILE(1:LEN_TRIM(DFILE))
                  
                  ! Issue warning if TiffDbg is not true from CO DEBUGOPT
                  IF (.not. TiffDbg) THEN
                     CALL ERRHDL(PATH,ModNam,'W','315',KEYWRD,ILINE)
                  END IF
               
               CASE('CNPYTIFF')
                  TIFCanDbgFile = DFILE(1:LEN_TRIM(DFILE))
                  
                  ! Issue warning if TiffDbg is not true from CO DEBUGOPT
                  IF (.not. TiffDbg) THEN
                     CALL ERRHDL(PATH,ModNam,'W','315',KEYWRD,ILINE)
                  END IF

            END SELECT

      ELSE
C        WRITE Error Message:  Field is Too Long
         CALL ERRHDL(PATH,ModNam,'E','340',KEYWRD,ILINE)
         RETURN
      END IF

      RETURN
      END SUBROUTINE OUTPTFIL
      
      END MODULE ProcCtrlFile