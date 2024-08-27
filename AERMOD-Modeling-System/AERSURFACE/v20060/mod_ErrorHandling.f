C=======================================================================
      MODULE ErrorHandling
      
C     Error handling variables and routines

C     Uses the following modules: 
C     - StartVars
C     - FileUnits

C=======================================================================

C     Variable Declarations
      USE StartVars, only: ModLen, ModNam
      USE FileUnits, only: TmpMsgUnt, OutUnt
      
      IMPLICIT NONE
      SAVE
      
C***********************************************************************
C     This is The Global Variable Definition Block for Error Handling
C***********************************************************************

      logical                    :: FATAL             ! Fatal error occurred
      logical                    :: ERRLST            ! Errors were encountered
      logical                    :: EOF               ! End-of-File reached
      
      integer, parameter         :: ErrLen = 72       ! Max length of error message
      integer, parameter         :: ErrCodLen = 3     ! Max length of error code
      integer, parameter         :: ErrHntLen = 20    ! Max length of hint keyword
      integer, parameter         :: ErrTypLen = 1     ! Max length for error code type
      
      integer, parameter         :: IERRN=200         ! Number of Error/Warning/Informational Messages
      
      character (len=ErrLen)     :: ERRMSG(IERRN)     ! Data array of possible error messages
      character (len=ErrCodLen)  :: ERRCOD(IERRN)     ! Data array of possible error codes
      
      character (len=*), parameter  :: MSGFMT =       ! Message Output Format
     &          '(A2,1X,A1,A3,I6,1X,A15,1X,A50,1X,A20)'

      integer (kind=4)           :: IERROR            ! Count of error messages
      integer (kind=4)           :: IFTL              ! Total fatal errors (stats)
      integer (kind=4)           :: IWRN              ! Total warnings (stats)
      integer (kind=4)           :: INFO              ! Total information messages (stats)
      integer (kind=4)           :: NFATAL            ! Count of fata errors
      integer (kind=4)           :: NWARN             ! Count of warnings
      integer (kind=4)           :: IPAGE             ! Page number counter
      
      CONTAINS

C=======================================================================

      SUBROUTINE InitErrMsg

C     Initialize Error Code and Message Arrays
      
C=======================================================================
      implicit none
      
      integer (kind=4)   :: i
      
      ERRCOD(:) = ' '
      ERRMSG(:) = ' '

      i = 0
      
      ! General Setup Messages
      
      i=i+1
      ERRCOD(i) = '100'
      ERRMSG(i) = 'Invalid Pathway Specified'
      i=i+1
      ERRCOD(i) = '105'
      ERRMSG(i) = 'Invalid Keyword Specified'
      i=i+1
      ERRCOD(i) = '110'
      ERRMSG(i) = 'Keyword is Not Valid for This Pathway'
      i=i+1
      ERRCOD(i) = '115'
      ERRMSG(i) = 'STARTING or FINISHED Out of Sequence'
      i=i+1
      ERRCOD(i) = '120'
      ERRMSG(i) = 'Pathway is Out of Sequence:'
      i=i+1
      ERRCOD(i) = '125'
      ERRMSG(i) = 'Missing FINISHED-Runstream File Incomplete. ISTAT='
      i=i+1
      ERRCOD(i) = '130'
      ERRMSG(i) = 'Missing Mandatory Keyword'
      i=i+1
      ERRCOD(i) = '131'
      ERRMSG(i) = 'Missing Conditional Keyword'
      i=i+1
      ERRCOD(i) = '135'
      ERRMSG(i) = 'Nonrepeatable Keyword'
      i=i+1
      ERRCOD(i) = '140'
      ERRMSG(i) = 'Keyword is Out of Sequence'
      i=i+1
      ERRCOD(i) = '145'
      ERRMSG(i) = 'Duplicate Parameters Specified for Keyword'
      i=i+1
      ERRCOD(i) = '150'
      ERRMSG(i) = 'Too Few Parameters Specified for Keyword'
      i=i+1
      ERRCOD(i) = '155'
      ERRMSG(i) = 'Too Many Parameters Specified for Keyword'
      i=i+1
      ERRCOD(i) = '160'
      ERRMSG(i) = 'Invalid Parameter Specified for Keyword'
      i=i+1
      ERRCOD(i) = '165'
      ERRMSG(i) = 'More Than One Delimiter In A Field for Keyword'
      i=i+1
      ERRCOD(i) = '170'
      ERRMSG(i) = 'Keyword Conflict Encountered'
      
      ! CO Specific Messages
      i=i+1
      ERRCOD(i) = '200'
      ERRMSG(i) = 'Only one of PRIMARY or SECONDARY allowed'
      i=i+1
      ERRCOD(i) = '201'
      ERRMSG(i) = 'Only one of ZOEFF or ZORAD allowed'
      i=i+1
      ERRCOD(i) = '202'
      ERRMSG(i) = 'Anem. height is required when ZOEFF option is used'
      i=i+1
      ERRCOD(i) = '203'
      ERRMSG(i) = 'A LC Data File Has Not Been Specified'
      i=i+1
      ERRCOD(i) = '205'
      ERRMSG(i) = 'Invalid Keyword for Number of Sectors Specified'
      i=i+1
      ERRCOD(i) = '206'
      ERRMSG(i) = 'NAD Grid Files Missing (*.los & *.las)'
      i=i+1
      ERRCOD(i) = '207'
      ERRMSG(i) = 'SECTOR Required When VARYAP Flag Specified'
      i=i+1
      ERRCOD(i) = '208'
      ERRMSG(i) = 'Invalid # SECTOR Definitions for # of Sectors'
      i=i+1
      ERRCOD(i) = '209'
      ERRMSG(i) = 'DBGOPT EFFRAD is not applicable with OPTIONS ZORAD'
      i=i+1
      ERRCOD(i) = '210'
      ERRMSG(i) = 'SEASON Keyword Only Valid with ANNUAL and MONTHLY'
      i=i+1
      ERRCOD(i) = '227'
      ERRMSG(i) = 'Calc. IBL Height < Minimum (Based on Anem. Ht.)'
      i=i+1
      ERRCOD(i) = '228'
      ERRMSG(i) = 'Calc. IBL Height > Maximum (Based on Anem. Ht.)'
      i=i+1
      ERRCOD(i) = '230'
      ERRMSG(i) = 'Primary Title Cannot Be Blank'
      i=i+1
      ERRCOD(i) = '235'
      ERRMSG(i) = 'Secondary Title is Blank'
      i=i+1
      ERRCOD(i) = '240'
      ERRMSG(i) = 'File Name is Too Long, Exceeds Maximum Length'
      i=i+1
      ERRCOD(i) = '245'
      ERRMSG(i) = 'Illegal Numerical Field Encountered'
      i=i+1
      ERRCOD(i) = '247'
      ERRMSG(i) = 'Anemometer Height Must Be Within the Valid Range:'
      i=i+1
      ERRCOD(i) = '248'
      ERRMSG(i) = 'IBL Factor Must Be Within the Valid Range:'
      i=i+1
      ERRCOD(i) = '250'
      ERRMSG(i) = 'Invalid Horizontal Datum Specified'
      i=i+1
      ERRCOD(i) = '255'
      ERRMSG(i) = 'Arid Climate is Invalid With Continuous Snow'
      i=i+1
      ERRCOD(i) = '260'
      ERRMSG(i) = 'Invalid Number of Sectors Specified'
      i=i+1
      ERRCOD(i) = '265'
      ERRMSG(i) = 'Invalid Sector ID Specified'
      i=i+1
      ERRCOD(i) = '266'
      ERRMSG(i) = 'Sector IDs Must Be Consecutive'
      i=i+1
      ERRCOD(i) = '267'
      ERRMSG(i) = 'Gap or Overlap in Sector Start/End Directions'
      i=i+1
      ERRCOD(i) = '268'
      ERRMSG(i) = 'Min Sector Width of 30-degrees Required'
      i=i+1
      ERRCOD(i) = '269'
      ERRMSG(i) = 'Coverage For All Sectors Must Equal 360-degrees'
      i=i+1
      ERRCOD(i) = '270'
      ERRMSG(i) = 'Numeric Value Out of Range'
      i=i+1
      ERRCOD(i) = '271'
      ERRMSG(i) = 'Min Sector Width of 22.5-degrees Required'
      i=i+1
      ERRCOD(i) = '272'
      ERRMSG(i) = 'VARYAP Flag is Required on FREQ_SECT With Keyword'
      i=i+1
      ERRCOD(i) = '273'
      ERRMSG(i) = '' !not in use
      i=i+1
      ERRCOD(i) = '274'
      ERRMSG(i) = 'AP/NonAP Flag Required with VARYAP on FREQ_SECT'
      i=i+1
      ERRCOD(i) = '275'
      ERRMSG(i) = 'WINTERWS Not Valid When CLIMATE = NOSNOW'
      i=i+1
      ERRCOD(i) = '280'
      ERRMSG(i) = 'Invalid Month Specified'
      i=i+1
      ERRCOD(i) = '285'
      ERRMSG(i) = 'Month Was Previously Assigned to a Season'
      
      
      ! OU Specific Messages
      i=i+1
      ERRCOD(i) = '305'
      ERRMSG(i) = 'EFFRAD Not Specified on CO DEBUGOPT; Ignored'
      i=i+1
      ERRCOD(i) = '310'
      ERRMSG(i) = 'GRID Not Specified on CO DEBUGOPT; Ignored'
      i=i+1
      ERRCOD(i) = '315'
      ERRMSG(i) = 'TIFF Not Specified on CO DEBUGOPT; Ignored'
      
      
      i=i+1
      ERRCOD(i) = '340'
      ERRMSG(i) = 'File Name is Too Long, Exceeds Maximum Length'
      
      
      
      ! Read TIFF Tags
      i=i+1
      ERRCOD(i) = '405'
      ERRMSG(i) = 'Byte Order of Processor Could Not Be Determined'
      i=i+1
      ERRCOD(i) = '410'
      ERRMSG(i) = 'Byte Order of GeoTIFF Undetermined (See Log File)'
      i=i+1
      ERRCOD(i) = '415'
      ERRMSG(i) = 'File is Not Correctly Identified as a TIFF File'
      i=i+1
      ERRCOD(i) = '420'
      ERRMSG(i) = 'Allocation Error While Reading GeoTIFF File'
      i=i+1
      ERRCOD(i) = '425'
      ERRMSG(i) = 'GeoTIFF File Does Not Contain Georeference Info'
      i=i+1
      ERRCOD(i) = '430'
      ERRMSG(i) = 'GeoTIFF File Contains Unidentified Data Type'
      i=i+1
      ERRCOD(i) = '435'
      ERRMSG(i) = 'Required Georeference Data Was Not Found in GeoTIFF'
      i=i+1
      ERRCOD(i) = '440'
      ERRMSG(i) = 'Multiple Values Found for a GeoKey.  Expecting One'
      
      
      ! File I/O messages
      i=i+1
      ERRCOD(i) = '500'
      ERRMSG(i) = 'Fatal Error Occurred Opening a Primary I/O File'
      i=i+1
      ERRCOD(i) = '505'
      ERRMSG(i) = 'Fatal Error Occurred Reading from Input File'
      i=i+1
      ERRCOD(i) = '510'
      ERRMSG(i) = 'Fatal Error Occurred Reading from Temporary File'
      i=i+1
      ERRCOD(i) = '515'
      ERRMSG(i) = 'Fatal Error Occurred Writing to Output File'

      ! Runtime error messages:
      i=i+1
      ERRCOD(i) = '600'
      ERRMSG(i) = 'The Study Area Extends Beyond the Data File'
      i=i+1
      
      END SUBROUTINE InitErrMsg
      

      SUBROUTINE ERRHDL(PATHWY,ModNam,INERTP,INERCD,INPMSG,ILINE)
C***********************************************************************
C                 ERRHDL Procedure for AERSURFACE
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
C        CALLED FROM:  (This Is An Utility Program)
C***********************************************************************

      IMPLICIT NONE

      CHARACTER (len=*), intent(in)    ::  PATHWY   ! Runstream path
      CHARACTER (len=*), intent(in)    ::  ModNam   ! Module, Procedure
      CHARACTER (len=*), intent(in)    ::  INERTP   ! Message Type (E, W, I)
      CHARACTER (len=*), intent(in)    ::  INERCD   ! Message code
      CHARACTER (len=*), intent(in)    ::  INPMSG   ! Keyword or Hint
      INTEGER (kind=4),  intent(in)    ::  ILINE    ! Line number

      INTEGER (kind=4)           ::  I         ! Counter
      CHARACTER (len=ErrLen)     ::  ERRMG1   ! Local message
      CHARACTER (len=ErrCodLen)  ::  ICODE    ! Local message code
      CHARACTER (len=len(ModNam))::  TMPMOD   ! Local module/procedure
      CHARACTER (len=ErrHntLen)  ::  TMPMSG   ! Local keyword hint
      LOGICAL                    ::  FOUND    ! Message code found in array

C     Variable Initializations
      IERROR = IERROR + 1
      FOUND = .FALSE.
      I = 1

C     Check for Occurrence of 'E' Error Type, and Set FATAL Switch
      IF (INERTP .EQ. 'E') THEN
         FATAL = .TRUE.
         NFATAL = NFATAL + 1
         IF (NFATAL .EQ. 999) THEN
C           Number Of Fatal Errors Has Reached Limit of 999
            ERRMG1 = 'Number of Fatal Errors Has Reached Limit of 999'
            TMPMOD = 'ERRHDL'
            ICODE  = '999'
            TMPMSG = ' '
            WRITE(TmpMsgUnt,MSGFMT) PATHWY,INERTP,ICODE,ILINE,
     &               adjustl(trim(TMPMOD)),adjustl(trim(ERRMG1)),
     &               adjustl(trim(TMPMSG))
            GO TO 999
         ELSE IF (NFATAL .GT. 999) THEN
C           Skip Any More Error WRITEs
            GO TO 999
         END IF
      END IF

C     Go To Match The Error Massage
      DO WHILE (.NOT. FOUND .AND. I .LE. IERRN)
         IF (INERCD .EQ. ERRCOD(I)) THEN
            ERRMG1 = ERRMSG(I)
            FOUND = .TRUE.
         END IF
         I = I + 1
      END DO

      IF (.NOT. FOUND) THEN
         WRITE(ERRMG1,1001)
 1001    FORMAT('SYSTEM ERROR: MESSAGE NOT FOUND FOR THIS NUMBER!')
      END IF

C     Write Out The Error Message
      WRITE(TmpMsgUnt,MSGFMT) PATHWY,INERTP,INERCD,ILINE,
     &        adjustl(trim(ModNam)), adjustl(trim(ERRMG1)),
     &        adjustl(trim(INPMSG))

 999  RETURN
      END SUBROUTINE ERRHDL
      

      SUBROUTINE MSGWRT
C***********************************************************************
C                 MSGWRT Procedure for AERSURFACE
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
      IMPLICIT NONE

      CHARACTER (len=2)         ::  PATH     ! Runstream path
      
      CHARACTER (len=ModLen)    ::  ModNam   ! Module name
      CHARACTER (len=ErrTypLen) ::  ERRTP    ! Error type
      CHARACTER (len=ErrCodLen) ::  ERRCD    ! Error code (E, I, W)
      CHARACTER (len=ErrLen)    ::  ERRMG1   ! Error message
      CHARACTER (len=ErrHntLen) ::  ERRMG2   ! Error hint/keyword
      
      LOGICAL                   ::  MsgFlg   ! Flag to indicate there are messages 

      INTEGER :: IERRLN

      ModNam = 'MSGWRT'
      
      MsgFlg = .false.
      
      REWIND(TmpMsgUnt)
      
C     Determine if there are any records in the message file
      READ(TmpMsgUnt,MSGFMT,END=88,ERR=999) PATH,ERRTP,ERRCD,IERRLN,
     &                      ModNam,ERRMG1,ERRMG2
      IF( ERRTP .EQ. 'E' .OR. ERRTP .EQ. 'W' )THEN
         MsgFlg = .true.
      ENDIF

      REWIND(TmpMsgUnt)
   88 CONTINUE

      IF (MsgFlg) THEN

C        Write Out The Header Of The Message File
         WRITE(OutUnt,*) '********* Errors(E)/Warnings(W) ************'
         WRITE(OutUnt,*) 
         WRITE(OutUnt,*) 'PW      --- Pathway (if during Setup)       '
         WRITE(OutUnt,*) 'Code    --- Error Type + Error Code         '
         WRITE(OutUnt,*) 'L#      --- Line # (if during Setup)        '
         WRITE(OutUnt,*) 'ModNam  --- Module/Procedure                '
         WRITE(OutUnt,*) 'Message --- Hints For The Possible Solution '
         WRITE(OutUnt,*) '********************************************'
         WRITE(OutUnt,*) 
         WRITE(OutUnt,1114)
         WRITE(OutUnt,1115)
 1114    FORMAT('PW CODE   L#  ModNam          ','MESSAGE',T82,
     &                                                       'HINT')
 1115    FORMAT('-- ---- ----- --------------- ',50('-'),' ',
     &                                                    20('-'))
         WRITE(OutUnt,*) ' '
         
         EOF = .FALSE.
   
         DO WHILE (.NOT. EOF)
            READ(TmpMsgUnt,MSGFMT,END=99,ERR=999) PATH,ERRTP,ERRCD,
     &                         IERRLN,ModNam,ERRMG1,ERRMG2
C           Write Out The Error Message
            WRITE(OutUnt,MSGFMT) PATH,ERRTP,ERRCD,IERRLN,
     &        adjustl(trim(ModNam)), adjustl(trim(ERRMG1)),
     &        adjustl(trim(ERRMG2))
            GO TO 11
   99       EOF = .TRUE.
   11       CONTINUE
         END DO
      
      END IF

      GO TO 1000

C     WRITE Error Message: Error Reading Temp Error Message File
C  999 CALL ERRHDL(PATH,ModNam,'E','510','ERRORMSG')
  999 WRITE(*,1120)
      WRITE(OutUnt,1120) 
 1120 FORMAT('An error was encountered reading a temporary file.',/
     &       'Error/Warning messages could not be written to'/
     &       'the output file.')

 1000 RETURN
      END SUBROUTINE MSGWRT
      
      END MODULE ErrorHandling