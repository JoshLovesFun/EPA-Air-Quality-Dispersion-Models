C=======================================================================
      MODULE FileUnits
      
C     Variable declarations to and initializations for all file units
C     and only those filenames that have default values.  The variable
C     BGPath is declared and set as an empty to serve as a placeholder
C     and is a required argument for the NGRIDS subroutine which is part
C     of collection of NADCON procedures.

C     Uses the following modules: 
C     - StartVars
C     - Constants

C     Contains the following procedures: FILOPN, CMD_ARGS, USAGE

C=======================================================================

      USE StartVars, only: run_start, iYr2
      USE Constants, only: Version, FilNmLen

      IMPLICIT NONE
      SAVE
    
C     file units
      INTEGER (kind=4) :: CtrlUnt      = 20  ! Control file (runstream)
      INTEGER (kind=4) :: OutUnt       = 21  ! Input summary, messages
      
C     *** Unit 22, Log file, is hard coded in subroutines related to NADCON **
      INTEGER (kind=4) :: LogUnt       = 22  ! Log output file
      
      INTEGER (kind=4) :: SfcUnt       = 23  ! Surface values output file
      
      INTEGER (kind=4) :: TmpMsgUnt    = 25  ! Temporary message file
      

      INTEGER (kind=4) :: LCUnt        = 30  ! Land Cover GeoTIFF input file 
      INTEGER (kind=4) :: ImpUnt       = 32  ! Impervious data GeoTIFF input file
      INTEGER (kind=4) :: CanUnt       = 34  ! Canopy data GeoTIFF input file
      
      INTEGER (kind=4) :: LCGridUnt    = 40  ! land cover grid values
      INTEGER (kind=4) :: ImpGridUnt   = 42  ! impervious grid values
      INTEGER (kind=4) :: CanGridUnt   = 44  ! canopy grid values
      
      INTEGER (kind=4) :: TIFLCDbgUnt   = 50  ! Land cover TIFF file debug file
      INTEGER (kind=4) :: TIFImpDbgUnt  = 52  ! Impervious TIFF file debug file
      INTEGER (kind=4) :: TIFCanDbgUnt  = 54  ! Canopy TIFF file debug file
      
      INTEGER (kind=4) :: EffRadUnt    =  60  ! Effective radius file

C     file names      
      CHARACTER (len=FilNmLen)  :: CtrlFile      = 'aersurface.inp'
      CHARACTER (len=FilNmLen)  :: OutFile       = 'aersurface.out'
      CHARACTER (len=FilNmLen)  :: LogFile       = 'aersurface.log'    
      
      CHARACTER (len=FilNmLen)  :: SfcFile = 'sfc_chars.out'
        
      CHARACTER (len=FilNmLen)  :: TmpMsgFile    = 'aersurface.tmp'
      
      CHARACTER (len=FilNmLen) :: LCGridFile    = 'landcover.txt'
      CHARACTER (len=FilNmLen) :: ImpGridFile   = 'impervious.txt'
      CHARACTER (len=FilNmLen) :: CanGridFile   = 'canopy.txt'
      
      CHARACTER (len=FilNmLen) :: TIFLCDbgFile   = 'lc_tif_dbg.txt'
      CHARACTER (len=FilNmLen) :: TIFImpDbgFile  = 'imp_tif_dbg.txt'
      CHARACTER (len=FilNmLen) :: TIFCanDbgFile  = 'can_tif_dbg.txt'
      
      CHARACTER (len=FilNmLen)  :: EffRadFile    = 'effective_rad.txt'
      
      CHARACTER (len=FilNmLen)  :: NGPath = ''

      contains
      

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
C        MODIFIED:   Remove non-standard option for 
C                    CARRIAGECONTROL='Fortran' to control
C                    page feed in aermod.out file.  ASCII form
C                    feed character is used in subroutine HEADER
C                    to insert page feed instead of using Fortan
C                    carriage control.
C                    R.W. Brode, USEPA/OAQPS/AQMG, October 19, 2009
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

      IMPLICIT NONE

C     OPEN Input Runstream File
      OPEN (UNIT=CtrlUnt,FILE=CtrlFile,ACTION='READ',
     &      ERR=100,STATUS='OLD')

C     OPEN Primary Output File
      OPEN (UNIT=OutUnt,FILE=OutFile,
     &      ERR=200,STATUS='REPLACE')

C     OPEN Log File
      OPEN (UNIT=LogUnt,FILE=LogFile,
     &      ERR=300,STATUS='REPLACE')
     
C --- Write header info (AERSURFACE version, date and time)
      write(OutUnt,40,err=250) 'AERSURFACE Output File, Version ',
     &    trim(Version),run_start(2),run_start(3),iYr2,
     &    run_start(5),run_start(6),run_start(7)
     
C --- Write header info (AERSURFACE version, date and time)
      write(LogUnt,40,err=250) 'AERSURFACE Log File, Version ',
     &    trim(Version),run_start(2),run_start(3),iYr2,
     &    run_start(5),run_start(6),run_start(7)
     
   40 FORMAT('** ',2a,t67, i2.2,'/',i2.2,'/',i2.2,' **'/
     &       '**',t67,i2.2,':',i2.2,':',i2.2,' **'/ )
     
C     OPEN Temporary Message File
      open(UNIT=TmpMsgUnt,FILE=TmpMsgFile,
     &      ERR=400,status='REPLACE')

C     Write Out Update to the Screen
      WRITE(*,50)
  50  FORMAT(/,' Now Processing SETUP Information')

      RETURN

C     WRITE Error Message:  Error Opening File
 100  WRITE(*,101) trim(CtrlFile)
 101  FORMAT(/,' Error Opening Runstream Input File: ',a,//,
     &         ' Processing Aborted!')
      STOP

C     WRITE Error Message:  Error Opening File
 200  WRITE(*,201) trim(OutFile)
 201  FORMAT(/,' Error Opening Output File: ',a,//,
     &         ' Processing Aborted!')
      STOP
      
C     WRITE Error Message:  Error Writing to File
 250  WRITE(*,251) trim(OutFile)
 251  FORMAT(/,' Error Writing to Output File: ',a,//,
     &         ' Processing Aborted!')
      STOP

C     WRITE Error Message:  Error Opening File
 300  WRITE(*,301) trim(LogFile)
 301  FORMAT(/,' Error Opening Log File: ',a,//,
     &         ' Processing Aborted!')
      STOP

C     WRITE Error Message:  Error Writing to File
c 350  WRITE(*,351) trim(LogFile)
c 351  FORMAT(/,' Error Writing to Log File: ',a,//,
c     &         ' Processing Aborted!')
      STOP

C     WRITE Error Message:  Error Opening File
 400  WRITE(*,401) trim(TmpMsgFile)
 401  FORMAT(/,' Error Opening Temp Message File: ',a,//,
     &         ' Processing Aborted!')
      STOP

      END SUBROUTINE FILOPN
      
      SUBROUTINE CMD_ARGS
C***********************************************************************
C                CMD_ARGS
C
C        PURPOSE: Process command-line arguments
C
C        DATE:       January 2018
C
C        INPUTS:  Read command-line arguments for CtrlFile, OutFile,
C                 LogFile
C
C        OUTPUTS: Set  CtrlFile, OutFile, LogFile
C
C        CALLED FROM:  Main Program
C
C***********************************************************************

!     Variable declarations   
      implicit none

      integer (kind=4)   :: i, argcnt, linp, lout, llog, status
      character (len=FilNmLen) :: fbase
      character (len=FilNmLen) :: fpath

!     Initialize variables      
      i = 0; argcnt = 0
      linp = 0; lout = 0; llog = 0
      status = 0
      fpath = ""
      fbase = ""

!     Set input and output file names based on optional
!     command line arguments and use the basename of
!     the input file as a prefix for other scratch files.
      argcnt = command_argument_count()

      if (argcnt > 0) then
         ! loop over command line arguments
         do i=1,argcnt
            
            select case(i)
               ! set control file name from 1st argument
               case(1)
                  call get_command_argument(i,CtrlFile,linp,status)

                  if (status .ne. 0) then
                     call usage
                     stop
                  end if
               ! set output (.out) file name from 2nd argument
               case(2)
                  call get_command_argument(i,OutFile,lout,status)
                  if (status .ne. 0) then
                     call usage
                     stop
                  end if
               ! set log file name from 3rd argument
               case(3)
                  call get_command_argument(i,LogFile,llog,status)
                  if (status .ne. 0) then
                     call usage
                     stop
                  end if
            end select
         end do
         
         ! get control file path and base name based on position of first
         ! occurrence of "\" (Windows) or "/" (Linux) from right end of
         ! argument, if it exists
         do i=linp, 1, -1

            if (CtrlFile(i:i) == "/" .or. CtrlFile(i:i) == "\") then
               fbase = CtrlFile(i+1:linp)
               fpath = CtrlFile(1:i)
               exit
            end if
         end do
         
         ! if no occurrence of "/" or "\", path info not given,
         ! length of fbase = 0, set fbase to control file name
         if (len(trim(fbase)) == 0) fbase = CtrlFile
         
         ! strip extension from control filename and reset fbase 
         do i=len(CtrlFile), 1, -1
            if (CtrlFile(i:i) == "." .and. i /= 1) then
               fbase = fbase(1:i-1)
               exit
            end if
         end do

         ! set output filename and log filename if not in command-line
         if (argcnt < 2) OutFile = trim(fpath) // trim(fbase) // ".out"
         if (argcnt < 3) LogFile = trim(fpath) // trim(fbase) // ".log"

      end if
      
      END SUBROUTINE CMD_ARGS

      SUBROUTINE USAGE
C***********************************************************************
C     SUBROUTINE USAGE
C 
C     Purpose: Write command-line argument usage message to the screen
C
C     Assumptions: 
C
C     I/O:  Output message to screen
C
C     Called By: main program
C
C     Calls To:  <none>
C
C***********************************************************************
     
      IMPLICIT NONE
     
      WRITE(*,*) ""
      WRITE(*,*) "Usage:"
      WRITE(*,*) ""
      WRITE(*,*) "AERSURFACE "//Version//" can accept 0, 1, 2, "
     &           // "or 3 command-line arguments."
      WRITE(*,*) ""
      WRITE(*,*) "AERSURFACE"
      WRITE(*,*) "OR"
      WRITE(*,*) "AERSURFACE filename.inp"
      WRITE(*,*) "OR"
      WRITE(*,*) "AERSURFACE filename.inp filename.out"
      WRITE(*,*) "OR"
      WRITE(*,*) "AERSURFACE filename.inp filename.out "
     &           // "filename.log"
      RETURN
      END SUBROUTINE USAGE  

      END MODULE FileUnits
