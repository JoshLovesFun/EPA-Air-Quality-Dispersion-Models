      PROGRAM AERSURFACE
C ======================================================================
C
C                     MAIN Module of the AERSURFACE Tool
C                            (Version dated 24142)
C
C                                May 21, 2024
C
C       This version of AERSURFACE updates and replaces version 20060.
C       It includes enhancements to support processing of 1992 and all 
C       post-2001 Land Cover data, including all post-2001 Percent 
C       Impervious and Tree Canopy data (where available) to supplement 
C       concurrent land over data.
C
C       Additionally, the airport and non-airport flags have been re-defined 
C       in this version to clarify for the user whether a higher or lower 
C       surface roughness value is assigned to a sector depending on the 
C       designation used. Beginning with AERSURFACE 24142 a location or sector
C       may be defined with the former airport (AP) or non-airport (NONAP) 
C       designation but may also be described with low roughness (LOWZ0) or 
C       high roughness (HIGHZ0) respectively. 
C
C       The ZORAD and ZOEFF options for determining roughness, provided in this 
C       version, are equivalent to the methods used in AESURFACE version 20060. 
C       The ZORAD option is recommended for regulatory applications, while the 
C       research-grade ZOEFF option remains research-grade and is the equivalent 
C       of an alpha option and is not intended for regulatory applications at 
C       this time. 
C
C=======================================================================

      use StartVars,      only: RunStart
      use ProcCtrlFile,   only: RUN, RUNERR
      use FileUnits,      only: TmpMsgUnt,OutUnt,LogUnt,
     &                         CMD_ARGS, USAGE, FILOPN 
      use ErrorHandling,  only: InitErrMsg, MSGWRT, ERRHDL, FATAL
      use ProcCtrlFile,   only: SETUP
      use InitTiffParams, only: SetTiffParams
      use GetData,        only: GetLCCounts
      use SfcChars,       only: Write_SfcChars
      use AvgParams,      only: AvgSfcCharsSimple,AvgSfcCharsGeom

      implicit none
      
C***********************************************************************
C     Get date and time at run start
C***********************************************************************
      call RunStart
   
C***********************************************************************
C     Initialize variables
C***********************************************************************

      FATAL  = .FALSE.
      RUNERR = .FALSE.
     
      call InitErrMsg

C***********************************************************************
C     Process command-line arguments
C***********************************************************************

      call CMD_ARGS

C***********************************************************************
C     Open I/O files
C***********************************************************************

      call FILOPN
      
C***********************************************************************
C     Process runstream
C***********************************************************************
      call SETUP

C***********************************************************************
C     Report setup status
C***********************************************************************   
  
      ! print status for setup phase - if fatal, do not continue
CRLM      IF (FATAL) THEN
       IF (FATAL .OR. RUNERR) THEN !added RUNERR for confirmbounds check O027 WSP 20240530
         WRITE(*,110)
         WRITE(OutUnt,110)
         RUNERR = .true.
  110    FORMAT(
     &          /,'*************************************************',
     &          /,'*** AERSURFACE SETUP Finished UN-successfully ***',
     &          /,'*************************************************'
     &          /)
         go to 800
      
      ELSE
         WRITE(*,120)
         WRITE(OutUnt,120)
  120    FORMAT(/,'**********************************************',
     &          /,'*** AERSURFACE SETUP Finished Successfully ***',
     &          /,'**********************************************'
     &          /)

      END IF

C***********************************************************************
C     Determine continuation
C***********************************************************************

      ! if RUNORNOT was RUN and no fatal errors encountered, continue
      IF (RUN .and. .not. FATAL) THEN
         Continue
      ELSE
         GO TO 800
      END IF

C***********************************************************************
C     Check data control parameters
C***********************************************************************    

      CALL SetTiffParams 
      
CRLM      if (RUNERR) go to 800
      IF (RUNERR .or. FATAL) go to 800 !O027 MRLC resolution testing WSP 20240515

C-----------------------------------------------------------------------
C    Get counts of land use classes by sector
C-----------------------------------------------------------------------

      CALL GetLCCounts

CRLM      if (RUNERR) go to 800
      IF (RUNERR .or. FATAL) go to 800 !O027 MRLC resolution testing WSP 20240515

C-----------------------------------------------------------------------
C    Compute the sector averaged surface characteristics - 
C    Inverse Distance or Simple method - set in mod_AvgParams.f
C    6/28/07, CRT: Simple for Albedo, distance weighted geometric mean
C       for roughness, straight geometric mean for Bowen ratio    
C-----------------------------------------------------------------------

C     Use simple (arithmetic) area weighted average for albedo
      CALL AvgSfcCharsSimple

CRLM      if (RUNERR) go to 800
      IF (RUNERR .or. FATAL) go to 800 !O027 MRLC resolution testing WSP 20240515

C     Use geometric mean for Bowen (area weighted) and Zo (inv. 
C     distance weighted) IBL effective roughness when user opts for
C     ZOEFF, more simples inverse distance weighted average within 
C     user-defined radius when user opts for ZORAD.    

      CALL AvgSfcCharsGeom

CRLM      if (RUNERR) go to 800
      IF (RUNERR .or. FATAL) go to 800 !O027 MRLC resolution testing WSP 20240515

C-----------------------------------------------------------------------
C    Write the results to the AERMET-format output file for Stage 3
C    Write the results plus additional information to the log file
C-----------------------------------------------------------------------

      CALL Write_SfcChars

  800 CALL MSGWRT

      CLOSE(TmpMsgUnt,status='delete')
      
C***********************************************************************
C     Report processing status
C***********************************************************************   
      ! print status for processing phase 
      ! processing errors
CRLM      IF (RUN .and. RUNERR) THEN
      IF (RUN .and. (RUNERR .or. FATAL)) THEN !O027 MRLC resolution testing WSP 20240515
      
         WRITE(*,910)
         WRITE(OutUnt,910)
  910    FORMAT(
     &      /,'******************************************************',
     &      /,'*** AERSURFACE Processing Finished UN-successfully ***',
     &      /,'******************************************************')

C ---    Write UN-successful message to LogFile
         WRITE(LogUnt,910)

      ! processing completed successfully
      ELSEIF (RUN .and. .not. RUNERR) THEN
         WRITE(*,920)
         WRITE(OutUnt,920)
  920    FORMAT(
     &      /,'***************************************************',
     &      /,'*** AERSURFACE Processing Finished Successfully ***',
     &      /,'***************************************************')

      END IF
      
      CLOSE(OutUnt)
      CLOSE(LogUnt)

      STOP
      END     
