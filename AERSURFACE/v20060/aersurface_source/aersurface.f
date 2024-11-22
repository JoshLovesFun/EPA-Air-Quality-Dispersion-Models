      PROGRAM AERSURFACE
C ======================================================================
C
C                     MAIN Module of the AERSURFACE Tool
C                            (Version dated 20060)
C
C                               February 29, 2020
C
C       This version of AERSURFACE is an update to 19039_DRFT and replaces  
C       version 13016. It includes enhancements to support processing of  
C       1992/2001/2006/2011/2016 Land Cover data, including 2001/2006/2011/2016 
C       percent Impervious and Tree Canopy data (where available) to  
C       supplement concurrent land cover data.
C       
C       The ZORAD option for determining roughness, provided in this 
C       version, is equivalent to the method used in AERSURFACE
C       version 13016 and is recommended for regulatory applications.
C       The research-grade ZOEFF option is the equivalent of an alpha 
C       option and is meant only for review and is not intended for  
C       regulatory applications at this time. 
C
C=======================================================================
C
C       This version of AERSURFACE includes the following modifications 
C       relative to the previous versions dated 13016 and 19039_DRFT:
C            
C-----  Enhancements:
C
C       1.  Path/Keyword interface similar to AERMOD, AERMET, and AERMAP. 
C           NOTE: This version is not compatible with the redirected input 
C           from files generated with previous versions AERSURFACE.
C           
C       2.  Option to output surface values with appropriate AERMET
C           keywords for the PRIMARY or SECONDARY meteorological tower
C           location. (Default is PRIMARY.)
C           
C       3.  Option to separately characterize individual wind sectors as  
C           either airport or non-airport for determining surface roughness
C           length. Previous versions required all wind sectors to 
C           be characterized as either airport or non-airport.
C 
C       4.  Option (ZOEFF) to compute surface roughness length based on the 
C           Effective Roughness Model Coding Abstract (MCA) included in the
C           User's Guide in which n effective roughness is derived using an 
C           Internal Boundary Layer (IBL) method to estimate the appropriate 
C           fetch from the meteorological tower location based on the wind 
C           measurement height and local land cover characteristics. 
C           Default option (ZORAD) uses a fixed radial distance from the 
C           tower (e.g., 1 km) as in previous versions of AERSURFACE.
C
C       5.  Option to supplement land cover data with percent impervious 
C           and percent canopy data to derive surface roughness length. 
C           Percent impervious and percent tree canopy data are supplied  
C           as part of the National Land Cover Dataset and are only 
C           applicable to post-1992 NLCD years.
C
C-----  Miscellaneous:
C
C       1.  A valid range on the allowable height of the anemometer
C           representing surface wind measurements has been imposed. 
C           The valid range is 1.0 - 100.0 meters.
C
C       2.  This version of AERSURFACE assumes all NLCD data, including
C           land cover, percent impervious, and percent canopy are
C           provided in GeoTIFF format. State binary files cannot be 
C           processed with this version.
C
C       3.  Modified to account for non-standard pixel spacing in the
C           NLCD products. Previous versions assumed an exact reported
C           pixel size of 30.0 meters. The pixel spacing in more
C           recent data may not be reported in the GeoTIFF file as 
C           exactly 30.0 meters. 
C   
C ======================================================================

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
      IF (FATAL) THEN
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
      
      if (RUNERR) go to 800

C-----------------------------------------------------------------------
C    Get counts of land use classes by sector
C-----------------------------------------------------------------------

      CALL GetLCCounts

      if (RUNERR) go to 800

C-----------------------------------------------------------------------
C    Compute the sector averaged surface characteristics - 
C    Inverse Distance or Simple method - set in mod_AvgParams.f
C    6/28/07, CRT: Simple for Albedo, distance weighted geometric mean
C       for roughness, straight geometric mean for Bowen ratio    
C-----------------------------------------------------------------------

C     Use simple (arithmetic) area weighted average for albedo
      CALL AvgSfcCharsSimple

      if (RUNERR) go to 800

C     Use geometric mean for Bowen (area weighted) and Zo (inv. 
C     distance weighted) IBL effective roughness when user opts for
C     ZOEFF, more simples inverse distance weighted average within 
C     user-defined radius when user opts for ZORAD.    

      CALL AvgSfcCharsGeom

      if (RUNERR) go to 800

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
      IF (RUN .and. RUNERR) THEN
      
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
