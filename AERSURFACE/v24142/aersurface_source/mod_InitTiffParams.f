      MODULE InitTiffParams
      
      use ProcCtrlFile, only: GotImp, UseImp,
     &    GotCan, UseCan, fileType, TiffDbg, RUNERR
     
      USE UserParams, only: LCFile, ImpFile, CanFile
      
      USE Constants, only: Eps5
     
      USE FileUnits, only: LCUnt, LogFile, LogUnt,
     &    TIFLCDbgFile, TIFLCDbgUnt,
     &    OutUnt,
     &    ImpUnt, TIFImpDbgFile, TIFImpDbgUnt,
     &    CanUnt, TIFCanDbgFile, TIFCanDbgUnt
     
      USE TiffParams, only: Proc_TiffTags,
     &    LCVars, ImpVars, CanVars
      
      USE TiffTags, only: Read_TiffTags

      implicit none
      save
      
      logical     ::  fatalErr ! flag to indicate a fatalErr error occurred, stop processing
      
      contains
 

      SUBROUTINE SetTiffParams
C ----------------------------------------------------------------------     
C        Set parameters needed to read through NLCD TIFF data
C        file.
C        
C        For when impervious and/or canopy data are included, check
C        across file set to ensure they are compatible with regard to 
C        resolution and horizontal datum.
C ----------------------------------------------------------------------

      implicit none
      
c --- initialize logical for fatal error, fatalErr
      fatalErr = .false.
      
c --- intialize logicals for impervious and canopy files (only applies to 2001 data)
      UseImp  = .false.
      UseCan  = .false.
      
c --- .tif files - get info from tiff file
      IF (fileType == 'tif') THEN 

c ---    read nlcd tiff file tags and geokeys 
         
         CALL read_tifftags(LCFile, LCUnt, LogFile, LogUnt,
     &                      TIFFDbg, TIFLCDbgFile, TIFLCDbgUnt, 'NLCD')
         CALL proc_tifftags(LCVars,'NLCD')
               
c ---    read impervious and canopy tiff file tags and geokeys
c        for primary location
         IF (GotImp) THEN
            CALL read_tifftags(ImpFile,ImpUnt,LogFile,LogUnt,
     &                      TIFFDbg,TIFImpDbgFile,TIFImpDbgUnt,'MPRV')
            CALL proc_tifftags(ImpVars,'MPRV')
            
            ! check horizonal datum
            
c ---       perform consistency checks across land cover and
c           impervious file (horizontal datum and resolution in the
c           x, y, and z directions must match across all files,
c           else abort processing)
c ---       CRT, 1/31/2020: treat NAD83, WGS72, and WGS84 identically
            IF (LCVars%hdatum == ImpVars%hdatum) THEN
                UseImp = .true.
            ELSEIF ((LCVars%hdatum == "NAD83" .or. 
     &               LCVars%hdatum == "WGS72" .or. 
     &               LCVars%hdatum == "WGS84") .and. 
     &             (ImpVars%hdatum == "NAD83" .or. 
     &              ImpVars%hdatum == "WGS72" .or. 
     &              ImpVars%hdatum == "WGS84")) THEN
                     UseImp = .true.
            ELSE
                UseImp = .false.
                fatalErr = .true.
                write(*,810) 
                write(logUnt,810)
            END IF
            
            ! Check x,y resolution

            ! Check for same pixel scales for LC and Imp data
            IF (abs(LCVars%pxlScalex - ImpVars%pxlScalex) < Eps5 .and.
     &          abs(LCVars%pxlScaley - ImpVars%pxlScaley) < Eps5) THEN
                UseImp = .true.
            ELSEIF (DABS(LCVars%pxlScalex - 
     &                  ImpVars%pxlScalex) .LE. 0.03D0) THEN
                UseImp = .true.
            ELSE
                UseImp = .false.
                write(*,820)
                write(outUnt,820)
                write(logUnt,820)
            ENDIF
            
            ! Check for consistent z resolution for LC and Imp data
            IF (abs(LCVars%pxlScalez - ImpVars%pxlScalez) < Eps5) THEN
                UseImp = .true.
            ELSE
                UseImp = .false.
                write(*,830) 
                write(outUnt,830)
                write(logUnt,830)
            END IF
            
         END IF
         
         IF (GotCan) THEN
         
            CALL read_tifftags(CanFile,CanUnt,LogFile,LogUnt,
     &                      TIFFDbg,TIFCanDbgFile,TIFCanDbgUnt,'CNPY')
            CALL proc_tifftags(CanVars,'CNPY')
            
            ! check horizonal datum
            
c ---       perform consistency checks across land cover and
c           canopy file (horizontal datum and resolution in the
c           x, y, and z directions must match across all files,
c           else abort processing)
c ---       CRT, 1/31/2020: treat NAD83, WGS72, and WGS84 identically

            IF (LCVars%hdatum == CanVars%hdatum) THEN
                UseCan = .true.          
            ELSEIF ((LCVars%hdatum == "NAD83" .or. 
     &               LCVars%hdatum == "WGS72" .or. 
     &               LCVars%hdatum == "WGS84") .and. 
     &             (CanVars%hdatum == "NAD83" .or. 
     &              CanVars%hdatum == "WGS72" .or.
     &              CanVars%hdatum == "WGS84")) THEN
                     UseCan = .true.
            ELSE
                UseCan = .false.
                fatalErr = .true.
                write(*,815) 
                write(logUnt,815)
            END IF
            
            ! Check x,y resolution
            
            ! Check for same pixel scales for LC and Can data
            IF (abs(LCVars%pxlScalex - CanVars%pxlScalex) < Eps5 .and.
     &          abs(LCVars%pxlScaley - CanVars%pxlScaley) < Eps5) THEN
                UseCan = .true.
            ELSEIF (DABS(LCVars%pxlScalex - 
     &                  CanVars%pxlScalex) .LE. 0.03D0) THEN
                UseCan = .true.
            ELSE
                UseCan = .false.
                write(*,825)
                write(outUnt,825)
                write(logUnt,825)
            ENDIF
            
            ! Check for consistent z resolution for LC and Can data
            IF (abs(LCVars%pxlScalez - CanVars%pxlScalez) < Eps5) THEN
                UseCan = .true.
            ELSE
                UseCan = .false.
                write(*,835) 
                write(outUnt,835)
                write(logUnt,835)
            END IF
            
         END IF            
         
      ENDIF

c --- if fatalErr error occurred, abort processing
      if (fatalErr) RUNERR = .true. ! stop

      return
      

c --- format statements

  810 format(
     & /," ERROR: The horizontal datum of the land cover and",
     &   " impervious data files",
     & /,"        do not match. Processing cannot complete.")

  815 format(
     & /," ERROR: The horizontal datum of the land cover and",
     &   " canopy data files",
     & /,"        do not match. Processing cannot complete.")
     
  820 format(
     & /," ERROR: The horizontal resolution of the land cover and",
     &   " impervious data files",
     & /,"        do not match. Processing cannot complete.")
     
  825 format(
     & /," ERROR: The horizontal resolution of the land cover and",
     &   " canopy data files",
     & /,"        do not match. Processing cannot complete.")
     
  830 format(
     & /," ERROR: The vertical resolution of the land cover and",
     &   " impervious data files",
     & /,"        do not match. Processing cannot complete.")
     
  835 format(
     & /," ERROR: The vertical resolution of the land cover and",
     &   " canopy data files",
     & /,"        do not match. Processing cannot complete.")
     
      END Subroutine SetTiffParams
      
      END MODULE InitTiffParams