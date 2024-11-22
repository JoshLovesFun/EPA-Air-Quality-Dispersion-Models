C=======================================================================
      MODULE TiffParams
      
C     Variables and procedures to interpret and validate TIFF tag and 
C     geoKey values needed to read and process land cover data from
C     GeoTiff files.

C     Uses the following modules:
C     - Geographic
C     - FileUnits
C     - ProcCtrlFile
C     - Constants
C     - TiffTags

C     Contains the following procedures:
C     - subroutine proc_tifftags
C     - subroutine parse_coord

C=======================================================================

      
      use Geographic, only: Esquared, EarthRadius,
     &    CentMeridian, ProjOrigin, StdParallel1, StdParallel2
     
      use FileUnits, only: LogUnt, OutUnt
      
      use ProcCtrlFile, only: RUNERR
      
      use Constants, only: Eps5
      
      use TiffTags, only: tTags, gKeys, SampleFormat, tiffwrn,
     &    tifferr, allerr, swapbytes

      implicit none
      save

      ! tiffVars stores a subset of Tiff Tag and Geo Key values needed 
      ! to read the data from the TIFF file.  There should be a separate
      ! variable declared of type tifParams for each TIFF file read/processed
      ! (e.g., Land Cover, Impervious, Canopy, Elevation)
      type tiffVars
         integer (kind=8) :: tiffCols, tiffRows  ! number of rows and columns in NLCD file
         integer (kind=8) :: tiffULX, tiffULY    ! albers coordinates of upper left corner of file
         integer (kind=8) :: tiffLRX, tiffLRY    ! albers coordinates of lower right corner of file
         
         character(len=5) :: tiffType ! data organization ('strip' || 'tile')
      
         logical   :: byteswap         ! big/little endian inconsistent between file and machine
      
         integer (kind=8)   :: rowsPerStrip      ! number of data rows per strip
         integer (kind=8)   :: tileLen           ! length (height) of tile in pixels
         integer (kind=8)   :: tileWid           ! width of tile in pixels
         integer (kind=8)   :: bytesPerSample    ! number of bytes per data sample
            
         double precision   :: tiePtx    ! upper left X coordinate GeoTIFF
         double precision   :: tiePty    ! upper left Y coordinate GeoTIFF 
         double precision   :: tiePtz    ! upper left Z coordinate GeoTIFF (should be zero)
         double precision   :: pxlScalex ! model pixel scale X-direction
         double precision   :: pxlScaley ! model pixel scale Y-direction 
         double precision   :: pxlScalez ! model pixel scale Z-direction (should be zero or one)    
      
         character(len=10)  :: hdatum    ! Horizontal datum of tiff file
      
         integer (kind=8), allocatable :: dataOS(:)  ! offsets (in bytes) to data strips or tiles  
      
         integer (kind=8) :: StudyCenterCol, StudyCenterRow ! center col and row    
           
         double precision :: CellRes      ! grid resolution at center of study area (meters)
      end type tiffVars
   
      type(tiffVars)        :: LCVars  ! tiff variables from land cover GeoTIFF
      type(tiffVars)        :: ImpVars ! tiff variables from impervious GeoTIFF
      type(tiffVars)        :: CanVars ! tiff variables from canopy GeoTIFF
      
      contains

C=======================================================================
      subroutine proc_tifftags(vars,dbgType)
      
C     Processes/evaluates a subset of Tiff tags and GeoKeys 
C     potentially returned from Read_TiffTags. 

C=======================================================================

      implicit none
      
      ! tiff parameters for processing tiff file
      type(tiffVars), intent(inout)       :: vars
      
      integer (kind=4)   :: j,k     ! loop counters
      integer (kind=4)   :: ios     ! i/o status
      integer (kind=4)   :: tCnt    ! tiff tag counter
      integer (kind=4)   :: gCnt    ! geokey counter
      integer (kind=4)   :: als     ! allocation status

      character (len=50)   :: tmpStr    ! temp variable to read or store a string      

      character (len=4)  :: dbgType

      double precision :: prjNatOriginLat  ! Projection Origin - Latitude
      double precision :: prjNatOriginLon  ! Projection Origin - Longitude
      double precision :: prjCenterLat     ! Projection Center - Latitude
      double precision :: prjCenterLon     ! Projection Center - Longitude
      double precision :: prjFalseEast     ! Projection False Easting
      double precision :: prjFalseNorth    ! Projection False Northing
      
      ! degree, minute, second components for projection parameters if input by user
      ! (standard parallels, central meridian, and latitude of projection origin)
      integer (kind=4) :: stdPar1Deg, stdPar2Deg, ctrMerdDeg, prjOrgDeg
      integer (kind=4) :: stdPar1Min, stdPar2Min, ctrMerdMin, prjOrgMin
      double precision :: stdPar1Sec, stdPar2Sec, ctrMerdSec, prjOrgSec

      ! flags to indicate whether projection params were entered by user in DMS format   
      logical          :: stdPar1DMS, stdPar2DMS, ctrMerdDMS, prjOrgDMS
          

      ! got tag flags - need these flags for checks that have dependencies
      logical          :: rowFlg          ! rows flag
      logical          :: colFlg          ! columns flag
      logical          :: geoTypeFlg      ! geographic type flag (NAD27, NAD83, WGS84)
      logical          :: geoDatumFlg     ! geographic datum flag (NAD27, NAD83, WGS84)
      logical          :: geoEllipseFlg   ! geographic ellipsoid flag (Clarke 1866, GRS80, WGS84)
      logical          :: RasterTypeFlg   ! GT Raster Type flag (PixelIsArea or PixelIsPoint)
      logical          :: PixelIsArea     ! GT Raster Type = PixelIsArea
      logical          :: PixelIsPoint    ! GT Raster Type = PixelIsPoint
      logical          :: tiePntFlg       ! model tie point flag
      logical          :: pxlSclFlg       ! pixel scale flag
      logical          :: stripOSFlg      ! data strip offsets flag
      logical          :: rowsPerStripFlg ! rows per strip flag
      logical          :: tileOSFlg       ! data tile offsets flag
      logical          :: tileLenFlg      ! tile length flag
      logical          :: tileWidFlg      ! tile width flag
      logical          :: projUntsFlg     ! projected linear units flag
      logical          :: cntrLonFlg      ! projection center - longitude flag
      logical          :: cntrLatFlg      ! projection center - latitude flag
      logical          :: orgnLonFlg      ! projection origin - longitude flag
      logical          :: orgnLatFlg      ! projection origin - latitude flag
      logical          :: cntrlMeridFlg   ! central meridian flag (center lon, then origin lon)
      logical          :: projOriginFlg   ! center latitude flag (origin lat, then center lat)
      logical          :: falseEastFlg    ! false easting flag
      logical          :: falseNorthFlg   ! false northing flag
      logical          :: stndPar1Flg     ! standard parallel 1 flag
      logical          :: stndPar2Flg     ! standard parallel 2 flag

      integer (kind=8) :: ModelType   ! Model Type code (1 for UTM; 2 for geographic)
      integer (kind=8) :: geoType     ! geographic type (NAD27, NAD83, WGS84)
      integer (kind=8) :: geoDatum    ! geographic datum (NAD27, NAD83, WGS84)
      integer (kind=8) :: geoEllipse  ! geographic ellipsoid (Clarke 1866, GRS80, WGS84)
      integer (kind=8) :: projCStype  ! projected CS type value (includes datum and UTM zone)
      integer (kind=8) :: projUnts    ! projected linear units code    
      
             
      ! tie point anchor in raster space
      double precision  :: tiePtAncx   ! anchor point for tie point, x
      double precision  :: tiePtAncy   ! anchor point for tie point, y
      double precision  :: tiePtAncz   ! anchor point for tie point, z 

      
      ! fatal error encountered when processing tag values
      ! (missing tag or unexpected value)
      logical            :: fatalErr
      
      ! error returned from subroutine (parse_coords)
      logical            :: retrnErr 

      
      ! double prec. epsilon for logical comparison

C      double precision, parameter   :: dbleps = 0.01D0
      double precision, parameter   :: dbleps = 0.1D0 !threshold increased O027 RLM WSP 20240530
      
      ! tagFlgs is an array of the tags and keys AERSURFACE either requires
      ! or will utilize if present in the GeoTIFF file
      type tagFlgs
         integer (kind=4)  :: id        ! tag id        
         character(len=20) :: name      ! tag name
         logical           :: gotTag    ! flag to indicate tag is defined in tiff file
         integer (kind=4)  :: tNdx      ! array index in tag array
      end type tagFlgs
      
      type(tagFlgs)        :: tFlgs(14)  ! tiff tags
      type(tagFlgs)        :: gFlgs(16)  ! geokeys
      
C --- initialize tag and geokey counters
      tCnt = 0
      gCnt = 0
      
C --- initialize SampleFormat to default value per TIFF6.0 specs
      SampleFormat = 1
      
C --- initialize codes to 0, kind=8 (undefined)
      ModelType    = 0_8
      geoType      = 0_8
      geoDatum     = 0_8
      geoEllipse   = 0_8
      projCStype   = 0_8
      projUnts     = 0_8
      
C --- initialize tiePtAncx,y,z 
      tiePtAncx = 0.0D0      
      tiePtAncy = 0.0D0      
      tiePtAncz = 0.0D0      

C --- initialize projection parameters (DBLE)
      prjNatOriginLat  = 0.0D0
      prjNatOriginLon  = 0.0D0
      prjCenterLat     = 0.0D0
      prjCenterLon     = 0.0D0
      prjFalseEast     = 0.0D0
      prjFalseNorth    = 0.0D0

C --- load tag flags
      tCnt = tCnt+1
      tFlgs(tCnt) = tagFlgs(256,'ImageWidth',.false.,0)
      
      tCnt = tCnt+1
      tFlgs(tCnt) = tagFlgs(257,'ImageLength',.false.,0)

      tCnt = tCnt+1
      tFlgs(tCnt) = tagFlgs(258,'BitsPerSample',.false.,0)      
      
      tCnt = tCnt+1
      tFlgs(tCnt) = tagFlgs(259,'Compression',.false.,0)
      
      tCnt = tCnt+1
      tFlgs(tCnt) = tagFlgs(273,'StripOffsets',.false.,0)
      
      tCnt = tCnt+1
      tFlgs(tCnt) = tagFlgs(274,'Orientation',.false.,0)
      
      tCnt = tCnt+1
      tFlgs(tCnt) = tagFlgs(277,'SamplesPerPixel',.false.,0)
      
      tCnt = tCnt+1
      tFlgs(tCnt) = tagFlgs(278,'RowsPerStrip',.false.,0)
      
      tCnt = tCnt+1
      tFlgs(tCnt) = tagFlgs(322,'TileWidth',.false.,0)
      
      tCnt = tCnt+1
      tFlgs(tCnt) = tagFlgs(323,'TileLength',.false.,0)
      
      tCnt = tCnt+1
      tFlgs(tCnt) = tagFlgs(324,'TileOffsets',.false.,0)
      
      tCnt = tCnt+1
      tFlgs(tCnt) = tagFlgs(339,'SampleFormat',.false.,0)
      
      tCnt = tCnt+1
      tFlgs(tCnt) = tagFlgs(33550,'ModelPixelScale',.false.,0)
      
      tCnt = tCnt+1
      tFlgs(tCnt) = tagFlgs(33922,'ModelTiePoint',.false.,0)
      
C --- load geokey flags    
      gCnt = gCnt+1
      gFlgs(gCnt) = tagFlgs(1024,'GTModelType',.false.,0)
        
      gCnt = gCnt+1
      gFlgs(gCnt) = tagFlgs(1025,'GTRasterType',.false.,0)
        
      gCnt = gCnt+1
      gFlgs(gCnt) = tagFlgs(2048,'GeographicType',.false.,0)
        
      gCnt = gCnt+1
      gFlgs(gCnt) = tagFlgs(2050,'GeogGeodeticDatum',.false.,0)
        
      gCnt = gCnt+1
      gFlgs(gCnt) = tagFlgs(2056,'GeogEllipsoid',.false.,0)
        
      gCnt = gCnt+1
      gFlgs(gCnt) = tagFlgs(3072,'ProjectedCSType',.false.,0)

      gCnt = gCnt+1
      gFlgs(gCnt) = tagFlgs(3075,'ProjCoordTrans',.false.,0)

      gCnt = gCnt+1
      gFlgs(gCnt) = tagFlgs(3076,'ProjLinearUnits',.false.,0)
        
      gCnt = gCnt+1
      gFlgs(gCnt) = tagFlgs(3078,"ProjStdParallel1",.false.,0)
        
      gCnt = gCnt+1
      gFlgs(gCnt) = tagFlgs(3079,"ProjStdParallel2",.false.,0)
        
      gCnt = gCnt+1
      gFlgs(gCnt) = tagFlgs(3080,"ProjNatOriginLong",.false.,0)
        
      gCnt = gCnt+1
      gFlgs(gCnt) = tagFlgs(3081,"ProjNatOriginLat",.false.,0)
        
      gCnt = gCnt+1
      gFlgs(gCnt) = tagFlgs(3082,"ProjFalseEasting",.false.,0)
        
      gCnt = gCnt+1
      gFlgs(gCnt) = tagFlgs(3083,"ProjFalseNorthing",.false.,0)
        
      gCnt = gCnt+1
      gFlgs(gCnt) = tagFlgs(3088,"ProjCenterLong",.false.,0)
        
      gCnt = gCnt+1
      gFlgs(gCnt) = tagFlgs(3089,"ProjCenterLat",.false.,0)

C --- initialize flags
      fatalErr = .false.
      rowFlg = .false.
      colFlg = .false.
      geoTypeFlg = .false.
      geoDatumFlg = .false.
      geoEllipseFlg = .false.
      RasterTypeFlg = .false.
      PixelIsArea = .false.
      PixelIsPoint = .false.
      tiePntFlg = .false.
      pxlSclFlg = .false.
      stripOSFlg = .false.
      rowsPerStripFlg = .false.
      tileOSFlg = .false.
      tileLenFlg = .false.
      tileWidFlg = .false.
      projUntsFlg = .false.
      cntrLonFlg = .false.
      cntrLonFlg = .false.   
      cntrLatFlg = .false.   
      orgnLonFlg = .false.   
      orgnLatFlg = .false.   
      cntrlMeridFlg = .false.
      projOriginFlg = .false.
      falseEastFlg = .false. 
      falseNorthFlg = .false.
      stndPar1Flg = .false.  
      stndPar2Flg = .false. 
      
      stdPar1DMS = .false.
      stdPar2DMS = .false.
      ctrMerdDMS = .false.
      prjOrgDMS = .false.
      
      
C --- process error and warning flags returned from read_tifftags;
c     messages should have been written to debug file, no need to duplicate
         
      ! warning         
      if (tiffwrn) then
         ! place holder
      end if
         
      ! read error (file could not be read as a tiff file)
      if (tifferr) then
         ! place holder
      end if
         
      ! allocation error (allocatable arrays)
      if (allerr) then
         ! place holder
      end if
         
      ! if read error or allocation error encountered, 
      ! set fatal flag and exit subroutine
      if (tifferr .or. allerr) then 
         fatalErr  = .true.
         return
      end if
         
C --- Set byteswap flag
      vars%byteswap = swapbytes
      
C --- Loop over tiff tags array and local tiff flags array
c     Set flags and index refs
      
      ! loop tiff tags array
      do j=1, size(tTags)    

         ! loop over local tiff flags array
         do k=1, size(tFlgs)
         
            ! process tag if in the tFlgs array        
            if (tTags(j)%id == tFlgs(k)%id) then
      
               ! set gotTag flag to true
               tFlgs(k)%gotTag = .true.
               
               ! set array index for id in tag array
               tFlgs(k)%tNdx = j
               
            end if
           
         end do
         
      end do
      
C --- Loop over geokey array and local geokey flags array
c     Set flags and index refs
      
      ! loop geokey array
      do j=1, size(gKeys)          
      
         ! loop over local geokey flags array
         do k=1, size(gFlgs)
         
            ! process tag if in the tFlgs array        
            if (gKeys(j)%id == gFlgs(k)%id) then
      
               ! set gotTag flag to true
               gFlgs(k)%gotTag = .true.
               
               ! set index in tag array
               gFlgs(k)%tNdx = j
               
            end if
            
         end do
         
      end do
      
C --- process tiff tags found to collect/evaluate values
      
      ! loop over local tiff tag flags array
      do k=1, size(tFlgs)
        
         ! if gotTag is false, store error
         select case(tFlgs(k)%gotTag)
         
         ! tag not included in geotiff file
         case(.false.)
         
            ! Optional/conditional tags that do not generate a warning message 
            ! initially if omitted (may be evaluated later in code):
            !   StripOffsets (273), conditional
            !   RowsPerStrip (278), optional with default 
            !                       default set later using ImageLength
            !   TileWidth (322), conditional
            !   TileLength (323), conditional
            !   TileOffsets (324), conditional
            select case(tFlgs(k)%id)
            case(273,278,322,323,324)
               ! do not record error
            
            ! Optional tags with default values - warn user default value used
            
            ! compression - default value is 1 (uncompressed)  
            case(259)

               write(*,105) tFlgs(k)%id, trim(tFlgs(k)%name), dbgType
               write(LogUnt,105) tFlgs(k)%id, trim(tFlgs(k)%name), 
     &                                                        dbgType
               write(*,1050)
               write(LogUnt,1050)
 105  format(
     &  /," WARNING: TIFF tag ",i5," (",a,") was not found - default",
     &    " value used: ",A4)
1050  format(
     &    "          Compression = 1; uncompressed"/)
     
            ! orientation - default value is 1 (0,0 = northwest corner)  
            case(274)

               write(*,105)tFlgs(k)%id, trim(tFlgs(k)%name), dbgType
               write(LogUnt,105) tFlgs(k)%id, trim(tFlgs(k)%name), 
     &                                                       dbgType
               write(*,1100)
               write(LogUnt,1100)
1100  format(
     &   "          Orientation = 1; row-major with (0,0) = NW corner"/)
                 
            ! sample per pixel - default value is 1, 
            ! which is expected by AERMAP     
            case(277)
                  
               write(*,105)tFlgs(k)%id, trim(tFlgs(k)%name), dbgType
               write(LogUnt,105) tFlgs(k)%id, trim(tFlgs(k)%name), 
     &                                                       dbgType
               write(*,1150)
               write(LogUnt,1150)
1150  format(
     &    "          SamplesPerPixel = 1"/)
            
            ! sample format - default value is 1 (unsigned int)      
            case(339)
               
               SampleFormat = 1

               write(*,105)tFlgs(k)%id, trim(tFlgs(k)%name), dbgType
               write(LogUnt,105) tFlgs(k)%id, trim(tFlgs(k)%name), 
     &                                                       dbgType
               write(*,1200)
               write(LogUnt,1200)
1200  format(
     &    "          SampleFormat = 1; unsigned integer"/)
            
            ! required tags not found in tiff file - fatal error.
            case default
      
               fatalErr  = .TRUE. 
               write(*,205) tFlgs(k)%id, trim(tFlgs(k)%name), dbgType
               write(LogUnt,205) tFlgs(k)%id, trim(tFlgs(k)%name), 
     $                                                        dbgType
            
            end select
 205  format(
     &  /," ERROR: TIFF tag ",i5," (",a,") was not found: ",A4)
     
            ! When TIFF tag does exist, check value
            case(.true.)
                  
            select case(tFlgs(k)%id)
            ! image width - cols
            case(256)
               ! set number of columns
               colFlg = .true.
               vars%tiffCols = tTags(tFlgs(k)%tNdx)%intVal(1)  
               write(LogUnt,'(a,i7)') 
     &            " Data Columns: ",vars%tiffCols

                           
            ! image length - rows
            case(257)
               rowFlg = .true.
               vars%tiffRows = tTags(tFlgs(k)%tNdx)%intVal(1)
               write(LogUnt,'(a,i7)') 
     &            " Data Rows:    ",vars%tiffRows
                           
            ! bits per sample - should be 8 (1-byte), 16 (2 bytes), 
            !                   32 (4 bytes), or 64 (8 bytes)
            case(258)
               if (tTags(tFlgs(k)%tNdx)%intVal(1) ==  8 .or.
     &             tTags(tFlgs(k)%tNdx)%intVal(1) == 16 .or.
     &             tTags(tFlgs(k)%tNdx)%intVal(1) == 32 .or.
     &             tTags(tFlgs(k)%tNdx)%intVal(1) == 64) then
                  ! set bytes per sample - this sets up for future 
                  ! reading files other than 1=byte data
                  vars%bytesPerSample = 
     &               tTags(tFlgs(k)%tNdx)%intVal(1)/8_8
                  
               else
                  fatalErr  = .true. 
                  write(*,305) tFlgs(k)%id, trim(tFlgs(k)%name),  
     &               tTags(tFlgs(k)%tNdx)%intVal(1), dbgType
                  write(LogUnt,305) tFlgs(k)%id, trim(tFlgs(k)%name), 
     &               tTags(tFlgs(k)%tNdx)%intVal(1), dbgType
               end if

 305  format(
     &  /," ERROR: TIFF tag ",i5," (",a,") had an unexpected value",
     &    " (",i5,"): ",A4)
                                         
            ! compression - (must be 1, uncompressed)
            case(259)
               if (tTags(tFlgs(k)%tNdx)%intVal(1) /= 1) then
                  fatalErr  = .true. 
                  write(*,305) tFlgs(k)%id, trim(tFlgs(k)%name),  
     &               tTags(tFlgs(k)%tNdx)%intVal(1), dbgType
                  write(LogUnt,305) tFlgs(k)%id, trim(tFlgs(k)%name), 
     &               tTags(tFlgs(k)%tNdx)%intVal(1), dbgType
               end if
         
            ! strip offsets
            case(273)
               stripOSFlg = .true.
               allocate(vars%dataOS(size(tTags(tFlgs(k)%tNdx)
     &             %intVal)))
               vars%dataOS = tTags(tFlgs(k)%tNdx)%intVal   
                        
            ! samples per pixel (must be 1)
            case(277)
               if (tTags(tFlgs(k)%tNdx)%intVal(1) /= 1) then
                  fatalErr  = .true.  
                  write(*,305) tFlgs(k)%id, trim(tFlgs(k)%name),  
     &               tTags(tFlgs(k)%tNdx)%intVal(1), dbgType
                  write(LogUnt,305) tFlgs(k)%id, trim(tFlgs(k)%name), 
     &               tTags(tFlgs(k)%tNdx)%intVal(1), dbgType
               end if
           ! rows per strip
            case(278)
               rowsPerStripFlg = .true.
               vars%rowsPerStrip = tTags(tFlgs(k)%tNdx)%intVal(1)
           
            ! tile width
            case(322)
               tileWidFlg = .true.
               vars%tileWid = tTags(tFlgs(k)%tNdx)%intVal(1)
         
            ! tile length
            case(323)
               tileLenFlg = .true.
               vars%tileLen = tTags(tFlgs(k)%tNdx)%intVal(1)
         
            ! tile offsets
            case(324)
               tileOSFlg = .true.
               allocate(vars%dataOS(
     &            size(tTags(tFlgs(k)%tNdx)%intVal)),stat=als)
               vars%dataOS = tTags(tFlgs(k)%tNdx)%intVal
            
            ! sample format
            ! if present, should be 1 (unsigned int) or 2 (signed int)
            case(339)
               if (tTags(tFlgs(k)%tNdx)%intVal(1) /= 1 .and.
     &             tTags(tFlgs(k)%tNdx)%intVal(1) /= 2) then
                  fatalErr  = .true.  
                  write(*,305) tFlgs(k)%id, trim(tFlgs(k)%name),  
     &               tTags(tFlgs(k)%tNdx)%intVal(1), dbgType
                  write(LogUnt,305) tFlgs(k)%id, trim(tFlgs(k)%name), 
     &               tTags(tFlgs(k)%tNdx)%intVal(1), dbgType
               end if 
            
            ! model pixel scale - X and Y must be equal, fatal error if not equal
            ! 30m is expected, warn if not 30m
            case(33550)                  

               ! set pixel scale for file
               vars%pxlScalex = tTags(tFlgs(k)%tNdx)%dblVal(1)
               vars%pxlScaley = tTags(tFlgs(k)%tNdx)%dblVal(2)
               vars%pxlScalez = tTags(tFlgs(k)%tNdx)%dblVal(3)
                  
               pxlSclFlg = .true.
                     
               write(LogUnt,325) " Spatial Res. X: ",
     &            vars%pxlScalex
               write(LogUnt,325) " Spatial Res. Y: ",
     &            vars%pxlScaley
 325  format(a,f14.8," (meters)")
 
               if (abs(vars%pxlScalex-vars%pxlScaley) > dbleps) then
C ---             dbleps = 0.01D0

CRLM              dbleps chnages to 0.1D0 O027 WSP 20240530
                  fatalErr  = .true.  

                  write(*,330) dbgType
                  write(LogUnt,330) dbgType
                  write(OutUnt,330) dbgType
CRLM                  pxlSclFlg = .false. 
CRLM O027 MRLC resolution testing WSP 20240516
 330  format(
     &  /," ERROR: The spatial resolution must be the same in the ",
     &  /,"        X and Y directions! ",A4/)

               endif

               if (abs(vars%pxlScalex-30.0D0) > 0.5D0) then
C ---             dbleps = 0.01D0
                  write(*,335) dbgType
                  write(LogUnt,335) dbgType
                  write(OutUnt,335) dbgType
                  pxlSclFlg = .true.

                  write(OutUnt,*) "Spatial Resolution (m): ",
     &                             vars%pxlScalex
                  write(LogUnt,*)
               end if
 335  format(
     &  /," WARNING: The spatial resolution of the land cover",
     &  /,"          data is NOT the expected 30 meters! ",A4/)    

               ! check value of vertical scale; warn if not zero or 1  Eps5
c               if (vars%pxlScalez /= 0.0D0 .and. 
c     &             vars%pxlScalez /= 1.0D0) then
                if (abs(vars%pxlScalez - 0.0D0) > Eps5 .and.
     &             abs(vars%pxlScalez - 1.0D0) > Eps5) then
                  write(*,338)
                  write(LogUnt,338)
                  write(LogUnt,*)
               end if
 338  format(
     &  /," WARNING: The vertical scale resolution is inconsistent ",
     &  /,"          with expected values for land cover data! ",D24.15)
                   
            ! model tie point
            case(33922)
            
               ! set anchor point for tie point
               tiePtAncx = tTags(tFlgs(k)%tNdx)%dblVal(1)
               tiePtAncy = tTags(tFlgs(k)%tNdx)%dblVal(2)
               tiePtAncz = tTags(tFlgs(k)%tNdx)%dblVal(3)
               
               ! set tie x, y tie point
               vars%tiePtx = tTags(tFlgs(k)%tNdx)%dblVal(4)
               vars%tiePty = tTags(tFlgs(k)%tNdx)%dblVal(5)
               vars%tiePtz = tTags(tFlgs(k)%tNdx)%dblVal(6)
               
               tiePntFlg = .true.
               
               ! check vertical anchor and tie point values; warn if non-zero
c               if (tiePtAncz /= 0.0D0 .or. 
c     &             vars%tiePtz /= 0.0D0) then
               if (abs(tiePtAncz - 0.0D0) > Eps5 .or. 
     &             abs(vars%tiePtz - 0.0D0) > Eps5) then
                  write(*,350)
                  write(LogUnt,350)
               end if
 350  format(
     &  /," WARNING: The vertical reference coordinates are not ",
     &  /,"          expected for 2-dimensional land cover data.")
            end select ! tFlgs%id
            
         end select ! tFlgs%gotTag

         
      end do ! end loop over tFlgs (k)
      
C --- Special tag depencency processing
c     Determine tiff type (strip or tile)
      
      ! strip offsets and rows per strip both present
      if (stripOSFlg) then
         vars%tiffType = "strip"
         write(LogUnt,'(a)') " TIFF Data Organization: Strips"
         
         ! rows per strip are unknown, use default value (image length: # rows)
         if (.not. stripOSFlg .and. rowFlg) then
            vars%rowsPerStrip = vars%tiffRows
            write(*,360) vars%tiffRows
            write(LogUnt,360) vars%tiffRows
         end if
         
 360  format(
     &    "          RowsPerStrip = ",i7/)
         
      elseif (tileOSFlg .and. tileLenFlg .and. tileWidFlg) then
         vars%tiffType = "tile"
         write(LogUnt,'(a)') " TIFF Data Organization: Tiles"
         write(LogUnt,'(a,i6)') "            Tile Length: ",
     &                          vars%tileLen
         write(LogUnt,'(a,i6)') "             Tile Width: ",
     &                          vars%tileWid
      else
         fatalErr  = .true.
         write(*,362)
         write(LogUnt,362)
      end if

 362  format(
     &  /," ERROR: GeoTIFF data organization scheme (strips",
     &  /,"        or tiles) could not be determined.") 
                      
C --- process GeoKeys found to collect/evaluate values

      ! loop over local geokey flags array
      do k=1, size(gFlgs)
             
         ! if gotTag is false, store error
         select case(gFlgs(k)%gotTag)
         case(.false.)   
                 
            ! do not record error for certain id's - place holder 
            ! 2048: Geographic Type
            ! 2050: GeogGeodeticDatum
            ! 2056: GeogEllipsoid
            ! 3078: ProjStdParallel1
            ! 3079: ProjStdParallel2
            ! 3080: ProjNatOriginLong
            ! 3081: ProjNatOriginLat
            ! 3082: ProjFalseEasting
            ! 3083: ProjFalseNorting
            ! 3088: ProjCenterLong
            ! 3089: ProjCenterLat
            
            select case(gFlgs(k)%id)
            case(2048,2050,2056,3078,3079,3080,3081,3082,
     &           3083,3088,3089)
               ! do not record error
        
            ! GTRasterType, default = 1 (PixelIsArea)
            case(1025)
               RasterTypeFlg = .false.
               PixelIsArea = .true.
               PixelIsPoint = .false.
               write(*,505) gFlgs(k)%id, trim(gFlgs(k)%name)
               write(LogUnt,505) gFlgs(k)%id, trim(gFlgs(k)%name)
               write(*,5051)
               write(LogUnt,5051)
 505   format(
     &  /," WARNING: GeoKey ",i5," (",a,") was not found - default",
     &    "          value used.")
 5051  format("          Raster Type = 1; Pixel Is Area"/)
        
            ! ProjectedCSType, default = 32767 (User Defined)
            case(3072)
            
               write(*,505) gFlgs(k)%id, trim(gFlgs(k)%name)
               write(LogUnt,505) gFlgs(k)%id, trim(gFlgs(k)%name)
               write(*,5101)
               write(LogUnt,5101)
 5101  format(
     &    "      Projected Type = 32767; User Defined"/)
        
            ! ProjLinearUnits (3076), default = 9001 (Linear Meter)
            case(3076)
            
               write(*,505) gFlgs(k)%id, trim(gFlgs(k)%name)
               write(LogUnt,505) gFlgs(k)%id, trim(gFlgs(k)%name)
               write(*,5151)
               write(LogUnt,5151)
 5151  format(
     &    "      Projected Linear Unit = 9001; Linear Meter"/)
     
            ! all other tiff tags - GTModelType, ProjCoordTrans
            case default
               fatalErr  = .true.  
               write(*,605) gFlgs(k)%id,trim(gFlgs(k)%name)
               write(LogUnt,605) gFlgs(k)%id,trim(gFlgs(k)%name)

 605  format(
     &  /," ERROR: GeoKey ",i5," (",a,") was not found.")
               
            end select

     
         case(.true.)
                  
            select case(gFlgs(k)%id)
            ! GT model type; check for 1 (projection)
            ! (1=Projection, 2=Geographic, 3=Geocentric)
            case(1024)
               select case(gKeys(gFlgs(k)%tNdx)%intVal(1))
               case(1_8)
                  write(LogUnt,607) 
  607 format(" Model Type:  Projected")
     
               case default
                  fatalErr  = .true.  
                  write(*,610) gFlgs(k)%id,trim(gFlgs(k)%name),  
     &               gKeys(gFlgs(k)%tNdx)%intVal(1)
                  write(LogUnt,610) gFlgs(k)%id,trim(gFlgs(k)%name),  
     &               gKeys(gFlgs(k)%tNdx)%intVal(1)
     
 610  format(
     &  /," ERROR: GeoKey ",i5," (",a,") had an unexpected value (",
     &    i5,").")
     
               end select
            
            ! GT raster type; check for 1 (pixel is area)
            ! (1=PixelIsArea, 2=PixelIsPoint)
            case(1025)
               
               RasterTypeFlg = .true.
               if (gKeys(gFlgs(k)%tNdx)%intVal(1) == 1) then
                  PixelIsArea = .true.
                  PixelIsPoint = .false.
               else
                  if (gKeys(gFlgs(k)%tNdx)%intVal(1) == 2) then
                     PixelIsArea = .false.
                     PixelIsPoint = .true.
                  end if
                  fatalErr  = .true. 
                  write(*,610) gFlgs(k)%id,trim(gFlgs(k)%name),  
     &               gKeys(gFlgs(k)%tNdx)%intVal(1)
                  write(LogUnt,610) gFlgs(k)%id,trim(gFlgs(k)%name),  
     &               gKeys(gFlgs(k)%tNdx)%intVal(1)
               end if
            
            ! geographic type - provides datum or ellipsoid used to derive datum
            ! datum: 4267 = NAD27, 4269 = NAD83, 4322 = WGS72, 4326 = WGS84;
            ! ellipsoid: 4008 = Clarke 1866, 4019 = GRS80, 4030 = WGS84
            case(2048)
               
               select case(gKeys(gFlgs(k)%tNdx)%intVal(1))
               ! datum
               case(4267_8,4269_8,4322_8,4326_8)
                  geoTypeFlg = .true.
                  geoType = gKeys(gFlgs(k)%tNdx)%intVal(1)
                     
               ! ellipsoid
               case(4008_8,4019_8,4030_8)
                  geoEllipseFlg = .true.
                  geoEllipse = gKeys(gFlgs(k)%tNdx)%intVal(1)
                     
               ! specified datum/ellipsoid not supported; 
               ! issue warning but continue processing for now;
               ! will be addressed later when all geokeys are processed
               case default
                  geoTypeFlg = .true.
                  geoType = gKeys(gFlgs(k)%tNdx)%intVal(1)
                  fatalErr = .true.
                  write(*,610) gFlgs(k)%id,trim(gFlgs(k)%name),  
     &               gKeys(gFlgs(k)%tNdx)%intVal(1)
                  write(LogUnt,610) gFlgs(k)%id,trim(gFlgs(k)%name),  
     &               gKeys(gFlgs(k)%tNdx)%intVal(1)
               end select
            
            ! geographic datum/ellipsoid
            ! datum: 6267 = NAD27, 6269 = NAD83, 6322 = WGS72 6326 = WGS84;
            ! ellipsoid: 6008 = Clarke 1866, 6019 = GRS80, 6030 = WGS84
            case(2050)
            
               select case(gKeys(gFlgs(k)%tNdx)%intVal(1))
               ! datum
               case(6267_8,6269_8,6322_8,6326_8)
                  geoDatumFlg = .true.
                  geoDatum = gKeys(gFlgs(k)%tNdx)%intVal(1)
                     
               ! ellipsoid
               case(6008_8,6019_8,6030_8)
                  geoEllipseFlg = .true.
                  geoEllipse = gKeys(gFlgs(k)%tNdx)%intVal(1)
                     
               ! specified datum/ellipsoid not supported; 
               ! issue warning but continue processing for now;
               ! will be addressed later when all geokeys are processed
               case default
                  geoDatumFlg = .true.
                  geoDatum = gKeys(gFlgs(k)%tNdx)%intVal(1)
                  fatalErr = .true.
                  write(*,610) gFlgs(k)%id,trim(gFlgs(k)%name),  
     &               gKeys(gFlgs(k)%tNdx)%intVal(1)
                  write(LogUnt,610) gFlgs(k)%id,trim(gFlgs(k)%name),  
     &               gKeys(gFlgs(k)%tNdx)%intVal(1)
               end select      
            
            ! geographic ellipsoid:
            ! 7008 = Clarke 1866, 7019 = GRS80, 7030 = WGS84
            case(2056)
               
               select case(gKeys(gFlgs(k)%tNdx)%intVal(1))
               case(7008_8,7019_8,7030_8)
                  geoEllipseFlg = .true.
                  geoEllipse = gKeys(gFlgs(k)%tNdx)%intVal(1)
               
               ! specified ellipsoid not supported; 
               ! issue warning but continue processing for now;
               ! will be addressed later when all geokeys are processed 
               case default
               
                  geoEllipseFlg = .true.
                  geoEllipse = gKeys(gFlgs(k)%tNdx)%intVal(1)
                  fatalErr = .true.
                  write(*,610) gFlgs(k)%id,trim(gFlgs(k)%name),  
     &               gKeys(gFlgs(k)%tNdx)%intVal(1)
                  write(LogUnt,610) gFlgs(k)%id,trim(gFlgs(k)%name),  
     &               gKeys(gFlgs(k)%tNdx)%intVal(1)
               end select            
            
            ! Projection Coordinate Transformation Method
            ! Must be Albers Equal Area
            case(3075)
               if (gKeys(gFlgs(k)%tNdx)%intVal(1) /= 11) then
                  fatalErr = .true.
                  write(*,610) gFlgs(k)%id,trim(gFlgs(k)%name),  
     &               gKeys(gFlgs(k)%tNdx)%intVal(1)
                  write(LogUnt,610) gFlgs(k)%id,trim(gFlgs(k)%name),  
     &               gKeys(gFlgs(k)%tNdx)%intVal(1)
               end if
            
            ! Projection Linear Units - must be meters
            case(3076)
               if (gKeys(gFlgs(k)%tNdx)%intVal(1) /= 9001) then
                  fatalErr = .true.
                  write(*,610) gFlgs(k)%id,trim(gFlgs(k)%name),  
     &               gKeys(gFlgs(k)%tNdx)%intVal(1)
                  write(LogUnt,610) gFlgs(k)%id,trim(gFlgs(k)%name),  
     &               gKeys(gFlgs(k)%tNdx)%intVal(1)
               end if
            
            ! Projection Standard Parallel 1
            case(3078)
               stndPar1Flg = .true.
               StdParallel1 = gKeys(gFlgs(k)%tNdx)%dblVal(1)
            
            ! Projection Standard Parallel 2
            case(3079)
               stndPar2Flg = .true.
               StdParallel2 = gKeys(gFlgs(k)%tNdx)%dblVal(1)
            
            ! Projection Natural Origin Longitude
            ! Either 3081 or 3088 should exist.
            ! If they both exist, they should be the same.
            case(3080)
               orgnLonFlg = .true.
               prjNatOriginLon = gKeys(gFlgs(k)%tNdx)%dblVal(1)
            
            ! Projection Natural Origin, Latitude
            case(3081)
               orgnLatFlg = .true.
               prjNatOriginLat = gKeys(gFlgs(k)%tNdx)%dblVal(1)
            
            ! Projection False Easting
            case(3082)
               falseEastFlg = .true.
               prjFalseEast = gKeys(gFlgs(k)%tNdx)%dblVal(1)
            
            ! Projection False Northing
            case(3083)
               falseNorthFlg = .true.
               prjFalseNorth = gKeys(gFlgs(k)%tNdx)%dblVal(1)
            
            ! Projection Center Longitude
            case(3088)
               cntrLonFlg = .true.
               prjCenterLon = gKeys(gFlgs(k)%tNdx)%dblVal(1)
            
            ! Projection Center Latitude
            case(3089)
               cntrLatFlg = .true.
               prjCenterLat = gKeys(gFlgs(k)%tNdx)%dblVal(1)
            
            end select
               
         end select 
                 
      end do ! end loop over gFlgs (k) 
      
C --- Special tag dependency processing
C     Adjust tie points, derive corner coordinates
C     These are dependent on the pixel scale and raster type, 
C     so first need to check for existence of pixel scale,
C     raster type and tie points

      if (pxlSclFlg .and. tiePntFlg) then

C ---    Process GT Raster Type code if PixelIsArea; 
C ---    adjust model tie point to center of pixel

         if (PixelIsArea) then

            vars%CellRes = vars%pxlScaleX
     
            ! adjust model tie point to "data point" location based on
            ! PixelIsArea raster type; (0,0) is upper left corner of pixel
               
            ! Adjust x-coord to center of pixel, 1/2 pixel scale to the East
            vars%tiePtx = vars%tiePtx+(0.5D0-tiePtAncx)*
     &            vars%pxlScalex
               
            ! Adjust y-coord to center of pixel, 1/2 pixel scale to the South
            vars%tiePty = vars%tiePty-(0.5D0-tiePtAncy)*
     &            vars%pxlScaley

            write(LogUnt,375)
            write(LogUnt,376)
            write(LogUnt,380) vars%tiePtx
            write(LogUnt,381) vars%tiePty
            
 375  format(" GT Raster Type = Pixel Is Area")
 376  format(" Model Tie Point (NW corner) adjusted to midpoint",
     &       " of pixel.")
 380  format("      X Tie Point: ",f15.2," (meters)")
 381  format("      Y Tie Point: ",f15.2," (meters)")          
                             
C ---       assign upper right Albers Coordinates of file     
            vars%tiffULX = IDNINT(vars%tiePtx)
            vars%tiffULY = IDNINT(vars%tiePty)
         
C ---       compute lower right Albers Coordinates of file
c           (based on known 30m cell dimension)
            vars%tiffLRX = vars%tiffULX + 
     &         ((vars%tiffCols-1_8)*IDNINT(vars%CellRes))
            vars%tiffLRY = vars%tiffULY - 
     &         ((vars%tiffRows-1_8)*IDNINT(vars%CellRes))
            
         end if
  
      end if



C --- Check datum, set datum of file (hdatum) using geoType if avail, else use geoDatum
      if (geoTypeFlg) then
         select case(geoType)
         case(4267_8)
            vars%hdatum = "NAD27"
         case(4269_8)
            vars%hdatum = "NAD83"
         case(4322_8)
            vars%hdatum = "WGS72"
         case(4326_8)
            vars%hdatum = "WGS84"
         end select
         write(LogUnt,622) trim(vars%hdatum)
         
      elseif (geoDatumFlg) then
         select case(geoType)
         case(6267_8)
            vars%hdatum = "NAD27"
         case(6269_8)
            vars%hdatum = "NAD83"
         case(6322_8)
            vars%hdatum = "WGS72"
         case(6326_8)
            vars%hdatum = "WGS84"
         end select
         write(LogUnt,622) trim(vars%hdatum)
         
      elseif (geoEllipseFlg) then
         select case(geoEllipse)
         ! Clarke 1866 ellipsoid = NAD27
         case(4008_8,6008_8,7008_8)
            vars%hdatum = "NAD27"
         ! GRS80 ellipsoid = NAD83   
         case(4019_8,6019_8,7019_8)
            vars%hdatum = "NAD83"
         ! WGS84 ellipsoid = WGS84
         case(4030_8,6030_8,7030_8)
            vars%hdatum = 'WGS84'
         end select
         write(LogUnt,622) trim(vars%hdatum)
      end if
      
  622  format(/," Horizontal Datum: ",a)   
 
C --- WARNING if geoTypeFlg, geoDatumFlg, and geoEllipseFlg are all false,
c     default to NAD83
      if (.not. geoTypeFlg .and. .not. geoDatumFlg .and. 
     &    .not. geoEllipseFlg) then
         vars%hdatum = "NAD83"
         write(*,624)
         write(LogUnt,624) 
         write(OutUnt,624)   

 624  format(
     &  /," WARNING: Horizontal Datum was not defined in file. ",
     &    " Assume NAD83/WGS84 ")

      end if
      
      
C --- Set Eccentricity and Earth radius based on datum      
      select case(vars%hdatum) 
      
      case("NAD27")
         Esquared = 0.006768658D0
         EarthRadius = 6378206.0D0
      
      case("NAD83","WGS84")
         Esquared = 0.006694380D0
         EarthRadius = 6378137.0D0
      
      end select
   

C ---------------------------------------------------------------------
C ------------- VALIDATATION OF PROJECTION PARAMETERS -----------------
C --------- AND USER PROMPTS IF DATA OMITTED FROM GEOTIFF -------------
C --------------------------------------------------------------------

      ! set projection origin - latitude
      ! use natural origin latitude if available, else center latitude
      if (orgnLatFlg) then
         ProjOrigin = prjNatOriginLat
         projOriginFlg = .true.
      elseif (cntrLatFlg) then
         ProjOrigin = prjCenterLat
         projOriginFlg = .true.
      end if
      
      ! set central meridian
      ! use center longitude if avail., else use natural origin longitude
      if (cntrLonFlg) then
         CentMeridian = prjCenterLon
         cntrlMeridFlg = .true.
      elseif (orgnLonFlg) then
         CentMeridian = prjNatOriginLon
         cntrlMeridFlg = .true.
      end if
      
      ! Verify all needed albers params where retrieved from tiff file:
      ! central meridian, projection origin, and standard paralles
      ! Prompt user if all required parameters are not included
      ! Coordinates may be entered in decimal degrees or DMS
      if (.not. cntrlMeridFlg .or. .not. projOriginFlg .or.
     &    .not. stndPar1Flg .or. .not. stndPar2Flg) then
         write(*,710)
         write(LogUnt,710)
         write(*,715)    
  710 format(
     &  /," The Albers Equal Area projection parameters could not",
     &  /," be read from the GeoTIFF file must be supplied by the",
     &    " user.")
  715 format(
     &  /," Enter projection parameters when prompted.")
        
     
         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         ! prompt for 1st standard parallel !
         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            
  720    write(*,722)
  722 format(
     & //," Enter the 1st Standard Parallel",
     &  /," (Decimal degrees or DMS separated by a space)")
     
         read(*,'(a50)',iostat=ios) tmpStr 
                      
         if( ios /= 0 )THEN
            write(*,805) 
            goto 720
         endif

c        subroutine parse_coord(inStr,ddDegs,degs,mins,secs,dmsFlg,errFlg)             
         call parse_coord(tmpStr,StdParallel1,
     &                    stdPar1Deg,stdPar1Min,stdPar1Sec,
     &                    stdPar1DMS,retrnErr)
     
         if (retrnErr) then
            write(*,805) 
            goto 720
         end if
         
         stndPar1Flg = .true.
  
         if (stdPar1DMS) then
c            write(CtlOUnt,723) stdPar1Deg, stdPar1Min, stdPar1Sec 
            write(LogUnt,724) stdPar1Deg, stdPar1Min, stdPar1Sec 
         else
c            write(CtlOUnt,725) StdParallel1
            write(LogUnt,726) StdParallel1
         endif
            
c  723 format(i3,1x,i2,1x,f5.2,T35,"** 1st Standard Parallel (DMS)") 
  724 format(/,"    1st Standard Parallel (DMS): ", i3,1x,i2,1x,f5.2)
c  725 format(f10.2,T35,"** 1st Standard Parallel") 
  726 format(/,"    1st Standard Parallel: ", f10.2)   
     
  805 format(/," Invalid Entry")      
  
       
         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         ! prompt for 2nd standard parallel !
         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            
  730    write(*,732)
  732 format(
     & //," Enter the 2nd Standard Parallel",
     &  /," (Decimal degrees or DMS separated by a space)")
     
         read(*,'(a50)',iostat=ios) tmpStr 
            
         if( ios /= 0 )THEN
            write(*,805) 
            goto 730
         endif 

c        subroutine parse_coord(inStr,ddDegs,degs,mins,secs,dmsFlg,errFlg)             
         call parse_coord(tmpStr,StdParallel2,
     &                    stdPar2Deg,stdPar2Min,stdPar2Sec,
     &                    stdPar2DMS,retrnErr)
     
         if (retrnErr) then
            write(*,805) 
           goto 730
         end if
  
         stndPar2Flg = .true.
  
         if (stdPar2DMS) then
c            write(CtlOUnt,733) stdPar2Deg, stdPar2Min, stdPar2Sec 
            write(LogUnt,734) stdPar2Deg, stdPar2Min, stdPar2Sec 
         else
c            write(CtlOUnt,735) StdParallel2
            write(LogUnt,736) StdParallel2
         endif
            
c  733 format(i3,1x,i2,1x,f5.2,T35,"** 2nd Standard Parallel (DMS)") 
  734 format("    2nd Standard Parallel (DMS): ", i3,1x,i2,1x,f5.2)
c  735 format(f10.2,T35,"** 2nd Standard Parallel") 
  736 format("    2nd Standard Parallel: ", f10.2)   
           
            
         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         ! prompt for central meridian !
         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            
  740    write(*,742)
  742 format(
     & //," Enter the Central Meridian",
     &  /," (Decimal degrees or DMS separated by a space)")  
        
         read(*,'(a50)',iostat=ios) tmpStr 
            
         if( ios /= 0 )THEN
            write(*,805) 
            goto 740
         endif    

c        subroutine parse_coord(inStr,ddDegs,degs,mins,secs,dmsFlg,errFlg)             
         call parse_coord(tmpStr,CentMeridian,
     &                    ctrMerdDeg,ctrMerdMin,ctrMerdSec,
     &                    ctrMerdDMS,retrnErr)
     
         if (retrnErr) then
            write(*,805) 
            goto 740
         end if
  
         cntrlMeridFlg = .true.
  
         if (ctrMerdDMS) then
c            write(CtlOUnt,743) ctrMerdDeg, ctrMerdMin, ctrMerdSec 
            write(LogUnt,744) ctrMerdDeg, ctrMerdMin, ctrMerdSec 
         else
c            write(CtlOUnt,745) CentMeridian
            write(LogUnt,746) CentMeridian
         endif
            
c  743 format(i4,1x,i2,1x,f5.2,T35,"** Central Meridian (DMS)") 
  744 format("    Central Meridian (DMS): ", i4,1x,i2,1x,f5.2)        
c  745 format(f10.2,T35,"** Central Meridian") 
  746 format("    Central Meridian: ", f10.2)     
  
  
         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         ! prompt for latitude of projection's origin !
         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            
  750    write(*,752)
  752 format(
     & //," Enter the Latitude of Projection Origin",
     &  /," (Decimal degrees or DMS separated by a space)")
        
         read(*,'(a50)',iostat=ios) tmpStr 
            
         if( ios /= 0 )THEN
            write(*,805) 
            goto 750
         endif        

c        subroutine parse_coord(inStr,ddDegs,degs,mins,secs,dmsFlg,errFlg)             
         call parse_coord(tmpStr,ProjOrigin,
     &                    prjOrgDeg,prjOrgMin,prjOrgSec,
     &                    prjOrgDMS,retrnErr)
     
         if (retrnErr) then
            write(*,805) 
            goto 750
         end if
         
         projOriginFlg = .true.
  
         if (prjOrgDMS) then
c            write(CtlOUnt,753) prjOrgDeg, prjOrgMin, prjOrgSec 
            write(LogUnt,754) prjOrgDeg, prjOrgMin, prjOrgSec 
         else
c            write(CtlOUnt,755) ProjOrigin
            write(LogUnt,756) ProjOrigin
         endif
         
c  753 format(i3,1x,i2,1x,f5.2,T35,
c     &       "** Latitude of Projection Origin (DMS)") 
  754 format("    Latitude of Projection Origin (DMS): ",
     &       i3,1x,i2,1x,f5.2)        
c  755 format(f10.2,T35,"** Latitude of Projection Origin") 
  756 format("    Latitude of Projection Origin: ", f10.2)
   
            
         !!!!!!!!!!!!!!!!!!!!!!!!!!!!          
         ! prompt for false easting !
         !!!!!!!!!!!!!!!!!!!!!!!!!!!!
          
  760    write(*,762)
  762 format(
     & //," Enter the False Easting (meters)")                
        
         read(*,*) prjFalseEast    
            
         if( ios /= 0 )THEN
            write(*,805) 
            goto 760
         endif 
         
         falseEastFlg = .true.    
  
c         write(CtlOUnt,765) prjFalseEast
         write(LogUnt,766) prjFalseEast  
          
c  765 format(f10.2,T35,"** False Easting") 
  766 format("    False Easting: ", f10.2) 
            
            
         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         ! prompt for false northing !
         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         
  770    write(*,772)
  772 format(
     & //," Enter the False Northing (meters)")                 
        
         read(*,*) prjFalseNorth    
            
         if (ios /= 0) THEN
            write(*,805) 
            goto 770
         endif   
         
         falseNorthFlg = .true.
  
c         write(CtlOUnt,775) prjFalseNorth
         write(LogUnt,776) prjFalseNorth  
            
c  775 format(f10.2,T35,"** False Northing") 
  776 format("    False Northing: ", f10.2) 
  
      
      ! If standard parallels, central meridian, and proj origin
      ! are included in file, but not false easting and northing,
      ! issue warning and set false coords to zero.
      elseif (.not. falseNorthFlg .or. .not. falseEastFlg) then
      
         write(*,780)
         write(LogUnt,780)
         write(OutUnt,780) 
      
  780 format(
     &  /," WARNING: Albers Projection false Easting and/or Northing",
     &  /," coordinates were not found and are assumed to be 0.0")    
           
      end if ! end of prompts for projection parameters

     
      ! false easting  - adjust tie point, upper left, and lower right 
      ! easting coordinates if false easting is not zero
      if (falseEastFlg) then
         if (abs(prjFalseEast-0.0D0) > dbleps) then
            vars%tiePtx = vars%tiePtx + prjFalseEast
            vars%tiffULX = vars%tiffULX + IDNINT(prjFalseEast)
            vars%tiffLRX = vars%tiffLRX + IDNINT(prjFalseEast)
            write(*,635) prjFalseEast, vars%tiePtx
            write(LogUnt,635) prjFalseEast, vars%tiePtx
            write(OutUnt,635) prjFalseEast, vars%tiePtx
         end if            
      end if
    
  635 format(
     &  /," WARNING: Albers Projection references false Easting",
     &  /," coordinate at origin: ", f15.2, 
     &  /," Adjust Easting model tie point to: ", f15.2)
     
      ! false northing - adjust tie point, upper left, and lower right 
      ! northing coordinates if false northing is not zero
      if (falseNorthFlg) then
         if (abs(prjFalseNorth-0.0D0) > dbleps) then            
            vars%tiePty = vars%tiePty + prjFalseNorth
            vars%tiffULY = vars%tiffULY + IDNINT(prjFalseNorth)
            vars%tiffLRY = vars%tiffLRY + IDNINT(prjFalseNorth)
            write(*,636) prjFalseNorth, vars%tiePty
            write(LogUnt,636) prjFalseNorth, vars%tiePty
            write(OutUnt,636) prjFalseNorth, vars%tiePty
         end if          
      end if
      
  636 format(
     &  /," WARNING: Albers Projection references false Northing",
     &  /," coordinate at origin: ", f15.2, 
     &  /," Adjust Northing model tie point to: ", f15.2)

         
      if (fatalErr) RUNERR = .true. ! stop        
           
      return
      
      end subroutine  
      
C=======================================================================
      subroutine parse_coord(inStr,ddDegs,degs,mins,secs,dmsFlg,errFlg)

C     Parse a string containing a geographic coordinate expressed
C     as a single value in decimal degrees or separate components
C     in whole degrees, whole minutes, and floating point seconds.
C
C     Input:  String to parse
C     Output: Coordinate in decimal degrees and individual DMS 
C             components

C=======================================================================    
      
      implicit none
      
      character(len=*), intent(in)  :: inStr  ! string to parse
      double precision, intent(out) :: ddDegs ! decimal degrees
      integer (kind=4), intent(out) :: degs   ! whole degrees component
      integer (kind=4), intent(out) :: mins   ! whole minutes component
      double precision, intent(out) :: secs   ! floating secs component
      logical,          intent(out) :: dmsFlg ! user used DMS format
      logical,          intent(out) :: errFlg ! error encountered 
      
      integer (kind=4)              :: ios     ! i/o status
      
      dmsFlg = .false.
      errFlg = .false.
      
      ! attempt to read coordinate in DMS format
      read(inStr,*,iostat=ios) degs, mins, secs
      
      ! if no error, set dms flag to true
      if (ios == 0) then
         dmsFlg = .true.
      ! if error, attempt to read single number (decimal degrees)
      else
         read(inStr,*,iostat=ios) ddDegs
         ! if error, set error flag and return
         if (ios /= 0) then
            errFlg = .true.
            return
         ! if no error, set dms flag to false
         else
            dmsFlg = .false.
         end if            
      end if
      
      ! if dms format used, set dms components and convert to decimal degrees
      if (dmsFlg) then
         if (abs(degs) > 180 .or. 
     &       mins > 60 .or. mins < 0 .or.
     &       secs > 60.0 .or. secs < 0.0) then
            errFlg = .true.
            return
         else
            ddDegs = dble((real(abs(degs)*3600)+real(mins*60)+secs)
     &               /3600.0) 
            if (degs < 0) then
               ddDegs = ddDegs*(-1.0D0)
            end if
         end if
      elseif (dabs(ddDegs) > 180.0D0) then
            errFlg = .true.
            return
      end if     
      
      return
      
      end subroutine
      
      END MODULE TiffParams