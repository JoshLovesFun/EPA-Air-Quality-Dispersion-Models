C=======================================================================
      MODULE GetData

C     Variables and procedures used to read the land cover, canopy,
C     and impervious data from the GeoTIFF files and store the portion
C     of the data within the domain required to calculate surface
C     characteristics.  Includes procedures to output grid and debug
C     files.

C     Uses the following modules:
C     - StartVars
C     - Constants
C     - ProcCtrlFile
C     - UserParams
C     - ErrorHandling
C     - FileUnits
C     - LandCoverParams
C     - Geographic
C     - TiffParams

C     Contains the following procedures:
C     - subroutine GetLCCounts
C     - subroutine CheckLC
C     - subroutine GetGridVars
C     - subroutine Read_TiffData
C     - subroutine ProcessLC
C     - subroutine Write_DbgGrid
C     - subroutine Write_LogGrid
      
C=======================================================================
      
      USE StartVars, only: run_start, iYr2
      use Constants, only: ExceedTrans, TransCode, MissCode, 
     &                     ExceedMissHi, ExceedMissLow, Rad2Deg,
     &                     FilNmLen, MaxSectors
     
      use ProcCtrlFile, only: NLCDYear, ILINE,
     &    UseCan, UseImp, GridDbg

      use UserParams, only: WndSectors, CoordType, CoordTypeB,
     &    CenterLonIn, CenterLatIn, CenterUTMZone,
     &    CenterUTMN, CenterUTME,Datum,
     &    LCFile, ImpFile, CanFile,
     &    VaryAP, NumSectors, StartDir, Airport, Moisture, Arid,
     &    SeasonMnths, Zo_Method, Anem_Height, IBL_Factor, ZoRad
                
      use FileUnits, only: NGPath,
     &    LogUnt, LogFile, OutUnt,
     &    LCUnt, ImpUnt, CanUnt, 
     &    LCGridUnt, ImpGridUnt, CanGridUnt,
     &    LCGridFile, ImpGridFile, CanGridFile
     
      USE ErrorHandling, only: ERRHDL

      use LandCoverParams, only: InitLCSurfChar, ConsolSfcVals,
     &    NumLCTypes, LCSurfChar, Bowen, Z0, Z092_22, Z092_31,
     &    Z092_43, Z092_85, LCCatsAll92, LCCatsAll01,
     &    LCCatsOnly92, LCCatsOnly01
     
      use Geographic, only: ConfirmBounds,
     &    LATLON2ALBERS, ALBERS2LATLON, NADCON, UTMGEO, NGRIDS
     
      use TiffParams, only: tiffVars, LCVars, ImpVars, CanVars

      implicit none
      save
      
C ----------------------------------------------------------------------
C     Parameters and data needed to perform averages
C ----------------------------------------------------------------------  
      
C --- Exponent for distance for Inverse Distance averaging method
      
      INTEGER (kind=4), parameter     :: InvDPwr = 1  ! for roughness
      
C --- Multiplier to compute limit on minimum distance from study center to 
C     pixel center.  As distance goes to zero, influence of pixel on 
C     the average increases.  If distance is zero, will get error. 
C     Minimum distance is some fraction or percent of the pixel resolution 
      DOUBLE PRECISION, parameter     :: MinDFactr = 0.5D0 ! for roughness
      
C --- Minimum distance limit from study center to pixel center used in 
C     computing weighted inverse distance average of roughness
      DOUBLE PRECISION     :: MinD
      
C --- Default Internal Boundary Layer Height
      DOUBLE PRECISION     :: IBL_Height = 60.0D0 ! meters
      
C     Bowen ratio and albedo based on square area in which length and width are 2x StudyDomMeters
C     Roughness based on circular area with a radius = StudyRadiusMeters
      double precision, parameter :: StudyDomMeters = 5050.0D0      ! length and width of study domain from site
      double precision, parameter :: StudyRadiusMeters = 5050.0D0   ! length of study radius
      
      double precision            :: RingWidth! = 30.001D0
      integer (kind=4)            :: NumRings!  = 169 ! to cover 5 km based on ~ 30m RingWidth

C --- Counts for each land use category by sector
C     bowen ratio and albedo; surface roughness
      INTEGER (kind=4) :: LandCoverBr(MaxSectors,0:NumLCTypes-1)
      INTEGER (kind=4) :: LandCoverAlb(MaxSectors,0:NumLCTypes-1)
      INTEGER (kind=4) :: LandCoverZ0(MaxSectors,0:NumLCTypes-1)
 
C --- Total cell counts by sector (INCLUDE missing data and out of boundary)
C     bowen ratio and albedo; surface roughness
      INTEGER (kind=4) :: LandCovTotBr(MaxSectors) 
      INTEGER (kind=4) :: LandCovTotAlb(MaxSectors)
      INTEGER (kind=4) :: LandCovTotZ0(MaxSectors) 
             
C --- Total cell counts by sector (EXCLUDE missing data and out of boundary)
C     bowen ratio and albedo; surface roughness
      INTEGER (kind=4) :: LCCountBr(MaxSectors)
      INTEGER (kind=4) :: LCCountAlb(MaxSectors)
      INTEGER (kind=4) :: LCCountZ0(MaxSectors)
      

C --- Arrays used to hold a running sum of surface values by sector
C     up to 12 averaging periods (annual(1), seasonal(1-4), months(1-12))
      DOUBLE PRECISION    :: AlbSum(MaxSectors,12)
      DOUBLE PRECISION, ALLOCATABLE  :: ZoSumEff(:,:,:) ! ZOEFF
      DOUBLE PRECISION    :: ZoSumRad(MaxSectors,12)          ! ZORAD
      DOUBLE PRECISION    :: ZoSum(MaxSectors,12)             ! Post-processing   
      DOUBLE PRECISION    :: BowenSum(MaxSectors,12)
      
C --- Arrays used to hold sum of 1/distance for distance weighted averaging
C     Hold sum 1/d^InvDPwr
      DOUBLE PRECISION, ALLOCATABLE :: InvDSumZoEff(:,:,:) ! ZOEFF
      DOUBLE PRECISION    :: InvDSumZoRad(MaxSectors,12)          ! ZORAD  

C     Store data read from GeoTiff files     
      type tiffData

         integer (kind=8), allocatable   :: grid(:,:)   ! array to hold values
         integer (kind=8)                :: GridNRows   ! number of rows
         integer (kind=8)                :: GridNCols   ! number of cols
         integer (kind=8)                :: GridFrstRow ! first row
         integer (kind=8)                :: GridLastRow ! last row
         integer (kind=8)                :: GridFrstCol ! first row
         integer (kind=8)                :: GridLastCol ! last row
         integer (kind=8)                :: GridRowCnt  ! counter for filling array
         integer (kind=8)                :: GridColCnt  ! counter for filling array
         integer (kind=8)                :: GridCntrCol ! center col
         integer (kind=8)                :: GridCntrRow ! center row
         double precision                :: CntrAlbersX ! center of study area in Albers coords. (not center pixel)
         double precision                :: CntrAlbersY ! center of study area in Albers coords. (not center pixel)
         double precision                :: GridToTruDif ! degrees difference in grid north to true north
         double precision                :: DomAlbersLLX ! x-coord of lower left corner of domain  (Albers)
         double precision                :: DomAlbersLLY ! y-coord of lower left corner of domain  (Albers)
         double precision                :: CenterLat    ! latitude of domain center, decimal degrees, NAD83
         double precision                :: CenterLon    ! longitude of domain center, decimal degrees, NAD83
         
      end type   

      type(tiffData)  :: LCGrid   ! land cover values
      type(tiffData)  :: ImpGrid  ! impervious values
      type(tiffData)  :: CanGrid  ! canopy values

CCRT ----------------------------------------------------------------------
C     There are known cases in which the sum of the percent impervious area 
C     and the percent canopy area is greater than 100.  This is a concern
C     in particular when using 2001 canopy with 2006 impervious data since
C     the canopy data has not been updated to be current with the 2006 
C     impervious data. In order to warn the user when this occurs to an
C     excessive degree, count the total number of cells within some defined
C     radius, the number of cells for which canopy+impervious is greater
C     than some threshold above 100% for the same area, and compute the
C     percentage of cells that meet this criteria.

CCRT- Distance limit from primary site (Zo) for checking sum of 
C     canopy+impervious percents
CCRT- Threshold criteria when adding canopy+impervious cell gets counted
      DOUBLE PRECISION :: CanImpZoSumThrsh = 1.5D0
CCRT- Threshold criteria for percent of # cells where canopy+impervious
C     is greater than CanImpZoSumThrsh (issue warning above this amount)
      DOUBLE PRECISION :: CanImpZoPctThrsh = 0.01D0
CCRT- Count of the total number of cells within ZoRad of the 
C     primary site (Zo)      
      INTEGER (kind=4) :: CanImpZoCnt = 0
CCRT- Count of cells within ZoRad of the primary site (Zo) where
C     the sum of canopy+impervious > CanImpZoSumThrsh
      INTEGER (kind=4) :: CanImpZoNvldCnt = 0
CCRT- Percent of cells within ZoRad of the primary site (Zo) 
C     for which the sum of canopy+impervious > CanImpZoSumThrsh
      DOUBLE PRECISION :: CanImpZoNvldPct = 0.0D0

CCRT ----------------------------------------------------------------------   
   
      contains      

C=======================================================================
      SUBROUTINE GetLCCounts
C     
C     Purpose:  Get Land Cover counts for user-specified domain for 
C               albedo, Bowen ratio and surface roughness.
C
C     NOTE: The term cell refers to the area that one pixel in
C           the NLCD covers

C=======================================================================

      use ProcCtrlFile, only: RUNERR

      implicit none

      double precision   :: CellX, CellY      ! Cell albers coordinates (in meters)

      integer (kind=4)   :: MissCountBa, MissCountZ0   ! Total missing data cells + cells outside of state boundary

      integer (kind=4)   :: iGrdUnit
                            
      integer (kind=4)   :: kdum, isector ! counters
      integer (kind=8)   :: icol, jrow    ! counters            
      
      integer (kind=8)   :: tmprow, tmpcol  
      
      double precision   :: XTmp, YTmp  ! temp coordinates (UTM or Lat/Lon)
      integer (kind=4)   :: ZoneTmp     ! temp UTM zone
      
      logical            :: OK2Proceed ! OK flag
      
      iGrdUnit = 99
     
C --- initialize flag to proceed
      OK2Proceed = .true.
C --- initialize flag for "runtime" error
      RUNERR = .FALSE.
           
C --- Initialize arrays
      LandCoverBr(:,:)  = 0
      LandCoverAlb(:,:) = 0
      LandCoverZ0(:,:)  = 0
      
      LandCovTotBr(:)  = 0
      LandCovTotAlb(:) = 0
      LandCovTotZ0(:)  = 0
      
      LCCountBr(:)  = 0
      LCCountAlb(:) = 0
      LCCountZ0(:)  = 0
      
      AlbSum(:,:)     = 0.0D0
C      ZoSumEff(:,:,:) = 0.0D0
      BowenSum(:,:)   = 0.0D0
      
C      InvDSumZoEff(:,:,:) = 0.0D0
      
     
C --- Estimate "effective" radius for surface roughness land cover counts
C     if Zo_Method is ZOEFF, else use user-defined radius
      IF (Zo_Method == 'ZOEFF') THEN
         ZoRad = MIN( Anem_Height * IBL_Factor * 15.0D0, 5000.0D0 )
      END IF

      
C --- initialize counters (kind=8)
      LCGrid%GridRowCnt   = 0_8
      ImpGrid%GridRowCnt  = 0_8  
      CanGrid%GridRowCnt  = 0_8 

C-----------------------------------------------------------------------
C --- Compute domain and grid variables needed to read tiff data
C --- Confirm the data are in the domain
C --- Initialize grid arrays that store data
C --- Read tiff data
C     
C     Do this for each file specified for primary and secondary domains.
C-----------------------------------------------------------------------

C --- Primary Location
      
C --- determine coordinates and utm zone to pass
      IF (CoordType == "UTM") THEN
         ZoneTmp = CenterUTMZone
         XTmp = CenterUTME
         YTmp = CenterUTMN
      ELSE IF (CoordType == "LATLON") THEN
         ZoneTmp = 0
         XTmp = CenterLonIn
         YTmp = CenterLatIn
      END IF
  
C --- derive domain and grid variables for LC file
      call GetGridVars(LCVars, LCGrid, CoordType, XTmp, YTmp, ZoneTmp,
     &                 Datum, LCFile, 'NLCD')

C --- Initialize to zero with double precision
      LCGrid%grid = 0_8

C --- read tiff data from LC file

      call Read_TiffData(LCVars, LCGrid, LCFile, LCUnt, 'LC',
     &         NLCDYear)

C --- Impervious and Canopy files
      IF (UseImp) THEN     
C ---    derive domain and grid variables for impervious file
         call GetGridVars(ImpVars, ImpGrid, CoordType, XTmp, YTmp, 
     &                    ZoneTmp, Datum, ImpFile, 'MPRV')

C ---    Initialize to 127 with double precision
         ImpGrid%grid = 127_8
   
C ---    read tiff data from impervious file
         call Read_TiffData(ImpVars, ImpGrid, ImpFile, ImpUnt, 'Imp',
     &                     NLCDYear)
      ELSE
         ALLOCATE(ImpGrid%grid(1_8:LCGrid%GridNRows,
     &                         0_8:LCGrid%GridNCols+1_8))
         ImpGrid%grid = 0_8
      END IF
      
      IF (UseCan) THEN
C ---    derive domain and grid variables for canopy file
         call GetGridVars(CanVars, CanGrid, CoordType, XTmp, YTmp, 
     &                    ZoneTmp, Datum, CanFile, 'CNPY')
     
C ---    Initialize to 127 with double precision
         CanGrid%grid = 127_8
   
C ---    read tiff data from canopy file
         call Read_TiffData(CanVars, CanGrid, CanFile, CanUnt, 'Can',
     &                      NLCDYear) 
      ELSE
         ALLOCATE(CanGrid%grid(1_8:LCGrid%GridNRows,
     &                         0_8:LCGrid%GridNCols+1_8))
         CanGrid%grid = 0_8
      END IF


C----------------------------------------------------------------------- 
C --- Load surface characteristic arrays based on the nlcd type (year),  
C --- then loop over grid arrays and process land use to get surface char
C --- values and counts.  Do this first for primary location, then
C --- repeat for secondary location if specified.  Compute Albers
C --- current cell coordinate in Albers (meters) using land cover grid.
C-----------------------------------------------------------------------     
      
C --- Initialize seasonal land cover value arrays based on NLCD version (year)
c     Separate arrays are used for each surface parameter and additional arrays
c     are used for variation between airport/non-airport and arid conditions

      CALL InitLCSurfChar(NLCDYear)

C --- Consolidate seasonal land cover values into a single array per surface 
c     parameter, accounting for the variation for airport sites and arid
c     conditions.  If the site is not an airport, ConsolSfcVals should
c     only need to be called once.  If sectors are specified individually
c     as airport/non-airport, ConsolSfcVals will be called for each LC cell
c     processed in ProcessLC based on the sector within each cell resides.
      IF (.not. VaryAP) THEN
         CALL ConsolSfcVals(NLCDYear,Airport,Moisture,Arid)
      END IF   

C --- Process land cover value
      do jrow=1_8,size(LCGrid%grid,1)
         do icol=1_8,size(LCGrid%grid,2)-2_8

            ! compute row and column in file
            tmprow = LCGrid%GridFrstRow+(jrow-1_8)
            tmpcol = LCGrid%GridFrstCol+(icol-1_8)         
            
            ! compute CellX and CellY
            CellX = dble(LCVars%tiffULX) + (dble(tmpcol-1_8)*
     &                   LCVars%CellRes)
            CellY = dble(LCVars%tiffULY) - (dble(tmprow-1_8)*
     &                   LCVars%CellRes)
            
            ! process cell
            Call ProcessLC(LCVars,LCGrid,
     &                     NLCDYear,CellX,CellY,
     &                     LCGrid%grid(jrow,icol),
     &                     ImpGrid%grid(jrow,icol),
     &                     CanGrid%grid(jrow,icol))
         end do
      end do

C-----------------------------------------------------------------------
C     Output land cover counts for roughness to log file
C-----------------------------------------------------------------------
C --- Add header with version date and run date:

      WRITE(LogUnt,1333)
 1333 FORMAT(/'***************************')


      WRITE(LogUnt,1307) INT(ZoRad)
 1307 FORMAT(/' Land Cover Counts: Surface Roughness',
     &        ' (Within ',I4,'m Radius)')

C --- Skip over message for error writing to file
      GO TO 7777

C     WRITE Error Message:  Error Writing to File
C 350  WRITE(*,351) trim(LogFile)
C 351  FORMAT(/,' Error Writing to Log File: ',a,//,
C     &         ' Processing Aborted!')

C      STOP

7777  CONTINUE
      
C     Advance to next line of log file    

      WRITE(LogUnt,1308) (isector,isector=1,NumSectors)
 1308 FORMAT(/,29x,'SECTOR:',1x,16(4x,i4))

      WRITE(LogUnt,1310) (StartDir(isector),isector=1,NumSectors)
 1310 FORMAT('Cat',14x,'Starting Direction:',1x,16(2x,f6.1))
      WRITE(LogUnt,1303)
      
c --- output counts to log
      DO kdum = 0, NumLCTypes-1
         IF (LEN_TRIM(LCSurfChar(kdum)%ClassName) .NE. 0) THEN
            WRITE(LogUnt,1315) kdum, Trim(LCSurfChar(kdum)%ClassName),
     &                  (LandCoverZ0(isector,kdum), 
     &                   isector=1,NumSectors)
         END IF
      END DO
 1315 FORMAT(1x,i2,1x,a31,': ',16(i8))
 
c --- output total counts of cells per sector - to log
      WRITE(LogUnt,1303)
      WRITE(LogUnt,1316)'Total', 
     &      (LandCovTotZ0(isector),isector=1,NumSectors)
     
 1316 FORMAT(1x,a34,': ',16(i8))

C-----------------------------------------------------------------------
C     Output land cover counts for bowen/albedo to log file
C-----------------------------------------------------------------------

      WRITE(LogUnt,1301)
 1301 FORMAT(/' Land Cover Counts: Bowen Ratio and Albedo',
     &        ' (10km X 10km Domain)'/)
 
c --- output counts to log
      DO kdum = 0, NumLCTypes-1
         IF( LEN_TRIM(LCSurfChar(kdum) % ClassName) .NE. 0)THEN
            WRITE(LogUnt, 1302) kdum, Trim(LCSurfChar(kdum)%ClassName),
     &                 LandCoverBr(1,kdum)
         ENDIF
      END DO
 1302 FORMAT(i3,1x,a31,': ',i8)
 
      WRITE(LogUnt,1303)
 1303 FORMAT(36('-'))
 

      WRITE(LogUnt,1304)'Total', 
     &      LandCovTotBr(1)
     
 1304 FORMAT(1x,a34,': ',i8)

C----------------------------------------------------------------------- 
C     Output land cover grid from primary site to log file
C-----------------------------------------------------------------------

      
C-----------------------------------------------------------------------
C     Create land cover grid file for import into Arc GIS
C-----------------------------------------------------------------------

      IF (GridDbg) THEN

         call Write_DbgGrid(LCVars,LCGrid,"LC",LCGridUnt,
     &                   LCGridFile,LCFile)    
         
      END IF       
       
C-----------------------------------------------------------------------
C     Create impervious and canopy grid files
C-----------------------------------------------------------------------
      
      IF (GridDbg .and. UseImp) THEN

         call Write_DbgGrid(ImpVars,ImpGrid,"Imp",ImpGridUnt,
     &                      ImpGridFile,ImpFile)
         
      ENDIF

      IF (GridDbg .and. UseCan) THEN

         call Write_DbgGrid(CanVars,CanGrid,"Can",CanGridUnt,
     &                      CanGridFile,CanFile)
     &
         
      ENDIF


C-----------------------------------------------------------------------
C     Determine if count for Transitional class exceeds 10% in a sector (1992 NLCD Only)
C     Determine if count for missing (out of state boundary), 99, 
C     exceeds low bounds or high bounds in a sector; between low and high % 
C     generates a warning on screen and in log; >high boungs % terminates program
C     % must be calculated for B, alb separately from Z0
C-----------------------------------------------------------------------

      WRITE(LogUnt, *) 

c     compute for % for Bowen ratio and albedo
      IF( LandCovTotBr(1) > 0 )THEN
c        Transition cells, only applies to 1992 NLCD
         IF (NLCDYear == 1992) THEN        
            IF( REAL(LandCoverBr(1,TransCode))/
     &          REAL(LandCovTotBr(1))
     &         >  ExceedTrans )THEN
               WRITE(*, 1320) 
     &            (REAL(LandCoverBr(1,TransCode))
     &            /REAL(LandCovTotBr(1)))*100.,
     &            TransCode
               WRITE(LogUnt, 1320) 
     &            (REAL(LandCoverBr(1,TransCode))
     &            /REAL(LandCovTotBr(1)))*100.,
     &            TransCode
               WRITE(OutUnt, 1320) 
     &            (REAL(LandCoverBr(1,TransCode))
     &            /REAL(LandCovTotBr(1)))*100.,
     &            TransCode
            END IF
         END IF   
         
 1320 FORMAT(
     &  /,' WARNING: ',f5.1,'% of the cells extracted to compute ',
     &  /,' Bowen ratio and albedo are classified as "Transitional" 
     &   (',i2,').')
          
C        Missing data cells - outside of state boundary
         MissCountBa = LandCoverBr(1,MissCode)
         IF( REAL(MissCountBa)/
     &       REAL(LandCovTotBr(1)) >=  ExceedMissLow .and.
     &       REAL(MissCountBa)/
     &      REAL(LandCovTotBr(1)) <= ExceedMissHi )THEN
            WRITE(*, 1325)  
     &        (REAL(MissCountBa)
     &        /REAL(LandCovTotBr(1)))*100.
            WRITE(LogUnt, 1325) 
     &        (REAL(MissCountBa)
     &        /REAL(LandCovTotBr(1)))*100.
            WRITE(OutUnt, 1325) 
     &        (REAL(MissCountBa)
     &        /REAL(LandCovTotBr(1)))*100.
     
 1325 FORMAT( 
     &  /,' WARNING: The land cover values for ', f5.1, '% of',
     &  /,' the cells extracted to compute Bowen ratio and albedo',
     &  /,' are either missing, outside of the NLCD file boundary,',
     &  /,' or invalid.')         

         ELSEIF( REAL(MissCountBa)/
     &          REAL(LandCovTotBr(1)) > ExceedMissHi )THEN
            WRITE(*, 1326) 
     &        (REAL(MissCountBa)
     &        /REAL(LandCovTotBr(1)))*100.
            WRITE(LogUnt, 1326)  
     &        (REAL(MissCountBa)
     &        /REAL(LandCovTotBr(1)))*100.
            WRITE(OutUnt, 1326)  
     &        (REAL(MissCountBa)
     &        /REAL(LandCovTotBr(1)))*100.
            OK2Proceed = .FALSE.

 1326 FORMAT( 
     &  /,' ERROR: The land cover values for ', f5.1, '% of',
     &  /,' the cells extracted to compute Bowen ratio and albedo',
     &  /,' are either missing, outside of the NLCD file boundary,',
     &  /,' or invalid.')           
         ENDIF 
      ENDIF
         
      DO isector=1,NumSectors
c        compute for % for Z0
         IF( LandCovTotZ0(isector) > 0 )THEN
c           Transition cells, only applies to 1992 NLCD
            IF (NLCDYear == 1992) THEN       
               IF( REAL(LandCoverZ0(isector,TransCode))/
     &             REAL(LandCovTotZ0(isector))
     &            >  ExceedTrans )THEN
                  WRITE(*, 1330) 
     &           (   REAL(LandCoverZ0(isector,TransCode))/
     &               REAL(LandCovTotZ0(isector)))*100.,
     &               isector, TransCode
                  WRITE(LogUnt, 1330) 
     &           (   REAL(LandCoverZ0(isector,TransCode))/
     &               REAL(LandCovTotZ0(isector)))*100.,
     &               isector, TransCode
                  WRITE(OutUnt, 1330) 
     &           (   REAL(LandCoverZ0(isector,TransCode))/
     &               REAL(LandCovTotZ0(isector)))*100.,
     &               isector, TransCode
               END IF
            END IF
            
 1330 FORMAT(
     &  /,' WARNING: ',f5.1,'% of the cells extracted to compute ',
     &  /,' surface roughness in sector ',i2, ' are classified',
     &  /,' as "Transitional" (',i2,').')
            
C           Missing data cells - outside of state boundary
            MissCountZ0 = LandCoverZ0(isector,MissCode)
            IF( DBLE(MissCountZ0)/
     &          DBLE(LandCovTotZ0(isector)) >=  ExceedMissLow .and.
     &          DBLE(MissCountZ0)/
     &          DBLE(LandCovTotZ0(isector)) <= ExceedMissHi )THEN
               WRITE(*, 1335)  
     &           (REAL(MissCountZ0)
     &           /REAL(LandCovTotZ0(isector)))*100., isector
               WRITE(LogUnt, 1335) 
     &           (REAL(MissCountZ0)
     &           /REAL(LandCovTotZ0(isector)))*100., isector
               WRITE(OutUnt, 1335) 
     &           (REAL(MissCountZ0)
     &           /REAL(LandCovTotZ0(isector)))*100., isector
     
 1335 FORMAT( 
     &   /,' WARNING: The land cover values for ', f5.1, '% of',
     &   /,' the cells extracted to compute surface roughness',
     &   /,' in sector ',i2,' are either missing, outside of the',
     &   /,' NLCD file boundary, or invalid.')
     
            ELSEIF( REAL(MissCountZ0)/
     &             REAL(LandCovTotZ0(isector)) > ExceedMissHi )THEN
               WRITE(*, 1336) 
     &           (REAL(MissCountZ0)
     &           /REAL(LandCovTotZ0(isector)))*100., isector
               WRITE(LogUnt, 1336)  
     &           (REAL(MissCountZ0)
     &           /REAL(LandCovTotZ0(isector)))*100., isector
               WRITE(OutUnt, 1336)  
     &           (REAL(MissCountZ0)
     &           /REAL(LandCovTotZ0(isector)))*100., isector
               OK2Proceed = .FALSE.
               
 1336 FORMAT( 
     &   /,' ERROR: The land cover values for ', f5.1, '% of',
     &   /,' the cells extracted to compute surface roughness',
     &   /,' in sector ',i2,' are either missing, outside of the',
     &   /,' NLCD file boundary, or invalid.')
               
            ENDIF 
         ENDIF        
         
      ENDDO
      
CCRT- -------------------------------------------------------------------
C     Issue warning if the percentage of cells for which Canopy+Impervious
C     is greater than defined threshold percent.  
CCRT- -------------------------------------------------------------------
      IF (UseImp .and. UseCan) THEN
         IF (CanImpZoCnt > 0) THEN
            CanImpZoNvldPct = 
     &          DBLE(CanImpZoNvldCnt)/DBLE(CanImpZoCnt)
         ELSE
            CanImpZoNvldPct = 0.0D0
         END IF
         
         IF (CanImpZoNvldPct > CanImpZoPctThrsh) THEN
            WRITE(*,1338) CanImpZoNvldPct * 100.0D0, 
     &                    CanImpZoSumThrsh* 100.0D0
            WRITE(LogUnt,1338) CanImpZoNvldPct * 100.0D0, 
     &                         CanImpZoSumThrsh* 100.0D0
            WRITE(OutUnt,1338) CanImpZoNvldPct * 100.0D0, 
     &                         CanImpZoSumThrsh* 100.0D0 
         END IF
      END IF
      
 1338 FORMAT( 
     &   /,' WARNING: The sum of the canopy + impervious percent', 
     &   / ' values for ',f5.1,'% of the cells within a 1km',
     &   /,' radius of the met tower exceeds ',f5.1,'%.')

c     abort processing if loop is on last sector    
      IF( .Not. OK2Proceed )THEN
         WRITE(*,1340)
         WRITE(LogUnt,1340)
         WRITE(OutUnt,1341)
         RUNERR = .TRUE.
      ENDIF

 1340 FORMAT (/, ' AERSURFACE processing aborted!')
 1341 FORMAT (//,' AERSURFACE processing aborted!')
 
      CLOSE(LCUnt)
 
      RETURN
    
      END SUBROUTINE GetLCCounts
      
C=======================================================================       

      subroutine CheckLC(CellLC,MissCode,LCYear,FName)
      
C --- Checks landcover category retrieved from a data cell and verifies if it is
C     valid against either the 1992 or 2001 NLCD categories  (2006 are the same
C     as 2001.)  If value is not valid for either NLCD year, value is set to "0" 
C     for missing, out-of-bounds or invalid.  If it is a value that is distinct 
C     for one of the years, the year is compared against the NLCDYear. An error 
C     is generated if there is a conflict between the year specific category and 
C     the NLCDYear.

C=======================================================================      
      implicit none
           
      integer (kind=4), intent(inout)   :: CellLC   ! land cover category value
      integer (kind=4), intent(in)      :: MissCode ! missing value in file
      integer (kind=4), intent(in)      :: LCYear   ! NLCD version 1992, 2001, 2006 based on control file
      character (len=*), intent(in)     :: FName    ! data file name
      
      integer (kind=4)                  :: CatYear   ! Derived NLCD version based on category lookup
      
      character (len=FilNmLen), save    :: FNameTmp = ""  ! data file name from previous call
      
      integer (kind=4)                  :: iCat     ! loop counters
      
      ! flag to indicate LC value is valid for either 1992, 2001, or 2006
      logical  :: LCValidFlg   
       
      ! saved flag to indicate message previously written for LC cat not found
      ! in LC category list
      logical, save  :: InvalidLCMsg = .false.
      
C --- initialize variables
      LCValidFlg = .false.
      CatYear = 0

C --- if temp file name has not been set, initialize to file name passed 
      if (len(trim(FNameTmp)) == 0) then
         FNameTmp = FName
      end if
      
C --- reset message flag if new file is being processed
      if (trim(FName) /= trim(FNameTmp)) then
         InvalidLCMsg = .false.
      end if
      
C --- Verify LC value is valid based on LC categories for 1992 and 2001 (2006 same as 2001)
      if (LCYear == 1992) then    
         do iCat=1, size(LCCatsAll92)
            if (CellLC == LCCatsAll92(iCat)) then
               LCValidFlg = .true.
               CatYear = 1992
               exit
            end if
         end do
CRLM  O012_DataYearKeyword_WSP (BEGIN)         
CRLM     elseif (LCYear >= 2001) then
         else
CRLM  O012_DataYearKeyword_WSP (END)
         do iCat=1, size(LCCatsAll01)
            if (CellLC == LCCatsAll01(iCat)) then
               LCValidFlg = .true.
               CatYear = 2001
               exit
            end if
         end do   
      end if
CRLM  O012_DataYearKeyword_WSP (BEGIN)
CRLM Switching order to LCYear==1992, else
C --- Check to ensure code is not valid with a category unique to a different year
      if (LCYear == 1992) then
         do iCat=1, size(LCCatsOnly01)
            if (CellLC == LCCatsOnly01(iCat)) then
               CatYear = 2001  ! also used for 2006
               exit
            end if
         end do
      else
         do iCat=1, size(LCCatsOnly92)
            if (CellLC == LCCatsOnly92(iCat)) then
               CatYear = 1992
               exit
            end if
         end do
      end if
CRLM Original code below to end
CRLMC --- Check to ensure code is not valid with a category unique to a different year
CRLM      if (LCYear >= 2001) then
CRLM         do iCat=1, size(LCCatsOnly92)
CRLM            if (CellLC == LCCatsOnly92(iCat)) then
CRLM               CatYear = 1992
CRLM               exit
CRLM            end if
CRLM         end do
CRLMC --- for 2001 and 2006
CRLM      elseif (LCYear == 1992) then
CRLM         do iCat=1, size(LCCatsOnly01)
CRLM            if (CellLC == LCCatsOnly01(iCat)) then
CRLM               CatYear = 2001  ! also used for 2006
CRLM               exit
CRLM            end if
CRLM         end do
CRLM      end if
CRLM  O012_DataYearKeyword_WSP (END)

      
C --- if valid code but in conflict with a different year than specified,
C     in control file, this is an error.  print message and abort.
      if ((CatYear /= 0) .and. ((LCYear == 1992 .and. CatYear >= 2001)
     &   .or. (LCYear >= 2001 .and. CatYear == 1992))) then
         write(*,8025) trim(FName)
         write(LogUnt,8025) trim(FName)
         write(OutUnt,8025) trim(FName)
 8025    format(
     &    /," ERROR: One or more land cover codes were encountered",
     &    /," that are specific to a different version than the", 
     &    /," version specified in the control file.",
     &    /," Please verify the version of the NLCD file:",
     &    /,1x,a,
     &   //," Processing aborted.")
     
         stop
         
      else if (.not. LCValidFlg) then 
      
         if (.not. InvalidLCMsg) then
            write(*,8050)trim(FName)
            write(LogUnt,8050)trim(FName)
            write(OutUnt,8050)trim(FName)
 8050       format(
     &    /," WARNING: The NLCD file contains one or more land cover",
     &    /," code in conflict with the NLCD version specified.",
     &    /," These codes may represent missing data or data points",
     &    /," outside of the data boundaries of the file.",
     &    /," Please verify the version of the NLCD file:",
     &    /,1x,a,
     &   //," The land cover category will be set to missing.",/)
     
         end if  
         
         CellLC = MissCode
         InvalidLCMsg = .true.     
      
      end if
          
      return
      
      end subroutine CheckLC
      
C======================================================================= 
      SUBROUTINE GetGridVars(vars,grid,CType,XCoord,YCoord,UZone,
     &           CDatum,FName,DataType)
C     
C     Purpose:  Computes domain variables and derive geoTIFF grid 
C               variables needed to read the geoTIFF files.
C
C     NOTE: The term "cell" refers to single pixel in the NLCD file

C=======================================================================
      use ProcCtrlFile, only: RUNERR !added for confirmbounds error O027 RLM WSP 20240530

      implicit none
      
      type(tiffVars),    intent(in)        :: vars    ! tiff variables from land cover GeoTIFF
      type(tiffData),    intent(inout)     :: grid    ! land cover values
      character (len=*), intent(in)        :: CType   ! coordinate type (UTM or LATLON)
      double precision,  intent(in)        :: XCoord  ! x coordinate (UTM-E or Lon)
      double precision,  intent(in)        :: YCoord  ! y coordinate (UTM-N or Lat)
      integer (kind=4),  intent(in)        :: Uzone   ! UTM zone if CType is LATLON (ignored if lat/lon passed)
      character (len=*), intent(in)        :: CDatum  ! datum of coordinates (NAD27 or NAD83)
      character (len=*), intent(in)        :: FName   ! data file name  

      character (len=4), intent(in)        :: DataType ! type of data file
      
      ! domain variables      
      double precision :: EdgeAlbersX, EdgeAlbersY    ! edge of the study area on y-axis from center, Albers coords.
      double precision :: EdgeLat, EdgeLon            ! edge of studay area on y-axis from center, lat/lon  
      
      integer (kind=8) :: StudyDomAsCells     ! cell lengths of StudyDom (Bowen and albedo)

!     StudyRadiusAsCells: based on resolution, the number of cells to span the radius for Z0
!     - no longer needed, Z0 computed using effective radius based on IBL, keep in code for future use
!      integer (kind=8) :: StudyRadiusAsCells  ! study radius in cell lengths (Z0)

      double precision   :: CellX, CellY      ! Cell albers coordinates (in meters)
      double precision   :: CLat,CLon  ! DEBUG
      double precision   :: XPTIN, YPTIN, XPTOUT, YPTOUT  ! for NADCON
      double precision   :: DLOS, DLAS, DLOM, DLAM  ! for NADCON
      
      integer (kind=4)   :: Key     ! key for datum shift
      logical            :: NADShft ! Flag to indicate if NAD shift is necessary

      integer (kind=4)   :: DummyZone      ! Dummy Zone variable for UTMGEO call 
      
      ! Spheroid for call to UTMGEO for conversion of UTM to Lat/Lon
      ! Based on datum entered by user NAD27 or NAD83
      ! NAD27 => Clark 1866 = 0; NAD83 => GRS80 = 4
      integer (kind=4)   :: iSphere
             
      logical            :: OK2Proceed ! OK flag
      logical            :: NODATA     ! flag for existence of NAD Grid files


C --- Initialize logical flags     
      NODATA  = .false.    
      NADShft = .false.
      
C --- study domain and radius cell lengths
      StudyDomAsCells = IDNINT(StudyDomMeters/vars%CellRes)

!     StudyRadiusAsCells: based on resolution, the number of cells to span the radius for Z0
!     - no longer needed, Z0 computed using effective radius based on IBL, keep in code for future use
!      StudyRadiusAsCells  = IDNINT(StudyRadiusMeters/vars%CellRes)

C-----------------------------------------------------------------------
C     If the user entered UTM coordinates, convert to lat/lon
C-----------------------------------------------------------------------

      IF( CType == 'UTM' )THEN
         
         ! Determine spheroid for call to UTMGEO
         ! NAD27 = 0 (Clark 1866); NAD83 and WGS-84 = 4 (GRS-80)
         SELECT CASE(CDatum)
         CASE("NAD27")
            iSphere = 0
         CASE("NAD83")
            iSphere = 4
         END SELECT
        
         ! call UTMGEO to convert to lat/lon         
         CALL UTMGEO (333, UZone, DummyZone, XCoord, 
     &                YCoord, grid%CenterLon, grid%CenterLat, iSphere)

         ! The Center Latitude and Longitude are expressed in seconds on 
         ! return from UTMGEO; therefore the result needs to be divided by
         ! 3600. to obtain decimal degrees.  The longitude returned is 
         ! positive for North America; reverse to negative
         grid%CenterLat = grid%CenterLat/3600.0D0
         grid%CenterLon = grid%CenterLon/3600.0D0

      ELSE

         ! Determine spheroid for call to UTMGEO
         ! NAD27 = 0 (Clark 1866); NAD83 and WGS-84 = 4 (GRS-80)
         SELECT CASE(CDatum)
         CASE("NAD27")
            iSphere = 0
         CASE("NAD83")
            iSphere = 4
         END SELECT

         grid%CenterLat = YCoord
         grid%CenterLon = XCoord         
      END IF

C-----------------------------------------------------------------------
C     Determine if NAD shift is needed.  Shift is needed if datum of  
C     user's coordinates are inconsistent with the datum of the NLCD
C     (Imp & Can) file(s).  Accepted datum for GeoTIFF files is NAD27, 
C     NAD83, WGS72, WGS84. NAD83, WGS72, and WGS84 are treated identically.
C     User coordinates will be converted to match the datum of the NLCD file.
C     (7/2010 - with the addition of the impervious and canopy files,
C     the horizontal datum of the NLCD, impervious, and canopy files
C     must be identical)
C-----------------------------------------------------------------------

      ! Set the KEY parameter needed by NADCON which indicates the direction
      ! of the conversion. 
      ! KEY = +1 indicates a transformation of NAD 27 to NAD 83 datums
      ! KEY = -1 indicates a psuedo-transformation of NAD 83 to NAD 27 datums
      
      ! NLCD File datum
      SELECT CASE(vars%hdatum)
      ! NLCD File = NAD27
      CASE("NAD27")
         ! User Coords
         SELECT CASE(CDatum)
         ! User Coords = NAD83 => convert user coords to NAD27
         CASE("NAD83")
            NADShft = .true.
            Key = -1
         END SELECT
      
      !NLCD File = NAD83, WGS72, or WGS84
      CASE("NAD83","WGS72","WGS84")
         ! User Coords
         SELECT CASE(CDatum)
         ! User coords = NAD27 => convert user coords to NAD83
         CASE("NAD27")
            NADShft = .true.
            Key = +1
         END SELECT
      END SELECT
      
      ! Compute NAD shift and conversion
      SELECT CASE(NADShft)
      
      CASE(.true.)

         CALL NGRIDS (NODATA,.true.,0,NGPath)
            IF( NODATA )THEN
               WRITE(*,1250)
               WRITE(LogUnt,1250)
               WRITE(OutUnt,1250)
               STOP
            ENDIF
                    
 1250 FORMAT(/,' NAD GRID FILES MISSING (*.los & *.las)!',
     &       /,' AERSURFACE Processing Aborted!'/)
         
         ! Call NADCON 2.1 code to compute DATUM shifts and convert
         ! geographic coordinates between NAD27 and NAD83, and vice versa.
         
         ! NADCON(XPT,YPT,XPT2,YPT2,DLOS,DLAS,DLOM,DLAM,KEY)
         ! XPT, YPT: NAD27 coordinates input to NADCON when converting from NAD27 to NAD83
         !   However, when converting from NAD83 to NAD27, these coordinates are
         !   both input and output values.  Use to input NAD83 values and returned 
         !   NAD27 values converted from NAD83 (due to an interative guess/check 
         !   process in NADCON). Input as postive decimal degrees.
         ! XPT2, YPT2: NAD83 coordinates returned from NADCON when converting from NAD27
         !   to NAD83.  
         ! DLOS, DLAS: Lon/Lat shift in seconds (returned)
         ! DLOM, DLOS: Lon/Lat shift in meters (returned)
         ! KEY: +1 indicates a transformation of NAD 27 to NAD 83 datums
         !      -1 indicates a psuedo-transformation of NAD 83 to NAD 27 datums
c  ****************************************************************************************           
c           WARNING: Use temp variables for input and output coordinates.  NADCON 
c                    will reverse the signs of longitudinal values under certain
c                    conditions.
c  ****************************************************************************************         

         XPTIN =  grid%CenterLon
         YPTIN =  grid%CenterLat
                  
      
         SELECT CASE(Key)
         CASE(1)

            CALL NADCON(XPTIN,YPTIN,XPTOUT,YPTOUT,DLOS,DLAS,
     &                  DLOM,DLAM,KEY)
            ! set converted center coordinates
            grid%CenterLon =   XPTOUT
            grid%CenterLat =   YPTOUT
            
         CASE(-1)
            CALL NADCON(XPTIN,YPTIN,XPTOUT,YPTOUT,DLOS,DLAS,
     &                  DLOM,DLAM,KEY)
            ! set converted center coordinates
            grid%CenterLon =   XPTIN
            grid%CenterLat =   YPTIN
         END SELECT
         
      END SELECT

C     convert study center coordinates entered by user to albers coords.
      CALL LATLON2ALBERS (grid%CenterLat,grid%CenterLon,
     &            grid%CntrAlbersX,grid%CntrAlbersY)

C-----------------------------------------------------------------------
C     Compute the column and row of the center of the study area
C     for each 
C-----------------------------------------------------------------------
      grid%GridCntrCol = IDNINT((grid%CntrAlbersX - 
     &       DBLE(vars%tiffULX))/vars%CellRes)+1_8
      grid%GridCntrRow = IDNINT((DBLE(vars%tiffULY) - 
     &       grid%CntrAlbersY)/vars%CellRes)+1_8
      
      ! DEBUG TO GET LAT/LON FOR CENTER PIXEL 
      ! which is the lat/lon of the center of the pixel 
      ! containing the study site.
      CellX = dble(vars%tiffULX) + 
     &      (dble(grid%GridCntrCol-1_8)*vars%CellRes)
      CellY = dble(vars%tiffULY) - 
     &      (dble(grid%GridCntrRow-1_8)*vars%CellRes)
      CALL ALBERS2LATLON (CellX,CellY,CLat,CLon)
      
      ! END OF DEBUG
      
      WRITE(LogUnt, 1260) DataType, trim(FName),
     &                    grid%CenterLat, grid%CenterLon, 
     &                    grid%CntrAlbersX, grid%CntrAlbersY,
     &                    grid%GridCntrRow, grid%GridCntrCol
 1260 FORMAT(/,' ***************************',//
     &         ' Getting grid information for ',a4,': ',
     &         a,/
     &         ' Based on the following input coordinates,',
     &         ' converted if needed to LATLON/NAD83: '/
     &         '    Latitude:  ', F12.6,/
     &         '    Longitude: ', F12.6,/
     &       /,' The study center is located at',/
     &         '    Albers projection X-coordinate: ',F10.0,/
     &         '    Albers projection Y-coordinate: ', F10.0,/
     &         '    Row in data file:   ', I8,/
     &         '    Column in data file:', I8)
     


C-----------------------------------------------------------------------
C     Compute the number and first and last rows and columns in study area 
C       for bowen ratio and albedo together and separately for Zo.
C     Derive coordinates for lower left corner of applicable portion 
C       of grids.
C-----------------------------------------------------------------------
      

      grid%GridFrstRow = grid%GridCntrRow - (StudyDomAsCells)
      grid%GridLastRow = grid%GridCntrRow + (StudyDomAsCells)
      grid%GridFrstCol = grid%GridCntrCol - (StudyDomAsCells)
      grid%GridLastCol = grid%GridCntrCol + (StudyDomAsCells)

      grid%GridNRows = grid%GridLastRow-grid%GridFrstRow+1_8
      grid%GridNCols = grid%GridLastCol-grid%GridFrstCol+1_8
      
C --- Allocate grid arrays
C     Add one to the cols to have a place to store the row coord (Albers Y)
      ALLOCATE(grid%grid(1_8:grid%GridNRows,0_8:grid%GridNCols+1_8))

C --- Compute Albers lower left of domain
      grid%DomAlbersLLX = dble(vars%tiffULX) + 
     &              (dble(grid%GridFrstCol-1_8) *
     &              vars%CellRes) - (vars%CellRes/2.0D0)
      grid%DomAlbersLLY = dble(vars%tiffULY) - 
     &              (dble(grid%GridLastRow-1_8) *
     &              vars%CellRes) - (vars%CellRes/2.0D0)


C-----------------------------------------------------------------------
C     Compute the amount of rotation needed to orient the study grid 
C     from grid north to true north.  This value will be applied to 
C     the flow angle computed for each cell using Albers coordinates 
C     when determining the user defined sector each cell resides in.  
C
C     Get lat/lon at top edge of study area from the center.  Compute
C     difference from center lat to edge lat.  (center - edge)  This is
C     amount needed to rotate grid to true-north.  Rotation is clockwise
C     in the west, counter clockwise in the east, and 0 on the -96.0 W
C     meridian
C-----------------------------------------------------------------------

C     compute difference in decimal degrees from grid-north to true-north

C     first, derive coordinates at the top (grid-north) edge of the study area
      EdgeAlbersX = grid%CntrAlbersX
      EdgeAlbersY = grid%CntrAlbersY + StudyRadiusMeters

C     second, convert edge coordinates from Albers to lat/lon      
      CALL ALBERS2LATLON(EdgeAlbersX, EdgeAlbersY, 
     &                   EdgeLat, EdgeLon)

C     third, get Albers coordinates for due north of study center
      CALL LATLON2ALBERS (EdgeLat,grid%CenterLon, 
     &                    EdgeAlbersX,EdgeAlbersY)

C     finally, calculate GridToTruDif angle from delta-X and delta-Y
C     from study center to due North at edge of study area
      grid%GridToTruDif = DATAN2((EdgeAlbersX-grid%CntrAlbersX),
     &                      (EdgeAlbersY-grid%CntrAlbersY))
      grid%GridToTruDif = grid%GridToTruDif * Rad2Deg
      
      WRITE (LogUnt,600) grid%GridToTruDif
  600 FORMAT(/,' Rotation of Albers grid to true North (deg): '
     &, F10.5)
     
     
C-----------------------------------------------------------------------
C     Confirm that the area of interest is within the boundaries of
C     the NLCD data
C-----------------------------------------------------------------------

      CALL ConfirmBounds(StudyDomAsCells, grid%GridCntrCol, 
     &     grid%GridCntrRow, vars%tiffCols,  
     &     vars%tiffRows, OK2Proceed)


      IF( .NOT. OK2Proceed )THEN
         WRITE(*,700) trim(FName),
     &                grid%CntrAlbersX, grid%CntrAlbersY, 
     &                vars%tiffULX, vars%tiffULY,
     &                vars%tiffLRX, vars%tiffLRY
         WRITE(LogUnt,700) trim(FName),
     &                grid%CntrAlbersX, grid%CntrAlbersY, 
     &                vars%tiffULX, vars%tiffULY,
     &                vars%tiffLRX, vars%tiffLRY
         WRITE(OutUnt,700) trim(FName),
     &                grid%CntrAlbersX, grid%CntrAlbersY, 
     &                vars%tiffULX, vars%tiffULY,
     &                vars%tiffLRX, vars%tiffLRY
         CALL ERRHDL('CN','GetGridVars','E','600',DataType,ILINE)
         RUNERR = .TRUE. !errhdl sets FATAL which is only used in setup O027 RLM WSP 20240530
      ENDIF
     
  700 FORMAT( //
     &        ' ERROR: The study area extends beyond the data file: ',/
     &          6X, a,/
     &        ' Processing aborted!',//
     &        ' Study center coordinates (Albers X, Y): ',
     &         F10.0, ', ', F10.0/
     &        ' Upper left coordinates of data file (Albers X, Y):  ',
     &         I9, ', ', I9/
     &        ' Lower right coordinates of data file (Albers X, Y): ',
     &         I9, ', ', I9 )

     
      END SUBROUTINE GetGridVars

C=======================================================================      
      subroutine Read_TiffData(vars,grid,FName,FUnit,DType,LCYear)

C     Read data from Tiff file
C     Tiff file will be read differently depending on whether it is a
C     stripped tiff or tiled tiff file.
C     * Stripped tiff files - data is organized in strips that are 
C         rowsPerStrip long and the width (in columns) of the image
C     * Tiled tiff files - data is organized into rectangular tiles that
C         tileLen (rows) long and tileWid (cols) wide

C=======================================================================

      implicit none

      character (len=*), intent(in)   :: FName   ! data file name
      integer (kind=4),  intent(in)   :: FUnit   ! data file unit
      character (len=*), intent(in)   :: DType   ! data type LC, Imp, or Can
      integer (kind=4),  intent(in)   :: LCYear  ! land cover year (NLCD version) 1992, 2001, 2006, 2011
      type(tiffVars),    intent(in)   :: vars    ! tiff variables from land cover GeoTIFF
      type(tiffData),    intent(inout):: grid    ! land cover values     

      integer (kind=1)   :: TempLC(4)  ! 1-byte of data read from file
      integer (kind=2)   :: tmpInt2
      
      double precision   :: CellX, CellY      ! Cell albers coordinates (in meters)
      integer (kind=4)   :: CellLC            ! land cover category value

      integer (KIND=4)   :: idos         ! counters
      integer (KIND=8)   :: jrow, kcol   ! counters      
      
      integer (kind=8)   :: thisrec        ! current record 

      integer (kind=8)   :: tilesAcross    ! width of image in tiles
      integer (kind=8)   :: tilesDown      ! length (height) of image in tiles
      integer (kind=8)   :: tileNum        ! number of current tile read
      integer (kind=8)   :: tileCol        ! tile position (columns of tiles)
      integer (kind=8)   :: tileRow        ! tile position (rows of tiles)
      integer (kind=8)   :: tileFirstRow   ! first row in current tile
      integer (kind=8)   :: tileFirstCol   ! first col in current tile
      
      integer (kind=8)   :: tmprow, tmpcol ! current row and column      
     
     
C --- Land cover data - must exist
      OPEN( UNIT=FUnit, FILE=FName, FORM = 'UNFORMATTED', 
     &      STATUS='OLD', ACCESS='DIRECT', RECL=1, ERR=5000)

      WRITE(*,'(a/1x,a)') ' The input file opened: ', FName

      select case(vars%tiffType)
         
      ! bin files are processed as stripped tiff files with a single strip
      ! and an offset of 0 bytes
      case("strip")
           
         ! initialize tmprow = 0
         tmprow = 0_8

         ! loop over offsets array
         SOffsets: DO idos=1_8, size(vars%dataOS)
         
            SRowLoop: DO jrow = 1_8,vars%rowsPerStrip

               ! increment row counter to keep track of current data row
               tmprow = tmprow+1_8
            
               ! initialize column to 0 for new row
               tmpcol = 0_8
               
               ! initialize column counts for bowen/albedo and roughness grids
               grid%GridColCnt = 0_8

               ! cycle loop if first row has not been reached yet
               if (tmprow < grid%GridFrstRow) then
                  cycle
               elseif (tmprow > grid%GridLastRow) then
                  exit
               end if
            
C ---          Compute cell Y coord - albers            
               CellY = dble(vars%tiffULY) - 
     &                (dble(tmprow-1_8)*vars%CellRes)

C ---          Increment counter and store values in applicable 
C              arrays for later output if within grid boundaries
               IF (tmprow >= grid%GridFrstRow .and. 
     &             tmprow <= grid%GridLastRow) THEN
                  grid%GridRowCnt = tmprow-grid%GridFrstRow+1_8
                  grid%grid(grid%GridRowCnt,0_8) = 
     &               IDNINT(CellY)
               END IF
                  
               SColLoop: DO kcol = 1_8,vars%tiffCols

                  ! increment column counter to keep track of current data column
                  tmpcol = tmpcol+1_8
            
                  if (tmpcol < grid%GridFrstCol .or. 
     &                tmpcol > grid%GridLastCol) then
                     cycle
                  end if
               
C ---             Compute cell x coord - albers
                  CellX = dble(vars%tiffULX) + 
     &                   (dble(tmpcol-1_8)*vars%CellRes)


C ---             Compute record number (implied 1 byte per record)
c                  = offset/bytesPerRecord + (no. samples from beginning of strip)*bytesPerSample/bytesPerRecord
                  thisrec = vars%dataOS(idos) + 
     &                   ((((jrow-1_8)*vars%tiffCols) + tmpcol) * 
     &                      vars%bytesPerSample)

C ---             initialize cell value to temporary missing code 
C                 (a value that shouldn't exist)
                  CellLC = -99

C ---             read land cover value
                  select case(vars%bytesPerSample)
                  case(1)
                     ! nlcd
                     READ(FUnit, rec=thisrec, ERR=5010) TempLC(1)
                     CellLC = INT(TempLC(1),kind=4)
                     
                  case(2)
                     ! nlcd
                     READ(FUnit, rec=thisrec, ERR=5010) TempLC(1)
                     READ(FUnit, rec=thisrec+1_8, ERR=5010) TempLC(2)

                     if (vars%byteswap) then 
                        tmpInt2 = Transfer((/TempLC(2),TempLC(1)/),
     &                                       tmpInt2)
                     else
                        tmpInt2 = Transfer((/TempLC(1),TempLC(2)/),
     &                                       tmpInt2)
                     end if
                     
                     CellLC = INT(tmpInt2,kind=4)
                     
                  case(4)
                     ! nlcd
                     READ(FUnit, rec=thisrec, ERR=5010) TempLC(1)
                     READ(FUnit, rec=thisrec+1_8, ERR=5010) TempLC(2)
                     READ(FUnit, rec=thisrec+2_8, ERR=5010) TempLC(3)
                     READ(FUnit, rec=thisrec+3_8, ERR=5010) TempLC(4)

                     if (vars%byteswap) then 
                        CellLC = Transfer((/TempLC(4),TempLC(3),
     &                                  TempLC(2),TempLC(1)/),CellLC)
                     else
                        CellLC = Transfer((/TempLC(1),TempLC(2),
     &                                  TempLC(3),TempLC(4)/),CellLC)
                     end if
                                         
                  end select

C ---             Determine if the LC value retrieved is valid and set
C                 NLCD year type flag if can be determined    
                  if (DType == 'LC') call CheckLC(CellLC,MissCode,
     &                                               LCYear,FName)

C ---             Increment counter and store values in applicable
C                 arrays for later output if within grid boundaries
                  IF (tmprow >= grid%GridFrstRow .and. 
     &                tmprow <= grid%GridLastRow .and.
     &                tmpcol >= grid%GridFrstCol .and. 
     &                tmpcol <= grid%GridLastCol) THEN
                     grid%GridColCnt = tmpcol-grid%GridFrstCol+1_8
                     grid%grid(grid%GridRowCnt,grid%GridColCnt) = 
     &                    CellLC
                  END IF

               END DO SColLoop

            END DO SRowLoop
 
         END DO SOffsets 
                   
      case("tile")
             
         ! initialize tmprow = 0, tmpcol = 0, tileNum = 0
         tmprow = 0_8
         tmpcol = 0_8
         tileNum = 0_8
                 
         ! compute number of tiles across and down
         tilesAcross = (vars%tiffCols+vars%tileWid-1_8)/
     &                  vars%tileWid
         tilesDown = (vars%tiffRows+vars%tileLen-1_8)/
     &                  vars%tileLen

         ! loop over offsets array
         TOffsets: DO idos=1_8, size(vars%dataOS)         

            ! compute number of current tile, tile row, tile col
            ! first row in tile, first col in tile
            tileNum = tileNum+1_8
            tileRow = int((tileNum-1_8)/tilesAcross)+1_8
            tileCol = mod(tileNum-1_8,tilesAcross)+1_8
            tileFirstRow = ((tileRow-1_8)*vars%tileLen)+1_8
            tileFirstCol = ((tileCol-1_8)*vars%tileWid)+1_8
               
            ! compute current data row, decrement by 1 since incremented at 
            ! beginning of loop below
            tmprow = tileFirstRow-1_8
               
            ! loop over rows in tile
            TRowLoop: DO jrow=1_8, vars%tileLen
            
               grid%GridColCnt = 0_8
            
               ! increment row and compute column (decrement by 1 since incremented
               ! at beginning of loop below)
               tmprow = tmprow+1_8
               tmpcol = tileFirstCol-1_8

               ! cycle loop if current row precedes first row to process
               if (tmprow < grid%GridFrstRow) then
                  cycle
!               elseif (tmprow > grid%GridLastRow .and. 
!     &                 (LC1992 .or. LC2001)) then 
               ! exit loop if current row exceeds last row to process, 
               ! unless NLCD Type not determined
               ! 9/2010 - NLCD version is not longer derived from data
               elseif (tmprow > grid%GridLastRow) then 
                  exit
               end if

C ---          Compute cell Y coord - albers            
               CellY = dble(vars%tiffULY) - 
     &                (dble(tmprow-1_8)*vars%CellRes)

C ---          Increment counter and store values in applicable BaGrid and ZoGrid 
C              arrays for later output if within grid boundaries
               IF (tmprow >= grid%GridFrstRow .and. 
     &             tmprow <= grid%GridLastRow) THEN
                  grid%GridRowCnt = tmprow-grid%GridFrstRow+1_8
                  grid%grid(grid%GridRowCnt,0_8) = IDNINT(CellY)
               END IF
                  
               ! loop over columns in tile
               TColLoop: DO kcol=1_8, vars%tileWid
               
                  ! increment column
                  tmpcol = tmpcol+1_8

C ---             Compute record number (implied 1 byte per record)
c                  = offset/bytesPerRecord + (no. samples from beginning of strip)*bytesPerSample/bytesPerRecord + 1
                  thisrec =  vars%dataOS(idos) + 
     &                   ((((tmprow-tileFirstRow)*vars%tileWid) + 
     &                      (tmpcol-tileFirstCol))*
     &                       vars%bytesPerSample) + 1_8

                  if (tmpcol < grid%GridFrstCol .or. 
     &                tmpcol > grid%GridLastCol) then
                     cycle
                  end if
               
C ---             Compute cell x coord - albers
                  CellX = dble(vars%tiffULX) + 
     &                   (dble(tmpcol-1_8)*vars%CellRes)
                     
C ---             initialize cell value to missing
                  CellLC = -99

C ---             read land cover value
                  select case(vars%bytesPerSample)
                  case(1)
                     ! nlcd
                     READ(FUnit, rec=thisrec, ERR=5010) TempLC(1)
                     CellLC = INT(TempLC(1),kind=4)

                  case(2)
                     ! nlcd
                     READ(FUnit, rec=thisrec, ERR=5010) TempLC(1)
                     READ(FUnit, rec=thisrec+1_8, ERR=5010) TempLC(2)

                     if (vars%byteswap) then 
                        tmpInt2 = Transfer((/TempLC(2),TempLC(1)/),
     &                                       tmpInt2)
                     else
                        tmpInt2 = Transfer((/TempLC(1),TempLC(2)/),
     &                                       tmpInt2)
                     end if
                     
                     CellLC = INT(tmpInt2,kind=4)                     
                     
                  case(4)
                     ! nlcd
                     READ(FUnit, rec=thisrec, ERR=5010) TempLC(1)
                     READ(FUnit, rec=thisrec+1_8, ERR=5010) TempLC(2)
                     READ(FUnit, rec=thisrec+2_8, ERR=5010) TempLC(3)
                     READ(FUnit, rec=thisrec+3_8, ERR=5010) TempLC(4)

                     if (vars%byteswap) then 
                        CellLC = Transfer((/TempLC(4),TempLC(3),
     &                                  TempLC(2),TempLC(1)/),CellLC)
                     else
                        CellLC = Transfer((/TempLC(1),TempLC(2),
     &                                  TempLC(3),TempLC(4)/),CellLC)
                     end if
                                         
                  end select

C ---             Determine if the LC value retrieved is valid                 
                  IF (DType == 'LC') call CheckLC(CellLC,MissCode,
     &                                               LCYear,FName)
                  
C ---             Any negative LC values are invalid.  
c                 Map to zero(0).
                  IF( CellLC < 0 ) CellLC = 0

C ---             Increment counter and store values in applicable BaGrid and ZoGrid 
C                 arrays for later output if within grid boundaries
                  IF (tmprow >= grid%GridFrstRow .and. 
     &                tmprow <= grid%GridLastRow .and.
     &                tmpcol >= grid%GridFrstCol .and. 
     &                tmpcol <= grid%GridLastCol) THEN
                     grid%GridColCnt = tmpcol-grid%GridFrstCol+1_8
                     grid%grid(grid%GridRowCnt,grid%GridColCnt) = 
     &                       CellLC
                  END IF


                END DO TColLoop ! tileWid, cols
            END DO TRowLoop ! tileLen, rows
         END DO TOffsets ! tile offsets         
      
      END SELECT
      
      CLOSE(FUnit)
      
      IF (DType == 'LC') THEN
C ---    Set ring width and number of rings based on pixel scale 
C        for effective roughness method for Zo (ZoEFF)
         RingWidth = vars%pxlScalex
         NumRings = ceiling(StudyRadiusMeters/RingWidth)
         
C ---    Allocate arrays for effective roughness method for Zo (ZoEFF)
         ALLOCATE(ZoSumEff(MaxSectors,12,NumRings))  ! 12 = months
         ALLOCATE(InvDSumZoEff(MaxSectors,12,NumRings)) ! 12 = months
         ZoSumEff(:,:,:) = 0.0D0
         InvDSumZoEff(:,:,:) = 0.0D0
      END IF
      
      
      Return
      
 5000 WRITE(*,5005) trim(FName)
      WRITE(LogUnt,5005) trim(FName)
      WRITE(OutUnt,5005) trim(FName)
 5005 FORMAT(/,' There was a problem opening the NLCD file:'
     &       /,a,//,' Processing was aborted.',/)
      STOP
      
 5010 WRITE(*,5015) trim(FName)
      WRITE(LogUnt,5015) trim(FName)
      WRITE(OutUnt,5015) trim(FName)
 5015 FORMAT(/,' There was a problem reading the TIFF file:'
     &       /,a,//,' Processing was aborted.',/)
      STOP
      
      end subroutine Read_TiffData
      
C=======================================================================      
      SUBROUTINE ProcessLC(vars,grid,LCYear,CellX,CellY,
     &                     CellLC,CellImp,CellCan)
      
C --- Process land cover value extracted from NLCD file.  
C     Compute distance from center; determine sector when applicable;
C     accumulate counts and sums needed to compute averages.

C=======================================================================

      implicit none

      ! input variables      
      DOUBLE PRECISION,  intent(in) :: CellX, CellY     ! albers coords of current cell
C --- use double precision integers for CellLC, CellImp & CellCan
      INTEGER (KIND=8),  intent(in) :: CellLC           ! land cover value from cell
      INTEGER (KIND=8),  intent(in) :: CellImp          ! impervious value from cell
      INTEGER (KIND=8),  intent(in) :: CellCan          ! canopy value from cell
      integer (kind=4),  intent(in) :: LCYear           ! land cover year (NLCD version) 1992, 2001, 2006
      type(tiffVars),    intent(in) :: vars             ! tiff variables from land cover GeoTIFF
      type(tiffData),    intent(in) :: grid             ! land cover values 
      
      ! double precision to store converted impervious and canopy percentages
      DOUBLE PRECISION :: CellImpR, CellCanR
      
      ! double precision to store converted temporary impervious and canopy percentages
      ! to address cases where CellImpR + CellCanR > 1
      DOUBLE PRECISION :: CellImpTmp, CellCanTmp
      
      ! logical to indicate impervious and canopy are valid and not missing
      LOGICAL          :: ImpCanVldFlg
      
      ! temp value for Zo based on land cover, seaon/month, 
      ! and use of impervious and canopy 
      DOUBLE PRECISION :: Zotmp
      
      ! used to compute angle of cell to center for sector 
      ! Lat and Lon difference between the center of the study
      ! area and the cell being processed
      DOUBLE PRECISION :: Xangle, Yangle
      
      ! Mathematical angle of the study center to the center
      ! of the cell of interest
      DOUBLE PRECISION :: CellAngle
      
      ! Flow vector angle of the study center to the center
      ! of the cell of interest (i.e., the head of the wind arrow)
      DOUBLE PRECISION :: FlowAngle  
      
      DOUBLE PRECISION :: Distance  ! distance to study center
      
      INTEGER (kind=4) :: DistIndex  ! index for distance range
      INTEGER (kind=4) :: WindSector ! windsector reference
      
      INTEGER (kind=4) :: kdum, mnum  ! loop counters
      DOUBLE PRECISION :: tmpStart, tmpEnd ! temp sector start/end directions

      ! initialize flag to indicate impervious and canopy data are both valid and not missing
      ImpCanVldFlg = .TRUE.

      ! initialize tmp impervious and canopy normalization variables
      CellImpTmp = 0.0D0
      CellCanTmp = 0.0D0
      
CCRT- convert canopy and impervious integers to decimal values (0.0-1.0)
      CellCanR = DBLE(CellCan)/100.0D0
      CellImpR = DBLE(CellImp)/100.0D0
      
      ! if canopy or impervious value is outside 0-100, set valid flag to false
      IF (CellImp < 0_8 .OR. CellImp > 100_8) ImpCanVldFlg = .FALSE.
      IF (CellCan < 0_8 .OR. CellCan > 100_8) ImpCanVldFlg = .FALSE.

      ! Compute distance from center of current cell to study center 
      Distance =  DSQRT(DABS(grid%CntrAlbersX-CellX)**2 +
     &                 (DABS(grid%CntrAlbersY-CellY)**2))
     
      
C --- compute minimum distance limit from pixel center to study center 
C     to use when computing inverse distance weighted average for Zo
      MinD = vars%pxlScalex*MinDFactr

C --- Any negative values are invalid.  Map to zero(0) if not caught prior.
c      IF( CellLC < 0 ) CellLC = 0


C --- Derive cell sector based on user's sector definitions
      
      ! User did not define sectors, 1 sector by default
      IF (NumSectors == 1) THEN

          WindSector = 1

      ! User defined sectors
      ELSE

         Xangle = CellX - grid%CntrAlbersX
         Yangle = CellY - grid%CntrAlbersY

         CellAngle = DATAN2(Yangle, Xangle)
         CellAngle = CellAngle * Rad2Deg
         
         CellAngle = CellAngle + grid%GridToTruDif
         
         IF( CellAngle < 0.0D0 )THEN
            CellAngle = CellAngle + 360.0D0
         ENDIF
         
         ! compute flow angle
         FlowAngle = 90.0D0 - CellAngle

         IF( FlowAngle < 0.0D0 )THEN
            FlowAngle = FlowAngle + 360.0D0
         ENDIF
         
         WindSector = 0
         
         SectorLoop: DO kdum =1,NumSectors
            tmpStart = dble(StartDir(kdum))
            IF (kdum == NumSectors) THEN
               tmpEnd = dble(StartDir(1))
            ELSE
               tmpEnd = dble(StartDir(kdum+1))
            END IF
            IF (tmpStart > tmpEnd) THEN
               IF( FlowAngle >= tmpStart .AND.
     &             FlowAngle <= 360.0D0 .or.
     &             FlowAngle >= 0.0D0 .AND.
     &             FlowAngle < tmpEnd)Then
                   WindSector = kdum
                   EXIT SectorLoop
               END IF
            ELSE
               IF( FlowAngle >= tmpStart .AND.
     &            FlowAngle < tmpEnd )Then
                   WindSector = kdum
                   EXIT SectorLoop
               END IF
            END IF
         ENDDO SectorLoop    ! SectorLoop    
         
      ENDIF                  ! # Sectors = 1
      
      ! Call consolsfcvals to consolidate sfcvals for current pixel
      ! if VaryAp is true - need a cleaner way to do this just for airport sectors
      ! since arid conditions are site wide
      IF (VaryAP) THEN
         CALL ConsolSfcVals(LCYear,WndSectors%sec_ap(WindSector),
     &                         Moisture,Arid)
      END IF

C --- keep a count of cells by land use category (ignore sector)
C     used for bowen ratio and albedo
     
      LandCoverBr(1,CellLC) = LandCoverBr(1,CellLC) + 1
      
      LandCoverAlb(1,CellLC) = LandCoverAlb(1,CellLC) + 1
      
C --- keep a total count of cells for bowen ratio and albedo (ignore sector)
      LandCovTotBr(1) = LandCovTotBr(1) + 1
      
      LandCovTotAlb(1) = LandCovTotAlb(1) + 1
      
      
C --- keep a total count of cells for bowen and albedo excluding the 
C     missing and out of bounds (ignore sector)
      IF( CellLC /= MissCode )THEN
        LCCountBr(1) = LCCountBr(1)+ 1  
        LCCountAlb(1) = LCCountAlb(1)+ 1
      END IF


C --- Keep a count of categories for surface roughness.
C     Use ZoRad value to define the domain land cover 
C     category counts for the table in the log file.
      IF (Distance <= ZoRad )THEN
     
         LandCoverZ0(WindSector,CellLC) = 
     &   LandCoverZ0(WindSector,CellLC) + 1    

         LandCovTotZ0(WindSector) = 
     &   LandCovTotZ0(WindSector) + 1 
     
C ---    keep a total count of cells by sector excluding the 
C        missing and out of bounds
         IF( CellLC /= MissCode )THEN                    
            LCCountZ0(WindSector) =
     &      LCCountZ0(WindSector) + 1
         END IF
         
         IF (UseCan .or. UseImp) THEN
         
CCRT-       Keep a total count of cells independent of sector
C           that are within 1km of the study center for check
C           sum of canopy + impervious percentages
            IF (Distance <= ZoRad) THEN
               CanImpZoCnt = CanImpZoCnt + 1
            END IF

CCRT-       Keep a total count of cells in which the
C           impervious + canopy decimal percent is > CanImpZoSumThrsh
            IF (ImpCanVldFlg .and. 
     &         (CellImpR + CellCanR > CanImpZoSumThrsh)) THEN
               CanImpZoNvldCnt = CanImpZoNvldCnt + 1
            END IF

         END IF
         
      END IF   
      
C --- for each month compute the geometric sums needed for average
C     bowen and albedo; ignore sector

      MnthLoop: DO mnum = 1, 12

         IF (CellLC /= MissCode) THEN

            BowenSum(1,mnum) = 
     &      BowenSum(1,mnum) +
     &        LOG(Bowen(CellLC,SeasonMnths(mnum)))

C           for Zo, limit the distance used for weighted average 
C           to MinD which is some fraction of the pixel resolution  
            IF (Zo_Method == 'ZOEFF') ZoRad = StudyRadiusMeters

            IF (Distance <= ZoRad) THEN

               DistIndex = NINT(Distance/RingWidth) + 1

               ! if impervious and/or canopy used, compute Zo from table values
               ! else, use table values directly
               IF (LCYear > 1992 .AND. (UseImp .OR. UseCan) .AND. 
     &             ImpCanVldFlg) THEN
               
C ---             Check for Percent_Canopy + Percent_Imperv > 1.0
                  IF (CellCanR + CellImpR > 1.00) THEN
C                    Normalize Canopy and Imperv fractions
                     CellCanTmp = CellCanR/(CellCanR+CellImpR)
                     CellImpTmp = CellImpR/(CellCanR+CellImpR)
                     CellCanR = CellCanTmp
                     CellImpR = CellImpTmp
                  ENDIF
               
                  ! 2001 Developed (21, 22, 23, 24)
                  IF (CellLC >= 21 .AND. CellLC <= 24) THEN
CCRT                 Remove canopy threshold and treat as developed category
CCRT                     IF (CellCanR >= 0.25) THEN
CCRT                     ! Assume mixed forest (cat 43) if canopy cover .ge. 0.25
CCRT                       Zotmp = Z092_43(SeasonMnths(mnum))
CCRT                     ELSE
                       ! airport
                       IF (WndSectors%sec_ap(WindSector)) THEN
                        ! Use weighted geometric mean of forest, buildings, bare rock and grass                      
                        Zotmp = EXP(
     &                  (LOG(Z092_43(SeasonMnths(mnum)))*CellCanR) +
     &                (0.10*LOG(Z092_22(SeasonMnths(mnum)))*CellImpR) +
     &                (0.90*LOG(Z092_31(SeasonMnths(mnum)))*CellImpR) +
     &                  (LOG(Z092_85(SeasonMnths(mnum)))*
     &                    (1.0-CellCanR-CellImpR)) )
                       ! non-airport
                       ELSE
                        ! Use weighted geometric mean of forest, buildings, bare rock and grass                         
                        Zotmp = EXP(
     &                  (LOG(Z092_43(SeasonMnths(mnum)))*CellCanR) +
     &                (0.90*LOG(Z092_22(SeasonMnths(mnum)))*CellImpR) +
     &                (0.10*LOG(Z092_31(SeasonMnths(mnum)))*CellImpR) +
     &                  (LOG(Z092_85(SeasonMnths(mnum)))*
     &                    (1.0-CellCanR-CellImpR)) )
                       ENDIF
CCRT                     ENDIF
                     
                  ! 2001 Forests (41, 42, 43)
                  ELSEIF (CellLC >= 41 .AND. CellLC <= 43) THEN
                      Zotmp = Z0(CellLC,SeasonMnths(mnum))

                  ! 2001 Woody Wetlands (90 - 94)
                  ELSEIF (CellLC >= 90 .AND. CellLC <= 94) THEN
                     IF (CellCanR .GE. 0.25) THEN
                      ! Use woody wetland value if canopy cover .ge. 0.25
                      Zotmp = Z0(CellLC,SeasonMnths(mnum))
                     ELSE
                      ! Use weighted geometric mean of woody wetland, buildings, bare rock and grass                      
                      Zotmp = EXP(
     &                (LOG(Z0(CellLC,SeasonMnths(mnum)))*CellCanR) +
     &                (LOG(Z092_31(SeasonMnths(mnum)))*CellImpR) +
     &                (LOG(Z092_85(SeasonMnths(mnum)))*
     &                  (1.0-CellCanR-CellImpR)) )
                     ENDIF
                     
                  ! Other
                  ELSE
                     Zotmp = Z0(CellLC,SeasonMnths(mnum))
                  ENDIF

               ! if canopy and impervious data omitted
               ELSE
                  Zotmp = Z0(CellLC,SeasonMnths(mnum))
               ENDIF
               
               SELECT CASE (Zo_Method)
                  CASE ('ZOEFF')

                     IF (Distance < MinD) THEN
                        InvDSumZoEff(WindSector,mnum,DistIndex) = 
     &                  InvDSumZoEff(WindSector,mnum,DistIndex) +
     &                 (1.0D0/(MinD)**InvDPwr)
                     
                        ZoSumEff(WindSector,mnum,DistIndex) = 
     &                  ZoSumEff(WindSector,mnum,DistIndex) +
     &                 ((1.0D0/(MinD)**InvDPwr)* LOG(Zotmp))
                     ELSE
                        InvDSumZoEff(WindSector,mnum,DistIndex) = 
     &                  InvDSumZoEff(WindSector,mnum,DistIndex) +
     &                  (1.0D0/(Distance)**InvDPwr)
                     
                        ZoSumEff(WindSector,mnum,DistIndex) = 
     &                  ZoSumEff(WindSector,mnum,DistIndex) +
     &                 ((1.0D0/(Distance)**InvDPwr)* LOG(Zotmp))
                     
                     END IF
               
                  CASE ('ZORAD')

                     IF (Distance < MinD) THEN
                        InvDSumZoRad(WindSector,mnum) = 
     &                   InvDSumZoRad(WindSector,mnum)+
     &                   (1.0/MinD**InvDPwr)
                     
                        ZoSumRad(WindSector,mnum) = 
     &                    ZoSumRad(WindSector,mnum)+
     &                 ((1.0/MinD**InvDPwr)*
     &                  LOG(Zotmp))
                     ELSE
                        InvDSumZoRad(WindSector,mnum) = 
     &                   InvDSumZoRad(WindSector,mnum)+
     &                   (1.0/Distance**InvDPwr)
                     
                        ZoSumRad(WindSector,mnum) = 
     &                    ZoSumRad(WindSector,mnum)+
     &                 ((1.0/Distance**InvDPwr)*
     &                  LOG(Zotmp))
                     END IF
               END SELECT
               
            ENDIF 
            
         ENDIF
               
      END DO MnthLoop

      RETURN
               
      END SUBROUTINE ProcessLC
      
C=======================================================================
      SUBROUTINE Write_DbgGrid(vars,grid,DType,FUnit,FName,TName)
         
C     Write_DbgGrid
C        Output debug grid of values from GeoTIFF file (land cover, 
C        impervious, or canopy).

C=======================================================================
      
      use StartVars, only: run_start, iYr2

c      USE UserParams, only: ZoRad

      use Constants, only:  Version

      implicit none

     
      type(tiffVars),     intent(in)   :: vars       ! geotiff variables 
      type(tiffData),     intent(in)   :: grid       ! geotiff values
      character (len=*),  intent(in)   :: FName      ! grid output file name
      integer (kind=4),   intent(in)   :: FUnit      ! grid output file unit
      character (len=*),  intent(in)   :: TName      ! GeoTIFF file name
      character (len=*),  intent(in)   :: DType      ! data type LC, Imp, or Can
      
      integer (kind=8)                 :: icol, jrow ! loop counters
      character (len=10)               :: tempStr    ! temporary string
      

C     Eval data type based on value passed - to display in output file
      SELECT CASE(DType)
         CASE("LC")
            tempStr = "Land Cover"
         CASE("Imp")
            tempStr = "Impervious"
         CASE("Can")
            tempStr = "Canopy"
      END SELECT
      
      OPEN(UNIT=FUnit,FILE=FName,STATUS='UNKNOWN',ERR=5050)   

      write(FUnit,45) 'AERSURFACE Grid Debug File, Version ',
     &    trim(Version),run_start(2),run_start(3),iYr2,
     &    run_start(5),run_start(6),run_start(7)
     
   45 FORMAT('** ',2a,t67, i2.2,'/',i2.2,'/',i2.2,' **'/
     &       '**',t67,i2.2,':',i2.2,':',i2.2,' **'/ )

      
      WRITE(FUnit,'(4a/a,f12.6,", ",f12.6," (NAD83)",/)',ERR=5060)
     &      '10km x 10km ',tempStr,' Data Grid Extracted from: ',
     &      trim(TName),
     &      'Centered On: ',
     &      grid%CenterLat,grid%CenterLon 
      WRITE(FUnit,'(a,t32,i10)',ERR=5060) 'ncols',grid%GridNCols
      WRITE(FUnit,'(a,t32,i10)',ERR=5060) 'nrows',grid%GridNRows
      WRITE(FUnit,'(a,t33,f11.1)',ERR=5060) 'xllcorner',
     &                                     grid%DomAlbersLLX
      WRITE(FUnit,'(a,t33,f11.1)',ERR=5060) 'yllcorner',
     &                                     grid%DomAlbersLLY
      WRITE(FUnit,'(a,t34,f12.3)',ERR=5060) 'cellsize',
     &      vars%CellRes
      WRITE(FUnit,'(a,t41,a)',ERR=5060) 'NODATA_value','0'
      
C --- Grid
      DO jrow = 1_8,grid%GridNRows
         WRITE(FUnit,1300) 
     &     (grid%grid(jrow,icol),icol=0_8,grid%GridNCols)
      ENDDO
 1300 FORMAT(i15,'. : ',500(i3,1x))
 
      CLOSE(FUnit)

      RETURN       
       
 5050 WRITE(*,5055) trim(FName)
      WRITE(LogUnt,5055) trim(FName)
      WRITE(OutUnt,5055) trim(FName)
 5055 FORMAT(
     &  /,' There was a problem opening the grid debug file:'
     &  /,a,//,' Processing will continue.',/) 
      RETURN  

 5060 WRITE(*,5065) trim(FName)
      WRITE(LogUnt,5065) trim(FName)
      WRITE(OutUnt,5065) trim(FName)
 5065 FORMAT(
     &  /,' There was a problem writing to the grid debug file:'
     &  /,a,//,' Processing will continue.',/) 
      CLOSE(FUnit)
      RETURN 
   
      END SUBROUTINE Write_DbgGrid
      
C=======================================================================      
      SUBROUTINE Write_LogGrid(vars,grid,DType,FUnit,TName)

C     Write_LogGrid
C        Output grid of values from GeoTIFF file (land cover, 
C        impervious, or canopy) to log file.

C=======================================================================
     
      use StartVars, only: run_start, iYr2

c      USE UserParams, only: Anem_Height, ZoRad

      use Constants, only:  Version

      integer (kind=4),   intent(in)   :: FUnit      ! grid output file unit

      type(tiffVars),     intent(in)   :: vars    ! geotiff variables 
      type(tiffData),     intent(in)   :: grid    ! geotiff values
      character (len=*),  intent(in)   :: DType   ! data type LC, Imp, or Can
      character (len=*),  intent(in)   :: TName   ! GeoTIFF file name
      
      integer (kind=8)                 :: icol, jrow ! loop counters
      character (len=10)               :: tmpStr     ! temporary string
      
      
      SELECT CASE(DType)
         CASE("LC")
            tmpStr = "Land Cover"
         CASE("Imp")
            tmpStr = "Impervious"
         CASE("Can")
            tmpStr = "Canopy"
      END SELECT

      write(FUnit,45) Version,
     &    run_start(2),run_start(3),iYr2, run_start(5),
     &    run_start(6),run_start(7)
   45 format('** AERSURFACE GRID Debug File, Version ',a10,t67,'** ',
     &        i2.2,'/',i2.2,'/',i2.2,/'** ',t67,'** ',
     &        i2.2,':',i2.2,':',i2.2/)

      WRITE(FUnit,'(/a//4a/a,f12.6,", ",f12.6," (NAD83)",/)',
     &                                                         ERR=6010)
     &      ' ***************************',
     &      '10km x 10km ',tmpStr,' Data Grid Extracted from: ',
     &      trim(TName),
     &      'Centered On: ',
     &      grid%CenterLat,grid%CenterLon 

C --- Add parameters from Unit 40 output
      WRITE(FUnit,'(a,t32,i10)',ERR=5060) 'ncols',grid%GridNCols
      WRITE(FUnit,'(a,t32,i10)',ERR=5060) 'nrows',grid%GridNRows
      WRITE(FUnit,'(a,t33,f11.1)',ERR=5060) 'xllcorner',
     &                                     grid%DomAlbersLLX
      WRITE(FUnit,'(a,t33,f11.1)',ERR=5060) 'yllcorner',
     &                                     grid%DomAlbersLLY
      WRITE(FUnit,'(a,t34,f12.3)',ERR=5060) 'cellsize',
     &      vars%CellRes
      WRITE(FUnit,'(a,t41,a)',ERR=5060) 'NODATA_value','0'

C --- Grid
      DO jrow = 1_8,grid%GridNRows
         WRITE(FUnit,1300) 
     &     (grid%grid(jrow,icol),icol=0_8,grid%GridNCols)
      ENDDO
 1300 FORMAT(i15,'. : ',500(i3,1x))
 
      RETURN
      
 6010 WRITE(*,6015) trim(LogFile)
      WRITE(FUnit,6015) trim(LogFile)
      WRITE(OutUnt,6015) trim(LogFile)
 6015 FORMAT(
     &  /,' There was a problem writing to the log file:'
     &  /,a,//,' Processing was aborted.')

 5060 CONTINUE

      STOP
   
      END SUBROUTINE Write_LogGrid
      
      end module
      