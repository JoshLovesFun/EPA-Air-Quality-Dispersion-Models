C=======================================================================
      MODULE UserParams
      
C     Variables to store user input specified in the aersurface control
C     file (aersurface.inp) and others derived from user input.

C     Uses the following modules:
C     - Constants

C     Contains the following procedures: none

C=======================================================================

C     Declaration of user input variables and those derived from 
C     user inputs

C     StatnOrdr      USER INPUT: Met station order (PRIMARY or SECONDARY)
C     Zo_Method       USER INPUT: Method for computing roughness (ZORAD or ZOEFF)

C     LCFile:        USER INPUT: land use data file
C     ImpFile:       USER INPUT: impervious file (2001 NLCD supplement)
C     CanFile:       USER INPUT: canopy file (2001 NLCD supplement)

C     CoordType:     USER INPUT: Coordinate type (UTM or LATLON)
C     Datum:         USER INPUT: Dataum reference (NAD27 or NAD83)

C     CenterUTME:     USER INPUT: UTM Easting, in meters, of center of primary study
C     CenterUTMN:     USER INPUT: UTM Northing, in meters, of center of primary study
C     CenterUTMZone:  USER INPUT: UTM Zone of center of primary study
C     CenterLatIn:    USER INPUT: latitude, in decimal degrees, of center of primary study
C     CenterLonIn:    USER INPUT: longitude, in decimal degrees, of center of primary study

C     Anem_Height:   USER_INPUT: Anemometer height

C     IBL_Factor:    USER_INPUT: Internal boundary layer factor applied to Anem_Height

C     RingWidth:     Pixel scale used in calculating roughness using ZOEFF

C     VaryAP:        USER_INPUT: vary airport association by roughness sector

C     AirportChr:    USER INPUT: indicates whether site is at an airport
C                    (can affect sfc roughness)
C     Airport:       Logical flag (derived), true if site is at an airport

C     AridChr:       USER INPUT: indicates if site is in an arid region
C                    (can affect albedo and bowen ratio)
C     Arid:          Logical flag (derived), true if site is in an arid region

C     SnowChr:       USER INPUT: indicates if there was snow during the winter
C                    (can affect sfc roughness, albedo, and bowen ratio)
C     Snow:          Logical flag (derived), true if snow occurred during the 
C                    winter

C     Moisture:      USER INPUT: indicates average, wet, or dry sfc moisture
C                    (A=Average, W=Wet, D=Dry; affects bowen ratio)
C     MoistStr:      Stores output label for surface moisture
C                    (Average, Wet, Dry)
C     ZoRad:   USER INPUT: radius of the study area (km) for sfc roughness

C     TemporalRes:   USER INPUT: resolution of sfc characteristics
C                    (A=ANNUAL, S=SEASONAL, M=MONTHLY)
C     TempResStr:    Stores output label for temporal resolution
C                    (ANNUAL, SEASONAL, MONTHLY)

C     WndSectors():  USER INPUT: array of sector ID, start/end directions
C     StartDir():    USER INPUT: array of start directions for each wind sector
C     NumSectors:    USER INPUT: number of discrete wind direction sectors

C     SeasonMnths(): 1-D array to hold assignment of a season to
C                    month. Indices 1-12 represent Jan-Dec.  Each month is 
C                    assigned to a season represented by an integer value where:
C                    1=winter w/o snow, 2=winter w/ snow, 3=spring, 4=summer
C                    and 5=autumn.  Default assingment:
C                    dec-feb=winter (w/ or w/o snow based on add'l user input),
C                    mar-may=spring, jun-aug=summer, sep-nov=fall

C     WinWSnMnths:     USER INPUT: Months assigned to winter w/ continous snow
C     WinNoSnMnths:    USER INPUT: Months assigned to winter w/o continous snow
C                                  or late autumn
C     SprgMnths:       USER INPUT: Months assigned to Transitional Spring
C     SumrMnths:       USER INPUT: Months assigned to Mid-Summer lush veg
C     AutmMnths:       USER INPUT: Months assigned to Autumn unharvested crops
C                    
C-----------------------------------------------------------------------
      
      USE Constants, only: FilNmLen

      IMPLICIT NONE
      SAVE
      
      CHARACTER (LEN=FilNmLen)  :: LCFile
      CHARACTER (LEN=FilNmLen)  :: ImpFile, CanFile
      CHARACTER (LEN=8)  :: CoordType,CoordTypeB,TempResStr
      CHARACTER (LEN=7)  :: MoistStr
      CHARACTER (LEN=5)  :: Datum, DatumB
      CHARACTER (LEN=1)  :: TemporalRes

      CHARACTER(len=50) :: tmpStr, tmpStr2       ! temp string used for output

      CHARACTER (LEN=9) :: StatnOrdr 
      CHARACTER (LEN=5) :: Zo_Method
      
      CHARACTER (LEN=1) :: AirportChr, AridChr, SnowChr, Moisture
      CHARACTER (LEN=1) :: AssignMnthChr

      DOUBLE PRECISION :: CenterUTME,  CenterUTMN
      DOUBLE PRECISION :: CenterLatIn, CenterLonIn

      DOUBLE PRECISION :: Anem_Height ! Anemometer height
      DOUBLE PRECISION :: IBL_Factor  ! IBL Factor applied to Anem_Height
      DOUBLE PRECISION :: ZoRad       ! Radius used for surface roughness land cover counts
    
C --- Number of sector keywords
      INTEGER (kind=4)        :: SectKyWds
      
C --- Wind sector definitions
      TYPE SectorDefs
         INTEGER (kind=4)     :: sec_id(16)    ! sector id
         DOUBLE PRECISION     :: sec_start(16) ! start direction
         DOUBLE PRECISION     :: sec_end (16)  ! end direction
         LOGICAL              :: sec_ap(16)    ! airport flag
      END TYPE
      
      TYPE(SectorDefs)        :: WndSectors    ! sector definitions
      
      DOUBLE PRECISION        :: StartDir(16)
      
      INTEGER (kind=4) :: CenterUTMZone
      INTEGER (kind=4) :: NumSectors

      LOGICAL :: VaryAP
      LOGICAL :: Airport, Arid, Snow
      
C --- Declare Logical variable to flag SEASONAL results
      LOGICAL :: L_SEASONAL
      Logical :: AssignMnth = .FALSE.
      
      INTEGER (kind=4) :: SeasonMnths(12) = 0  
      
      CHARACTER (LEN=60) :: WinWSnMnths, WinNoSnMnths, SprgMnths, 
     &                      SumrMnths, AutmMnths   


      END MODULE UserParams

