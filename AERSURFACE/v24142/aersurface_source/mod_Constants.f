C=======================================================================

      MODULE Constants
      
C     Variable declarations and parameter settings for many (not all) 
C     of the constant values used throughout aersurface.  Some variable 
C     constants are set in specific modules.

C     Uses the following modules: none

C     Contains the following procedures: none
      
C=======================================================================

      IMPLICIT NONE
      SAVE
      
      CHARACTER (len=10), PARAMETER  :: Version = '24142' ! Version

      DOUBLE PRECISION, PARAMETER :: PI = 3.14159265359D0  ! Value of Pi
      DOUBLE PRECISION, PARAMETER :: Deg2Rad = PI/180.0D0  ! Degrees to Radians
      DOUBLE PRECISION, PARAMETER :: Rad2Deg = 180.0D0/PI  ! Radians to Degrees
      
      DOUBLE PRECISION, PARAMETER :: Eps5 = 0.00001D0
      
C --- Anemometer Height - allowable maximum and minimum values (meters)
      DOUBLE PRECISION, PARAMETER :: AnemHt_Min = 1.0D0
      DOUBLE PRECISION, PARAMETER :: AnemHt_MAX = 100.0D0
      
C --- IBL Factor - allowable maximum and minimum values
      DOUBLE PRECISION, PARAMETER :: IBLFact_Min = 5.0D0
      DOUBLE PRECISION, PARAMETER :: IBLFact_MAX = 10.0D0
    
C --- IBL Height - allowable maximum and minimum values (meters)
      DOUBLE PRECISION, PARAMETER :: IBL_Min = 30.0D0
      DOUBLE PRECISION, PARAMETER :: IBL_Max = 600.0D0
      
C --- Roughness Radius - allowable maximum and minimum values (kilometers)
      DOUBLE PRECISION, PARAMETER :: ZoRad_Min = 0.5D0
      DOUBLE PRECISION, PARAMETER :: ZoRad_Max = 5.0D0

C --- Maximum number of surface roughness sectors allowed
      INTEGER (kind=4), PARAMETER :: MaxSectors = 16   
      
      DOUBLE PRECISION, PARAMETER :: ExceedTrans = 0.1D0     ! Exceedence limit (decimal percent) for Transition landcover
      DOUBLE PRECISION, PARAMETER :: ExceedMissLow = 0.001D0 ! Lower Exceedence limit (decimal percent) for Missing landcover
      DOUBLE PRECISION, PARAMETER :: ExceedMissHi = 0.5D0    ! Upper Exceedence limit (decimal percent) for Missing landcover
      
      INTEGER (kind=4), PARAMETER :: TransCode = 33        ! Landcover code for Transition category (1992 NLCD Only)
      INTEGER (kind=4), PARAMETER :: MissCode  = 0         ! Landcover code for Missing data

C --- Season Labels based on Seasonal Categories in the deposition algorithms in AERMOD 
      CHARACTER (LEN=*), PARAMETER :: WinWSnowLbl = 
     & 'Winter with continuous snow on the ground'
      CHARACTER (LEN=*), PARAMETER :: WinNoSnowLbl = 
     & 'Late autumn after frost and harvest, or winter with no snow'
      CHARACTER (LEN=*), PARAMETER :: SpringLbl = 
     & 'Transitional spring (partial green coverage, short annuals)'
      CHARACTER (LEN=*), PARAMETER :: SummerLbl = 
     & 'Midsummer with lush vegetation'      
      CHARACTER (LEN=*), PARAMETER :: AutumnLbl = 
     & 'Autumn with unharvested cropland'
     
C --- filename length
      INTEGER (kind=4), PARAMETER :: FilNmLen     = 200 ! Max filename length

      
      END MODULE Constants