C=======================================================================
      MODULE LandCoverParams
      
C     Stores land cover values by category and season for NLCD 1992 and 
C     2001/2006/2011 datasets. Some categories are replicated for special
C     designations specified by the user related to setting, climate,
C     and precip (airport/non-airport [Airp/NAirp]; arid/nonarid [Ar/NAr],
C     below average, average, or above rainfall [Wet/Avg/Dry]).

C     Uses the following modules:
C     - none

C     Uses the following procedures:
C     - subroutine InitLCSurfChar
C     - subroutine ConsolSfcVals

C=======================================================================

      IMPLICIT NONE
      SAVE

C     NumLCTypes = 
C     Number of Land Cover Types; the number is set to 100              
C     rather than the actual number so we can easily match               
C     land cover class number in the table with the                      
C     corresponding one in this array to avoid the need to              
C     reference a mapping of table of class numbers                      
C     to array elements.                                    
C                                                                        
C     Array limits are 0 to 99 where 0 indicates cell is                 
C     outside state boundary and 99 becomes the category                 
C     for missing land cover (in an NLCD file, the value                 
C     for missing data is -9999 so we map it to category 99)

      INTEGER (kind=4), PARAMETER :: NumLCTypes = 100

C     SeasVar = Number of seasonal variances (winter is broken into
C     two variances to indicate snow or no snow)      
C     Array indices: 1-Wnt w/o snow, 2-Wnt w/snow, 3,4,5 - Spr,Sum,Fall
      INTEGER (kind=4), PARAMETER :: SeasVar = 5 

C     Albedo, Z0, BowenAvg, BowenWet, BowenDry
C     albedo; roughness length; and average, wet, dry Bowen ratios
C     A season value is set to -99.0 for season that are
C     not relevant for a particular category (e.g., Bowen
C     ratio for winter with continuous snow cover when 
C     climate conditions are specified by the user as arid. 
C     Winter with continuous snow cover is not applicable for
C     an arid climate.) 

      DOUBLE PRECISION :: Albedo(0:NumLCTypes-1,SeasVar),
     &                    Albedo_Ar(0:NumLCTypes-1,SeasVar),
     &                    Albedo_NAr(0:NumLCTypes-1,SeasVar),
     &                    Z0(0:NumLCTypes-1,SeasVar),
     &                    Z0_Airp(0:NumLCTypes-1,SeasVar),
     &                    Z0_NAirp(0:NumLCTypes-1,SeasVar),
     &                    Z0_Ar(0:NumLCTypes-1,SeasVar),
     &                    Z0_NAr(0:NumLCTypes-1,SeasVar),
     &                    Bowen(0:NumLCTypes-1,SeasVar),
     &                    BowenAvg(0:NumLCTypes-1,SeasVar),
     &                    BowenAvg_Ar(0:NumLCTypes-1,SeasVar),
     &                    BowenAvg_NAr(0:NumLCTypes-1,SeasVar),
     &                    BowenWet(0:NumLCTypes-1,SeasVar),
     &                    BowenWet_Ar(0:NumLCTypes-1,SeasVar),
     &                    BowenWet_NAr(0:NumLCTypes-1,SeasVar),
     &                    BowenDry(0:NumLCTypes-1,SeasVar),
     &                    BowenDry_Ar(0:NumLCTypes-1,SeasVar),
     &                    BowenDry_NAr(0:NumLCTypes-1,SeasVar)

C --- 92 categories and roughness values that carry over to 2001 and 2006 
C     calculations     
      DOUBLE PRECISION :: Z092_22(SeasVar), 
     &                    Z092_31(SeasVar), 
     &                    Z092_43(SeasVar), 
     &                    Z092_85(SeasVar)

C     Store land cover category names based on NLCD version
C     specified by the user
      TYPE NLCDCOVERAGE
            INTEGER (kind=4)   :: ClassNum  ! class/category number
            CHARACTER (LEN=40) :: ClassName ! class/category name
      END TYPE NLCDCOVERAGE
     
      TYPE (NLCDCOVERAGE), DIMENSION(0:NumLCTypes-1) :: LCSurfChar
 
C     Variables to store land cover category codes;
C     (All codes for 92 and 01/06 versions and those codes that
C      are unique to 92 and 01/06 versions.) 2001 and 2006 categories
C      are identical and are stored in the variables designated as "01"   
      INTEGER (kind=4)   :: LCCatsAll92(21), LCCatsAll01(29)
      INTEGER (kind=4)   :: LCCatsOnly92(5), LCCatsOnly01(13)
      
      DATA LCCatsAll92  /11,12,21,22,23,31,32,33,41,42,43,
     &                   51,61,71,81,82,83,84,85,91,92/
     
      DATA LCCatsAll01  /11,12,21,22,23,24,31,32,41,42,43,51,
     &                   52,71,72,73,74,81,82,90,91,92,93,94,
     &                   95,96,97,98,99/
     
      DATA LCCatsOnly92 /33,61,83,84,85/
      DATA LCCatsOnly01 /24,52,72,73,74,90,93,94,95,96,97,98,99/
      
      contains
      
C=======================================================================
      SUBROUTINE InitLCSurfChar(Year)
      
C     Populate Surface Characterist arrays with seasonal values
C     and category descriptions based on NLCD version used (1992, 
C     2001, 2006, 2011, 2016).
C=======================================================================
      
      implicit none

      integer (kind=4), intent(in)   :: Year  ! 1992, 2001, 2006, 2011, 2016

      integer (kind=4) :: inum
      
C --- 92 categories and roughness values that carry over to 2001, 2006, ... calculations
      
      Z092_22(:) = (/1.0D0, 1.0D0, 1.0D0, 1.0D0, 1.0D0/)
      Z092_31(:) = (/0.05D0, 0.05D0, 0.05D0, 0.05D0, 0.05D0/)
      Z092_43(:) = (/0.9D0, 0.8D0, 1.1D0, 1.3D0, 1.3D0/)
      Z092_85(:) = (/0.01D0, 0.005D0, 0.015D0, 0.02D0, 0.015D0/)
      
      select case(Year)

      ! 1-Wnt w/o snow, 2-Wnt w/snow
      ! 3,4,5 - Spr,Sum,Fall
      
C ----------------------------------------------------------------------
C --- 1992 
C ----------------------------------------------------------------------
      case(1992)

C        ----------------
C        ---- ALBEDO ----
C        ----------------
   
         Albedo(11,:)     = (/0.1D0, 0.1D0, 0.1D0, 0.1D0, 0.1D0/)
         Albedo(12,:)     = (/0.7D0, 0.7D0, 0.6D0, 0.6D0, 0.6D0/)
         Albedo(21,:)     = (/0.18D0, 0.45D0, 0.16D0, 0.16D0, 0.16D0/)
         Albedo(22,:)     = (/0.18D0, 0.35D0, 0.18D0, 0.18D0, 0.18D0/)
         Albedo(23,:)     = (/0.18D0, 0.35D0, 0.18D0, 0.18D0, 0.18D0/)
         Albedo_Ar(31,:)  = (/0.2D0, -99.0D0, 0.2D0, 0.2D0, 0.2D0/)
         Albedo_NAr(31,:) = (/0.2D0, 0.6D0, 0.2D0, 0.2D0, 0.2D0/)
         Albedo(32,:)     = (/0.2D0, 0.6D0, 0.2D0, 0.2D0, 0.2D0/)
         Albedo(33,:)     = (/0.18D0, 0.45D0, 0.18D0, 0.18D0, 0.18D0/)
         Albedo(41,:)     = (/0.17D0, 0.5D0, 0.16D0, 0.16D0, 0.16D0/)
         Albedo(42,:)     = (/0.12D0, 0.35D0, 0.12D0, 0.12D0, 0.12D0/)
         Albedo(43,:)     = (/0.14D0, 0.42D0, 0.14D0, 0.14D0, 0.14D0/)
         Albedo_Ar(51,:) = (/0.25D0, -99.0D0, 0.25D0, 0.25D0, 0.25D0/)
         Albedo_NAr(51,:) = (/0.18D0, 0.5D0, 0.18D0, 0.18D0, 0.18D0/)
         Albedo(61,:)     = (/0.18D0, 0.5D0, 0.14D0, 0.18D0, 0.18D0/)
         Albedo(71,:)     = (/0.2D0, 0.6D0, 0.18D0, 0.18D0, 0.18D0/)
         Albedo(81,:)     = (/0.18D0, 0.6D0, 0.14D0, 0.2D0, 0.2D0/)
         Albedo(82,:)     = (/0.18D0, 0.6D0, 0.14D0, 0.2D0, 0.2D0/)
         Albedo(83,:)     = (/0.18D0, 0.6D0, 0.14D0, 0.2D0, 0.2D0/)
         Albedo(84,:)     = (/0.18D0, 0.6D0, 0.18D0, 0.18D0, 0.18D0/)
         Albedo(85,:)     = (/0.18D0, 0.6D0, 0.15D0, 0.15D0, 0.15D0/)
         Albedo(91,:)     = (/0.14D0, 0.3D0, 0.14D0, 0.14D0, 0.14D0/)
         Albedo(92,:)     = (/0.14D0, 0.3D0, 0.14D0, 0.14D0, 0.14D0/)
   

C        -----------------------
C        ---- SFC ROUGHNESS ----
C        -----------------------

         Z0(11,:)     = (/0.001D0, 0.001D0, 0.001D0, 0.001D0, 0.001D0/)
         Z0(12,:)     = (/0.002D0, 0.002D0, 0.002D0, 0.002D0, 0.002D0/)
         Z0(21,:)       = (/0.30D0, 0.30D0, 0.40D0, 0.40D0, 0.40D0/)
         Z0(22,:)       = (/1.0D0, 1.0D0, 1.0D0, 1.0D0, 1.0D0/)
         Z0_Airp(23,:)  = (/0.07D0, 0.07D0, 0.07D0, 0.07D0, 0.07D0/)
         Z0_NAirp(23,:) = (/0.7D0, 0.7D0, 0.7D0, 0.7D0, 0.7D0/)
         Z0_Ar(31,:)    = (/0.05D0, -99.0D0, 0.05D0, 0.05D0, 0.05D0/)
         Z0_NAr(31,:)   = (/0.05D0, 0.05D0, 0.05D0, 0.05D0, 0.05D0/)
         Z0(32,:)       = (/0.3D0, 0.3D0, 0.3D0, 0.3D0, 0.3D0/)
         Z0(33,:)       = (/0.2D0, 0.2D0, 0.2D0, 0.2D0, 0.2D0/)
         Z0(41,:)       = (/0.6D0, 0.5D0, 1.0D0, 1.3D0, 1.3D0/)
         Z0(42,:)       = (/1.3D0, 1.3D0, 1.3D0, 1.3D0, 1.3D0/)
         Z0(43,:)       = (/0.9D0, 0.8D0, 1.1D0, 1.3D0, 1.3D0/)        ! Weighted GM 41&42
         Z0_Ar(51,:)    = (/0.15D0, -99.0D0, 0.15D0, 0.15D0, 0.15D0/)
         Z0_NAr(51,:)   = (/0.3D0, 0.15D0, 0.3D0, 0.3D0, 0.3D0/)
         Z0(61,:)       = (/0.1D0, 0.05D0, 0.2D0, 0.3D0, 0.3D0/)
         Z0(71,:)       = (/0.01D0, 0.005D0, 0.05D0, 0.1D0, 0.1D0/)
         Z0(81,:)       = (/0.02D0, 0.01D0, 0.03D0, 0.15D0, 0.15D0/)
         Z0(82,:)       = (/0.02D0, 0.01D0, 0.03D0, 0.2D0, 0.2D0/)
         Z0(83,:)       = (/0.02D0, 0.01D0, 0.03D0, 0.15D0, 0.15D0/)
         Z0(84,:)       = (/0.02D0, 0.01D0, 0.02D0, 0.05D0, 0.05D0/)
         Z0(85,:)       = (/0.01D0, 0.005D0, 0.015D0, 0.02D0, 0.015D0/)
         Z0(91,:)       = (/0.4D0, 0.3D0, 0.5D0, 0.5D0, 0.5D0/)        ! Weighted GM 43&92
         Z0(92,:)       = (/0.2D0, 0.1D0, 0.2D0, 0.2D0, 0.2D0/)
   
   
C        -------------------------------
C        ---- BOWEN RATIO - Average ----
C        -------------------------------

         BowenAvg(11,:)     = (/0.1D0, 0.1D0, 0.1D0, 0.1D0, 0.1D0/)
         BowenAvg(12,:)     = (/0.5D0, 0.5D0, 0.5D0, 0.5D0, 0.5D0/)
         BowenAvg(21,:)     = (/1.0D0, 0.5D0, 0.8D0, 0.8D0, 1.0D0/)
         BowenAvg(22,:)     = (/1.5D0, 0.5D0, 1.5D0, 1.5D0, 1.5D0/)
         BowenAvg(23,:)     = (/1.5D0, 0.5D0, 1.5D0, 1.5D0, 1.5D0/)
         BowenAvg_Ar(31,:)  = (/6.0D0, -99.0D0, 3.0D0, 4.0D0, 6.0D0/)
         BowenAvg_NAr(31,:) = (/1.5D0, 0.5D0, 1.5D0, 1.5D0, 1.5D0/)
         BowenAvg(32,:)     = (/1.5D0, 0.5D0, 1.5D0, 1.5D0, 1.5D0/)
         BowenAvg(33,:)     = (/1.0D0, 0.5D0, 1.0D0, 1.0D0, 1.0D0/)
         BowenAvg(41,:)     = (/1.0D0, 0.5D0, 0.7D0, 0.3D0, 1.0D0/)
         BowenAvg(42,:)     = (/0.8D0, 0.5D0, 0.7D0, 0.3D0, 0.8D0/)
         BowenAvg(43,:)     = (/0.9D0, 0.5D0, 0.7D0, 0.3D0, 0.9D0/)
         BowenAvg_Ar(51,:)  = (/6.0D0, -99.0D0, 3.0D0, 4.0D0, 6.0D0/)
         BowenAvg_NAr(51,:) = (/1.5D0, 0.5D0, 1.0D0, 1.0D0, 1.5D0/)
         BowenAvg(61,:)     = (/0.7D0, 0.5D0, 0.3D0, 0.5D0, 0.7D0/)
         BowenAvg(71,:)     = (/1.0D0, 0.5D0, 0.4D0, 0.8D0, 1.0D0/)
         BowenAvg(81,:)     = (/0.7D0, 0.5D0, 0.3D0, 0.5D0, 0.7D0/)
         BowenAvg(82,:)     = (/0.7D0, 0.5D0, 0.3D0, 0.5D0, 0.7D0/)
         BowenAvg(83,:)     = (/0.7D0, 0.5D0, 0.3D0, 0.5D0, 0.7D0/)
         BowenAvg(84,:)     = (/0.7D0, 0.5D0, 0.3D0, 0.5D0, 0.7D0/)
         BowenAvg(85,:)     = (/0.7D0, 0.5D0, 0.3D0, 0.5D0, 0.7D0/)
         BowenAvg(91,:)     = (/0.2D0, 0.5D0, 0.2D0, 0.2D0, 0.2D0/)
         BowenAvg(92,:)     = (/0.1D0, 0.5D0, 0.1D0, 0.1D0, 0.1D0/)


C        ---------------------------
C        ---- BOWEN RATIO - Wet ----
C        ---------------------------

         BowenWet(11,:)     = (/0.1D0, 0.1D0, 0.1D0, 0.1D0, 0.1D0/)
         BowenWet(12,:)     = (/0.5D0, 0.5D0, 0.5D0, 0.5D0, 0.5D0/)
         BowenWet(21,:)     = (/0.6D0, 0.5D0, 0.6D0, 0.6D0, 0.6D0/)
         BowenWet(22,:)     = (/1.0D0, 0.5D0, 1.0D0, 1.0D0, 1.0D0/)
         BowenWet(23,:)     = (/1.0D0, 0.5D0, 1.0D0, 1.0D0, 1.0D0/)
         BowenWet_Ar(31,:)  = (/2.0D0, -99.0D0, 1.0D0, 1.5D0, 2.0D0/)
         BowenWet_NAr(31,:) = (/1.0D0, 0.5D0, 1.0D0, 1.0D0, 1.0D0/)
         BowenWet(32,:)     = (/1.0D0, 0.5D0, 1.0D0, 1.0D0, 1.0D0/)
         BowenWet(33,:)     = (/0.7D0, 0.5D0, 0.7D0, 0.7D0, 0.7D0/)
         BowenWet(41,:)     = (/0.4D0, 0.5D0, 0.3D0, 0.2D0, 0.4D0/)
         BowenWet(42,:)     = (/0.3D0, 0.5D0, 0.3D0, 0.2D0, 0.3D0/)
         BowenWet(43,:)     = (/0.35D0, 0.5D0, 0.3D0, 0.2D0, 0.35D0/)
         BowenWet_Ar(51,:)  = (/2.0D0, -99.0D0, 1.0D0, 1.5D0, 2.0D0/)
         BowenWet_NAr(51,:) = (/1.0D0, 0.5D0, 0.8D0, 0.8D0, 1.0D0/)
         BowenWet(61,:)     = (/0.4D0, 0.5D0, 0.2D0, 0.3D0, 0.4D0/)
         BowenWet(71,:)     = (/0.5D0, 0.5D0, 0.3D0, 0.4D0, 0.5D0/)
         BowenWet(81,:)     = (/0.4D0, 0.5D0, 0.2D0, 0.3D0, 0.4D0/)
         BowenWet(82,:)     = (/0.4D0, 0.5D0, 0.2D0, 0.3D0, 0.4D0/)
         BowenWet(83,:)     = (/0.4D0, 0.5D0, 0.2D0, 0.3D0, 0.4D0/)
         BowenWet(84,:)     = (/0.4D0, 0.5D0, 0.2D0, 0.3D0, 0.4D0/)
         BowenWet(85,:)     = (/0.4D0, 0.5D0, 0.2D0, 0.3D0, 0.4D0/)
         BowenWet(91,:)     = (/0.1D0, 0.5D0, 0.1D0, 0.1D0, 0.1D0/)
         BowenWet(92,:)     = (/0.1D0, 0.5D0, 0.1D0, 0.1D0, 0.1D0/)
   

C        ---------------------------
C        ---- BOWEN RATIO - Dry ----
C        ---------------------------

         BowenDry(11,:)     = (/0.1D0, 0.1D0, 0.1D0, 0.1D0, 0.1D0/)
         BowenDry(12,:)     = (/0.5D0, 0.5D0, 0.5D0, 0.5D0, 0.5D0/)
         BowenDry(21,:)     = (/2.5D0, 0.5D0, 2.0D0, 2.0D0, 2.5D0/)
         BowenDry(22,:)     = (/3.0D0, 0.5D0, 3.0D0, 3.0D0, 3.0D0/)
         BowenDry(23,:)     = (/3.0D0, 0.5D0, 3.0D0, 3.0D0, 3.0D0/)
         BowenDry_Ar(31,:)  = (/10.0D0, -99.0D0, 5.0D0, 6.0D0, 10.0D0/)
         BowenDry_NAr(31,:) = (/3.0D0, 0.5D0, 3.0D0, 3.0D0, 3.0D0/)
         BowenDry(32,:)     = (/3.0D0, 0.5D0, 3.0D0, 3.0D0, 3.0D0/)
         BowenDry(33,:)     = (/2.0D0, 0.5D0, 2.0D0, 2.0D0, 2.0D0/)
         BowenDry(41,:)     = (/2.0D0, 0.5D0, 1.5D0, 0.6D0, 2.0D0/)
         BowenDry(42,:)     = (/1.5D0, 0.5D0, 1.5D0, 0.6D0, 1.5D0/)
         BowenDry(43,:)     = (/1.75D0, 0.5D0, 1.5D0, 0.6D0, 1.75D0/)
         BowenDry_Ar(51,:)  = (/10.0D0, -99.0D0, 5.0D0, 6.0D0, 10.0D0/)
         BowenDry_NAr(51,:) = (/3.0D0, 0.5D0, 2.5D0, 2.5D0, 3.0D0/)
         BowenDry(61,:)     = (/2.0D0, 0.5D0, 1.0D0, 1.5D0, 2.0D0/)
         BowenDry(71,:)     = (/2.0D0, 0.5D0, 1.0D0, 2.0D0, 2.0D0/)
         BowenDry(81,:)     = (/2.0D0, 0.5D0, 1.0D0, 1.5D0, 2.0D0/)
         BowenDry(82,:)     = (/2.0D0, 0.5D0, 1.0D0, 1.5D0, 2.0D0/)
         BowenDry(83,:)     = (/2.0D0, 0.5D0, 1.0D0, 1.5D0, 2.0D0/)
         BowenDry(84,:)     = (/2.0D0, 0.5D0, 1.0D0, 1.5D0, 2.0D0/)
         BowenDry(85,:)     = (/2.0D0, 0.5D0, 1.0D0, 1.5D0, 2.0D0/)
         BowenDry(91,:)     = (/0.2D0, 0.5D0, 0.2D0, 0.2D0, 0.2D0/)
         BowenDry(92,:)     = (/0.2D0, 0.5D0, 0.2D0, 0.2D0, 0.2D0/)


         DO inum = 0, NumLCTypes-1
            LCSurfChar(inum) = NLCDCOVERAGE(inum,'')
         ENDDO
         
         LCSurfChar(0)  = NLCDCOVERAGE(0,
     &                   'Miss/Out-of-Bounds/Undef')

         LCSurfChar(11) = NLCDCOVERAGE(11,'Open Water')
         LCSurfChar(12) = NLCDCOVERAGE(12,'Perennial Ice/Snow')

         LCSurfChar(21) = NLCDCOVERAGE(21,'Low Intensity Residential')
         LCSurfChar(22) = NLCDCOVERAGE(22,'High Intensity Residential')
         LCSurfChar(23) = NLCDCOVERAGE(23,
     &                   'Comm/Indust/Tran')

         LCSurfChar(31) = NLCDCOVERAGE(31,'Bare Rock/Sand/Clay')
         LCSurfChar(32) = NLCDCOVERAGE(32,'Quarries/StripMines/Gravel')
         LCSurfChar(33) = NLCDCOVERAGE(33,'Transitional')

         LCSurfChar(41) = NLCDCOVERAGE(41,'Deciduous Forest')
         LCSurfChar(42) = NLCDCOVERAGE(42,'Evergreen Forest')
         LCSurfChar(43) = NLCDCOVERAGE(43,'Mixed Forest')

         LCSurfChar(51) = NLCDCOVERAGE(51,'Shrubland')

         LCSurfChar(61) = NLCDCOVERAGE(61,'Orchards/Vineyard/Other')

         LCSurfChar(71) = NLCDCOVERAGE(71,'Grasslands/Herbaceous')

         LCSurfChar(81) = NLCDCOVERAGE(81,'Pasture/Hay')
         LCSurfChar(82) = NLCDCOVERAGE(82,'Row Crops')
         LCSurfChar(83) = NLCDCOVERAGE(83,'Small Grains')
         LCSurfChar(84) = NLCDCOVERAGE(84,'Fallow')
         LCSurfChar(85) = NLCDCOVERAGE(85,'Urban/Recreational Grass')

         LCSurfChar(91) = NLCDCOVERAGE(91,'Woody Wetlands')
         LCSurfChar(92) = NLCDCOVERAGE(92,
     &                   'Emergent Herb Wetlands')


C ----------------------------------------------------------------------
C --- 2001, 2006, 2011, 2016
C ----------------------------------------------------------------------   

      ! 1-Wnt w/o snow, 2-Wnt w/snow
      ! 3,4,5 - Spr,Sum,Fall
  
      case(2001,2006,2011,2016)

C        ----------------
C        ---- ALBEDO ----
C        ----------------
   
         Albedo(11,:)     = (/0.1D0, 0.1D0, 0.1D0, 0.1D0, 0.1D0/)      ! 11 - 1992
         Albedo(12,:)     = (/0.7D0, 0.7D0, 0.6D0, 0.6D0, 0.6D0/)      ! 12 - 1992
         Albedo(21,:)     = (/0.18D0, 0.6D0, 0.15D0, 0.15D0, 0.15D0/)  ! 85 - 1992
         Albedo(22,:)     = (/0.18D0, 0.45D0, 0.16D0, 0.16D0, 0.16D0/) ! 21 - 1992
         Albedo(23,:)     = (/0.18D0, 0.35D0, 0.18D0, 0.18D0, 0.18D0/) ! 23 - 1992
         Albedo(24,:)     = (/0.18D0, 0.35D0, 0.18D0, 0.18D0, 0.18D0/) ! 23 - 1992
         Albedo_Ar(31,:)  = (/0.2D0, -99.0D0, 0.2D0, 0.2D0, 0.2D0/)    ! 31 - 1992
         Albedo_NAr(31,:) = (/0.2D0, 0.6D0, 0.2D0, 0.2D0, 0.2D0/)      ! 31 - 1992
         Albedo(32,:)     = (/0.14D0, 0.3D0, 0.14D0, 0.14D0, 0.14D0/)  ! 91 - 1992
         Albedo(41,:)     = (/0.17D0, 0.5D0, 0.16D0, 0.16D0, 0.16D0/)  ! 41 - 1992
         Albedo(42,:)     = (/0.12D0, 0.35D0, 0.12D0, 0.12D0, 0.12D0/) ! 42 - 1992
         Albedo(43,:)     = (/0.14D0, 0.42D0, 0.14D0, 0.14D0, 0.14D0/) ! 43 - 1992
         Albedo_Ar(51,:)  = (/0.25D0, -99.0D0, 0.25D0, 0.25D0, 0.25D0/)! 51 - 1992
         Albedo_NAr(51,:) = (/0.18D0, 0.5D0, 0.18D0, 0.18D0, 0.18D0/)  ! 51 - 1992
         Albedo_Ar(52,:)  = (/0.25D0, -99.0D0, 0.25D0, 0.25D0, 0.25D0/)! 51 - 1992
         Albedo_NAr(52,:) = (/0.18D0, 0.5D0, 0.18D0, 0.18D0, 0.18D0/)  ! 51 - 1992
         Albedo(71,:)     = (/0.2D0, 0.6D0, 0.18D0, 0.18D0, 0.18D0/)   ! 71 - 1992
         Albedo(72,:)     = (/0.2D0, 0.6D0, 0.18D0, 0.18D0, 0.18D0/)   ! 71 - 1992
         Albedo(73,:)     = (/0.2D0, 0.6D0, 0.18D0, 0.18D0, 0.18D0/)   ! 71 - 1992
         Albedo(74,:)     = (/0.2D0, 0.6D0, 0.18D0, 0.18D0, 0.18D0/)   ! 71 - 1992
         Albedo(81,:)     = (/0.18D0, 0.6D0, 0.14D0, 0.2D0, 0.2D0/)    ! 81 - 1992
         Albedo(82,:)     = (/0.18D0, 0.6D0, 0.14D0, 0.2D0, 0.2D0/)    ! 82 - 1992
         Albedo(90,:)     = (/0.14D0, 0.3D0, 0.14D0, 0.14D0, 0.14D0/)   ! 91 - 1992
         Albedo(91,:)     = (/0.14D0, 0.3D0, 0.14D0, 0.14D0, 0.14D0/)   ! 91 - 1992
         Albedo(92,:)     = (/0.14D0, 0.3D0, 0.14D0, 0.14D0, 0.14D0/)   ! 91 - 1992
         Albedo(93,:)     = (/0.14D0, 0.3D0, 0.14D0, 0.14D0, 0.14D0/)   ! 91 - 1992
         Albedo(94,:)     = (/0.14D0, 0.3D0, 0.14D0, 0.14D0, 0.14D0/)   ! 91 - 1992
         Albedo(95,:)     = (/0.14D0, 0.3D0, 0.14D0, 0.14D0, 0.14D0/)   ! 92 - 1992
         Albedo(96,:)     = (/0.14D0, 0.3D0, 0.14D0, 0.14D0, 0.14D0/)   ! 92 - 1992
         Albedo(97,:)     = (/0.14D0, 0.3D0, 0.14D0, 0.14D0, 0.14D0/)   ! 92 - 1992
         Albedo(98,:)     = (/0.14D0, 0.3D0, 0.14D0, 0.14D0, 0.14D0/)   ! 92 - 1992
         Albedo(99,:)     = (/0.14D0, 0.3D0, 0.14D0, 0.14D0, 0.14D0/)   ! 92 - 1992
   

C        -----------------------
C        ---- SFC ROUGHNESS ----
C        -----------------------

         Z0(11,:)     = (/0.001D0, 0.001D0, 0.001D0, 0.001D0, 0.001D0/)    ! 11 - 1992
         Z0(12,:)     = (/0.002D0, 0.002D0, 0.002D0, 0.002D0, 0.002D0/)    ! 12 - 1992
         Z0_Airp(21,:)  = (/0.02D0, 0.01D0, 0.02D0, 0.03D0, 0.03D0/)       ! Weighted GM - 1992 22&31&43&85
         Z0_NAirp(21,:) = (/0.02D0, 0.01D0, 0.03D0, 0.04D0, 0.03D0/)       ! Weighted GM - 1992 22&31&43&85
         Z0_Airp(22,:)  = (/0.03D0, 0.02D0, 0.03D0, 0.04D0, 0.03D0/)       ! Weighted GM - 1992 22&31&43&85
         Z0_NAirp(22,:) = (/0.07D0, 0.05D0, 0.09D0, 0.1D0, 0.09D0/)        ! Weighted GM - 1992 22&31&43&85
         Z0_Airp(23,:)  = (/0.05D0, 0.04D0, 0.06D0, 0.06D0, 0.06D0/)       ! Weighted GM - 1992 22&31&43&85
         Z0_NAirp(23,:) = (/0.3D0, 0.2D0, 0.3D0, 0.3D0, 0.3D0/)            ! Weighted GM - 1992 22&31&43&85
         Z0_Airp(24,:)  = (/0.07D0, 0.07D0, 0.07D0, 0.08D0, 0.08D0/)       ! Weighted GM - 1992 22&31&43&85
         Z0_NAirp(24,:) = (/0.7D0, 0.7D0, 0.7D0, 0.7D0, 0.7D0/)            ! Weighted GM - 1992 22&31&43&85
         Z0_Ar(31,:)    = (/0.05D0, -99.0D0, 0.05D0, 0.05D0, 0.05D0/)      ! 31 - 1992
         Z0_NAr(31,:)   = (/0.05D0, 0.01D0, 0.05D0, 0.05D0, 0.05D0/)       
         Z0(32,:)       = (/0.05D0, 0.01D0, 0.05D0, 0.05D0, 0.05D0/)       ! 31 - 1992
         Z0(41,:)       = (/0.6D0, 0.5D0, 1.0D0, 1.3D0, 1.3D0/)            ! 41 - 1992
         Z0(42,:)       = (/1.3D0, 1.3D0, 1.3D0, 1.3D0, 1.3D0/)            ! 42 - 1992
         Z0(43,:)       = (/0.9D0, 0.8D0, 1.1D0, 1.3D0, 1.3D0/)            ! 43 - 1992
         Z0_Ar(51,:)    = (/0.05D0, -99.0D0, 0.05D0, 0.05D0, 0.05D0/)      ! 51 - 1992 * 1/3
         Z0_NAr(51,:)   = (/0.1D0, 0.05D0, 0.1D0, 0.1D0, 0.1D0/)           ! 51 - 1992 * 1/3
         Z0_Ar(52,:)    = (/0.15D0, -99.0D0, 0.15D0, 0.15D0, 0.15D0/)      ! 51 - 1992
         Z0_NAr(52,:)   = (/0.3D0, 0.15D0, 0.3D0, 0.3D0, 0.3D0/)           ! 51 - 1992
         Z0(71,:)       = (/0.01D0, 0.005D0, 0.05D0, 0.1D0, 0.1D0/)        ! 71 - 1992
         Z0(72,:)       = (/0.01D0, 0.005D0, 0.05D0, 0.1D0, 0.1D0/)        ! 71 - 1992
         Z0(73,:)       = (/0.01D0, 0.005D0, 0.05D0, 0.05D0, 0.05D0/)        
         Z0(74,:)       = (/0.01D0, 0.005D0, 0.05D0, 0.05D0, 0.05D0/)        
         Z0_Airp(81,:)  = (/0.02D0, 0.01D0, 0.02D0, 0.03D0, 0.03D0/)       ! 21 - 2001
         Z0_NAirp(81,:) = (/0.02D0, 0.01D0, 0.03D0, 0.15D0, 0.15D0/)       ! 81 - 1992
         Z0_Airp(82,:)  = (/0.02D0, 0.01D0, 0.02D0, 0.03D0, 0.03D0/)       ! 21 - 2001
         Z0_NAirp(82,:) = (/0.03D0, 0.014D0, 0.04D0, 0.2D0, 0.2D0/)         
         Z0(90,:)       = (/0.4D0, 0.3D0, 0.5D0, 0.5D0, 0.5D0/)            ! 91 - 1992
         Z0(91,:)       = (/0.4D0, 0.3D0, 0.5D0, 0.5D0, 0.5D0/)            ! 91 - 1992
         Z0(92,:)       = (/0.2D0, 0.1D0, 0.2D0, 0.2D0, 0.2D0/)            ! 92 - 1992
         Z0(93,:)       = (/0.4D0, 0.3D0, 0.5D0, 0.5D0, 0.5D0/)            ! 91 - 1992
         Z0(94,:)       = (/0.2D0, 0.1D0, 0.2D0, 0.2D0, 0.2D0/)            ! 92 - 1992
         Z0(95,:)       = (/0.2D0, 0.1D0, 0.2D0, 0.2D0, 0.2D0/)            ! 92 - 1992
         Z0(96,:)       = (/0.2D0, 0.1D0, 0.2D0, 0.2D0, 0.2D0/)            ! 92 - 1992
         Z0(97,:)       = (/0.2D0, 0.1D0, 0.2D0, 0.2D0, 0.2D0/)            ! 92 - 1992
         Z0(98,:)       = (/0.05D0, 0.005D0, 0.05D0, 0.05D0, 0.05D0/)            
         Z0(99,:)       = (/0.05D0, 0.005D0, 0.05D0, 0.05D0, 0.05D0/)            
   
   
C        -------------------------------
C        ---- BOWEN RATIO - Average ----
C        -------------------------------

         BowenAvg(11,:)     = (/0.1D0, 0.1D0, 0.1D0, 0.1D0, 0.1D0/)        ! 11 - 1992
         BowenAvg(12,:)     = (/0.5D0, 0.5D0, 0.5D0, 0.5D0, 0.5D0/)        ! 12 - 1992
         BowenAvg(21,:)     = (/0.7D0, 0.5D0, 0.3D0, 0.5D0, 0.7D0/)        ! 85 - 1992
         BowenAvg(22,:)     = (/1.0D0, 0.5D0, 0.8D0, 0.8D0, 1.0D0/)        ! 21 - 1992
         BowenAvg(23,:)     = (/1.2D0, 0.5D0, 1.1D0, 1.1D0, 1.2D0/)        
         BowenAvg(24,:)     = (/1.5D0, 0.5D0, 1.5D0, 1.5D0, 1.5D0/)        ! 23 - 1992
         BowenAvg_Ar(31,:)  = (/6.0D0, -99.0D0, 3.0D0, 4.0D0, 6.0D0/)      ! 31 - 1992
         BowenAvg_NAr(31,:) = (/1.5D0, 0.5D0, 1.5D0, 1.5D0, 1.5D0/)        ! 31 - 1992
         BowenAvg(32,:)     = (/0.2D0, 0.5D0, 0.2D0, 0.2D0, 0.2D0/)        ! 91 - 1992
         BowenAvg(41,:)     = (/1.0D0, 0.5D0, 0.7D0, 0.3D0, 1.0D0/)        ! 41 - 1992
         BowenAvg(42,:)     = (/0.8D0, 0.5D0, 0.7D0, 0.3D0, 0.8D0/)        ! 42 - 1992
         BowenAvg(43,:)     = (/0.9D0, 0.5D0, 0.7D0, 0.3D0, 0.9D0/)        ! 43 - 1992
         BowenAvg_Ar(51,:)  = (/4.0D0, -99.0D0, 2.0D0, 3.0D0, 4.0D0/)      
         BowenAvg_NAr(51,:) = (/1.5D0, 0.5D0, 1.0D0, 1.0D0, 1.5D0/)        ! 51 - 1992
         BowenAvg_Ar(52,:)  = (/6.0D0, -99.0D0, 3.0D0, 4.0D0, 6.0D0/)      ! 51 - 1992
         BowenAvg_NAr(52,:) = (/1.5D0, 0.5D0, 1.0D0, 1.0D0, 1.5D0/)        ! 51 - 1992
         BowenAvg(71,:)     = (/1.0D0, 0.5D0, 0.4D0, 0.8D0, 1.0D0/)        ! 71 - 1992
         BowenAvg(72,:)     = (/1.0D0, 0.5D0, 0.4D0, 0.8D0, 1.0D0/)        ! 71 - 1992
         BowenAvg(73,:)     = (/1.0D0, 0.5D0, 0.4D0, 0.8D0, 1.0D0/)        ! 71 - 1992
         BowenAvg(74,:)     = (/1.0D0, 0.5D0, 0.4D0, 0.8D0, 1.0D0/)        ! 71 - 1992
         BowenAvg(81,:)     = (/0.7D0, 0.5D0, 0.3D0, 0.5D0, 0.7D0/)        ! 81 - 1992
         BowenAvg(82,:)     = (/0.7D0, 0.5D0, 0.3D0, 0.5D0, 0.7D0/)        ! 82 - 1992
         BowenAvg(90,:)     = (/0.2D0, 0.5D0, 0.2D0, 0.2D0, 0.2D0/)        ! 91 - 1992
         BowenAvg(91,:)     = (/0.2D0, 0.5D0, 0.2D0, 0.2D0, 0.2D0/)        ! 91 - 1992
         BowenAvg(92,:)     = (/0.2D0, 0.5D0, 0.2D0, 0.2D0, 0.2D0/)        ! 91 - 1992
         BowenAvg(93,:)     = (/0.2D0, 0.5D0, 0.2D0, 0.2D0, 0.2D0/)        ! 91 - 1992
         BowenAvg(94,:)     = (/0.2D0, 0.5D0, 0.2D0, 0.2D0, 0.2D0/)        ! 91 - 1992
         BowenAvg(95,:)     = (/0.1D0, 0.5D0, 0.1D0, 0.1D0, 0.1D0/)        ! 92 - 1992
         BowenAvg(96,:)     = (/0.1D0, 0.5D0, 0.1D0, 0.1D0, 0.1D0/)        ! 92 - 1992
         BowenAvg(97,:)     = (/0.1D0, 0.5D0, 0.1D0, 0.1D0, 0.1D0/)        ! 92 - 1992
         BowenAvg(98,:)     = (/0.1D0, 0.1D0, 0.1D0, 0.1D0, 0.1D0/)        ! 11 - 1992
         BowenAvg(99,:)     = (/0.1D0, 0.1D0, 0.1D0, 0.1D0, 0.1D0/)        ! 11 - 1992


C        ---------------------------
C        ---- BOWEN RATIO - Wet ----
C        ---------------------------

         BowenWet(11,:)     = (/0.1D0, 0.1D0, 0.1D0, 0.1D0, 0.1D0/)        ! 11 - 1992
         BowenWet(12,:)     = (/0.5D0, 0.5D0, 0.5D0, 0.5D0, 0.5D0/)        ! 12 - 1992
         BowenWet(21,:)     = (/0.4D0, 0.5D0, 0.2D0, 0.3D0, 0.4D0/)        ! 85 - 1992
         BowenWet(22,:)     = (/0.6D0, 0.5D0, 0.6D0, 0.6D0, 0.6D0/)        ! 21 - 1992
         BowenWet(23,:)     = (/0.8D0, 0.5D0, 0.8D0, 0.8D0, 0.8D0/)        
         BowenWet(24,:)     = (/1.0D0, 0.5D0, 1.0D0, 1.0D0, 1.0D0/)        ! 23 - 1992
         BowenWet_Ar(31,:)  = (/2.0D0, -99.0D0, 1.0D0, 1.5D0, 2.0D0/)      ! 31 - 1992
         BowenWet_NAr(31,:) = (/1.0D0,   0.5D0, 1.0D0, 1.0D0, 1.0D0/)      ! 31 - 1992
         BowenWet(32,:)     = (/0.1D0, 0.5D0, 0.1D0, 0.1D0, 0.1D0/)        ! 91 - 1992
         BowenWet(41,:)     = (/0.4D0, 0.5D0, 0.3D0, 0.2D0, 0.4D0/)        ! 41 - 1992
         BowenWet(42,:)     = (/0.3D0, 0.5D0, 0.3D0, 0.2D0, 0.3D0/)        ! 42 - 1992
         BowenWet(43,:)     = (/0.35D0, 0.5D0, 0.3D0, 0.2D0, 0.35D0/)      ! 43 - 1992
         BowenWet_Ar(51,:)  = (/1.5D0, -99.0D0, 0.8D0, 0.9D0, 1.5D0/)      
         BowenWet_NAr(51,:) = (/1.0D0, 0.5D0, 0.8D0, 0.8D0, 1.0D0/)        ! 51 - 1992
         BowenWet_Ar(52,:)  = (/2.0D0, -99.0D0, 1.0D0, 1.5D0, 2.0D0/)      ! 51 - 1992
         BowenWet_NAr(52,:) = (/1.0D0, 0.5D0, 0.8D0, 0.8D0, 1.0D0/)        ! 51 - 1992
         BowenWet(71,:)     = (/0.5D0, 0.5D0, 0.3D0, 0.4D0, 0.5D0/)        ! 71 - 1992
         BowenWet(72,:)     = (/0.5D0, 0.5D0, 0.3D0, 0.4D0, 0.5D0/)        ! 71 - 1992
         BowenWet(73,:)     = (/0.5D0, 0.5D0, 0.3D0, 0.4D0, 0.5D0/)        ! 71 - 1992
         BowenWet(74,:)     = (/0.5D0, 0.5D0, 0.3D0, 0.4D0, 0.5D0/)        ! 71 - 1992
         BowenWet(81,:)     = (/0.4D0, 0.5D0, 0.2D0, 0.3D0, 0.4D0/)        ! 81 - 1992
         BowenWet(82,:)     = (/0.4D0, 0.5D0, 0.2D0, 0.3D0, 0.4D0/)        ! 82 - 1992
         BowenWet(90,:)     = (/0.1D0, 0.5D0, 0.1D0, 0.1D0, 0.1D0/)        ! 91 - 1992
         BowenWet(91,:)     = (/0.1D0, 0.5D0, 0.1D0, 0.1D0, 0.1D0/)        ! 91 - 1992
         BowenWet(92,:)     = (/0.1D0, 0.5D0, 0.1D0, 0.1D0, 0.1D0/)        ! 91 - 1992
         BowenWet(93,:)     = (/0.1D0, 0.5D0, 0.1D0, 0.1D0, 0.1D0/)        ! 91 - 1992
         BowenWet(94,:)     = (/0.1D0, 0.5D0, 0.1D0, 0.1D0, 0.1D0/)        ! 91 - 1992
         BowenWet(95,:)     = (/0.1D0, 0.5D0, 0.1D0, 0.1D0, 0.1D0/)        ! 92 - 1992
         BowenWet(96,:)     = (/0.1D0, 0.5D0, 0.1D0, 0.1D0, 0.1D0/)        ! 92 - 1992
         BowenWet(97,:)     = (/0.1D0, 0.5D0, 0.1D0, 0.1D0, 0.1D0/)        ! 92 - 1992
         BowenWet(98,:)     = (/0.1D0, 0.1D0, 0.1D0, 0.1D0, 0.1D0/)        ! 11 - 1992
         BowenWet(99,:)     = (/0.1D0, 0.1D0, 0.1D0, 0.1D0, 0.1D0/)        ! 11 - 1992
   

C        ---------------------------
C        ---- BOWEN RATIO - Dry ----
C        ---------------------------

         BowenDry(11,:)     = (/0.1D0, 0.1D0, 0.1D0, 0.1D0, 0.1D0/)        ! 11 - 1992
         BowenDry(12,:)     = (/0.5D0, 0.5D0, 0.5D0, 0.5D0, 0.5D0/)        ! 12 - 1992
         BowenDry(21,:)     = (/2.0D0, 0.5D0, 1.0D0, 1.5D0, 2.0D0/)        ! 85 - 1992
         BowenDry(22,:)     = (/2.5D0, 0.5D0, 2.0D0, 2.0D0, 2.5D0/)        ! 21 - 1992
         BowenDry(23,:)     = (/3.0D0, 0.5D0, 3.0D0, 3.0D0, 3.0D0/)        ! 23 - 1992
         BowenDry(24,:)     = (/3.0D0, 0.5D0, 3.0D0, 3.0D0, 3.0D0/)        ! 23 - 1992
         BowenDry_Ar(31,:)  = (/10.0D0, -99.0D0, 5.0D0, 6.0D0, 10.0D0/)    ! 31 - 1992
         BowenDry_NAr(31,:) = (/3.0D0, 0.5D0, 3.0D0, 3.0D0, 3.0D0/)        ! 31 - 1992
         BowenDry(32,:)     = (/0.2D0, 0.5D0, 0.2D0, 0.2D0, 0.2D0/)        ! 91 - 1992
         BowenDry(41,:)     = (/2.0D0, 0.5D0, 1.5D0, 0.6D0, 2.0D0/)        ! 41 - 1992
         BowenDry(42,:)     = (/1.5D0, 0.5D0, 1.5D0, 0.6D0, 1.5D0/)        ! 42 - 1992
         BowenDry(43,:)     = (/1.75D0, 0.5D0, 1.5D0, 0.6D0, 1.75D0/)      ! 43 - 1992
         BowenDry_Ar(51,:)  = (/7.0D0, -99.0D0, 4.0D0, 6.0D0, 7.0D0/)      
         BowenDry_NAr(51,:) = (/3.0D0, 0.5D0, 2.5D0, 2.5D0, 3.0D0/)        ! 51 - 1992
         BowenDry_Ar(52,:)  = (/10.0D0, -99.0D0, 5.0D0, 6.0D0, 10.0D0/)    ! 51 - 1992
         BowenDry_NAr(52,:) = (/3.0D0, 0.5D0, 2.5D0, 2.5D0, 3.0D0/)        ! 51 - 1992
         BowenDry(71,:)     = (/2.0D0, 0.5D0, 1.0D0, 2.0D0, 2.0D0/)        ! 71 - 1992
         BowenDry(72,:)     = (/2.0D0, 0.5D0, 1.0D0, 2.0D0, 2.0D0/)        ! 71 - 1992
         BowenDry(73,:)     = (/2.0D0, 0.5D0, 1.0D0, 2.0D0, 2.0D0/)        ! 71 - 1992
         BowenDry(74,:)     = (/2.0D0, 0.5D0, 1.0D0, 2.0D0, 2.0D0/)        ! 71 - 1992
         BowenDry(81,:)     = (/2.0D0, 0.5D0, 1.0D0, 1.5D0, 2.0D0/)        ! 81 - 1992
         BowenDry(82,:)     = (/2.0D0, 0.5D0, 1.0D0, 1.5D0, 2.0D0/)        ! 82 - 1992
         BowenDry(90,:)     = (/0.2D0, 0.5D0, 0.2D0, 0.2D0, 0.2D0/)        ! 91 - 1992
         BowenDry(91,:)     = (/0.2D0, 0.5D0, 0.2D0, 0.2D0, 0.2D0/)        ! 91 - 1992
         BowenDry(92,:)     = (/0.2D0, 0.5D0, 0.2D0, 0.2D0, 0.2D0/)        ! 91 - 1992
         BowenDry(93,:)     = (/0.2D0, 0.5D0, 0.2D0, 0.2D0, 0.2D0/)        ! 91 - 1992
         BowenDry(94,:)     = (/0.2D0, 0.5D0, 0.2D0, 0.2D0, 0.2D0/)        ! 91 - 1992
         BowenDry(95,:)     = (/0.2D0, 0.5D0, 0.2D0, 0.2D0, 0.2D0/)        ! 92 - 1992
         BowenDry(96,:)     = (/0.2D0, 0.5D0, 0.2D0, 0.2D0, 0.2D0/)        ! 92 - 1992
         BowenDry(97,:)     = (/0.2D0, 0.5D0, 0.2D0, 0.2D0, 0.2D0/)        ! 92 - 1992
         BowenDry(98,:)     = (/0.1D0, 0.1D0, 0.1D0, 0.1D0, 0.1D0/)        ! 11 - 1992
         BowenDry(99,:)     = (/0.1D0, 0.1D0, 0.1D0, 0.1D0, 0.1D0/)        ! 11 - 1992


         DO inum = 0, NumLCTypes-1
            LCSurfChar(inum) = NLCDCOVERAGE(inum,'')
         ENDDO
         
         LCSurfChar(0)  = NLCDCOVERAGE(0,
     &                   'Missing, Out-of-Bounds, or Undefined')

         LCSurfChar(11) = NLCDCOVERAGE(11,'Open Water')
         LCSurfChar(12) = NLCDCOVERAGE(12,'Perennial Ice/Snow')

         LCSurfChar(21) = NLCDCOVERAGE(21,'Developed, Open Space')
         LCSurfChar(22) = NLCDCOVERAGE(22,'Developed, Low Intensity')
         LCSurfChar(23) = NLCDCOVERAGE(23,
     &                   'Developed, Medium Intensity')
         LCSurfChar(24) = NLCDCOVERAGE(24,
     &                   'Developed, High Intensity')

         LCSurfChar(31) = NLCDCOVERAGE(31,
     &                   'Barren Land (Rock/Sand/Clay)')
         LCSurfChar(32) = NLCDCOVERAGE(32,'Unconsolidated Shore')

         LCSurfChar(41) = NLCDCOVERAGE(41,'Deciduous Forest')
         LCSurfChar(42) = NLCDCOVERAGE(42,'Evergreen Forest')
         LCSurfChar(43) = NLCDCOVERAGE(43,'Mixed Forest')

         LCSurfChar(51) = NLCDCOVERAGE(51,'Dwarf Scrub')
         LCSurfChar(52) = NLCDCOVERAGE(52,'Shrub/Scrub')

         LCSurfChar(71) = NLCDCOVERAGE(71,'Grasslands/Herbaceous')
         LCSurfChar(72) = NLCDCOVERAGE(72,'Sedge/Herbaceous')
         LCSurfChar(73) = NLCDCOVERAGE(73,'Lichens')
         LCSurfChar(74) = NLCDCOVERAGE(74,'Moss')

         LCSurfChar(81) = NLCDCOVERAGE(81,'Pasture/Hay')
         LCSurfChar(82) = NLCDCOVERAGE(82,'Cultivated Crops')

         LCSurfChar(90) = NLCDCOVERAGE(90,'Woody Wetlands')
         LCSurfChar(91) = NLCDCOVERAGE(91,
     &                   'Palustrine Forested Wetland')
         LCSurfChar(92) = NLCDCOVERAGE(92,
     &                   'Palustrine Scrub/Shrub Wetland')
         LCSurfChar(93) = NLCDCOVERAGE(93,
     &                   'Estuarine Forested Wetland')
         LCSurfChar(94) = NLCDCOVERAGE(94,
     &                   'Estuarine Scrub/Shrub Wetland')
         LCSurfChar(95) = NLCDCOVERAGE(95,
     &                   'Emergent Herbaceous Wetland')
         LCSurfChar(96) = NLCDCOVERAGE(96,
     &                   'Palustrine Emergent Wetland (Persistent)')
         LCSurfChar(97) = NLCDCOVERAGE(97,
     &                   'Estuarine Emergent Wetland')
         LCSurfChar(98) = NLCDCOVERAGE(98,
     &                   'Palustrine Aquatic Bed')
         LCSurfChar(99) = NLCDCOVERAGE(99,
     &                   'Estuarine Aquatic Bed')

      end select

      END SUBROUTINE InitLCSurfChar

  
C=======================================================================      
      SUBROUTINE ConsolSfcVals(Year,Airport,Moisture,Arid)
C
C     Load reference surface value arrays.  Handles variation
C     related to site characteristics - airport, moisture, arid
C
C=======================================================================

      implicit none      
      
      integer (kind=4), intent(in)   :: Year     ! 1992, 2001, 2006
      logical,          intent(in)   :: Airport  ! is site/sector an airport
C     Moisture: A=Average, W=Wet, D=Dry
      character (len=*),intent(in)   :: Moisture ! Moisture conditions
      logical,          intent(in)   :: Arid     ! Arid conditions
      
      integer (kind=4)               :: i, j    ! loop counters

C     sfc moisture, adjust bowen ratio for average, wet, dry
C     independent of NLCDYear 
      DO i=0, NumLCTypes-1
         DO j=1, SeasVar
            IF( Moisture == 'A') THEN
                Bowen(i,j) = BowenAvg(i,j)
            ELSEIF( Moisture == 'W') THEN
                Bowen(i,j) = BowenWet(i,j)
            ELSEIF( Moisture == 'D') THEN
                Bowen(i,j) = BowenDry(i,j)
            ENDIF
         ENDDO
      ENDDO

C     airport - adjust sfc roughness array
      select case(Year)
      case(1992)
         IF( Airport ) THEN
            DO j=1, SeasVar
               Z0(23,j) = Z0_Airp(23,j)
            END DO  
         ELSE
            DO j=1, SeasVar
               Z0(23,j) = Z0_NAirp(23,j)
            END DO
         END IF
         
      case(2001,2006,2011,2016)
         IF( Airport ) THEN
            DO j=1, SeasVar
               Z0(21,j) = Z0_Airp(21,j)
               Z0(22,j) = Z0_Airp(22,j)
               Z0(23,j) = Z0_Airp(23,j)
               Z0(24,j) = Z0_Airp(24,j)
               Z0(81,j) = Z0_Airp(81,j)
               Z0(82,j) = Z0_Airp(82,j)
            END DO  
         ELSE
            DO j=1, SeasVar
               Z0(21,j) = Z0_NAirp(21,j)
               Z0(22,j) = Z0_NAirp(22,j)
               Z0(23,j) = Z0_NAirp(23,j)
               Z0(24,j) = Z0_NAirp(24,j)
               Z0(81,j) = Z0_NAirp(81,j)
               Z0(82,j) = Z0_NAirp(82,j)
            END DO
         END IF
      
      end select
      
C     arid - adjust sfc roughness array, albedo, bowen ratio
      select case(Year)
      case(1992)
         IF( Arid ) THEN
            DO j=1, SeasVar
               Z0(31,j) = Z0_Ar(31,j)
               Z0(51,j) = Z0_Ar(51,j)
               Albedo(31,j) = Albedo_Ar(31,j)
               Albedo(51,j) = Albedo_Ar(51,j)
               IF( Moisture == 'A') THEN
                  Bowen(31,j) = BowenAvg_Ar(31,j)
                  Bowen(51,j) = BowenAvg_Ar(51,j)
               ELSEIF( Moisture == 'W') THEN
                  Bowen(31,j) = BowenWet_Ar(31,j)
                  Bowen(51,j) = BowenWet_Ar(51,j)
               ELSEIF( Moisture == 'D') THEN
                  Bowen(31,j) = BowenDry_Ar(31,j)
                  Bowen(51,j) = BowenDry_Ar(51,j)
               END IF
            END DO
         ELSE
            DO j=1, SeasVar
               Z0(31,j) = Z0_NAr(31,j)
               Z0(51,j) = Z0_NAr(51,j)
               Albedo(31,j) = Albedo_NAr(31,j)
               Albedo(51,j) = Albedo_NAr(51,j)
               IF( Moisture == 'A') THEN
                  Bowen(31,j) = BowenAvg_NAr(31,j)
                  Bowen(51,j) = BowenAvg_NAr(51,j)
               ELSEIF( Moisture == 'W') THEN
                  Bowen(31,j) = BowenWet_NAr(31,j)
                  Bowen(51,j) = BowenWet_NAr(51,j)
               ELSEIF( Moisture == 'D') THEN
                  Bowen(31,j) = BowenDry_NAr(31,j)
                  Bowen(51,j) = BowenDry_NAr(51,j)
               END IF
            END DO
         END IF
         
      case(2001,2006,2011,2016)
         IF( Arid ) THEN
            DO j=1, SeasVar
               Z0(31,j) = Z0_Ar(31,j)
               Z0(51,j) = Z0_Ar(51,j)
               Z0(52,j) = Z0_Ar(52,j)
               Albedo(31,j) = Albedo_Ar(31,j)
               Albedo(51,j) = Albedo_Ar(51,j)
               Albedo(52,j) = Albedo_Ar(52,j)
               IF( Moisture == 'A') THEN
                  Bowen(31,j) = BowenAvg_Ar(31,j)
                  Bowen(51,j) = BowenAvg_Ar(51,j)
                  Bowen(52,j) = BowenAvg_Ar(52,j)
               ELSEIF( Moisture == 'W') THEN
                  Bowen(31,j) = BowenWet_Ar(31,j)
                  Bowen(51,j) = BowenWet_Ar(51,j)
                  Bowen(52,j) = BowenWet_Ar(52,j)
               ELSEIF( Moisture == 'D') THEN
                  Bowen(31,j) = BowenDry_Ar(31,j)
                  Bowen(51,j) = BowenDry_Ar(51,j)
                  Bowen(52,j) = BowenDry_Ar(52,j)
               END IF
            END DO
         ELSE
            DO j=1, SeasVar
               Z0(31,j) = Z0_NAr(31,j)
               Z0(51,j) = Z0_NAr(51,j)
               Z0(52,j) = Z0_NAr(52,j)
               Albedo(31,j) = Albedo_NAr(31,j)
               Albedo(51,j) = Albedo_NAr(51,j)
               Albedo(52,j) = Albedo_NAr(52,j)
               IF( Moisture == 'A') THEN
                  Bowen(31,j) = BowenAvg_NAr(31,j)
                  Bowen(51,j) = BowenAvg_NAr(51,j)
                  Bowen(52,j) = BowenAvg_NAr(52,j)
               ELSEIF( Moisture == 'W') THEN
                  Bowen(31,j) = BowenWet_NAr(31,j)
                  Bowen(51,j) = BowenWet_NAr(51,j)
                  Bowen(52,j) = BowenWet_NAr(52,j)
               ELSEIF( Moisture == 'D') THEN
                  Bowen(31,j) = BowenDry_NAr(31,j)
                  Bowen(51,j) = BowenDry_NAr(51,j)
                  Bowen(52,j) = BowenDry_NAr(52,j)
               END IF
            END DO
         END IF
         
      end select
      
      RETURN
      
      END SUBROUTINE


      END MODULE LandCoverParams
