C=======================================================================
      MODULE AvgParams
      
C     Variable declarations and procedures used to derive average
C     surface values for Bowen ratio, albedo, and surface roughness.

C     Uses the following modules:
C     - StartVars
C     - Constants
C     - ProcCtrlFile
C     - UserParams
C     - LandCoverParams
C     - FileUnits
C     - GetData

C     Contains the following procedures:
C     - subroutine AvgSfcCharsSimple
C     - subroutine AvgSfcCharsGeom
C     - subroutine EffectiveRoughness4
C     - double function z0_eff_func

C=======================================================================

      USE StartVars, only: run_start, iYr2, ModLen, ModNam
      
      USE Constants, only: Version, MaxSectors, MissCode,
     &    IBL_Min, IBL_Max 

      use ProcCtrlFile, only: NumSeas, EffRadDbg, ILINE
      
      USE UserParams, only: Zo_Method, Anem_Height, IBL_Factor, ZoRad, 
     &    NumSectors, TemporalRes, SeasonMnths, Snow, StartDir
     
      USE LandCoverParams, only: NumLCTypes, Albedo

      USE FileUnits, only: LogFile, LogUnt, OutUnt, EffRadUnt,
     &    EffRadFile
     
      USE GetData

      USE ErrorHandling, only: ERRHDL

      IMPLICIT NONE
      SAVE
      

C --- Arrays hold averaged values of surface characteristics by sector
C     up to 12 averaging periods (annual(1), season(1-4), months(1-12))

      DOUBLE PRECISION    :: AvgAlbedo(MaxSectors,12)
      DOUBLE PRECISION    :: AvgZo(MaxSectors,12)
      DOUBLE PRECISION    :: AvgBowen(MaxSectors,12)
      
CCRT- Arrays to hold abbreviated Effective Roughness data from 
C     Effective Roughness procedure for log file output.
      DOUBLE PRECISION    :: EffZoMnthSec(12,MaxSectors)  ! effective roughness by month, sector
      DOUBLE PRECISION    :: FchZoMnthSec(12,Maxsectors)  ! fetch for eff roughness by month, sector
          
      contains
      
C=======================================================================
      SUBROUTINE AvgSfcCharsSimple
C
C     Compute simple area-weighted arithmetic averages across the domain 
C     (no sector dependency) for albedo. 
C
C=======================================================================

      IMPLICIT NONE

      INTEGER (kind=4) :: i,j,k ! counter
      
      ModNam = "AvgSfcCharsSimple"
      
C --- For average arrays, the last element represents the 4 seasons 
C     (winter=1, spring=2, summer=3, fall=4) or 12 monts.  If averaging 
C     period is annual, average is stored in index 1, if monthly,
C     there will be 12 monthly averages, if seasonal, averages will be
C     stored in indices 1-4 based on the AERMET mapping of
C     seasons and months:
C      Winter = January, February, December
C      Spring = March, April, May
C      Summer = June, July, August
C      Autumn = September, October, November

    
C ---------------------------------------------------------------------
C     Begin averaging sfc characteristics by season as specified by 
C     the user.  No sector dependence for albedo and Bowen ratio.
C     Do not include cells with land use class marked as outside 
C     boundary (0 in NLCD92) or missing data (99 in NLCD92).
C
C     Current Implementation for Computing Annual Average
C     for each Sfc Characteristic (NLCD92):
C
C     (LC type = Land use Class)
C       1 Get a count of grid cells for each sector where LC type is 
C         not missing or out of bounds (99 or 0 for NLCD92).

C       2 If output is annual or monthly, compute monthly averages
C         by looping over the LC types multiplying the count of each 
C         LC type for the domain by the value of the sfc characteristic for 
C         the season the for which the month is assigned.  Keep a running
C         sum of the computation for each month across all LC types.

C       3 Compute an average for each month by dividing the total
C         computed for the month by the total count of grid cells in the domain
C         (excluding missing or out of bounds)

C       4 If output is to be annual, compute an annual average by summing the 
C         monthly averages and dividing by 12 (number of months). 

C       5 If output is seasonal, loop over 4 seasons (restricted to 4 since
C         all of winter must be considered to have continuous snow or wthout
C         continuous snow.) For each season loop over all
C         LC types multiplying the count of each type by the value
C         of the sfc characteristic for the current season.  Keep
C         a running sum across all LC Types for each season.

C       6 Compute an average seasonal value by dividing the sum for each season
C         by the number of grid cells (exclude missing or out of bounds).
C ---------------------------------------------------------------------

C --- initialize arrays
      AvgAlbedo(:,:) = 0.0D0
      AlbSum(:,:) = 0.0D0

c --- compute monthly averages for both annual and monthly resolution
c --- annual average will then be computed from monthly average

c --- get sum of values for each surface characteristic by month
      IF( TemporalRes == 'A' .OR. TemporalRes == 'M') THEN
c        ignore sectors, treat as one sector
         DO i=1, 1
c           for LCTypes, exclude outside border and missing
            DO j=0, NumLCTypes-1
               DO k=1, 12  ! 12 months/year
                  IF( j /= MissCode )THEN
                     AlbSum(i,k) =
     &                  AlbSum(i,k)+
     &                 (DBLE(LandCoverAlb(i,j))*
     &                  Albedo(j,SeasonMnths(k)))
                  ENDIF
               ENDDO
            ENDDO
         ENDDO
         
c ------ compute monthly averages 
c        ignore sectors; treat as one sector
         DO i=1, 1
            DO j=1, 12 ! months
               AvgAlbedo(i,j) =
     &            AlbSum(i,j)/DBLE(LCCountAlb(i))
            ENDDO
         ENDDO
         
c ------ compute annual average if resolution is annual
         IF( TemporalRes == 'A' )THEN
c           ignore sectors; treat as one sector
            DO i=1, 1
               AlbSum(i,1) = 0.0D0
               DO j=1, 12 ! months
                  AlbSum(i,1) = AlbSum(i,1)+AvgAlbedo(i,j)
               ENDDO
               AvgAlbedo(i,1) = AlbSum(i,1)/12.0D0
            ENDDO
         ENDIF

c     seasonal
      ELSE
c        ignore sectors; treat as one sector
         DO i=1, 1
c           for LCTypes, exclude outside border and missing data
            DO j=0, NumLCTypes-1
               DO k=1, NumSeas
                  IF( j /= MissCode )THEN
                     IF( k == 1 .AND. .NOT. Snow )THEN
                        AlbSum(i,k) =
     &                     AlbSum(i,k)+
     &                     (DBLE(LandCoverAlb(i,j))*Albedo(j,1))
                     ELSE
                        AlbSum(i,k) =
     &                     AlbSum(i,k)+
     &                     (DBLE(LandCoverAlb(i,j))*Albedo(j,k+1))
                     ENDIF
                  ENDIF
               ENDDO
            ENDDO
         ENDDO

C        now compute season averages
c        ignore sectors; treat as one sector
         DO i=1, 1
            DO j=1, NumSeas
               AvgAlbedo(i,j) =
     &            AlbSum(i,j)/DBLE(LCCountAlb(i))
           ENDDO
         ENDDO
      ENDIF


      RETURN
      END SUBROUTINE AvgSfcCharsSimple


      SUBROUTINE AvgSfcCharsGeom
C=======================================================================
C     
C     Compute geometric means for Bowen ratio and surface roughness.
C     Bowen ratio based on single area-weighted average across the domain 
C     (no sector dependency); surface roughness incorporates sector-
C     dependency and inverse-distance weighting.
C
C     Use sector-dependent inverse-distance weighted average within 
C     user-defined radius for OPTION ZORAD. For OPTION ZOEFF, use IBL 
C     effective roughness method.
C
C=======================================================================
 
      use UserParams, only: WndSectors, tmpStr, tmpStr2, Airport, VaryAP

      use FileUnits, only: LogUnt
    
      IMPLICIT NONE

      DOUBLE PRECISION  :: Zo_DistProfile(NumRings), Z0_Eff, Z0_Fch  ! zo by distance, eff zo, fetch

      DOUBLE PRECISION  :: AvgZo_Fetch   ! Average Z0 Fetch for use in land cover counts by month and sector

      INTEGER (kind=4)  :: NumMnths(NumSeas) ! number of months assigned to each of four seasons
      
      INTEGER (kind=4) :: i, j, k, l, m ! counters
 
C     IBL Height - actual, max limit, min limit as strings      
      CHARACTER (len=10) ::  IBLHtStr, IBLMinStr, IBLMaxStr

      ModNam = 'AvgSfcCharsGeom'
     
C --- initialize arrays
      NumMnths(:) = 0
      EffZoMnthSec(:,:) = 0.0D0
      FchZoMnthSec(:,:) = 0.0D0
      AvgZo(:,:)     = 0.0D0
      AvgBowen(:,:)  = 0.0D0

C --- For average arrays, the last element represents the 4 seasons 
C     (winter=1, spring=2, summer=3, fall=4) or 12 months.  If averaging 
C     period is annual, average is stored in index 1, if averaging
C     is annual, average is stored in index 1, if monthly,
C     there will be 12 monthly averages, if seasonal, averages will be
C     stored in indices 1-4 based on the AERMET mapping of
C     seasons and months:
C      Winter = January, February, December
C      Spring = March, April, May
C      Summer = June, July, August
C      Autumn = September, October, November


C     Compute surface roughness, Zo, using IBL effective roughness method, 
C     when Zo_Method = 'ZOEFF'. Use inverse distance-weighted method 
C     based on user radius when Zo_Method = 'ZORAD'.

      SELECT CASE (Zo_Method)
      CASE ('ZOEFF')

C        Compute IBL_Height if anemometer valid flag is true
C        IBL factor is set in module 
         IBL_Height = Anem_Height*IBL_Factor
         
C        set IBL Ht, Min limit, and max limits as strings for output      
         WRITE(IBLHtStr,'(f10.1)') IBL_Height
         WRITE(IBLMinStr,'(f10.1)') IBL_Min
         WRITE(IBLMaxStr,'(f10.1)') IBL_Max
         
C        Check for minimum and maximum IBL Height, reset and print 
C        warning if under minimum or over maximum
         IF (IBL_Height < IBL_Min) THEN
            CALL ERRHDL('',ModNam,'W','227','Set IBL = '//
     &                    trim(adjustl(IBLMinStr))//'m',ILINE)
         
            WRITE(LogUnt,50) trim(adjustl(IBLHtStr)),IBL_Factor,
     &                       trim(adjustl(IBLMinStr)),
     &                       trim(adjustl(IBLMinStr))
            WRITE(*,50)      trim(adjustl(IBLHtStr)),IBL_Factor,
     &                       trim(adjustl(IBLMinStr)),
     &                       trim(adjustl(IBLMinStr))
         
            IBL_Height = IBL_Min
            
         ELSEIF (IBL_Height > IBL_Max) THEN
            CALL ERRHDL('',ModNam,'W','228','IBL Set = '//
     &                    trim(adjustl(IBLMaxStr))//'m',ILINE)
         
            WRITE(LogUnt,52) trim(adjustl(IBLHtStr)),IBL_Factor,
     &                       trim(adjustl(IBLMaxStr)),
     &                       trim(adjustl(IBLMaxStr))
            WRITE(*,52)      trim(adjustl(IBLHtStr)),IBL_Factor,
     &                       trim(adjustl(IBLMaxStr)),
     &                       trim(adjustl(IBLMaxStr))
         
            IBL_Height = IBL_Max
         
         END IF
         
   50    FORMAT("WARNING: The calculated IBL height (",a," m),"/,
     &        "computed as the Anemometer Height X ",f4.1,", is"/,
     &        "LESS THAN the minimum allowed height (",a," m).",/
     &        "The IBL height was reset to ",a," m.")
         
   52    FORMAT("WARNING: The calculated IBL height (",a," m),"/,
     &        "computed as the Anemometer Height X ",f4.1,", is"/,
     &        "GREATER THAN the maximum allowed height (",a," m).",/
     &        "The IBL height was reset to ",a," m.")
           
C ---    compute monthly averages 
         DO i=1, NumSectors
            DO j=1, 12 ! months
         
               Zo_DistProfile(:) = 0.0D0
         
               DO k=1, NumRings ! distance range
         
C ---          Average roughness is the sum of the Zo values for each cell
C              (weighted by distance) divided by the sum of 1/distance
               if (InvDSumZoEff(i,j,k) .lt. 0.0001) then
C ---             InvDSumZoEff is zero (or near zero), probably due to no pixels within
C                 sector for this distance range for k=1.  Cycle to next distance range.
C                 Value of 0.0 for k=1 will be adjusted below to match value for k=2.
                  cycle
               else
                  Zo_DistProfile(k) =
     &            EXP(ZoSumEff(i,j,k)/InvDSumZoEff(i,j,k))
               end if
              
               END DO
               
C ---          Check for "unassigned" values for k=1 (set to 0.0 above).
               DO l=1,SIZE(Zo_DistProfile)
                  IF (Zo_DistProfile(l) .lt. 0.0001) THEN
                     DO m=l,SIZE(Zo_DistProfile)
                        IF (m+1 .lt. SIZE(Zo_DistProfile)) THEN
                           IF (Zo_DistProfile(m+1) .ge. 0.0001D0) THEN
                           
                              Zo_DistProfile(l) = Zo_DistProfile(m+1)
         
                              exit
                           END IF
                        END IF
                     END DO
                  END IF
               END DO
         
C ---          Check for "unassigned" values for k=1 (set to 0.0 above).
c               IF (Zo_DistProfile(1) .lt. 0.0001D0) THEN
cC                 Assign value for k=2 for this sector/month
c                  Zo_DistProfile(1) = Zo_DistProfile(2)
c               END IF

               CALL EffectiveRoughness4(Zo_DistProfile,IBL_Height,
     &                                  Z0_Eff,Z0_Fch,i,j,RingWidth,
     &                                  NumRings)


CCRT           Store computed effective roughness and fetch values     
               EffZoMnthSec(j,i) = Z0_Eff
               FchZoMnthSec(j,i) = Z0_Fch
               
               AvgZo(i,j) = Z0_Eff
               
C***********   This code is duplicated for both cases, need to clean up
C ---          Consider only 1 sector for bowen ratio
               IF( i == 1 )THEN
         
C ---             Average Bowen ratio is the sum of the B values for each cell
C                 divided by the total number of cells
                  AvgBowen(i,j) =
     &               BowenSum(i,j)/LCCountBr(i)
         
                  AvgBowen(i,j) = EXP(AvgBowen(i,j))
             
               ENDIF
             
            ENDDO
         ENDDO
         
C ---    Calculate "average" fetch distance to include in log file; note that "actual"
C        fetch distance varies by sector and month.
         AvgZo_Fetch = 0.0D0

         DO i=1, NumSectors
            DO j=1, 12 ! months
               AvgZo_Fetch = AvgZo_Fetch + FchZoMnthSec(j,i)
            ENDDO
         ENDDO

C ---    Average fetch across all months and sectors for debug table
         AvgZo_Fetch = AvgZo_Fetch/(12.0D0*DBLE(Numsectors))

      CASE('ZORAD')
      
C ---    compute monthly averages 
         DO i=1, NumSectors
            DO j=1, 12 ! months
         
C ---          Average roughness is the sum of the Zo values for each cell
C              (weighted by distance) divided by the sum of 1/distance
               AvgZo(i,j) =
     &           ZoSumRad(i,j)/InvDSumZoRad(i,j)
         
               AvgZo(i,j) = EXP(AvgZo(i,j))
               
               EffZoMnthSec(j,i) = AvgZo(i,j)
               
C*****   ***   This code is duplicated for both cases, need to clean up        
C ---           Consider only 1 sector for bowen ratio
                IF( i == 1 )THEN
         
C ---             Average Bowen ratio is the sum of the B values for each cell
C                 divided by the total number of cells
                  AvgBowen(i,j) =
     &               BowenSum(i,j)/DBLE(LCCountBr(i))
         
                  AvgBowen(i,j) = EXP(AvgBowen(i,j))
             
                ENDIF
             
            ENDDO
         ENDDO
C ---    Set fetch variables used for eff roughness method to 
C        user defined radius for output of month-sector values in
C        log file
         FchZoMnthSec = ZoRad    
         AvgZo_Fetch  = ZoRad        

      END SELECT

C --- Reinitialize sum arrays for reuse       
      ZoSum    = 0.0D0
      BowenSum = 0.0D0
         
C --- if resolution is SEASONAL, compute seasonal averages
      SELECT CASE(TemporalRes)

C     if resolution is seasonal
      CASE("S")
         DO i=1, NumSectors
            DO j=1, 12 ! months
               IF( SeasonMnths(j) == 1 .OR.   
     &          SeasonMnths(j) == 2 )THEN
                  
                  IF( i == 1 )THEN
                     NumMnths(1) = NumMnths(1)+1
                  ENDIF
     
                  ZoSum(i,1) = ZoSum(i,1) +
     &             AvgZo(i,j)

C ---             Consider only 1 sector for bowen ratio
                  IF( i == 1 )THEN     
                     BowenSum(i,1) = BowenSum(i,1) +
     &                AvgBowen(i,j)
                  ENDIF
     
               ELSE
                  IF( i == 1 )THEN
                     NumMnths(SeasonMnths(j)-1) = 
     &               NumMnths(SeasonMnths(j)-1) + 1
                  ENDIF
     
                  ZoSum(i,SeasonMnths(j)-1) = 
     &            ZoSum(i,SeasonMnths(j)-1) + AvgZo(i,j)

C ---             Consider only 1 sector for bowen ratio
                  IF( i == 1 )THEN      
                     BowenSum(i,SeasonMnths(j)-1) = 
     &               BowenSum(i,SeasonMnths(j)-1) + AvgBowen(i,j)
                  ENDIF
               ENDIF
            ENDDO
         ENDDO

C ---    Reinitialize average arrays for reuse
         AvgZo    = 0.0
         AvgBowen = 0.0
         
         DO i=1, NumSectors
            DO j=1, NumSeas     
               AvgZo(i,j) = ZoSum(i,j)/DBLE(NumMnths(j))
C ---          Consider only 1 sector for bowen ratio
               IF( i == 1 )THEN          
                  AvgBowen(i,j) = BowenSum(i,j)/DBLE(NumMnths(j))
               ENDIF
            ENDDO 
         ENDDO

C     if resolution is annual         
      CASE("A")
         DO i=1, NumSectors
            DO j=1, 12 ! months
               ZoSum(i,1) = ZoSum(i,1)+AvgZo(i,j)
C ---          Consider only 1 sector for bowen ratio
               IF( i == 1 )THEN          
                  BowenSum(i,1) = BowenSum(i,1)+AvgBowen(i,j)
               ENDIF
            ENDDO          

            AvgZo(i,1) = ZoSum(i,1)/12.0D0
            
C ---       Consider only 1 sector for bowen ratio
            IF( i == 1 )THEN          
               AvgBowen(i,1) = BowenSum(i,1)/12.0D0
            ENDIF
         ENDDO
         
      END SELECT 


      IF (VaryAP) THEN
C ---    Assign Non-Airport sector IDs to string
         tmpStr = ""
         j = 0
         DO i=1,numSectors
            IF (.NOT.WndSectors%sec_ap(i)) THEN
               j = j + 1
               WRITE(tmpStr2,'(i2)') i
               IF (i==1) THEN
                  tmpStr = trim(adjustl(tmpStr2))
               ELSE
                  tmpStr = trim(adjustl(tmpStr))//" "
     &                   //trim(adjustl(tmpStr2))
               END IF
            END IF
         END DO
         IF (LEN_TRIM(tmpStr) .EQ. 0) THEN
C ---       No NonAP sectors found
            tmpStr = "None"
         ELSE IF (j .EQ. numSectors) THEN
C ---       All sectors are NonAP
            tmpStr = "All"
         END IF
      ELSE IF (.NOT.Airport ) THEN
         tmpStr = "All"
      ELSE
         tmpStr = "None"
      END IF 
      
      
C --- Write abbreviated effective roughness and fetch info to log file

CRLM O013_AirportFlag_WSP Begin      
      WRITE(LogUnt,53,err=910) trim(adjustl(tmpStr))
   53 format('** High Z0 (Non-Airport) Sector IDs: ',a)
CRLM   53 format('** Non-Airport Sector IDs: ',a)      
CRLM O013_AirportFlag_WSP Begin
      
      WRITE(LogUnt,'(/a)')" Roughness (m) by Month and Sector"
      WRITE(LogUnt,101) ("----------",i=1,12)
      
      WRITE(LogUnt,'(7x,a,49x,a)') "Start", "Month"
      WRITE(LogUnt,102) (i,i=1,12)
      
  101 FORMAT (12a)
  102 FORMAT ("Sector  Dir ",12(i7,2x))
      
      DO i=1,NumSectors
         write(LogUnt,103) i, StartDir(i), (EffZoMnthSec(j,i),j=1,12)
      END DO
           
  103 FORMAT (2x,i2,3x,f5.1,12(f9.3))
      
      WRITE(LogUnt,101) ("----------",i=1,12)


      WRITE(LogUnt,'(//a)')" Roughness Fetch (m) by Month and Sector"
      WRITE(LogUnt,101) ("----------",i=1,12)
      WRITE(LogUnt,'(7x,a,49x,a)') "Start", "Month"
      WRITE(LogUnt,102) (i,i=1,12)
    
     
      DO i=1,NumSectors
         write(LogUnt,108) i, StartDir(i), (FchZoMnthSec(j,i),j=1,12)
      END DO
       
  108 FORMAT (2x,i2,3x,f5.1,12(f9.1))
  
      WRITE(LogUnt,101) ("----------",i=1,12)

C --- Write "average" roughness fetch to Log file
      WRITE(LogUnt,*) ''
      WRITE(LogUnt,'(a,1x,f7.1)') " Average Roughness Fetch (m) = ", 
     &                                         AvgZo_Fetch


      RETURN

C     WRITE Error Message:  Error Writing to File
C 350  WRITE(*,351) trim(LogFile)
C 351  FORMAT(/,' Error Writing to Log File: ',a,//,
C     &         ' Processing Aborted!')

910   CONTINUE

      STOP

      END SUBROUTINE AvgSfcCharsGeom


C=======================================================================      
      subroutine EffectiveRoughness4(Z0,href,z0_eff_fnl,fetch,indx,jndx,
     &                               delx, xdnum)
C This porgram calculates the effective roughness length given 
C an array of roughness lengths, z0, as a function of distance, xd, from 
C measurement location
C The wind speed is measured at zref
C Uses Miyake's model described in Wieranga (BLM, 63, 323-363,1993)
C Solves k*x+href*average(log(z0))-average(z0)=href*(log(href)-1);
C
C Procedure is documented in "Effective Roughness" MCA developed 
C by Akula Venkatram
C
C Adapted from MATLAB code developed by Dr. Akula Venkatram
C
C=======================================================================

      implicit none

      integer         J
      
      double precision, dimension(:),  intent(in)   :: Z0          ! roughness array - from tower outward
      double precision,                intent(in)   :: href        ! ibl reference height
      double precision,                intent(out)  :: z0_eff_fnl  ! final roughness value
      double precision,                intent(out)  :: fetch       ! fetch
      integer (kind=4),    intent(in)   :: indx        ! array index
      integer (kind=4),    intent(in)   :: jndx        ! array index
      double precision,    intent(in)   :: delx        ! ring width
      integer (kind=4),    intent(in)   :: xdnum       ! number of rings
      
      double precision    :: k         ! von Karman constant
      double precision    :: z0_avg    ! average roughness value           
      double precision    :: xrad      ! radial distance from tower where ibl ht = ref ht

      double precision    :: hb_old    ! tmp ibl ht variable
      double precision    :: hb_new    ! tmp ibl ht variable
      double precision    :: z0_in     ! derived roughness going inward toward tower
      double precision    :: z0_out    ! derived roughness going outward from tower
              
      double precision    :: err       ! error limit
      double precision    :: aa        ! aa = hb(i)*(log(hb(i)/z0_avg)-1)+k*delx
      double precision    :: slope     ! slope between two ibl hts at two points
      double precision    :: href_tmp  ! temp ibl reference height (local variable)

      double precision, allocatable  :: xd(:)    ! ring distance array - from tower outward 
      double precision, allocatable  :: hb(:)    ! ibl ht array - from tower outward
      double precision, allocatable  :: z0_r(:)  ! roughness array - inward toward tower
      double precision, allocatable  :: hb_r(:)  ! ibl ht array - use inward toward tower

      integer (kind=4) i,iter, iterlimit ! counters
      
      integer (kind=4) :: irngndx, orngndx  ! inner outer ring index for final eff roughness
      integer (kind=4) :: startndx  ! distance index for first calc when working toward the tower

      double precision    :: hb0               ! ibl height at the tower for current iteration
      double precision    :: ornghb0, irnghb0  ! ibl height at tower for inner and outer ring when defined
      double precision    :: xradfract         ! fractional distance of final xrad between inner and outer rings
      
      logical :: endofdom          ! reached end of domain - during while working from tower outward
      
      logical :: gotirng, gotorng  ! got inner/outer ring distance
      logical :: gotfnlxrad        ! got final radial distance from tower
      
      logical :: l_dbgopn  ! flag for opened debug file

      
C     initialize logical flags     
      gotirng = .false.
      gotorng = .false.
      gotfnlxrad = .false.
      endofdom = .false.

C     Initialize constants and limits 
      k = 0.4D0          ! von Karman constant        
      iterlimit = 10     ! iteration limit
      err = 10.0D0       ! error limit

C     intitialize local variable for IBL ref height 
C     so original value is not overwritten
      href_tmp = href
      
C     allocate arrays
      allocate(xd(xdnum)) 
      allocate(hb(xdnum))
      allocate(z0_r(xdnum))
      allocate(hb_r(xdnum)) 
  
C     Inquire if dbg file is opened, open if needed
      if (EffRadDbg) then   
         inquire(unit=EffRadUnt,opened=l_dbgopn)  
         if (.not. l_dbgopn) then  
            open(EffRadUnt,file=EffRadFile,status='unknown',err=920)

            write(EffRadUnt,45) 'AERSURFACE EFFRAD Debug File, ',
     &        'Version ',trim(Version),run_start(2),run_start(3),iYr2,
     &       run_start(5),run_start(6),run_start(7)
     
   45       format('** ',3a,t67, i2.2,'/',i2.2,'/',i2.2,' **'/
     &        '**',t67,i2.2,':',i2.2,':',i2.2,' **'/ )

            write(EffRadUnt,46) Anem_Height, IBL_Factor
   46       format('Anemometer Height = ',f5.1,'m;',
     &        'IBL Factor = ',f5.1/ )

             write(EffRadUnt,'(" *************************** " /)',
     &        err=8000)

        endif

      endif


C     initialize distance array
      xd(1) = delx  
      do i = 2, xdnum
         xd(i) = xd(i-1) + delx
      end do
      

C -------------------------------------------------------------------
C     Work from tower location outward to get intial value for xrad
C -------------------------------------------------------------------
    
C     Initialize counter for integration loop
      i=1

      z0_avg = sqrt(z0(i)*z0(i+1))         ! geometric average of z0

C     Initialize boundary layer heights for each distance ring
C     Set boundary layer height at starting point to first roughness
C     Set all subsequent heights to 2 x the max roughness in the roughness array
      hb(1) = z0(1)                 
      do j = 2, xdnum
         hb(j) = 2.0D0*maxval(z0(1:j))

      enddo

C --- Perform integration to determine effective roughness and fetch;
C     exit loop once LHS exceeds RHS, or if distance limit is reached
      do while ((hb(i)<=href_tmp) .and. .not. endofdom)

C       Integration of dh/dx, where h=ibl ht (i.e., hb) and 
C       dx = distance (i.e., delx)
        aa=hb(i)*(log(hb(i)/z0_avg)-1.0D0)+k*delx

C       initialize iterative counter        
        iter=1

c       Save next ibl height        
        hb_old=hb(i+1)

c       if no convergence, compute new ibl height    
        do while ((err>1.0e-03) .and. (iter<iterlimit))

c           New ibl height    
            hb_new=(hb_old+aa)/log(hb_old/z0_avg)

c           Compute difference between old and new ibl values 
            err=abs((hb_new-hb_old)/hb_old)

c           Save new ibl       
            hb_old=hb_new

c           Track iteration
            iter=iter+1

        end do

c       Increment counter
        i=i+1
      
c       if distance reached the extent of the domain,
c       set logical endofdom = true, and abbreviate step two
c       since working back toward the tower will likely always
c       overshoot the tower
        if (i==size(xd)-1) then
            endofdom = .true.  
            
C           set ibl reference height to last computed height
            href_tmp = hb_new
            exit
            
        else
        
            hb(i)=hb_new

            z0_avg = sqrt(z0(i)*z0(i+1))  ! use geometric mean of z0
            
C           initialize error limit
            err=10.0D0
         
        end if
    
      end do

C     if reached edge of domain, set radial distance 
      if (endofdom) then
      
C        set xrad to full domain fetch
         xrad = xd(i+1)       

      else
      
C       compute slope of line
        slope = (xd(i)-xd(i-1))/(hb(i)-hb(i-1))
  
C       radial distance from tower where ibl = href 
        xrad = xd(i-1)+slope*(href_tmp-hb(i-1))
  
      end if

C     compute z0, going out from the tower  
      z0_out = z0_eff_func(minval(z0(1:i)),xrad,href_tmp)  

C ----------------------------------------------------------------------
C     Use xrad as an approximate fetch and work toward the tower
C       to derive final effective roughness.
C
C     Start at ring just beyond xrad. Work toward tower to find ibl at 
C       tower based on initial starting point.  After first iteration, 
C       if ibl > href at tower, move in one ring.  If ibl < href, move
C       out one ring.
C     Continue to move in or out until the two consecutive rings are 
C       found in which some point in between will produce ibl = href
C       at the tower.
C     Once the two rings are found, use the distance for the midpoint
C       between the two rings to compute effective roughness.      
C ----------------------------------------------------------------------   

C     initialize start distance based on current xrad 
      startndx = i
      
C     initialize ibl computed for inner and outer rings
      ornghb0 = 0.0D0
      irnghb0 = 0.0D0

C     loop until final radial distance is determined
      do while (.not. gotfnlxrad)

C        reverse arrays with the first element equal to last ring computed
         do i=1,startndx+1
            z0_r(i) = z0(startndx-i+2)
         end do

C        if edge of domain encountered, set final radial distance and 
C        flag, exit loop
         if (endofdom) then
            gotfnlxrad = .true.
            xrad = xd(startndx+1)
            exit
         end if
          
C        initialize ibl height at start distance or ring
         hb_r(1) = z0_r(1) 

C        initialize ibl for subsequent inner distances or rings
         hb_r(2:startndx+1) = 2.0D0*maxval(z0_r(1:startndx+1))
          
C        work toward tower 
         do i=1,startndx

C           compute average roughness for each ring          
            z0_avg = sqrt(z0_r(i)*z0_r(i+1))  ! geometric average - Brode
            
C           Integration of dh/dx, where h=ibl ht (i.e., hb) and 
C           dx = distance (i.e., delx)   
            aa=hb_r(i)*(log(hb_r(i)/z0_avg)-1.0D0)+k*delx
            
C           initialize iteration counter  
            iter=1

C           initialize error limit         
            err=10.0D0

C           store ibl height         
            hb_old=hb_r(i+1)

C           iterate until convergence or iteration limit encountered    
            do while ((err>1.0e-03) .and. (iter<iterlimit))

C              compute new ibl ht      
               hb_new=(hb_old+aa)/log(hb_old/z0_avg)

C              compute error      
               err=abs((hb_new-hb_old)/hb_old)

C              save new ibl ht         
               hb_old=hb_new

C              increment counter         
               iter=iter+1
         
            end do

C           if not yet reached start index, prepare for another iteration  
            if (i<=startndx) then
               hb_r(i+1)=hb_new
            end if
            
         end do

C        set ibl ht         
         hb0 = hb_r(startndx+1)

C        if computed ibl ht equals (or very close to) reference ibl height
C        set final distance flag and final radial distance
         if (abs(hb0-href_tmp) < 0.01D0) then
            gotfnlxrad = .true.  ! xrad is the same as current ring distance
            xrad = xd(startndx+1)

C        if computed ibl ht less or greater than reference ibl height,
C        move back or move up a ring and try again unless both distances
C        have already be tried. If so, compute radial distance based on proportion
         elseif (hb0 < href_tmp) then
            gotirng = .true.
            irngndx = startndx
            irnghb0 = hb0
            if (gotirng .and. gotorng) then
               xradfract = (href_tmp-irnghb0)/(ornghb0-irnghb0)
               xrad = xd(irngndx) + (xradfract*delx)
               gotfnlxrad = .true.
            else
               startndx = startndx+1
            end if
         
         elseif (hb0 > href_tmp) then
            gotorng = .true.
            orngndx = startndx
            ornghb0 = hb0
            if (gotirng .and. gotorng) then
               xradfract = (href_tmp-irnghb0)/(ornghb0-irnghb0)
               xrad = xd(orngndx) + (xradfract*delx)
               gotfnlxrad = .true.
            else
               startndx = startndx-1
            end if
         end if

      end do

C     compute effective rougness working toward tower
      z0_in = z0_eff_func(minval(z0_r(1:startndx+1)),xrad,href_tmp)

C     compute final effective roughness from 2 roughness values
C     (working outward from tower and inward toward tower)      
      z0_eff_fnl = sqrt(z0_in*z0_out)
      
C     compute fetch   
      fetch=(href_tmp*log(href_tmp/z0_eff_fnl)-href_tmp+z0_eff_fnl)/k
      
C --- Output results if debug flag set by user
      if (EffRadDbg) then
         write(EffRadUnt,222,err=900) indx, jndx
222      format(' Sector: ',i2,'  Month: ',i2)
         write(EffRadUnt,*,err=900)
         write(EffRadUnt,*,err=900)  'Boundary Layer Height (m) = ', 
     &      href_tmp
         write(EffRadUnt,*,err=900)  
         write(EffRadUnt,*,err=900)  'Average Roughness (m)     = ', 
     &        z0_avg
         write(EffRadUnt,*,err=900)  'Upwind Influence Dist (m) = ', 
     &        xrad
         write(EffRadUnt,*,err=900)  'Estimated Fetch Dist (m)  = ', 
     &        fetch
         write(EffRadUnt,*,err=900)  
     &       'Effective Roughness From Tower (m)   = ', z0_out
         write(EffRadUnt,*,err=900)  
     &       'Effective Roughness Toward Tower (m) = ', z0_in
         write(EffRadUnt,*,err=900)  
     &       'Final Effective Roughness (m)        = ', z0_eff_fnl
         write(EffRadUnt,*,err=900)
         write(EffRadUnt,*,err=900)  ' Dist (m)     z0 (m) '
         write(EffRadUnt,*,err=900)  '---------   ---------'
         do i = 1,NumRings
            write(EffRadUnt,234,err=900)  xd(i), z0(i)
234         format(1x,f8.3,3x,f8.3)
         end do
            write(EffRadUnt,248,err=900) xd(NumRings), z0(NumRings)
248         format(1x,f8.3,3x,f8.3)
        end if

      return
      
  900 write (LogUnt,910) EffRadFile
      Write (OutUnt,910) EffRadFile
      write (*,910) EffRadFile
  910 format(/,' There was a problem writing to the effective',
     &         ' roughness debug file: ',/,a,//,
     &         ' Processing aborted.',/)
      stop
      
  920 write (LogUnt,910) EffRadFile
      Write (OutUnt,910) EffRadFile
      write (*,910) EffRadFile
c  930 format(/,' There was a problem opening the effective',
c     &         ' roughness debug file: ',/,a,//,
c     &         ' Processing aborted.',/)
      stop
     
c     problem writing to log file      
 8000 continue  
      write(LogUnt,8005) EffRadFile(1:LEN_TRIM(EffRadFile))
      write(OutUnt,8005) EffRadFile(1:LEN_TRIM(EffRadFile))
 8005 format(/,' There was a problem writing to the file:'
     &    /,' ',a,//' Processing was aborted.',/)
C ---     
      stop

      end subroutine EffectiveRoughness4
      


C=======================================================================
      double precision function z0_eff_func(z0_init,xrad,href)
C
C     Compute effective roughness using new value for xrad
C
C=======================================================================      

      double precision, intent(in)   :: z0_init  ! initial roughness
      double precision, intent(in)   :: xrad     ! radial distance
      double precision, intent(in)   :: href     ! ibl reference height
      
      double precision z0_old, z0_new  ! old and new roughness values (tmp storage)
      double precision err, vonk       ! convergence error limit, Von Karman constant
      integer (kind=4) iter, iterlimit  ! iteration counter and limit
      
c      % Iterate for z0_eff

C     Initialize parameters 
      vonk = 0.4D0            ! von Karman constant        
      iterlimit = 10      
      err = 10.0D0

C     intialize iteration counter and store initial roughness value      
      iter=1
   
      z0_old = z0_init
      
C     loop until convergence limit or iteration limit encountered   
      do while ((err>=1.0e-03) .and. (iter<iterlimit))

C      compute new roughness value       
       z0_new=(href*log(href/z0_old)-vonk*xrad)/(href/z0_old-1.0D0)

C      compute convergence       
       err=abs((z0_new-z0_old)/z0_old)

C      store new roughness value
       z0_old=z0_new

C      increment iteration counter
       iter=iter+1     
         
      end do

C     set effective roughness as return variable      
      z0_eff_func=z0_new

      return
      
      end function z0_eff_func

      END MODULE AvgParams