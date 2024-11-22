MODULE met_fields
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     This module contains dynamic allocation subroutines.
!
!     Development History:
!     2009-05-26  Original Development (ENVIRON International Corp.)
!     2011-09-30  Major re-write to support more output formats.
!     2012-03-14  Make sure we don't allocate more than once.
!     2012-01-17  Moved zface2 and avg_zface to their respective routines.
!     2013-05-01  Implement AERMOD minimum wind speed, following AERMET v12345.
!     2013-05-02  Added fixed-control-file-format supporting multiple outs.
!     2013-07-12  For aggregation, use zPt(,) instead of zmid(), because the 
!                 latter is the average over the full 3-D domain.
!     2013-09-28  Added support the each 1-D output to have its own timezone.
!     2014-09-18  Add optional minimum mixing height and abs(L) in AERMOD modes.
!     2014-10-09  Bug fix: only first AERSFC output file contained values.
!     2016-03-17  Moved PGtype, CalcPBL here for consistency, added AER_MIXHT
!     2017-06-20  New output: BLN, BNA, or KML of 3D sub-domain.
!     2018-12-26  Read WRF's CLDFRA output and use for cldcvr if WRFv3.6 or newer.
!     2021-06-24  Add ability to use WRF's hybrid vertical coordinate
!     2021-09-28  Adapted for new overhauled version of AERMET with over
!                 water processing (keyword aer_use_NEW).
!
!------------------------------------------------------------------------------

!
!-----Declarations for Outputs (gridded or point [1x1 grid])
!
  integer   :: NumOuts                         ! Number of output files reqested
  integer   :: nx,ny,nz,nzOut                  ! OUTPUT grid dimensions
  integer   :: ijlatlon                        ! IJ, LL, or LCC subset type flag
  integer   :: iVertMap                        ! 0=aggregate, 1=ZFACE, 2=ZMID
  integer   :: iOver                           ! global over 0=auto, 1=land, 2=water
!
!-----Variables read from the user-supplied mmif.inp file, domain sub-setting
!
  integer   :: iBeg,iEnd,jBeg,jEnd             ! IJ subsetting variables
  real      :: BegLat,EndLat,BegLon,EndLon     ! LATLON subsetting variables
  real      :: BegXlcc,EndXlcc,BegYlcc,EndYlcc ! LATLON subsetting variables
  real      :: x0met,y0met,xfmet,yfmet         ! sub-grid extents (km)
  real      :: xWwrf,ySwrf,xEwrf,yNwrf         ! full wrf grid extents (km)
  real, dimension(4,2) :: wrfgrid_latlon       ! full grid corners
  ! The four corners (clockwise from the SW corner), 2nd index is lat(,1) lon(,2)
  real, dimension(4,2) :: subgrid_latlon       ! sub-grid corners
  ! The four corners (clockwise from the SW corner), 2nd index is lat(,1) lon(,2)
!
!-----Variables from the WRF files
!
  character (len=3)   :: metform = "duh"       ! which input: WRF or MM5?
  character (len=8)   :: datum                 ! Datum (NWS-84 or WGS-84)
  character (len=8)   :: pmap                  ! LCC, PS, or EM
  character (len=25)  :: lucat                 ! Which Land-Use dataset was used?
  integer   :: iProj                           ! WRF/MM5 Projection number
  integer   :: ihyb                            ! WRF hybrid coordinate flag
  real      :: stdlat,stdlon,tlat1,tlat2       ! Projection definition
  real      :: conefact                        ! Cone factor
  real      :: cenlat,cenlon                   ! Coarse grid central lat-lon
  real      :: origlat,origlon                 ! Use to change LCC output origin 
  real      :: deltax                          ! MM5/WRF grid resolution (km)
!
!-----Declarations for MM5/WRF fields
!
  integer :: irlg,ndathr,ibtz          ! run length, timestamp, time zone
  integer :: ibyr,ibmo,ibdy,ibhr,ibsec ! beginning time variables
  integer :: ieyr,iemo,iedy,iehr,iesec ! ending    time variables
  integer :: num_land_cat              ! Number of land-use categories
  integer :: nsoil                     ! Number of (staggered) soil layers in WRF
  integer :: water_cat                 ! WRF Water land-use category
  integer :: lake_cat                  ! WRF Lake  land-use category
  integer :: ice_cat                   ! WRF Ice   land-use category

  real :: ptop,p0,ps0,tlp,ts0          ! MM5 vertical information
  real, allocatable, dimension(:,:,:)  :: ustag,vstag, hold3d
  real, allocatable, dimension(:,:,:)  :: uu,vv,ww,tt,qq,qcloud,qice,cldfra
  real, allocatable, dimension(:,:,:)  :: zh,ph,phb,pa,pab,psax
  
  real, allocatable, dimension(:,:)    :: hold2d,ustar,cosalpha,sinalpha
  real, allocatable, dimension(:,:)    :: ps,psb,pbl,aerpbl,pbl_last
  real, allocatable, dimension(:,:)    :: topo,tsfc,qsw,qlw,rlu,trn
  real, allocatable, dimension(:,:)    :: ylat,xlon,ylatU,xlonU,ylatV,xlonV
  real, allocatable, dimension(:,:)    :: rainro,rainco,rainr,rainc
  
  real, allocatable, dimension(:)      :: sigma,sigmid
  real, allocatable, dimension(:)      :: c1h,c2h
!
!-----Declarations for OUTPUT fields, see below for what each variable represents
!
  logical, allocatable, dimension(:,:) :: iswater    ! water_cat or lake_cat
  integer, allocatable, dimension(:,:) :: ilu,ipgt   ! landuse, PG stability class

  real, allocatable, dimension(:,:,:)  :: uOut,vOut,wOut,pOut,tOut,qOut,qcOut
  real, allocatable, dimension(:,:,:)  :: psTmp
  real, allocatable, dimension(:,:)    :: u10,v10,t10,sst,t2,q2,psfc
  real, allocatable, dimension(:,:)    :: wstar,mol,z0,albedo,bowen,lai,smois
  real, allocatable, dimension(:,:)    :: rain,cldcvr,shflux,lhflux
  real, allocatable, dimension(:)      :: ulev,vlev,tlev,plev,qlev
  real, allocatable, dimension(:)      :: zm         ! m, middle of INPUT layers
  real, allocatable, dimension(:)      :: zface,zmid ! m, defines  OUTPUT layers
  real, allocatable, dimension(:)      :: zfaceC     ! for CALMET  OUTPUT layers
  real, allocatable, dimension(:)      :: zfaceS     ! for SCICHEM OUTPUT layers
  real, allocatable,dimension(:,:,:,:) :: aersfc     ! albedo,bowen,z0 AERSURFACE
!
!-----Variables to control outputs, mostly for AER* modes
!
  integer, allocatable, dimension(:,:) :: related_out ! iOut of related files
  integer, allocatable, dimension(:,:) :: nzPt ! 1D low and high points to output
  real,    allocatable, dimension(:,:) :: zPt  ! zh() at each ijPT for AER* output
  integer, allocatable, dimension(:) :: ijPt   ! like ijlatlon, but for AER*
  integer, allocatable, dimension(:) :: PtZone ! like ibtz, but for AER*
  integer, allocatable, dimension(:) :: PtOver ! over 0=auto/detect, 1=land, 2=water
  integer, allocatable, dimension(:) :: iPt,jPt! location for AER* output
  real,    allocatable, dimension(:) :: PtLat,PtLon   ! locations for AER* output
  real,    allocatable, dimension(:) :: PtXlcc,PtYlcc ! locations for AER* output
  character (len=9),  allocatable, dimension(:) :: OutForm  ! Output format
  character (len=15), allocatable, dimension(:) :: OutType  ! Output type
  character (len=256),allocatable, dimension(:) :: OutFile  ! Output filenames
  character (len=256),allocatable, dimension(:) :: METfile  ! Input  filenames
  character (len=5),  dimension(0:2)            :: OverWhat ! 0=auto 1=land 2=water
  character (len=6)   :: PGtype           ! SRDT or GOLDER, 
  character (len=6)   :: CalcPBL          ! WRF  or MMIF
  character (len=6)   :: aer_mixht        ! WRF,MMIF,AERMET (for AERMET mode)
  character (len=8)   :: CloudCover       ! WRF,ANGEVINE,or RANDALL (source)
  logical, allocatable, dimension(:) :: aer_use_BULKRN  ! use bulk Richardson method
  logical   :: aer_use_TSKC = .false.     ! T/F to use clouds (total sky cover)
  logical   :: aer_use_NEW  = .false.     ! T/F to use new/old AERMET version
  logical   :: pdebug = .false.           ! print debugging statements
  real      :: aer_min_Speed = 0.0        ! minimum allowed wind speed for AERMOD mode
  real      :: aer_min_MixHt = 1.         ! minimum allowed mixing ht. for AERMOD mode
  real      :: aer_min_Obuk  = 1.         ! minimum allowed Obuk abs(L) in AERMOD mode
  real      :: VPTG                       ! vertical potential temperature gradient
!
!------------------------------------------------------------------------------
!
CONTAINS
!
!------------------------------------------------------------------------------
!
  subroutine alloc_wrf(nx,ny,nz)
!
!-----Allocate WRF fields
!
    implicit none
    integer nx,ny,nz

!   (nx)   -- Max west-east unstaggered
!   (ny)   -- Max south-north unstaggered
!   (nz)   -- Max bottom-top unstaggered
!   (nx+1) -- Max west-east staggered
!   (ny+1) -- Max south-north staggered
!   (nz+1) -- Max bottom-top staggered
    if (.not. allocated(ustag)) then
       allocate( ustag(nx+1,ny,nz))! staggered winds
       allocate( vstag(nx,ny+1,nz))
       allocate( uu(nx,ny,nz)    ) ! un-staggered winds (m/s)
       allocate( vv(nx,ny,nz)    )
       allocate( ww(nx,ny,0:nz)  ) ! vertical wind speed (m/s)
       allocate( zh(nx,ny,0:nz)  ) ! height of levels (not layers)
       allocate( tt(nx,ny,nz)    ) ! temperature (K)
       allocate( qq(nx,ny,nz)    ) ! water vapor mixing ratio (kg/kg)
       allocate( qcloud(nx,ny,nz)) ! cloud water mixing ratio (kg/kg)
       allocate( qice(nx,ny,nz)  ) ! cloud ice   mixing ratio (kg/kg)
       allocate( cldfra(nx,ny,nz)) ! cloud fraction (unitless, between 0 and 1)
       allocate( pa(nx,ny,nz)    ) ! perturbation pressure (Pa)
       allocate( pab(nx,ny,nz)   ) ! base state pressure (Pa)
       allocate( ph(nx,ny,nz+1)  ) ! perturbation geopotential (m^2/s^2)
       allocate( phb(nx,ny,nz+1) ) ! base-state   geopotential (m^2/s^2)
       allocate( psax(nx,ny,nz)  ) ! Pstar (MM5, WRF sigma) or
                                   ! layer d(pdry)/d(sigma) (WRF hybrid)

       allocate( cosalpha(nx,ny) ) ! Local cosine of map rotation
       allocate( sinalpha(nx,ny) ) ! Local sine   of map rotation
       allocate( ps(nx,ny)       ) ! MU  perturbation dry air mass in column (Pa)
       allocate( psb(nx,ny)      ) ! MUB base state dry air mass in column (Pa)
       allocate( pbl(nx,ny)      ) ! PBL height (m)
       allocate( pbl_last(nx,ny) ) ! last hour's PBL height (m)
       allocate( aerpbl(nx,ny)   ) ! PBL height (m) for AERMET mode
       allocate( rlu(nx,ny)      ) ! landuse index
       allocate( tsfc(nx,ny)     ) ! surface temperature (K)
       allocate( rainc(nx,ny)    ) ! convective rain accumulation
       allocate( rainr(nx,ny)    ) ! non-convective rain accumulation
       allocate( topo(nx,ny)     ) ! terrain (m)
       allocate( qsw(nx,ny)      ) ! downward shortwave flux (W/m^2)
       allocate( qlw(nx,ny)      ) ! downward longwave  flux (W/m^2)
       allocate( shflux(nx,ny)   ) ! sensible heat flux (W/m^2)
       allocate( lhflux(nx,ny)   ) ! latent   heat flux (W/m^2)
       allocate( sst(nx,ny)      ) ! sea surface temperature (K)
       allocate( t2(nx,ny)       ) ! temperature at 2m (K)
       allocate( q2(nx,ny)       ) ! water vapor mixing ratio at 2m (kg/kg)
       allocate( ustar(nx,ny)    ) ! U* in similarity theory (m/s)
       allocate( rainro(nx,ny)   ) ! non-convective rain accumulation (mm)
       allocate( rainco(nx,ny)   ) ! convective     rain accumulation (mm)
       rainro = -999.              ! flag used to detect when not enough data
       rainco = -999.              ! before desired output time is specified
       allocate( ylat(nx,ny)     ) ! Latitude
       allocate( xlon(nx,ny)     ) ! Longitude
       allocate( ylatU(nx+1,ny)  ) ! U-grid staggered Latitude
       allocate( xlonU(nx+1,ny)  ) ! U-grid staggered Longitude
       allocate( ylatV(nx,ny+1)  ) ! V-grid staggered Latitude
       allocate( xlonV(nx,ny+1)  ) ! V-grid staggered Longitude
       
       allocate( sigma(0:nz)     ) ! ZNW eta values on full (w) levels
       allocate( sigmid(nz)      ) ! ZNU eta values on half (mass) levels 
       allocate( c1h(nz)         ) ! C1 = dB/d(sigma) on half (mass) levels
       allocate( c2h(nz)         ) ! C2 = (1 - dB/d(sigma))*(P0 - Pt) on half
                                   ! (mass) levels 
    end if
    
  end subroutine alloc_wrf
!
!------------------------------------------------------------------------------
!
  subroutine alloc_mm5(nx,ny,nz)
!
!-----Allocate MM5 fields
!
    implicit none
    integer nx,ny,nz

!   (nx)   -- Max west-east unstaggered
!   (ny)   -- Max south-north unstaggered
!   (nz)   -- Max bottom-top unstaggered
    
    if (.not. allocated(hold3d)) then
       allocate( hold3d(nx+1,ny+1,nz)) ! 3-D staggered grids
       allocate( hold2d(nx+1,ny+1))    ! 2-D staggered grids
       allocate( ustag(nx+1,ny+1,nz))  ! staggered winds
       allocate( vstag(nx+1,ny+1,nz))
       allocate( uu(nx,ny,nz)    ) ! unstaggered winds
       allocate( vv(nx,ny,nz)    )
       allocate( ww(nx,ny,0:nz)  ) ! vertical wind speed (m/s) at cell faces,
                                   ! ww(,,0) is the ground (z=0).
       allocate( zh(nx,ny,0:nz)  ) ! height of cell interfaces, zh(,,0) is ground
       allocate( tt(nx,ny,nz)    ) ! temperature (K)
       allocate( qq(nx,ny,nz)    ) ! water vapor mixing ratio (kg/kg)
       allocate( qcloud(nx,ny,nz)) ! cloud water mixing ratio (kg/kg)
       allocate( qice(nx,ny,nz)  ) ! cloud ice   mixing ratio (kg/kg)
       allocate( pa(nx,ny,nz)    ) ! perturbation pressure (Pa)
       allocate( psax(nx,ny,nz)  ) ! Pstar (MM5, WRF sigma) or
                                   ! layer d(pdry)/d(sigma) (WRF hybrid)
       
       allocate( cosalpha(nx,ny) ) ! Local cosine of map rotation
       allocate( sinalpha(nx,ny) ) ! Local sine   of map rotation
       allocate( pbl(nx,ny)      ) ! PBL height (m)
       allocate( pbl_last(nx,ny) ) ! Last hour's PBL height (m)
       allocate( aerpbl(nx,ny)   ) ! PBL height (m) for AERMOD
       allocate( rlu(nx,ny)      ) ! landuse index
       allocate( tsfc(nx,ny)     ) ! surface temperature (K)
       allocate( rainc(nx,ny)    ) ! convective rain accumulation    
       allocate( rainr(nx,ny)    ) ! non-convective rain accumulation
       allocate( topo(nx,ny)     ) ! terrain (m)                     
       allocate( qsw(nx,ny)      ) ! downward shortwave flux (W/m^2)
       allocate( qlw(nx,ny)      ) ! downward longwave  flux (W/m^2)
       allocate( shflux(nx,ny)   ) ! sensible heat flux (W/m^2)
       allocate( lhflux(nx,ny)   ) ! latent   heat flux (W/m^2)
       allocate( sst(nx,ny)      ) ! sea surface temperature (K)
       allocate( t2(nx,ny)       ) ! temperature at 2m (K)
       allocate( q2(nx,ny)       ) ! water vapor mixing ratio at 2m (kg/kg)
       allocate( ustar(nx,ny)    ) ! U* in similarity theory (m/s)        
       allocate( rainro(nx,ny)   ) ! non-convective rain accumulation (mm)
       allocate( rainco(nx,ny)   ) ! convective     rain accumulation (mm)
       allocate( trn(nx,ny)      ) ! used to calculate MM5 cell face heights
       allocate( ylat(nx,ny)     ) ! Latitude 
       allocate( xlon(nx,ny)     ) ! Longitude
       
       allocate( sigma(0:nz)     ) ! derived sigma levels
       allocate( sigmid(nz)      ) ! SIGMAH in MM5 file
    end if
                              
  end subroutine alloc_mm5    
!
!------------------------------------------------------------------------------
!
  subroutine alloc_met(nx,ny,nz,nzOut)
!
!-----Allocate CALMET fields
!
    implicit none

    integer nx,ny,nz,nzOut

!   nx     -- Max west-east,   same for input and outpt
!   ny     -- Max south-north, same for input and outpt
!   nz     -- Max bottom-top,  for input (MM5/WRF)
!   nzOut  -- Output bottom-top for 3-D (aggregated/interpolated) fields
!   Note: uOut,vOut etc. must have nzOut+1, in case CALMET format requirements
!   of 0-20m first layer require us to add a layer.  Indexed by nzFinal, so
!   we can be sure the extra top level will never by used unless it's needed.
 
    if (.not. allocated(uOut)) then
       allocate( psTmp(nx,ny,nzOut+1) )   ! vertically aggregated pstar
       allocate( uOut(nx,ny,nzOut+1) )    ! u-wind (m/s)
       allocate( vOut(nx,ny,nzOut+1) )    ! v-wind (m/s)
       allocate( wOut(nx,ny,0:(nzOut+1))) ! Vertical velocity (m/s) at each
                                          ! grid cell interface. 
                                          ! W(,,0) is the ground, where W is zero
       allocate( tOut(nx,ny,nzOut+1) )    ! temperature (K)
       allocate( pOut(nx,ny,nzOut+1) )    ! pressure (mb)
       allocate( qOut(nx,ny,nzOut+1) )    ! mixing ratio (kg/kg)
       allocate( qcOut(nx,ny,nzOut+1))    ! cloud water mixing ratio (kg/kg)
!
!   These 2-D fields are all DERIVED fields.  Fields native to MM5/WRF
!   should be placed in alloc_mm5 and alloc_wrf.
!
       allocate( u10(nx,ny)   ) ! U-wind at 10m
       allocate( v10(nx,ny)   ) ! V-wind at 10m
       u10 = -999. 
       v10 = -999.
       allocate( t10(nx,ny)   ) ! temperature at 10m
       allocate( psfc(nx,ny)  ) ! surface pressure (mb)
       allocate( wstar(nx,ny) ) ! convective velocity scale
       allocate( mol(nx,ny)   ) ! monin-obukhov length, native in WRF
       allocate( z0(nx,ny)    ) ! roughness length, native in WRF
       allocate( albedo(nx,ny)) ! albedo of surface, native in WRF
       allocate( smois(nx,ny) ) ! soil moisture (kg/kg)
       allocate( bowen(nx,ny) ) ! bowen ratio
       allocate( rain(nx,ny)  ) ! rain rate (mm/hr)
       allocate( cldcvr(nx,ny)) ! derived fractional cloud cover (dimensionless)
       allocate( lai(nx,ny)   ) ! leaf area index
       allocate( ipgt(nx,ny)  ) ! PGT stability class
       allocate( ilu(nx,ny)   ) ! Landuse code
       allocate( iswater(nx,ny))! True if point is water, false if land

       allocate( zm(nz)       ) ! input WRF/MM5 data layer mid-points
       allocate( ulev(nz)     ) ! temporary output holders
       allocate( vlev(nz)     )
       allocate( tlev(nz)     )
       allocate( plev(nz)     )
       allocate( qlev(nz)     )
    end if

  end subroutine alloc_met
!
!------------------------------------------------------------------------------
!
  subroutine dealloc_wrf
!
!-----De-allocate WRF fields
!
    if (.not. allocated(ustag)) then
       write(*,*) 
       write(*,*) "Error: WRF file time range does not include requested time."
       write(*,*) 
       return
    endif
    if (allocated( ustag    )) deallocate( ustag    )
    if (allocated( vstag    )) deallocate( vstag    )
    if (allocated( uu       )) deallocate( uu       )
    if (allocated( vv       )) deallocate( vv       )
    if (allocated( ww       )) deallocate( ww       )
    if (allocated( tt       )) deallocate( tt       )
    if (allocated( qq       )) deallocate( qq       )
    if (allocated( qcloud   )) deallocate( qcloud   )
    if (allocated( qice     )) deallocate( qice     )
    if (allocated( cldfra   )) deallocate( cldfra   )
    if (allocated( zh       )) deallocate( zh       )
    if (allocated( pa       )) deallocate( pa       )
    if (allocated( pab      )) deallocate( pab      )
    if (allocated( ph       )) deallocate( ph       )
    if (allocated( phb      )) deallocate( phb      )
    if (allocated( ps       )) deallocate( ps       )
    if (allocated( psb      )) deallocate( psb      )
    if (allocated( psax     )) deallocate( psax     )
    if (allocated( pbl      )) deallocate( pbl      )
    if (allocated( pbl_last )) deallocate( pbl_last )
    if (allocated( aerpbl   )) deallocate( aerpbl   )
    if (allocated( rlu      )) deallocate( rlu      )
    if (allocated( tsfc     )) deallocate( tsfc     )
    if (allocated( rainc    )) deallocate( rainc    )
    if (allocated( rainr    )) deallocate( rainr    )
    if (allocated( topo     )) deallocate( topo     )
    if (allocated( qsw      )) deallocate( qsw      )
    if (allocated( qlw      )) deallocate( qlw      )
    if (allocated( shflux   )) deallocate( shflux   )
    if (allocated( lhflux   )) deallocate( lhflux   )
    if (allocated( t2       )) deallocate( t2       )
    if (allocated( q2       )) deallocate( q2       )
    if (allocated( ustar    )) deallocate( ustar    )
    if (allocated( rainro   )) deallocate( rainro   )
    if (allocated( rainco   )) deallocate( rainco   )
    if (allocated( ylat     )) deallocate( ylat     )
    if (allocated( xlon     )) deallocate( xlon     )

    if (allocated( cosalpha )) deallocate( cosalpha )
    if (allocated( sinalpha )) deallocate( sinalpha )
                                                           
    if (allocated( sigma    )) deallocate( sigma    )
    if (allocated( sigmid   )) deallocate( sigmid   )

  end subroutine dealloc_wrf
!
!------------------------------------------------------------------------------
!
  subroutine dealloc_mm5
!
!-----De-allocate MM5 fields
!
    if (allocated( ustag    )) deallocate( ustag    )
    if (allocated( vstag    )) deallocate( vstag    )
    if (allocated( uu       )) deallocate( uu       )
    if (allocated( vv       )) deallocate( vv       )
    if (allocated( ww       )) deallocate( ww       )
    if (allocated( tt       )) deallocate( tt       )
    if (allocated( qq       )) deallocate( qq       )
    if (allocated( qcloud   )) deallocate( qcloud   )
    if (allocated( qice     )) deallocate( qice     )
    if (allocated( zh       )) deallocate( zh       )
    if (allocated( pa       )) deallocate( pa       )
                                                    
    if (allocated( psax     )) deallocate( psax     )
    if (allocated( pbl      )) deallocate( pbl      )
    if (allocated( pbl_last )) deallocate( pbl_last )
    if (allocated( aerpbl   )) deallocate( aerpbl   )
    if (allocated( rlu      )) deallocate( rlu      )
    if (allocated( trn      )) deallocate( trn      )
    if (allocated( tsfc     )) deallocate( tsfc     )
    if (allocated( rainc    )) deallocate( rainc    )
    if (allocated( rainr    )) deallocate( rainr    )
    if (allocated( topo     )) deallocate( topo     )
    if (allocated( qsw      )) deallocate( qsw      )
    if (allocated( qlw      )) deallocate( qlw      )
    if (allocated( shflux   )) deallocate( shflux   )
    if (allocated( lhflux   )) deallocate( lhflux   )
    if (allocated( t2       )) deallocate( t2       )
    if (allocated( q2       )) deallocate( q2       )
    if (allocated( ustar    )) deallocate( ustar    )
    if (allocated( rainro   )) deallocate( rainro   )
    if (allocated( rainco   )) deallocate( rainco   )
    if (allocated( ylat     )) deallocate( ylat     )
    if (allocated( xlon     )) deallocate( xlon     )

    if (allocated( cosalpha )) deallocate( cosalpha )
    if (allocated( sinalpha )) deallocate( sinalpha )

    if (allocated( sigma    )) deallocate( sigma    )
    if (allocated( sigmid   )) deallocate( sigmid   )
    
  end subroutine dealloc_mm5
!
!------------------------------------------------------------------------------
!
  subroutine dealloc_met
!
!-----De-allocate CALMET fields
!
    if (.not. allocated(uOut)) return ! if read_wrf failed to find any data
    if (allocated( psTmp   )) deallocate( psTmp  )
    if (allocated( uOut    )) deallocate( uOut   )
    if (allocated( vOut    )) deallocate( vOut   )
    if (allocated( wOut    )) deallocate( wOut   )
    if (allocated( tOut    )) deallocate( tOut   )
    if (allocated( pOut    )) deallocate( pOut   )
    if (allocated( qOut    )) deallocate( qOut   )
    if (allocated( qcOut   )) deallocate( qcOut  )

    if (allocated( u10     )) deallocate( u10    )
    if (allocated( v10     )) deallocate( v10    )
    if (allocated( t10     )) deallocate( t10    )
    if (allocated( psfc    )) deallocate( psfc   )
    if (allocated( mol     )) deallocate( mol    )
    if (allocated( wstar   )) deallocate( wstar  )
    if (allocated( z0      )) deallocate( z0     )
    if (allocated( albedo  )) deallocate( albedo )
    if (allocated( smois   )) deallocate( smois  )
    if (allocated( bowen   )) deallocate( bowen  )
    if (allocated( rain    )) deallocate( rain   )
    if (allocated( cldcvr  )) deallocate( cldcvr )
    if (allocated( lai     )) deallocate( lai    )
    if (allocated( ipgt    )) deallocate( ipgt   )
    if (allocated( ilu     )) deallocate( ilu    )
    if (allocated( iswater )) deallocate( iswater)

    if (allocated( zface   )) deallocate( zface  )
    if (allocated( zmid    )) deallocate( zmid   )
    if (allocated( zm      )) deallocate( zm     )
    if (allocated( ulev    )) deallocate( ulev   )
    if (allocated( vlev    )) deallocate( vlev   )
    if (allocated( tlev    )) deallocate( tlev   )
    if (allocated( plev    )) deallocate( plev   )
    if (allocated( qlev    )) deallocate( qlev   )

  end subroutine dealloc_met
!
!------------------------------------------------------------------------------
!
END MODULE met_fields
