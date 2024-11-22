subroutine read_mm5(iUnit,lFirst,lNewHr,l10MW,lq2,lz0,lalbedo,lmol,llai, &
     lCLDFRA,iErr)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     READ_MM5 reads a raw MM5 (v3) output file and prepares variables fields
!     for further processing.
!
!     NOTE: this version assumes MM5 was run with 24-cat USGS landuse data
!
!     Development History:
!     2009-05-26  Original Development (EPA/R7, ENVIRON International Corp.)
!     2011-09-30  Look for z0, albedo, MOL, LAI, etc. in file.  Support other
!                 projections, and sub-set by LL & KM, etc.
!     2011-11-15  Print MM5 sigma levels with heights even for CALPUFF.
!     2011-12-12  Minor fix: clear up confusion between central and std lat/lon
!     2012-03-07  Minor fix: use new ymdh2nDatHr routine, eliminate jday
!     2013-05-02  Added fixed-control-file-format supporting multiple outs.
!     2013-07-15  Bug fix: when using KM to set output in quadrant 3 of the 
!                 domain, wrong grid cell was being selected.
!     2013-07-16  Made screen output more regular and pretty.
!     2013-09-24  Moved the check for supported land-use types to mmif.f90.
!     2013-09-29  Retro-fitted to accomodate WRF's many land-use methods.
!     2014-03-06  Added calculation of cosalpha,sinalpha for PS/EM wind rotation.
!     2014-05-21  Added SMOIS (soil moisture) to list of fields to read.
!     2017-06-20  New output: BLN, BNA, or KML of 3D sub-domain.
!     2018-11-24  Bug fix: cenlat,cenlon should be origlat,origlon.
!     2019-11-08  Added reading CLDFRA, if it exists in MM5 files.
!     2021-06-24  Modifications to PSAX for WRF's hybrid vertical coordinate
!------------------------------------------------------------------------------
!
  USE met_fields
  USE functions
  USE module_llxy
  implicit none
!
!-----MM5 big header
!
  character*80 bhic(50,20),bhrc(20,20)
  integer bhi(50,20)
  real bhr(20,20)
!
!-----MM5 sub-header
!
  character*4 cstag,ctype
  character*24 curdate
  character*8 cfield
  character*25 cunit
  character*46 cdescrip
  integer ndim,istart(4),ifinish(4)  
  real xtime
!
!-----MM5 grid parameters
!
  real dxrat,dxcrs, xcen,ycen
  TYPE(proj_info)      :: proj ! for use with module_llxy from WRFv3.3
!
!-----MM5 time parameters
!
  integer mm5yr,mm5mo,mm5day,mm5hr,mm5min
!
!-----Miscellaneous variables
!
  integer nxin,nyin,nzin     ! Input (staggered) grid size
  integer iflag
  integer i,j,k,nest,iErr,iunit,nxcrs,nycrs,ip1,jp1
  real    rd,grav,term1,term2,rtmpr,rtmpc,x,y,lat,lon, alpha,diff
  logical lfirst,lnewhr,l10mw,lq2,lz0,lalbedo,lmol,llai,lCLDFRA
!
!-----Data statements
!
  data rd /287./
  data grav /9.8/
!
!-----Entry point
!
  lucat = "USGS"  ! MMIF only support MM5 run with USGS 24-category land-use
  num_land_cat = 24
  water_cat = 16
  lake_cat  = -1
  ice_cat   = 24

  iErr = 0

10 continue
!
!-----Top of loop over MM5 file sections
!
  read(iunit,end=999) iflag
!
!-----Read big header
!
  if (iflag == 0) then
     read(iunit) bhi,bhr,bhic,bhrc  
     ptop  = bhr(2,2)
     ps0   = bhr(2,5)
     ts0   = bhr(3,5)
     tlp   = bhr(4,5)
     nxin  = bhi(17,1)  ! All data in MM5 files have the same size,
     nyin  = bhi(16,1)  ! the size (dimensions) of the staggered grid.
     nzin  = bhi(12,11) ! In MM5, not the bottom-top staggered dim
     nx = nxin - 1      ! non-staggered grid dimensions
     ny = nyin - 1
     nz = nzin
!
!-----If first time reading the first MM5 file, set up grid parameters
!
     if (lfirst) then
        nycrs = bhi(5,1)
        nxcrs = bhi(6,1)
        xcen  = 1. + float(nxcrs-1)/2.
        ycen  = 1. + float(nycrs-1)/2.

        dxcrs = bhr(1,1)/1000. ! in km
        nest  = bhi(13,1)

        call alloc_mm5(nx,ny,nz)
        call alloc_met(nx,ny,nz,nzOut)

        iProj  = bhi(7,1) ! 1=LCC, 2=PS, 3=EM
        dxrat  = float(bhi(20,1))
        deltax = (dxcrs/dxrat) ! in km
        tlat1  = bhr(5,1) ! true latitude 1
        tlat2  = bhr(6,1) ! true latitude 2
! not needed        plat   = bhr(7,1) ! pole latitude for PS and EM projections 
        stdlat   = bhr(2,1) ! central latitude,  in MM5 == Standard Latitude
        stdlon   = bhr(3,1) ! central longitude, in MM5 == Standard Longitude
        conefact = bhr(4,1)
!       ibltyp = bhi(4,13) not used

        pbl_last = 0.    ! default to none
        datum = "NWS-84" ! MM5 default
        if (iProj == 1) pmap = "LCC" ! CALPUFF terminology
        if (iProj == 2) pmap = "PS"
        if (iProj == 3) pmap = "EM"

     endif ! if (lfirst) then

     goto 10
!
!-----Read sub-header data and current date/time
!
  elseif (iflag == 1) then
     read(iunit) ndim,istart,ifinish,xtime,cstag,ctype,curdate,cfield, &
                 cunit,cdescrip
!
!-----Process MM5 date stamp if this is a new data hour
!
     if (lnewhr) then
        read(curdate(1:4),  '(i4)') mm5yr
        read(curdate(6:7),  '(i2)') mm5mo
        read(curdate(9:10), '(i2)') mm5day
        read(curdate(12:13),'(i2)') mm5hr
        read(curdate(15:16),'(i2)') mm5min  ! Sometimes min is 59: round Hr up
        if (mm5min > 55) then 
           mm5hr = mm5hr + 1  ! nint(mm5min/60.) has gfortran bug
           mm5min = 0         ! FIXME if you want sub-hourly time steps
        endif
        call add_hour(mm5yr,mm5mo,mm5day,mm5hr,ibtz)  ! Time zone shift
        call ymdh2nDatHr(mm5yr,mm5mo,mm5day,mm5hr,nDatHr)
        lnewhr = .false.
     endif
!
!-----Read 3-D data fields.  Note that all grids in MM5 files have the same
!     dimension, nxin by nyin, even though that's the size of just a few
!     grids (the staggered U,V).  For the un-staggered grids, there are 
!     values in the cells along the top and right edges, but they are the
!     same values as in adjacent cells.  
!     tt(nx,ny) == tt(nxin,ny) == tt(ny,nyin).
!     To avoid having different grid sizes (dimensions) for variables 
!     that are common to all three grids (MM5, WRF, and output), we
!     read in grids into a temporary container (hold3d or hold2d) and
!     transfer the useful part.  Note the trick to handle W.
!
     nxin = nx + 1      ! nxin,nyin,nzin are not stored globally, so 
     nyin = ny + 1      ! we need to set them again.
     nzin = nz
     if     (cfield == 'U       ') then
        read(iunit) (((ustag(i,j,nz-k+1),j=1,nyin),i=1,nxin),k=1,nz)
     elseif (cfield == 'V       ') then
        read(iunit) (((vstag(i,j,nz-k+1),j=1,nyin),i=1,nxin),k=1,nz)
     elseif (cfield == 'W       ') then
        deallocate(hold3d)
        allocate(hold3d(nxin,nyin,0:nz))
        read(iunit) (((hold3d(i,j,nz-k+1),j=1,nyin),i=1,nxin),k=1,nz+1)
        ww = hold3d(1:nx,1:ny,0:nz)
        deallocate(hold3d)
        allocate(hold3d(nxin,nyin,nz))
     elseif (cfield == 'T       ') then
        read(iunit) (((hold3d(i,j,nz-k+1),j=1,nyin),i=1,nxin),k=1,nz)
        tt = hold3d(1:nx,1:ny,1:nz)
     elseif (cfield == 'PP      ') then
        read(iunit) (((hold3d(i,j,nz-k+1),j=1,nyin),i=1,nxin),k=1,nz)
        pa = hold3d(1:nx,1:ny,1:nz) ! Pa
     elseif (cfield == 'Q       ') then
        read(iunit) (((hold3d(i,j,nz-k+1),j=1,nyin),i=1,nxin),k=1,nz)
        qq = hold3d(1:nx,1:ny,1:nz) ! kg/kg
     elseif (cfield == 'CLW     ') then 
        read(iunit) (((hold3d(i,j,nz-k+1),j=1,nyin),i=1,nxin),k=1,nz)
        qcloud = hold3d(1:nx,1:ny,1:nz) ! kg/kg
     elseif (cfield == 'ICE     ') then
        read(iunit) (((hold3d(i,j,nz-k+1),j=1,nyin),i=1,nxin),k=1,nz)
        qice = hold3d(1:nx,1:ny,1:nz) ! kg/kg
     elseif (cfield == 'CLDFRA  ') then
        read(iunit) (((hold3d(i,j,nz-k+1),j=1,nyin),i=1,nxin),k=1,nz)
        CLDFRA = hold3d(1:nx,1:ny,1:nz)
        lCLDFRA = .true.
!
!-----Read the rest of the (2-D) fields
!
     elseif (cfield == 'PBL HGT ') then
        read(iunit) ((hold2d(i,j),j=1,nyin),i=1,nxin)
        pbl = hold2d(1:nx,1:ny)
     elseif (cfield == 'PSTARCRS') then
        read(iunit) ((hold2d(i,j),j=1,nyin),i=1,nxin)
        do k = 1,nz
          psax(:,:,k) = hold2d(:,:)
        enddo
     elseif (cfield == 'TERRAIN ') then
        read(iunit) ((hold2d(i,j),j=1,nyin),i=1,nxin)
        topo = hold2d(1:nx,1:ny)
     elseif (cfield == 'LAND USE') then
        read(iunit) ((hold2d(i,j),j=1,nyin),i=1,nxin)
        rlu = hold2d(1:nx,1:ny)
     elseif (cfield == 'RAIN NON') then
        read(iunit) ((hold2d(i,j),j=1,nyin),i=1,nxin)
        rainr = hold2d(1:nx,1:ny)
     elseif (cfield == 'RAIN CON') then
        read(iunit) ((hold2d(i,j),j=1,nyin),i=1,nxin)
        rainc = hold2d(1:nx,1:ny)
     elseif (cfield == 'GROUND T') then 
        read(iunit) ((hold2d(i,j),j=1,nyin),i=1,nxin)
        tsfc = hold2d(1:nx,1:ny)
     elseif (cfield == 'SWDOWN  ') then
        read(iunit) ((hold2d(i,j),j=1,nyin),i=1,nxin)
        qsw = hold2d(1:nx,1:ny)
     elseif (cfield == 'LWDOWN  ') then
        read(iunit) ((hold2d(i,j),j=1,nyin),i=1,nxin)
        qlw = hold2d(1:nx,1:ny)
     elseif (cfield == 'SHFLUX  ') then
        read(iunit) ((hold2d(i,j),j=1,nyin),i=1,nxin)
        shflux = hold2d(1:nx,1:ny)
     elseif (cfield == 'LHFLUX  ') then
        read(iunit) ((hold2d(i,j),j=1,nyin),i=1,nxin)
        lhflux = hold2d(1:nx,1:ny)
     elseif (cfield == 'SOIL M 1 ') then
        read(iunit) ((hold2d(i,j),j=1,nyin),i=1,nxin)
        smois = hold2d(1:nx,1:ny)
     elseif (cfield == 'U10     ') then
        read(iunit) ((hold2d(i,j),j=1,nyin),i=1,nxin)
        u10 = hold2d(1:nx,1:ny)
        l10mw = .true.
     elseif (cfield == 'V10     ') then
        read(iunit) ((hold2d(i,j),j=1,nyin),i=1,nxin)
        v10 = hold2d(1:nx,1:ny)
     elseif (cfield == 'TSEASFC ') then
        read(iunit) ((hold2d(i,j),j=1,nyin),i=1,nxin)
        sst = hold2d(1:nx,1:ny)
     elseif (cfield == 'T2      ') then
        read(iunit) ((hold2d(i,j),j=1,nyin),i=1,nxin)
        t2 = hold2d(1:nx,1:ny)
     elseif (cfield == 'Q2      ') then
        read(iunit) ((hold2d(i,j),j=1,nyin),i=1,nxin)
        q2 = hold2d(1:nx,1:ny) ! kg/kg
        lq2 = .true.
     elseif (cfield == 'UST     ') then
        read(iunit) ((hold2d(i,j),j=1,nyin),i=1,nxin)
        ustar = hold2d(1:nx,1:ny)
     elseif (cfield == 'ZNT     ') then
        read(iunit) ((hold2d(i,j),j=1,nyin),i=1,nxin)
        z0 = hold2d(1:nx,1:ny)
        lz0 = .true.
     elseif (cfield == 'ALBEDO  ' .or. cfield == 'ALB     ') then
        read(iunit) ((hold2d(i,j),j=1,nyin),i=1,nxin)
        albedo = hold2d(1:nx,1:ny) / 100. ! was percent, now fraction
        lalbedo = .true.
     elseif (cfield == 'LAI     ') then
        read(iunit) ((hold2d(i,j),j=1,nyin),i=1,nxin)
        lai = hold2d(1:nx,1:ny)
        llai = .true.
     elseif (cfield == 'M-O LENG') then
        read(iunit) ((hold2d(i,j),j=1,nyin),i=1,nxin)
        mol = hold2d(1:nx,1:ny)
        lmol = .true.
     elseif (cfield == 'LATITCRS') then
        read(iunit) ((hold2d(i,j),j=1,nyin),i=1,nxin)
        ylat = hold2d(1:nx,1:ny)
     elseif (cfield == 'LONGICRS') then
        read(iunit) ((hold2d(i,j),j=1,nyin),i=1,nxin)
        xlon = hold2d(1:nx,1:ny)
!
!-----Read the (single) 1-D field
!
     elseif (cfield == 'SIGMAH  ') then
        read(iunit) (sigmid(nz-k+1),k=1,nz)
     elseif (cfield == 'SFZ0    ') then
        read(iunit)
     else ! skip any other fields found in the MM5 file
        read(iunit)
     endif

     goto 10 ! read the next field
!
!-----Finished reading fields for current date/time
!
  elseif (iflag == 2) then
     lnewhr = .true.
!
!-----Determine hourly incremental total precipitation rates (mm/hr)
!
     do j = 1,ny
        do i = 1,nx
           rtmpr = rainr(i,j) ! MM5 saves rain in cm (accumulated)
           rtmpc = rainc(i,j)
           if (xtime == 0 .or. nint(xtime/60.) == 1) then ! Forecast hour 0, 1
              rainr(i,j) = amax1(0., rtmpr*10.) ! convert to mm/hr
              rainc(i,j) = amax1(0., rtmpc*10.)
           else if (lfirst) then  ! First MM5 file has forecast hour > 1 !
              rainr(i,j) = 0. ! What else can we do?  rain is the accumulated
              rainc(i,j) = 0. ! rain, but we don't know last hour's value!
           else                   ! Normal case: not first time-stamp read
              rainr(i,j) = amax1(0., ( rtmpr-rainro(i,j))*10. )
              rainc(i,j) = amax1(0., ( rtmpc-rainco(i,j))*10. )
           endif
           rainro(i,j) = rtmpr ! Save the accumlated rain up till now
           rainco(i,j) = rtmpc ! for next hour of this run (in cm)

           rain(i,j) = rainr(i,j) + rainc(i,j)
           if (rain(i,j) < 0.01) rain(i,j) = 0. ! Limit very small rain rates
        enddo
     enddo
!
!-----Determine total pressure (pa) in mb, vapor mixing ratios in g/kg,
!     and de-stagger the U and V winds
!
     do k = 1,nz
        do j = 1,ny
           jp1 = j + 1
           do i = 1,nx
              ip1  =  i + 1
              pa(i,j,k) = (pa(i,j,k) + psax(i,j,1)*sigmid(k) + ptop)/100.
              uu(i,j,k) = 0.25*(ustag(i,j,k) + ustag(ip1,j,k) + &
                              ustag(i,jp1,k) + ustag(ip1,jp1,k))
              vv(i,j,k) = 0.25*(vstag(i,j,k) + vstag(ip1,j,k) + &
                              vstag(i,jp1,k) + vstag(ip1,jp1,k))
           enddo
        enddo
     enddo
!
!-----Calculate level heights (zh) for each time step
!
     sigma(0) = 1.
     do k = 1,nz
        sigma(k) = 2.*sigmid(k) - sigma(k-1)
     enddo

     do j = 1,ny
        do i = 1,nx
           p0 = sigma(0)*psax(i,j,1) + ptop
           term1 = rd*tlp/(2.*grav)*(alog(p0/ps0))**2
           term2 = (rd*ts0/grav)*alog(p0/ps0)
           trn(i,j) = -(term1 + term2)
        enddo
     enddo

     zh(1:nx,1:ny,0) = 0. ! ground level
     do k = 1,nz
        do j = 1,ny
           do i = 1,nx
              p0 = sigma(k)*psax(i,j,1) + ptop
              term1 = rd*tlp/(2.*grav)*(alog(p0/ps0))**2
              term2 = (rd*ts0/grav)*alog(p0/ps0)
              zh(i,j,k) = -(term1 + term2) - trn(i,j) ! grid cell face hgts
           enddo
        enddo
     enddo
!
!-----Calculate the surface pressure in mb (simple linear extrapolation)
!
     do j = 1,ny
        do i = 1,nx
           psfc(i,j) = pa(i,j,1) - zh(i,j,1)/2. * (pa(i,j,2) - pa(i,j,1)) / &
                ((zh(i,j,1)+zh(i,j,2))/2. - zh(i,j,1)/2.)
        end do
     end do
!
!-----If first time in first MM5 file, calculate static fields:
!     1) lat,lon of sub-grid
!     2) sigma levels at layer interfaces
!     3) topography (MSL)
!     4) layer interface heights (AGL)
!
     if (lfirst) then

        if (origlat == -999. .or. origlon == -999.) then ! over-ride origin
           origlat = stdlat
           origlon = stdlon
        endif

!----Sanity check for valid origin

        if ( origlat <  -90. .or. origlat >  90. .or. &
             origlon < -180. .or. origlon > 360.) then ! sanity checks
           write(*,*)
           write(*,*) "Unable to determine the latitude of the origin of the projected "
           write(*,*) "coordinate system. Please add the ORIGIN keyword to your mmif.inp file."
           write(*,*) "You can use the cenlat,cenlon = ",cenlat,cenlon
           write(*,*) "or you can use any nice, round numbers that are easier to remember."
           stop
        endif

        call map_set(iProj,proj,lat1=origlat,lon1=origlon,knowni=0.,knownj=0., &
             dx=deltax*1000.,stdlon=stdlon,truelat1=tlat1,truelat2=tlat2)

!-----Calculate cosalpha and sinalpha, found in WRF output but not in MM5 output.
!     These should use stdlon, not cenlon or origlon, because the latter two are
!     variable - any downstream program can move the origin of the coordinate
!     system overlaid upon the plane defined by (stdlon, tlat1, tlat2).

        if (iProj == 1) then ! LCC

           do j = 1, ny
              do i = 1, nx
                 diff = stdlon - xlon(i,j)
                 if (diff > 180.) then
                    diff = diff - 360.
                 else if (diff < -180.) then
                    diff = diff + 360.
                 end if

                 alpha = diff * conefact * d2r * sign(1.,ylat(i,j)) ! radians

                 cosalpha(i,j) = cos(alpha)
                 sinalpha(i,j) = sin(alpha)

              end do
           end do

        else if (iProj == 2) then ! PS

           do j = 1, ny
              do i = 1, nx
                 diff = stdlon - xlon(i,j)
                 if (diff > 180.) then
                    diff = diff - 360.
                 else if (diff < -180.) then
                    diff = diff + 360.
                 end if

                 alpha = diff * d2r * sign(1.,ylat(i,j)) ! radians

                 cosalpha(i,j) = cos(alpha)
                 sinalpha(i,j) = sin(alpha)
                 
              end do
           end do

        else ! EM

           cosalpha = 1. ! effectively no rotation, see uv2sd in functions.f90
           sinalpha = 0.

        end if
!
!-----Lower-left and upper-right corners of the full MM5 grid. We'll use
!     xWwrf, ySwrf etc (with WRF in the name) just because it's already declared.
!
!-----The calls to latlon_to_ij() using ylat(),xlon() should ALWAYS return
!     either a full integer or a half (x.0000 or x.5000). But sometimes it
!     returns x.99996 or x.4973 or something like that. This is presumably
!     a bug in WRF's share/module_llxy.F, which I re-purposed for use in MMIF.
!     I'll fix this by rounding here. I used to fix this below, after 
!     converting I,J to X,Y by multiplying by deltax. That half-fix has now
!     been removed. 

        call latlon_to_ij(proj,ylat(1,1),xlon(1,1),xWwrf,ySwrf)
        xWwrf = nint(xWwrf * 2.)/2. ! round to nearest whole or half integer
        ySwrf = nint(ySwrf * 2.)/2.
        xWwrf = xWwrf * deltax      ! in km
        ySwrf = ySwrf * deltax

        call latlon_to_ij(proj,ylat(nx,ny),xlon(nx,ny),xEwrf,yNwrf)
        xEwrf = nint(xEwrf * 2.)/2. ! round to nearest whole or half integer
        yNwrf = nint(yNwrf * 2.)/2.
        xEwrf = xEwrf * deltax      ! in km
        yNwrf = yNwrf * deltax
!
!-----Conform to CALMET's terminology for the LL corner: use corners 
!     of lower-left and upper-right grid _edges_, not the cell _centers_.
!
        xWwrf = xWwrf - deltax/2.
        ySwrf = ySwrf - deltax/2.
        xEwrf = xEwrf + deltax/2.
        yNwrf = yNwrf + deltax/2.
!
!-----Convert the projected x0met,y0met,xfmet,yfmet to lat-lon and save
!
        call ij_to_latlon(proj,xWwrf/deltax,ySwrf/deltax, & ! SW corner
             wrfgrid_latlon(1,1),wrfgrid_latlon(1,2) )
        call ij_to_latlon(proj,xEwrf/deltax,ySwrf/deltax, & ! SE corner
             wrfgrid_latlon(2,1),wrfgrid_latlon(2,2) )
        call ij_to_latlon(proj,xEwrf/deltax,yNwrf/deltax, & ! NE corner
             wrfgrid_latlon(3,1),wrfgrid_latlon(3,2) )
        call ij_to_latlon(proj,xWwrf/deltax,yNwrf/deltax, & ! NW corner
             wrfgrid_latlon(4,1),wrfgrid_latlon(4,2) )

        write(*,*)
        write(*,*)'Grid parameters for the input (full) MM5 domain:'
        write(*,'(a,a13,i13)')  '              Projection:',adjustr(PMAP),iProj
        write(*,'(a,2f13.5)')   '        Standard Lat/lon:',stdlat,stdlon
        if (PMAP == "LCC") &
             write(*,'(a,2f13.5)') '          True Latitudes:',tlat1,tlat2
        if (PMAP == "PS") &
             write(*,'(a,2f13.5)') '           True Latitude:',tlat1
        write(*,'(a,i13)')      '                 NEST ID:',nest
        write(*,'(a,i13,i7,i6)')'   Un-staggered NX,NY,NZ:',nx,ny,nz
        write(*,'(a,f13.3)')    '    Grid Resolution (km):',deltax
        write(*,'(a,2f13.3)')   ' Low-Lft x/y corner (km):',xWwrf,ySwrf
        write(*,'(a,2f13.3)')   ' Upr-Rgt x/y corner (km):',xEwrf,yNwrf
        write(*,'(a,2f13.5)')   ' SW Lat-Lon corner (deg):',wrfgrid_latlon(1,1),&
             wrfgrid_latlon(1,2)
        write(*,'(a,2f13.5)')   ' NE Lat-Lon corner (deg):',wrfgrid_latlon(3,1),&
             wrfgrid_latlon(3,2)
        write(*,*)
!
!-----If specified, set subset by Lat,Lon
!
        if (ijlatlon == 2) then
           call latlon_to_ij(proj,BegLat,BegLon,x,y)
           iBeg = floor((x * deltax - xWwrf)/deltax + 1)
           jBeg = floor((y * deltax - ySwrf)/deltax + 1)
           if (EndLat == BegLat .and. EndLon == BegLon) then
              iEnd = iBeg ! Single-point data
              jEnd = jBeg
           else
              call latlon_to_ij(proj,EndLat,EndLon,x,y)
              iEnd = ceiling((x * deltax - xWwrf)/deltax)
              jEnd = ceiling((y * deltax - ySwrf)/deltax)
           end if
        endif
!
!-----If specified, set subset by Xlcc, Ylcc
!
        if (ijlatlon == 3) then
           iBeg = floor((BegXlcc - xWwrf)/deltax + 1)
           jBeg = floor((BegYlcc - ySwrf)/deltax + 1)
           if (EndXlcc == BegXlcc .and. EndYlcc == BegYlcc) then
              iEnd = iBeg ! Single-point data
              jEnd = jBeg
           else
              iEnd = ceiling((EndXlcc - xWwrf)/deltax)
              jEnd = ceiling((EndYlcc - ySwrf)/deltax)
           end if
        endif
!
!-----Select entire domain (minus potential edges) if requested:
!
        if (iBeg <= 0) iBeg = 1  - iBeg
        if (iEnd <= 0) iEnd = nx + iEnd
        if (jBeg <= 0) jBeg = 1  - jBeg
        if (jEnd <= 0) jEnd = ny + jEnd
!
!-----If specified, do the  Lat,Lon or LCC to I,J conversion for AER* outs
!
        do i = 1, NumOuts
           if (ijPt(i) == 2) then
              call latlon_to_ij(proj,PtLat(i),PtLon(i),x,y)
              iPt(i) = floor((x * deltax - xWwrf)/deltax + 1)
              jPt(i) = floor((y * deltax - ySwrf)/deltax + 1)
           else if (ijPt(i) == 3) then
              iPt(i) = floor((PtXlcc(i) - xWwrf)/deltax + 1)
              jPt(i) = floor((PtYlcc(i) - ySwrf)/deltax + 1)
           end if
        end do
!
!-----Sanity checks
!
        call grid_in_grid(iBeg,iEnd,jBeg,jEnd,nx,ny,"MM5")
        do i = 1, NumOuts
           if (ijPt(i) /= 0) &
                call point_in_grid(iPt(i),jPt(i),iBeg,iEnd,jBeg,jEnd,i)
        end do
!
!-----Find the lower-left and upper-right corners of the sub-domain
!
        call latlon_to_ij(proj,ylat(iBeg,jBeg),xlon(iBeg,jBeg),x0met,y0met)
        x0met = nint(x0met * 2.)/2. ! round to nearest whole or half integer
        y0met = nint(y0met * 2.)/2.
        x0met = x0met * deltax      ! in km
        y0met = y0met * deltax

        call latlon_to_ij(proj,ylat(iEnd,jEnd),xlon(iEnd,jEnd),xfmet,yfmet)
        xfmet = nint(xfmet * 2.)/2. ! round to nearest whole or half integer
        yfmet = nint(yfmet * 2.)/2.
        xfmet = xfmet * deltax      ! in km
        yfmet = yfmet * deltax      ! using "f" for consistency with CALMET
!
!-----Conform to CALMET's terminology for the LL corner: use corners 
!     of lower-left and upper-right grid _edges_, not the cell _centers_.
!
        x0met = x0met - deltax/2.
        y0met = y0met - deltax/2.
        xfmet = xfmet + deltax/2.
        yfmet = yfmet + deltax/2.
!
!-----Convert the projected x0met,y0met,xfmet,yfmet to lat-lon and save
!
        call ij_to_latlon(proj,x0met/deltax,y0met/deltax, & ! SW corner
             subgrid_latlon(1,1),subgrid_latlon(1,2) )
        call ij_to_latlon(proj,xfmet/deltax,y0met/deltax, & ! SE corner
             subgrid_latlon(2,1),subgrid_latlon(2,2) )
        call ij_to_latlon(proj,xfmet/deltax,yfmet/deltax, & ! NE corner
             subgrid_latlon(3,1),subgrid_latlon(3,2) )
        call ij_to_latlon(proj,x0met/deltax,yfmet/deltax, & ! NW corner
             subgrid_latlon(4,1),subgrid_latlon(4,2) )
!
!-----Write the sub-domain details to the screen
!
        write(*,*)'    Grid parameters for: Output 3-D sub-domain'
        write(*,'(a,i13,i7,i6)')'                NX,NY,NZ:', &
             iEnd-iBeg+1, jEnd-jBeg+1,nzOut
        write(*,'(a,2f13.3)')   ' Low-Lft x/y corner (km):',x0met,y0met
        write(*,'(a,2f13.3)')   ' Upr-Rgt x/y corner (km):',xfmet,yfmet
        write(*,'(a,2f13.5)')   ' SW Lat-Lon corner (deg):',subgrid_latlon(1,1),&
             subgrid_latlon(1,2)
        write(*,'(a,2f13.5)')   ' NE Lat-Lon corner (deg):',subgrid_latlon(3,1),&
             subgrid_latlon(3,2)
        write(*,'(a,2i13)')     ' Low-Lft I/J cell center:',iBeg,jBeg
        write(*,'(a,2i13)')     ' Upr-Rgt I/J cell center:',iEnd,jEnd
        write(*,*) 

        do i = 1, NumOuts
           if (ijPt(i) > 0) then
              write(*,'(a,i6,5a)')  &
                   '     Grid parameters for: Output 1-D point  #',i,  &
                   ' ',trim(OutFile(i))
!                  ' (',trim(OutForm(i)),' ',trim(OutType(i)),')'

              call latlon_to_ij(proj,ylat(iPt(i),jPt(i)),xlon(iPt(i),jPt(i)),x,y)
              x = x * deltax - deltax/2.! in km
              y = y * deltax - deltax/2.! in km
              if (abs(x - nint(x)) <= 0.003) x = float(nint(x))
              if (abs(y - nint(y)) <= 0.003) y = float(nint(y))
              write(*,'(a,2f13.3)')   ' Low-Lft x/y corner (km):',x,y
              call ij_to_latlon(proj,x/deltax,y/deltax,lat,lon)
              x = x + deltax
              y = y + deltax
              write(*,'(a,2f13.3)')   ' Upr-Rgt x/y corner (km):',x,y
              write(*,'(a,2f13.5)')   ' SW Lat-Lon corner (deg):',lat,lon
              call ij_to_latlon(proj,x/deltax,y/deltax,lat,lon)
              write(*,'(a,2f13.5)')   ' NE Lat-Lon corner (deg):',lat,lon
              write(*,'(a,2i13)')     '         I/J cell center:',iPt(i),jPt(i)
              write(*,*) 
           end if
        end do
!
!-----Convert land-use codes to integers, and set the iswater flag
!
        iswater = .false. ! initialize
        do j = 1,ny
           do i = 1,nx
              ilu(i,j) = int(rlu(i,j))
              if (ilu(i,j) == 16) iswater(i,j) = .true.
           enddo
        enddo
!
!-----Tell the user if we found 10m winds, z0, albedo, L, LAI
!
        write(*,*) "In this MM5 file:"
        if (l10MW) then
           write(*,*) "  10m winds            : found"
        else
           write(*,*) "  10m winds            : NOT FOUND"
        endif
        if (lz0) then
           write(*,*) "  Roughness length     : found"
        else
           write(*,*) "  Roughness length     : NOT FOUND"
        endif
        if (lalbedo) then
           write(*,*) "  Albedo               : found"
        else
           write(*,*) "  Albedo               : NOT FOUND"
        endif
        if (lmol) then
           write(*,*) "  Monin-obukhov length : found"
        else
           write(*,*) "  Monin-obukhov length : NOT FOUND"
        endif
        if (llai) then
           write(*,*) "  Leaf area index (LAI): found"
        else
           write(*,*) "  Leaf area index (LAI): NOT FOUND"
        endif

        call print_met_levels

        lfirst = .false.
     endif  ! if (lfirst) then

  endif ! if (iflag == 0,1,2) then
  return

999 continue
  if (lfirst) then
     iErr = 2 ! error opening the very first file, maybe a WRF file?
  else
     iErr = 1 ! probably the normal end of a file
  endif

  return

end subroutine read_mm5
