subroutine read_wrf(cdfid,lFirst,iTime,TimeStamp,get_precip_only,    &
     l10mw,lq2,lz0,lalbedo,lmol,llai,lCLDFRA, debug)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     READ_WRF reads a raw WRF/ARW output file and prepares variables fields
!     for further processing.
!
!     NOTE: this version support most, but not all, the possible land-use 
!     datasets (USGS, MODIS, MODIFIED_IGBP_MODIS_NOAH, NLCD50, and NLCD40).
!
!     Development History:
!     2009-05-26  Original Development (ENVIRON International Corp.)
!     2011-09-30  Look for z0, albedo, MOL, LAI, etc. in file.  Support other
!                 projections, and sub-set by LL & KM, etc.
!     2011-11-15  Print WRF sigma levels with heights even for CALPUFF.
!     2011-12-12  Minor fix: clear up confusion between central and std lat/lon
!     2012-03-13  Bug fix: XTIME and saving of precip were wrong for the first
!                 WRFOUT file from a WRF initialization.
!     2012-09-04  Bug fix: some compilations of netCDF don't blank text strings
!                 passed to nf_get_att_text().  We'll blank them before passing.
!     2013-03-05  Set iswater(:,:) every time step - it changes with time due 
!                 to changing sea ice.
!     2013-05-02  Moved sanity check for CALMET and WRF using same projection to
!                 mmif.f90, to facilitate moving outform from met_fields.f90.
!     2013-05-02  Added support for multiple single-point output files.
!     2013-06-18  Moved check for supported landuse types to mmif.f90, which
!                 allowed the removal of logical force_run (no longer used).
!     2013-07-15  Bug fix: when using KM to set output in quadrant 3 of the 
!                 domain, wrong grid cell was being selected.
!     2013-07-16  Made screen output more regular and pretty.
!     2013-09-25  Added support for several other land-use datasets.
!     2014-05-21  Added SMOIS (soil moisture) to list of fields to read.
!     2017-06-20  New output: BLN, BNA, or KML of 3D sub-domain.
!     2018-12-26  Read WRF's CLDFRA output and use for cldcvr.
!     2019-11-08  Bug fix: older versions of WRF don't have CLDFRA, so skip.
!     2021-06-24  Add ability to use WRF's hybrid vertical coordinate
!
!------------------------------------------------------------------------------
!
  USE wrf_netcdf
  USE met_fields
  USE module_llxy
  USE functions
  implicit none
  include 'netcdf.inc'
!
!-----Variable declaration
!
  character (len=19), intent(out) :: TimeStamp   ! WRF data time-stamp
  character (len=80)              :: SimStart    ! start of this WRF run
  character (len=80)              :: TitleString ! To read WRF version

  integer, intent(in)  :: cdfid,iTime
  integer :: i,j,k,ip1,jp1,id,nest,rcode,iyr,imo,idy,ihr
  real    :: grav,rcp,rtmpr,rtmpc,xtime,x,y,lat,lon, tmp

  logical, intent(in)  :: debug, get_precip_only
  logical, intent(out) :: lFirst,l10mw,lq2,lz0,lalbedo,lmol,llai,lCLDFRA

  TYPE(proj_info)      :: proj ! for use with module_llxy from WRFv3.3
!
!-----Data statements
!
  data rcp  /0.2859/     ! Rd/Cp, (287 J/deg/kg)/(1004 J/deg/kg)
  data grav /9.8/        ! acceleration due to gravity
!
!-----Entry point
!
!-----First time through, allocate WRF fields, initialize variables
!     calculate grid parameters, read and determine time-invariant 
!     terrain data.
!
  if (lFirst) then

     rcode = nf_inq_dimid(cdfid,'west_east',id)
     rcode = nf_inq_dimlen(cdfid,id,nx)

     rcode = nf_inq_dimid(cdfid,'south_north',id)
     rcode = nf_inq_dimlen(cdfid,id,ny)

     rcode = nf_inq_dimid(cdfid,'bottom_top',id)
     rcode = nf_inq_dimlen(cdfid,id,nz)

     rcode = nf_inq_dimid(cdfid,'soil_layers_stag',id)
     rcode = nf_inq_dimlen(cdfid,id,nsoil) ! number of (staggered) soil layers

     call alloc_wrf(nx,ny,nz)
     call alloc_met(nx,ny,nz,nzOut)

!-----Check if WRF was run with the hybrid vertical coordinate
!     (hybrid terrain-following + isobaric aloft)
!
     if (.not. get_precip_only) then
       rcode = nf_get_att_int(cdfid,nf_global,'HYBRID_OPT',ihyb)
       if (rcode.ne.0) then
         write(*,*)
         write(*,*)'Cannot find attribute HYBRID_OPT in WRF file'
         write(*,*)'Assuming WRF was run with original', &
                   ' terrain-following eta coordinate'
         ihyb = 0
       elseif (ihyb.ne.0) then
         write(*,*)
         write(*,*)'WRF was run with the hybrid vertical coordinate'
       else
         write(*,*)
         write(*,*)'WRF was run with the original eta vertical coordinate'
       endif
     endif

     rcode = nf_get_att_int(cdfid,nf_global, 'MAP_PROJ',iProj)
     rcode = nf_get_att_real(cdfid,nf_global,'MOAD_CEN_LAT',stdlat)
     rcode = nf_get_att_real(cdfid,nf_global,'STAND_LON',stdlon)
     rcode = nf_get_att_real(cdfid,nf_global,'TRUELAT1',tlat1)
     rcode = nf_get_att_real(cdfid,nf_global,'TRUELAT2',tlat2)
     rcode = nf_get_att_real(cdfid,nf_global,'CEN_LAT',cenlat) ! of this domain,
     rcode = nf_get_att_real(cdfid,nf_global,'CEN_LON',cenlon) ! not relevant
     rcode = nf_get_att_real(cdfid,nf_global,'DX',deltax) ! in meters
     rcode = nf_get_att_int(cdfid,nf_global, 'GRID_ID',nest)
     lucat = "" ! initialize: some compilations of netCDF don't blank the var
     rcode = nf_get_att_text(cdfid,nf_global,'MMINLU',lucat)
     rcode = nf_get_att_int(cdfid,nf_global,'NUM_LAND_CAT',num_land_cat)
     rcode = nf_get_att_int(cdfid,nf_global,'ISWATER',water_cat)
     rcode = nf_get_att_int(cdfid,nf_global,'ISLAKE', lake_cat)
     rcode = nf_get_att_int(cdfid,nf_global,'ISICE',  ice_cat)
     rcode = nf_get_att_text(cdfid,nf_global,'TITLE', TitleString)

     call get_var_2d_real_cdf(cdfid,'XLAT',ylat,nx,ny,iTime,debug)
     call get_var_2d_real_cdf(cdfid,'XLONG',xlon,nx,ny,iTime,debug)
     call get_var_2d_real_cdf(cdfid,'HGT',topo,nx,ny,iTime,debug)

     call get_var_2d_real_cdf(cdfid,'COSALPHA',cosalpha,nx,ny,iTime,debug)
     call get_var_2d_real_cdf(cdfid,'SINALPHA',sinalpha,nx,ny,iTime,debug)
!
!-----Set defaults
!
     pbl_last = 0.

     datum = "NWS-84"     ! WRF uses a spherical earth, radius 6370000m
                          ! See share/module_llxy.f90 in any WRF code.
     if (iProj == 1) then ! set pmap using CALPUFF terminology
        pmap = "LCC" 
     elseif (iProj == 2) then
        pmap = "PS"
     elseif (iProj == 3) then
        pmap = "EM"
     else
        pmap = "OTHER"
     endif

     if (tlat1 > tlat2) then            ! reverse the order if reversed
        tmp = tlat1
        tlat1 = tlat2
        tlat2 = tmp
     end if

     call lc_cone(tlat1,tlat2,conefact) ! in WRF's module_llxy
     deltax = deltax / 1000.            ! convert m to km
!
!-----Find the projected coordinate of the lower-left point of the WRF domain,
!     and calculate all distances relative to this point.  
!
!     If the WRF run's WPS setup specified ref_x and ref_y, then the central
!     (lat,lon) of the mother of all domains (MOAD_CEN_LAT, STAND_LON) are 
!     not necessarily the same as the Standard Lat/Lon.  They might be the
!     same, but we cannot be sure.  By default we assume they are the same,
!     but we give the user the option to specify the (lat,lon) where they'd
!     like the projected coodinate system's origin to be.
!
     if (origlat == -999. .or. origlon == -999.) then  ! over-ride origin
        origlat = stdlat
        origlon = stdlon
     endif

!----Sanity check for valid origin

     if (origlat <  -90. .or. origlat >  90. .or. &
         origlon < -180. .or. origlon > 360.) then ! sanity checks
        write(*,*)
        write(*,*) "Unable to determine the latitude of the origin of the projected "
        write(*,*) "coordinate system. Please add the ORIGIN keyword to your mmif.inp file."
        write(*,*) "You can use the cenlat,cenlon = ",cenlat,cenlon
        write(*,*) "or you can use any nice, round numbers that are easier to remember."
        stop
     endif
!
!-----This sets the map projection, by saying the point (lat1,lon1) corresponds
!     to the point (0,0) in LCC space (i.e. the origin of the LCC grid is there).
!     The user is technically free to set (lat1,lon1) to anything.  It's just 
!     convenient to set lon1=stdlon=cenlon, but not required.  In CALPUFF's 
!     input files, no distiction is made between stdlon and cenlon -- they use
!     false easting and northing to shift the grid.  
!
     call map_set(iProj,proj,lat1=origlat,lon1=origlon,knowni=0.,knownj=0.,  & 
          dx=deltax*1000.,stdlon=stdlon,truelat1=tlat1,truelat2=tlat2)
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
!-----Convert the projected xWmet,yXmet,xEmet,yNmet to lat-lon and save.
!     These are the _corners_ of the grid boxes, not XLAT(,) XLONG(,) which
!     are the centers of each grid cell. Also not quite XLAT_U(,) XLONG_V(). 
!     They are stored in geo_em.d0?.nc files as corner_lats,corner_lons
!     attribute, but those are not available in WRFOUT files.
!
     call ij_to_latlon(proj,xWwrf/deltax,ySwrf/deltax, & ! SW corner
          wrfgrid_latlon(1,1),wrfgrid_latlon(1,2) )
     call ij_to_latlon(proj,xEwrf/deltax,ySwrf/deltax, & ! SE corner
          wrfgrid_latlon(2,1),wrfgrid_latlon(2,2) )
     call ij_to_latlon(proj,xEwrf/deltax,yNwrf/deltax, & ! NE corner
          wrfgrid_latlon(3,1),wrfgrid_latlon(3,2) )
     call ij_to_latlon(proj,xWwrf/deltax,yNwrf/deltax, & ! NW corner
          wrfgrid_latlon(4,1),wrfgrid_latlon(4,2) )

     if (.not. get_precip_only) then ! calculate, but don't print 
        write(*,*)
        write(*,*)'    Grid parameters for: Input (full) WRF domain'
        write(*,'(a,a13,i13)')  '              Projection:',adjustr(PMAP),iProj
        write(*,'(a,2f13.5)')   '          Origin Lat/lon:',origlat,origlon
        if (PMAP == "LCC") then
           write(*,'(a,2f13.5)')'          True Latitudes:',tlat1,tlat2
        else
           write(*,'(a,2f13.5)')'           True Latitude:',tlat1
        endif
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
        call flush(6)
     end if
!
!-----If specified, set subset grid by Lat,Lon 
!
     if (ijlatlon == 2) then
        call latlon_to_ij(proj,BegLat,BegLon,x,y)    ! x,y really I,J here
        iBeg = floor((x * deltax - xWwrf)/deltax + 1)
        jBeg = floor((y * deltax - ySwrf)/deltax + 1)
        if (EndLat == BegLat .and. EndLon == BegLon) then
           iEnd = iBeg ! Single-point data
           jEnd = jBeg
        else
           call latlon_to_ij(proj,EndLat,EndLon,x,y) ! x,y really I,J here
           iEnd = ceiling((x * deltax - xWwrf)/deltax)
           jEnd = ceiling((y * deltax - ySwrf)/deltax)
        end if
     endif
!
!-----If specified, set subset grid by Xlcc, Ylcc
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
!-----Select entire domain if requested (or trim -iBeg off edges)
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
        end if
        if (ijPt(i) == 3) then
           iPt(i) = floor((PtXlcc(i) - xWwrf)/deltax + 1)
           jPt(i) = floor((PtYlcc(i) - ySwrf)/deltax + 1)
        end if
     end do
!
!-----Sanity checks
!
     if (.not. get_precip_only) then
        call grid_in_grid(iBeg,iEnd,jBeg,jEnd,nx,ny,"WRF")
        do i = 1, NumOuts
           if (ijPt(i) /= 0) &
                call point_in_grid(iPt(i),jPt(i),iBeg,iEnd,jBeg,jEnd,i)
        end do
     end if
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
     yfmet = yfmet * deltax
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
     if (.not. get_precip_only) then ! calculate, but don't print
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
                   '     Grid parameters for: Output 1-D point  #',i, &
                   ' ',trim(OutFile(i))
!                   ' (',trim(OutForm(i)),' ',trim(OutType(i)),')'

              call latlon_to_ij(proj,ylat(iPt(i),jPt(i)),xlon(iPt(i),jPt(i)),x,y)
              x = nint(x * 2.)/2. ! round to nearest whole or half integer
              y = nint(y * 2.)/2.
              x = x * deltax - deltax/2.! in km
              y = y * deltax - deltax/2.! in km
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
     end if

  end if  ! if (lFirst) 
!
!-----Sometimes we just need to get the precip field for this hour
!
  if (get_precip_only) then

     call get_var_2d_real_cdf(cdfid,'RAINC', rainco, nx,ny,iTime,debug)
     call get_var_2d_real_cdf(cdfid,'RAINNC',rainro, nx,ny,iTime,debug)
     call TimeStamp2ymdh(TimeStamp,iyr,imo,idy,ihr)
     call add_hour(iyr,imo,idy,ihr,ibtz)     ! time zone shift
     
     write(*,'(a,i4,3i2.2,4a)') " Precipitation read at time: ",  &
          iyr,imo,idy,ihr, " LST = ",TimeStamp," UTC"

     return

  end if

!          
!-----Read time-variable fields
!  
  call get_var_3d_real_cdf(cdfid,'U',ustag,nx+1,ny,nz,iTime,debug)
  call get_var_3d_real_cdf(cdfid,'V',vstag,nx,ny+1,nz,iTime,debug)
!
!-----De-stagger the U,V winds while we're here
! 
  do k = 1,nz
     do j = 1,ny
        jp1 = j + 1
        do i = 1,nx
           ip1  =  i + 1
           uu(i,j,k) = 0.5*(ustag(i,j,k) + ustag(ip1,j,k))
           vv(i,j,k) = 0.5*(vstag(i,j,k) + vstag(i,jp1,k))
        enddo
     enddo
  enddo

  call get_var_3d_real_cdf(cdfid,'W',     ww,    nx,ny,nz+1,iTime,debug) ! vert V
  call get_var_3d_real_cdf(cdfid,"PH",    ph,    nx,ny,nz+1,itime,debug) ! pert
  call get_var_3d_real_cdf(cdfid,"PHB",   phb,   nx,ny,nz+1,iTime,debug) ! base
  call get_var_3d_real_cdf(cdfid,"P",     pa,    nx,ny,nz,iTime,debug) ! press
  call get_var_3d_real_cdf(cdfid,"PB",    pab,   nx,ny,nz,iTime,debug) ! base
  call get_var_3d_real_cdf(cdfid,'T',     tt,    nx,ny,nz,iTime,debug) ! temp
  call get_var_3d_real_cdf(cdfid,'QVAPOR',qq,    nx,ny,nz,iTime,debug) ! kg/kg
  call get_var_3d_real_cdf(cdfid,'QCLOUD',qcloud,nx,ny,nz,iTime,debug) ! kg/kg

  if (allocated(hold3d)) deallocate(hold3d)
  allocate(hold3d(nx,ny,nsoil))
  call get_var_3d_real_cdf(cdfid,'SMOIS', hold3d,nx,ny,nsoil,iTime,debug)
  smois(1:nx,1:ny) = hold3d(1:nx,1:ny,1)  ! save the top layer of soil moisture
  deallocate(hold3d) ! no need to allocate again, only used by read_mm5().

  call get_var_2d_real_cdf(cdfid,'LU_INDEX',rlu, nx,ny,iTime,debug) ! land-use
  call get_var_2d_real_cdf(cdfid,'MU',    ps,    nx,ny,iTime,debug) ! air mass
  call get_var_2d_real_cdf(cdfid,'MUB',   psb,   nx,ny,iTime,debug) ! base state
  call get_var_2d_real_cdf(cdfid,'RAINC', rainc, nx,ny,iTime,debug) ! convective
  call get_var_2d_real_cdf(cdfid,'RAINNC',rainr, nx,ny,iTime,debug) ! non-conv.
  call get_var_2d_real_cdf(cdfid,'TSK',   tsfc,  nx,ny,iTime,debug) ! skin temp
  call get_var_2d_real_cdf(cdfid,'PBLH',  pbl,   nx,ny,iTime,debug) ! mix height
  call get_var_2d_real_cdf(cdfid,'SWDOWN',qsw,   nx,ny,iTime,debug) ! shortwave
  call get_var_2d_real_cdf(cdfid,'GLW',   qlw,   nx,ny,iTime,debug) ! longwave
  call get_var_2d_real_cdf(cdfid,'UST',   ustar, nx,ny,iTime,debug) ! friction V
  call get_var_2d_real_cdf(cdfid,'U10',   u10,   nx,ny,iTime,debug) ! 10m U
  call get_var_2d_real_cdf(cdfid,'V10',   v10,   nx,ny,iTime,debug) ! 10m V
  call get_var_2d_real_cdf(cdfid,'SST',   sst,   nx,ny,iTime,debug) ! sea temp
  call get_var_2d_real_cdf(cdfid,'T2',    t2,    nx,ny,iTime,debug) ! 2m temp
  call get_var_2d_real_cdf(cdfid,'Q2',    q2,    nx,ny,iTime,debug) ! 2m humidity
  call get_var_2d_real_cdf(cdfid,'PSFC',  psfc,  nx,ny,iTime,debug) ! surf press
  call get_var_2d_real_cdf(cdfid,'HFX',   shflux,nx,ny,iTime,debug) ! heat flux
  call get_var_2d_real_cdf(cdfid,'LH',    lhflux,nx,ny,iTime,debug) ! evaporation

  call get_var_1d_real_cdf(cdfid,'ZNW',   sigma, nz+1,iTime,debug)  ! levels
  call get_var_1d_real_cdf(cdfid,'ZNU',   sigmid,nz,  iTime,debug)
!
! Set total (base+perturbation) airmass by layer according to vertical grid
!
  if (ihyb.eq.0) then
    do k = 1,nz
      psax(:,:,k) = psb(:,:) + ps(:,:)
    enddo
  else
    call get_var_1d_real_cdf(cdfid,'C1H',c1h,nz,iTime,debug)
    call get_var_1d_real_cdf(cdfid,'C2H',c2h,nz,iTime,debug)
    do k = 1,nz
      psax(:,:,k) = c1h(k)*(psb(:,:) + ps(:,:)) + c2h(k)
    enddo
  endif

  l10mw = .true. ! All WRF versions >= 2 have near-surface (u10,v10,T2,Q2 etc)
  lq2   = .true.
!
! Ask if this WRFOUT file has CLDFRA, cloud fraction
!
  rcode = nf_inq_varid(cdfid,'CLDFRA',id)
  if (rcode == nf_noerr) then
     call get_var_3d_real_cdf(cdfid,'CLDFRA',cldfra,nx,ny,nz,iTime,debug) ! unitless
     lCLDFRA = .true. ! we have CLDFRA in this file
  endif
!
! Ask if this WRFOUT file has QICE, the Ice mixing ratio
!
  rcode = nf_inq_varid(cdfid,'QICE',id)
  if (rcode == nf_noerr) then
     call get_var_3d_real_cdf(cdfid,'QICE',  qice,  nx,ny,nz,iTime,debug)
  else
     qice(1:nx,1:ny,1:nz) = 0. ! only used by cloud_cover()
  endif
!
! Ask if this WRFOUT file has ZNT, the TIME-VARYING ROUGHNESS LENGTH
!
  rcode = nf_inq_varid(cdfid,'ZNT',id)
  if (rcode == nf_noerr) then
     call get_var_2d_real_cdf(cdfid,'ZNT',z0,nx,ny,iTime,debug) ! in m
     lz0 = .true. ! we have z0 in this file
  endif
!
! Ask if this WRFOUT file has ALBEDO
!
  rcode = nf_inq_varid(cdfid,'ALBEDO',id)
  if (rcode == nf_noerr) then
     call get_var_2d_real_cdf(cdfid,'ALBEDO',albedo,nx,ny,iTime,debug)
     lalbedo = .true. ! we have albedo in this file
  endif
!
! Ask if this WRFOUT file has LAI
!
  rcode = nf_inq_varid(cdfid,'LAI',id)
  if (rcode == nf_noerr) then
     call get_var_2d_real_cdf(cdfid,'LAI',lai,nx,ny,iTime,debug)
     llai = .true. ! we have lai in this file
  endif
!
! Ask if this WRFOUT file has RMOL, 1/Monin Obukhov Length
!
  rcode = nf_inq_varid(cdfid,'RMOL',id)
  if (rcode == nf_noerr) then
     call get_var_2d_real_cdf(cdfid,'RMOL',mol,nx,ny,iTime,debug)
     mol  = 1./mol ! change 1/L to L
     lmol = .true. ! we have L in this file
  endif
!
! The variable XTIME is only available in WRF3 output, not in WRFv2 output.
! We'll calculate it always, even though it's available sometimes.
!
  SimStart = "" ! initialize: some compilations of netCDF don't blank the var
  rcode = nf_get_att_text(cdfid,nf_global,'SIMULATION_START_DATE',SimStart)
  call TimeStampDiff(trim(SimStart),trim(TimeStamp),i)
  xtime = i*60. ! in minutes, for compatibility with WRFv3's XTIME
!
!-----Get total pressure (mb), total P*, and convert temperature from 
!     potential to actual in K
!
  psfc = psfc / 100.                 ! convert Pa to mb
  pa   = (pa + pab)/100.             ! pressure in mb
  tt   = (tt + 300.)*(pa/1000.)**Rcp ! temperature in K
  ph   = (ph + phb)/grav             ! convert geopotential to height
!
!-----Find the height of each level, which can vary in both space and time
!
  zh(1:nx,1:ny,0) = 0. ! ground level
  do k = 1,nz
     do j = 1,ny
        do i = 1,nx
           zh(i,j,k) = ph(i,j,k+1) - ph(i,j,1) ! cell face heights
        enddo
     enddo
  enddo

  iswater = .false. ! initialize and re-create each time-stamp read

  do j = 1,ny
     do i = 1,nx
!
!-----Determine hourly incremental total precipitation rates (mm/hr)
!
        rtmpr = rainr(i,j)     ! WRF saves rain in mm (accumulated)
        rtmpc = rainc(i,j)
        if (xtime == 0. .or. nint(xtime/60.) == 1) then ! Forecast hour 0 or 1.
           rainr(i,j) = amax1(0., rtmpr)                ! Use rain, even if it's
           rainc(i,j) = amax1(0., rtmpc)                ! during spin-up period.
        else if (rainro(i,j) == -999.) then             ! Non-initialzed old rain
           if (i == 1 .and. j == 1) then
              write(*,*) "*** WARNING: Precipitation field at ",TimeStamp,&
                   " UTC set to zero,"
              write(*,*) "             because we don't have the previous ",&
                   "hour's precip to subtract."
              write(*,*)
           endif
           rainr(i,j) = 0.     ! What else can we do?  rain is the accumulated
           rainc(i,j) = 0.     ! rain, but we don't know last hour's value!
        else                   ! Normal case: not the first time-stamp read
           rainr(i,j) = amax1(0., rtmpr - rainro(i,j))
           rainc(i,j) = amax1(0., rtmpc - rainco(i,j))
        endif
        rainro(i,j) = rtmpr    ! Save the accumlated rain up till now
        rainco(i,j) = rtmpc    ! for next hour of this run (in cm)

        rain(i,j) = rainr(i,j) + rainc(i,j)
        if (rain(i,j) < 0.01) rain(i,j) = 0. ! Limit very small rain rates
!
!-----Landuse processing:
!
        ilu(i,j) = int(rlu(i,j))            ! convert to integer

        if (lucat(1:6) == "NLCD  ") then    ! NLCD50, run only by EPA, using an 
           water_cat =  1                   ! R script that did not set ISWATER
           lake_cat  = -1                   ! or ISICE correctly.  Fix that.
           ice_cat   =  2
        end if
!
!-----Do some re-mapping of water and land categores, for CALPUFF's sake,
!     following the values found in WRFv3.5.1 run/LANDUSE.TBL file.
!
        if ( ilu(i,j) == lake_cat) ilu(i,j) = water_cat ! re-map lakes to water
 
        if (lucat(1:6) == "NLCD  ") then           ! NLCD50

           ! Set the two other water categories to Open Water
           if ( ilu(i,j) == 31 .or. ilu(i,j) == 48 ) ilu(i,j) = water_cat
           ! set 'urban and built up' to 'Developed Medium Intensity'
           if ( ilu(i,j) == 44) ilu(i,j) = 5
           ! set 'permanent snow and ice' to 'Perennial Ice/Snow'
           if ( ilu(i,j) == 46) ilu(i,j) = 2
             
        else if (lucat(1:6) == "NLCD40") then      ! New in WRFv3.5

           ! set Open Water set to IGBP Water
           if ( ilu(i,j) == 21) ilu(i,j) = water_cat
           ! set Developed to Urban and Built-Up
           if ( ilu(i,j) >= 23 .and. ilu(i,j) <= 26) ilu(i,j) = 13 

        else if (lucat(1:5) == "MODIS") then

           ! Set Open Water set to IGBP Water
           if ( ilu(i,j) == 21) ilu(i,j) = water_cat 
           ! set residential/commercial to Urban and Built-Up
           if ( ilu(i,j) >= 31 .and. ilu(i,j) <= 33) ilu(i,j) = 13 

        else if (lucat == "MODIFIED_IGBP_MODIS_NOAH") then

           ! set residential/commercial to Urban and Built-Up
           if ( ilu(i,j) >= 31 .and. ilu(i,j) <= 33) ilu(i,j) = 13 

        end if
!
!-----Landuse, in particular sea-ice, changes with time.  Fill iswater every
!     time step we got from WRF.
!
        if ( ilu(i,j) == water_cat) iswater(i,j) = .true.
        if ( ilu(i,j) == ice_cat)   iswater(i,j) = .false. ! over-ride if ice

     enddo  ! do i = 1,nx
  enddo     ! do j = 1,ny
!
!-----First time through, print some useful into to the screen
!
  if (lFirst) then
!
!-----Tell the user if we found 10m winds, z0, albedo, L, LAI
!
     write(*,*) "In this WRF file:"
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
     write(*,*) "  WRF land-use data set: ",trim(lucat)
     write(*,'(a,i5)') "   Number of categories :",num_land_cat

     call print_met_levels

     lFirst = .false.

  endif

  return
end subroutine read_wrf
