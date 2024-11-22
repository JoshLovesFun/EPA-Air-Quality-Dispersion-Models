!Contains:
!
! subroutine scichem_useful(iUseful)
! subroutine medoc_hour(iUnit,binary)
! subroutine scichem_terrain(iUnit,fname)
! subroutine scichem_sampler_old(iUnit)
! subroutine scichem_sampler(iUnit)
!
!
!******************************************************************************
!
subroutine scichem_layers
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     Prints the output SCICHEM layers to the screen.
!
!     Development History:
!     2013-09-27  Original Development (ENVIRON International Corp), extracted
!                 from scichem_header().
!
!------------------------------------------------------------------------------
!
  USE met_fields
  implicit none
!
!-----Variable declaration
!
  integer :: k
!
!-----Entry point
!
! write the final levels being written, to the screen
!
  write(*,'(a)') " Output MET-to-SCICHEM layers (for a point at sea level):"
  write(*,'(a)') "   Level  Interface(m)     Center(m)"
  write(*,'(8x,f14.2)') zface(0)
  do k = 1, nzOut
     write(*,'(i8,2f14.2)') k,zface(k),zmid(k)
  end do
  write(*,*)

  return
end subroutine scichem_layers
!
!******************************************************************************
!
subroutine scichem_useful(iUnit,fnameU,fname1)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     Writes some information useful in a SCICHEM *.INP and *.MSC file.
!
!     Development History:
!     2011-09-30  New with MMIF v2.0
!     2012-10-01  Bug fix: ZiMin = ZiMax = -999 always.  Fix that.
!     2013-01-22  Removed ZB, changed ZMAX from zface(nzOut) to zmid(nzOut).
!     2013-09-21  All non-hourly outputs now open and close their files in
!                 their subroutines.
!     2015-02-24  Updated Useful file based on discussions with SCICHEM authors.
!     2016-09-15  Updated Useful file based on discussions with SCICHEM authors.
!
!------------------------------------------------------------------------------
!
  USE met_fields
  implicit none
!
!-----Variable declaration
!
  character (len=*), intent(in) :: fnameU ! Filename of Useful Info file
  character (len=*), intent(in) :: fname1 ! Filename of the MEDOC file
  integer,           intent(in) :: iUnit  ! Logical unit for output

  integer :: ni,nj, iy,im,id,ih
  real    :: xMin,xMax, yMin,yMax
!
!-----Format statements
!
1 format(a)
2 format(a," = ",f9.3,",")
3 format(a," = ",i9,",")
4 format(a,f9.3)
5 format(a,i5,3i3,i6,3i3)
!
!-----Entry point
!
  ni = iEnd-iBeg+1
  nj = jEnd-jBeg+1

  xMin = x0met+deltax/2.      ! format requires grid cell centers, not the 
  yMin = y0met+deltax/2.      ! grid edges (corners) like calmet requires

  xMax = xMin + (ni-1)*deltax
  yMax = yMin + (nj-1)*deltax

  open(iUnit,file=fnameU,status='unknown')

  write(iUnit,1) "KEYWORD FORMAT SETTINGS (preferred/easier interface):"
  write(iUnit,*)
  write(iUnit,*) "CO STARTING"
  write(iUnit,4) "DOMIAN   UTMxMin UTMxMax UTMyMin UTMyMAX ",zmid(nzOut)
  write(iUnit,5) "TIMEZONE ",ibtz
  iy=ibyr; im=ibmo; id=ibdy; ih=ibhr
  call add_hour(iy,im,id,ih,-1) ! must start SCICHEM an hour before first output
  write(iUnit,5) "STARTEND ",iy,im,id,ih, ieyr,iemo,iedy,iehr
  write(iUnit,1) "TIMEREF  LOCAL"
  write(iUnit,1) "AVERTIME 3600 SECONDS" ! *** FIXME for sub-hourly timesteps
  write(iUnit,1) "MAXTSTEP  900 SECONDS"
  write(iUnit,1) "CO FINISHED"
  write(iUnit,*)
  write(iUnit,1) "ME STARTING"
  write(iUnit,1) "ME METFILE MEDOC_LIST MEDOC_files.list"
  write(iUnit,1) "ME TIMEREF UTC"
  write(iUnit,1) "ME FINISHED"
  write(iUnit,*)
  write(iUnit,1) "The file MEDOC_files.list should have the following format:"
  write(iUnit,1) "MEDOC_LIST"
  write(iUnit,1) "PATH=/path/to/file/location/"
  write(iUnit,1) OutFile(1)
  write(iUnit,1) "(Append other files here)"

  write(iUnit,*)
  write(iUnit,*)
  write(iUnit,1) "NAMELIST FORMAT SETTINGS (advanced interface):"
  write(iUnit,1) "The DOMAIN below is the lower-left and upper-right corners of "
  write(iUnit,1) "the WRF sub-domain. The SCICHEM domain must fit inside this domain."
  write(iUnit,1) "Delete XMIN,XMAX,YMIN,YMAX from &DOMAIN to have SCICHEM "
  write(iUnit,1) "automatically use the largest possible domain."
  write(iUnit,*) 
  write(iUnit,1) "&DOMAIN"
  write(iUnit,1) "     CMAP =  'LATLON',"
  write(iUnit,2) "     XMIN", xlon(iBeg,jBeg)
  write(iUnit,2) "     XMAX", xlon(iEnd,jEnd)
  write(iUnit,2) "     YMIN", ylat(iBeg,jBeg)
  write(iUnit,2) "     YMAX", ylat(iEnd,jEnd)
  write(iUnit,2) "     ZMAX", zmid(nzOut)
  write(iUnit,*) 
  write(iUnit,1) "In the *.MSC file:"
  write(iUnit,1) "For LOCAL_MET, F means data in UTC, T means in LST."
  write(iUnit,1) ""
  write(iUnit,1) "&MET"
  write(iUnit,1) "     MET_TYPE  = 'MEDOC_LIST',"
  write(iUnit,1) "     BL_TYPE   = 'OPER',"
  write(iUnit,1) "     ENSM_TYPE = 'OPER3.1'," 
  write(iUnit,1) "     LOCAL_MET = F," ! F means data in UTC, T means in LST
  write(iUnit,2) "     ZIMIN", zmid(1)
  write(iUnit,2) "     ZIMAX", zmid(nzOut)
  write(iUnit,3) "     NZB  ", nzOut
  write(iUnit,1) "     PR_TYPE =  'METFILE',"
  write(iUnit,1) "/"
  write(iUnit,1) "@019MEDOC_file_list.txt"
  write(iUnit,*) 
  write(iUnit,1) "The 3 characters after the @ sign are the length of the"
  write(iUnit,1) "filename that follows, 'MEDOC_file_list.txt' in this example."
  write(iUnit,1) "That file should contain a header line 'MEDOC' and the list"
  write(iUnit,1) "of filenames, one per line. This is useful when the MEDOC files "
  write(iUnit,1) "for an annual simulation are provided as monthly files rather than "
  write(iUnit,1) "a single annual file. The example below shows only one file but an "
  write(iUnit,1) "annual simulation would have a series of files for the other months."
  write(iUnit,1) "This example gives only one file:"
  write(iUnit,*) 
  write(iUnit,1) "MEDOC"
  write(iUnit,1) trim(fname1)

  close(iUnit)
  
  return
end subroutine scichem_useful
!
!******************************************************************************
!
subroutine medoc_header(iUnit,binary)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     Writes the header section to the SCICHEM MEDOC file.
!     See the SCICHEM User's Guide, under "MEDOC Format".
!
!     Development History:
!     2014-02-30  New with MMIF v3.1.  Added support for SCICHEM 3.0 headers
!
!------------------------------------------------------------------------------
!
  USE met_fields
  USE functions
  implicit none
!
!-----Variable declaration
!
  integer, intent(in) :: iUnit         ! Logical unit for output
  logical, intent(in) :: binary        ! Write a text or binary file?
!
!-----Format statements, from SCICHEM User's Guide Section 5.2
!
9001 FORMAT(6(A8,1X))
9004 FORMAT("# ",A21," = ",8g12.6)
9005 FORMAT("# ",A21," = ",8i12)
9006 FORMAT("# ",A21," = ",A)
!
!-----Entry point
!
  if (binary) then ! write output in binary format

     if (pmap == "LCC") then
        write(iUnit) 'BLAMBERT'
        write(iUnit) tlat1,tlat2   ! two true lats for Lambert Conformal
     else if (pmap == "PS") then
        write(iUnit) 'BPOLAR  '
        write(iUnit) tlat1         ! only one true lat for Polar Stereographic
     else if (pmap == "EM") then
        write(iUnit) 'BMERCATR'    
        write(iUnit) tlat1         ! only one true lat for Mercator
     end if

     write(iUnit) 'SIGMAZ  '       ! Vertical Coordinate

     write(iUnit) 0,0              ! No. Staggered 3d and 2d fields

  else ! write output in TEXT (ASCII) format

     if (pmap == 'LCC') then
        write(iUnit,9001) 'FLAMBERT'
        write(iUnit,9004) 'Map Parameters       ',tlat1,tlat2
     else if (pmap == 'PS') then
        write(iUnit,9001) 'FPOLAR  '
        write(iUnit,9004) 'Map Parameters       ',tlat1
     else if (pmap == 'EM') then
        write(iUnit,9001) 'FMERCATR'
        write(iUnit,9004) 'Map Parameters       ',tlat1
     end if

     write(iUnit,9006)    'Vertical Coordinate  ','SIGMAZ  '

     write(iUnit,9005)    'No. Staggered Fields ',0,0

  end if

end subroutine medoc_header
!
!******************************************************************************
!
subroutine medoc_hour(iUnit,binary)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     Writes data to the SCICHEM MEDOC file.
!     See section 5.2 of the SCICHEM User's Guide.
!
!     Development History:
!     2011-09-30  New with MMIF v2.0
!     2013-01-22  Force vertical velocity at H = zmid(nzOut) to be zero.
!                 Also see changes in interpolate.f90, for a bug fix related
!                 to the vertical grid structure in SCICHEM.
!     2014-03-06  In SCICHEM 3.0: LAT,LON now where X,Y = 0,0 (cenlat,cenlon).
!     2014-03-06  Added MetForm, z0, ustar, albedo, and bowen to the output.
!     2014-07-30  Suppress output past last requested to accomodate PtZone(iOut).
!     2014-09-18  Move calls to pbl_limits to output routines, instead of main().
!     2015-02-24  MEDOC time-stamps now forced to be in GMT, so LOCAL_MET = F.
!     2017-01-24  Bug fix: cenlat,cenlon should be origlat,origlon.
!     2017-06-23  Added LAI to MEDOC file, to support SCICHEM 3.2.
!     2018-04-04  Added units to MEDOC file, supported by SCICHEM 3.2.
!     2020-11-30  Changed format statement for ASCII MEDOC files to avoid
!                 rounding roughness length values to 0.0000 (impossible value).
!     2020-12-15  Set PBL to be not less than Venkatram (1980) mechanical 
!                 mixing height. 
!
!------------------------------------------------------------------------------
!
  USE met_fields
  USE functions
  USE parse_control, ONLY : iEndDat
  implicit none
!
!-----Variable declaration
!
  integer, intent(in)  :: iUnit                     ! Logical unit for output
  logical, intent(in)  :: binary                    ! Write a text or binary file?

  integer              :: iyr,imo,idy,ihr,imin,isec ! time stamps
  integer              :: i,j,k
  integer,   parameter :: NREPER = 0                ! Not used in SCICHEM
  integer,   parameter :: NVAR3D = 6, NVAR2D = 9    ! Number of 3D and 2D vars
  integer,   parameter :: idum = 0                  ! dummies
  real,      parameter :: dum  = 0.
  character (len=8), parameter         :: NAMDUM = "IGNORED "
  character (len=8), dimension(NVAR3D) :: NAM3D
  character (len=8), dimension(NVAR3D) :: UNI3D
  character (len=8), dimension(NVAR2D) :: NAM2D
  character (len=8), dimension(NVAR2D) :: UNI2D
  real, allocatable, dimension(:,:,:)  :: wTmp      ! container for interp'd wOut
  real, allocatable, dimension(:,:)    :: pblTmp    ! container for output mixing ht
  real                                 :: ZiMech    ! mechanical mixed layer ht (m)
  real                                 :: wstarOut  ! protoect from change
!
!-----Format statements, from SCICHEM User's Guide Section 5.2
!
9001 FORMAT(6(A8,1X))
9002 FORMAT(6(I12,1X))
9003 FORMAT(6(F12.6,1X))
!
!-----Entry point
!
  call nDatHr2ymdh(nDatHr,iyr,imo,idy,ihr,24) ! SCICHEM uses hour num 1-24
  call add_hour(iyr,imo,idy,ihr,-ibtz)        ! MEDOC files should be in GMT
  call legal_timestamp(iyr,imo,idy,ihr,24)    ! SCICHEM uses hour num 1-24
  imin = 0 ! FIXME: assumes all MM5/WRF data is output on the hour
  isec = 0
  if (nDatHr > iEndDat) return ! after last output for this timezone (ibtz)
!
!-----SCICHEM requires that W = 0 (no vertical velocity) at the top of the 
!     output (MEDOC) domain, which is at H = D = SZ(nzOut) = zmid(nzOut).
!     Also, the MEDOC file format can only handle the case where ALL the 
!     velocity components (including horizontal component, presumably) are
!     staggered w.r.t. the grid cell centers, or where NONE of them are. 
!     So we'll write an un-staggered grid by interpolating W to the grid
!     cell centers.
!
!-----CALPUFF and SCICHEM bomb if the PBL depth is outside of the modeling
!     domain, so limit to be between the lowest and highest layer mid-points 
!     (same as MCIP does).
!
  allocate( wTmp(nx,ny,nzOut) ) 
  allocate( pblTmp(nx,ny)     ) 
  pblTmp = PBL ! the whole field

  do j = jBeg, jEnd
     do i = iBeg, iEnd
        do k = 1, nzOut-1
           wTmp(i,j,k) = (wOut(i,j,k)+wOut(i,j,k-1)) / 2. ! interp to cell center
        end do
        wTmp(i,j,nzOut) = 0.                  ! zero vertical velocity in the top layer

        ! Turns out this is a bad idea: ZiMech is quite often > PBL
        ! if (CalcPBL == "MMIF") then
        !    call venkatram_mech_mixh(ustar(i,j),ZiMech)
        !    if (mol(i,j) < 0.) then                     ! convective hours
        !       pblTmp(i,j) = max( ZiMech, pbl(i,j) )
        !    else                                        ! stable hours
        !       pblTmp(i,j) = ZiMech
        !    endif
        ! endif

        wstarOut = wstar(i,j)                 ! protect from change, not written
        call pbl_limits(zmid(1),zmid(nzOut), mol(i,j),ustar(i,j),wstarOut, &
             pblTmp(i,j))
     end do
  end do
!
!-----Set up the output names, both 3-D and 2-D
!
  NAM3D(1) = 'U       ' ! uOut x-compoenent of wind speed
  UNI3D(1) = 'M/S     ' ! (m/s)
  NAM3D(2) = 'V       ' ! vOut y-component of wind speed 
  UNI3D(2) = 'M/S     ' ! (m/s)
  NAM3D(3) = 'W       ' ! wOut vertical wind speed 
  UNI3D(3) = 'M/S     ' ! (m/s)
  NAM3D(4) = 'TA      ' ! tout absolute air temperature 
  UNI3D(4) = 'K       ' ! (K)
  NAM3D(5) = 'H       ' ! qOut humidity ratio
  UNI3D(5) = 'G/G     ' ! (g/g or kg/kg)
  NAM3D(6) = 'CLD     ' ! cloud liquid water content
  UNI3D(6) = 'G/G     ' ! (g/g or kg/kg)

  NAM2D(1) = 'TOPO    ' ! terrain elevation 
  UNI2D(1) = 'M       ' ! (m)
  NAM2D(2) = 'ZI      ' ! PBL height 
  UNI2D(2) = 'M       ' ! (m)
  NAM2D(3) = 'HFLX    ' ! sensible heat flux 
  UNI2D(3) = 'W/M2    ' ! (W/m^2)
  NAM2D(4) = 'PRECIP  ' ! precipitation rate 
  UNI2D(4) = 'MM/HR   ' ! (mm/hr)
  NAM2D(5) = 'CC      ' ! fractional cloud cover (dimensionless)
  UNI2D(5) = 'FRACTION' ! 
  NAM2D(6) = 'ZRUF(T) ' ! surface roughness, time-varying
  UNI2D(6) = 'M       ' ! (m)
  NAM2D(7) = 'USTAR   ' ! surface friction velocity 
  UNI2D(7) = 'M/S     ' ! (m/s)
  NAM2D(8) = 'ALBEDO  ' ! surface albedo 
  UNI2D(8) = 'DIMLESS ' ! (dimensionless)
  NAM2D(9) = 'LAI     ' ! leaf area index 
  UNI2D(9) = 'DIMLESS ' ! (dimensionless)

  if (binary) then ! write output in binary format

     write(iUnit) 'BBBBBBBB'
     write(iUnit) MetForm//'     ', 'F       '       ! Model, lstagger
     write(iUnit) idy,imo,iyr,ihr,imin,isec          ! rec 3 timestamp
!                 jday jmth jyr  jhr  jmin jsec
     write(iUnit) idum,idum,idum,idum,idum,idum      ! rec 4 not used in SCICHEM
!                 IMAX        JMAX        KMAX  NREPER NVAR3D NVAR2D
     write(iUnit) iEnd-iBeg+1,jEnd-jBeg+1,nzOut,NREPER,NVAR3D,NVAR2D ! rec 5
     write(iUnit) idum,idum,idum,idum,idum,idum      ! rec 6 not used in SCICHEM
     write(iUnit) idum,idum,idum                     ! rec 7 not used in SCICHEM
!                 SZ                  DX(m)      DY(m)
     write(iUnit) (zmid(k),k=1,nzOut),deltax*1000.,deltax*1000.,   &
!         X0(km)          Y0(km)   X0,Y0 is the lower-left grid cell CENTER
          x0met+deltax/2.,y0met+deltax/2.,                         &
!         LAT,LON is where X,Y = 0,0    ZTOP, aka H
          origlat,origlon,dum,dum,dum,dum,zmid(nzOut)! rec 8
     write(iUnit) (NAMDUM,   i=1,NREPER),       &    ! rec 9 names, units
                  (NAM3D(i), i=1,NVAR3D),(UNI3D(i),i=1,NVAR3D),      &
                  (NAM2D(i), i=1,NVAR2D),(UNI2D(i),i=1,NVAR2D)
!     write(iUnit) (dum,i=1,3*NREPER)                ! rec 10 not used in SCICHEM
     
     write(iUnit) (((uOut(i,j,k), i=iBeg,iEnd),j=jBeg,jEnd),k=1,nzOut)
     write(iUnit) (((vOut(i,j,k), i=iBeg,iEnd),j=jBeg,jEnd),k=1,nzOut)
     write(iUnit) (((wTmp(i,j,k), i=iBeg,iEnd),j=jBeg,jEnd),k=1,nzOut) ! wOut
     write(iUnit) (((tOut(i,j,k), i=iBeg,iEnd),j=jBeg,jEnd),k=1,nzOut)
     write(iUnit) (((qOut(i,j,k), i=iBeg,iEnd),j=jBeg,jEnd),k=1,nzOut)
     write(iUnit) (((qcOut(i,j,k),i=iBeg,iEnd),j=jBeg,jEnd),k=1,nzOut)

     write(iUnit) ((topo(i,j),    i=iBeg,iEnd),j=jBeg,jEnd)
     write(iUnit) ((pblTmp(i,j) , i=iBeg,iEnd),j=jBeg,jEnd)
     write(iUnit) ((shflux(i,j),  i=iBeg,iEnd),j=jBeg,jEnd)
     write(iUnit) ((rain(i,j),    i=iBeg,iEnd),j=jBeg,jEnd)
     write(iUnit) ((cldcvr(i,j),  i=iBeg,iEnd),j=jBeg,jEnd)
     write(iUnit) ((z0(i,j),      i=iBeg,iEnd),j=jBeg,jEnd)
     write(iUnit) ((ustar(i,j),   i=iBeg,iEnd),j=jBeg,jEnd)
     write(iUnit) ((albedo(i,j),  i=iBeg,iEnd),j=jBeg,jEnd)
     write(iUnit) ((lai(i,j),     i=iBeg,iEnd),j=jBeg,jEnd)

  else ! write output in TEXT (ASCII) format

     write(iUnit,9001) 'FFFFFFFF'                    ! rec 1 format flag
     write(iUnit,9001) MetForm//'     ','FALSE   '   ! Model, lstagger
!                     jday jmth jyr jhr jmin jsec
     write(iUnit,9002) idy,imo,iyr,ihr,imin,isec     ! rec 3 timestamp
     write(iUnit,9002) idum,idum,idum,idum,idum,idum ! rec 4 not used in SCICHEM
!                      IMAX        JMAX        KMAX  NREPER NVAR3D NVAR2D
     write(iUnit,9002) iEnd-iBeg+1,jEnd-jBeg+1,nzOut,NREPER,NVAR3D,NVAR2D ! rec 5
     write(iUnit,9002) idum,idum,idum,idum,idum,idum ! rec 6 not used in SCICHEM
     write(iUnit,9002) idum,idum,idum                ! rec 7 not used in SCICHEM
!                      SZ                  DX(m)       DY(m)  
     write(iUnit,9003) (zmid(k),k=1,nzOut),deltax*1000,deltax*1000,  &
!         X0(km)          Y0(km)    X0,Y0 is the lower-left grid cell CENTER
          x0met+deltax/2.,y0met+deltax/2.,               &
!         LAT,LON is where X,Y = 0,0    ZTOP, aka H
          origlat,origlon,dum,dum,dum,dum,zmid(nzOut)! rec 8
     write(iUnit,9001) (NAMDUM,   i=1,NREPER), &     ! rec 9 names and units
                       (NAM3D(i), i=1,NVAR3D),(UNI3D(i),i=1,NVAR3D), &
                       (NAM2D(i), i=1,NVAR2D),(UNI2D(i),i=1,NVAR2D)
!     write(iUnit,9003) (dum,i=1,3*NREPER)           ! rec 10 not used in SCICHEM
     
     write(iUnit,9003) (((uOut(i,j,k), i=iBeg,iEnd),j=jBeg,jEnd),k=1,nzOut)
     write(iUnit,9003) (((vOut(i,j,k), i=iBeg,iEnd),j=jBeg,jEnd),k=1,nzOut)
     write(iUnit,9003) (((wTmp(i,j,k), i=iBeg,iEnd),j=jBeg,jEnd),k=1,nzOut) !wOut
     write(iUnit,9003) (((tOut(i,j,k), i=iBeg,iEnd),j=jBeg,jEnd),k=1,nzOut)
     write(iUnit,9003) (((qOut(i,j,k), i=iBeg,iEnd),j=jBeg,jEnd),k=1,nzOut)
     write(iUnit,9003) (((qcOut(i,j,k),i=iBeg,iEnd),j=jBeg,jEnd),k=1,nzOut)

     write(iUnit,9003) ((topo(i,j),    i=iBeg,iEnd),j=jBeg,jEnd)
     write(iUnit,9003) ((pblTmp(i,j),  i=iBeg,iEnd),j=jBeg,jEnd)
     write(iUnit,9003) ((shflux(i,j),  i=iBeg,iEnd),j=jBeg,jEnd)
     write(iUnit,9003) ((rain(i,j),    i=iBeg,iEnd),j=jBeg,jEnd)
     write(iUnit,9003) ((cldcvr(i,j),  i=iBeg,iEnd),j=jBeg,jEnd)
     write(iUnit,9003) ((z0(i,j),      i=iBeg,iEnd),j=jBeg,jEnd)
     write(iUnit,9003) ((ustar(i,j),   i=iBeg,iEnd),j=jBeg,jEnd)
     write(iUnit,9003) ((albedo(i,j),  i=iBeg,iEnd),j=jBeg,jEnd)
     write(iUnit,9003) ((lai(i,j),     i=iBeg,iEnd),j=jBeg,jEnd)

  endif

  deallocate( wTmp )
  deallocate( pblTmp )

  return
end subroutine medoc_hour
!
!******************************************************************************
!
subroutine scichem_terrain(iUnit,fname)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     Writes the ASCII (text) Terrain file for SCICHEM.  See section 5.4 
!     of the SCICHEM User's Guide.  Note that this file cannot be used with
!     a MEDOC file (which contains its own copy of the terrain) but this
!     subroutine is kept here in case it becomes useful some day in the future.
!
!     Development History:
!     2011-09-30  New with MMIF v2.0.
!     2012-01-31  Bug fix: was writing the whole grid, not just sub-grid.
!     2013-09-21  All non-hourly outputs now open and close their files in
!                 their subroutines.
!
!------------------------------------------------------------------------------
!
  USE met_fields
  USE functions
  implicit none
!
!-----Variable declaration
!
  character (len=*), intent(in) :: fname ! filename of output file
  integer, intent(in) :: iUnit           ! Logical unit for output
  integer             :: i,j
!
!-----Entry point
!
  open(iUnit,file=fname,status='unknown')

  write(iUnit,*) "KM      ",xlon(iBeg,jBeg),ylat(iBeg,jBeg)
  write(iUnit,*) x0met+deltax/2.,y0met+deltax/2.,deltax,deltax, nx,ny
  write(iUnit,'(12i6)') ((nint(topo(i,j)),i=iBeg,iEnd),j=jBeg,jEnd)

  close(iUnit)

  return
end subroutine scichem_terrain
!
!******************************************************************************
!
subroutine scichem_sampler_old(iUnit,fname)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     Writes an ASCII (text) Sampler Location (receptor) file for SCICHEM.  
!     See section 4.6 of the 2010 version of the SCICHEM User's Guide.  
!     Note: a newer format for the Sampler file was developed in the 2012
!           version of SCICHEM.
!
!     Development History:
!     2012-01-31  New with MMIF v2.1.
!     2013-09-21  All non-hourly outputs now open and close their files in
!                 their subroutines.
!
!------------------------------------------------------------------------------
!
  USE met_fields
  USE functions
  implicit none
!
!-----Variable declaration
!
  character (len=*), intent(in) :: fname ! filename of output file
  integer, intent(in) :: iUnit         ! Logical unit for output
  integer             :: i,j,k
!
!-----Entry point
!
  open(iUnit,file=fname,status='unknown')

  write(iUnit,*) "MATNAME ",0
  do k = 1, nzOut
     do j = jBeg, jEnd
        do i = iBeg, iEnd
           write(iUnit,*) &
                x0met+deltax/2. + (i-iBeg)*deltax,  & ! km
                y0met+deltax/2. + (j-jBeg)*deltax,  & ! km
                zmid(k)*(1. - topo(i,j)/zmid(nzOut))  ! m, transformed
        end do
     end do
  end do

  close(iUnit)

  return
end subroutine scichem_sampler_old
!
!******************************************************************************
!
subroutine scichem_sampler(iUnit,fname)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     Writes an ASCII (text) Sampler Location (receptor) file for SCICHEM.  
!     See section 4.5 of the SCICHEM-2012 User's Guide.  
!
!     Development History:
!     2013-01-25  New with MMIF v2.3.
!     2013-09-21  All non-hourly outputs now open and close their files in
!                 their subroutines.
!
!------------------------------------------------------------------------------
!
  USE met_fields
  USE functions
  implicit none
!
!-----Variable declaration
!
  character (len=*), intent(in) :: fname ! filename of output file
  integer, intent(in) :: iUnit           ! Logical unit for output
  integer             :: i,j,k
!
!-----Entry point
!
  open(iUnit,file=fname,status='unknown')

  write(iUnit,'(a)') "SCIPUFF SENSOR"
  do k = 1, nzOut
     do j = jBeg, jEnd
        do i = iBeg, iEnd
           write(iUnit,*) &
                x0met+deltax/2. + (i-iBeg)*deltax,    & ! km
                y0met+deltax/2. + (j-jBeg)*deltax,    & ! km
                zmid(k)*(1. - topo(i,j)/zmid(nzOut)), & ! m, transformed
                "MET"  ! or "MET TURB" 
        end do
     end do
  end do

  close(iUnit)

  return
end subroutine scichem_sampler
