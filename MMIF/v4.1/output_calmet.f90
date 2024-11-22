!Contains:
!
! subroutine calmet_layers(nzFinal)
! subroutine calmet_useful(iUnit,fname,fname1,nzFinal)
! subroutine calmet_header(iUnit,Aux,calmet_version,nzFinal)
! subroutine calmet_hour(iUnit,Aux,calmet_version,nzFinal)
! subroutine calmet_terrain(iUnit,fname)
!
!******************************************************************************
!
subroutine calmet_layers(nzFinal)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     Adjusts the calmet layers to make sure the first layers is 0:20 meters.
!     Only relevant for the case of Aggregation, not Interpolation.  The main
!     program checks that when using Interpolation, the first level is 0:20m.
!
!     Development History:
!     2013-09-20  Original Development (ENVIRON International Corp), extracted
!                 from calmet_header().
!
!------------------------------------------------------------------------------
!
  USE met_fields
  USE functions
  implicit none
!
!-----Variable declaration
!
  integer, intent(out) :: nzFinal            ! CALMET-specific final num layers
  integer              :: k
!
!-----Entry point
!
  if (allocated(zfaceC)) then
     write(*,*) "Programming error: zfaceC already allocated in calmet_header"
     stop
  endif

  if (zface(1) /= 20.) then ! don't fix it if it ain't broken!
!
!-----Force vertical grid structure to a 20 m deep first layer in CALPUFF
!
     write(*,'(a)') " Final MET-to-CALMET layer mapping (ZFACE values): "
     if (zface(1) < 19. .and. zface(2) < 30.) then
        write(*,'(a,f7.3,a)') "    Action: removing level at ",zface(1)," m."
        nzFinal = nzOut - 1
        allocate(zfaceC(0:nzFinal))
        zfaceC(0) = zface(0)
        zfaceC(1) = 20.
        do k = 2,nzFinal
           zfaceC(k) = zface(k+1)
        end do
     elseif ((zface(1) <  19. .and. zface(2) >= 30.) .or. &
          (zface(1) >= 19. .and. zface(1) <  30.)) then
        write(*,'(a,f7.3,a)') "    Action: moving level from ",zface(1), &
             " to 20 m."
        nzFinal = nzOut
        allocate(zfaceC(0:nzFinal))
        zfaceC(0) = zface(0)
        zfaceC(1) = 20.
        do k = 2,nzFinal
           zfaceC(k) = zface(k)
        end do
     else ! zface(1) >= 19. .and. zface(2) >= 30.
        write(*,'(a)') "    Action: adding a level at 20 m."
        nzFinal = nzOut + 1
        allocate(zfaceC(0:nzFinal))
        zfaceC(0) = zface(0)
        zfaceC(1) = 20.
        do k = 2,nzFinal
           zfaceC(k) = zface(k-1)
        end do
     endif
  else ! if (zface(1) /= 20.) then
     nzFinal = nzOut
     allocate(zfaceC(0:nzFinal))
     zfaceC = zface ! copy 
  endif
!
!-----Round zfaceC values to 2 decimal places (1 cm), so Useful 
!     Info File and CALMET/AUX files have the exact same values.
!     If the CALPUFF.INP and CALMET.DAT zface values don't match,
!     CALPUFF refuses to run.
!
  do k = 1,nzFinal
     zfaceC(k) = 0.01 * nint(100*zfaceC(k))
  end do
!
!-----Write out the values to the screen
!
  if (iVertMap == 0) then ! aggregation
     write(*,'(a)') "   Level         Sigma       Initial         Final"
     do k = 0, max(nzOut,nzFinal)
        if (k <= nzOut) then
           write(*,'(i8,f14.6,f14.2,$)') k,sigma(k),zface(k)
        else
           write(*,'(i8,a14,a14,$)') k," "," "
        endif
        if (k <= nzFinal) then
           write(*,'(f14.2)') zfaceC(k)
        else
           write(*,'(a14)') " "
        endif
     end do
  else                    ! interpolation
     write(*,'(a)') " Output MET-to-CALMET layers: "
     write(*,'(a)') "   Level      ZFACE(m)     Center(m)"
     write(*,'(8x,f14.2)') zface(0)
     do k = 1, nzOut
        write(*,'(i8,2f14.2)') k,zface(k),zmid(k)
     end do
  endif
  write(*,*)

  return
end subroutine calmet_layers
!
!******************************************************************************
!
subroutine calmet_useful(iUnit,fname,fname1,nzFinal)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     Writes the Useful Info file for calmet.
!
!     Development History:
!     2013-09-20  Original Development (ENVIRON International Corp), extracted
!                 from calmet_header().
!     2017-01-24  Bug fix: cenlat,cenlon should be origlat,origlon.
!     2017-09-22  Added beg/end timestamps and run-length to calpuff useful file.
!     2018-02-07  Increased significant figures for DGRIDKM to support <1km grids.
!
!------------------------------------------------------------------------------
!
  USE met_fields
  USE functions
  implicit none
!
!-----Variable declaration
!
  character (len=*), intent(in) :: fname  ! filename of Useful info file
  character (len=*), intent(in) :: fname1 ! filename of output #1, CALMET.DAT
  integer,   intent(in) :: iUnit          ! fortran output unit
  integer,   intent(in) :: nzFinal        ! CALMET-specific final num layers
  integer               :: k              ! local indexing var
!
!-----Entry point
!-----Write vertical grid structure to optional useful info file 
!
  open(iUnit,file=fname,status='unknown')

  write(iUnit,'(a,i5,a)') '! IBYR = ',ibyr,' !'
  write(iUnit,'(a,i3,a)') '! IBMO = ',ibmo,' !'
  write(iUnit,'(a,i3,a)') '! IBDY = ',ibdy,' !'
  write(iUnit,'(a,i3,a)') '! IBHR = ',ibhr,' !'
  write(iUnit,*)
  write(iUnit,'(a,i5,a)') '! IEYR = ',ieyr,' !'
  write(iUnit,'(a,i3,a)') '! IEMO = ',iemo,' !'
  write(iUnit,'(a,i3,a)') '! IEDY = ',iedy,' !'
  write(iUnit,'(a,i3,a)') '! IEHR = ',iehr,' !'
  write(iUnit,*)
  write(iUnit,'(a,i3,a)') '! XBTZ = ',ibtz,' !'
  write(iUnit,'(a,i8,a)') '! IRLG = ',irlg,' !'
  write(iUnit,*)
  write(iUnit,'(3a)')     '! METDAT = ',trim(fname1),' !'
  write(iUnit,'(3a)')     '! PMAP   = ',trim(pmap),' !'
  if (origlat >= 0) then
     write(iUnit,'(a,f8.3,a)') '! RLAT0 = ',origlat,'N !'
  else
     write(iUnit,'(a,f8.3,a)') '! RLAT0 = ',-origlat,'S !'
  endif
  if (origlon >= 0) then
     write(iUnit,'(a,f8.3,a)') '! RLON0 = ',origlon,'E !'
  else
     write(iUnit,'(a,f8.3,a)') '! RLON0 = ',-origlon,'W !'
  endif
  if (tlat1 >= 0) then
     write(iUnit,'(a,f8.3,a)') '! XLAT1 = ',tlat1,'N !'
  else
     write(iUnit,'(a,f8.3,a)') '! XLAT1 = ',-tlat1,'S !'
  endif
  if (tlat2 >= 0) then
     write(iUnit,'(a,f8.3,a)') '! XLAT2 = ',tlat2,'N !'
  else
     write(iUnit,'(a,f8.3,a)') '! XLAT2 = ',-tlat2,'S !'
  endif
  write(iUnit,'(3a)')      '! DATUM = ',trim(datum),' !'
  write(iUnit,'(a,i3,a)')  '! NX = ',iEnd-iBeg+1,' !'
  write(iUnit,'(a,i3,a)')  '! NY = ',jEnd-jBeg+1,' !'
  write(iUnit,'(a,i3,a)')  '! NZ = ',nzFinal,' !'
  write(iUnit,'(a,f12.6,a)')'! DGRIDKM = ',deltax,' !'

  write(iUnit,'(a,$)')     '! ZFACE = 0., 20.'
  do k = 2,nzFinal
     write(iUnit,'(",",f8.2,$)') zfaceC(k)
  enddo
  write(iUnit,'(a)') ' !'

  write(iUnit,'(a,f10.3,a)')'! XORIGKM = ',x0met,' !'
  write(iUnit,'(a,f10.3,a)')'! YORIGKM = ',y0met,' !'

  if (lucat == "USGS") then
     write(iUnit,'(a)')'! IURB1 = 1 !'
     write(iUnit,'(a)')'! IURB2 = 1 !'
  else if (lucat(1:6) == "NLCD  ") then ! NLCD50
     write(iUnit,'(a)')'! IURB1 = 3 !'
     write(iUnit,'(a)')'! IURB2 = 6 !'
  else if (lucat == "NLCD40") then
     write(iUnit,'(a)')'! IURB1 = 13 !'
     write(iUnit,'(a)')'! IURB2 = 13 !'
  else if (lucat == "MODIS") then
     write(iUnit,'(a)')'! IURB1 = 13 !'
     write(iUnit,'(a)')'! IURB2 = 13 !'
  else if (lucat == "MODIFIED_IGBP_MODIS_NOAH") then
     write(iUnit,'(a)')'! IURB1 = 13 !'
     write(iUnit,'(a)')'! IURB2 = 13 !'
  end if

  close(iUnit)

  return
end subroutine calmet_useful
!
!******************************************************************************
!
subroutine calmet_header(iUnit,Aux,calmet_version,nzFinal)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     WRITE_HEADER writes data to the CALMET.DAT header records.
!     If Aux == .true. instead writes CALMET.AUX (cloud liquid water) header.
!
!     Development History:
!     2009-05-26  Original Development (EPA Region 7, ENVIRON International Corp)
!     2011-12-12  Minor fix: clear up confusion between central and std lat/lon
!     2012-01-31  Made sure aggregation parts (moving/adding levels) gets skipped
!                 if ZFACE(1) is 20m, either by chance or if using interpolation.
!                 Many more small changes, some cosmetic or helpful.
!     2012-03-07  Clarified the 6.x output beginning time-stamp to be more
!                 consistent with CALMET v5.8 output.  
!     2013-02-25  Lengthened file/path strings from 120 to 256 characters, 
!                 based on user feedback.
!     2013-07-15  Added support for multiple output files from a single run.
!     2013-09-20  Split the Useful stuff into its own subroutine, and made
!                 this subroutine write EITHER a CALMET.DAT header (either
!                 v5.8 or v6.x) or a CALMET.AUX header.
!     2017-01-24  Bug fix: cenlat,cenlon should be origlat,origlon.
!
!------------------------------------------------------------------------------
!
  USE met_fields
  USE functions
  implicit none
!
!-----Variable declaration
!
  real,    intent(in) :: calmet_version     ! 5.8, or anything else for 6.x
  integer, intent(in) :: iUnit              ! fortran output unit
  integer, intent(in) :: nzFinal            ! CALMET-specific final num layers
  logical, intent(in) :: Aux                ! True to write CALMET.AUX

  character :: dataset*16, dataver*16, datamod*64, comment*132
  character :: daten*12, utmhem*4, axtz*8, label*8

  integer   :: hrs,min                           ! for calmet.aux header
  integer   :: byr,bmo,bdy,bhr                   ! for calmetv6.x header 
  integer   :: nssta,nusta,npsta,nowsta,idate    ! calmet header codes
  integer   :: iwat1,iwat2,irtype,iwfcod,iutmzn  ! calmet header codes
  parameter(nssta=0,nusta=0,npsta=-1,nowsta=0)   ! calmet header codes

  logical*4 :: lcalgrd
  real      :: feast,fnorth   
!
!-----Data statements
!
  data dataset /'CALMET.DAT'/   ! Emulating CALMET output
  data dataver /'2.0' /         ! 2.0 for v5.8; 2.1 for v6.x and AUX
  data datamod /'No-OBS file structure with embedded control file'/
  data irtype  /   1  /         ! Run Type, must be 1 to run CALPUFF
  data iwfcod  /   1  /         ! Diagnostic wind module
  data lcalgrd /.true./         ! Output contains 3-D W and T fields
  data utmhem  /'N   '/         ! N/A, but needs a default
  data iutmzn  /  -1  /         ! N/A, but needs a default
  data idate   /   0  /         ! No date in the header portion of the file
  data daten   /'            '/ ! No NIMA data 
  data feast   /   0. /         ! False easting
  data fnorth  /   0. /         ! False northing
!
!-----Entry point
!
  iwat1 = water_cat  ! beginning water category, not 50 (as in std calpuff)
  iwat2 = iwat1      ! ending    water category, not 55

!   if (lucat == "USGS") then
!      iwat1 = 16 
!      iwat2 = 16   
!   else if (lucat(1:6) == "NLCD  ") then
!      iwat1 = 1          ! beginning water category, not 50
!      iwat2 = 1          ! ending    water category, not 55
!   else if (lucat(1:6) == "NLCD40") then
!      iwat1 = 17         ! beginning water category, not 50
!      iwat2 = 17         ! ending    water category, not 55
!   else if (lucat == "MODIS") then
!      iwat1 = 17         ! beginning water category, not 50
!      iwat2 = 17         ! ending    water category, not 55
!   endif

  if (Aux) then
     dataset = 'CALMET.AUX'
     dataver = '2.1'
  else
     dataset = 'CALMET.DAT'
     if (calmet_version == 5.8) then
        dataver = '2.0'
     else
        dataver = '2.1'
     endif
  end if

!                      'Produced by CALMET Version: '
  write(comment,'(a)') 'Produced by MMIF VERSION 4.1 2023-10-30'
!
!-----Write file declaration and comment
!
  write(iUnit) dataset,dataver,datamod   ! Record #1: File Declaration
  write(iUnit) 1                         ! Rec #2: Number of comment lines
  write(iUnit) comment                   ! Record #3: comment
!
!-----Write run control parameters (record #4)
!     ibtz as we define it is UTC-ibtz.  CALMETv5.8 assumes always positive.
!     x0met,y0met must be the lower left corner of the current sub-grid.
!     x0met,y0met,deltax are in km, but CALMET wants them in meters.
!
  if (Aux) then ! write CALMET.AUX (cloud liquid water) file
     hrs = int(ibtz)
     min = (ibtz - hrs) * 60
     write(axtz,'(a,S,i3.2,i2.2)') "UTC",hrs,min ! S forces sign +/-
     write(iUnit) byr,bmo,bdy,bhr,ibsec, ieyr,iemo,iedy,iehr,iesec,    &
               axtz,irlg,iEnd-iBeg+1,jEnd-jBeg+1,nzFinal,deltax*1000., &
               x0met*1000.,y0met*1000.,pmap,datum,daten,feast,fnorth,  &
               utmhem,iutmzn,origlat,origlon,tlat1,tlat2, 1, 1 
     ! The last "1, 1" means 1 2D field and 1 3D field
  else if (calmet_version == 5.8) then
     write(iUnit) ibyr,ibmo,ibdy,ibhr,-ibtz,irlg,irtype,                &
               iEnd-iBeg+1,jEnd-jBeg+1,nzFinal,deltax*1000.,             &
               x0met*1000.,y0met*1000.,iwfcod,nssta,nusta,npsta,         &
               nowsta,num_land_cat,iwat1,iwat2,lcalgrd,pmap,datum,daten, &
               feast,fnorth,utmhem,iutmzn,origlat,origlon,tlat1,tlat2 
  else ! CALMET version 6.x format, dataver = 2.1
     hrs = int(ibtz)
     min = (ibtz - hrs) * 60
     write(axtz,'(a,S,i3.2,i2.2)') "UTC",hrs,min ! S forces sign +/-
!
!-----In v5.8 and below, time-stamps are really a label.  Hour=1 starts
!     at the instant 0:00 (midnight) and ends at the instant 1:00.  In v6.x,
!     this changed to beginning and ending instants in time.   So we need
!     to change the beginning time-stamp to reflect the instant that the 
!     first requested output (labeled) hour started.
!     
     byr = ibyr ; bmo = ibmo ; bdy = ibdy ; bhr = ibhr - 1
     call legal_timestamp(byr,bmo,bdy,bhr,23)
     write(iUnit) byr,bmo,bdy,bhr,ibsec, ieyr,iemo,iedy,iehr,iesec,      &
               axtz,irlg,irtype,                                         &
               iEnd-iBeg+1,jEnd-jBeg+1,nzFinal,deltax*1000.,             &
               x0met*1000.,y0met*1000.,iwfcod,nssta,nusta,npsta,         &
               nowsta,num_land_cat,iwat1,iwat2,lcalgrd,pmap,datum,daten, &
               feast,fnorth,utmhem,iutmzn,origlat,origlon,tlat1,tlat2 
  endif
!
!-----Write vertical grid structure (zface) to CALMET.AUX file (0's are idum).
!     Also write the header for the Cloud Water Mixing Ratio, which must be
!     in g/kg.  Not sure what the R_4 data type means (maybe real*4) but 
!     it's required.
!
  if (Aux) then 
     write(iUnit) 'ZFACE   ',0,0,0,0,zfaceC 
     write(iUnit) '2D_VARS ','IGNORE2D','G/KG    ','R_4 '
     write(iUnit) '3D_VARS ','CLDMR   ','G/KG    ','R_4 '
  else
!
!-----Write vertical grid structure (zface) to CALMET.DAT file
!
     label = "ZFACE"
     if (calmet_version == 5.8) then
        write(iUnit) label,idate,zfaceC
     else
        write(iUnit) label,0,0,0,0,zfaceC ! 0's are idum
     endif
!
!-----Write 2-D static arrays for output grid to CALMET.DAT,
!     same format for both v5.8 and v6.x types.
!
! surface roughness length z0

     if (calmet_version == 5.8) then
        write(iUnit) 'Z0      ',idate,  z0(iBeg:iEnd, jBeg:jEnd)
     else
        write(iUnit) 'Z0      ',0,0,0,0,z0(iBeg:iEnd, jBeg:jEnd)
     endif

! land use categories

     if (calmet_version == 5.8) then
        write(iUnit) 'ILANDU  ',idate,  ilu(iBeg:iEnd, jBeg:jEnd)
     else
        write(iUnit) 'ILANDU  ',0,0,0,0,ilu(iBeg:iEnd, jBeg:jEnd)
     endif

! elevations

     if (calmet_version == 5.8) then
        write(iUnit) 'ELEV    ',idate,  topo(iBeg:iEnd, jBeg:jEnd)
     else
        write(iUnit) 'ELEV    ',0,0,0,0,topo(iBeg:iEnd, jBeg:jEnd)
     endif

! leaf area index

     if (calmet_version == 5.8) then
        write(iUnit) 'XLAI    ',idate,  lai(iBeg:iEnd, jBeg:jEnd)
     else
        write(iUnit) 'XLAI    ',0,0,0,0,lai(iBeg:iEnd, jBeg:jEnd)
     endif

  end if

  return
end subroutine calmet_header

!******************************************************************************

subroutine calmet_hour(iUnit,Aux,calmet_version,nzFinal)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     CALMET_HOUR assures that the first CALMET layer is 20m deep, 
!     writes hourly data fields to the CALMET.DAT file, OR if Aux == .true.
!     writes cloud liquid water to the CALMET.AUX file instead.
!
!     Development History:
!     2009-05-26  Original Development (EPA/Region 7, ENVIRON International Corp.)
!     2012-01-31  Bug fix: pOut(,,1) was interpolated between 0 and pOut(,,2).
!                 Changed 2D fields DENSITY and RH to use 2m values, like CALMET
!                 would if using observational data.
!                 Made sure aggregation parts (moving/adding levels) gets skipped
!                 if ZFACE(1) is 20m, either by chance or if using interpolation.
!                 To avoid the dependence on T10 (calculated) we instead set
!                 the new 0-20m T to be interpolated between known T2 and T(1).
!                 CALPUFF isn't very sensitive to T anyway, and I suspect that
!                 aggregation will become unpopular -- I believe interpolation
!                 will prove to be more accurate that aggregation.
!     2012-03-16  Testing of the new sfc_layer() routine shows that its T10 
!                 calculation is better than the old pblmet() routine.  The
!                 workaround for reliance on a calcuated T10 above can now be
!                 reversed.  
!     2012-03-07  Clarified the 6.x output beginning time-stamp to be more
!                 consistent with CALMET v5.8 output.  
!     2013-09-20  Made this subroutine write EITHER a CALMET.DAT hour (either
!                 v5.8 or v6.x)  OR a CALMET.AUX hour.
!     2014-07-30  Suppress output past last requested to accomodate PtZone(iOut).
!     2014-09-18  Move calls to pbl_limits to output routines, instead of main().
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
  real,    allocatable, dimension(:,:) :: r2d
  integer, allocatable, dimension(:,:) :: i2d
  real,    intent(in) :: calmet_version      ! 5.8, or anything else for 6.x
  integer, intent(in) :: iUnit               ! fortran output unit
  integer, intent(in) :: nzFinal             ! CALMET-specific final num layers
  logical, intent(in) :: Aux                 ! True to write CALMET.AUX

  character           :: label*8
  integer             :: i,j,k, iw2,int2,ipcode
  integer             :: bdathr, iy,im,id,ih ! time-stamps for v6.x
  real, parameter     :: Rdry = 287.         ! Dry air gas constant (J/deg/kg)
  real                :: ztop,zbot
  real                :: ZiMech              ! mechanical mixed layer ht (m)
!
!-----Entry point
!
  if (nDatHr > iEndDat)  return ! after last output for this timezone (ibtz)

  if (zface(1) /= 20.) then ! don't fix it if it ain't broken!
!
!     Map variables to a vertical grid structure consisting of a 20 m deep
!     first layer in CALPUFF.  Note that although wOut is allocated (essentially)
!     as wOut(1:nx,1:ny,0:nz), CALMET does not write out or access wOut(,,0),
!     the vertical velocity at the ground surface.  Also note that pOut and qOut
!     are not written, so there's no need to deal with them when moving layers.
!
!-----In this case, we remove a layer and move data layers down,
!     and nzFinal = nzOut - 1
!
     if (zface(1) < 19. .and. zface(2) < 30.) then
        int2 = 2
        iw2  = 3
        do k = 3,nzFinal
           do j = jBeg,jEnd
              do i = iBeg,iEnd
                 uOut(i,j,k) = uOut(i,j,k+1)
                 vOut(i,j,k) = vOut(i,j,k+1)
                 wOut(i,j,k) = wOut(i,j,k+1)
                 tOut(i,j,k) = tOut(i,j,k+1)
                 qcOut(i,j,k)= qcOut(i,j,k+1)
              enddo
           enddo
        enddo
!
!-----In these two cases, number of layers remains the same,
!     and nzFinal = nzOut
!
     elseif (zface(1) < 19. .and. zface(2) >=30.) then
        int2 = 2
        iw2  = 2
     elseif (zface(1) >= 19. .and. zface(2) < 30.) then
        int2 = 1
        iw2  = 2
!
!-----In this case, we add a layer and move data layers up,
!     and nzFinal = nzOut + 1
!
     else ! zface(1) >= 19. .and. zface(2) >= 30.
          ! Action: adding a level at 20 m."
        int2 = 1
        iw2  = 1
        do k = nzFinal,3,-1
           do j = jBeg,jEnd
              do i = iBeg,iEnd
                 uOut(i,j,k) = uOut(i,j,k-1)
                 vOut(i,j,k) = vOut(i,j,k-1)
                 wOut(i,j,k) = wOut(i,j,k-1)
                 tOut(i,j,k) = tOut(i,j,k-1)
                 qcOut(i,j,k)= qcOut(i,j,k-1)
              enddo
           enddo
        enddo
     endif
!
!-----Determine new layer 2 data by interpolation; assign vertical velocity
!
     do j = jBeg,jEnd
        do i = iBeg,iEnd
           zbot = 0.5*(zface(int2+1) - zface(int2-1))
           ztop = 0.5*(zfaceC(2) + zfaceC(1)) - 0.5*(zface(int2) + zface(int2-1))

           uOut(i,j,2) = uOut(i,j,int2) + (ztop/zbot) * &
                (uOut(i,j,int2+1)-uOut(i,j,int2))

           vOut(i,j,2) = vOut(i,j,int2) + (ztop/zbot) * &
                (vOut(i,j,int2+1)-vOut(i,j,int2))

           tOut(i,j,2) = tOut(i,j,int2) + (ztop/zbot) * &
                (tOut(i,j,int2+1)-tOut(i,j,int2))

           qcOut(i,j,2) = qcOut(i,j,int2) + (ztop/zbot) * &
                (qcOut(i,j,int2+1)-qcOut(i,j,int2))

           wOut(i,j,2) = wOut(i,j,iw2)
        enddo
     enddo
!
!-----Layer 1 data are now 10-m data; interpolate vertical velocity.
!
     do j = jBeg,jEnd
        do i = iBeg,iEnd
           uOut(i,j,1) = u10(i,j)
           vOut(i,j,1) = v10(i,j)
           tOut(i,j,1) = t10(i,j) ! could use T2, or interp between T2 and T(1)
!            ztop = 10. - 2.      ! interpolate betweeen T2 and T(1)
!            zbot = zface(1) - 2.
!            tOut(i,j,1) = t2(i,j) + (ztop/zbot)*(tOut(i,j,2) - t2(i,j))
           if (int2 .eq. 1) then
              wOut(i,j,1) = (zfaceC(1)/zface(1))*wOut(i,j,1) ! between 0 and W(1)
                 ! Qcloud at ground is not zero, so we don't have anything to 
              qcOut(i,j,1)= qcOut(i,j,1) ! interpolate between.  Just keep it.
           else
              ztop = zfaceC(1) - zface(1)
              zbot = zfaceC(2) - zface(1)
              wOut(i,j,1) = wOut(i,j,1) +(ztop/zbot)*(wOut(i,j,2) -wOut(i,j,1))
              qcOut(i,j,1)= qcOut(i,j,1)+(ztop/zbot)*(qcOut(i,j,2)-qcOut(i,j,1))
           endif
        enddo
     enddo
  end if ! if (zface(1) /= 20.) then 
!
!-----For CALPUFF v6.x format, specify the beginning and anding instants
!     for this output hour.  Internally, we "label" each hour using 
!     hour-ending format.  So hour 2 goes from 1:00 to 2:00.  For v6.x 
!     output, we set ibsec = iesec = 0.  This might change later, if anyone
!     has sub-hourly WRF output and wants to run CALPUFF with it.
!
  call nDatHr2ymdh(ndathr,iy,im,id,ih,23)
  call add_hour(iy,im,id,ih,-1) ! hour "2" begins at 1:00
  call ymdh2nDatHr(iy,im,id,ih,bdathr)
!
!-----Write the 2-D filler and 3-D cloud liquid water field to CALMET.AUX file
!
  if (Aux) then
     allocate( r2d(iBeg:iEnd,jBeg:jEnd) )

     do j = jBeg,jEnd
        do i = iBeg,iEnd
           r2d(i,j) = 0.   ! Filler, because CALPUFF v6.4 needs at least
        end do             ! one 2-D fields
     end do
     write(label,'(a)') 'IGNORE2D'
     write(iUnit) label,bdathr,0,ndathr,0,r2d  ! Filler 2D field

     do k = 1,nzFinal
        do j = jBeg,jEnd
           do i = iBeg,iEnd
              r2d(i,j) = qcOut(i,j,k)*1000.    ! Cloud liquid water mixing ratio
              if (r2d(i,j) < 1.e-8) r2d(i,j) = 0. ! in g/kg, not too small
           end do
        end do
        write(label,'(a,i3.3)') 'CLDMR',k
        write(iUnit) label,bdathr,0,ndathr,0,r2d  ! Cloud Liquid Water
     end do
!
!-----Done, delallocate and leave
!
     deallocate(r2d)
     return
  end if
!
!-----Allocate temporary containers for CALMET.DAT files
!
  allocate( r2d(iBeg:iEnd,jBeg:jEnd) )
  allocate( i2d(iBeg:iEnd,jBeg:jEnd) )
!
!-----Write the 3-D fields to CALMET.DAT file
!
  do k = 1,nzFinal
     do j = jBeg,jEnd
        do i = iBeg,iEnd
           r2d(i,j) = uOut(i,j,k)
        end do
     end do
     write(label,'(a,i3)') "U-LEV",k
     if (calmet_version == 5.8) then
        write(iUnit) label,ndathr,r2d             ! U
     else
        write(iUnit) label,bdathr,0,ndathr,0,r2d  ! U
     endif

     do j = jBeg,jEnd
        do i = iBeg,iEnd
           r2d(i,j) = vOut(i,j,k)
        end do
     end do
     write(label,'(a,i3)') "V-LEV",k
     if (calmet_version == 5.8) then
        write(iUnit) label,ndathr,r2d             ! V
     else
        write(iUnit) label,bdathr,0,ndathr,0,r2d  ! V
     endif

     do j = jBeg,jEnd
        do i = iBeg,iEnd
           r2d(i,j) = wOut(i,j,k) ! note that wOut(,,0) is not used by CALMET
        end do
     end do
     write(label,'(a,i3)') "WFACE",k
     if (calmet_version == 5.8) then
        write(iUnit) label,ndathr,r2d             ! W
     else
        write(iUnit) label,bdathr,0,ndathr,0,r2d  ! W
     endif
  end do

  do k = 1,nzFinal
     do j = jBeg,jEnd
        do i = iBeg,iEnd
           r2d(i,j) = tOut(i,j,k)
        end do
     end do
     write(label,'(a,i3)') "T-LEV",k
     if (calmet_version == 5.8) then
        write(iUnit) label,ndathr,r2d             ! T
     else
        write(iUnit) label,bdathr,0,ndathr,0,r2d  ! T
     endif
  end do
!
!-----Write the 2-D fields
!
  do j = jBeg,jEnd
     do i = iBeg,iEnd
        i2d(i,j) = ipgt(i,j)
     end do
  end do
  if (calmet_version == 5.8) then
     write(iUnit) 'IPGT    ',ndathr,i2d            ! PGT
  else
     write(iUnit) 'IPGT    ',bdathr,0,ndathr,0,i2d ! PGT
  endif

  do j = jBeg,jEnd
     do i = iBeg,iEnd
        r2d(i,j) = ustar(i,j)
     end do
  end do
  if (calmet_version == 5.8) then
     write(iUnit) 'USTAR   ',ndathr,r2d            ! Ustar
  else
     write(iUnit) 'USTAR   ',bdathr,0,ndathr,0,r2d ! Ustar
  endif

  do j = jBeg,jEnd
     do i = iBeg,iEnd

        r2d(i,j) = pbl(i,j)
        ! call venkatram_mech_mixh(ustar(i,j),ZiMech)
        !if (CalcPBL == "MMIF") then
        !   if (mol(i,j) < 0.) then                     ! convective hours
        !      r2d(i,j) = max( ZiMech, pbl(i,j) )
        !   else                                        ! stable hours
        !      r2d(i,j) = ZiMech
        !   endif
        !endif
!
!-----CALPUFF and SCICHEM bomb if the PBL depth is outside of the modeling
!     domain, so limit to be between the lowest and highest layer mid-points 
!     (same as MCIP does). wstar is written below, so let it change to be 
!     consistent with each mixing height. 
!
        call pbl_limits(zmid(1),zmid(nzOut), mol(i,j),ustar(i,j),wstar(i,j), &
             r2d(i,j))
     end do
  end do
  if (calmet_version == 5.8) then
     write(iUnit) 'ZI      ',ndathr,r2d            ! PBL
  else
     write(iUnit) 'ZI      ',bdathr,0,ndathr,0,r2d ! PBL
  endif

  do j = jBeg,jEnd
     do i = iBeg,iEnd
        r2d(i,j) = mol(i,j)
     end do
  end do
  if (calmet_version == 5.8) then
     write(iUnit) 'EL      ',ndathr,r2d            ! MOL
  else
     write(iUnit) 'EL      ',bdathr,0,ndathr,0,r2d ! MOL
  endif

  do j = jBeg,jEnd
     do i = iBeg,iEnd
        r2d(i,j) = wstar(i,j)
     end do
  end do
  if (calmet_version == 5.8) then
     write(iUnit) 'WSTAR   ',ndathr,r2d            ! Wstar
  else
     write(iUnit) 'WSTAR   ',bdathr,0,ndathr,0,r2d ! Wstar
  endif

  do j = jBeg,jEnd
     do i = iBeg,iEnd
        r2d(i,j) = rain(i,j)
     end do
  end do
  if (calmet_version == 5.8) then
     write(iUnit) 'RMM     ',ndathr,r2d            ! Rain
  else
     write(iUnit) 'RMM     ',bdathr,0,ndathr,0,r2d ! Rain
  endif

  do j = jBeg,jEnd
     do i = iBeg,iEnd
        r2d(i,j) = tsfc(i,j)
     end do
  end do
  if (calmet_version == 5.8) then
     write(iUnit) 'TEMPK   ',ndathr,r2d            ! Tsrf
  else
     write(iUnit) 'TEMPK   ',bdathr,0,ndathr,0,r2d ! Tsrf
  endif

  do j = jBeg,jEnd ! use 2m MM5/WRF level to set density
     do i = iBeg,iEnd
        r2d(i,j) = density_fn(t2(i,j),psfc(i,j),q2(i,j)) ! RHO
     end do
  end do
  if (calmet_version == 5.8) then
     write(iUnit) 'RHO     ',ndathr,r2d            ! Density
  else
     write(iUnit) 'RHO     ',bdathr,0,ndathr,0,r2d ! Density
  endif

  do j = jBeg,jEnd
     do i = iBeg,iEnd
        r2d(i,j) = qsw(i,j)
     end do
  end do
  if (calmet_version == 5.8) then
     write(iUnit) 'QSW     ',ndathr,r2d            ! Shortwave
  else
     write(iUnit) 'QSW     ',bdathr,0,ndathr,0,r2d ! Shortwave
  endif

  do j = jBeg,jEnd ! use 2m MM5/WRF level to set Relative Humidity
     do i = iBeg,iEnd
        i2d(i,j) = min(100, nint(q2(i,j) / qs_fn(t2(i,j),psfc(i,j)) * 100.)) ! rh
     end do
  end do
  if (calmet_version == 5.8) then
     write(iUnit) 'IRH     ',ndathr,i2d            ! RH
  else
     write(iUnit) 'IRH     ',bdathr,0,ndathr,0,i2d ! RH
  endif

  do j = jBeg,jEnd
     do i = iBeg,iEnd
        if (rain(i,j) > 0.) then
           if (tOut(i,j,1) < 273.15) then
              ipcode = 20
           else
              ipcode = 10
           endif
        else
           ipcode = 0
        endif
        i2d(i,j) = ipcode
     end do
  end do
  if (calmet_version == 5.8) then
     write(iUnit) 'IPCODE  ',ndathr,i2d            ! Precip code
  else
     write(iUnit) 'IPCODE  ',bdathr,0,ndathr,0,i2d ! Precip code
  endif
!
!-----Done, delallocate  
!
  deallocate(r2d)
  deallocate(i2d)

  return
END subroutine calmet_hour
!
!******************************************************************************
!
subroutine calmet_terrain(iUnit,fname)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     Writes a DSAA (Golden Software Surfer 6.0 ASCII *.GRD) file containing
!     the MM5/WRF terrain.
!
!     Development History:
!     2011-09-30  New with MMIF v2.0
!     2017-06-20  Write a terr.lvl file with the CALMET terrain.
!
!------------------------------------------------------------------------------
!
  USE met_fields
  implicit none
!
!-----Variable declaration
!
  character (len=*), intent(in) :: fname ! filename of output *.GRD file
  integer, intent(in) :: iUnit
  integer :: ni,nj, i,j
  real    :: xMin,xMax, yMin,yMax, zMin,zMax

!
!-----Format statements
!
1 format(a)                      ! 1st line: DSAA
2 format(2i12)                   ! 2nd line: NX,NJ
3 format(2f12.4)                 ! 3rd/4th line: Xmin,Xmax/Ymin,Ymax
5 format(1p,2e12.4)              ! 5th line: Zmin,Zmax
9 format(1p,e11.4,999(1x,e11.4)) ! Remaining lines, data, NX values per line
!
!-----Entry point
!
  ni = iEnd-iBeg+1
  nj = jEnd-jBeg+1

  xMin = x0met+deltax/2.      ! format requires grid cell centers, not the 
  yMin = y0met+deltax/2.      ! grid edges (corners) like calmet requires

  xMax = xMin + (ni-1)*deltax
  yMax = yMin + (nj-1)*deltax
!
!-----Fix the situation where, presumably due to numerical round-off, 
!     xMax = 312.001 or 311.999.  Those are really just 312.000.
!
  if (abs(xMax - nint(xMax)) <= 0.003) xMax = float(nint(xMax))
  if (abs(yMax - nint(yMax)) <= 0.003) yMax = float(nint(yMax))

  zMin = minval(topo(iBeg:iEnd,jBeg:jEnd))
  zMax = maxval(topo(iBeg:iEnd,jBeg:jEnd))

  open(iUnit,file=fname,status='unknown')

  write(iUnit,1) "DSAA"
  write(iUnit,2) ni, nj
  write(iUnit,3) xMin, xMax
  write(iUnit,3) yMin, yMax
  write(iUnit,5) zMin, zMax
  do j = jBeg,jEnd
     write(iUnit,9) (topo(i,j), i=iBeg,iEnd)
  end do
  
  close(iUnit) ! done writing terrain file
!
!-----Also write a helpful terr.lvl file that CALPUFF users are used to.
!
  i = index(fname,".",.true.) - 1       ! find .grd
  if (i < 0) i = len_trim(fname)        ! use whole basename if no dot
  open(iUnit,file=fname(1:i) // ".lvl") ! add .lvl to basename

  write(iUnit,1) "LVL2"
  write(iUnit,1) "'Level Flags LColor LStyle LWidth FFGColor FBGColor FPattern FMode"
  write(iUnit,1) '0 0 "Black" "Invisible" 0 "White" "R18 G18 B18" "Solid" 2 1 1'
  write(iUnit,1) '1 0 "Black" "Invisible" 0 "Ocean Green" "R18 G18 B18" "Solid" 2 1 1'
  write(iUnit,1) '50 0 "Black" "Invisible" 0 "R114 G161 B153" "R23 G23 B23" "Solid" 2 1 1'
  write(iUnit,1) '150 0 "Black" "Invisible" 0 "R132 G173 B153" "R32 G32 B32" "Solid" 2 1 1'
  write(iUnit,1) '300 0 "Black" "Invisible" 0 "R163 G193 B153" "R45 G45 B45" "Solid" 2 1 1'
  write(iUnit,1) '450 0 "Black" "Invisible" 0 "R193 G214 B153" "R59 G59 B59" "Solid" 2 1 1'
  write(iUnit,1) '600 0 "Black" "Invisible" 0 "R224 G234 B153" "R73 G73 B73" "Solid" 2 1 1'
  write(iUnit,1) '750 0 "Black" "Invisible" 0 "Chalk" "R87 G87 B87" "Solid" 2 1 1'
  write(iUnit,1) '900 0 "Black" "Invisible" 0 "R242 G235 B140" "R101 G101 B101" "Solid" 2 1 1'
  write(iUnit,1) '1050 0 "Black" "Invisible" 0 "R228 G214 B126" "R115 G115 B115" "Solid" 2 1 1'
  write(iUnit,1) '1200 0 "Black" "Invisible" 0 "R214 G194 B112" "R129 G129 B129" "Solid" 2 1 1'
  write(iUnit,1) '1300 0 "Black" "Invisible" 0 "R200 G173 B98" "R143 G143 B143" "Solid" 2 1 1'
  write(iUnit,1) '1500 0 "Black" "Invisible" 0 "R187 G152 B85" "R157 G157 B157" "Solid" 2 1 1'
  write(iUnit,1) '1650 0 "Black" "Invisible" 0 "R173 G132 B71" "R170 G170 B170" "Solid" 2 1 1'
  write(iUnit,1) '1800 0 "Black" "Invisible" 0 "Brown" "R190 G190 B190" "Solid" 2 1 1'
  write(iUnit,1) '1950 0 "Black" "Invisible" 0 "R169 G126 B84" "R198 G198 B198" "Solid" 2 1 1'
  write(iUnit,1) '2100 0 "Black" "Invisible" 0 "R198 G169 B141" "R212 G212 B212" "Solid" 2 1 1'
  write(iUnit,1) '2250 0 "Black" "Invisible" 0 "R226 G212 B198" "R226 G226 B226" "Solid" 2 1 1'
  write(iUnit,1) '2400 0 "Black" "Invisible" 0 "White" "R240 G240 B240" "Solid" 2 1 1'

  close(iUnit)

  return
end subroutine calmet_terrain
!
!******************************************************************************
!
subroutine qa_plots(iUnit,fname,type)
!
!------------------------------------------------------------------------------
!     MESOSCALE MODEL INTERFACE PROGRAM (MMIF)
!     VERSION 4.1 2023-10-30
!
!     Writes a BLN, BNA, or KML file of the full WRF/MM5 domain and requested
!     output domain. BLN and BNA files mimic CALPUFF Q/A output, so they are
!     in the same projection as the input WRF/MM5 output. KMLs are always 
!     lon-lat. DAT file of the points is 
!
!     Development History:
!     2017-06-20  New with MMIF v3.4
!     2018-12-21  Lon values in KML file did not have enough sig figs for lat<0.
!     2019-03-11  Bug fix: remove extra spaces in lat-lon strings in KMLs
!     2020-03-02  Bug fix: lat's for points in KML output formatted incorrectly.
!
!------------------------------------------------------------------------------
!
  USE met_fields
  USE module_llxy
  implicit none
!
!-----Variable declaration
!
  integer,           intent(in) :: iUnit

  character (len=*), intent(in) :: fname  ! filename of output *.GRD file
  character (len=*), intent(in) :: type   ! BLN, BNA, DAT, or KML
  character (len=12)            :: LatStr ! for KML to remove extra spaces

  integer   i,j
  real      x,y, lat,lon

  TYPE(proj_info)               :: proj  ! for use with module_llxy from WRF
!
!-----Format statements
!
1 format(a)                      ! string
2 format(f12.6,",",a,",",i1)         ! KML     lines: Lat,Lon,Elev
! format(f12.6,",",f9.6,",",i1)  ! old KML     lines: Lat,Lon,Elev
3 format(2f12.4)                 ! BLN/BNA lines: Xmin,Xmax/Ymin,Ymax
4 format(2f12.4,2f12.6,a)        ! X,Y,Lat,Lon,String
!
!-----Entry point
!
  call map_set(iProj,proj,lat1=origlat,lon1=origlon,knowni=0.,knownj=0.,  & 
       dx=deltax*1000.,stdlon=stdlon,truelat1=tlat1,truelat2=tlat2)

  open(iUnit,file=fname,status='unknown')

  if (type == "BLN") then

     write(iUnit,1) '5 0 "Requested sub-grid"'
     write(iUnit,3) x0met, y0met
     write(iUnit,3) xfmet, y0met
     write(iUnit,3) xfmet, yfmet
     write(iUnit,3) x0met, yfmet
     write(iUnit,3) x0met, y0met
     write(iUnit,1) '5 0 "Full WRF grid"'
     write(iUnit,3) xWwrf, ySwrf
     write(iUnit,3) xEwrf, ySwrf
     write(iUnit,3) xEwrf, yNwrf
     write(iUnit,3) xWwrf, yNwrf
     write(iUnit,3) xWwrf, ySwrf

  else if (type == "BNA") then

     write(iUnit,1) '" Met","grid",5'
     write(iUnit,3) x0met, y0met
     write(iUnit,3) xfmet, y0met
     write(iUnit,3) xfmet, yfmet
     write(iUnit,3) x0met, yfmet
     write(iUnit,3) x0met, y0met
     write(iUnit,1) '" WRF","grid",5'
     write(iUnit,3) xWwrf, ySwrf
     write(iUnit,3) xEwrf, ySwrf
     write(iUnit,3) xEwrf, yNwrf
     write(iUnit,3) xWwrf, yNwrf
     write(iUnit,3) xWwrf, ySwrf

  else if (type == "DAT") then
     write(iUnit,1) "       X(km)       Y(km)    lat(deg)    lon(deg) Name"
     do i = 1, NumOuts
        if (ijPt(i) /= 0) then ! ijPt(i) == 0 means non-point (grid) output
           if (OutType(i) == "SFC" .or. OutType(i) == "DATA"    .or. &
               OutType(i) == "PFL" .or. OutType(i) == "ONSITE"  .or. &
               OutType(i) == "FSL") then
              
              call latlon_to_ij(proj,ylat(iPt(i),jPt(i)),xlon(iPt(i),jPt(i)),x,y)
              x = nint(x * 2.)/2. * deltax ! round to nearest whole or half 
              y = nint(y * 2.)/2. * deltax ! integer, and convert to km
              
              write(iUnit,4) x, y, xlon(iPt(i),jPt(i)),ylat(iPt(i),jPt(i)), &
                   ' "'// trim(OutForm(i)) // " " // trim(OutType(i)) // '"'
           end if
        end if
     end do
        
  else if (type == "KML") then
     
     write(iUnit,1) '<?xml version="1.0" encoding="iso-8859-1"?>'
     write(iUnit,1) '<kml xmlns="http://earth.google.com/kml/2.0">'
     write(iUnit,1) '<Document>'
     write(iUnit,1) '  <Style id="lineBlue">'
     write(iUnit,1) '    <LineStyle>'
     write(iUnit,1) '      <color>FFFF0000</color>'
     write(iUnit,1) '      <width>2</width>'
     write(iUnit,1) '    </LineStyle>'
     write(iUnit,1) '  </Style>'
     write(iUnit,1) '  <Style id="lineRed">'
     write(iUnit,1) '    <LineStyle>'
     write(iUnit,1) '      <color>FF0000FF</color>'
     write(iUnit,1) '      <width>2</width>'
     write(iUnit,1) '    </LineStyle>'
     write(iUnit,1) '  </Style>'
     write(iUnit,1) '  <Style id="pointRed">'
     write(iUnit,1) '    <IconStyle>'
     write(iUnit,1) '      <color>ff0000ff</color>'
     write(iUnit,1) '      <scale>1.2</scale>'
     write(iUnit,1) '      <Icon>'
     write(iUnit,1) '         <href>http://maps.google.com/mapfiles/kml/shapes/placemark_circle.png</href>'
     write(iUnit,1) '      </Icon>'
     write(iUnit,1) '    </IconStyle>'
     write(iUnit,1) '  </Style>'
!     write(iUnit,1) '  <Folder>'
!     write(iUnit,1) '    <name>WRF/MMIF Domains</name>'
     write(iUnit,1) '    <Placemark>'
     write(iUnit,1) '      <description>'
     write(iUnit,1) '        <![CDATA[CALPUFF/SCICHEM domain]]>'
     write(iUnit,1) '      </description>'
     write(iUnit,1) '      <name>MMIF 3D Sub-domain</name>'
     write(iUnit,1) '      <styleUrl>#lineBlue</styleUrl>'
     write(iUnit,1) '      <LineString>'
     write(iUnit,1) '        <tessellate>1</tessellate>'
     write(iUnit,1) '        <altitudeMode>clampToGround</altitudeMode>'
     write(iUnit,1) '        <coordinates>'
!
!-----Write the output sub-domain
!
     call ij_to_latlon(proj,x0met/deltax,y0met/deltax,lat,lon) ! SW corner
        write(LatStr,'(f9.6)') lat
        write(iUnit,2) lon,trim(adjustl(LatStr)),0

     ! Southern edge

     j = jBeg
     do i = iBeg,iEnd
        call latlon_to_ij(proj,ylat(i,j),xlon(i,j),x,y)
        ! round to nearest whole or half integer, convert to KM, add half cell
        x = nint(x * 2.)/2. * deltax + deltax/2.
        y = nint(y * 2.)/2. * deltax - deltax/2.
        call ij_to_latlon(proj,x/deltax,y/deltax,lat,lon)
        write(LatStr,'(f9.6)') lat
        write(iUnit,2) lon,trim(adjustl(LatStr)),0
     end do

     ! Eastern edge

     i = iEnd
     do j = jBeg,jEnd
        call latlon_to_ij(proj,ylat(i,j),xlon(i,j),x,y)
        ! round to nearest whole or half integer, convert to KM, add half cell
        x = nint(x * 2.)/2. * deltax + deltax/2.
        y = nint(y * 2.)/2. * deltax + deltax/2.
        call ij_to_latlon(proj,x/deltax,y/deltax,lat,lon)
        write(LatStr,'(f9.6)') lat
        write(iUnit,2) lon,trim(adjustl(LatStr)),0
     end do

     ! Northern edge
        
     j = jEnd
     do i = iEnd,iBeg,-1
        call latlon_to_ij(proj,ylat(i,j),xlon(i,j),x,y)
        ! round to nearest whole or half integer, convert to KM, add half cell
        x = nint(x * 2.)/2. * deltax - deltax/2.
        y = nint(y * 2.)/2. * deltax + deltax/2.
        call ij_to_latlon(proj,x/deltax,y/deltax,lat,lon)
        write(LatStr,'(f9.6)') lat
        write(iUnit,2) lon,trim(adjustl(LatStr)),0
     end do

     ! Western edge

     i = iBeg
     do j = jEnd,jBeg,-1
        call latlon_to_ij(proj,ylat(i,j),xlon(i,j),x,y)
        ! round to nearest whole or half integer, convert to KM, add half cell
        x = nint(x * 2.)/2. * deltax - deltax/2.
        y = nint(y * 2.)/2. * deltax - deltax/2.
        call ij_to_latlon(proj,x/deltax,y/deltax,lat,lon)
        write(LatStr,'(f9.6)') lat
        write(iUnit,2) lon,trim(adjustl(LatStr)),0
     end do
        
     write(iUnit,1) '        </coordinates>'
     write(iUnit,1) '      </LineString>'
     write(iUnit,1) '    </Placemark>'
     write(iUnit,1) '    <Placemark>'
     write(iUnit,1) '      <description>'
     write(iUnit,1) '        <![CDATA[Full WRF domain]]>'
     write(iUnit,1) '      </description>'
     write(iUnit,1) '      <name>Full WRF domain</name>'
     write(iUnit,1) '      <styleUrl>#lineRed</styleUrl>'
     write(iUnit,1) '      <LineString>'
     write(iUnit,1) '        <tessellate>1</tessellate>'
     write(iUnit,1) '        <altitudeMode>clampToGround</altitudeMode>'
     write(iUnit,1) '        <coordinates>'
!
!-----Write the full WRF/MM5 domain
!
     call ij_to_latlon(proj,xWwrf/deltax,ySwrf/deltax,lat,lon) ! SW corner
     write(LatStr,'(f9.6)') lat
     write(iUnit,2) lon,trim(adjustl(LatStr)),0

     ! Southern edge

     j = 1
     do i = 1,nx
        call latlon_to_ij(proj,ylat(i,j),xlon(i,j),x,y)
        ! round to nearest whole or half integer, convert to KM, add half cell
        x = nint(x * 2.)/2. * deltax + deltax/2.
        y = nint(y * 2.)/2. * deltax - deltax/2.
        call ij_to_latlon(proj,x/deltax,y/deltax,lat,lon)
        write(LatStr,'(f9.6)') lat
        write(iUnit,2) lon,trim(adjustl(LatStr)),0
     end do

     ! Eastern edge

     i = nx
     do j = 1,ny
        call latlon_to_ij(proj,ylat(i,j),xlon(i,j),x,y)
        ! round to nearest whole or half integer, convert to KM, add half cell
        x = nint(x * 2.)/2. * deltax + deltax/2.
        y = nint(y * 2.)/2. * deltax + deltax/2.
        call ij_to_latlon(proj,x/deltax,y/deltax,lat,lon)
        write(LatStr,'(f9.6)') lat
        write(iUnit,2) lon,trim(adjustl(LatStr)),0
     end do

     ! Northern edge
        
     j = ny
     do i = nx,1,-1
        call latlon_to_ij(proj,ylat(i,j),xlon(i,j),x,y)
        ! round to nearest whole or half integer, convert to KM, add half cell
        x = nint(x * 2.)/2. * deltax - deltax/2.
        y = nint(y * 2.)/2. * deltax + deltax/2.
        call ij_to_latlon(proj,x/deltax,y/deltax,lat,lon)
        write(LatStr,'(f9.6)') lat
        write(iUnit,2) lon,trim(adjustl(LatStr)),0
     end do

     ! Western edge

     i = 1
     do j = ny,1,-1
        call latlon_to_ij(proj,ylat(i,j),xlon(i,j),x,y)
        ! round to nearest whole or half integer, convert to KM, add half cell
        x = nint(x * 2.)/2. * deltax - deltax/2.
        y = nint(y * 2.)/2. * deltax - deltax/2.
        call ij_to_latlon(proj,x/deltax,y/deltax,lat,lon)
        write(LatStr,'(f9.6)') lat
        write(iUnit,2) lon,trim(adjustl(LatStr)),0
     end do
        
     write(iUnit,1) '        </coordinates>'
     write(iUnit,1) '      </LineString>'
     write(iUnit,1) '    </Placemark>'
!
!-----Write out the POINT output too
!
     do i = 1, NumOuts
        if (ijPt(i) /= 0) then ! ijPt(i) == 0 means non-point (grid) output
           if (OutType(i) == "SFC" .or. OutType(i) == "DATA"    .or. &
               OutType(i) == "PFL" .or. OutType(i) == "ONSITE"  .or. &
               OutType(i) == "FSL") then
              
              write(iUnit,1) '    <Placemark>'
              write(iUnit,1) '      <description>'
              write(iUnit,1) '        <![CDATA[' // trim(OutForm(i)) // " " // &
                   trim(OutType(i)) // ']]>'
              write(iUnit,1) '      </description>'
              write(iUnit,1) '      <name>' // trim(OutFile(i)) // '</name>'
              write(iUnit,1) '      <styleUrl>#pointRed</styleUrl>'
              write(iUnit,1) '      <Point>'
              write(iUnit,1) '        <coordinates>'
              write(LatStr,'(f9.6)') ylat(iPt(i),jPt(i))
              ! print*,"i      = ",i
              ! print*,"iPt(i) = ",iPt(i)
              ! print*,"jPt(i) = ",jPt(i)
              ! print*,"ylat   = ",ylat(iPt(i),jPt(i))
              ! print*,"LatStr = ",trim(LatStr)
              write(iUnit,2) xlon(iPt(i),jPt(i)),trim(adjustl(LatStr)),0
              write(iUnit,1) '        </coordinates>'
              write(iUnit,1) '      </Point>'
              write(iUnit,1) '    </Placemark>'

           end if
        end if
     end do
!     write(iUnit,1) '  </Folder>'
     write(iUnit,1) '</Document>'
     write(iUnit,1) '</kml>'
     
  endif

  close(iUnit)

  return
end subroutine qa_plots

