PROGRAM calmet2ioapi

	!***********************************************************************
	!  DESCRIPTION:
	!  Create an IOAPI-netCDF file from a calmet.dat output file created
	! with calmetv5.8 or greater.
	!
	!  PRECONDITIONS REQUIRED:
	!       "setenv"s for output file
	!
	!  SUBROUTINES AND FUNCTIONS CALLED:
	!       I/O API and utility routines; Lambert conversion routines
	!       from libemstuff
	!
	!  REVISION  HISTORY:
	!       smo:  11/13/2000 - modify to create a program that will
	!             create a calmet.dat file for calgrid and calpuff from
	!             mcip output
	!       smo:  11/20/2000 - modify to create a netCDF file from a
	!             calmet.dat
	!       smo:  09/09/2003 - modify for calmet v5_53
	!       baa:  02/15/2007 - modify for calmet v6.211
	!       nnd:  08/12/2010 - modify to fortran 90 and proper coding
	!             standards
	!       baa:  02/28/2012 - added UTM coordinates and 
	!             correction for 0 npsta coding error
	!
	!***********************************************************************

	IMPLICIT NONE

	! INCLUDES:
	INCLUDE 'PARMS3.EXT'      ! I/O API constants
	INCLUDE 'FDESC3.EXT'      ! I/O API file description data structure
	INCLUDE 'IODECL3.EXT'     ! I/O API function declarations

	! I/O API functions
	INTEGER :: promptffile
	INTEGER :: julian
	CHARACTER(LEN=16) :: promptmfile

	!=========================================
	! LOCAL VARIABLES and their descriptions:
	!=========================================

	! CALMET Header variables
	CHARACTER(LEN=16) :: dataset ! Dataset name (CALMET.DAT)
	CHARACTER(LEN=16) :: dataver ! Dataset version
	CHARACTER(LEN=64) :: datamod ! Dataset message field
	INTEGER :: ncom ! Number of comment records
	CHARACTER(LEN=132), DIMENSION(:), ALLOCATABLE :: comment !Comment lines
	INTEGER :: ibyr, ibmo, ibdy ! Date (year, month, day)
	INTEGER :: ibhr, ibtz ! End of hour & timezone offset
	INTEGER :: irlg ! Run length (hours)
	INTEGER :: irtype ! run type
	INTEGER :: nx, ny, nz ! Number of grid cells in x, y & vertcalunit
	REAL :: dgrid ! grid spacing
	REAL :: xorigr, yorigr ! coordinate (m) of SW corner (1,1)
	INTEGER :: iwfcod ! Wind field module used
	INTEGER :: nssta ! Number of surface met stations
	INTEGER :: nusta ! Number of upper air stations
	INTEGER :: npsta ! Number of precipitation stations
	INTEGER :: nowsta ! Number of over water stations
	INTEGER :: nlu ! Number of land use categories
	INTEGER :: iwat1 ! Range of land use categories
	INTEGER :: iwat2 ! Corresponding to water surfaces
	LOGICAL :: lcalgrd ! Flag for all variables
	CHARACTER(LEN=8) :: pmap ! Map projection
	CHARACTER(LEN=8) :: datum ! DATUM code for grid coordinates
	CHARACTER(LEN=12) :: daten ! NIMA date (MM-DD-YYY) for datum definitions
	REAL :: feast, fnorth ! False easting or northing for TTM, LLC or LAZA
	CHARACTER(LEN=4) :: utmhem ! Hemisphere for UTM projection
	INTEGER :: iutmzn ! UTM zone
	REAL :: rnlat0, relon0 ! north lat or east lon for projection origin
	REAL :: xlat1, xlat2 ! North lat for projection parallels
	INTEGER :: idum ! Not used
	CHARACTER(LEN=8) :: clab1 ! Var label ('ZFACE')
	REAL, DIMENSION(:), ALLOCATABLE :: zfacem ! Heights (m)
	CHARACTER(LEN=8) :: clab2 ! Var label ('XSSTA')
	REAL, DIMENSION(:), ALLOCATABLE :: xssta ! X coords of surface met sta
	CHARACTER(LEN=8) :: clab3 ! Var label ('YSSTA')
	REAL, DIMENSION(:), ALLOCATABLE :: yssta ! Y coords of surface met sta
	CHARACTER(LEN=8) :: clab4 ! Var label ('XUSTA')
	REAL, DIMENSION(:), ALLOCATABLE :: xusta ! X coords of upper air sta
	CHARACTER(LEN=8) :: clab5 ! Var label ('YUSTA')
	REAL, DIMENSION(:), ALLOCATABLE :: yusta ! Y coords of upper air sta
	CHARACTER(LEN=8) :: clab6 ! Var label ('XPSTA')
	REAL, DIMENSION(:), ALLOCATABLE :: xpsta ! X coords of precip sta
	CHARACTER(LEN=8) :: clab7 ! Var label ('YPSTA')
	REAL, DIMENSION(:), ALLOCATABLE :: ypsta ! U coords of precip sta
	CHARACTER(LEN=8) :: clab8 ! Var label ('Z0')
	REAL, DIMENSION(:, :), ALLOCATABLE :: z0 ! surface roughness (m))
	CHARACTER(LEN=8) :: clab9 ! Var label ('ILANDU')
	REAL, DIMENSION(:, :), ALLOCATABLE :: ilandu ! land use category
	CHARACTER(LEN=8) :: clab10 ! Var label ('ELEV')
	REAL, DIMENSION(:, :), ALLOCATABLE :: elev ! terrain elevation
	CHARACTER(LEN=8) :: clab11 ! Var label ('XLAI')
	REAL, DIMENSION(:, :), ALLOCATABLE :: xlai ! leaf area index
	CHARACTER(LEN=8) :: clab12 ! Var label ('NEARS')
	REAL, DIMENSION(:, :), ALLOCATABLE :: nears ! nearest surface met sta

	!CALMET Data Variables
	! Variable names from file
	INTEGER :: ndathr
	CHARACTER(LEN=8) :: clabu, clabv, clabw, clabt, clabsc, clabus
	CHARACTER(LEN=8) :: clabzi, clabl, clabws, clabrmm, clabtk, clabd
	CHARACTER(LEN=8) :: clabq, clabrh, clabpc
	REAL, DIMENSION(:, :, :), ALLOCATABLE :: U ! U wind (m/s)
	REAL, DIMENSION(:, :, :), ALLOCATABLE :: V ! V wind (m/s)
	REAL, DIMENSION(:, :, :), ALLOCATABLE :: W ! Vertical velocity (m/s)
	REAL, DIMENSION(:, :, :), ALLOCATABLE :: ZTEMP ! 3d Temperature (K)
	REAL, DIMENSION(:, :), ALLOCATABLE :: IPGT ! PGT stability class
	REAL, DIMENSION(:, :), ALLOCATABLE :: USTAR ! Surface friction velocity (m/s)
	REAL, DIMENSION(:, :), ALLOCATABLE :: ZI ! Mixing Height (m)
	REAL, DIMENSION(:, :), ALLOCATABLE :: EL ! Monin-Obukhov Length (m)
	REAL, DIMENSION(:, :), ALLOCATABLE :: WSTAR ! Convective veolocity scale (m/s)
	REAL, DIMENSION(:, :), ALLOCATABLE :: RMM ! Precipitation rate (mm/hr)
	REAL, DIMENSION(:, :), ALLOCATABLE :: TEMPK ! Near-surface Temp (K)
	REAL, DIMENSION(:, :), ALLOCATABLE :: RHO ! Near Surface air Density (kg/m^3)
	REAL, DIMENSION(:, :), ALLOCATABLE :: QSW ! Short-Ware solar radiation (W/m^2)
	REAL, DIMENSION(:, :), ALLOCATABLE :: IRH ! Near-Surface Relative Humidity
	REAL, DIMENSION(:, :), ALLOCATABLE :: IPCODE ! Precip type

	! File variables
	INTEGER :: calunit ! Unit number for CALMET output file
	INTEGER :: logunit ! Unit number for I/O API log file
	CHARACTER(LEN=16) :: calg2f
	CHARACTER(LEN=16) :: cald2f
	CHARACTER(LEN=16) :: cald3f

	! File Metadata
	! 2d grid
	INTEGER, DIMENSION(5) :: g2type
	CHARACTER(LEN=16), DIMENSION(5) :: g2name, g2units
	CHARACTER(LEN=80), DIMENSION(5) :: g2desc
	DATA g2type /m3real, m3int, m3real, m3real, m3real/
	DATA g2name /'Z0', 'ILANDU', 'ELEV', 'XLAI', 'NEARS'/
	DATA g2units /'meters', 'none', 'meters', 'none', 'none'/
	DATA g2desc /'Surface roughness length', 'Land use category', &
		'Terrain elevation', 'Leaf Area Index', 'Nearest surface met station'/

	! 3d data
	INTEGER, DIMENSION(4) :: d3type
	CHARACTER(LEN=16), DIMENSION(4) :: d3name, d3units
	CHARACTER(LEN=80), DIMENSION(4) :: d3desc
	DATA d3type /4*m3real/
	DATA d3name /'U', 'V', 'W', 'T'/
	DATA d3units /3*'m/s', 'K'/
	DATA d3desc /'U-component of the wind', 'V-component of the wind', &
		'W-component of the wind', 'Air temperature'/

	! 2d data
	INTEGER, DIMENSION(11) :: d2type
	CHARACTER(LEN=16), DIMENSION(11) :: d2name, d2units
	CHARACTER(LEN=80), DIMENSION(11) :: d2desc
	DATA d2type /m3int, 8*m3real, 2*m3int/
	DATA d2name /'IPGT', 'USTAR', 'ZI', 'EL', 'WSTAR', 'RMM', 'TEMPK', &
		'RHO', 'QSW', 'IRH', 'IPCODE'/
	DATA d2units /'none', 'm/s', 'm', 'm', 'm/s', 'mm/hr', 'K', 'kg/m^3', &
		'W/m^2', '%', 'none'/
	DATA d2desc /'PGT stability class', 'Surface friction velocity', &
		'Mixing height', 'Monin-Obukhov length', 'Convective velocity scale', &
		'Precipiation rate', 'Near-surface temperature', &
		'Near-surface air density', 'Short-wave solar radiation', &
		'Near-surface relative humidity', 'Precipitation type code'/

	! internal variables
	INTEGER :: g2vars = 4
	INTEGER :: d3vars = 2
	INTEGER :: d2vars = 11
	INTEGER :: i, j, k, t
	INTEGER :: allocStatus
	INTEGER :: JDATE, JTIME

	! internal constants
	CHARACTER(LEN=12), PARAMETER :: progname = 'calmet2ioapi'

	! CALMET Variables not in docs
	integer :: ibsec, ieyr, iemo, iedy, iehr, iesec
	CHARACTER(LEN=8) :: axtz
	!*******************************************************************
	! BEGIN CODE
	!*******************************************************************
	!=============================
	! open unformatted input file
	!=============================
	calunit = promptffile('Enter logcalunit name for CALMET output file', &
		.TRUE., .FALSE., 'CALMET', progname)

	!=========================
	! Read header information
	!=========================
	! File Declaration
	read(calunit) dataset, dataver, datamod
	print*,'dataset = ', dataset, ' dataver = ', dataver, ' datamod = ', datamod

	! Comment lines
	read(calunit) ncom
	allocate(comment(ncom))
	do i = 1, ncom
		read(calunit) comment(i)
	enddo

	if(dataver.eq.'2.1')then ! Version 6 ???
		read(calunit) ibyr, ibmo, ibdy, ibhr, ibsec, ieyr, iemo, iedy, &
			iehr, iesec, axtz, irlg, irtype, nx, ny, nz, dgrid, xorigr, &
			yorigr, iwfcod, nssta, nusta, npsta, nowsta, nlu, iwat1, iwat2,&
			lcalgrd, pmap,datum,daten,feast,fnorth,utmhem,iutmzn, rnlat0, &
			relon0,xlat1,xlat2
	else ! Our version & documented version
		read(calunit)ibyr, ibmo, ibdy, ibhr, ibtz, irlg, irtype, nx, ny, nz, &
			dgrid, xorigr, yorigr, iwfcod, nssta, nusta, npsta, nowsta, &
			nlu, iwat1, iwat2, lcalgrd, pmap, datum, daten, feast, fnorth, &
			utmhem, iutmzn, rnlat0, relon0, xlat1, xlat2
	endif

	!**********************
	! Allocate grid arrays
	!**********************
	! Vertcalunit
	allocate(zfacem(nz + 1), stat = allocStatus)
	if(allocStatus /= 0) stop "error allocating vertcalunit array"

	! Stations
	allocate(xssta(nssta), yssta(nssta), xusta(nusta), yusta(nusta), &
		xpsta(npsta), ypsta(npsta), stat = allocStatus)
	if(allocStatus /= 0) stop "error allocating station arrays"

	! 2d grid arrays
	allocate(z0(nx, ny), elev(nx, ny), xlai(nx, ny), nears(nx, ny), &
		ilandu(nx, ny), stat = allocStatus)
	if(allocStatus /= 0) stop "error allocating 2d grid arrays"

	!****************************
	! Read remaining header data
	!****************************
	read(calunit) clab1, idum, zfacem
	if(nssta .ge. 1) then ! Met surface stations
		read(calunit) clab2, idum, xssta
		read(calunit) clab3, idum, yssta
	endif
	if(nusta .ge. 1) then ! Upper air stations
		read(calunit) clab4, idum, xusta
		read(calunit) clab5, idum, yusta
	endif
	if(npsta .ge. 1) then ! Precipitation stations
		read(calunit) clab6, idum, xpsta
		read(calunit) clab7, idum, ypsta
	endif
	read(calunit) clab8, idum, z0
	read(calunit) clab9, idum, ilandu
	read(calunit) clab10, idum, elev
	read(calunit) clab11, idum, xlai
	if(nssta .ge. 1) then
		read(calunit) clab12, idum, nears
		g2vars = g2vars + 1
	endif

	!============================
	! Setup output file metadata
	!============================
	! Initialize I/O API:
	logunit = INIT3()        !  initialization returns unit # for log

!
! - baa (02/28/12) Assumes either UTM or LCC, standard for most US applications
!
	! Set projection parameters
	if(pmap .eq.'UTM   ')then
	  gdtyp3d = utmgrd3
	  p_alp3d = dble(iutmzn)
	else
	  gdtyp3d = lamgrd3 ! Lambert conformal grid
	  ycent3d = rnlat0
	  xcent3d = relon0
	  p_alp3d = xlat1
	  p_bet3d = xlat2
	  p_gam3d = relon0
	endif 
	! Set grid parameters
	gdnam3d = 'calmet2netCDF'
	xorig3d = xorigr
	yorig3d = yorigr
	xcell3d = dgrid
	ycell3d = dgrid
	ftype3d = GRDDED3
	ncols3d = nx
	nrows3d = ny
	nthik3d = 1

	! Set vertical levels
	vgtyp3d = VGHVAL3 ! Height above mean sea level (m)
	vgtop3d = IMISS3
	vglvs3d = zfacem

	! Date fields
	sdate3d = 1000 * ibyr  +  julian(ibyr, ibmo, ibdy)
	stime3d = ibhr * 10000
	mxrec3d = irlg

	! File description
	fdesc3d(1) = comment(1)
	fdesc3d(2) = "Dataset name: " // TRIM(dataset)
	fdesc3d(3) = "Dataset version: " // TRIM(dataver)
	fdesc3d(4) = "Dataset message: " // TRIM(datamod)

	!====================================
	! Set grid specific file information
	!====================================
	tstep3d = 00000
	nlays3d = 1
	nvars3d = g2vars

	vtype3d(1:nvars3d) = g2type
	vname3d(1:nvars3d) = g2name
	units3d(1:nvars3d) = g2units
	vdesc3d(1:nvars3d) = g2desc

	!=======================================
	! Open 2d gridded file and write output
	!=======================================
	! open file
	calg2f = promptmfile('Enter file name for 2d gridded output', &
		FSCREA3, 'CALGRID2D', progname)

	! Write to file
	call writeError(calg2f, clab8, sdate3d, stime3d, z0, progname)

	call writeError(calg2f, clab9, sdate3d, stime3d, ilandu, progname)

	call writeError(calg2f, clab10, sdate3d, stime3d, elev, progname)

	call writeError(calg2f, clab11, sdate3d, stime3d, xlai, progname)

	IF(nssta .ge. 1) THEN
		call writeError(calg2f,clab12,sdate3d, stime3d, nears, progname)
	END IF

	!===================================
	! Allocate time dependent variables
	!===================================
	allocate(u(nx, ny, nz), v(nx, ny, nz), w(nx, ny, nz), &
		ztemp(nx, ny, nz), stat = allocStatus)
	if(allocStatus /= 0) stop "error allocating 3d arrays"

	allocate(el(nx, ny), ustar(nx, ny), zi(nx, ny), wstar(nx, ny), &
		tempk(nx, ny), rho(nx, ny), qsw(nx, ny), irh(nx, ny), rmm(nx, ny), &
		ipcode(nx, ny), ipgt(nx, ny), stat = allocStatus)
	if(allocStatus /= 0) stop "error allocating 2d arrays"

	JDATE = sdate3d
	JTIME = stime3d

	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	! Begin looping through time periods
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	do t = 1, irlg
		!**********************
		! Read wind components
		!**********************
		do k = 1, nz
			read(calunit) clabu, ndathr,((u(i,j,k),i=1,nx),j=1,ny)
			read(calunit) clabv, ndathr,((v(i,j,k),i=1,nx),j=1,ny)
			if(lcalgrd)then
				read(calunit) clabw, ndathr,((w(i,j,k),i=1,nx),j=1,ny)
				if(k .eq. 1) d3vars = d3vars + 1
			endif
		enddo

		!***************************
		! Read 3d temperature field
		!***************************
		if(lcalgrd .and. irtype .eq. 1)then
			do k = 1, nz
				read(calunit) clabt, ndathr,((ztemp(i,j,k),i=1,nx),j=1,ny)
				if(k .eq. 1) d3vars = d3vars + 1
			enddo
		endif

		!***************
		! Read 2 fields
		!***************
		read(calunit) clabsc, ndathr, ipgt
		read(calunit) clabus, ndathr, ustar
		read(calunit) clabzi, ndathr, zi
		read(calunit) clabl, ndathr, el
		read(calunit) clabws, ndathr, wstar
		if(npsta.ne.0)then
		  read(calunit) clabrmm, ndathr, rmm
		endif
		read(calunit) clabtk, ndathr, tempk
		read(calunit) clabd, ndathr, rho
		read(calunit) clabq, ndathr, qsw
		read(calunit) clabrh, ndathr, irh
		if(npsta.ne.0)then
		  read(calunit)clabpc,ndathr,ipcode
		endif

	if(t .eq. 1) then
		! Create 2d file
		nlays3d = 1

		nvars3d = d2vars
		vname3d(1:nvars3d) = d2name
		vdesc3d(1:nvars3d) = d2desc
		vtype3d(1:nvars3d) = d2type
		units3d(1:nvars3d) = d2units

		mxrec3d = irlg
		tstep3d = 10000
		cald2f = promptmfile('Enter file name for 2d data output', &
		FSCREA3, 'CALMET2D', progname)

		! Create 3d file
		nlays3d = nz

		nvars3d = d3vars
		vname3d(1:nvars3d) = d3name
		vdesc3d(1:nvars3d) = d3desc
		vtype3d(1:nvars3d) = d3type
		units3d(1:nvars3d) = d3units

		cald3f = promptmfile('Enter file name for 3d data output', &
		FSCREA3, 'CALMET3D', progname)


	end if ! end first timestep if

	!***********************
	! Write 2d data to file
	!***********************
	! IPTG
	call writeError(cald2f, clabsc,JDATE,JTIME,ipgt, progname)
	! ustar
	call writeError(cald2f, clabus,JDATE,JTIME,ustar, progname)
	! zi
	call writeError(cald2f, clabzi, JDATE, JTIME, zi, progname)
	! mol
	call writeError(cald2f, clabl, JDATE, JTIME, el, progname)
	! wstar
	call writeError(cald2f,clabws,JDATE,JTIME,wstar, progname)
	! rmm
	if(npsta.ne.0)then
	 call writeError(cald2f,clabrmm,JDATE,JTIME,rmm, progname)
	endif
	! tempk
	call writeError(cald2f,clabtk, JDATE,JTIME, tempk, progname)
	! rho
	call writeError(cald2f,clabd, JDATE,JTIME, rho, progname)
	! qsw
	call writeError(cald2f,clabq, JDATE,JTIME, qsw, progname)
	! irh
	call writeError(cald2f,clabrh, JDATE,JTIME, irh, progname)
	! ipcode
	if(npsta.ne.0)then
	  call writeError(cald2f,clabpc, JDATE,JTIME, ipcode, progname)
	endif

	!***********************
	! Write 3d data to file
	!***********************
	call writeError(cald3f, 'U', JDATE,JTIME,u, progname)

	call writeError(cald3f, 'V', JDATE, JTIME, v, progname)

	if(lcalgrd)then
		call writeError(cald3f, 'W', JDATE, JTIME, w, progname)

		call writeError(cald3f, 'T', JDATE, JTIME, ztemp, progname)
	endif

	call nextime(JDATE, JTIME, 10000)

enddo

close(calunit)
CALL M3EXIT('calmet2netCDF_v5_53', JDATE, JTIME, &
	'Successful completion of program calmet2netCDF_v5_53', 0)

end program calmet2ioapi

subroutine writeError(f, var, jdate, jtime, arr, progname)
	!===================================================================
	! Attempts to write array to output file, creating an error message
	! if write attempt fails.
	!===================================================================
	implicit none

	INCLUDE 'PARMS3.EXT'      ! I/O API constants
	INCLUDE 'FDESC3.EXT'      ! I/O API file description data structure
	INCLUDE 'IODECL3.EXT'     ! I/O API function declarations
	CHARACTER(LEN=16), INTENT(IN) :: f
	CHARACTER(LEN=*), INTENT(IN) :: var
	INTEGER, INTENT(IN) :: jdate, jtime
	REAL, DIMENSION(*), INTENT(IN) :: arr
	CHARACTER(*), INTENT(IN) :: progname

	CHARACTER(LEN=160) :: mesg

	IF(.NOT. WRITE3(f, var, jdate, jtime, arr)) THEN
			MESG = 'Error writing "'//trim(var)//' to file "'//trim(f)//'"'
			CALL M3EXIT(progname, jdate, jtime, mesg, 2)
		END IF
end subroutine
