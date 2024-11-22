	Program jetpluin
c
c........................................................................
c
c	JETPLUIN is designed to perform two tasks including:
c
c	a) Read the input file for the JETPLU/DEGADIS model input.  From
c	this information, JETPLUIN prepares the necessary file to run the
c	JETPLU/DEGADIS model.
c
c	b) Generate a command file to execute the JETPLU/DEGADIS model from
C	the input information.  This portion is highly dependent on the
c	VAX/VMS environment because of the manipulation of character strings
c	and the instructions necessary to invoke the VAX/VMS commands.
c
c.........................................................................
c
c	The input file RUNNAME.IN is to be created before this program
c	is invoked using free formats.  The order of input is as follows:
c
c	<TITLE1>
C	<TITLE2>
C	<TITLE3>
C	<TITLE4>
C
C	<U0>  <Z0>
C	<ZR>
C	<INDVEL>  <ISTAB>  <RML>
C	<TAMB>  <PAMB>  <RELHUM>
C	<TSURF>
C
C	<GASNAM>
C	<GASMW>
C	<AVTIME>
C	<TEMJET>
C	<GASULC>  <GASLLC>  <GASZZC>
C	<INDHT>  <CPK>  <CPP>
C	<NDEN>
C
C	<ERATE>
C	<ELEJET>  <DIAJET>
C	<TEND>
c	<DISTMX>
C
C	Note that for readability, blank lines were inserted between the
c	input sections specifying a simulation title, atmospheric conditions,
c	gas properties, and the particular release conditions.  Symbol
c	definitions are as follows:
c
c	<TITLE1>, <TITLE2>, <TITLE3>, and <TITLE4> are four lines of up
c		to 80 characters each of a title for this simulation.
c
c	<U0> (m/s) is the ambient wind velocity at <Z0> (m).
c
c	<ZR> is the surface roughness (m).
c
c	<INDVEL> is an indicator which determines the method of calculation
c		for the ambient velocity profile in the jet/plume model as
c		follows:
c
c		For <INDVEL>=1, the Pasquill-Gifford stability category
c		   (in <ISTAB> using 1 for A, 2 for B, etc.) is used
c		   along with <ZR> to determine the Monin-Obukhov length
c		   <RML>; the log velocity profile is then determined
c		   using <RML>.
c
c		For <INDVEL>=2, the Monin-Obukhov length <RML> is supplied
c		   by the user; the log velocity profile is then determined
c		   using <RML>.  Note that <ISTAB> must still be specified.
c
c	<TAMB>, <PAMB>, and <RELHUM> are the ambient temperature (K), the
c		ambient pressure (atm or N/m**2), and the relative humidity
c		(%), respectively.
c
c	<TSURF> is the surface temperature (K); if <TSURF> < 250 K, <TSURF> is
c		set to <TAMB>.
c
c	<GASNAM> is a three-letter designation for the contaminant's name.
c		Any character string of three letters or less is valid; this
c		is for user run identification and does not access property
c		data.
c
c	<GASMW> is the contaminant's molecular weight (kg/kmole).
c
c	<AVTIME> is the averaging time (s).  This parameter is
c		used to estimate the value of <DELTAY>.
c
c	<TEMJET> is the temperature of the jet (K).
c
c	<GASULC> and <GASLLC> are the concentrations to be used for estimating
c		contours for an upper and lower concentration level in
c		DEGADIS.  The calculations are made for the elevation
c		<GASZZC>.  Note that the JETPLU/DEGADIS computations will be
c		carried out to <GASLLC> for steady-state release simulations
c		and <GASLLC>/4 for transient release simulations.
c
c	<INDHT> is used to include heat transfer in the DEGADIS computations.
c		Heat transfer is not included for <INDHT>=0.  For <INDHT>=1,
c		heat transfer is included, and the heat transfer coefficient
c		is calculated by DEGADIS.  <CPP> and <CPK> are used to
c		calculate the heat capacity as a function of
c		temperature according to the correlation included in
c		DEGADIS.  If a constant heat capacity is
c		desired, set <CPP> to 0. and <CPK> to the desired heat
c		capacity (J/kg K).
c
c	<NDEN>	is used to specify the contaminant density profile.
c		There are three cases for <NDEN>:
c
c		<NDEN> = -1; The simulation treats the contaminant as if
c			it were an ideal gas with a molal heat capacity
c			equal to that of air.  Water condensation effects
c			are ignored.  (equivalent to "isothermal" mode)
c
c		<NDEN> = 0;  The simulation treats the contaminant as if
c			it were an ideal gas with the heat capacity indicated
c			by <CPK> and <CPP>.  Water condensation effects
c			are taken into account as appropriate.
c			(equivalent to "nonisothermal" mode)
c
c		<NDEN> > 0; <NDEN> specifies the number of triples which follow
c			in the next <NDEN> lines.  The triples are used to
c			specify the contaminant concentration as a function of
c			density based on adiabatic mixing with ambient air.
c			The ordered triples represent (in order):
c			  (1) the contaminant mole fraction
c			  (2) the contaminant concentration (kg contam/m3 mix)
c			  (3) the mixture density (kg mixture/m3 mixture)
c			The ordered triples must go from pure air to pure
c			contaminant.  (equivalent to "isothermal" mode)
c
c	<ERATE> is the mass evolution (release) rate (kg/s).
c
c	<ELEJET> is the inital jet elevation (m); the minimum jet elevation
c		is twice the surface roughness.  <DIAJET> is the initial
c		jet diameter (m).
c
c	<TEND> is the duration of the primary release (s).  For steady-state
c		releases, set <TEND> to 0.; to run the jet/plume model only,
c		set <TEND> to a negative number.
c
c	<DISTMX> is the maximum distance between output points in the JETPLU
c		output (m).
c
c.........................................................................
c
c	T. Spicer
c	University of Arkansas
c	Department of Chemical Engineering
c	Fayetteville, AR 72701
c
c	(501) 575-4951
c
c.........................................................................
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	include 'DEGIN.prm'

	dimension DEN(5,igen)
C
	logical check4
c
	character*80 TITLE(4)
C
	character*100 OPNf
	character OPNf1(100)
	equivalence (opnf1(1),opnf)

	character*3 gasnam
c
	data vkc/0.35D0/
c
C... GET THE FILE NAME TO BE USED BY ALL OF THE ROUTINES.......................
C
	call runname(opnf)
	nchar = index(opnf,' ') - 1
	opnf = opnf(1:nchar) // '.in'
C
C... Now get the rest of the desired information from RUNNAME.IN
C
	open(unit=8, file=opnf, status='OLD')

	read(8,8000) title(1)
	read(8,8000) title(2)
	read(8,8000) title(3)
	read(8,8000) title(4)
 8000	format(a80)
 8010	format(a3)
	read(8,*) u0, z0
	read(8,*) zr

	read(8,*) indvel, istab, rml
c
c... check for out-of-bounds responses
c
	if( indvel.ne.1 .and. indvel.ne.2) indvel = 1
	if( istab.le.0 .or. istab.gt.6) istab = 4
c
c... TAMB, PAMB, RELHUM
c
	read(8,*) tamb, pamb, relhum

	if( tamb.le.0. ) tamb = tamb+273.15D0
	if( pamb.gt.1.1 ) pamb = pamb/101325.D0
	if( relhum.lt.0. .or. relhum.gt.100. ) relhum = 50.
c
c... Calculate the absolute humidity HUMID
c
	vaporp = watvp(tamb)
	sat = wmw/wma*vaporp/(pamb - vaporp)
	humid = relhum/100.D0 * sat

	read(8,*) tsurf
	if( tsurf.lt.250. ) tsurf = tamb

	read(8,8010) gasnam
	read(8,*) gasmw
	read(8,*) avtime
	read(8,*) TEMJET
	read(8,*) gasulc, gasllc, gaszzc
	if( gasllc.le.0. ) gasllc = 0.01
	if( gasulc.le.gasllc ) gasulc = dmax1( 1.1D0*gasllc, 1.0D0)

c
c... Now that AVTIME has been set, ATMDEF can be called to set the value of....
c	DELTAY BETAY, DELTAZ, BETAZ, and GAMMAZ.  RML will be set as needed.
c	Also, calculate USTAR.
c
	rrml = rml
	call atmdef(zr, istab, avtime, rml, deltay, betay,
     .		deltaz, betaz, gammaz, sigxco, sigxp, sigxmd)
	if(indvel .ne. 1) rml = rrml
	ustar = u0*vkc/(dlog((z0+zr)/zr) - psif(z0,rml))


c
c... Recover INDHT, CPK, and CPP.  If CPP is set to 0, then CPK contains.......
c	the (constant) heat capacity.
c
	read(8,*) indht, CPK, cpp
	if(cpp .eq. 0.D0) then
		cpp = 1.D0
		CPK = CPK*gasmw - 3.33D4
	endif

c
c... recover NDEN and set ISOFL and DEN accordingly............................
c
	read(8,*) nden
	if(nden .lt. -1) nden=-1

	if(nden .eq. -1) then
	    isofl = 1
	    rhoe = pamb*gasmw/rgas/TEMJET
	    rhoa = Pamb*(1.D0+humid)*wmw/(rgas*(wmw/wma + humid))/Tamb
	    den(1,1) = 0.D0
	    den(2,1) = 0.D0
	    den(3,1) = rhoa
	    den(4,1) = 0.D0
	    den(5,1) = tamb
	    den(1,2) = 1.D0
	    den(2,2) = rhoe
	    den(3,2) = rhoe
	    den(4,2) = 0.D0
	    den(5,2) = tamb
	else if(nden .eq. 0) then
	    isofl = 0
	else
	    isofl = 1

	    do 100 iii = 1,nden
	    read(8,*) den(1,iii), den(2,iii), den(3,iii)
	    den(4,iii) = 0.D0
	    den(5,iii) = tamb
 100	    continue
	endif

	nden0 = nden
	if(nden .eq. -1) nden0 = 2


	read(8,*) erate
	read(8,*) elejet, DIAJET
	if(elejet .lt. 2.D0*zr) then
	    elejet = 2.D0*zr
	    write(6,*) 'JETPLUIN: ELEJET has been increased to: ',elejet
	endif
	read(8,*) tend
	check4 = .true.
	if(tend .gt. 0.) check4 = .false.

	read(8,*) distmx
c
c... It is time to prepare the input file for the JETPLU model.................
c
	opnf = opnf(1:nchar) // '.ino'
	open(unit=1, file=opnf, status='UNKNOWN')
	opnf = opnf(1:nchar) // ' '

	write(1,603) title(1)
	write(1,603) title(2)
	write(1,603) title(3)
	write(1,603) title(4)
c
c... atmospheric parameters
c
	write(1,*) u0, z0
	write(1,*) zr
	write(1,*) istab, rml, ustar
	write(1,*) tamb, pamb, humid, relhum, tsurf
	write(1,*) avtime, deltay, betay
	write(1,*) deltaz, betaz, gammaz
c
c... contaminant parameters
c
	write(1,*) gasmw
	write(1,*) temjet
	write(1,*) gasulc, gasllc, gaszzc
	write(1,*) CPK, cpp
	write(1,*) nden, nden0

	if(nden0 .gt. 0) then
	    do 200 iii = 1,nden0
	    write(1,*) (den(jjj,iii),jjj=1,5)
 200	    continue
	endif

	write(1,*) isofl
c
c... spill parameters
c
	write(1,*) erate
	write(1,*) elejet, diajet
c
c... coding parameters
c
	alfa1 = 0.028D0
	alfa2 = 0.37D0
	write(1,*) alfa1, alfa2
	write(1,*) distmx, tend


 603	format(a80)
	close(unit=1)

c
c... Now, prepare the command file and kick off the run .....................
c
	call runjet(opnf,tend,u0,check4)
c
	stop
	end
