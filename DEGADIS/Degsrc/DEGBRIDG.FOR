c
c... Code for Microsoft's FORTRAN 5.0 that is not used with Intel's compiler
c
c$NOTSTRICT
c        INTERFACE TO FUNCTION SYSTEM[C] (STRING)
c        INTEGER*2 SYSTEM
c        CHARACTER*1 STRING[REFERENCE]
c        END
c........................................................................
c
	PROGRAM DEGBRIDG
C
c........................................................................
c
C	Program description:
C
C	DEGBRIDG is designed to read the input file for the JETPLU/DEGADIS
C	model input.  From this information and the output from the JETPLU
c	model, DEGBRIDG prepares the necessary input file to run DEGADIS
c	(RUN_NAME.INP).  This code is taylored for Microsoft's FORTRAN 5.0.
C
c..........................................................................
c
c	The structure of the RUN_NAME.IN file is outlined in JETPLUIN.
c
c	The output file from the OOMS model contains three (3) variables
c	as follows (in RUN_NAME.IND):
c
c	<DIST>  <CC>  <HWIDTH>
c
c	where:
c
c	  <DIST>   is the distance to the centerline touchdown (m)
c	  <CC>     is the centerline concentration at <DIST>
c	  <HWIDTH> is jet/plume half width at <DIST>
c
c	To establish the initial conditions for DEGADIS, the source
c	concentration is assumed to be <CC>, and the source radius is assumed
c	to be <HWIDTH>.
C
c..........................................................................
c
C	Program usage:
C
C	Consult Volume III of the Final Report to U. S. Coast Guard
C	contract DT-CG-23-80-C-20029 entitled "Development of an
C	Atmospheric Dispersion Model for Heavier-than-Air Gas Mixtures".
C
C	J. A. Havens
C	T. O. Spicer
C
C	University of Arkansas
C	Department of Chemical Engineering
C	Fayetteville, AR 72701
C
C	April 1985
C
C
C	This project was sponsored by the U. S. Coast Guard and the Gas
C	Research Institute under contract DT-CG-23-80-C-20029.
C
C
C	Disclaimer:
C
C	This computer code material was prepared by the University of
C	Arkansas as an account of work sponsored by the U. S. Coast Guard
C	and the Gas Research Institute.  Neither the University of Arkansas,
C	nor any person acting on its behalf:
C
C	a.  Makes any warranty or representation, express or implied,
C	    with respect to the accuracy, completeness, or usefulness
C	    of the information contained in this computer code material,
C	    or that the use of any apparatus, method, numerical model,
C	    or process disclosed in this computer code material may not
C	    infringe privately owned rights; or
C
C	b.  Assumes any liability with respect to the use of, or for
C	    damages resulting from the use of, any information,
C	    apparatus, method, or process disclosed in this computer
C	    code material.
C
C
C***************************************************************************
C***************************************************************************
C***************************************************************************
C
C
	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )
C
	include 'degin.prm'
c
	COMMON
     ./cgprop/	gasmw,temjet,rhoe,cpk,cpp,gasulc,gasllc,gaszzc,gasnam
     ./comatm/	istab,tamb,pamb,humid,isofl,tsurf,ihtfl,htco,iwtfl,wtco,
     .		humsrc
     ./comss/	ess,slen,swid,outcc,outsz,outb,outl
     ./csigx/	sigxco,sigxp,sigxmd,sigxfl
     ./GEN1/	PTIME(igen), ET(igen), R1T(igen), PWC(igen), PTEMP(igen),
     .		PFRACV(igen), PENTH(igen), PRHO(igen)
     ./GEN2/	DEN(5,igen)
     ./ITI/	T1,TINP,TSRC,TOBS,TSRT
     ./NEND/	POUND
     ./oomsin/	oodist,avtime
     ./PARM/	U0,Z0,ZR,rml,USTAR,vkc,gg,gRHOE,RHOA,DELTA,BETA,GAMMAF,
     .		ycLOW
     ./PHLAG/	CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
     ./TITL/	TITLE
C
	character*80 TITLE(4)
C
	character*3 gasnam
	character*24 TSRC,TINP,TOBS,TSRT
C
	LOGICAL CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5

	character*100 opnf
	character opnf1(100)
	equivalence (opnf1(1),opnf)

	character*4 in,ind,INP
	character*100 dummy
c
c... include the interface for the _system function
c       declare appropriate return type
c
	INTEGER*4 SYSTEM,iv
c
c
c check1
c check2=t	cloud type release with no liquid source; SRC1	DEGADIS1
c again		local communications in SSSUP			SSSUP
c check3	local communications between SRC1 and NOBL	DEGADIS1
c check4=t	steady state simulation				DEGADISIN
c check5=t	operator sets sort parameters			ESTRT3
c
	data CHECK1/.false./,CHECK2/.false./,AGAIN/.false./
	data CHECK3/.false./,CHECK4/.false./,CHECK5/.false./
C
	DATA POUND/-1.E-20/
C
	DATA PTIME/igen*0.D0/
	DATA ET/igen*0.D0/,R1T/igen*0.D0/
	DATA PWC/igen*0.D0/,PTEMP/igen*0.D0/
	DATA PFRACV/igen*0.D0/,PENTH/igen*0.D0/
	DATA PRHO/igen*0.D0/
	data DEN/igen*0.,igen*0.,igen*0.,igen*0.,igen*0./

	DATA INP/'.INP'/,IN/'.IN '/,IND/'.IND'/
C
c
C... GET THE FILE NAME TO BE USED BY ALL OF THE ROUTINES........................
C
	write(6,*) 'Beginning DEGBRIDGE...'

	call runname(opnf)
	nchar = index(opnf,' ') - 1
	opnf = opnf(1:nchar) // ind(1:4)
C
C... First, get the desired information from RUN_NAME.IND
C
	open(unit=8, file=opnf, status='OLD')
	read(8,*) oodist, cc, hwidth
	close(unit=8)
c
c... If OODIST is zero, discontinue the run
c
	if(oodist .eq. 0.D0) then
	    write(6,*) '   ...terminating the run since yCLOW is met.'
c
c... invoke DOS' command.com with the command line to copy the file
c
	    dummy = opnf(1:nchar)
	    opnf = 'copy '//dummy(1:nchar)//'.out  '//
     .				dummy(1:nchar)//'.lis'
	    iv = SYSTEM(opnf(1:2*nchar+15))
	    opnf = 'del '//dummy(1:nchar)//'.inp'
	    iv = SYSTEM(opnf(1:nchar+8))
	    iv = SYSTEM('echo ...')
	    iv = SYSTEM('echo ...')
	    iv = SYSTEM(
     .		'echo Ignore any file-not-found messages to follow.')
	    iv = SYSTEM('echo ...')
	    iv = SYSTEM('echo ...')
	    iv = SYSTEMQQ('exit /b')
	    stop
	endif
C
C... Now, get the desired information from RUN_NAME.IN
C
	opnf = opnf(1:nchar) // in(1:4)
	open(unit=8, file=opnf, status='OLD')

	read(8,8000) title(1)
	read(8,8000) title(2)
	read(8,8000) title(3)
	read(8,8000) title(4)
	read(8,*) u0, z0
	read(8,*) zr

	read(8,*) indvel, istab, rml

	if( indvel.ne.1 .or. indvel.ne.2) indvel = 1
	if( istab.le.0 .or. istab.gt.6) istab = 4

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
	humsrc= 0.D0

	read(8,*) tsurf
	if( tsurf.lt.250. ) tsurf = tamb

	read(8,8010) gasnam
	read(8,*) gasmw
	read(8,*) avtime
	read(8,*) TEMJET
	read(8,*) gasulc, gasllc, gaszzc
	if(gasllc .le. 0.) gasllc = 0.01
	if(gasulc .le. gasllc) gasulc = dmax1( 1.1D0*gasllc, 1.0D0)

c
c... Now that AVTIME has been set, the value of DELTAY can be fixed.  Also
c	set the values of BETAY
c
c
c... Based on INDVEL, set the Monin-Obukhov length as desired............
c
	rrml = rml
	call atmdef(zr, istab, avtime, rml, deltay, betay,
     .		deltaz, betaz, gammaz, sigxco, sigxp, sigxmd)
	if(indvel .ne. 1) rml = rrml
c
c... Recover INDHT, CPK, and CPP.  If CPP is set to 0, then CPK contains........
c	the (constant) heat capacity.
c
	read(8,*) indht, CPK, cpp
	if(cpp .eq. 0.D0) then
		cpp = 1.D0
		CPK = CPK*gasmw - 3.33D4
	endif

c
c... recover NDEN and set ISOFL and DEN accordingly.............................
c
	read(8,*) nden
	if(nden .lt. -1) nden=-1

	if(nden .eq. -1) then
		isofl = 1
                rhoe = pamb*gasmw/rgas/TEMJET
		grhoe = rhoe
                rhoa = Pamb*
     .              (1.D0+humid)*wmw/(rgas*(wmw/wma + humid))/Tamb
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
		den(1,3) = 2.D0
	else if(nden .eq. 0) then
		isofl = 0

                rhoe = pamb*gasmw/rgas/TEMJET
		grhoe = rhoe
                rhoa = Pamb*
     .              (1.D0+humid)*wmw/(rgas*(wmw/wma + humid))/Tamb

		wc = 1.D0
		wa = 0.D0
		enth = cpc(temjet)*(temjet - tamb)
		call setden(wc, wa, enth)

	else
		isofl = 1

		do iii = 1,nden
		read(8,*) den(1,iii), den(2,iii), den(3,iii)
		den(4,iii) = 0.D0
		den(5,iii) = tamb
		enddo
		den(1,nden+1) = 2.D0
		rhoe = den(3,nden)
		grhoe = rhoe
		rhoa = den(3,1)
	endif

	nden0 = nden
	if(nden .eq. -1) nden0 = 2


	read(8,*) erate
	read(8,*) elejet, DIAJET
	read(8,*) tend
	check4 = .true.
	if(tend .gt. 0.) check4 = .false.

	read(8,*) distmx

	close(unit=8)

c
c... It is time to prepare the input file for DEGADIS....................
c
	opnf = opnf(1:nchar) // inp(1:4)
	open(unit=8, file=opnf, status='UNKNOWN')
c
c... TITLE
c
	DO 200 I=1,4
	WRITE(8,8000) TITLE(I)
  200	CONTINUE
C
c... Atmospheric parameters:
c
	WRITE(8,1020) U0,Z0,ZR
C
c... stability, averaging time for DELTAY, and derived parameters
c
	WRITE(8,1040) istab
	write(8,1020) oodist,avtime
	WRITE(8,1060) indvel,rml
c
c... ambient pressure, temperatures, and humidity
c
	write(8,1025) tamb,pamb,humid,relhum
c
	ihtfl = indht
	htco  = 0.
	iwtfl = 0
	wtco  = 0.
c
	write(8,1060) isofl,tsurf
	write(8,1060) ihtfl,htco
	write(8,1060) iwtfl,wtco
C
c... gas characteristics
c
	write(8,8010) gasnam
	WRITE(8,1020) gasmw, temjet,rhoe
	write(8,1020) cpk,cpp
	WRITE(8,1020) gasulc,gasllc,gaszzc
c
c... density curve if isothermal
c
	if(isofl .eq. 0) goto 460

	WRITE(8,1040) nden0
	DO 440 I=1,nden0
	WRITE(8,1025) DEN(1,I),DEN(2,I),DEN(3,I),Den(4,I),den(5,i)
  440	CONTINUE
C
c
 460	yclow = gasllc/1.02D0
	WRITE(8,1010) ycLOW
c
c... source description FOR A DILUTED SOURCE ...
C
	call adiabat(0,wc,wa,yc,ya,cc,rho,wm,enth,temp)
c
	gmass0 = 0.		! no initial cloud mass
	write(8,1020) gmass0
	np = 4
c
	if(check4) tend = 60230.D0 ! [=] sec
c
	ess  = erate
	r1ss = hwidth
	slen = 2.D0*hwidth
	swid = pi*r1ss**2/slen/2.D0
	pwc(1) = wc
	ptemp(1) = temp
	pfracv(1) = 1.0
c
	PTIME(1) = 0.D0
	et(1)    = ess
	r1t(1)   = r1ss
	PWC(1)   = pwc(1)
	PTEMP(1) = ptemp(1)
	PFRACV(1)= 1.0D0

	PTIME(2) = tend
	et(2)    = ess
	r1t(2)   = r1ss
	PWC(2)   = pwc(1)
	PTEMP(2) = ptemp(1)
	PFRACV(2)= 1.0D0

	PTIME(3) = tend + 1.
	et(3)    = 0.
	r1t(3)   = 0.
	PWC(3)   = pwc(1)
	PTEMP(3) = ptemp(1)
	PFRACV(3)= 1.0D0

	PTIME(4) = tend + 2.
	et(4)    = 0.
	r1t(4)   = 0.
	PWC(4)   = pwc(1)
	PTEMP(4) = ptemp(1)
	PFRACV(4)= 1.0D0

	WRITE(8,1040) NP
	DO 800 I=1,NP
  800	WRITE(8,1030) PTIME(I),ET(I),R1T(I), PWC(I),PTEMP(I),PFRACV(I)
C
	write(8,*) check4
C
	call dattim(tinp)
	write(8,1050) tinp
C
	CLOSE(UNIT=8)
C
c
 1010	format(1x,1pg14.7)
 1020	format(3(1x,1pg14.7))
 1025	format(5(1x,1pg14.7))
 1030	format(1x,5(1pg14.7,1x),1pg14.7)
 1040	format(1x,I5)
 1050	format(a24)
 1060	format(1x,i4,1x,1pg14.7)

 8000	format(a80)
 8010	format(a3)
 8020	format(80a1)
c
	stop
	END
