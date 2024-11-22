	PROGRAM Pfile
C
C***************************************************************************
C***************************************************************************
C***************************************************************************
C
C	Program description:
C
C	PFILE reads the LIS file and outputs the max concentrations as
c	a function of distance and isopleths.
C
C
C	T. O. Spicer
C
C	University of Arkansas
C	Department of Chemical Engineering
C	Fayetteville, AR 72701
C
C	January 1990
C
C
C
C***************************************************************************
C***************************************************************************
C***************************************************************************
C
c
C

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	include 'DEG1.prm'
C
	COMMON
     ./ALP/	ALPHA,alpha1
     ./cgprop/	gasmw,gastem,gasrho,gascpk,gascpp,gasulc,gasllc,gaszzc,
     .		gasnam
     ./comatm/	istab,tamb,pamb,humid,isofl,tsurf,ihtfl,htco,iwtfl,wtco,
     .		humsrc
     ./comss/	ess,slen,swid,outcc,outsz,outb,outl,swcl,swal,senl,srhl
     ./csprd/	ce, delrmn
     ./GEN2/	DEN(5,igen)
     ./ITI/	T1,TINP,TSRC,TOBS,TSRT
     ./NEND/	POUND
     ./oomsin/	oodist,avtime
     ./PARM/	U0,Z0,ZR,rml,USTAR,vkc,gg,RHOE,RHOA,DELTAy,BETAy,GAMMAF,
     .		yclow
     ./phicom/	iphifl,dellay
     ./PHLAG/	CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
     ./titl/	title
C
	LOGICAL CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
c
	character*24 tsrc,tinp,tobs,tsrt
C
	character*3 gasnam
	character*80 title(4)
c
	logical jetplu, cflag
c
	character*4 dummy
c
	character*80 opnf1,str1
	character opnf(80),str(80)
C
	EQUIVALENCE (opnf(1),opnf1)
	EQUIVALENCE (str(1),str1)

C.........................................................
C
C	DATA
C
	DATA POUND/-1.D-20/
C
C
C... UNITS:
c
C	9 -- I/O WITH DISK
C
C
C... GET THE FILE NAME FOR FILE CONTROL
C
 100	write(6,1100)
 1100	format(/,' Enter the existing simulation name as a proper',
     .		' file name: ',$)
	read(5,1150) opnf1
 1150	format(a40)
C
c... open and read the LIS file to get some parameters of the run.............
c
	nchar = index(opnf1,' ') - 1
	opnf1 = opnf1(1:nchar) // '.lis'
	OPEN(UNIT=8, file=opnf1, status='OLD', err=100)
c
c... see which models were run and set JETPLU
C
	jetplu = .true.
	read(8,1300) str1
 1300	format(a80)
	nchr = index(str1,'JETPLU')
	if(nchr.eq.0) then
	    jetplu=.false.
	    goto 105
	endif
	goto 150
c
c... check to ensure INP file exists since there are no error handling
c	provisions in IO for "nonjet" runs.
c
 105	opnf1 = opnf1(1:nchar) // '.inp'
	open(unit=9, file=opnf1, status='OLD', err=110)
	close(unit=9)
	goto 120

 110	continue
	write(6,1180) (opnf(iii),iii=1,nchar+4)
 1180	format(/,' The specified INP file could not be opened: ',40a1)
	goto 100
C
c... read the INP file to get CHECK4....................................
c
 120	CALL io(gmass0, opnf1)
	goto 200
c
c... read the IN file to get check4......................................
c
 150	opnf1 = opnf1(1:nchar) // '.in'
	open(unit=1, file=opnf1, status='OLD', err=100)
c
	READ(1,'(a80)') (title(j),J=1,4)
	READ(1,*) u0,z0
	READ(1,*) ZR
	read(1,*) indvel,istab, rml
	read(1,*) tamb, pamb, relhum
	read(1,*) tsurf
	read(1,'(a3)')  gasnam
	read(1,*) gasmw
	read(1,*) avtime
	read(1,*) gastem
	read(1,*) gasulc, gasllc, gaszzc
	read(1,*) indht, gascpk, gascpp
	read(1,*) nden
	nden = max(nden, -1)
	isofl = 0
	if(nden .gt. 0) then
	    isofl = 1
	    do 112 iii = 1,nden
	    read(1,*) (den(jjj,iii),jjj=1,3)
  112	    continue
	    den(1,nden+1) = 2.D0
	    rhoa = den(3,1)
	    rhoe = den(3,nden)
	else
	    if(nden .eq. -1) isofl = 1
	    rhoa = Pamb*(1.D0+humid)*wmw/(rgas*(wmw/wma + humid))/Tamb
	    rhoe = pamb*gasmw/rgas/gastem
	endif
	gasrho = rhoe

	read(1,*) erate
	QINIT = erate/rhoe
	read(1,*) elejet, diajet

	READ(1,*) tend
	read(1,*) distmx

	close(unit=1)
c
	check4 = .false.
	if(tend.le.0.D0) check4 = .true.
c
c... set up the output files...............................................
c
 200	opnf1 = opnf1(1:nchar) // '.c'
	write(5,1291) (opnf(iii),iii=1,nchar+2)
 1291	format(//,' Maximum concentration as a function of ',
     .		'distance is in: ',40a1)
	open(unit=1, file=opnf1, status='UNKNOWN')

	opnf1 = opnf1(1:nchar) // '.i'
	write(5,1292) (opnf(iii),iii=1,nchar+2)
 1292	format(//,' Concentration isopleths as a function of ',
     .		'distance are in: ',40a1)
	open(unit=2, file=opnf1, status='UNKNOWN')
c
c... loop through any initial stuff in the JETPLU output; the next few reads
c	guarantee the comment section is bypassed before checking for '(m)'
c
	if(.not.jetplu) goto 300
	do 229 iii=1,8
	read(8,1300) opnf1
 229	continue

 230	read(8,1300) opnf1
	nchar = index(opnf1,'(m)')
	if(nchar.ne.0) then
	    read(8,1300) opnf1
	    goto 232
	endif
	goto 230

c
c... read through the "jet/plume" output
c
 232	continue
	iii = 0
 250	continue
	read(8,2250,end=300,err=300) dist,zj,yc,cc,rho,temp,sy,sz,
     .		yzsp,wllc,wulc,ym,zm
	write(1,9000) dist,ym,yzsp
	write(2,9000) dist,wllc,wulc
 9000	format(4(1x,1pg11.4))
 2250	format(1x,12(g9.3,1x),g9.3)
	iii = iii+1
	if(iii.eq.3) then
	    read(8,'(a4)',end=300,err=300) dummy
	    iii = 0
	endif
	goto 250
c
c
c... skip through the "nonjet" source and don't output anything.
c
 300	read(8,1300,end=800,err=800) opnf1
	read(8,1300,end=800,err=800) opnf1

	do 339 iii=1,13
	read(8,1300) opnf1
 339	continue

 340	read(8,1300) opnf1
	nchar = index(opnf1,'sec')
	if(nchar.ne.0) then
	    read(8,1300) opnf1
	    call endit(xxx,yyy)
	    goto 400
	endif
	goto 340
c
c... read through the downwind dispersion calculations
c
 400	if(.not.check4) goto 500
c
c... for steady-state runs, finish the file
c
 445	continue
	read(8,1300) opnf1
	nchar = index(opnf1,'(m)')
	if(nchar.ne.0) then
	    read(8,1300) opnf1
	    goto 450
	endif
	goto 445
c
c... set cflag to control calls to ADIABAT
c
 450	continue
	cflag = isofl.eq.1 .or. ihtfl.eq.0
	iii = 0
 460	continue
	if(cflag) then
	    read(8,2460,end=800,err=800) dist,yc,cc,rho,temp,bb,sz,sy,
     .			wllc,wulc
	else
	    read(8,2461,end=800,err=800) dist,yc,cc,rho,gamma,
     .			temp,bb,sz,sy,wllc,wulc
	endif
 2460	format(1x,11(1x,g9.3,1x))
 2461	format(1x,3(1x,g9.3,1x),2x,f7.4,2x,1x,g10.3,1x,6(1x,g9.3,1x))
	write(1,9000) dist,yc
	write(2,9000) dist,wllc,wulc
	iii = iii+1
	if(iii.eq.3) then
	    read(8,'(a4)',end=800,err=800) dummy
	    iii = 0
	endif
	goto 460
c
c... process transient outputs
c
 500	continue
	read(8,1300,end=800,err=800) opnf1
	nchar = index(opnf1,'beginning')
	if(nchar.ne.0) then
	    backspace(unit=8)
	    read(8,'(6x,30x,g14.7)',end=800,err=800) time
	    read(8,1300,end=800,err=800) opnf1
	    read(8,1300,end=800,err=800) opnf1
	    read(8,1300,end=800,err=800) opnf1
	    read(8,1300,end=800,err=800) opnf1
	    goto 550
	endif
	goto 500
c
c... set cflag to control calls to ADIABAT
c
 550	continue
	write(1,*) '----------------- For the time: ',time,' s'
	write(2,*) '----------------- For the time: ',time,' s'
	cflag = isofl.eq.1
	iii = 0
 560	continue
	if(cflag) then
	    read(8,2461,end=500,err=500) dist,yc,cc,rho,temp,bb,sz,sy,
     .			wllc,wulc
	else
	    read(8,2461,end=500,err=500) dist,yc,cc,rho,gamma,
     .			temp,bb,sz,sy,wllc,wulc
	endif
	if(yc .ne. 0.D0) then
	    write(1,9000) dist,yc
	    write(2,9000) dist,wllc,wulc
	endif
	iii = iii+1
	if(iii.eq.3) then
	    read(8,'(a4)',end=500,err=500) dummy
	    iii = 0
	endif
	goto 560
c
C
c... close the file
c
 800	CLOSE(UNIT=8)
	close(unit=1)
	close(unit=2)
c
c... another run?
c
	write(6,3001)
 3001	format(//,' Do you want information about another run? [y,n] ',
     .		'<N> ',$)
	read(5,'(a80)') opnf1
	nchar = index(opnf1,' ')
	if(nchar.eq.1) stop
	call upcase(opnf(1))
	nchar = index(opnf1,'Y')
	if(nchar.eq.1) goto 100
C
	STOP
	END
c
c
c
	subroutine endit(xxx,yyy)
c
c......................................................................
c
c	Subroutine to read through and output list and return the last
c	entries in the list from the first two columns of numbers.
c
c......................................................................
c
	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )
c
	character*4 dummy
c
c
	iii = 0
 100	continue
	read(8,*,end=200,err=200) xx,yy
	iii = iii+1
	if(iii.eq.3) then
	    read(8,'(a4)',end=200,err=200) dummy
	    iii = 0
	endif
	xxx = xx
	yyy = yy
	goto 100
c
 200	continue
	return
	end
