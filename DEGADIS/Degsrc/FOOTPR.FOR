ctohere        
	program footpr
c
c...................................................................
c
c       Program to use internally generated grid to output DEGADIS
c       predicted horizontal slice to TECPLOT with logical ijk-type grid.
c
c...................................................................
c
	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	parameter (imax=17)
	parameter (maxk=150)

	include 'DEG1.prm'

	dimension rz(imax)
	dimension zs(imax), ys(imax-2)
	dimension crz(imax)
	dimension zz(imax),xx(imax)
	dimension xpos(maxk,2)
	dimension kmax(maxk)
C
	COMMON
     ./ALP/     ALPHA,alpha1
     ./cgprop/  gasmw,gastem,gasrho,gascpk,gascpp,gasulc,gasllc,gaszzc,
     .          gasnam
     ./comatm/  istab,tamb,pamb,humid,isofl,tsurf,ihtfl,htco,iwtfl,wtco,
     .          humsrc
     ./comss/   ess,slen,swid,outcc,outsz,outb,outl,swcl,swal,senl,srhl
     ./csprd/   ce, delrmn
     ./dwncmn/  jetplu
     ./GEN2/    DEN(5,igen)
     ./ITI/     T1,TINP,TSRC,TOBS,TSRT
     ./NEND/    POUND
     ./oomsin/  oodist,avtime
     ./PARM/    U0,Z0,ZR,rml,USTAR,vkc,gg,RHOE,RHOA,DELTAy,BETAy,GAMMAF,
     .          yclow
     ./phicom/  iphifl,dellay,elejet,diajet
     ./PHLAG/   CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
     ./titl/    title
     ./pict/    ytarget
C
	LOGICAL CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
c
	character*24 tsrc,tinp,tobs,tsrt
C
	character*3 gasnam
	character*80 title(4)
c
	logical jetplu, deg
c
	character*80 opnf1,scratch,jetnam,trannam
	character opnf(80)
C
	EQUIVALENCE (opnf(1),opnf1)

	data rz/-1.15d0,-1.01d0,-0.9995d0,-0.9d0,-0.8d0,-0.6d0,
     .          -0.4d0,-0.2d0,0.0d0,0.2d0,0.4d0,0.6d0,0.8d0,0.9d0,
     .          0.9995d0,1.01d0,1.15d0/
	data zs/0.0d0,0.05d0,0.1D0,0.2d0,0.3d0,0.4d0,0.5d0,0.6d0,0.7d0,
     .          0.8d0,0.85d0,0.9d0,0.95d0,
     .          1.0d0,1.05D0,1.1D0,1.15d0/
	data ys/-1.15d0,-1.01d0,-0.9d0,-0.8d0,-0.6d0,
     .          -0.4d0,-0.2d0,0.0d0,0.2d0,0.4d0,0.6d0,0.8d0,0.9d0,
     .          1.01d0,1.15d0/

	data ln/10/

	DATA POUND/-1.D-20/

	print *,'This program calculates the concentrations in a' 
	print *,'horizontal slice from separate jet/plume and'
	print *,'ground-level DEGADIS simulations.  At present,'
	print *,'application is limited to a plume which does'
	print *,'not return to ground level, and a ground-level'
	print *,'source where no gas blanket is formed.'
	print *,'The lowest concentration of limit must be'
	print *,'consistently specified, and both simulations'
	print *,'must be made for an isothermal ideal gas.'
C
C... GET THE jet/plume FILE NAME FOR FILE CONTROL
C
 100    write(6,1100)
 1100   format(/,' Enter the existing DEGADIS jet/plume simulation',
     .          ' name as a proper file name: ',$)
	read(5,1150) opnf1
	jetnam = opnf1
 1150   format(a40)
	write(6,1155)
 1155   format(/,' Enter the target mole fraction for the picture: ',$)
	read(5,*) ytarget
c
c... set the one parameter which may drop through the cracks if jet only
c
	dellay = 2.15d0  
	
	nchar = index(opnf1,' ') - 1
	opnf1 = opnf1(1:nchar) // '.ino'  
	write(6,1181) (opnf(iii),iii=1,nchar+4)
 1181   format(/,' Attempting to open the INO file: ',40a1)
	open(unit=1, file=opnf1, status='OLD', err=100)
	write(6,*) '...ino file opened'
c
	READ(1,'(a80)') (title(j),J=1,4)
	READ(1,*) u0,z0
	READ(1,*) ZR
	read(1,*) istab, rml, ustar
	read(1,*) tamb, pamb, humid, relhum, tsurf
	read(1,*) avtime, deltay, betay
	read(1,*) deltaz, betaz, gammaz

	read(1,*) gasmw
	read(1,*) gastem
	read(1,*) gasulc, gasllc, gaszzc
	read(1,*) gascpk, gascpp
	read(1,*) nden, nden0
	if(nden0 .gt. 0) then
	    do iii = 1,nden0
		read(1,*) (den(jjj,iii),jjj=1,5)
	    enddo
	    den(1,nden0+1) = 2.D0
	    rhoa = den(3,1)
	    rhoe = den(3,nden0)
	else
	    rhoa = Pamb*(1.D0+humid)*wmw/(rgas*(wmw/wma + humid))/Tamb
	    rhoe = pamb*gasmw/rgas/gastem
	endif
	gasrho = rhoe

	read(1,*) isofl

	read(1,*) erate
	QINIT = erate/rhoe
	read(1,*) elejet, diajet

	READ(1,*) ALFA1, ALFA2
	read(1,*) distmx, tend

	close(unit=1)
	    
	    wc = 1.D0
	    wa = 0.D0
	    enth = cpc(gastem) * (gastem - tamb)
	    call setden(wc, wa, enth)
c
c... check to ensure jet/plume simulation is long enough
c... open and read the LIS file to get some parameters of the run.............
c
	if(ytarget.lt.gasllc) then
	    write(6,*) 'The target mole fraction is less than the',
     .          ' lower limit of concern.'
	    write(6,*) 'target mole fraction: ',ytarget
	    write(6,*) 'lower limit of concern: ',gasllc
	    write(6,*) 'Change the input file and rerun the simulation.'
	    stop
	endif

	opnf1 = opnf1(1:nchar) // '.lis'
	OPEN(UNIT=9, file=opnf1, status='OLD', err=100)
c
c... see which models were run (and set JETPLU and DEG); get XJET AND XDOWN
C
	jetplu = .true.
	read(9,1300) scratch
 1300   format(a80)
	nchr = index(scratch,'JETPLU')
	if(nchr.eq.0) then
	    jetplu=.false.
	    xjet = 0.
	    goto 132
	endif
	
	opnf1 = opnf1(1:nchar) // '.vel'
	open(unit=8, file=opnf1, status='OLD', err=100)                  
c
c... loop through any initial stuff in the JETPLU output; the next few reads
c       guarantee the comment section is bypassed before checking for '(m)'
c
	do 129 iii=1,8
	read(9,1300) opnf1
 129    continue

 130    read(9,1300) opnf1
	nchar = index(opnf1,'(m)')
	if(nchar.ne.0) then
	    read(9,1300) opnf1
	    call endit(xjet,xxx)
	    goto 132
	endif
	goto 130

 132    continue
	read(9,1300,end=135,err=135) opnf1
	read(9,1300,end=135,err=135) opnf1
	deg = .true.
	goto 138

 135    deg = .false.
	goto 200
c
c... loop through the header information from DEGADIS
c
 138    do 139 iii=1,13
	read(9,1300) opnf1
 139    continue

 140    read(9,1300) opnf1
	nchar = index(opnf1,'sec')
	if(nchar.ne.0) then
	    read(9,1300) opnf1
	    call endit(xxx,xrad)
	    goto 142
	endif
	goto 140

 142    xdown = xjet+xrad

 145    if(.not.check4) goto 200
	read(9,1300) opnf1
	nchar = index(opnf1,'(m)')
	if(nchar.ne.0) then
	    read(9,1300) opnf1
	    call endit(xmax,xxx)
	    goto 200
	endif
	goto 145
c
c... now set to process requests for DEGADIS predictions .......................
c
 200    continue
	rewind(unit=9)
c
c... strip the list of widths from the jet output
c
	do iii=1,8
	  read(9,1301) opnf1
 1301     format(a80)
	enddo

	do while(index(opnf1,'(m)').eq.0)
	   read(9,1301) opnf1
	enddo
	read(9,1300) opnf1
c
c... read the plume widths and record to jetnam.jet
c
	opnf1 = jetnam
	nchar = index(opnf1,' ') - 1    
	opnf1 = opnf1(1:nchar) // '.fpr'
	OPEN(UNIT=10, file=opnf1, status='unknown')

	ii = 1
	read(9,1303,iostat=ierr) xxxo,wllco
	do while(ierr.eq.0)
	    read(9,1303,iostat=ierr) xxx,wllc
	    if(ierr.eq.0) then
		if(wllc.ne.0 .or. wllco.ne.0) write(10,*) xxxo,wllco
		xxxo = xxx
		wllco = wllc
	    else
		write(10,*) xxxo,wllco
		ii = -2
	    endif
 1303       format(1x,g9.0,81x,G9.0)
	    ii = ii+1
	    if(ii.eq.3) then
		ii = 0
		read(9,1301,iostat=ierr) opnf1
	    endif
	enddo
	if(ii.ne.-1) write(10,*) xxx,wllc  ! write this one if needed
	close(unit=9)
	close(unit=10)
C
C... GET THE transient FILE NAME FOR FILE CONTROL
C
  113   write(6,1103)
 1103   format(/,' Enter the existing DEGADIS ground-level simulation',
     .          ' name as a proper file name: ',$)
	read(5,1150) opnf1
	trannam = opnf1
c
c... check to ensure this file exists since there are no error handling
c       provisions in STRT2.
c
	nchar = index(opnf1,' ') - 1
	opnf1 = opnf1(1:nchar) // '.tr2'
	open(unit=8, file=opnf1, status='OLD', err=113)
	close(unit=8)
C
c... read the TR2 file to get the thermodynamic information...................
c       Note that this also loads DEN.
c
	CALL strt2(opnf1, hmrte)
c
c... open the list file and the footprint time listing
c
	opnf1 = trannam
	nchar = index(opnf1,' ') - 1
	opnf1 = opnf1(1:nchar) // '.lis'
	open(unit=8, file=opnf1, status='OLD', err=113)

	opnf1 = opnf1(1:nchar) // '.fpr'
	OPEN(UNIT=10, file=opnf1, status='unknown')

c
c... strip the list of widths from the transient output
c
	do iii=1,8
	  read(8,1301) opnf1
	enddo

	do while(index(opnf1,'Minimum Distance').eq.0)
	   read(8,1301) opnf1
	enddo
	read(8,1300) opnf1
	read(8,1300) opnf1
	read(8,1300) opnf1

	itime = 0
	do while(index(opnf1,'Time after beginning').ne.0)
	    read(opnf1,1311) time
 1311       format(35x,g15.0)
	    itime = itime + 1
	    print *,'Processing time: ',time
	    read(8,1300) opnf1       ! read through the header for each time
	    read(8,1300) opnf1
	    read(8,1300) opnf1
	    read(8,1300) opnf1

	    write(scratch,'(''.00'',I1)') itime
	    write(10,'(a4,2x,1pg13.6)') scratch(1:4),time        
	    opnf1 = trannam
	    nchar = index(opnf1,' ') - 1
	    opnf1 = opnf1(1:nchar) // scratch(1:4)
	    open(unit=9, file=opnf1, status='unknown')
	    xxx = 1.
	    ii = 1
	    iprint = 1
	    read(8,1314) xxxo,wllco
	    do while(xxx.ne.0)
		read(8,1314) xxx,wllc
		if(wllco.ne.0 .or. wllc.ne.0) then
		    write(9,*) xxxo,wllco
		    xxxo = xxx
		    wllco = wllc
		elseif(iprint.eq.1) then
		    write(9,*) xxxo,wllco
		    iprint = 0
		endif
 1314           format(1x,g10.0,81x,g9.0)
		ii = ii+1
		if(ii.eq.3) then
		    read(8,1300) opnf1
		    ii = 0
		endif
	    enddo
	    close(unit=9)
	    ierr = 0
	do while(index(opnf1,'Time after beginning').eq.0.and.ierr.eq.0)
	       read(8,1300,iostat=ierr) opnf1   ! read through header
	    enddo
	enddo
	close(unit=8)
	close(unit=10)
c
c... Now we are ready to set up a grid for each time...
c
c
c... open the list file and the footprint time listing
c
	opnf1 = jetnam
	nchar = index(opnf1,' ') - 1
	opnf1 = opnf1(1:nchar) // '.fpr'
	open(unit=8, file=opnf1, status='unknown')

	opnf1 = trannam    
	nchar = index(opnf1,' ') - 1
	opnf1 = opnf1(1:nchar) // '.fpr'
	OPEN(UNIT=10, file=opnf1, status='unknown')

	ntime = itime
	if(ntime.gt.maxk) stop 'increase maxk for ntime'
	do itime = 1,ntime
	    read(10,'(a4,2x,g13.6)') scratch(1:4),time 
	    opnf1 = opnf1(1:nchar) // scratch(1:4)
	    open(unit=9, file=opnf1, status='unknown')
	    ierr = 0
	    kk = 0
	    dowhile(ierr.eq.0)
		kk = kk+1
		read(9,*,iostat=ierr) xpos(kk,1),xpos(kk,2)
	    enddo
	    kk = kk-1
	    
	    ierr = 0
	    do while(ierr.eq.0)
		kk = kk+1
		if(kk .gt. maxk) then
		    stop 'make maxk bigger'
		endif
		read(8,*,iostat=ierr) xpos(kk,1),xpos(kk,2)
	    enddo
	    kk = kk-1
	    rewind(unit=8)
	    rewind(unit=9)
c
c... now sort the combined vectors and smooth the combined profiles
c
	    kmax(itime) = kk
	    do kk=1,kmax(itime)
		kmin = kk
		do kj = kk,kmax(itime)
		    if(xpos(kj,1).lt.xpos(kmin,1)) kmin = kj
		enddo
		x1 = xpos(kk,1)        
		x2 = xpos(kk,2)
		xpos(kk,1) = xpos(kmin,1)
		xpos(kk,2) = xpos(kmin,2)
		xpos(kmin,1) = x1
		xpos(kmin,2) = x2
	    enddo
	    write(9,*) xpos(1,1),xpos(1,2)
	    do kk=2,kmax(itime)-1
		ave = 0.5*(xpos(kk-1,2)+xpos(kk+1,2))
		if(ave.gt.xpos(kk,2)) xpos(kk,2)=
     .                  max(xpos(kk-1,2),xpos(kk+1,2))
		write(9,*) xpos(kk,1),xpos(kk,2)
	    enddo
	    write(9,*) xpos(kmax(itime),1),xpos(kmax(itime),2)
	    close(unit=9)
	enddo
	close(unit=8)
	close(unit=10)

c
c... loop through each file to build the final results
c
	opnf1 = trannam    
	nchar = index(opnf1,' ') - 1
	opnf1 = opnf1(1:nchar) // '.fpr'
	OPEN(UNIT=10, file=opnf1, status='unknown')

	do itime = 1,ntime
	    read(10,'(a4,2x,g13.6)') scratch(1:4),time 
	    print *,'expanding file for: ',time,' s'
	    opnf1 = opnf1(1:nchar) // scratch(1:4)
	    open(unit=9, file=opnf1, status='unknown')
	    do kk=1,kmax(itime)
		read(9,*) xpos(kk,1),xpos(kk,2) ! read sorted distances/widths
	    enddo
	    close(unit=9)
	    open(unit=9, file=opnf1,status='unknown')
c
c... create the header to carry zone title, etc.
c
	    write(9,*) ' variables = "x","y","DEG mole frac"'
	    write(9,*) ' zone T="DEGADIS simulation at ',time,' s",'
	    write(9,*) ' I=',imax-2,', J=',kmax(itime),', F=point' 
	    do kk=1,kmax(itime)  ! write the framework
		do ii=1,imax-2
		   width = ys(ii)*max(xpos(kk,2),10.D0)
		   write(9,*) xpos(kk,1),width,0.D0
		enddo
	    enddo
	enddo
	close(unit=9)
	close(unit=10)
c
c... loop through each file to add the jet/plume contribution
c
	opnf1 = jetnam
	nchar = index(opnf1,' ') -1
	opnf1 = opnf1(1:nchar) // '.vel'
	open(unit=9, file=opnf1, status='old')

	opnf1 = trannam    
	nchar = index(opnf1,' ') - 1
	opnf1 = opnf1(1:nchar) // '.fpr'
	OPEN(UNIT=10, file=opnf1, status='unknown')

	opnf1 = opnf1(1:nchar) // '.lis'
	open(unit=12, file=opnf1, status='unknown')

	do itime = 1,ntime
	    read(10,'(a4,2x,g13.6)') scratch(1:4),time 
	    print *,'jet/plume contribution for: ',time,' s'
	    opnf1 = opnf1(1:nchar) // scratch(1:4)
	    open(unit=8, file=opnf1, status='unknown')
	    open(unit=11, file='scratch', status='unknown')
	    ierr = 0
	    dowhile(ierr.eq.0)
		read(8,1300,iostat=ierr) scratch
		write(11,1300) scratch
	    enddo
	    rewind(unit=8)
	    rewind(unit=9)
	    rewind(unit=11)

	    read(11,1300) scratch
	    write(8,1300) scratch
	    read(11,1300) scratch       
	    write(8,1300) scratch
	    read(11,1300) scratch       
	    write(8,1300) scratch
	    icallj = -1
	    icalld = -1
	    do kk=1,kmax(itime)  ! read the framework
		do ii=1,imax-2
		   read(11,*) xpos(kk,1),xx(ii),zz(ii)
		enddo
		call jetpnt(icallj,xpos(kk,1),gaszzc,imax-2, xx,zz)
		if(icalld.ne.4)
     .          call dwnpnt(icalld,time,xpos(kk,1),gaszzc,imax-2, xx,zz)
		do ii=1,imax-2
		   write(8,*) xpos(kk,1),xx(ii),zz(ii)
		enddo
	    enddo
	    close(unit=11)
	    close(unit=8)
	enddo
	close(unit=10)
	stop 'ctohere'  
	
c
c... At present, only a single call to jetpnt is implemented
c
c        rewind(unit=9)  ! LIS file

	kk = 0

c        if(x.le.xjet .and. jetplu) then
c        if(jetplu) call jetpnt(imax, rz,crz,zz,xx,kk)
c        else if(x.lt.xdown .and. deg .and. check4) then
c            call srcpnt(x-xjet, y, z, xjet, yy)
c        else if(x.le.xmax .and. deg .and. check4) then
c        if(deg) call dwnpnt(imax, zs,ys, crz,zz,kk)
c        endif

c        if(x.gt.xjet .and. jetplu .and. .not.deg) write(6,2990)
c 2990   format(/,' This point cannot be calculated using DEG4S; ',
c     .          'the JETPLU calculations were',/,
c     .          ' not made for low enough concentrations.  Rerun ',
c     .          'JETPLUIN with a lower value',/,
c     .          ' for the lower limit of concern.')

c        if(x.gt.xmax .and. deg) write(6,2995)
c 2995   format(/,' The steady-state simulation was not calculated ',
c     .          'to these distances.',/,
c     .          ' Rerun JETPLUIN with a lower value',/,
c     .          ' for the lower limit of concern.')
c
	stop
	end



	subroutine endit(xxx,yyy)
c
c......................................................................
c
c       Subroutine to read through and output list and return the last
c       entries in the list from the first two columns of numbers.
c
c......................................................................
c
	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )
c
	character*4 dummy
c
c
	iii = 0
 100    continue
	read(9,*,end=200,err=200) xx,yy
	iii = iii+1
	if(iii.eq.3) then
	    read(9,'(a4)',end=200,err=200) dummy
	    iii = 0
	endif
	xxx = xx
	yyy = yy
	goto 100
c
 200    continue
	return
	end
c
c.............................................................
c
	subroutine jetpnt(icall,xx,zz,imax,yy,cccc)
c
c.............................................................
c
c       Subroutine to output the concentrations from the JETPLU output.
c
c.............................................................
c
	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )
c
	common          ! NB: rhoa and rhoc are local variables!
     ./cgprop/  gasmw,gastem,gasrho,gascpk,gascpp,gasulc,gasllc,gaszzc,
     .          gasnam
     ./phicom/  iphifl,dellay,elejet,diajet    
     ./pict/    ytarget    

	dimension cccc(1:imax)
	dimension yy(1:imax)
	character*80 opnf1,string
	character opnf(80)
C
	EQUIVALENCE (opnf(1),opnf1)
C
	character*3 gasnam
c
c... loop through any initial stuff in the JETPLU output; the next few reads
c       guarantee the comment section is bypassed before checking for '(m)'
c
c        write(6,*) 'Entering jetpnt, icall: ',icall

c
c... if icall=-1, read through header
c
c        if(icall.eq.-1) then
c        do 129 iii=1,8
c        read(9,1300) opnf1
 1300   format(a80)
c 129    continue

c 130    read(9,1300) opnf1
c        nchar = index(opnf1,'(m)')
c        if(nchar.ne.0) then
c            read(9,1300) opnf1
c            goto 140
c        endif
c        goto 130
c        endif
c
c... initialize the search for this spot ..................
c
 140    continue
	if(icall.eq.-1) then
	   read(9,*,iostat=ierr) xjo,zjo,cco,syo,szo,uco,uao,thetao
	       xpro = xjo + tan(thetao)*(zjo-zz)
c               print*,xjo,zjo,cco,syo,szo,uco,uao,thetao
	   read(9,*,iostat=ierr) xjn,zjn,ccn,syn,szn,ucn,uan,thetan 
	       xprn = xjn + tan(thetan)*(zjn-zz)  
	   icall = 2
	endif

	do while(ierr.eq.0)
	    if(xx.ge.xpro .and. xx.le.xprn) then
		frac = (xx-xpro)/(xprn-xpro)
		xj = xjo + frac*(xjn-xjo)
		zj = zjo + frac*(zjn-zjo)
		cc = cco + frac*(ccn-cco)
		sy = syo + frac*(syn-syo)
		sz = szo + frac*(szn-szo)
		uc = uco + frac*(ucn-uco)
		ua = uao + frac*(uan-uao)
		theta = atan((xx-xj)/(zj-zz))
c
c... before determining the concentration at the desired point, take
c       the "image" contribution out of CC read from the output listing.
c
		cc = cc/(1.D0 + exp(-0.5D0*(2.D0*zj/sz)**2))
c
c... contribution from the "real" jet/plume
c
		rr = sqrt((xx-xj)**2 + (zz-zj)**2)
		argz  = 0.5D0*(rr/sz)**2
		rr = sqrt((xx-xj)**2 + (zz+zj)**2)
		argzr = 0.5D0*(rr/sz)**2
		cz = 0.D0
		if(argz .le. 2.31125D0) cz = cc*exp(-argz)
		if(argzr .le.2.31125D0) cz = cz + cc*exp(-argzr)
c
c... loop through all lateral values
c
		do jlat=1,imax
		    arg = 0.5D0*(yy(jlat)/sy)**2
		    if(arg .le. 2.31125D0) then
			cdos = cz * exp(-arg)
		    else
			cdos = 0.
		    endif
		    call adiabat(0,wc,wa,yy1,ya,cdos,r,w,t,tt)
		    cccc(jlat) = cccc(jlat) + yy1
		enddo
		return
	    else                ! move the "new" to the "old" and read again
		xjo = xjn
		zjo = zjn
		cco = ccn
		syo = syn
		szo = szn
		uco = ucn
		uao = uan
		thetao = thetan
		xpro = xprn
		read(9,*,iostat=ierr) xjn,zjn,ccn,syn,szn,ucn,uan,thetan 
		xprn = xjn + tan(thetan)*(zjn-zz)  
		icall = icall+1
	   endif

	   if(icall.eq.3) then
		read(9,'(a80)',iostat=ierr) string
		icall = 0
	   endif
	enddo

	return
	end
c
c
c
	subroutine srcpnt(xdos,ydos,zdos, xjet, yy)
c
c.............................................................
c
c       Subroutine to output the concentration at a given position
c       from the source output.
c
c.............................................................
c
	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )
c
	character*80 opnf1
	character opnf(80)
C
	EQUIVALENCE (opnf(1),opnf1)

	character*4 dummy
c
c
	yy   = ydos
	ydos = zdos
	zdos = yy
c
c... loop through any initial stuff in the listing; the next few reads
c       guarantee the comment section is bypassed before checking for '(m)'
c
	do 130 iii=1,8
	read(9,1300) opnf1
 1300   format(a80)
 130    continue

 140    read(9,1300) opnf1
	nchar = index(opnf1,'sec')
	if(nchar.ne.0) then
	    read(9,1300) opnf1
	    goto 150
	endif
	goto 140

 150    continue
c
c... read through the source to find the pertinent info....................
c
	ymin = 2.
	ymax = 0.
	iii = 0
 100    continue
	read(9,*,end=1000,err=200) tt,rr,hh,qstar,sz,yc
	iii = iii+1
	if(iii.eq.3) then
	    read(9,'(a4)') dummy
	    iii = 0
	endif
c
c... record the min and max mole fractions at this location
c
	if(hh.ge.zdos .and. rr.ge.sqrt(xdos**2 + ydos**2)) then
	    ymax = max(ymax, yc)
	    ymin = min(ymin, yc)
	endif
	goto 100
c
c... An error on the read means the source listing is complete.
c       report the results
c
 200    continue
	if(ymin .eq. 2.) ymin = 0.
	write(6,990) xdos+xjet,ydos,zdos, ymax, ymin
 990    format(//,' From the source calculation...',
     .          //' For the position: x= ',
     .          1pg12.5,' m, y= ',1pg12.5,
     .          ' m, z= ',1pg12.5,' m',/,
     .          ' The maximum mole fraction when the cloud was ',
     .          'at this position was: ',1pg12.5,/,
     .          ' The minimum mole fraction when the cloud was ',
     .                  'at this position was: ',1pg12.5)
	aa   = ydos
	ydos = zdos
	zdos = aa
	return
c
 1000   continue
	write(6,*) 'SRCPNT? Premature end-of-file.'
	stop
	end
c
c
c
c        subroutine dwnpnt(xdos,ydos,zdos, yy)
c
c
	subroutine dwnpnt(icall,time,xx,zz,imax,yy,cccc)
c
c
c.............................................................
c
c       Subroutine to output the concentration at a given position
c       from the downwind output.
c
c.............................................................
c
	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )
c
	include 'DEG1.prm'
C
	COMMON
     ./ALP/     ALPHA,alpha1
     ./cgprop/  gasmw,gastem,gasrho,gascpk,gascpp,gasulc,gasllc,gaszzc,
     .          gasnam
     ./comatm/  istab,tamb,pamb,humid,isofl,tsurf,ihtfl,htco,iwtfl,wtco,
     .          humsrc
     ./comss/   ess,slen,swid,outcc,outsz,outb,outl,swcl,swal,senl,srhl
     ./dwncmn/  jetplu
     ./GEN2/    DEN(5,igen)
     ./PARM/    U0,Z0,ZR,rml,USTAR,vkc,gg,RHOE,RHOA,DELTAy,BETAy,GAMMAF,
     .          yclow
     ./phicom/  iphifl,dellay,elejet,diajet
     ./pict/    ytarget
C
	dimension yy(1:imax)
	dimension cccc(1:imax)
c
	character*3 gasnam
c
	character*80 string
C
	character*4 dummy
c
	logical cflag, jetplu
c
c... set cflag to control calls to ADIABAT
c
	cflag = isofl.eq.1 .or. ihtfl.eq.0
c
c... loop through any initial stuff in the listing; the next few reads
c       guarantee the comment section is bypassed before checking for '(m)'
c
c        if(jetplu) then
c            do 100 iii=1,13
c 100        read(9,1300) string
 1300       format(a80)
c
c 110        read(9,1300) string
c            nchar = index(string,'(m)')
c            if(nchar.ne.0) goto 120
c            goto 110
c        endif
	if(icall.eq.-1) then
	    ftime = 0.D0
	    dowhile(ftime.ne.time)
		dowhile(index(string,'Time after beginning').eq.0)
		    read(12,1300) string
		enddo
		read(string,'(35x,g15.0)') ftime
	    enddo
c            print *,'dwnpnt time: ',ftime
	    read(12,1300) string
	    read(12,1300) string
	    read(12,1300) string
	    read(12,1300) string
	if(cflag) then
	    read(12,*,iostat=ierr) disto,yco,cco,rhoo,tempo,bbo,szo,syo
	else
	    read(12,*,iostat=ierr) disto,yco,cco,rhoo,gammao,
     .                                  tempo,bbo,szo,syo
	endif
	if(cflag) then
	    read(12,*,iostat=ierr) distn,ycn,ccn,rhon,tempn,bbn,szn,syn
	else
	    read(12,*,iostat=ierr) distn,ycn,ccn,rhon,gamman,
     .                                  tempn,bbn,szn,syn
	endif
	    icall = 2
	endif
c
c... process the output
c
	ierr = 0
	dowhile(ierr.eq.0)
	    if(xx.ge.disto .and. xx.le.distn) then  ! found the slot
		frac = (xx-disto)/(distn-disto)
		dist = xx
		yc = yco + frac*(ycn-yco)
		cc = cco + frac*(ccn-cco)
		rho= rhoo+ frac*(rhon-rhoo)
		temp=tempo+frac*(tempn-tempo)
		bb = bbo + frac*(bbn-bbo)
		sz = szo + frac*(szn-szo)
		sy = syo + frac*(syn-syo)
		gamma=gammao+frac*(gamman-gammao)
c
		argz = (zz/sz)**alpha1
		cz = cc*exp(-argz)
c
c... now calculate lateral values
c
		do jlat=1,imax
		    argy = 0.D0
		    if(abs(yy(jlat)).gt.bb) then
			if(sy.ne.0.D0) then
			    argy = ((abs(yy(jlat))-bb)/sy)**2
			    if(argy .lt. 80.) then
				cdos = cz*exp(-argy)
				if(cflag) then
		    call adiabat(0,wc,wa,yc,ya,cdos,r,w,t,tt)
				else
		    call adiabat(-1,wc,wa,yc,ya,cdos,r,w,gamma,tt)
				endif
			     else
				yc = 0.  ! argy>80 so contribution is small
			     endif
			else
			    yc = 0.D0   ! sy is zero
			endif
		    else                ! then yy<bb
			cdos = cz
			if(cflag) then
			  call adiabat(0,wc,wa,yc,ya,cdos,r,w,t,tt)
			else
			  call adiabat(-1,wc,wa,yc,ya,cdos,r,w,gamma,tt)
			endif
		    endif
		    cccc(jlat) = cccc(jlat) + yc

		enddo
		return
	    else
		disto = distn
		yco   = ycn
		cco   = ccn
		rhoo  = rhon
		tempo = tempn
		bbo   = bbn
		szo   = szn
		syo   = syn
		gammao= gamman

		if(cflag) then
	    read(12,*,iostat=ierr) distn,ycn,ccn,rhon,tempn,bbn,szn,syn
		else
	    read(12,*,iostat=ierr) distn,ycn,ccn,rhon,gamman,
     .                                  tempn,bbn,szn,syn
		endif
		icall = icall+1
		if(icall.eq.3) then
		    read(12,'(a4)') dummy
		    icall = 0
		endif
	    endif
	enddo
	icall = 4
	return
	end 
