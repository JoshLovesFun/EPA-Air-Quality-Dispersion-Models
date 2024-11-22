	program deg_fox
c
c...................................................................
c
c       Program to use internally generated grid to output DEGADIS
c       predictions to TECPLOT with logical ijk-type grid.
c
c...................................................................
c
	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	parameter (imax=17)

	include 'DEG1.prm'

	dimension rz(imax)
	dimension zs(imax), ys(imax-2)
	dimension crz(imax)
	dimension zz(imax),xx(imax)
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
     ./pict/    ytarget,kmax
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
	character*80 opnf1,scratch
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
C
C... GET THE FILE NAME FOR FILE CONTROL
C
 100    write(6,1100)
 1100   format(/,' Enter the existing DEGADIS simulation name as a',
     .          ' proper file name: ',$)
	read(5,1150) opnf1
 1150   format(a40)
	write(6,1155)
 1155   format(/,' Enter the target mole fraction for the picture: ',$)
	read(5,*) ytarget
	write(6,1160)
 1160   format(/,' Enter the maximum number of slices ',
     .          '(100 suggested): ',$)
	read(5,*) kmax
c
c... setthe one parameter which may drop through the cracks if jet only
c
	dellay = 2.15d0  
c
c... check to ensure this file exists since there are no error handling
c       provisions in STRT2SS.
c
	nchar = index(opnf1,' ') - 1
	opnf1 = opnf1(1:nchar) // '.tr2'
	open(unit=8, file=opnf1, status='OLD', err=110)
	close(unit=8)
C
c... read the TR2 file to get the thermodynamic information...................
c       Note that this also loads DEN.
c
	CALL strt2(opnf1, hmrte, ccp)
	if(.not.check4) then
	    write(6,1200)
 1200       format(//,' This is a transient simulation, and ',
     .          'DEG4S can only be used to output',/,
     .          ' concentrations in the jet portion of the ',
     .          'calculations.',//,
     .          ' To get concentration time histories for the ',
     .          '"nonjet" portion of this',/,
     .          ' transient run, use DEG4.')
	else
	    goto 122
	endif

 110    continue
	write(6,1180) (opnf(iii),iii=1,nchar+4)
 1180   format(/,' The specified TR2 file could not be opened: ',40a1)
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
	    do 112 iii = 1,nden0
	    read(1,*) (den(jjj,iii),jjj=1,5)
  112       continue
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
c
c... check for ss/transient
c
	check4 = .false.
	if(tend.le.0.D0) then
	    check4 = .true.
	    wc = 1.D0
	    wa = 0.D0
	    enth = cpc(gastem) * (gastem - tamb)
	    call setden(wc, wa, enth)
	    goto 122
	else
	    write(6,*) 'This is a transient simulation, but there'
     .          ,'are not yet any'
	    write(6,*) '    provisions for tecplot files as a',
     .          ' function of time.'
	    write(6,*) 'A single output file will be generated',
     .          ' reflecting the maximum jet/plume extent.'
	    check4 = .true.
	    wc = 1.D0
	    wa = 0.D0
	    enth = cpc(gastem) * (gastem - tamb)
	    call setden(wc, wa, enth)
	    goto 122
	endif
	goto 100
c
c... check to ensure jet/plume simulation is long enough
c... open and read the LIS file to get some parameters of the run.............
c
 122    continue
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
	opnf1 = opnf1(1:nchar) // '.tec'
	open(unit=ln, file=opnf1, status='UNKNOWN')                  
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
c
c... At present, only a single call to jetpnt is implemented
c
c... create the zone title, etc. in FILE.TEC
c
	write(6,*) 'creating the file.tec file'
c
	rewind(unit=9)  ! LIS file

	kk = 0

c        if(x.le.xjet .and. jetplu) then
	if(jetplu) call jetpnt(imax, rz,crz,zz,xx,kk)
c        else if(x.lt.xdown .and. deg .and. check4) then
c            call srcpnt(x-xjet, y, z, xjet, yy)
c        else if(x.le.xmax .and. deg .and. check4) then
	if(deg) call dwnpnt(imax, zs,ys, crz,zz,kk)
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
	close(unit=ln)
c
c... create the header to carry zone title, etc.
c
	open(unit=ln, file='tecplot.hd', status='UNKNOWN')
	write(ln,*) ' variables = "x","y","z","DEG mole frac"'
	write(ln,*) ' zone T="DEGADIS simulation", I=',imax,', J=',
     .         imax,', K=',kk,',' 
	write(ln,*) '       F=point' 
	write(6,*) 'append file.tec to tecplot.hd before preploting'
  
	close(unit=ln)
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
	subroutine jetpnt(imax,rz,crz,zz,xx,kk)
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
     ./pict/    ytarget,kmax    

	dimension rz(1:imax),crz(1:imax)
	dimension zz(1:imax),xx(1:imax)
	character*80 opnf1
	character opnf(80)
C
	EQUIVALENCE (opnf(1),opnf1)
C
	character*3 gasnam
c
c... loop through any initial stuff in the JETPLU output; the next few reads
c       guarantee the comment section is bypassed before checking for '(m)'
c
d        write(6,*) 'Entering jetpnt'

	kk = 0
	ratio = exp(log(ytarget)/real(10*kmax))
	ccold = 1.D0
	ilast = 0

	do 129 iii=1,8
	read(9,1300) opnf1
 1300   format(a80)
 129    continue

 130    read(9,1300) opnf1
	nchar = index(opnf1,'(m)')
	if(nchar.ne.0) then
	    read(9,1300) opnf1
	    goto 140
	endif
	goto 130
c
c... initialize the search for this xpos,zpos ..................
c
 140    continue
	do jj=1,imax
	   crz(jj) = 0.
	enddo
	if(ilast.eq.1) return            ! out of points to read
	read(8,*,iostat=ierr) xj,zj,cc,sy,sz,uc,ua,theta
	if(ierr .ne. 0) then
	    if(iskip .eq. 0) return
	    ilast = 1
	    backspace(unit=8)
	    read(8,*,iostat=ierr) xj,zj,cc,sy,sz,uc,ua,theta
	    goto 150
	endif
	if(ccold*ratio.lt.cc .and. kk.ne.0) then
	    write(6,*) 'skipping xj: ',xj
	    iskip = 1
	    goto 140        ! pass this one up
	endif
	iskip = 0

 150    continue
	ccold = cc
	kk = kk+1  
c
c... before determining the concentration at the desired point, take
c       the "image" contribution out of CC read from the output listing.
c
	cc = cc/(1.D0 + exp(-0.5D0*(2.D0*zj/sz)**2))
c
c... contribution from the "real" jet/plume
c
	    zzz = zj+rz(1)*dellay*sz*cos(theta)
	    scale = 1.D0
	    if(zzz.lt.0.D0) scale = -zj/(rz(1)*dellay*sz*cos(theta))
	do jj=1,imax
	    if(jj.gt. imax/2) scale = 1.D0
	    xx(jj) = xj - scale * rz(jj)*dellay*sz*sin(theta)
	    zz(jj) = zj + scale * rz(jj)*dellay*sz*cos(theta)
	    rr = sqrt((xx(jj)-xj)**2 + (zz(jj)-zj)**2)
	argz  = 0.5D0*(rr/sz)**2
	    rr = sqrt((xx(jj)-xj)**2 + (zz(jj)+zj)**2)
	argzr = 0.5D0*(rr/sz)**2
	if(argz .le. 2.31125D0) then
	    crz(jj) = cc*exp(-argz)
	else
	    crz(jj) = 0.D0
	endif
	if(argzr .le. 2.31125D0) crz(jj) = crz(jj) + cc*exp(-argzr)
	enddo
c
c... loop through all lateral values
c
	do jvert = 1,imax
	do jlat=1,imax
	    yyyy = rz(jlat)*dellay*sy
	arg = 0.5D0*(rz(jlat)*dellay)**2
	if(arg .le. 2.31125D0) then
	    cdos = crz(jvert)*exp(-arg)
	else
	    cdos = 0.
	endif
	call adiabat(0,wc,wa,yy1,ya,cdos,r,w,t,tt)
c
c... report the results
c
	write(10,1000) xx(jvert),-yyyy,zz(jvert),yy1
 1000      format(1x,7(1x,1pg13.6))
	  enddo
	enddo
	goto 140

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
	subroutine dwnpnt(imax,zs,ys,crz,zz,kk)
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
     ./pict/    ytarget, kmax
C
	dimension zs(1:imax), ys(1:imax-2), crz(1:imax)
	dimension zz(1:imax)
c
	character*3 gasnam
c
	character*80 opnf1
	character opnf(80)
C
	EQUIVALENCE (opnf(1),opnf1)

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
c 100        read(9,1300) opnf1
 1300       format(a80)
c
c 110        read(9,1300) opnf1
c            nchar = index(opnf1,'(m)')
c            if(nchar.ne.0) goto 120
c            goto 110
c        endif

 120    do 130 iii=1,13
 130    read(9,1300) opnf1

 140    read(9,1300) opnf1
	nchar = index(opnf1,'(m)')
	if(nchar.ne.0) then
	    read(9,1300) opnf1
	    goto 150
	endif
	goto 140
c
c... process the output
c
 150    continue

	iii = 0
 160    continue
	
	do jj=1,imax            ! zero "crz"
	    crz(jj) = 0.d0
	enddo

	if(cflag) then
	    read(9,*,end=1000,err=1000) dist,yc,cc,rho,temp,bb,sz,sy
	else
	    read(9,*,end=1000,err=1000) dist,yc,cc,rho,gamma,
     .                                  temp,bb,sz,sy
	endif

	kk = kk+1       ! increment kk
	iii = iii+1
	if(iii.eq.3) then
	    read(9,'(a4)') dummy
	    iii = 0
	endif
c
c... calculate a scale which will ensure that "ytarget" is in the picture
c       "factor" is a multiple of either Sy or Sz
c
	qq = max(log(cc*rgas*tamb/ytarget/pamb/gasmw), 1.D0)
	factor = max(qq**(1.D0/alpha1), sqrt(qq))
c
c... loop through the vertical center line locations and calculate
c       concentrations
c
	do jj=1,imax
	    zz(jj) = factor*zs(jj)*dellay*sz
	    argz = (zz(jj)/sz)**alpha1
	    crz(jj) = cc*exp(-argz)
	enddo
c
c... now calculate lateral values
c
	do jvert=1,imax
	    do jlat=1,(imax-2)/2
	    if(sy.ne.0.D0) then
		ydos = factor*ys(jlat)*dellay*sy
		argy = (ydos/sy)**2
	    else
		argy = 0.D0
	    endif
	    if(argy .lt. 80.) then
		cdos = crz(jvert)*exp(-argy)
		if(cflag) then
		    call adiabat(0,wc,wa,yy,ya,cdos,r,w,t,tt)
		else
		    call adiabat(-1,wc,wa,yy,ya,cdos,r,w,gamma,tt)
		endif
	    else
		yy = 0.
	    endif
	    write(10,1200) dist,-ydos+bb,zz(jvert), yy
	    enddo
 1200       format(1x,7(1x,1pg13.6))
c
c... now for the middle section
c
		cdos = crz(jvert)
		if(cflag) then
		    call adiabat(0,wc,wa,yy,ya,cdos,r,w,t,tt)
		else
		    call adiabat(-1,wc,wa,yy,ya,cdos,r,w,gamma,tt)
		endif
	    if(bb.eq.0.D0) then
		write(10,1200) dist,bb,zz(jvert), yy
		write(10,1200) dist,bb,zz(jvert), yy
		write(10,1200) dist,bb,zz(jvert), yy
	    else
		write(10,1200) dist,+bb,zz(jvert), yy
		write(10,1200) dist,0.D0,zz(jvert), yy
		write(10,1200) dist,-bb,zz(jvert), yy
	    endif
c
c... finish the other half
c
	    do jlat=(imax+1)/2,imax-2
	    if(sy.ne.0.D0) then
		ydos = factor*ys(jlat)*dellay*sy
		argy = (ydos/sy)**2
	    else
		argy = 0.D0
	    endif
	    if(argy .lt. 80.) then
		cdos = crz(jvert)*exp(-argy)
		if(cflag) then
		    call adiabat(0,wc,wa,yy,ya,cdos,r,w,t,tt)
		else
		    call adiabat(-1,wc,wa,yy,ya,cdos,r,w,gamma,tt)
		endif
	    else
		yy = 0.
	    endif
	    write(10,1200) dist,-(ydos+bb),zz(jvert), yy
	    enddo
	enddo
	goto160
c
c... report the results
c
c       write(6,990) xdos,ydos,zdos, yy
c 990   format(//,' From the downwind, ground-level dispersion ',
c     .         'calculation...',//' The mole fraction at x= ',
c     .         1pg12.5,' m, y= ',1pg12.5,
c     .         ' m, z= ',1pg12.5,' m is:',/,5x,1pg12.5)
c
c
c... this should be the end-of-file.
c
 1000   continue
	return
	end 
