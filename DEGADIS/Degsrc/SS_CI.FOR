c ??? flags spots to keep in mind
	program ss_ci
c
c..............................................................
c
c       Program to automate the field test comparison process.
c
c       SS_CI.FOR estimates the confidence intervals on the ratio
c	of DEGADIS-predicted concentrations to observed concentrations
c	included in SS_EXP.TAB.
c
c..............................................................
c
c
c... nres = nratio*(nratio+1)/2
c
	parameter (nratio=150, nres=11325)
c
	character*132 string1, dummy1
	character*1 string(132), dummy(132)
	EQUIVALENCE (STRING(1),string1)
	EQUIVALENCE (dummy(1),dummy1)

	character*10 run

	logical thisone, anyone

	dimension ratio(nratio)
	dimension res(nres)
	dimension ci(3), alp(3), ppp(3), xp(3)
c
	data ltab/10/, ldat/11/, linp/12/, ltec/14/

	data ci/0.90, 0.95, 0.99/
	data alp/0.1, 0.05, 0.01/	! 1.-ci
	data ppp/0.05, 0.025, 0.005/	! alp/2.
	data xp/-1.6449, -1.960, -2.5758/	! P(W<=xp) = p
c
c
	write(6,*) 'SS_CI...'

	open(unit=linp, file='ss_ci.inp', status='old')
	open(unit=ltab, file='ss_exp.tab', status='old')
	open(unit=ldat, file='ss_ci.dat', status='unknown')
	open(unit=ltec, file='ss_ci.tec', status='unknown')	!???
c
c... write the header information to SS_CI.DAT and SS_CI.TEC
c
	write(ldat,1020)
 1020	format(8x,'SS_CI.DAT summary...',/)
	write(ldat,1200)
	write(ldat,1220)
 1200	format(' -------------------------------------------------',
     .                  '--------------------------')
 1220	format(/,
     .		' Cases considered:',/,' --------------------------')
	write(ltec,*) 'TITLE = "Comparison of DEGADIS-predicted ',
     .			'and observed concentrations"'
	write(ltec,*) 'VARIABLES = obsmf premf obs_pre pre_obs xdist'
	write(ltec,*) 'ZONE'
c
c... Major loop start ... read SS_CI.INP for the cases to be processed.
c
	npts = 0
  50	continue
	read(linp,1100,end=9000) string1
	if(index(string1(1:1),'!').eq.1) goto 50	! bypass commented lines
	iii = index(string1,' ')
	run = string1(1:iii)
	anyone = .false.
	rewind(unit=ltab)
c
c... inner loop to find the results in SS_EXP.TAB
c
   90	continue
	read(ltab,1100,end=1000) string1
	read(ltab,1100,end=1000) string1
  100	continue
	thisone = .false.
	read(ltab,1100,end=1000) string1
 1100	format(a132)
 1101	format(132a1)
 1102	format(1x,a132)
	iii = index(string1,';') - 1
	dummy1 = string1(2:iii)
	iii = iii - 1
	if(index(dummy1(1:iii),run(1:iii)).eq.1 .and.
     .		index(run(1:iii),dummy1(1:iii)).eq.1) then
	    thisone = .true.
	    anyone  = .true.
	    jjj = len_trim(string1)
	    write(ldat,1101) (string(kkk),kkk=1,jjj)
	    write(6,*) 'Processing: ',string1(2:jjj)
	endif
c
c... read past next 5 lines
c
	read(ltab,1100) string1
	read(ltab,1100) string1
	read(ltab,1100) string1
	read(ltab,1100) string1
	read(ltab,1100) string1
c
c... read the numbers from SS_EXP.TAB
c
  200	continue
	read(ltab,1100) string1
	if(index(string1,'-') .eq. 2) goto 900
	read(string1(2:12),2000) xdist
 2000	format(g11.0)
	ydist_ = 0
	read(string1(13:23),2000, err=220) ydist
	goto 240
 220	continue
	    ydist_ = 1
	    ydist = 0.D0
 240	continue
	read(string1(24:34),2000) zdist
	read(string1(35:45),2000) obsmf
	read(string1(46:56),2000) premf
	read(string1(57:67),2000) rat_o_p
	read(string1(68:79),2000) rat_p_o
c
c... do these values get included in the CI search?
c
	if(thisone) then
	    npts = npts + 1
	    ratio(npts) = rat_o_p
	    write(ltec,'(5(1x,1pg13.5))') obsmf,premf,rat_o_p,
     .						rat_p_o, xdist
	    write(6,*) 'Using this one: ', xdist
	endif
c
c... get the next location for which a prediction is required
c
	goto 200
c
c... skip to the next line in TAB file
c
  900	continue
	read(ltab,1100,end=1000) string1
	goto 100
c
c... The TAB file has been read.  (1) Has the desired run been found?
c	(2) get the next run name to be processed.
c
 1000	continue
	if(.not.anyone) stop 'No SS_EXP.TAB entry found for this run.'
	goto 50
c
c... The entries in SS_CI.INP have been exhausted.  It is now time to do
c	the calculations.
c
 9000	continue
c???????????????????????????????????????

	write(ldat,1200)
	write(ldat,*) ' '
	write(ldat,*) 'Number of points considered: ',npts
	if(npts .gt. nratio) stop 'too many points'
	nrespt = ((npts+1)*npts)/2

	do 9120 i=1,npts
	do 9100 j=1,i
	iii = ((i-1)*i)/2 + j
	res(iii) = (ratio(i) + ratio(j))/2.
 9100	continue
 9120	continue

	write(6,*) 'Entering QCKSRT'
	call qcksrt(nrespt,res)
c
c... estimate ntest for each CI
c
	do 9140 iii=1,3
	wp = npts*(npts+1)
	wp = wp/4. + xp(iii)*sqrt( wp*(2.*npts+1.)/24.)		! for npts>50
	if(npts .lt. 50) then
	    write(6,*) 'For n=',npts,
     .		' enter the value of wp from Table A13 for p=',ppp(iii)
	    read(5,*) wp
	endif
	ntest = nint(wp)
	write(ldat,10001) ci(iii)*100.
10001	format(//,' For the ',f4.1,' % confidence interval, ')
	write(ldat,10000)  ntest, res(ntest), nrespt-ntest+1,
     .		res(nrespt-ntest+1)
10000	format(/,'    The ',i6,'th entry is: ',1pg13.5,
     .			' (lower limit to the mean Obs/Pre ratio)',
     .		/,'    The ',i6,'th entry is: ',1pg13.5,
     .			' (upper limit to the mean Obs/Pre ratio)')
	write(ldat,10002)  1./res(nrespt-ntest+1),1./res(ntest)
10002	format(/,'    The lower limit to the mean Pre/Obs ratio is ',
     .			1pg13.5,
     .		/,'    The upper limit to the mean Pre/Obs ratio is ',
     .			1pg13.5)
 9140	continue

	sumr = 0
	sum = 0 	

	do 9150 iii=1,npts
	sum = sum + ratio(iii)
	sumr = sumr + 1./ratio(iii)
 9150	continue

	av = sum/float(npts)
	dev = 0.

	do 175 iii=1,npts
	dev = dev + (ratio(iii)-av)**2
 175	continue

	var = dev/float(npts-1)
	stddev = sqrt(var)

	write(ldat,*) ' '
	write(ldat,*) '     av(Obs/Pre): ',av
	write(ldat,*) ' stddev(Obs/Pre): ',stddev

	write(ldat,*) ' '
	write(ldat,*) 'Min/Max Ratio (Obs/Pre): ',Ratio(1), ratio(npts)
c
c... now compute the statistical properties of the reciprocal ratio
c
	av = sumr/sum*av
	dev = 0.

	do 9200 iii=1,npts
	dev = dev + (1./ratio(iii)-av)**2
 9200	continue

	var = dev/float(npts-1)
	stddev = sqrt(var)

	write(ldat,*) ' '
	write(ldat,*) '     av(Pre/Obs): ',av
	write(ldat,*) ' stddev(Pre/Obs): ',stddev

	close(unit=ldat)
	close(unit=ltab)
c
	stop
	end
c
c
c
      SUBROUTINE QCKSRT(N,ARR)
      PARAMETER (M=7,NSTACK=50,FM=7875.,FA=211.,FC=1663.
     *    ,FMI=1.2698413E-4)
      DIMENSION ARR(N),ISTACK(NSTACK)
      JSTACK=0
      L=1
      IR=N
      FX=0.
10    IF(IR-L.LT.M)THEN
        DO 13 J=L+1,IR
          A=ARR(J)
          DO 11 I=J-1,1,-1
            IF(ARR(I).LE.A)GO TO 12
            ARR(I+1)=ARR(I)
11        CONTINUE
          I=0
12        ARR(I+1)=A
13      CONTINUE
        IF(JSTACK.EQ.0)RETURN
        IR=ISTACK(JSTACK)
        L=ISTACK(JSTACK-1)
        JSTACK=JSTACK-2
      ELSE
        I=L
        J=IR
        FX=MOD(FX*FA+FC,FM)
        IQ=L+(IR-L+1)*(FX*FMI)
        A=ARR(IQ)
        ARR(IQ)=ARR(L)
20      CONTINUE
21        IF(J.GT.0)THEN
            IF(A.LT.ARR(J))THEN
              J=J-1
              GO TO 21
            ENDIF
          ENDIF
          IF(J.LE.I)THEN
            ARR(I)=A
            GO TO 30
          ENDIF
          ARR(I)=ARR(J)
          I=I+1
22        IF(I.LE.N)THEN
            IF(A.GT.ARR(I))THEN
              I=I+1
              GO TO 22
            ENDIF
          ENDIF
          IF(J.LE.I)THEN
            ARR(J)=A
            I=J
            GO TO 30
          ENDIF
          ARR(J)=ARR(I)
          J=J-1
        GO TO 20
30      JSTACK=JSTACK+2
        IF(JSTACK.GT.NSTACK)PAUSE 'NSTACK must be made larger.'
        IF(IR-I.GE.I-L)THEN
          ISTACK(JSTACK)=IR
          ISTACK(JSTACK-1)=I+1
          IR=I-1
        ELSE
          ISTACK(JSTACK)=I-1
          ISTACK(JSTACK-1)=L
          L=I+1
        ENDIF
      ENDIF
      GO TO 10
      END
