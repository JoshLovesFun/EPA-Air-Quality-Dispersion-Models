	SUBROUTINE trap(N,N1)
c
C..............................................................
C
C	SUBROUTINE TRAP -- DIAGNOSTICS to help with problem diagnosis
c
C..............................................................
C

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	include 'DEG2.prm'
c
	COMMON /ITI/T1,TINP,TSRC,TOBS,TSRT

	real*4 tt1
c
	character*24 TINP,TSRC,TOBS,TSRT
c
	character*24 tt
	character*80 dd
C
	WRITE(lunlog,1100)
	WRITE(lunlog,1110)
	write(lunlog,1115) n
c
c... check to see if the user is ready to read the text of the error
c	message
c
	write(lunlog,1000)
	read(5,1210,err=10) dd
 10	write(lunlog,1002)
 1200	format(' More -- <CR> ',$)
 1210	format(a80)
C
	IF(N .EQ. 1 ) then
			WRITE(lunlog,2010) N1
			write(lunlog,1200)
			read(lunlog,1210, err=11) dd
   11			WRITE(lunlog,2011)
	else IF(N .EQ. 2 ) then
			WRITE(lunlog,2020) N1
	else IF(N .EQ. 3 ) then
			WRITE(lunlog,2030) N1
			write(lunlog,1200)
			read(lunlog,1210, err=31) dd
   31			WRITE(lunlog,2031)
	else IF(N .EQ. 4 ) then
			WRITE(lunlog,2040)
	else IF(N .EQ. 5 ) then
			WRITE(lunlog,2050)
	else IF(N .EQ. 6 ) then
			WRITE(lunlog,2060) n1
	else IF(N .EQ. 7 ) then
			WRITE(lunlog,2070) n1
	else IF(N .EQ. 8 ) then
			WRITE(lunlog,2080) N1
			write(lunlog,1200)
			read(lunlog,1210, err=81) dd
   81			WRITE(lunlog,2081)
	else IF(N .EQ. 9 ) then
			WRITE(lunlog,2090) N1
			write(lunlog,1200)
			read(lunlog,1210, err=91) dd
   91			WRITE(lunlog,2091)
	else IF(N .EQ. 10) then
			WRITE(lunlog,2100) N1
			write(lunlog,1200)
			read(lunlog,1210, err=101) dd
  101			WRITE(lunlog,2101)
	else IF(N .EQ. 11) then
			WRITE(lunlog,2110)
	else IF(N .EQ. 12) then
			WRITE(lunlog,2120)
	else IF(N .EQ. 13) then
			WRITE(lunlog,2130)
	else IF(N .EQ. 14) then
			WRITE(lunlog,2140)
	else IF(N .EQ. 15) then
			WRITE(lunlog,2150)
	else IF(N .EQ. 16) then
			WRITE(lunlog,2160)
	else IF(N .EQ. 17) then
			WRITE(lunlog,2170)
	else IF(N .EQ. 18) then
			WRITE(lunlog,2180) N1
			write(lunlog,1200)
			read(lunlog,1210, err=181) dd
  181			WRITE(lunlog,2181)
	else IF(N .EQ. 19) then
			WRITE(lunlog,2190) N1
	else IF(N .EQ. 20) then
			WRITE(lunlog,2200)
	else IF(N .EQ. 21) then
			WRITE(lunlog,2210)
	else IF(N .EQ. 22) then
			WRITE(lunlog,2220)
	else IF(N .EQ. 23) then
			WRITE(lunlog,2230) maxnob
			write(lunlog,1200)
			read(lunlog,1210, err=231) dd
  231			WRITE(lunlog,2231)
	else IF(N .EQ. 24) then
			WRITE(lunlog,2240)
	else IF(N .EQ. 25) then
			WRITE(lunlog,2250)
	else IF(N .EQ. 26) then
			WRITE(lunlog,2260)
	else IF(N .EQ. 27) then
			WRITE(lunlog,2270)
	else IF(N .EQ. 28) then
			WRITE(lunlog,2280)
	else IF(N .EQ. 29) then
			WRITE(lunlog,2290)
	else IF(N .EQ. 30) then
			WRITE(lunlog,2300)
	else IF(N .EQ. 31) then
			WRITE(lunlog,2310)
	else IF(N .EQ. 32) then
			WRITE(lunlog,2320)
	else IF(N .EQ. 33) then
			WRITE(lunlog,2330)
	endif
C
 1000	format(/,5x,'Ready for the text of the error message? <CR> ',$)
 1002	format(/)
 1100	FORMAT(5X,'The best laid plans of mice and men...')
 1110	FORMAT(5X,'You have entered a TRAP -- the land of no RETURN.')
 1115	format(' Code: ',i4)

 2010	FORMAT(5X,'DEG1? Source integration has returned IHLF=',I3,/,
     ./,'     This error occurs during integration of the equations',/,
     .' which describe the gas source.  IHLF is an error code',/,
     .' returned by the integration package RKGST.',//,
     .'     When IHLF=11, more than 10 bisections of the initial',/,
     .' increment of the independent variable were necessary to make',/,
     .' an integration step within the specified error.  Reduce the',/,
     .' initial step size of the independent variable',/,
     .' (STPIN in the ER1 file).  If this does not work,',/,
     .' it will be necessary to either increase the error criteria',/,
     .' for all of the dependent variables being integrated',/,
     .' (ERBND in the ER1 file) or increase the error criteria',/,
     .' for the variable violating the criteria by decreasing the',/,
     .' error weight for that variable (one of the following: WTRG,',/,
     .' WTTM, WTYA, WTYC, WTEB, WTMB, or WTUH in the ER1 file).',/)
 2011	format(
     .'     When IHLF=12, the initial increment of the independent',/,
     .' variable (STPIN) is 0.  Correct the ER1 file, and execute the',/,
     .' program again.',//,
     .'     When IHLF=13, the initial increment of the independent',/,
     .' variable (STPIN) is not the same sign as the difference',/,
     .' between the upper bound of the interval and the lower bound',/,
     .' of the interval.  STPIN must be positive.  Correct the ER1',/,
     .' file, and execute the program again.',//)

 2020	FORMAT(5X,'ALPH? ZBRENT has failed in JETPLU IERR: ',I4,//)

 2030	format(5x,'SZF? Local integration failed; IHLF=',I3,/,
     ./,'     This error occurs during estimation of SZ over the',/,
     .' source when no gas is present.  IHLF is an error code',/,
     .' returned by the integration package RKGST.',//,
     .'     When IHLF=11, more than 10 bisections of the initial',/,
     .' increment of the independent variable were necessary to make',/,
     .' an integration step within the specified error.  Reduce the',/,
     .' initial step size of the independent variable',/,
     .' (SZSTP0 in the ER1 file).  If this does not work,',/,
     .' increase the error criteria for all of the dependent',/,
     .' variables being integrated (SZERR in the ER1 file).',//,
     .'     When IHLF=12, the initial increment of the independent',/,
     .' variable (SZSTP0) is 0.  Correct the ER1 file, and execute the',/,
     .' program again.',/)
 2031	format(
     .'     When IHLF=13, the initial increment of the independent',/,
     .' variable (SZSTP0) is not the same sign as the difference',/,
     .' between the upper bound of the interval and the lower bound',/,
     .' of the interval.  SZSTP0 must be positive.  Correct the ER1',/,
     .' file, and execute the program again.',//)

 2040	format(5x,'Reserved')

 2050	FORMAT(5X,'CRFG? MORE POINTS FOR GEN3 WERE NEEDED',//,
     .'     The COMMON area /GEN3/ stores representative values',/,
     .' of the calculated source parameters.  If this message',/,
     .' occurs, relax the CRFG error criteria (CRFGER) in the',/,
     .' ER1 file.  If this is a common problem, the length of the',/,
     .' /GEN3/ vectors can be increased by changing the value of',/,
     .' MAXL in DEG1.PRM and recompiling and relinking DEGADIS.',//)

 2060	FORMAT(5X,'TUPF? OBSERVER CALCULATION - ZBRENT FAILED:',I2,//,
     .'     The trial and error search associated with finding the',/,
     .' upwind edge of the gas source for an observer failed.',/,
     .' Often, this problem can be avoided by adding one or two',/,
     .' additional observers to the present number of observers',/,
     .' (which changes the conditions for the trial and error).',/,
     .' Another possibility is to increase the error criteria for',/,
     .' this function (ERTUPF) in the ER2 file.',//)

 2070	FORMAT(5X,'TUPF? OBSERVER CALCULATION - ZBRENT FAILED:',i2,//,
     .'     The trial and error search associated with finding the',/,
     .' downwind edge of the gas source for an observer failed.',/,
     .' Often, this problem can be avoided by adding one or two',/,
     .' additional observers to the present number of observers',/,
     .' (which changes the conditions for the trial and error).',/,
     .' Another possibility is to increase the error criteria for',/,
     .' this function (ERTDNF) in the ER2 file.',//)

 2080	FORMAT(5X,'SSSUP? OBSERVER INTEGRATION FAILED, IHLF=',I3,//,
     .'     This error occurs during integration of the five',/,
     .' differential equations which average the source for each',/,
     .' observer.  IHLF is an error code returned by the',/,
     .' integration package RKGST.',//,
     .'     When IHLF=11, more than 10 bisections of the initial',/,
     .' increment of the independent variable were necessary to make',/,
     .' an integration step within the specified error.  Reduce the',/,
     .' initial step size of the independent variable',/,
     .' (STPO in the ER2 file).  If this does not work,',/,
     .' it will be necessary to either increase the error criteria',/,
     .' for all of the dependent variables being integrated',/,
     .' (ERRO in the ER2 file) or increase the error criteria',/,
     .' for the variable violating the criteria by decreasing the',/,
     .' error weight for that variable (one of the following: WTAIO,',/,
     .' WTQOO, or WTSZO in the ER2 file).',/)
 2081	format(
     .'     When IHLF=12, the initial increment of the independent',/,
     .' variable (STPO) is 0.  Correct the ER2 file, and execute the',/,
     .' program again.',//,
     .'     When IHLF=13, the initial increment of the independent',/,
     .' variable (STPO) is not the same sign as the difference',/,
     .' between the upper bound of the interval and the lower bound',/,
     .' of the interval.  STPO must be positive.  Correct the ER2',/,
     .' file, and execute the program again.',//)

 2090	FORMAT(5X,'SSSUP/DEG2S? PSEUDO-STEADY INTEG FAILED, IHLF=',I3,
     .//,'     This error occurs during integration of the four',/,
     .' differential equations describing the portion of the',/,
     .' downwind calculation when b>0.  The routine calling TRAP is',/,
     .' SSSUP if a transient simulation is being executed; if a',/,
     .' steady-state simulation is being executed, the calling',/,
     .' routine is DEG2S.  IHLF is an error code returned by the',/,
     .' integration package RKGST.',//,
     .'     When IHLF=11, more than 10 bisections of the initial',/,
     .' increment of the independent variable were necessary to make',/,
     .' an integration step within the specified error.  Reduce the',/,
     .' initial step size of the independent variable',/,
     .' (STPP in the ER2 file).  If this does not work,',/,
     .' it will be necessary to either increase the error criteria',/,
     .' for all of the dependent variables being integrated',/,
     .' (ERRP in the ER2 file) or increase the error criteria',/,
     .' for the variable violating the criteria by decreasing the',/,
     .' error weight for that variable (one of the following: WTSZP,',/,
     .' WTSYP, WTBEP, or WTDH in the ER2 file).',/)
 2091	format(
     .'     When IHLF=12, the initial increment of the independent',/,
     .' variable (STPP) is 0.  Correct the ER2 file, and execute the',/,
     .' program again.',//,
     .'     When IHLF=13, the initial increment of the independent',/,
     .' variable (STPP) is not the same sign as the difference',/,
     .' between the upper bound of the interval and the lower bound',/,
     .' of the interval.  STPP must be positive.  Correct the ER2',/,
     .' file, and execute the program again.',//)

 2100	FORMAT(5X,'SSSUP/DEG2S? GAUSSIAN INTEGRATION FAIL, IHLF=',I3,
     .//,'     This error occurs during integration of the',/,
     .' differential equations describing the portion of the',/,
     .' downwind calculation when b=0.  The routine calling TRAP is',/,
     .' SSSUP if a transient simulation is being executed; if a',/,
     .' steady-state simulation is being executed, the calling',/,
     .' routine is DEG2S.  IHLF is an error code returned by the',/,
     .' integration package RKGST.',//,
     .'     When IHLF=11, more than 10 bisections of the initial',/,
     .' increment of the independent variable were necessary to make',/,
     .' an integration step within the specified error.  Reduce the',/,
     .' initial step size of the independent variable',/,
     .' (STPG in the ER2 file).  If this does not work,',/,
     .' it will be necessary to either increase the error criteria',/,
     .' for all of the dependent variables being integrated',/,
     .' (ERRG in the ER2 file) or increase the error criteria',/,
     .' for the variable violating the criteria by decreasing the',/,
     .' error weight for that variable (either WTRUH or WTDHG',/,
     .' in the ER2 file).',//)
 2101	format(
     .'     When IHLF=12, the initial increment of the independent',/,
     .' variable (STPG) is 0.  Correct the ER2 file, and execute the',/,
     .' program again.',//,
     .'     When IHLF=13, the initial increment of the independent',/,
     .' variable (STPG) is not the same sign as the difference',/,
     .' between the upper bound of the interval and the lower bound',/,
     .' of the interval.  STPG must be positive.  Correct the ER2',/,
     .' file, and execute the program again.',//)

 2110	FORMAT(5X,'SSSUP/DEG2S? TOTAL No. OF RECORDS EXCEED 120000',
     .//,'     This is an arbitrary stopping point for the process',/,
     .' in order to keep a runaway simulation from filling up disk',/,
     .' space.  Relax the output specifications (ODLP, ODLLP, ODLG,',/,
     .' or ODLLG) in the ER2 file in order to generate less output',/,
     .' if the input parameters are valid.',//)

 2120	FORMAT(5X,'Reserved')
 2130	FORMAT(5X,'Reserved')
 2140	FORMAT(5X,'Reserved')
 2150	FORMAT(5X,'Reserved')
 2160	FORMAT(5X,'Reserved')

 2170	format(5x,'TPROP/ADDHEAT? ZBRENT failure',//,
     .'     ZBRENT failed because the temperature was not bounded',/,
     .' properly.',/,
     .' Check the input conditions, and execute the program again.',//)

 2180	FORMAT(5X,'ALPH? ALPHA INTEGRATION FAILED, IHLF=',I3,//,
     .'     The integration which determines the integral least',/,
     .' squares fit for ALPHA has failed.  Note that small values',/,
     .' of the Monin-Obukhov length ( RML < O(1m) ) in combination',/,
     .' with stable atmospheric conditions may cause this failure.',/,
     .' IHLF is an error code returned by the integration package',/,
     .' RKGST.',//,
     .'     When IHLF=11, more than 10 bisections of the initial',/,
     .' increment of the independent variable were necessary to make',/,
     .' an integration step within the specified error.  Reduce the',/,
     .' absolute value of the initial step size of the independent',/,
     .' variable (STPINZ in the ER1 file).  If this does not work,',/,
     .' it will be necessary to increase the error criteria',/,
     .' (ERBNDZ in the ER1 file).',//)
 2181	format(
     .'     When IHLF=12, the initial increment of the independent',/,
     .' variable (STPINZ) is 0. Correct the ER1 file, and execute',/,
     .' the program again.',//,
     .'     When IHLF=13, the initial increment of the independent',/,
     .' variable (STPINZ) is not the same sign as the difference',/,
     .' between the upper bound of the interval and the lower bound',/,
     .' of the interval.  STPINZ must be negative.  Correct the ER1',/,
     .' file, and execute the program again.  This error will also',/,
     .' occur if the surface roughness ZR is greater than the',/,
     .' reference height Z0.',//)

 2190	FORMAT(5X,'ALPH? ZBRENT has failed to locate ALPHA IERR: ',I4,//,
     .'     The search procedure which determines ALPHA has failed.',/,
     .' This error may be the result of an unusual velocity',/,
     .' specification such as small values of the Monin-Obukhov',/,
     .' length ( ML < O(1.m) ) or small reference heights',/,
     .' ( Z0 < O(10. * ML) ).  IERR is an error code returned by',/,
     .' the routine ZBRENT.',//,
     .'     When IERR=1, the search for ALPHA failed after a',/,
     .' specified number of iterations. Increase the error bound',/,
     .' used by ZBRENT (EPS in ER1 file).',//,
     .'     When IERR=2, the basic assumption that the function which',/,
     .' governs the search for ALPHA changes sign over the specified',/,
     .' interval is false.  Increase the search interval by',/,
     .' decreasing the lower bound of ALPHA (XLI in the ER1 file)',/,
     .' and increasing the upper bound (XRI in the ER1 file).',//)

 2200	format(5x,'ESTRT? Premature EOF in RUNNAME.ER1 or RUNNAME.ER2.'
     .,//,'       The portion of the program which reads ER1 and',/,
     .' ER2 files encountered an end-of-file mark before all of',/,
     .' the information had been read.  Confirm these files, and',/,
     .' execute the program again.  If necessary, copy and edit the',/,
     .' appropriate EXAMPLE file, and execute the program again.',//)

 2210	FORMAT(5X,'ESTRT1/ESTRT2/ESTRT2SS/ESTRT3? DECODE failed',//,
     .'     The portion of the program which reads the ER1, ER2,',/,
     .' or the ER3 file failed to understand a numerical entry.',/,
     .' The numbers must appear in columns 11-20 of the line with',/,
     .' no alphabetic characters in the field.  (Note that',/,
     .' exponential notation is not allowed.)  This restriction',/,
     .' does not apply to comment lines which have an exclamation',/,
     .' point (!) in the first column.',//)

 2220	format(5x,'ESTRT1? The parameter file RUNNAME.ER1 ',
     .'was not found.',//,
     .'     The ER1 file was not found for the current simulation',/,
     .' (RUNNAME).  Copy the file EXAMPLE.ER1 file to RUNNAME.ER1',/,
     .' and edit it as necessary.  Execute the program again.',//)

 2230	format(5x,'SORTS1? Fewer than 3 points sorted for any time.',//,
     .'     Only one or two simulation points were applicable for',/,
     .' the sort times specified.  There are four possible causes',/,
     .' for this condition:',//,
     .'     (1) If this message appears when the sort times are',/,
     .' defaulted (CHECK5 is set to 0. in the ER3 file), the',/,
     .' number of observers will probably have to be increased',/,
     .' to give a good resolution of the downwind concentration',/,
     .' field.  (The number of observers is NOBS in the ER2 file',/,
     .' with a maximum of ',I3,' (MAXNOB) in DEG2.PRM.)',/,
     .' As a rule of thumb, one gets good resolution of the downwind',/,
     .' concentration field if the ratio',/,
     .' (secondary source duration / number of observers) is less',/,
     .' than about 10 seconds (or 20 at most).',//,
     .'     (2) The sort times specified in the ER3 file were',/,
     .' before the simulation had developed significantly.',/,
     .' This is only applicable when the user is specifying the sort',/,
     .' times (i.e. when CHECK5 is set to 1. in the ER3 file).',/,
     .' Increase the time of the first sort (ERT1), and rerun the',/,
     .' program.',/)
 2231	format(
     .'     (3) The sort times specified in the ER3 file were',/,
     .' after the gas was below the lowest concentration of interest',/,
     .' This is only applicable when the user is specifying the sort',/,
     .' times (i.e. when CHECK5 is set to 1. in the ER3 file).',/,
     .' Decrease the time of the first sort (ERT1), and rerun the',/,
     .' program.  If additional results are desired for later',/,
     .' times, restart the simulation and specify a lower',/,
     .' concentration of interest in the input step (lower',/,
     .' YCLOW in DEGINP or GASLLC in JETPLUIN).',//,
     .'     (4) The distance specified in DEG4 was too large for the',/,
     .' lowest concentration of interest specified.  Restart the',/,
     .' simulation and lower the lowest concentration of interest',/,
     .' (by lowering YCLOW in DEGINP or GASLLC in JETPLUIN).',//)

 2240	format(5x,'TPROP? Trial and error loop compromised',//,
     .'     TPROP estimates the temperature of a mixture based',/,
     .' upon the composition and enthalpy of the mixture.  Ensure',/,
     .' the properties for the diffusing species are entered',/,
     .' correctly and execute the simulation again.',//)

 2250	format(5x,'TPROP? Isothermal density loop compromised',//,
     .'     This error should never occur, but if it does, rebuild',/,
     .' the model from the original files and run the simulation',/,
     .' over.',//)

 2260	format(5x,'TPROP? Invalid entry flag in ADIABAT',//,
     .'     This is a programming diagnostic and should never occur.',/,
     .' If it does, rebuild the model from the original files.',//)

 2270	format(5x,'Reserved')

 2280	format(5x,'TPROP? IGEN request too large in SETDEN',//,
     .'     The subroutine SETDEN (in TPROP) performs a series of',/,
     .' adiabatic mixing calculations with a specified gas mixture',/,
     .' and ambient air and places the result in the array',/,
     .' DEN(5,IGEN).  This error indicates more points are needed in',/,
     .' DEN than were originally specified.  Increase the allocation',/,
     .' for DEN by changing the value of IGEN in DEGINP.PRM and',/,
     .' recompiling and relinking DEGADIS.',//)

 2290	format(5x,'PHIF? flag IPHIFL is out of bounds',//,
     .'     Proper values of IPHIFL are integers between 1 and 5 ',/,
     .' inclusive.  Although values of IPHIFL are entered in the ER1',/,
     .' file as real numbers, they should be in this range.  Check',/,
     .' the ER1 file, and execute the program again.',//)

 2300	format(5x,'SSSUP/DEG2S? concentration greater than RHOE',//,
     .'     If the concentration of the contaminant becomes',/,
     .' greater than the pure component density for an isothermal',/,
     .' simulation, this error will occur.  However, this situation',/,
     .' should never occur.  Check the input conditions, and execute',/,
     .' the program again.',//)

 2310	format(5x,'SSSUP? concentration greater than RHOE',//,
     .'     If the concentration of the contaminant becomes',/,
     .' greater than the pure component density for an isothermal',/,
     .' simulation, this error will occur.  However, this situation',/,
     .' should never occur.  Check the input conditions, and execute',/,
     .' the program again.',//)

 2320	format(5x,'PSS? Sz convergence failure.',//,
     .'     This is a programming diagnostic and should never occur.',/,
     .' If it does, check the input conditions, and execute the',/,
     .' program again.',//)

 2330	format(5x,'SSG? Sz convergence failure.',//,
     .'     This is a programming diagnostic and should never occur.',/,
     .' If it does, check the input conditions, and execute the',/,
     .' program again.',//)
C
	CLOSE(UNIT=9)
C
C
	write(lunlog,1200)
	read(lunlog,1210, err=2999) dd
c
 2999	call dattim(tt)
	tt1 = t1
	ttime = secnds(tt1)/60.
C
	WRITE(lunlog,3000) TT
	WRITE(lunlog,3010) Ttime
 3000	FORMAT(1X,' -- ENDing AT ',A24)
 3010	FORMAT(5X,' ***** ELAPSED TIME ***** ',1pg13.5,' MIN ',//,
     .		1x,80('*'),/,1x,80('*'),//,1x,15('TRAP-'),'TRAP',//,
     .		1x,'Note that programs ',
     .		'which follow for this RUNNAME will produce ',
     .		'unexpected errors.',//,1x,15('TRAP-'),'TRAP',//,
     .		1x,80('*'),/,1x,80('*'),//)
C

	stop 'TRAP routine premature exit'
	END
