	SUBROUTINE GETT
c
C..............................................................
C
C	SUBROUTINE TO ESTABLISH THE TIME SORT PARAMETERS
C
c..............................................................
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

c
	include 'DEG3.prm'
C
	COMMON
     ./ALP/	ALPHA,alpha1
     ./CNOBS/	NOBS
     ./ERROR/	ERT1,ERDT,ERNTIM
     ./PARMSC/	RM,SZM,EMAX,RMAX,TSC1,ALEPH,TEND
     ./PHLAG/	CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
     ./SORTIN/	TIM(maxnt),NTIM,ISTRT
     ./SSCON/	NREC(maxnob,2),T0(maxnob),XV(maxnob)
C
	LOGICAL CHECK1,CHECK2,CHECK3,CHECK4,CHECK5,AGAIN
C
	DATA T1/0.D0/, DT/0.D0/, TF/0.D0/
c
C
C... IF CHECK5 IS SET, GET THE TIME SORT PARAMETERS FROM /ERROR/.............
C
	IF(.NOT.CHECK5) GO TO 90
C
	T1 = ERT1
	DT = ERDT
	NTIM = INT(ERNTIM)
	GO TO 95
C
C
C... This subroutine sets the default time sort windows if so desired..........
C
C... The first sort time is set for potential low wind speed cases,
C	while the last sort time is set for potential high wind speed
C	cases.  The first sort time is taken to be when the first
C	observer passes through x=RMAX.  The last sort time is taken
C	to be when the last observer passes through x=6*RMAX.
C	The default value for the number of sort times is set to 10.
C	Obviously, these values generate some sort times which will be
C	useless; hopefully, these values will show the user where to
C	look on the next go around.
c
C	The number of times has been doubled to 20 and the time interval
C	has been doubled in order to give more information for lower
C	concentrations (for the toxic gas problem). tos,5mar86
C
  90	CONTINUE
C
	T1 = T0(1) + (2.D0*RMAX)**(1.D0/ALPHA1)/ALEPH
	TF = T0(NOBS) + (6.D0*RMAX)**(1.D0/ALPHA1)/ALEPH
	NTIM = 20		! for toxic gas problem
C
c DT = (TF-T1)/FLOAT(NTIM-1)
	DT = 2.D0*(TF-T1)/FLOAT(NTIM-1)	! for toxic gas problem
	DT = FLOAT(NINT(DT))
C
	IF(DT .GE. 5.) GO TO 95
	DT = 5.
	NTIM = INT((TF - T1)/DT) + 1
C
   95	CONTINUE
C
c... fill in the vectors of times
c
	T1 = FLOAT(nINT(T1))	! MAKE T1 AN INTEGER VALUE
C
	ntim = max( ntim,4)	! fill in the lowest four times at least

	DO 100 I = 1,ntim
	TIM(I) = DT*FLOAT(I-1) + T1
  100	CONTINUE
C
	RETURN
	END
