	SUBROUTINE GETT
c
C..............................................................
C
C	SUBROUTINE TO ESTABLISH THE TIME SORT PARAMETERS for DEG4
C
c.............................................................
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	include 'DEG4.prm'
C
	COMMON
     ./ALP/	ALPHA,alpha1
     ./cdos/	idos, dosx(ndos), dosyz(4,2,ndos)
     ./CNOBS/	NOBS
     ./ERROR/	ERT1,ERDT,ERNTIM
     ./oomsin/	oodist,avtime
     ./PARMSC/	RM,SZM,EMAX,RMAX,TSC1,ALEPH,TEND
     ./PHLAG/	CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
     ./SORTIN/	TIM(maxnt),NTIM,ISTRT
     ./SSCON/	NREC(maxnob,2),T0(maxnob),XV(maxnob)
C
	LOGICAL CHECK1,CHECK2,CHECK3,CHECK4,CHECK5,AGAIN
C
	DATA T1/0./,DT/0./,TF/0./
C
C... This subroutine sets the time sort windows for concentration as
C     a function of time at a given position.
C
	dist = dosx(idos)
C
	T1 = ts( T0(1), dist-oodist)	! time first obs. crosses dist-oodist
	TF = ts( T0(nobs), dist-oodist)	! time last observer crosses dist-oodist
	NTIM = maxnt
C
	DT = (TF-T1)/FLOAT(NTIM-1)
	DT = dmax1( DBLE(NINT(DT)), 1.D0)
	NTIM = min( INT((TF - T1)/DT) + 1, maxnt)
C
	T1 = FLOAT(nINT(T1))	!MAKE T1 AN INTEGER VALUE
	write(lunlog,1000) t1, tf, dt, dist
 1000	format('  t1: ',1pg13.5,'  tf: ',1pg13.5,'  dt: ',1pg13.5,
     .' dist:  ',1pg13.5)
C
	ntim = max( ntim,4)	! fill in the lowest four times at least
	DO 100 I = 1,ntim
	TIM(I) = DT*FLOAT(I-1) + T1
  100	CONTINUE
C
	RETURN
	END
