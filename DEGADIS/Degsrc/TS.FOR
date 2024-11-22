	FUNCTION TS(T0l,DIST)
c
C..............................................................
C
C	FUNCTION TO CALCULATE A SPECIFIED TIME for a specific
c	observer at a specified location.
C
C..............................................................
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	COMMON
     ./PARMSC/	RM,SZM,EMAX,RMAX,TSC1,ALEPH,TEND
     ./ALP/	ALPHA,alpha1
C
	TS = T0l + (DIST+RMAX)**(1.D0/ALPHA1) /ALEPH
C
	RETURN
	END
