C..........................................................
C
C	FUNCTIONS ASSOCIATED WITH THE OBSERVER CALCULATIONS
C
C..........................................................
C
	FUNCTION UIT(T,T0l)
c
c.................................................................
c
C	FUNCTION TO RETURN OBSERVER VELOCITY AS A FUNCTION OF TIME
C
C.................................................................
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	COMMON
     ./PARMSC/	RM,SZM,EMAX,RMAX,TSC1,ALEPH,TEND
     ./ALP/	ALPHA,alpha1
C
	UIT = ALPHA1 * ALEPH**ALPHA1 *(T-T0l)**ALPHA
C
	RETURN
	END
c
c
c
	FUNCTION XIT(Tl,T0l)
c
C...............................................................
C
C	FUNCTION TO RETURN POSITION AS A FUNCTION OF TIME AND T0
C
C...............................................................
C

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	COMMON
     ./PARMSC/	RM,SZM,EMAX,RMAX,TSC1,ALEPH,TEND
     ./ALP/	ALPHA,alpha1
C
	xit = -rmax
	arg = tl-t0l
	if(arg .le. 0.D0) return
	XIT = (ALEPH*(Tl - T0l))**ALPHA1 - RMAX
C
	RETURN
	END
c
c
c
	FUNCTION T0OB(X,T)
c
c................................................................
C
C	FUNC TO RETURN A VALUE OF T0 BASED ON A POSITION AND TIME
c
C.................................................................
C

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	COMMON
     ./PARMSC/	RM,SZM,EMAX,RMAX,TSC1,ALEPH,TEND
     ./ALP/	ALPHA,alpha1
C
	ARG = 0.D0
	CHECK = ABS((ABS(X)-ABS(RMAX)))/(ABS(X)+ABS(RMAX))
	IF(CHECK .GT. 0.001D0) ARG = (X + RMAX)**(1.D0/ALPHA1)/ALEPH
	T0OB = T - ARG
	RETURN
	END
