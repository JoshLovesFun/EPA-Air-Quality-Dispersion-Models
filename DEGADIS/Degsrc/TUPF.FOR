C..............................................................
C
C	OBSERVER TRIAL AND ERROR FUNCTIONS
C	  --- TUPF ---- TDNF ---
c...............................................................
C
	FUNCTION TUPF
c
c...............................................................
c
c	Returns the time this particular observer crosses the upwind
c	edge of the secondary source.
c
c...............................................................
C
	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	include 'DEG2.prm'
c
	COMMON
     ./ALP/	ALPHA,alpha1
     ./cedge/	T0L, direc
     ./ERROR/	SY0ER,ERRO,SZ0ER,WTAIO,WTQOO,WTSZO,ERRP,SMXP,WTSZP,
     .		WTSYP,WTBEP,WTDH,ERRG,SMXG,ERTDNF,ERTUPF,WTRUH,WTDHG
     ./PARMSC/	RM,SZM,EMAX,RMAX,TSC1,ALEPH,TEND

	external edge
c
C
	TMAX = RMAX**(1.D0/ALPHA1)/ALEPH + T0L
	TMIN = T0L
	IF(T0L .LT. 0.) TMIN = 0.
c
c... TMIN and TMAX represent the first and last time this observer could
c	encounter the upwind edge of the source.  TMAX is the time when the
c	observer passes over x=0, and TMIN is the time the observer is
c	released (unless set to zero because the spill has not yet begun).
c	Now, refine the guess of TMIN and TMAX by calling LIMIT
c
	DT = (TMAX - TMIN) / 20.D0
	TL = TMIN
	direc = -1.D0
	call limit(edge, tl, dt, tmax, tmin)
c
c... Now, use ZBRENT to get desired convergence between new TMAX and TMIN.
c
	call zbrent(tupf, edge, tmin, tmax, ertupf, ierr)
	if(ierr .ne. 0) CALL trap(6,ierr)
C
	RETURN
	END
c
c
c
	FUNCTION TDNF
c
c...............................................................
c
c	Returns the time this particular observer crosses the downwind
c	edge of the secondary source.
c
c...............................................................
C

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	include 'DEG2.prm'
c
	COMMON
     ./ALP/	ALPHA,alpha1
     ./cedge/	T0L, direc
     ./ERROR/	SY0ER,ERRO,SZ0ER,WTAIO,WTQOO,WTSZO,ERRP,SMXP,WTSZP,
     .		WTSYP,WTBEP,WTDH,ERRG,SMXG,ERTDNF,ERTUPF,WTRUH,WTDHG
     ./PARMSC/	RM,SZM,EMAX,RMAX,TSC1,ALEPH,TEND

	external edge
c
C
	TMIN = RMAX**(1.D0/ALPHA1)/ALEPH + T0L
	IF(TMIN .LT. 0.) TMIN = 0.
	TMAX = (2.D0*RMAX)**(1.D0/ALPHA1)/ALEPH + T0L
c
c... TMIN and TMAX represent the first and last time this observer could
c	encounter the downwind edge of the source.  TMAX is the time when the
c	observer passes over x=+RMAX, and TMIN is the time the observer
c	passes over x=0 (unless set to zero because the spill has not begun
c	yet).  Now, refine the guess of TMIN and TMAX by calling LIMIT.
c
	DT = (TMAX - TMIN) / 20.D0
	TL = TMAX
	direc = +1.D0
	call limit(edge, tl, dt, tmax, tmin)
c
c... Now, use ZBRENT to get desired convergence between new TMAX and TMIN.
c
	call zbrent(tdnf, edge, tmin, tmax, ertdnf, ierr)
	if(ierr .ne. 0) CALL trap(7,ierr)
c
	RETURN
	END
c
c
c
	function edge(time)
c
c....................................................................
c
c	Function to calculate the difference between the observer position
c	and the source radius.
c
c.....................................................................
c
	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	include 'DEG2.prm'

	common
     ./cedge/	T0L, direc
     ./GEN3/	radg(2,maxl),qstr(2,maxl),srcden(2,maxl),srcwc(2,maxl),
     .		srcwa(2,maxl),srcen(2,maxl)

	XO   = XIT(time, T0L)
	XG   = direc * AFGEN(RADG, time, 'edge')
	edge = XO - XG

	return
	end
