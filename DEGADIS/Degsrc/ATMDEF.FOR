	subroutine atmdef(zr, istab, avtime, rml, deltay, betay,
     .		deltaz, betaz, gammaz, sigxco, sigxp, sigxmd)
c
c.........................................................................
c
c	Subroutine to set some default atmospheric parameters based on values.
c
c	input values		output values
c	------------		-------------
c				rml
c	zr			deltay, betay
c	istab			deltaz, betaz, gammaz
c	avtime			sigxco, sigxp, sigxmd
c
c
c........................................................................
c

	implicit real*8(a-h, o-z), integer*4(i-n)

c
c... Lookup based on the stability
c

	goto(100,200,300,400,500,600) istab
c
c.......................................................... A stability 
c
 100	timeav = dmax1( avtime, 18.4D0)
	deltay = 0.423D0*(timeav/600.D0)**0.2D0
	betay  = 0.9D0

	deltaz = 107.66D0
	betaz  = -1.7172D0
	gammaz = 0.2770D0

	rml    = -11.43D0 * zr**0.103D0

	sigxco = 0.02D0
	sigxp  = 1.22D0
	sigxmd = 130.D0
	return
c
c.......................................................... B stability 
c
 200	timeav = dmax1( avtime, 18.4D0)
	deltay = 0.313D0*(timeav/600.D0)**0.2D0
	betay  = 0.9D0

	deltaz = 0.1355D0
	betaz  = 0.8752D0
	gammaz = 0.0136D0

	rml    = -25.98D0 * zr**0.171D0

	sigxco = 0.02D0
	sigxp  = 1.22D0
	sigxmd = 130.D0
	return
c
c.......................................................... C stability 
c
 300	timeav = dmax1( avtime, 18.4D0)
	deltay = 0.210D0*(timeav/600.D0)**0.2D0
	betay  = 0.9D0

	deltaz = 0.09623D0
	betaz  = 0.9477D0
	gammaz = -0.0020D0

	rml    = -123.4D0 * zr**0.304D0

	sigxco = 0.02D0
	sigxp  = 1.22D0
	sigxmd = 130.D0
	return

c
c.......................................................... D stability 
c
 400	timeav = dmax1( avtime, 18.3D0)
	deltay = 0.136D0*(timeav/600.D0)**0.2D0
	betay  = 0.9D0

	deltaz = 0.04134D0
	betaz  = 1.1737D0
	gammaz = -0.0316D0

	rml    = 0.0D0		! used for infinity

	sigxco = 0.04D0
	sigxp  = 1.14D0
	sigxmd = 100.D0
	return

c
c.......................................................... E stability 
c
 500	timeav = dmax1( avtime, 11.4D0)
	deltay = 0.102D0*(timeav/600.D0)**0.2D0
	betay  = 0.9D0

	deltaz = 0.02275D0
	betaz  = 1.3010D0
	gammaz = -0.0450D0

	rml    = 123.4D0 * zr**0.304D0

	sigxco = 0.17D0
	sigxp  = 0.97D0
	sigxmd = 50.D0
	return

c
c.......................................................... F stability 
c
 600	timeav = dmax1( avtime, 4.6D0)
	deltay = 0.0674D0*(timeav/600.D0)**0.2D0
	betay  = 0.9D0

	deltaz = 0.01122D0
	betaz  = 1.4024D0
	gammaz = -0.0540D0

	rml    = 25.98D0 * zr**0.171D0

	sigxco = 0.17D0
	sigxp  = 0.97D0
	sigxmd = 50.D0
	return
	end
