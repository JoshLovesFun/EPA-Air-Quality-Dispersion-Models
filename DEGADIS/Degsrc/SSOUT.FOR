	subroutine ssout(out)
c
c................................................................
c
c	subroutine to control the output for steady state simulations
c
c................................................................
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	dimension out(9)
c
	common
     ./ALP/	ALPHA,alpha1
     ./cgprop/	gasmw,gastem,gasrho,gascpk,gascpp,gasulc,gasllc,gaszzc,
     .		gasnam
     ./comfl/	cflag,clfl,cufl
     ./oomsin/	oodist,avtime
C
	logical cflag
C
c
	data ip/0/
c
	dist = out(1) + oodist
	yc = out(2)
	cc = out(3)
	rho = out(4)
	gamma = out(5)
	temp = out(6)
	b = out(7)
	sz = out(8)
	sy = out(9)
c
	if(.not.cflag) then
	   call adiabat(-2,wc,wa,gasllc,ya,clfl,r,w,gamma,tt)
	   call adiabat(-2,wc,wa,gasulc,ya,cufl,r,w,gamma,tt)
	endif
c
	arg = (gaszzc/sz)**alpha1
	if(arg .ge. 80.) then
	   if(cflag) then
		WRITE(8,1120) DIST,yc,Cc,rho,temp,B,SZ,SY
	   else
		WRITE(8,1125) DIST,yc,Cc,rho,gamma,temp,B,SZ,SY
	   endif
	   goto 600
	endif
c
	ccz = cc/exp(arg)
	if(ccz .lt. clfl) then
	   if(cflag) then
		WRITE(8,1120) DIST,yc,Cc,rho,temp,B,SZ,SY
	   else
		WRITE(8,1125) DIST,yc,Cc,rho,gamma,temp,B,SZ,SY
	   endif
	   goto 600
	endif

	arg = -(dlog(clfl/cc) + (gaszzc/sz)**alpha1)
	blfl = sqrt(arg)*sy + b
c
	if(ccz .lt. cufl) then
	   if(cflag) then
		WRITE(8,1120) DIST,yc,Cc,rho,temp,B,SZ,SY,blfl
	   else
		WRITE(8,1125) DIST,yc,Cc,rho,gamma,temp,B,SZ,SY,blfl
	   endif
	   goto 600
	endif

	arg = -(dlog(cufl/cc) + (gaszzc/sz)**alpha1)
	bufl = sqrt(arg)*sy + b
	if(cflag) then
	   WRITE(8,1120) DIST,yc,Cc,rho,temp,B,SZ,SY,blfl,bufl
	else
	   WRITE(8,1125) DIST,yc,Cc,rho,gamma,temp,B,SZ,SY,blfl,bufl
	endif
c
  600	continue
	ip = ip + 1
	if(ip .eq. 3) then
	   ip = 0
	   write(8,1119)
	endif
C
C
 1119	FORMAT(1H )
 1120	FORMAT(1H ,11(1X,1PG9.3,1X))
 1125	FORMAT(1H ,3(1X,1PG9.3,1X),2x,0PF7.4,2x,1X,1PG10.3,1X,
     .		6(1X,1PG9.3,1X))
C
	return
	end
