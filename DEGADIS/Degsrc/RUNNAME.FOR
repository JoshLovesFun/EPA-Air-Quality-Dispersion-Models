	subroutine runname(opnf)
c
c........................................................................
c
c	Microsoft's FORTRAN 5.0 interface to get the RUNNAME from
c	the command line or ask for it if not given.
c
c.........................................................................
c
	character*(*) opnf

	integer*2 nnn, status
	character*80 buffer1
	character*1 buffer(80)
	equivalence (buffer1,buffer(1))
c
c... get the RUNNAME if entered on the command line
c
	nnn = 1
	call getarg(nnn, buffer1, status)
	opnf = buffer1
	if(status .gt. 0) return
c
c... ask for the run name since it was not on the command line
c
  100	write(6,1000)
 1000	format(/,' Enter the run name desired for this simulation: ',$)
	read(6,'(80a1)',err=100) buffer
	if(index(buffer1,' ') .eq. 1) goto 100
	opnf = buffer1
	return
	end
