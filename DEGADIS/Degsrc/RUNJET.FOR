c$NOTSTRICT
c	INTERFACE TO FUNCTION SYSTEM[C] (STRING)
c	INTEGER*2 SYSTEM
c	CHARACTER*1 STRING[REFERENCE]
c	END
c
c... Code for Microsoft's FORTRAN 5.0
c
c........................................................................
c
	subroutine runjet(buffer, tend, u0, check4)
c
c........................................................................
c
c	RUNJET generates a command file to execute the JETPLU/DEGADIS model
C	from the input information.  This portion is highly dependent on the
c	DOS environment because of the instructions necessary to invoke
c	the DOS commands.
c
c.........................................................................
c

	Implicit Real*8 ( A-H, O-Z ), Integer*4 ( I-N )

	include 'DEGIN.prm'
C
	character*(*) buffer
	logical check4

	character*120 OPNf
	character OPNf1(120)
	equivalence (opnf1(1),opnf)

	character*4 er1,er2,er3,scl,sr3,lis
	character*1 plus
c
c
C
	DATA er1/'.er1'/,er2/'.er2'/,er3/'.er3'/
	data scl/'.scl'/,sr3/'.sr3'/,lis/'.lis'/
	data plus/'+'/
c
c
	opnf = buffer
	nchar = index(opnf,' ') - 1

	opnf = opnf(1:nchar) // '.bat'
c
	open(unit=8, file=opnf, status='UNKNOWN')
c
c... Lines to start the jet/plume model and invoke DEGBRIDGE
c
  10	continue
	iii = index(opnf,'\')
	if(iii .ne. 0) then
	    iii = iii+1
	    opnf = opnf(iii:nchar) // ' '
	    nchar = nchar - iii+1
	    goto 10
	endif
	write(8,1160) (opnf1(iii),iii=1,nchar)
 1160	format('%degadis%\jetplu    ',40a1)
c
c... Bypass DEGBRIDGE if this is an "JETPLU only" run
c
	if(tend .lt. 0.) goto 3000
	write(8,1180) (opnf1(i),i=1,nchar)
 1180	format('%degadis%\degbridg  ',40a1)
c
c... copy the ER1 file
c
	opnf = opnf(1:nchar) // er1(1:4)
c
c... do a zero windspeed case
c
	write(8,1250) (opnf1(i),i=1,nchar+4)
 1250	format('copy  %DEGADIS%\example.er1  ',40a1)
	IF(u0 .eq. 0.) then
		write(8,1280) (opnf1(i),i=1,nchar)
		goto 1340
		endif
	opnf = opnf(1:nchar) // er2(1:4)
c
	write(8,1260) (opnf1(i),i=1,nchar+4)
 1260	format('copy  %DEGADIS%\example.er2  ',40a1)
	opnf = opnf(1:nchar) // er3(1:4)
c
	if(.not.check4) then		! transient
c
		write(8,1270) (opnf1(i),i=1,nchar+4)
 1270		format('copy  %DEGADIS%\example.er3  ',40a1)
c
		write(8,1280) (opnf1(i),i=1,nchar)
 1280		format('%DEGADIS%\DEG1  ',40a1)
		write(8,1300) (opnf1(i),i=1,nchar)
 1300		format('%DEGADIS%\DEG2  ',40a1)
		write(8,1320) (opnf1(i),i=1,nchar)
 1320		format('%DEGADIS%\DEG3  ',40a1)
c
	else
		write(8,1280) (opnf1(i),i=1,nchar)
c
		write(8,1330) (opnf1(i),i=1,nchar)
 1330		format('%DEGADIS%\DEG2S ',40a1)
c
	endif

	opnf =               opnf(1:nchar) // '.out' //
     .		plus(1:1) // opnf(1:nchar) // scl(1:4) //
     .		plus(1:1) // opnf(1:nchar) // sr3(1:4) //
     .		'   '     // opnf(1:nchar) // lis(1:4)
	write(8,1370) (opnf1(i),i=1,4*nchar+21)
 1370	format('copy ',120a1)
c
c... perform some file cleanup
c
	    opnf = opnf(1:nchar) // er1(1:4)
	    write(8,1400) (opnf1(i),i=1,nchar+4)
	    opnf = opnf(1:nchar) // er2(1:4)
	    write(8,1400) (opnf1(i),i=1,nchar+4)
	    opnf = opnf(1:nchar) // sr3(1:4)
	    write(8,1400) (opnf1(i),i=1,nchar+4)
 1400	    format('del ',40a1)
	if(.not.check4) then
	    opnf = opnf(1:nchar) // er3(1:4)
	    write(8,1400) (opnf1(i),i=1,nchar+4)
	else
	    opnf = opnf(1:nchar) // scl(1:4)
	    write(8,1400) (opnf1(i),i=1,nchar+4)
	endif
c
c... close the command file
c
 1340	continue
	close(unit=8)

 3000	continue
	write(6,2099)
 2099	format(/,' JETPLUIN - the batch file is now ready.')
c
c
c... invoke DOS' command.com with the command line to run the BATch file
c
c... include the interface for the _system function
c       declare return types of the 2 C functions
c
c	INTEGER*2 SYSTEM,iv
c
c... NOTE:  The next 2 lines are commented which would start the batch
c	    file for the user.  Unfortunately, Microsoft's FORTRAN (or C)
c	    does not allow this to be done (cleanly) so that this program
c	    is terminated before starting the batch file.  This may pose
c	    unexpected problems for transient simulations, especially when
c	    memory resident programs are active.  Therefore, to avoid these
c	    problems and to provide for a similar interface as DEGINP, the
c	    automatic starting feature is disabled.
c
c 	opnf = opnf(1:nchar) // ' '
c	iv = SYSTEM(opnf(1:nchar)//' 'C)
	return
	END
