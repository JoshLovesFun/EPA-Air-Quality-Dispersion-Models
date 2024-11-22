	subroutine rundeg(infile)
c
c... Subroutine to create a command file to run DEG on the PC
c
	implicit real*8(a-h,o-z), integer*4(i-n)
c
	common
     ./parm/	u0,z0,zr,rml,ustar,vkc,gg,rhoe,rhoa,deltay,betay,gammaf,
     .		yclow
     ./PHLAG/	CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5


	LOGICAL CHECK1,CHECK2,AGAIN,CHECK3,CHECK4,CHECK5
c
	character*(*) infile

	character*110 opnrup
	character*1 opnrup1(110)
	equivalence (opnrup1(1),opnrup)
c
	character*4 er1,er2,er3,bat,scl,sr3,lis
	character*1 plus
	character*2 con
	data scl/'.scl'/,sr3/'.sr3'/,lis/'.lis'/
	data bat/'.bat'/er1/'.er1'/,er2/'.er2'/,er3/'.er3'/
	data plus/'+'/,con/'  '/
c
c
	nchar = index(infile,' ') - 1
	infile = infile(1:nchar) // bat(1:4)
c
	write(6,1220) infile
 1220	format(/,' The batch file will be generated under',
     .		' the file name:',/,10x,a40)
c
	open(unit=8, file=infile, status='UNKNOWN')
c
c... strip off any directory reference from the run name
c
	infile = infile(1:nchar) // ' '
	opnrup = infile(1:nchar)
  10	iii = index(opnrup,'\')
	if(iii .ne. 0) then
	    opnrup = opnrup(iii+1:nchar)
	    nchar = nchar - iii
	    goto 10
	endif

	opnrup = opnrup(1:nchar) // er1(1:4)
c
	write(8,1250) (opnrup1(i),i=1,nchar+4)
 1250	format('copy %DEGADIS%\example.er1 ',40a1)
	IF(u0 .eq. 0.) then
	    write(8,1280) (opnrup1(i),i=1,nchar)
	    opnrup = opnrup(1:nchar) // er1(1:4)
	    write(8,1400) (opnrup1(i),i=1,nchar+4)
	    opnrup = opnrup(1:nchar) // '.tr2'
	    write(8,1400) (opnrup1(i),i=1,nchar+4)
	    goto 1340
	endif
	opnrup = opnrup(1:nchar) // er2(1:4)
c
	write(8,1260) (opnrup1(i),i=1,nchar+4)
 1260	format('copy %DEGADIS%\example.er2 ',40a1)
	opnrup = opnrup(1:nchar) // er3(1:4)
c
	if(.not.check4) then		! transient
c
	    write(8,1270) (opnrup1(i),i=1,nchar+4)
 1270	    format('copy %DEGADIS%\example.er3 ',40a1)
c
	    write(8,1280) (opnrup1(i),i=1,nchar)
 1280	    format('%DEGADIS%\DEG1  ',40a1)
	    write(8,1300) (opnrup1(i),i=1,nchar)
 1300	    format('%DEGADIS%\DEG2  ',40a1)
	    write(8,1320) (opnrup1(i),i=1,nchar)
 1320	    format('%DEGADIS%\DEG3  ',40a1)
c
	    opnrup = opnrup(1:nchar) // scl(1:4) //
     .		plus(1:1) // opnrup(1:nchar) // sr3(1:4) //
     .		con(1:2) // opnrup(1:nchar) // lis(1:4)
	    write(8,1370) (opnrup1(i),i=1,3*nchar+15)
 1370	    format('copy ',110a1)
c

	    opnrup = opnrup(1:nchar) // er1(1:4)
	    write(8,1400) (opnrup1(i),i=1,nchar+4)
	    opnrup = opnrup(1:nchar) // er2(1:4)
	    write(8,1400) (opnrup1(i),i=1,nchar+4)
	    opnrup = opnrup(1:nchar) // sr3(1:4)
	    write(8,1400) (opnrup1(i),i=1,nchar+4)
 1400	    format('del ',40a1)
	else
	    write(8,1280) (opnrup1(i),i=1,nchar)
c
	    write(8,1330) (opnrup1(i),i=1,nchar)
 1330	    format('%DEGADIS%\DEG2S  ',40a1)
c
	    opnrup = opnrup(1:nchar) // scl(1:4) //
     .		plus(1:1) // opnrup(1:nchar) // sr3(1:4) //
     .		con(1:2) // opnrup(1:nchar) // lis(1:4)
	    write(8,1370) (opnrup1(i),i=1,3*nchar+15)

	    opnrup = opnrup(1:nchar) // er1(1:4)
	    write(8,1400) (opnrup1(i),i=1,nchar+4)
	    opnrup = opnrup(1:nchar) // er2(1:4)
	    write(8,1400) (opnrup1(i),i=1,nchar+4)
	    opnrup = opnrup(1:nchar) // scl(1:4)
	    write(8,1400) (opnrup1(i),i=1,nchar+4)
	    opnrup = opnrup(1:nchar) // sr3(1:4)
	    write(8,1400) (opnrup1(i),i=1,nchar+4)
	endif
c
 1340	close(unit=8)

	return
	END
