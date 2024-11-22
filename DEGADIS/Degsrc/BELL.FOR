$NOTSTRICT
        INTERFACE TO FUNCTION SYSTEM[C] (STRING)
        INTEGER*2 SYSTEM
        CHARACTER*1 STRING[REFERENCE]
        END
c
c... Code for Microsoft's FORTRAN 5.0
c
c...................................................................

	program bell

c...................................................................
c
c... Program to ring the bell using the text file BELL.TXT
c
c-------------------------------------------------------------------
c
c	Microsoft's FORTRAN 5.0 coding
c
c... Based on DEMOEXEC.FOR from Microsoft - demonstration program for
c	calling C library functions
c
c
c  	include the interface for the _system function
c
c       declare return types of the 2 C functions
c
	INTEGER*2 SYSTEM,i
c
c
	character*80 dummy
	character*1 dummy1(80)
	equivalence (dummy, dummy1(1))

c
c
c... invoke DOS' command.com with the command line:
c
c
c               echo ^G
c
	I = SYSTEM('echo  'C)
	stop ' '
	end
