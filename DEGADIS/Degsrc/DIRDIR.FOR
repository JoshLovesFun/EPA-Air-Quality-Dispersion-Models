c
c... Code for Microsoft's FORTRAN 5.0 not needed with Intel compiler.
c
c$NOTSTRICT
c        INTERFACE TO FUNCTION SYSTEM[C] (STRING)
c        INTEGER*2 SYSTEM
c        CHARACTER*1 STRING[REFERENCE]
c        END
c
c-------------------------------------------------------------------

	subroutine dirdir(exten)

c-------------------------------------------------------------------
c
c... Subroutine to determine the list of files which exist in this
c	directory with extension EXTEN.
c
c-------------------------------------------------------------------
c

	implicit real*8(a-h,o-z), integer*4(i-n)

c
c  	include the interface for the _system function
c       declare return type
c
	INTEGER*4 SYSTEM,i
c
c
	character*80 dummy
	character*1 dummy1(80)
	equivalence (dummy, dummy1(1))

	character*1 dflt
	character*4 exten

	data dflt/'*'/
c
c... get a file spec
c
   50	write(6,800)
  800	format(/,' Enter the file specification: ',$)
	read(5,900) dummy
  900	format(a80)
	nnn = index(dummy,' ')
	if(nnn .eq. 1) then
	    iii = 5
	    dummy = dflt(1:1) // exten(1:4)
	    goto 100
	endif
c
c... If they included the extension, trim it off.  Then add the extension INP.
c
	jjj = index(dummy,']')
	jjj = jjj+1
	iii = index(dummy(jjj:80),'.')
	if(iii.eq.0) then
	    iii = nnn
	else
	    iii = jjj-1 + iii
	endif
	dummy = dummy(1:iii-1) // exten(1:4)

	iii = iii+3
  100	continue
	write(6,950) (dummy1(jjj),jjj=1,iii)
 950	format(//,1x,'Looking for: ',75a1)
c
c
c... invoke DOS' command.com with the command line:
c
c
c               dir DUMMY|more
c
	I = SYSTEM('dir '//dummy(1:iii)//'|sort|more')
	WRITE (6,*) '<CR> to continue'
	read(5,900) dummy
	return
	end
