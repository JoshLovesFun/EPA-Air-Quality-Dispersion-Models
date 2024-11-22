$NOTSTRICT
	INTERFACE TO FUNCTION SYSTEM[C] (STRING)
	INTEGER*2 SYSTEM
	CHARACTER*1 STRING[REFERENCE]
	END
c
c... Code for Microsoft's FORTRAN 5.0
c
c........................................................................
c
	program autoedit
c
c.....................................................................
c
c	Program to edit an AUTOEXEC.BAT file to append the line
c
c	SET DEGADIS=...
c
c	and to modify (or create) a PATH statement to use the target
c	directory.
c
c...................................................................
c
	character*132 string1
	character*1 string(132)
	equivalence (string1,string(1))

	character*132 root, opnf, string2

	integer*2 nnn
c
c... include the interface for the _system function
c       declare return types of the 2 C functions
c
	INTEGER*2 SYSTEM,iv
c
c... Let me introduce myself...
c
	write(6,1900)
 1900	format(//,' AUTOEDIT edits your AUTOEXEC.BAT file to ',
     .			'prepare for using DEGADIS.')
c
c... Determine the current default directory specification.
c
	root = 'C:\'
   10	nroot = index(root,' ') -1
	write(6,2000) root(1:nroot)
 2000	format(//,' Is the location of the boot directory: ',a5)
	write(6,2020)
 2020	format(/,'    Yes or No? ',$)
	read(5,'(a40)') string1
	if(string1(1:1).eq.'n' .or. string1(1:1).eq.'N') then
	    write(6,2040)
 2040	    format(/,' Please enter the boot directory name: ',$)
	    read(5,*) root
	    goto 10
	endif
c
c... Determine the current default directory specification.
c
	iv = SYSTEM('cd >xx.xx'//' 'C)
	open(unit=10, file='xx.xx', status='OLD')
	read(10,'(a40)') opnf
	nopnf = index(opnf,' ') 
	opnf = opnf(1:nopnf-1) // '\'
	iv = SYSTEM('del xx.xx'//' 'C)
	string1 = 'set DEGADIS=' // opnf(1:nopnf-1)
	iv = SYSTEM(string1(1:nopnf+11)//' 'C)
c
c... open the autoexec.bat file and read until an EOF is encountered.  Create
c	a new AUTOEXEC.BAT file from the old file.  In
c	the loop, check to see if DEGADIS has been set already.
c
	string2 = root(1:nroot) // 'autoexec.bat'
	open(unit=10, file=string2, status='UNKNOWN')

	string2 = opnf(1:nopnf) // 'autoexec.bat'
	open(unit=11, file=string2, status='UNKNOWN')

	kkk = 0
	kkkp= 0
 100	continue
	read(10,'(a132)', err=200, end=200) string1
	jjj = len_trim(string1)
	write(11,'(132a1)') (string(iii),iii=1,jjj)
	call upcase(string1)
	if(string1(1:12) .eq. 'SET DEGADIS=') then
	    kkk = 1
	    write(6,*)
     .		'Definition for DEGADIS being replaced in AUTOEXEC.BAT'
	    backspace (unit=11)
	    string1 = 'set DEGADIS=' // opnf(1:nopnf-1)
	    write(11,'(132a1)') (string(iii),iii=1,nopnf+11)
	endif
	if(string1(1:4) .eq. 'PATH') then
	    kkkp = 1
	    iii = index(string1(1:jjj), opnf(1:nopnf-1))
	    if(iii .eq. 0) then
		backspace (unit=11)
		    if(jjj+1+nopnf-1 .gt. 127) then
			write(6,*) 'PATH statement could not be modified',
     .				' because it would exceed 127 characters.'
			goto 100
		    endif
		write(6,*)'PATH statement being modified in AUTOEXEC.BAT'
	    	string1 = string1(1:jjj)//';'//opnf(1:nopnf-1)
		write(11,'(132a1)') (string(iii),iii=1,jjj+nopnf)
	    endif
	endif
	goto 100
c
c... close both files, copy the new autoexec.bat to the root, and delete
c	the new autoexec.bat in the DEGADIS target directory.
c
  200	if(kkk.eq.0) then
	    string1 = 'set DEGADIS=' // opnf(1:nopnf-1)
	    write(11,'(132a1)') (string(iii),iii=1,nopnf+11)
	endif
	if(kkkp.eq.0) then
	    string1 = 'PATH ' // opnf(1:nopnf-1)
	    write(11,'(132a1)') (string(iii),iii=1,nopnf+4)
	endif
	close(unit=10)
	close(unit=11)
c
	write(6,*) ' '
	string2 = 'copy  ' // root(1:nroot) // 'autoexec.bat  ' //
     .			      root(1:nroot) // 'autoexec.bak/v' // ' '
	nnn = 6+nroot+14+nroot+14
	if(kkk.ne.0) iv = SYSTEM(string2(1:nnn)//' 'C)

	string2 = 'copy  ' // opnf(1:nopnf) // 'autoexec.bat  ' //
     .			      root(1:nroot) // 'autoexec.bat/v' // ' '
	nnn = 6+nopnf+14+nroot+14
	iv = SYSTEM(string2(1:nnn)//' 'C)

	string2 = 'del  ' // opnf(1:nopnf) // 'autoexec.bat' // ' '
	nnn = 5+nopnf+12
	iv = SYSTEM(string2(1:nnn)//' 'C)
c
c... Remind the user the machine must be rebooted for the changes to 
c	take effect.
c
	write(6,3000)
 3000	format(//,' When the batch job finishes, please reboot ',
     .			'the computer because the changes',/,
     .		' to AUTOEXEC.BAT will only take effect after ',
     .			'the computer is rebooted.',//)
	stop
	end
