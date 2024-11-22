	subroutine upcase(string)
c
c..........................................................
c
c	Coding to convert a string to all uppercase.
c
c..........................................................
c
	character*(*) string
	character*1 ddd

	nnn = len(string)
	do 100 iii=1,nnn
	ddd = string(iii:iii)
	jjj = ichar(ddd)
	if(jjj.ge.97 .and. jjj.le.122) then
	    jjj = jjj - 32
	    ddd = char(jjj)
	    string(iii:iii) = ddd
	endif
  100	continue

	return
	end
