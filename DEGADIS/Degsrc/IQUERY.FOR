	function iquery(string, dflt, value)
c
c.....................................................................
c
c   Function to query the user about a particular parameter value.
c
c	IQUERY is used to control program execution based on the user
c	response.
c
c		IQUERY > 0 indicates a question was asked (?).
c		IQUERY = 0 indicates no troubles.
c		IQUERY < 0 indicates the user wants to return to the
c				previous question (/).
c
c.....................................................................
c
	implicit real*8(a-h,o-z), integer*4(i-n)

	character*(*) string
	character*20 str2

	mmm = len(string)
c
c... Pose the question and get the response.
c
  100	write(6,1000) string(1:mmm),dflt
 1000	format(/,1x,a56,' <',1pg12.5,'> ',$)
	read(5,1200) str2
 1200	format(a20)
c
c... check for a cry for help.
c
	nnn = index(str2,'?')
	if(nnn .ne. 0) then
	    iquery = 1
	    return
	endif
c
c... check for a request for the previous question.
c
	nnn = index(str2,'/')
	if(nnn .ne. 0) then
	    iquery = -1
	    return
	endif
c
c... check for a default response.
c
	nnn = index(str2,' ')
	if(nnn .eq. 1) then
	    value = dflt
	    iquery = 0
	    return
	endif
c
c... translate the response and put it in VALUE
c
	nnn = nnn-1
	read( str2(1:nnn), 1400, err=100) value
 1400	format(g24.0)
	iquery = 0

	return
	end
