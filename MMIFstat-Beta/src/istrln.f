      function istrln( string )

c-----This routine returns the non-blank length of a string.
c
c     Inputs:
c       string   C   string for determining length

      implicit none

      integer istrln
      character*(*) string

      integer i

      istrln = 0
      do 10 i = LEN( string ),1,-1
         if( string(i:i) .NE. ' ' ) then
             istrln = i
             goto 9999
         endif
   10 continue
c
 9999 continue
      return
      end
