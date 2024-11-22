      Subroutine MBLK1(maray,nchrs)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - MBLK1 Version 01.0                          ---
CC--- Subroutine description - blank out character array.           ---
CC---------------------------------------------------------------------
CC
CC    maray - character array to be filled with blanks.
CC    nchrs - number of blanks to fill.
CC
CC********************************************************************
CC
      integer*2 i
CC
      character*1 maray,mblnk
CC
      dimension maray(3000)
CC
      data mblnk/' '/
CC
CC*****************************************************************
CC
      do 10 i=1,nchrs
        maray(i) = mblnk
   10 continue
      return
      end
