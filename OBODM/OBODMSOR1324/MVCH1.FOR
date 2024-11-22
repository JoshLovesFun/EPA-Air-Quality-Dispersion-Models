      Subroutine MVCH1(mbufn,mbufo,nchrs)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - MVCH1 Version 01.0                          ---
CC--- Subroutine description - move characters from one string to   ---
CC---                          another.                             ---
CC---------------------------------------------------------------------
CC
CC        mbufn - character*1 input array to be moved.
CC        mbufo - character*1 output array to be filled.
CC        nchrs - number of characters to move.
CC
CC********************************************************************
CC
      integer*2 i
CC
      character*1 mbufn,mbufo
CC
      dimension mbufn(500),mbufo(500)
CC
CC********************************************************************
CC
      do 10 i=1,nchrs
        mbufo(i) = mbufn(i)
   10 continue
      return
      end
