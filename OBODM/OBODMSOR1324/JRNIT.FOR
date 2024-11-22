      Function JRNIT(raray,n,rinit,iflg)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - JRNIT Version 01.0                          ---
CC--- Subroutine description - initialize n locations of real array,---
CC---                          raray, depending on iflg.            ---
CC---------------------------------------------------------------------
CC
CC        iflg = 0 rinit is single valued.
CC               1 rinit is array.
CC
CC**********************************************************************
CC
      integer*2 i,j
CC
      dimension raray(50000),rinit(50000)
CC
CC**********************************************************************
CC
      do 10 i=1,n
        j = 1
        if (iflg .ne. 0) j = i
        raray(i) = rinit(j)
   10 continue
      JRNIT = n
      return
      end
