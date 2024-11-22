      Function JINIT(iaray,n,init,iflg)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - JINIT Version 01.0                          ---
CC--- Subroutine description - initialize n locations of integer    ---
CC---                          array iaray from init, depending on  ---
CC---                          iflg.                                ---
CC---------------------------------------------------------------------
CC
CC        iflg = 0, init is single valued.
CC               1, init is array.
CC
CC**********************************************************************
CC
      integer*2 i,j
CC
      dimension iaray(50000),init(50000)
CC
CC**********************************************************************
CC
      do 10 i=1,n
        j = 1
        if (iflg .ne. 0) j = i
        iaray(i) = init(j)
   10 continue
      JINIT = n
      return
      end
