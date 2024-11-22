      Function IPKBK(maray,nchr)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IPKBK Version 01.0                          ---
CC--- Subroutine description - removes extra blanks from labels.    ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      integer*2 i,k,l,jflg
CC
      character*1 mblnk,maray
CC
      dimension maray(500)
CC
      data mblnk/' '/
CC
CC**********************************************************************
CC
      k = 0
      l = 0
      do 10 i=1,nchr
        if (maray(i) .ne. mblnk) then
          l = 0
          jflg = 1
        else
          l = l + 1
          jflg = 1
          if (l .gt. 1) jflg = 0
        end if
        if (jflg .ne. 0) then
          k = k + 1
          maray(k) = maray(i)
        end if
   10 continue
      if (k .lt. nchr) then
        l = k + 1
        do 20 i=l,nchr
          maray(i) = mblnk
   20   continue
      end if
      if (maray(k) .eq. mblnk) k = k - 1
      IPKBK = k
      return
      end
