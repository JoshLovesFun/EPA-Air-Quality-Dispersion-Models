CCPCITS
      interface to Subroutine SOUND[c](ifreq)
      integer*2 ifreq
      end
      interface to Subroutine NOSOUND[c]
      end
      interface to Subroutine WAITM[c](i1,i2)
      integer*2 i1,i2
      end
CCPCITE
      Subroutine IBELL(ntime)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IBELL Version 01.1                          ---
CC--- Subroutine description - ring bell                            ---
CC---------------------------------------------------------------------
CC
CC    ntime - number of times to ring bell
CC
CC*********************************************************************
CC
      integer*2 i1,i2,ifreq
CCOMP1S
c     character*1 mbell
CCOMP1E
CC
      data i1,i2,ifreq/0,1,250/
CCOMP1S
c     data mbell/''/
CCOMP1E
CC
CC*********************************************************************
CC
      do 10 n=1,ntime
CCPCITS
        call SOUND(ifreq)
        call WAITM(i1,i2)
CCPCITE
CCOMP1S
c       WRITE (*,9000) mbell
CCOMP1E
   10 continue
CCPCITS
      call NOSOUND
CCPCITE
      return
CCOMP1S
c9000 format (a1,$)
CCOMP1E
      end
