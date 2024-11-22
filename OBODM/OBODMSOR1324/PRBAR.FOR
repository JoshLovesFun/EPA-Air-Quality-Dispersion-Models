CCPCITS
      interface to Subroutine SHOWM[C]
      end
      interface to Subroutine HIDEM[C]
      end
      Subroutine PRBAR
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - PRBAR version 01.1                          ---
CC--- Subroutine description - display menu bar                     ---
CC---------------------------------------------------------------------
CC
CC*********************************************************************
CC
      character*1 mntmp
      integer*2 jchar,nrept,iloc,jloc,kbac,kfor,icoff
CC
      include 'ONAMES.INC'
      include 'OCNTRL.INC'
CC
CC    icmsta - status flag indicating choice of background and foreground
CC            colors for each field in menu bar
CC            0 - option unavailable or mode off
CC            1 - option avaialble
CC            2 - option available and highlight first letter
CC            3 - option chosen
CC            4 - mode on
CC    ipage - video page number
CC
      dimension mntmp(32)
CC
      equivalence (mcmlab,mntmp)
CC
      data icoff/3/
CC
CC*********************************************************************
CC
CC      write appropriate fields in menu bar
CC
      call CRTYP(icoff)
      if (locxms .ne. 0.or.locyms .ne. 0) call HIDEM
      jloc = icmrow
      iloc = 0
      nrept = 1
      call QCMOV(iloc,jloc)
      jchar = ICHAR(' ')
      kfor = ibackt
      kbac = iforet
      call QSTXT(jchar,kbac,kfor,nrept)
      ioff = 0
      do 20 n=1,icmnum
        iloc = icmcol + icmbeg(n) - 1
        do 10 m=1,icmlen(n)
          iloc = iloc + 1
          ioff = ioff + 1
          if (icmsta(n) .ne. 0) then
            kfor = ibackt
            kbac = iforet
            if (icmsta(n) .eq. 2.and.m .eq. 1) kfor = 15
            if (icmsta(n) .eq. 0) kfor = kbac
            if (icmsta(n) .eq.3) then
              kfor = 15
              kbac = ibackt
            end if
            call QCMOV(iloc,jloc)
            jchar = ichar(mntmp(ioff))
            call QSTXT(jchar,kbac,kfor,nrept)
          end if
   10   continue
   20 continue
      if (locxms .ne. 0.or.locyms .ne. 0) call SHOWM
      return
      end
CCPCITE
