      Subroutine STRPQ
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - STRPQ Version 01.1                          ---
CC--- Subroutine description - initialize plots.                    ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      character*22 msrch
      integer*2 ipmd,ipal,ibord,icbk,icmod
CC
      include 'OCNTRL.INC'
      include 'OWNTRL.INC'
      include 'ONAMES.INC'
CC
      data msrch/'SCRATCH plot file'/
CC
CC**********************************************************************
CC
CC       initialize plotting -
      nplots = nplots + 1
      lcolor = -1
CC      write plot commands to output file, initial title record.
      if (iplfil .gt. 0) then
        WRITE (iplfil,9000,err=10,iostat=ier) xnchpl,ynchpl,nplots,
     *                                        (mtitle(i),i=1,nchtrs)
        go to 20
   10   i = KERRS(2,mplnam,ier,0,xdum,xdum,xdum,xdum,0)
        iplfil = 0
   20 end if
      ipmd = ipmode
CCPCITS
      call QSMOD(ipmd)
CCPCITE
      ibord = 4
      if (iswplt(9) .eq. 2.or.iswplt(9) .eq. 4.or.iswplt(9) .eq. 5)
     *    ibord = 1
CCPCITS
      call QOVSC(ibord)
CCPCITE
      icbk = iswplt(9)
      if (ipmode .lt. 16) then
        ipal = 1
CCPCITS
        call QCOLR(icbk,ipal)
CCPCITE
      else
        ipal = 0
CCPCITS
        icmod = 1
        call QPREG(ipal,icbk,icmod)
CCPCITE
      end if
      if (ipmode .lt. 16) then
        iswplt(13) = 1
        iswplt(14) = 2
        iswplt(15) = 3
      else
        iswplt(13) = iswplt(16)
        iswplt(14) = iswplt(12)
        iswplt(15) = iswplt(23)
        if (iswplt(14) .le. 0.or.iswplt(14) .eq. iswplt(9)) then
          iswplt(14) = iswplt(16) + 1
        end if
        if (iswplt(15) .le. 0.or.iswplt(15) .eq. iswplt(9)) then
          iswplt(15) = iswplt(16) + 2
        end if
      end if
      rastrh = pixelx / xnchpl
      rastrv = pixely / ynchpl
      if (iscfil .gt. 0) then
CC        write plot commands to scratch file for hard copy
CC        plot generation.
        REWIND iscfil
        WRITE (iscfil,9001,err=30,iostat=ier) xnchpl,ynchpl,nplots,
     *                                        iswplt(9),(iswplt(i),
     *                                        i=13,15)
        go to 40
   30   i = KERRS(2,msrch,ier,0,xdum,xdum,xdum,xdum,0)
        iscfil = 0
      end if
   40 return
CC
 9000 format (' S',2f8.4,i4/80a1)
 9001 format (' S',2f8.4,5i2)
      end
