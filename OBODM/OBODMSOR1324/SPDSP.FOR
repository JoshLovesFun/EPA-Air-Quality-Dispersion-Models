CCPCITS
      Subroutine SPDSP(iop)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - SPDSP Version 01.2                          ---
CC--- Subroutine description - place special labels on screen menu. ---
CC---------------------------------------------------------------------
CC
CC    iop - index of special label
CC          0 - place color options on graphics screen 68.
CC
CC**********************************************************************
CC
      character*67 mbufr
CC
      integer*2 icha,ibak,ifor,ione,irow,icol
CC
      include 'OCNTRL.INC'
      include 'OWNTRL.INC'
      include 'ONAMES.INC'
CC
      data mbufr/'Color Options -  , , , , , , , , , , , , , , ,'/
CC
CC**********************************************************************
CC
      if (iop .eq. 0) then
        if (ipmode .lt. 16) then
          ncolr = 4
          nchr = 24
          jcol = 26
        else
          ncolr = 16
          nchr = 46
          jcol = 16
        end if
        icoL = jcol
        irow = 4
        ibak = ibackt
        ifor = iforet
        call IWRSC(icol,irow,nchr,mbufr,ibak,ifor)
        icol = icol + 14
        ione = 1
        ibak = iforet
        if (ibak .eq. 0) ibak = 1
        do 10 i=1,ncolr
          icol = icol + 2
          call QCMOV(icol,irow)
          icha = ichar(malpha(i))
          ifor = i - 1
          call QSTXT(icha,ibak,ifor,ione)
          ibak = 0
   10   continue
      end if
      return
      end
CCPCITE
