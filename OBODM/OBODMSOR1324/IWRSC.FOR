      Subroutine IWRSC(jcol,irow,nchr,mbufr,kbac,kfor)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IWRSC Version 01.1  (PCIT version)          ---
CC--- Subroutine description - write line to crt screen.
CC---------------------------------------------------------------------
CC
CC    jcol  - start column number on screen (0 - 79 lf to rt).
CC    irow  - start row number on screen (0 - 24 bot to top).
CC    nchr  - number of characters in buffer mbufr.
CC    mbufr - character buffer to write to screen.
CC    kbac  - current background color index.
CC    kfor  - current foreground color index.
CC
CC*********************************************************************
CC
      character*1 mbufr,mer,mwr
      integer*2 kbac,kfor,icha,jcol,icol,irow,ione,i,ia,is,ie,ib,js,je,
     *          ll
CC
      dimension mbufr(80),mer(2),mwr(2)
CC
      include 'OCNTRL.INC'
CC
      data mer/'E','R'/,ione/1/,mwr/'W','R'/
CC
CC*********************************************************************
CC
      if (isystm .eq. 2) then
        ia = 0
        ip = 0
        is = ICHAR('@')
        ie = ICHAR('~')
        ib = ICHAR(' ')
        if (ifhiof .eq. 0) then
          js = ICHAR('[')
          je = ICHAR(']')
        else
          js = 237
          je = 238
        end if
        do 10 i=1,nchr
          icol = i - 1 + jcol
          icha = ICHAR(mbufr(i))
CC          end of data field ']' or '~'.
          if ((ia .eq. 0.and.icha .eq. je).or.icha .eq. ie) then
            if (icha .ne. ie) then
              kbac = ibackt
              kfor = iforet
            else
              icha = ib
              ia = 0
              ip = 1
            end if
          end if
          if (i .eq. 79.and.(mbufr(i) .eq. mer(1).or.
     *        mbufr(i) .eq. mwr(1))) then
            kbac = ibackd + 8
            kfor = ifored
          end if
          if (icha .eq. is) then
            ia = 1
            ll = 1
            icha = ib
            kbac = ibackd
            kfor = ifored
          else
            ll = 0
          end if
CCPCITS
          call QCMOV(icol,irow)
          if (kfor .ne. iforet.or.(icol .gt. 0.and.icol .lt. nmxcol-1
     *        .and.irow .gt. 0.and.irow .lt. nmxrow-1)) then
            call QSTXT(icha,kbac,kfor,ione)
          else
            call QSTXT(icha,kfor,kbac,ione)
          end if
CCPCITE
CC          start of data field '[' or '@'.
          if (icha .eq. js.or.ll .eq. 1) then
            kbac = ibackd
            kfor = ifored
          else if (ip .eq. 1) then
            kbac = ibackt
            kfor = iforet
            ip = 0
          end if
   10   continue
      else if (isystm .eq. 1) then
        WRITE (*,9000) (mbufr(i),i=1,nchr)
      end if
      return
CC
 9000 format (80a1)
      end
