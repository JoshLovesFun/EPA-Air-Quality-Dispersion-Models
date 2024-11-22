CCPCITS
      Subroutine IOCRT(mscrn,iline,itabs,ispac,iferr,iback,ifore)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IOCRT Version 01.1                          ---
CC--- Subroutine description - write menu to crt screen.            ---
CC---------------------------------------------------------------------
CC
CC    mscrn - 80 by 25 character array containing the screen menu.
CC            the first dimension (80) specifies screen columns left to
CC            right, 1 to 80.  the second dimension specifies screen
CC            line number top to bottom, 1 to 25.
CC    iline - array giving the line numbers for lines in mscrn that
CC            contain data fields. if iline(k) > 0, then the line
CC            starting at mscrn(1,iline(k)) contains data fields,
CC            where k = 1 to 20 (maximum number of lines that can
CC            contain data fields = 20).
CC    iferr - array indicating if data in each data field is erroneous
CC            or not. if iferr(k,l) =
CC              0 - no error.
CC              1 - erroneous, highlight this field.
CC              2 - warning, highlight this field.
CC    iback - background color index.
CC    ifore - foreground color index.
CC
CC
CC*********************************************************************
CC
      character*1 mscrn,mbufr,mblnk,mer
      character*2 mvbf2
      character*4 mvbfs
      integer*2 ibac,ifor,izro,kbac,kfor,irow,icol,i,j,k,iber,ifer,
     *          ibwr,ifwr
CC
      dimension mscrn(80,25),iline(20),iferr(20,10),itabs(20,10),
     *          ispac(20,10),mbufr(80),mer(2)
CC
      include 'OCNTRL.INC'
CC
      equivalence (mvbf2,mvbfs)
CC
      data izro/0/,mblnk/' '/
CC
CC*********************************************************************
CC
CC      program version number.
      write (mvbfs,9000) versod
      i = versod
      write (mvbf2,9001) i
      call mvch1(mvbfs,mscrn(45,1),4)
      do 10 i=2,nmxcol-1
        if ((mscrn(i,1) .eq. mblnk.and.mscrn(i+1,1) .eq. mblnk).or.
     *      (mscrn(i,1) .eq. mblnk.and.mscrn(i-1,1) .eq. CHAR(196)))
     *      then
        mscrn(i,1) = CHAR(196)
        end if
        if ((mscrn(i,nmxrow).eq.mblnk.and.mscrn(i+1,nmxrow).eq.mblnk)
     *  .or.(mscrn(i,nmxrow).eq.mblnk.and.mscrn(i-1,nmxrow).eq.
     *      CHAR(196))) then
          mscrn(i,nmxrow) = CHAR(196)
        end if
   10 continue
      do 20 j=2,nmxrow-1
        if (mscrn(1,j) .eq. mblnk) then
          mscrn(1,j) = CHAR(179)
        end if
        if (mscrn(nmxcol,j) .eq. mblnk) then
          mscrn(nmxcol,j) = CHAR(179)
        end if
   20 continue
      if (mscrn(1,1) .eq. mblnk) mscrn(1,1) = CHAR(218)
      if (mscrn(1,nmxrow) .eq. mblnk) mscrn(1,nmxrow) = CHAR(192)
      if (mscrn(nmxcol,1) .eq. mblnk) mscrn(nmxcol,1) = CHAR(191)
      if (mscrn(nmxcol,nmxrow) .eq. mblnk)
     *    mscrn(nmxcol,nmxrow) = CHAR(217)
CC
      ibac = iback
      ifor = ifore
      kbac = ibac
      kfor = ifor
      mer(1) = CHAR(16)
      mer(2) = CHAR(17)
      iber = ibackt + 8
      ifer = iforet
      ibwr = iber
      ifwr = ifer
      irow = nmxrow - 1
      icol = 0
      call QCMOV(icol,irow)
      j = 0
CC      loop down over screen lines.
   30 j = j + 1
      if (j .le. nmxrow) then
        irow = nmxrow - j
        if (irow .eq. 0.or.irow .eq. nmxrow-1) then
CC          border.
          kbac = iforet
          kfor = ibackt
        else
          kbac = ibac
          kfor = ifor
        end if
CC        move line into buffer.
        call MVCH1(mscrn(1,j),mbufr,80)
        call IWRSC(izro,irow,nmxcol,mbufr,kbac,kfor)
CC        see if any data fields on this line are in error.
        do 50 i=1,20
          if (iline(i) .eq. j) then
            do 40 k=1,10
              icol = itabs(i,k) - 2
              mbufr(1) = mer(1)
              mbufr(2) = mer(2)
              if (iferr(i,k) .eq. 1) then
CC                error flag set.
                call IWRSC(icol,irow,1,mbufr(1),iber,ifer)
                icol = icol + ispac(i,k) + 1
                call IWRSC(icol,irow,1,mbufr(2),iber,ifer)
              else if (iferr(i,k) .eq. 2) then
CC                warning flag set.
                call IWRSC(icol,irow,1,mbufr(1),ibwr,ifwr)
                icol = icol + ispac(i,k) + 1                            010400
                call IWRSC(icol,irow,1,mbufr(2),ibwr,ifwr)
              end if
   40       continue
          end if
   50   continue
        go to 30
      end if
      return
CC
 9000 format (f4.1)
 9001 format (i2.2)
      end
CCPCITE
