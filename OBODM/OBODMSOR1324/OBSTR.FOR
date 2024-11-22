      Subroutine OBSTR(lmode,ncolm,imode,ibord,ipmod)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - OBSTR Version 01.1                          ---
CC--- Subroutine description - initialize text/graphics mode and set---
CC---                          up terminal for screen menu edit     ---
CC---                          processing.                          ---
CC---------------------------------------------------------------------
CC
CC    lmode - returned current text/graphics mode.
CC    ncolm - returned number of columns terminal currently set to.
CC    imode - returned new text/graphics mode.
CC    ibord - type of text/graphics board.
CC    ipmod - type of graphics control.
CC
CC*********************************************************************
CC
CCOMP1S-HP2645A
c     character*1 mescp
CCOMP1E-HP2645A
CCPCITS
      integer*2 imod,ibrd,lmod,ncol
CC
      include 'ONAMES.INC'
      include 'OCNTRL.INC'
CC
CCPCITE
CC
CC*********************************************************************
CC
CCOMP1S-HP2645A
c     lmode = 1
c     ncolm = 78
c     imode = 1
c     ibord = 1
c     ipmod = 1
c     mescp = CHAR(27)
c     WRITE (*,9000) mescp,'&','f','0','a','1','k','2','L',mescp,'p',
c    *               mescp,'&','f','0','a','2','k','2','L',mescp,'q',
c    *               mescp,'&','f','0','a','3','k','2','L',mescp,'r',
c    *               mescp,'&','f','0','a','4','k','2','L',mescp,'s',
c    *               mescp,'&','f','0','a','5','k','2','L',mescp,'t',
c    *               mescp,'&','f','0','a','6','k','2','L',mescp,'u',
c    *               mescp,'&','f','0','a','7','k','2','L',mescp,'v',
c    *               mescp,'&','f','0','a','8','k','2','L',mescp,'w'
c9000 format ($,80a1)
CCOMP1E-HP2645A
CCPCITS
CC      get current mode and number of columns.
      call QRMOD(lmod,ncol)
      lmode = lmod
      ncolm = ncol
      imode = 3
CC      initialize OBODM screen menu control.
CC      determine type of installed graphics board.
      ibrd = 2
      call QVIDB(ibrd)
      ibord = ibrd
CC      ibord - 0 - unknown video card.
CC              1 - installed CGA card.
CC              2 - installed EGA card.
CC              3 - installed VGA card.
   10 if (ibord .eq. 0) then
        i = KERRS(11,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0)
        if (i .gt. -4.and.i .lt. 0) then
          ibord = IABS(i)
        else
          go to 10
        end if
      end if
      if (ibord .eq. 1) then
        imode = 3
        ipmod = 4
      else if (ibord .eq. 2) then
        imode = 3
        ipmod = 16
      else if (ibord .eq. 3) then
        imode = 3
        ipmod = 18
      else
        ibord = 0
        go to 10
      end if
      if (ipmod .lt. 16) then
        mcolor(2) = mcolor(12)
        mcolor(3) = mcolor(14)
        mcolor(4) = mcolor(16)
      end if
CC        clear crt screen and set mode of crt display.
CC       imode  type(1) columns rows color(6) char-size pages(7)
CC         0      t        40    25     2        8x8      0-7
CC         1      t        40    25    16        8x8      0-7
CC         2      t        80    25     2        8x8      0-7
CC         3      t        80    25    16        8x8      0-7
CC         4      g       320   200     4        8x8      0-
CC         5      g       320   200     4        8x8      0-
CC         6      g       640   200     2        8x8      0-
CC         7      t(2)     80    25     2        9x14     0-7
CC         8      g(3)    640   400     2        8x16     0
CC         9      g(4)    -     -       -        -        -
CC        10      g(4)    -     -       -        -        -
CC        11      g(4)    -     -       -        -        -
CC        12      g(4)    -     -       -        -        -
CC        13(*)   g       320   200    16        8x8      0-7
CC        14(*)   g       640   200    16        8x8      0-3
CC        15(*)   g       640   350     4        8x14     0-1
CC        16(*)   g       640   350    16        8x14     0-1
CC        17(**)  g       640   480     2        8x16     0
CC        18(**)  g       640   480    16        8x16     0
CC        19(**)  g       320   200   256        8x8      0
CC
CC        (*) requires EGA card.
CC        (**) requires VGA card.
CC        (1) t/g = use as test (t) or graphics (g) mode.
CC        (2) restricted to 50 hz monochrome displays (or to multi-sync
CC            displays).  do not use on fixed frequency color monitors.
CC            sending an incorrect signal to a monitor can cause damage
CC            to the monitor.
CC        (3) the mode for users with at&t graphics adapter.
CC        (4) reserved.  not used by any graphics card to date.
CC        (5) restricted to 50 hz monochrome and multi-synch displays.
CC            see note #3 above.
CC        (6) this assumes your graphics board has the standard amount
CC            of memory.
CC        (7) the number of available pages depends on the amount of
CC            memory on your graphics board.
      imod = imode
      isvbrd = ibord
      call QSMOD(imod)
CCPCITE
      return
      end
