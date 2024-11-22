CCPCITS
      Function IFCTC[C](signum)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IFCTC Version 01.1                          ---
CC--- Subroutine description - sets ifctlc equal to 1 or 2 if a     ---
CC---                          CTRL C is typed.                     ---
CC---------------------------------------------------------------------
CC
CC    signum - integer*2 value of SIG$INT for CRTL C.
CC
CC*********************************************************************
CC
      integer*2 signum,iscn,ichr
CC
      include 'OCNTRL.INC'
      include 'ONAMES.INC'
CC
      data iscn/1/,ichr/32/
CC
CC*********************************************************************
CC
      IFCTC = 1
      iflg = 0
      if (ifccok .eq. 2) then
CC        interupt general processing if signum is equal to 2.
        ifctlc = signum
      else if (ifccok .eq. 1) then
CC        interupt at input data field.
        ifctlc = 1
        iflg = 1
      else if (ifccok .eq. 0) then
CC        ring bell, cannot interupt at this time.
        call ibell(1)
        ifctlc = 0
      else
        stop
      end if
CC      reset Crtl-C break
      i = IRSBK()
CC      if at data field, send character to keyboard buffer, to
CC      provide a time break to complete this asynchronous routine
CC      (IFCTC), making sure ifctlc gets back into common.
      if (iflg .eq. 1) call WRBUF(iscn,ichr)
      return
      end
CCPCITE
