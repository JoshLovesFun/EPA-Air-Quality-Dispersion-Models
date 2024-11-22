CCPCITS
      interface to Subroutine SETDM[C](imode)
      integer*2 imode
      end
      Subroutine QSMOD(imode)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - QSMOD Version 01.1                          ---
CC--- Subroutine description - clears screen and sets mode of crt   ---
CC---                          display and sets predefined colors.
CC---------------------------------------------------------------------
CC
CC    imode - 0 <= mode <= 7
CC
CC                  resolution         color      text/graphic
CC    imode = 0   40 col x 25 rows      b//w          text
CC          = 1   40 col x 25 rows      color         text
CC          = 2   80 col x 25 rows      b/w           text
CC          = 3   80 col x 25 rows      color         text
CC          = 4  320 col x 200 rows     color        graphic
CC          = 5  320 col x 200 rows     b/w          graphic
CC          = 6  640 col x 200 rows     b/w          graphic
CC          = 7   80 col x 25 rows      b/w           text (monochrome)
CC
CC**********************************************************************
CC
      integer*2 imode,jclrs,jregs,jmode
      byte icol(20),irow(20),ixpix(20),iypix(20),istdc(16)
CC
      include 'OCNTRL.INC'
CC
      data icol/2*40,2*80,2*40,2*80,20,40,80,2*0,40,5*80,40/,
     *     irow/11*25,2*0,4*25,2*30,25/,
     *     ixpix/4*64,2*32,10*64,64,3*64/,
     *     iypix/4*20,2*20,10*20,35,3*48/,
     *     istdc/0,1,2,3,4,5,20,7,56,57,58,59,60,61,62,63/
CC
CC**********************************************************************
CC
      icolmx = icol(imode+1)
      irowmx = irow(imode+1)
      imxpix = ixpix(imode+1) * 10
      imypix = iypix(imode+1) * 10
      call SETDM(imode)
CC      set palette registers to "standard" colors if EGA or better
      if (isvbrd .ge. 2) then
        jmode = 0
        do 10 i=1,16
          jregs = i - 1
          jclrs = istdc(i)
          call QPREG(jregs,jclrs,jmode)
   10   continue
      end if
      return
      end
CCPCITE
