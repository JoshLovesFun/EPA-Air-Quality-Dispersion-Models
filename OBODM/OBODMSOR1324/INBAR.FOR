CCPCITS
      Function INBAR(ix,iy,numbr)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - INBAR version 01.1                          ---
CC--- Subroutine description - see if screen location is in a valid ---
CC---                          command bar field.                   ---
CC---------------------------------------------------------------------
CC
CC    inputs:
CC    ix - column location
CC    iy - row location
CC    returns:
CC    inbar - command number ( -1 if not valid location )
CC    numbr - field number (0 indicates repeat location)
CC
CC*********************************************************************
CC
      integer*2 ix,iy,numbr
CC
      include 'OCNTRL.INC'
CC
CC*********************************************************************
CC
CC      determine if in field
CC
      inbar = -1
      numbr = -1
CC
CC      check row
CC
      if (iy .ne. (24-icmrow)) return
CC
CC      check if in repeat column
CC
      if (ix .eq. 0) then
        numbr = 0
        return
      end if
CC
CC      check column extremes
CC
      nbeg = icmbeg(1) + icmcol
      nend = icmbeg(icmnum) + icmcol + icmlen(icmnum) - 1
      if (ix .lt. nbeg.or.ix .gt. nend) return
CC
CC      scan over fields
CC
      do 10 n=1,icmnum
        nsave = n
        nbeg = icmbeg(n)+icmcol
        nend = nbeg + icmlen(n) - 1
        if (ix .ge. nbeg.and.ix .le. nend) go to 20
   10 continue
      go to 30
   20 continue
CC
CC      map to command number if it is a currently valid field
CC
      if (icmsta(nsave) .gt. 0) then
        inbar = icmmap(nsave)
        numbr = nsave
      end if
CC
   30 continue
      return
      end
CCPCITE
