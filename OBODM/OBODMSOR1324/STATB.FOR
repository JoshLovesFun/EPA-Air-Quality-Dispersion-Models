CCPCITS
      Subroutine STATB(numsc)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - STATB version 01.1                          ---
CC--- Subroutine description - set status of fields in command bar  ---
CC---------------------------------------------------------------------
CC
CC    numsc - screen menu number, or control number if negative.
CC
CC    returns -
CC    icmsta(n) = 0:  off
CC                2:  on with highlighted first letter
CC
CC*********************************************************************
CC
      include 'OCNTRL.INC'
CC
CC*********************************************************************
CC
      if (numsc .ne. 0) then
CC
CC----set all fields to display
CC
        do 10 n=1,7
          icmsta(n) = 2
   10   continue
CC
CC----turn of main (icmsta(1))
CC
CC        if already at main.        
        if (numsc .eq. 3) icmsta(1) = 0
CC
CC----turn off help (icmsta(2))
CC
        if ((numsc .ge. 36.and.numsc .le. 53).or.
     *      (numsc .ge. 79.and.numsc .le. 81).or.                       010700
     *      (numsc .ge. 54.and.numsc .le. 56).or.
     *      numsc .lt. 0) icmsta(2) = 0
CC
CC----turn of quit (icmsta(3))
CC
CC      none currently
CC
CC----turn off next (icmsta(4))
CC
        if (numsc .eq. -3.or.(numsc .eq. 56.and.(iswopt(32) .eq. 2.or.  071196
     *      iswopt(32) .eq. 3)))                                        071196
     *      icmsta(4) = 0                                               071196
CC
CC----turn off back (icmsta(5))
CC
        if(numsc .eq. 1.or.numsc .eq. 36.or.numsc .eq. 52.or.           071196
     *     numsc .eq. -2.or.(numsc .eq. 56.and.                         021098
     *     (iswopt(32) .eq. 1.or.iswopt(32) .eq. 3)))                   071196
     *     icmsta(5) = 0                                                071196
CC
CC----turn off enter (icmsta(6))
CC
        if (numsc .eq. -1.or.numsc.eq. -2.or.numsc.eq. -3.or.           010700
     *      (numsc .ge. 36.and.numsc .le. 56).or.                       010700
     *      (numsc .ge. 79.and.numsc .le. 81) )                         010700
     *      icmsta(6) = 0                                               121098
CC
CC----turn off return (icmsta(7))
CC
        icmsta(7) = 0
        if ((numsc .ge. 36.and.numsc .le. 56) .or.
     *      (numsc .ge. 79.and.numsc .le. 81) .or.                      010700
     *      numsc .eq. -1.or.numsc.eq. -2.or.numsc .eq. -3)             121098
     *      icmsta(7) = 2                                               121098
CC
CC----icmsta(8) not currently used
CC
        icmsta(8) = 0
      else
        do 20 n=1,8
          icmsta(n) = 0
   20   continue
      end if
      return
      end
CCPCITE
