CCPCITS
      interface to integer*4 Function IFCTC[C](signum)
      integer*2 signum
      end
CC      Signal support
      interface to integer Function SIGNALQQ[c,loadds,alias:'_signal']
     *                                      (SIGNAL, HANDLER)
      integer*2 SIGNAL[value]
      external HANDLER
      end
      Function IRSBK()
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IRSBK Version 01.1                          ---
CC--- Subroutine description - resets break to call IFCTC if a      ---
CC---                          CTRL C is typed.                     ---
CC---------------------------------------------------------------------
CC
CC*********************************************************************
CC
CC      Signal support
      integer SIG$ERR,SIG$INT,SIGNALQQ[EXTERN]   ! SIGNALQQ return value
      parameter (SIG$ERR   = -1)
      parameter (SIG$INT   =  2)
      external IFCTC
CC
CC*********************************************************************
CC
CC      Signal support
      IRSBK = SIGNALQQ(SIG$INT,IFCTC)
      return
      end
CCPCITE
