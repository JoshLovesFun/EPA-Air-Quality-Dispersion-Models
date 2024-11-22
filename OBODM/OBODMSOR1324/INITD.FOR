      Function INITD(numrs,norec)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - INITD Version 01.1                          ---
CC--- Subroutine description -  initialize disk records for ifmode  ---
CC---                           = 1, summary mode.                  ---
CC---------------------------------------------------------------------
CC
CC    numrs - number of records to write.
CC    norec - start record number.
CC
CC    returns - INITD
CC          0 - records written.
CC         >0 - error, error number returned.
CC
CC**********************************************************************
CC
      include 'OCNTRL.INC'
CC
CC**********************************************************************
CC
      INITD = 0
      i = JINIT(iptbuf,128,0,0)
      jorec = norec - 1
      do 10 i=1,numrs
        jorec = jorec + 1
CC        write record.
        ier = IORAN(jorec,iptbuf,iogfil)
        if (ier .gt. 0) go to 20
   10 continue
      go to 30
   20 INITD = ier
   30 return
      end
