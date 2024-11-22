      Function JDSKR(numwd,norec,array)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - JDSKR Version 01.1                          ---
CC--- Subroutine description - reads disk data and stores in array. ---
CC---                                                               ---
CC---------------------------------------------------------------------
CC
CC    numwd - number of words to read.
CC    norec - starting record number.
CC    array - array to store data into.
CC
CC    returns - JDSKR
CC          0 - data read ok.
CC         <0 - error, returns negative of error number.
CC
CC**********************************************************************
CC
      dimension array(20000),xrcbuf(128)
CC
      include 'OCNTRL.INC'
CC
      equivalence (ircbuf(1),xrcbuf(1))
CC
CC**********************************************************************
CC
      JDSKR = 0
      jorec = norec - 1
      ien = 0
   10 ist = ien + 1
      if (ist .le. numwd) then
        ien = ist + 127
        if (ien .gt. numwd) ien = numwd
        jorec = jorec + 1
CC        read record.
        ier = INRAN(jorec,ircbuf,iogfil)
        if (ier .lt. 0) go to 30
        do 20 i=ist,ien
          array(i) = xrcbuf(i-ist+1)
   20   continue
        go to 10
      end if
      go to 40
   30 JDSKR = ier
   40 return
      end
