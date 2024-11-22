      Function IORAN(norec,nobuf,iofil)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IORAN Version 01.1                          ---
CC--- Subroutine description - writes buffer to random access file. ---
CC---------------------------------------------------------------------
CC
CC     norec - record number
CC     nobuf - output record buffer
CC     iofil - logical units number of output file
CC
CC     returns IORAN - 0 - no errors.
CC                    >0 - write error number.
CC
CC**********************************************************************
CC
      include 'OCNTRL.INC'
CC
      dimension nobuf(128)
CC
CC**********************************************************************
CC
      IORAN = 0
      WRITE (iofil,rec=norec,iostat=ier,err=10) nobuf
      go to 20
   10 IORAN = ier
   20 return
      end
