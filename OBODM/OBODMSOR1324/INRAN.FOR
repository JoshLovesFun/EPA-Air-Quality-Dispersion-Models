      Function INRAN(norec,nobuf,iofil)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - INRAN Version 01.1                          ---
CC--- Subroutine description - reads a random access file record.   ---
CC---------------------------------------------------------------------
CC
CC     norec - record number
CC     nobuf - output record buffer
CC     iofil - logical units of input file.
CC
CC     returns - INRAN - 0 - no errors.
CC                      <0 - negative of read error number.
CC
CC**********************************************************************
CC
      include 'OCNTRL.INC'
CC
      dimension nobuf(128)
CC
CC**********************************************************************
CC
      INRAN = 0
      i = JINIT(nobuf,128,0,0)
CCPCITS
      READ (iofil,rec=norec,iostat=ier,err=20,end=10) nobuf
CCPCITE
CCOMP1S
c     READ (iofil,rec=norec,iostat=ier,err=20) nobuf
CCOMP1E
      go to 30
CCPCITS
   10 ier = 6501
CCPCITE
   20 INRAN = -ier
   30 return
      end
