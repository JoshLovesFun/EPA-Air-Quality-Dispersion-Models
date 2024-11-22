      Function IOCPT(iptpn,iptrc,iptbf,incbf,iofil,mfile)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IOCPT Version 01.1                          ---
CC--- Subroutine description - increment buffer pointer and if      ---
CC---                          exceeds buffer limit, increment      ---
CC---                          record counter, write buffer and      ---
CC---                          reset pointer.                       ---
CC---------------------------------------------------------------------
CC
CC     iptpn - pointer to desired word in output buffer.
CC     iptrc - file record number
CC     iptbf - output buffer array
CC     incbf - pointer increment
CC     iofil - logical units number of data file
CC     mfile - file name
CC
CC     returns - IOCPT - 0 - if no errors.
CC                      >0 - if write error occurred.
CC
CC**********************************************************************
CC
      character*22 mfile
CC
      dimension iptbf(128)
CC
CC**********************************************************************
CC
      IOCPT = 0
      iptpn = iptpn + incbf
      if (iptpn .gt. 128) then
        IOCPT = IORAN(iptrc,iptbf,iofil)
        if (IOCPT .gt. 0) then
          ier = KERRS(2,mfile,IOCPT,iptrc,xdum,xdum,xdum,xdum,0)
          return
        end if
        iptrc = iptrc + 1
        iptpn = 1
      end if
      return
      end
