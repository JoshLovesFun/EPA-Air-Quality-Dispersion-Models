      Function INCPT(iptpn,iptrc,iptbf,incbf,iofil,mfile,ifwrt)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - INCPT Version 01.1                          ---
CC--- Subroutine description - increment buffer pointer and if      ---
CC---                          exceeds buffer limit, increment      ---
CC---                          record counter, read buffer and      ---
CC---                          reset pointer.                       ---
CC---------------------------------------------------------------------
CC
CC     iptpn - pointer to desired word in input buffer.
CC     iptrc - file record number
CC     iptbf - input buffer array
CC     incbf - pointer increment
CC     iofil - logical units number of data file
CC     mfile - file name
CC     ifwrt - update old file record before reading new record
CC             0 - no
CC             1 - yes
CC
CC     returns - INCPT - 0 - if no errors.
CC                      <0 - negative of read error number.
CC                      >0 - write error number.
CC
CC**********************************************************************
CC
      character*22 mfile
CC
      dimension iptbf(128)
CC
CC**********************************************************************
CC
      INCPT = 0
      iptpn = iptpn + incbf
      if (iptpn .gt. 128) then
        if (ifwrt .ne. 0.and.iptrc .gt. 0) then
          INCPT = IORAN(iptrc,iptbf,iofil)
          if (INCPT .gt. 0) then
            ier = KERRS(2,mfile,INCPT,iptrc,xdum,xdum,xdum,xdum,0)
            return
          end if
        end if
        iptrc = iptrc + 1
        INCPT = INRAN(iptrc,iptbf,iofil)
        iptpn = 1
        if (INCPT .lt. 0) then
          ier = KERRS(1,mfile,IABS(INCPT),iptrc,xdum,xdum,xdum,xdum,0)
          return
        end if
      end if
      return
      end
