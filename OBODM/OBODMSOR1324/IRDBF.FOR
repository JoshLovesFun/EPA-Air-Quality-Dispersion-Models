      Function IRDBF(mbfin,nchrs,value)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IRDBF Version 01.1                          ---
CC--- Subroutine description - read internal buffer and return      ---
CC---                          numeric result in value.             ---
CC---------------------------------------------------------------------
CC
CC    mbfin - character string containing numeric value.
CC    nchrs - number of characters in mbufin.
CC    value - returned numeric value.
CC
CC    returns - IRDBF
CC               0 - no errors.
CC              >0 - read error number.
CC              -1 - x in field, value set to rmissd and string mbfin
CC                   blanked out.
CC              -2 - c in field, value set to rmissc and string mbfin
CC                   set to right justified c.
CC              -3 - field is blank, value returned as zero.
CC
CC**********************************************************************
CC
      character*1 mbfin,mblnk,mxchr,mcchr
      character*20 mbfrs
CC
      dimension mbfin(20)
CC
      data mblnk/' '/,mxchr/'X'/,mcchr/'C'/,rmissd/-1.0e20/,
     *     rmissc/-1.0e21/
CC
CC********************************************************************
CC
      call MBLK1(mbfrs,20)
      call MVCH1(mbfin,mbfrs,nchrs)
      IRDBF = -3
      if (mbfrs .ne. mblnk) then
CC        see if field contains an x.
        if (IFCMP(mbfin,nchrs,mxchr,1,1) .ne. 0) then
CC          x for delete value.
          call MBLK1(mbfin,nchrs)
          value = rmissd
          IRDBF = -1
        else if (IFCMP(mbfin,nchrs,mcchr,1,1) .ne. 0) then
CC          c for compute value.
          call MBLK1(mbfin,nchrs)
          mbfin(nchrs) = mcchr
          value = rmissc
          IRDBF = -2
        else
          i = LRJST(mbfrs,20,0)
          READ (mbfrs,9000,iostat=IRDBF,err=10) xdumy
          value = xdumy
          IRDBF = 0
        end if
      else
        IRDBF = -3
        value = 0.0
      end if
   10 return
CC
 9000 format (f20.0)
      end
