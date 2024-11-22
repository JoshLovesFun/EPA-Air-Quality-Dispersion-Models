      Function JWHER(nvl,itabs,ispac,ival,nch)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - JWHER Version 01.0                          ---
CC--- Subroutine description - returns data field start column      ---
CC---                          and number of columns in data field. ---
CC---------------------------------------------------------------------
CC
CC    nvl   - value data row index.
CC    itabs  - array of start columns for data fields.
CC    ispac - array of number of columns in data fields.
CC    ival  - number of data field on row.
CC    JWHER - returned column index for array mscrn.
CC    nch   - returned number of columns in data field for array mscrn.
CC
CC    returns - JWHER = itabs(nvl,ival) + 1, and nch.
CC
CC**********************************************************************
CC
      dimension itabs(20,10),ispac(20,10)
CC
CC**********************************************************************
CC
CC      column index in mscrn.
      JWHER = itabs(nvl,ival)
CC      number of columns in mscrn data field.
      nch = ispac(nvl,ival)
      return
      end
