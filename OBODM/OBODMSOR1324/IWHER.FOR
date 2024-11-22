      Function IWHER(nline,iline,itabs,ispac,linc,ival,nln,nsc,nch)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IWHER Version 01.0                          ---
CC--- Subroutine description - returns row number on which data are ---
CC---                          found and nln, nsc and nch           ---
CC---------------------------------------------------------------------
CC
CC    nline - value of previous data row index.
CC    iline - array of row numbers containing data fields.
CC    itabs  - array of start columns for data fields.
CC    ispac - array of number of columns in data fields.
CC    linc  - data row index increment (decrement).
CC    ival  - number of data field on row.
CC    nln   - returned line or row index for array mscrn.
CC    nsc   - returned column index for array mscrn.
CC    nch   - returned number of columns in data field for array mscrn.
CC
CC    returns - nline + linc, nln, nsc and nch.
CC
CC**********************************************************************
CC
      dimension iline(20),itabs(20,10),ispac(20,10)
CC
CC**********************************************************************
CC
CC      data line index.
      IWHER = nline + linc
CC      row index in mscrn.
      nln = iline(IWHER)
CC      column index in mscrn.
      nsc = itabs(IWHER,ival)
CC      number of columns in mscrn data field.
      nch = ispac(IWHER,ival)
      return
      end
