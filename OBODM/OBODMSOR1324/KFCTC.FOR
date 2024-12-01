      Function KFCTC(jcols,jcole,jrow)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - KFCTC Version 01.1                          ---
CC--- Subroutine description - Determines what user wants to do     ---
CC---                          after typing CTRL C. when ifccok     ---
CC---                          equals 1.                            ---
CC---------------------------------------------------------------------
CC
CC     jcols - data field start column.
CC     jcole - data field end column.
CC     jrow  - data field row number.
CC
CC     returns - KFCTC - 1 - to activate quit? N/Y [N] pop up menu.
CC
CC*********************************************************************
CC
      integer*2 icol,irow,ibak,ifor,icha,ione,jcols,jcole,jrow
CC
      include 'OCNTRL.INC'
      include 'ONAMES.INC'
CC
      data ione/1/
CC
CC*********************************************************************
CC
      KFCTC = 1
CCPCITS
CC      blank out data field to remove ^C generated by system.
      if (jcols .ge. 0) then
        icha = ichar('?')
        ibak = ibackd
        ifor = ifored
        irow = jrow
        do 10 i=jcols,jcole
          icol = i
          call QCMOV(icol,irow)
          if (i-jcols .gt. 1) icha = ichar(' ')
          call QSTXT(icha,ibak,ifor,ione)
   10   continue
        if (jcols .eq. jcole) then
          icha = ichar(']')
          icol = jcole + 1
          call QCMOV(icol,irow)
          ibak = ibackt
          ifor = iforet
          call QSTXT(icha,ibak,ifor,ione)
        end if
      end if
CCPCITE
      return
      end
