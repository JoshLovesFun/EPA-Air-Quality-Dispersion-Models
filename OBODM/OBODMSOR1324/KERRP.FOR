      Function KERRP(ier,ifprt)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - KERRP Version 01.1                          ---
CC--- Subroutine description - sets up call to error/warn function  ---
CC---                          KERRS                                ---
CC---------------------------------------------------------------------
CC
CC    ier   - error number.  see function KERRS for the error number
CC            details.
CC    ifprt - print to CRT or output print file.
CC            0 - CRT
CC            1 - print file
CC
CC    returns - KERRP = 0 no errors or problems.
CC                      1 f1 entered, terminate.
CC                      2 f2 or esc entered.
CC                      3 Yes reply entered.
CC                      4 No reply entered.
CC                     <0 value read and retured, use abs value.
CC
CC**********************************************************************
CC
      character*22 mdumy
CC
      include 'OCNTRL.INC'
      include 'ONAMES.INC'
CC
CC********************************************************************
CC
      do 20 i=1,nmxlin
        do 10 j=1,nmxfld
          if (iferrm(i,j) .gt. 0) then
            nvl = iwher(i,ilinem,itabsm,ispacm,0,j,nln,nsc,nch)
            mdumy = mblnk1
            call mvch1(mscrnm(nsc,nln),mdumy,nch)
            if (iferrm(i,j) .eq. 1) then
              ier = 22
            else
              ier = 23
            end if
            KERRP = KERRS(ier,mdumy,idum,idum,xdum,xdum,xdum,xdum,ifprt)
            if (KERRP .eq. 1.or.KERRP .eq. 2) go to 30
          end if
   10   continue
   20 continue
   30 return
      end
