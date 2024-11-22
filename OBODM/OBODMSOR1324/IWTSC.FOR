      Function IWTSC(numsc,numwk,mscrn,iline,iferr)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IWTSC Version 01.1                          ---
CC--- Subroutine description - write work data for screen menu numsc---
CC---------------------------------------------------------------------
CC
CC    numsc - screen number to write.
CC    numwk - index of work file data under screen number numsc.
CC    mscrn - nmxcol by nmxrow array to write screen from.
CC    iline - array containing the line numbers on which data are found.
CC    iferr - array containing error flags for each data field on each
CC            line.
CC
CC    returns - IWTSC
CC                0 - no errors.
CC               >0 - errors.
CC
CC    common used -
CC    nxmscn - maximum number of screen menus possible.
CC    nrecda - array containing the start record for each screen
CC             menu data record in the work data file mwknam.
CC
CC**********************************************************************
CC
      integer*2 i,j
CC
      character*1 mscrn,mdumy
      character*90 mbfrs
CC
      dimension mscrn(80,25),iline(20),iferr(20,10)
CC
      include 'OCNTRL.INC'
      include 'ONAMES.INC'
CC
CC**********************************************************************
CC
      IWTSC = 0
      if (numsc .gt. 0.and.numsc .le. nmxscn) then
        if (nrecda(numsc) .gt. 0) then
CC          start record in work file for menu data.
          irec = nrecda(numsc) - 1 + (numwk - 1) * nrecdt(numsc)
CC          loop over possible data field lines.
          do 10 j=1,nmxlin
            call MBLK1(mbfrs,90)
CC            if data for this line.
            if (iline(j) .gt. 0) then
CC              write error flags
              WRITE (mbfrs,9000,iostat=ier,err=20) (iferr(j,i),i=1,
     *                                              nmxfld)
              i = LRJST(mbfrs,90,0)
CC              move line to output buffer.
              mdumy = mscrn(1,iline(j))
              mscrn(1,iline(j)) = malpha(1)
              call MVCH1(mscrn(1,iline(j)),mbfrs,nmxcol)
              mscrn(1,iline(j)) = mdumy
CC            write data record.
              irec = irec + 1
              WRITE (iwkfil,rec=irec,iostat=ier,err=20) mbfrs
            end if
   10     continue
        end if
      end if
      go to 30
   20 iferrs = KERRS(2,mwknam,ier,irec,xdum,xdum,xdum,xdum,0)
      if (ier .eq. 6422.or.ier .eq. 913.or.ier .eq. 956) then
        i = KERRS(25,mwknam,ier,irec,xdum,xdum,xdum,xdum,0)
      end if
   30 return
 9000 format (10i1)
      end
