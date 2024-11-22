      Function IRDSC(numsc,numwk,mscrn,iline,itabs,ispac,iferr,iflgs)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IRDSC Version 01.1                          ---
CC--- Subroutine description - read work screen given by numsc.     ---
CC---------------------------------------------------------------------
CC
CC    numsc - screen number to display.
CC    numwk - index of work file data under screen number numsc.
CC    mscrn - nmxcol by nmxrow array to store screen into.
CC    iline - array containing the line numbers on which data are found.
CC    itabs - array containing the start character position for each
CC            data parameter on each data line.
CC    ispac - array containing the number of characters for each data
CC            parameter for each data line.
CC    iferr - array of data field error flags.
CC            0 - no error.
CC            1 - data in field erroneous.
CC    iflgs - flag indicating whether or not the work file is read.
CC            0 - no
CC            1 - yes
CC
CC    returns - IRDSC
CC               0 - no errors, all data fields are blank.
CC              >0 - no errors, number of non-blank data fields.
CC
CC    common used -
CC    nxmscn - maximum number of screen menus possible.
CC    nrecsc - array containing the start record for each screen
CC             menu template in the screen data base file mdbnam.
CC    nrecda - array containing the start record for each screen
CC             menu data record in the work data file mwknam.
CC    nrecdt - array containing the number of data records per data
CC             set for each screen menu.
CC    nmxrow - maximum rows in menu.
CC    nmxcol - maximum columns in menu.
CC    nmxlin - maximum number of data lines in menu.
CC    nmxfld - maximum number of data fields per line.
CC
CC**********************************************************************
CC
      integer*2 i,j,n,irow
CC
      character*1 mscrn,mbuf1,mnull
      character*10 mbfer
      character*84 mbf84
      character*90 mbfrs
CC
      dimension mscrn(80,25),iline(20),itabs(20,10),ispac(20,10),
     *          iferr(20,10),mbuf1(90)
CC
      include 'OCNTRL.INC'
      include 'ONAMES.INC'
CC
      equivalence (mbfrs,mbf84,mbuf1(1)),(mbfer,mbuf1(81))
CC
CC**********************************************************************
CC
      mnull = CHAR(0)
      IRDSC = 0
CC      blank out screen menu array.
      call MBLK1(mscrn,nmxcol*nmxrow)
CC      zero out data field pointer arrays.
      i = JINIT(iline,nmxlin,0,0)
      i = JINIT(itabs,nmxlin*nmxfld,0,0)
      i = JINIT(ispac,nmxlin*nmxfld,0,0)
      i = JINIT(iferr,nmxlin*nmxfld,0,0)
      if (numsc .gt. 0.and.numsc .le. nmxscn) then
CC        get start record of screen menu template.
        irec = nrecsc(numsc) - 1
        n = 0
   10   irec = irec + 1
CC        read data base screen menu template file into buffer.
CCPCITS
        READ (idbfil,rec=irec,iostat=ier,err=60,end=50) mbf84
CCPCITE
CCOMP1S
c       READ (idbfil,rec=irec,iostat=ier,err=60) mbf84
CCOMP1E
CC        read row number and number of characters in row.
        READ (mbf84,9000,iostat=ier,err=60) irow,icol
CC        if screen menu or data field indicator record.
        if (irow .gt. 0.or.icol .gt. 0) then
          if (n .eq. 0) then
CC            move screen menu to screen array.
            call MVCH1(mbuf1(5),mscrn(1,irow),icol)
            go to 10
          else
CC            get where data fields are located within screen menu.
            n = n + 1
            READ (mbf84,9001,iostat=ier,err=60) iline(n-1),
     *            (itabs(n-1,k),ispac(n-1,k),k=1,nmxfld)
            go to 10
          end if
        else
CC          increment row counter for data fields.
          n = n + 1
          if (n .le. 1.and.nrecda(numsc) .gt. 0) go to 10
        end if
CC        if any data fields for this menu.
        if (nrecda(numsc) .gt. 0.and.n .gt. 2.and.iflgs .ne. 0.and.
     *      iwkfil .gt. 0) then
CC          start record in work file for menu data.
          irec = nrecda(numsc) - 1 + (numwk - 1) * nrecdt(numsc)
CC          loop over data field lines.
          do 40 j=1,nmxlin
CC            if data for this line.
            if (iline(j) .gt. 0) then
CC              read saved data from work file.
              irec = irec + 1
CCPCITS
              READ (iwkfil,rec=irec,iostat=ier,err=70,end=50) mbfrs
CCPCITE
CCOMP1S
c             READ (iwkfil,rec=irec,iostat=ier,err=70) mbfrs
CCOMP1E
              if (mbuf1(1) .eq. malpha(1)) then
                i = IFNUL(mbfrs,90)
                READ (mbfer,9002,iostat=ier,err=20) (iferr(j,i),i=1,
     *                nmxfld)
CC                loop over data fields.
   20           do 30 i=1,nmxfld
CC                  if data is in data field.
                  if (itabs(j,i) .gt. 0) then
CC                    move data field to screen menu.
                    call MVCH1(mbuf1(itabs(j,i)),mscrn(itabs(j,i),
     *                         iline(j)),ispac(j,i))
                    if (NUMC1(mscrn(itabs(j,i),iline(j)),ispac(j,i))
     *                  .gt. 0) IRDSC = IRDSC + 1
                    if (isystm .eq. 1) then
                      if (iferr(j,i) .eq. 1) then
                        mscrn(nmxcol-1,iline(j)) = malpha(5)
                      else if (iferr(j,i) .eq. 2) then
                        mscrn(nmxcol-1,iline(j)) = malpha(23)
                      end if
                    end if
                  else
CC                    no more data for this line.
                    go to 40
                  end if
   30           continue
              else
CC                no more data for this menu.
                go to 70
              end if
            end if
   40     continue
        end if
      end if
      go to 70
CCPCITS
   50 ier = 6501
CCPCITE
   60 iferrs = KERRS(1,mdbnam,ier,irec,xdum,xdum,xdum,xdum,0)
   70 return
CC
 9000 format (2i2)
 9001 format (i2,10(i2,i2))
 9002 format (10i1)
      end
