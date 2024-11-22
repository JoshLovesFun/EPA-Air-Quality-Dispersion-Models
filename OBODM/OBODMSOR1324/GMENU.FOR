CCPCITS
      Subroutine GMENU
CC                        05/12/95
CC
CC     Program to write OBODM data base screen menu template file
CC     OBODMENU.OBD
CC
CC     version 1.0
CC
CC     OBODMENU program code by:
CC     J. R. Bjorklund,  H. E. Cramer Co., Inc.
CC     P.O. Box 411, Sandy, UT 84091-0411 (801) 561-4964.
CC
CC     This program assumes each screen menu template file is named
CC     screen##, where ## is 01 to 99.  The second and third
CC     characters of the first menu row contain the screen menu
CC     number 01 to 99 and the fourth and fifth characters contain
CC     the number of different data value sets for this menu that
CC     are to be stored in the OBODM work data file.  The sixth and
CC     seventh characters give the data category.
CC         number           category
CC            1            initial menu
CC            2            data files menu
CC            3            main menu
CC            4            receptor data menus
CC            5            source data menus
CC            6            met. data menus
CC            7            control data menus
CC            8            graphics data menus
CC            9            help menus
CC           10            data units menus
CC           11            default fuels menus
CC           12            default files menus
CC           13            setup menu
CC           14            misc
CC
CC     All menu templates are written to file OBODMENU.OBD.  Also, if
CC     debug is on (ifdbug = 1), file ftn22 is created to show the
CC     relative contents of file OBODMENU.OBD.  However, the last 25
CC     records written to ftn22 are actually the first 25 written to
CC     OBODMENU.OBD.  These 25 records contain control data (nmxscn,
CC     numscs, numsce, nrecsc, nrecda, nrecdt, nsubsc, nkeybd, iback1,
CC     ifore1, iback2, ifore2, iversn, nmxrow, nmxcol, nmxlin, nmxfld,
CC     iflrjs).  The record number of each record in OBODMENU.OBD is
CC     given by the first five characters, followed by ' - ', in file
CC     ftn22.
CC
CC**********************************************************************
CC
      character*1 mblnk1,mlbrk,mrbrk,mzero,mscrn
      character*6 mbuf1
      character*8 mfile
      character*84 mbuf2
      character*84 mscren
CC
CC      ireint - control record written to the first record of the
CC               idisc file.  includes:nmxscn[1],numscs[20],numsce[20],
CC               nrecsc[100],nrecda[100],nrecdt[100],nsubsc[100],
CC               nkeybd[2*28],iback1[1],ifore1[1],iback2[1],ifore2[1],
CC               iversn[1],nmxrow[1],nmxcol[1],nmxlin[1],nmxfld[1],
CC               iflrjs[1]
CC               where [] indicates the number of 4 byte words.
CC      mscren - output buffer for rows of screen menus.
CC      mscrn  - menu buffer.
CC      iline  - array giving the menu row numbers that contain data
CC               fields. if iline(k) > 0, then the row given by
CC               mscrn(1,iline(k)) contains 1 or more data fields.
CC      itabs  - array giving the start columns of each data field in
CC               the menu. mscrn(itabs(k,l),iline(k))
CC      ispac  - array giving the number of columns in each data field
CC               in the menu. mscrn(itabs(k,l)+ispac(k,l)-1,iline(k))
CC               gives the last column in the data field.
CC      numscs - array of start menus numbers for each data category.
CC      numsce - array of end menu numbers for each data category.
CC      nrecsc - disc start record for screen menu.
CC      nrecda - disc start record in work data file for screen menu.
CC      nrecdt - number of work file records per data set for screen
CC               menu.
CC      nsubsc - number of data sets for screen menu.
CC      nkeybd - keyboard key numbers for paging, cursor control, etc.
CC      iback1 - text background color.
CC      ifore1 - text fore ground color.
CC      iback2 - data field background color.
CC      ifore2 - data field foreground color.
CC      iversn - program version number multiplied by 10.
CC      nmxrow - maximum number of rows per menu.
CC      nmxcol - maximum number of columns per menu.
CC      nmxlin - maximum number of data lines per menu.
CC      nmxfld - maximum number of data fields per data line.
CC      iflrjs - left (=1) or right (=0) justified editing.
CC
      dimension ireint(507),mscren(25),mscrn(84,25),iline(20),
     *          itabs(20,10),ispac(20,10),numscs(20),numsce(20),
     *          nrecsc(100),nrecda(100),nrecdt(100),nsubsc(100),
     *          nkeybd(2,28)
CC
      equivalence (nmxscn,ireint(1)),(numscs(1),ireint(2)),
     *            (numsce(1),ireint(22)),(nrecsc(1),ireint(42)),
     *            (nrecda(1),ireint(142)),(nrecdt(1),ireint(242)),
     *            (nsubsc(1),ireint(342)),(nkeybd(1,1),ireint(442)),
     *            (iback1,ireint(498)),(ifore1,ireint(499)),
     *            (iback2,ireint(500)),(ifore2,ireint(501)),
     *            (iversn,ireint(502)),(nmxrow,ireint(503)),
     *            (nmxcol,ireint(504)),(nmxlin,ireint(505)),
     *            (nmxfld,ireint(506)),(iflrjs,ireint(507)),
     *            (mbuf2,mscren(1),mscrn(1,1))
CC
      data mblnk1/' '/,mlbrk/'['/,mrbrk/']'/,mzero/'0'/,
CC      set maximum number of 4 byte words in array ireint & program
CC      version number.
     *     numwrd/507/,iversd/10/,iflrjs/1/,
     *     numscs/20*100/,
     *     numsce/20*0/
CC    the pairs of values in nkeybd are: enter, F1, F2, F3, F4, F5, F6,
CC    F7, F8, F9, F10, PgUp, PgDn, Up Aro, Dn Aro, Lf Tab, Rt Tab,
CC    Lf Aro, Rt Aro, Del, Ins, Ctrl k, Backsp, Home, End, Esc,
CC    Ctrl Home, Ctrl End
CC
CC      PC keyboard
      data nkeybd/1,13,0,59,0,60,0,61,0,62,0,63,0,64,0,65,0,66,0,67,
     *            0,68,0,73,0,81,0,72,0,80,0,15,1,09,0,75,0,77,0,83,
     *            0,82,1,11,1,8,0,71,0,79,1,27,0,119,0,117/
CC
CC>     HP9000 700/92, 2645A keyboard
CC>   data nkeybd/0,13,0,112,0,113,0,114,0,115,0,116,0,117,0,118,0,119,
CC>  *            0,120,0,121,0,86,0,85,0,68,0,67,0,65,0,66,0,9,0,80,
CC>  *            0,81,0,26,1,11,1,8,0,71,0,79,1,27,0,119,0,117/
CC
CC**********************************************************************
CC
CC      set maximum number of rows per menu.
      nmxrow = 25
CC      set maximum number of columns per menu.
      nmxcol = 80
CC      add 4 for length of data block.
      nmxblk = nmxcol + 4
CC      maximum number of lines containing data within a menu.
      nmxlin = 20
CC      maximum number of data fields per line.
      nmxfld = 10
CC      text background color.
      iback1 = 1
CC      data background color.
      iback2 = 4
CC      text foreground color.
      ifore1 = 7
CC      data foreground color.
      ifore2 = 14
CC      program version number.
      iversn = iversd
CC      debug flag. 0 = no, 1 = yes.
      ifdbug = 0
CC      counter giving the maximum number of data fields per menu
CC      for all menus processed.
      nmxln = 0
CC      counter giving the maximum number of screen menus processed.
      nmxscn = 0
CC      logical unit number of the OBODMENU.OBD template file.
      idsfil = 21
      open (unit=idsfil,file='OBODMENU.OBD',iostat=ier,err=130,
     *      status='unknown',access='direct',form='unformatted',
     *      recl=nmxblk)
CC
      write (*,9015)
CC      counter for screen menu number being processed.
      iscren = 0
CC      number of disc records required to write control data in array
CC      ireint.
      idsrec = (numwrd + 20) / 21
CC      initialize workfile record pointer.
      jdsrec = 0
CC      increment the screen menu number.
   10 iscren = iscren + 1
CC      set to process 99 screens.
      if (iscren .le. 99) then
CC        place screen menu number in buffer.
        write (mfile,9000) iscren
CC        open the respective screen menu file.
        open (unit=20,file=mfile,iostat=ier,err=20,status='old')
        go to 30
CC          error opening screen menu file
   20   if (ifdbug .ne. 0) then
          write (*,9014) ier,mfile
          write (*,9017)
          stop
        end if
        go to 10
CC        initialize data line count.
   30   nvl = 0
CC        set all data lines off.
        i = JINIT(iline,nmxlin,0,0)
CC        set all start data columns off.
        i = JINIT(itabs,nmxfld*nmxlin,0,0)
CC        loop over screen menu rows.
        do 70 j=1,nmxrow
CC          blank out row of output buffer.
          mscren(j) = mblnk1
CC          read row from input file into menu buffer starting
CC          at charcter 5.
          read (20,9002,err=160,end=170) (mscrn(i,j),i=5,nmxblk)
CC          if first row.
          if (j .eq. 1) then
CC            move control info into buffer
            call MVCH1(mscrn(6,j),mbuf1,6)
CC            blank out first and fourth through seventh control chars.
            mscrn(5,j) = mblnk1
            call MBLK1(mscrn(8,j),4)
CC            get screen menu template number and number of data sets
CC            possible for this menu template.
CC            numsc - screen menu template number.
CC            numda - number of sets of data associated with menu.
CC            isetc - data category number (receptor, source, etc.)
            read (mbuf1,9001) numsc,numda,isetc
CC            set start and end menus for category isetc.
            numscs(isetc) = min0(numscs(isetc),numsc)
            numsce(isetc) = max0(numsce(isetc),numsc)
CC            set disc record number for screen numsc.
            nrecsc(numsc) = idsrec + 1
CC            set number of sets of data associated with menu.
            nsubsc(numsc) = numda
CC            set maximum screens processed.
            nmxscn = max0(nmxscn,numsc)
          end if
CC          get number of non-blank characters in row.
          n = NUMC1(mscrn(5,j),nmxcol)
CC          if row is non-blank.
          if (n .gt. 0) then
CC            set row number and number of columns.
            write (mbuf1,9003) j,n
CC            put row number and number of columns in menu buffer
CC            columns one to four.
            call MVCH1(mbuf1,mscrn(1,j),4)
CC            see if data possible for this row, look for [.
            if (numda .gt. 0.and.ifcmp(mscrn(5,j),nmxcol,mlbrk,1,0)
     *          .gt. 0) then
CC              increment data line count.
              nvl = nvl + 1
              if (nvl .gt. nmxlin) go to 140
CC              count maximum data lines.
              nmxln = max0(nmxln,nvl)
CC              save line number of row containing data field(s).
              iline(nvl) = j
CC               initialize data field counter.
              ld = 0
CC              loop over characters in data line.
   40         k = 0
   50         k = k + 1
              if (k .lt. nmxcol) then
CC                look for each occurrence of [.
                if (mscrn(k+4,j) .eq. mlbrk) then
CC                  increment data field counter.
                  ld = ld + 1
                  if (ld .gt. nmxfld) go to 150
CC                  save start column of data field.
                  itabs(nvl,ld) = k + 1
CC                  initialize number of columns in data field.
                  ispac(nvl,ld) = 1
CC                  loop over data field columns, looking for ].
                  m = k
   60             m = m + 1
                  if (m .le. nmxcol) then
CC                    look for ].
                    if (mscrn(m+4,j) .eq. mrbrk) then
CC                      save number of columns in field.
                      ispac(nvl,ld) = m - k - 1
CC                      reset search index.
                      k = m
                    else
                      go to 60
                    end if
                  end if
                end if
                go to 50
              end if
            end if
CC            increment the disc file record number.
            idsrec = idsrec + 1
            nrec = idsrec
CC            write row of screen menu to output file.
            write (idsfil,rec=nrec,iostat=ier,err=180) mscren(j)
            if (ifdbug .ne. 0) then
              write (22,9004) nrec,mscren(j)
            end if
          end if
   70   continue
CC
        call MBLK1(mscren,nmxblk)
CC        indicate end of screen menu template.
        do 80 i=1,4
          mscrn(i,1) = mzero
   80   continue
CC        increment output record counter.
        idsrec = idsrec + 1
        nrec = idsrec
CC        write end of screen menu record.
        write (idsfil,rec=nrec,iostat=ier,err=180) mscren(1)
        if (ifdbug .ne. 0) then
          write (22,9004) nrec,mscren(1)
        end if
CC        if data field lines, add after template lines.
        if (nvl .gt. 0) then
CC          follow screen menu rows with data field lines used for
CC          generating work file.
          do 100 i=1,nvl
            call MBLK1(mscren,nmxblk)
            ld = 0
CC            determine number of fields set.
            do 90 j=1,nmxfld
              if (itabs(i,j) .gt. 0) ld = ld + 1
   90       continue
CC            write data fields row number, start column, number of
CC            columns and error flag field.
            write (mbuf2,9005) iline(i),(itabs(i,j),ispac(i,j),j=1,ld)
CC            increment output record counter.
            idsrec = idsrec + 1
            nrec = idsrec
            write (idsfil,rec=nrec,iostat=ier,err=180) mscren(1)
            if (ifdbug .ne. 0) then
              write (22,9004) nrec,mscren(1)
            end if
  100     continue
        end if
        call MBLK1(mscren,nmxblk)
CC        indicate end of this screen menu.
        do 110 i=1,6
          mscrn(i,1) = mzero
  110   continue
        idsrec = idsrec + 1
        nrec = idsrec
CC        write final record for current screen menu.
        write (idsfil,rec=nrec,iostat=ier,err=180) mscren(1)
        if (ifdbug .ne. 0) then
          write (22,9004) nrec,mscren(1)
        end if
        if (numda .le. 0.or.nvl .le. 0) then
CC          set start record for work data file to zero if no data fields.
          nrecda(numsc) = 0
        else
CC          set start record for work data file for this screen.
          nrecda(numsc) = jdsrec + 1
CC          set number of work file data lines for this screen
          nrecdt(numsc) = nvl
CC          increment work data file record pointer by number of data
CC          screens possible for this screen menu template.
          jdsrec = jdsrec + numda * nvl
        end if
        close (unit=20,status='keep')
        go to 10
      end if
      irec = 0
      ind = 0
CC      write out maximum number of screen menus and start records for
CC      menu templates and work data.
  120 irec = irec + 1
      ist = ind + 1
      if (ist .le. numwrd) then
        ind = ist + 20
        if (ind .gt. numwrd) ind = numwrd
        nrec = irec
        write (idsfil,rec=nrec,iostat=ier,err=180) (ireint(i),i=ist,ind)
        if (ifdbug .ne. 0) then
          write (22,9006) nrec,(ireint(i),i=ist,ind)
        end if
        go to 120
      end if
      x = idsrec * nmxblk
      x = x / 1000.0
      y = jdsrec * 90
      y = y / 1000.0
      write (*,9007) nmxscn,nmxln,idsrec,x,jdsrec,y
      if (nmxscn .gt. 0.and.nmxln .gt. 0.and.idsrec .gt. 0.and.
     *    jdsrec .gt. idsrec) then
        write (*,9016)
        close (unit=idsfil,status='keep')
        go to 200
      end if
      go to 190
  130 write (*,9008) ier
      go to 190
  140 write (*,9009) nvl,iscren,nmxlin
      go to 190
  150 write (*,9010) ld,iscren,nmxfld
      go to 190
  160 write (*,9011) ier,mfile
      go to 190
  170 write (*,9012) mfile
      go to 190
  180 write (*,9013) ier,nrec
  190 write (*,9017)
  200 write (*,9018)
      pause 'Type - Enter to continue.'
      return
CC
 9000 format ('SCREEN',i2.2)
CC>     HP9000
CC>?9000 format ('screen',i2.2)
 9001 format (3i2)
 9002 format (80a1)
 9003 format (2i2.2)
 9004 format (i5,'-',a84)
 9005 format (31i2.2)
 9006 format (i5,'-',21i5)
 9007 format (/' Maximum number of screen menu templates =',i5/
     *' Maximum number of data field lines per menu =',i5/
     *' Maximum number of records in file OBODMENU.OBD =',i5/
     *' (File size is ',f7.3,' k bytes)'/
     *' Maximum number of records in work file OBODWORK.OBD =',i5/
     *' (File size is ',f7.3,' k bytes)')
 9008 format (' Open error ',i6,' file OBODMENU.OBD')
 9009 format (' Error, max. number of data lines exceeded = ',i2,' for s
     *creen ',i2,', max = ',i2)
 9010 format (' Error, max. number of data fields exceeded = ',i2,' for
     *screen ',i2,', max = ',i2)
 9011 format (' Read error ',i6,' file ',a6)
 9012 format (' Read error, eof read on file ',a6)
 9013 format (' Write error ',i6,' on file OBODMENU.OBD at rec ',i6)
 9014 format (' Warning - Open error ',i6,' file ',a8)
 9015 format (/' Generating OBODM data base file OBODMENU.OBD, please wa
     *it.')
 9016 format (/' OBODM data base file OBODMENU.OBD generation complete.'
     *)
 9017 format (/' OBODM data base file OBODMENU.OBD is not complete and c
     *annot be used.')
 9018 format (/' End of Processing.')
      end
CCPCITE
