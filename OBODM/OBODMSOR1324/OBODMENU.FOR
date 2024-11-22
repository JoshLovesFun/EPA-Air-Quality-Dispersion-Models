      Program OBODMENU
cc                        05/12/95
cc
cc     Program to write OBODM data base screen menu template file
cc     OBODMENU.OBD
cc
cc     version 1.0
cc
cc     OBODMENU program code by:
cc     J. R. Bjorklund,  H. E. Cramer Co., Inc.
cc     P.O. Box 411, Sandy, UT 84091-0411 (801) 561-4964.
cc
cc     This program assumes each screen menu template file is named
cc     screen##, where ## is 01 to 99.  The second and third
cc     characters of the first menu row contain the screen menu
cc     number 01 to 99 and the fourth and fifth characters contain
cc     the number of different data value sets for this menu that
cc     are to be stored in the OBODM work data file.  The sixth and
cc     seventh characters give the data category.
cc         number           category
cc            1            initial menu
cc            2            data files menu
cc            3            main menu
cc            4            receptor data menus
cc            5            source data menus
cc            6            met. data menus
cc            7            control data menus
cc            8            graphics data menus
cc            9            help menus
cc           10            data units menus
cc           11            default fuels menus
cc           12            default files menus
cc           13            setup menu
cc
cc     All menu templates are written to file OBODMENU.OBD.  Also, if
cc     debug is on (ifdbug = 1), file ftn22 is created to show the
cc     relative contents of file OBODMENU.OBD.  However, the last 25
cc     records written to ftn22 are actually the first 25 written to
cc     OBODMENU.OBD.  These 25 records contain control data (nmxscn,
cc     numscs, numsce, nrecsc, nrecda, nrecdt, nsubsc, nkeybd, iback1,
cc     ifore1, iback2, ifore2, iversn, nmxrow, nmxcol, nmxlin, nmxfld,
cc     iflrjs).  The record number of each record in OBODMENU.OBD is
cc     given by the first five characters, followed by ' - ', in file
cc     ftn22.
cc
cc**********************************************************************
cc
      character*1 mblnk1,mlbrk,mrbrk,mzero,mscrn
      character*6 mbuf1
      character*8 mfile
      character*84 mbuf2
      character*84 mscren
cc
cc      ireint - control record written to the first record of the
cc               idisc file.  includes:nmxscn[1],numscs[20],numsce[20],
cc               nrecsc[100],nrecda[100],nrecdt[100],nsubsc[100],
cc               nkeybd[2*28],iback1[1],ifore1[1],iback2[1],ifore2[1],
cc               iversn[1],nmxrow[1],nmxcol[1],nmxlin[1],nmxfld[1],
cc               iflrjs[1]
cc               where [] indicates the number of 4 byte words.
cc      mscren - output buffer for rows of screen menus.
cc      mscrn  - menu buffer.
cc      iline  - array giving the menu row numbers that contain data
cc               fields. if iline(k) > 0, then the row given by
cc               mscrn(1,iline(k)) contains 1 or more data fields.
cc      itabs  - array giving the start columns of each data field in
cc               the menu. mscrn(itabs(k,l),iline(k))
cc      ispac  - array giving the number of columns in each data field
cc               in the menu. mscrn(itabs(k,l)+ispac(k,l)-1,iline(k))
cc               gives the last column in the data field.
cc      numscs - array of start menus numbers for each data category.
cc      numsce - array of end menu numbers for each data category.
cc      nrecsc - disc start record for screen menu.
cc      nrecda - disc start record in work data file for screen menu.
cc      nrecdt - number of work file records per data set for screen
cc               menu.
cc      nsubsc - number of data sets for screen menu.
cc      nkeybd - keyboard key numbers for paging, cursor control, etc.
cc      iback1 - text background color.
cc      ifore1 - text fore ground color.
cc      iback2 - data field background color.
cc      ifore2 - data field foreground color.
cc      iversn - program version number multiplied by 10.
cc      nmxrow - maximum number of rows per menu.
cc      nmxcol - maximum number of columns per menu.
cc      nmxlin - maximum number of data lines per menu.
cc      nmxfld - maximum number of data fields per data line.
cc      iflrjs - left (=1) or right (=0) justified editing.
cc
      dimension ireint(507),mscren(25),mscrn(84,25),iline(20),
     *          itabs(20,10),ispac(20,10),numscs(20),numsce(20),
     *          nrecsc(100),nrecda(100),nrecdt(100),nsubsc(100),
     *          nkeybd(2,28)
cc
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
cc
      data mblnk1/' '/,mlbrk/'['/,mrbrk/']'/,mzero/'0'/,
cc      set maximum number of 4 byte words in array ireint & program
cc      version number.
     *     numwrd/507/,iversd/10/,iflrjs/1/,
     *     numscs/20*100/,
     *     numsce/20*0/
cc    the pairs of values in nkeybd are: enter, F1, F2, F3, F4, F5, F6,
cc    F7, F8, F9, F10, PgUp, PgDn, Up Aro, Dn Aro, Lf Tab, Rt Tab,
cc    Lf Aro, Rt Aro, Del, Ins, Ctrl k, Backsp, Home, End, Esc,
cc    Ctrl Home, Ctrl End
cc
CCPCITS
      data nkeybd/1,13,0,59,0,60,0,61,0,62,0,63,0,64,0,65,0,66,0,67,
     *            0,68,0,73,0,81,0,72,0,80,0,15,1,09,0,75,0,77,0,83,
     *            0,82,1,11,1,8,0,71,0,79,1,27,0,119,0,117/
CCPCITE
CCOMP1S
c     data nkeybd/0,13,0,112,0,113,0,114,0,115,0,116,0,117,0,118,0,119,
c    *            0,120,0,121,0,86,0,85,0,68,0,67,0,65,0,66,0,9,0,80,
c    *            0,81,0,26,1,11,1,8,0,71,0,79,1,27,0,119,0,117/
CCOMP1E
cc
cc**********************************************************************
cc
cc      set maximum number of rows per menu.
      nmxrow = 25
cc      set maximum number of columns per menu.
      nmxcol = 80
cc      add 4 for length of data block.
      nmxblk = nmxcol + 4
cc      maximum number of lines containing data within a menu.
      nmxlin = 20
cc      maximum number of data fields per line.
      nmxfld = 10
cc      text background color.
      iback1 = 1
cc      data background color.
      iback2 = 4
cc      text foreground color.
      ifore1 = 7
cc      data foreground color.
      ifore2 = 14
cc      program version number.
      iversn = iversd
cc      debug flag. 0 = no, 1 = yes.
      ifdbug = 0
cc      counter giving the maximum number of data fields per menu
cc      for all menus processed.
      nmxln = 0
cc      counter giving the maximum number of screen menus processed.
      nmxscn = 0
cc      logical unit number of the OBODMENU.OBD template file.
      idsfil = 21
      open (unit=idsfil,file='OBODMENU.OBD',iostat=ier,err=130,
     *      status='unknown',access='direct',form='unformatted',
     *      recl=nmxblk)
cc
      write (*,9015)
cc      counter for screen menu number being processed.
      iscren = 0
cc      number of disc records required to write control data in array
cc      ireint.
      idsrec = (numwrd + 20) / 21
cc      initialize workfile record pointer.
      jdsrec = 0
cc      increment the screen menu number.
   10 iscren = iscren + 1
cc      set to process 99 screens.
      if (iscren .le. 99) then
cc        place screen menu number in buffer.
        write (mfile,9000) iscren
cc        open the respective screen menu file.
        open (unit=20,file=mfile,iostat=ier,err=20,status='old')
        go to 30
   20   if (ifdbug .ne. 0) then
cc          error opening screen menu file
          write (*,9014) ier,mfile
        end if
        go to 10
cc        initialize data line count.
   30   nvl = 0
cc        set all data lines off.
        i = JINIT(iline,nmxlin,0,0)
cc        set all start data columns off.
        i = JINIT(itabs,nmxfld*nmxlin,0,0)
cc        loop over screen menu rows.
        do 70 j=1,nmxrow
cc          blank out row of output buffer.
          mscren(j) = mblnk1
cc          read row from input file into menu buffer starting
cc          at charcter 5.
          read (20,9002,err=160,end=170) (mscrn(i,j),i=5,nmxblk)
cc          if first row.
          if (j .eq. 1) then
cc            move control info into buffer
            call MVCH1(mscrn(6,j),mbuf1,6)
cc            blank out first and fourth through seventh control chars.
            mscrn(5,j) = mblnk1
            call MBLK1(mscrn(8,j),4)
cc            get screen menu template number and number of data sets
cc            possible for this menu template.
cc            numsc - screen menu template number.
cc            numda - number of sets of data associated with menu.
cc            isetc - data category number (receptor, source, etc.)
            read (mbuf1,9001) numsc,numda,isetc
cc            set start and end menus for category isetc.
            numscs(isetc) = min0(numscs(isetc),numsc)
            numsce(isetc) = max0(numsce(isetc),numsc)
cc            set disc record number for screen numsc.
            nrecsc(numsc) = idsrec + 1
cc            set number of sets of data associated with menu.
            nsubsc(numsc) = numda
cc            set maximum screens processed.
            nmxscn = max0(nmxscn,numsc)
          end if
cc          get number of non-blank characters in row.
          n = NUMC1(mscrn(5,j),nmxcol)
cc          if row is non-blank.
          if (n .gt. 0) then
cc            set row number and number of columns.
            write (mbuf1,9003) j,n
cc            put row number and number of columns in menu buffer
cc            columns one to four.
            call MVCH1(mbuf1,mscrn(1,j),4)
cc            see if data possible for this row, look for [.
            if (numda .gt. 0.and.ifcmp(mscrn(5,j),nmxcol,mlbrk,1,0)
     *          .gt. 0) then
cc              increment data line count.
              nvl = nvl + 1
              if (nvl .gt. nmxlin) go to 140
cc              count maximum data lines.
              nmxln = max0(nmxln,nvl)
cc              save line number of row containing data field(s).
              iline(nvl) = j
cc               initialize data field counter.
              ld = 0
cc              loop over characters in data line.
   40         k = 0
   50         k = k + 1
              if (k .lt. nmxcol) then
cc                look for each occurrence of [.
                if (mscrn(k+4,j) .eq. mlbrk) then
cc                  increment data field counter.
                  ld = ld + 1
                  if (ld .gt. nmxfld) go to 150
cc                  save start column of data field.
                  itabs(nvl,ld) = k + 1
cc                  initialize number of columns in data field.
                  ispac(nvl,ld) = 1
cc                  loop over data field columns, looking for ].
                  m = k
   60             m = m + 1
                  if (m .le. nmxcol) then
cc                    look for ].
                    if (mscrn(m+4,j) .eq. mrbrk) then
cc                      save number of columns in field.
                      ispac(nvl,ld) = m - k - 1
cc                      reset search index.
                      k = m
                    else
                      go to 60
                    end if
                  end if
                end if
                go to 50
              end if
            end if
cc            increment the disc file record number.
            idsrec = idsrec + 1
            nrec = idsrec
cc            write row of screen menu to output file.
            write (idsfil,rec=nrec,iostat=ier,err=180) mscren(j)
            if (ifdbug .ne. 0) then
              write (22,9004) nrec,mscren(j)
            end if
          end if
   70   continue
cc
        call MBLK1(mscren,nmxblk)
cc        indicate end of screen menu template.
        do 80 i=1,4
          mscrn(i,1) = mzero
   80   continue
cc        increment output record counter.
        idsrec = idsrec + 1
        nrec = idsrec
cc        write end of screen menu record.
        write (idsfil,rec=nrec,iostat=ier,err=180) mscren(1)
        if (ifdbug .ne. 0) then
          write (22,9004) nrec,mscren(1)
        end if
cc        if data field lines, add after template lines.
        if (nvl .gt. 0) then
cc          follow screen menu rows with data field lines used for
cc          generating work file.
          do 100 i=1,nvl
            call MBLK1(mscren,nmxblk)
            ld = 0
cc            determine number of fields set.
            do 90 j=1,nmxfld
              if (itabs(i,j) .gt. 0) ld = ld + 1
   90       continue
cc            write data fields row number, start column, number of
cc            columns and error flag field.
            write (mbuf2,9005) iline(i),(itabs(i,j),ispac(i,j),j=1,ld)
cc            increment output record counter.
            idsrec = idsrec + 1
            nrec = idsrec
            write (idsfil,rec=nrec,iostat=ier,err=180) mscren(1)
            if (ifdbug .ne. 0) then
              write (22,9004) nrec,mscren(1)
            end if
  100     continue
        end if
        call MBLK1(mscren,nmxblk)
cc        indicate end of this screen menu.
        do 110 i=1,6
          mscrn(i,1) = mzero
  110   continue
        idsrec = idsrec + 1
        nrec = idsrec
cc        write final record for current screen menu.
        write (idsfil,rec=nrec,iostat=ier,err=180) mscren(1)
        if (ifdbug .ne. 0) then
          write (22,9004) nrec,mscren(1)
        end if
        if (numda .le. 0.or.nvl .le. 0) then
cc          set start record for work data file to zero if no data fields.
          nrecda(numsc) = 0
        else
cc          set start record for work data file for this screen.
          nrecda(numsc) = jdsrec + 1
cc          set number of work file data lines for this screen
          nrecdt(numsc) = nvl
cc          increment work data file record pointer by number of data
cc          screens possible for this screen menu template.
          jdsrec = jdsrec + numda * nvl
        end if
        close (unit=20,status='keep')
        go to 10
      end if
      irec = 0
      ind = 0
cc      write out maximum number of screen menus and start records for
cc      menu templates and work data.
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
      stop
cc
CCOMP1S
c9000 format ('screen',i2.2)
CCOMP1E
CCPCITS
 9000 format ('SCREEN',i2.2)
CCPCITE
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
     */' Now run program OBODMKEY.EXE for keyboard and color setup.')
 9017 format (/' OBODM data base file OBODMENU.OBD is not complete and c
     *annot be used.')
 9018 format (/' End of Processing.')
      end
      Subroutine MVCH1(mbufin,mbufot,nchrs)
cc
cc---------------------------------------------------------------------
cc--- Author organization- H. E. Cramer Co., Inc.                   ---
cc--- Subroutine code - MVCH1 version 01.00                         ---
cc--- Subroutine description - move characters from one string to   ---
cc---                          another.                             ---
cc---------------------------------------------------------------------
cc
cc        mbufin - character*1 input array to be moved.
cc        mbufot - character*1 output array to be filled.
cc        nchrs  - number of characters to move.
cc
cc********************************************************************
cc
      character*1 mbufin,mbufot
cc
      dimension mbufin(500),mbufot(500)
cc
cc********************************************************************
cc
      do 10 i=1,nchrs
        mbufot(i) = mbufin(i)
   10 continue
      return
      end
      Subroutine MBLK1(marray,nchrs)
cc
cc---------------------------------------------------------------------
cc--- Author organization- H. E. Cramer Co., Inc.                   ---
cc--- Subroutine code - MBLK1 version 01.00                         ---
cc--- Subroutine description - blank out character array.           ---
cc---------------------------------------------------------------------
cc
cc    marray - character array to be filled with blanks.
cc    nchrs  - number of blanks to fill.
cc
cc********************************************************************
cc
      character*1 marray,mblnk1
cc
      dimension marray(300)
cc
      data mblnk1/' '/
cc
cc*****************************************************************
cc
      do 10 i=1,nchrs
        marray(i) = mblnk1
   10 continue
      return
      end
      Function JINIT(iarray,n,init,iflg)
cc
cc---------------------------------------------------------------------
cc--- Author organization- H. E. Cramer Co., Inc.                   ---
cc--- Subroutine code - JINIT version 01.00                         ---
cc--- Subroutine description - initialize n locations of integer    ---
cc---                          array init, depending on iflg.       ---
cc---------------------------------------------------------------------
cc
cc        iflg = 0, init is single valued.
cc               1, init is array.
cc
cc**********************************************************************
cc
      dimension iarray(1000),init(1000)
cc
cc**********************************************************************
cc
      do 10 i=1,n
        j = 1
        if (iflg .ne. 0) j = i
        iarray(i) = init(j)
   10 continue
      JINIT = n
      return
      end
      Function NUMC1(marray,nchrs)
cc
cc---------------------------------------------------------------------
cc--- Author organization- H. E. Cramer Co., Inc.                   ---
cc--- Subroutine code - NUMC1 version 01.00                         ---
cc--- Subroutine description - return the index of the last non-    ---
cc---                          blank character in a string.         ---
cc---------------------------------------------------------------------
cc
cc        marray - character array.
cc        nchrs  - number of characters the array marray.
cc
cc        returns - NUMC1 -
cc                  0 = array all blank.
cc                 >0 = index number of last non-blank character.
cc
cc******************************************************************
cc
      character*1 marray(2000),mblnk1
cc
      data mblnk1/' '/
cc
cc*****************************************************************
cc
      NUMC1 = 0
      i = nchrs + 1
   10 if (i .gt. 1.and.NUMC1 .eq. 0) then
        i = i - 1
        if (marray(i) .ne. mblnk1) NUMC1 = i
        go to 10
      end if
      return
      end
      Function IFCHR(mchar,nchrs)
cc
cc---------------------------------------------------------------------
cc--- Author organization- H. E. Cramer Co., Inc.                   ---
cc--- Subroutine code - IFCHR version 01.00                         ---
cc--- Subroutine description - determine if mchar is an upper or    ---
cc---                          lower case character equal to the    ---
cc---                          nchrs'th character in the alphabet.  ---
cc---                                         or                    ---
cc---                          return in mchar the alpha character  ---
cc---                          with index equal to the absolute     ---
cc---                          value of nchrs.                      ---
cc---------------------------------------------------------------------
cc
cc        mchar  - character*1 character to compare or return.
cc        nchrs  - sequence number of character to compare against.
cc                 > 0 & < 27,  then compare mchar to upper and lower
cc                              case characters corresponding to
cc                              sequence number nchrs.
cc                 > -53 & < 0, then return the character with sequence
cc                              given by the absolute value of nchrs.
cc                              -1 to -26 is a to z,
cc                             -27 to -52 is a to z.
cc
cc        returns - IFCHR -
cc                  if nchrs > 0 -
cc                                0 = no compare.
cc                                1 = yes compare upper case.
cc                               -1 = yes compare lower case.
cc                  if nchrs < 0 -
cc                                0 = nchrs out of range no character
cc                                    returned.
cc                            nchrs = character determined by nchrs is
cc                                    returned in mchar.
cc
cc********************************************************************
cc
      character*1 malph,mchar
cc
      dimension malph(52)
cc
      data malph/'A','B','C','D','E','F','G','H','I','J','K','L','M',
     *           'N','O','P','Q','R','S','T','U','V','W','X','Y','Z',
     *           'a','b','c','d','e','f','g','h','i','j','k','l','m',
     *           'n','o','p','q','r','s','t','u','v','w','x','y','z'/
cc
cc********************************************************************
cc
      if (nchrs .gt. 0.and.nchrs .lt. 27) then
        if (mchar .eq. malph(nchrs)) then
          IFCHR = 1
        else if (mchar .eq. malph(nchrs+26)) then
          IFCHR = -1
        else
          IFCHR = 0
        end if
      else if (iabs(nchrs) .gt. 0.and.iabs(nchrs) .lt. 53) then
        IFCHR = nchrs
        mchar = malph(iabs(IFCHR))
      else
        IFCHR = 0
      end if
      return
      end
      Function ifalf(mchar)
cc
cc---------------------------------------------------------------------
cc--- Author organization- H. E. Cramer Co., Inc.                   ---
cc--- Subroutine code - ifalf version 01.00                         ---
cc--- Subroutine description - determine if mchar is upper or lower ---
cc---                          case alphabetic character.           ---
cc---------------------------------------------------------------------
cc
cc        mchar  - character*1 character to compare
cc
cc        returns - ifalf -
cc                  0 = not alphabetic
cc            1 to 26 = alphabetic sequence number of upper case
cc                      character.
cc          -1 to -26 = negative of alphabetic sequence number of lower
cc                      case character.
cc
cc********************************************************************
cc
      character*1 mchar
cc
cc********************************************************************
cc
      ifalf = 0
      i = 0
   10 if (i .lt. 26.and.ifalf .eq. 0) then
        i = i + 1
        ifalf = IFCHR(mchar,i) * i
        go to 10
      end if
      return
      end
      Function ifcmp(maray1,nchrs1,maray2,nchrs2,iflg)
cc
cc---------------------------------------------------------------------
cc--- Author organization- H. E. Cramer Co., Inc.                   ---
cc--- Subroutine code - ifcmp version 01.00                         ---
cc--- Subroutine description - compare two character strings to see ---
cc---                          if the second is imbedded within the ---
cc---                          first.                               ---
cc---------------------------------------------------------------------
cc
cc        maray1 - 1st character string to compare.
cc        nchrs1 - number of characters in maray1.
cc        maray2 - 2nd character string to compare, routine looks
cc                 for this string within maray1.
cc        nchrs2 - number of characters in maray2.
cc        iflg   - upper/lower case flag.
cc                 0 - compare as is, do not look for upper/lower case.
cc                 1 - if alphabetic, check both upper and lower case.
cc
cc        returns - ifcmp
cc                  0 - string maray2 not found in maray1.
cc                 >0 - sequence number of start of string maray2
cc                      in maray1.
cc
cc********************************************************************
cc
      character*1 maray1,maray2,m1,m2
cc
      dimension maray1(100),maray2(100)
cc
cc********************************************************************
cc
      ifcmp = 0
      j = 0
cc      set max. no. of characters to check in first array.
      n = nchrs1 - nchrs2 + 1
cc      loop over first array.
   10 if (j .lt. n.and.ifcmp .eq. 0) then
        j = j + 1
        i = 0
        k = j
cc        loop over second array.
   20   if (i .lt. nchrs2) then
          i = i + 1
cc          set characters to compare.
          m1 = maray1(k)
          m2 = maray2(i)
cc          if upper/lower case sensitive.
          if (iflg .ne. 0) then
cc            numberic value of characters. <0=lower case, >0=upper.
            l1 = ifalf(m1)
            l2 = ifalf(m2)
cc            if both characters are alphabetic.
            if (l1 .ne. 0.and.l2 .ne. 0) then
cc              if lower case, change to upper case.
              if (l1 .lt. 0) then
                l1 = IFCHR(m1,l1)
              end if
cc              if lower case, change to upper case.
              if (l2 .lt. 0) then
                l2 = IFCHR(m2,l2)
              end if
            end if
          end if
cc          compare characters.
          if (m1 .ne. m2) then
cc            not the same, end second loop.
            i = nchrs2
          else
cc            same, if last of second array, set start character of
cc            found string.
            if (i .eq. nchrs2) then
              ifcmp = j
              i = nchrs2
            else if (k .lt. nchrs1) then
cc              not last character, increment first array index.
              k = k + 1
            else
cc              first array exceeded.
              i = nchrs2
            end if
          end if
          go to 20
        end if
        go to 10
      end if
      return
      end
