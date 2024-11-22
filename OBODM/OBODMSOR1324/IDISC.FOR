      Function IDISC(numsc,mscrn,iline,itabs,ispac,iferr)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IDISC Version 01.2                          ---
CC--- Subroutine description - display current directory data file  ---
CC---                          names.                               ---
CC---------------------------------------------------------------------
CC
CC    numsc - screen number for current disc file names menu template
CC            for receptor, source and meteorology data files.
CC    mscrn - screen menu display buffer.
CC    iline - array of line numbers containing data fields.
CC    itabs - array of start columns of each data field.
CC    ispac - array of number of columns in each field.
CC    iferr - array of erroneous data flags for each field.
CC
CC    returns - IDISC -
CC              0 - default file name data set, return to current menu.
CC              1 - terminate.
CC              2 - main menu.
CC              3 - return to current menu.
CC
CC**********************************************************************
CC
      integer*2 jfset,j,nn,numf,nl,k,nlns
CC
      character*1 mscrn,mdumy,mperd,mast
      character*4 mextn
      character*22 mname,mlabl
CC
      dimension mscrn(80,25),iline(20),itabs(20,10),ispac(20,10),
     *          iferr(20,10),jflgs(12,3),ifset(4),mname(50,3),
     *          namen(3),mextn(3)
CC
      include 'OCNTRL.INC'
      include 'ONAMES.INC'
      include 'OCALCS.INC'
      include 'OWNTRL.INC'
CC
      equivalence (mdumy,mdirec),(jflgs(1,1),ixlpnt(1,1)),
     *            (namen(1),ixlpnt(37,1)),(mname(1,1),ixlpnt(40,1))
CC
CCPCITS
      data mextn/'.INP','.OUT','.SOL'/
CCPCITE
CCOMP1S
c     data mextn/'.inp','.out','.sol'/
CCOMP1E
      data mlabl/'*** No Files Found ***'/,mperd/'.'/,mast/'*'/
CC
CC**********************************************************************
CC
CC      display current disc files.
CC
      jfset = 0
CC      initialize flags to indicate if numscs(2) screen menu file names
CC      have been changed or not.
      i = JINIT(ifset,4,0,0)
      i = JINIT(namen,3,0,0)
CC
CC      get file names ending with the file type extensions .inp, .out,
CC      and .sol.
   10 do 40 i=1,3
        call MBLK1(mname(1,i),22*50)
        call FILES(0,mdumy,mname(1,i),namen(i),mextn(i))
CC        if the first argument to subroutine FILES equals 0, then
CC        subroutine FILES scans the current working disc directory for
CC        all file names that end with the file extension type given
CC        by mextn(i).  the file names are returned starting in array
CC        mname(1,i) and the number of names is returned in namen(i).
        if (namen(i) .le. -10) namen(i) = namen(i) + 10
        if (mdirec(1) .ne. mblnk1) then
          call FILES(1,mdirec,mname(namen(i)+1,i),n,mextn(i))
CC          if the first argument to subroutine FILES equals 1, then
CC          subroutine FILES scans the disc directory given by mdirec
CC          for all file names that end with the file extension type
CC          given by mextn.  the file names are returned starting in
CC          array mname(namen(i)+1,i).  the number of names is returned
CC          in n.
          if (n .le. -10) n = n + 10
          if (n .gt. 0) then
            do 20 j=1,n
              k = LRJST(mname(namen(i)+j,i),22,0)
              if (k .gt. 1) then
                mname(namen(i)+j,i)(k-1:k-1) = mast
              end if
   20       continue          
          end if
          namen(i) = namen(i) + n
        end if
        if (namen(i) .gt. 0) then
          do 30 j=1,namen(i)
            l = IFNUL(mname(j,i),22)
            k = LRJST(mname(j,i),22,0)
   30     continue
        end if
   40 continue
CC
CC      if we have file names.
      nn = 0
CC        If at end, turn off NEXT.                                     071196
   50 iswopt(32) = 0                                                    071196
      if (nn+12 .ge. namen(1).and.nn+12 .ge. namen(2).and.              071196
     *    nn+12 .ge. namen(3)) then                                     071196
        iswopt(32) = 2                                                  071196
      end if                                                            071196
CC        If at start, turn off BACK                                    071196
      if (nn .eq. 0) then                                               071196
        if (iswopt(32) .eq. 0) then                                     071196
          iswopt(32) = 1                                                071196
        else                                                            071196
          iswopt(32) = 3                                                071196
        end if                                                          071196
      end if                                                            071196
CC      screen numsc, input, output and solution file names.
      numf = IRDSC(numsc,1,mscrn,iline,itabs,ispac,iferr,0)             071196
CC      start - 1.
      nl = nn
CC      display up to 12 names.
      do 60 nvl=1,12
CC        increment file names counter.
        nn = nn + 1
CC        fill screen menu file names.
        nln = iline(nvl)
CC        input file name.
        nsc = JWHER(nvl,itabs,ispac,1,nch)
        if (nn .le. namen(1)) then
          call MVCH1(mname(nn,1),mscrn(nsc-23,nln),22)
          if (ifset(1) .eq. nn) then
            if (ifset(2) .eq. nn) then
              mscrn(nsc,nln) = malpha(2)
            else
              mscrn(nsc,nln) = malpha(7)
            end if
          end if
        else if (namen(1) .le. 0.and.nvl .eq. 1) then
          call MVCH1(mlabl,mscrn(nsc-23,nln),22)
        end if
CC        output file name.
        nsc = JWHER(nvl,itabs,ispac,2,nch)
        if (nn .le. namen(2)) then
          call MVCH1(mname(nn,2),mscrn(nsc-23,nln),22)
          if (ifset(3) .eq. nn) then
            mscrn(nsc,nln) = malpha(19)
          end if
        else if (namen(2) .le. 0.and.nvl .eq. 1) then
          call MVCH1(mlabl,mscrn(nsc-23,nln),22)
        end if
CC        solution file name.
        nsc = JWHER(nvl,itabs,ispac,3,nch)
        if (nn .le. namen(3)) then
          call MVCH1(mname(nn,3),mscrn(nsc-23,nln),22)
          if (ifset(4) .eq. nn) then
            if (ifset(1) .le. 0.and.ifset(2) .le. 0) then
              mscrn(nsc,nln) = malpha(7)
            else
              mscrn(nsc,nln) = malpha(19)
            end if
          end if
        else if (namen(3) .le. 0.and.nvl .eq. 1) then
          call MVCH1(mlabl,mscrn(nsc-23,nln),22)
        end if
   60 continue
CC      display screen menu.
      IDISC = IOSCN(numsc,mscrn,iline,itabs,ispac,iferr,lstrw,0)
CC      check users response.
      if (IDISC .eq. 1) then
CC        terminate.
        go to 110
      else if (IDISC .eq. 2) then
CC        go to main menu.
        ifnext = 3
        go to 110
      else if (IDISC .eq. 3) then
CC        return to current menu.
        go to 110
      else if (IDISC .ge. 4.and.IDISC .le. 10) then
CC        n/a
        nn = nl
      else if (IDISC .eq. 11) then
CC        go back to previous group of names.
        nn = MAX0(nl - 12,0)
        if (nl .le. 0) call IBELL(1)
      else if (IDISC .eq. 0) then
CC        see if any files selected.
        i = JINIT(iferr,nmxlin*nmxfld,0,0)
        i = JINIT(jflgs,36,0,0)
        k = 0
CC        loop over menu lines.
        do 80 nvl=1,12
          nlns = iline(nvl)
CC          loop over data fields.
          do 70 i=1,3
            nsc = JWHER(nvl,itabs,ispac,i,nch)
CC            if field is set.
            if (mscrn(nsc,nlns) .ne. mblnk1) then
CC              if 's' for save data file.
              if (IFCHR(mscrn(nsc,nlns),19) .ne. 0) then
CC                output save file.
                jflgs(nvl,i) = 2
CC              if 'g' for get data file.
              else if (IFCHR(mscrn(nsc,nlns),7) .ne. 0) then
CC                input get file.
                jflgs(nvl,i) = 1
              else if (IFCHR(mscrn(nsc,nlns),2) .ne. 0) then
CC                both get and save file.
                jflgs(nvl,i) = 3
              else
CC                error.
                iferr(nvl,i) = 1
                jflgs(nvl,i) = 0
              end if
            else
              jflgs(nvl,i) = 0
              if (i .eq. 1) then
                if (ifset(1) .eq. nl+nvl) ifset(1) = 0
                if (ifset(2) .eq. nl+nvl) ifset(2) = 0
              else if (i .eq. 2) then
                if (ifset(3) .eq. nl+nvl) ifset(3) = 0
              else
                if (ifset(4) .eq. nl+nvl) ifset(4) = 0
              end if
            end if
            if (jflgs(nvl,i) .ne. 0) k = 1
   70     continue
   80   continue
CC        if errors.
        if (IFWER(iferr,nmxlin*nmxfld) .eq. 1) then
          nn = nl
          go to 50
        end if
CC        if files selected.
        if (k .ne. 0) then
CC          read data files screen menu.
          numf = IRDSC(numscs(2),1,mscrn,iline,itabs,ispac,iferr,1)
CC          loop over line flags.
          do 100 i=1,12
            k = i + nl
CC            loop over data field flags.
            do 90 j=1,3
CC              if file selected.
              if (jflgs(i,j) .ne. 0) then
                if (j .eq. 1) then
                  if (jflgs(i,j) .eq. 1.or.jflgs(i,j) .eq. 3) then
CC                    input get.
                    nvl = IWHER(1,iline,itabs,ispac,0,1,nln,nsc,nch)
                    ifset(1) = k
                  else
CC                    input save.
                    nvl = IWHER(1,iline,itabs,ispac,0,2,nln,nsc,nch)
                    ifset(2) = k
                  end if
                else if (j .eq. 2) then
CC                  print output.
                  nvl = IWHER(2,iline,itabs,ispac,0,1,nln,nsc,nch)
                  ifset(3) = k
                else
CC                  solution data.
                  nvl = IWHER(2,iline,itabs,ispac,0,2,nln,nsc,nch)
                  ifset(4) = k
                end if
CC                move file name into screen menu data field.
                n = IFCMP(mname(i+nl,j),nch,mperd,1,0)
                if (n .gt. 0) then
                  nch = n - 1
                  call MBLK1(mscrn(nsc,nln),22)
                end if
                call MVCH1(mname(i+nl,j),mscrn(nsc,nln),MIN0(nch,22))
                l = LRJST(mscrnm(nsc,nln),nch,iflrjs)
CC                indicate file name changed.
                jfset = jfset + 1
                if (j .eq. 1.and.jflgs(i,j) .eq. 3) then
                  nscs = nsc
                  nlns = nln
                  nvl = IWHER(1,iline,itabs,ispac,0,2,nln,nsc,nch)
                  call MVCH1(mscrn(nscs,nlns),mscrn(nsc,nln),MIN0(nch,
     *                       22))
                  jfset = jfset + 1
                  ifset(2) = k
                end if
              end if
   90       continue
  100     continue
CC          write data file names back out to work file.
          i = IWTSC(numscs(2),1,mscrn,iline,iferr)
        else
          call IBELL(1)
        end if
        if (ISUMI(namen,3) .eq. 0) go to 110
        if (ifset(1) .gt. 0.and.ifset(2) .gt. 0.and.ifset(3) .gt. 0.and.
     *      ifset(4) .gt. 0) go to 110
        if (nn .ge. namen(1).and.nn .ge. namen(2).and.nn .ge.
     *      namen(3)) nn = nl
      else
CC        page down.
        if (nn .ge. namen(1).and.nn .ge. namen(2).and.nn .ge.
     *      namen(3)) then
          nn = 0
          call IBELL(1)
        end if
      end if
      go to 50
CC      if any names changed, return IDISC = 0.
  110 if (IDISC .ne. 1.and.IDISC .ne. 2) then
        if (jfset .gt. 0) then
          IDISC = 0
        else
          IDISC = 3
        end if
      end if
      iswopt(32) = 0                                                    071196
      return
      end
