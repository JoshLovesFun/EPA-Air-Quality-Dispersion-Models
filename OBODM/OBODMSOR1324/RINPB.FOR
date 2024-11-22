      Subroutine RINPB
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - RINPB Version 01.2                          ---
CC--- Subroutine description - get disc data file names and open    ---
CC---                          data files.                          ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      character*1 mplus,mfnnm,mequl
CC
      character*1 mdumy
      integer*2 icol,irow,ibak,ifor
      character*1 mperd,mat,map
      character*7 mdnam
      character*12 msch1,msch2
      character*22 motsav,mfile,mname,msave
      character*58 mfnam,mfnms
CC
      dimension iofil(4),mfile(4),mfnnm(24)
CC
      include 'OCNTRL.INC'
      include 'ONAMES.INC'
      include 'OCDSPM.INC'
      include 'OCALCS.INC'
      include 'OWNTRL.INC'
CC
      equivalence (mfnam,mfnnm(1),mscrch(1)),(iofil(1),igtfil),
     *            (mfile(1),mgtnam),(mfnms,mscrch(59)),
     *            (ifrsd,functx(2))
CC
      data mperd/'.'/,mat/'@'/,map/'~'/,
     *     mdnam/'OBPRINT'/,mplus/'+'/,mequl/'='/,
     *     msch1/'SCRATCH1.OBD'/,msch2/'SCRATCH2.OBD'/
CC
CC**********************************************************************
CC
      ifrsd = 1
      if (ifrsav .eq. 1) go to 230
      msave = msvnam
      ifrsd = 0
      motsav = motnam
CC      start screen number.
      numsc = numscs(2)
      if (ifbtch .gt. 0) go to 40
CC      get input/output data file names.
   10 numf = IRDSC(numsc,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
      if (ncontr(2) .eq. 9999) then
        do 30 j=1,2
          do 20 i=1,2
            nvl = IWHER(j,ilinem,itabsm,ispacm,0,i,nln,nsc,nch)
            call MBLK1(mscrnm(nsc,nln),nch)
   20     continue
   30   continue
        ncontr(2) = 0
      end if
CC      display screen menu.
      iferrs = IOSCN(numsc,mscrnm,ilinem,itabsm,ispacm,iferrm,lstrw,0)
      if (iferrs .ne. 10) i = IWTSC(numsc,1,mscrnm,ilinem,iferrm)
      if (iferrs .eq. 1) go to 230
      ifbtch = 0
CC      set error counter to zero.
      ner = IFWER(iferrm,nmxlin*nmxfld)
      jfers = iferrs
CC
CC      check user response.
      if (iferrs .eq. 2) then
CC        if errors.
        if (ner .eq. 1) go to 10
CC        go to main menu.
        ifnext = 3
        go to 230
      else if (iferrs .ge. 3.and.iferrs .le. 7) then
CC        display aux. screens or current disc files.
        if (iferrs .eq. 7) then
          iflg = 1
        else
          iflg = 0
        end if
        iferrs = IFRSP(jfers,mscrnm,ilinem,itabsm,ispacm,iferrm,iflg)
        if (iferrs .eq. 1.or.iferrs .eq. 2) then
          if (iferrs .eq. 2.and.ner .eq. 1) go to 10
          if (iferrs .eq. 1) go to 230
          go to 230
        end if
        go to 10
      else if (iferrs .ge. 8.and.iferrs .le. 10) then
CC        n/a
        go to 10
      else if (iferrs .eq. 11) then
CC        go to previous menu.
        ifnext = 1
        go to 230
CC      see if page down.
      else if (iferrs .eq. 12.and.ifinit .eq. 0) then
CC        go to main menu without file open.
        ifnext = 3
        go to 230
      end if
CC      enter only or ifinit > 0.
      go to 70
   40 numf = IRDSC(numsc,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
      do 60 j=1,2
        do 50 i=1,2
          ibcrec = ibcrec + 1
          READ (ibcfil,9000,iostat=ier,err=210,end=220) mname
          nvl = IWHER(j,ilinem,itabsm,ispacm,0,i,nln,nsc,nch)
          call MVCH1(mname,mscrnm(nsc,nln),nch)
   50   continue
   60 continue
      iswopt(30) = 1
      iferrs = IOSCN(numsc,mscrnm,ilinem,itabsm,ispacm,iferrm,lstrw,0)
      iswopt(30) = 0
      ifbtch = 0    
      i = KERRS(33,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0)
      ifbtch = 1
      ifwrt = 4
      ishfil = 0                                                        030499
      inhfil = 0                                                        030499
      isgfil = 0                                                        030499
   70 i = JINIT(iferrm,nmxlin*nmxfld,0,0)
CC
CC      set data file names
      if (ifbtch .le. 0) ifwrt = 0
CC      get input file name.
      nvl = IWHER(0,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (ifbtch .le. 0) then
CC        see if batch file name
        ip = IFCMP(mscrnm(nsc,nln),nch,mequl,1,0)
        if (ip .gt. 0) then
          mscrnm(nsc+ip-1,nln) = mblnk1
          nbtchc = 0
          ifbtch = 0
          call MVCH1(mscrnm(nsc,nln),mbcnam,22)
          i = LRJST(mbcnam,22,1)                                        090498
          call ADEXT(mbcnam,1)                                          090498
          call FNAME(mfnam,58,mdirec,mbcnam)
          ibcfil = 19
          OPEN (unit=ibcfil,file=mfnam,iostat=ier,err=80,
     *          status='old',form='formatted')
          ifbtch = 1
          go to 40
   80     iferrm(nvl,1) = 1
          ifbtch = 0
          if (KERRS(3,mbcnam,ier,idum,xdum,xdum,xdum,xdum,0) .eq. 1)
     *        go to 230
          go to 10
        end if
      end if
CC      see if all names are to be set to input file name.
      ip = IFCMP(mscrnm(nsc,nln),nch,mplus,1,0)
      if (ip .gt. 0) then
CC        set all names the same.
        mscrnm(nsc+ip-1,nln) = mblnk1
        j = nsc
        k = nln
        ifwrt = 4
      end if
      mgtnam = mblnk1
      call MVCH1(mscrnm(nsc,nln),mgtnam,MIN0(nch,22))
      call ADEXT(mgtnam,1)
CC      get output save file name.
      nsc = JWHER(nvl,itabsm,ispacm,2,nch)
      msvnam = mblnk1
      if (ip .gt. 0) then
        call MVCH1(mscrnm(j,k),mscrnm(nsc,nln),nch)
      else
CC        see if remaining two names are set to output save file name.
        ip = IFCMP(mscrnm(nsc,nln),nch,mplus,1,0)
        if (ip .gt. 0) then
CC          set remaining names the same.
          mscrnm(nsc+ip-1,nln) = mblnk1
          j = nsc
          k = nln
          ifwrt = 3
        end if
      end if
      call MVCH1(mscrnm(nsc,nln),msvnam,MIN0(nch,22))
      call ADEXT(msvnam,1)
CC      get print output file name.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      motnam = mblnk1
      if (ip .gt. 0) then
        call MVCH1(mscrnm(j,k),mscrnm(nsc,nln),nch)
      else
        ip = IFCMP(mscrnm(nsc,nln),nch,mplus,1,0)
        if (ip .gt. 0) then
CC          set solution file name same as output file name.
          mscrnm(nsc+ip-1,nln) = mblnk1
          j = nsc
          k = nln
          ifwrt = 2
        end if
      end if
      call MVCH1(mscrnm(nsc,nln),motnam,MIN0(nch,22))
CC      see if need to default print output file name.
      if (motnam .eq. mblnk1) then
        if (msvnam .ne. mblnk1) then
          i = IFCMP(msvnam,22,mperd,1,0)
          if (i .le. 0) i = 23
          call MVCH1(msvnam,motnam,i-1)
          ifwrt = 1
        else if (mgtnam .ne. mblnk1) then
          i = IFCMP(mgtnam,22,mperd,1,0)
          if (i .le. 0) i = 23
          call MVCH1(mgtnam,motnam,i-1)
          ifwrt = 1
        else
          motnam = mblnk1
          call MVCH1(mdnam,motnam,7)
          ifwrt = 1
        end if
      end if
      if (ifwrt .eq. 1) then
        call MVCH1(motnam,mscrnm(nsc,nln),22)
        i = LRJST(mscrnm(nsc,nln),22,iflrjs)
      end if
      call ADEXT(motnam,2)
CC      get solution file name.
      nsc = JWHER(nvl,itabsm,ispacm,2,nch)
      mognam = mblnk1
      if (ip .gt. 0) then
        call MVCH1(mscrnm(j,k),mscrnm(nsc,nln),nch)
      end if
      call MVCH1(mscrnm(nsc,nln),mognam,MIN0(nch,22))
      call ADEXT(mognam,3)
      if (ifwrt .ne. 0) then
        ibak = ibackt
        ifor = iforet
        if (ifwrt .eq. 4.or.ifbtch .gt. 0) then
          nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
          icol = nsc - 2
          irow = nmxrow - nln
          call MBLK1(mfnnm,24)
          call MVCH1(mscrnm(nsc,nln),mfnnm(2),nch)
          i = LRJST(mfnnm(2),nch,iflrjs)
          mfnnm(1) = mat
          mfnnm(24) = map
          call IWRSC(icol,irow,24,mfnam,ibak,ifor)
        end if
        if (ifwrt .ge. 3.or.ifbtch .gt. 0) then
          nvl = IWHER(1,ilinem,itabsm,ispacm,0,2,nln,nsc,nch)
          icol = nsc - 2
          irow = nmxrow - nln
          call MBLK1(mfnnm,24)
          call MVCH1(mscrnm(nsc,nln),mfnnm(2),nch)
          i = LRJST(mfnnm(2),nch,iflrjs)
          mfnnm(1) = mat
          mfnnm(24) = map
          call IWRSC(icol,irow,24,mfnam,ibak,ifor)
        end if
        nvl = IWHER(2,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
        icol = nsc - 2
        irow = nmxrow - nln
        call MBLK1(mfnnm,24)
        call MVCH1(mscrnm(nsc,nln),mfnnm(2),nch)
        i = LRJST(mfnnm(2),nch,iflrjs)
        mfnnm(1) = mat
        mfnnm(24) = map
        call IWRSC(icol,irow,24,mfnam,ibak,ifor)
        if (ifwrt .ge. 2.or.ifbtch .gt. 0) then
          nvl = IWHER(2,ilinem,itabsm,ispacm,0,2,nln,nsc,nch)
          icol = nsc - 2
          irow = nmxrow - nln
          call MBLK1(mfnnm,24)
          call MVCH1(mscrnm(nsc,nln),mfnnm(2),nch)
          i = LRJST(mfnnm(2),nch,iflrjs)
          mfnnm(1) = mat
          mfnnm(24) = map
          call IWRSC(icol,irow,24,mfnam,ibak,ifor)
        end if
        i = IWTSC(numsc,1,mscrnm,ilinem,iferrm)
        ifwrt = 0
      end if
CC      left justify file names.
      do 90 j=1,4
        i = LRJST(mfile(j),22,1)
   90 continue
CC      open data files.
      ifcnvt = 0
      lunum = 22
      k = 0
      im = 0
      if (iofil(2) .gt. 0) then
        if (ifrdwt(1) .gt. 0.or.ifrdwt(2) .gt. 0.or.
     *      ifrdwt(3) .gt. 0.or.ifrdwt(4) .gt. 0) then
          im = 1
        end if
      end if
      do 190 j=1,2
        do 180 i=1,2
          lunum = lunum + 1
          k = k + 1
          if (iofil(k) .gt. 0) then
            if (k .eq. 2) then
              if (im .ne. 0) then
                n = 1
                if (ifsave .gt. 0.and.ifstco .eq. 0) then
  100             n = 0
                  iferrs = KERRS(8,msave,idum,idum,xdum,xdum,xdum,
     *                           xdum,0)
                  if (iferrs .eq. 3) then
                    n = 1
                  else if (iferrs .eq. 1.or.iferrs .eq. 2) then
                    go to 230
                  else if (iferrs .ne. 4) then
                    call IBELL(4)
                    go to 100
                  end if
                end if
                if (n .ne. 0) then
                  call STODT
                  ifsave = ifsave + 1
                end if
              end if
            end if
          end if
          if (iofil(k) .gt. 0) then
            if (k .ne. 3.or.(k .eq. 3.and.mfile(k) .ne. motsav)) then
              close (unit=iofil(k),err=110)
  110         if (k .eq. 1) then
                l = jinit(ifrdwt,4,0,0)
              else if (k .eq. 4) then
                ifrdwt(5) = 0
                ifrdwt(6) = 0
              end if
            end if
          end if
          iofil(k) = 0
          jflg = 0
          if (mfile(k)(1:1) .ne. mblnk1.and.mfile(k)(1:1) .ne. mperd)
     *        then
            jflg = 1
          else if (k .eq. 2) then
            jflg = 2
          end if
          if (jflg .eq. 1) then
            call MBLK1(mfnam,58)
            call FNAME(mfnam,58,mdirec,mfile(k))
          end if
          if (jflg .gt. 0) then
            if (k .eq. 1) then
              OPEN (unit=lunum,file=mfnam,iostat=ier,err=170,
     *              status='old',form='formatted')
              iofil(k) = lunum
              if (ifbtch .gt. 0) nbtchc = nbtchc + 1
            else if (k .eq. 2) then
              ifsave = 0
              if (jflg .eq. 1) call FNAME(mfnms,58,mdirec,mfile(k-1))
              if (mfnms .ne. mfnam.or.jflg .eq. 2) then
                if (jflg .eq. 1) then
                  if (ifstco .ne. 0.and.msvnam .eq. msch2) then
                    CLOSE (unit=lunum,status='delete',err=120)
                  else
                    CLOSE (unit=lunum,status='keep',err=120)
                  end if
  120            ifstco = 0
                 OPEN (unit=lunum,file=mfnam,iostat=ier,err=170,
     *                  status='unknown',form='formatted')
                else
                  msvnam = msch2
                  OPEN (unit=lunum,file=msch2,iostat=ier,err=170,
     *                  status='unknown',form='formatted')
                  ifstco = 1
                end if
                iofil(k) = lunum
              else
                iofil(k) = iofil(k-1)
              end if
            else if (k .eq. 3) then
              if (mfile(k) .ne. motsav) then
                OPEN (unit=lunum,file=mfnam,iostat=ier,err=170,
     *                status='unknown',form='formatted')
              end if
              iofil(k) = lunum
              if (ifbtch .eq. 0) then
                l = 0
  130           l = l + 1
                READ (iotfil,9000,err=140,end=140) mdumy
                go to 130
  140           l = l - 1
                if (l .gt. 0) then
                  m = KERRS(54,motnam,idum,idum,xdum,xdum,xdum,xdum,0)
                  if (m .eq. 3) then
                    l = 0
                  else
                    l = 1
                  end if
                else
                  l = 0
                end if
                if (l .eq. 0) then
                  REWIND (unit=iotfil,err=150)
                else
                  BACKSPACE (unit=iotfil,err=150)
                end if
  150           continue
              end if
            else if (k .eq. 4) then
              if (ifstch .ne. 0.and.mognam .eq. msch1) then
                CLOSE (unit=lunum,status='delete',err=160)
              else
                CLOSE (unit=lunum,status='keep',err=160)
              end if
  160         ifstch = 0
              OPEN (unit=lunum,file=mfnam,iostat=ier,err=170,
     *              status='unknown',access='direct',
     *              form='unformatted',recl=512)
              iofil(k) = lunum
            end if
          else
            if (k .eq. 4) then
                mognam = msch1
CC                open scratch solution/graphics file.
                OPEN (unit=lunum,file=msch1,iostat=ier,err=170,
     *                status='unknown',access='direct',
     *                form='unformatted',recl=512)
                iofil(k) = lunum
                ifstch = 1
            end if
          end if
          go to 180
  170     iferrm(j,i) = 1
          if (k .eq. 2.and.ifstco .ne. 0) ifstco = 0
          if (k .eq. 4.and.ifstch .ne. 0) ifstch = 0
          if (KERRS(3,mfile(k),ier,idum,xdum,xdum,xdum,xdum,0) .eq. 1)
     *        go to 230
          if (ifbtch .ne. 0) then
            ifbtch = 0
            if (KERRS(3,mfile(k),ier,idum,xdum,xdum,xdum,xdum,0) .eq. 1)
     *        go to 230       
            go to 220
          end if
          go to 10
  180   continue
  190 continue
CC      if errors.
      if (IFWER(iferrm,nmxlin*nmxfld) .eq. 1) then
        if (ifbtch .gt. 0) then
          iferrs = 1
          go to 230
        else
          do 200 i=1,4
            if (iofil(i) .gt. 0) CLOSE (unit=iofil(i),err=200)
  200     continue
          go to 10
        end if
      end if
CC      go get data from input save files.
      if (ifinit .eq. 0.and.igtfil .gt. 0.and.ifbtch .eq. 0) then
        i = KERRS(32,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0)
      end if
      ifrsd = 1
      go to 230
  210 if (KERRS(1,mbcnam,ier,idum,xdum,xdum,xdum,xdum,0) .eq. 1)
     *    go to 230
  220 iferrs = 1
  230 return
CC
 9000 format (a22)
      end
      Subroutine ADEXT(mname,indx)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - ADEXT Version 01.0                          ---
CC--- Subroutine description - add extension to file name.          ---
CC---                          data files.                          ---
CC---------------------------------------------------------------------
CC
CC     mname - file name.
CC     indx  - extension index.
CC
CC**********************************************************************
CC
      character*1 mperd
      character*4 mext
      character*22 mname
CC
      dimension mext(3)
CC
      include 'ONAMES.INC'
CC
      data mperd/'.'/
CCPCITS
      data mext/'.INP','.OUT','.SOL'/
CCPCITE
CCOMP1S
c     data mext/'.inp','.out','.sol'/
CCOMP1E
CC
CC**********************************************************************
CC
      if (mname .ne. mblnk1) then
        i = IFCMP(mname,22,mperd,1,0)
        if (i .eq. 0) then
          j = LRJST(mname,22,1)
          if (j .lt. 22) then
            mname(j+1:j+MIN0(22-j,4)) = mext(indx)
          end if
          i = LRJST(mname,22,0)
        end if
      end if
      return
CC
 9000 format (a1)
      end
