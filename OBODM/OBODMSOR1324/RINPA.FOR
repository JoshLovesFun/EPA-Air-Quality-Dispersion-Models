CCPCITS
      Interface to Subroutine RDSTA[C](idm1,idm2,idm3)
      integer*2 idm1[Reference],idm2[Reference],idm3[Reference]
      end
CCPCITE
      Subroutine RINPA
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - RINPA Version 01.1                          ---
CC--- Subroutine description - get default disc drive, directory.   ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
CCPCITS
      integer*2 idm1,idm2,idm3,idm4
CCPCITE
      character*1 mdum,mastr,mbufr,mesc,mlbrk,mzero,msemi,mfmt,         020797
     *            mschi,mnum
      character*2 mfms
      character*8 mfmts
      character*11 mbf10                                                090500
      character*12 mschr
      character*36 mfuel,mspec
      character*58 mfnam
      character*78 mbufs
CC
      dimension ireint(507),mbufr(80),mfmt(8),mschi(12),mnum(4)
CC
      include 'OCNTRL.INC'
      include 'ONAMES.INC'
      include 'OCALCS.INC'
      include 'OCDSPM.INC'
      include 'OWNTRL.INC'
CC
      equivalence (mbufs,mbufr(1)),(mfmt(1),mfmts),(mfms,mfmt(2)),
     *            (mschi(1),mschr)
CC      this equivalence assumes the arrays numscs(20), numsce(20),
CC      nrecsc(100), nrecda(100), nrecdt(100), nsubsc(100),
CC      nkeybd(2,28), ibackt, iforet, ibackd, ifored, verson, nmxrow,
CC      nmxcol, nmxlin, nmxfld, iflrjs in common 'OCNTRL.INC'.
      equivalence (nmxscn,ireint(1)),(ivers,verson),(mbf10,mnpbuf)
CC
      data numwd/507/,mastr/'*'/,mlbrk/'['/,mzero/'0'/,                 020797
     *     mfmts/'(  a1,\)'/,msemi/';'/,mschr/'SCRATCH .OBD'/,
     *     mnum/'1','2','3','4'/
CC
CC**********************************************************************
CC
      if (ifinit .lt. 0) then
        iferrs = IOSCN(0,mdum,idum,idum,idum,idum,idum,0)
CC        get time and date.
CCPCITS
        call GETTIM(idm1,idm2,idm3,idm4)
CCPCITE
CCOMP1S
c       call NTIME(iniyrs,inimon,inidys,idm1,idm2)
CCOMP1E
        inihrs = idm1 * 100 + idm2
        inimin = idm2
CCPCITS
        call GETDAT(idm1,idm2,idm3)
        iniyrs = idm1
        inimon = idm2
        inidys = idm3
CCPCITE
        injdys = 0
        call JULAN(iniyrs,inimon,inidys,injdys)
        irnyrs = iniyrs                                                 020797
        irnmon = inimon
        irndys = inidys
        irnhrs = inihrs
        irnjdy = injdys
CC        Determine value of code returned for no characters in input   020797
CC        buffer.  Assume no characters at this point.                  020797
        call RDSTA(idm1,idm2,idm3)                                      020797
        nochar = idm2                                                   020797
CC        delete any old scratch files.
        imxfil = 18
        do 11 i=1,4
          mschi(8) = mnum(i)
          OPEN (unit=imxfil,file=mschr,status='old',err=11)
          CLOSE (unit=imxfil,status='delete',err=11)
   11   continue
        imxfil = 0
CC        maximum number of screen menus.
CC>     nscmx = 74
CC        maximum disc storage required for OBODWORK.OBD
CC>     nbymx = 257
CC
CC      open screen menu template file.
        idbfil = 20
        OPEN (unit=idbfil,file=mdbnam,iostat=ier,err=230,status='old',
     *         access='direct',form='unformatted',recl=84)
        irec = 0
        ind = 0
CC        get maximum number of screen menus, menu disc file start
CC        records, work data disc file start records and version.
   10   irec = irec + 1
        ist = ind + 1
CC        numwd is the total dimension of nmxscn,numscs,numsce,nrecsc,
CC        nrecda,nrecdt,nsubsc,nkeybd,ibackt,iforet,ibackd,ifored,
CC        verson,nmxrow,nmxcol,nmxlin,nmxfld,iflrjs.
        if (ist .le. numwd) then
          ind = ist + 20
          if (ind .gt. numwd) ind = numwd
CCPCITS
          READ (idbfil,rec=irec,iostat=ier,err=270,end=260) (ireint(i),
     *                                                       i=ist,ind)
CCPCITE
CCOMP1S
c         READ (idbfil,rec=irec,iostat=ier,err=270) (ireint(i),
c    *                                               i=ist,ind)
CCOMP1E
          go to 10
        end if
        verson = ivers / 10.0
CC>     nscmx = 0
CC>     do 20 i=1,20
CC>       nscmx = MAX0(nscmx,numsce(i))
CC>20   continue
CC        maximum number of screens.
CC>     if (nscmx .le. 0) nscmx = 74
CC        maximum work file disc space K.
CC>     if (iflend .gt. 0) then
CC>       nbymx = (iflend * 90 + 999) / 1000
CC>     else
CC>       nbymx = 257
CC>     end if
CCPCITS
        mesc = CHAR(27)
        do 20 i=1,28
          n = 1
          mnpbuf(n) = mesc
          n = n + 1
          mnpbuf(n) = mlbrk
          if (nkeybd(1,i) .eq. 0) then
            n = n + 1
            mnpbuf(n) = mzero
            n = n + 1
            mnpbuf(n) = msemi
          end if
          xdumy = nkeybd(2,i)
          if (xdumy .gt. 0.0) then
            j = IWTBF(mbufr,5,xdumy,0,1)
            call MVCH1(mbufr,mnpbuf(n+1),j)
            n = n + j
            n = n + 1
            mnpbuf(n) = msemi
            if (nkeybd(1,i) .eq. 0) then
              n = n + 1
              mnpbuf(n) = mzero
              n = n + 1
              mnpbuf(n) = msemi
            end if
            call MVCH1(mbufr,mnpbuf(n+1),j)
            n = n + j
            n = n + 1
            j = IFCHR(mnpbuf(n),-42)
            WRITE (mfms,9002) n
            if (mfmt(2) .ne. mblnk1) then
              if (mfmt(2) .eq. mzero) then
                  mfmt(2) = mblnk1
              end if
            end if
            WRITE (*,mfmts) (mnpbuf(j),j=1,n)
          end if
   20   continue
CCPCITE
CC        initialize OBODM screen mode.
        ifccok = 1
        ifinit = 0
        iflg = 0
      else
        iflg = 1
      end if
CC      start screen number 01.
   30 numsc = numscs(1)
CC      display disc drive and data file directory menu.
   40 numf = IRDSC(numsc,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
      ifset = 0
      if (ncontr(1) .eq. 9999) then
        nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
        call MBLK1(mscrnm(nsc,nln),nch)
        ncontr(1) = 0
      end if
CC      display mod date on screen 1.                                   081298
      WRITE (mbf10,9003) versod,ivrmod
      call MVCH1(mbf10,mscrnm(58,25),11)                                090500
CC>   call MVCH!(mbf10,mscrnm(59,25),10)                                090500
      call MVCH1(moddat,mscrnm(70,25),8)                                081298
CC      display screen menu.
      iferrs = IOSCN(numsc,mscrnm,ilinem,itabsm,ispacm,iferrm,lstrw,0)
      if (iflg .ne. 0) then
CC        save screen menu.
        if (iferrs .ne. 10) then
          nvl = IWHER(2,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
          if (numc1(mscrnm(nsc,nln),nch) .gt. 0) then
            if (IFCHR(mscrnm(nsc,nln),25) .ne. 0) then
              ifset = 1
            end if
          end if
          mscrnm(nsc,nln) = malpha(14)
          i = IWTSC(numsc,1,mscrnm,ilinem,iferrm)
        end if
        ifwrt = 0
      else
        ifwrt = 1
      end if
CC
CC      if terminate.
      if (iferrs .eq. 1) go to 310
      if (iferrs .eq. 11) then
        go to 40
      end if
CC
CC      check user response.
      if (iferrs .eq. 2) then
CC        main menu.
        ifnext = 3
        if (iflg .ne. 0) go to 310
        go to 50
      else if (iferrs .ge. 3.and.iferrs .le. 7) then
CC        display help screens.
        iferrs = IFRSP(iferrs,mscrnm,ilinem,itabsm,ispacm,iferrm,0)
        if (iferrs .eq. 1.or.iferrs .eq. 2) then
          if (iferrs .eq. 1) go to 310
          go to 50
        else
          go to 40
        end if
      else if (iferrs .ge. 8.and.iferrs .le. 10) then
        go to 40
      end if
   50 i = JINIT(iferrm,nmxlin*nmxfld,0,0)
CC
CC      set default directory.
      nvl = IWHER(0,ilinem,itabsm,ispacm,1,1,nlns,nscs,nchs)
      call MBLK1(mdirec,36)
      call MVCH1(mscrnm(nscs,nlns),mdirec,MIN0(nchs,36))
CC      left justify directory pathname.
      i = LRJST(mdirec,36,1)
CCPCITS
      if (numc1(mdirec,36) .gt. 0) then
        if (IFPTH(mdirec,mscrnm) .le. 0) then
          iferrm(nvl,1) = 1
          ier = KERRS(15,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0)
        end if
      end if
CCPCITE
CC
CC      see if setup desired.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (IFCHR(mscrnm(nsc,nln),25) .ne. 0) then
        ifset = 1
      end if
CC
      if (IFWER(iferrm,nmxlin*nmxfld) .eq. 1) go to 40
CC
      if (iflg .eq. 0) then
CC        see if old work file exists.
        iwkfil = 21
        OPEN (unit=iwkfil,file=mwknam,iostat=ier,err=170,status='old',
     *        access='direct',form='unformatted',recl=90)
CC        old work file exists, see if want to initialize data from old
CC        work file.
   60   iferrs = KERRS(24,mdum,idum,idum,xdum,xdum,xdum,xdum,0)
        if (iferrs .eq. 1) go to 310
        if (iferrs .eq. 3) then
CC          answer yes to use old work file question.
          ifinit = 1
          ncontr(2) = 0
          go to 80
        else if (iferrs .ne. 4) then
          go to 60
        end if
CC        answer no to use old work file question.
        go to 170
CC
CC        old work file not found, open new work file.
   70   OPEN (unit=iwkfil,file=mwknam,iostat=ier,err=240,status='new',
     *        access='direct',form='unformatted',recl=90)
CC
CC        open fuels/explosives data file.
   80   ifdfil = 22
        OPEN (unit=ifdfil,file=mfdnam,iostat=ier,err=180,status='old',
     *        form='formatted')
CC        determine fuels/explosives disc start record.
   90   n = 0
        j = 0
        do 100 i=1,100
          if (nrecda(i) .gt. n) then
            k = nrecdt(i)
            n = nrecda(i)
            j = nsubsc(i)
          end if
  100   continue
CC        start record for fuels/explosives data in mwknam.
        iflstr = n + k * j + 1
CC>     nbymx = (90 * (iflstr + 200) + 999) / 1000
CC        read fuels/explosives data and write to work file.
        irec = 0
        jrec = iflstr
  110   irec = irec + 1
CC        fuel/explosive.
        READ (ifdfil,9000,iostat=ier,err=200,end=130) mfuel,dheat,dburn,
     *                                                nspec
        n = jrec
        nn = jrec + nspec + 1
        WRITE (iwkfil,rec=n,iostat=ier,err=250) mfuel,dheat,dburn,nspec,
     *                                          nn
        do 120 i=1,nspec
          irec = irec + 1
          READ (ifdfil,9001,iostat=ier,err=200,end=130) mspec,dsmol,
     *                                                  dsden,dhalf,
     *                                                  dfctr
          n = jrec + i
          WRITE (iwkfil,rec=n,iostat=ier,err=250) mspec,dsmol,dsden,
     *                                            dhalf,dfctr
        nl = n + 1
  120   continue
        jrec = nn
        go to 110
      end if
CC
  130 if (ifwrt .ne. 0) then
        nvl = IWHER(2,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
        mscrnm(nsc,nln) = malpha(14)
        i = IWTSC(numsc,1,mscrnm,ilinem,iferrm)
      end if
      iflend = nl
CC>   nbymx = (iflend * 90 + 999) / 1000
      if (ifinit .gt. 0) then
        i = KERRS(31,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0)
      end if
      if (ifset .eq. 1) then
        iflg = 1
        ker = 0
CC        start screen number 72.
        numsc = numscs(13)
CC        display setup menu.
  140   numf = IRDSC(numsc,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
        if (ker .eq. 0) then
          nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
          if (iflrjs .eq. 0) then
            mscrnm(nsc,nln) = malpha(18)
          else
            mscrnm(nsc,nln) = malpha(12)
          end if
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          mscrnm(nsc,nln) = malpha(ibackt+1)
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          mscrnm(nsc,nln) = malpha(ibackd+1)
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          mscrnm(nsc,nln) = malpha(iforet+1)
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          mscrnm(nsc,nln) = malpha(ifored+1)
        end if
CC        display screen menu.
        iferrs = IOSCN(numsc,mscrnm,ilinem,itabsm,ispacm,iferrm,lstrw,0)
        i = IWTSC(numsc,1,mscrnm,ilinem,iferrm)
        call MBLK1(mnpbuf,22)
        ifwrt = 0
CC
CC        if terminate.
        if (iferrs .eq. 1) go to 310
        if (iferrs .eq. 11) then
          go to 30
        end if
CC
CC        check user response.
        if (iferrs .eq. 2) then
CC          main menu.
          ifnext = 3
          go to 150
        else if (iferrs .ge. 3.and.iferrs .le. 7) then
CC          display help screens.
          iferrs = IFRSP(iferrs,mscrnm,ilinem,itabsm,ispacm,iferrm,0)
          if (iferrs .eq. 1.or.iferrs .eq. 2) then
            if (iferrs .eq. 1) go to 310
            go to 150
          else
            go to 140
          end if
        else if (iferrs .ge. 8.and.iferrs .le. 10) then
          go to 140
        end if
  150   i = JINIT(iferrm,nmxlin*nmxfld,0,0)
CC
CC        get edit mode.
        k = 0
        nvl = IWHER(0,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        if (IFCHR(mscrnm(nsc,nln),12) .ne. 0.or.mscrnm(nsc,nln) .eq.
     *      mblnk1) then
          iflrj = 1
        else if (IFCHR(mscrnm(nsc,nln),18) .ne. 0) then
          iflrj = 0
        else
          k = k + 1
          mnpbuf(k) = mscrnm(nsc,nln)
          iferrm(nvl,1) = 1
          ifwrt = -1
        end if
        if (iflrjs .ne. iflrj.and.ifwrt .eq. 0) ifwrt = 1
CC        set text background color.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        i = IFFOP(mscrnm(nsc,nln),8)
        if (i .ge. 0.and.i .le. 7) then
          ibact = i
        else
          k = k + 1
          if (k .gt. 1) then
            mnpbuf(k) = msemi
            k = k + 1
          end if
          mnpbuf(k) = mscrnm(nsc,nln)
          iferrm(nvl,1) = 1
          ifwrt = -1
        end if
        if (ibackt .ne. ibact.and.ifwrt .eq. 0) ifwrt = 1
CC        set data field background color.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        i = IFFOP(mscrnm(nsc,nln),8)
        if (i .ge. 0.and.i .le. 7) then
          ibacd = i
        else
          k = k + 1
          if (k .gt. 1) then
            mnpbuf(k) = msemi
            k = k + 1
          end if
          mnpbuf(k) = mscrnm(nsc,nln)
          iferrm(nvl,1) = 1
          ifwrt = -1
        end if
        if (ibackd .ne.ibacd.and.ifwrt .eq. 0) ifwrt = 1
CC        set text foreground color.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        i = IFFOP(mscrnm(nsc,nln),8)
        if (i .ge. 0.and.i .le. 7) then
          if (ibact .ne. i) then
            ifort = i
          else
            k = k + 1
            if (k .gt. 1) then
              mnpbuf(k) = msemi
              k = k + 1
            end if
            mnpbuf(k) = mscrnm(nsc,nln)
            iferrm(nvl,1) = 1
            ifwrt = -1
          end if
        else
          iferrm(nvl,1) = 1
          ifwrt = -1
        end if
        if (iforet .ne. ifort.and.ifwrt .eq. 0) ifwrt = 1
CC        set data field foreground color.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        i = IFFOP(mscrnm(nsc,nln),15)
        if (i .ge. 0.and.i .le. 14) then
          if (ibacd .ne. i) then
            iford = i
          else
            k = k + 1
            if (k .gt. 1) then
              mnpbuf(k) = msemi
              k = k + 1
            end if
            mnpbuf(k) = mscrnm(nsc,nln)
            iferrm(nvl,1) = 1
            ifwrt = -1
          end if
        else
          iferrm(nvl,1) = 1
          ifwrt = -1
        end if
        if (ifored .ne. iford.and.ifwrt .eq. 0) ifwrt = 1
CC
        ker = IFWER(iferrm,nmxlin*nmxfld)
        if (ker .ne. 0) then
          if (KERRS(22,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .eq. 1)
     *        go to 310
          i = IWTSC(numsc,1,mscrnm,ilinem,iferrm)
          go to 140
        end if
        i = JINIT(iferrm,nmxlin*nmxfld,0,0)
        i = IWTSC(numsc,1,mscrnm,ilinem,iferrm)
        if (ifwrt .eq. 1) then
          iflrjs = iflrj
          ibackt = ibact
          ibackd = ibacd
          iforet = ifort
          ifored = iford
CC          save new settings
          irec = 0
          ind = 0
  160     irec = irec + 1
          ist = ind + 1
          if (ist .le. numwd) then
            ind = ist + 20
            if (ind .gt. numwd) ind = numwd
            WRITE (idbfil,rec=irec,iostat=ier,err=280) (ireint(i),i=ist,
     *                                                  ind)
            go to 160
          end if
        end if
      end if
      ifset = 0
      go to 310
CC
CC      error section.
CC
CC      open error on old work file OBODMWORK.OBD.
  170 CLOSE (unit=iwkfil,status='delete',err=70)
      go to 70
CC
CC      open error on fuels data file OBODFUEL.OBD.
  180 if (NUMC1(mdirec,36) .gt. 0) then
        call MBLK1(mbufr,80)
        mbufr(1) = mastr
        call MVCH1(mfdnam,mbufr(2),22)
        call FNAME(mfnam,58,mdirec,mbufr)
        OPEN (unit=ifdfil,file=mfnam,iostat=ier,err=190,status='old',
     *        form='formatted')
        go to 90
      end if
CC
CC      open error on fuels/explosives data file OBODFUEL.OBD.
  190 if (KERRS(3,mfdnam,ier,idum,xdum,xdum,xdum,xdum,0) .eq. 1)
     *    go to 310
      go to 210
CC
CC      read error on fuels/explosives data file.
  200 if (KERRS(1,mfdnam,ier,irec,xdum,xdum,xdum,xdum,0) .eq. 1)
     *    go to 310
  210 iferrs = KERRS(26,mdum,idum,idum,xdum,xdum,xdum,xdum,0)
      if (iferrs .eq. 4) then
        go to 300
      else if (iferrs .ne. 3) then
        go to 220
      end if
      CLOSE (unit=ifdfil,status='keep',err=220)
  220 ifdfil = 0
      nl = 0
      go to 130
CC
CC      open error on data base file OBODMENU.OBD.
  230 if (KERRS(3,mdbnam,idum,idum,xdum,xdum,xdum,xdum,0) .eq. 1)
     *    go to 310
      i = KERRS(36,mdbnam,idum,idum,xdum,xdum,xdum,xdum,0)
      go to 300
CC
CC      open OBODM work file error.
  240 if (KERRS(3,mwknam,ier,idum,xdum,xdum,xdum,xdum,0) .eq. 1)
     *    go to 310
      i = KERRS(25,mdum,idum,idum,xdum,xdum,xdum,xdum,0)
      go to 300
CC
CC      write error on work file.
  250 if (KERRS(2,mwknam,ier,n,xdum,xdum,xdum,xdum,0) .eq. 1)
     *    go to 310
      i = KERRS(25,mdum,idum,idum,xdum,xdum,xdum,xdum,0)
      go to 300
CC
CC      read error on OBODM screen menu template data base file.
CCPCITS
  260 ier = 6501
CCPCITE
  270 if (KERRS(1,mdbnam,ier,idum,xdum,xdum,xdum,xdum,0) .eq. 1)
     *    go to 310
      go to 290
  280 if (KERRS(2,mdbnam,ier,idum,xdum,xdum,xdum,xdum,0) .eq. 1)
     *    go to 310
  290 i = KERRS(5,mdbnam,idum,idum,xdum,xdum,xdum,xdum,0)
CC      terminate execution.
  300 iferrs = 1
  310 return
CC
 9000 format (a36,2f10.0,i4)
 9001 format (a36,4f10.0)
CCPCITS
 9002 format (i2)
CCPCITE
 9003 format ('V',f4.1,'.',i4.4)                                        051801
      end
