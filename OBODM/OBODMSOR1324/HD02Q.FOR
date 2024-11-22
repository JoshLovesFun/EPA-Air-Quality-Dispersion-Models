      Subroutine HD02Q(ihdfl,mfnam)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - HD02Q Version 01.1                          ---
CC--- Subroutine description - generate plot on IBM Proprinter,     ---
CC---                          Epson FX, Epson LQ, Alps P2400C,     ---
CC---                          IBM XL24 AGM, Okidata, Canon BJ200e  ---
CC---------------------------------------------------------------------
CC
CC    ihdfl - logical units number of plot output file.
CC    mfnam - name of plot output file.
CC
CC********************************************************************
CC
      character*1 mpbyt,mesc,mfrm,mdumy,mnull,mthre,mptch,msclr,
     *            mcr,mn1,mn2,mode,mqest,mat,mcolr
      character*10 mbuf10
      character*22 mfnam,msrch
      character*80 mbuf80
      integer*2 ipbyt,ibtst,n3,jtr,ker
CCOMP1S
c     integer*2 n4
CCOMP1E
CC
CC      note the arrays ipbyt and mpbyt are equivalenced to common
CC      array calcsa.  if calcsa is reduced in size, check idim and
CC      jdim below to make sure plotting capability is not impaired.
CC      see also Subroutines HD01Q and LBITQ.
      dimension ipbyt(40000),mpbyt(80000),jcolr(16)
CC
      include 'OCNTRL.INC'
      include 'OWNTRL.INC'
      include 'OCALCS.INC'
      include 'ONAMES.INC'
      include 'OCDSPM.INC'
CC
      equivalence (mnpbuf(1),mbuf80,mbuf10),
     *            (calcsa(1),ipbyt(1),mpbyt(1)),
     *            (ilinem(1),i),(ilinem(2),j),(ilinem(3),ilst),
     *            (ilinem(4),jlst),(ilinem(5),ipen),(ilinem(6),ioff),
     *            (ilinem(7),jstrt),(ilinem(8),jendt),
     *            (ilinem(9),jmax),(ilinem(10),idim),
     *            (ilinem(11),jflg),(ilinem(12),imaxc),
     *            (ilinem(13),imaxr),(ilinem(14),lcolr)
CC
      data jcolr/0,2,6,2,3,1,4,0,0,2,6,2,5,1,4,0/,
     *     msrch/'SCRATCH plot file'/
CC
CC*********************************************************************
CC
      jflg = 1
CC        set mesc equal to escape character.
      mesc = CHAR(27)
CC      set null character.
      mnull = CHAR(0)
CC      set form feed.
      mfrm = CHAR(12)
CC      plot mode. (double speed, double density)
      mode = CHAR(2)
CC      question mark.
      mqest = CHAR(63)
CC      at sign.
      mat = CHAR(64)
CC      '3'.
      mthre = CHAR(51)
CC      paper pitch.
      mptch = CHAR(1)
CC      carriage return
      mcr = CHAR(13)
CC      'r' (color).
      msclr = CHAR(114)
      if (ifhcpy .ne. 4) then
CC        epson lq,
        vdpi = 180.0
      else
CC        epson fx,
        vdpi = 216.0
      end if
      hdpi = 120.0
      jer = 5
      irec = 0
      REWIND (unit=iscfil,iostat=ier,err=200)
   10 irec = irec + 1
      mbuf80 = mblnk1
CC      read data record.
      irec = irec + 1
      READ (iscfil,9000,iostat=ier,err=200,end=210) mbuf80
CC      if an s in col. 2.
      if (IFCHR(mnpbuf(2),19) .eq. 0) go to 10
CC
CC        read plot dimensions.
        READ (mbuf80,9001,iostat=ier,err=200) xnch,ynch
CC        set plot inches as generated on the crt screen.
        if (ifswxy .eq. 0) then
          xchs = xnch
          ychs = ynch
        else
CC          swap x any y dimensions.
          xchs = ynch
          ychs = xnch
        end if
CC        maximum number of dots or horizontal pixels assuming a
CC        horizontal dot density of hdpi dots per inch.
        imaxc = (paperw - 0.5 - xftmrg - rgtmrg) * hdpi + 0.5
CC        set scale of horizontal dimension in pixels per inch.
        scalx = FLOAT(imaxc) / xchs
CC        maximum number of lines in paperl inch plot assuming a veritcal
CC        line density of vdpi lines per inch.
        imaxr = (paperl - 1.5 - topmrg - botmrg) * vdpi + 0.5
CC        set scale of vertical dimension in lines per inch.
        scaly = FLOAT(imaxr) / ychs
CC
CC        determine the number of integer*2 words required to hold
CC        one line of horizontal plot data.
        idim = (imaxc + 1) / 2
CC        number of bytes required for one horizontal plot line.
        idimb = idim * 2
        if (ifcolr .eq. 0) then
CC          no color, number of plot lines required per plot segment.
          jdim = (2 * nmxcor) / idim
          ioff = 0
        else
CC          color, number of plot lines required per plot segment.
          jdim = (2 * nmxcor - idim) / idim
          ioff = idim
        end if
CC        number of blank lines at top of page for top margin.
        ntop = topmrg * vdpi + 0.5
CC        number of blank characters for left margin.
        nled = xftmrg * hdpi + 0.5
CC        total number of integer*4 words required to generate plot.
        nmxdm = (idim * jdim + ioff + 1) / 2
        jrec = 1
        jtr = 1
CC        write initializing escape record.
        WRITE (ihdfl,9003,iostat=ier,err=240) mnull,mesc,mat
        ker = 0
CC        write form feed to advance to top of page.
   20   jrec = jrec + 1
        jtr = 2
        WRITE (ihdfl,9003,iostat=ier,err=240) mnull,mfrm
        ker = 0
CC        set line spacing (paper feed pitch) at mptch/180 inch.
   30   jrec = jrec + 1
        jtr = 3
        WRITE (ihdfl,9003,iostat=ier,err=240) mnull,mesc,mthre,mptch
        ker = 0
   40   jrec = jrec + 1
        jtr = 4
        WRITE (ihdfl,9003,iostat=ier,err=240) mnull,mesc,mqest,
     *                                        malpha(12),mode
        ker = 0
   50   if (ntop .le. 0) go to 70
CC          write line feeds for top margin, ntop.
          mn1 = CHAR(1)
          mn2 = CHAR(0)
          jtr = 5
          i = 0
   60     i = i + 1
          if (i .le. ntop) then
            jrec = jrec + 1
            WRITE (ihdfl,9003,iostat=ier,err=240) mnull,mcr,mcr,mesc,
     *                                            malpha(12),mn1,mn2,
     *                                            mnull
            ker = 0
            go to 60
          end if
CC      begin loop over segment of plot.  each segment consists of jdim
CC      lines, starting and the top and looping to the bottom.
   70 jendt = imaxr + jdim
CC      loop over plot segments.
   80 jendt = jendt - jdim
CC      if end of plt go to finish up.
      if (jendt .le. 0) go to 220
        jstrt = jendt - (jdim - 1)
        if (jstrt .lt. 1) jstrt = 1
CC        initialize input plot data for current plot segment.
        irec = 0
        iend = 0
        jer = 5
        REWIND (unit=iscfil,iostat=ier,err=200)
        jer = 0
   90   irec = irec + 1
        mbuf80 = mblnk1
CC        read data record.
        READ (iscfil,9000,iostat=ier,err=200,end=210) mbuf80
        jer = 0
        iend = 0
CC        if an s in col. 2.
  100   if (IFCHR(mnpbuf(2),19) .eq. 0) go to 110
          jmax = 0
          ilst = 0
          jlst = 0
          i = JRNIT(calcsa,nmxdm,0.0,0)
CC
CC        if a plot record, p in col. 2.
  110   if (IFCHR(mnpbuf(2),16) .eq. 0) go to 120
CC
CC          read pen position, up/down flag and color index.
          READ (mbuf80,9002,iostat=ier,err=200) xpl,ypl,ipen,icolor
          if (ifswxy .ne. 0) then
CC            swap x any y dimensions.
            a = xpl
            xpl = (ynch - ypl)
            ypl = a
          end if
CC          horizontal dot (pixel) number.
          i = xpl * scalx + 0.5
CC          line number.
          j = ypl * scaly + 0.5
CC          limit points to a lower limit of 1 and upper x limit of
CC          imaxc and upper y limit of imaxr.
          i = MIN0(MAX0(i,1),imaxc)
          j = MIN0(MAX0(j,1),imaxr)
CC          save last color.
          lcolr = jcolr(icolor+1)
CC          go save point.
          call FILLQ
CC
CC         if end of input plot data, e in col. 2.
  120   if (IFCHR(mnpbuf(2),5) .eq. 0) go to 90
CC
CC          end of input plot data.
          if (iend .eq. 0) iend = 1
CC          complete and dump plot to printer.
CC          number of lines in this plot segment.
          m = jendt - jstrt + 1
          k = 0
  130     k = k + 1
CC          loop over lines.
          if (k .gt. m) go to 80
          if (ifctlc .eq. 2) then
            if (JFCTC(53) .ne. 0) go to 220
          end if
CC            reverse order of y points.
            j = m + 1 - k
            l = idim + 1
CC            determine number of bytes for this line.
            do 140 i=1,idim
              l = l-1
              if (ipbyt((j-1)*idim+l+ioff) .ne. 0) go to 150
  140       continue
            l = 1
  150       l = 2 * l
            l = MIN0(MAX0(l,2),idimb)
CCPCITS
CC            swap byte pairs for pc.
            do 160 i=1,l,2
              n1 = (j-1) * idimb + i + ioff * 2
              mdumy = mpbyt(n1)
              mpbyt(n1) = mpbyt(n1+1)
              mpbyt(n1+1) = mdumy
  160       continue
CCPCITE
CC            write bytes for this row.
CC            if not color branch around color code.
            if (ifcolr .le. 0) go to 190
CC              loop over colors.
              n = 0
  170         n = n + 1
              if (n .le. 7) then
                l1 = 0
CC                see which bytes have color n-1 set.
                do 180 n1=1,l
                  ibtst = ICHAR(mpbyt((j-1)*idimb+n1+ioff*2))
                  n3 = n - 1
CC                  if color set.
CCPCITS
                  if (BTEST(ibtst,n3)) then
CCPCITE
CCOMP1S
c                 n4 = 1
c                 n2 = IIBITS(ibtst,n3,n4)
c                 if (n2 .gt. 0) then
CCOMP1E
                    mpbyt(n1) = CHAR(128)
                    l1 = n1
                  else
                    mpbyt(n1) = mnull
                  end if
  180           continue
                if (l1 .gt. 0) then
                  n2 = (l1 + nled) / 256
                  n1 = (l1 + nled) - n2 * 256
                  mn1 = CHAR(n1)
                  mn2 = CHAR(n2)
                  mcolr = CHAR(n-1)
                  jtr = 6
                  jrec = jrec + 1
                  if (nled .gt. 0) then
CC                    left margin > 0.
                    WRITE (ihdfl,9003,iostat=ier,err=240) mnull,mcr,
     *                             mcr,mesc,msclr,mcolr,mesc,
     *                             malpha(12),mn1,mn2,(mnull,i=1,nled),
     *                             (mpbyt(i),i=1,l1)
                  else
CC                    no left margin.
                    WRITE (ihdfl,9003,iostat=ier,err=240) mnull,mcr,
     *                             mcr,mesc,msclr,mcolr,mesc,
     *                             malpha(12),mn1,mn2,(mpbyt(i),i=1,l1)
                  end if
                  ker = 0
                end if
CC                go get next color.
                go to 170
              end if
CC              go get next plot line.
              go to 130
CC              write non-color plot line.
  190         n2 = (l + nled) /256
              n1 = (l + nled) - n2 * 256
              mn1 = CHAR(n1)
              mn2 = CHAR(n2)
              jtr = 7
              jrec = jrec + 1
              if (nled .gt. 0) then
CC                left margin > 0.
                WRITE (ihdfl,9003,iostat=ier,err=240) mnull,mcr,
     *                          mcr,mesc,malpha(12),mn1,mn2,
     *                          (mnull,i=1,nled),(mpbyt((j-1)*idimb+
     *                          i),i=1,l)
              else
CC                no left margin.
                WRITE (ihdfl,9003,iostat=ier,err=240) mnull,mcr,
     *                          mcr,mesc,malpha(12),mn1,mn2,
     *                          (mpbyt((j-1)*idimb+i),i=1,l)
              end if
              ker = 0
CC              go get next line.
              go to 130
CC        read error.
  200   i = KERRS(1,msrch,ier,irec,xdum,xdum,xdum,xdum,0)
        jer = jer + 1
        if (jer .lt. 5) go to 90
        go to 260
CC        premature end-of-file.
  210   if (jer .ge. 5) go to 260
        if (jmax .gt. 0) then
          i = IFCHR(mnpbuf(2),-5)
          go to 100
        end if
        go to 80
CC      end of plot.
  220 jtr = 8
      jrec = jrec + 1
      WRITE (ihdfl,9003,iostat=ier,err=240) mnull,mesc,mat
      ker = 0
  230 jrec = jrec + 1
      jtr = 9
      WRITE (ihdfl,9003,iostat=ier,err=240) mnull,mfrm
      ker = 0
      go to 260
  240 i = KERRS(2,mfnam,ier,jrec,xdum,xdum,xdum,xdum,0)
      ker = ker + 1
      if (ker .lt. 5) go to (20,30,40,50,60,170,130,230,250),jtr
  250 continue
      WRITE (ihdfl,9003,err=260) mnull,mfrm
  260 return
 9000 format (a80)
 9001 format (2x,2f8.4,i4)
 9002 format (2x,2f8.4,2i2)
 9003 format (1642a1)
      end
