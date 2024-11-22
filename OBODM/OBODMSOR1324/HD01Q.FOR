      Subroutine HD01Q(ihdfl,mfnam)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - HD01Q Version 01.1                          ---
CC--- Subroutine description - generate plot on hp2235a ruggedwriter---
CC---                          480 printer or hp laser jet xx       ---
CC---                          printer.                             ---
CC---------------------------------------------------------------------
CC
CC    ihdfl - logical units number of plot output file.
CC    mfnam - name of plot output file.
CC
CC********************************************************************
CC
      character*1 mpbyt,mesc,mfrm,mdumy,mnull,mctlz,mchg
      character*10 mbuf10
      character*22 mfnam,msrch
      character*80 mbuf80
      integer*2 ipbyt,ker,jtr
CC
CC      note the arrays ipbyt and mpbyt are equivalenced to common
CC      array calcsa.  if calcsa is reduced in size, check idim and
CC      jdim below to make sure plotting capability is not impaired.
CC      see also Subroutines HD02Q and LBITQ.
      dimension ipbyt(40000),mpbyt(80000)
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
     *            (ilinem(13),imaxr)
CC
      data msrch/'SCRATCH plot file'/
CC
CC*********************************************************************
CC
      ker = 0
      jflg = 0
      ioff = 0
CC        set mesc equal to escape character.
      mesc = CHAR(27)
CC        set null character.
      mnull = CHAR(0)
CC        form feed.
      mfrm = CHAR(12)
CC        cntrl z.
      mctlz = CHAR(26)
CC        substitute for cntrl z.
      mchg = CHAR(24)
      if (ifhcpy .gt. 1) then
        hdpi = 180.0
        vdpi = 180.0
      else
        hdpi = 300.0
        vdpi = 300.0
      end if
      jer = 5
      irec = 0
      REWIND (unit=iscfil,iostat=ier,err=190)
   10 irec = irec + 1
      mbuf80 = mblnk1
CC      read data record.
      irec = irec + 1
      READ (iscfil,9000,iostat=ier,err=190,end=200) mbuf80
CC      if an s in col. 2.
      if (IFCHR(mnpbuf(2),19) .eq. 0) go to 10
CC
CC        read plot dimensions.
        READ (mbuf80,9001,iostat=ier,err=190) xnch,ynch
        if (ifswxy .eq. 0) then
          xchs = xnch
          ychs = ynch
        else
          xchs = ynch
          ychs = xnch
        end if
CC        maximum number of horizontal dots (pixels) in plot, using a
CC        horizontal print density of hdpi dots per inch.
        imaxc = ABS(paperw - 0.5 - xftmrg - rgtmrg) * hdpi + 0.5
CC        set scale of horizontal dimension in pixels per inch. xchs is
CC        the horizonal plot dimension in inches as generated on the
CC        crt screen.
        scalx = FLOAT(imaxc) / xchs
CC        maximum number of lines using paperl inch paper and using a
CC        vertical line density of vdpi lines per inch.
        imaxr = ABS(paperl - 1.5 - topmrg - botmrg) * vdpi + 0.5
CC        set scale of vertical dimension in lines per inch. ychs is the
CC        vertical plot dimension in inches as generated on the crt
CC        screen.
        scaly = FLOAT(imaxr) / ychs
CC
        jrec = 0
CC        initialize hp ruggedwriter 480 or hp laserjet xx.
        if (ifhcpy .ne. 1) then
          jrec = jrec + 1
          jtr = 1
          WRITE (ihdfl,9010,iostat=ier,err=250) mnull,mfrm
          ker = 0
        else
cc          reset printer
          WRITE (ihdfl,9210,iostat=ier,err=250) mnull,mesc
        end if
CC        set zero space between lines.
   20   jrec = jrec + 1
        jtr = 2
        WRITE (ihdfl,9002,iostat=ier,err=250) mnull,mesc,0
        ker = 0
CC        set horizontal pixel density to hdpi ppi.
   30   jrec = jrec + 1
        jtr = 3
        i = hdpi
        WRITE (ihdfl,9003,iostat=ier,err=250) mnull,mesc,i
        ker = 0
CC        start plot.
   40   jrec = jrec + 1
        jtr = 4
        WRITE (ihdfl,9004,iostat=ier,err=250) mnull,mesc
        ker = 0
CC        determine space required to generate a horizontal plot line.
CC        idim is the number of integer*2 words required per plot line.
   50   idim = (((imaxc + 7) / 8) + 1) / 2
CC        number of bytes required per plot line.
        idimb = idim * 2
CC        number of plot lines available per plot segment.
        jdim = (2 * nmxcor) / idim
CC        number of blank lines at top of plot.
        ntop = topmrg * vdpi + 0.5
CC        number of blank characters for left margin.
        nled = xftmrg * hdpi / 8.0 + 0.5
CC        total number of integer*4 words required to generate plot.
        nmxdm = (idim * jdim + 1) / 2
        if (ntop .le. 0) go to 70
CC          write top margin
          jtr = 5
          i = 0
   60     i = i + 1
          if (i .le. ntop) then
            jrec = jrec + 1
            WRITE (ihdfl,9005,iostat=ier,err=250) mnull,mesc,mnull
            ker = 0
            go to 60
          end if
CC      begin loop over segments of plot.  the program loops over plot
CC      segments of jdim lines each, starting at the top of the plot
CC      and looping over segments to the bottom.
CC      start with top line.
   70 jendt = imaxr + jdim
CC      loop over segments.
   80 jendt = jendt - jdim
CC      if no more segments go to end of plot.
      if (jendt .le. 0) go to 210
CC        determine start line of segment
        jstrt = jendt - (jdim - 1)
        if (jstrt .lt. 1) jstrt = 1
CC        read generated plot data from file iscfil.
        irec = 0
        iend = 0
        jer = 5
        REWIND (unit=iscfil,iostat=ier,err=190)
        jer = 0
   90   irec = irec + 1
        mbuf80 = mblnk1
CC        read data record.
        READ (iscfil,9000,iostat=ier,err=190,end=200) mbuf80
        jer = 0
        iend = 0
CC        if an s in col. 2.
  100   if (IFCHR(mnpbuf(2),19) .eq. 0) go to 110
CC          plot start record was read, initialize.
          jmax = 0
          ilst = 0
          jlst = 0
          i = JRNIT(calcsa,nmxdm,0.0,0)
CC
CC        if a plot record, p in col. 2.
  110   if (IFCHR(mnpbuf(2),16) .eq. 0) go to 120
CC
CC          read pen position, up/down flag and color index.
          READ (mbuf80,9001,iostat=ier,err=190) xpl,ypl,ipen,icolor
          if (ifswxy .ne. 0) then
CC            swap x and y dimensions.
            a = xpl
            xpl = (ynch - ypl)
            ypl = a
          end if
CC          calculate horizontal dot or pixel number.
          i = xpl * scalx + 0.5
CC          calculate line number.
          j = ypl * scaly + 0.5
CC          limit pixels to 1 to imaxc in horizontal direction and
CC          1 to imaxr in vertical (line) direction.
          i = MIN0(MAX0(i,1),imaxc)
          j = MIN0(MAX0(j,1),imaxr)
          call FILLQ
CC
CC         if end of input plot data, e in col. 2.
  120   if (IFCHR(mnpbuf(2),5) .eq. 0) go to 90
CC
CC          end of plot input plot data detected.
          if (iend .eq. 0) iend = 1
CC          complete and dump plot to printer.
CC          number of lines to write.
          m = jendt - jstrt + 1
          k = 0
CC          loop over lines.
  130     k = k + 1
          if (k .le. m) then
          if (ifctlc .eq. 2) then
            if (JFCTC(53) .ne. 0) go to 260
          end if
CC            reverse order of y points.
            j = m + 1 - k
            l = idim + 1
CC            determine number of bytes for this line.
            do 140 i=1,idim
              l = l-1
              if (ipbyt((j-1)*idim+l) .ne. 0) go to 150
  140       continue
            l = 1
  150       l = 2 * l
            l = MIN0(MAX0(l,2),idimb)
            WRITE (mbuf10,9006) l + nled
            do 160 n=1,10
              if (mnpbuf(n) .ne. mblnk1) go to 170
  160       continue
            n = 10
CC            write bytes for this row.
  170       jrec = jrec + 1
CCPCITS
CC            swap byte pairs for pc.
            do 180 i=1,l,2
              n1 = (j-1) * idimb + i
              mdumy = mpbyt(n1)
              mpbyt(n1) = mpbyt(n1+1)
              mpbyt(n1+1) = mdumy
              if (mpbyt(n1) .eq. mctlz) then
                mpbyt(n1) = mchg
              end if
              if (mpbyt(n1+1) .eq. mctlz) then
                mpbyt(n1+1) = mchg
              end if
  180       continue
CCPCITE
CC            if left margin > 0.
            if (nled .gt. 0) then
              jtr = 6
              WRITE (ihdfl,9007,iostat=ier,err=250) mnull,mesc,
     *                          (mnpbuf(i),i=n,10),malpha(23),
     *                          (mnull,i=1,nled),(mpbyt((j-1)*idimb+
     *                          i),i=1,l)
            else
CC              no left margin.
              jtr = 7
              WRITE (ihdfl,9007,iostat=ier,err=250) mnull,mesc,
     *                          (mnpbuf(i),i=n,10),malpha(23),
     *                          (mpbyt((j-1)*idimb+i),i=1,l)
            end if
            ker = 0
CC            go get next plot line.
            go to 130
          end if
CC          go get next plot segment.
          go to 80
CC        read error.
  190   i = KERRS(1,msrch,ier,irec,xdum,xdum,xdum,xdum,0)
        jer = jer + 1
        if (jer .lt. 5) go to 90
        go to 270
CC        premature end-of-file.
  200   if (jer .ge. 5) go to 270
        if (jmax .gt. 0) then
          i = IFCHR(mnpbuf(2),-5)
          go to 100
        end if
        go to 80
CC      end of plot.
  210 jrec = jrec + 1
      jtr = 8
      WRITE (ihdfl,9008,iostat=ier,err=250) mnull,mesc
      ker = 0
CC      return line spacing to 6 lines per inch.
  220 jrec = jrec + 1
      jtr = 9
      WRITE (ihdfl,9002,iostat=ier,err=250) mnull,mesc,8
      ker = 0
CC      return to standard character spacing.
  230 jrec = jrec + 1
      jtr = 10
      WRITE (ihdfl,9009,iostat=ier,err=250) mnull,mesc
      ker = 0
  240 jrec = jrec + 1
      jtr = 11
      if (ifhcpy .eq. 1) then
        WRITE (ihdfl,9210,iostat=ier,err=250) mnull,mesc
      else
        WRITE (ihdfl,9010,iostat=ier,err=250) mnull,mfrm
      end if
      go to 270
  250 i = KERRS(2,mfnam,ier,jrec,xdum,xdum,xdum,xdum,0)
      ker = ker + 1
      if (ker .lt. 5) then
        go to (20,30,40,50,60,130,130,220,230,240,260),jtr
      end if
  260 if (ifhcpy .eq. 1) then
        WRITE (ihdfl,9210,err=270) mnull,mesc
      end if
      WRITE (ihdfl,9010,err=270) mnull,mfrm
  270 return
CC
 9000 format (a80)
 9001 format (2x,2f8.4,2i2)
 9002 format (2a1,'&l',i1,'C')
 9003 format (2a1,'*t',i3.3,'R')
 9004 format (2a1,'*rA')
 9005 format (2a1,'*b1W',a1)
 9006 format (i10)
 9007 format (2a1,'*b',400a1)
 9008 format (2a1,'*rB')
 9009 format (2a1,'*t90R')
 9010 format (2a1)
 9210 format (2a1,'E')
      end
