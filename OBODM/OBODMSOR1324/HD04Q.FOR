      Subroutine HD04Q(ihdfl,mfnam)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - HD04Q Version 01.1                          ---
CC--- Subroutine description - write plot information to a dec      ---
CC---                          inkjet printer.                      ---
CC---------------------------------------------------------------------
CC
CC    ihdfl - logical units number of plot output file.
CC    mfnam - name of plot output file.
CC
CC**********************************************************************
CC
      character*1 mfrm,mesc,mbksl,mnull
      character*22 mfnam,msrch
      character*80 mnpbfs
      integer*2 lit1,lhue,lit2,lsat
CC
      dimension lit1(16),lhue(16),lit2(16),lsat(16)
CC
      include 'OCNTRL.INC'
      include 'OWNTRL.INC'
      include 'OCALCS.INC'
      include 'OCDSPM.INC'
      include 'ONAMES.INC'
CC
      equivalence (mnpbuf(1),mnpbfs)
CC
CC      dark, dkblue, dkgren, dkcyan, dkred, dkmage, yelbrn, dkwhit,
CC      ltgray, ltblue, ltgren, ltcyan, ltred, ltmage, yellow, white.
      data lit1/  0, 57, 42, 71, 57, 71, 57, 66, 33, 71, 71, 57, 42, 85,
     *           85,100/,
     *     lhue/  0,  0,240,300, 90, 60,180,  0,  0, 20,260,330,120, 60,
     *          180,  0/,
     *     lit2/  0, 57, 42, 71, 57, 71, 57, 66, 33, 71, 71, 57, 42, 85,
     *           85,100/,
     *     lsat/  0,100,100,100,100,100,100,  0,  0,100,100,100,100,100,
     *          100,  0/
CC
      data msrch/'SCRATCH plot file'/
CC
CC**********************************************************************
CC
CC      form feed.
      mfrm = CHAR(12)
CC      escape.
      mesc = CHAR(27)
CC      back slash.
      mbksl = CHAR(92)
CC      null.
      mnull = CHAR(0)
      if (ipmode .lt. 16) then
        lit1(2) = lit1(12)
        lit1(3) = lit1(14)
        lit1(4) = lit1(16)
        lhue(2) = lhue(12)
        lhue(3) = lhue(14)
        lhue(4) = lhue(16)
        lit2(2) = lit2(12)
        lit2(3) = lit2(14)
        lit2(4) = lit2(16)
        lsat(2) = lsat(12)
        lsat(3) = lsat(14)
        lsat(4) = lsat(16)
        icwt = 3
      else
        icwt = 15
      end if
      jer = 2
      irec = 0
      REWIND (unit=iscfil,iostat=ier,err=30)
      iend = 0
      jer = 0
      iflg = 0
      jrec = 0
   10 irec = irec + 1
      if (ifctlc .eq. 2) then
        if (JFCTC(53) .ne. 0) go to 50
      end if
      READ (iscfil,9000,iostat=ier,err=30,end=50) mnpbuf
      jer = 0
CC
CC      see if start record 's'.
   20 if (IFCHR(mnpbuf(2),19) .ne. 0) then
        READ (mnpbfs,9001,iostat=ier,err=30) xnchpl,ynchpl,nplots,
     *                                       iswplt(9),(iswplt(i),
     *                                       i=13,15)
        jer = 0
        irec = irec + 1
CC        initialize plot output, etc.
        if (nplots .gt. 1) then
          jrec = jrec + 1
          WRITE (ihdfl,9009,iostat=ier,err=40) mnull,mfrm
        end if
        jrec = jrec + 1
        WRITE (ihdfl,9002,iostat=ier,err=40) mnull,mesc
        if (iswplt(9) .eq. 0) then
CC          back ground is black, invert to white.
          iswplt(9) = icwt
          ichg = 1
          if (iswplt(13) .eq. icwt) then
            iswplt(13) = 0
          else if (iswplt(13) .eq. 0) then
            iswplt(13) = icwt
          end if
          if (iswplt(14) .eq. icwt) then
            iswplt(14) = 0
          else if (iswplt(14) .eq. 0) then
            iswplt(14) = icwt
          end if
          if (iswplt(15) .eq. icwt) then
            iswplt(15) = 0
          else if (iswplt(15) .eq. 0) then
            iswplt(15) = icwt
          end if
        else
CC          background is white
          ichg = 0
        end if
        if (ifcolr .eq. 0) then
          iswplt(13) = 0
          iswplt(14) = 0
          iswplt(15) = 0
        end if
CC        call swindo(0.0,0.0,xnchpl,ynchpl)
CC        plot limits.
        xmax = 767
        ymax = 479
CC        plot units per inch scale.
        xscl = xmax / (paperw - 0.5)
        yscl = ymax / (paperl - 1.5)
CC        left margin.
        ledx = xftmrg * xscl + 0.5
CC        top margin.
        ledy = topmrg * yscl + 0.5
CC        determine adjusted scale from inches to plot units.
        xscls = (paperw - 0.5 - xftmrg - rgtmrg) * xscl
        yscls = (paperl - 1.5 - topmrg - botmrg) * yscl
        if (ifswxy .eq. 0) then
          xscls = xscls / xnchpl
          yscls = yscls / ynchpl
        else
          xscls = xscls / ynchpl
          yscls = yscls / xnchpl
        end if
CC        call scolor(mcolor(iswplt(9)+1,0)
        i = iswplt(9) + 1
        WRITE (ihdfl,9003,iostat=ier,err=40) mnull,0,lit1(i),lhue(i),
     *                                       lit2(i),lsat(i),0
CC        call scolor(mcolor(iswplt(13)+1,1)
        i = iswplt(13) + 1
        WRITE (ihdfl,9003,iostat=ier,err=40) mnull,1,lit1(i),lhue(i),
     *                                       lit2(i),lsat(i),1
CC        call scolor(mcolor(iswplt(14)+1,2)
        i = iswplt(14) + 1
        WRITE (ihdfl,9003,iostat=ier,err=40) mnull,2,lit1(i),lhue(i),
     *                                       lit2(i),lsat(i),2
CC        call scolor(mcolor(iswplt(15)+1,3)
        i = iswplt(15) + 1
        WRITE (ihdfl,9003,iostat=ier,err=40) mnull,3,lit1(i),lhue(i),
     *                                       lit2(i),lsat(i),3
        lcolor = -1
        iflg = 1
CC
CC      see if plot record 'p'.
      else if (IFCHR(mnpbuf(2),16) .ne. 0) then
        READ (mnpbfs,9001,iostat=ier,err=30) xp,yp,ipen,jcolr
        if (ifcolr .eq. 0) then
          jcolr = ichg * icwt
        end if
        if (ifswxy .eq. 0) then
          yp = ynchpl - yp
        else
          a = xp
          xp = ynchpl - yp
          yp = xnchpl - a
        end if
        ixp = xp * xscl + 0.5 + ledx
        jyp = yp * yscl + 0.5 + ledy
        jer = 0
        icolor = JCLRQ(jcolr,ichg,icr,isc,icwt)
CC        call scolor(mcolor(icr),isc)
        WRITE (ihdfl,9003,iostat=ier,err=40) mnull,isc,lit1(icr),
     *                                       lhue(icr),lit2(icr),
     *                                       lsat(i),isc
        if (ipen .eq. 2) then
CC          call line(xp,yp)
          if (ipenl .eq. 0) then
            WRITE (ihdfl,9004,iostat=ier,err=40) mnull,ixpl,jypl,ixp,
     *                                           jyp
          else
            WRITE (ihdfl,9005,iostat=ier,err=40) mnull,ixp,jyp
          end if
          ipenl = 1
        else
CC          call move(xp,yp)
          ipenl = 0
        end if
        ixpl = ixp
        jypl = jyp
        iflg = 2
CC
CC      see if outlined area 'b'.
      else if (IFCHR(mnpbuf(2),2) .ne. 0) then
        READ (mnpbfs,9006,iostat=ier,err=30) xp1,yp1,xp2,yp2,jcolr
        if (ifcolr .eq. 0) then
          jcolr = ichg * icwt
        end if
        if (IFEQU(xp1-xp2,0.0) .ne. 0.or.IFEQU(yp1-yp2,0.0) .ne. 0) then
          if (ifswxy .eq. 0) then
            yp1 = ynchpl - yp1
            yp2 = ynchpl - yp2
          else
            a = xp1
            xp1 = ynchpl - yp1
            yp1 = xnchpl - a
            a = xp2
            xp2 = ynchpl - yp2
            yp2 = xnchpl - a
          end if
          ixp1 = xp1 * xscl + 0.5 + ledx
          jyp1 = yp1 * yscl + 0.5 + ledy
          ixp2 = xp1 * xscl + 0.5 + ledx
          jyp2 = yp2 * yscl + 0.5 + ledy
          icolor = JCLRQ(jcolr,ichg,icr,isc,icwt)
CC          call scolor(mcolor(icr),isc)
          WRITE (ihdfl,9003,iostat=ier,err=40) mnull,isc,lit1(icr),
     *                                         lhue(icr),lit2(icr),
     *                                         lsat(i),isc
          ixpl = ixp1
          jypl = jyp1
          ipenl = 0
CC          call sshade(,1)
          WRITE (ihdfl,9007,iostat=ier,err=40) mnull,jyp1
CC          call box(xp1,yp1,xp2,yp2)
          WRITE (ihdfl,9004,iostat=ier,err=40) mnull,ixp1,jyp1,ixp2,
     *                                         jyp1
          WRITE (ihdfl,9005,iostat=ier,err=40) mnull,ixp2,jyp2
          WRITE (ihdfl,9005,iostat=ier,err=40) mnull,ixp1,jyp2
          WRITE (ihdfl,9005,iostat=ier,err=40) mnull,ixp1,jyp1
CC          call snshad
          WRITE (ihdfl,9008,iostat=ier,err=40) mnull
          iflg = 3
        end if
CC
CC      see if end record 'e'.
      else if (IFCHR(mnpbuf(2),5) .ne. 0) then
        jrec = jrec + 1
        WRITE (ihdfl,9009,iostat=ier,err=40) mnull,mesc,mbksl
        iflg = 4
        if (iend .ne. 0) go to 60
      end if
      go to 10
CC
CC      read error.
   30 call MBLK1(mnpbuf,60)
      i = KERRS(1,msrch,ier,irec,xdum,xdum,xdum,xdum,0)
      jer = jer + 1
      if (jer .gt. 2) go to 60
      if (jer .gt. 1) iend = 1
      if (iend .ne. 0) then
        i = IFCHR(mnpbuf(2),-5)
        go to 20
      end if
      go to 10
CC
CC      write error.
   40 i = KERRS(2,mfnam,ier,jrec,xdum,xdum,xdum,xdum,0)
      go to 60
CC
   50 if (iflg .ne. 4) then
        jrec = jrec + 1
        WRITE (ihdfl,9009,iostat=ier,err=40) mnull,mesc,mbksl
      end if
   60 return
CC
 9000 FORMAT (80a1)
 9001 FORMAT (2x,2f8.4,5i2)
 9002 FORMAT (2a1,'Pp')
 9003 FORMAT (a1,'S(M',i2,'(L',i3,')(A H',i3,'L',i3,'S',i3,'))W(I',i2,
     *')')
 9004 FORMAT (a1,'P[',i4,',',i4,']V[',i4,',',i4,']')
 9005 FORMAT (a1,'V[',i4,',',i4,']')
 9006 FORMAT (2x,4f8.4,i2)
 9007 FORMAT (a1,'W(S1  [,',i4,'])')
 9008 FORMAT (a1,'W(S0)')
 9009 FORMAT (3a1)
      end
