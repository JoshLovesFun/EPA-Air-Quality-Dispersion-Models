      Subroutine HD03Q(ihdfl,mfnam)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - HD03Q Version 01.1                          ---
CC--- Subroutine description - generate plot for hp7475a plotter.   ---
CC---                          reads data from scratch file iscfil. ---
CC---------------------------------------------------------------------
CC
CC    ihdfl - logical units number of plot output file.
CC    mfnam - name of plot output file.
CC
CC**********************************************************************
CC
      character*1 mesc,mchr,mnull
      character*22 mfnam,msrch
      character*80 mnpbfs
CC
      dimension kclr(100)
CC
      include 'ONAMES.INC'
      include 'OCNTRL.INC'
      include 'OWNTRL.INC'
CC
      equivalence (mnpbuf(1),mnpbfs),(kclr(1),itabsm(1,1))
CC
      data msrch/'SCRATCH plot file'/
CC
CC**********************************************************************
CC
CC      set escape.
      mesc = CHAR(27)
CC      set carriage return.
      mchr = CHAR(23)
CC      set null.
      mnull = CHAR(0)
      ipass = 0
      nclr = 0
   10 ipass = ipass + 1
      lclr = 0
      lpen = -1
      icls = -1
      irec = 0
      jer = 5
      REWIND (unit=iscfil,iostat=ier,err=30)
      jer = 0
   20 irec = irec + 1
      if (ifctlc .eq. 2) then
        if (JFCTC(53) .ne. 0) go to 80
      end if
CC      read scratch file.
      READ (iscfil,9000,iostat=ier,err=30,end=70) mnpbfs
      jer = 0
      go to 40
CC      read error on scratch file.
   30 i = KERRS(1,msrch,ier,irec,xdum,xdum,xdum,xdum,0)
      jer = jer + 1
      if (jer .lt. 5) go to 20
      go to 100
   40 if (irec .eq. 1) then
        if (ipass .gt. 1) go to 20
CC        first record, get plot size.
        READ (mnpbfs,9001) xnch,ynch
CC        turn on plotter 'esc.('.
        jrec = 1
        WRITE (ihdfl,9003,iostat=ier,err=90) mnull,mesc
        jrec = jrec + 1
CC        abort any device control instruction that may be partially
CC        decoded or executed 'esc.j'.
        WRITE (ihdfl,9004,iostat=ier,err=90) mnull,mesc
        jrec = jrec + 1
CC        abort any partially decoded hp-gl instruction and clear
CC        buffer of all instructions 'exc.k'.
        WRITE (ihdfl,9005,iostat=ier,err=90) mnull,mesc
        jrec = jrec + 1
CC        reset all handshake parameters to their default values
CC        'esc.r'.
        WRITE (ihdfl,9006,iostat=ier,err=90) mnull,mesc
CCOMP1S
c       jrec = jrec + 1
CC        set plotter configuration 'esc.@;0:'.  default buffer size to
CC        1024, disable hardwire handshake, disable monitor mode,
CC        enable normal mode.
c       WRITE (ihdfl,9007,iostat=ier,err=90) mnull,mesc
CCOMP1E
        jrec = jrec + 1
CC        set extended output handshake mode 'esc.n;19:'.  set xoff
CC        trigger to 19 or dc3.
        WRITE (ihdfl,9008,iostat=ier,err=90) mnull,mesc
CC        set handshake mode 2 'esc.i100;;17:'.  100 is number of empty
CC        bytes in buffer when xoff is sent.  17 is xon trigger
CC        character dc1.
        jrec = jrec + 1
        WRITE (ihdfl,9009,iostat=ier,err=90) mnull,mesc
CCOMP1S
c       jrec = jrec + 1
CC        set output mode 'esc.m0;13;10;13:'.  turn around delay is 0,
CC        output trigger character is 13 or cr, echo terminate is 10 or
CC        lf, output terminator is 13 or cr.
c       WRITE (ihdfl,9010,iostat=ier,err=90) mnull,mesc
CCOMP1E
CC        initialize plotter.
        jrec = jrec + 1
CCOMP1S
c       WRITE (ihdfl,9011,iostat=ier,err=90) mnull,mchr
CCOMP1E
CCPCITS
        WRITE (ihdfl,9011,iostat=ier,err=90) mnull
CCPCITE
CC        set p1,p2 to default.
        jrec = jrec + 1
CCOMP1S
c       WRITE (ihdfl,9012,iostat=ier,err=90) mnull,mchr
CCOMP1E
CCPCITS
        WRITE (ihdfl,9012,iostat=ier,err=90) mnull
CCPCITE
CC        set scaling.
        ixsc = (paperw - 0.5) + 0.5
        iysc = (paperl - 1.5) + 0.5
        sclx = (paperw - 0.5 - xftmrg - rgtmrg) / xnch
        scly = (paperl - 1.5 - topmrg - botmrg) / ynch
        jrec = jrec + 1
CCOMP1S
c       WRITE (ihdfl,9013,iostat=ier,err=90) mnull,ixsc,iysc,mchr
CCOMP1E
CCPCITS
        WRITE (ihdfl,9013,iostat=ier,err=90) mnull,ixsc,iysc
CCPCITE
      else
CC        if 'e' in col. 2.
        if (mnpbuf(2) .eq. malpha(5)) then
CC          end of plot.
          if (ipass .lt. nclr) go to 10
          go to 100
CC        if not 'p'.
        else if (mnpbuf(2) .ne. malpha(16)) then
          go to 20
        end if
CC        'p' in col. 2, plot point.
        READ (mnpbfs,9001) xp,yp,ipen,iclr
        xpp = xp * sclx + xftmrg
        ypp = yp * scly + botmrg
        iclr = MOD(iclr,6) + 1
        if (ifcolr .eq. 0) then
          iclr = 1
        end if
        if (ipass .eq. 1) then
CC          save all color codes encountered.
          if (iclr .ne. lclr) then
            if (nclr .gt. 0) then
              do 50 i=1,nclr
                if (iclr .eq. kclr(i)) go to 60
   50         continue
            end if
CC            accumulate total number of colors.
            nclr = nclr + 1
            kclr(nclr) = iclr
          end if
        end if
CC        only plot one color per pass to minimize pen changes.
   60   if (iclr .ne. kclr(ipass)) go to 20
CC        check color.
        if (iclr .ne. icls) then
CC          change pens.
          icls = iclr
          jrec = jrec + 1
CCOMP1S
c         WRITE (ihdfl,9014,iostat=ier,err=90) mnull,mchr
CCOMP1E
CCPCITS
          WRITE (ihdfl,9014,iostat=ier,err=90) mnull
CCPCITE
          jrec = jrec + 2
CCOMP1S
c         WRITE (ihdfl,9015,iostat=ier,err=90) mnull,0,mchr
c         WRITE (ihdfl,9015,iostat=ier,err=90) mnull,iclr,mchr
CCOMP1E
CCPCITS
          WRITE (ihdfl,9015,iostat=ier,err=90) mnull,0
          WRITE (ihdfl,9015,iostat=ier,err=90) mnull,iclr
CCPCITE
        end if
        if (ipen .ge. 0) then
          if (ipen .ne. lpen) then
            if (ipen .eq. 2) then
CC              move pen (down) to point xpp,ypp.
              jrec = jrec + 1
CCOMP1S
c             WRITE (ihdfl,9016,iostat=ier,err=90) mnull,mchr
CCOMP1E
CCPCITS
              WRITE (ihdfl,9016,iostat=ier,err=90) mnull
CCPCITE
              jrec = jrec + 1
CCOMP1S
c             WRITE (ihdfl,9017,iostat=ier,err=90) mnull,xpp,ypp,mchr
CCOMP1E
CCPCITS
              WRITE (ihdfl,9017,iostat=ier,err=90) mnull,xpp,ypp
CCPCITE
            else
CC              move pen (up) to point xpp,ypp.
              jrec = jrec + 1
CCOMP1S
c             WRITE (ihdfl,9014,iostat=ier,err=90) mnull,mchr
CCOMP1E
CCPCITS
              WRITE (ihdfl,9014,iostat=ier,err=90) mnull
CCPCITE
              jrec = jrec + 1
CCOMP1S
c             WRITE (ihdfl,9017,iostat=ier,err=90) mnull,xpp,ypp,mchr
CCOMP1E
CCPCITS
              WRITE (ihdfl,9017,iostat=ier,err=90) mnull,xpp,ypp
CCPCITE
            end if
          else
            jrec = jrec + 1
CCOMP1S
c           WRITE (ihdfl,9017,iostat=ier,err=90) mnull,xpp,ypp,mchr
CCOMP1E
CCPCITS
            WRITE (ihdfl,9017,iostat=ier,err=90) mnull,xpp,ypp
CCPCITE
          end if
        else
          jrec = jrec + 1
CCOMP1S
c         WRITE (ihdfl,9014,iostat=ier,err=90) mnull,mchr
CCOMP1E
CCPCITS
          WRITE (ihdfl,9014,iostat=ier,err=90) mnull
CCPCITE
          jrec = jrec + 1
CCOMP1S
c         WRITE (ihdfl,9017,iostat=ier,err=90) mnull,xpp,ypp,mchr
CCOMP1E
CCPCITS
          WRITE (ihdfl,9017,iostat=ier,err=90) mnull,xpp,ypp
CCPCITE
        end if
        lpen = ipen
      end if
      go to 20
   70 if (ipass .lt. nclr) go to 10
   80 continue
CCOMP1S
c     jrec = jrec + 1
c     WRITE (ihdfl,9014,iostat=ier,err=90) mnull,mchr
c     jrec = jrec + 1
c     WRITE (ihdfl,9015,iostat=ier,err=90) mnull,0,mchr
c     jrec = jrec + 1
c     WRITE (ihdfl,9018,iostat=ier,err=90) mnull,mesc,mchr
CCOMP1E
CCPCITS
      jrec = jrec + 1
      WRITE (ihdfl,9014,iostat=ier,err=90) mnull
      jrec = jrec + 1
      WRITE (ihdfl,9015,iostat=ier,err=90) mnull,0
      jrec = jrec + 1
      WRITE (ihdfl,9018,iostat=ier,err=90) mnull,mesc
CCPCITE
      go to 100
   90 i = KERRS(2,mfnam,ier,jrec,xdum,xdum,xdum,xdum,0)
  100 return
CC
 9000 format (a80)
 9001 format (2x,2f8.4,2i2)
CCOMP1S
c9003 format ($,2a1,'.(')
c9004 format ($,2a1,'.J')
c9005 format ($,2a1,'.K')
c9006 format ($,2a1,'.R')
c9007 format ($,2a1,'.@;0:')
c9008 format ($,2a1,'.N;19:')
c9009 format ($,2a1,'.I100;;17:')
c9010 format ($,2a1,'.M0;13;10;13:')
c9011 format ($,a1,'IN;',a1)
c9012 format ($,a1,'IP;',a1)
c9013 format ($,a1,'SC 0,',i5,',0,',i5,';',a1)
c9014 format ($,a1,'PU;',a1)
c9015 format ($,a1,'SP',i2,';',a1)
c9016 format ($,a1,'PD;',a1)
c9017 format ($,a1,'PA',f8.4,',',f8.4,';',a1)
c9018 format ($,2a1,'.)',a1)
CCOMP1E
CCPCITS
 9003 format (2a1,'.(',\)
 9004 format (2a1,'.J',\)
 9005 format (2a1,'.K',\)
 9006 format (2a1,'.R',\)
 9008 format (2a1,'.N;19:',\)
 9009 format (2a1,'.I100;;17:',\)
 9011 format (a1,'IN;',\)
 9012 format (a1,'IP;',\)
 9013 format (a1,'SC 0,',i5,',0,',i5,';',\)
 9014 format (a1,'PU;',\)
 9015 format (a1,'SP',i2,';',\)
 9016 format (a1,'PD;',\)
 9017 format (a1,'PA',f8.4,',',f8.4,';',\)
 9018 format (2a1,'.)',\)
CCPCITE
      end
