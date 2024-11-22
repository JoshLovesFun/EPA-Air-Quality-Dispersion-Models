      program analysis
c=======================================================================
c
c       ANALYSIS - A POST-PROCESSOR FOR OFFSHORE AND COASTAL
c                  DISPERSION MODEL (OCD)
c       LEVEL 970320  (Level also defined in subroutine PAGE)
c
c       Joseph C. Chang
c       EARTH TECH
c       196 Baker Avenue
c       Concord, MA 01742
c       Tel (508) 371-4256
c       Fax (508) 371-4280
c       e-mail chang@src.com, or
c              jchang@earthtech.com
c
c       I/O:
c       unit 25: program options (ana.dat)
c       unit 26: output listing (ana.out)
c       unit 8:  single binary input file (conc01.bin) for the AVERGE,
c                CUMFRQ, PEAK, TOPVAL, and EXTRCT modules.
c       unit 9:  averaged binary output file (avconc.bin) for the AVERGE
c                module
c       unit 11-22: multiple binary input files (conc01.bin through
c                conc12.bin) to be merged by the SEQADD module
c       unit 10: merged binary output file (sqconc.bin) for the SEQADD
c                module
c       unit 27: output file (ana.top) used to generate map of top N conc.
c                for X hour average at a specified time
c       unit 28: output file (ana.pek) used to generate map of conc.
c                exceedances for X hour average at a specified time
c       unit 29: output file (ana.plt) used to generate, for a certain
c                averaging period, conc. data whose isopleths can be
c                independently plotted by external plotting software.
c
c       Note that the SEQADD and AVERGE modules must be run separately,
c       while any combination of the CUMFRQ, PEAK, TOPVAL, and EXTRCT
c       modules can be run simultaneously.
c
c=======================================================================
c
        parameter (maxrec=3000)
        common /head/npage,nline
        dimension rmet(7),conc(maxrec),temp(24,maxrec)
        dimension wk1(maxrec),wk2(maxrec),wk3(maxrec)  ! place holders
        dimension wk4(maxrec),wk5(maxrec),wk6(maxrec)  ! place holders
        dimension xrecpt(maxrec),yrecpt(maxrec)
        character*80 title(3)
        character*4 keys(6),skeys(6)
        character*4 word
        logical lexist
        data keys /'CUMF','AVER','TOPV','PEAK','SEQA','EXTR'/
        data skeys /'cumf','aver','topv','peak','seqa','extr'/
c
        npage = 0
        nline = 0
        icall = 0
c
c   Define unit numbers
c
        ic = 25
        iout = 26
        itop = 27
        ipek = 28
        iplt = 29
        ich = 8
        ioutavg = 9
        ioutseq = 10
        initseq = 11
c
c   Open ascii input and output files, common to all routines
c
        call fopen (ic,'ana.dat','old','formatted')
        call fopen (iout,'ana.out','unknown','formatted')
c
c     Ensure that the file ANA.TOP does not already exist
c     This is probably not necessary.  However, it is always
c     good to ensure that the file ANA.TOP is up to date.
c
        inquire (file='ana.top',exist=lexist)
        if (lexist) then
          open (unit=itop,file='ana.top',status='old')
          close (unit=itop,status='delete')
        end if
c
c     Ensure that the file ANA.PEK does not already exist
c     This is probably not necessary.  However, it is always
c     good to ensure that the file ANA.PEK is up to date.
c
        inquire (file='ana.pek',exist=lexist)
        if (lexist) then
          open (unit=ipek,file='ana.pek',status='old')
          close (unit=ipek,status='delete')
        end if
c
c     Ensure that the file ANA.PLT does not already exist
c     This is probably not necessary.  However, it is always
c     good to ensure that the file ANA.PLT is up to date.
c
        inquire (file='ana.plt',exist=lexist)
        if (lexist) then
          open (unit=iplt,file='ana.plt',status='old')
          close (unit=iplt,status='delete')
        end if
c
30      read(ic,1,end=9000) word
1       format(a4)
        icall = icall + 1
        if (icall.gt.1 .and. (word.eq.keys(2).or.word.eq.keys(5)
     *  .or.word.eq.skeys(2).or.word.eq.skeys(5)) ) then
          print 11,word
11        format (1x,'Cannot use ',a4,' simultaneously with TOPVAL, PEAK
     1, CUMFRQ, or EXTRCT')
          stop
        endif
c
        do 50 i=1,6
          if(word.eq. keys(i)) go to 70
          if(word.eq.skeys(i)) go to 70
50      continue
        write(iout,2) word
        print 2,word
2       format(/,'   Keyword not understood: ',a4)
        go to 9000
70      k=i
c
       go to (100,200,300,400,500,600), k
c
100     call fopen (ich,'conc01.bin','old','unformatted')
c  Take a peek of the title
        read(ich) title
        call page (iout,title)
        rewind (unit=ich)
        call cumfrq (ic,iout,ich,maxrec,rmet,conc,temp,wk1,wk2,
     1               title,xrecpt,yrecpt)
        close (unit=ich)
        go to 30
c
200     call fopen (ich,'conc01.bin','old','unformatted')
c  Take a peek of the title
        read(ich) title
        call page (iout,title)
        rewind (unit=ich)
        call fopen (ioutavg,'avconc.bin','unknown','unformatted')
        call averge (ic,iout,ich,ioutavg,maxrec,rmet,conc,temp,wk1,wk2,
     1               title,xrecpt,yrecpt)
        go to 30
c
300     call fopen (ich,'conc01.bin','old','unformatted')
        call fopen (itop,'ana.top','unknown','formatted')
c  Take a peek of the title
        read(ich) title
        call page (iout,title)
        rewind (unit=ich)
        call topval (ic,iout,ich,itop,maxrec,rmet,conc,wk1,wk2,wk3,
     1               wk4,wk5,title,xrecpt,yrecpt)
        close (unit=ich)
        go to 30
c
400     call fopen (ich,'conc01.bin','old','unformatted')
        call fopen (ipek,'ana.pek','unknown','formatted')
c  Take a peek of the title
        read(ich) title
        call page (iout,title)
        rewind (unit=ich)
        call peak (ic,iout,ich,ipek,maxrec,rmet,conc,temp,wk1,wk2,
     1             wk3,wk4,wk5,wk6,title,xrecpt,yrecpt)
        close (unit=ich)
        go to 30
c
500     call fopen (initseq,'conc01.bin','old','unformatted')
c  Take a peek of the title
        read(initseq) title
        title(1)=title(1)(1:71)//' *MERGED*'
        call page (iout,title)
        close (unit=initseq)
        call seqadd (ic,iout,ioutseq,initseq,maxrec,rmet,conc,temp,
     1               title,xrecpt,yrecpt)
c   Files will be opened within seqadd.
        go to 30
c
600     call fopen (ich,'conc01.bin','old','unformatted')
        call fopen (iplt,'ana.plt','unknown','formatted')
c  Take a peek of the title
        read(ich) title
        call page (iout,title)
        rewind (unit=ich)
        call extrct (ic,iout,ich,iplt,maxrec,conc,wk1,title,
     &               xrecpt,yrecpt)
        close (unit=ich)
        go to 30
c
9000    continue
        stop
        end
c
c--------------------------------------------------------------------
c  subroutine averge
c
c  purpose:
c       This routine creates a new file of running (overlapping)
c       averages for a user-specified length of period
c
c  parameters:
c  n       integer   number of hours to be averaged
c  nrecpt  integer   number of receptors
c
c--------------------------------------------------------------------
      subroutine averge (ic,iout,ich,ioutavg,maxrec,rmet,conc,
     1               temp,avg,sigma,title,xrecpt,yrecpt)
c
      dimension sigma(maxrec),conc(maxrec),rmet(7),temp(24,maxrec),
     1          avg(maxrec)
      dimension xrecpt(maxrec),yrecpt(maxrec)
      character*80 title(3)
      logical eof1
      ispt=1
c
c  Read header records from the binary file, then writes them out
c  to the averaged binary file
c
      call readhead(ich,title,nrecpt,maxrec,xrecpt,yrecpt)
      call writhead(ioutavg,title,nrecpt,maxrec,xrecpt,yrecpt)
c
c  Read and check the input data
c
      read(ic,*,err=8100) n
      if (n.le.1) then
        print 205,n
205     format(' AVERGE: illegal input for # no. of hours to be averaged
     1 = ',i7)
        stop
      endif
c
      call lines(10,iout,title)
      write(iout,45)
45    format(//,'   AVERGE')
      write(iout,1) n,nrecpt
1     format(//,
     15x,'# of hours to be averaged               = ',i10,/
     25x,'# of receptors                          = ',i10)
      call writrecp (iout,title,nrecpt,maxrec,xrecpt,yrecpt)
c
      do 500 i = 1,maxrec
        sigma(i) = 0.0
500   continue
1000  nr=n-1
      do 1500 irec=1,nr
        call readit(rmet,conc,nrecpt,ich,eof1)
        if(eof1) go to 8500
        ispt=ispt+1
        do 1200 j=1,nrecpt
          sigma(j)=sigma(j)+conc(j)
          temp(ispt,j)=conc(j)
          if (nr.eq.1)  temp(n,j)=0
1200    continue
1500  continue
      irec=0
1700  call readit(rmet,conc,nrecpt,ich,eof1)
      if (eof1) go to 5000
      ispt=ispt+1
      if (ispt.gt.n) ispt=1
      do 2000 j=1,nrecpt
        sigma(j)=sigma(j)+conc(j)-temp(ispt,j)
        avg(j)=sigma(j)/float(n)
        temp(ispt,j)=conc(j)
2000  continue
      call writit(rmet,avg,nrecpt,ioutavg)
      irec=irec+1
      go to 1700
5000  write(iout,10)irec
10    format(/,10x,i5,' records updated')
      print 221
221   format(/,' Normal termination of AVERGE')
      stop
c
8100  print 211
211   format(' AVERGE: illegal data line.')
      stop
8500  print 212
212   format(' AVERGE: unexpected end of file.')
      stop
      end
c
c--------------------------------------------------------------------
c  subroutine cumfrq
c
c  purpose:
c       This routine is used to compute cumulative frequency
c       distributions and average concentrations for up to MAXREC
c       receptors.
c
c
c  parameters:
c  dayin   real      number of days to be read
c  hourin  real      number of hours to be read
c  rfact   real      factor to convert internal concentration units
c                    (gm/m**3)
c  scale   real      scale factor for input concentrations
c  nh      integer   number of hours in the averaging period
c  nlev    integer   number of levels
c  nrecpt  integer   number of receptors
c  lev     real      concentration levels
c
c--------------------------------------------------------------------
      subroutine cumfrq (ic,iout,ich,maxrec,rmet,conc,fcat,val,viol,
     1               title,xrecpt,yrecpt)
      parameter (maxlev=20)
c
      dimension lev(maxlev),freq(maxlev+2)
      dimension rmet(7),conc(maxrec),fcat(24,maxrec),val(maxrec),
     1          viol(maxrec)
      dimension xrecpt(maxrec),yrecpt(maxrec)
      character*80 title(3)
      character*20 units
      real lev
      integer all
      logical eof1
      icnt=0
c
      if (maxlev+2.gt.24) then
        print 209
209     format(' CUMFRQ: need to allocate bigger space for the FCAT arra
     1y')
        stop
      endif
c
c  Read header records from the binary file
c
      call readhead(ich,title,nrecpt,maxrec,xrecpt,yrecpt)
c
c  Read and check the input data
c
      read(ic,*,err=8000) lp,nh,dayin,hourin,rfact,scale,nlev,units
      read(ic,*,err=8000) (lev(ii),ii=1,nlev)
      if (hourin.lt.0) then
        print 205,hourin
205     format(' CUMFRQ: illegal input for hourin = ',i7)
        stop
      endif
      if (dayin.lt.0) then
        print 204,dayin
204     format(' CUMFRQ: illegal input for dayin = ',i7)
        stop
      endif
      if (rfact.le.0.) then
        print 207,rfact
207     format(' CUMFRQ: illegal input for conversion factor = ',g9.2)
        stop
      endif
      if (scale.le.0.) then
        print 208,scale
208     format(' CUMFRQ: illegal input for scaling factor = ',g9.2)
        stop
      endif
      if (nlev.le.0 .or. nlev.gt.maxlev) then
        print 201,nlev,maxlev
201     format(' CUMFRQ: illegal input for # of conc. levels = ',i7,', t
     1he max. is = ',i7)
        stop
      endif
      if (nh.le.0) then
        print 202,nh
202     format(' CUMFRQ: illegal input for # of hours in each record = '
     1,i7)
        stop
      endif
      if (lp.le.0) then
        print 203,lp
203     format(' CUMFRQ: illegal input for # of records in an averaging 
     1period = ',i7)
        stop
      endif
      if (nh.ne.1) then
        print 231,nh
231     format(' CUMFRQ: Each record corresponds to ',i2,' hours, which 
     &is highly unusualy.',/,' Do you want to continue?')
        pause
      endif
c
      call lines(15+nlev,iout,title)
      write(iout,45)
45    format(//,'   CUMFRQ')
      write(iout,1) lp,nh,nrecpt,nlev,dayin,hourin,rfact,scale,units
1     format(//,
     15x,'# of records in an averaging period     = ',i10,/
     25x,'# of hours in each record               = ',i10,/
     35x,'# of receptors                          = ',i10,/
     45x,'# of conc. levels                       = ',i10,/
     55x,'DAYIN                                   = ',f10.0,/
     65x,'HOURIN                                  = ',f10.0,/
     75x,'conversion factor                       = ',1p,g10.2,/
     85x,'scaling factor                          = ',1p,g10.2,/
     95x,'conc. units                             = ',a20)
      write(iout,4) (i,lev(i),i=1,nlev)
4     format(5x,'Conc. level ',i2,' = ',g13.4)
      call writrecp (iout,title,nrecpt,maxrec,xrecpt,yrecpt)
c
      scale = scale*rfact
      all=1.
      if(hourin.gt.0.0.or.dayin.gt.0.0) all=0.
c  Two extra levels are reserved for 1) concentations higher than the
c  highest concentration level, and 2) averages.
      mlev=nlev+2
      lev(nlev+1) = 1.0e20
      hour=0.
      do 1300 i = 1,maxrec
        do 1200 j = 1,mlev
          fcat(j,i) = 0.0
1200    continue
        viol(i) = 0.0
1300  continue
      hourin=hourin+24.*dayin
c=========================
c     Start read loop
c=========================
1500  if(all.ne.1.and.hour.ge.hourin) go to 3000
      do 1700 i = 1,nrecpt
        val(i) = 0.0
1700  continue
      do 2500 ihr=1,lp
        if(icnt.eq.1) go to 2200
        call readit(rmet,conc,nrecpt,ich,eof1)
        if(eof1) go to 3000
        icnt=1
        go to 2300
2200    call readit(rmet,conc,nrecpt,ich,eof1)
        if(eof1) go to 3000
2300    do 2400 i=1,nrecpt
          val(i) = val(i) + scale*conc(i)/float(lp)
2400    continue
2500  continue
      hour=hour+float(lp*nh)
      do 2800 i=1,nrecpt
        do 2600 j=1,nlev
          if(val(i).le.lev(j)) go to 2700
2600    continue
        j = nlev + 1
2700    fcat(j,i)=fcat(j,i)+1.
        fcat(mlev,i)=fcat(mlev,i)+val(i)
2800  continue
      go to 1500
c=========================
c     End of read loop
c=========================
3000  if(hour.gt.0.0) go to 3100
      print 10
10    format(' CUMFRQ: Input file too short.')
      stop
3100  do 3300 i=1,nrecpt
        do 3200 j=1,mlev
          fcat(j,i)=fcat(j,i)*float(lp*nh)/hour
3200    continue
3300  continue
      lev(nlev+2) = hour
      do 6000 istart=1,nrecpt,16
        iend=min0(istart+15,nrecpt)
        do 5900 lstart=1,mlev,5
          lend=min0(lstart+4,mlev)
          call page (iout,title)
          write(iout,11) lp,hour
11        format(/,6x,'Cumulative frequencies of ',i5,
     x           '-hour averages for ',f5.0,' observations')
          if(lend.ne.mlev) go to 5700
          kt=(lend-lstart+1)
          go to (4100,4200,4300,4400,4500),kt
4100      write(iout,13)
13        format(/,'   ','receptor   iavg:# obs=  ')
          go to 4600
4200      write(iout,14)
14    format(/,'   ','receptor      ',1('   level    '),'avg:# obs=  ')
          go to 4600
4300      write(iout,15)
15    format(/,'   ','receptor      ',2('   level    '),'avg:# obs=  ')
          go to 4600
4400      write(iout,16)
16    format(/,'   ','receptor      ',3('   level    '),'avg:# obs=  ')
          go to 4600
4500      write(iout,17)
17    format(/,'   ','receptor      ',4('   level    '),'avg:# obs=  ')
4600      write(iout,18)(lev(il),il=lstart,lend)
18        format(15x,5(g11.4,' '))
          write(iout,*)
          go to 5750
5700      write(iout,25)
25        format(/,4x,'receptor      ',5('   level    '))
          write(iout,26)(lev(il),il=lstart,lend)
26        format(15x,5(1x,f10.1,' '))
          write(iout,*)
5750      continue
          do 5850 ir=istart,iend
            do 5800 j=lstart,lend
              viol(ir)=viol(ir)+fcat(j,ir)
              freq(j)=viol(ir)
5800        continue
            if(lend.ne.mlev) go to 5820
            freq(mlev)=fcat(mlev,ir)
            fcat(mlev,ir) = 0.0
5820        write(iout,30)ir, (fcat(il,ir),il=lstart,lend)
30          format(3x,i4,' freq    ',5(f10.4,'  '))
            write(iout,31) (freq(il),il=lstart,lend)
31          format(7x,   ' cum freq',5(f10.4,'  '))
            write(iout,*)
5850      continue
5900    continue
6000  continue
      print 221
221   format(/,' Normal termination of CUMFRQ')
      return
c
8000  print 41
41    format(' CUMFRQ: illegal data line.')
      stop
      end
c
c--------------------------------------------------------------------
c  subroutine extrct
c
c  purpose:
c       This subroutine extracts records that correspond to a
c       certain time period in the concentration time series, and
c       generates a space-delimited ASCII concentration file so
c       that isopleth can be plotted by an external plotting software
c       package.
c
c
c  parameters:
c  lp      integer   number of records in the averaging period
c  nh      integer   number of hours represented by each record
c  nrecpt  integer   number of receptors
c  rfact   real      factor to convert internal concentration units
c                    (gm/m**3)
c  scale   real      scale factor for input concentrations
c
c  maxprod int       the limit of the product of number of receptors
c                    and the number of concentration levels of interest.
c
c--------------------------------------------------------------------
      subroutine extrct (ic,iout,ich,iplt,maxrec,conc,avr,
     1               title,xrecpt,yrecpt)
c
      dimension avr(maxrec),conc(maxrec)
      dimension xrecpt(maxrec),yrecpt(maxrec)
      character*80 title(3)
      character*20 units
      logical eof1
c
c  Read header records from the binary file
c
      call readhead(ich,title,nrecpt,maxrec,xrecpt,yrecpt)
c
c  Read and check the input data
c
      read(ic,*,err=8000) lp,nh,dayend,hourend,rfact,scale,units
c
c  If dayend=0 AND hourend=0, then
c  the last hourly record (i.e., LP defaulted to 1) will be considered
c
      if (lp.le.0) then
        print 201,lp
201     format(' EXTRCT: illegal input for # of records in an averaging 
     1period = ',i7)
        stop
      endif
c
      if (nh.ne.1) then
        print 231,nh
231     format(' EXTRCT: Each record corresponds to ',i2,' hours, which 
     &is highly unusualy.',/,' Do you want to continue?')
        pause
      endif
c
      if (nh.le.0) then
        print 202,nh
202     format(' EXTRCT: illegal input for # of hours in each record = '
     1,i7)
        stop
      endif
c
      if (dayend.lt.0. .or. hourend.lt.0.) then
        print 203
203     format(' EXTRCT: illegal input for DAYEND or HOUREND, negative v
     1alues not allowed')
        stop
      end if
c
      if (dayend.eq.0. .and. hourend.gt.0.) then
        print 204
204     format(' EXTRCT: if DAYEND=0, then HOUREND must also =0')
        stop
      end if
c
      if (dayend.gt.0. .and. hourend.eq.0.) then
        print 205
205     format(' EXTRCT: if HOUREND=0, then DAYEND must also =0')
        stop
      end if
c
      if (rfact.le.0.) then
        print 207,rfact
207     format(' EXTRCT: illegal input for conversion factor = ',g9.2)
        stop
      endif
c
      if (scale.le.0.) then
        print 208,scale
208     format(' EXTRCT: illegal input for scaling factor = ',g9.2)
        stop
      endif
c
c     Default LP to 1 if necessary
c
      if (dayend.eq.0. .and. hourend.eq.0.) lp=1
c
      call lines(16,iout,title)
      write(iout,45)
45    format(//,'   EXTRCT')
      write(iout,1) lp,nh,nrecpt,dayend,hourend,rfact,scale,units
1     format(//,
     15x,'# of records in an averaging period     = ',i10,/
     25x,'# of hours in each record               = ',i10,/
     45x,'# of receptors                          = ',i10,/
     55x,'DAYEND                                  = ',f10.0,/
     65x,'HOUREND                                 = ',f10.0,/
     75x,'conversion factor                       = ',1p,g10.2,/
     85x,'scaling factor                          = ',1p,g10.2,/
     95x,'conc. units                             = ',a20)
      call writrecp (iout,title,nrecpt,maxrec,xrecpt,yrecpt)
c
      scale = scale*rfact
      do ir=1,nrecpt
        avr(ir) = 0.
      end do
c
      if (dayend.eq.0. .and. hourend.eq.0.) then
c
c *** Locate the last record
c
      itot=0
100   continue
         read (ich,end=99)
         itot=itot+1    ! Keep track of total no. of hours in the file
      goto 100
99    continue
      backspace (ich)   ! Need two BACKSPACE to position the pointer
      backspace (ich)   ! at the beginning of the last record
c
      else
c
c *** Find out how many records to skip
c
      iskip=(dayend-1)*24+hourend-lp
c
      if (iskip.gt.0) then
         do i=1,iskip
            read (ich,end=8500)
         end do
      end if
c
      end if
c
c *** Read data for the time period of interest
c
      do j=1,lp
        call readit(rmet,conc,nrecpt,ich,eof1)
        if(eof1) go to 8500
        do ir=1,nrecpt
          avr(ir)=avr(ir)+conc(ir)
        end do
      end do
c
      do ir=1,nrecpt
        avr(ir)=avr(ir)/float(lp)
      end do
c
c *** Write output (constant-width, 14 characters wide)
c
c     Calculate real values for DAYEND and HOUREND, when they both
c     equal zero.
c
      if (dayend.eq.0. .and. hourend.eq.0.) then
        if (mod(itot,24).eq.0) then
          dayend=itot/24
          hourend=24
        else
          dayend=itot/24+1
          hourend=mod(itot,24)
        end if
      end if
c
      write (iplt,'(a/a/a)') title
      write (iplt,309) dayend,hourend,nrecpt
309   format(3i14)
      do i=1,nrecpt
      write (iplt,311) xrecpt(i),yrecpt(i),avr(i)*scale
311   format(3f14.4)
      end do
c
      print 221
221   format(/,' Normal termination of EXTRCT')
      return
c
8000  print 209
209   format(' EXTRCT: illegal data line.')
      stop
8500  print 210
210   format(' EXTRCT: unexpected end of concentration file.')
      stop
      end
c
c-----------------------------------------------------------------------
c subroutine: fopen
c
c purpose:  A file-open utility program
c
c arguments passed:
c    variable   type    description
c      iunit    int     logical unit number of the file to be opened
c      cname    char    name of the file to be opened
c      cstatus  char    status of the file to be opened,
c                       'new', 'old', or 'unknown'
c      cform    char    form of the file to be opened,
c                       'formatted' or 'unformatted'
c
c calling routines: analysis
c
c-----------------------------------------------------------------------
c
      subroutine fopen (iunit,cname,cstatus,cform)
      character*(*) cname,cstatus,cform
      integer       iunit
      logical       lflag
      character*1   ians
c
c Check the validity of cstatus and cform
c
        if (cstatus.ne.'NEW' .and. cstatus.ne.'OLD' .and.
     1      cstatus.ne.'new' .and. cstatus.ne.'old' .and.
     2      cstatus.ne.'unknown' .and. cstatus.ne.'UNKNOWN') then
          print *,'Error in fopen: cstatus = ',cstatus,' not recognized'
          stop
        end if
c
        if (cform.ne.'FORMATTED' .and. cform.ne.'UNFORMATTED' .and.
     1      cform.ne.'formatted' .and. cform.ne.'unformatted') then
          print *,'Error in fopen: cform = ',cform,' not recognized'
          stop
        end if
c
        if (cstatus.eq.'NEW' .or. cstatus.eq.'new') then
c
c  Check whether the file already exists or not
          inquire (file=cname,exist=lflag)
          if (.not.lflag) then
            open (unit=iunit,file=cname,status=cstatus,form=cform)
          else
            print *,'File ',cname,' already exists, overwrite it ? (y/n,
     1 default=y)  '
            read 11,ians
            if (ians.eq.'N' .or. ians.eq.'n') stop
            open (unit=iunit,file=cname,status='unknown',form=cform)
          end if
c
        else if (cstatus.eq.'OLD' .or. cstatus.eq.'old') then
c
c  Check whether the file already exists or not
          inquire (file=cname,exist=lflag)
          if (lflag) then
            open (unit=iunit,file=cname,status=cstatus,form=cform)
          else
            print *,'File ',cname,' does not exist!'
            stop
          end if
c
        else if (cstatus.eq.'UNKNOWN' .or. cstatus.eq.'unknown') then
c
          open (unit=iunit,file=cname,status=cstatus,form=cform)
c
        end if
c
11      format(a1)
        return
        end
c
c--------------------------------------------------------------------
c  subroutine lines
c
c  purpose:
c       This routine counts the number of lines that have been printed
c       on a page.
c
c  arguments:
c       n       number of new lines
c       iout    output unit number
c       title   title of the run
c
c--------------------------------------------------------------------
      subroutine lines (n,iout,title)
c
      common /head/ npage,nline
      character*80 title(3)
      nline = nline + n
      if(nline.le.63) return
      call page (iout,title)
      nline = nline + n
      return
      end
c
c--------------------------------------------------------------------
c  subroutine page
c
c  purpose:
c       This routine skips to a new page and writes a page header
c
c--------------------------------------------------------------------
      subroutine page (iout,title)
c
      common /head/ npage,nline
      character*1 pb
      character*80 title(3)
      pb=char(12)
      npage=npage+1
      nline=5
      write(iout,1000) pb,npage
1000  format(a1,'  POST-PROCESSING ANALYSIS PROGRAM    LEVEL 970320',
     &4x,'PAGE',1x,i3,/)
      write(iout,1001) title
1001  format(a80,/,a80,/,a80,/)
      return
      end
c
c--------------------------------------------------------------------
c  subroutine peak
c
c  purpose:
c       This subroutine is used to identify non-overlapping
c       n-hour averages that exceed a user-specified threshold
c
c  parameters:
c  lp     integer   number of records in the averaging period
c  thr     real      threshold
c  dayin   real      number of days to be read
c  hourin  real      number of hours to be read
c  nh      integer   number of hours represented by each record
c  rfact   real      factor to convert internal concentration units
c                    (gm/m**3)
c  units   character units for labeling
c  lprint  character flag for detailed output
c  nrecpt  integer   number of receptors
c  scale   real      scale factor for input concentrations
c
c--------------------------------------------------------------------
      subroutine peak (ic,iout,ich,ipek,maxrec,air,conc,total,cmax,nn,
     1             day,hours,irecid,recval,title,xrecpt,yrecpt)
c
c  Strictly speaking, there is an error in the above statement.  This
c  is because in the calling routine, the last three arrays were
c  declared as real, while they are declared integer in the subroutine.
c  However, this should not cause any problem since all is really needed
c  is the space allocated for the three arrays, and the values of the
c  three arrays are of no consequence in the calling routine.  We can
c  always declare additional three integer working arrays; however, this
c  is not economical in terms of memroy allocation.
c
      logical eof1 
      dimension nn(maxrec),air(7),rmet(7,24),
     1          day(maxrec),hours(maxrec),conc(maxrec),
     2          total(24,maxrec),cmax(maxrec),metnme(7,2),
     3          irecid(maxrec),recval(maxrec)
      dimension xrecpt(maxrec),yrecpt(maxrec)
      dimension rmetavr(7)
      character*80 title(3)
      integer day,hours
      character*4 metnme
      character*20 units
      character*1  lprint
      data (metnme(i,1),i=1,7)
     &   /' MIX',' DIR','STAB',' SPD',' MIX','STAB',' SPD'/
      data (metnme(i,2),i=1,7)
     &   /' (w)','    ',' (w)',' (w)',' (l)',' (l)',' (l)'/
c
c  Read header records from the binary file
c
      call readhead(ich,title,nrecpt,maxrec,xrecpt,yrecpt)
c
c  Read and check the input data
c
      read(ic,*,err=8000) lp,nh,thr,dayin,hourin,rfact,scale,lprint,
     &                    units
      if (lp.le.0) then
        print 201,lp
201     format(' PEAK: illegal input for # of records in an averaging pe
     1riod = ',i7)
        stop
      endif
      if (thr.lt.0.) then
        print 202,thr
202     format(' PEAK: illegal input for threshold conc. = ',g9.2)
        stop
      endif
      if (dayin.lt.0) then
        print 204,dayin
204     format(' PEAK: illegal input for dayin = ',i7)
        stop
      endif
      if (hourin.lt.0) then
        print 205,hourin
205     format(' PEAK: illegal input for hourin = ',i7)
        stop
      endif
      if (nh.le.0) then
        print 203,nh
203     format(' PEAK: illegal input for # of hours in each record = ',i
     17)
        stop
      endif
      if (nh.ne.1) then
        print 231,nh
231     format(' PEAK: Each record corresponds to ',i2,' hours, which is
     & highly unusualy.',/,' Do you want to continue?')
        pause
      endif
      if (rfact.le.0.) then
        print 207,rfact
207     format(' PEAK: illegal input for conversion factor = ',g9.2)
        stop
      endif
      if (scale.le.0.) then
        print 208,scale
208     format(' PEAK: illegal input for scaling factor = ',g9.2)
        stop
      endif
c
      if (lprint.eq.'Y') lprint='y'
      if (lprint.eq.'N') lprint='n'
      if (lprint.ne.'y' .and. lprint.ne.'n') then
        print 209,lprint
209     format(' PEAK: illegal input for output flag = ',a7)
        stop
      endif
c
      call lines(18,iout,title)
      write(iout,45)
45    format(//,'   PEAK')
      write(iout,1) lp,nh,nrecpt,dayin,hourin,rfact,scale,thr,
     1              lprint,units
1     format(//,
     15x,'# of records in an averaging period     = ',i10,/
     25x,'# of hours in each record               = ',i10,/
     35x,'# of receptors                          = ',i10,/
     45x,'DAYIN                                   = ',f10.0,/
     55x,'HOURIN                                  = ',f10.0,/
     65x,'conversion factor                       = ',1p,g10.2,/
     75x,'scaling factor                          = ',1p,g10.2,/
     85x,'threshold conc. in external units       = ',1p,g10.2,/
     95x,'output amount flag                      = ',a1,/
     a5x,'conc. units                             = ',a20)
      call writrecp (iout,title,nrecpt,maxrec,xrecpt,yrecpt)
c
      write (ipek,'(a/a/a)') title
      write (ipek,*) nrecpt
      do i=1,nrecpt
      write (ipek,*) i,xrecpt(i),yrecpt(i)
      end do
c
      scale = scale*rfact
      hourin=hourin+24.*dayin
      hour=0
      mm=0
      all=1.
      if(hourin.gt.0.0.or.dayin.gt.0.0) all=0.
      icnt = 0
      do 1500 i = 1,maxrec
        cmax(i) = 0.0
        day(i) = 0
        hours(i) = 0
        nn(i) = 0
        irecid(i) = 0
        recval(i) = 0.0
1500  continue
      xlp = float(lp)
      call page (iout,title)
c=========================
c     Start read loop
c=========================
1550  if(all.ne.1..and.hour.ge.hourin) go to 4000
c
      delmw=0.
      delnw=0.
      delml=0.
      delnl=0.
      do k=1,7
        rmetavr(k)=0.
      end do
c
      do 2500 i=1,lp
        if(icnt.eq.1)go to 2200
        call readit(air,conc,nrecpt,ich,eof1)
        if(eof1) go to 8100
        icnt=1
        go to 2250
2200    call readit(air,conc,nrecpt,ich,eof1)
        if(eof1) go to 4000
2250    do 2300 l=1,7
          rmet(l,i)=air(l)
          rmetavr(l)=rmetavr(l)+air(l)/xlp   ! average met conditions
2300    continue
c
c ***   Calculate vector average winds
c
        trad=air(2)/57.29578
        sint=sin(trad)  ! resultant average wind
        cost=cos(trad)
        urw=air(4)*sint
        vrw=air(4)*cost
        url=air(7)*sint
        vrl=air(7)*cost
        delmw=delmw+urw/xlp
        delnw=delnw+vrw/xlp
        delml=delml+url/xlp
        delnl=delnl+vrl/xlp
        rmetavr(4)=sqrt(delnw*delnw+delmw*delmw)
        rmetavr(7)=sqrt(delnl*delnl+delml*delml)
        rmetavr(2)=angarc(delmw,delnw)
c
        do 2350 k=1,nrecpt
          conc(k)=conc(k)*scale
          if(i.lt.lp) total(i,k)=conc(k)
2350    continue
2500  continue
c
      mm=mm+1      ! Counter for the total number of averaging periods
      hour=hour+float(lp*nh)
c
      itot=0       ! Initialize counter for the number of receptors
c                  ! that show exceedances for the current averaging
c                  ! period.
      do 3800 k=1,nrecpt
        avr = 0.0
        do 2700 i = 1,lp-1
          avr = avr + total(i,k)/xlp
2700    continue
        avr = avr + conc(k)/xlp
        if(avr.le.cmax(k)) go to 3000
        cmax(k)=avr
        day(k)=int((hour-1.)/24.)+1
        hours(k)=amod(hour-1.,24.)+1
3000    continue
        if(avr.lt.thr) go to 3800
c
        itot=itot+1
        irecid(itot)=k
        recval(itot)=avr
c
        nn(k)=nn(k)+1
        iday=int((hour-1.)/24.)+1
        ihour=amod(hour-1.,24.)+1.
        call lines(3,iout,title)
        if(lprint.eq.'y') call lines(lp+3,iout,title)
        write(iout,11)thr,units,iday,ihour,k
11      format(/,2x,f10.4,1x,a20,' exceeded at day ',i4,' hour ',i2,
     x      2x, 'at receptor ',i3)
        if(lprint.eq.'n')go to 3700
        write(iout,12)units
12      format('  ****',1x,'concentrations (',a20,') ',7x,'weather')
        write(iout,41) (metnme(i,1),i=1,7),(metnme(i,2),i=1,7)
41      format('  RECORD',7x,'TOTAL',12x,7(1x,a4),/,32x,7(1x,a4))
        do 3600 i=1,lp-1
          write(iout,17)i,total(i,k),nint(rmet(1,i)),nint(rmet(2,i)),
     &                  nint(rmet(3,i)),rmet(4,i),nint(rmet(5,i)),
     &                  nint(rmet(6,i)),rmet(7,i)
17        format(5x,i2,4x,f10.4,11x,3i5,f5.1,2i5,f5.1)
3600    continue
        write(iout,17) lp,conc(k),nint(rmet(1,lp)),nint(rmet(2,lp)),
     &                 nint(rmet(3,lp)),rmet(4,lp),nint(rmet(5,lp)),
     &                 nint(rmet(6,lp)),rmet(7,lp)
3700    write(iout,16) avr,nint(rmetavr(1)),nint(rmetavr(2)),
     &                 nint(rmetavr(3)),rmetavr(4),nint(rmetavr(5)),
     &                 nint(rmetavr(6)),rmetavr(7)
16      format('  mean ',4x,f10.4,11x,3i5,f5.1,2i5,f5.1)
3800  continue
c
c *** Generate information for later plotting of maps
c
      call clen(len1,20,units) ! Find out the number of non-blank
c                              ! characters in UNITS
      if (itot.gt.0) then
         write (ipek,101) iday,ihour,itot,thr,''''//units(1:len1)//'''',
     &                  nint(rmetavr(1)),nint(rmetavr(2)),
     &                  nint(rmetavr(3)),rmetavr(4),nint(rmetavr(5)),
     &                  nint(rmetavr(6)),rmetavr(7)
101      format (i5,i3,i6,f12.4,2x,a20,3i5,f5.1,2i5,f5.1)
         do i=1,itot
            write (ipek,102) irecid(i),recval(i)
102         format (4x,i7,f12.4)
         end do
      end if
c
      go to 1550
c=========================
c     End of read loop
c=========================
4000  call page (iout,title)
      write(iout,19)units,mm,lp
19    format(/,' Total maximum conc. (',a20,') for ',i5,' (',
     x i2,'-hour) averaging period(s)',/)
      call lines(3,iout,title)
      do 5000 lstart = 1,nrecpt,8
        call lines(5,iout,title)
        lend = min0(lstart+7,nrecpt)
        write(iout,21)(l,l=lstart,lend)
21      format('  recep  ',8(2x,i3,3x))
        write(iout,*)
        write(iout,23) (cmax(k),k=lstart,lend)
23      format('   conc  ',8(f8.3))
        write(iout,24) (day(k),hours(k),k=lstart,lend)
24      format(' day/hr  ',8(i4,2x,i2),/)
5000  continue
c
      call page (iout,title)
      mmlp=mm*lp
      write(iout,27)lp,thr,units,mmlp
27    format(/,' Number of ',i2,'-hour averages above ',f10.4,1x,a20,
     x ' for ',i5,' hours',//,' receptors:',/)
      call lines(5,iout,title)
      do 7000 n=1,nrecpt,8
        call lines(2,iout,title)
        m=min0(n+7,nrecpt)
        write(iout,28)n,m,(nn(k),k=n,m)
28      format(1x,i3,'-',i3,' ',8(3x,i3,3x))
        write(iout,*)
7000  continue
      print 221
221   format(/,' Normal termination of PEAK')
      return
c
8000  print 210
210   format(' PEAK: illegal data line.')
      stop
8100  print 211
211   format(' PEAK: unexpected end of file.')
      stop
      end
c
c--------------------------------------------------------------------
c  subroutine readhead
c
c  purpose:
c       This routine reads the header records from the binary file.
c
c  arguments:
c       ich     input channel for reading data
c       title   title of the run
c       nrecpt  # of receptors
c       maxrec  max. # of receptors
c       xrecpt  x-coord., m, of receptors
c       yrecpt  y-coord., m, of receptors
c
c--------------------------------------------------------------------
      subroutine readhead (ich,title,nrecpt,maxrec,xrecpt,yrecpt)
c
      character*80 title(3)
      dimension xrecpt(maxrec),yrecpt(maxrec)
c
      read(ich) title
      read(ich) nrecpt,(xrecpt(i),yrecpt(i),i=1,nrecpt)
      if (nrecpt.le.0 .or. nrecpt.gt.maxrec) then
        print 10,nrecpt,maxrec
10      format(' READHEAD: illegal input for nrecpt = ',i7,', the max.
     1 is = ',i7)
        stop
      endif
c
      return
      end
c
c--------------------------------------------------------------------
c  subroutine readit
c
c  purpose:
c       This routine reads one concentration file record, which has up
c       to MAXREC receptor concentrations.
c
c  arguments:
c       rmet    array containing meteorology: mix(water),wd,ist(water),
c               ws(water),mix(land),ist(land),ws(land)
c       conc    array of size nrec to receive concentration values
c       nrec    # of concentrations to be read for each record
c       ich     input channel for reading data
c       eof1    end of file logical variable
c
c--------------------------------------------------------------------
      subroutine readit (rmet,conc,nrec,ich,eof1)
c
      logical eof1
      dimension conc(nrec),rmet(7)
c
      eof1 = .false.
      read(ich,end=1000) rmet,conc
      return
1000  eof1 = .true.
      return
      end
c
c--------------------------------------------------------------------
c  subroutine seqadd
c
c  purpose:
c      This routine creates a new file of concentrations added (and
c      possibly scaled) from a total of 1 to 12 input concentration
c      files.
c
c  parameters:
c  n       integer   number of concentration files to be processed
c  nrecpt  integer   number of receptors
c  scale   real      scale factor for each file
c
c--------------------------------------------------------------------
      subroutine seqadd (ic,iout,ioutseq,initseq,maxrec,rmet,conc,temp,
     1               title,xrecpt,yrecpt)
      parameter (maxfile=12)
c
      dimension conc(maxrec),rmet(7),temp(maxrec),scale(maxfile)
      dimension xrecpt(maxrec),yrecpt(maxrec)
      character*80 title(3),titlen(3)
      character k2*2,fname*40
      logical eof1
c
c  Read header records from the first binary file
c
      call fopen (initseq,'conc01.bin','old','unformatted')
      call readhead(initseq,title,nrecpt,maxrec,xrecpt,yrecpt)
      title(1)=title(1)(1:71)//' *MERGED*'
      close (initseq)
c
c  read and check the input data
c
      read(ic,*,err=8100) n
      read(ic,*,err=8100) (scale(i),i=1,n)
      if(n.lt.1 .or. n.gt.maxfile) then
        print 205,n,maxfile
205     format(' SEQADD: illegal input for # of files to be merged = ',i
     17,', the max. is = ',i7)
        stop
      endif
c
      call lines(10+n,iout,title)
      write(iout,25)
25    format(//,'   SEQADD')
      write(iout,1) n,nrecpt
1     format(//,
     15x,'# of files to be merged                 = ',i10,/
     25x,'# of receptors                          = ',i10)
      write(iout,4) (i,scale(i),i=1,n)
4     format(5x,'Scaling factor for file ',i2,' = ',g13.4)
      call writrecp (iout,title,nrecpt,maxrec,xrecpt,yrecpt)
c
c     Open merged output binary file, and write the header records
c     to the merged binary file
c
      call fopen (ioutseq,'sqconc.bin','unknown','unformatted')
      call writhead(ioutseq,title,nrecpt,maxrec,xrecpt,yrecpt)
c
c     open necessary input binary files
c
      do 20 i=1,n
        iu = initseq + i-1
        write(k2,'(i2.2)') i
        fname='conc'//k2//'.bin'
        call fopen (iu,fname,'old','unformatted')
        call readhead(iu,titlen,ndummy,maxrec,xrecpt,yrecpt)
        if (ndummy.ne.nrecpt) then
          print 207,nrecpt,ndummy,fname
207       format(' SEQADD: inconsistent number of receptors found, expec
     1ting ',i4,/,' but found ',i4,' from file ',a12)
          stop
        end if
20    continue
c
      irec = 0
300   do 500 i = 1,maxrec
        conc(i) = 0.0
500   continue
      do 1000 ifile=1,n
        ich = initseq + ifile -1
        call readit(rmet,temp,nrecpt,ich,eof1)
        if(eof1) go to 5000
        do 800 j=1,nrecpt
          conc(j) = conc(j) + temp(j)*scale(ifile)
800     continue
1000  continue
      call writit(rmet,conc,nrecpt,ioutseq)
      irec = irec + 1
      go to 300
5000  write(iout,10)irec
10    format(//,10x,i5,' hours summed')
      print 221
221   format(/,' Normal termination of SEQADD')
      stop
c
8100  print 211
211   format(' SEQADD: illegal data line.')
      stop
      end
c
c--------------------------------------------------------------------
c  subroutine topval
c
c  purpose:
c       This subroutine displays the highest n concentrations
c       (user-specified) modeled at each receptor.
c
c
c  parameters:
c  lp      integer   number of records in the averaging period
c  nh      integer   number of hours represented by each record
c  nm      integer   number of topmost averages values to be printed
c                    for each receptor
c  lm      integer   number of topmost averages values to be printed
c                    for each averaging period
c  dayin   real      number of days to be read
c  hourin  real      number of hours to be read
c  nrecpt  integer   number of receptors
c  rfact   real      factor to convert internal concentration units
c                    (gm/m**3)
c  scale   real      scale factor for input concentrations
c
c  maxtop  int       the top maxtop highest and second-highest values
c                    of all the receptors will also be analyzed.
c  maxprod int       the limit of the product of number of receptors
c                    and the number of concentration levels of interest.
c
c--------------------------------------------------------------------
      subroutine topval (ic,iout,ich,itop,maxrec,air,conc,avr,t1,t2,
     1               indx,irank,title,xrecpt,yrecpt)
      parameter (maxtop=25)
      parameter (maxprod=50000)
c
      dimension value(maxprod+1),avr(maxrec),day(maxprod+1),
     1 hours(maxprod+1),air(7),conc(maxrec),jx(maxprod+1),
     2 t1(maxrec),t2(maxrec),t1rec(maxtop),
     3 t2rec(maxtop),t3(maxtop),t4(maxtop),
     4 indx(maxrec),irank(maxrec)
      dimension rmet(7,24),rmetavr(7)
      dimension xrecpt(maxrec),yrecpt(maxrec)
      character*80 title(3)
      character*20 units
      character*4 metnme(7,2)
      integer all,day,hours,t1rec,t2rec
      logical eof1
      icnt=0
      data (metnme(i,1),i=1,7)
     &   /' MIX',' DIR','STAB',' SPD',' MIX','STAB',' SPD'/
      data (metnme(i,2),i=1,7)
     &   /' (w)','    ',' (w)',' (w)',' (l)',' (l)',' (l)'/
c
c  Read header records from the binary file
c
      call readhead(ich,title,nrecpt,maxrec,xrecpt,yrecpt)
c
c  Read and check the input data
c
      read(ic,*,err=8000) lp,nh,nm,lm,dayin,hourin,rfact,scale,units
      if (lp.le.0) then
        print 201,lp
201     format(' TOPVAL: illegal input for # of records in an averaging 
     1period = ',i7)
        stop
      endif
      if (nh.le.0) then
        print 202,nh
202     format(' TOPVAL: illegal input for # of hours in each record = '
     1,i7)
        stop
      endif
      if (nh.ne.1) then
        print 231,nh
231     format(' TOPVAL: Each record corresponds to ',i2,' hours, which 
     &is highly unusualy.',/,' Do you want to continue?')
        pause
      endif
      if (nm.le.0) then
        print 203,nm
203     format(' TOPVAL: illegal input for # of topmost average values =
     1 ',i7,' for each receptor')
        stop
      endif
      if (lm.le.0) then
        print 216,lm
216     format(' TOPVAL: illegal input for # of topmost average values =
     1 ',i7,' for each averaging period')
        stop
      endif
      if (lm.gt.nrecpt) then
        print 217,lm
217     format(' TOPVAL: illegal input for # of topmost average values =
     1 ',i7,' for each averaging period')
        stop
      endif
      if (dayin.lt.0) then
        print 204,dayin
204     format(' TOPVAL: illegal input for dayin = ',i7)
        stop
      endif
      if (hourin.lt.0) then
        print 205,hourin
205     format(' TOPVAL: illegal input for hourin = ',i7)
        stop
      endif
      if (rfact.le.0.) then
        print 207,rfact
207     format(' TOPVAL: illegal input for conversion factor = ',g9.2)
        stop
      endif
      if (scale.le.0.) then
        print 208,scale
208     format(' TOPVAL: illegal input for scaling factor = ',g9.2)
        stop
      endif
      if(nm*nrecpt.gt.maxprod .or. nm*nrecpt.le.0) then
        print 213,nm,nrecpt,maxprod
213     format(' TOPVAL: product of # topmost values = ',i5,' and # rece
     1ptors = ',i5,/,' out of range, the max. is = ',i7)
        stop
      end if
c
      call lines(16,iout,title)
      write(iout,45)
45    format(//,'   TOPVAL')
      write(iout,1) lp,nh,nm,lm,nrecpt,dayin,hourin,rfact,scale,units
1     format(//,
     15x,'# of records in an averaging period     = ',i10,/
     25x,'# of hours in each record               = ',i10,/
     35x,'# of topmost values for each recetpor   = ',i10,/
     35x,'# of topmost values for each period     = ',i10,/
     45x,'# of receptors                          = ',i10,/
     55x,'DAYIN                                   = ',f10.0,/
     65x,'HOURIN                                  = ',f10.0,/
     75x,'conversion factor                       = ',1p,g10.2,/
     85x,'scaling factor                          = ',1p,g10.2,/
     95x,'conc. units                             = ',a20)
      call writrecp (iout,title,nrecpt,maxrec,xrecpt,yrecpt)
c
      write (itop,'(a/a/a)') title
      write (itop,*) nrecpt
      do i=1,nrecpt
      write (itop,*) i,xrecpt(i),yrecpt(i)
      end do
c
      scale = scale*rfact
      hourin=hourin+24.*dayin
      if(nm.eq.1) nm = 2
      all=1.
      if(hourin.gt.0.0.or.dayin.gt.0.0) all=0.
      hour=0.
      do 1200 i = 1,maxprod+1
        value(i) = 0.0     ! Stores top NM conc. at each receptor
        day(i) = 0         ! Stores the day info. for VALUE
        hours(i) = 0       ! Stores the hour info. for VALUE
1200  continue
      xlp = float(lp)
c=========================
c     Start read loop
c=========================
1500  if(all.ne.1.and.hour.ge.hourin) go to 4000
      do 1600 ir=1,nrecpt
        avr(ir) = 0.
        indx(ir) = 0
        irank(ir) = 0
1600  continue
c
      delmw=0.
      delnw=0.
      delml=0.
      delnl=0.
      do k=1,7
        rmetavr(k)=0.
      end do
c
      do 2100 j=1,lp
        if(icnt.eq.1) go to 1800
        call readit(air,conc,nrecpt,ich,eof1)
        if(eof1) go to 8500
        icnt=1
        go to 1900
1800    call readit(air,conc,nrecpt,ich,eof1)
        if(eof1) go to 4000
1900    do l=1,7
          rmet(l,j)=air(l)
          rmetavr(l)=rmetavr(l)+air(l)/xlp   ! average met conditions
        end do
c
c ***   Calculate vector average winds
c
        trad=air(2)/57.29578
        sint=sin(trad)  ! resultant average wind
        cost=cos(trad)
        urw=air(4)*sint
        vrw=air(4)*cost
        url=air(7)*sint
        vrl=air(7)*cost
        delmw=delmw+urw/xlp
        delnw=delnw+vrw/xlp
        delml=delml+url/xlp
        delnl=delnl+vrl/xlp
        rmetavr(4)=sqrt(delnw*delnw+delmw*delmw)
        rmetavr(7)=sqrt(delnl*delnl+delml*delml)
        rmetavr(2)=angarc(delmw,delnw)
c
        do 2000 ir=1,nrecpt
          avr(ir)=avr(ir)+conc(ir)
2000    continue
2100  continue
c
      hour=hour+float(lp*nh)
c
      do 3000 ir=1,nrecpt
        iday=int((hour-1.)/24.)+1
        ihour=amod(hour-1.,24.)+1.
        avr(ir)=avr(ir)/float(lp)
        k=(ir-1)*nm
        im=nm
2200    kim=k+im
        kim1=kim -1
        if(avr(ir).le.value(kim)) go to 2500
        if(im.eq.1) go to 2450
        value(kim)=value(kim1)  ! Stores top NM conc. at each receptor
        day(kim)=day(kim1)      ! Stores the day info. for VALUE
        hours(kim)=hours(kim1)  ! Stores the hour info. for VALUE
        xday=day(kim)
        xhours=hours(kim)
        im=im-1
        go to 2200
2450    im = 0
2500    if(im.ge.nm) go to 3000
        kimp1=k+im+1
        value(kimp1)=avr(ir)    ! Stores top NM conc. at each receptor
        day(kimp1)=int((hour-1.)/24.)+1  ! Stores day info. for VALUE
        hours(kimp1)=amod(hour-1.,24.)+1 ! Stores hour info. for VALUE
        xday=day(kimp1)
        xhours=hours(kimp1)
3000  continue
c
      call clen(len1,20,units) ! Find out the number of non-blank
c                              ! characters in UNITS
c
c *** Rank the average concentrations for the current period
c
      call makerank (nrecpt,avr,indx,irank)
      write (itop,101)  iday,ihour,lm,''''//units(1:len1)//'''',
     &                  nint(rmetavr(1)),nint(rmetavr(2)),
     &                  nint(rmetavr(3)),rmetavr(4),nint(rmetavr(5)),
     &                  nint(rmetavr(6)),rmetavr(7)
101   format(i5,i3,i6,2x,a20,3i5,f5.1,2i5,f5.1)
      do i = nrecpt,nrecpt-lm+1,-1
         write (itop,102) indx(i),avr(indx(i))*scale
102      format(i7,f12.4)
      end do
c
      go to 1500
c=========================
c     End of read loop
c=========================
c
c     Get top maxtop (25) highest and second-highest values
c
4000  do 4100 i = 1,nrecpt
        t1(i) = value((i-1)*nm + 1) * scale
        t2(i) = value((i-1)*nm + 2) * scale
4100  continue
      do 4150 i = 1,maxtop
        t1rec(i) = i
        t2rec(i) = i
        t3(i) = 0.0
        t4(i) = 0.0
4150  continue
      nlim = min0(maxtop,nrecpt)
      do 4300 j = 1,nlim
        do 4250 i = 1,nrecpt
          if(t1(i).le.t3(j)) go to 4200
          t3(j) = t1(i)
          t1rec(j) = i
4200      continue
          if(t2(i).le.t4(j)) go to 4250
          t4(j) = t2(i)
          t2rec(j) = i
4250    continue
        t1(t1rec(j)) = 0.0
        t2(t2rec(j)) = 0.0
4300  continue
      do 4400 i=1,maxprod
        value(i)=value(i)*scale
4400  continue
c
c  Each printed page contains information for 16 receptors
c
      do 6000 istart=1,nrecpt,16
        iend=min0(istart+15,nrecpt)
        do 5500 lstart=1,nm,5
          lend=min0(lstart+4,nm)
          call page (iout,title)
          do 4500 im=lstart,lend
            jx(im)=im
4500      continue
          write(iout,10)nm,lp,hour
10        format(/,7x,'Top ',i3,' concentrations of ',i2,
     x           '-hour averages ','for ',f6.0,' hours')
          write(iout,11)(jx(il),il=lstart,lend)
11        format(/,4x,'receptor    ',5(3x,'top ',i3,'  '))
          write(iout,*)
          do 5000 ir=istart,iend
            k=(ir-1)*nm
            lst1=lstart+k
            lnd1=lend+k
            write(iout,13)ir,(value(kx),kx=lst1,lnd1)
13          format(6x,i3,7x,5(f10.4,2x))
            write(iout,14)(day(kil),hours(kil),kil=lst1,lnd1)
14          format(4x,'day/hour',4x,5(2i5,2x))
            write(iout,*)
5000      continue
5500    continue
6000  continue
c
      call page (iout,title)
      write(iout,15) nlim
15    format(/,' Top ',i2,' highest and second-highest concentrations',
     x //)
      write(iout,16)
16    format(18x,'Highest',22x,'Second-highest',//,
     x 2x,'rank',5x,'receptor   concentration',8x,
     x 'receptor   concentration',//)
      write(iout,17) (j,t1rec(j),t3(j),t2rec(j),t4(j),j=1,nlim)
17    format(3x,i2,9x,i3,7x,f10.3,12x,i3,7x,f10.3,/)
      print 221
221   format(/,' Normal termination of TOPVAL')
      return
c
8000  print 209
209   format(' TOPVAL: illegal data line.')
      stop
8500  print 210
210   format(' TOPVAL: unexpected end of concentration file.')
      stop
      end
c
c--------------------------------------------------------------------
c  subroutine writhead
c
c  purpose:
c       This routine writes the header records to the binary file.
c
c  arguments:
c       ich     output channel for reading data
c       title   title of the run
c       nrecpt  # of receptors
c       maxrec  max. # of receptors
c       xrecpt  x-coord., m, of receptors
c       yrecpt  y-coord., m, of receptors
c
c--------------------------------------------------------------------
      subroutine writhead (ich,title,nrecpt,maxrec,xrecpt,yrecpt)
c
      character*80 title(3)
      dimension xrecpt(maxrec),yrecpt(maxrec)
c
      write(ich) title
      write(ich) nrecpt,(xrecpt(i),yrecpt(i),i=1,nrecpt)
c
      return
      end
c
c--------------------------------------------------------------------
c  subroutine writit
c
c  purpose:
c       This routine writes one concentration file record, which has up
c       to MAXREC receptor concentrations.
c
c  arguments:
c       rmet    array containing meteorology: mix(water),wd,ist(water),
c               ws(water),mix(land),ist(land),ws(land)
c       conc    array of size nrec to receive concentration values
c       nrec    # of concentrations to be writ for each record
c       ich     output channel for writing data
c
c--------------------------------------------------------------------
      subroutine writit (rmet,conc,nrec,ich)
c
      dimension conc(nrec),rmet(7)
c
      write(ich) rmet,conc
      return
      end
c
c--------------------------------------------------------------------
c  subroutine writrecp
c
c  purpose:
c       This routine writes the receptor locations to the ASCII
c       output file.
c
c  arguments:
c       iout    output channel for reading data
c       title   title of the run
c       nrecpt  # of receptors
c       maxrec  max. # of receptors
c       xrecpt  x-coord., m, of receptors
c       yrecpt  y-coord., m, of receptors
c
c--------------------------------------------------------------------
      subroutine writrecp (iout,title,nrecpt,maxrec,xrecpt,yrecpt)
c
      character*80 title(3)
      dimension xrecpt(maxrec),yrecpt(maxrec)
c
      call lines(2,iout,title)
      write(iout,11)
11    format(//,5x,'Receptor Locations (m):',//)
      do 10 i=1,nrecpt,2
      i1=i
      i2=i+1
      call lines(1,iout,title)
      if (i2.le.nrecpt) then
      write(iout,21) i1,xrecpt(i1),yrecpt(i1),i2,xrecpt(i2),yrecpt(i2)
21    format(5x,i4,': (',f12.1,',',f12.1,')',
     1       5x,i4,': (',f12.1,',',f12.1,')')
      else
      write(iout,21) i1,xrecpt(i1),yrecpt(i1)
      end if
10    continue
c
      return
      end
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      function angarc (delm,deln)
C
C PURPOSE: DETERMINES APPROPRIATE ANGLE OF TAN(ANG) = DELM/DELN
C          WHICH IS REQUIRED FOR CALCULATION OF RESULTANT
C          WIND DIRECTION.
C
C I/O:   DELM, AVERAGE WIND COMPONENT IN THE EAST DIRECTION.
C        DELN, AVERAGE WIND COMPONENT IN THE NORTH DIRECTION.
C
C       MINERALS MANAGEMENT SERVICE
C       U.S. DEPARTMENT OF THE INTERIOR
C
C OCD             REVISION HISTORY:
C    DCD 880906   CREATED.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      if (deln) 10,40,80
10    if (delm) 20,30,20
c
20    angarc=57.29578*atan(delm/deln)+180.
      return
30    angarc=180.
      return
40    if (delm) 50,60,70
50    angarc=270.
      return
60    angarc=0.
c                                                                       
c     angarc=0. indicates indeterminate angle
c
      return
70    angarc=90.
      return
80    if (delm) 90,100,110
90    angarc=57.29578*atan(delm/deln)+360.
      return 
100   angarc=360.
      return
110   angarc=57.29578*atan(delm/deln)
c
      return
      end
c
      subroutine makerank (n,arr,indx,irank)
c----------------------------------------------------------------
c
c  To create an index table and a rank table for an array.
c
c  The index table is a table that contains pointers telling which number
c  array element comes first (i.e., the smallest) in numerical order,
c  which second, and so on.  For example, INDX(1)=4 means that the 4th
c  array elemet is the smallest, and INDX(3)=1 means that the 1st array
c  element is the third smallest.
c
c  The rank table is a table that contains the numerical rank of
c  the array elements.  A rank table's jth entry give the rank of the
c  jth element of the original array, ranging from 1 (if that element
c  ws the smallest) to N (if that element was the largest).  For example,
c  IRANK(1)=3 means that the 1st array element is the third smallest, and
c  IRANK(3)=4 means that the 3rd array element is the fourth smallest.
c
c  Subroutines INDEXX and RANK from Numerical Recipes are used to
c  create the index and rank tables.
c
c  The driver program MAKERANK written by:
c
c  Joseph C. Chang  (508) 371-4256, chang@src.com
c  EARTH TECH
c  196 Baker Avenue
c  Concord, MA 01742
c  Fax (508) 371-4280
c
c  Input:
c  ------
c  N      integer      dimension of array
c  ARR    real         array, dimensioned N
c
c  Output:
c  -------
c  INDX   integer      index table, dimensioned N
c  IRANK  integer      rank table, dimensioned N
c
c----------------------------------------------------------------
      integer n,indx(n),irank(n)
      real arr(n)
c
c *** Calculate index table
c
      call indexx (n,arr,indx)
c
c *** Calculate rank table
c
      call rank (n,indx,irank)
c
      return
      end
c
      SUBROUTINE indexx(n,arr,indx)
c     This routine is adapted from
c     Press et al. (1986): Numerical Recipes, Cambridge U. Press
      INTEGER n,indx(n),M,NSTACK
      REAL arr(n)
      PARAMETER (M=7,NSTACK=50)
      INTEGER i,indxt,ir,itemp,j,jstack,k,l,istack(NSTACK)
      REAL a
      do 11 j=1,n
        indx(j)=j
11    continue
      jstack=0
      l=1
      ir=n
1     if(ir-l.lt.M)then
        do 13 j=l+1,ir
          indxt=indx(j)
          a=arr(indxt)
          do 12 i=j-1,1,-1
            if(arr(indx(i)).le.a)goto 2
            indx(i+1)=indx(i)
12        continue
          i=0
2         indx(i+1)=indxt
13      continue
        if(jstack.eq.0)return
        ir=istack(jstack)
        l=istack(jstack-1)
        jstack=jstack-2
      else
        k=(l+ir)/2
        itemp=indx(k)
        indx(k)=indx(l+1)
        indx(l+1)=itemp
        if(arr(indx(l+1)).gt.arr(indx(ir)))then
          itemp=indx(l+1)
          indx(l+1)=indx(ir)
          indx(ir)=itemp
        endif
        if(arr(indx(l)).gt.arr(indx(ir)))then
          itemp=indx(l)
          indx(l)=indx(ir)
          indx(ir)=itemp
        endif
        if(arr(indx(l+1)).gt.arr(indx(l)))then
          itemp=indx(l+1)
          indx(l+1)=indx(l)
          indx(l)=itemp
        endif
        i=l+1
        j=ir
        indxt=indx(l)
        a=arr(indxt)
3       continue
          i=i+1
        if(arr(indx(i)).lt.a)goto 3
4       continue
          j=j-1
        if(arr(indx(j)).gt.a)goto 4
        if(j.lt.i)goto 5
        itemp=indx(i)
        indx(i)=indx(j)
        indx(j)=itemp
        goto 3
5       indx(l)=indx(j)
        indx(j)=indxt
        jstack=jstack+2
        if(jstack.gt.NSTACK)pause 'NSTACK too small in indexx'
        if(ir-i+1.ge.j-l)then
          istack(jstack)=ir
          istack(jstack-1)=i
          ir=j-1
        else
          istack(jstack)=j-1
          istack(jstack-1)=l
          l=i
        endif
      endif
      goto 1
      END
c
      SUBROUTINE rank(n,indx,irank)
c     This routine is adapted from
c     Press et al. (1986): Numerical Recipes, Cambridge U. Press.
      INTEGER n,indx(n),irank(n)
      INTEGER j
      do 11 j=1,n
        irank(indx(j))=j
11    continue
      return
      END
c
c-----------------------------------------------------------------------
      subroutine clen(len,lena,a)
c-----------------------------------------------------------------------
c
c CLEN          Version: 1.0            Level: 910530
c
c       Joseph C. Chang
c       EARTH TECH
c       196 Baker Avenue
c       Concord, MA 01742
c       Tel (508) 371-4256
c       Fax (508) 371-4280
c       e-mail chang@src.com, or
c              jchang@earthtech.com
c
c PURPOSE:      Determine the lenght of a character string "a" after
c               ignoring all the trailing blanks.
c
c ARGUMENTS:
c  len     integer   length of the character "a" neglecting all the
c                    trailing blanks
c  lena    integer   original length of the character "a" declared in
c                    the calling program
c  a       character the character string whose length is to be determined
c
c CALLING ROUTINES:
c
c EXTERNAL ROUTINES:    none
c
c NOTE:
c  Leading blanks will be left intact.
c-----------------------------------------------------------------------
        character *(*) a
        do 10 i=lena,1,-1
        if (a(i:i).ne.' ') goto 11
10      continue
        print *,'WARNING IN CLEN:, "a" is all blanks!'
        len=0
        return
11      len=i
        return
        end
