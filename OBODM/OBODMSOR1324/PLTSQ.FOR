CC-OBODM graphics related routines.
      Subroutine PLTSQ(jflg)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - PLTSQ Version 01.1                          ---
CC--- Subroutine description - open and close plot files or devices ---
CC---------------------------------------------------------------------
CC
CC       jflg - flag to specify initialize (=1), close (=2),
CC
CC**********************************************************************
CC
      integer*2 imode                                                   031297
      character*1 mfdef
      character*3 mport
      character*5 mbufr
      character*22 mfnam,mlptn
CC
      dimension mport(2),mfdef(12)
CC
      include 'OCNTRL.INC'
      include 'OWNTRL.INC'
      include 'ONAMES.INC'
CC
      equivalence (mfdef(1),mfnam),(mfdef(4),mbufr)
CC
      data mfnam/'OBD     .PLT'/,
     *     mlptn/' '/,
     *     mport/'LPT','COM'/
CC
CC**********************************************************************
CC
      iferrs = 0
      if (jflg .eq. 1) then
CC
CC        initialize plotting.
        ibpage = 1
        idpage = 0
        ispage = idpage
        call STRPQ
        iswplt(29) = 1
CCPCITS
        call CLSMS
CCPCITE
      else
CC
CC        terminate current plot - jflg = 2.
        iswplt(26) = 0
        call PLT2Q(0.0,0.0,3)
        if (iplfil .gt. 0) then
          WRITE (iplfil,9000,err=10,iostat=ier)
          go to 20
   10     i = KERRS(2,mplnam,ier,0,xdum,xdum,xdum,xdum,0)
          iplfil = 0
        end if
   20   if (iscfil .gt. 0) then
          WRITE (iscfil,9000,err=30,iostat=ier)
          REWIND iscfil
          go to 40
   30     i = KERRS(2,mnpbuf,ier,0,xdum,xdum,xdum,xdum,0)
          iscfil = 0
        end if
   40   isavc = ifored
        if (iswplt(9) .eq. ifored) ifored = mod(ifored+1,7)
CC        temporarily save next segment and set segment pointer
CC        as if plotting.
        isvnx = ifnext
        ifnext = 20
CC        if hard copy set to yes, generate.
        if (ifhcpy .eq. 0.or.ifhcpy .gt. 11.or.iscfil .eq. 0) then
          imsg = 40
        else
          imsg = 41
        end if
        iferrs = KERRS(-imsg,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0)
        if (iferrs .eq. 1.or.iferrs .eq. 2) go to 90
        if (imsg .eq. 41) then
          if (iferrs .eq. 3) then
            ihdfl = 33
            CLOSE (unit=ihdfl,status='keep',err=50)
   50       if (ifprno .eq. 0.or.ifprno .eq. 4) then
              n = 0
   60         n = n + 1
              if (n .le. 99999) then
                WRITE (mbufr,9001) n
                OPEN (unit=ihdfl,file=mfnam,iostat=ier,err=60,
     *                status='new')
                if (iotfil .gt. 0) then
                  WRITE (iotfil,9002) nplots,mfnam
                end if
                j = KERRS(-42,mnpbuf,n,idum,xdum,xdum,xdum,xdum,0)
              else
CC                cannot open hard copy plot file, files OBD00001.PLT
CC                through OBD99999.PLT already exits.
                ifhcpy = 0
                iscfil = 0
                i = KERRS(45,mfnam,ier,idum,xdum,xdum,xdum,xdum,0)
                go to 80
              end if
            else
              if (ifprno .gt. 4.and.ifprno .lt. 7) then
                j = 2
                k = 4
              else if (ifprno .gt. -20.and.ifprno .lt. -10) then
                j = 2
                k = 10
              else
                j = 1
                k = 0
              end if
              WRITE (mlptn,9003) mport(j),IABS(ifprno)-k
              OPEN (unit=ihdfl,file=mlptn,iostat=ier,err=70,
     *              status='old')
              if (j .eq. 1) then
                l = KERRS(-43,mnpbuf,IABS(ifprno)-k,idum,xdum,xdum,
     *                    xdum,xdum,0)
              else
                l = KERRS(-44,mnpbuf,IABS(ifprno)-k,idum,xdum,xdum,
     *                    xdum,xdum,0)
              end if
            end if
            ifccok = 2
            if (ifhcpy .eq. 1.or.ifhcpy .eq. 2) then
CC              hp laserjet xx or hp 2235a rugged writer 480.
              call HD01Q(ihdfl,mfnam)
            else if (ifhcpy .gt. 2.and.ifhcpy .lt. 10) then
              call HD02Q(ihdfl,mfnam)
            else if (ifhcpy .eq. 10) then
CC              hp7475 bed plotter.
              call HD03Q(ihdfl,mfnam)
            else if (ifhcpy .eq. 11) then
CC              dec inkjet printer.
              call HD04Q(ihdfl,mfnam)
            end if
            iferrs = 4
            ifccok = 1
            CLOSE (unit=ihdfl,status='keep',err=80)
          end if
          go to 80
   70     i = KERRS(3,mlptn,ier,idum,xdum,xdum,xdum,xdum,0)
          ifhcpy = 0
          iscfil = 0
   80     icolor = ICLRQ(1,1)
        end if
   90   continue
CC        restore segment pointer.
        ifnext = isvnx
        ifored = isavc
        iswplt(29) = 0
CCPCITS
        ibpage = 1
        idpage = 0
        ispage = idpage
        call SETMS
        imode = imodes                                                  031297
        call QSMOD(imode)                                               031297
        call RESMS(0)
CCPCITE
      end if
      return
CC
 9000 format (' END PLOT')
 9001 format (i5.5)
 9002 format (' *** OBODM, hard copy of plot number ',i6,' written to fi
     *le ',a22)
 9003 format (a3,i1)
 9004 format (a1)
      end
