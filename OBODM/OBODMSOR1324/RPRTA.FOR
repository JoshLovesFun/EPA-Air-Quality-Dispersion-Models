      Subroutine RPRTA
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - RPRTA Version 01.2                          ---
CC--- Subroutine description - save input data to save files and    ---
CC---                          print input data.                    ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      character*1 mslsh,mless,mbufr,mcoma,mdash,mrprn
      character*11 mrtyp
CC
      dimension mbufr(80),mrtyp(2)
CC
      include 'OCNTRL.INC'
      include 'ONAMES.INC'
      include 'OCDSPM.INC'
      include 'OCALCS.INC'
CC
      data mslsh/'/'/,mless/'<'/,mcoma/','/,mdash/'-'/,mrprn/')'/,
     *     mrtyp/'Rectangular','      Polar'/
CC
CC**********************************************************************
CC
      if (ifhrly .gt. 0.and.ifbtch .eq. 0) then
        i = KERRS(57,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0)
      end if
CC      reset number of grid points.
      ngrdpt = nxpnts * nypnts
CC      reset total number of points.
      ntotpt = ngrdpt + nxypnt
CC      reset calculation offset indices for dos, con, etc.
      j = JINIT(jjjoff,4,0,0)
      ntotal = -ntotpt
      do 10 i=1,4
        if (iswopt(i) .gt. 0) then
          ntotal = ntotal + ntotpt
          jjjoff(i) = ntotal
        end if
   10 continue
      ntotal = ntotal + ntotpt
      if (ntotal+1 .gt. nmxcor) then
        ifrdwt(5) = -1
        if (KERRS(9,mnpbuf,ntotal,nmxcor,xdum,xdum,xdum,xdum,0) .ge. 1)
     *      go to 70
        ifnext = 3
        go to 70
      end if
      if (iswopt(6) .eq. 2.and.(ifgpwn .gt. 0.or.nvsprt .gt. 0)) then
        if (KERRS(10,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .eq. 1)
     *      go to 70
        ifrdwt(3) = -1
        ifnext = 3
        go to 70
      end if
CC
CC        write save data file.
      if (ifsave .gt. 0.and.ifstco .eq. 0) then
   20   if (msvnam .ne. mblnk1) then
          iferrs = KERRS(8,msvnam,idum,idum,xdum,xdum,xdum,xdum,0)
          if (iferrs .eq. 3) then
            call STODT
            ifsave = ifsave + 1
          else if (iferrs .eq. 1.or.iferrs .eq. 2) then
            go to 70
          else if (iferrs .ne. 4) then
            call IBELL(4)
            go to 20
          end if
        end if
      else
        call STODT
        if (iferrs .eq. 1.or.iferrs .eq. 2) go to 70
        ifsave = ifsave + 1
      end if
      if (ifbtch .eq. 0) ifrsav = 1
CC      initialize graphics/solution output file.
      if (ifhrly .gt. 0) then
        nmxdys = 366
        nmxhrs = 8784
      else
        nmxdys = 1
        nmxhrs = 1
      end if
      call SAVID
      if (iferrs .eq. 1.or.iferrs .eq. 2) go to 70
      sdumy = refspd * constl(ispunt+1)
      if (ispunt .ne. 9) sdumy = sdumy / constt(jspunt+1)
      if (pwrlaw .lt. vllimt(7)) then
        pwrlaw = DFMET(1,radinx,istabl,sdumy,0.0)
      end if
      xdumy = sigmap
      if (isyunt .ne. 0) xdumy = xdumy * radtod
      iflg = 0
      if (xdumy .lt. vllimt(9)) then
        sigmap = DFMET(3,radinx,istabl,sdumy,0.0)
        sigmap = PWLAW(sigmap,tauosa,tautmo,0.2)
        sigmap = sigmap * constg(isyunt+1)
        iflg = 1
      end if
      xdumy = sigmep
      if (iszunt .ne. 0) xdumy = xdumy * radtod
      if (xdumy .lt. vllimt(10)) then
        sigmep = DFMET(2,radinx,istabl,sdumy,0.0)
        sigmep = sigmep * constg(iszunt+1)
      end if
      xdumy = trbixr
      if (isxunt .ne. 0) xdumy = xdumy * radtod
      if (xdumy .lt. vllimt(11)) then
        if (iflg .eq. 0) then
          trbixr = 1.33 * sigmap
          if (isxunt .ne. isyunt) then
            if (isyunt .eq. 0) then
              trbixr = trbixr / constg(2)
            else
              trbixr = trbixr * constg(2)
            end if
          end if
        else
          trbixr = DFMET(6,radinx,istabl,sdumy,0.0)
          trbixr = PWLAW(trbixr,tauosa,tautmo,0.2)
          trbixr = trbixr * constg(isxunt+1)
        end if
      end if
      xdumy = hmdpth * constl(ihmunt+1)
      if (xdumy .lt. vllimt(15)) then
        hmdpth = DFMET(4,radinx,istabl,sdumy,0.0)
      end if
      xdumy = constc(ivgunt+1,2) * dphidz / constl(jvgunt+1)
      if (xdumy .lt. vllimt(14)) then
        dphidz = DFMET(5,radinx,istabl,sdumy,airhum) * constl(jvgunt+1)
     *                 / constc(ivgunt+1,2)
      end if
CC      set source function time.
      do 30 i=1,nsourc
        if (taucld(i) .lt. 2.5) taucld(i) = 2.5
   30 continue
      zpsave = zcoord(1)
CC
CC      print input data.
CC
      if (iotfil .gt. 0) then
CC        print models selected.
        n = ISUMI(iswopt,3)
        i = 0
        if (n .gt. 0) then
          if (nvsprt .gt. 0) i = i + n
        end if
        n = n + i + 2
        if (iswopt(4) .gt. 0) then
          n = n + 1
        end if
        if (iswopt(8) .gt. 0) n = n + 1
        if (clrinx .gt. 0.0) n = n + 1
        if (ISUMI(navhrs,6) .gt. 0) n = n + 1
        if (iswopt(27) .gt. 0) n = n + 1
        if (ifhrly .gt. 0) n = n + 2
        if (iswopt(27) .gt. 0) n = n + 1
        n = n + 3
        i = IOPUT(1,2,1,n)
        if (iswopt(1) .gt. 0) then
          WRITE (iotfil,9000)
          if (nvsprt .gt. 0) WRITE (iotfil,9001)
        end if
        if (iswopt(2) .gt. 0) then
          WRITE (iotfil,9002)
          if (nvsprt .gt. 0) WRITE (iotfil,9001)
        end if
        if (iswopt(3) .gt. 0) then
          WRITE (iotfil,9003)
          if (nvsprt .gt. 0) WRITE (iotfil,9001)
        end if
        if (iswopt(4) .gt. 0) then
          WRITE (iotfil,9004)
        end if
        if (iswopt(8) .eq. 1) then
          WRITE (iotfil,9005)
        else if (iswopt(8) .eq. 2) then
          WRITE (iotfil,9006)
        end if
        if (iswopt(6) .eq. 2) then
          WRITE (iotfil,9007)
        else if (iswopt(6) .eq. 1.or.iswopt(6) .eq. 3) then             120702
          WRITE (iotfil,9008)
        else
          WRITE (iotfil,9009)
        end if
        if (iswopt(23) .le. 0) then
          WRITE (iotfil,9010)
        else
          WRITE (iotfil,9011)
        end if
        if (iswopt(25) .le. 0) then
          WRITE (iotfil,9012)
        else
          WRITE (iotfil,9013)
        end if
        if (clrinx .gt. 0.0) then
          WRITE (iotfil,9014) clrinx
        end if
        if (iswopt(27) .gt. 0) then
          WRITE (iotfil,9015)
        end if
        if (ifhrly .gt. 0) then
          if (iswopt(10) .eq. 0) then
            WRITE (iotfil,9016)
          else
            WRITE (iotfil,9017)
          end if
          if (iswopt(11) .eq. 0) then
            WRITE (iotfil,9018)
          else
            WRITE (iotfil,9019)
          end if
        end if
        if (ISUMI(navhrs,6) .gt. 0) then
          call MBLK1(mbufr,38)
          k = 0
          do 40 j=1,6
            if (navhrs(j) .gt. 0) then
              xdumy = navhrs(j)
              n = IWTBF(mbufr,5,xdumy,0,1)
              k = k + 1
              call MVCH1(mbufr,mbufr(k+5),n)
              k = k + n
              mbufr(k+5) = mcoma
            end if
   40     continue
          if (k .gt. 0) then
            mbufr(k+5) = mblnk1
            i = LRJST(mbufr(6),33,0)
            n = IPKBK(mbufr(6),33)
            j = MAX0(33-n,1)
            WRITE (iotfil,9020) (mdash,i=1,j-1),mblnk1,(mbufr(i+5),
     *                          i=1,n)
          end if
        end if
        write (iotfil,9021) mrtyp(igrect+1)
CC
CC        print options selected.
        n = 3
        if (nvsprt .gt. 0) n = n + 1
        if (nsourc .gt. 1.or.ngroup .gt. 0) n = n + 1
        if (iswopt(1) .gt. 0) n = n + 1
        if (ISUMI(iswopt(2),2) .gt. 0) n = n + 1
        if (iswopt(4) .gt. 0) n = n + 1
        if (iswopt(12) .gt. 0) n = n + 1
        if (iswopt(18) .gt. 0) n = n + 1
        if (ifanul .gt. 0) n = n + 1
        if (ifsean .gt. 0) n = n + 1
        if (IOPUT(0,2,2,n+1) .eq. 0) then
          WRITE (iotfil,9022)
        end if
        if (ifmode .eq. 0) then
          WRITE (iotfil,9023)
        else
          WRITE (iotfil,9024)
        end if
        if (iswopt(12) .gt. 0) WRITE (iotfil,9025)
        if (iswopt(18) .gt. 0) WRITE (iotfil,9026)
        if (ifanul .gt. 0) WRITE (iotfil,9027)
        if (ifsean .eq. 1) WRITE (iotfil,9028)
        if (ifsean .eq. 2) WRITE (iotfil,9029)
        if (nsourc .gt. 1) then
          if (ngroup .eq. 0) then
            WRITE (iotfil,9030)
          else
            WRITE (iotfil,9031)
          end if
        end if
        if (iswopt(1) .gt. 0) then
          if (iswopt(17) .le. 9) then
            n = 1
            call MVCH1(munitm(iswopt(13)+1),mbufr(n),10)
            n = n + 10
            mbufr(n) = mless
            mbufr(n+1) = mblnk1
            call MVCH1(munitt(iswopt(14)+1),mbufr(n+2),7)
            n = n + 9
            mbufr(n) = mless
            mbufr(n+1) = mslsh
            if (iswopt(17) .ne. 9) then
              call MVCH1(munitv(1),mbufr(n+2),18)
              n = n + 20
              mbufr(n) = mless
              mbufr(n+1) = mblnk1
            end if
            if (iswopt(17) .eq. 9) then
              call MVCH1(munitv(2),mbufr(n+2),5)
              n = n + 6
            else
              call MVCH1(munitl(iswopt(17)+1),mbufr(n+2),11)
              n = n + 12
              i = IFCHS(mbufr,n-11,n,19,0,1)
              if (iswopt(17) .eq. 2) i = IFCHS(mbufr,n-11,n,5,15,2)
            end if
          else
            n = 1
            call MVCH1(munitv(iswopt(17)-7),mbufr(n),18)
            n = n + 18
            mbufr(n) = mblnk1
            call MVCH1(munitt(iswopt(14)+1),mbufr(n+1),7)
            n = n + 7
          end if
          n = JPACK(mbufr,n)
          j = MAX0(45-n,1)
          WRITE (iotfil,9032) (mdash,i=1,j+1),mblnk1,(mbufr(i),i=1,n)
        end if
        if (ISUMI(iswopt(2),2) .gt. 0) then
          if (iswopt(17) .le. 9) then
            n = 1
            call MVCH1(munitm(iswopt(13)+1),mbufr(n),10)
            n = n + 10
            mbufr(n) = mless
            mbufr(n+1) = mslsh
            if (iswopt(17) .ne. 9) then
              call MVCH1(munitv(1),mbufr(n+2),18)
              n = n + 20
              mbufr(n) = mless
              mbufr(n+1) = mblnk1
            end if
            if (iswopt(17) .eq. 9) then
              call MVCH1(munitv(2),mbufr(n+2),5)
              n = n + 6
            else
              call MVCH1(munitl(iswopt(17)+1),mbufr(n+2),11)
              n = n + 12
              i = IFCHS(mbufr,n-11,n,19,0,1)
              if (iswopt(17) .eq. 2) i = IFCHS(mbufr,n-11,n,5,15,2)
            end if
          else
            n = 1
            call MVCH1(munitv(iswopt(17)-7),mbufr(n),18)
            n = n + 17
          end if
          n = JPACK(mbufr,n)
          j = MAX0(38 - n,1)
          WRITE (iotfil,9033) (mdash,i=1,j+1),mblnk1,(mbufr(i),i=1,n)
        end if
        if (iswopt(4) .gt. 0) then
          n = 1
          call MVCH1(munitm(iswopt(13)+1),mbufr(n),10)
          n = n + 10
          mbufr(n) = mless
          mbufr(n+1) = mslsh
          if (iswopt(16) .le. 8) then
            call MVCH1(munita(1),mbufr(n+2),7)
            n = n + 9
            call MVCH1(munitl(iswopt(16)+1),mbufr(n),11)
            if (iswopt(16) .eq. 2) i = IFCHS(mbufr,n,n+10,5,15,2)
            n = n + 10
          else
            j = iswopt(16) - 8
            call MVCH1(munita(j+1),mbufr(n+2),7)
            n = n + 8
          end if
          n = JPACK(mbufr,n)
          j = MAX0(41-n,1)
          WRITE (iotfil,9034) (mdash,i=1,j+1),mblnk1,(mbufr(i),i=1,n)
        end if
CC
CC        print receptor geometry.
        if (IOPUT(0,2,3,10) .eq. 0) then
          WRITE (iotfil,9035)
        end if
        WRITE (iotfil,9036) munitg(igangu+1),grdang
        call MVCH1(munitl(igunit+1),mbufr,11)
        call MBLK1(mbufr(12),11)
        if (igrect .le. 0) then
          call MVCH1(munitl(igunit+1),mbufr(12),11)
          k = 11
        else
          call MVCH1(munitg(1),mbufr(12),7)
          k = 7
        end if
        call MVCH1(munitl(izunit+1),mbufr(23),11)
        n1 = IPKBK(mbufr,11)
        j = MAX0(36-n1,1)
        WRITE (iotfil,9037) (mdash,i=1,j-1),mblnk1,(mbufr(i),i=1,n1)
        n2 = IPKBK(mbufr(12),k)
        j = MAX0(36-n2,1)
        WRITE (iotfil,9038) malpha(25),(mdash,i=1,j-1),mblnk1,
     *                      (mbufr(11+i),i=1,n2)
        n = IPKBK(mbufr(23),11)
        j = MAX0(36-n,1)
        WRITE (iotfil,9038) malpha(26),(mdash,i=1,j-1),mblnk1,
     *                      (mbufr(22+i),i=1,n)
        WRITE (iotfil,9039) xorgin,yorgin
        call UNITS(mbufr(40),2,igunit,0,0,1,0)
        call UNITS(mbufr(42),3,igunit,igrect,0,1,0)
        call UNITS(mbufr(45),2,izunit,0,0,1,0)
        if (nxpnts .gt. 0.and.nypnts .gt. 0) then
          i = IOPUT(0,2,3,1+(nxpnts+8)/11)
          j = 12 - n1
          WRITE (iotfil,9040) malpha(24),(mbufr(i),i=1,n1),mrprn,
     *                        (mdash,i=1,j),(xcoord(i),i=1,nxpnts)
          i = IOPUT(0,2,3,1+(nypnts+8)/11)
          j = 12 - n2
          if (ifdbug .eq. 26) then                                      020797
            k = 26                                                      020797
          else                                                          020797
            k = 25                                                      020797
          end if                                                        020797
          WRITE (iotfil,9040) malpha(k),(mbufr(11+i),i=1,n2),mrprn,     020797
     *                        (mdash,i=1,j),(ycoord(i),i=1,nypnts)
          if (iswopt(6) .eq. 2.or.iswopt(6) .eq. 3) then                120702
CC            print complex terrain.
            if (nxpnts .gt. 0.and.nypnts .gt. 0) then
              iex = 0
CC              loop over sets of x's.
   50         isx = iex + 1
              if (isx .le. nxpnts) then
                iex = isx + 3
                if (iex .gt. nxpnts) iex = nxpnts
                jsy = nypnts + 1
                nad = 7
                lflg = 1
CC                loop over y points in reverse order.
   60           jsy = jsy - 1
                if (jsy .le. 0) go to 50
                jm = IOPUT(0,2,3,nad)
                if (jm .ne. 0.or.lflg .ne. 0) then
                  WRITE (iotfil,9041) (mbufr(39+i),i=1,2),(xcoord(i),
     *                                i=isx,iex)
                  if (iswopt(6) .eq. 2) then                            120702
                    WRITE (iotfil,9042) (mbufr(41+i),i=1,3),
     *                                  (mbufr(44+i),i=1,2)
                  else                                                  120702
                    WRITE (iotfil,9047) (mbufr(41+i),i=1,3),            120702
     *                                  (mbufr(44+i),i=1,2)             120702
                  end if                                                120702
                  if (lflg .eq. 0) nlines = nlines + 6
                  lflg = 0
                  nad = 1
                end if
                l = (jsy - 1) * nxpnts
                WRITE (iotfil,9043) ycoord(jsy),(zcoord(i+l),i=isx,iex)
                go to 60
              end if
            end if
          else
            if (zcoord(1) .gt. 0.0) then
              WRITE (iotfil,9044) (mbufr(44+i),i=1,2),zcoord(1)
            end if
          end if
        end if
        if (nxypnt .gt. 0) then
          nad = 3                                                       111098
          lflg = 1                                                      111098
          do i=1,nxypnt,2                                               111098
            j = IOPUT(0,2,3,nad)                                        111098
            if (j .gt. 0.or.lflg .gt. 0) then                           111098
              WRITE (iotfil,9045) (mbufr(j),j=1,11),(mbufr(11+j),       111098
     *                            j=1,11),(mbufr(22+j),j=1,11)          111098
              if (nad .eq. 1) llines = llines + 2                       111098
            end if                                                      111098
            nad = 1                                                     111098
            lflg = 0                                                    111098
            k = 0                                                       111098
            if (i .lt. nxypnt) k = 1                                    111098
            WRITE (iotfil,9046) (xdscrt(j),ydscrt(j),zdscrt(j),j=i,i+k) 111098
          end do                                                        111098
        end if
      end if
   70 return
CC
 9000 format (' - Dosage')
 9001 format ('   With depletion due to gravitational deposition')
 9002 format (' - Concentration')
 9003 format (' - Time-average concentration')
 9004 format (' - Gravitational deposition')
 9005 format (' - Using final cloud rise ht. for all calc. distances.')
 9006 format (' - Cloud rise height is dependent on calc. distance.')
 9007 format (' - Complex terrain is being used.')
 9008 format (' - Flat terrain is assumed with receptor heights > zero t
     *reated as flag poles.')
 9009 format (' - Flat terrain is assumed.')
 9010 format (' - Grid system origin is not added to source rectangular
     *coordinates')
 9011 format (' - Grid system origin is added to source rectangular coor
     *dinates')
 9012 format (' - Instantaneous sources can use both stable and adiabati
     *c plume rise.')
 9013 format (' - Instantaneous sources are restricted to stable plume r
     *ise.')
 9014 format (' - Calculations restricted to a clearing index greater th
     *an ',8('-'),1x,f7.1)
 9015 format (' - Calculations restricted to daytime hours (sunrise+1hr
     *to sunset-1hr)')
 9016 format (' - Hours with calm wind speeds result in zero dispersion/
     *deposition.')
 9017 format (' - Hours with calm wind speeds use 1.0 m/s and previous h
     *ours wind direction')
 9018 format (' - Missing hours result in zero dispersion/deposition.')
 9019 format (' - Missing hours use previous hours data or defaults.')
 9020 format (' - Calculation averaging times (hrs) ------',33a1)
 9021 format (' - Grid receptor system is ',38('-'),a11)
 9022 format (/26x,'- Print Output Options -'/)
 9023 format (' - Print and save data using Detailed processing mode')
 9024 format (' - Print and save data using Summary processing mode')
 9025 format (' - Print highest and second highest at each receptor')
 9026 format (' - Print table of maximum 50 receptors')
 9027 format (' - Print annual table')
 9028 format (' - Print seasonal tables')
 9029 format (' - Print quarterly tables')
 9030 format (' - Print sum of sources only')
 9031 format (' - Print source groups')
 9032 format (' - Print dosage output units ',47a1)
 9033 format (' - Print concentration output units ',47a1)
 9034 format (' - Print deposition output units ',47a1)
 9035 format (/21x,'- Receptor Grid System Geometry -'/)
 9036 format (' Grid system orientation angle (',a7,') ',28('-'),f7.1)
 9037 format (' Receptor X coordinate units ',11('-'),36a1)
 9038 format (10x,a1,' coordinate units ',11('-'),36a1)
 9039 format (' X origin ',55('-'),1x,f10.2/' Y origin ',55('-'),1x,
     *f10.2)
 9040 format (1x,a1,' Axis of the grid system (',13a1,12('-'),2(f11.2,
     *',')/,(5x,6(f11.2,',')))
 9041 format (/33x,'- X axis (',2a1,') -'/14x,4f13.2)
 9042 format (' Y axis (',3a1,')',12x,'- Complex Terrain Heights (',
     *2a1,') -'/14x,53('-')/)
 9043 format (1x,f13.2,4f13.1)
 9044 format (' Z height of the grid system (',2a1,')',33('-'),f10.2)
 9045 format (/' Discrete receptor X,Y,Z ',15('-'),'(',2(11a1,','),
     *11a1,')')                                                         111098
 9046 format (7x,2('(',2(f11.2,','),f9.2,')'))                          111098
 9047 format (' Y axis (',3a1,')',12x,'- Flag Pole Heights       (',    120702
     *2a1,') -'/14x,53('-')/)                                           120702

      end
