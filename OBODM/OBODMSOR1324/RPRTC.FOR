      Subroutine RPRTC
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - RPRTC Version 01.3                          --- 072497
CC--- Subroutine description -  print input data and convert to     ---
CC---                           internal units for calculations.    ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
CC
      character*1 mbufr,mdash
      character*3 mcube
      character*4 mnone,mfrom,mprt                                      051503
      character*7 mcomp
      character*11 mgasp
      character*80 mbufs
CC
      dimension mbufr(80),mgasp(2),ill(25),iul(25),vll(25),vul(25),
     *          mfrom(2)                                                051503
CC
      include 'OCNTRL.INC'
      include 'ONAMES.INC'
      include 'OCDSPM.INC'
      include 'OCALCS.INC'
CC
      equivalence (mbufr(1),mbufs)
CC
      data mgasp/'    Gaseous',
     *           'Particulate'/,
     *     mcube/'**3'/,mdash/'-'/,
     *     mcomp/'Compute'/,
     *     mnone/'None'/,
     *     mfrom/'From','Vect'/                                         051509
CC
CC**********************************************************************
CC
CC
      if (iotfil .gt. 0) then
CC
CC        print pollutant/species material characteristics
        n = 9
        if (spmass .gt. 0.0.and.qtfuel .gt. 0.0) then
          n = n + 2
        else
          n = n + 1
        end if
        if (IOPUT(0,2,5,n) .eq. 0) then
          WRITE (iotfil,9000)
        end if
        if (NUMC1(mspeci,36) .le. 0) then
          call MBLK1(mbufr,36)
          call MVCH1(mnone,mbufr,4)
        else
          call MVCH1(mspeci,mbufr,36)
        end if
        n = LRJST(mbufr,36,1)
        j = MAX0(52-n,1)
        WRITE (iotfil,9001) (mdash,i=1,j-1),mblnk1,(mbufr(i),i=1,n)
        WRITE (iotfil,9002) mgasp(ifgpwn+1)
        if (decays .gt. 0.0) then
          call UNITS(mbufr,2,idcunt,0,0,1,5)
          WRITE (iotfil,9003) (mbufr(i),i=1,2),decays
        else
          WRITE (iotfil,9004)
        end if
        i = IWTBF(mbufr,12,whtmol,2,0)
        WRITE (iotfil,9008) (mbufr(i),i=1,12)
        i = IWTBF(mbufr(9),12,densty,4,0)
        call MBLK1(mbufr,8)
        call UNITS(mbufr,5,idunit,jdunit,0,1,11)
        n = NUMC1(mbufr,5)
        call MVCH1(mcube,mbufr(n+1),3)
        WRITE (iotfil,9005) (mbufr(i),i=1,8),(mbufr(i+8),i=1,12)
        call MBLK1(mbufr,28)
        if (spmass .gt. 0.0.and.qtfuel .gt. 0.0) then
          i = IWTBF(mbufr(3),12,spmass,4,0)
          call UNITS(mbufr,2,isunit,idumy,0,1,3)
          i = IWTBF(mbufr(17),12,qtfuel,4,0)
          call UNITS(mbufr(15),2,ifunit,idumy,0,1,3)
          WRITE (iotfil,9006) (mbufr(i),i=1,28)
        else
          i = IWTBF(mbufr,12,qfract,9,0)
          WRITE (iotfil,9007) (mbufr(i),i=1,12)
        end if
        if (ifgpwn .gt. 0) then
          if (ifgend .ne. 0) then
            if (drpsig .gt. 0.0) then
              i = IOPUT(0,2,5,1)
              i = IWTBF(mbufr,12,drpsig,5,0)
              WRITE (iotfil,9009) (mbufr(i),i=1,12)
            end if
            if (drpmmd .gt. 0.0) then
              i = IOPUT(0,2,5,1)
              call UNITS(mbufr,2,ipsunt,0,0,1,0)
              i = IWTBF(mbufr(3),12,drpmmd,2,0)
              WRITE (iotfil,9010) (mbufr(i),i=1,2),(mbufr(i+2),i=1,12)
            end if
          end if
          call UNITS(mbufr,2,ipsunt,0,0,1,0)
          if (IFEQU(drpupr(1),drplwr(1)) .eq. 0) then
            i = IOPUT(0,2,5,1 + (nvsprt + 4) / 8)
            WRITE (iotfil,9011) (mbufr(i),i=1,2),(drpupr(i),i=1,nvsprt)
          else
            i = IOPUT(0,2,5,2 * (1 + (nvsprt + 4) / 8))
            WRITE (iotfil,9012) (mbufr(i),i=1,2),(drpupr(i),i=1,nvsprt)
            WRITE (iotfil,9013) (mbufr(i),i=1,2),(drplwr(i),i=1,nvsprt)
          end if
          i = IOPUT(0,2,5,2 + (nvsprt + 2) / 5)
          WRITE (iotfil,9014) betale
          WRITE (iotfil,9015) (pctmat(i),i=1,nvsprt)
          tmp1 = airtmp
          tmp2 = airprs
          airtmp = constc(itpunt+1,2) * (airtmp - constc(itpunt+1,1))
          airprs = airprs * constp(iprunt+1)
CC
CC          calculate particle settling velocity
          iferrs = 0
          do 10 i=1,nvsprt
CC            convert diameters to micrometers
            drpu = drpupr(i) * constl(ipsunt+1) / constl(8)
            if (drplwr(i) .le. 0.0) then
              drpl = drpupr(i)
            else
              drpl = drplwr(i) * constl(ipsunt+1) / constl(8)
            end if
            davg = DRPAV(drpu * 5.0e-5, drpl * 5.0e-5, 0.1) * 2.0e4
            if (vspart(i) .le. 0.0) then
CC              default particle settling velocity.
              vspart(i) = TRMVS(davg * 5.0e-5,i) * 0.01 /
     *                    constl(ipvunt+1) * constt(jpvunt+1)
              if (vspart(i) .lt. 0.0) then
                ifnext = 3
                ifrdwt(5) = -1
                go to 30
              end if
            end if
            if (gammad(i) .lt. 0.0) gammad(i) = FGAMA(i)
   10     continue
          airtmp = tmp1
          airprs = tmp2
          i = IOPUT(0,2,5,1 + (nvsprt + 2) / 5)
          call UNITS(mbufr,5,ipvunt,jpvunt,0,1,7)
          WRITE (iotfil,9016) (mbufr(i),i=1,5),(vspart(i),i=1,nvsprt)
          if (gammad(1) .ge. 0.0) then
            i = IOPUT(0,2,5,1 + (nvsprt + 2) / 5)
            WRITE (iotfil,9017) (gammad(i),i=1,nvsprt)
          end if
        end if
CC
CC        print meteorological data
        j = 10
        if (iniyrs .gt. 0) j = j + 6
        if (geolon + geolat + zondif .gt. rmissd) j = j + 3
        if (istabl .gt. 0) j = j + 1
        if (IOPUT(0,2,6,j) .eq. 0) then
          WRITE (iotfil,9018)
        end if
        do 20 i=1,25
          if (i .lt. 6) then
            ill(i) = vllimt(i)
            if (i .eq. 5) then
              iul(5) = nmxdys
            else
              iul(i) = vulimt(i)
            end if
          else
            vll(i) = vllimt(i)
            vul(i) = vulimt(i)
          end if
   20   continue
        ill(18) = vll(18)
        iul(18) = vul(18)
        if (ill(18) .lt. 1.or.ill(18) .gt. 6) ill(18) = 1
        if (iul(18) .lt. 1.or.iul(18) .gt. 6) iul(18) = 6
        ill(19) = vll(19)
        iul(19) = vul(19)
        ill(24) = vll(24)
        iul(24) = vul(24)
CC>     if (iul(1) .gt. 100)  iul(1) = iul(1) - (iul(1) / 100) * 100    020797
        if (ill(4) .ge. 100) ill(4) = ill(4) / 100                      072497
        if (iul(4) .ge. 100) iul(4) = iul(4) / 100
CC        wind speed
        if (ispunt .ne. 9) then
          vll(6) = vll(6) * constt(jspunt+1)
          vul(6) = vul(6) * constt(jspunt+1)
        end if
        vul(6) = vul(6) / constl(ispunt+1)
        vll(6) = vll(6) / constl(ispunt+1)
CC        sigmap
        if (isyunt .ne. 0) then
          vll(9) = vll(9) / radtod
          vul(9) = vul(9) / radtod
        end if
CC        sigmep
        if (iszunt .ne. 0) then
          vll(10) = vll(10) / radtod
          vul(10) = vul(10) / radtod
        end if
CC        trbixr
        if (isxunt .ne. 0) then
          vll(11) = vll(11) / radtod
          vul(11) = vul(11) / radtod
        end if
CC        airtmp
        if (itpunt .ne. 0) then
          vll(12) = vll(12) / constc(itpunt+1,2) + constc(itpunt+1,1)
          vul(12) = vul(12) / constc(itpunt+1,2) + constc(itpunt+1,1)
        end if
CC        airprs
        if (iprunt .ne. 0) then
          vll(13) = vll(13) / constp(iprunt+1)
          vul(13) = vul(13) / constp(iprunt+1)
        end if
CC        dphidz
        vll(14) = vll(14) * constl(jvgunt+1) / constc(ivgunt+1,2)
        vul(14) = vul(14) * constl(jvgunt+1) / constc(ivgunt+1,2)
CC        hmdpth
        vll(15) = vll(15) / constl(ihmunt+1)
        vul(15) = vul(15) / constl(ihmunt+1)
CC        spdshr
        if (jssunt .ge. 0) then
          vll(16) = vll(16) * constt(jssunt+1)
          vul(16) = vul(16) * constt(jssunt+1)
        end if
        vul(16) = vul(16) / constl(issunt+1)
        vll(16) = vll(16) / constl(issunt+1)
CC        dirshr
        if (idsunt .ne. 0) then
          vll(17) = vll(17) / radtod
          vul(17) = vul(17) / radtod
        end if
CC        tauosa
        vll(20) = vll(20) / constt(istunt+1)
        vul(20) = vul(20) / constt(istunt+1)
CC        roughl
        if (irount .ne. 5) then
          vll(21) = vll(21) * constl(6) / constl(irount+1)
          vul(21) = vul(21) * constl(6) / constl(irount+1)
        end if
CC        cldhgt
        if (iceunt .ne. 0) then
          vll(25) = vll(25) / constl(iceunt+1)
          vul(25) = vul(25) / constl(iceunt+1)
        end if
        if (iniyrs .gt. 0) then
          if (zondif .lt. 0.0) then
            if (ifdstm .eq. 0) then
              i = 19
              j = 20
              k = 4
            else
              i = 4
              j = 19
              k = 20
            end if
          else if (IFEQU(zondif,0.0) .eq. 0) then                       071196
            i = 7
            j = 13
            k = 20
            ifdstm = 0
          else if (zondif .le. 5.5) then
            i = 5
            j = 19
            k = 20
          else if (zondif .le. 6.5) then
            i = 3
            j = 19
            k = 20
          else if (zondif .le. 7.5) then
            i = 13
            j = 19
            k = 20
          else if (zondif .le. 8.5) then
            i = 16
            j = 19
            k = 20
          end if
          if (i .eq. 3.or.i .eq. 5.or.i .eq. 13.or.i .eq. 16) then
            if (ifdstm .eq. 0) then
              j = 19
            else
              j = 4
            end if
          end if
          mbufr(1) = malpha(i)
          mbufr(2) = malpha(j)
          mbufr(3) = malpha(k)
          j = inihrs
          if (inihrs .lt. 0) then
            j = 0
            inimin = 0
          end if
          WRITE (iotfil,9019) ill(1),iul(1),iniyrs,ill(2),iul(2),inimon,
     *                        ill(3),iul(3),inidys,(mbufr(i),i=1,3),
     *                        ill(4),iul(4),j,inimin,ill(5),iul(5),
     *                        injdys
        end if
        if (geolon + geolat + zondif .gt. rmissd) then
          WRITE (iotfil,9020) geolon,geolat,zondif
        end if
        call UNITS(mbufr,4,iprunt,0,0,1,4)
        call UNITS(mbufr(5),2,iceunt,0,0,1,0)
        WRITE (iotfil,9021) (mbufr(i),i=1,4),vll(13),vul(13),airprs,
     *                      ill(24),iul(24),cldcov,(mbufr(i),i=5,6),
     *                      vll(25),vul(25),cldhgt,ill(19),iul(19),
     *                      radinx
        if (istabl .gt. 0) WRITE (iotfil,9022) malpha(ill(18)),
     *       malpha(iul(18)),malpha(istabl)
        i = IOPUT(0,2,6,3)
        call UNITS(mbufr,2,irount,0,0,1,0)
        call UNITS(mbufr(3),5,ivgunt,jvgunt,0,1,15)
        if (iswopt(25) .gt. 0.and.dphidz .lt. 3.344e-4)
     *      dphidz = 3.344e-4
        i = IWTBF(mbufr(8),12,dphidz,5,0)
        WRITE (iotfil,9024) refhgt,(mbufr(i),i=1,2),vll(21),vul(21),
     *                      roughl,(mbufr(i),i=3,7),vll(14),vul(14),
     *                      (mbufr(i),i=8,19)
        i = IOPUT(0,2,6,16)
        call UNITS(mbufr,5,ispunt,jspunt,0,1,7)
        call UNITS(mbufr(6),2,itpunt,0,0,1,6)
        call UNITS(mbufr(8),2,isyunt,0,0,1,1)
        call UNITS(mbufr(10),2,iszunt,0,0,1,1)
        call UNITS(mbufr(12),2,isxunt,0,0,1,1)
        call UNITS(mbufr(14),2,istunt,0,0,1,5)
        if (ispunt .ne. 9) then
          spd = refspd * constl(ispunt+1) / constt(jspunt + 1)
        else
          spd = refspd * constl(ispunt+1)
        end if
        spd = AMAX1(PWLAW(spd,2.0,refhgt,pwrlaw),vllimt(6))
        WRITE (iotfil,9025) (mbufr(i),i=1,5),vll(6),vul(6),refspd,spd,
     *                      (mbufr(i),i=6,7),vll(12),vul(12),airtmp,
     *                      (mbufr(i),i=8,9),vll(9),vul(9),sigmap,
     *                      (mbufr(i),i=10,11),vll(10),vul(10),sigmep,
     *                      (mbufr(i),i=12,13),vll(11),vul(11),trbixr,
     *                      (mbufr(i),i=14,15),vll(20),vul(20),tauosa,
     *                      vll(22),vul(22),airhum
        ydumy = roughl * constl(irount+1) / constl(6)
        if (roughl .gt. 0.0.and.IFEQU(ydumy,10.0) .ne. 0) then
CC          print adjusted sigma-a and sigma-e for roughness
          xdumy = PWLAW(sigmap,ydumy,10.0,0.2)
          WRITE (iotfil,9026) xdumy,(mbufr(i),i=8,9)
          xdumy = PWLAW(sigmep,ydumy,10.0,0.2)
          WRITE (iotfil,9027) xdumy,(mbufr(i),i=10,11)
          xdumy = PWLAW(trbixr,ydumy,10.0,0.2)
          WRITE (iotfil,9028) xdumy,(mbufr(i),i=12,13)
        end if
        call UNITS(mbufr,2,ihmunt,0,0,1,0)
        call UNITS(mbufr(3),5,idsunt,jdsunt,0,1,14)
        i = IWTBF(mbufr(8),8,pwrlaw,4,0)
        mprt = mfrom(1)                                                 051503
        if (iswopt(34) .gt. 0.or.ihrfmt .gt. 1) mprt = mfrom(2)         051503
        WRITE (iotfil,9029) (mbufr(i),i=1,2),vll(15),vul(15),hmdpth,
     *                mprt,vll(8),vul(8),wnddir,(mbufr(i),i=3,7),       051503 
     *                      vll(17),vul(17),dirshr,vll(7),vul(7),
     *                      (mbufr(i),i=8,15)
        call UNITS(mbufr,5,issunt,jssunt,0,1,7)
        i = IWTBF(mbufr(6),7,spdshr,4,0)
        if (IFCHR(mbufr(12),3) .ne. 0) then
          call MVCH1(mcomp,mbufr(6),7)
        end if
        WRITE (iotfil,9030) (mbufr(i),i=1,5),vll(16),vul(16),
     *                      (mbufr(i),i=6,12)
        if (iswopt(6) .eq. 2) then
          i = IWTBF(mbufr,10,hgtmet,1,0)
          call UNITS(mbufr(11),2,ihunit,0,0,1,0)
          WRITE (iotfil,9031) mbufr(11),mbufr(12),(mbufr(i),i=1,10)
        end if
      end if
CC
   30 return
CC
 9000 format (/14x,'- Pollutant/Species Material Characteristics -'/)
 9001 format (' Pollutant/species name ',52a1)
 9002 format (' Pollutant/species is ',41('-'),2x,a11)
 9003 format (' Particle material half-life (',2a1,') ',30('-'),2x,
     *f10.2)
 9004 format (' Particle material half-life ',37('-'),2x,'infinite')
 9005 format (' Density of species (',8a1,') ',33('-'),12a1)
 9006 format (' Ratio of mass of pollutant/species (',2a1,') ',22('-'),
     *1x,12a1/' per mass of fuel or explosive (',2a1,') ',27('-'),1x,   072497
     *12a1)
 9007 format (' Fraction of exhaust cloud constituting pollutant/species
     * ',5('-'),1x,12a1)
 9008 format (' Molecular weight (g/g-mol) ',36('-'),12a1)
 9009 format (' Geometric std. dev. of the particle size dist. ',
     *15('-'),12a1)
 9010 format (' Mass-median diam. of the particle size dist. (',2a1,
     *') ',13('-'),12a1)
 9011 format (' Average particle diameters (',2a1,') ',8('-'),
     *3(f8.3,',')/(4x,8(f8.3,',')))
 9012 format (' Upper limits of particle diameters (',2a1,') ',
     *8('-'),3(f8.3,',')/(4x,8(f8.3,',')))
 9013 format (' Lower limits of particle diameters (',2a1,') ',
     *8('-'),3(f8.3,',')/(4x,8(f8.3,',')))
 9014 format (' Ratio of lagrangian to eulerian time-scales ',23('-'),
     *1x,f7.5)
 9015 format (' Fraction of particle material for each category ',
     *10('-'),1x,2(f7.5,',')/(4x,9(f7.5,',')))
 9016 format (' Particle settling velocity for each category (',5a1,
     *') ',7('-'),2(f7.4,',')/(4x,9(f7.4,',')))
 9017 format (' Particle reflection coefficient for each category ',
     *7('-'),1x,2(f8.6,',')/(4x,8(f8.6,',')))
 9018 format (/26x,'- Meteorological Data -'/26x,'( [#-#] min-max limits
     * )'/)
 9019 format (' Year [',i4,'-',i4,']',54('-'),1x,i4/                    020797
     *' Month [',i2,'-',i2,']',59('-'),1x,i2/
     *' Day [',i2,'-',i2,']',61('-'),1x,i2/
     *' Hour (',3a1,') [',i2.2,'-',i2.2,']',52('-'),1x,i4.4/
     *' Minute ',65('-'),1x,i2/
     *' Julian day [',i3,'-',i3,']',51('-'),1x,i3)
 9020 format (' West longitude (degrees) ',38('-'),2x,f10.5/
     *' North latitude (degrees) ',38('-'),2x,f10.5/
     *' Time zone difference from GMT (hours) ',31('-'),2x,f4.0)
 9021 format (' Surface pressure (',4a1,') [',f6.1,'-',f6.1,']',24('-'),
     *f12.2/' Cloud cover (8ths) [',i1,'-',i1,']',39('-'),f12.2/
     *' Cloud ceiling (',2a1,') [',f7.1,'-',f7.1,']',27('-'),f12.2/
     *' Net radiation index [',i2,'-',i2,']',43('-'),1x,f4.0)
 9022 format (' Pasquill stability category [',a1,'-',a1,']',40('-'),1x,
     *a1)
 9023 format (1x,80a1)
 9024 format (' Wind speed reference height (m) ',31('-'),f12.2/
     *' Surface roughness length (',2a1,') [',f6.2,'-',f6.2,']',18('-'),
     *f12.3/' Vertical grad. of pot. temp. (',5a1,') [',f6.3,'-',f6.3,
     *']',9('-'),2x,12a1)
 9025 format (' Reference wind speed (',5a1,') [',f5.1,'-',f5.1,']',
     *26('-'),1x,f6.3/' Minimum (at 2m) wind speed (m/s) ',35('-'),1x,
     *f6.3/' Air temperature (',2a1,') [',f5.1,'-',f5.1,']',32('-'),2x,
     *f7.3/' Standard dev. of wind direction angle (',2a1,') [',f7.4,
     *'-',f7.4,']',6('-'),1x,f8.4/' Standard dev. wind elevation angle (
     *',2a1,') [',f7.4,'-',f7.4,']',9('-'),1x,f8.4/' Longitudinal turbul
     *ence intensity (',2a1,') [',f7.4,'-',f8.4,']',9('-'),1x,f8.4/
     *' Measurement time for std. dev. wind dir. angle (',2a1,') [',f4.1
     *,'-',f6.1,']-',1x,f8.2/' Air humidity (%) (',f4.1,'-',f5.1,')',
     *40('-'),1x,f5.1)
 9026 format (' Standard dev. of wind direction angle is adjusted for ro
     *ughness length to '/52x,10('-'),1x,f8.4,' (',2a1,')')
 9027 format (' Standard dev. of wind elevation angle is adjusted for ro
     *ughness length to '/52x,10('-'),1x,f8.4,' (',2a1,')')
 9028 format (' Longitudinal turbulence intensity is adjusted for roughn
     *ess length to '/52x,10('-'),1x,f8.4,' (',2a1,')')
 9029 format (' Surface mixing layer height (',2a1,') [',f4.1,'-',f7.1,
     *']',18('-'),f10.2/' Wind direction (',a4,') (deg) [',f6.1,'-',    051503
     *f6.1,']',24('-'),1x,f7.1/' Wind-direction shear (',5a1,') [',f6.1,
     *'-',f6.1,']',23('-'),1x,f7.2/' Wind-speed power law exponent [',
     *f5.3,'-',f5.3,']',23('-'),1x,8a1)
 9030 format (' Wind-speed shear (',5a1,') [',f6.2,'-',f6.2,']',27('-'),
     *1x,7a1)
 9031 format (' Elevation of meteorological data measurement site (',
     *2a1,') ',9('-'),1x,10a1)
      end
