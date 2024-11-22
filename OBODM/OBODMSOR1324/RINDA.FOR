      Subroutine RINDA
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - RINDA Version 01.3                          --- 072497
CC--- Subroutine description - get initializing data from saved     ---
CC---                          data files.                          ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      include 'OCNTRL.INC'
      include 'ONAMES.INC'
      include 'OCALCS.INC'
      include 'OWNTRL.INC'
CC
      equivalence (ifrsd,functx(2)),(isav1,functx(3)),(isav2,functx(4))
CC
CC**********************************************************************
CC
      if (ifrsd .ne. 0) then
        if (ifrsav .eq. 1) then
          isav1 = iogfil
          isav2 = igtfil
          iogfil = 0
          igtfil = isvfil
        end if
        call GETD1
        call GETD2(mscrnm,ilinem,itabsm,ispacm,iferrm)
        call GETD3(mscrnm,ilinem,itabsm,ispacm,iferrm)
        call GETD4(mscrnm,ilinem,itabsm,ispacm,iferrm)
      end if
      return
      end
      Subroutine GETD1
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - GETD1 Version 01.3                          --- 072497
CC--- Subroutine description - reads OBODM save data files.         ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
CC
      character*1 mperd,mdash,mdnam,menam,msnam,mxynm,mxfrm,myfrm,mzfrm
      character*6 mbufs
      character*8 mschf
      character*80 mbufr
CC
      dimension mdnam(22),menam(22),msnam(22),vll(25),vul(25),mxynm(22),
     *          mxfrm(60),myfrm(60),mzfrm(60),sigxo(10),sigyo(10),      111098
     *          sigzo(10)                                               111098
CC
      include 'OCNTRL.INC'
      include 'ONAMES.INC'
      include 'OCDSPM.INC'
      include 'OCALCS.INC'
CC
      equivalence (mbufr,mbufs,mnpbuf(1)),(mdnam(1),mnhnam),
     *            (menam(1),mshnam),(msnam(1),msgnam),(mxynam,mxynm(1)),
     *            (mxform,mxfrm(1)),(myform,myfrm(1)),(mzform,mzfrm(1))
CC
      data mperd/'.'/,mschf/'.SCRATCH'/,mdash/'-'/
CC
CC**********************************************************************
CC
CC        read initial data from save files.
CC
      if (iogfil .gt. 0) then
        ifrdwt(5) = 0
        if (igtfil .le. 0) then
          if (mognam .ne. mblnk1.and.mognam(1:1) .ne. mperd.and.
     *        mognam .ne. mschf) then
CC            read solution/graphics file to see if it contains data.
            call GETID(ier)
            if (ier .eq. 0) then
              ifrdwt(5) = 1
            end if
          end if
        end if
      end if
CC      if input saved data.
      ivrsd = 10
      if (igtfil .gt. 0) then
        i = JINIT(ifrdwt,4,0,0)
        isect = 0
   10   REWIND (unit=igtfil,iostat=ier,err=80)
        irec = 0
   20   irec = irec + 1
        iflg = 0
        READ (igtfil,9000,iostat=ier,err=80,end=90) mbufr
        if (isect .eq. 0.and.irec .le. 1) then
          isect = 1
          READ (mbufs,9001,iostat=ier,err=80) ivrsd
          if (ivrsd .ge. 100) ivrsd = 10                                111098
          go to 20
        else if (isect .eq. 1) then
          if (IFCHR(mbufr,1) .ne. 0.and.mbufr(3:3) .eq. mdash) then
CC            receptor data.
            isect = 2
            go to 30
          else
            go to 20
          end if
        else if (isect .eq. 2) then
          if (IFCHR(mbufr,2) .ne. 0.and.mbufr(3:3) .eq. mdash) then
CC            source data.
            isect = 3
            go to 40
          else
            go to 20
          end if
        else if (isect .eq. 3) then
          if (IFCHR(mbufr,3) .ne. 0.and.mbufr(3:3) .eq. mdash) then
CC            met. data.
            isect = 4
            go to 60
          else
            go to 20
          end if
        else if (isect .eq. 4) then
          if (IFCHR(mbufr,4) .ne. 0.and.mbufr(3:3) .eq. mdash) then
CC            control data.
            isect = 5
            go to 70
          else
            go to 20
          end if
        end if
        go to 110
CC
CC          read receptor data.
   30   irec = irec + 1
        iflg = 2
        if (ivrsd .le. 10) then                                         111098
CC          version 1.0
          READ (igtfil,9001,iostat=ier,err=70,end=80) igrect,igunit,
     *        igangu,nxpnts,nypnts,nxypnt,ifgeng,ngrdpt,ntotpt,ntotal,
     *        jjjoff,idum
        else
CC          version 1.1 and greater                                     111098
          READ (igtfil,9001,iostat=ier,err=80,end=90) igrect,igunit,
     *        igangu,nxpnts,nypnts,nxypnt,ifgeng,ngrdpt,ntotpt,ntotal,
     *        jjjoff,izunit
        end if
        irec = irec + 2
        if (ivrsd .le. 10) then
CC          Version 1.0
          READ (igtfil,9002,iostat=ier,err=70,end=80) xcoord,ycoord,
     *        zcoord(1),xdscrt,ydscrt,zdscrt,grdang,xdum,xdum,xorgin,   111098
     *        yorgin,gxstrt,gxincr,gxendp,gystrt,gyincr,gyendp
          if (nxpnts+nxypnt .gt. 0) ifrdwt(2) = 2
        else
CC          Version 1.1 and greater                                     111098
          READ (igtfil,9002,iostat=ier,err=80,end=90) xcoord,ycoord,
     *        zcoord,xdscrt,ydscrt,zdscrt,grdang,xorgin,yorgin,gxstrt,
     *        gxincr,gxendp,gystrt,gyincr,gyendp
          if (nxpnts+nxypnt .gt. 0) ifrdwt(2) = 1                       072497
        end if
        irec = irec + (nmxxyz * 5 + nmxxyz * nmxxyz + 9 + 4) / 5
CC        Version 1.2 and greater                                       111098
        if (ivrsd .ge. 12) then
          READ (igtfil,9003,iostat=ier,err=10) mxynm,mxfrm,myfrm,mzfrm
          irec = irec + 3
          if (nxpnts+nxypnt .gt. 0) ifrdwt(2) = 1
        end if
        go to 10
CC
CC        read input source data.
   40   irec = irec + 1
        iflg = 3
        if (ivrsd .le. 12) then                                         072497
CC          Up to version 1.2                                           072497
          READ (igtfil,9003,iostat=ier,err=80,end=90) mpolnt,mspeci,
     *         ((msoids(i,j),i=1,36),j=1,10),mforms,menam               072497
          irec = irec + (605 + 79) / 80
        else
CC          Version 1.3 and greater                                     111098
          READ (igtfil,9003,iostat=ier,err=80,end=90) mpolnt,mspeci,    072497
     *         msoids,mforms,menam                                      072497
          irec = irec + (2045 + 79) / 80                                072497
        end if                                                          072497
        if (ivrsd .le. 10) then
CC          version 1.0
          READ (igtfil,9001,iostat=ier,err=70,end=80) nvsprt,ifgpwn,
     *        ifgend,nsourc,idum,isunit,ifunit,ipsunt,ipvunt,jpvunt,
     *        idunit,jdunit,idcunt,(isrect(i),i=1,10),                  111098
     *        (idrect(i),i=1,10),(iqunit(i),i=1,10),                    111098
     *        (jqunit(i),i=1,10),(kqunit(i),i=1,10),                    111098
     *        (idum,i=1,10),(ismunt(i),i=1,10),(isotyp(i),i=1,10),      111098
     *        ifhsly,(ivarso(i),i=1,4),(idum,i=1,15),ihsfmt             111098
          irec = irec + (114 + 12) / 13                                 072497
        else if (ivrsd .le. 12) then                                    072497
CC          version 1.1 and 1.2                                         072497
          READ (igtfil,9001,iostat=ier,err=80,end=90) nvsprt,ifgpwn,
     *        ifgend,nsourc,isunit,ifunit,ipsunt,ipvunt,jpvunt,idunit,
     *        jdunit,idcunt,(isrect(i),i=1,10),(idrect(i),i=1,10),      072497
     *        (iqunit(i),i=1,10),(jqunit(i),i=1,10),(kqunit(i),i=1,10), 072497
     *        (ismunt(i),i=1,10),(isotyp(i),i=1,10),ifhsly,ivarso,      072497
     *        ihsfmt,((isofrq(i,j),i=1,24),j=1,10)                      072497
          irec = irec + (328 + 12) / 13
        else                                                            072497
CC          Version 1.3 and greater                                     111098
          READ (igtfil,9001,iostat=ier,err=80,end=90) nvsprt,ifgpwn,    072497
     *        ifgend,nsourc,isunit,ifunit,ipsunt,ipvunt,jpvunt,idunit,  072497
     *        jdunit,idcunt,isrect,idrect,iqunit,jqunit,kqunit,ismunt,  072497
     *        isotyp,ifhsly,ivarso,ihsfmt,isofrq                        072497
          irec = irec + (1568 + 12) / 13                                072497
        end if
        if (ivrsd .le. 10) then
CC          version 1.0
          READ (igtfil,9002,iostat=ier,err=70,end=80) dcutof,betale,
     *        decays,densty,whtmol,drpmmd,drpsig,drpupr,drplwr,pctmat,
     *        vspart,gammad,spmass,qtfuel,qfract,(qemiss(i),i=1,10),    111098
     *        (taucld(i),i=1,10),(relhgt(i),i=1,10),(xdum,i=1,10),      111098
     *        (clddxs(i),i=1,10),(clddys(i),i=1,10),(sigxo(i),i=1,10),  111098
     *        (sigyo(i),i=1,10),(sigzo(i),i=1,10),(xsmlry(i),i=1,10),   111098
     *        (xsmlrz(i),i=1,10),(xlrgry(i),i=1,10),(xlrgrz(i),i=1,10), 111098
     *        (hetcnt(i),i=1,10),(brnrat(i),i=1,10),(xdum,i=1,10),      111098
     *        (clddxe(i),i=1,10),(clddye(i),i=1,10),(relhte(i),i=1,10), 111098
     *        (gammat(i),i=1,10),(alphas(i),i=1,10),(betass(i),i=1,10)  111098
          do 50 i=1,nsourc
            xdum = SQRT(sigxo(i)**2 + sigyo(i)**2)                      111098
            cldlng(i) = 4.3 * xdum
            cldsht(i) = cldlng(i)
            cldang(i) = 0.0
            if (relhgt(i) .gt. 0.0) then
              clddpt(i) = 4.3 * sigzo(i)                                111098
            else
              clddpt(i) = 2.15 * sigzo(i)                               111098
            end if
   50     continue
          if (nsourc .gt. 0) ifrdwt(3) = 2
          irec = irec + (480 + 4) / 5                                   111098
        else if (ivrsd .le. 12) then                                    072497
CC          version 1.1 and 1.2                                         072497
          READ (igtfil,9002,iostat=ier,err=80,end=90) dcutof,betale,    072497
     *        decays,densty,whtmol,drpmmd,drpsig,drpupr,drplwr,pctmat,  072497
     *        vspart,gammad,spmass,qtfuel,qfract,(qemiss(i),i=1,10),    072497
     *        (taucld(i),i=1,10),(relhgt(i),i=1,10),(clddxs(i),i=1,10), 072497
     *        (clddys(i),i=1,10),(cldlng(i),i=1,10),(cldsht(i),i=1,10), 072497
     *        (cldang(i),i=1,10),(clddpt(i),i=1,10),(xsmlry(i),i=1,10), 072497
     *        (xsmlrz(i),i=1,10),(xlrgry(i),i=1,10),(xlrgrz(i),i=1,10), 072497
     *        (hetcnt(i),i=1,10),(brnrat(i),i=1,10),(clddxe(i),i=1,10), 072497
     *        (clddye(i),i=1,10),(relhte(i),i=1,10),(gammat(i),i=1,10), 072497
     *        (alphas(i),i=1,10),(betass(i),i=1,10),(clddzs(i),i=1,10), 072497
     *        (clddze(i),i=1,10)                                        072497
          if (nsourc .gt. 0) ifrdwt(3) = 1
          irec = irec + (490 + 4) / 5                                   072497
        else                                                            072497
CC          Version 1.3 and greater                                     111098
          READ (igtfil,9002,iostat=ier,err=80,end=90) dcutof,betale,    072497
     *        decays,densty,whtmol,drpmmd,drpsig,drpupr,drplwr,pctmat,  072497
     *        vspart,gammad,spmass,qtfuel,qfract,qemiss,taucld,relhgt,  072497
     *        clddxs,clddys,cldlng,cldsht,cldang,clddpt,xsmlry,xsmlrz,  072497
     *        xlrgry,xlrgrz,hetcnt,brnrat,clddxe,clddye,relhte,gammat,  072497
     *        alphas,betass,clddzs,clddze                               072497
          if (nsourc .gt. 0) ifrdwt(3) = 1                              072497
          irec = irec + (1410 + 4) / 5                                  072497
        end if
        go to 10
CC
CC        read input meteorological data.
   60   irec = irec + 1
        iflg = 4
        READ (igtfil,9003,iostat=ier,err=80,end=90) mformt,mdnam
        irec = irec + (173 + 79) / 80
        if (ivrsd .le. 10) then                                         111098
CC          version 1.0
          READ (igtfil,9001,iostat=ier,err=70,end=80) ifhrly,iceunt,
     *        ispunt,jspunt,issunt,jssunt,idsunt,jdsunt,isxunt,isyunt,
     *        iszunt,istunt,ihmunt,ivgunt,jvgunt,irount,itpunt,iprunt,
     *        idum,idum,idum,ihrfmt,ischmd,(ivarmt(i),i=1,25),istabl,   111098
     *        iniyrs,inimon,inidys,inihrs,inimin,injdys,ifdstm,iscyrs,  111098
     *        iscdys,ihunit                                             111098
        else
CC          version 1.1 and greater                                     111098
          READ (igtfil,9001,iostat=ier,err=80,end=90) ifhrly,iceunt,
     *        ispunt,jspunt,issunt,jssunt,idsunt,jdsunt,isxunt,isyunt,
     *        iszunt,istunt,ihmunt,ivgunt,jvgunt,irount,itpunt,iprunt,
     *        ihrfmt,ischmd,ivarmt,istabl,iniyrs,inimon,inidys,inihrs,
     *        inimin,injdys,ifdstm,iscyrs,iscdys,ihunit
        end if
        irec = irec + (421 + 12) / 13
        if (ivrsd .le. 10) then                                         111098
CC          version 1.0
          READ (igtfil,9002,iostat=ier,err=70,end=80) refspd,pwrlaw,
     *        wnddir,sigmap,sigmep,trbixr,airtmp,airprs,dphidz,hmdpth,
     *        spdshr,dirshr,radinx,tauosa,roughl,cldcov,cldhgt,refhgt,
     *        xdum,xdum,alphad,betadf,zondif,geolon,geolat,airhum,
     *        stblty,clearx,(xdum,i=1,50),hgtmet                        111098
          if (refspd .gt. 0.0) ifrdwt(4) = 2
        else
CC          version 1.1 and greater                                     111098
          READ (igtfil,9002,iostat=ier,err=80,end=90) refspd,pwrlaw,
     *        wnddir,sigmap,sigmep,trbixr,airtmp,airprs,dphidz,hmdpth,
     *        spdshr,dirshr,radinx,tauosa,roughl,cldcov,cldhgt,refhgt,
     *        alphad,betadf,zondif,geolon,geolat,airhum,stblty,clearx,
     *        vll,vul,hgtmet
          i = jrnit(vllimt,25,vll,1)                                    111098
          i = jrnit(vulimt,25,vul,1)                                    111098
          if (refspd .gt. 0.0) ifrdwt(4) = 1
        end if
CC        correct previous versions, RINPL had wrong value for year.    072497
        if (vllimt(1) .lt. 1000.0) vllimt(1) = 1900.0                   072497
        go to 10
CC
CC        Read input control/option data.
   70   irec = irec + 1
        iflg = 1
        if (ivrsd .le. 10) then                                         111098
CC          Version 1.0
          READ (igtfil,9003,iostat=ier,err=70,end=80) mtitle
          if (ISUMI(iswopt,4) .gt. 0) ifrdwt(1) = 2
        else
CC          Version 1.1 and greater                                     111098
          READ (igtfil,9003,iostat=ier,err=80,end=90) mtitle,msnam
          if (ISUMI(iswopt,4) .gt. 0) ifrdwt(1) = 1
        end if
        irec = irec + 1
        if (ivrsd .le. 10) then                                         111098
CC          Version 1.0, 1.1                                            111098
          READ (igtfil,9001,iostat=ier,err=80,end=90) iswopt,itmunt,    111098
     *        ngroup,(isogrp(i),i=1,20),(nsogrp(i),i=1,10),ifsean,      111098
     *        ifanul,navhrs                                             111098
          irec = irec + (78 + 12) / 13                                  111098
        else if (ivrsd .eq. 12) then                                    111098
CC          Version 1.2                                                 111098
          READ (igtfil,9001,iostat=ier,err=80,end=90) iswopt,itmunt,
     *        ngroup,(isogrp(i),i=1,20),(nsogrp(i),i=1,10),ifsean,      072497
     *        ifanul,navhrs,ifmode                                      072497
          irec = irec + (79 + 12) / 13                                  072497
        else
CC          Version 1.3 and greater                                     111098
          READ (igtfil,9001,iostat=ier,err=80,end=90) iswopt,itmunt,    072497
     *        ngroup,isogrp,nsogrp,ifsean,ifanul,navhrs,ifmode          072497
          irec = irec + (199 + 12) / 13                                 072497
        end if                                                          072497
        READ (igtfil,9002,iostat=ier,err=80,end=90) timavg,timinv,
     *      clrinx
        go to 10
CC
CC        file read error.
   80   if (KERRS(1,mgtnam,ier,irec,xdum,xdum,xdum,xdum,0) .eq. 1)
     *      go to 120
        go to 100
   90   if (KERRS(4,mgtnam,irec,ier,xdum,xdum,xdum,xdum,0) .ge. 1)
     *      go to 120
  100   if (irec .gt. 1) then
          if (isect .eq. 1) then
            ifrdwt(2) = -1
          else if (isect .eq. 2) then
            ifrdwt(3) = -1
          else if (isect .eq. 3) then
            ifrdwt(4) = -1
          else if (isect .eq. 4) then
            ifrdwt(1) = -1
          end if
          if (iflg .eq. 0) then
            if (isect .eq. 1) then
              ifrdwt(2) = 0
            else if (isect .eq. 2) then
              ifrdwt(3) = 0
            else if (isect .eq. 3) then
              ifrdwt(4) = 0
            else if (isect .eq. 4) then
              ifrdwt(1) = 0
            end if
            isect = isect + 1
          end if
          go to 10
        else
          i = jinit(ifrdwt,4,0,0)
        end if
      end if
  110 if (ifrsav .eq. 0) ifsave = 0
  120 return
CC
 9000 format (a80)
 9001 format (13i6)
 9002 format (5e15.7)
 9003 format (80a1)
      end
      Subroutine GETD2(mscrn,iline,itabs,ispac,iferr)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - GETD2 Version 01.2                          --- 082497
CC--- Subroutine description - reads OBODM save data files.         ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      character*1 mscrn
      character*80 mbufr
CC
      dimension mscrn(80,25),iline(20),itabs(20,10),ispac(20,10),
     *          iferr(20,10)
CC
      include 'OCNTRL.INC'
      include 'ONAMES.INC'
      include 'OCDSPM.INC'
      include 'OCALCS.INC'
CC
      equivalence (mbufr,mnpbuf(1))
CC
CC**********************************************************************
CC
CC        set up menus from save files.
CC
      i = JINIT(iferr,nmxlin*nmxfld,0,0)
      if (igtfil .gt. 0) then
CC                                                                      s
CC        place receptor data in work file, screen numscs(4) to
CC        numscs(4) + 4.
        numsc = numscs(4)
        numf = IRDSC(numsc,1,mscrn,iline,itabs,ispac,iferr,0)
        nvl = IWHER(1,iline,itabs,ispac,0,1,nln,nsc,nch)
CC        if rectangular coordinates.
        if (igrect .ne. 1) then
CC          rectangular.
          mscrn(nsc,nln) = malpha(18)
          igrect = 0
CC          if polar coordinates.
        else
CC          polar.
          mscrn(nsc,nln) = malpha(16)
        end if
CC
CC        receptor x,y units.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        call UNITS(mscrn(nsc,nln),nch,igunit,i,0,1,0)
CC
CC        receptor z units.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        call UNITS(mscrn(nsc,nln),nch,izunit,i,0,1,0)
CC
CC        receptor origin.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        i = IWTBF(mscrn(nsc,nln),nch,xorgin,1,iflrjs)
        nsc = JWHER(nvl,itabs,ispac,2,nch)
        i = IWTBF(mscrn(nsc,nln),nch,yorgin,1,iflrjs)
CC
CC        add source rectangular coords to xorgin,yorgin
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        if (iswopt(23) .eq. 0) then
          mscrn(nsc,nln) = malpha(14)
        else
          mscrn(nsc,nln) = malpha(25)
        end if
CC
CC        complex terrain flag.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        if (iswopt(6) .le. 1) then
          i = IWTBF(mscrn(nsc,nln),nch,zcoord(1),1,iflrjs)
        else if (iswopt(6) .eq. 3) then                                 120702
          mscrn(nsc,nln) = malpha(24)                                   120702
        else if (iswopt(6) .eq. 2) then                                 120702
          mscrn(nsc,nln) = malpha(3)
        end if
CC
CC        orientation angle units.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        if (igangu .ne. 1) then
CC          degrees.
          mscrn(nsc,nln) = malpha(30)
          igangu = 0
        else
CC          radians.
          mscrn(nsc,nln) = malpha(44)
        end if
CC
CC        receptor orientation angle.
        nsc = JWHER(nvl,itabs,ispac,2,nch)
        i = IWTBF(mscrn(nsc,nln),nch,grdang,2,iflrjs)
CC
CC        generate receptor grid system axes.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        mscrn(nsc,nln) = malpha(14)
CC        write screen data to work file.
        i = IWTSC(numsc,1,mscrn,iline,iferr)
CC
        if (ifgeng .ne. 0) then
CC          place grid system generation parameters in screen
CC          numscs(4) + 1.
          numsc = numscs(4) + 1
          numf = IRDSC(numsc,1,mscrn,iline,itabs,ispac,iferr,0)
CC
CC          start of x axis.
          nvl = IWHER(1,iline,itabs,ispac,0,1,nln,nsc,nch)
          i = IWTBF(mscrn(nsc,nln),nch,gxstrt,1,iflrjs)
CC
CC          increment for x axis.
          nsc = JWHER(nvl,itabs,ispac,2,nch)
          i = IWTBF(mscrn(nsc,nln),nch,gxincr,1,iflrjs)
CC
CC          end of x axis.
          nsc = JWHER(nvl,itabs,ispac,3,nch)
          i = IWTBF(mscrn(nsc,nln),nch,gxendp,1,iflrjs)
CC
CC          start of y axis.
          nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
          i = IWTBF(mscrn(nsc,nln),nch,gystrt,1,iflrjs)
CC
CC          increment for y axis.
          nsc = JWHER(nvl,itabs,ispac,2,nch)
          i = IWTBF(mscrn(nsc,nln),nch,gyincr,1,iflrjs)
CC
CC          end of y axis.
          nsc = JWHER(nvl,itabs,ispac,3,nch)
          i = IWTBF(mscrn(nsc,nln),nch,gyendp,1,iflrjs)
CC
CC          x,y,z coordinate data file name.
          nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
          call MVCH1(mxynam,mscrn(nsc,nln),22)
          i = LRJST(mscrn(nsc,nln),nch,iflrjs)
CC
CC          format of X coordinate data
          nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
          call MVCH1(mxform,mscrn(nsc,nln),60)
          i = LRJST(mscrn(nsc,nln),nch,iflrjs)
CC
CC          format of Y coordinate data
          nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
          call MVCH1(myform,mscrn(nsc,nln),60)
          i = LRJST(mscrn(nsc,nln),nch,iflrjs)
CC
CC          format of Z elevation data
          nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
          call MVCH1(mzform,mscrn(nsc,nln),60)
          i = LRJST(mscrn(nsc,nln),nch,iflrjs)
CC
CC          order of Z elevation data.
          nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
          if (iswopt(31) .eq. 0) then
            mscrn(nsc,nln) = malpha(1)
          else
            mscrn(nsc,nln) = malpha(2)
          end if
CC
CC          write data to work file.
          i = IWTSC(numsc,1,mscrn,iline,iferr)
        end if
CC
CC        place x axis, screen numscs(4)+2 in work file.
        numsc = numscs(4) + 2
        numf = IRDSC(numsc,1,mscrn,iline,itabs,ispac,iferr,0)
        n = nsubsc(numsc)                                               082497
        ner = IPACK(n,xcoord,xdumy,xdumy,xdumy,xdumy,xdumy,             082497
     *              nmxxyz,nxpnts,numsc,mscrn,iline,itabs,ispac,
     *              iferr,1,1)
CC
CC        place y axis, screen numscs(4)+3, in work file.
        numsc = numscs(4) + 3
        numf = IRDSC(numsc,1,mscrn,iline,itabs,ispac,iferr,0)
        n = nsubsc(numsc)                                               082497
        ner = IPACK(n,ycoord,xdumy,xdumy,xdumy,xdumy,xdumy,             082497
     *              nmxxyz,nypnts,numsc,mscrn,iline,itabs,ispac,
     *              iferr,2,1)
CC
CC        place discrete points, screen numscs(4)+5, in work file.
        numsc = numscs(4) + 5
        numf = IRDSC(numsc,1,mscrn,iline,itabs,ispac,iferr,0)
        n = nsubsc(numsc)                                               082497
        ner = IPACK(n,xdscrt,ydscrt,zdscrt,xdumy,xdumy,xdumy,           082497
     *              nmxxyz,nxypnt,numsc,mscrn,iline,itabs,ispac,
     *              iferr,3,1)
CC
CC        start source screen number (10).
        numsc = numscs(5)
        numf = IRDSC(numsc,1,mscrn,iline,itabs,ispac,iferr,0)
        nvl = IWHER(1,iline,itabs,ispac,0,1,nln,nsc,nch)
        call MBLK1(mscrn(nsc,nln),nch)
        call MVCH1(mpolnt,mscrn(nsc,nln),MIN0(nch,36))
        i = LRJST(mscrn(nsc,nln),nch,iflrjs)
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        call MBLK1(mscrn(nsc,nln),nch)
        call MVCH1(mspeci,mscrn(nsc,nln),MIN0(nch,36))
        i = LRJST(mscrn(nsc,nln),nch,iflrjs)
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
CC        gaseous or particulate.
        if (ifgpwn .eq. 0) then
          mscrn(nsc,nln) = malpha(7)
        else
          mscrn(nsc,nln) = malpha(16)
        end if
CC        molecular weight.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        if (whtmol .gt. 0.0) then
          i = IWTBF(mscrn(nsc,nln),nch,whtmol,5,iflrjs)
        end if
CC        density.
        nvl = IWHER(nvl,iline,itabs,ispac,1,2,nln,nscs,nchs)
        nsc = JWHER(nvl,itabs,ispac,1,nch)
        if (densty .gt. 0.0) then
          i = IWTBF(mscrn(nscs,nln),nchs,densty,5,iflrjs)
          call UNITS(mscrn(nsc,nln),nch,idunit,jdunit,0,1,11)
        end if
CC        pollutant/species half-life.
        nvl = IWHER(nvl,iline,itabs,ispac,1,2,nln,nscs,nchs)
        nsc = JWHER(nvl,itabs,ispac,1,nch)
        if (decays .gt. 0.0) then
          i = IWTBF(mscrn(nscs,nln),nchs,decays,3,iflrjs)
          call UNITS(mscrn(nsc,nln),nch,idcunt,jdumy,0,1,5)
        end if
CC        species for species/total fuel ratio.
        nvl = IWHER(nvl,iline,itabs,ispac,1,2,nln,nscs,nchs)
        nsc = JWHER(nvl,itabs,ispac,1,nch)
        if (spmass .gt. 0.0) then
          i = IWTBF(mscrn(nscs,nln),nchs,spmass,5,iflrjs)
          call UNITS(mscrn(nsc,nln),nch,isunit,jdumy,0,1,3)
        end if
CC        fuel for species/total fuel ratio.
        nvl = IWHER(nvl,iline,itabs,ispac,1,2,nln,nscs,nchs)
        nsc = JWHER(nvl,itabs,ispac,1,nch)
        if (qtfuel .gt. 0.0) then
          i = IWTBF(mscrn(nscs,nln),nchs,qtfuel,5,iflrjs)
          call UNITS(mscrn(nsc,nln),nch,ifunit,jdumy,0,1,3)
        end if
CC        fraction of species per total material.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        if (qfract .gt. 0.0) then
          i = IWTBF(mscrn(nsc,nln),nch,qfract,nch-2,iflrjs)
        end if
CC        if source data hourly.
        nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
        if (ifhsly .eq. 0) then
          mscrn(nsc,nln) = malpha(14)
        else
          mscrn(nsc,nln) = malpha(25)
        end if
CC        write data to work file.
        i = IWTSC(numsc,1,mscrn,iline,iferr)
CC        if particulate data.
        if (ifgpwn .gt. 0) then
CC          screen 11.
          numsc = numscs(5) + 1
          numf = IRDSC(numsc,1,mscrn,iline,itabs,ispac,iferr,0)
CC          particle diameter units.
          nvl = IWHER(1,iline,itabs,ispac,0,1,nln,nsc,nch)
          call UNITS(mscrn(nsc,nln),nch,ipsunt,jdumy,0,1,0)
CC          particle settling velocity units.
          nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
          call UNITS(mscrn(nsc,nln),nch,ipvunt,jpvunt,0,1,7)
          if (ifgend .ne. 0) then
CC            automatic generation of particle size distribution.
            nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
CC            number of size categories.
            xdumy = nvsprt
            i = IWTBF(mscrn(nsc,nln),nch,xdumy,0,iflrjs)
CC            mass-median diameter.
            nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
            i = IWTBF(mscrn(nsc,nln),nch,drpmmd,2,iflrjs)
CC            geometric standard deviation.
            nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
            i = IWTBF(mscrn(nsc,nln),nch,drpsig,5,iflrjs)
          else
            nvl = nvl + 3
          end if
CC          automatic generation of particle cats.
          nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
          mscrn(nsc,nln) = malpha(14)
CC          ratio of lagrangian to eulerian time scales.
          nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
          i = IWTBF(mscrn(nsc,nln),nch,betale,1,iflrjs)
CC          write data to work file.
          i = IWTSC(numsc,1,mscrn,iline,iferr)
CC          particle distribution, screen12.
          numsc = numscs(5) + 2
CC          pack particulate data and go to next source menu.
          n = nsubsc(numsc)                                             082497
          ner = IPACK(n,drpupr,drplwr,pctmat,vspart,gammad,xdumy,       082497
     *                nmxdrp,nvsprt,numsc,mscrn,iline,itabs,ispac,
     *                iferr,4,1)
        end if
      end if
      return
      end
      Subroutine GETD3(mscrn,iline,itabs,ispac,iferr)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - GETD3 Version 01.2                          ---
CC--- Subroutine description - reads OBODM save data files.         ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      character*1 mscrn
CC
      dimension mscrn(80,25),iline(20),itabs(20,10),ispac(20,10),
     *          iferr(20,10)
CC
      include 'OCNTRL.INC'
      include 'ONAMES.INC'
      include 'OCDSPM.INC'
      include 'OCALCS.INC'
CC
CC**********************************************************************
CC
CC        set initial data from save files.
CC
      if (igtfil .gt. 0) then
CC        number of sources, screen13.
        numsc = numscs(5) + 3
        numf = IRDSC(numsc,1,mscrn,iline,itabs,ispac,iferr,0)
CC        total number of sources.
        nvl = IWHER(1,iline,itabs,ispac,0,1,nln,nsc,nch)
        xdumy = nsourc
        i = IWTBF(mscrn(nsc,nln),nch,xdumy,0,iflrjs)
        i = IWTSC(numsc,1,mscrn,iline,iferr)
CC        loop over sources.
        do 20 isorc=1,nsourc
CC          screen14.
          numsc = numscs(5) + 4
CC        source data for source isorc.
          numf = IRDSC(numsc,isorc,mscrn,iline,itabs,ispac,iferr,0)
CC          source id name.
          nvl = IWHER(1,iline,itabs,ispac,0,1,nln,nsc,nch)
          call MBLK1(mscrn(nsc,nln),nch)
          call MVCH1(msoids(1,isorc),mscrn(nsc,nln),MIN0(nch,36))
          i = LRJST(mscrn(nsc,nln),nch,iflrjs)
CC          source type (1-volume,2-line).
          jtype = IVGET(isotyp(isorc),1)
          if (jtype .eq. 1) then
CC            volume source.
            idumy = 22
          else if (jtype .eq. 2) then
CC            line source.
            idumy = 12
          else
            idumy = 22
          end if
          nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
          mscrn(nsc,nln) = malpha(idumy)
CC          source emission type (1-instantaneous,
CC          2-quasi-continuous square wave.
          itype = IVGET(isotyp(isorc),2)
          nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
          if (itype .eq. 1) then
CC            instantaneous source.
            idumy = 9
          else if (itype .eq. 2) then
CC            quasi-continuous source.
            idumy = 17
          else
            idumy = 9
          end if
          mscrn(nsc,nln) = malpha(idumy)
CC          set indices, depending on the source menu used.
CC          mass units.
          j1 = 3
CC          heat/mass units.
          j2 = 13
CC          mass/time units.
          j3 = 12
CC          get units of total mass or rate of material burned.
          nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
          idumy = IVGET(iqunit(isorc),2)
          jdumy = IVGET(iqunit(isorc),1)
          call UNITS(mscrn(nsc,nln),nch,idumy,jdumy,0,1,j1)
CC          total mass of material or emission rate.
          nsc = JWHER(nvl,itabs,ispac,2,nch)
          i = IWTBF(mscrn(nsc,nln),nch,qemiss(isorc),5,iflrjs)
CC          units of heat content.
          nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
          idumy = IVGET(jqunit(isorc),1)
          jdumy = IVGET(jqunit(isorc),2)
          call UNITS(mscrn(nsc,nln),nch,idumy,jdumy,0,1,j2)
CC          fuel heat content.
          nsc = JWHER(nvl,itabs,ispac,2,nch)
          i = IWTBF(mscrn(nsc,nln),nch,hetcnt(isorc),5,iflrjs)
          nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
          if (itype .eq. 2) then
CC            units of fuel burn rate.
            idumy = IVGET(kqunit(isorc),1)
            jdumy = IVGET(kqunit(isorc),2)
            call UNITS(mscrn(nsc,nln),nch,idumy,jdumy,0,1,j3)
CC            fuel burn rate.
            nsc = JWHER(nvl,itabs,ispac,2,nch)
            i = IWTBF(mscrn(nsc,nln),nch,brnrat(isorc),5,iflrjs)
          else
            call mblk1(mscrn(nsc,nln),nch)
            nsc = JWHER(nvl,itabs,ispac,2,nch)
            call mblk1(mscrn(nsc,nln),nch)
            if (iflrjs .eq. 0) then
              mscrn(nsc+nch-1,nln) = malpha(3)
            else
              mscrnm(nsc,nln) = malpha(3)
            end if
          end if
CC          get hours during which source is burned or detonated.
          do 10 j=1,24
            jj = MOD(j,10)
            if (jj .eq. 0) jj = 10
            if (jj .eq. 1) then
              nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
            end if
            nsc = JWHER(nvl,itabsm,ispacm,jj,nch)
            if (isofrq(j,isorc) .ne. 0) then
              mscrn(nsc,nln) = malpha(25)
            else
              mscrn(nsc,nln) = malpha(14)
            end if
   10     continue
          i = IWTSC(numsc,isorc,mscrn,iline,iferr)
CC          instantaneous or quasi-continuous source, screen15.
          if (jtype .ne. 2) then
            if (itype .ne. 1) then
CC              screen 15.
              numsc = numscs(5) + 5
            else
CC              screen 16.
              numsc = numscs(5) + 6
            end if
          else
CC            screen 17.
            numsc = numscs(5) + 7
          end if
          numf = IRDSC(numsc,isorc,mscrn,iline,itabs,ispac,iferr,0)
CC          source coordinates are rectangular or polar.
          idumy = IVGET(isrect(isorc),2)
          nvl = IWHER(0,iline,itabs,ispac,1,1,nln,nsc,nch)
          if (idumy .eq. 0) then
CC            rectangular.
            jdumy = 18
          else
CC            polar.
            jdumy = 16
          end if
          mscrn(nsc,nln) = malpha(jdumy)
CC          units x source coordinates.
          i = IVGET(isrect(isorc),1)
          nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
          call UNITS(mscrn(nsc,nln),nch,i,jdumy,0,1,0)
CC          source x coordinate.
          nsc = JWHER(nvl,itabs,ispac,2,nch)
          i = IWTBF(mscrn(nsc,nln),nch,clddxs(isorc),1,iflrjs)
CC          source y coordinate.
          nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
          i = IWTBF(mscrn(nsc,nln),nch,clddys(isorc),1,iflrjs)
CC          source base elevation.
          nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
          i = IWTBF(mscrn(nsc,nln),nch,clddzs(isorc),1,iflrjs)
CC          emission height units.
          i = IVGET(isrect(isorc),3)
          nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
          call UNITS(mscrn(nsc,nln),nch,i,jdumy,0,1,0)
CC          emission height.
          nsc = JWHER(nvl,itabs,ispac,2,nch)
          i = IWTBF(mscrn(nsc,nln),nch,relhgt(isorc),1,iflrjs)
          if (jtype .eq. 2) then
CC            end of line source information.
CC            x coordinate of end of line source.
            nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
            i = IWTBF(mscrn(nsc,nln),nch,clddxe(isorc),2,iflrjs)
CC            y coordinate of end of line source.
            nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
            i = IWTBF(mscrn(nsc,nln),nch,clddye(isorc),2,iflrjs)
CC            z source base elevation at end of line source.
            nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
            i = IWTBF(mscrn(nsc,nln),nch,clddze(isorc),2,iflrjs)
CC            height of end of line source.
            nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
            i = IWTBF(mscrn(nsc,nln),nch,relhte(isorc),2,iflrjs)
CC            units of width of line source.
            i = IVGET(idrect(isorc),1)
            nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
            call UNITS(mscrn(nsc,nln),nch,i,jdumy,0,1,0)
CC            width of line source.
            nsc = JWHER(nvl,itabs,ispac,2,nch)
            i = IWTBF(mscrn(nsc,nln),nch,cldsht(isorc),2,iflrjs)
CC            units of line source vertical dimension.
            i = IVGET(idrect(isorc),2)
            nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
            call UNITS(mscrn(nsc,nln),nch,i,jdumy,0,1,0)
CC            depth or vertical dimension of initial source.
            nsc = JWHER(nvl,itabs,ispac,2,nch)
            i = IWTBF(mscrn(nsc,nln),nch,clddpt(isorc),2,iflrjs)
          else
            if (itype .eq. 1) then
CC              units of instantaneous source diameter.
              i = IVGET(idrect(isorc),1)
              nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
              call UNITS(mscrn(nsc,nln),nch,i,jdumy,0,1,0)
CC              instantaneous source diameter.
              nsc = JWHER(nvl,itabs,ispac,2,nch)
              i = IWTBF(mscrn(nsc,nln),nch,cldlng(isorc),2,iflrjs)
            else
CC              quasi-continuous length units of long side of source.
              i = IVGET(idrect(isorc),1)
              nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
              call UNITS(mscrn(nsc,nln),nch,i,jdumy,0,1,0)
CC              long side of source.
              nsc = JWHER(nvl,itabs,ispac,2,nch)
              i = IWTBF(mscrn(nsc,nln),nch,cldlng(isorc),2,iflrjs)
CC              length of short side of source.
              nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
              i = IWTBF(mscrn(nsc,nln),nch,cldsht(isorc),2,iflrjs)
CC              angle to long side of source.
              nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
              i = IWTBF(mscrn(nsc,nln),nch,cldang(isorc),2,iflrjs)
CC              depth or vertical dimension units of initial source.
              i = IVGET(idrect(isorc),2)
              nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
              call UNITS(mscrn(nsc,nln),nch,i,jdumy,0,1,0)
CC              vertical source dimension.
              nsc = JWHER(nvl,itabs,ispac,2,nch)
              i = IWTBF(mscrn(nsc,nln),nch,clddpt(isorc),2,iflrjs)
            end if
          end if
          i = IWTSC(numsc,isorc,mscrn,iline,iferr)
CC          remaining source parameters, screen 18.
          numsc = numscs(5) + 8
          numf = IRDSC(numsc,isorc,mscrn,iline,itabs,ispac,iferr,0)
CC          air entrainment coefficient.
          nvl = IWHER(1,iline,itabs,ispac,0,1,nln,nsc,nch)
          i = IWTBF(mscrn(nsc,nln),nch,gammat(isorc),3,iflrjs)
CC          units of lateral cloud expansion distance.
          nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
          idumy = IVGET(ismunt(isorc),1)
          call UNITS(mscrn(nsc,nln),nch,idumy,jdumy,0,1,0)
CC          lateral rectilinear cloud expansion distance.
          nsc = JWHER(nvl,itabs,ispac,2,nch)
          i = IWTBF(mscrn(nsc,nln),nch,xsmlry(isorc),1,iflrjs)
CC          units of vertical cloud expansion distance.
          nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
          idumy = IVGET(ismunt(isorc),2)
          call UNITS(mscrn(nsc,nln),nch,idumy,jdumy,0,1,0)
CC          vertical rectilinear cloud expansion distance.
          nsc = JWHER(nvl,itabs,ispac,2,nch)
          i = IWTBF(mscrn(nsc,nln),nch,xsmlrz(isorc),1,iflrjs)
CC          units of lateral reference distance.
          nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
          idumy = IVGET(ismunt(isorc),3)
          call UNITS(mscrn(nsc,nln),nch,idumy,jdumy,0,1,0)
CC          lateral reference distance.
          nsc = JWHER(nvl,itabs,ispac,2,nch)
          i = IWTBF(mscrn(nsc,nln),nch,xlrgry(isorc),1,iflrjs)
CC          units of vertical reference distance.
          nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
          idumy = IVGET(ismunt(isorc),4)
          call UNITS(mscrn(nsc,nln),nch,idumy,jdumy,0,1,0)
CC          vertical reference distance.
          nsc = JWHER(nvl,itabs,ispac,2,nch)
          i = IWTBF(mscrn(nsc,nln),nch,xlrgrz(isorc),1,iflrjs)
CC          lateral cloud expansion coefficient.
          nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
          i = IWTBF(mscrn(nsc,nln),nch,alphas(isorc),2,iflrjs)
CC          vertical cloud expansion coefficient.
          nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
          i = IWTBF(mscrn(nsc,nln),nch,betass(isorc),2,iflrjs)
          i = IWTSC(numsc,isorc,mscrn,iline,iferr)
   20   continue
      end if
      return
      end
      Subroutine GETD4(mscrn,iline,itabs,ispac,iferr)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - GETD4 Version 01.1                          ---
CC--- Subroutine description - reads OBODM save data files.         ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      character*1 mscrn
      character*58 mfnam
CC
      dimension mscrn(80,25),iline(20),itabs(20,10),ispac(20,10),
     *          iferr(20,10)
CC
      include 'OCNTRL.INC'
      include 'ONAMES.INC'
      include 'OCDSPM.INC'
      include 'OCALCS.INC'
CC
      equivalence (mfnam,mscrch(1))
CC
CC**********************************************************************
CC
CC        set initial data from save files.
CC
      if (igtfil .gt. 0) then
        if (ifhsly .ne. 0) then
CC
CC          screen numscs(5)+9, hourly source data, screen 19.
          numsc = numscs(5) + 9
          numf = IRDSC(numsc,1,mscrn,iline,itabs,ispac,iferr,0)
CC          name of input hourly source data file.
          nvl = IWHER(1,iline,itabs,ispac,0,1,nln,nsc,nch)
          call MVCH1(mshnam,mscrn(nsc,nln),MIN0(nch,22))
          i = LRJST(mscrn(nsc,nln),nch,iflrjs)
          if (NUMC1(mshnam,22) .le. 0) go to 20                         030499
CC          open data file as logical unit ishfil.
          ishfil = 28
CC          combine file name and directory.
          call FNAME(mfnam,58,mdirec,mshnam)
          CLOSE (unit=ishfil,status='keep',err=10)
CC              formatted character.
   10     OPEN (unit=ishfil,file=mfnam,iostat=ier,err=20,
     *          status='old',form='formatted')
          go to 30
   20     iferr(nvl,1) = 1
          ifrdwt(3) = -1
          ishfil = 0                                                    030499
          if (KERRS(3,mshnam,ier,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *        go to 40
   30     i = IWTSC(numsc,1,mscrn,iline,iferr)
          i = JINIT(iferr,nmxlin*nmxfld,0,0)
CC
CC          screen numscs(5)+10, which source data are hourly source
CC          data, screen 20.
          numsc = numscs(5) + 10
          numf = IRDSC(numsc,1,mscrn,iline,itabs,ispac,iferr,1)
CC          check if source number in file.
          nvl = IWHER(1,iline,itabs,ispac,0,1,nln,nsc,nch)
          if (ivarso(1) .gt. 0) then
            xdumy = ivarso(1)
            i = IWTBF(mscrn(nsc,nln),nch,xdumy,0,iflrjs)
          end if
CC          check if total mass of material is in data file.
          nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
          if (ivarso(2) .gt. 0) then
            xdumy = ivarso(2)
            i = IWTBF(mscrn(nsc,nln),nch,xdumy,0,iflrjs)
          end if
CC          check if heat content is in data file.
          nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
          if (ivarso(3) .gt. 0) then
            xdumy = ivarso(3)
            i = IWTBF(mscrn(nsc,nln),nch,xdumy,0,iflrjs)
          end if
CC          check if burn rate is in data file.
          nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
          if (ivarso(4) .gt. 0) then
            xdumy = ivarso(4)
            i = IWTBF(mscrn(nsc,nln),nch,xdumy,0,iflrjs)
          end if
CC            write screen data.
          i = IWTSC(numsc,1,mscrn,iline,iferr)
CC
CC          screen (21) numscs(5)+11, hourly source data format.
          if (ihsfmt .gt. 0) then
            numsc = numscs(5) + 11
            numf = IRDSC(numsc,1,mscrn,iline,itabs,ispac,iferr,0)
CC            set data record format.
            nvl = IWHER(1,iline,itabs,ispac,0,1,nln,nsc,nch)
            call MVCH1(mforms,mscrn(nsc,nln),MIN0(nch,75))
            i = NUMC1(mforms,150)
            if (i .gt. 75) then
              nvl = IWHER(nvl,iline,itabs,ispac,1,1,nln,nsc,nch)
              call MVCH1(mforms(76),mscrn(nsc,nln),MIN0(nch,i-75))
            end if
CC            write screen data.
            i = IWTSC(numsc,1,mscrn,iline,iferr)
          end if
        end if
      end if
   40 return
      end
