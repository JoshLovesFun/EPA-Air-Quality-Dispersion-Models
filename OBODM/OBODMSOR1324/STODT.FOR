      Subroutine STODT
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - STODT Version 01.2                          ---
CC--- Subroutine description - writes OBODM save data files.        ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      character*1 mdnam,menam,msnam,mxynm,mxfrm,myfrm,mzfrm
      character*17 mesg
CC
      dimension mdnam(22),menam(22),msnam(22),mxynm(22),mesg(4),
     *          mxfrm(60),myfrm(60),mzfrm(60)
CC
      include 'OCNTRL.INC'
      include 'ONAMES.INC'
      include 'OCDSPM.INC'
      include 'OCALCS.INC'
CC
      equivalence (mnhnam,mdnam(1)),(mshnam,menam(1)),(msgnam,msnam(1)),
     *            (mxynam,mxynm(1)),(mxform,mxfrm(1)),(myform,myfrm(1)),
     *            (mzform,mzfrm(1))
CC
      data mesg/'A - Receptor data',
     *          'B - Source data  ',
     *          'C - Met. data    ',
     *          'D - Control data '/
CC
CC**********************************************************************
CC
CC        write data to save files.
CC
      if (isvfil .gt. 0) then
        REWIND (unit=isvfil,iostat=ier,err=10)
        irec = 1
CC
CC        write Version number.
        ivrsd = versod * 10.0 + 0.1
        WRITE (isvfil,9002,iostat=ier,err=10) ivrsd
        irec = irec + 1
CC
CC        write receptor data.
        WRITE (isvfil,9000,iostat=ier,err=10) mesg(1)
        irec = irec + 1
        WRITE (isvfil,9002,iostat=ier,err=10) igrect,igunit,igangu,
     *         nxpnts,nypnts,nxypnt,ifgeng,ngrdpt,ntotpt,ntotal,jjjoff,
     *         izunit
        irec = irec + 2
        WRITE (isvfil,9003,iostat=ier,err=10) xcoord,ycoord,zcoord,
     *         xdscrt,ydscrt,zdscrt,grdang,xorgin,yorgin,gxstrt,
     *         gxincr,gxendp,gystrt,gyincr,gyendp
        irec = irec + (nmxxyz * 5 + nmxxyz * nmxxyz + 9 + 4) / 5
        WRITE (isvfil,9001,iostat=ier,err=10) mxynm,mxfrm,myfrm,mzfrm
        irec = irec + 3
CC
CC        write source data.
        WRITE (isvfil,9000,iostat=ier,err=10) mesg(2)
        irec = irec + 1
        WRITE (isvfil,9001,iostat=ier,err=10) mpolnt,mspeci,msoids,
     *                                        mforms,menam
        irec = irec + (36 + 36 + 360 + 151 + 22 + 79) / 80
        WRITE (isvfil,9002,iostat=ier,err=10) nvsprt,ifgpwn,ifgend,
     *         nsourc,isunit,ifunit,ipsunt,ipvunt,jpvunt,idunit,jdunit,
     *         idcunt,isrect,idrect,iqunit,jqunit,kqunit,ismunt,isotyp,
     *         ifhsly,ivarso,ihsfmt,isofrq
        irec = irec + (14 + 7 * nmxsor + 240 + 12) / 13
        WRITE (isvfil,9003,iostat=ier,err=10) dcutof,betale,decays,
     *         densty,whtmol,drpmmd,drpsig,drpupr,drplwr,pctmat,vspart,
     *         gammad,spmass,qtfuel,qfract,qemiss,taucld,relhgt,clddxs,
     *         clddys,cldlng,cldsht,cldang,clddpt,xsmlry,xsmlrz,xlrgry,
     *         xlrgrz,hetcnt,brnrat,clddxe,clddye,relhte,gammat,alphas,
     *         betass,clddzs,clddze
        irec = irec + (10 + 5 * nmxdrp + 23 * nmxsor + 4) / 5
CC
CC        write met. data.
        WRITE (isvfil,9000,iostat=ier,err=10) mesg(3)
        irec = irec + 1
        WRITE (isvfil,9001,iostat=ier,err=10) mformt,mdnam
        irec = irec + (151 + 22 + 79) / 80
        WRITE (isvfil,9002,iostat=ier,err=10) ifhrly,iceunt,ispunt,
     *        jspunt,issunt,jssunt,idsunt,jdsunt,isxunt,isyunt,iszunt,
     *        istunt,ihmunt,ivgunt,jvgunt,irount,itpunt,iprunt,ihrfmt,
     *        ischmd,ivarmt,istabl,iniyrs,inimon,inidys,inihrs,inimin,
     *        injdys,ifdstm,iscyrs,iscdys,ihunit
        irec = irec + (30 + 25 + 366 + 12) / 13
        WRITE (isvfil,9003,iostat=ier,err=10) refspd,pwrlaw,wnddir,
     *        sigmap,sigmep,trbixr,airtmp,airprs,dphidz,hmdpth,spdshr,
     *        dirshr,radinx,tauosa,roughl,cldcov,cldhgt,refhgt,alphad,
     *        betadf,zondif,geolon,geolat,airhum,stblty,clearx,vllimt,
     *        vulimt,hgtmet
        irec = irec + (27 + 2 * 25 + 4) / 5
CC
CC        write control data.
        WRITE (isvfil,9000,iostat=ier,err=10) mesg(4)
        irec = irec + 1
        WRITE (isvfil,9001,iostat=ier,err=10) mtitle,msnam
        irec = irec + (50 + 22 + 79) / 80
        WRITE (isvfil,9002,iostat=ier,err=10) iswopt,itmunt,ngroup,
     *        isogrp,nsogrp,ifsean,ifanul,navhrs,ifmode
        irec = irec + (40 + 4 + 20 + 10 + 6 + 12) / 13
        WRITE (isvfil,9003,iostat=ier,err=10) timavg,timinv,clrinx
        irec = irec + 1
        go to 20
CC
CC        file write error.
   10   iferrs = KERRS(2,msvnam,ier,irec,xdum,xdum,xdum,xdum,0)
CC
      end if
   20 return
CC
 9000 format (a17)
 9001 format (80a1)
 9002 format (13i6)
 9003 format (5e15.7)
      end
