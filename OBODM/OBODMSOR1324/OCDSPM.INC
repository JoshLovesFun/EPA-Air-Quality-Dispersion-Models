CC      Include file common block OCDSPM (OBODM Version 1.2)
CC        Particle related parameters. Note if the dimension of arrays
CC        drpupr, drplwr, pctmat, vspart, gammad are changed, nmxdrp
CC        must be changed.
      common /OCDSPM/ nvsprt,ifgpwn,dcutof,betale,ifgend,decays,densty,
     *                whtmol,drpmmd,drpsig,drpupr(50),drplwr(50),
     *                pctmat(50),vspart(50),gammad(50)
CC        meteorological related parameters
      common /OCDSPM/ ifhrly,iceunt,ispunt,jspunt,issunt,jssunt,idsunt,
     *                jdsunt,isxunt,isyunt,iszunt,istunt,ihmunt,ivgunt,
     *                jvgunt,irount,itpunt,iprunt,ihrfmt,ischmd,
     *                ivarmt(25),istabl,ischrs,nformt,ihunit,iscdct,
     *                nmxdys,nmxhrs,inhrec,refspd,pwrlaw,wnddir,sigmap,
     *                sigmep,trbixr,airtmp,airprs,dphidz,hmdpth,spdshr,
     *                dirshr,stblty,radinx,tauosa,roughl,airhum,clearx,
     *                cldcov,cldhgt,airden,airmol,wndspd,refhgt,alphad,
     *                betadf,spshrc,gammac,gammai,airvsc,clrinx,
     *                convrt(20,3),vllimt(25),vulimt(25),hgtmet
CC        Time/date related parameters
      common /OCDSPM/ iniyrs,inimon,inidys,inihrs,injdys,inimin,ifdstm,
     *                zondif,iscyrs,iscdys(366),iseasn,nseasn(4),itmunt,
     *                timavg,timinv,iofhrs,iquart,nquart(4)
CC        Terrain related parameters
      common /OCDSPM/ geolon,geolat
CC        Calculated temporary parameters
      common /OCDSPM/ kssndx,nvlprt,ngrpct,zpsave,ngrplt,ifmxpc,ifmxpi,
     *                vertcl,timtox,sigxax,sigyax,sigzax,dekayd,tcosth,
     *                tsinth,xtemp1,ytemp1,xpdist,ypdist,zpdist,xxdist,
     *                xzdist,xydist,zbotly,ztoply,delthp,timtau,sigatr,
     *                sigepr,sigapr,factrs,alatrl,sigxop,sigyop,sigzop,
     *                vertgv,pekdos,pekcon,pconav,pdepgr,hightl,sigxls,
     *                sigyls,sigzls,xcord1,ycord1,qkemss,cldris,cldfct,
     *                tautmo,clddst,alongw,sigyta,trbixt,trbixp,timtas,
     *                sigmaq,sigmeq,trbixq,wspmin,pekcna,timris,svdxss, 110198
     *                svdyss,svdzss,svhgts,svisss,svlngs,svshts,svdpts, 110198
     *                svhtes,dxincs,dyincs,dzincs,dhincs,dqincs,nlincs, 110198
     *                dxssss,dyssss,dzssss,dhssss,ifutms,jfutms,kfutms, 110198                       
     *                lfutms,ifwrtt(10),icpont                          110198
CC        Conversion constants
      common /OCDSPM/ dtorad,radtod,sqr2pi,sq2pii,sqrt2i,twopii,
     *                constl(10),consth(5),constm(8),constp(4),
     *                constt(3),constc(4,2),consta(11),constv(13),
     *                constg(2),constr(4),gravty,spheat
CC      End common block OCDSPM
