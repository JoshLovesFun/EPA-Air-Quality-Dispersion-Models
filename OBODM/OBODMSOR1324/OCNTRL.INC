CC      Include file common block OCNTRL (OBODM Version 1.3)
CC        General control parameters.  Note if the dimension of arrays
CC        nsogrp and isogrp are changed, nmxgrp must be changed and the
CC        dimension of isogrp must be twice that of nsogrp.
      integer*2 icolmx,irowmx,imxpix,imypix,isvmod,isvbrd,ifmous,imxscl,
     *          imyscl,imsech,imsecl,numbtn,locxms,locyms,icmnum,icmcol,
     *          icmrow,icmbeg,icmlen,icmsta,icmmap,icmkey
      common /OCNTRL/ idbfil,iwkfil,igtfil,isvfil,iotfil,iogfil,inhfil,
     *                ishfil,iplfil,ifdfil,iscfil,itrfil,impfil,ibcfil,
     *                ierfil,isgfil,imxfil,isystm,ibcrec,llines,linetl,
     *                ipages,itable,iferrs,ifnext,ifinit,ifdbug,ifprlp,
     *                iftran,rmissd,rmissc,nmxcor,nmxxyz,nmxsor,nmxdrp,
     *                nmxgrp,nmxscn,numscs(20),numsce(20),nrecsc(100),
     *                nrecda(100),nrecdt(100),nsubsc(100),nkeybd(2,28),
     *                ibackt,iforet,ibackd,ifored,verson,nmxrow,nmxcol,
     *                nmxlin,nmxfld,iflrjs,versod,ifrdwt(6),ilbstr(5),
     *                nlbsch(5),ngroup,nsogrp(50),isogrp(100),nchtrs,   072497
     *                iswopt(38),ntothr,navhrs(6),ifsean,ifanul,iflstr,
     *                iflend,ifgrph,nxtran,nytran,istrec,ilinem(20),
     *                itabsm(20,10),ispacm(20,10),iferrm(20,10),irnyrs,
     *                irnmon,irndys,irnhrs,irnjdy,ibpage,idpage,ispage,
     *                irdcol,irdrow,iptrec,iptpnt,iogrec,iogpnt,
     *                iptbuf(128),ircbuf(128),ifbtch,ifrsav,ifsave
      common /OCNTRL/ ifstch,nbtchc,ifrddm,isgrec,ifstco,ifstcp,ifccok,
     *                ifctlc,ifcnvt,ifpsol,ifhiof,ifmode,icolmx,irowmx,
     *                imxpix,imypix,isvmod,isvbrd,ifmous,imxscl,imyscl,
     *                imsech,imsecl,numbtn,locxms,locyms,icmnum,icmcol,
     *                icmrow,icmbeg(8),icmlen(8),icmsta(8),icmmap(8),
     *                icmkey(8),nochar,ivrmod                           090498
CC      End common block OCNTRL
