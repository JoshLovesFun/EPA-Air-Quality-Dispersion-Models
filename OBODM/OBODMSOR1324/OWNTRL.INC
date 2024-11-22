CC      Include file common block OWNTRL (OBODM Version 1.3)
CC        Graphics parameters.  Note that if the dimension of the
CC        arrays spclxs, spclys, spclht, spclan, ispclc, isppol, ispclr,
CC        are changed, nmxspl must be changed.
      common /OWNTRL/ nbxpss,ioffst(2),nmxctr,nmxspl,nmxfnc,nxyfnc,
     *                ncontr(2),joffs1,ifpass,contrs(20,2),ifgpol,
     *                ifspol,iswplt(30),xnchpl,ynchpl,sclxes,sclyes,
     *                icolor,lcolor,rastrh,rastrv,ibaudr,nplots,pxmins,
     *                pymins,pxmaxs,pymaxs,xlmrgn,ybmrgn,xrmrgn,ytmrgn,
     *                ichars,xinchs,yinchs,xplpnt,yplpnt,htpchr,htnchr,
     *                htlchr,splftm,spbotm,xrmrpg,ytmrpg,irxndx,iryndx,
     *                jrxndx,jryndx,lswxax,lswyax,ndgxes,ndgyes,xincax,
     *                yincax,nincxs,nincys,zmincs(2),zminds(2),xsmins,
     *                ysmins,nchtcr,nchxcr,nchycr,nchlcr,csmaxs(2),
     *                csmins(2),xymins,sclxys,spmins,ifbndx,xbndxs,
     *                ybndxs,hcminc,ipoptn(10),npoptn,jpoptn,ifhcpy,
     *                spfrct,ifdash,ncurvs,nctlbl,ixlpnt(60,2),
     *                iylpnt(60,2),ialpnt(60,2),ixlpts(200),
     *                iylpts(200),spclxs(20),spclys(20),spclht(20),
     *                spclan(20),ispclc(20),isppol(20),ispclr(20),
     *                nspcls,icontr,cscale,dscale,jsogrp(100)           072497
CC
CC        Note if the second dimension of icnclr and icndsh (2) is
CC        changed, nmxctr must be changed.  Also, if the dimension
CC        of functx and functy are changed, nmxfnc must be changed.
      common /OWNTRL/ icnclr(20,2),icndsh(20,2),ipmode,functx(100),
     *                functy(100),xbxpsl(100),xbxpel(100),ybxpsl(100),
     *                ybxpel(100),ilstxs,ilstys,ifnewp,imodes,lmodes,
     *                ncolmn,iboard,pixelx,pixely,ifswxy,ifcolr,topmrg,
     *                botmrg,xftmrg,rgtmrg,ifprno,nhravg,nhrlab,ifchlb,
     *                ifavrg,paperw,paperl,iftpol
CC      End common block OWNTRL
