CC      Include file common block OCALCS (OBODM version 1.3)
CC        Source related parameters.  Note if the dimension of the
CC        source parameters is changed, nmxsor must also be changed.
      common /OCALCS/ nsourc,nlsorc,isunit,ifunit,ipsunt,ipvunt,jpvunt,
     *                idunit,jdunit,idcunt,iduhet,jduhet,idubrn,jdubrn,
     *                ifhsly,ivarso(4),insrec,ihsfmt,heatdf,burndf,
     *                hgtmax,spmass,qtfuel,qfract,isrect(50),idrect(50),072497
     *                iqunit(50),jqunit(50),kqunit(50),ismunt(50),      072497
     *                isotyp(50),qemiss(50),taucld(50),relhgt(50),      072497
     *                clddxs(50),clddys(50),clddzs(50),cldlng(50),      072497
     *                cldsht(50),cldang(50),xsmlry(50),xsmlrz(50),      072497
     *                xlrgry(50),xlrgrz(50),hetcnt(50),brnrat(50),      072497
     *                clddxe(50),clddye(50),clddze(50),relhte(50),      072497
     *                cldmax(50),gammat(50),alphas(50),betass(50),      072497
     *                cldmxd(50),clddpt(50),calhgt(50),isofrq(24,50),   072497
     *                nforms,ifcalc                                     072497
CC        Receptor related parameters.  Note if the dimension of zcoord
CC        or calcsa are changed, nmxcor must also be changed.  Calcsa
CC        is dimensioned to 2 * nmxcor and zcoord to nmxcor.  Also, if
CC        the dimension of calcsa is changed see Subroutines HD01Q,
CC        HD02Q, LBITQ which equivalence arrays to calcsa. Also, if
CC        the dimension of xcoord, ycoord, xdscrt, ydscrt, xteran or
CC        yteran are changed, nmxxyz must also be changed.
      common /OCALCS/ igrect,igunit,izunit,igangu,nxpnts,nypnts,nxypnt,
     *                ifgeng,ngrdpt,ntotpt,ntotal,jjjoff(4),xcoord(100),
     *                ycoord(100),zcoord(10000),xdscrt(100),ydscrt(100),
     *                zdscrt(100),grdang,xorgin,yorgin,gxstrt,gxincr,
     *                gxendp,gystrt,gyincr,gyendp,ascale,bscale
CC        Calculation array
      common /OCALCS/ calcsa(20000),xteran(100),yteran(100)
CC      End common block OCALCS
