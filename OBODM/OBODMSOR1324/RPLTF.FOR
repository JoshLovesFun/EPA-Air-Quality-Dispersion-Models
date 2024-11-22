      Subroutine RPLTF
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - RPLTF Version 01.2                          --- 041806
CC--- Subroutine description - determine plot scale, inches, etc.,  ---
CC---                          plot axes, label axes, plot grid     ---
CC---                          coordinate points, plot north arrow. ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
CC
      include 'OCNTRL.INC'
      include 'OWNTRL.INC'
      include 'OCALCS.INC'
      include 'ONAMES.INC'
      include 'OCDSPM.INC'
CC
CC**********************************************************************
CC
      splftm = xlmrgn
      spbotm = ybmrgn
CC
      lcolor = -1
      call PLTSQ(1)
      iswplt(17) = 1
CC        begin plot section
      call PLT2Q(xlmrgn,ybmrgn,3)
CC
CC        determine plot x and y scale factors
      i = 0
      if (sclxes .le. 0.0) then
        xdumy = ABS(pxmaxs - pxmins)
        if (xdumy .le. 0.0) xdumy = 1.0
        sclxes = ABS((xnchpl - xlmrgn - xrmrgn) / xdumy)
        i = 1
      end if
      if (sclyes .le. 0.0) then
        xdumy = ABS(pymaxs - pymins)
        if (xdumy .le. 0.0) xdumy = 1.0
        sclyes = ABS((ynchpl - ytmrgn - ybmrgn) / xdumy)
        i = 1
      end if
      if (i .ne. 0) then
        sclxys = SQRT(sclxes * sclxes + sclyes * sclyes)
      end if
CC
CC        set reverse sense switches
      irxndx = 0
      iryndx = 0
      if (pxmaxs .lt. pxmins) then
CC        reverse x axis
        pp = pxmaxs
        pxmaxs = pxmins
        pxmins = pp
        irxndx = 1
      end if
      if (pymaxs .lt. pymins) then
CC        reverse y axis
        pp = pymaxs
        pymaxs = pymins
        pymins = pp
        iryndx = 1
      end if
      jrxndx = 0
      jryndx = 0
CC
CC        set number of inches in plot area (xinchs,yinchs) inside
CC        margins set upper plot bounds (xrmrpg,ytmrpg)
      xinchs = xnchpl - xlmrgn - xrmrgn
      xrmrpg = xlmrgn + xinchs
      yinchs = ynchpl - ybmrgn - ytmrgn
      ytmrpg = ybmrgn + yinchs
      if (iswplt(4) .le. 1) then
CC
CC        plot axes, label coordinates
        icolor = ICLRQ(iswplt(9),0)
        if (iswplt(4) .eq. 0) call BKGDQ(xlmrgn,ybmrgn,xrmrpg,ytmrpg,1)
        icolor = ICLRQ(iswplt(16),1)
        call ILXEQ(1)
        call ILXEQ(2)
        if (iswplt(4) .eq. 0) call BKGDQ(xlmrgn,ybmrgn,xrmrpg,ytmrpg,2)
      end if
      if (htlchr .gt. 0.0) then
CC
CC        draw vertical, horizontal and caption labels
        icolor = ICLRQ(iswplt(16),1)
        call LABLQ(mlablx,nchxcr,1)
        call LABLQ(mlably,nchycr,2)
        call LABLQ(mlngnd,nchlcr,3)
      end if
      if (iswplt(8) .gt. 0.and.nxpnts .gt. 1.and.nypnts .gt. 1) then
CC
CC        plot grid coordinate points.
        iswplt(17) = 2
        ichars = 4
        icolor = ICLRQ(iswplt(16),1)
        if (ifgpol .eq. 0) then
          call PNTSQ(xcoord,ycoord,nxpnts-1,nypnts-1,1,2,2,0.0)
        else
          call PNTSQ(xcoord,ycoord,nxpnts,nypnts,1,1,1,0.0)
        end if
      end if
      iswplt(17) = 1
      if (iswplt(27) .ne. 0) then
CC
CC        indicate north
        ang = -grdang * dtorad
        xpl1p = xlmrgn - htnchr * (2.0 + spfrct) - 1.5 * (htnchr +
     *          spmins)
        if (xpl1p .lt. 0.0) xpl1p = 0.0
        ypl1p = yinchs + ybmrgn - (0.27 + htpchr) - htpchr
        call SYMBQ(xpl1p,ypl1p,htnchr,malpha(14),0.0,1)
        xpl1p = xpl1p + htnchr + spmins
        ypl1p = ypl1p + htnchr
        call PLT2Q(xpl1p,ypl1p,3)
        xpl1p = xpl1p + 0.25 * SIN(ang)
        ypl1p = ypl1p + 0.25 * COS(ang)
        call PLT2Q(xpl1p,ypl1p,2)
        call SPCLQ(xpl1p,ypl1p,htpchr,ang,7)
      end if
      if (iswplt(28) .ne. 0) then
CC        plot background map.
        iswplt(17) = 2
        call PMAPQ
      end if
      iswplt(17) = 1
      return
      end
      Subroutine LABLQ(mlabl,nchmx,iop)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - LABLQ Version 01.0                          ---
CC--- Subroutine description - plots vertical, horizontal and       ---
CC---                          caption labels.                      ---
CC---------------------------------------------------------------------
CC
CC        mlabl  - label to plot.
CC        nchmx  - maximum number of characters to scan.
CC        iop    - vertical, horizontal, caption flag.
CC                 1 = x axis axis label.
CC                 2 = y axis axis label.
CC                 3 = caption label.
CC
CC**********************************************************************
CC
      integer*2 i,j,lflg,kflg,ntot,np,nl,nc,nd,ns,nb,nsmx
CC
      character*1 mdol,mlabl,mscch
CC
      dimension mlabl(200),mscch(300)
CC
      include 'OCNTRL.INC'
      include 'OWNTRL.INC'
      include 'ONAMES.INC'
CC
      data mdol/'$'/
CC
CC**********************************************************************
CC
CC      try first to fit line within axes limits.
      lflg = 0
      xnchs = xinchs
      ynchs = yinchs
      spmn2 = 2.0 * spmins
CC      specified height of characters.
      hts = htlchr
CC      lines too long flag. 0 = no, 1 = yes.
   10 kflg = 0
CC      length of axis.
      xop0 = xinchs
CC      maximum line length.
      xop1 = xnchs
CC      maximum space (height) for label or legend lines.
      xop2 = spbotm
CC      shift distance to start of label or legend.
      xop3 = xlmrgn
      ntot = nchmx
      if (iop .eq. 2) then
        xop0 = yinchs
        xop1 = ynchs
        xop2 = splftm
        xop3 = ybmrgn
      end if
      xop1 = xop1 - 2.0 * hts
CC      input label character counter.
      np = 0
CC      lines to plot counter.
      nl = 0
CC      plot character counter.
      nc = 0
CC      number of $ end sentinels.
      nd = 0
CC      number of characters per plot line.
      ns = 0
CC      maximum number of characters per plot line.
      nsmx = 0
   20 np = np + 1
      nc = nc + 1
      if (np .gt. ntot) then
        mscch(nc) = mdol
      else
        ns = ns + 1
        mscch(nc) = mlabl(np)
      end if
      if (mscch(nc) .ne. mdol) then
        nd = 0
        if (FLOAT(ns) * hts .gt. xop1) then
          nb = nc + 1
          j = 0
   30     nb = nb - 1
          if (nb .gt. 0) then
            j = j + 1
            if (mscch(nb) .ne. mblnk1) go to 30
            mscch(nb) = mdol
            nl = nl + 1
            nsmx = MAX0(ns - j,nsmx)
            ns = j - 1
            kflg = kflg + 1
          else
            nc = nc + 1
            mscch(nc) = mdol
            nsmx = MAX0(ns,nsmx)
            ns = 0
          end if
        end if
        go to 20
      else
        nsmx = MAX0(ns,nsmx)
        nd = nd + 1
        if (nd .le. 1) then
          nl = nl + 1
          ns = 0
          go to 20
        end if
      end if
      if (nc .gt. 2) then
CC        space between axis label and legend or axis and axis label.
        st = 2.0 * hts
CC        space between lines.
        sp = spfrct * hts
CC        see if enough room.
   40   if (FLOAT(nl) * hts + sp * (FLOAT(nl) - 1.0) + st + 0.5 * hts
     *      .gt. xop2) then
CC          height of lines exceeds space available.
          i = 0
          if (sp .gt. spmins) then
CC            reduce space between lines.
            sp = sp - 0.01 * hcminc
            i = 1
            if (sp .lt. spmins) sp = spmins
          end if
          if (st .gt. spmn2) then
CC            reduce space above label.
            st = st - 0.01 * hcminc
            i = i + 1
            if (st .lt. spmn2) st = spmn2
          end if
          if (i .ne. 0) go to 40
          if (lflg .eq. 0) then
            lflg = 1
            xnchs = xnchpl
            ynchs = ynchpl
            go to 10
          end if
          if (hts .gt. hcminc) then
CC            reduce height of characters.
            hts = hts - 0.01 * hcminc
            go to 10
          end if
          if (kflg .ne. 0.and.iop .eq. 3) then
            if (nchmx .gt. 0) then
              j = 0
CC              remove all $ symbols from lines.
              do 50 i=1,nchmx
              if (mlabl(i) .eq. mdol) then
                j = j + 1
                mlabl(i) = mblnk1
              end if
   50         continue
              if (j .gt. 0) go to 10
            end if
          end if
        end if
        if (iswplt(7) .ne. 0) then
CC          left justify on centered longest line.
          xln = FLOAT(nsmx) * hts
          xy1 = xop3 + 0.5 * (xop0 - xln)
          if (xy1 + xln + 0.01 .gt. xnchpl) xy1 = xnchpl - xln - 0.01
          if (xy1 .lt. 0.0) xy1 = 0.0
        end if
        if (iop .eq. 1.or.iop .eq. 3) then
CC          x axis decrement between lines.
          xy2inc = hts + sp
CC          start y of label lines.
          xy2 = xop2 - st + sp
        else
CC          y axis increment between lines.
          xy2inc =  - (hts + sp)
CC          start x of labels lines.
          xy2 = xop2 - st - (nl - 1) * (hts + sp)
          if (xy2 .lt. hts) xy2 = hts + 0.01
        end if
        kflg = 0
        nd = 0
        np = 0
        nb = 0
   60   nb = nb + 1
        if (nb .le. nl) then
CC          plot line.
          ns = 0
CC          character counter for present line.
   70     ns = ns + 1
          if (np + ns .le. nc) then
CC            search for $ symbol at end of line.
            if (mscch(np+ns) .ne. mdol) then
              nd = 0
              go to 70
            else
CC              increment $ symbol counter, $$ is end of label.
              nd = nd + 1
CC              number of characters to plot.
              nm = ns - 1
              if (nm .gt. 0) then
                xy2 = xy2 - xy2inc
                if (iop .eq. 2.and.xy2 .lt. hts) xy2 = hts + 0.1
                if (iswplt(7) .eq. 0) then
CC                  center line.
                  xln = FLOAT(nm) * hts
                  xy1 = xop3 + 0.5 * (xop0 - xln)
                  if (xy1 + xln + 0.01 .gt. xnchpl) then
                    xy1 = xnchpl - xln - 0.01
                  end if
                  if (xy1 .lt. 0.0) xy1 = 0.0
                end if
                jrxndx = 1
                jryndx = 1
CC                switch if y axis label.
                call SWCHQ(xy1,xy2,xf1,yf1,MOD(iop,2))
                if (iop .ne. 2) then
CC                  x axis label or caption legend.
                  call SYMBQ(xf1,yf1,hts,mscch(np+1),0.0,nm)
                  if (iop .eq. 1) then
CC                    no labels may be plotted in the interval between
CC                    splftm and xlmrgn.
                    if (xf1 .lt. splftm) splftm = xf1
                  end if
CC                  no labels may be plotted in the interval between
CC                  spbotm and ybmrgn.
                  spbotm = yf1 - 0.5 * hts
                else
CC                  y axis label.
                  call SYMBQ(xf1,yf1,hts,mlabl(np+1),90.0,nm)
                  if (np .eq. 0) splftm = xf1 - hts
                end if
                kflg = 1
                jrxndx = 0
                jryndx = 0
              end if
              np = np + ns
              if (nd .le. 1) go to 60
            end if
          end if
        end if
      end if
      return
      end
      Subroutine PNTSQ(xl,yl,nx,ny,iflg,isx,isy,ang)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - PNTSQ Version 01.1                          ---
CC--- Subroutine description - control plotting of special symbols  ---
CC---                          at points.                           ---
CC---------------------------------------------------------------------
CC
CC        xl     - array of x coordinates.
CC        yl     - array of y coordinates.
CC        nx     - index of last x point.
CC        ny     - index of last y point.
CC        iflg   - plot flag.
CC                 0 = treat x and y arrays as paired points.
CC                 1 = treat x and y arrays as coordinate system. for
CC                     each y 1 to ny, each x is plotted 1 to nx.
CC        isx    - start x index.
CC        isy    - start y index.
CC        ang    - angle to plot symbol (clockwise relative to 0 degrees
CC                 north).
CC
CC**********************************************************************
CC
      integer*2 ii,jj,lflg
CC
      dimension xl(50),yl(50)
CC
      include 'OCNTRL.INC'
      include 'OWNTRL.INC'
      include 'OCDSPM.INC'
      include 'OCALCS.INC'
CC
CC**********************************************************************
CC
      ii = isx - 1
      jj = isy - 1
   10 jj = jj + 1
      if (jj .le. ny) then
        if (iflg .ne. 0) then
          ii = isx - 1
        end if
   20   ii = ii + 1
        if (ii .gt. nx) go to 10
        if (ifgpol .eq. 0) then
          xp = xl(ii)
          yp = yl(jj)
        else
          yp = yl(jj) * dtorad
          xp = xl(ii) * SIN(yp) + xorgin
          yp = xl(ii) * COS(yp) + yorgin
        end if
        lflg = 1
        if (lswxax .eq. 1) then
          if (xp .gt. 0.0) then
            xp = ALOG10(xp)
          else
            lflg = 0
          end if
        end if
        if (lflg .ne. 0) then
          xp = (xp - pxmins) * sclxes + xlmrgn
          if (lswyax .eq. 1) then
            if (yp .gt. 0.0) then
              yp = ALOG10(yp)
            else
              lflg = 0
            end if
          end if
          if (lflg .ne. 0) then
            yp = (yp - pymins) * sclyes + ybmrgn
            call SPCLQ(xp,yp,0.5*htpchr,ang,ichars)
          end if
        end if
        go to (10,20),iflg + 1
      end if
      return
      end
      Subroutine ILXEQ(iop)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - ILXEQ Version 01.1                          ---
CC--- Subroutine description - plot x and y exes.                   ---
CC---------------------------------------------------------------------
CC
CC        iop    - axis flag.
CC                 1 = x axis.
CC                 2 = y axis.
CC
CC**********************************************************************
CC
      integer*2 jop,mcl,ncl,ll,ifst,kk,ib
CC
      character*1 mnmbr,mten
CC
      dimension mnmbr(12),mten(2),lswxy(2),nincxy(2),pxymin(2),xyinc(2),
     *          xysmin(2),pxymax(2),xyscl(2),xylmrg(2),xyinch(2),
     *          splxy(2),xyrmrp(2),ndgxy(2),jrxy(2)
CC
      include 'OCNTRL.INC'
      include 'OWNTRL.INC'
CC
      equivalence (lswxax,lswxy(1)),(nincxs,nincxy(1)),
     *            (pxmins,pxymin(1)),(xincax,xyinc(1)),
     *            (xsmins,xysmin(1)),(pxmaxs,pxymax(1)),
     *            (sclxes,xyscl(1)),(xlmrgn,xylmrg(1)),
     *            (xinchs,xyinch(1)),(splftm,splxy(1)),
     *            (xrmrpg,xyrmrp(1)),(ndgxes,ndgxy(1)),
     *            (jrxndx,jrxy(1))
CC
      data mten/'1','0'/
CC
CC**********************************************************************
CC
      if (iop .eq. 1) an = 0.0
      jop = MOD(iop,2) + 1
      jrxy(jop) = 0
      call PLT2Q(xlmrgn,ybmrgn,3)
      if (lswxy(iop) .eq. 1) then
        nincxy(iop) = 10
CC        log
        nn = pxymin(iop)
        xyst = nn
        xyinc(iop) = 1.0
      else
CC        linear.
        if (xyinc(iop) .le. 0.0) then
          call STRTQ(xyinc(iop),xyst,pxymax(iop),pxymin(iop),xyscl(iop))
        else
          xyst = pxymin(iop)
        end if
        xx = 0.5 * ABS(pxymax(iop) + pxymin(iop))
        yxp = xyinc(iop) * xyscl(iop)
        if (xx .gt. 1.0.and.yxp .gt. 0.0) then
          xx = ALOG10(xx) + 2.0
          i = xx
          xx = htnchr * i
          if (nincxy(iop) .le. 0) nincxy(iop) = 10
   21     if (xx .gt. yxp*nincxy(iop)) then
            nincxy(iop) = 2 * nincxy(iop)
            go to 21
          end if
        end if
      end if
      yxp= xylmrg(jop)
      yt1 = 0.075
      yt2 = 0.05
      if (iswplt(10) .ne. 0) then
        yt1 = xyinch(jop)
      end if
      nn = 1
      if (lswxy(iop) .eq. 1) then
        if (ABS(pxymax(iop)) .gt. 9.0.or.ABS(pxymin(iop)) .gt. 9.0) then
          mcl = 2
          ncl = 4
        else
          mcl = 1
          ncl = 3
        end if
      end if
      if (iop .eq. 1) then
        t1 = spfrct * htnchr + htnchr
        t2 = spfrct * htnchr + 2.0 * htnchr
      else
        t1 = spfrct * htnchr
        t2 = spfrct * htnchr + ncl * htnchr
      end if
      do 30 ll=1,2
        ifst = 0
        if (lswxy(iop) .ne. 1) then
          xx = xyst - xyinc(iop)
        else
          kk = xyst - 1.0
          xx = 9.0
        end if
        ib = nincxy(iop) - 1
        xyp = 0.0
   10   if (xyp .lt. xyrmrp(iop)) then
          xx = xx + xyinc(iop)
          ib = ib + 1
          if (ib .gt. nincxy(iop)) then
            ib = 1
            if (lswxy(iop) .eq. 1) then
              ib = 2
              kk = kk + 1
              xx = 2.0
            end if
          end if
          if (lswxy(iop) .ne. 1) then
            xyp = xx
          else
            xyp = ALOG10(xx * 10.0**kk)
          end if
          xyp = (xyp - pxymin(iop)) * xyscl(iop)
          xyp = xyp + 1.0e-8
          if (xyp .ge. 0.0) then
            xyp = xyp + xylmrg(iop)
            if (xyp .le. xyrmrp(iop)) then
              if (iswplt(4) .eq. 0) then
                call SWCHQ(xyp,yxp,xpl1p,ypl1p,iop)
                call PLT2Q(xpl1p,ypl1p,3)
              end if
              if (ib .ge. nincxy(iop)) then
                if (iop .eq. 1) ib = 0
                if (iswplt(10) .eq. 0.or.nn .ge. 0) then
                  if (iswplt(4) .eq. 0) then
                    call SWCHQ(xyp,yxp+yt1*FLOAT(nn),xpl1p,ypl1p,iop)
                    call PLT2Q(xpl1p,ypl1p,2)
                  end if
                end if
                if (iop .eq. 2.and.ifst .eq. 0) go to 20
                if (lswxy(iop) .eq. 1) then
                  if (nn .ge. 0) then
                    xx = kk + 1
                    jrxy(jop) = 1
                    if (iop .eq. 1) then
                      xm = xyp - 0.5 * htnchr * (FLOAT(ncl) - 0.25)
                      xq = xm + 1.5 * htnchr
                      if (xx .ge. 0.0) xq = xm + 2.0 * htnchr
                      if (irxndx .ne. 0) xm = xm + htnchr * (1.25 +
     *                                        FLOAT(ncl))
                      xr2 = yxp - t2
                    else
                      xq = t2
                      xm = xyp - htnchr
                      if (iryndx .ne. 0) xm = xyp + htnchr
                      xr2 = yxp - xq
                    end if
                    call SWCHQ(xm,xr2,xpl1p,ypl1p,iop)
                    call SYMBQ(xpl1p,ypl1p,htnchr,mten,0.0,2)
                    if (iop .eq. 1) then
                      if (irxndx .eq. 0) splftm = AMIN1(splftm,xm)
                      if (irxndx .ne. 0) then
                        splftm = AMIN1(splftm,xyrmrp(iop) - xm +
     *                           xylmrg(iop))
                      end if
                      spbotm = AMIN1(spbotm,yxp - t2)
                      if (irxndx .ne. 0) xq = xq + htnchr * (FLOAT(ncl)
     *                                        - 0.25)
                      xr2 = yxp - t1
                    else
                      spbotm = AMIN1(spbotm,xyp - htnchr)
                      splftm = AMIN1(splftm,yxp - xq)
                      xm = 0.01 + mcl * htnchr
                      if (xx .lt. 0.0) xm = xm + htnchr
                      xq = xyp
                      xr2 = yxp - xm
                    end if
                    nch = IWTBF(mnmbr,12,xx,0,1)
                    call SWCHQ(xq,xr2,xpl1p,ypl1p,iop)
                    call SYMBQ(xpl1p,ypl1p,htnchr,mnmbr,0.0,nch)
                    jrxy(jop) = 0
                  end if
                  xx = 1.0
                  ib = 1
                  kk = kk + 1
                  go to 20
                end if
                if (iop .eq. 2) ib = 0
                if (nn .lt. 0) go to 20
                nch = iwtbf(mnmbr,12,xx,ndgxy(iop),1)
                jrxy(jop) = 1
                if (iop .eq. 1) then
                  xm = xyp - 0.5 * htnchr * (FLOAT(nch) - 0.25)
                  if (irxndx .ne. 0) xm = xm + htnchr * (FLOAT(nch) -
     *                                    0.25)
                  xr2 = yxp - t1
                else
                  xq = 0.5 * htnchr * (FLOAT(nch) - 0.25)
                  if (iryndx .ne. 0) xq = xq - htnchr * (FLOAT(nch) -
     *                                    0.25)
                  if (IFEQU(pxmins,0.0) .ne. 0.and.xyp-xq .lt. ybmrgn)
     *                xq = 0.0
                  if (iryndx .eq. 0) then
                    an = 90.0
                    if (splftm .lt. xlmrgn.and.xyp-xq .lt. spbotm+
     *                  htnchr) then
                      xq = xyp - spbotm - htnchr - 0.02
                    end if
                  else
                    an = -90.0
                    if (splftm .lt. xlmrgn.and.ytmrpg-xyp+xq+ybmrgn .lt.
     *                  spbotm+htnchr) then
                      xq = xyp - ytmrpg - ybmrgn + spbotm + htnchr +
     *                     0.02
                    end if
                  end if
                  xm = xyp - xq
                  xr2 = yxp - t1
                end if
                call SWCHQ(xm,xr2,xpl1p,ypl1p,iop)
                call SYMBQ(xpl1p,ypl1p,htnchr,mnmbr,an,nch)
                if (iop .eq. 1) then
                  if (irxndx .eq. 0) splftm = AMIN1(splftm,xm)
                  if (irxndx .ne. 0) then
                    splftm = AMIN1(splftm,xyrmrp(iop) - xm +
     *                       xylmrg(iop))
                  end if
                  spbotm = AMIN1(spbotm,yxp - t1)
                else
                  if (iryndx .eq. 0) spbotm = AMIN1(spbotm,xyp - xq)
                  if (iryndx .ne. 0) spbotm = AMIN1(spbotm,ytmrpg-xyp +
     *                                     xq + ybmrgn)
                  splftm = AMIN1(splftm,yxp - t1 - htnchr)
                end if
                jrxy(jop) = 0
                go to 20
              end if
              if (iswplt(4) .eq. 0) then
                call SWCHQ(xyp,yxp+yt2*FLOAT(nn),xpl1p,ypl1p,iop)
                call PLT2Q(xpl1p,ypl1p,2)
              end if
            end if
   20       ifst = 1
          end if
          go to 10
        end if
        yxp = xyrmrp(jop)
        nn = -1
   30 continue
      call PLT2Q(xlmrgn,ybmrgn,3)
      jrxy(jop) = 0
      return
      end
      Subroutine SWCHQ(xp1,yp1,xp2,yp2,iop)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - SWCHQ Version 01.0                          ---
CC--- Subroutine description - switches axes.                       ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      if (iop .eq. 1) then
        xp2 = xp1
        yp2 = yp1
      else
        xp2 = yp1
        yp2 = xp1
      end if
      return
      end
      Subroutine PMAPQ
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - PMAPQ Version 01.0                          ---
CC--- Subroutine description - plots digitized map data for OBODM.  ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      integer*2 i,j,k,ns,iend,idvc,nflg,jflg,lflgs,kflgs
CC
      character*1 malph,mdumy,mbufs
      character*2 munit
      character*80 mbufr
CC
      dimension xdumy(8),mdumy(80),malph(20),mbufs(80),xs(20),ys(20),
     *          xe(20),ye(20),x(2),y(2)
CC
      include 'OCNTRL.INC'
      include 'OWNTRL.INC'
      include 'OCDSPM.INC'
      include 'ONAMES.INC'
      include 'OCALCS.INC'                                              041806
CC
      equivalence (mbufr,mbufs(1),mnpbuf(1))
CC
      data dlng/0.05/
CC
CC**********************************************************************
CC
      ns = 0
      iend = 0
      htp = hcminc
      if (ipmode .ge. 16) then
        idvc = 1
      else
        idvc = 0
      end if
      nchlb = 0
      xpp = 0.0
      ypp = 0.0
      jflg = 0
      if (iswplt(28) .le. 1) then
        if (idvc .ne. 0) then
          icolor = ICLRQ(15,1)
        else
          icolor = ICLRQ(3,1)
        end if
        if (icolor .eq. iswplt(9)) icolor = ICLRQ(1,1)
      end if
      if (impfil .ne. 0) then
CC        read and plot digitized map data.
        impfil = IABS(impfil)
        irec = 1
        REWIND (unit=impfil,err=10)
CC        read map data title record.
   10   READ (impfil,9000,iostat=ier,err=20,end=160) mbufr
        go to 30
   20   ier = KERRS(1,mapnam,ier,irec,xdum,xdum,xdum,xdum,0)
        impfil = 0
        go to 160
CC        get distance units of map data.
   30   READ (mbufr,9001,iostat=ier,err=40) munit
        call UNITS(munit,2,iunit,jdumy,0,0,0)
        go to 50
   40   ier = KERRS(1,mapnam,ier,irec,xdum,xdum,xdum,xdum,0)
        iunit = 0
   50   jfdash = ifdash
CC
CC        go process map data.
        xp = 0.0
        yp = 0.0
   60   irec = irec + 1
CC        read map data.
        READ (impfil,9000,iostat=ier,err=80,end=70) mbufr
        go to 90
   70   iend = 1
        go to 100
   80   ier = KERRS(1,mapnam,ier,irec,xdum,xdum,xdum,xdum,0)
        go to 60
   90   imxch = 80
        imxwd = 8
        ier = IFSRD(mbufr,80,mdumy,xdumy,imxch,imxwd,-30,rmissd,0,      041806
     *             rmissd) 
        if (ier .eq. 0) then                                            030797
CC          blank record.
          go to 60
        else if (ier .lt. 0) then                                       030797
CC          read error.
          ier = KERRS(1,mapnam,IABS(ier),irec,xdum,xdum,xdum,xdum,0)    030797
          go to 60
        end if
  100   if (imxch .gt. 0.or.iend .ne. 0) then
CC          code, label record.
          ipen = 3
CC          if old label.
          nchs = nchlb
          if (nchlb .gt. 0.and.xpp .gt. 0.0) then
            nflg = 1
            x(1) = xpp
            x(2) = xpp + nchs * htp
            y(1) = ypp
            y(2) = ypp + htp
            if (x(2) .lt. xlmrgn.or.x(1) .gt. xrmrpg) then
              nflg = 0
            end if
            if (y(2) .lt. ybmrgn.or.y(1) .gt. ytmrpg) then
              nflg = 0
            end if
            if (nflg .gt. 0) then
              if (ns .gt. 0) then
                do 130 i=1,ns
                  do 120 j=1,2
                    do 110 k=1,2
                      if (xs(i) .lt. x(j).and.x(j) .lt. xe(i)) then
                        if (ys(i) .lt. y(k).and.y(k) .lt. ye(i)) then
                          nflg = 0
                        end if
                      end if
  110               continue
  120             continue
                  if (nflg .gt. 0) then
                    if (x(1) .lt. xs(i).and.xs(i) .lt. x(2)) then
                      if ((y(1) .lt. ys(i).and.ys(i) .lt. y(2)) .or.
     *                    (y(1) .lt. ye(i).and.ye(i) .lt. y(2))) then
                        nflg = 0
                      end if
                    else if (x(1) .lt. xe(i).and.xe(i) .lt. x(2)) then
                      if ((y(1) .lt. ys(i).and.ys(i) .lt. y(2)) .or.
     *                    (y(1) .lt. ye(i).and.ye(i) .lt. y(2))) then
                        nflg = 0
                      end if
                    end if
                  end if
  130           continue
              end if
            end if
            if (nflg .gt. 0.and.ns .lt. 20) then
              ns = ns + 1
              xs(ns) = x(1)
              xe(ns) = x(2)
              ys(ns) = y(1)
              ye(ns) = y(2)
            end if
            if (nflg .gt. 0) then
              call SYMBQ(xpp,ypp,htp,malph,0.0,nchs)
            end if
            jflg = jflg + 1
          end if
          if (iend .ne. 0) go to 150
CC          check for new label.
          nchlb = 0
          xpp = 0.0
          kflgs = xdumy(1)
          lflgs = IABS(kflgs)
          if (lflgs .ge. 1.and.lflgs .le. 5) then
            if (kflgs .gt. 0) then
              i = 1
            else
              i = 2
            end if
            nchlb = NUMC1(mbufs(i+2),20)
            call MVCH1(mbufs(i+2),malph,nchlb)
            if (lflgs .eq. 1) then
CC              geographic boundary.
              ifdash = 1
CC              color black/white or same as axes.
              if (iswplt(28) .eq. 2) then
                if (idvc .ne. 0) then
                  icolor = ICLRQ(iswplt(16),1)
                else
                  icolor = ICLRQ(iswplt(13),1)
                end if
              end if
CC              do not label.
              nchlb = 0
            else if (lflgs .eq. 2) then
CC              political boundary areas.
              ifdash = 4
CC              color cyan.
              if (iswplt(28) .eq. 2) then
                if (idvc .ne. 0) then
                  icolor = ICLRQ(11,1)
                else
                  icolor = ICLRQ(iswplt(14),1)
                end if
              end if
            else if (lflgs .eq. 3) then
CC              roads.
              ifdash = 0
CC              color magenta.
              if (iswplt(28) .eq. 2) then
                if (idvc .ne. 0) then
                  icolor = ICLRQ(13,1)
                else
                  icolor = ICLRQ(iswplt(15),1)
                end if
              end if
CC              do not label.
              nchlb = 0
            else if (lflgs .eq. 4) then
CC              sites.
              ifdash = 0
CC              color green-yellow.
              if (iswplt(28) .eq. 2) then
                if (idvc .ne. 0) then
                  icolor = ICLRQ(10,1)
                else
                  icolor = ICLRQ(iswplt(13),1)
                end if
              end if
            else if (lflgs .eq. 5) then
CC              airports.
              ifdash = 5
CC              color blue-magenta.
              if (iswplt(28) .eq. 2) then
                if (idvc .ne. 0) then
                  icolor = ICLRQ(9,1)
                else
                  icolor = ICLRQ(iswplt(14),1)
                end if
              end if
            end if
          end if
        else if (imxwd .gt. 1.and.xdumy(1) .gt. rmissd.and.             041806
     *           xdumy(2) .gt. rmissd) then                             041806
CC          x,y pair record.
          i = -1
  140     i = i + 2
          if (i .ge. imxwd) go to 60
CC          calculate plot point. check to see if a label is pending.
CC          if yes, see if label start point already saved in xpp,ypp.
CC          if not, save label point in xpp,ypp.
          xdumy(i) = xdumy(i) * constl(iunit+1) / constl(igunit+1)      041806
          xdumy(i+1) = xdumy(i+1) * constl(iunit+1) /constl(igunit+1)   041806
          xp = (xdumy(i) - pxmins) * sclxes + xlmrgn
          yp = (xdumy(i+1) - pymins) * sclyes + ybmrgn
          if (nchlb .gt. 0.and.xpp .le. 0.0) then
            ypp = yp + 0.02
            if (kflgs .lt. 0) then
              xpp = xp - nchlb * htp - 0.02
            else
              xpp = xp + 0.02
            end if
            if (xpp .lt. xlmrgn.and.xpp+nchlb*htp .gt. xlmrgn) then
              xpp = xlmrgn + 0.02
            end if
            if (ypp .lt. ybmrgn.and.ypp+htp .gt. ybmrgn) then
              ypp = ybmrgn + 0.02
            end if
            if (xpp .lt. xrmrpg.and.xpp+nchlb*htp .gt. xrmrpg) then
              xpp = xrmrpg - 0.02 - nchlb * htp
            end if
            if (ypp .lt. ytmrpg.and.ypp+htp .gt. ytmrpg) then
              ypp = ytmrpg - 0.02 - htp
            end if
          end if
          call PLT1Q(xp,yp,ipen,dlng)
          jflg = jflg + 1
          ipen = 2
          go to 140
        end if
        go to 60
  150   if (jflg .gt. 0) call PLT1Q(xp,yp,3,dlng)
      end if
  160 return
CC
 9000 format (a80)
 9001 format (52x,a2)
      end
