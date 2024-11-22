      Subroutine RPLTG
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - RPLTG Version 01.2                          ---
CC--- Subroutine description - plot functy as a function of functx  ---
CC---                          or plot the line of surface maximum  ---
CC---                          height or magnitude.                 ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      dimension as(400),bs(400),cs(400),ds(400),xf(7),yf(7),istrt(200),
     *          iend(200),xk(400),yk(400),ispac(620),ztran(10000)
CC
      include 'OCNTRL.INC'
      include 'OWNTRL.INC'
      include 'OCDSPM.INC'
      include 'OCALCS.INC'
CC
      equivalence (ispac(1),ilinem(1)),(istrt(1),ispac(1)),
     *            (iend(1),ispac(201)),(xf(1),ispac(401)),
     *            (yf(1),ispac(415)),(ztran(1),zcoord(1)),
     *            (calcsa(10001),as(1)),(calcsa(10401),bs(1)),
     *            (calcsa(10801),cs(1)),(calcsa(11201),ds(1)),
     *            (calcsa(11601),xk(1)),(calcsa(12001),yk(1))
CC
CC**********************************************************************
CC
      isave = iswplt(3)
      iswplt(17) = 2
      if (ipoptn(jpoptn) .eq. 1.or.ipoptn(jpoptn) .eq. 2) then
        icolor = ICLRQ(iswplt(12),1)
        ifdash = iswplt(6)
      else
        icolor = ICLRQ(iswplt(23),1)
        ifdash = iswplt(22)
      end if
      dlng = 0.05
      if (IANYI(ipoptn,npoptn,3) .ne. 0.or.IANYI(ipoptn,npoptn,4) .ne.
     *    0) dlng = 0.25
CC
CC        y as a function of x being plotted.
      if (ipoptn(jpoptn) .eq. 1) then
        jfpol = 0
        mm = MIN0(nxyfnc,400)
        i = JRNIT(xk,mm,functx,1)
        i = JRNIT(yk,mm,functy,1)
        ncv = 1
        istrt(ncv) = 1
        iend(ncv) = mm
      else
CC        grid system, must find line of maximum surface height.
        xmax = rmissd
        ymax = rmissd
        if (icontr .eq. 1) then
          jfpol = iftpol
          iswplt(3) = 1
          ix = 0
          i = 0
   10     if (i .lt. nxtran.and.ix .eq. 0) then
            i = i + 1
            if (xteran(i) .ge. pxmins) then
              ix = MAX0(i-1,1)
            end if
            go to 10
          end if
          nx = 0
          i = 0
   20     if (i .lt. nxtran.and.nx .eq. 0) then
            i = i + 1
            if (xteran(i) .gt. pxmaxs) then
              nx = i
            end if
            go to 20
          end if
          iy = 0
          i = 0
   30     if (i .lt. nytran.and.iy .eq. 0) then
            i = i + 1
            if (yteran(i) .ge. pymins) then
              iy = MAX0(i-1,1)
            end if
            go to 30
          end if
          ny = 0
          i = 0
   40     if (i .lt. nytran.and.ny .eq. 0) then
            i = i + 1
            if (yteran(i) .gt. pymaxs) then
              ny = i
            end if
            go to 40
          end if
          if (nx .le. 0) nx = nxtran
          if (ny .le. 0) ny = nytran
          nxs = nxtran
          nys = nytran
          if (ny-iy+1 .gt. 1) then
            do 50 j=iy+1,ny
              ymax = AMAX1(ymax, yteran(j) - yteran(j-1))
   50       continue
          end if
          if (nx-ix+1 .gt. 1) then
            do 60 i=ix+1,nx
              xmax = AMAX1(xmax, xteran(i) - xteran(i-1))
   60       continue
          end if
        else
          jfpol = ifgpol
          nx = nxpnts
          ny = nypnts
          nys = nypnts
          nxs = nxpnts
          ix = 1
          iy = 1
          if (ny-iy+1 .gt. 1) then
            do 70 j=iy+1,ny
              ymax = AMAX1(ymax, ycoord(j) - ycoord(j-1))
   70       continue
          end if
          if (nx-ix+1 .gt. 1) then
            do 80 i=ix+1,nx
              xmax = AMAX1(xmax, xcoord(i) - xcoord(i-1))
   80       continue
          end if
        end if
        mm = 0
        dr = 0.001 / sclxys
        ymx = ANTRU(wnddir-grdang)
        if ((ymx .ge. 0.0.and.ymx .le. 45.0).or.
     *      (ymx .ge. 315.0.and.ymx .le. 360.0).or.
     *      (ymx .ge. 135.0.and.ymx .le. 225.0)) then
          indx = 1
        else
          indx = 2
        end if
        if (icontr .eq. 2.and.indx .eq. 1) go to 170
CC        loop over x.
        do 160 ii=ix,nx
CC          find index at or close to maximum
          ymx = rmissd
          kk = 0
          lflg = 0
CC          loop over y
          do 90 jj=iy,ny
            ln = (jj-1)*nxs
            if (icontr .eq. 1) then
              czmax = ztran(ln+ii+joffs1)
            else
              czmax = calcsa(ln+ii+joffs1)
            end if
            if (jj .gt. 1) then
CC            if surface is not flat set lflg
              if (ABS(czmxl-czmax) .gt. 0.0) lflg = 1
            end if
            if (czmax .gt. ymx) then
CC            set index at maximum.
            kk = jj
CC          save maximum surface height.
            ymx = czmax
            end if
            czmxl = czmax
   90     continue
          if (lflg .gt. 0.and.kk .gt. 0) then
CC            y at maximum.
            if (icontr .eq. 1) then
              ypt = yteran(kk)
            else
              ypt = ycoord(kk)
            end if
CC            -1, +1 points.
            i1 = MAX0(kk - 1,1)
            i2 = MIN0(kk + 1,nys)
            ln = i1 - 1
            nm = 0
  100       if (ln .lt. i2) then
              ln = ln + 1
              nm = nm + 1
              if (icontr .eq. 1) then
                xf(nm) = yteran(ln)
              else
                xf(nm) = ycoord(ln)
              end if
              mn = (ln - 1) * nxs
CC              go to log world to help insure stable solution.
              if (icontr .eq. 1) then
                yf(nm) = ztran(mn+ii+joffs1)
              else
                yf(nm) = calcsa(mn+ii+joffs1)
              end if
              go to 100
            end if
            if (nm .gt. 1) then
CC              xf must be in ascending order.
              do 120 l2=2,nm
                do 110 l1=2,nm
                  if (xf(l1) .lt. xf(l1-1)) then
                    xpt = xf(l1)
                    xf(l1) = xf(l1-1)
                    xf(l1-1) = xpt
                    xpt = yf(l1)
                    yf(l1) = yf(l1-1)
                    yf(l1-1) = xpt
                  end if
  110           continue
  120         continue
              if (dr .le. 0.0) dr = 0.01 * (xf(2) - xf(1))
CC              get spline coefficients around maximum.
              call SPLNQ(xf,yf,as,bs,cs,ds,nm,jferr)
              if (jferr .eq. 0) then
                jj = 1
                ymx = rmissd
CC                find maximum.
                xpc = xf(jj) - dr
  130           xpc = xpc + dr
  140           if (xpc .ge. xf(jj+1)) then
                  jj = jj + 1
                  if (jj .lt. nm) go to 140
                end if
                if (jj .lt. nm) then
                  ypc = xpc - xf(jj)
                  ypc = yf(jj) + ypc * (bs(jj) + (xpc - xf(jj+1)) *
     *                  (2.0 * cs(jj) + cs(jj+1) + as(jj) * ypc) *
     *                  0.16666667)
                  if (ypc .gt. ymx) then
                    ypt = xpc
                    ymx = ypc
                  end if
                  go to 130
                end if
              else
                ymx = rmissd
                ypt = xf(1)
                do 150 jj=1,nm
                  if (yf(jj) .gt. rmissd) then
                    ymx = yf(jj)
                    ypt = xf(jj)
                  end if
  150           continue
              end if
            else
              ypt = xf(1)
            end if
            mm = mm + 1
            if (mm .le. 400) then
CC              save x,y location of maximum.
              if (icontr .eq. 1) then
                xk(mm) = xteran(ii)
              else
                xk(mm) = xcoord(ii)
              end if
              yk(mm) = ypt
            end if
          end if
  160   continue
        if (icontr .eq. 2.and.indx .eq. 2) go to 260
CC        loop over y.
  170   do 250 jj=iy,ny
CC          find index at or close to maximum.
          ymx = rmissd
          lflg = 0
CC          loop over x.
          ln = (jj - 1) * nxs
          do 180 ii=ix,nx
            if (icontr .eq. 1) then
              czmax = ztran(ln+ii+joffs1)
            else
              czmax = calcsa(ln+ii+joffs1)
            end if
            if (ii .gt. 1) then
CC              if surface is not flat set lflg.
              if (ABS(czmxl-czmax) .gt. 0.0) lflg = 1
            end if
            if (czmax .gt. ymx) then
CC              set index at maximum.
              kk = ii
CC              save maximum surface height.
              ymx = czmax
            end if
            czmxl = czmax
  180     continue
          if (lflg .gt. 0) then
CC            x at maximum.
            if (icontr .eq. 1) then
              ypt = xteran(kk)
            else
              ypt = xcoord(kk)
            end if
CC            -1, +1 points.
            i1 = MAX0(kk - 1,1)
            i2 = MIN0(kk + 1,nxs)
            mn = i1 - 1
            nm = 0
  190       if (mn .lt. i2) then
              mn = mn + 1
              nm = nm + 1
              if (icontr .eq. 1) then
                xf(nm) = xteran(mn)
              else
                xf(nm) = xcoord(mn)
              end if
CC              go to log world to help insure stable solution.
              if (icontr .eq. 1) then
                yf(nm) = ztran(ln+mn+joffs1)
              else
                yf(nm) = calcsa(ln+mn+joffs1)
              end if
              go to 190
            end if
            if (nm .gt. 1) then
CC              xf must be in ascending order.
              do 210 l2=2,nm
                do 200 l1=2,nm
                  if (xf(l1) .lt. xf(l1-1)) then
                    xpt = xf(l1)
                    xf(l1) = xf(l1-1)
                    xf(l1-1) = xpt
                    xpt = yf(l1)
                    yf(l1) = yf(l1-1)
                    yf(l1-1) = xpt
                  end if
  200           continue
  210         continue
CC              get spline coefficients around maximum.
              call SPLNQ(xf,yf,as,bs,cs,ds,nm,jferr)
              if (jferr .eq. 0) then
                ii = 1
                ymx = rmissd
CC                find maximum.
                xpc = xf(ii) - dr
  220           xpc = xpc + dr
  230           if (xpc .ge. xf(ii+1)) then
                  ii = ii + 1
                  if (ii .lt. nm) go to 230
                end if
                if (ii .lt. nm) then
                  ypc = xpc - xf(ii)
                  ypc = yf(ii) + ypc * (bs(ii) + (xpc - xf(ii+1)) *
     *                  (2.0 * cs(ii) + cs(ii+1) + as(ii) * ypc) *
     *                  0.16666667)
                  if (ypc .le. ymx) go to 220
                  ypt = xpc
                  ymx = ypc
                  go to 220
                end if
              else
                ymx = rmissd
                ypt = xf(1)
                do 240 ii=1,nm
                  if (yf(ii) .gt. rmissd) then
                    ymx = yf(ii)
                    ypt = xf(ii)
                  end if
  240           continue
              end if
            else
              ypt = xf(1)
            end if
            mm = mm + 1
            if (mm .le. 400) then
CC              save x,y location of maximum.
              if (icontr .eq. 1) then
                yk(mm) = yteran(jj)
              else
                yk(mm) = ycoord(jj)
              end if
              xk(mm) = ypt
            end if
          end if
  250   continue
  260   ncv = 0
        if (mm .gt. 0) then
          if (mm .le. 2) then
            ncv = 1
            istrt(1) = 1
            iend(1) = mm
          else
            ncv = 0
            ii = 0
            n = 0
  270       ii = ii + 1
            if (xk(ii) .gt. rmissd.and.yk(ii) .gt. rmissd) then
              n = n + 1
              l1 = n
              l = l1
              as(n) = xk(ii)
              bs(n) = yk(ii)
              xk(ii) = rmissd
              yk(ii) = rmissd
  280         do 290 jj=ii+1,mm
                if (xk(jj) .gt. rmissd.and.yk(jj) .gt. rmissd) then
                  if (ABS(as(l) - xk(jj)) .le. xmax.and.
     *                ABS(bs(l) - yk(jj)) .le. ymax) then
                    n = n + 1
                    as(n) = xk(jj)
                    bs(n) = yk(jj)
                    xk(jj) = rmissd
                    yk(jj) = rmissd
                  end if
                end if
  290         continue
              l2 = n
              if (l2 .gt. l) then
                l = l + 1
                go to 280
              end if
              if (l2 .eq. l1) then
                l2 = l2 + 1
                as(l2) = as(l1)
                bs(l2) = bs(l1)
                n = n + 1
              end if
              ncv = ncv + 1
              istrt(ncv) = l1
              iend(ncv) = l2
            end if
            if (ii .lt. mm-1) go to 270
            do 300 ii=1,iend(ncv)
              xk(ii) = as(ii)
              yk(ii) = bs(ii)
  300       continue
          end if
        end if
      end if
      if (ncv .gt. 0) then
CC        process locations of maximum.
        do 350 ll=1,ncv
          l1 = istrt(ll)
          l2 = iend(ll)
          if (l2 .gt. l1) then
            if (l2-l1+1 .gt. 2) then
              do 320 jj=l1,l2-1
                do 310 ii=l1,l2-1
                  if (xk(ii) .gt. xk(ii+1)) then
                    xpc = xk(ii)
                    xk(ii) = xk(ii+1)
                    xk(ii+1) = xpc
                    xpc = yk(ii)
                    yk(ii) = yk(ii+1)
                    yk(ii+1) = xpc
                  end if
  310           continue
  320         continue
            end if
            if (ipoptn(jpoptn) .ne. 1.and.jfpol .ne. 0) then
              do 330 i=l1,l2
                xpc = yk(i) * dtorad
                yk(i) = xk(i) * COS(xpc) + yorgin
                xk(i) = xk(i) * SIN(xpc) + xorgin
  330         continue
            end if
            if (l2 .gt. l1) then
              n = l1
              do 340 ii=l1+1,l2
                if (ABS(xk(ii)-xk(n)) .gt. 0.1) then
                  n = n + 1
                  xk(n) = xk(ii)
                  yk(n) = yk(ii)
                end if
  340         continue
              l2 = n
            end if
            call CURVQ(xk(l1),yk(l1),as,bs,cs,ds,l2-l1+1,dlng,1)
          end if
  350   continue
      end if
      iswplt(3) = isave
      return
      end
      Subroutine CURVQ(xl,yl,a,b,c,d,nnpts,dlng,jsw)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - CURVQ Version 01.0                          ---
CC--- Subroutine description - controls plotting of curves x vs. y  ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      integer*2 ii,jj,ksw,iflg
CC
      dimension xl(400),yl(400),a(400),b(400),c(400),d(400)
CC
      include 'OCNTRL.INC'
      include 'OWNTRL.INC'
CC
CC**********************************************************************
CC
      lsw = 3
      ksw = 1
      nn = nnpts
      if (lswxax .ne. 0.or.lswyax .ne. 0) then
        nn = 0
        do 10 ii=1,nnpts
          iflg = 1
          if (lswxax .eq. 1) then
            if (xl(ii) .gt. 0.0) then
              xl(ii) = ALOG10(xl(ii))
            else
              iflg = 0
            end if
          end if
          if (iflg .ne. 0) then
            if (lswyax .eq. 1) then
              if (yl(ii) .gt. 0.0) then
                yl(ii) = ALOG10(yl(ii))
              else
                iflg = 0
              end if
            end if
            if (iflg .ne. 0) then
              nn = nn + 1
              if (ii .ne. nn) then
                xl(nn) = xl(ii)
                yl(nn) = yl(ii)
              end if
            end if
          end if
   10   continue
      end if
      jj = 0
      if (iswplt(3) .ne. 0.or.nn .eq. 1) go to 40
      nn = MIN0(nn,400)
      dlr = 0.001 / sclxes
      call SPLNQ(xl,yl,a,b,c,d,nn,jferr)
      if (jferr .ne. 0) go to 40
      jj = 1
      ksw = 2
      xf = xl(1) - dlr
   20 xf = xf + dlr
   30 if (xf .ge. xl(jj+1)) then
        jj = jj + 1
        if (jj .lt. nn) go to 30
      end if
      if (jj .ge. nn) go to 60
      yf = xf - xl(jj)
      yf = yl(jj) + yf * (b(jj) + (xf - xl(jj+1)) * (2.0 * c(jj) +
     *     c(jj+1) + a(jj) * yf) * 0.1666667)
      go to 50
   40 jj = jj + 1
      if (jj .gt. nn) go to 60
      xf = xl(jj)
      yf = yl(jj)
   50 if (jsw .ne. 2) then
        xp = (xf - pxmins) * sclxes + xlmrgn
        yp = (yf - pymins) * sclyes + ybmrgn
      else
        xp = (yf - pxmins) * sclxes + xlmrgn
        yp = (xf - pymins) * sclyes + ybmrgn
      end if
      call PLT1Q(xp,yp,lsw,dlng)
      lsw = 2
      if (nn .eq. 1) call PLT1Q(xp,yp,lsw,dlng)
      go to (40,20),ksw
   60 if (lsw .eq. 2) call PLT1Q(xp,yp,3,dlng)
      return
      end
      Subroutine SPLNQ(x,y,a,b,c,d,nn,iers)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - SPLNQ Version 01.0                          ---
CC--- Subroutine description - cubic spline curve fit routine.      ---
CC---------------------------------------------------------------------
CC
CC        x      - x axis array.
CC        y      - y axis array.
CC        a,b,c,d- spline coefficients.
CC        nn     - number of points.
CC        iers    - error return flag.
CC                 0 = no errors.
CC                 1 = error, cannot fit.
CC
CC**********************************************************************
CC
      integer*2 np,ii,ntm
CC
      dimension x(400),y(400),a(400),b(400),c(400),d(400)
CC
CC**********************************************************************
CC
      iers = 1
      if (nn .gt. 2) then
        c(1) = 0.0
        c(nn) = 0.0
        q = 1.07179677
CC          q = 4.0 * (2.0 - SQRT(3.0))
        np = nn - 1
        do 10 ii=1,np
          a(ii) = x(ii+1) - x(ii)
          b(ii) = (y(ii+1) - y(ii)) / a(ii)
          if (ii .ge. 2) then
            c(ii) = 2.0 * (b(ii) - b(ii-1)) / (a(ii-1) + a(ii))
CC            d(ii) = c(ii) * 3.0 / 2.0
            d(ii) = c(ii) * 1.5
          end if
   10   continue
        ntm = 0
        xm = 1.0
   20   if (ntm .lt. 80.and.xm .ge. 1.0e-3) then
          xm = 0.0
          ntm = ntm + 1
          do 30 ii=2,np
            yp = c(ii+1)
            yp = q * (((yp - c(ii-1)) / (1.0 + a(ii) / a(ii-1)) - yp) *
     *           0.5 - c(ii) + d(ii))
            if (ABS(yp) .gt. xm) xm = ABS(yp)
            c(ii) = c(ii) + yp
   30     continue
          go to 20
        end if
        if (ntm .lt. 80) then
          do 40 ii=1,np
            a(ii) = (c(ii+1) - c(ii)) / a(ii)
   40     continue
          iers = 0
        else
          iers = 1
        end if
      end if
      return
      end
