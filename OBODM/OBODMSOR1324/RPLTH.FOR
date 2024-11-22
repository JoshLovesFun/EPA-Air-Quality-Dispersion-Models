      Subroutine RPLTH
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - RPLTH Version 01.2                          ---
CC--- Subroutine description - draws ncontr(icontr) contours of     ---
CC---                          values (contrs(i,icontr),i=1,        ---
CC---                          ncontr(icontr)) and labels them. the ---
CC---                          interpolation method is taken from   ---
CC---                          Algorithm 474, 'Bivariate Interpola- ---
CC---                          tion and Smooth Surface Fitting Based---
CC---                          on Local Procedures', by Hiroshi     ---
CC---                          Akima, Communications Of The ACM,    ---
CC---                          Jan. 1974, p.26. the method is based ---
CC---                          on a piece-wise function composed of ---
CC---                          a set of bicubic polynomials in x and---
CC---                          y. each polynomial is applicable to a---
CC---                          rectangle of the input grid in the   ---
CC---                          x-y plane. each polynomial is        ---
CC---                          determined locally. each grid cell   ---
CC---                          determined by adjacent pairs of x and---
CC---                          y values is subdivided into from 5 to---
CC---                          10 subcells, depending on the size of---
CC---                          the original cell (actual plot inches)--
CC---                          in an attempt to keep the subcell    ---
CC---                          size about 0.05 inches on a side.    ---
CC---                          where this requirement would create  ---
CC---                          more than 10 or fewer than 5 subcells---
CC---                          the values 10 and 5, respectively, are--
CC---                          used. the interpolation scheme       ---
CC---                          referenced above is the applied, and ---
CC---                          linear interpolation is used between ---
CC---                          the corners of each subcell to find  ---
CC---                          points which lie on contour lines.   ---
CC---                          straight lines are drawn between two ---
CC---                          such points in a subcell.            ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      character*14 mlabl
CC
      dimension zz(11,11),ifl(10,10),u(11),v(11),za(4,2),zb(5),zab(2,3),
     *          zx(2),zy(2),zxy(2),ztran(10000)
CC
      include 'OCNTRL.INC'
      include 'OWNTRL.INC'
      include 'OCALCS.INC'
      include 'ONAMES.INC'
CC
      equivalence (mlabl,mnpbuf(1)),(z3a2,za(1,1)),(z3a3,za(2,1)),
     *            (z3a4,za(3,1)),(z3a5,za(4,1)),(z4a2,za(1,2)),
     *            (z4a3,za(2,2)),(z4a4,za(3,2)),(z4a5,za(4,2)),
     *            (z4b1,zb(1)),(z4b2,zb(2)),(z4b3,zb(3)),(z4b4,zb(4)),
     *            (z4b5,zb(5)),(za3b2,zab(1,1)),(za4b2,zab(2,1)),
     *            (za3b3,zab(1,2)),(za4b3,zab(2,2)),(za3b4,zab(1,3)),
     *            (za4b4,zab(2,3)),(zx43,zx(1)),(zx44,zx(2)),
     *            (zy43,zy(1)),(zy44,zy(2)),(zxy43,zxy(1)),
     *            (zxy44,zxy(2)),(p00,z33),(p01,zy33),(p10,zx33),
     *            (p11,zxy33),(sw,fe),(w2,wy2,fa,q00),(w3,wy3,fb,q10),
     *            (wx2,fc,q20),(wx3,fd,q30),(z3a2,p02),(z4a2,p03),
     *            (z4b1,p12),(z4b2,p13),(z4b4,p20),(z4b5,p21),
     *            (za3b2,p22),(za3b4,p23),(ztran(1),zcoord(1))
CC
CC**********************************************************************
CC
      if (icontr .eq. 1) then
        ix = 0
        i = 0
   10   if (i .lt. nxtran.and.ix .eq. 0) then
          i = i + 1
          if (xteran(i) .ge. pxmins) then
            ix = MAX0(i-1,1)
          end if
          go to 10
        end if
        nx = 0
        i = 0
   20   if (i .lt. nxtran.and.nx .eq. 0) then
          i = i + 1
          if (xteran(i) .gt. pxmaxs) then
            nx = i
          end if
          go to 20
        end if
        iy = 0
        i = 0
   30   if (i .lt. nytran.and.iy .eq. 0) then
          i = i + 1
          if (yteran(i) .ge. pymins) then
            iy = MAX0(i-1,1)
          end if
          go to 30
        end if
        ny = 0
        i = 0
   40   if (i .lt. nytran.and.ny .eq. 0) then
          i = i + 1
          if (yteran(i) .gt. pymaxs) then
            ny = i
          end if
          go to 40
        end if
        if (nx .le. 0) nx = nxtran
        if (ny .le. 0) ny = nytran
        nxs = nxtran
        jfpol = iftpol
      else
        nx = nxpnts
        ny = nypnts
        nxs = nxpnts
        ix = 1
        iy = 1
        jfpol = ifgpol
      end if
      iswplt(17) = 2
      if (IABS(ipoptn(jpoptn)) .eq. 4) then
        do 60 js=iy,ny
          i1 = (js - 1) * nxs
          do 50 is=ix,nx
            if (icontr .eq. 1) then
              xx1 = (ztran(is+i1+joffs1) + zmincs(icontr)) *
     *              zminds(icontr)
            else
              xx1 = (calcsa(is+i1+joffs1) + zmincs(icontr)) *
     *              zminds(icontr)
            end if
            if (xx1 .lt. 1.0) xx1 = 1.0
            if (icontr .eq. 1) then
              ztran(is+i1+joffs1) = ALOG(xx1)
            else
              calcsa(is+i1+joffs1) = ALOG(xx1)
            end if
   50     continue
   60   continue
      end if
      lym2 = ny - 2
      lxm2 = nx - 2
      nctlbl = 0
      nypnp = ny
      if (jfpol .ne. 0) then
        yy1 = 0.0
        do 70 i=iy+1,ny
          yy2 = ABS(ycoord(i) - ycoord(i-1))
          if (yy2 .gt. yy1) yy1 = yy2
   70   continue
        yy2 = DIFNQ(jfpol,0,ycoord(ny),ycoord(iy))
        if (ABS(yy2) .le. yy1+1.0) then
          if (ny+1 .le. nmxxyz) then
            nypnp = ny + 1
            ycoord(nypnp) = ycoord(iy)
          end if
        end if
      end if
      if (icontr .eq. 1) then
        call CMXMN(ix,iy,nxtran,nx,ny,ztran(joffs1+1),
     *             csmax,csmin,idumy,idumy)
      else
        call CMXMN(ix,iy,nxpnts,nx,ny,calcsa(joffs1+1),
     *             csmax,csmin,idumy,idumy)
      end if
CC
CC        loop over contour levels
      do 400 ks=1,ncontr(icontr)
        ncurvs = 0
        c = contrs(ks,icontr)
        if (IABS(ipoptn(jpoptn)) .eq. 4) then
          c = ALOG((c + zmincs(icontr)) * zminds(icontr))
        end if
        
        if (c .lt. csmin.or.c .gt. csmax) go to 400
CC
CC          main do loop over input data grid
        do 390 jj=iy+1,nypnp
CC
CC          determine number of subdivisions in y direction
          if (jfpol .eq. 0) then
            nsuby = 6
            if (icontr .eq. 1) then
              dv1 = (yteran(jj) - yteran(jj-1)) * 0.2
            else
              dv1 = (ycoord(jj) - ycoord(jj-1)) * 0.2
            end if
            dv = dv1 * sclyes
            if (dv .gt. 0.05) then
              nsuby = dv * 100.0 + 1.5
              if (nsuby .gt. 11) nsuby = 11
              if (icontr .eq. 1) then
                dv1 = (yteran(jj) - yteran(jj-1)) / (nsuby - 1)
              else
                dv1 = (ycoord(jj) - ycoord(jj-1)) / (nsuby - 1)
              end if
              dv = dv1 * sclyes
            end if
            nsby1 = nsuby - 1
            if (icontr .eq. 1) then
              yjj1 = yteran(jj-1) * sclyes
            else
              yjj1 = ycoord(jj-1) * sclyes
            end if
          else
            nsuby = 11
            dv1 = DIFNQ(jfpol,0,ycoord(jj),ycoord(jj-1))
            dv1 = ABS(dv1) / (nsuby-1)
            dv = dv1
            nsby1 = nsuby-1
            yjj1 = ycoord(jj-1)
          end if
          do 80 js=1,nsuby
            v(js) = yjj1 + (js - 1) * dv
   80     continue
          iym2 = jj - 2
          iym3 = iym2 - 1
          iyml = jj - nypnp
          iyml1 = iyml + 1
          ix6 = ix - 1
          jj2 = jj + 2
          jj1 = jj + 1
          if (jj2 .gt. nypnp) jj2 = jj2 - nypnp
          if (jj1 .gt. nypnp) jj1 = jj1 - nypnp
          if (iyml1 .lt. iy-1) then
            if (icontr .eq. 1) then
              bb6 = DIFNQ(0,1,yteran(jj2),yteran(jj1))
            else
              bb6 = DIFNQ(jfpol,1,ycoord(jj2),ycoord(jj1))
            end if
          end if
          if (jj .gt. ny) then
            jn = iy
          else
            jn = jj
          end if
          if (jj1 .gt. ny) then
            jn1 = iy
          else
            jn1 = jj1
          end if
          if (jj2 .gt. ny) then
            jn2 = iy
          else
            jn2 = jj2
          end if
CC
CC          main do loop over input data grid.
CC
          do 380 ii=ix,nx
            ixml = ii - nx
            ixm1 = ii - 1
            if (ixm1 .ge. ix) then
CC              determine number of subdivisions in x direction.
              nsubx = 6
              if (icontr .eq. 1) then
                du1 = (xteran(ii) - xteran(ii-1)) * 0.2
              else
                du1 = (xcoord(ii) - xcoord(ii-1)) * 0.2
              end if
              du = du1 * sclxes
              if (du .gt. 0.05) then
                nsubx = du * 100.0 + 1.5
                if (nsubx .gt. 11) nsubx = 11
                if (icontr .eq. 1) then
                  du1 = (xteran(ii) - xteran(ii-1)) / (nsubx - 1)
                else
                  du1 = (xcoord(ii) - xcoord(ii-1)) / (nsubx - 1)
                end if
                du = du1 * sclxes
              end if
              nsbx1 = nsubx - 1
              if (icontr .eq. 1) then
                xii1 = xteran(ii-1) * sclxes
              else
                xii1 = xcoord(ii-1) * sclxes
              end if
              do 90 is=1,nsubx
                u(is) = xii1 + (is - 1) * du
   90         continue
            else
CC              routines to pick up necessary x, y, and z values, to
CC              compute the za, zb, and zab values, and to estimate
CC              them when necessary -- preliminary when ii.eq.1.
              if (icontr .eq. 1) then
                yy3 = yteran(jj-1)
                yy4 = yteran(jj)
              else
                yy3 = ycoord(jj-1)
                yy4 = ycoord(jj)
              end if
              bb3 = DIFNQ(jfpol,1,yy4,yy3)
              b3sq = bb3*bb3
              if (icontr .eq. 1) then
                if (iym2 .ge. iy) ab2 = DIFNQ(0,1,yy3,yteran(iym2))
                if (iym3 .ge. iy) ab1 = DIFNQ(0,1,yteran(iym2),
     *                                 yteran(iym3))
                if (iyml .lt. iy-1) bb4 = DIFNQ(0,1,yteran(jj1),yy4)
              else
                if (iym2 .ge. iy) ab2 = DIFNQ(jfpol,1,yy3,
     *                                        ycoord(iym2))
                if (iym3 .ge. iy) ab1 = DIFNQ(jfpol,1,ycoord(iym2),
     *                                 ycoord(iym3))
                if (iyml .lt. iy-1) bb4 = DIFNQ(jfpol,1,ycoord(jj1),
     *                                          yy4)
              end if
              if (iyml1 .lt. iy-1) bb5 = bb6
              go to 120
            end if
CC            to save the old values.
            z3a2 = z3a3
            z4a2 = z4a3
            xx3 = xx4
            z33 = z43
            z3b3 = z4b3
            aa3 = aa4
            a3sq = aa3 * aa3
            z3a3 = z3a4
            z4a3 = z4a4
            za3b2 = za4b2
            za3b3 = za4b3
            za3b4 = za4b4
  100       xx4 = xx5
            z43 = z53
            z4b1 = z5b1
            z4b2 = z5b2
            z4b3 = z5b3
            z4b4 = z5b4
            z4b5 = z5b5
            aa4 = aa5
            z3a4 = z3a5
            z4a4 = z4a5
            za4b2 = za5b2
            za4b3 = za5b3
            za4b4 = za5b4
  110       xx5 = xx6
            z53 = z63
            z54 = z64
            z5b1 = z6b1
            z5b2 = z6b2
            z5b3 = z6b3
            z5b4 = z6b4
            z5b5 = z6b5
CC            to compute the za, zb, and zab values and to estimate
CC            the zb values when (jj.le.3).or.(jj.ge.nypnp-1).
  120       ix6 = ix6 + 1
            if (ix6 .gt. nx) go to 130
            if (icontr .eq. 1) then
              xx6 = xteran(ix6)
            else
              xx6 = xcoord(ix6)
            end if
            if (icontr .eq. 1) then
              z63 = ztran((jj-2)*nxs+ix6+joffs1)
              z64 = ztran((jn-1)*nxs+ix6+joffs1)
            else
              z63 = calcsa((jj-2)*nxs+ix6+joffs1)
              z64 = calcsa((jn-1)*nxs+ix6+joffs1)
            end if
            z6b3 = (z64 - z63) * bb3
            if (lym2 .lt. iy) then
              z6b2 = z6b3
              z6b4 = z6b3
            else
              if (iym2 .ge. iy) then
                if (icontr .eq. 1) then
                  z62 = ztran((jj-3)*nxs+ix6+joffs1)
                else
                  z62 = calcsa((jj-3)*nxs+ix6+joffs1)
                end if
                z6b2 = (z63 - z62) * ab2
              end if
              if (iym2 .ge. iy.and.iyml .eq. iy-1) then
                z6b4 = z6b3 + z6b3 - z6b2
              else
                if (icontr .eq. 1) then
                  z65 = ztran((jn1-1)*nxs+ix6+joffs1)
                else
                  z65 = calcsa((jn1-1)*nxs+ix6+joffs1)
                end if
                z6b4 = (z65 - z64) * bb4
                if (iym2 .eq. iy-1) then
                  z6b2 = z6b3 + z6b3 - z6b4
                end if
              end if
            end if
            if (iym3 .ge. iy) then
              if (icontr .eq. 1) then
                z6b1 = (z62 - ztran((jj-4)*nxs+ix6+joffs1)) * ab1
              else
                z6b1 = (z62 - calcsa((jj-4)*nxs+ix6+joffs1)) * ab1
              end if
            else
              z6b1 = z6b2 + z6b2 - z6b3
            end if
            if (iyml1 .lt. iy-1) then
              if (icontr .eq. 1) then
                z6b5 = (ztran((jn2-1)*nxs+ix6+joffs1) - z65) * bb5
              else
                z6b5 = (calcsa((jn2-1)*nxs+ix6+joffs1) - z65) * bb5
              end if
            else
              z6b5 = z6b4 + z6b4 - z6b3
            end if
            if (ix6 .eq. ix) go to 110
            if (IFEQU(xx6,xx5) .ne. 0) then
              aa5 = 1.0 / (xx6 - xx5)
            else
              aa5 = 0.1
            end if
            z3a5 = (z63 - z53) * aa5
            z4a5 = (z64 - z54) * aa5
            za5b2 = (z6b2 - z5b2) * aa5
            za5b3 = (z6b3 - z5b3) * aa5
            za5b4 = (z6b4 - z5b4) * aa5
            if (ix6 .eq. ix+1) go to 100
            go to 140
CC            to estimate the za and zab values when (ii.ge.nx-1).and.
CC            (nx.gt.2).
  130       if (lxm2 .ge. ix) then
              z3a5 = z3a4 + z3a4 - z3a3
              z4a5 = z4a4 + z4a4 - z4a3
              if (ixml .ge. ix) then
                za5b2 = za4b2 + za4b2 - za3b2
                za5b3 = za4b3 + za4b3 - za3b3
                za5b4 = za4b4 + za4b4 - za3b4
              end if
              go to 150
            end if
CC            to estimate the za and zab values when (ii.ge.nx-1).and.
CC            (nx.eq.2).
            z3a5 = z3a4
            z4a5 = z4a4
            if (ixml .eq. ix-1) go to 150
            za5b2 = za4b2
            za5b3 = za4b3
            za5b4 = za4b4
CC            to estimate the za and zab values when ii.eq.1.
  140       if (ixm1 .eq. ix-1) then
              z3a3 = z3a4 + z3a4 - z3a5
              z3a2 = z3a3 + z3a3 - z3a4
              z4a3 = z4a4 + z4a4 - z4a5
              z4a2 = z4a3 + z4a3 - z4a4
              za3b2 = za4b2 + za4b2 - za5b2
              za3b3 = za4b3 + za4b3 - za5b3
              za3b4 = za4b4 + za4b4 - za5b4
              go to 160
            end if
CC            numerical differentiation --- to determine partial
CC            derivatives zx, zy, and zxy as weighted means of
CC            divided differences za, zb, and zab, respectively
CC            to save the old values when ii.ne.1.
  150       zx33 = zx43
            zx34 = zx44
            zy33 = zy43
            zy34 = zy44
            zxy33 = zxy43
            zxy34 = zxy44
CC            new computation.
  160       do 170 jy=1,2
              w2 = ABS(za(4,jy) - za(3,jy))
              w3 = ABS(za(2,jy) - za(1,jy))
              sw = w2 + w3
              if (sw-1.0e-35 .gt. 0.0) then
                swi = 1.0 / sw
                wx2 = w2 * swi
                wx3 = w3 * swi
              else
                wx2 = 0.5
                wx3 = 0.5
              end if
              zx(jy) = wx2 * za(2,jy) + wx3 * za(3,jy)
              w2 = ABS(zb(jy+3) - zb(jy+2))
              w3 = ABS(zb(jy+1) - zb(jy))
              sw = w2 + w3
              if (sw-1.0e-35 .gt. 0.0) then
                swi = 1.0 / sw
                wy2 = w2 * swi
                wy3 = w3 * swi
              else
                wy2 = 0.5
                wy3 = 0.5
              end if
              zy(jy) = wy2 * zb(jy+1) + wy3 * zb(jy+2)
              zxy(jy) = wy2 * (wx2 * zab(1,jy) + wx3 * zab(2,jy)) +
     *                  wy3 * (wy2 * zab(1,jy+1) + wx3 * zab(2,jy+1))
  170       continue
            if (ixm1 .ge. ix) then
CC              determination of the coefficients of the polynomial.
              zx3b3 = FLWKQ(zx34-zx33,bb3)
              zx4b3 = FLWKQ(zx44 - zx43, bb3)
              zy3a3 = FLWKQ(zy43 - zy33, aa3)
              zy4a3 = FLWKQ(zy44 - zy34, aa3)
              fa = za3b3 - zx3b3 - zy3a3 + zxy33
              fb = zx4b3 - zx3b3 - zxy43 + zxy33
              fc = zy4a3 - zy3a3 - zxy34 + zxy33
              fd = zxy44 - zxy43 - zxy34 + zxy33
              fe = fa + fa - fb - fc
              p02 = FLWKQ(2.0 * (z3b3 - zy33) + z3b3 - zy34, bb3)
              p03 = FLWKQ(-2.0 * z3b3 + zy34 + zy33, b3sq)
              p12 = FLWKQ(2.0 * (zx3b3 - zxy33) + zx3b3 - zxy34, bb3)
              p13 = FLWKQ(-2.0 * zx3b3 + zxy34 + zxy33, b3sq)
              p20 = FLWKQ(2.0 * (z3a3 - zx33) + z3a3 - zx43, aa3)
              p21 = FLWKQ(2.0 * (zy3a3 - zxy33) + zy3a3 - zxy43, aa3)
              p22 = FLWKQ(3.0 * (fa + fe) + fd, aa3 * bb3)
              p23 = FLWKQ(-3.0 * fe - fb - fd, aa3 * b3sq)
              p30 = FLWKQ(-2.0 * z3a3 + zx43 + zx33, a3sq)
              p31 = FLWKQ(-2.0 * zy3a3 + zxy43 + zxy33, a3sq)
              p32 = FLWKQ(-3.0 * fe - fc - fd, bb3 * a3sq)
              p33 = FLWKQ(fd + fe + fe, a3sq * b3sq)
CC              computation of the polynomial
              do 190 jy=1,nsuby
                dy1 = (jy - 1) * dv1
                q00 = p00 + dy1 * (p01 + dy1 * (p02 + dy1 * p03))
                q10 = p10 + dy1 * (p11 + dy1 * (p12 + dy1 * p13))
                q20 = p20 + dy1 * (p21 + dy1 * (p22 + dy1 * p23))
                q30 = p30 + dy1 * (p31 + dy1 * (p32 + dy1 * p33))
                do 180 jx=1,nsubx
                  dx1 = (jx - 1) * du1
                  zz(jx,jy) = q00 + dx1 *(q10 + dx1 *(q20 + dx1 * q30))
  180           continue
  190         continue
CC
CC              process contour value.
CC
              ifdash = icndsh(ks,icontr)
              icolor = ICLRQ(icnclr(ks,icontr),1)
              do 210 js=1,nsby1
                do 200 is=1,nsbx1
                  ifl(is,js) = 0
  200           continue
  210         continue
              i2 = 0
CC
CC              loops over subdivided grid cell.
CC
CC              these loops check all four sides of each subcell
CC              for contour crossings. the order is normally
CC              ((ia=1,nsbx1),ja=1,nsby1), until a crossing is
CC              found, then the contour is 'followed' from each
CC              subcell to the next, and each subcell is marked
CC              (ifl(i,j)=1) until the contour becomes closed or
CC              leaves the main grid cell. xx1,yy1 is the first
CC              crossing point found in a subcell, and (xx2,y2)
CC              is the second. i and j are the x and y counters
CC              for the subcells. i1 is the flag for how many
CC              (1 or 2) crossings have been found (and which of
CC              the 4 sides (xx2,yy2) is on) for the current
CC              subcell. kfl is the flag to prevent plotter pen
CC              movement if the pen is already located at (xx1,
CC              yy1). jfl is the flag which signifies that a
CC              reversal of direction has just been made in
CC              'following' the contour. lfl is the flag which
CC              signifies whether contours in the current main
CC              cell and the main cell directly to the left have
CC              been labeled.
              do 370 ja=1,nsby1
                do 360 ia=1,nsbx1
                  js = ja
                  is = ia
                  i1 = 0
                  if (ifl(is,js) .eq. 0) then
                    kfl = 0
                    jfl = 0
CC                    check lower side of subcell.
  220               if ((zz(is,js)-c)*(zz(is+1,js)-c) .le. 0.0) then
CC                     linear interpolation for x-coordinate of contour.
                      xx2 = zz(is,js) - zz(is+1,js)
                      if (IFEQU(xx2,0.0) .eq. 0) then
                        xx2 = 1.0
                      else
                        xx2 = (zz(is,js)-c) / xx2
                      end if
                      xx2 = u(is) + du * xx2
                      yy2 = v(js)
CC                      determine if 2 contour intersections have been
CC                      found for current subcell.
                      if (i1 .ne. 0) go to 260
                      xx1 = xx2
                      yy1 = yy2
                      i1 = 1
                    end if
CC                    check left side of subcell.
  230               if ((zz(is,js)-c)*(zz(is,js+1)-c) .le. 0.0) then
                      xx2 = u(is)
CC                     linear interpolation for y-coordinate of contour.
                      yy2 = zz(is,js) - zz(is,js+1)
                      if (IFEQU(yy2,0.0) .eq. 0) then
                        yy2 = 1.0
                      else
                        yy2 = (zz(is,js) - c) / yy2
                      end if
                      yy2 = v(js) + dv * yy2
CC                      determine if 2 contour intersections have been
CC                      found for current subcell.
                      if (i1 .ne. 0) go to 280
                      xx1 = xx2
                      yy1 = yy2
                      i1 = 2
                    end if
CC                      check upper side of subcell.
  240               if ((zz(is,js+1)-c)*(zz(is+1,js+1)-c) .le. 0.0) then
CC                     linear interpolation for x-coordinate of contour.
                      xx2 = zz(is,js+1) - zz(is+1,js+1)
                      if (IFEQU(xx2,0.0) .eq. 0) then
                        xx2 = 1.0
                      else
                        xx2 = (zz(is,js+1) - c) / xx2
                      end if
                      xx2 = u(is) + du * xx2
                      yy2 = v(js+1)
CC                      determine if 2 contour intersections have been
CC                      found for current subcell.
                      if (i1 .ne. 0) go to 300
                      xx1 = xx2
                      yy1 = yy2
                      i1 = 3
                    end if
CC                    determine if 2 contour intersections have been
CC                    found for current subcell.
                    if (i1 .eq. 0) go to 350
CC                    check right side of subcell.
  250               if ((zz(is+1,js)-c)*(zz(is+1,js+1)-c) .gt. 0.0)
     *                  go to 220
                    xx2 = u(is+1)
CC                    linear interpolation for y-coordinate of contour.
                    yy2 = zz(is+1,js) - zz(is+1,js+1)
                    if (IFEQU(yy2,0.0) .eq. 0) then
                      yy2 = 1.0
                    else
                      yy2 = (zz(is+1,js) - c) / yy2
                    end if
                    yy2 = v(js) + dv * yy2
                    go to 320
CC                    mark current subcell as having been checked for
CC                    current contour value.
  260               ifl(is,js) = 1
CC                    look for next subcell at either end of the line
CC                    segment in the current subcell.
  270               if (js .eq. 1) go to 340
                    if (ifl(is,js-1) .ne. 0) go to 340
                    js = js - 1
                    if (kfl .eq. 0) then
                      kfl = 1
                      call CHCKQ(xx1,yy1,3,ks,0,jfpol)
                    end if
                    call CHCKQ(xx2,yy2,2,ks,0,jfpol)
                    i1 = 3
                    jfl = 0
                    xx1 = xx2
                    yy1 = yy2
                    go to 250
CC                    mark current subcell as having been checked for
CC                    current contour value.
  280               ifl(is,js) = 1
CC                    look for next subcell at either end of the line
CC                    segment in the current subcell.
  290               if (is .eq. 1) go to 340
                    if (ifl(is-1,js) .ne. 0) go to 340
                    is = is - 1
                    if (kfl .eq. 0) then
                      kfl = 1
                      call CHCKQ(xx1,yy1,3,ks,0,jfpol)
                    end if
                    call CHCKQ(xx2,yy2,2,ks,0,jfpol)
                    i1 = 4
                    jfl = 0
                    xx1 = xx2
                    yy1 = yy2
                    go to 220
CC                    mark current subcell as having been checked for
CC                    current contour value.
  300               ifl(is,js) = 1
CC                    look for next subcell at either end of the line
CC                    segment in the current subcell.
  310               if (js .eq. nsby1) go to 340
                    if (ifl(is,js+1) .ne. 0) go to 340
                    js = js + 1
                    if (kfl .eq. 0) then
                      kfl = 1
                      call CHCKQ(xx1,yy1,3,ks,0,jfpol)
                    end if
                    call CHCKQ(xx2,yy2,2,ks,0,jfpol)
                    i1 = 1
                    jfl = 0
                    xx1 = xx2
                    yy1 = yy2
                    go to 230
  320               ifl(is,js) = 1
CC                    mark current subcell as having been checked for
CC                    current contour value.
  330               if (is .eq. nsbx1) go to 340
CC                    look for next subcell at either end of the line
CC                    segment in the current subcell.
                    if (ifl(is+1,js) .ne. 0) go to 340
                    is = is + 1
                    if (kfl .eq. 0) then
                      kfl = 1
                      call CHCKQ(xx1,yy1,3,ks,0,jfpol)
                    end if
                    call CHCKQ(xx2,yy2,2,ks,0,jfpol)
                    i1 = 2
                    jfl = 0
                    xx1 = xx2
                    yy1 = yy2
                    go to 240
CC                    reverse direction of following contour and try
CC                    again, or if reversal has already been made,
CC                    draw line segment and go to end of loop for ia.
  340               if (jfl .eq. 0) then
                      jfl = 1
                      temp = xx1
                      xx1 = xx2
                      xx2 = temp
                      temp = yy1
                      yy1 = yy2
                      yy2 = temp
                      go to (270,290,310,330),i1
                    end if
                    if (kfl .eq. 0) call CHCKQ(xx2,yy2,3,ks,0,jfpol)
                    call CHCKQ(xx1,yy1,2,ks,0,jfpol)
                    i2 = 1
                  end if
  350             continue
  360           continue
  370         continue
            end if
  380     continue
  390   continue
CC
CC        label contours
        if (icontr .eq. 1) then
          if (IABS(iswplt(11)) .ne. 3) call CHCKQ(xx1,yy1,2,ks,1,jfpol)
        else
          if (IABS(iswplt(24)) .ne. 3) call CHCKQ(xx1,yy1,2,ks,1,jfpol)
        end if
  400 continue
      if (IABS(ipoptn(jpoptn)) .eq. 4) then
        zminp = 1.0 / zminds(icontr)
        do 420 js=iy,ny
          i1 = (js - 1) * nxs
          do 410 is=ix,nx
            if (icontr .eq. 1) then
              ztran(is+i1+joffs1) = (FSEXP(ztran(is+i1+joffs1)) *
     *                              zminp) - zmincs(icontr)
            else
              calcsa(is+i1+joffs1) = (FSEXP(calcsa(is+i1+joffs1)) *
     *                              zminp) - zmincs(icontr)
            end if
  410     continue
  420   continue
      end if
      j = 0
      if (icontr .eq. 1) then
        if (IABS(iswplt(11)) .ne. 3.and.iswplt(11) .lt. 0) then
          j = iswplt(11)
        end if
      else
        if (IABS(iswplt(24)) .ne. 3.and.iswplt(24) .lt. 0) then
          j = iswplt(24)
        end if
      end if
      if (j .ne. 0) then
CC        draw legend of contour values vs. labels
        iswplt(17) = 1
        xx2 = 14.0
        if (ncontr(icontr) .gt. 5) xx2 = 29.0
        xx2 = xx2 * htnchr
        yy2 = ncontr(icontr)
        yy2 = AMIN1(yy2, 5.0) * 1.5 * htnchr + htnchr
        if (icontr .eq. 1) then
          iswplt(20) = LBLPQ(xx1,yy1,xx2,yy2,iswplt(20))
        else
          iswplt(25) = LBLPQ(xx1,yy1,xx2,yy2,iswplt(25))
        end if
        xx2 = xx1 + xx2
        yy2 = yy1 + yy2
        if (ncontr(icontr) .gt. 5) lng = 27
        icolor = ICLRQ(iswplt(9),0)
        call BKGDQ(xx1,yy1,xx2,yy2,1)
        icolor = ICLRQ(iswplt(16),1)
        call BKGDQ(xx1,yy1,xx2,yy2,2)
        x1 = xx1 + 0.5 * htnchr
        y1 = yy2
        do 440 is=1,ncontr(icontr)
          icolor = ICLRQ(icnclr(is,icontr),1)
          y1 = y1 - 1.5 * htnchr
          if (is .eq. 6) then
            x1 = x1 + 15.0 * htnchr
            y1 = yy2 - 1.5 * htnchr
          end if
          if (j .ne. -2) then
            WRITE (mlabl,9000) malpha(is),contrs(is,icontr)
          else
            WRITE (mlabl,9001) is,contrs(is,icontr)
          end if
          call SYMBQ(x1,y1,htnchr,mnpbuf,0.0,14)
  440   continue
        if (nbxpss .lt. 10) then
          nbxpss = nbxpss + 1
          xbxpsl(nbxpss) = xx1
          xbxpel(nbxpss) = xx2
          ybxpsl(nbxpss) = yy1
          ybxpel(nbxpss) = yy2
        end if
      end if
      iswplt(17) = 2
CC
      return
CC
 9000 format (a1,' - ',1p,e9.2,1x)
 9001 format (i2,' -',1p,e9.2,1x)
      end
      Function FLWKQ(arg1,arg2)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - FLWKQ Version 01.0                          ---
CC--- Subroutine description - prevent floating underflow of arg1 * ---
CC---                          arg2.                                ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      if (IFEQU(arg1,0.0) .ne. 0.and.IFEQU(arg2,0.0) .ne. 0) then
        if (ABS(arg1) .ge. 1.0.or.ABS(arg2) .ge. 1.0) then
          FLWKQ = arg1 * arg2
        else
          temp1 = 1.0e-37 / arg1
          if (ABS(temp1) .lt. ABS(arg2)) then
            FLWKQ = arg1 * arg2
          else
            FLWKQ = 0.0
          end if
        end if
      else
        FLWKQ = 0.0
      end if
      return
      end
      Subroutine CHCKQ(xx,yy,kr,kc,ks,jfpol)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - CHCKQ Version 01.2                          ---
CC--- Subroutine description - save points at which isopleth labels ---
CC---                          are to be drawn.                     ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      integer*2 lp,nl,lflg,jj,nn,mm,kk,ii,i2,jk
      character*1 mnmbr
CC
      dimension mnmbr(12)
CC
      include 'OCNTRL.INC'
      include 'OWNTRL.INC'
      include 'OCDSPM.INC'
      include 'ONAMES.INC'
      include 'OCALCS.INC'
CC
      save lp,nl,xplst,yplst
CC
      data dlng/0.05/
CC
CC**********************************************************************
CC
CC        calc current plot position.
      if (jfpol .eq. 0) then
        xp = xx
        yp = yy
      else
        x1 = yy * dtorad
        xp = xx * SIN(x1) + xorgin * sclxes
        yp = xx * COS(x1) + yorgin * sclyes
      end if
      xp = xp - pxmins * sclxes + xlmrgn
      yp = yp - pymins * sclyes + ybmrgn
      if (ks .eq. 0) then
        lflg = 0
      else
        lflg = 2
      end if
   10 if (lflg .eq. 0) then
CC
CC--------plot point.
        call PLT1Q(xp,yp,kr,dlng)
        if (ifbndx .eq. 0) return
        if (icontr .eq. 1) then
          if (IABS(iswplt(11)) .eq. 3) return
        else
          if (IABS(iswplt(24)) .eq. 3) return
        end if
        if (ifbndx .eq. 1) then
          x1 = xp
          y1 = yp
          jj = kr
        else
          x1 = xbndxs
          y1 = ybndxs
          if (ifbndx .eq. 2) then
            jj = kr
          else
            jj = 3
          end if
        end if
CC        if pen up.
        if (jj .eq. 2) then
CC          continuation of line, pen down, save end point.
          ixlpnt(nl,2) = x1 * 1.0e3
          iylpnt(nl,2) = y1 * 1.0e3
CC          calc. angle of slope of contour segment.
          ang = ANTRU(ARTAN(y1 - yplst,x1 - xplst) * radtod)
          if (ang .gt. 180.0) ang = ang - 180.0
          if (ang .gt. 90.0) ang = ang - 180.0
          ialpnt(nl,2) = ang * 10.0
CC          increment segment point counter.
          lp = lp + 1
          if (lp .le. 1) then
CC            first continuation point, save angle of segment.
            ialpnt(nl,1) = ialpnt(nl,2)
          end if
          xplst = xp
          yplst = yp
          return
        end if
      end if
      if (lflg .le. 1) then
CC        new line, save start point.
CC        60 is the max dimension of the 1st dimension of ixlpnt, iylpnt,
CC        ialpnt and the maximum number of contour segment end points
CC        that can be saved.
        if (ncurvs + 1 .le. 60) then
CC          increment segment counter for contour kc.
          ncurvs = ncurvs + 1
CC          set first point of segment.
          ixlpnt(ncurvs,1) = x1 * 1.0e3
          iylpnt(ncurvs,1) = y1 * 1.0e3
CC          zero out end point of segment and angles.
          ialpnt(ncurvs,1) = 0
          ialpnt(ncurvs,2) = 0
          ixlpnt(ncurvs,2) = 0
          iylpnt(ncurvs,2) = 0
          nl = ncurvs
          lp = 0
          xplst = xp
          yplst = yp
          return
        end if
      end if
CC        too many segments or end of plot, find end points that
CC        match and merge segments together.
      idf = 1
      do 100 nn=1,2
        if (ncurvs .gt. 1) then
          mm = 0
CC          loop over segments.
   20     mm = mm + 1
          if (mm .lt. ncurvs) then
   30       kk = mm + 1
CC            first point of segment mm.
            ix1 = ixlpnt(mm,1)
            iy1 = iylpnt(mm,1)
CC            last point of segment mm.
            ix2 = ixlpnt(mm,2)
            iy2 = iylpnt(mm,2)
            if (ix1 + iy1 + ix2 + iy2 .gt. 0) then
CC              compare segment mm to all other segments.
              do 50 jj=kk,ncurvs
CC                loop over first and last points of segment jj.
                do 40 ii=1,2
CC                  first or last point of segment jj.
                  ix3 = ixlpnt(jj,ii)
                  iy3 = iylpnt(jj,ii)
CC                  is point set.
                  if (ix3 + iy3 .gt. 0) then
CC                    is first point of segment mm set.
                    if (ix1 + iy1 .gt. 0) then
                      i2 = 1
CC                      is point of segment mm close to point of
CC                      segment jj.
                      if (IABS(ix1-ix3) .le. idf.and.IABS(iy1-iy3) .le.
     *                    idf) go to 60
                    end if
CC                    is last point of segment jj set.
                    if (ix2 + iy2 .gt. 0) then
                      i2 = 2
CC                      is point of segment mm close to point of
CC                      segment jj.
                      if (IABS(ix2-ix3) .le. idf.and.IABS(iy2-iy3) .le.
     *                    idf) go to 60
                    end if
                  end if
   40           continue
   50         continue
            end if
CC            no points are close, go get next segment.
            go to 20
CC            segments match, merge two segments together.
   60       ii = MOD(ii,2) + 1
            ixlpnt(mm,i2) = ixlpnt(jj,ii)
            iylpnt(mm,i2) = iylpnt(jj,ii)
            ialpnt(mm,i2) = ialpnt(jj,ii)
CC            zero out points that match.
            do 70 ii=1,2
              ixlpnt(jj,ii) = 0
              iylpnt(jj,ii) = 0
              ialpnt(jj,ii) = 0
   70       continue
CC            go look for more matches.
            go to 30
          end if
CC          pack segments down removing end points that are not set.
          kk = 0
          mm = 0
   80     if (mm .lt.ncurvs) then
            mm = mm + 1
            if (ixlpnt(mm,1)+iylpnt(mm,1)+ixlpnt(mm,2)+iylpnt(mm,2)
     *          .gt. 0) then
              kk = kk + 1
              do 90 jj=1,2
                ixlpnt(kk,jj) = ixlpnt(mm,jj)
                iylpnt(kk,jj) = iylpnt(mm,jj)
                ialpnt(kk,jj) = ialpnt(mm,jj)
   90         continue
            end if
            go to 80
          end if
CC          reset the number of contour segments saved.
          ncurvs = kk
        end if
        idf = 200
  100 continue
CC      60 is the max dimension of the 1st dimension of ixlpnt, iylpnt,
CC      ialpnt.
      if (ks .eq. 0.and.kk .lt. 60) then
        lflg = 1
        go to 10
      end if
CC
CC--------label isopleths
CC      set flag to allow plotting over entire plot area.
CC      minimum height of characters.
      if (htnchr .lt. spmins) htnchr = spmins
CC      set color of contour line.
      icolor = ICLRQ(icnclr(kc,icontr),1)
      if (icontr .eq. 1) then
        jk = IABS(iswplt(11))
      else
        jk = IABS(iswplt(24))
      end if
      if (jk .le. 0) then
        xk = 4.0
      else
        xk = 2.0
      end if
CC      number of contour segments.
      if (ncurvs .gt. 0) then
CC        loop over contour segments.
        do 140 ii=1,ncurvs
          x2 = -1.0e4
          y2 = -1.0e4
CC          loop over end points of segment ii.
          do 130 kk=1,2
            i2 = kk - 2
CC            first or last point of segment ii.
            x1 = FLOAT(ixlpnt(ii,kk)) * 0.001
            y1 = FLOAT(iylpnt(ii,kk)) * 0.001
CC            if point set.
            if (x1 + y1 .gt. 0.0) then
CC              if the first point or distance between first and
CC              last > xk*0.125.
              if (kk .eq. 1.or.SQRT((x1-x2)**2 + (y1-y2)**2) .gt.
     *            xk*0.125) then
CC                see if too close to previously plotted label.
                if (nctlbl .gt. 0) then
                  do 110 jj=1,nctlbl
                    x3 = FLOAT(ixlpts(jj)) * 0.001
                    y3 = FLOAT(iylpts(jj)) * 0.001
                    if (SQRT((x1-x3)**2 + (y1-y3)**2) .le. xk*htnchr)
     *                  go to 120
  110             continue
                end if
CC                determine angle of isopleth line.
                ang = ialpnt(ii,kk) * 0.1
CC                determine number of digits in number.
                if (jk .ne. 1) then
                  if (jk .ne. 2) then
                    xs = contrs(kc,icontr)
                    nd = 2
                  else
                    xs = kc
                    nd = 0
                  end if
                  nch = iwtbf(mnmbr,9,xs,nd,1)
                else
                  nch = 1
                  mnmbr(1) = malpha(kc)
                end if
CC                determine if number will go out of plot bounds.
                if (IFOTQ(x1,y1,nch,htnchr,ang) .eq. 0) then
CC                  draw label.
                  iswplt(17) = 1
                  call SYMBQ(x1,y1,htpchr,mnmbr,ang,nch)
                  iswplt(17) = 2
                  x2 = x1
                  y2 = y1
                  if (nctlbl .lt. 200) then
                    nctlbl = nctlbl + 1
                    ixlpts(nctlbl) = x1 * 1.0e3
                    iylpts(nctlbl) = y1 * 1.0e3
                  end if
                end if
              end if
            end if
  120       ixlpnt(ii,kk) = -IABS(ixlpnt(ii,kk))
            iylpnt(ii,kk) = -IABS(iylpnt(ii,kk))
  130     continue
  140   continue
      end if
      ncurvs = 0
      if (ks .eq. 0) then
CC        move pen back.
        call PLT2Q(xp,yp,3)
        lflg = 1
        go to 10
      end if
      xplst = xp
      yplst = yp
      return
      end
      Function DIFNQ(ifpol,lf,v1,v2)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - DIFNQ Version 01.0                          ---
CC--- Subroutine description - difference or inverse difference of  ---
CC---                          two values (angles, etc.)            ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      DIFNQ = v1 - v2
      if (ifpol .ne. 0) then
        if (ABS(DIFNQ) .gt. 180.0) DIFNQ = SIGN(360.0-ABS(DIFNQ),DIFNQ)
      end if
      if (lf .ne. 0) then
        if (IFEQU(DIFNQ,0.0) .ne. 0) then
          DIFNQ = 1.0/DIFNQ
        end if
      end if
      return
      end
      Function IFOTQ(xl,yl,nc,h,a)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IFOTQ Version 01.0                          ---
CC--- Subroutine description - see if number or label will exceed   ---
CC---                          plot boundaries. if only partially   ---
CC---                          exceeded, label is moved in by shift----
CC---                          ing xl and or yl.                    ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      include 'OCNTRL.INC'
      include 'OWNTRL.INC'
      include 'OCDSPM.INC'
CC
CC**********************************************************************
CC
      y1 = a * dtorad
      r = nc * h + 0.001
      x1 = r * COS(y1)
      y1 = r * SIN(y1)
      if (iswplt(17) .le. 1) then
        xll = 0.0
        ybl = 0.0
        xrl = xnchpl
        ytl = ynchpl
      else
        xll = xlmrgn
        ybl = ybmrgn
        xrl = xrmrpg
        ytl = ytmrpg
      end if
      IFOTQ = 1
      if (IFBXQ(xl,yl,xll,xrl,ybl,ytl) .ne. 0.or.
     *    IFBXQ(xl+x1,yl+y1,xll,xrl,ybl,ytl) .ne. 0) then
        if (a .gt. 60.0) then
          xl = xl - 0.02
        else if (a .gt. 30.0) then
          xl = xl - 0.02
          yl = yl + 0.02
        else if (a .gt. -30.0) then
          yl = yl + 0.02
        else if (a .gt. -60.0) then
          xl = xl + 0.02
          yl = yl + 0.02
        else
          xl = xl + 0.02
        end if
        IFOTQ = 0
        if (xl .le. xll) xl = xll + 0.01
        if (yl .le. ybl) yl = ybl + 0.01
        if (xl .ge. xrl) xl = xrl - 0.01
        if (yl .ge. ytl) yl = ytl - 0.01
        if (xl+x1 .le. xll.or.xl+x1 .ge. xrl.or.yl+y1 .le. ybl.or.
     *      yl+y1 .ge. ytl) then
          xl = xl - x1
          yl = yl - y1
        end if
      end if
      return
      end
