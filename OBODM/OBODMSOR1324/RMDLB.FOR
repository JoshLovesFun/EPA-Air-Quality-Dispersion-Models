CC-DEBUGS
CCPCITS        
CC>   Interface to function system[C] (string)
CC>   integer*2 system
CC>   character*1 string[REFERENCE]
CC>   end
CCPCITE
CC-DEBUGE
      Subroutine RMDLB
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - RMDLB Version 01.3                          --- 072497
CC--- Subroutine description - controls calculations of the         ---
CC---                          dispersion models.                   ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
CC-DEBUGS
CCPCITS
CC>   integer*2 system
CCPCITE
CC-DEBUGE
CC
      include 'OCNTRL.INC'
      include 'OCDSPM.INC'
      include 'OCALCS.INC'
      include 'ONAMES.INC'
      include 'OWNTRL.INC'
CC
CC**********************************************************************
CC
CC-DEBUGS
      if (ifdbug .gt. 0.and.ifdbug .lt. 7) then                         020797
        if (iotfil .gt. 0) WRITE (iotfil,9000)
CCPCITS
CC        Invoke COMMAND.COM with the command line:
CC>     i = system('mem /c | more'C)
CC>     if (i .eq. -1) pause 'Could not run COMMAND.COM'
CCPCITE
      end if
CC-DEBUGE
CC
      if (icpont .ne. 0) go to 10
      i = jinit(ifwrtt,10,0,0)
CC      determine minimum wind speed.
      wspmin = AMAX1(PWLAW(refspd,2.0,refhgt,pwrlaw),vllimt(6))
      if (ABS(xorgin)+ABS(yorgin) .gt. 0.0) then
        if (ABS(xorgin)+ABS(yorgin) .gt. 1.0e5) then
          jfutms = 2
        else
          jfutms = 1
        end if
      else
        jfutms = 0
      end if
      ifutms = 0
      kfutms = 0
      if (jfutms .gt. 0) then
        if (nxpnts .gt. 0.and.nypnts .gt. 0) then
          if (ABS(xcoord(1))+ABS(ycoord(1)) .gt. 1.0e5) then
            ifutms = 1
          end if
        end if
        if (nxypnt .gt. 0) then
          if (ABS(xdscrt(1))+ABS(ydscrt(1)) .gt. 1.0e5) then
            kfutms = 1
          end if
        end if
      end if
CC        begin loop over sources.
      kssndx = 0
CC      save print met. data option flag.
   10 icpont = 0
      if (kssndx .lt. nsourc) then
        kssndx = kssndx + 1
        calhgt(kssndx) = 0.0                                            072497
        if (ifhrly .ne. 0) then
          i = inihrs
          if (i .ge. 100) i = i / 100
          i = i + iofhrs
          if (i .lt. 1.or.i .gt. 24) then
            if (KERRS(59,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0) .eq. 1)
     *          go to 100
            ifnext = 3
            ifrdwt(5) = -1
            go to 100
          end if
          if (isofrq(i,kssndx) .eq. 0.or.ifcalc .le. 0) then
CC            zero out calculation arrays.
            i = JRNIT(calcsa,ntotal,0.0,0)
            icpont = 2                                                  110198
            go to 100                                                   110198
          end if
        end if
        if (IFEQU(qemiss(kssndx),0.0) .eq. 0) then
          i = JRNIT(calcsa,ntotal,0.0,0)
          icpont = 2                                                    110198
          go to 100                                                     110198
        end if
CC
        if (alphas(kssndx) .gt. 0.0) alphad = alphas(kssndx)
        if (betass(kssndx) .gt. 0.0) betadf = betass(kssndx)
        cldmax(kssndx) = 0.0
        cldmxd(kssndx) = 0.0
CC        source emission type.
CC          1-instantaneous
CC          2-quasi-continuous square wave
        itype = IVGET(isotyp(kssndx),2)
CC        source type.
CC          1-volume/point
CC          2-line
        jtype = IVGET(isotyp(kssndx),1)
CC        plume rise flag.
        ifplm = 0
        ifmxpc = 0
        ifmxpi = iswopt(25) * 2
        if (brnrat(kssndx) .gt. 0.0.and.hetcnt(kssndx) .gt. 0.0) then
CC          turn on plume rise.
          ifplm = 1
        end if
        if (itype .eq. 1.and.(relhgt(kssndx) .lt. 0.0.or.               072497
     *      clddpt(kssndx) .lt. 0.0.or.cldsht(kssndx) .lt. 0.0)) then   072497
CC          Determine default diameter of detonation.                   072497
          airden = AIRDN(airprs,airtmp,airhum)                          072497
          rads = 0.89 * (3.0 * hetcnt(kssndx) * qemiss(kssndx) /        072497
     *         (4.0 * 3.14159 * spheat * airden * 1.0e6 * (airtmp +     072497
     *         constc(2,1))))** 0.333333                                072497
          diam = 2.0 * rads                                             072497
        end if                                                          072497
CC        save parameters.                                              072497
        svdxss = clddxs(kssndx)                                         072497
        svdyss = clddys(kssndx)                                         072497
        svdzss = clddzs(kssndx)                                         072497
        svhgts = relhgt(kssndx)                                         072497
        svisss = qemiss(kssndx)                                         072497
        svlngs = cldlng(kssndx)                                         072497
        svshts = cldsht(kssndx)                                         072497
        svdpts = clddpt(kssndx)                                         072497
        svhtes = relhte(kssndx)
CC        if line source, simulate with multiple volume sources.
        nlsorc = 0
        if (jtype .eq. 2) then
          if (IVGET(isrect(kssndx),2) .eq. 1) then
CC            convert from polar coordinates.
            a = clddys(kssndx) * dtorad
            dxssss = clddxs(kssndx) * SIN(a) + xorgin
            dyssss = clddxs(kssndx) * COS(a) + yorgin
            a = clddye(kssndx) * dtorad
            dxesss = clddxe(kssndx) * SIN(a) + xorgin
            dyesss = clddxe(kssndx) * COS(a) + yorgin
            clddxs(kssndx) = dxssss
            clddys(kssndx) = dyssss
            clddxe(kssndx) = dxesss
            clddye(kssndx) = dyesss
            i = IVSTO(isrect(kssndx),2,0)
          else
CC            rectangular coordinates.
            dxssss = clddxs(kssndx)
            dyssss = clddys(kssndx)
            dxesss = clddxe(kssndx)
            dyesss = clddye(kssndx)
          end if
          dzssss = clddzs(kssndx)
          dzesss = clddze(kssndx)
CC          number of volume sources.
          if (cldsht(kssndx) .le. 0.0) then                             072497
CC            default.                                                  072497
            dumy = diam                                                 072497
          else                                                          072497
            dumy = cldsht(kssndx)                                       072497
          end if                                                        072497
          nlsorc = AMAX1((cldlng(kssndx) / cldsht(kssndx) + 0.5), 2.0)
CC          determine x, y and h increments.
          dumy = 1.0 / FLOAT(nlsorc)
          dxincs = (dxesss - dxssss) * dumy
          dyincs = (dyesss - dyssss) * dumy
          dzincs = (dzesss - dzssss) * dumy
          if (relhgt(kssndx) .lt. 0.0) then                             072497
CC            default to 1/2 diameter.                                  072497
            if (clddpt(kssndx) .lt. 0.0) then                           072497
              dumy1 = 0.5 * diam                                        072497
            else                                                        072497
              dumy1 = 0.5 * clddpt(kssndx)                              072497
            end if                                                      072497
          else                                                          072497
            dumy1 = relhgt(kssndx)                                      072497
          end if                                                        072497
          if (relhte(kssndx) .lt. 0.0) then                             072497
CC            default to 1/2 diameter.                                  072497
            dumy2 = 0.5 * diam                                          072497
          else                                                          072497
            dumy2 = relhte(kssndx)                                      072497
          end if                                                        072497
          dhincs = (dumy2 - dumy1) * dumy                               072497
          dhssss = dumy1                                                072497
CC          divide up source emission strength.
          dqincs = qemiss(kssndx) * dumy
          nlincs = 0
          dxssss = dxssss - dxincs + 0.5 * dxincs
          dyssss = dyssss - dyincs + 0.5 * dyincs
          dzssss = dzssss - dzincs + 0.5 * dzincs
          dhssss = dhssss - dhincs + 0.5 * dhincs
          cldlng(kssndx) = SQRT(dxincs**2 + dyincs**2)                  072497
        end if
   20   if (nlsorc .gt. 0) then
CC          loop over volume sources simulating a single line source.
          nlincs = nlincs + 1
          if (nlincs .gt. nlsorc) then
CC            go restore saved parameters.
            nlsorc = 0
CC            continue with next source.
            icpont = 1                                                  110198
            go to 100                                                   110198
          end if
CC          set emission strength.
          qemiss(kssndx) = dqincs
CC          set x coordinate.
          dxssss = dxssss + dxincs
          clddxs(kssndx) = dxssss
CC          set y coordinate.
          dyssss = dyssss + dyincs
          clddys(kssndx) = dyssss
CC          set z elevation.
          dzssss = dzssss + dzincs
          clddzs(kssndx) = dzssss
CC          set emission height.
          dhssss = dhssss + dhincs
          relhgt(kssndx) = dhssss
        end if
        if (nlsorc .le. 0.or.(nlsorc .gt. 0.and.nlincs .le. 1)) then
CC          zero out calculation arrays.
          i = JRNIT(calcsa,ntotal,0.0,0)
        end if
CC        if sources are in polar coordinates
        if (IVGET(isrect(kssndx),2) .le. 0) then
CC          rectangular source coordinates
          xsl = clddxs(kssndx)
          ysl = clddys(kssndx)
          if (jfutms .gt. 0) then
            if (ABS(xsl)+ABS(ysl) .gt. 1.0e5) then
                lfutms = 1
            else
                lfutms = 0
            end if
            if (jfutms .ne. 2.or.lfutms .ne. 1) then
              xsl = xsl + xorgin
              ysl = ysl + yorgin
            end if
          end if
        else
CC          polar source coordinates
          a = clddys(kssndx) * dtorad
          xsl = clddxs(kssndx) * SIN(a) + xorgin
          ysl = clddxs(kssndx) * COS(a) + yorgin
        end if
CC        initialize standard deviations of source material dist.
        if (cldlng(kssndx) .le. 0.0) then                               072497
CC          default diameter of detonation.                             072497
          cldlng(kssndx) = diam                                         072497
        end if                                                          072497
        if (cldsht(kssndx) .le. 0.0) then                               072497
          cldsht(kssndx) = diam                                         072497
        end if                                                          072497
        if (clddpt(kssndx) .le. 0.0) then                               072497
          clddpt(kssndx) = diam                                         072497
        end if                                                          072497
        if (relhgt(kssndx) .lt. 0.0) then                               072497
          if (clddpt(kssndx) .lt. 0.0) then                             072497
            relhgt(kssndx) = 0.5 * diam                                 072497
          else                                                          072497
            relhgt(kssndx) = 0.5 * clddpt(kssndx)                       072497
          end if                                                        072497
        end if                                                          072497
        if (nlsorc .gt. 0) then
          a = 2.35482
        else
          a = 4.3
        end if
        b = wnddir - grdang
        if (b .lt. 0.0) b = b + 360.0
        if (b .gt. 360.0) b = b - 360.0
        b = ABS(b-cldang(kssndx))
        if (b .gt. 180.0) b = b - 180.0
        if (b .gt. 90.0) b = b - 90.0
        b = b * dtorad
        sigxop = (cldsht(kssndx) * SIN(b) + cldlng(kssndx) * COS(b)) /  072497
     *           4.3                                                    072497
        c = 1.570796 - b
        sigyop = (cldsht(kssndx) * SIN(c) + cldlng(kssndx) * COS(c)) / a072497          072497
        if (IFEQU(relhgt(kssndx),0.0) .ne. 0) then
          sigzop = clddpt(kssndx) / 4.3                                 072497
        else
          sigzop = clddpt(kssndx) / 2.15                                072497
        end if
        calhgt(kssndx) = relhgt(kssndx)                                 072497
CC-DEBUGS
        if (ifdbug .gt. 0.and.ifdbug .lt. 7) then                       020797
          if (ifhrly .ne. 0) then                                       030499
            WRITE (iotfil,9006) inihrs,inidys,inimon,iniyrs,inimin,     030499
     *                          injdys,ntothr                           030499
          end if                                                        030499
          WRITE (iotfil,9001) kssndx,nvsprt,ifprlp,ifmxpi,istrec,
     *                        isrect(kssndx),iqunit(kssndx),
     *                        isotyp(kssndx),qemiss(kssndx),
     *                        taucld(kssndx),relhgt(kssndx),
     *                        clddxs(kssndx),clddys(kssndx),
     *                        sigxop,sigyop,sigzop,xsmlry(kssndx),
     *                        xsmlrz(kssndx),xlrgry(kssndx),
     *                        xlrgrz(kssndx),hetcnt(kssndx),
     *                        brnrat(kssndx),gammat(kssndx),
     *                        alphas(kssndx),betass(kssndx),
     *                        cldlng(kssndx),cldsht(kssndx),            072497
     *                        clddpt(kssndx),cldang(kssndx),a,b,c,      073098
     *                        nlsorc                                    073098
        end if
CC-DEBUGE
        icalc = 0
CC
CC-DEBUGS
        if (ifdbug .gt. 2.and.ifdbug .lt. 7) then                       020797
          WRITE (iotfil,9002) sigmep,sigmap,trbixr,sigmeq,sigmaq,trbixq
        end if
CC-DEBUGE
CC        set emission source strength.
        qkemss = qemiss(kssndx) * qfract
        if (nvsprt .le. 0) then
          if (densty .le. 0.0) densty = 1.0
          if (IVGET(iqunit(kssndx),2) .eq. 7.and.iswopt(13) .ne. 7) then
CC             input units are in particles, but material is specified
CC             as gaseous, assume a drop diameter of 1 micrometer.
CC             convert to grams.
            qkemss = qkemss * densty * 4.1887902 * 5.0e-5**3
          end if
          if (IVGET(iqunit(kssndx),2) .ne. 7.and.iswopt(13) .eq. 7) then
CC            calculate conversion factor for grams to particles
CC            4.1887902 is 4.0*pi/3.0.
            qkemss = qkemss / (densty * 4.1887902 * 5.0e-5**3)
          end if
        end if
CC        adjust wind direction for grid orientation and save in radians
        dumy = wnddir - grdang
        ctemp9 = dumy * dtorad
CC        cos of wind direction angle.
        tcosth = COS(ctemp9)
CC        sin of wind direction angle.
        tsinth = SIN(ctemp9)
        dumy = ABS(dumy)
        if (ABS(dumy) .le. 0.0) then
          tcosth = 1.0
          tsinth = 0.0
        else if (ABS(dumy-90.0) .le. 0.0) then
          tcosth = 0.0
          tsinth = 1.0
        else if (ABS(dumy-180.0) .le. 0.0) then
          tcosth = -1.0
          tsinth = 0.0
        else if (ABS(dumy-270.0) .le. 0.0) then
          tcosth = 0.0
          tsinth = -1.0
        end if
CC
CC-DEBUGS
        if (ifdbug .gt. 1.and.ifdbug .lt. 7) then                       020797
          WRITE (iotfil,9003) wspmin,qemiss(kssndx),qkemss,wnddir,
     *                        grdang,ctemp9,tcosth,tsinth
        end if
CC-DEBUGE
CC
CC        begin loop over x and y receptor coordinates
CC
        m = 1
        ij = 0
        j = 0
   30   j = j + 1
        if (j .gt. nypnts) go to 50
        yps = ycoord(j)
        i = 0
   40   i = i + 1
        if (i .gt. nxpnts) go to 30
        xps = xcoord(i)
        if (iswopt(6) .le. 1) then
          zpdist = zcoord(1)
        else
          zpdist = zcoord((j-1)*nxpnts+i)
        end if
        if (igrect .eq. 0) then
CC          rectangular coordinate system
          xcord1 = xcoord(i)
          ycord1 = ycoord(j)
          if (jfutms .gt. 0) then
            if (jfutms .ne. 2.or.ifutms .ne. 1) then
              xcord1 = xcord1 + xorgin
              ycord1 = ycord1 + yorgin
            end if
          end if
        else
CC          polar coordinate system
          xcord1 = xcoord(i) * SIN(ycoord(j) * dtorad) + xorgin
          ycord1 = xcoord(i) * COS(ycoord(j) * dtorad) + yorgin
        end if
        go to 70
   50   if (nxypnt .le. 0) go to 90
        m = 2
        i = 0
   60   i = i + 1
        if (i .gt. nxypnt) go to 90
        j = i
        zpdist = zdscrt(i)
        xps = xdscrt(i)
        yps = ydscrt(j)
        if (igrect .eq. 0) then
CC          rectangular coordinate system
          xcord1 = xdscrt(i)
          ycord1 = ydscrt(j)
          if (jfutms .gt. 0) then
            if (jfutms .ne. 2.or.kfutms .ne. 1) then
              xcord1 = xcord1 + xorgin
              ycord1 = ycord1 + yorgin
            end if
          end if
        else
CC          polar coordinate system
          xcord1 = xdscrt(i) * SIN(ydscrt(j) * dtorad) + xorgin
          ycord1 = xdscrt(i) * COS(ydscrt(j) * dtorad) + yorgin
        end if
   70   ij = ij + 1
        xtemp1 = xcord1 - xsl
        ytemp1 = ycord1 - ysl
CC        calculate alongwind (xpdist) and crosswind (ypdist) distance
CC        to receptor.
        xpdist = -xtemp1 * tsinth - ytemp1 * tcosth
        ypdist =  xtemp1 * tcosth - ytemp1 * tsinth
        jtype = IABS(jtype)
CC        if receptor is upwind of source, no calc.
        if (xpdist .le. 0.0) go to 80
CC-DEBUGS
        if (ifdbug .gt. 0.and.ifdbug .lt. 7) then                       020797
          if (m .eq. 1) then
            WRITE (iotfil,9004) i,j,ij,m,xcoord(i),ycoord(j),xpdist,
     *                          ypdist,zpdist,itype,jtype,ifplm,
     *                          xtemp1,ytemp1
          else
            WRITE (iotfil,9005) i,j,ij,m,xdscrt(i),ydscrt(j),xpdist,
     *                          ypdist,zpdist,itype,jtype,ifplm,
     *                          xtemp1,ytemp1
          end if
        end if
CC-DEBUGE
        if (ifctlc .eq. 2) then
          if (ifhrly .eq. 0) ifctlc = 3
          if (jfctc(52) .eq. 1) go to 100
        end if
        if (ifdbug .eq. 26) then                                        020797
          zpdist = ypdist                                               020797
          ypdist = zpdist                                               020797
        end if                                                          020797
        dumy = DISPR(itype,jtype,icalc,ij,ifplm,xps,yps)
CC        end loop over x and y receptor coordinates
   80   go to (40,60),m
   90   if (nlsorc .gt. 0) go to 20
        icpont = 1                                                      110198
CC      End loop over sources
      end if
  100 return
CC
CC-DEBUGS
 9000 format (/' *-*-* entered RMDLB')
 9001 format (' RMDLB - kssndx,nvsprt,ifprlp,ifmxpi,istrec=',5i6/
     *9x,'isrect,iqunit,isotyp,qemiss=',3i5,g13.6/
     *9x,'taucld,relhgt,clddxs=',3g13.6/
     *9x,'clddys,sigxop,sigyop=',3g13.6/
     *9x,'sigzop,xsmlry,xsmlrz=',3g13.6/
     *9x,'xlrgry,xlrgrz,hetcnt=',3g13.6/
     *9x,'brnrat,gammat,alphas=',3g13.6/
     *9x,'betass,cldlng,cldsht=',3g13.6/
     *9x,'clddpt,cldang,a=',3g13.6/
     *9x,'b,c,nlsorc=',2g13.6,i5)                                       073098
 9002 format (' RMDLB 20 - sigmep,sigmap,trbixr=',1p,3e13.6/
     *12x,'sigmeq,sigmaq,trbixq=',3e13.6)
 9003 format (' RMDLB 20+ - wspmin,qemiss(kssndx),qkemss=',3g13.6/
     *13x,'wnddir,grdang,ctemp9=',3g13.6/
     *13x,'tcosth,tsinth=',2g13.6)
 9004 format (' RMDLB 70 - i,j,ij,m,xcoord(i),ycoord(j)=',4i5,2f12.2/
     *12x,'xpdist,ypdist,zpdist,itype,jtype,ifplm=',3f12.2,3i4/
     *12x,'xtemp1,ytemp1=',2f12.2)
 9005 format (' RMDLB 70 - i,j,ij,m,xdscrt(i),ydscrt(j)=',4i6,2f12.2/
     *12x,'xpdist,ypdist,zpdist,itype,jtype,ifplm=',3f12.2,3i4/
     *12x,'xtemp1,ytemp1=',2f12.2)
 9006 format (' RMDLB - inihrs,inidys,inimon,iniyrs=',4i6/              030499
     *9x,'inimin,injdys,ntothr=',3i5)                                   030499
CC-DEBUGE
      end
      Function DISPR(itype,jtype,icalc,ij,ifplm,xpnt,ypnt)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - DISPR Version 01.1                          ---
CC--- Subroutine description - calculate dispersion from volume     ---
CC---                          sources.                             ---
CC---------------------------------------------------------------------
CC
CC      itype  - source emission type - instantaneous=1,
CC               quasi-continuous square wave=2.
CC      jtype  - source type - volume/point=1, line=2.
CC      icalc  - flag returned
CC               0 - dispersion and deposition all zero.
CC               1 - dispersion or deposition > zero.
CC               9 - segment end point exceeded.
CC      ij     - index of receptor point.
CC      ifplm  - cloud rise flag.
CC               0 - no cloud rise.
CC               1 - yes cloud rise.
CC
CC**********************************************************************
CC
CC
      double precision ERFXS,tempa,tempb
CC
      include 'OCNTRL.INC'
      include 'OCDSPM.INC'
      include 'OCALCS.INC'
      include 'ONAMES.INC'
CC
CC**********************************************************************
CC
      DISPR = 0.0
      cldris = 0.0
      cldfct = 1.0
      clddst = 0.0
      timris = 0.0
      factrs = 1.0
CC      the check on if iswopt(6) = 2 was removed for this logic.       112196      
      if (zpdist .gt. hmdpth + hgtmet) then
CC        z > mixing depth plus met. site elevation, no calcs.
        if (kssndx .eq. 1) then
          k = KERRS(12,mnpbuf,idum,idum,zpdist,xpnt,ypnt,
     *              hmdpth+hgtmet,1)
        end if
        go to 30
      end if
CC
CC        wind speed at source height.
      wndspd = AMAX1(PWLAW(refspd,ABS(relhgt(kssndx)),refhgt,pwrlaw),
     *               wspmin)
      if (iswopt(6) .eq. 2) then
        hmats = hmdpth + hgtmet - clddzs(kssndx)
        if (zpdist .ge. hgtmet) then
          hmatz = hmdpth
        else
          hmatz = hmdpth + hgtmet - zpdist
        end if
        zcalc = 0.0
      else
        hmats = hmdpth
        hmatz = hmdpth
        zcalc = zpdist
      end if
CC      plume rise.
      if (ifplm .ne. 0) then
        if (ABS(relhgt(kssndx)) .gt. hmats) then
          hightl = ABS(relhgt(kssndx))
          go to 10
        end if
CC        calculate plume rise at distance xpdist.
        if (itype .ge. 2) then
CC          quasi-continuous.
          if (taucld(kssndx) .gt. 15.0) then
            cldris = PLUMC(xpdist,hetcnt(kssndx),brnrat(kssndx),
     *               sigyop,dphidz,gammat(kssndx))
          else
CC            burn time too short, treat as instantaneous.
            cldris = PLUMI(xpdist,hetcnt(kssndx),brnrat(kssndx),
     *               sigyop,taucld(kssndx),dphidz,iswopt(25),
     *               gammat(kssndx))
          end if
        else
CC          instantaneous sources.
          cldris = PLUMI(xpdist,hetcnt(kssndx),brnrat(kssndx),
     *             sigyop,taucld(kssndx),dphidz,iswopt(25),
     *             gammat(kssndx))
        end if
        hightl = ABS(relhgt(kssndx)) + cldris
        if (cldris .gt. 0.67*(hmats-ABS(relhgt(kssndx)))) then
          if (dphidz .gt. 0.01) then
            cldrsp = cldris
          else
            if (itype .ge. 2) then
CC              quasi-continuous.
              if (taucld(kssndx) .gt. 15.0) then
                cldrsp = PLUMC(xpdist,hetcnt(kssndx),brnrat(kssndx),
     *                   sigyop,0.01,gammat(kssndx))
              else
CC                burn time too short, treat as instantaneous.
                cldrsp = PLUMI(xpdist,hetcnt(kssndx),brnrat(kssndx),
     *                   sigyop,taucld(kssndx),0.01,iswopt(25),
     *                   gammat(kssndx))
              end if
            else
CC              instantaneous sources.
              cldrsp = PLUMI(xpdist,hetcnt(kssndx),brnrat(kssndx),
     *                 sigyop,taucld(kssndx),0.01,iswopt(25),
     *                 gammat(kssndx))
            end if
          end if
          if (hmats-ABS(relhgt(kssndx)) .le. 0.5*cldrsp) then
            cldfct = 1.0
          else if (1.5*cldrsp .le. hmats-ABS(relhgt(kssndx))) then
            cldfct = 0.0
          else
            cldfct = 1.5 - (hmats - ABS(relhgt(kssndx))) / cldrsp
          end if
          cldris = (0.62 + 0.38 * cldfct) * (hmats -
     *              ABS(relhgt(kssndx)))
          cldfct = 1.0 - cldfct
          hightl = ABS(relhgt(kssndx)) + cldris
        end if
        cldmax(kssndx) = AMAX1(cldmax(kssndx), cldris)
        cldmxd(kssndx) = clddst
      else
        hightl = ABS(relhgt(kssndx))
      end if
   10 if (iswopt(6) .eq. 2) then
        hightz = AMAX1(hightl + clddzs(kssndx) - zpdist, 0.0)
      else
        hightz = hightl
      end if
CC
CC      check if source height greater than mixing depth.
      if (hightl .gt. hmats) then
        if (nvsprt .gt. 0) then
CC          calculating deposition, reset height to mixing layer depth.
          hmats = hightl
          hmatz = hightz
        else
CC          source does not contribute.
          go to 30
        end if
      end if
CC-DEBUGS
      if (ifdbug .gt. 1.and.ifdbug .lt. 7) then                         020797
        WRITE (iotfil,9000) cldris,cldfct,cldmxd(kssndx),wndspd,hightl,
     *                      cldrsp,hmdpth,cldmax(kssndx),hmatz,hmats,
     *                      hightz
      end if
CC-DEBUGE
CC
      if (cldris .gt. 0.0) then                                         021098
CC        Change to force dimensions to be equal and equal to sigyop    021098
CC        to comply with derivation of plume rise equations.            021098
CC        adjust alongwind dimension for plume rise.                    021098
        sigxls = sigyop + gammat(kssndx) * cldris / 2.15                021098
CC                                                                      021098
CC        adjust crosswind source dimension for plume rise.             021098
        sigyls = sigyop + gammat(kssndx) * cldris / 2.15                021098
CC                                                                      021098
CC        adjust vertical dimension for plume rise.                     021098
        sigzls = sigyop + gammat(kssndx) * cldris / 2.15                021098
                                                                        021098
      else                                                              021098
CC        adjust alongwind dimension for plume rise.                   
        sigxls = sigxop + gammat(kssndx) * cldris / 2.15               
CC                                                                     
CC        adjust crosswind source dimension for plume rise.            
        sigyls = sigyop + gammat(kssndx) * cldris / 2.15               
CC                                                                     
CC        adjust vertical dimension for plume rise.                    
        sigzls = sigzop + gammat(kssndx) * cldris / 2.15               
      end if                                                            021098
CC
CC        calculate vertical virtual distance, zzdist.
      zzdist = VRXYZ(sigzls,sigmeq,xlrgrz(kssndx),xsmlrz(kssndx),
     *         betadf)
CC        calculate sigz.
      sigzp = SGXYZ(sigmeq,zzdist,xsmlrz(kssndx),betadf,xpdist,0.0)
      if (sigzp .le. 0.0) go to 30
CC
      vss = 0.0
CC
CC        calculate bottom zbots and top ztops of transport layer.
      call TZ1Z2(sigzp,hightl,ztops,zbots,hmdpth,vss,0,betale)
CC
CC        calculate transport wind speed wndsp.
      wndsp = WSBAR(zbots,ztops,refspd,refhgt,pwrlaw,wspmin)
CC
CC        time to distance.
      timev = xpdist / wndsp
CC
CC-DEBUGS
      if (ifdbug .gt. 1.and.ifdbug .lt. 7) then                         020797
        WRITE (iotfil,9001) sigxls,sigyls,sigzls,zzdist,sigzp,zbots,
     *                      ztops,wndsp,timev,cldfct,timris
      end if
CC-DEBUGE
CC
      nvlprt = 0
   20 sigzax = sigzp
      xzdist = zzdist
      timtox = timev
      ztoply = ztops
      zbotly = zbots
      wndspd = wndsp
      if (ifprlp .gt. 0) then
CC
CC        loop over particle categories.  this loop is used only
CC        when betale > 0.0, which requires the need to consider
CC        crossing trajectories. sigma-a becomes a function
CC        of particle size.
        nvlprt = nvlprt + 1
        vss = vspart(nvlprt)
CC
CC        calculate bottom zbotly and top ztoply of transport layer.
        call TZ1Z2(sigzax,hightl,ztoply,zbotly,hmatz,vss,nvsprt,betale)
CC
CC        calculate transport wind speed wndspd.
        wndspd = WSBAR(zbotly,ztoply,refspd,refhgt,pwrlaw,wspmin)
CC
CC        time to distance.
        timtox = xpdist / wndspd
CC-DEBUGS
        if (ifdbug .gt. 1.and.ifdbug .lt. 7) then                       020797
          WRITE (iotfil,9002) nvlprt,vss,zbotly,ztoply,wndspd,timtox
        end if
CC-DEBUGE
      end if
CC      cloud rise time.
      timris = clddst / wndspd
CC
CC
CC        calculate wind direction shear, delthp.
      delthp = DELTD(zbotly,ztoply,dirshr)
CC
CC        calculate wind speed shear.
      spshrc =  DELTU(zbotly,ztoply,refspd,refhgt,pwrlaw,spdshr)
CC
      call DISPS(itype,vss)
      if (sigxax .le. 0.0.or.sigyax .le. 0.0.or.sigzax .le. 0.0)go to 30
CC
      pdepgr = 0.0
CC
CC
      if (nvsprt .gt. 0.and.iswopt(4) .gt. 0) then
CC        calculate gravitational deposition.
        pdepgr = GRVDP(itype,jtype,xpdist,ypdist,sigxax,hightz,hmatz)
        if (iswopt(4) .gt. 0) then
CC          accumulate gravitational deposition.
          if (ACCUM(jjjoff(4)+ij,pdepgr) .gt. 0.0) icalc = 1
        end if
      end if
CC
      pekdos = 0.0
      pekcon = 0.0
      pconav = 0.0
CC
CC
CC        calculate dosage, concentration, time-average concentration.
      if (iswopt(3) .gt. 0) then
        sigar = sigapr                                                  022800
        sigyta = sigyax                                                 022800
CC        Adjust sigma-a, sigy for time-average con. 
        if (timris .lt. timtau.and.timavg .lt. timtau) then
CC          Determine sigma-a for time timavg.
          time = AMAX1(timavg,2.5)
          sigar = PWLAW(sigapr,time,timtas,0.2) * factrs
CC 
CC          Calculate initial xydist virtual distance
          ydist = VRXYZ(sigyls,sigar,xlrgry(kssndx),xsmlry(kssndx),
     *            alphad)
CC 
CC          Calculate sigyax
          sigyta = SGXYZ(sigar,ydist,xsmlry(kssndx),alphad,xpdist,
     *             delthp/4.3)
        end if
      end if
CC-DEBUGS
      if (ifdbug .gt. 0.and.ifdbug .lt. 7) then                         020797
        WRITE (iotfil,9006) wndspd,hightl,xpdist,ypdist,sigyta,sigxax,
     *                      sigyax,sigzax,timtox,timtau,sigar
      end if
CC-DEBUGE
      if (ISUMI(iswopt,3) .gt. 0) then
        if (itype .eq. 1.or.(itype .eq. 2.and.iswopt(1) .eq. 0)) then
CC          type 1 sources or type 2 under special conditions.
          if (itype .eq. 2) then
            timtx = timtox + 0.5 * timtau
          else
            timtx = timtox
          end if
CC          concentration.
          pekcon = CONCE(itype,jtype,xpdist,ypdist,zcalc,timtx,timtau,
     *             sigxax,sigyax,sigzax,hightz,hmatz)
CC          dosage.
          if (itype .eq. 1) then
            pekdos = pekcna * 2.506628 * sigxax / wndspd
          end if
          if (iswopt(3) .gt. 0) then
CC            time-average concentration.
            if (itype .eq. 1) then
              if (IFEQU(sigyax,sigyta) .ne. 0) then
                if (sigyta .gt. 0.0) then
                  pconav = CONCE(itype,jtype,xpdist,ypdist,zcalc,
     *                     timtox,timtau,sigxax,sigyta,sigzax,hightz,
     *                     hmatz)
                else
                  pekcna = 0.0
                end if
              end if
              pconav = pekcna * 2.506628 * sigxax/ (wndspd * timavg)
              tempa = wndspd * timavg / (2.828427 * sigxax)
              alongw = ERFXS(tempa)
              pconav = pconav * alongw
            else if (itype .eq. 2) then
              if (IFEQU(sigyax,sigyta) .ne. 0) then
                if (sigyta .gt. 0.0) then
                  pconav = CONCE(itype,jtype,xpdist,ypdist,zcalc,timtx,
     *                     timavg,sigxax,sigyta,sigzax,hightz,hmatz)
                else
                  pekcna = 0.0
                end if
              end if
              pconav = pekcna
              temp2 = wndspd / (2.828427 * sigxax)
              temp1 = (timtau - timavg) * temp2
              temp2 = (timtau + timavg) * temp2
              tempa = temp1
              tempb = temp2
              arga = (timavg - timtau) * ERFXS(tempa) +
     *               (timavg + timtau) * ERFXS(tempb)
              temp1 = -temp1*temp1
              temp2 = -temp2*temp2
              temp1 = FSEXP(temp1)
              temp2 = FSEXP(temp2)
CC              1.5957691 = 2*SQRT(2)/SQRT(pi)
              argb = -1.5957691 * sigxax * (temp1-temp2) / wndspd
              alongw = AMAX1((arga + argb) / (2.0 * timavg), 0.0)
              pconav = pconav * alongw
CC-DEBUGS
              if (ifdbug .gt. 3.and.ifdbug .lt. 7) then                 020797
                WRITE (iotfil,9008) sigyax,sigyta,pekcna,sigxax,wndspd,
     *                              timavg,taucld(kssndx),temp1,temp2,
     *                              tempa,tempb,arga,argb,alongw,pconav
              end if
CC-DEBUGE
            end if
          end if
        else
CC          type 2 or 3 sources with dosage, intravenous dosage or
CC          vertical deposition. also, type 3 sources with time-average
CC          concentration.
          xdumy = CONCD(itype,jtype,xpdist,ypdist,zcalc,hightz,hmatz)
        end if
        if (ISUMI(iswopt,3) .gt. 0.and.iswopt(17) .ge. 10) then
CC          convert to units of ppm, ppb or ppt.
          xdumy = 1.0e3 * 22.4 * 1013.25 * (273.15 + airtmp) /
     *            (273.15 * airprs * whtmol)
          if (iswopt(17) .ge. 11) xdumy = xdumy * 1.0e3                 070996
          if (iswopt(17) .ge. 12) xdumy = xdumy * 1.0e3                 070996
        else
          xdumy = 1.0
        end if
        if (pconav .gt. pekcon) pconav = pekcon                         022800
        if (iswopt(1) .gt. 0) then
          if (ACCUM(jjjoff(1)+ij,pekdos*xdumy) .gt. 0.0) icalc = 1
        end if
        if (iswopt(2) .gt. 0) then
          if (ACCUM(jjjoff(2)+ij,pekcon*xdumy) .gt. 0.0) icalc = 1
        end if
        if (iswopt(3) .gt. 0) then
          if (ACCUM(jjjoff(3)+ij,pconav*xdumy) .gt. 0.0) icalc = 1
        end if
      end if
CC-DEBUGS
      if (ifdbug .gt. 0.and.ifdbug .lt. 7) then                         020797
        WRITE (iotfil,9007) nvlprt,pekdos,pekcon,
     *                      pconav,pdepgr
      end if
CC-DEBUGE
CC
CC      return to next particle category.
      if (ifprlp .gt. 0.and.nvlprt .lt. nvsprt) go to 20
   30 return
CC
CC-DEBUGS
 9000 format (' DISPR - cldris,cldfct,cldmxd(kssndx)=',3g13.6/
     *9x,'wndspd,hightl,cldrsp=',3g13.6/
     *9x,'hmdpth,cldmax(kssndx),hmatz=',3g13.6/
     *9x,'hmats,hightz=',2g13.6)
 9001 format (' DISPR+- sigxls,sigyls,sigzls=',3g13.6/
     *9x,'zzdist,sigzp,zbots=',3g13.6/
     *9x,'ztops,wndsp,timev=',3g13.6/
     *9x,'cldfct,timris=',2g13.6)
 9002 format (' DISPR 20 - nvlprt,vss,zbotly=',i4,2g13.6/
     *12x,'ztoply,wndspd,timtox=',3g13.6)
 9003 format (' DISPR 20+- ifevp,deltah,fracth=',i4,2g13.6/
     *12x,'hdiff,hightl,nvsprt=',2g13.6,i4)
 9004 format (' DISPR 20+- ifevp,ifpss,hightl=',2i4,g13.6/
     *12x,'timtb,fract,xpdist=',3g13.6)
 9005 format (' DISPR 20+- timtox,xzdist,sigzax=',3g13.6)
 9006 format (' DISPR 20+- wndspd,hightl,xpdist=',3g13.6/
     *12x,'ypdist,sigyta,sigxax=',3g13.6/
     *12x,'sigyax,sigzax,timtox=',3g13.6/
     *12x,'timtau,sigar=',2g13.6)
 9007 format (5x,' DISPR 20+ - nvlprt,pekdos,pekcon=',i4,2g13.6/
     *18x,'pconav,pdepgr=',2g13.6)
 9008 format (5x,' DISPR 20+ - sigyax,sigyta,pekcna=',3g13.6/
     *12x,'sigxax,wndspd,timavg=',3g13.6/
     *12x,'taucld,temp1,temp2=',3g13.6/
     *12x,'tempa,tempb,arga=',2d13.6,g13.6/
     *12x,'argb,alongw,pconav=',3g13.6)
CC-DEBUGE
      end
      Function GRVDP(itype,jtype,xdist,ydist,sigxc,htatz,hmatz)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - GRVDP Version 01.0                          ---
CC--- Subroutine description - calculate gravitational deposition at---
CC---                          distance xdist (g/m**2).             ---
CC---------------------------------------------------------------------
CC
CC       itype  - source emission type.
CC                1 - instantaneous.
CC                2 - quasi-continuous square wave.
CC       jtype  - source type.
CC                1 - volume/point.
CC                2 - line.
CC       xdist  - alongwind distance to receptor, not considering
CC                wind direction shear (m).
CC       ydist  - crosswind distance to receptor, not considering
CC                wind direction shear (m).
CC       sigxc  - std. dev. of the alongwind source distribution (m).
CC       htatz  - source height (m).
CC       hmatz  - mixing layer depth (m).
CC
CC**********************************************************************
CC
      include 'OCNTRL.INC'
      include 'OCDSPM.INC'
      include 'OCALCS.INC'
CC
CC**********************************************************************
CC
      GRVDP = 0.0
CC
CC      adjust alongwind and crosswind distances for wind direction
CC      shear.
      tempa = 0.5 * dirshr * dtorad * htatz
      rcosd = COS(tempa)
      rsind = SIN(tempa)
CC
CC      alongwind distance.
      xsdst = xdist * rcosd + ydist * rsind
CC
CC      crosswind distance.
      ysdst = ydist * rcosd - xdist * rsind
      sigy = SGXYZ(sigapr,xydist,xsmlry(kssndx),alphad,xsdst,
     *       delthp/4.3)
      if (sigy .gt. 0.0) then
        sigz = SGXYZ(sigepr,xzdist,xsmlrz(kssndx),betadf,xsdst,0.0)
        if (sigz .gt. 0.0) then
CC
CC          time to distance.
          timtx = xsdst / wndspd
CC
CC          lateral term.
CC
CC          volume/point source.
          alatrl = YLATR(ysdst,sigy)
CC
CC          depletion due to decay.
          dekayd = DECAA(decays,timtx)
CC
          tempb = xsdst + xzdist - xsmlrz(kssndx) * (1.0 - betadf)
          if (tempb .gt. 0.0) then
CC            source units.
            iqunt = IVGET(iqunit(kssndx),2)
CC
CC            vertical term.
            if (ifprlp .eq. 0) then
              vertgv = VERT3(1,nvsprt,sigz,tempb,betadf,xsdst,wndspd,
     *                 htatz,hmatz,iqunt,densty)
            else
              vertgv = VERT3(nvlprt,nvlprt,sigz,tempb,betadf,xsdst,
     *                 wndspd,htatz,hmatz,iqunt,densty)
            end if
CC
CC          gravitational deposition.
CC            point/volume source.
            GRVDP = qkemss / (6.283185 * sigy * sigz * tempb) *
     *              vertgv * dekayd * alatrl * cldfct
          end if
        end if
      end if
CC
CC-DEBUGS
      if (ifdbug .gt. 0.and.ifdbug .lt. 7) then                         020797
        WRITE (iotfil,9000) itype,jtype,xdist,ydist,sigxc,tempa,rcosd,
     *                      rsind,xsdst,ysdst,sigy,sigz,timtx,wndspd,
     *                      tempb,alatrl,dekayd,vertgv,qkemss,cldfct,
     *                      taucld(kssndx),GRVDP
      end if
CC-DEBUGE
      return
CC
CC-DEBUGS
 9000 format (' GRVDP - itype,jtype,xdist,ydist,sigxc=',2i4,3g13.6/
     *9x,'tempa,rcosd,rsind=',3g13.6/
     *9x,'xsdst,ysdst,sigy=',3g13.6/
     *9x,'sigz,timtx,wndspd=',3g13.6/
     *9x,'tempb,alatrl,dekayd=',3g13.6/
     *9x,'vertgv,qkemss,cldfct=',3g13.6/
     *9x,'taucld(kssndx),grvdp=',2g13.6)
CC-DEBUGE
      end
      Function WSBAR(zbot,ztop,refsp,refht,pwrlw,wsmin)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - WSBAR Version 01.0                          ---
CC--- Subroutine description - compute the transport wind speed for ---
CC---                          the layer zbot to ztop.              ---
CC---------------------------------------------------------------------
CC
CC        zbot   - height of bottom of layer (m).
CC        ztop   - height of top of layer (m).
CC        pwrlw  - power law coefficient.
CC        refsp  - reference wind speed (m/s).
CC        refht  - reference height (m).
CC        wsmin  - minimum wind speed (m/s).
CC
CC**********************************************************************
CC
      if (ztop .le. zbot) then
        WSBAR = wsmin
      else if (pwrlw .le. 0.0) then
        WSBAR = refsp
      else
        WSBAR = AMAX1(refsp * (ztop**(pwrlw + 1) - zbot**(pwrlw + 1)) /
     *          (refht**pwrlw * (ztop - zbot) * (pwrlw + 1.0)) , wsmin)
      end if
      return
      end
      Subroutine DISPS(itype,vss)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - DISPS Version 01.0                          ---
CC--- Subroutine description - calculate dispersion from volume     ---
CC---                          sources.                             ---
CC---------------------------------------------------------------------
CC
CC      itype  - source emission type - instantaneous=1,
CC               quasi-continuous square wave=2.
CC      vss    - particle settling velocity (m/s).
CC
CC**********************************************************************
CC
      include 'OCNTRL.INC'
      include 'OCDSPM.INC'
      include 'OCALCS.INC'
CC
CC**********************************************************************
CC
CC      determine adjustment time for sigma-a
CC      source function time.
      timtau = AMAX1(2.5,taucld(kssndx))
CC      if source is instantaneous.
      if (itype .eq. 1) then
CC        if using cloud rise.
        if (timris .gt. 0.0) then
CC          use time to cloud rise.
          timtas = timris
          timts = timris
        else
CC          use source function time.
          timtas = timtau
          timts = timtau
        end if
      else
CC        source is quasi-continuous.
        if (timris .gt. 0.0) then
          timts = timris
        else
          timts = timtau
        end if
        if (timris .gt. timtau) then
          timtas = timris
        else
          timtas = timtau
        end if
      end if
CC
CC        determine sigma-a for model being used.
      sigatr = PWLAW(sigmaq,timtas,tautmo,0.2)
      sigapr = sigatr
      sigepr = sigmeq
CC
CC        determine instantaneous longitudinal turbulence.
      trbixt = PWLAW(trbixq,timts,tautmo,0.2)
      trbixp = trbixt
CC
CC        calculate crosswind virtual distance.
      xydist = VRXYZ(sigyls,sigatr,xlrgry(kssndx),xsmlry(kssndx),
     *         alphad)
CC
CC        calculate sigyp.
      sigyp = SGXYZ(sigatr,xydist,xsmlry(kssndx),alphad,xpdist,
     *        delthp/4.3)
      sigyax = sigyp
CC
CC        calculate alongwind virtual distance.
      xxdist = VRXYZ(sigxls,trbixt,xlrgry(kssndx),xsmlry(kssndx),1.0)
CC
CC        calculate sigx.
      sigxax = SGXYZ(trbixt,xxdist,xsmlry(kssndx),1.0,xpdist,
     *         0.06*spshrc/wndspd)
CC
CC-DEBUGS
      if (ifdbug .gt. 2.and.ifdbug .lt. 7) then                         020797
        WRITE (iotfil,9000) timtau,timtas,sigatr,trbixt,xydist,sigyp,
     *                      sigyax,xxdist,sigxax
      end if
CC-DEBUGE
      if (ifprlp .gt. 0) then
CC
CC          correct sigmeq, sigz for crossing trajectories.
        sigepr = CRSSE(sigmeq,vss,wndspd,betale)
        if (IFEQU(sigepr,sigmeq) .ne. 0) then
CC
CC          calculate xzdist for corrected sige.
          xzdist = VRXYZ(sigzls,sigepr,xlrgrz(kssndx),xsmlrz(kssndx),
     *             betadf)
CC
CC          calculate corrected sigz.
          sigzax = SGXYZ(sigepr,xzdist,xsmlrz(kssndx),betadf,xpdist,
     *                   0.0)
        end if
CC
CC        correct sigmaq, sigy for crossing trajectories.
        sigapr = CRSSA(sigatr,vss,wndspd,sigmeq,betale,factrs)
CC        correct longitudinal turbulence for crossing trajectories.
        trbixp = trbixt * factrs
CC
CC        calculate initial xydist virtual distance
        xydist = VRXYZ(sigyls,sigapr,xlrgry(kssndx),xsmlry(kssndx),
     *           alphad)
CC
CC        calculate sigyax
        sigyax = SGXYZ(sigapr,xydist,xsmlry(kssndx),alphad,xpdist,
     *           delthp/4.3)
CC
CC        recalculate alongwind virtual distance.
        xxdist = VRXYZ(sigxls,trbixp,xlrgry(kssndx),xsmlry(kssndx),
     *           1.0)
CC
CC        recalculate sigx.
        sigxax = SGXYZ(trbixp,xxdist,xsmlry(kssndx),1.0,xpdist,
     *           0.06*spshrc/wndspd)
      else
        sigapr = sigatr
        sigepr = sigmeq
        trbixp = trbixt
      end if
CC
CC-DEBUGS
      if (ifdbug .gt. 2.and.ifdbug .lt. 7) then                         020797
        WRITE (iotfil,9001) sigepr,xzdist,sigzax,sigapr,trbixp,xydist,
     *                      sigyax,xxdist,sigxax,delthp,spshrc,factrs
      end if
CC-DEBUGE
      return
CC
CC-DEBUGS
 9000 format (' DISPS - timtau,timtas,sigatr=',3g13.6/
     *9x,'trbixt,xydist,sigyp=',3g13.6/
     *9x,'sigyax,xxdist,sigxax=',3g13.6)
 9001 format (' DISPS+- sigepr,xzdist,sigzax=',3g13.6/
     *9x,'sigapr,trbixp,xydist=',3g13.6/
     *9x,'sigyax,xxdist,sigxax=',3g13.6/
     *9x,'delthp,spshrc,factrs=',3g13.6)
CC-DEBUGE
      end
      Function PLUMC(xdist,heat,brnrt,sigyo,dphdz,gamat)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - PLUMC Version 01.1                          ---
CC--- Subroutine description - calculate plume rise for quasi-      ---
CC---                          continuous open sources.             ---
CC---------------------------------------------------------------------
CC
CC      xdist  - alongwind distance to receptor (m).
CC      heat   - heat content of fuel (cal/g).
CC      brnrt  - burn rate of fuel (g/s).
CC      sigyo  - initial standard deviation of the crosswind source
CC               material distribution (m).
CC      dphdz  - vertical potential temperature gradient (deg k/m).
CC      gamat  - air entrainment coefficient.
CC
CC      parameters taken from common
CC
CC      airtmp - ambient air temperature (deg c).
CC      airden - air density (g/cm**3).
CC      hightl - source release height (m).
CC      airhum - air humidity (percent).
CC      airprs - air pressure (mb).
CC      wndspd - wind speed (m/s).
CC      gravty - acceleration of gravity 9.80665 (m/s**2).
CC      spheat - specific heat of air (0.24 cal/g.deg k).
CC      gammac - air entrainment coefficient for volume/line sources.
CC      ifmxpc - flag used to tell whether or not the maximum stable
CC               rise will exceed the maximum adiabatic rise.
CC               0 - unknown, determine if true or false.
CC               1 - maximum stable rise is greater than maximum
CC                   adiabatic rise. when stable rise exceeds adiabatic
CC                   rise, the adiabatic rise is used. also, the
CC                   distance to the maximum adiabatic rise is always
CC                   used.
CC               2 - maximum stable rise is less the maximum adiabatic
CC                   rise.
CC      returns -
CC      PLUMC  - plume rise height (m) at distance xdist.
CC      clddst - returned distance to maximum plume rise (m).
CC
CC**********************************************************************
CC
      include 'OCNTRL.INC'
      include 'OCDSPM.INC'
CC
CC**********************************************************************
CC
CC      stability parameter.
      s = AMAX1(dphdz,3.344e-4) * gravty / (airtmp + 273.15)
      sqrts = SQRT(s)
CC      source radius.
      ro = 2.15 * sigyo
CC      determine buoyancy flux.
CC      open burn.
      airden = AIRDN(airprs,airtmp,airhum)
CC      buoyancy.
      fb = AMAX1(gravty * heat * brnrt / (3.14159265 * spheat *
     *     (airtmp + 273.15) * airden * 1.0e6), 0.0)
CC      air entrainment coefficient.
      if (gamat .le. 0.0) gamat = gammac
CC        determine momentum flux.
CC      no momentum.
      fm = 0.0
      betaj = 1.0
CC      initial vertical velocity
      vrtvl = 0.0
CC
      if (iswopt(8) .le. 1) then
        xdst = 1.0e8
      else
        xdst = xdist
      end if
CC      if stable.
   10 if (dphdz .gt. 0.0) then
CC        stable atmosphere.
        if (ifmxpc .eq. 0) then
CC          set distance for max. rise. calc.
          xdst = 1.0e8
        end if
CC
CC        distance to maximum rise.
        clddst = 3.14159265 * wndspd / sqrts
        if (xdst .gt. clddst) then
          xp = clddst
        else
          xp = xdst
        end if
        a = AMAX1(AMIN1(1.0 - COS(xp * sqrts / wndspd), 2.0), 0.0)
        b = AMAX1(SIN(xp * sqrts / wndspd), 0.0)
        PLUMC = 3.0 * (fb * a / (wndspd * gamat * gamat * s) +
     *          fm * b / (betaj * betaj * wndspd * sqrts))
        if (fb .le. 0.0.and.vrtvl .gt. 0.0) then
          a = 6.0 * vrtvl * ro / wndspd
          if (PLUMC**0.33333333 .gt. a) then
            PLUMC = a * a * a
          end if
        end if
      end if
CC      if adiabatic or if stable and looking for max. rise or max.
CC      stable rise exceeds adiabatic.
      if (dphdz .le. 0.or.(dphdz .gt. 0.0.and.ifmxpc .le. 1)) then
CC        buoyancy.
        if (fb .gt. 0.0) then
          if (fb .gt. 55.0) then
            cldds = 34.0 * fb**0.4
          else
            cldds = 14.0 * fb**0.625
          end if
          cldds = 3.5 * cldds
        else
          cldds = 10.0 * hightl
        end if
        if (xdst .gt. cldds) then
          xp = cldds
        else
          xp = xdst
        end if
        pluma = 3.0 * (fb * xp * xp / (2.0 * gamat * gamat * wndspd *
     *          wndspd * wndspd) + fm * xp / (betaj * betaj * wndspd *
     *          wndspd))
      end if
CC      stable atmosphere.
      if (dphdz .gt. 0.0) then
CC        if looking for max. rise.
        if (ifmxpc .eq. 0) then
CC          if stable rise > adiabatic rise.
          if (PLUMC .gt. pluma) then
            ifmxpc = 1
          else
CC            stable rise < adiabatic rise.
            ifmxpc = 2
          end if
CC          set distance.
          if (iswopt(8) .le. 1) then                                    070996
            xdst = 1.0e8                                                070996
          else                                                          070996
            xdst = xdist
          end if                                                        070996
          go to 10
CC        if stable rise exceeds adiabatic, limit rise.
        else if (ifmxpc .eq. 1) then
          PLUMC = AMIN1(PLUMC,pluma)
          clddst = cldds
        end if
      else
CC        adiabatic atmosphere.
        clddst = cldds
        PLUMC = pluma
      end if
      PLUMC = AMAX1(PLUMC + (ro / gamat)**3, 0.0)
      PLUMC = PLUMC**0.33333333
      PLUMC = AMAX1(PLUMC - ro / gamat, 0.0)
      return
      end
      Function PLUMI(xdist,heat,brnrt,sigyo,tau,dphdz,ifstb,gamat)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - PLUMI Version 01.1                          ---
CC--- Subroutine description - calculate plume rise for instantan-  ---
CC---                          eous sources.
CC---------------------------------------------------------------------
CC
CC      xdist  - alongwind distance to receptor (m).
CC      heat   - heat content of fuel (cal/g).
CC      brnrt  - burn rate of fuel (g/s).
CC      sigyo  - initial standard deviation of the crosswind source
CC               distribution (m).
CC      tau    - source function time (sec).
CC      dphdz  - vertical potential temperature gradient (deg k/m).
CC      ifstb  - option used to either use both stable and adiabatic
CC               plume rise for instantaneous sources or only use
CC               stable rise.
CC               0 - use both stable and adiabatic rise.
CC               1 - use only stable rise.
CC      gamat  - air entrainment coefficient.
CC
CC      parameters taken from common
CC
CC      airtmp - ambient air temperature (deg c).
CC      airden - air density (g/cm**3).
CC      airhum - air humidity (percent).
CC      airprs - air pressure (mb).
CC      wndspd - wind speed (m/s).
CC      gravty - acceleration of gravity 9.80665 (m/s**2).
CC      spheat - specific heat of air (0.24 cal/g.deg k).
CC      gammai - air entrainment coefficient.
CC      ifmxpi - flag used to tell whether or not the maximum stable
CC               rise will exceed the maximum adiabatic rise.
CC               0 - unknown, determine if true or false.
CC               1 - maximum stable rise is greater than maximum
CC                   adiabatic rise. when stable rise exceeds adiabatic
CC                   rise, the adiabatic rise is used. also, the
CC                   distance to the maximum adiabatic rise is always
CC                   used.
CC               2 - maximum stable rise is less than or equal to
CC                   maximum adiabatic rise, maximum adiabatic rise
CC                   is ignored for stable rise calculations.
CC
CC      returns -
CC      PLUMI  - plume rise height (m) at distance xdist.
CC      clddst - returned distance to maximum plume rise (m).
CC
CC**********************************************************************
CC
      include 'OCNTRL.INC'
      include 'OCDSPM.INC'
CC
CC**********************************************************************
CC
CC      source radius
      r0 = sigyo * 2.15
CC
CC      determine buoyancy flux.
      airden = AIRDN(airprs,airtmp,airhum)
      fi = 3.0 * gravty * heat * brnrt * tau / (4.0 * 3.14159265 *
     *     spheat * (airtmp + 273.15) * airden * 1.0e6)
      if (gamat .le. 0.0) gamat = gammai
      if (iswopt(8) .le. 1) then
        xdst = 1.0e8
      else
        xdst = xdist
      end if
CC-DEBUGS
      if (ifdbug .gt. 2.and.ifdbug .lt. 7) then                         020797
        WRITE (iotfil,9000) sigyo,r0,airprs,airtmp,airhum,airden,
     *                      gravty,heat,brnrt,tau,spheat,gamat,
     *                      xdist,xdst,dphdz,fi,ifstb,ifmxpi
      end if
CC-DEBUGE
CC
CC      if a stable atmosphere or if forced to use stable algorithms.
   10 if (dphdz .gt. 0.0.or.ifstb .ne. 0) then
CC        if looking for maximum rise.
        if (ifmxpi .eq. 0) then
CC          distance to check for maximum plume rise.
          xdst = 1.0e8
        end if
CC        stable conditions or adiabatic forced to stable rise.
        s = AMAX1(dphdz,3.344e-4) * gravty / (airtmp + 273.15)
        sqrts = SQRT(s)
CC        distance to maximum plume rise.
        clddst = 3.14159326 * wndspd / sqrts
CC
        if (xdst .gt. clddst) then
          xp = clddst
        else
          xp = xdst
        end if
CC
CC        stable atmosphere.
        PLUMI = 4.0 * fi * AMAX1(AMIN1(1.0 - COS(sqrts * xp / wndspd),
     *          2.0), 0.0) / (s * gamat**3)
CC-DEBUGS
        if (ifdbug .gt. 2.and.ifdbug .lt. 7) then                       020797
          WRITE (iotfil,9001) s,sqrts,clddst,xp,wndspd,PLUMI,ifmxpi
        end if
CC-DEBUGE
      end if
CC      if both stable and adiabatic option selected.
      if (ifstb .eq. 0) then
CC        if adiabatic or if stable and need to check for max. stable
CC        rise or limit max. stable rise)
        if (dphdz .le. 0.0.or.(dphdz .gt. 0.0.and.ifmxpi .le. 1)) then
CC          unstable/neutral conditions.
          if (fi .le. 300.0 * wndspd**0.6666667) then
CC            distance to maximum adiabatic rise.
            cldds = 12.0 * SQRT(fi) * wndspd**0.3333333
          else
            cldds = 50.0 * fi**0.25 * SQRT(wndspd)
          end if
          if (xdst .gt. cldds) then
            xp = cldds
          else
            xp = xdst
          end if
CC
          pluma = 2.0 * fi * xp * xp / (gamat**3 * wndspd * wndspd)
CC-DEBUGS
          if (ifdbug .gt. 2.and.ifdbug .lt. 7) then                     020797
            WRITE (iotfil,9002) cldds,xdst,xp,pluma,ifmxpi
          end if
CC-DEBUGE
        end if
CC        if stable.
        if (dphdz .gt. 0.0) then
CC          if looking for max. stable rise.
          if (ifmxpi .eq. 0) then
            if (PLUMI .gt. pluma) then
CC              stable rise > adiabatic rise.
              ifmxpi = 1
            else
CC              stable rise < adiabatic rise.
              ifmxpi = 2
            end if
CC            distance to max. rise.
            if (iswopt(8) .le. 1) then                                  070996
              xdst = 1.0e8                                              070996
            else                                                        070996
              xdst = xdist
            end if                                                      070996
            go to 10
          else if (ifmxpi .eq. 1) then
CC            limit max. rise and set max. distance.
            clddst = cldds
            PLUMI = AMIN1(PLUMI,pluma)
          end if
        else
CC          adiabatic rise and distance.
          PLUMI = pluma
          clddst = cldds
        end if
      end if
      PLUMI = AMAX1((PLUMI + (r0 / gamat)**4)**0.25 - r0 / gamat, 0.0)
CC-DEBUGS
      if (ifdbug .gt. 2.and.ifdbug .lt. 7) then                         020797
        WRITE (iotfil,9003) PLUMI
      end if
CC-DEBUGE
      return
CC-DEBUGS
 9000 format (' PLUMI - sigyo,r0,airprs=',3g13.6/
     *9x,'airtmp,airhum,airden=',3g13.6/
     *9x,'gravty,heat,brnrt=',3g13.6/
     *9x,'tau,spheat,gamat=',3g13.6/
     *9x,'xdist,xdst,dphdz=',3g13.6/
     *9x,'fi,ifstb,ifmxpi=',g13.6,2i4)
 9001 format (' PLUMI - s,sqrts,clddst=',3g13.6/
     *9x,'xp,wndspd,plumi=',3g13.6/
     *9x,'ifmxpi=',i4)
 9002 format (' PLUMI - cldds,xdst,xp=',3g13.6/
     *9x,'pluma,ifmxpi=',g13.6,i5)
 9003 format (' PLUMI - plumi=',g13.6)
CC-DEBUGE
      end
      Function CONCD(itype,jtype,xdist,ydist,zdist,htatz,hmatz)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - CONCD Version 01.0                          ---
CC--- Subroutine description - calculate concentration or dosage    ---
CC---                          from quasi-continuous sources.       ---
CC---------------------------------------------------------------------
CC
CC      itype   - source emission type.
CC                1 - instantaneous.
CC                2 - quasi-continuous square wave.
CC      jtype   - source type.
CC                1 - volume/point.
CC                2 - line.
CC      xdist   - alongwind distance to receptor (m).
CC      ydist   - crosswind distance to receptor (m).
CC      zdist   - height of receptor (m).
CC      htatz   - source height (m).
CC      hmatz   - mixing layer depth (m).
CC
CC**********************************************************************
CC
      integer*2 ncon,ic1,ic2
CC
      include 'OCNTRL.INC'
      include 'OCDSPM.INC'
      include 'OCALCS.INC'
CC
CC**********************************************************************
CC
      CONCD = 0.0
CC
      if (iswopt(1) .gt. 0.or.iswopt(3) .gt. 0) then
        sigs = 4.0
      else
        sigs = 2.45
      end if
      dtime = sigs * sigxax / wndspd
CC      if quasi-continuous square wave.
      if (itype .eq. 2) then
        dtime = dtime + taucld(kssndx)
      end if
      t2 = timtox + dtime
      t1 = sigs * sigxax / wndspd
      t1 = AMAX1(xdist / wndspd - t1, 0.1)
      if (t1 .lt. 0.0) t1 = 0.1
      if (t1 .gt. t2) t1 = 0.1 * t2
      if (iswopt(3) .gt. 0) then
        ta1 = AMAX1(timtox - 0.5 * timavg + 0.5 * taucld(kssndx), 0.1)  022800
        ta2 = timtox + 0.5 * timavg + 0.5 * taucld(kssndx)              022800
      else
        ta1 = t1
        ta2 = t2
      end if
      ts = AMIN1(t1,ta1)
      te = AMAX1(t2,ta2)
      avcon = 0.0
      ncon = 0
      pekcon = rmissd
      pconav = 0.0
      cona = 0.0
      ic1 = -1
      pc1 = 0.0
      pc2 = 0.0
      pc3 = 0.0
      if (timinv .le. 0.0) then
        deltm = (te - ts) * 1.0e-2
      else
        deltm = timinv
      end if
CC-DEBUGS
      if (ifdbug .gt. 2.and.ifdbug .lt. 7) then                         020797
        WRITE (iotfil,9000) dtime,sigxax,wndspd,xdist,timtox,
     *                      timavg,timinv,t1,t2,ta1,ta2,ts,te,deltm
      end if
CC-DEBUGE
      timp = ts - deltm
   10 timp = timp + deltm
CC-DEBUGS
        if (ifdbug .gt. 0.and.ifdbug .lt. 7) then                       020797
          WRITE (iotfil,9001) timp
        end if
CC-DEBUGE
        con = CONCE(itype,jtype,xdist,ydist,zdist,timp,timtau,sigxax,
     *              sigyax,sigzax,htatz,hmatz)
        cona = con
        if (iswopt(3) .gt. 0.and.timp .ge. ta1.and.timp .le. ta2)
     *      then
          sigar = sigmaq
          sigy = sigyax                                                 022800
          if (timris .lt. timtau.and.timavg .lt. timtau) then
CC            See if need to modify sigar, sigy and sigx.
            sigar = PWLAW(sigmaq,timavg,timtas,0.2) * factrs
            xydst = VRXYZ(sigyls,sigar,xlrgry(kssndx),xsmlry(kssndx),
     *              alphad)
            sigy = SGXYZ(sigar,xydst,xsmlry(kssndx),alphad,xdist,
     *             delthp/4.3)
          end if
          if (sigy .gt. 0.0) then
            cona = CONCE(itype,jtype,xdist,ydist,zdist,timp,timtau,
     *                   sigxax,sigy,sigzax,htatz,hmatz)
          else
            cona = 0.0
          end if
        else
          cona = 0.0
        end if

        ic1 = ic1 + 1
        if (ic1 .eq. 0) then
          pc1 = con
        else if (MOD(ic1,2) .eq. 0) then
          pc2 = pc2 + con
          ic2 = 0
        else
          pc3 = pc3 + con
          ic2 = 1
        end if
        pekcon = AMAX1(con,pekcon)
CC-DEBUGS
        if (ifdbug .gt. 3.and.ifdbug .lt. 7) then                       020797
          WRITE (iotfil,9002) ic1,ic2,pc1,pc2,pc3,con,pekcon,sigar,sigy,022800
     *                        sigmaq,cona                               022800
        end if
CC-DEBUGE
        if (timp .ge. ta1.and.timp .le. ta2) then
          avcon = avcon + cona
          ncon = ncon + 1
CC-DEBUGS
          if (ifdbug .gt. 3.and.ifdbug .lt. 7) then                     020797
            WRITE (iotfil,9003) ncon,avcon
          end if
CC-DEBUGE
        end if
        if (timp .le. te) then
          go to 10
        end if
CC        accumulate concentration for quasi-continuous source
   20 CONCD = AMAX1(pekcon,CONCD)
      if (ic2 .eq. 0) then
        pc2 = pc2 - con
      end if
      pc1 = pc1 + con
      pekdos = deltm * (pc1 + 4.0 * pc3 + 2.0 * pc2) * 3.333333e-1
      if (ncon .gt. 0) pconav = avcon / FLOAT(ncon)
      if (pconav .gt. pekcon) pconav = pekcon                           022800
CC-DEBUGS
      if (ifdbug .gt. 0.and.ifdbug .lt. 7) then                         020797
        WRITE (iotfil,9004) ncon,ic2,CONCD,pc1,pc2,pc3,deltm,wndspd,
     *                      avcon,con,pekdos,pconav,CONCD
      end if
CC-DEBUGE
      return
CC
CC-DEBUGS
 9000 format (' CONCD - dtime,sigxax,wndspd=',3g13.6/
     *9x,'xdist,timtox,timavg=',3g13.6/
     *9x,'timinv,t1,t2=',3g13.6/
     *9x,'ta1,ta2,ts=',3g13.6/
     *9x,'te,deltm=',2g13.6)
 9001 format (' CONCD - timp=',g13.6)
 9002 format (' CONCD - ic1,ic2,pc1,pc2,pc3=',2i5,3g13.6/
     *9x,'con,pekcon,sigar,sigy=',4g13.6/                               022800
     *9x,'sigmaq,cona=',2g13.6)                                         022800
 9003 format (' CONCD - ncon,avcon=',i5,g13.6)
 9004 format (' CONCD - ncon,ic2,CONCD,pc1=',2i5,2g13.6/
     *9x,'pc2,pc3,deltm=',3g13.6/
     *9x,'wndspd,avcon,con=',3g13.6/
     *9x,'pekdos,pconav,concd=',3g13.6)
CC-DEBUGE
      end
      Subroutine TZ1Z2(sigz,hp,ztop,zbot,hm,vss,nvs,betal)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - TZ1Z2 Version 01.0                          ---
CC--- Subroutine description - calculate the top and bottom heights ---
CC---                          of the transport layer.              ---
CC---------------------------------------------------------------------
CC
CC      sigz   - std. dev. vertical source distribution (m).
CC      hp     - source height (m).
CC      ztop   - returned top of transport layer (m).
CC      zbot   - returned bottom of transport layer (m).
CC      hm     - mixing layer depth (m).
CC      vss    - settling velocity of particle, if nvs > 0 (m/s).
CC      nvs    - number of particle size categories, if > 0.
CC      betal  - ratio of lagrangian to eulerian time-scales used in the
CC               correction factor on sigmep and sigmap for crossing
CC               trajectory effects of heavy particles.
CC
CC**********************************************************************
CC
      tempa = 2.15 * sigz
      zbot = AMAX1(hp - tempa,2.0)
      if (nvs .le. 0) then
        ztop = AMIN1(hp + tempa,hm)
      else
        if (betal .gt. 0.0.and.vss .gt. 0.0) then
          ztop = AMIN1(hp,hm)
        else
          ztop = AMIN1(hp + tempa,hm)
        end if
      end if
      return
      end
      Function VRXYZ(sxyzo,sigea,xlrgr,xsmlr,bealf)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - VRXYZ Version 01.0                          ---
CC--- Subroutine description - calculate virtual distance to source ---
CC---                          for the x, y and z dimensions (m)    ---
CC---------------------------------------------------------------------
CC
CC        sxyzo  - initial alongwind, crosswind or vertical std. dev.
CC                 of the source material distribution (m).
CC        sigea  - std. dev. of the alongwind, crosswind or vertical
CC                 wind angle (radians).
CC        xlrgr  - reference distance to sxyzo (m).
CC        xsmlr  - distance from the virtual point source overwhich
CC                 rectilinear expansion occurs (m) in the x, y or z
CC                 dimension.
CC        bealf  - cloud expansion coefficient for the x, y or z
CC                 dimension.
CC
CC**********************************************************************
CC
CC      calculate x, y or z virtual distance.
      if (sxyzo .le. sigea * xsmlr) then
        VRXYZ = AMAX1(sxyzo / sigea - xlrgr , 0.0)
      else
        VRXYZ = AMAX1(bealf * xsmlr * (sxyzo / (sigea * xsmlr))**
     *          (1.0 / bealf) - xlrgr + xsmlr * (1.0 - bealf) , 0.0)
      end if
      return
      end
      Function VERT1(dh1,dh2,dh3,dh4,sigz,gama,ifbox,h,hm)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - VERT1 Version 01.0                          ---
CC--- Subroutine description - calculate the vertical term for gases---
CC---------------------------------------------------------------------
CC
CC        dh1    - -(h-z)  where h is the effective source height
CC        dh2    - (h+z)   and z is the receptor height (m).
CC        dh3    - (h-z)
CC        dh4    - -(h+z)
CC        sigz   - std. dev. source distribution (m).
CC        gama   - surface reflection coefficient (0 to 1).
CC        ifbox  - box model flag.
CC                 0 - cannot use box model.
CC                 1 - can use box model.
CC        h      - effective source height (m).
CC        hm     - mixing layer depth (m).
CC
CC**********************************************************************
CC
CC        calc vertical term for dos & con, with result returned in
CC        VERT1.
      VERT1 = 0.0
      if (sigz .gt. 0.0) then
        sigzi = -0.5 / (sigz * sigz)
        tmp1 = dh2 * dh2 * sigzi
        VERT1 = gama * FSEXP(tmp1)
        tmp2 = dh1 * dh1 * sigzi
        VERT1 = VERT1 + FSEXP(tmp2)
        tmp1 = 1.0
        tmp2 = gama
        tmp3 = tmp2 * tmp2
        tmp4 = 0.0
   10   tmp4 = tmp4 + 2.0
        if (ifbox .eq. 0.or.tmp4 .le. 6.0) then
          if (h .le. hm) then
            tmp5 = tmp4 * hm
          else
            tmp5 = tmp4 * h
          end if
          tmp6 = tmp5 + dh4
          tmp6 = tmp6 * tmp6 * sigzi
          if ((tmp4 .le. 2.0.and.tmp6 .gt. -30.0).or.
     *        tmp6 .ge. -10.0) then
            tmp7 = tmp5 + dh1
            tmp8 = tmp5 + dh3
            tmp9 = tmp5 + dh2
            tmp7 = tmp7 * tmp7 * sigzi
            tmp8 = tmp8 * tmp8 * sigzi
            tmp9 = tmp9 * tmp9 * sigzi
            VERT1 = VERT1 + tmp1 * FSEXP(tmp6) + tmp2 *
     *              (FSEXP(tmp7) + FSEXP(tmp8)) + tmp3 *
     *              FSEXP(tmp9)
            tmp1 = tmp2
            tmp2 = tmp3
            tmp3 = tmp3 * gama
            go to 10
          end if
        else
          VERT1 = 2.506628 * sigz / hm
        end if
      end if
      return
      end
      Function VERT2(n1,n2,dh1,dh2,sigz,h,hm,timx,iqunt,dnsty)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - VERT2 Version 01.0                          ---
CC--- Subroutine description - calculate the vertical term for      ---
CC---                          particle categories n1 to n2.        ---
CC---------------------------------------------------------------------
CC
CC        n1     - starting particle category.
CC        n2     - ending particle category.
CC        dh1    - (h-z)  where h is the effective source height
CC        dh2    - (h+z)  and z is the receptor height (m).
CC        sigz   - std. dev. source distribution (m).
CC        h      - effective source height (m).
CC        hm     - mixing layer depth (m).
CC        pct    - array of particle mass fractions (0 to 1).
CC        timx   - time to receptor distance (s).
CC        iqunt  - source units
CC                 0 - grams
CC                 1 - kilograms
CC                 2 - pounds
CC                 3 - ounces
CC                 4 - tons
CC                 5 - milligrams
CC                 6 - micrograms
CC                 7 - particles
CC        dnsty  - particle density (g/cm**3).
CC
CC**********************************************************************
CC
      include 'OCDSPM.INC'
      include 'OCNTRL.INC'
CC
CC**********************************************************************
CC
CC        calc vertical term for deposition, result returned in VERT2.
      VERT2 = 0.0
      do 10 nv=n1,n2
        gama = FGAMA(nv)
        vs = vspart(nv)
        hp = vs * timx
        p1 = -dh1 + hp
        p2 = dh2 - hp
        p3 = -p1
        p4 = -p2
        tmp1 = pctmat(nv) * VERT1(p1,p2,p3,p4,sigz,gama,0,h,hm)
        if ((iqunt .eq. 7.and.iswopt(13) .ne. 7).or.
     *      (iqunt .ne. 7.and.iswopt(13) .eq. 7)) then
CC          calculate average radius in cm.
          ravg = DRPAV(drpupr(nv) * 5.0e-5, drplwr(nv) * 5.0e-5, 0.1)
          if (iqunt .eq. 7) then
CC            convert from particles to grams (4.1887902 is 4.0*pi/3.0)
            tmp1 = tmp1 * dnsty * 4.1887902 * ravg**3
          else
CC            convert from grams to particles
            tmp1 = tmp1 / (dnsty * 4.1887902 * ravg**3)
          end if
        end if
        VERT2 = VERT2 + tmp1
   10 continue
      return
      end
      Function DECAA(decay,time)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - DECAA Version 01.0                          ---
CC--- Subroutine description - calculate decay due to physical or   ---
CC---                          chemical processes depletion term.   ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      if (decay .gt. 0.0) then
        DECAA = FSEXP(-decay * time)
      else
        DECAA = 1.0
      end if
      return
      end
      Function YLATR(yy,sigy)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - YLATR Version 01.0                          ---
CC--- Subroutine description - calculate the lateral term for the   ---
CC---                          dispersion equation.                 ---
CC---------------------------------------------------------------------
CC
CC      yy     - lateral distance from cloud centerline (m).
CC      sigy   - standard deviation of the cloud material distribution
CC               (m).
CC
CC**********************************************************************
CC
      YLATR = FSEXP(-0.5 * yy * yy / (sigy * sigy))
      return
      end
      Function CRSSA(sigat,vss,wndsp,sigme,betal,factr)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - CRSSA Version 01.0                          ---
CC--- Subroutine description - correct sigma-a for crossing         ---
CC---                          trajectories.                        ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
CC        correct siga for crossing trajectories.
      tempa = vss / wndsp
      tmps = tempa / sigme
      if (tmps .gt. 0.2386) then
        if (betal .gt. 0.0) then
          atemp7 = betal / sigme
        else
          atemp7 = 0.0
        end if
        tempb = (atemp7 * tempa)**2
        factr = 1.0 / (1.0 + 4.0 * tempb)**0.25
        CRSSA = sigat * factr
      else
        CRSSA = sigat
        factr = 1.0
      end if
      return
      end
      Function SGXYZ(sigea,vrdst,xsmr,bealf,xdist,del)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - SGXYZ Version 01.0                          ---
CC--- Subroutine description - calculate standard deviation of      ---
CC---                          alongwind, crosswind or vertical     ---
CC---                          source material distribution at      ---
CC---                          distance xdist (m).                  ---
CC---------------------------------------------------------------------
CC
CC        sigea  - standard deviation of the alongwind, crosswind
CC                 or vertical wind angle (radians).
CC        vrdst  - alongwind, crosswind or vertical virtual dist. (m).
CC        xsmr   - distance from the virtual point source overwhich
CC                 rectilinear expansion occurs (m).
CC        bealf  - cloud expansion coefficient.
CC        xdist  - alongwind distance (m).
CC        del    - wind shear term.
CC               - for sigx use: 0.06 * wind speed shear / wind speed
CC               - for sigy use: wind direction shear / 4.3
CC               - for sigz use: 0.0
CC
CC**********************************************************************
CC
CC      calculate sigx, sigy or sigz for non-area sources.
      xp = (xdist + vrdst - xsmr * (1.0 - bealf)) / (bealf * xsmr)
      if (xp .gt. 0.0) then
        SGXYZ = SQRT((sigea * xsmr * xp**bealf)**2 + (del * xdist)**2)
      else
        SGXYZ = 0.0
      end if
      return
      end
      Function DELTU(zbot,ztop,refsp,refht,pwrlw,spdsh)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - DELTU Version 01.0                          ---
CC--- Subroutine description - wind speed shear over layer from     ---
CC---                          zbot to ztop (m/s).                  ---
CC---------------------------------------------------------------------
CC
CC        pwrlw  - power law coefficient.
CC        refsp  - reference wind speed (m/s).
CC        refht  - reference height (m).
CC        zbot   - height at bottom of layer (m).
CC        ztop   - height at top of layer (m).
CC        spdsh  - preset wind speed shear (m/s).
CC
CC********************************************************************
CC
      if (spdsh .gt. 0.0) then
        DELTU = spdsh
      else
        if (pwrlw .le. 0.0.or.ztop .le. zbot) then
          DELTU = 0.0
        else
          DELTU = ABS(refsp * (ztop**pwrlw - zbot**pwrlw) /
     *            (refht**pwrlw) )
        end if
      end if
      return
      end
      Function CONCE(itype,jtype,xdist,ydist,zdist,timtx,timtc,sigxc,
     *               sigyc,sigzc,htatz,hmatz)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - CONCE Version 01.0                          ---
CC--- Subroutine description - calculate concentration at distance  ---
CC---                          xdist (g/m**3).                      ---
CC---------------------------------------------------------------------
CC
CC      itype   - source emission type.
CC                1 - instantaneous.
CC                2 - quasi-continuous square wave.
CC                3 - quasi-continuous exponential wave.
CC      jtype   - source type.
CC                1 - volume/point.
CC                2 - line.
CC      xdist   - alongwind distance to receptor (m).
CC      ydist   - crosswind distance to receptor (m).
CC      zdist   - height of receptor (m).
CC      timtx   - cloud travel time (s).
CC      timtc   - source function time (s).
CC      sigxc   - std. dev. alongwind source distribution (m).
CC      sigyc   - std. dev. crosswind source distribution (m).
CC      sigzc   - std. dev. vertical source distribution (m).
CC      htatz   - source height (m).
CC      hmatz   - mixing layer depth (m).
CC
CC**********************************************************************
CC
      double precision ERFXF,tempb
CC
      include 'OCNTRL.INC'
      include 'OCDSPM.INC'
      include 'OCALCS.INC'
CC
CC**********************************************************************
CC
CC-DEBUGS
      peak = 0.0
CC-DEBUGE
      CONCE = 0.0
      if (htatz .le. hmatz) then
CC
CC
CC        calculate vertical term for concentration.
        hight = htatz
        fhght1 = hight - zdist
        fhght2 = hight + zdist
        if (nvsprt .le. 0) then
CC          non particulate vertical term.
          vertcl = VERT1(-fhght1,fhght2,fhght1,-fhght2,sigzc,1.0,1,
     *             hight,hmatz)
        else
          iqunt = IVGET(iqunit(kssndx),2)
          if (ifprlp .eq. 0) then
CC
CC            particle vertical term loops over all categories.
            vertcl = VERT2(1,nvsprt,fhght1,fhght2,sigzc,hight,hmatz,
     *               timtox,iqunt,densty)
          else
CC
CC            particle vertical term for category nvlprt.
            vertcl = VERT2(nvlprt,nvlprt,fhght1,fhght2,sigzc,hight,
     *               hmatz,timtox,iqunt,densty)
          end if
        end if
CC
CC        calculate lateral term for concentration.
CC
CC        volume/point source.
        alatrl = YLATR(ydist,sigyc)
CC
CC        calculate depletion term for decay.
        dekayd = DECAA(decays,timtx)
CC
        if (itype .eq. 1) then
CC
CC          instantaneous source.
CC
CC          15.74961 = (2*pi)**(3/2)
          CONCE = qkemss / (15.74961 * sigyc * sigzc * sigxc)
          alongw = FSEXP(-0.5 * ((xdist - wndspd * timtx) / sigxc)**2)
CC
        else if (itype .eq. 2) then
CC
CC          quasi-continuous square wave source.
CC
          CONCE = qkemss / (6.283185 * sigyc * sigzc * wndspd *
     *            taucld(kssndx))
          tempb = xdist
          alongw = ERFXF(tempb,sigxc,wndspd,timtc,timtx)
CC
        end if
CC-DEBUGS
        peak = CONCE
CC-DEBUGE
CC
CC        concentration.
        pekcna = CONCE * alatrl * cldfct * dekayd * vertcl
        CONCE = pekcna * alongw
CC      CONCE = CONCE * vertcl * alatrl * cldfct * dekayd * alongw
      end if
CC-DEBUGS
      if (ifdbug .gt. 0.and.ifdbug .lt. 7) then                         020797
        if (ifdbug .gt. 1) then
          WRITE (iotfil,9000) fhght1,fhght2,htatz,hmatz,sigepr
        end if
        WRITE (iotfil,9001) itype,jtype,xdist,ydist,zdist,timtx,timtc,
     *                      sigxc,sigyc,sigzc,wndspd,qkemss,peak,
     *                      vertcl,alatrl,cldfct,dekayd,alongw,CONCE
      end if
CC-DEBUGE
      if (CONCE .lt. 0.0) CONCE = 0.0
      return
CC
CC-DEBUGS
 9000 format (' CONCE - fhght1,fhght2,htatz=',3g13.6/
     *9x,'hmatz,sigepr=',2g13.6)
 9001 format (' CONCE - itype,jtype,xdist,ydist=',2i4,2g13.6/
     *9x,'zdist,timtx,timtc=',3g13.6/
     *9x,'sigxc,sigyc,sigzc=',3g13.6/
     *9x,'wndspd,qkemss,peak=',3g13.6/
     *9x,'vertcl,alatrl,cldfct=',3g13.6/
     *9x,'dekayd,alongw,CONCE=',3g13.6)
CC-DEBUGE
      end
      Function VERT3(n1,n2,sigz,xp,betd,xdist,wndsp,h,hm,iqunt,dnsty)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - VERT3 Version 01.0                          ---
CC--- Subroutine description - calculate vertical term for          ---
CC---                          gravitational deposition.            ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
CC        n1       - particle starting category.
CC        n2       - ending particle category.
CC        sigz     - std. dev. source distribution (m)
CC        xp       - xdist + xz - xsm * (1.0 - betd)
CC                   where xz is the vertical virtual distance (m)
CC                   and xsm is the distance from the virtual point
CC                   source over which rectilinear expansion in the
CC                   vertical occurs.
CC        betd     - vertical cloud expansion coefficient.
CC        xdist    - distance to receptor (m).
CC        wndsp    - wind speed (m/s).
CC        h        - effective release height (m).
CC        hm       - mixing layer depth (m).
CC        iqunt  - source units
CC                 0 - grams
CC                 1 - kilograms
CC                 2 - pounds
CC                 3 - ounces
CC                 4 - tons
CC                 5 - milligrams
CC                 6 - micrograms
CC                 7 - particles
CC        dnsty    - particle density (g/cm**3).
CC
CC**********************************************************************
CC
      include 'OCDSPM.INC'
      include 'OCNTRL.INC'
CC
CC**********************************************************************
CC
      VERT3 = 0.0
      sigzi = -0.5 / (sigz * sigz)
      t = xdist / wndsp
      f = betd * xdist
      do 20 nv=n1,n2
        gama = FGAMA(nv)
        vs = vspart(nv)
        hp = h - vs * t
        fp = vs * (xp - f) / wndsp
        if (gama .lt. 1.0) then
          vm = (fp + betd * h) * FSEXP(-0.5 * (hp / sigz)**2)
          vn = 0.0
          a = 0.0
          c = 1.0
   10     a = a + 2.0
          if (h .le. hm) then
            b = a * hm
          else
            b = a * h
          end if
          d = b - hp
          e = b + hp
          d = d * d * sigzi
          e = e * e * sigzi
          if (a .le. 2.0.or.d .ge. -10.0.or.e .ge. -10.0) then
            vn = vn + c * ((betd * (b - h) - fp) * FSEXP(d) + gama *
     *           (betd * (b + h) + fp) * FSEXP(e))
            if (gama .gt. 0.0) then
              c = c * gama
              go to 10
            end if
          end if
CC          pctmat gives the fraction of material in category nv.
          a = pctmat(nv) * (vm + vn) * (1.0 - gama)
          if ((iqunt .eq. 7.and.iswopt(13) .ne. 7).or.
     *        (iqunt .ne. 7.and.iswopt(13) .eq. 7)) then
CC            calculate average radius in cm.
            ravg = DRPAV(drpupr(nv) * 5.0e-5, drplwr(nv) * 5.0e-5, 0.1)
            if (iqunt .eq. 7) then
CC              convert from particles to grams (4.1887902 is 4.0*pi/
CC              3.0)
              a = a * dnsty * 4.1887902 * ravg**3
            else
CC              convert from grams to particles
              a = a / (dnsty * 4.1887902 * ravg**3)
            end if
          end if
          VERT3 = VERT3 + a
        end if
   20 continue
      VERT3 = AMAX1(VERT3,0.0)
      return
      end
      Function ERFXF(x,sigx,wndsp,tau,tm)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - ERFXF Version 01.0                          ---
CC--- Subroutine description - calculate the gaussian error function---
CC---------------------------------------------------------------------
CC
CC       if tm < 0 then
CC           ERFXF = 0.5 * ERFXS(x)
CC       if tm >= 0 then
CC           ERFXF = 0.5 * (ERFXS[(x-wndsp*(tm-tau))/(SQRT(2)*sigx)] -
CC                          ERFXS[(x-wndsp*tm)/(SQRT(2)*sigx)])
CC
CC**********************************************************************
CC
      double precision ERFXF,ERFXS,c,g,x,a
CC
CC**********************************************************************
CC
      d = 1.0
      g = 0.0
      if (tm .lt. 0.0) then
        b = 0.0
        a = x
        c = a
      else
        b = 1.0 / (1.414214 * sigx)
        a = tm - tau
        c = (x - wndsp * a) * b
      end if
   10 g = g + ERFXS(c) * d
      if (tm .gt. 0.0.or.wndsp .gt. 0.0) then
        if (d .gt. 0.0) then
          d = -1.0
          a = tm
          c = (x - wndsp * a) * b
          go to 10
        end if
      end if
      ERFXF = 0.5 * g
      return
      end
      Function CRSSE(sigme,vss,wndsp,betal)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - CRSSE Version 01.0                          ---
CC--- Subroutine description - correct sigma-e for crossing         ---
CC---                          trajectories.                        ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
CC        correct sige for crossing trajectories
      a = vss / wndsp
      b = a / sigme
      if (b .gt. 0.2386.and.betal .gt. 0.0) then
        CRSSE = sigme / (1.0 + (betal / sigme * a)**2)**0.25
      else
        CRSSE = sigme
      end if
      return
      end
      Function DELTD(zbot,ztop,dirsh)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - DELTD Version 01.0                          ---
CC--- Subroutine description - wind direction shear over layer from ---
CC---                          zbot to ztop. (degrees)              ---
CC---------------------------------------------------------------------
CC
CC        zbot   - height at bottom of layer (m).
CC        ztop   - height at top of layer (m).
CC        dirsh  - rate of change of wind direction with height
CC                 (degrees/m).
CC
CC*********************************************************************
CC
      data dtrad/0.01745329/
CC
CC********************************************************************
CC
      if (ztop .gt. zbot) then
        DELTD = dirsh * dtrad * (ztop - zbot)
      else
        DELTD = 0.0
      end if
      return
      end
