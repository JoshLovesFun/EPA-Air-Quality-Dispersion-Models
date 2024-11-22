      Subroutine RPRTB
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - RPRTB Version 01.3                          --- 072497
CC--- Subroutine description - print input data.                    ---
CC---                                                               ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      character*1 mbufr,mdash,mcoma,mone,mappr                          073098
      character*3 mbuf
      character*4 mnone
      character*6 mstyp
      character*11 mrtyp
      character*16 metyp
CC
      dimension metyp(2),mstyp(2),mrtyp(2),mbufr(80)
CC
      include 'OCNTRL.INC'
      include 'ONAMES.INC'
      include 'OCDSPM.INC'
      include 'OCALCS.INC'
CC
      data mdash/'-'/,mcoma/','/,mone/'1'/,mappr/'~'/,                  073098
     *     metyp/' Instantaneous  ',
     *           'Quasi-continuous'/,
     *     mstyp/'Volume',
     *           ' Line '/
      data mrtyp/'Rectangular',
     *           '   Polar   '/,
     *     mnone/'None'/
CC
CC**********************************************************************
CC
      if (iotfil .gt. 0) then
        if (nsourc .gt. 0) then
CC
CC          print source geometry/emission values.
          if (IOPUT(0,2,4,13) .eq. 0) then
            WRITE (iotfil,9000)
          end if
          llines = llines - 2
          if (NUMC1(mpolnt,36) .le. 0) then
            call MBLK1(mbufr,36)
            call MVCH1(mnone,mbufr,4)
          else
            call MVCH1(mpolnt,mbufr,36)
          end if
          n = LRJST(mbufr,36,1)
          j = MAX0(48-n,1)
          WRITE (iotfil,9001) (mdash,i=1,j-1),mblnk1,(mbufr(i),i=1,n)
          if (NUMC1(mspeci,36) .le. 0) then
            call MBLK1(mbufr,36)
            call MVCH1(mnone,mbufr,4)
          else
            call MVCH1(mspeci,mbufr,36)
          end if
          n = LRJST(mbufr,36,1)
          j = MAX0(57-n,1)
          WRITE (iotfil,9002) (mdash,i=1,j-1),mblnk1,(mbufr(i),i=1,n)
          WRITE (iotfil,9003) nsourc
          WRITE (iotfil,9004)
          do 10 k=1,nsourc
            if (IOPUT(0,2,4,1) .ne. 0) then
              WRITE (iotfil,9004)
              llines = llines + 4
            end if
            i1 = IVGET(isrect(k),2)
            i2 = MAX0(IVGET(isotyp(k),2),1)
            i3 = MAX0(IVGET(isotyp(k),1),1)
            call MVCH1(msoids(1,k),mbufr,36)
            if (NUMC1(mbufr,36) .le. 0) call MVCH1(mnone,mbufr,4)
            WRITE (iotfil,9005) k,(mbufr(i),i=1,36),mrtyp(i1+1),
     *                          mstyp(i3),metyp(i2)
   10     continue
          i = IOPUT(0,2,4,5)
          llines = llines - 1
          WRITE (iotfil,9006)
          lflg = 0
          n = 0
          do 20 k=1,nsourc
            if (IOPUT(0,2,4,1) .ne. 0) then
              WRITE (iotfil,9006)
              llines = llines + 4
            end if
            i1 = IVGET(isrect(k),2)
CC            if line source                                            072497
            if (IVGET(isotyp(k),1) .eq. 2) lflg = lflg + 1
            call UNITS(mbufr(5),2,IVGET(isrect(k),1),0,0,1,0)
            if (i1 .eq. 0) then
              call UNITS(mbufr(7),2,IVGET(isrect(k),1),0,0,1,0)
            else
              call UNITS(mbufr(7),2,0,0,0,1,1)
            end if
            call UNITS(mbufr(9),2,IVGET(isrect(k),3),0,0,1,0)
            call UNITS(mbufr(11),2,IVGET(iqunit(k),2),0,0,1,3)
            mbufr(13) = mblnk1
            mbufr(14) = mblnk1
            if (relhgt(k) .lt. 0.0) then
              if (clddpt(k) .lt. 0.0) then                              072497
                mbufr(19) = mappr
                n = n + 1
                a = airprs * constp(iprunt+1)
                t = constc(itpunt+1,2) * (airtmp - constc(itpunt+1,1))
                h = hetcnt(k) * consth(IVGET(jqunit(k),1)+1) /
     *              constm(IVGET(jqunit(k),2)+1)
                q = qemiss(k) * constm(IVGET(iqunit(k),2)+1)
CC                default radius of detonation.                         072497
                d = AIRDN(a,t,airhum)
                dumy = 0.89 * (3.0 * h * q / (4.0 * 3.14159 * spheat *
     *                 d * 1.0e6 * (t + constc(2,1))))**0.333333
              else                                                      072497
                dumy = 0.5 * clddpt(k)                                  072497
              end if                                                    072497
            else
              mbufr(19) = mblnk1
              dumy = relhgt(k)
            end if
            i = IWTBF(mbufr(20),9,dumy,1,0)
            WRITE (iotfil,9007) k,clddxs(k),(mbufr(i),i=5,6),
     *             clddys(k),(mbufr(i),i=7,8),clddzs(k),(mbufr(i),
     *             i=5,6),(mbufr(i),i=20,28),mbufr(19),
     *             (mbufr(i),i=9,10),qemiss(k),(mbufr(i),i=11,12)       073098
   20     continue
          if (n .gt. 0) then
            if (IOPUT(0,2,4,1) .ne. 0) then
              WRITE (iotfil,9006)
              llines = llines + 4
            end if
            WRITE (iotfil,9021) mappr
          end if
          if (lflg .gt. 0) then
            i = IOPUT(0,2,4,5)
            llines = llines - 1
            WRITE (iotfil,9008)
            i3 = 0                                                      072497
            do 30 k=1,nsourc
CC              if line                                                 072497
              if (IVGET(isotyp(k),1) .eq. 2) then
                if (IOPUT(0,2,4,1) .ne. 0) then
                  WRITE (iotfil,9008)
                  llines = llines + 4
                end if
                call UNITS(mbufr,2,IVGET(isrect(k),3),0,0,1,0)
                i1 = IVGET(isrect(k),2)
                call UNITS(mbufr(3),2,IVGET(isrect(k),1),0,0,1,0)
                if (i1 .eq. 0) then
                  call MVCH1(mbufr,mbufr(5),2)
                else
                  call UNITS(mbufr(5),2,0,0,0,1,1)
                end if
                if (relhte(k) .lt. 0.0) then                            072497
                  a = airprs * constp(iprunt+1)                         072497
                  t = constc(itpunt+1,2) * (airtmp - constc(itpunt+1,1))072497
                  h = hetcnt(k) * consth(IVGET(jqunit(k),1)+1) /        072497
     *                constm(IVGET(jqunit(k),2)+1)                      072497
                  q = qemiss(k) * constm(IVGET(iqunit(k),2)+1)          072497
CC                  default radius of detonation.                       072497
                  d = AIRDN(a,t,airhum)                                 072497
                  dumy = 0.89 * (3.0 * h * q / (4.0 * 3.14159 * spheat *072497
     *                   d * 1.0e6 * (t + constc(2,1))))**0.333333      072497
                  mbufr(19) = mappr                                     072497
                  i3 = 1                                                072497
                else                                                    072497
                  mbufr(19) = mblnk1                                    072497
                  dumy = relhte(k)                                      072497
                end if                                                  072497
                i = IWTBF(mbufr(20),9,dumy,1,0)                         072497
                WRITE (iotfil,9007) k,clddxe(k),(mbufr(i),i=3,4),
     *                              clddye(k),(mbufr(i),i=5,6),
     *                              clddze(k),(mbufr(i),i=3,4),
     *                              (mbufr(i),i=20,28),mbufr(19),       072497
     *                              (mbufr(i),i=1,2)                    072497
              end if
   30       continue
            if (i3 .ne. 0) then                                         072497
              if (IOPUT(0,2,4,1) .ne. 0) then                           072497
                WRITE (iotfil,9008)                                     072497
                llines = llines + 4                                     072497
              end if                                                    072497
              write (iotfil,9021) mappr                                 072497
            end if                                                      071797
          end if
          i = IOPUT(0,2,4,5)
          llines = llines - 1
          WRITE (iotfil,9009)
          i1 = 0
          i2 = 0
          do 40 k=1,nsourc
            if (IOPUT(0,2,4,1) .ne. 0) then
              WRITE (iotfil,9009)
              llines = llines + 4
            end if
            if (IVGET(isotyp(k),1) .ne. 1) then
              i2 = i2 + 1
            else if (IVGET(isotyp(k),2) .ne. 1) then
              i1 = i1 + 1
            end if
            call UNITS(mbufr(7),2,IVGET(ismunt(k),1),0,0,1,0)
            call UNITS(mbufr(9),2,IVGET(ismunt(k),2),0,0,1,0)
            call UNITS(mbufr(11),2,IVGET(ismunt(k),3),0,0,1,0)
            call UNITS(mbufr(13),2,IVGET(ismunt(k),4),0,0,1,0)
            WRITE (iotfil,9010) k,xsmlry(k),(mbufr(i),i=7,8),
     *                          xsmlrz(k),(mbufr(i),i=9,10),
     *                          xlrgry(k),(mbufr(i),i=11,12),
     *                          xlrgrz(k),(mbufr(i),i=13,14),
     *                          gammat(k),alphas(k),betass(k)
   40     continue
          if (i1 .gt. 0.or.i2 .gt. 0) then
            i3 = 0                                                      072497
            i = IOPUT(0,2,4,5)
            llines = llines - 1
            WRITE (iotfil,9011)
            do 50 k=1,nsourc
              if (IVGET(isotyp(k),1) .ne. 1.or.IVGET(isotyp(k),2) .ne.
     *            1) then
CC                Line or quasi-continuous                              072497
                if (IOPUT(0,2,4,1) .ne. 0) then
                  WRITE (iotfil,9011)
                  llines = llines + 4
                end if
                call UNITS(mbufr,2,IVGET(idrect(k),1),0,0,1,0)
                call UNITS(mbufr(3),2,IVGET(idrect(k),2),0,0,1,0)
                if (IVGET(isotyp(k),1) .ne. 1.and.                      072497
     *              IVGET(isotyp(k),2) .eq. 1) then                     072497
CC                      Instantaneous line soure                        072497
                  a = airprs * constp(iprunt+1)                         072497
                  t = constc(itpunt+1,2) * (airtmp - constc(itpunt+1,1))072497
                  h = hetcnt(k) * consth(IVGET(jqunit(k),1)+1) /        072497
     *                constm(IVGET(jqunit(k),2)+1)                      072497
                  q = qemiss(k) * constm(IVGET(iqunit(k),2)+1)          072497
CC                  default radius of detonation.                       072497
                  d = AIRDN(a,t,airhum)                                 072497
                  dumy = 0.89 * (3.0 * h * q / (4.0 * 3.14159 * spheat *072497
     *                   d * 1.0e6 * (t + constc(2,1))))**0.333333      072497
                  i = IWTBF(mbufr(10),8,cldlng(k),2,0)                  072497
                  if (cldsht(k) .le. 0.0) then                          072497
                    i = IWTBF(mbufr(20),8,2.0*dumy,2,0)                 072497
                    mbufr(8) = mappr                                    072497
                    i3 = 1                                              072497
                  else                                                  072497
                    i = IWTBF(mbufr(20),8,cldsht(k),2,0)                072497
                    mbufr(8) = mblnk1                                   072497
                  end if                                                072497
                  if (clddpt(k) .le. 0.0) then                          072497
                    i = IWTBF(mbufr(30),8,2.0*dumy,2,0)                 072497
                    mbufr(9) = mappr                                    072497
                    i3 = 1                                              072497
                  else                                                  072497
                    i = IWTBF(mbufr(30),8,clddpt(k),2,0)                072497
                    mbufr(9) = mblnk1                                   072497
                  end if                                                072497
                else                                                    072497
                  i = IWTBF(mbufr(10),8,cldlng(k),2,0)                  072497
                  i = IWTBF(mbufr(20),8,cldsht(k),2,0)                  072497
                  i = IWTBF(mbufr(30),8,clddpt(k),2,0)                  072497
                end if                                                  072497
                WRITE (iotfil,9012) k,(mbufr(i),i=10,17),               072497
     *                (mbufr(i),i=1,2),(mbufr(i),i=20,27),mbufr(8),     072497
     *                (mbufr(i),i=1,2),(mbufr(i),i=30,37),mbufr(9),     072497
     *                (mbufr(i),i=3,4),cldang(k)                        072497
              end if
   50       continue
            if (i3 .ne. 0) then                                         072497
              if (IOPUT(0,2,4,1) .ne. 0) then                           072497
                WRITE (iotfil,9011)                                     072497
                llines = llines + 4                                     072497
              end if                                                    072497
              write (iotfil,9021) mappr                                 072497
            end if                                                      072497
          end if
          i = IOPUT(0,2,4,5)
          llines = llines - 1
          i1 = -1
          n = 0
          do 60 k=1,nsourc
CC            i2: 1=instant, 2=quasi
            i2 = IVGET(isotyp(k),2)
CC            i3: 1=volumne, 2=line
            i3 = IVGET(isotyp(k),1)
            if (i1 .ne. i2.or.IOPUT(0,2,4,1) .ne. 0) then
              i1 = i2
              if (i2 .ne. 1.or.i3 .eq. 2) then
                WRITE (iotfil,9013)
              else
                WRITE (iotfil,9014)
              end if
              llines = llines + 4
            end if
            call UNITS(mbufr,5,IVGET(jqunit(k),1),IVGET(jqunit(k),2),
     *                 0,1,13)
            call UNITS(mbufr(6),5,IVGET(kqunit(k),1),IVGET(kqunit(k),2),
     *                 0,1,12)
            i = IWTBF(mbufr(16),10,hetcnt(k),2,0)
            i = IWTBF(mbufr(27),10,brnrat(k),2,0)
            i = LRJST(mbufr,5,1)
            i = LRJST(mbufr(6),5,1)
            call UNITS(mbufr(11),2,IVGET(isrect(k),1),0,0,1,0)
            call UNITS(mbufr(57),2,IVGET(iqunit(k),3),0,0,1,5)
            if (i2 .ne. 1.or.i3 .eq. 2) then
CC              quasi-cont. or line                                     072497
              WRITE (iotfil,9015) k,(mbufr(i+15),i=1,10),
     *                            (mbufr(i),i=1,5),(mbufr(i+26),i=1,10),
     *                            (mbufr(i),i=6,10),taucld(k),
     *                            (mbufr(i+56),i=1,2)
            else
CC              Instantaneous volume                                    072497
              if (clddpt(k) .le. rmissc) then                           072497
                mbufr(68) = mappr
                n = n + 1
                a = airprs * constp(iprunt+1)
                t = constc(itpunt+1,2) * (airtmp - constc(itpunt+1,1))
                h = hetcnt(k) * consth(IVGET(jqunit(k),1)+1) /
     *                    constm(IVGET(jqunit(k),2)+1)
                q = qemiss(k) * constm(IVGET(iqunit(k),2)+1)
CC                default diameter of detonation.
                d = AIRDN(a,t,airhum)
                dumy = 0.89 * (3.0 * h * q / (4.0 * 3.14159 * spheat *
     *                 d * 1.0e6 * (t + constc(2,1))))**0.333333
                dumy = 2.0 * dumy
              else
                mbufr(68) = mblnk1
                dumy = clddpt(k)                                        072497
              end if
              i = IWTBF(mbufr(60),8,dumy,2,0)
              WRITE (iotfil,9015) k,(mbufr(i+15),i=1,10),
     *                            (mbufr(i),i=1,5),(mbufr(i+26),i=1,10),
     *                            (mbufr(i),i=6,10),taucld(k),
     *                            (mbufr(i+56),i=1,2),(mbufr(59+i),i=1,
     *                            8),mbufr(68),(mbufr(i),i=11,12)
            end if
   60     continue
          if (n .gt. 0) then
            if (IOPUT(0,2,4,1) .ne. 0) then
              WRITE (iotfil,9014)
              llines = llines + 4
            end if
            WRITE (iotfil,9021) mappr
          end if
          if (ifhrly .gt. 0) then
            i = IOPUT(0,2,4,5)
            llines = llines - 1
            i1 = -1
            i2 = 0
            j = vllimt(4)
            if (j .ge. 100) j = j / 100
            do 80 k=1,nsourc
              if (i1 .ne. i2.or.IOPUT(0,2,4,1) .ne. 0) then
                i1 = i2
                WRITE (iotfil,9016) (i,i=j,j+24-1)
                llines = llines + 4
              end if
              do 70 i=1,24
                if (isofrq(i,k) .gt. 0) then
                  mbufr(i) = malpha(25)
                else
                  mbufr(i) = malpha(14)
                end if
   70         continue
              WRITE (iotfil,9017) k,(mbufr(i),i=1,24)
   80       continue
          end if
          if (ngroup .gt. 0) then
            i = IOPUT(0,2,4,ngroup+6)
            llines = llines - 4
            WRITE (iotfil,9018)
            lgrps = 0
            ngrps = 0
   90       ngrps = ngrps + 1
            if (ngrps .le. ngroup) then
              j = nsogrp(ngrps)
              call MBLK1(mbufr,80)
              l = 0
              n = 0
              do 100 i=1,j
                xdum = isogrp(lgrps+i)
                k = IWTBF(mbuf,3,xdum,0,0)
                if (isogrp(lgrps+i) .gt. 0) then
                  call MVCH1(mbuf,mbufr(l+1),3)
                  mbufr(l+4) = mcoma
                  l = l + 4
                else
                  if (i .eq. 1) then
                    mbufr(l+3) = mone
                    l = l + 4
                  else if (IABS(isogrp(lgrps+i)) .lt.
     *                     IABS(isogrp(lgrps+i-1))) then
                    mbufr(l+3) = mone
                    l = l + 4
                  else
                    mbufr(l) = mblnk1
                  end if
                  call MVCH1(mbuf,mbufr(l+1),3)
                  mbufr(l+4) = mcoma
                  l = l + 4
                end if
                if (l .ge. 60) then
                  n = n + 1
                  k = IOPUT(0,2,4,1)
                  if (k .eq. 1) then
                    WRITE (iotfil,9018)
                    llines = llines + 5
                  end if
                  if (n .le. 1) then
                    WRITE (iotfil,9019) ngrps,(mbufr(k),k=1,l)
                  else
                    WRITE (iotfil,9020) (mbufr(k),k=1,l)
                  end if
                  l = 0
                  call MBLK1(mbufr,80)
                end if
  100         continue
              if (l .gt. 0) then
                n = n + 1
                k = IOPUT(0,2,4,1)
                if (k .eq. 1) then
                  WRITE (iotfil,9018)
                  llines = llines + 5
                end if
                if (n .le. 1) then
                  WRITE (iotfil,9019) ngrps,(mbufr(k),k=1,l)
                else
                  WRITE (iotfil,9020) (mbufr(k),k=1,l)
                end if
                l = 0
                call MBLK1(mbufr,80)
              end if
              lgrps = lgrps + j
              go to 90
            end if
          end if
        end if
      end if
      return
CC
 9000 format (/19x,'- Source Geometry/Emission Strength -'/)
 9001 format (' Material or fuel/explosive ',48a1)
 9002 format (' Pollutant/species ',57a1)
 9003 format (' Total number of sources ',46('-'),1x,i4)
 9004 format (/' Source',6x,'Ident',26x,'Reference   Source     Emission
     *'/' Number',39x,'System     Type',8x,'Type'/1x,77('-'))
 9005 format (1x,i3,2x,36a1,1x,a11,2x,a6,1x,a16)
 9006 format (/' Source     X',12x,'Y',12x,'Z',10x,'Release',8x,'Emissio
     *n'/' Number Coordinate   Coordinate   Elevation',7x,'Height',8x,
     *'Strength'/1x,77('-'))
 9007 format (2x,i3,2x,3(1x,f9.1,1x,2a1),1x,9a1,a1,2a1,1x,1p,e12.5,1x,
     *5a1)
 9008 format (/' Source    End X',8x,'End Y',8x,'End Z',5x,'End Release'
     */' Number Coordinate   Coordinate   Elevation',7x,'Height'/1x,
     *58('-'))
 9009 format (/' Source Rect. Expan. Dist. / Reference Dist.   /Air Entr
     *ain-/Dispersion Coeff.'/' Number Crosswind  Vertical/Crosswind  Ve
     *rtical/ment Coeff. /Crosswind Vertical'/1x,78('-'))
 9010 format (2x,i3,2x,4(f8.2,2a1),2(4x,f7.3),3x,f7.3)
 9011 format (/' Source',8x,'Initial Dimensions',9x,'/ Angle from North'
     */' Number  Long side  Short side   Vertical /    to Longside'/
     *1x,60('-'))
 9012 format (2x,i3,2x,1x,8a1,1x,2a1,2(1x,8a1,a1,2a1),6x,f7.2,' deg')   072497
 9013 format (/' Source',5x,'Fuel Heat',8x,'Fuel Burn',6x,'Fuel Burn'/
     *' Number',6x,'Content',11x,'Rate',11x,'Time'/
     *1x,77('-'))
 9014 format (/' Source',5x,'Fuel Heat',8x,'Fuel Burn',6x,'Fuel Burn',
     *3x,'Initial'/' Number',6x,'Content',11x,'Rate',11x,'Time',6x,
     *'Diameter'/1x,77('-'))
 9015 format (2x,i3,2x,2(1x,10a1,1x,5a1),1x,f9.1,1x,2a1,1x,8a1,a1,2a1)
 9016 format (/' Source',11x,'Hours in which source is burned or detonat
     *ed'/' Number',i2,23i3/1x,77('-'))
 9017 format (2x,i3,1x,24(2x,a1))
 9018 format (' Source grouping -'//' Group'/' Number',20x,'Source Numbe072497
     *rs'/1x,68('-'))                                                   072497
 9019 format (3x,i2,4x,60a1)
 9020 format (9x,60a1)
 9021 format (1x,a1,' Means the value is defaulted for each hour of met.
     * data.')
      end
