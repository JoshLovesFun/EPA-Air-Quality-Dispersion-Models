      Subroutine OUTPT(joffs,jndxs,kndxs,mgrpb,ngrpb,vmx50,vmxxx,vmxyy,
     *                 imxdt,ikoff,nmx50)                               122198
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - OUTPT Version 01.1                          ---
CC--- Subroutine description - print tables of calculated dispersion---
CC---------------------------------------------------------------------
CC
CC        joffs  - offset index in array calcsa for variable being
CC                 printed.
CC        jndxs  - index of variable being printed.
CC                 1) dosage.
CC                 2) concentration.
CC                 3) time-average concentration.
CC                 4) gravitational deposition.
CC        kndxs  - print dispersion, max 50 or 1st or 2nd highest.
CC                 0 - dispersion.
CC                 1 - max. 50.
CC               >=2 - 1st and 2nd highest.
CC        mgrpb  - source group label, if ngrpb > 0.
CC        ngrpb  - number of characters in mgrpb.
CC
CC**********************************************************************
CC
      character*1 mqunt,mgrpb,mlabl,maxs                                112196
      character*11 mlab1
      character*15 mlab2
      character*80 mnpbf,mlngd
CC
      dimension mqunt(26),mgrpb(190),vmx50(50),vmxxx(50),vmxyy(50),
     *          imxdt(50),jmxdt(20000),iyr(3),imo(3),idy(3),jdy(3),     122198
     *          ihr(3),mlab1(2),mlab2(2),mlabl(15,2)                    110198
CC
      include 'OCNTRL.INC'
      include 'ONAMES.INC'
      include 'OCDSPM.INC'
      include 'OCALCS.INC'
      include 'OWNTRL.INC'
CC
      equivalence (mnpbf,mnpbuf(1)),(mlngd,mlngnd(1)),(mlab2,mlabl),    122198
     *            (jmxdt(1),calcsa(1))                                  122198
CC
      data mlab1/'Rectangular','Polar      '/,
     *     mlab2/'Highest        ','Second Highest '/
CC
CC**********************************************************************
CC
      nl2 = 0
      nl3 = 0
      nadln = 0
      if (ifdbug .eq. 26) then                                          020797
        maxs = malpha(26)                                               020797
      else                                                              020797
        maxs = malpha(25)                                               020797
      end if                                                            020797
      if (ngrpb .gt. 0) then
        if (iswopt(22) .eq. 0) then
          nl2 = (132 - ngrpb) / 2
        else
          nl2 = (78 - ngrpb) / 2
        end if
        nl2 = MAX0(nl2,1)
        nadln = nadln + (ngrpb + 129) / 130 + 1
      end if
      if (nhrlab .gt. 0.and.kndxs .le. 0) then
        if (iswopt(22) .eq. 0) then
          nl3 = (132 - nhrlab) / 2
        else
          nl3 = (78 - nhrlab) / 2
        end if
        nl3 = MAX0(nl3,1)
        nadln = nadln + (nhrlab + 129) / 130 + 1
      end if
CC      print calculations.
      call MVCH1(modelt(jndxs),mqunt,26)
      j = NUMC1(mqunt,26)
      i = (26 - j) / 2
      if (i .gt. 0) then
        i = LRJST(mqunt,j+i,0)
      end if
      if (kndxs .eq. 0) then
        inp = 1
        if (ngrdpt .gt. 0) then
          if (isgfil .gt. 0.and.iswopt(5) .gt. 0) then
cc            write SigmaPlot output file
            isgrec = isgrec + 1
            WRITE (isgfil,9000,iostat=ier,err=60) (mtitle(i),i=1,50)
            isgrec = isgrec + 1
            WRITE (isgfil,9001,iostat=ier,err=60) nxpnts,nypnts,
     *             mlab1(igrect+1),xorgin,yorgin,munitl(igunit+1),
     *             grdang,nsourc
            isgrec = isgrec + 9
            do 10 i=1,nsourc
              xdumy = clddxs(i) / constl(iswopt(15)+1)
              k = IVGET(isrect(i),2)
              j = IVGET(isrect(i),1)
              if (k .le. 0) then
                ydumy = clddys(i) / constl(iswopt(15)+1)
                call MVCH1(munitl(j+1),mnpbuf,10)
              else
                ydumy = clddys(i)
                call MBLK1(mnpbuf,10)
                call MVCH1(munitg(1),mnpbuf,7)
              end if
              isgrec = isgrec + 1
              WRITE (isgfil,9002,iostat=ier,err=60) i,xdumy,
     *                            munitl(j+1),ydumy,(mnpbuf(l),l=1,10)
   10       continue
            isgrec = isgrec + 1
            WRITE (isgfil,9003,iostat=ier,err=60) mblnk1
            WRITE (isgfil,9003,iostat=ier,err=60) (mscrch(ilbstr(1)+
     *                                            i-1),i=1,nlbsch(1))
            if (nhrlab .gt. 0) then
              isgrec = isgrec + 1
              WRITE (isgfil,9003,iostat=ier,err=60) (mhrlab(i),
     *                                               i=1,nhrlab)
            end if
            if (ngrpb .gt. 0) then
              n = 2 * ngrpb
              isgrec = isgrec + 1
              WRITE (isgfil,9003,iostat=ier,err=60) (mgrpb(i),
     *                                               i=1,ngrpb)
            end if
            if (nlbsch(5) .gt. 0) then
              isgrec = isgrec + 1
              WRITE (isgfil,9003,iostat=ier,err=60) (mscrch(ilbstr(5)+
     *                                              i-1),i=1,nlbsch(5))
            end if
            n1 = iswopt(15)+1
            if (igrect .le. 0) then
              call MVCH1(munitl(n1),mnpbuf,11)
            else
              call MBLK1(mnpbuf,11)
              call MVCH1(munitg(1),mnpbuf,7)
            end if
            if (nxpnts .gt. 1.and.nypnts .gt. 1) then
              isgrec = isgrec + 1
              WRITE (isgfil,9004,iostat=ier,err=60) (mscrch(ilbstr(4)+
     *                                              i-1),i=1,nlbsch(4))
              isgrec = isgrec + 3
              WRITE (isgfil,9005,iostat=ier,err=60) munitl(n1),
     *                                              (mnpbuf(i),i=1,11)
              isgrec = isgrec + 1
              do 30 j=1,nypnts
                j5 = (j - 1) * nxpnts + joffs
                do 20 i=1,nxpnts
                  isgrec = isgrec + 1
                  WRITE (isgfil,9006,iostat=ier,err=60) xcoord(i),
     *                   ycoord(j),calcsa(i+j5)
   20           continue
   30         continue
            else
              if (nxpnts .gt. 1) then
                isgrec = isgrec + 1
                WRITE (isgfil,9007,iostat=ier,err=60) munitl(n1),
     *                                 (mscrch(ilbstr(4)+i-1),i=1,
     *                                  nlbsch(4))
                isgrec = isgrec + 3
                WRITE (isgfil,9008,iostat=ier,err=60)
                do 40 i=1,nxpnts
                  isgrec = isgrec + 1
                  WRITE (isgfil,9009,iostat=ier,err=60) xcoord(i),
     *                                                 calcsa(i+joffs)
   40           continue
              else
                isgrec = isgrec + 1
                WRITE (isgfil,9007,iostat=ier,err=60) munitl(n1),
     *                                     (mscrch(ilbstr(4)+i-1),i=1,
     *                                      nlbsch(4))
                isgrec = isgrec + 3
                WRITE (isgfil,9008,iostat=ier,err=60)
                do 50 j=1,nypnts
                  isgrec = isgrec + 1
                  WRITE (isgfil,9009,iostat=ier,err=60) ycoord(j),
     *                                                 calcsa(j+joffs)
   50           continue
              end if
            end if
            go to 70
   60       i = KERRS(2,msgnam,ier,isgrec,xdum,xdum,xdum,xdum,0)
            iswopt(5) = 0
          end if
CC
CC          print grid coordinate system calculations.
CC          set the number of values per line.
   70     if (iswopt(22) .eq. 0) then
            nvpl = 9
          else
            nvpl = 4
          end if
          if (ngrdpt .gt. 1) then
            call CMXMN(1,1,nxpnts,nxpnts,nypnts,calcsa(joffs+1),dmax,
     *                 dmin,imx,imy)
            call MBLK1(mnpbuf,80)
            WRITE (mnpbf,9010) dmax,xcoord(imx),maxs,ycoord(imy)        020797
            nm2 = JPACK(mnpbf,63)
            if (iswopt(22) .eq. 0) then
              nm1 = (132 - nm2) / 2
            else
              nm1 = (78 - nm2) / 2
            end if
          else                                                          111098
            dmax = calcsa(joffs+1)                                      111098
          end if
          iex = 0
CC          set flag to print subheadings.
          iflg = 1
CC          loop over sets of x's.
   80     isx = iex + 1
          if (isx .le. nxpnts) then
            iex = isx + nvpl - 1
            if (iex .gt. nxpnts) iex = nxpnts
            jflg = 1
            jsy = nypnts + 1
CC            loop over y points in reverse order.
   90       jsy = jsy - 1
            if (jsy .le. 0) go to 80
            if (iflg .ne. 0) then
CC              determine number of lines required for subheading.
              nad = nadln + 7
              if (nlbsch(5) .gt. 0) nad = nad + 1
              if (ngrdpt .gt. 1) nad = nad + 2
              iad = 1
            else
              if (jflg .eq. 0) then
                nad = 0
                iad = 2
              else
                nad = 7
                iad = 3
              end if
            end if
            jm = IOPUT(inp,4,2,nad+1)
            if (jm .ne. 0.or.iflg .ne. 0.or.jflg .ne. 0) then
              if (jm .ne. 0.or.iflg .ne. 0) then
                jflg = 1
                if (ngrpb .gt. 0) then
                  WRITE (iotfil,9011) (mblnk1,i=1,nl2),(mgrpb(i),i=1,
     *                                 ngrpb)
                end if
                if (nhrlab .gt. 0) then
                  WRITE (iotfil,9011) (mblnk1,i=1,nl3),(mhrlab(i),i=1,
     *                                 nhrlab)
                end if
                if (nlbsch(5) .gt. 0) then
                  if (iswopt(22) .eq. 0) then
                    WRITE (iotfil,9012) (mscrch(ilbstr(5)+i-1),i=1,
     *                                   nlbsch(5))
                  else
                    WRITE (iotfil,9013) (mscrch(ilbstr(5)+i-1),i=1,
     *                                   nlbsch(5))
                  end if
                  if (iad .ne. 1) llines = llines + 1
                end if
                if (ngrdpt .gt. 1) then
                  WRITE (iotfil,9014) (mblnk1,i=1,nm1),(mnpbuf(i),
     *                                 i=1,nm2)
                  if (iad .ne. 1) llines = llines + 2
                end if
                if (iad .ne. 1) llines = llines + nadln
              end if
              if (jflg .ne. 0) then
                if (iswopt(22) .eq. 0) then
                  WRITE (iotfil,9015) (mscrch(ilbstr(2)+i-1),i=1,12),
     *                                (xcoord(i),i=isx,iex)
                  WRITE (iotfil,9016) maxs,(mscrch(ilbstr(3)+j1-1),     112196
     *                                j1=1,12),(mqunt(j1),j1=1,26)      112196
                else
                  WRITE (iotfil,9017) (mscrch(ilbstr(2)+i-1),i=1,12),
     *                                (xcoord(i),i=isx,iex)
                  WRITE (iotfil,9018) maxs,(mscrch(ilbstr(3)+j1-1),     112196
     *                                j1=1,12),(mqunt(j1),j1=1,26)      112196
                end if
                if (iad .eq. 2) llines = llines + 7
                inp = 0
                iflg = 0
                jflg = 0
              end if
            end if
            if (dmax .gt. 0.0) then                                     111098
              k = (jsy - 1) * nxpnts + joffs
              WRITE (iotfil,9019) ycoord(jsy),(calcsa(i+k),i=isx,iex)
              go to 90
            else                                                        111098
              WRITE (iotfil,9035)                                       111098
            end if                                                      111098
          end if
        end if
        if (nxypnt .gt. 0) then
CC          print discrete coordinate calculations.
          call MBLK1(mnpbuf,14)
          call MVCH1(mscrch(ilbstr(4)),mnpbuf,MIN0(nlbsch(4),14))
          iflg = 1
          k = ngrdpt + joffs
          if (nxypnt .gt. 1) then
            call CMXMN(1,1,nxypnt,nxypnt,1,calcsa(k+1),dmax,dmin,imx,
     *                 imy)
            call MBLK1(mlngnd,80)
            WRITE (mlngd,9020) dmax,xdscrt(imx),ydscrt(imx),zdscrt(imx)
            nm2 = JPACK(mlngd,76)
            if (iswopt(22) .eq. 0) then
              nm1 = (132 - nm2) / 2
            else
              nm1 = (78 - nm2) / 2
            end if
          else                                                          111098
            dmax = calcsa(k+1)                                          111098
          end if
CC          set the number of values per line.
          if (iswopt(22) .eq. 0) then
            nvpl = 2
          else
            nvpl = 1
          end if
          iex = 0
CC          loop over discrete points.
  100     isx = iex + 1
          if (isx .le. nxypnt) then
            iex = isx + nvpl - 1
            if (iex .gt. nxypnt) iex = nxypnt
            if (iflg .ne. 0) then
CC              determine number of lines required for subheading.
              nad = nadln + 5
              if (nlbsch(5) .gt. 0) nad = nad + 1
              if (nxypnt .gt. 1) nad = nad + 1
            else
              nad = 0
            end if
            jm = IOPUT(inp,4,2,nad+1)
            if (jm .ne. 0.or.iflg .ne. 0) then
              if (ngrpb .gt. 0) then
                WRITE (iotfil,9011) (mblnk1,i=1,nl2),(mgrpb(i),i=1,
     *                               ngrpb)
              end if
              if (nhrlab .gt. 0) then
                WRITE (iotfil,9011) (mblnk1,i=1,nl3),(mhrlab(i),i=1,
     *                               nhrlab)
              end if
              if (nxypnt .gt. 1) then
                WRITE (iotfil,9014) (mblnk1,i=1,nm1),(mlngnd(i),i=1,nm2)
                if (nad .eq. 0) llines = llines + 2
              end if
              WRITE (iotfil,9021) (mblnk1,(malpha(j),j=24,26),
     *                            (mnpbuf(j),j=1,14),i=1,nvpl)
              WRITE (iotfil,9022) (mblnk1,(mscrch(ilbstr(2)+j-1),j=
     *                            1,12),mblnk1,(mscrch(ilbstr(3)+j-1),j=
     *                            1,12),mblnk1,(mscrch(ilbstr(2)+j-1),
     *                            j=1,12),i=1,nvpl)
              if (iswopt(22) .eq. 0) then
                WRITE (iotfil,9023)
              else
                WRITE (iotfil,9024)
              end if
              if (nad .eq. 0) llines = llines + 5 + nadln
              iflg = 0
              inp = 0
            end if
            if (dmax .gt. 0.0) then                                     111098
              WRITE (iotfil,9025) (xdscrt(i),ydscrt(i),zdscrt(i),
     *                             calcsa(i+k),i=isx,iex)
              go to 100
            else                                                        111098
              WRITE (iotfil,9035)                                       111098
            end if                                                      111098
          end if
        end if
      else if (kndxs .eq. 1) then
CC         max. 50 table.
        inp = 1
        dmax = 0.0                                                      111098
        do j=1,nmx50                                                    111098
          if (vmx50(j) .gt. dmax) dmax = vmx50(j)                       111098
        end do                                                          111098
        do 110 j=1,nmx50
          jm = IOPUT(inp,4,4,1)
          inp = 0
          if (jm .ne. 0) then
            if (ngrpb .gt. 0) then
              WRITE (iotfil,9011) (mblnk1,i=1,nl2),(mgrpb(i),i=1,ngrpb)
            end if
            if (nlbsch(5) .gt. 0) then
              if (iswopt(22) .eq. 0) then
                WRITE (iotfil,9012) (mscrch(ilbstr(5)+i-1),i=1,
     *                               nlbsch(5))
              else
                WRITE (iotfil,9013) (mscrch(ilbstr(5)+i-1),i=1,
     *                               nlbsch(5))
              end if
            end if
            WRITE (iotfil,9026) (mscrch(i),i=ilbstr(4),ilbstr(4)+13),
     *                          (mscrch(i),i=ilbstr(2),ilbstr(2)+11),
     *                          (mscrch(i),i=ilbstr(3),ilbstr(3)+11)
            llines = llines + 4
          end if
          call IGTDT(imxdt(j),ifclc,ihr(1),idy(1),imo(1),iyr(1),1)
          jdy(1) = 0
          call JULAN(iyr(1),imo(1),idy(1),jdy(1))
          WRITE (iotfil,9027) vmx50(j),vmxxx(j),vmxyy(j),iyr(1),imo(1),
     *                        idy(1),jdy(1),ihr(1)
          if (dmax .le. 0.0) then                                       111098
            WRITE (iotfil,9035)                                         111098
            go to 120                                                   111098
          end if                                                        111098
  110   continue
  120   continue                                                        111098
      else if (kndxs .ge. 2) then
CC         1st, 2nd highest.
        if (ngrdpt .gt. 0) then
          if (isgfil .gt. 0.and.iswopt(5) .gt. 0) then
CC            write SigmaPlot output file
            isgrec = isgrec + 1
            WRITE (isgfil,9000,iostat=ier,err=180) (mtitle(i),i=1,50)
            isgrec = isgrec + 1
            WRITE (isgfil,9001,iostat=ier,err=180) nxpnts,nypnts,
     *              mlab1(igrect+1),xorgin,yorgin,munitl(igunit+1),
     *              grdang,nsourc
            isgrec = isgrec + 9
            do 130 i=1,nsourc
              xdumy = clddxs(i) / constl(iswopt(15)+1)
              k = IVGET(isrect(i),2)
              j = IVGET(isrect(i),1)
              if (k .le. 0) then
                ydumy = clddys(i) / constl(iswopt(15)+1)
                call MVCH1(munitl(j+1),mnpbuf,10)
              else
                ydumy = clddys(i)
                call MBLK1(mnpbuf,10)
                call MVCH1(munitg(1),mnpbuf,7)
              end if
              isgrec = isgrec + 1
              WRITE (isgfil,9002,iostat=ier,err=180) i,xdumy,
     *                             munitl(j+1),ydumy,(mnpbuf(l),l=1,10)
  130       continue
            isgrec = isgrec + 1
            if (kndxs .eq. 2) then
              j = 8
              k = 1
            else
              j = 15
              k = 2
            end if
            WRITE (isgfil,9003,iostat=ier,err=180) mblnk1
            if (ifmode .eq. 0) then                                     110198
              WRITE (isgfil,9003,iostat=ier,err=180) (mlabl(i,k),i=1,j),
     *                                               (mscrch(ilbstr(1)+
     *                                               i-1),i=1,nlbsch(1))
            else                                                        110198
              WRITE (isgfil,9003,iostat=ier,err=180) (mscrch(ilbstr(1)+
     *                                               i-1),i=1,nlbsch(1))
            end if                                                      110198
            if (ngrpb .gt. 0) then
              n = 2 * ngrpb
              isgrec = isgrec + 1
              WRITE (isgfil,9003,iostat=ier,err=180) (mgrpb(i),
     *                                                i=1,ngrpb)
            end if
            if (nlbsch(5) .gt. 0) then
              isgrec = isgrec + 1
              WRITE (isgfil,9003,iostat=ier,err=180) (mscrch(ilbstr(5)+
     *                                               i-1),i=1,nlbsch(5))
            end if
            n1 = iswopt(15)+1
            if (igrect .le. 0) then
              call MVCH1(munitl(n1),mnpbuf,11)
            else
              call MBLK1(mnpbuf,11)
              call MVCH1(munitg(1),mnpbuf,7)
            end if
            if (nxpnts .gt. 1.and.nypnts .gt. 1) then
              isgrec = isgrec + 1
              WRITE (isgfil,9004,iostat=ier,err=180) (mscrch(ilbstr(4)+
     *                                               i-1),i=1,nlbsch(4))
              isgrec = isgrec + 3
              WRITE (isgfil,9005,iostat=ier,err=180) munitl(n1),
     *                                               (mnpbuf(i),i=1,11)
              isgrec = isgrec + 1
              do 150 j=1,nypnts
                j5 = (j - 1) * nxpnts                                   110198
                do 140 i=1,nxpnts
                  isgrec = isgrec + 1
                  WRITE (isgfil,9006,iostat=ier,err=180) xcoord(i),     110198
     *                                                   ycoord(j),     110198
     *                                           calcsa(i+j5+joffs)     110198
  140           continue
  150         continue
            else
              if (nxpnts .gt. 1) then
                isgrec = isgrec + 1
                WRITE (isgfil,9007,iostat=ier,err=180) munitl(n1),
     *                                  (mscrch(ilbstr(4)+i-1),i=1,
     *                                   nlbsch(4))
                isgrec = isgrec + 3
                WRITE (isgfil,9008,iostat=ier,err=180)
                do 160 i=1,nxpnts
                  isgrec = isgrec + 1
                  WRITE (isgfil,9009,iostat=ier,err=180) xcoord(i),     110198
     *                                              calcsa(i+joffs)     110198
  160           continue
              else
                isgrec = isgrec + 1
                WRITE (isgfil,9007,iostat=ier,err=180) munitl(n1),
     *                                      (mscrch(ilbstr(4)+i-1),i=1,
     *                                       nlbsch(4))
                isgrec = isgrec + 3
                WRITE (isgfil,9008,iostat=ier,err=180)
                do 170 j=1,nypnts
                  isgrec = isgrec + 1
                  WRITE (isgfil,9009,iostat=ier,err=180) ycoord(j),     110198
     *                                              calcsa(j+joffs)     110198
  170           continue
              end if
            end if
            go to 190
  180       i = KERRS(2,msgnam,ier,isgrec,xdum,xdum,xdum,xdum,0)
            iswopt(5) = 0
          end if
CC
CC          print grid coordinate system highest or 2nd highest.
CC          set the number of values per line.
  190     if (iswopt(22) .eq. 0) then
            nvpl = 3
          else
            nvpl = 2
          end if
          inp = 1
          if (ngrdpt .gt. 1) then
            call CMXMN(1,1,nxpnts,nxpnts,nypnts,calcsa(joffs+1),dmax,   110198
     *                 dmin,imx,imy)                                    110198
            call MBLK1(mnpbuf,80)
            WRITE (mnpbf,9010) dmax,xcoord(imx),maxs,ycoord(imy)        020797
            nm2 = JPACK(mnpbf,63)
            if (iswopt(22) .eq. 0) then
              nm1 = (132 - nm2) / 2
            else
              nm1 = (78 - nm2) / 2
            end if
          else                                                          111098
            dmax = calcsa(joffs+1)                                      111098
          end if
          iex = 0
CC          set flag to print subheadings.
          iflg = 1
CC          loop over sets of x's.
  200     isx = iex + 1
          if (isx .le. nxpnts) then
            iex = isx + nvpl - 1
            if (iex .gt. nxpnts) iex = nxpnts
            jflg = 1
            jsy = nypnts + 1
CC            loop over y points in reverse order.
  210       jsy = jsy - 1
            if (jsy .le. 0) go to 200
            if (iflg .ne. 0) then
CC              determine number of lines required for subheading.
              nad = nadln + 7
              if (nlbsch(5) .gt. 0) nad = nad + 1
              if (ngrdpt .gt. 1) nad = nad + 2
              iad = 1
            else
              if (jflg .eq. 0) then
                nad = 0
                iad = 2
              else
                nad = 7
                iad = 3
              end if
            end if
            jm = IOPUT(inp,4,3+kndxs,nad+1)
            if (jm .ne. 0.or.iflg .ne. 0.or.jflg .ne. 0) then
              if (jm .ne. 0.or.iflg .ne. 0) then
                jflg = 1
                if (ngrpb .gt. 0) then
                  WRITE (iotfil,9011) (mblnk1,i=1,nl2),(mgrpb(i),i=1,
     *                                 ngrpb)
                end if
                if (nlbsch(5) .gt. 0) then
                  if (iswopt(22) .eq. 0) then
                    WRITE (iotfil,9012) (mscrch(ilbstr(5)+i-1),i=1,
     *                                   nlbsch(5))
                  else
                    WRITE (iotfil,9013) (mscrch(ilbstr(5)+i-1),i=1,
     *                                   nlbsch(5))
                  end if
                  if (iad .ne. 1) llines = llines + 1
                end if
                if (ngrdpt .gt. 1) then
                  WRITE (iotfil,9014) (mblnk1,i=1,nm1),(mnpbuf(i),
     *                                 i=1,nm2)
                  if (iad .ne. 1) llines = llines + 2
                end if
                if (iad .ne. 1) llines = llines + nadln
              end if
              if (jflg .ne. 0) then
                if (iswopt(22) .eq. 0) then
                  WRITE (iotfil,9028) (mscrch(ilbstr(2)+i-1),i=1,12),
     *                                (xcoord(i),i=isx,iex)
                  WRITE (iotfil,9029) maxs,(mscrch(ilbstr(3)+j1-1),     112196
     *                                j1=1,12),(mqunt(j1),j1=1,26)      112196
                else
                  WRITE (iotfil,9030) (mscrch(ilbstr(2)+i-1),i=1,12),
     *                                (xcoord(i),i=isx,iex)
                  WRITE (iotfil,9031) maxs,(mscrch(ilbstr(3)+j1-1),     112196
     *                                j1=1,12),(mqunt(j1),j1=1,26)      112196
                end if
                if (iad .eq. 2) llines = llines + 7
                inp = 0
                iflg = 0
                jflg = 0
              end if
            end if
            if (dmax .gt. 0.0) then                                     111098
              k = (jsy - 1) * nxpnts
              do 220 i=isx,iex
                l = i - isx + 1
                call IGTDT(jmxdt(i+k+ikoff),ifclc,ihr(l),idy(l),imo(l), 122198
     *                     iyr(l),0)                                    122198
                jdy(l) = 0
                call JULAN(iyr(l),imo(l),idy(l),jdy(l))
  220         continue
              WRITE (iotfil,9032) ycoord(jsy),(calcsa(i+k+joffs),       110198
     *                            imo(i-isx+1),idy(i-isx+1),            111098
     *                            iyr(i-isx+1),jdy(i-isx+1),            111098
     *                            ihr(i-isx+1),i=isx,iex)               110198
              go to 210
            else                                                        111098
              WRITE (iotfil,9035)                                       111098
            end if                                                      111098
          end if
        end if
        if (nxypnt .gt. 0) then
          inp = 1
CC          print discrete coordinate calculations.
          call MBLK1(mnpbuf,14)
          call MVCH1(mscrch(ilbstr(4)),mnpbuf,MIN0(nlbsch(4),14))
          iflg = 1
          if (nxypnt .gt. 1) then
            call CMXMN(1,1,nxypnt,nxypnt,1,calcsa(ngrdpt+1+joffs),dmax, 110198
     *                 dmin,imx,imy)                                    110198
            call MBLK1(mlngnd,80)
            WRITE (mlngd,9020) dmax,xdscrt(imx),ydscrt(imx),zdscrt(imx)
            nm2 = JPACK(mlngd,76)
            if (iswopt(22) .eq. 0) then
              nm1 = (132 - nm2) / 2
            else
              nm1 = (78 - nm2) / 2
            end if
          else                                                          111098
            dmax = calcsa(ngrdpt+1+joffs)                               111098
          end if
          iex = 0
CC          loop over discrete points.
  230     iex = iex + 1
          if (iex .le. nxypnt) then
            if (iflg .ne. 0) then
CC              determine number of lines required for subheading.
              nad = nadln + 5
              if (nxypnt .gt. 1) nad = nad + 1
            else
              nad = 0
            end if
            jm = IOPUT(inp,4,3+kndxs,nad+1)
            if (jm .ne. 0.or.iflg .ne. 0) then
              if (ngrpb .gt. 0) then
                WRITE (iotfil,9011) (mblnk1,i=1,nl2),(mgrpb(i),i=1,
     *                               ngrpb)
              end if
              if (nxypnt .gt. 1) then                                   111098
                WRITE (iotfil,9014) (mblnk1,i=1,nm1),(mlngnd(i),i=1,
     *                               nm2)
                if (nad .eq. 0) llines = llines + 2
              end if
              WRITE (iotfil,9033) (mnpbuf(j),j=1,14),
     *                            (mscrch(ilbstr(2)+j-1),j=1,12),
     *                            (mscrch(ilbstr(3)+j-1),j=1,12),
     *                            (mscrch(ilbstr(2)+j-1),j=1,12)
              if (nad .eq. 0) llines = llines + 5 + nadln
              iflg = 0
              inp = 0
            end if
            if (dmax .gt. 0.0) then                                     111098
              call IGTDT(jmxdt(iex+ngrdpt+ikoff),ifclc,ihr(1),idy(1),   122198
     *                   imo(1),iyr(1),0)                               122198
              jdy(1) = 0
              call JULAN(iyr(1),imo(1),idy(1),jdy(1))
              WRITE (iotfil,9034) xdscrt(iex),ydscrt(iex),zdscrt(iex),  110198
     *                            calcsa(iex+ngrdpt+joffs),imo(1),      110198
     *                            idy(1),iyr(1),jdy(1),ihr(1)           110198
              go to 230
            else                                                        111098
              WRITE (iotfil,9035)                                       111098
            end if                                                      111098
          end if
        end if
      end if
      return
CC
 9000 format (/' Title=',50a1)
 9001 format (' Number of grid system x coordinates=',i4/
     *        ' Number of grid system y coordinates=',i4/
     *        ' Grid system is ',a11/
     *        ' X origin=',f12.2,', Y origin=',f12.2,1x,a11/
     *        ' Grid system orientation angle=',f7.2,' degrees'//
     *        ' Number of sources=',i4//3x,'Source',10x,'X',22x,'Y'/
     *        3x,'Number',5x,'Coordinate',13x,'Coordinate'/
     *        3x,'------',5x,'----------',13x,'----------')
 9002 format (5x,i3,5x,f11.2,1x,a10,1x,f11.2,1x,10a1)
 9003 format (80a1)
 9004 format (/9x,'X',12x,'Y'/1x,2(3x,'Coordinate'),2x,50a1)
 9005 format (2x,'(',a11,')',1x,'(',11a1,')'/4x,55('-'))
 9006 format (1x,2(2x,f11.2),6x,g16.6)
 9007 format (/5x,'Distance'/3x,'(',5a2,')',3x,33a2)
 9008 format (3x,55('-'))
 9009 format (3x,f11.2,6x,g16.6)
 9010 format ('(Maximum = >',g12.5,'< at x = >',f11.2,', ',a1,' = >',   020797
     *f11.2,')')                                                        020797
 9011 format (/132a1/(2x,130a1))
 9012 format (48x,35a1)
 9013 format (21x,35a1)
 9014 format (/132a1)
 9015 format (/57x,'- X Axis',12a1,'-'/14x,9f13.3/)
 9016 format (' ',a1,' Axis ',12a1,35x,'- ',26a1,' -'/14x,116('-')/)    112196
 9017 format (/30x,'- X Axis',12a1,'-'/14x,4f13.3/)
 9018 format (' ',a1,' Axis ',12a1,10x,'- ',26a1,' -'/14x,53('-')/)     112196
 9019 format (1x,f13.3,1x,9g13.6)
 9020 format ('(Maximum = >',g12.5,'< at X,Y,Z = >',f11.2,2(',>',f11.2),
     *')')
 9021 format (/2(a1,5x,a1,12x,a1,12x,a1,7x,14a1))
 9022 format (2(3(a1,12a1),15x))
 9023 format (1x,107('-')/)
 9024 format (1x,53('-')/)
 9025 format (2(1x,f12.3,2f13.3,g15.6))
 9026 format (/27x,'x',12x,'y',6x,'Year Month Day Julian Hour'/
     *6x,14a1,2(1x,12a1),18x,'day'/6x,67('-'))
 9027 format (7x,g12.6,2(3x,f10.1),2x,i4,3x,i2.2,3x,i2.2,3x,i3.3,2x,
     *i4.4)
 9028 format (/45x,'- X Axis',12a1,'-'/15x,3(f13.3,' Mo/Dy/Yr Jdy  Hr  '
     *))
 9029 format (' ',a1,' Axis ',12a1,30x,'- ',26a1,' -'/14x,98('-')/)     112196
 9030 format (/28x,'- X Axis',12a1,'-'/15x,2(f13.3,' Mo/Dy/Yr Jdy  Hr  '
     *))
 9031 format (' ',a1,' Axis ',12a1,10x,'- ',26a1,' -'/14x,65('-')/)     112196
 9032 format (1x,f13.3,3(1x,g13.6,1x,i2.2,'/',i2.2,'/',i2.2,1x,i3.3,1x, 020797
     *i4.4))
 9033 format (/9x,'X',12x,'Y',12x,'Z',7x,14a1,' Mo/Dy/Yr Jdy  Hr'/
     *2x,3(1x,12a1)/3x,74('-')/)
 9034 format (2x,3(1x,f12.2),2x,g14.6,1x,3(1x,i2.2),1x,i3.3,1x,i4.4)
 9035 format (22x,'All values in this table are zero')                  111098
      end
