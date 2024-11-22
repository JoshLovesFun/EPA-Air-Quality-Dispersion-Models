      Subroutine RPLTA
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - RPLTA Version 01.3                          --- 072497
CC--- Subroutine description - get plot type(s) and plot details.   ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
CC
      character*1 mdumy
      character*3 mlabl
CC
      dimension ker(3),mlabl(2)
CC
      include 'OCNTRL.INC'
      include 'OCDSPM.INC'
      include 'OWNTRL.INC'
      include 'OCALCS.INC'
      include 'ONAMES.INC'
CC
      equivalence (kquan,kssndx),(kfnxt,wspmin),(ihrs,pekcon),
     *            (jdys,pekdos)
CC
      save nsub
CC
      data mlabl/'Yes',' No'/,nsub/1/
CC
CC**********************************************************************
CC
      if (kfnxt .eq. 1) go to 10
      if (kfnxt .eq. 2) go to 130
      if (ifrddm .eq. 0) ifrddm = 1
      iferrs = 0
CC      close previous plot.
      if (nplots .gt. 0.and.iswplt(29) .ne. 0) call PLTSQ(2)
CC      if error.
      if (iferrs .eq. 1.or.iferrs .eq. 2) go to 200
      ifnewp = 2
      if (IANYI(ipoptn,npoptn,1) .ne. 0) ifnewp = 1
      nbxpss = 0
      i = JINIT(ker,3,0,0)
      ifchlb = 0
CC
CC      start screen number (59).
   10 numsc = numscs(8) + 2
      if (ifmode .eq. 0) then
        if (nsourc .le. 1) ngrplt = -1
      else
        if (ngroup .le. 0.or.nsourc .le. 1) then
          ngrplt = -1
        else if (ngroup .gt. 0) then
          ngrplt = ngroup
        end if
      end if
CC
CC      display and read data for screen numsc.
CC
CC      screen numsc, dosage, concentration, etc.
      numf = IRDSC(numsc,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
CC
CC      indicate which quantities were calculated or not.
      nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
      mdumy = mscrnm(nsc,nln)
      do 20 i=1,4
        if (iswopt(i) .ne. 0) then
          call MVCH1(mlabl(1),mscrnm(40,nln+i+3),3)
          if (mscrnm(nsc,nln) .eq. mblnk1.or.
     *        IFCHR(mscrnm(nsc,nln),5) .ne. 0) then
            mscrnm(nsc,nln) = malpha(i)
          end if
        else
          call MVCH1(mlabl(2),mscrnm(40,nln+i+3),3)
        end if
   20 continue
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
        if (ngrplt .le. 0) then
          mscrnm(nsc,nln) = malpha(19)
        else
          mscrnm(nsc,nln) = malpha(7)
        end if
      end if
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
        i = IWTBF(mscrnm(nsc,nln),nch,cscale,4,iflrjs)
      end if
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
        i = IWTBF(mscrnm(nsc,nln),nch,dscale,4,iflrjs)
      end if
      if (iswplt(26) .eq. 0) then
CC        display screen menu.
        iferrs = IOSCN(numsc,mscrnm,ilinem,itabsm,ispacm,iferrm,
     *                 lstrw,0)
      else
        iferrs = 0
      end if
CC      save screen menu.
      if (iferrs .ne. 10) then
        j = 0
        if (IFCHR(mscrnm(nsc,nln),5) .ne. 0) then
          mnpbuf(1) = mscrnm(nsc,nln)
          mscrnm(nsc,nln) = mdumy
          j = 1
        end if
        i = IWTSC(numsc,1,mscrnm,ilinem,iferrm)
        if (j .gt. 0) then
          mscrnm(nsc,nln) = mnpbuf(1)
        end if
      end if
CC      save iferrs.
      jfers = iferrs
      ifwrt = 0
      ker(1) = IFWER(iferrm,nmxlin*nmxfld)
CC
CC      check user response to menu.
      if (jfers .eq. 1) then
CC        terminate.
        go to 200
      else if (jfers .eq. 2) then
CC        main menu.
        ifnext = 3
        go to 200
      else if (jfers .ge. 3.and.jfers .le. 7) then
CC        display aux. menus.
        iferrs = IFRSP(jfers,mscrnm,ilinem,itabsm,ispacm,iferrm,0)
        if (iferrs .eq. 1.or.iferrs .eq. 2) then
CC          terminate or main menu.
          ifnext = 3
          jfers = iferrs
          go to 200
        else
CC          redisplay menu.
          go to 10
        end if
      else if (jfers .eq. 8) then
        iswplt(26) = 1
      else if (jfers .ge. 9.and.jfers .le. 10) then
        go to 10
      else if (jfers .eq. 11) then
CC        go back to graphics initialization.
        ifnext = 8
        go to 200
      end if
CC      turn off error indicators for this menu.
      i = JINIT(iferrm,nmxlin*nmxfld,0,0)
CC
CC      check data.
CC
CC      get quantity to plot.
      nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
      ksssv = kquan
      if (mscrnm(nsc,nln) .ne. mblnk1) then
        if (iswopt(1) .ne. 0.and.IFCHR(mscrnm(nsc,nln),1) .ne. 0) then
CC          dosage.
          kquan = 1
        else if (iswopt(2) .ne. 0.and.IFCHR(mscrnm(nsc,nln),2) .ne. 0)
     *           then
CC          concentration.
          kquan = 2
        else if (iswopt(3) .ne. 0.and.IFCHR(mscrnm(nsc,nln),3) .ne. 0)
     *           then
CC          time-average concentration.
          kquan = 3
        else if (iswopt(4) .ne. 0.and.IFCHR(mscrnm(nsc,nln),4) .ne. 0)
     *           then
CC          gravitational deposition.
          kquan = 4
        else if (IFCHR(mscrnm(nsc,nln),5) .ne. 0) then
CC          quit graphics and return to main menu.
          iferrs = 2
          ifnext = 3
          go to 200
        else
          iferrm(nvl,1) = 1
        end if
        if (ksssv .ne. kquan) then
          if (nplots .gt. 0.and.iferrm(nvl,1) .ne. 1.and.
     *        IANYI(ipoptn,npoptn,1) .ne. 0) then
            pymins = rmissd
            pymaxs = -rmissd
            sclyes = rmissd
          end if
        end if
      else
        do 30 i=1,4
          if (iswopt(i) .ne. 0) then
            kquan = i
            go to 40
          end if
   30   continue
        iferrm(nvl,1) = 1
        iswplt(26) = 0
        ifnext = 3
        go to 200
      end if
   40 if (ksssv .ne. kquan) ifchlb = 1
CC
CC      see if sum of sources or source group.
      ksssv = ngrplt
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (ngrplt .ge. 0) then
        if (mscrnm(nsc,nln) .ne. mblnk1) then
          if (IFCHR(mscrnm(nsc,nln),19) .ne. 0) then
            ngrplt = 0
            if (ifmode .ne. 0.and.ngroup .gt. 0) iferrm(nvl,1) = 1
          else if (IFCHR(mscrnm(nsc,nln),7) .ne. 0) then
            if (ngrplt .le. 0) ngrplt = 1
            if (nsourc .le. 1) iferrm(nvl,1) = 1
            if (ifmode .ne. 0.and.ngroup .eq. 0) iferrm(nvl,1) = 1
          else
            iferrm(nvl,1) = 1
          end if
        else
          ngrplt = 0
          if (ifmode .ne. 0) then
            if (ngroup .le. 0) then
              ngrplt = 0
            else
              ngrplt = ngroup
            end if
          end if
        end if
      end if
CC
CC      scale factor C multiplier.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
      if (i .gt. 0) then
CC        error.
        iferrm(nvl,1) = 1
      else if (i .lt. 0) then
CC        default.
        ifwrt = 1
        cscale = 1.0
        i = IWTBF(mscrnm(nsc,nln),nch,cscale,1,iflrjs)
      else
CC        set C scale factor.
        cscale = xdumy
      end if
CC
CC      scale factor D add after multiply.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
      if (i .gt. 0) then
CC        error.
        iferrm(nvl,1) = 1
      else if (i .lt. 0) then
CC        default.
        ifwrt = 1
        dscale = 0.0
        i = IWTBF(mscrnm(nsc,nln),nch,dscale,1,iflrjs)
      else
CC        set D scale factor.
        dscale = xdumy
      end if
CC
      ner = IFWER(iferrm,nmxlin*nmxfld)
      if (ifwrt .gt. 0.or.ner .gt. 0.or.(ner .eq. 0.and.ker(1) .ne. 0))
     *    then
CC        rewrite screen data.
        i = IWTSC(numsc,1,mscrnm,ilinem,iferrm)
      end if
      if (ner .gt. 0) then
        if (KERRP(22,0) .ge. 1) go to 200
      end if
      ker(1) = ner
      if (ker(1) .eq. 1) then
        iswplt(26) = 0
        go to 10
      end if
      if (ksssv .ne. ngrplt) ifchlb = 1
   50 ksssv = ngrplt + 5 * ngrpct + 7 * ISUMI(jsogrp,2*nmxgrp)
CC
CC      source groups.
      if (ngrplt .gt. 0) then
        if (ifmode .eq. 0) then
CC          screen numscs(8)+3 (60), plot source groups.
          numsc = numscs(8) + 3
        else
CC          screen numscs(14)+4 (77), plot source groups.
          numsc = numscs(14) + 4
        end if
   60   numf = IRDSC(numsc,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1)      072497
        if (ifmode .ne. 0) then
          nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
          xdumy = nsub                                                  072497
          l = IWTBF(mscrnm(nsc,nln),3,xdumy,0,0)                        072497
          if (nsub .gt. 0.and.nsub .le. ngroup) then                    072497
            i = 0                                                       072497
   70       if (i .lt. 5) then                                          072497
              i = i + 1                                                 072497
              if (i+nsub-1 .le. ngroup) then                            072497
                n = 64                                                  072497
                nn = 0                                                  072497
                xdumy = i + nsub - 1                                    072497
                m = IWTBF(mscrnm(nsc-71,nln+5+(i-1)*3),2,xdumy,-2,0)    072497
                kk = MIN0(nsogrp(i+nsub-1),60)                          072497
                j = 0                                                   072497
                if (i+nsub-1 .gt. 1) then                               072497
                  do 80 k=1,i+nsub-2                                    072497
                    j = j + nsogrp(k)                                   072497
   80             continue                                              072497
                end if                                                  072497
                do 90 k=1,kk                                            072497
                  m = 0                                                 072497
                  nn = nn + 1                                           072497
                  if (nn .gt. 20) then                                  072497
                    n = 64                                              072497
                    nn = 1                                              072497
                  end if                                                072497
                  xdumy = isogrp(k+j)                                   072497
                  if (xdumy .lt. 0.0.and.ABS(xdumy) .gt. 9.0) m = 1     072497
                  ll = (i - 1) * 3 + (k + 19) / 20 - 1                  072497
                  l = IWTBF(mscrnm(nsc-n-m,nln+5+ll),2+m,xdumy,0,0)     072497
                  n = n - 3                                             072497
   90           continue                                                072497
              else                                                      072497
                call MBLK1(mscrnm(nsc-71,nln+5+(i-1)*3),3)              072497
              end if                                                    072497
              go to 70                                                  072497
            end if                                                      072497
          end if                                                        072497
        end if
        if (iswplt(26) .eq. 0) then
CC          display screen menu.
          iferrs = IOSCN(numsc,mscrnm,ilinem,itabsm,ispacm,iferrm,
     *                   lstrw,0)
        else
          iferrs = 0
        end if
CC        save screen menu.
        if (iferrs .ne. 10.and.ifmode .eq. 0) then                      072497
          i = IWTSC(numsc,1,mscrnm,ilinem,iferrm)                       072497
        end if                                                          072497
CC        save iferrs.
        jfers = iferrs
        ifwrt = 0
        ker(2) = IFWER(iferrm,nmxlin*nmxfld)
CC
CC        check user response to menu.
        if (jfers .eq. 1) then
CC          terminate.
          go to 200
        else if (jfers .eq. 2) then
CC          main menu.
          ifnext = 3
          go to 200
        else if (jfers .ge. 3.and.jfers .le. 7) then
CC          display aux. menus.
          iferrs = IFRSP(jfers,mscrnm,ilinem,itabsm,ispacm,iferrm,0)
          if (iferrs .eq. 1.or.iferrs .eq. 2) then
CC            terminate or main menu.
            ifnext = 3
            jfers = iferrs
            go to 200
          else
CC            redisplay menu.
            go to 50
          end if
        else if (jfers .eq. 8) then
          iswplt(26) = 1
        else if (jfers .ge. 9.and.jfers .le. 10) then
          go to 50
        else if (jfers .eq. 11) then
          go to 10
        end if
CC        turn off error indicators for this menu.
        i = JINIT(iferrm,nmxlin*nmxfld,0,0)
CC
CC        check data.
CC
CC        get source groups.
        if (ifmode .eq. 0) then
          ngrpct = 0
          i = JINIT(jsogrp,2*nmxgrp,0,0)
          do 110 nvl=1,15                                               072497
            nln = ilinem(nvl)
            do 100 i=1,10
              nsc = JWHER(nvl,itabsm,ispacm,i,nch)
              if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
                l = IRDBF(mscrnm(nsc,nln),nch,xdumy)
                if (l .ne. 0) then
                  iferrm(nvl,i) = 1
                else
                  if (ABS(xdumy) .gt. 0.0) then
                    if (ngrpct .lt. 2*nmxgrp) then
                      ngrpct = ngrpct + 1
                      jsogrp(ngrpct) = ABS(xdumy)
                      if (jsogrp(ngrpct) .gt. nsourc) then
                        jsogrp(ngrpct) = nsourc
                      end if
                      if (xdumy .lt. 0.0) then
                        jsogrp(ngrpct) = -jsogrp(ngrpct)
                      end if
                      if (jsogrp(ngrpct) .lt. 0.and.ngrpct .gt. 1) then
                        if (jsogrp(ngrpct-1) .gt. IABS(jsogrp(ngrpct)))
     *                      then
                          iferrm(nvls,is) = 1
                        end if
                      end if
                    end if
                  else
                    iferrm(nvl,i) = 1
                  end if
                end if
              end if
              nvls = nvl
              is = i
  100       continue
  110     continue
        else
          nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
          if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then                   072497
            i = IRDBF(mscrnm(nsc,nln),nch,xdumy)                        072497
            if (i .gt. 0) then                                          072497
CC              Not numeric.                                            072497
              j = LRJST(mscrnm(nsc,nln),nch,1)                          072497
CC              Check for N (next).                                     072497
              if (IFCHR(mscrnm(nsc,nln),14) .ne. 0) then                072497
CC                Display next set of groups.                           072497
                nsub = nsub + 5                                         072497
                if (nsub .gt. ngroup-4) nsub = ngroup - 4               072497
                go to 60                                                072497
CC              Check for P (previous).                                 072497
              else if (IFCHR(mscrnm(nsc,nln),16) .ne. 0) then           072497
                nsub = nsub - 5                                         072497
                if (nsub .lt. 1) nsub = 1                               072497
                go to 60                                                072497
              else                                                      072497
                iferrm(nvl,1) = 1                                       072497
              end if                                                    072497
            else if (i .lt. 0) then                                     072497
              iferrm(nvl,1) = 1                                         072497
            else                                                        072497
CC              Numeric value.                                          072497
              if (xdumy .lt. 0.0) then                                  072497
CC                set first of group to be displayed.                   072497
                nsub = xdumy                                            072497
                if (ABS(nsub) .gt. 0.and.ABS(nsub) .le. ngroup) then    072497
                  nsub = ABS(nsub)                                      072497
                  go to 60                                              072497
                end if                                                  072497
                iferrm(nvl,1) = 1                                       072497
              else                                                      072497
CC                Set group number to plot.                             072497
                ngrplt = xdumy                                          072497
                nsub = ngrplt                                           072497
              end if                                                    072497
            end if                                                      072497
            if (ngrplt .lt. 1.or.ngrplt .gt. nmxgrp) then               072497
              iferrm(nvl,1) = 1
            end if
            if (ngrplt .ge. 1.and.ngrplt .le. ngroup) then
              ngrpct = 0
              i = JINIT(jsogrp,2*nmxgrp,0,0)
              i = 0
              k = 0
              l = 0
  120         if (i .lt. ngroup.and.ngrpct .eq. 0) then
                i = i + 1
                if (i .eq. ngrplt) then
                  ngrpct = nsogrp(i)
                  l = JINIT(jsogrp,ngrpct,isogrp(k+1),1)
                end if
                k = k + nsogrp(i)
                go to 120
              end if
              if (ngrpct .eq. 0) iferrm(nvl,1) = 1
            else
              iferrm(nvl,1) = 1
            end if
          else
            ngrplt = 1
            ngrpct = nsogrp(1)
            i = JINIT(jsogrp,ngrpct,isogrp,1)
            ifwrt = 1
            mscrnm(nsc,nln) = malpha(1)
          end if
        end if
CC
        if (ngrpct .le. 0) iferrm(1,1) = 1
        ner = IFWER(iferrm,nmxlin*nmxfld)
        if (ifwrt .gt. 0.or.ner .gt. 0.or.(ner .eq. 0.and.
     *      ker(2) .ne. 0)) then
CC          rewrite screen data.
          i = IWTSC(numsc,1,mscrnm,ilinem,iferrm)
        end if
        if (ner .gt. 0) then
          if (KERRP(22,0) .ge. 1) go to 200
        end if
        ker(2) = ner
        if (ker(2) .eq. 1) then
          iswplt(26) = 0
          go to 60                                                      072497
        end if
      end if
      if (ksssv .ne. ngrplt + 5*ngrpct + 7*ISUMI(jsogrp,2*nmxgrp))
     *    ifchlb = 1
CC
CC
      ksssv = ioffst(2)
      if (nxyfnc .eq. 0) then
CC        grid system calculations.
        ioffst(2) = 0
      else
CC        single axis calculations.
        ioffst(2) = -1
      end if
      if (ksssv .ne. ioffst(2)) ifchlb = 1
CC
CC      plot distance versus quantity, isopleths, etc.
  130 if (nxyfnc .gt. 0) then
        if (ifmode .eq. 0) then
CC          screen (61).
          numsc = numscs(8) + 4
          iavmn = 0
        else
CC          screen (75)
          numsc = numscs(14) + 2
          iavmn = 1
        end if
      else
        if (ifmode .eq. 0) then
CC          screen (62).
          numsc = numscs(8) + 5
          iavmn = 0
        else
CC          screen (76).
          numsc = numscs(14) + 3
          iavmn = 1
        end if
      end if
      numf = IRDSC(numsc,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
CC
CC        place selected quantity name in menu.
      nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
      jm = kquan
      if (kquan .le. 2) then
        jm = kquan
      else if (kquan .eq. 3) then
        jm = kquan + 2
      else if (kquan .eq. 4) then
        jm = kquan + 3
      end if
      if (nxyfnc .gt. 0) then
        call MVCH1(modelt(jm),mscrnm(23,nln+1),14)
      else
        call MVCH1(modelt(jm),mscrnm(23,nln+4),14)
        call MVCH1(modelt(jm),mscrnm(40,nln+5),14)
        call MVCH1(modelt(jm),mscrnm(40,nln+6),14)
      end if
CC      place default average type in menu.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (ifhrly .gt. 0) then
        if (ifavrg .le. iavmn) ifavrg = 0
        lsv = ifavrg
        if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
          if (iswopt(12) .gt. 0) then
            mdumy = malpha(5)
            lsv = 5
          else if (ifanul .gt. 0) then
            mdumy = malpha(4)
            lsv = 4
          else if (ifsean .eq. 1) then
            mdumy = malpha(2)
            lsv = 2
          else if (ifsean .eq. 2) then
            mdumy = malpha(3)
            lsv = 3
          else
            if (ifmode .eq. 0) then
              mdumy = malpha(1)
              lsv = 1
            else
              mdumy = mblnk1
              lsv = 0
            end if
          end if
          mscrnm(nsc,nln) = mdumy
        end if
      else
        mscrnm(nsc,nln) = mblnk1
      end if
CC      place number of hours in average in menu.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (ifhrly .gt. 0) then
        if (lsv .eq. 1.or.lsv .ge. 5) then
          if (ifmode .ne. 0) then
            k = 23
            do 140 i=1,6
              if (navhrs(i) .gt. 0) then
                xdumy = navhrs(i)
                j = IWTBF(mscrnm(nsc-k,nln-1),2,xdumy,0,0)
                k = k - 3
              end if
  140       continue
          end if
          if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
CC            place default averaging time in menu.
            if (nhravg .gt. 0) then
              xdumy = nhravg
            else
              xdumy = 0.0
              i = 0
  150         if (i .lt. 6.and.xdumy .le. 0.0) then
                i = i + 1
                if (navhrs(i) .gt. 0) then
                  xdumy = navhrs(i)
                end if
                go to 150
              end if
              if (xdumy .le. 0.0) then
                xdumy = 1
              end if
            end if
            i = IWTBF(mscrnm(nsc,nln),2,xdumy,0,iflrjs)
          end if
        else
          call MBLK1(mscrnm(nsc,nln),nch)
        end if
      else
        call MBLK1(mscrnm(nsc,nln),nch)
      end if
CC      place start time or season or quarter in menu.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (ifhrly .le. 0) then
        call MBLK1(mscrnm(nsc,nln),nch)
      else
        if (lsv .ge. 4) then
          call MBLK1(mscrnm(nsc,nln),nch)
        else
          if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
            xdumy = 1.0
            i = IWTBF(mscrnm(nsc,nln),nch,xdumy,0,iflrjs)
          end if
        end if
      end if
CC
      if (iswplt(26) .eq. 0) then
CC        display screen menu.
        iferrs = IOSCN(numsc,mscrnm,ilinem,itabsm,ispacm,iferrm,
     *                 lstrw,0)
      else
        iferrs = 0
      end if
CC      save screen menu.
      if (iferrs .ne. 10) i = IWTSC(numsc,1,mscrnm,ilinem,iferrm)
CC      save iferrs.
      jfers = iferrs
      ifwrt = 0
      ker(3) = IFWER(iferrm,nmxlin*nmxfld)
CC
CC      check user response to menu.
      if (jfers .eq. 1) then
CC        terminate.
        go to 200
      else if (jfers .eq. 2) then
CC        main menu.
        ifnext = 3
        go to 200
      else if (jfers .ge. 3.and.jfers .le. 7) then
CC        display aux. menus.
        iferrs = IFRSP(jfers,mscrnm,ilinem,itabsm,ispacm,iferrm,0)
        if (iferrs .eq. 1.or.iferrs .eq. 2) then
CC          terminate or main menu.
          ifnext = 3
          jfers = iferrs
          go to 200
        else
CC          redisplay menu.
          go to 130
        end if
      else if (jfers .eq. 8) then
        iswplt(26) = 1
      else if (jfers .ge. 9.and.jfers .le. 10) then
        go to 130
      else if (jfers .eq. 11) then
CC        go back to previous menu.
        if (ngrplt .le. 0) go to 10
        go to 50
      end if
CC      turn off error indicators for this menu.
      i = JINIT(iferrm,nmxlin*nmxfld,0,0)
CC
CC      check data.
CC
CC      get contents of plot, set number of possible options.
  160 ksssv = npoptn
      ier = 0
      npoptn = 0
      jpoptn = 0
      i = JINIT(ipoptn,10,0,0)
      if (nxyfnc .gt. 0) then
        j = 2
        l = 2
      else
        j = 5
        l = 7
      end if
      nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
CC      loop over fields.
      do 180 i=1,j
        nsc = JWHER(nvl,itabsm,ispacm,i,nch)
        if (mscrnm(nsc,nln) .ne. mblnk1) then
CC          loop over poss. options.
          do 170 k=1,l
            if (IFCHR(mscrnm(nsc,nln),k) .ne. 0) then
              if (l .eq. 7.and.k .le. 3.and.iftran .le. 0) then
                iferrm(nvl,i) = 1
                ier = 1
                go to 180
              end if
              npoptn = npoptn + 1
              if (nxyfnc .gt. 0) then
                if (k .eq. 1) then
                  ipoptn(npoptn) = 1
                else
                  ipoptn(npoptn) = 5
                end if
              else
                if (k .eq. 1) then
                  ipoptn(npoptn) = 2
                else if (k .eq. 2) then
                  ipoptn(npoptn) = 3
                else if (k .eq. 3) then
                  ipoptn(npoptn) = 4
                else if (k .eq. 4) then
                  ipoptn(npoptn) = -2
                else if (k .eq. 5) then
                  ipoptn(npoptn) = -3
                else if (k .eq. 6) then
                  ipoptn(npoptn) = -4
                else if (k .eq. 7) then
                  ipoptn(npoptn) = 5
                end if
              end if
              go to 180
            end if
  170     continue
          iferrm(nvl,i) = 1
          ier = 1
        end if
  180 continue
      if (npoptn .eq. 0) then
        npoptn = 1
        if (nxyfnc .gt. 0) then
          ipoptn(npoptn) = 1
        else
          ipoptn(npoptn) = -3
        end if
      end if
      if (ksssv .ne. npoptn.and.ier .eq. 0) ifchlb = 1
      if (ifhrly .gt. 0) then
CC
CC        get average type.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        ksssv = ifavrg
CC        ifavrg - averaging type for plotting.
CC              1 - single N-hour average event.
CC              2 - seasonal average.
CC              3 - quarterly average.
CC              4 - annual average.
CC              5 - highest N-hour average.
CC              6 - second highest N-hour average.
        if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
          ifavrg = IFFOP(mscrnm(nsc,nln),6) + 1
          if (ifavrg .eq. 4) nhravg = ntothr
          if (ifavrg .le. iavmn) then
            iferrm(nvl,1) = 1
          end if
          if (ifavrg .ge. 2) ifrddm = 1
        else
CC          field is blank, default to first averaging time.
          if (ifavrg .le. iavmn.or.ifavrg .gt. 6) then
            if (ifmode .eq. 0) then
              ifavrg = 1
            else
              if (iswopt(12) .gt. 0) then
                ifavrg = 5
              else if (ifanul .gt. 0) then
                ifavrg = 4
              else if (ifsean .eq. 1) then
                ifavrg = 2
              else if (ifsean .eq. 2) then
                ifavrg = 3
              else
                ifavrg = 0
              end if
            end if
          end if
          if (ifavrg .gt. 0) then
            ifwrt = 1
            mscrnm(nsc,nln) = malpha(ifavrg)
          else
            iferrm(nvl,1) = 1
          end if
        end if
        if (ifmode .ne. 0.and.iferrm(nvl,1) .eq. 1) then
          if (iswopt(12) .eq. 0.and.ifanul .eq. 0.and.ifsean .eq. 0)
     *        then
            xdum = 0.0
            call MBLK1(mnpbuf,22)
            call MVCH1(modelt(jm),mnpbuf,14)
            i = KERRS(28,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0)
            go to 10
          end if
        end if
        if (ksssv .ne. ifavrg.and.iferrm(nvl,1) .ne. 1) ifchlb = 1
CC
CC        get number of hours in average.
        ksssv = nhravg
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        if (ifavrg .le. 1.or.ifavrg .ge. 5) then
          if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
            i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
            if (i .gt. 0) then
CC              error.
              iferrm(nvl,1) = 1
            else if (i .lt. 0) then
CC              default.
              ifwrt = 1
              if (navhrs(1) .gt. 0) then
                xdumy = navhrs(1)
              else
                xdumy = 1.0
              end if
              i = IWTBF(mscrnm(nsc,nln),nch,xdumy,0,iflrjs)
            else
CC              set averaging time.
              nhravg = xdumy
            end if
          end if
          if (ifmode .ne. 0) then
            j = 0
            i = 0
  190       if (i .lt. 6.and.j .eq. 0) then
              i = i + 1
              if (nhravg .eq. navhrs(i)) then
                j = 1
              end if
              go to 190
            end if
            if (j .eq. 0) iferrm(nvl,1) = 1
          end if
          if (ksssv .ne. nhravg) ifchlb = 1
        end if
CC
CC        get start hour, season number, quarter number or number of
CC        hours in average.
        if (ifavrg .gt. iavmn.and.ifavrg .le. 3) then
          ksssv = (ifrddm + 2) + iyrs + jdys + ihrs
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          n = LRJST(mscrnm(nsc,nln),nch,1)
          i = LRJST(mscrnm(nsc,nln),nch,iflrjs)
          if (n .gt. 0) then
            i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
            if (i .ne. 0) then
              iferrm(nvl,1) = 1
            else if (ifavrg .eq. 1) then
              if (n .eq. 8) then
CC                8 digit number, assume YYMMDDHH start date.
                i = xdumy
                iyrs = i / 10**6
                i = i - (iyrs * 10**6)
                imos = i / 10**4
                i = i - (imos * 10**4)
                idys = i / 10**2
                ihrs = i - (idys * 10**2)
                if (imos .lt. vllimt(2).or.imos .gt. vulimt(2).or.
     *              idys .lt. vllimt(3).or.idys .gt. vulimt(3).or.
     *              ihrs .lt. vllimt(4).or.ihrs .gt. vulimt(4)) then
                  iferrm(nvl,1) = 1
                else
                  jdys = 0
                  call JULAN(iyrs,imos,idys,jdys)
                  ifrddm = -1
                end if
              else if (n .eq. 7) then
                i = xdumy
                iyrs = i / 10**5
                i = i - (iyrs * 10**5)
                jdys = i / 10**2
                ihrs = i - (jdys * 10**2)
                if (jdys .lt. vllimt(5).or.jdys .gt. vulimt(5).or.
     *              ihrs .lt. vllimt(4).or.ihrs .gt. vulimt(4)) then
                  iferrm(nvl,1) = 1
                else
                  call JULAN(iyrs,imos,idys,jdys)
                  ifrddm = -1
                end if
              else
                if (xdumy .gt. 0.0.and.xdumy .le. ntothr) then
                  ifrddm = xdumy
                else
                  iferrm(nvl,1) = 1
                end if
              end if
            else if (ifavrg .eq. 2.or.ifavrg .eq. 3) then
              i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
              if (i .ne. 0) then
                iferrm(nvl,1) = 1
              else
                if (xdumy .gt. 0.0.and.xdumy .lt. 5) then
                  iseasn = xdumy
                  ifrddm = 1
                  if (ifsean .eq. 1) then
                    nhravg = nseasn(iseasn)
                  else
                    nhravg = nquart(iseasn)
                  end if
                else
                  iferrm(nvl,1) = 1
                end if
              end if
            end if
          else
CC            default start hour, season # or N-hour
            xdumy = 0
            if (ifavrg .eq. 1) then
CC              default start time
              ifrddm = ifrddm + 1
              if (ifrddm .gt. ntothr) ifrddm = 1
              ifrddm = MAX0(ifrddm,1)
              xdumy = ifrddm
            else if (ifavrg .eq. 2.or.ifavrg .eq. 3) then
CC              default season or quarter.
              iseasn = iseasn + 1
              if (iseasn .gt. 4) iseasn = 1
              if (ifsean .eq. 1) then
                nhravg = nseasn(iseasn)
              else
                nhravg = nquart(iseasn)
              end if
              ifrddm = 1
              xdumy = iseasn
            end if
            ifwrt = 1
            i = iwtbf(mscrnm(nsc,nln),nch,xdumy,0,iflrjs)
          end if
          if (ksssv .ne. (ifrddm+2)+iyrs+jdys+ihrs.and.
     *        iferrm(nvl,1) .ne. 1) ifchlb = 1
        end if
CC
        ner = IFWER(iferrm,nmxlin*nmxfld)
        if (ifwrt .gt. 0.or.ner .gt. 0.or.(ner .eq. 0.and.
     *      ker(3) .ne. 0)) then
CC          rewrite screen data.
          i = IWTSC(numsc,1,mscrnm,ilinem,iferrm)
        end if
        if (ner .gt. 0) then
          if (KERRP(22,0) .ge. 1) go to 200
        end if
        ker(3) = ner
        if (ker(3) .eq. 1) then
          iswplt(26) = 0
          go to 130
        end if
      else
        ifavrg = 0
      end if
      iferrs = 0
CC      check if data complete.
      if (IFWER(ker,3) .eq. 1) then
        ifset = -1
      else
        ifset = 1
        if (ifgrph .le. 0) ifset = 0
      end if
      ifrdwt(6) = ifset
      if (ifset .le. 0) then
        ifnext = 3
      end if
  200 return
      end
