      Subroutine RPLTD
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - RPLTD Version 01.2                          ---
CC--- Subroutine description - plot details.                        ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
CC
      dimension ker(6),ioptn(16),idumy(16)
CC
      include 'OCNTRL.INC'
      include 'OCDSPM.INC'
      include 'OWNTRL.INC'
      include 'OCALCS.INC'
      include 'ONAMES.INC'
CC
      equivalence (idumy(1),factrs),(ioptn(1),sigzls),(ker(1),delthp),
     *            (iflg,sigyls),(jfers,pekcna),(kfnxt,wspmin)
CC
CC**********************************************************************
CC
      ifnxt = ifnext
      if (ibackd+ibackt .le. 0.and.ifored+iforet .ge. 14) then
        ifclr = 0
      else
        ifclr = 1
      end if
      iferrs = 0
      if (kfnxt .eq. 2) go to 110
      if (kfnxt .eq. 1) go to 20
      i = JINIT(ker,6,0,0)
      isw2l = iswplt(2)
      iflg = 0
CC
CC        screen numsc (66), plot details.
   10 numsc = numscs(8) + 9
      numf = IRDSC(numsc,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
CC      provide defaults.
      nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
      if (mscrnm(nsc,nln) .eq. mblnk1.or.nplots .le. 0) then
        if (iswopt(2) .eq. 0.and.nxyfnc .gt. 0.and.nplots .le. 0) then
          iswplt(2) = 3
        end if
        mscrnm(nsc,nln) = malpha(iswplt(2)+1)
      end if
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (iswplt(27) .ge. 0.and.(mscrnm(nsc,nln) .eq. mblnk1.or.
     *    nplots .le. 0)) then
        mscrnm(nsc,nln) = malpha(iswplt(27)+1)
      end if
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (iswplt(28) .ge. 0.and.(mscrnm(nsc,nln) .eq. mblnk1.or.
     *    nplots .le. 0)) then
        mscrnm(nsc,nln) = malpha(iswplt(28)+1)
      end if
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (iswplt(4) .ge. 0.and.(mscrnm(nsc,nln) .eq. mblnk1.or.
     *    nplots .le. 0)) then
        mscrnm(nsc,nln) = malpha(iswplt(4)+1)
      end if
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (iswplt(5) .ge. 0.and.(mscrnm(nsc,nln) .eq. mblnk1.or.
     *    nplots .le. 0)) then
        mscrnm(nsc,nln) = malpha(iswplt(5)+1)
      end if
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (iswplt(3) .ge. 0.and.(mscrnm(nsc,nln) .eq. mblnk1.or.
     *    nplots .le. 0)) then
        mscrnm(nsc,nln) = malpha(iswplt(3)+1)
      end if
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (iswplt(7) .ge. 0.and.(mscrnm(nsc,nln) .eq. mblnk1.or.
     *    nplots .le. 0)) then
        mscrnm(nsc,nln) = malpha(iswplt(7)+1)
      end if
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (iswplt(10) .ge. 0.and.(mscrnm(nsc,nln) .eq. mblnk1.or.
     *    nplots .le. 0)) then
        mscrnm(nsc,nln) = malpha(iswplt(10)+1)
      end if
CC      display screen menu.
      iferrs = IOSCN(numsc,mscrnm,ilinem,itabsm,ispacm,iferrm,lstrw,0)
CC      save screen menu.
      if (iferrs .ne. 10) i = IWTSC(numsc,1,mscrnm,ilinem,iferrm)
CC      save iferrs.
      jfers = iferrs
      ifwrt = 0
      ker(1) = IFWER(iferrm,nmxlin*nmxfld)
CC
CC      check user response to menu.
      if (jfers .eq. 1) then
CC        terminate.
        go to 280
      else if (jfers .eq. 2) then
CC        main menu.
        ifnext = 3
        go to 280
      else if (jfers .ge. 3.and.jfers .le. 7) then
CC        display aux. menus.
        iferrs = IFRSP(jfers,mscrnm,ilinem,itabsm,ispacm,iferrm,0)
        if (iferrs .eq. 1.or.iferrs .eq. 2) then
CC          terminate or main menu.
          jfers = iferrs
          ifnext = 3
          go to 280
        else
CC          redisplay menu.
          go to 10
        end if
      else if (jfers .eq. 8) then
        ifnext = 20
      else if (jfers .ge. 9.and.jfers .le. 10) then
        go to 10
      else if (jfers .eq. 11) then
        ifnext = 17
        go to 280
      end if
CC      turn off error indicators for this menu.
      i = JINIT(iferrm,nmxlin*nmxfld,0,0)
CC
CC      check data.
CC
CC      get log/linear flag.
      nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        j = IFFOP(mscrnm(nsc,nln),4)
        if (j .lt. 0) then
          iferrm(nvl,1) = 1
        else
          iswplt(2) = j
        end if
      else
        if (nxyfnc .eq. 0) then
          iswplt(2) = 0
          mscrnm(nsc,nln) = malpha(1)
        else
          iswplt(2) = 3
          mscrnm(nsc,nln) = malpha(4)
        end if
        ifwrt = 1
      end if
      if (iswplt(2) .ne. isw2l) then
        iflg = 1
        if (IANYI(ipoptn,npoptn,1) .ne. 0) then
          call DMXMQ(0,functx,functy,nxyfnc,nxyfnc,0)
        else
          if (JANYI(ipoptn,npoptn,-2) .gt. 0.or.
     *        JANYI(ipoptn,npoptn,-3) .gt. 0.or.
     *        JANYI(ipoptn,npoptn,-4) .gt. 0) then
            call DMXMQ(0,xcoord,ycoord,nxpnts,nypnts,ifgpol)
          else if (JANYI(ipoptn,npoptn,2) .gt. 0.or.
     *             JANYI(ipoptn,npoptn,3) .gt. 0.or.
     *             JANYI(ipoptn,npoptn,4) .gt. 0) then
            call DMXMQ(0,xteran,yteran,nxtran,nytran,iftpol)
          else
            call DMXMQ(0,xcoord,ycoord,nxpnts,nypnts,ifgpol)
          end if
        end if
      end if
      isw2l = iswplt(2)
CC
CC      get north arrow flag.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        j = IFFOP(mscrnm(nsc,nln),2)
        if (j .lt. 0) then
          iferrm(nvl,1) = 1
        else
          iswplt(27) = j
        end if
      else
        iswplt(27) = 0
        mscrnm(nsc,nln) = malpha(iswplt(27)+1)
        ifwrt = 1
      end if
CC
CC      get background map flag.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        j = IFFOP(mscrnm(nsc,nln),3)
        if (j .lt. 0) then
          iferrm(nvl,1) = 1
        else
          iswplt(28) = j
          if (ifclr .eq. 0.and.iswplt(28) .gt. 1) iswplt(28) = 1
        end if
      else
        iswplt(28) = 0
        mscrnm(nsc,nln) = malpha(iswplt(28)+1)
        ifwrt = 1
      end if
CC
CC      get plot axes and label coordinate positions flag.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        j = IFFOP(mscrnm(nsc,nln),3)
        if (j .lt. 0) then
          iferrm(nvl,1) = 1
        else
          iswplt(4) = j
        end if
      else
        iswplt(4) = 0
        mscrnm(nsc,nln) = malpha(iswplt(4)+1)
        ifwrt = 1
      end if
CC
CC      get horizontal/vertical relationship flag.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        j = IFFOP(mscrnm(nsc,nln),2)
        if (j .lt. 0) then
          iferrm(nvl,1) = 1
        else
          iswplt(5) = j
        end if
      else
        iswplt(5) = 0
        mscrnm(nsc,nln) = malpha(iswplt(5)+1)
        ifwrt = 1
      end if
CC
CC      get smooth or point to point flag.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        j = IFFOP(mscrnm(nsc,nln),2)
        if (j .ge. 0) then
          iswplt(3) = j
        else
          iferrm(nvl,1) = 1
        end if
      else
        iswplt(3) = 0
        mscrnm(nsc,nln) = malpha(iswplt(3)+1)
        ifwrt = 1
      end if
CC
CC      get center or left justify labels flag.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        j = IFFOP(mscrnm(nsc,nln),2)
        if (j .ge. 0) then
          iswplt(7) = j
        else
          iferrm(nvl,1) = 1
        end if
      else
        iswplt(7) = 0
        mscrnm(nsc,nln) = malpha(iswplt(7)+1)
        ifwrt = 1
      end if
CC
CC      get major/minor tick mark flag.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        j = IFFOP(mscrnm(nsc,nln),2)
        if (j .lt. 0) then
          iferrm(nvl,1) = 1
        else
          iswplt(10) = j
        end if
      else
        iswplt(10) = 0
        mscrnm(nsc,nln) = malpha(iswplt(10)+1)
        ifwrt = 1
      end if
CC
CC
      ner = IFWER(iferrm,nmxlin*nmxfld)
      if (ifwrt .gt. 0.or.ner .gt. 0.or.(ner .eq. 0.and.
     *    ker(1) .ne. 0)) then
CC        rewrite screen data.
        i = IWTSC(numsc,1,mscrnm,ilinem,iferrm)
      end if
      if (ner .gt. 0) then
        if (KERRP(22,0) .ge. 1) go to 280
      end if
      ker(1) = ner
      if (ker(1) .eq. 1) then
        ifnext = ifnxt
        go to 10
      end if
      if (jfers .eq. 8) go to 280
CC
CC
CC        screen numsc (67), plot details line type.
   20 numsc = numscs(8) + 10
      numf = IRDSC(numsc,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
CC      indicate dispersion quantity in menu
      nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
      call MVCH1(mscrch(ilbstr(4)),mscrnm(3,nln),MIN0(14,nlbsch(4)))
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,2,1,nln,nsc,nch)
      call MVCH1(mscrch(ilbstr(4)),mscrnm(19,nln),MIN0(14,nlbsch(4)))
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,3,1,nln,nsc,nch)
      call MVCH1(mscrch(ilbstr(4)),mscrnm(3,nln),MIN0(14,nlbsch(4)))
      if (IANYI(ipoptn,npoptn,1) .ne. 0.and.nxyfnc .gt. 0) then
        nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
        if (mscrnm(nsc,nln) .eq. mblnk1.or.nplots .le. 0) then
          mscrnm(nsc,nln) = malpha(iswplt(6)+1)
        end if
      else
        if (IANYI(ipoptn,npoptn,2) .ne. 0) then
          if (iftran .gt. 0) then
            nvl = IWHER(2,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
            if (mscrnm(nsc,nln) .eq. mblnk1.or.nplots .le. 0) then
              mscrnm(nsc,nln) = malpha(iswplt(6)+1)
            end if
          end if
          nvl = IWHER(3,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
          if (mscrnm(nsc,nln) .eq. mblnk1.or.nplots .le. 0) then
            mscrnm(nsc,nln) = malpha(iswplt(22)+1)
          end if
        end if
        if (JANYI(ipoptn,npoptn,3) .gt. 0.or.JANYI(ipoptn,npoptn,4) .gt.
     *      0) then
          n = 0
          nvl = 3
          do 40 j=1,2
            nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
            do 30 i=1,10
              n = n + 1
              if (n .le. ncontr(1)) then
                nsc = JWHER(nvl,itabsm,ispacm,i,nch)
                if (mscrnm(nsc,nln) .eq. mblnk1.or.nplots .le. 0) then
                  mscrnm(nsc,nln) = malpha(icndsh(n,1)+1)
                end if
              end if
   30       continue
   40     continue
        end if
        if (JANYI(ipoptn,npoptn,-3) .gt. 0.or.JANYI(ipoptn,npoptn,-4)
     *      .gt. 0) then
          n = 0
          nvl = 5
          do 60 j=1,2
            nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
            do 50 i=1,10
              n = n + 1
              if (n .le. ncontr(2)) then
                nsc = JWHER(nvl,itabsm,ispacm,i,nch)
                if (mscrnm(nsc,nln) .eq. mblnk1.or.nplots .le. 0) then
                  mscrnm(nsc,nln) = malpha(icndsh(n,2)+1)
                end if
              end if
   50       continue
   60     continue
        end if
      end if
CC      display screen menu.
      iferrs = IOSCN(numsc,mscrnm,ilinem,itabsm,ispacm,iferrm,lstrw,0)
CC      save screen menu.
      if (iferrs .ne. 10) i = IWTSC(numsc,1,mscrnm,ilinem,iferrm)
CC      save iferrs.
      jfers = iferrs
      ifwrt = 0
      ker(2) = IFWER(iferrm,nmxlin*nmxfld)
CC
CC      check user response to menu.
      if (jfers .eq. 1) then
CC        terminate.
        go to 280
      else if (jfers .eq. 2) then
CC        main menu.
        ifnext = 3
        go to 280
      else if (jfers .ge. 3.and.jfers .le. 7) then
CC        display aux. menus.
        iferrs = IFRSP(jfers,mscrnm,ilinem,itabsm,ispacm,iferrm,0)
        if (iferrs .eq. 1.or.iferrs .eq. 2) then
CC          terminate or main menu.
          jfers = iferrs
          ifnext = 3
          go to 280
        else
CC          redisplay menu.
          go to 20
        end if
      else if (jfers .eq. 8) then
        ifnext = 20
      else if (jfers .ge. 9.and.jfers .le. 10) then
        go to 20
      else if (jfers .eq. 11) then
        go to 10
      end if
CC      turn off error indicators for this menu.
      i = JINIT(iferrm,nmxlin*nmxfld,0,0)
CC
CC      check data.
      if (nxyfnc .gt. 0) then
CC
CC        get dispersion as a function of distance line type flag.
        nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
        if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
          j = IFFOP(mscrnm(nsc,nln),16)
          if (j .lt. 0) then
            iferrm(nvl,1) = 1
          else
            iswplt(6) = j
          end if
        else
          iswplt(6) = 0
          mscrnm(nsc,nln) = malpha(iswplt(6)+1)
          ifwrt = 1
        end if
      else
CC
CC        get line type for line of maximum terrain.
        nvl = IWHER(2,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
        if (JANYI(ipoptn,npoptn,2) .ne. 0) then
          if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
            j = IFFOP(mscrnm(nsc,nln),16)
            if (j .lt. 0) then
              iferrm(nvl,1) = 1
            else
              iswplt(6) = j
            end if
          else
            iswplt(6) = 0
            mscrnm(nsc,nln) = malpha(iswplt(6)+1)
            ifwrt = 1
          end if
        end if
CC
CC        get line type for line of maximum dispersion.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        if (JANYI(ipoptn,npoptn,-2) .ne. 0) then
          if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
            j = IFFOP(mscrnm(nsc,nln),16)
            if (j .lt. 0) then
              iferrm(nvl,1) = 1
            else
              iswplt(22) = j
            end if
          else
            iswplt(22) = 0
            mscrnm(nsc,nln) = malpha(iswplt(22)+1)
            ifwrt = 1
          end if
        end if
        nn = 2
CC
CC        get line type for terrain contour levels.
        if (JANYI(ipoptn,npoptn,3) .ne. 0.or.JANYI(ipoptn,npoptn,4)
     *      .ne. 0) then
          n = 0
          nn = 0
          do 80 k=1,2
            nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
            do 70 j=1,10
              n = n + 1
              if (n .le. ncontr(1)) then
                nsc = JWHER(nvl,itabsm,ispacm,j,nch)
                if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
                  l = IFFOP(mscrnm(nsc,nln),16)
                  if (l .ge. 0) then
                    icndsh((k-1)*10+j,1) = l
                  else
                    iferrm(nvl,j) = 1
                  end if
                else
                  icndsh((k-1)*10+j,1) = 0
                  mscrnm(nsc,nln) = malpha(icndsh((k-1)*10+j,1)+1)
                  ifwrt = 1
                end if
              end if
   70       continue
   80     continue
        end if
CC
CC        get line type for dispersion isopleth levels.
        if (JANYI(ipoptn,npoptn,-3) .ne. 0.or.JANYI(ipoptn,npoptn,-4)
     *      .ne. 0) then
          nvl = nvl + nn
          n = 0
          do 100 k=1,2
            nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
            do 90 j=1,10
              n = n + 1
              if (n .le. ncontr(2)) then
                nsc = JWHER(nvl,itabsm,ispacm,j,nch)
                if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
                  l = IFFOP(mscrnm(nsc,nln),16)
                  if (l .ge. 0) then
                    icndsh((k-1)*10+j,2) = l
                  else
                    iferrm(nvl,j) = 1
                  end if
                else
                  icndsh((k-1)*10+j,2) = 0
                  mscrnm(nsc,nln) = malpha(icndsh((k-1)*10+j,2)+1)
                  ifwrt = 1
                end if
              end if
   90       continue
  100     continue
        end if
      end if
CC
CC
      ner = IFWER(iferrm,nmxlin*nmxfld)
      if (ifwrt .gt. 0.or.ner .gt. 0.or.(ner .eq. 0.and.
     *    ker(2) .ne. 0)) then
CC        rewrite screen data.
        i = IWTSC(numsc,1,mscrnm,ilinem,iferrm)
      end if
      if (ner .gt. 0) then
        if (KERRP(22,0) .ge. 1) go to 280
      end if
      ker(2) = ner
      if (ker(2) .eq. 1) then
        ifnext = ifnxt
        go to 20
      end if
      if (jfers .eq. 8) go to 280
      if (ifclr .eq. 0) go to 250
CC
CC          screen numsc (68), plot details line color.
CC
CC          subroutine SPDSP displays the default color options by a
CC          call from OBSIO, checking for screen 68.
  110 numsc = numscs(8) + 11
      if (ipmode .lt. 16.and.isystm .eq. 2) then
        ncolr = 4
      else
        ncolr = 16
      end if
      numf = IRDSC(numsc,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
CC      indicate dispersion quantity in menu
      nvl = IWHER(3,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
      call MVCH1(mscrch(ilbstr(4)),mscrnm(3,nln),MIN0(14,nlbsch(4)))
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,2,1,nln,nsc,nch)
      call MVCH1(mscrch(ilbstr(4)),mscrnm(19,nln),MIN0(14,nlbsch(4)))
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,3,1,nln,nsc,nch)
      call MVCH1(mscrch(ilbstr(4)),mscrnm(3,nln),MIN0(14,nlbsch(4)))
CC      set plot background color default.
      nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
      call LSCLQ(1,0,ioptn)
      if (mscrnm(nsc,nln) .eq. mblnk1.or.nplots .le. 0) then
        mscrnm(nsc,nln) = malpha(iswplt(9)+1)
      end if
CC      set axes and label color default.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      call LSCLQ(2,0,idumy)
      if (mscrnm(nsc,nln) .eq. mblnk1.or.nplots .le. 0) then
        mscrnm(nsc,nln) = malpha(iswplt(16)+1)
      end if
      if (IANYI(ipoptn,npoptn,1) .ne. 0.and.nxyfnc .gt. 0) then
CC        set dispersion as a function of distance line color default.
        call LSCLQ(3,1,idumy)
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        if (mscrnm(nsc,nln) .eq. mblnk1.or.nplots .le. 0) then
          mscrnm(nsc,nln) = malpha(iswplt(12)+1)
        end if
      else
        if (JANYI(ipoptn,npoptn,2) .gt. 0) then
CC          set line color for line of maximum terrain default.
          nvl = IWHER(4,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
          call LSCLQ(3,1,idumy)
          if (iftran .gt. 0.and.(mscrnm(nsc,nln) .eq. mblnk1.or.
     *        nplots .le. 0)) then
            mscrnm(nsc,nln) = malpha(iswplt(12)+1)
          end if
        end if
        if (JANYI(ipoptn,npoptn,-2) .gt. 0) then
CC          set line color for line of maximum dispersion default.
          nvl = IWHER(5,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
          call LSCLQ(3,2,idumy)
          if (mscrnm(nsc,nln) .eq. mblnk1.or.nplots .le. 0) then
            mscrnm(nsc,nln) = malpha(iswplt(23)+1)
          end if
        end if
        if (JANYI(ipoptn,npoptn,3) .ne. 0.or.JANYI(ipoptn,npoptn,4)
     *      .ne. 0) then
          if (iftran .gt. 0) then
CC              set line color for terrain contour level defaults.
            n = 0
            nvl = 5
            icontr = 1
            do 130 k=1,2
              nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
              do 120 j=1,10
                nsc = JWHER(nvl,itabsm,ispacm,j,nch)
                n = n + 1
                if (n .le. ncontr(1)) then
                  call LSCLQ(5,n,idumy)
                  if (mscrnm(nsc,nln) .eq. mblnk1.or.nplots .le. 0) then
                    mscrnm(nsc,nln) = malpha(icnclr(n,1)+1)
                  end if
                end if
  120         continue
  130       continue
          end if
        end if
        if (JANYI(ipoptn,npoptn,-3) .ne. 0.or.JANYI(ipoptn,npoptn,-4)
     *      .ne. 0) then
CC          set line color for dispersion isopleth level defaults.
          n = 0
          nvl = 7
          icontr = 2
          do 150 k=1,2
            nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
            do 140 j=1,10
              nsc = JWHER(nvl,itabsm,ispacm,j,nch)
              n = n + 1
              if (n .le. ncontr(2)) then
                call LSCLQ(5,n,idumy)
                if (mscrnm(nsc,nln) .eq. mblnk1.or.nplots .le. 0) then
                  mscrnm(nsc,nln) = malpha(icnclr(n,2)+1)
                end if
              end if
  140       continue
  150     continue
        end if
      end if
      if (IANYI(ipoptn,npoptn,5) .ne. 0.and.nspcls .gt. 0) then
CC        set color for special symbols defaults.
        n = 0
        nvl = 9
        do 170 k=1,5
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          do 160 j=1,10
            nsc = JWHER(nvl,itabsm,ispacm,j,nch)
            n = n + 1
            if (n .le. nspcls) then
              call LSCLQ(4,n,idumy)
              if (mscrnm(nsc,nln) .eq. mblnk1.or.nplots .le. 0) then
                mscrnm(nsc,nln) = malpha(ispclr(n)+1)
              end if
            end if
  160     continue
  170   continue
      end if
      nvl = IWHER(14,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
      if (isystm .eq. 2.and.(ipmode .ge. 16.or.ipmode .eq. 4.or.
     *    ISUMI(ioptn,16) .le. 4)) then
        call MBLK1(mscrnm(1,nln+2),nmxcol)
        call MBLK1(mscrnm(1,nln+3),nmxcol)
        call MBLK1(mscrnm(1,nln+4),nmxcol)
      end if
CC      display screen menu.
      iferrs = IOSCN(numsc,mscrnm,ilinem,itabsm,ispacm,iferrm,lstrw,0)
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
        go to 280
      else if (jfers .eq. 2) then
CC        main menu.
        ifnext = 3
        go to 280
      else if (jfers .ge. 3.and.jfers .le. 7) then
CC        display aux. menus.
        iferrs = IFRSP(jfers,mscrnm,ilinem,itabsm,ispacm,iferrm,0)
        if (iferrs .eq. 1.or.iferrs .eq. 2) then
CC          terminate or main menu.
          jfers = iferrs
          ifnext = 3
          go to 280
        else
CC          redisplay menu.
          go to 110
        end if
      else if (jfers .eq. 8) then
        ifnext = 20
      else if (jfers .ge. 9.and.jfers .le. 10) then
        go to 110
      else if (jfers .eq. 11) then
        go to 20
      end if
CC      turn off error indicators for this menu.
      i = JINIT(iferrm,nmxlin*nmxfld,0,0)
CC
CC      check data.
CC
CC      get plot background color.
      nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        j = IFFOP(mscrnm(nsc,nln),16)
        if (j .ge. 0) then
          iswplt(9) = j
          call LSCLQ(-1,0,idumy)
        else
          iferrm(nvl,1) = 1
        end if
      else
        iswplt(9) = 0
        mscrnm(nsc,nln) = malpha(iswplt(9)+1)
        ifwrt = 1
      end if
CC
CC      get axes and labels color.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        j = IFFOP(mscrnm(nsc,nln),ncolr)
        if (j .ge. 0) then
          iswplt(16) = j
          call LSCLQ(-2,0,idumy)
        else
          iferrm(nvl,1) = 1
        end if
      else
        iswplt(16) = 1
        mscrnm(nsc,nln) = malpha(iswplt(16)+1)
        ifwrt = 1
      end if
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (IANYI(ipoptn,npoptn,1) .ne. 0.and.nxyfnc .gt. 0) then
CC
CC        get dispersion as a function of distance line color.
        if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
          j = IFFOP(mscrnm(nsc,nln),ncolr)
          if (j .ge. 0) then
            iswplt(12) = j
            call LSCLQ(-3,1,idumy)
          else
            iferrm(nvl,1) = 1
          end if
        else
          iswplt(12) = iswplt(16)
          mscrnm(nsc,nln) = malpha(iswplt(12)+1)
          ifwrt = 1
        end if
      end if
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (JANYI(ipoptn,npoptn,2) .ne. 0) then
CC
CC        get line color for line of maximum terrain.
        if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
          j = IFFOP(mscrnm(nsc,nln),ncolr)
          if (j .ge. 0) then
            iswplt(12) = j
            call LSCLQ(-3,1,idumy)
          else
            iferrm(nvl,1) = 1
          end if
        else
          iswplt(12) = iswplt(16)
          mscrnm(nsc,nln) = malpha(iswplt(12)+1)
          ifwrt = 1
        end if
      end if
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (nxyfnc .eq. 0) then
        if (JANYI(ipoptn,npoptn,-2) .ne. 0) then
CC
CC          get line color for line of maximum dispersion.
          if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
            j = IFFOP(mscrnm(nsc,nln),ncolr)
            if (j .ge. 0) then
              iswplt(23) = j
              call LSCLQ(-3,2,idumy)
            else
              iferrm(nvl,1) = 1
            end if
          else
            iswplt(23) = iswplt(16)
            mscrnm(nsc,nln) = malpha(iswplt(23)+1)
            ifwrt = 1
          end if
        end if
      end if
      nn = 2
      if (JANYI(ipoptn,npoptn,3) .ne. 0.or.JANYI(ipoptn,npoptn,4)
     *    .ne. 0) then
CC
CC        get line color for terrain contour levels.
        icontr = 1
        nn = 0
        n = 0
        do 200 k=1,2
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          do 190 j=1,10
            n = n + 1
            if (n .le. ncontr(1)) then
              nsc = JWHER(nvl,itabsm,ispacm,j,nch)
              if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
                l = IFFOP(mscrnm(nsc,nln),ncolr)
                if (l .ge. 0) then
                  icnclr((k-1)*10+j,1) = l
                  call LSCLQ(-5,(k-1)*10+j,idumy)
                else
                  iferrm(nvl,j) = 1
                end if
              else
                icnclr((k-1)*10+j,1) = iswplt(16)
                mscrnm(nsc,nln) = malpha(icnclr((k-1)*10+j,1)+1)
                ifwrt = 1
              end if
            end if
  190     continue
  200   continue
      end if
      nvl = nvl + nn
      nn = 2
      if (JANYI(ipoptn,npoptn,-3) .ne. 0.or.JANYI(ipoptn,npoptn,-4)
     *    .ne. 0) then
CC
CC        get line color for dispersion isopleth levels.
        icontr = 2
        nn = 0
        n = 0
        do 220 k=1,2
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          do 210 j=1,10
            n = n + 1
            if (n .le. ncontr(2)) then
              nsc = JWHER(nvl,itabsm,ispacm,j,nch)
              if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
                l = IFFOP(mscrnm(nsc,nln),ncolr)
                if (l .ge. 0) then
                  icnclr((k-1)*10+j,2) = l
                  call LSCLQ(-5,(k-1)*10+j,idumy)
                else
                  iferrm(nvl,j) = 1
                end if
              else
                icnclr((k-1)*10+j,2) = iswplt(16)
                mscrnm(nsc,nln) = malpha(icnclr((k-1)*10+j,2)+1)
                ifwrt = 1
              end if
            end if
  210     continue
  220   continue
      end if
      nvl = nvl + nn
      if (IANYI(ipoptn,npoptn,5) .ne. 0.and.nspcls .gt. 0) then
CC
CC        get color for special symbols.
        n = 0
        do 240 k=1,5
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          do 230 j=1,10
            n = n + 1
            if (n .le. nspcls) then
              nsc = JWHER(nvl,itabsm,ispacm,j,nch)
              if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
                l = IFFOP(mscrnm(nsc,nln),ncolr)
                if (l .ge. 0) then
                  ispclr((k-1)*10+j) = l
                  call LSCLQ(-4,(k-1)*10+j,idumy)
                else
                  iferrm(nvl,j) = 1
                end if
              else
                ispclr((k-1)*10+j) = iswplt(16)
                mscrnm(nsc,nln) = malpha(ispclr((k-1)*10+j)+1)
                ifwrt = 1
              end if
            end if
  230     continue
  240   continue
      end if
CC
CC
      ner = IFWER(iferrm,nmxlin*nmxfld)
      if (ifwrt .gt. 0.or.ner .gt. 0.or.(ner .eq. 0.and.ker(3) .ne. 0))
     *    then
CC        rewrite screen data.
        i = IWTSC(numsc,1,mscrnm,ilinem,iferrm)
      end if
      if (ner .gt. 0) then
        if (KERRP(22,0) .ge. 1) go to 280
      end if
      ker(3) = ner
      if (ker(3) .eq. 1) then
        ifnext = ifnxt
        go to 110
      end if
      if (jfers .eq. 8) go to 280
  250 ker(3) = 0
  280 kfnxt = 0
      return
      end
