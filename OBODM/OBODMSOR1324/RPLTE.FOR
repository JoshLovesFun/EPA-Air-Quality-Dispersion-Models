      Subroutine RPLTE
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - RPLTE Version 01.2                          ---
CC--- Subroutine description - plot details.                        ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
CC
      dimension ker(6)
CC
      include 'OCNTRL.INC'
      include 'OCDSPM.INC'
      include 'OWNTRL.INC'
      include 'OCALCS.INC'
      include 'ONAMES.INC'
CC
      equivalence (ker(1),delthp),(iflg,sigyls),(jfers,pekcna),
     *            (kfnxt,wspmin)
CC
CC**********************************************************************
CC
      kfnxt = 0
      if (jfers .eq. 8) go to 100
      if (ibackd+ibackt .le. 0.and.ifored+iforet .ge. 14) then
        ifclr = 0
      else
        ifclr = 1
      end if
CC
CC        screen numsc (69), plot details grid coord. positions,
CC        isopleth legends.
   10 numsc = numscs(8) + 12
      numf = IRDSC(numsc,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
CC      indicate dispersion quantity in menu
      nvl = IWHER(2,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
      call MVCH1(mscrch(ilbstr(4)),mscrnm(9,nln),MIN0(14,nlbsch(4)))
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      call MVCH1(mscrch(ilbstr(4)),mscrnm(25,nln),MIN0(14,nlbsch(4)))
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      call MVCH1(mscrch(ilbstr(4)),mscrnm(38,nln),MIN0(14,nlbsch(4)))
CC      plot internal grid point positions default.
      nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
      if (iswplt(8) .ge. 0.and.(mscrnm(nsc,nln) .eq. mblnk1.or.
     *    nplots .le. 0)) then
        j = iswplt(8)
        if (j .gt. 0) j = 1
        mscrnm(nsc,nln) = malpha(j+1)
      end if
      if (JANYI(ipoptn,npoptn,-3) .gt. 0.or.JANYI(ipoptn,npoptn,-4)
     *    .gt. 0) then
CC        set how to label isopleth lines defaults.
        nvl = IWHER(2,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
        j = IABS(iswplt(24))
        if (j .ge. 0.and.(mscrnm(nsc,nln) .eq. mblnk1.or.
     *      nplots .le. 0)) then
          mscrnm(nsc,nln) = malpha(j+1)
        end if
        if (j .eq. 1.or.j .eq. 2) then
CC          set isopleth legend of indices default.
          nvl = IWHER(3,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
          if (mscrnm(nsc,nln) .eq. mblnk1.or.nplots .le. 0) then
            if (iswplt(24) .lt. 0) then
              l = 0
            else
              l = 1
            end if
            mscrnm(nsc,nln) = malpha(l+1)
          end if
        end if
        if (iswplt(24) .eq. -1.or.iswplt(24) .eq. -2) then
CC          set where to place isopleth legend of indices default.
          nvl = IWHER(4,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
          if (iswplt(25) .ge. 0.and.(mscrnm(nsc,nln) .eq. mblnk1.or.
     *        nplots .le. 0)) then
            mscrnm(nsc,nln) = malpha(iswplt(25)+1)
          end if
        end if
      end if
CC      display screen menu.
      iferrs = IOSCN(numsc,mscrnm,ilinem,itabsm,ispacm,iferrm,lstrw,0)
CC      save screen menu.
      if (iferrs .ne. 10) i = IWTSC(numsc,1,mscrnm,ilinem,iferrm)
CC      save iferrs.
      jfers = iferrs
      ifwrt = 0
      ker(4) = IFWER(iferrm,nmxlin*nmxfld)
CC
CC      check user response to menu.
      if (jfers .eq. 1) then
CC        terminate.
        go to 140
      else if (jfers .eq. 2) then
CC        main menu.
        ifnext = 3
        go to 140
      else if (jfers .ge. 3.and.jfers .le. 7) then
CC        display aux. menus.
        iferrs = IFRSP(jfers,mscrnm,ilinem,itabsm,ispacm,iferrm,0)
        if (iferrs .eq. 1.or.iferrs .eq. 2) then
CC          terminate or main menu.
          jfers = iferrs
          ifnext = 3
          go to 140
        else
CC          redisplay menu.
          go to 10
        end if
      else if (jfers .ge. 9.and.jfers .le. 10) then
        go to 10
      else if (jfers .eq. 11) then
        if (ifclr .eq. 0) go to 110
        go to 120
      end if
CC      turn off error indicators for this menu.
      i = JINIT(iferrm,nmxlin*nmxfld,0,0)
CC
CC      check data.
CC
CC      plot internal grid point positions.
      nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        j = IFFOP(mscrnm(nsc,nln),2)
        if (j .ge. 0) then
          iswplt(8) = j
CC          use 4 to plot plus symbol.
          if (j .eq. 1) j = 4
        else
          iferrm(nvl,1) = 1
        end if
      else
        iswplt(8) = 0
        mscrnm(nsc,nln) = malpha(iswplt(8)+1)
        ifwrt = 1
      end if
      if (nxyfnc .eq. 0) then
CC
CC        get how to label isopleth lines.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
          j = IFFOP(mscrnm(nsc,nln),4)
          if (j .ge. 0) then
            iswplt(24) = j
          else
            iferrm(nvl,1) = 1
          end if
        else
          iswplt(24) = 0
          mscrnm(nsc,nln) = malpha(iswplt(24)+1)
          ifwrt = 1
        end if
        if (IABS(iswplt(24)) .eq. 1.or.IABS(iswplt(24)) .eq. 2) then
CC
CC          see if isopleth legend of indices is desired.
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
            j = IFFOP(mscrnm(nsc,nln),2)
            if (j .ge. 0) then
              if (j .eq. 0) then
CC                plot legend of isopleth indices.
                iswplt(24) = -IABS(iswplt(24))
              else
CC                no isopleth legend of indices plotted.
                iswplt(24) = IABS(iswplt(24))
              end if
            else
              iferrm(nvl,1) = 1
            end if
          else
            iswplt(24) = -IABS(iswplt(24))
            mscrnm(nsc,nln) = malpha(IABS(iswplt(24))+1)
            ifwrt = 1
          end if
        end if
        if (iswplt(24) .eq. -1.or.iswplt(24) .eq. -2) then
CC
CC          see where to place isopleth legend of indices.
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
            j = IFFOP(mscrnm(nsc,nln),8)
            if (j .ge. 0) then
              iswplt(25) = j
            else
              iferrm(nvl,1) = 1
            end if
          else
            iswplt(25) = 0
            mscrnm(nsc,nln) = malpha(iswplt(25)+1)
            ifwrt = 1
          end if
        end if
      end if
CC
CC
      ner = IFWER(iferrm,nmxlin*nmxfld)
      if (ifwrt .gt. 0.or.ner .gt. 0.or.(ner .eq. 0.and.
     *    ker(4) .ne. 0)) then
CC        rewrite screen data.
        i = IWTSC(numsc,1,mscrnm,ilinem,iferrm)
      end if
      if (ner .gt. 0) then
        if (KERRP(22,0) .ge. 1) go to 140
      end if
      ker(4) = ner
      if (ker(4) .eq. 1) go to 10
      if (jfers .eq. 8) go to 100
   20 if ((JANYI(ipoptn,npoptn,3) .gt. 0.or.JANYI(ipoptn,npoptn,4) .gt.
     *    0).and.nxyfnc .eq. 0.and.iftran .gt. 0) then
CC
CC
CC        screen numsc (70), plot details, contour legends.
        numsc = numscs(8) + 13
        numf = IRDSC(numsc,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
CC        set how to label contour lines default
        nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
        j = IABS(iswplt(11))
        if (j .ge. 0.and.(mscrnm(nsc,nln) .eq. mblnk1.or.
     *      nplots .le. 0)) then
          mscrnm(nsc,nln) = malpha(j+1)
        end if
        if (j .eq. 1.or.j .eq. 2) then
CC          set if contour legend of indices is desired default.
          nvl = IWHER(2,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
          if (mscrnm(nsc,nln) .eq. mblnk1.or.nplots .le. 0) then
            if (iswplt(11) .lt. 0) then
              l = 0
            else
              l = 1
            end if
            mscrnm(nsc,nln) = malpha(l+1)
          end if
        end if
        if (iswplt(11) .eq. -1.or.iswplt(11) .eq. -2) then
CC          set where to place isopleth legend of indices.
          nvl = IWHER(3,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
          if (iswplt(20) .ge. 0.and.(mscrnm(nsc,nln) .eq. mblnk1.or.
     *        nplots .le. 0)) then
            mscrnm(nsc,nln) = malpha(iswplt(20)+1)
          end if
        end if
CC        display screen menu.
        iferrs = IOSCN(numsc,mscrnm,ilinem,itabsm,ispacm,iferrm,lstrw,0)
CC        save screen menu.
        if (iferrs .ne. 10) i = IWTSC(numsc,1,mscrnm,ilinem,iferrm)
CC        save iferrs.
        jfers = iferrs
        ifwrt = 0
        ker(5) = IFWER(iferrm,nmxlin*nmxfld)
CC
CC        check user response to menu.
        if (jfers .eq. 1) then
CC          terminate.
          go to 140
        else if (jfers .eq. 2) then
CC          main menu.
          ifnext = 3
          go to 140
        else if (jfers .ge. 3.and.jfers .le. 7) then
CC          display aux. menus.
          iferrs = IFRSP(jfers,mscrnm,ilinem,itabsm,ispacm,iferrm,0)
          if (iferrs .eq. 1.or.iferrs .eq. 2) then
CC            terminate or main menu.
            jfers = iferrs
            ifnext = 3
            go to 140
          else
CC            redisplay menu.
            go to 20
          end if
        else if (jfers .ge. 9.and.jfers .le. 10) then
          go to 20
        else if (jfers .eq. 11) then
          go to 10
        end if
CC        turn off error indicators for this menu.
        i = JINIT(iferrm,nmxlin*nmxfld,0,0)
CC
CC        check data.
CC
CC        get how to label contour lines.
        nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
        if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
          j = IFFOP(mscrnm(nsc,nln),4)
          if (j .ge. 0) then
            iswplt(11) = j
          else
            iferrm(nvl,1) = 1
          end if
        end if
        if (IABS(iswplt(11)) .eq. 1.or.IABS(iswplt(11)) .eq. 2) then
CC
CC          see if contour legend of indices is desired.
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
            j = IFFOP(mscrnm(nsc,nln),2)
            if (j .ge. 0) then
              if (j .eq. 0) then
CC                plot legend of contour indices.
                iswplt(11) = -IABS(iswplt(11))
              else
CC                no contour legend of indices plotted.
                iswplt(11) = IABS(iswplt(11))
              end if
            else
              iferrm(nvl,1) = 1
            end if
          else
            iswplt(11) = -IABS(iswplt(11))
            mscrnm(nsc,nln) = malpha(IABS(iswplt(11))+1)
            ifwrt = 1
          end if
        end if
        if (iswplt(11) .eq. -1.or.iswplt(11) .eq. -2) then
CC
CC          see where to place isopleth legend of indices.
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
            j = IFFOP(mscrnm(nsc,nln),8)
            if (j .ge. 0) then
              iswplt(20) = j
            else
              iferrm(nvl,1) = 1
            end if
          else
            iswplt(20) = 0
            mscrnm(nsc,nln) = malpha(iswplt(20)+1)
            ifwrt = 1
          end if
        end if
CC
CC
        ner = IFWER(iferrm,nmxlin*nmxfld)
        if (ifwrt .gt. 0.or.ner .gt. 0.or.(ner .eq. 0.and.
     *      ker(5) .ne. 0)) then
CC          rewrite screen data.
          i = IWTSC(numsc,1,mscrnm,ilinem,iferrm)
        end if
        if (ner .gt. 0) then
          if (KERRP(22,0) .ge. 1) go to 140
        end if
        ker(5) = ner
        if (ker(5) .eq. 1) go to 20
        if (jfers .eq. 8) go to 100
      end if
CC
CC
CC        screen numsc (71), plot details.
      jflg = 0
   30 numsc = numscs(8) + 14
      numf = IRDSC(numsc,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
CC      provide defaults.
      if (sclxes .le. 0.0.or.iflg .ne. 0) then
        iflg = 1
        sclxes = 0.0
        sclyes = 0.0
        iret = 1
        go to 90
      end if
CC      default x,y inches in plot, xnchpl, ynchpl.
   40 nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0.or.nplots .le. 0) then
        i = IWTBF(mscrnm(nsc,nln),nch,xnchpl,2,iflrjs)
      end if
      nsc = JWHER(nvl,itabsm,ispacm,2,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0.or.nplots .le. 0) then
        i = IWTBF(mscrnm(nsc,nln),nch,ynchpl,2,iflrjs)
      end if
CC      plot margins, xlmrgn, xrmrgn, ybmrgn, ytmrgn.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0.or.nplots .le. 0) then
        i = IWTBF(mscrnm(nsc,nln),nch,xlmrgn,2,iflrjs)
      end if
      nsc = JWHER(nvl,itabsm,ispacm,2,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0.or.nplots .le. 0) then
        i = IWTBF(mscrnm(nsc,nln),nch,xrmrgn,2,iflrjs)
      end if
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0.or.nplots .le. 0) then
        i = IWTBF(mscrnm(nsc,nln),nch,ybmrgn,2,iflrjs)
      end if
      nsc = JWHER(nvl,itabsm,ispacm,2,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0.or.nplots .le. 0) then
        i = IWTBF(mscrnm(nsc,nln),nch,ytmrgn,2,iflrjs)
      end if
CC      min,max x,y axes, pxmins, pymins, pxmaxs, pymaxs
      nvls = nvl
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0.or.nplots .le. 0) then
        i = IWTBF(mscrnm(nsc,nln),nch,pxmins,2,iflrjs)
      end if
      nsc = JWHER(nvl,itabsm,ispacm,2,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0.or.nplots .le. 0) then
        i = IWTBF(mscrnm(nsc,nln),nch,pymins,2,iflrjs)
      end if
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0.or.nplots .le. 0) then
        i = IWTBF(mscrnm(nsc,nln),nch,pxmaxs,2,iflrjs)
      end if
      nsc = JWHER(nvl,itabsm,ispacm,2,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0.or.nplots .le. 0) then
        i = IWTBF(mscrnm(nsc,nln),nch,pymaxs,2,iflrjs)
      end if
CC      scale of x,y axes, sclxes, sclyes.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0.or.iflg .ne. 0.or.
     *    nplots .le. 0) then
        i = IWTBF(mscrnm(nsc,nln),nch,sclxes,6,iflrjs)
      end if
      nsc = JWHER(nvl,itabsm,ispacm,2,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0.or.iflg .ne. 0.or.
     *    nplots .le. 0) then
        i = IWTBF(mscrnm(nsc,nln),nch,sclyes,6,iflrjs)
      end if
CC      height of label characters.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0.or.nplots .le. 0) then
        i = IWTBF(mscrnm(nsc,nln),nch,htlchr,4,iflrjs)
      end if
CC      height of axis coordinate numbers.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0.or.nplots .le. 0) then
        i = IWTBF(mscrnm(nsc,nln),nch,htnchr,4,iflrjs)
      end if
CC      height of special symbols or characters.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0.or.nplots .le. 0) then
        i = IWTBF(mscrnm(nsc,nln),nch,htpchr,4,iflrjs)
      end if
CC      increment between tick marks, xincax, yincax.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0.or.nplots .le. 0) then
        xdumy = rmissc
        xincax = 0.0
        i = IWTBF(mscrnm(nsc,nln),nch,xdumy,2,iflrjs)
      end if
      nsc = JWHER(nvl,itabsm,ispacm,2,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0.or.nplots .le. 0) then
        yincax = 0.0
        xdumy = rmissc
        i = IWTBF(mscrnm(nsc,nln),nch,xdumy,2,iflrjs)
      end if
CC      number of digits to right of decimal for axis coordinate number.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0.or.nplots .le. 0) then
        xdumy = rmissc
        ndgxes = -1
        i = IWTBF(mscrnm(nsc,nln),nch,xdumy,1,iflrjs)
      end if
      nsc = JWHER(nvl,itabsm,ispacm,2,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .le. 0.or.nplots .le. 0) then
        xdumy = rmissc
        ndgyes = -1
        i = IWTBF(mscrnm(nsc,nln),nch,xdumy,1,iflrjs)
      end if
CC      display screen menu.
      iferrs = IOSCN(numsc,mscrnm,ilinem,itabsm,ispacm,iferrm,lstrw,0)
CC      save screen menu.
      if (iferrs .ne. 10) i = IWTSC(numsc,1,mscrnm,ilinem,iferrm)
CC      save iferrs.
      jfers = iferrs
      ifwrt = 0
      ker(6) = IFWER(iferrm,nmxlin*nmxfld)
CC
CC      check user response to menu.
      if (jfers .eq. 1) then
CC        terminate.
        go to 140
      else if (jfers .eq. 2) then
CC        main menu.
        ifnext = 3
        go to 140
      else if (jfers .ge. 3.and.jfers .le. 7) then
CC        display aux. menus.
        iferrs = IFRSP(jfers,mscrnm,ilinem,itabsm,ispacm,iferrm,0)
        if (iferrs .eq. 1.or.iferrs .eq. 2) then
CC          terminate or main menu.
          jfers = iferrs
          ifnext = 3
          go to 140
        else
CC          redisplay menu.
          go to 30
        end if
      else if (jfers .ge. 9.and.jfers .le. 10) then
        go to 30
      else if (jfers .eq. 11) then
        if ((JANYI(ipoptn,npoptn,3) .gt. 0.or.JANYI(ipoptn,npoptn,4)
     *      .gt. 0).and.nxyfnc .eq. 0.and.iftran .gt. 0) go to 20
        go to 10
      end if
CC      turn off error indicators for this menu.
      i = JINIT(iferrm,nmxlin*nmxfld,0,0)
CC
CC      check data.
CC
CC      x,y inches in plot, xnchpl, ynchpl
      nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
        if (i .eq. 0) then
          xnchpl = xdumy
        else
          iferrm(nvl,1) = 1
        end if
      end if
      nsc = JWHER(nvl,itabsm,ispacm,2,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
        if (i .eq. 0) then
          ynchpl = xdumy
        else
          iferrm(nvl,2) = 1
        end if
      end if
CC
CC      plot margins, xlmrgn, xrmrgn, ybmrgn, ytmrgn.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
        if (i .eq. 0) then
          xlmrgn = xdumy
        else
          iferrm(nvl,1) = 1
        end if
      end if
      nsc = JWHER(nvl,itabsm,ispacm,2,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
        if (i .eq. 0) then
          xrmrgn = xdumy
        else
          iferrm(nvl,2) = 1
        end if
      end if
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
        if (i .eq. 0) then
          ybmrgn = xdumy
        else
          iferrm(nvl,1) = 1
        end if
      end if
      nsc = JWHER(nvl,itabsm,ispacm,2,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
        if (i .eq. 0) then
          ytmrgn = xdumy
        else
          iferrm(nvl,2) = 1
        end if
      end if
CC
CC      min,max x,y axes, pxmins, pymins, pxmaxs, pymaxs
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
      if (i .le. 0) then
        if (i .eq. 0) then
          pxmins = xdumy
        else
          pxmins = -rmissd
        end if
      else
        iferrm(nvl,1) = 1
      end if
      nsc = JWHER(nvl,itabsm,ispacm,2,nch)
      i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
      if (i .le. 0) then
        if (i .eq. 0) then
          pymins = xdumy
        else
          pymins = -rmissd
        end if
      else
        iferrm(nvl,2) = 1
      end if
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
      if (i .le. 0) then
        if (i .eq. 0) then
          pxmaxs = xdumy
        else
          pxmaxs = rmissd
        end if
      else
        iferrm(nvl,1) = 1
      end if
      nsc = JWHER(nvl,itabsm,ispacm,2,nch)
      i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
      if (i .le. 0) then
        if (i .eq. 0) then
          pymaxs = xdumy
        else
          pymaxs = rmissd
        end if
      else
        iferrm(nvl,2) = 1
      end if
CC
CC      scale of x,y axes, sclxes, sclyes.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
      if (i .le. 0) then
        if (i .eq. 0.and.xdumy .gt. 0.0) then
          sclxes = xdumy
        else
          sclxes = 0.0
        end if
      else
        iferrm(nvl,1) = 1
      end if
      nsc = JWHER(nvl,itabsm,ispacm,2,nch)
      i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
      if (i .le. 0) then
        if (i .eq. 0.and.xdumy .gt. 0.0) then
          sclyes = xdumy
        else
          sclyes = 0.0
        end if
      else
        iferrm(nvl,2) = 1
      end if
CC
      nvln = nvl
   50 if (sclxes .le. 0.0.or.sclyes .le. 0.0.or.pxmaxs .le. rmissd.or.
     *    pymaxs .le. rmissd.or.pxmins .ge. -rmissd.or.pymins .ge.
     *    -rmissd) then
        iret = 2
        go to 90
   60   nvl = nvls
CC        min,max x,y axes, pxmins, pymins, pxmaxs, pymaxs
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        i = IWTBF(mscrnm(nsc,nln),nch,pxmins,2,iflrjs)
        nsc = JWHER(nvl,itabsm,ispacm,2,nch)
        i = IWTBF(mscrnm(nsc,nln),nch,pymins,2,iflrjs)
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        i = IWTBF(mscrnm(nsc,nln),nch,pxmaxs,2,iflrjs)
        nsc = JWHER(nvl,itabsm,ispacm,2,nch)
        i = IWTBF(mscrnm(nsc,nln),nch,pymaxs,2,iflrjs)
CC        scale of x,y axes, sclxes, sclyes.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        i = IWTBF(mscrnm(nsc,nln),nch,sclxes,6,iflrjs)
        nsc = JWHER(nvl,itabsm,ispacm,2,nch)
        i = IWTBF(mscrnm(nsc,nln),nch,sclyes,6,iflrjs)
        ifwrt = 1
        nvl = nvln
        if (sclxes .gt. 0.0.and.sclyes .gt. 0.0) then
          sclxys = SQRT(sclxes * sclxes + sclyes * sclyes)
        end if
        if (jflg .eq. 1) go to 70
      end if
CC
CC      height of label characters.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
        if (i .le. 0) then
          if (i .eq. 0) then
            htlchr = xdumy
          else
            htlchr = 2.7 * spmins + spmins
            i = IWTBF(mscrnm(nsc,nln),nch,htlchr,4,iflrjs)
            ifwrt = 1
          end if
        else
          iferrm(nvl,1) = 1
        end if
      end if
CC
CC      height of axis coordinate numbers.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
        if (i .le. 0) then
          if (i .eq. 0) then
            htnchr = xdumy
          else
            htnchr = 2.7 * spmins
            i = IWTBF(mscrnm(nsc,nln),nch,htnchr,4,iflrjs)
            ifwrt = 1
          end if
        else
          iferrm(nvl,1) = 1
        end if
      end if
CC
CC      height of special symbols or characters.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
        if (i .le. 0) then
          if (i .eq. 0) then
            htpchr = xdumy
          else
            htpchr = htnchr
            i = IWTBF(mscrnm(nsc,nln),nch,htpchr,4,iflrjs)
            ifwrt = 1
          end if
        else
          iferrm(nvl,1) = 1
        end if
      end if
      htlchr = AMAX1(htlchr,spmins)
      htnchr = AMAX1(htnchr,spmins)
      htpchr = AMAX1(htpchr,spmins)
CC
CC      increment between tick marks, xincax, yincax.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (iswplt(4) .ne. 2) then
        if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
          i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
          if (i .le. 0) then
            if (i .eq. 0) then
              xincax = xdumy
            else
              xincax = 0.0
              i = IWTBF(mscrnm(nsc,nln),nch,rmissc,4,iflrjs)
              ifwrt = 1
            end if
          else
            iferrm(nvl,1) = 1
          end if
        end if
        nsc = JWHER(nvl,itabsm,ispacm,2,nch)
        if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
          i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
          if (i .le. 0) then
            if (i .eq. 0) then
              yincax = xdumy
            else
              yincax = 0.0
              i = IWTBF(mscrnm(nsc,nln),nch,rmissc,4,iflrjs)
              ifwrt = 1
            end if
          else
            iferrm(nvl,1) = 1
          end if
        end if
      end if
CC
CC      number of digits to right of decimal for axis coordinate number
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (iswplt(4) .ne. 2) then
        if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
          i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
          if (i .le. 0) then
            if (i .eq. 0) then
              ndgxes = xdumy
            else
              ndgxes = -1
              i = IWTBF(mscrnm(nsc,nln),nch,rmissc,4,iflrjs)
              ifwrt = 1
            end if
          else
            iferrm(nvl,1) = 1
          end if
        end if
        nsc = JWHER(nvl,itabsm,ispacm,2,nch)
        if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
          i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
          if (i .le. 0) then
            if (i .eq. 0) then
              ndgyes = xdumy
            else
              ndgyes = -1
              i = IWTBF(mscrnm(nsc,nln),nch,rmissc,4,iflrjs)
              ifwrt = 1
            end if
          else
            iferrm(nvl,1) = 1
          end if
        end if
      end if
CC
CC
   70 ner = IFWER(iferrm,nmxlin*nmxfld)
      if (ifwrt .gt. 0.or.ner .gt. 0.or.(ner .eq. 0.and.
     *    ker(6) .ne. 0)) then
CC        rewrite screen data.
        i = IWTSC(numsc,1,mscrnm,ilinem,iferrm)
      end if
      if (ner .gt. 0) then
        if (KERRP(22,0) .ge. 1) go to 140
      end if
      ker(6) = ner
      if (ker(6) .eq. 1) go to 30
      if (jflg .eq. 0) then
CC        check data against scales.
        sxsav = sclxes
        sysav = sclyes
        sclxes = 0.0
        sclyes = 0.0
        iret = 3
        go to 90
   80   d1 = sxsav / sclxes
        d2 = sysav / sclyes
CC        check if within 1% of each other.
        if (d1 .gt. 1.01.or.d1 .lt. 0.99.or.d2 .gt. 1.01 .or.
     *      d2 .lt. 0.99) then
          i = KERRS(51,mnpbuf,idum,idum,xdum,xdum,xdum,xdum,0)
          if (i .eq. 1) then
            iferrs = 1
            go to 140
          else if (i .eq. 2) then
            ifnext = 3
            iferrs = 2
            go to 140
          else if (i .eq. 3) then
CC            go rescale axes.
            sclxes = 0.0
            sclyes = 0.0
            jflg = 1
            ifwrt = 1
            go to 50
          end if
CC          do not rescale, but go back and redisplay the menu.
          sclxes = sxsav
          sclyes = sysav
          iflg = 1
          go to 30
        end if
      end if
      go to 100
CC      rescale axes.
   90 if (IANYI(ipoptn,npoptn,1) .ne. 0) then
        call DMXMQ(1,functx,functy,nxyfnc,nxyfnc,0)
      else
        if (JANYI(ipoptn,npoptn,-2) .gt. 0.or.
     *      JANYI(ipoptn,npoptn,-3) .gt. 0.or.
     *      JANYI(ipoptn,npoptn,-4) .gt. 0) then
          call DMXMQ(1,xcoord,ycoord,nxpnts,nypnts,ifgpol)
        else if (JANYI(ipoptn,npoptn,2) .gt. 0.or.
     *           JANYI(ipoptn,npoptn,3) .gt. 0.or.
     *           JANYI(ipoptn,npoptn,4) .gt. 0) then
          call DMXMQ(1,xteran,yteran,nxtran,nytran,iftpol)
        else
          call DMXMQ(1,xcoord,ycoord,nxpnts,nypnts,ifgpol)
        end if
      end if
      go to (40,60,80),iret
CC
CC      check if data complete.
  100 if (IFWER(ker,6) .eq. 1) then
        ifset = -1
      else
        ifset = 1
      end if
      ifrdwt(6) = ifset
      if (ifset .le. 0) then
        ifnext = 3
        iferrs = 2
      end if
      go to 140
  110 kfnxt = 1
      go to 130
  120 kfnxt = 2
  130 ifnext = 18
      iferrs = 0
  140 return
      end
