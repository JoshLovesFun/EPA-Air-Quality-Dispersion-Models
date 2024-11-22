      Subroutine RINPK
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - RINPK Version 01.3                          --- 072497
CC--- Subroutine description - displays and reads source data.      ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
CC
      dimension jflgs(50),ker(11)
CC
      include 'OCNTRL.INC'
      include 'OCDSPM.INC'
      include 'OCALCS.INC'
      include 'ONAMES.INC'
      include 'OWNTRL.INC'
CC
      equivalence (jfnxt,functx(1)),(jflgs(1),functx(2)),
     *            (ker(1),functx(52)),(numsc,functx(63)),
     *            (isorc,functx(70))
CC
CC********************************************************************
CC
      if (jfnxt .eq. 11) go to 40
      if (jfnxt .eq. 12) go to 70
      if (ifhsly .ne. 0) then
CC
CC        screen numsc+11 (21), hourly source data.
   10   nn = 11
        numf = IRDSC(numsc+nn,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
CC
CC        display screen menu.
        iferrs = IOSCN(numsc+nn,mscrnm,ilinem,itabsm,ispacm,iferrm,
     *                 lstrw,12)
CC        save screen menu.
        if (iferrs .ne. 10) i = IWTSC(numsc+nn,1,mscrnm,ilinem,iferrm)
CC        if terminate.
        if (iferrs .eq. 1) go to 70
        ker(10) = IFWER(iferrm,nmxlin*nmxfld)
        jfers = iferrs
        ifwrt = 0
CC
CC        check user response.
        if (jfers .eq. 2) then
CC          main menu.
          go to 40
        else if (jfers .ge. 3.and.jfers .le. 7) then
CC          display aux. menus.
          iferrs = IFRSP(jfers,mscrnm,ilinem,itabsm,ispacm,iferrm,0)
          if (iferrs .eq. 1.or.iferrs .eq. 2) then
CC            terminate or main menu.
            jfers = iferrs
            go to 40
          else
CC            redisplay menu.
            go to 10
          end if
        else if (jfers .ge. 8.and.jfers .le. 10) then
          go to 10
        else if (jfers .eq. 11) then
          go to 80
        end if
CC        turn of error array.
        i = JINIT(iferrm,nmxlin*nmxfld,0,0)
CC
CC        check data.
CC
CC        get data record format.
        nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
        if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
          ihsfmt = 1
          call MBLK1(mforms,151)
          call MVCH1(mscrnm(nsc,nln),mforms,nch)
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nchs)
          nche = nchs
          if (nch+nchs .gt. 150) nchs = 150 - nch
          call MVCH1(mscrnm(nsc,nln),mforms(nch+1),nchs)
          ier = IFORM(mforms,nch+nchs,nch,nvl)
          if (ier .gt. 0) then
            ifwrt = 1
            call MBLK1(mscrnm(nsc,nln-1),nch)
            call MBLK1(mscrnm(nsc,nln),nche)
            call MVCH1(mforms,mscrnm(nsc,nln-1),nch)
            i = LRJST(mscrnm(nsc,nln-1),nch,iflrjs)
            i = NUMC1(mforms,nch+nchs)
            if (i .gt. nch) then
              call MVCH1(mforms(nch+1),mscrnm(nsc,nln),nchs-nch)
              i = LRJST(mscrnm(nsc,nln),nch,1)
            end if
          end if
        else
          ihsfmt = 0
        end if
        ner = IFWER(iferrm,nmxlin*nmxfld)
        if (ifwrt .gt. 0.or.ner .gt. 0.or.(ner .eq. 0.and.
     *      ker(10) .gt. 0)) then
CC          rewrite screen data.
          i = IWTSC(numsc+nn,1,mscrnm,ilinem,iferrm)
        end if
        if (ner .gt. 0) then
          if (KERRP(22,0) .ge. 1) go to 70
        end if
        ker(10) = ner
        if (ker(10) .eq. 1) then
          if (jfers .eq. 0) go to 10
        end if
      end if
CC
CC      check to see if source data are complete.
   40 if (ifgpwn .eq. 0) then
        ker(2) = 0
        ker(3) = 0
      end if
      if (ifhrly .ne. 0) then
        k = 10
      else
        k = 7
      end if
      if (IFWER(ker,k) .eq. 1) then
        ifset = -1
      else if (nsourc .le. 0.or.qfract .le. 0.0) then
        ifset = 0
      else
        ifset = 1
        if (ifhsly .gt. 0) then
          if (ivarso(1) .le. 0.or.ISUMI(ivarso(2),3) .le. 0) ifset = 0  060607
        end if
        if (ifgpwn .gt. 0.and.iswopt(6) .eq. 2) ifset = -1
        if (ifset .gt. 0) then
          i = 0
   50     i = i + 1
          itype = IVGET(isotyp(i),2)
          jtype = IVGET(isotyp(i),1)
          if (ifhsly .le. 0.or.ivarso(2) .le. 0) then
            if (qemiss(i) .le. 0.0) ifset = 0
          end if
          if (relhgt(i) .gt. rmissc.and.relhgt(i) .lt. 0.0)
     *        relhgt(i) = 0.0
          if (relhte(i) .gt. rmissc.and.relhte(i) .lt. 0.0)             072497
     *        relhte(i) = relhgt(i)                                     072497
          if (clddxs(i) .le. rmissd) then
            if (nxpnts .gt. 0) then
              clddxs(i) = xcoord(1)
            else if (nxypnt .gt. 0) then
              clddxs(i) = xdscrt(1)
            else
              clddxs(i) = 0.0
            end if
          end if
          if (clddxe(i) .le. rmissd) clddxe(i) = clddxs(i)
          if (clddys(i) .le. rmissd) then
            if (nypnts .gt. 0) then
              clddys(i) = ycoord(1)
            else if (nxypnt .gt. 0) then
              clddys(i) = ydscrt(1)
            else
              clddys(i) = 0.0
            end if
          end if
          if (clddye(i) .le. rmissd) clddye(i) = clddys(i)
          if (clddzs(i) .le. rmissd) clddzs(i) = 0.0
          if (clddze(i) .le. rmissd) clddze(i) = clddzs(i)
          if (itype .eq. 1) then
CC            Instantaneous                                             072497
            if (jtype .eq. 1) then
CC              Instantaneous Volume/Point                              072497
              if (clddpt(i) .gt. rmissc.and.clddpt(i) .le. 0.0)         072497
     *            ifset = 0                                             072497
            else
CC              Instantaneous Line                                      072497
              if (cldsht(i) .gt. rmissc.and.cldsht(i) .le. 0.0)         072497
     *            ifset = 0                                             072497
              if (clddpt(i) .gt. rmissc.and.clddpt(i) .le. 0.0)         072497
     *            ifset = 0                                             072497
            end if
          else
CC            Quasi-Continuous                                          072497
            if (jtype .eq. 1) then
CC              Quasi-Continuous Volume/Point                           072497
              if (cldlng(i) .le. 0.0) ifset = 0
              if (cldsht(i) .le. 0.0) ifset = 0
              if (cldang(i) .lt. 0.0.or.cldang(i) .gt. 360.0) ifset = 0
              if (clddpt(i) .le. 0.0) ifset = 0
            else
CC              Quasi-Continuous Line                                   072497
              if (cldsht(i) .le. 0.0) ifset = 0
              if (clddpt(i) .le. 0.0) ifset = 0
            end if
          end if
          if (xsmlry(i) .le. 0.0) xsmlry(i) = 50.0
          if (xsmlrz(i) .le. 0.0) xsmlrz(i) = 50.0
          if (xlrgry(i) .lt. 0.0) xlrgry(i) = 0.0
          if (xlrgrz(i) .lt. 0.0) xlrgrz(i) = 0.0
          if (ifhsly .le. 0.or.ivarso(3) .le. 0) then
            if (hetcnt(i) .lt. 0.0) ifset = 0
          end if
          if (ifhsly .le. 0.or.ivarso(4) .le. 0) then
            if (brnrat(i) .lt. 0.0) ifset = 0
          end if
          if (ifhsly .le. 0) then
            if (taucld(i) .le. 0.0) ifset = 0
          end if
          if (gammat(i) .le. 0.0) then
            if (IVGET(isotyp(i),2) .eq. 1) then
              gammat(i) = gammai
            else
              gammat(i) = gammac
            end if
          end if
          if (alphas(i) .le. 0.0) then
            if (taucld(i) .le. 15.0) then
              alphas(i) = 1.0
            else
              alphas(i) = 0.9
            end if
          end if
          if (betass(i) .le. 0.0) betass(i) = 1.0
          if (i .lt. nsourc.and.ifset .ne. 0) go to 50
          if (ifset .gt. 0.and.ifgpwn .gt. 0) then
            if (nvsprt .gt. 0) then
              i = 0
   60         i = i + 1
              if (drpupr(i) .le. 0.0) ifset = 0
              if (drplwr(i) .le. 0.0) drplwr(i) = drpupr(i)
              if (pctmat(i) .lt. 0.0) ifset = 0
              if (vspart(i) .gt. rmissc.and.vspart(i) .le. 0.0)
     *            ifset = 0
              if (gammad(i) .gt. rmissc.and.gammad(i) .lt. 0.0)
     *            ifset = 0
              if (i .lt. nvsprt.and.ifset .gt. 0) go to 60
            else
              ifset = 0
            end if
          end if
        end if
      end if
      ifrdwt(3) = ifset
      jfnxt = 0
      go to 90
   70 jfnxt = 12
      go to 90
   80 jfnxt = 9
   90 return
      end
