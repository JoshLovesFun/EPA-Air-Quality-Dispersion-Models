      Subroutine RINPD
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - RINPD Version 01.2                          ---
CC--- Subroutine description - displays and reads receptor data.    ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      character*7 mpol
      character*58 mfnam
CC
      dimension ker(6),nvls(3),ifrm(3)
CC
      include 'OCNTRL.INC'
      include 'ONAMES.INC'
      include 'OCDSPM.INC'
      include 'OCALCS.INC'
      include 'OWNTRL.INC'
CC
      equivalence (jfnxt,functx(1)),(ker(1),functx(2))
CC
      data mpol/'(polar)'/
CC
CC********************************************************************
CC
CC      start screen number.
      numsc = numscs(4)
      ifnext = 3
      if (jfnxt .gt. 0.and.ifgeng .gt. 0) go to 30
      if (iswopt(6) .eq. 2) then                                        020797
        if (ifdbug .eq. 26) then                                        020797
          if (KERRS(67,mnpbuf,ier,idum,xdum,xdum,xdum,xdum,0) .eq. 1)   020797
     *        go to 170                                                 020797
          iswopt(6) = 0                                                 020797
        end if                                                          020797
      end if                                                            020797
CC
      i = JINIT(ker,6,0,0)
CC      display and read data for screens numsc to numsc+3.
CC
CC      screen numsc (04), start of receptor data.
   10 numf = IRDSC(numsc,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
      nvl = IWHER(8,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
      mscrnm(nsc,nln) = malpha(14)
      nvl = IWHER(9,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
      mscrnm(nsc,nln) = malpha(14)
      nvl = IWHER(10,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
      mscrnm(nsc,nln) = malpha(14)
      nvl = IWHER(6,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
      if (iswopt(6) .eq. 0) then
        call mblk1(mscrnm(nsc,nln),nch)
      else if (iswopt(6) .eq. 1) then
        i = IWTBF(mscrnm(nsc,nln),nch,zcoord(1),1,iflrjs)
      else if (iswopt(6) .eq. 3) then                                   120702    
        call MBLK1(mscrnm(nsc,nln),nch)                                 120702
        if (iflrjs .eq. 0) then                                         120702
          mscrnm(nsc+nch-1,nln) = malpha(24)                            120702
        else                                                            120702
          mscrnm(nsc,nln) = malpha(24)                                  120702
        end if                                                          120702
      else
        if (ifgpwn .gt. 0.or.nvsprt .gt. 0) then
          iferrm(nvl,1) = 1
        end if
        call MBLK1(mscrnm(nsc,nln),nch)
        if (iflrjs .eq. 0) then
          mscrnm(nsc+nch-1,nln) = malpha(3)
        else
          mscrnm(nsc,nln) = malpha(3)
        end if
      end if
      iferrs = IOSCN(numsc,mscrnm,ilinem,itabsm,ispacm,iferrm,lstrw,0)
      if (iferrs .ne. 10) i = IWTSC(numsc,1,mscrnm,ilinem,iferrm)
CC      if terminate.
      if (iferrs .eq. 1) go to 160
      ker(1) = IFWER(iferrm,nmxlin*nmxfld)
CC      flag to indicate if screen data must be rewritten.
CC      0 - no
CC      1 - yes
      ifwrt = 0
CC      flag to indicate if grid system is to be generated/read or not.
CC      0 - no
CC      1 - yes
      ifgeng = -IABS(ifgeng)
CC      save user response.
      jfers = iferrs
CC
CC      check users response.
      if (jfers .eq. 2.or.jfers .eq. 11) then
        go to 160
      else if (jfers .ge. 3.and.jfers .le. 7) then
        iferrs = IFRSP(jfers,mscrnm,ilinem,itabsm,ispacm,iferrm,0)
CC        terminate or main menu.
        if (iferrs .eq. 1.or.iferrs .eq. 2) then
          jfers = iferrs
          go to 160
        else
CC          redisplay menu.
          go to 10
        end if
      else if (jfers .ge. 8.and.jfers .le. 10) then
        go to 10
      end if
CC      turn off error array.
      i = JINIT(iferrm,nmxlin*nmxfld,0,0)
CC
CC      check data.
CC
CC      if rectangular coordinates, r or blank.
      nvl = IWHER(0,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (IFCHR(mscrnm(nsc,nln),18) .ne. 0.or.
     *    mscrnm(nsc,nln) .eq. mblnk1) then
CC        rectangular.
        igrect = 0
        if (mscrnm(nsc,nln) .eq. mblnk1) then
          mscrnm(nsc,nln) = malpha(18)
          ifwrt = 1
        end if
CC      if polar coordinates, p.
      else if (IFCHR(mscrnm(nsc,nln),16) .ne. 0) then
CC        polar.
        igrect = 1
      else
CC        error in input field.
        iferrm(nvl,1) = 1
      end if
CC
CC      set receptor x,y units index.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        call UNITS(mscrnm(nsc,nln),nch,i,idumy,0,0,0)
        if (i .lt. 0) then
CC          error.
          iferrm(nvl,1) = 1
        else
CC          set length units index.
          igunit = i
        end if
      else
CC        field is blank, default to m.
        ifwrt = 1
        call MBLK1(mscrnm(nsc,nln),nch)
        if (iflrjs .eq. 0) then
          mscrnm(nsc+nch-1,nln) = malpha(39)
        else
          mscrnm(nsc,nln) = malpha(39)
        end if
        igunit = 0
      end if
CC
CC      set height/elevation units index.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
        call UNITS(mscrnm(nsc,nln),nch,i,idumy,0,0,0)
        if (i .lt. 0) then
CC          error.
          iferrm(nvl,1) = 1
        else
CC          set height units index.
          izunit = i
        end if
      else
CC        field is blank, default to m.
        ifwrt = 1
        call MBLK1(mscrnm(nsc,nln),nch)
        if (iflrjs .eq. 0) then
          mscrnm(nsc+nch-1,nln) = malpha(39)
        else
          mscrnm(nsc,nln) = malpha(39)
        end if
        izunit = 0
      end if
CC
CC      receptor x origin.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
      if (i .gt. 0) then
CC        error.
        iferrm(nvl,1) = 1
      else if (i .lt. 0) then
CC        default x origin.
        ifwrt = 1
        xorgin = 0.0
        i = IWTBF(mscrnm(nsc,nln),nch,xorgin,1,iflrjs)
      else
CC        set x origin.
        xorgin = xdumy
      end if
CC
CC      receptor y origin.
      nsc = JWHER(nvl,itabsm,ispacm,2,nch)
      i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
      if (i .gt. 0) then
CC        error.
        iferrm(nvl,2) = 1
      else if (i .lt. 0) then
CC        default y origin.
        ifwrt = 1
        yorgin = 0.0
        i = IWTBF(mscrnm(nsc,nln),nch,yorgin,1,iflrjs)
      else
CC        set y origin.
        yorgin = xdumy
      end if
CC
CC      see if add source rectangular coordinates to grid system
CC      xorgin, yorgin.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (numc1(mscrnm(nsc,nln),nch) .gt. 0) then
        if (ifchr(mscrnm(nsc,nln),14) .ne. 0) then
          iswopt(23) = 0
        else if (ifchr(mscrnm(nsc,nln),25) .ne. 0) then
          iswopt(23) = 1
        else
          iferrm(nvl,1) = 1
        end if
      else
        iswopt(23) = 0
      end if
CC
CC      complex terrain/flag pole/flat terrain flag, iswopt(6).
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
      if (i .gt. 0) then
        iferrm(nvl,1) = 1
      else if (i .eq. -2) then
        if (ifdbug .ne. 26) then                                        020797
          iswopt(6) = 2
        else                                                            020797
          mscrnm(nsc,nln) = mblnk1                                      020797
          iswopt(6) = 0                                                 020797
          if (KERRS(67,mnpbuf,ier,idum,xdum,xdum,xdum,xdum,0) .eq. 1)   020797
     *        go to 170                                                 020797
        end if                                                          020797
      else if (i .eq. -1) then                                          120702
       iswopt(6) = 3                                                    120702
      else if (i .lt. 0) then
        zcoord(1) = 0.0
        iswopt(6) = 0
      else
        zcoord(1) = xdumy
        if (zcoord(1) .gt. 0.0) then
          iswopt(6) = 1
        else
          iswopt(6) = 0
          zcoord(1) = 0.0
        end if
      end if
CC
CC      orientation angle units.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (mscrnm(nsc,nln) .ne. mblnk1) then
        call UNITS(mscrnm(nsc,nln),nch,i,idumy,0,0,1)
        if (i .lt. 0) then
CC          error.
          iferrm(nvl,1) = 1
        else
CC          set orientation angle units index.
          igangu = i
        end if
      else
CC        default orientation angle units index to degrees, d.
        ifwrt = 1
        mscrnm(nsc,nln) = malpha(30)
        igangu = 0
      end if
CC
CC      receptor orientation angle.
      nsc = JWHER(nvl,itabsm,ispacm,2,nch)
      i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
   20 if (i .gt. 0) then
CC        error.
        iferrm(nvl,2) = 1
      else if (i .lt. 0) then
CC        default orientation angle to zero.
        ifwrt = 1
        grdang = 0.0
        i = IWTBF(mscrnm(nsc,nln),nch,grdang,1,iflrjs)
      else
        if (xdumy .lt. -179.9.or.xdumy .ge. 360.0) then
CC          orientation angle out of range.
          i = 1
          go to 20
        end if
CC        set orientation angle.
        grdang = xdumy
      end if
CC
CC      flag to generate/read receptor grid system axes.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (IFCHR(mscrnm(nsc,nln),14) .ne. 0.or.
     *    mscrnm(nsc,nln) .eq. mblnk1) then
CC        no grid generation.
        ifgeng = -IABS(ifgeng)
      else if (IFCHR(mscrnm(nsc,nln),25) .ne. 0) then
CC        generate grid system.
        ifgeng = 1
      else
CC        error.
        iferrm(nvl,1) = 1
        ifgeng = -IABS(ifgeng)
      end if
CC
CC      flag to delete current x,y grid system receptors
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (IFCHR(mscrnm(nsc,nln),14) .ne. 0.or.
     *    mscrnm(nsc,nln) .eq. mblnk1) then
CC        no delete.
        ifdxy = 0
      else if (IFCHR(mscrnm(nsc,nln),25) .ne. 0) then
CC        delete current grid system receptors.
        ifdxy = 1
      else
CC        error.
        iferrm(nvl,1) = 1
        ifdxy = 0
      end if
CC
CC      flag to delete current discrete receptors.
      nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
      if (IFCHR(mscrnm(nsc,nln),14) .ne. 0.or.
     *    mscrnm(nsc,nln) .eq. mblnk1) then
CC        no delete.
        ifdyx = 0
      else if (IFCHR(mscrnm(nsc,nln),25) .ne. 0) then
CC        delete discrete receptors.
        ifdyx = 1
      else
CC        error.
        iferrm(nvl,1) = 1
        ifdyx = 0
      end if
      ner = IFWER(iferrm,nmxlin*nmxfld)
CC      if rewrite data flag is set.
      if (ifwrt .gt. 0.or.ner .gt. 0.or.(ner .eq. 0.and.ker(1) .gt. 0))
     *    then
        i = IWTSC(numsc,1,mscrnm,ilinem,iferrm)
      end if
      if (ner .gt. 0) then
        if (KERRP(22,0) .ge. 1) go to 160
      end if
      ker(1) = ner
      if (jfers .eq. 0.and.ker(1) .eq. 1) go to 10
      if (ifdxy .ne. 0) then
        nxpnts = 0
        nypnts = 0
        i = JRNIT(zcoord,nmxxyz*nmxxyz,rmissd,0)
        i = JRNIT(xcoord,nmxxyz,rmissd,0)
CC        write x axis screen menus.
        nsubs = nsubsc(numsc+2)
        ner = IPACK(nsubs,xcoord,xdumy,xdumy,xdumy,xdumy,xdumy,
     *              nmxxyz,nxpnts,numsc+2,mscrnm,ilinem,itabsm,ispacm,
     *              iferrm,1,1)
        if (ker(1) .eq. 0) ker(1) = ner
        i = JRNIT(ycoord,nmxxyz,rmissd,0)
CC        write y axis screen menus.
        nsubs = nsubsc(numsc+3)
        ner = IPACK(nsubs,ycoord,xdumy,xdumy,xdumy,xdumy,xdumy,
     *              nmxxyz,nypnts,numsc+3,mscrnm,ilinem,itabsm,ispacm,
     *              iferrm,2,1)
        if (ker(1) .eq. 0) ker(1) = ner
      end if
      if (ifdyx .ne. 0) then
        nxypnt = 0
        i = JRNIT(xdscrt,nmxxyz,rmissd,0)
        i = JRNIT(ydscrt,nmxxyz,rmissd,0)
        i = JRNIT(zdscrt,nmxxyz,rmissd,0)
CC        write discrete point screen menus.
        nsubs = nsubsc(numsc+5)
        ner = IPACK(nsubs,xdscrt,ydscrt,zdscrt,xdumy,xdumy,xdumy,
     *              nmxxyz,nxypnt,numsc+5,mscrnm,ilinem,itabsm,ispacm,
     *              iferrm,3,1)
        if (ker(1) .eq. 0) ker(1) = ner
      end if
CC
CC      if user wishes to generate grid system.
   30 if (ifgeng .gt. 0) then
        iflg = 0
        jflg = 0
        nn = 1
CC        screen numsc+1 (05), generate grid system axes.
CC
CC        read screen template and data from work file.
        numf = IRDSC(numsc+nn,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
CC        set receptor length units abbrev in menu.
        nvl = 1
        nln = ilinem(nvl)
        call UNITS(mscrnm(20,nln-1),3,igunit,0,0,1,0)
        nvl = 2
        nln = ilinem(nvl)
        call UNITS(mscrnm(20,nln-1),3,igunit,0,igrect,1,0)
        if (igrect .gt. 0) then
          call MVCH1(mpol,mscrnm(25,nln-1),7)
        end if
CC        display screen menu.
        iferrs = IOSCN(numsc+nn,mscrnm,ilinem,itabsm,ispacm,iferrm,
     *                 lstrw,0)
CC        save screen menu in work file.
        if (iferrs .ne. 10) i = IWTSC(numsc+nn,1,mscrnm,ilinem,iferrm) 
CC        if user wishes to terminate.
        if (iferrs .eq. 1) go to 160
        ker(2) = IFWER(iferrm,nmxlin*nmxfld)
        jfers = iferrs
CC
CC        check users response.
        if (jfers .eq. 2) then
          go to 160
        else if (jfers .ge. 3.and.jfers .le. 7) then
          iferrs = IFRSP(jfers,mscrnm,ilinem,itabsm,ispacm,iferrm,0)
CC          terminate or main menu.
          if (iferrs .eq. 1.or.iferrs .eq. 2) then
            jfers = iferrs
            go to 160
          else
CC            redisplay menu.
            go to 30
          end if
        else if (jfers .ge. 8.and.jfers .le. 10) then
          go to 30
        else if (jfers .eq. 11) then
          go to 10
        end if
CC        turn off error array.
        i = JINIT(iferrm,nmxlin*nmxfld,0,0)
CC
CC      check data.
CC        set x axis data counter to zero.
        ifdat = 0
        i = IWTSC(numsc+nn,1,mscrnm,ilinem,iferrm) 
CC
CC        start of x axis.
        nvl = IWHER(0,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
        if (i .ne. 0) then
CC          if non-blank field.
          if (i .ne. -3) then
CC            error.
            iferrm(nvl,1) = 1
          else
            gxstrt = rmissd
          end if
        else
CC          set start of x axis.
          gxstrt = xdumy
CC          increment data counter.
          ifdat = ifdat + 1
        end if
CC
CC        increment for x axis.
        nsc = JWHER(nvl,itabsm,ispacm,2,nch)
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
        if (i .ne. 0) then
CC          if non-blank field.
          if (i .ne. -3) then
CC            error.
            iferrm(nvl,2) = 1
          else
            gxincr = rmissd
          end if
        else
CC          set x axis increment.
          gxincr = xdumy
CC          increment data counter.
          if (IFEQU(gxincr,0.0) .ne. 0) ifdat = ifdat + 1
        end if
CC
CC        end of x axis.
        nsc = JWHER(nvl,itabsm,ispacm,3,nch)
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
        if (i .ne. 0) then
CC          if non-blank field.
          if (i .ne. -3) then
CC            error.
            iferrm(nvl,3) = 1
          else
            gxendp = rmissd
          end if
        else
CC          set end of x axis.
          gxendp = xdumy
CC          increment data counter.
          ifdat = ifdat + 1
        end if
CC
CC        set y axis data counter to zero.
        jfdat = 0
CC        start of y axis.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
        if (i .ne. 0) then
CC          if non-blank field.
          if (i .ne. -3) then
CC            error.
            iferrm(nvl,1) = 1
          else
            gystrt = rmissd
          end if
        else
CC          set start of y axis.
          gystrt = xdumy
CC          increment data counter.
          jfdat = jfdat + 1
        end if
CC
CC        increment for y axis.
        nsc = JWHER(nvl,itabsm,ispacm,2,nch)
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
        if (i .ne. 0) then
CC          if non-blank field.
          if (i .ne. -3) then
CC            error.
            iferrm(nvl,2) = 1
          else
            gyincr = rmissd
          end if
        else
CC          set y axis increment.
          gyincr = xdumy
CC          increment data counter.
          if (IFEQU(gyincr,0.0) .ne. 0) jfdat = jfdat + 1
        end if
CC
CC        end of y axis.
        nsc = JWHER(nvl,itabsm,ispacm,3,nch)
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
        if (i .ne. 0) then
CC          if non-blank field.
          if (i .ne. -3) then
CC            error.
            iferrm(nvl,3) = 1
          else
            gyendp = rmissd
          end if
        else
CC          set end of y axis.
          gyendp = xdumy
CC          increment data counter.
          jfdat = jfdat + 1
        end if
CC
CC        if x axis generation parameters are specified.
        if (ifdat .ge. 3) then
CC          generate x axis coordinates.
          i = JRNIT(xcoord,nmxxyz,rmissd,0)
          if (gxstrt .gt. gxendp) then
            if (gxincr .gt. 0.0) gxincr = -gxincr
          else if (gxstrt .lt. gxendp) then
            if (gxincr .lt. 0.0) gxincr = -gxincr
          end if
          xcoord(1) = gxstrt
          n = 1
   40     n = n + 1
          if (n .le. nmxxyz) then
            if ((gxincr .gt. 0.0.and.xcoord(n-1)+gxincr .le. gxendp)
     *         .or.(gxincr .lt. 0.0.and.xcoord(n-1)+gxincr .ge.
     *         gxendp)) then
              xcoord(n) = xcoord(n-1) + gxincr
              go to 40
            end if
          end if
          nxpnts = n - 1
          iflg = 1
        end if
CC
CC        if y axis generation parameters are specified.
        if (jfdat .ge. 3) then
CC          generate y axis coordinates.
          i = JRNIT(ycoord,nmxxyz,rmissd,0)
          if (gystrt .gt. gyendp) then
            if (gyincr .gt. 0.0) gyincr = -gyincr
          else if (gystrt .lt. gyendp) then
            if (gyincr .lt. 0.0) gyincr = -gyincr
          end if
          ycoord(1) = gystrt
          n = 1
   50     n = n + 1
          if (n .le. nmxxyz) then
            if ((gyincr .gt. 0.0.and.ycoord(n-1)+gyincr .le. gyendp)
     *          .or.(gyincr .lt. 0.0.and.ycoord(n-1)+gyincr .ge.
     *          gyendp)) then
              ycoord(n) = ycoord(n-1) + gyincr
              go to 50
            end if
          end if
          nypnts = n - 1
          jflg = 1
        end if
CC
CC        get name of input met. data file.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        ixyfl = 0
        if (NUMC1(mscrnm(nsc,nln),nch) .gt. 0) then
          call MBLK1(mxynam,22)
          call MVCH1(mscrnm(nsc,nln),mxynam,MIN0(nch,22))
          i = LRJST(mxynam,22,1)
CC          open data file as logical unit ixyfl.
          ixyfl = 31
CC          combine file name and directory.
          call FNAME(mfnam,58,mdirec,mxynam)
CC          formatted character.
          OPEN (unit=ixyfl,file=mfnam,iostat=ier,err=60,status='old',
     *          form='formatted')
          go to 70
   60     iferrm(nvl,1) = 1
          if (KERRS(3,mxynam,ier,idum,xdum,xdum,xdum,xdum,0) .ge. 1)
     *        go to 170
          ixyfl = 0
        else                                                            112196
          mxynam = mblnk1                                               112196
        end if
        if (ixyfl .eq. 0.and.iflg .eq. 0.and.jflg .eq. 0) ifgeng = 0
   70   if (ixyfl .gt. 0) then
CC
CC          get X axis format
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          nvls(1) = nvl
          i = LRJST(mscrnm(nsc,nln),nch,1)
          if (i .gt. 0) then
            if (i .gt. 1) then
              call MVCH1(mscrnm(nsc,nln),mxform,nch)
              ier = IFORM(mxform,nch,nch,nvl)
              if (ier .gt. 0) then
                ifwrt = 1
                call MVCH1(mxform,mscrnm(nsc,nln),nch)
              end if
              ifrm(1) = 2
            else
              if (mscrnm(nsc,nln) .eq. '*') then
                ifrm(1) = 1
              else
                iferrm(nvl,1) = 1
              end if
            end if
          else
            ifrm(1) = 0
          end if
          i = LRJST(mscrnm(nsc,nln),nch,iflrjs)
CC
CC          get Y axis format
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          nvls(2) = nvl
          i = LRJST(mscrnm(nsc,nln),nch,1)
          if (i .gt. 0) then
            if (i .gt. 1) then
              call MVCH1(mscrnm(nsc,nln),myform,nch)
              ier = IFORM(myform,nch,nch,nvl)
              if (ier .gt. 0) then
                ifwrt = 1
                call MVCH1(myform,mscrnm(nsc,nln),nch)
              end if
              ifrm(2) = 2
            else
              if (mscrnm(nsc,nln) .eq. '*') then
                ifrm(2) = 1
              else
                iferrm(nvl,1) = 1
              end if
            end if
          else
            ifrm(2) = 0
          end if
          i = LRJST(mscrnm(nsc,nln),nch,iflrjs)
CC
CC          get Z elevation format
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          nvls(3) = nvl
          i = LRJST(mscrnm(nsc,nln),nch,1)
          if (i .gt. 0) then
            if (i .gt. 1) then
              call MVCH1(mscrnm(nsc,nln),mzform,nch)
              ier = IFORM(mzform,nch,nch,nvl)
              if (ier .gt. 0) then
                ifwrt = 1
                call MVCH1(mzform,mscrnm(nsc,nln),nch)
              end if
              ifrm(3) = 2
            else
              if (mscrnm(nsc,nln) .eq. '*') then
                ifrm(3) = 1
              else
                iferrm(nvl,1) = 1
              end if
            end if
          else
            ifrm(3) = 0
          end if
          i = LRJST(mscrnm(nsc,nln),nch,iflrjs)
          if (ISUMI(ifrm(1),3) .le. 0) then
            iferrm(nvls(1),1) = 1
            iferrm(nvls(2),1) = 1
            iferrm(nvls(3),1) = 1
          end if
CC
CC          get order of Z elevations.
          nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
          if (mscrnm(nsc,nln) .ne. mblnk1) then
            if (IFCHR(mscrnm(nsc,nln),1) .ne. 0) then
              iswopt(31) = 0
            else if (IFCHR(mscrnm(nsc,nln),2) .ne. 0) then
              iswopt(31) = 1
            else
              iferrm(nvl,1) = 1
            end if
          else
            iswopt(31) = 0
          end if
CC
CC          read data file
          REWIND (unit=ixyfl,err=80)
   80     irec = 1
CC          get Nx and Ny number of X and Y coordinates
          read (ixyfl,*,iostat=ier,err=130,end=140) nx,ny
          if (nx .le. 0.and.nxpnts .gt. 0) nx = nxpnts
          if (ny .le. 0.and.nypnts .gt. 0) ny = nypnts
          if (nx .le. 0.or.nx .gt. nmxxyz) then
            xdum = nx
            i = KERRS(1,mxynam,0,irec,xdum,xdum,xdum,xdum,0)
            i = KERRS(21,mxynam,idum,idum,xdum,xdum,xdum,xdum,0)
            go to 30
          end if
          if (ny .le. 0.or.ny .gt. nmxxyz) then
            xdum = ny
            i = KERRS(1,mxynam,0,irec,xdum,xdum,xdum,xdum,0)
            i = KERRS(21,mxynam,idum,idum,xdum,xdum,xdum,xdum,0)
            go to 30
          end if
          if (ifrm(1) .gt. 0) then
            n = 1
            i = JRNIT(xcoord,nmxxyz,rmissd,0)
            if (ifrm(1) .eq. 1) then
              read (ixyfl,*,iostat=ier,err=130,end=140)
     *                (xcoord(i),i=1,nx)
            else
              read (ixyfl,mxform,iostat=ier,err=130,end=140)
     *                (xcoord(i),i=1,nx)
            end if
            nxpnts = nx
            iflg = 1
          end if
          irec = irec + 1
          if (ifrm(2) .gt. 0) then
            n = 2
            i = JRNIT(ycoord,nmxxyz,rmissd,0)
            if (ifrm(2) .eq. 1) then
              read (ixyfl,*,iostat=ier,err=130,end=140)
     *                (ycoord(j),j=1,ny)
            else
              read (ixyfl,myform,iostat=ier,err=130,end=140)
     *                (ycoord(j),j=1,ny)
            end if
            nypnts = ny
            jflg = 1
          end if
          irec = irec + 1
          if (ifrm(3) .gt. 0) then
            n = 3
            i = JRNIT(zcoord,nmxxyz*nmxxyz,rmissd,0)
            if (ifrm(3) .eq. 1) then
              if (iswopt(31) .eq. 0) then
                do 90 j=1,ny
                  k = (j - 1) * nx
                  read (ixyfl,*,iostat=ier,err=130,end=140)
     *                    (zcoord(k+i),i=1,nx)
                  irec = irec + 1
   90           continue
              else
                do 100 i=1,nx
                  read (ixyfl,*,iostat=ier,err=130,end=140)
     *                    (zcoord((j-1)*nx+i),j=1,ny)
                  irec = irec + 1
  100           continue
              end if
            else
              if (iswopt(31) .eq. 0) then
                do 110 j=1,ny
                  k = (j - 1) * nx
                  read (ixyfl,mzform,iostat=ier,err=130,end=140)
     *                    (zcoord(k+i),i=1,nx)
                  irec = irec + 1
  110           continue
              else
                do 120 i=1,nx
                  read (ixyfl,mzform,iostat=ier,err=130,end=140)
     *                    (zcoord((j-1)*nx+i),j=1,ny)
                  irec = irec + 1
  120           continue
              end if
            end if
          end if
          go to 150
  130     if (KERRS(1,mxynam,ier,irec,xdum,xdum,xdum,xdum,0) .eq. 1)
     *        go to 170
          iferrm(nvls(n),1) = 1
          go to 150
  140     if (KERRS(4,mxynam,irec,idum,xdum,xdum,xdum,xdum,0) .eq. 1)
     *       go to 170
          iferrm(nvls(n),1) = 1
        end if
CC        if data errors, go redisplay screen numsc+1 menu.
  150   ner = IFWER(iferrm,nmxlin*nmxfld)
        if (ner .gt. 0) then
          if (KERRP(22,0) .ge. 1) go to 160
        end if
        ker(2) = ner
        if (iflg .ne. 0.and.ner .eq. 0) then
CC
CC          write x axis screen menus.
          nsubs = nsubsc(numsc+2)
          ner = IPACK(nsubs,xcoord,xdumy,xdumy,xdumy,xdumy,xdumy,
     *                nmxxyz,nxpnts,numsc+2,mscrnm,ilinem,itabsm,ispacm,
     *                iferrm,1,1)
          if (ker(2) .eq. 0) ker(2) = ner
        end if
        if (jflg .ne. 0.and.ner .eq. 0) then
CC
CC          write y axis screen menus.
          nsubs = nsubsc(numsc+3)
          ner = IPACK(nsubs,ycoord,xdumy,xdumy,xdumy,xdumy,xdumy,
     *                nmxxyz,nypnts,numsc+3,mscrnm,ilinem,itabsm,ispacm,
     *                iferrm,2,1)
          if (ker(2) .eq. 0) ker(2) = ner
        end if
        if (jfers .eq. 0.and.ker(2) .eq. 1) then
          ifgeng = 1
          go to 30
        end if
      end if
      jfnxt = 2
      go to 170
  160 jfnxt = 5
  170 if (ixyfl .gt. 0) then
        CLOSE (unit=ixyfl,status='keep',err=180)
      end if
  180 return
      end
