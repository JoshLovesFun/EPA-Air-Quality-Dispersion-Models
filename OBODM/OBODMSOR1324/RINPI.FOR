      Subroutine RINPI
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - RINPI Version 01.1                          ---
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
     *            (isorc,functx(70)),(ifchg,functx(71)),
     *            (ifdtl,functx(72))
CC
CC********************************************************************
CC
   10 if (ifdtl .gt. 0) then
CC
CC        screen numsc+8 (18) get more source data.
CC        get source emission type.
        nn = 8
        itype = IVGET(isotyp(isorc),2)
CC        get source type.
        jtype = IVGET(isotyp(isorc),1)
        numf = IRDSC(numsc+nn,isorc,mscrnm,ilinem,itabsm,ispacm,
     *               iferrm,1)
CC        indicate source number in screen menu.
        xdumy = isorc
        nvl = IWHER(0,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        i = IWTBF(mscrnm(21,nln-3),2,xdumy,0,iflrjs)
CC        place source id in screen menu.
        call MVCH1(msoids(1,isorc),mscrnm(37,nln-3),36)
CC        default air entrainment coefficient.
        nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
        if (NUMC1(mscrnm(nsc,nln),nch) .le. 0.or.ifchg .ne. 0) then
CC          default air entrainment coefficient.
          if (taucld(isorc) .gt. 15.0) then
            xdumy = gammac
          else
            xdumy = gammai
          end if
          i = IWTBF(mscrnm(nsc,nln),nch,xdumy,4,iflrjs)
        end if
CC        default lateral cloud expansion distance.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        nscs = JWHER(nvl,itabsm,ispacm,2,nchs)
        if (NUMC1(mscrnm(nscs,nln),nchs) .le. 0) then
CC          default.
          iunt = 0
          call UNITS(mscrnm(nsc,nln),nch,iunt,jdumy,0,1,0)
          xdumy = 50.0
          i = IWTBF(mscrnm(nscs,nln),nchs,xdumy,1,iflrjs)
        end if
CC        default vertical cloud expansion distance.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        nscs = JWHER(nvl,itabsm,ispacm,2,nchs)
        if (NUMC1(mscrnm(nscs,nln),nchs) .le. 0) then
CC          default.
          iunt = 0
          call UNITS(mscrnm(nsc,nln),nch,iunt,jdumy,0,1,0)
          xdumy = 50.0
          i = IWTBF(mscrnm(nscs,nln),nchs,xdumy,1,iflrjs)
        end if
CC        default lateral reference distance.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        nscs = JWHER(nvl,itabsm,ispacm,2,nchs)
        if (NUMC1(mscrnm(nscs,nln),nchs) .le. 0) then
CC          default.
          iunt = 0
          call UNITS(mscrnm(nsc,nln),nch,iunt,jdumy,0,1,0)
          xdumy = 0.0
          i = IWTBF(mscrnm(nscs,nln),nchs,xdumy,1,iflrjs)
        end if
CC        default vertical reference distance.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        nscs = JWHER(nvl,itabsm,ispacm,2,nchs)
        if (NUMC1(mscrnm(nscs,nln),nchs) .le. 0) then
CC          default.
          iunt = 0
          call UNITS(mscrnm(nsc,nln),nch,iunt,jdumy,0,1,0)
          xdumy = 0.0
          i = IWTBF(mscrnm(nscs,nln),nchs,xdumy,1,iflrjs)
        end if
CC        default lateral cloud expansion coefficient.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        if (NUMC1(mscrnm(nsc,nln),nch) .le. 0.or.ifchg .ne. 0) then
          if (taucld(isorc) .le. 15.0) then
            xdumy = 1.0
          else
            xdumy = 0.9
          end if
          i = IWTBF(mscrnm(nsc,nln),nch,xdumy,1,iflrjs)
        end if
        ifchg = 0
CC        default vertical cloud expansion coefficient.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        if (NUMC1(mscrnm(nsc,nln),nch) .le. 0) then
          xdumy = 1.0
          i = IWTBF(mscrnm(nsc,nln),nch,xdumy,1,iflrjs)
        end if
CC
CC        display screen menu.
        iferrs = IOSCN(numsc+nn,mscrnm,ilinem,itabsm,ispacm,iferrm,
     *                 lstrw,0)
CC        write data to work file.
        if (iferrs .ne. 10) i = IWTSC(numsc+nn,isorc,mscrnm,ilinem,
     *                                iferrm)
CC        if terminate.
        if (iferrs .eq. 1) go to 100
        ker(7) = IFWER(iferrm,nmxlin*nmxfld)
        jfers = iferrs
        ifwrt = 0
CC
CC        check user response to menu.
        if (jfers .eq. 2) then
          go to 90
        else if (jfers .ge. 3.and.jfers .le. 7) then
CC          display aux. menus.
          iferrs = IFRSP(jfers,mscrnm,ilinem,itabsm,ispacm,iferrm,0)
          if (iferrs .eq. 1.or.iferrs .eq. 2) then
CC            terminate or main menu.
            jfers = iferrs
            go to 90
          else
CC            redisplay menu.
            go to 10
          end if
        else if (jfers .ge. 8.and.jfers .le. 10) then
          go to 10
        else if (jfers .eq. 11) then
          go to 110
        end if
CC        turn off error array.
        i = JINIT(iferrm,nmxlin*nmxfld,0,0)
CC
CC        get data.
CC
CC        get air entrainment coefficient.
        nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
   20   if (i .gt. 0) then
CC          error.
          iferrm(nvl,1) = 1
        else if (i .lt. 0) then
CC          default air entrainment coefficient.
          if (taucld(isorc) .gt. 15.0) then
            gammat(isorc) = gammac
          else
            gammat(isorc) = gammai
          end if
          ifwrt = 1
          i = IWTBF(mscrnm(nsc,nln),nch,gammat(isorc),1,iflrjs)
        else
          if (xdumy .lt. 0.4.or.xdumy .ge. 1.0) then
            i = 1
            go to 20
          end if
CC          set entrainment coefficient.
          gammat(isorc) = xdumy
          if (taucld(isorc) .gt. 15.0) then
            xdumy = gammac
          else
            xdumy = gammai
          end if
          if (gammat(isorc) .lt. 0.9*xdumy.or.gammat(isorc) .gt.
     *        1.1*xdumy) then
            iferrm(nvl,1) = 2
          end if
        end if
CC
CC        get units of lateral rectilinear cloud expansion distance.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        if (NUMC1(mscrnm(nsc,nln),nch) .eq. 0) then
CC          default.
          iunt = 0
          call UNITS(mscrnm(nsc,nln),nch,iunt,jdumy,0,1,0)
          ifwrt = 1
        else
          call UNITS(mscrnm(nsc,nln),nch,i,jdumy,0,0,0)
          if (i .lt. 0) then
CC            error.
            iferrm(nvl,1) = 1
            iunt = -1
          else
CC            set units index.
            iunt = i
          end if
        end if
        if (iunt .ge. 0) then
          ismunt(isorc) = IVSTO(ismunt(isorc),1,iunt)
        end if
CC
CC        get lateral rectilinear cloud expansion distance.
        nsc = JWHER(nvl,itabsm,ispacm,2,nch)
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
   30   if (i .gt. 0) then
CC          error.
          iferrm(nvl,2) = 1
        else if (i .lt. 0) then
CC          default lateral expansion distance.
          xsmlry(isorc) = 50.0
          ifwrt = 1
          i = IWTBF(mscrnm(nsc,nln),nch,xsmlry(isorc),1,iflrjs)
        else
          if (xdumy .le. 0.0) then
            i = 1
            go to 30
          end if
CC          set cloud expansion distance.
          xsmlry(isorc) = xdumy
        end if
CC
CC        get units of vertical rectilinear cloud expansion distance.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        if (NUMC1(mscrnm(nsc,nln),nch) .eq. 0) then
CC          default.
          iunt = 0
          call UNITS(mscrnm(nsc,nln),nch,iunt,jdumy,0,1,0)
          ifwrt = 1
        else
          call UNITS(mscrnm(nsc,nln),nch,i,jdumy,0,0,0)
          if (i .lt. 0) then
CC            error.
            iferrm(nvl,1) = 1
            iunt = -1
          else
CC            set units index.
            iunt = i
          end if
        end if
        if (iunt .ge. 0) then
          ismunt(isorc) = IVSTO(ismunt(isorc),2,iunt)
        end if
CC
CC        get vertical rectilinear cloud expansion distance.
        nsc = JWHER(nvl,itabsm,ispacm,2,nch)
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
   40   if (i .gt. 0) then
CC          error.
          iferrm(nvl,2) = 1
        else if (i .lt. 0) then
CC          default vertical expansion distance.
          xsmlrz(isorc) = 50.0
          ifwrt = 1
          i = IWTBF(mscrnm(nsc,nln),nch,xsmlrz(isorc),1,iflrjs)
        else
          if (xdumy .le. 0.0) then
            i = 1
            go to 40
          end if
CC          set cloud expansion distance.
          xsmlrz(isorc) = xdumy
        end if
CC
CC        get units of lateral reference distance.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        if (NUMC1(mscrnm(nsc,nln),nch) .eq. 0) then
CC          default.
          iunt = 0
          call UNITS(mscrnm(nsc,nln),nch,iunt,jdumy,0,1,0)
          ifwrt = 1
        else
          call UNITS(mscrnm(nsc,nln),nch,i,jdumy,0,0,0)
          if (i .lt. 0) then
CC            error.
            iferrm(nvl,1) = 1
            iunt = -1
          else
CC            set units index.
            iunt = i
          end if
        end if
        if (iunt .ge. 0) then
          ismunt(isorc) = IVSTO(ismunt(isorc),3,iunt)
        end if
CC
CC        get lateral reference distance.
        nsc = JWHER(nvl,itabsm,ispacm,2,nch)
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
   50   if (i .gt. 0) then
CC          error.
          iferrm(nvl,2) = 1
        else if (i .lt. 0) then
CC          default lateral reference distance.
          xlrgry(isorc) = 0.0
          ifwrt = 1
          i = IWTBF(mscrnm(nsc,nln),nch,xlrgry(isorc),1,iflrjs)
        else
          if (xdumy .lt. 0.0) then
            i = 1
            go to 50
          end if
CC          set lateral reference distance.
          xlrgry(isorc) = xdumy
        end if
CC
CC        get units of vertical reference distance.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        if (NUMC1(mscrnm(nsc,nln),nch) .eq. 0) then
CC          default.
          iunt = 0
          call UNITS(mscrnm(nsc,nln),nch,iunt,jdumy,0,1,0)
          ifwrt = 1
        else
          call UNITS(mscrnm(nsc,nln),nch,i,jdumy,0,0,0)
          if (i .lt. 0) then
CC            error.
            iferrm(nvl,1) = 1
            iunt = -1
          else
CC            set units index.
            iunt = i
          end if
        end if
        if (iunt .ge. 0) then
          ismunt(isorc) = IVSTO(ismunt(isorc),4,iunt)
        end if
CC
CC        get vertical reference distance.
        nsc = JWHER(nvl,itabsm,ispacm,2,nch)
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
   60   if (i .gt. 0) then
CC          error.
          iferrm(nvl,2) = 1
        else if (i .lt. 0) then
CC          default vertical reference distance.
          xlrgrz(isorc) = 0.0
          ifwrt = 1
          i = IWTBF(mscrnm(nsc,nln),nch,xlrgrz(isorc),1,iflrjs)
        else
          if (xdumy .lt. 0.0) then
            i = 1
            go to 60
          end if
CC          set vertical reference distance.
          xlrgrz(isorc) = xdumy
        end if
CC
CC        get lateral cloud expansion coefficient.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
   70   if (i .gt. 0) then
CC          error.
          iferrm(nvl,1) = 1
        else if (i .lt. 0) then
CC          default lateral cloud expansion coefficient.
          if (taucld(isorc) .le. 15.0) then
            alphas(isorc) = 1.0
          else
            alphas(isorc) = 0.9
          end if
          ifwrt = 1
          i = IWTBF(mscrnm(nsc,nln),nch,alphas(isorc),1,iflrjs)
        else
          if (xdumy .lt. 0.8.or.xdumy .gt. 1.0) then
            i = 1
            go to 70
          end if
CC          set lateral cloud expansion coefficient.
          alphas(isorc) = xdumy
          if (xdumy .lt. 1.0) then
            if (xdumy .lt. 0.9.or.taucld(isorc) .le. 15.0) then
              iferrm(nvl,1) = 2
            end if
          end if
        end if
CC
CC        get vertical cloud expansion coefficient.
        nvl = IWHER(nvl,ilinem,itabsm,ispacm,1,1,nln,nsc,nch)
        i = IRDBF(mscrnm(nsc,nln),nch,xdumy)
   80   if (i .gt. 0) then
CC          error.
          iferrm(nvl,1) = 1
        else if (i .lt. 0) then
CC          default vertical cloud expansion coefficient.
          betass(isorc) = 1.0
          ifwrt = 1
          i = IWTBF(mscrnm(nsc,nln),nch,betass(isorc),1,iflrjs)
        else
          if (xdumy .lt. 0.8.or.xdumy .gt. 1.0) then
            i = 1
            go to 80
          end if
CC          set vertical cloud expansion coefficient.
          betass(isorc) = xdumy
          if (xdumy .lt. 1.0) then
              iferrm(nvl,1) = 2
          end if
        end if
        ner = IFWER(iferrm,nmxlin*nmxfld)
        if (ifwrt .gt. 0.or.ner .gt. 0.or.(ner .eq. 0.and.
     *      ker(7) .gt. 0)) then
CC          rewrite screen data to work file.
          i = IWTSC(numsc+nn,1,mscrnm,ilinem,iferrm)
        end if
        if (ner .gt. 0) then
          if (KERRP(22,0) .ge. 1) go to 100
        end if
        ker(7) = ner
        if (jfers .eq. 0.and.ker(7) .eq. 1) go to 10
      end if
CC
CC      go get next source.
      if (nsourc .lt. isorc) then
          nsourc = isorc
          numf = IRDSC(numsc+3,1,mscrnm,ilinem,itabsm,ispacm,iferrm,1)
CC          set total number of sources.
          nvl = IWHER(1,ilinem,itabsm,ispacm,0,1,nln,nsc,nch)
          xdumy = nsourc
          i = IWTBF(mscrnm(nsc,nln),nch,xdumy,0,iflrjs)
          i = IWTSC(numsc+3,1,mscrnm,ilinem,iferrm)
      end if
      jfnxt = 4
      go to 120
   90 jfnxt = 11
      go to 120
  100 jfnxt = 12
      go to 120
  110 jfnxt = 6
  120 return
      end
