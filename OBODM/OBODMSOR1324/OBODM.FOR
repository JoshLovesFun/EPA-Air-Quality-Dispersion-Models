      Program OBODM
CC                       Program OBODM
CC     'Dugway Proving Ground Open Burn/Detonation Model'
CC                 'Version 01.3 - 07/24/97'                            041806
CC
CC     OBODM Version 01.3 program by -
CC     Jay R. Bjorklund, James F. Bowers & Greg C. Dodd
CC
CC     H.E. Cramer Co. Inc., P.O. Box 411, Sandy, UT 84091-0411,
CC     (801) 561-4964 or 561-7206
CC                     and
CC     Meteorological & Modeling Division
CC     West Desert Test Center, Dugway, Utah 84022-5000, (801) 831-5101
CC
CC**********************************************************************
CC**********************************************************************
CC
CC     This program is designed to calculate -
CC        1) Dosage
CC        2) concentration
CC        3) Time-average concentration
CC        4) Gravitational deposition
CC     Downwind of multiple -
CC        1) Instantaneous
CC        2) Quasi-continuous square wave
CC     Sources -
CC        1) Volume/point
CC        2) Line
CC     Program capabilities include -
CC        1) The ability to model multiple sources.
CC        2) The ability to model multiple types of sources.
CC        3) The ability to use either a rectangular or polar receptor
CC           grid system.
CC        4) The ability to specify source locations in either rectang-
CC           ular or polar coordinates.
CC        5) The ability to specify input and output data files in
CC           standard or user specified formats.
CC        7) The ability to provide multiple graphics displays in
CC           addition to printed output of calculations.
CC        8) The ability to use complex terrain.
CC
CC     All calculations in this program are relative to a rectangular
CC     coordinate system where 0 degrees north is the positive y axis
CC     and 90 degrees east is the positive x axis. All sources
CC     and calculation receptor points are relative to the origin
CC     of this coordinate system. All vertical measures referenced
CC     in this program are relative to the ground (zero).
CC
CC**********************************************************************
CC**********************************************************************
CC
CC
CCPCITS
      integer*2 kdy,khr,kmn,kms,idm1,idm2,ipos,istat,isvbk
CCPCITE
      character*1 mdumy,mslsh,mnull
      character*10 mbufr
      character*12 msch1,msch2
CC
      include 'OCNTRL.INC'
      include 'ONAMES.INC'
      include 'OCDSPM.INC'
      include 'OCALCS.INC'
      include 'OWNTRL.INC'
CC
      equivalence (jfnxt,functx(1)),(kfnxt,wspmin),(mbufr,mnpbuf(1))
CC
      data mslsh/'/'/,msch1/'SCRATCH1.OBD'/,msch2/'SCRATCH2.OBD'/
CC
CC**********************************************************************
CC
      mnull = CHAR(0)
      ifccok = -1
      ifctlc = 0
      iswopt(33) = 1                                                    090500
      iswopt(34) = 0                                                    051503
      iswopt(35) = 1                                                    051503
CCPCITS
      i = 0
      n = NARGS()
CC      see if menus are to be regenerated (/m) or debug /a through /f  020797
CC      or plot X, Z isopleths /z.
      if (n .gt. 0) then
        ipos = 1
CC        get option.
        call GETARG(ipos,mbufr,istat)
        if (istat .eq. 2) then
          if (mnpbuf(1) .eq. mslsh) then
            j = 0                                                       020797
            do while (j .lt. 6)                                         020797
              j = j + 1                                                 020797
              if (IFCHR(mnpbuf(2),j) .ne. 0) then                       020797
CC                Set debug level, 1-6.                                 020797
                ifdbug = j                                              020797
              end if                                                    020797
            end do                                                      020797
            if (IFCHR(mnpbuf(2),13) .ne. 0) then
CC              /m entered
              i = 13                                                    020797
            end if
            if (IFCHR(mnpbuf(2),26) .ne. 0) then                        020797
CC              The entered Y axis values are treated as Z values to    020797
CC              calculate centerline concentrations at each X,Z         020797
CC              coordinate.  Isopleths of X,Z concentrations can be     020797
CC              plotted.                                                020797
              ifdbug = 26                                               020797
            end if                                                      020797
          end if
        end if
      end if
      if (i .eq. 13) then                                               020797
        call GMENU
      end if
      call GTBRK(isvbk)
CC      Signal support
      i = IRSBK()
      idm1 = 1
      call STBRK(idm1)
CCPCITE
CC      get start time for run time.  prog. uses clock time, so run
CC      with little or no competition when timing a run.
CCOMP1S
c     call ntime(kyr,kmo,kdy,khr,kmn)
CCOMP1E
CCPCITS
      call GETTIM(khr,kmn,kms,idm2)
      call GETDAT(idm1,idm2,kdy)
CCPCITE
      xtimes = kdy * 24.0 * 3600.0 + khr * 3600.0 + kmn * 60.0 + kms
CC
      go to 20                                                          090498
   10 if (ifnext .eq. 3) then                                           090498
        if (ifbtch .eq. 0) then                                         090498
          if (ifrsav .ne. 0) then                                       090498
            ifnext = 2                                                  090498
          end if                                                        090498
        else                                                            090498
          ifnext = 2                                                    090498
        end if                                                          090498
      end if                                                            090498
   20 if (iferrs .ne. 1) then
CC-DEBUGS
CC>     if (ifdbug .gt. 0.and.ifdbug .lt. 7) then                       020797
CC>       WRITE (5,9000) iferrs,ifnext,ifmode
CC>       READ (6,*) iferrs
CC>     end if
CC-DEBUGE
CC        ifnext - determines which segment (Subroutine) to execute.
CC                 1 - RINPA, default drive and directory.
CC                 2 - default data save files.
CC                 3 - main menu.
CC                 4 - receptor data menus.
CC                 5 - source data menus.
CC                 6 - meteorological data menus.
CC                 7 - model control/options data menus.
CC                 8 - graphics data menus.
CC                 9 - save, print input data.
CC                10 - print input data.
CC                11 - print input data,initialize data for obod models.
CC                12 - continue execute obod models.
CC                13 - continue execute obod models.
CC                14 - print obod model solutions.
CC                15 - get what to plot, averaging time.
CC                16 - get data quantity to be plotted.
CC                17 - plot labels, special symbols, isopleths, legends
CC                18 - get plot details, orientation, etc.
CC                19 - get plot details, scale, color, etc.
CC                20 - start plot, draw axes, labels, etc.
CC                21 - plot selected functions.
CC
        if (ifinit .ge. 0) then
          if ((ifnext .ge. 9.and.ifnext .le. 14).or.ifnext .ge. 20) then
            ifccok = 2
          else
            ifccok = 1
          end if
          if (ifctlc .eq. 2) then
            if (JFCTC(52) .ne. 0) go to 20
          end if
        end if
        if (ifnext .le. 1) then
CC          go get default disc drive and directory.
          call RINPA
          if (iferrs .ne. 1) then
            if (ifinit .ne. 0) then
              call RINPB
              call RINDA
              call RINDB
              jfnxt = 0
   30         if (jfnxt .lt. 5) then
                if (jfnxt .ge. 0.and.jfnxt .le. 1) then
                  call RINPD
                  go to 30
                else if (jfnxt .ge. 2.and.jfnxt .le. 4) then
                  call RINPE
                  go to 30
                end if
              end if
              jfnxt = 0
   40         if (jfnxt .lt. 12) then
                if (jfnxt .ge. 0.and.jfnxt .le. 2) then
                  call RINPF
                  go to 40
                else if (jfnxt .ge. 3.and.jfnxt .le. 5) then
                  call RINPG
                  go to 40
                else if (jfnxt .eq. 6) then
                  call RINPH
                  go to 40
                else if (jfnxt .eq. 7) then
                  call RINPI
                  go to 40
                else if (jfnxt .ge. 8.and.jfnxt .le. 9) then
                  call RINPJ
                  go to 40
                else if (jfnxt .ge. 10) then
                  call RINPK
                  if (jfnxt .gt. 0.and.jfnxt .lt. 12) go to 40
                end if
              end if
              jfnxt = 0
   50         if (jfnxt .lt. 8) then
                if (jfnxt .ge. 0.and.jfnxt .le. 1) then
                  call RINPL
                  go to 50
                else if (jfnxt .eq. 2) then
                  call RINPM
                  go to 50
                else if (jfnxt .eq. 3) then
                  call RINPN
                  go to 50
                else if (jfnxt .ge. 4.and.jfnxt .le. 5) then
                  call RINPO
                  go to 50
                else if (jfnxt .ge. 6) then
                  call RINPP
                  if (jfnxt .gt. 0) go to 50
                end if
              end if
              call RINPQ
              iferrs = 0
              ifinit = 0
              motnam = mblnk1
            end if
          end if
          ifnext = 2
          go to 20
        else if (ifnext .eq. 2) then
CC          go get default data save files.
          call RINPB
          if (iferrs .ne. 1) call RINDA
          if (iferrs .ne. 1) call RINDB
          go to 20
        else if (ifnext .eq. 3) then
CC          go display main menu.
          call RINPC
          go to 20
        else if (ifnext .eq. 4) then
CC          go get receptor data.
          jfnxt = 0
   60     if (jfnxt .lt. 5.and.iferrs .ne. 1) then
            if (jfnxt .ge. 0.and.jfnxt .le. 1) then
              call RINPD
              go to 60
            else if (jfnxt .ge. 2.and.jfnxt .le. 4) then
              call RINPE
              go to 60
            end if
          end if
          go to 20
        else if (ifnext .eq. 5) then
CC          go get source data.
          jfnxt = 0
   70     if (jfnxt .eq. 12.or.iferrs .eq. 1) go to 20
          if (jfnxt .ge. 0.and.jfnxt .le. 2) then
            call RINPF
            go to 70
          else if (jfnxt .ge. 3.and.jfnxt .le. 5) then
            call RINPG
            go to 70
          else if (jfnxt .eq. 6) then
            call RINPH
            go to 70
          else if (jfnxt .eq. 7) then
            call RINPI
            go to 70
          else if (jfnxt .ge. 8.and.jfnxt .le. 9) then
            call RINPJ
            go to 70
          else if (jfnxt .ge. 10) then
            call RINPK
            if (jfnxt .gt. 0.and.jfnxt .lt. 12) go to 70
          end if
          go to 20
        else if (ifnext .eq. 6) then
CC          go get meteorological data.
   80     if (jfnxt .ge. 8.or.iferrs .eq. 1) go to 20
          if (jfnxt .ge. 0.and.jfnxt .le. 1) then
            call RINPL
            go to 80
          else if (jfnxt .eq. 2) then
            call RINPM
            go to 80
          else if (jfnxt .eq. 3) then
            call RINPN
            go to 80
          else if (jfnxt .ge. 4.and.jfnxt .le. 5) then
            call RINPO
            go to 80
          else if (jfnxt .ge. 6) then
            call RINPP
            if (jfnxt .gt. 0) go to 80
          end if
          go to 20
        else if (ifnext .eq. 7) then
CC          go get model control/option data.
          call RINPQ
          go to 20
        else if (ifnext .eq. 8) then
CC          go initialize graphics.
          ifnext = 15
          kfnxt = 0
          call RINPR
          go to 20
        else if (ifnext .eq. 9) then
CC          go save input data.
          ifnext = 10
          call RPRTA
          if (ifnext .ne. 3.and.iferrs .ne. 1) then
            call RPRTB
          end if
          go to 20
        else if (ifnext .eq. 10) then
CC          go print input data.
          ifnext = 11
          call RPRTC
          go to 20
        else if (ifnext .eq. 11) then
CC          go initialize execution of obod models.
          ifnext = 12
          call RPRTD
          go to 20
        else if (ifnext .eq. 12) then
CC          go execute obod models.
          ifnext = 13
          call RMDLA
          if (iferrs .ne. 1.and.ifnext .eq. 14.and.ifmode .eq. 1) then
            call RMDLD                                                  110198
          end if
          go to 10                                                      090498
        else if (ifnext .eq. 13) then
CC          continue execute obod models.
          ifnext = 12
   90     call RMDLB                                                    110198
          if (iferrs .ne. 1.and.icpont .gt. 0) then                     110198
            call RMDLC                                                  110198
            go to 90                                                    110198
          end if                                                        110198
          if (iferrs .ne. 1.and.ifmode .eq. 1) then
            call RMDLD                                                  110198
          end if
          go to 10                                                      090498
        else if (ifnext .eq. 14) then
          if (ifbtch .eq. 0) then
            if (ifrsav .eq. 0) then
              ifnext = 3
            else
              ifnext = 2
            end if
          else
            ifnext = 2
          end if
CC          go print solutions.
          if (ifmode .eq. 0) then
            call ROUTA
          else
            call ROUTB
          end if
          go to 10                                                      090498
        else if (ifnext .eq. 15) then
CC          see what to plot.
          ifnext = 16
          call RPLTA
          go to 20
        else if (ifnext .eq. 16) then
CC          go read, average, etc. quantity to be plotted
          ifnext = 17
          if (ifmode .eq. 0) then
            call RPLTB
          else
            call RPLTJ
          end if
          go to 20
        else if (ifnext .eq. 17) then
CC          default lables, get special symbols, isopleths, legends.
          ifnext = 18
          kfnxt = 0
          call RPLTC
          go to 20
        else if (ifnext .eq. 18) then
CC          get plot details, orientation, etc.
          ifnext = 19
          if (ifgrph .eq. 2) then
            call RPLTD
          end if
          go to 20
        else if (ifnext .eq. 19) then
CC          get remaining plot details.
          ifnext = 20
          if (ifgrph .eq. 2) then
            call RPLTE
          end if
          go to 20
        else if (ifnext .eq. 20) then
CC          graphics - determine plot scale, inches, etc., plot axes,
CC          label axes, plot grid coordinate points, plot north arrow.
          ifnext = 21
          call RPLTF
          go to 20
        else if (ifnext .eq. 21) then
  100     jpoptn = jpoptn + 1
CC-DEBUGS
CC>      if (ifdbug .gt. 0.and.ifdbug .lt. 7) then                      020797
CC>        WRITE (6,9001) jpoptn,npoptn,ipoptn,nxyfnc,nxpnts,
CC>  *                    nypnts,ioffst
CC>        pause 'Enter to continue. > '
CC>      end if
CC-DEBUGE
          if (jpoptn .gt. npoptn) then
            ifnext = 15
            kfnxt = 0
            go to 20
          else
            if (IABS(ipoptn(jpoptn)) .eq. 0) go to 100
            if (IABS(ipoptn(jpoptn)) .gt. 2) then
              if (IABS(ipoptn(jpoptn)) .le. 4) then
CC                contours or isopleths.
                if (ipoptn(jpoptn) .gt. 0) then
CC                  contours of terrain.
                  joffs1 = ioffst(1)
                  icontr = 1
                else
CC                  isopleths of dispersion.
                  joffs1 = ioffst(2)
                  icontr = 2
                end if
                go to 110
              end if
CC              if special symbols.
              if (IABS(ipoptn(jpoptn)) .eq. 5) go to 120
              ifnext = 15
              kfnxt = 0
              go to 20
            end if
            if (IABS(ipoptn(jpoptn)) .gt. 1) then
              if (ipoptn(jpoptn) .gt. 0) then
CC                maximum terrain line.
                joffs1 = ioffst(1)
                icontr = 1
              else
CC                maximum dispersion line.
                joffs1 = ioffst(2)
                icontr = 2
              end if
            end if
CC            go plot y as a function of x or line of maximum curve.
            ifnext = 21
            call RPLTG
            go to 20
  110       ifnext = 21
CC            go plot contours.
            call RPLTH
            go to 20
  120       ifnext = 21
CC            go plot special symbols.
            call RPLTI
            go to 20
          end if
        else
          ifnext = 3
          go to 20
        end if
      end if
      if (iotfil .gt. 0) then
CCOMP1S
c       call ntime(kyr,kmo,kdy,khr,kmn)
CCOMP1E
CCPCITS
        call GETTIM(khr,kmn,kms,idm2)
        call GETDAT(idm1,idm2,kdy)
CCPCITE
        xtimee = kdy * 24.0 * 3600.0 + khr * 3600.0 + kmn * 60.0 + kms
        d = xtimee - xtimes
        khr = d / 3600.0
        d = d - khr * 3600.0
        dum = d / 60.0
        WRITE (iotfil,9003) khr,dum
        WRITE (iotfil,9002)
      end if
CC
      iferrs = IOSCN(999,mdumy,idumy,idumy,idumy,idumy,idumy,0)
CCPCITS
      call STBRK(isvbk)
CCPCITE
CC      delete scratch solution file.
      if (ifstch .ne. 0.and.mognam .eq. msch1)
     *    CLOSE (unit=iogfil,status='delete',err=130)
CC      delete work file.
  130 if (iwkfil .gt. 0) CLOSE (unit=iwkfil,status='delete',err=140)
CC      delete scratch plot file.
  140 if (ifstcp .ne. 0) CLOSE (unit=iscfil,status='delete',err=150)
CC      delete scratch data save file.
  150 if (ifstco .ne. 0.and.msvnam .eq. msch2)
     *    CLOSE (unit=isvfil,status='delete',err=160)
  160 stop'end OBODM'
CC
CC-DEBUGS
 9000 format (/' OBODM - iferrs,ifnext,ifmode=',3i5/
     *' Enter - 1 then Enter to terminate or slash (/) then Enter to con
     *tinue. > ')
 9001 format (/' OBODM - jpoptn,npoptn=',2i4/' ipoptn=',10i4/
     *' nxyfnc,nxpnts,nypnts=',3i4/' ioffst=',2i4)
CC-DEBUGE
 9002 format (/' *** end of OBODM ***')
 9003 format (/' Elapsed run time = ',i5,' hours, ',f5.2,' minutes.')
      end
      BLOCK DATA
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - BLOCK DATA Version 01.1                     ---
CC--- Subroutine description - initialize OBODM common variables.   ---
CC---------------------------------------------------------------------
CC
CC**********************************************************************
CC
      include 'OCNTRL.INC'
      include 'ONAMES.INC'
      include 'OCDSPM.INC'
      include 'OCALCS.INC'
      include 'OWNTRL.INC'
CC
CC**********************************************************************
CC
CC      Update these parameters each time the program is updated, moddat
CC      is the update date and ivrmod is the sub version number, where
CC      xx.x.ivrmod gives the current version, xx.x is from versod.
      data moddat/'06/06/07'/,ivrmod/24/                                060607
CC        general control parameters
CCOMP1S
c     data isystm/1/
CCOMP1E
CCPCITS
      data isystm/2/
CCPCITE
      data idbfil/0/,iwkfil/0/,igtfil/0/,isvfil/0/,iotfil/0/,iogfil/0/,
     *     inhfil/0/,ishfil/0/,iplfil/0/,iscfil/0/,ifdfil/0/,itrfil/0/,
     *    impfil/0/,ibcfil/0/,ierfil/0/,isgfil/0/,llines/80/,linetl/60/,
     *     ipages/0/,ibcrec/0/,itable/0/,iferrs/0/,ifnext/0/,nochar/0/, 020797
     *     ifdbug/0/,rmissd/-1.0e20/,nmxcor/20000/,nmxxyz/100/,
     *     nmxsor/50/,nmxdrp/50/,nmxgrp/50/,isgrec/0/,imxfil/0/         072497
CC           the following 6 parameters must be ordered as shown in
CC           common.  if the dimension of any of these parameters is
CC           changed, you must also change code in sub. RINPA
CC           related to these parameters.  information for these
CC           parameters is read from the screen menu template data
CC           base file, which is generated by program OBODMenu.for.
      data nmxscn/0/,numscs/1,19*0/,numsce/20*0/,nrecsc/100*0/,
     *     nrecda/100*0/,nsubsc/100*0/,ibackt/1/,iforet/7/,ibackd/4/,
     *     ifored/14/,nrecdt/100*0/
CCPCITS
      data nkeybd/1,13,0,59,0,60,0,61,0,62,0,63,0,64,0,65,0,66,0,67,
     *            0,68,0,73,0,81,0,72,0,80,0,15,1,09,0,75,0,77,0,83,
     *            0,82,1,11,1,8,0,71,0,79,1,27,0,119,0,117/
CCPCITE
CCOMP1S
c     data nkeybd/0,13,0,112,0,113,0,114,0,115,0,116,0,117,0,118,0,119,
c    *            0,120,0,121,0,86,0,85,0,68,0,67,0,65,0,66,0,9,0,80,
c    *            0,81,0,26,1,11,1,8,0,71,0,79,1,27,0,119,0,117/
CCOMP1E
CC
      data ifgrph/0/,iftran/0/,ifrdwt/6*0/,istrec/0/,ilbstr/5*0/,
     *     ngroup/0/,nlbsch/5*0/,nsogrp/50*0/,isogrp/100*0/,nchtrs/0/,  072497
     *     rmissc/-1.0e21/,iswopt/38*0/,verson/0.0/,ntothr/0/,iptrec/0/,
     *     navhrs/6*0/,ifsean/0/,ifanul/0/,iptpnt/0/,iogrec/0/,
     *     versod/1.3/,ifprlp/0/,nformt/0/,nforms/0/,iogpnt/0/,         072497
     *     iflstr/0/,iflend/0/,nxtran/0/,nytran/0/,ircbuf/128*0/,
     *     ifnewp/0/,ilinem/10,19*0/,itabsm/31,199*0/,ispacm/36,199*0/,
     *     iferrm/200*0/,irnyrs/0/,irnmon/0/,irndys/0/,irnhrs/0/,
     *     irnjdy/0/,ibpage/1/,idpage/0/,ispage/0/,irdcol/0/,irdrow/0/,
     *     iptbuf/128*0/,ifbtch/0/,ifrsav/0/,ifsave/0/,ifstch/0/,
     *     nmxcol/80/,nmxrow/25/,nmxlin/20/,nmxfld/10/,iflrjs/0/,
     *     nbtchc/0/,ifrddm/1/,ifstcp/0/,ifstco/0/,ifccok/0/,
     *     ifctlc/0/,ifhiof/0/,ifmode/0/,ifcnvt/0/
      data numbtn/0/,icolmx/0/,irowmx/0/,imxpix/0/,imypix/0/,isvmod/0/,
     *     ifmous/0/,imxscl/8/,imyscl/8/,imsech/4/,imsecl/0/,locxms/0/,
     *     locyms/0/,isvbrd/0/,icmsta/8*0/,icmnum/7/,icmcol/0/,
     *     icmbeg/6,12,18,51,57,63,69,0/,icmlen/7*4,0/,icmrow/24/,
     *     icmmap/2,4,1,12,11,0,3,0/,icmkey/50,35,16,49,48,18,19,0/
CC
CC        names and labels
      data mtitle/50*' '/,mdbnam/'OBODMENU.OBD'/,mgtnam/' '/,
     *     mwknam/'OBODWORK.OBD'/,mshnam/' '/,mplnam/' '/,mnhnam/' '/,
     *     msvnam/' '/,motnam/' '/,mognam/' '/,mtrnam/' '/,
     *     mapnam/' '/,mfdnam/'OBODFUEL.OBD'/,mbcnam/' '/,
     *     mernam/'OBODMERR.OBD'/,msgnam/' '/,mxynam/' '/,
     *     malpha/'A','B','C','D','E','F','G','H','I','J','K','L','M',
     *            'N','O','P','Q','R','S','T','U','V','W','X','Y','Z',
     *            'a','b','c','d','e','f','g','h','i','j','k','l','m',
     *            'n','o','p','q','r','s','t','u','v','w','x','y','z'/,
     *     mnpbuf/80*' '/,mpolnt/36*' '/,mspeci/36*' '/,
     *     mcmlab/'Main','Help','Quit','Next','Back','Entr','Rtrn',
     *            ' '/
      data modelt/'Dosage                    ',
     *            'Concentration             ',
     *            'Time-Average Concentration',
     *            'Gravitational Deposition  ',
     *            'Time-Avg. Con.            ',
     *            'Deposition                ',
     *            'Grav. Dep.                '/
      data munitm/'Grams     ',
     *            'Kilograms ',
     *            'Pounds    ',
     *            'Ounces    ',
     *            'Tons      ',
     *            'Milligrams',
     *            'Micrograms',
     *            'Particles '/
      data munitl/'Meters     ',
     *            'Kilometers ',
     *            'Feet       ',
     *            'Yards      ',
     *            'Miles      ',
     *            'Centimeters',
     *            'Millimeters',
     *            'Micrometers',
     *            'Inches     '/
      data munita/'Square ',
     *            'Acre   ',
     *            'Hectare'/
      data munitg/'Degrees',
     *            'Radians'/
      data munitv/'Cubic             ',
     *            'Liter             ',
     *            'Parts per million ',
     *            'Parts per billion ',                                 020797
     *            'Parts per trillion'/
      data munitt/'Seconds',
     *            'Minutes',
     *            'Hours  '/
      data mdirec/36*' '/,mformt/151*' '/,mblnk1/' '/,
     *     msoids/1800*' '/,mscrch/300*' '/,mforms/151*' '/,            072497
     *     mlablx/60*' '/,mlably/60*' '/,mlngnd/360*' '/,
     *     mxform/' '/,myform/' '/,mzform/' '/
      data mcolor/'BLACK ',
     *            'BLUE  ',
     *            'GREEN ',
     *            'CYAN  ',
     *            'RED   ',
     *            'MAGENT',
     *            'YELLOW',
     *            'DKWHIT',
     *            'GRAY  ',
     *            'LTBLUE',
     *            'LTGREN',
     *            'LTCYAN',
     *            'LTRED ',
     *            'LTMAGE',
     *            'LTYELL',
     *            'LTWHIT'/
      data mscrnm/2000*' '/,mhrlab/56*' '/
CC        conversion constants
CC        pi/180.0,        180.0/pi,        sqrt(2*pi),
      data dtorad/0.01745329/,radtod/57.2957795/,sqr2pi/2.5066283/,
CC        1.0/sqrt(2*pi),    1.0/sqrt(2),      1.0/(2*pi)
     *     sq2pii/0.3989423/,sqrt2i/0.70710678/,twopii/0.159154943/,
CC        0.6/4.3,
     *     constt/1.0,60.0,3600.0/,
     *     constl/1.0,1.0e3,3.048e-1,1.016e-1,1.609344e3,1.0e-2,1.0e-3,
     *            1.0e-6,2.54e-2,0.514444/,
     *     consta/1.0,1.0e6,92904e-2,8.3631e-1,2.590007e6,1.0e-4,       041806
     *            1.0e-6,1.0e-12,6.45e-4,4.0468725e3,1.0e4/,            041806
     *     constm/1.0,1.0e3,453.5924,28.34952,9.071846e5,1.0e-3,1.0e-6,
     *            1.0/,
     *     constv/1.0,1.0e9,2.8317e-2,7.6456e-1,4.168228E9,1.0e-6,      041806
     *            1.0e-9,1.0e-18,1.6e-5,1.0e-3,1.0,1.0,1.0/,            041806
     *     constp/1.0,1.0e-2,3.38639e1,1.333224/,
     *     consth/1.0,1.0e3,2.519968e2,2.388459e-1,2.388459e-8/,
     *     constc/0.0,273.15,       32.0,     491.69,
     *            1.0,   1.0,5.555556e-1,5.555556e-1/,
     *     constg/1.0,5.72957795e1/,
     *     gravty/9.80665/,spheat/0.24/
CC        particle related parameters
      data nvsprt/0/,ifgpwn/0/,ifgend/0/,dcutof/2.0/,betale/-1.0e20/,
     *     decays/-1.0e20/,densty/1.0/,whtmol/18.015/,drpmmd/-1.0e20/,
     *     drpsig/-1.0e20/,drpupr/50*-1.0e20/,drplwr/50*-1.0e20/,
     *     pctmat/50*-1.0e20/,vspart/50*-1.0e21/,gammad/50*-1.0e21/
CC        meteorological related parameters
      data ifhrly/0/,iceunt/0/,ispunt/0/,jspunt/0/,issunt/0/,jssunt/0/,
     *     idsunt/0/,jdsunt/0/,isxunt/0/,isyunt/0/,iszunt/0/,istunt/0/,
     *     ihmunt/0/,ivgunt/0/,jvgunt/0/,irount/0/,itpunt/0/,iprunt/0/,
     *     ihrfmt/0/,ischmd/0/,istabl/0/,ifhsly/0/,insrec/0/,ihsfmt/0/,
     *     ivarmt/25*0/,ischrs/0/,iscdct/0/,nmxdys/366/,inhrec/0/,
     *     ivarso/4*0/,ifinit/-1/
      data refspd/-1.0e21/,pwrlaw/-1.0e21/,stblty/0.0/,
     *     wnddir/-1.0e21/,sigmap/-1.0e21/,sigmep/-1.0e21/,
     *     trbixr/-1.0e21/,airtmp/-1.0e21/,airprs/-1.0e21/,
     *     dphidz/-1.0e21/,hmdpth/-1.0e21/,spdshr/-1.0e21/,
     *     dirshr/-1.0e20/,radinx/-1.0e20/,tauosa/-1.0e20/,
     *     roughl/-1.0e20/,airden/-1.0e20/,airmol/28.9644/,airhum/50./,
     *     wndspd/-1.0e20/,cldcov/0.0/,cldhgt/9999.0/,refhgt/10.0/,
     *     alphad/1.0/,betadf/1.0/,spshrc/-1.0e20/,gammac/0.6/,
     *     gammai/0.64/,nmxhrs/8784/,airvsc/-1.0e20/,convrt/60*0.0/
      data vllimt/1900.0,1.0,1.0,0.0,1.0,1.0,0.0,0.0,1.0,1.0,1.0,-60.0, 020797
     *            600.0,-5.0,1.0,0.0,-45.0,1.0,-2.0,2.5,0.0,0.0,0.0,
     *            0.0,0.0/
     *     vulimt/2099.0,12.0,31.0,2300.0,366.0,50.0,5.0,360.0,80.0,
     *            50.0,106.4,60.0,1100.0,5.0,20000.,20.0,45.0,6.0,4.0,
     *            3600.0,100.0,100.0,20000.,8.0,20000./                 
CC        time/date related parameters
      data iniyrs/0/,inimon/0/,inidys/0/,inihrs/0/,inimin/0/,injdys/0/,
     *     ifdstm/0/,zondif/-1.0e20/,iscyrs/-1/,iscdys/366*0/,itmunt/0/,020797
     *     iseasn/0/,nseasn/4*0/,timavg/600.0/,timinv/-1.0e21/,
     *     iofhrs/1/,iquart/0/,nquart/4*0/
CC        terrain related parameters
      data geolon/-1.0e20/,geolat/-1.0e20/
CC        calculated temporary parameters
      data kssndx/0/,nvlprt/0/,ngrpct/0/,zpsave/0.0/,ngrplt/0/,
     *     vertcl/0.0/,timtox/0.0/,sigzax/0.0/,dekayd/0.0/,wspmin/0.0/,
     *     tcosth/0.0/,tsinth/0.0/,xtemp1/0.0/,ytemp1/0.0/,xpdist/0.0/,
     *     ypdist/0.0/,xxdist/0.0/,xydist/0.0/,xzdist/0.0/,zbotly/0.0/,
     *     ztoply/0.0/,delthp/0.0/,timtau/0.0/,sigatr/0.0/,sigepr/0.0/,
     *     sigapr/0.0/,factrs/0.0/,sigyax/0.0/,sigxax/0.0/,sigxop/0.0/,
     *     sigyop/0.0/,sigzop/0.0/,sigyta/0.0/,alongw/0.0/,tautmo/600./,
     *     zpdist/0.0/,timtas/0.0/
      data alatrl/0.0/,vertgv/0.0/,pekdos/0.0/,
     *     pekcon/0.0/,pconav/0.0/,pdepgr/0.0/,hightl/0.0/,trbixq/0.0/,
     *     sigxls/0.0/,sigyls/0.0/,sigzls/0.0/,xcord1/0.0/,ycord1/0.0/,
     *     clddst/0.0/,qkemss/0.0/,cldris/0.0/,cldfct/0.0/,ifmxpc/0/,
     *     ifmxpi/0/,trbixt/0.0/,trbixp/0.0/,sigmaq/0.0/,sigmeq/0.0/
CC        source related parameters.
      data nsourc/-1/,nlsorc/0/,isunit/0/,ifunit/0/,ipsunt/0/,ipvunt/0/,
     *     jpvunt/0/,idunit/0/,jdunit/0/,idcunt/0/,iduhet/0/,jduhet/0/,
     *     idubrn/0/,jdubrn/0/,heatdf/-1.0e20/,burndf/-1.0e20/,
     *     hgtmax/0.0/,spmass/0.0/,qtfuel/0.0/,qfract/0.0/,isrect/50*0/,072497
     *     idrect/50*0/,iqunit/50*0/,jqunit/50*0/,kqunit/50*0/,         072497
     *     isotyp/50*0/,qemiss/50*0.0/,taucld/50*0.0/,relhgt/50*0.0/,   072497
     *     clddxs/50*0.0/,clddys/50*0.0/,cldlng/50*0.0/,cldsht/50*0.0/, 072497
     *     cldang/50*0.0/,xsmlry/50*0.0/,xsmlrz/50*0.0/,xlrgry/50*0.0/, 072497
     *     xlrgrz/50*0.0/,hetcnt/50*0.0/,brnrat/50*0.0/,clddxe/50*0.0/, 072497
     *     clddye/50*0.0/,relhte/50*0.0/,cldmax/50*0.0/,gammat/50*0.0/, 072497
     *     alphas/50*0.0/,betass/50*0.0/,ifcalc/0/,cldmxd/50*0.0/,      072497
     *     isofrq/1200*1/,ismunt/50*0/,clddzs/50*0.0/,clddze/50*0.0/,   111098
     *     clddpt/50*0.0/,calhgt/50*0.0/                                072497
CC        receptor related parameters
      data igrect/0/,igunit/0/,nxpnts/0/,nypnts/0/,nxypnt/0/,ifgeng/0/,
     *     xcoord/100*-1.0e20/,ycoord/100*-1.0e20/,igangu/0/,ngrdpt/0/,
     *    zcoord/10000*-1.0e20/,xdscrt/100*-1.0e20/,ydscrt/100*-1.0e20/,
     *     zdscrt/100*-1.0e20/,grdang/-1.0e20/,xorgin/-1.0e20/,
     *     yorgin/-1.0e20/,ascale/1.0/,bscale/0.0/,ntotpt/0/,ntotal/0/,
     *     jjjoff/4*0/,gxstrt/0.0/gxincr/0.0/,gxendp/0.0/,gystrt/0.0/,
     *     gyincr/0.0/,gyendp/0.0/,izunit/0/
CC        calculation grid
      data calcsa/20000*0.0/,xteran/100*0.0/,yteran/100*0.0/
CC          graphics parameters
      data ioffst/2*-1/,nmxspl/20/,nmxctr/20/,nmxfnc/100/,icontr/0/,
     *     ncontr/2*9999/,nxyfnc/0/,joffs1/0/,contrs/40*0.0/,
     *     iswplt/-1,18*0,-1,10*0/,xsmins,ysmins/2*0.0/,
     *     xnchpl,ynchpl/2*0.0/,sclxes,sclyes/2*0.0/,icolor,lcolor/2*0/,
     *     rastrh,rastrv/2*0.0/,ibaudr/1200/,nplots/0/,ichars/4/,
     *     pxmaxs,pxmins,pymaxs,pymins/-1.0e20,1.0e20,-1.0e20,1.0e20/,
     *     xlmrgn,ybmrgn,xrmrgn,ytmrgn/2.0,3.0,1.0,1.0/,
     *     xincax,yincax/2*0.0/,xinchs,yinchs/2*0.0/,
     *     xplpnt,yplpnt/2*0.0/,htpchr/0.05/,htnchr/0.075/,
     *     htlchr/0.075/,splftm,spbotm/2*0.0/,xrmrpg,ytmrpg/2*0.0/,
     *     irxndx,iryndx,jrxndx,jryndx/4*0/,zmincs,zminds/4*0.0/,
     *     lswxax,lswyax/2*0/,ndgxes,ndgyes/2*0/,
     *     nincxs,nincys/2*0/,cscale/1.0/,dscale/0.0/,jsogrp/100*0.0/   072497
      data nchtcr,nchxcr,nchycr,nchlcr/4*0/,csmaxs,csmins/4*0.0/,
     *     xymins/0.0/,sclxys/0.0/,spmins/0.0/,spfrct/0.8/,
     *     ipoptn/10*0/,npoptn,jpoptn,ifhcpy/3*0/,
     *     ncurvs/0/,nctlbl/0/,ixlpnt/120*0/,iylpnt/120*0/,
     *     ialpnt/120*0/,spclxs/20*0.0/,spclys/20*0.0/,
     *     spclht/20*0.0/,spclan/20*0.0/,ispclc/20*0/,ispclr/20*0/,
     *     icnclr/40*0/,icndsh/40*0/,ifdash/0/,nspcls/0/,
     *     functx/100*0.0/,functy/100*0.0/,ixlpts/200*0/,iylpts/200*0/,
     *     ifbndx/0/,xbndxs/0.0/,ybndxs/0.0/,nbxpss/0/,xbxpsl/100*0.0/,
     *     xbxpel/100*0.0/,ybxpsl/100*0.0/,ybxpel/100*0.0/,hcminc/0.0/,
     *     ifgpol/0/,ifspol/0/,ifpass/0/,ilstxs/0/,ilstys/0/,ipmode/0/,
     *     isppol/20*0/,imodes/3/,lmodes/3/,ncolmn/78/,iboard/0/,
     *     pixelx/0.0/,pixely/0.0/,ifswxy/0/,ifcolr/0/,topmrg,botmrg,
     *     xftmrg,rgtmrg/4*0.0/,ifprno/0/,paperw/8.5/,paperl/11.0/
      data nhravg/0/,nhrlab/0/,ifchlb/0/,ifavrg/0/,iftpol/0/
CC
      end
