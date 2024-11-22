      Function IPACK(nsubs,valux,valuy,valuz,valua,valub,valuc,nmxps,
     *               nvals,numsc,mscrn,iline,itabs,ispac,iferr,iflgs,
     *               jflgs)
CC
CC---------------------------------------------------------------------
CC--- Author organization- H. E. Cramer Co., Inc.                   ---
CC--- Subroutine code - IPACK Version 01.2                          ---
CC--- Subroutine description - packs, sorts and saves x,y coordinate---
CC---                          and x,y,z discrete point data.       ---
CC---------------------------------------------------------------------
CC
CC      nsubs  - number of data screens for screen menu numsc.
CC      valux  - data value array stored into or taken from (iflgs>=1).
CC      valuy  - data value array stored into or taken from (iflgs>=3).
CC      valuz  - data value array stored into or taken from (iflgs>=3).
CC      valua  - data value array stored into or taken from (iflgs>=4).
CC      valub  - data value array stored into or taken from (iflgs>=4).
CC      valuc  - data value array stored into or taken from (iflgs>=5).
CC      nmxps  - maximum number of values possible.
CC      nvals  - number of values returned in or taken from arrays.
CC      numsc  - screen menu number.
CC      mscrn  - scratch screen array.
CC      iline  - array of screen line numbers containing data.
CC      itabs  - array of start columns for each field containing data.
CC      ispac  - array of number of columns for each data field.
CC      iferr  - array of error indicators for each data field.
CC      iflgs  - flag indicating type of data.  If iflgs is negative,
CC               data are not read from the work file, but are taken
CC               from current common and sorted only, and not written
CC               to the work file.
CC               1 - x axis coordinates, valux = xcoord.
CC               2 - y axis coordinates, valux = ycoord.
CC               3 - x,y,z discrete coordinates, valux = xdscrt,
CC                   valuy = ydscrt and valuz = zdscrt.
CC               4 - particle data valux = drpupr, valuy = drplwr,
CC                   valuz = pctmat, valua = vspart, valub = gammad.
CC               5 - special plot symbol data, valux = spclxs, valuy =
CC                   spclys, valuz = spclht, valua = spclan, valub =
CC                   spcclc, valuc = spcpol.
CC      ls  - read/write work file flag.
CC               0 - read, pack, sort and write work file.
CC               1 - write work file only, initialize.
CC
CC      returns - IPACK -
CC                 0 - no errors.
CC                 1 - error in data.
CC                 2 - warning data may be erroneous.
CC                 3 - warning too much data entered.
CC
CC**********************************************************************
CC
      integer*2 nlns,nvpl,n,nln,i,nsc,k
CC
      character*1 mscrn
CC
      dimension mscrn(80,25),iline(20),itabs(20,10),ispac(20,10),
     *          iferr(20,10),valux(100),valuy(100),valuz(100),
     *          valua(100),valub(100),valuc(100),kyfld(6),xdumy(6)
CC
      include 'OCNTRL.INC'
      include 'ONAMES.INC'
      include 'OWNTRL.INC'
CC
CC********************************************************************
CC
      iwrn = 0
      n = nmxps
      if (iflgs .le. 2) then
CC        set indices for x and y axis coordinates.
        nlns = 14
        nvpl = 5
      else if (iflgs .eq. 3) then
CC        set indices for discrete x,y,z coordinates.
        nlns = 15
        nvpl = 6
      else if (iflgs .eq. 4) then
CC        set indices for particulate data.
        nlns = 10
        nvpl = 5
      else if (iflgs .eq. 5) then
CC        set indices for special plot symbols data.
        nlns = 10
        nvpl = 6
      end if
      IPACK = 0
      if (jflgs .eq. 0) then
        if (iflgs .lt. 0) go to 60
CC        read data values from screen menus.
        nsub = 0
        n = 0
CC        loop over data sets.
   10   nsub = nsub + 1
        if (nsub .le. nsubs) then
CC          read screen menu and screen data.
          numf = IRDSC(numsc,nsub,mscrn,iline,itabs,ispac,iferr,1)
          j = JINIT(iferr,nmxlin*nmxfld,0,0)
CC          read data values.
          do 50 nvl=1,nlns
            nln = iline(nvl)
            do 20 i=1,nvpl
              nsc = itabs(nvl,i)
              nch = ispac(nvl,i)
              if (iflgs .lt. 5.or.i .lt. 6) then
CC                get value from data field.
                l = IRDBF(mscrn(nsc,nln),nch,xdumy(i))
                if (l .gt. 0) then
                  iferr(nvl,i) = 1
                  xdumy(i) = rmissd
                else if (l .lt. 0) then
                  xdumy(i) = rmissd
                  if (l .eq. -2) then
                    xdumy(i) = rmissc
                    if (iflgs .ne. 4.or.i .le. 3) then
                      iferr(nvl,i) = 1
                    end if
                  end if
                end if
              else
                if (mscrn(nsc,nln) .ne. mblnk1) then
                  if (IFCHR(mscrn(nsc,nln),18) .ne. 0) then
                    xdumy(i) = 0.0
                  else if (IFCHR(mscrn(nsc,nln),16) .ne. 0) then
                    xdumy(i) = 1.0
                  else
                    iferr(nvl,i) = 1
                  end if
                else
                  xdumy(i) = 0.0
                end if
              end if
   20       continue
            if (iflgs .le. 2) then
CC              x or y axis.
              do 30 i=1,nvpl
                if (xdumy(i) .gt. rmissd) then
                  n = n + 1
                  if (n .le. nmxps) then                  
                    valux(n) = xdumy(i)
                  else
                    iwrn = 1
                  end if
                end if
   30         continue
            else if (iflgs .eq. 3) then
              do 40 i=1,4,3
                if (xdumy(i) .gt. rmissd) then
                  n = n + 1
                  if (n .le. nmxps) then            
                    valux(n) = xdumy(i)
                    valuy(n) = xdumy(i+1)
                    valuz(n) = xdumy(i+2)
                    if (valuy(n) .le. rmissd) then
                      iferr(nvl,i+1) = 1
                    end if
                    if (valuz(n) .lt. 0.0) then
                      valuz(n) = 0.0
                    end if
                  else
                    iwrn = 1
                  end if
                end if
   40         continue
            else if (iflgs .eq. 4) then
              if (xdumy(1) .gt. 0.0) then
                n = n + 1
                if (n .le. nmxps) then
CC                  particle size upper bound.
                  valux(n) = xdumy(1)
CC                  particle size lower bound.
                  valuy(n) = xdumy(2)
CC                  if not set, make equal to upper bound.
                  if (valuy(n) .le. 0.0) valuy(n) = valux(n)
CC                  mass fraction.
                  valuz(n) = xdumy(3)
CC                  if not set, indicate error.
                  if (valuz(n) .le. 0.0) then
                    valuz(n) = rmissd
                    iferr(nvl,3) = 1
                  end if
CC                  particle settling velocity.
                  valua(n) = xdumy(4)
CC                  if not set, indicate calculated.
                  if (valua(n) .le. 0.0) then
                    valua(n) = rmissc
                  end if
CC                  particle reflection coefficient.
                  valub(n) = xdumy(5)
CC                  if not set, indicate calculated.
                  if (valub(n) .lt. 0.0) then
                    valub(n) = rmissc
                  else if (valub(n) .gt. 1.0) then
                    iferr(nvl,5) = 1
                  end if
                else
                  iwrn = 1
                end if
              end if
            else if (iflgs .eq. 5) then
              if (xdumy(1) .gt. rmissd) then
                n = n + 1
                if (n .le. nmxps) then
CC                  special symbol x coord.
                  valux(n) = xdumy(1)
CC                  special symbol y coord.
                  valuy(n) = xdumy(2)
CC                  symbol height.
                  valuz(n) = AMAX1(xdumy(3),spmins)
                  if (valuz(n) .lt. 0.0.or.valuz(n) .gt. 10.0) then
                    iferr(nvl,3) = 1
                  end if
CC                  symbol angle.
                  valua(n) = xdumy(4)
                  if (valua(n) .lt. -360.0.or.valua(n) .gt. 360.0) then
                    iferr(nvl,4) = 1
                  end if
CC                  symbol code.
                  valub(n) = AMAX1(xdumy(5),1.0)
                  if (valub(n) .lt. 0.0.or.valub(n) .gt. 15) then
                    iferr(nvl,5) = 1
                  end if
CC                  rectangular/polar flag.
                  valuc(n) = xdumy(6)
                  if (valuc(n) .lt. 0.0.or.valuc(n) .gt. 2.0) then
                    iferr(nvl,6) = 1
                  end if
                else
                  iwrn = 1
                end if
              end if
            end if
   50     continue
          i = IWTSC(numsc,nsub,mscrn,iline,iferr)
          ner = IFWER(iferr,nmxlin*nmxfld)
          if (IPACK .eq. 0) then
            IPACK = ner
          else if (IPACK .eq. 2) then
            if (ner .ne. 0) IPACK = ner
          end if
          go to 10
        end if
CC      set number of points.
   60   nvals = MIN0(n,nmxps)
        if (nvals .gt. 0) then
          i = JINIT(kyfld,6,0,0)
          if (iflgs .le. 2) then
            kyfld(1) = 1
          else
            kyfld(1) = 1
            kyfld(2) = nmxps + 1
            kyfld(3) = 2 * nmxps + 1
            if (iflgs .ge. 4) then
              kyfld(4) = 3 * nmxps + 1
              kyfld(5) = 4 * nmxps + 1
            end if
            if (iflgs .ge. 5) then
              kyfld(6) = 5 * nmxps + 1
            end if
          end if
CC          sort into ascending order, keying on valux.
          if (iflgs .ne. 4) then
            i = 1
          else
            i = -1
          end if
          call ORDER(valux,1,nvals,kyfld,i,IABS(iflgs))
          if (iflgs .le. 2.and.iflgs .gt. 0) then
CC            check for duplicates.
            n = 1
            do 70 i=2,nvals
              if (IFEQU(valux(i),valux(n)) .ne. 0) then
                n = n + 1
                valux(n) = valux(i)
              end if
   70       continue
            nvals = n
          end if
        end if
      end if
      if (iflgs .lt. 0) go to 170
      if (IPACK .ne. 1) then
        nsub = 0
        n = 0
        i = JINIT(iferr,nmxlin*nmxfld,0,0)
        l = 0
CC        loop over data sets.
   80   nsub = nsub + 1
        if (nsub .le. nsubs) then
          iferrs = IRDSC(numsc,nsub,mscrn,iline,itabs,ispac,iferr,l)
CC          write data values back to screen data areas.
          do 160 nvl=1,nlns
            nln = iline(nvl)
            if (iflgs .le. 2) then
CC              x or y axis.
              do 90 i=1,nvpl
                nsc = itabs(nvl,i)
                nch = ispac(nvl,i)
                if (iferr(nvl,i) .eq. 0) then
                  n = n + 1
                  if (n .le. nvals) then
                    j = IWTBF(mscrn(nsc,nln),nch,valux(n),1,iflrjs)
                  else
                    call MBLK1(mscrn(nsc,nln),nch)
                  end if
                end if
   90         continue
            else if (iflgs .eq. 3) then
              do 110 i=1,4,3
                if (iferr(nvl,i) .eq. 0.and.iferr(nvl,i+1) .eq. 0.and.
     *            iferr(nvl,i+2) .eq. 0) then
                  n = n + 1
                  if (n .le. nvals) then
                    xdumy(1) = valux(n)
                    xdumy(2) = valuy(n)
                    xdumy(3) = valuz(n)
                  end if
                  do 100 j=i,i+2
                    nsc = itabs(nvl,j)
                    nch = ispac(nvl,j)
                    if (n .le. nvals) then
                      k = IWTBF(mscrn(nsc,nln),nch,xdumy(j-i+1),1,
     *                          iflrjs)
                    else
                      call MBLK1(mscrn(nsc,nln),nch)
                    end if
  100             continue
                end if
  110         continue
            else if (iflgs .eq. 4) then
              j = 0
              do 120 i=1,nvpl
                if (iferr(nvl,i) .eq. 1) j = 1
  120         continue
              if (j .eq. 0) then
                n = n + 1
                if (n .le. nvals) then
CC                  particle size upper bound.
                  xdumy(1) = valux(n)
CC                  particle size lower bound.
                  xdumy(2) = valuy(n)
CC                  mass fraction.
                  xdumy(3) = valuz(n)
CC                  particle settling velocity.
                  xdumy(4) = valua(n)
CC                  particle reflection coefficient.
                  xdumy(5) = valub(n)
                end if
                do 130 i=1,nvpl
                  nsc = itabs(nvl,i)
                  nch = ispac(nvl,i)
                  if (n .le. nvals) then
                    if (i .le. 2) then
                      j = 2
                    else if (i .eq. 4) then
                      j = 4
                    else
                      j = 7
                    end if
                    k = IWTBF(mscrn(nsc,nln),nch,xdumy(i),j,iflrjs)
                  else
                    call MBLK1(mscrn(nsc,nln),nch)
                  end if
  130           continue
              end if
            else if (iflgs .eq. 5) then
              j = 0
              do 140 i=1,nvpl
                if (iferr(nvl,i) .eq. 1) j = 1
  140         continue
              if (j .eq. 0) then
                n = n + 1
                if (n .le. nvals) then
CC                  symbol x coord.
                  xdumy(1) = valux(n)
CC                  symbol y coord.
                  xdumy(2) = valuy(n)
CC                  symbol height.
                  xdumy(3) = valuz(n)
CC                  symbol angle.
                  xdumy(4) = valua(n)
CC                  symbol code.
                  xdumy(5) = valub(n)
CC                  x,y are rect./polar.
                  xdumy(6) = valuc(n)
                end if
                do 150 i=1,nvpl
                  nsc = itabs(nvl,i)
                  nch = ispac(nvl,i)
                  if (n .le. nvals) then
                    if (i .lt. nvpl) then
                      if (i .le. 4) then
                        j = 1
                      else
                        j = 0
                      end if
                      k = IWTBF(mscrn(nsc,nln),nch,xdumy(i),j,iflrjs)
                    else
                      if (xdumy(i) .le. 0.0) then
                        mscrn(nsc,nln) = malpha(18)
                      else
                        mscrn(nsc,nln) = malpha(16)
                      end if
                    end if
                  else
                    call MBLK1(mscrn(nsc,nln),nch)
                  end if
  150           continue
              end if
            end if
  160     continue
CC          write screen data back to work file.
          i = IWTSC(numsc,nsub,mscrn,iline,iferr)
          go to 80
        end if
      end if
  170 if (IPACK .le. 0.and.iwrn .ne. 0) IPACK = 3
      return
      end
