      program aercoare
c
c------------------------------------------------------------------------------
c     AERCOARE
c     VERSION 1.1 2013-04-18
c
c     overwater met data preprocessor for AERMOD
c     replaces AERMET given overwater meteorological data
c
c     Development History:
c
c     2013-04-18  (D13108)
c
c     change SFC file header to confirm to AERMOD/AERMET 12345
c
c     2012-10-01  (D12275)
c
c     Changed jwave= 2 so when either hwave or twave are missing
c                      well developed sea params are used instead
c                      of wave data. Previously treated twave and 
c                      hwave were treated individuallu resulting
c                      bogus (hwave/lwave) steep waves and large zo
c
c                      also moved hwave and twave calcs into AST
c                      subroutine so could use 1st estimate of U10
c                      when they are missing
c
c     2011-09-01 (D11244) First full Version of code released EPA as a Beta
c
c------------------------------------------------------------------------------
c     This program is free software; you can redistribute it and/or 
c     modify it under the terms of the GNU General Public License 
c     as published by the Free Software Foundation; either version 2 
c     of the License, or (at your option) any later version. 
c  
c     This program is distributed in the hope that it will be useful, 
c     but WITHOUT ANY WARRANTY; without even the implied warranty of 
c     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
c     GNU General Public License for more details. 
c------------------------------------------------------------------------------
c 
      implicit none
c
      character*80 fname, fdebug
      integer iargc, nargs
      logical ldebug
c
c retrieve the control file from the command line
c
      ldebug=.false.
      nargs = iargc()
      if(nargs.ne.1.and.nargs.ne.2) then
         fname ='aercoare.inp'
      elseif (nargs.eq.1) then
         call getarg(1,fname)
      else
         call getarg(1,fname)
         call getarg(2,fdebug)
         ldebug=.true.
      end if

      if (fname == "--version") then
         write(*,*) "AERCOARE VERSION 1.0 2012-10-01"
         stop
      endif
c
c process the control file & read header info from the overwater
c met file. Make sure the minimum number of variables are in the file
c inorder to calc overwater fluxes
c
      call get_control(fname,fdebug,ldebug)
c
c write header of SFC file
c
      call write_sfc_head
c
c call COARE modules looping thru observations
c
      call coare(ldebug)
c
      end program aercoare
c
c**********************************************************************
c**********************************************************************
c
      subroutine coare (ldebug)
c
c------------------------------------------------------------------------------
c     AERCOARE
c     VERSION 1.0 2011-MM-DD
c
c     Loops thru overwater meteorological data applying the COARE 3.0 
c     Algorithm
c
c     COARE Bulk Flux Algorithm version 3.0a. See Fairall et al.,2003: J.
c     Climate,16,571-591
c
c     Development History:
c     2011-MM-DD  ENVIRON International
c
c------------------------------------------------------------------------------
c
      implicit none
c
c  common variables ************************************************
c
c  COARE warm layer stuff
c    
      integer                    jamset,jday1
      real*8      qcol_ac,tau_ac,             fxp,tk_pwp,tau_old,
     &            ef_old,hf_old,rf_old,time_old,sst_old,xtime_old
      COMMON/warm/qcol_ac,tau_ac,jamset,jday1,fxp,tk_pwp,tau_old,
     &            ef_old,hf_old,rf_old,time_old,sst_old,xtime_old
c
c  mixing ht stuff
c
      logical                        lzimiss, lsmooth
      real*8         ziobs, zivenkl
      common /mixht/ ziobs, zivenkl, lzimiss, lsmooth
c
c  local variables ***********************************************
c
      character*80 filein,filemet,fileout,dum*1
      integer ireco, iargc, iceil, ncalm, nmiss, idate
      real*8 cc,rh,hwin,twin, ceil
      logical ldebug, eof, lmiss, lcalm
c
c initialize COARE optional warm layer variables and switches
c
      qcol_ac=0.
      tau_ac=0.
      fxp=0.5
      tk_pwp=19.0
      jamset=0
      jday1=1
      tau_old=0.06 
      ef_old=0
      hf_old=0
      rf_old=0
      time_old=0
      sst_old=0
c
c loop through data
c
      ireco=0
      ncalm = 0
      nmiss = 0
      lzimiss = .false.          ! for zi smoothing
      zivenkl = -99.             ! for zi smoothing
c
      eof = .false.
c
      if(ldebug) write(99,'(2a)') ' ireco xtime hf ef sst tau Wbar rf ',
     &'dter dt_wrm tk_pwp tkt*1000. Wg usr tsr qsr*1000 xmol zo zot zoq'

      do while (.not.eof)
         ireco=ireco+1              !count data records (hours for test data) 
c
c initialize output variables with missing data codes
c
         call set_missing
c
c  get obs variables
c
         call getobs (lmiss, lcalm, eof, ireco, idate)
c
         if(.not.eof) then
c         
            if(.not.lmiss.and..not.lcalm) then
c
c call COARE bulk flux routine *******
c                    
               call bulk_flux(ireco,ldebug)
c
c Process COARE output and add Mix hts etc
c
               call process_met
c
             end if
c
             if(lmiss) nmiss = nmiss + 1
             if(lmiss.and.ldebug) write(99,*)
     &          'hour with insufficient data at: ',idate
             if(lcalm) ncalm = ncalm + 1
             if(lcalm.and.ldebug) write(99,*) 'calm hour at   : ',idate
             if(lcalm.or.lmiss) lzimiss = .true.   ! for zi smoothing
c
c  write AERMOD met files
c
             call write_aermod (idate)
         end if
c     
      end do
c
      ireco = ireco - 1
c
      call missing_summary
c
      print *,' '
      print *,'AERCOARE processed ',ireco,' records'
      print *,'number of records with insufficient data: ',nmiss
      print *,'number of calm records                  : ',ncalm
      write(16,*) ' '
      write(16,*) ' '
      write(16,*) 'AERCOARE processed ',ireco,' records'
      write(16,*) 'number of records with insufficient data : ',nmiss
      write(16,*) 'number of calm records                   : ',ncalm
c
      print *,' '
      stop 'AERCOARE stopped normally'
c
      end subroutine coare
c
c**********************************************************************
c**********************************************************************
c
      subroutine missing_summary
c
c------------------------------------------------------------------------------
c     AERCOARE
c     VERSION 1.0 2012-10-01
c
c     write missing data summary
c
c     Development History:
c     2012-10-01  ENVIRON International
c
c------------------------------------------------------------------------------
c
c variable definitions
c
      integer, parameter :: nvmax = 24
      character*4 vnam(nvmax)
      logical lvar(nvmax)
      integer ivno (nvmax), nvar, vmiss(nvmax)
      real*8 vmin(nvmax),vmax(nvmax),vsca(nvmax)
      common /varin/ vnam, lvar, ivno, vmin, vmax, vsca, nvar, vmiss
c
c local variables
c
      integer i
c
      write(16,11)
   11 format(//,'Missing Data Summary by Variable (1)',/,
     &         ' Vname   No. Miss',/,
     &         ' ----- ----------')
c
      do i = 1, nvmax
         if(lvar(i)) write(16,12) vnam(i), vmiss(i)
   12    format(2x,a,1x,i10)
      end do
c
      write(16,13)
   13 format(/,'(1) - does not include whole missing hours caused',/,
     &   '      by non-sequential data.')
      return
c
      end subroutine missing_summary
c
c**********************************************************************
c**********************************************************************
c
      subroutine write_aermod (idate)
c
c------------------------------------------------------------------------------
c     AERCOARE
c     VERSION 1.0 2012-10-01
c
c     write out a AERMOD SFC and PFL records
c
c     Development History:
c     2012-10-01  ENVIRON International
c
c------------------------------------------------------------------------------
c
      implicit none
c
      integer       iccov,                            pcode
      real*8          hvf, wstar, ustar, gamma, zic, zim, xmol, znot, 
     & bowen, alb, wsref, wdref, zwsref, tref, ztref,        prec, relh, 
     &       press,        sigth, sigw
c
      common /aerout/ hvf, wstar, ustar, gamma, zic, zim, xmol, znot, 
     & bowen, alb, wsref, wdref, zwsref, tref, ztref, pcode, prec, relh, 
     &       press, iccov, sigth, sigw
c
      integer idate, iy, im, id, ih, iy2d, jul, julian, last
c
      real*8 badws, badwd, badtem, badst, badsw, trefc
c
      badws = 999.d0
      badwd = 999.d0
      badtem = 99.9d0
      badst =  99.d0
      badsw =  99.d0
c
c calc date variables
c
      call idate2ymdh(idate, iy, im, id, ih)
      if (iy .ge. 2000) then
         iy2d = iy - 2000
      else
         iy2d = iy - 1900
      end if
      jul = julian(iy, im, id)
c
c  SFC file
c
      write(3,11) iy2d, im, id, jul, ih, hvf, ustar, wstar, gamma,
     & zic, zim, xmol, znot, bowen, alb, wsref, wdref, zwsref, tref,
     & ztref, pcode, prec, relh, press, iccov 
c
   11 format(3(i2,1x),i3,1x,i2,1x,f6.1,1x,3(f6.3,1x),2(f6.1,1x),
     & f8.1,1x,f9.6,1x,2(f6.2,1x),f7.2,1x,4(f6.1,1x),i5,1x,f6.2,1x
     & 2(f6.0,1x),i5,1x,'NAD-OS ')
c
c  PFL file
c
       if(tref.lt.900.) then
          trefc = tref - 273.16       ! temp(degC) in PFL file
       else
          trefc = badtem
       end if
c
       if(ztref.lt.zwsref) then
          last = 0
          write(4,12) iy2d,im,id,ih,ztref,last,badwd,badws,trefc,
     &                badst,badsw
   12     format(4(i2,1x),f7.1,1x,i1,1x,f7.1,1x,3(f8.2,1x),f8.2)
          last = 1
          write(4,12) iy2d,im,id,ih,zwsref,last,wdref,wsref,badtem,
     &                sigth,sigw
       else if(ztref.eq.zwsref) then
          last = 1
          write(4,12) iy2d,im,id,ih,zwsref,last,wdref,wsref,trefc,
     &                sigth,sigw
       else
          last = 0
          write(4,12) iy2d,im,id,ih,zwsref,last,wdref,wsref,badtem,
     &                sigth,sigw
          last = 1
          write(4,12) iy2d,im,id,ih,ztref,last,badwd,badws,trefc,
     &                badst,badsw
       end if
c
      return
c      
      end subroutine write_aermod
c
c**********************************************************************
c**********************************************************************
c
      subroutine process_met
c
c------------------------------------------------------------------------------
c     AERCOARE
c     VERSION 1.0 2012-10-01
c
c     process met data from COARE for AERMOD input files
c     calc optional Venketram style mechanical mixing height
c     invoke limits on Monin Obukhov length and mixing height
c
c     Development History:
c     2012-10-01  ENVIRON International
c
c------------------------------------------------------------------------------
c
      implicit none
c
c
c default control variables
c
      real*8        xlatN, xlonW,        defzi, zimin, dlmin, wscalm, 
     &              dvptg, defzw, defzt, defzq, defdep
      integer                     tzone,                mixopt
      common /defs/ xlatN, xlonW, tzone, defzi, zimin, dlmin, wscalm,  
     &              dvptg, defzw, defzt, defzq, defdep, mixopt
c
      integer       iccov,                            pcode
      real*8          hvf, wstar, ustar, gamma, zic, zim, xmol, znot, 
     & bowen, alb, wsref, wdref, zwsref, tref, ztref,        prec, relh, 
     &       press,        sigth, sigw
c
      common /aerout/ hvf, wstar, ustar, gamma, zic, zim, xmol, znot, 
     & bowen, alb, wsref, wdref, zwsref, tref, ztref, pcode, prec, relh, 
     &       press, iccov, sigth, sigw
c
c  mixing ht stuff
c
      logical                        lzimiss, lsmooth
      real*8         ziobs, zivenkl
      common /mixht/ ziobs, zivenkl, lzimiss, lsmooth
c
c  COARE output
c
      real*8        usr,tsr,qsr,zo,zot,zoq,zL,rr,rt,rq,ri,
     &             dter,dqer,tkt,Du,Wg
      COMMON/ASLout/usr,tsr,qsr,zo,zot,zoq,zL,rr,rt,rq,ri,
     &             dter,dqer,tkt,Du,Wg
c
      real*8      hf,ef
      common/flux/hf,ef
c
c  local variables
c
      real*8 xmol1, xmol2, zivenk, vonk
c
      hvf = hf         ! sens heat
      znot = zo        ! wind speed zo
      alb = 0.055d0    ! COARE ocean daily average albedo
      vonk = 0.4d0     ! von karmans constant
c
c bowen ratio; follows same logic as in aermod_metcalc.f from
c mm5aermod
c
c Compute bowen ratio (hf/ef)
c
c * if sign of hf and ef is same -> compute, apply limit if appl.
c * if ef = 0
c   - hf > 0, B = 10.
c   - hf < 0, B = -1.0
c   - hf = 0, B = 0
c * if hf < 0, ef > 0, B = -1.0
c * if hf > 0, ef < 0, B = 1.0
c * limit maximimum positive value to 10.0
c * limit min 0 and positive value to 0.01
c
      if(hf.gt.0.d0.and.ef.lt.0.d0) then
         bowen = 1.d0
      elseif(hf.lt.0.d0.and.ef.gt.0.d0) then
         bowen = -1.d0
      elseif(ef.eq.0.d0) then
         if(hf.gt.0.d0) then
            bowen = 10.d0
         elseif(hf.lt.0.d0) then
            bowen = -1.d0
         else
            bowen = 0.d0
         end if
      elseif (hf.eq.0.d0) then
         bowen = 0.d0
      else
         bowen = hf/ef
      end if
c
      bowen = min(bowen,10.d0)
      if(bowen.ge.0.d0) bowen = max(0.01d0,bowen)
c
c  adjust min Monin Obukhov Length based on limit dlmin
c
      xmol1 = zwsref/zL
      if(abs(xmol1).lt.dlmin) then
         xmol2 = dsign(1.d0,xmol1)*dlmin
      else
         xmol2 = xmol1
      end if
c
c  adjust ustar to revised MO Length
c
      ustar = usr*(xmol2/xmol1)**(1.d0/3.d0)
c
c  as with AERMET limit max MO Length to 8888 for AERMOD
c
      if(xmol2.lt.-8888.d0) then
         xmol = -8888.d0               
      elseif (xmol2.gt.8888.d0) then
         xmol = 8888.d0
      else
         xmol = xmol2
      end if         
c
c overwater mixing height options
c              
      if(mixopt.eq.0) then                    ! use obs for both zim and zic
         if(ziobs.gt.0.d0) then
            zim = max(ziobs, zimin)
            if(xmol.lt.0.d0) then        
               zic = zim
            end if
         end if
c
      elseif (mixopt.eq.1) then               ! venketram zim, obs for zic
         zivenk = max(2300.d0*ustar**1.5d0, zimin)
         if (lsmooth) then
            call smthzi (zivenk, ustar, zim)  ! smooth if mixopt < 0
         else
            zim = zivenk
         end if
         if(xmol.lt.0.d0.and.ziobs.gt.0.d0) then        
             zic = max(ziobs, zimin)
         end if
c
      else                                    ! venketram for both zim & zic
         zivenk = max(2300.d0*ustar**1.5d0, zimin)
         if (lsmooth) then
            call smthzi (zivenk, ustar, zim)  ! smooth if mixopt < 0
         else
            zim = zivenk
         end if
         if(xmol.lt.0.d0) then        
            zic = zim
         end if
      end if
c
c wstar
c
      if(zic.gt.0.d0.and.xmol.lt.0.d0) then
         wstar = max(0.d0,-ustar**3.d0*zic/(vonk*xmol))**(1.d0/3.d0)
      end if
c
c VPTG set to missing if L > 0
c
      if(xmol.gt.0.d0) then
         gamma = -9.d0
      end if
c
      return
c
      end subroutine process_met
c
c**********************************************************************
c**********************************************************************
c
      subroutine smthzi( zim, ustar, zismth )
c
c------------------------------------------------------------------------------
c     AERCOARE
c     VERSION 1.0 2012-10-01
c
C     Computes a smoothed boundary layer height based on the
C     current hour's value of mechanical boundary layer height;
C     previous hour's smoothed boundary layer height; and
C     current hour's friction velocity
C
C     Assumptions: A missing mixing height forces the smoothing to
C                  restart with the next nonmissing mixing height;
C                  all hourly mechanical mixing heights are smoothed
c
c     Development History:
c     Original from AERMET Meteorological Preprocessor (V11069)
c     2012-10-01  ENVIRON International
c
c------------------------------------------------------------------------------
c
      implicit none
c
c  mixing ht stuff
c
      logical                        lzimiss, lsmooth
      real*8         ziobs, zivenkl
      common /mixht/ ziobs, zivenkl, lzimiss, lsmooth
c
C---- Local Variables
c
      real*8, parameter :: betatau = 2.0d0
      real*8    hnprev, ustar, zim, tau, xponen, zismth
      real*8    explim, deltat    
c
C---- Variable Initializations
c
      data deltat/3600.0d0/, explim/50.0d0/
c
      IF( (zivenkl .ge. 0.0d0) .and. (zim .gt. 0.0d0) .and.
     &    (ustar .gt. 0.d0) .and. (.not.lzimiss) ) then 

C------- The previous hour's smoothed PBL height, the current hour's
C        PBL height and USTAR are not missing and an hour has not been
C        skipped due to missing data (i.e., routine not called):
C        compute the current hour's smoothed PBL height

         tau   =  zivenkl / (betatau * ustar)
         xponen = deltat / tau
         if (xponen .gt. explim) then        ! avoid undeflow
            zismth = zim                                     
         ELSE                                                
            zismth = zivenkl*dexp(-xponen) + zim*(1.0d0 - dexp(-xponen))
         ENDIF                                                     

C------- Save the current values for the next hour's computations
         zivenkl = zismth
         lzimiss = .false.

      else
C------- One of the required parameters to smooth the mixing height
C        is missing, so the computation cannot be made
         zismth = zim
         zivenkl = zim

      ENDIF
c
      RETURN
c
      end subroutine smthzi
c
c**********************************************************************
c**********************************************************************
c
      subroutine getobs (lmiss,lcalm, eof, ireco, idate)
c
c------------------------------------------------------------------------------
c     AERCOARE
c     VERSION 1.0 2012-10-01
c
c     process a record of overwater meteorological data and
c     substitute defaults for some missing variables.
c     
c     calculate a couple of variables needed for COARE
c
c     Development History:
c     2012-10-01  ENVIRON International
c
c------------------------------------------------------------------------------
c
      implicit none
c
c variable definitions
c
      integer, parameter :: nvmax = 24
      character*4 vnam(nvmax)
      logical lvar(nvmax)
      integer ivno (nvmax), nvar, vmiss(nvmax)
      real*8 vmin(nvmax),vmax(nvmax),vsca(nvmax)
      common /varin/ vnam, lvar, ivno, vmin, vmax, vsca, nvar, vmiss
c
c default control variables
c
      real*8        xlatN, xlonW,        defzi, zimin, dlmin, wscalm, 
     &              dvptg, defzw, defzt, defzq, defdep
      integer                     tzone,                mixopt
      common /defs/ xlatN, xlonW, tzone, defzi, zimin, dlmin, wscalm,  
     &              dvptg, defzw, defzt, defzq, defdep, mixopt
c
c coare input variables
c
      real*8        xtime,u,ts,t,q,rs,rl,rain,xlat,xlon,qs,tsw
      COMMON/indata/xtime,u,ts,t,q,rs,rl,rain,xlat,xlon,qs,tsw        !qs added
c
      real*8         zu,zt,zq,ts_depth,zs,p,zi
      COMMON/heights/zu,zt,zq,ts_depth,zs,p,zi
c
      real*8      hwave,twave
      COMMON/wave/hwave,twave       
c
c coare options
c
      integer Jwarm, Jcool, Jwave
      common /options/ Jwarm, Jcool, Jwave
c
c aermod input variables
c
      integer       iccov,                            pcode
      real*8          hvf, wstar, ustar, gamma, zic, zim, xmol, znot, 
     & bowen, alb, wsref, wdref, zwsref, tref, ztref,        prec, relh, 
     &       press,        sigth, sigw
c
      common /aerout/ hvf, wstar, ustar, gamma, zic, zim, xmol, znot, 
     & bowen, alb, wsref, wdref, zwsref, tref, ztref, pcode, prec, relh, 
     &       press, iccov, sigth, sigw
c
c  mixing ht stuff
c
      logical                        lzimiss, lsmooth
      real*8         ziobs, zivenkl
      common /mixht/ ziobs, zivenkl, lzimiss, lsmooth
c
c local variables
c
      logical lmiss, lcalm, lwlcs, lceil, lcc, lsrad, eof
      integer iygmt, imgmt, idgmt, ihgmt, idate, idategmt
      integer iy, im, id, ih, i, icol, jul, julian, iymdh2idate
      integer iceil, ireco
      real*8 xdat(nvmax),cc,um,ea
c
c flag to check for necessary data for warm layer and/or cool skin
c      
      if(Jcool.ne.0.or.Jwarm.ne.0) then
         lwlcs = .true.
      else
         lwlcs = .false.
      end if
c
      read(2,*,err=98,end=99) iy,im,id,ih,(xdat(i), i = 1, nvar-4)
c
c process date and prepare a GMT date/time for COARE
c
      if(iy.le.99) then               ! check 2 digit year
         if(iy.ge.50) then
            iy = iy + 1900
         else
            iy = iy + 2000
         end if
      end if
      idate = iymdh2idate(iy,im,id,ih)
      print *,'processing (yearmodyhr): ',idate
      iygmt = iy
      imgmt = im
      idgmt = id
      ihgmt = ih
      jul = julian (iy,im,id)
      call add_hour(iygmt,imgmt,idgmt,ihgmt,tzone)      
      idategmt = iymdh2idate(iygmt,imgmt,idgmt,ihgmt)
c
c COARE datetime in GMT secs
c
      xtime = idategmt*1.d4           ! xtim can also be a variable in the file
c
c process the critical variables ws, wd, tsea, tair, rh
c if any are missing set the missing value flag & return
c also return if the winds are calm
c 
      lmiss = .false.
      lcalm = .false.
c
c wind speed (m/s)
c
      icol = ivno(1)
      if(xdat(icol).ge.vmin(1).and.xdat(icol).le.vmax(1)) then
         u = xdat(icol)*vsca(1)
         if(u.lt.wscalm) then
            u = 0.
            lcalm = .true.
         end if
         wsref = u
      else
         vmiss(1) = vmiss(1) + 1
         lmiss = .true.
      end if
c
c wdir, watch for calms
c
      if(lcalm) then
         wdref = 0.d0
      else
         icol = ivno(2)
         if(xdat(icol).ge.vmin(2).and.xdat(icol).le.vmax(2)) then
            wdref = xdat(icol)*vsca(2)
         else
            lmiss = .true.
            vmiss(2) = vmiss(2) + 1
         end if
      end if
c
c sea temp (C)
c
      icol = ivno(3)
      if(xdat(icol).ge.vmin(3).and.xdat(icol).le.vmax(3)) then
         ts = xdat(icol)*vsca(3)
      else
         vmiss(3) = vmiss(3) + 1
         lmiss = .true.
      end if
c
c air temp (t(C), tref(K))
c
      icol = ivno(4)
      if(xdat(icol).ge.vmin(4).and.xdat(icol).le.vmax(4)) then
         t = xdat(icol)*vsca(4)
         tref = 273.16 + t
      else
         vmiss(4) = vmiss(4) + 1
         lmiss = .true.
      end if
c
c relative humidity relh(%)
c
      icol = ivno(5)
      if(xdat(icol).ge.vmin(5).and.xdat(icol).le.vmax(5)) then
         relh = xdat(icol)*vsca(5)
c         q = relh/100.d0
      else
         vmiss(5) = vmiss(5) + 1
         lmiss = .true.
      end if
c
c pressure (mb)
c
      p = 1013.2 ! default if missing or not provided
      if(lvar(6)) then
         icol = ivno(6)
         if(xdat(icol).ge.vmin(6).and.xdat(icol).le.vmax(6)) then
            p = xdat(icol)*vsca(6)
         else
            vmiss(6) = vmiss(6) + 1
         end if
      end if
      press = p
c
c srad (w/m2)
c
      lsrad = .false.
      if(lvar(7)) then
         icol = ivno(7)
         if(xdat(icol).ge.vmin(7).and.xdat(icol).le.vmax(7)) then
            rs = xdat(icol)*vsca(7)
            lsrad = .true.
         else
            vmiss(7) = vmiss(7) + 1
         end if
      end if
c
c cloud cover (0-10)
c
      lcc = .false.
      if(lvar(8)) then
         icol = ivno(8)
         if(xdat(icol).ge.vmin(8).and.xdat(icol).le.vmax(8)) then
            cc = xdat(icol)*vsca(8)
            iccov = nint(cc)
            cc = cc/10.d0
            lcc = .true.
         else
            vmiss(8) = vmiss(8) + 1
         end if
      end if
c
c ceiling height (100s ft)
c
      lceil = .false.
      if(lvar(9)) then
         icol = ivno(9)
         if(xdat(icol).ge.vmin(9).and.xdat(icol).le.vmax(9)) then
            iceil = nint(xdat(icol)*vsca(9))
            lceil = .true.
         else
            vmiss(9) = vmiss(9) + 1
         end if
      end if
c
c precip (mm/hr)
c
      rain = 0.d0            ! def for COARE
      if(lvar(10)) then
         icol = ivno(10)
         if(xdat(icol).ge.vmin(10).and.xdat(icol).le.vmax(10)) then
            rain = xdat(icol)*vsca(10)
            prec = rain
         else
            vmiss(10) = vmiss(10) + 1
         end if
      end if
c
c sigma-theta (deg)
c      
      if(lvar(11)) then
         icol = ivno(11)
         if(xdat(icol).ge.vmin(11).and.xdat(icol).le.vmax(11)) then
            sigth = xdat(icol)*vsca(11)
         else
            vmiss(11) = vmiss(11) + 1
         end if
      end if
c
c sigma-w (m/s)
c      
      if(lvar(12)) then
         icol = ivno(12)
         if(xdat(icol).ge.vmin(12).and.xdat(icol).le.vmax(12)) then
            sigw = xdat(icol)*vsca(12)
         else   
            vmiss(12) = vmiss(12) + 1
         end if
      end if
c
c wind speed meas ht (m)
c if not in the file set to default
c
      zu = defzw
      zs = 10.                  ! standard ht for some COARE variables not used
      if(lvar(13)) then
         icol = ivno(13)
         if(xdat(icol).ge.vmin(13).and.xdat(icol).le.vmax(13)) then
            zu = xdat(icol)*vsca(13)
         else   
            vmiss(13) = vmiss(13) + 1
         end if
      end if
      zwsref = zu
c
c air temp meas ht (m)
c if not in the file set to default
c
      zt = defzt
      if(lvar(14)) then
         icol = ivno(14)
         if(xdat(icol).ge.vmin(14).and.xdat(icol).le.vmax(14)) then
            zt = xdat(icol)*vsca(14)
         else   
            vmiss(14) = vmiss(14) + 1
         end if
      end if
      ztref = zt
c
c RH meas ht (m)
c if not in the file set to default
c
      zq = defzq
      if(lvar(15)) then
         icol = ivno(15)
         if(xdat(icol).ge.vmin(15).and.xdat(icol).le.vmax(15)) then
            zq = xdat(icol)*vsca(15)
         else   
            vmiss(15) = vmiss(15) + 1
         end if
      end if
c
c Sea Temp meas depth (m)
c if not in the file set to default
c
      ts_depth = defdep
      if(lvar(16)) then
         icol = ivno(16)
         if(xdat(icol).ge.vmin(16).and.xdat(icol).le.vmax(16)) then
            ts_depth = xdat(icol)*vsca(16)
         else   
            vmiss(16) = vmiss(16) + 1
         end if
      end if
c
c Wave Sig Ht (m)
c
      hwave = -99.d0
      if(lvar(17)) then
         icol = ivno(17)
         if(xdat(icol).ge.vmin(17).and.xdat(icol).le.vmax(17)) then
            hwave = xdat(icol)*vsca(17)
         else   
            vmiss(17) = vmiss(17) + 1
         end if
      end if
c
c Wave period (s)
c
      twave = -99.d0
      if(lvar(18)) then
         icol = ivno(18)
         if(xdat(icol).ge.vmin(18).and.xdat(icol).le.vmax(18)) then
            twave = xdat(icol)*vsca(18)
         else   
            vmiss(18) = vmiss(18) + 1
         end if
      end if
c
c  downward longwave radiation (W/m2)
c
      rl = -99.d0
      if(lvar(19)) then
         icol = ivno(19)
         if(xdat(icol).ge.vmin(19).and.xdat(icol).le.vmax(19)) then
            rl = xdat(icol)*vsca(19)
         else   
            vmiss(19) = vmiss(19) + 1
         end if
      end if
c
c  overwater mixing height
c
      zi = defzi  ! assign COARE zi
      ziobs = -999.d0
      if(lvar(20)) then
         icol = ivno(20)
         if(xdat(icol).ge.vmin(20).and.xdat(icol).le.vmax(20)) then
            ziobs = xdat(icol)*vsca(20)
         else   
            vmiss(20) = vmiss(20) + 1
         end if
      end if
c
c  vertical potential temperature gradient
c  use default if missing or not provided
c
      gamma = dvptg
      if(lvar(21)) then
         icol = ivno(21)
         if(xdat(icol).ge.vmin(21).and.xdat(icol).le.vmax(21)) then
            gamma = xdat(icol)*vsca(21)
         else   
            vmiss(21) = vmiss(21) + 1
         end if
      end if
c
c lat (N)
c
      xlat = xlatN
      if(lvar(22)) then
         icol = ivno(22)
         if(xdat(icol).ge.vmin(22).and.xdat(icol).le.vmax(22)) then
            xlat = xdat(icol)*vsca(22)
         else   
            vmiss(22) = vmiss(22) + 1
         end if
      end if
c
c long (W)
c
      xlon = -xlonW              ! COARE uses degrees E
      if(lvar(23)) then
         icol = ivno(23)
         if(xdat(icol).ge.vmin(23).and.xdat(icol).le.vmax(23)) then
            xlon = -xdat(icol)*vsca(23)
         else   
            vmiss(23) = vmiss(23) + 1
         end if
      end if
c
c COARE Time (yyyymodyhrmnse)
c
      if(lvar(24)) then
         icol = ivno(24)
         if(xdat(icol).ge.vmin(24).and.xdat(icol).le.vmax(24)) then
            xtime = xdat(icol)*vsca(24)
         else   
            vmiss(24) = vmiss(24) + 1
         end if
      end if
c
c check for necessary warm layer and cool skin variables
c
      if(lwlcs.and..not.lmiss.and.rl.lt.0..and.lceil.and.lcc) then
         call radflx(iceil,cc,tref,relh,rl) ! calc down IR if needed from cloud/RH info
      end if
      if(lwlcs.and..not.lsrad) then
         lmiss = .true.         ! need solar rad for these options
      else if(lwlcs.and.rl.lt.0.) then
         lmiss = .true.         ! need solar meas of estimated downward LW rad for these options
      end if
c
c calc spec humidity (kg/kg) from temp, RH and Press
c
      if(.not.lmiss) then
         call humidity(t,p,ea)         !Teten's returns sat. air ea in mb
         ea=ea*relh/100.d0             !convert from RH using vapour pressure      
         q=.62197*(ea/(p-0.378*ea))    !Spec. humidity kg/kg
      end if
c
      return
c
c error reading in line
c
   98 print *,'error reading overwater met file'
      print *,'at line = ',ireco
      write(16,*) 'error reading overwater met file'
      write(16,*) 'at line = ',ireco
      stop 'AERCOARE terminated abnormally'
c
   99 eof=.true.
      return
c
      end subroutine getobs     
c
c**********************************************************************
c**********************************************************************
c
      subroutine bulk_flux(ireco, ldebug)
c
c------------------------------------------------------------------------------
c     AERCOARE
c     VERSION 1.0 2012-10-01
c
c     calculate some of the input variables for the COARE 3.0 bulk
c     flux algorithm including the optional warm layer corrections to
c     the bulk sea temperature
c
c     COARE Bulk Flux Algorithm version 3.0a. See Fairall et al.,2003: J.
c     Climate,16,571-591
c
c     Development History:
c     adapted from cor3_0f.for acccessed at:
c     ftp://ftp1.esrl.noaa.gov/users/cfairall/wcrp_wgsf/computer_programs/cor3_0/
c     2012-10-01  ENVIRON International
c
c------------------------------------------------------------------------------
c
      implicit none
      integer ireco
c
c  common variables ************************************************
c    
      real*8        xtime,u,ts,t,q,rs,rl,rain,xlat,xlon,qs,tsw
      COMMON/indata/xtime,u,ts,t,q,rs,rl,rain,xlat,xlon,qs,tsw
c
      integer                    jamset,jday1
      real*8      qcol_ac,tau_ac,             fxp,tk_pwp,tau_old,
     &            ef_old,hf_old,rf_old,time_old,sst_old,xtime_old
      COMMON/warm/qcol_ac,tau_ac,jamset,jday1,fxp,tk_pwp,tau_old,
     &            ef_old,hf_old,rf_old,time_old,sst_old,xtime_old
c
      real*8       al,be,cpa,cpw,grav,xlv,rhoa,rhow,rgas,toK,von,wetc
      COMMON/const/al,be,cpa,cpw,grav,xlv,rhoa,rhow,rgas,toK,von,wetc
c
      real*8         zu,zt,zq,ts_depth,zs,p,zi
      COMMON/heights/zu,zt,zq,ts_depth,zs,p,zi
c
      integer        Jwarm,Jcool,Jwave
      COMMON/options/Jwarm,Jcool,Jwave
c
      real*8     rns,rnl
      COMMON/rad/rns,rnl
c
      real*8        usr,tsr,qsr,zo,zot,zoq,zL,rr,rt,rq,ri,
     &             dter,dqer,tkt,Du,Wg
      COMMON/ASLout/usr,tsr,qsr,zo,zot,zoq,zL,rr,rt,rq,ri,
     &             dter,dqer,tkt,Du,Wg
c
      real*8      hf,ef
      common/flux/hf,ef
c
c  local variables ***********************************************
c
      real*8 u_zs,t_zs,q_zs,e_zs,esat_zs,qa_zs,rh_zs
      real*8 tau,rf,taur,sst,es
      real*8 CD,CE,CH,Cdn,Cen,Chn
      real*8 alfac,dwat,dtmp,dt_wrm,dqs_dt,q_pwp
      real*8 intime,sol_time,ctd1,ctd2,rich
      real*8 dsea,qjoule,qr_out,dtime
      logical ldebug
c
      real*8 psiu,psit,xmol,Wbar
c
      integer yy,mn,dd,hh,mm,ss, iter1, nsecs
c
      character*19 chtime   !only works with 19
c      
      save                  !necessary for some of the warm layer variables
c
      if(ireco.eq.1) then   !first line of data
       tsw=ts               !henceforth redefined after warm/cool calculations
       dter=0.3*jcool       !or in "if" block below when jwarm=0.
       sst_old=ts-dter      !for initial Rnl
       xtime_old=xtime
      endif
c
c Constants and coefficients (Stull 1988 p640). 
c
      Rgas=287.1                    !J/kg/K     gas const. dry air
      toK=273.16                    ! Celsius to Kelvin
      Cpa=1004.67                   !J/kg/K specific heat of dry air (Businger 1982)
c      Cpv=Cpa*(1.+0.00084*q)         !Moist air - currently not used (Businger 1982)
c      rhoa=P*100./(Rgas*(t+toK)*(1.+.00061*q)) !kg/m3  Moist air density NB q still g/kg
c
      rhoa=P*100./(Rgas*(t+toK)*(1.+.61*q)) !kg/m3  Moist air density now q in kg/kg
c
      call gravity(xlat,grav)       ! gravity equatorial value 9.72 
      al=2.1e-5*(ts+3.2)**0.79      !water thermal expansion coefft.
      be=0.026                      !salinity expansion coefft.
      cpw=4000.                     !J/kg/K specific heat water
      rhow=1022.                    !kg/m3  density water
      von=0.4                       !von Karman's "constant"
c
c   compute net radiation, updated in flux loop of ASL
c   oceanic emissivity 0.97 broadband; albedo 0.055 daily average
c
      if(jcool.ne.0.or.jwarm.ne.0) then
         Rnl= 0.97*(5.67e-8*(sst_old+toK)**4-rl) !Net longwave (up = +). Typically 3C warming=15W/m2
         Rns=0.945*rs                        !Net shortwave (into water)
      end if
c
c     START Warm Layer - check switch
c            
      if(Jwarm.eq.0) then  !jump over warm layer calculation
       tsw=ts
       go to 15            !convert humidities and calculate fluxes in ASL
      endif
c
c  convert time to decimal hours (yyyymnddhhmmss. -> hh.hhhh)
c
      write(chtime(1:19),'(f19.4)') xtime     
      read(chtime,12) yy,mn,dd,hh,mm,ss       !eg 1992,11,25,13,21,00
12    format(i4,5i2)
c
c     check for missing data by examining diff in time between the last
c     time this routine was called
c
      call delxtime(xtime_old,xtime,nsecs)
c  
      intime=(float(hh)+float(mm)/60.+ss/3600.) !eg 13.35
c
c   and then to local solar time in seconds
c
      sol_time=mod(xlon/15+intime+24,24.d00)*3600   !eg 85517.
c
      if(ireco.eq.1) go to 16         !first line of data. Set time_old and compute fluxes in ASL
c
c reset all variables at local midnight or if more than 4 missing hours
c
       if(sol_time.lt.time_old.or.nsecs.gt.14400) then 
         jday1=0                      !reset after last 6am test of first day
         jamset=0                     !indicates heat integration not yet started
         tau_ac=0.0
         qcol_ac=0.0
         dt_wrm=0.0  
c
c initial guess at warm layer parameters expected in early morning
c fxp=0.5 implies a shallow heating layer to start the integration;
c tk_pwp=19.0 implies warm layer thickness is a maximum from the day
c before and is not meant to match this timestep's fxp.
c
         fxp=0.5
         tk_pwp=19.0
         tsw=ts
         goto 16                      
       else if(sol_time.gt.21600..and.jday1.eq.1) then   !6 am too late to start on first day
         dt_wrm=0.
         tsw=ts
         goto 16
       else                       !compute warm layer. Rnl and "_old"s from previous timestep
         rich=.65                                    !critical Rich. No.
         ctd1=sqrt(2.*rich*cpw/(al*grav*rhow))        !u*^2 integrated so
         ctd2=sqrt(2.*al*grav/(rich*rhow))/(cpw**1.5) !has /rhow in both
         dtime=sol_time-time_old !time step for integrals
         qr_out=rnl+hf_old+ef_old+rf_old              !total cooling at surface
         q_pwp=fxp*rns-qr_out                         !total heat absorption in warm layer
         if(q_pwp.lt.50..and.jamset.eq.0) then       !integration threshold
           tsw=ts
           go to 16
         endif
         jamset=1                                    !indicates integration has started
         tau_ac=tau_ac+max(.002d00,tau_old)*dtime       !momentum integral
         if(qcol_ac+q_pwp*dtime.gt.0) then           
           do 10 iter1=1,5                           !iterate warm layer thickness
             fxp=1.-(0.28*0.014*(1.-dexp(-tk_pwp/0.014))   
     &           +0.27*0.357*(1.-dexp(-tk_pwp/0.357))         
     &           +.45*12.82*(1.-dexp(-tk_pwp/12.82)))/tk_pwp !Soloviev solar absorb. prof
             qjoule=(fxp*rns-qr_out)*dtime
             if((qcol_ac+qjoule.gt.0.0))
     &        tk_pwp=min(19.d00,ctd1*tau_ac/dsqrt(qcol_ac+qjoule))  !warm layer thickness
   10      continue
         else                                        !warm layer wiped out
           fxp=.75
           tk_pwp=19.
           qjoule=(fxp*rns-qr_out)*dtime             
         endif                                       
         qcol_ac=qcol_ac+qjoule                      !heat integral
         if(qcol_ac.gt.0) then                       !sign check on qcol_ac
           dt_wrm=ctd2*(qcol_ac)**1.5/tau_ac         !pwp model warming
         else
           dt_wrm=0.
         endif         
       endif
       if(tk_pwp.lt.ts_depth) then           !sensor deeper than pwp layer
         dsea=dt_wrm                         !all warming must be added to ts
       else                                  !warming deeper than sensor
         dsea=dt_wrm*ts_depth/tk_pwp         !assume linear temperature profile
       endif
       tsw=ts+dsea             !add warming above sensor to ts
c
   16 time_old=sol_time                     !all in local solar time
      xtime_old = xtime                     !last time of warm layer calc
c
c   END of warm layer calcs
c
c  q specified in kg/kg in calling routine
c 
   15 call humidity(tsw,p,es)       !sea sat vp returned in mb
      es=es*0.98                    !reduced for salinity Kraus 1972 p. 46
      qs=.62197*(es/(p-0.378*es))   !convert from mb to spec. humidity  kg/kg
c
      call ASL (ireco)   ! 
c
c compute surface fluxes and other parameters. Most of these variables
c are not used by AERMOD but are retained from the original COARE3 code
c
       sst=tsw-dter*jcool             !final skin temperature this timestep
       tau=rhoa*usr*usr*u/Du          !stress N/m2
       hf=-cpa*rhoa*usr*tsr           !sensible W/m2
       ef=-xlv*rhoa*usr*qsr           !latent W/m2
c
c compute heat flux due to rainfall (Not used by AERMOD)
c
       dwat=2.11e-5*((t+toK)/toK)**1.94                    !water vapour diffusivity
       dtmp=(1.+3.309e-3*t-1.44e-6*t*t)*0.02411/(rhoa*cpa) !heat diffusivity
       dqs_dt=q*xlv/(rgas*(t+toK)**2)                      !Clausius-Clapeyron
       alfac= 1./(1.+(wetc*xlv*dwat)/(cpa*dtmp))           !wet bulb factor
       rf= rain*alfac*cpw*((sst-t)+(qs-q-dqer)*xlv/cpa)/3600.
c
c compute momentum flux due to rainfall (Not used by AERMOD)
c
       taur=0.85*rain/3600*u  
c
c Webb correction to latent heat flux already in ef via zoq/rr function
c so return Wbar (Not used by AERMOD)
c
       Wbar=-1.61*usr*qsr/(1.+1.61*q)-usr*tsr/(t+toK)
c
c save fluxes for next timestep warm layer integrals
c
       sst_old = sst
       tau_old=tau 
       ef_old=ef
       hf_old=hf
       rf_old=rf
c
c compute transfer coefficients (Not used by AERMOD)
c
       CD=(USR/Du)**2
       CH=USR*TSR/(Du*(T-sst+.0098*zt)) 
       CE=USR*QSR/(Du*(Q-QS+dqer))                                      
c
c compute neutral transfer coefficients and met variables at standard height
c (Not used by AERMOD)
c
       Cdn=(0.4/dlog(zs/zo))**2
       Chn=0.4*0.4/(dlog(zs/zo)*dlog(zs/zot))
       Cen=0.4*0.4/(dlog(zs/zo)*dlog(zs/zoq))
c
c adjust met. variables to standard height (not used by AERMOD)
c
       u_zs=usr/von*(dlog(zs/zo)-psiu(zL))
       t_zs=sst+tsr/von*(dlog(zs/zot)-psit(zL))
       q_zs=(qs-dqer)+qsr/von*(dlog(zs/zoq)-psit(zL)) !kg/kg
       qa_zs=1000.*q_zs                               !g/kg
       e_zs=q_zs*p/(0.62197+0.378*q_zs)               !mb
       call humidity(t_zs,p,esat_zs)                  !mb
       rh_zs=e_zs/esat_zs                             !RH as decimal
c
c output fluxes for comparison to original COARE test cases or for
c debugging
c
      xmol = zu/zL     ! Monin Obukhov L
      if(ldebug) write(99,200)ireco,xtime,hf,ef,sst,tau,Wbar,rf,
     & dter,dt_wrm,tk_pwp,tkt*1000.,Wg,usr,tsr,qsr*1000,xmol,zo,zot,zoq
200   format(i6,f18.0,3f8.2,2f9.5,6f8.2,3f8.4,1pe10.2,3e10.2)
c
      return      !return to main program for another line of data
      end
c
c**********************************************************************
c**********************************************************************
c
      subroutine ASL (ireco)
c
c------------------------------------------------------------------------------
c     AERCOARE
c     VERSION 1.0 2012-10-01
c
c     TO EVALUATE SURFACE FLUXES, SURFACE ROUGHNESS AND STABILITY OF
c     THE ATMOSPHERIC SURFACE LAYER FROM BULK PARAMETERS BASED ON
c     LIU ET AL. (79) JAS 36 1722-1735
c
c     invokes the cool skin opttion
c
c     COARE Bulk Flux Algorithm version 3.0a. See Fairall et al.,2003: J.
c     Climate,16,571-591
c
c     Development History:
c     adapted from cor3_0f.for acccessed at:
c     ftp://ftp1.esrl.noaa.gov/users/cfairall/wcrp_wgsf/computer_programs/cor3_0/
c     2012-10-01  ENVIRON International
c
c------------------------------------------------------------------------------
c
      implicit none
      integer ireco
c
c  common variables ************************************************
c    
      real*8        xtime,u,ts,t,q,rs,rl,rain,xlat,xlon,qs,tsw
      COMMON/indata/xtime,u,ts,t,q,rs,rl,rain,xlat,xlon,qs,tsw        !qs added
c
      real*8     rns,rnl
      COMMON/rad/rns,rnl
c
      real*8         zu,zt,zq,ts_depth,zs,p,zi
      COMMON/heights/zu,zt,zq,ts_depth,zs,p,zi
c
      real*8       al,be,cpa,cpw,grav,xlv,rhoa,rhow,rgas,toK,von,wetc
      COMMON/const/al,be,cpa,cpw,grav,xlv,rhoa,rhow,rgas,toK,von,wetc
c
      integer        Jwarm,Jcool,Jwave
      COMMON/options/Jwarm,Jcool,Jwave
c
      real*8      hwave,twave
      COMMON/wave/hwave,twave       
c
      real*8        usr,tsr,qsr,zo,zot,zoq,zL,rr,rt,rq,ri,
     &             dter,dqer,tkt,Du,Wg,um
      COMMON/ASLout/usr,tsr,qsr,zo,zot,zoq,zL,rr,rt,rq,ri,
     &             dter,dqer,tkt,Du,Wg     
c
c  local variables
c
      integer nits, iter
      real*8 visw,charn,psiu,psit,zetu,L10,L,Beta,visa,psu,pst
      real*8 bigc,Bf,tcw
      real*8 hsb,hlb,alq,qcol,qout,dels,xlamx,dq,dt,ta
c
c Grachev and Fairall variables
c
      real*8 u10,zo10,cd10,ch10,ct10,zot10,cd,ct,cc,ribcu,ribu
      real*8 cwave,lwave,twopi
c
c Factors
c
      Beta=1.2     !Given as 1.25 in Fairall et al.(1996)
      twopi=3.14159*2.
c 
c Additional constants needed for cool skin
c
      visw=1.e-6                   !m2/s kinematic viscosity water
      tcw=0.6                      !W/m/K   Thermal conductivity water
      bigc=16.*grav*cpw*(rhow*visw)**3/(tcw*tcw*rhoa*rhoa)
      xlv=(2.501-0.00237*tsw)*1e+6 !J/kg latent heat of vaporization at ts (3C warming=0.3%)
      wetc=0.622*xlv*qs/(rgas*(tsw+toK)**2) !Clausius-Clapeyron
      visa=1.326e-5*(1.+6.542e-3*t+8.301e-6*t*t-4.84e-9*t*t*t)   !m2/s
          !Kinematic viscosity of dry air - Andreas (1989) CRREL Rep. 89-11
      ta=t+toK      !air temperature K
c
c Initial guesses
c
      dter=0.3*jcool              !cool skin Dt
      dqer=wetc*dter              !cool skin Dq
      zo=0.0001
      Wg=0.5                      !Gustiness factor initial guess
      tkt= 0.001*jcool                  !Cool skin thickness first guess
c
c Air-sea differences - includes warm layer in Dt and Dq
c
      Du=(u**2.+Wg**2.)**.5       !include gustiness in wind spd. difference
      Dt=tsw-t-0.0098*zt          !potential temperature difference.
      Dq=qs-q                     
c
c **************** neutral coefficients ******************
c
      u10=Du*dlog(10/zo)/dlog(zu/zo)
      usr=0.035*u10
      zo10=0.011*usr*usr/grav+0.11*visa/usr
      Cd10=(von/dlog(10/zo10))**2
      Ch10=0.00115
      Ct10=Ch10/sqrt(Cd10)
      zot10=10./dexp(von/Ct10)
      Cd=(von/dlog(zu/zo10))**2
c      
c ************* Grachev and Fairall (JAM, 1997) **********
c
      Ct=von/dlog(zt/zot10)         ! Temperature transfer coefficient
      CC=von*Ct/Cd                  ! z/L vs Rib linear coefficient
      Ribcu=-zu/(zi*0.004*Beta**3)  ! Saturation or plateau Rib 
      Ribu=-grav*zu*((Dt-dter)+0.61*ta*Dq)/(ta*Du**2)
      if (Ribu.lt.0.) then
          zetu=CC*Ribu/(1.+Ribu/Ribcu)   ! Unstable G and F
      else
          zetu=CC*Ribu*(1.+27./9.*Ribu/CC) ! Stable
      endif
      L10=zu/zetu                       ! MO length
      if (zetu.gt.50) then
        nits=1
      else
        nits=3   ! number of iterations
      endif
c
c First guess M-O stability dependent scaling params.(u*,t*,q*) to estimate zo and z/L
c
      usr= Du*von/(dlog(zu/zo10)-psiu(zu/L10))
      tsr=-(Dt-dter)*von/(dlog(zt/zot10)-psit(zt/L10))
      qsr=-(Dq-dqer)*von/(dlog(zq/zot10)-psit(zq/L10))
c      
      charn=0.011     !then modify Charnock for high wind speeds Chris' data
      if(Du.gt.10) charn=0.011+(0.018-0.011)*(Du-10)/(18-10)
      if(Du.gt.18) charn=0.018
c      
c **** Iterate across u*(t*,q*),zo(zot,zoq) and z/L including cool skin ****
c
      do 10 iter=1,nits
c
c  surface roughness options
c
       if(Jwave.eq.0) then
         zo=charn*usr*usr/grav + 0.11*visa/usr               !after Smith 1988
c
       else if(Jwave.eq.1) then
         if(twave.lt.0.) then ! Oost
            twave = 0.729*max(u10,0.5d0) ! use well developed if twave missing 
         end if
         cwave=grav*twave/twopi
         lwave=cwave*twave
         zo=(50./twopi)*lwave*(usr/cwave)**4.5+0.11*visa/usr !Oost et al.
c
       else if(Jwave.eq.2) then  
         if(hwave.lt.0..or.twave.lt.0.)then  ! if either twave or hwave missing 
           um = max(u10,.5d0)                ! then use well developed sea for both          
           hwave = 0.018*um*um*(1.+.015*um)
           twave = 0.729*um  
         end if
         cwave=grav*twave/twopi
         lwave=cwave*twave
         zo=1200.*hwave*(hwave/lwave)**4.5+0.11*visa/usr     !Taylor and Yelland 
       endif 
c
       rr=zo*usr/visa
c
c *** zoq and zot fitted to results from several ETL cruises ************
c
       zoq=min(1.15d-4,5.5e-5/rr**0.6)
       zot=zoq
c
       zL=von*grav*zu*(tsr*(1.+0.61*q)+0.61*ta*qsr)
     &   /((t+toK)*usr*usr*(1.+0.61*q))
       L=zu/zL
       psu=psiu(zu/L)
       pst=psit(zt/L)
       dqer=wetc*dter*jcool
       usr=Du*von/(dlog(zu/zo)-psiu(zu/L))
       tsr=-(Dt-dter)*von/(dlog(zt/zot)-psit(zt/L))
       qsr=-(Dq-dqer)*von/(dlog(zq/zoq)-psit(zq/L))
       Bf=-grav/ta*usr*(tsr+0.61*ta*qsr)
c
c include gust component
c at this time a ball park zi used, not actual meas or estimate
c because zi calc might depend on ustar and we would need to
c iterate
c
       if (Bf.gt.0) then
          Wg=Beta*(Bf*zi)**.333 
       else
          Wg=0.2
       endif
       Du=sqrt(u**2.+Wg**2.)        !include gustiness in wind spd.
c
       if(jcool.ne.0.or.jwarm.ne.0) then      
          rnl= 0.97*(5.67e-8*(tsw-dter+toK)**4-rl) !Recompute net longwave; cool skin=-2W/m2
       end if
c
c   Cool skin
c
       if(Jcool.ne.0) then
         hsb=-rhoa*cpa*usr*tsr
         hlb=-rhoa*xlv*usr*qsr
         qout=rnl+hsb+hlb
         dels=rns*(.065+11.*tkt-6.6e-5/tkt*(1.-dexp(-tkt/8.0e-4))) !Eq.16 Ohlmann 
         qcol=qout-dels
         alq=Al*qcol+be*hlb*cpw/xlv                         !Eq. 7 Buoy flux water
         if(alq.gt.0.) then                                 !originally (qcol.gt.0)
            xlamx=6./(1.+(bigc*alq/usr**4)**.75)**.333      !Eq 13 Saunders coeff.
            tkt=xlamx*visw/(sqrt(rhoa/rhow)*usr)            !Eq.11 Sublayer thickness
         else
            xlamx=6.                                         !prevent excessive warm skins
            tkt=min(.01d00,xlamx*visw/(sqrt(rhoa/rhow)*usr)) !Limit tkt
         endif
         dter=qcol*tkt/tcw                                  ! Eq.12 Cool skin
         dqer=wetc*dter
       end if
c 
   10 continue                                               ! end iterations
c
      return              !to main subroutine, bulk_flux
      end subroutine ASL
c
c**********************************************************************
c**********************************************************************
c
      subroutine delxtime(xtime1,xtime2,nsecs)
c
c------------------------------------------------------------------------------
c     AERCOARE
c     VERSION 1.0 2012-10-01
c
c     calc the diff (nsecs) between two COARE time stamps
c
c     Development History:
c     2012-10-01  ENVIRON International
c
c------------------------------------------------------------------------------
c     
      implicit none
c
      real*8 xtime1, xtime2
      character*19 chtime
      integer iy1,im1,id1,ih1,imin1,isec1,jul1,nhrs,julian
      integer iy2,im2,id2,ih2,imin2,isec2,jul2,nsecs
c
c  convert COARE time (yyyymnddhhmmss.0000)
c
      write(chtime(1:19),'(f19.4)') xtime1     
      read(chtime,'(i4,5i2)') iy1,im1,id1,ih1,imin1,isec1
      jul1 = julian(iy1,im1,id1)
      write(chtime(1:19),'(f19.4)') xtime2     
      read(chtime,'(i4,5i2)') iy2,im2,id2,ih2,imin2,isec2
      jul2 = julian(iy2,im2,id2)
c
c     check for missing data by examining diff in time between the last
c     time this routine was called
c
      call delhrs(iy1,jul1,ih1,iy2,jul2,ih2,nhrs)
c
      nsecs = nhrs*3600 + (imin2-imin1)*60 + isec2 - isec1
      return
      end subroutine delxtime
c
c**********************************************************************
c**********************************************************************
c
      subroutine humidity(T,P,esat)                                 
c
c------------------------------------------------------------------------------
c     AERCOARE
c     VERSION 1.0 2012-10-01
c
c     Tetens' formula for saturation vp Buck(1981) JAM 20, 1527-1532 
c     invokes the cool skin opttion
c
c     Development History:
c     adapted from cor3_0f.for acccessed at:
c     ftp://ftp1.esrl.noaa.gov/users/cfairall/wcrp_wgsf/computer_programs/cor3_0/
c     2012-10-01  ENVIRON International
c
c------------------------------------------------------------------------------
c     
      implicit none
      real*8 T,P,esat !  T(degC)  P(mb) esat(mb)

      esat = (1.0007+3.46d-6*P)*6.1121*dexp(17.502*T/(240.97+T)) !mb
c
      return
c
      end subroutine humidity
c
c**********************************************************************
c**********************************************************************
c
      function psiu(zL)
c
c------------------------------------------------------------------------------
c     AERCOARE
c     VERSION 1.0 2012-10-01
c
c     psiu and psit evaluate stability function for wind speed and scalars
c     matching Kansas and free convection forms with weighting f
c     convective form follows Fairall et al (1996) with profile constants
c     from Grachev et al (2000) BLM
c     stable form from Beljaars and Holtslag (1991)
c
c     Development History:
c     adapted from cor3_0f.for acccessed at:
c     ftp://ftp1.esrl.noaa.gov/users/cfairall/wcrp_wgsf/computer_programs/cor3_0/
c     2012-10-01  ENVIRON International
c
c------------------------------------------------------------------------------
c
      implicit none
      real*8 zL,x,y,psik,psic,f,psiu,c
      if(zL.lt.0) then
       x=(1-15.*zL)**.25                        !Kansas unstable
       psik=2.*dlog((1.+x)/2.)+dlog((1.+x*x)/2.)-2.*datan(x)
     &     +2.*datan(1.d0)
       y=(1.-10.15*zL)**.3333                   !Convective
       psic=1.5*dlog((1.+y+y*y)/3.)-dsqrt(3.d0)*datan((1.+2.*y)
     &      /dsqrt(3.d0))+4.*datan(1.d0)/dsqrt(3.d0)
       f=zL*zL/(1.+zL*zL)
       psiu=(1.-f)*psik+f*psic
      else
       c=min(50.d00,0.35*zL)                       !Stable
       psiu=-((1.+1.*zL)**1.+.6667*(zL-14.28)/dexp(c)+8.525)
      endif
c
      return
      end function psiu
c
c**********************************************************************
c**********************************************************************
c
      function psit(zL)
c
c------------------------------------------------------------------------------
c     AERCOARE
c     VERSION 1.0 2012-10-01
c
c     psiu and psit evaluate stability function for wind speed and scalars
c     matching Kansas and free convection forms with weighting f
c     convective form follows Fairall et al (1996) with profile constants
c     from Grachev et al (2000) BLM
c     stable form from Beljaars and Holtslag (1991)
c
c     Development History:
c     adapted from cor3_0f.for acccessed at:
c     ftp://ftp1.esrl.noaa.gov/users/cfairall/wcrp_wgsf/computer_programs/cor3_0/
c     2012-10-01  ENVIRON International
c
c------------------------------------------------------------------------------
c
      real*8 zL,x,y,psik,psic,f,psit,c
      if(zL.lt.0) then
       x=(1-15.*zL)**.5                          !Kansas unstable
       psik=2.*dlog((1.+x)/2.)
       y=(1.-34.15*zL)**.3333                    !Convective
       psic=1.5*dlog((1.+y+y*y)/3.)-dsqrt(3.d0)*datan((1.+2.*y)
     &       /dsqrt(3.d0))+4.*datan(1.d0)/dsqrt(3.d0)
       f=zL*zL/(1.+zL*zL)
       psit=(1.-f)*psik+f*psic
      else
       c=min(50.d00,0.35*zL)                        !Stable
       psit=-((1.+2.*zL/3.)**1.5+.6667*(zL-14.28)/dexp(c)+8.525)
      endif
      return
      end function psit
c
c**********************************************************************
c**********************************************************************
c
      Subroutine gravity(lat,g)
c
c------------------------------------------------------------------------------
c     AERCOARE
c     VERSION 1.0 2012-10-01
c
c     calculates g as a function of latitude using the 1980 IUGG formula
c       
c     Bulletin Geodesique, Vol 62, No 3, 1988 (Geodesist's Handbook)
c     p 356, 1980 Gravity Formula (IUGG, H. Moritz)
c     units are in m/sec^2 and have a relative precision of 1 part
c     in 10^10 (0.1 microGal)
c     code by M. Zumberge.
c
c     check values are:
c
c      g = 9.780326772 at latitude  0.0
c      g = 9.806199203 at latitude 45.0
c      g = 9.832186368 at latitude 90.0
c
c     Development History:
c     adapted from cor3_0f.for acccessed at:
c     ftp://ftp1.esrl.noaa.gov/users/cfairall/wcrp_wgsf/computer_programs/cor3_0/
c     2012-10-01  ENVIRON International
c
c------------------------------------------------------------------------------
c
      implicit none
      real*8 gamma, c1, c2, c3, c4, phi, lat, g
      gamma = 9.7803267715
      c1 = 0.0052790414
      c2 = 0.0000232718
      c3 = 0.0000001262
      c4 = 0.0000000007
      phi = lat * 3.14159265358979 / 180.0
      g = gamma * (1.0 
     $ + c1 * ((sin(phi))**2)
     $ + c2 * ((sin(phi))**4)
     $ + c3 * ((sin(phi))**6)
     $ + c4 * ((sin(phi))**8))
c
      return
c
      end subroutine gravity
c
c**********************************************************************
c**********************************************************************
c
      subroutine radflx (jceil,cc,tair,rh,qlw)
c
c------------------------------------------------------------------------------
c     AERCOARE
c     VERSION 1.0 2012-10-01
c --------------------------------------------------------------------
c --- CALMET    Version: 6.326        Level: 070404              RADFLX
c ---           F.Robe
c
c --- PURPOSE:  Compute downward long wave radiative 
c               fluxes at the surface for input to the COARE subroutine
c
c --- INPUT:  
c             JCEIL     - integer - station ceiling height (in 100's ft)
c             CC        - real    - Fractional Cloud cover
c             TAIR      - real    - Surface air temperature (K)
c             RH        - real    - Surface relative humidity (%)
c
c                                     
c --- OUTPUT:
c                qLW    - real    - Downward (not net) long wave  
c                                   rad. flux at the surface (W/m2)
c----------------------------------------------------------------------
c
      implicit none
      integer jceil
      real*8 rh, cc, wp, qlw, clw, tair, rn1, rn2, rn3
      real*8 rhfrac
c
c     DOWNWARD LONG WAVE RADIATIVE FLUX(qLW) 

c ---       clear sky: QLW=epsa*sigb*Tair**4, with 
c                sigb: Stefan Boltzman constant (5.67 e-8)
c                epsa: atm. longwave emissivity
c                epsa=0.725+0.17log10(wp), 
c                wp: precipitable water in cm, varies between 0.1 and 5 cm
c                see MM5 techincical notes (TN 398-June 1994)
c                precipitable water in cm as a function of Rel Hum (decimal=>rh/100)
c                and air temp (in K)- http://www.uswcl.ars.ag.gov/exper/relhum.htm
            rhfrac=rh*0.01d00

c --- 060525  Place a 1% floor on RH here
c           rh=AMAX1(.01,rh)
c --- 061230  Put bounds on wp to keep wp within range of applicability
c             (0.1< wp < 5cm) rather than putting bound on RH
            wp=0.439*rhfrac*dexp(26.23-5416./tair)/tair
            wp = max(wp,0.1d00)
            wp = min(wp,5.d00)
c
            qlw =(0.725+0.17*dlog10(wp))*5.67d-8 * tair**4
c
c  --- cloud enhancement factor: Clw
c     clw=c1*n1+c2*n2+c3*n3, (MM5 technical note  TN-398 - June 1994)
c        with ci = enhancement factor for cloud layer i 
c             ni = cloud fraction of low (below 800mb), 
c                   middle(800-450mb), high level clouds(> 450mb)
c             ni=function of RH in that layer - Not available (only surface RH)
c                attribute all the cloud cover (ccgrid) to the layer where the 
c                ceiling height (jceil) is - jceil is in 100's feet
c                800mb ~ 1950m ~ 6400ft (ICAO standard Atmosphere) 
c                450mb ~ 6400m ~ 21000ft
      rn1 = 0.d0
      rn2 = 0.d0
      rn3 = 0.d0
      if (jceil.lt.64) then
         rn1 = cc
      else if (jceil.le.210) then
         rn2 = cc
      else if (jceil.gt.210)then
         rn3 = cc
      endif
c
      clw = 0.26*rn1+0.22*rn2 + 0.006*rn3
      qlw = qlw*(1+clw)
c
      return
c
      end subroutine radflx
c
c**********************************************************************
c**********************************************************************
c
      subroutine write_sfc_head
c
c------------------------------------------------------------------------------
c     AERCOARE
c     VERSION 1.1 2013-04-18
c
c     Development History:
c
c     2013-04-18  (D13108)
c
c     change SFC file header to confirm to AERMOD/AERMET 12345
c
c     write out SFC file header with dummy station numbers
c
c------------------------------------------------------------------------------
c
      implicit none
c
c runtime data and version
c 
      character*10 timenow,datenow,tznow,version*6
      common /runt/timenow,datenow,tznow,version
c
c default control variables
c
      real*8        xlatN, xlonW,        defzi, zimin, dlmin, wscalm, 
     &              dvptg, defzw, defzt, defzq, defdep
      integer                     tzone,                mixopt
      common /defs/ xlatN, xlonW, tzone, defzi, zimin, dlmin, wscalm,  
     &              dvptg, defzw, defzt, defzq, defdep, mixopt
c
c local variables
c
      integer, parameter :: ista1 = 99999, ista2 = 99999, ista3 = 99999
      character*10 alat, alon
      character*6 spec1, spec2, spec3, spec4*9, v12345, spec5*9
c
      v12345 = ' 12345'  ! AERMOD/AERMET version for header
      spec1='UA_ID:'
      spec2='SF_ID:'
      spec3='OS_ID:'
      spec4=' VERSION:'
      spec5='AERCOARE:'
c
      write(alat,'(f9.3,"N")') xlatN
      write(alon,'(f9.3,"W")') xlonW
c
      write(3,11,err=99) alat, alon, spec1, ista1, spec2, ista2,
     & spec3, ista3, spec4, v12345, spec5, version
   11 format(2a10,t31,a6,t38,i8,t48,a6,t55,i8,t65,a6,t72,i8,t84,a9,
     & t93,a6,t135,a9,t144,a6)
      return
c
   99 print *,'Error writing SFC file header'
      print *,'alat, alon, spec1, ista1, spec2, ista2,',
     & 'spec3, ista3, version:'
      print *,alat, alon, spec1, ista1, spec2, ista2,
     & spec3, ista3, version
c
      write(16,*) 'Error writing SFC file header'
      write(16,*) 'alat, alon, spec1, ista1, spec2, ista2,',
     & 'spec3, ista3, version:'
      write(16,*) alat, alon, spec1, ista1, spec2, ista2,
     & spec3, ista3, version
c
      stop 'AERCOARE terminated abnormally'
c
      end subroutine write_sfc_head
c
c**********************************************************************
c**********************************************************************
c
      subroutine set_missing
c
c------------------------------------------------------------------------------
c     AERCOARE
c     VERSION 1.0 2012-10-01
c
c     assign missing data codes to SFC and PFL output variables
c
c     Development History:
c     2012-10-01  ENVIRON International
c
c------------------------------------------------------------------------------
c
      implicit none
c
      integer       iccov,                            pcode
      real*8          hvf, wstar, ustar, gamma, zic, zim, xmol, znot, 
     & bowen, alb, wsref, wdref, zwsref, tref, ztref,        prec, relh, 
     &       press,        sigth, sigw
c
      common /aerout/ hvf, wstar, ustar, gamma, zic, zim, xmol, znot, 
     & bowen, alb, wsref, wdref, zwsref, tref, ztref, pcode, prec, relh, 
     &       press, iccov, sigth, sigw
c
      hvf = -999.0d0
      wstar = -9.0d0
      ustar = -9.0d0
      gamma = -9.0d0
      zic = -999.0d0
      zim = -999.0d0
      xmol = -99999.0d0
      znot = -9.0d0
      bowen = -9.0d0
      alb = -9.0d0
      wsref = 999.0d0
      wdref = 999.0d0
      zwsref = -9.0d0
      tref = 999.0d0
      ztref = -9.0d0
      pcode = 9999
      prec = -9.0d0
      relh = 999.0d0
      press = 9999.d0
      iccov = 99
      sigth = 999.0d0
      sigw = 99.0d0
c     
      return
      end subroutine set_missing
c
c**********************************************************************
c**********************************************************************
c
      subroutine get_control(fname,fdebug,ldebug)
c
c------------------------------------------------------------------------------
c     AERCOARE
c     VERSION 1.0 2012-10-01
c
c     get & process variables from control input file (fname)
c
c     Development History:
c     2012-10-01  ENVIRON International
c
c------------------------------------------------------------------------------
c
      implicit none
c
c common blocks **************************************************
c
c
c runtime data and version
c 
      character*10 timenow,datenow,tznow,version*6
      common /runt/timenow,datenow,tznow,version
c
c variable definitions
c
      integer, parameter :: nvmax = 24
      character*4 vnam(nvmax)
      logical lvar(nvmax)
      integer ivno (nvmax), nvar, vmiss(nvmax)
      real*8 vmin(nvmax),vmax(nvmax),vsca(nvmax)
      common /varin/ vnam, lvar, ivno, vmin, vmax, vsca, nvar, vmiss
c
c default control variables
c
      real*8        xlatN, xlonW,        defzi, zimin, dlmin, wscalm, 
     &              dvptg, defzw, defzt, defzq, defdep
      integer                     tzone,                mixopt
      common /defs/ xlatN, xlonW, tzone, defzi, zimin, dlmin, wscalm,  
     &              dvptg, defzw, defzt, defzq, defdep, mixopt
c
c default coare options
c
      integer Jwarm, Jcool, Jwave
      common /options/ Jwarm, Jcool, Jwave
c
c  mixing ht stuff
c
      logical                        lzimiss, lsmooth
      real*8         ziobs, zivenkl
      common /mixht/ ziobs, zivenkl, lzimiss, lsmooth
c
c local arrays **************************************************
c
      integer, parameter :: nvp4 = nvmax+4
      integer ndum, i, j
      real*8 xlow, xhi, xsca
      logical ldebug
      character*4 var(nvp4), vdum
      character*80 filein, filesfc, filepfl, fileout, errmess, fname
      character*80 fdebug
      character*132 header
c
      ndum = nvp4
c
c get the date, time, and timezone at runtime
c
      call date_and_time(datenow,timenow,tznow)
c
c  open control/option file
c
      errmess ='error reading or opening control file'      ! control file
      open(unit=1,file=fname, status ='old', action='read', err=99)
c
c  read in the files names & open them
c
      filein = 'aercoare.csv'                               ! met input file
      errmess = 'error reading or opening met input file'
      read(1,*,err=99) filein
      open(unit=2, file=filein, status ='old', action='read',err=99)
c
      filesfc = 'aercoare.sfc'                              ! sfc file
      errmess = 'error reading or opening sfc output file'
      read(1,*,err=99) filesfc
      open(unit=3, file=filesfc, err=99)
c
      filepfl = 'aercoare.pfl'                              ! pfl file
      errmess = 'error reading or opening pfl output file'
      read(1,*,err=99) filepfl
      open(unit=4, file=filepfl, err=99)
c
      fileout = 'aercoare.out'                              ! listing
      errmess = 'error reading or opening listing output file'
      read(1,*,err=99) fileout
      open(unit=16, file=fileout, err=99)
c
      if(ldebug) then
         errmess = 'error opening optional debug output file'
         open(unit=99, file=fdebug, err=99)
      end if
c
c echo runtime info & file names to list file
c      
      write(16, 11) version,datenow,timenow(1:2),timenow(3:4),
     &              timenow(5:10),tznow,
     &              fname, filein, filesfc, filepfl
   11 format('AERCOARE Summary',/,
     &       '------------------------------ ----------',/,
     &       ' AERCOARE Version             : ',a,//,
     &       ' Run Date                     : ',a,/,
     &       ' Run Time (hour:min:sec)      : ',a,':',a,':',a,/,
     &       ' Run Time Zone                : ',a,/, 
     &       ' Control/Option file          : ',a,/,
     &       ' Overwater meteorological file: ',a,/,
     &       ' Output SFC file for AERMOD   : ',a,/,
     &       ' Output PFL file for AERMOD   : ',a,/)
c
      errmess = 'error reading latitude'                  
      read(1,*,err=98) xlatN                                 ! latN
      errmess = 'latitude must be within -90 to 90'
      if(xlatN.lt.-90..or.xlatN.gt.90.) then
         write(16,*) 'xlatN =', xlatN
         go to 98
      end if
c
      errmess = 'error reading longitude'                  
      read(1,*,err=98) xlonW                                ! LongW
      errmess = 'longitude must be within -180 to 180'
      if(xlonW.lt.-180..or.xlonW.gt.180.) then
         write(16,*) 'xlonW =', xlonW
         go to 98
      end if
c
      errmess = 'error time zone'                  
      read(1,*,err=98) tzone                                ! tzone
      errmess = 'time zone must be within -12 to 12'
      if(tzone.lt.-12.or.tzone.gt.12) then
         write(16,*) 'tzone =', tzone
         go to 98
      end if
c
      errmess = 'error reading minimum mixing height allowed'
      read(1,*,err=98) defzi                                ! defzi
      errmess = 'error COARE gust mixing height must be > 0.'
      if(zimin.lt.0.) then
         write(16,*) 'defzi =', defzi
         go to 98
      end if
c
      errmess = 'error reading minimum mixing height allowed'
      read(1,*,err=98) zimin                                ! zimin
      errmess = 'error min mixing height must be > 0.'
      if(zimin.lt.0.) then
         write(16,*) 'zimin =', zimin
         go to 98
      end if
c
      errmess ='error reading minimum abs(Monin-Obukhov Length) allowed'
      read(1,*,err=98) dlmin                                ! dlmin
      errmess ='minimum abs(Monin-Obukhov Length) must be > 0.'
      if(dlmin.lt.0.) then
         write(16,*) 'dlmin =', dlmin
         go to 98
      end if
c
      errmess = 'error calms threshold wind speed'
      read(1,*,err=98) wscalm                               ! wscalm
      errmess ='calms thresholds must be between 0-2 m/s'
      if(wscalm.lt.0..or.wscalm.gt.2.) then
         write(16,*) 'wscalm =', wscalm
         go to 98
      end if
c
      errmess = 'error reading def vert. pot. temp gradient'
      read(1,*,err=98) dvptg                                ! dvtpg
      errmess ='def VPTG must be between 0-0.10 degC/m'
      if(dvptg.lt.0..or.dvptg.gt.0.1) then
         write(16,*) 'dvptg =', dvptg
         go to 98
      end if
c
      errmess = 'error reading def wind measurement height'
      read(1,*,err=98) defzw                                ! defzw
      errmess = 'def wind measurement ht must be between .05-50 m'
      if(defzw.lt.0.05.or.defzw.gt.50.) then
         write(16,*) 'defzw =', defzw
         go to 98
      end if
c
      errmess = 'error reading def temp measurement height'
      read(1,*,err=98) defzt                                ! defzt
      errmess = 'def temp measurement ht must be between .05-50 m'
      if(defzt.lt.0.05.or.defzt.gt.50.) then
         write(16,*) 'defzt =', defzt
         go to 98
      end if
c
      errmess = 'error reading def RH measurement height'
      read(1,*,err=98) defzq                                ! defzq
      errmess = 'def RH measurement ht must be between .05-50 m'
      if(defzq.lt.0.05.or.defzq.gt.50.) then
         write(16,*) 'defzq =', defzq
         go to 98
      end if
c
      errmess = 'error reading def sea measurement depth'
      read(1,*,err=98) defdep                               ! defdep
      errmess = 'def sea measurement depth must be between .001-10 m'
      if(defdep.lt.0.001.or.defdep.gt.10.) then
         write(16,*) 'defdep =', defdep
         go to 98
      end if
c
      errmess = 'error reading overwater mixing height option'
      read(1,*,err=98) mixopt                                ! Mixopt
      errmess = 'Mixing Ht option must be -2 to 2'
      if(mixopt.lt.-2.or.mixopt.gt.2) then 
         write(16,*) 'mixopt =', mixopt
         go to 98
      else if (mixopt.lt.0) then
         lsmooth = .true.                                   ! smoothing
      else
         lsmooth = .false.
      end if
      mixopt = abs(mixopt)
c
      errmess = 'error reading COARE warm layer option'
      read(1,*,err=98) Jwarm                                ! Jwarm
      errmess = 'warm layer option must be 0 or 1'
      if(Jwarm.lt.0.or.Jwarm.gt.1) then
         write(16,*) 'Jwarm =', Jwarm
         go to 98
      end if
c
      errmess = 'error reading COARE cool skin option'
      read(1,*,err=98) Jcool                                ! Jcool
      errmess = 'cool skin option must be 0 or 1'
      if(Jcool.lt.0.or.Jcool.gt.1) then
         write(16,*) 'dvptg =', Jcool
         go to 98
      end if
c
      errmess = 'error reading COARE wave option'
      read(1,*,err=98) Jwave                                ! Jwave
      errmess = 'wave option must (0, 1 or 2)'
      if(Jwave.lt.0.or.Jwave.gt.2) then
         write(16,*) 'Jwave =', Jwave
         go to 98
      end if
c
c echo input control variables to list file
c
      write(16, 12) xlatN,  xlonW, tzone, defzi,  zimin, dlmin, wscalm, 
     &              dvptg, defzw, defzt, defzq, defdep, mixopt, Jwarm,
     &              Jcool, Jwave
   12 format(/,'Control File Options',/,
     &         '------------------------------- ----------',/,  
     &         ' Latitude (degN)              : ',f10.4,/,
     &         ' Longitude (degW)             : ',f10.4,/,
     &         ' Time Zone (5-EST.. 8-PST)    : ',i10,/,
     &         ' COARE Gust Mix Ht (m)        : ',f10.1,/,
     &         ' Mininum Overwater Mix Ht (m) : ',f10.1,/,
     &         ' Min Abs(L) allowed           : ',f10.1,/,
     &         ' Calm Threshold (m/s)         : ',f10.2,/,
     &         ' Default VPTG (degC/m)        : ',f10.3,/,
     &         ' Default Wind Meas. Ht (m)    : ',f10.2,/,
     &         ' Default Temp Meas. Ht (m)    : ',f10.2,/,
     &         ' Default RelH Meas. Ht (m)    : ',f10.2,/,
     &         ' Default Sea Temp Depth (m)   : ',f10.3,/,
     &         ' Mix Ht Option (-2 to 2)      : ',i10,/,
     &         ' COARE Warm Layer Option (0-1): ',i10,/,
     &         ' COARE Cool Skin Option (0-1) : ',i10,/,
     &         ' COARE Wave Option (0-2)      : ',i10)
c
c  get the order of the input met data from the header
c
      errmess = 'error reading in header line of met file'
      read(2,'(a)',err=98) header
      call get_variable_defs(header,var,nvar,ndum)
c
c  make sure the 1st 4 variables are the yr,mn,dy,hr
c
      if(var(1)(1:2).ne.'yr'.or.var(2)(1:2).ne.'mo'.or.
     &   var(3)(1:2).ne.'dy'.or.var(4)(1:2).ne.'hr') then
         print *, '1st 4 variable names must be "yr mo dy hr"'
         stop 'AERCOARE abnormally terminated'
      end if
c
c establish order of met data
c
      do i = 5, nvar
         do j = 1, nvmax
            if(var(i).eq.vnam(j)) then
               lvar(j) = .true.
               ivno(j) = i - 4
            end if
         end do
      end do
c
c  check to see if there any changes to def min, max, scale
c
      errmess ='error reading in variable def, min, max, scale'
   10 read(1,*,err=98,end = 20) vdum, xsca, xlow, xhi
      if(vdum.ne.'end'.and.vdum.ne.'END') then
         do j = 1, nvmax
            if(vdum.eq.vnam(j)) then
               vsca(j) = xsca
               vmin(j) = xlow
               vmax(j) = xhi
            end if
         end do
         go to 10
      end if
c
c  echo variable definitions for variables in the met file
c
   20 write(16, 13)
   13 format(//,'Overwater Input File Variables and Limits',/,
     &'  n name column     scale       min       max',/,
     &' -- ---- ------ --------- --------- ---------')
      do i = 1, nvmax
         if(lvar(i)) then
            write(16, 14) i,vnam(i),ivno(i)+4,vsca(i), vmin(i), vmax(i)
   14       format(i3,1x,a4,i7,3g10.3)
         end if
      end do
c
c close the control file no longer needed
c
      close(unit=1) 
c
c establish whether the minumum number of variables are present
c
c we need   ws          dir         tsea      tair           RH
c
      if(.not.lvar(1).or..not.lvar(2).or..not.lvar(3).or.
     &   .not.lvar(4).or..not.lvar(5)) then
         print *,'The overwater met file does not contain the minumum'
         print *,'number of variables (ws,dir,tsea,tair,RH)'
         write(16,*) 'The overwater met file does not contain the'
         write(16,*) 'minimum number of variables (ws,dir,tsea,tair,RH)'
         stop 'AERCOARE abnormally terminated'
      end if
c
c if the warm layer and/or cool skin options are selected we need
c solar radiation and either downwelling radiation or cloud cover
c and ceiling height
c
      if(Jcool.ne.0.or.Jwarm.ne.0) then
         if(.not.lvar(7)) then                   ! srad ?
            print *,'Warm Layer and/or Cool Skin Options Selected'
            print *,'without Solar Radiation data'
            write(16,*) 'Warm Layer and/or Cool Skin Options Selected'
            write(16,*) 'without Solar Radiation data'
            stop 'AERCOARE abnormally terminated'
         else if (.not.lvar(19).and..not.lvar(8).and..not.lvar(9)) then ! no rdow and no clouds ?
            print *,'Warm Layer and/or Cool Skin Options Selected'
            print *,'without Down LW Radiation, CCover, or Ceil. Ht.'
            write(16,*) 'Warm Layer and/or Cool Skin Options Selected'
            write(16,*) 'without Down LW Radiation, CCover,or Ceil. Ht.'
            stop 'AERCOARE abnormally terminated'
         end if
      end if

c
      return
c
   98 print *, errmess
      write(16,*) errmess
      stop 'AERCOARE abnormally terminated'
c
   99 print *, errmess
      stop 'AERCOARE abnormally terminated'
c
      end subroutine get_control
c
c**********************************************************************
c**********************************************************************
c
      subroutine get_variable_defs(line,var,nvar,nvmax)
c
c------------------------------------------------------------------------------
c     AERCOARE
c     VERSION 1.0 2012-10-01
c
c     return 4 character variable names from a line with nonletter
c     delimiters (e.g. "," " " etc) & convert all to lowercase
c
c     line - 132 character input line
c     var - array of 4 char names
c     nvar number of variables found in line
c
c     Development History:
c     2012-10-01  ENVIRON International
c
c------------------------------------------------------------------------------
c
      implicit none
      integer i, ivar, i1, i2, n, nvar, nvmax
      character*132 line
      character*4 var(*)
c
      n = len_trim(line)
      i1 = 1
      nvar = 0
c   
      do ivar = 1, nvmax
         do i = i1, n
            if((ichar(line(i:i)).ge.65.and.ichar(line(i:i)).le.90).or.   ! skip non letters
     &      (ichar(line(i:i)).ge.97.and.ichar(line(i:i)).le.122) ) then
               if(ivar.le.4) then
                  i2 = min(132,i + 1) ! 2 digit yr, mo, dy, hr
               else
                  i2 = min(132,i + 3) ! 4 digit variable names 
               end if
               call lowercase(line(i:i2),var(ivar))
c               print *, 'var ', var(ivar)
               nvar = nvar + 1
               exit
            else
               cycle
            end if
         end do
         if(i.gt.n) exit
         i1 = i2 + 1
      end do
c
      return
      end subroutine get_variable_defs
c
c**********************************************************************
c**********************************************************************
c
      subroutine lowercase(texti,texto)
c
c------------------------------------------------------------------------------
c     AERCOARE
c     VERSION 1.0 2012-10-01
c
c     convert mixed text to lowercase
c
c     Development History:
c     2012-10-01  ENVIRON International
c
c------------------------------------------------------------------------------
c
      implicit none
      integer i, n
      character*(*) texti, texto
c
      n = min(len(texti),len(texto)) ! the length of the shorter string just incase
      do i = 1, n
         if(ichar(texti(i:i)).ge.65.and.ichar(texti(i:i)).le.90) then
           texto(i:i) = char(ichar(texti(i:i))+32)
         else
           texto(i:i) = texti(i:i)
         end if
      end do
      return
      end subroutine lowercase
c
c**********************************************************************
c**********************************************************************
c
      block data
c
c------------------------------------------------------------------------------
c     AERCOARE
c     VERSION 1.1 2013-04-18
c
c     Development History:
c
c     2013-04-18  (D13108)
c
c     change SFC file header to confirm to AERMOD/AERMET 12345
c
c     2012-10-01  ENVIRON International
c
c------------------------------------------------------------------------------
c
      implicit none
c
c runtime data and version
c 
      character*10 timenow,datenow,tznow,version*6
      common /runt/timenow,datenow,tznow,version
c
c variable names with min/max limits and scales
c
      integer, parameter :: nvmax = 24
      character*4 vnam(nvmax)
      logical lvar(nvmax)
      integer ivno (nvmax), nvar, vmiss(nvmax)
      real*8 vmin(nvmax),vmax(nvmax),vsca(nvmax)
      common /varin/ vnam, lvar, ivno, vmin, vmax, vsca, nvar, vmiss
c
c default control variables
c
      real*8        xlatN, xlonW,        defzi, zimin, dlmin, wscalm, 
     &              dvptg, defzw, defzt, defzq, defdep
      integer                     tzone,                mixopt
      common /defs/ xlatN, xlonW, tzone, defzi, zimin, dlmin, wscalm,  
     &              dvptg, defzw, defzt, defzq, defdep, mixopt
c
c default coare options
c
      integer Jwarm, Jcool, Jwave
      common /options/ Jwarm, Jcool, Jwave
c
      data timenow,datenow,tznow,version/'badtime','baddate',
     &                                   'badtimez','D13108'/
c
c default variable names, min, max limits, scales to def units, column number
c
      data lvar /nvmax*.false./, nvar/0/          
c
      data vnam /'wspd', 'wdir', 'tsea', 'tair', 'relh', 'pres',         
     &           'srad', 'tsky', 'ceil', 'rain', 'sigt', 'sigw',         
     &           'zwsp', 'ztem', 'zrel', 'zdep', 'hwav', 'twav',         
     &           'rdow', 'mixh', 'vptg', 'latn', 'lonw', 'xtim'/                                 
c
      data vmin /    0.,     0.,    -3.,   -30.,     0.,   900., 
     &               0.,     0.,     0.,     0.,     0.,     0., 
     &               0.,     0.,     0.,     0.,     0.,     0., 
     &               0.,     0.,  0.005,   -90.,  -180., 1.9d13/                         
c
      data vmax /   50.,   360.,    50.,    50.,   100.,  1100.,
     &            1500.,    10.,  1000.,   254.,   105.,     5.,
     &              50.,    50.,    50.,    10.,    60.,    40.,
     &            1000.,  5000.,     .1,    90.,   180., 3.0d13/
c
      data vsca / nvmax*1./, ivno/nvmax*1/, vmiss/nvmax*0/
c
c  variable control defaults
c
      data defzi, zimin, dlmin, dvptg, defzw, defzt, defzq,
     &     defdep, wscalm /600., 25., 5., 0.01, 3.5, 3.5, 3.5, 
     &     0.5, 0.5 /
      data xlatN, xlonW, tzone /-99., -999., 0/
      data mixopt, Jwarm, Jcool, Jwave /4*0/
      end block data
