c
c                 (C) Copyright 1990 the Regents of the
c             University of California. All Rights Reserved.
c
c            This work was produced under the sponsorship of
c             the U.S. Department of Energy. The Government
c                    retains certain rights therein.
c
c            The user may copy and distribute the SLAB program or
c        include the SLAB program, in whole or in part, in another
c        computer program provided that the following conditions
c        are met:
c            1.  No fee may be charged for copying or distributing 
c                the SLAB program or any part thereof.
c            2.  The SLAB program may not be distributed,in whole 
c                or in part, as part of any commercial product.
c
c            This computer program is provided "as is".  An enhanced
c        and user supported commercial version of SLAB is available
c        from Bowman Environmental Engineering, Phone: (214) BEE-LINE.
c
c
c           ******  *******  *******    ***    ******   *     *
c          *           *     *         *   *    *    *   *   *
c          *           *     *        *     *   *    *    * *
c           *****      *     ****     *******   *    *     *
c                *     *     *        *     *   *    *     *
c                *     *     *        *     *   *    *     *
c          ******      *     *******  *     *  ******      *
c
c
c               ******  *******    ***    *******  *******
c              *           *      *   *      *     *
c              *           *     *     *     *     *
c               *****      *     *******     *     ****
c                    *     *     *     *     *     *
c                    *     *     *     *     *     *
c              ******      *     *     *     *     *******
c
c
c                   ******  *          ***    ******
c                  *        *         *   *    *    *
c                  *        *        *     *   *    *
c                   *****   *        *******   *****
c                        *  *        *     *   *    *
c                        *  *        *     *   *    *
c                  ******   *******  *     *  ******
c
c   Simple Prediction Heavy Gas Dispersion Model
c     Developed 1990 by DL Ermak
c
c   Meteorological Variables
c
c     - za    ambient measurement height (m)
c     - ta    ambient temperature ('k)
c     - ua    ambient wind speed (m/s)
c     - z0    surface roughness height (m)
c     - rh    relative humidity (percent)
c     - wmae  molecular weight of ambient air (kg)
c     - cpaa  heat capacity of ambient air (kg)
c     - wma   molecular weight of dry air (kg)
c     - cpa   heat capacity of dry air at const. p. (j/kg-'k)
c     - wmw   molecular weight of water (kg)
c     - cpwv  heat capacity of water vapor (j/kg-'k)
c     - cpwl  heat capacity of liquid water (j/kg-'k)
c     - rhoa  density of ambient air (kg/m3)
c     - pa    ambient atmospheric pressure (pa=n/m2=j/m3)
c     - hmx   mixing layer height (m)
c     - uastr ambient friction velocity (m/s)
c     - stab  stability class values
c
c    class   value      description
c      a      1.0      very unstable
c      b      2.0        unstable
c      c      3.0    slightly unstable
c      d      4.0         neutral
c      e      5.0     slightly stable
c      f      6.0         stable
c
c   default   0.0    input 'ala' for stability
c
c     - ala   inverse monin-obukhov length (1/m)
c             (ala is an input parameter only when stab=0.0)
c
c   source variables
c
c     - idspl spill source type
c           1 - evaporating pool release
c           2 - horizontal jet release
c           3 - vertical stack/jet release
c           4 - instantaneous or short duration evaporating pool release
c     - wms   molecular weight of source gas (kg)
c     - cps   heat capacity at const. p. (j/kg-'k)
c     - ts    temperature of source material ('k)
c     - rhos  density of source gas (kg/m3)
c     - qs    mass source rate (kg/s)
c     - as    source area (m2)
c     - ws    vapor evporation rate or vertical jet source velocity (m/s)
c     - bs    source half width; bs=.5*sqrt(as) (m)
c     - tsd   continuous source duration (s)
c     - qtcs  continuous source mass (kg)
c     - qtis  instantaneous source mass (kg)
c     - hs    source height (m)
c     - us    horizontal jet source velocity (m/s)
c     - tbp   boiling point temperature ('k)
c     - cmed0  initial liquid mass fraction
c     - dhe   heat of vaporization (j/kg)
c     - cpsl  liquid heat capacity (j/kg-'k)
c     - rhosl liquid density of source material (kg/m3)
c     - spb   saturation pressure constant  (default: spb=-1.)
c     - spc   saturation pressure constant  (default: spc=0.)
c
c   additional variable  definitions
c
c     - tav   concentration averaging time (s)
c     - zp(i) heights of concentration calculation; i=1,4 (m)
c     - xffm  far field length (m)
c     - ncalc sub-step multiplier (input parameter)
c     - nssm  number of calculational sub-steps (nssm=3*ncalc)
c     - grav  acceleration of gravity (m/s2)
c     - rr    gas constant (j/mol -'k)
c
c   subroutines used in main calculation
c        slope
c        solve
c        eval
c        slopepf
c        solvepf
c        evalpf
c        entran
c        thermo
c        store
c        errint
c        editin
c        edits
c        editcc
c
c    functions used in main calculation
c        uafn
c
      program state


      common sr3,third,f227,dbt,utst,wse,bs,bse,nssm
      common bb0,b0,r0,utm3,utc3,sft,htp,htp0,ncalc
      common h,bb,b,beta,r,g,vg,v,w,cm,u,t,rho,cv,xn,x
      common wma,cpa,grav,rhoa,rhos,pa,rr,qs,ws,hrf,urf
      common cf0,cf1,cri,tutrd,f17,f19,fu,ft,fv,sfu
      common cth,keval,uab,uab0,u0,cws,wss,ala0,zt,cu1,cu2

      common sr33,sctst,slup,sr2,srpi,cf00,sigb,tav0,uastr,ala
      common xk,aa,uastr2,rcf,afa,vh,stb,zl,hmx,ubs2,ubs20,xstr
      common idspl,nx,f(15),dy(15),betacp(61),sigp(61),cvpm(61),zm(61)
      common fcx(61),fcy(6,61),fcz(5,61),cvpt(6,4,61),bbcp(61),ccp(61)
      common alfg,alfgv,h0,tsd,qtcs,qtis,hs,us,stab,vg0,wcp(61)
      common zc,zc0,wc,wc0,sru0,rho0,bbv,bbv0,bv,bv0,dbbx,cmdap(61)
      common gw,fw,cp,facss,facp1,facp2,tbp,sfx,sfy,sfz

      common/var/vp(61),wp(61),hp(61),bbp(61),vgp(61),up(61),bp(61),
     1 rhop(61),cmp(61),tp(61),cvp(61),betap(61),xp(61),cmwvp(61),
     1 cmevp(61),uap(61),zcp(61),timp(61),bbxp(61),ugp(61),vxp(61),
     1 bxp(61),betaxp(61),qintp(61),tcld(61),cmwp(61),xccp(61),tccp(61)

      common za,ta,ua,z0,wms,cps,ts,nzpm,zp(5),
     1 as,xffm,tav,msfm,mnfm,mffm,tffm

      common tim,timn,vx,bbx,bbx0,gx,rmi,bbvx,bbvx0,
     1 ug,ug0,fug,phimi,phgam,idpf,nxtr,bx,bx0,bvx,bvx0,betax,
     1 qint,qint0,cm0,cmwv,cmwv0,cmev,cmev0,cp0,cpaa,cpwv,cpwl,
     1 cpsl,dhw,dhe,cmdaa,cmwa,rhosl,spa,spb,spc,spaw,spbw,t0,
     1 wmae,wmw,rhowl,cmw,cmw0,cmed,rh,tau0,tgon

c  constants for calculating stability regions

      dimension sum(15),dxxi(3),dxrk(4)

      open(2,file='input',status='old')
      open(3,file='predict',status='new')






c
c  ***  *     *  ***  *******  ***    ***    *        ***  *******  *******
c   *   **    *   *      *      *    *   *   *         *        *   *
c   *   * *   *   *      *      *   *     *  *         *       *    *
c   *   *  *  *   *      *      *   *******  *         *      *     ****
c   *   *   * *   *      *      *   *     *  *         *     *      *
c   *   *    **   *      *      *   *     *  *         *    *       *
c  ***  *     *  ***     *     ***  *     *  *******  ***  *******  *******
c

c  ============
c   read input
c  ============

      read (2,107) idspl,ncalc
  107 format (i5)
      read (2,108) wms,cps,tbp,cmed0,dhe,cpsl,rhosl,spb,spc
      read (2,108) ts,qs,as,tsd,qtis,hs
      read (2,108) tav,xffm,(zp(i),i=1,4)
  108 format (f10.3)

  110 read (2,108) z0
       if(z0 .lt. 0.) stop

      read (2,108) za,ua,ta,rh,stab
      stabin = stab

c ================================
c  calculate stability parameters
c ================================

      al1 = .0081/(z0**.3044)
      if (z0 .gt. .0111) then
         al2 = .0385/(z0**.1715)
         al3 = .0875/(z0**.1028)
       else
         al2 = al1 +.0137/(z0**.1715) + .0218
         al3 = al2 +.0557
      endif

      en2 = log(al2/al1)/log(2.)
      en3 = log(al3/al1)/log(3.)
      eni = en3 + en3 - en2
      dln = en2 - eni
      alm = al1*(3.5**(eni+dln/3.25))

      if (stab .eq. 0.) then
         read (2,108) ala
         aal = abs(ala)
         if (aal .lt. al2) then
            astb = (aal/al1)**(1./en2)
         else if (aal .lt. alm) then
            ral = (aal-al2)/(al3-al2)
            en = eni + dln/(1.+ral*ral)
            astb = (aal/al1)**(1./en)
         else
            astb = 3.5
         endif

              if (ala.ge.0.) then
                 stb = astb
               else
                 stb = -astb
              endif

         stab = 4.0 + stb

         else
            stb = stab -4.0
         astb = abs(stb)
         if (astb .lt. 2.0) then
            aal = al1*(astb**en2)
         else if (astb .lt. 3.5) then
            en = eni + dln/(1.+(astb-2.)*(astb-2.))
            aal = al1*(astb**en)
         else
            aal = alm
         endif

              if (stb.ge.0.) then
                 ala = aal
               else
                 ala = -aal
              endif

         endif

c =============
c  write input
c =============


      write (3,114) idspl,ncalc,wms,cps,tbp,cmed0,dhe,cpsl,rhosl,spb,
     1 spc,ts,qs,as,tsd,qtis,hs,tav,xffm,(i,zp(i),i=1,4)

  114 format (///'problem input'//'  idspl =',5x,i5/'  ncalc =',5x,i5/
     1 '  wms   =',f10.6/'  cps   =',f10.2/
     1 '  tbp   =',f10.2/'  cmed0 =',f10.2/
     1 '  dhe   =',f10.0/'  cpsl  =',f10.2/'  rhosl =',f10.2/
     1 '  spb   =',f10.2/'  spc   =',f10.2/
     1 '  ts    =',f10.2/'  qs    =',f10.2/'  as    =',f10.2/
     1 '  tsd   =',f10.0/'  qtis  =',f10.2/'  hs    =',f10.2/
     1 '  tav   =',f10.2/'  xffm  =',f10.2/('  zp(',i1,') =',f10.2))

      write (3,115) z0,za,ua,ta,rh,stabin
  115 format ('  z0    =',f10.6/'  za    =',f10.2/'  ua    =',f10.2/
     1 '  ta    =',f10.2/,'  rh    =',f10.2/,'  stab  =',f10.2)

      if (stabin .eq. 0.) then
      write (3,112) ala
  112 format ('  ala   =',f10.4)
      endif

c  ===============
c   set constants
c  ===============

c   gas constant

       rr = 8.31431

c  saturation pressure constant default

      if (spb .lt. 0.) then
        spb = dhe*wms/rr
        spc = 0.
      endif

c  boiling point temperature check

      spa = spb/(tbp+spc)
      if (ts .lt. tbp) ts = tbp
      if (cmed0 .gt. 0.) ts = tbp

c  initial droplet concentration

      cmed = cmed0

c  atmospheric and physical constants

      wma = 0.02896
      cpa = 1005.87
      grav = 9.80665
      pa = 101325.

      wmw = 0.01802
      rhowl = 1000.
      cpwv = 1846.
      cpwl = 4178.
      dhw = 2441000.
      spaw = 15.08
      spbw = 5514.

      rpwa = .01*rh*exp(spaw-spbw/ta)
      cmwa = wmw*rpwa/(wma+(wmw-wma)*rpwa)
      cmdaa = 1.-cmwa
      cpaa = cmdaa*cpa + cmwa*cpwv
      wmae = wma*wmw/(wmw+(wma-wmw)*cmwa)

c  number of integration steps

      nssm = ncalc + ncalc + ncalc
      msfm = 11
      mnfm = 50
      mffm = 61

c  number of zp values
      nzpm = 1
      do 125 i=2,4
      if (zp(i) .eq. 0.)  go to 130
      nzpm = i
  125 continue
  130 continue

c  constants
      sr2 = sqrt(2.)
      sr3 = sqrt(3.)
      sr33 = sr3/3.
      third = 1./3.
      tutrd = third + third

      xk = .41
      pi = 3.141592654
      srpi = sqrt(pi)
      facss = .5/(sr2*srpi)
      facp1 = .5/srpi
      facp2 = 1./srpi
      f227 = 2./27.
      f17 = 1./7.
      f19 = .187256

      sctst = .0005

      aa = 1.50
      cf00 = .086
      sigb = .0004
      cf1 = .02
      cri = .025
      cth = .14

      alfg = .25
      alfgv = .75
      cws = .05

      tav0 = 900.

      utst = 1.
      slup = -1.


      ft = 0.
      bmax = 0.

c   ====================
c   calculate parameters
c   ====================

c  calculate ambient meteorological values

      rhoa = wmae*pa/(rr*ta)

      tau0 = 10.
      at0 = 0.
      afa = .08*(((at0+tau0*exp(-at0/tau0))/tav0)**.2)

      if (stb .lt. 0.) then
      zl = exp(-.8*stb)
      else
      zl = 1. + .8*stb
      endif

      if (za .gt. 3.) then
      z = za
      else
      z = 3.
      endif

      ala0 = ala*(1.+z/zl)
      hmx = 130.*(2.**(3.-stb))
      bs = .5*sqrt(as)

c  check if source below mixing layer height
      if (hs .gt. (hmx-bs)) then
         write (3,140) hs,hmx,bs
  140   format ('input source height hs is greater than the',/,
     1   ' calculated mixing layer height hmx minus ',/,
     1   'the stack half width bs.  program terminated.',/,1p,
     1   10x,'hs =',e10.2,'(m)',/,10x,'hmx =',e10.2,'(m)',/,10x,'bs =',
     1   e10.2,'(m)')
         stop
      endif
      if(stb .lt. 0.) then
      phimi = 1./sqrt(sqrt(1.-16.*zl*ala0))
      phgam = -8.*ala0/(1.-phimi)
      endif

c  initialize velocity function

      zt = 2.71828183*z0
      cu1 = 1./zt
      cu2 = 0.
      uf = uafn(zt,z0,ala0,zl,hmx,zt,cu1,cu2)
      if (ala0 .lt. 0.) then
      phmi = 1./sqrt(sqrt(1.-16.*zl*ala0))
      gu = -8.*ala0/(1.-phmi)
      phm = phmi + (1.-phmi)/sqrt(1.+gu*zt)
      else
      phm = 1.+5.*ala0*zt/(1.+zt/zl)
      endif
      ufp = phm*(1.-zt/hmx)/zt
      cu1 = (2.*uf-zt*ufp)/zt
      cu2 = (zt*ufp-uf)/(zt*zt)

c  calculatre friction velocity

      uastr = xk*ua/uafn(za,z0,ala0,zl,hmx,zt,cu1,cu2)
      uastr2 = uastr*uastr
      uaz2 = (uastr/xk)*uafn(2.,z0,ala0,zl,hmx,zt,cu1,cu2)
      tffm = xffm/uaz2
      cf0 = uastr/uaz2
      rcf = sqrt(cf0/cf00)
      hrf = 4.0
      urf = uafn(hrf,z0,ala0,zl,hmx,zt,cu1,cu2)


c
c  *     *    ***    ***  *     *           *****     ***    *         *****
c  **   **   *   *    *   **    *          *     *   *   *   *        *     *
c  * * * *  *     *   *   * *   *          *        *     *  *        *
c  *  *  *  *******   *   *  *  *          *        *******  *        *
c  *     *  *     *   *   *   * *          *        *     *  *        *
c  *     *  *     *   *   *    **          *     *  *     *  *        *     *
c  *     *  *     *  ***  *     *           *****   *     *  *******   *****
c
c
c   main calculation


c ====================================
c ====================================
c  select source type and write input
c ====================================
c ====================================


  150 rhos = wms*pa/(rr*ts)
      qtcs = qs*tsd
      hqtcs = .5*qtcs
      nxtr = mffm + 1
      idpf = 0
      tgon = 1.

      if(idspl .eq. 1) then
c ==========================
c  evaporating pool release
c ==========================
      ws = qs/(rhos*as)
      bse = bs
      qtis = 0.
      hs = 0.
      us = 0.
      zc = 0.
      zc0 = 0.
      zcp(1) = 0.
      xcc0 = 0.
      tim = tsd
      cmed = 0.
      call editin
      go to 200

      else if (idspl .eq. 2) then
c ========================
c  horizontal jet release
c ========================
      t0 = ts
      cmev0 = 1.-cmed
      alfm = (wmae/wms)*cmev0
      betm = (rhoa/rhosl)*cmed
      rho = rhoa*ta/(alfm*t0+betm*ta)

      us = qs/(rho*as)
      alfg = 0.
      bb = bs
      h = bb+bb
      hh = .5*h
      htp = hs+hh
      if (hs .le. hh) then
         alfg = .25
         bb = .5*(sqrt(hs*hs + as+as) - hs)
         h = bb + hs
         htp = h
      endif

      if (htp .gt. hmx) htp=hmx
      if (h .gt. hmx) then
         bb = h*bb/hmx
         h = hmx
      endif
      ws = 0.
      qtis = 0.
      tp(1) = t0
      cm0 = 1.
      cmp(1) = cm0
      cvp(1) = 1.
      cmw0 = 0.
      cmwp(1) = cmw0
      cmwv0 = 0.
      cmwvp(1) = cmwv0
      cmevp(1) = cmev0
      cmdap(1) = 0.
      cp0 = cmev0*cps + cmed*cpsl

      bse = 1.
      zc = hs
      zcmx = hmx-.5*h
      if (zc .gt. zcmx) zc=zcmx
      zc0 = zc
      zcp(1) = zc
      xcc0 = 1.
      bxs0 = 0.
      tim = tsd

      call editin
      go to 400

      else if (idspl .eq. 3) then

c ======================
c  vertical jet release
c ======================


      t0 = ts
      cmev0 = 1.-cmed
      alfm = (wmae/wms)*cmev0
      betm = (rhoa/rhosl)*cmed
      rho = rhoa*ta/(alfm*t0+betm*ta)
      rho0 = rho

      ws = qs/(rho*as)
      us = 0.
      bse = 1.
      qtis = 0.
      xcc0 = 1.
      bxs0 = 0.
      tim = tsd

      call editin

c  plume rise calculation
      hsu = hs
      if (hs .lt. 1.0) hsu = 1.0
      uahs = (uastr/xk)*uafn(hsu,z0,ala0,zl,hmx,zt,cu1,cu2)

      betu = .4 + 1.2*uahs/ws
      hprm = 1.0363*ws*bs/(betu*sqrt(uahs*uastr))
      h67 = hprm**.85714
      do 155 i=1,3
      hprt = h67*((hs+hprm)**.14286)
      hprm = (hs+.85714*hprm)*hprt/(hs+hprm-.14286*hprt)
  155 continue

c  dense gas, momentum, or buoyant jet
      if (rho .gt. rhoa) then

c  dense gas jet plume rise
         ds = 4.0*bs/srpi
         fr2 = ws*ws/(grav*ds*(rho-rhoa)/rhoa)
         sgs = rho/rhoa
         rwus = ws/uahs
         hprd = 1.32*ds*((rwus*sgs*fr2)**.3333)
         hpr = hprm*hprd/sqrt(hprd*hprd+hprm*hprm)
         if ((hs+hpr) .gt. hmx) hpr=hmx-hs
         xpr = .435*hpr*hpr*hpr/(rwus*rwus*sgs*ds*ds)
         cvpk = 1.69*rwus/((hpr/ds)**1.85)
         if (cvpk .gt. .99) cvpk = .99
         cv = (.5236*(1.-cvpk) + cvpk)*cvpk
         cm = wms*cv/(wmae+(wms-wmae)*cv)
         r0 = .5*qs
         r = r0/cm
         cm0 = 1.
         cmev0 = 1.-cmed
         cmw0 = 0.
         cmwv0 = 0.
         cp0 = cmev0*cps + cmed*cpsl
         xn = 1.+xpr
         sft = 0.
         call thermo
         ijt = 2
        else

c  momentum or buoyant jet plume rise
         hprb = 1.7254*grav*(rhoa-rho)*ws*bs*bs/
     1     (rhoa*uahs*uastr*uastr)
         h35 = hprb**.6
         do 160 i=1,3
         hprt = h35*((hs+hprb)**.4)
         hprb = (hs+.6*hprb)*hprt/(hs+hprb-.4*hprt)
  160    continue
         hpr = sqrt(hprb*hprb+hprm*hprm)
         hs = hs+hpr
         if (hs .gt. (hmx-bs))  hs = hmx-bs
         us = (uastr/xk)*uafn(hs,z0,ala0,zl,hmx,zt,cu1,cu2)
         ws = 0.
         rho = rhoa
         rhos = rhoa
         as = qs/(rho*us)
         alfg = 0.
         bb = .5*sqrt(as)
         if (bb .lt. bs) bb = bs
         h = as/(bb+bb)
         hh = .5*h
         htp = hs+hh
         if (hs .lt. hh) then
            alfg = .25
            bb = h*bb/htp
            h = htp
         endif
         if (htp .gt. hmx) htp=hmx
         if (h .gt. hmx) then
            bb = h*bb/hmx
            h = hmx
         endif
         ts = (wms/wmae)*ta
         t0 = ts
         tp(1) = t0
         tbp = .99*amin1(ts,ta)
         spa = spb/(tbp+spc)
         spaw = spbw/tbp
         cm0 = 1.
         cmp(1) = cm0
         cvp(1) = 1.
         cmw0 = 0.
         cmwp(1) = cmw0
         cmwv0 = 0.
         cmwvp(1) = cmwv0
         cmev0 = 1.
         cmevp(1) = cmev0
         cmdap(1) = 0.
         cps = (wmae/wms)*cpa
         cp0 = cps
         zc = hs
         zcmx = hmx-.5*h
         if (zc .gt. zcmx) zc = zcmx
         zc0 = zc
         zcp(1) = zc
         tgon = 0.
         ijt = 0
      endif
      if (ijt .lt. 1) then

c  continue as horizontal jet
         go to 400
        else

c  go to plume rise interpolation
         go to 500
      endif

      else if (idspl .eq. 4) then
c ==========================================================
c  instantaneous of short duration evaporating pool release
c ==========================================================
      ws = qs/(rhos*as)
      bse = bs

      t0 = ts
      tp(1) = t0
      cm0 = 1.
      cmp(1) = cm0
      cvp(1) = 1.
      cmw0 = 0.
      cmwp(1) = cmw0
      cmwv0 = 0.
      cmwvp(1) = cmwv0
      cmdap(1) = 0.
      if (qtis .eq. 0.) cmed = 0.
      cmev0 = 1.-cmed
      cmevp(1) = cmev0
      cp0 = cmev0*cps + cmed*cpsl
      alfm = (wmae/wms)*cmev0
      betm = (rhoa/rhosl)*cmed
      rho = rhoa*ta/(alfm*t0+betm*ta)
      rho0 = rho
      rhop(1) = rho

      hs = qtis/(rho*as)
      us = 0.
      zc = 0.
      zc0 = 0.
      zcp(1) = 0.
      idpf = 2
      tim = 0.
      nxtr = 1
      call editin
      go to 700

      else
      go to 900

      endif


c ==========================
c ==========================
c  evaporating pool release
c ==========================
c ==========================

c ============================
c  pool source initialization
c ============================


  200 continue
      anstp = nssm*(msfm-1)

  250 nxi = 2
      dbt = 0.
      slup = slup + 1.

      dx = (bse+bse)/anstp
      x = -bse
      xp(1) = -bse
      timp(1) = tsd
      wse = ws*bs*bs/(bse*bse)
      wss = ws

      vg = 0.
      vg0 = 0.
      vgp(1) = 0.

      wc = 0.
      wc0 = 0.
      wcp(1) = 0.
      ug = 0.
      ug0 = 0.
      ugp(1) = 0.

      sft0 = 0.
      qint = 0.
      qint0 = 0.

      fv = 0.
      ft = 0.
      fu = 0.
      fw = 0.

      bb = bse
      bb0 = bse
      bbp(1) = bse
      bbv = bb
      bbv0 = bb
      b = .9*bse
      b0 = .9*bse
      bp(1) = .9*bse
      bv = b
      bv0 = b

      betap(1) = sqrt(bb*bb-b*b)/sr3

      rho = rhoa
      rhop(1) = rho
      rho0 = rho

      cvp(1) = 0.

      cm0 = 0.
      cmp(1) = cm0
      cmw0 = cmwa
      cmwp(1) = cmw0
      cmwv0 = cmwa
      cmwvp(1) = cmwv0
      cmev0 = 0.
      cmevp(1) = cmev0
      cmdap(1) = cmdaa

      cp0 = cpaa
      t0 = ta
      tp(1) = ta

      z1 = sr3*aa*xk*xk*xk*urf*dx/uastr
      z2 = (3.*z1*z0*z0)**(1./3.)
      fhz = log(1.+z2/z0)
      he = (3.*z1)/(fhz*fhz)
      do 275 i=1,5
      fhz = log(1.+he/z0)
      shz = he + z0
      fhh = shz*fhz*fhz - 2.*shz*fhz + he + he - z1
      he = he - fhh/(fhz*fhz)
  275 continue

      h = 0.
      hp(1) = h
      h0 = h
      htp = h
      htp0 = htp

      uab = 0.
      dh = he/5.
      h1 = -.5*dh
      h2 = 0.

      do 280 i=1,5
      h1 = h1 + dh
      h2 = h2 + dh
      u1 = uafn(h1,z0,ala0,zl,hmx,zt,cu1,cu2)
      u2 = uafn(h2,z0,ala0,zl,hmx,zt,cu1,cu2)
      uab = uab + 4.*u1 + 2.*u2
  280 continue
      uab = (uastr/xk)*(uab-u2)/30.
      uab0 = uab
      uap(1) = 0.
      u = uab
      u0 = u
      up(1) = 0.

      r0 = rho*u*bb*h
      sru0 = 0.

      w = u*he/(sr3*dx)
      wp(1) = w
      al = ala
      if(al) 290,291,291
  290 stby = 1. - rcf*10.*al
      go to 292
  291 stby = 1./(1.+rcf*10.*al)
  292 v = afa*stby*ua
      vp(1) = v
      vx = 0.
      vxp(1) = 0.


c ====================================
c ====================================
c  steady state plume dispersion mode
c  source region calculation
c ====================================
c ====================================

c ===============================================
c  integrate steady state conservation equations
c ===============================================


      do 355 nx=nxi,msfm
      do 345 ns=1,nssm

      do 306 i=1,11
  306 sum(i) = 0.
      dxxi(1) = .5*dx
      dxxi(2) = .5*dx
      dxxi(3) = dx

      dxrk(1) = dx/6.
      dxrk(2) = dx/3.
      dxrk(3) = dx/3.
      dxrk(4) = dx/6.

      do 325 k=1,4

      keval=k

      call slope

      do 320 j=1,11

      sum(j) = sum(j) + dxrk(k)*f(j)

      if (k .eq. 4) then
      dy(j) = sum(j)
      else

      xn = x + dxxi(k)
      dy(j) = dxxi(k) * f(j)
      endif

  320 continue

      call solve

      call thermo

      call eval

      call entran

      sum(2) = sum(2) + dbbx
      dbt = dbt + dbbx

  325 continue

      x = xn
      htp0 = htp
      r0 = r
      bb0 = bb
      b0 = b
      bbv0 = bbv
      bv0 = bv
      rho0 = rho
      h0 = h
      vg0 = vg
      wc0 = wc
      zc0 = zc
      t0 = t
      u0 = u
      uab0 = uab
      ubs20 = ubs2
      srug = .5*alfg*grav*(rho-rhoa)*bb*h*h
      sru0 = r*u - r*(1.-cm)*uab + srug
      qint0 = qint

      cm0 = cm
      cmw0 = cmw
      cmwv0 = cmwv
      cmev0 = cmev
      cp0 = cp

  345 continue

      call store
      xstr = h/cf0

  355 continue

c  check for source field recalculation

      if(slup.ge..5) goto 370
      if(utst.lt.2.) goto 390

  370 if(slup.gt.20.) goto 390

c  recalculate initial source size

      if(dbt.eq.0.) goto 374
      bmin = bse
      bsen = sqrt(bse*(bse+dbt))
      if(bmax.eq.0.) goto 382
      if(bsen.lt.bmax) goto 382
      bsen = .5*(bmax+bse)
      go to 382

  374 bmax = bse
      rhof2 = rhop(msfm) * rhop(msfm)
      dcm = 1. - cmp(msfm)
      dcm3 = dcm * dcm * dcm
      ua3 = uab*uab*uab

      bsen = 3.375 * alfg*grav * (rhop(msfm) - rhoa) * r/
     1 (ua3*rhof2*dcm3) - bbp(msfm) + bbp(1)
      bsen = .5*(bse+bsen)
      if(bsen.lt.bmax) goto 378
      bsen = .5*(bmin+bmax)
      go to 382
  378 if(bsen.gt.bmin) goto 382
      bsen = .5*(bmin+bse)

  382 rbs = abs(bsen-bse)/bsen
      if(rbs.gt.sctst) goto 386

      slup = 20.
  386 bse = bsen

c  return for source region recalculation
      go to 250

  390 if (qint .gt. hqtcs) then
          idspl = 4

c  switch to short duration release
          go to 150
      endif

      bxs0 = bse
      do 395 i=1,11
      xccp(i) = .5*(bse+xp(i))
  395 continue
      tccp(1) = (qintp(6)+qintp(6))/qs
      do 396 i=1,5
      tccp(i+i) = (qintp(5+i)+qintp(6+i))/qs
      tccp(i+i+1) = (qintp(6+i) + qintp(6+i))/qs
  396 continue

c  go to near field region calculation
      go to 600


c =====================================
c =====================================
c  horizontal and vertical jet release
c =====================================
c =====================================

c ======================================
c  horizontal jet source initialization
c ======================================


  400 msfm = 1
      mnfm = mffm - 1
      x = 1.
      xp(1) = 1.
      xccp(1) = 1.
      tccp(1) = 0.
      timp(1) = tsd
      wse = 0.

      vg = 0.
      vg0 = 0.
      vgp(1) = 0.

      wc = 0.
      wc0 = 0.
      wcp(1) = 0.
      ug = 0.
      ug0 = 0.
      ugp(1) = 0.

      fu = 0.
      fv = 0.
      fw = 0.
      ft = 0.
      qint = 0.
      qint0 = 0.

      bb0 = bb
      bbp(1) = bb
      bbv = bb
      bbv0 = bb
      b = .9*bb
      b0 = b
      bp(1) = b
      bv = b
      bv0 = b

      betap(1) = sqrt(bb*bb-b*b)/sr3
      rhop(1) = rho
      rho0 = rho

      hp(1) = h
      h0 = h
      htp0 = htp

      uab = 0.
      dh = htp/5.
      h1 = -.5*dh
      h2 = 0.

      do 405 i=1,5
      h1 = h1 + dh
      h2 = h2 + dh
      u1 = uafn(h1,z0,ala0,zl,hmx,zt,cu1,cu2)
      u2 = uafn(h2,z0,ala0,zl,hmx,zt,cu1,cu2)
      uab = uab + 4.*u1 + 2.*u2
  405 continue
      uab = (uastr/xk)*(uab-u2)/30.
      uab0 = uab
      uap(1) = uab

      u = us
      u0 = u
      up(1) = u

      r0 = rho*u*bb*h
      srug = .5*alfg*grav*(rho-rhoa)*bb*h*h
      sru0 = r0*u + srug

      delu = (rhoa/rho)*abs(uab-us)
      if (htp .gt. h)  then
         ubs20 = uastr2
         ustr2 = uastr2 + cf1*delu*delu
        else
         umgs = (uastr/uab)*us
         ubs20 = umgs*umgs
         ustr2 = ubs20 + cf1*delu*delu
      endif
      uah = uafn(htp,z0,ala0,zl,hmx,zt,cu1,cu2)
      w = (urf/uah)* aa*xk*sqrt(ustr2)
      wp(1) = w

      if (ala .lt. 0.)  then
         stby = 1. - rcf*10.*ala
        else
         stby = 1./(1. + rcf*10.*ala)
      endif
      atot = afa*stby
      rab = .5*sigb/atot
      va = atot*uab/(1. + rab*bb/sr3)
      vjp = aa*xk*delu
      v = sqrt(va*va + cf1*vjp*vjp)
      vp(1) = v

      vx = 0.
      vxp(1) = 0.

c  go to near field region calculation
      go to 600






c ========================================
c  vertical jet plume rise interpolations
c ========================================

  500 msfm = 2.0 + xpr/(bs+bs)
      msfm = min0(msfm,11)
      amsfm = msfm
      dxpr = xpr/(amsfm-1.)

      zcr = hs+hpr
      uab = (uastr/xk)*uafn(zcr,z0,ala0,zl,hmx,zt,cu1,cu2)
      uab0 = uab
      cfpr = 3.*(2.+(xpr/bs))*cf1
      cmpr = sqrt((1.-cm)*(1.-cm) + 4.*cm*cfpr)
      cupr = .5*uab/(1.-cfpr)
      u = cupr * (1.-cm-cfpr-cfpr+cmpr)
      u0 = u

      ar = qs/(rho*u*cm)
      am = qs/(rho*u)
      hft = 2.4 + 1.6/(1.+(xpr*xpr)/(100.*bs*bs))
      bb = sqrt(ar/hft)
      if (bb .lt. bs)  bb = bs
      b = .9*sqrt(am/hft)
      bsm = .9*bs
      if (b .lt. bsm)  b = bsm
      h = ar/(bb+bb)
      hh = .5*h
      htp = zcr+hh
      alfg = 0.
      if (hh .gt. zcr) then
         bb = h*bb/htp
         b = h*b/htp
         h = htp
         alfg = .25
      endif
      if (htp .gt. hmx) htp=hmx
      if (h .gt. hmx) then
         bb = h*bb/hmx
         b = h*b/hmx
         h = hmx
      endif
      bb0 = bb
      bbv = bb
      bbv0 = bb
      h0 = h
      htp0 = htp
      zc = zcr
      zcmx = hmx-.5*h
      if (zc .gt. zcmx) zc = zcmx
      zc0 = zc

      b0 = b
      bv = b
      bv0 = b

      rho0 = rho
      t0 = t
      if (ws .gt. u) then
         hs0 = bs+bs
        else
         hs0 = (bs+bs)*ws/u
      endif

      do 510 i=1,msfm

      uap(i) = uab
      xp(i) = 1.+(i-1.)*dxpr
      xir = (xp(i)-1.)/xpr
      xiz = (xp(i)-1.-xpr)/xpr
      up(i) = u*sqrt(xir)
      wcp(i) = ws*(u-up(i))/u

      hp(i) = hs0 + (h-hs0)*xir
      zcp(i) = hs+hpr*sqrt(1.-xiz*xiz)
      zcmx = hmx-.5*hp(i)
      if (zcp(i) .gt. zcmx) zcp(i) = zcmx
      bbp(i) = bs + (bb-bs)*xir
      bp(i) = bsm + (b-bsm)*xir
      betap(i) = sqrt(bbp(i)*bbp(i)-bp(i)*bp(i))/sr3

      tccp(i) = 2.*(xp(i)-1.)/u
      xccp(i) = xp(i)
      if (tccp(i) .lt. tsd) then
         qint = .5*qs*tccp(i)
         qintp(i) = qint
         qint0 = qint
        else
         nxtr = min0(nxtr,i)
         qint = hqtcs
         qint0 = qint
         tim = tccp(i)
         timp(i) = tim
         bbxp(i) = .5*u*tsd
         bxp(i) = .9999*bbxp(i)
         betaxp(i) = sqrt(bbxp(i)*bbxp(i)-bxp(i)*bxp(i))/sr3
      endif
      rtcm = (1.-cm)/cm
      cmp(i) = 1./(1.+xir*xir*rtcm)
      cvp(i) = wmae*cmp(i)/(wms+(wmae-wms)*cmp(i))
      cmdap(i) = (1.-cmp(i))*cmdaa
      cmwp(i) = (1.-cmp(i))*cmwa
      rhop(i) = -1.
      tp(i) = -1.
      ugp(i) = 0.
      vgp(i) = 0.
      cmevp(i) = -1.
      cmwvp(i) = -1.
      wp(i) = -1.
      vp(i) = -1.
      vxp(i) = 0.
  510 continue
      rhop(msfm) = rho
      tp(msfm) = t
      cmwvp(msfm) = cmwv
      cmevp(msfm) = cmev

      cm0 = cm
      cmev0 = cmev
      cmw0 = cmw
      cmwv0 = cmwv
      cp0 = cp

      mnfm = mffm-msfm
      x = xp(msfm)
      wse = 0.

      vg = 0.
      vg0 = 0.
      wc = 0.
      wc0 = 0.
      ug = 0.
      ug0 = 0.

      fu = 0.
      fv = 0.
      fw = 0.
      ft = 0.

      r0 = r
      srug = .5*alfg*grav*(rho-rhoa)*bb*h*h
      sru0 = r*(u-(1.-cm)*uab) + srug
      delu = (rhoa/rho)*abs(uab-u)
      if (htp .gt. h) then
         ubs20 = uastr2
         ustr2 = uastr2 + cf1*delu*delu
        else
         umgs = (uastr/uab)*u
         ubs20 = umgs*umgs
         ustr2 = ubs20 + cf1*delu*delu
      endif
      uah = uafn(htp,z0,ala0,zl,hmx,zt,cu1,cu2)
      w = (urf/uah)* aa*xk*sqrt(ustr2)
      wp(msfm) = w

      if (ala .lt. 0.) then
         stby = 1.-rcf*10.*ala
        else
         stby = 1./(1.+rcf*10.*ala)
      endif
      atot = afa*stby
      rab = .5*sigb/atot
      va = atot*uab/(1.+rab*bb/sr3)
      vjp = aa*xk*delu
      v = sqrt(va*va+cf1*vjp*vjp)
      vp(msfm) = v

c  check for entry to plume or puff mode
      if (qint .lt. hqtcs) then

c  go to nearfield region calculation
         go to 600
        else
         idpf = 2
         nxi = msfm + 1
         rho0 = rho
         r = .25*qs*tsd/cm
         r0 = r
         rmi = 0.
         bbx = r/(rho*bb*h)
         bbx0 = bbx
         bbvx = bbx
         bbvx0 = bbx
         bx = .9999*bbx
         bx0 = bx
         bvx = bx
         bvx0 = bx
         sru0 = r*(u-(1.-cm)*uab)
         fug = 0.
         ug = 0.
         ug0 = 0.
         cm0 = cm
         cmw0 = cmw
         cmwv0 = cmwv
         cmev0 = cmev
         cp0 = cp
         t0 = t

         if (xffm .lt. (2.*xp(msfm))) xffm = 2.*xp(msfm)
         ngam = log10((xffm/xp(msfm))-1.) + 1.
         nstp = nssm*mnfm
         anstp = nstp
         gam = 10.**(ngam/anstp)
         dx = (gam-1.)*(xffm-xp(msfm))/((gam**nstp)-1.)
         dt = dx/u

c  go to transient puff dispersion mode
         go to 730
      endif



c =====================================
c =====================================
c  steady state plume dispersion mode
c  near field region calculation
c =====================================
c =====================================

c =============================
c  pool, jet, or stack release
c =============================

c ================
c  initialization
c ================


  600 nxi = msfm + 1
      wss = 0.
      if(xffm.lt.(2.*xp(msfm))) xffm = 2.*xp(msfm)

      angam = log10((xffm/xp(msfm))-1.) + 1.
      nstp = nssm*mnfm
      anstp = nstp
      gam = 10.**(angam/anstp)
      dx = (gam-1.) * (xffm-xp(msfm))/((gam**nstp)-1.)

c ===============================================
c  integrate steady state conservation equations
c ===============================================

      do 675 nx=nxi,mffm

      do 660 ns = 1,nssm

      do 636 i=1,11
  636 sum(i) = 0.
      dxxi(1) = .5*dx
      dxxi(2) = .5*dx
      dxxi(3) = dx

      dxrk(1) = dx/6.
      dxrk(2) = dx/3.
      dxrk(3) = dx/3.
      dxrk(4) = dx/6.

      do 655 k=1,4

      call slope

      do 650 j=1,11

      sum(j) = sum(j) + dxrk(k) * f(j)
      if (k .eq. 4) then

      dy(j) = sum(j)
      else
      dy(j) = dxxi(k) * f(j)

      xn = x + dxxi(k)

      endif

  650 continue

      call solve
      call thermo
      call eval
      call entran

  655 continue

      x = xn
      htp0 = htp
      r0 = r
      bb0 = bb
      b0 = b
      bbv0 = bbv
      bv0 = bv
      rho0 = rho
      h0 = h
      vg0 = vg
      wc0 = wc
      zc0 = zc
      t0 = t
      u0 = u
      uab0 = uab
      ubs20 = ubs2
      if (htp .gt. h) then
         alfg = 0.
         srug = 0.
        else
         if (rho .gt. rhoa) then
            alfg = .25
            srug = .5*alfg*grav*(rho-rhoa)*bb*h*h
           else
            alfg = 0.
            srug = 0.
         endif
      endif

      sru0 = r*u - r*(1.-cm)*uab + srug
      qint0 = qint

      cm0 = cm
      cmw0 = cmw
      cmwv0 = cmwv
      cmev0 = cmev
      cp0 = cp

      dx = gam*dx

  660 continue

      xccp(nx) = x
      tccp(nx) = (qint+qint)/qs

      call store

      if(qint .lt. hqtcs) then
          go to 675
        else
          idpf = 2
          nxtr = nx
          nxi = nx+1
          dt = dx/u
          rho0 = rho
          r = .25*qs*tsd/cm
          r0 = r
          rmi = 0.
          bbx = r/(rho*bb*h)
          bbx0 = bbx
          bbxp(nx) = bbx
          bbvx = bbx
          bbvx0 = bbx
          bx = .9999*bbx
          bx0 = bx
          bxp(nx) = bx
          bvx = bx
          bvx0 = bx
          betaxp(nx) = sqrt(bbx*bbx-bx*bx)/sr3
          sru0 = r*(u-(1.-cm)*uab)
          fv = bbx*fv
          fu = bbx*fu
          fw = bbx*fw
          fug = bbx*fug
          ft = bbx*ft

          ug = (bb/bbx)*vg
          ug0 = ug
          ugp(nx) = ug

          cm0 = cm
          cmw0 = cmw
          cmwv0 = cmwv
          cmev0 = cmev
          cp0 = cp
          t0 = t

c  go to transient puff dispersion mode
          go to 730
      endif

  675 continue


c  go to time averaged volume concentration calculation
      go to 900


c ==========================================================
c ==========================================================
c  instantaneous or short duration evaporating pool release
c ==========================================================
c ==========================================================

c ================
c  initialization
c ================

  700 nxi = 2
      nstp = nssm*mffm
      anstp = nstp
      ttst = .1*(anstp-1.)
      if(tffm .gt. ttst) then
         gam = (10.*tffm/(anstp-1.))**(1./(anstp-2.))
         gmd = 1.+.02/(gam-.98)

         do 705 i=1,5
         gam = (10.*tffm*gmd*(gam-1.)+1.)**(1./(anstp-1.))
         gmd = 1.
  705    continue

         dt = (gam-1.)*tffm/(gam**(anstp-1.)-1.)
        else
         gam = 1.
         dt = .1
      endif

      x = 0.
      xp(1) = 0.
      timp(1) = 0.
      wse = ws

      vg = 0.
      vg0 = 0.
      vgp(1) = 0.
      ug = 0.
      ug0 = 0.
      ugp(1) = 0.
      wc = 0.
      wc0 = 0.
      wcp(1) = 0.

      sft0 = 0.
      qint = 0.
      qint0 = 0.


      fv = 0.
      ft = 0.
      fu = 0.
      fw = 0.
      fug = 0.

      bb = bs
      bb0 = bb
      bbp(1) = bb
      bbv = bb
      bbv0 = bb
      b = .9*bs
      b0 = b
      bp(1) = b
      bv = .9*bs
      bv0 = bv
      bbx = bs
      bbx0 = bbx
      bbxp(1) = bbx
      bbvx = bbx
      bbvx0 = bbx
      bx = .9*bs
      bx0 = bx
      bxp(1) = bx
      bvx = bx
      bvx0 = bx
      betap(1) = sqrt(bb*bb-b*b)/sr3
      betaxp(1) = sqrt(bbx*bbx-bx*bx)/sr3

      z1 = sr3*aa*xk*xk*urf*dt
      z2 = sqrt(2.*z1*z0)
      he = (2.*z2)/log(1.+z2/z0)
      do 708 i=1,5
      fhz = log(1.+he/z0)
      shz = he + z0
      he = he - (shz*fhz - he - z1)/fhz
  708 continue
      w = he/(sr3*dt)
      wp(1) = w
      v = 0.
      vp(1) = v
      vx = 0.
      vxp(1) = vx

      h = hs
      h0 = h
      hp(1) = h
      htp = h
      htp0 = htp

      u = 0.
      u0 = u
      up(1) = u
      uab = 0.

      if(h .gt. 0.) then
      dh = h/5.
      h1 = -.5*dh
      h2 = 0.
      do 710 i=1,5
      h1 = h1 + dh
      h2 = h2 + dh
      u1 = uafn(h1,z0,ala0,zl,hmx,zt,cu1,cu2)
      u2 = uafn(h2,z0,ala0,zl,hmx,zt,cu1,cu2)
      uab = uab + 4.*u1 + 2.*u2
  710 continue
      uab = (uastr/xk)*(uab-u2)/30.
      al = ala
      if (al .lt. 0.)  then
         stby = 1.-rcf*10.*al
        else
         stby = 1./(1.+rcf*10.*al)
      endif
      uah = uafn(htp,z0,ala0,zl,hmx,zt,cu1,cu2)
      w = (urf/uah)*aa*xk*uastr
      wp(1) = w
      atot = afa*stby
      rab = .5*sigb/atot
      va = atot*uab/(1.+rab*bb/sr3)
      delu = (uab-u)*(rhoa/rho)
      vjp = aa*xk*delu
      v = sqrt(va*va+cf1*vjp*vjp)
      vp(1) = v
      vx = va
      vxp(1) = vx
      endif

      uab0 = uab
      uap(1) = uab

      r0 = .25*qtis
      rmi = r0
      sru0 = 0.

c ================================
c ================================
c  transient puff dispersion mode
c ================================
c ================================

c ============================================
c  pool, jet, stack, or instantaneous release
c ============================================


c ========================================
c   integrate puff conservation equations
c ========================================

  730 do 775 nx=nxi,mffm

      do 765 ns=1,nssm
      if (tim .lt. tsd) then
         wss = ws
         xstr = h/cf0
        else
         wss = 0.
      endif

      do 735 i=1,15
      sum(i) = 0.
  735 continue

      dxxi(1) = .5*dt
      dxxi(2) = .5*dt
      dxxi(3) = dt

      dxrk(1) = dt/6.
      dxrk(2) = dt/3.
      dxrk(3) = dt/3.
      dxrk(4) = dt/6.

      do 745 k=1,4
      call slopepf

      do 740 j=1,15

      sum(j) = sum(j) + dxrk(k)*f(j)

      if(k .eq. 4) then
      dy(j) = sum(j)
      else
      timn = tim + dxxi(k)
      dy(j) = dxxi(k)*f(j)
      endif

  740 continue

      call solvepf
      call thermo
      call evalpf
      call entran

  745 continue

      x = xn
      tim = timn
      htp0 = htp
      r0 = r

      bb0 = bb
      b0 = b
      bbv0 = bbv
      bv0 = bv
      bbx0 = bbx
      bbvx0 = bbvx
      bx0 = bx
      bvx0 = bvx
      rho0 = rho

      h0 = h
      vg0 = vg
      ug0 = ug
      wc0 = wc
      zc0 = zc
      t0 = t
      u0 = u
      uab0 = uab
      ubs20 = ubs2
      sru0 = r*(u-(1.-cm)*uab)

      cm0 = cm
      cmw0 = cmw
      cmwv0 = cmwv
      cmev0 = cmev
      cp0 = cp

      dt = gam*dt

  765 continue
      xccp(nx) = x
      tccp(nx) = tim

      call store

  775 continue

  900 continue


c =============
c =============
c  time averaged volume concentration
c =============
c =============


c   steady state calc of timp

      if (nxtr .gt. 61) then
         xptr = (hqtcs/qint)*(xp(61)-xp(1)) + xp(1)
         bxtr = bxs0 + xptr - xcc0
         itr = 61
        else
         xptr = xp(nxtr)
         bxtr = bbxp(nxtr)
         itr = nxtr - 1
      endif
      if (itr .gt. 0) then
         txt = xptr - xcc0 - xcc0
         txb = txt + xptr
         bxr = (bxtr-bxs0)/(xptr-xcc0)
         do 925 i=1,itr
         timp(i) = tsd*(xp(i)+txt)/txb
         bbxp(i) = bxs0 + bxr*(xccp(i)-xcc0)
         bxp(i) = .9999*bbxp(i)
         betaxp(i) = sqrt(bbxp(i)*bbxp(i)-bxp(i)*bxp(i))/sr3
  925    continue
      endif

      if (idspl .eq. 1) then
         do 930 i=1,5
         timp(i) = timp(12-i)
  930    continue
      endif

c  write instantaneous spatially averaged cloud parameters

      call edits

c  calculate and write time averaged volume concentration
c   concentration contour parameters
c   concentration in the zp(i) plane
c   maximum concentration along centerline

      call editcc

      go to 110
      end


c =====================
c =====================
c   end of calculation
c =====================
c =====================


c
c  *******  *     *  *     *   *****   *******  ***   *****   *     *   ******
c  *        *     *  **    *  *     *     *      *   *     *  **    *  *
c  *        *     *  * *   *  *           *      *   *     *  * *   *  *
c  ****     *     *  *  *  *  *           *      *   *     *  *  *  *   *****
c  *        *     *  *   * *  *           *      *   *     *  *   * *        *
c  *        *     *  *    **  *     *     *      *   *     *  *    **        *
c  *         *****   *     *   *****      *     ***   *****   *     *  ******
c



      function uafn(z,z0,ala0,zl,hmx,zt,cu1,cu2)

c  calculates the normalized ambient wind speed at height z

      if (z .lt. zt) then
         uatm = cu1*z + cu2*z*z
      else
         if (ala0 .lt. 0) then
         phmi = 1./sqrt(sqrt(1.-16.*zl*ala0))
         gus = -8.*ala0/(1.-phmi)
         xu = sqrt(1.+gus*z)
         xu0 = sqrt(1.+gus*z0)
         uatm = log(z/z0) - phmi*(z-z0)/hmx
     1      -2.*(1.-phmi)*(log((1.+xu)/(1.+xu0))
     1      -(sqrt(xu) - sqrt(xu0))/(gus*hmx))
         else
            uatm = log(z/z0) - (z-z0)/hmx + 5.*ala0 * zl * ((1.+zl/hmx)
     1      *log((z+zl)/(z0+zl)) - (z-z0)/hmx)
         endif
      endif
      uafn = uatm
      return
      end




c
c   ******  *     *  ******
c  *        *     *   *    *
c  *        *     *   *    *
c   *****   *     *   *****   *****
c        *  *     *   *    *
c        *  *     *   *    *
c  ******    *****   ******
c
c
c          ******    *****   *     *  *******  ***  *     *  *******   ******
c           *    *  *     *  *     *     *      *   **    *  *        *
c           *    *  *     *  *     *     *      *   * *   *  *        *
c           *****   *     *  *     *     *      *   *  *  *  ****      *****
c           *  *    *     *  *     *     *      *   *   * *  *              *
c           *   *   *     *  *     *     *      *   *    **  *              *
c           *    *   *****    *****      *     ***  *     *  *******  ******
c




      subroutine slope

c  calculates the gradients used in the steady state plume conservation
c   equations

      common sr3,third,f227,dbt,utst,wse,bs,bse,nssm
      common bb0,b0,r0,utm3,utc3,sft,htp,htp0,ncalc
      common h,bb,b,beta,r,g,vg,v,w,cm,u,t,rho,cv,xn,x
      common wma,cpa,grav,rhoa,rhos,pa,rr,qs,ws,hrf,urf
      common cf0,cf1,cri,tutrd,f17,f19,fu,ft,fv,sfu
      common cth,keval,uab,uab0,u0,cws,wss,ala0,zt,cu1,cu2

      common sr33,sctst,slup,sr2,srpi,cf00,sigb,tav0,uastr,ala
      common xk,aa,uastr2,rcf,afa,vh,stb,zl,hmx,ubs2,ubs20,xstr
      common idspl,nx,f(15),dy(15),betacp(61),sigp(61),cvpm(61),zm(61)
      common fcx(61),fcy(6,61),fcz(5,61),cvpt(6,4,61),bbcp(61),ccp(61)
      common alfg,alfgv,h0,tsd,qtcs,qtis,hs,us,stab,vg0,wcp(61)
      common zc,zc0,wc,wc0,sru0,rho0,bbv,bbv0,bv,bv0,dbbx,cmdap(61)
      common gw,fw,cp,facss,facp1,facp2,tbp,sfx,sfy,sfz

      common/var/vp(61),wp(61),hp(61),bbp(61),vgp(61),up(61),bp(61),
     1 rhop(61),cmp(61),tp(61),cvp(61),betap(61),xp(61),cmwvp(61),
     1 cmevp(61),uap(61),zcp(61),timp(61),bbxp(61),ugp(61),vxp(61),
     1 bxp(61),betaxp(61),qintp(61),tcld(61),cmwp(61),xccp(61),tccp(61)

      common za,ta,ua,z0,wms,cps,ts,nzpm,zp(5),
     1 as,xffm,tav,msfm,mnfm,mffm,tffm

      common tim,timn,vx,bbx,bbx0,gx,rmi,bbvx,bbvx0,
     1 ug,ug0,fug,phimi,phgam,idpf,nxtr,bx,bx0,bvx,bvx0,betax,
     1 qint,qint0,cm0,cmwv,cmwv0,cmev,cmev0,cp0,cpaa,cpwv,cpwl,
     1 cpsl,dhw,dhe,cmdaa,cmwa,rhosl,spa,spb,spc,spaw,spbw,t0,
     1 wmae,wmw,rhowl,cmw,cmw0,cmed,rh,tau0,tgon


c  mass equation

      if (x .ge. bse) then
         rqs = 0.
      else
         rqs = .25*qs/bse
      endif

      vhwb = sr3*(v*h + w*bb)
      f(1) = rhoa*vhwb + rqs

c  half-width equation

      f(2) = (sr3*(rhoa/rho)*v + vg)/u

c  b parameter equation

      f(3) = (vg*b)/(u*bb)

c  crosswind velocity equation

      f(4) = alfgv*grav*(rho-rhoa)*h*h
      f(10) = fv

c  temperature flux equation

      f(5) = ft

c  downwind velocity flux equation

      f(6) = fu

c  vertical velocity equation

      f(7) = -grav*(rho-rhoa)*bb*h
      f(11) = fw

c  center height equation

      f(8) = wc/u

c  released mass in cloud

      f(9) = 2.*rho*bb*h*cm

      return
      end





      subroutine slopepf

c  calculates the gradients used in the transient puff convervation
c    equations

      common sr3,third,f227,dbt,utst,wse,bs,bse,nssm
      common bb0,b0,r0,utm3,utc3,sft,htp,htp0,ncalc
      common h,bb,b,beta,r,g,vg,v,w,cm,u,t,rho,cv,xn,x
      common wma,cpa,grav,rhoa,rhos,pa,rr,qs,ws,hrf,urf
      common cf0,cf1,cri,tutrd,f17,f19,fu,ft,fv,sfu
      common cth,keval,uab,uab0,u0,cws,wss,ala0,zt,cu1,cu2

      common sr33,sctst,slup,sr2,srpi,cf00,sigb,tav0,uastr,ala
      common xk,aa,uastr2,rcf,afa,vh,stb,zl,hmx,ubs2,ubs20,xstr
      common idspl,nx,f(15),dy(15),betacp(61),sigp(61),cvpm(61),zm(61)
      common fcx(61),fcy(6,61),fcz(5,61),cvpt(6,4,61),bbcp(61),ccp(61)
      common alfg,alfgv,h0,tsd,qtcs,qtis,hs,us,stab,vg0,wcp(61)
      common zc,zc0,wc,wc0,sru0,rho0,bbv,bbv0,bv,bv0,dbbx,cmdap(61)
      common gw,fw,cp,facss,facp1,facp2,tbp,sfx,sfy,sfz

      common/var/vp(61),wp(61),hp(61),bbp(61),vgp(61),up(61),bp(61),
     1 rhop(61),cmp(61),tp(61),cvp(61),betap(61),xp(61),cmwvp(61),
     1 cmevp(61),uap(61),zcp(61),timp(61),bbxp(61),ugp(61),vxp(61),
     1 bxp(61),betaxp(61),qintp(61),tcld(61),cmwp(61),xccp(61),tccp(61)

      common za,ta,ua,z0,wms,cps,ts,nzpm,zp(5),
     1 as,xffm,tav,msfm,mnfm,mffm,tffm

      common tim,timn,vx,bbx,bbx0,gx,rmi,bbvx,bbvx0,
     1 ug,ug0,fug,phimi,phgam,idpf,nxtr,bx,bx0,bvx,bvx0,betax,
     1 qint,qint0,cm0,cmwv,cmwv0,cmev,cmev0,cp0,cpaa,cpwv,cpwl,
     1 cpsl,dhw,dhe,cmdaa,cmwa,rhosl,spa,spb,spc,spaw,spbw,t0,
     1 wmae,wmw,rhowl,cmw,cmw0,cmed,rh,tau0,tgon


c  mass and downwind cloud center equations

      if(tim .ge. tsd) then
          rqs = 0.
          wss = 0.
          atau = 0.
        else
          rqs =.25*qs
          wss = ws
          if (r .gt. 0.) then
             atau = rqs/r
            else
             atau = 0.
          endif
      endif

      vhwb = sr3*((vx*bb+v*bbx)*h + w*bb*bbx)
      f(1) = rhoa*vhwb + rqs
      f(2) = u-atau*x

c  half-width equation

      f(3) = sr3*(rhoa/rho)*v + vg

c  b parameter equation

      f(4) = (vg*b)/bb

c  half-length equation
      f(5) = sr3*(rhoa/rho)*vx + ug

c  bx parameter equation

      f(6) = (ug*bx)/bbx

c  crosswind velocity equations

      fgrv = alfgv*grav*(rho-rhoa)*h*h

      f(7) = fgrv*bbx
      f(13) = fv
      f(8) = fgrv*bb
      f(14) = fug

c  temperature flux equation

      f(9) = ft

c  downwind velocity flux equation

      f(10) = fu

c  vertical velocity equation

      f(11) = -grav*(rho-rhoa)*bb*bbx*h
      f(15) = fw

c  center height equation

      f(12) = wc

      return
      end





      subroutine solve

c  solves the steady state plume conservation equations

      common sr3,third,f227,dbt,utst,wse,bs,bse,nssm
      common bb0,b0,r0,utm3,utc3,sft,htp,htp0,ncalc
      common h,bb,b,beta,r,g,vg,v,w,cm,u,t,rho,cv,xn,x
      common wma,cpa,grav,rhoa,rhos,pa,rr,qs,ws,hrf,urf
      common cf0,cf1,cri,tutrd,f17,f19,fu,ft,fv,sfu
      common cth,keval,uab,uab0,u0,cws,wss,ala0,zt,cu1,cu2

      common sr33,sctst,slup,sr2,srpi,cf00,sigb,tav0,uastr,ala
      common xk,aa,uastr2,rcf,afa,vh,stb,zl,hmx,ubs2,ubs20,xstr
      common idspl,nx,f(15),dy(15),betacp(61),sigp(61),cvpm(61),zm(61)
      common fcx(61),fcy(6,61),fcz(5,61),cvpt(6,4,61),bbcp(61),ccp(61)
      common alfg,alfgv,h0,tsd,qtcs,qtis,hs,us,stab,vg0,wcp(61)
      common zc,zc0,wc,wc0,sru0,rho0,bbv,bbv0,bv,bv0,dbbx,cmdap(61)
      common gw,fw,cp,facss,facp1,facp2,tbp,sfx,sfy,sfz

      common/var/vp(61),wp(61),hp(61),bbp(61),vgp(61),up(61),bp(61),
     1 rhop(61),cmp(61),tp(61),cvp(61),betap(61),xp(61),cmwvp(61),
     1 cmevp(61),uap(61),zcp(61),timp(61),bbxp(61),ugp(61),vxp(61),
     1 bxp(61),betaxp(61),qintp(61),tcld(61),cmwp(61),xccp(61),tccp(61)

      common za,ta,ua,z0,wms,cps,ts,nzpm,zp(5),
     1 as,xffm,tav,msfm,mnfm,mffm,tffm

      common tim,timn,vx,bbx,bbx0,gx,rmi,bbvx,bbvx0,
     1 ug,ug0,fug,phimi,phgam,idpf,nxtr,bx,bx0,bvx,bvx0,betax,
     1 qint,qint0,cm0,cmwv,cmwv0,cmev,cmev0,cp0,cpaa,cpwv,cpwl,
     1 cpsl,dhw,dhe,cmdaa,cmwa,rhosl,spa,spb,spc,spaw,spbw,t0,
     1 wmae,wmw,rhowl,cmw,cmw0,cmed,rh,tau0,tgon


      r = r0 + dy(1)
      bbv = bbv0 + dy(2)
      bv = bv0 + dy(3)
      g = dy(4)
      sft = dy(5)
      sfu = dy(6)
      gw = dy(7)
      zc = zc0 + dy(8)
      zcmx = hmx - .5*h
      if (zc .gt. zcmx)  zc = zcmx
      if (zc .lt. 0.)  zc = 0.
      qint = qint0 + dy(9)
      sfy = dy(10)
      sfz = dy(11)

      return
      end





      subroutine solvepf

c  solves the transient puff conservation equations

      common sr3,third,f227,dbt,utst,wse,bs,bse,nssm
      common bb0,b0,r0,utm3,utc3,sft,htp,htp0,ncalc
      common h,bb,b,beta,r,g,vg,v,w,cm,u,t,rho,cv,xn,x
      common wma,cpa,grav,rhoa,rhos,pa,rr,qs,ws,hrf,urf
      common cf0,cf1,cri,tutrd,f17,f19,fu,ft,fv,sfu
      common cth,keval,uab,uab0,u0,cws,wss,ala0,zt,cu1,cu2

      common sr33,sctst,slup,sr2,srpi,cf00,sigb,tav0,uastr,ala
      common xk,aa,uastr2,rcf,afa,vh,stb,zl,hmx,ubs2,ubs20,xstr
      common idspl,nx,f(15),dy(15),betacp(61),sigp(61),cvpm(61),zm(61)
      common fcx(61),fcy(6,61),fcz(5,61),cvpt(6,4,61),bbcp(61),ccp(61)
      common alfg,alfgv,h0,tsd,qtcs,qtis,hs,us,stab,vg0,wcp(61)
      common zc,zc0,wc,wc0,sru0,rho0,bbv,bbv0,bv,bv0,dbbx,cmdap(61)
      common gw,fw,cp,facss,facp1,facp2,tbp,sfx,sfy,sfz

      common/var/vp(61),wp(61),hp(61),bbp(61),vgp(61),up(61),bp(61),
     1 rhop(61),cmp(61),tp(61),cvp(61),betap(61),xp(61),cmwvp(61),
     1 cmevp(61),uap(61),zcp(61),timp(61),bbxp(61),ugp(61),vxp(61),
     1 bxp(61),betaxp(61),qintp(61),tcld(61),cmwp(61),xccp(61),tccp(61)

      common za,ta,ua,z0,wms,cps,ts,nzpm,zp(5),
     1 as,xffm,tav,msfm,mnfm,mffm,tffm

      common tim,timn,vx,bbx,bbx0,gx,rmi,bbvx,bbvx0,
     1 ug,ug0,fug,phimi,phgam,idpf,nxtr,bx,bx0,bvx,bvx0,betax,
     1 qint,qint0,cm0,cmwv,cmwv0,cmev,cmev0,cp0,cpaa,cpwv,cpwl,
     1 cpsl,dhw,dhe,cmdaa,cmwa,rhosl,spa,spb,spc,spaw,spbw,t0,
     1 wmae,wmw,rhowl,cmw,cmw0,cmed,rh,tau0,tgon


      r = r0 + dy(1)
      xn = x + dy(2)
      bbv = bbv0 + dy(3)
      bv = bv0 + dy(4)
      bbvx = bbvx0 + dy(5)
      bvx = bvx0 + dy(6)
      g = dy(7)
      gx = dy(8)
      sft = dy(9)
      sfu = dy(10)
      gw = dy(11)
      zc = zc0 + dy(12)
      zcmx = hmx - .5*h
      if (zc .gt. zcmx) zc = zcmx
      if (zc .lt. 0.)  zc = 0.
      sfy = dy(13)
      sfx = dy(14)
      sfz = dy(15)


      return
      end





      subroutine thermo

c  calculates the thermodynamic properties

      common sr3,third,f227,dbt,utst,wse,bs,bse,nssm
      common bb0,b0,r0,utm3,utc3,sft,htp,htp0,ncalc
      common h,bb,b,beta,r,g,vg,v,w,cm,u,t,rho,cv,xn,x
      common wma,cpa,grav,rhoa,rhos,pa,rr,qs,ws,hrf,urf
      common cf0,cf1,cri,tutrd,f17,f19,fu,ft,fv,sfu
      common cth,keval,uab,uab0,u0,cws,wss,ala0,zt,cu1,cu2

      common sr33,sctst,slup,sr2,srpi,cf00,sigb,tav0,uastr,ala
      common xk,aa,uastr2,rcf,afa,vh,stb,zl,hmx,ubs2,ubs20,xstr
      common idspl,nx,f(15),dy(15),betacp(61),sigp(61),cvpm(61),zm(61)
      common fcx(61),fcy(6,61),fcz(5,61),cvpt(6,4,61),bbcp(61),ccp(61)
      common alfg,alfgv,h0,tsd,qtcs,qtis,hs,us,stab,vg0,wcp(61)
      common zc,zc0,wc,wc0,sru0,rho0,bbv,bbv0,bv,bv0,dbbx,cmdap(61)
      common gw,fw,cp,facss,facp1,facp2,tbp,sfx,sfy,sfz

      common/var/vp(61),wp(61),hp(61),bbp(61),vgp(61),up(61),bp(61),
     1 rhop(61),cmp(61),tp(61),cvp(61),betap(61),xp(61),cmwvp(61),
     1 cmevp(61),uap(61),zcp(61),timp(61),bbxp(61),ugp(61),vxp(61),
     1 bxp(61),betaxp(61),qintp(61),tcld(61),cmwp(61),xccp(61),tccp(61)

      common za,ta,ua,z0,wms,cps,ts,nzpm,zp(5),
     1 as,xffm,tav,msfm,mnfm,mffm,tffm

      common tim,timn,vx,bbx,bbx0,gx,rmi,bbvx,bbvx0,
     1 ug,ug0,fug,phimi,phgam,idpf,nxtr,bx,bx0,bvx,bvx0,betax,
     1 qint,qint0,cm0,cmwv,cmwv0,cmev,cmev0,cp0,cpaa,cpwv,cpwl,
     1 cpsl,dhw,dhe,cmdaa,cmwa,rhosl,spa,spb,spc,spaw,spbw,t0,
     1 wmae,wmw,rhowl,cmw,cmw0,cmed,rh,tau0,tgon


      if(idpf .eq. 2) then
        if(timn .gt. tsd) then
            sqs = .25*qs*tsd
         else
            sqs = .25*qs*timn
         endif
         cm = (rmi+sqs)/r
      else
         if(xn .gt. bse) then
            sqs = .5*qs
         else
            sqs = .25*qs*(bse+xn)/bse
         endif
         cm = sqs/r
      endif

      cv = (wmae*cm)/(wms+(wmae-wms)*cm)

      cmw = (1.-cm)*cmwa
      cmda = (1.-cm)*(1.-cmwa)
      cmwvt = cmw + (r0/r)*(cmwv0-cmw0)
      cmevt = cm + (r0/r)*(cmev0-cm0)
      cmwv = cmwvt
      cmev = cmevt
      cmwd = cmw-cmwv
      cmed = cm-cmev

      dhe0 = dhe + tbp*(cpsl-cps)
      dhw0 = dhw + 298.2*(cpwl-cpwv)

      cp = cmda*cpa + cmwv*cpwv + cmwd*cpwl + cmev*cps + cmed*cpsl

      if (sft .eq. 0.) then
         sfte = 0.
        else
         sfte = -cp0*(t0-ta)*abs(sft)/(abs(sft) + r0*cp0*abs(t0-ta))
      endif

      etrn = (1.-cm)*cpaa*ta + cm*cps*ts
     1 + (r0/r)*(cp0*t0-(1.-cm0)*cpaa*ta-cm0*cps*ts + sfte)

      t = etrn/cp

      alfm = wmae*(cmda/wma + cmwv/wmw + cmev/wms)
      fft = exp(spa-spb/(t+spc))
      ggt = exp(spaw-spbw/t)
      pw = (wmae*cmw)/(alfm*wmw)
      pe = (wmae*cm)/(alfm*wms)

      ie = 0
      iw = 0
      if (cmev0 .eq. cm0) then
         if (fft .gt. pe) ie = 1
      endif
      if (cmwv0 .eq. cmw0) then
         if (ggt .gt. pw) iw = 1
      endif
      if ((ie+iw) .eq. 2) go to 70

      if (ie .eq. 1)  go to 5
      ffpr = (spb*fft)/((t+spc)*(t+spc))
      if (fft .gt. 1.)  then
         fft = 1. + spa*(t-tbp)/(tbp+spc)
         ffpr = spa/(tbp+spc)
      endif
      alfs = wms*(cmda/wma + cmwv/wmw)
      ab2 = .5*((1.-fft)/ffpr + (dhe0/cp)*(alfs+cmevt))
      ac = dhe0*(alfs - (1.-fft)*cmevt)/(cp*ffpr)
      dlti = ab2 - sqrt(ab2*ab2 + ac)
      dltm = dhe0*(cmevt-cm)/cp
      dlti = amax1(dltm,dlti)
      t = t + dlti
      fft = exp(spa - spb/(t+spc))
      ggt = exp(spaw - spbw/t)
    5 continue

      do 65 irep = 1,2

      tstr = t
      cmwvs = cmwv
      cmevs = cmev
      icyc = 0

   10 do 30 i=1,15
      if (ie .eq. 0) then
        cmwvm = amin1(cmwvs,cmw)
        alfs = wms*(cmda/wma + cmwvm/wmw)
        fftt = cm/(cm+alfs)
        if (fft .lt. fftt) then
           cmevs = alfs*fft/(1.-fft)
           cmevspr = spb*cmevs/((1.-fft)*(tstr+spc)*(tstr+spc))
          else
           tesrt = spb/(spa-log(fftt)) - spc
           cmevspr = spb*cm/((1.-fftt)*(tesrt+spc)*(tesrt+spc))
           cmevs = cmevspr*(tstr-tesrt) + cm
        endif
       else
        cmevs = cm
        cmevspr = 0.
      endif

   20 if (iw .eq. 0) then
        cmevm = amin1(cmevs,cm)
        alfw = wmw*(cmda/wma + cmevm/wms)
        ggtt = cmw/(cmw+alfw)
        if (ggt .lt. ggtt) then
           cmwvs = alfw*ggt/(1.-ggt)
           cmwvspr = spbw*cmwvs/((1.-ggt)*tstr*tstr)
          else
           twsrt = spbw/(spaw-log(ggtt))
           cmwvspr = spbw*cmw/((1.-ggtt)*twsrt*twsrt)
           cmwvs = cmwvspr*(tstr-twsrt) + cmw
        endif
       else
        cmwvs = cmw
        cmwvspr = 0.
      endif

      cmwvm = amin1(cmwvs,cmw)
      cmevm = amin1(cmevs,cm)

      cmwds = cmw-cmwvm
      cmeds = cm-cmevm
      cpstr = cmda*cpa + cmwvm*cpwv + cmwds*cpwl + cmevm*cps +
     1 cmeds*cpsl
      tstra = (etrn + dhw0*(cmwvt-cmwvs) + dhe0*(cmevt-cmevs))/cpstr

      gfnc = tstr - tstra
      dhwt = dhw0 + tstra*(cpwv-cpwl)
      dhet = dhe0 + tstra*(cps-cpsl)
      gfncp = 1. + (dhwt*cmwvspr + dhet*cmevspr)/cpstr
      tstr1 = tstr - gfnc/gfncp

      dtstr = abs(tstr1-tstra)
      tstr = tstr1

      fft = exp(spa-spb/(tstr+spc))
      ggt = exp(spaw-spbw/tstr)

      if (dtstr .lt. .001) go to 40
   30 continue

   40 cmwvm = amin1(cmwvs,cmw)
      cmevm = amin1(cmevs,cm)
      alfm = wmae*(cmda/wma+cmwvm/wmw+cmevm/wms)

      if (ie .eq. 1) then
         cmev = cm
        else
         cmevs = alfm*wms*fft/wmae
         if (cmevs .gt. cm) then
            cmev = cm
            ie = 1
            if (iw .eq. 0) icyc = 1
           else
            cmev = cmevs
         endif
      endif
      if (iw .eq. 1) then
         cmwv = cmw
        else
         cmwvs = alfm*wmw*ggt/wmae
         if (cmwvs .gt. cmw) then
            cmwv = cmw
            iw = 1
            if (ie .eq. 0) icyc = 1
           else
            cmwv = cmwvs
         endif
      endif

      cmwd = cmw-cmwv
      cmed = cm-cmev
      alfm = (cmda/wma + cmwv/wmw + cmev/wms)*wmae
      cp = cmda*cpa + cmwv*cpwv + cmwd*cpwl + cmev*cps + cmed*cpsl
      t = (etrn+dhw0*(cmwvt-cmwv) + dhe0*(cmevt-cmev))/cp

      if (ie .eq. 1) then
         fft = exp(spa-spb/(t+spc))
         cmevsc = (alfm*wms*fft)/wmae
         if (cmevsc .lt. cm) then
            ie = 0
            icyc = 1
         endif
      endif

      if (iw .eq. 1) then
         ggt = exp(spaw-spbw/t)
         cmwvsc = (alfm*wmw*ggt)/wmae
         if (cmwvsc .lt. cmw) then
            iw = 0
            icyc = 1
         endif
      endif

      if (icyc .eq. 0) go to 70
   65 continue

   70 betm = (rhoa/rhowl)*cmwd + (rhoa/rhosl)*cmed
      rho = rhoa*ta/(alfm*t + betm*ta)

      return
      end





      subroutine eval

c  calculates the steady state plume velocities and height

      common sr3,third,f227,dbt,utst,wse,bs,bse,nssm
      common bb0,b0,r0,utm3,utc3,sft,htp,htp0,ncalc
      common h,bb,b,beta,r,g,vg,v,w,cm,u,t,rho,cv,xn,x
      common wma,cpa,grav,rhoa,rhos,pa,rr,qs,ws,hrf,urf
      common cf0,cf1,cri,tutrd,f17,f19,fu,ft,fv,sfu
      common cth,keval,uab,uab0,u0,cws,wss,ala0,zt,cu1,cu2

      common sr33,sctst,slup,sr2,srpi,cf00,sigb,tav0,uastr,ala
      common xk,aa,uastr2,rcf,afa,vh,stb,zl,hmx,ubs2,ubs20,xstr
      common idspl,nx,f(15),dy(15),betacp(61),sigp(61),cvpm(61),zm(61)
      common fcx(61),fcy(6,61),fcz(5,61),cvpt(6,4,61),bbcp(61),ccp(61)
      common alfg,alfgv,h0,tsd,qtcs,qtis,hs,us,stab,vg0,wcp(61)
      common zc,zc0,wc,wc0,sru0,rho0,bbv,bbv0,bv,bv0,dbbx,cmdap(61)
      common gw,fw,cp,facss,facp1,facp2,tbp,sfx,sfy,sfz

      common/var/vp(61),wp(61),hp(61),bbp(61),vgp(61),up(61),bp(61),
     1 rhop(61),cmp(61),tp(61),cvp(61),betap(61),xp(61),cmwvp(61),
     1 cmevp(61),uap(61),zcp(61),timp(61),bbxp(61),ugp(61),vxp(61),
     1 bxp(61),betaxp(61),qintp(61),tcld(61),cmwp(61),xccp(61),tccp(61)

      common za,ta,ua,z0,wms,cps,ts,nzpm,zp(5),
     1 as,xffm,tav,msfm,mnfm,mffm,tffm

      common tim,timn,vx,bbx,bbx0,gx,rmi,bbvx,bbvx0,
     1 ug,ug0,fug,phimi,phgam,idpf,nxtr,bx,bx0,bvx,bvx0,betax,
     1 qint,qint0,cm0,cmwv,cmwv0,cmev,cmev0,cp0,cpaa,cpwv,cpwl,
     1 cpsl,dhw,dhe,cmdaa,cmwa,rhosl,spa,spb,spc,spaw,spbw,t0,
     1 wmae,wmw,rhowl,cmw,cmw0,cmed,rh,tau0,tgon


c  coupled velocity, height and average ambient velocity calculation

c   calculate downwind velocity
      u1 = (uastr/xk)*uafn(htp0,z0,ala0,zl,hmx,zt,cu1,cu2)
      u = u0
      bb = bb0+bbv-bbv0
      h = r/(rho*u*bb)
      rex = (rho0/rho)**(h/(h+bb+bb))

      do 1050 i=1,11
      gex = h/(h+bb+bb)
      if (u0 .gt. uab0) then
         rex0 = rho0*u0/(rho*u)
        else
         rex0 = rho0/rho
      endif
      rex = sqrt(rex*(rex0**gex))
      bb = (bb0+bbv-bbv0)*rex
      b = (b0+bv-bv0)*rex
      h = r/(rho*u*bb)
      if (h .gt. (bb+bb)) then
         bb = sqrt(r/(2.*rho*u))
         h = bb+bb
         rex = sqrt(rex0)
      endif
      hhf = .5*h
      if (zc .gt. hhf) then
         htp = zc + hhf
        else
         htp = h
      endif

      htph = .5*(htp0 + htp)
      u12 = (uastr/xk)*uafn(htph,z0,ala0,zl,hmx,zt,cu1,cu2)
      u2 = (uastr/xk)*uafn(htp,z0,ala0,zl,hmx,zt,cu1,cu2)
      uab = (htp0*uab0+(u1+4.*u12+u2)*(htp-htp0)/6.)/htp
      if (uab .gt. u2)  uab = u2

      if (sfu .eq. 0.) then
         sfue = 0.
        else
         sfue = -r0*(u0-uab0)*abs(sfu)/(abs(sfu) + r0*abs(u0-uab0))
      endif

      ut0 = uab*(1.-cm) + (sru0 + sfue)/r
      utg3 = alfg * grav * (rho-rhoa)*r/(2.*bb*rho*rho)

      if (utg3 .le. 0.) then
         ut2 = ut0
         go to 1040
      endif

 1015 utm3 = f227 * ut0 * ut0 * ut0
      utc3 = utg3 - utm3

      if (utc3 .gt. utm3) then
         ut2 = (ut0+ut0)/3.

        ut2 = amin1(ut2,u0)

         go to 1040
      endif

 1020 angl = acos(-utc3/utm3)
      ut2 = third*(ut0+2.*ut0*cos(third*angl))

 1040 ut2 = (u*ut2*ut2)**.333333
      dluk = abs(u-ut2)/sqrt(u*ut2)
      u = ut2
      h = r/(rho*u*bb)
      if (dluk .lt. .001) go to 1052
 1050 continue
 1052 continue

      if ((h .lt. (zc+zc)) .and. ((alfg*rho) .gt. (alfg*rhoa))) then
         h = zc + zc
         u = r/(rho*bb*h)
      endif

      if (h .gt. hmx) h = hmx
      hhf = .5*h
      zcmx = hmx - hhf
      if (zc .gt. zcmx)  zc = zcmx
      if (zc .gt. hhf) then
         htp = zc + hhf
        else
         htp = h
      endif
      htph = .5*(htp0+htp)
      u12 = (uastr/xk)*uafn(htph,z0,ala0,zl,hmx,zt,cu1,cu2)
      u2 = (uastr/xk)*uafn(htp,z0,ala0,zl,hmx,zt,cu1,cu2)
      uab = (htp0*uab0+(u1+4.*u12+u2)*(htp-htp0)/6.)/htp
      if (uab .gt. u2)  uab = u2

      dbbx = 0.
      if(utc3 .gt. utm3) then
        if(keval .eq. 4) then
         utst = 3.
         ru3 = (27./4.)*utg3/(ut0*ut0*ut0)
         bb = ru3*bb
         b = ru3*b
         dbbx = bb*(ru3-1.)/ru3
        endif
      endif
      beta = sqrt(bb*bb-b*b)/sr3

c  calculate crosswind velocities

      if (xn .gt. bse) then
         if (htp .gt. h) then
            vg = 0.
            if (wc0 .eq. 0.) then
               ew = 1.
              else
               ew = r0*abs(wc0)/(r0*abs(wc0) + abs(sfz))
            endif
            wc = ew*(gw + r0*wc0)/r
           else
            if (rho .gt. rhoa) then
               if (vg0 .eq. 0.) then
                  ev = 1.
                  vg0 = -2.*wc0*bb0/h0
                 else
                  ev = r0*vg0/(r0*vg0 + abs(sfy))
               endif
               vg = ev*(g + r0*vg0)/r
               wc = -vg*zc/bb
              else
               if (wc0 .eq. 0.) then
                  ew = 1.
                 else
                  ew = r0*abs(wc0)/(r0*abs(wc0) + abs(sfz))
               endif
               wc = ew*(gw + r0*wc0)/r
               vg = 0.
            endif
         endif
        else
         if (rho .gt. rhoa) then
            if (vg0 .eq. 0.) then
               ev = 1.
              else
               ev = r0*vg0/(r0*vg0 + abs(sfy))
            endif
            vg = ev*(g + r0*vg0)/r
           else
            vg = 0.
         endif
         wc = 0.
      endif

      return
      end





      subroutine evalpf

c  calculates the transient puff velocities and height

      common sr3,third,f227,dbt,utst,wse,bs,bse,nssm
      common bb0,b0,r0,utm3,utc3,sft,htp,htp0,ncalc
      common h,bb,b,beta,r,g,vg,v,w,cm,u,t,rho,cv,xn,x
      common wma,cpa,grav,rhoa,rhos,pa,rr,qs,ws,hrf,urf
      common cf0,cf1,cri,tutrd,f17,f19,fu,ft,fv,sfu
      common cth,keval,uab,uab0,u0,cws,wss,ala0,zt,cu1,cu2

      common sr33,sctst,slup,sr2,srpi,cf00,sigb,tav0,uastr,ala
      common xk,aa,uastr2,rcf,afa,vh,stb,zl,hmx,ubs2,ubs20,xstr
      common idspl,nx,f(15),dy(15),betacp(61),sigp(61),cvpm(61),zm(61)
      common fcx(61),fcy(6,61),fcz(5,61),cvpt(6,4,61),bbcp(61),ccp(61)
      common alfg,alfgv,h0,tsd,qtcs,qtis,hs,us,stab,vg0,wcp(61)
      common zc,zc0,wc,wc0,sru0,rho0,bbv,bbv0,bv,bv0,dbbx,cmdap(61)
      common gw,fw,cp,facss,facp1,facp2,tbp,sfx,sfy,sfz

      common/var/vp(61),wp(61),hp(61),bbp(61),vgp(61),up(61),bp(61),
     1 rhop(61),cmp(61),tp(61),cvp(61),betap(61),xp(61),cmwvp(61),
     1 cmevp(61),uap(61),zcp(61),timp(61),bbxp(61),ugp(61),vxp(61),
     1 bxp(61),betaxp(61),qintp(61),tcld(61),cmwp(61),xccp(61),tccp(61)

      common za,ta,ua,z0,wms,cps,ts,nzpm,zp(5),
     1 as,xffm,tav,msfm,mnfm,mffm,tffm

      common tim,timn,vx,bbx,bbx0,gx,rmi,bbvx,bbvx0,
     1 ug,ug0,fug,phimi,phgam,idpf,nxtr,bx,bx0,bvx,bvx0,betax,
     1 qint,qint0,cm0,cmwv,cmwv0,cmev,cmev0,cp0,cpaa,cpwv,cpwl,
     1 cpsl,dhw,dhe,cmdaa,cmwa,rhosl,spa,spb,spc,spaw,spbw,t0,
     1 wmae,wmw,rhowl,cmw,cmw0,cmed,rh,tau0,tgon


c  coupled velocity, height and average ambient velocity calculation

      u1 = (uastr/xk)*uafn(htp0,z0,ala0,zl,hmx,zt,cu1,cu2)
      gey = bbx0*h0/(h0*(bbx0+bb0)+2.*bb0*bbx0)
      gex = (bb0/bbx0)*gey
      rey = (rho0/rho)**gey
      rex = (rho0/rho)**gex
      bb = (bb0+bbv-bbv0)*rey
      b = (b0+bv-bv0)*rey
      bbx = (bbx0+bbvx-bbvx0)*rex
      bx = (bx0+bvx-bvx0)*rex

      beta = sqrt(bb*bb-b*b)/sr3
      betax = sqrt(bbx*bbx-bx*bx)/sr3

      h = r/(rho*bbx*bb)
      if(h .gt. hmx) h = hmx

      hhf = .5*h
      zcmx = hmx-hhf

      if(zc .gt. zcmx) zc = zcmx

      if(zc .gt. hhf) then

          htp = zc+hhf
        else
          htp = h
      endif

      htph = .5*(htp0+htp)
      u12 = (uastr/xk)*uafn(htph,z0,ala0,zl,hmx,zt,cu1,cu2)
      u2 = (uastr/xk)*uafn(htp,z0,ala0,zl,hmx,zt,cu1,cu2)
      uab = (htp0*uab0+(u1+4.*u12+u2)*(htp-htp0)/6.)/htp

      if (sfu .eq. 0.) then
         sfue = 0.
        else
         sfue = -r0*(u0-uab0)*abs(sfu)/(abs(sfu) + r0*abs(u0-uab0))
      endif

      u = (1.-cm)*uab + (sru0+sfue)/r

c  calculate crosswind velocities

      if(htp .gt. h) then
          vg = 0.
          ug = 0.
          if (wc0 .eq. 0.) then
             ew = 1.
            else
             ew = (r0*abs(wc0))/(r0*abs(wc0) + abs(sfz))
          endif
          wc = ew*(gw + r0*wc0)/r
        else
          if(rho .gt. rhoa) then
            if (vg0 .eq. 0.) then
               ev = 1.
               rbxy = bb0*bbx0/(bb0*bb0+bbx0*bbx0)
               if (h0 .gt. 0.)  vg0 = -2.*rbxy*bbx0*wc0/h0
              else
               ev = (r0*vg0)/(r0*vg0 + abs(sfy))
            endif
            vg = ev*(g + r0*vg0)/r
            if (ug0 .eq. 0.) then
               eu = 1.
               rbxy = bb0*bbx0/(bb0*bb0+bbx0*bbx0)
               if (h0 .gt. 0.)  ug0 = -2.*rbxy*bb0*wc0/h0
              else
               eu = (r0*ug0)/(r0*ug0 + abs(sfx))
            endif
            ug = eu*(gx + r0*ug0)/r
            wc = -(vg/bb+ug/bbx)*zc
          else
            if (wc0 .eq. 0.) then
               ew = 1.
              else
               ew = (r0*abs(wc0))/(r0*abs(wc0) + abs(sfz))
            endif
            wc = ew*(gw + r0*wc0)/r
            vg = 0.
            ug = 0.
          endif
      endif

      return
      end





      subroutine entran

c  calculates the entrainment rates and surface fluxes

      common sr3,third,f227,dbt,utst,wse,bs,bse,nssm
      common bb0,b0,r0,utm3,utc3,sft,htp,htp0,ncalc
      common h,bb,b,beta,r,g,vg,v,w,cm,u,t,rho,cv,xn,x
      common wma,cpa,grav,rhoa,rhos,pa,rr,qs,ws,hrf,urf
      common cf0,cf1,cri,tutrd,f17,f19,fu,ft,fv,sfu
      common cth,keval,uab,uab0,u0,cws,wss,ala0,zt,cu1,cu2

      common sr33,sctst,slup,sr2,srpi,cf00,sigb,tav0,uastr,ala
      common xk,aa,uastr2,rcf,afa,vh,stb,zl,hmx,ubs2,ubs20,xstr
      common idspl,nx,f(15),dy(15),betacp(61),sigp(61),cvpm(61),zm(61)
      common fcx(61),fcy(6,61),fcz(5,61),cvpt(6,4,61),bbcp(61),ccp(61)
      common alfg,alfgv,h0,tsd,qtcs,qtis,hs,us,stab,vg0,wcp(61)
      common zc,zc0,wc,wc0,sru0,rho0,bbv,bbv0,bv,bv0,dbbx,cmdap(61)
      common gw,fw,cp,facss,facp1,facp2,tbp,sfx,sfy,sfz

      common/var/vp(61),wp(61),hp(61),bbp(61),vgp(61),up(61),bp(61),
     1 rhop(61),cmp(61),tp(61),cvp(61),betap(61),xp(61),cmwvp(61),
     1 cmevp(61),uap(61),zcp(61),timp(61),bbxp(61),ugp(61),vxp(61),
     1 bxp(61),betaxp(61),qintp(61),tcld(61),cmwp(61),xccp(61),tccp(61)

      common za,ta,ua,z0,wms,cps,ts,nzpm,zp(5),
     1 as,xffm,tav,msfm,mnfm,mffm,tffm

      common tim,timn,vx,bbx,bbx0,gx,rmi,bbvx,bbvx0,
     1 ug,ug0,fug,phimi,phgam,idpf,nxtr,bx,bx0,bvx,bvx0,betax,
     1 qint,qint0,cm0,cmwv,cmwv0,cmev,cmev0,cp0,cpaa,cpwv,cpwl,
     1 cpsl,dhw,dhe,cmdaa,cmwa,rhosl,spa,spb,spc,spaw,spbw,t0,
     1 wmae,wmw,rhowl,cmw,cmw0,cmed,rh,tau0,tgon



c  vertical entrainment

      ubar = .5*ug
      urho = ubar*rhoa/rho
      vbar = .5*vg
      vrho = vbar*rhoa/rho
      delu = (uab-u) * (rhoa/rho)
      cf = uastr/uab
      umgs = cf * u
      ugmgs = cf * ubar
      vmgs = cf * vbar

      uhs2 = cf1*(delu*delu + vrho*vrho)

      if (htp .gt. h) then
         ubsi2 = uastr2 + uhs2
        else
         ubsi2 = umgs*umgs + vmgs*vmgs + cws*wss*uab
      endif
      ubs2 = ubsi2
      if (idspl .eq. 1)  then
         if (xn .gt. bse)  then
         ubs2 = ubsi2 + (ubs20-ubsi2)*exp((bse-xn)/xstr)
         endif
      endif
      if (idspl .eq. 4)  then
         if ((qs*timn) .gt. qtcs)  then
         ubs2 = ubsi2 + (ubs20-ubsi2)*exp((tsd-timn)*u/xstr)
         endif
      endif
      ubs = sqrt(ubs2)

      vh = cf*ubs
      tg = tgon*ta + (1.-tgon)*t
      if (htp .gt. h)  then
         uts2 = 0.
        else
         if (tg .gt. t) then
            tgt = .5*(tg+t)
            uts2 = ((cth*grav*(tg-t)*vh*h/tgt)**tutrd)
           else
            uts2 = 0.
         endif
      endif


 2045 ustr2 = ubs2 + uhs2 + uts2
      ustr = sqrt(ustr2)

      galc = cri*grav*(rho-rhoa)/rho
      if (galc .lt. 0.) galc = 0.
      alah = ala0/(1.+htp/zl)
      al = (alah*uastr2+galc)/ustr2
      if (al .lt. 0.) then
          phiit = 1./sqrt(1.-16.*htp*al)
      else
         phiit = 1. + 5.*htp*al
      endif

      fpr = 1. - htp/hmx
      fprb = 0.
      phiitb = 1.
      htst = 1.01*h
      if (htp .gt. htst) then
         hbt = htp - h
         fprb = 1. - hbt/hmx
         if (al .lt. 0.) then
            phiitb = 1./sqrt(1. - 16.*hbt*al)
           else
            phiitb = 1. + 5.*hbt*al
         endif
      endif
      uah = uafn(htp,z0,ala0,zl,hmx,zt,cu1,cu2)
      w = (urf/uah)*aa*xk*ustr*(fpr/phiit + fprb/phiitb)

c  horizontal entrainment

      al = ala
      if(al .lt. 0.) then
         stby = 1.-rcf*10.*al
      else
         stby = 1./(1.+rcf*10.*al)
      endif

      atot = afa*stby
      rab = .5*sigb/atot
      va = atot*uab/(1.+rab*bb/sr3)
      vjp = aa*xk*delu
      v = sqrt(va*va + cf1*vjp*vjp)

      if (zc .gt. (.5*h)) then
         sig = .5*h/sr3
        else
         sig = (h-zc)/sr3
      endif
      zrf = zc+.5*sig
      frzr = zrf/(zrf+z0)
      fgr = 1. -zrf/hmx
      if(stb .lt. 0.) then
          phmr = phimi + (1.-phimi)/sqrt(1.+phgam*zrf)
        else
          phmr = 1. + 5.*ala0*zrf/(1.+zrf/zl)
      endif
      vxs = .6*uastr*frzr*phmr*fgr/xk
      vax = atot*uab/(1.+rab*bbx/sr3)
      vx = sqrt(vax*vax+vxs*vxs)

c  velocity and temperature fluxes

      if (ala .lt. 0.) then
         phiit = 1./sqrt(1.-16.*htp*ala)
        else
         phiit = 1. + 5.*htp*ala
      endif
      umhs2 = cf1*abs(delu)*delu
      ugmhs2 = cf1*abs(urho)*urho
      vmhs2 = cf1*abs(vrho)*vrho
      wmhs2 = cf1*abs(wc)*wc
      rhoef = rho
      if(idpf .eq. 2) rhoef = rho*bbx
      if(htp .gt. h) then
         fu = rhoef*(bb+bb+h)*umhs2
         fug = 0.
         fv = 0.
         fw = -rhoef*h*wmhs2
         ft = 0.
      else
         fu = -rhoef*(bb*(umgs*umgs-uastr2)-(bb+h)*umhs2)
         fug = -rhoef*bb*(ugmgs*ugmgs+ugmhs2)
         fv = -rhoef*bb*(vmgs*vmgs+vmhs2)
         fw = 0.
         ft = rhoef*bb*vh*cp*(tg-t)
      endif

      return

      end





      subroutine store

c  stores the output variables

      common sr3,third,f227,dbt,utst,wse,bs,bse,nssm
      common bb0,b0,r0,utm3,utc3,sft,htp,htp0,ncalc
      common h,bb,b,beta,r,g,vg,v,w,cm,u,t,rho,cv,xn,x
      common wma,cpa,grav,rhoa,rhos,pa,rr,qs,ws,hrf,urf
      common cf0,cf1,cri,tutrd,f17,f19,fu,ft,fv,sfu
      common cth,keval,uab,uab0,u0,cws,wss,ala0,zt,cu1,cu2

      common sr33,sctst,slup,sr2,srpi,cf00,sigb,tav0,uastr,ala
      common xk,aa,uastr2,rcf,afa,vh,stb,zl,hmx,ubs2,ubs20,xstr
      common idspl,nx,f(15),dy(15),betacp(61),sigp(61),cvpm(61),zm(61)
      common fcx(61),fcy(6,61),fcz(5,61),cvpt(6,4,61),bbcp(61),ccp(61)
      common alfg,alfgv,h0,tsd,qtcs,qtis,hs,us,stab,vg0,wcp(61)
      common zc,zc0,wc,wc0,sru0,rho0,bbv,bbv0,bv,bv0,dbbx,cmdap(61)
      common gw,fw,cp,facss,facp1,facp2,tbp,sfx,sfy,sfz

      common/var/vp(61),wp(61),hp(61),bbp(61),vgp(61),up(61),bp(61),
     1 rhop(61),cmp(61),tp(61),cvp(61),betap(61),xp(61),cmwvp(61),
     1 cmevp(61),uap(61),zcp(61),timp(61),bbxp(61),ugp(61),vxp(61),
     1 bxp(61),betaxp(61),qintp(61),tcld(61),cmwp(61),xccp(61),tccp(61)

      common za,ta,ua,z0,wms,cps,ts,nzpm,zp(5),
     1 as,xffm,tav,msfm,mnfm,mffm,tffm

      common tim,timn,vx,bbx,bbx0,gx,rmi,bbvx,bbvx0,
     1 ug,ug0,fug,phimi,phgam,idpf,nxtr,bx,bx0,bvx,bvx0,betax,
     1 qint,qint0,cm0,cmwv,cmwv0,cmev,cmev0,cp0,cpaa,cpwv,cpwl,
     1 cpsl,dhw,dhe,cmdaa,cmwa,rhosl,spa,spb,spc,spaw,spbw,t0,
     1 wmae,wmw,rhowl,cmw,cmw0,cmed,rh,tau0,tgon


      xp(nx) = x
      bbp(nx) = bb
      bp(nx) = b
      vgp(nx) = vg
      cmp(nx) = cm
      tp(nx) = t
      rhop(nx) = rho
      up(nx) = u
      hp(nx) = h
      cvp(nx) = cv
      betap(nx) = beta
      wp(nx) = w
      vp(nx) = v
      cmdap(nx) = (1.-cm)*cmdaa
      cmwp(nx) = cmw
      cmwvp(nx) = cmwv
      cmevp(nx) = cmev
      uap(nx) = uab
      wcp(nx) = wc
      zcp(nx) = zc
      qintp(nx) = qint
      timp(nx) = tim
      bbxp(nx) = bbx
      bxp(nx) = bx
      betaxp(nx) = betax
      ugp(nx) = ug
      vxp(nx) = vx

      return
      end





      subroutine errint(x,erf,erfc)

c  calculates the error function

c     cic               errint         c3.2-020
      implicit real*8(a-h,o-z)
      real*4 x,erf,erfc
c     error,probability,and related functions
c     journal of research national bureau of standards  b-math
c     vol.74b  no.3  july-sept. 1970   page 211-224
c     toler=two**(-nbm)   nbm=48
c     c1=two**((nbc-one)/two)   nbc=11
c     ulcf=cons*c1    cons=.83
c     scf=two**(c1**2-nbc)
      data trrtpi,c1,ulcf/1.1283791670955,32.,26.56/
      data one,two,four,ulps/1.,2.,4.,1./
c     data toler,scf/3.5527136788005e-15,8.7777985100258e304/
      data toler,scf/3.5527136788005e-15,3.4028235e+38/
      if(x) 2,1,2
    1 erf=0.
      erfc=one
      return
    2 y=abs(x)
      ysq=y**2
      if(y-ulps)3,3,4
    4 if(y-ulcf) 10,10,11
   11 erf=one
      erfc=0.
      go to 7
    3 sum=0.
      dn=one
      tn=one
      pwr=ysq+ysq
    6 dn=dn+two
      tn=pwr*tn/dn
      sum=tn+sum
      if(tn-toler) 5,6,6
    5 erf=(sum+one)*trrtpi*y*exp(-ysq)
      erfc=one-erf
    7 if(x)8,9,9
    8 erf=-erf
      erfc=two-erfc
    9 return
   10 fnm2=0.
      gnm2=one
      fnm1=y+y
      gnm1=ysq+ysq+one
      prev=fnm1/gnm1
      wn=one
      bn=gnm1+four
   14 an=-wn*(wn+one)
      fn=bn*fnm1+an*fnm2
      gn=bn*gnm1+an*gnm2
      f=fn/gn
      if(abs(one-(f/prev))-toler) 12,12,13
   13 if(prev-f) 17,17,18
   17 if(gn.lt.scf) goto 16
      go to 15
   15 fn=fn/scf
      gn=gn/scf
      fnm1=fnm1/scf
      gnm1=gnm1/scf
   16 fnm2=fnm1
      gnm2=gnm1
      fnm1=fn
      gnm1=gn
      wn=wn+two
      bn=bn+four
      prev=f
      go to 14
   18 f=prev
   12 erfc=f*exp(-ysq)*trrtpi/two
      erf=one-erfc
      go to 7
      end





      subroutine editin

c  writes the input values used in the problem

      common sr3,third,f227,dbt,utst,wse,bs,bse,nssm
      common bb0,b0,r0,utm3,utc3,sft,htp,htp0,ncalc
      common h,bb,b,beta,r,g,vg,v,w,cm,u,t,rho,cv,xn,x
      common wma,cpa,grav,rhoa,rhos,pa,rr,qs,ws,hrf,urf
      common cf0,cf1,cri,tutrd,f17,f19,fu,ft,fv,sfu
      common cth,keval,uab,uab0,u0,cws,wss,ala0,zt,cu1,cu2

      common sr33,sctst,slup,sr2,srpi,cf00,sigb,tav0,uastr,ala
      common xk,aa,uastr2,rcf,afa,vh,stb,zl,hmx,ubs2,ubs20,xstr
      common idspl,nx,f(15),dy(15),betacp(61),sigp(61),cvpm(61),zm(61)
      common fcx(61),fcy(6,61),fcz(5,61),cvpt(6,4,61),bbcp(61),ccp(61)
      common alfg,alfgv,h0,tsd,qtcs,qtis,hs,us,stab,vg0,wcp(61)
      common zc,zc0,wc,wc0,sru0,rho0,bbv,bbv0,bv,bv0,dbbx,cmdap(61)
      common gw,fw,cp,facss,facp1,facp2,tbp,sfx,sfy,sfz

      common/var/vp(61),wp(61),hp(61),bbp(61),vgp(61),up(61),bp(61),
     1 rhop(61),cmp(61),tp(61),cvp(61),betap(61),xp(61),cmwvp(61),
     1 cmevp(61),uap(61),zcp(61),timp(61),bbxp(61),ugp(61),vxp(61),
     1 bxp(61),betaxp(61),qintp(61),tcld(61),cmwp(61),xccp(61),tccp(61)

      common za,ta,ua,z0,wms,cps,ts,nzpm,zp(5),
     1 as,xffm,tav,msfm,mnfm,mffm,tffm

      common tim,timn,vx,bbx,bbx0,gx,rmi,bbvx,bbvx0,
     1 ug,ug0,fug,phimi,phgam,idpf,nxtr,bx,bx0,bvx,bvx0,betax,
     1 qint,qint0,cm0,cmwv,cmwv0,cmev,cmev0,cp0,cpaa,cpwv,cpwl,
     1 cpsl,dhw,dhe,cmdaa,cmwa,rhosl,spa,spb,spc,spaw,spbw,t0,
     1 wmae,wmw,rhowl,cmw,cmw0,cmed,rh,tau0,tgon


c   release gas properties

      write (3,101) wms,cps,ts,rhos,tbp,cmed,cpsl,dhe,rhosl,spa,spb,spc

  101 format (//'release gas properties'//,1p,
     1 ' molecular weight of source gas (kg)     ',10x,'- wms  =',e12.4/
     1 ' vapor heat capacity, const. p.  (j/kg-k)',10x,'- cps  =',e12.4/
     1 ' temperature of source gas (k)           ',10x,'- ts   =',e12.4/
     1 ' density of source gas (kg/m3)           ',10x,'- rhos =',e12.4/
     1 ' boiling point temperature               ',10x,'- tbp  =',e12.4/
     1 ' liquid mass fraction                    ',10x,'- cmed0=',e12.4/
     1 ' liquid heat capacity (j/kg-k)           ',10x,'- cpsl =',e12.4/
     1 ' heat of vaporization (j/kg)             ',10x,'- dhe  =',e12.4/
     1 ' liquid source density (kg/m3)           ',10x,'- rhosl=',e12.4/
     1 ' saturation pressure constant            ',10x,'- spa  =',e12.4/
     1 ' saturation pressure constant (k)        ',10x,'- spb  =',e12.4/
     1 ' saturation pressure constant (k)        ',10x,'- spc  =',e12.4)

c   spill characteristics

      write (3,102) idspl,qs,tsd,qtcs,qtis,as,ws,bs,hs,us

  102 format (//'spill characteristics'//,1p,
     1 ' spill type                              ',10x,'- idspl=',i12/
     1 ' mass source rate (kg/s)                 ',10x,'- qs   =',e12.4/
     1 ' continuous source duration (s)          ',10x,'- tsd  =',e12.4/
     1 ' continuous source mass (kg)             ',10x,'- qtcs =',e12.4/
     1 ' instantaneous source mass (kg)          ',10x,'- qtis =',e12.4/
     1 ' source area (m2)                        ',10x,'- as   =',e12.4/
     1 ' vertical vapor velocity (m/s)           ',10x,'- ws   =',e12.4/
     1 ' source half width (m)                   ',10x,'- bs   =',e12.4/
     1 ' source height (m)                       ',10x,'- hs   =',e12.4/
     1 ' horizontal vapor velocity (m/s)         ',10x,'- us   =',e12.4)

c   field parameters

      write (3,103) tav,hmx,xffm,(zp(i),i=1,4)

  103 format (//'field parameters'//,1p,
     1 ' concentration averaging time (s)        ',10x,'- tav  =',e12.4/
     1 ' mixing layer height (m)                 ',10x,'- hmx  =',e12.4/
     1 ' maximum downwind distrace (m)           ',10x,'- xffm =',e12.4/
     1 ' concentration measurement height (m)    ',10x,'- zp(1)=',e12.4/
     1 '                                         ',10x,'- zp(2)=',e12.4/
     1 '                                         ',10x,'- zp(3)=',e12.4/
     1 '                                         ',10x,'- zp(4)=',e12.4)

c   ambient meteorological variables

      write (3,104) wmae,cpaa,rhoa,za,pa,ua,ta,rh,uastr,stab,ala,z0

  104 format (//'ambient meteorological properties'//,1p,
     1 ' molecular weight of ambient air (kg)   ',10x,'- wmae  =',e12.4/
     1 ' heat capacity of ambient air at const p. (j/kg-k)',
     1                                             '- cpaa  =',e12.4/
     1 ' density of ambient air (kg/m3)         ',10x,'- rhoa  =',e12.4/
     1 ' ambient measurement height (m)         ',10x,'- za    =',e12.4/
     1 ' ambient atmospheric pressure (pa=n/m2=j/m3)',6x,
     1 '- pa    =',e12.4/
     1 ' ambient wind speed (m/s)               ',10x,'- ua    =',e12.4/
     1 ' ambient temperature (k)                ',10x,'- ta    =',e12.4/
     1 ' relative humidity (percent)            ',10x,'- rh    =',e12.4/
     1 ' ambient friction velocity (m/s)        ',10x,'- uastr =',e12.4/
     1 ' atmospheric stability class value      ',10x,'- stab  =',e12.4/
     1 ' inverse monin-obukhov length (1/m)     ',10x,'- ala   =',e12.4/
     1 ' surface roughness height (m)           ',10x,'- z0    =',e12.4)

c   additional parameters

      write (3,105) ncalc,nssm,grav,rr,xk

  105 format(//'additional parameters'//,1p,
     1 ' sub-step multiplier                    ',10x,'- ncalc =',i12/
     1 ' number of calculational sub-steps      ',10x,'- nssm  =',i12/
     1 ' acceleration of gravity (m/s2)         ',10x,'- grav  =',e12.4/
     1 ' gas constant (j/mol- k)                ',10x,'- rr    =',e12.4/
     1 ' von karman constant                    ',10x,'- xk    =',e12.4)

      return
      end





      subroutine edits

c  writes the instantaneous spatially averaged cloud properties

      common sr3,third,f227,dbt,utst,wse,bs,bse,nssm
      common bb0,b0,r0,utm3,utc3,sft,htp,htp0,ncalc
      common h,bb,b,beta,r,g,vg,v,w,cm,u,t,rho,cv,xn,x
      common wma,cpa,grav,rhoa,rhos,pa,rr,qs,ws,hrf,urf
      common cf0,cf1,cri,tutrd,f17,f19,fu,ft,fv,sfu
      common cth,keval,uab,uab0,u0,cws,wss,ala0,zt,cu1,cu2

      common sr33,sctst,slup,sr2,srpi,cf00,sigb,tav0,uastr,ala

      common xk,aa,uastr2,rcf,afa,vh,stb,zl,hmx,ubs2,ubs20,xstr
      common idspl,nx,f(15),dy(15),betacp(61),sigp(61),cvpm(61),zm(61)
      common fcx(61),fcy(6,61),fcz(5,61),cvpt(6,4,61),bbcp(61),ccp(61)
      common alfg,alfgv,h0,tsd,qtcs,qtis,hs,us,stab,vg0,wcp(61)
      common zc,zc0,wc,wc0,sru0,rho0,bbv,bbv0,bv,bv0,dbbx,cmdap(61)
      common gw,fw,cp,facss,facp1,facp2,tbp,sfx,sfy,sfz

      common/var/vp(61),wp(61),hp(61),bbp(61),vgp(61),up(61),bp(61),
     1 rhop(61),cmp(61),tp(61),cvp(61),betap(61),xp(61),cmwvp(61),
     1 cmevp(61),uap(61),zcp(61),timp(61),bbxp(61),ugp(61),vxp(61),
     1 bxp(61),betaxp(61),qintp(61),tcld(61),cmwp(61),xccp(61),tccp(61)

      common za,ta,ua,z0,wms,cps,ts,nzpm,zp(5),
     1 as,xffm,tav,msfm,mnfm,mffm,tffm

      common tim,timn,vx,bbx,bbx0,gx,rmi,bbvx,bbvx0,
     1 ug,ug0,fug,phimi,phgam,idpf,nxtr,bx,bx0,bvx,bvx0,betax,
     1 qint,qint0,cm0,cmwv,cmwv0,cmev,cmev0,cp0,cpaa,cpwv,cpwl,
     1 cpsl,dhw,dhe,cmdaa,cmwa,rhosl,spa,spb,spc,spaw,spbw,t0,
     1 wmae,wmw,rhowl,cmw,cmw0,cmed,rh,tau0,tgon


      write (3,95)
      write (3,101)
      write (3,102) (xp(i),zcp(i),hp(i),bbp(i),bp(i),bbxp(i),
     1 bxp(i),cvp(i),rhop(i),tp(i),up(i),uap(i),i=1,61)

      write (3,103)
      write (3,104) (xp(i),cmp(i),cmevp(i),cmdap(i),cmwp(i),
     1 cmwvp(i),wcp(i),vgp(i),ugp(i),wp(i),vp(i),vxp(i),i=1,61)

   95 format('1'//'instantaneous spatially averaged cloud parameters')
  101 format(//5x,'x',8x,'zc',9x,'h',8x,'bb',9x,'b',8x,
     1 'bbx',7x,'bx',8x,'cv',8x,'rho',8x,'t',9x,'u',8x,'ua')
  102 format (1p,12e10.2)
  103 format('1'//5x,'x',8x,'cm',8x,'cmv',7x,'cmda',6x,'cmw',
     1 7x,'cmwv',6x,'wc',8x,'vg',8x,'ug',9x,'w',9x,'v',8x,'vx')
  104 format (1p,12e10.2)

      return
      end





      subroutine editcc

c  calculates and writes the time averaged volume concentration

      common sr3,third,f227,dbt,utst,wse,bs,bse,nssm
      common bb0,b0,r0,utm3,utc3,sft,htp,htp0,ncalc
      common h,bb,b,beta,r,g,vg,v,w,cm,u,t,rho,cv,xn,x
      common wma,cpa,grav,rhoa,rhos,pa,rr,qs,ws,hrf,urf
      common cf0,cf1,cri,tutrd,f17,f19,fu,ft,fv,sfu
      common cth,keval,uab,uab0,u0,cws,wss,ala0,zt,cu1,cu2

      common sr33,sctst,slup,sr2,srpi,cf00,sigb,tav0,uastr,ala
      common xk,aa,uastr2,rcf,afa,vh,stb,zl,hmx,ubs2,ubs20,xstr
      common idspl,nx,f(15),dy(15),betacp(61),sigp(61),cvpm(61),zm(61)
      common fcx(61),fcy(6,61),fcz(5,61),cvpt(6,4,61),bbcp(61),ccp(61)
      common alfg,alfgv,h0,tsd,qtcs,qtis,hs,us,stab,vg0,wcp(61)
      common zc,zc0,wc,wc0,sru0,rho0,bbv,bbv0,bv,bv0,dbbx,cmdap(61)
      common gw,fw,cp,facss,facp1,facp2,tbp,sfx,sfy,sfz

      common/var/vp(61),wp(61),hp(61),bbp(61),vgp(61),up(61),bp(61),
     1 rhop(61),cmp(61),tp(61),cvp(61),betap(61),xp(61),cmwvp(61),
     1 cmevp(61),uap(61),zcp(61),timp(61),bbxp(61),ugp(61),vxp(61),
     1 bxp(61),betaxp(61),qintp(61),tcld(61),cmwp(61),xccp(61),tccp(61)

      common za,ta,ua,z0,wms,cps,ts,nzpm,zp(5),
     1 as,xffm,tav,msfm,mnfm,mffm,tffm

      common tim,timn,vx,bbx,bbx0,gx,rmi,bbvx,bbvx0,
     1 ug,ug0,fug,phimi,phgam,idpf,nxtr,bx,bx0,bvx,bvx0,betax,
     1 qint,qint0,cm0,cmwv,cmwv0,cmev,cmev0,cp0,cpaa,cpwv,cpwl,
     1 cpsl,dhw,dhe,cmdaa,cmwa,rhosl,spa,spb,spc,spaw,spbw,t0,
     1 wmae,wmw,rhowl,cmw,cmw0,cmed,rh,tau0,tgon


c  calculate time averaged volume concentration
c   xp(i) = downwind distance
c   tccp(i) = time
c   tcld(i) = cloud duration
c   timp(i) = time of maximum concentration
c   bbp(i) = half width
c   bbcp(i) = effective half width with meander
c   betap(i) = half width parameter beta
c   betacp(i) = effective beta with meander

      tcmx = 2.*bbxp(61)/up(61)
      tcld(61) = amax1(tcmx,tsd)
      do 10 i=2,60
      in = 62-i
      tcmx = 2.*bbxp(in)/up(in)
      tcld(in) = amax1(tsd,amin1(tcld(in+1),tcmx))
   10 continue
      tcld(1) = tcld(2)

      if (ala .lt. 0.) then
         stby = 1.-rcf*10.*ala
        else
         stby = 1./(1.+rcf*10.*ala)
      endif
      asig = 2.*afa*stby/sigb

      if (up(1) .eq. 0.) up(1) = .001
      if (tav .eq. 0.) tav = 1.

      cc1 = 4.*sr2*srpi

      do 100 i=1,61

      tmdr = amin1(tcld(i),tav)
      afata = .08*(((tmdr+tau0*exp(-tmdr/tau0))/tav0)**.2)
      rav = afata/afa
      rav2 = rav*rav
      sig0 = asig*(sqrt(1.+sigb*(xp(i)-xp(1)))-1.)
      rjt = 1.
      if (idspl .eq. 2)  then
         rm3 = ((rhoa*uap(i)*(1.-cmp(i)))/(rhos*us*cmp(i)))
     1         **(1./3.)
         rlg = log((1.-rm3+(rm3*rm3))/((1.+rm3)*(1.+rm3)))
         rit = atan(((2.*rm3)-1.)/sr3) + atan(1./sr3)
         if (rm3 .lt. .1)  then
            rjt = 0.
           else
            rjt = 1. - (2./(3.*rm3*rm3))*(.5*rlg + sr3*rit)
         endif
      endif
      sigm2 = (rav2-1.)*sig0*sig0*rjt*rjt
      betac2 = betap(i)*betap(i) + sigm2
      betacp(i) = sqrt(betac2)
      bbcp(i) = sqrt(bp(i)*bp(i)+3.0*betac2)

      hhf = .5*hp(i)
      if (zcp(i) .gt. hhf) then
          htpp = zcp(i) + hhf
        else
          htpp = hp(i)
      endif
      sig2 = third*(htpp-zcp(i))*(htpp-zcp(i))
      sigp(i) = sqrt(sig2)

      if (sigp(i) .eq. 0.) then
          hdsig = sr3
        else
          hdsig = hp(i)/sigp(i)
      endif

      if (i .lt. nxtr) then
          cvbrx = amin1(tsd/tav,1.0)
          fcx(i) = facss*bbp(i)*hdsig*cvbrx*cvp(i)/bp(i)
        else
          epx1 = (bxp(i) + .5*up(i)*tav)/(sr2*betaxp(i))
          epx2 = (bxp(i) - .5*up(i)*tav)/(sr2*betaxp(i))
          if (epx1 .gt. 6.) then
              erfx1 = 1.0
            else
              call errint (epx1,erfx1,erfcx1)
          endif
          if (abs(epx2) .gt. 6.) then
              erfx2 = epx2/abs(epx2)
             else
              call errint (epx2,erfx2,erfcx2)
          endif
          cvbrx = epx1*erfx1 - epx2*erfx2 + facp2*
     1       (exp(-epx1*epx1) - exp(-epx2*epx2))
          facn = facp1*betaxp(i)*bbxp(i)*bbp(i)*hdsig
          facd = bxp(i)*up(i)*tav*bp(i)
          fcx(i) = facn*cvbrx*cvp(i)/facd
      endif

      voln = bbxp(i)*bbcp(i)*hp(i)*cvp(i)
      vold = cc1*bxp(i)*bp(i)*sigp(i)

      if (vold .ne. 0.) then
         ccp(i) = voln/vold
        else
         ccp(i) = 0.
      endif

      nzpm1 = nzpm + 1
      if (zcp(i) .gt. sigp(i)) then
         zm(i) = zcp(i)
         do 15 ii=1,5
         ezi = exp(-2.*zcp(i)*zm(i)/(sigp(i)*sigp(i)))
         gzi = zcp(i)*(1.-ezi)/(1.+ezi)
         gzip = (4.*zcp(i)*zcp(i)/(sigp(i)*sigp(i)))*
     1     ezi/((1.+ezi)*(1.+ezi))
         zm(i) = (gzi-zm(i)*gzip)/(1.-gzip)
   15    continue
        else
         zm(i) = 0.
      endif
      zp(nzpm1) = zm(i)

      do 20 j=1,nzpm1
      if (sigp(i) .eq. 0.) then
          fcz(j,i) = 0.
          if (zp(j) .eq. 0.) fcz(j,i) = 2.0
          else
            epx1 = -.5*(zp(j)-zcp(i))*(zp(j)-zcp(i))/
     1         (sigp(i)*sigp(i))
            epx2 = -.5*(zp(j)+zcp(i))*(zp(j)+zcp(i))/
     1          (sigp(i)*sigp(i))
        fcz(j,i) = exp(epx1) + exp(epx2)
      endif
   20 continue

      do 40 k=1,6
      ak = k
      y = .5*(ak-1.)*bbcp(i)
      epy1 = (y+bp(i))/(sr2*betacp(i))
      epy2 = (y-bp(i))/(sr2*betacp(i))
      if (abs(epy1) .gt. 6.0) then
          erfy1 = epy1/abs(epy1)
        else
          call errint (epy1,erfy1,erfcy1)
      endif
      if (abs(epy2) .gt. 6.0) then
          erfy2 = epy2/abs(epy2)
        else
          call errint (epy2,erfy2,erfcy2)
      endif
      fcy(k,i) = erfy1 - erfy2
   40 continue


      do 60 j=1,nzpm
      do 50 k=1,6
      cvpt(k,j,i) = fcx(i)*fcy(k,i)*fcz(j,i)
      if (cvpt(k,j,i) .gt. 1.0) cvpt(k,j,i) = 1.0
   50 continue
   60 continue

       cvpm(i) = fcx(i)*fcy(1,i)*fcz(nzpm1,i)
      if (cvpm(i) .gt. 1.0) cvpm(i) = 1.0

  100 continue

c  write the concentration contour parameters

      if (idspl .eq. 4) then

      write (3,105) tav
  105 format ('1'//'time averaged (tav =',f6.0,' s) volume',
     1 ' concentration:  concentration contour parameters'//
     1 'c(x,y,z,t) = cc(t) * (erf(xa)-erf(xb)) * (erf(ya)-erf(yb)) *',
     1 ' (exp(-za*za)+exp(-zb*zb))'//
     1 3x,'c(x,y,z,t) = concentration (volume fraction) at (x,y,z,t)'/
     1 5x,'       x = downwind distance (m)'/
     1 5x,'       y = crosswind horizontal distance (m)'/
     1 5x,'       z = height (m)'/
     1 5x,'       t = time (s)'/)

      write (3,106) (tccp(i),ccp(i),bp(i),betacp(i),zcp(i),sigp(i),
     1 xccp(i),bxp(k),betaxp(i),i=1,61)
  106 format (5x,'     erf = error functon'/
     1 5x,'      xa = (x-xc+bx)/(sr2*betax)'/
     1 5x,'      xb = (x-xc-bx)/(sr2*betax)'/
     1 5x,'      ya = (y+b)/(sr2*betac)'/
     1 5x,'      yb = (y-b)/(sr2*betac)'/
     1 5x,'     exp = exponential function'/
     1 5x,'      za = (z-zc)/(sr2*sig)'/
     1 5x,'      zb = (z+zc)/(sr2*sig)'/
     1 5x,'     sr2 = sqrt(2.0)'//
     1 5x,'t',8x,'cc(t)',5x,'b(t)',4x,'betac(t)',4x,'zc(t)',4x,'sig(t)',
     1 5x,'xc(t)',5x,'bx(t)',3x,'betax(t)'/,1p,(9e10.2))

      else

      write (3,107) tav
  107 format ('1'//'time averaged (tav =',f6.0,' s) volume',
     1 ' concentration:  concentration contour parameters'//
     1 'c(x,y,z,t) = cc(x) * (erf(xa)-erf(xb)) * (erf(ya)-erf(yb)) *',
     1 ' (exp(-za*za)+exp(-zb*zb))'//
     1 3x,'c(x,y,z,t) = concentration (volume fraction) at (x,y,z,t)'/
     1 5x,'       x = downwind distance (m)'/
     1 5x,'       y = crosswind horizontal distance (m)'/
     1 5x,'       z = height (m)'/
     1 5x,'       t = time (s)'/)

      write (3,108) (xp(i),ccp(i),bp(i),betacp(i),zcp(i),sigp(i),
     1 tccp(i),xccp(i),bxp(i),betaxp(i), i=1,61)
  108 format (5x,'     erf = error functon'/
     1 5x,'      xa = (x-xc+bx)/(sr2*betax)'/
     1 5x,'      xb = (x-xc-bx)/(sr2*betax)'/
     1 5x,'      ya = (y+b)/(sr2*betac)'/
     1 5x,'      yb = (y-b)/(sr2*betac)'/
     1 5x,'     exp = exponential function'/
     1 5x,'      za = (z-zc)/(sr2*sig)'/
     1 5x,'      zb = (z+zc)/(sr2*sig)'/
     1 5x,'     sr2 = sqrt(2.0)'//
     1 5x,'x',8x,'cc(x)',5x,'b(x)',4x,'betac(x)',4x,'zc(x)',4x,'sig(x)',
     1 16x,'t',8x,'xc(t)',5x,'bx(t)',3x,'betax(t)'/,1p,
     1 (6e10.2,10x,4e10.2))

      endif

c  write the concentration in the zp(i) plane

      do 125 j=1,nzpm

      write (3,110) tav,zp(j)
  110 format ('1'//'time averaged (tav =',f6.0,' s) volume',
     1 ' concentration:  concentration in the z =',f6.2,' plane.')

      write (3,115) (xp(i),timp(i),tcld(i),bbcp(i),(cvpt(k,j,i),k=1,6),
     1 i=1,61)
  115 format (//'    downwind    time of      cloud     effective',15x,
     1 'average concentration (volume fraction) at (x,y,z)'/
     1 '    distance    max conc    duration   half width',8x,'y/bbc=',
     1 5x,'y/bbc=',5x,'y/bbc=',5x,'y/bbc=',5x,'y/bbc=',5x,'y/bbc='/
     1 5x,' x (m)',8x,'(s)',9x,'(s)',7x,'bbc (m)',13x,'0.0',8x,'0.5',8x,
     1 '1.0',8x,'1.5',8x,'2.0',8x,'2.5'/,1p,(4e12.2,6x,6e11.2))

  125 continue

c  write the maximum concentration along centerline

      write (3,130) tav
  130 format ('1'//'time averaged (tav =',f6.0,' s) volume',
     1 ' concentration:  maximum concentration (volume',
     1 ' fraction) along centerline.')

      write (3,135) (xp(i),zm(i),cvpm(i),timp(i),tcld(i),i=1,61)
  135 format (//'    downwind                 maximum',
     1 4x,'time of',6x,'cloud'/
     1 4x,'distance',5x,'height',3x,'concentration',1x,'max conc',4x,
     1 'duration'/
     1 5x,' x (m)',7x,'z (m)',5x,'c(x,0,z)',7x,'(s)',9x,'(s)'/,1p,
     1 (5e12.2))

      return
      end





c
c             (C) Copyright 1990 the Regents of the
c         University of California. All Rights Reserved.
c
c        This work was produced under the sponsorship of
c         the U.S. Department of Energy. The Government
c                retains certain rights therein.
