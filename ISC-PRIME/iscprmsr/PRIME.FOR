c----------------------------------------------------------------------
      subroutine prime1
c----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812                PRIME1
c                J. Scire, D. Strimaitis, EARTH TECH
c                Prepared for EPRI under contract WO3527-01
c
c --- PURPOSE:  Initialize the variables used by the PRIME
c               building downwash algorithm
c
c --- INPUTS:  none
c
c --- OUTPUT:
c
c     Common block /DFSN/ variables:
c           afac,xbyrmax,wiz0,wiy0,wfz,wfy,
c           dua_ua,xdecay,xdecayi,
c           rurliz,rurliy,urbniz,urbniy
c
c --- PRIME1 called by:  MAIN (Host)
c --- PRIME1 calls:      none
c----------------------------------------------------------------------

c --- Include common blocks
      include 'dfsn.pri'
      real riz(6),riy(6),uiz(6),uiy(6)

c --- Ambient turbulence intensities are inferred from Briggs (1973)
c --- "Diffusion estimation for small emissions", ATDL-106;
      data riz/.20,.12,.08,.06,.03,.016/
      data riy/.22,.16,.11,.08,.06,.04/
      data uiz/.24,.24,.20,.14,.08,.08/
      data uiy/.32,.32,.22,.16,.11,.11/

c -----------------------
c --- /DFSN/ variables
c -----------------------

c --- Set the factor for defining when turb Approaches Asymptotic
c --- value, and also define the maximum allowed scaled distance
      afac=1.3
      xbyrmax=15.

c --- Turbulence intensities in wake (from Briggs rural curves)
      wiz0=0.06
      wiy0=0.08
c --- Wake Factors for sigw and sigv from Weil (1996)
      wfz=1.7
      wfy=1.7
c --- deltaU0/U0
      dua_ua=0.7
c --- Power-law exponent for turbulence intensity change in distance
      xdecay=0.666667
      xdecayi=1.5

c --- Pass turbulence intensities to /DFSN/ arrays
      do kst=1,6
         rurliz(kst)=riz(kst)
         rurliy(kst)=riy(kst)
         urbniz(kst)=uiz(kst)
         urbniy(kst)=uiy(kst)
      enddo
c
      return
      end
c----------------------------------------------------------------------
      subroutine numpr1
c----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812                NUMPR1
c                J. Scire, EARTH TECH
c                Prepared for EPRI under contract WO3527-01
c
c --- PURPOSE:  Initialize the variables used by the numerical
c               plume rise algorithm
c
c --- INPUTS:
c
c       Parameters:
c           MXENT, MXENTP1, MXNZ, MXNZP1, IO6
c
c --- OUTPUT:
c
c       Common block /NUMPARM/ variables:
c          GRAVI,RGAS,ZMIN,DS,NSTEP,SLAST,RP,ALPHAP(mxent),
c          BETAP(mxent),XCAT(mxentp1),NENT
c       Common block /AMBIENT/ variables:
c          ADIA,PTGRAD0,ZGPTA(mxnz),ZFACEA(mxnzp1)
c
c --- NUMPR1 called by:  MAIN (Host)
c --- NUMPR1 calls:      none
c----------------------------------------------------------------------

c --- Include parameters
      include 'params.pri'

c --- Include common blocks
      include 'numparm.pri'
      include 'ambient.pri'

c -----------------------
c --- /NUMPARM/ variables
c -----------------------
c
c --- Set the acceleration due to gravity (m/s**2)
      gravi=9.807

c --- Set the gas constant (m**2/s**2/deg. K)
      rgas=287.026

c --- Set the minimum plume centerline height (m)
      zmin=0.001

c --- Set the step size (m) in the numerical plume rise algorithm
      ds=1.0

c --- Set the internal save frequency of plume rise calculations (i.e.,
c     every DS*NSTEP meters) (NOTE: this the frequency with which the
c     results are saved internally -- not that passed back from the
c     NUMRISE routine)
      NSTEP=1

c --- Set the termination distance (m) of the plume rise calculation
      slast=5000.

c --- Set the radiation coefficient (kg/m**2/deg. K**3/s)
      rp=9.1e-11

c --- Set the perturbed entrainment coefficients
c     ALPHAP (parallel direction), BETAP (normal direction)
      nent=0
      alphap(1)=0.11
      betap(1)=0.6
      xcat(1)=-9.e9
      xcat(2)= 9.e9

c -----------------------
c --- /AMBIENT/ variables
c -----------------------

c --- Set dry adiabatic lapse rate (deg. K/m)
      adia=.0098

c --- Set minimum potential temperature lapse rate (deg. K/m)
      ptgrad0=0.0

c --- Set the default number of layers
      nza=45
      nzap1=nza+1
      if(nza.gt.mxnz)then
         write(io6,*)'ERROR in SUBR. NUMPR1 -- NZA is too large -- ',
     1   'NZA = ',nza,' MXNZ = ',mxnz
         stop
      endif
      if(nzap1.gt.mxnzp1)then
         write(io6,*)'ERROR in SUBR. NUMPR1 -- NZAP1 is too large -- ',
     1   'NZAP1 = ',nzap1,' MXNZP1 = ',mxnzp1
         stop
      endif

c --- Define the meteorological grid
c ---    Set grid points every 10 m from 10-200 m
         dz=10.
         nn=1
         zgpta(nn)=dz
         do i=2,20
            nn=nn+1
            zgpta(nn)=zgpta(nn-1)+dz
         enddo
c ---    Set grid points every 50 m from 250-500 m
         dz=50.
         do i=21,26
            nn=nn+1
            zgpta(nn)=zgpta(nn-1)+dz
         enddo
c ---    Set grid points every 100 m from 600-2000 m
         dz=100.
         do i=27,41
            nn=nn+1
            zgpta(nn)=zgpta(nn-1)+dz
         enddo
c ---    Set grid points every 500 m from 2500-4000 m
         dz=500.
         do i=42,45
            nn=nn+1
            zgpta(nn)=zgpta(nn-1)+dz
         enddo

c --- Compute the cell face heights from the grid point values
      zfacea(1)=0.0
      do i=2,nza
         zfacea(i)=0.5*(zgpta(i)+zgpta(i-1))
      enddo
      zfacea(nzap1)=zgpta(nza)+0.5*(zgpta(nza)-zgpta(nza-1))

      return
      end
c----------------------------------------------------------------------
      subroutine numrise(ldbhr,h,reff,texit,wexit,
     &                   ntr,xtr,ytr,ztr,rtr)
c----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  980310               NUMRISE
c                X.(J.) Zhang, J. Scire, D. Strimaitis,  EARTH TECH
c
c                Adapted from CALPUFF routine NUMRISE
c                for EPRI under contract WO3527-01
c
c --- PURPOSE:  Compute plume rise using a numerical solution to the
c               Non-Boussinesq conservation laws.  Model allows:
c
c               (1) arbitrary ambient temperature stratifications
c               (2) arbitrary uni-directional wind stratification
c               (3) any size of finite emission source
c               (4) is free of the Boussinesq approximation
c               (5) radiative heat loss
c
c               Concurrently, compute diffusion (sigmas) in bldg wake
c               and determine plume/cavity interaction
c
c --- INPUTS:
c         LDBHR - logical       - Flag for debug write statements
c             H - real          - Release height (m)
c          REFF - real          - Effective radius of release (m)
c            TP - real          - Exit temperature (deg. K)
c         WEXIT - real          - Exit velocity (m/s)
c           NTR - integer       - Number of points in trajectory passed
c                                 back to calling program (final point
c                                 is "final rise")
c
c     Common block /AMBIENT/ variables:
c           NZA,UAMB(mxnz),RAMB(mxnz),DEDZ(mxnzp1),TAMB(mxnz),
c           ZFACEA(mxnzp1),TAMB0,RAMB0
c     Common block /NUMPARM/ variables:
c           ZMIN, DS, NSTEP, SLAST, GRAVI
c     Common block /WAKEDAT/ variables:
c           HB, WB, XLB, RB, HR, XLR, XLC, XBADJ, YBADJ
c     Parameters:
c           MXNZ, MXNZP1, MXENT, MXENTP1, IO6
c
c --- OUTPUT:
c        XTR(ntr) - real          - Downwind distance from source (m)
c        YTR(ntr) - real          - Crosswind distance from source (m)
c        ZTR(ntr) - real          - Plume centerline height (m)
c        RTR(ntr) - real          - Plume radius (m)
c
c     Common block /WAKEDAT/ variables:
c           FQCAV
c
c --- NUMRISE called by:  PHEFF (HOST subroutine)
c --- NUMRISE calls:      ZMET,LUMP,RATE,MARCHING,UNLUMP
c                         ZSTREAM, POSITION, WAKE_U
c                         WAKE_DRDX, WAKE_DFSN, SIGZ, SIGY,
c                         WAKE_FQC
c----------------------------------------------------------------------
c --- Notation --- in (KG,M,S) units
c               NN:     Number of points along rise trajectory
c               X:      PLUME LOCATION (downwind from source)
c               Y:      PLUME LOCATION (crosswind from source)
c               Z:      PLUME HEIGHT
c               H:      Release height (flame ht., stack ht.)
c               ZE:     PLUME EQUILIBRIUM HEIGHT
c               S:      LENGTH ALONG PLUME CENTERLINE
c               R:      PLUME RADIUS
c               U:      PLUME HORIZONTAL VELOCITY
c               W:      PLUME VERTICAL VELOCITY
c               USC:    VELOCITY ALONG PLUME CENTERLINE
c               PHI:    ANGLE BETWEEN PLUME TRAJECTORY AND GROUND
c               TP:     PLUME TEMPERATURE
c               ua:     HORIZONTAL WIND SPEED
c               dudz:   WIND SHEAR
c               ta:     AMBIENT TEMPERATURE
c               dpdz:   AMBIENT POTENTIAL TEMPERATURE GRADIENT
c               ramb:   AMBIENT DENSITY
c               ra:     PLUME DENSITY
c               zmin:   Minimum plume centerline height (m)
c               ds:     Step size (m) in the numerical plume rise calc.
c               nstep:  Reporting frequency of numerical calc.
c               slast:  Termination distance (m) of plume rise calc.
c               gravi:  Acceleration due to gravity (m/s**2)
c----------------------------------------------------------------------
c --- Include files
      include 'params.pri'
      include 'ambient.pri'
      include 'numparm.pri'
      include 'wakedat.pri'

      COMMON /PLU/ S,X,Y,Z,R,U,V,W,USC,PHI,DEN,TP

      REAL XTR(ntr),YTR(ntr),ZTR(ntr),RTR(ntr)
      REAL XT(mxnw),YT(mxnw),ZT(mxnw),RT(mxnw)
      DIMENSION RHS(7),RHSTEMP(7),F(7),FTEMP(7)
      logical ldb,ldbnn,ldbu,ldbhr,linwake

c --- Use LDB as a local switch for more extensive debug output
c !!! ldb=ldbhr
      ldb=.FALSE.
c !!! ldbu=ldb
      ldbu=.FALSE.

      linwake=.FALSE.
      X=0.
      Y=0.
      Z=AMAX1(H,ZMIN)
      S=0.
      R=REFF
      U=0.000002
      w=wexit
      tp=texit
      drdxa=0.0
      ipositn=4

c --- Store stepping length
      ds0=ds

c --- Introduce ZCUMUL to track change in the vertical coordinate of the
c --- trajectory that arises from streamline inclination from horizontal
c --- This adjustment is applied outside of the system of rise equations
c --- out to a distance of 15R from downwind face of building
      zcumul=0.0
      r15src=xbadj+(xLb+15.*Rb)

c --- Get met. variables at release height
      call ZMET(z,ua0,ra,ta,dudz0,dpdz)

c --- Apply reduction in wake wind speed
      xb=x-xbadj
      yb=y-ybadj
      zb=AMAX1(z,zmin)
      call POSITION(xb,yb,zb,ipositn)
      ufac=1.0
      dufac=0.0
      if(ipositn.LT.4) call WAKE_U(.FALSE.,xb,yb,zb,ufac,dufac)
      ua=ufac*ua0
      dudz=ufac*dudz0+dufac*ua0

c --- Use Briggs plume rise estimates to set distance scale
c --- Compute initial buoyancy flux (m**4/s**3)
      deltat=amax1(tp-ta,0.0)
      fdum=w*r*r/tp
      fb=gravi*fdum*deltat
c --- Compute momentum flux (m**4/s**2)
      fm=w*fdum*ta
c --- Final neutral rise distance
      uam=amax1(ua,1.0)
c --- Momentum only: (do not base xmax on case where w<uam)
      wbyu=AMAX1(1.,w/uam)
      xmaxm=8.*r*wbyu*(1.+3./wbyu)**2
      if(fb.le.0.0)then
c ---    No buoyancy, momentum only
         xmax=xmaxm
      elseif(fb.lt.55)then
c ---    Buoyancy flux < 55 m**4/s**3
         xmaxb=49.*fb**0.625
         xmax=amax1(xmaxm,xmaxb)
      else
c ---    Buoyancy flux .GE. 55 m**4/s**3
         xmax=119*fb**0.4
      endif

c --- Use Briggs neutral rise to identify "minimal rise" cases
c --- Compute Briggs neutral final rise
      if(fb.LE.0.0) then
c ---    No buoyancy, momentum only
         znf=6.*r*w/uam
      elseif(fb.LT.55.) then
c ---    Buoyancy flux < 55 m**4/s**3
         znf=21.425*(fb**0.75)/uam
      else
c ---    Buoyancy flux .GE. 55 m**4/s**3
         znf=38.71*(fb**0.60)/uam
      endif
c --- Set minimum rise to 0.1 m
      znf0=AMAX1(0.1,znf)

c --- Guard against step length greater than likely rise
      dmin=0.5*znf0
      if(ds.GT.dmin) then
         ds=dmin
         if(ldb) then
            write(io6,*)'NUMRISE - initial step reset'
            write(io6,*)'znf,ds0,ds  :',znf,ds0,ds
         endif
      endif

c --- INDIRECT VARIABLES
      USC=SQRT(U*U+W*W)
      PHI=ATAN(W/U)
c --- PARAMETERS
      NP=NSTEP
      XNP=FLOAT(NP)
      nnp=1

c --- START MARCHING LOOP
      DEN=RA*TA/texit
      call LUMP(ua,ta,f)

999   continue

c --- Set local debug logical
      if(nnp.LT.150) then
         ldbnn=ldb
      else
         ldbnn=.FALSE.
      endif

c --- Define coordinates of plume relative to bldg.-defined origin
      xb=x-xbadj
      yb=y-ybadj
      zb=AMAX1(z+zcumul,zmin)
c --- Obtain mean streamline slopes here (within 15R of building)
      dxds=0.0
      dzdx=0.0
      dzstrm=0.0
      call POSITION(xb,yb,zb,ipositn)
      if(ipositn.GT.2 .AND. x.LE.r15src) then
         call ZSTREAM(hb,wb,xLb,rb,xLr,hr,xb,yb,zb,dzdx)
         dxds=U/USC
         dzds=dzdx*dxds
         dzstrm=dzds*ds
      endif
c --- Define the crosswind velocity component = zero
      dyds=0.0
      v=0.0

c --- Compute RHS of rate equations for this location
      call RATE(ua,dudz,ra,dpdz,ta,drdxa,rhs)

c --- PREDICTOR MARCHING
      call MARCHING(f,ftemp,rhs,ds)
      call UNLUMP(ua,ta,ra,ftemp)

c --- Extract met and apply reduction in wake wind speed
      zb=AMAX1(z+zcumul+dzstrm,zmin)
      call ZMET(zb,ua0,ra,ta,dudz0,dpdz)
      call POSITION(xb,yb,zb,ipositn)
      ufac=1.0
      dufac=0.0
      if(ipositn.LT.4) call WAKE_U(ldbu,xb,yb,zb,ufac,dufac)
      ua=ufac*ua0
      dudz=ufac*dudz0+dufac*ua0
      call RATE(ua,dudz,ra,dpdz,ta,drdxa,rhstemp)

c --- CORRECTOR
      DO I=1,7
         RHS(I)=0.5*(RHSTEMP(I)-RHS(I))
      ENDDO
      call MARCHING(ftemp,f,rhs,ds)
      call UNLUMP(ua,ta,ra,f)

c --- Compute incremental change in plume height to account for
c --- streamline ascent/descent, and add to cumulative change
      zcumul=zcumul+dzstrm
c --- Apply cumulative adjustment to plume height
      zc=AMAX1(z+zcumul,zmin)
c --- Define coordinates of plume relative to bldg.-defined origin
      xb=x-xbadj
      yb=y-ybadj
      zb=zc
      call POSITION(xb,yb,zb,ipositn)

c --- Numerical procedure may result in small negative downwind
c --- distance:  reset to zero and go to next step
      if(x.LT.0.0) then
         x=0.0
         s=s+ds
         nnp=nnp-1
         goto 96
      endif

c --- Write debug output if in debug mode
      if(ldbnn)then
         if(mod(nnp,1000).eq.1)write(io6,112)
112      format(1x,2x,'NNP',7x,'X',6x,'Y',6x,'Z',6x,'R',6x,'U',5x,'V',
     1   6x,'W',4x,'USC',5x,'PHI',4x,'DEN',5x,'TP',4x,'UA',5x,'RA',5x,
     2   'TA',4x,'DUDZ',5x,'DPDZ',3x,'DZDS',3x,'DYDS',2x,'IPOS',
     3   1x,'DELTAZ')
         deltaz=zc-h
         write(io6,114)nnp,x,y,zc,r,u,v,w,usc,phi,den,tp,ua,ra,ta,dudz,
     1    dpdz,dzds,dyds,ipositn,deltaz
114      format(1x,i5,f9.2,3f7.2,4f7.2,
     1    f8.4,f6.3,f7.2,f6.2,f7.3,f7.2,f9.4,
     2    f8.4,2f7.4,i5,f7.3)
      endif

c --- When trajectory inclination falls below 45 degrees, ignoring
c --- streamline descent, check for wake influence
      if(phi.LE.0.7854 .AND. ipositn.LT.4) then
         if(.not.LINWAKE) then
c ---       Plume centerline has just entered wake
            linwake=.TRUE.
            xbi=xb
c ---       Use unadjusted rise for BID
            base=AMAX1(H,ZMIN)
            rise=AMAX1(0.0,z-base)
            bidsq=(rise/3.5)**2
c ---       Guard against x.LE.0 due to precision
            if(x.LE.0.0) then
               szi=SQRT(bidsq)
               syi=szi
            else
               call SIGZ(x,szi)
               szi=SQRT(szi**2+bidsq)
               call SIGY(x,syi)
               syi=SQRT(syi**2+bidsq)
            endif

c --- Normal debug output
            if(ldbhr) then
               write(io6,*)'NUMRISE call to WAKE_DFSN'
               write(io6,*)'x,y,z,z+zcum:',x,y,z,zc
               write(io6,*)'ds,u,w      :',ds,u,w
               write(io6,*)'xb,phi      :',xb,phi
               write(io6,*)'szi,syi     :',szi,syi
            endif

c ---       Compute table of sigmas and growth rate in wake region
            call WAKE_DFSN(ldb,xbi,szi,syi)
         endif
c ---    Select plume radius growth rate for this location
         call WAKE_DRDX(x,drdxa)
      endif

c --- Process new position
      S=S+DS
      if(FLOAT(NNP/NP).eq.FLOAT(NNP)/XNP) THEN
         NN=NNP/NP
         if(nn.gt.mxnw)then
            write(io6,*)'Error in Subr. NUMRISE -- NN too large -- ',
     1      'NN = ',nn,' MXNW = ',mxnw
            stop
         endif
         XT(NN)=X
         YT(nn)=Y
         ZT(NN)=zc
         RT(NN)=R
c --- CHECK FOR PLUME EQUILIBRIUM HEIGHT
         IF(x .ge. xmax) THEN
            ZFIN=zc
            YFIN=Y
            XFIN=X
            RFIN=R
            GOTO 97
         ENDIF
      ENDIF

c --- Extract met and apply reduction in wake wind speed
96    call ZMET(zb,ua0,ra,ta,dudz0,dpdz)
      ufac=1.0
      dufac=0.0
      if(ipositn.LT.4) call WAKE_U(.FALSE.,xb,yb,zb,ufac,dufac)
      ua=ufac*ua0
      dudz=ufac*dudz0+dufac*ua0

c --- Next increment
      NNP=NNP+1
c --- Stop rise at local maximum (excludes streamline descent effects)
      if(w.lt.0.0)then
         zfin=zc
         yfin=y
         xfin=x
         rfin=r
         go to 97
      endif

c --- Adjust ds toward ds0 for next step
      if(ds.LT.ds0) ds=AMIN1(ds0,2.*ds)

      IF(S.LT.SLAST) GOTO 999
      ZFIN=zc
      YFIN=Y
      XFIN=X
      RFIN=R

97    CONTINUE

c --- Complete trajectory out to "15R" if required, to account for
c --- streamline slope (vertical only)
      xfin0=xfin
      x15r=r15src-xfin0
      if(x15r.GT.0.0) then
c ---    Set stepsize
         dsfin=nstep*ds
         dx15r=x15r/(mxnw-nn)
         dx15r=AMAX1(dsfin,dx15r)
c ---    Set range for additional steps
         n15r=MIN(x15r/dx15r,mxnw-nn)
         nbeg=nn+1
         nend=nn+n15r
         do in=nbeg,nend
c ---       Define coordinates of plume relative to bldg.-defined origin
            xbb=xt(in-1)-xbadj
            xbe=xbb+dx15r
            yb=yt(in-1)-ybadj
            zb=zt(in-1)
c ---       Obtain mean streamline slope
            dzdx=0.0
            call POSITION(xbb,yb,zb,ipos)
            if(ipos.GT.2) then
               call ZSTREAM(hb,wb,xLb,rb,xLr,hr,xbb,yb,zb,dzdxb)
               call ZSTREAM(hb,wb,xLb,rb,xLr,hr,xbe,yb,zb,dzdxe)
               dzdx=0.5*(dzdxb+dzdxe)
            endif
            xt(in)=xt(in-1)+dx15r
            yt(in)=yfin
            zt(in)=AMAX1(zmin,zt(in-1)+dzdx*dx15r)
            rt(in)=rfin
            zcumul=zcumul+dzdx*dx15r

c ---       Check for wake entry if this has not already happened
            if(.not.LINWAKE) then
               if(ipos.LT.4) then
c ---             Plume centerline has just entered wake
                  linwake=.TRUE.
c ---             Set "internal" variable names
                  x=xt(in)
                  z=zt(in)-zcumul
                  xbi=x-xbadj
c ---             Use unadjusted rise for BID
                  base=AMAX1(H,ZMIN)
                  rise=AMAX1(0.0,z-base)
                  bidsq=(rise/3.5)**2
                  call SIGZ(x,szi)
                  szi=SQRT(szi**2+bidsq)
                  call SIGY(x,syi)
                  syi=SQRT(syi**2+bidsq)

c --- Normal debug output
                  if(ldbhr) then
                     write(io6,*)'NUMRISE call to WAKE_DFSN'
                     write(io6,*)'x,y,z,z+zcum:',x,yfin,z,zt(in)
                     write(io6,*)'xb,phi      :',xbi,phi
                     write(io6,*)'szi,syi     :',szi,syi
                  endif

c ---             Compute table of sigmas and growth rate in wake region
                  call WAKE_DFSN(ldb,xbi,szi,syi)
               endif
c ---          Select plume radius growth rate for this location
               call WAKE_DRDX(x,drdxa)
            endif

         enddo
c ---    Update nn and reset "fin" data
         nn=nend
         xfin=xt(nn)
         zfin=zt(nn)
      endif

c --- Construct trajectory arrays for calling program
      if(nn.GT.ntr) then
c ---    Sample a subset of the nn points
         xtr(ntr)=xfin
         ytr(ntr)=yfin
         ztr(ntr)=zfin
         rtr(ntr)=rfin
         if(nn.LE.2*ntr) then
c ---       Fill elements with nearest values
            deln=FLOAT(nn)/FLOAT(ntr)
            do in=1,ntr-1
               jn=in*deln
               xtr(in)=xt(jn)
               ytr(in)=yt(jn)
               ztr(in)=zt(jn)
               rtr(in)=rt(jn)
            enddo
         else
c ---       Use sliding step-size to sample nearfield more frequently
            deln=2.*FLOAT(nn-ntr)/FLOAT(ntr*(ntr-1))
            rn=0.0
            do in=1,ntr-1
               rn=rn+1.0+(in-1)*deln
               jn=rn
               xtr(in)=xt(jn)
               ytr(in)=yt(jn)
               ztr(in)=zt(jn)
               rtr(in)=rt(jn)
            enddo
         endif
      else
c ---    Fill elements directly
         do in=1,nn
            xtr(in)=xt(in)
            ytr(in)=yt(in)
            ztr(in)=zt(in)
            rtr(in)=rt(in)
         enddo
c ---    Fill excess elements with final rise properties
         do it=nn+1,ntr
            xtr(it)=xfin
            ytr(it)=yfin
            ztr(it)=zfin
            rtr(it)=rfin
         enddo
      endif

c --- Restore step size (may have changed)
      ds=ds0

c --- Determine maximum fraction of plume captured in cavity
      if(LINWAKE .AND. xbi.LT.(xLb+xLR)) then
c ---    Plume centerline enters wake boundary before clearing cavity
         call WAKE_FQC(ldb,xbi,xtr,ztr,mxntr)
      else
         fqcav=0.0
      endif

c --- Normal debug output
      if(ldbhr) then
         delzfin=zfin-h
         write(io6,*)
         write(io6,*)'      Initial Plume Temperature = ',texit
         write(io6,*)'             Buoyancy flux (FB) = ',fb
         write(io6,*)'             Momentum flux (FM) = ',fm
         write(io6,*)'  Neutral dist. to final rise   = ',xmax
         write(io6,*)'  Calc distance to final rise   = ',xfin0
         write(io6,*)'Distance from final rise to 15R = ',x15r
         write(io6,*)'Total distance tabulated (XFIN) = ',xfin
         write(io6,*)'    Final Y displacement (YFIN) = ',yfin
         write(io6,*)'      Final plume height (ZFIN) = ',zfin
         write(io6,*)'     Final plume rise (DELZFIN) = ',delzfin
         write(io6,*)'      Final plume radius (RFIN) = ',rfin
         write(io6,*)'Cumul. streamline adj. (ZCUMUL) = ',zcumul
         write(io6,*)
         write(io6,*)'    Fraction of plume in CAVITY = ',fqcav
         write(io6,*)
      endif
c

c --- Extended debug output
      if(ldb) then
c ---    Write the arrays passed back to the calling routine
         write(io6,28)
28       format(/4x,'I',10x,'XTR',8x,'YTR',8x,'ZTR',8x,'RTR',8x,'sz?'/)
         do i=1,ntr
            write(io6,32)i,xtr(i),ytr(i),ztr(i),rtr(i),(rtr(i)*0.8)
32          format(i5,3x,5(f10.4,1x))
         enddo
         write(io6,*)
      endif

      RETURN
      END
c----------------------------------------------------------------------
      subroutine rate(ua,dudz,ra,dpdz,ta,drdxa,rhs)
c----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812                  RATE
c                X. Zhang, J. Scire,   EARTH TECH
c
c                Adapted from CALPUFF routine RATE
c                for EPRI under contract WO3527-01
c
c --- PURPOSE:  Compute the right-hand side of the equations
c
c --- INPUTS:
c         UA - real    - Current ambient wind speed (m/s)
c       DUDZ - real    - Current wind shear (1/s)
c         RA - real    - Current ambient air density (kg/m**3)
c       DPDZ - real    - Current ambient potential temperature gradient
c                        (deg. K/m)
c         TA - real    - Current ambient air temperature (deg. K)
c     ALPHA0 - real    - Plume entrainment coefficient (parallel)
c      DRDXA - real    - Growth rate of plume radius due to ambient turb
c
c     Common block /PLU/ variables:
c           X,R,U,V,W,USC,PHI,DEN,TP
c     Common block /NUMPARM/ variables:
c           GRAVI, RP,
c           NENT, ALPHAP(mxent), BETAP(mxent), XCAT(mxentp1),
c     Parameters:
c           MXENT, MXENTP1
c
c --- OUTPUT:
c        RHS(7) - real     - Right hand terms
c
c --- RATE called by:  NUMRISE
c --- RATE calls:      none
c----------------------------------------------------------------------
c --- Include files
      include 'params.pri'
      include 'numparm.pri'
      COMMON /PLU/ S,X,Y,Z,R,U,V,W,USC,PHI,DEN,TP
      DIMENSION RHS(7)
c ---   Constants:
c          GRAVI - Gravitational acceleration (m/s**2)
c          RP    - Radiation coefficient (kg/m**2/deg. K**3/s)
c --- Set default entrainment coefficients
      data alpha0/.11/, beta0/0.6/
c --- Define the entrainment coefficients
      alpha=alpha0
      beta=beta0
      if(nent.gt.0)then
c ---    Check if the plume is in the area where perturbed entrainment
c ---    coefficients apply
         if(x.lt.xcat(1))go to 99
         nentp1=nent+1
         do i=2,nentp1
            if(x.le.xcat(i))then
               alpha=alphap(i-1)
               beta=betap(i-1)
               go to 99
            endif
         enddo
c ---    Override any ambient growth rate
         drdxa=0.0
      endif
99    continue
        RHS(1)=2.0*R*ALPHA*RA*ABS(USC-UA*U/USC)+
     1                 2.0*R*BETA*RA*ABS(UA*SIN(PHI))
c
c ---   Condition entrainment to be .GE. growth due to ambient turb.
        rhs1a =2.0*R*RA*UA*drdxa
        rhs(1)=AMAX1(rhs(1),rhs1a)
c
        RHS(2)=-R*R*DEN*W*DUDZ
        RHS(3)=GRAVI*R*R*(RA-DEN)
        RHS(4)=-DPDZ*DEN*W*R*R-RP*R*(TP**4-TA**4)
        RHS(5)=W/USC
        RHS(6)=U/USC
        rhs(7)=v/usc
        RETURN
        END
c----------------------------------------------------------------------
      subroutine lump(ua,ta,f)
c----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812                  LUMP
c                X.(J.) Zhang, J. Scire,  EARTH TECH
c
c --- PURPOSE:  Calculate the lumped variables
c
c --- INPUTS:
c         UA - real    - Current ambient wind speed (m/s)
c         TA - real    - Current ambient air temperature (K)
c
c --- OUTPUT:
c          F(7) - real     - lumped variables
c
c --- LUMP called by:  NUMRISE
c --- LUMP calls:      none
c----------------------------------------------------------------------
        COMMON /PLU/ S,X,Y,Z,R,U,V,W,USC,PHI,DEN,TP
        DIMENSION F(7)
        F(1)=DEN*USC*R*R
        F(2)=F(1)*(U-UA)
        F(3)=F(1)*W
        F(4)=F(1)*(TP-TA)
        F(5)=Z
        F(6)=X
        F(7)=y
        RETURN
        END
c----------------------------------------------------------------------
      subroutine marching(fold,fnew,rhs,ds)
c----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812              MARCHING
c                X.(J.) Zhang, J. Scire,  EARTH TECH
c
c --- PURPOSE:  Marching S one step, either PREDICTOR or CORRECTOR
c
c --- INPUTS:
c       FOLD(7) - real     - Old values
c        RHS(7) - real     - Right hand terms
c            DS - real     - Distance (m) along plume axis
c
c
c --- OUTPUT:
c       FNEW(7) - real     - New values
c
c --- MARCHING called by:  NUMRISE
c --- MARCHING calls:      none
c----------------------------------------------------------------------
        DIMENSION FNEW(7),FOLD(7),RHS(7)
        DO 10 I=1,7
           FNEW(I)=FOLD(I)+RHS(I)*DS
10      CONTINUE
        RETURN
        END
c----------------------------------------------------------------------
      subroutine unlump(ua,ta,ra,f)
c----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812                UNLUMP
c                X.(J.) Zhang, J. Scire,  EARTH TECH
c
c --- PURPOSE:  Calculate physical variables from lumped variables
c
c --- INPUTS:
c         UA - real    - Current ambient wind speed (m/s)
c         TA - real    - Current ambient air temperature (K)
c         RA - real    - Current ambient air density (kg/m^3)
c       F(7) - real    - Lumped variables
c
c --- OUTPUT:
c       common /PLU/:
c          U,V,W,USC,R,TP,PHI,Z,Y,X
c
c --- UNLUMP called by:  NUMRISE
c --- UNLUMP calls:      none
c----------------------------------------------------------------------
        COMMON /PLU/ S,X,Y,Z,R,U,V,W,USC,PHI,DEN,TP
        DIMENSION F(7)
        U=UA+F(2)/F(1)
        W=F(3)/F(1)
        USC=SQRT(U*U+W*W)
        TP=TA+F(4)/F(1)
        DEN=RA*TA/TP
        R=SQRT(F(1)/USC/DEN)
        PHI=ATAN(W/U)
        Z=F(5)
        X=F(6)
        Y=F(7)
        RETURN
        END
c----------------------------------------------------------------------
      subroutine zmet(z,ua,ra,ta,dudz,dpdz)
c----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812                  ZMET
c                X.(J.) Zhang, J. Scire,  EARTH TECH
c
c --- PURPOSE:  Obtain ambient met parameters at height z
c               by interpolation of gridded values
c
c --- INPUTS:
c          Z - real    - Height (m)
c
c     Common block /AMBIENT/ variables:
c           NZA,UAMB(mxnz),RAMB(mxnz),TAMB(mxnz),ZFACEA(mxnzp1),
c           ZGPTA(mxnz),TAMB0,RAMB0,ADIA,PTGRAD0
c     Parameters:
c           MXNZ, MXNZP1
c
c --- OUTPUT:
c         UA - real    - Current ambient wind speed (m/s)
c         RA - real    - Current ambient air density (kg/m**3)
c         TA - real    - Current ambient air temperature (deg. K)
c       DUDZ - real    - Current wind shear (1/s)
c       DPDZ - real    - Current ambient potential temperature gradient
c                        (deg. K/m)
c
c --- ZMET called by:  NUMRISE
c --- ZMET calls:      none
c----------------------------------------------------------------------
c     Defined at grid center: uamb,tamb,ramb
c     Defined at zface:       dedz
c----------------------------------------------------------------------
c --- Include files
      include 'params.pri'
      include 'ambient.pri'

c --- Interpolate variables defined at grid cell center
      if(z.lt.zgpta(1))then

c ---    Height is below first grid point
         zfact=(zgpta(1)-z)/zgpta(1)
         ta=tamb(1)-(tamb(1)-tamb0)*zfact
         ra=ramb(1)-(ramb(1)-ramb0)*zfact
c ---    Wind speed at z=0 is assumed to be zero
         ua=uamb(1)*(1.0-zfact)
         dudz=uamb(1)/zgpta(1)
         dpdz=adia+(tamb(1)-tamb0)/zgpta(1)
         dpdz=amax1(dpdz,ptgrad0)

      else if(z.lt.zgpta(nza))then

c ---    Find the layer containing height, Z
         do i=2,nza
            if(z.le.zgpta(i))then
               im1=i-1
               delz=zgpta(i)-zgpta(im1)
               zfact=(zgpta(i)-z)/delz
               ta=tamb(i)-(tamb(i)-tamb(im1))*zfact
               ra=ramb(i)-(ramb(i)-ramb(im1))*zfact
               ua=uamb(i)-(uamb(i)-uamb(im1))*zfact
c ---          Compute wind speed gradient & pot. temp. gradient
               dudz=(uamb(i)-uamb(im1))/delz
               dpdz=adia+(tamb(i)-tamb(im1))/delz
               dpdz=amax1(dpdz,ptgrad0)
               go to 101
            endif
         enddo

      else

c ---    Height is at or above the top grid point -- persist values
c ---    at the top grid cell
         ta=tamb(nza)
         ra=ramb(nza)
         ua=uamb(nza)
c ---    Hold wind speed and temperature constant above top layer at
c ---    values at top grid point
         dudz=0.0
         dpdz=adia
         dpdz=amax1(dpdz,ptgrad0)
      endif

101   continue

      return
      end
c----------------------------------------------------------------------
      subroutine nummet(ws,zanem,p,tsurf,ptgrad,ldbhr)
c----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812                NUMMET
c                J. Scire, EARTH TECH
c
c --- PURPOSE:  Initialize the variables used by the numerical
c               plume rise algorithm
c
c --- INPUTS:
c
c         WS - real    - Wind speed (m/s) measured at anemometer ht.
c      ZANEM - real    - Anemometer height (m)
c          P - real    - Power law wind shear exponent (dimensionless)
c      TSURF - real    - Surface ambient air temperature (deg. K)
c     PTGRAD - real    - Ambient potential temperature lapse rate (K/m)
c      LDBHR - logical - Debug switch controlling debug output
c
c     Common block /AMBIENT/ variables:
c           NZA,ZFACEA(mxnzp1),ZGPTA(mxnz),ADIA,PTGRAD0
c     Common block /NUMPARM/ variables:
c           GRAVI,RGAS
c     Parameters:
c           MXNZ, MXNZP1
c
c --- OUTPUT:
c
c     Common block /AMBIENT/ variables:
c           UAMB(mxnz),RAMB(mxnz),TAMB(mxnz),DEDZ(mxnzp1),
c           TAMB0,RAMB0
c
c --- NUMMET called by:  PRIME
c --- NUMMET calls:      none
c----------------------------------------------------------------------

c --- Include parameters
      include 'params.pri'

      logical ldbhr

c --- Include common blocks
      include 'ambient.pri'
      include 'numparm.pri'

c --- Set the surface temperature (deg. K) & air density (kg/m**3)
      tamb0=tsurf
      ramb0=1.2

c --- Extrapolate the winds to the grid point levels
      do i=1,nza
         uamb(i)=ws*(zgpta(i)/zanem)**p
      enddo

c --- Compute the temperature at each grid point

c ---    Do not allow the potential temp. gradient < min. value
         gradact=amax1(ptgrad,ptgrad0)
         do i=1,nza
            tamb(i)=tamb0+(gradact-adia)*zgpta(i)
         enddo

c ---    Compute the potential temperature lapse rates at cell faces
         nzap1=nza+1
         do i=2,nza
            dedz(i)=adia+(tamb(i)-tamb(i-1))/(zgpta(i)-zgpta(i-1))
         enddo
         dedz(1)=adia+(tamb(1)-tamb0)/zgpta(1)
         dedz(nzap1)=dedz(nza)

c --- Create ambient density profile on grid center
      do i=1,nza
c ---    Compute average temperature in layer
         tbar=0.5*(tamb(i)+tamb0)
c ---    Compute ambient air density at height, ZGPTA(i)
         ramb(i)=ramb0*(tamb0/tamb(i))*exp(-gravi*zgpta(i)/
     1    (rgas*tbar))
      enddo
c
c ----------------
c --- Debug output
c ----------------
      if(ldbhr)then

         write(io6,*)
         write(io6,*)'WS      = ',ws,' (m/s)'
         write(io6,*)'ZANEM   = ',zanem,' (m)'
         write(io6,*)'P       = ',p
         write(io6,*)'TSURF   = ',tsurf,' (deg. K)'
         write(io6,*)'PTGRAD  = ',ptgrad,' (deg. K/m)'
         write(io6,*)'NZA     = ',nza
         write(io6,*)'ADIA    = ',adia,' (deg. K/m)'
         write(io6,*)'PTGRAD0 = ',ptgrad0,' (deg. K/m)'
         write(io6,*)

         write(io6,*)'TAMB0   = ',tamb0,' (deg. K)'
         write(io6,*)'RAMB0   = ',ramb0,' (kg/m**3)'
         write(io6,*)

         write(io6,8)
8        format(/4x,'I',8x,'ZGPTA',7x,'UAMB',7x,'TAMB'/)
         do i=1,nza
            write(io6,10)i,zgpta(i),uamb(i),tamb(i)
10          format(i5,3x,f10.1,1x,f10.2,1x,f10.2)
         enddo

c ---    Echo back pot. temp. gradient & air density
         write(io6,18)
18       format(/4x,'I',7x,'ZFACEA',7x,'DEDZ',6x,'ZGPTA',7x,'RAMB'/)
         do i=1,nza
            write(io6,22)i,zfacea(i),dedz(i),zgpta(i),ramb(i)
22          format(i5,3x,f10.1,1x,f10.5,1x,f10.1,1x,f10.3)
         enddo
         write(io6,22)nzap1,zfacea(nzap1),dedz(nzap1)
      endif

      return
      end
c----------------------------------------------------------------------
      subroutine zstream(H,W,L,R,LR,HR,x,y,z,dzdx)
c----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812               ZSTREAM
c                L. Schulman, J. Scire,   EARTH TECH
c                Prepared for EPRI under contract WO3527-01
c
c --- PURPOSE:  Estimates the local mean slope of streamlines in the
c               vicinity of buildings.  The local slope is equal to
c               w/u, where w is the local mean vertical velocity and
c               u the local mean longitudinal velocity.  For modeling
c               plume rise, the streamline deflection effect is modeled
c               as (w/u)(dx).
c
c --- INPUTS:
c                H - real              - Building height above ground
c                W - real              - Projected building width
c                L - real              - Along-wind building length
c                R - real              - Scale length from H and W
c               LR - real              - Length of downwind cavity from
c                                         lee face
c               HR - real              - Maximum cavity height above
c                                         ground
c                x - real              - downwind distances
c                y - real              - crosswind distances
c                z - real              - heights above ground
c
c --- OUTPUT:
c             dzdx - real              - Vertical streamline slope
c
c --- ZSTREAM called by:  NUMRISE
c --- ZSTREAM calls:      none
c----------------------------------------------------------------------
c
      real H,W,L,R,HR,LR,expz1,expz2,expzg,expx
      real x,y,z,dzdx2,zslope,dzdx,ypos
      data expx/1./,expz1/3./,expz2/1./
c
c --- Check for a building
      if(h.le.0.0)then
         dzdx=0.0
         go to 900
      endif

c --- Set a few constants
      hbyw=H/W
      ypos=abs(y)
      onebyr=1.0/R
      wby2=0.5*W
      rby2=0.5*R

c --- Power law exponent for slope approaching zero at ground
c --- Exponent modified for tall, narrow buidings
c --- zg is level below which slope is zero in far wake
      zg=0.
      expzg=0.3
      if(hbyw .ge. 2.) expzg=expzg*(0.5*hbyw)**2

c
c --- Local streamline slope (zslope) at z=H
c --- Local two-dimensional streamline slope (dzdx2)
c --- Local three-dimensional streamline slope (dzdx)
c --- (x,y,z)=(0,0,0) at ground of center of upwind building face
c
      if(x .lt. -R) then
c ---    Upwind of building influence
         zslope = 0.
         dzdx2  = 0.

      elseif(x .lt. 0.) then
c ---    Ascent upwind of building:
c ---    parobolic fit to slope=0 at (-R,0) with increasing slope
c ---    to (0,(HR-H))
c ---    vertical decay above building using expz1
c ---    below building nonzero slope above 2/3 H for R<H reducing
c ---    to ground as R approaches 2H
         zslope = 2.*(HR-H)*(x+R)*onebyr**2.
         if(z .gt. H) then
            dzdx2 = zslope/((z-H+R)*onebyr)**expz1
         elseif(R .le. H .and. z .le. 0.67*H) then
            dzdx2 = 0.
         elseif(R .le. H .and. z .gt. 0.67*H) then
            dzdx2 = zslope
         elseif(R .gt. H .and. z .le. 0.67*(2*H-R)) then
            dzdx2 = 0.
         elseif(R .gt. H .and. z .gt. 0.67*(2*H-R)) then
            dzdx2 = zslope
         else
            print *,'z out of bounds      ',x,z
         endif

      elseif(x .le. rby2) then
c ---    Ascent over building
c ---    parobolic fit from (0,0) with decreasing slope to
c ---    to (0.5R,(HR-H))
c ---    vertical decay above building using expz1
         zslope = (-(HR-H)*4.*onebyr)*(2.*x*onebyr-1.)
         if(z .le. H) then
            dzdx2 = zslope
         else
            dzdx2 = zslope/((z-H+R)*onebyr)**expz1
         endif

      elseif(x .le. L+LR) then
c ---    Descent over building to end of near wake
c ---    parobolic fit from (.5R,(HR-H)) with increasing slope to
c ---    to (L+LR,-H/2)
c ---    vertical decay above z=H using expz2
c ---    vertical decay below z=H using expzg
         zslope = (HR-H)*(R-2.*x)/((L-rby2+LR)**2)
         if(z .gt. H) then
            dzdx2 = zslope/((z-H+R)*onebyr)**expz2
         elseif(z .le. zg) then
            dzdx2 = 0.
         else
            dzdx2 = zslope*((z-zg)/(H-zg))**expzg
         endif

      else
c ---    Descent beyond near wake (far wake)
c ---    horizontal decay beyond L+LR using expx
c ---    vertical decay above z=H using expz2
c ---    vertical decay below z=H using expzg
         zslopeLR  = -2.*(HR-H)/(L-rby2+LR)
         zslope = zslopeLR/((x-(L+LR-R))*onebyr)**expx
         if(z .gt. H) then
            dzdx2 = zslope/((z-H+R)*onebyr)**expz2
         elseif(z .le. zg) then
            dzdx2 = 0.
         else
            dzdx2 = zslope*((z-zg)/(H-zg))**expzg
         endif

      endif

c --- Calculate 3-D slopes,: dzdx : from 2-d centerplane slope,: dzdx2
      if(ypos .gt. (wby2+R/3.))then
         dzdx=0.
      elseif(ypos .le. wby2)then
         dzdx=dzdx2
      else
         yscale=1.+(3.*onebyr)*(wby2-ypos)
         dzdx=dzdx2*yscale
      endif

900   continue
      return
      end
c-----------------------------------------------------------------------
      subroutine position(x,y,z,ipositn)
c-----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812              POSITION
c                L. Schulman, J. Scire, D. Strimaitis,   EARTH TECH
c                Prepared for EPRI under contract WO3527-01
c
c --- PURPOSE:  Identifies if (x,y,z) location is in the building,
c               in the near wake, in the far wake, or outside.
c               IPOSITN is set to: 1 if within the bldg.
c                                  2 if within the near wake
c                                  3 if within the far wake
c                                  4 if outside wake region
c
c --- INPUTS:
c                x - real              - downwind distance from center
c                                        of upwind face of bldg
c                y - real              - crosswind distance from center
c                                        of upwind face of bldg
c                z - real              - height above ground
c
c     Common block /WAKEDAT/ variables:
c           Hb,Wb,xLb,Rb,HR,xLR,xLC
c
c --- OUTPUT:
c
c          ipositn - integer           - 1 if (x,y,z) within building
c                                        2 if location within near wake
c                                        3 if location within far wake
c                                        4 if location outside
c
c --- POSITION called by:  NUMRISE, PCALC (HOST subroutine)
c --- POSITION calls:      WAKE_DIM
c----------------------------------------------------------------------
c
c --- Include parameters
      include 'params.pri'

c --- Include commons
      include 'wakedat.pri'

      data zero/0.0/, third/0.333333/, half/0.5/

c --- Define a fractional boundary just inside building
      data skin/0.99998/

c --- Initialize
      iposy=4
      iposz=4
      ipositn=4

c --- Screen out any cases without building
      if(Hb.le.zero) return

c --- Screen out positions upwind of building (and on windward face)
      if(x.le.zero) return

c --- Set y positive for calculations
      ypos=ABS(y)

c --- Set selected length scale products
      rby2=half*Rb
      rby3=third*Rb
      wby2=half*Wb

c --- Set ipositn to 1 if location within building
      xtest=x/xLB
      ytest=ypos/wby2
      ztest=z/Hb
      if(xtest.lt.skin .and. ztest.lt.skin .and. ytest.lt.skin) then
         ipositn=1
         return
      endif

c --- Calculate if location below height of near wake boundary
      if(xLC .lt. xLb)then
c ---    Reattachment
         if(x.lt.xLb) then
c ---       Cavity height equal to building height
            zcav=Hb
            if(z .le. zcav) iposz=2
         elseif(x.lt.(xLb+xLR)) then
c ---       Cavity height is ellipse with a=LR and b=H
            zcav=Hb*SQRT(1.-((x-xLb)/xLR)**2)
            if(z .le. zcav) iposz=2
         endif
      else
c ---    No reattachment
         if(x.le.rby2) then
c ---       Cavity height is parabola with vertex at height MAX(0.5R,HR)
c ---       and passing thru upwind building edge (0,H)
            zcav=HR+4.*(x-rby2)**2*(Hb-HR)/(Rb**2)
            if(z .le. zcav) iposz=2
         elseif(x.lt.(xLb+xLR)) then
c ---       Cavity height is ellipse with a=LR+L-0.5R and b=HR
            zcav=HR*SQRT(1.-((x-rby2)/(xLb+xLR-rby2))**2)
            if(z .le. zcav) iposz=2
         endif
      endif

c --- Calculate x-y near wake boundary
      if(x.le.Rb) then
c ---    Cavity width is parabola with vertex @ width MAX(R,W/2+R/3)
c ---    and passing thru upwind building edge (0,W/2)
         ycav=(wby2+rby3)-(x-Rb)**2/(3.*Rb)
         if(ypos .le. ycav) iposy=2
      elseif(x.lt.(xLb+xLR)) then
c ---    Cavity width is ellipse with a=W/2+R/3 and b=LR+L-R
         ycav=(wby2+rby3)*SQRT(1.-((x-Rb)/(xLb+xLR-Rb))**2)
         if(ypos .le. ycav) iposy=2
      endif

c --- Set ipositn to 2 if (x,y,z) location within near wake
      if( iposz .eq. 2 .and. iposy .eq. 2) ipositn=2

c --- Test for position in far wake if still 4
      if(ipositn.EQ.4) then
         call WAKE_DIM(x,Hb,Wb,Rb,zwake,ywake)
         if(z.le.zwake .AND. ypos.le.ywake) ipositn=3
      endif

      return
      end
c----------------------------------------------------------------------
      subroutine numgrad(x,xtr,ztr,ntr,zeff)
c----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812               NUMGRAD
c                J. Scire,  EARTH TECH
c                Prepared for EPRI under contract WO3527-01
c
c --- PURPOSE:  Compute the effective gradual plume height by
c               interpolation of the stored values.  Effective
c               plume height is the stack height + plume rise.
c
c --- INPUTS:
c                X - real       - Downwind distance (m)
c         XTR(ntr) - real array - Array of stored downwind distances (m)
c         ZTR(ntr) - real array - Array of stored effective plume height
c                                 at each downwind distance
c              NTR - integer    - Number of stored values in XTR, ZTR
c
c --- OUTPUT:
c             ZEFF - real       - Effective plume height (m) at
c                                 downwind distance X
c
c --- NUMGRAD called by:  PHEFF
c --- NUMGRAD calls:      none
c----------------------------------------------------------------------
c
      real xtr(ntr),ztr(ntr)
c
      if(x.ge.xtr(ntr))then
         zeff=ztr(ntr)
      else
         ntrm1=ntr-1
         zeff=ztr(1)
         do i=ntrm1,1,-1
            if(x.ge.xtr(i))then
               ip1=i+1
               zeff=ztr(ip1)-(ztr(ip1)-ztr(i))*(xtr(ip1)-x)/
     1          (xtr(ip1)-xtr(i))
               return
            endif
         enddo
      endif

      return
      end
c----------------------------------------------------------------------
      subroutine wake_drdx(x,drdx)
c----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812             WAKE_DRDX
c                J. Scire, D. Strimaitis,  EARTH TECH
c                Prepared for EPRI under contract WO3527-01
c
c --- PURPOSE:  Compute the plume radius growth rate in the wake
c               by interpolating among the stored values.
c
c --- INPUTS:
c                X - real       - Downwind distance (m) from source
c
c     Common block /PARAMS/ variables:
c           MXNTR
c     Common block /WAKEDAT/ variables:
c           NWAK, XWAK(mxntr), DRWAK(mxntr)
c
c --- OUTPUT:
c             DRDX - real       - Rate of growth of plume radius at
c                                 downwind distance X from source
c
c --- WAKE_DRDX called by:  NUMRISE
c --- WAKE_DRDX calls:      none
c----------------------------------------------------------------------
c
      include 'params.pri'
      include 'wakedat.pri'
c
c --- Set growth rate to zero outside interpolation region
c --- (all x outside wake)
      if(x.gt.xwak(nwak) .OR. x.lt.xwak(1))then
         drdx=0.0
      elseif(nwak.le.1) then
c ---    Wake turbulence does not alter this plume
         drdx=0.0
      else
         nwkm1=nwak-1
         drdx=drwak(1)
         do i=nwkm1,1,-1
            if(x.ge.xwak(i))then
               ip1=i+1
               drdx=drwak(ip1)-(drwak(ip1)-drwak(i))*(xwak(ip1)-x)/
     1              (xwak(ip1)-xwak(i))
               return
            endif
         enddo
      endif

      return
      end
c----------------------------------------------------------------------
      subroutine wake_ini(ldbhr,kst,rural,dsbh,dsbw,dsbl,
     &                    xadj,yadj,ubldg,ustack)
c----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812              WAKE_INI
c                D. Strimaitis, EARTH TECH
c                Prepared for EPRI under contract WO3527-01
c
c --- PURPOSE:  Refreshes variables in /wakedat/ common
c
c --- INPUTS:
c
c      LDBHR - logical - Debug output written when .TRUE.
c        KST - integer - PG stability class
c      RURAL - logical - Denotes rural dispersion when .TRUE.
c       DSBH - real    - Effective building height (m)
c       DSBW - real    - Effective building width (m) across flow
c       DSBL - real    - Effective building length (m) along flow
c       XADJ - real    - Distance (m) from source to upwind face of bldg
c                        along flow
c       XADJ - real    - Distance (m) from source to center of upwind
c                        face of bldg across flow
c      UBLDG - real    - Wind speed (m/s) at top of building
c     USTACK - real    - Wind speed (m/s) at release height
c
c     Parameters:
c           MXNTR
c
c --- OUTPUT:
c
c     Common block /WAKEDAT/ variables:
c           HB,WB,XLB,RSCALE,HR,XLR,XLC,XBADJ,YBADJ,
c           NWAK, XWAK(mxntr), SZWAK(mxntr), SYWAK(mxntr),
c           DRWAK(mxntr), XZVWAK, XYVWAK, UB, URH,
c           LRURL, ISTAB
c
c --- WAKE_INI called by:  PCALC (HOST subroutine)
c --- WAKE_INI calls:      WAKE_SCALES
c----------------------------------------------------------------------
c --- Include parameters
      include 'params.pri'

c --- Include common blocks
      include 'wakedat.pri'

      logical rural,ldbhr
      data zero/0.0/

c --- Transfer arguments to /wakedat/ variables
      istab =kst
      lrurl =rural
      Hb    =dsbh
      Wb    =dsbw
      xLb   =dsbl
      xbadj =xadj
      ybadj =yadj
      Ub    =ubldg
      Urh   =ustack

c --- Compute wake dimensions and related parameters
      call WAKE_SCALES(ldbhr)

c --- Reset contents of sigma arrays for wake region
      nwak=1
      xwak(1)=zero
      szwak(1)=zero
      sywak(1)=zero
      drwak(1)=zero

c --- Reset virtual distances for sigmas beyond wake
      xzvwak=zero
      xyvwak=zero

      return
      end
c-----------------------------------------------------------------------
      subroutine wake_scales(ldbhr)
c-----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812           WAKE_SCALES
c                L. Schulman, D. Strimaitis,  J. Scire,   EARTH TECH
c                Prepared for EPRI under contract WO3527-01
c
c --- PURPOSE:  Calculates length scale and wake dimensions
c
c --- INPUTS:
c            LDBHR - logical           - Control variable for debug
c                                        write statements
c
c     Common block /WAKEDAT/ variables:
c           Hb,Wb,xLb
c     Parameters:  IO6
c
c --- OUTPUT:
c
c     Common block /WAKEDAT/ variables:
c           Rb,HR,xLR,xLC
c
c --- WAKE_SCALES called by:  WAKE_INI
c --- WAKE_SCALES calls:      none
c----------------------------------------------------------------------
c
c --- Include parameters
      include 'params.pri'
c
c --- Include commons
      include 'wakedat.pri'

      logical ldbhr

c --- Set misc. constants
      data third/0.3333333/, twoby3/0.6666667/

      if(HB.le.0.) then
c ---    No building
         Rb=0.0
         Hb=0.0
         xLR=0.0
         xLC=0.0
      else
c
c ---    Set ratios
         rw = Wb/Hb
         rl = xLb/Hb
c ---    Fackrell limits on aspect ratio L/H
         if(rl .lt. 0.3) rl=0.3
         if(rl .gt. 3.0) rl=3.0
c
c ---    Length scale R --- Wilson
c ---    Wilson limits for length scale R
c ---    H/W or W/H not greater than 8 --  already behaves as 2-D
         HH=Hb                    ! only modify H to calculate R
         WW=Wb                    ! only modify W to calculate R
         if(HH.gt.8.0*WW)HH=8.0*WW
         if(WW.gt.8.0*HH)WW=8.0*HH
         Rb= (amin1(HH,WW)**twoby3) * (amax1(HH,WW)**third)
c
c ---    Reattachment for LC < L
         xLC = 0.9*Rb
c
c ---    Recirculation cavity length---Fackrell
c ---    Modify Fackrell for W/H less than 1 by weakening dependence
c ---    on L/H.  Snyder found that cavity did not increase in length
c ---    as W/H = L/H decreased from 1 to 0.33.
c ---    Let L/H dependence decrease from Fackrell dependence at W/H=1
c ---    to no dependence at W/H=0.33.
         explh = 0.3
         if(rw .lt. 1.) explh=AMAX1(0.0,0.3*(rw-0.33)/0.67)
         xLR = 1.8*Wb/(rl**explh*(1.+0.24*rw))
c
c ---    Maximum cavity height  (Wilson,ASHRAE):
         HR = Hb+0.22*Rb

      endif

c --- Write the results
      if(ldbhr)then
         write(io6,*)
         write(io6,*)'WAKE_SCALES inputs: '
         write(io6,*)'   HB    = ',Hb,' (m)'
         write(io6,*)'   WB    = ',Wb,' (m)'
         write(io6,*)'   LB    = ',xLb,' (m)'
         write(io6,*)
         write(io6,*)'WAKE_SCALES output: '
         write(io6,*)'   Scale length (R)               = ',Rb
         write(io6,*)'   Max. cavity height (HR)        = ',HR
         write(io6,*)'   Length of downwind cavity (LR) = ',xLR
         write(io6,*)'   Length of roof cavity (LC)     = ',xLC
         write(io6,*)
      endif
c
      return
      end
c-----------------------------------------------------------------------
      subroutine wake_dfsn(ldbhr,xi,szi,syi)
c-----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812             WAKE_DFSN
c                L. Schulman, D. Strimaitis,   EARTH TECH
c                Prepared for EPRI under contract WO3527-01
c
c --- PURPOSE: Tabulates sigmas and rate of plume growth as function
c              of location within the wake from modified Weil (1996)
c              analytical expressions
c
c --- INPUTS:
c            ldbhr - logical     - Flag for debug write statements
c                                  to upwind bldg wall
c               xi - real        - distance (m) from upwind bldg wall
c                                  to point where plume intersects wake
c              szi - real        - sigma-z (m) at xi
c              syi - real        - sigma-y (m) at xi
c
c     Common block /PARAMS/ variables:
c           MXNTR, MXNW
c     Common block /WAKEDAT/ variables:
c           XBADJ, Hb, Wb, xLb, Rb, xLR
c
c --- OUTPUT:
c
c     Common block /WAKEDAT/ variables:
c           NWAK, XWAK(mxntr), SZWAK(mxntr), SYWAK(mxntr),
c           DRWAK(mxntr), XZVWAK, XYVWAK,
c           NCAV, XCAV(mxntr), SZCAV(mxntr), SYCAV(mxntr),
c           XZVCAV, XYVCAV, LRURL, ISTAB
c
c --- WAKE_DFSN called by:  NUMRISE
c --- WAKE_DFSN calls    :  SIGZ, SIGY, XVZ, XVY,
c                           WAKE_XA, WAKE_CAV0, WAKE_TURB, WAKE_SIG
c----------------------------------------------------------------------
c
      include 'params.pri'
      include 'numparm.pri'
      include 'wakedat.pri'

c --- Define local variable arrays for fine-steps
      real dist(mxnw),asigz(mxnw),asigy(mxnw),dsz(mxnw)
      real csigz(mxnw),csigy(mxnw)

      logical ldbhr,lcav,lwak,lrevcav

c --- Misc. constants
      data zero/0.0/, half/0.5/, one/1.0/
      data rtpiby2/1.253314/

c --- Compute xa, where turbulent growth in the wake transitions
c --- to ambient growth rate, measured from upwind face of bldg
      call WAKE_XA(istab,lrurl,xLb,Rb,xaz,xay)
      xamx=AMAX1(xaz,xay)
      xamn=AMIN1(xaz,xay)

c --- Initialize CAVITY parameters
c --------------------------------
c --- Set distance from upwind face of bldg to END of cavity
      xcave=xLb+xLR
c --- Set distance from source to start of cavity
      distc=xLb+xbadj
c --- Set downwind distance to effective cavity source (when present),
c --- from the upwind face of bldg
      xbc=AMAX1(xi,xLb)
      xbc=AMIN1(xbc,xcave)
c --- Location of downwind edge of PDF region from effective cavity
c --- source
      xdc=xbc+xLR
c --- Set initial sigmas for cavity source using sigma-y at xi
      call WAKE_CAV0(syi,szcav0,sycav0)
c --- The cavity sigma-y will need to be revised if xi lies upwind of
c --- the downwind face of the bldg.
      if(xi.LT.xLb) then
         lrevcav=.TRUE.
      else
         lrevcav=.FALSE.
      endif

c --- Determine if any plume material in cavity may be modeled
c ------------------------------------------------------------
c --- Initialize output arrays
      ncav=1
      xcav(1)=xbc+xbadj
      szcav(1)=szcav0
      sycav(1)=sycav0
c --- Compute corresponding virtual distances
      xasrc=xcav(1)
      call XVZ(szcav0,xasrc,xzvcav)
      xzvcav=xzvcav-xasrc
      call XVY(sycav0,xyvcav)
      xyvcav=xyvcav-xasrc
      if(xi.GE.xcave) then
         lcav=.FALSE.
         lrevcav=.FALSE.
      else
         lcav=.TRUE.
      endif

c --- Is plume affected by wake turbulence?
c -------------------------------------------
c --- Initialize output arrays
      nwak=1
      xwak(1)=xi+xbadj
      szwak(1)=szi
      sywak(1)=syi
      drwak(1)=zero
c --- Compute corresponding virtual distances
      xasrc=xwak(1)
      call XVZ(szi,xasrc,xzvwak)
      xzvwak=xzvwak-xasrc
      call XVY(syi,xyvwak)
      xyvwak=xyvwak-xasrc
      if(xi.GE.xamx) then
         lwak=.FALSE.
         if(ldbhr) then
            write(io6,*)'----- WAKE_DFSN:        NWAK = ',nwak
            write(io6,*)'Z-dispersion reaches ambient at: ',(xaz+xbadj)
            write(io6,*)'Y-dispersion reaches ambient at: ',(xay+xbadj)
            write(io6,*)'z,y virtual distances (m)    = ',xzvwak,xyvwak
            write(io6,*)'xadj, yadj, xi        (m)    = ',xbadj,ybadj,xi
            write(io6,*)'Plume NOT altered by wake turbulence!'
            write(io6,*)
         endif
      else
         lwak=.TRUE.
      endif

c --- Return now if sigmas in wake do not need to be tabulated
      if(.NOT.lwak .AND. .NOT.lcav) return

c --- Compute location of downwind edge of PDF region from xi
      xd=xi+xLR

c --- Set stepping parameters
      dx=2.0
c --- Range of table is from point of entry into wake (xi), to the point
c --- at which ambient growth rate resumes (xa), plus one "ds" so that
c --- both sigmas reach ambient, and virtual distances are computed.
c --- When cavity sigmas are also computed, range may start at the
c --- downwind bldg face, and extend to end of cavity.
      xlow=xi
      xhi=xamx
      if(lcav) then
         xlow=AMIN1(xi,xbc)
         xhi=AMAX1(xamx,xcave)
      endif
      xrange=xhi-xlow+dx
      np=NINT(xrange/dx)+1
      np=MIN(np,mxnw-1)
      dx=xrange/(FLOAT(np)-one)
      dxi=one/dx
      nws=0
      ncs=0

c --- Fill first element of marching arrays using values at xlow
      dist(1)=xlow+xbadj
      if(lwak) then
         asigz(1)=szi
         asigy(1)=syi
c ---    Set inital plume growth rate in wake to zero
         dsz(1)=zero
      endif
      if(lcav) then
         csigz(1)=szcav0
         csigy(1)=sycav0
      endif

c --- Initialize distance (from upwind face of bldg)
      x=xlow

c --- Loop over steps in wake region
c -----------------------------------
      do n=2,np
         xold=x
         x=x+dx
         dist(n)=dist(n-1)+dx

c ---    Check to see if cavity data should be revised based on
c ---    data from previous step
         if(lrevcav .AND. xold.GE.xLb) then
            call WAKE_CAV0(asigy(n-1),szcav0,sycav0r)
            if(sycav0r.GT.sycav0) then
               sycav0=sycav0r
               sycav(1)=sycav0
c ---          Compute corresponding virtual distance
               xasrc=xcav(1)
               call XVY(sycav0,xyvcav)
               xyvcav=xyvcav-xasrc
c ---          Replace sigma-y values in stepping arrays
               do ir=1,n-1
                  csigy(ir)=AMAX1(csigy(ir),sycav0)
               enddo
            endif
            lrevcav=.FALSE.
         endif

c ---    Obtain sigmas for this step

c ---    First, persist initial values if upwind of starting point
         if(lwak .AND. (xi.GE.x)) then
            asigz(n)=asigz(n-1)
            asigy(n)=asigy(n-1)
            dsz(n)=dsz(n-1)
c ---       Set index for skipping entry when filling wake arrays
            nws=n
         endif
         if(lcav .AND. (xbc.GE.x)) then
            csigz(n)=szcav0
            csigy(n)=sycav0
c ---       Set index for skipping entry when filling cav arrays
            ncs=n
         endif

c ---    Now test again and apply full treatment when needed
         if(xold.GT.xamx) then
c ---       Ambient growth region in wake: use virtuals
            if(lwak .AND. (xi.LT.x)) then
               call SIGZ(dist(n)+xzvwak,asigz(n))
               call SIGY(dist(n)+xyvwak,asigy(n))
               dsz(n)=(asigz(n)-asigz(n-1))*dxi
            endif
c ---       Cavity source ---
            if(lcav .AND. (xbc.LT.x)) then
               call SIGZ(dist(n)+xzvcav,csigz(n))
               call SIGY(dist(n)+xyvcav,csigy(n))
            endif
         else
            if(x.LT.xamn) then
c ---          Wake growth for both sigz and sigy
c ---          Set x at mid-point of step
               xmid=half*(x+xold)
c ---          Compute turbulence intensities at midpoint
               call WAKE_TURB(istab,lrurl,xmid,xLb,Rb,wakiz,wakiy)
               if(lwak .AND. (xi.LE.x)) then
c ---             Compute sigmas in wake
                  call WAKE_SIG(x,xd,xold,wakiz,wakiy,asigz(n-1),
     &                          asigy(n-1),Hb,Wb,Rb,zk,yk,
     &                          asigz(n),asigy(n),dsz(n))
               endif
c ---          Cavity source ---
               if(lcav .AND. (xbc.LE.x)) then
                  call WAKE_SIG(x,xdc,xold,wakiz,wakiy,csigz(n-1),
     &                          csigy(n-1),Hb,Wb,Rb,zkc,ykc,
     &                          csigz(n),csigy(n),dzrate)
               endif
            else
c ---          At least one of the sigmas reaches ambient growth in wake
c ---          Process SIGMA-Z
               if(xold.GE.xaz) then
c ---             Ambient growth region in wake: use virtual x
                  if(lwak .AND. (xi.LE.x)) then
                     call SIGZ(dist(n)+xzvwak,asigz(n))
                     dsz(n)=(asigz(n)-asigz(n-1))*dxi
                  endif
c ---             Cavity source ---
                  if(lcav .AND. (xbc.LE.x)) then
                     call SIGZ(dist(n)+xzvcav,csigz(n))
                  endif
               elseif(x.GE.xaz) then
c ---             Transition from wake to ambient
                  xnew=xaz
                  xmid=half*(xnew+xold)
c ---             Compute turbulence intensities at midpoint
                  call WAKE_TURB(istab,lrurl,xmid,xLb,Rb,wakiz,wakiy)
                  if(lwak .AND. (xi.LE.xnew)) then
c ---                Compute sigma at xaz
                     call WAKE_SIG(xnew,xd,xold,wakiz,wakiy,asigz(n-1),
     &                             asigy(n-1),Hb,Wb,Rb,zk,ykdum,
     &                             sigzxa,sydum,dzrate)
c ---                Compute corresponding virtual distance
                     xasrc=xaz+xbadj
                     call XVZ(sigzxa,xasrc,xzvwak)
                     xzvwak=xzvwak-xasrc
c ---                Compute sigma at x
                     call SIGZ(dist(n)+xzvwak,asigz(n))
                     dsz(n)=(asigz(n)-asigz(n-1))*dxi
                  endif
c ---             Cavity source ---
                  if(lcav .AND. (xbc.LE.xnew)) then
                     call WAKE_SIG(xnew,xdc,xold,wakiz,wakiy,csigz(n-1),
     &                             csigy(n-1),Hb,Wb,Rb,zkc,ykdum,
     &                             sigzxa,sydum,dzrate)
                     call XVZ(sigzxa,xasrc,xzvcav)
                     xzvcav=xzvcav-xasrc
                     call SIGZ(dist(n)+xzvcav,csigz(n))
                  endif
               else
c ---             Wake growth for sigz
c ---             Set x at mid-point of step
                  xmid=half*(x+xold)
c ---             Compute turbulence intensities at midpoint
                  call WAKE_TURB(istab,lrurl,xmid,xLb,Rb,wakiz,wakiy)
                  if(lwak .AND. (xi.LE.x)) then
c ---                Compute sigmaz
                     call WAKE_SIG(x,xd,xold,wakiz,wakiy,asigz(n-1),
     &                             asigy(n-1),Hb,Wb,Rb,zk,ykdum,
     &                             asigz(n),sydum,dsz(n))
                  endif
c ---             Cavity source ---
                  if(lcav .AND. (xbc.LE.x)) then
                     call WAKE_SIG(x,xdc,xold,wakiz,wakiy,csigz(n-1),
     &                             csigy(n-1),Hb,Wb,Rb,zkc,ykdum,
     &                             csigz(n),sydum,dzrate)
                  endif
               endif
c ---          Process SIGMA-Y
               if(xold.GE.xay) then
c ---             Ambient growth region in wake: use virtual x
                  if(lwak .AND. (xi.LE.x)) then
                     call SIGY(dist(n)+xyvwak,asigy(n))
                  endif
c ---             Cavity source ---
                  if(lcav .AND. (xbc.LE.x)) then
                     call SIGY(dist(n)+xyvcav,csigy(n))
                  endif
               elseif(x.GE.xay) then
c ---             Transition from wake to ambient
                  xnew=xay
                  xmid=half*(xnew+xold)
c ---             Compute turbulence intensities at midpoint
                  call WAKE_TURB(istab,lrurl,xmid,xLb,Rb,wakiz,wakiy)
                  if(lwak .AND. (xi.LE.xnew)) then
c ---                Compute sigma at xay
                     call WAKE_SIG(xnew,xd,xold,turbz,turby,asigz(n-1),
     &                             asigy(n-1),Hb,Wb,Rb,zkdum,yk,
     &                             szdum,sigyxa,dzrate)
c ---                Compute corresponding virtual distance
                     xasrc=xay+xbadj
                     call XVY(sigyxa,xyvwak)
                     xyvwak=xyvwak-xasrc
c ---                Compute sigma at x
                     call SIGY(dist(n)+xyvwak,asigy(n))
                  endif
c ---             Cavity source ---
                  if(lcav .AND. (xbc.LE.xnew)) then
                     call WAKE_SIG(xnew,xdc,xold,wakiz,wakiy,csigz(n-1),
     &                             csigy(n-1),Hb,Wb,Rb,zkdum,ykc,
     &                             szdum,sigyxa,dzrate)
                     call XVY(sigyxa,xyvcav)
                     xyvcav=xyvcav-xasrc
                     call SIGY(dist(n)+xyvcav,csigy(n))
                  endif
               else
c ---             Wake growth for sigy
c ---             Set x at mid-point of step
                  xmid=half*(x+xold)
c ---             Compute turbulence intensities at midpoint
                  call WAKE_TURB(istab,lrurl,xmid,xLb,Rb,wakiz,wakiy)
                  if(lwak .AND. (xi.LE.x)) then
c ---                Compute sigmay
                     call WAKE_SIG(x,xd,xold,wakiz,wakiy,asigz(n-1),
     &                             asigy(n-1),Hb,Wb,Rb,zkdum,yk,
     &                             szdum,asigy(n),dzrate)
                  endif
c ---             Cavity source
                  if(lcav .AND. (xbc.LE.x)) then
                     call WAKE_SIG(x,xdc,xold,wakiz,wakiy,csigz(n-1),
     &                             csigy(n-1),Hb,Wb,Rb,zkdum,ykc,
     &                             szdum,csigy(n),dzrate)
                  endif
               endif
            endif
         endif

c --- Next distance
      enddo

c --- Construct arrays for /WAKEDAT/
c ----------------------------------

      if(lwak) then
c ---    WAK arrays:
         npw=np-nws

c ---    Place initial values into first element
         xwak(1)=xi+xbadj
         szwak(1)=szi
         sywak(1)=syi
         drwak(1)=zero
         if(npw.GE.mxntr) then
c ---       Sample a subset of the npw points
            nwak=mxntr
            xwak(nwak)=dist(np)
            szwak(nwak)=asigz(np)
            sywak(nwak)=asigy(np)
            drwak(nwak)=rtpiby2*dsz(np)
            if(npw.LE.2*mxntr) then
c ---          Fill elements with nearest values
               deln=FLOAT(npw)/FLOAT(nwak)
               do in=2,nwak-1
                  jn=in*deln+nws
                  xwak(in)=dist(jn)
                  szwak(in)=asigz(jn)
                  sywak(in)=asigy(jn)
                  drwak(in)=rtpiby2*dsz(jn)
               enddo
            else
c ---          Use sliding step-size to sample nearfield more frequently
               deln=2.*FLOAT(npw-mxntr)/FLOAT(mxntr*(mxntr-1))
               rn=one
               do in=2,nwak-1
                  rn=rn+one+(in-1)*deln
                  jn=rn+nws
                  xwak(in)=dist(jn)
                  szwak(in)=asigz(jn)
                  sywak(in)=asigy(jn)
                  drwak(in)=rtpiby2*dsz(jn)
               enddo
            endif
         else
c ---       Fill only those elements used
            nwak=npw
            do in=2,npw
               inp=in+nws
               xwak(in)=dist(inp)
               szwak(in)=asigz(inp)
               sywak(in)=asigy(inp)
               drwak(in)=rtpiby2*dsz(inp)
            enddo
         endif
      endif

      if(lcav) then
c ---    CAV arrays:
         npc=np-ncs

c ---    Place initial values into first element
         xcav(1)=xbc+xbadj
         szcav(1)=szcav0
         sycav(1)=sycav0
         if(npc.GE.mxntr) then
c ---       Sample a subset of the npc points
            ncav=mxntr
            xcav(ncav)=dist(np)
            szcav(ncav)=csigz(np)
            sycav(ncav)=csigy(np)
            if(npc.LE.2*mxntr) then
c ---          Fill elements with nearest values
               deln=FLOAT(npc)/FLOAT(ncav)
               do in=2,ncav-1
                  jn=in*deln+ncs
                  xcav(in)=dist(jn)
                  szcav(in)=csigz(jn)
                  sycav(in)=csigy(jn)
               enddo
            else
c ---          Use sliding step-size to sample nearfield more frequently
               deln=2.*FLOAT(npc-mxntr)/FLOAT(mxntr*(mxntr-1))
               rn=one
               do in=2,ncav-1
                  rn=rn+one+(in-1)*deln
                  jn=rn+ncs
                  xcav(in)=dist(jn)
                  szcav(in)=csigz(jn)
                  sycav(in)=csigy(jn)
               enddo
            endif
         else
c ---       Fill only those elements used
            ncav=npc
            do in=2,npc
               inp=in+ncs
               xcav(in)=dist(inp)
               szcav(in)=csigz(inp)
               sycav(in)=csigy(inp)
            enddo
         endif
      endif

      if(ldbhr) then

         write(io6,*)
         write(io6,*)'----- WAKE_DFSN:        NWAK = ',nwak
         write(io6,*)'Z-dispersion reaches ambient at: ',(xaz+xbadj)
         write(io6,*)'Y-dispersion reaches ambient at: ',(xay+xbadj)
         write(io6,*)'z,y virtual dist (m) - WAKE  = ',xzvwak,xyvwak
         write(io6,*)'z,y virtual dist (m) - CAV   = ',xzvcav,xyvcav
         write(io6,*)'xadj, yadj, xi        (m)    = ',xbadj,ybadj,xi
         write(io6,*)'xbc,distc,xdc         (m)    = ',xbc,distc,xdc
         write(io6,*)'lwak, nws, npw               = ',lwak,nws,npw
         write(io6,*)'lcav, ncs, npc               = ',lcav,ncs,npc
         write(io6,*)
c
c ---    Write the arrays passed back to the calling routine
         write(io6,28)
28       format(/4x,'I',9x,'XWAK',6x,'SZWAK',6x,'SYWAK',6x,'DRWAK',/)
         do i=1,nwak
            write(io6,32)i,xwak(i),szwak(i),sywak(i),drwak(i)
32          format(i5,3x,4(f10.4,1x))
         enddo
         write(io6,*)

         write(io6,29)
29       format(/4x,'I',9x,'XCAV',6x,'SZCAV',6x,'SYCAV',/)
         do i=1,ncav
            write(io6,33)i,xcav(i),szcav(i),sycav(i)
33          format(i5,3x,3(f10.4,1x))
         enddo
         write(io6,*)
      endif

      return
      end

c-----------------------------------------------------------------------
      subroutine wake_turb(kst,lrurl,x,L,R,tiz,tiy)
c-----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812             WAKE_TURB
c                L. Schulman, D. Strimaitis,   EARTH TECH
c                Prepared for EPRI under contract WO3527-01
c
c --- PURPOSE: Calculates turbulence intensity as a function of
c              location within the wake from modified Weil (1996)
c              analytical expressions
c
c --- INPUTS:
c              kst - integer     - PG stability class (1-6)
c            lrurl - logical     - Rural flag (T=Rural, F=Urban)
c                x - real        - distance (m) from upwind bldg wall
c                L - real        - dist (m) of downwind bldg wall from
c                                  upwind bldg wall
c                R - real        - wake scaling length (m)
c
c     Common block /DFSN/ variables:
c           wiz0,wiy0,wfz,wfy,
c           dua_ua,xdecay,xdecayi,
c           rurliz,rurliy,urbniz,urbniy
c
c --- OUTPUT:
c
c              tiz - real        - turbulence intensity sigw/u
c              tiy - real        - turbulence intensity sigv/u
c
c --- WAKE_TURB called by:  WAKE_DFSN
c --- WAKE_TURB calls    :  none
c----------------------------------------------------------------------
c
      include 'dfsn.pri'

      real L
      logical lrurl

c --- Misc. constants
      data one/1.0/, zero/0.0/

c --- Identify ambient turbulence intensity
      if(lrurl) then
         ambiz=rurliz(kst)
         ambiy=rurliy(kst)
      else
         ambiz=urbniz(kst)
         ambiy=urbniy(kst)
      endif

c --- Compute asymptotic turbulence intensity in far wake
      fariz=AMIN1(wiz0,ambiz)
      fariy=AMIN1(wiy0,ambiy)

c --- Compute turbulence intensity at position downwind of bldg
      xmL=AMAX1(zero,x-L)
      xfac=one/(((xmL+R)/R)**xdecay-dua_ua)
      tiz=fariz*(one+((wfz*wiz0/fariz-one)+dua_ua)*xfac)
      tiy=fariy*(one+((wfy*wiy0/fariy-one)+dua_ua)*xfac)

c --- Interpolate turbulence intensity if over roof of bldg
      if(x.LT.L) then
         xfrac=x/L
         tiz=ambiz+xfrac*(tiz-ambiz)
         tiy=ambiy+xfrac*(tiy-ambiy)
      endif

      return
      end

c-----------------------------------------------------------------------
      subroutine wake_u(ldb,x,y,z,ubyua,dufac)
c-----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  990726 (99207)           WAKE_U
c                D. Strimaitis,   EARTH TECH
c                Prepared for EPRI under contract WO3527-01
c
c --- PURPOSE: Calculates speed ratio u(wake)/u(ambient) as a function
c              of location within the wake
c
c              Modified by B. de Foy, 26th July 1999,
c              To set fmin as a minimum value for ubyua
c
c --- INPUTS:
c              ldb - logical     - flag for debug output
c                x - real        - downwind distance (m) from upwind
c                                  bldg wall
c                y - real        - crosswind distance (m) from center of
c                                  upwind bldg wall
c                z - real        - height (m) above ground
c
c     Common block /PARAMS/ variables:
c           MXNTR, MXNW
c     Common block /WAKEDAT/ variables:
c           Hb, Wb, xLb, Rb, xLR
c     Common block /DFSN/ variables:
c           dua_ua,xdecay,xdecayi
c
c --- OUTPUT:
c
c            ubyua - real        - U(x,z)/Ua speed in wake scaled by
c                                  ambient speed
c            dufac - real        - Gradient in speed factor above
c                                  Zcav
c
c --- WAKE_U called by:  NUMRISE, WAKE_DBG
c --- WAKE_U calls    :  CAVITY_HT, WAKE_DIM
c----------------------------------------------------------------------
c
      include 'params.pri'
      include 'dfsn.pri'
      include 'wakedat.pri'

      logical ldb

c --- Misc. constants
      data two/2.0/, one/1.0/, zero/0.0/
      data fmin/0.01/

c --- Compute cavity height above ground, and width
      call CAVITY_HT(Hb,Wb,xLb,Rb,xLC,xLR,HR,x,zcav,ycav)

c --- Compute far wake height above ground, and width
      call WAKE_DIM(x,Hb,Wb,Rb,hwake,wwake)

c --- Return "null" values if point is outside wake
      yabs=ABS(y)
      ubyua=one
      dufac=zero
      if(z.GE.hwake .OR. yabs.GE.wwake) return

c --- Adjust "base" speed deficit dua_ua if lateral position is
c --- beyond bldg width projection, but within the wake
      ymin=AMAX1(0.5*Wb,wwake-Rb/3.)
      du_ua=dua_ua
      ydiff=wwake-ymin
      if(yabs.GT.ymin .AND. ydiff.GT.zero) then
         du_ua=dua_ua*(one-(yabs-ymin)/ydiff)
      endif

c --- Scale speed deficit (Ua-U)/Ua =  du_ua in wake for
c --- position x downwind of bldg face
      xmL=AMAX1(zero,x-xLb)
      du_ua=du_ua*((xmL+Rb)/Rb)**(-xdecay)
c --- Interpolate factor if over roof of bldg (linear)
      if(x.LT.xLb) then
         xfrac=x/xLb
         du_ua=xfrac*du_ua
      endif

c --- Compute speed factor Ucav/Ua at top of cavity
c --- Assume that speed is constant below ZCAV, and increases linearly
c --- with height to ambient speed at HWAKE
      ucbyua=AMAX1(zero,(one-two*hwake*du_ua/(hwake+zcav)))

c --- Compute gradient in speed factor (zero below Zcav)
      dufac=zero
      if(z.GT.zcav) then
         dufac=(one-ucbyua)/(hwake-zcav)
      endif

c --- Compute speed factor U/Ua at height z
      zz=AMIN1(z,hwake)
c --- Ensure fmin as lower bound for ubyua
      ubyua=AMAX1(fmin,(ucbyua+dufac*(zz-zcav)))

      if(ldb) then
        write(io6,*)'WAKE_U         '
        write(io6,*)'       x,y,z = ',x,y,z
        write(io6,*)'hwake, zcav  = ',hwake, zcav
        write(io6,*)'wwake, ymin  = ',wwake, ymin
        write(io6,*)'du_ua, ucbyua= ',du_ua, ucbyua
        write(io6,*)'ubyua, dufac = ',ubyua,dufac
        write(io6,*)
      endif

      return
      end

c-----------------------------------------------------------------------
      subroutine wake_xa(kst,lrurl,L,R,xaz,xay)
c-----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  980310               WAKE_XA
c                D. Strimaitis,   EARTH TECH
c                Prepared for EPRI under contract WO3527-01
c
c --- PURPOSE: Calculates the distance from the upwind face of the
c              building to the point at which the turbulence intensity
c              in the wake approaches that in the ambient flow.
c
c              Final distances are limited to "xbyrmax" scale-lengths
c              (R) set in prime1, measured from downwind bldg face
c
c --- INPUTS:
c              kst - integer     - PG stability class (1-6)
c            lrurl - logical     - Rural flag (T=Rural, F=Urban)
c                L - real        - dist (m) of downwind bldg wall from
c                                  upwind bldg wall
c                R - real        - wake scaling length (m)
c
c     Common block /DFSN/ variables:
c           afac,xbyrmax,wiz0,wiy0,wfz,wfy,
c           dua_ua,xdecay,xdecayi,
c           rurliz,rurliy,urbniz,urbniy
c
c --- OUTPUT:
c
c              xaz - real        - distance (m) from upwind bldg wall
c                                  at which wake turbulence Iz = ambient
c              xay - real        - distance (m) from upwind bldg wall
c                                  at which wake turbulence Iy = ambient
c
c --- WAKE_XA called by:  WAKE_DFSN
c --- WAKE_XA calls    :  none
c----------------------------------------------------------------------
c
      include 'dfsn.pri'
      real L
      logical lrurl

c --- Misc. constants
      data one/1.0/

c --- Select ambient turbulence intensity
      if(lrurl) then
         ambiz=rurliz(kst)
         ambiy=rurliy(kst)
      else
         ambiz=urbniz(kst)
         ambiy=urbniy(kst)
      endif

c --- Compute asymptotic turbulence intensity in far wake
      fariz=AMIN1(wiz0,ambiz)
      fariy=AMIN1(wiy0,ambiy)

c --- Define the turbulence intensity at the transition point
      farizt=AMAX1(ambiz,afac*fariz)
      fariyt=AMAX1(ambiy,afac*fariy)

c --- Compute leading term
      x0byr=L/R-one

c --- Compute scaled distance at which Iz equals transition Iz
      xaz=x0byr+(dua_ua+(wfz*wiz0-fariz*(one-dua_ua))/
     &       (farizt-fariz))**xdecayi

c --- Compute distance at which Iy equals transition Iy
      xay=x0byr+(dua_ua+(wfy*wiy0-fariy*(one-dua_ua))/
     &       (fariyt-fariy))**xdecayi

c --- Cap distances
      xbyr=L/R+xbyrmax
      xaz=R*AMIN1(xbyr,xaz)
      xay=R*AMIN1(xbyr,xay)

      return
      end

c-----------------------------------------------------------------------
      subroutine wake_dim(x,H,W,R,hwake,wwake)
c-----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812              WAKE_DIM
c                D. Strimaitis,   EARTH TECH
c                Prepared for EPRI under contract WO3527-01
c
c --- PURPOSE: Calculates the vertical height and lateral half-width
c              of a building wake at a distance x from the upwind
c              face of the bldg
c
c --- INPUTS:
c                x - real        - dist (m) from upwind bldg face
c                H - real        - building height (m)
c                W - real        - building width (m)
c                R - real        - wake scaling length (m)
c
c --- OUTPUT:
c
c            hwake - real        - wake height (m)
c            wwake - real        - wake half-width (m)
c
c --- WAKE_DIM called by:  POSITION, WAKE_SIG
c --- WAKE_DIM calls    :  none
c----------------------------------------------------------------------
c --- Wake height from combination of Wilson (1979) and Weil (1996)
c --- limits for uniform approach wind

c --- Set const. used in wake height formula
      data cwkht/1.2/

c --- Misc. constants
      data half/0.5/, third/0.3333333/, zero/0.0/

c --- Scale distance by R
      xbyr=x/R
      xbyr3rd=xbyr**third

c --- Compute match to bdlg height at x=0
      xpbyr=-(H/(cwkht*R))**3
      dxbyr=xbyr-xpbyr

c --- Wake height
      hwake=zero
      if(xbyr.GT.zero) hwake =cwkht*R*dxbyr**third

c --- Wake half-width from empirical fit to Snyder wind tunnel data
      wwake=zero
      if(xbyr.GT.zero) wwake=half*W+(third*R)*xbyR3rd

      return
      end

c-----------------------------------------------------------------------
      subroutine wake_sig(x,xd,xold,turbz,turby,szold,syold,
     &                    H,W,R,zk,yk,sz,sy,dsz)
c-----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812              WAKE_SIG
c                D. Strimaitis,   EARTH TECH
c                Prepared for EPRI under contract WO3527-01
c
c --- PURPOSE: Calculates sigmas and d(sigma)/dx within the wake
c              at a distance x from the upwind face of the bldg,
c              prior to the ambient growth regime, for a "small"
c              distance increment
c
c --- INPUTS:
c                x - real        - dist (m) from upwind bldg face
c               xd - real        - dist (m) at which PDF growth ends
c             xold - real        - starting x of this step (m)
c     turbz, turby - real        - current turbulence intensities
c     szold, syold - real        - sigmas (m) at start of step
c   htwake, hwwake - real        - height and half-width (m) of wake
c           zk, yk - real        - matching constants for PDF transition
c
c --- OUTPUT:
c
c           zk, yk - real        - matching constants for PDF transition
c           sz, sy - real        - sigmas (m) at end of step
c              dsz - real        - d(sigmaz)/dx over step
c
c --- WAKE_SIG called by:  WAKE_DFSN
c --- WAKE_SIG calls    :  WAKE_DIM
c----------------------------------------------------------------------
c --- Wake height from combination of Wilson (1979) and Weil (1996)
c --- limits for uniform approach wind

c --- Misc. constants
      data two/2.0/

c --- Get wake dimensions
      call WAKE_DIM(x,H,W,R,htwake,hwwake)

c --- Use full width of the wake to scale lateral diffusivity
      fwwake=two*hwwake

      delx=x-xold
      xstepi=1./delx
      if(x.LT.xd) then
c ---    Pure PDF Form
         dsz=turbz
         sz=szold + delx*turbz
         sy=syold + delx*turby
      elseif(xold.GT.xd) then
c ---    Pure Wake Diffusivity Form
         dsz2=zk*turbz*htwake
         dsy2=yk*turby*fwwake
         sz=SQRT(szold**2+delx*dsz2)
         sy=SQRT(syold**2+delx*dsy2)
         dsz=(sz-szold)*xstepi
      else
c ---    Transition from PDF to Diffusivity Form
c ---    To end of PDF:
         delx=xd-xold
         sigzd=szold + delx*turbz
         sigyd=syold + delx*turby
         zk=two*sigzd/htwake
         yk=two*sigyd/fwwake
c ---    Beyond end of PDF:
         delx=x-xd
         dsz2=zk*turbz*htwake
         dsy2=yk*turby*fwwake
         sz=SQRT(sigzd**2+delx*dsz2)
         sy=SQRT(sigyd**2+delx*dsy2)
         dsz=(sz-szold)*xstepi
      endif

      return
      end
c-----------------------------------------------------------------------
      subroutine wake_dbg(io,ntr,xtr,ytr,ztr,rtr,nobid,hstack)
c-----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812              WAKE_DBG
c                D. Strimaitis,   EARTH TECH
c                Prepared for EPRI under contract WO3527-01
c
c --- PURPOSE: Reports salient features of PRIME results to
c              file for DEBUG purposes
c
c --- INPUTS:
c               io - integer     - unit for output file
c         XTR(ntr) - real        - Downwind distance (m)
c         YTR(ntr) - real        - Crosswind distance (m)
c         ZTR(ntr) - real        - Plume centerline height (m)
c         RTR(ntr) - real        - Plume radius (m)
c            NOBID - logical     - flag for BID
c           HSTACK - real        - height (m) of release
c
c     Common block /PARAMS/ variables:
c           MXNTR, MXNW
c     Common block /WAKEDAT/ variables:
c           XBADJ, Hb, Wb, xLb, Rb, xLR, xLC, HR,
c           XCAV, SZCAV, SYCAV
c
c --- OUTPUT: (written to file)
c
c          DBXB - real    - Distance (m) from upwind bldg face
c           DBX - real    - Distance (m) from source along wind
c           DBZ - real    - Plume centerline height above ground (m)
c          DBHC - real    - Cavity height above ground (m)
c          DBHW - real    - Wake height above ground (m)
c          DBSZ - real    - Sigma-z (m)
c          DBSY - real    - Sigma-y (m)
c          DBUW - real    - Wind speed factor at DBZ  (u/Ua)
c         DBRSZ - real    - Sigma-y (m) inferred from plume radius
c       IPOSITN - integer - 1: in bldg
c                           2: in cavity
c                           3: in far wake
c                           4: outside bldg influence
c         DBSZC - real    - Sigma-z (m) for cavity source
c         DBSYC - real    - Sigma-y (m) for cavity source
c
c --- WAKE_DBG called by:  PHEFF
c --- WAKE_DBG calls    :  WAKE_XSIG, WAKE_DIM, CAVITY_HT,
c                          POSITION, WAKE_U
c----------------------------------------------------------------------
c
      include 'params.pri'
      include 'wakedat.pri'

      REAL XTR(ntr),YTR(ntr),ZTR(ntr),RTR(ntr)
      logical nobid,ldb

      ldb=.FALSE.

      data rt2bypi/0.797885/

c --- Write section header to file
      write(io,*)
      write(io,*)'------------------------------------------------'
      write(io,*)'PRIME Module Results for Current Source and Hour'
      write(io,*)'          (all lengths in meters)'
      write(io,*)'------------------------------------------------'
      write(io,*)
      write(io,100)
      write(io,*)

c --- Report start of cavity as first point if it lies upwind of source
      if(xcav(1).LT.0.0) then
c ---    Set plume coordinates
         dbx=xcav(1)
         dby=ytr(1)
         dbz=0.0

c ---    Set initial values
         dbsz=0.0
         dbsy=0.0
         dbhw=0.0
         dbhc=0.0
         dbrsz=0.0
         dbuw=1.0
         ipositn=4

c ---    Compute related data
         rise=0.0
         xb=dbx-xbadj
         yb=dby-ybadj
         zb=dbz
         dbxb=xb

c ---    Set sigmas
         dbsz=0.0
         dbsy=0.0
         dbszc=szcav(1)
         dbsyc=sycav(1)

c ---    Set dr/dx of plume radius within wake region
         dbdrdx=0.0

         if(xb.GE.0.0) then
c ---       Set wake dimension along center plane from bldg
            call WAKE_DIM(xb,Hb,Wb,Rb,dbhw,dbww)

c ---       Set cavity dimension along centerplane from bldg
            call CAVITY_HT(Hb,Wb,xLb,Rb,xLC,xLR,HR,xb,dbhc,dbwc)

c ---       Set speed factor
            call POSITION(xb,yb,zb,ipositn)
            dbuw=1.0
            if(ipositn.LT.4) call WAKE_U(ldb,xb,yb,zb,dbuw,dbduw)
         endif

c ---    Report values
         write(io,101) dbxb,dbx,dbz,dbhw,dbhc,dbsz,dbsy,dbuw,dbduw,
     &                 dbrsz,dbdrdx,ipositn,dbszc,dbsyc
      endif

c --- Process point of release
c --- Set plume coordinates
      dbx=0.0
      dby=ytr(1)
      dbz=hstack

c --- Set initial values
      dbsz=0.0
      dbsy=0.0
      dbhw=0.0
      dbhc=0.0
      dbrsz=0.0
      dbuw=1.0
      ipositn=4

c --- Compute related data
      rise=dbz-hstack
      xb=dbx-xbadj
      yb=dby-ybadj
      zb=dbz
      dbxb=xb

c --- Set sigmas just downwind of source
      xzero=0.001
      call WAKE_XSIG(xzero,rise,nobid,dbsz,dbsy,dbszc,dbsyc)

c --- Set dr/dx of plume radius within wake region
      call WAKE_DRDX(dbx,dbdrdx)

      if(xb.GE.0.0) then
c ---    Set wake dimension along center plane from bldg
         call WAKE_DIM(xb,Hb,Wb,Rb,dbhw,dbww)

c ---    Set cavity dimension along centerplane from bldg
         call CAVITY_HT(Hb,Wb,xLb,Rb,xLC,xLR,HR,xb,dbhc,dbwc)

c ---    Set speed factor
         call POSITION(xb,yb,zb,ipositn)
         dbuw=1.0
         if(ipositn.LT.4) call WAKE_U(ldb,xb,yb,zb,dbuw,dbduw)
      endif

c --- Report values
      write(io,101) dbxb,dbx,dbz,dbhw,dbhc,dbsz,dbsy,dbuw,dbduw,dbrsz,
     &              dbdrdx,ipositn,dbszc,dbsyc

c --- Now loop over entries in plume rise array
      do it=1,ntr

c ---    Set plume coordinates
         dbx=xtr(it)
         dby=ytr(it)
         dbz=ztr(it)
         dbrsz=rtr(it)*rt2bypi

c ---    Set initial values
         dbhw=0.0
         dbhc=0.0
         dbuw=1.0
         ipositn=4

c ---    Compute related data
         rise=dbz-hstack
         xb=dbx-xbadj
         yb=dby-ybadj
         zb=dbz
         dbxb=xb

c ---    Set sigmas
         call WAKE_XSIG(dbx,rise,nobid,dbsz,dbsy,dbszc,dbsyc)

c ---    Set dr/dx of plume radius within wake region
         call WAKE_DRDX(dbx,dbdrdx)

         if(xb.GE.0.0) then
c ---       Set wake dimension along center plane from bldg
            call WAKE_DIM(xb,Hb,Wb,Rb,dbhw,dbww)

c ---       Set cavity dimension along centerplane from bldg
            call CAVITY_HT(Hb,Wb,xLb,Rb,xLC,xLR,HR,xb,dbhc,dbwc)

c ---       Set speed factor
            call POSITION(xb,yb,zb,ipositn)
            dbuw=1.0
            if(ipositn.LT.4) call WAKE_U(ldb,xb,yb,zb,dbuw,dbduw)
         endif

c ---    Report values
         write(io,101) dbxb,dbx,dbz,dbhw,dbhc,dbsz,dbsy,dbuw,dbduw,
     &                 dbrsz,dbdrdx,ipositn,dbszc,dbsyc

      enddo
      write(io,*)

100   format('     XB      X      Z   Hwake   Hcav    Sz     S',
     &       'y   Ufac  dUfac  R->Sz   dRdx  Pos  Szcav  Sycav')
101   format(1x,7f7.1,2f7.3,f7.1,f7.3,i4,2f7.1)

      return
      end
c-----------------------------------------------------------------------
      subroutine wake_cav0(sycapt,szcav0,sycav0)
c-----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812              WAKE_CAV0
c                D. Strimaitis, L. Schulman,   EARTH TECH
c                Prepared for EPRI under contract WO3527-01
c
c --- PURPOSE:  Compute the sigmas for a source placed on the floor
c               of the cavity, which produce the target cavity
c               concentration
c
c --- INPUTS:
c
c        SYCAPT - real    - Sigma-y (m) of plume at point where
c                           mass is captured in cavity
c
c     Common block /WAKEDAT/ variables:
c           Hb, Wb, xLR, xLC, HR, Ub, Urh
c
c --- OUTPUT:
c
c        SZCAV0 - real    - Initial sigma-z (m) of cavity source
c        SYCAV0 - real    - Initial sigma-y (m) of cavity source
c
c                 Note    - These sigmas reproduce the cavity
c                           concentration when the used in:
c                           C = Qc / (pi * us * szcav0 * sycav0)
c                           where us is the wind speed for the primary
c                           source, and Qc is the mass flux captured by
c                           and released from the cavity.
c
c --- WAKE_CAV0 called by:  WAKE_DFSN
c --- WAKE_CAV0 calls    :  none
c----------------------------------------------------------------------
c
      include 'params.pri'
      include 'wakedat.pri'

      data rt2pi/2.5066283/, rt2bypi/.7978846/, third/0.3333333/

c --- Interpret plume sigma-y at cavity entry point as equivalent
c --- top-hat;  limit to full width of bldg
      wcapt=AMIN1(rt2pi*sycapt,Wb)

c --- Set width scale for lateral distribution in cavity
      wscale=AMIN1(Wb,3.*Hb)
      wscale=AMAX1(wscale,third*Hb)
      wscale=AMAX1(wscale,wcapt)

c --- Sigma-y for equivalent top-hat distribution
      sycav0=wscale/rt2pi

c --- Set height of cavity behind the bldg
      if(xLC .LT. xLb) then
c ---    Reattachment
         hcav=Hb
      else
c ---    No Reattachment
         hcav=HR
      endif

c --- Set sigma-z that results in centerline concentration equal to
c --- cavity concentration
c --- Wilson & Britter 1982 approach to cavity concentration
      uratio=Ub/Urh
      szcav0=rt2bypi*uratio*hcav*third

      return
      end
c-----------------------------------------------------------------------
      subroutine cav_src(xr,yr,zr,fqcav0,qc,hc,yrc,zrc,szc,syc,n1,n2)
c-----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812               CAV_SRC
c                D. Strimaitis,   EARTH TECH
c                Prepared for EPRI under contract WO3527-01
c
c --- PURPOSE:  Select plume data for computing concentration at a
c               receptor due to mass contained in / released from cavity
c
c --- INPUTS:
c            XR - real    - Downwind distance (m) from stack to receptor
c            YR - real    - Crosswind distance (m) from stack to receptor
c            ZR - real    - Receptor height (m) above ground
c
c     Common block /WAKEDAT/ variables:
c           Hb, Wb, xLb, Rb, xLC, xLR, HR,
c           XBADJ, YBADJ, SZCAV, SYCAV, FQCAV
c
c --- OUTPUT:
c
c        FQCAV0 - real    - Fraction of plume mass rate captured
c                           and released by cavity
c         QC(3) - real    - Normalized emission rate (q/s) for cavity
c                           sources --- QC(1)+QC(2)=1.0
c         HC(3) - real    - Height (m) of cavity sources
c        YRC(3) - real    - Sigma-z (m) for cavity sources
c        YRC(3) - real    - Crosswind distance (m) from centerline
c                           of cavity sources to receptor
c        ZRC(3) - real    - Receptor height (m) above cavity
c        SZC(3) - real    - Sigma-z (m) for cavity sources
c        SYC(3) - real    - Sigma-y (m) for cavity sources
c         N1,N2 - integer - Index range for active sources
c                           1,1: Primary source ONLY (no cavity source
c                                contributions)
c                           1,2: Primary and "outside" cavity source
c                                contribution
c                           1,3: Primary and both "outside" and "inside"
c                                cavity source contributions
c                           2,2: "outside" cavity source ONLY
c                           2,3: Both "outside" and "inside" cavity
c                                sources
c                           3,3: "inside" cavity source ONLY
c
c ------------------------------------
c     NOTE:  3 sources are considered:
c                           (1)- the primary (point) source
c                           (2)- the cavity source that dominates
c                                "outside" of the cavity
c                           (3)- the cavity source that dominates
c                                "inside" of the cavity
c            For the 2 cavity sources, array data elements are ordered:
c                           (1)- RESERVED for primary source data
c                           (2)- "outside" cavity source
c                           (3)- "inside" cavity source
c
c --- CAV_SRC called by:  PSIMPL(HOST subroutine)
c --- CAV_SRC calls    :  POSITION, CAVITY_HT, WAKE_XSIG
c----------------------------------------------------------------------
c
      include 'params.pri'
      include 'wakedat.pri'

      real qc(3),hc(3),yrc(3),zrc(3),szc(3),syc(3)

      data rt2pi/2.5066283/

c --- Extract cavity sigmas from the first entry in the cavity arrays
      szcav0=szcav(1)
      sycav0=sycav(1)

c --- Pass mass fraction to calling program
      fqcav0=fqcav

c --- Set cavity source heights to zero
      hc(2)=0.0
      hc(3)=0.0

c --- Initialize cavity source mode
c --- (0: none, 1: "outside", 2: "inside", 3: both)
      mode=0

      if(fqcav.LE.0.0) then
c ---    No mass in cavity
         n1=1
         n2=1
         do i=2,3
            qc(i)=0.0
            yrc(i)=yr
            zrc(i)=zr
            szc(i)=szcav0
            syc(i)=sycav0
         enddo
      else
c ---    Find receptor location relative to center of upwind bldg face
         xrb=xr-xbadj
         yrb=yr-ybadj
         zrb=zr
         call POSITION(xrb,yrb,zrb,ipositn)

c ---    Set limits of transition zone at end of cavity
         x115b=xLb+1.15*xLR
         x85b=xLb+0.85*xLR
c ---    Adjust relative contribution of cavity sources near end
c ---    of cavity region
         if(xrb.GE.x115b) then
c ---       Receptor well outside cavity; use only "outside" source
            qc(2)=1.0
            qc(3)=0.0
            mode=1
         elseif(xrb.GT.x85b) then
c ---       Mix relative contribution so that they are equal at
c ---       end of cavity
            qc(2)=(xrb-x85b)/(x115b-x85b)
            qc(3)=1.0-qc(2)
            mode=3
         elseif(xrb.GT.xLb) then
c ---       Receptor well within cavity; use only "inside" source
            qc(2)=0.0
            qc(3)=1.0
            mode=2
         else
c ---       Receptor upwind of trailing edge of projected bldg;
c ---       use "inside" source, but drop mass fraction linearly
c ---       to zero at windward face of projected bldg
            qc(2)=0.0
            qc(3)=AMAX1(0.0,xrb/xLb)
            mode=2
         endif

         if(ipositn.EQ.4) then
c ---       Not within wake, so drop cavity source contribution
            mode=0
            n1=1
            n2=1
            do i=2,3
               qc(i)=0.0
               yrc(i)=yr
               zrc(i)=zr
               szc(i)=szcav0
               syc(i)=sycav0
            enddo
         else
c ---       Set receptor offset from centerline of cavity plume
c ---       Top-hat equivalent width of cavity sigma-y
            wtop=sycav0*rt2pi
c ---       Max distance from bldg center to centerline of cavity plume
            ybmax=0.5*(Wb-wtop)
            if(ybmax.LE.0.0) then
c ---          Plume spread exceeds bldg width so cavity source is
c ---          centered on bldg
               yrc(2)=yrb
            else
c ---          Source location relative to center of bldg
               ysb=-ybadj
               if(ysb.LT.0.0) then
                  yrc(2)=yrb-AMAX1(ysb,-ybmax)
               else
                  yrc(2)=yrb-AMIN1(ysb,ybmax)
               endif
            endif
            yrc(3)=yrc(2)

            if(ipositn.LE.2) then
c ---          Within cavity/bldg, so drop primary source contribution,
c ---          and place receptor on ground
               if(mode.EQ.3) then
                  n1=2
                  n2=3
               elseif(mode.EQ.2) then
                  n1=3
                  n2=3
               elseif(mode.EQ.1) then
                  n1=2
                  n2=2
               endif
               do i=n1,n2
                  zrc(i)=0.0
                  szc(i)=szcav0
                  syc(i)=sycav0
               enddo
               if((mode.EQ.1 .OR. mode.EQ.3) .AND.
     &               xr.GT.0.0) call WAKE_XSIG(xr,0.0,ldb,dumz,dumy,
     &                                         szc(2),syc(2))
            else
c ---          Contributions from primary & possibly both cavity plumes
               n1=1
               n2=3
c ---          Set pole height to height above cavity boundary
               if(xrb.GE.(xLb+xLR)) then
                  zrc(2)=zr
               else
                  call CAVITY_HT(Hb,Wb,xLb,Rb,xLC,xLR,HR,xrb,zcav,wcav)
                  zrc(2)=AMAX1(0.0,zr-zcav)
               endif
               zrc(3)=zrc(2)
               if(mode.EQ.2) then
c ---             No contribution from "outside" cavity source, so swap
c ---             data for "inside" source into "outside" source arrays
c ---             and reset n2=2
                  qc(2)=qc(3)
                  szc(2)=szcav0
                  syc(2)=sycav0
                  n2=2
               elseif(mode.EQ.1) then
c ---             No contribution from "inside" cavity source, so
c ---             reset n2=2
                  call WAKE_XSIG(xr,0.0,ldb,dumz,dumy,szc(2),syc(2))
                  n2=2
               else
c ---             Both cavity sources are used
                  szc(2)=szcav0
                  syc(2)=sycav0
                  szc(3)=szcav0
                  syc(3)=sycav0
                  if(xr.GE.0.0) call WAKE_XSIG(xr,0.0,.TRUE.,dumz,dumy,
     &                                         szc(2),syc(2))
               endif
            endif
         endif
      endif

c --- Final check: receptor upwind of primary source, or all mass in cav
c --- Do not allow n1=1 (primary source contribution)
      if(n1.EQ.1 .AND. (xr.LE.0.0 .OR. fqcav.EQ.1.0)) n1=2

      return
      end
c-----------------------------------------------------------------------
      subroutine wake_fqc(ldb,xbi,xtr,ztr,ntr)
c-----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812              WAKE_FQC
c                D. Strimaitis, L. Schulman,   EARTH TECH
c                Prepared for EPRI under contract WO3527-01
c
c --- PURPOSE: Computes the maximum plume mass captured by cavity.
c ---          Plume centerline enters wake boundary before clearing
c ---          downwind end of cavity, so WAKE_FQC is used to find
c ---          point where mass in cavity is greatest.  Note that
c ---          distances are measured from center of upwind face of bldg
c
c --- INPUTS:
c              ldb - logical     - Debug output switch
c              xbi - real        - Downwind distance (m) from upwind
c                                  face of bldg to point where plume
c                                  centerline enters wake
c         XTR(ntr) - real        - Downwind distance from source (m)
c         ZTR(ntr) - real        - Plume centerline height (m)
c              NTR - integer     - Number of entries in arrays
c
c     Common block /PARAMS/ variables:
c           MXNTR, MXNW
c     Common block /WAKEDAT/ variables:
c           Hb, Wb, xLb, Rb, xLC, xLR, HR, XBADJ, YBADJ
c
c --- OUTPUT:
c
c     Common block /WAKEDAT/ variables:
c           FQCAV
c
c --- WAKE_FQC called by:  NUMRISE
c --- WAKE_FQC calls    :  NUMGRAD, WAKE_XSIG, CAVITY_HT, FRGAUSS
c----------------------------------------------------------------------
c
      include 'params.pri'
      include 'wakedat.pri'

      REAL XTR(ntr),ZTR(ntr)
      logical ldb

      fqcav=0.0

c --- Define range of distances from upwind face of bldg at which to
c --- evaluate plume mass fraction within cavity
      xbstart=AMAX1(xbi,xLb)
      xbend=xLb+xLR
      xrange=0.99*(xbend-xbstart)
      yba=ABS(ybadj)
c --- Distance from source to end of cavity
      xend=xbend+xbadj

c --- Use at least 5 steps, with a maximum length of 10 m
      nstep=MAX(5,(1+xrange/10.))
      xstep=xrange/FLOAT(nstep)

c --- For vertical plane, compute mass fraction below Hb at the
c --- downwind end of the cavity.  This allows the influence of plume
c --- rise to continue lifting plume mass out of the influence of
c --- of the cavity structure for strongly buoyant releases.
c --- Use this value as a cap to fractz.
      call NUMGRAD(xend,xtr,ztr,ntr,zplm)
      call WAKE_XSIG(xend,0.0,.TRUE.,sz,sy,szc,syc)
      call FRGAUSS(zplm,sz,Hb,-Hb,fractz0)

      do is=0,nstep
         xb=xbstart+is*xstep
         x=xb+xbadj
         call NUMGRAD(x,xtr,ztr,ntr,zplm)
         call CAVITY_HT(Hb,Wb,xLb,Rb,xLC,xLR,HR,xb,zcav,ycav)
         call WAKE_XSIG(x,0.0,.TRUE.,sz,sy,szc,syc)
         call FRGAUSS(zplm,sz,zcav,-zcav,fractz)
         call FRGAUSS(yba,sy,ycav,-ycav,fracty)
         fz=AMIN1(fractz,fractz0)
         fract=fz*fracty
         if(fract.GT.fqcav) then
            fqcav=fract
            xmax=x
            fzmax=fz
         endif
      enddo

c --- Additional constraint:  account for fluctuations in cavity
c --- boundary on capturing low-level plumes by imposing a maximum
c --- capture fraction that linearly drops from 1.0 at 85% of the
c --- cavity length, to 0% at the end of the cavity.
      xb85=xLb+0.85*xLR
      if(xbstart.GT.xb85) then
         fq85=AMAX1( 0.0, 1.-(xbstart-xb85)/(xbend-xb85) )
         fqcav=AMIN1(fqcav,fq85)
      endif

      if(ldb) then
         write(io6,*)
         write(io6,*)'WAKE_FQC:'
         write(io6,*)'xbi,xbstart,xbend  = ',xbi, xbstart, xbend
         write(io6,*)'xstep,nstep,fractz0= ',xstep, nstep, fractz0
         write(io6,*)'xb85,xmax          = ',xb85,xmax
         write(io6,*)'fqcav,fzmax        = ',fqcav,fzmax
         write(io6,*)
      endif

      return
      end
c----------------------------------------------------------------------
      subroutine FRGAUSS(hcntr,sigma,h1,h2,fract)
c----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812               FRGAUSS
c                J. Scire, D. Strimaitis,  EARTH TECH
c                Prepared for EPRI under contract WO3527-01
c
c --- PURPOSE:  Compute the fraction of a Gaussian distribution between
c               two limits
c
c --- INPUTS:
c            HCNTR - real    - Center of Gaussian distribution (m)
c            SIGMA - real    - Standard deviation (m) of the
c                              distribution
c           H1, H2 - real    - Limits between which the distribution
c                              is to be integrated
c
c --- OUTPUT:
c            FRACT - real    - Fraction of the Gaussian distribution
c                              between H1 and H2
c
c --- FRGAUSS called by: WAKE_FQC
c --- FRGAUSS calls:     ERFDIF
c----------------------------------------------------------------------
c
      data sqrt2/1.4142136/,small/1.e-5/
c
c --- Prevent numerical problems with very small sigmas
      s=sqrt2*AMAX1(sigma,small)
c
      z1=(h1-hcntr)/s
      z2=(h2-hcntr)/s
c
      fract=0.5*ABS(ERFDIF(z1,z2))
c
      return
      end
c----------------------------------------------------------------------
      FUNCTION ERFDIF(X1,X2)
c----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812                ERFDIF
c
c     Taken from:
c --- CALPUFF    Version: 4.0       Level: 900228                ERFDIF
c                R. Yamartino, SRC
c
c --- PURPOSE:  Computes the difference: erfdif = erf(x1) - erf(x2).
c               Various methods are used to avoid roundoff errors
c               depending on the values of the two arguments.
c
c --- INPUTS:
c
c                X1 - real    - Argument 1 (no units)
c                X2 - real    - Argument 1 (no units)
c
c --- OUTPUTS:
c
c            ERFDIF - real    - erf(x1) - erf(x2)
c
c --- ERFDIF called by:  FRGAUSS
c --- ERFDIF calls:      ERF,ERFC
c----------------------------------------------------------------------
C *** V3.21
c
      ERFDIF=0.0
      IF(X1.EQ.X2) GO TO 40
      IF((X1*X2).LE.0.0) GO TO 50
      XTEST=ABS(X2)
      IF(ABS(X1).LT.XTEST) XTEST=ABS(X1)
c --- Some compilers cannot handle reals .LT. 1.18e-38, so reset cut
c     IF(XTEST.GE.13.306) GO TO 40
      if(xtest .GE. 9.15) GO TO 40
      IF(XTEST.LT.0.47) GO TO 50
C     CAN ONLY REACH HERE WHEN X1 AND X2 HAVE SAME SIGN.
      ISIGN=1
      XX1=X1
      XX2=X2
      IF(X1.GT.0.0) GO TO 30
      ISIGN=-1
      XX1=-XX1
      XX2=-XX2
C  30 ERFDIF=ISIGN*(ERFC(XX2)-ERFC(XX1))
   30 ERFCX1=0.0
      ERFCX2=0.0
c --- Some compilers cannot handle reals .LT. 1.18e-38, so reset cut
c     IF(XX1.LT.13.306) ERFCX1=ERFC(XX1)
c     IF(XX2.LT.13.306) ERFCX2=ERFC(XX2)
      if(xx1 .LT. 9.15) erfcx1=ERFC(xx1)
      if(xx2 .LT. 9.15) erfcx2=ERFC(xx2)
      ERFDIF=ISIGN*(ERFCX2-ERFCX1)
c --- Protect against flakey LAHEY compiler 4/9/89
      if(erfcx2.eq.erfcx1) erfdif=0.0
   40 RETURN
   50 ERFDIF=ERF(X1)-ERF(X2)
      RETURN
      END
c-----------------------------------------------------------------------
      FUNCTION ERF(XX)
c-----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812                   ERF
c
c     Taken from:
c --- CALPUFF    Version: 4.0       Level: 941228                   ERF
c                R. Yamartino, SRC
c
c --- PURPOSE:  Computes the error function, erf(x).
c ---           This is the Quick medium accuracy ERROR FUNCTION from
c ---           NBS 55.  Using an approximation due to Hastings;
c ---           absolute error about 3e-7
c
c
c --- INPUTS:
c
c                XX - real    - Argument  (no units)
c
c --- OUTPUTS:
c
c               ERF - real    - error function of x
c
c --- ERF called by:  ERFDIF
c --- ERF calls:   no routines
c----------------------------------------------------------------------
c
      real x, xx ,t, t16, a(6)
      data a/0.0000430638, 0.0002765672, 0.0001520143,
     *       0.0092705272, 0.0422820123, 0.0705230784/
      data xcut/ 3.919206/
c
      x = abs(xx)
      if(x .gt. xcut) then
         t16 = 0.0
      else
c
         t = ((((((((( a(1)*x + a(2) ) * x ) + a(3) ) * x ) + a(4) ) *
     x                    x ) + a(5) ) * x ) + a(6) ) * x
c
         t = 1.0 / (t + 1.0)
c
         t16 = t * t * t * t
         t16 = t16 * t16 * t16 * t16
      endif
c
      if(xx .gt. 0.0) then
         erf =  1.0 - t16
      else
         erf =  t16 - 1.0
      endif
c
      return
      end
c-----------------------------------------------------------------------
      FUNCTION ERFC(XX)
c-----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812                  ERFC
c
c     Taken from:
c --- CALPUFF    Version: 4.0       Level: 941228                  ERFC
c                R. Yamartino, SRC
c
c --- PURPOSE:  Computes the complementary error function, 1-erf(x).
c ---           This is the Quick medium accuracy COMP. ERROR FUNCTION
c ---           from NBS 55.  Using an approximation due to Hastings;
c ---           absolute error about 3e-7.  Asymptotic expression added
c ---           for large xx to reduce percent error.
c
c
c --- INPUTS:
c
c                XX - real    - Argument  (no units)
c
c --- OUTPUTS:
c
c              ERFC - real    - complementary error function of x
c
c --- ERFC called by:  ERFDIF
c --- ERFC calls:   no routines
c-----------------------------------------------------------------------
c
      real x, xx ,t, t16, a(6)
      data a/0.0000430638, 0.0002765672, 0.0001520143,
     *       0.0092705272, 0.0422820123, 0.0705230784/
      data xcutl/-3.919206/
      data xcuth/13.306   /
      data rtpii/0.5641896/
c
      if(xx .gt. xcuth) then
         erfc = 0.0
c
      elseif(xx .lt. xcutl) then
         erfc = 2.0
c
      elseif(xx .gt. 2.79) then
         x = abs(xx)
         z = 1.0 / x
         erfc = rtpii * z * exp(-x*x) * ( 1.0 - 0.5*z*z*(1.0-1.5*z*z) )
c
      else
         x = abs(xx)
         t = ((((((((( a(1)*x + a(2) ) * x ) + a(3) ) * x ) + a(4) ) *
     x                    x ) + a(5) ) * x ) + a(6) ) * x
c
         t = 1.0 / (t + 1.0)
c
c        erfc = t**16   for x > 0
         t16 = t * t * t * t
         t16 = t16 * t16 * t16 * t16
c
         if(xx .gt. 0.0) then
            erfc =  t16
         else
            erfc =  2.0 - t16
         endif
c
      endif
c
      return
      end
c----------------------------------------------------------------------
      subroutine wake_xsig(x,rise,NOBID,sz,sy,szc,syc)
c----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812             WAKE_XSIG
c                D. Strimaitis,  EARTH TECH
c                Prepared for EPRI under contract WO3527-01
c
c --- PURPOSE:  Extract sigmas in the wake by interpolating among the
c               stored values; compute sigmas outside tabulated range
c               using HOST sigma curves with BID or virtual source
c               adjustments
c
c --- INPUTS:
c                X - real       - Downwind distance (m) from source
c             RISE - real       - Gradual plume rise (m)
c            NOBID - logical    - Directs use of buoyancy enhancement
c
c     Common block /PARAMS/ variables:
c           MXNTR
c     Common block /WAKEDAT/ variables:
c           NWAK, XWAK(mxntr), SZWAK(mxntr), SYWAK(mxntr),
c           XZVWAK, XYVWAK
c           NCAV, XCAV(mxntr), SZCAV(mxntr), SYCAV(mxntr),
c           XZVCAV, XYVCAV
c
c --- OUTPUT:
c               SZ - real       - Sigma-z (m) at downwind distance X
c                                 due to primary source
c               SY - real       - Sigma-y (m) at downwind distance X
c                                 due to primary source
c              SZC - real       - Sigma-z (m) of cavity source at
c                                 downwind distance X from primary source
c              SYC - real       - Sigma-y (m) of cavity source at
c                                 downwind distance X from primary source
c
c --- WAKE_XSIG called by:  PDIS
c --- WAKE_XSIG calls:      SIGZ, SIGY
c----------------------------------------------------------------------
c
      include 'params.pri'
      include 'wakedat.pri'
c
      logical NOBID

c --- Primary source:
c -------------------
      if(x.LE.0.0) then
c ---    Report null values (these should never get used!)
         sz=0.0
         sy=0.0
      elseif(nwak.LE.1) then
c ---    Plume never altered by wake turbulence; use HOST curves
         call SIGZ(x,sz)
         call SIGY(x,sy)
         if(.not.NOBID) then
            bidsq=(rise/3.5)**2
            sz=SQRT(sz**2+bidsq)
            sy=SQRT(sy**2+bidsq)
         endif
      elseif(x.lt.xwak(1)) then
c ---    Point lies upwind of wake region; use HOST curves
         call SIGZ(x,sz)
         call SIGY(x,sy)
         if(.not.NOBID) then
            bidsq=(rise/3.5)**2
            sz=SQRT(sz**2+bidsq)
            sy=SQRT(sy**2+bidsq)
         endif
      elseif(x.gt.xwak(nwak)) then
c ---    Point lies downwind of transition to ambient growth; use
c ---    HOST curves with virtual distance adjustment
         call SIGZ(x+xzvwak,sz)
         call SIGY(x+xyvwak,sy)
      else
c ---    Point lies within range of tabulated values
         nwkm1=nwak-1
         sz=szwak(1)
         sy=sywak(1)
         do i=nwkm1,1,-1
            if(x.ge.xwak(i))then
               ip1=i+1
               fac=(xwak(ip1)-x)/(xwak(ip1)-xwak(i))
               sz=szwak(ip1)-(szwak(ip1)-szwak(i))*fac
               sy=sywak(ip1)-(sywak(ip1)-sywak(i))*fac
               goto 50
            endif
         enddo
      endif

c --- Cavity source:
c -------------------
50    if(ncav.LE.1) then
c ---    No contribution from cavity source (report initial values)
         szc=szcav(1)
         syc=sycav(1)
      elseif(x.lt.xcav(1)) then
c ---    Point lies upwind of cavity region (report initial values)
         szc=szcav(1)
         syc=sycav(1)
      elseif(x.gt.xcav(ncav)) then
c ---    Point lies downwind of transition to ambient growth; use
c ---    HOST curves with virtual distance adjustment
         call SIGZ(x+xzvcav,szc)
         call SIGY(x+xyvcav,syc)
      else
c ---    Point lies within range of tabulated values
         ncvm1=ncav-1
         szc=szcav(1)
         syc=sycav(1)
         do i=ncvm1,1,-1
            if(x.ge.xcav(i))then
               ip1=i+1
               fac=(xcav(ip1)-x)/(xcav(ip1)-xcav(i))
               szc=szcav(ip1)-(szcav(ip1)-szcav(i))*fac
               syc=sycav(ip1)-(sycav(ip1)-sycav(i))*fac
               return
            endif
         enddo
      endif

      return
      end
c-----------------------------------------------------------------------
      subroutine cavity_ht(H,W,L,R,LC,LR,HR,x,zcav,ycav)
c-----------------------------------------------------------------------
c
c --- PRIME      Version:  1.0     Level:  970812             CAVITY_HT
c                L. Schulman, EARTH TECH
c                Prepared for EPRI under contract WO3527-01
c
c --- PURPOSE:  Calculates height of cavity envelope as function of x
c
c --- INPUTS:
c                H - real              - Building height above ground
c                W - real              - Projected building width
c                L - real              - Along-wind building length
c                                        building face
c                R - real              - Scale length from H and W
c               LC - real              - Length of roof cavity
c               LR - real              - Length of downwind cavity from
c                                         lee face
c               HR - real              - Maximum cavity height above
c                                         ground
c                x - real              - downwind distances
c
c --- OUTPUT:
c
c          zcav    - real              - cavity height as function of x
c
c          ycav    - real              - cavity half-width as f(x)

c --- CAVITY_HT called by:  PRIME
c --- CAVITY_HT calls:      none
c----------------------------------------------------------------------
c
c
c
      real H,W,L,R,HR,LR,LC
      real x

c --- Initialize
      zcav=0.0
      ycav=0.0

c --- Cavity is not present upwind of bldg or at/beyond L+LR
      if(x.GE.(L+LR)) then
         return
      elseif(x.LT.0.0) then
         return
      endif
c
c     calculate x-y near wake boundary
c
      if(x.ge.0. .and. x.le.R) then
        ycav=(W/2.+R/3.)-(x-R)**2/(3.*R)
      elseif(x.gt.R .and. x.le.(L+LR)) then
        ycav=(W/2.+R/3.)*(1.-((x-R)/(L+LR-R))**2)**0.5
      endif

c     calculate x-z near wake boundary
c
      if(LC .lt. L)then       ! reattachment
c
        if(x.ge.0. .and. x.le.L) then
          zcav=H
        elseif(x.ge.L .and. x.le.(L+LR)) then
          zcav=H*(1.-((x-L)/LR)**2)**0.5
        endif
c
      else                    ! nonreattachment
        if(x.ge.0. .and. x.le.0.5*R) then
        zcav=HR+4.*(x-0.5*R)**2*(H-HR)/(R**2)
        elseif(x.gt.0.5*R .and. x.le.(L+LR)) then
          zcav=HR*(1.-((x-0.5*R)/(L+LR-0.5*R))**2)**0.5
        endif
      endif
c
      return
      end

